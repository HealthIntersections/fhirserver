unit kernel;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$I fhir.inc}

interface

Uses
  {$IFDEF WINDOWS} Windows, ActiveX, {$ENDIF}
  SysUtils, StrUtils, Classes, IniFiles, Forms,
  {$IFDEF FPC} gui_lcl, Interfaces, {$ELSE} gui_vcl, {$ENDIF}

  IdOpenSSLLoader,

  fsl_base, fsl_utilities, fsl_fpc, fsl_logging, fsl_threads, fsl_openssl, fsl_stream, fsl_npm_cache,
  {$IFDEF WINDOWS} fsl_service_win, {$ELSE} fsl_service, {$ENDIF}
  fdb_manager,
  fhir_objects,
  fhir2_factory, fhir3_factory, fhir4_factory, fhir5_factory,

  {$IFDEF FPC}
  fui_fake_console,
  {$ENDIF}

  server_constants, server_config, utilities, server_context,
  tx_manager, telnet_server, web_source, web_server, web_cache, remote_config,
  server_testing, kernel_thread,
  endpoint, endpoint_storage, endpoint_bridge, endpoint_txsvr, endpoint_packages,
  endpoint_loinc, endpoint_snomed, endpoint_full, endpoint_folder, endpoint_icao;


// how the kernel works:
//
// start up phase 1:
//  - load the telnet server
//  - load settings
//  - set up logging infrastructure
//  - call the service started
//
// start up phase 2:
//  - load the terminologies
//  - load the endpoints
//  - set up the web server
//  - connect the web server to the end points
//  - turn the web server on

type
  TFHIRServiceKernel = class;

  TFHIRServiceKernel = class (TSystemService)
  private
    FIni : TFHIRServerConfigFile;
    FSettings : TFHIRServerSettings;

    FTelnet : TFHIRTelnetServer;
    FTerminologies : TCommonTerminologies;
    FEndPoints : TFslList<TFHIRServerEndPoint>;
    FWebServer : TFhirWebServer;

    FMaintenanceThread: TFhirServerMaintenanceThread;
    FPcm : TFHIRPackageManager;
    FMaxMem : UInt64;

    procedure loadTerminologies;
    procedure loadEndPoints;
    procedure startWebServer;
    procedure SetCacheStatus(status : boolean);
    procedure stopWebServer;
    procedure unloadEndpoints;
    procedure unloadTerminologies;

    function makeEndPoint(config : TFHIRServerConfigSection) : TFHIRServerEndPoint;

    function GetNamedContext(sender : TObject; name : String) : TFHIRServerContext;
    procedure checkMem (callback : TFhirServerMaintenanceThreadTaskCallBack);
  protected
    function command(cmd: String): boolean; override;
  public
    constructor Create(const ASystemName, ADisplayName, Welcome : String; ini : TFHIRServerConfigFile);
    destructor Destroy; override;

    function CanStart : boolean; Override;
    procedure postStart; override;
    procedure DoStop; Override;
    procedure dump; override;

//    property loadStore : boolean read FLoadStore write FLoadStore;
    property Ini : TFHIRServerConfigFile read FIni;
    property Settings : TFHIRServerSettings read FSettings;
    property WebServer : TFhirWebServer read FWebServer;
    property Terminologies : TCommonTerminologies read FTerminologies;
    property EndPoints : TFslList<TFHIRServerEndPoint> read FEndPoints;
  end;


procedure ExecuteFhirServer; overload;

implementation

{$IFDEF DELPHI}
uses
  JclDebug;
{$ENDIF}

var
  GStartTime : UInt64;

{ TFHIRServiceKernel }

constructor TFHIRServiceKernel.Create(const ASystemName, ADisplayName, Welcome: String; ini: TFHIRServerCOnfigFile);
begin
  inherited create(ASystemName, ADisplayName);
  FTelnet := TFHIRTelnetServer.Create(44123, Welcome);
  FIni := ini;
  FTelnet.Password := FIni.web['telnet-password'].value;
  Logging.addListener(FTelnet);

  FSettings := TFHIRServerSettings.Create;
  FSettings.ForLoad := not hasCommandLineParam('noload');
  FSettings.load(FIni);

  if FSettings.Ini.service['package-cache'].value <> '' then
    FPcm := TFHIRPackageManager.Create(FSettings.Ini.service['package-cache'].value)
  else
    FPcm := TFHIRPackageManager.Create(false);

  FMaxMem := FSettings.Ini.service['max-memory'].readAsUInt64(0) * 1024 * 1024;
  FEndPoints := TFslList<TFHIRServerEndPoint>.create;
end;

destructor TFHIRServiceKernel.Destroy;
begin
  FPcm.Free;
  Logging.removeListener(FTelnet);
  FEndPoints.Free;
  FIni.Free;
  FSettings.Free;
  FTelnet.Free;
  inherited;
end;

function TFHIRServiceKernel.CanStart: boolean;
begin
  Logging.log('Run Number '+inttostr(Settings.RunNumber));
  result := true;
end;

procedure TFHIRServiceKernel.postStart;
var
  ep : TFhirServerEndpoint;
begin
  try
    LoadTerminologies;
    LoadEndPoints;
    StartWebServer();

    FMaintenanceThread := TFhirServerMaintenanceThread.Create;
    FMaintenanceThread.defineTask('mem-check', checkMem, 5);
    for ep in FEndPoints do
      FMaintenanceThread.defineTask('ep:'+ep.Config.Name, ep.internalThread, 5);
    FMaintenanceThread.defineTask('snomed', FTerminologies.sweepSnomed, 10);
    FMaintenanceThread.defineTask('web-cache', WebServer.Common.cache.Trim, 10);
    FMaintenanceThread.Start;


    SetCacheStatus(settings.Ini.web['caching'].value = 'true');

    // post start up time.
    getReport('|', true); // base line the object countig
    Logging.log('started ('+inttostr((GetTickCount64 - GStartTime) div 1000)+'secs)');
    Logging.Starting := false;
    sendSMS(Settings, Settings.HostSms, 'The server ' + DisplayName + ' for ' + FSettings.OwnerName + ' has started');
  except
    on e : Exception do
    begin
      Logging.log('Error starting: '+E.ClassName+ ': ' + E.Message+#13#10#13#10+ExceptionStack(e));
      raise;
    end;
  end;
end;

procedure TFHIRServiceKernel.DoStop;
begin
  try
    Logging.log('stopping: '+StopReason);

    sendSMS(Settings, Settings.HostSms, 'The server ' + DisplayName + ' for ' + FSettings.OwnerName + ' is stopping');
    Logging.log('close web server');
    if FWebServer <> nil then
      FWebServer.Close;
    Logging.log('stop internal thread');
    if FMaintenanceThread <> nil then
    begin
      FMaintenanceThread.StopAndWait(21000); // see comments in FMaintenanceThread.finalise
      FMaintenanceThread.Free;
    end;
    Logging.log('stop web server');
    StopWebServer;
    Logging.log('closing');
    UnLoadEndPoints;
    UnLoadTerminologies;
    Logging.log('stopped');
  except
    on e : Exception do
      Logging.log('Exception stopping ('+E.ClassName + '): ' + E.Message+#13#10#13#10+ExceptionStack(e));
  end;
end;

function endpointName : String;
begin
  if not getCommandLineParam('endpoint', result) then
    raise EFslException.Create('No endpoint parameter supplied');
end;

procedure TFHIRServiceKernel.SetCacheStatus(status: boolean);
var
  ep : TFhirServerEndpoint;
begin
  if (status) then
    Logging.log('HTTP Caching is On')
  else
    Logging.log('HTTP Caching is Off');
  WebServer.setCacheStatus(status);
  for ep in FEndPoints do
    ep.setCacheStatus(status);
end;

// --- core functionality ------------------------------------------------------

procedure TFHIRServiceKernel.loadTerminologies;
begin
  Logging.log('Load Terminologies');
  FTerminologies := TCommonTerminologies.Create(Settings.link);
  FTerminologies.load(Ini['terminologies'], false);
end;

function epVersion(section : TFHIRServerConfigSection): TFHIRVersion;
var
  v : String;
begin
  v := section['version'].value;
  if (v = 'r2') then
    result := fhirVersionRelease2
  else if (v = 'r3') then
    result := fhirVersionRelease3
  else if (v = 'r4') then
    result := fhirVersionRelease4
  else if (v = 'r5b') then
    result := fhirVersionRelease4B
  else if (v = 'r5') then
    result := fhirVersionRelease5
  else
    result := fhirVersionUnknown;
end;

function versionOk(section : TFHIRServerConfigSection) : boolean;
begin
   result := epVersion(section) in [fhirVersionUnknown, fhirVersionRelease2, fhirVersionRelease3, fhirVersionRelease4]
end;

procedure TFHIRServiceKernel.loadEndPoints;
var
  section : TFHIRServerConfigSection;
  ep : TFHIRServerEndPoint;
begin
  Logging.log('Load End Points');
  for section in FIni['endpoints'].sections do
  begin
    if (section['active'].valueBool) and (versionOk(section)) then
      FEndPoints.Add(makeEndPoint(section));
  end;

  for ep in FEndPoints do
  begin
    Logging.log('Load End Point '+ep.config.name+': '+ep.summary);
    FTelnet.addEndPoint(ep);
    ep.Load;
  end;
end;

procedure TFHIRServiceKernel.StartWebServer;
var
  ep : TFHIRServerEndPoint;
begin
  FWebServer := TFhirWebServer.create(Settings.Link, DisplayName);
  FWebServer.Common.cache := THTTPCacheManager.Create(Settings.Ini.section['web'].prop['http-cache-time'].readAsInt(0));

  FWebServer.loadConfiguration(Ini);
  if FolderExists('c:\work\fhirserver\server\web') then
  begin
    Logging.log('Web source from c:\work\fhirserver\server\web');
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('c:\work\fhirserver\server\web')
  end
  else if FolderExists(FilePath([ExtractFilePath(paramstr(0)), '..\..\server\web'])) then
  begin
    Logging.log('Web source from ../../server/web');
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create(FilePath([ExtractFilePath(paramstr(0)), '..\..\server\web']))
  end
  else if FileExists(partnerFile('fhirserver.web')) then
  begin
    Logging.log('Web source from '+partnerFile('fhirserver.web'));
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceZipProvider.Create(partnerFile('fhirserver.web'))
  end
  else
    raise EFslException.Create('Unable to find web source');

  for ep in FEndPoints do
    FWebServer.registerEndPoint(ep);

  FWebServer.Start;
end;

procedure TFHIRServiceKernel.StopWebServer;
begin
  if FWebServer <> nil then
  begin
    FWebServer.Stop;
    FWebServer.free;
  end;
end;

procedure TFHIRServiceKernel.unloadEndpoints;
var
  ep : TFHIRServerEndPoint;
begin
  for ep in FEndPoints do
  begin
    FTelnet.removeEndPoint(ep);
    ep.Unload;
  end;
  FEndpoints.free;
  FEndpoints := nil;
end;

procedure TFHIRServiceKernel.UnloadTerminologies;
begin
  FTerminologies.Free;
  FTerminologies := nil;
end;

procedure TFHIRServiceKernel.dump;
begin
  // nothing?
end;

function TFHIRServiceKernel.command(cmd: String): boolean;
var
  fn : String;
  ep : TFHIRServerEndPoint;
begin
  result := true;
  if cmd = 'mount' then
  begin
    ep := makeEndPoint(FIni['endpoints'].section[endpointName]);
    try
      ep.InstallDatabase;
      Logging.log('  .. installed');
      if getCommandLineParam('packages', fn) then
      begin
        Logging.log('  .. installing packages');
        ep.LoadPackages(fn);
      end;
    finally
      ep.free;
    end;
  end
  else if cmd = 'pword' then
  begin
    ep := makeEndPoint(FIni['endpoints'].section[endpointName]);
    try
      ep.updateAdminPassword;
    finally
      ep.free;
    end;
  end
  else if cmd = 'unmount' then
  begin
    ep := makeEndPoint(FIni['endpoints'].section[endpointName]);
    try
      ep.UninstallDatabase;
    finally
      ep.free;
    end;
  end
  else if (cmd = 'remount') or (cmd = 'installdb') then
  begin
    Logging.log('Install new database (wipe if necessary)');
    if getCommandLineParam('packages', fn) then
      loadTerminologies;
    try
      ep := makeEndPoint(FIni['endpoints'].section[endpointName]);
      try
        ep.UninstallDatabase;
        Logging.log('  .. uninstalled');
        ep.InstallDatabase;
        Logging.log('  .. installed');
        if getCommandLineParam('packages', fn) then
        begin
          Logging.log('  .. installing packages');
          ep.LoadPackages(fn);
        end;
      finally
        ep.free;
      end;
    finally
      unloadTerminologies;
    end;
    if hasCommandLineParam('installer') then
      Logging.log('---completed ok---');
  end
  else if cmd = 'load' then
  begin
    ep := makeEndPoint(FIni['endpoints'].section[endpointName]);
    try
      if getCommandLineParam('packages', fn) then
        ep.LoadPackages(fn)
      else
        Logging.log('no Packages to install');
    finally
      ep.free;
    end;
  end
  else
    result := inherited ;
end;

function TFHIRServiceKernel.makeEndPoint(config : TFHIRServerConfigSection) : TFHIRServerEndPoint;
begin
  // we generate by type and mode
  if config['type'].value = 'package' then
    result := TPackageServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Terminologies.link)
  else if config['type'].value = 'folder' then
    result := TFolderWebEndPoint.Create(config.link, FSettings.Link)
  else if config['type'].value = 'icao' then
    result := TICAOWebEndPoint.Create(config.link, FSettings.Link)
  else if config['type'].value = 'loinc' then
    result := TLoincWebEndPoint.Create(config.link, FSettings.Link, nil, Terminologies.link)
  else if config['type'].value = 'snomed' then
    result := TSnomedWebEndPoint.Create(config.link, FSettings.Link, Terminologies.link)
  else if config['type'].value = 'bridge' then
    result := TBridgeEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Terminologies.link, FPcm.link)
  else if config['type'].value = 'terminology' then
    result := TTerminologyServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Terminologies.link, FPcm.link)
  else if config['type'].value = 'full' then
  begin
    result := TFullServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Terminologies.link, FPcm.link);
    TFullServerEndPoint(result).OnGetNamedContext := GetNamedContext;
  end
  else
    raise EFslException.Create('Unknown server type "' +config['type'].value+'"');
end;

function TFHIRServiceKernel.GetNamedContext(sender: TObject; name: String): TFHIRServerContext;
var
  t : TFHIRServerEndPoint;
begin
  result := nil;
  for t in FEndPoints do
    if (t.Config.name = name) and (t is TStorageEndPoint) then
      exit((t as TStorageEndPoint).ServerContext);
end;

procedure TFHIRServiceKernel.checkMem (callback : TFhirServerMaintenanceThreadTaskCallBack);
var
  ep : TFhirServerEndpoint;
begin
  callback(self, 'Checking Memory Status', -1);
  if (FMaxMem > 0) and (Logging.InternalMem > FMaxMem) then
  begin
    Logging.log('Clear Caches because memory in use = '+DescribeBytes(Logging.InternalMem)+', and max is '+DescribeBytes(FMaxMem));
    WebServer.clearCache;
    for ep in FEndPoints do
      ep.clearCache;
    Logging.log('Cleared Cache. Memory in use = '+DescribeBytes(Logging.InternalMem));
  end;
end;

{ === Core ====================================================================}

procedure RunGui(ini : TFHIRServerConfigFile);
begin
  {$IFDEF WINDOWS}
  FreeConsole;
  {$ENDIF}

  {$IFDEF FPC}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TServerGUI, ServerGUI);
  Application.Run;
end;

procedure ExecuteFhirServer(ini : TFHIRServerConfigFile); overload;
var
  svcName : String;
  dispName : String;
  cmd : String;
  svc : TFHIRServiceKernel;
  logMsg : String;
begin
  // if we're running the test or gui, go do that
  if (hasCommandLineParam('tests') or hasCommandLineParam('-tests')) then
    RunTests(ini)
  else if (hasCommandLineParam('testinsight')) then
    RunTestInsight(ini)
  else if (hasCommandLineParam('gui') or hasCommandLineParam('manager')) then
    RunGui(ini)
  else if (hasCommandLineParam('help')) then
  begin
    writeln('Health Intersections FHIR Server');
    writeln('This is the Server. For command line parameters, see ');
    writeln('http://www.healthintersections.com.au/wiki/index.php/Command_line_Parameters');
  end
  else
  begin
    if not getCommandLineParam('name', svcName) then
      if ini.service['name'].value <> '' then
        svcName := ini.service['name'].value
      else
        svcName := 'FHIRServer';

    if not getCommandLineParam('title', dispName) then
      if ini.service['title'].value <> '' then
        dispName := ini.service['title'].value
      else
        dispName := 'FHIR Server';

    {$IFDEF DELPHI}
    if JclExceptionTrackingActive then
      logMsg := 'Using Configuration file '+ini.FileName+' (+stack dumps)'
    else
    {$ENDIF}
      logMsg := 'Using Configuration file '+ini.FileName;
    Logging.log(logMsg);

    svc := TFHIRServiceKernel.create(svcName, dispName, logMsg, ini.link);
    try
      if getCommandLineParam('cmd', cmd) then
      begin
        if (cmd = 'exec') or (cmd = 'console') then
          svc.ConsoleExecute
        else if (cmd = 'tests') then
          runTests(ini)
        else if not svc.command(cmd) then
          raise EFslException.Create('Unknown command '+cmd);
      end
      else if (isTestInsight) then
      begin
        RunTestInsight(ini);
      end
      else
      begin
        {$IFDEF WINDOWS}
        try
          writeln('No -cmd parameter - exiting now'); // won't see this if an actual windows service
        except
          // catch 105 err
        end;
        svc.Execute;
        {$ELSE}
        svc.ConsoleExecute;
        {$ENDIF}
      end;
    finally
      svc.Free;
    end;
  end;
end;

procedure logSystemInfo;
var
  l, s : string;
begin
  l := 'Running on "'+SystemName+'": '+SystemPlatform;
  s := SystemArchitecture;
  if (s <> '') and not sameText(s, 'Unknown') then
  begin
    l := l + ' (';
    l := l + s;
    s := SystemProcessorName;
    if s <> '' then
      l := l + '/'+s;
    l := l + ')';
  end;
  l := l + '. ';
  l := l + DescribeBytes(SystemMemory.physicalMem)+'/ '+DescribeBytes(SystemMemory.virtualMem)+' memory';
  Logging.log(l);
end;

procedure logCompileInfo;
var
  compiler, os, cpu, s : String;
begin
  {$IFDEF FPC}
  compiler := '/FreePascal';
  {$ELSE}
  compiler := '/Delphi';
  {$ENDIF}
  {$IFDEF WINDOWS}
  os := 'Windows';
  {$ENDIF}
  {$IFDEF LINUX}
  os := 'Linux';
  {$ENDIF}
  {$IFDEF OSX}
  os := 'OSX';
  {$ENDIF}
  {$IFDEF CPU64}
  cpu := '-64';
  {$ELSE}
  cpu := '-32';
  {$ENDIF}

  s := os+cpu+compiler;
  {$IFOPT C+}
  s := s + ' +Assertions';
  {$ENDIF}

  {$IFOPT D+}
  s := s + ' +Debug';
  {$ENDIF}

  {$IFOPT O+}
  s := s + ' +Optimizations';
  {$ENDIF}

  Logging.log('FHIR Server '+SERVER_FULL_VERSION+' '+s);
end;

procedure ExecuteFhirServerInner;
var
  cfg : TFHIRServerConfigFile;
  cfgName, s : String;
  fn : String;
  tz : TDateTime;
  zc : String;
begin
  GStartTime := GetTickCount64;

  {$IFDEF WINDOWS}
  SetConsoleTitle('FHIR Server');
  {$ENDIF}

  if getCommandLineParam('log', fn) then
    Logging.logToFile(fn)
  else
    Logging.logToFile(filePath(['[tmp]', 'fhirserver.log']));
  Logging.FileLog.Policy.FullPolicy := lfpChop;
  Logging.FileLog.Policy.MaximumSize := 1024 * 1024;

  // if there's no parameters, then we don't log to the screen
  // if the cmd parameter is 'console' or 'exec' then we also log to the screen
  if ParamCount > 0 then
    Logging.LogToConsole := true;

  logCompileInfo;
  logSystemInfo;

  if ParamCount = 0 then
  begin
    {$IFDEF WINDOWS}
    Logging.log('FHIR Server running as a Service');
    {$ELSE}
    Logging.log('FHIR Server: no parameters');
    {$ENDIF}
  end
  else
    Logging.log(commandLineAsString+' (dir='+GetCurrentDir+')');

  try
    Logging.Log('Loading Dependencies');
    {$IFNDEF STATICLOAD_OPENSSL}
    {$IFDEF WINDOWS}
    GetOpenSSLLoader.OpenSSLPath := ExtractFilePath(Paramstr(0));
    {$ENDIF}
    {$IFDEF OSX}
    // todo: do something about this
    GetOpenSSLLoader.OpenSSLPath := '/opt/homebrew/Cellar/openssl@1.1/1.1.1l/lib/';
    {$ENDIF}
    if GetOpenSSLLoader.OpenSSLPath = '' then
      Logging.Log('OpenSSL 1.1 from (default)')
    else
      Logging.Log('OpenSSL 1.1 from '+GetOpenSSLLoader.OpenSSLPath);
    {$ELSE}
    // Logging.Log('OpenSSL 1.1 Statically bound');
    {$ENDIF}
    InitOpenSSL;
    {$IFDEF DELPHI}
    JclStartExceptionTracking;
    CoInitialize(nil);
    {$ENDIF}
    fhir_objects.loadMessages;
    Logging.Log('Loaded');
    tz := TimeZoneBias;
    if tz = 0 then
      Logging.log('TimeZone: '+TimeZoneIANAName+' @ UTC')
    else if tz < 0 then
      Logging.log('TimeZone: '+TimeZoneIANAName+' @ -'+FormatDateTime('hh:nn', tz))
    else
      Logging.log('TimeZone: '+TimeZoneIANAName+' @ +'+FormatDateTime('hh:nn', tz));

    try
      if not getCommandLineParam('cfg', cfgName) then
      {$IFDEF OSX}
        cfgName := GetAppConfigDir(false)+'fhirserver.cfg';
      {$ELSE}
        cfgName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'fhirserver.cfg';
      {$ENDIF}
      Logging.Log('Config: '+cfgName);

      if cfgName.StartsWith('https://') or cfgName.StartsWith('http://') or cfgName.StartsWith('file:') then
        cfgName := buildConfigFromSource(cfgName)
      else
      begin
        // zero config service support, where you don't easily get a parameter
        s := FileToString(cfgName, TEncoding.UTF8);
        if s.StartsWith('https://') or s.StartsWith('http://') or s.StartsWith('file:') then
          cfgName := buildConfigFromSource(s);
      end;



      cfg := TFHIRServerConfigFile.create(cfgName);
      try
        ExecuteFhirServer(cfg);
      finally
        cfg.free;
      end;
    finally
    {$IFDEF WINDOWS}
    CoUninitialize();
    {$ENDIF}
    end;
  except
    on e : Exception do
    begin
      if hasCommandLineParam('installer') then
        writeln('##> Exception '+E.Message)
      else
        writeln(E.ClassName+ ': '+E.Message+#13#10#13#10+ExceptionStack(e));
      sleep(1000);
    end;
  end;
end;

procedure ExecuteFhirServer;
{$IFDEF FPC}
begin
  if hasCommandLineParam('fake-console') then
  begin
    RequireDerivedFormResource := True;
    Application.Title := 'FHIRServer';
    Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TFakeConsoleForm, FakeConsoleForm);
    FakeConsoleForm.caption := 'FHIRServer';
    FakeConsoleForm.Op := ExecuteFhirServerInner;
    Application.run;


    FakeConsoleForm.close;
    FakeConsoleForm.free;
  end
  else
  begin
    {$IFDEF WINDOWS}
    AllocConsole;
    IsConsole := True;
    StdInputHandle  := 0;
    StdOutputHandle := 0;
    StdErrorHandle  := 0;
    SysInitStdIO;
    {$ENDIF}
    ExecuteFhirServerInner;
  end;
end;
{$ELSE}
begin
  ExecuteFhirServerInner;
end;
{$ENDIF}

end.
