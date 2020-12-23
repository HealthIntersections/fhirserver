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
  {$IFDEF FPC} gui_lcl, {$ELSE} gui_vcl, {$ENDIF}

  IdOpenSSLLoader,

  fsl_base, fsl_utilities, fsl_fpc, fsl_logging, fsl_threads, fsl_openssl,
  {$IFDEF WINDOWS} fsl_service_win, {$ELSE} fsl_service, {$ENDIF}
  fdb_manager,
  fhir_objects,
  fhir2_factory, fhir3_factory, fhir4_factory, fhir5_factory,
  fhir2_javascript, fhir3_javascript, fhir4_javascript, fhir5_javascript,

  server_constants, server_config, utilities, server_context,
  {$IFNDEF NO_JS}server_javascript, {$ENDIF}
  tx_manager, telnet_server, web_source, web_server, web_cache,
  server_testing,
  endpoint, endpoint_storage, endpoint_bridge, endpoint_txsvr, endpoint_packages, endpoint_loinc, endpoint_snomed, endpoint_full;


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

  TFhirServerMaintenanceThread = class (TFslThread)
  private
    FKernel : TFHIRServiceKernel;
  protected
    function ThreadName : String; override;
    procedure Initialise; override;
    procedure Execute; override;
    procedure Finalise; override;
  public
    constructor Create(kernel : TFHIRServiceKernel);
    destructor Destroy; override;
  end;

  TFHIRServiceKernel = class (TSystemService)
  private
    FIni : TFHIRServerConfigFile;
    FSettings : TFHIRServerSettings;

    FTelnet : TFHIRTelnetServer;
    FTerminologies : TCommonTerminologies;
    FEndPoints : TFslList<TFHIRServerEndPoint>;
    FWebServer : TFhirWebServer;

    FMaintenanceThread: TFhirServerMaintenanceThread;

    procedure loadTerminologies;
    procedure loadEndPoints;
    procedure startWebServer;
    procedure stopWebServer;
    procedure unloadEndpoints;
    procedure unloadTerminologies;

    {$IFNDEF NO_JS}
    procedure registerJs(sender: TObject; js: TJsHost); virtual;
    {$ENDIF}

    function makeEndPoint(config : TFHIRServerConfigSection) : TFHIRServerEndPoint;

    function GetNamedContext(sender : TObject; name : String) : TFHIRServerContext;
  protected
    FStartTime : UInt64;
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

{$IFDEF WINDOWS}
uses
  JclDebug;
{$ENDIF}

{ TFhirServerMaintenanceThread }

constructor TFhirServerMaintenanceThread.Create(kernel : TFHIRServiceKernel);
begin
  inherited Create();
  FKernel := kernel;
end;

destructor TFhirServerMaintenanceThread.Destroy;
begin
  inherited;
end;

procedure TFhirServerMaintenanceThread.Initialise;
begin
  {$IFDEF WINDOWS}
  CoInitialize(nil);
  {$ENDIF}
  {$IFNDEF NO_JS}
  GJsHost := TJsHost.Create;
  //  todo, once eventing is sorted out  GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;
  {$ENDIF}
  TimePeriod := 5000;
end;

function TFhirServerMaintenanceThread.threadName: String;
begin
  result := 'Server Maintenance Thread';
end;

procedure TFhirServerMaintenanceThread.Execute;
var
  ep : TFhirServerEndpoint;
begin
  try
    for ep in FKernel.FEndPoints do
      if not Terminated then
        ep.internalThread;
    if not Terminated then
      FKernel.FTerminologies.sweepSnomed;
    if not Terminated then
      FKernel.WebServer.Common.cache.Trim;
  except
  end;
end;

procedure TFhirServerMaintenanceThread.Finalise;
begin
  {$IFNDEF NO_JS}
  GJsHost.Free;
  GJsHost := nil;
  {$ENDIF}
  {$IFDEF WINDOWS}
  CoUninitialize;
  {$ENDIF}
end;

{ TFHIRServiceKernel }

constructor TFHIRServiceKernel.Create(const ASystemName, ADisplayName, Welcome: String; ini: TFHIRServerCOnfigFile);
begin
  FStartTime := GetTickCount64;
  inherited create(ASystemName, ADisplayName);
  FTelnet := TFHIRTelnetServer.Create(44123, Welcome);
  FIni := ini;
  FTelnet.Password := FIni.web['telnet-password'].value;
  Logging.addListener(FTelnet);

  FSettings := TFHIRServerSettings.Create;
  FSettings.ForLoad := not hasCommandLineParam('noload');
  FSettings.load(FIni);

  FEndPoints := TFslList<TFHIRServerEndPoint>.create;
end;

destructor TFHIRServiceKernel.Destroy;
begin
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
begin
  try
    LoadTerminologies;
    LoadEndPoints;
    StartWebServer();
    FMaintenanceThread := TFhirServerMaintenanceThread.Create(self);
    FMaintenanceThread.Start;

    // post start up time.
    getReport('|', true); // base line the object counting
    Logging.log('started ('+inttostr((GetTickCount64 - FStartTime) div 1000)+'secs)');
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
    Logging.log('stop web server');

    sendSMS(Settings, Settings.HostSms, 'The server ' + DisplayName + ' for ' + FSettings.OwnerName + ' is stopping');
    if FMaintenanceThread <> nil then
    begin
      FMaintenanceThread.StopAndWait(100);
    end;
    FMaintenanceThread.Free;
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

{$IFNDEF NO_JS}
procedure TFHIRServiceKernel.registerJs(sender : TObject; js : TJsHost);
begin
  js.engine.registerFactory(fhir2_javascript.registerFHIRTypes, fhirVersionRelease2, TFHIRFactoryR2.create);
  js.engine.registerFactory(fhir3_javascript.registerFHIRTypes, fhirVersionRelease3, TFHIRFactoryR3.create);
  js.engine.registerFactory(fhir4_javascript.registerFHIRTypes, fhirVersionRelease4, TFHIRFactoryR4.create);
  js.engine.registerFactory(fhir4_javascript.registerFHIRTypesDef, fhirVersionUnknown, TFHIRFactoryR4.create);
  js.engine.registerFactory(fhir5_javascript.registerFHIRTypes, fhirVersionRelease5, TFHIRFactoryR5.create);
end;
{$ENDIF}

// --- core functionality ------------------------------------------------------

procedure TFHIRServiceKernel.loadTerminologies;
begin
  Logging.log('Load Terminologies');
  FTerminologies := TCommonTerminologies.Create(Settings.link);
  FTerminologies.load(Ini['terminologies'], false);
end;

procedure TFHIRServiceKernel.loadEndPoints;
var
  section : TFHIRServerConfigSection;
  ep : TFHIRServerEndPoint;
begin
  Logging.log('Load End Points');
  for section in FIni['endpoints'].sections do
  begin
    if section['active'].valueBool then
      FEndPoints.Add(makeEndPoint(section));
  end;

  for ep in FEndPoints do
  begin
    Logging.log('Load End Point '+ep.config.name+': '+ep.summary);
    FTelnet.addEndPoint(ep);
    {$IFNDEF NO_JS}
    ep.OnRegisterJs := registerJs;
    {$ENDIF}
    ep.Load;
  end;
end;

procedure TFHIRServiceKernel.StartWebServer;
var
  ep : TFHIRServerEndPoint;
begin
  FWebServer := TFhirWebServer.create(Settings.Link, DisplayName);
  FWebServer.Common.cache := THTTPCacheManager.Create;
  {$IFNDEF NO_JS}
  FWebServer.Common.OnRegisterJs := registerJs;
  {$ENDIF}
  FWebServer.loadConfiguration(Ini);
  if FolderExists('c:\work\fhirserver\server\web') then
  begin
    Logging.log('Web source from c:\work\fhirserver\server\web');
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('c:\work\fhirserver\server\web')
  end
  else if FolderExists('..\..\server\web') then
  begin
    Logging.log('Web source from ..\..\server\web');
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('..\..\server\web')
  end
  else if FileExists(partnerFile('fhirserver.web')) then
  begin
    Logging.log('Web source from '+partnerFile('fhirserver.web'));
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceZipProvider.Create(partnerFile('fhirserver.web'))
  end
  else
    raise Exception.Create('Unable to find web source');

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
  else if config['type'].value = 'loinc' then
    result := TLoincWebEndPoint.Create(config.link, FSettings.Link, nil, Terminologies.link)
  else if config['type'].value = 'snomed' then
    result := TSnomedWebEndPoint.Create(config.link, FSettings.Link, Terminologies.link)
  else if config['type'].value = 'bridge' then
    result := TBridgeEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Terminologies.link)
  else if config['type'].value = 'terminology' then
    result := TTerminologyServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Terminologies.link)
  else if config['type'].value = 'full' then
  begin
    result := TFullServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Terminologies.link);
    TFullServerEndPoint(result).OnGetNamedContext := GetNamedContext;
  end
  else
    raise Exception.Create('Unknown server type "' +config['type'].value+'"');
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


{ === Core ====================================================================}

procedure RunGui(ini : TFHIRServerConfigFile);
begin
  {$IFDEF WINDOWS}
  FreeConsole;
  {$ENDIF}

  {$IFDEF FPC}
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
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

    {$IFDEF WINDOWS}
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
        try
          writeln('No -cmd parameter - exiting now'); // won't see this if an actual windows service
        except
          // catch 105 err
        end;
        svc.Execute;
      end;
    finally
      svc.Free;
    end;
  end;
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

procedure ExecuteFhirServer;
var
  cfg : TFHIRServerConfigFile;
  cfgName : String;
  fn : String;
  tz : TDateTime;
begin
  {$IFDEF WINDOWS}
  SetConsoleTitle('FHIR Server');
  {$ENDIF}

  if getCommandLineParam('log', fn) then
    Logging.logToFile(fn)
  else if (FolderExists('c:\temp')) then
    Logging.logToFile('c:\temp\fhirserver.log')
  else
    Logging.logToFile(tempFile('fhirserver.log'));


  // if there's no parameters, then we don't log to the screen
  // if the cmd parameter is 'console' or 'exec' then we also log to the screen
  if ParamCount > 0 then
    Logging.LogToConsole := true;

  logCompileInfo;

  if ParamCount = 0 then
    Logging.log('FHIR Server running as a Service')
  else
    Logging.log(commandLineAsString);

  try
    Logging.Log('Loading Dependencies');
    {$IFDEF WINDOWS}
    GetOpenSSLLoader.OpenSSLPath := ExtractFilePath(Paramstr(0));
    {$ENDIF}
    InitOpenSSL;
    {$IFDEF WINDOWS}
    JclStartExceptionTracking;
    CoInitialize(nil);
    {$ENDIF}
    {$IFDEF FPC}
    initialiseTZData(partnerFile('tzdata.tar.gz'));
    {$ENDIF}
    tz := TimeZoneBias;
    if tz = 0 then
      Logging.log('TimeZone: '+TimeZoneIANAName+' @ UTC')
    else if tz < 0 then
      Logging.log('TimeZone: '+TimeZoneIANAName+' @ -'+FormatDateTime('hh:nn', tz))
    else
      Logging.log('TimeZone: '+TimeZoneIANAName+' @ +'+FormatDateTime('hh:nn', tz));

    try
      {$IFNDEF NO_JS}
      GJsHost := TJsHost.Create;
      try
      {$ENDIF}
        if not getCommandLineParam('cfg', cfgName) then
        {$IFDEF OSX}
          cfgName := GetAppConfigDir(false)+'fhirserver.cfg';
        {$ELSE}
          cfgName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'fhirserver.cfg';
        {$ENDIF}

        cfg := TFHIRServerConfigFile.create(cfgName);
        try
          ExecuteFhirServer(cfg);
        finally
          cfg.free;
        end;
      {$IFNDEF NO_JS}
      finally
        GJsHost.free;
      end;
      {$ENDIF}
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


end.

