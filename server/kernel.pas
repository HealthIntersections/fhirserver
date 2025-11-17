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
  {$IFDEF WINDOWS} Windows, ActiveX, FastMM4, {$ENDIF}
  SysUtils, StrUtils, Classes, IniFiles, Forms,
  {$IFDEF FPC} gui_lcl, Interfaces, {$ELSE} gui_vcl, {$ENDIF}

  IdOpenSSLLoader,

  fsl_base, fsl_utilities, fsl_fpc, fsl_logging, fsl_threads, fsl_openssl, fsl_stream, fsl_npm_cache, fsl_web_init, fsl_i18n,
  {$IFDEF WINDOWS} fsl_service_win, {$ELSE} fsl_service, {$ENDIF}
  fdb_manager,
  fhir_objects,
  fhir3_factory, fhir4_factory, fhir5_factory,

  {$IFDEF FPC}
  fui_fake_console,
  {$ENDIF}

  server_constants, server_config, utilities, server_context,
  tx_manager, telnet_server, web_source, web_server, web_cache, zero_config,
  server_testing, kernel_thread, server_stats,
  endpoint, endpoint_storage, endpoint_bridge, endpoint_txsvr,
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

  { TFHIRServiceKernel }

  TFHIRServiceKernel = class (TSystemService)
  private
    FIni : TFHIRServerConfigFile;
    FSettings : TFHIRServerSettings;

    FTelnet : TFHIRTelnetServer;
    FTerminologies : TCommonTerminologies;
    FEndPoints : TFslList<TFHIRServerEndPoint>;
    FWebServer : TFhirWebServer;
    FParams : TCommandLineParameters;

    FMaintenanceThread: TFhirServerMaintenanceThread;
    FPcm : TFHIRPackageManager;
    FMaxMem : UInt64;
    FI18n : TI18nSupport;
    FStatsCount : integer;
    FStatsRecord : TStatusRecord;

    function endpointName: String;
    procedure loadTerminologies;
    procedure loadEndPoints;
    procedure startWebServer;
    procedure SetCacheStatus(status : boolean);
    procedure stopWebServer;
    procedure unloadEndpoints;
    procedure unloadTerminologies;

    procedure recordStats(callback : TFhirServerMaintenanceThreadTaskCallBack);
    procedure sweepCaches(callback : TFhirServerMaintenanceThreadTaskCallBack);  

    function makeEndPoint(config : TFHIRServerConfigSection) : TFHIRServerEndPoint;

    function GetNamedContext(sender : TObject; name : String) : TFHIRServerContext;
    procedure checkMem (callback : TFhirServerMaintenanceThreadTaskCallBack);
  protected
    function command(cmd: String): boolean; override;
  public
    constructor Create(const ASystemName, ADisplayName, Welcome : String; ini : TFHIRServerConfigFile; params : TCommandLineParameters);
    destructor Destroy; override;

    function CanStart : boolean; Override;
    procedure postStart; override;
    procedure DoStop; Override;
    procedure dump; override;

    procedure StopCommand(sender : TObject);

//    property loadStore : boolean read FLoadStore write FLoadStore;
    property Ini : TFHIRServerConfigFile read FIni;
    property Settings : TFHIRServerSettings read FSettings;
    property WebServer : TFhirWebServer read FWebServer;
    property Terminologies : TCommonTerminologies read FTerminologies;
    property i18n : TI18nSupport read FI18n;
    property EndPoints : TFslList<TFHIRServerEndPoint> read FEndPoints;
  end;


procedure ExecuteFhirServer(params : TCommandLineParameters); overload;

implementation

{$IFDEF DELPHI}
uses
  JclDebug;
{$ENDIF}

function systemInfo : string;
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
  result := l;
end;

function compileInfo : String;
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
  cpu := ''; //'-64';
  {$ELSE}
  cpu := '-32';
  {$ENDIF}

  s := os+cpu+compiler+', '+BuildDescription;

  result := 'FHIR Server '+SERVER_FULL_VERSION+' '+s;
end;



var
  GStartTime : UInt64;

{ TFHIRServiceKernel }

constructor TFHIRServiceKernel.Create(const ASystemName, ADisplayName,
  Welcome: String; ini: TFHIRServerConfigFile; params : TCommandLineParameters);
begin
  inherited Create(ASystemName, ADisplayName);
  FParams := params;
  FTelnet := TFHIRTelnetServer.Create(44123, Welcome);
  FIni := ini;
  FTelnet.Password := FIni.web['telnet-password'].value;
  Logging.addListener(FTelnet);

  FSettings := TFHIRServerSettings.Create;
  FSettings.ForLoad := not params.has('noload');
  FSettings.load(FIni);

  if FSettings.Ini.service['package-cache'].value <> '' then
    FPcm := TFHIRPackageManager.Create(FSettings.Ini.service['package-cache'].value)
  else
    FPcm := TFHIRPackageManager.Create(npmModeSystem);

  FMaxMem := FSettings.Ini.service['max-memory'].readAsUInt64(0) * 1024 * 1024;
  FEndPoints := TFslList<TFHIRServerEndPoint>.Create;
  FStatsRecord := TStatusRecord.Create;
end;

destructor TFHIRServiceKernel.Destroy;
begin
  FTelnet.ShuttingDown := true;
  FParams.free;
  FStatsRecord.free;
  FI18n.free;
  FPcm.free;
  Logging.removeListener(FTelnet);
  FEndPoints.free;
  FIni.free;
  FSettings.free;
  FTelnet.free;
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
  i : integer;
begin
  GThreadDoingConstruction := true;
  try
    try
      LoadTerminologies;
      LoadEndPoints;
      StartWebServer();
      for ep in FEndPoints do
        ep.Started;

      recordStats(nil);
      FMaintenanceThread := TFhirServerMaintenanceThread.Create;
      FMaintenanceThread.defineTask('mem-check', checkMem, 35);
      FMaintenanceThread.defineTask('stats', recordStats, 60);
      i := 0;
      for ep in FEndPoints do
      begin
        FMaintenanceThread.defineTask('ep:'+ep.Config.Name, ep.internalThread, 60+i);
        i := i + 5;
      end;
      FMaintenanceThread.defineTask('web-cache', WebServer.Common.cache.Trim, 60);
      FMaintenanceThread.defineTask('sweep-cache', sweepCaches, 60);
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
  finally
    GThreadDoingConstruction := false;
  end;
end;

procedure TFHIRServiceKernel.DoStop; 
var
  ep : TFhirServerEndpoint;
begin
  try
    Logging.shuttingDown := true;
    Logging.log('stopping: '+StopReason);

    sendSMS(Settings, Settings.HostSms, 'The server ' + DisplayName + ' for ' + FSettings.OwnerName + ' is stopping');
    Logging.log('close web server');
    if FWebServer <> nil then
      FWebServer.Close;
    Logging.log('stop internal thread');
    if FMaintenanceThread <> nil then
    begin
      FMaintenanceThread.StopAndWait(21000); // see comments in FMaintenanceThread.finalise
      FMaintenanceThread.free;
    end;
    for ep in FEndPoints do
      if (ep <> nil) then
        ep.Stopping;

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

function TFHIRServiceKernel.endpointName : String;
begin
  if not FParams.get('endpoint', result) then
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

  FI18n := TI18nSupport.Create(FTerminologies.Languages.link);
  FI18n.loadPropertiesFile(partnerFile('Messages.properties'));
  FI18n.loadPropertiesFile(partnerFile('Messages_es.properties'));
  FI18n.loadPropertiesFile(partnerFile('Messages_de.properties'));
  FI18n.loadPropertiesFile(partnerFile('Messages_nl.properties'));
  FTerminologies.i18n := FI18n.link;                   
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
  else if (v = 'r4b') then
    result := fhirVersionRelease4B
  else if (v = 'r5') then
    result := fhirVersionRelease5
  else
    result := fhirVersionUnknown;
end;

function versionOk(section : TFHIRServerConfigSection) : boolean;
begin
   result := epVersion(section) in [fhirVersionUnknown, fhirVersionRelease2, fhirVersionRelease3, fhirVersionRelease4,  fhirVersionRelease4B,  fhirVersionRelease5]
end;

procedure TFHIRServiceKernel.loadEndPoints;
var
  section : TFHIRServerConfigSection;
  ep : TFHIRServerEndPoint;
begin
  Logging.log('Load End Points');
  for section in FIni['endpoints'].sections do
  begin
    if (section['active'].valueBool) and (versionOk(section)) and (section['type'].value <> 'tx-registry') then
      FEndPoints.Add(makeEndPoint(section));
  end;

  for ep in FEndPoints do
  begin
    Logging.log('Load End Point '+ep.config.name+': '+ep.summary);
    FTelnet.addEndPoint(ep);
    ep.Load;
  end;
end;

procedure TFHIRServiceKernel.startWebServer;
var
  ep : TFHIRServerEndPoint;
begin
  FWebServer := TFhirWebServer.Create(Settings.Link, DisplayName);
  FWebServer.Common.cache := THTTPCacheManager.Create(Settings.Ini.section['web'].prop['http-cache-time'].readAsInt(0));
  FWebServer.Common.cache.cacheDwellTime := Settings.Ini.service['cache-time'].readAsInt(DEFAULT_DWELL_TIME_MIN) / (24*60);
  FTelnet.OnGetRequestList := FWebServer.GetCurrentRequestReport;
  FTelnet.OnGetCurrentRequestCount := FWebServer.GetCurrentRequestCount;

  FWebServer.loadConfiguration(Ini);
  if FolderExists('c:\work\fhirserver\server\web') then
  begin
    Logging.log('Web source from c:\work\fhirserver\server\web');
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('c:\work\fhirserver\server\web')
  end
  else if FolderExists('/Users/grahamegrieve/work/server/server/web') then
  begin
    Logging.log('Web source from /Users/grahamegrieve/work/server/server/web');
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('/Users/grahamegrieve/work/server/server/web')
  end
  else if FolderExists(FilePath([TCommandLineParameters.execDir(), '.\web'])) then
  begin
    Logging.log('Web source from ./web');
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create(FilePath([TCommandLineParameters.execDir(), '.\web']))
  end
  else if FolderExists(FilePath([TCommandLineParameters.execDir(), '..\..\server\web'])) then
  begin
    Logging.log('Web source from ../../server/web');
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create(FilePath([TCommandLineParameters.execDir(), '..\..\server\web']))
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

  FTelnet.stats := FWebServer.stats.link;
  FWebServer.Start;
end;

procedure TFHIRServiceKernel.stopWebServer;
begin
  if FWebServer <> nil then
  begin
    FTelnet.OnGetCurrentRequestCount := nil;
    FTelnet.OnGetRequestList := nil;
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

procedure TFHIRServiceKernel.unloadTerminologies;
begin
  FTerminologies.free;
  FTerminologies := nil;
end;

procedure TFHIRServiceKernel.recordStats (callback : TFhirServerMaintenanceThreadTaskCallBack);
var
  ep : TFHIRServerEndPoint;
begin
  inc(FStatsCount);
  FStatsRecord.clear;
  FStatsRecord.magic := FStatsCount;
  FStatsRecord.Threads := GetThreadCount;
  FStatsRecord.Memory := Logging.InternalMem;

  FTerminologies.recordStats(FStatsRecord);
  for ep in FEndPoints do
    ep.recordStats(FStatsRecord);
  FWebServer.recordStats(FStatsRecord);
end;

procedure TFHIRServiceKernel.sweepCaches(callback: TFhirServerMaintenanceThreadTaskCallBack);
var
  ep : TFHIRServerEndPoint;
begin
  for ep in FEndPoints do
    ep.SweepCaches;
end;

procedure TFHIRServiceKernel.dump;
begin
  // nothing?
end;

procedure TFHIRServiceKernel.StopCommand(sender: TObject);
begin
  Stop('User command', false);
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
      ep.InstallDatabase(FParams);
      Logging.log('  .. installed');
      if FParams.get('packages', fn) then
      begin
        Logging.log('  .. installing packages');
        ep.LoadPackages(FParams.has('installer'), fn);
      end;
    finally
      ep.free;
    end;
  end
  else if cmd = 'pword' then
  begin
    ep := makeEndPoint(FIni['endpoints'].section[endpointName]);
    try
      ep.updateAdminPassword(FParams.get('password'));
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
    if FParams.get('packages', fn) then
      loadTerminologies;
    try
      ep := makeEndPoint(FIni['endpoints'].section[endpointName]);
      try
        ep.UninstallDatabase;
        Logging.log('  .. uninstalled');
        ep.InstallDatabase(FParams);
        Logging.log('  .. installed');
        if FParams.get('packages', fn) then
        begin
          Logging.log('  .. installing packages');
          ep.LoadPackages(FParams.has('installer'), fn);
        end;
      finally
        ep.free;
      end;
    finally
      unloadTerminologies;
    end;
    if FParams.has('installer') then
      Logging.log('---completed ok---');
  end
  else if cmd = 'load' then
  begin
    ep := makeEndPoint(FIni['endpoints'].section[endpointName]);
    try
      if FParams.get('packages', fn) then
        ep.LoadPackages(FParams.has('installer'), fn)
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
  if config['type'].value = 'folder' then
    result := TFolderWebEndPoint.Create(config.link, FSettings.Link, FI18n.link)
  else if config['type'].value = 'icao' then
    result := TICAOWebEndPoint.Create(config.link, FSettings.Link, FI18n.link)
  else if config['type'].value = 'loinc' then
    result := TLoincWebEndPoint.Create(config.link, FSettings.Link, nil, Terminologies.link, FI18n.link)
  else if config['type'].value = 'snomed' then
    result := TSnomedWebEndPoint.Create(config.link, FSettings.Link, Terminologies.link, FI18n.link)
  else if config['type'].value = 'bridge' then
    result := TBridgeEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config, false), Terminologies.link, FPcm.link, FI18n.link)
  else if config['type'].value = 'terminology' then
    result := TTerminologyServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config, false), Terminologies.link, FPcm.link, FI18n.link)
  else if config['type'].value = 'full' then
  begin
    result := TFullServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config, false), Terminologies.link, FPcm.link, FI18n.link);
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
  mem : UInt64;
begin
  mem := Logging.InternalMem;
  callback(self, 'Checking Memory Status', -1);
  if (FMaxMem > 0) and (mem > FMaxMem) then
  begin
    Logging.log('Clear Caches because memory in use = '+DescribeBytes(mem)+', and max is '+DescribeBytes(FMaxMem));
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

procedure ExecuteFhirServer(params : TCommandLineParameters; ini : TFHIRServerConfigFile); overload;
var
  svcName : String;
  dispName : String;
  cmd : String;
  svc : TFHIRServiceKernel;
  logMsg : String;
begin           
  SetThreadStatus('ExecuteFhirServer');
  // if we're running the test or gui, go do that
  if (params.has('tests') or params.has('-tests')) then
    RunTests(params, ini)
  else if (params.has('testinsight')) then
    RunTestInsight(params, ini)
  else if (params.has('gui') or params.has('manager')) then
    RunGui(ini)
  else if (params.has('help')) then
  begin
    writeln('Health Intersections FHIR Server');
    writeln('This is the Server. For command line parameters, see ');
    writeln('http://www.healthintersections.com.au/wiki/index.php/Command_line_Parameters');
  end
  else
  begin
    if not params.get('name', svcName) then
      if ini.service['name'].value <> '' then
        svcName := ini.service['name'].value
      else
        svcName := 'FHIRServer';

    if not params.get('title', dispName) then
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

    svc := TFHIRServiceKernel.Create(svcName, dispName, compileInfo+' '+systemInfo+#13#10+logMsg, ini.link, params.link);
    try
      {$IFDEF FPC}
      if FakeConsoleForm <> nil then
        FakeConsoleForm.OnStop := svc.StopCommand;
      {$ENDIF}
      if params.get('cmd', cmd) then
      begin
        if (cmd = 'exec') or (cmd = 'console') then
          svc.ConsoleExecute
        else if (cmd = 'tests') then
          runTests(params, ini)
        else if not svc.command(cmd) then
          raise EFslException.Create('Unknown command '+cmd);
      end
      else if (isTestInsight) then
      begin
        RunTestInsight(params, ini);
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
      svc.free;
    end;
  end;
end;


var
  logFilename : String;

procedure logDebuggingInfo;
var
  s : String;
begin
  s := 'Logging to '+logFilename+'. ';
  if UnderDebugger then
    s := s + 'Being debugged. '
  else
    s := s + 'No Debugger. ';
  {$IFDEF WINDOWS}
  if SuppressLeakDialog then
    s := s + 'No Leak Dialog'
  else
    s := s + 'Leaks displayed at end.';
  {$ENDIF}
  Logging.log(s);
end;

procedure initLogging(params : TCommandLineParameters; cfg : TCustomIniFile);
begin
  if cfg.valueExists('config', 'log') then
    logFilename := cfg.readString('config', 'log', '')
  else if params.has('-tests') then
    logFilename := filePath(['[tmp]', 'fhirserver-tests.log'])
  else
    logFilename := filePath(['[tmp]', 'fhirserver.log']);
  Logging.logToFile(logFilename);
  Logging.FileLog.Policy.FullPolicy := lfpChop;
  Logging.FileLog.Policy.MaximumSize := 1024 * 1024;

  // if there's no parameters, then we don't log to the screen
  // if the cmd parameter is 'console' or 'exec' then we also log to the screen
  {$IFDEF WINDOWS}
  if ParamCount > 0 then
    Logging.LogToConsole := true;
  {$ELSE}
  Logging.LogToConsole := true;
  {$ENDIF}

  Logging.log(compileInfo);
  Logging.log(systemInfo);
  logDebuggingInfo;

  if not params.hasParams then
  begin
    {$IFDEF WINDOWS}
    Logging.log('FHIR Server running as a Service');
    {$ELSE}
    Logging.log('FHIR Server: no parameters');
    {$ENDIF}
  end
  else
    Logging.log(params.asString+' (dir='+GetCurrentDir+')');
  Logging.log('Command Line Parameters: see https://github.com/HealthIntersections/fhirserver/wiki/Command-line-Parameters-for-the-server');

end;

procedure loadDependencies;
var
  tz : TDateTime;
begin
  Logging.Log('Loading Dependencies');
  {$IFNDEF STATICLOAD_OPENSSL}
  {$IFDEF WINDOWS}
  GetOpenSSLLoader.OpenSSLPath := TCommandLineParameters.execDir();
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
  tz := TimeZoneBias;
  if tz = 0 then
    Logging.log('TimeZone: '+TimeZoneIANAName+' @ UTC')
  else if tz < 0 then
    Logging.log('TimeZone: '+TimeZoneIANAName+' @ -'+FormatDateTime('hh:nn', tz))
  else
    Logging.log('TimeZone: '+TimeZoneIANAName+' @ +'+FormatDateTime('hh:nn', tz));
  Logging.Log('Loaded');
end;

procedure ExecuteFhirServerInner(params : TCommandLineParameters);
var
  localDir : String;
  localConfig : TIniFile;
  cfg : TFHIRServerConfigFile;
  cfgName, s : String;
  fn : String;
  zc : String;
begin
  SetThreadName('kernel');
  SetThreadStatus('ExecuteFhirServerInner');
  GStartTime := GetTickCount64;

  {$IFDEF WINDOWS}
  SetConsoleTitle('FHIR Server');
  {$ENDIF}
  {$IFDEF OSX}
    localDir := GetAppConfigDir(false);
  {$ELSE}
    localDir := IncludeTrailingPathDelimiter(params.execDir());
  {$ENDIF}

  if (params.get('cfg', fn)) then
    localConfig := TIniFile.Create(fn)
  else
  begin
    localConfig := TIniFile.Create(localDir + 'fhirserver.ini');
    fn := '';
  end;
  try
    try
      initLogging(params, localConfig);
      loadDependencies;
      Logging.Log('Local config: '+localConfig.FileName+' (exists = '+booleanToString(fileExists(localConfig.FileName))+')');
      try
        if localConfig.valueExists('config', 'zero') then
          cfgName := loadRemoteConfig(params, localConfig.readString('config', 'zero', ''), localConfig)
        else if localConfig.valueExists('config', 'cfgFile') then
          cfgName := localConfig.ReadString('config', 'cfgFile', '')
        else if fn.EndsWith('.cfg') then
          cfgName := fn
        else
          cfgName := localDir + 'fhirserver.cfg';
        Logging.Log('Actual config: '+cfgName);

          // ok, now, what are we running?
        cfg := TFHIRServerConfigFile.Create(cfgName);
        try
          ExecuteFhirServer(params, cfg);
        finally
          cfg.free;
        end;
      finally
        {$IFDEF WINDOWS}
        CoUninitialize();
        {$ENDIF}
      end;
    finally
      localConfig.free;
    end;
  except
    on e : Exception do
    begin
      if params.has('installer') then
        writeln('##> Exception '+E.Message)
      else
        writeln(E.ClassName+ ': '+E.Message+#13#10#13#10+ExceptionStack(e));
      sleep(1000);
    end;
  end;
end;

procedure ExecuteFhirServerInnerEvent;
var
  cp : TCommandLineParameters;
begin
  cp := TCommandLineParameters.create;
  try
    ExecuteFhirServerInner(cp);
  finally
    cp.free;
  end;
end;

procedure ExecuteFhirServer(params : TCommandLineParameters);
{$IFDEF FPC}
begin
  if params.has('debugging') then
    UnderDebugger := true
  else
  begin
    {$IFDEF WINDOWS}
    noFMMLeakMessageBox := true;
    {$ENDIF}
    SuppressLeakDialog := true;
  end;

  if params.has('console') or params.has('fake-console') then
  begin
    RequireDerivedFormResource := True;
    Application.Title := 'FHIRServer';
    Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TFakeConsoleForm, FakeConsoleForm);
    FakeConsoleForm.caption := 'FHIRServer';
    FakeConsoleForm.Op := ExecuteFhirServerInnerEvent;
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
    ExecuteFhirServerInner(params);
  end;
end;
{$ELSE}
begin
  ExecuteFhirServerInner(params);
end;
{$ENDIF}

end.
