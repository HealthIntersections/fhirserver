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
  {$IFDEF FPC} {odbcsqldyn, }gui_lcl, {$ELSE} gui_vcl, {$ENDIF}

  IdOpenSSLLoader,

  fsl_base, fsl_utilities, fsl_fpc, fsl_logging, fsl_threads,
  {$IFDEF WINDOWS} fsl_service_win, {$ELSE} fsl_service, {$ENDIF}
  fdb_manager,

  server_constants, server_config, utilities, {$IFNDEF NO_JS}server_javascript, {$ENDIF}
  tx_manager, telnet_server, web_source, web_server,
  server_testing,
  endpoint, endpoint_bridge, endpoint_txsvr, endpoint_packages, endpoint_loinc, endpoint_snomed, endpoint_full;


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
  TFhirServerMaintenanceThread = class (TFslThread)
  private
    FLastSweep: TDateTime;
    FEndPoints : TFslList<TFHIRServerEndPoint>;
  protected
    procedure Execute; override;
  public
    constructor Create(endPoints : TFslList<TFHIRServerEndPoint>);
    destructor Destroy; override;
  end;

type
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

    function command(cmd: String): boolean;
  protected
    FStartTime : cardinal;
  public
    constructor Create(const ASystemName, ADisplayName, Welcome : String; ini : TFHIRServerConfigFile);
    destructor Destroy; override;

    function CanStart : boolean; Override;
    procedure postStart; override;
    procedure DoStop; Override;
    procedure dump; override;

//    property loadStore : boolean read FLoadStore write FLoadStore;
    property Telnet : TFHIRTelnetServer read FTelnet;
    property Ini : TFHIRServerConfigFile read FIni;
    property Settings : TFHIRServerSettings read FSettings;
    property WebServer : TFhirWebServer read FWebServer;
    property Terminologies : TCommonTerminologies read FTerminologies;
    property EndPoints : TFslList<TFHIRServerEndPoint> read FEndPoints;
  end;


procedure ExecuteFhirServer; overload;

implementation

uses
  {$IFDEF WINDOWS} JclDebug {$ENDIF};

{ TFhirServerMaintenanceThread }

constructor TFhirServerMaintenanceThread.Create(endPoints : TFslList<TFHIRServerEndPoint>);
begin
  inherited Create();
  FLastSweep := Now;
  FEndPoints := endPoints;
end;

destructor TFhirServerMaintenanceThread.Destroy;
begin
  FEndPoints.free;
  inherited;
end;

procedure TFhirServerMaintenanceThread.Execute;
var
  ep : TFhirServerEndpoint;
begin
  SetThreadName('Server Maintenance Thread');
  SetThreadStatus('Working');
  Logging.log('Starting TFhirServerMaintenanceThread');
  try
{$IFDEF WINDOWS}
    CoInitialize(nil);
{$ENDIF}
    {$IFNDEF NO_JS}
    GJsHost := TJsHost.Create;
// todo    FServer.Common.OnRegisterJs(self, GJsHost);
    {$ENDIF}
//    GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;

    repeat
//      FServer.Settings.MaintenanceThreadStatus := 'sleeping';
      sleep(10);
      if not terminated then
        for ep in FEndPoints do
          ep.internalThread;
    until terminated;
//    try
//      FServer.Settings.MaintenanceThreadStatus := 'dead';
//    except
//    end;
//    try
//      FServer.FMaintenanceThread := nil;
//    except
//    end;
    {$IFNDEF NO_JS}
    GJsHost.Free;
    GJsHost := nil;
    {$ENDIF}


{$IFDEF WINDOWS}
    CoUninitialize;
{$ENDIF}
    Logging.log('Ending TFhirServerMaintenanceThread');
  except
    Logging.log('Failing TFhirServerMaintenanceThread');
  end;
  SetThreadStatus('Done');
  closeThread;
end;

{ TFHIRServiceKernel }

constructor TFHIRServiceKernel.Create(const ASystemName, ADisplayName, Welcome: String; ini: TFHIRServerCOnfigFile);
begin
  FStartTime := GetTickCount;
  inherited create(ASystemName, ADisplayName);
  FTelnet := TFHIRTelnetServer.Create(44123, Welcome);
  FIni := ini;
  FTelnet.Password := FIni.web['telnet-password'].value;

  FSettings := TFHIRServerSettings.Create;
  FSettings.ForLoad := not hasCommandLineParam('noload');
  FSettings.load(FIni);

  FEndPoints := TFslList<TFHIRServerEndPoint>.create;
end;

destructor TFHIRServiceKernel.Destroy;
begin
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
    FMaintenanceThread := TFhirServerMaintenanceThread.Create(FEndPoints.link);
    FMaintenanceThread.Start;

    // post start up time.
    getReport('|', true); // base line the object counting
    Logging.log('started ('+inttostr((GetTickCount - FStartTime) div 1000)+'secs)');
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
//  js.engine.registerFactory(fhir2_javascript.registerFHIRTypes, fhirVersionRelease2, TFHIRFactoryR2.create);
//  js.engine.registerFactory(fhir3_javascript.registerFHIRTypes, fhirVersionRelease3, TFHIRFactoryR3.create);
//  js.engine.registerFactory(fhir4_javascript.registerFHIRTypes, fhirVersionRelease4, TFHIRFactoryR4.create);
//  js.engine.registerFactory(fhir4_javascript.registerFHIRTypesDef, fhirVersionUnknown, TFHIRFactoryR4.create);
//  js.engine.registerFactory(fhir5_javascript.registerFHIRTypes, fhirVersionRelease5, TFHIRFactoryR5.create);
end;
{$ENDIF}

// --- core functionality ------------------------------------------------------

procedure TFHIRServiceKernel.loadTerminologies;
begin
  FTerminologies := TCommonTerminologies.Create(Settings.link);
  FTerminologies.load(Ini['terminologies'], false);
end;

procedure TFHIRServiceKernel.loadEndPoints;
var
  section : TFHIRServerConfigSection;
  ep : TFHIRServerEndPoint;
begin
  for section in FIni['endpoints'].sections do
  begin
    if section['active'].valueBool then
      FEndPoints.Add(makeEndPoint(section));
  end;

  for ep in FEndPoints do
  begin
    Logging.log('Load End Point '+ep.summary);
    ep.Load;
  end;
end;

procedure TFHIRServiceKernel.StartWebServer;
var
  ep : TFHIRServerEndPoint;
begin
  FWebServer := TFhirWebServer.create(Settings.Link, Telnet.Link, DisplayName);
  {$IFNDEF NO_JS}
  FWebServer.Common.OnRegisterJs := registerJs;
  {$ENDIF}
  FWebServer.loadConfiguration(Ini);
  if FolderExists('c:\work\fhirserver\server\web') then
  begin
    Logging.log('Web source from c:\work\fhirserver\server\web');
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('c:\work\fhirserver\server\web')
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
  // todo: registerEndPoints;

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
    ep.Unload;
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
  name, fn : String;
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
    ep := makeEndPoint(FIni['endpoints'].section[endpointName]);
    try
      ep.UninstallDatabase;
      ep.InstallDatabase;
      if getCommandLineParam('packages', fn) then
        ep.LoadPackages(fn);
    finally
      ep.free;
    end;
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
    result := false;
end;

function TFHIRServiceKernel.makeEndPoint(config : TFHIRServerConfigSection) : TFHIRServerEndPoint;
begin
  // we generate by type and mode
  if config['type'].value = 'package' then
    result := TPackageServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Telnet.Link, Terminologies.link)
  else if config['type'].value = 'loinc' then
    result := TLoincWebEndPoint.Create(config.link, FSettings.Link, nil, Telnet.Link, Terminologies.link)
  else if config['type'].value = 'snomed' then
    result := TSnomedWebEndPoint.Create(config.link, FSettings.Link, Telnet.Link, Terminologies.link)
  else if config['type'].value = 'bridge' then
    result := TBridgeEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Telnet.Link, Terminologies.link)
  else if config['type'].value = 'terminology' then
    result := TTerminologyServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Telnet.Link, Terminologies.link)
  else if config['type'].value = 'full' then
    result := TFullServerEndPoint.Create(config.link, FSettings.Link, connectToDatabase(config), Telnet.Link, Terminologies.link)
  else
    raise Exception.Create('Unknown server type ' +config['type'].value);
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
  fn : String;
  svc : TFHIRServiceKernel;
  logMsg : String;
begin
  {$IFDEF WINDOWS}
  SetConsoleTitle('FHIR Server');
  {$ENDIF}

  // 1. logging.
  if getCommandLineParam('log', fn) then
    Logging.logToFile(fn);

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
    // if there's no parameters, then we don't log to the screen
    // if the cmd parameter is 'console' or 'exec' then we also log to the
    if Logging.FileLog = nil then
    begin
      if (FileExists('c:\temp')) then
        Logging.logToFile('c:\temp\fhirserver.log')
      else
        Logging.logToFile(tempFile('fhirserver.log'));
    end;
    if ParamCount > 0 then
      Logging.LogToConsole := true;

    Logging.log(commandLineAsString);

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
      logMsg := 'FHIR Server '+SERVER_FULL_VERSION+' '+Logging.buildDetails+'. Using ini file '+ini.FileName+' (+stack dumps)'
    else
    {$ENDIF}
      logMsg := 'FHIR Server '+SERVER_FULL_VERSION+' '+Logging.buildDetails+'. Using ini file '+ini.FileName;
    if Logging.FileLog <> nil then
      logMsg := logMsg + '. Log File = '+Logging.FileLog.filename;

    Logging.log(logMsg);
    dispName := dispName + ' '+SERVER_FULL_VERSION+' '+Logging.buildDetails+'';

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

procedure ExecuteFhirServer;
var
  cfg : TFHIRServerConfigFile;
  cfgName : String;
begin
  {$IFDEF WINDOWS}
  JclStartExceptionTracking;
  GetOpenSSLLoader.OpenSSLPath := ExtractFilePath(Paramstr(0));
  {$ENDIF}
  try
    {$IFDEF FPC}
    initialiseTZData(partnerFile('tzdata.tar.gz'));
    {$ENDIF}
    {$IFDEF WINDOWS}
    CoInitialize(nil);
    {$ENDIF}
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

