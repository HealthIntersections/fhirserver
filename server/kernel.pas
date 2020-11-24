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

  fsl_base, fsl_utilities, fsl_fpc, fsl_logging,
  {$IFDEF WINDOWS} fsl_service_win, {$ELSE} fsl_service, {$ENDIF}
  fdb_manager,

  server_constants, server_ini, utilities, {$IFNDEF NO_JS}server_javascript, {$ENDIF}
  tx_manager, telnet_server, web_source, webserver,
  server_testing,
  endpoint, general_endpoint, bridge_endpoint, terminology_endpoint, package_endpoint;


(*

type
  TFHIRServiceTxServer = class (TFHIRServiceDataStore)
  private
    FStores : TFslMap<TTerminologyFhirServerStorage>;

    procedure configureResource(cfg : TFHIRResourceConfig);
    procedure registerEndPoint(code, path : String; db : TFDBManager; factory : TFHIRFactory; listF, listP : TStringList; UTGFolder : String);
  protected
    function setup : boolean; override;
    procedure closeDown; override;
    procedure registerEndPoints; override;
    function WantActive : boolean; override;
    function WantThreads : boolean; override;
  public
    destructor Destroy; override;
    function command(cmd : String) : boolean; override;
  end;

*)
type
  TFHIRServiceKernel = class (TSystemService)
  private
    Fcallback: TInstallerCallback;
    FLoadStore : boolean;
    FIni : TFHIRServerIniFile;
    FSettings : TFHIRServerSettings;
    FProgress : integer;
    FProgressName : string;

    FDatabases : TFslMap<TFDBManager>;

    FTerminologies : TCommonTerminologies;
    FTelnet : TFHIRTelnetServer;
    FWebServer : TFhirWebServer;
    FEndPoints : TFslList<TFHIRServerEndPoint>

    procedure StartWebServer;
    procedure StopWebServer;
    procedure ConnectToDatabases;
    procedure CloseDatabases;
    procedure LoadTerminologies;
    procedure UnloadTerminologies;
  protected
    FStartTime : cardinal;

    procedure cb(i: integer; s: WideString);
    procedure fetchProgress(sender: TObject; progess: integer);
    procedure fetchProgress2(sender: TObject; pct: integer; done: boolean; desc: String);
    procedure finishProgress(status: String);
    procedure resetProgress(name: String);

    function initialise : boolean; // called while starting service;
    function setup : boolean;  // called once service is stated to have started
    procedure registerEndPoints; // this is where subclasses actually add all their functionality
    {$IFNDEF NO_JS}
    procedure registerJs(sender: TObject; js: TJsHost); virtual;
    {$ENDIF}
    procedure closeDown;
    function WantActive : boolean; virtual;
    function WantThreads : boolean; virtual;
  public
    constructor Create(const ASystemName, ADisplayName, Welcome : String; ini : TFHIRServerIniFile);
    destructor Destroy; override;

    function CanStart : boolean; Override;
    procedure postStart; override;
    procedure DoStop; Override;
    procedure dump; override;

    property callback : TInstallerCallback read Fcallback write Fcallback;
    property loadStore : boolean read FLoadStore write FLoadStore;
    property Telnet : TFHIRTelnetServer read FTelnet;
    property Ini : TFHIRServerIniFile read FIni;
    property Settings : TFHIRServerSettings read FSettings;
    property WebServer : TFhirWebServer read FWebServer;
    property Databases : TFslMap<TFDBManager> read FDatabases;
    property Terminologies : TCommonTerminologies read FTerminologies;

    function command(cmd : String) : boolean; virtual;
  end;


procedure ExecuteFhirServer; overload;

implementation

uses
  {$IFDEF WINDOWS} JclDebug {$ENDIF};

constructor TFHIRServiceKernel.Create(const ASystemName, ADisplayName, Welcome: String; ini: TFHIRServerIniFile);
begin
  FStartTime := GetTickCount;
  inherited create(ASystemName, ADisplayName);
  FTelnet := TFHIRTelnetServer.Create(44123, Welcome);
  FIni := ini;
  FTelnet.Password := FIni.web['telnet-password'];

  FSettings := TFHIRServerSettings.Create;
  FSettings.ForLoad := not hasCommandLineParam('noload');
  FSettings.load(FIni);
end;

destructor TFHIRServiceKernel.Destroy;
begin
  FDatabases.Free;
  FIni.Free;
  FSettings.Free;
  FTelnet.Free;
  inherited;
end;

function TFHIRServiceKernel.CanStart: boolean;
begin
  Logging.log('Run Number '+inttostr(Settings.RunNumber));
  try
    result := initialise;
  except
    on e : Exception do
    begin
      Logging.log('Error starting: '+E.ClassName+ ': ' + E.Message+#13#10#13#10+ExceptionStack(e));
      raise;
    end;
  end;
end;

procedure TFHIRServiceKernel.postStart;
begin
  if setup then
    StartWebServer();
  getReport('|', true);
  Logging.log('started ('+inttostr((GetTickCount - FStartTime) div 1000)+'secs)');
  Logging.Starting := false;
end;

procedure TFHIRServiceKernel.DoStop;
begin
  try
    Logging.log('stopping: '+StopReason);
    Logging.log('stop web server');
    StopWebServer;
    Logging.log('closing');
    closeDown;
    Logging.log('stopped');
  except
    on e : Exception do
      Logging.log('Exception stopping ('+E.ClassName + '): ' + E.Message+#13#10#13#10+ExceptionStack(e));
  end;
end;

procedure TFHIRServiceKernel.StartWebServer;
begin
  FWebServer := TFhirWebServer.create(Settings.Link, Telnet.Link, DisplayName);
  {$IFNDEF NO_JS}
  FWebServer.Common.OnRegisterJs := registerJs;
  {$ENDIF}
  FWebServer.loadConfiguration(Ini);
  if FolderExists('c:\work\fhirserver\server\webn') then
  begin
    Logging.log('Web source from '+Ini.web['folder']);
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('c:\work\fhiserver\server\web')
  end
  else if FileExists(partnerFile('fhirserver.web')) then
  begin
    Logging.log('Web source from '+partnerFile('fhirserver.web'));
    FWebServer.Common.SourceProvider := TFHIRWebServerSourceZipProvider.Create(partnerFile('fhirserver.web'))
  end
  else
    raise Exception.Create('Unable to find web source');

  registerEndPoints;

  FWebServer.Start(WantActive, WantThreads);
end;

procedure TFHIRServiceKernel.StopWebServer;
begin
  if FWebServer <> nil then
  begin
    FWebServer.Stop;
    FWebServer.free;
  end;
end;

procedure TFHIRServiceKernel.dump;
begin
  // nothing?
end;

function TFHIRServiceKernel.command(cmd: String): boolean;
begin
  result := false;
end;

function TFHIRServiceKernel.WantActive: boolean;
begin
  result := false;
end;

function TFHIRServiceKernel.WantThreads: boolean;
begin
  result := false;
end;

function TFHIRServiceKernel.initialise: boolean;
begin
  FDatabases := TFslMap<TFDBManager>.create('fhir.svc');
  ConnectToDatabases;
  result := true;
end;

function TFHIRServiceKernel.setup: boolean;
begin
  if FTerminologies = nil then
    LoadTerminologies;
  result := true;
end;

procedure TFHIRServiceKernel.registerEndPoints;
begin
  // todo...

end;

procedure TFHIRServiceKernel.closeDown;
begin
  UnloadTerminologies;
  CloseDatabases;
end;

{$IFNDEF NO_JS}
procedure TFHIRServiceKernel.registerJs(sender: TObject; js: TJsHost);
begin
  // nothing
end;
{$ENDIF}

procedure TFHIRServiceKernel.cb(i: integer; s: WideString);
begin
  if Assigned(Fcallback) then
    Fcallback(i, s);
end;

procedure TFHIRServiceKernel.resetProgress(name: String);
begin
  FProgress := 0;
  FProgressName := name;
  Logging.start(name+' ');
end;

procedure TFHIRServiceKernel.fetchProgress(sender: TObject; progess: integer);
begin
  if Assigned(Fcallback) then
    callback(progess, FProgressName)
  else
  begin
    if progess > FProgress then
    begin
      Logging.continue('.');
      FProgress := FProgress + 5;
    end;
  end;
end;

procedure TFHIRServiceKernel.fetchProgress2(sender: TObject; pct: integer; done: boolean; desc: String);
begin
  if Assigned(Fcallback) then
    callback(pct, FProgressName)
  else
  begin
    if pct > FProgress then
    begin
      Logging.continue('.');
      FProgress := FProgress + 5;
    end;
  end;
end;

procedure TFHIRServiceKernel.finishProgress(status: String);
begin
  if not Assigned(Fcallback) then
    Logging.finish(' '+status);
end;


Procedure TFHIRServiceKernel.ConnectToDatabases();
var
  s : String;
  details : TFHIRServerIniComplex;
begin
  Logging.log('Load Databases. Config file = '+Ini.FileName);
  for s in Ini.databases.keys do
  begin
    details := Ini.databases[s];
    FDatabases.Add(s, connectToDatabase(s, details));
  end;
  Logging.log('Databases Loaded');
end;

procedure TFHIRServiceKernel.CloseDatabases;
begin
  if FDatabases <> nil then
    FDatabases.Clear;
end;


procedure TFHIRServiceKernel.LoadTerminologies;
begin
  FTerminologies := TCommonTerminologies.Create(Settings.link);
  FTerminologies.load(Ini, FDatabases, false);
end;

procedure TFHIRServiceKernel.UnloadTerminologies;
begin
  FTerminologies.Free;
  FTerminologies := nil;
end;

{ === Core ====================================================================}

procedure RunGui(ini : TFHIRServerIniFile);
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

procedure ExecuteFhirServer(ini : TFHIRServerIniFile); overload;
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
      if ini.service['name'] <> '' then
        svcName := ini.service['name']
      else
        svcName := 'FHIRServer';

    if not getCommandLineParam('title', dispName) then
      if ini.service['title'] <> '' then
        dispName := ini.service['title']
      else
        dispName := 'FHIR Server';

    {$IFDEF WINDOWS}
    if JclExceptionTrackingActive then
      logMsg := 'FHIR Server '+SERVER_VERSION+' '+Logging.buildDetails+'. Using ini file '+ini.FileName+' (+stack dumps)'
    else
    {$ENDIF}
      logMsg := 'FHIR Server '+SERVER_VERSION+' '+Logging.buildDetails+'. Using ini file '+ini.FileName;
    if Logging.FileLog <> nil then
      logMsg := logMsg + '. Log File = '+Logging.FileLog.filename;

    Logging.log(logMsg);
    dispName := dispName + ' '+SERVER_VERSION+' '+Logging.buildDetails+'';

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
  ini : TFHIRServerIniFile;
  iniName : String;
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
        if not getCommandLineParam('ini', iniName) then
        {$IFDEF OSX}
          iniName := GetAppConfigDir(false)+'fhirserver.ini';
        {$ELSE}
          iniName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'fhirserver.ini';
        {$ENDIF}

        ini := TFHIRServerIniFile.create(iniName);
        try
          ExecuteFhirServer(ini);
        finally
          ini.free;
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

(*
{ TFHIRServiceTxServer }

destructor TFHIRServiceTxServer.Destroy;
begin
  FStores.Free;
  inherited;
end;

procedure TFHIRServiceTxServer.configureResource(cfg : TFHIRResourceConfig);
begin
  cfg.Supported := true;
  cfg.cmdSearch := true;
  cfg.cmdOperation := true;
  cfg.cmdUpdate := false;
  cfg.cmdDelete := false;
  cfg.cmdValidate := false;
  cfg.cmdHistoryInstance := false;
  cfg.cmdHistoryType := false;
  cfg.cmdCreate := false;
  cfg.cmdVRead := false;
end;

function TFHIRServiceTxServer.setup: boolean;
begin
  FStores := TFslMap<TTerminologyFhirServerStorage>.create('Tx.Stores');
  result := inherited setup;
end;

procedure TFHIRServiceTxServer.registerEndPoint(code, path : String; db : TFDBManager; factory : TFHIRFactory; listF, listP : TStringList; UTGFolder : String);
var
  s : String;
  store : TTerminologyFhirServerStorage;
begin
  if UTGFolder <> '' then
    Logging.log('Load Terminology EndPoint for '+factory.versionString+'. UTG = "'+UTGFolder+'"')
  else
    Logging.log('Load Terminology EndPoint for '+factory.versionString+'');

  store := TTerminologyFhirServerStorage.Create(factory.link);
  try
    store.FServerContext := TFHIRServerContext.Create(store.Link, TTerminologyServerFactory.create(factory.version));
    store.FServerContext.Globals := Settings.Link;
    Telnet.addContext(store.FServerContext);
    store.FServerContext.TerminologyServer := TTerminologyServer.Create(db.link, factory.Link, Terminologies.link);
    store.FServerContext.userProvider := TTerminologyFHIRUserProvider.Create;
    if factory.version <> fhirVersionRelease2 then
      configureResource(store.FServerContext.ResConfig['CodeSystem']);
    configureResource(store.FServerContext.ResConfig['ValueSet']);
    configureResource(store.FServerContext.ResConfig['NamingSystem']);
    configureResource(store.FServerContext.ResConfig['ConceptMap']);

    store.loadPackage(factory, factory.corePackage, false);
    if UTGFolder <> '' then
      store.loadUTGFolder(factory, UTGFolder)
    else if factory.txPackage <> '' then
      store.loadPackage(factory, factory.txPackage, true);
    store.loadPackage(factory, factory.txSupportPackage, false);
    for s in listP do
      store.loadPackage(factory, s, false);
    for s in listF do
      store.loadFile(factory, s);

    WebServer.registerEndPoint('r4', path, store.FServerContext.Link, ini);
    FStores.Add(code, store.link);
  finally
    store.Free;
  end;
end;

procedure TFHIRServiceTxServer.registerEndPoints;
var
  s : String;
  details : TFHIRServerIniComplex;
  factory : TFHIRFactory;
  listF : TStringList;
  listP : TStringList;
begin
  for s in Ini.endpoints.sortedKeys do
  begin
    details := Ini.endpoints[s];
    Logging.log('Initialise endpoint '+s+' at '+details['path']+' for '+details['version']);

    if details['version'] = 'r2' then
    begin
      factory := TFHIRFactoryR2.Create;
    end
    else if details['version'] = 'r3' then
    begin
      factory := TFHIRFactoryR3.Create;
    end
    else if details['version'] = 'r4' then
    begin
      factory := TFHIRFactoryR4.Create;
    end
    else if details['version'] = 'r5' then
    begin
      factory := TFHIRFactoryR5.Create;
    end
    else
      raise EFslException.Create('Cannot load end-point '+s+' version '+details['version']);
    try
      listF := TStringList.create;
      listP := TStringList.create;
      try
//        listF.CommaText := ini.kernel[details['version']+'-files'];
//        listP.CommaText := ini.kernel[details['version']+'-packages'];
//        registerEndPoint(s, details['path'], Databases[details['database']], factory, listF, listP, ini.kernel['utg-folder']);
      finally
        listF.Free;
        listP.Free;
      end;
    finally
      factory.Free;
    end;
  end;
  WebServer.IsTerminologyServerOnly := true;
end;

procedure TFHIRServiceTxServer.closeDown;
var
  t : TTerminologyFhirServerStorage;
begin
  if FStores <> nil then
  begin
    for t in FStores.values do
    begin
      telnet.removeContext(t.FServerContext);
      t.FServerContext.free;
    end;
    FStores.Clear;
  end;
  inherited;
end;

function TFHIRServiceTxServer.WantActive: boolean;
begin
  result := true;
end;

function TFHIRServiceTxServer.WantThreads: boolean;
begin
  result := false;
end;

function TFHIRServiceTxServer.command(cmd: String): boolean;
begin
  result := false;
end;

*)

{ TFHIRServiceGeneral }

destructor TFHIRServiceGeneral.Destroy;
begin
  FPackageUpdater.Free;
  FContexts.Free;
  inherited;
end;

function TFHIRServiceGeneral.initialise : boolean;
begin
  result := inherited initialise;
  FContexts := TFslMap<TFHIRServerContext>.create('server contexts');
end;

function TFHIRServiceGeneral.setup : boolean;
begin
  result := inherited setup;
end;

procedure TFHIRServiceGeneral.closeDown;
var
  ctxt : TFHIRServerContext;
begin
  for ctxt in FContexts.Values do
    Telnet.removeContext(ctxt);
  if FPackageUpdater <> nil then
    FPackageUpdater.Terminate;
  inherited CloseDown;
end;

function TFHIRServiceGeneral.WantActive : boolean;
begin
  result := not FNotServing;
end;

function TFHIRServiceGeneral.WantThreads : boolean;
begin
  result := not FNotServing;
end;

function TFHIRServiceGeneral.doGetNamedContext(sender: TObject; name: String): TFHIRServerContext;
begin
  result := FContexts[name];
end;

{$IFNDEF NO_JS}
procedure TFHIRServiceGeneral.registerJs(sender : TObject; js : TJsHost);
begin
  js.engine.registerFactory(fhir2_javascript.registerFHIRTypes, fhirVersionRelease2, TFHIRFactoryR2.create);
  js.engine.registerFactory(fhir3_javascript.registerFHIRTypes, fhirVersionRelease3, TFHIRFactoryR3.create);
  js.engine.registerFactory(fhir4_javascript.registerFHIRTypes, fhirVersionRelease4, TFHIRFactoryR4.create);
  js.engine.registerFactory(fhir4_javascript.registerFHIRTypesDef, fhirVersionUnknown, TFHIRFactoryR4.create);
  js.engine.registerFactory(fhir5_javascript.registerFHIRTypes, fhirVersionRelease5, TFHIRFactoryR5.create);
end;
{$ENDIF}

procedure TFHIRServiceGeneral.registerEndPoints;
var
  ctxt : TFHIRServerContext;
  store : TFHIRNativeStorageService;
  s : String;
  details : TFHIRServerIniComplex;
  conn : TFDBConnection;
begin
  store := nil;

  for s in Ini.endpoints.sortedKeys do
  begin
    details := Ini.endpoints[s];
    Logging.log('Initialise endpoint '+s+' at '+details['path']+' for '+details['version']);

    case FKnownVersion of
      fhirVersionRelease2: if details['version'] <> 'r2' then continue;
      fhirVersionRelease3: if details['version'] <> 'r3' then continue;
      fhirVersionRelease4: if details['version'] <> 'r4' then continue;
      fhirVersionRelease5: if details['version'] <> 'r5' then continue;
    end;

    if hasCommandLineParam('r5') and (details['version'] <> 'r5') then
      continue;
    if hasCommandLineParam('r4') and (details['version'] <> 'r4') then
      continue;
    if hasCommandLineParam('r3') and (details['version'] <> 'r3') then
      continue;
    if hasCommandLineParam('r2') and (details['version'] <> 'r2') then
      continue;

    if details['version'] = 'r2' then
    begin
      store := TFHIRNativeStorageServiceR2.create(Databases[details['database']].Link, TFHIRFactoryR2.Create);
    end
    else if details['version'] = 'r3' then
    begin
      store := TFHIRNativeStorageServiceR3.create(Databases[details['database']].Link, TFHIRFactoryR3.Create);
    end
    else if details['version'] = 'r4' then
    begin
      store := TFHIRNativeStorageServiceR4.create(Databases[details['database']].Link, TFHIRFactoryR4.Create);
    end
    else if details['version'] = 'r5' then
    begin
      store := TFHIRNativeStorageServiceR5.create(Databases[details['database']].Link, TFHIRFactoryR5.Create);
    end
    else
      raise EFslException.Create('Cannot load end-point '+s+' version '+details['version']);

    try
      ctxt := TFHIRServerContext.Create(store.Link, TKernelServerFactory.create(store.Factory.version));
      try
        Telnet.addContext(ctxt);
        FContexts.add(s, ctxt.Link);
        ctxt.OnGetNamedContext := doGetNamedContext;
        Logging.log('  .. check DB '+details['database']);
        checkDatabase(store.DB, store.Factory, ctxt.ServerFactory);
        ctxt.Globals := Settings.Link;
        store.ServerContext := ctxt;
        ctxt.TerminologyServer := TTerminologyServer.Create(store.DB.link, ctxt.factory.Link, Terminologies.link);
        Logging.log('  .. load DB '+details['database']);
        store.Initialise();
        ctxt.userProvider := TSCIMServer.Create(store.db.link, Ini.admin['scim-salt'], WebServer.host, Ini.admin['default-rights'], false);
        WebServer.registerEndPoint(s, details['path'], ctxt.Link, Ini);
      finally
        ctxt.Free;
      end;
    finally
      store.Free;
    end;
  end;

  for ctxt in FContexts.values do
  begin
    conn := (TFHIRServerContext(ctxt).Storage as TFHIRNativeStorageService).DB.GetConnection('Done-Loading');
    try
      ctxt.doneLoading(conn);
      conn.release;
    except
      on e : Exception do
      begin
        conn.error(e);
        raise;
      end;
    end;
  end;

//      FWebServer.CDSHooksServer.registerService(TCDAHooksCodeViewService.create);
//      FWebServer.CDSHooksServer.registerService(TCDAHooksIdentifierViewService.create);
//      FWebServer.CDSHooksServer.registerService(TCDAHooksPatientViewService.create);
//      FWebServer.CDSHooksServer.registerService(TCDAHackingHealthOrderService.create);

  if Ini.web['package-server'] <> '' then
  begin
    WebServer.PackageServer.DB := Databases[Ini.web['package-server']].Link;
    Logging.log('Starting TPackageUpdaterThread (db = '+Ini.web['package-server']+')');
    FPackageUpdater := TPackageUpdaterThread.Create(WebServer, WebServer.PackageServer.DB.Link);
  end;
end;

function endpoint : String;
begin
  if not getCommandLineParam('endpoint', result) then
    raise EFslException.Create('No endpoint parameter supplied');
end;

function TFHIRServiceGeneral.command(cmd : String) : boolean;
var
  name, fn : String;
begin
  result := true;
  if cmd = 'mount' then
  begin
    CanStart;
    try
      name := InstallDatabase(endpoint);
      if getCommandLineParam('unii', fn) then
        ImportUnii(fn, Databases[name]);
    finally
      closeDown;
    end;
  end
  else if cmd = 'pword' then
  begin
    CanStart;
    try
      updateAdminPassword(endpoint);
    finally
      closeDown;
    end;
  end
  else if cmd = 'unmount' then
  begin
    CanStart;
    try
      UninstallDatabase(endpoint);
    finally
      closeDown;
    end;
  end
  else if cmd = 'remount' then
  begin
    CanStart;
    try
      UninstallDatabase(endpoint);
      name := InstallDatabase(endpoint);
      if getCommandLineParam('unii', fn) then
        ImportUnii(fn, Databases[name]);
      if getCommandLineParam('packages', fn) then
      begin
        LoadPackages(endpoint, fn);
      end;
    finally
      closeDown;
    end;
  end
  else if cmd = 'load' then
  begin
    CanStart;
    try
      if getCommandLineParam('packages', fn) then
        LoadPackages(name, fn)
      else
        Logging.log('no Packages to install');
    finally
      closeDown;
    end;
  end
  else
  begin
    result := inherited command(cmd);
  end
end;

Procedure TFHIRServiceGeneral.checkDatabase(db : TFDBManager; factory : TFHIRFactory; serverFactory : TFHIRServerFactory);
var
  ver : integer;
  conn : TFDBConnection;
  dbi : TFHIRDatabaseInstaller;
  meta : TFDBMetaData;
begin
  conn := Db.GetConnection('check version');
  try
    meta := conn.FetchMetaData;
    try
      if meta.HasTable('Config') then
      begin
        // db version check
        ver := conn.CountSQL('Select Value from Config where ConfigKey = 5');
        if (ver <> ServerDBVersion) then
        begin
          Logging.log('Upgrade Database from version '+inttostr(ver)+' to '+inttostr(ServerDBVersion));
          dbi := TFHIRDatabaseInstaller.create(conn, factory.link, serverfactory.link);
          try
            dbi.upgrade(ver);
          finally
            dbi.Free;
          end;
        end;
      end;
    finally
      meta.Free;
    end;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

function TFHIRServiceGeneral.InstallDatabase(name : String) : String;
var
  db : TFDBManager;
  dbi : TFHIRDatabaseInstaller;
  scim : TSCIMServer;
  salt, un, pw, em, sql, dr : String;
  conn : TFDBConnection;
  details : TFHIRServerIniComplex;
  v : TFHIRVersion;
begin
  // check that user account details are provided
  salt := Ini.admin['scim-salt'];
  if (salt = '') then
    raise EFHIRException.create('You must define a scim salt in the ini file');
  dr := Ini.admin['default-rights'];
  if (dr = '') then
  begin
    Ini.admin['default-rights'] := 'openid,fhirUser,profile,user/*.*';
    dr := Ini.admin['default-rights'];
  end;
  if (dr = '') then
    raise EFHIRException.create('You must define some default rights for SCIM users in the ini file');
  un := Ini.admin['username'];
  if (un = '') then
    raise EFHIRException.create('You must define an admin username in the ini file');
  getCommandLineParam('password', pw);
  if (pw = '') then
    raise EFHIRException.create('You must provide a admin password as a parameter to the command');
  em := Ini.admin['email'];
  if (em = '') then
    raise EFHIRException.create('You must define an admin email in the ini file');

  sql := 'C:\work\fhirserver\sql';
  if not DirectoryExists(sql) then
    sql := IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0)))+'sql';

  details := Ini.endpoints[name];
  if details = nil then
    raise EFslException.Create('Undefined endpoint '+name);

  if details['version'] = 'r2' then
    v := fhirVersionRelease2
  else if details['version'] = 'r3' then
    v := fhirVersionRelease3
  else if details['version'] = 'r4' then
    v := fhirVersionRelease4
  else if details['version'] = 'r5' then
    v := fhirVersionRelease5
  else
    raise EFslException.Create('unknown version '+details['version']);

  result := details['database'];
  if result = '' then
    raise EFslException.Create('No defined database '+name);
  if Databases.ContainsKey(result) then
    db := Databases[result].Link
  else
    db := connectToDatabase(result, Ini.databases[result]);
  try
    Logging.log('mount endpoint '+result+' on '+db.DBDetails);
    scim := TSCIMServer.Create(db.Link, salt, Ini.web['host'], Ini.admin['default-rights'], true);
    try
      conn := db.GetConnection('setup');
      try
        dbi := TFHIRDatabaseInstaller.create(conn, factoryFactory(v), TKernelServerFactory.create(v));
        try
          dbi.callback := callback;
          dbi.Bases.Add('http://healthintersections.com.au/fhir/argonaut');
          dbi.Bases.Add('http://hl7.org/fhir');
          dbi.Install(scim);
        finally
          dbi.free;
        end;
        scim.DefineAnonymousUser(conn);
        scim.DefineAdminUser(conn, un, pw, em);
        conn.Release;
        Logging.log('done');
      except
         on e:exception do
         begin
           Logging.log('Error: '+e.Message);
           conn.Error(e);
           recordStack(e);
           raise;
         end;
      end;
    finally
      scim.Free;
    end;
  finally
    db.free;
  end;
end;

procedure TFHIRServiceGeneral.UnInstallDatabase;
var
  db : TFDBManager;
  dbi : TFHIRDatabaseInstaller;
  conn : TFDBConnection;
  n : String;
  details : TFHIRServerIniComplex;
  v : TFHIRVersion;
begin
  details := Ini.endpoints[name];
  if details = nil then
    raise EFslException.Create('Undefined endpoint '+name);

  if details['version'] = 'r2' then
    v := fhirVersionRelease2
  else if details['version'] = 'r3' then
    v := fhirVersionRelease3
  else if details['version'] = 'r4' then
    v := fhirVersionRelease4
  else if details['version'] = 'r5' then
    v := fhirVersionRelease5
  else
    raise EFslException.Create('unknown version '+details['version']);

  n := details['database'];
  if n = '' then
    raise EFslException.Create('No defined database '+name);
  if Databases.ContainsKey(n) then
    db := Databases[n].Link
  else
    db := connectToDatabase(n, Ini.databases[n]);
  try
    Logging.log('unmount database '+n+' at '+db.DBDetails);
    conn := db.GetConnection('setup');
    try
      dbi := TFHIRDatabaseInstaller.create(conn, factoryFactory(v), TKernelServerFactory.create(v));
      try
        dbi.callback := callback;
        dbi.UnInstall;
      finally
        dbi.free;
      end;
      conn.Release;
      Logging.log('done');
    except
       on e:exception do
       begin
         Logging.log('Error: '+e.Message);
         conn.Error(e);
         recordStack(e);
         raise;
       end;
    end;
  finally
    db.Free;
  end;
end;

procedure TFHIRServiceGeneral.LoadPackages(name, plist: String);
var
  p, pi, pv : String;
  pl : TArray<String>;
  ploader : TPackageLoader;
  pcm : TFHIRPackageManager;
  li : TPackageLoadingInformation;
  details : TFHIRServerIniComplex;
  v : TFHIRVersion;
  dbn : String;
  db : TFDBManager;
  loadList : TArray<String>;
begin
  details := Ini.endpoints[name];
  if details = nil then
    raise EFslException.Create('Undefined endpoint '+name);

  if details['version'] = 'r2' then
    v := fhirVersionRelease2
  else if details['version'] = 'r3' then
    v := fhirVersionRelease3
  else if details['version'] = 'r4' then
    v := fhirVersionRelease4
  else if details['version'] = 'r5' then
    v := fhirVersionRelease5
  else
    raise EFslException.Create('unknown version '+details['version']);

  FknownVersion := v;
  FNotServing := true;

  cb(1, 'Load: start kernel');
  postStart;

  dbn := details['database'];
  if dbn = '' then
    raise EFslException.Create('No defined database '+name);
  pcm := TFHIRPackageManager.Create(false);
  try
    pcm.OnWork := fetchProgress2;
    resetProgress('Package hl7.fhir.r4.core#4.0.1');
    if not pcm.autoInstallPackage('hl7.fhir.r4.core', '4.0.1') then
      raise EFHIRException.create('Package hl7.fhir.r4.core#4.0.1 not found');
    finishProgress('ok');


    db := Databases[dbn].Link;
    try
      Logging.log('Load: register value sets');
      // identifyValueSets(db);
      li := TPackageLoadingInformation.Create(PF_CONST[v]);
      try
        pl := plist.Split([',']);
        for p in pl do
        begin
          if p <> '' then
          begin
            StringSplit(p, '#', pi, pv);
            resetProgress('Package '+p);
            if not pcm.autoInstallPackage(pi, pv) then
              raise EFHIRException.create('Package '+p+' not found');
            finishProgress('ok');
            ploader := TPackageLoader.create(factoryFactory(v));
            try
              li.OnLoadEvent := ploader.load;
              loadList := ploader.FFactory.resourceNames;
              Logging.log('Load Package '+pi+'#'+pv);
              pcm.loadPackage(pi, pv, loadList, li);
              WebServer.EndPoint(name).Transaction(ploader.bundle, true, p, '', opmCmdLine, callback);
            finally
              ploader.Free;
            end;
          end;
        end;
      finally
        li.Free;
      end;

      Logging.log('loaded');
      cb(95, 'Building Terminology Closure Tables');
      WebServer.EndPoint(name).Context.TerminologyServer.BuildIndexes(not assigned(callback));
      cb(100, 'Cleaning up');

      DoStop;
    finally
      db.free;
    end;
  finally
    pcm.Free;
  end;
end;

procedure TFHIRServiceGeneral.updateAdminPassword(name: String);
var
  db : TFDBManager;
  scim : TSCIMServer;
  salt, un, pw, em, result : String;
  conn : TFDBConnection;
  details : TFHIRServerIniComplex;
begin
  // check that user account details are provided
  salt := Ini.admin['scim-salt'];
  if (salt = '') then
    raise EFHIRException.create('You must define a scim salt in the ini file');
  un := Ini.admin['username'];
  if (un = '') then
    raise EFHIRException.create('You must define an admin username in the ini file');
  getCommandLineParam('password', pw);
  if (pw = '') then
    raise EFHIRException.create('You must provide a admin password as a parameter to the command');
  em := Ini.admin['email'];
  if (em = '') then
    raise EFHIRException.create('You must define an admin email in the ini file');

  details := Ini.endpoints[name];
  if details = nil then
    raise EFslException.Create('Undefined endpoint '+name);

  result := details['database'];
  if result = '' then
    raise EFslException.Create('No defined database '+name);
  if Databases.ContainsKey(result) then
    db := Databases[result].Link
  else
    db := connectToDatabase(result, Ini.databases[result]);
  try
    Logging.log('fix admin password for '+result);
    scim := TSCIMServer.Create(db.Link, salt, Ini.web['host'], Ini.admin['default-rights'], true);
    try
      conn := db.GetConnection('setup');
      try
        scim.UpdateAdminUser(conn, un, pw, em);
        conn.Release;
        Logging.log('done');
      except
         on e:exception do
         begin
           Logging.log('Error: '+e.Message);
           conn.Error(e);
           recordStack(e);
           raise;
         end;
      end;
    finally
      scim.Free;
    end;
  finally
    db.free;
  end;
end;

end.


function makeKernel(const ASystemName, ADisplayName, Welcome : String; ini : TFHIRServerIniFile) : TFHIRServiceKernel;
var
  mode : String;
begin
  mode := ini.service['mode'];
  if mode = 'bridge' then
  begin
    Logging.log('Mode: Bridge Server');
    result := TFHIRServiceBridgeServer.Create(ASystemName, ADisplayName, Welcome, ini)
  end
  else if mode = 'tx' then
  begin
    Logging.log('Mode: Terminology Server');
    result := TFHIRServiceTxServer.Create(ASystemName, ADisplayName, Welcome, ini)
  end
  else if (mode = 'general') or (mode = '') then
  begin
    Logging.log('Mode: General purpose Server');
    result := TFHIRServiceGeneral.Create(ASystemName, ADisplayName, Welcome, ini)
  end
  else
    raise Exception.Create('Unknown kernel mode '+mode);
end;


