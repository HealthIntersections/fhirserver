unit general_endpoint;

{$i fhir.inc}

interface

uses
  SysUtils, StrUtils, Classes, IniFiles,

  IdMultipartFormData, IdHeaderList, IdCustomHTTPServer, IdHTTPServer, IdTCPServer, IdContext, IdHTTP, IdCookie, IdZLibCompressorBase, IdSSL, IdSMTP,
  IdCompressorZLib, IdZLib, IdSchedulerOfThreadPool, IdGlobalProtocols, IdMessage, IdExplicitTLSClientServerBase, IdGlobal, fsl_websocket,
  IdOpenSSLIOHandlerServer, IdOpenSSLIOHandlerClient, IdOpenSSLVersion, IdOpenSSLX509,

  fsl_base, fsl_utilities, fsl_logging, fsl_threads,
  ftx_ucum_services, fsl_http,
  fhir_objects,  fhir_factory, fhir_pathengine, fhir_parser, fhir_common,
  {$IFNDEF NO_JS}fhir_javascript, {$ENDIF}
  fsl_npm_cache,

  fhir2_factory, fhir3_factory, fhir4_factory, fhir5_factory,
  fhir2_context, fhir3_context, fhir4_context, fhir5_context,
  fhir2_indexinfo, fhir3_indexinfo, fhir4_indexinfo, fhir5_indexinfo,
  indexing_r2, indexing_r3, indexing_r4, indexing_r5,
  subscriptions_r2, subscriptions_r3, subscriptions_r4, subscriptions_r5,
  operations_r2, operations_r3, operations_r4, operations_r5,
  fhir2_validator, fhir3_validator, fhir4_validator, fhir5_validator,
  validator_r2, validator_r3, validator_r4, validator_r5,
  {$IFNDEF NO_JS} fhir2_javascript, fhir3_javascript, fhir4_javascript, fhir5_javascript, {$ENDIF}
  fhir2_pathengine, fhir3_pathengine, fhir4_pathengine, fhir5_pathengine,

  fdb_manager,
  fhir_indexing,
  tx_manager, tx_server,
  tx_unii,
  scim_server,
  database_installer, server_version, server_config, utilities,
  server_context,
  {$IFNDEF NO_JS}server_javascript, {$ENDIF}
  storage, database,
  server_factory, indexing, subscriptions,
  web_base, endpoint;

Type
  TGeneralServerEndPoint = class;

  TKernelServerFactory = class (TFHIRServerFactory)
  private
    FVersion : TFHIRVersion;
  public
    constructor Create(version : TFHIRVersion);

    function makeIndexes : TFHIRIndexBuilder; override;
    function makeValidator: TFHIRValidatorV; override;
    function makeIndexer : TFHIRIndexManager; override;
    function makeEngine(context : TFHIRWorkerContextWithFactory; ucum : TUcumServiceImplementation) : TFHIRPathEngineV; override;
    function makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager; override;

    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); override;
  end;

  TPackageLoader = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FBundle: TFHIRBundleW;
  public
    constructor Create(factory : TFHIRFactory);
    destructor Destroy; override;

    property bundle : TFHIRBundleW read FBundle;
    procedure load(rType, id : String; stream : TStream);
  end;

  TFHIRServerThread = class (TThread)
  protected
    FServer: TGeneralServerEndPoint;
  public
    constructor Create(server: TGeneralServerEndPoint; suspended : boolean);
  end;

  TFhirServerEmailThread = class (TFHIRServerThread) // todo: make this a TFslThread
  protected
    procedure Execute; override;
  public
    constructor Create(server: TGeneralServerEndPoint);
  end;

  TFhirServerSubscriptionThread = class(TFHIRServerThread)
  protected
    procedure Execute; override;
  public
    constructor Create(server: TGeneralServerEndPoint);
  end;

  TGeneralServerEndPoint = class (TFHIRServerEndPoint)
  private
    FSubscriptionThread: TFhirServerSubscriptionThread;
    FEmailThread: TFhirServerEmailThread;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager);
    destructor Destroy; override;

    function summary : String; override;
    procedure internalThread; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
  end;

//  if (true {active and threads}) then
//  begin
//    FSubscriptionThread := TFhirServerSubscriptionThread.Create(self);
//    FEmailThread := TFhirServerEmailThread.Create(self);
//  end;
//  if FSubscriptionThread <> nil then
//    FSubscriptionThread.Terminate;
//  if FEmailThread <> nil then
//    FEmailThread.Terminate;
{
    procedure convertFromVersion(stream : TStream; format : TFHIRFormat; version : TFHIRVersion; const lang : THTTPLanguages);
    procedure convertToVersion(stream : TStream; format : TFHIRFormat; version : TFHIRVersion; const lang : THTTPLanguages);
    function encodeAsyncResponseAsJson(request : TFHIRRequest; reqUrl : String; secure : boolean; fmt : TFHIRFormat; transactionTime : TFslDateTime; names : TStringList) : string;

}


implementation


{ TKernelServerFactory }

constructor TKernelServerFactory.create(version : TFHIRVersion);
begin
  inherited Create;
  FVersion := version;
end;

function TKernelServerFactory.makeValidator: TFHIRValidatorV;
begin
  case FVersion of
    fhirVersionRelease2 : result := TFHIRValidator2.Create(TFHIRServerWorkerContextR2.Create(TFHIRFactoryR2.create));
    fhirVersionRelease3 : result := TFHIRValidator3.Create(TFHIRServerWorkerContextR3.Create(TFHIRFactoryR3.create));
    fhirVersionRelease4 : result := TFHIRValidator4.Create(TFHIRServerWorkerContextR4.Create(TFHIRFactoryR4.create));
    fhirVersionRelease5 : result := TFHIRValidator5.Create(TFHIRServerWorkerContextR5.Create(TFHIRFactoryR5.create));
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating Validator');
  end;
end;

function TKernelServerFactory.makeEngine(context: TFHIRWorkerContextWithFactory; ucum: TUcumServiceImplementation): TFHIRPathEngineV;
begin
  case FVersion of
    fhirVersionRelease2 : result := TFHIRPathEngine2.Create(context as TFHIRWorkerContext2, ucum);
    fhirVersionRelease3 : result := TFHIRPathEngine3.Create(context as TFHIRWorkerContext3, ucum);
    fhirVersionRelease4 : result := TFHIRPathEngine4.Create(context as TFHIRWorkerContext4, ucum);
    fhirVersionRelease5 : result := TFHIRPathEngine5.Create(context as TFHIRWorkerContext5, ucum);
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating FHIRPathEngine');
  end;
end;

function TKernelServerFactory.makeIndexer : TFHIRIndexManager;
begin
  case FVersion of
    fhirVersionRelease2 : result := TFhirIndexManager2.Create;
    fhirVersionRelease3 : result := TFhirIndexManager3.Create;
    fhirVersionRelease4 : result := TFhirIndexManager4.Create;
    fhirVersionRelease5 : result := TFhirIndexManager5.Create;
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating Indexes');
  end;
end;

function TKernelServerFactory.makeIndexes: TFHIRIndexBuilder;
begin
  case FVersion of
    fhirVersionRelease2 : result := TFHIRIndexBuilderR2.create;
    fhirVersionRelease3 : result := TFHIRIndexBuilderR3.create;
    fhirVersionRelease4 : result := TFHIRIndexBuilderR4.create;
    fhirVersionRelease5 : result := TFHIRIndexBuilderR5.create;
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating index information');
  end;
end;

function TKernelServerFactory.makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager;
begin
  case FVersion of
    fhirVersionRelease2 : result := TSubscriptionManagerR2.Create(ServerContext);
    fhirVersionRelease3 : result := TSubscriptionManagerR3.Create(ServerContext);
    fhirVersionRelease4 : result := TSubscriptionManagerR4.Create(ServerContext);
    fhirVersionRelease5 : result := TSubscriptionManagerR5.Create(ServerContext);
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating subcription manager');
  end;
end;

procedure TKernelServerFactory.setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer});
begin
  case FVersion of
    fhirVersionRelease2 : TFHIRServerWorkerContextR2(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease3 : TFHIRServerWorkerContextR3(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease4 : TFHIRServerWorkerContextR4(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease5 : TFHIRServerWorkerContextR5(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Setting Terminology Server');
  end;
end;

function factoryFactory(v : TFHIRVersion) : TFHIRFactory;
begin
  case v of
    fhirVersionRelease2: result := TFHIRFactoryR2.create;
    fhirVersionRelease3: result := TFHIRFactoryR3.create;
    fhirVersionRelease4: result := TFHIRFactoryR4.create;
    fhirVersionRelease5: result := TFHIRFactoryR5.create;
  else
    raise EFHIRUnsupportedVersion.Create(v, 'creating factory');
  end;
end;

{ TPackageLoader }

constructor TPackageLoader.Create(factory: TFHIRFactory);
begin
  inherited Create;
  FFactory := factory;
  FBundle := FFactory.wrapBundle(FFactory.makeResource('Bundle'));
end;

destructor TPackageLoader.Destroy;
begin
  FFactory.Free;
  FBundle.Free;
  inherited;
end;

procedure TPackageLoader.load(rType, id: String; stream: TStream);
var
  p : TFHIRParser;
begin
  p := FFactory.makeParser(nil, ffJson, THTTPLanguages.create('en'));
  try
    FBundle.addEntry.resource := p.parseResource(stream);
  finally
    p.Free;
  end;

end;


{ TGeneralServerEndPoint }

constructor TGeneralServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager);
begin

end;

destructor TGeneralServerEndPoint.Destroy;
begin

  inherited;
end;

procedure TGeneralServerEndPoint.internalThread;
begin

                 (*
      if not terminated and (FLastSweep < Now - (DATETIME_SECOND_ONE * 5)) then
      begin
        try
          FServer.Settings.MaintenanceThreadStatus := 'Sweeping Sessions';
          for ep in FServer.FEndPoints do
            ep.Context.Storage.Sweep;
          if not FServer.settings.ForLoad then
            FServer.Common.Google.commit;
        except
        end;
        FLastSweep := Now;
      end;
      if not FServer.settings.ForLoad then
      begin
        if (not terminated) then
          try
            FServer.Settings.MaintenanceThreadStatus := 'Building Indexes';
            for ep in FServer.FEndPoints do
              ep.Context.TerminologyServer.BuildIndexes(false);
          except
          end;
      end;
      if (not terminated) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Processing Observations';
          for ep in FServer.FEndPoints do
            ep.Context.Storage.ProcessObservations;
        except
        end;
      if (not terminated) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Checking Snomed';
          FServer.FEndPoints.First.Context.TerminologyServer.CommonTerminologies.sweepSnomed;
        except
        end;
      if (not terminated) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Checking Async Tasks';
          for ep in FServer.FEndPoints do
            ep.CheckAsyncTasks;
        except
        end;
      if (not terminated) and (FServer.FTwilioServer <> nil) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Sweeping Twilio';
          FServer.FTwilioServer.sweep;
        except
        end;
      if (not terminated) then
        try
          FServer.Settings.MaintenanceThreadStatus := 'Sweeping Client Cache';
          for ep in FServer.FEndPoints do
            ep.Context.ClientCacheManager.sweep;
        except
        end;
        *)
end;

function TGeneralServerEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  raise Exception.Create('Not Done Yet');
end;

function TGeneralServerEndPoint.summary: String;
begin
  result := 'FHIR Server for Version ? using '+describeDatabase(Config);
end;

(*
procedure TFhirWebServer.convertFromVersion(stream: TStream; format : TFHIRFormat; version: TFHIRVersion; const lang : THTTPLanguages);
var
  b : TBytes;
begin
  {$IfDEF NO_CONVERSION}
  raise EFHIRException.create('Version Conversion Services are not made available on this server');
  {$ELSE}
  b := StreamToBytes(stream);
  b := TFhirVersionConvertors.convertResource(b, format, OutputStyleNormal, lang, version, CURRENT_FHIR_VERSION);
  stream.Size := 0;
  stream.Write(b[0], length(b));
  stream.Position := 0;
  {$ENDIF}
end;

procedure TFhirWebServer.convertToVersion(stream: TStream; format : TFHIRFormat; version: TFHIRVersion; const lang : THTTPLanguages);
var
  b : TBytes;
begin
  {$IfDEF NO_CONVERSION}
  raise EFHIRException.create('Version Conversion Services are not made available on this server');
  {$ELSE}
  b := StreamToBytes(stream);
  b := TFhirVersionConvertors.convertResource(b, format, OutputStyleNormal, lang, CURRENT_FHIR_VERSION, version);
  stream.Size := 0;
  stream.Write(b[0], length(b));
  stream.Position := 0;
  {$ENDIF}
end;

*)

{ TFhirServerSubscriptionThread }

constructor TFhirServerSubscriptionThread.Create(server: TGeneralServerEndPoint);
begin
  FreeOnTerminate := true;
  FServer := server;
  inherited Create(server, false);
end;

procedure TFhirServerSubscriptionThread.Execute;
var
  ep : TFhirWebServerEndpoint;
begin
  SetThreadName('Server Subscription Thread');
  SetThreadStatus('Working');
  {$IFNDEF NO_JS}
  GJsHost := TJsHost.Create;
  FServer.OnRegisterJs(self, GJsHost);
  {$ENDIF}
//  GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;
  Logging.log('Starting TFhirServerSubscriptionThread');
  try
    FServer.Settings.SubscriptionThreadStatus := 'starting';
    repeat
      FServer.Settings.SubscriptionThreadStatus := 'sleeping';
      sleep(1000);
      FServer.Settings.SubscriptionThreadStatus := 'processing subscriptions';
      // todo FServer.Context.Storage.ProcessSubscriptions;
    until terminated;
    try
      FServer.Settings.SubscriptionThreadStatus := 'dead';
    except
    end;
    try
      FServer.FSubscriptionThread := nil;
    except
    end;
    Logging.log('Ending TFhirServerSubscriptionThread');
  except
    Logging.log('Failing TFhirServerSubscriptionThread');
  end;
  {$IFNDEF NO_JS}
  GJsHost.Free;
  GJsHost := nil;
  {$ENDIF}
  SetThreadStatus('Done');
  closeThread;
end;

{ TFhirServerEmailThread }

constructor TFhirServerEmailThread.Create(server: TGeneralServerEndPoint);
begin
  FreeOnTerminate := true;
  inherited Create(server, false);
end;

procedure TFhirServerEmailThread.Execute;
var
  i: integer;
  ep : TFhirWebServerEndpoint;
begin
  SetThreadName('Server Email Thread');
  SetThreadStatus('Working');
  {$IFNDEF NO_JS}
  GJsHost := TJsHost.Create;
  FServer.OnRegisterJs(self, GJsHost);
  {$ENDIF}
//  GJsHost.registry := FServer.ServerContext.EventScriptRegistry.Link;
  Logging.log('Starting TFhirServerEmailThread');
  try
    FServer.Settings.EmailThreadStatus := 'starting';
    repeat
      FServer.Settings.EmailThreadStatus := 'sleeping';
      i := 0;
      while not terminated and (i < 60) do
      begin
        sleep(1000);
        inc(i);
      end;
      if not terminated then
      begin
        FServer.Settings.EmailThreadStatus := 'processing Emails';
        // todo FServer.Context.Storage.ProcessEmails;
      end;
    until terminated;
    try
      FServer.Settings.EmailThreadStatus := 'dead';
    except
    end;
    try
      FServer.FEmailThread := nil;
    except
    end;
    Logging.log('Ending TFhirServerEmailThread');
  except
    Logging.log('Failing TFhirServerEmailThread');
  end;
  {$IFNDEF NO_JS}
  GJsHost.Free;
  GJsHost := nil;
  {$ENDIF}
  SetThreadStatus('Done');
  closeThread;
end;

{ TFHIRServerThread }

constructor TFHIRServerThread.Create(server: TGeneralServerEndPoint; suspended: boolean);
begin
  FServer := server;
  inherited Create(suspended);
end;




//function TFhirWebServer.registerEndPoint(code, path: String; context: TFHIRServerContext; ini : TFHIRServerConfigFile): TFhirWebServerEndpoint;
//begin
//  result := TFhirWebServerEndpoint.create(code, path, context, Common.Link);
//  FEndPoints.Add(result);
//  context.userProvider.OnProcessFile := result.ReturnProcessedFile;
//  result.AuthServer := TAuth2Server.Create(context.Factory.link, ini, Common.Host, inttostr(Common.SslPort), path);
//  result.AuthServer.UserProvider := context.userProvider.Link;
//  result.AuthServer.ServerContext := context.Link;
//  result.AuthServer.EndPoint := result.ClientAddress(true);
//  result.AuthServer.OnProcessFile := result.ReturnProcessedFile;
//  result.AuthServer.OnGetPatients := result.GetPatients;
//  result.AuthServer.OnProcessLaunchParams := result.GetLaunchParameters;
//  result.AuthServer.Active := true;
//  context.JWTServices := TJWTServices.Create;
//  context.JWTServices.Cert := FCertFile;
//  context.JWTServices.Password := FSSLPassword;
//  context.JWTServices.DatabaseId := context.DatabaseId;
//  context.JWTServices.Host := Common.Host;
//  context.FormalURLPlain := 'http://'+Common.host+port(Common.ActualPort, 80)+path;
//  context.FormalURLSecure := 'https://'+Common.host+port(Common.ActualSSLPort, 443)+path;
////  context.JWTServices.JWKAddress := ?;
//end;



end.