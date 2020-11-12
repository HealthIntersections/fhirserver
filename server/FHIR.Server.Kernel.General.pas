unit FHIR.Server.Kernel.General;

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
  SysUtils, StrUtils, Classes, IniFiles,

  fsl_base, fsl_utilities, fsl_logging,
  ftx_ucum_services, fsl_http,
  fhir_objects,  fhir_factory, fhir_pathengine, fhir_parser, fhir_common,
  {$IFNDEF NO_JS}fhir_javascript, {$ENDIF}
  fsl_npm_cache,

  fhir2_factory, fhir3_factory, fhir4_factory, fhir5_factory,
  fhir2_context, fhir3_context, fhir4_context, fhir5_context,
  fhir2_indexinfo, fhir3_indexinfo, fhir4_indexinfo, fhir5_indexinfo,
  FHIR.Server.IndexingR2, FHIR.Server.IndexingR3, FHIR.Server.IndexingR4, FHIR.Server.IndexingR5,
  FHIR.Server.SubscriptionsR2, FHIR.Server.SubscriptionsR3, FHIR.Server.SubscriptionsR4, FHIR.Server.SubscriptionsR5,
  FHIR.Server.OperationsR2, FHIR.Server.OperationsR3, FHIR.Server.OperationsR4, FHIR.Server.OperationsR5,
  fhir2_validator, fhir3_validator, fhir4_validator, fhir5_validator,
  FHIR.Server.ValidatorR2, FHIR.Server.ValidatorR3, FHIR.Server.ValidatorR4, FHIR.Server.ValidatorR5,
  {$IFNDEF NO_JS} fhir2_javascript, fhir3_javascript, fhir4_javascript, fhir5_javascript, {$ENDIF}
  fhir2_pathengine, fhir3_pathengine, fhir4_pathengine, fhir5_pathengine,

  fdb_manager,
  fhir_indexing,
  FHIR.Tx.Manager, FHIR.Tx.Server,
  FHIR.Tx.Unii,
  FHIR.Scim.Server,
  FHIR.Server.DBInstaller, FHIR.Server.Version, FHIR.Server.Ini, FHIR.Server.Utilities,
  FHIR.Server.Context,
  {$IFNDEF NO_JS}FHIR.Server.Javascript, {$ENDIF}
  FHIR.Server.Storage, FHIR.Server.Database,
  FHIR.Server.Factory, FHIR.Server.Indexing, FHIR.Server.Subscriptions, FHIR.Server.Web,
  FHIR.Server.Kernel.Base;

Type
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

  TFHIRServiceGeneral = class (TFHIRServiceDataStore)
  private
    FPackageUpdater : TPackageUpdaterThread;
    FContexts : TFslMap<TFHIRServerContext>;
    FKnownVersion : TFHIRVersion;
    FNotServing : boolean;

    function doGetNamedContext(sender : TObject; name : String) : TFHIRServerContext;
    Procedure checkDatabase(db : TFslDBManager; factory : TFHIRFactory; serverFactory : TFHIRServerFactory);

    // commands
    function InstallDatabase(name : String) : String;
    procedure updateAdminPassword(name : String);
    procedure UnInstallDatabase(name : String);
    procedure LoadPackages(name, plist : String);
//
//    property NotServing : boolean read FNotServing write FNotServing;
  protected
    function initialise : boolean; override;
    function setup : boolean; override;
    procedure closeDown; override;
    {$IFNDEF NO_JS}
    procedure registerJs(sender: TObject; js: TJsHost); override;
    {$ENDIF}
    procedure registerEndPoints; override;
    function WantActive : boolean; override;
    function WantThreads : boolean; override;
 public
    destructor Destroy; override;
    function command(cmd : String) : boolean; override;
  end;

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
  conn : TFslDBConnection;
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

Procedure TFHIRServiceGeneral.checkDatabase(db : TFslDBManager; factory : TFHIRFactory; serverFactory : TFHIRServerFactory);
var
  ver : integer;
  conn : TFslDBConnection;
  dbi : TFHIRDatabaseInstaller;
  meta : TFslDBMetaData;
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
  db : TFslDBManager;
  dbi : TFHIRDatabaseInstaller;
  scim : TSCIMServer;
  salt, un, pw, em, sql, dr : String;
  conn : TFslDBConnection;
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
  db : TFslDBManager;
  dbi : TFHIRDatabaseInstaller;
  conn : TFslDBConnection;
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
  db : TFslDBManager;
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
  db : TFslDBManager;
  scim : TSCIMServer;
  salt, un, pw, em, result : String;
  conn : TFslDBConnection;
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
