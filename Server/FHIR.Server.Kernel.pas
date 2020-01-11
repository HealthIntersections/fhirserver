unit FHIR.Server.Kernel;

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


{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

Uses
  Windows, SysUtils, Classes, IniFiles, ActiveX, ComObj,

  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Service,
  FHIR.Web.Fetcher,
  FHIR.Snomed.Importer, FHIR.Snomed.Services, FHIR.Snomed.Expressions, FHIR.Tx.RxNorm, FHIR.Tx.Unii,
  FHIR.Loinc.Importer, FHIR.Loinc.Services, FHIR.Ucum.Services,
  FHIR.Database.Manager, FHIR.Database.ODBC, FHIR.Database.Dialects, FHIR.Database.SQLite,
  FHIR.Base.Factory, FHIR.Cache.PackageManager, FHIR.Base.Parser, FHIR.Base.Lang, FHIR.Javascript.Base, FHIR.Base.Common, FHIR.Base.PathEngine,

  FHIR.R2.Factory, FHIR.R3.Factory, FHIR.R4.Factory, FHIR.R5.Factory,
  FHIR.R2.Context, FHIR.R3.Context, FHIR.R4.Context, FHIR.R5.Context,
  FHIR.R2.IndexInfo, FHIR.R3.IndexInfo, FHIR.R4.IndexInfo, FHIR.R5.IndexInfo,
  FHIR.Server.IndexingR2, FHIR.Server.IndexingR3, FHIR.Server.IndexingR4, FHIR.Server.IndexingR5,
  FHIR.Server.SubscriptionsR2, FHIR.Server.SubscriptionsR3, FHIR.Server.SubscriptionsR4, FHIR.Server.SubscriptionsR5,
  FHIR.Server.OperationsR2, FHIR.Server.OperationsR3, FHIR.Server.OperationsR4, FHIR.Server.OperationsR5,
  FHIR.R2.Validator, FHIR.R3.Validator, FHIR.R4.Validator, FHIR.R5.Validator,
  FHIR.Server.ValidatorR2, FHIR.Server.ValidatorR3, FHIR.Server.ValidatorR4, FHIR.Server.ValidatorR5,
  FHIR.R2.Javascript, FHIR.R3.Javascript, FHIR.R4.Javascript, FHIR.R5.Javascript,
  FHIR.R2.PathEngine, FHIR.R3.PathEngine, FHIR.R4.PathEngine, FHIR.R5.PathEngine,

  FHIR.Tools.Indexing,
  FHIR.Tx.Manager, FHIR.Tx.Server,
  FHIR.Server.Storage,
  FHIR.Server.Web, FHIR.Server.DBInstaller, FHIR.Server.Database, FHIR.Base.Objects,
  FHIR.Server.Constants, FHIR.Server.Context, FHIR.Server.Utilities, FHIR.Server.WebSource,
  FHIR.Scim.Server, FHIR.CdsHooks.Service, FHIR.Server.Javascript, FHIR.Server.Factory,
  FHIR.Server.Indexing, FHIR.Server.Subscriptions, FHIR.Server.Manager, FHIR.Server.Ini, FHIR.Server.Version;

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

  TFHIRService = class (TSystemService)
  private
    FStartTime : cardinal;
    FIni : TFHIRServerIniFile;
    FSettings : TFHIRServerSettings;
    FDatabases : TFslMap<TKDBManager>;
    FTerminologies : TCommonTerminologies;
    FWebServer : TFhirWebServer;

    FNotServing : boolean;
    FLoadStore : boolean;
    FInstaller : boolean;
    Fcallback: TInstallerCallback;
    FKnownVersion : TFHIRVersion;
    FProgress : integer;
    FProgressName : string;

    function connectToDatabase(s : String; details : TFHIRServerIniComplex) : TKDBManager;
    Procedure checkDatabase(db : TKDBManager; factory : TFHIRFactory; serverFactory : TFHIRServerFactory);
    procedure ConnectToDatabases();
    procedure LoadTerminologies;
    procedure InitialiseRestServer(version : TFHIRVersion);
    procedure StopRestServer;
    procedure UnloadTerminologies;
    procedure CloseDatabase;
    procedure validate;
    procedure InstallerCallBack(i : integer; s : String);
    procedure cb(i : integer; s : WideString);
    procedure identifyValueSets(db : TKDBManager);
//    function RegisterValueSet(id: String; conn: TKDBConnection): integer;
    procedure registerJs(sender: TObject; js: TJsHost);
    function getLoadResourceList(factory: TFHIRFactory; mode: TFHIRInstallerSecurityMode): TArray<String>;
    procedure resetProgress(name : String);
    procedure finishProgress(status : String);
    procedure fetchProgress(sender : TObject; progess : integer);
    procedure fetchProgress2(sender : TObject; pct : integer; done : boolean; desc : String);
  protected
    function CanStart : boolean; Override;
    procedure postStart; override;
    procedure DoStop; Override;
    procedure dump; override;
  public
    constructor Create(const ASystemName, ADisplayName, AIniName: String);
    destructor Destroy; override;

    procedure Load(name, plist : String; mode : TFHIRInstallerSecurityMode);
//    procedure LoadbyProfile(fn : String; init : boolean);
    procedure Index;
    function InstallDatabase(name : String; securityMode : TFHIRInstallerSecurityMode) : String;
    procedure UnInstallDatabase(name : String);
    procedure updateAdminPassword(name : String);

    property NotServing : boolean read FNotServing write FNotServing;
    property callback : TInstallerCallback read Fcallback write Fcallback;
  end;

procedure ExecuteFhirServer;

implementation

uses
  FHIR.Support.Logging, JclDebug;

procedure CauseException;
begin
  raise EFHIRException.create('Test');
end;

function endpoint : String;
begin
  if not FindCmdLineSwitch('endpoint', result, true, [clstValueNextParam]) then
    raise EFslException.Create('No endpoint parameter supplied');
end;

procedure ExecuteFhirServer;
var
  iniName : String;
  svcName : String;
  dispName : String;
  cmd : String;
  fn, fn2, name : String;
  svc : TFHIRService;
  smode, plist : String;
  mode : TFHIRInstallerSecurityMode;
begin
  //AllocConsole;
  try
    Consolelog := true;
    if FindCmdLineSwitch('log', fn, true, [clstValueNextParam]) then
    begin
      filelog := true;
      logfile := fn;
    end;

    CoInitialize(nil);
    if not FindCmdLineSwitch('ini', iniName, true, [clstValueNextParam]) then
      iniName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'fhirserver.ini';
    if not FindCmdLineSwitch('name', svcName, true, [clstValueNextParam]) then
      svcName := 'FHIRServer';
    if not FindCmdLineSwitch('title', dispName, true, [clstValueNextParam]) then
      dispName := 'FHIR Server';
    iniName := iniName.replace('.dstu', '.dev');

    if JclExceptionTrackingActive then
      logt('FHIR Service '+SERVER_VERSION+'. Using ini file '+iniName+' with stack dumps on')
    else
      logt('FHIR Service '+SERVER_VERSION+'. Using ini file '+iniName+' (no stack dumps)');
    if filelog then
      logt('Log File = '+logfile);
    dispName := dispName + ' '+SERVER_VERSION;

    svc := TFHIRService.Create(svcName, dispName, iniName);
    try
        GJsHost := TJsHost.Create;
        try
          if FindCmdLineSwitch('installer') then
          begin
            svc.FInstaller := true;
            svc.callback := svc.InstallerCallBack;
          end;

          svc.FLoadStore := not FindCmdLineSwitch('noload');

          if FindCmdLineSwitch('cmd', cmd, true, [clstValueNextParam]) then
          begin
            if cmd = 'manager' then
            begin
              FreeConsole;
              ServerManagerForm := TServerManagerForm.Create(nil);
              try
                ServerManagerForm.ShowModal;
              finally
                FreeAndNil(ServerManagerForm);
              end;
            end
            else if cmd = 'mount' then
            begin
              if FindCmdLineSwitch('mode', smode, true, [clstValueNextParam]) then
                mode := readInstallerSecurityMode(smode)
              else
                mode := ismUnstated;
              name:= svc.InstallDatabase(endpoint, mode);
              if FindCmdLineSwitch('unii', fn, true, [clstValueNextParam]) then
                ImportUnii(fn,  svc.FDatabases[name]);
            end
            else if cmd = 'pword' then
              svc.updateAdminPassword(endpoint)
            else if cmd = 'unmount' then
              svc.UninstallDatabase(endpoint)
            else if cmd = 'remount' then
            begin
              if FindCmdLineSwitch('mode', smode, true, [clstValueNextParam]) then
                mode := readInstallerSecurityMode(smode)
              else
                mode := ismUnstated;
              svc.DebugMode := true;
              svc.FNotServing := true;
              svc.UninstallDatabase(endpoint);
              name := svc.InstallDatabase(endpoint, mode);
              if FindCmdLineSwitch('unii', fn2, true, [clstValueNextParam]) then
                ImportUnii(fn2, svc.FDatabases[name]);
  //            if FindCmdLineSwitch('profile', fn, true, [clstValueNextParam]) then
  //              svc.LoadByProfile(fn, true)
  //            else
              if FindCmdLineSwitch('packages', fn2, true, [clstValueNextParam]) then
                svc.Load(endpoint, fn2, mode);
            end
            else if cmd = 'load' then
            begin
              svc.DebugMode := true;
              svc.FNotServing := true;
              if not FindCmdLineSwitch('endpoint', name, true, [clstValueNextParam]) then
                raise Exception.Create('Must provide an endpoint for loading a package');
              if not FindCmdLineSwitch('package', plist, true, [clstValueNextParam]) then
                raise Exception.Create('Must specify a package');
              svc.Load(name, plist, ismUnstated);
            end
            else if cmd = 'txdirect' then
            begin
              svc.UninstallDatabase(fn);
              svc.InstallDatabase(fn, ismTerminologyServer);
              svc.ConsoleExecute;
            end
  //          else if cmd = 'profile' then
  //            svc.LoadByProfile(fn, false)
            else if cmd = 'validate' then
            begin
              svc.FNotServing := true;
              svc.validate;
            end
            else if cmd = 'index' then
              svc.index
            else if cmd = 'unii' then
            begin
              svc.ConnectToDatabases;
             // ImportUnii(fn, svc.Fdb)
            end
            else if cmd = 'exec' then
              svc.ConsoleExecute
            else
              raise EFslException.Create('Unknown command '+cmd);
          end
          else
          begin
            try
              writeln('No -cmd parameter - exiting now'); // won't see this if an actual service
            except
              // catch 105 err
            end;
            svc.Execute;
          end;
        finally
          GJsHost.free;
        end;
//      finally
//        factory.Free;
//      end;
    finally
      svc.Free;
    end;
  except
    on e : Exception do
    begin
      if FindCmdLineSwitch('installer') then
        writeln('##> Exception '+E.Message)
      else
        logt(E.ClassName+ ': '+E.Message+#13#10#13#10+ExceptionStack(e));
      raise;
    end;
  end;
end;

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


{ TFHIRService }

constructor TFHIRService.Create(const ASystemName, ADisplayName, AIniName: String);
begin
  FStartTime := GetTickCount;
  inherited create(ASystemName, ADisplayName);
  FIni := TFHIRServerIniFile.Create(AIniName);
  FSettings := TFHIRServerSettings.Create;
  FSettings.ForLoad := not FindCmdLineSwitch('noload');
  FSettings.load(FIni);
  FDatabases := TFslMap<TKDBManager>.create;
end;

destructor TFHIRService.Destroy;
begin
  CloseDatabase;
  FDatabases.Free;
  FIni.Free;
  FSettings.Free;
  inherited;
end;

procedure TFHIRService.registerJs(sender : TObject; js : TJsHost);
begin
  js.engine.registerFactory(FHIR.R2.Javascript.registerFHIRTypes, fhirVersionRelease2, TFHIRFactoryR2.create);
  js.engine.registerFactory(FHIR.R3.Javascript.registerFHIRTypes, fhirVersionRelease3, TFHIRFactoryR3.create);
  js.engine.registerFactory(FHIR.R4.Javascript.registerFHIRTypes, fhirVersionRelease4, TFHIRFactoryR4.create);
  js.engine.registerFactory(FHIR.R4.Javascript.registerFHIRTypesDef, fhirVersionUnknown, TFHIRFactoryR4.create);
  js.engine.registerFactory(FHIR.R5.Javascript.registerFHIRTypes, fhirVersionRelease5, TFHIRFactoryR5.create);
end;

function TFHIRService.CanStart: boolean;
begin
  if not DebugMode then
  begin
    filelog := true;
    Consolelog := true;
  end;
  logt('Run Number '+inttostr(FSettings.RunNumber));

  try
    if FDatabases.IsEmpty then
      ConnectToDatabases;
    if FTerminologies = nil then
      LoadTerminologies;
    InitialiseRestServer(FknownVersion);
    result := true;
  except
    on e : Exception do
    begin
      logt(E.ClassName+ ': ' + E.Message+#13#10#13#10+ExceptionStack(e));
      raise;
    end;
  end;
  logt('started ('+inttostr((GetTickCount - FStartTime) div 1000)+'secs)');
  log_as_starting := false;
end;

procedure TFHIRService.DoStop;
begin
  try
    logt('stopping: '+StopReason);
    logt('stop rest server');
    StopRestServer;
    logt('unload terminologies');
    UnloadTerminologies;
    logt('stopped');
  except
    on e : Exception do
      logt('Exception stopping ('+E.ClassName + '): ' + E.Message+#13#10#13#10+ExceptionStack(e));
  end;
end;

procedure TFHIRService.dump;
begin
  inherited;
  logt(KDBManagers.Dump);
end;

function TFHIRService.connectToDatabase(s : String; details : TFHIRServerIniComplex) : TKDBManager;
var
  dbn, ddr : String;
begin
  dbn := details['database'];
  ddr := details['driver'];
  if details['type'] = 'mssql' then
  begin
    logt('Connect to '+s+' ('+details['type']+'://'+details['server']+'/'+dbn+')');
    if ddr = '' then
      ddr := 'SQL Server Native Client 11.0';
    result := TKDBOdbcManager.create(s, 100, 0, ddr, details['server'], dbn, details['username'], details['password']);
  end
  else if details['type'] = 'mysql' then
  begin
    logt('Connect to '+s+' ('+details['type']+'://'+details['server']+'/'+dbn+')');
    result := TKDBOdbcManager.create(s, 100, 0, ddr, details['server'], dbn, details['username'], details['password']);
  end
  else if details['type'] = 'SQLite' then
  begin
    logt('Connect to '+s+' ('+details['type']+':'+dbn+')');
    result := TKDBSQLiteManager.create(s, dbn, false);
  end
  else
    raise ELibraryException.Create('Unknown database type '+s);
end;

Procedure TFHIRService.ConnectToDatabases();
var
  s : String;
  details : TFHIRServerIniComplex;
begin
  logt('Load Databases. Config file = '+FIni.FileName);
  for s in FIni.databases.keys do
  begin
    details := FIni.databases[s];
    FDatabases.Add(s, connectToDatabase(s, details));
  end;
  logt('Databases Loaded');
end;

Procedure TFHIRService.checkDatabase(db : TKDBManager; factory : TFHIRFactory; serverFactory : TFHIRServerFactory);
var
  ver : integer;
  conn : TKDBConnection;
  dbi : TFHIRDatabaseInstaller;
  meta : TKDBMetaData;
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
          logt('Upgrade Database from version '+inttostr(ver)+' to '+inttostr(ServerDBVersion));
          dbi := TFHIRDatabaseInstaller.create(conn, '', factory.link, serverfactory.link);
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

procedure TFHIRService.cb(i: integer; s: WideString);
begin
  if FInstaller then
    InstallerCallBack(i, s);
end;

procedure TFHIRService.CloseDatabase;
begin
  if FDatabases <> nil then
    FDatabases.Clear;
end;

function Locate(s, fn : String; first : boolean) : String;
begin
  result := s;
  if FolderExists(result) then
    if first then
      result := IncludeTrailingPathDelimiter(s)+'examples-json.zip'
    else
      result := IncludeTrailingPathDelimiter(s)+'examples.json.zip';
  if not FileExists(result) then
    result := IncludeTrailingPathDelimiter(ExtractFilePath(fn))+s;
  if not FileExists(result) then
    raise EIOException.create('Unable to find file '+s);
end;

function TFHIRService.getLoadResourceList(factory : TFHIRFactory; mode : TFHIRInstallerSecurityMode) : TArray<String>;
begin
  case mode of
    ismUnstated, ismOpenAccess: result := factory.resourceNames;
    ismClosedAccess: result := factory.resourceNames;
    ismReadOnly: result := factory.resourceNames;
    ismTerminologyServer: result := factory.canonicalResources;
  end;
end;

function asString(list : TArray<String>) : String;
var
  s : String;
begin
  result := '';
  for s in list do
    result := result+','+s;
  result := result.Substring(1);
end;

procedure TFHIRService.Load(name, plist: String; mode : TFHIRInstallerSecurityMode);
var
  p, pi, pv : String;
  pl : TArray<String>;
  ploader : TPackageLoader;
  pcm : TFHIRPackageManager;
  li : TPackageLoadingInformation;
  details : TFHIRServerIniComplex;
  v : TFHIRVersion;
  dbn : String;
  db : TKDBManager;
  loadList : TArray<String>;
begin
  FNotServing := true;
  cb(1, 'Load: Connect to databases');
  if FDatabases.IsEmpty then
    ConnectToDatabases;
  details := FIni.endpoints[name];
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

    cb(1, 'Load: start kernel');
    CanStart;

    db := FDatabases[dbn].Link;
    try
      logt('Load: register value sets');
      identifyValueSets(db);
      li := TPackageLoadingInformation.Create('4.0.1');
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
              loadList := getLoadResourceList(ploader.FFactory, mode);
              logt('Load Package '+pi+'#'+pv+' - resources of type '+asString(loadList));
              pcm.loadPackage(pi, pv, loadList, li);
              FWebServer.EndPoint(name).Transaction(ploader.bundle, true, p, '', callback);
            finally
              ploader.Free;
            end;
          end;
        end;
      finally
        li.Free;
      end;

      logt('loaded');
      cb(95, 'Building Terminology Closure Tables');
      FWebServer.EndPoint(name).Context.TerminologyServer.BuildIndexes(not assigned(callback));
      cb(100, 'Cleaning up');

      DoStop;
    finally
      db.free;
    end;
  finally
    pcm.Free;
  end;
end;
(*
procedure TFHIRService.LoadbyProfile(fn: String; init : boolean);
var
  ini : TFHIRServerIniFile;
  f : TFileStream;
  i : integer;
begin
  FNotServing := true;
  ini := TFHIRServerIniFile.Create(fn);
  try
    fn := fn.Replace('.dstu', '');
    if FDb = nil then
      ConnectToDatabase;
    CanStart;
    if init then
    begin
      fn := ini.ReadString(voVersioningNotApplicable, 'control', 'load', '');
      logt('Load database from '+fn);
      f := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
      try
        FWebServer.Transaction(f, true, fn, 'http://hl7.org/fhir', ini, callback);
      finally
        f.Free;
      end;
    end;
    for i := 1 to ini.ReadInteger(voVersioningNotApplicable, 'control', 'files', 0) do
    begin
      fn := ini.ReadString(voVersioningNotApplicable, 'control', 'file'+inttostr(i), '');
      if (fn <> '') then
      begin
        repeat
          logt('Load '+fn);
          f := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
          try
            FWebServer.Transaction(f, false, fn, ini.ReadString(voVersioningNotApplicable, 'control', 'base'+inttostr(i), ''), ini, callback);
          finally
            f.Free;
          end;
        until ini.ReadInteger(voVersioningNotApplicable, 'process', 'start', -1) = -1;
      end;
    end;
    logt('done');
    FTerminologyServer.BuildIndexes(true);
    DoStop;
  finally
    ini.free;
  end;
end;
*)

procedure TFHIRService.LoadTerminologies;
begin
  FTerminologies := TCommonTerminologies.Create(FSettings.link);
  FTerminologies.load(FIni, FDatabases, false);
end;

procedure TFHIRService.postStart;
begin
end;

procedure TFHIRService.UnloadTerminologies;
begin
  FTerminologies.Free;
  FTerminologies := nil;
end;

procedure TFHIRService.updateAdminPassword(name: String);
var
  db : TKDBManager;
  scim : TSCIMServer;
  salt, un, pw, em, result : String;
  conn : TKDBConnection;
  details : TFHIRServerIniComplex;
begin
  // check that user account details are provided
  salt := FIni.admin['scim-salt'];
  if (salt = '') then
    raise EFHIRException.create('You must define a scim salt in the ini file');
  un := FIni.admin['username'];
  if (un = '') then
    raise EFHIRException.create('You must define an admin username in the ini file');
  FindCmdLineSwitch('password', pw, true, [clstValueNextParam]);
  if (pw = '') then
    raise EFHIRException.create('You must provide a admin password as a parameter to the command');
  em := FIni.admin['email'];
  if (em = '') then
    raise EFHIRException.create('You must define an admin email in the ini file');

  details := FIni.endpoints[name];
  if details = nil then
    raise EFslException.Create('Undefined endpoint '+name);

  result := details['database'];
  if result = '' then
    raise EFslException.Create('No defined database '+name);
  if FDatabases.ContainsKey(result) then
    db := FDatabases[result].Link
  else
    db := connectToDatabase(result, FIni.databases[result]);
  try
    logt('fix admin password for '+result);
    scim := TSCIMServer.Create(db.Link, salt, FIni.web['host'], FIni.admin['default-rights'], true);
    try
      conn := db.GetConnection('setup');
      try
        scim.UpdateAdminUser(conn, un, pw, em);
        conn.Release;
        logt('done');
      except
         on e:exception do
         begin
           logt('Error: '+e.Message);
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

procedure TFHIRService.InitialiseRestServer(version : TFHIRVersion);
var
  ctxt : TFHIRServerContext;
  store : TFHIRNativeStorageService;
  s : String;
  details : TFHIRServerIniComplex;
begin
  FWebServer := TFhirWebServer.create(FSettings.Link, DisplayName);
  FWebServer.OnRegisterJs := registerJs;
  FWebServer.loadConfiguration(FIni);
  logt('Web source from '+FIni.web['folder']);
  FWebServer.SourceProvider := TFHIRWebServerSourceFolderProvider.Create(ProcessPath(ExtractFilePath(FIni.FileName), FIni.web['folder']));

  for s in FIni.endpoints.sortedKeys do
  begin
    details := FIni.endpoints[s];
    logt('Initialise endpoint '+s+' at '+details['path']+' for '+details['version']);

    case version of
      fhirVersionRelease2: if details['version'] <> 'r2' then continue;
      fhirVersionRelease3: if details['version'] <> 'r3' then continue;
      fhirVersionRelease4: if details['version'] <> 'r4' then continue;
      fhirVersionRelease5: if details['version'] <> 'r5' then continue;
    end;

    if FindCmdLineSwitch('r5') and (details['version'] <> 'r5') then
      continue;
    if FindCmdLineSwitch('r4') and (details['version'] <> 'r4') then
      continue;
    if FindCmdLineSwitch('r3') and (details['version'] <> 'r3') then
      continue;
    if FindCmdLineSwitch('r2') and (details['version'] <> 'r2') then
      continue;

    if details['version'] = 'r2' then
    begin
      store := TFHIRNativeStorageServiceR2.create(FDatabases[details['database']].Link, TFHIRFactoryR2.Create);
    end
    else if details['version'] = 'r3' then
    begin
      store := TFHIRNativeStorageServiceR3.create(FDatabases[details['database']].Link, TFHIRFactoryR3.Create);
    end
    else if details['version'] = 'r4' then
    begin
      store := TFHIRNativeStorageServiceR4.create(FDatabases[details['database']].Link, TFHIRFactoryR4.Create);
    end
    else if details['version'] = 'r5' then
    begin
      store := TFHIRNativeStorageServiceR5.create(FDatabases[details['database']].Link, TFHIRFactoryR5.Create);
    end
    else
      raise EFslException.Create('Cannot load end-point '+s+' version '+details['version']);


    try
      ctxt := TFHIRServerContext.Create(store.Link, TKernelServerFactory.create(store.Factory.version));
      try
        logt('  .. check DB '+details['database']);
        checkDatabase(store.DB, store.Factory, ctxt.ServerFactory);
        ctxt.Globals := FSettings.Link;
        store.ServerContext := ctxt;
        ctxt.TerminologyServer := TTerminologyServer.Create(store.DB.link, ctxt.factory.Link, FTerminologies.link);
        logt('  .. load DB '+details['database']);
        store.Initialise();
        ctxt.userProvider := TSCIMServer.Create(store.db.link, FIni.admin['scim-salt'], FWebServer.host, FIni.admin['default-rights'], false);
        FWebServer.registerEndPoint(s, details['path'], ctxt.Link, FIni);
      finally
        ctxt.Free;
      end;
    finally
      store.Free;
    end;
  end;
//      FWebServer.CDSHooksServer.registerService(TCDAHooksCodeViewService.create);
//      FWebServer.CDSHooksServer.registerService(TCDAHooksIdentifierViewService.create);
//      FWebServer.CDSHooksServer.registerService(TCDAHooksPatientViewService.create);
//      FWebServer.CDSHooksServer.registerService(TCDAHackingHealthOrderService.create);

  FWebServer.Start(not FNotServing);
end;

function TFHIRService.InstallDatabase(name : String; securityMode : TFHIRInstallerSecurityMode) : String;
var
  db : TKDBManager;
  dbi : TFHIRDatabaseInstaller;
  scim : TSCIMServer;
  salt, un, pw, em, sql, dr : String;
  conn : TKDBConnection;
  details : TFHIRServerIniComplex;
  v : TFHIRVersion;
begin
  // check that user account details are provided
  salt := FIni.admin['scim-salt'];
  if (salt = '') then
    raise EFHIRException.create('You must define a scim salt in the ini file');
  dr := FIni.admin['default-rights'];
  if (dr = '') then
  begin
    case securityMode of
      ismOpenAccess : FIni.admin['default-rights'] := 'openid,fhirUser,profile,user/*.*';
      ismClosedAccess : FIni.admin['default-rights'] := 'openid,fhirUser,profile';
      ismReadOnly : FIni.admin['default-rights'] := 'openid,fhirUser,profile,user/*.read';
      ismTerminologyServer : FIni.admin['default-rights'] := 'openid,fhirUser,profile,user/CodeSystem.read,user/ConceptMap.read,user/ValueSet.read'
    end;
    dr := FIni.admin['default-rights'];
  end;
  if (dr = '') then
    raise EFHIRException.create('You must define some default rights for SCIM users in the ini file');
  un := FIni.admin['username'];
  if (un = '') then
    raise EFHIRException.create('You must define an admin username in the ini file');
  FindCmdLineSwitch('password', pw, true, [clstValueNextParam]);
  if (pw = '') then
    raise EFHIRException.create('You must provide a admin password as a parameter to the command');
  em := FIni.admin['email'];
  if (em = '') then
    raise EFHIRException.create('You must define an admin email in the ini file');

  sql := 'C:\work\fhirserver\sql';
  if not DirectoryExists(sql) then
    sql := IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0)))+'sql';

  details := FIni.endpoints[name];
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
  if FDatabases.ContainsKey(result) then
    db := FDatabases[result].Link
  else
    db := connectToDatabase(result, FIni.databases[result]);
  try
    logt('mount endpoint '+result);
    scim := TSCIMServer.Create(db.Link, salt, FIni.web['host'], FIni.admin['default-rights'], true);
    try
      conn := db.GetConnection('setup');
      try
        dbi := TFHIRDatabaseInstaller.create(conn, sql, factoryFactory(v), TKernelServerFactory.create(v));
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
        logt('done');
      except
         on e:exception do
         begin
           logt('Error: '+e.Message);
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

procedure TFHIRService.InstallerCallBack(i: integer; s: String);
begin
  writeln('##> '+inttostr(i)+' '+s);
end;

procedure TFHIRService.UnInstallDatabase;
var
  db : TKDBManager;
  dbi : TFHIRDatabaseInstaller;
  conn : TKDBConnection;
  n : String;
  details : TFHIRServerIniComplex;
  v : TFHIRVersion;
begin
  details := FIni.endpoints[name];
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
  if FDatabases.ContainsKey(n) then
    db := FDatabases[n].Link
  else
    db := connectToDatabase(n, FIni.databases[n]);
  try
    logt('unmount database '+n);
    conn := db.GetConnection('setup');
    try
      dbi := TFHIRDatabaseInstaller.create(conn, '', factoryFactory(v), TKernelServerFactory.create(v));
      try
        dbi.callback := callback;
        dbi.UnInstall;
      finally
        dbi.free;
      end;
      conn.Release;
      logt('done');
    except
       on e:exception do
       begin
         logt('Error: '+e.Message);
         conn.Error(e);
         recordStack(e);
         raise;
       end;
    end;
  finally
    db.Free;
  end;
end;

procedure TFHIRService.StopRestServer;
begin
  FWebServer.Stop;
  FWebServer.free;
end;

procedure TFHIRService.identifyValueSets(db : TKDBManager);
begin
(*
function TFHIRService.RegisterValueSet(id: String; conn : TKDBConnection): integer;
begin
  result := Conn.CountSQL('Select ValueSetKey from ValueSets where URL = '''+SQLWrapString(id)+'''');
  if result = 0 then
  begin
    result := FTerminologyServer.NextValueSetKey;
    Conn.ExecSQL('Insert into ValueSets (ValueSetKey, URL, NeedsIndexing) values ('+inttostr(result)+', '''+SQLWrapString(id)+''', 1)');
  end;
end;



 logt('Register ValueSets');
  Db.Connection('register value sets',
    procedure (conn : TKDBConnection)
    var
      vs : TFhirValueSetW;
      vsl : TFslList TFHIRValueSetList;
    begin
      vsl := FTerminologyServer.GetValueSetList;
      try
        for vs in vsl do
          vs.Tags['tracker'] := inttostr(RegisterValueSet(vs.url, conn));
      finally
        vsl.Free;
      end;
    end);*)
end;

procedure TFHIRService.Index;
begin
  raise EFslException.Create('not supported yet');
(*
  FNotServing := true;
  if FDb = nil then
    ConnectToDatabase;
  CanStart;
  logt('index database');
  FTerminologyServer.BuildIndexes(true);
  DoStop;
*)
end;

procedure TFHIRService.validate;
begin
  raise EFslException.Create('not supported yet');
(*
  FNotServing := true;
  if FDb = nil then
    ConnectToDatabase;
  CanStart;
  logt('validate resources');
  FWebServer.ServerContext.Storage.RunValidation;
  DoStop;
*)
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
  p := FFactory.makeParser(nil, ffJson, 'en');
  try
    FBundle.addEntry.resource := p.parseResource(stream);
  finally
    p.Free;
  end;

end;

procedure TFHIRService.resetProgress(name: String);
begin
  if not FInstaller then
  begin
    FProgress := 0;
    FProgressName := name;
    logtn(name+' ');
  end;
end;

procedure TFHIRService.fetchProgress(sender: TObject; progess: integer);
begin
  if FInstaller then
    Fcallback(progess, FProgressName)
  else
  begin
    if progess > FProgress then
    begin
      write('.');
      FProgress := FProgress + 5;
    end;
  end;
end;

procedure TFHIRService.fetchProgress2(sender: TObject; pct: integer; done: boolean; desc: String);
begin
  if FInstaller then
    Fcallback(pct, FProgressName)
  else
  begin
    if pct > FProgress then
    begin
      write('.');
      FProgress := FProgress + 5;
    end;
  end;
end;

procedure TFHIRService.finishProgress(status: String);
begin
  if not FInstaller then
    writeln(' '+status);
end;

end.


