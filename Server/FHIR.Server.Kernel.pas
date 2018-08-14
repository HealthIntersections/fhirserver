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
  FHIR.Loinc.Importer, FHIR.Loinc.Services,
  FHIR.Database.Manager, FHIR.Database.ODBC, FHIR.Database.Dialects, FHIR.Database.SQLite,
  FHIR.Base.Factory, FHIR.Cache.PackageManager, FHIR.Base.Parser, FHIR.Base.Lang, FHIR.Javascript.Base, FHIR.Base.Common,

  FHIR.R2.Factory, FHIR.R3.Factory, FHIR.R4.Factory,
  FHIR.R2.IndexInfo, FHIR.R3.IndexInfo, FHIR.R4.IndexInfo,
  FHIR.Server.IndexingR2, FHIR.Server.IndexingR3, FHIR.Server.IndexingR4,
  FHIR.Server.SubscriptionsR2, FHIR.Server.SubscriptionsR3, FHIR.Server.SubscriptionsR4,
  FHIR.Server.OperationsR2, FHIR.Server.OperationsR3, FHIR.Server.OperationsR4,
  FHIR.R2.Validator, FHIR.R3.Validator, FHIR.R4.Validator,
  FHIR.Server.ValidatorR2, FHIR.Server.ValidatorR3, FHIR.Server.ValidatorR4,
  FHIR.R2.Javascript, FHIR.R3.Javascript, FHIR.R4.Javascript,

  FHIR.Tools.Indexing,
  FHIR.Tx.Manager, FHIR.Tx.Server,
  FHIR.Server.Storage,
  FHIR.Server.Web, FHIR.Server.DBInstaller, FHIR.Server.Database, FHIR.Base.Objects,
  FHIR.Server.Constants, FHIR.Server.Context, FHIR.Server.Utilities, FHIR.Server.WebSource,
  FHIR.Scim.Server, FHIR.CdsHooks.Service, FHIR.Server.Javascript, FHIR.Server.Factory,
  FHIR.Server.Indexing, FHIR.Server.Subscriptions, FHIR.Server.Manager;

Type
  TKernelServerFactory = class (TFHIRServerFactory)
  private
    FVersion : TFHIRVersion;
  public
    constructor create(version : TFHIRVersion);

    function makeIndexes : TFHIRIndexBuilder; override;
    function makeValidator: TFHIRValidatorV; override;
    function makeIndexer : TFHIRIndexManager; override;
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
    TestMode : Boolean;
    FIni : TFHIRServerIniFile;
    FSettings : TFHIRServerSettings;
    FDatabases : TFslMap<TKDBManager>;
    FTerminologies : TCommonTerminologies;
    FWebServer : TFhirWebServer;

    FNotServing : boolean;
    FLoadStore : boolean;
    FInstaller : boolean;
    Fcallback: TInstallerCallback;

    function connectToDatabase(s : String; details : TFHIRServerIniComplex) : TKDBManager;
    Procedure checkDatabase(db : TKDBManager; factory : TFHIRFactory; serverFactory : TFHIRServerFactory);
    procedure ConnectToDatabases();
    procedure LoadTerminologies;
    procedure InitialiseRestServer;
    procedure StopRestServer;
    procedure UnloadTerminologies;
    procedure CloseDatabase;
    procedure validate;
    procedure InstallerCallBack(i : integer; s : String);
    procedure cb(i : integer; s : WideString);
    procedure identifyValueSets(db : TKDBManager);
//    function RegisterValueSet(id: String; conn: TKDBConnection): integer;
    function fetchFromUrl(fn : String; var bytes : TBytes) : boolean;
    procedure registerJs(sender: TObject; js: TJsHost);
  protected
    function CanStart : boolean; Override;
    procedure postStart; override;
    procedure DoStop; Override;
    procedure dump; override;
  public
    Constructor Create(const ASystemName, ADisplayName, AIniName: String);
    Destructor Destroy; override;

    procedure Load(name, fn : String);
//    procedure LoadbyProfile(fn : String; init : boolean);
    procedure Index;
    function InstallDatabase(name : String) : String;
    procedure UnInstallDatabase(name : String);

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
    raise Exception.Create('No endpoint parameter supplied');
end;

procedure ExecuteFhirServer;
var
  iniName : String;
  svcName : String;
  dispName : String;
  cmd : String;
  dir, fn, fn2, ver, ldate, lver, dest, name : String;
  svc : TFHIRService;
  ini : TIniFile;
  factory : TFHIRFactory;
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
        GJsHost := TJsHost.Create('');
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
              name:= svc.InstallDatabase(endpoint);
              if FindCmdLineSwitch('unii', fn, true, [clstValueNextParam]) then
                ImportUnii(fn,  svc.FDatabases[name]);
            end
            else if cmd = 'unmount' then
              svc.UninstallDatabase(endpoint)
            else if cmd = 'remount' then
            begin
              svc.DebugMode := true;
              svc.FNotServing := true;
              svc.UninstallDatabase(endpoint);
              name := svc.InstallDatabase(endpoint);
              if FindCmdLineSwitch('unii', fn2, true, [clstValueNextParam]) then
                ImportUnii(fn2, svc.FDatabases[name]);
  //            if FindCmdLineSwitch('profile', fn, true, [clstValueNextParam]) then
  //              svc.LoadByProfile(fn, true)
  //            else
              if FindCmdLineSwitch('load', fn2, true, [clstValueNextParam]) then
                svc.Load(endpoint, fn2);
            end
            else if cmd = 'load' then
            begin
              svc.DebugMode := true;
              svc.FNotServing := true;
  //            svc.Load(fn);
            end
            else if cmd = 'txdirect' then
            begin
              svc.UninstallDatabase(fn);
              svc.InstallDatabase(fn);
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
              raise Exception.Create('Unknown command '+cmd);
          end
          else
          begin
            writeln('No -cmd parameter - exiting now'); // won't see this if an actual service
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
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating Validator');
  end;
end;

function TKernelServerFactory.makeIndexer : TFHIRIndexManager;
begin
  case FVersion of
    fhirVersionRelease2 : result := TFhirIndexManager2.Create;
    fhirVersionRelease3 : result := TFhirIndexManager3.Create;
    fhirVersionRelease4 : result := TFhirIndexManager4.Create;
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
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating subcription manager');
  end;
end;

procedure TKernelServerFactory.setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer});
begin
  case FVersion of
    fhirVersionRelease2 : TFHIRServerWorkerContextR2(ValidatorContext).TerminologyServer := (server as TTerminologyServer).Link;
    fhirVersionRelease3 : TFHIRServerWorkerContextR3(ValidatorContext).TerminologyServer := (server as TTerminologyServer).Link;
    fhirVersionRelease4 : TFHIRServerWorkerContextR4(ValidatorContext).TerminologyServer := (server as TTerminologyServer).Link;
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
  inherited;
end;

procedure TFHIRService.registerJs(sender : TObject; js : TJsHost);
begin
  js.registerVersion(TFHIRServerWorkerContextR2.Create(TFHIRFactoryR2.create), FHIR.R2.Javascript.registerFHIRTypes);
  js.registerVersion(TFHIRServerWorkerContextR3.Create(TFHIRFactoryR3.create), FHIR.R3.Javascript.registerFHIRTypes);
  js.registerVersion(TFHIRServerWorkerContextR4.Create(TFHIRFactoryR4.create), FHIR.R4.Javascript.registerFHIRTypes);
end;

function TFHIRService.CanStart: boolean;
begin
  if not DebugMode then
  begin
    filelog := true;
    Consolelog := true;
  end;
  logt('Run Number '+inttostr(FSettings.RunNumber));

  result := false;
  try
    if FDatabases.IsEmpty then
      ConnectToDatabases;
    if FTerminologies = nil then
      LoadTerminologies;
    InitialiseRestServer;
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
    StopRestServer;
    logt('stopping.b: '+StopReason);
    UnloadTerminologies;
    logt('stopping.c: '+StopReason);
  except
    on e : Exception do
      logt(E.ClassName + ': ' + E.Message+#13#10#13#10+ExceptionStack(e));
  end;
    logt('stopping.d: '+StopReason);
end;

procedure TFHIRService.dump;
begin
  inherited;
  logt(KDBManagers.Dump);
end;

function TFHIRService.fetchFromUrl(fn: String; var bytes: TBytes): boolean;
var
  fetch : TInternetFetcher;
begin
  fetch := TInternetFetcher.create;
  try
    fetch.URL := AppendForwardSlash(fn)+'validator.pack';
    try
      fetch.Fetch;
      bytes := fetch.Buffer.AsBytes;
      exit(true);
    except
    end;
    fetch.URL := fn;
    try
      fetch.Fetch;
      bytes := fetch.Buffer.AsBytes;
      exit(true);
    except
      exit(false);
    end;
  finally
    fetch.Free;
  end;
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
var
  db : TKDBManager;
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

procedure TFHIRService.Load(name, fn: String);
var
  f : TFileStream;
  st : TStringList;
  s, src, plist, p, pi, pv : String;
  first : boolean;
  ini : TIniFile;
  i : integer;
  bytes : TBytes;
  bs : TBytesStream;
  pl : TArray<String>;
  ploader : TPackageLoader;
  pcm : TFHIRPackageManager;
  details : TFHIRServerIniComplex;
  v : TFHIRVersion;
  dbn : String;
  db : TKDBManager;
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
  else
    raise EFslException.Create('unknown version '+details['version']);

  dbn := details['database'];
  if dbn = '' then
    raise EFslException.Create('No defined database '+name);
  cb(1, 'Load: start kernel');
  CanStart;

  db := FDatabases[dbn].Link;
  try

    logt('Load: register value sets');
    identifyValueSets(db);

    // first we load any packages in the -packages parameter
    if FindCmdLineSwitch('packages', plist, true, [clstValueNextParam]) then
    begin
      pcm := TFHIRPackageManager.Create(false);
      try
        pl := plist.substring(1).Split([',']);
        for p in pl do
        begin
          StringSplit(p, '-', pi, pv);
          if not pcm.packageExists(pi, pv) then
            raise EFHIRException.create('Package '+p+' not found');
          logt('Load Package '+pi+'-'+pv);
          ploader := TPackageLoader.create(factoryFactory(v));
          try
            pcm.loadPackage(pi, pv, ploader.FFactory.canonicalResources, ploader.load);
            FWebServer.EndPoint(name).Transaction(ploader.bundle, true, p, '', callback);
          finally
            ploader.Free;
          end;
        end;
      finally
        pcm.Free;
      end;
    end;

    cb(2, 'Load from '+fn);
    if fn.StartsWith('http://') or fn.StartsWith('https://') then
    begin
      if fetchFromUrl(fn, bytes) then
      begin
        bs := TBytesStream.Create(bytes);
        try
          FWebServer.EndPoint(name).Transaction(bs, true, fn, '', nil, callback);
        finally
          bs.Free;
        end;
      end
      else
        raise EIOException.create('Unable to fetch from "'+fn+'"');
    end
    else if fn.EndsWith('.json') or fn.EndsWith('.xml') then
    begin
      logt('Load database from '+fn);
      f := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
      try
        FWebServer.EndPoint(name).Transaction(f, true, fn, 'http://hl7.org/fhir', nil, callback);
      finally
        f.Free;
      end
    end
    else
    begin
      if FolderExists(fn) then
        fn := IncludeTrailingPathDelimiter(fn)+'load.ini';
      logt('Load database from sources listed in '+fn);
      if not FileExists(fn) then
        raise EIOException.create('Load Ini file '+fn+' not found');
      ini := TIniFile.Create(fn);
      st := TStringList.Create;
      try
        ini.ReadSection('files', st);
        first := true;
        for s in st do
        begin
          logt('Check file '+s);
          src := locate(s, fn, first);
          first := false;
        end;

        i := 0;
        first := true;
        for s in st do
        begin
          inc(i);
          logt('Load file '+inttostr(i)+' of '+inttostr(st.count)+':'+s);
          cb(0, 'Load from '+s);
          src := locate(s, fn, first);
          f := TFileStream.Create(src, fmOpenRead + fmShareDenyWrite);
          try
            FWebServer.EndPoint(name).Transaction(f, first, src, ini.ReadString('files', s, ''), nil, callback);
          finally
            f.Free;
          end;
          first := false;
        end;
      finally
        st.Free;
        ini.Free;
      end;
    end;

    logt('done');
    cb(95, 'Building Terminology Closure Tables');
    FWebServer.EndPoint(name).Context.TerminologyServer.BuildIndexes(not assigned(callback));
    cb(100, 'Cleaning up');

    DoStop;
  finally
    db.free;
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

procedure TFHIRService.InitialiseRestServer;
var
  ctxt : TFHIRServerContext;
  store : TFHIRNativeStorageService;
  s : String;
  details : TFHIRServerIniComplex;
  ep : TFhirWebServerEndpoint;
begin
  FWebServer := TFhirWebServer.create(FSettings.Link, DisplayName);
  FWebServer.OnRegisterJs := registerJs;
  FWebServer.loadConfiguration(FIni);
  logt('Web source from '+FIni.web['folder']);
  FWebServer.SourceProvider := TFHIRWebServerSourceFolderProvider.Create(ProcessPath(ExtractFilePath(FIni.FileName), FIni.web['folder']));

  for s in FIni.endpoints.Keys do
  begin
    details := FIni.endpoints[s];
    logt('Initialise endpoint '+s+' at '+details['path']+' for '+details['version']);

    if FindCmdLineSwitch('r4') and (details['version'] <> 'r4') then
      break;

    if details['version'] = 'r2' then
    begin
      store := TFHIRNativeStorageServiceR2.create(FDatabases[details['database']], TFHIRFactoryR2.Create);
    end
    else if details['version'] = 'r3' then
    begin
      store := TFHIRNativeStorageServiceR3.create(FDatabases[details['database']], TFHIRFactoryR3.Create);
    end
    else if details['version'] = 'r4' then
    begin
      store := TFHIRNativeStorageServiceR4.create(FDatabases[details['database']], TFHIRFactoryR4.Create);
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
        ctxt.Validate := true; // move to database config FIni.ReadBool(voVersioningNotApplicable, 'fhir', 'validate', true);
        store.Initialise();
        ctxt.userProvider := TSCIMServer.Create(store.db.link, FIni.admin['scim-salt'], FWebServer.host, FIni.admin['default-rights'], false);
        ep := FWebServer.registerEndPoint(s, details['path'], ctxt.Link, FIni);
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

function TFHIRService.InstallDatabase(name : String) : String;
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

end.


