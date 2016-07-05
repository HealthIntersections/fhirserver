unit FHIRServerApplicationCore;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

interface

Uses
  Windows, SysUtils, Classes, IniFiles, ActiveX, ComObj,
  AdvExceptions,
  SystemService, SystemSupport, FileSupport,
  SnomedImporter, SnomedServices, SnomedExpressions, RxNormServices, UniiServices,
  LoincImporter, LoincServices,
  KDBManager, KDBOdbcExpress, KDBDialects,
  TerminologyServer,
  FHIRRestServer, DBInstaller, FHIRConstants, FhirServerTests, FHIROperation, FHIRDataStore, FHIRBase, FHIRPath,
  FHIRServerConstants,
  SCIMServer;

Type
  TFHIRService = class (TSystemService)
  private
    FStartTime : cardinal;
    TestMode : Boolean;
    FIni : TIniFile;
    FDb : TKDBManager;
    FTerminologyServer : TTerminologyServer;
    FWebServer : TFhirWebServer;
    FWebSource : String;
    FNotServing : boolean;

    procedure ConnectToDatabase(noCheck : boolean = false);
    procedure LoadTerminologies;
    procedure InitialiseRestServer;
    procedure StopRestServer;
    procedure UnloadTerminologies;
    procedure CloseDatabase;
    procedure CheckWebSource;
    function dbExists : Boolean;
    procedure validate;
  protected
    function CanStart : boolean; Override;
    procedure DoStop; Override;
    procedure dump; override;
  public
    Constructor Create(const ASystemName, ADisplayName, AIniName: String);
    Destructor Destroy; override;

    procedure ExecuteTests(all : boolean);
    procedure Load(fn : String);
    procedure LoadbyProfile(fn : String; init : boolean);
    procedure Index;
    procedure InstallDatabase;
    procedure UnInstallDatabase;
  end;

procedure ExecuteFhirServer;

implementation

uses
  FHIRLog, JclDebug;

procedure CauseException;
begin
  raise Exception.Create('Test');
end;

procedure ExecuteFhirServer;
var
  iniName : String;
  svcName : String;
  dispName : String;
  dir, fn, ver, lver : String;
  svc : TFHIRService;
begin
  try
    CoInitialize(nil);
    if not FindCmdLineSwitch('ini', iniName, true, [clstValueNextParam]) then
      iniName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'fhirserver.ini';

    if not FindCmdLineSwitch('name', svcName, true, [clstValueNextParam]) then
      svcName := 'fhirserver';
    if not FindCmdLineSwitch('title', dispName, true, [clstValueNextParam]) then
      dispName := 'FHIR Server';
    iniName := iniName.replace('.dstu', '.dev');

    if JclExceptionTrackingActive then
      logt('FHIR Service '+SERVER_VERSION+' ('+FHIR_GENERATED_VERSION+'). Using ini file '+iniName+' with stack dumps on')
    else
      logt('FHIR Service '+SERVER_VERSION+' ('+FHIR_GENERATED_VERSION+'). Using ini file '+iniName+' (no stack dumps)');
    dispName := dispName + ' '+SERVER_VERSION+' ('+FHIR_GENERATED_VERSION+')';


    svc := TFHIRService.Create(svcName, dispName, iniName);
    try
      if FindCmdLineSwitch('mount') then
      begin
        svc.InstallDatabase;
        if FindCmdLineSwitch('unii', fn, true, [clstValueNextParam]) then
          ImportUnii(fn, svc.Fdb);
      end
      else if FindCmdLineSwitch('unmount') then
        svc.UninstallDatabase
      else if FindCmdLineSwitch('remount') then
      begin
        svc.FNotServing := true;
        svc.UninstallDatabase;
        svc.InstallDatabase;
        if FindCmdLineSwitch('unii', fn, true, [clstValueNextParam]) then
          ImportUnii(fn, svc.Fdb);
        if FindCmdLineSwitch('profile', fn, true, [clstValueNextParam]) then
          svc.LoadByProfile(fn, true)
        else if FindCmdLineSwitch('load', fn, true, [clstValueNextParam]) then
          svc.Load(fn);
      end
      else if FindCmdLineSwitch('txdirect') then
      begin
        svc.UninstallDatabase;
        svc.InstallDatabase;
        svc.DebugExecute;
      end
      else if FindCmdLineSwitch('profile', fn, true, [clstValueNextParam]) then
        svc.LoadByProfile(fn, false)
      else if FindCmdLineSwitch('validate') then
      begin
        svc.FNotServing := true;
        svc.validate;
      end
      else if FindCmdLineSwitch('index') then
        svc.index
      else if FindCmdLineSwitch('tests') then
        svc.ExecuteTests(false)
      else if FindCmdLineSwitch('tests-all') then
        svc.ExecuteTests(true)
      else if FindCmdLineSwitch('snomed-rf1', dir, true, [clstValueNextParam]) then
      begin
        FindCmdLineSwitch('sver', ver, true, [clstValueNextParam]);
        svc.FIni.WriteString('snomed', 'cache', importSnomedRF1(dir, svc.FIni.ReadString('internal', 'store', IncludeTrailingPathDelimiter(ProgData)+'fhirserver'), ver));
      end
      else if FindCmdLineSwitch('snomed-rf2', dir, true, [clstValueNextParam]) then
      begin
        FindCmdLineSwitch('sver', ver, true, [clstValueNextParam]);
        svc.FIni.WriteString('snomed', 'cache', importSnomedRF2(dir, svc.FIni.ReadString('internal', 'store', IncludeTrailingPathDelimiter(ProgData)+'fhirserver'), ver));
      end
      else if FindCmdLineSwitch('loinc', dir, true, [clstValueNextParam]) and FindCmdLineSwitch('lver', lver, true, [clstValueNextParam]) then
        svc.FIni.WriteString('loinc', 'cache', importLoinc(dir, lver, svc.FIni.ReadString('internal', 'store', IncludeTrailingPathDelimiter(ProgData)+'fhirserver')))
      else if FindCmdLineSwitch('rxstems', dir, true, []) then
      begin
        generateRxStems(TKDBOdbcDirect.create('fhir', 100, 0, 'SQL Server Native Client 11.0',
          svc.FIni.ReadString('database', 'server', ''), svc.FIni.ReadString('RxNorm', 'database', ''),
          svc.FIni.ReadString('database', 'username', ''), svc.FIni.ReadString('database', 'password', '')))
      end
      else if FindCmdLineSwitch('ncistems', dir, true, []) then
      begin
        generateRxStems(TKDBOdbcDirect.create('fhir', 100, 0, 'SQL Server Native Client 11.0',
          svc.FIni.ReadString('database', 'server', ''), svc.FIni.ReadString('NciMeta', 'database', ''),
          svc.FIni.ReadString('database', 'username', ''), svc.FIni.ReadString('database', 'password', '')))
      end
  //    procedure ReIndex;
  //    procedure clear(types : String);
      else if FindCmdLineSwitch('unii', fn, true, [clstValueNextParam]) then
      begin
       svc.ConnectToDatabase;
       ImportUnii(fn, svc.Fdb)
      end
      else
        svc.Execute;
    finally
      svc.Free;
    end;
  except
    on e : Exception do
    begin
      logt(E.ClassName+ ': '+E.Message+#13#10#13#10+ExceptionStack(e));
      raise;
    end;
  end;
end;

{ TFHIRService }

constructor TFHIRService.Create(const ASystemName, ADisplayName, AIniName: String);
begin
  FStartTime := GetTickCount;
  inherited create(ASystemName, ADisplayName);
  FIni := TIniFile.Create(AIniName);
  CheckWebSource;
end;

function TFHIRService.dbExists: Boolean;
var
  conn : TKDBConnection;
  meta : TKDBMetaData;
begin
  conn := FDb.GetConnection('test');
  try
    meta := conn.FetchMetaData;
    try
      result := meta.HasTable('Config');
    finally
      meta.free;
    end;
    conn.Release;
  except
    on e:exception do
    begin
      conn.Error(e);
      result := false;
    end;
  end;
end;

destructor TFHIRService.Destroy;
begin
  CloseDatabase;
  FIni.Free;
  inherited;
end;

function TFHIRService.CanStart: boolean;
begin
  result := false;
  try
    if FDb = nil then
      ConnectToDatabase;
    if FTerminologyServer = nil then
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
end;

procedure TFHIRService.DoStop;
begin
  try
    logt('stop: '+StopReason);
    StopRestServer;
    UnloadTerminologies;
  except
    on e : Exception do
      logt(E.ClassName + ': ' + E.Message+#13#10#13#10+ExceptionStack(e));
  end;
end;

procedure TFHIRService.dump;
begin
  inherited;
  logt(KDBManagers.Dump);
end;

procedure TFHIRService.ExecuteTests(all : boolean);
begin
  ExecuteFhirServerTests(all);
end;

Procedure TFHIRService.ConnectToDatabase(noCheck : boolean = false);
var
  dbn : String;
  ver : integer;
  conn : TKDBConnection;
  dbi : TFHIRDatabaseInstaller;
  meta : TKDBMetaData;
begin
  dbn := FIni.ReadString('database', 'database', '');
  if FIni.ValueExists('database', 'database'+FHIR_GENERATED_VERSION) then
     dbn := FIni.ReadString('database', 'database'+FHIR_GENERATED_VERSION, '');
  if TestMode then
    FDb := TKDBOdbcDirect.create('fhir', 100, 0, 'SQL Server Native Client 11.0', '(local)', 'fhir-test', '', '')
  else if FIni.ReadString('database', 'type', '') = 'mssql' then
  begin
    logt('Database mssql://'+FIni.ReadString('database', 'server', '')+'/'+dbn);
    FDb := TKDBOdbcDirect.create('fhir', 100, 0, 'SQL Server Native Client 11.0',
      FIni.ReadString('database', 'server', ''), dbn,
      FIni.ReadString('database', 'username', ''), FIni.ReadString('database', 'password', ''));
  end
  else if FIni.ReadString('database', 'type', '') = 'mysql' then
  begin
    logt('Database mysql://'+FIni.ReadString('database', 'server', '')+'/'+dbn);
    raise Exception.Create('Not Done Yet');
    // principally because of text indexing, but also because I don't know how to connect to a mysql server in an open source way
  end
  else
  begin
    logt('Database not configured');
    raise Exception.Create('Database Access not configured');
  end;
  if not noCheck then
  begin
    conn := FDb.GetConnection('check version');
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
            dbi := TFHIRDatabaseInstaller.create(conn, '');
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
end;

procedure TFHIRService.CheckWebSource;
var
  ini : TIniFile;
  s : String;
begin
  FWebSource := FIni.ReadString('fhir', 'source'+FHIR_GENERATED_VERSION, '');
  if FWebSource = '' then
    FWebSource := FIni.ReadString('fhir', 'source', '');
  logt('Using FHIR Specification at '+FWebSource);

  if not FileExists(IncludeTrailingPathDelimiter(FWebSource)+'version.info') then
    raise Exception.Create('FHIR Publication not found at '+FWebSource);
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(FWebSource)+'version.info');
  try
    s := ini.ReadString('FHIR', 'version', '');
    if s <> FHIR_GENERATED_VERSION then
      raise Exception.Create('FHIR Publication version mismatch: expected '+FHIR_GENERATED_VERSION+', found "'+ini.ReadString('FHIR', 'version', '')+'"')
    else
      logt('Version ok ('+s+')');
  finally
    ini.Free;
  end;
end;

procedure TFHIRService.CloseDatabase;
begin
  FDB.Free;
end;

procedure TFHIRService.Load(fn: String);
var
  f : TFileStream;
begin
  FNotServing := true;
  if FDb = nil then
    ConnectToDatabase;
  CanStart;
  logt('Load database from '+fn);
  f := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
  try
    FWebServer.Transaction(f, true, fn, 'http://hl7.org/fhir', nil);
  finally
    f.Free;
  end;
  logt('done');

  FTerminologyServer.BuildIndexes(true);

  DoStop;
end;

procedure TFHIRService.LoadbyProfile(fn: String; init : boolean);
var
  ini : TIniFile;
  f : TFileStream;
  i : integer;
begin
  FNotServing := true;
  ini := TIniFile.Create(fn);
  try
    fn := fn.Replace('.dstu', '');
    if FDb = nil then
      ConnectToDatabase;
    CanStart;
    if init then
    begin
      fn := ini.ReadString('control', 'load', '');
      logt('Load database from '+fn);
      f := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
      try
        FWebServer.Transaction(f, true, fn, 'http://hl7.org/fhir', ini);
      finally
        f.Free;
      end;
    end;
    for i := 1 to ini.ReadInteger('control', 'files', 0) do
    begin
      fn := ini.ReadString('control', 'file'+inttostr(i), '');
      if (fn <> '') then
      begin
        repeat
          logt('Load '+fn);
          f := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
          try
            FWebServer.Transaction(f, false, fn, ini.ReadString('control', 'base'+inttostr(i), ''), ini);
          finally
            f.Free;
          end;
        until ini.ReadInteger('process', 'start', -1) = -1;
      end;
    end;
    logt('done');
    FTerminologyServer.BuildIndexes(true);
    DoStop;
  finally
    ini.free;
  end;
end;

procedure TFHIRService.LoadTerminologies;
begin
  FTerminologyServer := TTerminologyServer.create(FDB.Link);
  FTerminologyServer.load(FIni);
end;

procedure TFHIRService.UnloadTerminologies;
begin
  FTerminologyServer.Free;
  FTerminologyServer := nil;
end;

procedure TFHIRService.Index;
begin
  FNotServing := true;
  if FDb = nil then
    ConnectToDatabase;
  CanStart;
  logt('index database');
  FTerminologyServer.BuildIndexes(true);
  DoStop;
end;

procedure TFHIRService.validate;
begin
  FNotServing := true;
  if FDb = nil then
    ConnectToDatabase;
  CanStart;
  logt('validate resources');
  FWebServer.DataStore.RunValidation;
  DoStop;
end;

procedure TFHIRService.InitialiseRestServer;
begin
  FWebServer := TFhirWebServer.create(FIni.FileName, FDb, DisplayName, FTerminologyServer);
  FWebServer.Start(not FNotServing);
  FWebServer.DataStore.ForLoad := FindCmdLineSwitch('forload')
end;

procedure TFHIRService.InstallDatabase;
var
  db : TFHIRDatabaseInstaller;
  scim : TSCIMServer;
  salt, un, pw, em, sql, dr : String;
  conn : TKDBConnection;

begin
  // check that user account details are provided
  salt := FIni.ReadString('scim', 'salt', '');
  if (salt = '') then
    raise Exception.Create('You must define a scim salt in the ini file');
  dr := FIni.ReadString('scim', 'default-rights', '');
  if (dr = '') then
    raise Exception.Create('You must define some default rights for SCIM users in the ini file');
  un := FIni.ReadString('admin', 'username', '');
  if (un = '') then
    raise Exception.Create('You must define an admin username in the ini file');
  FindCmdLineSwitch('password', pw, true, [clstValueNextParam]);
  if (pw = '') then
    raise Exception.Create('You must provide a admin password as a parameter to the command');
  em := FIni.ReadString('admin', 'email', '');
  if (em = '') then
    raise Exception.Create('You must define an admin email in the ini file');

  sql := 'C:\work\fhirserver\sql';
  if not DirectoryExists(sql) then
    sql := IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0)))+'sql';


  if FDb = nil then
    ConnectToDatabase;
  logt('mount database');
  scim := TSCIMServer.Create(FDB.Link, '', salt, FIni.ReadString('web', 'host', ''), FIni.ReadString('scim', 'default-rights', ''), true);
  try
    conn := FDb.GetConnection('setup');
    try
      db := TFHIRDatabaseInstaller.create(conn, sql);
      try
        db.Bases.Add('http://healthintersections.com.au/fhir/argonaut');
        db.Bases.Add('http://hl7.org/fhir');
        db.TextIndexing := not FindCmdLineSwitch('no-text-index');
        db.Install(scim);
      finally
        db.free;
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
end;

procedure TFHIRService.UnInstallDatabase;
var
  db : TFHIRDatabaseInstaller;
  conn : TKDBConnection;
begin
  if FDb = nil then
    ConnectToDatabase(true);
  logt('unmount database');
  conn := FDb.GetConnection('setup');
  try
    db := TFHIRDatabaseInstaller.create(conn, '');
    try
      db.UnInstall;
    finally
      db.free;
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
end;

procedure TFHIRService.StopRestServer;
begin
  FWebServer.Stop;
  FWebServer.free;
end;

end.

