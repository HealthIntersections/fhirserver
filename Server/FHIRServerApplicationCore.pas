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
  FHIRRestServer, DBInstaller, FHIRConstants, FhirServerTests, FHIROperation, FHIRDataStore,
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

    procedure ConnectToDatabase;
    procedure LoadTerminologies;
    procedure InitialiseRestServer;
    procedure StopRestServer;
    procedure UnloadTerminologies;
    procedure CloseDatabase;
    procedure CheckWebSource;
    function dbExists : Boolean;
  protected
    function CanStart : boolean; Override;
    procedure DoStop; Override;
    procedure dump; override;
  public
    Constructor Create(const ASystemName, ADisplayName, AIniName: String);
    Destructor Destroy; override;

    procedure ExecuteTests;
    procedure Load(fn : String);
    procedure LoadbyProfile(fn : String; init : boolean);
    procedure Index;
    procedure InstallDatabase;
    procedure UnInstallDatabase;
  end;

procedure ExecuteFhirServer;

implementation

uses
  JclDebug;

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

  lines : TStringList;
  s : String;
begin
  try
    CoInitialize(nil);
    if not FindCmdLineSwitch('ini', iniName, true, [clstValueNextParam]) then
      iniName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'fhirserver.ini';

    if not FindCmdLineSwitch('name', svcName, true, [clstValueNextParam]) then
      svcName := 'fhirserver';
    if not FindCmdLineSwitch('title', dispName, true, [clstValueNextParam]) then
      dispName := 'FHIR Server (DSTU2)';
    iniName := iniName.replace('.dstu', '.dev');
    if JclExceptionTrackingActive then
      writelnt('FHIR Service (DSTU2). Using ini file '+iniName+' with stack dumps on')
    else
      writelnt('FHIR Service (DSTU2). Using ini file '+iniName+' (no stack dumps)');
    dispName := dispName + ' (DSTU2)';


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
      else if FindCmdLineSwitch('profile', fn, true, [clstValueNextParam]) then
        svc.LoadByProfile(fn, false)
      else if FindCmdLineSwitch('index') then
        svc.index
      else if FindCmdLineSwitch('tests') then
        svc.ExecuteTests
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
        generateRxStems(TKDBOdbcDirect.create('fhir', 100, 'SQL Server Native Client 11.0',
          svc.FIni.ReadString('database', 'server', ''), svc.FIni.ReadString('RxNorm', 'database', ''),
          svc.FIni.ReadString('database', 'username', ''), svc.FIni.ReadString('database', 'password', '')))
      end
      else if FindCmdLineSwitch('ncistems', dir, true, []) then
      begin
        generateRxStems(TKDBOdbcDirect.create('fhir', 100, 'SQL Server Native Client 11.0',
          svc.FIni.ReadString('database', 'server', ''), svc.FIni.ReadString('NciMeta', 'database', ''),
          svc.FIni.ReadString('database', 'username', ''), svc.FIni.ReadString('database', 'password', '')))
      end
  //    procedure ReIndex;
  //    procedure clear(types : String);
      else
        svc.Execute;
    finally
      svc.Free;
    end;
  except
    on e : Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message+#13#10#13#10+ExceptionStack(e));
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
      result := meta.Tables.Table['Config'] <> nil;
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
      writelnt(E.ClassName+ ': ' + E.Message+#13#10#13#10+ExceptionStack(e));
      raise;
    end;
  end;
  writelnt('started ('+inttostr((GetTickCount - FStartTime) div 1000)+'secs)');
end;

procedure TFHIRService.DoStop;
begin
  try
    writelnt('stop: '+StopReason);
    StopRestServer;
    UnloadTerminologies;
  except
    on e : Exception do
      Writelnt(E.ClassName + ': ' + E.Message+#13#10#13#10+ExceptionStack(e));
  end;
end;

procedure TFHIRService.dump;
begin
  inherited;
  writelnt(KDBManagers.Dump);
  writelnt(FWebServer.dump);
end;

procedure TFHIRService.ExecuteTests;
var
  tests : TFhirServerTests;
begin
  try
    TestMode := true;
    tests := TFhirServerTests.Create;
    try
      tests.ini := FIni;
      tests.executeLibrary;
      if FDb = nil then
        ConnectToDatabase;
      if dbExists then
        UnInstallDatabase;
      InstallDatabase;
      LoadTerminologies;
      tests.TerminologyServer := FTerminologyServer.Link;
      tests.executeBefore;

      CanStart;
      TFHIRQuestionnaireBuilderTests.runTests(FIni, FWebServer.DataStore);
      tests.executeRound1;
      DoStop;

      CanStart;
      tests.executeRound2;
      DoStop;

      UnloadTerminologies;
      UnInstallDatabase;

      tests.executeAfter; // final tests - these go on for a very long time,
    finally
      tests.Free;
    end;
    ExitCode := 0;
  except
    on e: Exception do
    begin
      writelnt(e.Message);
      ExitCode := 1;
    end;
  end;
end;

Procedure TFHIRService.ConnectToDatabase;
begin
  if TestMode then
    FDb := TKDBOdbcDirect.create('fhir', 100, 'SQL Server Native Client 11.0', '(local)', 'fhir-test', '', '')
  else if FIni.ReadString('database', 'type', '') = 'mssql' then
  begin
    writelnt('Database mssql://'+FIni.ReadString('database', 'server', '')+'/'+FIni.ReadString('database', 'database', ''));
    FDb := TKDBOdbcDirect.create('fhir', 100, 'SQL Server Native Client 11.0',
      FIni.ReadString('database', 'server', ''), FIni.ReadString('database', 'database', ''),
      FIni.ReadString('database', 'username', ''), FIni.ReadString('database', 'password', ''));
  end
  else if FIni.ReadString('database', 'type', '') = 'mysql' then
  begin
    writelnt('Database mysql://'+FIni.ReadString('database', 'server', '')+'/'+FIni.ReadString('database', 'database', ''));
    raise Exception.Create('Not Done Yet')
  end
  else
  begin
    writelnt('Database not configured');
    raise Exception.Create('Database Access not configured');
  end;
end;

procedure TFHIRService.CheckWebSource;
var
  ini : TIniFile;
  s : String;
begin
  FWebSource := FIni.ReadString('fhir', 'source', '');
  writelnt('Using FHIR Specification at '+FWebSource);

  if not FileExists(IncludeTrailingPathDelimiter(FWebSource)+'version.info') then
    raise Exception.Create('FHIR Publication not found at '+FWebSource);
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(FWebSource)+'version.info');
  try
    s := ini.ReadString('FHIR', 'version', '');
    if s <> FHIR_GENERATED_VERSION then
      raise Exception.Create('FHIR Publication version mismatch: expected '+FHIR_GENERATED_VERSION+', found "'+ini.ReadString('FHIR', 'version', '')+'"');
  //  if ini.ReadString('FHIR', 'revision', '??') <> FHIR_GENERATED_REVISION then
  //    raise Exception.Create('FHIR Publication version mismatch: expected '+FHIR_GENERATED_REVISION+', found '+ini.ReadString('FHIR', 'revision', '??'));
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
  writelnt('Load database from '+fn);
  f := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
  try
    FWebServer.Transaction(f, true, fn, 'http://hl7.org/fhir', nil);
  finally
    f.Free;
  end;
  writelnt('done');

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
      writelnt('Load database from '+fn);
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
          writelnt('Load '+fn);
          f := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
          try
            FWebServer.Transaction(f, false, fn, ini.ReadString('control', 'base'+inttostr(i), ''), ini);
          finally
            f.Free;
          end;
        until ini.ReadInteger('process', 'start', -1) = -1;
      end;
    end;
    writelnt('done');
    FTerminologyServer.BuildIndexes(true);
    DoStop;
  finally
    ini.free;
  end;
end;

procedure TFHIRService.LoadTerminologies;
begin
  FTerminologyServer := TTerminologyServer.create(FDB);
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
  writelnt('index database');
  FTerminologyServer.BuildIndexes(true);
  DoStop;
end;

procedure TFHIRService.InitialiseRestServer;
begin
  FWebServer := TFhirWebServer.create(FIni.FileName, FDb, DisplayName, FTerminologyServer);
  FWebServer.Start(not FNotServing);
end;

procedure TFHIRService.InstallDatabase;
var
  db : TFHIRDatabaseInstaller;
  conn : TKDBConnection;
  conntx : TKDBConnection;
  scim : TSCIMServer;
  salt, un, pw, em, sql, dr : String;

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
  writelnt('mount database');
  scim := TSCIMServer.Create(FDB, '', salt, FIni.ReadString('web', 'host', ''), FIni.ReadString('scim', 'default-rights', ''), true);
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
      writelnt('done');
    except
       on e:exception do
       begin
         writelnt('Error: '+e.Message);
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
  conn, connTx : TKDBConnection;
begin
  if FDb = nil then
    ConnectToDatabase;
  writelnt('unmount database');
  conn := FDb.GetConnection('setup');
  try
    db := TFHIRDatabaseInstaller.create(conn, '');
    try
      db.UnInstall;
    finally
      db.free;
    end;
    conn.Release;
    writelnt('done');
  except
     on e:exception do
     begin
       writelnt('Error: '+e.Message);
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

