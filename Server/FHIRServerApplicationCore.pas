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
  SysUtils, IniFiles, ActiveX, ComObj,
  SystemService, SystemSupport,
  SnomedImporter, SnomedServices, SnomedExprssions,
  KDBManager, KDBOdbcExpress, KDBDialects,
  FHIRRestServer, DBInstaller, FHIRConstants;

Type
  TFHIRService = class (TSystemService)
  private
    TestMode : Boolean;
    FIni : TIniFile;
    FDb : TKDBManager;
    FWebServer : TFhirWebServer;
    FWebSource : String;
    procedure ConnectToDatabase;
    procedure LoadTerminologies;
    procedure InitialiseRestServer;
    procedure StopRestServer;
    procedure UnloadTerminologies;
    procedure CloseDatabase;
    procedure CheckWebSource;
    function dbExists : Boolean;
    procedure TestSnomedExpressions;
  protected
    function CanStart : boolean; Override;
    procedure DoStop; Override;
  public
    Constructor Create(const ASystemName, ADisplayName, AIniName: String);
    Destructor Destroy; override;

    procedure ExecuteTests;
    procedure InstallDatabase;
    procedure UnInstallDatabase;
  end;

procedure ExecuteFhirServer;

implementation

procedure ExecuteFhirServer;
var
  iniName : String;
  svcName : String;
  dispName : String;
  dir : String;
  svc : TFHIRService;
begin
  CoInitialize(nil);
  if not FindCmdLineSwitch('ini', iniName, true, [clstValueNextParam]) then
  begin
    if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'fhir.local.ini') then
      iniName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'fhir.local.ini'
    else
      iniName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'fhir.ini';
  end;
  writeln('FHIR Service. Using ini file '+iniName);

  if not FindCmdLineSwitch('name', svcName, true, [clstValueNextParam]) then
    svcName := 'fhirserver';
  if not FindCmdLineSwitch('title', dispName, true, [clstValueNextParam]) then
    dispName := 'FHIR Server';

  svc := TFHIRService.Create(svcName, dispName, iniName);
  try
    if FindCmdLineSwitch('mount') then
      svc.InstallDatabase
    else if FindCmdLineSwitch('unmount') then
      svc.UninstallDatabase
    else if FindCmdLineSwitch('remount') then
    begin
      svc.UninstallDatabase;
      svc.InstallDatabase
    end
    else if FindCmdLineSwitch('tests') then
      svc.ExecuteTests
    else if FindCmdLineSwitch('snomed-rf1', dir, true, [clstValueNextParam]) then
      svc.FIni.WriteString('snomed', 'cache', importSnomedRF1(dir, svc.FIni.ReadString('internal', 'store', IncludeTrailingPathDelimiter(ProgData)+'fhirserver')))
    else if FindCmdLineSwitch('snomed-rf2', dir, true, [clstValueNextParam]) then
      svc.FIni.WriteString('snomed', 'cache', importSnomedRF2(dir, svc.FIni.ReadString('internal', 'store', IncludeTrailingPathDelimiter(ProgData)+'fhirserver')))
//    procedure ReIndex;
//    procedure clear(types : String);
    else
      svc.Execute;
  finally
    svc.Free;
  end;
end;

{ TFHIRService }

constructor TFHIRService.Create(const ASystemName, ADisplayName, AIniName: String);
begin
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
    LoadTerminologies;
    InitialiseRestServer;
    result := true;
  except
    on e : Exception do
      Writeln(e.Message);
  end;
  writeln('started');
end;

procedure TFHIRService.DoStop;
begin
  try
    writeln('stop: '+StopReason);
    StopRestServer;
    UnloadTerminologies;
  except
    on e : Exception do
      Writeln(e.Message);
  end;
end;

procedure TFHIRService.ExecuteTests;
var
  meta : TKDBMetaData;
begin
  TestMode := true;
  if FDb = nil then
    ConnectToDatabase;
  // was there anything left?
  if dbExists then
    UnInstallDatabase;
  InstallDatabase;
  LoadTerminologies;

  // library checks:
    // snomed checks
    TestSnomedExpressions;

  // initial state
  CanStart;

  DoStop;

  // 2nd cycle: after everything is loaded
  CanStart;
  DoStop;

  UnloadTerminologies;
  UnInstallDatabase;

  // final tests - these go on for a very long time,
  // import rf1
  // import rf2
end;

Procedure TFHIRService.ConnectToDatabase;
begin
  if TestMode then
    FDb := TKDBOdbcDirect.create('fhir', 100, 'SQL Server Native Client 11.0', '(local)', 'fhir-test', '', '')
  else if FIni.ReadString('database', 'type', '') = 'mssql' then
  begin
    Writeln('Database mssql://'+FIni.ReadString('database', 'server', '')+'/'+FIni.ReadString('database', 'database', ''));
    FDb := TKDBOdbcDirect.create('fhir', 100, 'SQL Server Native Client 11.0',
      FIni.ReadString('database', 'server', ''), FIni.ReadString('database', 'database', ''),
      FIni.ReadString('database', 'username', ''), FIni.ReadString('database', 'password', ''));
  end
  else if FIni.ReadString('database', 'type', '') = 'mysql' then
  begin
    Writeln('Database mysql://'+FIni.ReadString('database', 'server', '')+'/'+FIni.ReadString('database', 'database', ''));
    raise Exception.Create('Not Done Yet')
  end
  else
  begin
    Writeln('Database not configured');
    raise Exception.Create('Database Access not configured');
  end;
end;

procedure TFHIRService.CheckWebSource;
var
  ini : TIniFile;
  s : String;
begin
  FWebSource := FIni.ReadString('fhir', 'source', '');
  writeln('Using FHIR Specification at '+FWebSource);

  if not FileExists(IncludeTrailingPathDelimiter(FWebSource)+'version.info') then
    raise Exception.Create('FHIR Publication not found at '+FWebSource);
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(FWebSource)+'version.info');
  try
    s := ini.ReadString('FHIR', 'version', '');
    if s <> FHIR_GENERATED_VERSION then
      raise Exception.Create('FHIR Publication version mismatch: expected '+FHIR_GENERATED_VERSION+', found "'+ini.ReadString('FHIR', 'version', '')+'"');
    if ini.ReadString('FHIR', 'revision', '??') <> FHIR_GENERATED_REVISION then
      raise Exception.Create('FHIR Publication version mismatch: expected '+FHIR_GENERATED_REVISION+', found '+ini.ReadString('FHIR', 'revision', '??'));
  finally
    ini.Free;
  end;
end;

procedure TFHIRService.CloseDatabase;
begin
  FDB.Free;
end;

procedure TFHIRService.LoadTerminologies;
var
  sf : String;
begin
  sf := FIni.ReadString('snomed', 'cache', '');
  if FileExists(sf) then
  begin
    write('Load Snomed');
    GSnomeds := TSnomedServiceList.Create;
    GSnomeds.DefaultDefinition := TSnomedServices.Create;
    GSnomeds.DefaultDefinition.Load(sf);
    writeln(' - done');
  end;
end;

procedure TFHIRService.UnloadTerminologies;
begin
  GSnomeds.Free;
end;

procedure TFHIRService.InitialiseRestServer;
begin
  FWebServer := TFhirWebServer.create(FIni.FileName, FDb, DisplayName);
  FWebServer.Start;
end;

procedure TFHIRService.InstallDatabase;
var
  db : TFHIRDatabaseInstaller;
  conn : TKDBConnection;
begin
  if FDb = nil then
    ConnectToDatabase;
  Writeln('mount database');
  conn := FDb.GetConnection('setup');
  try
    db := TFHIRDatabaseInstaller.create(conn);
    try
      db.Install;
    finally
      db.free;
    end;
    conn.Release;
    Writeln('done');
  except
     on e:exception do
     begin
       Writeln('Error: '+e.Message);
       conn.Error(e);
       raise;
     end;
  end;
end;

procedure TFHIRService.UnInstallDatabase;
var
  db : TFHIRDatabaseInstaller;
  conn : TKDBConnection;
begin
  if FDb = nil then
    ConnectToDatabase;
  Writeln('unmount database');
  conn := FDb.GetConnection('setup');
  try
    db := TFHIRDatabaseInstaller.create(conn);
    try
      db.UnInstall;
    finally
      db.free;
    end;
    conn.Release;
    Writeln('done');
  except
     on e:exception do
     begin
       Writeln('Error: '+e.Message);
       conn.Error(e);
       raise;
     end;
  end;
end;

procedure TFHIRService.StopRestServer;
begin
  FWebServer.Stop;
  FWebServer.free;
end;

procedure TFHIRService.TestSnomedExpressions;
begin
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '297186008 | motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '297186008').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '217724009 | accident caused by blizzard | +297186008 | motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '217724009 +297186008 | motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '217724009'#13#10' + 297186008 | motorcycle accident |'#13#10'').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '217724009 + 297186008 '#13#10'| motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '217724009 | accident caused by blizzard |:116680003 | is a | =297186008 | motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '297186008 | motorcycle accident |:116680003 | is a | =217724009 | accident caused by blizzard |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '83152002 | oophorectomy |: 260686004 | method |=257820006| laser excision - action |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '313056006 | epiphysis of ulna |:272741003 | laterality | =7771000 | left |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '119189000 | ulna part | + 312845000 | epiphysis of upper limb |:272741003 | laterality | =7771000 | left |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '83152002 | oophorectomy |:260686004 | method |=257820006| laser excision - action |,363704007 | procedure site | =20837000 | structure of right ovary |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '120053002 | Salpingectomy |:260686004 | method | =261519002 | diathermy excision - action |,363704007 | procedure site | =113293009 | structure of left fallopian tube |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '116028008 | salpingo-oophorectomy |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '71388002 | procedure |:{260686004 | method | =129304002 | excision - action |,405813007 | procedure site - Direct | =15497006 | ovarian '+'structure |}{260686004 | method | =129304002 | excision - action |,405813007 | procedure site - Direct | =31435000 | fallopian tube structure |}').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '116028008 | salpingo-oophorectomy |: {260686004 | method |=257820006| laser excision - action |,363704007 | procedure site | =20837000 | structure of right ovary |}{260686004 | '+'method | =261519002 | diathermy excision - action |,363704007 | procedure site | =113293009 | structure of left fallopian tube |}').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '71620000 | fracture of femur |: 42752001 | due to | = (217724009 | accident caused by blizzard | +297186008 | motorcycle accident |)').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '24136001 | hip joint structure |: 272741003 | laterality | =7771000 | left |').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '397956004 | prosthetic arthroplasty of the hip |:363704007 | procedure site | = (24136001 | hip joint structure | :272741003 | laterality | =7771000 | left |)').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '397956004 | prosthetic arthroplasty of the hip |:363704007 | procedure site | = (24136001 | hip joint structure| :272741003 | laterality | =7771000 | left |) {363699004 |'+' direct device | =304120007 | total hip replacement prosthesis |,260686004 | method | =257867005 | insertion - action |}').Free;
  TSnomedExpressionParser.Parse(GSnomeds.DefaultDefinition, '243796009 | situation with explicit context |: {363589002 | associated procedure | = (397956004 | prosthetic arthroplasty of the hip |:363704007 | procedure site | = (24136001 | '+'hip joint structure | :272741003 | laterality | =7771000 | left |) {363699004 | direct device | =304120007 | total hip replacement prosthesis |, '+'260686004 | method | =257867005 | insertion - action |}), 408730004 | procedure context | =385658003 | done |, 408731000 | temporal context | =410512000 | current or specified |, 408732007 | subject relationship context |=410604004 | subject of record | }').Free;
end;

end.

(*


procedure TFHIRRepositoryManager.doSweep;
var
  i, t : integer;
begin
  FLock.Lock('sweep');
  try
    t := FRepositories.count;
  finally
    FLock.Unlock;
  end;
  for i := 0 to t - 1 do
    TFHIRRepository(FRepositories.Objects[i]).Sweep;
end;

procedure TFHIRRepository.ReIndex;
var
  store : TFhirStorage;
begin
  store := AcquireStorage('en');
  try
    store.Connection := nil; // gwdb.ConnMan.GetConnection('fhir');
    try
      store.ReIndex;
      store.Connection.Release;
    except
      on e:exception do
      begin
        store.Connection.Error(e);
        raise;
      end
    end;
  finally
    store.free;
  end;
end;

procedure TFHIRRepository.clear(types: String);
var
  l : String;
  a : TFhirResourceTypeSet;
  i : integer;
  store : TFhirStorage;
begin
  a := [];
  while (types <> '') do
  begin
    StringSplit(types, ';', l, types);
    if (l = 'all') then
      a := ALL_RESOURCE_TYPES;
    i := StringArrayIndexOfInsensitive(CODES_TFhirResourceType, l);
    if i >= 0 then
      a := a + [TFhirResourceType(i)];
  end;
  store := AcquireStorage('en');
  try
    store.Connection := nil; // gwdb.ConnMan.GetConnection('fhir');
    try
      store.Clear(a);
      store.Connection.Release;
    except
      on e:exception do
      begin
        store.Connection.Error(e);
        raise;
      end
    end;
  finally
    store.free;
  end;
end;
*)

