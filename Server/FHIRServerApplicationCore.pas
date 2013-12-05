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
  SystemService,
  KDBManager, KDBOdbcExpress, KDBDialects,
  FHIRRestServer, DBInstaller, FHIRConstants;

Type
  TFHIRService = class (TSystemService)
  private
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
  protected
    function CanStart : boolean; Override;
    procedure DoStop; Override;
  public
    Constructor Create(const ASystemName, ADisplayName, AIniName: String);
    Destructor Destroy; override;

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
  ConnectToDatabase;
  CheckWebSource;
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
    StopRestServer;
    UnloadTerminologies;
  except
    on e : Exception do
      Writeln(e.Message);
  end;
end;

Procedure TFHIRService.ConnectToDatabase;
begin
  if FIni.ReadString('database', 'type', '') = 'mssql' then
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
begin
  { todo }
end;

procedure TFHIRService.UnloadTerminologies;
begin
  { todo }
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
  conn := FDb.GetConnection('setup');
  try
    db := TFHIRDatabaseInstaller.create(conn);
    try
      db.Install;
    finally
      db.free;
    end;
    conn.Release;
  except
     on e:exception do
     begin
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
  conn := FDb.GetConnection('setup');
  try
    db := TFHIRDatabaseInstaller.create(conn);
    try
      db.UnInstall;
    finally
      db.free;
    end;
    conn.Release;
  except
     on e:exception do
     begin
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

