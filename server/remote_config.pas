unit remote_config;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  SysUtils, Classes, IniFiles,
  fsl_base, fsl_utilities, fsl_fetcher, fsl_logging, fsl_json,
  fdb_manager, fdb_sqlite3,
  fhir_objects,
  server_config, database_installer, server_factory,
  endpoint_txsvr;

function buildConfigFromSource(src : String) : String;

implementation

type
  TEndPointInfo = class (TFslObject)
  private
    FVersion: TFHIRVersion;
    FFilename: String;
    FPackages: TStringList;
  public
    constructor Create(version : TFHIRVersion); overload;
    destructor Destroy; override;

    property filename : String read FFilename write FFilename;
    property version : TFHIRVersion read FVersion write FVersion;
    property Packages : TStringList read FPackages;

  end;

  TConfigurationBuilder = class (TFslObject)
  private
    FLastPct : Integer;
    FJson : TJsonObject;
    FFolder : String;
    FUrl : String;
    FFiles : TFslStringDictionary;
    FEndPoints : TFslMap<TEndPointInfo>;
    procedure DownloadProgress(sender : TObject; progress : integer);
    procedure downloadFile(fn : String);
    procedure DownloadFiles;
    procedure readConfig;
    procedure buildEndPoint(ver : string);
    procedure buildEndPoints;
    procedure CreateDatabase(v : TFHIRVersion; fn : String);
    function doUpgrade(v : TFHIRVersion; fn : String) : boolean;
    procedure buildConfig(fn : String);
    procedure DownloadFileList(files: TJsonObject);
    procedure seePackages(realm : TJsonObject);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

constructor TConfigurationBuilder.Create;
begin
  inherited;
  FFiles := TFslStringDictionary.create;
  FEndPoints := TFslMap<TEndPointInfo>.create;
  FEndPoints.Add('r2', TEndPointInfo.create(fhirVersionRelease2));
  FEndPoints.Add('r3', TEndPointInfo.create(fhirVersionRelease3));
  FEndPoints.Add('r4', TEndPointInfo.create(fhirVersionRelease4));
  FEndPoints.Add('r5', TEndPointInfo.create(fhirVersionRelease5));
  FEndPoints.defaultValue := nil;
end;

destructor TConfigurationBuilder.Destroy;
begin
  FEndPoints.Free;
  FFiles.Free;
  FJson.Free;
  inherited;
end;

function TConfigurationBuilder.doUpgrade(v : TFHIRVersion; fn : String) : boolean;
var
  sql : TFDBSQLiteManager;
  conn : TFDBConnection;
  installer : TFHIRDatabaseInstaller;
begin
  result := false;
  sql := TFDBSQLiteManager.Create('cfg', fn, true);
  try
    conn := sql.GetConnection('install');
    try
      installer := TFHIRDatabaseInstaller.Create(conn, makeTxFactory(v), makeTxServerFactory(v));
      try
        try
          installer.Upgrade;
          result := true;
        except
          on e : Exception do
            Logging.log('Unable to upgrade existing database '+fn+': '+e.message);
        end;
      finally
        installer.Free;
      end;
      conn.Release;
    except
      on e : exception do
      begin
        conn.Error(e);
      end;
    end;
  finally
    sql.Free;
  end;
end;

procedure TConfigurationBuilder.CreateDatabase(v : TFHIRVersion; fn : String);
var
  sql : TFDBSQLiteManager;
  conn : TFDBConnection;
  installer : TFHIRDatabaseInstaller;
begin
  sql := TFDBSQLiteManager.Create('cfg', fn, true);
  try
    conn := sql.GetConnection('install');
    try
      installer := TFHIRDatabaseInstaller.Create(conn, makeTxFactory(v), makeTxServerFactory(v));
      try
        installer.InstallTerminologyServer;
      finally
        installer.Free;
      end;
      conn.Release;
    except
      on e : exception do
      begin
        conn.Error(e);
      end;
    end;
  finally
    sql.Free;
  end;
end;

procedure TConfigurationBuilder.buildConfig(fn: String);
var
  cfg : TFHIRServerConfigFile;
  n, v : String;
  i, rn : integer;
  sct : TFHIRServerConfigSection;
  ini : TIniFile;
begin
  rn := 1;
  if FileExists(fn) then
  begin
    cfg := TFHIRServerConfigFile.Create(fn);
    try
      rn := StrToIntDef(cfg.section['service']['runNumber'].value, 1);
    finally
      cfg.free;
    end;
    DeleteFile(fn);
  end;

  cfg := TFHIRServerConfigFile.Create(fn);
  try
    cfg.section['service']['runNumber'].value := inttostr(rn);
    ini := TIniFile.Create(FilePath([ExtractFilePath(ParamStr(0)), 'web.ini']));
    try
      cfg.web['host'].value := ini.ReadString('web', 'host', 'localhost');
      cfg.web['http'].value := inttostr(ini.ReadInteger('web', 'http', 80));
      cfg.web['https'].value := ini.ReadString('web', 'https', '');
      cfg.web['certname'].value := ini.ReadString('web', 'certname', '');
      cfg.web['cacertname'].value := ini.ReadString('web', 'cacertname', '');
      cfg.web['certkey'].value := ini.ReadString('web', 'certkey', '');
      cfg.web['password'].value := ini.ReadString('web', 'password', '');
      cfg.web['telnet-password'].value := ini.ReadString('admin', 'telnet-pword', NewGuidId);
      cfg.admin['email'].value := ini.ReadString('admin', 'email', 'noone@fhir.org');
      cfg.admin['ownername'].value := ini.ReadString('admin', 'user', 'Local User');
    finally
      ini.Free;
    end;

    cfg.web['http-max-conn'].value := '50';
    cfg.web['http-cache-time'].value := '1000';
    cfg.web['plain-mode'].value := 'false';
    cfg.web['caching'].value := 'true';
    cfg.service['langfile'].value := FilePath([ExtractFilePath(ParamStr(0)), 'lang.dat']);
    cfg.admin['scim-salt'].value := NewGuidId;

    for n in FFiles.Keys do
    begin
      sct := cfg.section['terminologies'].section[PathTitle(n)];
      sct['type'].value := FFiles[n];
      sct['active'].value := 'true';
      if StringArrayExists(['rxnorm', 'ndc', 'unii'], FFiles[n]) then
      begin
        sct['db-type'].value := 'sqlite';
        sct['db-file'].value := FilePath([FFolder, n]);
        sct['db-auto-create'].value := 'false';
      end
      else
      begin
        sct['source'].value := FilePath([FFolder, n]);
        if (FFiles[n] = 'snomed!') then
        begin
          sct['type'].value := 'snomed';
          sct['default'].value := 'true';
        end;
      end;
    end;

    for n in FEndPoints.Keys do
    begin
      if FEndPoints[n].filename <> '' then
      begin
        sct := cfg.section['endpoints'].section[n];
        sct['type'].value := 'terminology';
        sct['path'].value := '/'+n;
        sct['version'].value := n;
        sct['active'].value := 'true';
        sct['security'].value := 'open';
        sct['db-type'].value := 'sqlite';
        sct['db-file'].value := FEndPoints[n].filename;
        sct['db-auto-create'].value := 'false';
        sct['packages'].values.Assign(FEndPoints[n].Packages);
      end;
    end;

    cfg.Save;
  finally
    cfg.Free;
  end;
end;

procedure TConfigurationBuilder.buildEndPoint(ver : string);
var
  fn : String;
  ep : TEndPointInfo;
begin
  ep := FEndPoints['r'+ver];
  if (ep = nil) then
    raise EFslException.Create('Version "'+ver+'" is unknown');

  fn := FilePath([FFolder, 'endpoint-r'+ver+'.db']);
  ep.filename := fn;
  if not FileExists(fn) then
    CreateDatabase(ep.version, fn)
  else if not doUpgrade(ep.version, fn) then
  begin
    Logging.log('Rebuilding database. Closure tables will be lost');
    CreateDatabase(ep.version, fn);
  end;
end;

procedure TConfigurationBuilder.buildEndPoints;
var
  v, vl : String;
begin
  if not getCommandLineParam('version', vl) then
    vl := '*';
  if (vl = '*') then
    vl := '2,3,4,5';

  for v in vl.Split([';', ',']) do
    buildEndPoint(v);
end;

procedure TConfigurationBuilder.DownloadFileList(files : TJsonObject);
var
  fn, r : String;
begin
  for fn in files.properties.Keys do
  begin
    FFiles.Add(fn, (files.node[fn] as TJsonString).value);
    downloadFile(fn);
  end;
end;

procedure TConfigurationBuilder.DownloadFiles;
var
  realm, files : TJsonObject;
  r, i : String;
begin
  Logging.log('Realm: uv');
  realm := FJson.forceObj['uv'];
  SeePackages(realm);
  files := realm.forceObj['files'];
  DownloadFileList(files);

  if not getCommandLineParam('realm', r) then
    r := '*';

  if (r = '*') then
  begin
    for i in FJson.properties.Keys do
      if i <> 'uv' then
      begin
        Logging.log('Realm: '+i);
        realm := FJson.forceObj[i];
        SeePackages(realm);
        files := realm.forceObj['files'];
        DownloadFileList(files);
      end;
  end
  else
  begin
    for i in r.split([';', ',']) do
    begin
      Logging.log('Realm: '+i);
      realm := FJson.forceObj[i];
      SeePackages(realm);
      files := realm.forceObj['files'];
      DownloadFileList(files);
    end;
  end;
end;

procedure TConfigurationBuilder.DownloadProgress(sender: TObject; progress: integer);
begin
  if progress >= FLastPct + 2 then
  begin
    FLastPct := progress;
    Logging.continue('.');
  end;
end;

procedure TConfigurationBuilder.readConfig;
var
  json : TJsonObject;
  src : String;
begin
  src := URLPath([FUrl, 'config.json']);
  Logging.log('Read Zero Config from '+src);
  if (src.StartsWith('file:')) then
    FJson := TJSONParser.ParseFile(src.Substring(5))
  else
    FJson := TInternetFetcher.fetchJson(src);
end;

procedure TConfigurationBuilder.seePackages(realm: TJsonObject);
var
  i : integer;
  pck : TJsonObject;
begin
  pck := realm.forceObj['packages'];

  for i := 0 to pck.forceArr['r5'].Count - 1 do
    FEndPoints['r5'].Packages.Add(pck.arr['r5'].Value[i]);

  for i := 0 to pck.forceArr['r4'].Count - 1 do
    FEndPoints['r4'].Packages.Add(pck.arr['r4'].Value[i]);

  for i := 0 to pck.forceArr['r3'].Count - 1 do
    FEndPoints['r3'].Packages.Add(pck.arr['r3'].Value[i]);

  for i := 0 to pck.forceArr['r2'].Count - 1 do
    FEndPoints['r2'].Packages.Add(pck.arr['r2'].Value[i]);
end;

procedure TConfigurationBuilder.downloadFile(fn : String);
var
  src, tgt : String;
  fetcher : TInternetFetcher;
  start : TDateTime;
begin
  src := UrlPath([FUrl, fn]);
  tgt := FilePath([FFolder, fn]);
  if (src.StartsWith('file:')) then
  begin
    if (FileExists(tgt)) then
      Logging.log(fn+' already copied')
    else
    begin
      Logging.start('Copy '+fn);
      BytesToFile(FileToBytes(src.Substring(5)), tgt);
      Logging.finish(' Done');
    end;
  end
  else
  begin
    FLastPct := 0;
    if FileExists(tgt) then
      Logging.log(fn+' already downloaded')
    else
    begin
      Logging.start('Download '+fn);
      try
        start := UniversalDateTime;
        fetcher := TInternetFetcher.Create;
        try
          fetcher.OnProgress := DownloadProgress;
          fetcher.URL := src;
          fetcher.Fetch;
          fetcher.Buffer.SaveToFileName(tgt);
          Logging.finish(' Done ('+DescribeBytes(fetcher.buffer.size)+', '+DescribePeriod(UniversalDateTime - start)+')');
        finally
          fetcher.Free;
        end;
      except
        on e : Exception do
        begin
          Logging.finish(' '+e.Message);
          raise;
        end;
      end;
    end;
  end;
end;

function buildConfigFromSource(src : String) : String;
var
  cb : TConfigurationBuilder;
  dir : String;
begin
 if not getCommandLineParam('local', dir) then
   dir := UserFolder;

  result := FilePath([dir, 'fhir-server', 'fhir-server-config.cfg']);
  try
    cb := TConfigurationBuilder.Create;
    try
      cb.FUrl := src;
      cb.FFolder := ExtractFilePath(result);
      Logging.log('Local Config in '+cb.FFolder);
      if not FolderExists(cb.FFolder) then
        ForceDirectories(cb.FFolder);
      cb.readConfig;
      cb.DownloadFiles;
      cb.buildEndPoints;
      cb.buildConfig(result);
    finally
      cb.Free;
    end;
  except
    on e : Exception do
    begin
      Logging.log('Zero Configuration Process failed: '+e.message);
      if FileExists(result) then
        Logging.log('Continuing on last successful configuration')
      else
      begin
        Logging.log('First time, so can''t continue.');
        raise;
      end;
    end;
  end;
end;

{ TEndPointInfo }

constructor TEndPointInfo.Create(version : TFHIRVersion);
begin
  inherited Create;
  FPackages := TStringList.Create;
  FVersion := version;
end;

destructor TEndPointInfo.Destroy;
begin
  FPackages.Free;
  inherited;
end;

end.
