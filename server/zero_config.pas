unit zero_config;

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
  fsl_base, fsl_utilities, fsl_fetcher, fsl_logging, fsl_json, fsl_threads,
  fdb_manager, fdb_sqlite3,
  fhir_objects,
  server_config, database_installer, server_factory, server_constants,
  endpoint_txsvr;

function loadRemoteConfig(params : TCommandLineParameters; src : String; local : TIniFile) : String;

implementation

type

  { TEndPointInfo }

  TEndPointInfo = class (TFslObject)
  private
    FActive: boolean;
    FMode: String;
    FVersion: TFHIRVersion;
    FFilename: String;
    FPackages: TStringList;
    FKind: String;
  public
    constructor Create(version : TFHIRVersion); overload;
    destructor Destroy; override;

    property active : boolean read FActive write FActive;
    property kind : String read FKind write FKind;
    property mode : String read FMode write FMode;
    property filename : String read FFilename write FFilename;
    property version : TFHIRVersion read FVersion write FVersion;
    property Packages : TStringList read FPackages;

  end;

  { TConfigurationBuilder }

  TConfigurationBuilder = class (TFslObject)
  private
    FParams : TCommandLineParameters;
    FLastPct : Integer;
    FJson : TJsonObject;
    FFolder : String;
    FUrl : String;
    FFiles : TFslStringDictionary;
    FEndPoints : TFslMap<TEndPointInfo>;
    FMode : String;
    procedure DownloadProgress(sender : TObject; progress : integer);
    procedure downloadFile(fn : String); overload;
    procedure downloadFile(src, tgt : String); overload;
    procedure DownloadFiles;
    function fixDBPath(fn: String): String;
    procedure readConfig;
    procedure buildEndPoint(ep : TEndPointInfo);
    procedure setupEndPoints;
    procedure CreateDatabase(v : TFHIRVersion; fn : String);
    function doUpgrade(v : TFHIRVersion; fn : String) : boolean;
    procedure buildConfig(fn : String; local : TCustomIniFile);
    procedure DownloadFileList(files: TJsonObject);
    procedure seePackages(realm : TJsonObject);
  public
    constructor Create(params : TCommandLineParameters);
    destructor Destroy; override;
  end;

constructor TConfigurationBuilder.Create(params : TCommandLineParameters);
begin
  inherited Create;
  FParams := params;
  FFiles := TFslStringDictionary.Create;
  FEndPoints := TFslMap<TEndPointInfo>.Create;
  FEndPoints.Add('r2', TEndPointInfo.Create(fhirVersionRelease2));
  FEndPoints.Add('r3', TEndPointInfo.Create(fhirVersionRelease3));
  FEndPoints.Add('r4', TEndPointInfo.Create(fhirVersionRelease4));
  FEndPoints.Add('r5', TEndPointInfo.Create(fhirVersionRelease5));
  FEndPoints.defaultValue := nil;
end;

destructor TConfigurationBuilder.Destroy;
begin
  FParams.free;
  FEndPoints.free;
  FFiles.free;
  FJson.free;
  inherited;
end;

function TConfigurationBuilder.doUpgrade(v : TFHIRVersion; fn : String) : boolean;
var
  sql : TFDBSQLiteManager;
  conn : TFDBConnection;
  installer : TFHIRDatabaseInstaller;
begin
  result := false;
  sql := TFDBSQLiteManager.Create('cfg', fn, false, true);
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
        installer.free;
      end;
      conn.Release;
    except
      on e : exception do
      begin
        conn.Error(e);
      end;
    end;
  finally
    sql.free;
  end;
end;

procedure TConfigurationBuilder.CreateDatabase(v : TFHIRVersion; fn : String);
var
  sql : TFDBSQLiteManager;
  conn : TFDBConnection;
  installer : TFHIRDatabaseInstaller;
begin
  sql := TFDBSQLiteManager.Create('cfg', fn, false, true);
  try
    conn := sql.GetConnection('install');
    try
      installer := TFHIRDatabaseInstaller.Create(conn, makeTxFactory(v), makeTxServerFactory(v));
      try
        installer.InstallTerminologyServer;
      finally
        installer.free;
      end;
      conn.Release;
    except
      on e : exception do
      begin
        conn.Error(e);
      end;
    end;
  finally
    sql.free;
  end;
end;

function TConfigurationBuilder.fixDBPath(fn : String) : String;
begin
  if (fn.StartsWith('http:') or fn.StartsWith('https:')) then
  begin
    result := FilePath([FFolder, fn.Substring(fn.LastIndexOf('/')+1)]);
    downloadFile(fn, result);
  end
  else if (ExtractFilePath(fn) = '') then
    result := FilePath([FFolder, fn])
  else
    result := fn;
end;

function def(s1, s2, s3 : String) : String;
begin
  if (s1 <> '') then
    result := s1
  else if (s2 <> '') then
    result := s2
  else
    result := s3;
end;

procedure TConfigurationBuilder.buildConfig(fn: String; local : TCustomIniFile);
var
  cfg : TFHIRServerConfigFile;
  n, v : String;
  rn : integer;
  sct : TFHIRServerConfigSection;
  ep, o : TJsonObject;
  lwi, mode : String;
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

    cfg.web['host'].value := def(local.ReadString('web', 'host', ''), cfg.web['host'].value, 'localhost');
    cfg.web['http'].value := def(local.ReadString('web', 'http', ''), cfg.web['http'].value, '80');
    cfg.web['https'].value := def(local.ReadString('web', 'https', ''), cfg.web['https'].value, '');
    cfg.web['rproxy-http'].value := def(local.ReadString('web', 'rproxy-http', ''), cfg.web['rproxy-http'].value, '');
    cfg.web['rproxy-https'].value := def(local.ReadString('web', 'rproxy-https', ''), cfg.web['rproxy-https'].value, '');
    cfg.web['rproxy-cert-header'].value := def(local.ReadString('web', 'rproxy-cert-header', ''), cfg.web['rproxy-cert-header'].value, '');
    cfg.web['rproxy-ssl-value'].value := def(local.ReadString('web', 'rproxy-ssl-value', ''), cfg.web['rproxy-ssl-value'].value, '');
    cfg.web['certname'].value := def(local.ReadString('web', 'certname', ''), cfg.web['certname'].value, '');
    cfg.web['cacertname'].value := def(local.ReadString('web', 'cacertname', ''), cfg.web['cacertname'].value, '');
    cfg.web['certkey'].value := def(local.ReadString('web', 'certkey', ''), cfg.web['certkey'].value, '');
    cfg.web['password'].value := def(local.ReadString('web', 'password', ''), cfg.web['password'].value, '');
    cfg.web['telnet-password'].value := def(local.ReadString('config', 'telnet-pword', NewGuidId), cfg.web['telnet-password'].value, '');
    cfg.web['robots.txt'].value := def(local.ReadString('web', 'robots.txt', ''), cfg.web['robots.txt'].value, '');
    cfg.admin['log-folder'].value := def(local.ReadString('web', 'logFolder', ''), cfg.admin['log-folder'].value, '');
    cfg.admin['email'].value := def(local.ReadString('config', 'email', ''), cfg.admin['email'].value, 'noone@fhir.org');
    cfg.admin['ownername'].value := def(local.ReadString('config', 'user', ''), cfg.admin['ownername'].value, 'Local User');
    cfg.service['max-memory'].value := def(local.ReadString('config', 'max-memory', ''), cfg.service['max-memory'].value, '0');
    cfg.service['cache-time'].value := def(local.ReadString('config', 'cache-time', ''), cfg.service['cache-time'].value, inttostr(DEFAULT_DWELL_TIME_MIN));

    cfg.web['http-max-conn'].value := '50';
    cfg.web['http-cache-time'].value := '1000';
    cfg.web['plain-mode'].value := 'false';
    cfg.web['caching'].value := 'true';
    cfg.service['langfile'].value := partnerFile('lang.dat');
    cfg.service['package-cache'].value := ExtractFilePath(fn);
    cfg.admin['scim-salt'].value := NewGuidId;

    for n in FFiles.Keys do
    begin
      sct := cfg.section['terminologies'].section[PathTitle(n)];
      sct['type'].value := FFiles[n];
      sct['active'].value := 'true';
      if StringArrayExists(['rxnorm', 'ndc', 'unii', 'cpt', 'omop', 'xig'], FFiles[n]) then
      begin
        sct['db-type'].value := 'sqlite';
        if (FFiles[n] = 'cpt') and (local.ValueExists('cpt', 'local-source')) then
          sct['db-file'].value := local.ReadString('cpt', 'local-source', '')
        else if (n.startsWith('file:')) then
          sct['db-file'].value := FilePath([FFolder, extractFileName(n.subString(5))])
        else
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
      if (FEndPoints[n].active) and (FEndPoints[n].Packages.Count > 0) then
      begin
        buildEndPoint(FEndPoints[n]);
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

    ep := FJson.forceObj['endpoints'];
    for n in ep.properties.keys do
    begin
      o := ep.obj[n];
      mode := o.str['mode'];
      if (mode = '') or (mode = FMode) then
      begin
        sct := cfg.section['endpoints'].section[n];
        sct['type'].value := o.str['type'];
        sct['path'].value := o.str['path'];
        sct['active'].value := 'true';
        sct['db-type'].value := o.str['db-type'];
        sct['db-source'].value := o.str['db-file'];
        sct['db-file'].value := fixDbPath(o.str['db-file']);
        sct['db-auto-create'].value := o.str['db-auto-create'];
        if o.has('folder') then
          sct['folder'].value := o.str['folder'].Replace('{local}', FFolder);
      end;
    end;

    cfg.Save;
  finally
    cfg.free;
  end;
end;

procedure TConfigurationBuilder.buildEndPoint(ep : TEndPointInfo);
var
  fn : String;
begin
  fn := FilePath([FFolder, 'endpoint-r'+CODES_FHIR_GENERATED_PUBLICATION[ep.version]+'.db']);
  ep.filename := fn;
  if not FileExists(fn) then
    CreateDatabase(ep.version, fn)
  else if not doUpgrade(ep.version, fn) then
  begin
    Logging.log('Rebuilding database. Closure tables will be lost');
    CreateDatabase(ep.version, fn);
  end;
end;

procedure TConfigurationBuilder.setupEndPoints;
var
  v, vl : String;
  ep : TEndPointInfo;
begin
  if not FParams.get('version', vl) then
    vl := '*';
  if (vl = '*') then
    vl := '2,3,4,5';

  for v in vl.Split([';', ',']) do
  begin
    ep := FEndPoints['r'+v];
    if (ep = nil) then
      raise EFslException.Create('Version "'+v+'" is unknown')
    else
      ep.active := true;
  end;
end;

procedure TConfigurationBuilder.DownloadFileList(files : TJsonObject);
var
  fn : String;
begin
  for fn in files.properties.Keys do
  begin
    FFiles.Add(fn, (files.node[fn] as TJsonString).value);
    downloadFile(fn);
  end;
end;

procedure TConfigurationBuilder.DownloadFiles;
var
  content, realm, files : TJsonObject;
  r, i : String;
begin
//  Logging.log('Realm: uv');
  content := FJson.forceObj['content'];
  realm := content.forceObj['uv'];
  SeePackages(realm);
  files := realm.forceObj['files'];
  DownloadFileList(files);

  if not FParams.get('realm', r) then
    r := '*';

  if (r = '*') then
  begin
    for i in content.properties.Keys do
      if i <> 'uv' then
      begin
//        Logging.log('Realm: '+i);
        realm := content.forceObj[i];
        SeePackages(realm);
        files := realm.forceObj['files'];
        DownloadFileList(files);
      end;
  end
  else
  begin
    for i in r.split([';', ',']) do
    begin
//      Logging.log('Realm: '+i);
      realm := content.forceObj[i];
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
  f : TFileStream;
  src : String;
begin
  src := URLPath([FUrl, 'config.json']);
  Logging.log('Read Zero Config from '+src);
  if (src.StartsWith('file:')) then
    FJson := TJSONParser.ParseFile(src.Substring(5))
  else
    FJson := TInternetFetcher.fetchJson(src+'?timestamp='+TFslDateTime.makeUTC.toHL7);
  f := TFileStream.Create(FilePath([FFolder, 'config.json']), fmCreate);
  try
    TJSONWriter.writeObject(f, FJson, true);
  finally
    f.free;
  end;
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
begin
  if (fn.StartsWith('file:')) then
  begin
    src := fn;
    tgt := FilePath([FFolder, extractFileName(fn)]);
  end
  else
  begin
    if (isAbsoluteUrl(fn)) then
    begin
      src := fn;
      tgt := FilePath([FFolder, ExtractFileName(fn)]);
    end
    else
    begin
      src := UrlPath([FUrl, fn]);
      tgt := FilePath([FFolder, fn]);
    end;
  end;
  downloadFile(src, tgt);
end;

procedure TConfigurationBuilder.downloadFile(src, tgt : String);
var
  fetcher : TInternetFetcher;
  start : TDateTime;
begin
  if (src.StartsWith('file:')) then
  begin
    if not (FileExists(tgt)) then
    begin
      Logging.start('Copy '+src);
      BytesToFile(FileToBytes(src.Substring(5)), tgt);
      Logging.finish(' Done');
    end;
//    else
//      Logging.log(fn+' already copied')
  end
  else
  begin
    FLastPct := 0;
    if not FileExists(tgt) then
    begin
      Logging.start('Download '+src);
      try
        start := now;
        fetcher := TInternetFetcher.Create;
        try
          fetcher.OnProgress := DownloadProgress;
          fetcher.URL := src;
          fetcher.Fetch;
          fetcher.Buffer.SaveToFileName(tgt);
          Logging.finish(' Done ('+DescribeBytes(fetcher.buffer.size)+', '+DescribePeriod(now - start)+')');
        finally
          fetcher.free;
        end;
      except
        on e : Exception do
        begin
          Logging.finish(' '+e.Message);
          raise;
        end;
      end;
      //else
      //  Logging.log(fn+' already downloaded')
    end;
  end;
end;

function loadRemoteConfig(params : TCommandLineParameters; src : String; local : TIniFile) : String;
var
  cb : TConfigurationBuilder;
  dir : String;
begin
  SetThreadStatus('loadRemoteConfig');
  dir := local.ReadString('config', 'local', UserFolder);

  result := FilePath([dir, 'fhir-server', 'fhir-server-config.cfg']);
  try
    cb := TConfigurationBuilder.Create(params.link);
    try
      cb.FMode := local.ReadString('config', 'mode', '');
      cb.FUrl := src;
      cb.FFolder := ExtractFilePath(result);
      if not FolderExists(cb.FFolder) then
        ForceDirectories(cb.FFolder);
      cb.readConfig;
      Logging.log('Local Config in '+cb.FFolder);
      cb.DownloadFiles;
      cb.setupEndPoints;
      cb.buildConfig(result, local);
    finally
      cb.free;
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
  FPackages.free;
  inherited;
end;

end.
