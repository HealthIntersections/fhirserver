unit endpoint_packages;

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
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  fsl_base, fsl_utilities, fsl_threads, fsl_logging, fsl_json, fsl_http, fsl_npm, fsl_stream, fsl_versions, fsl_i18n,
  fdb_manager,
  fhir_objects,
  package_spider,

  server_config, utilities,
  database_installer, telnet_server,
  tx_manager,  kernel_thread, server_stats,
  web_event, web_base, endpoint, session;

type
  TPackageServerEndPoint = class;

  TPackageUpdaterThread = class(TFslThread)
  private
    FDB : TFDBManager;
    FEndPoint : TPackageServerEndPoint;
    FNextRun : TDateTime;
    FLastEmail : TDateTime;
    FZulip : TZulipTracker;
    procedure RunUpdater;
    procedure doSendEmail(dest, subj, body : String);
  protected
    function ThreadName : String; override;
    procedure Initialise; override;
    procedure Execute; override;
  public
    constructor Create(db : TFDBManager; endPoint : TPackageServerEndPoint);
    destructor Destroy; override;
  end;

  TMatchTableSort = (mtsNull, mtsId, mtsVersion, mtsDate, mtsFhirVersion, mtsCanonical, mtsDownloads, mtsKind);

  { TFHIRPackageWebServer }

  TFHIRPackageWebServer = class (TFhirWebServerEndpoint)
  private
    FDB : TFDBManager;
    FLastUpdate : TDateTime;
    FNextScan : TDateTIme;
    FScanning: boolean;
    FSystemToken : String;
    FCrawlerLog : TJsonObject;
    FBucketFolder, FBucketPath : String;

    procedure setDB(value : TFDBManager);
    function status : String;

    function getVersion(v : String) : String;
    function interpretVersion(v : String) : String;

    function genTable(url : String; list: TFslList<TJsonObject>; sort : TMatchTableSort; rev, inSearch, secure, packageLevel, versioned: boolean): String;

    function serveCreatePackage(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo) : String;

    procedure servePage(fn : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; secure : boolean);
    procedure serveDownload(secure : boolean; id, version : String; response : TIdHTTPResponseInfo);
    procedure serveVersions(id, sort : String; secure : boolean; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveSearch(name, dependson, canonicalPkg, canonicalUrl, FHIRVersion, dependency, sort : String; secure, objWrapper : boolean; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveUpdates(date : TFslDateTime; secure : boolean; response : TIdHTTPResponseInfo);
    procedure serveProtectForm(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; id : String);
    procedure serveLog(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveBroken(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; filter : String);
    procedure serveUpload(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; secure : boolean; id : String);
    procedure processProtectForm(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; id, pword : String);
    procedure SetScanning(const Value: boolean);

    function doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;
  public
    constructor Create(code, path : String; common : TFHIRWebServerCommon); override;
    destructor Destroy; override;
    function link  : TFHIRPackageWebServer; overload;
    function description : String; override;

    property DB : TFDBManager read FDB write SetDB;
    property NextScan : TDateTIme read FNextScan write FNextScan;
    property scanning : boolean read FScanning write SetScanning;
    property SystemToken : String read FSystemToken write FSystemToken;

    function PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TFslTimeTracker) : String; override;
    function SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TFslTimeTracker) : String; override;
    function logId : string; override;
  end;

  { TPackageServerEndPoint }

  TPackageServerEndPoint = class (TFHIRServerEndPoint)
  private
    FPackageServer : TFHIRPackageWebServer;
    FUpdater : TPackageUpdaterThread;
    FSystemToken, FBucketFolder, FBucketPath : String;
    procedure dumpVersions;
    procedure upgradeDatabase;         
    procedure generateHashes(conn : TFDBConnection);
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; i18n : TI18nSupport);
    destructor Destroy; override;

    property SystemToken : String read FSystemToken write FSystemToken;
    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
    procedure InstallDatabase(params : TCommandLineParameters); override;
    procedure UninstallDatabase; override;
    procedure LoadPackages(installer : boolean; plist : String); override;
    procedure updateAdminPassword(pw : String); override;
    procedure Load; override;
    Procedure Unload; override;
    procedure internalThread(callback : TFhirServerMaintenanceThreadTaskCallBack); override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure clearCache; override; 
    procedure SweepCaches; override;
    procedure SetCacheStatus(status : boolean); override;
    procedure getCacheInfo(ci: TCacheInformation); override;
    procedure recordStats(rec : TStatusRecord); override;
  end;

implementation

function TPackageServerEndPoint.cacheSize(magic : integer): UInt64;
begin
  result := inherited cacheSize(magic);
end;

procedure TPackageServerEndPoint.clearCache;
begin
  inherited;
end;

procedure TPackageServerEndPoint.SweepCaches;
begin
  inherited SweepCaches;
end;

constructor TPackageServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; i18n : TI18nSupport);
begin
  inherited Create(config, settings, db, common, nil, i18n);
  upgradeDatabase;
  FSystemToken := settings.Ini.service.prop['system-token'].value;
  FBucketFolder := config['bucket-folder'].value;
  FBucketPath := config['bucket-path'].value;
  if (FBucketFolder <> '') then
    dumpVersions;
end;

destructor TPackageServerEndPoint.Destroy;
begin
  FPackageServer.free;

  inherited;
end;

procedure TPackageServerEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited;

end;

procedure TPackageServerEndPoint.recordStats(rec: TStatusRecord);
begin
  inherited recordStats(rec);
  // nothing
end;

procedure TPackageServerEndPoint.Load;
begin
  FUpdater := TPackageUpdaterThread.Create(Database.Link, self);
end;

procedure TPackageServerEndPoint.Unload;
begin
  FUpdater.StopAndWait(50);
  FUpdater.free;
  FUpdater := nil;
end;

procedure TPackageServerEndPoint.InstallDatabase(params: TCommandLineParameters
  );
var
  dbi : TFHIRDatabaseInstaller;
  conn : TFDBConnection;
begin
  conn := Database.GetConnection('install');
  try
    dbi := TFHIRDatabaseInstaller.Create(conn, nil, nil);
    try
      dbi.installPackageServer;
    finally
      dbi.free;
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

procedure TPackageServerEndPoint.UninstallDatabase;
var
  dbi : TFHIRDatabaseInstaller;
  conn : TFDBConnection;
begin
  conn := Database.GetConnection('uninstall');
  try
    dbi := TFHIRDatabaseInstaller.Create(conn, nil, nil);
    try
      dbi.Uninstall;
    finally
      dbi.free;
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

procedure TPackageServerEndPoint.internalThread(callback: TFhirServerMaintenanceThreadTaskCallBack);
begin
  // nothing, for now
  // todo: health check on spider
end;

procedure TPackageServerEndPoint.LoadPackages(installer : boolean; plist: String);
begin
  raise EFslException.Create('This is not applicable to this endpoint');
end;

function TPackageServerEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  inherited makeWebEndPoint(common);
  FPackageServer := TFHIRPackageWebServer.Create(config.name, config['path'].value, common);
  FPackageServer.DB := Database.Link;
  FPackageServer.NextScan := FUpdater.FNextRun;
  FPackageServer.SystemToken := FSystemToken;
  FPackageServer.FBucketPath := FBucketPath;
  FPackageServer.FBucketFolder := FBucketFolder;
  WebEndPoint := FPackageServer;
  FUpdater.Start;
  result := FPackageServer.link;
end;

procedure TPackageServerEndPoint.SetCacheStatus(status: boolean);
begin
  inherited;
end;

function TPackageServerEndPoint.summary: String;
begin
  result := 'Package Server using '+describeDatabase(Config);
end;

procedure TPackageServerEndPoint.updateAdminPassword(pw: String);
begin
  raise EFslException.Create('This is not applicable to this endpoint');
end;

procedure TPackageServerEndPoint.dumpVersions;
var
  c : TFDBConnection;
  fn : String;
begin
  c := Database.GetConnection('Populate Bucket');
  try
    c.SQL := 'select Id, Version, Content from PackageVersions';
    c.prepare;
    c.execute;
    while c.FetchNext do
    begin
      fn := FilePath([FBucketFolder, c.ColStringByName['Id']+'-'+c.ColStringByName['Version']+'.tgz']);
      if not FileExists(fn) then
        BytesToFile(c.ColBlobByName['Content'], fn);
    end;
    c.terminate;
    c.Release;
  except
    on e: Exception do
      c.Error(e);
  end;
end;

procedure TPackageServerEndPoint.upgradeDatabase;
var
  c : TFDBConnection;
  m : TFDBMetaData;
  t : TFDBTable;
  inst : TFHIRDatabaseInstaller;
begin
  c := Database.GetConnection('Version check');
  try
    m := c.FetchMetaData;
    try
      if not (m.HasTable('Packages')) then
      begin
        inst := TFHIRDatabaseInstaller.Create(c, nil, nil);
        try
          inst.installPackageServer;
        finally
          inst.free;
        end;
      end
      else
      begin
        if m.HasTable('Packages') then
        begin
          t := m.GetTable('Packages');
          if not t.hasColumn('Security') then
            c.ExecSQL('ALTER TABLE Packages ADD Security int NULL');
        end;
        if m.HasTable('PackageVersions') then
        begin
          t := m.GetTable('PackageVersions');
          if not t.hasColumn('UploadCount') then
          begin
            c.ExecSQL('ALTER TABLE PackageVersions ADD UploadCount int NULL');
            c.ExecSQL('Update PackageVersions set UploadCount = 1');
          end;
          if not t.hasColumn('Hash') then
          begin
            c.ExecSQL('ALTER TABLE PackageVersions ADD Hash nchar(128) NULL');
          end;
          if not t.hasColumn('Author') then
          begin
            c.ExecSQL('ALTER TABLE PackageVersions ADD HomePage nchar(128) NULL');
            c.ExecSQL('ALTER TABLE PackageVersions ADD Author nchar(128) NULL');
            c.ExecSQL('ALTER TABLE PackageVersions ADD License nchar(128) NULL');
          end;
        end;
        if not m.HasTable('PackageURLs') then
        begin
          c.ExecSQL('CREATE TABLE PackageURLs(PackageVersionKey int NOT NULL,	URL nchar(128) NOT NULL)');
          c.ExecSQL('Create INDEX SK_PackageURLs ON PackageURLs(PackageVersionKey)');
          c.ExecSQL('Delete from PackageDependencies');
          c.ExecSQL('Delete from PackageFHIRVersions');
          c.ExecSQL('Delete from PackagePermissions');
          c.ExecSQL('Delete from PackageVersions');
          c.ExecSQL('Delete from PackageDependencies');
        end;
      end;
    finally
      m.free;
    end;
    generateHashes(c);
    c.Release;
  except
    on e: Exception do
      c.Error(e);
  end;
end;

procedure TPackageServerEndPoint.generateHashes(conn : TFDBConnection);
var
  hashes : TFslStringDictionary;
  s : String;
begin
  hashes := TFslStringDictionary.Create;
  try
    conn.SQL := 'select PackageVersionKey, Content from PackageVersions where Hash is NULL';
    conn.prepare;
    conn.execute;
    while conn.FetchNext do
      hashes.AddOrSetValue(conn.ColStringByName['PackageVersionKey'], genHash(conn.ColBlobByName['Content']));
    conn.Terminate;

    for s in hashes.keys do
      conn.ExecSQL('update PackageVersions set Hash = '''+hashes[s]+''' where PackageVersionKey = '+s);
  finally
    hashes.free;
  end;
end;

{ TPackageUpdaterThread }

constructor TPackageUpdaterThread.Create(db: TFDBManager; endPoint : TPackageServerEndPoint);
begin
  inherited Create;
  FDB := db;
  FEndPoint := endPoint;
  FNextRun := now + 1/(24 * 60);
  FZulip := TZulipTracker.Create('https://fhir.zulipchat.com/api/v1/messages',
      'pascal-github-bot@chat.fhir.org', FEndPoint.Settings.ZulipPassword);
end;

destructor TPackageUpdaterThread.Destroy;
begin
  FDB.free;
  FZulip.free;
  inherited;
end;

procedure TPackageUpdaterThread.Execute;
begin
  FEndPoint.FPackageServer.scanning := true;
  try
    RunUpdater;
  finally
    FEndPoint.FPackageServer.scanning := false;
  end;
  FEndPoint.FPackageServer.NextScan := now + 1/24;
end;

procedure TPackageUpdaterThread.Initialise;
begin
  TimePeriod := 60 * 60 * 1000;
end;

procedure TPackageUpdaterThread.doSendEmail(dest, subj, body : String);
begin
  sendEmail(FEndPoint.Settings, dest, subj, body);
end;


procedure TPackageUpdaterThread.RunUpdater;
var
  conn : TFDBConnection;
  upd : TPackageUpdater;
begin
  conn := FDB.getConnection('server.packages.update');
  try
    upd := TPackageUpdater.Create(FZulip.link, FEndPoint.FBucketFolder);
    try
      upd.OnSendEmail := doSendEmail;
      try
        upd.update(conn);
        if (TFslDateTime.makeToday.DateTime <> FLastEmail) then
        begin
          if upd.errors <> '' then
            sendEmail(FEndPoint.Settings, 'grahameg@gmail.com', 'Package Feed Errors', upd.errors);
          FLastEmail := TFslDateTime.makeToday.DateTime;
        end;
      except
        on e : exception do
        begin
          Logging.log('Exception updating packages: '+e.Message);
        end;
      end;
      FEndPoint.FPackageServer.FCrawlerLog := upd.CrawlerLog.Link;
    finally
      upd.free;
    end;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

function TPackageUpdaterThread.threadName: String;
begin
  result := 'Package Updater';
end;

function readSort(sort : String) : TMatchTableSort;
begin
  if sort.StartsWith('-') then
    sort := sort.Substring(1);
  if (SameText('id', sort)) then
    result := mtsId
  else if (SameText('version', sort)) then
    result := mtsVersion
  else if (SameText('date', sort)) then
    result := mtsDate
  else if (SameText('fhirversion', sort)) then
    result := mtsFhirVersion
  else if (SameText('canonical', sort)) then
    result := mtsCanonical
  else if (SameText('downloads', sort)) then
    result := mtsDownloads
  else if (SameText('kind', sort)) then
    result := mtsKind
  else
    result := mtsNull;
end;


function TFHIRPackageWebServer.description: String;
begin
  result := 'Package Server - browser packages, or use the <a href="https://simplifier.net/docs/package-server/home">Package Server API</a>';
end;

destructor TFHIRPackageWebServer.Destroy;
begin
  FDB.free;
  inherited;
end;

procedure TFHIRPackageWebServer.setDB(value : TFDBManager);
begin
  FDB := nil;
  FDB := value;
end;

procedure TFHIRPackageWebServer.SetScanning(const Value: boolean);
begin
  FScanning := Value;
  FLastUpdate := now;
end;

function TFHIRPackageWebServer.status: String;
begin
  if FScanning then
    result := 'Scanning for updates now'
  else if FlastUpdate = 0 then
    result := 'First Scan in '+DescribePeriod(FNextScan-now)
  else
    result := 'Next Scan in '+DescribePeriod(FNextScan-now)+'. Last scan was '+DescribePeriod(now - FLastUpdate)+' ago';
end;

function genSort(this, sort : TMatchTableSort; rev : boolean) : String;
begin
  case this of
    mtsId : result := 'id';
    mtsVersion : result := 'version';
    mtsDate : result := 'date';
    mtsFhirVersion : result := 'fhirversion';
    mtsCanonical : result := 'canonical';
    mtsDownloads : result := 'downloads';
    mtsKind : result := 'kind';
  end;
  if (this = sort) and not rev then
    result := '-'+ result;
end;

type
  TFHIRPackageWebServerSorter = class (TFslComparer<TJsonObject>)
  private
    sort : TMatchTableSort;
    factor : integer;
  public
    constructor Create(sort : TMatchTableSort; factor : integer);
    function Compare(const l, r : TJsonObject) : integer; override;
  end;

constructor TFHIRPackageWebServerSorter.Create(sort: TMatchTableSort; factor: integer);
begin
  inherited Create;
  self.sort := sort;
  self.factor := factor;
end;

function TFHIRPackageWebServerSorter.Compare(const l, r : TJsonObject) : integer;
begin
  case sort of
    mtsId : result := CompareText(l['name'], r['name']) * factor;
    mtsVersion : result := CompareText(l['version'], r['version']) * factor;
    mtsDate : result := CompareText(l['date'], r['date']) * factor;
    mtsFhirVersion : result := CompareText(l['fhirVersion'], r['fhirVersion']) * factor;
    mtsCanonical : result := CompareText(l['canonical'], r['canonical']) * factor;
    mtsDownloads : result := (l.int['count'] - r.int['count']) * factor;
    mtsKind : result := CompareText(l['kind'], r['kind']) * factor;
  else
    result := 0;
  end;
end;

function TFHIRPackageWebServer.genTable(url : String; list: TFslList<TJsonObject>; sort : TMatchTableSort; rev, inSearch, secure, packageLevel, versioned: boolean): String;
var
  b : TFslStringBuilder;
  i : TJsonObject;
  ss, v : String;
begin
  if inSearch then
    ss := '&sort='
  else
    ss := '?sort=';
  if rev then
    list.sort(TFHIRPackageWebServerSorter.Create(sort, -1))
  else
    list.sort(TFHIRPackageWebServerSorter.Create(sort, 1));
  b := TFslStringBuilder.Create;
  try
    if versioned then
      v := 'Version Specific Mode'
    else
      v := 'All Versions Mode';

    if list.count = 1 then   
      b.Append('<p>1 match <i>('+v+')</i></p>'#13#10)
    else
      b.Append('<p>'+inttostr(list.count)+' matches <i>('+v+')</i></p>'#13#10);
    b.Append('<table class="grid pck-matches">'#13#10);
    b.Append('<tr class="pck-header">'#13#10);
    if (inSearch) then
      b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsId, sort, rev)+'">id</a></td>'#13#10);
    b.Append('  <td class="pck-col" style="white-space:nowrap; width:100px"><a href="'+url+ss+genSort(mtsVersion, sort, rev)+'">version</a></td>'#13#10);
    b.Append('  <td class="pck-col" style="white-space:nowrap; width:100px"><a href="'+url+ss+genSort(mtsDate, sort, rev)+'">date</a></td>'#13#10);
    b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsFhirVersion, sort, rev)+'">FHIR Version</a></td>'#13#10);
    if (inSearch) then
      b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsCanonical, sort, rev)+'">Canonical</a></td>'#13#10);
    b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsKind, sort, rev)+'">Kind</a></td>'#13#10);
    if (not versioned) then
      b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsDownloads, sort, rev)+'"># Downloads</a></td>'#13#10);
    b.Append('</tr">'#13#10);
    for i in list do
    begin
      b.Append('<tr class="pck-match">'#13#10);
      if (inSearch) then
        b.Append('  <td class="pck-cell"><a href="'+i['url']+'" title="'+FormatTextToHTML(i['description'])+'">'+i['name']+'</a></td>'#13#10);
      if (inSearch) then
        if versioned then
          b.Append('  <td class="pck-cell" style="white-space:nowrap; width:100px">'+i['version']+'</td>'#13#10)
        else
          b.Append('  <td class="pck-cell" style="white-space:nowrap; width:100px">'+i['version']+' (<a href="'+URLPath([AbsoluteURL(secure), i['name']])+'">all</a>)</td>'#13#10)
      else
        b.Append('  <td class="pck-cell" style="white-space:nowrap; width:100px"><a href="'+i['url']+'" title="'+FormatTextToHTML(i['description'])+'">'+i['version']+'</a></td>'#13#10);
      b.Append('  <td class="pck-cell" style="white-space:nowrap; width:100px">'+i['date'].Substring(0, 10)+'</td>'#13#10);
      b.Append('  <td class="pck-cell">'+i['fhirVersion']+'</td>'#13#10);
      if (inSearch) then
        b.Append('  <td class="pck-cell"><a href="'+i['canonical']+'">'+i['canonical']+'</a></td>'#13#10);
      b.Append('  <td class="pck-cell">'+i['kind']+'</td>'#13#10);
      if (not versioned) then
        b.Append('  <td class="pck-cell">'+i['count']+'</td>'#13#10);
      b.Append('</tr">'#13#10);
    end;
    b.Append('</table>'#13#10);
    result := b.ToString;
  finally
    b.free;
  end;
end;

function TFHIRPackageWebServer.getVersion(v : String) : String;
begin
  if v.StartsWith('5.0') then
    result := '4.0'
  else if v.StartsWith('4.0') then
    result := '5.0'
  else if v.StartsWith('3.0') then
    result := '3.0'
  else if v.StartsWith('1.0') then
    result := '1.0'
  else if SameText(v, 'r5') then
    result := '5.0'
  else if SameText(v, 'r4') then
    result := '4.0'
  else if SameText(v, 'r3') then
    result := '3.0'
  else if SameText(v, 'r2') then
    result := '1.0'
  else
    result := v;
end;

function TFHIRPackageWebServer.interpretVersion(v: String): String;
  function processVersion(v: String): String;
  begin
    if (v.StartsWith('6.0')) then
      result := 'R6'
    else if (v.StartsWith('5.0')) then
      result := 'R5'
    else if (v.StartsWith('4.3')) then
      result := 'R4B'
    else if (v.StartsWith('4.0')) then
      result := 'R4'
    else if (v.StartsWith('3.0')) then
      result := 'R3'
    else if (v.StartsWith('1.0')) then
      result := 'R2'
    else if (v.StartsWith('1.4')) then
      result := 'R2B'
    else if (v.StartsWith('4')) then
      result := 'R5'
    else if (v.StartsWith('3')) then
      result := 'R4'
    else if (v.StartsWith('1.2')) then
      result := 'R2B'
    else if (v.StartsWith('1')) then
      result := 'R3'
    else
      result := '??V'
  end;
var
  ts : TStringList;
  i : integer;
begin
  ts := TStringList.Create;
  try
    ts.CommaText := v;
    for i := 0 to ts.Count - 1 do
      ts[i] := processVersion(ts[i]);
    result := ts.CommaText;
  finally
    ts.free;
  end;
end;

function TFHIRPackageWebServer.link: TFHIRPackageWebServer;
begin
  result := TFHIRPackageWebServer(inherited link);
end;

function TFHIRPackageWebServer.logId: string;
begin
  result := 'PK';
end;

procedure TFHIRPackageWebServer.serveDownload(secure : boolean; id, version : String; response : TIdHTTPResponseInfo);
var
  conn : TFDBConnection;
  procedure sendRow;
  var
    pvk : integer;
  begin
    if FBucketPath <> '' then
    begin
      if (secure) then
        response.Redirect(FBucketPath.replace('http:', 'https:')+'/'+id+'-'+version+'.tgz')
      else
        response.Redirect(FBucketPath+'/'+id+'-'+version+'.tgz');
    end
    else
    begin
      response.ResponseNo := 200;
      response.ResponseText := 'OK';
      response.ContentType := 'application/tar+gzip';
      response.ContentStream := TBytesStream.Create(conn.GetColBlobByName('Content'));
      response.ContentDisposition := 'attachment; filename="'+id+'#'+version+'.tgz"';
      response.FreeContentStream := true;
      pvk := conn.GetColIntegerByName('PackageVersionKey');
      conn.Terminate;
      conn.SQL := 'Update PackageVersions set DownloadCount = DownloadCount + 1 where PackageVersionKey = '+inttostr(pvk);
      conn.Prepare;
      conn.Execute;
      conn.Terminate;
      conn.SQL := 'Update Packages set DownloadCount = DownloadCount + 1 where Id = '''+SQLWrapString(id)+'''';
      conn.Prepare;
      conn.Execute;
    end;
  end;
begin
  conn := FDB.getConnection('Package.server.download');
  try
    conn.SQL := 'Select PackageVersionKey, Content from PackageVersions where Id = '''+SQLWrapString(id)+''' and Version = '''+SQLWrapString(version)+'''';
    conn.Prepare;
    conn.Execute;
    if conn.FetchNext then
      sendRow
    else
    begin
      conn.Terminate;
      conn.SQL := 'Select PackageVersionKey, Content from PackageVersions where Id = '''+SQLWrapString(id)+''' and Version like '''+SQLWrapString(version)+'-%'' order by PubDate desc';
      conn.Prepare;
      conn.Execute;
      if conn.FetchNext then
        sendRow
      else
      begin
        response.ResponseNo := 404;
        response.ResponseText := 'Not found';
        response.ContentType := 'text/plain';
        response.ContentText := 'The package "'+id+'#'+version+'" is not known by this server';
      end;
    end;
    conn.Terminate;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;


procedure TFHIRPackageWebServer.servePage(fn : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; secure : boolean);
var
  vars : TFslMap<TFHIRObject>;
begin
  vars := TFslMap<TFHIRObject>.Create('vars');
  try
    vars.add('count', TFHIRObjectText.Create(FDB.CountSQL('Select count(*) from PackageVersions', 'Package.server.home')));
    vars.add('downloads', TFHIRObjectText.Create(FDB.CountSQL('Select sum(DownloadCount) from Packages', 'Package.server.home')));
    vars.add('prefix', TFHIRObjectText.Create(AbsoluteURL(secure)));
    vars.add('ver', TFHIRObjectText.Create('4.0.1'));
    vars.add('status', TFHIRObjectText.Create(status));
    returnFile(request, response, nil, request.Document, fn, false, vars);
  finally
    vars.free;
  end;
end;

procedure TFHIRPackageWebServer.serveProtectForm(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String);
var
  conn : TFDBConnection;
  vars : TFslMap<TFHIRObject>;
begin
  conn := FDB.getConnection('Package.server.protect');
  try
    conn.sql := 'Select ManualToken from Packages where Id = '''+sqlWrapString(id)+'''';
    conn.prepare;
    conn.Execute;
    if conn.ColStringByName['ManualToken'] <> '' then
    begin
      response.ResponseNo := 409;
      response.ContentText := 'This package is already secured';
    end
    else
    begin
      conn.terminate;
      response.ResponseNo := 200;
      response.ResponseText := 'OK';
      vars := TFslMap<TFHIRObject>.Create;
      try
        vars.add('prefix', TFHIRObjectText.Create(AbsoluteUrl(false)));
        vars.add('ver', TFHIRObjectText.Create('4.0.1'));
        vars.add('pid', TFHIRObjectText.Create(id));
        returnFile(request, response, nil, request.Document, 'packages-protect.html', false, vars);
      finally
        vars.free;
      end;
    end;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

function colorForStatus(s : String) : String;
begin
  if s = 'error' then
    result := 'maroon'
  else if s = 'warning' then
    result := 'navy'
  else
    result := 'black'
end;

procedure TFHIRPackageWebServer.serveLog(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  vars : TFslMap<TFHIRObject>;
  html : TFslStringBuilder;
  feed, item : TJsonObject;
  msgs : TJsonArray;
  allOK : boolean;
  i : integer;
begin
  response.ResponseNo := 200;
  response.ResponseText := 'OK';
  
  if (request.Accept.contains('/html')) then
  begin
    html := TFslStringBuilder.create;
    try
      if FCrawlerLog.has('status') then
        html.append('<p>The Crawler has not yet completed processing the feeds</p>'#13#10)
      else
      begin
        html.append('<p>Feeds from '+FormatTextToHTML(FCrawlerLog['master'])+' ('+FormatTextToHTML(FCrawlerLog['run-time'])+')</p>'#13#10);
        for feed in FCrawlerLog.arr['feeds'].asObjects do
        begin
          html.append('<p><b>'+FormatTextToHTML(feed['url'])+'</b> ('+FormatTextToHTML(feed['fetch-time'])+')</p>'#13#10);
          html.append('<ul>'#13#10);
          allOk := true;
          for item in feed.arr['items'].asObjects do
          begin
            if item['status'] = 'Already Processed' then
            begin
              // nothing
            end
            else
            begin
              html.append('<li style="color: Black">');
              if (item.has('id')) then
                html.append('<span title="'+FormatTextToHTML(item['guid'])+'">'+FormatTextToHTML(item['id'])+'</span>: ')
              else
                html.append(item['guid']+': ');

              if item['status'] = 'Fetched' then
                html.append('<span style="color: DarkGreen>')
              else
              begin              
                allOK := false;
                html.append('<span style="color: Maroon">');
              end;
              html.append(item['status']+'</span>');
              if (item.has('messages')) then
              begin
                msgs := item.arr['messages'];
                if (msgs.Count = 1) then
                  html.append('. <span style="color: '+colorForStatus(msgs.Obj[0]['type'])+'">'+FormatTextToHTML(msgs.Obj[0]['message'])+'</span>')
                else
                begin
                  html.append('<ul>');
                  for i := 0 to msgs.Count - 1 do
                    html.append('<li style="color: '+colorForStatus(msgs.Obj[0]['type'])+'">'+FormatTextToHTML(msgs.Obj[0]['message'])+'</li>');
                  html.append('</ul>'#13#10);
                end
              end;
              html.append('</li>'#13#10);
            end;
          end;
          if (allOK) then
            html.append('<li style="color: Black">All OK</li>'#13#10);
          html.append('</ul>'#13#10);
        end;
      end;
      vars := TFslMap<TFHIRObject>.Create;
      try
        vars.add('prefix', TFHIRObjectText.Create(AbsoluteUrl(false)));
        vars.add('ver', TFHIRObjectText.Create('4.0.1'));
        vars.add('log', TFHIRObjectText.Create(html.ToString));
        returnFile(request, response, nil, request.Document, 'packages-log.html', false, vars);
      finally
        vars.free;
      end
    finally
      html.free;
    end;
  end
  else
  begin
    response.ContentType := 'application/json';
    response.ContentText := TJsonWriterDirect.writeObjectStr(FCrawlerLog, true);
  end;
end;

function codeForKind(kind : integer): String;
begin
  case TFHIRPackageKind(kind) of
    fpkNull : result := '';
    fpkCore : result := 'Core';
    fpkIG : result := 'IG';
    fpkIGTemplate : result := 'Template';
    fpkTool : result := 'Tool';
    fpkToolGen : result := 'Tool/Gen';
    fpkGroup : result := 'Group';
    fpkExamples : result := 'Examples';
  else
    result := '??kind ('+inttostr(kind)+')';
  end;
end;

procedure TFHIRPackageWebServer.serveVersions(id, sort : String; secure : boolean; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
var
  conn : TFDBConnection;
  json, v, dist: TJsonObject;
  src, keys, n, vs : String;
  vars : TFslMap<TFHIRObject>;
  vers : TFslMap<TJsonObject>;
  list : TFslList<TJsonObject>;
begin
  conn := FDB.getConnection('Package.server.versions');
  try
    conn.sql := 'Select PackageVersionKey, Version, PubDate, FhirVersions, Canonical, DownloadCount, Kind, HomePage, Author, License, Hash, Description from PackageVersions where Id = '''+sqlWrapString(id)+''' order by PubDate asc';
    conn.prepare;
    conn.Execute;
    list := TFslList<TJsonObject>.Create;
    try
      vers := TFslMap<TJsonObject>.Create('vers');
      vars := TFslMap<TFHIRObject>.Create('vars');
      try
        keys := '';
        json := TJsonObject.Create;
        try
          json['_id'] := id;
          json['name'] := id;
          while conn.FetchNext do
          begin
            json.forceObj['dist-tags']['latest'] := conn.ColStringByName['Version'];
            v := json.forceObj['versions'].forceObj[conn.ColStringByName['Version']];
            list.Add(v.link);
            vers.add(conn.ColStringByName['PackageVersionKey'], v.link);
            CommaAdd(keys, conn.ColStringByName['PackageVersionKey']);
            v['name'] := id;
            v['_id'] := id+'@'+interpretVersion(conn.ColStringByName['FhirVersions']);
            v['date'] := conn.ColDateTimeExByName['PubDate'].toXML;
            v['version'] := conn.ColStringByName['Version'];
            v['fhirVersion'] := interpretVersion(conn.ColStringByName['FhirVersions']);
            v['kind'] := codeForKind(conn.ColIntegerByName['Kind']);
            v['count'] := conn.ColStringByName['DownloadCount'];
            v['canonical'] := conn.ColStringByName['Canonical'];
            if (conn.ColStringByName['HomePage'] <> '') then
              v['homepage'] := conn.ColStringByName['HomePage'];
            if (conn.ColStringByName['License'] <> '') then
              v['license'] := conn.ColStringByName['License'];
            if (conn.ColStringByName['Author'] <> '') then
              v.forceObj['author']['name'] := conn.ColStringByName['Author'];
            if not conn.ColNullByName['Description'] then
            begin
              json['description'] := conn.ColBlobAsStringByName['Description'];
              v['description'] := conn.ColBlobAsStringByName['Description'];
            end;
            v['url'] := URLPath([AbsoluteUrl(secure), id, conn.ColStringByName['Version']]);
            dist := v.forceObj['dist'];
            dist['shasum'] := conn.ColStringByName['Hash'];
            if (FBucketPath <> '') then
            begin
              if (secure) then
                dist['tarball'] := FBucketPath.replace('http:', 'https:')+'/'+id+'-'+conn.ColStringByName['Version']+'.tgz'
              else
                dist['tarball'] := FBucketPath+'/'+id+'-'+conn.ColStringByName['Version']+'.tgz';
            end
            else if (secure) then
              dist['tarball'] := 'https://'+request.Host+'/'+id+'/'+conn.ColStringByName['Version']
            else
              dist['tarball'] := 'http://'+request.Host+'/'+id+'/'+conn.ColStringByName['Version'];
          end;
          conn.terminate;
          conn.sql := 'Select PackageVersionKey, Dependency from PackageDependencies where PackageVersionKey in ('+keys+')';
          conn.prepare;
          conn.Execute;
          while conn.FetchNext do
          begin
            n := conn.ColStringByName['Dependency'];
            vs := n.substring(n.indexOf('#')+1);
            n := n.substring(0, n.indexOf('#'));
            vers[conn.ColStringByName['PackageVersionKey']].forceObj['dependencies'][n] := vs;
          end;

          vars.add('name', TFHIRObjectText.Create(json['name']));
          vars.add('desc', TFHIRObjectText.Create(FormatTextToHTML(json['description'])));
          src := TJsonWriterDirect.writeObjectStr(json, true);
        finally
          json.free;
        end;
        conn.terminate;
        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        if (request.Accept.contains('/html')) then
        begin
          vars.add('prefix', TFHIRObjectText.Create(AbsoluteUrl(false)));
          vars.add('ver', TFHIRObjectText.Create('4.0.1'));
          vars.add('matches', TFHIRObjectText.Create(genTable(AbsoluteUrl(secure)+'/'+ID, list, readSort(sort), sort.startsWith('-'), false, secure, false, false)));
          vars.add('status', TFHIRObjectText.Create(status));
          vars.add('count', TFHIRObjectText.Create(conn.CountSQL('Select count(*) from PackageVersions where Id = '''+sqlWrapString(id)+'''')));
          vars.add('downloads', TFHIRObjectText.Create(conn.CountSQL('select Sum(DownloadCount) from PackageVersions where Id = '''+sqlWrapString(id)+'''')));
          returnFile(request, response, nil, request.Document, 'packages-versions.html', false, vars);
        end
        else
        begin
          response.ContentType := 'application/json';
          response.ContentText := src;
        end;
      finally
        vars.free;
        vers.free;
      end;
    finally
      list.free;
    end;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

procedure TFHIRPackageWebServer.serveBroken(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; filter : String);
var
  conn : TFDBConnection;
  json, v, dist: TJsonObject;
  src, name, dep, ver : String;
  vars : TFslMap<TFHIRObject>;
  list : TJsonArray;
  html : TFslStringBuilder;
  i : integer;
  ids : TStringList;
begin
  conn := FDB.getConnection('Package.server.broken');
  try
  //  conn.sql := 'select Id || ''#'' || version as Source, Dependency from PackageDependencies, PackageVersions where PackageDependencies.PackageVersionKey =  PackageVersions.PackageVersionKey and Dependency not in (select Id || ''#'' || version from PackageVersions) order by Source';
    json := TJsonObject.Create;
    try
      ids := TStringList.create;
      try
        conn.sql := 'select Id, Version from PackageVersions';
        conn.prepare;
        conn.Execute;
        while conn.FetchNext do
          ids.add(conn.ColStringByName['Id']+'#'+ TSemanticVersion.getMajMin(conn.ColStringByName['Version']));
        conn.terminate;
        ids.sort;
        conn.sql := 'select Id || ''#'' || version as Source, Dependency from PackageDependencies, PackageVersions where PackageDependencies.PackageVersionKey =  PackageVersions.PackageVersionKey';
        conn.prepare;
        conn.Execute;
        while conn.FetchNext do
        begin
          if (filter = '') or (conn.ColStringByName['Source'].contains(filter)) then
          begin
            dep := conn.ColStringByName['Dependency'];
            ver := TSemanticVersion.getMajMin(dep.substring(dep.indexOf('#')+1));
            if ids.IndexOf(dep.substring(0, dep.indexOf('#')+1)+ver) = -1 then
            begin
              list := json.forceArr[conn.ColStringByName['Source']];
              list.add(conn.ColStringByName['Dependency']);
            end;
          end;
        end;
      finally
        ids.free;
      end;

      response.ResponseNo := 200;
      response.ResponseText := 'OK';
      if (request.Accept.contains('/html')) then
      begin
        html := TFslStringBuilder.create;
        try           
          html.Append('<table class="grid">'#13#10);
          html.Append('<tr><td><b>Source Package</b></td><td><b>Broken Dependencies</b></td></tr>'#13#10);
          for name in json.properties.SortedKeys do
          begin
            list := json.arr[name];
            html.Append('<tr><td>'+name+'</td><td>');
            for i := 0 to list.Count - 1 do
            begin
              if i > 0 then
                html.append(', ');
              html.append(list.Value[i]);
            end;
            html.append('</td></tr>'#13#10);
          end;
          html.Append('</table>'#13#10);
          vars := TFslMap<TFHIRObject>.Create('vars');
          try
            vars.add('prefix', TFHIRObjectText.Create(AbsoluteUrl(false)));
            vars.add('ver', TFHIRObjectText.Create('4.0.1'));
            vars.add('filter', TFHIRObjectText.Create(FormatTextToHTML(filter)));
            vars.add('table', TFHIRObjectText.Create(html.ToString));
            vars.add('status', TFHIRObjectText.Create(status));
            returnFile(request, response, nil, request.Document, 'packages-broken.html', false, vars);
          finally
            vars.free;
          end;
        finally
          html.free;
        end;
      end
      else
      begin
        json.str['date'] := FormatDateTime('c', now);
        response.ContentType := 'application/json';
        response.ContentText := TJsonWriterDirect.writeObjectStr(json, true);
      end;
    finally
      json.free;
    end;
    conn.terminate;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

function sel(this, that : String) : string;
begin
  if (this = that) then
    result := 'selected'
  else
    result := '';
end;

procedure TFHIRPackageWebServer.serveSearch(name, dependson, canonicalPkg, canonicalUrl,
  FHIRVersion, dependency, sort: String; secure, objWrapper: boolean;
  request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  conn : TFDBConnection;
  json : TJsonArray;
  v, obj, w : TJsonObject;
  e : TJsonNode;
  filter, src, dep : String;
  vars : TFslMap<TFHIRObject>;
  list : TFslList<TJsonObject>;
  dt : TFslDateTime;
  versioned : boolean;
  index : integer;
  deps, depsDone : TStringList;
begin
  conn := FDB.getConnection('Package.server.search');
  try
    list := TFslList<TJsonObject>.Create;
    try
      versioned := false;
      deps := TStringList.create;
      depsDone := TStringList.create;
      json := TJsonArray.Create;
      try
        depsDone.Sorted := true;
        repeat
          filter := '';
          if name <> '' then
          begin
            versioned := name.contains('#');
            if (name.Contains('#')) then
            begin
              filter := filter + ' and PackageVersions.id like ''%'+SQLWrapString(name.Substring(0, name.IndexOf('#')))+'%'' and PackageVersions.version like '''+SQLWrapString(name.Substring(name.IndexOf('#')+1))+'%''';

            end
            else
              filter := filter + ' and PackageVersions.id like ''%'+SQLWrapString(name)+'%''';
          end;
          if deps.count > 0 then
            filter := filter + ' and PackageVersions.PackageVersionKey in (select PackageDependencies.PackageVersionKey from PackageDependencies where PackageDependencies.Dependency in ('+deps.CommaText+'))'
          else if dependson <> '' then
          begin
            filter := filter + ' and PackageVersions.PackageVersionKey in (select PackageDependencies.PackageVersionKey from PackageDependencies where PackageDependencies.Dependency like ''%'+SQLWrapString(dependson)+'%'')';
            versioned := dependson.contains('#');
          end;

          if canonicalPkg <> '' then
            if canonicalPkg.EndsWith('%') then
              filter := filter + ' and PackageVersions.canonical like '''+SQLWrapString(canonicalPkg)+''''
            else
              filter := filter + ' and PackageVersions.canonical = '''+SQLWrapString(canonicalPkg)+'''';
          if canonicalUrl <> '' then
            filter := filter + ' and PackageVersions.PackageVersionKey in (Select PackageVersionKey from PackageURLs where URL like '''+SQLWrapString(canonicalUrl)+'%'')';
          if FHIRVersion <> '' then
            filter := filter + ' and PackageVersions.PackageVersionKey in (Select PackageVersionKey from PackageFHIRVersions where Version like '''+SQLWrapString(getVersion(FHIRVersion))+'%'')';

          if dependency <> '' then
            if dependency.Contains('#') then
              filter := filter + ' and PackageVersions.PackageVersionKey in (select PackageVersionKey from PackageDependencies where Dependency like '''+SQLWrapString(dependency)+'%'')'
            else if dependency.Contains('|') then
              filter := filter + ' and PackageVersions.PackageVersionKey in (select PackageVersionKey from PackageDependencies where Dependency like '''+SQLWrapString(dependency.Replace('|', '#'))+'%'')'
            else
              filter := filter + ' and PackageVersions.PackageVersionKey in (select PackageVersionKey from PackageDependencies where Dependency like '''+SQLWrapString(dependency)+'#%'')';

          Logging.log(filter);
          if versioned then
            conn.sql := 'select Id, Version, PubDate, FhirVersions, Kind, Canonical, Description from PackageVersions '+
              'where PackageVersions.PackageVersionKey > 0 '+filter+' order by PubDate'
          else
            conn.sql := 'select Packages.Id, Version, PubDate, FhirVersions, Kind, PackageVersions.Canonical, Packages.DownloadCount, Description from Packages, PackageVersions '+
              'where Packages.CurrentVersion = PackageVersions.PackageVersionKey '+filter+' order by PubDate';
          conn.prepare;
          conn.Execute;
          deps.clear;
          while conn.FetchNext do
          begin
            dep := ''''+conn.ColStringByName['Id']+'#'+conn.ColStringByName['Version']+'''';
            if (not versioned) or not depsDone.Find(dep, index) then
            begin
              if (versioned) then
              begin
                depsDone.add(dep);
                if (dependson <> '') then
                  deps.add(dep);
              end;
              v := TJsonObject.Create;
              json.add(v);
              list.Add(v.Link);
              v['name'] := conn.ColStringByName['Id'];
              dt := conn.ColDateTimeExByName['PubDate'];
              if (dt.year > 0) then
                v['date'] := dt.toXML;
              v['version'] := conn.ColStringByName['Version'];
              v['fhirVersion'] := interpretVersion(conn.ColStringByName['FhirVersions']);
                v['count'] := conn.ColStringByName['DownloadCount'];
              v['canonical'] := conn.ColStringByName['Canonical'];
              if not conn.ColNullByName['Description'] then
                v['description'] := conn.ColBlobAsStringByName['Description'];
              v['kind'] := codeForKind(conn.ColIntegerByName['Kind']);
              v['url'] := URLPath([AbsoluteUrl(secure), conn.ColStringByName['Id'], conn.ColStringByName['Version']]);
            end;
          end;
          conn.terminate;
        until deps.count = 0;

        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        if (request.Accept.contains('/html')) then
        begin
          vars := TFslMap<TFHIRObject>.Create('vars');
          try
            vars.add('name', TFHIRObjectText.Create(name));
            vars.add('dependson', TFHIRObjectText.Create(dependson));
            vars.add('canonicalPkg', TFHIRObjectText.Create(canonicalPkg));
            vars.add('canonicalUrl', TFHIRObjectText.Create(canonicalUrl));
            vars.add('FHIRVersion', TFHIRObjectText.Create(FHIRVersion));
            vars.add('count', TFHIRObjectText.Create(conn.CountSQL('Select count(*) from PackageVersions')));
            vars.add('prefix', TFHIRObjectText.Create(AbsoluteUrl(secure)));
            vars.add('ver', TFHIRObjectText.Create('4.0.1'));
            vars.add('r2selected', TFHIRObjectText.Create(sel('R2', FHIRVersion)));
            vars.add('r3selected', TFHIRObjectText.Create(sel('R3', FHIRVersion)));
            vars.add('r4selected', TFHIRObjectText.Create(sel('R4', FHIRVersion)));
            vars.add('r5selected', TFHIRObjectText.Create(sel('R5', FHIRVersion)));
            vars.add('matches', TFHIRObjectText.Create(genTable(AbsoluteUrl(secure)+'/catalog?name='+name+'&dependson='+EncodeMIME(dependson)+'&fhirVersion='+FHIRVersion+'&canonicalPkg='+canonicalPkg+'&canonical='+canonicalUrl, list, readSort(sort), sort.startsWith('-'), true, secure, true, versioned)));
            vars.add('status', TFHIRObjectText.Create(status));
            vars.add('downloads', TFHIRObjectText.Create(conn.CountSQL('select Sum(DownloadCount) from PackageVersions')));
            returnFile(request, response, nil, request.Document, 'packages-search.html', secure, vars);
          finally
            vars.free;
          end;
        end
        else
        begin
          if objWrapper then
          begin
            obj := TJsonObject.create;
            try
              for e in json do
              begin
                w := TJsonObject.create;
                obj.forceArr['objects'].add(w);
                w.obj['package'] := (e as TJsonObject).link;
              end;
              src := TJsonWriterDirect.writeObjectStr(obj, true);
            finally
              obj.free;
            end;
          end
          else
            src := TJsonWriterDirect.writeArrayStr(json, true);
          response.ContentType := 'application/json';
          response.ContentText := src;
        end;
      finally
        json.free;
        deps.free;
        depsDone.free;
      end;
    finally
      list.free;
    end;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

procedure TFHIRPackageWebServer.serveUpdates(date : TFslDateTime; secure : boolean; response : TIdHTTPResponseInfo);
var
  conn : TFDBConnection;
  json : TJsonArray;
  v : TJsonObject;
  src : String;
begin
  conn := FDB.getConnection('Package.server.search');
  try
    conn.sql := 'select Id, Version, PubDate, FhirVersions, Kind, Canonical, Description from PackageVersions where Indexed >= :d order by Indexed';
    conn.prepare;
    conn.BindDateTimeEx('d', date);
    conn.Execute;
    json := TJsonArray.Create;
    try
      while conn.FetchNext do
      begin
        v := TJsonObject.Create;
        json.add(v);
        v['name'] := conn.ColStringByName['Id'];
        v['date'] := conn.ColDateTimeExByName['PubDate'].toXML;
        v['version'] := conn.ColStringByName['Version'];
        v['canonical'] := conn.ColStringByName['Canonical'];
        v['fhirVersion'] := interpretVersion(conn.ColStringByName['FhirVersions']);
        if not conn.ColNullByName['Description'] then
          v['description'] := conn.ColBlobAsStringByName['Description'];
        v['kind'] := codeForKind(conn.ColIntegerByName['Kind']);
        v['url'] := URLPath([AbsoluteUrl(secure), conn.ColStringByName['Id'], conn.ColStringByName['Version']]);
      end;

      src := TJsonWriterDirect.writeArrayStr(json, true);
    finally
      json.free;
    end;
    conn.terminate;
    response.ResponseNo := 200;
    response.ResponseText := 'OK';
    response.Date := TFslDateTime.makeUTC.DateTime;
    response.ContentType := 'application/json';
    response.ContentText := src;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

procedure TFHIRPackageWebServer.serveUpload(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; secure: boolean; id: String);
  procedure check(test : boolean; code : integer; msg : String);
  begin
    if not test then
      raise EWebServerException.Create(code, msg, '', '');
  end;
var
  src : TBytes;
  npm : TNpmPackage;
  c : TFDBConnection;
  mt, s : String;
  vkey : integer;
  new : boolean;
begin
  c := FDB.GetConnection('package.upload.current');
  try
    check(isValidPackageId(id), 404, 'Invalid Package ID "'+id+'"');
    src := StreamToBytes(request.PostStream);
    npm := TNpmPackage.fromPackage(src, 'Post Content', nil, false);
    try
      check(npm.name = id, 400, 'Package ID mismatch: "'+id+'" vs "'+npm.name+'"');
      check(npm.version <> '', 400, 'Package has no version');
      check(npm.date <> '', 400, 'Package has no Date');
      check(npm.kind <> fpkNull, 400, 'Package has no Kind');
      check(npm.canonical <> '', 400, 'Package has no Canonical');
      check(npm.fhirVersionList <> '', 400, 'Package has no FHIR Versions');

      mt := '';
      c.SQL := 'Select * from Packages where Id = '''+SQLWrapString(id)+'''';
      c.Prepare;
      c.Execute;
      new := not c.FetchNext;
      if (not new) then 
        mt := c.ColStringByName['ManualToken'];
      c.Terminate;

      if (mt = '') then
        check((request.AuthUsername = 'system') and (request.AuthPassword = FSystemToken), 401, 'Valid System Password required')
      else
        check((request.AuthUsername = 'ig') and (request.AuthPassword = mt), 401, 'Valid IG Password required');

      // does current exist?
      vKey := c.CountSQL('Select PackageVersionKey from PackageVersions where Id = '''+SQLWrapString(id)+''' and version = ''current''');
      if vKey = 0 then
      begin
        vkey := c.CountSQL('Select Max(PackageVersionKey) from PackageVersions') + 1; 
        c.SQL := 'Insert into PackageVersions '+
         '(PackageVersionKey, GUID, PubDate, Indexed, Id, Version, Kind, DownloadCount, Canonical, FhirVersions, UploadCount, Description, ManualToken, Hash, Content) values ('+
         inttostr(vkey)+', '''+SQLWrapString(NewGuidId)+''', :d, getDate(), '''+SQLWrapString(id)+''', ''current'', '''+
           inttostr(ord(npm.kind))+''', 0, :u, :f, 1, :desc, null, :hash, :c)';
      end
      else
      begin
        c.ExecSQL('Delete from PackageFHIRVersions where PackageVersionKey = '+inttostr(vkey));
        c.ExecSQL('Delete from PackageDependencies where PackageVersionKey = '+inttostr(vkey));
        c.SQL := 'Update PackageVersions set PubDate = :d, Indexed = getDate(), Kind = '''+inttostr(ord(npm.kind))+''', Canonical = :u, '+
                 'FhirVersions = :f, UploadCount = UploadCount + 1, Description = :desc, Content = :c where PackageVersionKey = '+inttostr(vkey);      
      end;
      c.prepare;
      c.BindDateTimeEx('d', TFslDateTime.fromHL7(npm.date));
      c.BindString('u', npm.canonical);
      c.BindString('f', npm.fhirVersionList);
      c.BindBlobFromString('desc', 'Content from IG publisher');
      c.BindString('hash', genHash(src));
      c.BindBlob('c', src);
      c.Execute;
      c.Terminate;
      for s in npm.fhirVersionList.Split([',']) do
        c.ExecSQL('Insert into PackageFHIRVersions (PackageVersionKey, Version) values ('+inttostr(vkey)+', '''+SQLWrapString(s)+''')');
      for s in npm.dependencies do
        c.ExecSQL('Insert into PackageDependencies (PackageVersionKey, Dependency) values('+inttostr(vkey)+', '''+SQLWrapString(s)+''')');

      if new then
        c.ExecSQL('Insert into Packages (PackageKey, Id, Canonical, CurrentVersion, DownloadCount) values ('+
          inttostr(c.CountSQL('Select Max(PackageKey) from Packages') + 1)+', '''+SQLWrapString(id)+''', '''+SQLWrapString(npm.canonical)+''', '+inttostr(vkey)+', 0, null, 0)');
      
      response.ResponseNo := 200;
      response.ContentText := 'Current Version Updated';
    finally
      npm.free;
    end;
    c.Release;
  except
    on e : EWebServerException do
    begin
      c.Error(e);
      response.ResponseNo := e.Code;
      response.ContentText := e.Message;
    end;
    on e : Exception do
    begin
      c.Error(e);
      response.ResponseNo := 500;
      response.ContentText := e.Message;
    end;
  end;
end;

function TFHIRPackageWebServer.serveCreatePackage(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
var
  blob : TBytes;
  conn : TFDBConnection;
  canonical, mask, token, id, version : String;
  i : integer;
  npm : TNpmPackage;
  ts : TStringList;
begin
  conn := FDB.GetConnection('Package.server.create');
  try
    token := request.AuthPassword;
    if (token = '') then
      raise EFslException.Create('Authorization is required');
    mask := conn.LookupString('PackagePermissions', 'ManualToken', token, 'Mask', '');
    if (mask = '') then
      raise EFslException.Create('Authorization header not acceptable');
    if request.PostStream = nil then
      raise EFslException.Create('No Post Content found');
    blob := StreamToBytes(request.PostStream);
    npm := TNpmPackage.fromPackage(blob, '', nil, false);
    try
      id := npm.info['name'];
      version := npm.info['version'];
      result := 'Create Package '+id+'#'+version;
      canonical := npm.info['canonical'];
      if (id = '') then
        raise EFslException.Create('No NPM Name found in package');
      if not isValidPackageId(id) then
        raise EFslException.Create('Id "'+id+'" is not valid');
      if (version = '') then
        raise EFslException.Create('No version found in package');
      if not TSemanticVersion.isValid(version) then
        raise EFslException.Create('Version "'+version+'" is not valid');
      if (canonical = '') then
        raise EFslException.Create('No canonical found in package');
      if not isAbsoluteUrl(canonical) then
        raise EFslException.Create('Canonical "'+canonical+'" is not valid');
      i := mask.IndexOf('*');
      if i > 1 then
      begin
        if (id.Substring(i) <> mask.Substring(i)) then
          raise EFslException.Create('The security context has permissions of "'+mask+'" and is not authorised to upload "'+id+'"');
      end;
      conn.SQL := 'Select Count(*) from PackageVersions where Id = :i and Version = :v';
      conn.Prepare;
      conn.BindString('i', id);
      conn.BindString('v', version);
      conn.Execute;
      conn.FetchNext;
      if conn.ColInteger[1] > 0 then
        raise EFslException.Create('The packageId "'+id+'#'+version+'" already exists, and can''t be changed');
      conn.Terminate;

      ts := TStringList.Create;
      try
        TPackageUpdater.processURLs(npm, ts);

        // ok, it's passed all the tests...
        TPackageUpdater.commit(conn, blob, npm, TFslDateTime.makeUTC, NewGuidId, id, version, npm.description, canonical, token, ts, npm.kind);
      finally
        ts.free;
      end;
    finally
      npm.free;
    end;
    conn.Release;
    response.ResponseNo := 202;
    response.ResponseText := 'Created';
  except
    on e : Exception do
    begin
      conn.Error(e);
      response.ResponseNo := 500;
      response.ResponseText := 'Error';
      response.ContentText := e.Message;
    end;
  end;
end;

function TFHIRPackageWebServer.PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TFslTimeTracker) : String;
begin
  countRequest;
  result := doRequest(AContext, request, response, id, false);
end;

procedure TFHIRPackageWebServer.processProtectForm(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id, pword: String);
var
  conn : TFDBConnection;
  vars : TFslMap<TFHIRObject>;
  token : String;
begin
  if pword <> FSystemToken then
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Invalid System Password';
  end
  else
  begin
    conn := FDB.getConnection('Package.server.protect');
    try
      conn.sql := 'Select ManualToken from Packages where Id = '''+sqlWrapString(id)+'''';
      conn.prepare;
      conn.Execute;
      if not conn.FetchNext then
      begin
        response.ResponseNo := 409;
        response.ContentText := 'This package is not known';
      end
      else if conn.ColStringByName['ManualToken'] <> '' then
      begin
        response.ResponseNo := 409;
        response.ContentText := 'This package is already secured';
        conn.terminate;
      end
      else
      begin
        conn.terminate;
        token := NewGuidId;
        conn.ExecSQL('Update Packages set Security = 1, ManualToken = '''+pword+''' where Id = '''+sqlWrapString(id)+'''');
        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        vars := TFslMap<TFHIRObject>.Create;
        try
          vars.add('prefix', TFHIRObjectText.Create(AbsoluteUrl(false)));
          vars.add('ver', TFHIRObjectText.Create('4.0.1'));
          vars.add('pid', TFHIRObjectText.Create(id));
          vars.add('pword', TFHIRObjectText.Create(pword));
          returnFile(request, response, nil, request.Document, 'packages-protected.html', false, vars);
        finally
          vars.free;
        end;
      end;
      conn.release;
    except
      on e : Exception do
      begin
        conn.error(e);
        raise;
      end;
    end;
  end;
end;

function TFHIRPackageWebServer.doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; secure : boolean) : String;
var
  pm : THTTPParameters;
  s : TArray<String>;
  sId : string;
begin
  pm := THTTPParameters.Create(request.UnparsedParams);
  try
    if (request.CommandType = hcGET) and (request.Document = '/packages/catalog') then
    begin
      if not pm.has('lastUpdated') then
      begin
        serveSearch(pm['name'], pm['dependson'], pm['pkgcanonical'], pm['canonical'], pm['fhirversion'], pm['dependency'], pm['sort'], secure, false, request, response);
        result := 'Search Packages';
      end
      else if pm['lastUpdated'].startsWith('-') then
      begin
        serveUpdates(TFslDateTime.makeToday.add(StrToIntDef(pm['lastUpdated'], -30)), secure, response);
        result := 'Packages updates since '+pm['lastUpdated'];
      end
      else
      begin
        serveUpdates(TFslDateTime.fromXML(pm['lastUpdated']), secure, response);
        result := 'Packages updates since '+pm['lastUpdated'];
      end;
    end
    else if (request.CommandType = hcGET) and (request.Document = '/packages/-/v1/search') then
    begin
      serveSearch(pm['text'], pm['dependson'], pm['pkgcanonical'], pm['canonical'], pm['fhirversion'], pm['dependency'], pm['sort'], secure, true, request, response);
      result := 'Search Packages (v1)';
    end
    else if (request.CommandType = hcGET) and (request.Document = '/packages/log') then
    begin
      serveLog(request, response);
    end
    else if (request.CommandType = hcGET) and (request.Document = '/packages/broken') then
    begin
      serveBroken(request, response, pm['filter']);
    end
    else if (request.CommandType = hcGET) and (request.Document = '/packages/protect') then
    begin
      serveProtectForm(request, response, pm['id']);
    end
    else if (request.CommandType = hcPOST) and (request.Document = '/packages/protect') then
    begin
      processProtectForm(request, response, pm['id'], pm['password']);
    end
    else if request.CommandType = hcGET then
    begin
      s := request.document.subString(PathWithSlash.length).split(['/']);
      if length(s) = 1 then
      begin
        if s[0] = '' then
          response.Redirect('/packages/catalog')
        else if s[0].endsWith('.html') then
        begin
          servePage(s[0], request, response, secure);
          result := 'Package Web Doco';
        end
        else
        begin
          serveVersions(s[0], pm['sort'], secure, request, response);
          result := 'Package Versions for '+s[0];
        end;
      end
      else if length(s) = 2 then
      begin
        serveDownload(secure, s[0], s[1], response);
        result := 'Package Download for '+s[0]+'#'+s[1];
      end
      else if (request.Accept.contains('/html')) then
      begin
        servePage('packages-home.html', request, response, secure);
        result := 'Package Web Request';
      end
      else
        raise EFslException.Create('The operation GET '+request.Document+' is not supported');
    end
    else if (request.CommandType = hcPUT) and request.Document.StartsWith('/packages/') and request.Document.endsWith('/current') then
    begin
      sId := request.Document.Substring(10, request.Document.Length - (8 + 10));
      serveUpload(request, response, secure, sId);
    end
    else if (request.CommandType = hcPOST) and (request.Document = '/packages') then
    begin
      result := serveCreatePackage(request, response);
    end
    else
    begin
      raise EFslException.Create('The operation '+request.Command+' is not supported');
    end;
  finally
    pm.free;
  end;
end;

constructor TFHIRPackageWebServer.Create(code, path: String; common: TFHIRWebServerCommon);
begin
  inherited Create(code, path, common);
  FCrawlerLog := TJsonObject.create;
  FCrawlerLog['status'] := 'No crawl has completed yet';
end;

function TFHIRPackageWebServer.SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo;  cert: TIdOpenSSLX509; id: String; tt : TFslTimeTracker): String;
begin
  countRequest;
  result := doRequest(AContext, request, response, id, true);
end;

end.
