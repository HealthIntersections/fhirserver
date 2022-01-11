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
  fsl_base, fsl_utilities, fsl_threads, fsl_logging, fsl_json, fsl_http, fsl_npm, fsl_stream, fsl_versions,
  fdb_manager,
  fhir_objects,
  package_spider,

  server_config, utilities,
  database_installer, telnet_server,
  tx_manager, time_tracker, kernel_thread,
  web_event, web_base, endpoint, session;

type
  TPackageServerEndPoint = class;

  TPackageUpdaterThread = class(TFslThread)
  private
    FDB : TFDBManager;
    FEndPoint : TPackageServerEndPoint;
    FNextRun : TDateTime;
    FLastEmail : TDateTime;
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

  TFHIRPackageWebServer = class (TFhirWebServerEndpoint)
  private
    FDB : TFDBManager;
    FLastUpdate : TDateTime;
    FNextScan : TDateTIme;
    FScanning: boolean;
    FSystemToken : String;

    procedure setDB(value : TFDBManager);
    function status : String;

    function getVersion(v : String) : String;
    function interpretVersion(v : String) : String;

    function genTable(url : String; list: TFslList<TJsonObject>; sort : TMatchTableSort; rev, inSearch, secure, packageLevel: boolean): String;

    function serveCreatePackage(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo) : String;

    procedure servePage(fn : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; secure : boolean);
    procedure serveDownload(id, version : String; response : TIdHTTPResponseInfo);
    procedure serveVersions(id, sort : String; secure : boolean; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveSearch(name, canonicalPkg, canonicalUrl, FHIRVersion, sort : String; secure : boolean; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveUpdates(date : TFslDateTime; secure : boolean; response : TIdHTTPResponseInfo);
    procedure serveProtectForm(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; id : String);
    procedure serveUpload(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; secure : boolean; id : String);
    procedure processProtectForm(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; id, pword : String);
    procedure SetScanning(const Value: boolean);

    function doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;
  public
    destructor Destroy; override;
    function link  : TFHIRPackageWebServer; overload;
    function description : String; override;

    property DB : TFDBManager read FDB write SetDB;
    property NextScan : TDateTIme read FNextScan write FNextScan;
    property scanning : boolean read FScanning write SetScanning;
    property SystemToken : String read FSystemToken write FSystemToken;

    function PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String; override;
    function SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TTimeTracker) : String; override;
    function logId : string; override;
  end;

  TPackageServerEndPoint = class (TFHIRServerEndPoint)
  private
    FPackageServer : TFHIRPackageWebServer;
    FUpdater : TPackageUpdaterThread;
    FSystemToken : String;
    procedure upgradeDatabase;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
    destructor Destroy; override;

    property SystemToken : String read FSystemToken write FSystemToken;
    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
    procedure InstallDatabase; override;
    procedure UninstallDatabase; override;
    procedure LoadPackages(plist : String); override;
    procedure updateAdminPassword; override;
    procedure Load; override;
    Procedure Unload; override;
    procedure internalThread(callback : TFhirServerMaintenanceThreadTaskCallBack); override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure clearCache; override;
    procedure SetCacheStatus(status : boolean); override;
    procedure getCacheInfo(ci: TCacheInformation); override;
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

constructor TPackageServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
begin
  inherited create(config, settings, db, common, nil);
  upgradeDatabase;
  FSystemToken := settings.Ini.service.prop['system-token'].value;
end;

destructor TPackageServerEndPoint.Destroy;
begin
  FPackageServer.Free;

  inherited;
end;

procedure TPackageServerEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited;

end;

procedure TPackageServerEndPoint.Load;
begin
  FUpdater := TPackageUpdaterThread.Create(Database.Link, self);
end;

procedure TPackageServerEndPoint.Unload;
begin
  FUpdater.StopAndWait(50);
  FUpdater.Free;
  FUpdater := nil;
end;

procedure TPackageServerEndPoint.InstallDatabase;
var
  dbi : TFHIRDatabaseInstaller;
  conn : TFDBConnection;
begin
  conn := Database.GetConnection('install');
  try
    dbi := TFHIRDatabaseInstaller.create(conn, nil, nil);
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
    dbi := TFHIRDatabaseInstaller.create(conn, nil, nil);
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

procedure TPackageServerEndPoint.internalThread;
begin
  // nothing, for now
  // todo: health check on spider
end;

procedure TPackageServerEndPoint.LoadPackages(plist: String);
begin
  raise EFslException.Create('This is not applicable to this endpoint');
end;

function TPackageServerEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  FPackageServer := TFHIRPackageWebServer.Create(config.name, config['path'].value, common);
  FPackageServer.DB := Database.Link;
  FPackageServer.NextScan := FUpdater.FNextRun;
  FPackageServer.SystemToken := FSystemToken;
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

procedure TPackageServerEndPoint.updateAdminPassword;
begin
  raise EFslException.Create('This is not applicable to this endpoint');
end;

procedure TPackageServerEndPoint.upgradeDatabase;
var
  c : TFDBConnection;
  m : TFDBMetaData;
  t : TFDBTable;
begin
  c := Database.GetConnection('Version check');
  try
    m := c.FetchMetaData;
    try
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
    finally
      m.Free;
    end;
    c.Release;
  except
    on e: Exception do
      c.Error(e);
  end;
end;

{ TPackageUpdaterThread }

constructor TPackageUpdaterThread.Create(db: TFDBManager; endPoint : TPackageServerEndPoint);
begin
  inherited create;
  FDB := db;
  FEndPoint := endPoint;
  FNextRun := now + 1/(24 * 60);
end;

destructor TPackageUpdaterThread.Destroy;
begin
  FDB.Free;
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
    upd := TPackageUpdater.create;
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
  FDB.Free;
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
  inherited create;
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

function TFHIRPackageWebServer.genTable(url : String; list: TFslList<TJsonObject>; sort : TMatchTableSort; rev, inSearch, secure, packageLevel: boolean): String;
var
  b : TFslStringBuilder;
  i : TJsonObject;
  ss : String;
  function securityLink(i : TJsonObject) : String;
  begin
    if i.str['security'] = 'all' then
      result := '<img src="/sec_secure.png" title="New versions of this package must be posted manually, and a password is required"/> Locked'
    else if i.str['security'] = 'current' then
      result := '<img src="/sec_protected.png" title="The web spider maintains this package. A password is required to post new #current versions"/> Protected'
    else
      result := '<img src="/spacer.png"/><a href="protect?id='+i['name']+'">Protect me</a>';
  end;
begin
  if inSearch then
    ss := '&sort='
  else
    ss := '?sort=';
  if rev then
    list.sort(TFHIRPackageWebServerSorter.create(sort, -1))
  else
    list.sort(TFHIRPackageWebServerSorter.create(sort, 1));
  b := TFslStringBuilder.Create;
  try
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
    b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsDownloads, sort, rev)+'"># Downloads</a></td>'#13#10);
    if packageLevel then
      b.Append('  <td class="pck-col" style="white-space:nowrap; width:150px">Security</td>'#13#10);
    b.Append('</tr">'#13#10);
    for i in list do
    begin
      b.Append('<tr class="pck-match">'#13#10);
      if (inSearch) then
        b.Append('  <td class="pck-cell"><a href="'+i['url']+'" title="'+FormatTextToHTML(i['description'])+'">'+i['name']+'</a></td>'#13#10);
      if (inSearch) then
        b.Append('  <td class="pck-cell" style="white-space:nowrap; width:100px">'+i['version']+' (<a href="'+URLPath([AbsoluteURL(secure), i['name']])+'">all</a>)</td>'#13#10)
      else
        b.Append('  <td class="pck-cell" style="white-space:nowrap; width:100px"><a href="'+i['url']+'" title="'+FormatTextToHTML(i['description'])+'">'+i['version']+'</a></td>'#13#10);
      b.Append('  <td class="pck-cell" style="white-space:nowrap; width:100px">'+i['date'].Substring(0, 10)+'</td>'#13#10);
      b.Append('  <td class="pck-cell">'+i['fhirVersion']+'</td>'#13#10);
      if (inSearch) then
        b.Append('  <td class="pck-cell"><a href="'+i['canonical']+'">'+i['canonical']+'</a></td>'#13#10);
      b.Append('  <td class="pck-cell">'+i['kind']+'</td>'#13#10);
      b.Append('  <td class="pck-cell">'+i['count']+'</td>'#13#10);
      if packageLevel then
        b.Append('  <td class="pck-cell" style="white-space:nowrap; width:150px">'+securityLink(i)+'</td>'#13#10);
      b.Append('</tr">'#13#10);
    end;
    b.Append('</table>'#13#10);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFHIRPackageWebServer.getVersion(v : String) : String;
begin
  if v.StartsWith('4.0') then
    result := '4.0'
  else if v.StartsWith('3.0') then
    result := '3.0'
  else if v.StartsWith('1.0') then
    result := '1.0'
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
    if (v.StartsWith('4.0')) then
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
      result := '??'
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
    ts.Free;
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

procedure TFHIRPackageWebServer.serveDownload(id, version : String; response : TIdHTTPResponseInfo);
var
  conn : TFDBConnection;
  procedure sendRow;
  var
    pvk : integer;
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
  vars := TFslMap<TFHIRObject>.create('vars');
  try
    vars.add('count', TFHIRObjectText.create(FDB.CountSQL('Select count(*) from PackageVersions', 'Package.server.home')));
    vars.add('downloads', TFHIRObjectText.create(FDB.CountSQL('Select sum(DownloadCount) from Packages', 'Package.server.home')));
    vars.add('prefix', TFHIRObjectText.create(AbsoluteURL(secure)));
    vars.add('ver', TFHIRObjectText.create('4.0.1'));
    vars.add('status', TFHIRObjectText.create(status));
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
      vars := TFslMap<TFHIRObject>.create;
      try
        vars.add('prefix', TFHIRObjectText.create(AbsoluteUrl(false)));
        vars.add('ver', TFHIRObjectText.create('4.0.1'));
        vars.add('pid', TFHIRObjectText.create(id));
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

function codeForKind(kind : integer): String;
begin
  case TFHIRPackageKind(kind) of
    fpkCore : result := 'Core';
    fpkIG : result := 'IG';
    fpkIGTemplate : result := 'Template';
    fpkTool : result := 'Tool';
    fpkToolGen : result := 'Tool/Gen';
    fpkGroup : result := 'Group';
    fpkExamples : result := 'Examples';
  else
    result := '??';
  end;
end;

procedure TFHIRPackageWebServer.serveVersions(id, sort : String; secure : boolean; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
var
conn : TFDBConnection;
json, v: TJsonObject;
src : String;
vars : TFslMap<TFHIRObject>;
list : TFslList<TJsonObject>;
begin
  conn := FDB.getConnection('Package.server.versions');
  try
    conn.sql := 'Select Version, PubDate, FhirVersions, Canonical, DownloadCount, Kind, Description from PackageVersions where Id = '''+sqlWrapString(id)+''' order by PubDate asc';
    conn.prepare;
    conn.Execute;
    list := TFslList<TJsonObject>.create;
    try
      vars := TFslMap<TFHIRObject>.create('vars');
      try
        json := TJsonObject.Create;
        try
          json['_id'] := id;
          json['name'] := id;
          while conn.FetchNext do
          begin
            json.forceObj['dist-tags']['latest'] := conn.ColStringByName['Version'];
            v := json.forceObj['versions'].forceObj[conn.ColStringByName['Version']];
            list.Add(v.link);
            v['name'] := id;
            v['date'] := conn.ColDateTimeExByName['PubDate'].toXML;
            v['version'] := conn.ColStringByName['Version'];
            v['fhirVersion'] := interpretVersion(conn.ColStringByName['FhirVersions']);
            v['kind'] := codeForKind(conn.ColIntegerByName['Kind']);
            v['count'] := conn.ColStringByName['DownloadCount'];
            v['canonical'] := conn.ColStringByName['Canonical'];
            if not conn.ColNullByName['Description'] then
            begin
              json['description'] := conn.ColBlobAsStringByName['Description'];
              v['description'] := conn.ColBlobAsStringByName['Description'];
            end;
            v['url'] := URLPath([AbsoluteUrl(secure), id, conn.ColStringByName['Version']]);
          end;

          vars.add('name', TFHIRObjectText.create(json['name']));
          vars.add('desc', TFHIRObjectText.create(FormatTextToHTML(json['description'])));
          src := TJsonWriterDirect.writeObjectStr(json, true);
        finally
          json.Free;
        end;
        conn.terminate;
        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        if (request.Accept.contains('/html')) then
        begin
          vars.add('prefix', TFHIRObjectText.create(AbsoluteUrl(false)));
          vars.add('ver', TFHIRObjectText.create('4.0.1'));
          vars.add('matches', TFHIRObjectText.create(genTable(AbsoluteUrl(secure)+'/'+ID, list, readSort(sort), sort.startsWith('-'), false, secure, false)));
          vars.add('status', TFHIRObjectText.create(status));
          vars.add('count', TFHIRObjectText.create(conn.CountSQL('Select count(*) from PackageVersions where Id = '''+sqlWrapString(id)+'''')));
          vars.add('downloads', TFHIRObjectText.create(conn.CountSQL('select Sum(DownloadCount) from PackageVersions where Id = '''+sqlWrapString(id)+'''')));
          returnFile(request, response, nil, request.Document, 'packages-versions.html', false, vars);
        end
        else
        begin
          response.ContentType := 'application/json';
          response.ContentText := src;
        end;
      finally
        vars.free;
      end;
    finally
      list.Free;
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

function sel(this, that : String) : string;
begin
  if (this = that) then
    result := 'selected'
  else
    result := '';
end;

procedure TFHIRPackageWebServer.serveSearch(name, canonicalPkg, canonicalURL, FHIRVersion, sort : String; secure : boolean; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
var
  conn : TFDBConnection;
  json : TJsonArray;
    v : TJsonObject;
    filter, src : String;
    vars : TFslMap<TFHIRObject>;
    list : TFslList<TJsonObject>;
begin
  conn := FDB.getConnection('Package.server.search');
  try
    filter := '';
    if name <> '' then
      filter := filter + ' and PackageVersions.id like ''%'+SQLWrapString(name)+'%''';
    if canonicalPkg <> '' then
      if canonicalPkg.EndsWith('%') then
        filter := filter + ' and PackageVersions.canonical like '''+SQLWrapString(canonicalPkg)+''''
      else
        filter := filter + ' and PackageVersions.canonical = '''+SQLWrapString(canonicalPkg)+'''';
    if canonicalUrl <> '' then
      filter := filter + ' and PackageVersions.PackageVersionKey in (Select PackageVersionKey from PackageURLs where URL like '''+SQLWrapString(canonicalUrl)+'%'')';
    if FHIRVersion <> '' then
      filter := filter + ' and PackageVersions.PackageVersionKey in (Select PackageVersionKey from PackageFHIRVersions where Version like '''+SQLWrapString(getVersion(FHIRVersion))+'%'')';

    conn.sql := 'select Packages.Id, Version, PubDate, FhirVersions, Kind, PackageVersions.Canonical, Packages.DownloadCount, Security, Packages.ManualToken, Description from Packages, PackageVersions '+
      'where Packages.CurrentVersion = PackageVersions.PackageVersionKey '+filter+' order by PubDate';
    conn.prepare;
    conn.Execute;
    list := TFslList<TJsonObject>.create;
    try
      json := TJsonArray.Create;
      try
        while conn.FetchNext do
        begin
          v := TJsonObject.Create;
          json.add(v);
          list.Add(v.Link);
          v['name'] := conn.ColStringByName['Id'];
          v['date'] := conn.ColDateTimeExByName['PubDate'].toXML;
          v['version'] := conn.ColStringByName['Version'];
          v['fhirVersion'] := interpretVersion(conn.ColStringByName['FhirVersions']);
            v['count'] := conn.ColStringByName['DownloadCount'];
          v['canonical'] := conn.ColStringByName['Canonical'];
          if not conn.ColNullByName['Description'] then
            v['description'] := conn.ColBlobAsStringByName['Description'];
          if conn.ColStringByName['ManualToken'] = '' then
            v.str['security'] := 'none'
          else if conn.ColIntegerByName['Security'] = 0 then
            v.str['security'] := 'all'
          else
            v.str['security'] := 'current';
          v['kind'] := codeForKind(conn.ColIntegerByName['Kind']);
          v['url'] := URLPath([AbsoluteUrl(secure), conn.ColStringByName['Id'], conn.ColStringByName['Version']]);
        end;

        src := TJsonWriterDirect.writeArrayStr(json, true);
      finally
        json.Free;
      end;

      conn.terminate;
      response.ResponseNo := 200;
      response.ResponseText := 'OK';
      if (request.Accept.contains('/html')) then
      begin
        vars := TFslMap<TFHIRObject>.create('vars');
        try
          vars.add('name', TFHIRObjectText.create(name));
          vars.add('canonicalPkg', TFHIRObjectText.create(canonicalPkg));
          vars.add('canonicalUrl', TFHIRObjectText.create(canonicalUrl));
          vars.add('FHIRVersion', TFHIRObjectText.create(FHIRVersion));
          vars.add('count', TFHIRObjectText.create(conn.CountSQL('Select count(*) from PackageVersions')));
          vars.add('prefix', TFHIRObjectText.create(AbsoluteUrl(secure)));
          vars.add('ver', TFHIRObjectText.create('4.0.1'));
          vars.add('r2selected', TFHIRObjectText.create(sel('R2', FHIRVersion)));
          vars.add('r3selected', TFHIRObjectText.create(sel('R3', FHIRVersion)));
          vars.add('r4selected', TFHIRObjectText.create(sel('R4', FHIRVersion)));
          vars.add('matches', TFHIRObjectText.create(genTable(AbsoluteUrl(secure)+'/catalog?name='+name+'&fhirVersion='+FHIRVersion+'&canonicalPkg='+canonicalPkg+'&canonical='+canonicalUrl, list, readSort(sort), sort.startsWith('-'), true, secure, true)));
          vars.add('status', TFHIRObjectText.create(status));
          vars.add('downloads', TFHIRObjectText.create(conn.CountSQL('select Sum(DownloadCount) from PackageVersions')));
          returnFile(request, response, nil, request.Document, 'packages-search.html', secure, vars);
        finally
          vars.free;
        end;
      end
      else
      begin
        response.ContentType := 'application/json';
        response.ContentText := src;
      end;
    finally
      list.Free;
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
      json.Free;
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
      raise EWebServerException.Create(code, msg);
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
    npm := TNpmPackage.fromPackage(src, 'Post Content', nil);
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
         '(PackageVersionKey, GUID, PubDate, Indexed, Id, Version, Kind, DownloadCount, Canonical, FhirVersions, UploadCount, Description, ManualToken, Content) values ('+
         inttostr(vkey)+', '''+SQLWrapString(NewGuidId)+''', :d, getDate(), '''+SQLWrapString(id)+''', ''current'', '''+
           inttostr(ord(npm.kind))+''', 0, :u, :f, 1, :desc, null, :c)';
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
      c.BindBlob('c', src);
      c.Execute;
      c.Terminate;
      for s in npm.fhirVersionList.Split([',']) do
        c.ExecSQL('Insert into PackageFHIRVersions (PackageVersionKey, Version) values ('+inttostr(vkey)+', '''+SQLWrapString(s)+''')');
      for s in npm.dependencies do
        c.ExecSQL('Insert into PackageDependencies (PackageVersionKey, Dependency) values('+inttostr(vkey)+', '''+SQLWrapString(s)+''')');

      if new then
        c.ExecSQL('Insert into Packages (PackageKey, Id, Canonical, CurrentVersion, DownloadCount, ManualToken, Security) values ('+
          inttostr(c.CountSQL('Select Max(PackageKey) from Packages') + 1)+', '''+SQLWrapString(id)+''', '''+SQLWrapString(npm.canonical)+''', '+inttostr(vkey)+', 0, null, 0)');
      
      response.ResponseNo := 200;
      response.ContentText := 'Current Version Updated';
    finally
      npm.Free;
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
    npm := TNpmPackage.fromPackage(blob, '', nil);
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
      npm.Free;
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

function TFHIRPackageWebServer.PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String;
begin
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
        vars := TFslMap<TFHIRObject>.create;
        try
          vars.add('prefix', TFHIRObjectText.create(AbsoluteUrl(false)));
          vars.add('ver', TFHIRObjectText.create('4.0.1'));
          vars.add('pid', TFHIRObjectText.create(id));
          vars.add('pword', TFHIRObjectText.create(pword));
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
  pm := THTTPParameters.create(request.UnparsedParams);
  try
    if (request.CommandType = hcGET) and (request.Document = '/packages/catalog') then
    begin
      if not pm.has('lastUpdated') then
      begin
        serveSearch(pm['name'], pm['pkgcanonical'], pm['canonical'], pm['fhirversion'], pm['sort'], secure, request, response);
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
        if s[0].endsWith('.html') then
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
        serveDownload(s[0], s[1], response);
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

function TFHIRPackageWebServer.SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo;  cert: TIdOpenSSLX509; id: String; tt : TTimeTracker): String;
begin
  result := doRequest(AContext, request, response, id, true);
end;

end.
