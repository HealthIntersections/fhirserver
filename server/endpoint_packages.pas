unit endpoint_packages;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  fsl_base, fsl_utilities, fsl_threads, fsl_logging, fsl_json, fsl_http, fsl_npm, fsl_stream,
  fdb_manager,
  fhir_objects,
  package_spider,

  server_config, utilities,
  database_installer, telnet_server,
  tx_manager,
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
  protected
    function ThreadName : String; override;
    procedure Initialise; override;
    procedure Execute; override;
  public
    constructor Create(db : TFDBManager; endPoint : TPackageServerEndPoint);
    destructor Destroy; override;
  end;

  TMatchTableSort = (mtsNull, mtsId, mtsVersion, mtsDate, mtsFhirVersion, mtsCanonical, mtsDownloads);

  TFHIRPackageWebServer = class (TFhirWebServerEndpoint)
  private
    FDB : TFDBManager;
    FLastUpdate : TDateTime;
    FNextScan : TDateTIme;
    FScanning: boolean;

    procedure setDB(value : TFDBManager);
    function status : String;

    function getVersion(v : String) : String;
    function interpretVersion(v : String) : String;

    function genTable(url : String; list: TFslList<TJsonObject>; sort : TMatchTableSort; rev, inSearch, secure: boolean): String;

    function serveCreatePackage(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo) : String;

    procedure servePage(fn : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; secure : boolean);
    procedure serveDownload(id, version : String; response : TIdHTTPResponseInfo);
    procedure serveVersions(id, sort : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveSearch(name, canonical, FHIRVersion, sort : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveUpdates(date : TFslDateTime; response : TIdHTTPResponseInfo);
    procedure SetScanning(const Value: boolean);
    function doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;
  public
    destructor Destroy; override;
    function link  : TFHIRPackageWebServer; overload;
    function description : String; override;

    property DB : TFDBManager read FDB write SetDB;
    property NextScan : TDateTIme read FNextScan write FNextScan;
    property scanning : boolean read FScanning write SetScanning;

    function PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String) : String; override;
    function SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String) : String; override;
  end;

  TPackageServerEndPoint = class (TFHIRServerEndPoint)
  private
    FPackageServer : TFHIRPackageWebServer;
    FUpdater : TPackageUpdaterThread;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
    destructor Destroy; override;

    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
    procedure InstallDatabase; override;
    procedure UninstallDatabase; override;
    procedure LoadPackages(plist : String); override;
    procedure updateAdminPassword; override;
    procedure Load; override;
    Procedure Unload; override;
    procedure internalThread; override;
    function cacheSize : UInt64; override;
    procedure clearCache; override;
  end;

implementation

function TPackageServerEndPoint.cacheSize: UInt64;
begin
  result := inherited cacheSize;
end;

procedure TPackageServerEndPoint.clearCache;
begin
  inherited;
end;

constructor TPackageServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
begin
  inherited create(config, settings, db, common);
end;

destructor TPackageServerEndPoint.Destroy;
begin
  FPackageServer.Free;

  inherited;
end;

procedure TPackageServerEndPoint.Load;
begin
  FUpdater := TPackageUpdaterThread.Create(Database.Link, self);
  FUpdater.Start;
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
  raise Exception.Create('This is not applicable to this endpoint');
end;

function TPackageServerEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  FPackageServer := TFHIRPackageWebServer.Create(config.name, config['path'].value, common);
  FPackageServer.DB := Database.Link;
  FPackageServer.NextScan := FUpdater.FNextRun;
  WebEndPoint := FPackageServer;
  result := FPackageServer.link;
end;

function TPackageServerEndPoint.summary: String;
begin
  result := 'Package Server using '+describeDatabase(Config);
end;

procedure TPackageServerEndPoint.updateAdminPassword;
begin
  raise Exception.Create('This is not applicable to this endpoint');
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

procedure TPackageUpdaterThread.RunUpdater;
var
  conn : TFDBConnection;
  upd : TPackageUpdater;
begin
  conn := FDB.getConnection('server.packages.update');
  try
    upd := TPackageUpdater.create;
    try
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
  else
    result := 0;
  end;
end;

function TFHIRPackageWebServer.genTable(url : String; list: TFslList<TJsonObject>; sort : TMatchTableSort; rev, inSearch, secure: boolean): String;
var
  b : TFslStringBuilder;
  i : TJsonObject;
  ss : String;
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
    b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsVersion, sort, rev)+'">version</a></td>'#13#10);
    b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsDate, sort, rev)+'">date</a></td>'#13#10);
    b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsFhirVersion, sort, rev)+'">FHIR Version</a></td>'#13#10);
    if (inSearch) then
      b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsCanonical, sort, rev)+'">Canonical</a></td>'#13#10);
    b.Append('  <td class="pck-col"><a href="'+url+ss+genSort(mtsDownloads, sort, rev)+'"># Downloads</a></td>'#13#10);
    b.Append('</tr">'#13#10);
    for i in list do
    begin
      b.Append('<tr class="pck-match">'#13#10);
      if (inSearch) then
        b.Append('  <td class="pck-cell"><a href="'+i['url']+'" title="'+FormatTextToHTML(i['description'])+'">'+i['name']+'</a></td>'#13#10);
      if (inSearch) then
        b.Append('  <td class="pck-cell">'+i['version']+' (<a href="'+URLPath([AbsoluteURL(secure), i['name']])+'">all</a>)</td>'#13#10)
      else
        b.Append('  <td class="pck-cell"><a href="'+i['url']+'" title="'+FormatTextToHTML(i['description'])+'">'+i['version']+'</a></td>'#13#10);
      b.Append('  <td class="pck-cell">'+i['date'].Substring(0, 10)+'</td>'#13#10);
      b.Append('  <td class="pck-cell">'+i['fhirVersion']+'</td>'#13#10);
      if (inSearch) then
        b.Append('  <td class="pck-cell"><a href="'+i['canonical']+'">'+i['canonical']+'</a></td>'#13#10);
      b.Append('  <td class="pck-cell">'+i['count']+'</td>'#13#10);
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

procedure TFHIRPackageWebServer.serveDownload(id, version : String; response : TIdHTTPResponseInfo);
var
  conn : TFDBConnection;
begin
  conn := FDB.getConnection('Package.server.download');
  try
    conn.SQL := 'Select Content from PackageVersions where Id = '''+SQLWrapString(id)+''' and Version = '''+SQLWrapString(version)+'''';
      conn.Prepare;
      conn.Execute;
      if conn.FetchNext then
      begin
        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        response.ContentType := 'application/tar+gzip';
        response.ContentStream := TBytesStream.Create(conn.GetColBlobByName('Content'));
        response.ContentDisposition := 'attachment; filename="'+id+'#'+version+'.tgz"';
        response.FreeContentStream := true;
        conn.Terminate;
        conn.SQL := 'Update PackageVersions set DownloadCount = DownloadCount + 1 where Id = '''+SQLWrapString(id)+''' and Version = '''+SQLWrapString(version)+'''';
        conn.Prepare;
        conn.Execute;
        conn.Terminate;
        conn.SQL := 'Update Packages set DownloadCount = DownloadCount + 1 where Id = '''+SQLWrapString(id)+'''';
        conn.Prepare;
        conn.Execute;
      end
      else
      begin
        response.ResponseNo := 404;
        response.ResponseText := 'Not found';
        response.ContentType := 'text/plain';
        response.ContentText := 'The package "'+id+'#'+version+'" is not known by this server';
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

procedure TFHIRPackageWebServer.serveVersions(id, sort : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
var
  conn : TFDBConnection;
  json, v: TJsonObject;
  src : String;
  vars : TFslMap<TFHIRObject>;
  list : TFslList<TJsonObject>;
begin
  conn := FDB.getConnection('Package.server.versions');
  try
      conn.sql := 'Select Version, PubDate, FhirVersions, Canonical, DownloadCount, Description from PackageVersions where Id = '''+sqlWrapString(id)+''' order by PubDate asc';
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
              v['count'] := conn.ColStringByName['DownloadCount'];
              v['canonical'] := conn.ColStringByName['Canonical'];
              if not conn.ColNullByName['Description'] then
              begin
                json['description'] := conn.ColBlobAsStringByName['Description'];
                v['description'] := conn.ColBlobAsStringByName['Description'];
              end;
              v['url'] := URLPath([AbsoluteUrl(false), id, conn.ColStringByName['Version']]);
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
            vars.add('matches', TFHIRObjectText.create(genTable(AbsoluteUrl(false)+'/'+ID, list, readSort(sort), sort.startsWith('-'), false, false)));
            vars.add('status', TFHIRObjectText.create(status));
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

procedure TFHIRPackageWebServer.serveSearch(name, canonical, FHIRVersion, sort : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
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
      filter := filter + ' and PackageVersions.id like ''%'+name+'%''';
    if canonical <> '' then
      filter := filter + ' and PackageVersions.canonical like '''+canonical+'%''';
    if FHIRVersion <> '' then
      filter := filter + ' and PackageVersions.PackageVersionKey in (Select PackageVersionKey from PackageFHIRVersions where Version like '''+getVersion(FHIRVersion)+'%'')';

    conn.sql := 'select Packages.Id, Version, PubDate, FhirVersions, PackageVersions.Canonical, Packages.DownloadCount, Description from Packages, PackageVersions '+
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
          v['url'] := URLPath([AbsoluteUrl(false), conn.ColStringByName['Id'], conn.ColStringByName['Version']]);
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
          vars.add('canonical', TFHIRObjectText.create(canonical));
          vars.add('FHIRVersion', TFHIRObjectText.create(FHIRVersion));
          vars.add('count', TFHIRObjectText.create(conn.CountSQL('Select count(*) from PackageVersions')));
          vars.add('prefix', TFHIRObjectText.create(AbsoluteUrl(false)));
          vars.add('ver', TFHIRObjectText.create('4.0.1'));
          vars.add('r2selected', TFHIRObjectText.create(sel('R2', FHIRVersion)));
          vars.add('r3selected', TFHIRObjectText.create(sel('R3', FHIRVersion)));
          vars.add('r4selected', TFHIRObjectText.create(sel('R4', FHIRVersion)));
          vars.add('matches', TFHIRObjectText.create(genTable(AbsoluteUrl(false)+'/catalog?name='+name+'&fhirVersion='+FHIRVersion+'&canonical='+canonical, list, readSort(sort), sort.startsWith('-'), true, false)));
          vars.add('status', TFHIRObjectText.create(status));
          returnFile(request, response, nil, request.Document, 'packages-search.html', false, vars);
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

procedure TFHIRPackageWebServer.serveUpdates(date : TFslDateTime; response : TIdHTTPResponseInfo);
var
  conn : TFDBConnection;
  json : TJsonArray;
  v : TJsonObject;
  src : String;
begin
  conn := FDB.getConnection('Package.server.search');
  try
    conn.sql := 'select Id, Version, PubDate, FhirVersions, Canonical, Description from PackageVersions where Indexed >= :d order by Indexed';
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
        v['url'] := URLPath([AbsoluteUrl(false), conn.ColStringByName['Id'], conn.ColStringByName['Version']]);
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

function TFHIRPackageWebServer.serveCreatePackage(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
var
  blob : TBytes;
  conn : TFDBConnection;
  canonical, mask, token, id, version : String;
  i : integer;
  npm : TNpmPackage;
begin
  conn := FDB.GetConnection('Package.server.create');
  try
    token := request.AuthPassword;
    if (token = '') then
      raise Exception.Create('Authorization is required');
    mask := conn.LookupString('PackagePermissions', 'ManualToken', token, 'Mask', '');
    if (mask = '') then
      raise Exception.Create('Authorization header not acceptable');
    if request.PostStream = nil then
      raise Exception.Create('No Post Content found');
    blob := StreamToBytes(request.PostStream);
    npm := TNpmPackage.fromPackage(blob, '', nil);
    try
      id := npm.info['name'];
      version := npm.info['version'];
      result := 'Create Package '+id+'#'+version;
      canonical := npm.info['canonical'];
      if (id = '') then
        raise Exception.Create('No NPM Name found in package');
      if not isValidPackageId(id) then
        raise Exception.Create('Id "'+id+'" is not valid');
      if (version = '') then
        raise Exception.Create('No version found in package');
      if not isValidSemVer(version) then
        raise Exception.Create('Version "'+version+'" is not valid');
      if (canonical = '') then
        raise Exception.Create('No canonical found in package');
      if not isAbsoluteUrl(canonical) then
        raise Exception.Create('Canonical "'+canonical+'" is not valid');
      i := mask.IndexOf('*');
      if i > 1 then
      begin
        if (id.Substring(i) <> mask.Substring(i)) then
          raise Exception.Create('The security context has permissions of "'+mask+'" and is not authorised to upload "'+id+'"');
      end;
      conn.SQL := 'Select Count(*) from PackageVersions where Id = :i and Version = :v';
      conn.Prepare;
      conn.BindString('i', id);
      conn.BindString('v', version);
      conn.Execute;
      conn.FetchNext;
      if conn.ColInteger[1] > 0 then
        raise Exception.Create('The packageId "'+id+'#'+version+'" already exists, and can''t be changed');
      conn.Terminate;

      // ok, it's passed all the tests...
      TPackageUpdater.commit(conn, blob, npm, TFslDateTime.makeUTC, NewGuidId, id, version, npm.description, canonical, token, npm.kind);
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

function TFHIRPackageWebServer.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String) : String;
begin
  result := doRequest(AContext, request, response, id, false);
end;

function TFHIRPackageWebServer.doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; secure : boolean) : String;
var
  pm : THTTPParameters;
  s : TArray<String>;
begin
  pm := THTTPParameters.create(request.UnparsedParams);
  try
    if (request.CommandType = hcGET) and (request.Document = '/packages/catalog') then
    begin
      if not pm.has('lastUpdated') then
      begin
        serveSearch(pm['name'], pm['canonical'], pm['fhirversion'], pm['sort'], request, response);
        result := 'Search Packages';
      end
      else if pm['lastUpdated'].startsWith('-') then
      begin
        serveUpdates(TFslDateTime.makeToday.add(StrToIntDef(pm['lastUpdated'], -30)), response);
        result := 'Packages updates since '+pm['lastUpdated'];
      end
      else
      begin
        serveUpdates(TFslDateTime.fromXML(pm['lastUpdated']), response);
        result := 'Packages updates since '+pm['lastUpdated'];
      end;
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
          serveVersions(s[0], pm['sort'], request, response);
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
        raise Exception.Create('The operation GET '+request.Document+' is not supported');
    end
    else if (request.CommandType = hcPOST) and (request.Document = '/packages') then
    begin
      result := serveCreatePackage(request, response);
    end
    else
    begin
      raise Exception.Create('The operation '+request.Command+' is not supported');
    end;
  finally
    pm.free;
  end;
end;

function TFHIRPackageWebServer.SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo;  cert: TIdOpenSSLX509; id: String): String;
begin
  result := doRequest(AContext, request, response, id, true);
end;

end.
