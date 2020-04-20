unit FHIR.Server.Packages;

interface

uses
  SysUtils, Classes, System.Generics.Collections,
  IdContext, IdCustomHTTPServer,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Json,
  FHIR.Web.Parsers,
  FHIR.Database.Manager,
  FHIR.Base.Objects, FHIR.Support.Stream,
  FHIR.R4.Types, // choice of R4 is totally arbitrary
  FHIR.Server.WebBase, FHIR.Server.Session;

type
  TMatchTableSort = (mtsNull, mtsId, mtsVersion, mtsDate, mtsFhirVersion, mtsCanonical, mtsDownloads);

  TFHIRPackageServer = class (TFslObject)
  private
    FPath : String;
    FDB : TFSLDBManager;
    FLastUpdate : TDateTime;
    FNextScan : TDateTIme;
    FReturnProcessFileEvent : TReturnProcessFileEvent;
    FScanning: boolean;


    procedure setDB(value : TFSLDBManager);
    function status : String;

    function getVersion(v : String) : String;
    function interpretVersion(v : String) : String;

    function genTable(url : String; list: TFslList<TJsonObject>; sort : TMatchTableSort; rev, inSearch: boolean): String;

    procedure serveHomePage(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveDownload(id, version : String; response : TIdHTTPResponseInfo);
    procedure serveVersions(id, sort : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveSearch(name, canonical, FHIRVersion, sort : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
    procedure serveUpdates(date : TFslDateTime; response : TIdHTTPResponseInfo);
    procedure SetScanning(const Value: boolean);
  public
    destructor Destroy; override;

    property path : String read FPath write FPath;  // includes http://host/path
    property OnReturnProcessFileEvent : TReturnProcessFileEvent read FReturnProcessFileEvent write FReturnProcessFileEvent;

    property DB : TFSLDBManager read FDB write SetDB;
    property NextScan : TDateTIme read FNextScan write FNextScan;
    property scanning : boolean read FScanning write SetScanning;
    procedure serve(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
  end;

implementation

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


destructor TFHIRPackageServer.Destroy;
begin
  FDB.Free;
  inherited;
end;

procedure TFHIRPackageServer.setDB(value : TFSLDBManager);
begin
  FDB := nil;
  FDB := value;
end;

procedure TFHIRPackageServer.SetScanning(const Value: boolean);
begin
  FScanning := Value;
  FLastUpdate := now;
end;

function TFHIRPackageServer.status: String;
begin
  if FScanning then
    result := 'Scanning for updates now'
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

function TFHIRPackageServer.genTable(url : String; list: TFslList<TJsonObject>; sort : TMatchTableSort; rev, inSearch: boolean): String;
var
  b : TFslStringBuilder;
  i : TJsonObject;
  factor : integer;
  ss : String;
begin
  if inSearch then
    ss := '&sort='
  else
    ss := '?sort=';
  if rev then factor := -1 else factor := 1;
  list.sort(function (const l, r : TJsonObject) : integer
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
    end);
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
        b.Append('  <td class="pck-cell">'+i['version']+' (<a href="'+URLPath([FPath, i['name']])+'">all</a>)</td>'#13#10)
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

function TFHIRPackageServer.getVersion(v : String) : String;
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

function TFHIRPackageServer.interpretVersion(v: String): String;
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

procedure TFHIRPackageServer.serveDownload(id, version : String; response : TIdHTTPResponseInfo);
begin
  FDB.connection('Package.server.download',
    procedure (conn : TFslDBConnection)
    begin
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
    end
  );
end;

procedure TFHIRPackageServer.serveHomePage(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  vars : TFslMap<TFHIRObject>;
begin
  vars := TFslMap<TFHIRObject>.create('vars');
  try
    vars.add('count', TFHIRInteger.create(FDB.CountSQL('Select count(*) from PackageVersions', 'Package.server.home')));
    vars.add('downloads', TFHIRInteger.create(FDB.CountSQL('Select sum(DownloadCount) from Packages', 'Package.server.home')));
    vars.add('prefix', TFHIRString.create(path));
    vars.add('ver', TFHIRString.create('4.0.1'));
    vars.add('status', TFHIRString.create(status));
    FReturnProcessFileEvent(request, response, nil, request.Document, 'packages-home.html', false, vars);
  finally
    vars.free;
  end;
end;

procedure TFHIRPackageServer.serveVersions(id, sort : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
begin
  FDB.connection('Package.server.versions',
    procedure (conn : TFslDBConnection)
    var
      json, v: TJsonObject;
      src : String;
      vars : TFslMap<TFHIRObject>;
      list : TFslList<TJsonObject>;
    begin
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
              v['url'] := URLPath([path, id, conn.ColStringByName['Version']]);
            end;

            vars.add('name', TFHIRString.create(json['name']));
            vars.add('desc', TFHIRString.create(FormatTextToHTML(json['description'])));
            src := TJsonWriterDirect.writeObjectStr(json, true);
          finally
            json.Free;
          end;
          conn.terminate;
          response.ResponseNo := 200;
          response.ResponseText := 'OK';
          if (request.Accept.contains('/html')) then
          begin
            vars.add('prefix', TFHIRString.create(path));
            vars.add('ver', TFHIRString.create('4.0.1'));
            vars.add('matches', TFHIRString.create(genTable(FPath+'/'+ID, list, readSort(sort), sort.startsWith('-'), false)));
            vars.add('status', TFHIRString.create(status));
            FReturnProcessFileEvent(request, response, nil, request.Document, 'packages-versions.html', false, vars);
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
    end
  );
end;

function sel(this, that : String) : string;
begin
  if (this = that) then
    result := 'selected'
  else
    result := '';
end;

procedure TFHIRPackageServer.serveSearch(name, canonical, FHIRVersion, sort : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
begin
  FDB.connection('Package.server.search',
    procedure (conn : TFslDBConnection)
    var
      json : TJsonArray;
      v : TJsonObject;
      filter, src : String;
      vars : TFslMap<TFHIRObject>;
      list : TFslList<TJsonObject>;
    begin
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
            v['url'] := URLPath([path, conn.ColStringByName['Id'], conn.ColStringByName['Version']]);
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
            vars.add('name', TFHIRString.create(name));
            vars.add('canonical', TFHIRString.create(canonical));
            vars.add('FHIRVersion', TFHIRString.create(FHIRVersion));
            vars.add('count', TFHIRInteger.create(conn.CountSQL('Select count(*) from PackageVersions')));
            vars.add('prefix', TFHIRString.create(path));
            vars.add('ver', TFHIRString.create('4.0.1'));
            vars.add('r2selected', TFHIRString.create(sel('R2', FHIRVersion)));
            vars.add('r3selected', TFHIRString.create(sel('R3', FHIRVersion)));
            vars.add('r4selected', TFHIRString.create(sel('R4', FHIRVersion)));
            vars.add('matches', TFHIRString.create(genTable(FPath+'/catalog?name='+name+'&fhirVersion='+FHIRVersion+'&canonical='+canonical, list, readSort(sort), sort.startsWith('-'), true)));
            vars.add('status', TFHIRString.create(status));
            FReturnProcessFileEvent(request, response, nil, request.Document, 'packages-search.html', false, vars);
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
    end
  );
end;

procedure TFHIRPackageServer.serveUpdates(date : TFslDateTime; response : TIdHTTPResponseInfo);
begin
  FDB.connection('Package.server.search',
    procedure (conn : TFslDBConnection)
    var
      json : TJsonArray;
      v : TJsonObject;
      src : String;
    begin
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
          v['url'] := URLPath([path, conn.ColStringByName['Id'], conn.ColStringByName['Version']]);
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
    end
  );
end;

procedure TFHIRPackageServer.serve(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
var
  pm : TParseMap;
  s : TArray<String>;
begin
  pm := TParseMap.create(request.UnparsedParams);
  try
    if request.Document = '/packages/catalog' then
    begin
      if not pm.VarExists('lastUpdated') then
        serveSearch(pm.GetVar('name'), pm.GetVar('canonical'), pm.GetVar('fhirversion'), pm.GetVar('sort'), request, response)
      else if pm.getVar('lastUpdated').startsWith('-') then
        serveUpdates(TFslDateTime.makeToday.add(StrToIntDef(pm.GetVar('lastUpdated'), -30)), response)
      else
        serveUpdates(TFslDateTime.fromXML(pm.GetVar('lastUpdated')), response)
    end
    else
    begin
      s := request.document.subString(10).split(['/']);
      if length(s) = 1 then
        serveVersions(s[0], pm.GetVar('sort'), request, response)
      else if length(s) = 2 then
        serveDownload(s[0], s[1], response)
      else if (request.Accept.contains('/html')) then
        serveHomePage(request, response)
      else
        raise Exception.Create('Not done yet for '+request.Document);
    end;
  finally
    pm.free;
  end;
end;

end.
















