unit FHIR.Server.Packages;

interface

uses
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Json,
  FHIR.Web.Parsers,
  FHIR.Database.Manager;

type
  TFHIRPackageServer = class (TFslObject)
  private
    FPath : String;
    FDB : TFSLDBManager;

    procedure setDB(value : TFSLDBManager);

    function getVersion(v : String) : String;
    function interpretVersion(v : String) : String;

    procedure serveDownload(id, version : String; response : TIdHTTPResponseInfo);
    procedure serveVersions(id : String; response : TIdHTTPResponseInfo);
    procedure serveSearch(name, canonical, FHIRVersion : String; response : TIdHTTPResponseInfo);
    procedure serveUpdates(date : TFslDateTime; response : TIdHTTPResponseInfo);
  public
    destructor Destroy; override;

    property path : String read FPath write FPath;  // includes http://host/path

    property DB : TFSLDBManager read FDB write SetDB;
    procedure serve(request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo);
  end;

implementation

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
      result := 'R4'
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

procedure TFHIRPackageServer.serveVersions(id : String; response : TIdHTTPResponseInfo);
begin
  FDB.connection('Package.server.versions',
    procedure (conn : TFslDBConnection)
    var
      json, v: TJsonObject;
      src : String;
    begin
      conn.sql := 'Select Version, PubDate, FhirVersions, Canonical, Description from PackageVersions where Id = '''+sqlWrapString(id)+''' order by PubDate asc';
      conn.prepare;
      conn.Execute;
      json := TJsonObject.Create;
      try
        json['_id'] := id;
        json['name'] := id;
        while conn.FetchNext do
        begin
          json.forceObj['dist-tags']['latest'] := conn.ColStringByName['Version'];
          v := json.forceObj['versions'].forceObj[conn.ColStringByName['Version']];
          v['name'] := id;
          v['date'] := conn.ColDateTimeExByName['PubDate'].toXML;
          v['version'] := conn.ColStringByName['Version'];
          v['fhirVersion'] := interpretVersion(conn.ColStringByName['FhirVersions']);
          v['canonical'] := interpretVersion(conn.ColStringByName['Canonical']);
          if not conn.ColNullByName['Description'] then
          begin
            json['description'] := conn.ColBlobAsStringByName['Description'];
            v['description'] := conn.ColBlobAsStringByName['Description'];
          end;
          v['url'] := URLPath([path, id, conn.ColStringByName['Version']]);
        end;

        src := TJsonWriterDirect.writeObjectStr(json, true);
      finally
        json.Free;
      end;
      conn.terminate;
      response.ResponseNo := 200;
      response.ResponseText := 'OK';
      response.ContentType := 'application/json';
      response.ContentText := src;
    end
  );
end;

procedure TFHIRPackageServer.serveSearch(name, canonical, FHIRVersion : String; response : TIdHTTPResponseInfo);
begin
  FDB.connection('Package.server.search',
    procedure (conn : TFslDBConnection)
    var
      json : TJsonArray;
      v : TJsonObject;
      filter, src : String;
    begin
      filter := '';
      if name <> '' then
        filter := filter + ' and PackageVersions.id like ''%'+name+'%''';
      if canonical <> '' then
        filter := filter + ' and PackageVersions.canonical like '''+canonical+'%''';
      if FHIRVersion <> '' then
        filter := filter + ' and PackageVersions.PackageVersionKey in (Select PackageVersionKey from PackageFHIRVersions where Version like '''+getVersion(FHIRVersion)+'%'')';

      conn.sql := 'select Packages.Id, Version, PubDate, FhirVersions, PackageVersions.Canonical, Description from Packages, PackageVersions '+
        'where Packages.CurrentVersion = PackageVersions.PackageVersionKey '+filter+' order by PubDate';
      conn.prepare;
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
          v['fhirVersion'] := interpretVersion(conn.ColStringByName['FhirVersions']);
          v['canonical'] := interpretVersion(conn.ColStringByName['Canonical']);
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
      response.ContentType := 'application/json';
      response.ContentText := src;
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
          v['canonical'] := interpretVersion(conn.ColStringByName['Canonical']);
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
  if request.Document = '/packages/catalog' then
  begin
    pm := TParseMap.create(request.UnparsedParams);
    try
      if pm.VarExists('lastUpdated') then
        serveUpdates(TFslDateTime.fromXML(pm.GetVar('lastUpdated')), response)
      else
        serveSearch(pm.GetVar('name'), pm.GetVar('canonical'), pm.GetVar('fhirversion'), response);
    finally
      pm.free;
    end;
  end
  else
  begin
    s := request.document.subString(10).split(['/']);
    if length(s) = 1 then
      serveVersions(s[0], response)
    else if length(s) = 2 then
      serveDownload(s[0], s[1], response)
    else
      raise Exception.Create('Not done yet for '+request.Document);
  end;
end;

end.
















