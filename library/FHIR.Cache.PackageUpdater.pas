unit FHIR.Cache.PackageUpdater;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Json, FHIR.Support.MXml, FHIR.Support.Logging,
  FHIR.Web.Fetcher,
  FHIR.Database.Manager, FHIR.Database.ODBC,
  FHIR.Cache.NpmPackage;

const
  MASTER_URL = 'https://cdn.jsdelivr.net/gh/FHIR/ig-registry/fhir-ig-list.json';
  // 'https://raw.githubusercontent.com/FHIR/ig-registry/master/fhir-ig-list.json';

Type
  TPackageUpdater = class (TFslObject)
  private
    FDB : TFslDBConnection;
    procedure log(msg : String);

    function fetchUrl(url : string) : TBytes;
    function fetchJson(url : string) : TJsonObject;
    function fetchXml(url : string) : TMXmlElement;

    function hasStored(guid : String) : boolean;
    procedure store(source, guid : String; date : TFslDateTime; package : Tbytes);

    procedure updateItem(source : String; item : TMXmlElement);
    procedure updateTheFeed(url : String);
  public
    procedure update(DB : TFslDBConnection);

    class procedure test(db : TFslDBManager);
  end;

implementation

{ TPackageUpdater }

function TPackageUpdater.fetchJson(url: string): TJsonObject;
begin
  result := TJSONParser.Parse(fetchUrl(url));
end;

function TPackageUpdater.fetchUrl(url: string): TBytes;
var
  fetcher : TInternetFetcher;
begin
  fetcher := TInternetFetcher.Create;
  try
    fetcher.URL := url+'?'+TFslDateTime.makeLocal().toHL7;
    fetcher.Fetch;
    result := fetcher.Buffer.AsBytes;
  finally
    fetcher.Free;
  end;
end;

function TPackageUpdater.fetchXml(url: string): TMXmlElement;
begin
  result := TMXmlParser.Parse(fetchUrl(url), [xpResolveNamespaces, xpDropWhitespace, xpDropComments, xpHTMLEntities]);
end;

function TPackageUpdater.hasStored(guid: String): boolean;
begin
  FDB.SQL := 'select PackageVersionKey from PackageVersions where GUID = '''+SQLWrapString(guid)+'''';
  FDB.Prepare;
  FDB.Execute;
  result := FDB.FetchNext;
  FDB.Terminate;
end;

procedure TPackageUpdater.log(msg: String);
begin
  logt(msg);
end;

procedure TPackageUpdater.store(source, guid: String; date : TFslDateTime; package: Tbytes);
var
  npm : TNpmPackage;
  pkey, vkey, cvkey : integer;
  id, version, description, canonical : String;
  kind : TFHIRPackageKind;
  fver, dep : String;
  fhirVersions : String;
begin
  npm := TNpmPackage.fromPackage(package, source+'#'+guid, nil);
  try
    id := npm.name;
    version := npm.version;
    description := npm.description;
    kind := npm.kind;
    canonical := npm.canonical;

    vkey := FDB.CountSQL('Select Max(PackageVersionKey) from PackageVersions') +1;
    FDB.SQL := 'Insert into PackageVersions '+
      '(PackageVersionKey,   GUID,                    PubDate, Indexed, Id,                   Version,                            Kind,                    Canonical, FhirVersions, Description, Content) values ('+
       inttostr(vkey)+', '''+SQLWrapString(guid)+''', :d, getDate(), '''+SQLWrapString(id)+''', '''+SQLWrapString(version)+''', '''+inttostr(ord(kind))+''', :u, :f, :t, :c)';
    FDB.prepare;
    FDB.BindBlobFromString('t', description);
    FDB.BindString('u', canonical);
    FDB.BindString('f', npm.fhirVersionList);
    FDB.BindDateTimeEx('d', date);
    FDB.BindBlob('c', package);
    FDB.Execute;
    FDB.Terminate;

    for fver in npm.fhirVersionList.Split([',']) do
      FDB.ExecSQL('Insert into PackageFHIRVersions (PackageVersionKey, Version) values ('+inttostr(vkey)+', '''+SQLWrapString(fver)+''')');
    for dep in npm.dependencies do
      FDB.ExecSQL('Insert into PackageDependencies (PackageVersionKey, Dependency) values('+inttostr(vkey)+', '''+SQLWrapString(dep)+''')');

    pkey := FDB.CountSQL('Select Max(PackageKey) from Packages where Id = '''+SQLWrapString(id)+'''');
    if pkey = 0 then
    begin
      pkey := FDB.CountSQL('Select Max(PackageKey) from Packages') + 1;
      FDB.SQL := 'Insert into Packages (PackageKey, Id, CurrentVersion, Canonical) values ('+inttostr(pkey)+', '''+SQLWrapString(id)+''', '+inttostr(vkey)+', '''+SQLWrapString(canonical)+''')';
      FDB.prepare;
      FDB.Execute;
      FDB.Terminate;
    end
    else
    begin
      cvkey := FDB.CountSQL('Select PackageVersionKey from PackageVersions order by PubDate desc');
      if (cvkey = vkey) then
      begin
        FDB.SQL := 'Update Packages set Canonical = '''+SQLWrapString(canonical)+''', CurrentVersion = cvkey where PackageKey = '+inttostr(pkey);
        FDB.prepare;
        FDB.Execute;
        FDB.Terminate;
      end;
    end;
  finally
    npm.Free;
  end;
end;

class procedure TPackageUpdater.test(db: TFslDBManager);
var
  conn : TFslDBConnection;
begin
  conn := db.GetConnection('test');
  try
    TPackageUpdater.Create.update(conn);
  finally
    conn.Release;
  end;
end;

procedure TPackageUpdater.update(DB : TFslDBConnection);
var
  json : TJsonObject;
  arr : TJsonArray;
  i : integer;
begin
  FDB := DB;
  try
    log('Fetch '+MASTER_URL);
    json := fetchJson(MASTER_URL);
    try
      arr := json.arr['feeds'];
      for i := 0 to arr.Count - 1 do
        updateTheFeed(arr.Obj[i].str['url']);
    finally
      json.free;
    end;
  except
    on e : Exception do
    begin
      Log('Exception Processing Registry: '+e.Message)
    end;
  end;
end;

procedure TPackageUpdater.updateTheFeed(url: String);
var
  xml : TMXmlElement;
  channel : TMXmlElement;
  item : TMXmlElement;
begin
  try
    log('Fetch '+url);
    xml := fetchXml(url);
    try
      for channel in xml.first.Children do
      begin
        if (channel.Name = 'channel') then
        begin
          for item in channel.Children do
          begin
            if (item.Name = 'item') then
            begin
              updateItem(url, item);
            end;
          end;
        end;
      end;
    finally
      xml.Free;
    end;
  except
    on e : Exception do
    begin
      log('Exception processing feed: '+url+': '+e.Message);
    end;
  end;
end;

procedure TPackageUpdater.updateItem(source : String; item: TMXmlElement);
var
  guid : String;
  content : TBytes;
  date : TFslDateTime;
begin
  guid := item.element('guid').Text;
  try
    if (not hasStored(guid)) then
    begin
      date := TFslDateTime.fromFormat('dd mmm yyyy hh:nn:ss', item.element('pubDate').Text.Substring(5));
      log('Fetch '+item.element('link').Text);
      content := fetchUrl(item.element('link').Text);
      store(source, guid, date, content);
    end;
  except
    on e : Exception do
    begin
      log('Exception processing item: '+guid+': '+e.Message);
    end;
  end;
end;

end.
