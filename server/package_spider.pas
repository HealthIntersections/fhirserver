unit package_spider;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{$I fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_json, fsl_xml, fsl_logging,
  fsl_fetcher,
  fdb_manager,
  fsl_npm;

const
  MASTER_URL = 'https://raw.githubusercontent.com/FHIR/ig-registry/master/fhir-ig-list.json';

Type
  TPackageRestrictions = class (TFslObject)
  private
    FJson : TJsonArray;
    function matches(package, mask : String) : boolean;
  public
    constructor Create(json : TJsonArray);
    destructor Destroy; override;

    function isOk(package, feed : String) : boolean;
  end;

  TSendEmailEvent = procedure (dest, subj, body : String);

  TPackageUpdater = class (TFslObject)
  private
    FDB : TFDBConnection;
    FErrors: String;
    FFeedErrors : String;
    FOnSendEmail : TSendEmailEvent;
    FTotalBytes : Cardinal;
    procedure log(msg, source : String; error : boolean);

    function fetchUrl(url, mimetype : string) : TBytes;
    function fetchJson(url : string) : TJsonObject;
    function fetchXml(url : string) : TMXmlElement;

    function hasStored(guid : String) : boolean;
    procedure store(source, guid : String; date : TFslDateTime; package : Tbytes; idver : String);

    procedure updateItem(source : String; item : TMXmlElement; pr : TPackageRestrictions);
    procedure updateTheFeed(url, source, email : String; pr : TPackageRestrictions);
  public
    procedure update(DB : TFDBConnection);

    property errors : String read FErrors;
    property OnSendEmail : TSendEmailEvent read FOnSendEmail write FOnSendEmail;

    class procedure test(db : TFDBManager);

    class procedure commit(conn : TFDBConnection; pck : TBytes; npm : TNpmPackage; date : TFslDateTime; guid, id, version, description, canonical, token : String; kind : TFHIRPackageKind);
  end;

implementation

{ TPackageUpdater }

class procedure TPackageUpdater.commit(conn: TFDBConnection; pck: TBytes; npm : TNpmPackage; date : TFslDateTime; guid, id, version, description, canonical, token: String; kind: TFHIRPackageKind);
var
  fver, dep : String;
  pkey, vkey, cvkey : integer;
begin
  vkey := conn.CountSQL('Select Max(PackageVersionKey) from PackageVersions') +1;
  conn.SQL := 'Insert into PackageVersions '+
    '(PackageVersionKey, GUID, PubDate, Indexed, Id, Version, Kind, DownloadCount, Canonical, FhirVersions, Description, ManualToken, Content) values ('+
    inttostr(vkey)+', '''+SQLWrapString(guid)+''', :d, getDate(), '''+SQLWrapString(id)+''', '''+SQLWrapString(version)+''', '''+inttostr(ord(kind))+''', 0, :u, :f, :desc, :mt, :c)';
  conn.prepare;
  conn.BindDateTimeEx('d', date);
  conn.BindString('u', canonical);
  conn.BindString('f', npm.fhirVersionList);
  conn.BindBlobFromString('desc', description);
  conn.BindString('mt', token);
  conn.BindBlob('c', pck);
  conn.Execute;
  conn.Terminate;

  for fver in npm.fhirVersionList.Split([',']) do
    conn.ExecSQL('Insert into PackageFHIRVersions (PackageVersionKey, Version) values ('+inttostr(vkey)+', '''+SQLWrapString(fver)+''')');
  for dep in npm.dependencies do
    conn.ExecSQL('Insert into PackageDependencies (PackageVersionKey, Dependency) values('+inttostr(vkey)+', '''+SQLWrapString(dep)+''')');

  pkey := conn.CountSQL('Select Max(PackageKey) from Packages where Id = '''+SQLWrapString(id)+'''');
  if pkey = 0 then
  begin
    pkey := conn.CountSQL('Select Max(PackageKey) from Packages') + 1;
    conn.SQL := 'Insert into Packages (PackageKey, Id, CurrentVersion, DownloadCount, Canonical) values ('+inttostr(pkey)+', '''+SQLWrapString(id)+''', '+inttostr(vkey)+', 0, '''+SQLWrapString(canonical)+''')';
    conn.prepare;
    conn.Execute;
    conn.Terminate;
  end
  else
  begin
    cvkey := conn.CountSQL('Select PackageVersionKey from PackageVersions order by PubDate desc, Version desc');
    if (cvkey = vkey) then    // if we aded the most recent
    begin
      conn.SQL := 'Update Packages set Canonical = '''+SQLWrapString(canonical)+''', CurrentVersion = '+inttostr(cvkey)+' where PackageKey = '+inttostr(pkey);
      conn.prepare;
      conn.Execute;
      conn.Terminate;
    end;
  end;
end;

function TPackageUpdater.fetchJson(url: string): TJsonObject;
begin
  result := TJSONParser.Parse(fetchUrl(url, 'application/json'));
end;

function TPackageUpdater.fetchUrl(url, mimetype: string): TBytes;
var
  fetcher : TInternetFetcher;
begin
  fetcher := TInternetFetcher.Create;
  try
    fetcher.URL := url+'?'+TFslDateTime.makeLocal().toHL7;
    // fetcher.Accept := mimetype;
    fetcher.userAgent := 'HealthIntersections/FhirServer';
    fetcher.Fetch;
    result := fetcher.Buffer.AsBytes;
  finally
    fetcher.Free;
  end;
  FTotalBytes := FTotalBytes + length(result);
end;

function TPackageUpdater.fetchXml(url: string): TMXmlElement;
begin
  result := TMXmlParser.Parse(fetchUrl(url, 'application/xml'), [xpResolveNamespaces, xpDropWhitespace, xpDropComments, xpHTMLEntities]);
end;

function TPackageUpdater.hasStored(guid: String): boolean;
begin
  FDB.SQL := 'select PackageVersionKey from PackageVersions where GUID = '''+SQLWrapString(guid)+'''';
  FDB.Prepare;
  FDB.Execute;
  result := FDB.FetchNext;
  FDB.Terminate;
end;

procedure TPackageUpdater.log(msg, source: String; error : boolean);
begin
  if error then
  begin
    FErrors := FErrors + msg+' (from '+source+')'+#13#10;
    FFeedErrors := FFeedErrors + msg+' (from '+source+')'+#13#10;
  end;
  Logging.log(msg);
end;

procedure TPackageUpdater.store(source, guid: String; date : TFslDateTime; package: Tbytes; idver : String);
var
  npm : TNpmPackage;
  id, version, description, canonical, fhirVersion : String;
  kind : TFHIRPackageKind;
begin
  npm := TNpmPackage.fromPackage(package, source+'#'+guid, nil);
  try
    id := npm.name;
    version := npm.version;
    if (id+'#'+version <> idver) then
    begin
      log('Error processing '+idver+': actually found '+id+'#'+version+' in the package', source, true);
      exit;
    end;
    description := npm.description;
    kind := npm.kind;
    canonical := npm.canonical;
    if npm.notForPublication then
    begin
      log('Error processing '+idver+': this package is not suitable for publication', source, true);
      exit;
    end;
    fhirVersion := npm.fhirVersion;
    if not isValidPackageId(id) then
      raise Exception.Create('Id "'+id+'" is not valid');
    if not isValidSemVer(version) then
      raise Exception.Create('Version "'+version+'" is not valid');
    if (canonical = '') then
      raise Exception.Create('No canonical found in rss');
    if not isAbsoluteUrl(canonical) then
      raise Exception.Create('Canonical "'+canonical+'" is not valid');

    commit(FDB, package, npm, date, guid, id, version, description, canonical, '', kind);

  finally
    npm.Free;
  end;
end;

class procedure TPackageUpdater.test(db: TFDBManager);
var
  conn : TFDBConnection;
begin
  conn := db.GetConnection('test');
  try
    TPackageUpdater.Create.update(conn);
  finally
    conn.Release;
  end;
end;

procedure TPackageUpdater.update(DB : TFDBConnection);
var
  json : TJsonObject;
  arr : TJsonArray;
  i : integer;
  pr : TPackageRestrictions;
begin
  log('Start Package Scan', '', false);
  FTotalBytes := 0;
  FErrors := '';
  FDB := DB;
  try
    log('Fetch '+MASTER_URL, '', false);
    json := fetchJson(MASTER_URL);
    try
      pr := TPackageRestrictions.create(json.arr['package-restrictions'].Link);
      try
        arr := json.arr['feeds'];
        for i := 0 to arr.Count - 1 do
          updateTheFeed(arr.Obj[i].str['url'], MASTER_URL, arr.Obj[i].str['email'], pr);
      finally
        pr.Free;
      end;
    finally
      json.free;
    end;
  except
    on e : Exception do
    begin
      Log('Exception Processing Registry: '+e.Message, MASTER_URL, true)
    end;
  end;
  log('Finish Package Scan - '+Logging.DescribeSize(FTotalBytes, 0), '', false);
end;

procedure TPackageUpdater.updateTheFeed(url, source, email: String; pr : TPackageRestrictions);
var
  xml : TMXmlElement;
  channel : TMXmlElement;
  item : TMXmlElement;
begin
  try
    log('Fetch '+url, source, false);
    FFeedErrors := '';

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
              updateItem(url, item, pr);
            end;
          end;
        end;
      end;
    finally
      xml.Free;
    end;
    if (FFeedErrors <> '') and (email <> '') then
        FOnSendEmail(email, 'Errors Processing '+url, FFeedErrors);
  except
    on e : Exception do
    begin
      log('Exception processing feed: '+url+': '+e.Message, source, false);
      if (email <> '') then
        FOnSendEmail(email, 'Exception Processing '+url, e.Message);
    end;
  end;
end;

procedure TPackageUpdater.updateItem(source : String; item: TMXmlElement; pr : TPackageRestrictions);
var
  guid : String;
  content : TBytes;
  date : TFslDateTime;
  id, url, d: String;
begin
  url := '??';
  guid := item.element('guid').Text;
  try
    id := item.element('title').Text;
    if pr.isOk(id, source) then
    begin
      if (not hasStored(guid)) then
      begin
        d := item.element('pubDate').Text.Replace('  ', ' ').Substring(5);
        if (d.length > 2) and (d[2] = ' ') and StringIsInteger16(d[1]) then
          d := '0'+d;
        date := TFslDateTime.fromFormat('dd mmm yyyy hh:nn:ss', d);
        log('Fetch '+item.element('link').Text, source, false);
        url := item.element('link').Text;
        content := fetchUrl(url, 'application/tar+gzip');
        store(source, guid, date, content, id);
      end;
    end
    else
      log('The package '+id+' is not allowed to come from '+source, source, true);
  except
    on e : Exception do
    begin
      log('Exception processing item: '+guid+' from '+url+': '+e.Message, source, true);
    end;
  end;
end;

{ TPackageRestrictions }

constructor TPackageRestrictions.Create(json: TJsonArray);
begin
  inherited Create;
  FJson := json;
end;

destructor TPackageRestrictions.Destroy;
begin
  FJson.Free;
  inherited;
end;

function TPackageRestrictions.isOk(package, feed: String): boolean;
var
  e, f : TJsonNode;
  eo : TJsonObject;
begin
  result := true;
  if FJson <> nil then
  begin
    for e in FJson do
    begin
      eo := e as TJsonObject;
      if matches(package, eo.str['mask']) then
      begin
        result := false;
        for f in eo.arr['feeds'] do
          result := result or (feed = TJsonString(f).value);
        exit(result);
      end;
    end;
  end
end;

function TPackageRestrictions.matches(package, mask: String): boolean;
var
  i : integer;
begin
  i := mask.IndexOf('*');
  package := package.Substring(0, i);
  mask := mask.Substring(0, i);
  result := package = mask;
end;

end.
