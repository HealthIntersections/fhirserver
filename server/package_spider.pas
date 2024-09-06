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
  SysUtils, Classes, IniFiles,
  IdHashSHA,
  fsl_base, fsl_utilities, fsl_json, fsl_xml, fsl_logging, fsl_versions,
  fsl_fetcher, fsl_zulip,
  fdb_manager, fdb_dialects,
  fsl_npm;

const
  MASTER_URL = 'https://raw.githubusercontent.com/FHIR/ig-registry/master/package-feeds.json';
  MANUAL_REG_URL = 'https://raw.githubusercontent.com/FHIR/ig-registry/master/manual-package-list.json';
  EMAIL_DAYS_LIMIT= 7;

Type
  EPackageCrawlerException = class (EFslException);

  TPackageRestrictions = class (TFslObject)
  private
    FJson : TJsonArray;
    function matches(package, mask : String) : boolean;
  public
    constructor Create(json : TJsonArray);
    destructor Destroy; override;

    function isOk(package, feed : String; var list : String) : boolean;
  end;

  TSendEmailEvent = procedure (dest, subj, body : String) of object;

  TZulipItem = class (TFslObject)
  private
    FWhen : TDateTime;
  end;

  TZulipTracker = class (TFslObject)
  private
    FZulip : TZulipSender;
    FErrors : TFslMap<TZulipItem>;
  public
    constructor Create(address, email, apikey : String);
    destructor Destroy; override;

    function Link : TZulipTracker; overload;

    procedure error(err : String);
    procedure send;
  end;

  TCrawlerLogMode = (clmStart, clmHeader, clmError, clmWarning, clmNote);

  { TPackageUpdater }

  TPackageUpdater = class (TFslObject)
  private
    FDB : TFDBConnection;
    FErrors: String;
    FFeedErrors : String;
    FOnSendEmail : TSendEmailEvent;
    FTotalBytes : Cardinal;
    FIni : TIniFile;
    FZulip : TZulipTracker;
    FCrawlerLog : TFslStringBuilder;
    FHasErrors : boolean;
    procedure clog(s : String; mode : TCrawlerLogMode);
    procedure DoSendEmail(dest, subj, body : String);
    procedure log(msg, source : String; error : boolean);

    function fetchUrl(url, mimetype : string) : TBytes;
    function fetchJson(url : string) : TJsonObject;
    function fetchXml(url : string) : TMXmlElement;

    function hasStored(guid : String) : boolean;
    procedure SetCrawlerLog(AValue: TFslStringBuilder);
    procedure store(source, url, guid : String; date : TFslDateTime; package : Tbytes; idver : String);

    procedure updateItem(source : String; item : TMXmlElement; i : integer; pr : TPackageRestrictions);
    procedure updateTheFeed(url, source, email : String; pr : TPackageRestrictions);
  public
    constructor Create(zulip : TZulipTracker);
    destructor Destroy; override;

    procedure update(DB : TFDBConnection);

    property errors : String read FErrors;
    property CrawlerLog : TFslStringBuilder read FCrawlerLog write SetCrawlerLog;
    property OnSendEmail : TSendEmailEvent read FOnSendEmail write FOnSendEmail;

    class procedure test(db : TFDBManager);

    class procedure processURLs(npm : TNpmPackage; ts : TStringList);
    class procedure commit(conn : TFDBConnection; pck : TBytes; npm : TNpmPackage; date : TFslDateTime; guid, id, version, description, canonical, token : String; urls : TStringList; kind : TFHIRPackageKind);
  end;


function genHash(bytes : TBytes) : String;

implementation

function fix(url : String) : String;
begin
  result := url.Replace('http:', 'https:');
end;

{ TPackageUpdater }

class procedure TPackageUpdater.commit(conn: TFDBConnection; pck: TBytes; npm : TNpmPackage; date : TFslDateTime; guid, id, version, description, canonical, token: String; urls : TStringList; kind: TFHIRPackageKind);
var
  fver, dep, u : String;
  pkey, vkey, cvkey, cc : integer;
begin
  vkey := conn.CountSQL('Select Max(PackageVersionKey) from PackageVersions') +1;
  conn.SQL := 'Insert into PackageVersions '+
    '(PackageVersionKey, GUID, PubDate, Indexed, Id, Version, Kind, DownloadCount, Canonical, FhirVersions, UploadCount, Description, ManualToken, Hash, Content) values ('+
    inttostr(vkey)+', '''+SQLWrapString(guid)+''', :d, '+DBGetDate(conn.dialect)+', '''+SQLWrapString(id)+''', '''+SQLWrapString(version)+''', '''+inttostr(ord(kind))+''', 0, :u, :f, 1, :desc, :mt, :hash, :c)';
  conn.prepare;
  conn.BindDateTimeEx('d', date);
  conn.BindString('u', canonical);
  conn.BindString('f', npm.fhirVersionList);
  conn.BindBlobFromString('desc', description);
  conn.BindString('mt', token);
  conn.BindString('hash', genHash(pck));
  conn.BindBlob('c', pck);
  conn.Execute;
  conn.Terminate;

  for fver in npm.fhirVersionList.Split([',']) do
    conn.ExecSQL('Insert into PackageFHIRVersions (PackageVersionKey, Version) values ('+inttostr(vkey)+', '''+SQLWrapString(fver)+''')');
  for dep in npm.dependencies do
    conn.ExecSQL('Insert into PackageDependencies (PackageVersionKey, Dependency) values('+inttostr(vkey)+', '''+SQLWrapString(dep)+''')');
  for u in urls do
    conn.ExecSQL('Insert into PackageURLs (PackageVersionKey, URL) values('+inttostr(vkey)+', '''+SQLWrapString(u)+''')');

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
    cc := conn.CountSQL('Select count(PackageVersionKey) from PackageVersions where Id = '''+SQLWrapString(id)+''' and Version <> ''current''');
    cvkey := conn.CountSQL('Select PackageVersionKey from PackageVersions where Id = '''+SQLWrapString(id)+''' and Version <> ''current'' order by PubDate desc, Version');
    if (cvkey = vkey) or (cc = 1) then    // if we aded the most recent
    begin
      conn.SQL := 'Update Packages set Canonical = '''+SQLWrapString(canonical)+''', CurrentVersion = '+inttostr(vkey)+' where PackageKey = '+inttostr(pkey);
      conn.prepare;
      conn.Execute;
      conn.Terminate;
    end;
  end;
end;

constructor TPackageUpdater.Create(zulip: TZulipTracker);
begin
  inherited Create;
  FZulip := zulip;
end;

destructor TPackageUpdater.Destroy;
begin
  FCrawlerLog.free;
  FZulip.free;
  inherited;
end;

procedure TPackageUpdater.clog(s: String; mode: TCrawlerLogMode);
begin
  case mode of
    clmStart: FCrawlerLog.append('<p>'+FormatTextToHTML(s)+'</p>'#13#10);
    clmHeader: FCrawlerLog.append('<p><b>'+FormatTextToHTML(s)+'</b></p>'#13#10);
    clmError:
      begin
        FCrawlerLog.append('<li style="color: Maroon">'+FormatTextToHTML(s)+'</li>'#13#10);
        FHasErrors := true;
      end;
    clmWarning: FCrawlerLog.append('<li style="color: Navy">'+FormatTextToHTML(s)+'</li>'#13#10);
    clmNote: FCrawlerLog.append('<li style="color: Black">'+FormatTextToHTML(s)+'</li>'#13#10);
  end;
end;

procedure TPackageUpdater.DoSendEmail(dest, subj, body: String);
var
  dt : TDateTime;
begin
  dt := FIni.ReadDate('sent', dest, 0);
  if dt < now - EMAIL_DAYS_LIMIT then
  begin
    FIni.WriteDate('sent', dest, now);
    FOnSendEmail(dest, subj, body);
  end;
end;

function TPackageUpdater.fetchJson(url: string): TJsonObject;
begin
  if Logging.shuttingDown then
    Abort;
  result := TJSONParser.Parse(fetchUrl(url, 'application/json'));
end;

function TPackageUpdater.fetchUrl(url, mimetype: string): TBytes;
var
  fetcher : TInternetFetcher;
begin
  if Logging.shuttingDown then
    Abort;
  fetcher := TInternetFetcher.Create;
  try
    fetcher.URL := url+'?'+TFslDateTime.makeLocal().toHL7;
    // fetcher.Accept := mimetype;
    fetcher.userAgent := 'HealthIntersections/FhirServer';
    fetcher.Fetch;
    result := fetcher.Buffer.AsBytes;
  finally
    fetcher.free;
  end;
  FTotalBytes := FTotalBytes + length(result);
end;

function TPackageUpdater.fetchXml(url: string): TMXmlElement;
begin
  if Logging.shuttingDown then
    Abort;
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

procedure TPackageUpdater.SetCrawlerLog(AValue: TFslStringBuilder);
begin
  FCrawlerLog.free;
  FCrawlerLog:=AValue;
end;

procedure TPackageUpdater.log(msg, source: String; error : boolean);
begin
  if error then
  begin
    FErrors := FErrors + msg+' (from '+source+')'+#13#10;
    FFeedErrors := FFeedErrors + msg+' (from '+source+')'+#13#10;
    if FZulip <> nil then
      FZulip.error(msg+' (from '+source+')');
  end;
  Logging.log(msg);
end;

class procedure TPackageUpdater.processURLs(npm : TNpmPackage; ts : TStringList);
var
  s : String;
  json : TJsonObject;
  xml : TMXmlDocument;
  e, u : TMXmlElement;
  bytes : TBytes;
begin
  for s in npm.list('package') do
  begin
    try
      if s.EndsWith('.json') then
      begin
        bytes := npm.loadBytes('package', s);
        json := TJSONParser.parse(bytes);
        try
          if json.has('url') then
            ts.Add(json.str['url']);
        finally
          json.free;
        end;
      end
      else if s.EndsWith('.xml') then
      begin
        bytes := npm.loadBytes('package', s);
        xml := TMXmlParser.parse(bytes, [xpResolveNamespaces, xpDropWhitespace, xpDropComments, xpHTMLEntities]);
        try
          e := xml.docElement;
          u := e.element('url');
          if u <> nil then
            ts.Add(u.attribute['value']);
        finally
          e.free;
        end;
      end
    except
      on e : Exception do
      begin
        BytesToFile(bytes, '/Users/grahamegrieve/temp/content.bin');
        Logging.log('Error processing '+npm.name+'#'+npm.version+'/package/'+s+': '+e.Message);
      end;
    end;
  end;
end;

procedure TPackageUpdater.store(source, url, guid: String; date : TFslDateTime; package: Tbytes; idver : String);
var
  npm : TNpmPackage;
  id, version, description, canonical, fhirVersion : String;
  kind : TFHIRPackageKind;
  ts : TStringList;
begin
  if Logging.shuttingDown then
    Abort;
  npm := TNpmPackage.fromPackage(package, source+'#'+guid, nil, true);
  try
    id := npm.name;
    version := npm.version;
    if (id+'#'+version <> idver) then
    begin
      log('Warning processing '+idver+': actually found '+id+'#'+version+' in the package', source, true);
      clog(idver+': actually found '+id+'#'+version+' in the package', clmWarning);
    end;

    description := npm.description;
    kind := npm.kind;
    canonical := npm.canonical;
    if npm.notForPublication then
    begin
      log('Warning processing '+idver+': this package is not suitable for publication (likely broken links)', source, true);
      clog(idver+': not suitable for publication (likely broken links)', clmWarning);
    end;
    fhirVersion := npm.fhirVersion;
    if not isValidPackageId(id) then
      raise EPackageCrawlerException.Create('NPM Id "'+id+'" is not valid from '+source);
    if not TSemanticVersion.isValid(version) then
      raise EPackageCrawlerException.Create('NPM Version "'+version+'" is not valid from '+source);
    if (canonical = '') then
    begin
      log('Warning processing '+idver+': No canonical found in npm (from '+url+')', source, true);
      clog(idver+': No canonical found in npm (from '+url+')', clmWarning);
      canonical := 'http://simplifier.net/packages/fictitious/'+id;
    end;
    if not isAbsoluteUrl(canonical) then
      raise EPackageCrawlerException.Create('NPM Canonical "'+canonical+'" is not valid from '+source);

    ts := TStringList.Create;
    try
      processURLs(npm, ts);
      
      commit(FDB, package, npm, date, guid, id, version, description, canonical, '', ts, kind);
    finally
      ts.free;
    end;

  finally
    npm.free;
  end;
end;

class procedure TPackageUpdater.test(db: TFDBManager);
var
  conn : TFDBConnection;
  this : TPackageUpdater;
begin
  conn := db.GetConnection('test');
  try
    this := TPackageUpdater.Create(nil);
    try
      this.update(conn);
    finally
      this.free;
    end;
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
  FIni := TIniFile.Create(tempFile('package-spider.ini'));
  try
    log('Start Package Scan', '', false);
    FTotalBytes := 0;
    FErrors := '';
    FDB := DB;
    try
      log('Fetch '+MASTER_URL, '', false);
      clog('Master URL: '+MASTER_URL, clmStart);
      json := fetchJson(MASTER_URL);
      try
        pr := TPackageRestrictions.Create(json.arr['package-restrictions'].Link);
        try
          arr := json.arr['feeds'];
          for i := 0 to arr.Count - 1 do
            updateTheFeed(fix(arr.Obj[i].str['url']), MASTER_URL, arr.Obj[i].str['errors'].Replace('|', '@').Replace('_', '.'), pr);
        finally
          pr.free;
        end;
      finally
        json.free;
      end;
      clog('', clmHeader);
    except
      on e : EAbort do
      begin
        Log('Terminate Package Spider - shutting down', MASTER_URL, true)
      end;
      on e : Exception do
      begin
        Log('Exception Processing Registry: '+e.Message, MASTER_URL, true)
      end;
    end;
    //try
    //  log('Fetch '+MANUAL_REG_URL, '', false);
    //  json := fetchJson(MANUAL_REG_URL);
    //  try
    //    pr := TPackageRestrictions.Create(json.arr['package-restrictions'].Link);
    //    try
    //      arr := json.arr['feeds'];
    //      for i := 0 to arr.Count - 1 do
    //        updateTheFeed(fix(arr.Obj[i].str['url']), MASTER_URL, arr.Obj[i].str['errors'].Replace('|', '@').Replace('_', '.'), pr);
    //    finally
    //      pr.free;
    //    end;
    //  finally
    //    json.free;
    //  end;
    //except
    //  on e : Exception do
    //  begin
    //    Log('Exception Processing Registry: '+e.Message, MASTER_URL, true)
    //  end;
    //end;
    if FZulip <> nil then
      FZulip.send;
    log('Finish Package Scan - '+Logging.DescribeSize(FTotalBytes, 0), '', false);
  finally
    FIni.free;
  end;
end;

procedure TPackageUpdater.updateTheFeed(url, source, email: String; pr : TPackageRestrictions);
var
  xml : TMXmlElement;
  channel : TMXmlElement;
  item : TMXmlElement;
  i : integer;
begin
  if Logging.shuttingDown then
    Abort;
  try
    clog('Process '+url, clmHeader);
    FCrawlerLog.append('<ul>'#13#10);
    log('Fetch '+url, source, false);
    FFeedErrors := '';
    FHasErrors := false;

    xml := fetchXml(url);
    try
      for channel in xml.first.Children do
      begin
        if (channel.Name = 'channel') then
        begin
          i := 0;
          for item in channel.Children do
          begin
            if (item.Name = 'item') then
            begin
              updateItem(url, item, i, pr);
              inc(i);
            end;
          end;
        end;
      end;
    finally
      xml.free;
    end;
    if not FHasErrors then
      clog('All OK', clmNote);
    if (FFeedErrors <> '') and (email <> '') then
        DoSendEmail(email, 'Errors Processing '+url, FFeedErrors);
    FCrawlerLog.append('</ul>'#13#10);
  except
    on e : Exception do
    begin                                                   
      clog('Exception: '+e.Message, clmError);
      FCrawlerLog.append('</ul>'#13#10);
      log('Exception processing feed: '+url+': '+e.Message, source, false);
      if (email <> '') then
        DoSendEmail(email, 'Exception Processing '+url, e.Message);
    end;
  end;
end;

procedure TPackageUpdater.updateItem(source : String; item: TMXmlElement; i : integer; pr : TPackageRestrictions);
var
  guid : String;
  content : TBytes;
  date : TFslDateTime;
  id, url, d, list: String;
begin
  if Logging.shuttingDown then
    Abort;
  url := '[link not found]';
  if item.element('guid') = nil then
  begin
    log('Error processing item from '+source+'#item['+inttostr(i)+']: no guid provided', source, true);
    clog('item['+inttostr(i)+']: no guid provided', clmError);
    exit;
  end;
  guid := item.element('guid').Text;
  try
    id := item.element('title').Text;
    if (item.element('notForPublication') <> nil) and ('true' = item.element('notForPublication').text) then
    begin
      clog(guid+': not for publication', clmError);
      exit;
    end;
    if pr.isOk(id, source, list) then
    begin
      if (not hasStored(guid)) then
      begin
        d := item.element('pubDate').Text.toLower.Replace('  ', ' ');
        if (d.substring(0, 6).contains(',')) then
          d := d.substring(d.indexOf(',')+1)
        else if StringStartsWith(d, ['mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun']) then
          d := d.substring(d.indexOf(' ')+1);
        d := d.trim;
        if (d.length > 2) and (d[2] = ' ') and StringIsInteger16(d[1]) then
          d := '0'+d;
        try
          date := TFslDateTime.fromFormat('dd mmm yyyy hh:nn:ss', d);
        except
          date := TFslDateTime.fromFormat('dd mmmm yyyy hh:nn:ss', d);
        end;
        url := fix(item.element('link').Text);
        log('Fetch '+url, source, false);
        content := fetchUrl(url, 'application/tar+gzip');
        store(source, url, guid, date, content, id);
        clog(guid+': Fetched '+url, clmNote);
      end;
    end
    else
    begin
      if not (source.contains('simplifier.net')) then
      begin
        log('The package '+id+' is not allowed to come from '+source+' (allowed: '+list+')', source, true);
        clog(guid+': The package '+id+' is not allowed to come from '+source+' (allowed: '+list+')', clmError);
      end;
    end;
  except
    on e : Exception do
    begin
      log('Exception processing item: '+guid+' from '+url+': '+e.Message, source, true);
      clog(guid+': '+e.Message, clmError);
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
  FJson.free;
  inherited;
end;

function TPackageRestrictions.isOk(package, feed: String; var list : String): boolean;
var
  e, f : TJsonNode;
  eo : TJsonObject;
  p, m, ff, v : String;
begin
  result := true;
  list := '';
  if FJson <> nil then
  begin
    for e in FJson do
    begin
      eo := e as TJsonObject;
      p := fix(package);
      m := fix(eo.str['mask']);
      if matches(p, m) then
      begin
        result := false;
        for f in eo.arr['feeds'] do
        begin
          ff := fix(feed);
          v := fix(TJsonString(f).value);
          result := result or (ff = v);
          CommaAdd(list, TJsonString(f).value);
        end;
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

{ TZulipTracker }

constructor TZulipTracker.Create(address, email, apikey: String);
begin
  inherited Create;
  FZulip := TZulipSender.Create(address, email, apikey);
  FErrors := TFslMap<TZulipItem>.Create;
end;

destructor TZulipTracker.Destroy;
begin
  FErrors.free;
  FZulip.free;
  inherited;
end;

procedure TZulipTracker.error(err: String);
begin
  if not FErrors.ContainsKey(err) then
    FErrors.Add(err, TZulipItem.create);
end;

function TZulipTracker.Link: TZulipTracker;
begin
  result := TZulipTracker(inherited link);
end;

procedure TZulipTracker.send;
var
  msg : String;
  s : string;
  item : TZulipItem;
begin
  msg := '';
  for s in Ferrors.Keys do
  begin
    item := FErrors[s];
    if item.FWhen < now - 1 then
    begin
      msg := msg + '* '+s+#13#10;
      item.FWhen := now;
    end;
  end;
  if msg <> '' then
  begin
    Logging.log('Send to Zulip: '+msg);
    FZulip.sendMessage('tooling/Package Crawlers', 'Packages2', msg);
  end;
end;

function genHash(bytes : TBytes) : String;
var
  hash : TIdHashSHA1;
begin
  hash := TIdHashSHA1.Create;
  try
    {$IFDEF DELPHI}
    raise EFslException.Create('Not Implemented Yet');
    {$ELSE}
    result := hash.HashBytesAsHex(bytes);
    {$ENDIF}
  finally
    hash.free;
  end;
end;


end.
