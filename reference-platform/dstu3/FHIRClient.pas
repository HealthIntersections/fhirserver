unit FHIRClient;

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

{$IFNDEF FHIR3}
This is the dstu3 version of the FHIR code
{$ENDIF}


interface

uses
  SysUtils, Classes, Generics.Collections, Soap.EncdDecd,
  IdHTTP, IdSSLOpenSSL, IdComponent,
  StringSupport, EncodeSupport, GuidSupport, DateSupport, MimeMessage,
  AdvObjects, AdvBuffers, {$IFNDEF OSX}AdvWinInetClients, {$ENDIF}AdvJson,
  FHIRParser, FHIRResources, FHIRTypes, FHIRUtilities,
  FHIRConstants, FHIRContext, FHIRSupport, FHIRParserBase, FHIRBase,
  SmartOnFhirUtilities, CdsHooksUtilities;

Type
  EFHIRClientException = class (Exception)
  private
    FIssue : TFhirOperationOutcome;
  public
    constructor Create(message : String; issue : TFhirOperationOutcome);
    destructor Destroy; override;

    property issue : TFhirOperationOutcome read FIssue;
  end;

  TFhirClient = {abstract} class (TAdvObject)
  private
    FOnWork: TWorkEvent;
  public
    function link : TFhirClient; overload;

    function conformance(summary : boolean) : TFhirCapabilityStatement; virtual;
    function transaction(bundle : TFHIRBundle) : TFHIRBundle; virtual;
    function createResource(resource : TFhirResource; var id : String) : TFHIRResource; virtual;
    function readResource(atype : TFhirResourceType; id : String) : TFHIRResource; virtual;
    function updateResource(resource : TFhirResource) : TFHIRResource; overload; virtual;
    procedure deleteResource(atype : TFhirResourceType; id : String); virtual;
    function search(allRecords : boolean; params : TDictionary<String, String>) : TFHIRBundle; overload; virtual;
    function search(atype : TFhirResourceType; allRecords : boolean; params : TDictionary<String, String>) : TFHIRBundle; overload; virtual;
    function search(atype : TFhirResourceType; allRecords : boolean; params : string) : TFHIRBundle; overload; virtual;
    function searchPost(atype : TFhirResourceType; allRecords : boolean; params : TDictionary<String, String>; resource : TFhirResource) : TFHIRBundle; virtual;
    function searchAgain(link : String) : TFHIRBundle; overload; virtual;
    function operation(atype : TFhirResourceType; opName : String; params : TFhirParameters) : TFHIRResource; virtual;
    function historyType(atype : TFhirResourceType; allRecords : boolean; params : TDictionary<String, String>) : TFHIRBundle; virtual;

    function address : String; virtual;
    function format : TFHIRFormat; virtual;
    property OnWork: TWorkEvent read FOnWork write FOnWork;
  end;

  TFhirHTTPClientHTTPVerb = (get, post, put, delete, options, patch);

  TFhirHTTPClientStatusEvent = procedure (client : TObject; details : String) of Object;

  // this is meant ot be used once, and then disposed of
  TFhirHTTPClient = class (TFhirClient)
  private
    FUrl : String;
    FJson : Boolean;
    {$IFNDEF OSX}
    http : TAdvWinInetClient;
    {$ENDIF}
    indy : TIdHTTP;
    ssl : TIdSSLIOHandlerSocketOpenSSL;
    FOnClientStatus : TFhirHTTPClientStatusEvent;
    FSmartToken: TSmartOnFhirAccessToken;
    FTimeout: cardinal;
    FUseIndy: boolean;
    FWorker : TFHIRWorkerContext;
    FAllowR2: boolean;
    FCertPWord: String;

    FCertFile: String;
    FProxy: String;
//    FLastUpdated : TDateTimeEx;
    procedure status(msg : String);
    procedure getSSLpassword(var Password: String);
    function serialise(resource : TFhirResource):TStream; overload;
    function makeUrl(tail : String; params : TDictionary<String, String> = nil) : String;
    function makeUrlPath(tail : String) : String;
    function CreateParser(stream : TStream) : TFHIRParser;
    function exchange(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; ct : String = '') : TStream;
    function fetchResource(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; ct : String = '') : TFhirResource;
    function makeMultipart(stream: TStream; streamName: string; params: TDictionary<String, String>; var mp : TStream) : String;
    procedure SetSmartToken(const Value: TSmartOnFhirAccessToken);
    procedure SetTimeout(const Value: cardinal);
    procedure createClient;
    procedure setHeader(name, value : String);
    function GetHeader(name : String) : String;
    function exchangeIndy(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; ct: String): TStream;
    {$IFNDEF OSX}
    function exchangeHTTP(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; ct: String): TStream;
    {$ENDIF}

    function authoriseByOWinIndy(server, username, password : String): TJsonObject;
    function authoriseByOWinHttp(server, username, password : String): TJsonObject;
    procedure SetCertFile(const Value: String);
    procedure SetCertPWord(const Value: String);
    procedure SetUseIndy(const Value: boolean);  protected
    function Convert(stream : TStream) : TStream; virtual;
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    constructor Create(worker : TFHIRWorkerContext; url : String; json : boolean); overload;
    destructor Destroy; override;
    property url : String read FUrl;

    property Json : boolean read FJson write FJson;

    function link : TFhirHTTPClient; overload;
    property smartToken : TSmartOnFhirAccessToken read FSmartToken write SetSmartToken;
    property timeout : cardinal read FTimeout write SetTimeout;
    property UseIndy : boolean read FUseIndy write SetUseIndy; // set this to true for a service, but you may have problems with SSL
    property allowR2 : boolean read FAllowR2 write FAllowR2;
    property certFile : String read FCertFile write SetCertFile;
    property certPWord : String read FCertPWord write SetCertPWord;
    property proxy : String read FProxy write FProxy;

//    procedure doRequest(request : TFHIRRequest; response : TFHIRResponse);
    procedure cancelOperation;

    function address : String; override;
    function format : TFHIRFormat; override;

    function conformance(summary : boolean) : TFhirCapabilityStatement; override;
    function transaction(bundle : TFHIRBundle) : TFHIRBundle; override;
    function createResource(resource : TFhirResource; var id : String) : TFHIRResource; override;
    function readResource(atype : TFhirResourceType; id : String) : TFHIRResource; override;
    function updateResource(resource : TFhirResource) : TFHIRResource; overload; override;
    procedure deleteResource(atype : TFhirResourceType; id : String); override;
    function search(allRecords : boolean; params : TDictionary<String, String>) : TFHIRBundle; overload; override;
    function search(atype : TFhirResourceType; allRecords : boolean; params : TDictionary<String, String>) : TFHIRBundle; overload; override;
    function search(atype : TFhirResourceType; allRecords : boolean; params : string) : TFHIRBundle; overload; override;
    function searchPost(atype : TFhirResourceType; allRecords : boolean; params : TDictionary<String, String>; resource : TFhirResource) : TFHIRBundle; override;
    function searchAgain(link : String) : TFHIRBundle; overload; override;
    function operation(atype : TFhirResourceType; opName : String; params : TFhirParameters) : TFHIRResource; override;
    function historyType(atype : TFhirResourceType; allRecords : boolean; params : TDictionary<String, String>) : TFHIRBundle; override;
    function cdshook(id : String; request : TCDSHookRequest) : TCDSHookResponse;

    //authorization support
    procedure authoriseByOWin(server, username, password : String);

    property OnClientStatus : TFhirHTTPClientStatusEvent read FOnClientStatus write FOnClientStatus;
  end;

implementation

uses
  TextUtilities;

{ EFHIRClientException }

constructor EFHIRClientException.create(message: String; issue: TFhirOperationOutcome);
begin
  inherited create(message);
  FIssue := issue;
  end;

destructor EFHIRClientException.destroy;
begin
  FIssue.Free;
  inherited;
end;

{ TFhirHTTPClient }

function TFhirHTTPClient.Convert(stream: TStream): TStream;
var
  s : String;
begin
  if FALlowR2 then
  begin
    s := StreamToString(stream, TEncoding.UTF8);
    if s.Contains('<Conformance') then
    begin
      s := s.Replace('<Conformance', '<CapabilityStatement');
      s := s.Replace('</Conformance', '</CapabilityStatement');
      s := s.Replace('"Conformance"', '"CapabilityStatement"');
      s := s.Replace('"DiagnosticOrder"', '"DiagnosticRequest"');
    end;
    result := TStringStream.Create(s, TEncoding.UTF8)
  end
  else
    result := stream;
end;

constructor TFhirHTTPClient.create(worker : TFHIRWorkerContext; url: String; json : boolean);
begin
  Create;
  FWorker := worker;
  FUrl := URL;
  FJson := json;
  {$IFNDEF WINDOWS}
  FUseIndy := true;
  {$ENDIF}
end;

destructor TFhirHTTPClient.destroy;
begin
  FWorker.Free;
  FSmartToken.Free;
  ssl.Free;
  indy.free;
  {$IFNDEF OSX}
  http.Free;
  {$ENDIF}
  inherited;
end;


procedure TFhirHTTPClient.DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if assigned(OnWork) then
    OnWork(Self, AWorkMode, AWorkCount);
end;

function TFhirHTTPClient.link: TFhirHTTPClient;
begin
  result := TFhirHTTPClient(inherited Link);
end;

function TFhirHTTPClient.conformance(summary : boolean): TFhirCapabilityStatement;
var
  params : TDictionary<String, String>;
begin
  params := TDictionary<String, String>.create;
  try
    if summary then
      params.Add('_summary', 'true');
    result := FetchResource(MakeUrl('metadata', params), get, nil) as TFhirCapabilityStatement;
  finally
    params.Free;
  end;
end;


function TFhirHTTPClient.transaction(bundle : TFHIRBundle) : TFHIRBundle;
Var
  src : TStream;
begin
  src := serialise(bundle);
  try
    result := fetchResource(makeUrl(''), post, src) as TFhirBundle;
  finally
    src.free;
  end;
end;


function readIdFromLocation(resType, location : String) : String;
var
  a : TArray<String>;
begin
  a := location.split(['/']);
  if length(a) < 2 then
    raise Exception.Create('Unable to process location header (too short)');
  if a[length(a)-2] = '_history' then
  begin
    if length(a) < 4 then
      raise Exception.Create('Unable to process location header (too short for a version specific location). Location: '+location);
    if a[length(a)-4] <> resType  then
      raise Exception.Create('Unable to process location header (version specific, but resource doesn''t match). Location: '+location);
    result := a[length(a)-3]; // 1 for offset, 2 for _history and vers
  end
  else if a[length(a)-2] <> resType then
    raise Exception.Create('Unable to process location header (resource doesn''t match). Location: '+location);
  result := a[length(a)-1];
end;

function TFhirHTTPClient.createResource(resource: TFhirResource; var id : String): TFHIRResource;
Var
  src : TStream;
begin
  src := serialise(resource);
  try
    result := nil;
    try
      result := fetchResource(MakeUrl(CODES_TFhirResourceType[resource.resourceType]), post, src);
      id := readIdFromLocation(CODES_TFhirResourceType[resource.resourceType], getHeader('Location'));
      result.link;
    finally
      result.free;
    end;
  finally
    src.free;
  end;
end;

function TFhirHTTPClient.updateResource(resource : TFhirResource) : TFHIRResource;
Var
  src : TStream;
begin
  if (resource.meta <> nil) and (resource.meta.versionId <> '') then
    SetHeader('Content-Location', MakeUrlPath(CODES_TFhirResourceType[resource.resourceType]+'/'+resource.id+'/history/'+resource.meta.versionId));

  src := serialise(resource);
  try
      result := fetchResource(MakeUrl(CODES_TFhirResourceType[resource.resourceType]+'/'+resource.id), put, src);
  finally
    src.free;
  end;
end;

procedure TFhirHTTPClient.deleteResource(atype : TFhirResourceType; id : String);
begin
  exchange(MakeUrl(CODES_TFhirResourceType[aType]+'/'+id), delete, nil).free;
end;

//-- Worker Routines -----------------------------------------------------------


function TFhirHTTPClient.serialise(resource: TFhirResource): TStream;
var
  ok : boolean;
  comp : TFHIRComposer;
begin
  ok := false;
  result := TBytesStream.create;
  try
    if Fjson then
      comp := TFHIRJsonComposer.create(FWorker.link, 'en')
    else
      comp := TFHIRXmlComposer.create(FWorker.link, 'en');
    try
      comp.Compose(result, resource, false, nil);
    finally
      comp.free;
    end;
    result.position := 0;
    ok := true;
  finally
    if not ok then
      result.free;
  end;
end;

procedure TFhirHTTPClient.SetCertFile(const Value: String);
begin
  FCertFile := Value;
  indy.free;
  indy := nil;
  {$IFNDEF OSX}
  http.Free;
  http := nil;
  {$ENDIF}
end;

procedure TFhirHTTPClient.SetCertPWord(const Value: String);
begin
  FCertPWord := Value;
  indy.free;
  indy := nil;
  {$IFNDEF OSX}
  http.Free;
  http := nil;
  {$ENDIF}
end;

procedure TFhirHTTPClient.SetSmartToken(const Value: TSmartOnFhirAccessToken);
begin
  FSmartToken.Free;
  FSmartToken := Value;
  // todo: set the header for the access token
end;

procedure TFhirHTTPClient.status(msg: String);
begin
  if assigned(FOnClientStatus) then
    FOnClientStatus(self, msg);
end;

function encodeParams(params : TDictionary<String, String>) : String;
var
  s : String;
begin
  result := '';
  for s in params.Keys do
    result := result + s+'='+EncodeMIME(params[s])+'&';
end;

function TFhirHTTPClient.search(atype: TFhirResourceType; allRecords: boolean; params: TDictionary<String, String>): TFHIRBundle;
begin
  result := search(atype, allrecords, encodeParams(params));
end;

function TFhirHTTPClient.search(atype: TFhirResourceType; allRecords: boolean; params: string): TFHIRBundle;
var
  s : String;
  feed : TFHIRBundle;
begin
//    client.Request.RawHeaders.Values['Content-Location'] := MakeUrlPath(CODES_TFhirResourceType[resource.resourceType]+'/'+id+'/history/'+ver);
  result := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'?'+params, get, nil) as TFHIRBundle;
  try
    s := result.links['next'];
    while AllRecords and (s <> '') do
    begin
      feed := fetchResource(s, get, nil) as TFhirBundle;
      try
        result.entryList.AddAll(feed.entryList);
        s := feed.links['next'];
      finally
        feed.free;
      end;
    end;
    if allRecords then
      result.link_List.Clear;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFhirHTTPClient.searchAgain(link: String): TFHIRBundle;
begin
  result := fetchResource(link, get, nil) as TFHIRBundle;
end;

function TFhirHTTPClient.search(allRecords: boolean; params: TDictionary<String, String>): TFHIRBundle;
begin
  result := search(frtNull, allrecords, encodeParams(params));
end;

function TFhirHTTPClient.searchPost(atype: TFhirResourceType; allRecords: boolean; params: TDictionary<String, String>; resource: TFhirResource): TFHIRBundle;
var
  src, frm : TStream;
  ct : String;
begin
  src := serialise(resource);
  try
    src.Position := 0;
    ct := makeMultipart(src, 'src', params, frm);
    try
      result := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'/_search', post, frm) as TFhirBundle;
    finally
      frm.Free;
    end;
  finally
    src.free;
  end;
end;


function TFhirHTTPClient.operation(atype : TFhirResourceType; opName : String; params : TFhirParameters) : TFHIRResource;
Var
  src : TStream;
begin
  src := serialise(params);
  try
    src.Position := 0;
    if aType = frtNull then
      result := fetchResource(makeUrl('$'+opName), post, src)
    else
    result := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'/$'+opName, post, src);
  finally
    src.free;
  end;
end;

function TFhirHTTPClient.fetchResource(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; ct : String = ''): TFhirResource;
var
  ret, conv : TStream;
  p : TFHIRParser;
begin
  ret := exchange(url, verb, source, ct);
  try
    if ret.Size = 0 then
      result := nil
    else
    begin
      conv := Convert(ret);
      try
        p := CreateParser(conv);
        try
          p.parse;
          if (p.resource = nil) then
            raise Exception.create('No response bundle');
          result := p.resource.link;
        finally
          p.free;
        end;
      finally
        if (conv <> ret) then
          conv.free;
      end;
    end;
  finally
    ret.free;
  end;
end;

function TFhirHTTPClient.format: TFHIRFormat;
begin
  if Json then
    result := ffJson
  else
    result := ffXml;
end;

function TFhirHTTPClient.makeMultipart(stream: TStream; streamName: string; params: TDictionary<String, String>; var mp : TStream) : String;
var
  m : TMimeMessage;
  p : TMimePart;
  s : String;
begin
  m := TMimeMessage.create;
  try
    p := m.AddPart(NewGuidURN);
    p.ContentDisposition := 'form-data; name="'+streamName+'"';
    p.Content.LoadFromStream(stream);
    for s in params.Keys do
    begin
      p := m.AddPart(NewGuidURN);
      p.ContentDisposition := 'form-data; name="'+s+'"';
      p.Content.AsBytes := TEncoding.UTF8.GetBytes(params[s]);
    end;
    m.Boundary := '---'+AnsiString(copy(GUIDToString(CreateGUID), 2, 36));
    m.start := m.parts[0].Id;
    result := 'multipart/form-data; boundary='+String(m.Boundary);
    mp := TMemoryStream.Create;
    m.WriteToStream(mp, false);
  finally
    m.free;
  end;
end;

function TFhirHTTPClient.makeUrl(tail: String; params : TDictionary<String, String> = nil): String;
begin
  result := FURL;
  if not result.EndsWith('/') and (tail <> '') then
    result := result + '/';
  result := result + tail;
  if params <> nil then
    result := result + '?' + encodeParams(params);
end;

function TFhirHTTPClient.makeUrlPath(tail: String): String;
var
  s : String;
begin
  StringSplit(FURL, '://', s, result);
  StringSplit(result, '://', s, result);
  if not result.EndsWith('/') then
    result := result + '/';
  result := result + tail;
end;

procedure StringSplitTrim(Const sValue, sDelimiter : String; Var sLeft, sRight: String);
begin
  StringSplit(sValue, sDelimiter, sLeft, sRight);
  sLeft := trim(sLeft);
  sRight := trim(sRight);
end;

function TFhirHTTPClient.readResource(atype: TFhirResourceType; id: String): TFHIRResource;
begin

  result := nil;
  try
    result := fetchResource(MakeUrl(CODES_TFhirResourceType[AType]+'/'+id), get, nil);
    result.link;
  finally
    result.free;
  end;
end;

function TFhirHTTPClient.CreateParser(stream: TStream): TFHIRParser;
begin
  if FJSon then
    result := TFHIRJsonParser.create(FWorker.Link, 'en')
  else
    result := TFHIRXmlParser.create(FWorker.Link, 'en');
  result.source := stream;
end;

function TFhirHTTPClient.historyType(atype: TFhirResourceType; allRecords: boolean; params: TDictionary<String, String>): TFHIRBundle;
var
  s : String;
  feed : TFHIRBundle;
  i : integer;
begin
//    client.Request.RawHeaders.Values['Content-Location'] := MakeUrlPath(CODES_TFhirResourceType[resource.resourceType]+'/'+id+'/history/'+ver);
  status('Fetch History for '+CODES_TFhirResourceType[aType]);
  result := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'/_history?'+encodeParams(params), get, nil) as TFhirBundle;
  try
    s := result.links['next'];
    i := 1;
    while AllRecords and (s <> '') do
    begin
      inc(i);
      status('Fetch History for '+CODES_TFhirResourceType[aType]+' page '+inttostr(i));
      feed := fetchResource(s, get, nil) as TFhirBundle;
      try
        result.entryList.AddAll(feed.entryList);
        s := feed.links['next'];
      finally
        feed.free;
      end;
    end;
    if allRecords then
      result.link_List.Clear;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFhirHTTPClient.createClient;
begin
  if FUseIndy then
  begin
    if (indy = nil) then
    begin
      indy := TIdHTTP.create(nil);
      indy.HandleRedirects := true;
      indy.OnWork := DoWork;
      if (proxy <> '') then
      begin
        try
          indy.ProxyParams.ProxyServer := proxy.Split([':'])[0];
          indy.ProxyParams.ProxyPort := StrToInt(proxy.Split([':'])[1]);
        except
          raise Exception.Create('Unabel to process proxy "'+proxy+'" - use address:port');
        end;
      end;
      ssl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      indy.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      ssl.SSLOptions.SSLVersions := [sslvTLSv1_2];
      if certFile <> '' then
      begin
        ssl.SSLOptions.CertFile := certFile;
        ssl.SSLOptions.KeyFile := ChangeFileExt(certFile,'.key');
        ssl.OnGetPassword := getSSLpassword;
      end;
    end;
  end
  {$IFNDEF OSX}
  else if http = nil then
  begin
    if certFile <> '' then
      raise Exception.Create('Certificates are not supported with winInet yet'); // have to figure out how to do that ...
    http := TAdvWinInetClient.Create;
    http.UseWindowsProxySettings := true;
    http.UserAgent := 'FHIR Client';
  end;
  {$ENDIF}
end;

procedure TFhirHTTPClient.setHeader(name, value: String);
begin
  createClient;
  if FUseIndy then
    indy.Request.RawHeaders.Values[name] := value
  {$IFNDEF OSX}
  else
    http.Headers.AddOrSetValue(name, value);
  {$ENDIF}
end;

function TFhirHTTPClient.GetHeader(name: String): String;
begin
  createClient;
  if FUseIndy then
    result := indy.Response.RawHeaders.Values[name]
  {$IFNDEF OSX}
  else
    result := http.getResponseHeader(name);
  {$ENDIF}
end;


procedure TFhirHTTPClient.getSSLpassword(var Password: String);
begin
  Password := FCertPWord;
end;

function TFhirHTTPClient.exchange(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; ct : String = '') : TStream;
begin
  createClient;
  if FUseIndy then
    result := exchangeIndy(url, verb, source, ct)
  {$IFNDEF OSX}
  else
    result := exchangeHTTP(url, verb, source, ct)
  {$ENDIF}
end;

{$IFNDEF OSX}
function TFhirHTTPClient.exchangeHTTP(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; ct: String): TStream;
var
  ok : boolean;
  op : TFHIROperationOutcome;
  code : integer;
  procedure processException;
  var
    cnt : string;
    comp : TFHIRParser;
  begin
    cnt := http.Response.AsUnicode;
    if StringFind(cnt, 'OperationOutcome') > 0 then
    begin
      removeBom(cnt);
      if FJson then
        comp := TFHIRJsonParser.create(FWorker.Link, 'en')
      else
        comp := TFHIRXmlParser.create(FWorker.Link, 'en');
      try
        comp.source := TBytesStream.create(http.response.AsBytes);
        comp.Parse;
        if (comp.resource <> nil) and (comp.resource.ResourceType = frtOperationOutcome) then
        begin
          op := TFhirOperationOutcome(comp.resource);
          if (op.text <> nil) and (op.text.div_ <> nil) then
            Raise EFHIRClientException.create(op.text.div_.AsPlainText, comp.resource.link as TFhirOperationOutcome)
          else if (op.issueList.Count > 0) and (op.issueList[0].diagnostics <> '') then
            Raise EFHIRClientException.create(op.issueList[0].diagnostics, comp.resource.link as TFhirOperationOutcome)
          else
            raise exception.Create(cnt)
        end
        else
          raise exception.Create(cnt)
      finally
        comp.source.free;
        comp.Free;
      end;
    end
    else
      raise exception.Create(cnt)
  end;
begin
  if FJson then
begin
    http.RequestType := 'application/fhir+json; charset=utf-8';
    http.ResponseType := 'application/fhir+json; charset=utf-8';
  end
  else
  begin
    http.RequestType := 'application/fhir+xml; charset=utf-8';
    http.ResponseType := 'application/fhir+xml; charset=utf-8';
  end;
  if ct <> '' then
    http.RequestType := ct;

  repeat
  http.SetAddress(url);
  ok := false;
      case verb of
        get :
          begin
          http.RequestMethod := 'GET';
          end;
        post :
          begin
          http.RequestMethod := 'POST';
          http.Request := TADvBuffer.create;
          http.Request.LoadFromStream(source);
          end;
        put :
          begin
          http.RequestMethod := 'PUT';
          http.Request.LoadFromStream(source);
          end;
        delete :
          http.RequestMethod := 'DELETE';
        patch :
          begin
          http.RequestMethod := 'PATCH';
          http.RequestType := 'application/json-patch+json; charset=utf-8';
          end;
        options :
          begin
          http.RequestMethod := 'OPTIONS';
          end;
      end;

      http.Response := TAdvBuffer.create;
      http.Execute;

      code := StrToInt(http.ResponseCode);
      if (code < 200) or (code >= 600) Then
        raise exception.create('unexpected condition');
    if (code >= 300) and (code < 400) then
      url := http.getResponseHeader('Location');
  until (code < 300) or (code >= 400);

  if code >= 300 then
    processException;
      ok := true;
      result := TMemoryStream.Create;
  // if this breaks, the stream leaks
      http.Response.SaveToStream(result);
      result.Position := 0;
  end;
{$ENDIF}


function TFhirHTTPClient.exchangeIndy(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; ct : String) : TStream;
var
  comp : TFHIRParser;
  ok : boolean;
  cnt : String;
  op : TFHIROperationOutcome;
begin
  if FJson then
  begin
    indy.Request.ContentType := 'application/fhir+json; charset=utf-8';
    indy.Request.Accept := 'application/fhir+json; charset=utf-8';
  end
  else
  begin
    indy.Request.ContentType := 'application/fhir+xml; charset=utf-8';
    indy.Request.Accept := 'application/fhir+xml; charset=utf-8';
  end;
  if ct <> '' then
    indy.Request.ContentType := ct;
  if smartToken <> nil then
    indy.Request.CustomHeaders.values['Authorization'] := 'Bearer '+smartToken.accessToken;

  ok := false;
  result := TMemoryStream.create;
  Try
    Try
      case verb of
        get : indy.Get(url, result);
        post : indy.Post(url, source, result);
        put : indy.Put(url, source, result);
        delete : indy.delete(url);
        options : indy.Options(url);
{$IFNDEF VER260}    patch : indy.Patch(url, source, result); {$ENDIF}
      else
        raise Exception.Create('Unknown HTTP method '+inttostr(ord(verb)));
      end;

      if (indy.ResponseCode < 200) or (indy.ResponseCode >= 300) Then
        raise exception.create('unexpected condition');
      ok := true;
      if (result <> nil) then
         result.Position := 0;
    except
      on E:EIdHTTPProtocolException do
      begin
        cnt := e.ErrorMessage;
        if StringFind(cnt, 'OperationOutcome') > 0 then
        begin
          removeBom(cnt);
          if FJson then
            comp := TFHIRJsonParser.create(FWorker.Link, 'en')
          else
            comp := TFHIRXmlParser.create(FWorker.Link, 'en');
          try
            comp.source := TStringStream.create(cnt);
            comp.Parse;
            if (comp.resource <> nil) and (comp.resource.ResourceType = frtOperationOutcome) then
            begin
              op := TFhirOperationOutcome(comp.resource);
              if (op.text <> nil) and (op.text.div_ <> nil) then
                Raise EFHIRClientException.create(op.text.div_.AsPlainText, comp.resource.link as TFhirOperationOutcome)
              else if (op.issueList.Count > 0) and (op.issueList[0].diagnostics <> '') then
                Raise EFHIRClientException.create(op.issueList[0].diagnostics, comp.resource.link as TFhirOperationOutcome)
              else
                raise exception.Create(cnt)
            end
            else
              raise exception.Create(cnt)
          finally
            comp.source.free;
            comp.Free;
          end;
        end
        else
          raise exception.Create(cnt)
      end;
      on e : exception do
        raise;
    end;
  finally
    if not ok then
      result.free;
  end;
end;

procedure TFhirHTTPClient.SetTimeout(const Value: cardinal);
begin
  FTimeout := Value;
  createClient;
  if FUseIndy then
  begin
    indy.IOHandler.ReadTimeout := Value;
    indy.ReadTimeout := Value;
  end;
end;

procedure TFhirHTTPClient.SetUseIndy(const Value: boolean);
begin
  {$IFNDEF OSX}
  FUseIndy := Value;
  {$ELSE}
  // ignore...?
  {$ENDIF}
end;

function TFhirHTTPClient.address: String;
begin
  result := FUrl;
end;

procedure TFhirHTTPClient.authoriseByOWin(server, username, password: String);
var
  token : TJsonObject;
begin
  if FUseIndy then
    token := authoriseByOWinIndy(server, username, password)
  else
    token := authoriseByOWinHttp(server, username, password);
  try
  smartToken := TSmartOnFhirAccessToken.Create;
    smartToken.accessToken := token.str['access_token'];
    smartToken.expires := now + (StrToInt(token.num['expires_in']) * DATETIME_SECOND_ONE);
  finally
    token.Free;
  end;
end;

function TFhirHTTPClient.authoriseByOWinHttp(server, username, password: String): TJsonObject;
begin
  raise Exception.Create('Not done yet');
end;

function TFhirHTTPClient.authoriseByOWinIndy(server, username, password: String): TJsonObject;
var
  ss : TStringStream;
  resp : TMemoryStream;
begin
  createClient;
  indy.Request.ContentType := 'application/x-www-form-urlencoded';

  ss := TStringStream.Create('grant_type=password&username='+username+'&password='+(password));
  try
    resp := TMemoryStream.create;
    Try
      indy.Post(server, ss, resp);
      if (indy.ResponseCode < 200) or (indy.ResponseCode >= 300) Then
        raise exception.create('unexpected condition');
      resp.Position := 0;
      result := TJSONParser.Parse(resp);
    finally
      resp.Free;
    end;
  finally
    ss.Free;
  end;
end;

procedure TFhirHTTPClient.cancelOperation;
begin
  if not FUseIndy then
    raise Exception.Create('Cancel not supported')
  else if indy <> nil then
    indy.Disconnect;
end;


function TFhirHTTPClient.cdshook(id: String; request: TCDSHookRequest): TCDSHookResponse;
begin
  result := nil;
end;

{ TFhirClient }

function TFhirClient.link: TFhirClient;
begin
  result := TFhirClient(inherited Link);
end;

function TFhirClient.address: String;
begin
  raise Exception.Create('Must override address in '+className);
end;

function TFhirClient.conformance(summary: boolean): TFhirCapabilityStatement;
begin
  raise Exception.Create('Must override conformance() in '+className);
end;

function TFhirClient.createResource(resource: TFhirResource; var id: String): TFHIRResource;
begin
  raise Exception.Create('Must override createResource() in '+className);
end;

procedure TFhirClient.deleteResource(atype: TFhirResourceType; id: String);
begin
  raise Exception.Create('Must override deleteResource() in '+className);
end;

function TFhirClient.format: TFHIRFormat;
begin
  result := ffUnspecified;
end;

function TFhirClient.historyType(atype: TFhirResourceType; allRecords: boolean; params: TDictionary<String, String>): TFHIRBundle;
begin
  raise Exception.Create('Must override historyType() in '+className);
end;

function TFhirClient.operation(atype: TFhirResourceType; opName: String; params: TFhirParameters): TFHIRResource;
begin
  raise Exception.Create('Must override operation() in '+className);
end;

function TFhirClient.readResource(atype: TFhirResourceType; id: String): TFHIRResource;
begin
  raise Exception.Create('Must override readResource() in '+className);
end;

function TFhirClient.search(atype: TFhirResourceType; allRecords: boolean; params: string): TFHIRBundle;
begin
  raise Exception.Create('Must override search() in '+className);
end;

function TFhirClient.searchAgain(link: String): TFHIRBundle;
begin
  raise Exception.Create('Must override searchAgain() in '+className);
end;

function TFhirClient.search(allRecords: boolean; params: TDictionary<String, String>): TFHIRBundle;
begin
  raise Exception.Create('Must override search() in '+className);
end;

function TFhirClient.search(atype: TFhirResourceType; allRecords: boolean; params: TDictionary<String, String>): TFHIRBundle;
begin
  raise Exception.Create('Must override search() in '+className);
end;

function TFhirClient.searchPost(atype: TFhirResourceType; allRecords: boolean; params: TDictionary<String, String>; resource: TFhirResource): TFHIRBundle;
begin
  raise Exception.Create('Must override searchPost() in '+className);
end;

function TFhirClient.transaction(bundle: TFHIRBundle): TFHIRBundle;
begin
  raise Exception.Create('Must override transaction() in '+className);
end;

function TFhirClient.updateResource(resource: TFhirResource): TFHIRResource;
begin
  raise Exception.Create('Must override updateResource() in '+className);
end;

end.

