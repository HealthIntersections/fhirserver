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


interface

uses
  SysUtils, Classes, Generics.Collections, Soap.EncdDecd,
  IdHTTP, IdSSLOpenSSL, IdComponent,
  StringSupport, EncodeSupport, GuidSupport, DateSupport, MimeMessage,
  AdvObjects, AdvBuffers, AdvJson,
  {$IFNDEF OSX}AdvWinInetClients, {$ENDIF}
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

  TFHIRClientLogger = class (TAdvObject)
  public
    function Link : TFHIRClientLogger; overload;
    procedure logExchange(verb, url, status, requestHeaders, responseHeaders : String; request, response : TStream);  virtual;
  end;

  TNullLogger = class (TFHIRClientLogger)
  public
    procedure logExchange(verb, url, status, requestHeaders, responseHeaders : String; request, response : TStream);  override;
  end;


  TFhirClient = {abstract} class (TAdvObject)
  private
    FLogger : TFHIRClientLogger;
    FLastURL: String;
    FProvenance: TFhirProvenance;
    FLastOperationId: String;
    FVersionSpecific: boolean;
    procedure SetProvenance(const Value: TFhirProvenance);
  protected
    procedure SetLogger(const Value: TFHIRClientLogger); virtual;
    function encodeParams(params: TStringList): String;
  public
    Destructor Destroy; override;
    function link : TFhirClient; overload;
    property provenance : TFhirProvenance read FProvenance write SetProvenance;

    function conformance(summary : boolean) : TFhirCapabilityStatement; virtual;
    function transaction(bundle : TFHIRBundle) : TFHIRBundle; virtual;
    function createResource(resource : TFhirResource; var id : String) : TFHIRResource; virtual;
    function readResource(atype : TFhirResourceType; id : String) : TFHIRResource; virtual;
    function updateResource(resource : TFhirResource) : TFHIRResource; overload; virtual;
    procedure deleteResource(atype : TFhirResourceType; id : String); virtual;
    function search(allRecords : boolean; params : TStringList) : TFHIRBundle; overload; virtual;
    function search(allRecords : boolean; params : string) : TFHIRBundle; overload; virtual;
    function search(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle; overload; virtual;
    function search(atype : TFhirResourceType; allRecords : boolean; params : string) : TFHIRBundle; overload; virtual;
    function searchPost(atype : TFhirResourceType; allRecords : boolean; params : TStringList; resource : TFhirResource) : TFHIRBundle; virtual;
    function searchAgain(link : String) : TFHIRBundle; overload; virtual;
    function operation(atype : TFhirResourceType; opName : String; params : TFhirParameters) : TFHIRResource; overload; virtual;
    function operation(atype : TFhirResourceType; id, opName : String; params : TFhirParameters) : TFHIRResource; overload; virtual;
    function historyType(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle; virtual;

    procedure terminate; virtual;
    function address : String; virtual;
    function format : TFHIRFormat; virtual;
    property versionSpecific : boolean read FVersionSpecific write FVersionSpecific;
    property LastURL : String read FLastURL write FLastURL;
    property LastOperationId : String read FLastOperationId write FLastOperationId; // some servers return an id that links to their own internal log for debugging
    property Logger : TFHIRClientLogger read FLogger write SetLogger;
  end;

  TFhirHTTPClientHTTPVerb = (httpGet, httpPost, httpPut, httpDelete, httpOptions, httpPatch);
const

  CODES_TFhirHTTPClientHTTPVerb : array [TFhirHTTPClientHTTPVerb] of String = ('GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH');
type
  TFhirHTTPClientStatusEvent = procedure (client : TObject; details : String) of Object;

  // use only in one thread at a time
  TFhirHTTPClient = class (TFhirClient)
  private
    FUrl : String;
    FJson : Boolean;
    {$IFDEF MSWINDOWS}
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
    FTerminated : boolean;

    FCertFile: String;
    FProxy: String;
    FPassword: String;
    FUsername: String;
    FCertKey: String;
//    FLastUpdated : TDateTimeEx;
    procedure status(msg : String);
    procedure getSSLpassword(var Password: String);
    function serialise(resource : TFhirResource):TStream; overload;
    function makeUrl(tail : String; params : TStringList = nil) : String;
    function makeUrlPath(tail : String) : String;
    function CreateParser(stream : TStream) : TFHIRParser;
    function exchange(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; ct : String = '') : TStream;
    function fetchResource(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; ct : String = '') : TFhirResource;
    function makeMultipart(stream: TStream; streamName: string; params: TStringList; var mp : TStream) : String;
    procedure SetSmartToken(const Value: TSmartOnFhirAccessToken);
    procedure SetTimeout(const Value: cardinal);
    procedure createClient;
    procedure setHeader(name, value : String);
    function GetHeader(name : String) : String;
    function exchangeIndy(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; ct: String): TStream;
    {$IFDEF MSWINDOWS}
    function exchangeHTTP(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; ct: String): TStream;
    {$ENDIF}

    function authoriseByOWinIndy(server, username, password : String): TJsonObject;
    function authoriseByOWinHttp(server, username, password : String): TJsonObject;
    procedure SetCertFile(const Value: String);
    procedure SetCertPWord(const Value: String);
    procedure SetUseIndy(const Value: boolean);
    function Convert(stream : TStream) : TStream; virtual;
    function mimeType(fmt: String): String;
    procedure SetCertKey(const Value: String);
  public
    constructor Create(worker : TFHIRWorkerContext; url : String; json : boolean); overload;
    constructor Create(worker : TFHIRWorkerContext; url : String; json : boolean; timeout : integer); overload;
    constructor Create(worker : TFHIRWorkerContext; url : String; json : boolean; timeout : integer; proxy : String); overload;
    destructor Destroy; override;
    property url : String read FUrl;

    property Json : boolean read FJson write FJson;

    function link : TFhirHTTPClient; overload;
    property smartToken : TSmartOnFhirAccessToken read FSmartToken write SetSmartToken;
    property timeout : cardinal read FTimeout write SetTimeout;
    property UseIndy : boolean read FUseIndy write SetUseIndy; // set this to true for a service, but you may have problems with SSL
    property allowR2 : boolean read FAllowR2 write FAllowR2;
    property certFile : String read FCertFile write SetCertFile;
    property certKey : String read FCertKey write SetCertKey;
    property certPWord : String read FCertPWord write SetCertPWord;
    property proxy : String read FProxy write FProxy;
    property username : String read FUsername write FUsername;
    property password : String read FPassword write FPassword;

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
    function search(allRecords : boolean; params : TStringList) : TFHIRBundle; overload; override;
    function search(allRecords : boolean; params : string) : TFHIRBundle; overload; override;
    function search(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle; overload; override;
    function search(atype : TFhirResourceType; allRecords : boolean; params : string) : TFHIRBundle; overload; override;
    function searchPost(atype : TFhirResourceType; allRecords : boolean; params : TStringList; resource : TFhirResource) : TFHIRBundle; override;
    function searchAgain(link : String) : TFHIRBundle; overload; override;
    function operation(atype : TFhirResourceType; opName : String; params : TFhirParameters) : TFHIRResource; overload; override;
    function operation(atype : TFhirResourceType; id, opName : String; params : TFhirParameters) : TFHIRResource; overload; override;
    function historyType(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle; override;
    function cdshook(id : String; request : TCDSHookRequest) : TCDSHookResponse;

    procedure terminate; override;

    //authorization support
    procedure authoriseByOWin(server, username, password : String);

    property OnClientStatus : TFhirHTTPClientStatusEvent read FOnClientStatus write FOnClientStatus;
  end;

  TThreadManagementEvent = procedure (sender : TFhirClient; var stop : boolean) of object;

  TFhirThreadedClientPackage = class (TAdvObject)
  private
    FCommand: TFHIRCommandType;
    FSummary: boolean;
    FError: String;
    FResult: TFhirResource;
    FDone: Boolean;
    FThread: TThread;
    FResourceType: TFhirResourceType;
    FAllRecords: boolean;
    Fparams : TStringList;
    FparamString : String;
    FUrl: String;
    FId: String;
    FResource: TFhirResource;
    FName: String;
    FLastURL: String;
    procedure SetResult(const Value: TFhirResource);
    procedure SetResource(const Value: TFhirResource);
  public
    destructor Destroy; override;
    function Link : TFhirThreadedClientPackage; overload;

    property Thread : TThread read FThread write FThread;
    property Done : Boolean read FDone write FDone;

    property command : TFHIRCommandType read FCommand write FCommand;
    property summary : boolean read FSummary write FSummary;
    property resourceType : TFhirResourceType read FResourceType write FResourceType;
    property allRecords : boolean read FAllRecords write FAllRecords;
    property params : TStringList read FParams write FParams;
    property paramString : string read FParamString write FParamString;
    property url : String read FUrl write FUrl;
    property id : String read FId write FId;
    property name : String read FName write FName;
    property resource : TFhirResource read FResource write SetResource;

    property result : TFhirResource read FResult write SetResult;
    property error : String read FError write FError;
    property lastUrl : String read FLastURL write FLastURL;
  end;

  TThreadClientThread = class (TThread)
  private
    FPackage : TFHIRThreadedClientPackage;
    FDone : boolean;
    FClient : TFhirClient;
  protected
    procedure execute; override;
  public
    Constructor Create(client: TFhirClient; pack : TFhirThreadedClientPackage);
    destructor Destroy; override;
  end;

  TFhirThreadedClient = class (TFhirClient)
  private
    FInternal : TFhirClient;
    FEvent : TThreadManagementEvent;
    procedure wait(Package : TFHIRThreadedClientPackage);
  protected
    procedure SetLogger(const Value: TFHIRClientLogger); override;
  public
    Constructor Create(internal : TFhirClient; event : TThreadManagementEvent);
    Destructor Destroy; override;

    function Link : TFhirThreadedClient; overload;

    function conformance(summary : boolean) : TFhirCapabilityStatement; override;
    function transaction(bundle : TFHIRBundle) : TFHIRBundle; override;
    function createResource(resource : TFhirResource; var id : String) : TFHIRResource; override;
    function readResource(atype : TFhirResourceType; id : String) : TFHIRResource; override;
    function updateResource(resource : TFhirResource) : TFHIRResource; overload; override;
    procedure deleteResource(atype : TFhirResourceType; id : String); override;
    function search(allRecords : boolean; params : TStringList) : TFHIRBundle; overload; override;
    function search(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle; overload; override;
    function search(atype : TFhirResourceType; allRecords : boolean; params : string) : TFHIRBundle; overload; override;
    function searchPost(atype : TFhirResourceType; allRecords : boolean; params : TStringList; resource : TFhirResource) : TFHIRBundle; override;
    function searchAgain(link : String) : TFHIRBundle; overload; override;
    function operation(atype : TFhirResourceType; opName : String; params : TFhirParameters) : TFHIRResource; overload; override;
    function operation(atype : TFhirResourceType; id, opName : String; params : TFhirParameters) : TFHIRResource; overload; override;
    function historyType(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle; override;

    function address : String; override;
    function format : TFHIRFormat; override;
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

constructor TFhirHTTPClient.Create(worker: TFHIRWorkerContext; url: String; json: boolean; timeout: integer);
begin
  Create(worker, url, json);
  FTimeout := timeout;
end;

constructor TFhirHTTPClient.Create(worker: TFHIRWorkerContext; url: String; json: boolean; timeout: integer; proxy: String);
begin
  Create(worker, url, json);
  FTimeout := timeout;
  FProxy := proxy;
end;

constructor TFhirHTTPClient.create(worker : TFHIRWorkerContext; url: String; json : boolean);
begin
  Create;
  FLogger := TNullLogger.create;
  FWorker := worker;
  FUrl := URL;
  FJson := json;
  {$IFNDEF WINDOWS}
  FUseIndy := true;
  {$ENDIF}
end;

destructor TFhirHTTPClient.destroy;
begin
  FProvenance.Free;
  FWorker.Free;
  FSmartToken.Free;
  ssl.Free;
  indy.free;
  {$IFDEF MSWINDOWS}
  http.Free;
  {$ENDIF}
  inherited;
end;


function TFhirHTTPClient.link: TFhirHTTPClient;
begin
  result := TFhirHTTPClient(inherited Link);
end;

function TFhirHTTPClient.conformance(summary : boolean): TFhirCapabilityStatement;
var
  params : TStringList;
begin
  params := TStringList.create;
  try
    if summary then
      params.AddPair('_summary', 'true');
    result := FetchResource(MakeUrl('metadata', params), httpGet, nil) as TFhirCapabilityStatement;
  finally
    params.Free;
  end;
end;


procedure TFhirHTTPClient.terminate;
begin
  FTerminated := true;
  if FUseIndy and (indy <> nil) then
    indy.Disconnect;
end;

function TFhirHTTPClient.transaction(bundle : TFHIRBundle) : TFHIRBundle;
Var
  src : TStream;
begin
  src := serialise(bundle);
  try
    result := fetchResource(makeUrl(''), httpPost, src) as TFhirBundle;
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
      result := fetchResource(MakeUrl(CODES_TFhirResourceType[resource.resourceType]), httpPost, src);
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
      result := fetchResource(MakeUrl(CODES_TFhirResourceType[resource.resourceType]+'/'+resource.id), httpPut, src);
  finally
    src.free;
  end;
end;

procedure TFhirHTTPClient.deleteResource(atype : TFhirResourceType; id : String);
begin
  exchange(MakeUrl(CODES_TFhirResourceType[aType]+'/'+id), httpDelete, nil).free;
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
      comp := TFHIRJsonComposer.create(FWorker.link, OutputStyleNormal, 'en')
    else
      comp := TFHIRXmlComposer.create(FWorker.link, OutputStyleNormal, 'en');
    try
      comp.Compose(result, resource, nil);
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
  {$IFDEF MSWINDOWS}
  http.Free;
  http := nil;
  {$ENDIF}
end;

procedure TFhirHTTPClient.SetCertKey(const Value: String);
begin
  FCertKey := Value;
  indy.free;
  indy := nil;
  {$IFDEF MSWINDOWS}
  http.Free;
  http := nil;
  {$ENDIF}
end;

procedure TFhirHTTPClient.SetCertPWord(const Value: String);
begin
  FCertPWord := Value;
  indy.free;
  indy := nil;
  {$IFDEF MSWINDOWS}
  http.Free;
  http := nil;
  {$ENDIF}
end;

procedure TFhirClient.SetLogger(const Value: TFHIRClientLogger);
begin
  FLogger.Free;
  FLogger := Value;
end;

procedure TFhirClient.SetProvenance(const Value: TFhirProvenance);
begin
  FProvenance.Free;
  FProvenance := Value;
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

function TFhirClient.encodeParams(params : TStringList) : String;
var
  i : integer;
  s : String;
begin
  result := '';
  for i := 0 to params.Count - 1 do
  begin
    s := params.Names[i];
    result := result + s+'='+EncodeMIME(params.ValueFromIndex[i])+'&';
  end;
end;

function TFhirHTTPClient.search(atype: TFhirResourceType; allRecords: boolean; params: TStringList): TFHIRBundle;
begin
  result := search(atype, allrecords, encodeParams(params));
end;

function TFhirHTTPClient.search(atype: TFhirResourceType; allRecords: boolean; params: string): TFHIRBundle;
var
  s : String;
  res : TFHIRResource;
  feed : TFHIRBundle;
begin
  res := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'?'+params, httpGet, nil);
//    client.Request.RawHeaders.Values['Content-Location'] := MakeUrlPath(CODES_TFhirResourceType[resource.resourceType]+'/'+id+'/history/'+ver);
  if not (res is TFHIRBundle) then
    raise Exception.Create('Found a resource of type '+res.fhirType+' expecting a Bundle');

  result := res as TFHIRBundle;
  try
    s := result.links['next'];
    while AllRecords and (s <> '') do
    begin
      feed := fetchResource(s, httpGet, nil) as TFhirBundle;
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

function TFhirHTTPClient.search(allRecords: boolean; params: string): TFHIRBundle;
begin
  result := search(frtNull, allrecords, params);
end;

function TFhirHTTPClient.searchAgain(link: String): TFHIRBundle;
begin
  result := fetchResource(link, httpGet, nil) as TFHIRBundle;
end;

function TFhirHTTPClient.search(allRecords: boolean; params: TStringList): TFHIRBundle;
begin
  result := search(frtNull, allrecords, encodeParams(params));
end;

function TFhirHTTPClient.searchPost(atype: TFhirResourceType; allRecords: boolean; params: TStringList; resource: TFhirResource): TFHIRBundle;
var
  src, frm : TStream;
  ct : String;
begin
  src := serialise(resource);
  try
    src.Position := 0;
    ct := makeMultipart(src, 'src', params, frm);
    try
      result := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'/_search', httpPost, frm) as TFhirBundle;
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
      result := fetchResource(makeUrl('$'+opName), httpPost, src)
    else
    result := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'/$'+opName, httpPost, src);
  finally
    src.free;
  end;
end;

function TFhirHTTPClient.operation(atype : TFhirResourceType; id, opName : String; params : TFhirParameters) : TFHIRResource;
Var
  src : TStream;
begin
  if params = nil then
  begin
    result := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'/'+id+'/$'+opName, httpGet, nil);
  end
  else
  begin
    src := serialise(params);
    try
      src.Position := 0;
      result := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'/'+id+'/$'+opName, httpPost, src);
    finally
      src.free;
    end;
  end;
end;

function TFhirHTTPClient.fetchResource(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; ct : String = ''): TFhirResource;
var
  ret, conv : TStream;
  p : TFHIRParser;
begin
  FTerminated := false;
  ret := exchange(url, verb, source, ct);
  if FTerminated then
    abort;
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

function TFhirHTTPClient.makeMultipart(stream: TStream; streamName: string; params: TStringList; var mp : TStream) : String;
var
  m : TMimeMessage;
  p : TMimePart;
  s : String;
  i : integer;
begin
  m := TMimeMessage.create;
  try
    p := m.AddPart(NewGuidURN);
    p.ContentDisposition := 'form-data; name="'+streamName+'"';
    p.Content.LoadFromStream(stream);
    for i := 0 to params.Count -1 do
    begin
      s := params.Names[i];
      p := m.AddPart(NewGuidURN);
      p.ContentDisposition := 'form-data; name="'+s+'"';
      p.Content.AsBytes := TEncoding.UTF8.GetBytes(params.ValueFromIndex[i]);
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

function TFhirHTTPClient.makeUrl(tail: String; params : TStringList = nil): String;
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
    result := fetchResource(MakeUrl(CODES_TFhirResourceType[AType]+'/'+id), httpGet, nil);
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

function TFhirHTTPClient.historyType(atype: TFhirResourceType; allRecords: boolean; params: TStringList): TFHIRBundle;
var
  s : String;
  feed : TFHIRBundle;
  i : integer;
begin
//    client.Request.RawHeaders.Values['Content-Location'] := MakeUrlPath(CODES_TFhirResourceType[resource.resourceType]+'/'+id+'/history/'+ver);
  status('Fetch History for '+CODES_TFhirResourceType[aType]);
  result := fetchResource(makeUrl(CODES_TFhirResourceType[aType])+'/_history?'+encodeParams(params), httpGet, nil) as TFhirBundle;
  try
    s := result.links['next'];
    i := 1;
    while AllRecords and (s <> '') do
    begin
      inc(i);
      status('Fetch History for '+CODES_TFhirResourceType[aType]+' page '+inttostr(i));
      feed := fetchResource(s, httpGet, nil) as TFhirBundle;
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
      if (proxy <> '') then
      begin
        try
          indy.ProxyParams.ProxyServer := proxy.Split([':'])[0];
          indy.ProxyParams.ProxyPort := StrToInt(proxy.Split([':'])[1]);
        except
          raise Exception.Create('Unable to process proxy "'+proxy+'" - use address:port');
        end;
      end;
      ssl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      indy.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      ssl.SSLOptions.SSLVersions := [sslvTLSv1_2];
      ssl.SSLOptions.Method := sslvTLSv1_2;

      if certFile <> '' then
      begin
        ssl.SSLOptions.CertFile := certFile;
        if certKey <> '' then
          ssl.SSLOptions.KeyFile := certKey
        else
          ssl.SSLOptions.KeyFile := ChangeFileExt(certFile,'.key');
        ssl.OnGetPassword := getSSLpassword;
      end;
    end;
  end
  {$IFDEF MSWINDOWS}
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
  {$IFDEF MSWINDOWS}
  else
    http.Headers.AddOrSetValue(name, value);
  {$ENDIF}
end;

function TFhirHTTPClient.GetHeader(name: String): String;
begin
  createClient;
  if FUseIndy then
    result := indy.Response.RawHeaders.Values[name]
  {$IFDEF MSWINDOWS}
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
  LastURL := url;
  if FUseIndy then
    result := exchangeIndy(url, verb, source, ct)
  {$IFDEF MSWINDOWS}
  else
    result := exchangeHTTP(url, verb, source, ct)
  {$ENDIF}
end;

function TFhirHTTPClient.mimeType(fmt : String): String;
begin
  {$IFDEF FHIR2}
  result := 'application/'+fmt+'+fhir';
  {$ELSE}
  result := 'application/fhir+'+fmt;
  {$ENDIF}
  if versionSpecific then
    result := result + '; fhir-version=r'+FHIR_GENERATED_PUBLICATION;
end;

{$IFDEF MSWINDOWS}
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
    else if cnt = '' then
      raise exception.Create(http.ResponseCode+' ' +http.ResponseText)
    else
      raise exception.Create(cnt)
  end;
begin
  if FJson then
  begin
    http.RequestType := mimeType('json')+'; charset=utf-8';
    http.ResponseType := mimeType('json')+'; charset=utf-8';
  end
  else
  begin
    http.RequestType := mimeType('xml')+'; charset=utf-8';
    http.ResponseType := mimeType('xml')+'; charset=utf-8';
  end;
  if ct <> '' then
    http.RequestType := ct;

  if provenance <> nil then
    http.Headers['X-Provenance'] := resourceToString(Provenance, ffJson)
  else
    http.Headers.Remove('X-Provenance');

  if password <> '' then
  begin
    http.Username := username;
    http.Password := Password;
  end;
  // todo: if smartToken <> nil then
  // todo:  indy.Request.CustomHeaders.values['Authorization'] := 'Bearer '+smartToken.accessToken;  ? how to set this on wininet

  repeat
    http.SetAddress(url);
    ok := false;
      case verb of
        httpGet :
          begin
          http.RequestMethod := 'GET';
          end;
        httpPost :
          begin
          http.RequestMethod := 'POST';
          http.Request := TADvBuffer.create;
          http.Request.LoadFromStream(source);
          end;
        httpPut :
          begin
          http.RequestMethod := 'PUT';
          http.Request.LoadFromStream(source);
          end;
        httpDelete :
          http.RequestMethod := 'DELETE';
        httpPatch :
          begin
          http.RequestMethod := 'PATCH';
          http.RequestType := 'application/json-patch+json; charset=utf-8';
          end;
        httpOptions :
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

  FLastOperationId := http.Headers['X-Request-Id'];

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
    indy.Request.ContentType := mimeType('json')+'; charset=utf-8';
    indy.Request.Accept := mimeType('json')+'; charset=utf-8';
  end
  else
  begin
    indy.Request.ContentType := mimeType('xml')+'; charset=utf-8';
    indy.Request.Accept := mimeType('xml')+'; charset=utf-8';
  end;
  if ct <> '' then
    indy.Request.ContentType := ct;
  if smartToken <> nil then
    indy.Request.CustomHeaders.values['Authorization'] := 'Bearer '+smartToken.accessToken;

  if password <> '' then
  begin
    indy.Request.BasicAuthentication:= true;
    indy.Request.UserName := UserName;
    indy.Request.Password := Password;
  end;

  if provenance <> nil then
    indy.Request.CustomHeaders.values['X-Provenance'] := resourceToString(Provenance, ffJson)
  else if indy.Request.CustomHeaders.IndexOfName('X-Provenance') > -1 then
    indy.Request.CustomHeaders.Delete(indy.Request.CustomHeaders.IndexOfName('X-Provenance'));


  ok := false;
  result := TMemoryStream.create;
  Try
    Try
      case verb of
        httpGet :    indy.Get(url, result);
        httpPost :   indy.Post(url, source, result);
        httpPut :    indy.Put(url, source, result);
        httpDelete : indy.delete(url);
        httpOptions: indy.Options(url);
{$IFNDEF VER260}
        httpPatch :  indy.Patch(url, source, result);
{$ENDIF}
      else
        raise Exception.Create('Unknown HTTP method '+inttostr(ord(verb)));
      end;

      FLogger.logExchange(CODES_TFhirHTTPClientHTTPVerb[verb], url, indy.ResponseText, indy.Request.RawHeaders.Text, indy.Response.RawHeaders.Text, source, result);
      if (indy.ResponseCode < 200) or (indy.ResponseCode >= 300) Then
        raise exception.create('unexpected condition');
      ok := true;
      if (result <> nil) then
         result.Position := 0;
      FLastOperationId := indy.Response.RawHeaders.Values['X-Request-Id'];
    except
      on E:EIdHTTPProtocolException do
      begin
        FLogger.logExchange(CODES_TFhirHTTPClientHTTPVerb[verb], url, indy.ResponseText, indy.Request.RawHeaders.Text, indy.Response.RawHeaders.Text, source, result);
        cnt := e.ErrorMessage;
        if cnt = '' then
          cnt := e.message;
        FLastOperationId := indy.Response.RawHeaders.Values['X-Request-Id'];

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
  {$IFDEF MSWINDOWS}
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
var
  b : TBytes;
  req, resp : TStream;
  json : TJsonObject;
begin
  b := TEncoding.UTF8.GetBytes(request.AsJson);
  req := TMemoryStream.Create;
  try
    req.Write(b[0], length(b));
    req.Position := 0;
    resp := exchange(UrlPath([FURL, 'cds-services', id]), httpPost, req, 'application/json');
    try
      json := TJSONParser.Parse(resp);
      try
        result := TCDSHookResponse.Create(json);
      finally
        json.Free;
      end;
    finally
      resp.free;
    end;
  finally
    req.Free;
  end;
end;

{ TFhirClient }

function TFhirClient.link: TFhirClient;
begin
  result := TFhirClient(inherited Link);
end;

function TFhirClient.operation(atype: TFhirResourceType; id, opName: String; params: TFhirParameters): TFHIRResource;
begin
  raise Exception.Create('Must override operation() in '+className);
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

destructor TFhirClient.Destroy;
begin
  FLogger.Free;
  inherited;
end;

function TFhirClient.format: TFHIRFormat;
begin
  result := ffUnspecified;
end;

function TFhirClient.historyType(atype: TFhirResourceType; allRecords: boolean; params : TStringList): TFHIRBundle;
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

function TFhirClient.search(allRecords: boolean; params: string): TFHIRBundle;
begin
  raise Exception.Create('Must override search() in '+className);
end;

function TFhirClient.searchAgain(link: String): TFHIRBundle;
begin
  raise Exception.Create('Must override searchAgain() in '+className);
end;

function TFhirClient.search(allRecords: boolean; params : TStringList): TFHIRBundle;
begin
  raise Exception.Create('Must override search() in '+className);
end;

function TFhirClient.search(atype: TFhirResourceType; allRecords: boolean; params : TStringList): TFHIRBundle;
begin
  raise Exception.Create('Must override search() in '+className);
end;

function TFhirClient.searchPost(atype: TFhirResourceType; allRecords: boolean; params : TStringList; resource: TFhirResource): TFHIRBundle;
begin
  raise Exception.Create('Must override searchPost() in '+className);
end;

procedure TFhirClient.terminate;
begin

end;

function TFhirClient.transaction(bundle: TFHIRBundle): TFHIRBundle;
begin
  raise Exception.Create('Must override transaction() in '+className);
end;

function TFhirClient.updateResource(resource: TFhirResource): TFHIRResource;
begin
  raise Exception.Create('Must override updateResource() in '+className);
end;

{ TFhirThreadedClient }

function TFhirThreadedClient.address: String;
begin
  result := FInternal.address;
end;

function TFhirThreadedClient.conformance(summary: boolean): TFhirCapabilityStatement;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdConformanceStmt;
    pack.summary := summary;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link as TFhirCapabilityStatement;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

constructor TFhirThreadedClient.Create(internal: TFhirClient; event: TThreadManagementEvent);
begin
  Inherited Create;
  FInternal := internal;
  FEvent := event;
end;

function TFhirThreadedClient.createResource(resource: TFhirResource; var id: String): TFHIRResource;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdCreate;
    pack.resource := resource.Link;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link;
    id := pack.id;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

procedure TFhirThreadedClient.deleteResource(atype: TFhirResourceType; id: String);
begin
 raise Exception.Create('Not Done Yet');
end;

destructor TFhirThreadedClient.Destroy;
begin
  FInternal.free;
  inherited;
end;

function TFhirThreadedClient.format: TFHIRFormat;
begin
  result := FInternal.format;
end;

function TFhirThreadedClient.historyType(atype: TFhirResourceType; allRecords: boolean; params : TStringList): TFHIRBundle;
begin
  raise Exception.Create('Not Done Yet');
end;

function TFhirThreadedClient.link: TFhirThreadedClient;
begin
  result := TFhirThreadedClient(inherited Link);
end;

function TFhirThreadedClient.operation(atype: TFhirResourceType; id, opName: String; params: TFhirParameters): TFHIRResource;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdOperation;
    pack.resourceType := aType;
    pack.id := id;
    pack.Name := opName;
    pack.resource := params.Link;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

function TFhirThreadedClient.operation(atype: TFhirResourceType; opName: String; params: TFhirParameters): TFHIRResource;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdOperation;
    pack.resourceType := aType;
    pack.Name := opName;
    pack.resource := params.Link;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

function TFhirThreadedClient.readResource(atype: TFhirResourceType; id: String): TFHIRResource;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdRead;
    pack.resourceType := aType;
    pack.id := id;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

function TFhirThreadedClient.search(atype: TFhirResourceType; allRecords: boolean; params : TStringList): TFHIRBundle;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdSearch;
    pack.resourceType := aType;
    pack.allRecords := allRecords;
    pack.params := params;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link as TFHIRBundle;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

function TFhirThreadedClient.search(atype: TFhirResourceType; allRecords: boolean; params: string): TFHIRBundle;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdSearch;
    pack.resourceType := aType;
    pack.allRecords := allRecords;
    pack.paramString := params;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link as TFHIRBundle;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

function TFhirThreadedClient.search(allRecords: boolean; params : TStringList): TFHIRBundle;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdSearch;
    pack.resourceType := frtNull;
    pack.allRecords := allRecords;
    pack.params := params;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link as TFHIRBundle;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

function TFhirThreadedClient.searchAgain(link: String): TFHIRBundle;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdSearch;
    pack.url := link;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link as TFHIRBundle;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

function TFhirThreadedClient.searchPost(atype: TFhirResourceType; allRecords: boolean; params : TStringList; resource: TFhirResource): TFHIRBundle;
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TFhirThreadedClient.SetLogger(const Value: TFHIRClientLogger);
begin
  inherited;
  FInternal.Logger := Value.link;
end;

function TFhirThreadedClient.transaction(bundle: TFHIRBundle): TFHIRBundle;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdTransaction;
    pack.resource := bundle.link;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link as TFHIRBundle;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

function TFhirThreadedClient.updateResource(resource: TFhirResource): TFHIRResource;
var
  pack : TFHIRThreadedClientPackage;
begin
  pack := TFHIRThreadedClientPackage.create;
  try
    pack.command := fcmdUpdate;
    pack.resource := resource.link;
    pack.Thread := TThreadClientThread.create(FInternal.link, pack.Link);
    wait(pack);
    result := pack.result.link as TFHIRResource;
    LastUrl := pack.lastUrl;
  finally
    pack.free;
  end;
end;

procedure TFhirThreadedClient.wait(Package : TFHIRThreadedClientPackage);
var
  stop : boolean;
begin
  repeat
    stop := false;
    FEvent(self, stop);
    if stop then
    begin
      try
        Package.Thread.Terminate;
        FInternal.terminate;
      except
      end;
      abort;
    end;
  until Package.Done;
  if Package.error <> '' then
    raise Exception.Create(Package.error);
end;

{ TFHIRThreadedClientPackage }

destructor TFHIRThreadedClientPackage.Destroy;
begin
  FResource.Free;
  FResult.Free;
  inherited;
end;

function TFhirThreadedClientPackage.Link: TFhirThreadedClientPackage;
begin
  result := TFhirThreadedClientPackage(inherited Link);
end;

procedure TFhirThreadedClientPackage.SetResource(const Value: TFhirResource);
begin
  FResource.Free;
  FResource := Value;
end;

procedure TFHIRThreadedClientPackage.SetResult(const Value: TFhirResource);
begin
  FResult.Free;
  FResult := Value;
end;

{ TThreadClientThread }

constructor TThreadClientThread.Create(client: TFhirClient; pack: TFhirThreadedClientPackage);
begin
  FClient := client;
  FPackage := pack;
  FreeOnTerminate := true;
  inherited create(false);
end;

destructor TThreadClientThread.Destroy;
begin
  FClient.Free;
  FPackage.Free;
  inherited;
end;

procedure TThreadClientThread.execute;
var
  id : String;
begin
  try
    try
      case FPackage.command of
        fcmdConformanceStmt: FPackage.result := FClient.conformance(FPackage.summary);
        fcmdTransaction : FPackage.result := FCLient.transaction(FPackage.resource as TFHIRBundle);
        fcmdRead : FPackage.result := FClient.readResource(FPackage.ResourceType, FPackage.id);
        fcmdCreate :
          begin
            FPackage.result := FClient.createResource(FPackage.resource, id);
            FPackage.id := id;
          end;
        fcmdSearch :
          if FPackage.FUrl <> '' then
            FPackage.result := FClient.searchAgain(FPackage.url)
          else if FPackage.resourceType = frtNull then
            if FPackage.params <> nil then
              FPackage.result := FClient.search(FPackage.allRecords, FPackage.params)
            else
              FPackage.result := FClient.search(FPackage.allRecords, FPackage.paramString)
          else
            if FPackage.params <> nil then
              FPackage.result := FClient.search(FPackage.resourceType, FPackage.allRecords, FPackage.params)
            else
              FPackage.result := FClient.search(FPackage.resourceType, FPackage.allRecords, FPackage.paramString);
        fcmdUpdate : FPackage.result := FClient.updateResource(FPackage.Resource);
        fcmdOperation :
          if FPackage.id <> '' then
            FPackage.result := FClient.operation(FPackage.resourceType, FPackage.id, FPackage.name, FPackage.resource as TFhirParameters)
          else
            FPackage.result := FClient.operation(FPackage.resourceType, FPackage.name, FPackage.resource as TFhirParameters);
      else
        raise Exception.Create('Not done yet');
      end;
    except
      on e : exception do
        FPackage.error := e.Message;
    end;
    FPackage.FLastURL := FClient.FLastURL;
  finally
    FPackage.FDone := true;
  end;
end;

{ TFHIRClientLogger }

function TFHIRClientLogger.Link: TFHIRClientLogger;
begin
  result := TFHIRClientLogger(inherited Link);
end;


procedure TFHIRClientLogger.logExchange(verb, url, status, requestHeaders,
  responseHeaders: String; request, response: TStream);
begin

end;

{ TNullLogger }

procedure TNullLogger.logExchange(verb, url, status, requestHeaders, responseHeaders: String; request, response: TStream);
begin
  // nothing
end;

end.

