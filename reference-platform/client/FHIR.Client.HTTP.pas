unit FHIR.Client.HTTP;

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
  SysUtils, Classes,
  FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Json,
  IdHTTP, IdSSLOpenSSL, IdComponent,
  {$IFNDEF OSX}FHIR.Web.WinInet, {$ENDIF}
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.Base.Common, FHIR.Client.Base, FHIR.Base.Lang,
  FHIR.Smart.Utilities;

type
  TFhirHTTPClientHTTPVerb = (httpGet, httpPost, httpPut, httpDelete, httpOptions, httpPatch);

  // use only in one thread at a time
  TFHIRHTTPCommunicator = class (TFHIRClientCommunicator)
  private
    FUrl : String;
    FProxy: String;
    FUseIndy: boolean;
    FCertFile: String;
    FPassword: String;
    FUsername: String;
    FCertKey: String;
    FCertPWord: String;
    FTerminated : boolean;
    FTimeout: cardinal;
    FBytesToTransfer: Int64;

    indy : TIdHTTP;
    ssl : TIdSSLIOHandlerSocketOpenSSL;
    {$IFDEF MSWINDOWS}
    http : TFslWinInetClient;
    {$ENDIF}

    procedure createClient;
    function exchangeIndy(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; headers : THTTPHeaders): TStream;
    {$IFDEF MSWINDOWS}
    function exchangeHTTP(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; headers : THTTPHeaders): TStream;
    {$ENDIF}
    function makeMultipart(stream: TStream; streamName: string; params: TStringList; var mp : TStream) : String;
    function exchange(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; headers : THTTPHeaders) : TStream;
    function GetHeader(name : String) : String;
    procedure setHeader(name, value : String);
    function makeUrl(tail : String) : String;
    function makeUrlPath(tail : String) : String;
    function serialise(resource : TFhirResourceV):TStream; overload;
    function fetchResource(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; headers : THTTPHeaders) : TFhirResourceV;

    procedure SetTimeout(const Value: cardinal);
    procedure SetUseIndy(const Value: boolean);
    procedure SetCertFile(const Value: String);
    procedure SetCertPWord(const Value: String);
    procedure SetCertKey(const Value: String);
    procedure getSSLpassword(var Password: String);

    function authoriseByOWinIndy(server, username, password : String): TJsonObject;
    function authoriseByOWinHttp(server, username, password : String): TJsonObject;

    procedure HTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure HTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure HTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
  public
    constructor Create(url : String); overload;
    destructor Destroy; override;
    function link: TFHIRHTTPCommunicator; overload;

    property UseIndy : boolean read FUseIndy write SetUseIndy; // set this to true for a service, but you may have problems with SSL
    property proxy : String read FProxy write FProxy;
    property certFile : String read FCertFile write SetCertFile;
    property certKey : String read FCertKey write SetCertKey;
    property certPWord : String read FCertPWord write SetCertPWord;
    property username : String read FUsername write FUsername;
    property password : String read FPassword write FPassword;
    property timeout : cardinal read FTimeout write SetTimeout;

    function address : String; override;

    function conformanceV(summary : boolean) : TFHIRResourceV; override;
    function transactionV(bundle : TFHIRResourceV) : TFHIRResourceV; override;
    function createResourceV(resource : TFHIRResourceV; var id : String) : TFHIRResourceV; override;
    function readResourceV(atype : TFhirResourceTypeV; id : String) : TFHIRResourceV; override;
    function updateResourceV(resource : TFHIRResourceV) : TFHIRResourceV; overload; override;
    procedure deleteResourceV(atype : TFHIRResourceTypeV; id : String); override;
    function searchV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; overload; override;
    function searchPostV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList; resource : TFHIRResourceV) : TFHIRResourceV; override;
    function searchAgainV(link : String) : TFHIRResourceV; overload; override;
    function operationV(atype : TFHIRResourceTypeV; opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function operationV(atype : TFHIRResourceTypeV; id, opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function historyTypeV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; override;

    // special case that gives direct access to the communicator...
    function customGet(path : String; headers : THTTPHeaders) : TFslBuffer; override;
    function customPost(path : String; headers : THTTPHeaders; body : TFslBuffer) : TFslBuffer; override;
    procedure terminate; override;

    // special functions
    procedure authoriseByOWin(server, username, password : String);
  end;

const
  CODES_TFhirHTTPClientHTTPVerb : array [TFhirHTTPClientHTTPVerb] of String = ('GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH');

implementation

function readIdFromLocation(resType, location : String) : String;
var
  a : TArray<String>;
begin
  a := location.split(['/']);
  if length(a) < 2 then
    raise EFHIRException.create('Unable to process location header (too short)');
  if a[length(a)-2] = '_history' then
  begin
    if length(a) < 4 then
      raise EFHIRException.create('Unable to process location header (too short for a version specific location). Location: '+location);
    if a[length(a)-4] <> resType  then
      raise EFHIRException.create('Unable to process location header (version specific, but resource doesn''t match). Location: '+location);
    result := a[length(a)-3]; // 1 for offset, 2 for _history and vers
  end
  else if a[length(a)-2] <> resType then
    raise EFHIRException.create('Unable to process location header (resource doesn''t match). Location: '+location);
  result := a[length(a)-1];
end;


{ TFHIRHTTPCommunicator }

constructor TFHIRHTTPCommunicator.Create(url: String);
begin
  inherited create;
  FUrl := url;
  {$IFNDEF WINDOWS}
  FUseIndy := true;
  {$ENDIF}
end;

destructor TFHIRHTTPCommunicator.destroy;
begin
  ssl.Free;
  indy.free;
  {$IFDEF MSWINDOWS}
  http.Free;
  {$ENDIF}
  inherited;
end;

function TFHIRHTTPCommunicator.link: TFHIRHTTPCommunicator;
begin
  result := TFHIRHTTPCommunicator(inherited Link);
end;

function TFHIRHTTPCommunicator.address: String;
begin
  result := FUrl;
end;

procedure TFHIRHTTPCommunicator.SetUseIndy(const Value: boolean);
begin
  {$IFDEF MSWINDOWS}
  FUseIndy := Value;
  {$ELSE}
  // ignore...?
  {$ENDIF}
end;

procedure TFHIRHTTPCommunicator.SetTimeout(const Value: cardinal);
begin
  FTimeout := Value;
  createClient;
  if FUseIndy then
  begin
    indy.IOHandler.ReadTimeout := Value;
    indy.ReadTimeout := Value;
  end;
end;

procedure TFHIRHTTPCommunicator.SetCertFile(const Value: String);
begin
  FCertFile := Value;
  indy.free;
  indy := nil;
  {$IFDEF MSWINDOWS}
  http.Free;
  http := nil;
  {$ENDIF}
end;

procedure TFHIRHTTPCommunicator.SetCertKey(const Value: String);
begin
  FCertKey := Value;
  indy.free;
  indy := nil;
  {$IFDEF MSWINDOWS}
  http.Free;
  http := nil;
  {$ENDIF}
end;

procedure TFHIRHTTPCommunicator.SetCertPWord(const Value: String);
begin
  FCertPWord := Value;
  indy.free;
  indy := nil;
  {$IFDEF MSWINDOWS}
  http.Free;
  http := nil;
  {$ENDIF}
end;

procedure TFHIRHTTPCommunicator.getSSLpassword(var Password: String);
begin
  Password := FCertPWord;
end;

function TFHIRHTTPCommunicator.makeMultipart(stream: TStream; streamName: string; params: TStringList; var mp : TStream) : String;
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

function TFHIRHTTPCommunicator.makeUrl(tail: String): String;
begin
  result := FURL;
  if not result.EndsWith('/') and (tail <> '') then
    result := result + '/';
  result := result + tail;
end;

function TFHIRHTTPCommunicator.makeUrlPath(tail: String): String;
var
  s : String;
begin
  StringSplit(FURL, '://', s, result);
  StringSplit(result, '://', s, result);
  if not result.EndsWith('/') then
    result := result + '/';
  result := result + tail;
end;

function TFHIRHTTPCommunicator.exchange(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; headers : THTTPHeaders) : TStream;
var
  h : THTTPHeaders;
begin
  if Assigned(FClient.OnProgress) then
    FClient.OnProgress(FClient, 'Requesting', 0, false);
  FHeaders.accept := '';
  FHeaders.prefer := '';
  createClient;
  FClient.LastURL := url;
  try
    if FUseIndy then
      result := exchangeIndy(url, verb, source, headers)
    {$IFDEF MSWINDOWS}
    else
      result := exchangeHTTP(url, verb, source, headers)
    {$ENDIF}
  finally
    if Assigned(FClient.OnProgress) then
      FClient.OnProgress(FClient, 'Done', 100, true); // just make sure this fires with a done.
  end;
end;

procedure TFHIRHTTPCommunicator.createClient;
begin
  if FUseIndy then
  begin
    if (indy = nil) then
    begin
      indy := TIdHTTP.create(nil);
      indy.OnWork := HTTPWork;
      indy.OnWorkBegin := HTTPWorkBegin;
      indy.OnWorkEnd := HTTPWorkEnd;
      indy.HandleRedirects := true;
      if (proxy <> '') then
      begin
        try
          indy.ProxyParams.ProxyServer := proxy.Split([':'])[0];
          indy.ProxyParams.ProxyPort := StrToInt(proxy.Split([':'])[1]);
        except
          raise EFHIRException.create('Unable to process proxy "'+proxy+'" - use address:port');
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
      raise EFHIRException.create('Certificates are not supported with winInet yet'); // have to figure out how to do that ...
    http := TFslWinInetClient.Create;
    http.UseWindowsProxySettings := true;
    http.UserAgent := 'FHIR Client';
  end;
  {$ENDIF}
end;

function TFHIRHTTPCommunicator.serialise(resource: TFhirResourceV): TStream;
var
  ok : boolean;
  comp : TFHIRComposer;
begin
  ok := false;
  result := TBytesStream.create;
  try
    comp := FClient.makeComposer(FClient.format, OutputStyleNormal);
    try
      comp.Compose(result, resource);
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

function TFHIRHTTPCommunicator.exchangeIndy(url : String; verb : TFhirHTTPClientHTTPVerb; source : TStream; headers : THTTPHeaders) : TStream;
var
  comp : TFHIRParser;
  ok : boolean;
  cnt : String;
  op : TFHIROperationOutcomeW;
begin
  indy.Request.ContentType := MIMETYPES_TFHIRFormat_Version[FClient.format, FCLient.version]+'; charset=utf-8';
  indy.Request.Accept := indy.Request.ContentType;
  if headers.contentType <> '' then
    indy.Request.ContentType := headers.contentType;
  if headers.accept <> '' then
    indy.Request.Accept := headers.accept;
  if headers.prefer <> '' then
    indy.Request.CustomHeaders.values['Prefer'] := headers.prefer;

  if FClient.smartToken <> nil then
    indy.Request.CustomHeaders.values['Authorization'] := 'Bearer '+FClient.smartToken.accessToken;

  if password <> '' then
  begin
    indy.Request.BasicAuthentication:= true;
    indy.Request.UserName := UserName;
    indy.Request.Password := Password;
  end;

  if FCLient.provenance <> nil then
    indy.Request.CustomHeaders.values['X-Provenance'] := ProvenanceString
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
        raise EFHIRException.create('Unknown HTTP method '+inttostr(ord(verb)));
      end;

      FClient.Logger.logExchange(CODES_TFhirHTTPClientHTTPVerb[verb], url, indy.ResponseText, indy.Request.RawHeaders.Text, indy.Response.RawHeaders.Text, source, result);
      if (indy.ResponseCode < 200) or (indy.ResponseCode >= 300) Then
        raise EFHIRException.create('unexpected condition');
      ok := true;
      if (result <> nil) then
         result.Position := 0;
      FHeaders.contentType := indy.Response.ContentType;
      FHeaders.location := indy.Response.Location;
      FHeaders.LastOperationId := indy.Response.RawHeaders.Values['X-Request-Id'];
      FHeaders.Progress := indy.Response.RawHeaders.Values['X-Progress'];
      FHeaders.contentLocation := indy.Response.RawHeaders.Values['Content-Location'];
      FClient.LastStatus := indy.ResponseCode;
      FClient.LastStatusMsg := indy.ResponseText;
    except
      on E:EIdHTTPProtocolException do
      begin
        FClient.Logger.logExchange(CODES_TFhirHTTPClientHTTPVerb[verb], url, indy.ResponseText, indy.Request.RawHeaders.Text, indy.Response.RawHeaders.Text, source, result);
        cnt := e.ErrorMessage;
        if cnt = '' then
          cnt := e.message;
        FHeaders.contentType := indy.Response.ContentType;
        FHeaders.location := indy.Response.Location;
        FHeaders.contentLocation := indy.Response.RawHeaders.Values['Content-Location'];
        FHeaders.LastOperationId := indy.Response.RawHeaders.Values['X-Request-Id'];
        FHeaders.Progress := indy.Response.RawHeaders.Values['X-Request-Id'];
        FClient.LastStatus := indy.ResponseCode;
        FClient.LastStatusMsg := indy.ResponseText;

        if StringFind(cnt, 'OperationOutcome') > 0 then
        begin
          removeBom(cnt);
          comp := FClient.makeParser(FClient.format);
          try
            comp.source := TStringStream.create(cnt);
            comp.Parse;
            if (comp.resource <> nil) and (comp.resource.fhirType = 'OperationOutcome') then
            begin
              op := opWrapper.create(comp.resource.Link);
              try
                if (op.hasText) then
                  Raise EFHIRClientException.create(op.text, op.link)
                else
                  raise EFHIRException.create(cnt)
              finally
                op.Free;
              end;
            end
            else
              raise EFHIRException.create(cnt)
          finally
            comp.source.free;
            comp.Free;
          end;
        end
        else
          raise EFHIRException.create(cnt)
      end;
      on e : exception do
        raise;
    end;
  finally
    if not ok then
      result.free;
  end;
end;

{$IFDEF MSWINDOWS}
function TFHIRHTTPCommunicator.exchangeHTTP(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; headers : THTTPHeaders): TStream;
var
  ok : boolean;
  op : TFhirOperationOutcomeW;
  code : integer;
  procedure processException;
  var
    cnt : string;
    p : TFHIRParser;
  begin
    cnt := http.Response.AsText;
    if StringFind(cnt, 'OperationOutcome') > 0 then
    begin
      removeBom(cnt);
      p := FClient.makeParser(FClient.format);
      try
        p.source := TBytesStream.create(http.response.AsBytes);
        p.Parse;
        if (p.resource <> nil) and (p.resource.fhirType = 'OperationOutcome') then
        begin
          op := opWrapper.create(p.resource.link);
          try
            if (op.hasText) then
              Raise EFHIRClientException.create(op.text, op.link)
            else
              raise EFHIRException.create(cnt)
          finally
            op.Free;
          end;
        end
        else
          raise EFHIRException.create(cnt)
      finally
        p.source.free;
        p.Free;
      end;
    end
    else if cnt = '' then
      raise EFHIRException.create(http.ResponseCode+' ' +http.ResponseText)
    else
      raise EFHIRException.create(cnt)
  end;
begin
  http.RequestType := MIMETYPES_TFHIRFormat_Version[FClient.format, FCLient.version]+'; charset=utf-8';
  http.ResponseType := http.RequestType;
  if headers.contentType <> '' then
    http.RequestType := headers.contentType;

  if FClient.provenance <> nil then
    http.Headers['X-Provenance'] := ProvenanceString
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
          http.Request := TFslBuffer.create;
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

      http.Response := TFslBuffer.create;
      http.Execute;

      code := StrToInt(http.ResponseCode);
      FClient.LastStatus := code;
      FClient.LastStatusMsg := http.ResponseText;
      if (code < 200) or (code >= 600) Then
        raise EFHIRException.create('unexpected condition');
    if (code >= 300) and (code < 400) then
      url := http.getResponseHeader('Location');
  until (code < 300) or (code >= 400);

  FHeaders.contentType := http.Headers['Content-Type'];
  FHeaders.location := http.Headers['Location'];
  FHeaders.contentLocation := http.Headers['Content-Location'];
  FHeaders.LastOperationId := http.Headers['X-Request-Id'];
  FHeaders.progress := http.Headers['X-Progress'];

  if code >= 300 then
    processException;
      ok := true;
      result := TMemoryStream.Create;
  // if this breaks, the stream leaks
      http.Response.SaveToStream(result);
      result.Position := 0;
  end;
{$ENDIF}

function TFHIRHTTPCommunicator.fetchResource(url: String; verb: TFhirHTTPClientHTTPVerb; source: TStream; headers : THTTPHeaders): TFhirResourceV;
var
  ret : TStream;
  p : TFHIRParser;
begin
  FTerminated := false;
  ret := exchange(url, verb, source, headers);
  if FTerminated then
    abort;
  try
    if ret.Size = 0 then
      result := nil
    else
    begin
      p := FClient.makeParser(FClient.format);
      try
        p.source := ret;
        p.parse;
        if (p.resource = nil) then
          raise EFHIRException.create('No response bundle');
        result := p.resource.link;
      finally
        p.free;
      end;
    end;
  finally
    ret.free;
  end;
end;

procedure TFHIRHTTPCommunicator.setHeader(name, value: String);
begin
  createClient;
  if FUseIndy then
    indy.Request.RawHeaders.Values[name] := value
  {$IFDEF MSWINDOWS}
  else
    http.Headers.AddOrSetValue(name, value);
  {$ENDIF}
end;

function TFHIRHTTPCommunicator.GetHeader(name: String): String;
begin
  createClient;
  if FUseIndy then
    result := indy.Response.RawHeaders.Values[name]
  {$IFDEF MSWINDOWS}
  else
    result := http.getResponseHeader(name);
  {$ENDIF}
end;

procedure TFHIRHTTPCommunicator.terminate;
begin
  if not FUseIndy then
    raise EFHIRException.create('Cancel not supported')
  else
  begin
    FTerminated := true;
    if FUseIndy and (indy <> nil) then
      indy.Disconnect;
  end;
end;

function TFHIRHTTPCommunicator.conformanceV(summary : boolean): TFHIRResourceV;
var
  headers : THTTPHeaders;
begin
  if summary then
    result := FetchResource(MakeUrl('metadata')+'?_summary=true', httpGet, nil, headers)
  else
    result := FetchResource(MakeUrl('metadata'), httpGet, nil, headers);
end;

function TFHIRHTTPCommunicator.transactionV(bundle : TFHIRResourceV) : TFHIRResourceV;
Var
  src : TStream;
  headers : THTTPHeaders;
begin
  src := serialise(bundle);
  try
    result := fetchResource(makeUrl(''), httpPost, src, headers);
  finally
    src.free;
  end;
end;

function TFHIRHTTPCommunicator.createResourceV(resource: TFhirResourceV; var id : String): TFHIRResourceV;
Var
  src : TStream;
  headers : THTTPHeaders;
begin
  src := serialise(resource);
  try
    result := nil;
    try
      result := fetchResource(MakeUrl(resource.fhirType), httpPost, src, headers);
      id := readIdFromLocation(resource.fhirType, getHeader('Location'));
      result.link;
    finally
      result.free;
    end;
  finally
    src.free;
  end;
end;

function TFHIRHTTPCommunicator.readResourceV(atype: TFhirResourceTypeV; id: String): TFHIRResourceV;
var
  headers : THTTPHeaders;
begin
  result := nil;
  try
    result := fetchResource(MakeUrl(AType+'/'+id), httpGet, nil, headers);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRHTTPCommunicator.updateResourceV(resource : TFhirResourceV) : TFHIRResourceV;
Var
  src : TStream;
  headers : THTTPHeaders;
  vId : String;
begin
  vId := getResourceVersionId(resource);
  if vId <> '' then
    SetHeader('Content-Location', MakeUrlPath(resource.fhirType+'/'+resource.id+'/history/'+vId));

  src := serialise(resource);
  try
    result := fetchResource(MakeUrl(resource.fhirType+'/'+resource.id), httpPut, src, headers);
  finally
    src.free;
  end;
end;

procedure TFHIRHTTPCommunicator.deleteResourceV(atype : TFhirResourceTypeV; id : String);
var
  headers : THTTPHeaders;
begin
  exchange(MakeUrl(aType+'/'+id), httpDelete, nil, headers).free;
end;

function TFHIRHTTPCommunicator.searchV(atype: TFhirResourceTypeV; allRecords: boolean; params: string): TFHIRResourceV;
var
  s : String;
  bnd : TFHIRResourceV;
  bh : TFHIRBundleW;
  headers : THTTPHeaders;
begin
  bh := FClient.BundleFactory.Create(fetchResource(makeUrl(aType)+'?'+params, httpGet, nil, headers));
  try
    if bh.resource.fhirType <> 'Bundle' then
      raise EFHIRException.create('Found a resource of type '+bh.resource.fhirType+' expecting a Bundle');
    s := bh.next;
    while AllRecords and (s <> '') do
    begin
      bnd := fetchResource(s, httpGet, nil, headers);
      try
        bh.addEntries(bnd);
        s := bh.next(bnd);
      finally
        bnd.free;
      end;
    end;
    if allRecords then
      bh.clearLinks;
    result := bh.resource.Link;
  finally
    bh.Free;
  end;
end;

function TFHIRHTTPCommunicator.searchAgainV(link: String): TFHIRResourceV;
var
  headers : THTTPHeaders;
begin
  result := fetchResource(link, httpGet, nil, headers) as TFHIRResourceV;
end;

function TFHIRHTTPCommunicator.searchPostV(atype: TFhirResourceTypeV; allRecords: boolean; params: TStringList; resource: TFhirResourceV): TFHIRResourceV;
var
  src, frm : TStream;
  ct : String;
  headers : THTTPHeaders;
begin
  src := serialise(resource);
  try
    src.Position := 0;
    ct := makeMultipart(src, 'src', params, frm);
    try
      result := fetchResource(makeUrl(aType)+'/_search', httpPost, frm, headers) as TFHIRResourceV;
    finally
      frm.Free;
    end;
  finally
    src.free;
  end;
end;

function TFHIRHTTPCommunicator.operationV(atype : TFhirResourceTypeV; opName : String; params : TFHIRResourceV) : TFHIRResourceV;
Var
  src : TStream;
  headers : THTTPHeaders;
begin
  src := serialise(params);
  try
    src.Position := 0;
    if aType = '' then
      result := fetchResource(makeUrl('$'+opName), httpPost, src, headers)
    else
    result := fetchResource(makeUrl(aType)+'/$'+opName, httpPost, src, headers);
  finally
    src.free;
  end;
end;

function TFHIRHTTPCommunicator.operationV(atype : TFhirResourceTypeV; id, opName : String; params : TFHIRResourceV) : TFHIRResourceV;
Var
  src : TStream;
  headers : THTTPHeaders;
begin
  if params = nil then
  begin
    result := fetchResource(makeUrl(aType)+'/'+id+'/$'+opName, httpGet, nil, headers);
  end
  else
  begin
    src := serialise(params);
    try
      src.Position := 0;
      result := fetchResource(makeUrl(aType)+'/'+id+'/$'+opName, httpPost, src, headers);
    finally
      src.free;
    end;
  end;
end;

function TFHIRHTTPCommunicator.historyTypeV(atype: TFhirResourceTypeV; allRecords: boolean; params: string): TFHIRResourceV;
var
  s : String;
  feed : TFHIRResourceV;
  i : integer;
  headers : THTTPHeaders;
  bh : TFHIRBundleW;
begin
//    client.Request.RawHeaders.Values['Content-Location'] := MakeUrlPath(CODES_TFhirResourceType[resource.resourceType]+'/'+id+'/history/'+ver);
  notify('Fetch History for '+aType);
  bh := FClient.BundleFactory.Create(fetchResource(makeUrl(aType)+'/_history?'+params, httpGet, nil, headers) as TFHIRResourceV);
  try
    s := bh.next;
    i := 1;
    while AllRecords and (s <> '') do
    begin
      inc(i);
      notify('Fetch History for '+aType+' page '+inttostr(i));
      feed := fetchResource(s, httpGet, nil, headers) as TFHIRResourceV;
      try
        bh.addEntries(feed);
        s := bh.next(feed);
      finally
        feed.free;
      end;
    end;
    if allRecords then
      bh.clearLinks;;
    result := bh.resource.Link;
  finally
    bh.Free;
  end;
end;

procedure TFHIRHTTPCommunicator.HTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if FBytesToTransfer = 0 then // No Update File
    Exit;

  if Assigned(FClient.OnProgress) then
    FClient.OnProgress(FClient, 'Downloading', Round((AWorkCount / FBytesToTransfer) * 100), false);
end;

procedure TFHIRHTTPCommunicator.HTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  FBytesToTransfer := AWorkCountMax;

  if Assigned(FClient.OnProgress) then
    FClient.OnProgress(FClient, 'Download Starting', 0, false);
end;

procedure TFHIRHTTPCommunicator.HTTPWorkEnd(Sender: TObject;
  AWorkMode: TWorkMode);
begin
  FBytesToTransfer := 0;
  if Assigned(FClient.OnProgress) then
    FClient.OnProgress(FClient, 'Downloaded', 100, true);
end;

function TFHIRHTTPCommunicator.customGet(path: String; headers: THTTPHeaders): TFslBuffer;
var
  ret : TStream;
begin
  ret := exchange(URLPath([Furl, path]), httpGet, nil, headers);
  try
    result := TFslBuffer.Create;
    try
      result.LoadFromStream(ret);
      result.link;
    finally
      result.Free;
    end;
  finally
    ret.Free;
  end;
end;

function TFHIRHTTPCommunicator.customPost(path: String; headers: THTTPHeaders; body : TFslBuffer): TFslBuffer;
var
  req : TMemoryStream;
  ret : TStream;
begin
  req := TMemoryStream.Create;
  try
    body.SaveToStream(req);
    req.Position := 0;
    ret := exchange(Furl+'/'+path, httpPost, req, headers);
    try
      result := TFslBuffer.Create;
      try
        result.LoadFromStream(ret);
        result.link;
      finally
        result.Free;
      end;
    finally
      ret.Free;
    end;
  finally
    req.Free;
  end;
end;

procedure TFHIRHTTPCommunicator.authoriseByOWin(server, username, password: String);
var
  token : TJsonObject;
begin
  if FUseIndy then
    token := authoriseByOWinIndy(server, username, password)
  else
    token := authoriseByOWinHttp(server, username, password);
  try
    FClient.smartToken := TClientAccessToken.Create;
    FClient.smartToken.accessToken := token.str['access_token'];
    FClient.smartToken.expires := now + (StrToInt(token.num['expires_in']) * DATETIME_SECOND_ONE);
  finally
    token.Free;
  end;
end;

function TFHIRHTTPCommunicator.authoriseByOWinHttp(server, username, password: String): TJsonObject;
begin
  raise EFHIRException.create('Not done yet');
end;

function TFHIRHTTPCommunicator.authoriseByOWinIndy(server, username, password: String): TJsonObject;
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
        raise EFHIRException.create('unexpected condition');
      resp.Position := 0;
      result := TJSONParser.Parse(resp);
    finally
      resp.Free;
    end;
  finally
    ss.Free;
  end;
end;

(*
function TFHIRHTTPCommunicator.Convert(stream: TStream): TStream;
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
*)




end.
