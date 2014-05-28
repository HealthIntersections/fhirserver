Unit FHIRRestServer;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

Interface

Uses
  SysUtils, Classes, IniFiles, ActiveX, AltovaXMLLib_TLB, System.Generics.Collections,

  EncodeSupport, GuidSupport, DateSupport, BytesSupport, StringSupport,

  AdvBuffers, AdvObjectLists, AdvStringMatches, AdvZipParts, AdvZipReaders, AdvVCLStreams, AdvMemories,

  kCritSct, ParseMap, TextUtilities, KDBManager, HTMLPublisher, KDBDialects,
  DCPsha256, JSON,

  IdMultipartFormData, IdHeaderList, IdCustomHTTPServer, IdHTTPServer,
  IdTCPServer, IdContext, IdSSLOpenSSL, IdHTTP, IdSoapMime, IdCookie,
  IdZLibCompressorBase, IdCompressorZLib, IdZlib,

  TerminologyServer, SnomedServices, SnomedPublisher, SnomedExpressions, LoincServices, LoincPublisher,
  TerminologyWebServer, AuthServer,

  fhirbase,
  FHIRTypes,
  fhirresources,
  fhirparser,
  fhircomponents,
  fhirparserbase,
  fhirtags,
  fhirconstants,
  fhirsupport,
  fhirAtomFeed,
  FHIRClient,
  FHIRLang,
  FHIROperation,
  FHIRDataStore;

Type
  ERestfulAuthenticationNeeded = class (ERestfulException)
  private
    FMsg : String;
  public
    Constructor Create(Const sSender, sMethod, sReason, sMsg : String; aStatus : word); Overload; Virtual;
    Property Msg : String read FMsg;
  end;

  TFhirWebServer = class;

  TFhirServerMaintenanceThread = class (TThread)
  private
    FServer : TFhirWebServer;
    FLastSweep : TDateTime;
  protected
    procedure Execute; override;
  public
    constructor create(server : TFHIRWebServer);
  end;

  TFhirWebServer = Class(TAdvObject)
  Private
    FIni : TIniFile;
    FLock : TCriticalSection;
    FPort : Integer;
    FSSLPort : Integer;
    FCertFile : String;
    FSSLPassword : String;
//    FBaseURL : String;
//    FProxy : String;
    FBasePath : String;
    FSecurePath : String;
    FHost : String;
    FSpecPath : String;
    FAltPath : String;
    FName : String;
    FPlainServer : TIdHTTPServer;
    FSSLServer : TIdHTTPServer;
    FIOHandler: TIdServerIOHandlerSSLOpenSSL;

    FClientCount : Integer;

    FFhirStore : TFHIRDataStore;
    FFacebookLike : boolean;
    FTerminologyWebServer : TTerminologyWebServer;
    FThread : TFhirServerMaintenanceThread;
    FActive : boolean;
    FAuthServer : TAuth2Server;

    function OAuthPath(secure : boolean):String;

    function BuildCompartmentList(user : TFHIRUserStructure) : String;

    function GetResource(session : TFhirSession; rtype : TFhirResourceType; lang, id, ver : String) : TFhirResource;
    function transform(resource : TFhirResource; lang, xslt : String) : string;
    procedure HandleWebUIRequest(request : TFHIRRequest; response : TFHIRResponse);
    procedure HandleWebQuestionnaire(request : TFHIRRequest; response : TFHIRResponse);

    function SpecFile(path : String) : String;
    function AltFile(path : String) : String;
    Procedure ReturnSpecFile(response : TIdHTTPResponseInfo; stated, path : String);
    Procedure ReturnProcessedFile(response : TIdHTTPResponseInfo; named, path : String; variables: TDictionary<String, String> = nil);
    Procedure ReadTags(Headers: TIdHeaderList; Request : TFHIRRequest); overload;
    Procedure ReadTags(header : String; Request : TFHIRRequest); overload;
    function CheckSessionOK(session : TFhirSession; ip : string) : Boolean;
    Function BuildFhirHomePage(comps, lang, host, sBaseURL : String; session : TFhirSession): String;
    Function BuildFhirAuthenticationPage(lang, host, path, msg : String; secure : boolean) : String;
    Function BuildFhirUploadPage(lang, host, sBaseURL : String; aType : TFHIRResourceType; session : TFhirSession) : String;
    Procedure CreatePostStream(AContext: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream);
    Procedure ParseAuthenticationHeader(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    Procedure PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure : Boolean; path : String);
    Procedure ProcessOutput(oRequest : TFHIRRequest; oResponse : TFHIRResponse; response: TIdHTTPResponseInfo; relativeReferenceAdjustment : integer; pretty, gzip : boolean);
    function extractFileData(const request : TStream; const contentType, name: String; var sContentType : String; var params : String): TStream;
    Procedure StartServer(active : boolean);
    Procedure StopServer;
    Function ProcessZip(lang : String; oStream : TStream) : TFHIRAtomFeed;
    procedure SSLPassword(var Password: String);
    procedure SendError(response: TIdHTTPResponseInfo; status : word; format : TFHIRFormat; lang, message, url : String; session : TFhirSession; addLogins : boolean; path : String; relativeReferenceAdjustment : integer);
    Procedure ProcessRequest(request : TFHIRRequest; response : TFHIRResponse);
    function BuildRequest(lang, sBaseUrl, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept, sCookie: String; oPostStream: TStream; oResponse: TFHIRResponse;     var aFormat: TFHIRFormat; var redirect: boolean; formparams: String; bAuth, secure : Boolean; out relativeReferenceAdjustment : integer; var pretty : boolean): TFHIRRequest;
    procedure DoConnect(AContext: TIdContext);
    procedure DoDisconnect(AContext: TIdContext);
    Function WebDesc : String;
    function EndPointDesc : String;
    procedure GetWebUILink(resource : TFhirResource; base, id, ver : String; var link, text : String);
  Public
    Constructor Create(ini : TFileName; db : TKDBManager; Name : String; terminologyServer : TTerminologyServer);
    Destructor Destroy; Override;

    Procedure Start(active : boolean);
    Procedure Stop;
    Procedure Transaction(stream : TStream);
  End;

Implementation


Uses
  Windows, Registry,

  AdvExceptions,

  FHIRUtilities,

  FileSupport,
  FaceBookSupport;

Function GetMimeTypeForExt(AExt: String): String;
Var
  fReg: TRegistry;
Begin
  AExt := lowercase(AExt);
  if (AExt = '.css') Then
    result := 'text/css'
  Else if AExt = '.ico' Then
    result := 'image/x-icon'
  Else if AExt = '.png' Then
    result := 'image/png'
  Else if AExt = '.gif' Then
    result := 'image/gif'
  Else if AExt = '.jpg' Then
    result := 'image/jpeg'
  Else if AExt = '.mpg' Then
    result := 'video/mpeg'
  Else if AExt = '.js' Then
    result := 'text/javascript'
  Else
  Begin
    Try
      fReg := TRegistry.Create;
      Try
        fReg.RootKey := HKEY_LOCAL_MACHINE;
        fReg.OpenKeyReadOnly('Software\Classes\' + AExt);
        Result := freg.ReadString('Content Type');
        fReg.CloseKey;
      Finally
        freg.Free;
      End;
    Except
    End;
  End;
  If Result = '' Then
    Result := 'application/octet-stream';
End;

{ TFhirWebServer }

Function ProcessPath(base, path : String): string;
var
  s : String;
begin
  base := base.Substring(0, base.Length-1);
  if path.StartsWith('..\') then
  begin
    s := base;
    while path.StartsWith('..\') do
    begin
      path := path.Substring(3);
      s := ExtractFilePath(s);
      s := s.Substring(0, s.Length-1);
    end;
    result := IncludeTrailingPathDelimiter(s)+IncludeTrailingPathDelimiter(path);
  end
  else
    result := IncludeTrailingPathDelimiter(path);
end;

Constructor TFhirWebServer.Create(ini : TFileName; db : TKDBManager; Name : String; terminologyServer : TTerminologyServer);
var
  s : String;
Begin
  Inherited Create;
  FLock := TCriticalSection.Create('fhir-rest');
  FName := Name;
  FIni := TIniFile.Create(ini);

  write('Load & Cache Store: ');
  FSpecPath := ProcessPath(ExtractFilePath(ini), FIni.ReadString('fhir', 'source', ''));
  FAltPath := ProcessPath(ExtractFilePath(ini), FIni.ReadString('fhir', 'other', ''));

  FTerminologyWebServer := TTerminologyWebServer.create(terminologyServer.Link);

  FFhirStore := TFHIRDataStore.Create(db, FSpecPath, FAltPath, terminologyServer, FINi);
  if FIni.ReadString('web', 'host', '') <> '' then
  begin
    if FIni.ReadString('web', 'base', '') <> '' then
      s := FIni.ReadString('web', 'base', '')
    else
      s := FIni.ReadString('web', 'secure', '');

    FFhirStore.FormalURL := AppendForwardSlash(FIni.ReadString('web', 'host', '')) + s;
  end;
  writeln(inttostr(FFhirStore.TotalResourceCount)+' resources');
  // Basei Web server configuration
  FBasePath := FIni.ReadString('web', 'base', '');
  FSecurePath := FIni.ReadString('web', 'secure', '');
  FPort := FIni.ReadInteger('web', 'http', 0);
  FSSLPort := FIni.ReadInteger('web', 'https', 0);
  FCertFile := FIni.ReadString('web', 'certname', '');
  FSSLPassword := FIni.ReadString('web', 'certpword', '');
  FHost := FIni.ReadString('web', 'host', '');
  FFacebookLike := FIni.ReadString('facebook.com', 'like', '') = '1';

  if FIni.readString('web', 'clients', '') = '' then
    raise Exception.Create('No Authorization file found');
  FAuthServer := TAuth2Server.Create(FIni.readString('web', 'clients', ''), FAltPath, FHost, inttostr(FSSLPort));
  FAuthServer.FHIRStore := FFhirStore.Link;
  FAuthServer.OnProcessFile := ReturnProcessedFile;

  if (FPort <> 0) and (FSSLPort <> 0) then
    writeln('Web Server: http = '+inttostr(FPort)+', https = '+inttostr(FSSLPort))
  else if (FPort <> 0) then
    writeln('Web Server: http = '+inttostr(FPort))
  else if (FSSLPort <> 0) then
    writeln('Web Server: https = '+inttostr(FSSLPort))
  else
    writeln('Web Server not configued');

  if (FBasePath <> '') and (FSecurePath <> '') then
    writeln(' ...paths: open = '+FBasePath+', secure = '+FSecurePath)
  else if (FPort <> 0) then
    writeln(' ...paths: open = '+FBasePath)
  else if (FSSLPort <> 0) then
    writeln(' ...paths: secure = '+FSecurePath)
  else
    writeln(' ...paths: <none>');

//  FAuthRequired := FIni.ReadString('fhir', 'oauth-secure', '') = '1';
//  FAppSecrets := FIni.ReadString('fhir', 'oauth-secrets', '');
End;

Destructor TFhirWebServer.Destroy;
Begin
  FTerminologyWebServer.free;
  FIni.Free;
  FAuthServer.Free;
  FFhirStore.CloseAll;
  FFhirStore.Free;
  FLock.Free;
  Inherited;
End;

procedure TFhirWebServer.DoConnect(AContext: TIdContext);
begin
  CoInitialize(nil);
  FLock.Lock;
  try
    inc(FClientCount);
  finally
    FLock.Unlock;
  end;
end;

procedure TFhirWebServer.DoDisconnect(AContext: TIdContext);
begin
  FLock.Lock;
  try
    dec(FClientCount);
  finally
    FLock.Unlock;
  end;
  CoUninitialize;
end;

function TFhirWebServer.EndPointDesc: String;
begin
  result := '';
  if FBasePath <> '' then
    result := result + ' <li><a href="'+FBasePath+'">Unsecured access at '+FBasePath+'</a> - direct access with no security considerations</li>'#13#10;
  if FSecurePath <> '' then
    result := result + ' <li><a href="'+FSecurePath+'">Secured access at '+FSecurePath+'</a> - Login required</li>'#13#10;
end;

Procedure TFhirWebServer.Start(active : boolean);
Begin
  FActive := active;
  StartServer(active);
  FThread := TFhirServerMaintenanceThread.create(self);
End;

Procedure TFhirWebServer.Stop;
Begin
  FThread.Terminate;
  StopServer;
End;

Procedure TFhirWebServer.StartServer(active : boolean);
Begin
  if FPort > 0 then
  begin
    FPlainServer := TIdHTTPServer.Create(Nil);
    FPlainServer.ServerSoftware := 'Health Intersections FHIR Server';
    FPlainServer.ParseParams := False;
    FPlainServer.DefaultPort := FPort;
    FPlainServer.KeepAlive := False;
    FPlainServer.OnCreatePostStream := CreatePostStream;
    FPlainServer.OnCommandGet := PlainRequest;
    FPlainServer.OnCommandOther := PlainRequest;
    FPlainServer.OnConnect := DoConnect;
    FPlainServer.OnDisconnect := DoDisconnect;
    FPlainServer.Active := active;
  end;
  if FSSLPort > 0 then
  begin
    If Not FileExists(FCertFile) Then
      Raise Exception.Create('SSL Certificate "'+FCertFile+' could not be found');
    If Not FileExists(ChangeFileExt(FCertFile, '.key')) Then
      Raise Exception.Create('SSL Certificate Private Key "'+ChangeFileExt(FCertFile, '.key')+' could not be found');
    FSSLServer := TIdHTTPServer.Create(Nil);
    FSSLServer.ServerSoftware := 'Health Intersections FHIR Server';
    FSSLServer.ParseParams := False;
    FSSLServer.DefaultPort := FSSLPort;
    FSSLServer.KeepAlive := False;
    FSSLServer.OnCreatePostStream := CreatePostStream;
    FIOHandler := TIdServerIOHandlerSSLOpenSSL.Create(Nil);
    FSSLServer.IOHandler := FIOHandler;
    FIOHandler.SSLOptions.Method := sslvSSLv3;
    FIOHandler.SSLOptions.CertFile := FCertFile;
    FIOHandler.SSLOptions.KeyFile := ChangeFileExt(FCertFile, '.key');
    FIOHandler.OnGetPassword := SSLPassword;
    FSSLServer.OnCommandGet := SecureRequest;
    FSSLServer.OnCommandOther := SecureRequest;
    FSSLServer.OnConnect := DoConnect;
    FSSLServer.OnDisconnect := DoDisconnect;
    FSSLServer.OnParseAuthentication := ParseAuthenticationHeader;
    FSSLServer.Active := active;
  end;
end;

Procedure TFhirWebServer.StopServer;
Begin
  if FSSLServer <> nil then
  begin
    FSSLServer.Active := False;
    FreeAndNil(FSSLServer);
    FreeAndNil(FIOHandler);
  end;
  if FPlainServer <> nil then
  begin
    FPlainServer.Active := False;
    FreeAndNil(FPlainServer);
  end;
End;

procedure TFhirWebServer.Transaction(stream: TStream);
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
begin
  req := TFHIRRequest.Create;
  try
    req.CommandType := fcmdTransaction;
    req.Feed := ProcessZip('en', stream);
    req.session := FFhirStore.CreateImplicitSession('service');
    req.LoadParams('');
    resp := TFHIRResponse.Create;
    try
      ProcessRequest(req, resp);
    finally
      resp.Free;
    end;
  finally
    req.Free;
  end;

end;

function TFhirWebServer.WebDesc: String;
begin
  if (FPort = 0) then
    result := 'HTTPS is supported on Port '+inttostr(FSSLPort)+'.'
  else if FSSLPort = 0 then
    result := 'HTTP is supported on Port '+inttostr(FPort)+'.'
  else
    result := 'HTTPS is supported on Port '+inttostr(FSSLPort)+'. HTTP is supported on Port '+inttostr(FPort)+'.'
end;

Procedure TFhirWebServer.CreatePostStream(AContext: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream);
Begin
  VPostStream := TMemoryStream.Create;
End;

Procedure CheckId(lang, id : String);
var
  i : integer;
begin
  if (Length(id) > 36) then
    Raise ERestfulException.Create('TFhirWebServer', 'SplitId', StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [id]), HTTP_ERR_BAD_REQUEST);
  for i := 1 to length(id) do
    if not CharInSet(id[i], ['a'..'z', '0'..'9', 'A'..'Z', '.', '-']) then
      Raise ERestfulException.Create('TFhirWebServer', 'SplitId', StringFormat(GetFhirMessage('MSG_ID_INVALID', lang), [id, id[i]]), HTTP_ERR_BAD_REQUEST);
end;

procedure TFhirWebServer.ParseAuthenticationHeader(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := AAuthType = 'Bearer';
  VUserName := AAuthType;
  VPassword := AAuthData;
end;

Procedure TFhirWebServer.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
begin
  if FileExists(SpecFile(request.Document)) then
    ReturnSpecFile(response, request.Document, SpecFile(request.Document))
  else if FileExists(AltFile(request.Document)) then
    ReturnSpecFile(response, request.Document, AltFile(request.Document))
  else if request.Document.EndsWith('.hts') and FileExists(ChangeFileExt(AltFile(request.Document), '.html')) then
    ReturnProcessedFile(response, request.Document, ChangeFileExt(AltFile(request.Document), '.html'))
//  else if FileExists(FAltPath+ExtractFileName(request.Document.replace('/', '\'))) then
//    ReturnSpecFile(response, request.Document, FAltPath+ExtractFileName(request.Document.replace('/', '\')))
//  else if FileExists(FSpecPath+ExtractFileName(request.Document.replace('/', '\'))) then
//    ReturnSpecFile(response, request.Document, FSpecPath+ExtractFileName(request.Document.replace('/', '\')))
  else if request.Document.StartsWith(FBasePath, false) then
    HandleRequest(AContext, request, response, false, false, FBasePath)
  else if request.Document.StartsWith(FSecurePath, false) then
    HandleRequest(AContext, request, response, false, true, FSecurePath)
  else if request.Document = '/' then
    ReturnProcessedFile(response, '/homepage.html', AltFile('/homepage.html'))
  else if FTerminologyWebServer.handlesRequest(request.Document) then
    FTerminologyWebServer.Process(AContext, request, response)
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
    writeln('miss: '+request.Document);
  end;
end;

function TFhirWebServer.SpecFile(path : String) : String;
begin
  if path.StartsWith('/') then
    result := FSpecPath+path.Substring(1).Replace('/', '\')
  else
    result := '';
end;

function TFhirWebServer.AltFile(path : String) : String;
begin
  if path.StartsWith('/') then
    result := FAltPath+path.Substring(1).Replace('/', '\')
  else
    result := '';
end;


Procedure TFhirWebServer.SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
begin
  if FileExists(SpecFile(request.Document)) then
    ReturnSpecFile(response, request.Document, SpecFile(request.Document))
  else if FileExists(IncludeTrailingPathDelimiter(FAltPath)+request.Document) then
    ReturnSpecFile(response, request.Document, IncludeTrailingPathDelimiter(FAltPath)+request.Document)
  else if FileExists(AltFile(ExtractFileName(request.Document))) then
    ReturnSpecFile(response, request.Document, AltFile(ExtractFileName(request.Document)))
  else if request.Document.StartsWith(FBasePath, false) then
    HandleRequest(AContext, request, response, true, false, FBasePath)
  else if request.Document.StartsWith(FSecurePath, false) then
    HandleRequest(AContext, request, response, true, true, FSecurePath)
  else if FTerminologyWebServer.handlesRequest(request.Document) then
    FTerminologyWebServer.Process(AContext, request, response)
  else if request.Document.StartsWith('/oauth2') then
    FAuthServer.HandleRequest(AContext, request, response)
  else if request.Document = '/' then
    ReturnProcessedFile(response, '/hompage.html', AltFile('/homepage.html'))
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
    writeln('miss: '+request.Document);
  end;
end;

Procedure TFhirWebServer.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure : Boolean; path : String);
var
  sHost : string;
  oRequest : TFHIRRequest;
  oResponse : TFHIRResponse;
  sCookie : string;
  sContentType : String;
  oStream : TStream;
  sDoc : String;
  s : String;
  aFormat : TFHIRFormat;
  lang : String;
  sPath : String;
  session : TFhirSession;
  redirect : Boolean;
  formparams : String;
  relativeReferenceAdjustment : integer;
  pretty : boolean;
  c : integer;
  domain : String;
Begin
  Session := nil;
  try
      if ssl then
        sHost := 'https://'+request.Host
      else
        sHost := 'http://'+request.Host;
      domain := request.Host;
      if domain.Contains(':') then
        domain := domain.Substring(0, domain.IndexOf(':'));

      lang := request.AcceptLanguage;
      s := request.ContentType;
      if pos(';', s) > 0 then
        s := copy(s, 1, pos(';', s) - 1); //only read up to the first ';'
      if (SameText(s, 'application/x-www-form-urlencoded') or SameText(request.Command, 'get') or SameText(request.Command, 'options')) and (request.UnparsedParams <> '') then
        sDoc := request.Document +'?'+ request.UnparsedParams
      else
        sDoc := request.Document;
      try
        sContentType := request.ContentType;
      if sContentType = '' then
        sContentType := request.Accept;

      if s.StartsWith('multipart/form-data', true) then
        oStream := extractFileData(request.PostStream, request.ContentType, 'file', sContentType, formparams)
      else if request.PostStream <> nil then
      begin
        oStream := TMemoryStream.create;
        oStream.CopyFrom(request.PostStream, request.PostStream.Size);
        oStream.Position := 0;
      end
      else
        oStream := TStringStream.Create(request.UnparsedParams);
      try
        response.CustomHeaders.add('Access-Control-Allow-Origin: *');
        response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, DELETE');
        response.CustomHeaders.add('Access-Control-Expose-Headers: Content-Location');
        response.Expires := Now - 1; //don't want anyone caching anything
        oResponse := TFHIRResponse.Create;
        Try
          if request.AuthUsername = 'Bearer' then
            sCookie := request.AuthPassword
          else
          begin
            c := request.Cookies.GetCookieIndex(FHIR_COOKIE_NAME);
            if c > -1 then
              sCookie := request.Cookies[c].CookieText.Substring(FHIR_COOKIE_NAME.Length+1);
          end;

          oRequest := BuildRequest(lang, path, sHost, request.CustomHeaders.Values['Origin'], request.RemoteIP, request.CustomHeaders.Values['content-location'],
             request.Command, sDoc, sContentType, request.Accept, sCookie, oStream, oResponse, aFormat, redirect, formparams, secure, ssl, relativeReferenceAdjustment, pretty);
          try
            ReadTags(request.RawHeaders.Values['Category'], oRequest);
            session := oRequest.Session.Link;
            if redirect then
            begin
              if oRequest.Session <> nil then
              begin
                FAuthServer.setCookie(response, FHIR_COOKIE_NAME, oRequest.Session.Cookie, domain, '', oRequest.Session.Expires, false);
                response.Redirect(oRequest.Session.OriginalUrl);
              end
              else
                response.Redirect(oRequest.baseUrl);
            end
            else if oRequest.CommandType = fcmdUnknown then
            begin
              response.ResponseNo := 200;
              response.ContentType := 'text/html';
              response.FreeContentStream := true;
              response.ContentStream := StringToUTFStream(BuildFhirHomePage(oRequest.compartments, lang, sHost, path, oRequest.Session));
            end
            else if (oRequest.CommandType = fcmdUpload) and (oRequest.Resource = nil) and (oRequest.Feed = nil) Then
            begin
              response.ResponseNo := 200;
              response.ContentType := 'text/html';
              response.FreeContentStream := true;
              response.ContentStream := StringToUTFStream(BuildFhirUploadPage(lang, sHost, '', oRequest.ResourceType, oRequest.Session));
            end
            else if (oRequest.CommandType = fcmdConformanceStmt) and (oRequest.ResourceType <> frtNull) then
            begin
              response.ResponseNo := 200;
              response.ContentType := 'text/html';
// no - just use *              response.CustomHeaders.add('Access-Control-Allow-Origin: '+request.RawHeaders.Values['Origin']);
              response.CustomHeaders.add('Access-Control-Request-Method: GET, POST, PUT, DELETE');
              response.FreeContentStream := true;
              response.ContentStream := StringToUTFStream('OK');
            end
            else 
            begin
              try
                if oRequest.CommandType = fcmdWebUI then
                  HandleWebUIRequest(oRequest, oResponse)
                else
                  ProcessRequest(oRequest, oResponse);
              except
                on e : EAbort do
                begin
                  if oResponse.HTTPCode < 300 then
                    raise;
                end;
                on e : Exception do
                  raise;
              end;
              ProcessOutput(oRequest, oResponse, response, relativeReferenceAdjustment, pretty, request.AcceptEncoding.Contains('gzip'));
// no - just use *              if request.RawHeaders.Values['Origin'] <> '' then
//                 response.CustomHeaders.add('Access-Control-Allow-Origin: '+request.RawHeaders.Values['Origin']);
              response.ETag := oResponse.versionId;
              response.LastModified := oResponse.lastModifiedDate; // todo: timezone
              if oResponse.Categories.count > 0 then
                response.CustomHeaders.add('Category: '+ oResponse.Categories.AsHeader);
              if oResponse.originalId <> '' then
                response.CustomHeaders.add('X-Original-Location: '+oResponse.originalId);
              if oResponse.ContentLocation <> '' then
                response.CustomHeaders.add('Content-Location: '+oResponse.ContentLocation);
              if oResponse.Location <> '' then
                response.Location := oResponse.Location;
            end;
            response.WriteContent;
          finally
            oRequest.Free;
          end;
        Finally
         oResponse.free;
        End;
      finally
        oStream.free;
      end;
    except
      on e : ERestfulAuthenticationNeeded do
      begin
        if aFormat = ffXhtml then
        begin
          response.ResponseNo := 200;
          response.ContentType := 'text/html';
          response.FreeContentStream := true;
          response.ContentStream := StringToUTFStream(BuildFhirAuthenticationPage(lang, sHost, sPath + sDoc, e.Msg, ssl));
        end
        else
          SendError(response, e.Status, aFormat, lang, e.Message, sPath, session, true, sPath + sDoc, relativeReferenceAdjustment);
      end;
      on e: ERestfulException do
        SendError(response, e.Status, aFormat, lang, e.Message, sPath, session, false, path, relativeReferenceAdjustment);
      on e: Exception do
        SendError(response, HTTP_ERR_INTERNAL, aFormat, lang, e.Message, sPath, session, false, path, relativeReferenceAdjustment);
    end;
  finally
    session.free;
  end;
end;

procedure TFhirWebServer.HandleWebQuestionnaire(request: TFHIRRequest; response: TFHIRResponse);
var
  id, ver : String;
  questionnaire : TFHIRQuestionnaire;
  s : String;
begin
   // get the right questionnaire
  StringSplit(request.Id.Substring(14), '/', id, ver);
  questionnaire := GetResource(request.Session, frtQuestionnaire, request.Lang, id, ver) as TFHirQuestionnaire;
  try
    // convert to xhtml 
    s := transform(questionnaire, request.Lang, FAltPath+'QuestionnaireToHTML.xslt');
    // insert page headers:
    s := s.Replace('</title>', '</title>'+#13#10+TFHIRXhtmlComposer.PageLinks);
    s := s.Replace('<body>', '<body>'+#13#10+TFHIRXhtmlComposer.Header(request.Session, request.baseUrl, request.Lang));
    s := s.Replace('</body>', TFHIRXhtmlComposer.Footer(request.baseUrl)+#13#10+'</body>');
    response.Body := s;
    response.ContentType := 'text/html';
  finally
    questionnaire.free;
  end;
end;

procedure TFhirWebServer.HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse);
begin
  if request.Id.StartsWith('Questionnaire/') then
    HandleWebQuestionnaire(request, response)
  else
    raise Exception.Create('Unknown request');
end;

procedure TFhirWebServer.SendError(response: TIdHTTPResponseInfo; status : word; format : TFHIRFormat; lang, message, url : String; session : TFhirSession; addLogins : boolean; path : String; relativeReferenceAdjustment : integer);
var
  issue : TFhirOperationOutcome;
  report :  TFhirOperationOutcomeIssue;
  oComp : TFHIRComposer;
  e : TFhirExtension;
begin
  response.ResponseNo := status;
  response.FreeContentStream := true;

  if format = ffAsIs then
  begin
    response.ContentType := 'text/plain';
    response.ContentStream := StringToUTFStream(message);
  end
  else
  begin
    issue := TFhirOperationOutcome.create;
    try
      issue.text := TFhirNarrative.create;
      issue.text.statusST := NarrativeStatusGenerated;
      issue.text.div_ := ParseXhtml(lang, '<div><p>'+FormatTextToXML(message)+'</p></div>', xppReject);
      if addLogins then
      begin
        if FAuthServer.HL7Appid <> '' then
        begin
          e := issue.ExtensionList.Append;
          e.urlST := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          e.value := TFhirString.create('http://hl7.amg-hq.net/tools/signup_redirect.cfm?apiKey='+FAuthServer.HL7Appid+'&returnURL='+EncodeMime(path)+'/state/'+FAuthServer.MakeLoginToken(path, apHL7));
        end;
        if FAuthServer.FacebookAppid <> '' then
        begin
          e := issue.ExtensionList.Append;
          e.urlST := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          e.value := TFhirString.create('https://www.facebook.com/dialog/oauth?client_id='+FAuthServer.FacebookAppid+'&redirect_uri='+path+'&state='+FAuthServer.MakeLoginToken(path, apFacebook));
        end;
        if FAuthServer.GoogleAppid <> '' then
        begin
          e := issue.ExtensionList.Append;
          e.urlST := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          e.value := TFhirString.create('https://accounts.google.com/o/oauth2/auth?client_id='+FAuthServer.GoogleAppid+'&response_type=code&scope=openid%20email&redirect_uri='+path+'&state='+FAuthServer.MakeLoginToken(path, apGoogle));
        end;
      end;
      report := issue.issueList.Append;
      report.severityST := IssueSeverityError;
      report.details := TFHIRString.create(message);
      response.ContentStream := TMemoryStream.Create;
      oComp := nil;
      case format of
        ffXml: oComp := TFHIRXmlComposer.Create(lang);
        ffXhtml:
          begin
          oComp := TFHIRXhtmlComposer.Create(lang);
          TFHIRXhtmlComposer(oComp).BaseURL := AppendForwardSlash(url);
          TFHIRXhtmlComposer(oComp).Session := Session.Link;
          TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
          end;
        ffJson: oComp := TFHIRJsonComposer.Create(lang);
      end;
      try
        response.ContentType := oComp.MimeType;
        oComp.Compose(response.ContentStream, '', '', issue, false);
        response.ContentStream.Position := 0;
      finally
        oComp.free;
      end;
    finally
      issue.free;
    end;
  end;
  response.WriteContent;
end;


function extractProp(contentType, name : String) : string;
begin
  if contentType.Contains(name+'=') then
  begin
    result := contentType.Substring(contentType.IndexOf(name+'=')+name.Length+1);
    if result.Contains(';') then
      result := result.Substring(0, result.indexOf(';'));
  end
  else
    result := '';
end;

Function TFhirWebServer.BuildRequest(lang, sBaseUrl, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept, sCookie : String; oPostStream : TStream; oResponse : TFHIRResponse; var aFormat : TFHIRFormat;
   var redirect : boolean; formparams : String; bAuth, secure : Boolean; out relativeReferenceAdjustment : integer; var pretty : boolean) : TFHIRRequest;
Var
  sURL, sId, sType, msg : String;
  aResourceType : TFHIRResourceType;
  oRequest : TFHIRRequest;
  parser : TFHIRParser;

  Function NextSegment(var url : String):String;
  var
    i : integer;
  Begin
    i := StringFind(url, ['/']);
    if i = 0 then
    begin
      result := url;
      url := '';
    end
    else
    begin
      inc(relativeReferenceAdjustment);
      result := copy(url, 1, i-1);
      url := copy(url, i + 1, $FFF);
    end;
  End;
  procedure ForceMethod(sMethod : String);
  begin
    if (sCommand <> sMethod) Then
      raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_FORMAT', lang), [sResource, sMethod]), HTTP_ERR_FORBIDDEN);
  end;
var
  i : integer;
  session : TFhirSession;
  check : boolean;
  p : TParseMap;
  s : String;
Begin
  relativeReferenceAdjustment := 0;
  Result := nil;
  oRequest := TFHIRRequest.Create;
  try
    oRequest.Lang := lang;
    oResponse.origin := sOrigin;
    oRequest.PostFormat := ffAsIs;
    oResponse.Format := ffAsIs;
    aFormat := ffAsIs;
    oRequest.baseUrl := sHost + AppendForwardSlash(sBaseURL);
    oRequest.url := sHost + sResource;
//    oRequest.versionId := ''; // sETag;
    oRequest.lastModifiedDate := 0; // Xml
    oRequest.contentLocation := sContentLocation; // for version aware updates

    If Not StringStartsWithSensitive(sResource, sBaseURL) Then
    begin
      if StringStartsWith(sResource, '/images/', false) then
        Raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', 'images not served', HTTP_ERR_NOTFOUND)
      else
        Raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_NO_MODULE', lang), [sResource]), HTTP_ERR_NOTFOUND);
    end;

    sURL := Copy(sResource, length(sBaseURL)+1, $FFF);
    i := StringFind(sURL, ['?']);
    if (i = 0) then
      oRequest.LoadParams('')
    else
    begin
      oRequest.LoadParams(copy(sURL, i+1, $FFF));
      sUrl := copy(sURL, 1, i-1);
    end;
    if sUrl.StartsWith('/') then
      sUrl := sUrl.Substring(1);
    if formparams <> '' then
      oRequest.LoadParams(formparams);

    if (sCommand <> 'GET') then
    begin
      if oRequest.Parameters.VarExists('_format') then
        sContentType := oRequest.Parameters.GetVar('_format');
      if StringStartsWithInsensitive(sContentType, 'application/json') or StringStartsWithInsensitive(sContentType, 'application/fhir+json') or StringStartsWithInsensitive(sContentType, 'application/json+fhir') or StringStartsWithInsensitive(sContentType, 'json') or StringStartsWithInsensitive(sContentType, 'text/json') Then
        oRequest.PostFormat := ffJson
      else if StringStartsWithInsensitive(sContentType, 'text/html') or StringStartsWithInsensitive(sContentType, 'html') or StringStartsWithInsensitive(sContentType, 'application/x-zip-compressed') or StringStartsWithInsensitive(sContentType, 'application/zip') Then
        oRequest.PostFormat := ffXhtml
      else if StringStartsWithInsensitive(sContentType, 'text/xml') or StringStartsWithInsensitive(sContentType, 'application/xml') or
          StringStartsWithInsensitive(sContentType, 'application/fhir+xml') or StringStartsWithInsensitive(sContentType, 'application/xml+fhir') or StringStartsWithInsensitive(sContentType, 'xml') Then
        oRequest.PostFormat := ffXML;
    end;

    if oRequest.Parameters.VarExists('_format') then
      sContentAccept := oRequest.Parameters.GetVar('_format');
    if StringExistsSensitive(sContentAccept, 'application/json') or StringExistsInsensitive(sContentAccept, 'json') Then
      oResponse.Format := ffJson
    else if StringExistsSensitive(sContentAccept, 'text/xml') Then
      oResponse.Format := ffXML
    else if StringExistsSensitive(sContentAccept, 'text/html') Then
      oResponse.Format := ffXhtml
    else if StringExistsSensitive(sContentAccept, 'application/xml') Then
      oResponse.Format := ffXML
    else if StringExistsInsensitive(sContentAccept, 'xml') Then
      oResponse.Format := ffXML
    else if StringExistsInsensitive(sContentAccept, 'html') Then
      oResponse.Format := ffXhtml
    else if oRequest.PostFormat <> ffAsIs then
      oResponse.Format := oRequest.PostFormat;

    if oRequest.Parameters.VarExists('_pretty') then
      pretty := oRequest.Parameters.GetVar('_pretty') = 'true'
    else if sContentAccept.Contains('pretty=') then
      pretty := extractProp(sContentAccept, 'pretty') = 'true'
    else
      pretty := false;

    aFormat := oResponse.Format;

    // ok, now that we've read the content types, security check
    if bAuth then
    begin
      if sUrl = 'logout' then
      begin
        FFhirStore.EndSession(sCookie, sClient);
        oRequest.session := nil;
        redirect := true;
      end
      else if (sURL = 'internal') then
        redirect := true
      else if (sUrl <> 'auth-login') and FFhirStore.GetSession(sCookie, session, check) then
      begin
        if check and not CheckSessionOK(session, sClient) then
          Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', lang), msg, HTTP_ERR_UNAUTHORIZED);
        oRequest.session := session
      end
      else
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', lang), msg, HTTP_ERR_UNAUTHORIZED);
    end
    else
      oRequest.session := FFhirStore.CreateImplicitSession(sClient);

    relativeReferenceAdjustment := 0;
    if not redirect then
    begin
      sType := NextSegment(sURL);
      if (sType = '') Then
      begin
        if sCommand = 'OPTIONS' then
          oRequest.CommandType := fcmdConformanceStmt
        else if (sCommand = 'POST') then
        begin
          if formparams <> '' then
          begin
            p := TParseMap.createSmart(formparams);
            try
              s := p.GetVar('op');
              if (s = 'transaction') or (s = '') then
                oRequest.CommandType := fcmdTransaction
              else if (s = 'validation') or (s = 'validate') then
                oRequest.CommandType := fcmdValidate
              else if (s = 'mailBox') then
                oRequest.CommandType := fcmdMailBox
              else
                raise Exception.create('Unknown Operation: '+s);
            finally
              p.free;
            end;
          end
          else
            oRequest.CommandType := fcmdTransaction
        end
        else
        begin
          ForceMethod('GET');
          oRequest.CommandType := fcmdUnknown;
        end
      end
      else if (sType = '_web') then // special
      begin
        ForceMethod('GET');
        oRequest.CommandType := fcmdWebUI;
        oRequest.id := sUrl;
        sUrl := '';
      end
      else if (sType = '_service') then
      begin
        raise Exception.Create('not done yet');
      end
      else if (sType = '_tags') then
      begin
        ForceMethod('GET');
        oRequest.CommandType := fcmdGetTags;
      end
      else if (sType = '_history') then
      begin
        ForceMethod('GET');
        oRequest.CommandType := fcmdHistorySystem;
      end
      else if (sType = 'metadata') or (sType = 'metadata.xml') or (sType = 'metadata.htm') or (sType = 'metadata.json') Then
      begin
        oRequest.CommandType := fcmdConformanceStmt;
        ForceMethod('GET');
        if sType = 'metadata.htm' then
          oResponse.format := ffXhtml
        else if sType = 'metadata.xml' then
          oResponse.format := ffXml
        else if sType = 'metadata.json' then
          oResponse.format := ffJson
      end
      else if (sType = 'Mailbox') then
      begin
        oRequest.CommandType := fcmdMailbox;
        ForceMethod('POST');
      end
      else if (sType = 'validation') then
      begin
        oRequest.CommandType := fcmdValidate;
        ForceMethod('POST');
        p := TParseMap.createSmart(formparams);
        try
          if p.VarExists('profile') and (p.GetVar('profile') <> '') then
            oRequest.categories.AddValue(TAG_FHIR_SCHEME_PROFILE, 'http://localhost/profile/@'+p.GetVar('profile'), '');
        finally
          p.free;
        end;
      end
      else if (sType = '_search') then
      begin
        oRequest.CommandType := fcmdSearch;
        if (sCommand <> 'GET') and (sCommand <> 'POST') then
          raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_SYNTAX', lang), [sResource]), HTTP_ERR_BAD_REQUEST);
      end
      else
      begin
        if (sType <> '') And not RecogniseFHIRResourceManagerName(sType, aResourceType) Then
          Raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_NO_MODULE', lang), [sType]), HTTP_ERR_NOTFOUND);
        oRequest.ResourceType := aResourceType;
        sId := NextSegment(sURL);
        if sId = '' then
        begin
          if sCommand = 'GET' then
          begin
            oRequest.CommandType := fcmdSearch;
            oRequest.DefaultSearch := true;
          end
          else if sCommand = 'POST' then
            oRequest.CommandType := fcmdCreate
          else
            raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_SYNTAX', lang), [sResource]), HTTP_ERR_BAD_REQUEST);
        end
        else if not StringStartsWith(sId, '_', true) Then
        begin
          // operations on a resource
          CheckId(lang, sId);
          oRequest.Id := sId;
          sId := NextSegment(sUrl);
          if (sId = '') Then
          begin
            if sCommand = 'GET' Then
              oRequest.CommandType := fcmdRead
            else if sCommand = 'PUT' Then
              oRequest.CommandType := fcmdUpdate
            else if sCommand = 'DELETE' Then
              oRequest.CommandType := fcmdDelete
            else if sCommand = 'OPTIONS' then // CORS
              oRequest.CommandType := fcmdConformanceStmt
            else
              raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_FORMAT', lang), [sResource, 'GET, PUT or DELETE']), HTTP_ERR_FORBIDDEN);
          end
          else if (sId = '_history') then
          begin
            sId := NextSegment(sURL);
            if sId = '' then
            begin
              ForceMethod('GET');
              oRequest.CommandType := fcmdHistoryInstance;
            end
            else if not StringStartsWith(sId, '_', true) Then
            begin
              CheckId(lang, sId);
              oRequest.SubId := sId;
              sId := NextSegment(sURL);
              if sid = '' then
              begin
                ForceMethod('GET');
                oRequest.CommandType := fcmdVersionRead;
              end
              else if (sid = '_tags') then
              begin
                sId := NextSegment(sURL);
                if sId = '' then
                begin
                  if sCommand = 'GET' Then
                    oRequest.CommandType := fcmdGetTags
                  else if sCommand = 'POST' Then
                    oRequest.CommandType := fcmdUpdateTags
//                  else if sCommand = 'DELETE' Then
//                    oRequest.CommandType := fcmdDeleteTags
                  else
                    raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_FORMAT', lang), [sResource, 'GET, POST or DELETE']), HTTP_ERR_FORBIDDEN);
                end else if sId = '_delete' then
                begin
                  ForceMethod('POST');
                  oRequest.CommandType := fcmdDeleteTags;
                end
                else
                  raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_FORMAT', lang), [sResource, 'GET, POST or DELETE']), HTTP_ERR_FORBIDDEN);
              end
              else
                raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_SYNTAX', lang), [sResource]), HTTP_ERR_BAD_REQUEST);
            end
            else
              raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_SYNTAX', lang), [sResource]), HTTP_ERR_BAD_REQUEST);
          end
          else if (sId = '_tags') then
          begin
            if sCommand = 'GET' Then
              oRequest.CommandType := fcmdGetTags
            else if sCommand = 'POST' Then
              oRequest.CommandType := fcmdUpdateTags
            else if sCommand = 'DELETE' Then
              oRequest.CommandType := fcmdDeleteTags
            else
              raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_FORMAT', lang), [sResource, 'GET, POST or DELETE']), HTTP_ERR_FORBIDDEN);
          end
          else if StringArrayExistsInSensitive(CODES_TFhirResourceType, sId) then
          begin
            aResourceType := TFHIRResourceType(StringArrayIndexOfInSensitive(CODES_TFhirResourceType, sId));
            if (COMPARTMENT_PARAM_NAMES[oRequest.ResourceType, aResourceType] <> '') then
            begin
              if oRequest.ResourceType <> frtPatient then
                raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_UNKNOWN_COMPARTMENT', lang), [sResource, 'GET, POST or DELETE']), HTTP_ERR_FORBIDDEN);

              oRequest.CompartmentId := oRequest.Id;
              oRequest.CommandType := fcmdSearch;
              oRequest.ResourceType := aResourceType;
              oRequest.Id := '';
            end
            else
              raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_SYNTAX', lang), [sResource]), HTTP_ERR_BAD_REQUEST);
          end
          else
            raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_SYNTAX', lang), [sResource]), HTTP_ERR_BAD_REQUEST);
        end
        else if (sId = '_validate') Then
        begin
          ForceMethod('POST');
          oRequest.CommandType := fcmdValidate;
          sId := NextSegment(sUrl);
          if sId <> '' Then
          Begin
            if (sURL <> '') or (sId = '') Then
              raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', 'Bad Syntax in "'+sResource+'"', HTTP_ERR_BAD_REQUEST);
            CheckId(lang, copy(sId, 2, $FF));
            oRequest.Id := copy(sId, 2, $FF);
          End;
        end
        else if (sId = '_tags') or (sId = '_tags.xml') or (sId = '_tags.json') Then
        begin
          ForceMethod('GET');
          oRequest.CommandType := fcmdGetTags
        end
        else if (sId = '_search') or (sId = '_search.xml') or (sId = '_search.json') Then
          oRequest.CommandType := fcmdSearch
        else if (sId = '_history') or (sId = '_history.xml') or (sId = '_history.json') Then
        begin
          ForceMethod('GET');
          oRequest.CommandType := fcmdHistoryType
        end
        // Extension on this server - remove?
        else if (sId = '_upload') or (sId = '_upload.htm') then
        begin
          oRequest.CommandType := fcmdUpload;
          oRequest.PostFormat := ffXhtml;
        end
        else
          raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_SYNTAX', lang), ['URL'+sResource]), HTTP_ERR_BAD_REQUEST);
      End;

      if (sURL <> '') then
        raise ERestfulException.Create('TFhirWebServer', 'HTTPRequest', StringFormat(GetFhirMessage('MSG_BAD_SYNTAX', lang), [sResource]), HTTP_ERR_BAD_REQUEST);

      if (oRequest.Session <> nil) and (oRequest.Session.User <> nil) and (oRequest.Session.user.resource.PatientList.Count > 0) then
        oRequest.compartments := BuildCompartmentList(oRequest.Session.user);

      if (oRequest.CommandType in [fcmdTransaction, fcmdUpdate, fcmdValidate, fcmdCreate, fcmdMailbox]) or ((oRequest.CommandType in [fcmdUpload, fcmdSearch, fcmdWebUI]) and (sCommand = 'POST') and (oPostStream.Size > 0)) Then
      begin
        oRequest.CopyPost(oPostStream);
        if (sContentType = 'application/x-zip-compressed') or (sContentType = 'application/zip') then
          oRequest.Feed := ProcessZip(lang, oPostStream)
        else
        begin
          oRequest.Source := TAdvBuffer.Create;
          oRequest.Source.LoadFromStream(oPostStream);
          oPostStream.Position := 0;
          if oRequest.ResourceType = frtBinary then
          begin
            oRequest.Resource := TFhirBinary.create;
            TFhirBinary(oRequest.Resource).Content.loadFromStream(oPostStream);
            TFhirBinary(oRequest.Resource).ContentType := sContentType;
          end
          else
            try
              parser := MakeParser(lang, oRequest.PostFormat, oPostStream, xppReject);
              try
                oRequest.Resource := parser.resource.Link;
                oRequest.Feed := parser.feed.Link;
                if (oRequest.CommandType = fcmdTransaction) and (oRequest.feed = nil) then
                begin
                  oRequest.feed := TFHIRAtomFeed.create;
                  oRequest.Feed.fhirBaseUrl := oRequest.baseUrl;
                  oRequest.feed.entries.add(TFHIRAtomEntry.create);
                  oRequest.feed.entries[0].resource := oRequest.Resource.link;
                  oRequest.feed.entries[0].id := NewGuidURN;
                  oRequest.resource := nil;
                end;
              finally
                parser.free;
              end;
            except
              on e : Exception do
                if oRequest.CommandType <> fcmdValidate then
                  raise;
            end;
        end;
      end
      else if (oRequest.CommandType in [fcmdUpdateTags, fcmdDeleteTags]) then
      begin
        parser := MakeParser(lang, oRequest.PostFormat, oPostStream, xppDrop);
        try
          oRequest.categories.AddAll(parser.Tags);
        finally
          parser.free;
        end;
      end;
    end;

    result := oRequest.Link;
  Finally
    oRequest.Free;
  End;
End;

Function TFhirWebServer.ProcessZip(lang : String; oStream : TStream) : TFHIRAtomFeed;
var
  rdr : TAdvZipReader;
  p : TFHIRParser;
  i : integer;
  s : TAdvVCLStream;
  e : TFHIRAtomEntry;
begin
  result := TFHIRAtomFeed.Create;
  try
    result.id := NewGuidURN;
    rdr := TAdvZipReader.Create;
    try
      s := TAdvVCLStream.Create;
      s.Stream := oStream;
      rdr.Stream := s;
      rdr.Parts := TAdvZipPartList.Create;
      rdr.ReadZip;
      for i := 0 to rdr.Parts.Count - 1 Do
      begin
        if rdr.Parts[i].name.EndsWith('.json') then
          p := TFHIRJsonParser.create(lang)
        else
          p := TFHIRXmlParser.create(lang);
        try
          p.source := TBytesStream.create(rdr.parts[i].AsBytes);
          p.Parse;
          if p.feed <> nil then
            result.entries.AddAll(p.feed.entries)
          else
          begin
            e := TFHIRAtomEntry.create;
            try
              if pos('(', rdr.parts[i].Name) > 0 Then
              begin
                e.id := 'http://hl7.org/fhir/'+CODES_TFHIRResourceType[p.resource.ResourceType]+'/'+GetStringCell(GetStringCell(rdr.parts[i].Name, 1, '('), 0, ')');
                e.links.Rel['html'] := rdr.parts[i].Name.Substring(0, rdr.parts[i].Name.IndexOf('('))+'.html';
              end
              else if rdr.parts[i].Name.EndsWith('.profile.xml') then
                e.id := 'http://hl7.org/fhir/'+CODES_TFHIRResourceType[p.resource.ResourceType]+'/'+copy(rdr.parts[i].Name, 1, length(rdr.parts[i].Name) - 12)
              else
                e.id := NewGuidURN;
              e.resource := p.resource.Link;
              e.summary := TFhirXHtmlNode.create;
              e.summary.Name := 'div';
              e.summary.NodeType := fhntElement;
              result.entries.add(e.Link);
            finally
              e.free;
            end;
          end;
        finally
          p.source.free;
          p.free;
        end;
      end;
    finally
      rdr.free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;


procedure TFhirWebServer.SSLPassword(var Password: String);
begin
  Password := FSSLPassword;
end;

Procedure TFhirWebServer.ProcessOutput(oRequest : TFHIRRequest; oResponse : TFHIRResponse; response: TIdHTTPResponseInfo; relativeReferenceAdjustment : integer; pretty, gzip : boolean);
var
  oComp : TFHIRComposer;
  b : TBytes;
  stream : TMemoryStream;
  ownsStream : boolean;
  comp : TIdCompressorZLib;
begin
  response.ResponseNo := oResponse.HTTPCode;
  response.ContentType := oResponse.ContentType;
  stream := TMemoryStream.create;
  try
    ownsStream := true;
    if oresponse.feed <> nil then
    begin
      if oResponse.Format = ffxhtml then
      begin
        oComp := TFHIRXhtmlComposer.Create(oRequest.lang);
        TFHIRXhtmlComposer(oComp).BaseURL := AppendForwardSlash(oRequest.baseUrl);
        TFHIRXhtmlComposer(oComp).Session := oRequest.Session.Link;
        TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
        TFHIRXhtmlComposer(oComp).OnGetLink := GetWebUILink;
        response.ContentType := oComp.MimeType;
      end
      else if oResponse.format = ffJson then
      begin
        oComp := TFHIRJsonComposer.Create(oRequest.lang);
        response.ContentType := oComp.MimeType;
      end
      else
      begin
        oComp := TFHIRXmlComposer.Create(oRequest.lang);
        response.ContentType := 'application/atom+xml; charset=UTF-8';
      end;
      try
        oComp.Compose(stream, oResponse.Feed, pretty);
      finally
        oComp.Free;
      end;
    end
    else if oResponse.Resource <> nil then
    Begin
      if oResponse.Resource is TFhirBinary then
      begin
        TFhirBinary(oResponse.Resource).Content.SaveToStream(stream);
        response.ContentType := TFhirBinary(oResponse.Resource).ContentType;
        response.ContentDisposition := 'attachment;';
      end
      else
      begin
        if oResponse.Format = ffJson then
          oComp := TFHIRJsonComposer.Create(oRequest.lang)
        else if oResponse.Format = ffXhtml then
        begin
          oComp := TFHIRXhtmlComposer.Create(oRequest.lang);
          TFHIRXhtmlComposer(oComp).BaseURL := AppendForwardSlash(oRequest.baseUrl);
          TFHIRXhtmlComposer(oComp).Session := oRequest.Session.Link;
          TFHIRXhtmlComposer(oComp).Tags := oResponse.categories.Link;
          TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
          TFHIRXhtmlComposer(oComp).OnGetLink := GetWebUILink;
        end
        else if oResponse.Format = ffXml then
          oComp := TFHIRXmlComposer.Create(oRequest.lang)
        else if oResponse.Resource._source_format = ffJson then
          oComp := TFHIRJsonComposer.Create(oRequest.lang)
        else
          oComp := TFHIRXmlComposer.Create(oRequest.lang);
        try
          response.ContentType := oComp.MimeType;
          oComp.Compose(stream, oRequest.id, oRequest.subId, oResponse.resource, pretty);
        finally
          oComp.Free;
        end;
      end;
    end
    else if oRequest.CommandType in [fcmdGetTags, fcmdUpdateTags, fcmdDeleteTags] then
    begin
      if oResponse.Format = ffxhtml then
      begin
        oComp := TFHIRXhtmlComposer.Create(oRequest.lang);
        TFHIRXhtmlComposer(oComp).BaseURL := AppendForwardSlash(oRequest.baseUrl);
        TFHIRXhtmlComposer(oComp).Session := oRequest.Session.Link;
        TFHIRXhtmlComposer(oComp).relativeReferenceAdjustment := relativeReferenceAdjustment;
        TFHIRXhtmlComposer(oComp).OnGetLink := GetWebUILink;
        response.ContentType := oComp.MimeType;
      end
      else if oResponse.format = ffJson then
      begin
        oComp := TFHIRJsonComposer.Create(oRequest.lang);
        response.ContentType := oComp.MimeType;
      end
      else
      begin
        oComp := TFHIRXmlComposer.Create(oRequest.lang);
        response.ContentType := oComp.MimeType;
      end;
      try
        oComp.Compose(stream, oRequest.ResourceType, oRequest.Id, oRequest.SubId, oResponse.Categories, pretty);
      finally
        oComp.Free;
      end;
    end
    else
    begin
      if response.ContentType = '' then
        response.ContentType := 'text/plain';
      b := TEncoding.UTF8.GetBytes(oResponse.Body);
      stream.write(b, length(b));
    end;
    stream.Position := 0;
    if gzip then
    begin
      response.ContentStream := TMemoryStream.Create;
      comp := TIdCompressorZLib.Create(nil);
      try
        comp.CompressStream(stream, response.ContentStream, 9, GZIP_WINBITS, 9, 0);
      finally
        comp.Free;
      end;
      response.ContentStream.Position := 0;
      response.ContentEncoding := 'gzip';
    end
    else
    begin
      response.ContentStream := stream;
      ownsStream := false;
    end;
  finally
    if ownsStream then
      stream.Free;
  end;
end;

procedure TFhirWebServer.ProcessRequest(request: TFHIRRequest; response: TFHIRResponse);
var
  store : TFhirOperation;
begin
  writeln('Request: cmd='+CODES_TFHIRCommandType[request.CommandType]+', type='+CODES_TFhirResourceType[request.ResourceType]+', id='+request.Id+', user='+request.Session.Name+', params='+request.Parameters.Source);
  store := TFhirOperation.Create(request.Lang, FFhirStore.Link);
  try
    store.Connection := FFhirStore.DB.GetConnection('Operation');
    store.precheck(request, response);
    try
      store.Connection.StartTransact;
      try
        store.Execute(request, response);
        store.Connection.Commit;
      except
        store.Connection.Rollback;
        raise
      end;
      store.Connection.Release;
    except
      on e:exception do
      begin
        store.Connection.Error(e);
        raise;
      end;
    end;
  finally
    store.Free;
  end;
end;

function TFhirWebServer.BuildFhirAuthenticationPage(lang, host, path, msg : String; secure : boolean): String;
var
  authurl : string;
begin
  authurl := OAuthPath(secure);


result :=
'<?xml version="1.0" encoding="UTF-8"?>'#13#10+
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
''#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
'<head>'#13#10+
'    <title>FHIR RESTful Server - FHIR v'+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+'</title>'#13#10+
TFHIRXhtmlComposer.PageLinks+#13#10+
FHIR_JS+
'</head>'#13#10+
''#13#10+
'<body>'#13#10+
''#13#10+
TFHIRXhtmlComposer.Header(nil, FBasePath, lang)+
'<h2>HL7Connect '+GetFhirMessage('NAME_SERVER', lang)+'</h2>'#13#10;



result := result +
'<p>'#13#10+
GetFhirMessage('MSG_AUTH_REQUIRED', lang)+ '</p>'#13#10;
if msg <> '' then
result := result +
  '<p><b>'+ FormatTextToHTML(msg)+'</b></p>'#13#10;

result := result +
'<p><a href="/oauth2/auth?client_id=web&response_type=code&scope=all&redirect_uri='+authurl+'/internal&state='+FAuthServer.MakeLoginToken(path, apGoogle)+'">Login using OAuth</a></p>'+#13#10;

result := result +
TFHIRXhtmlComposer.Footer(lang);

end;


function TFhirWebServer.BuildFhirHomePage(comps, lang, host, sBaseURL : String; session : TFhirSession): String;
var
  counts : Array [TFHIRResourceType] of Integer;
  a : TFHIRResourceType;
  db : TKDBConnection;
  s : String;
  names : TStringList;
  profiles : TAdvStringMatch;
  i, j : integer;
  cmp : String;
  b : TStringBuilder;
begin
  for a := low(TFHIRResourceType) to high(TFHIRResourceType) do
    if (comps = '') or (COMPARTMENT_PARAM_NAMES[frtPatient, a] <> '') then
      counts[a] := 0
    else
      counts[a] := -1;

  if (comps <> '') then
    cmp := ' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id in ('+comps+'))'
  else
    cmp := '';


  profiles := TAdvStringMatch.create;
  try
    profiles.forced := true;
    if FFhirStore <> nil then
    begin
      db := FFhirStore.DB.GetConnection('fhir');
      try
        db.sql := 'select ResourceName, count(*) as Count from Ids,  Types where Ids.ResourceTypeKey = Types.ResourceTypeKey '+cmp+' group by ResourceName';

        db.prepare;
        db.execute;
        while db.fetchNext do
        begin
          j := StringArrayIndexOfSensitive(CODES_TFHIRResourceType, db.ColStringByname['ResourceName']);
          if j > -1 then
            counts[TFhirResourceType(j)] := db.ColIntegerByName['Count'];
        end;
        db.terminate;
        db.Release;
      except
        on e:exception do
        begin
          db.Error(e);
          raise;
        end;
      end;
    end;

   s := host+sBaseURL;
   b := TStringBuilder.Create;
   try
    b.Append(
  '<?xml version="1.0" encoding="UTF-8"?>'#13#10+
  '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
  '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
  ''#13#10+
  '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
  '<head>'#13#10+
  '    <title>FHIR RESTful Server - FHIR v'+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+'</title>'#13#10+
  TFHIRXhtmlComposer.pagelinks+
  FHIR_JS+
  '</head>'#13#10+
  ''#13#10+
  '<body>'#13#10+
  TFHIRXhtmlComposer.Header(Session, sBaseURL, lang));

  b.Append(
  '<h2>HL7Connect '+GetFhirMessage('NAME_SERVER', lang)+'</h2>'#13#10);

    if session <> nil then
      b.Append('<p>Welcome '+FormatTextToXml(session.Name)+'</p>'#13#10);

  b.Append(
  '<p>'#13#10+
  StringFormat(GetFhirMessage('MSG_HOME_PAGE_1', lang), ['<a href="http://hl7.org/fhir">http://hl7.org/fhir</a>'])+#13#10+
  StringFormat(GetFhirMessage('MSG_HOME_PAGE_2', lang), [s])+' This server defines some <a href="local.hts">extensions to the API</a>'+#13#10+
  '</p>'#13#10+
  '<hr/>'#13#10+
  ''#13#10+
  '<p>System Operations:</p><ul><li> <a href="'+sBaseUrl+'/metadata">Conformance Profile</a> (or <a href="'+sBaseUrl+'/metadata?_format=text/xml">as xml</a> or <a href="'+sBaseUrl+'/metadata?_format=application/json">JSON</a>)</li>'+#13#10+
  '<li><a class="tag" href="'+sBaseUrl+'/_tags">Tags</a> used on this system</li><li><a href="'+sBaseUrl+'/_search">General Search</a> (the form''s a bit weird, but the API is useful)</li>'+
  '<li><a href="'+sBaseUrl+'/_history">Full History</a> (History of all resources)</li>'+#13#10+
  '<li><a href="#upload">Upload Operations</a></li>'+#13#10+
  '</ul>'+#13#10+
  ''#13#10+
  '<hr/>'#13#10+
  '<p>'+GetFhirMessage('MSG_HOME_PAGE_3', lang)+'</p>'+#13#10);


  b.Append(
  '<table class="lines">'#13#10+

             '<tr><th>'+GetFhirMessage('NAME_TYPE', lang)+'</th>'+
             '<th>'+GetFhirMessage('NAME_STORED', lang)+'</th>'+
             '<th colspan="4">'+GetFhirMessage('NAME_OPERATIONS', lang)+'</th><td style="border-left: 1px solid grey"/><th>'+GetFhirMessage('NAME_TYPE', lang)+'</th>'+
             '<th>'+GetFhirMessage('NAME_STORED', lang)+'</th>'+
             '<th colspan="4">'+GetFhirMessage('NAME_OPERATIONS', lang)+'</th></tr>'#13#10);

    names := TStringList.create;
    Try
      for a := TFHIRResourceType(1) to High(TFHIRResourceType) do
        if counts[a] > -1 then
          names.Add(CODES_TFHIRResourceType[a]);

      names.Sort;
      j := 0;
      for i := 0 to names.count div 2 do
      begin
        inc(j);
        if j mod 2 = 1 then
          b.Append('<tr bgcolor="#F0F0F0">')
        else
          b.Append('<tr bgcolor="#FFFFFF">');

        a := TFHIRResourceType(StringArrayIndexOfSensitive(CODES_TFHIRResourceType, names[i]));
        b.Append(TFHIRXhtmlComposer.ResourceLinks(a, lang, sBaseUrl, counts[a], true, true));

        b.Append('<td style="border-left: 1px solid grey"/>');

        if (i + names.count div 2) + 1 < names.count then
        begin
          a := TFHIRResourceType(StringArrayIndexOfSensitive(CODES_TFHIRResourceType, names[1 + i + names.count div 2]));
          b.Append(TFHIRXhtmlComposer.ResourceLinks(a, lang, sBaseUrl, counts[a], true, true));
        end;

        b.Append('</tr>');

      end;
    finally
      names.free;
    end;
    b.Append(
  '</table>'#13#10+
  '<hr/><h2>'+GetFhirMessage('NAME_UPLOAD_SERVICES', lang)+'</h2>'#13#10+
  '<a name="upload"> </a><form enctype="multipart/form-data" method="POST">'+#13#10+
  '<p><input type="hidden" name="_format" size="text/html"/><br/>'+#13#10+
  ''+GetFhirMessage('MSG_CONTENT_MESSAGE', lang)+'.<br/><br/>'+#13#10+
  ''+GetFhirMessage('MSG_CONTENT_UPLOAD', lang)+': <br/><input type="file" name="file" size="60"/><br/>'+#13#10+
  'or just paste your xml or json here:<br/> <textarea name="src" cols="70" rows="5"/>'+#13#10+
  '</textarea><br/><br/>'+#13#10+
  '<table class="none"><tr><td>Operation:</td><td> <select size="1" name="op">'+#13#10+
  ' <option value="transaction">Transaction</option>'+#13#10+
  ' <option value="validation">Validation</option>'+#13#10+
  ' <option value="mailbox">MailBox Submission</option>'+#13#10+
  '</select></td></tr>'+#13#10+
  '<tr><td>Profile:</td><td> <select size="1" name="profile">'+#13#10+
  '<option value=""></option>'+#13#10+
  FFhirStore.ProfilesAsOptionList+
  '</select> (if validating, use the selected profile)</td></tr></table><br/>'+#13#10+
  '<input type="submit" value="'+GetFhirMessage('NAME_UPLOAD', lang)+'"/>'#13#10+
  '</p></form>'#13#10+
  TFHIRXhtmlComposer.footer(sBaseURL));
  result := b.ToString;
   finally
     b.Free;
   end;
  finally
    profiles.free;
  end;
end;

function TFhirWebServer.BuildFhirUploadPage(lang, host, sBaseURL : String; aType : TFHIRResourceType; session : TFhirSession): String;
var
  s : String;
begin
  s := host+sBaseURL;

  result :=
'<?xml version="1.0" encoding="UTF-8"?>'#13#10+
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
''#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
'<head>'#13#10+
'    <title>'+StringFormat(GetFhirMessage('UPLOAD', lang), [CODES_TFHIRResourceType[aType]])+'</title>'#13#10+
'    <link rel="Stylesheet" href="/css/fhir.css" type="text/css" media="screen" />'#13#10+
FHIR_JS+
'</head>'#13#10+
''#13#10+
'<body>'#13#10+
''#13#10+
'<div class="header">'#13#10+
'  <a href="http://www.hl7.org/fhir" title="'+GetFhirMessage('MSG_HOME_PAGE_TITLE', lang)+'"><img border="0" src="/img/flame16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>'#13#10+
''#13#10+
'  &copy; HL7.org 2011-2013'#13#10+
'  &nbsp;'#13#10+
'  <a href="http://www.hl7connect.com">HL7Connect</a> '+GetFhirMessage('NAME_SERVER', lang)+''#13#10+
'  &nbsp;'#13#10+
'  FHIR '+GetFhirMessage('NAME_VERSION', lang)+' '+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+''#13#10;

if session <> nil then
  result := result +'&nbsp;&nbsp;'+FormatTextToXml(Session.Name);

result := result +
'  &nbsp;<a href="'+s+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a>'#13#10+
'</div>'#13#10+
''#13#10+
'<div id="div-cnt" class="content">'#13#10+
'<h2>'+StringFormat(GetFhirMessage('UPLOAD', lang), [CODES_TFHIRResourceType[aType]])+'</h2>'#13#10+
'<form action="'+s+lowercase(CODES_TFHIRResourceType[aType])+'/upload" enctype="multipart/form-data" method="POST">'+#13#10+
'<input type="hidden" name="format" size="text/html"/><br/>'+#13#10+
''+GetFhirMessage('MSG_CONTENT_UPLOAD', lang)+': <input type="file" name="file" size="60"/><br/>'+#13#10+
'<input type="submit" value="Upload"/>'#13#10+
'</form>'#13#10+
''#13#10+
'<p><br/><a href="'+s+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a></p>'+
'</div>'#13#10+
'</body>'#13#10+
'</html>'#13#10+
''#13#10
end;


function TFhirWebServer.extractFileData(const request : TStream; const contentType, name: String; var sContentType : String; var params : String): TStream;
var
  m : TIdSoapMimeMessage;
  sLeft, sRight: String;
  iLoop : Integer;
  oPart : TIdSoapMimePart;
  sHeader : String;
  sName : String;
  sFilename : String;
  sContent : String;
begin
  params := '';
  result := nil;
  m := TIdSoapMimeMessage.Create;
  Try
    m.ReadFromStream(request, contentType);
    For iLoop := 0 To m.Parts.Count - 1 Do
    Begin
      oPart := m.Parts.PartByIndex[iLoop];
      sHeader := oPart.ContentDisposition;
      StringSplit(sHeader, ';', sLeft, sHeader);
      If trim(sLeft) = 'form-data' Then
      Begin
        StringSplit(Trim(sHeader), ';', sLeft, sHeader);
        StringSplit(Trim(sLeft), '=', sLeft, sRight);
        If Trim(sLeft) = 'name' Then
          sName := RemoveQuotes(Trim(sRight));
        StringSplit(Trim(sHeader), '=', sLeft, sRight);
        If Trim(sLeft) = 'filename' Then
          sFileName := RemoveQuotes(Trim(sRight));
        If (result = nil) and (sName <> '') And (sFileName <> '') And (oPart.Content.Size > 0) Then
        begin
          result := TBytesStream.Create(StreamToBytes(oPart.Content));
          sContentType := oPart.Mediatype;
        end
        else if (result = nil) and (sName = 'src') then
        begin
          sContent := BytesAsString(StreamToBytes(oPart.Content));
          result := TStringStream.create(StreamToBytes(oPart.Content)); // trim
          if StringStartsWith(sContent, '<', false) then
            sContentType := 'application/xml'
          else if StringStartsWith(sContent, '{', false) then
            sContentType := 'application/json'
          else
            raise exception.create('unable to determine encoding type for '+sContent);
        end
        else
          params := params + sName +'='+Trim(BytesAsAnsiString(StreamToBytes(oPart.Content)))+'&';
      End
    End;
  finally
    m.free;
  end;
  if result = nil then
    raise exception.create('unable to process input');
end;

function TFhirWebServer.GetResource(session : TFhirSession; rtype: TFhirResourceType; lang, id, ver: String): TFhirResource;
var
  request : TFHIRRequest;
  response : TFHIRResponse;
begin
  request := TFHIRRequest.create;
  response := TFHIRResponse.Create;
  try
    request.Session := session.link;
    request.ResourceType := rType;
    request.Lang := lang;
    request.Id := id;
    request.LoadParams('');
    if (ver = '') then 
      request.CommandType := fcmdRead
    else
    begin
      request.CommandType := fcmdVersionRead;
      request.SubId := ver;
    end;
    ProcessRequest(request, response);
    if response.Resource <> nil then
      result := response.resource.link
    else
      raise Exception.Create('Unable to find resource '+CODES_TFhirResourceType[rtype]+'/'+id+'/'+ver);
  finally
    response.free;
    request.Free;
  end;
end;

procedure TFhirWebServer.GetWebUILink(resource: TFhirResource; base, id, ver: String; var link, text: String);
var
  tail : String;
begin
  link := '';
  if (resource <> nil) and (id <> '') then
  begin
    tail := id;
    if ver <> '' then
      tail := tail + '/' + ver;
    if resource.ResourceType = frtQuestionnaire then
    begin
      link := FBasePath+'/_web/Questionnaire/'+tail;
      text := 'Try out the Questionnaire as a web form';
    end;
  end;
end;

function TFhirWebServer.OAuthPath(secure: boolean): String;
begin
  if secure then
  begin
    if FSSLPort = 443 then
      result := 'https://'+FHost+FSecurePath
    else
      result := 'https://'+FHost+':'+inttostr(FSSLPort)+FSecurePath;
  end
  else
  begin
    if FPort = 80 then
      result := 'http://'+FHost+FSecurePath
    else
      result := 'http://'+FHost+':'+inttostr(FPort)+FSecurePath;
  end;
end;

function HashPword(s : String): AnsiString;
var
  hash : TDCP_sha256;
  res : TBytes;
begin
  result := '';
  hash := TDCP_sha256.Create(nil);
  try
    hash.Init;
    hash.UpdateStr(s);
    SetLength(res, hash.GetHashSize div 8);
    hash.Final(res[0]);
  finally
    hash.free;
  end;
  result := EncodeHexadecimal(res);
end;

constructor ERestfulAuthenticationNeeded.Create(const sSender, sMethod, sReason, sMsg: String; aStatus : Word);
begin
  Create(sSender, sMethod, sReason, aStatus);
  FMsg := sMsg;
end;


function TFhirWebServer.CheckSessionOK(session: TFhirSession; ip : string): Boolean;
var
  id, name, email, msg : String;
begin
  if session.provider = apGoogle then
    result := GoogleGetDetails(session.InnerToken, FAuthServer.GoogleAppKey, id, name, email, msg)
  else if session.provider = apFacebook then
    result := FacebookGetDetails(session.InnerToken, id, name, email, msg)
  else
    result := false;
  if result then
    result := session.Id = id;
  if result then
    FFhirStore.MarkSessionChecked(session.Cookie, session.Name)
  else
    FFhirStore.EndSession(session.Cookie, ip);
end;

procedure TFhirWebServer.ReadTags(Headers: TIdHeaderList; Request: TFHIRRequest);
var
  i : integer;
begin
  for i := 0 to Headers.Count - 1 do
    if Headers.Names[i] = 'Category' then
      ReadTags(Headers.Strings[i], Request);
end;

Procedure TFhirWebServer.ReadTags(header : String; Request : TFHIRRequest);
var
  s, s1, l, r, n, v : string;
  cat : TFHIRAtomCategory;
begin
  StringSplit(trim(header), ',', s, s1);
  while (s <> '') do
  begin
    StringSplit(trim(s), ';', l, r);
    cat := TFHIRAtomCategory.create;
    try
      cat.term := l;
      StringSplit(trim(r), ';', l, r);
      while (l <> '') do
      begin
        StringSplit(trim(l), '=', n, v);
        v := StringReplace(v, '"', '');
        if n = 'scheme' then
          cat.scheme := v
        else if n = 'label' then
          cat.label_ := v;
        StringSplit(trim(r), ';', l, r);
      end;
      Request.categories.add(cat.link);
    finally
      cat.free;
    end;
    StringSplit(trim(s1), ',', s, s1);
  end;
end;


procedure TFhirWebServer.ReturnProcessedFile(response: TIdHTTPResponseInfo; named, path: String; variables: TDictionary<String, String> = nil);
var
  s, n : String;
begin
  writeln('script: '+named);
  s := FileToString(path, TEncoding.UTF8);
  s := s.Replace('[%id%]', FName, [rfReplaceAll]);
  s := s.Replace('[%ver%]', FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION, [rfReplaceAll]);
  s := s.Replace('[%web%]', WebDesc, [rfReplaceAll]);
  s := s.Replace('[%host%]', FHost, [rfReplaceAll]);
  s := s.Replace('[%endpoints%]', EndPointDesc, [rfReplaceAll]);
  if variables <> nil then
    for n in variables.Keys do
      s := s.Replace('[%'+n+'%]', variables[n], [rfReplaceAll]);

  response.ContentStream := TBytesStream.Create(TEncoding.UTF8.GetBytes(s));
  response.FreeContentStream := true;
  response.ContentType := 'text/html';
end;

procedure TFhirWebServer.ReturnSpecFile(response : TIdHTTPResponseInfo; stated, path: String);
begin
  writeln('file: '+stated);
  response.Expires := now + 1;
  response.ContentStream := TFileStream.Create(path, fmOpenRead);
  response.FreeContentStream := true;
  response.ContentType := GetMimeTypeForExt(ExtractFileExt(path));
end;

//procedure TFhirWebServer.DoSendFHIR(iMsgKey, SrcID: Integer; request: TFHIRRequest; response: TFHIRResponse);
//var
//  client : TFHIRClient;
//begin
//  client := TFhirClient.create(FBaseURL, false);
//  try
//    FClientLock.Lock('MakeClient');
//    Try
//      FClients.Add(client);
//    Finally
//      FClientLock.Unlock;
//    End;
//    try
//      if (request.CommandType = fcmdUpdate) and (request.id = '') then
//        request.id := 'test';
//
//      client.doRequest(request, response);
//    finally
//      FClientLock.Lock('CloseClient');
//      Try
//        FClients.Remove(client);
//            Finally
//        FClientLock.Unlock;
//      End;
//    end;
//  finally
//    client.free;
//  end;
//end;
//
function TFhirWebServer.BuildCompartmentList(user: TFHIRUserStructure): String;
  function tail(s : String):String;
  begin
    result := copy(s, 10, $FF);
  end;
var
  i : integer;
begin
  result := ''''+tail(user.resource.patientList[0])+'''';
  for i := 1 to user.resource.patientList.count - 1 do
    result := result+', '''+tail(user.resource.patientList[i])+'''';
  for i := 0 to user.TaggedCompartments.count - 1 do
    result := result+', '''+user.TaggedCompartments[i]+'''';
end;

{ TFhirServerMaintenanceThread }

constructor TFhirServerMaintenanceThread.create(server: TFHIRWebServer);
begin
  FreeOnTerminate := true;
  FServer := server;
  FLastSweep := now;
  inherited Create;
end;

procedure TFhirServerMaintenanceThread.Execute;
begin
  CoInitialize(nil);
  repeat
    sleep(1000);
    if FServer.FActive then
      FServer.FFhirStore.ProcessSubscriptions;
    if FLastSweep < now - DATETIME_MINUTE_ONE then
    begin
      try
        FServer.FFhirStore.Sweep;
      except
      end;
      FLastSweep := now;
    end;
  until Terminated;
  CoUninitialize;
end;

function TFhirWebServer.transform(resource: TFhirResource; lang, xslt: String): string;
var
  xslt2: AltovaXMLLib_TLB.XSLT2;
  xml : TFHIRXmlComposer;
  b : TBytesStream;
  s : String;
  AltovaXml : AltovaXMLLib_TLB.Application;
begin
  b := TBytesStream.Create(nil);
  try
    xml := TFHIRXmlComposer.Create(lang);
    try
      xml.Compose(b, '', '', resource, false);
    finally
      xml.Free;
    end;
    s := TEncoding.UTF8.GetString(b.Bytes);
  finally
    b.free;
  end;

  AltovaXml := AltovaXMLLib_TLB.CoApplication.Create;
  xslt2 := AltovaXml.XSLT2;
  xslt2.InputXMLFromText := s;
  xslt2.XSLFileName := xslt;
  result := xslt2.ExecuteAndGetResultAsString;
  xslt2 := nil;
  AltovaXml := nil;
end;


End.


