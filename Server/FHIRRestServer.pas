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
  SysUtils, Classes, IniFiles, ActiveX,

  EncodeSupport, GuidSupport, DateSupport, BytesSupport, StringSupport,

  AdvBuffers, AdvObjectLists, AdvStringMatches, AdvZipParts, AdvZipReaders, AdvVCLStreams,

  kCritSct, ParseMap, TextUtilities, KDBManager,
  DCPsha256,

  IdMultipartFormData, IdHeaderList, IdCustomHTTPServer, IdHTTPServer,
  IdTCPServer, IdContext, IdSSLOpenSSL, IdHTTP, IdSoapMime, IdCookie,
  IdSoapTestingUtils,

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

Const
  FHIR_COOKIE_NAME = 'fhir-session-cookie';

Type
  ERestfulAuthenticationNeeded = class (ERestfulException)
  private
    FMsg : String;
  public
    Constructor Create(Const sSender, sMethod, sReason, sMsg : String; aStatus : word); Overload; Virtual;
    Property Msg : String read FMsg;
  end;

  TFhirLoginToken = Class (TAdvObject)
  private
    FProvider : TFHIRAuthProvider;
    FPath : String;
    FExpires : TDateTime;
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

    FClientLock : TCriticalSection;
    FClients : TAdvObjectList;
    FClientCount : Integer;

    FFhirStore : TFHIRDataStore;
    FFacebookLike : boolean;
    FFacebookAppid : String;
    FFacebookAppSecret : String;
    FLoginTokens : TStringList;
    FGoogleAppid : String;
    FGoogleAppSecret : String;
    FGoogleAppKey : String;
    FAppSecrets : String;
    FHL7Appid : String;
    FHL7AppSecret : String;

    function OAuthPath(secure : boolean):String;

    function BuildCompartmentList(user : TFHIRUserStructure) : String;

    function SpecFile(path : String) : String;
    function AltFile(path : String) : String;
    Procedure ReturnSpecFile(response : TIdHTTPResponseInfo; stated, path : String);
    Procedure ReturnProcessedFile(response : TIdHTTPResponseInfo; named, path : String);
    Procedure ReadTags(Headers: TIdHeaderList; Request : TFHIRRequest); overload;
    Procedure ReadTags(header : String; Request : TFHIRRequest); overload;
    Function ProcessOAuthLogin(path, url, ip : String; request : TFHIRRequest; response : TFHIRResponse; var msg : String; secure : boolean) : Boolean;
    Function ProcessDirectLogin(url, ip : String; request : TFHIRRequest; response : TFHIRResponse; var msg : String) : Boolean;
    Function CheckLoginToken(state : string; var original : String; var provider : TFHIRAuthProvider):Boolean;
    function CheckSessionOK(session : TFhirSession; ip : string) : Boolean;
    Function BuildFhirHomePage(comps, lang, host, sBaseURL : String; session : TFhirSession): String;
    Function BuildFhirAuthenticationPage(lang, host, path, msg : String; secure : boolean) : String;
    Function BuildFhirUploadPage(lang, host, sBaseURL : String; aType : TFHIRResourceType; session : TFhirSession) : String;
    Procedure CreatePostStream(AContext: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream);
    Procedure PlainRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    Procedure SecureRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    Procedure HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo; ssl, secure : Boolean; path : String);
    Procedure ProcessOutput(oRequest : TFHIRRequest; oResponse : TFHIRResponse; AResponseInfo: TIdHTTPResponseInfo);
    function extractFileData(const request : TStream; const contentType, name: String; var sContentType : String; var params : String): TStream;
    Procedure StartServer;
    Procedure StopServer;
    Function ProcessZip(lang : String; oStream : TStream) : TFHIRAtomFeed;
    procedure SSLPassword(var Password: AnsiString);
    procedure SendError(AResponseInfo: TIdHTTPResponseInfo; status : word; format : TFHIRFormat; lang, message, url : String; session : TFhirSession; addLogins : boolean; path : String);
    Function MakeLoginToken(path : String; provider : TFHIRAuthProvider) : String;
    Procedure ProcessRequest(request : TFHIRRequest; response : TFHIRResponse);
    function BuildRequest(lang, sBaseUrl, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept, sCookie: String; oPostStream: TStream; oResponse: TFHIRResponse;
      var aFormat: TFHIRFormat; var redirect: boolean; formparams: String; bAuth, secure : Boolean): TFHIRRequest;
    procedure DoConnect(AContext: TIdContext);
    procedure DoDisconnect(AContext: TIdContext);
    Function WebDesc : String;
    function EndPointDesc : String;
  Public
    Constructor Create(ini : TFileName; db : TKDBManager; Name : String);
    Destructor Destroy; Override;

    Procedure Start;
    Procedure Stop;
  End;

Implementation


Uses
  Windows, Registry,

  AdvExceptions,

  FHIRUtilities,

  FileSupport,
  FacebookSupport;

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

Constructor TFhirWebServer.Create(ini : TFileName; db : TKDBManager; Name : String);
Begin
  Inherited Create;
  FLock := TCriticalSection.Create('fhir-rest');
  FName := Name;
  FIni := TIniFile.Create(ini);
  FLoginTokens := TStringList.create;
  writeln('Load & Cache Store');
  FSpecPath := ProcessPath(ExtractFilePath(ini), FIni.ReadString('fhir', 'source', ''));
  FAltPath := ProcessPath(ExtractFilePath(ini), FIni.ReadString('fhir', 'other', ''));

  FFhirStore := TFHIRDataStore.Create(db, FSpecPath, FAltPath);

  // Basei Web server configuration
  FBasePath := FIni.ReadString('web', 'base', '');
  FSecurePath := FIni.ReadString('web', 'secure', '');
  FPort := FIni.ReadInteger('web', 'http', 0);
  FSSLPort := FIni.ReadInteger('web', 'https', 0);
  FCertFile := FIni.ReadString('web', 'certname', '');
  FSSLPassword := FIni.ReadString('web', 'certpword', '');
  FHost := FIni.ReadString('web', 'host', '');


  // OAuth Configuration
  FHL7Appid := FIni.ReadString('hl7.org', 'app-id', '');
  FHL7AppSecret := FIni.ReadString('hl7.org', 'app-secret', '');
  FFacebookLike := FIni.ReadString('facebook.com', 'like', '') = '1';
  FFacebookAppid := FIni.ReadString('facebook.com', 'app-id', '');
  FFacebookAppSecret := FIni.ReadString('facebook.com', 'app-secret', '');
  FGoogleAppid := FIni.ReadString('google.com', 'app-id', '');
  FGoogleAppSecret := FIni.ReadString('google.com', 'app-secret', '');
  FGoogleAppKey := FIni.ReadString('google.com', 'app-key', '');

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
  FIni.Free;
  FFhirStore.CloseAll;
  FLoginTokens.Free;
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

Procedure TFhirWebServer.Start;
Begin
  StartServer;
End;

Procedure TFhirWebServer.Stop;
Begin
  StopServer;
End;

Procedure TFhirWebServer.StartServer;
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
    FPlainServer.Active := True;
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
    FSSLServer.Active := True;
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
    if not (id[i] in ['a'..'z', '0'..'9', 'A'..'Z', '.', '-']) then
      Raise ERestfulException.Create('TFhirWebServer', 'SplitId', StringFormat(GetFhirMessage('MSG_ID_INVALID', lang), [id, id[i]]), HTTP_ERR_BAD_REQUEST);
end;

procedure setCookie(AResponseInfo: TIdHTTPResponseInfo; const cookiename, cookieval, domain, path: String; expiry: TDateTime; secure: Boolean);
var
  cookie: TIdCookie;
begin
  cookie := AResponseInfo.Cookies.Add;
  cookie.CookieName := cookiename;
  cookie.Value := cookieval;
  cookie.Domain := domain;
  cookie.Path := path;
  cookie.Expires := expiry;
  cookie.Secure := secure;
end;

Procedure TFhirWebServer.PlainRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if FileExists(SpecFile(ARequestInfo.Document)) then
    ReturnSpecFile(AResponseInfo, ARequestInfo.Document, SpecFile(ARequestInfo.Document))
  else if FileExists(AltFile(ARequestInfo.Document)) then
    ReturnSpecFile(AResponseInfo, ARequestInfo.Document, AltFile(ARequestInfo.Document))
  else if FileExists(FAltPath+ExtractFileName(ARequestInfo.Document.replace('/', '\'))) then
    ReturnSpecFile(AResponseInfo, ARequestInfo.Document, FAltPath+ExtractFileName(ARequestInfo.Document.replace('/', '\')))
  else if FileExists(FSpecPath+ExtractFileName(ARequestInfo.Document.replace('/', '\'))) then
    ReturnSpecFile(AResponseInfo, ARequestInfo.Document, FSpecPath+ExtractFileName(ARequestInfo.Document.replace('/', '\')))
  else if ARequestInfo.Document.StartsWith(FBasePath, false) then
    HandleRequest(AContext, ARequestInfo, AResponseInfo, false, false, FBasePath)
  else if ARequestInfo.Document.StartsWith(FSecurePath, false) then
    HandleRequest(AContext, ARequestInfo, AResponseInfo, false, true, FSecurePath)
  else if ARequestInfo.Document = '/' then
    ReturnProcessedFile(AResponseInfo, '/hompage.html', AltFile('/homepage.html'))
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := 'Document '+ARequestInfo.Document+' not found';
    writeln('miss: '+ARequestInfo.Document);
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


Procedure TFhirWebServer.SecureRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if FileExists(SpecFile(ARequestInfo.Document)) then
    ReturnSpecFile(AResponseInfo, ARequestInfo.Document, SpecFile(ARequestInfo.Document))
  else if FileExists(IncludeTrailingPathDelimiter(FAltPath)+ARequestInfo.Document) then
    ReturnSpecFile(AResponseInfo, ARequestInfo.Document, IncludeTrailingPathDelimiter(FAltPath)+ARequestInfo.Document)
  else if FileExists(AltFile(ExtractFileName(ARequestInfo.Document))) then
    ReturnSpecFile(AResponseInfo, ARequestInfo.Document, AltFile(ExtractFileName(ARequestInfo.Document)))
  else if ARequestInfo.Document.StartsWith(FBasePath, false) then
    HandleRequest(AContext, ARequestInfo, AResponseInfo, true, false, FBasePath)
  else if ARequestInfo.Document.StartsWith(FSecurePath, false) then
    HandleRequest(AContext, ARequestInfo, AResponseInfo, true, true, FSecurePath)
  else if ARequestInfo.Document = '/' then
    ReturnProcessedFile(AResponseInfo, '/hompage.html', AltFile('/homepage.html'))
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := 'Document '+ARequestInfo.Document+' not found';
    writeln('miss: '+ARequestInfo.Document);
  end;
end;

Procedure TFhirWebServer.HandleRequest(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo; ssl, secure : Boolean; path : String);
var
  sHost : string;
  oRequest : TFHIRRequest;
  oResponse : TFHIRResponse;
  sContent : String;
  sCookie : string;
  sContentType : String;
  sType : String;
  oStream : TStream;
  sDoc : String;
  profileparam : String;
  s : String;
  aFormat : TFHIRFormat;
  lang : String;
  sPath : String;
  session : TFhirSession;
  redirect : Boolean;
  formparams : String;
Begin
  Session := nil;
  try
      if ssl then
        sHost := 'https://'+ARequestInfo.Host
      else
        sHost := 'http://'+ARequestInfo.Host;
      lang := ARequestInfo.AcceptLanguage;
      s := ARequestInfo.ContentType;
      if pos(';', s) > 0 then
        s := copy(s, 1, pos(';', s) - 1); //only read up to the first ';'
      if (SameText(s, 'application/x-www-form-urlencoded') or SameText(ARequestInfo.Command, 'get') or SameText(ARequestInfo.Command, 'options')) and (ARequestInfo.UnparsedParams <> '') then
        sDoc := ARequestInfo.Document +'?'+ ARequestInfo.UnparsedParams
      else
        sDoc := ARequestInfo.Document;
      try
        sContentType := ARequestInfo.ContentType;
      if sContentType = '' then
        sContentType := ARequestInfo.Accept;

      if s.StartsWith('multipart/form-data', true) then
        oStream := extractFileData(ARequestInfo.PostStream, ARequestInfo.ContentType, 'file', sContentType, formparams)
      else if ARequestInfo.PostStream <> nil then
      begin
        oStream := TMemoryStream.create;
        oStream.CopyFrom(ARequestInfo.PostStream, ARequestInfo.PostStream.Size);
        oStream.Position := 0;
      end
      else
        oStream := TStringStream.Create(ARequestInfo.UnparsedParams);
      try
        AResponseInfo.RawHeaders.add('Access-Control-Allow-Origin: *');
        AResponseInfo.Expires := Now - 1; //don't want anyone caching anything
        oResponse := TFHIRResponse.Create;
        Try
          if ARequestInfo.Cookies.Cookie[FHIR_COOKIE_NAME, FHost] <> nil then
            sCookie := ARequestInfo.Cookies.Cookie[FHIR_COOKIE_NAME, FHost].CookieText;

          oRequest := BuildRequest(lang, path, sHost, ARequestInfo.CustomHeaders.Values['Origin'], ARequestInfo.RemoteIP, ARequestInfo.CustomHeaders.Values['content-location'],
             ARequestInfo.Command, sDoc, sContentType, ARequestInfo.Accept, sCookie, oStream, oResponse, aFormat, redirect, formparams, secure, ssl);
          try
            ReadTags(ARequestInfo.CustomHeaders.Values['Categories'], oRequest);
            session := oRequest.Session.Link;
            if redirect then
            begin
              if oRequest.Session <> nil then
              begin
                setCookie(AResponseInfo, FHIR_COOKIE_NAME, oRequest.Session.Cookie, '' {oCtxt.Host}, '/'{sPath + FBaseURL}, oRequest.Session.Expires, false {oCtxt.IsSSL});
                AResponseInfo.Redirect(oRequest.Session.OriginalUrl);
              end
              else
                AResponseInfo.Redirect(oRequest.baseUrl);
            end
            else if oRequest.CommandType = fcmdUnknown then
            begin
              AResponseInfo.ResponseNo := 200;
              AResponseInfo.ContentType := 'text/html';
              AResponseInfo.FreeContentStream := true;
              AResponseInfo.ContentStream := StringToUTFStream(BuildFhirHomePage(oRequest.compartments, lang, sHost, path, oRequest.Session));
            end
            else if (oRequest.CommandType = fcmdUpload) and (oRequest.Resource = nil) and (oRequest.Feed = nil) Then
            begin
              AResponseInfo.ResponseNo := 200;
              AResponseInfo.ContentType := 'text/html';
              AResponseInfo.FreeContentStream := true;
              AResponseInfo.ContentStream := StringToUTFStream(BuildFhirUploadPage(lang, sHost, '', oRequest.ResourceType, oRequest.Session));
            end
            else if (oRequest.CommandType = fcmdConformanceStmt) and (oRequest.ResourceType <> frtNull) then
            begin
              AResponseInfo.ResponseNo := 200;
              AResponseInfo.ContentType := 'text/html';
              AResponseInfo.RawHeaders.add('Access-Control-Allow-Origin: '+ARequestInfo.RawHeaders.Values['Origin']);
              AResponseInfo.RawHeaders.add('Access-Control-Request-Method: GET, POST, PUT, DELETE');
              AResponseInfo.FreeContentStream := true;
              AResponseInfo.ContentStream := StringToUTFStream('OK');
            end
            else
            begin
              try
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
              ProcessOutput(oRequest, oResponse, AResponseInfo);
              if ARequestInfo.RawHeaders.Values['Origin'] <> '' then
                AResponseInfo.RawHeaders.add('Access-Control-Allow-Origin: '+ARequestInfo.RawHeaders.Values['Origin']);
              AResponseInfo.ETag := oResponse.versionId;
              AResponseInfo.LastModified := oResponse.lastModifiedDate; // todo: timezone
              if oResponse.Categories.count > 0 then
                AResponseInfo.RawHeaders.add('Category: '+ oResponse.Categories.AsHeader);
              if oResponse.originalId <> '' then
                AResponseInfo.RawHeaders.add('X-Original-Location: '+oResponse.originalId);
              if oResponse.ContentLocation <> '' then
                AResponseInfo.RawHeaders.add('Content-Location: '+oResponse.ContentLocation);
              if oResponse.Location <> '' then
                AResponseInfo.Location := oResponse.Location;
            end;
            AResponseInfo.WriteContent;
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
          AResponseInfo.ResponseNo := 200;
          AResponseInfo.ContentType := 'text/html';
          AResponseInfo.FreeContentStream := true;
          AResponseInfo.ContentStream := StringToUTFStream(BuildFhirAuthenticationPage(lang, sHost, sPath + sDoc, e.Msg, ssl));
        end
        else
          SendError(AResponseInfo, e.Status, aFormat, lang, e.Message, sPath, session, true, sPath + sDoc);
      end;
      on e: ERestfulException do
        SendError(AResponseInfo, e.Status, aFormat, lang, e.Message, sPath, session, false, path);
      on e: Exception do
        SendError(AResponseInfo, HTTP_ERR_INTERNAL, aFormat, lang, e.Message, sPath, session, false, path);
    end;
  finally
    session.free;
  end;
end;

procedure TFhirWebServer.SendError(AResponseInfo: TIdHTTPResponseInfo; status : word; format : TFHIRFormat; lang, message, url : String; session : TFhirSession; addLogins : boolean; path : String);
var
  issue : TFhirOperationOutcome;
  report :  TFhirOperationOutcomeIssue;
  oComp : TFHIRComposer;
  e : TFhirExtension;
begin
  AResponseInfo.ResponseNo := status;
  AResponseInfo.FreeContentStream := true;

  if format = ffAsIs then
  begin
    AResponseInfo.ContentType := 'text/plain';
    AResponseInfo.ContentStream := StringToUTFStream(message);
  end
  else
  begin
    issue := TFhirOperationOutcome.create;
    try
      issue.text := TFhirNarrative.create;
      issue.text.statusST := NarrativeStatusGenerated;
      issue.text.div_ := ParseXhtml(lang, '<div><p>'+FormatTextToXML(message)+'</p></div>');
      if addLogins then
      begin
        if FHL7Appid <> '' then
        begin
          e := issue.ExtensionList.Append;
          e.urlST := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          e.value := TFhirString.create('http://hl7.amg-hq.net/tools/signup_redirect.cfm?apiKey='+FHL7Appid+'&returnURL='+EncodeMime(path)+'/state/'+MakeLoginToken(path, apHL7));
        end;
        if FFacebookAppid <> '' then
        begin
          e := issue.ExtensionList.Append;
          e.urlST := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          e.value := TFhirString.create('https://www.facebook.com/dialog/oauth?client_id='+FFacebookAppid+'&redirect_uri='+path+'&state='+MakeLoginToken(path, apFacebook));
        end;
        if FGoogleAppid <> '' then
        begin
          e := issue.ExtensionList.Append;
          e.urlST := 'http://www.healthintersections.com.au/fhir/extensions#auth-token';
          e.value := TFhirString.create('https://accounts.google.com/o/oauth2/auth?client_id='+FGoogleAppid+'&response_type=code&scope=openid%20email&redirect_uri='+path+'&state='+MakeLoginToken(path, apGoogle));
        end;
      end;
      report := issue.issueList.Append;
      report.severityST := IssueSeverityError;
      report.details := TFHIRString.create(message);
      AResponseInfo.ContentStream := TMemoryStream.Create;
      oComp := nil;
      case format of
        ffXml: oComp := TFHIRXmlComposer.Create(lang);
        ffXhtml:
          begin
          oComp := TFHIRXhtmlComposer.Create(lang);
          TFHIRXhtmlComposer(oComp).BaseURL := AppendForwardSlash(url);
          TFHIRXhtmlComposer(oComp).Session := Session.Link;
          end;
        ffJson: oComp := TFHIRJsonComposer.Create(lang);
      end;
      try
        AResponseInfo.ContentType := oComp.MimeType;
        oComp.Compose(AResponseInfo.ContentStream, '', '', issue, false);
        AResponseInfo.ContentStream.Position := 0;
      finally
        oComp.free;
      end;
    finally
      issue.free;
    end;
  end;
  AResponseInfo.WriteContent;
end;


Function TFhirWebServer.BuildRequest(lang, sBaseUrl, sHost, sOrigin, sClient, sContentLocation, sCommand, sResource, sContentType, sContentAccept, sCookie : String; oPostStream : TStream; oResponse : TFHIRResponse; var aFormat : TFHIRFormat; var redirect : boolean; formparams : String; bAuth, secure : Boolean) : TFHIRRequest;
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


    if (sCommand <> 'GET') then
    begin
      if oRequest.Parameters.VarExists('_format') then
        sContentType := oRequest.Parameters.GetVar('_format');
      if StringStartsWithInsensitive(sContentType, 'application/json') or StringStartsWithInsensitive(sContentType, 'application/fhir+json') or StringStartsWithInsensitive(sContentType, 'application/json+fhir') or StringStartsWithInsensitive(sContentType, 'json') Then
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
      else if (sUrl <> 'auth-login') and FFhirStore.GetSession(sCookie, session, check) then
      begin
        if check and not CheckSessionOK(session, sClient) then
          Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', lang), msg, HTTP_ERR_UNAUTHORIZED);
        oRequest.session := session
      end
      else if (sURL = 'login') then
      begin
        if ProcessDirectLogin(sUrl, sClient, oRequest, oResponse, msg) then
          redirect := true
        else
          Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_LOGIN_FAILED', lang), msg, HTTP_ERR_UNAUTHORIZED);
      end
      else if ProcessOAuthLogin(sBaseUrl, sUrl, sClient, oRequest, oResponse, msg, secure) then
        redirect := true
      else
        Raise ERestfulAuthenticationNeeded.Create('TFhirWebServer', 'HTTPRequest', GetFhirMessage('MSG_AUTH_REQUIRED', lang), msg, HTTP_ERR_UNAUTHORIZED);
    end
    else
      oRequest.session := FFhirStore.CreateImplicitSession(sClient);

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
      else if (sType = 'mailbox') then
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
              CheckId(lang, copy(sId, 2, $FF));
              oRequest.SubId := copy(sId, 2, $FF);
              sId := NextSegment(sURL);
              if sid = '' then
              begin
                ForceMethod('GET');
                oRequest.CommandType := fcmdVersionRead;
              end
              else if (sid = '_tags') then
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
          else if StringArrayExistsSensitive(PLURAL_CODES_TFhirResourceType, sId) then
          begin
            aResourceType := TFHIRResourceType(StringArrayIndexOfSensitive(PLURAL_CODES_TFhirResourceType, sId));
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
            if (sURL <> '') or (sId = '') or (sId[1] <> '@') Then
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

      if (oRequest.CommandType in [fcmdTransaction, fcmdUpdate, fcmdValidate, fcmdCreate, fcmdMailbox]) or ((oRequest.CommandType = fcmdUpload) and (oPostStream.Size > 0)) Then
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
              parser := MakeParser(lang, oRequest.PostFormat, oPostStream);
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
        parser := MakeParser(lang, oRequest.PostFormat, oPostStream);
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
                e.id := 'http://hl7.org/fhir/'+LOWERCASE_CODES_TFHIRResourceType[p.resource.ResourceType]+'/@'+GetStringCell(GetStringCell(rdr.parts[i].Name, 1, '('), 0, ')')
              else if rdr.parts[i].Name.EndsWith('.profile.xml') then
                e.id := 'http://hl7.org/fhir/'+LOWERCASE_CODES_TFHIRResourceType[p.resource.ResourceType]+'/@'+copy(rdr.parts[i].Name, 1, length(rdr.parts[i].Name) - 12)
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


procedure TFhirWebServer.SSLPassword(var Password: AnsiString);
begin
  Password := FSSLPassword;
end;

Procedure TFhirWebServer.ProcessOutput(oRequest : TFHIRRequest; oResponse : TFHIRResponse; AResponseInfo: TIdHTTPResponseInfo);
var
  oComp : TFHIRComposer;
  b : TBytes;
begin
  AResponseInfo.ResponseNo := oResponse.HTTPCode;
  AResponseInfo.ContentType := oResponse.ContentType;
  AResponseInfo.ContentStream := TMemoryStream.Create;
  if oresponse.feed <> nil then
  begin
    if oResponse.Format = ffxhtml then
    begin
      oComp := TFHIRXhtmlComposer.Create(oRequest.lang);
      TFHIRXhtmlComposer(oComp).BaseURL := AppendForwardSlash(oRequest.baseUrl);
      TFHIRXhtmlComposer(oComp).Session := oRequest.Session.Link;
      AResponseInfo.ContentType := oComp.MimeType;
    end
    else if oResponse.format = ffJson then
    begin
      oComp := TFHIRJsonComposer.Create(oRequest.lang);
      AResponseInfo.ContentType := oComp.MimeType;
    end
    else
    begin
      oComp := TFHIRXmlComposer.Create(oRequest.lang);
      AResponseInfo.ContentType := 'application/atom+xml; charset=UTF-8';
    end;
    try
      oComp.Compose(AResponseInfo.ContentStream, oResponse.Feed, false);
    finally
      oComp.Free;
    end;
  end
  else if oResponse.Resource <> nil then
  Begin
    if oResponse.Resource is TFhirBinary then
    begin
      TFhirBinary(oResponse.Resource).Content.SaveToStream(AResponseInfo.ContentStream);
      AResponseInfo.ContentType := TFhirBinary(oResponse.Resource).ContentType;
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
      end
      else if oResponse.Format = ffXml then
        oComp := TFHIRXmlComposer.Create(oRequest.lang)
      else if oResponse.Resource._source_format = ffJson then
        oComp := TFHIRJsonComposer.Create(oRequest.lang)
      else
        oComp := TFHIRXmlComposer.Create(oRequest.lang);
      try
        AResponseInfo.ContentType := oComp.MimeType;
        oComp.Compose(AResponseInfo.ContentStream, oRequest.id, oRequest.subId, oResponse.resource, false);
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
      AResponseInfo.ContentType := oComp.MimeType;
    end
    else if oResponse.format = ffJson then
    begin
      oComp := TFHIRJsonComposer.Create(oRequest.lang);
      AResponseInfo.ContentType := oComp.MimeType;
    end
    else
    begin
      oComp := TFHIRXmlComposer.Create(oRequest.lang);
      AResponseInfo.ContentType := oComp.MimeType;
    end;
    try
      oComp.Compose(AResponseInfo.ContentStream, oRequest.ResourceType, oRequest.Id, oRequest.SubId, oResponse.Categories, false);
    finally
      oComp.Free;
    end;
  end
  else
  begin
    if AResponseInfo.ContentType = '' then
      AResponseInfo.ContentType := 'text/plain';
    b := TEncoding.UTF8.GetBytes(oResponse.Body);
    AResponseInfo.ContentStream.write(b, length(b));
  end;
  AResponseInfo.ContentStream.Position := 0;
end;

procedure TFhirWebServer.ProcessRequest(request: TFHIRRequest; response: TFHIRResponse);
var
  store : TFhirOperation;
  conn : TKDBConnection;
begin
  store := TFhirOperation.Create(request.Lang, FFhirStore.Link);
  try
    store.Request := request.Link;
    store.Response := response.Link;
    store.Connection := FFhirStore.DB.GetConnection('Operation');
    store.precheck;
    try
      store.Connection.StartTransact;
      try
        store.Execute;
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
'<h2>Login to HL7Connect</h2>'+#13#10+
'<form method="POST" action="login">'#13#10+
'  <input type="hidden" name="url" value="'+path+'"/></td></tr> '#13#10+
'  <table border="0">'#13#10+
'   <tr><td>Login: </td><td><input type="text" size="20" name="login"/></td></tr> '#13#10+
'   <tr><td>Password: </td><td><input type="password" size="20" name="password"/></td></tr>'#13#10+
'  </table>'#13#10+
'  <input type="submit" title="Login"/>'#13#10+
'</form>'#13#10+
'<hr/><p><b>Or:</b></p>'+#13#10;
if FHL7Appid <> '' then
result := result +
'<p><a href="http://hl7.amg-hq.net/tools/signup_redirect.cfm?apiKey='+FHL7Appid+'&returnURL='+EncodeMime(authurl+'/state/'+MakeLoginToken(path, apHL7))+'"><img style="vertical-align: middle" src="http://www.hl7.org/assets/systemimages/HL7logo.gif"/> Sign in with your HL7 account</a></p>'+#13#10;

if FFacebookAppid <> '' then
result := result +
'<p><a href="https://www.facebook.com/dialog/oauth?client_id='+FFacebookAppid+'&redirect_uri='+authurl+'&state='+MakeLoginToken(path, apFacebook)+'"><img height="58" width="318" src="/facebook.png"/></a></p>'+#13#10;

if FGoogleAppid <> '' then
result := result +
'<p><a href="https://accounts.google.com/o/oauth2/auth?client_id='+FGoogleAppid+'&response_type=code&scope=openid%20email&redirect_uri='+authurl+'&state='+MakeLoginToken(path, apGoogle)+'"><img height="58" width="318" src="https://developers.google.com/accounts/images/sign-in-with-google.png"/></a></p>'+#13#10;

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
  i, j, k : integer;
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
  StringFormat(GetFhirMessage('MSG_HOME_PAGE_2', lang), [s])+#13#10+
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
            sContentType := 'text/xml'
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

function TFhirWebServer.MakeLoginToken(path : String; provider : TFHIRAuthProvider): String;
var
  login : TFhirLoginToken;
  i : integer;
  t : TDateTime;
begin
  t := now;
  FLock.Lock;
  try
    login := TFhirLoginToken.create;
    try
      login.FPath := path;
      login.FExpires := now + DATETIME_MINUTE_ONE * 30;
      login.Fprovider := provider;
      result := OAUTH_LOGIN_PREFIX + copy(GUIDToString(CreateGuid), 2, 36);
      FLoginTokens.AddObject(result, login.link);
    finally
      login.free;
    end;
    for i := FLoginTokens.Count - 1 downto 0 do
    begin
      login := TFhirLoginToken(FLoginTokens.Objects[i]);
      if login.FExpires < t then
      begin
        login.free;
        FLoginTokens.Delete(i);
      end;
    end;
  finally
    FLock.Unlock;
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

function TFhirWebServer.ProcessDirectLogin(url, ip : String; request: TFHIRRequest; response: TFHIRResponse; var msg : String): Boolean;
var
  login, password : String;
  user : TFhirUser;
  len : String;
begin
  result := false;
  login := request.Parameters.GetVar('login');
  password := request.Parameters.GetVar('password');
  user := FFhirStore.GetFhirUser('', Login);
  len := user.sessionLength;
  if len = '' then
    len := '4800';
  if user <> nil then
    try
      result := SameText(HashPword(password), user.password);
      if result then
        request.Session := FFhirStore.RegisterSession(apNone, '', user.login, user.name, user.email, request.Parameters.getVar('url'), len, ip);
    finally
      user.free;
    end;
end;

function TFhirWebServer.ProcessOAuthLogin(path, url, ip : String; request: TFHIRRequest; response: TFHIRResponse; var msg : String; secure : boolean): Boolean;
var
  token, expires, state, id, name, original, email, pname, idt : String;
  provider : TFHIRAuthProvider;
  ok : boolean;
begin
  if url = 'auth-login' then
  begin
    // direct login
    pname := request.Parameters.GetVar('provider');
    token := request.Parameters.GetVar('access_token');
    idt := request.Parameters.GetVar('id_token');
    expires := request.Parameters.GetVar('expires');
    if pname = '' then
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Provider Required', HTTP_ERR_BAD_REQUEST);
    if (pname = 'google') then
      provider := apGoogle
    else if (pname = 'facebook') then
      provider := apFacebook
    else if (pname = 'custom') then
      provider := apCustom
    else
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Provider Value invalid (facebook or google)', HTTP_ERR_BAD_REQUEST);
    if (provider <> apCustom) and (token = '') then
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Access Token Required', HTTP_ERR_BAD_REQUEST);
    if (pname = 'google') and (idt = '') then
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'ID Token required for google logins', HTTP_ERR_BAD_REQUEST);
    if expires = '' then
      expires := '1800'; // 30min
    if provider = apGoogle then
      result := GoogleGetDetails(token, FGoogleAppKey, id, name, msg)
    else if provider = apFacebook then
      result := FacebookGetDetails(token, id, name, msg)
    else
    begin
      id := request.Parameters.GetVar('id');
      name := request.Parameters.GetVar('name');
      if pos(request.Parameters.GetVar('secret'), FAppSecrets) = 0 then
        raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Application Secret not recognised', HTTP_ERR_BAD_REQUEST);
      result := true;
    end;
    if not result then
      raise ERestfulException.create('TFhirWebServer', 'ProcessOAuthLogin', 'Unable to confirm Access Token: '+msg, HTTP_ERR_BAD_REQUEST);
    request.Session := FFhirStore.RegisterSession(provider, token, id, name, email, FSecurePath, expires, ip);
  end
  else if StringStartsWith(url, 'state/', false) and StringStartsWith(copy(url, 7, $FF), OAUTH_LOGIN_PREFIX, false) then
  begin
    // HL7
    result := CheckLoginToken(copy(url, 7, $FF), original, provider);
    if result then
    begin
      // todo: check the signature
      id := request.Parameters.GetVar('userid');
      name := request.Parameters.GetVar('fullName');
      expires := inttostr(60 * 24 * 10); // 10 days
      request.Session := FFhirStore.RegisterSession(aphl7, '', id, name, email, original, expires, ip);
    end;
  end
  else
  begin
    result := request.Parameters.VarExists('state');
    if result then
    begin
      state := request.Parameters.GetVar('state');
      result := StringStartsWith(state, OAUTH_LOGIN_PREFIX, false);
      if result then
      begin
        result := false;
        if not CheckLoginToken(state, original, provider) then
          msg := 'The state does not match. You may be a victim of a cross-site spoof'
        else if request.Parameters.VarExists('error') then
          msg := request.Parameters.GetVar('error_description')
        else
        begin
          if provider = apGoogle then
          begin
            ok := GoogleCheckLogin(FGoogleAppid, FGoogleAppSecret, OAuthPath(secure), request.Parameters.GetVar('code'), token, expires, msg);
            if ok then
              result := GoogleGetDetails(token, FGoogleAppKey, id, name, msg);
          end
          else
          begin
            ok := FacebookCheckLogin(FFacebookAppid, FFacebookAppSecret, OAuthPath(secure), request.Parameters.GetVar('code'), token, expires, msg);
            if ok then
              result := FacebookGetDetails(token, id, name, msg);
          end;
          if result then
            request.Session := FFhirStore.RegisterSession(provider, token, id, name, email, original, expires, ip);
        end;
      end;
    end;
  end;
end;

function TFhirWebServer.CheckLoginToken(state: string; var original : String; var provider : TFHIRAuthProvider): Boolean;
var
  i : integer;
  token : TFhirLoginToken;
begin
  FLock.Lock;
  try
    i := FLoginTokens.Indexof(state);
    result := i <> -1;
    if result then
    begin
      token := TFhirLoginToken(FLoginTokens.Objects[i]);
      original := token.FPath;
      provider := token.FProvider;
      token.free;
      FLoginTokens.Delete(i);
    end;
  finally
    FLock.Unlock;
  end;
end;

constructor ERestfulAuthenticationNeeded.Create(const sSender, sMethod, sReason, sMsg: String; aStatus : Word);
begin
  Create(sSender, sMethod, sReason, aStatus);
  FMsg := sMsg;
end;


function TFhirWebServer.CheckSessionOK(session: TFhirSession; ip : string): Boolean;
var
  id, name, msg : String;
begin
  if session.provider = apGoogle then
    result := GoogleGetDetails(session.token, FGoogleAppKey, id, name, msg)
  else if session.provider = apFacebook then
    result := FacebookGetDetails(session.token, id, name, msg)
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


procedure TFhirWebServer.ReturnProcessedFile(response: TIdHTTPResponseInfo; named, path: String);
var
  s : String;
  b : TBytesStream;
begin
  writeln('script: '+named);
  s := FileToString(path, TEncoding.UTF8);
  s := s.Replace('[%id%]', FName, [rfReplaceAll]);
  s := s.Replace('[%ver%]', FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION, [rfReplaceAll]);
  s := s.Replace('[%web%]', WebDesc, [rfReplaceAll]);
  s := s.Replace('[%endpoints%]', EndPointDesc, [rfReplaceAll]);
  response.ContentStream := TBytesStream.Create(TEncoding.UTF8.GetBytes(s));
  response.FreeContentStream := true;
  response.ContentType := 'text/html';
end;

procedure TFhirWebServer.ReturnSpecFile(response : TIdHTTPResponseInfo; stated, path: String);
begin
  writeln('file: '+stated);
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
  list : TStringList;
begin
  result := ''''+tail(user.resource.patientList[0])+'''';
  for i := 1 to user.resource.patientList.count - 1 do
    result := result+', '''+tail(user.resource.patientList[i])+'''';
  for i := 0 to user.TaggedCompartments.count - 1 do
    result := result+', '''+user.TaggedCompartments[i]+'''';
end;

End.


