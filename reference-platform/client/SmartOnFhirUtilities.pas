unit SmartOnFhirUtilities;

{
todo:
Apps MUST generate an unpredictable state parameter for each user session. An app MUST validate the state value for any request sent to its redirect URL; include state with all authorization requests; and validate the state value included in access tokens it receives.

* support AUD on redirect url
* add redirect url to token requeest
* implement refresh_token on server / support in client
* The authorization server’s response MUST include the HTTP “Cache-Control” response header field with a value of “no-store,” as well as the “Pragma” response header field with a value of “no-cache.”
* On occasion, an app may receive a FHIR resource that contains a “reference” to a resource hosted on a different resource server. The app SHOULD NOT blindly follow such references and send along its access_token, as the token may be subject to potential theft. The app SHOULD either ignore the reference, or initiate a new request for access to that resource.
* http://smarthealthit.org/fhir/scopes/patient/*.read.. openID scopes have a URI prefix of http://openid.net/specs/openid-connect-core-1_0#
* implement http://hl7.org/fhir/smart-app-launch/conformance/

}
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



{
Using the Smart App Launch client library

Using Smart App Launch consists of 2 main phases:

1. registration
- you must register with the specific Smart App Launch Server. You give it a name, and a redirect URL, and it will return a client id

- for this library, the redirect URL must take the form of http://localhost:[port]/done where post is any number
  between 1 and 65535. There is no flexibility associated with this

- if you are a confidential application (e.g. server side) you can also ask for a client secret. This library supports secrets,
  though it is a server side library that is not associated with actual secret applications (but it's supported anyway for
  development assistance.

- you also need to know the authorization and token end points on the server. These are advertised in the
  server's conformance statement, and the function usesSmartOnFHIR below can extract them for you

- at the end of the registration process, you have all the information in the TRegisteredServer record

2. Operational Use
- Create a TSmartOnFhirLoginForm. give it:
  * logoPath : the path to a PNG describing your application (used by some servers)
  * server: the server details from the registration phase
  * scopes: the scopes you are asking. See Smart App Launch documentation for what are valid scopes
  * handle error:
       if this is false, and an error occurs, the form will close without telling
       the user about the error, and the host can handle the user. If this is true,
       the form will handle it (this is generally a more comfortable UI for the user)

- Important: you may need to set the following registry key to get OAuth to work:
  // HKEY_LOCAL_MACHINE\SOFTWARE\(Wow6432Node\)Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION\[your exe name] = 10000
  see https://msdn.microsoft.com/en-us/library/ee330730(v=vs.85).aspx or http://blogs.msdn.com/b/patricka/archive/2015/01/12/controlling-webbrowser-control-compatibility.aspx

  (alternate approach: put <meta http-equiv="X-UA-Compatible" content="IE=edge"> in every page...)

- Show the form (.showmodal).

- the form will create an HTTP Server that only binds to the local host. This
  SHOULD avoid problems with firewalls, etc, but this is not guaranteed.
  Note that a localhost server is required because the POX IE control doesn't inform
  the host application about redirects, and OAuth finishes with a redirect.
  (And why not use DCEF? - it's not maintained to the current version of webkit,
  it's a distibution nightmare, and you can't use it in dlls)

- the modal result of the form will have one of the 3 values
  mrOK: Authorization succeeded. The outcome information will be able in .Token on the form - a TSmartOnFhirAccessToken
  mrCancel: the user cancelled the process
  mrAbort: something went wrong. if HandleError is false, a description of what went wrong will be in ErrorMessage

- note that the OAuth process in the browser could go wrong for many reasons that are not known to the
  Smart App Launch client. In these cases, the user has no choice but to process cancel, so you should not
  assume that cancel means the user chose not to succeed - just that they already know it failed.

3. CDS hooks Support

 when registering a server, you can specify a list of CDS Hooks that it implements.
 For further information, see CDSHooksUtilities.
}

interface

uses
  SysUtils, Classes,
  IdContext, IdHTTPServer, IdCustomHTTPServer, IdSocketHandle, IdHTTP, IdSSLOpenSSL,
  StringSupport, EncodeSupport, DateSupport, JWT, ParseMap, GuidSupport, TextUtilities,
  AdvObjects, AdvJson, AdvGenerics,
  FHIRBase, FHIRResources, FHIRTypes, FHIRUtilities;

type
  TRegisteredCDSHook = class (TAdvObject)
  private
    Fname: String;
    FpreFetch: TStringList;
    FHook : String;
  public
    constructor Create; override;
    Destructor Destroy; Override;
    Function Link : TRegisteredCDSHook; overload;

    property name : String read Fname write Fname;
    property hook : String read FHook write FHook;
    property preFetch : TStringList read FpreFetch;
  end;

  TSmartAppLaunchMode = (salmNone, salmOAuthClient, salmBackendClient);

const
  CODES_TSmartAppLaunchMode : array [TSmartAppLaunchMode] of string = ('None', 'OAuth Client', 'Backend Services');
type
  // information about a server required to get Smart App Launch working
  TRegisteredFHIRServer = class (TAdvObject)
  private
    Fname: String;
    FfhirEndpoint: String;
    FSmartAppLaunchMode: TSmartAppLaunchMode;
    Fclientid: String;
    Fclientsecret: String;
    FautoUseHooks: boolean;
    Fcdshooks: TAdvList<TRegisteredCDSHook>;
    Fredirectport: integer;
    FtokenEndpoint: String;
    FauthorizeEndpoint: String;
    FFormat: TFHIRFormat;
    FPassword: String;
    FUserName: String;
    FissuerUrl: String;
    Fpassphrase: String;
    Fprivatekey: String;
    FSSLPassphrase: String;
    FSSLPublicCert: String;
    FSSLPrivateKey: String;
  public
    constructor Create; override;
    Destructor Destroy; Override;
    Function Link : TRegisteredFHIRServer; overload;
    procedure clear;
    procedure writeToJson(o : TJsonObject);
    procedure readFromJson(o : TJsonObject);

    function addCdsHook(name : String; hook : String) : TRegisteredCDSHook;
    function cdshookSummary : String;
    function doesHook(c : string; var hook : TRegisteredCDSHook) : boolean;

    // user casual name for the server
    property name : String read Fname write Fname;

    // the FHIR Base endpoint for the server
    property fhirEndpoint : String read FfhirEndpoint write FfhirEndpoint;

    // as is, choose from the conformance statement at run time. Else XML or JSON
    property format : TFHIRFormat read FFormat write FFormat;

    // whether the server needs Smart App Launch
    property SmartAppLaunchMode: TSmartAppLaunchMode read FSmartAppLaunchMode write FSmartAppLaunchMode;

    // you can get these 2 endpoints from the fhirEndPoint using usesSmartOnFHIR below:

    // where to point the browser to authorise a user
    property authorizeEndpoint : String read FauthorizeEndpoint write FauthorizeEndpoint;

    // where to get the access token after authorization has completed
    property tokenEndpoint : String read FtokenEndpoint write FtokenEndpoint;

    // registered client id
    property clientid : String read Fclientid write Fclientid;

    // client secret, if we're pretending we're a confidential application.
    // this is for testing purposes; the notepad++ plug-in is not a confidential app
    // (nor any other application using this library)
    property clientsecret : String read Fclientsecret write Fclientsecret;

    // the port for redirecting to this server
    property redirectport : integer read Fredirectport write Fredirectport;

    // backend services
    property issuerUrl : String read FissuerUrl write FissuerUrl;
    property privatekey : String read Fprivatekey write Fprivatekey;
    property passphrase : String read Fpassphrase write Fpassphrase;

    // what CDS hooks are used on this server
    property cdshooks : TAdvList<TRegisteredCDSHook> read Fcdshooks;

    // whether to use hooks automatically, or only if the server is connected
    property autoUseHooks : boolean read FautoUseHooks write FautoUseHooks;

    property username : String read FUserName write FUserName;
    property password : String read FPassword write FPassword;

    property SSLPublicCert : String read FSSLPublicCert write FSSLPublicCert;
    property SSLPrivateKey : String read FSSLPrivateKey write FSSLPrivateKey;
    property SSLPassphrase : String read FSSLPassphrase write FSSLPassphrase;
    function isSSL : boolean;
  end;

  // result of a Smart App Launch authorization
  TSmartOnFhirAccessToken = class (TAdvObject)
  private
    FidToken: TJWT;
    Fscopes: String;
    FaccessToken: String;
    Fexpires: TDateTime;
    procedure SetidToken(const Value: TJWT);
  public
    Destructor Destroy; override;

    function link : TSmartOnFhirAccessToken; overload;

    // the bearer access token to add to the HTTP calls. If you assign the
    // access token to a TFhirHTTPClient, it will do this
    property accessToken : String read FaccessToken write FaccessToken;

    // when this token expires (or 0 if expiry is unknown) (in local time)
    property expires : TDateTime read Fexpires write Fexpires;

    // the scopes that were actualyl granted (may be more or less than originally asked for)
    property scopes : String read Fscopes write Fscopes;

    // if an openID token was returned, the details in it (note: unverified)
    property idToken : TJWT read FidToken write SetidToken;

    // convenient username for the user, if any appropriate information is available
    function username : String;
  end;

  // called by a client to login via Smart App Launch
  // launches the user's referred browser, and waits for the cycle to complete
  TSmartLoginState = (stStarting, stDone, stComplete, stError);
  TIdleEvent = procedure(out stop : boolean) of object;
  TOpenURLEvent = procedure(url : String) of object;
  TSmartAppLaunchLogin = class (TAdvObject)
  private
    FOnIdle: TIdleEvent;
    Ftoken: TSmartOnFhirAccessToken;
    Fserver: TRegisteredFHIRServer;

    webserver : TIdHTTPServer;

    FFinalState : string;
    FAuthCode : String;
    FInitialState : string;
    FLogoPath: String;
    FScopes: TArray<String>;
    State : TSmartLoginState;
    FOnOpenURL: TOpenURLEvent;
    Fversion: String;
    FName: String;
    FErrorMessage : String;

    procedure SetServer(const Value: TRegisteredFHIRServer);
    procedure Settoken(const Value: TSmartOnFhirAccessToken);
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure initWebServer;
    procedure openBrowser;
    procedure closeWebServer;
    function template(title, body, redirect: String): String;
    function loginOAuthClient: boolean;
    function loginBackendClient : boolean;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    property server : TRegisteredFHIRServer read Fserver write SetServer;
    property scopes : TArray<String> read FScopes write FScopes;
    property OnIdle : TIdleEvent read FOnIdle write FOnIdle;
    property OnOpenURL : TOpenURLEvent read FOnOpenURL write FOnOpenURL;
    property token : TSmartOnFhirAccessToken read Ftoken write Settoken;
    property name : String read FName write FName;
    property version : String read Fversion write Fversion;
    property logoPath : String read FLogoPath write FLogoPath;
    property ErrorMessage : String read FErrorMessage write FErrorMessage;

    function login : boolean;
  end;

// given a conformance statement, check the server is using Smart App Launch, and extract the end points
function usesSmartOnFHIR(conf : TFhirCapabilityStatement; var authorize, token, register: String): Boolean;

// build the launch token (used by the form)
function buildAuthUrl(server : TRegisteredFHIRServer; scopes, state : String) : String;

// called after the authorization part finishes to get an actual access token
function getSmartOnFhirAuthToken(server : TRegisteredFHIRServer; authcode : String) : TSmartOnFhirAccessToken;



implementation

function buildAuthUrl(server : TRegisteredFHIRServer; scopes, state : String) : String;
begin
  result := server.authorizeEndpoint+'?response_type=code&client_id='+server.clientid+'&redirect_uri=http://localhost:'+inttostr(server.redirectport)+'/done&scope='+EncodeMIME(scopes)+'&state='+state+'&aud='+server.fhirEndpoint;
end;

function usesSmartOnFHIR(conf : TFhirCapabilityStatement; var authorize, token, register: String): Boolean;
var
  cc : TFhirCodeableConcept;
  ex1, ex2 : TFhirExtension;
begin
  result := false;
  authorize := '';
  token := '';
  if conf.restList.Count <> 1 then
    raise Exception.Create('Unable to find rest entry in conformance statement');
  if (conf.restList[0].security <> nil) then
  begin
    for cc in conf.restList[0].security.serviceList do
      if cc.hasCode('http://hl7.org/fhir/restful-security-service', 'SMART-on-FHIR') or cc.hasCode('http://hl7.org/fhir/restful-security-service', 'OAuth2') or
        // temporary work around for Josh's server
         cc.hasCode('http://hl7.org/fhir/vs/restful-security-service', 'SMART-on-FHIR') or cc.hasCode('http://hl7.org/fhir/vs/restful-security-service', 'OAuth2') then
      begin
        for ex1 in conf.restList[0].security.extensionList do
          if ex1.url = 'http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris' then
            for ex2 in ex1.extensionList do
              if ex2.url = 'authorize' then
                authorize := TFHIRUri(ex2.value).value
              else if ex2.url = 'token' then
                token := TFHIRUri(ex2.value).value
              else if ex2.url = 'register' then
                register := TFHIRUri(ex2.value).value;

      end;
  end;
  result := (token <> '') and (authorize <> '');
end;

function getSmartOnFhirAuthTokenRequest(server : TRegisteredFHIRServer; request : String) : TSmartOnFhirAccessToken;
var
  http: TIdHTTP;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  post, resp : TBytesStream;
  json : TJSONObject;
  s : String;
begin
  result := nil;

  post := TBytesStream.create(TEncoding.UTF8.getBytes(request));
  try
    http := TIdHTTP.Create(nil);
    Try
      if server.clientsecret <> '' then
      begin
        http.Request.BasicAuthentication := True;
        http.Request.Username := server.clientid;
        http.Request.Password := server.clientsecret;
      end;
      ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
      Try
        http.IOHandler := ssl;
        ssl.SSLOptions.Mode := sslmClient;
        ssl.SSLOptions.Method := sslvTLSv1_2;
        http.Request.ContentType := 'application/x-www-form-urlencoded; charset=UTF-8';
        resp := TBytesStream.create;
        try
          try
            http.Post(server.tokenEndpoint, post, resp);
            resp.position := 0;
            json := TJSONParser.Parse(resp);
            try
              result := TSmartOnFhirAccessToken.Create;
              try
                if json.vStr['token_type'] <> 'Bearer' then
                  raise Exception.Create('token type is not "Bearer"');
                result.accesstoken := json.vStr['access_token'];
                result.scopes := json.vStr['scope'];
                s := json.vStr['expires_in'];
                if (s <> '') then
                begin
                  if not StringIsInteger16(s) then
                    raise Exception.Create('expires_in is not an integer');
                  result.expires := now + StrToInt(s) * DATETIME_SECOND_ONE;
                end;
                if json.vStr['id_token'] <> '' then
                  result.idToken := TJWTUtils.unpack(json.vStr['id_token'], false, nil);
                result.Link;
              finally
                result.Free;
              end;
            finally
              json.free;
            end;
          except
            on e : EIdHTTPProtocolException do
              raise Exception.Create(e.message+' : '+e.ErrorMessage);
            on e:Exception do
              raise;
          end;
        finally
          resp.free;
        end;
      finally
        ssl.free;
      end;
    finally
      http.free;
    end;
  finally
    post.free;
  end;
end;

function getSmartOnFhirAuthToken(server : TRegisteredFHIRServer; authcode : String) : TSmartOnFhirAccessToken;
begin
  if server.clientsecret = '' then
    result := getSmartOnFhirAuthTokenRequest(server, 'code='+authcode+'&grant_type=authorization_code&client_id='+server.clientid+'&redirect_uri='+EncodeMime('http://localhost:'+inttostr(server.redirectport)+'/done'))
  else
    result := getSmartOnFhirAuthTokenRequest(server, 'code='+authcode+'&grant_type=authorization_code&redirect_uri='+EncodeMime('http://localhost:'+inttostr(server.redirectport)+'/done'));
end;

{ TSmartOnFhirAccessToken }

destructor TSmartOnFhirAccessToken.Destroy;
begin
  FidToken.Free;
  inherited;
end;

function TSmartOnFhirAccessToken.link: TSmartOnFhirAccessToken;
begin
  result := TSmartOnFhirAccessToken(inherited Link);
end;

procedure TSmartOnFhirAccessToken.SetidToken(const Value: TJWT);
begin
  FidToken.Free;
  FidToken := Value;
end;

function TSmartOnFhirAccessToken.username: String;
begin
  if FidToken = nil then
    result := '??'
  else
    result := idtoken.name
end;

{ TRegisteredFHIRServer }

function TRegisteredFHIRServer.addCdsHook(name: String; hook: String): TRegisteredCDSHook;
begin
  result := TRegisteredCDSHook.Create;
  try
    result.name := name;
    result.hook := hook;
    cdshooks.add(result.link);
  finally
    result.Free;
  end;
end;

function TRegisteredFHIRServer.cdshookSummary: String;
var
  c : TRegisteredCDSHook;
begin
  result := '';
  for c in cdshooks do
    CommaAdd(result, c.name);
end;

procedure TRegisteredFHIRServer.clear;
begin
  Fname := '';
  FfhirEndpoint := '';
  FSmartAppLaunchMode := salmNone;
  Fclientid := '';
  Fclientsecret := '';
  FautoUseHooks := false;
  Fcdshooks.clear;
  Fredirectport := 0;
  FtokenEndpoint := '';
  FauthorizeEndpoint := '';
end;

constructor TRegisteredFHIRServer.Create;
begin
  inherited;
  Fcdshooks := TAdvList<TRegisteredCDSHook>.create;
end;

destructor TRegisteredFHIRServer.Destroy;
begin
  Fcdshooks.Free;
  inherited;
end;

function TRegisteredFHIRServer.doesHook(c: string; var hook : TRegisteredCDSHook): boolean;
var
  h : TRegisteredCDSHook;
begin
  result := false;
  for h in cdshooks do
    if (c = h.hook) then
    begin
      hook := h;
      exit(true);
    end;
end;

function TRegisteredFHIRServer.isSSL: boolean;
begin
  result := FileExists(SSLPublicCert);
end;

function TRegisteredFHIRServer.Link: TRegisteredFHIRServer;
begin
  result := TRegisteredFHIRServer(Inherited Link);
end;

procedure TRegisteredFHIRServer.readFromJson(o: TJsonObject);
var
  arr : TJsonArray;
  n, n2 : TJsonNode;
  o1 : TJsonObject;
  c : TRegisteredCDSHook;
begin
  clear;

  name := o.vStr['name'];
  fhirEndpoint := o.vStr['fhir'];
  username := o.vStr['username'];
  password := o.vStr['password'];
  if password.StartsWith('##') then
    password := strDecrypt(password.Substring(2), 35252);
  if StringArrayExistsSensitive(['true', 'outh'], o.vStr['smart']) then
    FSmartAppLaunchMode := salmOAuthClient
  else if StringArrayExistsSensitive(['false', 'none'], o.vStr['smart']) then
    FSmartAppLaunchMode := salmNone
  else if StringArrayExistsSensitive(['backend'], o.vStr['smart']) then
    FSmartAppLaunchMode := salmBackendClient
  else
    FSmartAppLaunchMode := salmNone;
  if o.vStr['format']  = 'xml' then
    format := ffXml
  else if o.vStr['format']  = 'json' then
    format := ffJson
  else
    format := ffUnspecified;

  tokenEndpoint := o.vStr['token'];
  authorizeEndpoint := o.vStr['authorize'];
  clientid := o.vStr['clientid'];
  redirectport := StrToIntDef(o.vStr['port'], 0);
  clientsecret := o.vStr['secret'];
  issuerUrl := o.vStr['issuer'];
  privatekey := o.vStr['key'];
  passphrase := strDecrypt(o.vStr['passphrase'], 42344);

  SSLPublicCert := o.vStr['ssl-cert'];
  SSLPrivateKey := o.vStr['ssl-key'];
  SSLpassphrase := strDecrypt(o.vStr['ssl-passphrase'], 42345);

  autoUseHooks := o.bool['auto-cds-hooks'];
  arr := o.arr['cdshooks'];
  if arr <> nil then
    for n in arr do
    begin
      o1 := n as TJsonObject;
      c := TRegisteredCDSHook.Create;
      cdshooks.Add(c);
      c.hook := o1.vStr['hook'];
      c.name := o1.vStr['name'];
      arr := o.arr['prefetch'];
      if arr <> nil then
        for n2 in arr do
          c.preFetch.Add((n2 as TJsonString).value)
    end;
end;

procedure TRegisteredFHIRServer.writeToJson(o: TJsonObject);
var
  arr, arr2 : TJsonArray;
  c : TRegisteredCDSHook;
  s : String;
begin
  o.clear;
  o.vStr['name'] := name;
  o.vStr['fhir'] := fhirEndpoint;
  o.vStr['username'] := username;
  o.vStr['password'] := '##'+strEncrypt(password, 35252);
  case format of
    ffXml: o.vStr['format'] := 'xml';
    ffJson: o.vStr['format'] := 'json';
    ffTurtle : o.vStr['format'] := 'turtle';
  else
    o.vStr['format'] := 'either';
  end;
  case SmartAppLaunchMode of
    salmNone: o.vStr['smart'] := 'none';
    salmOAuthClient: o.vStr['smart'] := 'oauth';
    salmBackendClient: o.vStr['smart'] := 'backend';
  end;
  o.vStr['token'] := tokenEndpoint;
  o.vStr['authorize'] := authorizeEndpoint;

  o.vStr['clientid'] := clientid;
  o.vStr['port'] := inttostr(redirectport);
  o.vStr['secret'] := clientsecret;
  o.vStr['issuer'] := issuerUrl;
  o.vStr['key'] := privatekey;
  o.vStr['passphrase'] := strEncrypt(passphrase, 42344);

  o.vStr['ssl-cert'] := SSLPublicCert;
  o.vStr['ssl-key'] := SSLPrivateKey;
  o.vStr['ssl-passphrase'] := strEncrypt(SSLpassphrase, 42345);

  o.bool['auto-cds-hooks'] := autoUseHooks;
  if cdshooks.Count > 0 then
  begin
    arr := o.forceArr['cdshooks'];
    for c in cdshooks do
    begin
      o := arr.addObject;
      o.vStr['hook'] := c.hook;
      o.vStr['name'] := c.name;
      if (c.preFetch.Count > 0) then
      begin
        arr2 := o.forceArr['prefetch'];
        for s in c.preFetch do
          arr2.add(s);
      end;
    end;
  end;
end;

{ TRegisteredCDSHook }

constructor TRegisteredCDSHook.Create;
begin
  inherited;
  FpreFetch := TStringList.Create;
end;

destructor TRegisteredCDSHook.Destroy;
begin
  FPrefetch.Free;
  inherited;
end;

function TRegisteredCDSHook.Link: TRegisteredCDSHook;
begin
  result := TRegisteredCDSHook(inherited Link);
end;


function doSmartAppLaunchLogin(server : TRegisteredFHIRServer; authorizeURL, tokenURL : String; idle : TIdleEvent; token : TSmartOnFhirAccessToken) : boolean;
begin

end;


{ TSmartAppLaunchLogin }

procedure TSmartAppLaunchLogin.closeWebServer;
begin
  webserver.Active := false;
  webserver.free;
end;

constructor TSmartAppLaunchLogin.Create;
begin
  inherited;
  FlogoPath := path([ExtractFilePath(paramstr(0)), ChangeFileExt(ExtractFileName(paramstr(0)), '.png')]);
end;

destructor TSmartAppLaunchLogin.Destroy;
begin
  Fserver.Free;
  Ftoken.Free;
  inherited;
end;

function templateSource : String;
begin
  result :=
'<!DOCTYPE HTML>'+#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
'<head>'+#13#10+
'  <title>${title}</title>'+#13#10+
''+#13#10+
'${redirect}'+#13#10+
'  <meta name="viewport" content="width=device-width, initial-scale=1.0"/>'+#13#10+
'  <meta name="author" content="http://hl7.org/fhir"/>'+#13#10+
''+#13#10+
'  <link rel="stylesheet" href="http://hl7.org/fhir/fhir.css"/>'+#13#10+
''+#13#10+
'    <!-- Bootstrap core CSS -->'+#13#10+
'  <link rel="stylesheet" href="http://hl7.org/fhir/dist/css/bootstrap.css"/>'+#13#10+
'  <link rel="stylesheet" href="http://hl7.org/fhir/assets/css/bootstrap-fhir.css"/>'+#13#10+
''+#13#10+
'    <!-- Project extras -->'+#13#10+
'  <link rel="stylesheet" href="http://hl7.org/fhir/assets/css/project.css"/>'+#13#10+
'  <link rel="stylesheet" href="http://hl7.org/fhir/assets/css/pygments-manni.css"/>'+#13#10+
'  <link rel="stylesheet" href="jquery-ui.css"/>'+#13#10+
''+#13#10+
'    <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->'+#13#10+
'    <!-- [if lt IE 9]>'+#13#10+
'  <script src=""http://hl7.org/fhir/assets/js/html5shiv.js"></script>'+#13#10+
'  <script src=""http://hl7.org/fhir/assets/js/respond.min.js"></script>'+#13#10+
'  <![endif] -->'+#13#10+
''+#13#10+
'    <!-- Favicons -->'+#13#10+
'  <link sizes="144x144" rel="apple-touch-icon-precomposed" href="http://hl7.org/fhir/assets/ico/apple-touch-icon-144-precomposed.png"/>'+#13#10+
'  <link sizes="114x114" rel="apple-touch-icon-precomposed" href="http://hl7.org/fhir/assets/ico/apple-touch-icon-114-precomposed.png"/>'+#13#10+
'  <link sizes="72x72" rel="apple-touch-icon-precomposed" href="http://hl7.org/fhir/assets/ico/apple-touch-icon-72-precomposed.png"/>'+#13#10+
'  <link rel="apple-touch-icon-precomposed" href="http://hl7.org/fhir/assets/ico/apple-touch-icon-57-precomposed.png"/>'+#13#10+
'  <link rel="shortcut icon" href="http://hl7.org/fhir/assets/ico/favicon.png"/>'+#13#10+
''+#13#10+
'</head>'+#13#10+
'<body>'+#13#10+
'  <div id="segment-header" class="segment">  <!-- segment-header -->'+#13#10+
'    <div class="container">  <!-- container -->'+#13#10+
'      <a no-external="true" id="logo" href="http://hl7.org/fhir"><img src="http://hl7.org/fhir/assets/images/fhir-logo-www.png" alt="logo fhir"/> </a>'+#13#10+
'      <div id="hl7-status">'+#13#10+
'        <b>${name}</b>'+#13#10+
'      </div>'+#13#10+
''+#13#10+
'      <div id="hl7-nav">'+#13#10+
'         <a no-external="true" id="hl7-logo" href="http://www.hl7.org">'+#13#10+
'          <img src="http://hl7.org/fhir/assets/images/hl7-logo.png" width="42" alt="visit the hl7 website" height="50"/>'+#13#10+
'        </a>'+#13#10+
'      </div>'+#13#10+
'    </div>'+#13#10+
'    <div class="container">  <!-- container -->'+#13#10+
'  </div></div>  <!-- /segment-header -->'+#13#10+
''+#13#10+
''+#13#10+
'  <div id="segment-navbar" class="segment">  <!-- segment-navbar -->'+#13#10+
'    <div id="stripe"> </div>'+#13#10+
'    <div class="container">  <!-- container -->'+#13#10+
''+#13#10+
''+#13#10+
'      <nav class="navbar navbar-inverse">'+#13#10+
'        <div class="container">'+#13#10+
'          <button data-target=".navbar-inverse-collapse" data-toggle="collapse" type="button" class="navbar-toggle">'+#13#10+
'            <span class="icon-bar"> </span>'+#13#10+
'            <span class="icon-bar"> </span>'+#13#10+
'            <span class="icon-bar"> </span>'+#13#10+
'          </button>'+#13#10+
'          <a href="index.html" class="navbar-brand hidden">FHIR</a>'+#13#10+
'        </div>  <!-- /.container -->'+#13#10+
'      </nav>  <!-- /.navbar -->'+#13#10+
''+#13#10+
''+#13#10+
'  <!-- /HEADER CONTENT -->'+#13#10+
'    </div>  <!-- /container -->'+#13#10+
'  </div>  <!-- /segment-navbar -->'+#13#10+
''+#13#10+
''+#13#10+
''+#13#10+
'  <div id="segment-content" class="segment">  <!-- segment-content -->'+#13#10+
'  <div class="container">  <!-- container -->'+#13#10+
'            <div class="row">'+#13#10+
'              <div class="inner-wrapper">'+#13#10+
'<div class="col-12">'+#13#10+
''+#13#10+
'    ${body}'+#13#10+
''+#13#10+
'<p>&nbsp;</p>'+#13#10+
'</div>'+#13#10+
''+#13#10+
'        </div>  <!-- /inner-wrapper -->'+#13#10+
'            </div>  <!-- /row -->'+#13#10+
'        </div>  <!-- /container -->'+#13#10+
''+#13#10+
'    </div>  <!-- /segment-content -->'+#13#10+
''+#13#10+
''+#13#10+
'  <div id="segment-footer" class="segment">  <!-- segment-footer -->'+#13#10+
'    <div class="container">  <!-- container -->'+#13#10+
'      <div class="inner-wrapper">'+#13#10+
'        <p>'+#13#10+
'        &reg;&copy; Health Intersections 2011+. ${name} (v${version})'+#13#10+
'        </p>'+#13#10+
'      </div>  <!-- /inner-wrapper -->'+#13#10+
'    </div>  <!-- /container -->'+#13#10+
'  </div>  <!-- /segment-footer -->'+#13#10+
''+#13#10+
'  <div id="segment-post-footer" class="segment hidden">  <!-- segment-post-footer -->'+#13#10+
'    <div class="container">  <!-- container -->'+#13#10+
'    </div>  <!-- /container -->'+#13#10+
'  </div>  <!-- /segment-post-footer -->'+#13#10+
''+#13#10+
'      <!-- JS and analytics only. -->'+#13#10+
'      <!-- Bootstrap core JavaScript'+#13#10+
'================================================== -->'+#13#10+
'  <!-- Placed at the end of the document so the pages load faster -->'+#13#10+
'<script src="http://hl7.org/fhir/assets/js/jquery.js"> </script>     <!-- note keep space here, otherwise it will be transformed to empty tag -> fails -->'+#13#10+
'<script src="http://hl7.org/fhir/dist/js/bootstrap.min.js"> </script>'+#13#10+
'<script src="http://hl7.org/fhir/assets/js/respond.min.js"> </script>'+#13#10+
''+#13#10+
'<script src="http://hl7.org/fhir/assets/js/fhir.js"> </script>'+#13#10+
''+#13#10+
'  <!-- Analytics Below'+#13#10+
'================================================== -->'+#13#10+
''+#13#10+
''+#13#10+
''+#13#10+
'</body>'+#13#10+
'</html>'+#13#10;
end;

function TSmartAppLaunchLogin.template(title, body, redirect : String) : String;
begin
  result := templateSource.Replace('${title}', title).Replace('${name}', name).Replace('${version}', version).Replace('${body}', body);
  if redirect = '' then
    result := result.Replace('${redirect}', '')
  else
    result := result.Replace('${redirect}', '<meta http-equiv="refresh" content="0; url='+redirect+'" />');
end;

procedure TSmartAppLaunchLogin.DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s : TArray<String>;
  pm : TParseMap;
begin
  if ARequestInfo.Document = '/done' then
  begin
    s := ARequestInfo.RawHTTPCommand.Split([' ']);
    pm := TParseMap.create(s[1].Substring(6));
    try
      FFinalState := pm.GetVar('state');
      if pm.getVar('error') <> '' then
        FErrorMessage := pm.getVar('error')
      else
        FAuthCode := pm.GetVar('code');
      State := stDone;
    finally
      pm.free;
    end;

    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ResponseText := 'OK';
    AResponseInfo.ContentText := Template('Smart App Launch', 'Checking Authorization, please wait...', '/complete');
  end
  else if ARequestInfo.Document = '/complete' then
  begin
    while State <> stComplete do
      sleep(100);
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ResponseText := 'OK';
    AResponseInfo.ContentText := Template('Smart App Launch', 'App Launch Sequence is complete. You can close this window now and go back to the application', '');
  end
  else
  begin
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ResponseText := 'OK';
    AResponseInfo.ContentStream := TFileStream.Create(FLogoPath, fmOpenRead + fmShareDenyWrite);
    AResponseInfo.FreeContentStream := true;
  end;
end;

procedure TSmartAppLaunchLogin.initWebServer;
var
  SHandle: TIdSocketHandle;
begin
  webserver := TIdHTTPServer.Create(nil);
  SHandle := webserver.Bindings.Add;
  SHandle.IP := '127.0.0.1';
  SHandle.Port := server.redirectPort;
  webserver.OnCommandGet := DoCommandGet;
  webserver.Active := true;
end;

function TSmartAppLaunchLogin.login: boolean;
begin
  case server.SmartAppLaunchMode of
    salmNone: raise Exception.Create('Smart App Launch is not configured for this server');
    salmOAuthClient: result := loginOAuthClient;
    salmBackendClient: result := loginBackendClient;
  end;
end;

function TSmartAppLaunchLogin.loginBackendClient: boolean;
var
  jwt : TJWT;
  jwt_header : string;
  sl, s : String;
begin
 // 1. building the JWT
  jwt := TJWT.Create;
  try
    jwt.issuer := server.issuerUrl;
    jwt.subject := server.clientid;
    jwt.expires := now + 1 * DATETIME_MINUTE_ONE;
    jwt.audience := server.tokenEndpoint;
    jwt.id := NewGuidId;
    jwt_header := TJWTUtils.pack(jwt, jwt_hmac_rsa256, nil, server.privatekey, server.passphrase);
  finally
    jwt.Free;
  end;

  // 2. submit to server;
  sl := '';
  for s in scopes do
    sl := sl +' '+s.replace('user/', 'system/');

  token := getSmartOnFhirAuthTokenRequest(server,
    'scope='+sl.Trim+'&'+
    'grant_type=client_credentials'+'&'+
    'client_assertion_type=urn:ietf:params:oauth:client-assertion-type:jwt-bearer'+'&'+
    'client_assertion='+jwt_header);
  result := true;
end;

function TSmartAppLaunchLogin.loginOAuthClient: boolean;
var
  stop : boolean;
begin
  initWebServer;
  try
    openBrowser;
    result := false;
    while state <> stDone do
    begin
      OnIdle(stop);
      if stop then
        exit;
    end;
    if (FInitialState <> FFinalState) then
      raise Exception.create('State parameter mismatch ('+FInitialState+'/'+FFinalState+')');
    if FAuthcode <> '' then
      token := getSmartOnFhirAuthToken(server, FAuthcode);
    state := stComplete;
    sleep(40); // give web server a chance
    if FErrorMessage <> '' then
      result := false
    else
      result := true;
  finally
    closeWebServer;
  end;
end;

procedure TSmartAppLaunchLogin.openBrowser;
var
  url : String;
  sl, s : String;
begin
  FInitialState := NewGuidId;
  sl := 'openid profile';
  for s in scopes do
    sl := sl +' '+s;
  url := buildAuthUrl(server, sl, FInitialState);
  OnOpenURL(url);
end;

procedure TSmartAppLaunchLogin.SetServer(const Value: TRegisteredFHIRServer);
begin
  Fserver.Free;
  Fserver := Value;
end;

procedure TSmartAppLaunchLogin.Settoken(const Value: TSmartOnFhirAccessToken);
begin
  Ftoken.Free;
  Ftoken := Value;
end;

end.
