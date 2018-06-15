unit FHIR.Smart.Login;

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
  IdContext, IdHTTPServer, IdCustomHTTPServer, IdSocketHandle, IdHTTP, IdSSLOpenSSL,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Certs, FHIR.Web.Parsers, FHIR.Support.Stream, FHIR.Support.Json,
  FHIR.Base.Objects, FHIR.Base.Common, FHIR.Client.Base, FHIR.Base.Lang, FHIR.Smart.Utilities;

type
 // called by a client to login via Smart App Launch
  // launches the user's referred browser, and waits for the cycle to complete
  TSmartLoginState = (stStarting, stDone, stComplete, stError);
  TIdleEvent = procedure(out stop : boolean) of object;
  TOpenURLEvent = procedure(url : String) of object;
  TSmartAppLaunchLogin = class (TFslObject)
  private
    FOnIdle: TIdleEvent;
    Ftoken: TClientAccessToken;
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
    procedure Settoken(const Value: TClientAccessToken);
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
    property token : TClientAccessToken read Ftoken write Settoken;
    property name : String read FName write FName;
    property version : String read Fversion write Fversion;
    property logoPath : String read FLogoPath write FLogoPath;
    property ErrorMessage : String read FErrorMessage write FErrorMessage;

    function login : boolean;
  end;

implementation

function doSmartAppLaunchLogin(server : TRegisteredFHIRServer; authorizeURL, tokenURL : String; idle : TIdleEvent; token : TClientAccessToken) : boolean;
begin
  result := false;
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
    salmNone: raise EFHIRException.create('Smart App Launch is not configured for this server');
    salmOAuthClient: result := loginOAuthClient;
    salmBackendClient: result := loginBackendClient;
  else
    result := false;
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
      raise EFHIRException.create('State parameter mismatch ('+FInitialState+'/'+FFinalState+')');
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

procedure TSmartAppLaunchLogin.Settoken(const Value: TClientAccessToken);
begin
  Ftoken.Free;
  Ftoken := Value;
end;

end.
