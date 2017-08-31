unit SmartOnFhirTestingLogin;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  IdHttp, IdSSLOpenSSL,
  StringSupport, GuidSupport, TextUtilities, BytesSupport,
  AdvObjects,
  SmartOnFhirUtilities;

type
  TSmartOnFhirTestingLogin = class (TAdvObject)
  private
    FScopes: String;
    FToken: TSmartOnFhirAccessToken;
    FServer: TRegisteredFHIRServer;
    FInitialState : String;
    FClient : TIdHttp;
    FSsl : TIdSSLIOHandlerSocketOpenSSL;
    FPassword: String;
    FUserName: String;
    function extractHost(url : String) : String;
    function fetch(url : String) : String;
    function fetchPost(url : String; body : string) : String;
    procedure SetServer(const Value: TRegisteredFHIRServer);
    function makeInitialRequest(var url : String) : String;
    function makeFollowUpRequest(id, url: String) : String;
    function makeFinalRequest(url: String) : String;
  public
    constructor Create; override;
    destructor Destroy; override;

    property server : TRegisteredFHIRServer read FServer write SetServer;
    property scopes : String read FScopes write FScopes;
    property token : TSmartOnFhirAccessToken read FToken;
    property username : String read FUserName write FUserName;
    property password : String read FPassword write FPassword;

    procedure login;

  end;
implementation

{ TSmartOnFhirTestingLogin }

constructor TSmartOnFhirTestingLogin.Create;
begin
  inherited;
  FServer := TRegisteredFHIRServer.create;
  FClient := TIdHttp.Create(nil);
  FClient.AllowCookies := true;
  FClient.HandleRedirects := true;
  FSsl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
  FClient.IOHandler := FSsl;
  FSsl.SSLOptions.Mode := sslmClient;
  FSsl.SSLOptions.Method := sslvTLSv1_2;
end;

destructor TSmartOnFhirTestingLogin.Destroy;
begin
  FSsl.Free;
  FClient.Free;
  FServer.Free;
  FToken.Free;
  inherited;
end;

function TSmartOnFhirTestingLogin.extractHost(url: String): String;
var
  i : integer;
begin
  i := 10;
  while (i < url.Length) and (url[i] <> '/') do
    inc(i);
  result := url.Substring(0, i-1);
end;

function TSmartOnFhirTestingLogin.fetch(url: String): String;
var
  resp : TMemoryStream;
begin
  FClient.Request.Accept := 'text/html';
  resp := TMemoryStream.create;
  try
    FClient.Get(url, resp);
    resp.position := 0;
    result := StreamToString(resp, TEncoding.UTF8);
  finally
    resp.free;
  end;
end;

function TSmartOnFhirTestingLogin.fetchPost(url : String; body : string) : String;
var
  post : TBytesStream;
  resp : TMemoryStream;
begin
  FClient.Request.Accept := 'text/html';
  FClient.Request.ContentType := 'application/x-www-form-urlencoded';
  post := TBytesStream.create(TEncoding.ASCII.getBytes(body));
  try
    resp := TMemoryStream.create;
    try
      FClient.Post(url, post, resp);
      resp.position := 0;
      result := StreamToString(resp, TEncoding.UTF8);
    finally
      resp.free;
    end;
  finally
    post.Free;
  end;
end;

procedure TSmartOnFhirTestingLogin.login;
var
  id, url : String;
begin
  FInitialState := NewGuidId;
  id := makeInitialRequest(url);
  url := makeFollowupRequest(id, url);
  url := makeFinalRequest(url);
  url := url.Substring(url.IndexOf('code=')+5);
  if url.Contains('&') then
    url := url.Substring(0, url.IndexOf('&'));

  FToken := getSmartOnFhirAuthToken(server, url);
end;

function TSmartOnFhirTestingLogin.makeFinalRequest(url: String): String;
var
  page : String;
begin
  FClient.HandleRedirects := false;
  FClient.HTTPOptions := FClient.HTTPOptions + [hoNoProtocolErrorException];
  page := fetchPost(extractHost(server.authorizeEndpoint)+url,
    'form=true&'+
    'readClinical=1&writeClinical=1&readData=1&writeData=1&readMeds=1&writeMeds=1&readSchedule=1&writeSchedule=1&readAudit=1&writeAudit=1&readDocuments=1&writeDocuments=1&readFinancial=1&writeFinancial=1&readOther=1&writeOther=1');
  result := FClient.Response.Location;
end;

function TSmartOnFhirTestingLogin.makeFollowUpRequest(id, url: String): String;
var
  page, l, r : String;
begin
  page := fetchPost(extractHost(server.authorizeEndpoint)+url,
    'id='+id+'&'+
    'username='+username+'&'+
    'password='+password);
  StringSplit(page, '<form method="POST" action="', l, r);
  StringSplit(r, '"', result, r);
end;

function TSmartOnFhirTestingLogin.makeInitialRequest(var url : String) : String;
var
  page, l, r : String;
begin
  url := buildAuthUrl(server, scopes, FInitialState);
  page := fetch(url);
  StringSplit(page, '<form method="POST" action="', l, r);
  StringSplit(r, '"', url, r);
  StringSplit(page, '<input type="hidden" name="id" value="', l, r);
  StringSplit(r, '"', result, r);
end;

procedure TSmartOnFhirTestingLogin.SetServer(const Value: TRegisteredFHIRServer);
begin
  FServer.Free;
  FServer := Value;
end;

end.

