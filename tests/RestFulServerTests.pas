unit RestFulServerTests;

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
  Windows, Sysutils, Classes, IniFiles,
  DUnitX.TestFramework, IdHttp, IdSSLOpenSSL,
  DateSupport, StringSupport, FileSupport,
  AdvObjects, AdvGenerics, AdvStringBuilders,
  FHIRConstants, FHIRBase, FHIRLang, FHIRTypes, FHIRResources, FHIRSupport, FHIRUtilities,
  FHIRClient, SCIMObjects,
  SmartOnFhirUtilities, SmartOnFhirTestingLogin,
  FHIRServerConstants, ServerUtilities, FHIRServerContext, FHIRStorageService, FHIRUserProvider,
  FHIRRestServer, WebSourceProvider;

Const
  TEST_USER_NAME = 'DunitX-test-user';
  TEST_ANON_USER_NAME = 'DunitX-anon-user';
  JWT = 'eyJhbGciOiJSUzI1NiIsImtpZCI6IjZjZDg1OTdhMWY2ODVjZTA2Y2NhNDJkZTBjMjVmNmY0YjA0OGQ2ODkifQ.'+
     'eyJhenAiOiI5NDAwMDYzMTAxMzguYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiI5NDAwMDYzMTAxMzguYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMTE5MDQ2MjAwNTMzNjQzOTIyODYiLCJlbWFpbCI6ImdyYWhhbWVnQGdtYWlsLmNvbSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLC'+'JhdF9oYXNoIjoiRGltRTh0Zy1XM1U3dFY1ZkxFS2tuUSIsImlzcyI6ImFjY291bnRzLmdvb2dsZS5jb20iLCJpYXQiOjE1MDExMjg2NTgsImV4cCI6MTUwMTEzMjI1OH0.'+
     'DIQA4SvN66r6re8WUxcM9sa5PnVaX1t0Sh34H7ltJE1ElFrwzRAQ3Hz2u_M_gE7a4NaxmbJDFQnVGI3PvZ1TD_bT5zFcFKcq1NWe6kHNFYYn3slxSzGuj02bCdRsTRKu9LXs0YZM1uhbbimOyyajJHckT3tT2dpXYCdfZvBLVu7LUwchBigxE7Q-QnsXwJh28f9P-C1SrA-hLkVf9F7E7zBXgUtkoEyN4rI7FLI6tP7Yc_i'+'ryICAVu2rR9AZCU-hTICbN-uBC7Fuy-kMr-yuu9zdZsaS4LYJxyoAPZNIengjtNcceDVX9m-Evw-Z1iwSFTtDvVyBlC7xbf4JdxaSRw'; // token from google for Grahame Grieve

type
  TTestStorageService = class;

  TTestFHIROperationEngine = class (TFHIROperationEngine)
  private
    FIsReadAllowed : boolean;
    FStorage : TTestStorageService;
  public
    function opAllowed(resource : string; command : TFHIRCommandType) : Boolean;

    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    procedure ExecuteConformanceStmt(request: TFHIRRequest; response : TFHIRResponse); override;
    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse); override;

    property IsReadAllowed : boolean read FIsReadAllowed write FIsReadAllowed;
  end;

  TTestingFHIRUserProvider = class (TFHIRUserProvider)
  public
    Function loadUser(id : String; var key : integer) : TSCIMUser; override;
    function CheckLogin(username, password : String; var key : integer) : boolean; override;
  end;

  TTestOAuthLogin = class (TAdvObject)
  private
    client_id, name, redirect, state, scope, jwt, patient: String;
  public
    function Link : TTestOAuthLogin; overload;
  end;

  TTestStorageService = class (TFHIRStorageService)
  private
    FlastSession : TFHIRSession;
    FLastReadSystem : String;
    FLastReadUser : String;
    FLastUserEvidence : TFHIRUseridEvidence;
    FLastSystemEvidence : TFHIRSystemIdEvidence;

    FOAuths : TAdvMap<TTestOAuthLogin>;

    procedure reset;
  protected
    function GetTotalResourceCount: integer; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    procedure RecordFhirSession(session: TFhirSession); override;
    procedure QueueResource(r: TFhirResource; dateTime: TDateTimeEx); override;
    function createOperationContext(lang : String) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); override;
    procedure recordOAuthLogin(id, client_id, scope, redirect_uri, state : String); override;
    function hasOAuthSession(id : String; status : integer) : boolean; override;
    function fetchOAuthDetails(key, status : integer; var client_id, name, redirect, state, scope : String) : boolean; override;
    procedure updateOAuthSession(id : String; state, key : integer; var client_id : String); override;
    procedure recordOAuthChoice(id : String; scopes, jwt, patient : String); override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;
    procedure RegisterConsentRecord(session: TFhirSession); override;
    function getClientInfo(id : String) : TRegisteredClientInformation; override;
    function getClientName(id : String) : string; override;
    function storeClient(client : TRegisteredClientInformation; sessionKey : integer) : String; override;
  end;


  [TextFixture]
  TRestFulServerTests = Class (TObject)
  private
    FIni : TFHIRServerIniFile;
    FStore : TTestStorageService;
    FContext : TFHIRServerContext;
    FServer : TFhirWebServer;
    FClientXml : TFhirHTTPClient;
    FClientJson : TFhirHTTPClient;
    FClientSSL : TFhirHTTPClient;
    FClientSSLCert : TFhirHTTPClient;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [TestCase] Procedure TestLowLevelXml;
    [TestCase] Procedure TestLowLevelJson;
    [TestCase] Procedure TestCapabilityStatementXml;
    [TestCase] Procedure TestCapabilityStatementJson;
    [TestCase] Procedure TestSSL;
    [TestCase] Procedure TestCapabilityStatementSSL;

    [TestCase] Procedure TestPatientExampleJson;
    [TestCase] Procedure TestPatientExampleXml;
    [TestCase] Procedure TestPatientExampleSSL;
    [TestCase] Procedure TestPatientExampleOWin;
    [TestCase] Procedure TestPatientExampleSmartOnFhir;
    [TestCase] Procedure TestConformanceCertificateNone;
    [TestCase] Procedure TestConformanceCertificate;
    [TestCase] Procedure TestConformanceCertificateNotOk;
    [TestCase] Procedure TestConformanceCertificateSpecified;
    [TestCase] Procedure TestConformanceCertificateWrong;
    [TestCase] Procedure TestConformanceCertificateOptionalNone;
    [TestCase] Procedure TestConformanceCertificateOptionalRight;
    [TestCase] Procedure TestConformanceCertificateOptionalWrong;

    [TestCase] Procedure TestPatientExampleCertificate;
    [TestCase] Procedure TestPatientExampleCertificateJWT;
    [TestCase] Procedure TestPatientExampleCertificateJWTNoCert;
  end;

implementation


{ TTestStorageService }

destructor TTestStorageService.Destroy;
begin
  FOAuths.Free;
  FlastSession.Free;
  inherited;
end;

constructor TTestStorageService.Create;
begin
  inherited;
  FOAuths := TAdvMap<TTestOAuthLogin>.create;
end;

function TTestStorageService.createOperationContext(lang: String): TFHIROperationEngine;
begin
  result := TTestFHIROperationEngine.create(nil, lang);
  TTestFHIROperationEngine(result).FIsReadAllowed := true;
  TTestFHIROperationEngine(result).FStorage := self;
end;

procedure TTestStorageService.Yield(op: TFHIROperationEngine; exception: Exception);
begin
  op.free;
end;

function TTestStorageService.GetTotalResourceCount: integer;
begin
  result := 1;
end;

procedure TTestStorageService.QueueResource(r: TFhirResource; dateTime: TDateTimeEx);
begin
end;

procedure TTestStorageService.RecordFhirSession(session: TFhirSession);
begin
  FlastSession.Free;
  FlastSession := session.Link;
end;

procedure TTestStorageService.recordOAuthChoice(id, scopes, jwt, patient: String);
var
  l : TTestOAuthLogin;
begin
  l := FOAuths[id];
  l.scope := scopes;
  l.jwt := jwt;
  l.patient := patient;
end;

procedure TTestStorageService.recordOAuthLogin(id, client_id, scope, redirect_uri, state: String);
var
  l : TTestOAuthLogin;
begin
  l := TTestOAuthLogin.Create;
  try
    l.client_id := client_id;
    l.redirect := redirect_uri;
    l.state := state;
    l.scope := scope;
    FOAuths.Add(id, l.Link);
  finally
    l.Free;
  end;
end;

function TTestStorageService.hasOAuthSession(id: String; status: integer): boolean;
begin
  result := FOAuths.ContainsKey(id);
end;

procedure TTestStorageService.updateOAuthSession(id: String; state, key: integer; var client_id : String);
var
  l : TTestOAuthLogin;
begin
  l := FOAuths[id];
  if not FOAuths.containsKey(inttostr(key)) then
    FOAuths.Add(inttostr(key), l.Link);
  client_id := FOAuths[inttostr(key)].client_id;
end;

function TTestStorageService.fetchOAuthDetails(key, status: integer; var client_id, name, redirect, state, scope: String): boolean;
var
  l : TTestOAuthLogin;
begin
  result := FOAuths.ContainsKey(inttostr(key));
  if result then
  begin
    l := FOAuths[inttostr(key)];
    client_id := l.client_id;
    name := l.name;
    redirect := l.redirect;
    state := l.state;
    scope := l.scope;
  end;
end;

function TTestStorageService.getClientInfo(id: String): TRegisteredClientInformation;
begin
  result := TRegisteredClientInformation.Create;
  result.name := getClientName(id);
  result.redirects.Add('http://localhost:961/done');
  result.secret := 'this-password-is-never-used';
end;

function TTestStorageService.getClientName(id: String): string;
begin
  result := 'test server';
end;

procedure TTestStorageService.RegisterAuditEvent(session: TFhirSession; ip: String);
begin
end;

procedure TTestStorageService.RegisterConsentRecord(session: TFhirSession);
begin
end;

procedure TTestStorageService.reset;
begin
  FlastSession.Free;
  FlastSession := nil;
  FLastReadSystem := '';
  FLastReadUser := '';
  FLastUserEvidence := userNoInformation;
  FLastSystemEvidence := systemNoInformation;
  FOAuths.Clear;
end;

function TTestStorageService.storeClient(client: TRegisteredClientInformation;
  sessionKey: integer): String;
begin

end;

{ TTestingFHIRUserProvider }

function TTestingFHIRUserProvider.CheckLogin(username, password: String; var key: integer): boolean;
begin
  result := true;
  if (username = 'test') and (password = 'test') then
    key := 1
  else
    result := false;
end;

function TTestingFHIRUserProvider.loadUser(id: String; var key: integer): TSCIMUser;
begin
  result := TSCIMUser.CreateNew;
  if (id = 'ANONYMOUS') then
    result.userName := TEST_ANON_USER_NAME
  else
    result.userName := TEST_USER_NAME;
  result.formattedName := result.userName+'.formatted';
  key := 1;
end;

{ TTestFHIROperationEngine }

procedure TTestFHIROperationEngine.CommitTransaction;
begin
end;

procedure TTestFHIROperationEngine.ExecuteConformanceStmt(request: TFHIRRequest; response: TFHIRResponse);
var
  oConf : TFhirCapabilityStatement;
  c : TFhirContactPoint;
  ct: TFhirContactDetail;
begin
  FStorage.FLastReadUser := request.Session.Username;
  FStorage.FLastReadSystem := request.Session.SystemName;
  FStorage.FLastUserEvidence := request.Session.UserEvidence;
  FStorage.FLastSystemEvidence := request.Session.SystemEvidence;
  response.HTTPCode := 200;
  oConf := TFhirCapabilityStatement.Create;
  response.Resource := oConf;

  oConf.id := 'FhirServer';
  ct := oConf.contactList.Append;
  c := ct.telecomList.Append;
  c.system := ContactPointSystemOther;
  c.value := 'http://healthintersections.com.au/';

  oConf.version := FHIR_GENERATED_VERSION+'-'+SERVER_VERSION; // this conformance statement is versioned by both
  oConf.name := 'Health Intersections FHIR Server Conformance Statement';
  oConf.publisher := 'Health Intersections'; //
  oConf.description := 'Standard Conformance Statement for the open source Reference FHIR Server provided by Health Intersections';
  oConf.status := PublicationStatusActive;
  oConf.experimental := false;
  oConf.date := TDateTimeEx.makeUTC;
  oConf.software := TFhirCapabilityStatementSoftware.Create;
  oConf.software.name := 'Reference Server';
  oConf.software.version := SERVER_VERSION;
  oConf.software.releaseDate := TDateTimeEx.fromXml(SERVER_RELEASE_DATE);
end;

function TTestFHIROperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse; ignoreHeaders: boolean): boolean;
//var
//  resourceKey : integer;
//  field : String;
//  comp : TFHIRParserClass;
//  needsObject : boolean;
var
  filename : String;
  format : TFHIRFormat;
begin
  result := false;
  NotFound(request, response);
  filename := Path(['C:\work\org.hl7.fhir\build\publish', 'patient-'+request.Id+'.xml']);
  if check(response, FIsReadAllowed and (request.ResourceName = 'Patient') and FileExists(filename), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
  begin
    result := true;
    FStorage.FLastReadUser := request.Session.Username;
    FStorage.FLastReadSystem := request.Session.SystemName;
    FStorage.FLastUserEvidence := request.Session.UserEvidence;
    FStorage.FLastSystemEvidence := request.Session.SystemEvidence;

    response.HTTPCode := 200;
    response.Message := 'OK';
    response.Body := '';
    response.versionId := '1';
    response.LastModifiedDate := FileGetModified(filename);
    format := ffXml;
    response.Resource := FileToResource(filename, format);
  end;
end;

procedure TTestFHIROperationEngine.ExecuteSearch(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.bundle := TFhirBundle.Create(BundleTypeSearchset);
end;

function TTestFHIROperationEngine.opAllowed(resource: string; command: TFHIRCommandType): Boolean;
begin
  result := true;
end;

procedure TTestFHIROperationEngine.RollbackTransaction;
begin
end;

procedure TTestFHIROperationEngine.StartTransaction;
begin
end;
{ TRestFulServerTests }

procedure TRestFulServerTests.Setup;
begin
  FIni := TFHIRServerIniFile.Create('C:\work\fhirserver\tests\server-tests.ini');
  FStore := TTestStorageService.create();
  FContext := TFHIRServerContext.Create(FStore.Link);
  FContext.ownername := 'Test-Server';
  FServer := TFhirWebServer.Create(FIni.Link, 'Test-Server', nil, FContext.Link);
//      ctxt.TerminologyServer := FterminologyServer.Link;
  FContext.UserProvider := TTestingFHIRUserProvider.Create;
  FContext.userProvider.OnProcessFile := FServer.ReturnProcessedFile;
  FServer.AuthServer.UserProvider := FContext.userProvider.Link;
  FServer.OWinSecuritySecure := true;
  FServer.ServeMissingCertificate := true;
  FServer.ServeUnknownCertificate := true;
  FServer.Start(true);
  FServer.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('C:\work\fhirserver\web');

  FClientXml := TFhirHTTPClient.Create(FContext.ValidatorContext.Link, FServer.ClientAddress(false), false);
  FClientXml.UseIndy := true;
  FClientJson := TFhirHTTPClient.Create(FContext.ValidatorContext.Link, FServer.ClientAddress(false), true);
  FClientJson.UseIndy := true;
  FClientSSL := TFhirHTTPClient.Create(FContext.ValidatorContext.Link, FServer.ClientAddress(true), false);
  FClientSSL.UseIndy := true;
  FClientSSLCert := TFhirHTTPClient.Create(FContext.ValidatorContext.Link, FServer.ClientAddress(true), true);
  FClientSSLCert.UseIndy := true;
end;

procedure TRestFulServerTests.TearDown;
begin
  FClientXml.Free;
  FClientJson.Free;
  FClientSSL.Free;
  FClientSSLCert.Free;
  FServer.Stop;
  FServer.Free;
  FStore.Free;
  FContext.Free;
  FIni.Free;
end;

procedure TRestFulServerTests.TestCapabilityStatementJson;
var
  cs : TFhirCapabilityStatement;
begin
  cs := FClientJson.conformance(false);
  try
    Assert.IsNotNull(cs);
  finally
    cs.Free;
  end;
end;

procedure TRestFulServerTests.TestCapabilityStatementSSL;
var
  cs : TFhirCapabilityStatement;
begin
  FClientSSL.smartToken := nil;
  cs := FClientSSL.conformance(false);
  try
    Assert.IsNotNull(cs);
  finally
    cs.Free;
  end;
end;


procedure TRestFulServerTests.TestCapabilityStatementXml;
var
  cs : TFhirCapabilityStatement;
begin
  cs := FClientXml.conformance(false);
  try
    Assert.IsNotNull(cs);
  finally
    cs.Free;
  end;
end;

procedure TRestFulServerTests.TestLowLevelXml;
var
  http : TIdHTTP;
  resp : TBytesStream;
begin
  http := TIdHTTP.Create(nil);
  Try
//    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
//    Try
//      http.IOHandler := ssl;
//      ssl.SSLOptions.Mode := sslmClient;
//      ssl.SSLOptions.Method := sslvTLSv1_2;
//    finally
//      ssl.free;
//    end;
    http.Request.Accept := 'application/fhir+xml';
    resp := TBytesStream.create;
    try
      http.Get(FServer.ClientAddress(false)+'/metadata', resp);
      resp.position := 0;
      Assert.isTrue(http.ResponseCode = 200, 'response code <> 200');
      Assert.isTrue(http.Response.ContentType = 'application/fhir+xml', 'response content type <> application/fhir+xml');;
    finally
      resp.free;
    end;
  finally
    http.free;
  end;
end;

procedure TRestFulServerTests.TestConformanceCertificate;
var
  cs : TFHIRCapabilityStatement;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := false;
  FServer.ServeUnknownCertificate := true;
  FServer.CertificateIdList.clear;
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := 'C:\work\fhirserver\tests\client.test.fhir.org.cert';
  FClientSSL.certPWord := 'test';

  cs := FClientSSL.conformance(false);
  try
    Assert.IsNotNull(cs);
  finally
    cs.Free;
  end;

  Assert.IsTrue(FStore.FLastReadSystem  = 'client.test.fhir.org', 'SystemName should be "client.test.fhir.org" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = TEST_ANON_USER_NAME, 'Username should be "'+TEST_ANON_USER_NAME+'" not '+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userAnonymous, 'UserEvidence should be "'+CODES_UserIdEvidence[userAnonymous]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemFromCertificate, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemFromCertificate]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestConformanceCertificateNotOk;
var
  cs : TFHIRCapabilityStatement;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := false;
  FServer.ServeUnknownCertificate := false;
  FServer.CertificateIdList.clear;
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := 'C:\work\fhirserver\tests\client.test.fhir.org.cert';
  FClientSSL.certPWord := 'test';

  try
    cs := FClientSSL.conformance(false);
    try
      Assert.IsFalse(true);
    finally
      cs.Free;
    end;
  except
    on e:exception do
      Assert.IsTrue(e.message.contains('SSL')); // all good - access should be refused
  end;
  Assert.IsTrue(FStore.FLastReadSystem  = '', 'SystemName should be "" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = '', 'Username should be "", not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userNoInformation, 'UserEvidence should be "'+CODES_UserIdEvidence[userNoInformation]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemNoInformation, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemNoInformation]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestConformanceCertificateNone;
var
  cs : TFHIRCapabilityStatement;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := false;
  FServer.CertificateIdList.clear;
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := '';
  FClientSSL.certPWord := '';

  try
    cs := FClientSSL.conformance(false);
    try
      Assert.IsFalse(true);
    finally
      cs.Free;
    end;
  except
    on e:exception do
      Assert.IsTrue(e.message.contains('handshake')); // all good - access should be refused
  end;

  Assert.IsTrue(FStore.FLastReadSystem  = '', 'SystemName should be "" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = '', 'Username should be "", not '+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userNoInformation, 'UserEvidence should be "'+CODES_UserIdEvidence[userNoInformation]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemNoInformation, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemNoInformation]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestConformanceCertificateOptionalNone;
var
  cs : TFHIRCapabilityStatement;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := true;
  FServer.CertificateIdList.add('B7:90:70:D1:D8:D1:1B:9D:03:86:F4:5B:B5:69:E3:C4');
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := '';
  FClientSSL.certPWord := '';

  cs := FClientSSL.conformance(false);
  try
    Assert.IsNotNull(cs);
  finally
    cs.Free;
  end;

  Assert.IsTrue(FStore.FLastReadSystem  = 'Unknown', 'SystemName should be "Unknown" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = TEST_ANON_USER_NAME, 'Username should be "'+TEST_ANON_USER_NAME+'" not '+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userAnonymous, 'UserEvidence should be "'+CODES_UserIdEvidence[userAnonymous]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemUnknown, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemUnknown]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestConformanceCertificateOptionalRight;
var
  cs : TFHIRCapabilityStatement;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := true;
  FServer.CertificateIdList.add('B7:90:70:D1:D8:D1:1B:9D:03:86:F4:5B:B5:69:E3:C4');
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := 'C:\work\fhirserver\tests\client.test.fhir.org.cert';
  FClientSSL.certPWord := 'test';

  cs := FClientSSL.conformance(false);
  try
    Assert.IsNotNull(cs);
  finally
    cs.Free;
  end;
  Assert.IsTrue(FStore.FLastReadSystem  = 'client.test.fhir.org', 'SystemName should be "client.test.fhir.org" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = TEST_ANON_USER_NAME, 'Username should be "'+TEST_ANON_USER_NAME+'" not '+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userAnonymous, 'UserEvidence should be "'+CODES_UserIdEvidence[userAnonymous]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemFromCertificate, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemFromCertificate]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestConformanceCertificateOptionalWrong;
var
  cs : TFHIRCapabilityStatement;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := true;
  FServer.ServeUnknownCertificate := false;
  FServer.CertificateIdList.add('B7:90:70:D1:D8:D1:1B:9D:03:86:F4:5B:B5:69:E3:C4');
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := 'C:\work\fhirserver\tests\local.fhir.org.cert';
  FClientSSL.certPWord := 'test';

  try
    cs := FClientSSL.conformance(false);
    try
      Assert.IsFalse(true);
    finally
      cs.Free;
    end;
  except
    on e:exception do
      Assert.IsTrue(e.message.contains('connecting')); // all good - access should be refused
  end;
  Assert.IsTrue(FStore.FLastReadSystem  = '', 'SystemName should be "" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = '', 'Username should be "", not '+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userNoInformation, 'UserEvidence should be "'+CODES_UserIdEvidence[userNoInformation]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemNoInformation, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemNoInformation]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestConformanceCertificateSpecified;
var
  cs : TFHIRCapabilityStatement;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := false;
  FServer.CertificateIdList.add('B7:90:70:D1:D8:D1:1B:9D:03:86:F4:5B:B5:69:E3:C4');
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := 'C:\work\fhirserver\tests\client.test.fhir.org.cert';
  FClientSSL.certPWord := 'test';

  cs := FClientSSL.conformance(false);
  try
    Assert.IsNotNull(cs);
  finally
    cs.Free;
  end;

  Assert.IsTrue(FStore.FLastReadSystem  = 'client.test.fhir.org', 'SystemName should be "client.test.fhir.org" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = TEST_ANON_USER_NAME, 'Username should be "'+TEST_ANON_USER_NAME+'" not '+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userAnonymous, 'UserEvidence should be "'+CODES_UserIdEvidence[userAnonymous]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemFromCertificate, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemFromCertificate]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestConformanceCertificateWrong;
var
  cs : TFHIRCapabilityStatement;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := false;
  FServer.ServeUnknownCertificate := false;
  FServer.CertificateIdList.add('B7:90:70:D1:D8:D1:1B:9D:03:86:F4:5B:B5:69:E3:C4');
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := 'C:\work\fhirserver\tests\local.fhir.org.cert';
  FClientSSL.certPWord := 'test';

  try
    cs := FClientSSL.conformance(false);
    try
      Assert.IsFalse(true);
    finally
      cs.Free;
    end;
  except
    on e:exception do
      Assert.IsTrue(e.message.contains('connecting')); // all good - access should be refused
  end;
  Assert.IsTrue(FStore.FLastReadSystem  = '', 'SystemName should be "" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = '', 'Username should be "", not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userNoInformation, 'UserEvidence should be "'+CODES_UserIdEvidence[userNoInformation]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemNoInformation, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemNoInformation]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestPatientExampleCertificate;
var
  res : TFhirResource;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := false;
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := 'C:\work\fhirserver\tests\client.test.fhir.org.cert';
  FClientSSL.certPWord := 'test';

  try
    res := FClientSSL.readResource(frtPatient, 'example');
    try
      Assert.IsFalse(true, 'should not be abe to read without authorization, but got a '+res.className);
    finally
      res.free;
    end;
  except
    on e:EFHIRClientException do
      Assert.isTrue(e.issue.hasIssueList and (e.issue.issueList[0].code = IssueTypeLogin), 'Isseue type is wrong');
    on e:exception do
      Assert.isTrue(false, e.ClassName+'; '+ e.Message);
  end;
  Assert.IsTrue(FStore.FLastReadSystem  = '', 'SystemName should be "" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = '', 'Username should be "", not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userNoInformation, 'UserEvidence should be "'+CODES_UserIdEvidence[userNoInformation]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemNoInformation, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemNoInformation]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestPatientExampleCertificateJWT;
var
  res : TFhirResource;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := true;
  FServer.Start(true);
  FClientSSL.certFile := 'C:\work\fhirserver\tests\client.test.fhir.org.cert';
  FClientSSL.certPWord := 'test';
  FClientSSL.smartToken := TSmartOnFhirAccessToken.create;
  FClientSSL.smartToken.accessToken := JWT;
  FClientSSL.smartToken.expires := now + 20 * DATETIME_MINUTE_ONE;

  res := FClientSSL.readResource(frtPatient, 'example');
  try
    Assert.IsNotNull(res, 'no resource returned');
    Assert.isTrue(res is TFHIRPatient, 'Resource should be Patient, not '+res.className);
    Assert.IsTrue(res.id = 'example');
  finally
    res.free;
  end;

  Assert.IsTrue(FStore.FLastReadSystem  = 'client.test.fhir.org', 'SystemName should be "client.test.fhir.org" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = 'grahameg@gmail.com', 'Username should be "grahameg@gmail.com", not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userBearerJWT, 'UserEvidence should be "'+CODES_UserIdEvidence[userBearerJWT]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemFromCertificate, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemFromCertificate]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestPatientExampleCertificateJWTNoCert;
var
  res : TFhirResource;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := true;
  FServer.Start(true);
  FClientSSL.smartToken := TSmartOnFhirAccessToken.create;
  FClientSSL.smartToken.accessToken := JWT;
  FClientSSL.smartToken.expires := now + 20 * DATETIME_MINUTE_ONE;

  res := FClientSSL.readResource(frtPatient, 'example');
  try
    Assert.IsNotNull(res, 'no resource returned');
    Assert.isTrue(res is TFHIRPatient, 'Resource should be Patient, not '+res.className);
    Assert.IsTrue(res.id = 'example');
  finally
    res.free;
  end;

  Assert.IsTrue(FStore.FLastReadSystem  = 'Unknown', 'SystemName should be "Unknown" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = 'grahameg@gmail.com', 'Username should be "grahameg@gmail.com", not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userBearerJWT, 'UserEvidence should be "'+CODES_UserIdEvidence[userBearerJWT]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemUnknown, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemUnknown]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestPatientExampleJson;
var
  res : TFhirResource;
begin
  FStore.reset;
  res := FClientJson.readResource(frtPatient, 'example');
  try
    Assert.IsNotNull(res, 'no resource returned');
    Assert.isTrue(res is TFHIRPatient, 'Resource should be Patient, not '+res.className);
    Assert.IsTrue(res.id = 'example');
  finally
    res.Free;
  end;

  Assert.IsTrue(FStore.FLastReadSystem  = 'Unknown', 'SystemName should be "Unknown" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser = TEST_ANON_USER_NAME, 'Username should be "'+TEST_ANON_USER_NAME+'" not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userAnonymous, 'UserEvidence should be "'+CODES_UserIdEvidence[userAnonymous]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemUnknown, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemUnknown]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestPatientExampleOWin;
var
  res : TFhirResource;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := true;
  FServer.CertificateIdList.clear;
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := '';
  FClientSSL.certPWord := '';

  FClientSSL.authoriseByOWin(FClientSSL.url+'/'+OWIN_TOKEN_PATH, 'test', 'test');
  res := FClientSSL.readResource(frtPatient, 'example');
  try
    Assert.IsNotNull(res, 'no resource returned');
    Assert.isTrue(res is TFHIRPatient, 'Resource should be Patient, not '+res.className);
    Assert.IsTrue(res.id = 'example');
  finally
    res.Free;
  end;
  Assert.IsTrue(FStore.FLastReadSystem  = 'test', 'SystemName should be "test" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser = TEST_ANON_USER_NAME, 'Username should be "'+TEST_ANON_USER_NAME+'" not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userAnonymous, 'UserEvidence should be "'+CODES_UserIdEvidence[userAnonymous]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemFromOWin, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemFromOWin]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestPatientExampleSmartOnFhir;
var
  res : TFhirResource;
  tester : TSmartOnFhirTestingLogin;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := true;
  FServer.CertificateIdList.clear;
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := '';
  FClientSSL.certPWord := '';

  tester := TSmartOnFhirTestingLogin.create;
  try
    tester.server.fhirEndPoint := FServer.AuthServer.EndPoint;
    tester.server.authorizeEndpoint := 'https://'+FIni.readString(voMaybeVersioned, 'web', 'host', '')+':'+FIni.readString(voMaybeVersioned, 'web', 'https', '')+FIni.readString(voMaybeVersioned, 'web', 'auth-path', '')+'/auth';
    tester.server.tokenEndPoint := 'https://'+FIni.readString(voMaybeVersioned, 'web', 'host', '')+':'+FIni.readString(voMaybeVersioned, 'web', 'https', '')+FIni.readString(voMaybeVersioned, 'web', 'auth-path', '')+'/token';
    tester.scopes := 'openid profile user/*.*';
    tester.server.clientid := 'web';
    tester.server.redirectport := 961;
    tester.server.clientsecret := 'this-password-is-never-used';
    tester.username := 'test';
    tester.password := 'test';
    tester.login;
    FClientSSL.SmartToken := tester.token.link;
  finally
    tester.Free;
  end;
  res := FClientSSL.readResource(frtPatient, 'example');
  try
    Assert.IsNotNull(res, 'no resource returned');
    Assert.isTrue(res is TFHIRPatient, 'Resource should be Patient, not '+res.className);
    Assert.IsTrue(res.id = 'example');
  finally
    res.Free;
  end;

  Assert.IsTrue(FStore.FLastReadSystem  = 'web', 'SystemName should be "web" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = TEST_USER_NAME, 'Username should be "'+TEST_USER_NAME+'", not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userLogin, 'UserEvidence should be "'+CODES_UserIdEvidence[userLogin]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemFromOAuth, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemFromOAuth]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestPatientExampleSSL;
var
  res : TFhirResource;
begin
  FStore.reset;
  FServer.Stop;
  FServer.ServeMissingCertificate := true;
  FServer.CertificateIdList.clear;
  FServer.Start(true);
  FClientSSL.smartToken := nil;
  FClientSSL.certFile := '';
  FClientSSL.certPWord := '';

  try
    res := FClientSSL.readResource(frtPatient, 'example');
    try
      Assert.IsFalse(true, 'should not be abe to read without authorization, but got a '+res.className);
    finally
      res.free;
    end;
  except
    on e:EFHIRClientException do
      Assert.isTrue(e.issue.hasIssueList and (e.issue.issueList[0].code = IssueTypeLogin), 'Isseue type is wrong');
    on e:exception do
      Assert.isTrue(false, e.ClassName+'; '+ e.Message);
  end;
  Assert.IsTrue(FStore.FLastReadSystem  = '', 'SystemName should be "" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser  = '', 'Username should be "", not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userNoInformation, 'UserEvidence should be "'+CODES_UserIdEvidence[userNoInformation]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemNoInformation, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemNoInformation]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestPatientExampleXml;
var
  res : TFhirResource;
begin
  FStore.reset;
  res := FClientXml.readResource(frtPatient, 'example');
  try
    Assert.IsNotNull(res, 'no resource returned');
    Assert.isTrue(res is TFHIRPatient, 'Resource should be Patient, not '+res.className);
    Assert.IsTrue(res.id = 'example');
  finally
    res.Free;
  end;

  Assert.IsTrue(FStore.FLastReadSystem  = 'Unknown', 'SystemName should be "Unknown" not "'+FStore.FLastReadSystem+'"');
  Assert.IsTrue(FStore.FLastReadUser = TEST_ANON_USER_NAME, 'Username should be "'+TEST_ANON_USER_NAME+'" not "'+FStore.FLastReadUser+'"');
  Assert.IsTrue(FStore.FLastUserEvidence  = userAnonymous, 'UserEvidence should be "'+CODES_UserIdEvidence[userAnonymous]+'" not "'+CODES_UserIdEvidence[FStore.FLastUserEvidence]+'"');
  Assert.IsTrue(FStore.FLastSystemEvidence = systemUnknown, 'SystemEvidence should be "'+CODES_SystemIdEvidence[systemUnknown]+'" not "'+CODES_SystemIdEvidence[FStore.FLastSystemEvidence]+'"');
end;

procedure TRestFulServerTests.TestSSL;
var
  http : TIdHTTP;
  resp : TBytesStream;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
begin
  http := TIdHTTP.Create(nil);
  Try
    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    Try
      http.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      ssl.SSLOptions.Method := sslvTLSv1_2;
      http.Request.Accept := 'application/fhir+xml';
      resp := TBytesStream.create;
      try
        http.Get(FServer.ClientAddress(true)+'/metadata', resp);
        resp.position := 0;
        Assert.isTrue(http.ResponseCode = 200, 'response code <> 200');
        Assert.isTrue(http.Response.ContentType = 'application/fhir+xml', 'response content type <> application/fhir+xml');;
      finally
        resp.free;
      end;
    finally
      ssl.free;
    end;
  finally
    http.free;
  end;

end;

procedure TRestFulServerTests.TestLowLevelJson;
var
  http : TIdHTTP;
  resp : TBytesStream;
begin
  http := TIdHTTP.Create(nil);
  Try
//    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
//    Try
//      http.IOHandler := ssl;
//      ssl.SSLOptions.Mode := sslmClient;
//      ssl.SSLOptions.Method := sslvTLSv1_2;
//    finally
//      ssl.free;
//    end;
    http.Request.Accept := 'application/fhir+json';
    resp := TBytesStream.create;
    try
      http.Get(FServer.ClientAddress(false)+'/metadata', resp);
      resp.position := 0;
      Assert.isTrue(http.ResponseCode = 200, 'response code <> 200');
      Assert.isTrue(http.Response.ContentType = 'application/fhir+json', 'response content type <> application/fhir+json');;
    finally
      resp.free;
    end;
  finally
    http.free;
  end;
end;

{ TTestOAuthLogin }

function TTestOAuthLogin.Link: TTestOAuthLogin;
begin
  result := TTestOAuthLogin(inherited Link);
end;

initialization
  TDUnitX.RegisterTestFixture(TRestFulServerTests);
end.
