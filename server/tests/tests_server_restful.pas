unit FHIR.Tests.RestfulServer;

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
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Windows, Sysutils, Classes, IniFiles,
  {$IFDEF FPC} FPCUnit, TestRegistry, {$ELSE} DUnitX.TestFramework, {$ENDIF}
  IdHttp, IdSSLOpenSSL,
  fsl_base, fsl_utilities, fsl_tests, fsl_json, fsl_http,
  fhir_factory, fhir_common,
  ftx_ucum_services,
  fhir4_constants, fhir4_context, fhir_objects,  fhir_utilities, fhir4_types, fhir4_resources, fhir4_resources_base, fhir_pathengine,
  fhir4_utilities, fhir4_validator, fhir4_indexinfo, fhir4_javascript,
  fhir4_factory, fhir_indexing, fhir_javascript, fhir4_pathengine,
  fhir_client, FHIR.Version.Client, fsl_scim,
  fhir_oauth, FHIR.Tests.SmartLogin,
  FHIR.Tx.Server,
  FHIR.Server.Constants, FHIR.Server.Utilities, FHIR.Server.Context, FHIR.Server.Storage, FHIR.Server.UserMgr, FHIR.Server.Indexing, FHIR.Server.Session, FHIR.Server.Ini,
  FHIR.Server.Web, FHIR.Server.WebSource, FHIR.Server.Factory, FHIR.Server.Subscriptions, FHIR.Server.Javascript, FHIR.Server.JWT, FHIR.Server.Telnet,
  FHIR.Server.ValidatorR4, FHIR.Server.IndexingR4, FHIR.Server.SubscriptionsR4;

{$IFNDEF FPC}
Const
  TEST_USER_NAME = 'DunitX-test-user';
  TEST_ANON_USER_NAME = 'DunitX-anon-user';
  JWT = 'eyJhbGciOiJSUzI1NiIsImtpZCI6IjZjZDg1OTdhMWY2ODVjZTA2Y2NhNDJkZTBjMjVmNmY0YjA0OGQ2ODkifQ.'+
     'eyJhenAiOiI5NDAwMDYzMTAxMzguYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiI5NDAwMDYzMTAxMzguYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMTE5MDQ2MjAwNTMzNjQzOTIyODYiLCJlbWFpbCI6ImdyYWhhbWVnQGdtYWlsLmNvbSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLC'+'JhdF9oYXNoIjoiRGltRTh0Zy1XM1U3dFY1ZkxFS2tuUSIsImlzcyI6ImFjY291bnRzLmdvb2dsZS5jb20iLCJpYXQiOjE1MDExMjg2NTgsImV4cCI6MTUwMTEzMjI1OH0.'+
     'DIQA4SvN66r6re8WUxcM9sa5PnVaX1t0Sh34H7ltJE1ElFrwzRAQ3Hz2u_M_gE7a4NaxmbJDFQnVGI3PvZ1TD_bT5zFcFKcq1NWe6kHNFYYn3slxSzGuj02bCdRsTRKu9LXs0YZM1uhbbimOyyajJHckT3tT2dpXYCdfZvBLVu7LUwchBigxE7Q-QnsXwJh28f9P-C1SrA-hLkVf9F7E7zBXgUtkoEyN4rI7FLI6tP7Yc_i'+'ryICAVu2rR9AZCU-hTICbN-uBC7Fuy-kMr-yuu9zdZsaS4LYJxyoAPZNIengjtNcceDVX9m-Evw-Z1iwSFTtDvVyBlC7xbf4JdxaSRw'; // token from google for Grahame Grieve

Type
  TTestServerFactory = class (TFHIRServerFactory)
  private
    FVersion : TFHIRVersion;
  public
    constructor Create(version : TFHIRVersion);

    function makeIndexes : TFHIRIndexBuilder; override;
    function makeValidator: TFHIRValidatorV; override;
    function makeIndexer : TFHIRIndexManager; override;
    function makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager; override;
    function makeEngine(validatorContext : TFHIRWorkerContextWithFactory; ucum : TUcumServiceImplementation) : TFHIRPathEngineV; override;

    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); override;
  end;

  TTestStorageService = class;

  TTestFHIROperationEngine = class (TFHIROperationEngine)
  private
    FIsReadAllowed : boolean;
    FStorage : TTestStorageService;
  protected
    procedure processGraphQL(graphql: String; request : TFHIRRequest; response : TFHIRResponse); override;
  public
    function opAllowed(resource : string; command : TFHIRCommandType) : Boolean; override;

    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    procedure ExecuteMetadata(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); override;
    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse); override;
    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; override;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResourceV; override;
    function getResourceByUrl(aType : string; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResourceV; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    function patientIds(request : TFHIRRequest; res : TFHIRResourceV) : TArray<String>; override;

    property IsReadAllowed : boolean read FIsReadAllowed write FIsReadAllowed;
  end;

  TTestingFHIRUserProvider = class (TFHIRUserProvider)
  public
    Function loadUser(id : String; var key : integer) : TSCIMUser; override;
    function CheckLogin(username, password : String; var key : integer) : boolean; override;
    Function loadUser(key : integer) : TSCIMUser; overload; override;
    function CheckId(id : String; var username, hash : String) : boolean; override;
    function loadOrCreateUser(id, name, email : String; var key : integer) : TSCIMUser; override;
  end;

  TTestOAuthLogin = class (TFslObject)
  private
    client_id, name, redirect, state, scope, jwt, patient, launch: String;
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
    FContext : TFHIRServerContext;
    FServer : TFhirWebServer;
    FEndpoint : TFhirWebServerEndpoint;

    FOAuths : TFslMap<TTestOAuthLogin>;

    procedure reset;
    procedure SetContext(const Value: TFHIRServerContext);
  protected
    function GetTotalResourceCount: integer; override;
  public
    constructor Create(factory : TFHIRFactory); override;
    destructor Destroy; override;
    procedure RecordFhirSession(session: TFhirSession); override;
    procedure QueueResource(session : TFHIRSession; r: TFhirResourceV; dateTime: TFslDateTime); override;
    function createOperationContext(const lang : THTTPLanguages) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); override;
    procedure recordOAuthLogin(id, client_id, scope, redirect_uri, state, launch : String); override;
    function hasOAuthSession(id : String; status : integer) : boolean; override;
    function fetchOAuthDetails(key, status : integer; var client_id, name, redirect, state, scope, launch : String) : boolean; overload; override;
    function fetchOAuthDetails(id : String; var client_id, redirect, state, scope, launch : String) : boolean; overload; override;
    procedure updateOAuthSession(id : String; state, key : integer; var client_id : String); override;
    procedure recordOAuthChoice(id : String; scopes, jwt, patient : String); override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;
    procedure RegisterConsentRecord(session: TFhirSession); override;
    function getClientInfo(id : String) : TRegisteredClientInformation; override;
    function getClientName(id : String) : string; override;
    function storeClient(client : TRegisteredClientInformation; sessionKey : integer) : String; override;

    procedure FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList); override;
    procedure Sweep; override;
    procedure CloseFhirSession(key: integer); override;
    procedure QueueResource(session : TFHIRSession; r: TFhirResourceV); overload; override;
    function RetrieveSession(key : integer; var UserKey, Provider : integer; var Id, Name, Email : String) : boolean; override;
    function ProfilesAsOptionList : String; override;
    procedure ProcessSubscriptions; override;
    procedure ProcessEmails; override;
    procedure ProcessObservations; override;
    procedure RunValidation; override;
    function FetchResource(key : integer) : TFHIRResourceV; override;
    procedure fetchClients(list : TFslList<TRegisteredClientInformation>); override;
    property ServerContext : TFHIRServerContext read FContext write SetContext;

    function loadPackages : TFslMap<TLoadedPackageInformation>; override;
    function fetchLoadedPackage(id : String) : TBytes; override;
    procedure recordPackageLoaded(id, ver : String; count : integer; blob : TBytes); override;

    Procedure SetUpRecording(Session : TFHIRSession); override;
    procedure RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception); override;
    procedure FinishRecording(); override;
  end;


  [TextFixture]
  TRestFulServerTests = Class (TObject)
  private
    FIni : TFHIRServerIniFile;
    FGlobals : TFHIRServerSettings;
    FStore : TTestStorageService;
    FContext : TFHIRServerContext;
    FServer : TFhirWebServer;
    FClientXml : TFhirClient;
    FClientJson : TFhirClient;
    FClientSSL : TFhirClient;
    FClientSSLCert : TFhirClient;
    FEndpoint : TFhirWebServerEndpoint;
    procedure registerJs(snder : TObject; js : TJsHost);
    function SslGet(url: String): TFHIRResource;
    function getJson(url: String): TJsonObject;
  public
    [Setup] Procedure SetUp;
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

    [TestCase] procedure TestSmartStandaloneLaunchCS;
    [TestCase] procedure TestSmartStandaloneLaunchWK;
    [TestCase] procedure TestSmartStandaloneLaunchNU;
    [TestCase] procedure TestSmartStandaloneLaunchError;
    [TestCase] procedure TestSmartStandaloneLaunchBadRedirect;
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

{$ENDIF}

implementation

{$IFNDEF FPC}
{ TTestStorageService }

destructor TTestStorageService.Destroy;
begin
  FOAuths.Free;
  FlastSession.Free;
  inherited;
end;

procedure TTestStorageService.CloseFhirSession(key: integer);
begin
  raise EFslException.Create('Not Implemented');
end;

constructor TTestStorageService.Create(factory : TFHIRFactory);
begin
  inherited;
  FOAuths := TFslMap<TTestOAuthLogin>.create('oauths');
end;

function TTestStorageService.createOperationContext(const lang : THTTPLanguages): TFHIROperationEngine;
begin
  result := TTestFHIROperationEngine.create(self, FContext.Link, lang);
  TTestFHIROperationEngine(result).FIsReadAllowed := true;
  TTestFHIROperationEngine(result).FStorage := self;
end;

procedure TTestStorageService.Yield(op: TFHIROperationEngine; exception: Exception);
begin
  TTestFHIROperationEngine(op).FServerContext.Free;
  op.free;
end;

function TTestStorageService.GetTotalResourceCount: integer;
begin
  result := 1;
end;

procedure TTestStorageService.QueueResource(session : TFHIRSession; r: TFhirResourceV; dateTime: TFslDateTime);
begin
end;

procedure TTestStorageService.RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception);
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

procedure TTestStorageService.recordOAuthLogin(id, client_id, scope, redirect_uri, state, launch: String);
var
  l : TTestOAuthLogin;
begin
  l := TTestOAuthLogin.Create;
  try
    l.client_id := client_id;
    l.redirect := redirect_uri;
    l.state := state;
    l.scope := scope;
    l.launch := launch;
    FOAuths.Add(id, l.Link);
  finally
    l.Free;
  end;
end;

procedure TTestStorageService.recordPackageLoaded(id, ver: String; count: integer; blob: TBytes);
begin
  raise EFslException.Create('Not Implemented');
end;

function TTestStorageService.hasOAuthSession(id: String; status: integer): boolean;
begin
  result := FOAuths.ContainsKey(id);
end;

function TTestStorageService.loadPackages: TFslMap<TLoadedPackageInformation>;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestStorageService.ProcessEmails;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestStorageService.ProcessObservations;
begin
  //
end;

procedure TTestStorageService.ProcessSubscriptions;
begin
  // do nothing...
end;

function TTestStorageService.ProfilesAsOptionList: String;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestStorageService.QueueResource(session : TFHIRSession; r: TFhirResourceV);
begin
  raise EFslException.Create('Not Implemented');
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

procedure TTestStorageService.fetchClients(list: TFslList<TRegisteredClientInformation>);
begin
  raise EFslException.Create('Not Implemented');
end;

function TTestStorageService.fetchLoadedPackage(id: String): TBytes;
begin
  raise EFslException.Create('Not Implemented');
end;

function TTestStorageService.fetchOAuthDetails(id: String; var client_id, redirect, state, scope, launch: String): boolean;
var
  l : TTestOAuthLogin;
begin
  result := FOAuths.ContainsKey(id);
  if result then
  begin
    l := FOAuths[id];
    client_id := l.client_id;
    redirect := l.redirect;
    state := l.state;
    scope := l.scope;
    launch := l.launch;
  end;

end;

function TTestStorageService.fetchOAuthDetails(key, status: integer; var client_id, name, redirect, state, scope, launch: String): boolean;
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
    launch := l.launch;
  end;
end;

function TTestStorageService.FetchResource(key: integer): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestStorageService.FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestStorageService.FinishRecording();
begin
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

function TTestStorageService.RetrieveSession(key: integer; var UserKey, Provider: integer; var Id, Name, Email: String): boolean;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestStorageService.RunValidation;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestStorageService.SetContext(const Value: TFHIRServerContext);
begin
  FContext := Value;
end;

procedure TTestStorageService.SetupRecording(Session : TFHIRSession);
begin
end;

function TTestStorageService.storeClient(client: TRegisteredClientInformation; sessionKey: integer): String;
begin

end;

procedure TTestStorageService.Sweep;
begin
  //
end;

{ TTestingFHIRUserProvider }

function TTestingFHIRUserProvider.CheckId(id: String; var username, hash: String): boolean;
begin
  raise EFslException.Create('Not Implemented');
end;

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

function TTestingFHIRUserProvider.loadOrCreateUser(id, name, email: String; var key: integer): TSCIMUser;
begin
  raise EFslException.Create('Not Implemented');
end;

function TTestingFHIRUserProvider.loadUser(key: integer): TSCIMUser;
begin
  raise EFslException.Create('Not Implemented');
end;

{ TTestFHIROperationEngine }

procedure TTestFHIROperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; httpCode: Integer; name, message: String; patients : TArray<String>);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestFHIROperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; opName: String; httpCode: Integer; name, message: String; patients : TArray<String>);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestFHIROperationEngine.CommitTransaction;
begin
end;

procedure TTestFHIROperationEngine.ExecuteMetadata(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  oConf : TFhirCapabilityStatement;
  c : TFhirContactPoint;
  ct: TFhirContactDetail;
  rest : TFhirCapabilityStatementRest;
  ext : TFhirExtension;
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

  oConf.version := FHIR_GENERATED_VERSION+'-'+SERVER_FULL_VERSION; // this conformance statement is versioned by both
  oConf.name := 'Health Intersections FHIR Server Conformance Statement';
  oConf.publisher := 'Health Intersections'; //
  oConf.description := 'Standard Conformance Statement for the open source Reference FHIR Server provided by Health Intersections';
  oConf.status := PublicationStatusActive;
  oConf.experimental := false;
  oConf.date := TFslDateTime.makeUTC;
  oConf.software := TFhirCapabilityStatementSoftware.Create;
  oConf.software.name := 'Reference Server';
  oConf.software.version := SERVER_FULL_VERSION;
  oConf.software.releaseDate := TFslDateTime.fromXml(SERVER_RELEASE_DATE);

  rest := oConf.restList.Append;
  rest.security := TFhirCapabilityStatementRestSecurity.Create;
  ext := rest.security.addExtension('http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris');
  ext.addExtension('authorize', 'https://'+FStorage.FServer.host+':'+inttostr(FStorage.FServer.SSLPort)+FStorage.FEndpoint.path + FStorage.FEndpoint.AuthServer.AuthPath);
  ext.addExtension('token', 'https://'+FStorage.FServer.host+':'+inttostr(FStorage.FServer.SSLPort)+FStorage.FEndpoint.path + FStorage.FEndpoint.AuthServer.TokenPath);
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
  filename := FHIR_TESTING_FILE(4, 'examples', 'patient-'+request.Id+'.xml');
  if check(response, FIsReadAllowed and (request.ResourceName = 'Patient') and FileExists(filename), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
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
  response.resource := TFhirBundle.Create(BundleTypeSearchset);
end;

function TTestFHIROperationEngine.GetResourceById(request: TFHIRRequest; aType, id, base: String; var needSecure: boolean): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

function TTestFHIROperationEngine.getResourceByUrl(aType: String; url, version: string; allowNil: boolean; var needSecure: boolean): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

function TTestFHIROperationEngine.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
begin
  raise EFslException.Create('Not Implemented');
end;

function TTestFHIROperationEngine.opAllowed(resource: string; command: TFHIRCommandType): Boolean;
begin
  result := true;
end;

function TTestFHIROperationEngine.patientIds(request: TFHIRRequest; res: TFHIRResourceV): TArray<String>;
begin
  result := nil;
end;

procedure TTestFHIROperationEngine.processGraphQL(graphql: String; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTestFHIROperationEngine.RollbackTransaction;
begin
end;

procedure TTestFHIROperationEngine.StartTransaction;
begin
end;
{ TRestFulServerTests }

function TRestFulServerTests.getJson(url: String): TJsonObject;
var
  http : TIdHTTP;
  resp : TBytesStream;
  fmt : TFHIRFormat;
begin
  http := TIdHTTP.Create(nil);
  Try
    http.Request.Accept := 'application/fhir+json';
    resp := TBytesStream.create;
    try
      http.Get(FEndpoint.ClientAddress(false)+url, resp);
      resp.position := 0;
      fmt := ffJson;
      result := TJSONParser.Parse(resp);
    finally
      resp.free;
    end;
  finally
    http.free;
  end;
end;

procedure TRestFulServerTests.registerJs(snder: TObject; js: TJsHost);
begin
  js.engine.registerFactory(fhir4_javascript.registerFHIRTypes, fhirVersionRelease4, TFHIRFactoryR4.create);
  js.engine.registerFactory(fhir4_javascript.registerFHIRTypesDef, fhirVersionUnknown, TFHIRFactoryR4.create);
end;

procedure TRestFulServerTests.Setup;
begin
  FIni := TFHIRServerIniFile.Create('C:\work\fhirserver\utilities\tests\server-tests.ini');
  FGlobals := TFHIRServerSettings.Create;
  FGLobals.load(FIni);

  FServer := TFhirWebServer.create(FGlobals.Link, TFHIRTelnetServer.create(44122, 'test'), 'Test-Server');
  FServer.OnRegisterJs := registerJs;
  FServer.loadConfiguration(FIni);
  FServer.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('C:\work\fhirserver\server\web');

  FStore := TTestStorageService.create(TFHIRFactoryX.create);
  FContext := TFHIRServerContext.Create(FStore.Link, TTestServerFactory.create(FStore.Factory.version));
  FContext.Globals := FGlobals.Link;
  FStore.ServerContext := FContext;
  FContext.TerminologyServer := TTerminologyServer.Create(nil, FContext.factory.Link, nil);
  FContext.Validate := true; // move to database config FIni.ReadBool(voVersioningNotApplicable, 'fhir', 'validate', true);
  FContext.UserProvider := TTestingFHIRUserProvider.Create;
  FEndpoint := FServer.registerEndPoint('test', '/test', FContext.Link, FIni);
  FServer.OWinSecuritySecure := true;
  FServer.ServeMissingCertificate := true;
  FServer.ServeUnknownCertificate := true;
  FStore.FServer := FServer;
  FStore.FEndpoint := FEndpoint;
  FServer.Start(true, true);

  FClientXml := TFhirClients.makeIndy(FContext.ValidatorContext.Link as TFHIRWorkerContext, FEndpoint.ClientAddress(false), false);
  FClientJson := TFhirClients.makeIndy(FContext.ValidatorContext.Link as TFHIRWorkerContext, FEndpoint.ClientAddress(false), true);
  FClientSSL := TFhirClients.makeIndy(FContext.ValidatorContext.Link as TFHIRWorkerContext, FEndpoint.ClientAddress(true), false);
  FClientSSLCert := TFhirClients.makeIndy(FContext.ValidatorContext.Link as TFHIRWorkerContext, FEndpoint.ClientAddress(true), true);
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
  FGlobals.Free;
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
      http.Get(FEndpoint.ClientAddress(false)+'/metadata', resp);
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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := 'C:\work\fhirserver\utilities\tests\client.test.fhir.org.cert';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := 'test';

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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := 'C:\work\fhirserver\utilities\tests\client.test.fhir.org.cert';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := 'test';

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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := 'C:\work\fhirserver\utilities\tests\client.test.fhir.org.cert';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := 'test';

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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := 'C:\work\fhirserver\utilities\tests\local.fhir.org.cert';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := 'test';

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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := 'C:\work\fhirserver\utilities\tests\client.test.fhir.org.cert';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := 'test';

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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := 'C:\work\fhirserver\utilities\tests\local.fhir.org.cert';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := 'test';

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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := 'C:\work\fhirserver\utilities\tests\client.test.fhir.org.cert';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := 'test';

  try
    res := FClientSSL.readResource(frtPatient, 'example');
    try
      Assert.IsFalse(true, 'should not be abe to read without authorization, but got a '+res.className);
    finally
      res.free;
    end;
  except
    on e:EFHIRClientException do
      Assert.isTrue(e.issue.code = itLogin, 'Isseue type is wrong');
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
  FServer.Start(true, true);
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := 'C:\work\fhirserver\utilities\tests\client.test.fhir.org.cert';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := 'test';
  FClientSSL.smartToken := TClientAccessToken.create;
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
  FServer.Start(true, true);
  FClientSSL.smartToken := TClientAccessToken.create;
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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

  (FClientSSL.Communicator as TFHIRHTTPCommunicator).authoriseByOWin(FClientSSL.address+'/'+OWIN_TOKEN_PATH, 'test', 'test');
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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

  tester := TSmartOnFhirTestingLogin.create;
  try
    tester.server.fhirEndPoint := FEndpoint.ClientAddress(true);
    tester.server.thishost := 'localhost';
    tester.server.authorizeEndpoint := 'https://'+FServer.host+':'+inttostr(FServer.SSLPort)+FEndpoint.path + FEndpoint.AuthServer.AuthPath;
    tester.server.tokenEndPoint := 'https://'+FServer.host+':'+inttostr(FServer.SSLPort)+FEndpoint.path + FEndpoint.AuthServer.TokenPath;
    tester.scopes := 'openid profile user/*.*';
    tester.server.clientid := 'web';
    tester.server.redirectport := 961;
    tester.server.clientsecret := 'this-password-is-never-used';
    tester.username := 'test';
    tester.password := 'test';
    tester.login(stmAllOk);
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
  FServer.Start(true, true);
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

  try
    res := FClientSSL.readResource(frtPatient, 'example');
    try
      Assert.IsFalse(true, 'should not be abe to read without authorization, but got a '+res.className);
    finally
      res.free;
    end;
  except
    on e:EFHIRClientException do
      Assert.isTrue(e.issue.code = itLogin, 'Isseue type is wrong');
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

procedure TRestFulServerTests.TestSmartStandaloneLaunchBadRedirect;
var
  json : TJsonObject;
  tester : TSmartOnFhirTestingLogin;
  res : TFhirResource;
begin
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

  json := getJson('/.well-known/smart-configuration');
  try
    tester := TSmartOnFhirTestingLogin.create;
    try
      tester.server.fhirEndPoint := FEndpoint.ClientAddress(true);
      tester.server.thishost := 'localhost';
      tester.server.authorizeEndpoint := json.str['authorization_endpoint'];
      tester.server.tokenEndPoint := json.str['token_endpoint'];
      tester.scopes := 'openid profile user/*.*';
      tester.server.clientid := 'web';
      tester.server.redirectport := 961;
      tester.server.clientsecret := 'this-password-is-never-used';
      tester.username := 'test';
      tester.password := 'test';
      Assert.WillRaiseWithMessage(procedure begin
        tester.login(stmBadRedirect);
        end, EFHIRException, 'HTTP/1.1 400 Bad Request');
    finally
      tester.Free;
    end;
  finally
    json.Free;
  end;
end;

procedure TRestFulServerTests.TestSmartStandaloneLaunchCS;
var
  cs : TFhirCapabilityStatement;
  tester : TSmartOnFhirTestingLogin;
  res : TFhirResource;
begin
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

  cs := sslGet('/metadata') as TFhirCapabilityStatement;
  try
    tester := TSmartOnFhirTestingLogin.create;
    try
      tester.server.fhirEndPoint := FEndpoint.ClientAddress(true);
      tester.server.thishost := 'localhost';
      tester.server.authorizeEndpoint := cs.restList[0].security.getExtensionByUrl('http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris').getExtensionString('authorize');
      tester.server.tokenEndPoint := cs.restList[0].security.getExtensionByUrl('http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris').getExtensionString('token');
      tester.scopes := 'openid fhirUser user/*.*';
      tester.server.clientid := 'web';
      tester.server.redirectport := 961;
      tester.server.clientsecret := 'this-password-is-never-used';
      tester.username := 'test';
      tester.password := 'test';
      tester.login(stmAllOk);
      FClientSSL.SmartToken := tester.token.link;
    finally
      tester.Free;
    end;
    Assert.IsTrue(FClientSSL.SmartToken.idToken <> nil);
    res := FClientSSL.readResource(frtPatient, 'example');
    try
      Assert.IsNotNull(res, 'no resource returned');
      Assert.isTrue(res is TFHIRPatient, 'Resource should be Patient, not '+res.className);
      Assert.IsTrue(res.id = 'example');
    finally
      res.Free;
    end;
  finally
    cs.Free;
  end;
end;

procedure TRestFulServerTests.TestSmartStandaloneLaunchError;
var
  json : TJsonObject;
  tester : TSmartOnFhirTestingLogin;
  res : TFhirResource;
begin
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

  json := getJson('/.well-known/smart-configuration');
  try
    tester := TSmartOnFhirTestingLogin.create;
    try
      tester.server.fhirEndPoint := FEndpoint.ClientAddress(true);
      tester.server.thishost := 'localhost';
      tester.server.authorizeEndpoint := json.str['authorization_endpoint'];
      tester.server.tokenEndPoint := json.str['token_endpoint'];
      tester.scopes := 'openid profile user/*.*';
      tester.server.clientid := 'web';
      tester.server.redirectport := 961;
      tester.server.clientsecret := 'this-password-is-never-used';
      tester.username := 'test';
      tester.password := 'test';
      Assert.WillRaiseWithMessage(procedure begin
        tester.login(stmBadLogin);
        end, EFHIRException, 'http://localhost:961/done?error=access_denied&error_description=Login%20failed&state='+tester.state);

    finally
      tester.Free;
    end;
  finally
    json.Free;
  end;
end;

procedure TRestFulServerTests.TestSmartStandaloneLaunchNU;
var
  json : TJsonObject;
  tester : TSmartOnFhirTestingLogin;
  res : TFhirResource;
begin
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

  json := getJson('/.well-known/smart-configuration');
  try
    tester := TSmartOnFhirTestingLogin.create;
    try
      tester.server.fhirEndPoint := FEndpoint.ClientAddress(true);
      tester.server.thishost := 'localhost';
      tester.server.authorizeEndpoint := json.str['authorization_endpoint'];
      tester.server.tokenEndPoint := json.str['token_endpoint'];
      tester.scopes := 'user/*.*';
      tester.server.clientid := 'web';
      tester.server.redirectport := 961;
      tester.server.clientsecret := 'this-password-is-never-used';
      tester.username := 'test';
      tester.password := 'test';
      tester.login(stmAllOk);
      FClientSSL.SmartToken := tester.token.link;
    finally
      tester.Free;
    end;
    Assert.IsTrue(FClientSSL.SmartToken.idToken = nil);
    res := FClientSSL.readResource(frtPatient, 'example');
    try
      Assert.IsNotNull(res, 'no resource returned');
      Assert.isTrue(res is TFHIRPatient, 'Resource should be Patient, not '+res.className);
      Assert.IsTrue(res.id = 'example');
    finally
      res.Free;
    end;
  finally
    json.Free;
  end;
end;

procedure TRestFulServerTests.TestSmartStandaloneLaunchWK;
var
  json : TJsonObject;
  tester : TSmartOnFhirTestingLogin;
  res : TFhirResource;
begin
  FClientSSL.smartToken := nil;
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certFile := '';
  (FClientSSL.Communicator as TFHIRHTTPCommunicator).certPWord := '';

  json := getJson('/.well-known/smart-configuration');
  try
    tester := TSmartOnFhirTestingLogin.create;
    try
      tester.server.fhirEndPoint := FEndpoint.ClientAddress(true);
      tester.server.thishost := 'localhost';
      tester.server.authorizeEndpoint := json.str['authorization_endpoint'];
      tester.server.tokenEndPoint := json.str['token_endpoint'];
      tester.scopes := 'openid profile user/*.*';
      tester.server.clientid := 'web';
      tester.server.redirectport := 961;
      tester.server.clientsecret := 'this-password-is-never-used';
      tester.username := 'test';
      tester.password := 'test';
      tester.login(stmAllOk);
      FClientSSL.SmartToken := tester.token.link;
    finally
      tester.Free;
    end;
    Assert.IsTrue(FClientSSL.SmartToken.idToken <> nil);
    res := FClientSSL.readResource(frtPatient, 'example');
    try
      Assert.IsNotNull(res, 'no resource returned');
      Assert.isTrue(res is TFHIRPatient, 'Resource should be Patient, not '+res.className);
      Assert.IsTrue(res.id = 'example');
    finally
      res.Free;
    end;
  finally
    json.Free;
  end;
end;

function TRestFulServerTests.SslGet(url : String) : TFHIRResource;
var
  http : TIdHTTP;
  resp : TBytesStream;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  fmt : TFHIRFormat;
begin
  http := TIdHTTP.Create(nil);
  Try
    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    Try
      http.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      ssl.SSLOptions.Method := sslvTLSv1_2;
      http.Request.Accept := 'application/fhir+json';
      resp := TBytesStream.create;
      try
        http.Get(FEndpoint.ClientAddress(true)+url, resp);
        resp.position := 0;
        fmt := ffJson;
        result := streamToResource(resp, fmt);
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
        http.Get(FEndpoint.ClientAddress(true)+'/metadata', resp);
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
      http.Get(FEndpoint.ClientAddress(false)+'/metadata', resp);
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

{ TTestServerFactory }

constructor TTestServerFactory.create(version : TFHIRVersion);
begin
  inherited Create;
  FVersion := version;
end;

function TTestServerFactory.makeValidator: TFHIRValidatorV;
begin
  result := TFHIRValidator4.Create(TFHIRServerWorkerContextR4.Create(TFHIRFactoryR4.create));
end;

function TTestServerFactory.makeEngine(validatorContext: TFHIRWorkerContextWithFactory; ucum: TUcumServiceImplementation): TFHIRPathEngineV;
begin
  result := TFHIRPathEngine4.Create(validatorContext as TFHIRWorkerContext4, ucum);
end;

function TTestServerFactory.makeIndexer : TFHIRIndexManager;
begin
  result := TFhirIndexManager4.Create;
end;

function TTestServerFactory.makeIndexes: TFHIRIndexBuilder;
begin
  result := TFHIRIndexBuilderR4.create;
end;

function TTestServerFactory.makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager;
begin
  result := TSubscriptionManagerR4.Create(ServerContext);
end;

procedure TTestServerFactory.setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer});
begin
  TFHIRServerWorkerContextR4(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
end;


initialization
  TDUnitX.RegisterTestFixture(TRestFulServerTests);
{$ENDIF}
end.
