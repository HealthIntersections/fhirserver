unit AuthServer;

// to convert .crt to .pem openssl x509 -in mycert.crt -out mycert.pem -outform PEM

interface

uses
  SysUtils, Classes, System.Generics.Collections,

  IniFiles,

  IdContext, IdCustomHTTPServer, IdCookie,

  ParseMap, KDBManager, KDBDialects, KCritSct,

  StringSupport, EncodeSupport, GUIDSupport, DateSupport, AdvObjects, AdvMemories, AdvJSON, JWT, AdvExceptions,

  FacebookSupport, SCIMServer, SCIMObjects,

  FHIRDataStore, FHIRSupport, FHIRBase, FHIRResources, FHIRConstants, FHIRSecurity, FHIRUtilities;

Const
  FHIR_COOKIE_NAME = 'fhir-session-idx';

type
  TDoSearchEvent = function (session : TFhirSession; rtype : string; lang, params : String) : TFHIRBundle of object;

  TTokenCategory = (tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcOther);

  TFhirLoginToken = Class (TAdvObject)
  private
    FProvider : TFHIRAuthProvider;
    FPath : String;
    FExpires : TDateTime;
  end;

  // predefined token per user for testing

  // this is a server that lives at /oauth2
  TAuth2Server = class (TAdvObject)
  private
    FLock : TCriticalSection;
    FIni : TIniFile;
    FFhirStore : TFHIRDataStore;
    FOnProcessFile : TProcessFileEvent;
    FFilePath : String;
    FSSLPort : String;
    FHost : String;
    FRootCert : String;
    FSSLCert : String;
    FSSLPassword : String;

    FFacebookAppid : String;
    FFacebookAppSecret : String;
    FLoginTokens : TStringList;
    FGoogleAppid : String;
    FGoogleAppSecret : String;
    FGoogleAppKey : String;
//    FAppSecrets : String;
    FHL7Appid : String;
    FHL7AppSecret : String;
    FAdminEmail : String;
    FSCIMServer : TSCIMServer;
    FEndPoint: String;
    FOnDoSearch : TDoSearchEvent;
    function GetPatientListAsOptions : String;
    Procedure HandleAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleLogin(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleChoice(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleUserDetails(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleSkype(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleTokenData(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    Procedure HandleKey(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap;response: TIdHTTPResponseInfo);
    function checkNotEmpty(v, n: String): String;
    function isAllowedRedirect(client_id, redirect_uri: String): boolean;
    function isAllowedAud(client_id, aud_uri: String): boolean;
    procedure SetFhirStore(const Value: TFHIRDataStore);
    function BuildLoginList(id : String) : String;
    function AltFile(path: String): String;
    Function CheckLoginToken(state : string; var original : String; var provider : TFHIRAuthProvider):Boolean;
    procedure loadScopeVariables(variables: TDictionary<String, String>; scope: String; user : TSCIMUser);
    procedure readScopes(scopes: TStringList; params: TParseMap);

  public
    Constructor Create(ini : String; filePath, Host, SSLPort : String; SCIM : TSCIMServer);
    Destructor Destroy; override;

    Procedure HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo);
    Procedure HandleDiscovery(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

    procedure setCookie(response: TIdHTTPResponseInfo; const cookiename, cookieval, domain, path: String; expiry: TDateTime; secure: Boolean);

    property FHIRStore : TFHIRDataStore read FFhirStore write SetFhirStore;
    property OnProcessFile : TProcessFileEvent read FOnProcessFile write FOnProcessFile;

    property FacebookAppid : String read FFacebookAppid;
    property GoogleAppid : String read FGoogleAppid;
    property GoogleAppKey : String read FGoogleAppKey;
    property HL7Appid : String read FHL7Appid;
    function MakeLoginToken(path: String; provider: TFHIRAuthProvider): String;
    property RootCert : String read FRootCert write FRootCert;
    property SSLCert : String read FSSLCert write FSSLCert;
    property SSLPassword : String read FSSLPassword write FSSLPassword;
    property AdminEmail : String read FAdminEmail write FAdminEmail;

    function AuthPath : String;
    function BasePath : String;
    function TokenPath : String;
    Property EndPoint : String read FEndPoint write FEndPoint;
    Property Ini : TIniFile read FIni;
    property OnDoSearch : TDoSearchEvent read FOnDoSearch write FOnDoSearch;
  end;


implementation

uses
  FHIRLog, SystemService;

const
  CODES_TTokenCategory : array [TTokenCategory] of String = ('Clinical', 'Data', 'Meds', 'Schedule', 'Audit', 'Documents', 'Financial', 'Other');

// categories for web login
// tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcOther
  {$IFDEF FHIR2}
  RESOURCE_CATEGORY : array [TFHIRResourceType] of TTokenCategory =
    (
    tcOther,
    tcFinancial , // frtAccount,
    tcClinical , // frtAllergyIntolerance,
    tcSchedule , // frtAppointment,
    tcSchedule , // frtAppointmentResponse,
    tcAudit , // frtAuditEvent,
    tcOther , // frtBasic,
    tcDocuments , // frtBinary,
    tcOther , // frtBodySite,
    tcOther , // frtBundle,
    tcClinical , // frtCarePlan,
    tcFinancial , // frtClaim,
    tcFinancial , // frtClaimResponse,
    tcClinical , // frtClinicalImpression,
    tcOther , // frtCommunication,
    tcOther , // frtCommunicationRequest,
    tcDocuments , // frtComposition,
    tcOther , // frtConceptMap,
    tcClinical , // frtCondition,
    tcOther , // frtConformance,
    tcFinancial , // frtContract,
    tcFinancial , // frtCoverage,
    tcOther , // frtDataElement,
    tcClinical,  // frtDetectedIssue,
    tcOther , // frtDevice,
    tcOther , // frtDeviceComponent,
    tcOther , // frtDeviceMetric,
    tcOther , // frtDeviceUseRequest,
    tcClinical, // frtDeviceUseStatement,
    tcOther , // frtDiagnosticOrder,
    tcClinical, // frtDiagnosticReport
    tcDocuments , // frtDocumentManifest,
    tcDocuments, // frtDocumentReference
    tcFinancial , // frtEligibilityRequest,
    tcFinancial, // frtEligibilityResponse
    tcSchedule , // frtEncounter,
    tcFinancial, // frtEnrollmentRequest
    tcFinancial , // frtEnrollmentResponse,
    tcSchedule, // frtEpisodeOfCare
    tcFinancial , // frtExplanationOfBenefit,
    tcClinical, // frtFamilyMemberHistory
    tcClinical , // frtFlag,
    tcClinical , // frtGoal,
    tcOther , // frtGroup,
    tcSchedule , // frtHealthcareService,
    tcData, // frtImagingObjectSelection
    tcData , // frtImagingStudy,
    tcMeds , // frtImmunization,
    tcMeds , // frtImmunizationRecommendation,
    tcOther , // frtImplemnetationGuide,
    tcOther , // frtList,
    tcOther , // frtLocation,
    tcOther , // frtMedia,
    tcMeds , // frtMedication,
    tcMeds , // frtMedicationAdministration,
    tcMeds , // frtMedicationDispense,
    tcMeds , // frtMedicationPrescription,
    tcMeds , // frtMedicationStatement,
    tcOther , // frtMessageHeader,
    tcOther , // frtNamingSystem,
    tcMeds , // frtNutritionOrder,
    tcData , // frtObservation,
    tcOther , // frtOperationDefinition,
    tcOther , // frtOperationOutcome,
    tcOther , // frtOrder,
    tcOther , // frtOrderResponse,
    tcOther , // frtOrganization,
    tcOther , // frtParameters,
    tcSchedule , // frtPatient,
    tcFinancial , // frtPaymentNotice,
    tcFinancial , // frtPaymentReconciliation,
    tcOther , // frtPerson,
    tcOther , // frtPractitioner,
    tcClinical , // frtProcedure,
    tcOther , // frtProcedureRequest,
    tcOther , // frtProcessRequest,
    tcOther , // frtProcessResponse,
    tcAudit , // frtProvenance,
    tcOther , // frtQuestionnaire,
    tcOther, // frtQuestionnaireResponse
    tcClinical , // frtReferralRequest,
    tcOther , // frtRelatedPerson,
    tcOther , // frtRiskAssessment,
    tcSchedule , // frtSchedule,
    tcOther , // frtSearchParameter,
    tcSchedule , // frtSlot,
    tcData , // frtSpecimen,
    tcOther , // frtStructureDefinition,
    tcOther , // frtSubscription,
    tcOther , // frtSubstance,
    tcOther , // frtSupplyDelivery,
    tcOther , // frtSupplyRequest,
    tcOther , // frtTestScript,
    tcOther , // frtValueSet,
    tcClinical, tcOther); // frtVisionPrescription);
  {$ELSE}
  RESOURCE_CATEGORY : array [TFHIRResourceType] of TTokenCategory =
    (
    tcOther, // frtNull
    tcFinancial , // frtAccount
    tcOther, // ActivityDefinition
    tcClinical , // frtAllergyIntolerance
    tcSchedule , // frtAppointment
    tcSchedule , // frtAppointmentResponse
    tcAudit , // frtAuditEvent
    tcOther , // frtBasic
    tcDocuments , // frtBinary
    tcOther , // frtBodySite
    tcOther , // frtBundle
    tcClinical , // frtCarePlan
    tcClinical , // frtCareTeam
    tcFinancial , // frtClaim
    tcFinancial , // frtClaimResponse
    tcClinical , // frtClinicalImpression
    tcOther , // frtCodeSystem
    tcOther , // frtCommunication
    tcOther , // frtCommunicationRequest
    tcOther , // frtCompartmentDefinition
    tcDocuments , // frtComposition
    tcOther , // frtConceptMap
    tcClinical , // frtCondition
    tcOther , // frtConformance
    tcOther, // frtConsent
    tcFinancial , // frtContract
    tcFinancial , // frtCoverage
    tcOther , // frtDataElement
    tcOther , // frtDecisionSupportRule
    tcClinical,  // frtDetectedIssue
    tcOther , // frtDevice
    tcOther , // frtDeviceComponent
    tcOther , // frtDeviceMetric
    tcOther , // frtDeviceUseRequest
    tcClinical, // frtDeviceUseStatement
    tcClinical, // frtDiagnosticReport
    tcClinical , // frtDiagnosticOrder
    tcDocuments , // frtDocumentManifest
    tcDocuments, // frtDocumentReference
    tcFinancial , // frtEligibilityRequest
    tcFinancial, // frtEligibilityResponse
    tcSchedule , // frtEncounter
    tcOther, // frtEndpoint
    tcFinancial, // frtEnrollmentRequest
    tcFinancial , // frtEnrollmentResponse
    tcSchedule, // frtEpisodeOfCare
    tcOther, // frtExpansionProfile
    tcFinancial , // frtExplanationOfBenefit
    tcClinical, // frtFamilyMemberHistory
    tcClinical , // frtFlag
    tcClinical , // frtGoal
    tcOther , // frtGroup
    tcClinical , // frtGuidanceResponse
    tcSchedule , // frtHealthcareService
    tcData, // frtImagingManifest
    tcData , // frtImagingStudy
    tcMeds , // frtImmunization
    tcMeds , // frtImmunizationRecommendation
    tcOther , // frtImplemnetationGuide
    tcOther, // frtLibrary
    tcClinical , // frtLinkage
    tcOther , // frtList
    tcOther , // frtLocation
    tcOther, // frtMeasure
    tcOther , // frtMedia
    tcOther , // frtMedia
    tcMeds , // frtMedication
    tcMeds , // frtMedicationAdministration
    tcMeds , // frtMedicationDispense
    tcMeds , // frtMedicationPrescription
    tcMeds , // frtMedicationStatement
    tcOther , // frtMessageHeader
    tcOther , // frtNamingSystem
    tcMeds , // frtNutritionOrder
    tcData , // frtObservation
    tcOther , // frtOperationDefinition
    tcOther , // frtOperationOutcome
    tcOther , // frtOrganization
    tcOther , // frtParameters
    tcSchedule , // frtPatient
    tcFinancial , // frtPaymentNotice
    tcFinancial , // frtPaymentReconciliation
    tcOther , // frtPerson
    tcOther , // frtPlanDefinition
    tcOther , // frtPractitioner
    tcOther , // frtPractitionerRole
    tcClinical , // frtProcedure
    tcOther , // frtProcedureRequest
    tcOther , // frtProcessRequest
    tcOther , // frtProcessResponse
    tcAudit , // frtProvenance
    tcOther , // frtQuestionnaire
    tcOther, // frtQuestionnaireResponse
    tcClinical , // frtReferralRequest,
    tcOther , // frtRelatedPerson,
    tcOther , // frtRiskAssessment,
    tcSchedule , // frtSchedule,
    tcOther , // frtSearchParameter,
    tcData, // frtSequence
    tcSchedule , // frtSlot,
    tcData , // frtSpecimen,
    tcOther , // frtStructureDefinition,
    tcOther , // frtStructureMap[,
    tcOther , // frtSubscription,
    tcOther , // frtSubstance,
    tcOther , // frtSupplyDelivery,
    tcOther , // frtSupplyRequest,
    tcOther , // frtTask,
    tcOther{$IFNDEF FHIR2CM} , // frtTestScript,
    tcOther , // frtValueSet,
    tcClinical,// frtVisionPrescription,
    tcOther {$ENDIF}); // frtCustom

  {$ENDIF}


{ TAuth2Server }

constructor TAuth2Server.Create(ini: String; filePath, Host, SSLPort : String; SCIM : TSCIMServer);
begin
  inherited create;
  FSCIMServer := SCIM;
  FIni := TIniFile.Create(ini);
  FFilePath := filePath;
  FHost := host;
  FSSLPort := SSLPort;
  FLock := TCriticalSection.Create('auth-server');
  FLoginTokens := TStringList.create;

  FHL7Appid := FIni.ReadString('hl7.org', 'app-id', '');
  FHL7AppSecret := FIni.ReadString('hl7.org', 'app-secret', '');
  FFacebookAppid := FIni.ReadString('facebook.com', 'app-id', '');
  FFacebookAppSecret := FIni.ReadString('facebook.com', 'app-secret', '');
  FGoogleAppid := FIni.ReadString('google.com', 'app-id', '');
  FGoogleAppSecret := FIni.ReadString('google.com', 'app-secret', '');
  FGoogleAppKey := FIni.ReadString('google.com', 'app-key', '');

end;

destructor TAuth2Server.Destroy;
begin
  FLoginTokens.Free;;
  FLock.Free;
  FFhirStore.Free;
  FIni.Free;
  FSCIMServer.free;
  inherited;
end;


function TAuth2Server.GetPatientListAsOptions: String;
var
  bundle : TFhirBundle;
  entry : TFhirBundleEntry;
  patient : TFhirPatient;
  b : TStringBuilder;
begin
  bundle := OnDoSearch(nil, 'Patient', 'en', '_summary=true&__wantObject=true');
  b := TStringBuilder.create;
  try
    b.Append('<option value=""/>');
    for entry in bundle.entryList do
    begin
      patient := entry.resource as TFhirPatient;
      b.Append('<option value="');
      b.Append(patient.id);
      b.Append('">');
      b.Append(HumanNamesAsText(patient.nameList));
      b.Append(' (');
      b.Append(patient.id);
      b.Append(')</option>');
    end;
    result := b.ToString;
  finally
    b.Free;
    bundle.Free;
  end;
end;

function TAuth2Server.BasePath: String;
begin
  if FSSLPort = '443' then
    result := 'https://'+FHost+'/oauth2'
  else
    result := 'https://'+FHost+':'+FSSLPort+'/oauth2';
end;

function TAuth2Server.MakeLoginToken(path : String; provider : TFHIRAuthProvider): String;
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




function TAuth2Server.checkNotEmpty(v , n : String) : String;
begin
  if (v = '') then
    raise Exception.Create('Parameter "'+n+'" not found');
  result := v;
end;


function TAuth2Server.AltFile(path : String) : String;
begin
  if path.StartsWith('/') then
    result := FFilePath+path.Substring(1).Replace('/', '\')
  else
    result := '';
end;


function TAuth2Server.isAllowedAud(client_id, aud_uri: String): boolean;
begin
  result := (aud_uri = EndPoint);
end;

function TAuth2Server.isAllowedRedirect(client_id, redirect_uri: String): boolean;
var
  i : integer;
  s : String;
begin
  i := 0;
  result := false;
  repeat
    s := FIni.ReadString(client_id, 'redirect'+inttostr(i), '');
    result := s = redirect_uri;
    inc(i);
  until result or (s = '');
end;

procedure TAuth2Server.setCookie(response: TIdHTTPResponseInfo; const cookiename, cookieval, domain, path: String; expiry: TDateTime; secure: Boolean);
var
  cookie: TIdCookie;
begin
  cookie := response.Cookies.Add;
  cookie.CookieName := cookiename;
  cookie.Value := cookieval;
  cookie.Domain := domain;
  cookie.Path := '/';  // path;
  cookie.Expires := expiry;
  cookie.Secure := secure;
end;




procedure TAuth2Server.SetFhirStore(const Value: TFHIRDataStore);
begin
  FFhirStore.Free;
  FFhirStore := Value;
end;

function TAuth2Server.TokenPath: String;
begin
 result := '/oauth2/token';
end;

procedure TAuth2Server.HandleAuth(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params : TParseMap; response: TIdHTTPResponseInfo);
var
  client_id : String;
  scope : String;
  redirect_uri : String;
  state : String;
  aud : String;
  id : String;
  conn : TKDBConnection;
  variables : TDictionary<String,String>;
begin
  if params.GetVar('response_type') <> 'code' then
    raise Exception.Create('Only response_type allowed is ''code''');
  client_id := checkNotEmpty(params.GetVar('client_id'), 'client_id');
  if FIni.ReadString(client_id, 'name', '') = '' then
    raise Exception.Create('Unknown Client Identifier "'+client_id+'"');
  redirect_uri := checkNotEmpty(params.GetVar('redirect_uri'), 'redirect_uri');
  if not isAllowedRedirect(client_id, redirect_uri) then
    raise Exception.Create('Unacceptable Redirect url "'+redirect_uri+'"');
  scope := checkNotEmpty(params.GetVar('scope'), 'scope');
  state := checkNotEmpty(params.GetVar('state'), 'state');
  aud := checkNotEmpty(params.GetVar('aud'), 'aud');
  if not isAllowedAud(client_id, aud) then
    raise Exception.Create('Unacceptable FHIR Server URL "'+aud+'"');

  id := newguidid;
  conn := FFhirStore.DB.GetConnection('oatuh2');
  try
    conn.ExecSQL('insert into OAuthLogins (Id, Client, Scope, Redirect, ClientState, Status, DateAdded) values ('''+id+''', '''+client_id+''', '''+SQLWrapString(scope)+''', '''+SQLWrapString(redirect_uri)+''', '''+SQLWrapString(state)+''', 1, '+DBGetDate(conn.Owner.Platform)+')');
    conn.release;

  except
    on e:exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;

  variables := TDictionary<String,String>.create;
  try

    variables.Add('idmethods', BuildLoginList(id));
    variables.Add('client', FIni.ReadString(client_id, 'name', ''));
    OnProcessFile(response, session, '/oauth_login.html', AltFile('/oauth_login.html'), true, variables)
  finally
    variables.free;
  end;
end;

procedure TAuth2Server.loadScopeVariables(variables : TDictionary<String,String>; scope : String; user : TSCIMUser);
//patient/*.read	Permission to read any resource for the current patient
//user/*.*	Permission to read and write all resources that the current user can access
//openid profile	Permission to retrieve information about the current logged-in user
//launch	Permission to obtain launch context when app is launched from an EHR
//launch/patient	When launching outside the EHR, ask for a patient to be selected at launch time
var
  security : TFHIRSecurityRights;
  t : TFhirResourceType;
begin
  variables.add('userlevel', '');
  variables.add('userinfo', '');
  security := TFHIRSecurityRights.create(FFhirStore.ValidatorContext.Link, user, scope, true);
  try
    if security.canAdministerUsers then
      variables.add('useradmin', '<input type="checkbox" name="useradmin" value="1"/> Administer Users')
    else
      variables.add('useradmin', '');
    if security.canGetUserInfo then
      variables['userinfo'] := 'checked';

    for t := Low(TFHIRResourceType) to High(TFHIRResourceType) do
    begin
      variables.AddOrSetValue('read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]], '');
      variables.AddOrSetValue('write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]], '');
      variables.AddOrSetValue('read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]+'disabled', 'disabled');
      variables.AddOrSetValue('write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]+'disabled', 'disabled');
    end;

    for t := Low(TFHIRResourceType) to High(TFHIRResourceType) do
    begin
      if security.canRead(CODES_TFHIRResourceType[t]) then
      begin
        variables['read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]] := 'checked';
        variables['read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]+'disabled'] := '';
      end;
      if security.canWrite(CODES_TFHIRResourceType[t]) then
      begin
        variables['write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]] := 'checked';
        variables['write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]+'disabled'] := '';
      end;
    end;
  finally
    security.free;
  end;
end;

procedure TAuth2Server.readScopes(scopes : TStringList; params : TParseMap);
var
  pfx : String;
  t : TFhirResourceType;
  all : boolean;
begin
  scopes.clear;
  if (params.getVar('userInfo') = '1') then
  begin
    scopes.add('openid');
    scopes.add('profile');
  end;
  if (params.getVar('useradmin') = '1') then
    scopes.add(SCIM_ADMINISTRATOR);

  // if there's a patient, then the scopes a patient specific
  if (params.getVar('patient') = '') then
    pfx := 'User/'
  else
    pfx := 'Patient/';

  all := true;
  for t := Low(TFHIRResourceType) to High(TFHIRResourceType) do
    if (params.GetVar('read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]) <> '1') or
      (params.GetVar('write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]) <> '1') then
      all := false;

  if all then
    scopes.Add(pfx+'*.*')
  else
  begin
    for t := Low(TFHIRResourceType) to High(TFHIRResourceType) do
    begin
      if params.GetVar('read'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]) = '1' then
        scopes.Add(pfx+CODES_TFHIRResourceType[t]+'.read');
      if params.GetVar('write'+CODES_TTokenCategory[RESOURCE_CATEGORY[t]]) = '1' then
        scopes.Add(pfx+CODES_TFHIRResourceType[t]+'.write');
    end;
  end;
end;

procedure TAuth2Server.HandleChoice(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  client_id, name, authurl: String;
  conn : TKDBConnection;
  variables : TDictionary<String,String>;
  scopes : TStringList;
  redirect, state, scope : String;
begin
  if session = nil then
    raise Exception.Create('User Session not found');

  if FSSLPort = '443' then
    authurl := 'https://'+FHost+'/oauth2'
  else
    authurl := 'https://'+FHost+':'+FSSLPort+'/oauth2';

  try
    conn := FFhirStore.DB.GetConnection('OAuth2');
    try
      conn.SQL := 'select Client, Name, Scope, Redirect, ClientState from OAuthLogins, Sessions where OAuthLogins.SessionKey = '+inttostr(session.key)+' and Status = 2 and OAuthLogins.SessionKey = Sessions.SessionKey';
      conn.Prepare;
      conn.Execute;
      if not conn.FetchNext then
        raise Exception.Create('State Error - session "'+inttostr(session.key)+'" not ready for a choice');
      client_id := conn.ColStringByName['Client'];
      name := conn.ColStringByName['Name'];
      redirect := conn.ColStringByName['Redirect'];
      state := conn.ColStringByName['ClientState'];
      scope := conn.ColStringByName['Scope'];
      conn.Terminate;

      if params.getVar('form') = 'true' then
      begin
        scopes := TStringList.create;
        try
          readScopes(scopes, params);

          session.JWT := TJWT.Create;
          session.jwt.header['kid'] := authurl+'/auth_key'; // cause we'll sign with our SSL certificate
          session.jwt.issuer := FHost;
          session.jwt.expires := session.Expires;
          session.jwt.issuedAt := now;
          session.jwt.id := FHost+'/sessions/'+inttostr(Session.Key);


          if params.getVar('user') = '1' then
          begin
          // if user rights granted
            session.jwt.subject := Names_TFHIRAuthProvider[session.Provider]+':'+session.id;
            session.jwt.name := session.Name;
            if session.Email <> '' then
              session.jwt.email := session.Email;
          end;
          session.JWTPacked := TJWTUtils.rsa_pack(session.jwt, jwt_hmac_rsa256, ChangeFileExt(FSSLCert, '.key'), FSSLPassword);

          conn.SQL := 'Update OAuthLogins set Status = 3, DateChosen = '+DBGetDate(conn.Owner.Platform)+', Rights = :r, Patient = :p, Jwt = :jwt where Id = '''+SQLWrapString(Session.OuterToken)+'''';
          conn.prepare;
          conn.BindBlobFromString('r', scopes.CommaText);
          conn.BindBlobFromString('jwt', session.JWTPacked);
          if params.getVar('patient') = '' then
            conn.BindNull('p')
          else
          begin
            conn.BindString('p', params.GetVar('patient'));
            session.PatientList.Add(params.GetVar('patient'));
          end;
          conn.Execute;
          conn.Terminate;

          session.scopes := scopes.CommaText.Replace(',', ' ');
          FHIRStore.RegisterConsentRecord(session);
          response.Redirect(redirect+'?code='+session.OuterToken+'&state='+state);
        finally
          scopes.Free;
        end;
      end
      else
      begin
        variables := TDictionary<String,String>.create;
        try
          variables.Add('client', FIni.ReadString(client_id, 'name', ''));
          variables.Add('username', name);
          variables.Add('patient-list', GetPatientListAsOptions);
          loadScopeVariables(variables, scope, session.User);
          OnProcessFile(response, session, '/oauth_choice.html', AltFile('/oauth_choice.html'), true, variables)
        finally
          variables.free;
        end;
      end;
      conn.Release;
    except
      on e:exception do
      begin
        conn.Error(e);
        recordStack(e);
        raise;
      end;
    end;
  finally
    session.Free;
  end;
end;

procedure TAuth2Server.HandleDiscovery(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  obj :  TJsonObject;
begin
  obj := TJsonObject.create;
  try
    obj['issuer'] := FHost;
    obj['authorization_endpoint'] := BasePath+'/auth';
    obj['token_endpoint'] := BasePath+'/token';
    obj['jwks_uri'] :=  BasePath+'/auth_key';
    obj['registration_endpoint'] := 'mailto:'+FAdminEmail;
    obj.arr['scopes_supported'] := TJsonArray.create.add('read').add('write').add('user');

    obj['subject_types_supported'] := 'public';
    obj.arr['id_token_signing_alg_values_supported'] := TJsonArray.create.add('RS256');
    obj.arr['response_types_supported'] := TJsonArray.create.add('id_token');
    if FSSLPort = '443' then
      obj['service_documentation'] :=  'https://'+FHost+'/local.hts'
    else
      obj['service_documentation'] :=  'https://'+FHost+':'+FSSLPort+'/local.hts';
    response.ResponseNo := 200;
    response.ResponseText := 'OK';
    response.ContentType := 'application/json';
    response.ContentText := TJSONWriter.writeObjectStr(obj, true);
  finally
    obj.free;
  end;

end;

procedure TAuth2Server.HandleKey(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  jwk : TJWK;
  authurl : String;
begin
  if FSSLPort = '443' then
    authurl := 'https://'+FHost+'/oauth2'
  else
    authurl := 'https://'+FHost+':'+FSSLPort+'/oauth2';

  jwk := TJWTUtils.loadKeyFromRSACert(AnsiString(FSSLCert));
  try
    jwk.obj['alg'] := 'RS256';
    jwk.obj['use'] := 'sig';
    jwk.obj['kid'] := authurl+'/auth_key';

    response.ContentType := 'application/json';
    response.ContentText := TJSONWriter.writeObjectStr(jwk.obj, true);
  finally
    jwk.free;
  end;
end;

procedure TAuth2Server.HandleLogin(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  conn : TKDBConnection;
  id, username, password, domain, state, jwt : String;
  authurl, token, expires, msg, uid, name, email : String;
  provider : TFHIRAuthProvider;
  ok : boolean;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));

  conn := FFhirStore.DB.GetConnection('OAuth2');
  try
    if params.VarExists('id') and params.VarExists('username') and params.VarExists('password') then
    begin
      id := params.GetVar('id');
      username := params.GetVar('username');
      password := params.GetVar('password');

      if not FSCIMServer.CheckLogin(username, password) then
        raise Exception.Create('Login failed');

      if conn.CountSQL('select count(*) from OAuthLogins where Id = '''+SQLWrapString(id)+''' and Status = 1') <> 1 then
        raise Exception.Create('State failed - no login session active');

      session := FFhirStore.RegisterSession(apInternal, '', id, username, '', '', '', '1440', AContext.Binding.PeerIP, '');
      try
        conn.ExecSQL('Update OAuthLogins set Status = 2, SessionKey = '+inttostr(session.Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(id)+'''');
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        response.Redirect('/oauth2/auth_choice');
      finally
        session.Free;
      end;
    end
    else if request.document.startsWith('/oauth2/auth_dest/state/') then
    begin
      // HL7
      if not CheckLoginToken(copy(request.document, 25, $FF), id, provider) then
        raise Exception.Create('The state does not match. You may be a victim of a cross-site spoof (or this server has restarted, try again)');
      uid := params.GetVar('userid');
      name := params.GetVar('fullName');
      expires := inttostr(60 * 24 * 10); // 10 days
      session := FFhirStore.RegisterSession(aphl7, '', id, uid, name, '', '', expires, AContext.Binding.PeerIP, '');
      try
        conn.ExecSQL('Update OAuthLogins set Status = 2, SessionKey = '+inttostr(session.Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(id)+'''');
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        response.Redirect('/oauth2/auth_choice');
      finally
        session.Free;
      end;
    end
    else if (params.VarExists('state')) then
    begin
      if FSSLPort = '443' then
        authurl := 'https://'+FHost+'/oauth2/auth_dest'
      else
        authurl := 'https://'+FHost+':'+FSSLPort+'/oauth2/auth_dest';

      state := params.GetVar('state');
      if not StringStartsWith(state, OAUTH_LOGIN_PREFIX, false) then
        raise Exception.Create('State Prefix mis-match');
      if not CheckLoginToken(state, id, provider) then
        raise Exception.Create('The state does not match. You may be a victim of a cross-site spoof (or this server has restarted, try again)');
      if params.VarExists('error') then
        raise Exception.Create('error_description');

      if provider = apGoogle then
      begin
        ok := GoogleCheckLogin(FGoogleAppid, FGoogleAppSecret, authurl, params.GetVar('code'), token, expires, jwt, msg);
        if ok then
          ok := GoogleGetDetails(token, FGoogleAppKey, jwt, uid, name, email, msg);
      end
      else
      begin
        ok := FacebookCheckLogin(FFacebookAppid, FFacebookAppSecret, authurl, params.GetVar('code'), token, expires, msg);
        if ok then
          ok := FacebookGetDetails(token, uid, name, email, msg);
      end;
      if not ok then
        raise Exception.Create('Processing the login failed ('+msg+')');
      session := FFhirStore.RegisterSession(provider, token, id, uid, name, email, '', expires, AContext.Binding.PeerIP, '');
      try
        conn.ExecSQL('Update OAuthLogins set Status = 2, SessionKey = '+inttostr(session.Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(id)+'''');
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        response.Redirect('/oauth2/auth_choice');
      finally
        session.Free;
      end;
    end
    else
      raise Exception.Create('Login attempt not understood');
    conn.release;
  except
    on e:exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TAuth2Server.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo);
var
  params : TParseMap;
begin
  logt('Auth: '+request.Document);
  try
    // cors
    response.CustomHeaders.add('Access-Control-Allow-Origin: *');
    response.CustomHeaders.add('Access-Control-Expose-Headers: Content-Location, Location');
    response.CustomHeaders.add('Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS');
    if request.RawHeaders.Values['Access-Control-Request-Headers'] <> '' then
      response.CustomHeaders.add('Access-Control-Allow-Headers: '+request.RawHeaders.Values['Access-Control-Request-Headers']);
    response.ContentType := 'application/json';

    params := TParseMap.create(request.UnparsedParams);
    try
      if (request.Document = '/oauth2/auth') then
        HandleAuth(AContext, request, session, params, response)
      else if (request.Document.startsWith('/oauth2/auth_dest')) then
        HandleLogin(AContext, request, session, params, response)
      else if (request.Document = '/oauth2/auth_choice') then
        HandleChoice(AContext, request, session, params, response)
      else if (request.Document = '/oauth2/token') then
        HandleToken(AContext, request, session, params, response)
      else if (request.Document = '/oauth2/token_data') then
        HandleTokenData(AContext, request, session, params, response)
      else if (request.Document = '/oauth2/auth_skype') then
        HandleSkype(AContext, request, session, params, response)
      else if (request.Document = '/oauth2/auth_key') then
        HandleKey(AContext, request, session, params, response)
      else if (request.Document = '/oauth2/discovery') then
        HandleDiscovery(AContext, request, response)
      else if (request.Document = '/oauth2/userdetails') then
        HandleUserDetails(AContext, request, session, params, response)
      else
        raise Exception.Create('Invalid URL');
    finally
      params.Free;
    end;
  except
    on e : Exception do
    begin
      logt('Auth Exception: '+e.Message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TAuth2Server.HandleSkype(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  conn : TKDBConnection;
  token, id, name, email, password, domain : String;
begin
  domain := request.Host;
  if domain.Contains(':') then
    domain := domain.Substring(0, domain.IndexOf(':'));
  conn := FFhirStore.DB.GetConnection('OAuth2');
  try
    if params.getVar('form') <> '' then
    begin
      token := checkNotEmpty(params.GetVar('token'), 'token');
      id := checkNotEmpty(params.GetVar('id'), 'id');
      name := checkNotEmpty(params.GetVar('name'), 'name');
      email := checkNotEmpty(params.GetVar('email'), 'email');
      password := checkNotEmpty(params.GetVar('password'), 'password');

      if FIni.ReadString('admin', 'password', '') <> password then
        raise Exception.Create('Admin Password fail');

      // update the login record
      // create a session
      session := FFhirStore.RegisterSession(apInternal, '', token, id, name, email, '', inttostr(24*60), AContext.Binding.PeerIP, '');
      try
        conn.ExecSQL('Update OAuthLogins set Status = 2, SessionKey = '+inttostr(session.Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(token)+'''');
      finally
        session.Free;
      end;
      response.ContentText := 'done';
    end
    else if params.getVar('id') <> '' then
    begin
      if not FFhirStore.GetSessionByToken(params.GetVar('id'), session) then
        raise Exception.Create('State Error (1)');
      try
        if conn.CountSQL('Select Count(*) from OAuthLogins where Status = 2 and SessionKey = '+inttostr(session.Key)) <> 1 then
          raise Exception.Create('State Error (2)');
        setCookie(response, FHIR_COOKIE_NAME, session.Cookie, domain, '', session.Expires, false);
        response.Redirect('/oauth2/auth_choice');
      finally
        session.Free;
      end
    end
    else
      OnProcessFile(response, session, '/oauth2/auth_skype.html', AltFile('/oauth_skype.html'), true, nil);
    conn.Release;
  except
    on e:exception do
    begin
      response.ContentText := 'error: '+e.message;
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

function readFromScope(scope, name : String) : String;
var
  i : integer;
var
  list : TStringList;
begin
  result := '';
  list := TStringList.create;
  try
    list.CommaText := scope.Replace(' ', ',');
    for i := 0 to list.Count - 1 do
      if list[i].StartsWith(name+':') then
        result := list[i].Substring(name.Length+1)
  finally
    list.free;
  end;
end;

procedure TAuth2Server.HandleToken(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  code, clientId, clientSecret, uri, errCode : string;
  psecret, pclientid : String;
  conn : TKDBConnection;
  json : TJSONWriter;
  buffer : TAdvMemoryStream;
  launch, scope : String;
begin
  buffer := TAdvMemoryStream.Create;
  try
    try
      // first, we check the fixed value
      errCode := 'unsupported_grant_type';
      if params.getVar('grant_type') <> 'authorization_code' then
        raise Exception.Create('Invalid grant_type - must be authorization_code');

      // now, check the code
      errCode := 'invalid_request';
      code := checkNotEmpty(params.getVar('code'), 'code');
      if not FFhirStore.GetSessionByToken(code, session) then // todo: why is session passed in too?
        raise Exception.Create('Authorization Code not recognized');
      try
        conn := FFhirStore.DB.GetConnection('OAuth2');
        try
          conn.SQL := 'select Client, Redirect, Scope from OAuthLogins, Sessions where OAuthLogins.SessionKey = '+inttostr(session.key)+' and Status = 3 and OAuthLogins.SessionKey = Sessions.SessionKey';
          conn.prepare;
          conn.execute;
          if not conn.fetchnext then
            raise Exception.Create('Authorization Code not recognized (2)');

          pclientid := conn.ColStringByName['Client'];
          psecret := FIni.ReadString(pclientId, 'secret', '');

          // what happens now depends on whether there's a client secret or not
          if (psecret = '') then
          begin
            // user must supply the correct client id
            errCode := 'invalid_client';
            clientId := checkNotEmpty(params.getVar('client_id'), 'client_id');
            if clientId <> pclientid then
              raise Exception.Create('Client Id is wrong ("'+clientId+'") is wrong in parameter');
          end
          else
          begin
            // client id and client secret must be in the basic header. Check them
            clientId := request.AuthUsername;
            clientSecret := request.AuthPassword;
            if clientId <> pclientid then
              raise Exception.Create('Client Id is wrong ("'+clientId+'") in Authorization Header');
            if clientSecret <> psecret then
              raise Exception.Create('Client Secret in Authorization header is wrong ("'+clientSecret+'")');
          end;

          // now check the redirect URL
          uri := checkNotEmpty(params.getVar('redirect_uri'), 'redirect_uri');
          errCode := 'invalid_request';
          if conn.ColStringByName['Redirect'] <> uri then
            raise Exception.Create('Mismatch between claimed and actual redirection URIs');

          // ok, well, it's passed.
          scope := conn.ColStringByName['Scope'];
          launch := readFromScope(scope, 'launch');
          conn.terminate;

          conn.ExecSQL('Update OAuthLogins set Status = 4, DateTokenAccessed = '+DBGetDate(conn.owner.platform)+' where Id = '''+session.OuterToken+'''');

          json := TJsonWriter.create;
          try
            json.Stream := buffer.link;
            json.Start;
            json.Value('access_token', session.Cookie);
            json.Value('token_type', 'Bearer');
            json.Value('expires_in', inttostr(trunc((session.Expires - now) / DATETIME_SECOND_ONE)));
            json.Value('id_token', session.JWTPacked);
            json.Value('scope', scope);
            json.Value('patient', launch);
            json.Finish;
          finally
            json.Free;
          end;
          conn.Release;
        except
          on e:exception do
          begin
            conn.Error(e);
            recordStack(e);
            raise;
          end;
        end;
      finally
        session.free;
      end;
      response.ResponseNo := 200;
    except
      on e : Exception do
      begin
        response.ResponseNo := 500;
        json := TJsonWriter.create;
        try
          json.Stream := buffer.link;
          json.Start;
          json.Value('error', errCode);
          json.Value('error_description', e.Message);
          json.Finish;
        finally
          json.Free;
        end;
      end;
    end;
    response.ContentText := buffer.Buffer.AsUnicode;
    response.WriteContent;
  finally
    buffer.Free;
  end;
end;

procedure TAuth2Server.HandleTokenData(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  token, clientId, clientSecret : string;
  json : TJSONWriter;
  buffer : TAdvMemoryStream;
  check : boolean;
begin
  buffer := TAdvMemoryStream.Create;
  try
    try
      if request.AuthUsername <> 'Bearer' then
        raise Exception.Create('OAuth2 Access Token is required in the HTTP Authorization Header (type Bearer)');
      token := checkNotEmpty(params.getVar('token'), 'token');
      if request.AuthPassword <> token then
        raise Exception.Create('Access Token Required');

      clientId := checkNotEmpty(params.getVar('client_id'), 'client_id');
      clientSecret := checkNotEmpty(params.getVar('client_secret'), 'client_secret');

      if FIni.ReadString(clientId, 'secret', '') <> clientSecret then
        raise Exception.Create('Client Id or secret is wrong ("'+clientId+'")');

      if not FFhirStore.GetSession(token, session, check) then
      begin
        json := TJsonWriter.create;
        try
          json.Stream := buffer.link;
          json.Start;
          json.Value('active', false);
          json.Finish;
        finally
          json.Free;
        end;
      end
      else
      try
        json := TJsonWriter.create;
        try
          json.Stream := buffer.link;
          json.Start;
          json.Value('active', true);
          json.Value('token_type', 'Bearer');
          json.Value('exp', inttostr(trunc((session.Expires - EncodeDate(1970, 1, 1)) / DATETIME_SECOND_ONE)));
          json.Value('iat', inttostr(trunc((session.FirstCreated - EncodeDate(1970, 1, 1)) / DATETIME_SECOND_ONE)));
          json.Value('scope', session.scopes);
          json.Value('use_count', inttostr(session.useCount));
          if session.canGetUser then
          begin
            json.Value('user_id', Names_TFHIRAuthProvider[session.Provider]+':'+session.id);
            json.Value('user_name', session.Name);
            if session.Email <> '' then
              json.Value('email', session.Email);
          end;
          json.Finish;
        finally
          json.Free;
        end;
      finally
        session.free;
      end;
      response.ResponseNo := 200;
    except
      on e : Exception do
      begin
        json := TJsonWriter.create;
        try
          json.Stream := buffer.link;
          json.Start;
          json.Value('error', 'invalid_client');
          json.Value('error_description', e.Message);
          json.Finish;
        finally
          json.Free;
        end;
      end;
    end;
    response.ContentText := buffer.Buffer.AsUnicode;
    response.WriteContent;
  finally
    buffer.Free;
  end;
end;


procedure TAuth2Server.HandleUserDetails(AContext: TIdContext; request: TIdHTTPRequestInfo; session: TFhirSession; params: TParseMap; response: TIdHTTPResponseInfo);
var
  variables : TDictionary<String,String>;
begin
  if session = nil then
    response.Redirect('/oauth2/auth?client_id=web&response_type=code&scope=openid%20profile%20user/*.*%20'+SCIM_ADMINISTRATOR+'&redirect_uri='+EndPoint+'/internal&aud='+EndPoint+'&state='+MakeLoginToken(EndPoint, apGoogle))
  else
  begin
    if params.getVar('form') = 'true' then
    begin
      raise Exception.Create('Not done yet');
    end
    else
    begin
      variables := TDictionary<String,String>.create;
      try
        variables.Add('username', session.User.username);
        OnProcessFile(response, session, '/oauth_userdetails.html', AltFile('/oauth_userdetails.html'), true, variables)
      finally
        variables.free;
      end;
    end;
  end;
end;

function TAuth2Server.AuthPath: String;
begin
  result := '/oauth2/auth';
end;

function TAuth2Server.BuildLoginList(id : String): String;
var
  authurl : String;
  path : String;
begin
  path := '/oauth2/auth_dest';
  if FSSLPort = '443' then
    authurl := 'https://'+FHost+path
  else
    authurl := 'https://'+FHost+':'+FSSLPort+path;

  result := '';
  if FHL7Appid <> '' then
    result := result +
      '<li><a href="http://hl7.amg-hq.net/tools/signup_redirect.cfm?apiKey='+FHL7Appid+'&returnURL='+EncodeMime(authurl+'/state/'+MakeLoginToken(id, apHL7))+'">Use your HL7 website account</a></li>'+#13#10;

  if FFacebookAppid <> '' then
    result := result +
      '<li><a href="https://www.facebook.com/dialog/oauth?client_id='+FFacebookAppid+'&redirect_uri='+authurl+'&state='+MakeLoginToken(id, apFacebook)+'">Login through Facebook</a></li>'+#13#10;

  if FGoogleAppid <> '' then
    result := result +
      '<li><a href="https://accounts.google.com/o/oauth2/auth?client_id='+FGoogleAppid+'&response_type=code&scope=openid%20email&redirect_uri='+authurl+'&state='+MakeLoginToken(id, apGoogle)+'">Login through Google</a></li>'+#13#10;

  result := result +'<li>Authenticate to the Server Administrator directly using Skype (token = '+id+'),<br/> then <a href="/oauth2/auth_skype?id='+id+'">click here</a></li>'+#13#10;
  result := result +
    '<li>Or login directly (if you have an account): <form method="POST" action="'+path+'">'+
    '<input type="hidden" name="id" value="'+id+'"/>'+
    'Username <input type="text" name="username"/> <br/>'+
    'Password <input type="password" name="password"/> <br/>'+
    '<input type="submit" value="Login"/>'+
    '</form></li>'+#13#10;

end;

function TAuth2Server.CheckLoginToken(state: string; var original : String; var provider : TFHIRAuthProvider): Boolean;
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

end.
