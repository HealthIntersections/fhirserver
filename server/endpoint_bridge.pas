unit endpoint_bridge;

{$i fhir.inc}

{
This unit shows an example of how to add a FHIR Server into another
application server. It instantiates a FHIR Server, and provides storage
to allow the server to expose underlying application functionality e.g. from a
relational database.

This example FHIR server uses some CSV files to provide somewhat meaningful
services. The intent is that you replace the CSV routines with functionality
from your own application (todo: move to database)

There are 3 different classes that you must subclass to implement an end-point:

TFHIRUserProvider
  - provides user information. At a minimum, you must provide user information about anonymous sessions.
  - if you want to support OAuth (smart on fhir) additional user work is required. OAuth users are defined by the application

TFHIRStorageService
  - provides persistent storage services for the server as a whole
  - as well FetchResourceCounts, you must implement createOperationContext/Yield, which provide you Operation Engine

TFHIROperationEngine
  - responds to the actual service calls from clients
  - note that this runs in the contexts of an Indy HTTP server thread
  - you override any of the execute* methods to provide the functionality you want
  - you also must override the transaction methods, though you are not required to
  - do anything with them

Bridge Database

The bridge database is very simple:

Patient:
  PatientKey int Pimary Key
  VersionId int
  LastModified dateTime
  Active  int (0 or 1)
  MRN String[20]
  Surname String[60]
  First String[60]
  Middle String[60]
  Gender  String[1]
  Birthdate date
}

interface

Uses
  SysUtils, StrUtils, Classes, IniFiles,
  fsl_base, fsl_utilities, fsl_collections, fsl_threads, fsl_stream, fsl_json, fsl_http,
  fdb_manager, fdb_dialects,
  ftx_ucum_services,
  fhir_objects,  fhir_validator, fhir_factory, fhir_pathengine, fhir_utilities, fhir_common, fsl_scim,

  // change which version is implemented by changing these imports
  fhir3_types, fhir3_resources, fhir3_constants, fhir3_utilities, fhir3_factory, fhir3_pathengine,
  fhir3_validator, fhir3_indexinfo, validator_r3,

  fhir_indexing,
  server_factory, indexing, subscriptions, session, user_manager, server_config,
  server_context, storage, utilities, tx_manager,
  telnet_server,
  web_base, endpoint, endpoint_storage;


Const
  SYSTEM_ID = 'http://example.org/mrn-id';

Type
  TBridgeEndPoint = class;

 TExampleServerFactory = class (TFHIRServerFactory)
  public
    function makeIndexes : TFHIRIndexBuilder; override;
    function makeValidator: TFHIRValidatorV; override;
    function makeIndexer : TFHIRIndexManager; override;
    function makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager; override;
    function makeEngine(validatorContext : TFHIRWorkerContextWithFactory; ucum : TUcumServiceImplementation) : TFHIRPathEngineV; override;

    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); override;
  end;

  TExampleFhirServerStorage = class;

  TExampleFHIROperationEngine = class (TFHIROperationEngine)
  private
    FConnection : TFDBConnection;

    function loadPatient(key : integer) : TFHIRPatient;

    function patientCreate(request: TFHIRRequest; response : TFHIRResponse) : String;
    procedure patientUpdate(request: TFHIRRequest; response : TFHIRResponse);
    function patientRead(request: TFHIRRequest; response : TFHIRResponse) : boolean;
    function CreatePatientRecord(key: integer; pat: TFHIRPatient): TFHIRPatient;
  protected
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    procedure processGraphQL(graphql: String; request : TFHIRRequest; response : TFHIRResponse); override;

    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    function ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String; override;
    function ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; override;
  public
    constructor Create(storage : TExampleFhirServerStorage; serverContext : TFHIRServerContext; Conn : TFDBConnection; const lang : THTTPLanguages);

    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; override;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResourceV; override;
    function getResourceByUrl(aType : string; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResourceV; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    function patientIds(request : TFHIRRequest; res : TFHIRResourceV) : TArray<String>; override;
  end;

  TExampleFhirServerStorage = class (TFHIRStorageService)
  private
    FDataBase : TFDBManager;
    FServerContext : TFHIRServerContext; // not linked
  protected
    function GetTotalResourceCount: integer; override;
  public
    constructor Create(factory : TFHIRFactory); override;
    destructor Destroy; override;
    function Link : TExampleFhirServerStorage; overload;

    // no OAuth Support

    // server total counts:
    procedure FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList); override;

    procedure RecordFhirSession(session: TFhirSession); override;
    procedure CloseFhirSession(key: integer); override;
    procedure QueueResource(session : TFHIRSession; r: TFhirResourceV); overload; override;
    procedure QueueResource(session : TFHIRSession; r: TFhirResourceV; dateTime: TFslDateTime); overload; override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;

    function ProfilesAsOptionList : String; override;

    procedure ProcessSubscriptions; override;
    procedure ProcessObservations; override;
    procedure RunValidation; override;

    function createOperationContext(const lang : THTTPLanguages) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); override;

    procedure Sweep; override;
    function RetrieveSession(key : integer; var UserKey, Provider : integer; var Id, Name, Email : String) : boolean; override;
    procedure ProcessEmails; override;
    function FetchResource(key : integer) : TFHIRResourceV; override;
    function getClientInfo(id : String) : TRegisteredClientInformation; override;
    function getClientName(id : String) : string; override;
    function storeClient(client : TRegisteredClientInformation; sessionKey : integer) : String; override;
    procedure fetchClients(list : TFslList<TRegisteredClientInformation>); override;
    function loadPackages : TFslMap<TLoadedPackageInformation>; override;
    function fetchLoadedPackage(id : String) : TBytes; override;
    procedure recordPackageLoaded(id, ver : String; count : integer; blob : TBytes); override;

    Procedure SetUpRecording(session : TFhirSession); override;
    procedure RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception); override;
    procedure FinishRecording(); override;
  end;

  TExampleFHIRUserProvider = class (TFHIRUserProvider)
  public
    Function loadUser(key : integer) : TSCIMUser; overload; override;
    Function loadUser(id : String; var key : integer) : TSCIMUser; overload; override;
    function CheckLogin(username, password : String; var key : integer) : boolean; override;
    function CheckId(id : String; var username, hash : String) : boolean; override;
    function loadOrCreateUser(id, name, email : String; var key : integer) : TSCIMUser; override;
    function allowInsecure : boolean; override;
  end;

  TBridgeWebServer = class (TStorageWebEndpoint)
  private
    FBridge : TBridgeEndPoint;
  protected

    Function BuildFhirHomePage(compList : TFslList<TFHIRCompartmentId>; logId : String; const lang : THTTPLanguages; host, sBaseURL: String; Session: TFHIRSession; secure: boolean): String; override;
    Function BuildFhirUploadPage(const lang : THTTPLanguages; host, sBaseURL: String; aType: String; Session: TFHIRSession): String; override;
    Function BuildFhirAuthenticationPage(const lang : THTTPLanguages; host, path, logId, Msg: String; secure: boolean; params : String): String; override;
    function HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime; override;
    procedure GetWebUILink(resource: TFhirResourceV; base, statedType, id, ver: String; var link, text: String); override;
    Function ProcessZip(const lang : THTTPLanguages; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerConfigFile; Context: TOperationContext; var cursor: integer): TFHIRBundleW; override;
    function DoSearch(Session: TFHIRSession; rtype: string; const lang : THTTPLanguages; params: String): TFHIRBundleW; override;
  public
    destructor Destroy; override;
    function link : TBridgeWebServer; overload;
    function description : String; override;
  end;

  TBridgeEndPoint = class (TStorageEndPoint)
  private
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
    destructor Destroy; override;
    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;

    procedure Load; override;
    procedure Unload; override;
    procedure InstallDatabase; override;
    procedure UninstallDatabase; override;
    procedure LoadPackages(plist : String); override;
    procedure updateAdminPassword; override;
    procedure internalThread; override;
    function cacheSize : UInt64; override;
    procedure clearCache; override;
  end;

implementation

{ TBridgeEndPoint }

function TBridgeEndPoint.cacheSize: UInt64;
begin
  result := inherited cacheSize;
end;

procedure TBridgeEndPoint.clearCache;
begin
  inherited;
end;

constructor TBridgeEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies);
begin
  inherited create(config, settings, db, common);
end;

destructor TBridgeEndPoint.Destroy;
begin
  inherited;
end;

procedure TBridgeEndPoint.internalThread;
begin
  // nothing
end;

procedure TBridgeEndPoint.Load;
var
  s : String;
  store : TExampleFhirServerStorage;
begin
  s := Config['data-path'].value;
  if (s = '') then
    s := SystemTemp;
  store := TExampleFhirServerStorage.create(TFHIRFactoryR3.create);
  store.FDataBase := Database.Link;
  FServerContext := TFHIRServerContext.Create(Config.name, store, TExampleServerFactory.create);
  store.FServerContext := FServerContext;
  FServerContext.Globals := Settings.Link;
  FServerContext.userProvider := TExampleFHIRUserProvider.Create;
end;

procedure TBridgeEndPoint.LoadPackages(plist: String);
begin
  raise Exception.Create('This is not supported by the bridge end point');
end;

procedure TBridgeEndPoint.InstallDatabase;
var
  conn : TFDBConnection;
begin
  conn := Database.GetConnection('install');
  try
    conn.ExecSQL('CREATE TABLE Config ('+#13#10+
       ' ConfigKey '+DBKeyType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, False)+',  '+#13#10+
       ' Value nchar(200) '+ColCanBeNull(conn.owner.platform, False)+') '+CreateTableInfo(conn.owner.platform));
    conn.ExecSQL('Create INDEX SK_Config_ConfigKey ON Config (ConfigKey)');
    conn.ExecSQL('Insert into Config (ConfigKey, Value) values (100, ''bridge||Installed '+TFslDateTime.makeLocal.toString+''')');

    conn.ExecSQL('CREATE TABLE Patients ('+#13#10+
       ' PatientKey '+DBKeyType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, False)+','+#13#10+
       ' VersionId int '+ColCanBeNull(conn.owner.platform, False)+','+#13#10+
       ' LastModified '+DBDateTimeType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, False)+','+#13#10+
       ' Active int '+ColCanBeNull(conn.owner.platform, False)+','+#13#10+
       ' MRN nchar(20) '+ColCanBeNull(conn.owner.platform, False)+','+#13#10+
       ' Surname nchar(60) '+ColCanBeNull(conn.owner.platform, False)+','+#13#10+
       ' First nchar(60) '+ColCanBeNull(conn.owner.platform, False)+','+#13#10+
       ' Middle nchar(60) '+ColCanBeNull(conn.owner.platform, True)+','+#13#10+
       ' Gender int '+ColCanBeNull(conn.owner.platform, True)+','+#13#10+
       ' Birthdate '+DBDateTimeType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, True)+#13#10+
       PrimaryKeyType(conn.owner.Platform, 'PK_Patients', 'PatientKey')+') '+CreateTableInfo(conn.owner.platform));
    conn.ExecSQL('Create INDEX PK_MRN ON Patients (MRN)');
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

procedure TBridgeEndPoint.UninstallDatabase;
var
  conn : TFDBConnection;
  meta : TFDBMetaData;
begin
  conn := Database.GetConnection('uninstall');
  try
    meta := conn.FetchMetaData;
    try
      if meta.HasTable('Patients') then
        conn.DropTable('Patients');
      if meta.HasTable('Config') then
        conn.DropTable('Config');
    finally
      meta.Free;
    end;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

procedure TBridgeEndPoint.Unload;
begin
  FServerContext.Free;
  FServerContext := nil;
end;

procedure TBridgeEndPoint.updateAdminPassword;
begin
  raise Exception.Create('This is not supported the bridge end point');
end;

function TBridgeEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
var
  wep : TBridgeWebServer;
begin
  wep := TBridgeWebServer.Create(Config.name, Config['path'].value, common, self);
  wep.FBridge := self;
  WebEndPoint := wep;
  result := wep;
end;

function TBridgeEndPoint.summary: String;
begin
  result := 'Bridge Server using '+describeDatabase(Config);
end;

{ TExampleFHIROperationEngine }

constructor TExampleFHIROperationEngine.create(storage : TExampleFhirServerStorage; serverContext : TFHIRServerContext; Conn : TFDBConnection; const lang : THTTPLanguages);
begin
  inherited Create(storage, serverContext, lang);
  FConnection := Conn;
end;

procedure TExampleFHIROperationEngine.StartTransaction;
begin
  FConnection.StartTransact;
end;

procedure TExampleFHIROperationEngine.CommitTransaction;
begin
  FConnection.Commit;
end;

procedure TExampleFHIROperationEngine.RollbackTransaction;
begin
  FConnection.Rollback;
end;

procedure TExampleFHIROperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; httpCode: Integer; name, message: String; patients : TArray<String>);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFHIROperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; opName: String; httpCode: Integer; name, message: String; patients : TArray<String>);
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFHIROperationEngine.ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
begin
  if request.ResourceName = 'Patient' then
    result := patientCreate(request, response)
  else
    raise EFHIRException.create('The resource "'+request.ResourceName+'" is not supported by this server');
end;

function TExampleFHIROperationEngine.ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean;
begin
  if request.ResourceName = 'Patient' then
    result := patientRead(request, response)
  else
    raise EFHIRException.create('The resource "'+request.ResourceName+'" is not supported by this server');
end;


function TExampleFHIROperationEngine.ExecuteUpdate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): Boolean;
begin
  result := true;
  if request.ResourceName = 'Patient' then
    patientUpdate(request, response)
  else
    raise EFHIRException.create('The resource "'+request.ResourceName+'" is not supported by this server');
end;

function TExampleFHIROperationEngine.GetResourceById(request: TFHIRRequest; aType, id, base: String; var needSecure: boolean): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFHIROperationEngine.getResourceByUrl(aType: String; url, version: string; allowNil: boolean; var needSecure: boolean): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFHIROperationEngine.loadPatient(key: integer): TFHIRPatient;
begin
  FConnection.SQL := 'Select * from Patients where PatientKey = '+inttostr(key);
  FConnection.prepare;
  FConnection.Execute;
  if FConnection.FetchNext then
  begin
    result := TFHIRPatient.Create;
    try
      result.id := inttostr(key);
      result.meta := TFHIRMeta.Create;
      result.meta.versionId := FConnection.ColStringByName['VersionId'];
      result.meta.lastUpdated := FConnection.ColDateTimeExByName['LastModified'];
      with result.identifierList.Append do
      begin
        system := SYSTEM_ID;
        value := FConnection.ColStringByName['MRN'];
      end;
      with result.nameList.Append do
      begin
        family := FConnection.ColStringByName['Surname'];
        givenList.add(FConnection.ColStringByName['First']);
        if (FConnection.ColStringByName['Middle'] <> '') then
          givenList.add(FConnection.ColStringByName['Middle']);
      end;
      result.gender := TFhirAdministrativeGenderEnum(FConnection.ColIntegerByName['Gender']);
      if not FConnection.ColNullByName['Birthdate'] then
        result.birthDate := FConnection.ColDateTimeExByName['Birthdate'];
      result.active := FConnection.ColIntegerByName['Active'] = 1;
      result.Link;
    finally
      result.Free;
    end;
  end
  else
    result := nil;
  FConnection.Terminate;
end;

function TExampleFHIROperationEngine.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFHIROperationEngine.patientIds(request: TFHIRRequest; res: TFHIRResourceV): TArray<String>;
begin
  result := nil;
end;

function TExampleFHIROperationEngine.patientCreate(request: TFHIRRequest; response: TFHIRResponse) : String;
var
  pat : TFHIRPatient;
begin
  pat := request.Resource as TFHIRPatient;
  response.Resource := CreatePatientRecord(0, pat);
  result := response.Resource.id;
  response.id := result;
  response.HTTPCode := 201;
  response.Message := 'Created';
  response.Location := request.baseUrl+'Patient/'+result+'/_history/1';
  response.LastModifiedDate := TFHIRPatient(response.Resource).meta.lastUpdated.DateTime;
end;

function TExampleFHIROperationEngine.CreatePatientRecord(key : integer; pat : TFHIRPatient) : TFHIRPatient;
var
  id : TFhirIdentifier;
begin
  id := pat.identifierList.BySystem(SYSTEM_ID);
  if (id = nil) or (id.value = '') then
    raise ERestfulException.Create('CreatePatientRecord', 422, itRequired, 'A MRN (system = "http://example.org/mrn-id") is required', lang);
  if FConnection.CountSQL('Select count(*) from Patients where MRN = '+id.value) > 0 then
    raise ERestfulException.Create('CreatePatientRecord', 422, itRequired, 'Duplicate MRNs are not allowed ('+id.value+')', lang);
  if (pat.nameList.IsEmpty) then
    raise ERestfulException.Create('CreatePatientRecord', 422, itRequired, 'A name is required', lang);
  if (pat.nameList[0].family = '') then
    raise ERestfulException.Create('CreatePatientRecord', 422, itRequired, 'A family name is required', lang);
  if (pat.nameList[0].givenList.isEmpty) then
    raise ERestfulException.Create('CreatePatientRecord', 422, itRequired, 'A given name is required', lang);

  if (key = 0) then
    key := FConnection.CountSQL('Select Max(PatientKey) from Patients')+1;

  FConnection.sql := 'insert into Patients (PatientKey, VersionId, LastModified, Active, MRN, Surname, First, Middle, Gender, Birthdate) '
                                  +'values (:pk,        :v,      :lm,          :a,    :mrn, :s, :f, :m, :g, :b)';
  FConnection.Prepare;
  FConnection.BindInteger('pk', key);
  FConnection.BindInteger('v', 1);
  FConnection.BindDateTimeEx('lm', TFslDateTime.makeUTC);
  FConnection.BindIntegerFromBoolean('a', (pat.activeElement = nil) or pat.active);
  FConnection.BindString('mrn', id.value);
  FConnection.BindString('s', pat.nameList[0].family);
  FConnection.BindString('f', pat.nameList[0].givenList[0].value);
  if (pat.nameList[0].givenList.Count > 1) then
    FConnection.BindString('m', pat.nameList[0].givenList[1].value)
  else
    FConnection.BindNull('m');
  FConnection.BindInteger('g', ord(pat.gender));
  if pat.birthDate.notNull then
    FConnection.BindDateTimeEx('b', pat.birthDate)
  else
    FConnection.BindNull('m');
  FConnection.Execute;
  FConnection.Terminate;

  result := loadPatient(key);
end;

procedure TExampleFHIROperationEngine.patientUpdate(request: TFHIRRequest; response: TFHIRResponse);
var
  key : integer;
  patClient, patServer : TFHIRPatient;
  sql : String;
  id : TFhirIdentifier;
begin
  patClient := request.Resource as TFHIRPatient;

  id := patClient.identifierList.BySystem(SYSTEM_ID);

  if not StringIsInteger32(patClient.id) then
    raise ERestfulException.Create('TExampleFHIROperationEngine.dataFromPatient', 422, itRequired, 'Paitent ID must be a 32bit integer', lang);
  key := StrToInt(patClient.id);

  patServer := nil;
  try
    // upsert support
    if FConnection.CountSQL('Select count(PatientKey) from Patients where PatientKey = '+inttostr(key)) = 0 then
      patServer := CreatePatientRecord(key, patClient)
    else if (id <> nil) and (FConnection.Lookup('Patients', 'PatientKey', patClient.id, 'MRN', '$$') <> id.value) then
      raise ERestfulException.Create('patientUpdate', 422, itRequired, 'The Patient''s MRN (system = "http://example.org/mrn-id") cannot be changed once created', lang)
    else
    begin
      sql := 'update Patients set VersionId = VersionId + 1, LastModified = :lm';
      if patClient.activeElement <> nil then
        CommaAdd(sql, 'Active= :a');
      if patClient.nameList.Count > 0 then
      begin
        if patClient.nameList[0].family <> '' then
           CommaAdd(sql, 'Surname = :s');
        if patClient.nameList[0].givenList.count > 0 then
           CommaAdd(sql, 'First =:f');
        if patClient.nameList[0].givenList.count > 1 then
           CommaAdd(sql, 'Middle = :m');
      end;
      if patClient.genderElement <> nil then
        CommaAdd(sql, 'Gender = :g');
      if patClient.birthDate.notNull then
        CommaAdd(sql, 'Birthdate = :b');
      FConnection.SQL := sql+' where PatientKey = '+inttostr(key);
      FConnection.Prepare;
      FConnection.BindDateTimeEx('lm', TFslDateTime.makeUTC);
      if patClient.activeElement <> nil then
        FConnection.BindIntegerFromBoolean('a', patClient.active);
      if patClient.nameList.Count > 0 then
      begin
        if patClient.nameList[0].family <> '' then
          FConnection.BindString('s', patClient.nameList[0].family);
        if patClient.nameList[0].givenList.count > 0 then
          FConnection.BindString('f', patClient.nameList[0].givenList[0].value);
        if patClient.nameList[0].givenList.count > 1 then
          FConnection.BindString('m', patClient.nameList[0].givenList[1].value)
      end;
      if patClient.genderElement <> nil then
        FConnection.BindInteger('g', ord(patClient.gender));
      if patClient.birthDate.notNull then
        FConnection.BindDateTimeEx('b', patClient.birthDate);
      FConnection.Execute;
      FConnection.Terminate;
      patServer := loadPatient(key);
    end;

    response.Resource := patServer.Link;
    response.Id := request.Id;
    response.versionId := patServer.meta.versionId;
    response.LastModifiedDate := patServer.meta.lastUpdated.DateTime;
    response.HTTPCode := 200;
    response.Message := 'OK';
    response.Location := request.baseUrl+'Patient/'+request.id+'/_history/'+response.versionId;
  finally
    patServer.Free;
  end;
end;

procedure TExampleFHIROperationEngine.processGraphQL(graphql: String; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFHIROperationEngine.patientRead(request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  pat : TFhirPatient;
begin
  result := false;
  if not StringIsInteger32(request.Id) then
  begin
    response.HTTPCode := 404;
    response.Message := 'Not Found';
    response.Resource := BuildOperationOutcome(lang, 'not found', IssueTypeUnknown);
  end
  else
  begin
    pat := loadPatient(StrToInt(request.Id));
    if pat <> nil then
    begin
      response.Resource := pat;
      response.HTTPCode := 200;
      response.Message := 'OK';
      result := true;
    end
    else
    begin
      response.HTTPCode := 404;
      response.Message := 'Not Found';
      response.Resource := BuildOperationOutcome(lang, 'not found', IssueTypeUnknown);
    end;
  end;
end;

{ TExampleFhirServerStorage }

constructor TExampleFhirServerStorage.Create(factory : TFHIRFactory);
begin
  inherited Create(factory);
end;

destructor TExampleFhirServerStorage.Destroy;
begin
  FDatabase.Free;
  inherited;
end;

procedure TExampleFhirServerStorage.fetchClients(list: TFslList<TRegisteredClientInformation>);
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFhirServerStorage.fetchLoadedPackage(id: String): TBytes;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception);
begin
end;

procedure TExampleFhirServerStorage.RecordFhirSession(session: TFhirSession);
begin
  // this server doesn't track sessions
end;

procedure TExampleFhirServerStorage.recordPackageLoaded(id, ver: String; count: integer; blob: TBytes);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.CloseFhirSession(key: integer);
begin
  // this server doesn't track sessions
end;

function TExampleFhirServerStorage.createOperationContext(const lang : THTTPLanguages): TFHIROperationEngine;
begin
  result := TExampleFHIROperationEngine.create(self.Link, FServerContext.link, FDataBase.GetConnection('operation'), lang);
end;


function TExampleFhirServerStorage.FetchResource(key: integer): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList);
begin
  counts.AddObject('Patient', TObject(FDataBase.CountSQL('Select count(*) from Patient', 'count')));
end;

procedure TExampleFhirServerStorage.FinishRecording;
begin
  inherited;
end;

function TExampleFhirServerStorage.getClientInfo(id: String): TRegisteredClientInformation;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFhirServerStorage.getClientName(id: String): string;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFhirServerStorage.GetTotalResourceCount: integer;
begin
  result := FDataBase.CountSQL('Select count(*) from Patient', 'count');
end;

function TExampleFhirServerStorage.Link: TExampleFhirServerStorage;
begin
  result := TExampleFhirServerStorage(inherited link);
end;

function TExampleFhirServerStorage.loadPackages: TFslMap<TLoadedPackageInformation>;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.ProcessEmails;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.ProcessObservations;
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.ProcessSubscriptions;
begin
  // nothing in this server
end;

function TExampleFhirServerStorage.ProfilesAsOptionList: String;
begin
  result := '';
end;

procedure TExampleFhirServerStorage.QueueResource(session : TFHIRSession; r: TFhirResourceV);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.QueueResource(session : TFHIRSession; r: TFhirResourceV; dateTime: TFslDateTime);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.RegisterAuditEvent(session: TFhirSession; ip: String);
begin
  // nothing in this server
end;

function TExampleFhirServerStorage.RetrieveSession(key: integer; var UserKey, Provider: integer; var Id, Name, Email: String): boolean;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.RunValidation;
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.SetupRecording(session: TFhirSession);
begin
end;

function TExampleFhirServerStorage.storeClient(client: TRegisteredClientInformation; sessionKey: integer): String;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.Sweep;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.Yield(op: TFHIROperationEngine; exception: Exception);
var
  eop : TExampleFHIROperationEngine;
begin
  eop := op as TExampleFHIROperationEngine;
  try
    if exception <> nil then
      eop.FConnection.Error(exception)
    else
      eop.FConnection.Release;
  finally
    op.Free;
  end;
end;

{ TExampleFHIRUserProvider }

function TExampleFHIRUserProvider.allowInsecure: boolean;
begin
  result := true;
end;

function TExampleFHIRUserProvider.CheckId(id: String; var username, hash: String): boolean;
begin
  if (id = 'user') then
  begin
    result := false;
    userName := 'Registered User';
    hash := inttostr(HashStringToCode32('Password'));
  end
  else
    result := false;
end;

function TExampleFHIRUserProvider.CheckLogin(username, password: String; var key : integer): boolean;
begin
  result := (username = 'user') and (HashStringToCode32('Password') = HashStringToCode32(password));
  if result then
    Key := 1;
end;

function TExampleFHIRUserProvider.loadOrCreateUser(id, name, email: String; var key: integer): TSCIMUser;
begin
  key := 1;
  result := loadUser(key);
end;

function TExampleFHIRUserProvider.loadUser(key: integer): TSCIMUser;
begin
  result := TSCIMUser.Create(TJsonObject.Create);
  result.userName := 'Registered User';
  result.formattedName := 'Registered User';
end;

function TExampleFHIRUserProvider.loadUser(id: String; var key: integer): TSCIMUser;
begin
  key := 1;
  result := LoadUser(key);
end;


{ TExampleServerFactory }

function TExampleServerFactory.makeEngine(validatorContext: TFHIRWorkerContextWithFactory; ucum: TUcumServiceImplementation): TFHIRPathEngineV;
begin
  raise EFslException.Create('Not supported in this server');
end;

function TExampleServerFactory.makeIndexer: TFHIRIndexManager;
begin
  raise EFslException.Create('Not supported in this server');
end;

function TExampleServerFactory.makeIndexes: TFHIRIndexBuilder;
begin
  result := TFHIRIndexBuilderR3.create;
end;

function TExampleServerFactory.makeSubscriptionManager(ServerContext: TFslObject): TSubscriptionManager;
begin
  raise EFslException.Create('Not supported in this server');
end;

function TExampleServerFactory.makeValidator: TFHIRValidatorV;
begin
  result := TFHIRValidator3.Create(TFHIRServerWorkerContextR3.Create(TFHIRFactoryR3.create));
end;

procedure TExampleServerFactory.setTerminologyServer(validatorContext: TFHIRWorkerContextWithFactory; server: TFslObject);
begin
  raise EFslException.Create('Not supported in this server');
end;

{ TBridgeWebServer }

destructor TBridgeWebServer.Destroy;
begin
  // nothing
  inherited;
end;

function TBridgeWebServer.BuildFhirAuthenticationPage(const lang: THTTPLanguages; host, path, logId, Msg: String; secure: boolean; params: String): String;
begin
  raise Exception.Create('Authentication is not supported for this endpoint');
end;

function TBridgeWebServer.BuildFhirHomePage(compList: TFslList<TFHIRCompartmentId>; logId: String; const lang: THTTPLanguages; host, sBaseURL: String; Session: TFHIRSession; secure: boolean): String;
begin
  result := processContent('template-fhir.html', secure, 'Bridge Home Page', 'Home Page');
end;

function TBridgeWebServer.BuildFhirUploadPage(const lang: THTTPLanguages; host, sBaseURL, aType: String; Session: TFHIRSession): String;
begin
  raise Exception.Create('Upload is not supported for this endpoint');
end;

function TBridgeWebServer.description: String;
begin
  result := 'Bridge, with data  '+FBridge.Database.DBDetails;
end;

function TBridgeWebServer.DoSearch(Session: TFHIRSession; rtype: string; const lang: THTTPLanguages; params: String): TFHIRBundleW;
begin
  raise Exception.Create('todo?');
end;

procedure TBridgeWebServer.GetWebUILink(resource: TFhirResourceV; base, statedType, id, ver: String; var link, text: String);
begin
  raise Exception.Create('WebUI is not supported for this endpoint');
end;

function TBridgeWebServer.HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
begin
  raise Exception.Create('WebUI is not supported for this endpoint');
end;

function TBridgeWebServer.link: TBridgeWebServer;
begin
  result := TBridgeWebServer(inherited link);
end;

function TBridgeWebServer.ProcessZip(const lang: THTTPLanguages; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerConfigFile; Context: TOperationContext; var cursor: integer): TFHIRBundleW;
begin
  raise Exception.Create('Upload is not supported for this endpoint');
end;

end.
