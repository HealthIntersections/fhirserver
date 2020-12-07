unit storage;

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

{$I fhir.inc}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_threads, fsl_utilities, fsl_stream, fsl_collections, fsl_logging, fsl_json,
  fsl_http,
  fdb_dialects, fsl_graphql,
  fhir_objects,  fhir_common, fhir_xhtml, fhir_parser, fhir_factory, fhir_utilities, fhir_pathengine,
  fhir_client, fhir_cdshooks,
  session,
  fhir_indexing, fhir_graphql,
  html_builder, subscriptions, utilities, server_constants, indexing, bundlebuilder,
  client_cache_manager;

Type
  TAsyncTaskStatus = (atsCreated, atsWaiting, atsProcessing, atsComplete, atsAborted, atsTerminated, atsError, atsDeleted);

  TPopulateConformanceEvent = procedure (sender : TObject; conf : TFhirCapabilityStatementW; secure : boolean; baseUrl : String; caps : Array of String) of object;

  TOperationMode = (opmRestful, opmUpload, opmInternal, opmCmdLine, opmSweep);
  TOperationLoggingLevel = (ollNone, ollHuman, ollInstaller);

  TOperationContext = class (TFslObject)
  private
    FMode : TOperationMode;
    FLogging : TOperationLoggingLevel;
    FCacheResponse : boolean;
    FInTransaction: boolean;
  public
    constructor Create; overload; override;
    constructor Create(mode : TOperationMode; logDetails : TOperationLoggingLevel); overload;
    constructor Create(mode : TOperationMode); overload;

    property mode : TOperationMode read FMode write FMode;
    property Logging : TOperationLoggingLevel read FLogging write FLogging;
    property CacheResponse : boolean read FCacheResponse write FCacheResponse;
    property inTransaction : boolean read FInTransaction write FInTransaction;
  end;

const
  OP_MODES_CHECK = [opmRestful, opmInternal];
  OP_CODES_NO_SEC_ON_INSERT = [opmSweep];

type
  TFHIRStorageService = class;
  TFHIROperationEngine = class;


  TFhirOperation = class abstract (TFslObject)
  protected
    FFactory : TFHIRFactory;
    function resolvePatient(manager: TFHIROperationEngine; request: TFHIRRequest; ref : String) : integer;
    function CreateBaseDefinition(base : String) : TFHIROperationDefinitionW;
    function isWrite : boolean; virtual;
    function owningResource : String; virtual; // for security purposes
    function makeParams(request : TFHIRRequest) : TFhirParametersW;

  public
    constructor Create(factory : TFHIRFactory);
    destructor Destroy; override;
    function Name : String; virtual;
    function Types : TArray<String>; virtual;
    function HandlesRequest(request : TFHIRRequest) : boolean; virtual;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; virtual;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse) : String; virtual;
    function formalURL : String; virtual;
  end;

  TMatchingResource = class (TFslName)
  public
    key : integer;
    version : integer;
  end;

  TMatchingResourceList = class (TFslNameList)
  private
    function GetEntry(iIndex: Integer): TMatchingResource;
  public
    Property entries[iIndex : Integer] : TMatchingResource Read GetEntry; Default;
  end;

  TLoadedPackageInformation = class (TFslObject)
  private
    FId : String;
    FVer : String;
    FDate : TFslDateTime;
    FCount: integer;
  public
    function link : TLoadedPackageInformation; overload;
    function summary : string;

    property id : String read FId write FId;
    property ver : String read FVer write FVer;
    property date : TFslDateTime read FDate write FDate;
    property count : integer read FCount write FCount;
  end;

  TAsyncTaskInformation = class (TFslObject)
  private
    FKey: integer;
    FNames: TArray<String>;
    FFormat: TFHIRFormat;
  public
    function link : TAsyncTaskInformation; overload;
    property key : integer read FKey write FKey;
    property names : TArray<String> read FNames write FNames;
    property format : TFHIRFormat read FFormat write FFormat;
  end;

  TRegisteredClientMode = (rcmOAuthClient, rcmBackendServices);

  TRegisteredClientInformation = class (TFslObject)
  private
    FRedirects: TStringList;
    FMode: TRegisteredClientMode;
    FName: String;
    FJwt: String;
    FPublicKey: String;
    FSecret: string;
    FsoftwareVersion: String;
    Flogo: String;
    Furl: String;
    FsoftwareId: String;
    FIssuer: String;
    FPatientContext: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TRegisteredClientInformation; overload;

    property name : String read FName write FName;
    property jwt : String read FJwt write FJwt; // for application verification service
    property mode : TRegisteredClientMode read FMode write FMode;
    property secret : string read FSecret write FSecret;
    Property redirects : TStringList read FRedirects;
    property publicKey : String read FPublicKey write FPublicKey;
    property issuer : String read FIssuer write FIssuer;
    property url : String read Furl write Furl;
    property logo : String read Flogo write Flogo;
    property softwareId : String read FsoftwareId write FsoftwareId;
    property softwareVersion : String read FsoftwareVersion write FsoftwareVersion;
    property patientContext : boolean read FPatientContext write FPatientContext;
  end;

  TFindResourceOption = (froFindDeletedResource, froForCommit);

  TFindResourceOptions = set of TFindResourceOption;

  TTestScriptBuilder = class (TFslObject)
  public
  private
  end;

  TFHIROperationEngine = class (TFslObject)
  private
    FOnPopulateConformance : TPopulateConformanceEvent;
    FLang : THTTPLanguages;
    function GetClientCacheManager: TClientCacheManager;
  protected
    FServerContext : TFslObject;
    FOperations : TFslList<TFhirOperation>;
    FEngine : TFHIRPathEngineV;
    FStorage : TFHIRStorageService;
    function factory : TFHIRFactory;
    procedure processGraphQL(graphql: String; request : TFHIRRequest; response : TFHIRResponse); virtual; abstract;

    procedure StartTransaction; virtual; abstract;
    procedure CommitTransaction; virtual; abstract;
    procedure RollbackTransaction; virtual; abstract;

    procedure ExecuteCapabilityStatement(request: TFHIRRequest; response : TFHIRResponse; full : boolean); virtual;
    procedure ExecuteTerminologyCapabilities(request: TFHIRRequest; response : TFHIRResponse); virtual;

    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; virtual;
    function  ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; virtual;
    function  ExecutePatch(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; virtual;
    procedure ExecuteVersionRead(request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteDelete(request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteHistory(request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse); virtual;
    Function  ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String; virtual;
    procedure ExecuteMetadata(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteUpload(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); virtual;
    function  ExecuteValidation(request: TFHIRRequest; response : TFHIRResponse; opDesc : String) : boolean; virtual;
    function ExecuteTransaction(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String; virtual;
    procedure ExecuteBatch(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); virtual;
    function ExecuteOperation(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String; virtual;
    procedure BuildSearchForm(request: TFHIRRequest; response: TFHIRResponse);
  public
    constructor Create(Storage : TFHIRStorageService; ServerContext : TFslObject; const lang : THTTPLanguages);
    destructor Destroy; override;

    procedure NoMatch(request: TFHIRRequest; response: TFHIRResponse);
    procedure NotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure VersionNotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure TypeNotFound(request: TFHIRRequest; response : TFHIRResponse);

    Property OnPopulateConformance : TPopulateConformanceEvent read FOnPopulateConformance write FOnPopulateConformance;
    property lang : THTTPLanguages read FLang write FLang;

    function opAllowed(resource : string; command : TFHIRCommandType) : Boolean; virtual;
    function check(response : TFHIRResponse; test : boolean; code : Integer; const lang : THTTPLanguages; message : String; issueCode : TFhirIssueType) : Boolean; virtual;
    Function Execute(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String;  virtual;
    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; virtual; abstract;
    function getResourcesByParam(aType : string; name, value : string; var needSecure : boolean): TFslList<TFHIRResourceV>; virtual;
    function FindResource(aType, sId : String; options : TFindResourceOptions; var resourceKey, versionKey : integer; request: TFHIRRequest; response: TFHIRResponse; sessionCompartments : TFslList<TFHIRCompartmentId>): boolean; virtual;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResourceV; virtual; abstract;
    function getResourceByUrl(aType : string; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResourceV; virtual; abstract;
    function GetResourceByKey(key : integer; var needSecure : boolean): TFHIRResourceV; virtual;
    function ResolveSearchId(resourceName : String; requestCompartment : TFHIRCompartmentId; SessionCompartments : TFslList<TFHIRCompartmentId>; baseURL, params : String) : TMatchingResourceList; virtual;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; httpCode : Integer; name, message : String; patients : TArray<String>); overload; virtual; abstract;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patients : TArray<String>); overload; virtual; abstract;
    function patientIds(request : TFHIRRequest; res : TFHIRResourceV) : TArray<String>; virtual; abstract;

    property clientCacheManager : TClientCacheManager read GetClientCacheManager;
    property Operations : TFslList<TFhirOperation> read FOperations;
    function createClient(const lang : THTTPLanguages; session: TFHIRSession) : TFhirClientV; virtual;
  end;

  TFHIRInternalCommunicator = class (TFHIRClientCommunicator)
  private
    FServerContext : TFslObject;
    FEngine : TFHIROperationEngine;
    FContext: TFHIRWorkerContextWithFactory;
    FSession: TFHIRSession;
    procedure SetContext(const Value: TFHIRWorkerContextWithFactory);
    procedure SetSession(const Value: TFHIRSession);
    procedure checkOutcome(resp: TFHIRResponse);
  public
    destructor Destroy; override;
    function link : TFHIRInternalCommunicator; overload;

    property Session : TFHIRSession read FSession write SetSession;
    property Context : TFHIRWorkerContextWithFactory read FContext write SetContext;

    function address : String; override;
    procedure doGetBundleBuilder(request: TFHIRRequest; context: TFHIRResponse; aType: TBundleType; out builder: TFhirBundleBuilder);

    function conformanceV(summary : boolean) : TFHIRResourceV; override;
    function transactionV(bundle : TFHIRResourceV) : TFHIRResourceV; override;
    function createResourceV(resource : TFHIRResourceV; var id : String) : TFHIRResourceV; override;
    function readResourceV(atype : string; id : String) : TFHIRResourceV; override;
    function vreadResourceV(atype : string; id, vid : String) : TFHIRResourceV; override;
    function updateResourceV(resource : TFHIRResourceV) : TFHIRResourceV; overload; override;
    procedure deleteResourceV(atype : string; id : String); override;
    function searchV(atype : string; allRecords : boolean; params : string) : TFHIRResourceV; overload; override;
    function searchPostV(atype : string; allRecords : boolean; params : TStringList; resource : TFHIRResourceV) : TFHIRResourceV; override;
    function searchAgainV(link : String) : TFHIRResourceV; overload; override;
    function operationV(atype : string; opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function operationV(atype : string; id, opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function historyTypeV(atype : string; allRecords : boolean; params : string) : TFHIRResourceV; override;
    function historyInstanceV(atype : string; id : String; allRecords : boolean; params : string) : TFHIRResourceV; override;
    function customGet(path : String; headers : THTTPHeaders) : TFslBuffer; override;
    function customPost(path : String; headers : THTTPHeaders; body : TFslBuffer) : TFslBuffer; override;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; patch : TJsonArray) : TFHIRResourceV; overload; override;
    procedure terminate; override;
  end;

  TFHIRStorageService = class (TFslObject)
  protected
    FFactory : TFHIRFactory;
    function GetTotalResourceCount: integer; virtual; abstract;
    function SupportsSubscriptions : boolean; virtual;
    function SupportsTransactions : boolean; virtual;
    function SupportsSearch : boolean; virtual;
    function SupportsHistory : boolean; virtual;
  public
    constructor Create(factory : TFHIRFactory); virtual;
    destructor Destroy; override;
    function Link : TFHIRStorageService; overload;
    property Factory : TFHIRFactory read FFactory;

    // OAuth Support
    procedure recordOAuthLogin(id, client_id, scope, redirect_uri, state, launch : String); virtual;
    function fetchOAuthDetails(key, status : integer; var client_id, name, redirect, state, scope, launch : String) : boolean; overload; virtual;
    function fetchOAuthDetails(id : String; var client_id, redirect, state, scope, launch : String) : boolean; overload; virtual;
    procedure recordOAuthChoice(id : String; scopes, jwt, patient : String); virtual;
    function hasOAuthSession(id : String; status : integer) : boolean; virtual;
    function hasOAuthSessionByKey(key, status : integer) : boolean; virtual;
    procedure updateOAuthSession(id : String; state, key : integer; var client_id : String); virtual;
    procedure RegisterConsentRecord(session: TFhirSession); virtual;
    function FetchAuthorization(hash : String; var PatientId : String; var ConsentKey, SessionKey : Integer; var Expiry : TDateTime; var jwt : String) : boolean; virtual;

    // server total counts:
    procedure FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList); virtual;  abstract;// comps = list of patient compartments
    Property TotalResourceCount: integer read GetTotalResourceCount;


    procedure Sweep; virtual; abstract;
    procedure RecordFhirSession(session: TFhirSession); virtual; abstract;
    procedure CloseFhirSession(key: integer); virtual; abstract;
    procedure QueueResource(session : TFHIRSession; r: TFhirResourceV); overload; virtual; abstract;
    procedure QueueResource(session : TFHIRSession; r: TFhirResourceV; dateTime: TFslDateTime); overload; virtual; abstract;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); virtual; abstract;
    function RetrieveSession(key : integer; var UserKey, Provider : integer; var Id, Name, Email : String) : boolean; virtual; abstract;

    function ProfilesAsOptionList : String; virtual; abstract;

    procedure ProcessSubscriptions; virtual; abstract;
    procedure ProcessEmails; virtual; abstract;
    procedure ProcessObservations; virtual; abstract;
    procedure RunValidation; virtual; abstract;


    function createOperationContext(const lang : THTTPLanguages) : TFHIROperationEngine; virtual; abstract;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); overload; virtual; abstract;
    function createClient(const lang : THTTPLanguages; ServerContext : TFslObject; context: TFHIRWorkerContextWithFactory; session: TFHIRSession) : TFHIRClientV; virtual;
    Procedure Yield(client : TFHIRClientV; exception : Exception); overload; virtual;
    function ExpandVS(vs: TFHIRValueSetW; ref: string; const lang : THTTPLanguages; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSetW; virtual;
    function LookupCode(system, version, code: String): String; virtual;
    function FetchResource(key : integer) : TFHIRResourceV; virtual; abstract;

    Procedure SetUpRecording(session : TFhirSession); virtual; abstract;
    procedure RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception); virtual; abstract;
    procedure FinishRecording(); virtual; abstract;

    function createAsyncTask(url, id : string; format : TFHIRFormat; secure : boolean) : integer; virtual;
    procedure setAsyncTaskDetails(key : integer; transactionTime : TFslDateTime; request : String); virtual;
    procedure updateAsyncTaskStatus(key : integer; status : TAsyncTaskStatus; message : String); virtual;
    procedure MarkTaskForDownload(key : integer; names : TStringList); virtual;
    function fetchTaskDetails(id : String; var key : integer; var status : TAsyncTaskStatus; var fmt : TFHIRFormat; var secure : boolean; var message, originalRequest : String; var transactionTime, expires : TFslDateTime; names : TStringList; var outcome : TBytes): boolean; virtual;
    procedure recordDownload(key : integer; name : String); virtual;
    procedure fetchExpiredTasks(tasks : TFslList<TAsyncTaskInformation>); virtual;
    procedure MarkTaskDeleted(key : integer); virtual;

    function getClientInfo(id : String) : TRegisteredClientInformation; virtual; abstract;
    function getClientName(id : String) : string; virtual; abstract;
    function storeClient(client : TRegisteredClientInformation; sessionKey : integer) : String; virtual; abstract;
    procedure fetchClients(list : TFslList<TRegisteredClientInformation>); virtual; abstract;

    function loadPackages : TFslMap<TLoadedPackageInformation>; virtual; abstract;
    function fetchLoadedPackage(id : String) : TBytes; virtual; abstract;
    procedure recordPackageLoaded(id, ver : String; count : integer; blob : TBytes); virtual; abstract;
    function cacheSize : UInt64; virtual;
    procedure clearCache; virtual;
  end;


implementation

uses
  server_context;

{ TFHIRStorageService }

function TFHIRStorageService.cacheSize: UInt64;
begin
  result := 0;
end;

procedure TFHIRStorageService.clearCache;
begin
  // nothing
end;

constructor TFHIRStorageService.Create(factory : TFHIRFactory);
begin
  inherited create;
  FFactory := factory;
end;

destructor TFHIRStorageService.Destroy;
begin
  FFactory.Free;
  inherited;
end;

function TFHIRStorageService.ExpandVS(vs: TFHIRValueSetW; ref: string; const lang : THTTPLanguages; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSetW;
begin
  raise EFHIRException.create('Expanding valuesets is not implemented in this server');
end;


function TFHIRStorageService.FetchAuthorization(hash: string; var PatientId : string; var ConsentKey, SessionKey: Integer; var Expiry: TDateTime; var jwt: String): boolean;
begin
  raise EFHIRException.create('This server does not support OAuth');
end;

procedure TFHIRStorageService.fetchExpiredTasks(tasks: TFslList<TAsyncTaskInformation>);
begin
  //
end;

function TFHIRStorageService.fetchOAuthDetails(id: String; var client_id, redirect, state, scope, launch: String): boolean;
begin
  raise EFHIRException.create('This server does not support OAuth');
end;

function TFHIRStorageService.fetchOAuthDetails(key, status: integer; var client_id, name, redirect, state, scope, launch: String): boolean;
begin
  raise EFHIRException.create('This server does not support OAuth');
end;

function TFHIRStorageService.fetchTaskDetails(id : String; var key : integer; var status: TAsyncTaskStatus; var fmt : TFHIRFormat; var secure : boolean; var message, originalRequest: String; var transactionTime, expires: TFslDateTime; names : TStringList; var outcome: TBytes): boolean;
begin
  raise EFHIRException.create('This server does not support Async tasks');
end;

function TFHIRStorageService.hasOAuthSession(id: String; status : integer): boolean;
begin
  raise EFHIRException.create('This server does not support OAuth');
end;

function TFHIRStorageService.hasOAuthSessionByKey(key, status: integer): boolean;
begin
  raise EFHIRException.create('This server does not support OAuth');
end;

function TFHIRStorageService.Link: TFHIRStorageService;
begin
  result := TFHIRStorageService(inherited Link);
end;

function TFHIRStorageService.LookupCode(system, version, code: String): String;
begin
  raise EFHIRException.create('Looking up codes is not implemented in this server');
end;

procedure TFHIRStorageService.MarkTaskDeleted(key: integer);
begin
  raise EFHIRException.create('This server does not support Async tasks');
end;

procedure TFHIRStorageService.MarkTaskForDownload(key: integer; names : TStringList);
begin
  raise EFHIRException.create('This server does not support Async tasks');
end;

procedure TFHIRStorageService.recordDownload(key: integer; name: String);
begin
  raise EFHIRException.create('This server does not support Async tasks');
end;

procedure TFHIRStorageService.recordOAuthChoice(id, scopes, jwt, patient: String);
begin
  raise EFHIRException.create('This server does not support OAuth');
end;

procedure TFHIRStorageService.recordOAuthLogin(id, client_id, scope, redirect_uri, state, launch: String);
begin
  raise EFHIRException.create('This server does not support OAuth');
end;

procedure TFHIRStorageService.RegisterConsentRecord(session: TFhirSession);
begin
  raise EFHIRException.create('This server does not support OAuth');
end;

procedure TFHIRStorageService.setAsyncTaskDetails(key: integer; transactionTime: TFslDateTime; request: String);
begin
  raise EFHIRException.create('This server does not support Async tasks');
end;

function TFHIRStorageService.SupportsHistory: boolean;
begin
  result := false;
end;

function TFHIRStorageService.SupportsSearch: boolean;
begin
  result := false;
end;

function TFHIRStorageService.SupportsSubscriptions: boolean;
begin
  result := false;
end;

function TFHIRStorageService.SupportsTransactions: boolean;
begin
  result := false;
end;

procedure TFHIRStorageService.updateAsyncTaskStatus(key: integer; status: TAsyncTaskStatus; message: String);
begin
  raise EFHIRException.create('This server does not support Async tasks');
end;

procedure TFHIRStorageService.updateOAuthSession(id : String; state, key: integer; var client_id : String);
begin
  raise EFHIRException.create('This server does not support OAuth');
end;

{ TFHIROperationEngine }


{ TOperationContext }

constructor TOperationContext.Create;
begin
  inherited Create;
end;

constructor TOperationContext.Create(mode : TOperationMode; logDetails : TOperationLoggingLevel);
begin
  Create;
  FMode := mode;
  FLogging := logDetails;
end;

constructor TOperationContext.Create(mode : TOperationMode);
begin
  Create;
  FMode := mode;
  FLogging := ollNone;
end;


{ TFHIROperationEngine }

constructor TFHIROperationEngine.create(Storage : TFHIRStorageService; ServerContext : TFslObject; const lang : THTTPLanguages);
begin
  inherited create;
  FServerContext := ServerContext;
  FLang := lang;
  FOperations := TFslList<TFhirOperation>.create;
  FStorage := Storage;
end;

function TFHIROperationEngine.createClient(const lang : THTTPLanguages; session: TFHIRSession): TFHIRClientV;
var
  int : TFHIRInternalCommunicator;
begin
  int := TFHIRInternalCommunicator.Create;
  try
    int.FServerContext := FServerContext.Link;
    int.FEngine := self.link as TFHIROperationEngine;
    int.Context := TFHIRServerContext(FServerContext).ValidatorContext.link;
    int.session := session.link;
    result := factory.makeClientInt(TFHIRServerContext(FServerContext).ValidatorContext.link, lang, int.link);
  finally
    int.Free;
  end;
end;

destructor TFHIROperationEngine.Destroy;
begin
  FEngine.Free;
  FStorage.Free;
  FOperations.Free;
  inherited;
end;

procedure TFHIROperationEngine.NoMatch(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_MATCH', lang), [request.ResourceName+'?'+request.parameters.source]);
  response.Body := response.Message;
  response.Resource := factory.BuildOperationOutcome(lang, response.Message);
end;

procedure TFHIROperationEngine.NotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.ResourceName+':'+request.Id]);
  response.Body := response.Message;
  response.Resource := factory.BuildOperationOutcome(lang, response.Message);
end;

function TFHIROperationEngine.opAllowed(resource: string; command: TFHIRCommandType): Boolean;
begin
  result := true;
end;

procedure TFHIROperationEngine.VersionNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.ResourceName+':'+request.Id+'/_history/'+request.subId]);
  response.Body := response.Message;
  response.Resource := factory.BuildOperationOutcome(lang, response.Message);
end;


procedure TFHIROperationEngine.TypeNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_UNKNOWN_TYPE', lang), [request.ResourceName]);
  response.Body := response.Message;
  response.Resource := factory.BuildOperationOutcome(lang, response.Message);
end;

function TFHIROperationEngine.check(response: TFHIRResponse; test: boolean; code : Integer; const lang : THTTPLanguages; message: String; issueCode : TFhirIssueType): Boolean;
begin
  result := test;
  if not test and (response <> nil) then
  begin
    response.HTTPCode := code;
    response.Message := message;
    response.ContentType := 'text/plain';
    response.Body := message;
    response.Resource := factory.BuildOperationOutcome(lang, message, issueCode);
  end;
end;


function TFHIROperationEngine.Execute(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): String;
begin
  InterlockedIncrement(GCounterFHIRRequests);
  try
    StartTransaction;
    try
      result := CODES_TFHIRCommandType[request.CommandType]+' on '+Request.Id;
      case request.CommandType of
        fcmdRead : ExecuteRead(request, response, false);
        fcmdUpdate : ExecuteUpdate(context, request, response);
        fcmdVersionRead : ExecuteVersionRead(request, response);
        fcmdDelete : ExecuteDelete(request, response);
        fcmdHistoryInstance, fcmdHistoryType, fcmdHistorySystem : ExecuteHistory(request, response);
        fcmdSearch : ExecuteSearch(request, response);
        fcmdCreate : result := ExecuteCreate(context, request, response, request.NewIdStatus, 0);
        fcmdMetadata : ExecuteMetadata(context, request, response);
        fcmdTransaction : result := ExecuteTransaction(context, request, response);
        fcmdBatch :
          begin
          result := 'Batch';
          ExecuteBatch(context, request, response);
          end;
        fcmdOperation : result := ExecuteOperation(context, request, response);
        fcmdUpload : ExecuteUpload(context, request, response);
        fcmdPatch : ExecutePatch(context, request, response);
        fcmdValidate : ExecuteValidation(request, response, 'Validation')
      else
        raise EFHIRException.create(GetFhirMessage('MSG_UNKNOWN_OPERATION', lang));
      End;

      CommitTransaction;
    except
      on e:exception do
      begin
        RollbackTransaction;
        raise;
      end;
    end;
  finally
    InterlockedDecrement(GCounterFHIRRequests);
  end;
end;

procedure TFHIROperationEngine.ExecuteBatch(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFHIRException.create('This server does not implement the "Batch" function');
end;

procedure TFHIROperationEngine.ExecuteMetadata(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
begin
  if (context <> nil) then
    context.CacheResponse := true;

  if not request.Parameters.has('mode') then
    ExecuteCapabilityStatement(request, response, true)
  else if request.Parameters['mode'] = 'full' then
    ExecuteCapabilityStatement(request, response, true)
  else if request.Parameters['mode'] = 'normative' then
    ExecuteCapabilityStatement(request, response, false)
  else if request.Parameters['mode'] = 'terminology' then
    ExecuteTerminologyCapabilities(request, response)
  else
    raise EFHIRException.Create('unknown mode '+request.Parameters['mode']);
end;

procedure TFHIROperationEngine.ExecuteCapabilityStatement(request: TFHIRRequest; response: TFHIRResponse; full : boolean);
var
  oConf : TFhirCapabilityStatementW;
  res : TFhirCapabilityStatementRestResourceW;
  a : String;
  html : TFslStringBuilder;
//  c : TFhirContactPoint;
  i : integer;
//  ct : TFhirConformanceContact;
  ServerContext : TFHIRServerContext;
begin
  ServerContext := TFHIRServerContext(FServerContext);
  try
    response.HTTPCode := 200;
    oConf := factory.wrapCapabilityStatement(factory.makeResource('CapabilityStatement'));
    try
      response.Resource := oConf.Resource.link;

      oConf.id := 'FhirServer';
      oConf.contact(cpsOther, 'http://healthintersections.com.au/');
      if ServerContext.FormalURLPlain <> '' then
        oConf.url := AppendForwardSlash(ServerContext.FormalURLPlain)+'metadata'
      else
        oConf.url := 'http://fhir.healthintersections.com.au/open/metadata';

      oConf.version := factory.versionString+'-'+SERVER_FULL_VERSION; // this conformance statement is versioned by both
      oConf.name := 'FHIR Reference Server Conformance Statement';
      oConf.description := 'Standard Conformance Statement for the open source Reference FHIR Server provided by Health Intersections';
      oConf.status := psActive;
      oConf.kind := cskInstance;
      oConf.acceptUnknown := csauBoth;

      oConf.date := TFslDateTime.makeUTC;
      oConf.software('Reference Server', SERVER_FULL_VERSION, SERVER_RELEASE_DATE);
      if ServerContext.FormalURLPlain <> '' then
        oConf.impl(ServerContext.FormalURLPlain, 'FHIR Server running at '+ServerContext.FormalURLPlain);
      if assigned(OnPopulateConformance) then
        OnPopulateConformance(self, oConf, request.secure, request.baseUrl, ['launch-standalone', 'launch-ehr', 'client-public', 'client-confidential-symmetric', 'sso-openid-connect', 'permission-offline', 'permission-patient', 'permission-user']);
      if factory.version <> fhirVersionRelease2 then
      begin
        oConf.fmt('application/fhir+xml');
        oConf.fmt('application/fhir+json');
      end
      else
      begin
        oConf.fmt('application/xml+fhir');
        oConf.fmt('application/json+fhir');
      end;

      oConf.fhirVersion := factory.versionString;
      if FStorage.SupportsSubscriptions then
        oConf.standardServer('http://hl7.org/fhir/CapabilityStatement/terminology-server', request.baseUrl+'websockets',
          TCDSHooks.patientView, TCDSHooks.codeView, TCDSHooks.identifierView, FStorage.SupportsTransactions, FStorage.SupportsSearch, FStorage.SupportsHistory)
      else
        oConf.standardServer('http://hl7.org/fhir/CapabilityStatement/terminology-server', '',
          TCDSHooks.patientView, TCDSHooks.codeView, TCDSHooks.identifierView, FStorage.SupportsTransactions, FStorage.SupportsSearch, FStorage.SupportsHistory);

      html := TFslStringBuilder.Create;
      try
        html.append('<div><h2>'+ServerContext.Globals.OwnerName+' Conformance Statement</h2><p>FHIR v'+factory.versionString+' released '+SERVER_RELEASE_DATE+'. '+
         'Server version '+SERVER_FULL_VERSION+' built '+SERVER_RELEASE_DATE+'</p><table class="grid"><tr><th>Resource Type</th><th>Profile</th><th>Read</th><th>V-Read</th><th>Search</th><th>Update</th><th>Updates</th><th>Create</th><th>Delete</th><th>History</th></tr>'+#13#10);
        for a in ServerContext.ValidatorContext.allResourceNames do
          if (a <> 'Custom') and (a <> 'Parameters') then
          begin
            if ServerContext.ResConfig[a].Supported and (a <> 'MessageHeader') and (a <> 'Custom') then
            begin
              if a = 'Binary' then
                html.append('<tr><td>'+a+'</td>'+
                '<td>--</td>')
              else
                html.append('<tr><td>'+a+'</td>'+
                '<td><a href="'+request.baseUrl+'StructureDefinition/'+lowercase(a)+'?format=text/html">'+lowercase(a)+'</a></td>');
              res := oConf.addResource(a);
              try
                res.profile := request.baseUrl+'StructureDefinition/'+lowercase(a);
                if (a <> 'MessageHeader') and (a <> 'Parameters') Then
                begin
                  if request.canRead(a) then
                  begin
                    html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
                    res.addInteraction('read');
                    if ServerContext.ResConfig[a].cmdVRead then
                    begin
                      html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
                      res.addInteraction('vread');
                      res.readHistory := true;
                    end
                    else
                      html.append('<td></td>');
                  end
                  else
                    html.append('<td align="center"></td><td align="center"></td>');
                  if ServerContext.ResConfig[a].cmdSearch and request.canRead(a) then
                  begin
                    res.addInteraction('search-type');
                    html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
                  end
                  else
                    html.append('<td></td>');
                  if ServerContext.ResConfig[a].cmdUpdate and request.canWrite(a) then
                  begin
                    res.addInteraction('update');
                    html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
                  end
                  else
                    html.append('<td></td>');
                  if ServerContext.ResConfig[a].cmdHistoryType and request.canRead(a) then
                  begin
                    res.addInteraction('history-type');
                    html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
                  end
                  else
                    html.append('<td></td>');
                  if ServerContext.ResConfig[a].cmdCreate and request.canWrite(a) then
                  begin
                    res.addInteraction('create');
                    html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
                  end
                  else
                    html.append('<td></td>');
                  if ServerContext.ResConfig[a].cmdDelete and request.canWrite(a) then
                  begin
                    res.addInteraction('delete');
                    html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
                  end
                  else
                    html.append('<td></td>');
                  if ServerContext.ResConfig[a].cmdHistoryInstance and request.canRead(a) then
                  begin
                    res.addInteraction('history-instance');
                    html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
                  end
                  else
                    html.append('<td></td>');
    //                html.append('<br/>search</td><td><ul>');
                  for i := 0 to ServerContext.Indexes.Indexes.count - 1 do
                    if (ServerContext.Indexes.Indexes[i].ResourceType = a) then
                      if ServerContext.Indexes.Indexes[i].Name <> '_query' then
                        res.addParam(html.AsString, ServerContext.Indexes.Indexes[i].Name, ServerContext.Indexes.Indexes[i].uri, ServerContext.Indexes.Indexes[i].Description, ServerContext.Indexes.Indexes[i].SearchType, ServerContext.Indexes.Indexes[i].TargetTypes);

    //              addParam(res, html, '_id', 'http://hl7.org/fhir/search', 'Resource Logical ID', SearchParamTypeToken, []);
                  res.addParam(html.AsString, '_text', 'http://hl7.org/fhir/search', 'General Text Search of the narrative portion', sptString, []);
                  res.addParam(html.AsString, '_profile', 'http://hl7.org/fhir/search', 'Search for resources that conform to a profile', sptReference, []);
                  res.addParam(html.AsString, '_security', 'http://hl7.org/fhir/search', 'Search for resources that have a particular security tag', sptReference, []);
                  res.addParam(html.AsString, '_sort', 'http://hl7.org/fhir/search', 'Specify one or more other parameters to use as the sort order', sptToken, []);
                  res.addParam(html.AsString, '_count', 'http://hl7.org/fhir/search', 'Number of records to return', sptNumber, []);
                  res.addParam(html.AsString, '_summary', 'http://hl7.org/fhir/search', 'Return just a summary for resources that define a summary view', sptNumber, []);
                  res.addParam(html.AsString, '_include', 'http://hl7.org/fhir/search', 'Additional resources to return - other resources that matching resources refer to', sptToken, factory.ResourceNames);
                  res.addParam(html.AsString, '_reverseInclude', 'http://hl7.org/fhir/search', 'Additional resources to return - other resources that refer to matching resources (this is trialing an extension to the specification)', sptToken, factory.ResourceNames);
                  res.addParam(html.AsString, '_filter', 'http://hl7.org/fhir/search', 'filter parameter as documented in the specification', sptToken, factory.ResourceNames);

    //              html.append('</ul>');                                                                                                                               }
                end;
                html.append('</tr>'#13#10);
                if (not res.hasInteraction) then
                  raise Exception.Create('No interactions for '+res.code+'?');

                  //<th>Search/Updates Params</th>
                  // html.append('n : offset<br/>');
                  // html.append('count : # resources per request<br/>');
                  // html.append(m.Indexes[i]+' : ?<br/>');
              finally
                res.free;
              end;
            end;
          end;
        html.append('</table>'#13#10);

        html.append('<p>Operations</p>'#13#10'<ul>'+#13#10);
        for i := 0 to FOperations.Count - 1 do
        begin
          if TFhirOperation(FOperations[i]).formalURL <> '' then
            oConf.addOperation(TFhirOperation(FOperations[i]).Name, TFhirOperation(FOperations[i]).formalURL)
          else
            oConf.addOperation(TFhirOperation(FOperations[i]).Name, AppendForwardSlash(ServerContext.FormalURLPlain)+'OperationDefinition/fso-'+TFhirOperation(FOperations[i]).name);
          html.append(' <li>'+TFhirOperation(FOperations[i]).name+': see OperationDefinition/fso-'+TFhirOperation(FOperations[i]).name+'</li>'#13#10);
        end;
        html.append('</ul>'#13#10);

        if ServerContext.ResConfig.ContainsKey('CodeSystem') and ServerContext.ResConfig['CodeSystem'].Supported and ServerContext.ResConfig['CodeSystem'].Supported then
          oConf.addInstantiates('http://hl7.org/fhir/CapabilityStatement/terminology-server');

        html.append('</div>'#13#10);
        // operations
        factory.setXhtml(oConf.Resource, TFHIRXhtmlParser.parse(lang, xppReject, [], html.AsString));
      finally
        html.free;
      end;

      if (request.Parameters.has('_graphql') and (response.Resource <> nil) and (response.Resource.fhirType <> 'OperationOutcome')) then
        processGraphQL(request.Parameters['_graphql'], request, response);
    finally
      oConf.free;
    end;

    AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function describeResourceTypes(aTypes : TArray<String>) : String;
var
  a : String;
begin
  result := '';
  for a in aTypes do
    if result = '' then
      result := a
    else
      result := result+' / '+a;
end;

procedure TFHIROperationEngine.BuildSearchForm(request: TFHIRRequest; response : TFHIRResponse);
var
  i, j : integer;
  s, pfx, desc, rn : String;
  ix, ix2 : TFhirIndex;
  types : TArray<String>;
  m : TStringList;
  ok : boolean;
begin
  response.HTTPCode := 200;
  response.ContentType := 'text/html';
  s :=
'<?xml version="1.0" encoding="UTF-8"?>'#13#10+
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
''#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
'<head>'#13#10+
'    <title>FHIR RESTful Server - FHIR v'+Factory.versionString+'</title>'#13#10+
TFHIRXhtmlComposer.PageLinks+
FHIR_JS+
'</head>'#13#10+
''#13#10+
'<body>'#13#10+
''#13#10+
TFHIRXhtmlComposer.Header(factory, request.Session, request.baseUrl, request.lang, SERVER_FULL_VERSION)+
'<h2>'+GetFhirMessage('SEARCH_TITLE', lang)+'</h2>'#13#10+
'</p>'#13#10;
if Request.DefaultSearch then
s := s +
'<form action="'+request.ResourceName+'/_search" method="GET">'#13#10+
'<table class="lines">'#13#10
else
s := s +
'<form action="_search" method="GET">'#13#10+
'<table class="lines">'#13#10;

  if request.ResourceName = '' then
  begin
    s := s +'<tr><td colspan="4"><b>General search:</b></td></tr>'+#13#10+
       '<tr><td align="left">_language</td><td><input type="text" name="_language"></td><td></td><td><select title="Missing?" size="1" nam'+'e="_language:missing"><option></option><option value="1">absent</option><option name="0">present</option></select></td></tr>'+#13#10+
       '<tr><td align="left">_lastUpdated </td><td><input type="text" name="_lastUpdated"></td><td>(Date)</td><td><select title="Missing?"'+' size="1" name="_lastUpdated-missing"><option></option><option value="1">absent</option><option name="0">present</option></select></td></tr>'+#13#10+
       '<tr><td align="right"> (before)</td><td><input type="text" name="_lastUpdated:before"></td><td> (before given date)</td><td></td><'+'/tr>'+#13#10+
       '<tr><td align="right"> (after)</td><td><input type="text" name="_lastUpdated:after"></td><td> (after given date)</td><td></td></tr'+'>'+#13#10+
       '<tr><td align="left">_profile</td><td><input type="text" name="_profile"></td><td></td><td><select title="Missing?" size="1" name='+'"_profile:missing"><option></option><option value="1">absent</option><option name="0">present</option></select></td></tr>'+#13#10+
       '<tr><td align="left">_security</td><td><input type="text" name="_security"></td><td></td><td><select title="Missing?" size="1" nam'+'e="_security:missing"><option></option><option value="1">absent</option><option name="0">present</option></select></td></tr>'+#13#10+
       '<tr><td align="left">_tag</td><td><input type="text" name="_tag"></td><td></td><td><select title="Missing?" size="1" name="_tag:mi'+'ssing"><option></option><option value="1">absent</option><option name="0">present</option></select></td></tr>'+#13#10;
  end
  else
  begin
    s := s +'<tr><td colspan="4"><b>'+request.ResourceName+':</b></td></tr>'+#13#10;
    for i := 0 to TFHIRServerContext(FServerContext).Indexes.Indexes.Count - 1 Do
    begin
      ix := TFHIRServerContext(FServerContext).Indexes.Indexes[i];
      if (ix.ResourceType = request.ResourceName) and (length(ix.TargetTypes) = 0) then
      begin
        desc := FormatTextToHTML(GetFhirMessage('ndx-'+request.ResourceName+'-'+ix.name, lang, ix.Description));
        if ix.SearchType = sptDate then
        begin
          s := s + '<tr><td align="left">'+ix.Name+' </td><td><input type="text" name="'+ix.Name+'"></td><td> '+desc+' on given date (yyyy-mm-dd)</td>'+
          '<td><select title="Missing?" size="1" name="'+ix.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10;
          s := s + '<tr><td align="right"> (before)</td><td><input type="text" name="'+ix.Name+':before"></td><td> (before given date)</td><td></td></tr>'#13#10;
          s := s + '<tr><td align="right"> (after)</td><td><input type="text" name="'+ix.Name+':after"></td><td> (after given date)</td><td></td></tr>'#13#10
        end
        else
          s := s + '<tr><td align="left">'+ix.Name+'</td><td><input type="text" name="'+ix.Name+'"></td><td> '+desc+'</td>'+
          '<td><select title="Missing?" size="1" name="'+ix.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10
      end;
    end;

    for i := 0 to TFHIRServerContext(FServerContext).Indexes.Indexes.Count - 1 Do
    begin
      ix := TFHIRServerContext(FServerContext).Indexes.Indexes[i];
      if (ix.ResourceType = request.ResourceName) and (length(ix.TargetTypes) > 0) then
      begin
        pfx := ix.Name;
        types := ix.TargetTypes;
        s := s +'<tr><td colspan="4"><b>'+ix.Name+'</b> ('+describeResourceTypes(types)+')<b>:</b></td></tr>'+#13#10;
        m := TStringList.create;
        try
          for j := 0 to TFHIRServerContext(FServerContext).Indexes.Indexes.Count - 1 Do
          begin
            ix2 := TFHIRServerContext(FServerContext).Indexes.Indexes[j];
            ok := false;
            for rn in types do
              ok := ok or (ix2.ResourceType = rn);
            if (ok) and (m.IndexOf(ix2.Name) = -1) then
            begin
              desc := FormatTextToHTML(GetFhirMessage('ndx-'+request.ResourceName+'-'+ix2.name, lang, ix2.Description));
              if (ix2.searchType = sptDate) then
              begin
                s := s + '<tr>&nbsp;&nbsp;<td align="left">'+ix2.Name+' (exact)</td><td><input type="text" name="'+pfx+'.'+ix2.Name+'"></td><td> '+desc+'</td>'+
          '<td><select title="Missing?" size="1" name="'+pfx+'.'+ix2.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10;
                s := s + '<tr>&nbsp;&nbsp;<td align="right">  (before)</td><td><input type="text" name="'+pfx+'.'+ix2.Name+':before"></td><td> (before given date) </td></tr>'#13#10;
                s := s + '<tr>&nbsp;&nbsp;<td align="right">  (after)</td><td><input type="text" name="'+pfx+'.'+ix2.Name+':after"></td><td> (after given date) </td></tr>'#13#10
              end
              else
                s := s + '<tr>&nbsp;&nbsp;<td align="left">'+ix2.Name+'</td><td><input type="text" name="'+pfx+'.'+ix2.Name+'"></td><td> '+desc+'</td>'+
          '<td><select title="Missing?" size="1" name="'+pfx+'.'+ix2.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10;
              m.add(ix2.Name);
            end;
          end;
        finally
          m.Free;
        end;
      end;
    end;
  end;

  s := s +
'<tr><td colspan="2"><hr/></td></tr>'#13#10+
'<tr><td align="right">'+GetFhirMessage('SEARCH_REC_TEXT', lang)+'</td><td><input type="text" name="'+SEARCH_PARAM_NAME_TEXT+'"></td><td> '+GetFhirMessage('SEARCH_REC_TEXT_COMMENT', lang)+'</td></tr>'#13#10+
'<tr><td align="right">'+GetFhirMessage('SEARCH_REC_OFFSET', lang)+'</td><td><input type="text" name="'+SEARCH_PARAM_NAME_OFFSET+'"></td><td> '+GetFhirMessage('SEARCH_REC_OFFSET_COMMENT', lang)+'</td></tr>'#13#10+
'<tr><td align="right">'+GetFhirMessage('SEARCH_REC_COUNT', lang)+'</td><td><input type="text" name="'+SEARCH_PARAM_NAME_COUNT+'"></td><td> '+StringFormat(GetFhirMessage('SEARCH_REC_COUNT_COMMENT', lang), [SEARCH_PAGE_LIMIT])+'</td></tr>'#13#10+
'<tr><td align="right">'+GetFhirMessage('SEARCH_SORT_BY', lang)+'</td><td><select size="1" name="'+SEARCH_PARAM_NAME_SORT+'">'+#13#10;
  for i := 0 to TFHIRServerContext(FServerContext).Indexes.Indexes.Count - 1 Do
  begin
    ix := TFHIRServerContext(FServerContext).Indexes.Indexes[i];
    if (ix.ResourceType = request.ResourceName) or ((request.ResourceName = '') and (ix.Name.startsWith('_'))) then
      s := s + '<option value="'+ix.Name+'">'+ix.Name+'</option>';
  end;
  s := s + '</select></td><td></td></tr>'#13#10+
'<tr><td align="right">'+GetFhirMessage('SEARCH_SUMMARY', lang)+'</td><td><input type="checkbox" name="'+SEARCH_PARAM_NAME_SUMMARY+'" value="true"></td><td> '+GetFhirMessage('SEARCH_SUMMARY_COMMENT', lang)+'</td></tr>'#13#10+
'</table>'#13#10+
'<p><input type="submit"/></p>'#13#10+
'</form>'#13#10+
''#13#10+
'<p>'+
TFHIRXhtmlComposer.Footer(factory, request.baseUrl, lang, request.internalRequestId);
  response.Body := s;
end;

function TFHIROperationEngine.ExecuteCreate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse; idState: TCreateIdState; iAssignedKey: Integer): String;
begin
  raise EFHIRException.create('This server does not implement the "Create" function');
end;

procedure TFHIROperationEngine.ExecuteDelete(request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFHIRException.create('This server does not implement the "Delete" function');
end;

procedure TFHIROperationEngine.ExecuteHistory(request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFHIRException.create('This server does not implement the "History" function');
end;

function TFHIROperationEngine.ExecuteOperation(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  i : integer;
  op : TFhirOperation;
begin
  result := 'Unknown Operation ';
  for i := 0 to FOperations.count - 1 do
  begin
    op := TFhirOperation(FOperations[i]);
    if (op.HandlesRequest(request)) then
    begin
      result := op.Execute(context, self, request, response);
      exit;
    end;
  end;
  raise EFHIRException.create('Unsupported Operation '+request.OperationName+' on resource '+request.ResourceName);
end;

function TFHIROperationEngine.ExecutePatch(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse): Boolean;
begin
  raise EFHIRException.create('This server does not implement the "Patch" function');
end;

function TFHIROperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse; ignoreHeaders : boolean) : boolean;
begin
  raise EFHIRException.create('This server does not implement the "Read" function');
end;

procedure TFHIROperationEngine.ExecuteSearch(request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFHIRException.create('This server does not implement the "Search" function');
end;

procedure TFHIROperationEngine.ExecuteTerminologyCapabilities(request: TFHIRRequest; response: TFHIRResponse);
var
  oConf : TFhirTerminologyCapabilitiesW;
  ServerContext : TFHIRServerContext;
  s : String;
begin
  ServerContext := TFHIRServerContext(FServerContext);
  try
    response.HTTPCode := 200;
    oConf := factory.makeTerminologyCapablities;
    try
      response.Resource := oConf.Resource.link;

      oConf.id := 'FhirServer';
      oConf.contact(cpsOther, 'http://healthintersections.com.au/');
      if ServerContext.FormalURLPlain <> '' then
        oConf.url := AppendForwardSlash(ServerContext.FormalURLPlain)+'metadata'
      else
        oConf.url := 'http://fhir.healthintersections.com.au/open/metadata';

      oConf.version := factory.versionString+'-'+SERVER_FULL_VERSION; // this conformance statement is versioned by both
      oConf.name := 'FHIR Reference Server Conformance Statement';
      oConf.description := 'Standard Conformance Statement for the open source Reference FHIR Server provided by Health Intersections';
      oConf.status := psActive;
      oConf.date := TFslDateTime.makeUTC;

      for s in ServerContext.TerminologyServer.listSystems do
        oConf.systemUri(s);

      oConf.addExpansionParameter('cache-id', 'This server supports caching terminology resources between calls. Clients only need to send value sets and codesystems once; there after tehy are automatically in scope for calls with the same cache-id. The cache is retained for 30 min from last call');
      oConf.addExpansionParameter('tx-resource', 'Additional valuesets needed for evaluation e.g. value sets referred to from the import statement of the value set being expanded');

      if (request.Parameters.has('_graphql') and (response.Resource <> nil) and (response.Resource.fhirType <> 'OperationOutcome')) then
        processGraphQL(request.Parameters['_graphql'], request, response);
    finally
      oConf.free;
    end;

    AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;

end;

function TFHIROperationEngine.ExecuteTransaction(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
begin
  raise EFHIRException.create('This server does not implement the "Transaction" function');
end;

function TFHIROperationEngine.ExecuteUpdate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): Boolean;
begin
  raise EFHIRException.create('This server does not implement the "Update" function');
end;

procedure TFHIROperationEngine.ExecuteUpload(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFHIRException.create('This server does not implement the "Upload" function');
end;

function TFHIROperationEngine.ExecuteValidation(request: TFHIRRequest; response: TFHIRResponse; opDesc: String): boolean;
begin
  raise EFHIRException.create('This server does not implement the "Validation" function');
end;

procedure TFHIROperationEngine.ExecuteVersionRead(request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFHIRException.create('This server does not implement the "VersionRead" function');
end;

function TFHIROperationEngine.factory: TFHIRFactory;
begin
  result := TFHIRServerContext(FServerContext).Factory;
end;

function TFHIROperationEngine.FindResource(aType, sId: String; options : TFindResourceOptions; var resourceKey, versionKey: integer; request: TFHIRRequest; response: TFHIRResponse; sessionCompartments : TFslList<TFHIRCompartmentId>): boolean;
begin
  raise EFHIRException.create('This server does not implement the "FindResource" function');
end;

function TFHIROperationEngine.GetClientCacheManager: TClientCacheManager;
begin
  result := TFHIRServerContext(FServerContext).ClientCacheManager;
end;

function TFHIROperationEngine.GetResourceByKey(key: integer; var needSecure: boolean): TFHIRResourceV;
begin
  raise EFHIRException.create('This server does not implement the "GetResourceByKey" function');
end;

function TFHIROperationEngine.getResourcesByParam(aType: string; name, value: string; var needSecure: boolean): TFslList<TFHIRResourceV>;
begin
  raise EFHIRException.create('This server does not implement the "getResourcesByParam" function');
end;

function TFHIROperationEngine.ResolveSearchId(resourceName : String; requestCompartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>; baseURL, params : String) : TMatchingResourceList;
begin
  raise EFHIRException.create('This server does not implement the "GetResourceByKey" function');
end;

function TFHIRStorageService.createAsyncTask(url, id: string; format : TFHIRFormat; secure : boolean): integer;
begin
  raise EFHIRException.create('Asynchronous Processing is not supported on this server');
end;

function TFHIRStorageService.createClient(const lang : THTTPLanguages; ServerContext : TFslObject; context: TFHIRWorkerContextWithFactory; session: TFHIRSession): TFHIRClientV;
var
  int : TFHIRInternalCommunicator;
begin
  int := TFHIRInternalCommunicator.Create;
  try
    int.FServerContext := ServerContext.Link;
    int.FEngine := createOperationContext(lang).link as TFHIROperationEngine;
    int.Context := context.link;
    int.session := session.link;
    result := factory.makeClientInt(TFHIRServerContext(ServerContext).ValidatorContext.link, lang, int.link);
  finally
    int.Free;
  end;
end;

procedure TFHIRStorageService.Yield(client: TFHIRClientV; exception: Exception);
begin
  yield(TFHIRInternalCommunicator(client.Communicator).FEngine, exception);
  client.Free;
end;

{ TMatchingResourceList }

function TMatchingResourceList.GetEntry(iIndex: Integer): TMatchingResource;
begin
  result := TMatchingResource(ObjectByIndex[iIndex]);
end;


{ TFHIRInternalCommunicator }

function TFHIRInternalCommunicator.address: String;
begin
  result := '(internal)';
end;

procedure TFHIRInternalCommunicator.checkOutcome(resp : TFHIRResponse);
var
  oow : TFhirOperationOutcomeW;
begin
  if resp.HTTPCode >= 300 then
    if (resp.Resource <> nil) and (resp.Resource.fhirType = 'OperationOutcome') then
    begin
      oow := FContext.factory.wrapOperationOutcome(resp.Resource.Link);
      try
        raise EFHIRClientException.Create(oow.text, oow.link);
      finally
        oow.Free;
      end;
    end
    else
      raise EFHIRClientException.Create(resp.Body, opWrapper.Create(FContext.factory.BuildOperationOutcome(THTTPLanguages.create('en'), resp.Body)));
end;

function TFHIRInternalCommunicator.conformanceV(summary: boolean): TFHIRResourceV;
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
begin
  req := TFHIRRequest.Create(context, roOperation, nil);
  try
    req.CommandType := fcmdMetadata;
    resp := TFHIRResponse.Create(FContext.link);
    try
      FEngine.ExecuteMetadata(nil, req, resp);
      checkOutcome(resp);
      result := resp.Resource.Link;
    finally
      resp.Free;
    end;
  finally
    req.Free;
  end;
end;

function TFHIRInternalCommunicator.createResourceV(resource: TFhirResourceV; var id: String): TFHIRResourceV;
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
  ctxt : TOperationContext;
begin
  ctxt := TOperationContext.Create(opmRestful);
  try
    req := TFHIRRequest.Create(context, roOperation, nil);
    try
      req.CommandType := fcmdCreate;
      req.resource := resource.link;
      req.ResourceName := resource.fhirType;
      req.Provenance := FClient.provenance.Link;

      resp := TFHIRResponse.Create(FContext.link);
      try
        FEngine.ExecuteCreate(ctxt, req, resp, idNoNew, 0);
        checkOutcome(resp);
        id := resp.Id;
        result := resp.Resource.Link;
      finally
        resp.Free;
      end;
    finally
      req.Free;
    end;
  finally
    ctxt.free;
  end;
end;

function TFHIRInternalCommunicator.customGet(path: String; headers: THTTPHeaders): TFslBuffer;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.customGet');
end;

function TFHIRInternalCommunicator.customPost(path: String; headers: THTTPHeaders; body: TFslBuffer): TFslBuffer;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.customPost');
end;

procedure TFHIRInternalCommunicator.deleteResourceV(atype: string; id: String);
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.deleteResourceV');
end;

destructor TFHIRInternalCommunicator.Destroy;
begin
  FEngine.Free;
  FContext.Free;
  FSession.Free;
  FServerContext.Free;
  inherited;
end;

function TFHIRInternalCommunicator.historyTypeV(atype: string; allRecords: boolean; params : string): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.historyTypeV');
end;

function TFHIRInternalCommunicator.historyInstanceV(atype: string; id : String; allRecords: boolean; params : string): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.historyInstanceV');
end;

function TFHIRInternalCommunicator.link: TFHIRInternalCommunicator;
begin
  result := TFHIRInternalCommunicator(inherited link);
end;

function TFHIRInternalCommunicator.operationV(atype: string; id, opName: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.operationV');
end;

function TFHIRInternalCommunicator.patchResourceV(atype: TFhirResourceTypeV; id: String; patch: TJsonArray): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.updateResourceV');
end;

function TFHIRInternalCommunicator.patchResourceV(atype: TFhirResourceTypeV; id: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.updateResourceV');
end;

function TFHIRInternalCommunicator.operationV(atype: string; opName: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.operationV');
end;

function TFHIRInternalCommunicator.readResourceV(atype: string; id: String): TFHIRResourceV;
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
  ctxt : TOperationContext;
begin
  ctxt := TOperationContext.Create(opmInternal);
  try
    req := TFHIRRequest.Create(context, roOperation, nil);
    try
      req.CommandType := fcmdRead;
      req.ResourceName := aType;
      req.id := id;

      resp := TFHIRResponse.Create(FContext.link);
      try
        FEngine.ExecuteRead(req, resp, true);
        checkOutcome(resp);
        id := resp.Id;
        result := resp.Resource.Link;
      finally
        resp.Free;
      end;
    finally
      req.Free;
    end;
  finally
    ctxt.free;
  end;
end;

function TFHIRInternalCommunicator.vreadResourceV(atype: string; id, vid: String): TFHIRResourceV;
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
  ctxt : TOperationContext;
begin
  ctxt := TOperationContext.Create(opmInternal);
  try
    req := TFHIRRequest.Create(context, roOperation, nil);
    try
      req.CommandType := fcmdRead;
      req.ResourceName := aType;
      req.id := id;
      req.SubId := vid;

      resp := TFHIRResponse.Create(FContext.link);
      try
        FEngine.ExecuteVersionRead(req, resp);
        checkOutcome(resp);
        id := resp.Id;
        result := resp.Resource.Link;
      finally
        resp.Free;
      end;
    finally
      req.Free;
    end;
  finally
    ctxt.free;
  end;
end;

procedure TFHIRInternalCommunicator.doGetBundleBuilder(request : TFHIRRequest; context: TFHIRResponse; aType: TBundleType; out builder: TFhirBundleBuilder);
var
  bnd : TFHIRBundleW;
begin
  if context.Format = ffNDJson then
    raise EFHIRException.CreateLang('NDJSON-ASYNC', request.Lang);
  bnd := TFHIRServerContext(FServerContext).Factory.wrapBundle(TFHIRServerContext(FServerContext).Factory.makeResource('Bundle'));
  try
    bnd.type_ := aType;
    builder := TFHIRBundleBuilderSimple.Create(TFHIRServerContext(FServerContext).Factory, bnd.link);;
  finally
    bnd.Free;
  end;
end;

function TFHIRInternalCommunicator.searchV(atype: string; allRecords: boolean; params: string): TFhirResourceV;
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
  ctxt : TOperationContext;
begin
  ctxt := TOperationContext.Create(opmInternal);
  try
    req := TFHIRRequest.Create(context, roOperation, nil);
    try
      req.CommandType := fcmdSearch;
      req.ResourceName := aType;
      req.Parameters := THTTPParameters.create(params+'&__wantObject=true');
      req.internalRequestId := TFHIRServerContext(FServerContext).Globals.nextRequestId;

      resp := TFHIRResponse.Create(FContext.link);
      try
//        resp.OnCreateBuilder := doGetBundleBuilder;
        FEngine.ExecuteSearch(req, resp);
        checkOutcome(resp);
        result := resp.Resource.Link;
      finally
        resp.Free;
      end;
    finally
      req.Free;
    end;
  finally
    ctxt.free;
  end;
end;

function TFHIRInternalCommunicator.searchAgainV(link: String): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.searchAgainV');
end;

function TFHIRInternalCommunicator.searchPostV(atype: string; allRecords: boolean; params : TStringList; resource: TFhirResourceV): TFhirResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.searchPostV');
end;

procedure TFHIRInternalCommunicator.SetContext(const Value: TFHIRWorkerContextWithFactory);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TFHIRInternalCommunicator.SetSession(const Value: TFHIRSession);
begin
  FSession.Free;
  FSession := Value;
end;

procedure TFHIRInternalCommunicator.terminate;
begin
  inherited;

end;

function TFHIRInternalCommunicator.transactionV(bundle: TFhirResourceV): TFhirResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.transactionV');
end;

function TFHIRInternalCommunicator.updateResourceV(resource: TFhirResourceV): TFhirResourceV;
begin
  raise EFHIRTodo.create('TFHIRInternalCommunicator.updateResourceV');
end;

{ TFhirOperation }

function TFhirOperation.Name: String;
begin
  result := '';
end;

function TFhirOperation.owningResource: string;
var
  t : string;
  b : boolean;
begin
  result := '';
  b := false;
  for t in Types do
    if b then
      raise EFHIRException.create('Multiple types for operation')
    else
    begin
      result := t;
      b := true;
    end;
  if (not b) then
    raise EFHIRException.create('No types for operation');
end;

function TFhirOperation.resolvePatient(manager: TFHIROperationEngine; request: TFHIRRequest; ref: String): integer;
var
  parts : TArray<String>;
  versionKey : integer;
begin
  parts := ref.Split(['/']);
  if length(parts) <> 2 then
    raise EFHIRException.create('Unable to understand the subject reference "'+ref+'"');
  if NOT manager.FindResource(parts[0], parts[1], [], result, versionKey, request, nil, nil) then
    result := 0;
end;

function TFhirOperation.Types: TArray<String>;
begin
  result := nil;
end;

constructor TFhirOperation.Create(factory: TFHIRFactory);
begin
  inherited create;
  FFactory := factory;
end;

function TFhirOperation.CreateBaseDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
//  result := TFhirOperationDefinition.Create;
//  try
//    result.id := 'fso-'+name;
//    result.meta := TFhirMeta.Create;
//    result.meta.lastUpdated := TFslDateTime.fromXml(FHIR_GENERATED_DATE);
//    result.meta.versionId := SERVER_FULL_VERSION;
//    result.url := AppendForwardSlash(base)+'OperationDefinition/fso-'+name;
//    result.version := FHIR_GENERATED_VERSION;
//    result.name := 'Operation Definition for "'+name+'"';
//    result.publisher := 'Grahame Grieve';
//    with result.contactList.Append do
//      with telecomList.Append do
//      begin
//        system := ContactPointSystemEmail;
//        value := 'grahame@fhir.org';
//      end;
//    result.description := 'Reference FHIR Server Operation Definition for "'+name+'"';
//    result.status := PublicationStatusDraft;
//    result.experimental := false;
//    result.date := TFslDateTime.fromXml(FHIR_GENERATED_DATE);
//    result.kind := OperationKindOperation;
//    result.code := name;
//    result.base := !{$IFNDEF FHIR4}TFhirReference.Create{$ENDIF}('http://hl7.org/fhir/OperationDefinition/'+name);
//    result.Link;
//  finally
//    result.Free;
//  end;
end;

function TFhirOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

destructor TFhirOperation.Destroy;
begin
  FFactory.Free;
  inherited;
end;

function TFhirOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse) : String;
begin
  // nothing
end;

function TFhirOperation.formalURL: String;
begin
  result := '';
end;

function TFhirOperation.HandlesRequest(request: TFHIRRequest): boolean;
var
  t : string;
begin
  result := (request.OperationName = Name) and
    (((length(types) = 0) and (request.ResourceName = '')) or StringArrayExistsSensitive(Types, request.ResourceName)) and
    ((request.version <> fhirVersionRelease4) or (request.subId = ''));
  if result then
  begin
    t := owningResource;
    if t = '' then
      t := request.ResourceName;
    if t = '' then
      result := ((isWrite and request.canWrite('') or (not isWrite and request.canRead('')))) // todo: what should it be?
    else
      result := ((isWrite and request.canWrite(t)) or (not isWrite and request.canRead(t)));
  end;
end;

function TFhirOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirOperation.makeParams(request: TFHIRRequest): TFhirParametersW;
var
  i : integer;
begin
  if (request.Resource <> nil) and (request.Resource.fhirType = 'Parameters') then
    result := FFactory.wrapParams(request.Resource.Link)
  else
    result := FFactory.wrapParams(FFactory.makeResource('Parameters'));
  try
    for i := 0 to request.Parameters.Count - 1 do
      result.AddParamStr(request.Parameters.Name[i], request.Parameters[request.Parameters.Name[i]]);
    result.link;
  finally
    result.Free;
  end;
end;


{ TAsyncTaskInformation }

function TAsyncTaskInformation.link: TAsyncTaskInformation;
begin
  result := TAsyncTaskInformation(inherited link);
end;

{ TRegisteredClientInformation }

constructor TRegisteredClientInformation.Create;
begin
  inherited;
  FRedirects := TStringList.Create;
end;

destructor TRegisteredClientInformation.Destroy;
begin
  FRedirects.Free;
  inherited;
end;

function TRegisteredClientInformation.link: TRegisteredClientInformation;
begin
  result := TRegisteredClientInformation(inherited Link);
end;

{ TLoadedPackageInformation }

function TLoadedPackageInformation.link: TLoadedPackageInformation;
begin
  result := TLoadedPackageInformation(inherited link);
end;

function TLoadedPackageInformation.summary: string;
begin
  result := 'v'+FVer+' (#'+inttostr(FCount)+' on '+FDate.truncToDay.toString('yyyy-mm-dd')+')';
end;

end.

