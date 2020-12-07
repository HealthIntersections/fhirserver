unit database;

{
  Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, IniFiles, Generics.Collections,
  fsl_base, fsl_threads, fsl_utilities, fsl_stream, fsl_xml,
  fsl_crypto, fsl_collections, fsl_json,
  fdb_manager, fdb_dialects,
  fsl_http, fsl_graphql,
  fsl_npm_cache,
  fhir_objects, fhir_parser, fhir_xhtml,  fhir_utilities, fhir_cdshooks,
  fhir_validator, fhir_common, fhir_factory, fhir_narrative,
  fhir_client,
  fhir_valuesets, fhir_diff, fhir_graphql, fhir_codegen,
  ftx_service, tx_server, ftx_ucum_services,
  fsl_scim, scim_server,
  indexing, session, subscriptions, security, obsservation_stats, bundlebuilder,
  closuremanager, graph_definition, tags, utilities,
  {$IFNDEF NO_JS}server_javascript,{$ENDIF}
  database_installer, mpi_search, server_context, storage, server_constants;

const
  MAXSQLDATE = 365 * 3000;
  NO_AUDIT_ON_SEARCH = false; // if you set this to true, the patient audit log won't pick up search results that include data about a patient, but search will be faster

const
  MAGIC_NUMBER = 941364592;

  CURRENT_FHIR_STORAGE_VERSION = 2;
  CONFIG_DEFAULT_RIGHTS = 10;
  CONFIG_DATABASE_VERSION = 5;

  RELEASE_DATE = '20131103';

  OP_MASK_TAG = 'this-tag-used-for-the-mask-operation-outcome';

  KEY_SAVE_SIZE = 1200;


type
  TKeyPair = class (TFslObject)
  private
    type_ : String;
    key : string;
  public
    constructor Create(t_ : String; key : string);
  end;

  TKeyList = class (TFslList<TKeyPair>)
  private
  public
    function forType(t_: String) : String;
    function forAll: String;
  end;

  TPatientIdTracker = class (TFslObject)
  private
    FIds : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure seeIds(ids : TArray<String>);
    function ids : TArray<String>;
  end;

  TFHIRQueuedResource = class (TFslObject)
  private
    FSession : TFHIRSession;
    FResource : TFHIRResourceV;
  public
    constructor Create(session : TFHIRSession; resource : TFHIRResourceV);
    destructor Destroy; override;

    property resource : TFHIRResourceV read FResource;
    property session : TFHIRSession read FSession;
  end;

  TFHIRTransactionEntryState = (tesIgnore, tesRead, tesCreate, tesUpdate, tesDelete);

  TFHIRTransactionEntry = class (TFslName)
  public
    state : TFHIRTransactionEntryState;
    id : String;
    originalId : String;
    key : integer;
    resType : String;
    version : String;
    outcomeVersion : integer;
    html : String;
    entry : TFHIRBundleEntryW;
    count : integer;
    function ignore : boolean;
    function deleted : boolean;
    function summary : string;
  end;

  TFHIRTransactionEntryList = class (TFslNameList)
  private
    FDropDuplicates: boolean;
    function GetEntry(iIndex: Integer): TFHIRTransactionEntry;
  public
    function ExistsByTypeAndId(entry : TFHIRTransactionEntry):boolean;
    Function GetByName(oName : String) : TFHIRTransactionEntry; Overload;
    Property entries[iIndex : Integer] : TFHIRTransactionEntry Read GetEntry; Default;
    function IndexByHtml(name : String) : integer; Overload;

    property DropDuplicates : boolean read FDropDuplicates write FDropDuplicates;
  end;

  TReferenceList = class (TStringList)
  public
    procedure seeReference(id : String);
    function asSql : String;
  end;

  TFHIRNativeOperationEngine = class;

  TFhirNativeOperation = class abstract (TFhirOperation)
  protected
    function native(engine : TFHIROperationEngine) : TFHIRNativeOperationEngine;
  end;

  TFHIRNativeStorageService = class;

  TFHIRNativeOperationEngine = class (TFHIROperationEngine)
  private
    FIndexer : TFHIRIndexManager;
    FTestServer : Boolean;
    FConnection : TFDBConnection;

    procedure checkNotSubsetted(meta : TFHIRMetaW; msg : String);

    function checkOkToStore(request: TFHIRRequest; response: TFHIRResponse; var secure : boolean) : boolean;
    function hasActCodeSecurityLabel(res : TFHIRResourceV; codes : array of string) : boolean;
    function hasConfidentialitySecurityLabel(res : TFHIRResourceV; codes : array of string) : boolean;

    function BuildHistoryResultSet(request: TFHIRRequest; response: TFHIRResponse; var searchKey, link, sql, title, base : String; var total : Integer) : boolean;
    procedure ProcessDefaultSearch(typekey : integer; session : TFHIRSession; aType : String; params : THTTPParameters; baseURL : String; requestCompartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>; id, key : string; op : TFHIROperationOutcomeW; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean);
    procedure ProcessMPISearch(typekey : integer; session : TFHIRSession; aType : String; params : THTTPParameters; baseURL : String; requestCompartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>; id, key : string; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean);
    function loadResourceVersion(versionKey : integer; allowNil : boolean) : TFHIRResourceV;
    procedure updateProvenance(prv : TFhirProvenanceW; inTransaction : boolean; rtype, id, vid : String);

    function GetNewResourceId(aType : String; ForTesting : boolean; var id : string; var key : integer):Boolean;
    function AddNewResourceId(aType, id : String; const lang : THTTPLanguages; forTesting : boolean; var resourceKey : integer) : Boolean;
    Procedure ProcessBlob(request: TFHIRRequest; response : TFHIRResponse; field : String; fmt : TFHIRFormat);
    function ScanId(request : TFHIRRequest; entry : TFHIRBundleEntryW; ids : TFHIRTransactionEntryList; index : integer) : TFHIRTransactionEntry;
    function commitResource(request: TFHIRRequest; response : TFHIRResponse; mode : TOperationMode; entry : TFHIRBundleEntryW; i : integer; id : TFHIRTransactionEntry; session : TFhirSession; resp : TFHIRBundleW) : boolean;
//    Function IsTypeAndId(s : String; var id : String):Boolean;


    procedure SaveProvenance(session : TFhirSession; prv : TFhirProvenanceW);

    procedure CheckCompartments(actual, allowed : TFslList<TFHIRCompartmentId>);
    procedure executeReadInTransaction(entry : TFhirBundleEntryW; request: TFHIRRequest; response : TFHIRResponse);

    procedure ReIndex;
    procedure CheckCreateNarrative(request : TFHIRRequest);

    function GetServerContext: TFHIRServerContext;

    function processCanonicalSearch(request : TFHIRRequest; bundle : TFHIRBundleBuilder) : boolean;
    function resolveReferenceForIndexing(sender : TFhirIndexManager; appInfo : TFslObject; url : String) : TFHIRResourceV;

  protected
    function factory : TFHIRFactory;
    function resolveConditionalURL(request : TFHIRRequest; resp : TFHIRResponse; url : String) : String;
    procedure FixXhtmlUrls(const lang : THTTPLanguages; base: String; ids: TFHIRTransactionEntryList; node: TFhirXHtmlNode);

    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    function  ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    function  ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; override;
    function  ExecutePatch(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; override;
    procedure ExecuteVersionRead(request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteDelete(request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteHistory(request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse); override;
    Function  ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String; override;
    procedure ExecuteUpload(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); override;
    function  ExecuteValidation(request: TFHIRRequest; response : TFHIRResponse; opDesc : String) : boolean; override;
    function ExecuteTransaction(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String; override;
    procedure ExecuteBatch(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); override;
    function ExecuteOperation(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String; override;

    procedure registerOperations; virtual; abstract;
    procedure adjustReferences(request : TFHIRRequest; resp : TFHIRResponse; te : TFHIRTransactionEntry; base : String; entry : TFHIRBundleEntryW; ids : TFHIRTransactionEntryList); virtual; abstract;
    function PerformQuery(context: TFHIRObject; path: String): TFHIRObjectList; virtual; abstract;
    function readRef(ref : TFHIRObject) : string; virtual; abstract;
    function getOpException(op : TFHIRResourceV) : String; virtual; abstract;
    procedure doAuditRestPat(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patients : TArray<String>);
    procedure doAuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patientId : String); virtual; abstract;
    procedure checkProposedContent(session : TFhirSession; request : TFHIRRequest; resource : TFHIRResourceV; tags : TFHIRTagList); virtual; abstract;
    procedure checkProposedDeletion(session : TFHIRSession; request : TFHIRRequest; resource : TFHIRResourceV; tags : TFHIRTagList); virtual; abstract;
  public
    constructor Create(const lang : THTTPLanguages; ServerContext : TFHIRServerContext; repository : TFHIRNativeStorageService; Connection : TFDBConnection);
    destructor Destroy; Override;
    function Link : TFHIRNativeOperationEngine; overload;

    Property Connection : TFDBConnection read FConnection;
    function Repository : TFHIRNativeStorageService;

    function opAllowed(resource : string; command : TFHIRCommandType) : Boolean; override;

    function AddResourceTobundle(request : TFHIRRequest; bundle : TFHIRBundleBuilder; isSecure : boolean; base : String; field : String; fmt : TFHIRFormat; purpose : TFHIRBundleEntrySearchMode; makeRequest : boolean; subsetted : boolean; var type_ : String; patIds : TPatientIdTracker; first : boolean = false) : TFHIRBundleEntryW; overload;
    procedure DefineConformanceResources(base : String); // called after database is created
    function isOkToDeleteSecurityLabel(request: TFHIRRequest; response: TFHIRResponse; system, code : String) : boolean;
    function GraphLookup(appInfo : TFslObject; requestType, id : String; var res : TFHIRResourceV) : boolean;
    function GraphFollowReference(appInfo : TFslObject; context : TFHIRResourceV; reference : TFHIRObject; out targetContext, target : TFHIRResourceV) : boolean;
    function GraphSearch(appInfo : TFslObject; requestType : String; params : TFslList<TGraphQLArgument>) : TFHIRBundleW;
    procedure GraphListResources(appInfo : TFslObject; requestType: String; params : TFslList<TGraphQLArgument>; list : TFslList<TFHIRResourceV>);
    procedure ExecuteGraphQL(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteGraphQLSystem(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteGraphQLInstance(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse);
    procedure processGraphQL(graphql: String; request : TFHIRRequest; response : TFHIRResponse); override;

    function FindSavedSearch(const sId : String; Session : TFHIRSession; typeKey : integer; var id, link, sql, title, base : String; var total : Integer; var summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean): boolean;
    function BuildSearchResultSet(typekey : integer; session : TFHIRSession; aType : String; params : THTTPParameters; baseURL : String; requestCompartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>; op : TFHIROperationOutcomeW; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean):String;
    procedure chooseField(aFormat : TFHIRFormat; summary : TFHIRSummaryOption; loadObjects : boolean; out fieldName : String; out prsrFmt : TFhirFormat; out needsObject : boolean); overload;
    Procedure CollectIncludes(session : TFhirSession; includes : TReferenceList; resource : TFHIRResourceV; path : String); virtual; abstract;
    function getResourceByReference(source : TFHIRResourceV; url : string; req : TFHIRRequest; allowNil : boolean; var needSecure : boolean): TFHIRResourceV;
    function FindResourceVersion(aType : String; sId, sVersionId : String; bAllowDeleted : boolean; var resourceVersionKey : integer; request: TFHIRRequest; response: TFHIRResponse): boolean;
    Procedure LoadTags(tags : TFHIRTagList; ResourceKey : integer);
    procedure CommitTags(tags : TFHIRTagList; key : integer);
    procedure markSubsetted(meta : TFHIRMetaW);
    procedure unmarkSubsetted(meta : TFHIRMetaW);
    procedure CreateIndexer;
    property Indexer : TFHIRIndexManager read FIndexer;
    function loadResources(keys : TList<integer>) : TFslList<TFHIRResourceV>;
    procedure processIncludes(request : TFHIRRequest; session : TFhirSession; secure : boolean; _includes, _reverseIncludes : String; bundle : TFHIRBundleBuilder; keys : TKeyList; base : String; const lang : THTTPLanguages; field : String; fmt : TFHIRFormat; patIds : TPatientIdTracker);

    // called when kernel actually wants to process against the store
//    Function Execute(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String; override;

    function  LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; override;
    function  LookupReferenceS(context : TFslObject; id : String) : TResourceWithReference;
    function FindResource(aType, sId : String; options : TFindResourceOptions; var resourceKey, versionKey : integer; request: TFHIRRequest; response: TFHIRResponse; sessionCompartments : TFslList<TFHIRCompartmentId>): boolean; override;
    function FindResourceConn(conn : TFDBConnection; aType, sId : String; options : TFindResourceOptions; var resourceKey, versionKey : integer; request: TFHIRRequest; response: TFHIRResponse; sessionCompartments : TFslList<TFHIRCompartmentId>): boolean;
    function GetResourceByKey(key : integer; var needSecure : boolean): TFHIRResourceV; override;
    function getResourcesByParam(aType : String; name, value : string; var needSecure : boolean): TFslList<TFHIRResourceV>; override;
    function ResolveSearchId(resourceName : String; requestCompartment: TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>; baseURL, params : String) : TMatchingResourceList; override;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResourceV; override;
    function getResourceByUrl(aType : String; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResourceV; override;
    function EncodeResource(r : TFHIRResourceV; xml : boolean; summary : TFHIRSummaryOption) : TBytes;

    Property TestServer : boolean read FTestServer write FTestServer;
    Property ServerContext : TFHIRServerContext read GetServerContext;

    // index maintenance
    procedure clear(a : TArray<String>);
    procedure storeResources(list: TFslList<TFHIRQueuedResource>; origin : TFHIRRequestOrigin; mode : TOperationMode);

    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;

    // custom resources
//    function loadCustomResources(response : TFHIRResponse; id : String; startup : boolean; names : TStringList) : boolean; overload;
//    function loadCustomResources(response : TFHIRResponse; key : integer; startup : boolean; names : TStringList) : boolean; overload;
  end;

  TFHIRNativeStorageService = class (TFHIRStorageService)
  private
    FDB: TFDBManager;

    FLastSearchKey: integer;
    FLastVersionKey: integer;
    FLastResourceKey: integer;
    FLastEntryKey: integer;
    FLastCompartmentKey: integer;
    FLastAsyncTaskKey : integer;
    FLastObservationKey : integer;
    FLastObservationCodeKey : integer;
    FLastObservationQueueKey : integer;
    FLastAuthorizationKey : integer;
    FLastConnectionKey : Integer;
    FLastClientKey : Integer;
    FTotalResourceCount: integer;
    FNextSearchSweep: TDateTime;
    FServerContext : TFHIRServerContext; // not linked
    FSpaces : TFHIRIndexSpaces;
    FRegisteredValueSets : TFslStringDictionary;

    FQueue: TFslList<TFHIRQueuedResource>;

    procedure LoadExistingResources(conn: TFDBConnection);
    procedure LoadSpaces(conn: TFDBConnection);
    procedure ProcessLoadedResources;

    procedure DoExecuteOperation(request: TFHIRRequest; response: TFHIRResponse; bWantSession: Boolean);
    function DoExecuteSearch(typekey : Integer; compartment : TFHIRCompartmentId; sessionCompartments: TFslList<TFHIRCompartmentId>; params: THTTPParameters; conn: TFDBConnection): String;
    function getTypeForKey(key: integer): String;
    procedure doRegisterTag(tag: TFHIRTag; conn: TFDBConnection);
    procedure checkRegisterTag(tag: TFHIRTag; conn: TFDBConnection);
    procedure RunValidateResource(i : integer; rtype, id : String; bufJson, bufXml : TFslBuffer; b : TStringBuilder);

//    procedure loadCustomResources(guides : TFslStringSet);
    procedure ProcessObservationContent(conn: TFDBConnection; key, rk: integer; obs : TFHIRObservationW; subj : integer; categories : TArray<Integer>);
    procedure ProcessObservation(conn: TFDBConnection; key : integer);
    function loadResource(conn: TFDBConnection; key : integer) : TFHIRResourceV;
    function resolveReference(conn: TFDBConnection; ref : string) : Integer;
    function resolveConcept(conn: TFDBConnection; c : TFHIRCodingW) : Integer; overload;
    function resolveConcept(conn: TFDBConnection; sys, code : String) : Integer; overload;
    procedure ProcessObservationValue(conn: TFDBConnection; key, subj : integer; isComp : boolean; categories, concepts, compConcepts : TArray<Integer>; dt, dtMin, dtMax : TDateTime; value : TFHIRXVersionElementWrapper);
    procedure ProcessObservationValueQty(conn: TFDBConnection; key, subj : integer; isComp : boolean; categories, concepts, compConcepts : TArray<Integer>; dt, dtMin, dtMax : TDateTime; value : TFHIRQuantityW);
    procedure ProcessObservationValueCode(conn: TFDBConnection; key, subj : integer; isComp : boolean; categories, concepts, compConcepts : TArray<Integer>; dt, dtMin, dtMax : TDateTime; value : TFHIRCodeableConceptW);
    procedure storeObservationConcepts(conn: TFDBConnection; ok : integer; isComp : boolean; categories, concepts, compConcepts : TArray<Integer>);
//    function Authorize(conn :  TFDBConnection; patientId : String; patientKey, consentKey, sessionKey : integer; JWT : String; expiry : TDateTime) : string;
  protected
    FLock: TFslLock;
    function GetTotalResourceCount: integer; override;
    procedure checkDefinitions; virtual; abstract;
    function SupportsSubscriptions : boolean; override;
    function SupportsTransactions : boolean; override;
    function SupportsSearch : boolean; override;
    function SupportsHistory : boolean; override;
  public
    constructor Create(DB: TFDBManager; factory : TFHIRFactory); reintroduce;
    destructor Destroy; Override;
    Function Link: TFHIRNativeStorageService; virtual;
    function engineFactory(const lang : THTTPLanguages; usage : String) : TFHIRNativeOperationEngine; virtual; abstract;
    procedure Initialise();
//    procedure SaveResource(res: TFHIRResourceV; dateTime: TFslDateTime; origin : TFHIRRequestOrigin);
    procedure RecordFhirSession(session: TFhirSession); override;
    procedure CloseFhirSession(key: integer); override;
    function ProfilesAsOptionList: String; override;
    function NextVersionKey: integer;
    function NextSearchKey: integer;
    function NextResourceKeySetId(connection : TFDBConnection; aType: String; id: string) : integer;
    function NextResourceKeyGetId(connection : TFDBConnection; aType: String; var id: string): integer;
    function NextEntryKey: integer;
    function NextCompartmentKey: integer;
    function NextAsyncTaskKey: integer;
    function nextObservationKey : integer;
    function nextObservationCodeKey : integer;
    function NextAuthorizationKey : integer;
    function NextConnectionKey : integer;
    function NextClientKey : integer;
    Function GetNextKey(connection : TFDBConnection; keytype: TKeyType; aType: String; var id: string): integer;
    procedure RegisterTag(tag: TFHIRTag; conn: TFDBConnection); overload;
    procedure RegisterTag(tag: TFHIRTag); overload;
    procedure checkProposedResource(session : TFhirSession; needsSecure, created : boolean; request : TFHIRRequest; resource : TFHIRResourceV; tags : TFHIRTagList); virtual; abstract;
    procedure SeeResource(key, vkey, pvkey: integer; id: string; needsSecure, created : boolean; resource: TFHIRResourceV; conn: TFDBConnection; reload: Boolean; session: TFhirSession; const lang : THTTPLanguages; src : TBytes); virtual; abstract;
    procedure checkDropResource(session : TFhirSession; request : TFHIRRequest; resource : TFHIRResourceV; tags : TFHIRTagList);
    procedure DropResource(key, vkey, pvkey: integer; id, resource: string; indexer: TFhirIndexManager; conn: TFDBConnection);
    procedure Sweep; override;
    function ResourceTypeKeyForName(name: String): integer;
    procedure ProcessSubscriptions; override;
    procedure ProcessEmails; override;
    procedure ProcessObservations; override;
    function TrackValueSet(id: String; conn: TFDBConnection; loading : boolean): integer;
    procedure StoreObservation(conn: TFDBConnection; key : integer);
    procedure UnStoreObservation(conn: TFDBConnection; key : integer);

    function ExpandVS(vs: TFHIRValueSetW; ref: string; const lang : THTTPLanguages; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSetW; override;
    function LookupCode(system, version, code: String): String; override;
    procedure QueueResource(session : TFHIRSession; r: TFHIRResourceV); overload; override;
    procedure QueueResource(session : TFHIRSession; r: TFHIRResourceV; dateTime: TFslDateTime); overload; override;
    procedure RunValidation; override;

    procedure recordOAuthLogin(id, client_id, scope, redirect_uri, state, launch : String); override;
    function fetchOAuthDetails(key, status : integer; var client_id, name, redirect, state, scope, launch : String) : boolean; override;
    function fetchOAuthDetails(id : String; var client_id, redirect, state, scope, launch : String) : boolean; overload; override;
    procedure recordOAuthChoice(id : String; scopes, jwt, patient : String); override;
    function hasOAuthSession(id : String; status : integer) : boolean; override;
    function hasOAuthSessionByKey(key, status : integer) : boolean; override;
    procedure updateOAuthSession(id : String; state, key : integer; var client_id : String); override;
    function FetchAuthorization(uuid : String; var PatientId : String; var ConsentKey, SessionKey : Integer; var Expiry : TDateTime; var jwt : String) : boolean; override;
    function RetrieveSession(key : integer; var UserKey, Provider : integer; var Id, Name, Email : String) : boolean; override;

    procedure FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList); override;
    function FetchResource(key : integer) : TFHIRResourceV; override;
    function createAsyncTask(url, id : string; format : TFHIRFormat; secure : boolean) : integer; override;
    procedure updateAsyncTaskStatus(key : integer; status : TAsyncTaskStatus; message : String); override;
    procedure MarkTaskForDownload(key : integer; names : TStringList); override;
    function fetchTaskDetails(id : String; var key : integer; var status : TAsyncTaskStatus; var fmt : TFHIRFormat; var secure : boolean; var message, sourceUrl : String; var transactionTime, expires : TFslDateTime; names : TStringList; var outcome : TBytes): boolean; override;
    procedure setAsyncTaskDetails(key : integer; transactionTime : TFslDateTime; request : String); override;
    procedure recordDownload(key : integer; name : String); override;
    procedure fetchExpiredTasks(tasks : TFslList<TAsyncTaskInformation>); override;
    procedure MarkTaskDeleted(key : integer); override;
    function getClientInfo(id : String) : TRegisteredClientInformation; override;
    function getClientName(id : String) : string; override;
    function storeClient(client : TRegisteredClientInformation; sessionKey : integer) : String; override;
    procedure fetchClients(list : TFslList<TRegisteredClientInformation>); override;

    property ServerContext : TFHIRServerContext read FServerContext write FServerContext;
    Property DB: TFDBManager read FDB;

    function loadPackages : TFslMap<TLoadedPackageInformation>; override;
    function fetchLoadedPackage(id : String) : TBytes; override;
    procedure recordPackageLoaded(id, ver : String; count : integer; blob : TBytes); override;
    function cacheSize : UInt64; override;
    procedure clearCache; override;
  end;

  TFslDateTimeWrapper = class (TFslObject)
  private
    FValue : TFslDateTime;
  public
    constructor Create(value : TFslDateTime);
    property Value : TFslDateTime read FValue;
  end;


function isLogicalReference(s : String) : boolean;
function typeForReference(ref : String) : String;

implementation

uses
  IdMessage, IdSMTP,
  fsl_logging,
  tx_manager, tx_operations,
  search;

function chooseFile(fReal, fDev : String) : String;
begin
  if FileExists(fDev) then
    result := fDev
  else
    result := fReal;
end;



function booleanToSQL(b : boolean): string;
begin
  if b then
    result := '1'
  else
    result := '0';
end;


{ TFHIRNativeOperationEngine }

constructor TFHIRNativeOperationEngine.Create(const lang : THTTPLanguages; ServerContext : TFHIRServerContext; repository : TFHIRNativeStorageService; Connection : TFDBConnection);
begin
  inherited Create(repository, ServerContext, lang);
  FConnection := Connection;

  registerOperations;
end;


Constructor TFslDateTimeWrapper.Create(value : TFslDateTime);
begin
  inherited Create;
  FValue := value;
end;

procedure TFHIRNativeOperationEngine.DefineConformanceResources(base: String);

var
  list : TFslList<TFHIRQueuedResource>;
  q : TFHIRQueuedResource;
  list2 : TFslList<TFHIRResourceV>;
  op : TFHIROperationDefinitionW;
  i : Integer;
begin
  list := TFslList<TFHIRQueuedResource>.create;
  try
    for i := 0 to FOperations.Count - 1 do
    begin
      op := TFhirOperation(FOperations[i]).CreateDefinition(base);
      try
        if (op <> nil) then
        begin
          op.tag := TFslDateTimeWrapper.create(TFslDateTime.makeLocal);
          list.Add(TFHIRQueuedResource.Create(nil, op.Resource.link));
        end;
      finally
        op.Free;
      end;
    end;
    for i := 0 to list.Count - 1 do
      list[i].Resource.Tags['internal'] := '1';
    if factory.version = fhirVersionRelease2 then
    begin
      list2 := TFslList<TFHIRResourceV>.create;
      try
        for q in list do
          list2.add(q.resource.link);
        Repository.ServerContext.TerminologyServer.declareCodeSystems(list2);
      finally
        list2.Free;
      end;
    end;
    storeResources(list, roConfig, opmCmdLine); // not actually sure whrn this is called
  finally
    list.Free;
  end;

end;

destructor TFHIRNativeOperationEngine.Destroy;
begin
/// Just checking: The Create method adds a bunch of Operations, and I'm not sure they are destroyed. To be checked.
///    Possibly also related with TFslObject and TFslList  - as noted in indexing.pas
  FIndexer.Free;
  inherited;
end;

procedure TFHIRNativeOperationEngine.doAuditRestPat(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; opName: String; httpCode: Integer; name, message: String; patients: TArray<String>);
var
  sid : String;
begin
  if length(patients) = 0 then
    doAuditRest(session, intreqid, extreqid, ip, resourceName, id, ver, verkey, op, provenance, opName, httpCode, name, message, '')
  else
   for sid in patients do
     doAuditRest(session, intreqid, extreqid, ip, resourceName, id, ver, verkey, op, provenance, opName, httpCode, name, message, sid);
end;

function TFHIRNativeOperationEngine.AddResourceTobundle(request : TFHIRRequest; bundle : TFHIRBundleBuilder; isSecure : boolean; base : String; field : String; fmt : TFHIRFormat; purpose : TFHIRBundleEntrySearchMode; makeRequest : boolean; subsetted : boolean; var type_ : String; patIds : TPatientIdTracker; first : boolean = false) : TFHIRBundleEntryW;
var
  parser : TFhirParser;
  mem : TBytesStream;
  sId, sAud : String;
  procedure addRequest(entry : TFHIRBundleEntryW);
  begin
    if (makeRequest) then
    begin
      entry.responseDate := TFslDateTime.fromTS(FConnection.ColTimeStampByName['StatedDate'], dttzUTC);
      entry.responseEtag := 'W/'+FConnection.ColStringByName['VersionId'];
      sAud := FConnection.ColStringByName['AuditId'];
      if sAud <> '' then
        entry.links['audit'] := 'AuditEvent/'+sAud;

      if FConnection.ColIntegerByName['Status'] = 1 then
      begin
        entry.requestUrl := AppendForwardSlash(base)+type_+'/'+sId;
        entry.requestMethod := 'PUT';
        entry.Tags['opdesc'] := 'Updated by '+FConnection.ColStringByName['Name']+' at '+entry.responseDate.ToString+ '(UTC)';
        entry.responseStatus := '200 OK';
      end
      else if FConnection.ColIntegerByName['Status'] = 2 then
      begin
        entry.requestUrl := AppendForwardSlash(base)+type_+'/'+sId;
        entry.requestMethod := 'DELETE';
        entry.Tags['opdesc'] := 'Deleted by '+FConnection.ColStringByName['Name']+' at '+entry.responseDate.ToString+ '(UTC)';
        entry.responseStatus := '204 No Content';
      end
      else
      begin
        entry.requestMethod := 'POST';
        entry.requestUrl := AppendForwardSlash(base)+type_;
        entry.Tags['opdesc'] := 'Created by '+FConnection.ColStringByName['Name']+' at '+entry.responseDate.ToString+ '(UTC)';
        entry.responseStatus := '201 Created';
      end;
    end;
  end;
begin
  result := nil;
  sId := FConnection.ColStringByName['Id'];
  type_ := FConnection.colStringByName['ResourceName'];
  if (FConnection.ColIntegerByName['Status'] = 2) then
  begin
    result := bundle.makeEntry;
    try
      addRequest(result);
      result.Url := AppendForwardSlash(base)+type_+'/'+sId;
      bundle.addEntry(result, first);
    finally
      result.Free;
    end;
  end
  else if not isSecure and (FConnection.ColIntegerByName['Secure'] = 1) then
  begin
    if bundle.hasSecureOp then
      exit;
    result := factory.wrapBundleEntry(factory.makeByName('Bundle.entry'));
    try
      result.resource := Factory.BuildOperationOutcome(THTTPLanguages.create('en'), 'Some resources have been omitted from this bundle because they are labelled with a with a security tag that means this server will only send it if the connection is secure', itSuppressed);
      result.resource.Tags[OP_MASK_TAG] := 'secure';
      addRequest(result);
      bundle.addEntry(result, first);
      bundle.hasSecureOp := true;
    finally
      result.Free;
    end;
  end
  else
  begin
    if fmt = ffUnspecified then
    begin
      // currently, we won't get here, because this will never be true - we need to audit each entry against it's patient.
      result := factory.wrapBundleEntry(factory.makeByName('Bundle.entry'));
      try
        result.Element.Tag := TFslBuffer.create;
        result.url := AppendForwardSlash(base)+type_+'/'+sId;
        TFslBuffer(result.Element.Tag).AsBytes := FConnection.ColBlobByName[field];
        result.Element.Tags['type'] := type_;
        if (purpose <> smUnknown) then
        begin
          result.searchMode := purpose;
          if (purpose = smMatch) and not FConnection.ColNullByName['Score1'] then
          begin
            result.searchScore := FloatToStr(FConnection.ColIntegerByName['Score1'] / 100);
            if FConnection.ColIntegerByName['Score2'] > 0 then
              result.searchMpiMatch := CODES_TMPICertainty[TMPICertainty(FConnection.ColIntegerByName['Score2'])];
          end;
        end;
        addRequest(result);
        bundle.addEntry(result, first);
      finally
        result.Free;
      end;
    end
    else
    begin
      mem := TBytesStream.Create(FConnection.ColBlobByName[field]);
      try
        parser := factory.makeParser(ServerContext.ValidatorContext.link, fmt, lang);
        try
          parser.source := mem;
          parser.ParserPolicy := xppDrop;
          parser.Parse;
          result := factory.wrapBundleEntry(factory.makeByName('Bundle.entry'));
          try
            result.resource := parser.resource.Link as TFHIRResourceV;
            if subsetted then
              factory.markWithTag(result.resource, 'http://hl7.org/fhir/v3/ObservationValue', 'SUBSETTED', 'Subsetted');
            patIds.seeIds(patientIds(request, result.resource));
            result.Url := AppendForwardSlash(base)+parser.resource.fhirType+'/'+parser.resource.id;
            if (purpose <> smUnknown) then
            begin
              result.searchmode := purpose;
              if (purpose = smMatch) and not FConnection.ColNullByName['Score1'] then
              begin
                result.searchscore := FloatToStr(FConnection.ColIntegerByName['Score1'] / 100);
                if FConnection.ColIntegerByName['Score2'] > 0 then
                  result.searchMpiMatch := CODES_TMPICertainty[TMPICertainty(FConnection.ColIntegerByName['Score2'])];
              end;
            end;
            addRequest(result);
            bundle.addEntry(result, first);
          finally
            result.Free;
          end;
        finally
          parser.free;
        end;
      finally
        mem.Free;
      end;
    end;
  end;
end;


function TFHIRNativeOperationEngine.EncodeResource(r: TFHIRResourceV; xml : boolean; summary : TFHIRSummaryOption): TBytes;
var
  b : TBytesStream;
  comp : TFHIRComposer;
begin
  b :=  TBytesStream.Create;
  try
    if (xml) then
      comp := factory.makeComposer(ServerContext.ValidatorContext.Link, ffXml, THTTPLanguages.create('en'),  OutputStyleNormal)
    else
      comp := factory.makeComposer(ServerContext.ValidatorContext.Link, ffJson, THTTPLanguages.create('en'),  OutputStyleNormal);
    try
      comp.SummaryOption := summary;
      comp.NoHeader := true;

      comp.Compose(b, r);
    finally
      comp.Free;
    end;
    result := copy(b.Bytes, 0, b.size);
  finally
    b.free;
  end;
end;

Function TFHIRNativeOperationEngine.ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
var
  sId, s : String;
  resourceKey, i : Integer;
  key : Integer;
  tags : TFHIRTagList;
  ok : boolean;
  needSecure : boolean;
  list : TMatchingResourceList;
  src : Tbytes;
  comps : TFslList<TFHIRCompartmentId>;
  mw : TFhirMetaW;
  client : TFhirClientV;
begin
  key := 0;
  CheckCreateNarrative(request);
  try
    ok := true;
    if not check(response, (context.mode in OP_CODES_NO_SEC_ON_INSERT) or request.canWrite(request.ResourceName), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
      ok := false;
    if ok and not check(response, opAllowed(request.ResourceName, fcmdCreate), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
      ok := false;

    if ok and (not check(response, request.Resource <> nil, 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang), itRequired)
         or not check(response, request.ResourceName = request.resource.fhirType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang), itInvalid)) then
      ok := false;

    ok := ok and checkOkToStore(request, response, needSecure);

    if ok and ServerContext.Validate and (context.mode in OP_MODES_CHECK) and (request.Session <> nil) and (request.adaptor = nil) then
    begin
      if not ExecuteValidation(request, response, 'Create Resource '+request.ResourceName+'/'+request.Id+' ('+request.originalId+')') then
        ok := false
      else
        response.Resource := nil;
    end;

    if ok and (request.IfNoneExist <> '') then
    begin
      s := request.IfNoneExist;
      if (s.Contains('?')) then
        s := s.Substring(s.IndexOf('?')+1);
      list := ResolveSearchId(request.ResourceName, request.compartment, request.SessionCompartments, request.baseUrl, s);
      try
        ok := false;
        if list.Count = 1 then
        begin
          response.HTTPCode := 200;
          response.Location := request.baseUrl+request.ResourceName+'/'+Connection.Lookup('Ids', 'ResourceKey', inttostr(list[0].key), 'Id', '')+'/_history/'+Connection.Lookup('Versions', 'ResourceVersionKey', inttostr(list[0].version), 'VersionId', '');
        end
        else if list.Count > 1 then
          check(response, false, 412, lang, GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang), itNotFound)
        else
          ok := true;
      finally
        list.Free;
      end;
    end;

    if ok then
    begin
      mw := factory.wrapMeta(request.Resource);
      try
        mw.lastUpdated := TFslDateTime.makeUTC;
        mw.versionId := '1';
        checkNotSubsetted(mw, 'Creating resource');
        tags := TFHIRTagList.create(factory.link);
        try
          tags.readTags(mw);
          if (request.hasTestingTag) then
            tags.forceTestingTag;
          if not ok then
            // nothing
          else if (idState = idCheckNew) then
          begin
            sId := request.Id;
            if not check(response, sId <> '', 404, lang, GetFhirMessage('MSG_INVALID_ID', lang), itInvalid) or
              not check(response, (Length(sId) <= ID_LENGTH) and AddNewResourceId(request.resourceName, sId, lang, tags.hasTestingTag, resourceKey), 404, lang, GetFhirMessage('MSG_INVALID_ID', lang), itInvalid) then
                ok := false;
          end
          else if (idState = idMaybeNew) and (request.Id <> '') then
          begin
            sId := request.Id;
            if not check(response, (Length(sId) <= ID_LENGTH) and AddNewResourceId(request.resourceName, sId, lang, tags.hasTestingTag, resourceKey), 404, lang, GetFhirMessage('MSG_INVALID_ID', lang), itInvalid) then
              ok := false;
          end
          else if (idState = idIsNew) then
          begin
            sid := request.id;
            resourceKey := iAssignedKey;
          end
          else if not check(response, GetNewResourceId(request.ResourceName, tags.hasTestingTag, sId, resourceKey), 404, lang, StringFormat(GetFhirMessage('MSG_DUPLICATE_ID', lang), [sId, request.ResourceName]), itDuplicate) then
             ok := false

          else
            request.resource.id := sId;

          if ok then
            if not check(response, request.Resource.id = sId, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.Resource.id+'/'+sId+' (1)', itInvalid) then
              ok := false;

          updateProvenance(request.Provenance, context.inTransaction, request.ResourceName, sid, '1');

          if ok then
          begin
            checkProposedContent(request.Session, request, request.Resource, tags);
            Repository.checkProposedResource(request.Session, needSecure, true, request, request.Resource, tags);
            {$IFNDEF NO_JS}
            if GJsHost.HasScripts(ttDataAdded, request.ResourceName) then
            begin
              client := createClient(request.Lang, request.Session);
              try
                GJsHost.checkChanges(ttDataAdded, request.Session, nil, request.Resource, client);
              finally
                client.Free;
              end;
            end;
           {$ENDIF}
            result := sId;
            request.id := sId;
            key := Repository.NextVersionKey;
            for i := 0 to tags.count - 1 do
              Repository.RegisterTag(tags[i], FConnection);

            FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Status, SessionKey, ForTesting, Secure, '+
                    'Tags, XmlContent, XmlSummary, JsonContent, JsonSummary) values (:k, :rk, :sd, :td, :v, :f, 0, :s, :ft, :sec, :tb, :xc, :xs, :jc, :js)';
            FConnection.prepare;
            try
              FConnection.BindInteger('k', key);
              FConnection.BindInteger('rk', resourceKey);
              FConnection.BindTimeStamp('sd', mw.lastUpdated.TimeStamp);
              FConnection.BindTimeStamp('td', mw.lastUpdated.TimeStamp);
              request.SubId := '1';
              FConnection.BindString('v', '1');
              FConnection.BindIntegerFromBoolean('sec', needSecure);
              if request.Session <> nil then
                FConnection.BindInteger('s', request.Session.Key)
              else
                FConnection.BindInteger('s', 0);
              FConnection.BindInteger('f', 2);
              FConnection.BindBlob('tb', tags.json);
              FConnection.BindIntegerFromBoolean('ft', tags.hasTestingTag);
              src := EncodeResource(request.Resource, true, soFull);
              FConnection.BindBlob('xc', src);
              FConnection.BindBlob('jc', EncodeResource(request.Resource, false, soFull));
              markSubsetted(mw);
              FConnection.BindBlob('xs', EncodeResource(request.Resource, true, soSummary));
              FConnection.BindBlob('js', EncodeResource(request.Resource, false, soSummary));
              unmarkSubsetted(mw);
              FConnection.Execute;
            finally
              FConnection.Terminate;
            end;
            CommitTags(tags, key);
            FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 0 where ResourceKey = '+inttostr(resourceKey));
            if ((request.ResourceName = 'AuditEvent') and request.Resource.hasTag('verkey')) then
              FConnection.ExecSQL('update Versions set AuditKey = '+inttostr(resourceKey)+' where ResourceVersionKey = '+request.Resource.Tags['verkey']);

            CreateIndexer;
            comps := FIndexer.execute(resourceKey, sId, request.resource, tags, request);
            try
              CheckCompartments(comps, request.SessionCompartments);
            finally
              comps.free
            end;
            Repository.SeeResource(resourceKey, key, 0, sId, needSecure, true, request.Resource, FConnection, false, request.Session, request.Lang, src);
            if request.resourceName = 'Patient' then
              FConnection.execSQL('update Compartments set CompartmentKey = '+inttostr(resourceKey)+' where Id = '''+sid+''' and CompartmentKey is null');
            response.HTTPCode := 201;
            response.Message := 'Created';
            response.Location := request.baseUrl+request.ResourceName+'/'+sId+'/_history/1';
            response.Resource := request.Resource.Link;
            response.LastModifiedDate := now;

            response.id := sId;
            response.versionId := '1';
            if not context.inTransaction and (request.Provenance <> nil) then
              SaveProvenance(request.Session, request.Provenance);
          end;
        finally
          tags.free;
        end;
      finally
        mw.free;
      end;
    end;
    if request.ResourceName <> 'AuditEvent' then // else you never stop
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, sid, '1', key, request.CommandType, request.Provenance, response.httpCode, '', response.message, patientIds(request, request.Resource));
  except
    on e: exception do
    begin
      if request.ResourceName <> 'AuditEvent' then // else you never stop
        AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, sid, '1', 0, request.CommandType, request.Provenance, 500, '', e.message, patientIds(request, request.Resource));
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteDelete(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  key, nvid, i, versionKey : Integer;
  tnow : TFslDateTime;
  tags : TFHIRTagList;
  list : TMatchingResourceList;
  ok : boolean;
  meta : TFHIRMetaW;
  rp : TFhirParametersW;
  current : TFHIRResourceV;
  client : TFhirClientV;
begin
  key := 0;
  nvid := 0;
  try
    ok := true;
    if not check(response, request.canWrite(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
      ok := false;

    if (request.DefaultSearch) then
    begin
      list := ResolveSearchId(request.ResourceName, request.compartment, request.SessionCompartments, request.baseUrl, request.Parameters.Source);
      try
        ok := false;
        if list.count = 0 then
          NoMatch(request, response)
        else if list.Count > 1 then
          check(response, false, 412, lang, GetFhirMessage('DELETE_MULTIPLE_MATCHES', lang), itNotFound)
        else
        begin
          request.Id := list[0].name;
          ok := true;
        end;
      finally
        list.Free;
      end;
    end;

    if ok and not check(response, length(request.id) <= ID_LENGTH, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id]), itInvalid) then
      ok := false;

    if ok and (not FindResource(request.ResourceName, request.Id, [froFindDeletedResource, froForCommit], resourceKey, versionKey, request, response, nil)) then
    begin
      response.HTTPCode := 204;
      response.Message := 'No Content';
      response.ContentType := 'text/plain';
      response.Body := '';
      ok := false;
    end;

    if ok and FTestServer and not check(response, request.id <> 'example', 400, lang, GetFhirMessage('MSG_RESOURCE_EXAMPLE_PROTECTED', lang), itForbidden) then
      ok := false;

    if ok and ((request.Resource <> nil) and not check(response, request.id = request.Resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.id+'/'+request.resource.id+' (2)', itInvalid)) Then
      ok := false;

    if ok then
    begin
      tnow := TFslDateTime.makeUTC;
      key := Repository.NextVersionKey;
      nvid := FConnection.CountSQL('Select Max(Cast(VersionId as '+DBIntType(FConnection.Owner.Platform)+')) from Versions where ResourceKey = '+IntToStr(resourceKey)) + 1;

      tags := TFHIRTagList.create(factory.link);

      meta := nil;
      rp := factory.wrapParams(factory.makeResource('Parameters'));
      try
        if request.resource <> nil then
        begin
          meta := factory.wrapMeta(request.resource);
          tags.readTags(meta);
        end
        else
          meta := factory.wrapMeta(factory.makeByName('Meta'));
        rp.addParam('meta', meta.Element.Link);
        LoadTags(tags, ResourceKey);
        tags.writeTags(meta);

        checkProposedDeletion(request.session, request, request.Resource, tags);
        Repository.checkDropResource(request.session, request, request.Resource, tags);
        {$IFNDEF NO_JS}
        if GJsHost.HasScripts(ttDataRemoved, request.ResourceName) then
        begin
          current := loadResourceVersion(versionKey, true);
          try
            client := createClient(request.Lang, request.Session);
            try
              GJsHost.checkChanges(ttDataRemoved, request.Session, current, nil, client);
            finally
              client.Free;
            end;
          finally
            current.Free;
          end;
        end;
        {$ENDIF}

        for i := 0 to tags.count - 1 do
          Repository.RegisterTag(tags[i], FConnection);

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Status, SessionKey, Secure, ForTesting, Tags) values '+
                                                             '(:k,:rk, :sd, :td, :v, :f, 2, :s, 0, 0, :t)';
        FConnection.prepare;
        try
          FConnection.BindInteger('k', key);
          FConnection.BindInteger('rk', resourceKey);
          FConnection.BindTimeStamp('sd', tnow.TimeStamp);
          FConnection.BindTimeStamp('td', tnow.TimeStamp);
          FConnection.BindString('v', inttostr(nvid));
          Request.SubId := inttostr(nvid);
          if request.Session = nil then
            FConnection.BindInteger('s', 0)
          else
            FConnection.BindInteger('s', request.Session.Key);
          FConnection.BindInteger('f', 0);
          FConnection.BindBlob('t', tags.json);

          FConnection.Execute;
        finally
          FConnection.Terminate;
        end;
        FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 1 where ResourceKey = '+inttostr(resourceKey));
        CommitTags(tags, key);
        FConnection.execSQL('Update IndexEntries set Flag = 2 where ResourceKey = '+IntToStr(resourceKey));
        CreateIndexer;
        Repository.DropResource(ResourceKey, key, versionKey, request.Id, request.ResourceName, FIndexer, FConnection);
        response.HTTPCode := 204;
        response.Message := GetFhirMessage('MSG_DELETED_DONE', lang);
        if request.resource <> nil then
        begin
          response.HTTPCode := 200;
          response.Message := GetFhirMessage('MSG_DELETED_DONE', lang);
          response.Resource := TFHIRServerContext(FServerContext).Factory.makeByName(request.Resource.fhirType) as TFHIRResourceV;
          response.Resource.id := request.id;
          response.Resource.setProperty('meta', meta.element.link);
        end;
      finally
        tags.free;
        meta.Free;
        rp.Free;
      end;
    end;
    AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, inttostr(nvid), key, request.CommandType, request.Provenance, response.httpCode, '', response.message, patientIds(request, request.Resource));
  except
    on e: exception do
    begin
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, inttostr(nvid), 0, request.CommandType, request.Provenance, 500, '', e.message, patientIds(request, request.Resource));
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteGraphQL(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
begin
  if request.id = '' then
    ExecuteGraphQLSystem(context, request, response)
  else
    ExecuteGraphQLInstance(context, request, response);
end;

procedure TFHIRNativeOperationEngine.ExecuteGraphQLSystem(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  gql : TFHIRGraphQLEngine;
  str : TStringBuilder;
begin
  try
    gql := TFHIRGraphQLEngine.Create((FServerContext as TFHIRServerContext).ValidatorContext.factory.link);
    try
      gql.appInfo := request.Link;
      gql.OnFollowReference := GraphFollowReference;
      gql.OnSearch := GraphSearch;
      gql.OnLookup := GraphLookup;
      gql.OnListResources := GraphListResources;
      if request.GraphQL <> nil then
        gql.GraphQL := request.GraphQL.Link
      else if request.Parameters.has('query') then
        gql.GraphQL := TGraphQLParser.parse(request.Parameters.Value['query'])
      else
        raise EJsonException.Create(GetFhirMessage('GRAPHQL_NOT_FOUND', request.lang));
      gql.focus := nil;
      gql.execute;
      response.Resource := nil;
      str := TStringBuilder.Create;
      try
        str.Append('{'+#13#10);
        str.Append('  "data" : '+#13#10);
        gql.output.write(str, 1);
        str.Append('}'+#13#10);
        response.Body := str.ToString;
      finally
        str.Free;
      end;
      response.ContentType := 'application/json';
    finally
      gql.Free;
    end;
  except
    on e : EJsonException do
    begin
      response.HTTPCode := 400;
      response.Message := GetFhirMessage('GRAPHQL_ERROR', request.Lang);
      response.Resource := Factory.BuildOperationOutcome(request.Lang, e, itInvalid);
    end;
    on e : Exception do
    begin
      response.HTTPCode := 500;
      response.Message := GetFhirMessage('GRAPHQL_ERROR', request.Lang);
      response.Resource := Factory.BuildOperationOutcome(request.Lang, e, itException);
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteGraphQLInstance(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  gql : TFHIRGraphQLEngine;
  str : TStringBuilder;
begin
  try
    if ExecuteRead(request, response, true) then
    begin
      gql := TFHIRGraphQLEngine.Create((FServerContext as TFHIRServerContext).ValidatorContext.factory.link);
      try
        gql.appInfo := request.Link;
        gql.OnFollowReference := GraphFollowReference;
        gql.OnSearch := GraphSearch;
        gql.OnLookup := GraphLookup;
        gql.OnListResources := GraphListResources;
        if request.GraphQL <> nil then
          gql.GraphQL := request.GraphQL.Link
        else if request.Parameters.has('query') then
          gql.GraphQL := TGraphQLParser.parse(request.Parameters.Value['query'])
        else
          raise EJsonException.Create(GetFhirMessage('GRAPHQL_NOT_FOUND', request.Lang));
        gql.focus := response.Resource.Link;
        gql.execute;
        response.Resource := nil;
        str := TStringBuilder.Create;
        try
          str.Append('{'+#13#10);
          str.Append('  "data" : '+#13#10);
          gql.output.write(str, 1);
          str.Append('}'+#13#10);
          response.Body := str.ToString;
        finally
          str.Free;
        end;
        response.ContentType := 'application/json';
      finally
        gql.Free;
      end;
    end;
  except
    on e : EJsonException do
    begin
      response.HTTPCode := 400;
      response.Message := 'Error in GraphQL';
      response.Resource := Factory.BuildOperationOutcome(request.Lang, e, itInvalid);
    end;
    on e : Exception do
    begin
      response.HTTPCode := 500;
      response.Message := 'Error processing GraphQL';
      response.Resource := Factory.BuildOperationOutcome(request.Lang, e, itException);
    end;
  end;
end;


function TFHIRNativeOperationEngine.BuildHistoryResultSet(request: TFHIRRequest; response: TFHIRResponse; var searchKey, link, sql, title, base : String; var total : Integer) : boolean;
var
  cmp : String;
  resourceKey, versionKey : integer;
  lt : string;
  id : String;
  i : integer;
  since, prior : TDateTime;
  rn : String;
begin
  // todo: restrict to readable resources
  id := FhirGUIDToString(CreateGuid); // this the id of the search
  searchKey := inttostr(Repository.NextSearchKey);
  FConnection.ExecSQL('insert into Searches (SearchKey, Id, Count, Type, Date, Summary) values ('+searchKey+', '''+id+''', 0, 2, '+DBGetDate(FConnection.Owner.Platform)+', '+booleanToSQl(false)+')');

  cmp := buildCompartmentsSQL(ServerContext.ResConfig, request.compartment, request.SessionCompartments);

  result := true;
  case request.CommandType of
    fcmdHistoryInstance :
      begin
        TypeNotFound(request, response);
        if not check(response, request.canRead(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
          result := false
        else if (length(request.id) > ID_LENGTH) or not FindResource(request.ResourceName, request.Id, [froFindDeletedResource], resourceKey, versionKey, request, response, nil) then
          result := false
        else
          FConnection.SQL := 'Insert into SearchEntries Select '+searchkey+', Ids.ResourceKey, Versions.ResourceVersionKey, RIGHT (''0000000000000''+CAST(Versions.ResourceVersionKey AS VARCHAR(14)),14) as sort, null, null ' +
           'from Versions, Ids, Sessions '+
           'where Versions.ResourceKey = '+inttostr(resourceKey)+' '+cmp+' and Versions.ResourceKey = Ids.ResourceKey and Versions.SessionKey = Sessions.SessionKey';
        title := StringFormat(GetFhirMessage('MSG_HISTORY', lang), [request.ResourceName+' '+GetFhirMessage('NAME_RESOURCE', lang)+' '+request.Id]);
        base := request.baseUrl+''+request.ResourceName+'/'+request.id+'/_history?';
      end;
    fcmdHistoryType :
      begin
        TypeNotFound(request, response);
        if not check(response, request.canRead(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
          result := false
        else
        begin
          FConnection.SQL := 'Insert into SearchEntries Select '+searchkey+', Ids.ResourceKey, Versions.ResourceVersionKey, RIGHT (''0000000000000''+CAST(Versions.ResourceVersionKey AS VARCHAR(14)),14) as sort, null, null ' +
          'from Versions, Ids, Sessions '+
          'where Ids.ResourceTypeKey = '+inttostr(ServerContext.ResConfig[request.ResourceName].key)+' '+cmp+' and Versions.ResourceKey = Ids.ResourceKey and Versions.SessionKey = Sessions.SessionKey';
          title := StringFormat(GetFhirMessage('MSG_HISTORY', lang), [request.ResourceName]);
          base := request.baseUrl+''+request.ResourceName+'/_history?';
        end;
      end;
    fcmdHistorySystem :
      begin
        if request.Session.canReadAll then
          FConnection.SQL := 'Insert into SearchEntries Select '+searchkey+', Ids.ResourceKey, Versions.ResourceVersionKey, RIGHT (''0000000000000''+CAST(Versions.ResourceVersionKey AS VARCHAR(14)),14) as sort, null, null ' +
            'from Versions, Ids, Sessions '+
            'where Versions.ResourceKey = Ids.ResourceKey '+cmp+' and Versions.SessionKey = Sessions.SessionKey'
        else
        begin
          lt := '';
          for rn in ServerContext.ValidatorContext.allResourceNames do
            if request.canRead(rn) then
              lt := lt+','+inttostr(Repository.ResourceTypeKeyForName(rn));
          FConnection.SQL := 'Insert into SearchEntries Select '+searchkey+', Ids.ResourceKey, Versions.ResourceVersionKey, RIGHT (''0000000000000''+CAST(Versions.ResourceVersionKey AS VARCHAR(14)),14) as sort, null, null ' +
            'from Versions, Ids, Sessions '+
            'where Versions.ResourceKey = Ids.ResourceKey '+cmp+' and Versions.SessionKey = Sessions.SessionKey and Ids.ResourceTypeKey in ('+lt.Substring(1)+')';
        end;

        title := StringFormat(GetFhirMessage('MSG_HISTORY', lang), [GetFhirMessage('NAME_SYSTEM', lang)]);
        base := request.baseUrl+'_history?';
      end;
  else
    FConnection.SQL := '';
  end;

  if result then
  begin
    since := MIN_DATE;
    prior := MIN_DATE;

    link := HISTORY_PARAM_NAME_ID+'='+id;
    for i := 0 to request.Parameters.Count - 1 do
      if request.Parameters.Name[i] = '_since' then
      begin
        since := TFslDateTime.fromXML(request.Parameters.Value[request.Parameters.Name[i]]).DateTime;
        FConnection.SQL := FConnection.SQL + ' and StatedDate >= :since ';
        base := base +'_since='+request.Parameters.Value[request.Parameters.Name[i]];
      end
      else if request.Parameters.Name[i] = '_prior' then
      begin
        prior := TFslDateTime.fromXML(request.Parameters.Value[request.Parameters.Name[i]]).DateTime;
        FConnection.SQL := FConnection.SQL + ' and StatedDate <= :prior ';
        base := base +'&_prior='+request.Parameters.Value[request.Parameters.Name[i]];
      end;
    if (prior = MIN_DATE) then
      base := base +'&_prior='+TFslDateTime.makeUTC.toXML;

    FConnection.SQL := FConnection.SQL+' order by ResourceVersionKey DESC';
    sql := FConnection.SQL;
    FConnection.Prepare;
    try
      if since <> MIN_DATE then
      begin
        FConnection.BindTimeStamp('since', DateTimeToTS(since));
        sql := sql + ' /* since = '+FormatDateTime('yyyy:mm:dd hh:nn:ss', since)+'*/';
      end;
      if prior <> MIN_DATE then
      begin
        FConnection.BindTimeStamp('prior', DateTimeToTS(prior));
        sql := sql + ' /* prior = '+FormatDateTime('yyyy:mm:dd hh:nn:ss', prior)+'*/';
      end;
      FConnection.Execute;
    finally
      FConnection.Terminate;
    end;
    total := FConnection.CountSQL('Select count(*) from SearchEntries where SearchKey = '+searchKey);
    FConnection.Sql := 'update Searches set Title = :t, Base = :b,  Link = :l, SqlCode = :s, count = '+inttostr(total)+' where SearchKey = '+searchkey;
    FConnection.Prepare;
    try
      FConnection.BindBlobFromString('t', title);
      FConnection.BindBlobFromString('b', base);
      FConnection.BindBlobFromString('l', link);
      FConnection.BindBlobFromString('s', sql);
      FConnection.Execute;
    finally
      FConnection.Terminate;
    end;
  end;
end;

function extractProfileId(const lang : THTTPLanguages; base, s : String) : String;
begin
  if s.StartsWith(base+'StructureDefinition/') and s.EndsWith('/$questionnaire') then
    result := s.Substring(0, s.Length-15).Substring(base.Length+8)
  else
    raise EFHIRException.CreateLang('MSG_INVALID_ID', lang);
end;



procedure TFHIRNativeOperationEngine.ExecuteHistory(request: TFHIRRequest; response: TFHIRResponse);
var
  offset, count, i : integer;
  bundle : TFHIRBundleBuilder;
  ok, reverse : boolean;
  id : String;
  dummy : TFHIRSummaryOption;
  link : string;
  sql : String;
  title : String;
  base : String;
//  keys : TStringList;
  total : integer;
  field : String;
  prsrFmt : TFhirFormat;
  needsObject : boolean;
  type_ : String;
  patIds : TPatientIdTracker;
begin
  patIds := TPatientIdTracker.create;
  try
    try
      if request.parameters.value[HISTORY_PARAM_NAME_ID] <> '' then
      begin
        ok := FindSavedSearch(request.parameters.value[HISTORY_PARAM_NAME_ID], request.Session, 2, id, link, sql, title, base, total, dummy, request.strictSearch, reverse);
        if check(response, ok, 400, lang, StringFormat(GetFhirMessage('MSG_HISTORY_EXPIRED', lang), [request.parameters.value[HISTORY_PARAM_NAME_ID]]), itProcessing) then
          link := HISTORY_PARAM_NAME_ID+'='+request.parameters.value[HISTORY_PARAM_NAME_ID]
      end
      else
        ok := BuildHistoryResultSet(request, response, id, link, sql, title, base, total);

      if ok then
      begin
        offset := 0;
        count := 50;
        for i := 0 to request.Parameters.Count - 1 do
          if request.Parameters.Name[i] = SEARCH_PARAM_NAME_OFFSET then
            offset := StrToIntDef(request.Parameters.Value[request.Parameters.Name[i]], 0)
          else if request.Parameters.Name[i] = '_count' then
            count := StrToIntDef(request.Parameters.Value[request.Parameters.Name[i]], 0);
        if (count < 2) then
          count := SEARCH_PAGE_DEFAULT
        else if (Count > SEARCH_PAGE_LIMIT) then
          count := SEARCH_PAGE_LIMIT;
        if offset < 0 then
          offset := 0;

        chooseField(response.Format, soFull, request.loadObjects, field, prsrFmt, needsObject);
        if (not needsObject) and NO_AUDIT_ON_SEARCH then
          prsrFmt := ffUnspecified;

        response.OnCreateBuilder(request, response, btHistory, bundle);
        try
          if response.Format <> ffUnspecified then
            base := base + '&_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
          bundle.setTotal(total);
          bundle.Tag('sql', sql);
          bundle.setLastUpdated(TFslDateTime.makeUTC);
          bundle.addLink('self', base+link);

          if (offset > 0) or (Count < total) then
          begin
            bundle.addLink('first', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
            if offset - count >= 0 then
              bundle.addLink('previous', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
            if offset + count < total then
              bundle.addLink('next', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
            if count < total then
              bundle.addLink('last', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((total div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
          end;

          FConnection.SQL :=
            'Select '+
            '  Ids.ResourceKey, ResourceName, Ids.Id, v.ResourceVersionKey, Audits.Id as AuditId, Ids.MostRecent, VersionId, Secure, StatedDate, Name, v.Status, Secure, Tags, '+field+' '+
            'from '+
            '  Ids, Sessions, SearchEntries, Types, Versions as v '+
            'left outer join Ids as Audits on v.AuditKey = Audits.ResourceKey '+
            'where '+
            '  SearchEntries.ResourceVersionKey = v.ResourceVersionKey and '+
            '  Types.ResourceTypeKey = Ids.ResourceTypeKey and '+
            '  v.SessionKey = Sessions.SessionKey and '+
            '  SearchEntries.ResourceKey = Ids.ResourceKey and '+
            '  SearchEntries.SearchKey = '+id+'  order by SearchEntries.ResourceVersionKey DESC OFFSET '+inttostr(offset)+' ROWS FETCH NEXT '+IntToStr(count+1)+' ROWS ONLY';
            FConnection.Prepare;
            try
              FConnection.Execute;
              while FConnection.FetchNext do
                AddResourceTobundle(request, bundle, request.secure, request.baseUrl, field, prsrFmt, smUnknown, true, request.parameters.has('_summary'), type_, patIds);
            finally
              FConnection.Terminate;
            end;

            bundle.setId(FhirGUIDToString(CreateGUID));
            bundle.tag('sql', sql);

            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.resource := bundle.getBundle;
        finally
          bundle.Free;
        end;
      end;
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message, patIds.ids);
    except
      on e: exception do
      begin
        AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, 500, '', e.message, patIds.ids);
        recordStack(e);
        raise;
      end;
    end;
  finally
    patIds.free;
  end;
end;


function TFHIRNativeOperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse; ignoreHeaders : boolean) : boolean;
var
  resourceKey, versionKey : integer;
  field : String;
  prsrFmt : TFhirFormat;
  needsObject : boolean;
begin
  result := false;
  try
    NotFound(request, response);
    if request.canRead(request.ResourceName) and check(response, opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (length(request.id) <= ID_LENGTH) and FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil) then
      begin
        chooseField(response.Format, request.Summary, request.loadObjects, field, prsrFmt, needsObject);
        FConnection.SQL := 'Select Secure, StatedDate, VersionId, '+field+' from Versions where ResourceKey = '+inttostr(resourceKey)+' and ResourceVersionKey = '+inttostr(versionKey);
        FConnection.Prepare;
        try
          FConnection.Execute;
          if FConnection.FetchNext then
          Begin
            if (FConnection.ColIntegerByName['Secure'] = 1) and not request.secure then
              check(response, false, 403, lang, 'This resource is labelled with a security tag that means this server will only send it if the connection is secure', itSuppressed)
            else if not ignoreHeaders and (request.IfNoneMatch <> '') and (request.IfNoneMatch = FConnection.GetColStringByName('VersionId')) then
            begin
              response.HTTPCode := 304;
              response.Message := 'Not Modified';
              response.Body := '';
            end
            else if not ignoreHeaders and (request.IfModifiedSince <> 0) and (request.IfModifiedSince > TSToDateTime(FConnection.ColTimeStampByName['StatedDate'])) then
            begin
              response.HTTPCode := 304;
              response.Message := 'Not Modified';
              response.Body := '';
            end
            else
            begin
              result := true;
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.versionId := FConnection.GetColStringByName('VersionId');
              response.LastModifiedDate := TSToDateTime(FConnection.ColTimeStampByName['StatedDate']);

              if not (request.ResourceName = 'StructureDefinition') then
                response.links.Add('edit-form', request.baseUrl+request.ResourceName+'/'+request.id+'/$qa-edit');

              response.links.Add('z-edit-src', request.baseUrl+request.ResourceName+'/'+request.id+'/$edit');
              processBlob(request, Response, field, prsrFmt);
            end;
          end;
        finally
          FConnection.Terminate;
        end;
      end;
    end;
    AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, response.httpCode, '', response.message, patientIds(request, response.Resource));
  except
    on e: exception do
    begin
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, 500, '', e.message, patientIds(request, request.Resource));
      recordStack(e);
      raise;
    end;
  end;
end;


procedure TFHIRNativeOperationEngine.executeReadInTransaction(entry : TFhirBundleEntryW; request: TFHIRRequest; response: TFHIRResponse);
  Function NextSegment(var url : String):String;
  var
    i : integer;
  Begin
    i := StringFind(url, ['/', '?']);
    if i = 0 then
    begin
      result := url;
      url := '';
    end
    else
    begin
//      inc(relativeReferenceAdjustment);
      result := copy(url, 1, i-1);
      url := copy(url, i + 1, $FFF);
    end;
  End;
var
  url, s : String;

begin
  url := entry.url;
  if (url.Contains('?')) then
  begin
    request.LoadParams(url.Substring(url.IndexOf('?')+1));
    url := url.Substring(0, url.IndexOf('?'));
  end
  else
    request.LoadParams('');
  request.Id := '';
  request.SubId := '';
  request.OperationName := '';

  s := NextSegment(url);
  if (s = '') then
    raise EFHIRException.CreateLang('NOT_DONE_YET', Request.lang) // system search
  else if (s.StartsWith('$')) then
    raise EFHIRException.CreateLang('NOT_DONE_YET', Request.lang) // system level operation
  else if (s.StartsWith('_')) then
    raise EFHIRException.CreateLang('NOT_DONE_YET', Request.lang) // system history
  else if not StringArrayExistsSensitive(factory.ResourceNames, s) then
    raise EFHIRException.create('Unknown path type "'+entry.url+'"')
  else
  begin
    request.ResourceName := s;
    s := NextSegment(url);
    if (s = '') then
      ExecuteSearch(request, response)
    else if (s.StartsWith('$')) then
      raise EFHIRException.CreateLang('NOT_DONE_YET', Request.lang) // resource type level operation
    else if (s.StartsWith('_')) then
      raise EFHIRException.CreateLang('NOT_DONE_YET', Request.lang) // resource type history
    else if not IsId(s) then
      raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.Lang, [entry.url])
    else
    begin
      request.Id := s;
      s := NextSegment(url);
      if (s = '') then
        ExecuteRead(request, response, false)
      else if (s.StartsWith('$')) then
        raise EFHIRException.CreateLang('NOT_DONE_YET', Request.lang) // resource instance level operation
      else if (s = '_history') then
      begin
        s := NextSegment(url);
        if (s = '') then
          ExecuteHistory(request, response)
        else if not IsId(s) then
          raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.Lang, [entry.url])
        else
        begin
          request.SubId := s;
          s := NextSegment(url);
          if (s = '') then
            ExecuteVersionRead(request, response)
          else
            raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.Lang, [entry.url])
        end;
      end
      else
        raise EFHIRException.CreateLang('MSG_UNKNOWN_TYPE', request.Lang, [entry.url])
    end;
  end;
end;

function TFHIRNativeOperationEngine.FindSavedSearch(const sId : String; Session : TFHIRSession; typeKey : integer; var id, link, sql, title, base : String; var total : Integer; var summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean): boolean;
begin
  // todo: check sesseion...
  if sId = '' then
    result := false
  else
  begin
    FConnection.sql := 'Select SearchKey, SessionKey, Count, Summary, Reverse, Title, Base, Link, SqlCode from Searches where Id = :s and Type = '+inttostr(typeKey);
    FConnection.Prepare;
    try
      FConnection.BindString('s', sId);
      FConnection.Execute;
      result := FConnection.FetchNext;
      if result then
      begin
        if (session.canReadAll and (FConnection.GetColIntegerByName('SessionKey') = 0)) or (Session.Key = FConnection.GetColIntegerByName('SessionKey')) then
        begin
          id := FConnection.GetColStringByName('SearchKey');
          total := FConnection.GetColIntegerByName('Count');
          link := TEncoding.UTF8.GetString(FConnection.GetColBlobByName('Link'));
          sql := TEncoding.UTF8.GetString(FConnection.GetColBlobByName('SqlCode'));
          title := TEncoding.UTF8.GetString(FConnection.GetColBlobByName('Title'));
          base := TEncoding.UTF8.GetString(FConnection.GetColBlobByName('Base'));
          summaryStatus := TFHIRSummaryOption(FConnection.GetColIntegerByName('Summary'));
          reverse := FConnection.GetColIntegerByName('Reverse') > 0;
        end
        else
          result := false;
      end;
    finally
      FConnection.Terminate;
    end;
    FConnection.ExecSQL('update Searches set date = '+DBGetDate(FConnection.Owner.Platform)+' where SearchKey = '+id);
  end;
end;

function TFHIRNativeOperationEngine.BuildSearchResultSet(typekey : integer; session : TFHIRSession; aType : String; params : THTTPParameters; baseURL: String; requestCompartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>; op : TFHIROperationOutcomeW; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean):String;
var
  id : string;
begin
  id := FhirGUIDToString(CreateGuid);
  result := inttostr(Repository.NextSearchKey);
  if params.has('_query') and (params['_query'] <> '') then
  begin
    if (params['_query'] = 'mpi') and (aType = 'Patient') then
      ProcessMPISearch(typekey, session, aType, params, baseURL, requestCompartment, sessionCompartments, id, result, link, sql, total, summaryStatus, strict, reverse)
    else
      raise EFHIRException.createLang('UNKNOWN_QUERY', lang, [params['_query']]);
  end
  else
    ProcessDefaultSearch(typekey, session, aType, params, baseURL, requestCompartment, sessionCompartments, id, result, op, link, sql, total, summaryStatus, strict, reverse);
end;

procedure TFHIRNativeOperationEngine.ProcessDefaultSearch(typekey : integer; session : TFHIRSession; aType : String; params : THTTPParameters; baseURL : String; requestCompartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>; id, key : string; op : TFHIROperationOutcomeW; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean);
var
  sp : TSearchProcessor;
  s : String;
  csql : String;
  iss : TFhirOperationOutcomeIssueW;
begin

  if (session = nil) or session.canReadAll then
    s := 'null'
  else
    s := inttostr(Session.Key);
  FConnection.ExecSQL('insert into Searches (SearchKey, Id, Count, Type, Date, Summary, SessionKey) values ('+key+', '''+id+''', 0, 1, '+DBGetDate(FConnection.Owner.Platform)+', '+inttostr(ord(summaryStatus))+', '+s+')');
  sp := TSearchProcessor.create(ServerContext);
  try
    sp.resConfig := ServerContext.ResConfig.Link;
    sp.strict := strict;
    sp.typekey := typekey;
    sp.type_ := aType;
    sp.compartment := requestCompartment.Link;
    sp.SessionCompartments := sessionCompartments.link;
    sp.baseURL := baseURL;
    sp.lang := lang;
    sp.params := params;
    CreateIndexer;
    sp.indexes := ServerContext.Indexes.Link;
    sp.session := session.link;
    sp.countAllowed := true;
    sp.Connection := FConnection.link;

    sp.build;
    sql := 'Insert into SearchEntries Select '+key+', ResourceKey, MostRecent as ResourceVersionKey, '+sp.sort+', null, null from Ids where Deleted = 0 and MostRecent is not null and '+sp.filter+' order by ResourceKey DESC';
    csql := 'Select count(ResourceKey) from Ids where Deleted = 0 and '+sp.filter;
    link := SEARCH_PARAM_NAME_ID+'='+id+'&'+sp.link_;
    reverse := sp.reverse;
    if (op <> nil) then
      for iss  in sp.Warnings do
        op.addIssue(iss, false);
  finally
    sp.free;
  end;

  if summaryStatus = soCount then
  begin
    total := FConnection.CountSQL(csql);
    sql := csql;
  end
  else
  begin
    FConnection.ExecSQL(sql);
    total := FConnection.CountSQL('Select count(*) from SearchEntries where SearchKey = '+key);
  end;
  FConnection.Sql := 'update Searches set Reverse = :r, Link = :l, SqlCode = :s, count = '+inttostr(total)+', Summary = '+inttostr(ord(summaryStatus))+' where SearchKey = '+key;
  FConnection.Prepare;
  try
    FConnection.BindIntegerFromBoolean('r', reverse);
    FConnection.BindBlobFromString('l', link);
    FConnection.BindBlobFromString('s', sql);
    FConnection.Execute;
  finally
    FConnection.Terminate;
  end;
end;

procedure TFHIRNativeOperationEngine.ProcessMPISearch(typekey : integer; session : TFHIRSession; aType : string; params : THTTPParameters; baseURL : String; requestCompartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>; id, key : string; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean);
var
  mpi : TMPISearchProcessor;
  s : String;
begin
  if (session = nil) then
    raise EFHIRException.create('no session?');

  s := inttostr(Session.Key);
  FConnection.ExecSQL('insert into Searches (SearchKey, Id, Count, Type, Date, Summary, SessionKey) values ('+key+', '''+id+''', 0, 1, '+DBGetDate(FConnection.Owner.Platform)+', '+inttostr(ord(summaryStatus))+', '+s+')');

  mpi := TMPISearchProcessor.create;
  try
    mpi.typekey := typekey;
    mpi.compartment := requestCompartment.Link;
    mpi.sessionCompartments := sessionCompartments.link;
    mpi.baseURL := baseURL;
    mpi.lang := lang;
    mpi.params := params;
    CreateIndexer;
    mpi.indexes := ServerContext.Indexes.Link;
    mpi.session := session.link;
    mpi.Connection := FConnection.link;
    mpi.key := key;
    mpi.execute;
    link := SEARCH_PARAM_NAME_ID+'='+id+'&'+mpi.link_;
    sql := '..mpi..';
    reverse := false;
  finally
    mpi.free;
  end;

  total := FConnection.CountSQL('Select count(*) from SearchEntries where SearchKey = '+key);

  FConnection.Sql := 'update Searches set Link = :l, SqlCode = :s, count = '+inttostr(total)+', Summary = '+inttostr(ord(summaryStatus))+' where SearchKey = '+key;
  FConnection.Prepare;
  try
    FConnection.BindBlobFromString('l', link);
    FConnection.BindBlobFromString('s', sql);
    FConnection.Execute;
  finally
    FConnection.Terminate;
  end;
end;


procedure TFHIRNativeOperationEngine.processGraphQL(graphql: String; request : TFHIRRequest; response : TFHIRResponse);
var
  gql : TFHIRGraphQLEngine;
  str : TStringBuilder;
begin
  try
    gql := TFHIRGraphQLEngine.Create((FServerContext as TFHIRServerContext).ValidatorContext.factory.link);
    try
      gql.appInfo := request.Link;
      gql.OnFollowReference := GraphFollowReference;
      gql.OnSearch := GraphSearch;
      gql.OnLookup := GraphLookup;
      gql.OnListResources := GraphListResources;
      gql.GraphQL := TGraphQLParser.parse(graphql);
      gql.focus := response.resource.Link;
      gql.execute;
      str := TStringBuilder.Create;
      try
        str.Append('{'+#13#10);
        str.Append('  "data" : '+#13#10);
        gql.output.write(str, 1);
        str.Append('}'+#13#10);
        response.resource := nil;
        response.Body := str.ToString;
      finally
        str.Free;
      end;
      response.ContentType := 'application/json';
    finally
      gql.Free;
    end;
  except
    on e : EJsonException do
    begin
      response.HTTPCode := 400;
      response.Message := 'Error in GraphQL';
      response.Resource := Factory.BuildOperationOutcome(request.Lang, e, itInvalid);
    end;
    on e : Exception do
    begin
      response.HTTPCode := 500;
      response.Message := 'Error processing GraphQL';
      response.Resource := Factory.BuildOperationOutcome(request.Lang, e, itException);
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.processIncludes(request : TFHIRRequest; session: TFhirSession; secure : boolean; _includes, _reverseIncludes: String; bundle: TFHIRBundleBuilder; keys : TKeyList; base : String; const lang : THTTPLanguages; field: String; fmt : TFHIRFormat; patIds : TPatientIdTracker);
var
  s, sql : String;
  sl, p : TArray<String>;
  sel : TStringList;
  key2, i: integer;
  type_ : string;
begin
  if ((_includes = '') and (_reverseIncludes = '')) or (keys.count = 0) then
    exit;

  sel := TStringList.Create;
  try
    if _includes.contains(',') then
      sl := _includes.Split([','])
    else
      sl := _includes.Split([';']);
    for s in sl do
    begin
      p := s.Split([':']);
      if (length(p) >= 2) and (length(p) <= 3) then
      begin
        key2 := ServerContext.Indexes.GetKeyByName(p[1]);
        if (key2 = 0) then
          raise EFHIRException.createLang('MSG_PARAM_UNKNOWN', lang, [p[1]]);
        if (length(p) = 3) then
        begin
          sel.Add('Ids.ResourceKey in (select Target from IndexEntries, Ids where IndexKey = '+inttostr(key2)+' and ResourceKey in ('+keys.forType(p[2])+') and Ids.ResourceKey = IndexEntries.Target and Ids.ResoureTypeKey = '+inttostr(ServerContext.ResConfig[p[2]].key)+')');
        end
        else
          sel.Add('Ids.ResourceKey in (select Target from IndexEntries where IndexKey = '+inttostr(key2)+' and ResourceKey in ('+keys.forType(p[0])+'))');
      end
      else
        raise EFHIRException.createLang('MSG_BAD_SYNTAX', lang, [s]);
    end;

    if _reverseIncludes.contains(',') then
      sl := _reverseIncludes.Split([','])
    else
      sl := _reverseIncludes.Split([';']);
    for s in sl do
    begin
      p := s.Split([':']);
      if (length(p) = 2) then
      begin
        key2 := ServerContext.Indexes.GetKeyByName(p[1]);
        if (key2 = 0) then
          raise EFHIRException.createLang('MSG_PARAM_UNKNOWN', lang, [p[0]]);
        sel.Add('Ids.ResourceKey in (select IndexEntries.ResourceKey from IndexEntries, Ids where IndexKey = '+inttostr(key2)+' and Target in ('+keys.forAll+') and Ids.ResourceKey = IndexEntries.ResourceKey and Ids.ResourceTypeKey = '+inttostr(ServerContext.ResConfig[p[0]].key)+')');
      end
      else
        raise EFHIRException.createLang('MSG_BAD_SYNTAX', lang, [s]);
    end;
    if (sel.count > 0) then
    begin
      sql := '('+sel[0]+')';
      for i := 1 to sel.Count - 1 do
        sql := sql +' or ('+sel[i]+')';

      sql := 'Select Ids.ResourceKey, Types.ResourceName, Ids.Id, VersionId, Secure, StatedDate, Name, Versions.Status, Tags, '+field+' from Versions, Ids, Sessions, Types '+
            'where Ids.Deleted = 0 and Types.ResourceTypeKey = Ids.ResourceTypeKey and Versions.SessionKey = Sessions.SessionKey and Versions.ResourceVersionKey = Ids.MostRecent and ('+sql+')';
      FConnection.SQL := sql;
      FConnection.Prepare;
      try
        FConnection.Execute;
        while FConnection.FetchNext do
          AddResourceTobundle(request, bundle, secure, base, field, fmt, smInclude, false, request.parameters.has('_summary'), type_, patIds);
      finally
        FConnection.Terminate;
      end;
    end;
  finally
    sel.Free;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteSearch(request: TFHIRRequest; response: TFHIRResponse);
var
  bundle : TFHIRBundleBuilder;
  id, link, base, sql, field : String;
  i, total, t : Integer;
  key : integer;
  offset, count : integer;
  ok, reverse, cok : boolean;
  summaryStatus : TFHIRSummaryOption;
  title: string;
  keys : TKeyList;
  prsrFmt : TFhirFormat;
  needsObject : boolean;
  op : TFHIROperationOutcomeW;
  type_ : String;
  be : TFHIRBundleEntryW;
  patIds : TPatientIdTracker;
begin
  patIds := TPatientIdTracker.create;
  try
    try
      ok := true;
      { todo:
       conformance
       quantity searches
      }
      if not check(response, request.canRead(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
        // ok := false
      else if (request.Parameters.Count = 0) and (response.Format = ffXhtml) and not request.hasCompartments then
        BuildSearchForm(request, response)
      else
      begin
        TypeNotFound(request, response);
        if request.resourceName <> '' then
        begin
          key := FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+request.ResourceName+'''');
          if not check(response, key > 0, 404, lang, 'Resource Type '+request.ResourceName+' not known', itNotSupported) then
              ok := false;
        end
        else
          key := 0;

        if ok then
        begin
          response.OnCreateBuilder(request, response, btSearchset, bundle);
          op := factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
          keys := TKeyList.Create;
          try
            bundle.setLastUpdated(TFslDateTime.makeUTC);
  //          bundle.base := request.baseUrl;

            summaryStatus := request.Summary;
            if FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], request.Session, 1, id, link, sql, title, base, total, summaryStatus, request.strictSearch, reverse) then
              link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
            else
              id := BuildSearchResultSet(key, request.Session, request.resourceName, request.Parameters, request.baseUrl, request.compartment, request.SessionCompartments, op, link, sql, total, summaryStatus, request.strictSearch, reverse);

            cok := false;
            if (total = 0) and StringArrayExistsInsensitive(factory.CanonicalResources, request.ResourceName) and (request.Parameters.has('url')) then
              cok := processCanonicalSearch(request, bundle);

            if not cok then
            begin
              bundle.setTotal(total);
              bundle.Tag('sql', sql);

              base := AppendForwardSlash(Request.baseUrl)+request.ResourceName+'?';
              if response.Format <> ffUnspecified then
                base := base + '_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
              bundle.addLink('self', base+link);

              offset := StrToIntDef(request.Parameters[SEARCH_PARAM_NAME_OFFSET], 0);
              if request.Parameters[SEARCH_PARAM_NAME_COUNT] = 'all' then
                count := SUMMARY_SEARCH_PAGE_LIMIT
              else
                count := StrToIntDef(request.Parameters[SEARCH_PARAM_NAME_COUNT], 0);
              if (count = 0) and request.Parameters.has(SEARCH_PARAM_NAME_COUNT) then
                summaryStatus := soCount;


              if (summaryStatus <> soCount) then
              begin
                if (count < 1) then
                  count := SEARCH_PAGE_DEFAULT
                else if (summaryStatus = soSummary) and (Count > SUMMARY_SEARCH_PAGE_LIMIT) then
                  count := SUMMARY_SEARCH_PAGE_LIMIT
                else if (summaryStatus = soText) and (Count > SUMMARY_TEXT_SEARCH_PAGE_LIMIT) then
                  count := SUMMARY_TEXT_SEARCH_PAGE_LIMIT
                else if (Count > SEARCH_PAGE_LIMIT) then
                  count := SEARCH_PAGE_LIMIT;

                if (offset > 0) or (Count < total) then
                begin
                  bundle.addLink('first', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
                  if offset - count >= 0 then
                    bundle.addLink('previous', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
                  if offset + count < total then
                    bundle.addLink('next', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
                  if count < total then
                    bundle.addLink('last', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((total div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
                end;

                chooseField(response.Format, summaryStatus, request.loadObjects, field, prsrFmt, needsObject);
                if (not needsObject) and (request.Elements.Count = 0) and not request.Parameters.has('__wantObject') and NO_AUDIT_ON_SEARCH then // param __wantObject is for internal use only
                  prsrfmt := ffUnspecified;

                FConnection.SQL := 'Select Ids.ResourceKey, Types.ResourceName, Ids.Id, VersionId, Secure, StatedDate, Name, Versions.Status, Score1, Score2, Tags, '+field+' from Versions, Ids, Sessions, SearchEntries, Types '+
                    'where Ids.Deleted = 0 and SearchEntries.ResourceVersionKey = Versions.ResourceVersionKey and Types.ResourceTypeKey = Ids.ResourceTypeKey and '+'Versions.SessionKey = Sessions.SessionKey and SearchEntries.ResourceKey = Ids.ResourceKey and SearchEntries.SearchKey = '+id;
                if reverse then
                  FConnection.SQL := FConnection.SQL + ' order by SortValue DESC'
                else
                  FConnection.SQL := FConnection.SQL + ' order by SortValue ASC';
                FConnection.Prepare;
                try
                  FConnection.Execute;
                  i := 0;
                  t := 0;
                  while FConnection.FetchNext do
                  Begin
                    inc(i);
                    if (i > offset) then
                    begin
                      AddResourceTobundle(request, bundle, request.secure, request.baseUrl, field, prsrFmt, smMatch, false, request.parameters.has('_summary'), type_, patIds);
                      keys.Add(TKeyPair.Create(type_, FConnection.ColStringByName['ResourceKey']));
                      inc(t);
                    end;
                    if (t = count) then
                      break;
                  End;
                finally
                  FConnection.Terminate;
                end;
              end;

              processIncludes(request, request.session, request.secure, request.Parameters['_include'], request.Parameters['_revinclude'], bundle, keys, request.baseUrl, request.lang, field, prsrFmt, patIds);
            end;

            bundle.setId(FhirGUIDToString(CreateGUID));
            if (op.issueCount > 0) then
            begin
              be := factory.wrapBundleEntry(factory.makeByName('Bundle.entry'));
              try
                be.resource := op.resource.Link;
                be.searchMode := smOutcome;
                bundle.addEntry(be.Link, false);
              finally
                be.Free;
              end;
            end;

            //bundle.link_List['self'] := request.url;
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.Resource := nil;
            response.resource := bundle.getBundle;
          finally
            keys.Free;
            bundle.Free;
            op.Free;
          end;
        end;
      end;
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message, patIds.ids);
    except
      on e: exception do
      begin
        AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message, patIds.ids);
        recordStack(e);
        raise;
      end;
    end;
  finally
    patids.Free;
  end;
end;

function TFHIRNativeOperationEngine.ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  resourceKey, versionKey : Integer;
  key, nvid, i : Integer;
  s : String;
  tags : TFHIRTagList;
  ok : boolean;
  needSecure : boolean;
  list : TMatchingResourceList;
  src : TBytes;
  meta : TFhirMetaW;
  b : TFHIRBundleW;
  current : TFHIRResourceV;
  client : TFhirClientV;
begin
  CheckCreateNarrative(request);

  nvid := 0;
  key := 0;
  try
    ok := true;
    if ok and not check(response, request.canWrite(request.ResourceName) or opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
      ok := false;

    if ok and (not check(response, request.Resource <> nil, 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang), itRequired) or
       not check(response, request.ResourceName = request.resource.fhirType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang), itInvalid) or
       not check(response, request.id = request.resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+'('+request.id +' / '+request.resource.id+')', itInvalid) or
       not check(response, length(request.id) <= ID_LENGTH, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id]), itInvalid)) then
      ok := false;

    ok := ok and checkOkToStore(request, response, needSecure);
    if ok and ServerContext.Validate and (context.mode in OP_MODES_CHECK) then
    begin
      if not ExecuteValidation(request, response, 'Update Resource '+request.ResourceName+'/'+request.Id) then
        ok := false
      else
        response.Resource := nil;
    end;

    if request.DefaultSearch then // conditional update
    begin
      list := ResolveSearchId(request.ResourceName, request.compartment, request.SessionCompartments, request.baseUrl, request.Parameters.Source);
      try
        if (list.Count = 0) then
        begin
          ExecuteCreate(context, request, response, idMaybeNew, 0);
          ok := false;
        end
        else
        if (list.Count > 1) then
        begin
          ok := false;
          check(response, false, 412, lang, GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang), itNotFound);
        end
        else
        begin
          request.Id := list[0].name;
          request.Resource.id := list[0].name;
        end;
      finally
        list.Free;
      end;
    end;

    if ok and not check(response, request.Resource.id <> '', 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISSING', lang)+' '+request.id+'/'+request.resource.id+' (3)', itRequired) Then
      ok := false;
    if ok and not check(response, request.id = request.Resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.id+'/'+request.resource.id+' (3)', itInvalid) Then
      ok := false;


    result := true;

    if ok and not FindResource(request.ResourceName, request.Id, [froFindDeletedResource, froForCommit], resourceKey, versionKey, request, response, nil) Then
    begin
      ExecuteCreate(context, request, response, idMaybeNew, 0);
      result := false;
      ok := false;
    end;


    if ok and not check(response, not ServerContext.ResConfig[request.ResourceName].versionUpdates or (request.ifMatch <> ''), 412, lang, GetFhirMessage('MSG_VERSION_AWARE', lang), itBusinessRule) then
      ok := false;


    if ok then
    begin
      key := Repository.NextVersionKey;
      nvid := FConnection.CountSQL('Select Max(Cast(VersionId as '+DBIntType(FConnection.Owner.Platform)+')) from Versions where ResourceKey = '+IntToStr(resourceKey)) + 1;

      if  (request.IfMatch <> '') or  ServerContext.ResConfig[request.ResourceName].versionUpdates then
      begin
        s := request.IfMatch;

        if not check(response, s = inttostr(nvid-1), 409, lang, StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), [inttostr(nvid-1), s]), itConflict) then
          ok := false;
      end;
    end;

    if ok then
    begin
      meta := factory.wrapMeta(request.Resource);
      tags := TFHIRTagList.create(factory.link);
      try
        tags.ReadTags(meta);
        LoadTags(tags, ResourceKey);
        if (request.hasTestingTag) then
          tags.forceTestingTag;
        tags.writeTags(meta);
        if checkOkToStore(request, response, needSecure) then
        begin
          meta.lastUpdated := TFslDateTime.makeUTC;
          meta.versionId := inttostr(nvid);
          CheckNotSubsetted(meta, 'Updating Resource');
          updateProvenance(request.Provenance, context.inTransaction, request.ResourceName, request.Id, inttostr(nvid));

          checkProposedContent(request.session, request, request.Resource, tags);
          Repository.checkProposedResource(request.Session, needSecure, true, request, request.Resource, tags);
          {$IFNDEF  NO_JS}
          if GJsHost.HasScripts(ttDataModified, request.ResourceName) then
          begin
            current := loadResourceVersion(versionKey, true);
            try
              client := createClient(request.Lang, request.Session);
              try
                GJsHost.checkChanges(ttDataModified, request.Session, current, request.Resource, client);
              finally
                client.Free;
              end;
            finally
              current.Free;
            end;
          end;
          {$ENDIF}

          for i := 0 to tags.count - 1 do
            Repository.RegisterTag(tags[i], FConnection);

          FConnection.execSQL('Update IndexEntries set Flag = 2 where ResourceKey = '+IntToStr(resourceKey));

          // check whether originalId matches?
          // to do: merging

          FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, TransactionDate, StatedDate, Format, VersionId, Status, '+
            'SessionKey, ForTesting, Secure, Tags, XmlContent, XmlSummary, JsonContent, JsonSummary) values (:k, :rk, :td, :sd, :f, :v, 1, :s, :ft, :sec, :tb, :xc, :xs, :jc, :js)';
          FConnection.prepare;
          try
            FConnection.BindInteger('k', key);
            FConnection.BindInteger('rk', resourceKey);
            FConnection.BindTimeStamp('td', meta.lastUpdated.TimeStamp);
            FConnection.BindTimeStamp('sd', meta.lastUpdated.TimeStamp);
            FConnection.BindString('v', inttostr(nvid));
            FConnection.BindIntegerFromBoolean('sec', needSecure);
            request.SubId := inttostr(nvid);
            if request.Session = nil then
              FConnection.BindNull('s')
            else
              FConnection.BindInteger('s', request.Session.Key);
            FConnection.BindInteger('f', 2);
            FConnection.BindIntegerFromBoolean('ft', tags.hasTestingTag);
            FConnection.BindBlob('tb', tags.json);
            src := EncodeResource(request.Resource, true, soFull);
            FConnection.BindBlob('xc', src);
            FConnection.BindBlob('jc', EncodeResource(request.Resource, false, soFull));
            markSubsetted(meta);
            FConnection.BindBlob('xs', EncodeResource(request.Resource, true, soSummary));
            FConnection.BindBlob('js', EncodeResource(request.Resource, false, soSummary));
            unmarkSubsetted(meta);
            FConnection.Execute;
          finally
            FConnection.Terminate;
          end;
          FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 0 where ResourceKey = '+inttostr(resourceKey));
          CommitTags(tags, key);
          CreateIndexer;
          FIndexer.execute(resourceKey, request.id, request.resource, tags, request).free;
          Repository.SeeResource(resourceKey, key, versionKey, request.id, needSecure, false, request.Resource, FConnection, false, request.Session, request.Lang, src);
          if ((request.ResourceName = 'AuditEvent') and request.Resource.hasTag('verkey')) then
            FConnection.ExecSQL('update Versions set AuditKey = '+inttostr(resourceKey)+' where ResourceVersionKey = '+request.Resource.Tags['verkey']);

          if (response.Resource <> nil) and (response.Resource.fhirType = 'Bundle') then
          begin
            b := factory.wrapBundle(response.Resource.Link);
            try
              b.addEntry('', request.Resource.link);
            finally
              b.Free;
            end;
          end
          else
            response.Resource := request.Resource.Link;
          response.versionId := inttostr(nvid);
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.lastModifiedDate := meta.lastUpdated.DateTime;
          response.Location := request.baseUrl+request.ResourceName+'/'+request.Id+'/_history/'+inttostr(nvid);
          if not context.inTransaction and (request.Provenance <> nil) then
            SaveProvenance(request.Session, request.Provenance);
        end;
      finally
        tags.free;
        meta.Free;
      end;
    end;
    if request.ResourceName <> 'AuditEvent' then // else you never stop
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, inttostr(nvid), key, request.CommandType, request.Provenance, response.httpCode, '', response.message, patientIds(request, request.Resource));
  except
    on e: exception do
    begin
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, inttostr(nvid), 0, request.CommandType, request.Provenance, 500, '', e.message, patientIds(request, request.Resource));
      recordStack(e);
      raise;
    end;
  end;
end;

function TFHIRNativeOperationEngine.ExecutePatch(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  resourceKey, versionKey : Integer;
  key, nvid, i : Integer;
  s : String;
  tags : TFHIRTagList;
  ok : boolean;
  needSecure : boolean;
  list : TMatchingResourceList;
  parser : TFHIRParser;
  comp : TFHIRComposer;
  json, json2 : TJsonObject;
  xml : TMXmlDocument;
  res : TFHIRResourceV;
  ms : TFslMemoryStream;
  src : TBytes;
  meta : TFhirMetaW;
  b : TFHIRBundleW;
  diff : TDifferenceEngine;
  params : TFHIRParametersW;
  current : TFHIRResourceV;
  client : TFhirClientV;
begin
  result := false;
  json := nil;
  xml := nil;
  res := nil;
  nvid := 0;
  key := 0;
  try
    ok := true;
    if ok and not check(response, request.canWrite(request.ResourceName) or opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
      ok := false;

    if ok and (not check(response, (request.Resource <> nil) or (request.patchJson <> nil) or (request.patchXml <> nil), 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang), itRequired) or
       not check(response, length(request.id) <= ID_LENGTH, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id]), itInvalid)) then
      ok := false;

    if ok and request.DefaultSearch then // conditional update
    begin
      list := ResolveSearchId(request.ResourceName, request.compartment, request.SessionCompartments, request.baseUrl, request.Parameters.Source);
      try
        if check(response, list.count = 1, 412, lang, GetFhirMessage('UPDATE_NOT_ONE_MATCH', lang), itNotFound) then
          request.Id := list[0].name
        else
          ok := false;
      finally
        list.Free;
      end;
    end;

    if ok then
      if not FindResource(request.ResourceName, request.Id, [froFindDeletedResource, froForCommit], resourceKey, versionKey, request, response, nil) Then
        ok := false;

    if ok and not check(response, not ServerContext.ResConfig[request.ResourceName].versionUpdates or (request.ifMatch <> ''), 412, lang, GetFhirMessage('MSG_VERSION_AWARE', lang), itBusinessRule) then
      ok := false;


    if ok then
    begin
      key := Repository.NextVersionKey;
      nvid := FConnection.CountSQL('Select Max(Cast(VersionId as '+DBIntType(FConnection.Owner.Platform)+')) from Versions where ResourceKey = '+IntToStr(resourceKey)) + 1;

      if  (request.IfMatch <> '') or  ServerContext.ResConfig[request.ResourceName].versionUpdates then
      begin
        s := request.IfMatch;

        if not check(response, s = inttostr(nvid-1), 412, lang, StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), [inttostr(nvid-1), s]), itConflict) then
          ok := false;
      end;
    end;

    if ok then
    begin
      // ok, now time to actually get the resource, so we can patch it
      if (request.patchJson <> nil) then
        FConnection.SQL := 'Select Secure, StatedDate, VersionId, JsonContent from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc'
      else
        FConnection.SQL := 'Select Secure, StatedDate, VersionId, XmlContent from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
      FConnection.Prepare;
      try
        FConnection.Execute;
        if not check(response, FConnection.FetchNext, 500, lang, 'Resource Not Found internally', itNotFound) then
          ok := false
        else if (request.patchJson <> nil) then
          json := TJSONParser.Parse(FConnection.ColBlobByName['JsonContent'])
        else if (request.patchXml <> nil) then
          xml := TMXmlParser.Parse(FConnection.ColBlobByName['XmlContent'], [xpResolveNamespaces])
        else
        begin
          parser := factory.makeParser(request.Context.link, ffXml, THTTPLanguages.create('en'));
          try
            res := parser.parseResource(FConnection.ColBlobByName['XmlContent']);
          finally
            parser.Free;
          end;
        end;
      finally
        FConnection.Terminate;
      end;
    end;

    if ok then
    begin
      try
        if (request.patchJson <> nil) then
        begin
          json2 := TJsonPatchEngine.applyPatch(json, request.patchJson);
          try
            parser := factory.makeParser(request.Context.link, ffJson, request.lang);
            try
              TFHIRJsonParserBase(parser).parse(json2);
              request.Resource := parser.resource.Link as TFHIRResourceV;
              request.PostFormat := ffJson;
              ms := TFslMemoryStream.Create;
              try
                request.Source.Clear;
                ms.Buffer := request.Source.Link;
                TJSONWriter.writeObject(ms, json2, true);
              finally
                ms.free;
              end;
            finally
              parser.Free;
            end;
          finally
            json2.Free;
          end;
        end
        else if (request.patchXml <> nil) then
        begin
          TXmlPatchEngine.execute(xml, xml, request.patchXml);
          request.Source.AsText := xml.ToXml;
          request.PostFormat := ffXml;
          parser := factory.makeParser(request.Context.link, ffXml, request.lang);
          try
            TFHIRXmlParserBase(parser).element := xml.document.Link;
            parser.parse();
            request.Resource := parser.resource.Link as TFHIRResourceV;
          finally
            parser.Free;
          end;
        end
        else // request.resource <> nil
        begin
          params := factory.wrapParams(request.Resource.link);
          try
            diff := TDifferenceEngine.Create(request.Context.link, factory.link);
            try
              request.Resource := diff.applyDifference(res, params) as TFHIRResourceV;
              comp := factory.makeComposer(request.Context.link, ffXml, THTTPLanguages.create('en'), OutputStyleNormal);
              try
                request.Source.AsBytes := comp.ComposeBytes(request.Resource);
              finally
                comp.Free;
              end;
              request.PostFormat := ffXml;
            finally
              diff.Free;
            end;
          finally
            params.Free;
          end;
        end;
      finally
        json.Free;
        xml.Free;
        res.Free;
      end;

      // ok, now we have a resource.....
      CheckCreateNarrative(request);
      ok := checkOkToStore(request, response, needSecure);
      if ok and ServerContext.Validate then
      begin
        if not ExecuteValidation(request, response, 'Update Resource '+request.ResourceName+'/'+request.Id) then
          ok := false
      else
        response.Resource := nil;
      end;

      if ok and not check(response, request.resource.id <> '', 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISSING', lang)+' '+request.id+'/'+request.resource.id+' (3)', itRequired) Then
        ok := false;
      if ok and not check(response, request.ResourceName = request.resource.fhirType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang), itInvalid) then
        ok := false;
      if ok and not check(response, request.id = request.resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.id+'/'+request.resource.id+' (3)', itInvalid) Then
        ok := false;

      if ok then
      begin
        result := true;
        meta := factory.wrapMeta(request.resource);
        tags := TFHIRTagList.create(factory.link);
        try
          tags.ReadTags(meta);
          LoadTags(tags, ResourceKey);
          tags.writeTags(meta);
          // don't need to recheck the approval on the tags, they must already have been loaded before the earlier check
          meta.lastUpdated := TFslDateTime.makeUTC;
          meta.versionId := inttostr(nvid);
          CheckNotSubsetted(meta, 'Patching Resource');
          updateProvenance(request.Provenance, context.inTransaction, request.ResourceName, request.Id, inttostr(nvid));
          checkProposedContent(request.session, request, request.resource, tags);
          Repository.checkProposedResource(request.Session, needSecure, true, request, request.Resource, tags);
          {$IFNDEF NO_JS}
          if GJsHost.HasScripts(ttDataModified, request.ResourceName) then
          begin
            current := loadResourceVersion(versionKey, true);
            try
              client := createClient(request.Lang, request.Session);
              try
                GJsHost.checkChanges(ttDataModified, request.Session, current, request.Resource, client);
              finally
                client.Free;
              end;
            finally
              current.Free;
            end;
          end;
          {$ENDIF}

          for i := 0 to tags.count - 1 do
            Repository.RegisterTag(tags[i], FConnection);
          FConnection.execSQL('Update IndexEntries set Flag = 2 where ResourceKey = '+IntToStr(resourceKey));

          FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, TransactionDate, StatedDate, Format, VersionId, Status, '+
            'SessionKey, ForTesting, Secure, Tags, XmlContent, XmlSummary, JsonContent, JsonSummary) values (:k, :rk, :td, :sd, :f, :v, 1, :s, :ft, :sec, :tb, :xc, :xs, :jc, :js)';
          FConnection.prepare;
          try
            FConnection.BindInteger('k', key);
            FConnection.BindInteger('rk', resourceKey);
            FConnection.BindTimeStamp('td', meta.lastUpdated.TimeStamp);
            FConnection.BindTimeStamp('sd', meta.lastUpdated.TimeStamp);
            FConnection.BindString('v', inttostr(nvid));
            FConnection.BindIntegerFromBoolean('sec', needSecure);
            request.SubId := inttostr(nvid);
            if request.Session = nil then
              FConnection.BindNull('s')
            else
              FConnection.BindInteger('s', request.Session.Key);
            FConnection.BindInteger('f', 2);
            FConnection.BindIntegerFromBoolean('ft', tags.hasTestingTag);
            FConnection.BindBlob('tb', tags.json);
            src := EncodeResource(request.resource, true, soFull);
            FConnection.BindBlob('xc', src);
            FConnection.BindBlob('jc', EncodeResource(request.resource, false, soFull));
            markSubsetted(meta);
            FConnection.BindBlob('xs', EncodeResource(request.resource, true, soSummary));
            FConnection.BindBlob('js', EncodeResource(request.resource, false, soSummary));
            unmarkSubsetted(meta);
            FConnection.Execute;
          finally
            FConnection.Terminate;
          end;
          FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 0 where ResourceKey = '+inttostr(resourceKey));
          CommitTags(tags, key);
          CreateIndexer;
          FIndexer.execute(resourceKey, request.id, request.resource, tags, request).free;
          Repository.SeeResource(resourceKey, key, versionKey, request.id, needSecure, false, request.resource, FConnection, false, request.Session, request.Lang, src);

          if (response.Resource <> nil) and (response.Resource.fhirType = 'Bundle') then
          begin
            b := factory.wrapBundle(response.Resource.Link);
            try
              b.addEntry('', request.Resource.link);
            finally
              b.Free;
            end;
          end
          else
            response.Resource := request.resource.Link;
          response.versionId := inttostr(nvid);

          response.HTTPCode := 200;
          response.Message := 'OK';
          response.lastModifiedDate := meta.lastUpdated.DateTime;
          response.Location := request.baseUrl+request.ResourceName+'/'+request.Id+'/_history/'+inttostr(nvid);
          if not context.inTransaction and (request.Provenance <> nil) then
            SaveProvenance(request.Session, request.Provenance);
        finally
          tags.free;
          meta.Free;
        end;
      end;
    end;
    AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, inttostr(nvid), key, request.CommandType, request.Provenance, response.httpCode, '', response.message, patientIds(request, request.Resource))
  except
    on e: exception do
    begin
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, inttostr(nvid), 0, request.CommandType, request.Provenance, 500, '', e.message, patientIds(request, request.Resource));
      recordStack(e);
      raise;
    end;
  end;
end;


function TFHIRNativeOperationEngine.ExecuteValidation(request: TFHIRRequest; response: TFHIRResponse; opDesc : String) : boolean;
var
  ctxt : TFHIRValidatorContext;
  b : TFHIRBinaryW;
  op : TFhirOperationOutcomeW;
  iss : TFhirOperationOutcomeIssueW;
begin
  try
    if opDesc = '' then
      opDesc := 'Validate resource '+request.id;

    if request.resourceName = 'Binary' then
    begin
      op := factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
      try
        response.outcome := op.link;
        b := factory.wrapBinary(request.Resource.link);
        try
          if b.contentType = '' then
            op.addIssue(factory.makeIssue(isError, itInvalid, 'Binary', 'A contentType is required'), true);
        finally
          b.Free;
        end;
      finally
        op.Free;
      end;
    end
    else if (request.Source <> nil) and (request.postFOrmat <> ffText) then
    begin
      ctxt := TFHIRValidatorContext.Create;
      try
        ctxt.IsAnyExtensionsAllowed := true;
        ctxt.ResourceIdRule := risOptional;
        ctxt.OperationDescription := opDesc;
        ServerContext.Validator.validate(ctxt, request.Source, request.PostFormat);
        response.outcome := ServerContext.Validator.describe(ctxt);
      finally
        ctxt.Free;
      end;
    end
    else
    begin
      ctxt := TFHIRValidatorContext.Create;
      try
        ctxt.IsAnyExtensionsAllowed := true;
        ctxt.ResourceIdRule := risOptional;
        ctxt.OperationDescription := opDesc;
        ServerContext.Validator.validate(ctxt, request.Resource);
        response.outcome := ServerContext.Validator.describe(ctxt);
      finally
        ctxt.Free;
      end;
    end;

    if response.outcome.issueCount = 0 then
      response.outcome.addIssue(factory.makeIssue(isInformation, itInformational, '', 'No issues detected during validation'), true);
    result := true;
    for iss in response.outcome.issues.forEnum do
      result := result and (iss.severity in [isInformation, isWarning]);
    if result then
      response.HTTPCode := 200
    else
    begin
      response.HTTPCode := 400;
      response.Resource := response.outcome.Resource.Link;
    end;
    if request.ResourceName <> 'AuditEvent' then
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message, patientIds(request, request.Resource));
  except
    on e: exception do
    begin
      if request.ResourceName <> 'AuditEvent' then
        AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, 500, '', e.message, patientIds(request, request.Resource));
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteVersionRead(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey, versionKey : integer;
  field : String;
  prsrFmt : TFhirFormat;
  needsObject : boolean;
begin
  try
    NotFound(request, response);
    if check(response, request.canRead(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (length(request.id) <= ID_LENGTH) and (length(request.subid) <= ID_LENGTH) and FindResource(request.ResourceName, request.Id, [froFindDeletedResource], resourceKey, versionKey, request, response, nil) then
      begin
        VersionNotFound(request, response);
        chooseField(response.Format, request.Summary, request.loadObjects, field, prsrFmt, needsObject);

        FConnection.SQL := 'Select Secure, VersionId, StatedDate, '+field+' from Versions where ResourceKey = '+inttostr(resourceKey)+' and VersionId = :v';
        FConnection.Prepare;
        try
          FConnection.BindString('v', request.SubId);
          FConnection.Execute;
          if FConnection.FetchNext then
          Begin
            if (FConnection.ColIntegerByName['Secure'] = 1) and not request.secure then
              check(response, false, 403, lang, 'This resource is labelled with a security tag that means this server will only send it if the connection is secure', itSuppressed)
            else if (request.IfNoneMatch <> '') and (request.IfNoneMatch = FConnection.GetColStringByName('VersionId')) then
            begin
              response.HTTPCode := 304;
              response.Message := 'Not Modified';
              response.Body := '';
            end
            else if (request.IfModifiedSince <> 0) and (request.IfModifiedSince > TSToDateTime(FConnection.ColTimeStampByName['StatedDate'])) then
            begin
              response.HTTPCode := 304;
              response.Message := 'Not Modified';
              response.Body := '';
            end
            else
            begin
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.versionId := FConnection.GetColStringByName('VersionId');
              response.LastModifiedDate := TSToDateTime(FConnection.ColTimeStampByName['StatedDate']);
              processBlob(request, response, field, prsrFmt);
            end;
          end;
        finally
          FConnection.Terminate;
        end;
      end;
    end;
    AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.SubId, 0, request.CommandType, request.Provenance, response.httpCode, '', response.message, patientIds(request, response.Resource));
  except
    on e: exception do
    begin
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, request.SubId, 0, request.CommandType, request.Provenance, 500, '', e.message, patientIds(request, response.Resource));
      recordStack(e);
      raise;
    end;
  end;
end;

function TFHIRNativeOperationEngine.factory: TFHIRFactory;
begin
  result := ServerContext.Factory;
end;

function TFHIRNativeOperationEngine.FindResource(aType : String; sId: String; options : TFindResourceOptions; var resourceKey, versionKey: integer; request: TFHIRRequest; response : TFhirResponse; sessionCompartments : TFslList<TFHIRCompartmentId>): boolean;
begin
  result := FindResourceConn(FConnection, aType, sId, options, resourceKey, versionKey, request, response, sessionCompartments);
end;

function TFHIRNativeOperationEngine.FindResourceConn(conn : TFDBConnection; aType : String; sId: String; options : TFindResourceOptions; var resourceKey, versionKey: integer; request: TFHIRRequest; response : TFhirResponse; sessionCompartments : TFslList<TFHIRCompartmentId>): boolean;
var
  cmp : String;
begin
  if (request <> nil) then
    cmp := buildCompartmentsSQL(ServerContext.ResConfig, request.compartment, request.SessionCompartments)
  else
    cmp := buildCompartmentsSQL(ServerContext.ResConfig, nil, sessionCompartments);

  conn.sql := 'select Ids.ResourceKey, Ids.MostRecent, Deleted from Ids, Types where Ids.id = :s '+cmp+' and Ids.ResourceTypeKey = Types.ResourceTypeKey and Supported = 1 and ResourceName = :n';
  conn.Prepare;
  conn.BindString('s', sId);
  conn.BindString('n', aType);
  conn.execute;
  result := conn.FetchNext;
  if result then
  begin
    if (froFindDeletedResource in options) or (conn.ColIntegerByName['Deleted'] = 0) then
    begin
      resourceKey := conn.ColIntegerByName['ResourceKey'];
      versionKey := conn.ColIntegerByName['MostRecent'];
    end
    else
    begin
      if response <> nil then
        check(response, false, 410, lang, StringFormat(GetFhirMessage('MSG_DELETED_ID', lang), [sId]), itNotFound);
      result := false;
    end;
  end
  else
  begin
    if froForCommit in options then
    begin
      // now we'll look again
      conn.terminate;
      conn.sql := 'select Ids.ResourceKey, Ids.MostRecent, Deleted from Ids, Types where Ids.id = :s and Ids.ResourceTypeKey = Types.ResourceTypeKey and Supported = 1 and ResourceName = :n';
      conn.Prepare;
      conn.BindString('s', sId);
      conn.BindString('n', aType);
      conn.execute;
      if conn.FetchNext then
      begin
        check(response, false, 403, lang, StringFormat(GetFhirMessage('MSG_NO_ACCESS', lang), [aType+'/'+sid]), itSecurity);
        abort;
      end;
    end;
    if response <> nil then
      check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [aType+'/'+sid]), itNotFound);
  end;
  conn.terminate;
end;

function TFHIRNativeOperationEngine.AddNewResourceId(aType, id : String; const lang : THTTPLanguages; forTesting : boolean; var resourceKey : integer) : Boolean;
var
  itype : integer;
  exists : boolean;
begin
  iType := ServerContext.ResConfig[aType].key;// FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[aType]+'''');
  result := iType > 0;
  if result then
  begin
    resourceKey := Repository.NextResourceKeySetId(FConnection, aType, id);

    FConnection.SQL := 'Select ResourceKey from Ids where ResourceTypeKey = :r and Id = :i';
    FConnection.Prepare;
    FConnection.BindInteger('r', itype);
    FConnection.BindString('i', id);
    FConnection.Execute;
    exists := FConnection.fetchNext;
    FConnection.Terminate;

    if exists then
      raise ERestfulException.create('TFHIRNativeOperationEngine.AddNewResourceId', 404, itDuplicate, 'MSG_DUPLICATE_ID', lang, [id, aType]);

    FConnection.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, Deleted, ForTesting) values (:k, :r, :i, null, 0, :ft)';
    FConnection.Prepare;
    FConnection.BindInteger('k', resourceKey);
    FConnection.BindInteger('r', itype);
    FConnection.BindIntegerFromBoolean('ft', forTesting);
    FConnection.BindString('i', id);
    FConnection.Execute;
    FConnection.Terminate;
    if IsNumericString(id) and StringIsInteger32(id) then
      FConnection.ExecSQL('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(iType)+' and LastId < '+id);
    FConnection.ExecSQL('update IndexEntries set target = '+inttostr(resourceKey)+' where SpaceKey = '+inttostr(iType)+' and value = '''+id+'''');
  end;
End;

function TFHIRNativeOperationEngine.getNewResourceId(aType: String; ForTesting : boolean; var id: string; var key: integer): Boolean;
var
  itype : integer;
  guid : boolean;
begin
  guid := false;
  iType := 0;
  FConnection.SQL := 'select ResourceTypeKey, LastId, IdGuids from Types where supported = 1 and ResourceName = '''+aType+'''';
  FConnection.Prepare;
  FConnection.Execute;
  if FConnection.FetchNext then
  begin
    iType := FConnection.ColIntegerByName['ResourceTypeKey'];
    guid := FConnection.ColIntegerByName['IdGuids'] = 1;
  end;
  FConnection.Terminate;
  result := iType > 0;
  if result then
  begin
    if guid then
    begin
      id := FhirGUIDToString(CreateGUID);
      key := Repository.NextResourceKeySetId(FConnection, aType, id);
    end
    else
    begin
      key := Repository.NextResourceKeyGetId(FConnection, aType, id);
      FConnection.ExecSQL('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(iType), 1);
    end;
    FConnection.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, Deleted, ForTesting) values (:k, :r, :i, null, 0, :ft)';
    FConnection.Prepare;
    FConnection.BindInteger('k', key);
    FConnection.BindInteger('r', itype);
    FConnection.BindString('i', id);
    FConnection.BindIntegerFromBoolean('ft', ForTesting);
    FConnection.Execute;
    FConnection.Terminate;
    FConnection.ExecSQL('update IndexEntries set target = '+inttostr(key)+' where SpaceKey = '+inttostr(iType)+' and value = '''+id+'''');
  end;
end;

procedure TFHIRNativeOperationEngine.checkNotSubsetted(meta: TFHIRMetaW; msg: String);
begin
  if meta.HasTag('http://hl7.org/fhir/v3/ObservationValue', 'SUBSETTED') then
    raise EFHIRException.createLang('SUBSETTED', lang, [msg]);
end;

function TFHIRNativeOperationEngine.hasActCodeSecurityLabel(res : TFHIRResourceV; codes : array of string) : boolean;
var
  c : TFhirCodingW;
  m : TFhirMetaW;
begin
  m := factory.wrapMeta(res);
  try
    result := false;
    for c in m.labels.forEnum do
      if (c.systemUri = 'http://hl7.org/fhir/v3/ActCode') and StringArrayExistsSensitive(codes, c.code) then
        exit(true);
  finally
    m.Free;
  end;
end;

function TFHIRNativeOperationEngine.hasConfidentialitySecurityLabel(res: TFHIRResourceV; codes: array of string): boolean;
var
  c : TFhirCodingW;
  m : TFhirMetaW;
begin
  m := factory.wrapMeta(res);
  try
    result := false;
    for c in m.labels.forEnum do
      if (c.systemUri = 'http://hl7.org/fhir/v3/Confidentiality') and StringArrayExistsSensitive(codes, c.code) then
        exit(true);
  finally
    m.Free;
  end;
end;

function TFHIRNativeOperationEngine.isOkToDeleteSecurityLabel(request: TFHIRRequest; response: TFHIRResponse; system, code : String): boolean;
begin
  result := check(response, request.secure or (system <> 'http://hl7.org/fhir/v3/ActCode') or not
    StringArrayExistsSensitive(['CPLYCD', 'CPLYJPP', 'CPLYOPP', 'CPLYOSP', 'CPLYPOL', 'ENCRYPT', 'ENCRYPTR', 'ENCRYPTT', 'ENCRYPTU',
                'ETH', 'GDIS', 'HIV', 'PSY', 'SCA', 'SDV', 'SEX', 'STD', 'TBOO', 'SICKLE', 'DEMO', 'DOB', 'GENDER', 'LIVARG', 'MARST', 'RACE', 'REL',
                'B', 'EMPL', 'LOCIS', 'SSP', 'ADOL', 'CEL', 'DIA', 'DRGIS', 'EMP', 'PDS', 'PRS'], code),
        403, lang, 'This security label can only be deleted on a secure connection', itSuppressed);
  if result then
    result := check(response, request.secure or (system <> 'http://hl7.org/fhir/v3/Confidentiality') or not
            StringArrayExistsSensitive(['L', 'M', 'N', 'R', 'U', 'V', 'B', 'D', 'I', 'ETH', 'HIV', 'PSY', 'SDV', 'C', 'S', 'T'], code),
        403, lang, 'This security label can only be deleted on a secure connection', itSuppressed);
end;

function TFHIRNativeOperationEngine.Link: TFHIRNativeOperationEngine;
begin
  result := TFHIRNativeOperationEngine(inherited Link);
end;

//function TFHIRNativeOperationEngine.loadCustomResources(response : TFHIRResponse; key: integer; startup : boolean; names : TStringList) : boolean;
//!{$IFDEF FHIR3}
//var
//  ig : TFHIRImplementationGuide;
//  package : TFhirImplementationGuidePackage;
//  needsSecure : boolean;
//  crs : TFslList<TFHIRCustomResourceInformation>;
//  cr : TFHIRCustomResourceInformation;
//  sp : TFhirSearchParameter;
//{$ENDIF}
//begin
//  result := false;
//!{$IFDEF FHIR3}
//  crs := TFslList<TFHIRCustomResourceInformation>.create;
//  try
//    ig := GetResourceByKey(key, needsSecure) as TFHIRImplementationGuide;
//    try
//      for package in ig.packageList do
//        crs.add(loadCustomResource(ig, package));
//      for cr in crs do
//      begin
//        names.add(cr.Name);
//        result := check(response, not ServerContext.ValidatorContext.hasCustomResource(cr.Name), 400, lang, 'Custom resource is already defined', itBusinessRule) and
//                  check(response, startup or CustomResourceNameIsOk(cr.Name), 400, lang, 'Custom resource is already defined', itBusinessRule);
//      end;
//      if result then
//        for cr in crs do
//        begin
//          for sp in cr.SearchParameters do
//            ServerContext.Indexes.Indexes.add(cr.Name, nil); // sp);
//          ServerContext.ValidatorContext.registerCustomResource(cr);
//        end;
//    finally
//      ig.Free;
//    end;
//  finally
//    crs.free;
//  end;
//{$ENDIF}
//end;

function TFHIRNativeOperationEngine.loadResources(keys: TList<integer>): TFslList<TFHIRResourceV>;
var
  parser : TFHIRParser;
  s : TBytes;
  sb : TFslStringBuilder;
  k : integer;
begin
  result := TFslList<TFHIRResourceV>.create;
  try
    sb := TFslStringBuilder.create;
    try
      for k in keys do
        sb.CommaAdd(inttostr(k));

      FConnection.SQL := 'Select JsonContent from Versions where ResourceKey in ('+sb.AsString+') order by ResourceVersionKey desc';
      FConnection.Prepare;
      try
        FConnection.Execute;
        while FConnection.FetchNext do
        begin
          s := FConnection.ColBlobByName['JsonContent'];
          parser := factory.makeParser(ServerContext.ValidatorContext.link, ffJson, lang);
          try
            result.Add(parser.parseResource(s));
          finally
            parser.free;
          end;
        end;
      finally
        FConnection.Terminate;
      end;
    finally
      sb.Free;
    end;
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRNativeOperationEngine.loadResourceVersion(versionKey: integer; allowNil : boolean): TFHIRResourceV;
var
  parser : TFHIRParser;
  s : TBytes;
begin
  FConnection.SQL := 'Select JsonContent from Versions where ResourceVersionKey = '+inttostr(versionKey);
  FConnection.Prepare;
  try
    FConnection.Execute;
    if not FConnection.FetchNext then
      if allowNil then
        exit(nil)
      else
        raise EFHIRException.create('Unable to find previous version of resource');
    s := FConnection.ColBlobByName['JsonContent'];
    parser := factory.makeParser(ServerContext.ValidatorContext.link, ffJson, lang);
    try
      result := parser.parseresource(s);
    finally
      parser.free;
    end;
  finally
    FConnection.Terminate;
  end;
end;

//function TFHIRNativeOperationEngine.loadCustomResources(response : TFHIRResponse; id: String; startup : boolean; names : TStringList) : boolean;
//!{$IFDEF FHIR3}
//var
//  ig : TFHIRImplementationGuide;
//  package : TFhirImplementationGuidePackage;
//  needsSecure : boolean;
//  crs : TFslList<TFHIRCustomResourceInformation>;
//  cr : TFHIRCustomResourceInformation;
//  sp : TFhirSearchParameter;
//{$ENDIF}
//begin
//  result := false;
//!{$IFDEF FHIR3}
//  crs := TFslList<TFHIRCustomResourceInformation>.create;
//  try
//    ig := GetResourceById(nil, 'ImplementationGuide', id, '', needsSecure) as TFHIRImplementationGuide;
//    try
//      for package in ig.packageList do
//        crs.add(loadCustomResource(ig, package));
//      result := true;
//      for cr in crs do
//      begin
//        names.Add(cr.Name);
//        result := result and check(response, not ServerContext.ValidatorContext.hasCustomResource(cr.Name), 400, lang, 'Custom resource is already defined', itBusinessRule) and
//                  check(response, startup or CustomResourceNameIsOk(cr.Name), 400, lang, 'Custom resource is already defined', itBusinessRule);
//      end;
//      if result then
//        for cr in crs do
//        begin
//          for sp in cr.SearchParameters do
//            ServerContext.Indexes.Indexes.add(cr.Name, nil); // sp);
//          ServerContext.ValidatorContext.registerCustomResource(cr);
//        end;
//    finally
//      ig.Free;
//    end;
//  finally
//    crs.free;
//  end;
//{$ENDIF}
//end;
//
//!{$IFDEF FHIR3}
//function TFHIRNativeOperationEngine.loadCustomResource(ig : TFHIRImplementationGuide; package : TFhirImplementationGuidePackage) : TFHIRCustomResourceInformation;
//var
//  rd : TFhirImplementationGuidePackageResource;
//  res : TFHIRResourceV;
//  sd : TFhirStructureDefinition;
//  list : TFslList<TFhirSearchParameter>;
//  cr : TFHIRCustomResourceInformation;
//  needsSecure : boolean;
//begin
//  sd := nil;
//  list := TFslList<TFhirSearchParameter>.create;
//  try
//    for rd in package.resourceList do
//    begin
//      if rd.source = nil then
//        res := nil
//      else if rd.source is TFhirUri then
//        res := getResourceByUrl(frtNull, (rd.source as TFhirUri).value, '', true, needsSecure)
//      else
//        res := getResourceByReference(ig, (rd.source as TFhirReference).reference, nil, true, needsSecure);
//
//      if res <> nil then // for now, we just ignore them...
//      begin
//        if res is TFHIRStructureDefinition then
//          if sd <> nil then
//            raise EFHIRException.create('Multiple structure definitions in a package; cannot load as a custom resource')
//          else
//            sd := TFHIRStructureDefinition(res);
//        if res is TFhirSearchParameter then
//          list.Add(TFhirSearchParameter(res).Link);
//      end;
//    end;
//    if sd = nil then
//      raise EFHIRException.create('No structure definitions in a package; cannot load as a custom resource');
//    cr := TFHIRCustomResourceInformation.Create(sd.Link);
//    try
//      cr.SearchParameters.AddAll(list);
//      result := cr.Link;
//    finally
//      cr.Free;
//    end;
//  finally
//    list.Free;
//  end;
//end;
//{$ENDIF}

function TFHIRNativeOperationEngine.checkOkToStore(request: TFHIRRequest; response: TFHIRResponse; var secure : boolean): boolean;
begin
  // check the security labels
  secure := false;
  result := check(response,
      not hasActCodeSecurityLabel(request.Resource, ['CPLYCD', 'CPLYJPP', 'CPLYOPP', 'CPLYOSP', 'CPLYPOL']),
        403, lang, 'This resource is labelled with a security tag that requires compliance with a policy, but no policy is known', itNotSupported);
  if result then
    if hasActCodeSecurityLabel(request.Resource, ['ENCRYPT', 'ENCRYPTR', 'ENCRYPTT', 'ENCRYPTU']) then
    begin
      secure := true;
      result := check(response, request.secure, 403, lang, 'This resource is labelled with a security tag that requires encryption, but encryption was not used', itBusinessRule);
    end;
  // extra conditions
  if result and not secure then
    secure :=
      hasActCodeSecurityLabel(request.Resource, ['ETH', 'GDIS', 'HIV', 'PSY', 'SCA', 'SDV', 'SEX', 'STD', 'TBOO', 'SICKLE', 'DEMO', 'DOB', 'GENDER', 'LIVARG',
                  'MARST', 'RACE', 'REL', 'B', 'EMPL', 'LOCIS', 'SSP', 'ADOL', 'CEL', 'DIA', 'DRGIS', 'EMP', 'PDS', 'PRS']) or
      hasConfidentialitySecurityLabel(request.Resource, ['R', 'U', 'V', 'B', 'D', 'I', 'ETH', 'HIV', 'PSY', 'SDV', 'C', 'S', 'T']);
end;

procedure TFHIRNativeOperationEngine.markSubsetted(meta: TFHIRMetaW);
begin
  meta.addTag('http://hl7.org/fhir/v3/ObservationValue', 'SUBSETTED', 'Subsetted');
end;

procedure TFHIRNativeOperationEngine.unmarkSubsetted(meta: TFHIRMetaW);
begin
  meta.removeTag('http://hl7.org/fhir/v3/ObservationValue', 'SUBSETTED');
end;

procedure TFHIRNativeOperationEngine.updateProvenance(prv: TFhirProvenanceW; inTransaction : boolean; rtype, id, vid: String);
begin
  if prv <> nil then
  begin
    if not inTransaction then
    begin
      prv.clearTargets;
      prv.clearSignatures;
    end;
    prv.addTarget(rtype+'/'+id+'/_history/'+vid);
  end;
end;

procedure TFHIRNativeOperationEngine.CreateIndexer;
begin
  if FIndexer = nil then
  begin
    FIndexer := ServerContext.ServerFactory.makeIndexer;
    FIndexer.Spaces := Repository.FSpaces.Link as TFhirIndexSpaces;
    FIndexer.Connection := FConnection.Link;
    FIndexer.Context := ServerContext.ValidatorContext.Link;
    FIndexer.Definitions := ServerContext.Indexes.Link;
    FIndexer.ResConfig := ServerContext.ResConfig.Link;
    FIndexer.Ucum := ServerContext.TerminologyServer.CommonTerminologies.Ucum.link;
    FIndexer.TerminologyServer := ServerContext.TerminologyServer.Link;
    FIndexer.Bases := ServerContext.Globals.Bases;
    FIndexer.KeyEvent := Repository.GetNextKey;
    FIndexer.OnResolveReference := resolveReferenceForIndexing;
    FIndexer.Engine := ServerContext.ServerFactory.makeEngine(ServerContext.ValidatorContext.Link, TUcumServiceImplementation.Create(FIndexer.Ucum.link));
    FIndexer.Engine.OnResolveReference := FIndexer.doResolve;
  end;
end;


procedure TFHIRNativeOperationEngine.StartTransaction;
begin
  Connection.StartTransact;
end;

procedure TFHIRNativeOperationEngine.ExecuteUpload(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  s : String;
  ok : boolean;
begin
  try
    ok := true;
    if not check(response, opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
      ok := false;

    if ok and not check(response, request.canWrite(request.ResourceName) and (request.Resource <> nil), 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang), itRequired) then
      ok := false;
    if ok and (request.Resource <> nil) then
      if not check(response, request.ResourceName = request.resource.fhirType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang), itInvalid) then
        ok := false;
    // todo: check version id integrity
    // todo: check version integrity

    if not ok then
      // do nothing
    else if request.resource.fhirType = 'Bundle' then
    begin
      ExecuteTransaction(context, request, response);
      exit;
    end
    else
    begin
      ExecuteCreate(context, request, response, idMaybeNew, 0);
      s := '1 new resource created @'+request.id;
    end;

    if ok then
    begin
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Body :=
    '<?xml version="1.0" encoding="UTF-8"?>'#13#10+
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
    '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
    ''#13#10+
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
    '<head>'#13#10+
    '    <title>'+StringFormat(GetFhirMessage('UPLOAD_DONE', lang), [request.resource.fhirType])+'</title>'#13#10+
    '    <link rel="Stylesheet" href="/css/fhir.css" type="text/css" media="screen" />'#13#10+
    FHIR_JS+
    '</head>'#13#10+
    ''#13#10+
    '<body>'#13#10+
    ''#13#10+
    '<div class="header">'#13#10+
    '  <a href="http://www.hl7.org/fhir" title="'+GetFhirMessage('MSG_HOME_PAGE_TITLE', lang)+'"><img src="/img/flame16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>'#13#10+
    ''#13#10+
    '  &copy; HL7.org 2011+'#13#10+
    '  &nbsp;'#13#10+
    '  '+ServerContext.Globals.OwnerName+' FHIR '+GetFhirMessage('NAME_IMPLEMENTATION', lang)+#13#10+
    '  &nbsp;'#13#10+
    '  '+GetFhirMessage('NAME_VERSION', lang)+' '+factory.versionString+#13#10;

    if request.session <> nil then
      response.Body := response.Body +'&nbsp;&nbsp;'+FormatTextToXml(request.Session.SessionName, xmlText);

    response.Body := response.Body +
    '  &nbsp;<a href="'+request.baseUrl+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a>'#13#10+
    '</div>'#13#10+
    ''#13#10+
    '<div id="div-cnt" class="content">'#13#10+
    '<h2>'+StringFormat(GetFhirMessage('UPLOAD_DONE', lang), [request.resource.fhirType])+'</h2>'#13#10+
    '<p>'+s+'</p>'#13#10+
    ''#13#10+
    '<p><br/><a href="'+request.baseUrl+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a></p>'+
    '</div>'#13#10+
    '</body>'#13#10+
    '</html>'#13#10+
    ''#13#10;
      response.ContentType:= 'text/html';
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

//Function TFHIRNativeOperationEngine.IsTypeAndId(s : String; var id : String):Boolean;
//var
//  l, r : String;
//begin
//  StringSplit(s, '/', l, r);
//  id := r;
//  if (l <> '') and (l[1] = '/') then
//    delete(l, 1, 1);
//  result := (StringArrayIndexOfSensitive(factory.resourceNames, l) > -1) and IsId(r);
//end;
//
function TFHIRNativeOperationEngine.ResolveSearchId(resourceName : String; requestCompartment: TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>; baseURL, params : String) : TMatchingResourceList;
var
  sp : TSearchProcessor;
  p : THTTPParameters;
  key : integer;
  item : TMatchingResource;
begin
  if resourceName = '' then
    key := 0
  else
  begin
    key := FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+resourceName+'''');
    assert(key > 0, 'Unable to find resource type '+resourceName);
  end;
  p := THTTPParameters.create(params);
  result := TMatchingResourceList.Create;
  try
    sp := TSearchProcessor.create(ServerContext);
    try
      sp.resConfig := ServerContext.ResConfig.Link;
      sp.typekey := key;
      sp.type_ := resourceName;
      sp.compartment := requestCompartment.Link;
      sp.sessionCompartments := sessionCompartments.link;
      sp.baseURL := baseURL;
      sp.lang := lang;
      sp.params := p;
      CreateIndexer;
      sp.indexes := ServerContext.Indexes.Link;
      sp.countAllowed := false;
      sp.Connection := FConnection.link;

      sp.build;

      FConnection.SQL := 'Select Ids.ResourceKey, Id, VersionId from Ids, Versions where ResourceTypeKey = '+inttostr(key)+' and MostRecent = ResourceVersionKey and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and '+sp.filter+')';
      FConnection.prepare;
      try
        FConnection.Execute;
        while FConnection.FetchNext do
        begin
          item := TMatchingResource.Create;
          try
            item.Name := FConnection.ColStringByName['Id'];
            item.key := FConnection.ColIntegerByName['ResourceKey'];
            item.version := FConnection.ColIntegerByName['VersionId'];
            result.Add(item.Link);
          finally
            item.free;
          end;
        end;
      finally
        FConnection.terminate;
      end;
    finally
      sp.free;
      p.Free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;


procedure TFHIRNativeOperationEngine.RollbackTransaction;
begin
  Connection.Rollback;
end;

//function TFHIRNativeOperationEngine.ResolveSearchIdCount(atype: String; compartmentId, compartments, baseURL, params: String): integer;
//var
//  list : TMatchingResourceList;
//begin
//  list := ResolveSearchId(atype, compartmentId, compartments, baseURL, params);
//  try
//    result := list.Count;
//  finally
//    list.free;
//  end;
//end;
//
procedure TFHIRNativeOperationEngine.SaveProvenance(session: TFhirSession; prv: TFhirProvenanceW);
begin
  prv.id := '';

  Repository.QueueResource(session, prv.Resource.link);
end;

function TFHIRNativeOperationEngine.scanId(request : TFHIRRequest; entry : TFHIRBundleEntryW; ids : TFHIRTransactionEntryList; index : integer) : TFHIRTransactionEntry;
var
  id : TFHIRTransactionEntry;
  k, versionKey : integer;
  sId, s, b : String;
  sParts : TArray<String>;
  baseok : boolean;
  aType : String;
  list : TMatchingResourceList;
begin
  if entry.resource <> nil then
    aType := entry.resource.fhirType
  else if (StringArrayExistsSensitive(['DELETE', 'GET'], entry.requestMethod)) or (entry.requesturl = '') then // must be deleting something
    raise EFHIRException.createLang('TRANSACTION_RESOURCE_MISSING', request.Lang, [inttostr(index+1)])
  else
  begin
    s := entry.requesturl;
    if s.contains('?') then
      s := s.subString(0, s.indexOf('?'));
    sParts := s.Split(['/']);
    aType := sParts[0];
    if not StringArrayExistsSensitive(factory.ResourceNames, atype) {and Not ServerContext.ValidatorContext.hasCustomResource(aType) }then
      raise EFHIRException.createLang('MSG_UNKNOWN_TYPE', Request.Lang, [sParts[0]]);
  end;

  if (entry.requestmethod = 'DELETE') and not ServerContext.ResConfig[aType].cmdDelete then
    raise EFHIRException.createLang('MSG_OP_NOT_ALLOWED', Request.Lang, ['Delete', entry.resource.fhirType]);


  if not ServerContext.ResConfig[aType].cmdUpdate and not ServerContext.ResConfig[aType].cmdCreate then
    raise EFHIRException.createLang('MSG_OP_NOT_ALLOWED', Request.Lang, ['Create', entry.resource.fhirType]);

  if (entry.resource <> nil) and  (entry.resource.id.Contains('[x]')) then
    raise EFHIRException.create('not handled - error in transaction (entry '+entry.resource.fhirType+'/'+entry.resource.id+' @ entry '+inttostr(index+1)+')');
    // this was a work around an for an error in the build tool - not needed any more
//    entry.resource.id := entry.resource.id.replace('[x]',entry.resource.fhirType);

  id := TFHIRTransactionEntry.create;
  try
    id.ResType := aType;

    if (entry.Url <> '') then
      id.Name := entry.Url
    else if (entry.resource <> nil) then
      id.Name := fullResourceUri(request.baseUrl, entry.resource.fhirType, entry.resource.id)
    else if entry.requestUrl <> '' then
      id.Name := fullResourceUri(request.baseUrl, entry.requestUrl)
    else
      id.Name := '??';
    id.outcomeVersion := 1;

    // figure out what kind of operation is involved here
    if entry.requestMethod <> '' then
    begin
      // does base matter?
      if entry.requestmethod = 'GET' then
      begin
        id.state := tesRead; // at this point, this is a null operation;
      end
      else if entry.requestmethod = 'POST' then
      begin
        id.state := tesCreate;
        if entry.requestIfNoneExist <> '' then
        begin
          s := entry.requestIfNoneExist;
          if (s.Contains('?')) then
            s := s.Substring(s.IndexOf('?')+1);
          list := ResolveSearchId(aType, request.compartment, request.SessionCompartments, request.baseUrl, s);
          try
            if list.Count = 1 then
            begin
              id.state := tesIgnore;
              id.id := list[0].Name;
              id.outcomeVersion := list[0].version+1;
            end
            else if list.Count > 1 then
              raise EFHIRException.create(GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang)+' (entry '+inttostr(index+1)+')');
          finally
            list.Free;
          end;
        end;

        if id.state = tesCreate then
        begin
          if not GetNewResourceId(entry.resource.fhirType, false {testing-todo},  sId, k) then
            raise EFHIRException.create(GetFhirMessage('MSG_RESOURCE_ID_FAIL', lang)+' (entry '+inttostr(index+1)+')');
          id.id := sId;
          id.key := k;
        end;
      end
      else if entry.requestmethod = 'PUT' then
      begin
        id.state := tesUpdate;
        if entry.requestUrl.Contains('?') then
        begin
          s := entry.requestUrl.substring(entry.requestUrl.IndexOf('?')+1);
          list := ResolveSearchId(aType, request.compartment, request.SessionCompartments, request.baseUrl, s);
          try
            id.count := list.count;
            if list.Count = 1 then // so we update this one
            begin
              id.id := list[0].Name;
              id.key := list[0].key;
              id.outcomeVersion := list[0].version+1;
            end
            else if list.Count = 0 then
            begin
              id.state := tesCreate;
              if not GetNewResourceId(entry.resource.fhirType, false {testing-todo},   sId, k) then
                raise EFHIRException.create(GetFhirMessage('MSG_RESOURCE_ID_FAIL', lang)+' (entry '+inttostr(index+1)+')');
              id.id := sId;
              id.key := k;
            end
            else if list.Count > 1 then
              raise EFHIRException.create(GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang)+' (entry '+inttostr(index+1)+')');
          finally
            list.Free;
          end;
        end
        else
        begin
          id.id := entry.resource.id;
          if (FindResource(aType, id.id, [froFindDeletedResource], id.key, versionKey, request, nil, nil)) then
            id.outcomeVersion := FConnection.CountSQL('select Max(VersionId) from Versions where ResourceKey = '+inttostr(id.key))+1;
        end;
        if ids.ExistsByTypeAndId(id) then
          raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [id.restype+'/'+id.id])+' (entry '+inttostr(index+1)+')');

        if (id.state = tesUpdate) and (id.key <> 0) then
        begin
          if entry.requestIfMatch <> '' then
          begin
            if 'W/"'+inttostr(id.outcomeVersion)+'"' <> entry.requestIfMatch then
              raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['W/"'+inttostr(id.outcomeVersion)+'"', entry.requestIfMatch])+' (entry '+inttostr(index+1)+')');
          end;
        end
        else if entry.requestIfMatch <> '' then
           raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['(n/a)', entry.requestIfMatch])+' (entry '+inttostr(index+1)+')');
      end
      else if entry.requestmethod = 'DELETE' then
      begin
        id.state := tesDelete;
        if entry.requestUrl.Contains('?') then
        begin
          s := entry.requestUrl.substring(entry.requestUrl.IndexOf('?')+1);
          list := ResolveSearchId(aType, request.compartment, request.SessionCompartments, request.baseUrl, s);
          try
            id.count := list.count;
            if list.Count = 1 then // so we delete this one
            begin
              id.id := list[0].Name;
              id.key := list[0].key;
              id.outcomeVersion := list[0].version;
            end
            else if list.Count = 0 then
            begin
              id.state := tesIgnore;
            end
            else if list.Count > 1 then
              raise EFHIRException.create(GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang)+' (entry '+inttostr(index+1)+')');
          finally
            list.Free;
          end;
        end
        else
        begin
          id.id := sParts[1];
          if (FindResource(aType, id.id, [froFindDeletedResource], id.key, versionKey, request, nil, nil)) then
            id.outcomeVersion := FConnection.CountSQL('select Max(VersionId) from Versions where ResourceKey = '+inttostr(id.key)) + 1;
        end;
        if (id.state = tesDelete) and ids.ExistsByTypeAndId(id) then
              raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [id.restype+'/'+id.id])+' (entry '+inttostr(index+1)+')');

        // version check
        if (id.state = tesDelete) and (id.key <> 0) then
        begin
          if entry.requestIfMatch <> '' then
          begin
            if 'W/"'+inttostr(id.outcomeVersion)+'"' <> entry.requestIfMatch then
             raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['W/"'+inttostr(id.outcomeVersion)+'"', entry.requestIfMatch])+' (entry '+inttostr(index+1)+')');
          end;
        end
        else if entry.requestIfMatch <> '' then
          raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['(n/a)', entry.requestIfMatch])+' (entry '+inttostr(index+1)+')')
        else if (id.state = tesDelete) then
          id.state := tesIgnore; // nothing to delete
      end
      else
        raise EFHIRException.create('illegal method type on entry?');
    end
    else
    begin
      id.state := tesCreate; // or it might be update - we'll figure out whether it's a create or an update based on the resource itself
      if (entry.resource = nil) then
        raise EFHIRException.createLang('TRANSACTION_RESOURCE_MISSING', request.Lang, [inttostr(index+1)]);
      if (entry.resource.id <> '') and not IsId(entry.resource.id) then
        entry.resource.id := ''; // just ignore it
//        raise EFHIRException.createLang('resource id is illegal ("'+entry.resource.id+'") (entry '+inttostr(index+1)+')');
      if (entry.Url = '') or (entry.Url.StartsWith(request.baseUrl)) then
        baseok := true
      else
      begin
        baseok := false;
        for b in ServerContext.Globals.Bases do
          if entry.Url.StartsWith(b) then
            baseOk := true;
      end;
      if (baseOk and (entry.resource.id <> '')) or (isGuid(entry.resource.id)) then
      begin
        id.id := entry.resource.id;
        id.state := tesUpdate;
        if ids.ExistsByTypeAndId(id) then
          if ids.dropDuplicates then
            id.state := tesIgnore
          else
            raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_TRANSACTION_DUPLICATE_ID', lang), [id.restype+'/'+id.id])+' (entry '+inttostr(index+1)+')');
      end;
      case id.state of
        tesIgnore: ; // yup, ignore it
        tesCreate:
          begin
            if not GetNewResourceId(entry.resource.FhirType,  false {testing-todo},  sId, k) then
              raise EFHIRException.create(GetFhirMessage('MSG_RESOURCE_ID_FAIL', lang)+' (entry '+inttostr(index+1)+')');
            id.id := sId;
            id.key := k;
            entry.resource.id := sId;
          end;
        tesUpdate:
          if (FindResource(aType, id.id, [froFindDeletedResource], id.key, versionKey, request, nil, nil)) then
            id.outcomeVersion := FConnection.CountSQL('select Max(VersionId) from Versions where ResourceKey = '+inttostr(id.key)) + 1;
      end;
    end;
    result := id;
    ids.add(id.link);
  finally
    id.free;
  end;
end;

function isLogicalReference(s : String) : boolean;
begin
  result := s.StartsWith('urn:oid:') or s.StartsWith('urn:uuid:');
end;


procedure TFHIRNativeOperationEngine.FixXhtmlUrls(const lang : THTTPLanguages; base : String; ids : TFHIRTransactionEntryList; node : TFhirXHtmlNode);
var
  i, j, k : integer;
  s, url, vHist : string;
begin
  if (node <> nil) and (node.NodeType = fhntElement) then
  begin
    if (node.Name = 'a') and (node.HasAttributes) and (node.Attributes.Get('href') <> '') then
    begin
      s := node.Attributes.Get('href');
      url := fullResourceUri(base, s);

      if (isHistoryURL(url)) then
        splitHistoryUrl(url, vhist)
      else
        vHist := '';

      j := ids.IndexByName(url);
      if (j = -1) and (s.endsWith('html')) then
        j := ids.IndexByHtml(s);
      k := 0;
      while (j = -1) and (k < ServerContext.Globals.Bases.Count - 1) do
      begin
        j := ids.IndexByName(ServerContext.Globals.Bases[k]+s);
        inc(k);
      end;

      if (j > -1) then
      begin
        if (vhist = '') then
          node.Attributes.SetValue('href', ids[j].resType+'/'+ids[j].id)
        else if (ids[j].version <> '') and (ids[j].version <> vHist) then
          raise EFHIRException.create(StringFormat(GetFhirMessage('Version ID Mismatch for '+url+' in narrative: reference to version '+vHist+', reference is '+ids[j].version, lang), [s]))
        else
          node.Attributes.SetValue('href', ids[j].resType+'/'+ids[j].id+'/_history/'+inttostr(ids[j].outcomeVersion));
      end
      else if isLogicalReference(s) then
        raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [s]));
    end;
    if (node.Name = 'img') and (node.Attributes.Get('src') <> '') then
    begin
      s := node.Attributes.Get('src');
      j := ids.IndexByName(s);
      if (j > -1) then
        node.Attributes.SetValue('src', base+ids[j].resType+'/'+ids[j].id)
      else if isLogicalReference(s) then
        raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [s]));
    end;
    for i := 0 to node.ChildNodes.count - 1 do
      FixXhtmlUrls(lang, base, ids, node.childNodes[i]);
  end;
end;


function TFHIRNativeOperationEngine.commitResource(request: TFHIRRequest; response : TFHIRResponse; mode : TOperationMode; entry: TFHIRBundleEntryW; i : integer; id: TFHIRTransactionEntry; session : TFHIRSession; resp : TFHIRBundleW) : Boolean;
var
  ne : TFHIRBundleEntryW;
  context : TOperationContext;
begin
  request.Id := id.id;
  if entry.resource <> nil then
    entry.resource.id := id.id;;

  request.originalId := id.originalId;
  request.SubId := '';
  request.ResourceName := ID.resType;
  request.resource := entry.resource.link;
  response.Location := '';
  response.versionId := '';

  //todo: check session
  context := TOperationContext.Create;
  try
    context.mode := mode;
    context.inTransaction := true;
    case id.state of
      tesIgnore: ;  // yup, ignore it
      tesRead: executeReadInTransaction(entry, request, response);
      tesCreate: ExecuteCreate(context, request, response, idIsNew, id.key);
      tesUpdate:
        begin
  //        if (entry.request <> nil) and (entry.requestUrl.contains('?')) then
  //        begin
  //          if Id.count <> ResolveSearchIdCount(id.resType, request.compartmentId, request.compartments, request.baseUrl, entry.requestUrl.substring(entry.requestUrl.IndexOf('?')+1)) then
  //            raise EFHIRException.createLang('error processing batch - id clash: one of the create statements altered the processing of a conditional update: '+entry.requestUrl);
  //        end;
          ExecuteUpdate(context, request, response);
        end;
      tesDelete:
        begin
  //        if (entry.request <> nil) and (entry.requestUrl.contains('?')) then
  //        begin
  //          if Id.count <> ResolveSearchIdCount(id.resType, request.compartmentId, request.compartments, request.baseUrl, entry.requestUrl.substring(entry.requestUrl.IndexOf('?')+1)) then
  //            raise EFHIRException.createLang('error processing batch - id clash: one of the create or update statements altered the processing of a conditional delete: '+entry.requestUrl);
  //        end;
          ExecuteDelete(request, Response);
        end;
    end;

    if response.HTTPCode < 300 then
      result := true
    else if response.Resource.fhirType = 'OperationOutcome' then
      result := false
    else
      result := check(response, response.HTTPCode < 300, response.HTTPCode, lang, response.Message, itNull);
    ne := id.entry;
    ne.resource := response.resource.Link;
    ne.responseStatus := inttostr(response.HTTPCode);
    ne.responseLocation := response.Location;
    ne.responseEtag := 'W/'+response.versionId;
    ne.responseDate := TFslDateTime.makeUTC(response.lastModifiedDate);
  finally
    context.Free;
  end;
end;

procedure ignoreEntry(req, resp : TFHIRBundleEntryW);
begin
  resp.responseStatus := '200';
  if (req.requestMethod = 'DELETE') then
    resp.responseStatus := '404';
end;


function TFHIRNativeOperationEngine.ExecuteTransaction(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
var
//  match : boolean;
  i : integer;
  resp : TFHIRBundleW;
  ids : TFHIRTransactionEntryList;
  ok : boolean;
  bundle : TFHIRBundleW;
  bl : TFslList<TFHIRBundleEntryW>;
  be, ne : TFHIRBundleEntryW;
  entry : TFHIRTransactionEntry;
  lpct : integer;
  procedure log(s : String);
  begin
    if context.Logging <> ollNone then
      Logging.log(s);
  end;
  procedure progress(pct : integer);
  begin
    if lpct <> pct then
    begin
      lpct := pct;
      if context.Logging = ollInstaller then
        Logging.log('%%%%: '+inttostr(pct));
    end;
  end;
begin
  result := 'Transaction';
  try
    ok := true;
    if not check(response, ServerContext.SupportTransaction, 405, lang, 'Transaction Operations not allowed', itNotSupported) then
      ok := false;
    if ok and not check(response, request.Resource.fhirType = 'Bundle', 400, lang, 'A bundle is required for a Transaction operation', itInvalid) then
      ok := false;

    if ok then
    begin
      bundle := factory.wrapBundle(request.Resource.link);
      if bundle.type_ = btBatch then
        executeBatch(context, request, response)
      else
      begin
        request.Source := nil; // ignore that now
        request.transactionResource := request.resource.link;
        resp := factory.wrapBundle(factory.makeResource('Bundle'));
        if request.Provenance <> nil then
        begin
          request.Provenance.clearTargets;
          request.Provenance.clearSignatures;
        end;
        ids := TFHIRTransactionEntryList.create;
        try
          resp.type_ := btTransactionResponse;
          ids.FDropDuplicates := bundle.Tags['duplicates'] = 'ignore';
  //        resp.base := baseUrl;
          ids.SortedByName;
          resp.id := FhirGUIDToString(CreateGuid);
          bl := bundle.entries;
          try
            log('Scanning IDs');
            // first pass: scan ids
            for i := 0 to bl.count - 1 do
            begin
              be := bl[i];
              progress(0+trunc(100 * (i / (bl.count * 10))));
              be.Tag := scanId(request, be, ids, i).Link;
            end;

            log('Updating References');
            // second pass: reassign references
            for i := 0 to bl.count - 1 do
            begin
              be := bl[i];
              entry := be.Tag as TFHIRTransactionEntry;
              progress(10+trunc(100 * (i / (bl.count * 10))));
              if not entry.ignore and not entry.deleted then
                adjustReferences(request, response, be.Tag as TFHIRTransactionEntry, request.baseUrl, be, ids);
            end;

            // thrid pass: commit resources
            bundle := bundle.Link;
            try
              log('Preparing');
              for i := 0 to bl.count - 1 do
              begin
                be := bl[i];
                progress(20+trunc(100 * (i / (bl.count * 10))));
                ne := resp.addEntry;
    //             ne.request := be.request.Link;
                (be.Tag as TFHIRTransactionEntry).entry := ne;
              end;

              for i := 0 to bl.count - 1 do
              begin
                be := bl[i];
                progress(30+trunc(100 * (i / (bl.count * 10))));
                if (be.Tag as TFHIRTransactionEntry).state = tesCreate then
                  ignoreEntry(be, (be.Tag as TFHIRTransactionEntry).entry);
              end;

              log('Processing');
              for i := 0 to bl.count - 1 do
              begin
                be := bl[i];
                progress(40+trunc(100 * (i / (bl.count * 5))));
                if (be.Tag as TFHIRTransactionEntry).state = tesCreate then
                begin
                  log(' c: '+ be.resource.fhirType+'/'+be.resource.id);
                  if not commitResource(request, response, context.mode, be, i, be.Tag as TFHIRTransactionEntry, request.Session, resp) then
                    Abort;
                end;
              end;
              for i := 0 to bl.count - 1 do
              begin
                be := bl[i];
                progress(60+trunc(100 * (i / (bl.count * 5))));
                if (be.Tag as TFHIRTransactionEntry).state = tesUpdate then
                begin
                  log(' u: '+ be.resource.fhirType+'/'+be.resource.id);
                  if not commitResource(request, response, context.mode, be, i, be.Tag as TFHIRTransactionEntry, request.Session, resp) then
                    Abort;
                end;
              end;
              for i := 0 to bl.count - 1 do
              begin
                be := bl[i];
                progress(80+trunc(100 * (i / (bl.count * 10))));
                if (be.Tag as TFHIRTransactionEntry).state = tesDelete then
                begin
                  log(' d: '+ be.resource.fhirType+'/'+be.resource.id);
                  if not commitResource(request, response, context.mode, be, i, be.Tag as TFHIRTransactionEntry, request.Session, resp) then
                    Abort;
                end;
              end;
              for i := 0 to bl.count - 1 do
              begin
                be := bl[i];
                progress(90+trunc(100 * (i / (bl.count * 10))));
                if (be.Tag as TFHIRTransactionEntry).state = tesRead then
                begin
                  log(' r: '+ be.resource.fhirType+'/'+be.resource.id);
                  if not commitResource(request, response, context.mode, be, i, be.Tag as TFHIRTransactionEntry, request.Session, resp) then
                    Abort;
                end;
              end;

            finally
              bundle.free;
            end;
          finally
            bl.Free;
          end;
          if (request.Provenance <> nil) then
            SaveProvenance(request.session, request.Provenance);
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.resource := resp.Resource.Link;
        finally
          ids.free;
          resp.free;
        end;
      end;
    end;
    if request.Resource <> nil then // batch
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      if request.Resource <> nil then // batch
        AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteBatch(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  req, resp : TFHIRBundleW;
  src, dest : TFHIRBundleEntryW;
  url : String;
  dummy : integer;
  mem : TFslMemoryStream;
  comp : TFHIRComposer;
  m : TVCLStream;
begin
  try
    req := factory.wrapBundle(request.resource.Link);
    resp := factory.wrapBundle(factory.makeResource('Bundle'));
    try
      resp.type_ := btBatchResponse;
      resp.id := NewGuidId;
      for src in req.entries.forEnum do
      begin
        dest := resp.addEntry;
        try
          if (resp.type_ = btBatch) and (src.requestMethod = '') then
            raise EFHIRException.create('No request details');
          if (src.requestMethod = '') then
          begin
            src.requestMethod := 'PUT';
            src.requestUrl := src.resource.fhirType+'/'+src.resource.id;
          end;
//          dest.requestMethod := src.requestMethod;
//          dest.requestUrl := src.requestUrl;
//          dest.requestIfNoneExist := src.requestIfNoneExist;
//          dest.requestIfMatch := src.requestIfMatch;
          request.reset;
          url := request.preAnalyse(src.requestUrl);
          request.analyse(src.requestMethod, url, dummy, nil);
          request.IfNoneMatch := src.requestifNoneMatch;
          if src.requestIfModifiedSince.notNull then
            request.IfModifiedSince := src.requestIfModifiedSince.UTC.DateTime;
          request.IfMatch := src.requestIfMatch;
          request.IfNoneExist := src.requestIfNoneExist;
          request.resource := src.resource.link;
          request.Source := TFslBuffer.Create;
          request.PostFormat := ffXml;
          if (context.mode in OP_MODES_CHECK) and ServerContext.validate and (request.resource <> nil) then
          begin
            comp := factory.makeComposer(ServerContext.ValidatorContext.Link, ffXml, THTTPLanguages.create('en'), OutputStylePretty);
            mem := TFslMemoryStream.Create;
            m := TVCLStream.Create;
            try
              m.Stream := mem.Link;
              mem.Buffer := request.source.Link;
              comp.compose(m, request.resource);
            finally
              m.Free;
              comp.Free;
              mem.Free;
            end;
          end;
          Execute(context, request, response);
          dest.responseStatus := inttostr(response.HTTPCode);
          dest.responseLocation := response.Location;
          dest.responseEtag := 'W/'+response.versionId;
          dest.responseDate := TFslDateTime.makeUTC(response.lastModifiedDate);
          dest.resource := response.resource.link;

        except
          on e : ERestfulException do
          begin
            dest.responseStatus := inttostr(e.Status);
            dest.resource := Factory.BuildOperationOutcome(request.Lang, e);
          end;
          on e : Exception do
          begin
            dest.responseStatus := '500';
            dest.resource := Factory.BuildOperationOutcome(request.Lang, e);
          end;
        end;
      end;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.resource := resp.Resource.Link;
    finally
      req.free;
      resp.free;
    end;
    AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, '', '', '', 0, fcmdBatch, request.Provenance, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, '', '', '', 0, fcmdBatch, request.Provenance, 500, '', e.message, []);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFHIRNativeOperationEngine.ExecuteOperation(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
begin
  if request.OperationName = 'graphql' then
  begin
    result := 'GraphQL';
    executeGraphQL(context, request, response)
  end
  else
  begin
    result := inherited ExecuteOperation(context, request, response);
    if (request.Parameters.has('_graphql') and (response.Resource <> nil) and (response.Resource.fhirType <> 'OperationOutcome')) then
    begin
      processGraphQL(request.Parameters['_graphql'], request, response);
      result := 'GraphQL';
    end;
  end;
end;

function typeForReference(ref : String) : String;
var
  list : TArray<String>;
begin
  list := ref.Split(['/']);
  result := list[0];
end;

function TFHIRNativeOperationEngine.opAllowed(resource : string; command: TFHIRCommandType): Boolean;
begin
  case command of
    fcmdUnknown : result := false;
    fcmdRead : result := ServerContext.ResConfig[resource].Supported;
    fcmdVersionRead : result := ServerContext.ResConfig[resource].Supported;
    fcmdUpdate : result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdUpdate;
    fcmdDelete : result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdDelete;
    fcmdHistoryInstance : result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdHistoryInstance;
    fcmdHistoryType : result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdHistoryType;
    fcmdHistorySystem : result := ServerContext.SupportSystemHistory;
    fcmdValidate : result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdValidate;
    fcmdSearch : result := ((resource = '') or ServerContext.ResConfig[resource].Supported) and ServerContext.ResConfig[resource].cmdSearch;
    fcmdCreate : result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdCreate;
    fcmdMetadata : result := true;
    fcmdUpload, fcmdTransaction : result := ServerContext.SupportTransaction;
    fcmdOperation : if resource = '' then
        result := true
      else
        result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdOperation;
  else
    result := false;
  end;
end;

//function TFHIRNativeOperationEngine.TypeForKey(key: integer): String;
//var
//  a : String;
//  b : boolean;
//begin
//  result := frtNull;
//  b := false;
//  for a := low(String) to high(String) do
//    if  ServerContext.ResConfig[a].key = key then
//    begin
//      result := a;
//      b := true;
//    end;
//  if not b then
//    raise EFHIRException.createLang('key '+inttostr(key)+' not found');
//end;

procedure TFHIRNativeOperationEngine.CommitTags(tags: TFHIRTagList; key: integer);
var
  i : integer;
begin
  if (tags.Count = 0) then
    exit;

  FConnection.sql := 'Insert into VersionTags (ResourceTagKey, ResourceVersionKey, TagKey, Display) values (:k, '+inttostr(key)+', :t, :l)';
  FConnection.prepare;
  try
    for i := 0 to tags.count - 1 do
    begin
      FConnection.BindInteger('k', ServerContext.TagManager.NextTagVersionKey);
      FConnection.BindInteger('t', tags[i].Key);
      FConnection.BindString('l', tags[i].Display);

      FConnection.Execute;
    end;
  finally
    FConnection.terminate;
  end;
end;

procedure TFHIRNativeOperationEngine.CommitTransaction;
begin
  Connection.Commit;
end;

procedure TFHIRNativeOperationEngine.ProcessBlob(request: TFHIRRequest; response: TFHIRResponse; field : String; fmt : TFHIRFormat);
var
  parser : TFHIRParser;
  mem : TBytesStream;
begin
  mem := TBytesStream.Create(FConnection.ColBlobByName[field]);
  try
    parser := factory.makeParser(ServerContext.ValidatorContext.link, fmt, lang);
    try
      parser.source := mem;
      parser.ParserPolicy := xppDrop;
      parser.Parse;
      response.Resource := parser.resource.Link as TFHIRResourceV;
    finally
      parser.free;
    end;
  finally
    mem.Free;
  end;
end;


function TFHIRNativeOperationEngine.processCanonicalSearch(request: TFHIRRequest; bundle: TFHIRBundleBuilder) : boolean;
begin
  result := false;
  if (request.ResourceName = 'CodeSystem') or (request.ResourceName = 'ValueSet') then
    result := ServerContext.TerminologyServer.findCanonicalResources(bundle, request.ResourceName, request.Parameters.Value['url'], request.Parameters.Value['version']);
end;

procedure TFHIRNativeOperationEngine.LoadTags(tags: TFHIRTagList; ResourceKey: integer);
var
  t, tf : TFhirTag;
  lbl : String;
begin
  FConnection.SQL := 'Select * from VersionTags where ResourceVersionKey = (select MostRecent from Ids where ResourceKey = '+inttostr(resourcekey)+')';
  FConnection.Prepare;
  FConnection.Execute;
  while FConnection.FetchNext do
  begin
    t := ServerContext.TagManager.GetTagByKey(FConnection.ColIntegerByName['TagKey']);
    if FConnection.ColStringByName['Display'] <> '' then
      lbl := FConnection.ColStringByName['Display']
    else
      lbl := t.Display;

    tf := tags.findTag(t.Category, t.system, t.code);
    if tf = nil then
      tags.AddTag(t.Key, t.Category, t.system, t.code, lbl)
    else
    begin
      if tf.display = '' then
        tf.display := lbl;
    end;
  end;
  FConnection.Terminate;
end;

function TFHIRNativeOperationEngine.LookupReference(context : TFHIRRequest; id: String): TResourceWithReference;
var
  a : String;
  resourceKey, versionKey: integer;
  parser : TFHIRParser;
  atype, b, base : String;
  s : TBytes;
begin
  base := context.baseUrl;
  if id.StartsWith(base) then
    id := id.Substring(base.Length)
  else for b in ServerContext.Globals.Bases do
    if id.StartsWith(b) then
      id := id.Substring(b.Length);
  aType := '';
  for a in ServerContext.ValidatorContext.allResourceNames do
    if id.startsWith(a + '/') then
    begin
      aType:= a;
      id := id.Substring(length(aType)+1);
    end;

  result := nil;
  if (aType <> '') and (length(id) <= ID_LENGTH) and FindResource(aType, id, [], resourceKey, versionKey, context, nil, nil) then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' and ResourceVersionKey = '+inttostr(versionKey);
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['JsonContent'];
        parser := factory.MakeParser(ServerContext.ValidatorContext.link, ffJson, lang);
        try
          result := TResourceWithReference.Create(id, parser.parseResource(s));
        finally
          parser.free;
        end;
      end
    finally
      FConnection.Terminate;
    end;
  end;
end;


function TFHIRNativeOperationEngine.LookupReferenceS(context: TFslObject; id: String): TResourceWithReference;
begin
  result := LookupReference(context as TFHIRRequest, id);
end;

//procedure TFHIRNativeOperationEngine.ExecuteGenerateQA(request: TFHIRRequest; response: TFHIRResponse);
//var
//  profile : TFHirStructureDefinition;
//  source : TFHIRResourceV;
//  resourceKey : integer;
//  id, fid : String;
//  builder : TQuestionnaireBuilder;
//  questionnaire : TFHIRQuestionnaire;
//begin
//  try
//    NotFound(request, response);
//    if check(response, opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
//    begin
//      if FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments) then
//      begin
//        source := GetResourceByKey(resourceKey);
//        try
//          // for now, we simply get the base profile
//          if source is TFHirStructureDefinition then
//            raise EFHIRException.createLang('Editing a profile via a profile questionnaire is not supported');
//
//          profile := GetProfileByURL('http://hl7.org/fhir/StructureDefinition/'+Codes_TFHIRResourceType[source.ResourceType], id);
//          try
//            fid := baseUrl+'StructureDefinition/'+id+'/$questionnaire';
//            questionnaire := Repository.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
//            try
//              builder := TQuestionnaireBuilder.Create;
//              try
//                builder.Profiles := Repository.Profiles.link;
//                builder.OnExpand := Repository.ExpandVS;
//                builder.OnLookupCode := Repository.LookupCode;
//                builder.onLookupReference := LookupReference;
//                builder.Context := request.Link;
//                builder.QuestionnaireId := fid;
//
//                builder.Profile := profile.link;
//                builder.Resource := source.Link as TFhirDomainResource;
//                if questionnaire <> nil then
//                  builder.PrebuiltQuestionnaire := questionnaire.Link;
//                builder.build;
//                if questionnaire = nil then
//                  Repository.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, builder.Questionnaire, builder.Dependencies);
//                response.HTTPCode := 200;
//                response.Message := 'OK';
//                response.Body := '';
//                response.LastModifiedDate := now;
//                response.links.Add('edit-post', baseUrl+request.ResourceName+'/'+request.id+'/$qa-post');
//                response.Resource := builder.Answers.Link;
//              finally
//                builder.Free;
//              end;
//            finally
//              questionnaire.free;
//            end;
//          finally
//            profile.free;
//          end;
//        finally
//          source.Free;
//        end;
//      end;
//    end;
//    AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
//  except
//    on e: exception do
//    begin
//      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
//      recordStack(e);
// raise;
//    end;
//  end;
//end;
//
//

function TFHIRNativeOperationEngine.FindResourceVersion(aType: String; sId, sVersionId: String; bAllowDeleted: boolean; var resourceVersionKey: integer; request: TFHIRRequest; response: TFHIRResponse): boolean;
begin
  FConnection.sql := 'select ResourceVersionKey from Ids, Types, Versions '+
    'where Ids.Id = :id and Ids.ResourceTypeKey = Types.ResourceTypeKey '+
    ' and Supported = 1 and Types.ResourceName = :n and Ids.ResourceKey = Versions.ResourceKey and Versions.Status < 2 and Versions.VersionId = :vid';
  FConnection.Prepare;
  FConnection.BindString('id', sId);
  FConnection.BindString('vid', sVersionId);
  FConnection.BindString('n', aType);
  FConnection.execute;
  result := FConnection.FetchNext;
  if result then
    resourceVersionKey := FConnection.ColIntegerByName['ResourceVersionKey']
  else
    check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.subid]), itNotFound);
  FConnection.terminate;
end;

//function TFHIRNativeOperationEngine.IdentifyValueset(request: TFHIRRequest; resource : TFHIRResourceV; params: THTTPParameters; base : String; var used, cacheId : string; allowNull : boolean = false) : TFHIRValueSet;
//begin
//  cacheId := '';
//  if (resource <> nil) and (resource is TFHIRValueSet) then
//    result := resource.Link as TFhirValueSet
//  else if params.has('_id') then
//  begin
//    result := GetValueSetById(request, params.getvar('_id'), base);
//    cacheId := result.url;
//    used := used+'&_id='+params.getvar('_id')
//  end
//  else if params.has('id') then
//  begin
//    result := GetValueSetById(request, params.getvar('id'), base);
//    cacheId := result.url;
//    used := used+'&id='+params.getvar('id')
//  end
//  else if params.has('identifier') then
//  begin
//    if not Repository.TerminologyServer.isKnownValueSet(params['identifier'], result) then
//      result := GetValueSetByIdentity(params['identifier'], params.getvar('version'));
//    cacheId := result.url;
//    used := used+'&identifier='+params['identifier']
//  end
//  else
//    result := constructValueSet(params, used, allowNull);
//  if params.has('nocache') then
//    cacheId := '';
//end;
//

//procedure TFHIRNativeOperationEngine.ExecuteQuestionnaireGeneration(request: TFHIRRequest; response : TFHIRResponse);
//var
//  profile : TFHirStructureDefinition;
//  op : TFHIROperationOutcomeW;
//  resourceKey : integer;
//  id, fid : String;
//  builder : TQuestionnaireBuilder;
//  questionnaire : TFHIRQuestionnaire;
//begin
//  try
//    NotFound(request, response);
//    if check(response, opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
//    begin
//      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and FindResource(frtStructureDefinition, request.Id, false, resourceKey, request, response, request.compartments)) then
//      begin
//        profile := nil;
//        try
//          // first, we have to identify the value set.
//          id := request.Id;
//          if request.Id <> '' then // and it must exist, because of the check above
//            profile := GetProfileById(request, request.Id, baseUrl)
//          else if request.Parameters.has('identifier') then
//            profile := GetProfileByURL(request.Parameters['identifier'], id)
//          else if (request.form <> nil) and request.form.hasParam('profile') then
//            profile := LoadFromFormParam(request.form.getparam('profile'), request.Lang) as TFHirStructureDefinition
//          else if (request.Resource <> nil) and (request.Resource is TFHirStructureDefinition) then
//            profile := request.Resource.Link as TFHirStructureDefinition
//          else
//            raise EFHIRException.createLang('Unable to find profile to convert (not provided by id, identifier, or directly)');
//
//          if id <> '' then
//          begin
//            fid := baseUrl+'StructureDefinition/'+id+'/$questionnaire';
//            questionnaire := Repository.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
//          end
//          else
//          begin
//            fid := newGUIDUrn;
//            questionnaire := nil;
//          end;
//
//          try
//            if questionnaire = nil then
//            begin
//              builder := TQuestionnaireBuilder.Create;
//              try
//                builder.Profile := profile.link;
//                builder.OnExpand := Repository.ExpandVS;
//                builder.onLookupCode := Repository.LookupCode;
//                builder.Context := request.Link;
//                builder.onLookupReference := LookupReference;
//                builder.QuestionnaireId := fid;
//                builder.build;
//                questionnaire := builder.questionnaire.Link;
//                if id <> '' then
//                  Repository.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.dependencies);
//              finally
//                builder.Free;
//              end;
//            end;
//            response.HTTPCode := 200;
//            response.Message := 'OK';
//            response.Body := '';
//            response.LastModifiedDate := now;
//            response.Resource := questionnaire.Link;
//          finally
//            questionnaire.Free;
//          end;
//        finally
//          profile.free;
//        end;
//        op := ServerContext.Validator.validateInstance(nil, response.Resource, 'Produce Questionnaire', nil);
//        try
//          if (op.hasErrors) then
//          begin
//            response.HTTPCode := 500;
//            response.Message := 'Questionnaire Generation Failed';
//            response.Resource.xmlId := 'src';
//            op.containedList.Add(response.Resource.Link);
//            response.Resource := op.link;
//          end;
//        finally
//          op.Free;
//        end;
//      end;
//    end;
//    inc(iCount);
//    TFHIRXhtmlComposer.Create(THTTPLanguages.create('en')).Compose(TFileStream.Create('c:\temp\q'+inttostr(iCount)+'.xml', fmCreate), response.Resource, true, nil);
//    AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
//  except
//    on e: exception do
//    begin
//      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
//      recordStack(e);
// raise;
//    end;
//  end;
//end;
//

function TFHIRNativeOperationEngine.GetResourceByUrl(aType : String; url, version: String; allowNil : boolean; var needSecure : boolean): TFHIRResourceV;
var
  s : TBytes;
  parser : TFHIRParser;
begin
  if version <> '' then
    FConnection.SQL := 'Select Secure, JsonContent from Versions where '+
      'ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and IndexKey in (Select IndexKey from Indexes where name = ''identifier'') and Value = :id) '+
      'and ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and IndexKey in (Select IndexKey from Indexes where name = ''version'') and Value = :ver) '+
      'and ResourceVersionKey in (select MostRecent from Ids) '+
      'order by ResourceVersionKey desc'
  else
    FConnection.SQL := 'Select Secure, JsonContent from Versions where '+
      'ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and IndexKey in (Select IndexKey from Indexes where name = ''url'') and Value = :id) '+
      'and ResourceVersionKey in (select MostRecent from Ids) '+
      'order by ResourceVersionKey desc';
  FConnection.Prepare;
  try
    FConnection.BindString('id', url);
    if version <> '' then
      FConnection.BindString('ver', version);
    FConnection.Execute;
    if not FConnection.FetchNext then
      if allowNil then
        exit(nil)
      else
        raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itUnknown, 'Unknown '+aType+' url: '+url, lang);
    needSecure := FConnection.ColIntegerByName['Secure'] = 1;
    s := FConnection.ColBlobByName['JsonContent'];
    parser := factory.MakeParser(ServerContext.ValidatorContext.link, ffJson, lang);
    try
      result := parser.parseResource(s);
      try
        if FConnection.FetchNext then
          raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'Found multiple matches for '+aType+' '+url+'. Pick one by the resource id', lang);
        result.link;
      finally
        result.free;
      end;
    finally
      parser.free;
    end;
  finally
    FConnection.Terminate;
  end;
end;

function TFHIRNativeOperationEngine.GetResourcesByParam(aType : String; name, value: String; var needSecure : boolean): TFslList<TFHIRResourceV>;
var
  s : TBytes;
  parser : TFHIRParser;
begin
  result := TFslList<TFHIRResourceV>.create;
  try
    FConnection.SQL := 'Select Secure, JsonContent from Versions where '+
      'ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and IndexKey in (Select IndexKey from Indexes where name = '''+name+''') and Value = :v) '+
      'and ResourceVersionKey in (select MostRecent from Ids) '+
        'order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.BindString('v', value);
      FConnection.Execute;
      while FConnection.FetchNext do
      begin
        s := FConnection.ColBlobByName['JsonContent'];
        needSecure := FConnection.ColIntegerByName['Secure'] = 1;
        parser := factory.MakeParser(ServerContext.ValidatorContext.link, ffJson, lang);
        try
          result.Add(parser.parseresource(s));
        finally
          parser.free;
        end;
      end;
    finally
      FConnection.Terminate;
    end;
    result.link;
  finally
    result.Free;
  end;
end;


function TFHIRNativeOperationEngine.GetServerContext: TFHIRServerContext;
begin
  result := TFHIRServerContext(FServerContext);
end;

function TFHIRNativeOperationEngine.GraphFollowReference(appInfo : TFslObject; context: TFHIRResourceV; reference: TFHIRObject; out targetContext, target: TFHIRResourceV): boolean;
var
  req : TFHIRRequest;
  secure : boolean;
begin
  req := TFHIRRequest(appInfo);
  target := getResourceByReference(context as TFHIRResourceV, readRef(reference), req, true, secure);
  result := (target <> nil) and (not secure or req.secure);
  if result then
    targetContext := target.Link
  else
    target.Free;
end;

procedure TFHIRNativeOperationEngine.GraphListResources(appInfo: TFslObject; requestType: String; params: TFslList<TGraphQLArgument>; list: TFslList<TFHIRResourceV>);
var
  sql : String;
  rk : integer;
  sp : TSearchProcessor;
  url : String;
  b : TStringBuilder;
  json : TFHIRParser;
  request : TFHIRRequest;
  p : TGraphQLArgument;
begin
  request := TFHIRRequest(appInfo);
  rk := Repository.ResourceTypeKeyForName(requestType);
  sql := 'Select Ids.ResourceKey, JsonContent from Ids, Versions where Deleted = 0 and Ids.MostRecent = Versions.ResourceVersionKey and Ids.ResourceTypeKey = '+inttostr(rk)+' ';
  if not params.Empty then
  begin
    b := TStringBuilder.create;
    try
      for p in params do
      begin
        b.append(p.Name.replace('_', '-'));
        b.append('=');
        b.append(EncodeMIME(p.Values[0].toString));
        b.Append('&');
      end;
      url := b.ToString;
    finally
      b.Free;
    end;

    sp := TSearchProcessor.create(ServerContext);
    try
      sp.resConfig := ServerContext.ResConfig.Link;
      sp.strict := true;
      sp.typekey := rk;
      sp.type_ := requestType;
      sp.compartment := request.compartment.Link;
      sp.sessionCompartments := request.SessionCompartments.Link;
      sp.baseURL := request.baseURL;
      sp.lang := lang;
      sp.params := THTTPParameters.create(url);
      CreateIndexer;
      sp.indexes := ServerContext.Indexes.Link;
      sp.session := request.session.link;
      sp.countAllowed := true;
      sp.Connection := FConnection.link;

      sp.build;

      sql := sql + ' and '+sp.filter;
    finally
      sp.Free;
    end;
  end;
  sql := sql + ' order by Ids.ResourceKey DESC';
  json := factory.makeParser(request.Context.link, ffJson, request.lang);
  try
    FConnection.Sql := sql;
    FConnection.Prepare;
    try
      FConnection.Execute;
      while FConnection.FetchNext do
      begin
        json.source := TBytesStream.Create(FConnection.ColBlobByName['JsonContent']);
        json.Parse;
        list.Add(json.resource.Link as TFHIRResourceV);
      end;
    finally
      FConnection.Terminate;
    end;
  finally
    json.Free;
  end;
end;

function TFHIRNativeOperationEngine.GraphLookup(appInfo: TFslObject; requestType, id: String; var res: TFHIRResourceV): boolean;
var
  req : TFHIRRequest;
  secure : boolean;
  base : String;
begin
  req := TFHIRRequest(appInfo);
  res := GetResourceById(req, requestType, id, base, secure);
  result := (res <> nil) and (not secure or req.secure);
  if (not result and (res <> nil)) then
  begin
    res.Free;
    res := nil;
  end;
end;

function TFHIRNativeOperationEngine.GraphSearch(appInfo: TFslObject; requestType: String; params: TFslList<TGraphQLArgument>) : TFHIRBundleW;
var
  request : TFHIRRequest;
  response : TFHIRResponse;
  b : TStringBuilder;
  p : TGraphQLArgument;
begin
  request := TFHIRRequest(appInfo);
  request.CommandType := fcmdSearch;
  request.loadObjects := true;
  request.ResourceName := requestType;
  request.strictSearch := true;
  response := TFHIRResponse.Create(request.Context.link);
  try
    b := TStringBuilder.create;
    try
      for p in params do
      begin
        b.append(p.Name[1]+ p.Name.substring(1).replace('_', '-'));
        b.append('=');
        b.append(EncodeMIME(p.Values[0].toString));
        b.Append('&');
      end;
      request.Parameters := THTTPParameters.create(b.toString);
      ExecuteSearch(request, response);
    finally
      b.free;
    end;
    if response.resource.fhirType = 'OperationOutcome' then
      raise EJsonException.Create(getOpException(response.resource));
    result := (FServerContext as TFHIRServerContext).ValidatorContext.factory.wrapBundle(response.resource.Link);
  finally
    response.Free;
  end;
end;

function TFHIRNativeOperationEngine.GetResourceById(request: TFHIRRequest; aType : String; id, base: String; var needSecure : boolean): TFHIRResourceV;
var
  resourceKey, versionKey : integer;
  parser : TFHIRParser;
  b : String;
  s : TBytes;
begin
  if id.StartsWith(base) and (base <> '') then
    id := id.Substring(base.Length)
  else for b in ServerContext.Globals.Bases do
    if id.StartsWith(b) then
      id := id.Substring(b.Length);
  if id.startsWith(aType+'/') then
    id := id.Substring(length(aType)+1);

  if (length(id) <= ID_LENGTH) and FindResource(aType, id, [], resourceKey, versionKey, request, nil, nil) then
  begin
    FConnection.SQL := 'Select Secure, JsonContent from Versions where ResourceKey = '+inttostr(resourceKey)+' and ResourceVersionKey = '+inttostr(versionKey);
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['JsonContent'];
        needSecure := FConnection.ColIntegerByName['Secure'] = 1;
        parser := factory.makeParser(ServerContext.ValidatorContext.link, ffJson, lang);
        try
          result := parser.parseResource(s);
        finally
          parser.free;
        end;
      end
      else
        raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'Unable to find '+aType+'/'+id, lang);
    finally
      FConnection.Terminate;
    end;
  end
  else
    raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'Unknown resource '+aType+'/'+id, lang);
end;


function TFHIRNativeOperationEngine.GetResourceByKey(key: integer; var needSecure : boolean): TFHIRResourceV;
var
  parser : TFHIRParser;
  s : TBytes;
begin
  FConnection.SQL := 'Select Secure, JsonContent from Versions where ResourceKey = '+inttostr(key)+' order by ResourceVersionKey desc';
  FConnection.Prepare;
  try
    FConnection.Execute;
    if FConnection.FetchNext then
    begin
      needSecure := FConnection.ColIntegerByName['Secure'] = 1;
      s := FConnection.ColBlobByName['JsonContent'];
      parser := factory.makeParser(ServerContext.ValidatorContext.link, ffJson, lang);
      try
        result := parser.parseresource(s);
      finally
        parser.free;
      end;
    end
    else
      raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'Unable to find resource '+inttostr(key), lang);
  finally
    FConnection.Terminate;
  end;
end;

function TFHIRNativeOperationEngine.getResourceByReference(source : TFHIRResourceV; url: string; req : TFHIRRequest; allowNil : boolean; var needSecure : boolean): TFHIRResourceV;
var
  parser : TFHIRParser;
  s : TBytes;
  i, key, versionKey : integer;
  id, ver : String;
  rtype : String;
  parts : TArray<String>;
  res : TFHIRResourceV;
begin
  result := nil;
  ver := '';
  if url.StartsWith('#') and (source.isDomainResource) then
  begin
    for res in factory.getContained(source).forEnum do
      if '#'+res.id = url then
        exit(res.link);
    raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'Cannot resolve local reference: '+url, lang);
  end;

  for i := 0 to ServerContext.Globals.Bases.Count - 1 do
    if url.StartsWith(ServerContext.Globals.Bases[i]) then
      url := url.Substring(ServerContext.Globals.Bases[i].Length);

  if (url.StartsWith('http://') or url.StartsWith('https://')) then
    raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'Cannot resolve external reference: '+url, lang);

  parts := url.Split(['/']);
  if length(parts) = 2 then
  begin
    if not StringArrayExistsSensitive(factory.ResourceNames, parts[0]) then
      raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'URL not understood: '+url, lang);
    rType := parts[0];
    id := parts[1];
  end else if (length(parts) = 4) and (parts[2] = '_history') then
  begin
    if not StringArrayExistsSensitive(factory.ResourceNames, parts[0]) then
      raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'URL not understood: '+url, lang);
    rType := parts[0];
    id := parts[1];
    ver := parts[3];
  end
  else
    raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'URL not understood: '+url, lang);

  if FindResource(rtype, id, [], key, versionKey, req, nil, nil) then
  begin
    if ver <> '' then
      FConnection.SQL := 'Select Secure, JsonContent from Versions where ResourceKey = '+inttostr(key)+' and VersionId = '''+sqlwrapString(ver)+''''
    else
      FConnection.SQL := 'Select Secure, JsonContent from Versions where ResourceKey = '+inttostr(key)+' and ResourceVersionKey = '+inttostr(versionKey);
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['JsonContent'];
        needSecure := FConnection.ColIntegerByName['Secure'] = 1;
        parser := factory.MakeParser(ServerContext.ValidatorContext.link, ffJson, lang);
        try
          result := parser.parseresource(s);
        finally
          parser.free;
        end;
      end
      else if not allowNil then
        raise ERestfulException.create('TFHIRNativeOperationEngine.GetResourceByUrl', 404, itNotFound, 'Unable to find resource '+inttostr(key), lang);
    finally
      FConnection.Terminate;
    end;
  end
  else
    result := nil;
end;




//function TFHIRNativeOperationEngine.constructValueSet(params: THTTPParameters; var used: String; allowNull : Boolean): TFhirValueset;
//var
//  empty : boolean;
//  function UseParam(name : String; var value : String) : boolean; overload;
//  begin
//    result := params.has(name);
//    value := params[name);
//    used := used + '&'+name+'='+EncodeMime(value);
//    empty := value <> '';
//  end;
//  function UseParam(name : String): String; overload;
//  begin
//    result := params[name);
//    used := used + '&'+name+'='+EncodeMime(result);
//    empty := result <> '';
//  end;
//var
//  s, l : String;
//  inc : TFhirValueSetComposeInclude;
//  filter : TFhirValueSetComposeIncludeFilter;
//begin
//  empty := true;
//
//  result := TFhirValueSet.create;
//  try
//    result.Name := useParam('name');
//    result.url := useParam('vs-identifier');
//    if result.url = '' then
//      result.url := NewGuidURN;
//    result.version := useParam('vs-version');
//    result.compose := TFhirValueSetCompose.create;
//    if useParam('import', s) then
//      result.compose.importList.Append.value := s;
//    if UseParam('system', s) then
//    begin
//      inc := result.compose.includeList.append;
//      if (s = 'snomed') then
//        inc.systemUri := 'http://snomed.info/sct'
//      else if (s = 'loinc') then
//        inc.systemUri := 'http://loinc.org'
//      else
//        inc.systemUri := s;
//      if UseParam('code', s) then
//      begin
//        while (s <> '') do
//        begin
//          StringSplit(s, ',', l, s);
//          inc.conceptList.Append.code := l;
//        end;
//      end;
//      s := useParam('property');
//      l := useParam('value');
//      if (s <> '') or (l <> '') then
//      begin
//        filter := inc.filterList.Append;
//        filter.property_ := s;
//        if filter.property_ = '' then
//          filter.property_ := 'concept';
//        filter.value := l;
//        filter.op := ReadOperator(UseParam('op'));
//      end;
//    end;
//    if not empty then
//      result.link
//    else if not allowNull then
//      raise EFHIRException.createLang('Not value set details found');
//  finally
//    result.free;
//  end;
//  if empty then
//    result := nil;
//end;
//
procedure TFHIRNativeOperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip: string; resourceName: String; id, ver: String; verkey : integer; op: TFHIRCommandType; provenance : TFhirProvenanceW; httpCode: Integer; name, message: String; patients : TArray<String>);
begin
  AuditRest(session, intreqid, extreqid, ip, resourceName, id, ver, verkey, op, provenance, '', httpCode, name, message, patients);
end;

procedure TFHIRNativeOperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip: string; resourceName: String; id, ver: String; verkey : integer; op: TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode: Integer; name, message: String; patients : TArray<String>);
begin
  doAuditRestPat(session, intreqid, extreqid, ip, resourceName, id, ver, verkey, op, provenance, opName, httpCode, name, message, patients);
end;

procedure TFHIRNativeOperationEngine.storeResources(list : TFslList<TFHIRQueuedResource>; origin : TFHIRRequestOrigin; mode : TOperationMode);
var
  i : integer;
  request: TFHIRRequest;
  response : TFHIRResponse;
  context : TOperationContext;
begin
  CreateIndexer;
  context := TOperationContext.create;
  try
    context.mode := mode;
    // cut us off from the external request
    request := TFHIRRequest.create(ServerContext.ValidatorContext.Link, origin, FIndexer.Definitions.Compartments.Link);
    response := TFHIRResponse.create(ServerContext.ValidatorContext.link);
    try
      for i := 0 to list.count - 1 do
      begin
        request.ResourceName := list[i].Resource.FhirType;
        request.Resource := list[i].Resource.link;
        request.CommandType := fcmdCreate;
        request.internalRequestId := TFHIRServerContext(FServerContext).Globals.nextRequestId;
        if (list[i].Resource.fhirType  = 'Bundle') and (list[i].Resource.Tags['process'] = 'true') then
        begin
          request.CommandType := fcmdTransaction;
        end
        else if (list[i].Resource.id <> '') then
        begin
          request.id := list[i].Resource.id;
          request.CommandType := fcmdUpdate;
        end;

        if list[i].Resource.Tag <> nil then
          request.lastModifiedDate := TFslDateTimeWrapper(list[i].Resource.Tag).Value.DateTime;
        request.Session := list[i].Session.Link;
        Execute(context, request, response);
      end;
    finally
      response.Free;
      request.free;
    end;
  finally
    context.Free;
  end;
end;


procedure TFHIRNativeOperationEngine.ReIndex;
var
  list : TStringList;
  i : integer;
  r : TFHIRResourceV;
  parser : TFHIRParser;
  k : integer;
  tags : TFHIRTagList;
begin
  Connection.ExecSQL('delete from SearchEntries');
  Connection.ExecSQL('delete from Searches');
  Connection.ExecSQL('delete from IndexEntries');

  k := Connection.CountSQL('select Max(IndexKey) from Indexes');
  for i := 0 to ServerContext.Indexes.Indexes.count - 1 do
  begin
    if Connection.CountSQL('select Count(IndexKey) from Indexes where Name = '''+ ServerContext.Indexes.indexes[i].Name+'''') = 0 then
    begin
      Connection.Sql := 'insert into Indexes (IndexKey, Name) values (:k, :d)';
      Connection.prepare;
      Connection.bindInteger('k', k);
      Connection.bindString('d', ServerContext.Indexes.indexes[i].Name);
      Connection.execute;
      inc(k);
      Connection.terminate;
    end;
  end;

  CreateIndexer;
  list := TStringList.create;
  try
    Connection.SQL := 'select * from Ids where ResourceTypeKey in (Select ResourceTypeKey from Types where ResourceName != ''Binary'')';
    Connection.prepare;
    Connection.execute;
    while Connection.fetchnext do
      list.addObject(Connection.ColStringByName['Id'], TObject(connection.ColIntegerByName['ResourceKey']));
    Connection.terminate;
    for i := 0 to list.count - 1 do
    begin
      FConnection.sql := 'select Tags, Content from Versions where Status != 2 and resourceVersionkey in (Select MostRecent from  Ids where ResourceKey = '+inttostr(Integer(list.objects[i]))+')';
      FConnection.prepare;
      FConnection.execute;
      if FConnection.FetchNext then
      begin
        tags := TFHIRTagList.create(factory.link);
        try
          parser := factory.MakeParser(ServerContext.ValidatorContext, ffJson, THTTPLanguages.create('en'));
          try
            r := parser.parseresource(Connection.ColBlobByName['JsonContent']);
            FConnection.terminate;
            Connection.StartTransact;
            try
              FIndexer.execute(Integer(list.objects[i]), list[i], r, tags, nil).free;
              Connection.Commit;
            except
              on e:exception do
              begin
                Connection.Rollback;
                recordStack(e);
                raise;
              end;
            end;
          finally
            parser.free;
          end;
        finally
          tags.free;
        end;
      end
      else
        FConnection.terminate;
    end;
  finally
    list.free;
  end;
end;


function TFHIRNativeOperationEngine.Repository: TFHIRNativeStorageService;
begin
  result := FStorage as TFHIRNativeStorageService;
end;

function TFHIRNativeOperationEngine.resolveConditionalURL(request : TFHIRRequest; resp : TFHIRResponse; url: String): String;
var
  s : String;
  parts : TArray<String>;
  list : TMatchingResourceList;
begin
  for s in ServerContext.Globals.Bases do
    if url.StartsWith(s) then
      url := url.Substring(s.Length);
  if url.StartsWith('/') then
    url := url.Substring(1);
  parts := url.Split(['?']);

  list := ResolveSearchId(parts[0], request.compartment, request.SessionCompartments, url, parts[1]);
  try
    if list.Count = 1 then
      result := parts[0]+'/'+list[0].Name
    else if list.Count > 1 then
      raise ERestfulException.create('TFHIRNativeOperationEngine.resolveConditionalURL', 412, itConflict, 'Multiple matches found for '+url, lang)
    else
      raise ERestfulException.create('TFHIRNativeOperationEngine.resolveConditionalURL', 404, itConflict, 'No matches found for '+url, lang);
  finally
    list.Free;
  end;
end;

function TFHIRNativeOperationEngine.resolveReferenceForIndexing(sender : TFhirIndexManager; appInfo : TFslObject; url: String): TFHIRResourceV;
var
  p : TArray<String>;
  ok : boolean;
  s : String;
  b : TBytes;
  resourceKey, versionKey : integer;
  parser : TFHIRParser;
  request : TFHIRRequest;
  bundle : TFHIRBundleW;
  be : TFhirBundleEntryW;
  conn : TFDBConnection;
begin
  request := appInfo as TFHIRRequest;
  if request.CommandType = fcmdTransaction then
  begin
    bundle := factory.wrapBundle(request.TransactionResource.link);
    for be in bundle.entries.forEnum do
    begin
      if be.resource <> nil then
      begin
        if be.url = url then
          exit(be.resource.link);
        if (be.resource.fhirType+'/'+be.resource.id = url) then
          exit(be.resource.link);
        if (url.startsWith(be.resource.fhirType+'/'+be.resource.id+'/_history')) then
          exit(be.resource.link);
      end;
    end;
  end;

  if (isAbsoluteUrl(url)) then
  begin
     ok := false;
     for s in ServerContext.Globals.Bases do
       if url.StartsWith(s) then
       begin
         ok := true;
         p := url.Substring(s.Length+1).Split(['/']);
         break;
       end;
    if not ok then
      exit(nil);
  end
  else
    p := url.Split(['/']);

  if (length(p) = 2) and (IsId(p[1])) and (factory.isResourceName(p[0])) then
    // that's ok
  else if (length(p) = 4) and (IsId(p[1])) and (factory.isResourceName(p[0])) and (p[2] = '_history') then
    // that's ok
  else
    exit(nil);

  result := nil;
  conn := FConnection;
  if conn.Prepared then
    conn := Repository.DB.GetConnection('Indexing Lookup');
  try
    if FindResourceConn(conn, p[0], p[1], [], resourceKey, versionKey, nil, nil, nil) then
    begin
      conn.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' and ResourceVersionKey = '+inttostr(versionKey);
      conn.Prepare;
      try
        conn.Execute;
        if conn.FetchNext then
        begin
          b := conn.ColBlobByName['JsonContent'];
          parser := factory.MakeParser(ServerContext.ValidatorContext.link, ffJson, lang);
          try
            result := parser.parseResource(b);
          finally
            parser.free;
          end;
        end
      finally
        conn.Terminate;
      end;
    end;
    if conn <> FConnection then
      conn.Release;
  except
    on e : Exception do
    begin
      if conn <> FConnection then
        conn.Error(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.clear(a: TArray<String>);
var
  i : String;
  k, l : string;
begin
  Connection.ExecSQL('delete from SearchEntries');
  Connection.ExecSQL('delete from Searches');
  Connection.ExecSQL('delete from IndexEntries');

  for i in factory.ResourceNames do
  begin
    if StringArrayExistsSensitive(a, i) then
    begin
      k := inttostr(Connection.CountSQL('select ResourceTypeKey from Types where ResourceName = '''+i+''''));
      // first thing we need to do is to make a list of all the resources that are going to be deleted
      // the list includes any resources of type k
      // any contained resources of type k
      l := '';
      connection.sql := 'select ResourceKey from Ids where ResourceTypeKey = '+k+' or ResourceKey in (select MasterResourceKey from Ids where ResourceTypeKey = '+k+')';
      connection.prepare;
      connection.Execute;
      while Connection.FetchNext do
        CommaAdd(l, connection.ColStringByName['ResourceKey']);
      connection.terminate;
      if (l <> '') then
      begin
        // now, any resources contained by either of those
        connection.sql := 'select ResourceKey from Ids where MasterResourceKey in ('+l+')';
        connection.prepare;
        connection.Execute;
        while Connection.FetchNext do
          CommaAdd(l, connection.ColStringByName['ResourceKey']);
        connection.terminate;

        Connection.ExecSQL('update ids set MostRecent = null where ResourceKey in ('+l+')');
        Connection.ExecSQL('delete from VersionTags where ResourceVersionKey in (select ResourceVersionKey from Versions where ResourceKey in ('+l+'))');
        Connection.ExecSQL('delete from Versions where ResourceKey in ('+l+')');
        Connection.ExecSQL('delete from Compartments where ResourceKey in ('+l+')');
        Connection.ExecSQL('delete from Compartments where CompartmentKey in ('+l+')');
        Connection.ExecSQL('delete from ids where ResourceKey in ('+l+')');
      end;
    end;
  end;
  Reindex;
end;

procedure TFHIRNativeOperationEngine.CheckCompartments(actual, allowed : TFslList<TFHIRCompartmentId>);
var
  a, b : TFHIRCompartmentId;
  ok : boolean;
begin
  if (allowed <> nil) and (allowed.Count > 0) then
  begin
    for a in actual do
    begin
      ok := false;
      for b in allowed do
        if (a.ResourceType = b.ResourceType) and (a.Id = b.Id) then
          ok := true;
      if not ok then
        raise EFHIRException.createLang('COMPARTMENT_ERROR', lang, [a.ToString]);
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.CheckCreateNarrative(request : TFHIRRequest);
var
  gen : TFHIRNarrativeGeneratorBase;
  x : TFhirXHtmlNode;
//  r : TFhirDomainResource;
//  profile : TFHirStructureDefinition;
begin
  x := factory.getXhtml(request.Resource);
  if x = nil then
  begin
    gen := factory.makeGenerator(ServerContext.ValidatorContext.link);
    try
      gen.generate(request.Resource);
    finally
      gen.free;
    end;
  end;
end;


procedure TFHIRNativeOperationEngine.chooseField(aFormat : TFHIRFormat; summary : TFHIRSummaryOption; loadObjects : boolean; out fieldName : String; out prsrFmt : TFhirFormat; out needsObject : boolean)
;
var
  s : String;
begin
  fieldName := '';
  prsrFmt := ffUnspecified;
  needsObject := loadObjects;

  if aFormat in [ffJson, ffNDJson] then
  begin
    s := 'Json';
    prsrFmt := ffJson;
  end
  else if aformat = ffXhtml then
  begin
    s := 'Json';
    prsrFmt := ffJson;
    needsObject := true;
  end
  else
  begin
    s := 'Xml';
    prsrFmt := ffXml;
  end;

  if summary = soSummary then
    fieldName := s+'Summary'
  else if summary = soFull then
    fieldName := s+'Content'
  else
  begin
    fieldName := s+'Content';
    needsObject := true;
  end;
end;

{ TReferenceList }

function TReferenceList.asSql: String;
var
  i, j : Integer;
  s : String;
  st : TStringList;
begin
  result := '';
  for i := 0 to Count - 1 Do
  begin
    if i > 0 then
      result := result + ' or ';
    s := '';
    st := TStringList(Objects[i]);
    for j := 0 to st.count - 1 do
      s := s + ', '''+st[j]+'''';
    result := result + '((Ids.ResourceTypeKey = (Select ResourceTypeKey from Types where ResourceName = '''+SQLWrapString(Strings[i])+''')) and (Ids.Id in ('+copy(s, 3, $FFFF)+')))';
  end;
end;

procedure TReferenceList.seeReference(id: String);
var
  i : Integer;
  t : TStringList;
  st, si : String;
begin
  StringSplit(id, '/', st, si);
  if (length(si) = 0) then
    exit;

  i := IndexOf(st);
  if i = -1 then
  begin
    t := TStringList.Create;
    t.Sorted := true;
    i := AddObject(st, t);
  end
  else
    t := TStringList(Objects[i]);

  if not t.find(si, i) then
    t.add(si);
end;

{ TFHIRTransactionEntryList }

function TFHIRTransactionEntryList.ExistsByTypeAndId(entry : TFHIRTransactionEntry):boolean;
var
  i : integer;
begin
  result := false;
  i := 0;
  while not result and (i < Count) do
  begin
    result := (entries[i].resType = entry.resType) and (entries[i].id = entry.id) and (entries[i] <> entry);
    inc(i);
  end;
end;

function TFHIRTransactionEntryList.GetByName(oName: String): TFHIRTransactionEntry;
begin
  result := TFHIRTransactionEntry(inherited GetByName(oName));
end;

function TFHIRTransactionEntryList.GetEntry(iIndex: Integer): TFHIRTransactionEntry;
begin
  result := TFHIRTransactionEntry(ObjectByIndex[iIndex]);
end;

function TFHIRTransactionEntryList.IndexByHtml(name: String): integer;
var
  i : integer;
begin
  result := -1;
  i := 0;
  while (result = -1) and (i < Count) do
  begin
    if (GetEntry(i).html = name) then
      result := i;
    inc(i);
  end;
end;

{ TFHIRTransactionEntry }

function TFHIRTransactionEntry.deleted: boolean;
begin
  result := state = tesDelete;
end;

function TFHIRTransactionEntry.ignore: boolean;
begin
  result := state = tesIgnore;
end;

function TFHIRTransactionEntry.summary: string;
begin
  result := restype+'/'+id;

end;



{ TKeyPair }

constructor TKeyPair.create(t_: String; key: string);
begin
  inherited Create;
  self.type_ := t_;
  self.key := key;
end;

{ TKeyList }

function TKeyList.forAll: String;
var
  key : TKeyPair;
begin
  result := '';
  for key in Self do
    if result = ''  then
      result := key.key
    else
      result := result +','+key.key;
end;

function TKeyList.forType(t_: String): String;
var
  key : TKeyPair;
begin
  result := '';
  for key in Self do
    if key.type_ = t_ then
      if result = ''  then
        result := key.key
      else
        result := result +','+key.key;
end;

{ TFHIRRepository }

constructor TFHIRNativeStorageService.Create(DB: TFDBManager; factory : TFHIRFactory);
begin
  inherited Create(factory);
  LoadMessages; // load while thread safe
//  FAppFolder := AppFolder;
  FDB := DB;
  FLock := TFslLock.Create('fhir-store');
  FQueue := TFslList<TFHIRQueuedResource>.Create;
  FSpaces := TFHIRIndexSpaces.create;
  FRegisteredValueSets := TFslStringDictionary.create;
End;

function TFHIRNativeStorageService.createAsyncTask(url, id: string; format : TFHIRFormat; secure : boolean): integer;
var
  key : integer;
  conn : TFDBConnection;
begin
  key := NextAsyncTaskKey;
  conn := DB.getConnection('async');
  try
    conn.SQL := 'Insert into AsyncTasks (TaskKey, id, SourceUrl, Format, Secure, Status, Created) values ('+inttostr(key)+', '''+SQLWrapString(id)+''', '''+SQLWrapString(url)+''', '+inttostr(ord(format))+', '+booleanToSQL(secure)+', '+inttostr(ord(atsCreated))+', '+DBGetDate(DB.Platform)+')';
    conn.Prepare;
    conn.Execute;
    conn.Terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
  result := key;
end;

procedure TFHIRNativeStorageService.Initialise();
var
  conn: TFDBConnection;
  rn : String;
  implGuides : TFslStringSet;
  cfg : TFHIRResourceConfig;
  pcm : TFHIRPackageManager;
  li : TPackageLoadingInformation;
  res : TFslStringSet;
begin
  ServerContext.SubscriptionManager := ServerContext.ServerFactory.makeSubscriptionManager(ServerContext);
  ServerContext.SubscriptionManager.dataBase := FDB.Link;
  ServerContext.SubscriptionManager.Base := 'http://localhost/';
  ServerContext.SubscriptionManager.OnExecuteOperation := DoExecuteOperation;
  ServerContext.SubscriptionManager.OnExecuteSearch := DoExecuteSearch;
  ServerContext.SubscriptionManager.OnGetSessionEvent := ServerContext.SessionManager.GetSessionByKey;

  Logging.log('  .. keys');

  implGuides := TFslStringSet.create;
  try
    conn := FDB.GetConnection('setup');
    try
      ServerContext.SessionManager.LastSessionKey := conn.CountSQL('Select max(SessionKey) from Sessions');
      FLastVersionKey := conn.CountSQL('Select Max(ResourceVersionKey) from Versions');
      ServerContext.TagManager.LastTagVersionKey := conn.CountSQL('Select Max(ResourceTagKey) from VersionTags');
      ServerContext.TagManager.LastTagKey := conn.CountSQL('Select Max(TagKey) from Tags');
      FLastSearchKey := conn.CountSQL('Select Max(SearchKey) from Searches');
      FLastResourceKey := conn.CountSQL('select Max(ResourceKey) from Ids');
      FLastEntryKey := conn.CountSQL('select max(EntryKey) from IndexEntries');
      FLastCompartmentKey := conn.CountSQL('select max(ResourceCompartmentKey) from Compartments');
      FLastAsyncTaskKey  := conn.CountSQL('select max(TaskKey) from AsyncTasks');
      FLastObservationKey := conn.CountSQL('select max(ObservationKey) from Observations');
      FLastObservationCodeKey := conn.CountSQL('select max(ObservationCodeKey) from ObservationCodes');
      FLastObservationQueueKey := conn.CountSQL('select max(ObservationQueueKey) from ObservationQueue');
      FLastAuthorizationKey := conn.CountSQL('select max(AuthorizationKey) from Authorizations');
      FLastConnectionKey := conn.CountSQL('select max(ConnectionKey) from Connections');
      FLastClientKey := conn.CountSQL('select max(ClientKey) from ClientRegistrations');
      conn.execSQL('Update Sessions set Closed = ' +DBGetDate(conn.Owner.Platform) + ' where Closed = null');

      Logging.log('  .. valuesets');
      Conn.SQL := 'Select ValueSetKey, URL from ValueSets';
      Conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
        if not FRegisteredValueSets.ContainsKey(Conn.ColStringByName['URL']) then
          FRegisteredValueSets.Add(Conn.ColStringByName['URL'], Conn.ColStringByName['ValueSetKey']);
      conn.terminate;

      Logging.log('  .. tags');
      conn.SQL := 'Select TagKey, Kind, Uri, Code, Display from Tags';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
      begin
        ServerContext.TagManager.add(conn.ColIntegerByName['TagKey'], TFHIRTagCategory(conn.ColIntegerByName['Kind']), conn.ColStringByName['Uri'], conn.ColStringByName['Code'], conn.ColStringByName['Display']).ConfirmedStored := true;
      end;
      conn.terminate;

      Logging.log('  .. spaces');

      LoadSpaces(conn);
      conn.SQL := 'Select * from Config';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
        if conn.ColIntegerByName['ConfigKey'] = 1 then
          ServerContext.SupportTransaction := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 2 then
          ServerContext.Globals.Bases.add(AppendForwardSlash(conn.ColStringByName['Value']))
        else if conn.ColIntegerByName['ConfigKey'] = 3 then
          ServerContext.SupportSystemHistory := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 4 then
          ServerContext.DoAudit := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 9 then
          ServerContext.Validate := false// conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 6 then
          ServerContext.DatabaseId := conn.ColStringByName['Value']
        else if conn.ColIntegerByName['ConfigKey'] = 7 then
          ServerContext.ResConfig[''].cmdSearch := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 8 then
        begin
          if not TFHIRVersions.matches(conn.ColStringByName['Value'], factory.versionString) then
            raise EFHIRException.create('Database FHIR Version mismatch. The database contains DSTU'+conn.ColStringByName['Value']+' resources, but this server is based on DSTU'+factory.versionString)
        end
        else if not (conn.ColIntegerByName['ConfigKey'] in [5,10,100]) then
          raise EFHIRException.create('Unknown Configuration Item '+conn.ColStringByName['ConfigKey']);

      conn.terminate;
      Logging.log('  .. resources');

      conn.SQL := 'Select * from Types';
      conn.Prepare;
      conn.Execute;
      While conn.FetchNext do
      begin
        rn := conn.ColStringByName['ResourceName'];
        if conn.ColStringByName['ImplementationGuide'] <> '' then
          implGuides.add(conn.ColStringByName['ImplementationGuide']);

        if ServerContext.ResConfig.ContainsKey(rn) then
          cfg := ServerContext.ResConfig[rn]
        else
        begin
          cfg := TFHIRResourceConfig.Create;
          cfg.name := rn;
          ServerContext.ResConfig.Add(cfg.name, cfg);
        end;
        cfg.key := conn.ColIntegerByName['ResourceTypeKey'];
        cfg.Supported := conn.ColStringByName['Supported'] = '1';
        cfg.IdGuids := conn.ColStringByName['IdGuids'] = '1';
        cfg.IdClient := conn.ColStringByName['IdClient'] = '1';
        cfg.IdServer := conn.ColStringByName['IdServer'] = '1';
        cfg.cmdUpdate := conn.ColStringByName['cmdUpdate'] = '1';
        cfg.cmdDelete := conn.ColStringByName['cmdDelete'] = '1';
        cfg.cmdValidate := conn.ColStringByName['cmdValidate'] = '1';
        cfg.cmdHistoryInstance := conn.ColStringByName['cmdHistoryInstance'] = '1';
        cfg.cmdHistoryType := conn.ColStringByName['cmdHistoryType'] = '1';
        cfg.cmdSearch := conn.ColStringByName['cmdSearch'] = '1';
        cfg.cmdCreate := conn.ColStringByName['cmdCreate'] = '1';
        cfg.cmdOperation := conn.ColStringByName['cmdOperation'] = '1';
        cfg.versionUpdates := conn.ColStringByName['versionUpdates'] = '1';
        cfg.LastResourceId := conn.ColIntegerByName['LastId'];
        cfg.storedResourceId := conn.ColIntegerByName['LastId'];
      end;
      conn.terminate;
      Logging.log('  .. rkeys');
      if conn.Owner.Platform = kdbMySQL then
        conn.SQL := 'select ResourceTypeKey, max(CASE WHEN RTRIM(Id) REGEXP ''^-?[0-9]+$'' THEN CAST(Id AS SIGNED) ELSE 0 END) as MaxId from Ids group by ResourceTypeKey'
      else if conn.Owner.Platform = kdbSQLite then
        conn.SQL := 'select ResourceTypeKey, max(CASE WHEN typeof(RTRIM(Id) + ''.0e0'') = ''integer'' THEN CAST(Id AS bigINT) ELSE 0 end) as MaxId from Ids group by ResourceTypeKey'
      else
        conn.SQL := 'select ResourceTypeKey, max(CASE WHEN TRY_CAST(Id AS integer) is not null THEN CAST(Id AS bigINT) ELSE 0 end) as MaxId from Ids group by ResourceTypeKey';
      conn.Prepare;
      conn.Execute;
      While conn.FetchNext do
      begin
        rn := getTypeForKey(conn.ColIntegerByName['ResourceTypeKey']);
        if StringIsInteger32(conn.ColStringByName['MaxId']) and (conn.ColIntegerByName['MaxId'] > ServerContext.ResConfig[rn].LastResourceId) then
          raise EFHIRException.create('Error in database - LastResourceId (' +
            inttostr(ServerContext.ResConfig[rn].LastResourceId) + ') < MaxId (' +
            inttostr(conn.ColIntegerByName['MaxId']) + ') found for ' +
            rn);
      end;
      conn.terminate;

      Logging.log('  .. reconcile');
      ServerContext.TagManager.crosslink;
      ServerContext.Indexes.ReconcileIndexes(conn);

      if ServerContext.TerminologyServer <> nil then
      begin
        // the order here is important: specification resources must be loaded prior to stored resources
        Logging.log('  .. Load Package '+ServerContext.Factory.corePackage+'#' + ServerContext.Factory.versionString);
        pcm := TFHIRPackageManager.Create(false);
        li := TPackageLoadingInformation.create(ServerContext.Factory.versionString);
        try
          li.OnLoadEvent := ServerContext.ValidatorContext.loadResourceJson;
          try
            res := TFslStringSet.Create(['StructureDefinition', 'SearchParameter', 'CompartmentDefinition']); // we only load a few things; everything else is left to the database
            try
              pcm.loadPackage(ServerContext.Factory.corePackage, ServerContext.Factory.versionString, res, li);
            finally
              res.free;
            end;
          finally
            li.Free;
          end;
        finally
          pcm.Free;
        end;
        if ServerContext.Globals.forLoad then
        begin
//          Logging.log('Load Custom Resources');
//          LoadCustomResources(implGuides);
          Logging.log('  .. Load Stored Resources');
          LoadExistingResources(conn);
          if (false) then
          begin
            Logging.log('  .. Check Definitions');
            checkDefinitions();
          end;
        end;
        Logging.log('  .. Process Loaded Resources');
        ProcessLoadedResources;

        Logging.log('  .. Load Subscription Queue');
        ServerContext.SubscriptionManager.LoadQueue(conn);
      end;
      conn.Release;
    except
      on e: Exception do
      begin
        conn.Error(e);
        recordStack(e);
        raise;
      end;
    end;
  finally
    implGuides.free;
  end;
end;

procedure TFHIRNativeStorageService.RecordFhirSession(session: TFhirSession);
var
  conn: TFDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL :=
      'insert into Sessions (SessionKey, UserKey, Created, Provider, Id, Name, Email, Expiry) values (:sk, :uk, :d, :p, :i, :n, :e, :ex)';
    conn.Prepare;
    conn.BindInteger('sk', session.key);
    conn.BindInteger('uk', StrToInt(session.User.id));
    conn.BindDateTimeEx('d', TFslDateTime.makeLocal);
    conn.BindInteger('p', integer(session.providerCode));
    conn.BindString('i', session.id);
    conn.BindString('n', session.SessionName);
//    conn.BindString('sn', session.SystemName);
    conn.BindString('e', session.email);
    conn.BindDateTimeEx('ex', TFslDateTime.makeLocal(session.expires));
    conn.Execute;
    conn.terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;

end;

procedure TFHIRNativeStorageService.recordOAuthChoice(id, scopes, jwt, patient: String);
var
  conn : TFDBConnection;
begin
  conn := DB.GetConnection('oauth2');
  try
    conn.SQL := 'Update OAuthLogins set Status = 3, DateChosen = '+DBGetDate(conn.Owner.Platform)+', Rights = :r, Patient = :p, Jwt = :jwt where Id = '''+SQLWrapString(id)+'''';
    conn.prepare;
    conn.BindBlobFromString('r', scopes);
    conn.BindBlobFromString('jwt', jwt);
    if patient = '' then
      conn.BindNull('p')
    else
    begin
      conn.BindString('p', patient);
    end;
    conn.Execute;
    conn.Terminate;
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

procedure TFHIRNativeStorageService.recordOAuthLogin(id, client_id, scope, redirect_uri, state, launch: String);
var
  conn : TFDBConnection;
begin
  conn := DB.GetConnection('oauth2');
  try
    conn.SQL := 'insert into OAuthLogins (Id, Client, Scope, Redirect, Status, DateAdded, Launch, ClientState) values (:id, :cl, :sc, :ru, 1, '+DBGetDate(DB.Platform)+', :l, :st)';
    conn.prepare;
    conn.BindString('id', id);
    conn.BindString('cl', client_id);
    conn.BindString('sc', scope);
    conn.BindString('ru', redirect_uri);
    conn.BindString('l', launch);
    conn.BindBlobFromString('st', state);
    conn.execute;
    conn.Terminate;
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

procedure TFHIRNativeStorageService.recordPackageLoaded(id, ver: String; count: integer; blob: TBytes);
var
  conn : TFDBConnection;
  k : integer;
begin
  conn := DB.getConnection('packages.list');
  try
    k := conn.CountSQL('Select max(LoadedPackageKey) from LoadedPackages')+1;
    conn.SQL := 'insert into LoadedPackages (LoadedPackageKey, Id, Version, DateLoaded, Resources, Content) values (:k, :i, :v, :d, :c, :b)';
    conn.Prepare;
    conn.BindKey('k', k);
    conn.BindString('i', id);
    conn.BindString('v', ver);
    conn.BindDateTimeEx('d', TFslDateTime.makeUTC);
    conn.BindInteger('c', count);
    conn.BindBlob('b', blob);
    conn.Execute;
    conn.Terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

destructor TFHIRNativeStorageService.Destroy;
var
  rc : TFHIRResourceConfig;
begin
  for rc in ServerContext.ResConfig.Values do
    DB.ExecSQL('Update Types set LastId = '+inttostr(rc.lastResourceId)+' where ResourceTypeKey = '+inttostr(rc.key), 'key-update');
  FQueue.free;
  FSpaces.Free;
  FRegisteredValueSets.Free;
  FLock.free;
  FDB.Free;
  inherited;
end;

procedure TFHIRNativeStorageService.DoExecuteOperation(request: TFHIRRequest; response: TFHIRResponse; bWantSession: Boolean);
var
  storage: TFHIRNativeOperationEngine;
  context : TOperationContext;
begin
  if bWantSession then
    request.session := ServerContext.SessionManager.CreateImplicitSession('n/a', ServerContext.Globals.OwnerName, 'subscripion manager', systemInternal, true, false);
  context := TOperationContext.create;
  try
    storage := engineFactory(THTTPLanguages.create('en'), 'fhir.operation');
    try
      storage.Connection.StartTransact;
      try
        storage.Execute(context, request, response);
        storage.Connection.Commit;
        storage.Connection.Release;
      except
        on e: Exception do
        begin
          storage.Connection.Rollback;
          storage.Connection.Error(e);
          recordStack(e);
          raise;
        end;
      end;
    finally
      storage.free;
    end;
  finally
    context.Free;
  end;
end;

function TFHIRNativeStorageService.DoExecuteSearch(typekey : Integer; compartment : TFHIRCompartmentId; sessionCompartments: TFslList<TFHIRCompartmentId>; params: THTTPParameters; conn: TFDBConnection): String;
var
  sp: TSearchProcessor;
begin
  sp := TSearchProcessor.Create(ServerContext);
  try
    sp.resConfig := ServerContext.ResConfig.Link;
    sp.typekey := typekey;
    sp.type_ := getTypeForKey(typekey);
    sp.compartment := compartment.Link;
    sp.sessionCompartments := sessionCompartments.link;
    sp.baseURL := ServerContext.FormalURLPlain; // todo: what?
    sp.lang := THTTPLanguages.create('en');
    sp.params := params;
    sp.indexes := ServerContext.Indexes.Link;
    sp.countAllowed := false;
    sp.Connection := conn.link;
    sp.build;
    result := sp.filter;
  finally
    sp.free;
  end;
end;

function TFHIRNativeStorageService.SupportsSubscriptions: boolean;
begin
  result := true;
end;

function TFHIRNativeStorageService.ExpandVS(vs: TFHIRValueSetW; ref: string; const lang : THTTPLanguages; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList) : TFHIRValueSetW;
var
  profile : TFHIRExpansionParams;
begin
  profile := TFHIRExpansionParams.Create;
  try
    profile.limitedExpansion := allowIncomplete;
    if (vs <> nil) then
      result := ServerContext.TerminologyServer.ExpandVS(vs, '', profile, '', dependencies, limit, count, offset, nil)
    else
    begin
      if ServerContext.TerminologyServer.isKnownValueSet(ref, vs) then
        result := ServerContext.TerminologyServer.ExpandVS(vs, ref, profile, '', dependencies, limit, count, offset, nil)
      else
      begin
        vs := ServerContext.TerminologyServer.getValueSetByUrl(ref);
        if vs = nil then
          vs := ServerContext.TerminologyServer.getValueSetByid(ref);
        if vs = nil then
          result := nil
        else
          result := ServerContext.TerminologyServer.ExpandVS(vs, ref, profile, '', dependencies, limit, count, offset, nil)
      end;
    end;
  finally
    profile.free;
  end;
end;

function TFHIRNativeStorageService.FetchAuthorization(uuid: String; var PatientId : String; var ConsentKey, SessionKey: Integer; var Expiry: TDateTime; var jwt: String): boolean;
var
  conn : TFDBConnection;
begin
  conn := DB.GetConnection('FetchAuthorization');
  try
    conn.SQL := 'select PatientId, ConsentKey, SessionKey, Expiry, JWT from Authorizations where Uuid = '''+SQLWrapString(uuid)+''' and status = 1';
    conn.Prepare;
    conn.Execute;
    result := conn.FetchNext;
    if result then
    begin
      PatientId := conn.ColStringByName['PatientId'];
      ConsentKey := conn.ColIntegerByName['ConsentKey'];
      SessionKey := conn.ColIntegerByName['SessionKey'];
      Expiry := TSToDateTime(conn.ColTimestampByName['Expiry']);
      JWT := TEncoding.UTF8.GetString(conn.ColBlobByName['JWT']);
    end;
    conn.Terminate;
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


procedure TFHIRNativeStorageService.fetchClients(list: TFslList<TRegisteredClientInformation>);
var
  client : TRegisteredClientInformation;
  conn : TFDBConnection;
begin
  conn := DB.getConnection('clients');
  try
    conn.SQL := 'select SoftwareId, SoftwareVersion, Uri, LogoUri, Name, Mode, Secret, PatientContext, JwksUri, Issuer, SoftwareStatement, PublicKey, Scopes, Redirects from ClientRegistrations';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
    begin
      client := TRegisteredClientInformation.Create;
      try
        client.name := conn.ColStringByName['Name'];
        client.jwt := ''; // conn.ColBlobByName['SoftwareStatement'];
        client.mode := TRegisteredClientMode(conn.ColIntegerByName['Mode']);
        client.secret := conn.ColStringByName['Secret'];
        client.redirects.Text := conn.ColBlobAsStringByName['Redirects'];
        client.publicKey := conn.ColBlobAsStringByName['PublicKey'];
        client.issuer := conn.ColStringByName['Issuer'];
        client.url := conn.ColStringByName['Uri'];
        client.logo := conn.ColStringByName['LogoUri'];
        client.softwareId := conn.ColStringByName['SoftwareId'];
        client.softwareVersion := conn.ColStringByName['SoftwareVersion'];
        client.patientContext := conn.ColIntegerByName['PatientContext'] = 1;
        list.Add(client.link);
      finally
        client.Free;
      end;
    end;
    conn.Terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeStorageService.fetchExpiredTasks(tasks: TFslList<TAsyncTaskInformation>);
var
  conn : TFDBConnection;
  task : TAsyncTaskInformation;
begin
  conn := DB.getConnection('async');
  try
    conn.SQL := 'select TaskKey, Format, Names from AsyncTasks where Status != '+inttostr(ord(atsDeleted))+' and Expires < '+DBGetDate(DB.Platform);
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
    begin
      task := TAsyncTaskInformation.Create;
      try
        task.key := conn.ColIntegerByName['TaskKey'];
        task.format := TFHIRFormat(conn.ColIntegerByName['Format']);
        task.names := conn.ColStringByName['Names'].Split([',']);
        tasks.Add(task.Link);
      finally
        task.Free;
      end;
    end;
    conn.Terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;


function TFHIRNativeStorageService.fetchLoadedPackage(id: String): TBytes;
var
  b : TBytes;
  conn : TFDBConnection;
begin
  SetLength(b, 0);
  conn := DB.getConnection('packages.list');
  try
    conn.SQL := 'select Id, Content from LoadedPackages';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
    begin
      if (conn.ColStringByName['Id'] = id) then
        b := conn.ColBlobByName['Content'];
    end;
    conn.Terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
  result := b;
end;

function TFHIRNativeStorageService.fetchOAuthDetails(id: String; var client_id, redirect, state, scope, launch: String): boolean;
var
  conn : TFDBConnection;
begin
  conn := DB.GetConnection('oauth2');
  try
    conn.SQL := 'select Client, Scope, Redirect, Launch, ClientState from OAuthLogins where id = :id';
    conn.Prepare;
    conn.BindString('id', id);
    conn.Execute;
    result := conn.FetchNext;
    if result then
    begin
      client_id := conn.ColStringByName['Client'];
      redirect := conn.ColStringByName['Redirect'];
      scope := conn.ColStringByName['Scope'];
      launch := conn.ColStringByName['Launch'];
      state := conn.ColBlobAsStringByName['ClientState'];
    end;
    conn.Terminate;
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

function TFHIRNativeStorageService.fetchOAuthDetails(key, status: integer; var client_id, name, redirect, state, scope, launch: String): boolean;
var
  conn : TFDBConnection;
begin
  conn := DB.GetConnection('oauth2');
  try
    conn.SQL := 'select Client, Name, Scope, Redirect, Launch, ClientState from OAuthLogins, Sessions where OAuthLogins.SessionKey = '+inttostr(key)+' and Status = '+inttostr(status)+' and OAuthLogins.SessionKey = Sessions.SessionKey';
    conn.Prepare;
    conn.Execute;
    result := conn.FetchNext;
    if result then
    begin
      client_id := conn.ColStringByName['Client'];
      name := conn.ColStringByName['Name'];
      redirect := conn.ColStringByName['Redirect'];
      scope := conn.ColStringByName['Scope'];
      launch := conn.ColStringByName['Launch'];
      state := conn.ColBlobAsStringByName['ClientState'];
    end;
    conn.Terminate;
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

function TFHIRNativeStorageService.FetchResource(key: integer): TFHIRResourceV;
var
  parser: TFHIRParser;
  mem: TBytes;
  conn: TFDBConnection;
begin
  conn := FDB.GetConnection('FetchResource');
  try
    conn.SQL :=
      'select Versions.ResourceVersionKey, Ids.Id, Secure, JsonContent from Ids, Types, Versions where '+
      'Versions.ResourceVersionKey = Ids.MostRecent and Ids.ResourceTypeKey = Types.ResourceTypeKey and Ids.ResourceKey = ' +inttostr(key);
    conn.Prepare;
    try
      conn.Execute;
      if not conn.FetchNext then
        raise EFHIRException.create('Unable to load resource '+inttostr(key));
      mem := conn.ColBlobByName['JsonContent'];
      parser := ServerContext.Factory.makeParser(ServerContext.ValidatorContext.link, ffJson, THTTPLanguages.create('en'));
      try
        result := parser.parseresource(mem);
      finally
        parser.free;
      end;
      conn.Release;
    finally
      conn.terminate;
    end;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeStorageService.FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts: TStringList);
var
  conn : TFDBConnection;
  j : integer;
  cmp : String;
begin
  cmp := buildCompartmentsSQL(ServerContext.ResConfig, nil, compList);

  counts.Clear;
  conn := DB.GetConnection('fhir-home-page');
  try
    conn.sql := 'select ResourceName, count(*) as Count from Ids,  Types where MasterResourceKey is null and Ids.ResourceTypeKey = Types.ResourceTypeKey '+cmp+' group by ResourceName';
    conn.prepare;
    conn.execute;
    while conn.fetchNext do
    begin
      j := counts.IndexOf(conn.ColStringByname['ResourceName']);
      if j = -1 then
        j := counts.add(conn.ColStringByname['ResourceName']);
      counts.objects[j] := TObject(conn.ColIntegerByName['Count']);
    end;
    conn.terminate;
    conn.Release;
  except
    on e:exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;

end;

function TFHIRNativeStorageService.fetchTaskDetails(id : String; var key : integer; var status: TAsyncTaskStatus; var fmt : TFHIRFormat; var secure : boolean; var message, sourceUrl: String; var transactionTime, expires: TFslDateTime; names : TStringList; var outcome: TBytes): boolean;
var
  conn : TFDBConnection;
begin
  conn := FDB.GetConnection('async');
  try
    conn.SQL := 'Select TaskKey, Status, Format, Secure, Message, SourceUrl, TransactionTime, Expires, Names, Outcome from AsyncTasks where Id = '''+SQLWrapString(id)+'''';
    conn.Prepare;
    conn.Execute;
    result := conn.FetchNext;
    if result then
    begin
      key := conn.ColIntegerByName['TaskKey'];
      status := TAsyncTaskStatus(conn.ColIntegerByName['status']);
      fmt := TFhirFormat(conn.ColIntegerByName['format']);
      message := conn.ColStringByName['Message'];
      secure := conn.ColIntegerByName['Secure'] <> 0;
      sourceUrl := conn.ColStringByName['SourceUrl'];
      transactionTime := conn.ColDateTimeExByName['TransactionTime'];
      expires := conn.ColDateTimeExByName['Expires'];
      names.CommaText := TEncoding.UTF8.GetString(conn.ColBlobByName['Names']);
      outcome := conn.ColBlobByName['Outcome']
    end;
    conn.Terminate;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeStorageService.CloseFhirSession(key: integer);
var
  conn: TFDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL := 'Update Sessions set closed = :d where SessionKey = ' +
      inttostr(key);
    conn.Prepare;
    conn.BindTimeStamp('d', DateTimeToTS(TFslDateTime.makeUTC.DateTime));
    conn.Execute;
    conn.terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;

end;

function TFHIRNativeStorageService.GetTotalResourceCount: integer;
begin
  result := FTotalResourceCount;
end;

function TFHIRNativeStorageService.getTypeForKey(key: integer): String;
var
  a: TFHIRResourceConfig;
begin
  FLock.Lock('getTypeForKey');
  try
    result := '';
    for a in ServerContext.ResConfig.Values do
      if a.key = key then
      begin
        result := a.Name;
        exit;
      end;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.hasOauthSession(id: String; status : integer): boolean;
var
  conn : TFDBConnection;
begin
  conn := DB.GetConnection('oauth2');
  try
    result := conn.CountSQL('select count(*) from OAuthLogins where Id = '''+SQLWrapString(id)+''' and Status = '+inttostr(status)) = 1;
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

function TFHIRNativeStorageService.hasOAuthSessionByKey(key, status: integer): boolean;
var
  conn : TFDBConnection;
begin
  conn := DB.GetConnection('oauth2');
  try
    result := conn.CountSQL('Select Count(*) from OAuthLogins where Status = '+inttostr(status)+' and SessionKey = '+inttostr(Key)) =1;
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

function TFHIRNativeStorageService.NextVersionKey: integer;
begin
  FLock.Lock('NextVersionKey');
  try
    inc(FLastVersionKey);
    result := FLastVersionKey;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRNativeStorageService.RegisterTag(tag: TFHIRTag; conn: TFDBConnection);
var
  C: TFHIRTag;
begin
  FLock.Lock('RegisterTag');
  try
    C := ServerContext.TagManager.findTag(tag.Category, tag.system, tag.code);
    if C <> nil then
    begin
      tag.key := C.key;
      if tag.Display = '' then
        tag.Display := C.Display;
      checkRegisterTag(tag, conn); // this is required because of a mis-match between the cached tags and the commit scope of doRegisterTag
    end
    else
    begin
      tag.key := ServerContext.TagManager.NextTagKey;
      doRegisterTag(tag, conn);
      ServerContext.TagManager.registerTag(tag);
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRNativeStorageService.doRegisterTag(tag: TFHIRTag; conn: TFDBConnection);
begin
  conn.SQL :=
    'insert into Tags (Tagkey, Kind, Uri, Code, Display) values (:k, :tk, :s, :c, :d)';
  conn.Prepare;
  conn.BindInteger('k', tag.key);
  conn.BindInteger('tk', ord(tag.Category));
  conn.BindString('s', tag.system);
  conn.BindString('c', tag.code);
  conn.BindString('d', tag.Display);
  conn.Execute;
  conn.terminate;
  tag.TransactionId := conn.transactionId;
end;

//function TFHIRNativeStorageService.Authorize(conn :  TFDBConnection; patientId : String; patientKey, consentKey, sessionKey : integer; JWT : String; expiry : TDateTime) : String;
//begin
//  conn.SQL := 'Insert into Authorizations (AuthorizationKey, PatientKey, PatientId, ConsentKey, SessionKey, Status, Expiry, Uuid, JWT) values (:k, :pk, :pid, :ck, :sk, 1, :e, :u, :j)';
//  conn.Prepare;
//  conn.BindInteger('k', NextAuthorizationKey);
//  conn.BindInteger('pk', patientKey);
//  conn.BindString('pid', patientId);
//  conn.BindInteger('ck', consentKey);
//  conn.BindInteger('sk', sessionKey);
//  conn.BindTimeStamp('e', DateTimeToTS(expiry));
//  result := NewGuidId;
//  conn.BindString('u', result);
//  conn.BindBlobFromString('j', jwt);
//  conn.Execute;
//  conn.Terminate;
//end;
//
function TFHIRNativeStorageService.cacheSize: UInt64;
begin
  result := inherited cacheSize + FRegisteredValueSets.sizeInBytes + FQueue.sizeInBytes;
end;

procedure TFHIRNativeStorageService.checkDropResource(session: TFhirSession; request: TFHIRRequest; resource: TFHIRResourceV; tags: TFHIRTagList);
begin
  // nothing at this time
end;

procedure TFHIRNativeStorageService.checkRegisterTag(tag: TFHIRTag; conn: TFDBConnection);
begin
  if tag.ConfirmedStored then
    exit;

  if conn.CountSQL('select Count(*) from Tags where TagKey = '+inttostr(tag.key)) = 0 then
    doRegisterTag(tag, conn)
  else if conn.transactionId <> tag.TransactionId then
    tag.ConfirmedStored := true;
end;

procedure TFHIRNativeStorageService.clearCache;
begin
  // nothing at this time.
  inherited;
end;

procedure TFHIRNativeStorageService.RegisterTag(tag: TFHIRTag);
var
  conn: TFDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    doRegisterTag(tag, conn);
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFHIRNativeStorageService.resolveConcept(conn: TFDBConnection; c: TFHIRCodingW): Integer;
begin
  if (c.systemUri <> '') and (c.code <> '') then
    result := resolveConcept(conn, c.systemUri, c.code)
  else
    result := 0;
end;

function TFHIRNativeStorageService.resolveConcept(conn: TFDBConnection; sys, code : String): Integer;
begin
  result := 0;
  conn.SQL := 'Select ConceptKey from Concepts where URL = '''+sqlWrapString(sys)+''' and Code = '''+sqlWrapString(code)+'''';
  conn.Prepare;
  conn.Execute;
  if conn.FetchNext then
    result := conn.ColIntegerByName['ConceptKey'];
  conn.Terminate;
  if (result = 0) then
  begin
    result := ServerContext.TerminologyServer.NextConceptKey;
    conn.execSQL('insert into Concepts (ConceptKey, URL, Code, NeedsIndexing) values ('+inttostr(result)+', '''+SQLWrapString(sys)+''', '''+SQLWrapString(code)+''', 1)');
  end;
end;

function TFHIRNativeStorageService.resolveReference(conn: TFDBConnection; ref: string): Integer;
var
  parts : TArray<String>;
begin
  result := 0;
  parts := ref.Split(['/']);
  if length(parts) = 2 then
  begin
    conn.SQL := 'Select ResourceKey from Ids, Types where Ids.Id = '''+sqlWrapString(parts[1])+''' and Types.ResourceName = '''+sqlWrapString(parts[0])+''' and Types.ResourceTypeKey = Ids.ResourceTypeKey';
    conn.Prepare;
    conn.Execute;
    if conn.FetchNext then
      result := conn.ColIntegerByName['ResourceKey'];
    conn.Terminate;
  end;
end;

function TFHIRNativeStorageService.ResourceTypeKeyForName(name: String): integer;
begin
  FLock.Lock('ResourceTypeKeyForName');
  try
    result := ServerContext.ResConfig[name].key;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.RetrieveSession(key: integer; var UserKey, Provider: integer; var Id, Name, Email: String): boolean;
var
  conn : TFDBConnection;
begin
  conn := DB.GetConnection('RetrieveSession');
  try
    conn.SQL := 'select UserKey, Provider, Id, Name, Email from Sessions where SessionKey = '+inttostr(key);
    conn.Prepare;
    conn.Execute;
    result := conn.FetchNext;
    if result then
    begin
      UserKey := conn.ColIntegerByName['UserKey'];
      Provider := conn.ColIntegerByName['Provider'];
      Id := conn.ColStringByName['Id'];
      Name := conn.ColStringByName['Name'];
      Email := conn.ColStringByName['Email'];
    end;
    conn.Terminate;
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


procedure TFHIRNativeStorageService.RunValidateResource(i : integer; rtype, id: String; bufJson, bufXml: TFslBuffer; b : TStringBuilder);
var
  ctxt : TFHIRValidatorContext;
  iss : TFHIROperationOutcomeIssueW;
begin
  try
    ctxt := TFHIRValidatorContext.Create;
    try
      ServerContext.Validator.validate(ctxt, bufXml, ffXml);
      ServerContext.Validator.validate(ctxt, bufJson, ffJson);
      if (ctxt.Issues.Count = 0) then
        Logging.log(inttostr(i)+': '+rtype+'/'+id+': passed validation')
      else
      begin
        Logging.log(inttostr(i)+': '+rtype+'/'+id+': failed validation');
        b.Append(inttostr(i)+': '+'http://local.healthintersections.com.au:960/open/'+rtype+'/'+id+' : failed validation'+#13#10);
        for iss in ctxt.Issues do
          if (iss.severity in [isFatal, isError]) then
            b.Append('  '+iss.display+#13#10);
      end;
    finally
      ctxt.Free;
    end;
  except
    on e:exception do
    begin
      recordStack(e);
      Logging.log(inttostr(i)+': '+rtype+'/'+id+': exception validating: '+e.message);
      b.Append(inttostr(i)+': '+'http://fhir2.healthintersections.com.au/open/'+rtype+'/'+id+' : exception validating: '+e.message+#13#10);
    end;
  end;
end;

procedure TFHIRNativeStorageService.RunValidation;
var
  conn : TFDBConnection;
  bufJ, bufX : TFslBuffer;
  b : TStringBuilder;
  i : integer;
begin
  b := TStringBuilder.Create;
  try
    conn := FDB.GetConnection('Run Validation');
    try
      conn.SQL := 'select ResourceTypeKey, Ids.Id, JsonContent, XmlContent from Ids, Versions where Ids.MostRecent = Versions.ResourceVersionKey';
      conn.Prepare;
      try
        conn.Execute;
        i := 0;
        while conn.FetchNext do
        begin
          bufJ := TFslBuffer.create;
          bufX := TFslBuffer.create;
          try
            bufJ.asBytes := conn.ColBlobByName['JsonContent'];
            bufX.asBytes := conn.ColBlobByName['XmlContent'];
            inc(i);
//            if (i = 57) then
            RunValidateResource(i, getTypeForKey(conn.ColIntegerByName['ResourceTypeKey']), conn.ColStringByName['Id'], bufJ, bufX, b);
          finally
            bufJ.free;
            bufX.free;
          end;
        end;
      finally
        conn.terminate;
      end;
      conn.release;
    except
      on e:exception do
      begin
        conn.error(e);
        raise;
      end;
    end;
    bufJ := TFslBuffer.Create;
    try
      bufJ.AsText := b.ToString;
      bufJ.SaveToFileName('c:\temp\validation.txt');
    finally
      bufJ.free;
    end;
  finally
    b.Free;
  end;
end;

function TFHIRNativeStorageService.SupportsHistory: boolean;
begin
  result := true;
end;

function TFHIRNativeStorageService.SupportsSearch: boolean;
begin
  result := true;
end;

function TFHIRNativeStorageService.SupportsTransactions: boolean;
begin
  result := true;
end;

procedure TFHIRNativeStorageService.Sweep;
var
  d: TDateTime;
  k : Integer;
  list: TFslList<TFHIRQueuedResource>;
  storage: TFHIRNativeOperationEngine;
  conn: TFDBConnection;
begin
  list := nil;
  d := TFslDateTime.makeUTC.DateTime;
  setThreadStatus('Sweeping Sessions');
  ServerContext.SessionManager.Sweep;
  FLock.Lock('sweep2');
  try
    if FQueue.Count > 0 then
    begin
      list := FQueue;
      FQueue := TFslList<TFHIRQueuedResource>.Create;
    end;
  finally
    FLock.Unlock;
  end;
  try
    setThreadStatus('Sweeping Search');
    if FNextSearchSweep < d then
    begin
      conn := FDB.GetConnection('Sweep.search');
      try
        k := conn.CountSQL('Select Max(SearchKey) from Searches');
        if (k > 200) then
        begin
          k := k - 200;
          conn.SQL := 'Delete from SearchEntries where SearchKey < '+inttostr(k);
          conn.Prepare;
          conn.Execute;
          conn.terminate;

          conn.SQL := 'Delete from Searches where SearchKey < '+inttostr(k);
          conn.Prepare;
          conn.Execute;
          conn.terminate;
        end;
        conn.Release;
      except
        on e: Exception do
        begin
          conn.Error(e);
          recordStack(e);
          raise;
        end;
      end;
      FNextSearchSweep := d + 10 * DATETIME_MINUTE_ONE;
    end;

    setThreadStatus('Sweeping - Closing');
    if list <> nil then
    begin
      setThreadStatus('Sweeping - audits');
      storage := engineFactory(THTTPLanguages.create('en'), 'fhir.sweep');
      try
        try
          storage.storeResources(list, roSweep, opmSweep);
          storage.Connection.Release;
        except
          on e: Exception do
          begin
            storage.Connection.Error(e);
            recordStack(e);
            raise;
          end;
        end;
      finally
        storage.free;
      end;
    end;
  finally
    list.free;
  end;
end;

procedure TFHIRNativeStorageService.UnStoreObservation(conn: TFDBConnection; key: integer);
begin
  inc(FLastObservationQueueKey);
  conn.ExecSQL('Insert into ObservationQueue (ObservationQueueKey, ResourceKey, Status) values ('+inttostr(FLastObservationQueueKey)+', '+inttostr(key)+', 0)');
end;

procedure TFHIRNativeStorageService.updateAsyncTaskStatus(key: integer; status: TAsyncTaskStatus; message: String);
var
  conn : TFDBConnection;
begin
  conn := DB.getConnection('async');
  try
    if status in [atsComplete, atsError] then
      conn.ExecSQL('Update AsyncTasks set Status = '+inttostr(ord(status))+', Message = '''+SQLWrapString(message)+''', Finished = '+DBGetDate(DB.Platform)+' where TaskKey = '+inttostr(key))
    else
      conn.ExecSQL('Update AsyncTasks set Status = '+inttostr(ord(status))+', Message = '''+SQLWrapString(message)+''' where TaskKey = '+inttostr(key));
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeStorageService.updateOAuthSession(id : String; state, key: integer; var client_id : String);
var
  conn : TFDBConnection;
begin
  conn := DB.GetConnection('oauth2');
  try
    conn.ExecSQL('Update OAuthLogins set Status = '+inttostr(state)+', SessionKey = '+inttostr(Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(id)+'''');

    conn.SQL := 'select Client from OAuthLogins where Id = '''+SQLWrapString(id)+'''';
    conn.Prepare;
    conn.Execute;
    if not conn.FetchNext then
      raise EFHIRException.create('Internal Error - unable to find record '+id);
    client_id := conn.ColStringByName['Client'];
    conn.Terminate;

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

function TFHIRNativeStorageService.TrackValueSet(id: String; conn : TFDBConnection; loading : boolean): integer;
var
  s : String;
begin
  FLock.Lock;
  try
    if not FRegisteredValueSets.TryGetValue(id, s) then
      s := '';
  finally
    FLock.Unlock;
  end;
  if s <> '' then
  begin
    result := StrToInt(s);
    if not loading then
      Conn.ExecSQL('Update ValueSets set NeedsIndexing = 1 where ValueSetKey = '+inttostr(result));
  end
  else
  begin
    result := FServerContext.TerminologyServer.NextValueSetKey;
    Conn.ExecSQL('Insert into ValueSets (ValueSetKey, URL, NeedsIndexing) values ('+inttostr(result)+', '''+SQLWrapString(id)+''', 1)');
  end
end;

procedure TFHIRNativeStorageService.setAsyncTaskDetails(key: integer; transactionTime: TFslDateTime; request: String);
var
  conn : TFDBConnection;
begin
  conn := DB.getConnection('async');
  try
    conn.sql := 'Update AsyncTasks set TransactionTime = :t, Request = :r where TaskKey = '+inttostr(key);
    conn.Prepare;
    conn.BindString('r', request);
    conn.BindDateTimeEx('t', transactionTime);
    conn.Execute;
    conn.Terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFHIRNativeStorageService.storeClient(client: TRegisteredClientInformation; sessionKey : integer): String;
var
  conn : TFDBConnection;
  key : integer;
begin
  conn := FDB.getconnection('clients');
  try
    key := NextClientKey;
    result := 'c.'+inttostr(key);
    conn.sql := 'Insert into ClientRegistrations '+
       '(ClientKey, DateRegistered, SessionRegistered, SoftwareId, SoftwareVersion, Uri, LogoUri, Name, Mode, Secret, '+
         'JwksUri, PatientContext, Issuer, SoftwareStatement, PublicKey, Scopes, Redirects) values '+
       '(:k, '+DBGetDate(FDB.Platform)+', :sk, :si, :sv, :u, :lu, :n, :m, :s, :j, :pc, :i, :ss, :pk, :sc, :r)';
    conn.Prepare;
    conn.BindInteger('k', key);
    if sessionKey <> 0 then
      conn.BindKey('sk', sessionKey)
    else
      conn.BindNull('sk');
    conn.BindStringOrNull('si', client.softwareId);
    conn.BindStringOrNull('sv', client.softwareVersion);
    conn.BindStringOrNull('u', client.url);
    conn.BindStringOrNull('lu', client.logo);
    conn.BindString('n', client.name);
    conn.BindInteger('m', ord(client.mode));
    conn.BindStringOrNull('s', client.secret);
    conn.BindNull('j');
    conn.BindStringOrNull('i', client.issuer);
    conn.BindNull('ss');
    conn.BindBlobFromString('pk', client.publicKey);
    conn.BindNull('sc');
    conn.BindBlobFromString('r', client.redirects.CommaText);
    conn.BindIntegerFromBoolean('pc', client.patientContext);
    conn.execute;
    conn.Terminate;
    conn.Release;
  except
    on e : exception do
    begin
      conn.Error(e);
      raise
    end;
  end;
end;

procedure TFHIRNativeStorageService.StoreObservation(conn: TFDBConnection; key: integer);
begin
  inc(FLastObservationQueueKey);
  conn.ExecSQL('Insert into ObservationQueue (ObservationQueueKey, ResourceKey, Status) values ('+inttostr(FLastObservationQueueKey)+', '+inttostr(key)+', 1)');
end;

procedure TFHIRNativeStorageService.storeObservationConcepts(conn: TFDBConnection; ok: integer; isComp : boolean; categories, concepts, compConcepts: TArray<Integer>);
  procedure iterate(arr : TArray<Integer>; src : integer);
  var
    i : integer;
  begin
    for i in arr do
      if i <> 0 then
      begin
        conn.BindInteger('k', nextObservationCodeKey);
        conn.BindInteger('ok', ok);
        conn.BindInteger('ck', i);
        conn.BindInteger('s', src);
        conn.Execute;
      end;
  end;
var
  s  : String;
  i : integer;
begin
  conn.sql := 'Insert into ObservationCodes (ObservationCodeKey, ObservationKey, ConceptKey, Source) values (:k, :ok, :ck, :s)';
  conn.Prepare;
  iterate(categories, 1);
  iterate(concepts, 2);
  iterate(compconcepts, 3);
  conn.Terminate;
  s := '';
  if isComp then
    for i in compConcepts do
      CommaAdd(s, inttostr(i))
  else
    for i in concepts do
      CommaAdd(s, inttostr(i));
  if (s <> '') then
    conn.execsql('Update Observations set CodeList = '''+s+''' where ObservationKey = '+inttostr(ok));
end;

procedure TFHIRNativeStorageService.DropResource(key, vkey, pvkey: integer; id: string; resource: String; indexer: TFhirIndexManager; conn: TFDBConnection);
begin
  FLock.Lock('DropResource');
  try
    if (resource = 'ValueSet') or (resource = 'ConceptMap') then
      ServerContext.TerminologyServer.DropTerminologyResource(resource, id)
    else if resource = 'StructureDefinition' then
      ServerContext.ValidatorContext.dropResource(resource, id);
    FServerContext.SubscriptionManager.DropResource(key, vkey, pvkey);
    FServerContext.QuestionnaireCache.clear(resource, id);
  finally
    FLock.Unlock;
  end;
  if (resource = 'Observation') then
    UnstoreObservation(conn, key);
end;

(*
procedure TFHIRNativeStorageService.SaveResource(res: TFHIRResourceV; dateTime: TFslDateTime; origin : TFHIRRequestOrigin);
var
  request: TFHIRRequest;
  response: TFHIRResponse;
begin
  request := TFHIRRequest.Create(ServerContext.ValidatorContext.Link, origin, ServerContext.Indexes.Compartments.Link);
  try
    request.ResourceName := res.fhirType;
    request.CommandType := fcmdCreate;
    request.resource := res.Link;
    request.lastModifiedDate := dateTime.UTC.DateTime;
    request.session := nil;
    response := TFHIRResponse.Create;
    try
      DoExecuteOperation(request, response, false);
    finally
      response.free;
    end;
  finally
    request.free;
  end;
end;
*)

procedure TFHIRNativeStorageService.ProcessEmails;
begin
  FServerContext.SubscriptionManager.ProcessEmails;
end;

procedure TFHIRNativeStorageService.ProcessLoadedResources;
begin
  ServerContext.ValidatorContext.LoadingFinished;
 // nothing
end;

procedure TFHIRNativeStorageService.ProcessObservationContent(conn: TFDBConnection; key, rk: integer; obs : TFHIRObservationW; subj : integer; categories : TArray<Integer>);
var
  cmp : TFhirObservationComponentW;
  cl : TFslList<TFHIRCodingW>;
  c : TFHIRCodingW;
  concepts, compConcepts : TArray<Integer>;
  dt, dtMin, dtMax : TDateTime;
  i : integer;
  value : TFHIRXVersionElementWrapper;
begin
  cl := obs.codings;
  try
    Setlength(concepts, cl.Count);
    Setlength(compConcepts, 0);
    i := 0;
    for c in cl do
    begin
      concepts[i] := resolveConcept(conn, c);
      inc(i);
    end;
  finally
    cl.free;
  end;

  dtMax := MAXSQLDATE;
  obs.getDates(dt, dtMin, dtMax);

  value := obs.valueW;
  if value = nil then
    value := obs.dataAbsentReason;
  try
    ProcessObservationValue(conn, rk, subj, false, categories, concepts, compConcepts, dt, dtMin, dtMax, value);
  finally
    value.free;
  end;
  for cmp in obs.components.forEnum do
  begin
    cl := cmp.codings;
    try
      Setlength(concepts, cl.Count);
      Setlength(compConcepts, 0);
      i := 0;
      for c in cl do
      begin
        concepts[i] := resolveConcept(conn, c);
        inc(i);
      end;
    finally
      cl.free;
    end;

    value := cmp.valueW;
    if value = nil then
      value := cmp.dataAbsentReason;
    try
      ProcessObservationValue(conn, rk, subj, true, categories, concepts, compConcepts, dt, dtMin, dtMax, value);
    finally
      value.free;
    end;
  end;
end;

procedure TFHIRNativeStorageService.ProcessObservation(conn: TFDBConnection; key: integer);
var
  rk : integer;
  deleted : boolean;
  obs : TFHIRObservationW;
  subj : integer;
  cl : TFslList<TFhirCodingW>;
  categories : TArray<Integer>;
  i : integer;
begin
  conn.sql := 'Select ResourceKey, Status from ObservationQueue where ObservationQueueKey = '+inttostr(key);
  conn.prepare;
  conn.Execute;
  conn.FetchNext;
  rk := conn.ColIntegerByName['ResourceKey'];
  deleted := conn.ColIntegerByName['Status'] = 0;
  conn.Terminate;
  conn.ExecSQL('Delete from ObservationCodes where ObservationKey in (select ObservationKey from Observations where ResourceKey = '+inttostr(rk)+')');
  conn.ExecSQL('Delete from Observations where ResourceKey = '+inttostr(rk));
  if not deleted then
  begin
    obs := Factory.wrapObservation(loadResource(conn, rk));
    try
      if (obs.subject <> '') and not isAbsoluteUrl(obs.subject) and
        (obs.hasTime) then
      begin
        subj := resolveReference(conn, obs.subject);
        if (subj <> 0) then
        begin
          cl := obs.categories;
          try
            SetLength(categories, cl.Count);
            for i := 0 to cl.Count - 1 do
              categories[i] := resolveConcept(conn, cl[i]);
          finally
            cl.Free;
          end;
          ProcessObservationContent(conn, key, rk, obs, subj, categories)
        end;
      end;
    finally
      obs.Free;
    end;
  end;
end;

procedure TFHIRNativeStorageService.ProcessObservations;
var
  conn : TFDBConnection;
  key : integer;
  cutoff : TDateTime;
begin
  cutoff := now + (DATETIME_MINUTE_ONE / 2);
  repeat
    conn := FDB.GetConnection('Observations');
    try
      key := conn.CountSQL('Select min(ObservationQueueKey) from ObservationQueue');
      if key > 0 then
      begin
        conn.startTransact;
        try
          processObservation(conn, key);
          conn.ExecSQL('Delete from ObservationQueue where ObservationQueueKey = '+inttostr(key));
          conn.commit;
        except
          conn.rollback;
          raise;
        end;
      end;
      conn.release;
    except
      on e : exception do
      begin
        conn.Error(e);
        raise;
      end;
    end;
  until (key = 0) or (now > cutoff);
end;

procedure TFHIRNativeStorageService.ProcessObservationValue(conn: TFDBConnection; key, subj : integer; isComp : boolean; categories, concepts, compConcepts : TArray<Integer>; dt, dtMin, dtMax: TDateTime; value: TFHIRXVersionElementWrapper);
begin
  if value is TFHIRQuantityW then
    ProcessObservationValueQty(conn, key, subj, isComp, categories, concepts, compConcepts, dt, dtMin, dtMax, value as TFhirQuantityW)
  else if value is TFhirCodeableConceptW then
    ProcessObservationValueCode(conn, key, subj, isComp, categories, concepts, compConcepts, dt, dtMin, dtMax, value as TFhirCodeableConceptW)
end;

procedure TFHIRNativeStorageService.ProcessObservationValueCode(conn: TFDBConnection; key, subj : integer; isComp : boolean; categories, concepts, compConcepts : TArray<Integer>; dt, dtMin, dtMax: TDateTime; value: TFHIRCodeableConceptW);
var
  c : TFHIRCodingW;
  ok, ck : Integer;
begin
  for c in value.codings.forEnum do
  begin
    ck := resolveConcept(conn, c);
    if (ck <> 0) then
    begin
      ok := nextObservationKey;
      conn.SQL := 'INSERT INTO Observations (ObservationKey, ResourceKey, SubjectKey, DateTime, DateTimeMin, DateTimeMax, ValueConcept, IsComponent) VALUES' +
                   '                         (:key, :rkey, :subj, :dt, :dtMin, :dtMax, :val, :ic)';
      conn.Prepare;
      conn.BindInteger('key', ok);
      conn.BindInteger('rkey', key);
      conn.BindInteger('subj', subj);
      if dt = 0 then
        conn.BindNull('dt')
      else
        conn.BindTimeStamp('dt', DateTimeToTS(dt));
      conn.BindTimeStamp('dtMin', DateTimeToTS(dtMin));
      conn.BindTimeStamp('dtMax', DateTimeToTS(dtMax));
      conn.BindIntegerFromBoolean('ic', isComp);
      conn.BindInteger('val', ck);
      conn.Execute;
      conn.Terminate;
      storeObservationConcepts(conn, ok, isComp, categories, concepts, compConcepts);
    end;
  end;
end;

procedure TFHIRNativeStorageService.ProcessObservationValueQty(conn: TFDBConnection; key, subj : integer; isComp : boolean; categories, concepts, compConcepts : TArray<Integer>; dt, dtMin, dtMax: TDateTime; value: TFHIRQuantityW);
var
  val, cval : TFslDecimal;
  upS, upC : TUcumPair;
  vU, cU, ok : Integer;
begin
  if (value.value <> '') and (value.code <> '') and (value.systemUri <> '')
    and (ServerContext.TerminologyServer.CommonTerminologies.Ucum <> nil) then
  begin
    val := TFslDecimal.ValueOf(value.value);
    vu := resolveConcept(conn, value.systemUri, value.code);
    if (value.systemUri = 'http://unitsofmeasure.org') then
    begin
      upS := TUcumPair.Create(val, value.code);
      try
        upC := ServerContext.TerminologyServer.CommonTerminologies.Ucum.getCanonicalForm(upS);
        try
          cval := upC.Value;
          cu := resolveConcept(conn, 'http://unitsofmeasure.org', upC.UnitCode);
        finally
          upC.Free;
        end;
      finally
        upS.Free;
      end;
    end
    else
      Cu := 0;
    ok := nextObservationKey;
    conn.SQL := 'INSERT INTO Observations (ObservationKey, ResourceKey, SubjectKey, DateTime, DateTimeMin, DateTimeMax, Value, ValueUnit, Canonical, CanonicalUnit, IsComponent) VALUES' +
                 '                         (:key, :rkey, :subj, :dt, :dtMin, :dtMax, :v, :vu, :c, :cu, :ic)';
    conn.Prepare;
    conn.BindInteger('key', ok);
    conn.BindInteger('rkey', key);
    conn.BindInteger('subj', subj);
    if dt = 0 then
      conn.BindNull('dt')
    else
      conn.BindTimeStamp('dt', DateTimeToTS(dt));
    conn.BindTimeStamp('dtMin', DateTimeToTS(dtMin));
    conn.BindTimeStamp('dtMax', DateTimeToTS(dtMax));
    conn.BindDouble('v', val.asDouble);
    conn.BindInteger('vu', vu);
    if (cu = 0) then
    begin
      conn.BindNull('c');
      conn.BindNull('cu');
    end
    else
    begin
      conn.BindDouble('c', cval.asDouble);
      conn.BindInteger('cu', cu);
    end;
    conn.BindIntegerFromBoolean('ic', isComp);
    conn.Execute;
    conn.Terminate;
    storeObservationConcepts(conn, ok, isComp, categories, concepts, compConcepts);

  end;
end;

procedure TFHIRNativeStorageService.ProcessSubscriptions;
begin
  FServerContext.SubscriptionManager.Process;
end;

function TFHIRNativeStorageService.ProfilesAsOptionList: String;
var
  i: integer;
  builder: TFslStringBuilder;
  Profiles: TFslStringMatch;
begin
  builder := TFslStringBuilder.Create;
  try
    Profiles := ServerContext.ValidatorContext.getProfileLinks(false);
    try
      for i := 0 to Profiles.Count - 1 do
      begin
        builder.append('<option value="');
        builder.append(Profiles.KeyByIndex[i]);
        builder.append('">');
        if Profiles.ValueByIndex[i] = '' then
        begin
          builder.append('@');
          builder.append(Profiles.KeyByIndex[i]);
          builder.append('</option>');
          builder.append(#13#10)
        end
        else
        begin
          builder.append(Profiles.ValueByIndex[i]);
          builder.append('</option>');
          builder.append(#13#10);
        end;
      end;
    finally
      Profiles.free;
    end;
    result := builder.AsString;
  finally
    builder.free;
  end;
end;

procedure TFHIRNativeStorageService.QueueResource(session : TFHIRSession; r: TFHIRResourceV; dateTime: TFslDateTime);
begin
  QueueResource(session, r);
end;

procedure TFHIRNativeStorageService.recordDownload(key: integer; name: String);
begin
  DB.ExecSQL('Update AsyncTasks set Downloads = Downloads + 1 where TaskKey = '+inttostr(key), 'async');
end;

procedure TFHIRNativeStorageService.QueueResource(session : TFHIRSession; r: TFHIRResourceV);
begin
  FLock.Lock;
  try
    FQueue.add(TFHIRQueuedResource.Create(session.Link, r.Link));
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.NextSearchKey: integer;
begin
  FLock.Lock('NextSearchKey');
  try
    inc(FLastSearchKey);
    result := FLastSearchKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.NextResourceKeyGetId(connection : TFDBConnection; aType: String; var id: string): integer;
var
  upd : boolean;
  key : integer;
  cfg : TFHIRResourceConfig;
begin
  key := 0;
  FLock.Lock('NextResourceKey');
  try
    inc(FLastResourceKey);
    result := FLastResourceKey;
    cfg := ServerContext.ResConfig[aType];
    inc(cfg.LastResourceId);
    upd := cfg.LastResourceId > cfg.storedResourceId;
    if upd then
    begin
      cfg.storedResourceId := cfg.LastResourceId + KEY_SAVE_SIZE;
      key := cfg.storedResourceId;
    end;
    id := inttostr(ServerContext.ResConfig[aType].LastResourceId);
  finally
    FLock.Unlock;
  end;
  if upd then
    connection.ExecSQL('Update Types set LastId = '+inttostr(key)+' where ResourceTypeKey = '+inttostr(cfg.key));
end;

function TFHIRNativeStorageService.NextResourceKeySetId(connection : TFDBConnection; aType: String; id: string): integer;
var
  i: integer;
begin
  FLock.Lock('NextResourceKey');
  try
    inc(FLastResourceKey);
    result := FLastResourceKey;
    if IsNumericString(id) and StringIsInteger32(id) then
    begin
      i := StrToInt(id);
      if (i > ServerContext.ResConfig[aType].LastResourceId) then
      begin
        ServerContext.ResConfig[aType].LastResourceId := i;
        if ServerContext.ResConfig[aType].LastResourceId > ServerContext.ResConfig[aType].storedResourceId then
        begin
          ServerContext.ResConfig[aType].storedResourceId := ServerContext.ResConfig[aType].LastResourceId + KEY_SAVE_SIZE;
          connection.ExecSQL('Update Types set LastId = '+inttostr(ServerContext.ResConfig[aType].storedResourceId)+' where ResourceTypeKey = '+inttostr(ServerContext.ResConfig[aType].key));
        end;
      end;
    end;
  finally
    FLock.Unlock;
  end;

end;

function TFHIRNativeStorageService.NextEntryKey: integer;
begin
  FLock.Lock('NextEntryKey');
  try
    inc(FLastEntryKey);
    result := FLastEntryKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.nextObservationKey: integer;
begin
  FLock.Lock('nextObservationKey');
  try
    inc(FLastObservationKey);
    result := FLastObservationKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.nextObservationCodeKey: integer;
begin
  FLock.Lock('nextObservationCodeKey');
  try
    inc(FLastObservationCodeKey);
    result := FLastObservationCodeKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.NextAsyncTaskKey: integer;
begin
  FLock.Lock('NextCompartmentKey');
  try
    inc(FLastAsyncTaskKey);
    result := FLastAsyncTaskKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.NextAuthorizationKey: integer;
begin
  FLock.Lock('NextAuthorizationKey');
  try
    inc(FLastAuthorizationKey);
    result := FLastAuthorizationKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.NextConnectionKey: integer;
begin
  FLock.Lock('NextConnectionKey');
  try
    inc(FLastConnectionKey);
    result := FLastConnectionKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.NextClientKey: integer;
begin
  FLock.Lock('NextConnectionKey');
  try
    inc(FLastClientKey);
    result := FLastClientKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.NextCompartmentKey: integer;
begin
  FLock.Lock('NextCompartmentKey');
  try
    inc(FLastCompartmentKey);
    result := FLastCompartmentKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.getClientInfo(id: String): TRegisteredClientInformation;
var
  client : TRegisteredClientInformation;
  conn : TFDBConnection;
begin
  client := TRegisteredClientInformation.Create;
  try
    conn := DB.getConnection('clients');
    try
      conn.SQL := 'select SoftwareId, SoftwareVersion, Uri, LogoUri, Name, Mode, Secret, PatientContext, JwksUri, Issuer, SoftwareStatement, PublicKey, Scopes, Redirects from ClientRegistrations where ClientKey = '+inttostr(strToIntDef(id.Substring(2), 0));
      conn.Prepare;
      conn.Execute;
      if not conn.FetchNext then
        raise EFHIRException.create('Unable to find registered client '+id);
      client.name := conn.ColStringByName['Name'];
      client.jwt := ''; // conn.ColBlobByName['SoftwareStatement'];
      client.mode := TRegisteredClientMode(conn.ColIntegerByName['Mode']);
      client.secret := conn.ColStringByName['Secret'];
      client.redirects.Text := conn.ColBlobAsStringByName['Redirects'];
      client.publicKey := conn.ColBlobAsStringByName['PublicKey'];
      client.issuer := conn.ColStringByName['Issuer'];
      client.url := conn.ColStringByName['Uri'];
      client.logo := conn.ColStringByName['LogoUri'];
      client.softwareId := conn.ColStringByName['SoftwareId'];
      client.softwareVersion := conn.ColStringByName['SoftwareVersion'];
      client.patientContext := conn.ColIntegerByName['PatientContext'] = 1;
      conn.Terminate;
      conn.Release;
    except
      on e: Exception do
      begin
        conn.Error(e);
        recordStack(e);
        raise;
      end;
    end;
    result := client.link;
  finally
    client.Free;
  end;
end;

function TFHIRNativeStorageService.getClientName(id: String): string;
var
  s : string;
  conn : TFDBConnection;
begin
  conn := DB.getConnection('clients');
  try
    conn.SQL := 'select Name from ClientRegistrations where ClientKey = '+inttostr(strToIntDef(id.Substring(2), 0));
    conn.Prepare;
    conn.Execute;
    if not conn.FetchNext then
      raise EFHIRException.create('Unable to find registered client '+id);
    s := conn.ColStringByName['Name'];
    conn.Terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
  result := s;
end;

function TFHIRNativeStorageService.GetNextKey(connection : TFDBConnection; keytype: TKeyType; aType: string; var id: string): integer;
begin
  case keytype of
    ktResource:
      result := NextResourceKeyGetId(connection, aType, id);
    ktEntries:
      result := NextEntryKey;
    ktCompartment:
      result := NextCompartmentKey;
  else
    raise EFHIRException.create('not done');
  end;
end;

function TFHIRNativeStorageService.Link: TFHIRNativeStorageService;
begin
  result := TFHIRNativeStorageService(Inherited Link);
end;


//procedure TFHIRNativeStorageService.loadCustomResources(guides: TFslStringSet);
////var
////  storage: TFHIRNativeOperationEngine;
////  s : String;
////  names : TStringList;
//begin
////  names := TStringList.create;
////  try
////    storage := engineFactory(THTTPLanguages.create('en'), 'fhir.loadCustom');
////    try
////      try
////        for s in guides do
////          if not storage.loadCustomResources(nil, s, true, names) then
////            raise EFHIRException.create('Error Loading Custom resources');
////        storage.Connection.Release;
////      except
////        on e : exception do
////        begin
////          storage.Connection.Error(e);
////          raise;
////        end;
////      end;
////    finally
////      storage.free;
////    end;
////  finally
////    names.Free;
////  end;
////  raise EFslException.Create('not currently supported');
//end;
//
procedure TFHIRNativeStorageService.LoadExistingResources(conn: TFDBConnection);
var
  parser: TFHIRParser;
  mem: TBytes;
  i: integer;
  cback: TFDBConnection;
  p : TFHIRResourceV;
begin
  conn.SQL :=
    'select Ids.ResourceKey, Versions.ResourceVersionKey, Ids.Id, Types.ResourceName, Secure, XmlContent from Ids, Types, Versions where '
    + 'Versions.ResourceVersionKey = Ids.MostRecent and ' +
    'Ids.ResourceTypeKey = Types.ResourceTypeKey and ' +
    '(Types.ResourceName in (''ValueSet'', ''EventDefinition'', ''Organization'', ''Device'' , ''CodeSystem'', ''ConceptMap'', ''StructureDefinition'', ''Questionnaire'', ''StructureMap'', ''Subscription'', ''SubscriptionTopic'')) and Versions.Status < 2';
  conn.Prepare;
  try
    cback := FDB.GetConnection('load2');
    try
      i := 0;
      conn.Execute;
      while conn.FetchNext do
      begin
        inc(i);
        mem := conn.ColBlobByName['XmlContent'];
        parser := factory.makeParser(ServerContext.ValidatorContext.link, ffXml, THTTPLanguages.create('en'));
        try
          try
            p := parser.parseResource(mem);
            try
              SeeResource(conn.ColIntegerByName['ResourceKey'],
                conn.ColIntegerByName['ResourceVersionKey'],
                0,
                conn.ColStringByName['Id'],
                conn.ColIntegerByName['Secure'] = 1,
                false, p, cback, true, nil, THTTPLanguages.create('en'), mem);
            finally
              p.Free;
            end;
          except
            on e : Exception do
            begin
              // log this, and keep trying
              Logging.log('Error loading '+conn.ColStringByName['ResourceKey']+' ('+conn.ColStringByName['ResourceName']+'/'+conn.ColStringByName['Id']+': '+e.Message);
            end;
          end;
        finally
          parser.free;
        end;
      end;
      cback.Release;
    except
      on e: Exception do
      begin
        cback.Error(e);
        recordStack(e);
        raise;
      end;
    end;
  finally
    conn.terminate;
  end;
  FTotalResourceCount := i;
end;

function TFHIRNativeStorageService.loadPackages: TFslMap<TLoadedPackageInformation>;
var
  map : TFslMap<TLoadedPackageInformation>;
  conn : TFDBConnection;
  o : TLoadedPackageInformation;
begin
  map := TFslMap<TLoadedPackageInformation>.create('loaded.packages');
  try
    conn := DB.getConnection('packages.list');
    try
      conn.SQL := 'select Id, Version, DateLoaded, Resources from LoadedPackages';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
      begin
        o := TLoadedPackageInformation.Create;
        try
          o.id := conn.ColStringByName['Id'];
          o.ver := conn.ColStringByName['Version'];
          o.date := conn.ColDateTimeExByName['DateLoaded'];
          o.count := conn.ColIntegerByName['Resources'];
          map.AddOrSetValue(o.id, o.link);
        finally
          o.Free;
        end;
      end;
      conn.Terminate;
      conn.Release;
    except
      on e: Exception do
      begin
        conn.Error(e);
        recordStack(e);
        raise;
      end;
    end;
    result := map.Link;
  finally
    map.Free;
  end;
end;

function TFHIRNativeStorageService.loadResource(conn: TFDBConnection; key: integer): TFHIRResourceV;
var
  parser: TFHIRParser;
  mem: TBytes;
begin
  conn.SQL :=
    'select Ids.ResourceKey, Versions.ResourceVersionKey, Ids.Id, Secure, JsonContent from Ids, Types, Versions where '
    + 'Versions.ResourceVersionKey = Ids.MostRecent and ' +
    'Ids.ResourceTypeKey = Types.ResourceTypeKey and Ids.ResourceKey = '+inttostr(key)+' and Versions.Status < 2';
  conn.Prepare;
  conn.Execute;
  if not conn.FetchNext then
    raise EFHIRException.create('unable to find resource '+inttostr(key));
  mem := conn.ColBlobByName['JsonContent'];
  parser := Factory.makeParser(ServerContext.ValidatorContext.link, ffJson, THTTPLanguages.create('en'));
  try
    result := parser.parseresource(mem);
  finally
    parser.Free;
  end;
  conn.terminate;
end;

procedure TFHIRNativeStorageService.LoadSpaces(conn: TFDBConnection);
begin
  conn.SQL := 'select * from Spaces';
  conn.prepare;
  conn.execute;
  while conn.FetchNext do
    FSpaces.RecordSpace(conn.ColStringByName['Space'], conn.ColIntegerByName['SpaceKey']);
  conn.terminate;
end;

function TFHIRNativeStorageService.LookupCode(system, version, code: String): String;
var
  prov: TCodeSystemProvider;
begin
  try
    prov := ServerContext.TerminologyServer.getProvider(system, version, nil);
    try
      if prov <> nil then
        result := prov.getDisplay(code, ServerContext.ValidatorContext.lang);
    finally
      prov.free;
    end;
  except
    result := '';
  end;
end;


procedure TFHIRNativeStorageService.MarkTaskDeleted(key: integer);
begin
  DB.ExecSQL('Update AsyncTasks set Status = '+inttostr(ord(atsDeleted))+' where TaskKey = '+inttostr(key), 'async');
end;

procedure TFHIRNativeStorageService.MarkTaskForDownload(key: integer; names : TStringList);
var
  exp : TFslDateTime;
  conn : TFDBConnection;
begin
  exp := TFslDateTime.makeLocal.add(1/24);
  conn := DB.getConnection('async');
  try
   conn.SQL := 'Update AsyncTasks set Expires = :t, Names = :n, Count = :c where TaskKey = '+inttostr(key);
   conn.Prepare;
   conn.BindDateTimeEx('t', exp);
   if names = nil then
   begin
     conn.BindNull('n');
     conn.BindInteger('c', 1);
   end
   else
   begin
     conn.BindBlobFromString('n', names.CommaText);
     conn.BindInteger('c', names.Count);
   end;
   conn.Execute;
   conn.Terminate;
   conn.Release;
 except
   on e: Exception do
   begin
     conn.Error(e);
     recordStack(e);
     raise;
   end;
 end;
end;

{ TFhirNativeOperation }

function TFhirNativeOperation.native(engine: TFHIROperationEngine): TFHIRNativeOperationEngine;
begin
  result := TFHIRNativeOperationEngine(engine);
end;
//
//function ReadOperator(lang, s : String) : TFhirFilterOperatorEnum;
//begin
//  s := LowerCase(s);
//  if s = '' then
//    result := FilterOperatorIsA
//  else if (s = '=') or (s = 'equals') then
//    result := FilterOperatorEqual
//  else if (s = 'nota') then
//    result := FilterOperatorIsNotA
//  else if (s = '<=') or (s = 'isa') then
//    result := FilterOperatorIsA
//  else if (s = 'regex') then
//    result := FilterOperatorRegex
//  else
//    raise EFHIRException.createLang('UNKNOWN_FILTER_OPERATOR', lang, [s]);
//end;
//

{ TFHIRQueuedResource }

constructor TFHIRQueuedResource.Create(session: TFHIRSession; resource: TFHIRResourceV);
begin
  inherited Create;
  FSession := session;
  FResource := resource;
end;

destructor TFHIRQueuedResource.Destroy;
begin
  FSession.Free;;
  FResource.Free;
  inherited;
end;

{ TPatientIdTracker }

constructor TPatientIdTracker.Create;
begin
  inherited;
  FIds := TStringList.Create;
  FIds.Sorted := true;
  FIds.Duplicates := Classes.dupIgnore;
end;

destructor TPatientIdTracker.Destroy;
begin
  FIds.Free;
  inherited;
end;

function TPatientIdTracker.ids: TArray<String>;
var
  i : integer;
begin
  SetLength(result, FIds.Count);
  for i := 0 to FIds.Count - 1 do
    result[i] := FIds[i];
end;

procedure TPatientIdTracker.seeIds(ids: TArray<String>);
var
  id : String;
begin
  for id in ids do
    FIds.Add(id);
end;

end.



