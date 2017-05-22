unit FHIRNativeStorage;

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

interface

uses
  SysUtils, Classes, IniFiles, Generics.Collections,
  kCritSct, DateSupport, kDate, DateAndTime, StringSupport, GuidSupport, OidSupport, DecimalSupport, BytesSupport, EncodeSupport,
  ParseMap, TextUtilities,
  AdvNames, AdvObjects, AdvObjectLists, AdvStringMatches, AdvExclusiveCriticalSections, AdvMemories, AdvVclStreams,
  AdvStringBuilders, AdvGenerics, AdvExceptions, AdvBuffers, AdvJson,
  KDBManager, KDBDialects, XmlSupport, MsXml, XmlPatch, MsXmlParser, GraphQL,
  FHIRResources, FHIRBase, FHIRTypes, FHIRParser, FHIRParserBase, FHIRConstants, FHIRContext, FHIROperations, FHIRXhtml,
  FHIRTags, FHIRValueSetExpander, FHIRValidator, FHIRIndexManagers, FHIRSupport, DifferenceEngine, FHIRMetaModel,
  FHIRUtilities, FHIRSubscriptionManager, FHIRSecurity, FHIRLang, FHIRProfileUtilities, FHIRPath, FHIRGraphQL,
  FHIRNarrativeGenerator, NarrativeGenerator, QuestionnaireBuilder,
  CDSHooksUtilities, {$IFNDEF FHIR2}FHIRStructureMapUtilities, ObservationStatsEvaluator, {$ENDIF} ClosureManager,
  ServerUtilities, ServerValidator, TerminologyServices, TerminologyServer, SCIMObjects, SCIMServer, DBInstaller, UcumServices, MPISearch,
  FHIRServerContext, FHIRStorageService, FHIRServerConstants;

const
  MAXSQLDATE = 365 * 3000;

const
  MAGIC_NUMBER = 941364592;

  CURRENT_FHIR_STORAGE_VERSION = 2;

  RELEASE_DATE = '20131103';

  OP_MASK_TAG = 'this-tag-used-for-the-mask-operation-outcome';

type
  TKeyPair = class (TAdvObject)
  private
    type_ : String;
    key : string;
  public
    constructor create(t_ : String; key : string);
  end;

  TKeyList = class (TAdvList<TKeyPair>)
  private
  public
    function forType(t_: String) : String;
    function forAll: String;
  end;

  TFHIRTransactionEntryState = (tesIgnore, tesRead, tesCreate, tesUpdate, tesDelete);

  TFHIRTransactionEntry = class (TAdvName)
  private
    state : TFHIRTransactionEntryState;
    id : String;
    originalId : String;
    key : integer;
    resType : String;
    version : String;
    outcomeVersion : integer;
    html : String;
    entry : TFhirBundleEntry;
    count : integer;
    function ignore : boolean;
    function deleted : boolean;
    function summary : string;
  end;

  TFHIRTransactionEntryList = class (TAdvNameList)
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

  TMatchingResource = class (TAdvName)
  private
    key : integer;
    version : integer;
  end;

  TMatchingResourceList = class (TAdvNameList)
  private
    function GetEntry(iIndex: Integer): TMatchingResource;
  public
    Property entries[iIndex : Integer] : TMatchingResource Read GetEntry; Default;
  end;

  TReferenceList = class (TStringList)
  public
    procedure seeReference(id : String);
    function asSql : String;
  end;


  TFHIRNativeOperationEngine = class;

  TFhirOperation = {abstract} class (TAdvObject)
  protected
    function CreateBaseDefinition(base : String) : TFHIROperationDefinition;
    function isWrite : boolean; virtual;
    function owningResource : TFhirResourceType; virtual; // for security purposes
    function makeParams(request : TFHIRRequest) : TFhirParameters;

    function buildExpansionProfile(request: TFHIRRequest; manager: TFHIRNativeOperationEngine; params : TFhirParameters) : TFHIRExpansionProfile;
  public
    function Name : String; virtual;
    function Types : TFhirResourceTypeSet; virtual;
    function HandlesRequest(request : TFHIRRequest) : boolean; virtual;
    function CreateDefinition(base : String) : TFHIROperationDefinition; virtual;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); virtual;
    function formalURL : String; virtual;
  end;

  TFHIRNativeStorageService = class;

  TFHIRNativeOperationEngine = class (TFHIROperationEngine)
  private
    FRepository : TFHIRNativeStorageService;
    FConnection : TKDBConnection;
    FFactory : TFHIRFactory;
    FIndexer : TFHIRIndexManager;
    FOperations : TAdvObjectList;
    FTestServer : Boolean;

    FSpaces: TFHIRIndexSpaces;
    FServerContext : TFHIRServerContext;

    procedure checkNotRedacted(meta : TFhirMeta; msg : String);
    procedure markRedacted(meta : TFhirMeta);
    procedure unmarkRedacted(meta : TFhirMeta);
    procedure AddCDSHooks(conf : TFhirCapabilityStatementRest);

    function checkOkToStore(request: TFHIRRequest; response: TFHIRResponse; var secure : boolean) : boolean;
    function isOkToDeleteSecurityLabel(request: TFHIRRequest; response: TFHIRResponse; c : TFHIRCoding) : boolean;
    function hasActCodeSecurityLabel(res : TFHIRResource; codes : array of string) : boolean;
    function hasConfidentialitySecurityLabel(res : TFHIRResource; codes : array of string) : boolean;

    procedure chooseField(aFormat : TFHIRFormat; summary : TFHIRSummaryOption; adaptor : TFHIRFormatAdaptor; out fieldName : String; out comp : TFHIRParserClass; out needsObject : boolean); overload;
    function opAllowed(resource : string; command : TFHIRCommandType) : Boolean;
    function FindSavedSearch(const sId : String; Session : TFHIRSession; typeKey : integer; var id, link, sql, title, base : String; var total : Integer; var summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean): boolean;
    function BuildSearchResultSet(typekey : integer; session : TFHIRSession; aType : String; params : TParseMap; baseURL, compartments, compartmentId : String; op : TFHIROperationOutcome; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean):String;
    function BuildHistoryResultSet(request: TFHIRRequest; response: TFHIRResponse; var searchKey, link, sql, title, base : String; var total : Integer) : boolean;
    procedure ProcessDefaultSearch(typekey : integer; session : TFHIRSession; aType : String; params : TParseMap; baseURL, compartments, compartmentId : String; id, key : string; op : TFHIROperationOutcome; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean);
    procedure ProcessMPISearch(typekey : integer; session : TFHIRSession; aType : String; params : TParseMap; baseURL, compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean);
    procedure BuildSearchForm(request: TFHIRRequest; response : TFHIRResponse);

    function GetResourceByKey(key : integer; var needSecure : boolean): TFHIRResource;
    function getResourceByReference(source : TFHIRResource; url, compartments : string; allowNil : boolean; var needSecure : boolean): TFHIRResource;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResource;
    function getResourceByUrl(aType : TFhirResourceType; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResource;
    function getResourcesByParam(aType : TFhirResourceType; name, value : string; var needSecure : boolean): TAdvList<TFHIRResource>;
    function loadResources(keys : TList<integer>) : TAdvList<TFHIRResource>;
    procedure updateProvenance(prv : TFHIRProvenance; rtype, id, vid : String);

    function FindResource(aType, sId : String; bAllowDeleted : boolean; var resourceKey : integer; request: TFHIRRequest; response: TFHIRResponse; compartments : String): boolean;
    function FindResourceVersion(aType : String; sId, sVersionId : String; bAllowDeleted : boolean; var resourceVersionKey : integer; request: TFHIRRequest; response: TFHIRResponse): boolean;
    procedure NoMatch(request: TFHIRRequest; response: TFHIRResponse);
    procedure NotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure VersionNotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure TypeNotFound(request: TFHIRRequest; response : TFHIRResponse);
    function GetNewResourceId(aType : String; ForTesting : boolean; var id : string; var key : integer):Boolean;
    function AddNewResourceId(aType, id, lang : string; forTesting : boolean; var resourceKey : integer) : Boolean;
    Procedure CollectIncludes(session : TFhirSession; includes : TReferenceList; resource : TFHIRResource; path : String);
    Procedure LoadTags(tags : TFHIRTagList; ResourceKey : integer);
    procedure CommitTags(tags : TFHIRTagList; key : integer);
    Procedure ProcessBlob(request: TFHIRRequest; response : TFHIRResponse; field : String; comp : TFHIRParserClass);
    function ResolveSearchId(resourceName, compartmentId, compartments : String; baseURL, params : String) : TMatchingResourceList;
    function ScanId(request : TFHIRRequest; entry : TFHIRBundleEntry; ids : TFHIRTransactionEntryList; index : integer) : TFHIRTransactionEntry;
    procedure FixXhtmlUrls(lang, base: String; ids: TFHIRTransactionEntryList; node: TFhirXHtmlNode);
    procedure adjustReferences(request : TFHIRRequest; resp : TFHIRResponse; te : TFHIRTransactionEntry; base : String; entry : TFHIRBundleEntry; ids : TFHIRTransactionEntryList);
    function resolveConditionalURL(request : TFHIRRequest; resp : TFHIRResponse; url : String) : String;
    function commitResource(request: TFHIRRequest; response : TFHIRResponse; upload : boolean; entry : TFHIRBundleEntry; i : integer; id : TFHIRTransactionEntry; session : TFhirSession; resp : TFHIRBundle) : boolean;

    procedure checkProposedContent(request : TFHIRRequest; resource : TFhirResource; tags : TFHIRTagList);
    procedure checkProposedDeletion(request : TFHIRRequest; resource : TFhirResource; tags : TFHIRTagList);

    function EncodeResource(r : TFhirResource; xml : boolean; summary : TFHIRSummaryOption) : TBytes;
    procedure AuditRest(session : TFhirSession; reqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenance; httpCode : Integer; name, message : String); overload;
    procedure AuditRest(session : TFhirSession; reqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenance; opName : String; httpCode : Integer; name, message : String); overload;
    procedure SaveProvenance(session : TFhirSession; prv : TFHIRProvenance);

    procedure CheckCompartments(actual, allowed : String);
    procedure executeReadInTransaction(entry : TFhirBundleEntryRequest; request: TFHIRRequest; response : TFHIRResponse);

    procedure processIncludes(session : TFhirSession; secure : boolean; _includes, _reverseIncludes : String; bundle : TFHIRBundle; keys : TKeyList; field : String; comp : TFHIRParserClass);

    procedure SetConnection(const Value: TKDBConnection);
    procedure ReIndex;
    procedure CheckCreateNarrative(request : TFHIRRequest);
    procedure CreateIndexer;
    function loadCustomResource(ig : TFHIRImplementationGuide; package : TFhirImplementationGuidePackage) : TFHIRCustomResourceInformation;
    procedure ExecuteGraphQL(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse);
    function GraphFollowReference(appInfo : TAdvObject; context : TFHIRResource; reference : TFHIRReference; out targetContext, target : TFHIRResource) : boolean;
    procedure GraphFollowReverseReference(appInfo : TAdvObject; focusType, focusId, requestType, requestParam : String; params : TAdvList<TGraphQLArgument>; list : TAdvList<TFhirResource>);

  protected
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    function  ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; override;
    function  ExecutePatch(request: TFHIRRequest; response : TFHIRResponse) : Boolean; override;
    procedure ExecuteVersionRead(request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteDelete(request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteHistory(request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse); override;
    Function  ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String; override;
    procedure ExecuteConformanceStmt(request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteUpload(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); override;
    function  ExecuteValidation(request: TFHIRRequest; response : TFHIRResponse; opDesc : String) : boolean; override;
    procedure ExecuteTransaction(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteBatch(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteOperation(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); override;
  public
    Constructor Create(lang : String; ServerContext : TFHIRServerContext; repository : TFHIRNativeStorageService);
    Destructor Destroy; Override;
    function Link : TFHIRNativeOperationEngine; overload;

    Property Connection : TKDBConnection read FConnection write SetConnection;
    Property Repository : TFHIRNativeStorageService read FRepository;

    // internal utility functions
    procedure addParam(srch : TFhirCapabilityStatementRestResourceSearchParamList; html : TAdvStringBuilder; n, url, d : String; t : TFhirSearchParamTypeEnum; tgts : Array of String);

    function AddResourceTobundle(bundle : TFHIRBundle; isSecure : boolean; base : String; field : String; comp : TFHIRParserClass; purpose : TFhirSearchEntryModeEnum; makeRequest : boolean; var type_ : String) : TFHIRBundleEntry; overload;
    function check(response : TFHIRResponse; test : boolean; code : Integer; lang, message : String; issueCode : TFhirIssueTypeEnum) : Boolean;
    procedure DefineConformanceResources(base : String); // called after database is created

    // when want an MRN to store the resource against
    function GetPatientId : String; virtual;

    // called when kernel actually wants to process against the store
//    Function Execute(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String; override;

    function  LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; override;

    Property TestServer : boolean read FTestServer write FTestServer;
    Property ServerContext : TFHIRServerContext read FServerContext;

    // index maintenance
    procedure clear(a : TFhirResourceTypeSet);
    procedure storeResources(list: TFHIRResourceList; origin : TFHIRRequestOrigin; upload : boolean);

    // custom resources
    function loadCustomResources(response : TFHIRResponse; id : String; startup : boolean; names : TStringList) : boolean; overload;
    function loadCustomResources(response : TFHIRResponse; key : integer; startup : boolean; names : TStringList) : boolean; overload;
  end;



  TFhirGenerateQAOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function HandlesRequest(request : TFHIRRequest) : boolean; override;
  end;

  TFhirHandleQAPostOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirQuestionnaireGenerationOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirExpandValueSetOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirLookupCodeSystemOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

{$IFNDEF FHIR2}
  TFhirCodeSystemComposeOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirSubsumesOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;
{$ENDIF}

  TFhirValueSetValidationOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirConceptMapTranslationOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirConceptMapClosureOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
    function checkName(request: TFHIRRequest; response : TFHIRResponse; var name : String) : boolean;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirPatientEverythingOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirGenerateDocumentOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  private
    procedure addResource(manager: TFHIRNativeOperationEngine; secure : boolean; bundle : TFHIRBundle; source : TFHIRDomainResource; reference : TFhirReference; required : boolean; compartments : String);
    procedure addSections(manager: TFHIRNativeOperationEngine; secure : boolean; bundle : TFHIRBundle; composition : TFhirComposition; sections : TFhirCompositionSectionList; compartments : String);
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirGenerateCDSHookOperation = class (TFHIROperation)
  private
    procedure processPatientView(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; req : TCDSHookRequest; context : TFHIRPatient; resp : TCDSHookResponse);
    procedure addNamingSystemInfo(ns : TFHIRNamingSystem; baseUrl : String; resp : TCDSHookResponse);
    procedure addSystemCard(resp : TCDSHookResponse; name, publisher, responsible, type_, usage, realm : String);

    procedure executeIdentifierView(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; req : TCDSHookRequest; resp : TCDSHookResponse);
    procedure executeCodeView(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; req : TCDSHookRequest; resp : TCDSHookResponse);
    procedure executePatientView(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; req : TCDSHookRequest; resp : TCDSHookResponse);
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirValidationOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirProcessClaimOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirCurrentTestScriptOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirGenerateSnapshotOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirGenerateTemplateOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirGenerateNarrativeOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirSuggestKeyWordsOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirGetMetaDataOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirAddMetaDataOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirDeleteMetaDataOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirDiffOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  {$IFNDEF FHIR2}
  TServerTransformerServices = class (TTransformerServices)
  private
    FServerContext : TFHIRServerContext;
  public
    Constructor create(ServerContext : TFHIRServerContext);
    Destructor Destroy; override;
    property ServerContext : TFHIRServerContext read FServerContext;

    function oid2Uri(oid : String) : String; override;
    function translate(appInfo : TAdvObject; src : TFHIRCoding; conceptMapUrl : String) : TFHIRCoding; override;
    procedure log(s : String); override;
  end;
  {$ENDIF}

  {$IFNDEF FHIR2}
  TFhirTransformOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;
  {$ENDIF}

  {$IFNDEF FHIR2}
  TFhirActivateOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirObservationStatsOperation = class (TFHIROperation)
  private
    function resolvePatient(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; ref : String) : integer;
    function resolveParameter(code : String): TObservationStatsParameter;
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;
  {$ENDIF}

  TFHIRNativeStorageService = class (TFHIRStorageService)
  private
    FLock: TCriticalSection;
    FDB: TKDBManager;

    FLastSearchKey: integer;
    FLastVersionKey: integer;
    FLastResourceKey: integer;
    FLastEntryKey: integer;
    FLastCompartmentKey: integer;
    FLastObservationKey : integer;
    FLastObservationQueueKey : integer;

    FTotalResourceCount: integer;
    FNextSearchSweep: TDateTime;
    FServerContext : TFHIRServerContext; // not linked

    FClaimQueue: TFHIRClaimList;
    FAudits: TFhirResourceList;

    FAppFolder : String;

    procedure LoadExistingResources(conn: TKDBConnection);
    procedure checkDefinitions;

    procedure DoExecuteOperation(request: TFHIRRequest; response: TFHIRResponse; bWantSession: Boolean);
    function DoExecuteSearch(typekey: integer; compartmentId, compartments: String; params: TParseMap; conn: TKDBConnection): String;
    function getTypeForKey(key: integer): String;
    procedure doRegisterTag(tag: TFHIRTag; conn: TKDBConnection);
    procedure checkRegisterTag(tag: TFHIRTag; conn: TKDBConnection);
    procedure RunValidateResource(i : integer; rtype, id : String; bufJson, bufXml : TAdvBuffer; b : TStringBuilder);

    procedure loadCustomResources(guides : TAdvStringSet);
    procedure StoreObservation(conn: TKDBConnection; key : integer);
    procedure UnStoreObservation(conn: TKDBConnection; key : integer);
    procedure ProcessObservation(conn: TKDBConnection; key : integer);
    function loadResource(conn: TKDBConnection; key : integer) : TFhirResource;
    function resolveReference(conn: TKDBConnection; ref : string) : Integer;
    function resolveConcept(conn: TKDBConnection; c : TFHIRCoding) : Integer; overload;
    function resolveConcept(conn: TKDBConnection; sys, code : String) : Integer; overload;
    procedure ProcessObservationValue(conn: TKDBConnection; key, subj, concept, subconcept : integer; dt, dtMin, dtMax : TDateTime; value : TFHIRType);
    procedure ProcessObservationValueQty(conn: TKDBConnection; key, subj, concept, subconcept : integer; dt, dtMin, dtMax : TDateTime; value : TFHIRQuantity);
    procedure ProcessObservationValueCode(conn: TKDBConnection; key, subj, concept, subconcept : integer; dt, dtMin, dtMax : TDateTime; value : TFHIRCodeableConcept);
  protected
    function GetTotalResourceCount: integer; override;
  public
    constructor Create(DB: TKDBManager; AppFolder: String);
    Destructor Destroy; Override;
    Function Link: TFHIRNativeStorageService; virtual;
    procedure Initialise(ini: TFHIRServerIniFile);
    procedure SaveResource(res: TFhirResource; dateTime: TDateAndTime; origin : TFHIRRequestOrigin);
    procedure RecordFhirSession(session: TFhirSession); override;
    procedure CloseFhirSession(key: integer); override;
    function ProfilesAsOptionList: String; override;
    function NextVersionKey: integer;
    function NextSearchKey: integer;
    function NextResourceKeySetId(aType: String; id: string) : integer;
    function NextResourceKeyGetId(aType: String; var id: string): integer;
    function NextEntryKey: integer;
    function NextCompartmentKey: integer;
    function nextObservationKey : integer;
    Function GetNextKey(keytype: TKeyType; aType: String; var id: string): integer;
    procedure RegisterTag(tag: TFHIRTag; conn: TKDBConnection); overload;
    procedure RegisterTag(tag: TFHIRTag); overload;
    procedure SeeResource(key, vkey: integer; id: string; needsSecure, created : boolean; resource: TFhirResource; conn: TKDBConnection; reload: Boolean; session: TFhirSession);
    procedure DropResource(key, vkey: integer; id, resource: string; indexer: TFhirIndexManager; conn: TKDBConnection);
    procedure RegisterConsentRecord(session: TFhirSession); override;
    procedure Sweep; override;
    function ResourceTypeKeyForName(name: String): integer;
    procedure ProcessSubscriptions; override;
    procedure ProcessObservations; override;
    function GenerateClaimResponse(claim: TFhirClaim): TFhirClaimResponse;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;

    function ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet; override;
    function LookupCode(system, version, code: String): String; override;
    procedure QueueResource(r: TFhirResource); overload; override;
    procedure QueueResource(r: TFhirResource; dateTime: TDateAndTime); overload; override;
    procedure RunValidation; override;

    procedure recordOAuthLogin(id, client_id, scope, redirect_uri, state : String); override;
    function fetchOAuthDetails(key, status : integer; var client_id, name, redirect, state, scope : String) : boolean; override;
    procedure recordOAuthChoice(id : String; scopes, jwt, patient : String); override;
    function hasOAuthSession(id : String; status : integer) : boolean; override;
    function hasOAuthSessionByKey(key, status : integer) : boolean; override;
    procedure updateOAuthSession(id : String; state, key : integer); override;

    function FetchResourceCounts(comps : String) : TStringList; override;

    property ServerContext : TFHIRServerContext read FServerContext write FServerContext;
    Property DB: TKDBManager read FDB;
  end;

implementation

uses
  SystemService,
  FHIRLog,
  TerminologyServerStore,
  SearchProcessor;

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

constructor TFHIRNativeOperationEngine.Create(lang : String; ServerContext : TFHIRServerContext; repository : TFHIRNativeStorageService);
begin
  inherited Create(lang);
  FServerContext := ServerContext;
  FRepository := repository;
  FFactory := TFHIRFactory.create(ServerContext.Validator.Context.link, lang);
  FOperations := TAdvObjectList.create;


  // order of registration matters - general validation must be after value set validation
  FOperations.add(TFhirExpandValueSetOperation.create);
  FOperations.add(TFhirLookupCodeSystemOperation.create);
  FOperations.add(TFhirValueSetValidationOperation.create);
  FOperations.add(TFhirConceptMapTranslationOperation.create);
  FOperations.add(TFhirConceptMapClosureOperation.create);
  FOperations.add(TFhirValidationOperation.create);
  FOperations.add(TFhirGenerateDocumentOperation.create);
  FOperations.add(TFhirPatientEverythingOperation.create);
  FOperations.add(TFhirGenerateQAOperation.create);
  FOperations.add(TFhirHandleQAPostOperation.create);
  FOperations.add(TFhirQuestionnaireGenerationOperation.create);
  FOperations.add(TFhirProcessClaimOperation.create);
  FOperations.add(TFhirCurrentTestScriptOperation.create);
  FOperations.add(TFhirGenerateSnapshotOperation.create);
  FOperations.add(TFhirGenerateCDSHookOperation.create);
  FOperations.add(TFhirGenerateTemplateOperation.create);
  FOperations.add(TFhirGenerateNarrativeOperation.create);
  FOperations.add(TFhirSuggestKeyWordsOperation.create);
  FOperations.add(TFhirGetMetaDataOperation.create);
  FOperations.add(TFhirAddMetaDataOperation.create);
  FOperations.add(TFhirDeleteMetaDataOperation.create);
  FOperations.add(TFhirDiffOperation.create);
  {$IFNDEF FHIR2}
  FOperations.add(TFhirTransformOperation.create);
  {$ENDIF}
  {$IFNDEF FHIR2}
  FOperations.add(TFhirActivateOperation.create);
  FOperations.add(TFhirSubsumesOperation.create);
  FOperations.add(TFhirCodeSystemComposeOperation.create);
  FOperations.add(TFhirObservationStatsOperation.create);
  {$ENDIF}
end;


procedure TFHIRNativeOperationEngine.DefineConformanceResources(base: String);

var
  list : TFhirResourceList;
  op : TFhirOperationDefinition;
  i : Integer;
begin
  list := TFhirResourceList.create;
  try
    for i := 0 to FOperations.Count - 1 do
    begin
      op := TFhirOperation(FOperations[i]).CreateDefinition(base);
      if (op <> nil) then
      begin
        op.tag := NowLocal;
        list.Add(op);
      end;
    end;
    for i := 0 to list.Count - 1 do
      list[i].Tags['internal'] := '1';
    {$IFNDEF FHIR2}
    FRepository.ServerContext.TerminologyServer.declareCodeSystems(list);
    {$ENDIF}
    storeResources(list, roConfig, true);
  finally
    list.Free;
  end;

end;

destructor TFHIRNativeOperationEngine.Destroy;
begin
  FOperations.Free;
  FIndexer.Free;
  FSpaces.free;
  FFactory.Free;
  FRepository.Free;
  inherited;
end;

function TFHIRNativeOperationEngine.AddResourceTobundle(bundle : TFHIRBundle; isSecure : boolean; base : String; field : String; comp : TFHIRParserClass; purpose : TFhirSearchEntryModeEnum; makeRequest : boolean; var type_ : String) : TFHIRBundleEntry;
var
  parser : TFhirParser;
  mem : TBytesStream;
  sId, sAud : String;
  entry : TFHIRBundleEntry;
  op : TFhirOperationOutcome;
begin
  sId := FConnection.ColStringByName['Id'];
  type_ := FConnection.colStringByName['ResourceName'];
  if (FConnection.ColIntegerByName['Status'] = 2) then
  begin
    result := TFHIRBundleEntry.Create;
    try
      result.fullUrl := AppendForwardSlash(base)+type_+'/'+sId;
      bundle.entryList.add(result.Link);
      result.request := TFhirBundleEntryRequest.Create;
      result.request.url := result.fullUrl;
      result.request.method := HttpVerbDELETE;
      result.response := TFhirBundleEntryResponse.Create;
      result.response.lastModified := TDateAndTime.CreateUTC(TSToDateTime(FConnection.ColTimeStampByName['StatedDate']));
      result.response.etag := FConnection.ColStringByName['VersionId'];
      result.Tags['opdesc'] := 'Deleted by '+FConnection.ColStringByName['Name']+' at '+result.response.lastModified.AsString+ '(UTC)';
      sAud := FConnection.ColStringByName['AuditId'];
      if sAud <> '' then
        result.link_List.AddRelRef('audit', 'AuditEvent/'+sAud);
    finally
      result.Free;
    end;
  end
  else if not isSecure and (FConnection.ColIntegerByName['Secure'] = 1) then
  begin
    for entry in bundle.entryList do
      if (entry.resource <> nil) and entry.HasTag(OP_MASK_TAG) then
        exit;
    op := BuildOperationOutcome('en', 'Some resources have been omitted from this bundle because they are labelled with a with a security tag that means this server will only send it if the connection is secure', IssueTypeSuppressed);
    bundle.entryList.Append.resource := op;
    op.Tags[OP_MASK_TAG] := 'secure';
  end
  else
  begin
    if comp = nil then
    begin
      result := bundle.entryList.Append;
      result.Tag := TAdvBuffer.create;
      result.fullUrl := AppendForwardSlash(base)+type_+'/'+sId;
      TAdvBuffer(result.Tag).AsBytes := FConnection.ColBlobByName[field];
      if (purpose <> SearchEntryModeNull) then
      begin
        result.search := TFhirBundleEntrySearch.Create;
        result.search.mode := purpose;
        if (purpose = SearchEntryModeMatch) and not FConnection.ColNullByName['Score1'] then
        begin
          result.search.score := FloatToStr(FConnection.ColIntegerByName['Score1'] / 100);
          if FConnection.ColIntegerByName['Score2'] > 0 then
            result.search.addExtension('http://hl7.org/fhir/StructureDefinition/patient-mpi-match').value := TFhirCode.Create(CODES_TMPICertainty[TMPICertainty(FConnection.ColIntegerByName['Score2'])]);
        end;
      end;
    end
    else
    begin
      mem := TBytesStream.Create(FConnection.ColBlobByName[field]);
      try
        parser := comp.Create(ServerContext.Validator.Context.link, lang);
        try
          parser.source := mem;
          parser.ParserPolicy := xppDrop;
          parser.Parse;
          entry := TFHIRBundleEntry.Create;
          try
            entry.resource := parser.resource.Link;
            entry.fullUrl := AppendForwardSlash(base)+CODES_TFhirResourceType[parser.resource.ResourceType]+'/'+parser.resource.id;
            if (purpose <> SearchEntryModeNull) then
            begin
              entry.search := TFhirBundleEntrySearch.Create;
              entry.search.mode := purpose;
              if (purpose = SearchEntryModeMatch) and not FConnection.ColNullByName['Score1'] then
              begin
                entry.search.score := FloatToStr(FConnection.ColIntegerByName['Score1'] / 100);
                if FConnection.ColIntegerByName['Score2'] > 0 then
                  entry.search.addExtension('http://hl7.org/fhir/StructureDefinition/patient-mpi-match').value := TFhirCode.Create(CODES_TMPICertainty[TMPICertainty(FConnection.ColIntegerByName['Score2'])]);
              end;
            end;
            bundle.entryList.add(entry.Link);
            result := entry;
          finally
            entry.Free;
          end;
        finally
          parser.free;
        end;
      finally
        mem.Free;
      end;
    end;
    if (makeRequest) then
    begin
      result.response := TFhirBundleEntryResponse.Create;
      result.response.lastModified := TDateAndTime.CreateUTC(TSToDateTime(FConnection.ColTimeStampByName['StatedDate']));
      result.response.etag := FConnection.ColStringByName['VersionId'];
      sAud := FConnection.ColStringByName['AuditId'];
      if sAud <> '' then
        result.link_List.AddRelRef('audit', 'AuditEvent/'+sAud);

      result.request := TFhirBundleEntryRequest.Create;
      if FConnection.ColIntegerByName['Status'] = 1 then
      begin
        result.request.url := AppendForwardSlash(base)+type_+'/'+sId;
        result.request.method := HttpVerbPUT;
        result.Tags['opdesc'] := 'Updated by '+FConnection.ColStringByName['Name']+' at '+result.response.lastModified.AsString+ '(UTC)';
      end
      else
      begin
        result.request.method := HttpVerbPOST;
        result.request.url := AppendForwardSlash(base)+type_;
        result.Tags['opdesc'] := 'Created by '+FConnection.ColStringByName['Name']+' at '+result.response.lastModified.AsString+ '(UTC)';
      end;
    end;
  end;
end;

function TFHIRNativeOperationEngine.check(response: TFHIRResponse; test: boolean; code : Integer; lang, message: String; issueCode : TFhirIssueTypeEnum): Boolean;
begin
  result := test;
  if not test and (response <> nil) then
  begin
    response.HTTPCode := code;
    response.Message := message;
    response.ContentType := 'text/plain';
    response.Body := message;
    response.Resource := BuildOperationOutcome(lang, message, issueCode);
  end;
end;


function TFHIRNativeOperationEngine.EncodeResource(r: TFhirResource; xml : boolean; summary : TFHIRSummaryOption): TBytes;
var
  b : TBytesStream;
  comp : TFHIRComposer;
begin
  b :=  TBytesStream.Create;
  try
    if (xml) then
      comp := TFHIRXmlComposer.Create(ServerContext.Validator.Context.Link, 'en')
    else
      comp := TFHIRJsonComposer.Create(ServerContext.Validator.Context.Link, 'en');
    try
      comp.SummaryOption := summary;
      comp.NoHeader := true;

      comp.Compose(b, r, false, nil);
    finally
      comp.Free;
    end;
    result := copy(b.Bytes, 0, b.size);
  finally
    b.free;
  end;
end;


procedure TFHIRNativeOperationEngine.addParam(srch : TFhirCapabilityStatementRestResourceSearchParamList; html : TAdvStringBuilder; n, url, d : String; t : TFhirSearchParamTypeEnum; tgts : Array of String);
var
  param : TFhirCapabilityStatementRestResourceSearchParam;
begin
  param := TFhirCapabilityStatementRestResourceSearchParam.create;
  try
    param.name := n;
    param.definition := url;
    param.documentation := d;
    param.type_ := t;
    srch.add(param.link);
  finally
    param.free;
  end;
//  html.append('<li>'+n+' : '+FormatTextToHTML(d)+'</li>');
end;


procedure TFHIRNativeOperationEngine.ExecuteConformanceStmt(request: TFHIRRequest; response: TFHIRResponse);
var
  oConf : TFhirCapabilityStatement;
  res : TFhirCapabilityStatementRestResource;
  a : String;
  html : TAdvStringBuilder;
  c : TFhirContactPoint;
  i : integer;
  op : TFhirCapabilityStatementRestOperation;
  ct : TFhirConformanceContact;
begin
  try

    response.HTTPCode := 200;
    oConf := TFhirCapabilityStatement.Create;
    response.Resource := oConf;

    oConf.id := 'FhirServer';
    ct := oConf.contactList.Append;
    c := ct.telecomList.Append;
    c.system := ContactPointSystemOther;
    c.value := 'http://healthintersections.com.au/';
    if ServerContext.FormalURLPlain <> '' then
      oConf.url := AppendForwardSlash(ServerContext.FormalURLPlainOpen)+'metadata'
    else
      oConf.url := 'http://fhir.healthintersections.com.au/open/metadata';

    oConf.version := FHIR_GENERATED_VERSION+'-'+SERVER_VERSION; // this conformance statement is versioned by both
    oConf.name := 'Health Intersections FHIR Server Conformance Statement';
    oConf.publisher := 'Health Intersections'; //
    oConf.description := 'Standard Conformance Statement for the open source Reference FHIR Server provided by Health Intersections';
    oConf.status := PublicationStatusActive;
    oConf.experimental := false;
    oConf.date := TDateAndTime.CreateUTC(UniversalDateTime);
    oConf.software := TFhirCapabilityStatementSoftware.Create;
    oConf.software.name := 'Reference Server';
    oConf.software.version := SERVER_VERSION;
    oConf.software.releaseDate := TDateAndTime.createXML(SERVER_RELEASE_DATE);
    if ServerContext.FormalURLPlainOpen <> '' then
    begin
      oConf.implementation_ := TFhirCapabilityStatementImplementation.Create;
      oConf.implementation_.description := 'FHIR Server running at '+ServerContext.FormalURLPlainOpen;
      oConf.implementation_.url := ServerContext.FormalURLPlainOpen;
    end;
    if assigned(OnPopulateConformance) then
      OnPopulateConformance(self, oConf);

    oConf.acceptUnknown := UnknownContentCodeBoth;
    {$IFNDEF FHIR2}
    oConf.formatList.Append.value := 'application/fhir+xml';
    oConf.formatList.Append.value := 'application/fhir+json';
    {$ELSE}
    oConf.formatList.Append.value := 'application/xml+fhir';
    oConf.formatList.Append.value := 'application/json+fhir';
    {$ENDIF}

    oConf.fhirVersion := FHIR_GENERATED_VERSION;
    oConf.restList.add(TFhirCapabilityStatementRest.Create);
    oConf.restList[0].mode := RestfulCapabilityModeServer;
    oConf.restList[0].addExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-websocket', request.baseUrl+'websockets');
    oConf.restList[0].interactionList.Append.code := SystemRestfulInteractionTransaction;
    oConf.restList[0].interactionList.Append.code := SystemRestfulInteractionSearchSystem;
    oConf.restList[0].interactionList.Append.code := SystemRestfulInteractionHistorySystem;
    {$IFDEF FHIR2}
    oConf.restList[0].transactionMode := TransactionModeBoth;
    {$ENDIF}
    oConf.text := TFhirNarrative.create;
    oConf.text.status := NarrativeStatusGenerated;

    {$IFNDEF FHIR2}
    oConf.instantiatesList.AddItem(TFHIRUri.Create('http://hl7.org/fhir/Conformance/terminology-server'));
    {$ENDIF}
    {$IFDEF FHIR2}
    ServerContext.TerminologyServer.declareSystems(oConf);
    {$ENDIF}
    if assigned(OnPopulateConformance) and request.secure then // only add smart on fhir things on a secure interface
      OnPopulateConformance(self, oConf);
    AddCDSHooks(oConf.restList[0]);

    html := TAdvStringBuilder.Create;
    try
      html.append('<div><h2>FHIR Reference Server Conformance Statement</h2><p>FHIR v'+FHIR_GENERATED_VERSION+' released '+RELEASE_DATE+'. '+
       'Reference Server version '+SERVER_VERSION+' built '+SERVER_RELEASE_DATE+'</p><table class="grid"><tr><th>Resource Type</th><th>Profile</th><th>Read</th><th>V-Read</th><th>Search</th><th>Update</th><th>Updates</th><th>Create</th><th>Delete</th><th>History</th></tr>'+#13#10);
      for a in ServerContext.ValidatorContext.allResourceNames do
      begin
        if ServerContext.ResConfig[a].Supported and (a <> 'MessageHeader') and (a <> 'Custom') then
        begin
          if a = 'Binary' then
            html.append('<tr><td>'+a+'</td>'+
            '<td>--</td>')
          else
            html.append('<tr><td>'+a+'</td>'+
            '<td><a href="'+request.baseUrl+'StructureDefinition/'+lowercase(a)+'?format=text/html">'+lowercase(a)+'</a></td>');
          res := TFhirCapabilityStatementRestResource.create;
          try
            res.type_Element := TFhirEnum.create('http://hl7.org/fhir/resource-types', a);
            if a <> 'Binary' then
              res.profile := FFactory.makeReference(request.baseUrl+'StructureDefinition/'+lowercase(a));
            if (a <> 'MessageHeader') and (a <> 'Parameters') Then
            begin
              if request.canRead(a)  then
              begin
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td><td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
                res.interactionList.Append.code := TypeRestfulInteractionRead;
                res.interactionList.Append.code := TypeRestfulInteractionVread;
                res.readHistory := true;
              end
              else
                html.append('<td align="center"></td><td align="center"></td>');
              if ServerContext.ResConfig[a].cmdSearch and request.canRead(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionSearchType;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if ServerContext.ResConfig[a].cmdUpdate and request.canWrite(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionUpdate;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if ServerContext.ResConfig[a].cmdHistoryType and request.canRead(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionHistoryType;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if ServerContext.ResConfig[a].cmdCreate and request.canWrite(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionCreate;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if ServerContext.ResConfig[a].cmdDelete and request.canWrite(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionDelete;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if ServerContext.ResConfig[a].cmdHistoryInstance and request.canRead(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionHistoryInstance;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              html.append('<td></td>');
//                html.append('<br/>search</td><td><ul>');
              for i := 0 to ServerContext.Indexes.Indexes.count - 1 do
                if (ServerContext.Indexes.Indexes[i].ResourceType = a) then
                  if ServerContext.Indexes.Indexes[i].Name <> '_query' then
                    addParam(res.searchParamList, html, ServerContext.Indexes.Indexes[i].Name, ServerContext.Indexes.Indexes[i].uri, ServerContext.Indexes.Indexes[i].Description, ServerContext.Indexes.Indexes[i].SearchType, ServerContext.Indexes.Indexes[i].TargetTypes);

//              addParam(res.searchParamList, html, '_id', 'http://hl7.org/fhir/search', 'Resource Logical ID', SearchParamTypeToken, []);
              addParam(res.searchParamList, html, '_text', 'http://hl7.org/fhir/search', 'General Text Search of the narrative portion', SearchParamTypeString, []);
              addParam(res.searchParamList, html, '_profile', 'http://hl7.org/fhir/search', 'Search for resources that conform to a profile', SearchParamTypeReference, []);
              addParam(res.searchParamList, html, '_security', 'http://hl7.org/fhir/search', 'Search for resources that have a particular security tag', SearchParamTypeReference, []);
              addParam(res.searchParamList, html, '_sort', 'http://hl7.org/fhir/search', 'Specify one or more other parameters to use as the sort order', SearchParamTypeToken, []);
              addParam(res.searchParamList, html, '_count', 'http://hl7.org/fhir/search', 'Number of records to return', SearchParamTypeNumber, []);
              addParam(res.searchParamList, html, '_summary', 'http://hl7.org/fhir/search', 'Return just a summary for resources that define a summary view', SearchParamTypeNumber, []);
              addParam(res.searchParamList, html, '_include', 'http://hl7.org/fhir/search', 'Additional resources to return - other resources that matching resources refer to', SearchParamTypeToken, ALL_RESOURCE_TYPE_NAMES);
              addParam(res.searchParamList, html, '_reverseInclude', 'http://hl7.org/fhir/search', 'Additional resources to return - other resources that refer to matching resources (this is trialing an extension to the specification)', SearchParamTypeToken, ALL_RESOURCE_TYPE_NAMES);
              addParam(res.searchParamList, html, '_filter', 'http://hl7.org/fhir/search', 'filter parameter as documented in the specification', SearchParamTypeToken, ALL_RESOURCE_TYPE_NAMES);

//              html.append('</ul>');                                                                                                                               }
            end;
            html.append('</tr>'#13#10);


              //<th>Search/Updates Params</th>
              // html.append('n : offset<br/>');
              // html.append('count : # resources per request<br/>');
              // html.append(m.Indexes[i]+' : ?<br/>');
            oConf.restList[0].resourceList.add(res.Link);
          finally
            res.free;
          end;
        end;
      end;
      html.append('</table>'#13#10);

      html.append('<p>Operations</p>'#13#10'<ul>'+#13#10);
      for i := 0 to FOperations.Count - 1 do
      begin
        op := oConf.restList[0].operationList.Append;
        op.name := TFhirOperation(FOperations[i]).Name;
        if TFhirOperation(FOperations[i]).formalURL <> '' then
          op.definition := FFactory.makeReference(TFhirOperation(FOperations[i]).formalURL)
        else
          op.definition := FFactory.makeReference('OperationDefinition/fso-'+op.name);
        html.append(' <li>'+op.name+': see OperationDefinition/fso-'+op.name+'</li>'#13#10);
      end;
      html.append('</ul>'#13#10);


      html.append('</div>'#13#10);
      // operations
      oConf.text.div_ := TFHIRXhtmlParser.parse(lang, xppReject, [], html.AsString);
    finally
      html.free;
    end;

    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

Function TFHIRNativeOperationEngine.ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
var
  sId, s : String;
  resourceKey, i : Integer;
  key : Integer;
  tags : TFHIRTagList;
  ok : boolean;
  tnow : TDateAndTime;
  needSecure : boolean;
  list : TMatchingResourceList;
begin
  key := 0;
  CheckCreateNarrative(request);
  try
    ok := true;
    if not check(response, request.canWrite(request.ResourceName), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      ok := false;
    if ok and not check(response, opAllowed(request.ResourceName, fcmdCreate), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      ok := false;

    if ok and not check(response, request.Resource <> nil, 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang), IssueTypeRequired) or not check(response, request.ResourceName = request.resource.fhirType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang), IssueTypeInvalid) then
      ok := false;

    ok := checkOkToStore(request, response, needSecure);

    if ok and ServerContext.Validate and not context.upload and (request.Session <> nil) and (request.adaptor = nil) then
    begin
      if not ExecuteValidation(request, response, 'Create Resource '+request.ResourceName+'/'+request.Id+' ('+request.originalId+')') then
        ok := false
      else
        response.Resource := nil;
    end;

    if request.IfNoneExist <> '' then
    begin
      s := request.IfNoneExist;
      if (s.Contains('?')) then
        s := s.Substring(s.IndexOf('?')+1);
      list := ResolveSearchId(request.ResourceName, request.compartmentId, request.compartments, request.baseUrl, s);
      try
        ok := false;
        if list.Count = 1 then
        begin
          response.HTTPCode := 200;
        end
        else if list.Count > 1 then
          check(response, false, 412, lang, GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang), IssueTypeNotFound)
        else
          ok := true;
      finally
        list.Free;
      end;
    end;

    if request.resource.meta = nil then
      request.resource.meta := TFhirMeta.Create;
    request.Resource.meta.lastUpdated := NowUTC;
    request.Resource.meta.versionId := '1';
    updateProvenance(request.Provenance, request.ResourceName, sid, '1');
    tnow := request.Resource.meta.lastUpdated.AsUTC;
    try
      checkNotRedacted(request.Resource.meta, 'Creating resource');
      tags := TFHIRTagList.create;
      try
        tags.readTags(request.resource.meta);
        if (request.hasTestingTag) then
          tags.forceTestingTag;
        if not ok then
          // nothing
        else if (idState = idCheckNew) then
        begin
          sId := request.Id;
          if not check(response, sId <> '', 404, lang, GetFhirMessage('MSG_INVALID_ID', lang), IssueTypeInvalid) or
            not check(response, (Length(sId) <= ID_LENGTH) and AddNewResourceId(request.resourceName, sId, lang, tags.hasTestingTag, resourceKey), 404, lang, GetFhirMessage('MSG_INVALID_ID', lang), IssueTypeInvalid) then
              ok := false;
        end
        else if (idState = idMaybeNew) and (request.Id <> '') then
        begin
          sId := request.Id;
          if not check(response, (Length(sId) <= ID_LENGTH) and AddNewResourceId(request.resourceName, sId, lang, tags.hasTestingTag, resourceKey), 404, lang, GetFhirMessage('MSG_INVALID_ID', lang), IssueTypeInvalid) then
            ok := false;
        end
        else if (idState = idIsNew) then
        begin
          sid := request.id;
          resourceKey := iAssignedKey;
        end
        else if not check(response, GetNewResourceId(request.ResourceName, tags.hasTestingTag, sId, resourceKey), 404, lang, StringFormat(GetFhirMessage('MSG_DUPLICATE_ID', lang), [sId, request.ResourceName]), IssueTypeDuplicate) then
           ok := false

        else
          request.resource.id := sId;

        if ok then
          if not check(response, request.Resource.id = sId, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.Resource.id+'/'+sId+' (1)', IssueTypeInvalid) then
            ok := false;

        if ok then
        begin


          checkProposedContent(request, request.Resource, tags);
          result := sId;
          request.id := sId;
          key := FRepository.NextVersionKey;
          for i := 0 to tags.count - 1 do
            FRepository.RegisterTag(tags[i], FConnection);

          FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Status, SessionKey, ForTesting, Secure, '+
                  'Tags, XmlContent, XmlSummary, JsonContent, JsonSummary) values (:k, :rk, :sd, :td, :v, :f, 0, :s, :ft, :sec, :tb, :xc, :xs, :jc, :js)';
          FConnection.prepare;
          try
            FConnection.BindInteger('k', key);
            FConnection.BindInteger('rk', resourceKey);
            FConnection.BindTimeStamp('sd', DateTimeToTS(tnow.GetDateTime));
            FConnection.BindTimeStamp('td', DateTimeToTS(tnow.GetDateTime));
            request.SubId := '1';
            FConnection.BindString('v', '1');
            FConnection.BindIntegerFromBoolean('sec', needSecure);
            if request.Session <> nil then
              FConnection.BindInteger('s', request.Session.Key)
            else
              FConnection.BindInteger('s', 0);
            FConnection.BindInteger('f', 2);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindIntegerFromBoolean('ft', tags.hasTestingTag);
            FConnection.BindBlobFromBytes('xc', EncodeResource(request.Resource, true, soFull));
            FConnection.BindBlobFromBytes('jc', EncodeResource(request.Resource, false, soFull));
            markRedacted(request.Resource.meta);
            FConnection.BindBlobFromBytes('xs', EncodeResource(request.Resource, true, soSummary));
            FConnection.BindBlobFromBytes('js', EncodeResource(request.Resource, false, soSummary));
            unmarkRedacted(request.Resource.meta);
            FConnection.Execute;
            CommitTags(tags, key);
          finally
            FConnection.Terminate;
          end;
          FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 0 where ResourceKey = '+inttostr(resourceKey));
          if ((request.ResourceEnum = frtAuditEvent) and request.Resource.hasTag('verkey')) then
            FConnection.ExecSQL('update Versions set AuditKey = '+inttostr(resourceKey)+' where ResourceVersionKey = '+request.Resource.Tags['verkey']);

          CreateIndexer;
          CheckCompartments(FIndexer.execute(resourceKey, sId, request.resource, tags), request.compartments);
          FRepository.SeeResource(resourceKey, key, sId, needSecure, true, request.Resource, FConnection, false, request.Session);
          if request.resourceEnum = frtPatient then
            FConnection.execSQL('update Compartments set CompartmentKey = '+inttostr(resourceKey)+' where Id = '''+sid+''' and CompartmentKey is null');
          response.HTTPCode := 201;
          response.Message := 'Created';
          response.Location := request.baseUrl+request.ResourceName+'/'+sId+'/_history/1';
          response.Resource := request.Resource.Link;
          response.LastModifiedDate := now;

          response.id := sId;
          response.versionId := '1';
          if request.Provenance <> nil then
            SaveProvenance(request.Session, request.Provenance);
        end;
      finally
        tags.free;
      end;
    finally
      tnow.Free;
    end;
    if request.ResourceEnum <> frtAuditEvent then // else you never stop
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, sid, '1', key, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceENum <> frtAuditEvent then // else you never stop
        AuditRest(request.session, request.requestId, request.ip, request.ResourceName, sid, '1', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteDelete(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  key, nvid, i : Integer;
  tnow : TDateTime;
  tags : TFHIRTagList;
  list : TMatchingResourceList;
  ok : boolean;

  meta : TFhirMeta;
  rp : TFhirParameters;

begin
  key := 0;
  nvid := 0;
  try
    ok := true;
    if not check(response, request.canWrite(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      ok := false;

      if (request.DefaultSearch) then
      begin
        list := ResolveSearchId(request.ResourceName, request.compartmentId, request.compartments, request.baseUrl, request.Parameters.Source);
        try
          ok := false;
          if list.count = 0 then
            NoMatch(request, response)
          else if list.Count > 1 then
            check(response, false, 412, lang, GetFhirMessage('DELETE_MULTIPLE_MATCHES', lang), IssueTypeNotFound)
          else
          begin
            request.Id := list[0].name;
            ok := true;
          end;
        finally
          list.Free;
        end;
      end;


    if ok and not check(response, length(request.id) <= ID_LENGTH, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id]), IssueTypeInvalid) then
      ok := false;

    if ok and (not FindResource(request.ResourceName, request.Id, true, resourceKey, request, response, request.compartments)) then
    begin
      response.HTTPCode := 204;
      response.Message := 'No Content';
      response.ContentType := 'text/plain';
      response.Body := '';
      ok := false;
    end;

    if ok and FTestServer and not check(response, request.id <> 'example', 400, lang, GetFhirMessage('MSG_RESOURCE_EXAMPLE_PROTECTED', lang), IssueTypeForbidden) then
      ok := false;


    if ok and ((request.Resource <> nil) and not check(response, request.id = request.Resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.id+'/'+request.resource.id+' (2)', IssueTypeInvalid)) Then
      ok := false;

    if ok then
    begin
      tnow := UniversalDateTime;
      key := FRepository.NextVersionKey;
      nvid := FConnection.CountSQL('Select Max(Cast(VersionId as '+DBIntType(FConnection.Owner.Platform)+')) from Versions where ResourceKey = '+IntToStr(resourceKey)) + 1;

      tags := TFHIRTagList.create;

      meta := nil;
      rp := TFhirParameters.create;

      try

        if request.resource <> nil then
        begin
          meta := request.resource.meta.Link;
          tags.readTags(request.resource.meta);
        end
        else
          meta := TFhirMeta.Create;
        with rp.parameterList.Append do
        begin
          name := 'meta';
          value := meta.Link;
        end;
        LoadTags(tags, ResourceKey);
        tags.writeTags(meta);

        checkProposedDeletion(request, request.Resource, tags);

        for i := 0 to tags.count - 1 do
          FRepository.RegisterTag(tags[i], FConnection);

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Status, SessionKey, Secure, ForTesting, Tags) values '+
                                                             '(:k,:rk, :sd, :td, :v, :f, 2, :s, 0, 0, :t)';
        FConnection.prepare;
        try
          FConnection.BindInteger('k', key);
          FConnection.BindInteger('rk', resourceKey);
          FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
          FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
          FConnection.BindString('v', inttostr(nvid));
          Request.SubId := inttostr(nvid);
          if request.Session = nil then
            FConnection.BindInteger('s', 0)
          else
            FConnection.BindInteger('s', request.Session.Key);
          FConnection.BindInteger('f', 0);
          FConnection.BindBlobFromBytes('t', tags.json);

          FConnection.Execute;
        finally
          FConnection.Terminate;
        end;
        FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 1 where ResourceKey = '+inttostr(resourceKey));
        CommitTags(tags, key);
        FConnection.execSQL('Update IndexEntries set Flag = 2 where ResourceKey = '+IntToStr(resourceKey));
        CreateIndexer;
        FRepository.DropResource(ResourceKey, key, request.Id, request.ResourceName, FIndexer, FConnection);
        response.HTTPCode := 204;
        response.Message := GetFhirMessage('MSG_DELETED_DONE', lang);
        if request.resource <> nil then
        begin
          response.HTTPCode := 200;
          response.Message := GetFhirMessage('MSG_DELETED_DONE', lang);
          response.Resource := FFactory.makeByName(codes_TFHIRResourceType[request.Resource.resourceType]) as TFhirResource;
          response.Resource.id := request.id;
          response.Resource.meta := meta.link;
        end;

      finally
        tags.free;
        meta.Free;
        rp.Free;
      end;
    end;
    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, inttostr(nvid), key, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, inttostr(nvid), 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteGraphQL(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  gql : TFHIRGraphQLEngine;
  str : TStringBuilder;
begin
  try
    if ExecuteRead(request, response, true) then
    begin
      gql := TFHIRGraphQLEngine.Create;
      try
        gql.appInfo := request.Link;
        gql.OnFollowReference := GraphFollowReference;
        gql.OnFollowReverseReference := GraphFollowReverseReference;
        if request.GraphQL <> nil then
          gql.queryDocument := request.GraphQL.Link
        else if request.Parameters.VarExists('query') then
          gql.queryDocument := TGraphQLParser.parse(request.Parameters.Value['query'])
        else
          raise EGraphQLException.Create('Unable to find GraphQL to execute');
        gql.focus := response.Resource.Link;
        gql.execute;
        response.Resource := nil;
        str := TStringBuilder.Create;
        try
          gql.output.write(str, 0);
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
    on e : EGraphQLException do
    begin
      response.HTTPCode := 400;
      response.Message := 'Error in GraphQL';
      response.Resource := BuildOperationOutcome(request.Lang, e, IssueTypeInvalid);
    end;
    on e : Exception do
    begin
      response.HTTPCode := 500;
      response.Message := 'Error processing GraphQL';
      response.Resource := BuildOperationOutcome(request.Lang, e, IssueTypeException);
    end;
  end;
end;


function TFHIRNativeOperationEngine.BuildHistoryResultSet(request: TFHIRRequest; response: TFHIRResponse; var searchKey, link, sql, title, base : String; var total : Integer) : boolean;
var
  cmp : String;
  resourceKey : integer;
  lt : string;
  id : String;
  i : integer;
  since, prior : TDateTime;
  rn : String;
begin
  // todo: restrict to readable resources
  id := FhirGUIDToString(CreateGuid); // this the id of the search
  searchKey := inttostr(FRepository.NextSearchKey);
  FConnection.ExecSQL('insert into Searches (SearchKey, Id, Count, Type, Date, Summary) values ('+searchKey+', '''+id+''', 0, 2, '+DBGetDate(FConnection.Owner.Platform)+', '+booleanToSQl(false)+')');

  if (request.compartments <> '') then
    cmp := ' and Ids.ResourceKey in (select ResourceKey from Compartments where TypeKey = '+inttostr(ServerContext.ResConfig['Patient'].key)+' and Id in ('+request.compartments+'))'
  else
    cmp := '';

  result := true;
  case request.CommandType of
    fcmdHistoryInstance :
      begin
        TypeNotFound(request, response);
        if not check(response, request.canRead(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
          result := false
        else if (length(request.id) > ID_LENGTH) or not FindResource(request.ResourceName, request.Id, true, resourceKey, request, response, request.compartments) then
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
        if not check(response, request.canRead(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
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
              lt := lt+','+inttostr(FRepository.ResourceTypeKeyForName(rn));
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
    for i := 0 to request.Parameters.getItemCount - 1 do
      if request.Parameters.VarName(i) = '_since' then
      begin
        since := DTReadDateTZ('yyyy-mm-ddThh:nn:ss.zzzzzz', request.Parameters.Value[request.Parameters.VarName(i)], false);
        FConnection.SQL := FConnection.SQL + ' and StatedDate >= :since ';
        base := base +'_since='+request.Parameters.Value[request.Parameters.VarName(i)];
      end
      else if request.Parameters.VarName(i) = '_prior' then
      begin
        prior := DTReadDateTZ('yyyy-mm-ddThh:nn:ss.zzzzzz', request.Parameters.Value[request.Parameters.VarName(i)], false);
        FConnection.SQL := FConnection.SQL + ' and StatedDate <= :prior ';
        base := base +'&_prior='+request.Parameters.Value[request.Parameters.VarName(i)];
      end;
    if (prior = MIN_DATE) then
      base := base +'&_prior='+FormatDateTime('yyyy-mm-dd''T''hh:nn:ss''Z''', UniversalDateTime, FormatSettings);

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

function extractProfileId(base, s : String) : String;
begin
  if s.StartsWith(base+'StructureDefinition/') and s.EndsWith('/$questionnaire') then
    result := s.Substring(0, s.Length-15).Substring(base.Length+8)
  else
    raise Exception.Create('Did not understand Questionnaire identifier');
end;



procedure TFHIRNativeOperationEngine.ExecuteHistory(request: TFHIRRequest; response: TFHIRResponse);
var
  offset, count, i : integer;
  bundle : TFHIRBundle;
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
  comp : TFHIRParserClass;
  needsObject : boolean;
  type_ : String;
begin
  try
    if request.parameters.value[HISTORY_PARAM_NAME_ID] <> '' then
    begin
      ok := FindSavedSearch(request.parameters.value[HISTORY_PARAM_NAME_ID], request.Session, 2, id, link, sql, title, base, total, dummy, request.strictSearch, reverse);
      if check(response, ok, 400, lang, StringFormat(GetFhirMessage('MSG_HISTORY_EXPIRED', lang), [request.parameters.value[HISTORY_PARAM_NAME_ID]]), IssueTypeProcessing) then
        link := HISTORY_PARAM_NAME_ID+'='+request.parameters.value[HISTORY_PARAM_NAME_ID]
    end
    else
      ok := BuildHistoryResultSet(request, response, id, link, sql, title, base, total);

    if ok then
    begin
      offset := 0;
      count := 50;
      for i := 0 to request.Parameters.getItemCount - 1 do
        if request.Parameters.VarName(i) = SEARCH_PARAM_NAME_OFFSET then
          offset := StrToIntDef(request.Parameters.Value[request.Parameters.VarName(i)], 0)
        else if request.Parameters.VarName(i) = '_count' then
          count := StrToIntDef(request.Parameters.Value[request.Parameters.VarName(i)], 0);
      if (count < 2) then
        count := SEARCH_PAGE_DEFAULT
      else if (Count > SEARCH_PAGE_LIMIT) then
        count := SEARCH_PAGE_LIMIT;
      if offset < 0 then
        offset := 0;

      chooseField(response.Format, soFull, request.Adaptor, field, comp, needsObject);
      if (not needsObject) then
        comp := nil;

      bundle := TFHIRBundle.Create(BundleTypeHistory);
      try
        if response.Format <> ffUnspecified then
          base := base + '&_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
        bundle.total := inttostr(total);
        bundle.Tags['sql'] := sql;
        bundle.meta := TFHIRMeta.create;
        bundle.meta.lastUpdated := NowUTC;
        bundle.link_List.AddRelRef('self', base+link);

        bundle.link_List.AddRelRef('self', base+link);

        if (offset > 0) or (Count < total) then
        begin
          bundle.link_List.AddRelRef('first', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
          if offset - count >= 0 then
            bundle.link_List.AddRelRef('previous', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
          if offset + count < total then
            bundle.link_List.AddRelRef('next', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
          if count < total then
            bundle.link_List.AddRelRef('last', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((total div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
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
            AddResourceTobundle(bundle, request.secure, request.baseUrl, field, comp, SearchEntryModeNull, true, type_);
        finally
          FConnection.Terminate;
        end;

        bundle.id := FhirGUIDToString(CreateGUID);
        bundle.Tags['sql'] := sql;

        response.HTTPCode := 200;
        response.Message := 'OK';
        response.Body := '';
        response.bundle := bundle.Link;
      finally
        bundle.Free;
      end;
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
    end;
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;


function TFHIRNativeOperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse; ignoreHeaders : boolean) : boolean;
var
  resourceKey : integer;
  field : String;
  comp : TFHIRParserClass;
  needsObject : boolean;
begin
  result := false;
  try
    NotFound(request, response);
    if request.canRead(request.ResourceName) and check(response, opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (length(request.id) <= ID_LENGTH) and FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments) then
      begin
        chooseField(response.Format, request.Summary, request.Adaptor, field, comp, needsObject);
        FConnection.SQL := 'Select Secure, StatedDate, VersionId, '+field+' from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
        FConnection.Prepare;
        try
          FConnection.Execute;
          if FConnection.FetchNext then
          Begin
            if (FConnection.ColIntegerByName['Secure'] = 1) and not request.secure then
              check(response, false, 403, lang, 'This resource is labelled with a security tag that means this server will only send it if the connection is secure', IssueTypeSuppressed)
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

              if not (request.ResourceEnum in [frtStructureDefinition]) then
                response.link_List.AddRelRef('edit-form', request.baseUrl+request.ResourceName+'/'+request.id+'/$qa-edit');

              response.link_List.AddRelRef('z-edit-src', request.baseUrl+request.ResourceName+'/'+request.id+'/$edit');
              processBlob(request, Response, field, comp);
            end;
          end;
        finally
          FConnection.Terminate;
        end;
      end;
    end;
    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;


procedure TFHIRNativeOperationEngine.executeReadInTransaction(entry : TFhirBundleEntryRequest; request: TFHIRRequest; response: TFHIRResponse);
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
    raise Exception.Create('not done yet') // system search
  else if (s.StartsWith('$')) then
    raise Exception.Create('not done yet') // system level operation
  else if (s.StartsWith('_')) then
    raise Exception.Create('not done yet') // system history
  else if not StringArrayExistsSensitive(CODES_TFhirResourceType, s) then
    raise Exception.Create('Unknown path type "'+entry.url+'"')
  else
  begin
    request.ResourceName := s;
    s := NextSegment(url);
    if (s = '') then
      ExecuteSearch(request, response)
    else if (s.StartsWith('$')) then
      raise Exception.Create('not done yet') // resource type level operation
    else if (s.StartsWith('_')) then
      raise Exception.Create('not done yet') // resource type history
    else if not IsId(s) then
      raise Exception.Create('Unknown path type "'+entry.url+'"')
    else
    begin
      request.Id := s;
      s := NextSegment(url);
      if (s = '') then
        ExecuteRead(request, response, false)
      else if (s.StartsWith('$')) then
        raise Exception.Create('not done yet') // resource instance level operation
      else if (s = '_history') then
      begin
        s := NextSegment(url);
        if (s = '') then
          ExecuteHistory(request, response)
        else if not IsId(s) then
          raise Exception.Create('Unknown path type "'+entry.url+'"')
        else
        begin
          request.SubId := s;
          s := NextSegment(url);
          if (s = '') then
            ExecuteVersionRead(request, response)
          else
            raise Exception.Create('Unknown path type "'+entry.url+'"')
        end;
      end
      else
        raise Exception.Create('Unknown path type "'+entry.url+'"')
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

function TFHIRNativeOperationEngine.BuildSearchResultSet(typekey : integer; session : TFHIRSession; aType : String; params : TParseMap; baseURL, compartments, compartmentId : String; op : TFHIROperationOutcome; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean):String;
var
  id : string;
begin
  id := FhirGUIDToString(CreateGuid);
  result := inttostr(FRepository.NextSearchKey);
  if params.VarExists('_query') and (params.getVar('_query') <> '') then
  begin
    if (params.getVar('_query') = 'mpi') and (aType = 'Patient') then
      ProcessMPISearch(typekey, session, aType, params, baseURL, compartments, compartmentId, id, result, link, sql, total, summaryStatus, strict, reverse)
    else
      raise exception.create('The query "'+params.getVar('_query')+'" is not known');
  end
  else
    ProcessDefaultSearch(typekey, session, aType, params, baseURL, compartments, compartmentId, id, result, op, link, sql, total, summaryStatus, strict, reverse);
end;

procedure TFHIRNativeOperationEngine.ProcessDefaultSearch(typekey : integer; session : TFHIRSession; aType : String; params : TParseMap; baseURL, compartments, compartmentId : String; id, key : string; op : TFHIROperationOutcome; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean);
var
  sp : TSearchProcessor;
  s : String;
  csql : String;
begin
  if (session = nil) or session.canReadAll then
    s := 'null'
  else
    s := inttostr(Session.Key);
  FConnection.ExecSQL('insert into Searches (SearchKey, Id, Count, Type, Date, Summary, SessionKey) values ('+key+', '''+id+''', 0, 1, '+DBGetDate(FConnection.Owner.Platform)+', '+inttostr(ord(summaryStatus))+', '+s+')');
  sp := TSearchProcessor.create(ServerContext);
  try
    sp.strict := strict;
    sp.typekey := typekey;
    sp.type_ := aType;
    sp.compartmentId := compartmentId;
    sp.compartments := compartments;
    sp.baseURL := baseURL;
    sp.lang := lang;
    sp.params := params;
    CreateIndexer;
    sp.indexes := ServerContext.Indexes.Link;
    sp.session := session.link;
    sp.countAllowed := true;
    sp.Connection := FConnection.link;

    sp.build;
    sql := 'Insert into SearchEntries Select '+key+', ResourceKey, MostRecent as ResourceVersionKey, '+sp.sort+', null, null from Ids where Deleted = 0 and '+sp.filter+' order by ResourceKey DESC';
    csql := 'Select count(ResourceKey) from Ids where Deleted = 0 and '+sp.filter;
    link := SEARCH_PARAM_NAME_ID+'='+id+'&'+sp.link_;
    reverse := sp.reverse;
    if (op <> nil) then
      op.issueList.AddAll(sp.Warnings);
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

procedure TFHIRNativeOperationEngine.ProcessMPISearch(typekey : integer; session : TFHIRSession; aType : string; params : TParseMap; baseURL, compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption; strict : boolean; var reverse : boolean);
var
  mpi : TMPISearchProcessor;
  s : String;
begin
  if (session = nil) then
    raise Exception.Create('no session?');
  s := inttostr(Session.Key);
  FConnection.ExecSQL('insert into Searches (SearchKey, Id, Count, Type, Date, Summary, SessionKey) values ('+key+', '''+id+''', 0, 1, '+DBGetDate(FConnection.Owner.Platform)+', '+inttostr(ord(summaryStatus))+', '+s+')');

  mpi := TMPISearchProcessor.create;
  try
    mpi.typekey := typekey;
    mpi.compartmentId := compartmentId;
    mpi.compartments := compartments;
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


procedure TFHIRNativeOperationEngine.processIncludes(session: TFhirSession; secure : boolean; _includes, _reverseIncludes: String; bundle: TFHIRBundle; keys : TKeyList; field: String; comp: TFHIRParserClass);
var
  s, sql : String;
  p : TArray<String>;
  sel : TStringList;
  key2, i: integer;
  type_ : string;
begin
  if ((_includes = '') and (_reverseIncludes = '')) or (keys.count = 0) then
    exit;

  sel := TStringList.Create;
  try
    for s in _includes.Split([';']) do
    begin
      p := s.Split([':']);
      if (length(p) >= 2) and (length(p) <= 3) then
      begin
        key2 := ServerContext.Indexes.GetKeyByName(p[1]);
        if (key2 = 0) then
          raise Exception.Create('Unknown Resource Search Parameter '''+p[1]+'''');
        if (length(p) = 3) then
        begin
          sel.Add('Ids.ResourceKey in (select Target from IndexEntries, Ids where IndexKey = '+inttostr(key2)+' and ResourceKey in ('+keys.forType(p[2])+') and Ids.ResourceKey = IndexEntries.Target and Ids.ResoureTypeKey = '+inttostr(ServerContext.ResConfig[p[2]].key)+')');
        end
        else
          sel.Add('Ids.ResourceKey in (select Target from IndexEntries where IndexKey = '+inttostr(key2)+' and ResourceKey in ('+keys.forType(p[0])+'))');
      end
      else
        raise Exception.Create('Unable to process include '+s);
    end;

    for s in _reverseIncludes.Split([';']) do
    begin
      p := s.Split([':']);
      if (length(p) = 2) then
      begin
        key2 := ServerContext.Indexes.GetKeyByName(p[1]);
        if (key2 = 0) then
          raise Exception.Create('Unknown Resource Parameter '+p[0]);
        sel.Add('Ids.ResourceKey in (select IndexEntries.ResourceKey from IndexEntries, Ids where IndexKey = '+inttostr(key2)+' and Target in ('+keys.forAll+') and Ids.ResourceKey = IndexEntries.ResourceKey and Ids.ResourceTypeKey = '+inttostr(ServerContext.ResConfig[p[0]].key)+')');
      end
      else
        raise Exception.Create('Unable to process include '+s);
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
          AddResourceTobundle(bundle, secure, '', field, comp, SearchEntryModeInclude, false, type_);
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
  bundle : TFHIRBundle;
  id, link, base, sql, field : String;
  i, total, t : Integer;
  key : integer;
  offset, count : integer;
  ok, reverse : boolean;
  summaryStatus : TFHIRSummaryOption;
  title: string;
  keys : TKeyList;
  comp : TFHIRParserClass;
  needsObject : boolean;
  op : TFHIROperationOutcome;
  type_ : String;
  be : TFhirBundleEntry;
begin
  try
    ok := true;
    count := 0;
    offset := 0;
    { todo:
     conformance
     quantity searches
    }
    if not check(response, request.canRead(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      // ok := false
    else if (request.Parameters.getItemCount = 0) and (response.Format = ffXhtml) and (request.compartmentId = '') then
      BuildSearchForm(request, response)
    else
    begin
      TypeNotFound(request, response);
      if request.resourceName <> '' then
      begin
        key := FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+request.ResourceName+'''');
        if not check(response, key > 0, 404, lang, 'Resource Type '+request.ResourceName+' not known', IssueTypeNotSupported) then
            ok := false;
      end
      else
        key := 0;

      if ok then
      begin
        bundle := TFHIRBundle.Create(BundleTypeSearchset);
        op := TFhirOperationOutcome.Create;
        keys := TKeyList.Create;
        try
//          bundle.base := request.baseUrl;
          bundle.meta := TFhirMeta.Create;
          bundle.meta.lastUpdated := NowUTC;

          summaryStatus := request.Summary;
          if FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], request.Session, 1, id, link, sql, title, base, total, summaryStatus, request.strictSearch, reverse) then
            link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
          else
            id := BuildSearchResultSet(key, request.Session, request.resourceName, request.Parameters, request.baseUrl, request.compartments, request.compartmentId, op, link, sql, total, summaryStatus, request.strictSearch, reverse);

          bundle.total := inttostr(total);
          bundle.Tags['sql'] := sql;

          base := AppendForwardSlash(Request.baseUrl)+request.ResourceName+'?';
          if response.Format <> ffUnspecified then
            base := base + '_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
          bundle.link_List.AddRelRef('self', base+link);

          offset := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_OFFSET), 0);
          if request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT) = 'all' then
            count := SUMMARY_SEARCH_PAGE_LIMIT
          else
            count := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT), 0);
          if (count = 0) and request.Parameters.VarExists(SEARCH_PARAM_NAME_COUNT) then
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
              bundle.link_List.AddRelRef('first', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              if offset - count >= 0 then
                bundle.link_List.AddRelRef('previous', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              if offset + count < total then
                bundle.link_List.AddRelRef('next', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              if count < total then
                bundle.link_List.AddRelRef('last', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((total div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
            end;

            chooseField(response.Format, summaryStatus, request.Adaptor, field, comp, needsObject);
            if (not needsObject) and not request.Parameters.VarExists('__wantObject') then // param __wantObject is for internal use only
              comp := nil;

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
                  AddResourceTobundle(bundle, request.secure, request.baseUrl, field, comp, SearchEntryModeMatch, false, type_);
                  keys.Add(TKeyPair.Create(type_, FConnection.ColStringByName['ResourceKey']));
                  inc(t);
                end;
                if (t = count) then
                  break;
              End;
            finally
              FConnection.Terminate;
            end;

            processIncludes(request.session, request.secure, request.Parameters.GetVar('_include'), request.Parameters.GetVar('_revinclude'), bundle, keys, field, comp);
          end;

          bundle.id := FhirGUIDToString(CreateGUID);
          if (op.issueList.Count > 0) then
          begin
            be := bundle.entryList.Append;
            be.resource := op.Link;
            be.search := TFhirBundleEntrySearch.Create;
            be.search.mode := SearchEntryModeOutcome;
          end;

          //bundle.link_List['self'] := request.url;
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.Body := '';
          response.Resource := nil;
          response.bundle := bundle.Link;
        finally
          keys.Free;
          bundle.Free;
          op.Free;
        end;
      end;
    end;
    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message);
      recordStack(e);
      raise;
    end;
  end;
end;


function TFHIRNativeOperationEngine.ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  resourceKey : Integer;
  key, nvid, i : Integer;
  s : String;
  tags : TFHIRTagList;
  ok : boolean;
  needSecure : boolean;
  tnow : TDateTime;
  list : TMatchingResourceList;
begin
  CheckCreateNarrative(request);

  nvid := 0;
  key := 0;
  try
    ok := true;
    if ok and not check(response, request.canWrite(request.ResourceName) or opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      ok := false;

    if ok and (not check(response, request.Resource <> nil, 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang), IssueTypeRequired) or
       not check(response, request.ResourceName = request.resource.fhirType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang), IssueTypeInvalid) or
       not check(response, request.id = request.resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+'('+request.id +' / '+request.resource.id+')', IssueTypeInvalid) or
       not check(response, length(request.id) <= ID_LENGTH, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id]), IssueTypeInvalid)) then
      ok := false;

    ok := checkOkToStore(request, response, needSecure);
    if ok and ServerContext.Validate and not context.upload then
    begin
      if not ExecuteValidation(request, response, 'Update Resource '+request.ResourceName+'/'+request.Id) then
        ok := false
      else
        response.Resource := nil;
    end;


    if request.DefaultSearch then // conditional update
    begin
      list := ResolveSearchId(request.ResourceName, request.compartmentId, request.compartments, request.baseUrl, request.Parameters.Source);
      try
        if (list.Count = 0) then
        begin
          ExecuteCreate(context, request, response, idMaybeNew, 0);
          result := false;
          ok := false;
        end
        else
        if (list.Count > 1) then
        begin
          ok := false;
          check(response, false, 412, lang, GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang), IssueTypeNotFound);
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

    if ok and not check(response, request.Resource.id <> '', 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISSING', lang)+' '+request.id+'/'+request.resource.id+' (3)', IssueTypeRequired) Then
      ok := false;
    if ok and not check(response, request.id = request.Resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.id+'/'+request.resource.id+' (3)', IssueTypeInvalid) Then
      ok := false;


    result := true;

    if ok and not FindResource(request.ResourceName, request.Id, true, resourceKey, request, response, request.compartments) Then
    begin
      ExecuteCreate(context, request, response, idMaybeNew, 0);
      result := false;
      ok := false;
    end;


    if ok and not check(response, not ServerContext.ResConfig[request.ResourceName].versionUpdates or (request.ifMatch <> ''), 412, lang, GetFhirMessage('MSG_VERSION_AWARE', lang), IssueTypeBusinessRule) then
      ok := false;


    if ok then
    begin
      key := FRepository.NextVersionKey;
      nvid := FConnection.CountSQL('Select Max(Cast(VersionId as '+DBIntType(FConnection.Owner.Platform)+')) from Versions where ResourceKey = '+IntToStr(resourceKey)) + 1;

      if  (request.IfMatch <> '') or  ServerContext.ResConfig[request.ResourceName].versionUpdates then
      begin
        s := request.IfMatch;

        if not check(response, s = inttostr(nvid-1), 412, lang, StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), [inttostr(nvid-1), s]), IssueTypeConflict) then
          ok := false;
      end;
    end;

    if ok then
    begin
      tags := TFHIRTagList.create;
      try
        if request.resource.meta = nil then
          request.resource.meta := TFhirMeta.Create;
        tags.ReadTags(request.resource.meta);
        LoadTags(tags, ResourceKey);
        if (request.hasTestingTag) then
          tags.forceTestingTag;
        tags.writeTags(request.resource.meta);
        if checkOkToStore(request, response, needSecure) then
        begin
          request.Resource.meta.lastUpdated := NowUTC;
          request.Resource.meta.versionId := inttostr(nvid);
          CheckNotRedacted(request.Resource.meta, 'Updating Resource');
          updateProvenance(request.Provenance, request.ResourceName, request.Id, inttostr(nvid));
          tnow := request.Resource.meta.lastUpdated.AsUTCDateTime;


          checkProposedContent(request, request.Resource, tags);

          for i := 0 to tags.count - 1 do
            FRepository.RegisterTag(tags[i], FConnection);

          FConnection.execSQL('Update IndexEntries set Flag = 2 where ResourceKey = '+IntToStr(resourceKey));

          // check whether originalId matches?
          // to do: merging

          FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, TransactionDate, StatedDate, Format, VersionId, Status, '+
            'SessionKey, ForTesting, Secure, Tags, XmlContent, XmlSummary, JsonContent, JsonSummary) values (:k, :rk, :td, :sd, :f, :v, 1, :s, :ft, :sec, :tb, :xc, :xs, :jc, :js)';
          FConnection.prepare;
          try
            FConnection.BindInteger('k', key);
            FConnection.BindInteger('rk', resourceKey);
            FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
            FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
            FConnection.BindString('v', inttostr(nvid));
            FConnection.BindIntegerFromBoolean('sec', needSecure);
            request.SubId := inttostr(nvid);
            if request.Session = nil then
              FConnection.BindNull('s')
            else
              FConnection.BindInteger('s', request.Session.Key);
            FConnection.BindInteger('f', 2);
            FConnection.BindIntegerFromBoolean('ft', tags.hasTestingTag);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('xc', EncodeResource(request.Resource, true, soFull));
            FConnection.BindBlobFromBytes('jc', EncodeResource(request.Resource, false, soFull));
            markRedacted(request.Resource.meta);
            FConnection.BindBlobFromBytes('xs', EncodeResource(request.Resource, true, soSummary));
            FConnection.BindBlobFromBytes('js', EncodeResource(request.Resource, false, soSummary));
            unmarkRedacted(request.Resource.meta);
            FConnection.Execute;
          finally
            FConnection.Terminate;
          end;
          FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 0 where ResourceKey = '+inttostr(resourceKey));
          CommitTags(tags, key);
          CreateIndexer;
          FIndexer.execute(resourceKey, request.id, request.resource, tags);
          FRepository.SeeResource(resourceKey, key, request.id, needSecure, false, request.Resource, FConnection, false, request.Session);

          if (response.Resource <> nil) and (response.Resource is TFhirBundle)  then
            response.bundle.entryList.add(request.Resource.Link)
          else
            response.Resource := request.Resource.Link;
          response.versionId := inttostr(nvid);
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.lastModifiedDate := tnow;
          response.Location := request.baseUrl+request.ResourceName+'/'+request.Id+'/_history/'+inttostr(nvid);
          if request.Provenance <> nil then
            SaveProvenance(request.Session, request.Provenance);
        end;
      finally
        tags.free;
      end;
    end;
    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, inttostr(nvid), key, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, inttostr(nvid), 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFHIRNativeOperationEngine.ExecutePatch(request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  resourceKey : Integer;
  key, nvid, i : Integer;
  s : String;
  tags : TFHIRTagList;
  ok : boolean;
  needSecure : boolean;
  tnow : TDateTime;
  list : TMatchingResourceList;
  parser : TFHIRParser;
  json, json2 : TJsonObject;
  xml : IXMLDOMDocument2;
  ms : TAdvMemoryStream;
begin
  result := false;
  json := nil;
  nvid := 0;
  key := 0;
  try
    ok := true;
    if ok and not check(response, request.canWrite(request.ResourceName) or opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      ok := false;

    if ok and (not check(response, (request.patchJson <> nil) or (request.patchXml <> nil), 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang), IssueTypeRequired) or
       not check(response, length(request.id) <= ID_LENGTH, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id]), IssueTypeInvalid)) then
      ok := false;

    if ok and request.DefaultSearch then // conditional update
    begin
      list := ResolveSearchId(request.ResourceName, request.compartmentId, request.compartments, request.baseUrl, request.Parameters.Source);
      try
        if check(response, list.count = 1, 412, lang, GetFhirMessage('UPDATE_NOT_ONE_MATCH', lang), IssueTypeNotFound) then
          request.Id := list[0].name
        else
          ok := false;
      finally
        list.Free;
      end;
    end;

    if ok then
      if not FindResource(request.ResourceName, request.Id, true, resourceKey, request, response, request.compartments) Then
        ok := false;

    if ok and not check(response, not ServerContext.ResConfig[request.ResourceName].versionUpdates or (request.ifMatch <> ''), 412, lang, GetFhirMessage('MSG_VERSION_AWARE', lang), IssueTypeBusinessRule) then
      ok := false;


    if ok then
    begin
      key := FRepository.NextVersionKey;
      nvid := FConnection.CountSQL('Select Max(Cast(VersionId as '+DBIntType(FConnection.Owner.Platform)+')) from Versions where ResourceKey = '+IntToStr(resourceKey)) + 1;

      if  (request.IfMatch <> '') or  ServerContext.ResConfig[request.ResourceName].versionUpdates then
      begin
        s := request.IfMatch;

        if not check(response, s = inttostr(nvid-1), 412, lang, StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), [inttostr(nvid-1), s]), IssueTypeConflict) then
          ok := false;
      end;
    end;

    // ok, now time to actually get the resource, so we can patch it
    if (request.patchJson <> nil) then
      FConnection.SQL := 'Select Secure, StatedDate, VersionId, JsonContent from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc'
    else
      FConnection.SQL := 'Select Secure, StatedDate, VersionId, XmlContent from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if not check(response, FConnection.FetchNext, 500, lang, 'Not Found internally', IssueTypeNotFound) then
        ok := false
      else if (request.patchJson <> nil) then
        json := TJSONParser.Parse(FConnection.ColBlobByName['JsonContent'])
      else
        xml := TMsXmlParser.Parse(FConnection.ColBlobByName['XmlContent'])
    finally
      FConnection.Terminate;
    end;

    if ok then
    begin
      try
        if (request.patchJson <> nil) then
        begin
          json2 := TJsonPatchEngine.applyPatch(json, request.patchJson);
          try
            parser := TFHIRJsonParser.Create(request.Context.link, request.lang);
            try
              TFHIRJsonParser(parser).parse(json2);
              request.Resource := parser.resource.Link;
              request.PostFormat := ffJson;
              ms := TAdvMemoryStream.Create;
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
        else
        begin
          TXmlPatchEngine.execute(xml, xml, request.patchXml);
          request.Source.AsUnicode := xml.xml;
          request.PostFormat := ffXml;
          parser := TFHIRXmlParser.Create(request.Context.link, request.lang);
          try
            TFHIRXmlParser(parser).element := xml.documentElement;
            parser.parse();
            request.Resource := parser.resource.Link;
          finally
            parser.Free;
          end;
        end;
      finally
        json.Free;
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

      if ok and not check(response, request.resource.id <> '', 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISSING', lang)+' '+request.id+'/'+request.resource.id+' (3)', IssueTypeRequired) Then
        ok := false;
      if ok and not check(response, request.ResourceName = request.resource.fhirType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang), IssueTypeInvalid) then
        ok := false;
      if ok and not check(response, request.id = request.resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.id+'/'+request.resource.id+' (3)', IssueTypeInvalid) Then
        ok := false;

      if ok then
      begin
        result := true;
        tags := TFHIRTagList.create;
        try
          if request.resource.meta = nil then
            request.resource.meta := TFhirMeta.Create;
          tags.ReadTags(request.resource.meta);
          LoadTags(tags, ResourceKey);
          tags.writeTags(request.resource.meta);
          // don't need to recheck the approval on the tags, they must already have been loaded before the earlier check
          request.resource.meta.lastUpdated := NowUTC;
          request.resource.meta.versionId := inttostr(nvid);
          CheckNotRedacted(request.resource.meta, 'Patching Resource');
          updateProvenance(request.Provenance, request.ResourceName, request.Id, inttostr(nvid));
          tnow := request.resource.meta.lastUpdated.AsUTCDateTime;

          checkProposedContent(request, request.resource, tags);
          for i := 0 to tags.count - 1 do
            FRepository.RegisterTag(tags[i], FConnection);
          FConnection.execSQL('Update IndexEntries set Flag = 2 where ResourceKey = '+IntToStr(resourceKey));

          FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, TransactionDate, StatedDate, Format, VersionId, Status, '+
            'SessionKey, Secure, Tags, XmlContent, XmlSummary, JsonContent, JsonSummary) values (:k, :rk, :td, :sd, :f, :v, 1, :s, :sec, :tb, :xc, :xs, :jc, :js)';
          FConnection.prepare;
          try
            FConnection.BindInteger('k', key);
            FConnection.BindInteger('rk', resourceKey);
            FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
            FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
            FConnection.BindString('v', inttostr(nvid));
            FConnection.BindIntegerFromBoolean('sec', needSecure);
            request.SubId := inttostr(nvid);
            if request.Session = nil then
              FConnection.BindNull('s')
            else
              FConnection.BindInteger('s', request.Session.Key);
            FConnection.BindInteger('f', 2);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('xc', EncodeResource(request.resource, true, soFull));
            FConnection.BindBlobFromBytes('jc', EncodeResource(request.resource, false, soFull));
            markRedacted(request.resource.meta);
            FConnection.BindBlobFromBytes('xs', EncodeResource(request.resource, true, soSummary));
            FConnection.BindBlobFromBytes('js', EncodeResource(request.resource, false, soSummary));
            unmarkRedacted(request.resource.meta);
            FConnection.Execute;
          finally
            FConnection.Terminate;
          end;
          FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 0 where ResourceKey = '+inttostr(resourceKey));
          CommitTags(tags, key);
          CreateIndexer;
          FIndexer.execute(resourceKey, request.id, request.resource, tags);
          FRepository.SeeResource(resourceKey, key, request.id, needSecure, false, request.resource, FConnection, false, request.Session);

          if (response.Resource <> nil) and (response.Resource is TFhirBundle)  then
            response.bundle.entryList.add(request.resource.Link)
          else
            response.Resource := request.resource.Link;
          response.versionId := inttostr(nvid);

        finally
          tags.free;
        end;
        response.HTTPCode := 200;
        response.Message := 'OK';
        response.lastModifiedDate := tnow;
        response.Location := request.baseUrl+request.ResourceName+'/'+request.Id+'/_history/'+inttostr(nvid);
        if request.Provenance <> nil then
          SaveProvenance(request.Session, request.Provenance);
      end;
    end;
    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, inttostr(nvid), key, request.CommandType, request.Provenance, response.httpCode, '', response.message)
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, inttostr(nvid), 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;


function TFHIRNativeOperationEngine.ExecuteValidation(request: TFHIRRequest; response: TFHIRResponse; opDesc : String) : boolean;
var
  i : integer;
  ctxt : TFHIRValidatorContext;
begin
  try
    if opDesc = '' then
      opDesc := 'Validate resource '+request.id;

    if request.resourceName = 'Binary' then
    begin
      response.outcome := TFhirOperationOutcome.create;
    end
    else if (request.Source <> nil) and (request.postFOrmat <> ffText) then
    begin
      ctxt := TFHIRValidatorContext.Create;
      try
        ctxt.IsAnyExtensionsAllowed := true;
        ctxt.ResourceIdRule := risOptional;
        ctxt.OperationDescription := opDesc;
        ServerContext.Validator.validate(ctxt, request.Source, request.PostFormat, nil);
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
        ServerContext.Validator.validate(ctxt, request.Resource, nil);
        response.outcome := ServerContext.Validator.describe(ctxt);
      finally
        ctxt.Free;
      end;
    end;

    result := true;
    for i := 0 to response.outcome.issueList.count - 1 do
      result := result and (response.outcome.issueList[i].severity in [IssueSeverityInformation, IssueSeverityWarning]);
    if result then
      response.HTTPCode := 200
    else
    begin
      response.HTTPCode := 400;
      response.Resource := response.outcome.Link;
    end;
    if request.ResourceEnum <> frtAuditEvent then
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceEnum <> frtAuditEvent then
        AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteVersionRead(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : integer;
  field : String;
  comp : TFHIRParserClass;
  needsObject : boolean;
begin
  try
    NotFound(request, response);
    if check(response, request.canRead(request.ResourceName) and opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (length(request.id) <= ID_LENGTH) and (length(request.subid) <= ID_LENGTH) and FindResource(request.ResourceName, request.Id, true, resourceKey, request, response, request.compartments) then
      begin
        VersionNotFound(request, response);
        chooseField(response.Format, request.Summary, request.Adaptor, field, comp, needsObject);

        FConnection.SQL := 'Select Secure, VersionId, StatedDate, '+field+' from Versions where ResourceKey = '+inttostr(resourceKey)+' and VersionId = :v';
        FConnection.Prepare;
        try
          FConnection.BindString('v', request.SubId);
          FConnection.Execute;
          if FConnection.FetchNext then
          Begin
            if (FConnection.ColIntegerByName['Secure'] = 1) and not request.secure then
              check(response, false, 403, lang, 'This resource is labelled with a security tag that means this server will only send it if the connection is secure', IssueTypeSuppressed)
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
              processBlob(request, response, field, comp);
            end;
          end;
        finally
          FConnection.Terminate;
        end;
      end;
    end;
    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.SubId, 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.SubId, 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFHIRNativeOperationEngine.FindResource(aType : String; sId: String; bAllowDeleted : boolean; var resourceKey: integer; request: TFHIRRequest; response : TFhirResponse; compartments : String): boolean;
var
  cmp : String;
begin
  if (compartments <> '') then
    cmp := ' and Ids.ResourceKey in (select ResourceKey from Compartments where TypeKey = '+inttostr(ServerContext.ResConfig['Patient'].key)+' and Id in ('+compartments+'))'
  else
    cmp := '';

  FConnection.sql := 'select Ids.ResourceKey, Deleted from Ids, Types where Ids.id = :s '+cmp+' and Ids.ResourceTypeKey = Types.ResourceTypeKey and Supported = 1 and ResourceName = :n';
  FConnection.Prepare;
  FConnection.BindString('s', sId);
  FConnection.BindString('n', aType);
  FConnection.execute;
  result := FConnection.FetchNext;
  if result then
  begin
    if bAllowDeleted or (FConnection.ColIntegerByName['Deleted'] = 0) then
    begin
      resourceKey := FConnection.ColIntegerByName['ResourceKey'];
    end
    else
    begin
      if response <> nil then
        check(response, false, 410, lang, StringFormat(GetFhirMessage('MSG_DELETED_ID', lang), [request.id]), IssueTypeNotFound);
      result := false;
    end;
  end
  else if response <> nil then
    check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.ResourceName+'/'+request.id]), IssueTypeNotFound);
  FConnection.terminate;
end;

procedure TFHIRNativeOperationEngine.AddCDSHooks(conf: TFhirCapabilityStatementRest);
var
  ext : TFhirExtension;
begin
  ext := conf.addExtension('http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity');
  ext.addExtension('name', 'Fetch Patient Alerts');
  ext.addExtension('activity', TCDSHooks.patientView);
  ext.addExtension('preFetchOptional', 'Patient/{{Patient.id}}');

  ext := conf.addExtension('http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity');
  ext.addExtension('name', 'Get Terminology Information');
  ext.addExtension('activity', TCDSHooks.codeView);

  ext := conf.addExtension('http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity');
  ext.addExtension('name', 'Get identifier Information');
  ext.addExtension('activity', TCDSHooks.identifierView);
end;

function TFHIRNativeOperationEngine.AddNewResourceId(aType, id, lang : string; forTesting : boolean; var resourceKey : integer) : Boolean;
var
  itype : integer;
  exists : boolean;
begin
  iType := ServerContext.ResConfig[aType].key;// FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[aType]+'''');
  result := iType > 0;
  if result then
  begin
    resourceKey := FRepository.NextResourceKeySetId(aType, id);

    FConnection.SQL := 'Select ResourceKey from Ids where ResourceTypeKey = :r and Id = :i';
    FConnection.Prepare;
    FConnection.BindInteger('r', itype);
    FConnection.BindString('i', id);
    FConnection.Execute;
    exists := FConnection.fetchNext;
    FConnection.Terminate;

    if exists then
      raise ERestfulException.create('TFHIRNativeOperationEngine', 'AddNewResourceId', StringFormat(GetFhirMessage('MSG_DUPLICATE_ID', lang), [id, aType]), 404, IssueTypeDuplicate);

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
      key := FRepository.NextResourceKeySetId(aType, id);
    end
    else
    begin
      key := FRepository.NextResourceKeyGetId(aType, id);
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

procedure TFHIRNativeOperationEngine.NoMatch(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_MATCH', lang), [request.ResourceName+'?'+request.parameters.source]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

procedure TFHIRNativeOperationEngine.NotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.ResourceName+':'+request.Id]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

procedure TFHIRNativeOperationEngine.VersionNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.ResourceName+':'+request.Id+'/_history/'+request.subId]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

function TFHIRNativeOperationEngine.GetPatientId(): String;
begin

end;

procedure TFHIRNativeOperationEngine.TypeNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_UNKNOWN_TYPE', lang), [request.ResourceName]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;


procedure TFHIRNativeOperationEngine.checkNotRedacted(meta: TFhirMeta; msg: String);
begin
  if meta.HasTag('http://hl7.org/fhir/v3/ObservationValue', 'REDACTED') then
    raise Exception.Create('Error '+msg+': This resource has been redacted, and cannot be used as the basis for this operation');
end;

function TFHIRNativeOperationEngine.hasActCodeSecurityLabel(res : TFHIRResource; codes : array of string) : boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if (res <> nil) and (res.meta <> nil) then
    for c in res.meta.securityList do
      if (c.system = 'http://hl7.org/fhir/v3/ActCode') and StringArrayExistsSensitive(codes, c.code) then
        exit(true);
end;

function TFHIRNativeOperationEngine.hasConfidentialitySecurityLabel(res: TFHIRResource; codes: array of string): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if (res <> nil) and (res.meta <> nil) then
    for c in res.meta.securityList do
      if (c.system = 'http://hl7.org/fhir/v3/Confidentiality') and StringArrayExistsSensitive(codes, c.code) then
        exit(true);
end;

function TFHIRNativeOperationEngine.isOkToDeleteSecurityLabel(request: TFHIRRequest; response: TFHIRResponse; c: TFHIRCoding): boolean;
begin
  if c = nil then
    exit(true);

  result := check(response, request.secure or (c.system <> 'http://hl7.org/fhir/v3/ActCode') or not
    StringArrayExistsSensitive(['CPLYCD', 'CPLYJPP', 'CPLYOPP', 'CPLYOSP', 'CPLYPOL', 'ENCRYPT', 'ENCRYPTR', 'ENCRYPTT', 'ENCRYPTU',
                'ETH', 'GDIS', 'HIV', 'PSY', 'SCA', 'SDV', 'SEX', 'STD', 'TBOO', 'SICKLE', 'DEMO', 'DOB', 'GENDER', 'LIVARG', 'MARST', 'RACE', 'REL',
                'B', 'EMPL', 'LOCIS', 'SSP', 'ADOL', 'CEL', 'DIA', 'DRGIS', 'EMP', 'PDS', 'PRS'], c.code),
        403, lang, 'This security label can only be deleted on a secure connection', IssueTypeSuppressed);
  if result then
    result := check(response, request.secure or (c.system <> 'http://hl7.org/fhir/v3/Confidentiality') or not
            StringArrayExistsSensitive(['L', 'M', 'N', 'R', 'U', 'V', 'B', 'D', 'I', 'ETH', 'HIV', 'PSY', 'SDV', 'C', 'S', 'T'], c.code),
        403, lang, 'This security label can only be deleted on a secure connection', IssueTypeSuppressed);
end;

function TFHIRNativeOperationEngine.Link: TFHIRNativeOperationEngine;
begin
  result := TFHIRNativeOperationEngine(inherited Link);
end;

function TFHIRNativeOperationEngine.loadCustomResources(response : TFHIRResponse; key: integer; startup : boolean; names : TStringList) : boolean;
var
  ig : TFHIRImplementationGuide;
  package : TFhirImplementationGuidePackage;
  needsSecure : boolean;
  crs : TAdvList<TFHIRCustomResourceInformation>;
  cr : TFHIRCustomResourceInformation;
  sp : TFhirSearchParameter;
begin
  result := false;
  crs := TAdvList<TFHIRCustomResourceInformation>.create;
  try
    ig := GetResourceByKey(key, needsSecure) as TFHIRImplementationGuide;
    try
      for package in ig.packageList do
        crs.add(loadCustomResource(ig, package));
      for cr in crs do
      begin
        names.add(cr.Name);
        result := check(response, not ServerContext.ValidatorContext.hasCustomResource(cr.Name), 400, lang, 'Custom resource is already defined', IssueTypeBusinessRule) and
                  check(response, startup or CustomResourceNameIsOk(cr.Name), 400, lang, 'Custom resource is already defined', IssueTypeBusinessRule);
      end;
      if result then
        for cr in crs do
        begin
          for sp in cr.SearchParameters do
            ServerContext.Indexes.Indexes.add(cr.Name, sp);
          ServerContext.ValidatorContext.registerCustomResource(cr);
        end;
    finally
      ig.Free;
    end;
  finally
    crs.free;
  end;
end;

function TFHIRNativeOperationEngine.loadResources(keys: TList<integer>): TAdvList<TFHIRResource>;
var
  parser : TFHIRParser;
  s : TBytes;
  sb : TAdvStringBuilder;
  k : integer;
begin
  result := TAdvList<TFHIRResource>.create;
  try
    sb := TAdvStringBuilder.create;
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
          parser := MakeParser(ServerContext.Validator.Context, lang, ffJson, s, xppDrop);
          try
            result.Add(parser.resource.Link);
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

function TFHIRNativeOperationEngine.loadCustomResources(response : TFHIRResponse; id: String; startup : boolean; names : TStringList) : boolean;
var
  ig : TFHIRImplementationGuide;
  package : TFhirImplementationGuidePackage;
  needsSecure : boolean;
  crs : TAdvList<TFHIRCustomResourceInformation>;
  cr : TFHIRCustomResourceInformation;
  sp : TFhirSearchParameter;
begin
  crs := TAdvList<TFHIRCustomResourceInformation>.create;
  try
    ig := GetResourceById(nil, 'ImplementationGuide', id, '', needsSecure) as TFHIRImplementationGuide;
    try
      for package in ig.packageList do
        crs.add(loadCustomResource(ig, package));
      result := true;
      for cr in crs do
      begin
        names.Add(cr.Name);
        result := result and check(response, not ServerContext.ValidatorContext.hasCustomResource(cr.Name), 400, lang, 'Custom resource is already defined', IssueTypeBusinessRule) and
                  check(response, startup or CustomResourceNameIsOk(cr.Name), 400, lang, 'Custom resource is already defined', IssueTypeBusinessRule);
      end;
      if result then
        for cr in crs do
        begin
          for sp in cr.SearchParameters do
            ServerContext.Indexes.Indexes.add(cr.Name, sp);
          ServerContext.ValidatorContext.registerCustomResource(cr);
        end;
    finally
      ig.Free;
    end;
  finally
    crs.free;
  end;
end;

function TFHIRNativeOperationEngine.loadCustomResource(ig : TFHIRImplementationGuide; package : TFhirImplementationGuidePackage) : TFHIRCustomResourceInformation;
var
  rd : TFhirImplementationGuidePackageResource;
  res : TFHIRResource;
  sd : TFhirStructureDefinition;
  list : TAdvList<TFhirSearchParameter>;
  cr : TFHIRCustomResourceInformation;
  needsSecure : boolean;
begin
  sd := nil;
  list := TAdvList<TFhirSearchParameter>.create;
  try
    for rd in package.resourceList do
    begin
      if rd.source = nil then
        res := nil
      else if rd.source is TFhirUri then
        res := getResourceByUrl(frtNull, (rd.source as TFhirUri).value, '', true, needsSecure)
      else
        res := getResourceByReference(ig, (rd.source as TFhirReference).reference, '', true, needsSecure);

      if res <> nil then // for now, we just ignore them...
      begin
        if res is TFHIRStructureDefinition then
          if sd <> nil then
            raise Exception.Create('Multiple structure definitions in a package; cannot load as a custom resource')
          else
            sd := TFHIRStructureDefinition(res);
        if res is TFhirSearchParameter then
          list.Add(TFhirSearchParameter(res).Link);
      end;
    end;
    if sd = nil then
      raise Exception.Create('No structure definitions in a package; cannot load as a custom resource');
    cr := TFHIRCustomResourceInformation.Create(sd.Link);
    try
      cr.SearchParameters.AddAll(list);
      result := cr.Link;
    finally
      cr.Free;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRNativeOperationEngine.checkOkToStore(request: TFHIRRequest; response: TFHIRResponse; var secure : boolean): boolean;
begin
  // check the security labels
  secure := false;
  result := check(response,
      not hasActCodeSecurityLabel(request.Resource, ['CPLYCD', 'CPLYJPP', 'CPLYOPP', 'CPLYOSP', 'CPLYPOL']),
        403, lang, 'This resource is labelled with a security tag that requires compliance with a policy, but no policy is known', IssueTypeNotSupported);
  if result then
    if hasActCodeSecurityLabel(request.Resource, ['ENCRYPT', 'ENCRYPTR', 'ENCRYPTT', 'ENCRYPTU']) then
    begin
      secure := true;
      result := check(response, request.secure, 403, lang, 'This resource is labelled with a security tag that requires encryption, but encryption was not used', IssueTypeBusinessRule);
    end;
  // extra conditions
  if result and not secure then
    secure :=
      hasActCodeSecurityLabel(request.Resource, ['ETH', 'GDIS', 'HIV', 'PSY', 'SCA', 'SDV', 'SEX', 'STD', 'TBOO', 'SICKLE', 'DEMO', 'DOB', 'GENDER', 'LIVARG',
                  'MARST', 'RACE', 'REL', 'B', 'EMPL', 'LOCIS', 'SSP', 'ADOL', 'CEL', 'DIA', 'DRGIS', 'EMP', 'PDS', 'PRS']) or
      hasConfidentialitySecurityLabel(request.Resource, ['R', 'U', 'V', 'B', 'D', 'I', 'ETH', 'HIV', 'PSY', 'SDV', 'C', 'S', 'T']);
end;

procedure TFHIRNativeOperationEngine.markRedacted(meta: TFhirMeta);
begin
  meta.addTag('http://hl7.org/fhir/v3/ObservationValue', 'REDACTED', 'redacted');
end;

procedure TFHIRNativeOperationEngine.unmarkRedacted(meta: TFhirMeta);
begin
  meta.removeTag('http://hl7.org/fhir/v3/ObservationValue', 'REDACTED');
end;

procedure TFHIRNativeOperationEngine.updateProvenance(prv: TFHIRProvenance; rtype, id, vid: String);
begin
  if prv <> nil then
  begin
    prv.targetList.Clear;
    prv.targetList.Append.reference := rtype+'/'+id+'/_history/'+vid;

    prv.signatureList.Clear;

    // todo: check this....
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

procedure TFHIRNativeOperationEngine.BuildSearchForm(request: TFHIRRequest; response : TFHIRResponse);
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
'    <title>FHIR RESTful Server - FHIR v'+FHIR_GENERATED_VERSION+'</title>'#13#10+
TFHIRXhtmlComposer.PageLinks+
FHIR_JS+
'</head>'#13#10+
''#13#10+
'<body>'#13#10+
''#13#10+
TFHIRXhtmlComposer.Header(request.Session, request.baseUrl, request.lang, SERVER_VERSION)+
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
    for i := 0 to ServerContext.Indexes.Indexes.Count - 1 Do
    begin
      ix := ServerContext.Indexes.Indexes[i];
      if (ix.ResourceType = request.ResourceName) and (length(ix.TargetTypes) = 0) then
      begin
        desc := FormatTextToHTML(GetFhirMessage('ndx-'+request.ResourceName+'-'+ix.name, lang, ix.Description));
        if ix.SearchType = SearchParamTypeDate then
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

    for i := 0 to ServerContext.Indexes.Indexes.Count - 1 Do
    begin
      ix := ServerContext.Indexes.Indexes[i];
      if (ix.ResourceType = request.ResourceName) and (length(ix.TargetTypes) > 0) then
      begin
        pfx := ix.Name;
        types := ix.TargetTypes;
        s := s +'<tr><td colspan="4"><b>'+ix.Name+'</b> ('+describeResourceTypes(types)+')<b>:</b></td></tr>'+#13#10;
        m := TStringList.create;
        try
          for j := 0 to ServerContext.Indexes.Indexes.Count - 1 Do
          begin
            ix2 := ServerContext.Indexes.Indexes[j];
            ok := false;
            for rn in types do
              ok := ok or (ix2.ResourceType = rn);
            if (ok) and (m.IndexOf(ix2.Name) = -1) then
            begin
              desc := FormatTextToHTML(GetFhirMessage('ndx-'+request.ResourceName+'-'+ix2.name, lang, ix2.Description));
              if (ix2.searchType = SearchParamTypeDate) then
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
  for i := 0 to ServerContext.Indexes.Indexes.Count - 1 Do
  begin
    ix := ServerContext.Indexes.Indexes[i];
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
TFHIRXhtmlComposer.Footer(request.baseUrl, lang);
  response.Body := s;
end;


procedure TFHIRNativeOperationEngine.CreateIndexer;
begin
  if FIndexer = nil then
  begin
    FSpaces := TFHIRIndexSpaces.Create(FConnection);
    FIndexer := TFHIRIndexManager.Create(FSpaces.Link as TFhirIndexSpaces, ServerContext.Indexes.Link, ServerContext.ValidatorContext.Link, ServerContext.ResConfig.Link);
    FIndexer.TerminologyServer := ServerContext.TerminologyServer.Link;
    FIndexer.Bases := ServerContext.Bases;
    FIndexer.KeyEvent := FRepository.GetNextKey;
  end;
end;

procedure TFHIRNativeOperationEngine.SetConnection(const Value: TKDBConnection);
begin
  FConnection := Value;
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
    if not check(response, opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      ok := false;

    if ok and not check(response, request.canWrite(request.ResourceName) and (request.Resource <> nil), 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang), IssueTypeRequired) then
      ok := false;
    if ok and (request.Resource <> nil) then
      if not check(response, request.ResourceName = request.resource.fhirType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang), IssueTypeInvalid) then
        ok := false;
    // todo: check version id integrity
    // todo: check version integrity

    if not ok then
      // do nothing
    else if request.resource is TFhirBundle then
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
    '    <title>'+StringFormat(GetFhirMessage('UPLOAD_DONE', lang), [CODES_TFHIRResourceType[request.resource.ResourceType]])+'</title>'#13#10+
    '    <link rel="Stylesheet" href="/css/fhir.css" type="text/css" media="screen" />'#13#10+
    FHIR_JS+
    '</head>'#13#10+
    ''#13#10+
    '<body>'#13#10+
    ''#13#10+
    '<div class="header">'#13#10+
    '  <a href="http://www.hl7.org/fhir" title="'+GetFhirMessage('MSG_HOME_PAGE_TITLE', lang)+'"><img src="/img/flame16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>'#13#10+
    ''#13#10+
    '  &copy; HL7.org 2011-2013'#13#10+
    '  &nbsp;'#13#10+
    '  '+ServerContext.OwnerName+' FHIR '+GetFhirMessage('NAME_IMPLEMENTATION', lang)+#13#10+
    '  &nbsp;'#13#10+
    '  '+GetFhirMessage('NAME_VERSION', lang)+' '+FHIR_GENERATED_VERSION+#13#10;

    if request.session <> nil then
      response.Body := response.Body +'&nbsp;&nbsp;'+FormatTextToXml(request.Session.Name);

    response.Body := response.Body +
    '  &nbsp;<a href="'+request.baseUrl+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a>'#13#10+
    '</div>'#13#10+
    ''#13#10+
    '<div id="div-cnt" class="content">'#13#10+
    '<h2>'+StringFormat(GetFhirMessage('UPLOAD_DONE', lang), [CODES_TFHIRResourceType[request.resource.ResourceType]])+'</h2>'#13#10+
    '<p>'+s+'</p>'#13#10+
    ''#13#10+
    '<p><br/><a href="'+request.baseUrl+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a></p>'+
    '</div>'#13#10+
    '</body>'#13#10+
    '</html>'#13#10+
    ''#13#10;
      response.ContentType:= 'text/html';
    end;
    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

Function IsTypeAndId(s : String; var id : String):Boolean;
var
  l, r : String;
begin
  StringSplit(s, '/', l, r);
  id := r;
  if (l <> '') and (l[1] = '/') then
    delete(l, 1, 1);
  result := (StringArrayIndexOfSensitive(CODES_TFHIRResourceType, l) > -1) and IsId(r);
end;

function TFHIRNativeOperationEngine.ResolveSearchId(resourceName, compartmentId, compartments : String; baseURL, params : String) : TMatchingResourceList;
var
  sp : TSearchProcessor;
  p : TParseMap;
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
  p := TParseMap.create(params);
  result := TMatchingResourceList.Create;
  try
    sp := TSearchProcessor.create(ServerContext);
    try
      sp.typekey := key;
      sp.type_ := resourceName;
      sp.compartmentId := compartmentId;
      sp.compartments := compartments;
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

//function TFHIRNativeOperationEngine.ResolveSearchIdCount(atype: TFHIRResourceType; compartmentId, compartments, baseURL, params: String): integer;
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
procedure TFHIRNativeOperationEngine.SaveProvenance(session: TFhirSession; prv: TFHIRProvenance);
begin
  prv.id := '';

  FRepository.QueueResource(prv);
end;

function TFHIRNativeOperationEngine.scanId(request : TFHIRRequest; entry : TFHIRBundleEntry; ids : TFHIRTransactionEntryList; index : integer) : TFHIRTransactionEntry;
var
  id : TFHIRTransactionEntry;
  k : integer;
  sId, s, b : String;
  sParts : TArray<String>;
  baseok : boolean;
  aType : String;
  list : TMatchingResourceList;
begin
  if entry.resource <> nil then
    aType := entry.resource.fhirType
  else if (entry.request = nil) or not (entry.request.method in [HttpVerbDELETE, HttpVerbGET]) or (entry.request.url = '') then // must be deleting something
    raise Exception.Create('A resource must be provided unless the entry has a transaction with method = delete and a url (entry '+inttostr(index+1)+')')
  else
  begin
    s := entry.request.url;
    if s.contains('?') then
      s := s.subString(0, s.indexOf('?'));
    sParts := s.Split(['/']);
    aType := sParts[0];
    if not StringArrayExistsSensitive(CODES_TFhirResourceType, atype) and Not ServerContext.ValidatorContext.hasCustomResource(aType) then
      raise Exception.Create('Unknown resource type '+sParts[0]+' in deletion URL');
  end;

  if (entry.request <> nil) and (entry.request.method = HttpVerbDELETE) and not ServerContext.ResConfig[aType].cmdDelete then
    Raise Exception.create('Deleting Resource '+CODES_TFHIRResourceType[entry.resource.ResourceType]+' is not supported in Transactions (entry '+inttostr(index+1)+')');


  if not ServerContext.ResConfig[aType].cmdUpdate and not ServerContext.ResConfig[aType].cmdCreate then
    Raise Exception.create('Resource '+CODES_TFHIRResourceType[entry.resource.ResourceType]+' is not supported in Transactions (entry '+inttostr(index+1)+')');

  if (entry.resource <> nil) and  (entry.resource.id.Contains('[x]')) then
    raise Exception.Create('not handled - error in transaction (entry '+CODES_TFHIRResourceType[entry.resource.ResourceType]+'/'+entry.resource.id+' @ entry '+inttostr(index+1)+')');
    // this was a work around an for an error in the build tool - not needed any more
//    entry.resource.id := entry.resource.id.replace('[x]',CODES_TFHIRResourceType[entry.resource.ResourceType]);

  id := TFHIRTransactionEntry.create;
  try
    id.ResType := aType;

    if (entry.resource <> nil) then
      id.Name := fullResourceUri(request.baseUrl, entry.resource.ResourceType, entry.resource.id)
    else if entry.request <> nil then
      id.Name := fullResourceUri(request.baseUrl, entry.request.url)
    else
      id.Name := '??';
    id.outcomeVersion := 1;

    // figure out what kind of operation is involved here
    if entry.request <> nil then
    begin
      // does base matter?
      case entry.request.method of
        HttpVerbGET :
          begin
            id.state := tesRead; // at this point, this is a null operation;
          end;
        HttpVerbPOST :
          begin
            id.state := tesCreate;
            if entry.request.ifNoneExist <> '' then
            begin
              s := entry.request.IfNoneExist;
              if (s.Contains('?')) then
                s := s.Substring(s.IndexOf('?')+1);
              list := ResolveSearchId(aType, request.compartmentId, request.compartments, request.baseUrl, s);
              try
                if list.Count = 1 then
                begin
                  id.state := tesIgnore;
                  id.id := list[0].Name;
                  id.outcomeVersion := list[0].version+1;
                end
                else if list.Count > 1 then
                  raise Exception.Create(GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang)+' (entry '+inttostr(index+1)+')');
              finally
                list.Free;
              end;
            end;

            if id.state = tesCreate then
            begin
              if not GetNewResourceId(entry.resource.fhirType, false {testing-todo},  sId, k) then
                raise exception.create(GetFhirMessage('MSG_RESOURCE_ID_FAIL', lang)+' (entry '+inttostr(index+1)+')');
              id.id := sId;
              id.key := k;
            end;
          end;
        HttpVerbPUT :
          begin
            id.state := tesUpdate;
            if entry.request.url.Contains('?') then
            begin
              s := entry.request.url.substring(entry.request.url.IndexOf('?')+1);
              list := ResolveSearchId(aType, request.compartmentId, request.compartments, request.baseUrl, s);
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
                    raise exception.create(GetFhirMessage('MSG_RESOURCE_ID_FAIL', lang)+' (entry '+inttostr(index+1)+')');
                  id.id := sId;
                  id.key := k;
                end
                else if list.Count > 1 then
                  raise Exception.Create(GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang)+' (entry '+inttostr(index+1)+')');
              finally
                list.Free;
              end;
            end
            else
            begin
              id.id := entry.resource.id;
              if (FindResource(aType, id.id, true, id.key, nil, nil, request.compartments)) then
                id.outcomeVersion := FConnection.CountSQL('select Max(VersionId) from Versions where ResourceKey = '+inttostr(id.key))+1;
            end;
            if ids.ExistsByTypeAndId(id) then
              raise Exception.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [id.restype+'/'+id.id])+' (entry '+inttostr(index+1)+')');

            if (id.state = tesUpdate) and (id.key <> 0) then
            begin
              if entry.request.ifMatch <> '' then
              begin
                if 'W/"'+inttostr(id.outcomeVersion)+'"' <> entry.request.ifMatch then
                  raise Exception.Create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['W/"'+inttostr(id.outcomeVersion)+'"', entry.request.ifMatch])+' (entry '+inttostr(index+1)+')');
              end;
            end
            else if entry.request.ifMatch <> '' then
               raise Exception.Create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['(n/a)', entry.request.ifMatch])+' (entry '+inttostr(index+1)+')');
          end;
        HttpVerbDELETE :
          begin
            id.state := tesDelete;
            if entry.request.url.Contains('?') then
            begin
              s := entry.request.url.substring(entry.request.url.IndexOf('?')+1);
              list := ResolveSearchId(aType, request.compartmentId, request.compartments, request.baseUrl, s);
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
                  raise Exception.Create(GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang)+' (entry '+inttostr(index+1)+')');
              finally
                list.Free;
              end;
            end
            else
            begin
              id.id := sParts[1];
              if (FindResource(aType, id.id, true, id.key, nil, nil, request.compartments)) then
                id.outcomeVersion := FConnection.CountSQL('select Max(VersionId) from Versions where ResourceKey = '+inttostr(id.key)) + 1;
            end;
            if (id.state = tesDelete) and ids.ExistsByTypeAndId(id) then
                  raise Exception.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [id.restype+'/'+id.id])+' (entry '+inttostr(index+1)+')');

            // version check
            if (id.state = tesDelete) and (id.key <> 0) then
            begin
              if entry.request.ifMatch <> '' then
              begin
                if 'W/"'+inttostr(id.outcomeVersion)+'"' <> entry.request.ifMatch then
                 raise Exception.Create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['W/"'+inttostr(id.outcomeVersion)+'"', entry.request.ifMatch])+' (entry '+inttostr(index+1)+')');
              end;
            end
            else if entry.request.ifMatch <> '' then
              raise Exception.Create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['(n/a)', entry.request.ifMatch])+' (entry '+inttostr(index+1)+')')
            else if (id.state = tesDelete) then
              id.state := tesIgnore; // nothing to delete
          end
        else
          raise Exception.Create('illegal method type on entry?');
      end;
    end
    else
    begin
      id.state := tesCreate; // or it might be update - we'll figure out whether it's a create or an update based on the resource itself
      if (entry.resource = nil) then
        raise Exception.create('resource cannot be missing (entry '+inttostr(index+1)+')');
      if (entry.resource.id <> '') and not IsId(entry.resource.id) then
        raise Exception.create('resource id is illegal ("'+entry.resource.id+'") (entry '+inttostr(index+1)+')');
      if (entry.fullUrl = '') or (entry.fullUrl.StartsWith(request.baseUrl)) then
        baseok := true
      else
      begin
        baseok := false;
        for b in ServerContext.bases do
          if entry.fullUrl.StartsWith(b) then
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
            raise Exception.create(StringFormat(GetFhirMessage('MSG_TRANSACTION_DUPLICATE_ID', lang), [id.restype+'/'+id.id])+' (entry '+inttostr(index+1)+')');
      end;
      case id.state of
        tesIgnore: ; // yup, ignore it
        tesCreate:
          begin
            if not GetNewResourceId(entry.resource.FhirType,  false {testing-todo},  sId, k) then
              raise exception.create(GetFhirMessage('MSG_RESOURCE_ID_FAIL', lang)+' (entry '+inttostr(index+1)+')');
            id.id := sId;
            id.key := k;
            entry.resource.id := sId;
          end;
        tesUpdate:
          if (FindResource(aType, id.id, true, id.key, nil, nil, request.compartments)) then
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


procedure TFHIRNativeOperationEngine.FixXhtmlUrls(lang, base : String; ids : TFHIRTransactionEntryList; node : TFhirXHtmlNode);
var
  i, j, k : integer;
  s, url, vHist : string;
begin
  if (node <> nil) and (node.NodeType = fhntElement) then
  begin
    if (node.Name = 'a') and (node.Attributes.Get('href') <> '') then
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
      while (j = -1) and (k < serverContext.Bases.Count - 1) do
      begin
        j := ids.IndexByName(ServerContext.Bases[k]+s);
        inc(k);
      end;

      if (j > -1) then
      begin
        if (vhist = '') then
          node.Attributes.SetValue('href', ids[j].resType+'/'+ids[j].id)
        else if (ids[j].version <> '') and (ids[j].version <> vHist) then
          Raise Exception.create(StringFormat(GetFhirMessage('Version ID Mismatch for '+url+' in narrative: reference to version '+vHist+', reference is '+ids[j].version, lang), [s]))
        else
          node.Attributes.SetValue('href', ids[j].resType+'/'+ids[j].id+'/_history/'+inttostr(ids[j].outcomeVersion));
      end
      else if isLogicalReference(s) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [s]));
    end;
    if (node.Name = 'img') and (node.Attributes.Get('src') <> '') then
    begin
      s := node.Attributes.Get('src');
      j := ids.IndexByName(s);
      if (j > -1) then
        node.Attributes.SetValue('src', base+ids[j].resType+'/'+ids[j].id)
      else if isLogicalReference(s) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [s]));
    end;
    for i := 0 to node.ChildNodes.count - 1 do
      FixXhtmlUrls(lang, base, ids, node.childNodes[i]);
  end;
end;


procedure TFHIRNativeOperationEngine.adjustReferences(request : TFHIRRequest; resp : TFHIRResponse; te : TFHIRTransactionEntry; base : String; entry : TFHIRBundleEntry; ids : TFHIRTransactionEntryList);
var
  refs : TFhirReferenceList;
  ref : TFhirReference;
  url : String;
  vhist : String;
  i, j, k : integer;
  attachments : TFhirAttachmentList;
  attachment : TFhirAttachment;
  extension : TFhirExtension;
  cl : TFhirDocumentReferenceContent;
begin
  if entry.resource = nil then
    exit;
  refs := TFhirReferenceList.create;
  try
    listReferences(entry.resource, refs);
    for i := 0 to refs.count - 1 do
    begin
      ref := refs[i];
      url := fullResourceUri(base, ref);

      if url.contains('?') then
        ref.reference := resolveConditionalURL(request, resp, ref.reference)
      else if (isHistoryURL(url)) then
        splitHistoryUrl(url, vhist)
      else
        vHist := '';

      j := ids.IndexByName(url);
      k := 0;
      while (j = -1) and (k < ServerContext.Bases.Count - 1) do
      begin
        j := ids.IndexByName(ServerContext.Bases[k]+ref.reference);
        inc(k);
      end;
      if (j <> -1) then
      begin
        if (vhist = '') then
          ref.reference := ids[j].resType+'/'+ids[j].id
        else if (ids[j].version <> '') and (ids[j].version <> vHist) then
          Raise Exception.create(StringFormat(GetFhirMessage('Version ID Mismatch for '+url+': reference to version '+vHist+', reference is '+ids[j].version, lang), [ref.reference]))
        else
          ref.reference :=  ids[j].resType+'/'+ids[j].id+'/_history/'+inttostr(ids[j].outcomeVersion);
      end;
    end;
  finally
    refs.free;
  end;
  attachments := TFhirAttachmentList.create;
  try
    ListAttachments(entry.resource, attachments);
    for i := 0 to attachments.count - 1 do
    begin
      attachment := attachments[i];
      j := ids.IndexByName(attachment.url);
      if (j > -1) then
        attachment.url := ids[j].resType+'/'+ids[j].id
      else if isLogicalReference(attachment.url) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [attachment.url]));
    end;
  finally
    attachments.free;
  end;
  if (entry.resource is TFhirDomainResource) then
  begin
    for i := 0 to TFhirDomainResource(entry.resource).extensionList.count - 1 do
    begin
      extension := TFhirDomainResource(entry.resource).extensionList[i];
      j := ids.IndexByName(extension.url);
      if (j > -1) then
        extension.url := base+ids[j].resType+'/'+ids[j].id
      else if isLogicalReference(extension.url) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [extension.url]));
    end;
  end;
  // special case: XdsEntry
  if (entry.resource.resourceType = frtDocumentReference) then
    for cl in TFhirDocumentReference(entry.resource).contentList do
    begin
      attachment := cl.attachment;
      if (attachment <> nil) then
      begin
        j := ids.IndexByName(attachment.url);
        if (j > -1) then
          attachment.url := base+ids[j].resType+'/'+ids[j].id
        else if isLogicalReference(Attachment.url) then
          Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [attachment.url]));
      end;
    end;

  if (TFhirDomainResource(entry.resource).text <> nil) then
    FixXhtmlUrls(lang, base, ids, TFhirDomainResource(entry.resource).text.div_);
end;

function TFHIRNativeOperationEngine.commitResource(request: TFHIRRequest; response : TFHIRResponse; upload : boolean; entry: TFHIRBundleEntry; i : integer; id: TFHIRTransactionEntry; session : TFHIRSession; resp : TFHIRBundle) : Boolean;
var
  ne : TFhirBundleEntry;
  context : TOperationContext;
begin
  if (entry.request <> nil) then
    logt(inttostr(i)+': '+entry.request.methodElement.value+' '+id.summary)
  else
    logt(inttostr(i)+': '+id.summary);

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
    context.upload := upload;
    case id.state of
      tesIgnore: ;  // yup, ignore it
      tesRead: executeReadInTransaction(entry.request, request, response);
      tesCreate: ExecuteCreate(context, request, response, idIsNew, id.key);
      tesUpdate:
        begin
  //        if (entry.request <> nil) and (entry.request.url.contains('?')) then
  //        begin
  //          if Id.count <> ResolveSearchIdCount(id.resType, request.compartmentId, request.compartments, request.baseUrl, entry.request.url.substring(entry.request.url.IndexOf('?')+1)) then
  //            raise Exception.Create('error processing batch - id clash: one of the create statements altered the processing of a conditional update: '+entry.request.url);
  //        end;
          ExecuteUpdate(context, request, response);
        end;
      tesDelete:
        begin
  //        if (entry.request <> nil) and (entry.request.url.contains('?')) then
  //        begin
  //          if Id.count <> ResolveSearchIdCount(id.resType, request.compartmentId, request.compartments, request.baseUrl, entry.request.url.substring(entry.request.url.IndexOf('?')+1)) then
  //            raise Exception.Create('error processing batch - id clash: one of the create or update statements altered the processing of a conditional delete: '+entry.request.url);
  //        end;
          ExecuteDelete(request, Response);
        end;
    end;

    if response.HTTPCode < 300 then
      result := true
    else if response.Resource is TFhirOperationOutcome then
      result := false
    else
      result := check(response, response.HTTPCode < 300, response.HTTPCode, lang, response.Message, IssueTypeNull);
    ne := id.entry;
    ne.resource := response.resource.Link;
    ne.response := TFhirBundleEntryResponse.Create;
    ne.response.status := inttostr(response.HTTPCode);
    ne.response.location := response.Location;
    ne.response.etag := 'W/'+response.versionId;
    ne.response.lastModified := TDateAndTime.CreateUTC(response.lastModifiedDate);
  finally
    context.Free;
  end;
end;

procedure ignoreEntry(req, resp : TFHIRBundleEntry);
begin
  resp.response := TFhirBundleEntryResponse.Create;
  resp.response.status := '200';
  if (req.request <> nil) and (req.request.method = HttpVerbDELETE) then
    resp.response.status := '404';
end;



procedure TFHIRNativeOperationEngine.ExecuteTransaction(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
//  match : boolean;
  i : integer;
  resp : TFHIRBundle;
  ids : TFHIRTransactionEntryList;
  ok : boolean;
  bundle : TFHIRBundle;
  ne : TFHIRBundleEntry;
  entry : TFHIRTransactionEntry;
begin
  try
    ok := true;
    if not check(response, ServerContext.SupportTransaction, 405, lang, 'Transaction Operations not allowed', IssueTypeNotSupported) then
      ok := false;
    if ok and not check(response, request.Resource is TFhirBundle, 400, lang, 'A bundle is required for a Transaction operation', IssueTypeInvalid) then
      ok := false;

    if ok then
    bundle := request.Resource as TFhirBundle;
    if bundle.type_ = BundleTypeBatch then
      executeBatch(context, request, response)
    else
    begin
      request.Source := nil; // ignore that now
      resp := TFHIRBundle.create(BundleTypeTransactionResponse);
      ids := TFHIRTransactionEntryList.create;
      try
        ids.FDropDuplicates := bundle.Tags['duplicates'] = 'ignore';
//        resp.base := baseUrl;
        ids.SortedByName;
        resp.id := FhirGUIDToString(CreateGuid);


        // first pass: scan ids
        for i := 0 to bundle.entryList.count - 1 do
        begin
          bundle.entryList[i].Tag := scanId(request, bundle.entryList[i], ids, i).Link;
          context.progress(0+trunc(100 * (i / (bundle.entryList.count * 10))));
        end;

        // third pass: reassign references
        for i := 0 to bundle.entryList.count - 1 do
        begin
          context.progress(10+trunc(100 * (i / (bundle.entryList.count * 10))));
          entry := bundle.entryList[i].Tag as TFHIRTransactionEntry;

          if not entry.ignore and not entry.deleted then
            adjustReferences(request, response, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.baseUrl, bundle.entryList[i], ids);
        end;

        // four pass: commit resources
        bundle := bundle.Link;
        try
          for i := 0 to bundle.entryList.count - 1 do
          begin
            context.progress(20+trunc(100 * (i / (bundle.entryList.count * 10))));
             ne := resp.entryList.Append;
//             ne.request := bundle.entryList[i].request.Link;
            (bundle.entryList[i].Tag as TFHIRTransactionEntry).entry := ne;
          end;

          for i := 0 to bundle.entryList.count - 1 do
          begin
            context.progress(30+trunc(100 * (i / (bundle.entryList.count * 10))));
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesCreate then
              ignoreEntry(bundle.entryList[i], resp.entryList[i]);
          end;

          for i := 0 to bundle.entryList.count - 1 do
          begin
            context.progress(40+trunc(100 * (i / (bundle.entryList.count * 5))));
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesCreate then
              if not commitResource(request, response, context.upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          end;
          for i := 0 to bundle.entryList.count - 1 do
          begin
            context.progress(60+trunc(100 * (i / (bundle.entryList.count * 5))));
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesUpdate then
              if not commitResource(request, response, context.upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          end;
          for i := 0 to bundle.entryList.count - 1 do
          begin
            context.progress(80+trunc(100 * (i / (bundle.entryList.count * 10))));
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesDelete then
              if not commitResource(request, response, context.upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          end;
          for i := 0 to bundle.entryList.count - 1 do
          begin
            context.progress(90+trunc(100 * (i / (bundle.entryList.count * 10))));
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesRead then
              if not commitResource(request, response, context.upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          end;

        finally
          bundle.free;
        end;
        response.HTTPCode := 200;
        response.Message := 'OK';
        response.bundle := resp.Link;
      finally
        ids.free;
        resp.free;
      end;
    end;
    if request.Resource <> nil then // batch
      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.Resource <> nil then // batch
        AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteBatch(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  req, resp : TFHIRBundle;
  src, dest : TFhirBundleEntry;
  url : String;
  dummy : integer;
  mem : TAdvMemoryStream;
  comp : TFHIRXmlComposer;
  m : TVCLStream;
begin
  try
    req := (request.resource as TFHIRBundle).Link;
    resp := TFHIRBundle.create(BundleTypeBatchResponse);
    try
      resp.id := NewGuidId;
      for src in req.entryList do
      begin
        dest := resp.entryList.Append;
        try
          write('.');

          if (resp.type_ = BundleTypeBatch) and (src.request = nil) then
            raise Exception.Create('No request details');
          if (src.request = nil) then
          begin
            src.request := TFhirBundleEntryRequest.Create;
            src.request.method := HttpVerbPUT;
            src.request.url := CODES_TFhirResourceType[src.resource.ResourceType]+'/'+src.resource.id;
          end;
          dest.request := src.request.Clone;
          request.reset;
          url := request.preAnalyse(src.request.url);
          request.analyse(CODES_TFhirHttpVerbEnum[src.request.method], url, dummy, nil);
          request.IfNoneMatch := src.request.ifNoneMatch;
          if src.request.ifModifiedSince <> nil then
            request.IfModifiedSince := src.request.ifModifiedSince.AsUTCDateTime;
          request.IfMatch := src.request.ifMatch;
          request.IfNoneExist := src.request.ifNoneExist;
          request.resource := src.resource.link;
          request.Source := TAdvBuffer.Create;
          request.PostFormat := ffXml;
          if not context.upload and ServerContext.validate and (request.resource <> nil) then
          begin
            comp := TFHIRXmlComposer.Create(ServerContext.Validator.Context.Link, 'en');
            mem := TAdvMemoryStream.Create;
            m := TVCLStream.Create;
            try
              m.Stream := mem.Link;
              mem.Buffer := request.source.Link;
              comp.compose(m, request.resource, true);
            finally
              m.Free;
              comp.Free;
              mem.Free;
            end;
          end;
          Execute(context, request, response);
          dest.response := TFhirBundleEntryResponse.Create;
          dest.response.status := inttostr(response.HTTPCode);
          dest.response.location := response.Location;
          dest.response.etag := 'W/'+response.versionId;
          dest.response.lastModified := TDateAndTime.CreateUTC(response.lastModifiedDate);
          dest.resource := response.resource.link;
        except
          on e : ERestfulException do
          begin
            dest.response := TFhirBundleEntryResponse.Create;
            dest.response.status := inttostr(e.Status);
            dest.resource := BuildOperationOutcome(request.Lang, e);
          end;
          on e : Exception do
          begin
            dest.response := TFhirBundleEntryResponse.Create;
            dest.response.status := '500';
            dest.resource := BuildOperationOutcome(request.Lang, e);
          end;
        end;
      end;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.bundle := resp.Link;
      logt('done');
    finally
      req.free;
      resp.free;
    end;
    AuditRest(request.session, request.requestId, request.ip, '', '', '', 0, fcmdBatch, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.requestId, request.ip, '', '', '', 0, fcmdBatch, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.ExecuteOperation(context : TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
var
  i : integer;
  op : TFhirOperation;
begin
  if request.OperationName = 'graphql' then
    executeGraphQL(context, request, response)
  else
  begin
    for i := 0 to FOperations.count - 1 do
    begin
      op := TFhirOperation(FOperations[i]);
      if (op.HandlesRequest(request)) then
      begin
        op.Execute(context, self, request, response);
        exit;
      end;
    end;
    raise Exception.Create('Unknown operation '+request.ResourceName+'/$'+request.OperationName);
  end;
end;

function typeForReference(ref : String) : String;
var
  list : TArray<String>;
begin
  list := ref.Split(['/']);
  result := list[0];
end;

procedure TFHIRNativeOperationEngine.CollectIncludes(session : TFhirSession; includes: TReferenceList; resource: TFHIRResource; path: String);
var
  s : String;
  matches : TFHIRObjectList;
  i : integer;
begin
  if resource = nil then
    exit;

  while path <> '' do
  begin
    StringSplit(path, ';', s, path);
    matches := resource.PerformQuery(s);
    try
      for i := 0 to matches.count - 1 do
        if (matches[i] is TFhirReference) and (TFhirReference(matches[i]).reference <> '') and (Session.canRead(typeForReference(TFhirReference(matches[i]).reference))) then
          includes.seeReference(TFhirReference(matches[i]).reference);
    finally
      matches.free;
    end;
  end;
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
    fcmdSearch : result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdSearch;
    fcmdCreate : result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdCreate;
    fcmdConformanceStmt : result := true;
    fcmdUpload, fcmdTransaction : result := ServerContext.SupportTransaction;
    fcmdOperation : result := ServerContext.ResConfig[resource].Supported and ServerContext.ResConfig[resource].cmdOperation;
  else
    result := false;
  end;
end;

//function TFHIRNativeOperationEngine.TypeForKey(key: integer): TFHIRResourceType;
//var
//  a : TFHIRResourceType;
//  b : boolean;
//begin
//  result := frtNull;
//  b := false;
//  for a := low(TFHIRResourceType) to high(TFHIRResourceType) do
//    if  ServerContext.ResConfig[a].key = key then
//    begin
//      result := a;
//      b := true;
//    end;
//  if not b then
//    raise exception.create('key '+inttostr(key)+' not found');
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

procedure TFHIRNativeOperationEngine.ProcessBlob(request: TFHIRRequest; response: TFHIRResponse; field : String; comp : TFHIRParserClass);
var
  parser : TFHIRParser;
  mem : TBytesStream;
begin
  mem := TBytesStream.Create(FConnection.ColBlobByName[field]);
  try
    parser := comp.Create(ServerContext.Validator.Context.link, lang);
    try
      parser.source := mem;
      parser.ParserPolicy := xppDrop;
      parser.Parse;
      response.Resource := parser.resource.Link;
    finally
      parser.free;
    end;
  finally
    mem.Free;
  end;
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
  resourceKey : integer;
  parser : TFHIRParser;
  atype, b, base : String;
  s : TBytes;
begin
  base := context.baseUrl;
  if id.StartsWith(base) then
    id := id.Substring(base.Length)
  else for b in ServerContext.Bases do
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
  if (aType <> '') and (length(id) <= ID_LENGTH) and FindResource(aType, id, false, resourceKey, nil, nil, '') then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['JsonContent'];
        parser := MakeParser(ServerContext.Validator.Context, lang, ffJson, s, xppDrop);
        try
          result := TResourceWithReference.Create(id, parser.resource.Link);
        finally
          parser.free;
        end;
      end
    finally
      FConnection.Terminate;
    end;
  end;
end;


//procedure TFHIRNativeOperationEngine.ExecuteGenerateQA(request: TFHIRRequest; response: TFHIRResponse);
//var
//  profile : TFHirStructureDefinition;
//  source : TFhirResource;
//  resourceKey : integer;
//  id, fid : String;
//  builder : TQuestionnaireBuilder;
//  questionnaire : TFHIRQuestionnaire;
//begin
//  try
//    NotFound(request, response);
//    if check(response, opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
//    begin
//      if FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments) then
//      begin
//        source := GetResourceByKey(resourceKey);
//        try
//          // for now, we simply get the base profile
//          if source is TFHirStructureDefinition then
//            raise Exception.Create('Editing a profile via a profile questionnaire is not supported');
//
//          profile := GetProfileByURL('http://hl7.org/fhir/StructureDefinition/'+Codes_TFHIRResourceType[source.ResourceType], id);
//          try
//            fid := baseUrl+'StructureDefinition/'+id+'/$questionnaire';
//            questionnaire := FRepository.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
//            try
//              builder := TQuestionnaireBuilder.Create;
//              try
//                builder.Profiles := FRepository.Profiles.link;
//                builder.OnExpand := FRepository.ExpandVS;
//                builder.OnLookupCode := FRepository.LookupCode;
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
//                  FRepository.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, builder.Questionnaire, builder.Dependencies);
//                response.HTTPCode := 200;
//                response.Message := 'OK';
//                response.Body := '';
//                response.LastModifiedDate := now;
//                response.link_List.AddRelRef('edit-post', baseUrl+request.ResourceName+'/'+request.id+'/$qa-post');
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
//    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
//  except
//    on e: exception do
//    begin
//      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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
    check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.subid]), IssueTypeNotFound);
  FConnection.terminate;
end;

//function TFHIRNativeOperationEngine.IdentifyValueset(request: TFHIRRequest; resource : TFHIRResource; params: TParseMap; base : String; var used, cacheId : string; allowNull : boolean = false) : TFHIRValueSet;
//begin
//  cacheId := '';
//  if (resource <> nil) and (resource is TFHIRValueSet) then
//    result := resource.Link as TFhirValueSet
//  else if params.VarExists('_id') then
//  begin
//    result := GetValueSetById(request, params.getvar('_id'), base);
//    cacheId := result.url;
//    used := used+'&_id='+params.getvar('_id')
//  end
//  else if params.VarExists('id') then
//  begin
//    result := GetValueSetById(request, params.getvar('id'), base);
//    cacheId := result.url;
//    used := used+'&id='+params.getvar('id')
//  end
//  else if params.VarExists('identifier') then
//  begin
//    if not FRepository.TerminologyServer.isKnownValueSet(params.getvar('identifier'), result) then
//      result := GetValueSetByIdentity(params.getvar('identifier'), params.getvar('version'));
//    cacheId := result.url;
//    used := used+'&identifier='+params.getvar('identifier')
//  end
//  else
//    result := constructValueSet(params, used, allowNull);
//  if params.varExists('nocache') then
//    cacheId := '';
//end;
//

var
  icount : integer;

//procedure TFHIRNativeOperationEngine.ExecuteQuestionnaireGeneration(request: TFHIRRequest; response : TFHIRResponse);
//var
//  profile : TFHirStructureDefinition;
//  op : TFhirOperationOutcome;
//  resourceKey : integer;
//  id, fid : String;
//  builder : TQuestionnaireBuilder;
//  questionnaire : TFHIRQuestionnaire;
//begin
//  try
//    NotFound(request, response);
//    if check(response, opAllowed(request.ResourceName, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
//    begin
//      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and FindResource(frtStructureDefinition, request.Id, false, resourceKey, request, response, request.compartments)) then
//      begin
//        profile := nil;
//        try
//          // first, we have to identify the value set.
//          id := request.Id;
//          if request.Id <> '' then // and it must exist, because of the check above
//            profile := GetProfileById(request, request.Id, baseUrl)
//          else if request.Parameters.VarExists('identifier') then
//            profile := GetProfileByURL(request.Parameters.getvar('identifier'), id)
//          else if (request.form <> nil) and request.form.hasParam('profile') then
//            profile := LoadFromFormParam(request.form.getparam('profile'), request.Lang) as TFHirStructureDefinition
//          else if (request.Resource <> nil) and (request.Resource is TFHirStructureDefinition) then
//            profile := request.Resource.Link as TFHirStructureDefinition
//          else
//            raise Exception.Create('Unable to find profile to convert (not provided by id, identifier, or directly)');
//
//          if id <> '' then
//          begin
//            fid := baseUrl+'StructureDefinition/'+id+'/$questionnaire';
//            questionnaire := FRepository.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
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
//                builder.OnExpand := FRepository.ExpandVS;
//                builder.onLookupCode := FRepository.LookupCode;
//                builder.Context := request.Link;
//                builder.onLookupReference := LookupReference;
//                builder.QuestionnaireId := fid;
//                builder.build;
//                questionnaire := builder.questionnaire.Link;
//                if id <> '' then
//                  FRepository.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.dependencies);
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
//    TFHIRXhtmlComposer.Create('en').Compose(TFileStream.Create('c:\temp\q'+inttostr(iCount)+'.xml', fmCreate), response.Resource, true, nil);
//    AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
//  except
//    on e: exception do
//    begin
//      AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
//      recordStack(e);
// raise;
//    end;
//  end;
//end;
//

function TFHIRNativeOperationEngine.GetResourceByUrl(aType : TFhirResourceType; url, version: String; allowNil : boolean; var needSecure : boolean): TFHIRResource;
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
        raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'Unknown '+CODES_TFHIRResourceType[aType]+' '+url, 404, IssueTypeUnknown);
    needSecure := FConnection.ColIntegerByName['Secure'] = 1;
    s := FConnection.ColBlobByName['JsonContent'];
    parser := MakeParser(ServerContext.Validator.Context, lang, ffJson, s, xppDrop);
    try
      result := parser.resource.Link as TFHIRResource;
      try
        if FConnection.FetchNext then
          raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'Found multiple matches for '+CODES_TFHIRResourceType[aType]+' '+url+'. Pick one by the resource id', 404, IssueTypeNotFound);
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

function TFHIRNativeOperationEngine.GetResourcesByParam(aType : TFhirResourceType; name, value: String; var needSecure : boolean): TAdvList<TFHIRResource>;
var
  s : TBytes;
  parser : TFHIRParser;
begin
  result := TAdvList<TFHIRResource>.create;
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
        parser := MakeParser(ServerContext.Validator.Context, lang, ffJson, s, xppDrop);
        try
          result.Add(parser.resource.Link);
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


function TFHIRNativeOperationEngine.GraphFollowReference(appInfo : TAdvObject; context: TFHIRResource; reference: TFHIRReference; out targetContext, target: TFHIRResource): boolean;
var
  req : TFHIRRequest;
  secure : boolean;
begin
  req := TFHIRRequest(appInfo);
  target := getResourceByReference(context, reference.reference, req.compartments, true, secure);
  result := (target <> nil) and (not secure or req.secure);
  if result then
    targetContext := target.Link
  else
    target.Free;
end;

procedure TFHIRNativeOperationEngine.GraphFollowReverseReference(appInfo: TAdvObject; focusType, focusId, requestType, requestParam: String; params: TAdvList<TGraphQLArgument>; list: TAdvList<TFhirResource>);
var
  sql : String;
  ik, tk, rk : integer;
  sp : TSearchProcessor;
  pm : TParseMap;
  url : String;
  b : TStringBuilder;
  json : TFHIRJsonParser;
  request : TFHIRRequest;
  p : TGraphQLArgument;
begin
  request := TFHIRRequest(appInfo);
  rk := FRepository.ResourceTypeKeyForName(requestType);
  if not FindResource(focusType, focusId, false, tk, request, nil, TFHIRRequest(appinfo).compartments) then
    raise Exception.Create('Unable to find resource '+focusType+'/'+focusId+' internally');  // which would be pretty weird because we must have already found it in this transaction
  ik := ServerContext.Indexes.GetKeyByName(requestParam);
  sql := 'Select Ids.ResourceKey, JsonContent from Ids, Versions where Deleted = 0 and Ids.MostRecent = Versions.ResourceVersionKey and Ids.ResourceTypeKey = '+inttostr(rk)+
    ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where IndexKey = '+inttostr(ik)+' and target = '+inttostr(tk)+') ';
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
      sp.strict := true;
      sp.typekey := rk;
      sp.type_ := requestType;
      sp.compartmentId := request.compartmentId;
      sp.compartments := request.compartments;
      sp.baseURL := request.baseURL;
      sp.lang := lang;
      sp.params := TParseMap.create(url);
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
  json := TFHIRJsonParser.Create(request.Context.link, request.lang);
  try
    FConnection.Sql := sql;
    FConnection.Prepare;
    try
      FConnection.Execute;
      while FConnection.FetchNext do
      begin
        json.source := FConnection.ColMemoryByName['JsonContent'];
        json.source.Position := 0;
        json.Parse;
        list.Add(json.resource.Link);
      end;
    finally
      FConnection.Terminate;
    end;
  finally
    json.Free;
  end;
end;

function TFHIRNativeOperationEngine.GetResourceById(request: TFHIRRequest; aType : String; id, base: String; var needSecure : boolean): TFHIRResource;
var
  resourceKey : integer;
  parser : TFHIRParser;
  b : String;
  s : TBytes;
begin
  if id.StartsWith(base) and (base <> '') then
    id := id.Substring(base.Length)
  else for b in ServerContext.Bases do
    if id.StartsWith(b) then
      id := id.Substring(b.Length);
  if id.startsWith(aType+'/') then
    id := id.Substring(length(aType)+1);

  if (length(id) <= ID_LENGTH) and FindResource(aType, id, false, resourceKey, request, nil, '') then
  begin
    FConnection.SQL := 'Select Secure, JsonContent from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['JsonContent'];
        needSecure := FConnection.ColIntegerByName['Secure'] = 1;
        parser := MakeParser(ServerContext.Validator.Context, lang, ffJson, s, xppDrop);
        try
          result := parser.resource.Link;
        finally
          parser.free;
        end;
      end
      else
        raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'Unable to find '+aType+'/'+id, 404, IssueTypeNotFound);
    finally
      FConnection.Terminate;
    end;
  end
  else
    raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'Unknown resource '+aType+'/'+id, 404, IssueTypeNotFound);
end;

function TFHIRNativeOperationEngine.GetResourceByKey(key: integer; var needSecure : boolean): TFHIRResource;
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
      parser := MakeParser(ServerContext.Validator.Context, lang, ffJson, s, xppDrop);
      try
        result := parser.resource.Link;
      finally
        parser.free;
      end;
    end
    else
      raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'Unable to find resource '+inttostr(key), 404, IssueTypeNotFound);
  finally
    FConnection.Terminate;
  end;
end;

function TFHIRNativeOperationEngine.getResourceByReference(source : TFHIRResource; url, compartments: string; allowNil : boolean; var needSecure : boolean): TFHIRResource;
var
  parser : TFHIRParser;
  s : TBytes;
  i, key : integer;
  id, ver : String;
  rtype : String;
  parts : TArray<String>;
  res : TFHIRResource;
begin
  result := nil;
  ver := '';
  if url.StartsWith('#') and (source is TFHIRDomainResource) then
  begin
    for res in TFHIRDomainResource(source).containedList do
      if '#'+res.id = url then
        exit(res);
    raise Exception.Create('Unable to find contained resource '+url);
  end;

  for i := 0 to ServerContext.Bases.Count - 1 do
    if url.StartsWith(ServerContext.Bases[i]) then
      url := url.Substring(ServerContext.Bases[i].Length);

  if (url.StartsWith('http://') or url.StartsWith('https://')) then
    raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'Cannot resolve external reference: '+url, 404, IssueTypeNotFound);

  parts := url.Split(['/']);
  if length(parts) = 2 then
  begin
    if not StringArrayExistsSensitive(CODES_TFhirResourceType, parts[0])  and not ServerContext.ValidatorContext.hasCustomResource(parts[0]) then
      raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'URL not understood: '+url, 404, IssueTypeNotFound);
    rType := parts[0];
    id := parts[1];
  end else if (length(parts) = 4) and (parts[2] = '_history') then
  begin
    if not StringArrayExistsSensitive(CODES_TFhirResourceType, parts[0])  and not ServerContext.ValidatorContext.hasCustomResource(parts[0]) then
      raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'URL not understood: '+url, 404, IssueTypeNotFound);
    rType := parts[0];
    id := parts[1];
    ver := parts[3];
  end
  else
    raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'URL not understood: '+url, 404, IssueTypeNotFound);

  if FindResource(rtype, id, false, key, nil, nil, compartments) then
  begin
    if ver <> '' then
      FConnection.SQL := 'Select Secure, JsonContent from Versions where ResourceKey = '+inttostr(key)+' and VersionId = '''+sqlwrapString(ver)+''''
    else
      FConnection.SQL := 'Select Secure, JsonContent from Versions where ResourceKey = '+inttostr(key)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['JsonContent'];
        needSecure := FConnection.ColIntegerByName['Secure'] = 1;
        parser := MakeParser(ServerContext.Validator.Context, lang, ffJson, s, xppDrop);
        try
          result := parser.resource.Link;
        finally
          parser.free;
        end;
      end
      else if not allowNil then
        raise ERestfulException.create('TFHIRNativeOperationEngine', 'GetResourceByUrl', 'Unable to find resource '+inttostr(key), 404, IssueTypeNotFound);
    finally
      FConnection.Terminate;
    end;
  end
  else
    result := nil;
end;



function ReadOperator(s : String) : TFhirFilterOperatorEnum;
begin
  s := LowerCase(s);
  if s = '' then
    result := FilterOperatorIsA
  else if (s = '=') or (s = 'equals') then
    result := FilterOperatorEqual
  else if (s = 'nota') then
    result := FilterOperatorIsNotA
  else if (s = '<=') or (s = 'isa') then
    result := FilterOperatorIsA
  else if (s = 'regex') then
    result := FilterOperatorRegex
  else
    raise Exception.create('Unhandled filter operator value "'+s+'"');
end;

//function TFHIRNativeOperationEngine.constructValueSet(params: TParseMap; var used: String; allowNull : Boolean): TFhirValueset;
//var
//  empty : boolean;
//  function UseParam(name : String; var value : String) : boolean; overload;
//  begin
//    result := params.VarExists(name);
//    value := params.GetVar(name);
//    used := used + '&'+name+'='+EncodeMime(value);
//    empty := value <> '';
//  end;
//  function UseParam(name : String): String; overload;
//  begin
//    result := params.GetVar(name);
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
//        inc.system := 'http://snomed.info/sct'
//      else if (s = 'loinc') then
//        inc.system := 'http://loinc.org'
//      else
//        inc.system := s;
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
//      raise Exception.Create('Not value set details found');
//  finally
//    result.free;
//  end;
//  if empty then
//    result := nil;
//end;
//
procedure TFHIRNativeOperationEngine.AuditRest(session: TFhirSession; reqid, ip: string; resourceName: String; id, ver: String; verkey : integer; op: TFHIRCommandType; provenance : TFhirProvenance; httpCode: Integer; name, message: String);
begin
  AuditRest(session, reqid, ip, resourceName, id, ver, verkey, op, provenance, '', httpCode, name, message);
end;

procedure TFHIRNativeOperationEngine.AuditRest(session: TFhirSession; reqid, ip: string; resourceName: String; id, ver: String; verkey : integer; op: TFHIRCommandType; provenance : TFhirProvenance; opName : String; httpCode: Integer; name, message: String);
var
  se : TFhirAuditEvent;
  c : TFhirCoding;
  p : TFhirAuditEventParticipant;
  o : TFhirAuditEventObject;
  procedure event(t, ts, td, s, sc : String; a : TFhirAuditEventActionEnum);
  begin
    se.event.type_ := TFhirCoding.create;
    c := se.event.type_;
    c.code := t;
    c.system := ts;
    c.display := td;
    c := se.event.subtypeList.append;
    c.code := s;
    c.system := sc;
    c.display := s;
    se.event.action := a;
  end;
begin
  if not ServerContext.DoAudit then
    exit;
  se := TFhirAuditEvent.create;
  try
    if verkey <> 0 then
      se.Tags['verkey'] := inttostr(verkey);
    if reqid <> '' then
      se.addExtension('http://healthintersections.com.au/fhir/StructureDefinition/request-id', reqid);

    se.event := TFhirAuditEventEvent.create;
    case op of
      fcmdRead:            event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'read',    'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdVersionRead:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'vread',   'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdUpdate:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'update',  'http://hl7.org/fhir/restful-operation', AuditEventActionU);
      fcmdDelete:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'delete',  'http://hl7.org/fhir/restful-operation', AuditEventActionD);
      fcmdHistoryInstance: event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-instance', 'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdCreate:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'create',  'http://hl7.org/fhir/restful-operation', AuditEventActionC);
      fcmdSearch:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'search',  'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdHistoryType:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-type', 'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdValidate:        event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'validate', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdConformanceStmt: event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'conformance',    'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdTransaction:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'transaction', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdBatch:           event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'batch', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdPatch:           event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'patch', 'http://hl7.org/fhir/restful-operation', AuditEventActionU);
      fcmdHistorySystem:   event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-system', 'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdUpload:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'upload', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdOperation:       event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'operation', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
    else // fcmdUnknown
      raise exception.create('unknown operation');
    end;
    if op = fcmdOperation then
    begin
      c := se.event.subtypeList.Append;
      c.code := opName;
      c.system := 'http://healthintersections.com.au/fhir/operation-name';
    end;
    if httpCode < 300 then
      se.event.outcome := AuditEventOutcome0
    else if httpCode < 500 then
      se.event.outcome := AuditEventOutcome4
    else
      se.event.outcome := AuditEventOutcome8; // no way we are going down...
    se.event.dateTime := NowUTC;
    se.Tag := se.event.dateTime.Link;

    se.source := TFhirAuditEventSource.create;
    se.source.site := ServerContext.OwnerName;
    se.source.identifier := TFhirIdentifier.Create;
    se.source.identifier.value := ServerContext.SystemId;
    se.source.identifier.system := 'urn:ietf:rfc:3986';

    c := se.source.type_List.Append;
    c.code := '3';
    c.display := 'Web Server';
    c.system := 'http://hl7.org/fhir/security-source-type';

    // participant - the web browser / user proxy
    p := se.participantList.Append;
    if session = nil then
      p.name := 'Server'
    else
    begin
      p.userId := TFhirIdentifier.Create;
      p.userId.value := inttostr(session.Key);
      p.userId.system := ServerContext.SystemId;
      p.altId := session.Id;
      p.name := session.Name;
    end;
    p.requestor := true;
    p.network := TFhirAuditEventParticipantNetwork.create;
    p.network.address := ip;
    p.network.type_ := NetworkType2;

    if resourceName <> '' then
    begin
      o := se.object_List.Append;
      o.reference := TFhirReference.create;
      if ver <> '' then
        o.reference.reference := resourceName+'/'+id+'/_history/'+ver
      else if id <> '' then
        o.reference.reference := resourceName+'/'+id;
      o.type_ := TFhirCoding.Create;
      o.type_.system := 'http://hl7.org/fhir/security-source-type';
      o.type_.code := '2';
      o.lifecycle := TFhirCoding.Create;
      o.lifecycle.system := 'http://hl7.org/fhir/object-lifecycle';
      case op of
        fcmdRead:            o.lifecycle.code := '6';
        fcmdVersionRead:     o.lifecycle.code := '6';
        fcmdUpdate:          o.lifecycle.code := '3';
        fcmdDelete:          o.lifecycle.code := '14';
        fcmdHistoryInstance: o.lifecycle.code := '9';
        fcmdCreate:          o.lifecycle.code := '1';
        fcmdSearch:          o.lifecycle.code := '6';
        fcmdHistoryType:     o.lifecycle.code := '9';
        fcmdValidate:        o.lifecycle.code := '4';
        fcmdConformanceStmt: o.lifecycle.code := '6';
        fcmdTransaction:     o.lifecycle.code := '3';
        fcmdHistorySystem:   o.lifecycle.code := '9';
        fcmdUpload:          o.lifecycle.code := '9';
      end;
      if op = fcmdSearch then
        o.query := StringAsBytes(name)
      else
        o.name := name;
    end;
    FRepository.queueResource(se);
  finally
    se.Free;
  end;
end;

procedure TFHIRNativeOperationEngine.storeResources(list : TFHIRResourceList; origin : TFHIRRequestOrigin; upload : boolean);
var
  i : integer;
  request: TFHIRRequest;
  response : TFHIRResponse;
  context : TOperationContext;
begin
  CreateIndexer;
  context := TOperationContext.create;
  try
    context.upload := upload;
    Connection.StartTransact;
    try
      // cut us off from the external request
      request := TFHIRRequest.create(ServerContext.ValidatorContext.Link, origin, FIndexer.Definitions.Compartments.Link);
      response := TFHIRResponse.create;
      try
        for i := 0 to list.count - 1 do
        begin
          request.ResourceName := list[i].FhirType;
          request.CommandType := fcmdCreate;
          request.Resource := list[i].link;

          if (list[i].id <> '') then
          begin
            request.id := list[i].id;
            request.CommandType := fcmdUpdate;
          end;

          if TFhirResource(list[i]).Tag <> nil then
            request.lastModifiedDate := TDateAndTime(TFhirResource(list[i]).Tag).GetDateTime;
          request.Session := nil;
          Execute(context, request, response);
        end;
      finally
        response.Free;
        request.free;
      end;
      Connection.Commit;
    except
      on e : Exception do
      begin
        Connection.Rollback;
        recordStack(e);
        raise;
      end;
    end;
  finally
    context.Free;
  end;
end;


procedure TFHIRNativeOperationEngine.ReIndex;
var
  list : TStringList;
  i : integer;
  r : TFHirResource;
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
        tags := TFHIRTagList.create;
        try
          parser := MakeParser(ServerContext.Validator.Context, 'en', ffJson, Connection.ColMemoryByName['JsonContent'], xppDrop);
          try
            r := parser.resource;
            FConnection.terminate;
            Connection.StartTransact;
            try
              FIndexer.execute(Integer(list.objects[i]), list[i], r, tags);
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


function TFHIRNativeOperationEngine.resolveConditionalURL(request : TFHIRRequest; resp : TFHIRResponse; url: String): String;
var
  s : String;
  parts : TArray<String>;
  list : TMatchingResourceList;
begin
  for s in ServerContext.Bases do
    if url.StartsWith(s) then
      url := url.Substring(s.Length);
  if url.StartsWith('/') then
    url := url.Substring(1);
  parts := url.Split(['?']);

  list := ResolveSearchId(parts[0], request.compartmentId, request.compartments, url, parts[1]);
  try
    if list.Count = 1 then
      result := parts[0]+'/'+list[0].Name
    else if list.Count > 1 then
      raise ERestfulException.Create('TFHIRNativeOperationEngine', 'resolveConditionalURL', 'Multiple matches found for '+url, 412, IssueTypeConflict)
    else
      raise ERestfulException.Create('TFHIRNativeOperationEngine', 'resolveConditionalURL', 'No matches found for '+url, 404, IssueTypeConflict);
  finally
    list.Free;
  end;
end;

procedure TFHIRNativeOperationEngine.clear(a: TFhirResourceTypeSet);
var
  i : TFhirResourceType;
  k, l : string;
begin
  Connection.ExecSQL('delete from SearchEntries');
  Connection.ExecSQL('delete from Searches');
  Connection.ExecSQL('delete from IndexEntries');

  for i := Low(TFhirResourceType) to High(TFhirResourceType) do
  begin
    if i in a then
    begin
      k := inttostr(Connection.CountSQL('select ResourceTypeKey from Types where ResourceName = '''+CODES_TFhirResourceType[i]+''''));
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

procedure TFHIRNativeOperationEngine.CheckCompartments(actual, allowed: String);
var
  act, all : TStringList;
  i : integer;
begin
  if allowed <> '' then
  begin
    act := TStringList.create;
    all := TStringList.create;
    try
      act.CommaText := actual;
      all.CommaText := allowed;
      for i := 0 to act.count - 1 do
        if all.IndexOf(act[i]) < 0 then
          raise Exception.create('Compartment error: no access to compartment for patient '+act[i]);
    finally
      act.free;
      all.free;
    end;
  end;
end;

procedure TFHIRNativeOperationEngine.CheckCreateNarrative(request : TFHIRRequest);
var
  gen : TNarrativeGenerator;
  r : TFhirDomainResource;
  profile : TFHirStructureDefinition;
begin
  if request.Resource is TFhirDomainResource then
  begin
    r := request.Resource as TFhirDomainResource;
    if (r <> nil) and ((r.text = nil) or (r.text.div_ = nil)) then
    begin
      profile := ServerContext.ValidatorContext.Profiles.ProfileByType[r.ResourceType].Link;
      try
        if profile = nil then
          r.text := nil
        else
        begin
          gen := TNarrativeGenerator.Create('', ServerContext.ValidatorContext.Profiles.Link, FRepository.LookupCode, LookupReference, request.Link);
          try
            gen.generate(r, profile);
          finally
            gen.Free;
          end;
        end;
      finally
        profile.Free;
      end;
    end;
  end;
end;

//procedure TFHIRNativeOperationEngine.ProcessMsgQuery(request: TFHIRRequest; response: TFHIRResponse; bundle : TFHIRBundle);
//begin
//  raise exception.create('query-response is not yet supported');
//end;

//function TFHIRNativeOperationEngine.BuildResponseMessage(request: TFHIRRequest; incoming: TFhirMessageHeader): TFhirMessageHeader;
//var
//  dst : TFhirMessageHeaderDestination;
//begin
//  result := TFhirMessageHeader.create;
//  try
//    result.id := GUIDToString(CreateGUID);
//    result.timestamp := NowUTC;
//    result.event := incoming.event.Clone;
//    result.response := TFhirMessageHeaderResponse.create;
//    result.response.identifier := NewGuidId;
//    result.response.code := ResponseCodeOk;
//    dst := result.destinationList.Append;
//    if incoming.source <> nil then
//    begin
//      dst.name := incoming.source.name;
//      dst.endpoint := incoming.source.endpoint;
//    end
//    else
//    begin
//      dst.name := 'No Source Provided';
//      dst.endpoint := 'http://example.com/unknown';
//    end;
//    result.source := TFhirMessageHeaderSource.create;
//    result.source.endpoint := baseUrl+'/mailbox';
//    result.source.name := 'Health Intersections';
//    result.source.software := ServerContext.OwnerName;
//    result.source.version := FHIR_GENERATED_VERSION;
//    result.source.contact := FFactory.makeContactPoint('email', 'grahame@healthintersections.com.au', '');
//    result.link;
//  finally
//    result.free;
//  end;
//end;
//
//procedure TFHIRNativeOperationEngine.ProcessMessage(request: TFHIRRequest; response : TFHIRResponse; msg, resp: TFhirMessageHeader; bundle: TFHIRBundle);
//var
//  s : String;
//begin
//  try
//    s := msg.event.code;
//    if s = 'MedicationAdministration-Complete' then
//      raise exception.create('MedicationAdministration-Complete is not yet supported')
//    else if s = 'MedicationAdministration-Nullification' then
//      raise exception.create('MedicationAdministration-Nullification is not yet supported')
//    else if s = 'MedicationAdministration-Recording' then
//      raise exception.create('MedicationAdministration-Recording is not yet supported')
//    else if s = 'MedicationAdministration-Update' then
//      raise exception.create('MedicationAdministration-Update is not yet supported')
//    else if s = 'admin-notify' then
//      raise exception.create('admin-notify is not yet supported')
//    else if s = 'diagnosticreport-provide' then
//      raise exception.create('diagnosticreport-provide is not yet supported')
//    else if s = 'observation-provide' then
//      raise exception.create('observation-provide is not yet supported')
//    else if s = 'query' then
//      ProcessMsgQuery(request, response, bundle)
//    else if s = 'query-response' then
//      raise exception.create('query-response is not yet supported')
////    else if s = 'make-claim' then
////      ProcessMsgClaim(request, msg, resp, bundle, bundle)
//    else
//      raise exception.create('Unknown message event: "'+s+'"');
//
//  except
//    on e:exception do
//    begin
//      resp.response.code := ResponseCodeFatalError;
//      resp.response.details := FFactory.makeReferenceText(e.message);
//    end;
//  end;
//end;
//
{
procedure TFHIRNativeOperationEngine.ProcessMsgClaim(request : TFHIRRequest; incoming, outgoing : TFhirMessageHeader; inbundle, outbundle: TFHIRBundle);
var
  id : string;
  claim : TFhirClaim;
  rem : TFhirRemittance;
  i : integer;
  svc : TFhirRemittanceService;
  entry : TFHIRBundleEntry;
  utc : TDateAndTime;
  context : TFHIRValidatorContext;
begin
  context := ServerContext.Validator.AcquireContext;
  try
    claim := GetResourceFrombundle(inbundle, incoming.dataList[0]) as TFhirClaim;
    id := MessageCreateResource(context, request, claim);
    rem := TFhirRemittance.create;
    try
      rem.identifier := FFactory.makeIdentifier('urn:ietf:rfc:3986', NewGuidURN);
      for i := 0 to claim.serviceList.count - 1 do
      begin
        svc := rem.serviceList.Append;
        svc.instance := claim.serviceList[i].instance;
        svc.rate := '0.8';
        svc.benefit := floatToStr(0.8 * StrToFloat(claim.serviceList[i].instance));
      end;
      id := baseURL+'/remittance/'+MessageCreateResource(context, request, rem);
      outgoing.dataList.add(FFactory.makeReference(id));
      utc := NowUTC;
      try
        entry := outbundle.addEntry('remittance', utc, id, id, rem);
      finally
        utc.free;
      end;
    finally
      rem.free;
    end;
  finally
    ServerContext.Validator.YieldContext(context);
  end;
end;

function TFHIRNativeOperationEngine.MessageCreateResource(context : TFHIRValidatorContext; request : TFHIRRequest; res: TFHIRResource): string;
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
begin
  req := TFHIRRequest.create;
  try
    req.Session := request.session.Link;
    req.CommandType := fcmdCreate;
    req.ResourceType := res.ResourceType;
    req.Resource := res.Link;
    req.baseUrl := baseUrl;
    resp := TFHIRResponse.create;
    try
      ExecuteCreate(context, false, req, resp, idNoNew, 0);
      result := req.Id;
    finally
      resp.free;
    end;
  finally
    req.free;
  end;
end;
}

(*
function TFHIRNativeOperationEngine.AddDeletedResourceTobundle(bundle: TFHIRBundle; sId, sType, base : String): TFHIRBundleEntry;
var
  entry : TFHIRBundleEntry;
begin
//  entry := TFHIRBundleEntry.Create;
//  try
//    entry.title := GetFhirMessage('NAME_RESOURCE', lang)+' '+sId+' '+GetFhirMessage('NAME_VERSION', lang)+' '+FConnection.ColStringByName['VersionId']+' ('+GetFhirMessage('NAME_DELETED', lang)+')';
//    entry.deleted := true;
//    entry.link_List['self'] := base+lowercase(sType)+'/'+sId+'/_history/'+FConnection.ColStringByName['VersionId'];
//    entry.updated := TDateAndTime.CreateUTC(TSToDateTime(FConnection.ColTimeStampByName['StatedDate']));
//    entry.resource.id := base+sId;
//    entry.published_ := NowUTC;
//    entry.authorName := FConnection.ColStringByName['Name'];
//    entry.categories.decodeJson(FConnection.ColBlobByName['Tags']);
//    bundle.entryList.add(entry.Link);
//    result := entry;
//  finally
//    entry.Free;
//  end;
  raise Exception.Create('To do');
  (*
  entry := TFHIRBundleEntry.Create;
  try
    entry.deleted := TFhirBundleEntryDeleted.Create;
    entry.deleted.type_ := sType;
    entry.deleted.resourceId := sId;
    entry.deleted.versionId := FConnection.ColStringByName['VersionId'];
    entry.deleted.instant := TDateAndTime.CreateUTC(TSToDateTime(FConnection.ColTimeStampByName['StatedDate']));
    bundle.entryList.add(entry.Link);
    result := entry;
  finally
    entry.Free;
  end;
  *.)

end;
*)


procedure TFHIRNativeOperationEngine.checkProposedContent(request : TFHIRRequest; resource: TFhirResource; tags: TFHIRTagList);
var
  l, r : String;
begin
  if resource is TFhirSubscription then
  begin
    if (TFhirSubscription(resource).status <> SubscriptionStatusRequested) and (request.origin = roRest) then // nil = from the internal system, which is allowed to
      raise Exception.Create('Subscription status must be "requested", not '+TFhirSubscription(resource).statusElement.value);
    if (TFhirSubscription(resource).channel = nil) then
      raise Exception.Create('Subscription must have a channel');
    if (TFhirSubscription(resource).channel.type_ = SubscriptionChannelTypeWebsocket) and not ((TFhirSubscription(resource).channel.payload = '') or StringArrayExistsSensitive(['application/xml+fhir', 'application/fhir+xml', 'application/xml', 'application/json+fhir', 'application/fhir+json', 'application/json'], TFhirSubscription(resource).channel.payload)) then
      raise Exception.Create('A websocket subscription must have a no payload, or the payload must be application/xml+fhir or application/json+fhir');
    if (TFhirSubscription(resource).status = SubscriptionStatusRequested) then
      TFhirSubscription(resource).status := SubscriptionStatusActive; // well, it will be, or it will be rejected later
    StringSplit(TFhirSubscription(resource).criteria, '?', l, r);
    if (StringArrayIndexOfSensitive(CODES_TFhirResourceType, l) < 1) or (r = '') then
      raise Exception.Create('Criteria is not valid');
  end;
  if (resource is TFhirOperationDefinition) then
  begin
    if resource.id.StartsWith('fso-') and (resource.tags['internal'] <> '1') then
      raise Exception.Create('operation Definitions that start with "fso-" are managed by the system directly, and cannot be changed through the REST API');
  end;
  if (resource is TFHIRStructureDefinition) then
  begin
    if ServerContext.ValidatorContext.hasCustomResourceDefinition(TFHIRStructureDefinition(resource)) then
      raise Exception.Create('Cannot update a structure definition that is in use as a custom resource');
  end;
end;

procedure TFHIRNativeOperationEngine.checkProposedDeletion(request: TFHIRRequest; resource: TFhirResource; tags: TFHIRTagList);
begin

  if (resource is TFhirOperationDefinition) then
  begin
    if resource.id.StartsWith('fso-') then
      raise Exception.Create('operation Definitions that start with "fso-" are managed by the system directly, and cannot be changed through the REST API');
  end;
  if (resource is TFHIRStructureDefinition) then
  begin
    if ServerContext.ValidatorContext.hasCustomResourceDefinition(TFHIRStructureDefinition(resource)) then
      raise Exception.Create('Cannot delete a structure definition that is in use as a custom resource');
  end;
end;

procedure TFHIRNativeOperationEngine.chooseField(aFormat : TFHIRFormat; summary : TFHIRSummaryOption; adaptor : TFHIRFormatAdaptor; out fieldName : String; out comp : TFHIRParserClass; out needsObject : boolean)
;
var
  s : String;
begin
  fieldName := '';
  comp := nil;
  needsObject := adaptor <> nil;

  if aFormat = ffJson then
  begin
    s := 'Json';
    comp := TFHIRJsonParser;
  end
  else if aformat = ffXhtml then
  begin
    s := 'Json';
    comp := TFHIRJsonParser;
    needsObject := true;
  end
  else
  begin
    s := 'Xml';
    comp := TFHIRXmlParser;
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



{ TFhirOperation }

function TFhirOperation.Name: String;
begin
  result := '';
end;

function TFhirOperation.owningResource: TFhirResourceType;
var
  t : TFhirResourceType;
  b : boolean;
begin
  result := frtNull;
  b := false;
  for t in Types do
    if b then
      raise Exception.Create('Multiple types for operation')
    else
    begin
      result := t;
      b := true;
    end;
  if (not b) then
    raise Exception.Create('No types for operation');
end;

function TFhirOperation.Types: TFhirResourceTypeSet;
begin
  result := [];
end;

function TFhirOperation.CreateBaseDefinition(base : String): TFHIROperationDefinition;
begin
  result := TFhirOperationDefinition.Create;
  try
    result.id := 'fso-'+name;
    result.meta := TFhirMeta.Create;
    result.meta.lastUpdated := TDateAndTime.CreateXML(FHIR_GENERATED_DATE);
    result.meta.versionId := SERVER_VERSION;
    result.url := AppendForwardSlash(base)+'OperationDefinition/fso-'+name;
    result.version := FHIR_GENERATED_VERSION;
    result.name := 'Operation Definition for "'+name+'"';
    result.publisher := 'Grahame Grieve';
    with result.contactList.Append do
      with telecomList.Append do
      begin
        system := ContactPointSystemEmail;
        value := 'grahame@fhir.org';
      end;
    result.description := 'Reference FHIR Server Operation Definition for "'+name+'"';
    result.status := PublicationStatusDraft;
    result.experimental := false;
    result.date := TDateAndTime.CreateXML(FHIR_GENERATED_DATE);
    result.kind := OperationKindOperation;
    result.code := name;
    result.base := TFhirReference.Create;
    result.base.reference := 'http://hl7.org/fhir/OperationDefinition/'+name;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFhirOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
begin
  // nothing
end;

function TFhirOperation.formalURL: String;
begin
  result := '';
end;

function TFhirOperation.HandlesRequest(request: TFHIRRequest): boolean;
var
  t : TFhirResourceType;
begin
  result := (request.OperationName = Name) and (request.ResourceEnum in Types);
  if result then
  begin
    t := owningResource;
    if t = frtNull then
      t := request.ResourceEnum;
    if t = frtNull then
      result := ((isWrite and request.canWrite('') or (not isWrite and request.canRead('')))) // todo: what should it be?
    else
      result := ((isWrite and request.canWrite(CODES_TFHIRresourceType[t])) or (not isWrite and request.canRead(CODES_TFHIRresourceType[t])));
  end;
end;

function TFhirOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirOperation.makeParams(request: TFHIRRequest): TFhirParameters;
var
  i : integer;
begin
  if (request.Resource <> nil) and (request.Resource is TFHIRParameters) then
    result := request.Resource.Link as TFHIRParameters
  else
    result := TFhirParameters.Create;
  try
    for i := 0 to request.Parameters.getItemCount - 1 do
      result.AddParameter(request.Parameters.VarName(i), request.Parameters.getVar(request.Parameters.VarName(i)));
    result.link;
  finally
    result.Free;
  end;
end;

function TFhirOperation.buildExpansionProfile(request: TFHIRRequest; manager: TFHIRNativeOperationEngine; params: TFhirParameters): TFHIRExpansionProfile;
var
  needSecure : boolean;
  exp : TFhirExpansionProfile;
  res : boolean;
begin
  res := false;
  {$IFNDEF FHIR2}
  exp := params.res['profile'] as TFHIRExpansionProfile;
  if exp <> nil then
    res := true
  else
    if params.str['profile'].StartsWith('http:') or params.str['profile'].StartsWith('https:') then
      exp := manager.getResourceByUrl(frtExpansionProfile, params.str['profile'], '', true, needSecure) as TFhirExpansionProfile
    else if params.str['profile'] <> '' then
      exp := manager.GetResourceById(request, 'ExpansionProfile', params.str['profile'], request.baseUrl, needSecure) as TFhirExpansionProfile
    else
  {$ENDIF}
     exp := nil;

  try
   if exp = nil then
     result := TFhirExpansionProfile.Create
   else
     result := exp.Clone;
   try
     if (not res) and (params.str['profile'] = 'http://www.healthintersections.com.au/fhir/expansion/no-details') then
       result.includeDefinition := true;
     if (params.str['_incomplete'] <> '') then
       result.limitedExpansion := StrToBoolDef(params.str['_incomplete'], false);
     if (params.str['limitedExpansion'] <> '') then
       result.limitedExpansion := StrToBoolDef(params.str['limitedExpansion'], false);
     if (params.str['displayLanguage'] <> '') then
       result.displayLanguage := params.str['displayLanguage'];
     if (params.str['includeDesignations'] <> '') then
       result.includeDesignations := StrToBoolDef(params.str['includeDesignations'], false);
     if (params.str['includeDefinition'] <> '') then
       result.includeDefinition := StrToBoolDef(params.str['includeDefinition'], false);
     if (params.str['activeOnly'] <> '') then
       result.activeOnly := StrToBoolDef(params.str['activeOnly'], false);
     if (params.str['excludeNested'] <> '') then
       result.excludeNested := StrToBoolDef(params.str['excludeNested'], false);
     if (params.str['excludeNotForUI'] <> '') then
       result.excludeNotForUI := StrToBoolDef(params.str['excludeNotForUI'], false);
     if (params.str['excludePostCoordinated'] <> '') then
       result.excludePostCoordinated := StrToBoolDef(params.str['excludePostCoordinated'], false);
     {$IFNDEF FHIR2}
     if (result.url = '') and not res then
       result.url := params.str['profile'];
     {$ENDIF}

     result.Link;
   finally
     result.free;
   end;
  finally
    if not res then
      exp.free;
  end;
end;

{ TFhirGenerateQAOperation }

function TFhirGenerateQAOperation.Name: String;
begin
  result := 'qa-edit';
end;

function TFhirGenerateQAOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirGenerateQAOperation.Types: TFhirResourceTypeSet;
begin
  result := ALL_RESOURCE_TYPES - [frtStructureDefinition];
end;

function TFhirGenerateQAOperation.HandlesRequest(request: TFHIRRequest): boolean;
begin
  result := inherited HandlesRequest(request) and (request.id <> '') and (request.SubId = '');
end;

function TFhirGenerateQAOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateQAOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirGenerateQAOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
begin
end;

{ TFhirHandleQAPostOperation }

function TFhirHandleQAPostOperation.Name: String;
begin
  result := 'qa-post';
end;

function TFhirHandleQAPostOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirHandleQAPostOperation.Types: TFhirResourceTypeSet;
begin
  result := ALL_RESOURCE_TYPES - [frtStructureDefinition];
end;

function TFhirHandleQAPostOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirHandleQAPostOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
begin
end;

function TFhirHandleQAPostOperation.isWrite: boolean;
begin
  result := true;
end;

{ TFhirQuestionnaireGenerationOperation }

function TFhirQuestionnaireGenerationOperation.Name: String;
begin
  result := 'questionnaire';
end;

function TFhirQuestionnaireGenerationOperation.owningResource: TFhirResourceType;
begin
  result := frtStructureDefinition;
end;

function TFhirQuestionnaireGenerationOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtStructureDefinition];
end;

function TFhirQuestionnaireGenerationOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirQuestionnaireGenerationOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  profile : TFHirStructureDefinition;
  op : TFhirOperationOutcome;
  resourceKey : integer;
  id, fid : String;
  builder : TQuestionnaireBuilder;
  questionnaire : TFHIRQuestionnaire;
  needSecure : boolean;
  ctxt : TFHIRValidatorContext;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, request.Session.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, request.CommandType), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource('StructureDefinition', request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        profile := nil;
        try
          // first, we have to identify the structure definition
          id := request.Id;
          if request.Id <> '' then // and it must exist, because of the check above
            profile := manager.GetResourceById(request, 'StructureDefinition', request.Id, request.baseUrl, needSecure) as TFhirStructureDefinition
          else if request.Parameters.VarExists('identifier') then
            profile := manager.GetResourceByURL(frtStructureDefinition, request.Parameters.getvar('identifier'), '', false, needSecure) as TFhirStructureDefinition
          else if (request.form <> nil) and request.form.hasParam('profile') then
            profile := LoadFromFormParam(request.Context, request.form.getparam('profile'), request.Lang) as TFHirStructureDefinition
          else if (request.Resource <> nil) and (request.Resource is TFHirStructureDefinition) then
            profile := request.Resource.Link as TFHirStructureDefinition
          else
            raise Exception.Create('Unable to find profile to convert (not provided by id, identifier, or directly)');

          profile.checkNoImplicitRules('QuestionnaireGeneration', 'profile');
          profile.checkNoModifiers('QuestionnaireGeneration', 'profile');

          if id <> '' then
          begin
            fid := request.baseUrl+'StructureDefinition/'+id+'/$questionnaire';
            questionnaire := manager.ServerContext.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
            questionnaire.checkNoImplicitRules('QuestionnaireGeneration', 'questionnaire');
            questionnaire.checkNoModifiers('QuestionnaireGeneration', 'questionnaire');
          end
          else
          begin
            fid := newGUIDUrn;
            questionnaire := nil;
          end;

          try
            if questionnaire = nil then
            begin
              builder := TQuestionnaireBuilder.Create;
              try
                builder.Profile := profile.link;
                builder.OnExpand := manager.FRepository.ExpandVS;
                builder.onLookupCode := manager.FRepository.LookupCode;
                builder.Context := request.Link;
                builder.onLookupReference := manager.LookupReference;
                builder.QuestionnaireId := fid;
                builder.Profiles := manager.ServerContext.ValidatorContext.Profiles.Link;
                builder.build;
                questionnaire := builder.questionnaire.Link;
                if id <> '' then
                  manager.ServerContext.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.dependencies);
              finally
                builder.Free;
              end;
            end;
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.Resource := questionnaire.Link;
          finally
            questionnaire.Free;
          end;
        finally
          profile.free;
        end;
        ctxt := TFHIRValidatorContext.Create;
        try
          ctxt.ResourceIdRule := risOptional;
          ctxt.IsAnyExtensionsAllowed := true;
          ctxt.OperationDescription := 'Produce Questionnaire';
          manager.ServerContext.Validator.validate(ctxt, response.Resource);
          op := manager.ServerContext.Validator.describe(ctxt);
        finally
          ctxt.Free;
        end;
        try
          if (op.hasErrors) then
          begin
            response.HTTPCode := 500;
            response.Message := 'Questionnaire Generation Failed';
            response.Resource.xmlId := 'src';
            op.containedList.Add(response.Resource.Link);
            response.Resource := op.link;
          end;
        finally
          op.Free;
        end;
      end;
    end;
    inc(iCount);
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirQuestionnaireGenerationOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirExpandValueSetOperation }

function TFhirExpandValueSetOperation.Name: String;
begin
  result := 'expand';
end;

function TFhirExpandValueSetOperation.owningResource: TFhirResourceType;
begin
  result := frtValueSet;
end;

function TFhirExpandValueSetOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtValueSet];
end;

function TFhirExpandValueSetOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirExpandValueSetOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  vs, dst : TFHIRValueSet;
  resourceKey : integer;
  url, cacheId, filter : String;
  profile : TFhirExpansionProfile;
  limit, count, offset : integer;
  params : TFhirParameters;
  needSecure : boolean;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        cacheId := '';
        params := makeParams(request);
        vs := nil;
        try
          // first, we have to identify the value set.
          if request.Id <> '' then // and it must exist, because of the check above
          begin
            vs := manager.GetResourceById(request, 'ValueSet', request.Id, request.baseUrl, needSecure) as TFHIRValueSet;
            cacheId := vs.url;
          end
          else if params.hasParameter('url') then
          begin
            url := params.str['url'];
            if (url.startsWith('ValueSet/')) then
              vs := manager.GetResourceById(request, 'ValueSet', url.substring(9), request.baseUrl, needSecure) as TFHIRValueSet
            else if (url.startsWith(request.baseURL+'ValueSet/')) then
              vs := manager.GetResourceById(request, 'ValueSet', url.substring(9), request.baseUrl, needSecure) as TFHIRValueSet
            else if not manager.ServerContext.TerminologyServer.isKnownValueSet(url, vs) then
              vs := manager.GetResourceByUrl(frtValueSet, request.Parameters.getvar('url'), request.Parameters.getvar('version'), false, needSecure) as TFHIRValueSet;
            cacheId := vs.url;
          end
          else if params.hasParameter('valueSet') then
            vs := params['valueSet'].Link as TFhirValueSet
          else if (request.Resource <> nil) and (request.Resource is TFHIRValueSet) then
            vs := request.Resource.Link as TFhirValueSet
          else if params.hasParameter('context') then
            raise Exception.Create('the "context" parameter is not yet supported')
          else
            raise Exception.Create('Unable to find value set to expand (not provided by id, identifier, or directly)');

          vs.checkNoImplicitRules('ExpandValueSet', 'ValueSet');
          vs.checkNoModifiers('ExpandValueSet', 'ValueSet');

          profile := buildExpansionProfile(request, manager, params);
          try
            filter := params.str['filter'];
            count := StrToIntDef(params.str['count'], 0);
            offset := StrToIntDef(params.str['offset'], 0);
            limit := StrToIntDef(params.str['_limit'], 0);

            dst := manager.ServerContext.TerminologyServer.expandVS(vs, cacheId, profile, filter, limit, count, offset);
            try
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := dst.Link;
              // response.categories.... no tags to go on this resource
            finally
              dst.free;
            end;
          finally
            profile.Free;
          end;
        finally
          vs.free;
          params.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirExpandValueSetOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/ValueSet-expand';
end;

function TFhirExpandValueSetOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirLookupCodeSystemOperation }

function TFhirLookupCodeSystemOperation.Name: String;
begin
  result := 'lookup';
end;

function TFhirLookupCodeSystemOperation.owningResource: TFhirResourceType;
begin
  result := frtCodeSystem;
end;

function TFhirLookupCodeSystemOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtCodeSystem];
end;

function TFhirLookupCodeSystemOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirLookupCodeSystemOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TFHIRLookupOpRequest;
  resp : TFHIRLookupOpResponse;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id <> '') then
        raise Exception.Create('Lookup does not take an identified resource');
      req := TFHIRLookupOpRequest.create();
      try
        if (request.Resource <> nil) and (request.Resource is TFHIRParameters) then
          req.load(request.Resource as TFHIRParameters)
        else
          req.load(request.Parameters);

        if (req.coding = nil) and (req.system <> '') then
        begin
          req.coding := TFhirCoding.Create;
          req.coding.system := req.system;
          req.coding.code := req.code;
          req.coding.version := req.version;
        end;
        if req.coding = nil then
          raise Exception.Create('Unable to find a code to lookup (need coding or system/code)');
        response.Body := '';
        response.LastModifiedDate := now;
        resp := TFHIRLookupOpResponse.Create;
        try
          try
            manager.ServerContext.TerminologyServer.lookupCode(req.coding, {$IFNDEF FHIR2}req.property_List{$ELSE} nil {$ENDIF}, resp);  // currently, we ignore the date
            response.Resource := resp.asParams;
            response.HTTPCode := 200;
            response.Message := 'OK';
          except
            on e : Exception do
            begin
              response.HTTPCode := 400;
              response.Message := 'Error';
              response.Resource := BuildOperationOutcome(request.Lang, e, IssueTypeCodeInvalid);
            end;
          end;
        finally
          resp.Free;
        end;
      finally
        req.Free;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirLookupCodeSystemOperation.formalURL: String;
begin
  {$IFNDEF FHIR2}
  result := 'http://hl7.org/fhir/OperationDefinition/CodeSystem-lookup';
  {$ELSE}
  result := 'http://hl7.org/fhir/OperationDefinition/ValueSet-lookup';
  {$ENDIF}
end;

function TFhirLookupCodeSystemOperation.isWrite: boolean;
begin
  result := false;
end;

{$IFNDEF FHIR2}
{ TFhirCodeSystemComposeOperation }

function TFhirCodeSystemComposeOperation.Name: String;
begin
  result := 'compose';
end;

function TFhirCodeSystemComposeOperation.owningResource: TFhirResourceType;
begin
  result := frtCodeSystem;
end;

function TFhirCodeSystemComposeOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtCodeSystem];
end;

function TFhirCodeSystemComposeOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirCodeSystemComposeOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TFHIRComposeOpRequest;
  resp : TFHIRComposeOpResponse;
  resourceKey : integer;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        req := TFHIRComposeOpRequest.Create;
        try
          if (request.Resource <> nil) and (request.Resource is TFHIRParameters) then
            req.load(request.Resource as TFHIRParameters)
          else
            req.load(request.Parameters);

          // first, we have to identify the Code System
          if request.Id <> '' then // and it must exist, because of the check above
            raise Exception.Create('Specifying a code system is not supported (only snomed-ct is supported)');
          if req.system <> 'http://snomed.info/sct' then
            raise Exception.Create('Only snomed-ct is supported)');
          // ok, it's snomed
          resp := TFHIRComposeOpResponse.Create;
          try
            try
              manager.ServerContext.TerminologyServer.composeCode(req, resp);
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := resp.asParams;
              response.HTTPCode := 200;
              response.Message := 'OK';
            except
              on e : Exception do
              begin
                response.HTTPCode := 400;
                response.Message := 'Error';
                response.Resource := BuildOperationOutcome(request.Lang, e, IssueTypeCodeInvalid);
              end;
            end;
          finally
            resp.Free;
          end;
        finally
          req.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirCodeSystemComposeOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/CodeSystem-compose';
end;

function TFhirCodeSystemComposeOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirSubsumesSystemOperation }

function TFhirSubsumesOperation.Name: String;
begin
  result := 'subsumes';
end;

function TFhirSubsumesOperation.owningResource: TFhirResourceType;
begin
  result := frtCodeSystem;
end;

function TFhirSubsumesOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtCodeSystem];
end;

function TFhirSubsumesOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirSubsumesOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TFHIRSubsumesOpRequest;
  resp : TFHIRSubsumesOpResponse;
  resourceKey : integer;
  cs : TFhirCodeSystem;
  cacheId : string;
  needSecure : boolean;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        req := TFHIRSubsumesOpRequest.create();
        try
          if (request.Resource <> nil) and (request.Resource is TFHIRParameters) then
            req.load(request.Resource as TFHIRParameters)
          else
            req.load(request.Parameters);

          // first, we have to identify the Code System
          if request.Id <> '' then // and it must exist, because of the check above
            cs := manager.GetResourceById(request, 'CodeSystem', request.Id, request.baseUrl, needSecure) as TFhirCodeSystem
          else if req.system <> '' then
            cs := manager.GetResourceByUrl(frtCodeSystem, req.system, req.version, false, needSecure) as TFhirCodeSystem
          else
            raise Exception.Create('No CodeSystem Identified (need a system parameter, or execute the operation on a CodeSystem resource');

          cacheId := cs.url;
          if (req.codingA = nil) and (req.codeA <> '') then
            req.codingA := TFhirCoding.Create(cs.url, req.codeA);
          if (req.codingB = nil) and (req.codeB <> '') then
            req.codingB := TFhirCoding.Create(cs.url, req.codeB);
          if req.codingA = nil then
            raise Exception.Create('No codeA or codingA parameter found');
          if req.codingB = nil then
            raise Exception.Create('No codeB or codingB parameter found');

          response.Body := '';
          response.LastModifiedDate := now;
          resp := TFHIRSubsumesOpResponse.Create;
          try
            try
              resp.outcome := manager.ServerContext.TerminologyServer.subsumes(cs, req.codingA, req.codingB);
              response.Resource := resp.asParams;
              response.HTTPCode := 200;
              response.Message := 'OK';
            except
              on e : Exception do
              begin
                response.HTTPCode := 400;
                response.Message := 'Error';
                response.Resource := BuildOperationOutcome(request.Lang, e, IssueTypeCodeInvalid);
              end;
            end;
          finally
            resp.Free;
          end;
        finally
          req.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirSubsumesOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/CodeSystem-subsumes';
end;

function TFhirSubsumesOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirObservationStatsOperation }

function TFhirObservationStatsOperation.Name: String;
begin
  result := 'stats';
end;

function TFhirObservationStatsOperation.owningResource: TFhirResourceType;
begin
  result := frtObservation;
end;

function TFhirObservationStatsOperation.resolvePatient(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; ref: String): integer;
var
  parts : TArray<String>;
begin
  parts := ref.Split(['/']);
  if length(parts) <> 2 then
    raise Exception.Create('Unable to understand the subject reference "'+ref+'"');
  if NOT manager.FindResource(parts[0], parts[1], false, result, request, nil, '') then
    result := 0;
end;


function TFhirObservationStatsOperation.resolveParameter(code: String): TObservationStatsParameter;
var
  i  : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TObservationStatsParameter, code);
  if i = -1 then
    raise Exception.Create('Unknown parameter '+code);
  result := TObservationStatsParameter(i);
end;


function TFhirObservationStatsOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtObservation];
end;

function TFhirObservationStatsOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirObservationStatsOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TFHIRStatsOpRequest;
  s : string;
  ose : TObservationStatsEvaluator;
  c : TFhirCoding;
  list : TAdvList<TFHIRResource>;
  res : TFHIRResource;
begin
  try
    manager.NotFound(request, response);
    req := TFHIRStatsOpRequest.create();
    try
      if (request.Resource <> nil) and (request.Resource is TFHIRParameters) then
        req.load(request.Resource as TFHIRParameters)
      else
        req.load(request.Parameters);

      ose := TObservationStatsEvaluator.create(manager.Connection);
      try
        ose.subject := req.subject;
        ose.subjectKey := resolvePatient(manager, request, req.subject);
        for s in req.codeList do
          ose.concepts.add(TFHIRCoding.Create(req.system, s));
        for c in req.codingList do
          ose.concepts.add(c.Link);
        if (ose.concepts.empty) then
          raise Exception.Create('no code or coding found');
        if (req.duration <> '') then
        begin
          ose.start := UniversalDateTime - DATETIME_HOUR_ONE * StrToFloat(req.duration);
          ose.finish := UniversalDateTime;
        end
        else if (req.period <> nil) then
        begin
          if (req.period.start = nil) then
            raise Exception.Create('Period.start is required');
          ose.start := req.period.start.AsUTCDateTime;
          if (req.period.end_ = nil) then
            raise Exception.Create('Period.end is required');
          ose.finish := req.period.end_.AsUTCDateTime;
        end
        else
          raise Exception.Create('duration or period is required');
        if (req.statisticList.Count = 0) then
          raise Exception.Create('at least one parameter is required');

        for s in req.statisticList do
          ose.parameters := ose.parameters + [resolveParameter(s)];

        ose.execute;
        if (req.include) then
        begin
          list := manager.loadResources(ose.Observations);
          try
            for res in list do
              ose.Resp.sourceList.Add(res.link as TFhirObservation)
          finally
            list.Free;
          end;
        end;

        response.Resource := ose.resp.asParams;
      finally
        ose.Free;
      end;

    finally
      req.Free;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirObservationStatsOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Observation-Stats';
end;

function TFhirObservationStatsOperation.isWrite: boolean;
begin
  result := false;
end;
{$ENDIF}

{ TFhirValidationOperation }

function TFhirValidationOperation.Name: String;
begin
  result := 'validate';
end;

function TFhirValidationOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirValidationOperation.Types: TFhirResourceTypeSet;
begin
  result := ALL_RESOURCE_TYPES;
end;

function TFhirValidationOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirValidationOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);

type TValidationOperationMode = (vomGeneral, vomCreate, vomUpdate, vomDelete);
var
  outcome : TFhirOperationOutcome;
  i : integer;
  profileId : String;
  profile : TFHirStructureDefinition;
  profiles : TValidationProfileSet;
  opDesc : string;
  result : boolean;
  needSecure : boolean;
  ctxt : TFHIRValidatorContext;
  function getParam(name : String) : String;
  var
    params : TFhirParameters;
  begin
    if request.Resource is TFhirParameters then
    begin
      params := request.Resource as TFhirParameters;
      if params.hasParameter(name) then
      begin
        result := (params.NamedParameter[name] as TFHIRPrimitiveType).StringValue;
        exit;
      end;
    end;
    result := request.Parameters.GetVar(name);
  end;
begin
  profileId := '';
  profile := nil;
  try
    profileId := getParam('profile');
    // reject mode - we don't know what to do with it
    if getParam('mode') <> '' then
      raise Exception.Create('Mode parameter is not (yet) supported');

    if StringStartsWith(ProfileId, 'http://localhost/StructureDefinition/') then
      profile := manager.GetResourceById(request, 'StructureDefinition', copy(ProfileId, 27, $FF), request.baseUrl, needSecure) as TFhirStructureDefinition
    else if StringStartsWith(ProfileId, 'StructureDefinition/') then
      profile := manager.GetResourceById(request, 'StructureDefinition', copy(ProfileId, 9, $FF), request.baseUrl, needSecure) as TFhirStructureDefinition
    else if StringStartsWith(ProfileId, request.baseUrl+'StructureDefinition/') then
      profile := manager.GetResourceById(request, 'StructureDefinition', copy(ProfileId, length(request.baseUrl)+9, $FF), request.baseUrl, needSecure) as TFhirStructureDefinition
    else if (profileId <> '') then
      profile := manager.GetResourceByURL(frtStructureDefinition, profileId, '', false, needSecure) as TFhirStructureDefinition;

    if Profile <> nil then
      opDesc := 'Validate resource '+request.id+' against profile '+profileId
    else if (profileId <> '') then
      raise Exception.Create('The profile "'+profileId+'" could not be resolved')
    else
      opDesc := 'Validate resource '+request.id;

    ctxt := TFHIRValidatorContext.Create;
    try
      ctxt.ResourceIdRule := risOptional;
      ctxt.IsAnyExtensionsAllowed := true;
      ctxt.OperationDescription := opDesc;
      if (request.Source <> nil) and not (request.Resource is TFhirParameters) then
      begin
        profiles := TValidationProfileSet.create(profile);
        try
          manager.ServerContext.Validator.validate(ctxt, request.Source, request.PostFormat, profiles)
        finally
          profiles.Free;
        end;
      end
      else
      begin
        if request.resource = nil then
          request.resource := manager.GetResourceById(request, request.ResourceName, request.Id, '', needSecure);
        profiles := TValidationProfileSet.create(profile);
        try
          manager.ServerContext.Validator.validate(ctxt, request.Resource, profiles);
        finally
          profiles.Free;
        end;
      end;
      outcome := manager.ServerContext.Validator.describe(ctxt);
    finally
      ctxt.Free;
    end;

    // todo: check version id integrity
    // todo: check version integrity

    response.Resource := outcome;
    result := true;
    for i := 0 to outcome.issueList.count - 1 do
      result := result and (outcome.issueList[i].severity in [IssueSeverityInformation, IssueSeverityWarning]);
    if result then
      response.HTTPCode := 200
    else
      response.HTTPCode := 400;
    if request.ResourceEnum <> frtAuditEvent then
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceEnum <> frtAuditEvent then
        manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;


function TFhirValidationOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/ValueSet-validate-code';
end;

function TFhirValidationOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirValueSetValidationOperation }

function TFhirValueSetValidationOperation.Name: String;
begin
  result := 'validate-code';
end;

function TFhirValueSetValidationOperation.owningResource: TFhirResourceType;
begin
  result := frtValueSet;
end;

function TFhirValueSetValidationOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtValueSet];
end;

function TFhirValueSetValidationOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirValueSetValidationOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirValueSetValidationOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  vs : TFHIRValueSet;
  resourceKey : integer;
  cacheId  : String;
  coded : TFhirCodeableConcept;
  coding : TFhirCoding;
  abstractOk : boolean;
  params, pout : TFhirParameters;
  needSecure : boolean;
  profile : TFhirExpansionProfile;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        cacheId := '';
        params := makeParams(request);
        try
          vs := nil;
          try
            // first, we have to identify the value set.
            if request.Id <> '' then // and it must exist, because of the check above
            begin
              vs := manager.GetResourceById(request, 'ValueSet', request.Id, request.baseUrl, needSecure) as TFHIRValueSet;
              cacheId := vs.url;
            end
            else if params.hasParameter('identifier') then
            begin
              if not manager.ServerContext.TerminologyServer.isKnownValueSet(params.str['identifier'], vs) then
                vs := manager.GetResourceByUrl(frtValueSet, params.str['identifier'], params.str['version'], false, needSecure) as TFHIRValueSet;
              cacheId := vs.url;
            end
            else if params.hasParameter('valueSet') then
              vs := (params.res['valueSet'] as TFhirValueSet).Link
            else if (request.Resource <> nil) and (request.Resource is TFHIRValueSet) then
              vs := request.Resource.Link as TFhirValueSet
            else
              vs := nil;
              // raise Exception.Create('Unable to find valueset to validate against (not provided by id, identifier, or directly)');

            coded := nil;
            try
              // ok, now we need to find the source code to validate
              if (request.form <> nil) and request.form.hasParam('coding') then
              begin
                coded := TFhirCodeableConcept.Create;
                coded.codingList.add(LoadDTFromFormParam(request.Context, request.form.getParam('coding'), request.lang, 'coding', TFhirCoding) as TFhirCoding)
              end
              else if (request.form <> nil) and request.form.hasParam('codeableConcept') then
                coded := LoadDTFromFormParam(request.Context, request.form.getParam('codeableConcept'), request.lang, 'codeableConcept', TFhirCodeableConcept) as TFhirCodeableConcept
              else if request.Parameters.VarExists('code') and request.Parameters.VarExists('system') then
              begin
                coded := TFhirCodeableConcept.Create;
                coding := coded.codingList.Append;
                coding.system := request.Parameters.GetVar('system');
                coding.version := request.Parameters.GetVar('version');
                coding.code := request.Parameters.GetVar('code');
                coding.display := request.Parameters.GetVar('display');
              end

              else if ((request.resource <> nil) and (request.Resource.ResourceType = frtParameters)) then
              begin
                params := request.Resource as TFhirParameters;
                if params.hasParameter('coding') then
                begin
                  coded := TFhirCodeableConcept.Create;
                  coded.codingList.Add(params['coding'].Link);
                end
                else if params.hasParameter('codeableConcept') then
                  coded := params['codeableConcept'].Link as TFhirCodeableConcept
                else if params.hasParameter('code') and params.hasParameter('system') then
                begin
                  coded := TFhirCodeableConcept.Create;
                  coding := coded.codingList.Append;
                  coding.system := TFHIRPrimitiveType(params['system']).StringValue;
                  if params.hasParameter('version') then
                    coding.version := TFHIRPrimitiveType(params['version']).StringValue;
                  coding.code := TFHIRPrimitiveType(params['code']).StringValue;
                  if params.hasParameter('display') then
                    coding.display := TFHIRPrimitiveType(params['display']).StringValue;
                end
                else
                  raise Exception.Create('Unable to find code to validate (params. coding | codeableConcept | code');
              end
              else
                raise Exception.Create('Unable to find code to validate (coding | codeableConcept | code');
              abstractOk := params.hasParameter('abstract') and TFHIRBoolean(params['abstract']).Value;

              if (coded = nil) then
                raise Exception.Create('Unable to find code to validate (coding | codeableConcept | code');

              if vs <> nil then
              begin
                vs.checkNoImplicitRules('ValueSetValidation', 'ValueSet');
                vs.checkNoModifiers('ValueSetValidation', 'ValueSet');
              end;

              profile := buildExpansionProfile(request, manager, params);
              try
                try
                  response.resource := manager.ServerContext.TerminologyServer.validate(vs, coded, profile, abstractOk);
                except
                  on e : Exception do
                  begin
                    pout := TFHIRParameters.create;
                    response.resource := pout;
                    pout.AddParameter('result', false);
                    pout.AddParameter('message', e.Message);
                    pout.AddParameter('cause', 'unknown');
                  end;
                end;
                response.HTTPCode := 200;
                response.Message := 'OK';
                response.Body := '';
                response.LastModifiedDate := now;
              finally
                profile.free;
              end;
            finally
              coded.Free;
            end;
          finally
            vs.free;
          end;
        finally
          params.free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirValueSetValidationOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-validate';
end;

{ TFhirPatientEverythingOperation }

function TFhirPatientEverythingOperation.Name: String;
begin
  result := 'everything';
end;

function TFhirPatientEverythingOperation.owningResource: TFhirResourceType;
begin
  result := frtPatient;
end;

function TFhirPatientEverythingOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtPatient];
end;

function TFhirPatientEverythingOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := CreateBaseDefinition(base);
  try
    result.{$IFDEF FHIR2}notes{$ELSE}comment{$ENDIF} := 'This server has little idea what a valid patient record is; it returns everything in the patient compartment, and any resource directly referred to from one of these';
    result.system := False;
    result.resourceList.AddItem('Patient');
    result.type_ := true;
    result.instance := true;
    with result.parameterList.Append do
    begin
      name := 'return';
      use := OperationParameterUseOut;
      min := '1';
      max := '1';
      documentation := 'Patient record as a bundle';
      type_ := {$IFNDEF FHIR2}AllTypesBundle {$ELSE}OperationParameterTypeBundle{$ENDIF};
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFhirPatientEverythingOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  bundle : TFHIRBundle;
  entry : TFHIRBundleEntry;
  includes : TReferenceList;
  id, link, base, sql, field : String;
  total : Integer;
  rkey : integer;
  reverse : boolean;
  wantsummary : TFHIRSummaryOption;
  title: string;
  keys : TKeyList;
  params : TParseMap;
  patient : TFHIRResource;
  prsr : TFHIRParserClass;
  needsObject : boolean;
  type_ : String;
begin
  try
    patient := nil;
    try
      // first, we have to convert from the patient id to a compartment id
      if manager.FindResource('Patient', request.Id, false, rkey, request, response, '') then
      begin
        request.compartmentId := request.Id;
        bundle := TFHIRBundle.Create(BundleTypeSearchset);
        includes := TReferenceList.create;
        keys := TKeyList.Create;
        params := TParseMap.Create('');
        try
//          bundle.base := request.baseUrl;
          if manager.FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], request.Session, 1, id, link, sql, title, base, total, wantSummary, request.strictSearch, reverse) then
            link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
          else
            id := manager.BuildSearchResultSet(0, request.Session, request.resourceName, params, request.baseUrl, request.compartments, request.compartmentId, nil, link, sql, total, wantSummary, request.strictSearch, reverse);
          bundle.total := inttostr(total);
          bundle.Tags['sql'] := sql;
          manager.chooseField(response.Format, wantsummary, request.Adaptor, field, prsr, needsObject);

          manager.FConnection.SQL := 'Select Ids.ResourceKey, Types.ResourceName, Ids.Id, VersionId, Secure, StatedDate, Name, Versions.Status, Tags, '+field+' from Versions, Ids, Sessions, SearchEntries, Types '+
              'where Ids.Deleted = 0 and SearchEntries.ResourceVersionKey = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey '+'and SearchEntries.ResourceKey = Ids.ResourceKey and Types.ResourceTypeKey = Ids.ResourceTypeKey and SearchEntries.SearchKey = '+id+' '+
              'order by SortValue ASC';
          manager.FConnection.Prepare;
          try
            manager.FConnection.Execute;
            while manager.FConnection.FetchNext do
            Begin
              entry := manager.AddResourceTobundle(bundle, request.secure, request.baseUrl, field, prsr, SearchEntryModeNull, false, type_);
              keys.Add(TKeyPair.create(type_, manager.FConnection.ColStringByName['ResourceKey']));

              if request.Parameters.VarExists('_include') then
                manager.CollectIncludes(request.session, includes, entry.resource, request.Parameters.GetVar('_include'));
              if (entry.resource.ResourceType = frtPatient) and (entry.resource.id = request.id) then
              begin
                if (patient = nil) then
                  patient := entry.resource.link
                else
                  raise Exception.Create('Multiple patient resources found in patient compartment');
              end;
            End;
          finally
            manager.FConnection.Terminate;
          end;

          // process reverse includes
//          if request.Parameters.VarExists('_reverseInclude') then
//            manager.CollectReverseIncludes(request.Session, includes, keys, request.Parameters.GetVar('_reverseInclude'), bundle, request, response, wantsummary);

//          //now, add the includes
//          if includes.Count > 0 then
//          begin
//            manager.FConnection.SQL := 'Select ResourceTypeKey, Ids.Id, VersionId, Secure, StatedDate, Name, Versions.Status, Tags, '+field+' from Versions, Sessions, Ids '+
//                'where Ids.Deleted = 0 and Ids.MostRecent = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey '+
//                'and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and '+includes.asSql+') order by ResourceVersionKey DESC';
//            manager.FConnection.Prepare;
//            try
//              manager.FConnection.Execute;
//              while manager.FConnection.FetchNext do
//                manager.AddResourceTobundle(bundle, baseUrlrequest.request.request., field, prsr);
//            finally
//              manager.FConnection.Terminate;
//            end;
//          end;

          if patient = nil then
            raise Exception.Create('No Patient resource found in patient compartment');
          bundle.deleteEntry(patient);
          bundle.entryList.Insert(0).resource := patient.Link;
          bundle.entryList[0].fullurl := AppendForwardSlash(request.baseUrl)+'Patient/'+patient.id;

          bundle.meta := TFhirMeta.Create;
          bundle.meta.lastUpdated := NowUTC;
          bundle.id := NewGuidURN;
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.Body := '';
          response.bundle := bundle.Link;
        finally
          params.free;
          includes.free;
          keys.Free;
          bundle.Free;
        end;
    end;
    finally
      patient.free;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, '', '', 0, request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirPatientEverythingOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Patient-everything';
end;

function TFhirPatientEverythingOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirGenerateDocumentOperation }

procedure TFhirGenerateDocumentOperation.addResource(manager: TFHIRNativeOperationEngine; secure : boolean; bundle: TFHIRBundle; source : TFHIRDomainResource; reference: TFhirReference; required: boolean; compartments : String);
var
  res : TFHIRResource;
  needSecure : boolean;
  entry : TFHIRBundleEntry;
  exists : boolean;
  url : String;
begin
  if reference = nil then
    exit;
  res := manager.getResourceByReference(source, reference.reference, compartments, true, needSecure);
  try
    if res <> nil then
    begin
      if needSecure and not secure then
      begin
        if required then
          raise ERestfulException.Create('TFhirGenerateDocumentOperation', 'Execute', 'This document contains resources labelled with a security tag that means this server will only send it if the connection is secure', 403, IssueTypeSuppressed);
      end
      else
      begin
        url := manager.ServerContext.FormalURLPlainOpen+'/'+res.fhirType+'/'+res.id;
        exists := false;
        for entry in bundle.entryList do
          if entry.fullUrl = url then
            exists := true;
        if not exists then
        begin
          entry := bundle.entryList.Append;
          entry.resource := res.Link;
          entry.fullUrl := manager.ServerContext.FormalURLPlainOpen+'/'+res.fhirType+'/'+res.id;
        end;
      end
    end
    else if required then
      raise Exception.Create('Unable to resolve reference '''+reference.reference+'''');
  finally
    res.Free;
  end;
end;

procedure TFhirGenerateDocumentOperation.addSections(manager: TFHIRNativeOperationEngine; secure : boolean; bundle: TFHIRBundle; composition : TFhirComposition; sections: TFhirCompositionSectionList; compartments : String);
var
  i, j : integer;
begin
  for i := 0 to sections.Count - 1 do
  begin
    for j := 0 to sections[i].entryList.Count - 1 do
      addResource(manager, secure, bundle, composition, sections[i].entryList[j], true, compartments);
    if (sections[i].hasSectionList) then
      addSections(manager, secure, bundle, composition, sections[i].sectionList, compartments);
  end;
end;

function TFhirGenerateDocumentOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := CreateBaseDefinition(base);
  try
    result.system := False;
    result.resourceList.AddItem('Composition');
    result.type_ := true;
    result.instance := true;
    with result.parameterList.Append do
    begin
      name := 'return';
      use := OperationParameterUseOut;
      min := '1';
      max := '1';
      documentation := 'Composition as a bundle (document)';
      type_ := {$IFNDEF FHIR2}AllTypesBundle {$ELSE}OperationParameterTypeBundle{$ENDIF};
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFhirGenerateDocumentOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  composition : TFhirComposition;
  bundle : TFhirBundle;
  resourceKey : integer;
  entry : TFhirBundleEntry;
  i, j : integer;
  needSecure : boolean;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if manager.FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments) then
      begin
        composition := manager.GetResourceByKey(resourceKey, needSecure) as TFhirComposition;
        try
          composition.checkNoImplicitRules('GenerateDocument', 'composition');
          composition.checkNoModifiers('GenerateDocument', 'composition');
          if needSecure and not request.secure then
            raise ERestfulException.Create('TFhirGenerateDocumentOperation', 'Execute', 'This document contains resources labelled with a security tag that means this server will only send it if the connection is secure', 403, IssueTypeSuppressed);

          bundle := TFhirBundle.Create(BundleTypeDocument);
          try
            bundle.id := copy(GUIDToString(CreateGUID), 2, 36).ToLower;
            bundle.meta := TFhirMeta.Create;
            bundle.meta.lastUpdated := NowUTC;
//            bundle.base := manager.ServerContext.FormalURLPlain;
            {$IFNDEF FHIR2}
            bundle.identifier := TFhirIdentifier.Create;
            bundle.identifier.system := 'urn:ietf:rfc:3986';
            bundle.identifier.value := NewGuidURN;
            {$ENDIF}
            entry := bundle.entryList.Append;
            entry.resource := composition.Link;
            entry.fullUrl := manager.ServerContext.FormalURLPlainOpen+'/Composition/'+composition.id;
            addResource(manager, request.secure, bundle, composition, composition.subject, true, request.compartments);
            addSections(manager, request.secure, bundle, composition, composition.sectionList, request.compartments);

            for i := 0 to composition.authorList.Count - 1 do
              addResource(manager, request.secure, bundle, composition, composition.authorList[i], false, request.compartments);
            for i := 0 to composition.attesterList.Count - 1 do
              addResource(manager, request.secure, bundle, composition, composition.attesterList[i].party, false, request.compartments);
            addResource(manager, request.secure, bundle, composition, composition.custodian, false, request.compartments);
            for i := 0 to composition.eventList.Count - 1 do
              for j := 0 to composition.eventList[i].detailList.Count - 1 do
                addResource(manager, request.secure, bundle, composition, composition.eventList[i].detailList[j], false, request.compartments);
            addResource(manager, request.secure, bundle, composition, composition.encounter, false, request.compartments);

            if request.Parameters.getvar('persist') = 'true' then
            begin
              request.ResourceName := bundle.FhirType;
              request.CommandType := fcmdUpdate;
              request.Id := bundle.id;
              request.Resource := bundle.link;
              manager.Execute(context, request, response);
            end
            else
            begin
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := bundle.Link;
            end;
          finally
            bundle.Free;
          end;
        finally
          composition.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirGenerateDocumentOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateDocumentOperation.Name: String;
begin
  result := 'document';
end;

function TFhirGenerateDocumentOperation.owningResource: TFhirResourceType;
begin
  result := frtComposition;
end;

function TFhirGenerateDocumentOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtComposition];
end;


{ TMatchingResourceList }

function TMatchingResourceList.GetEntry(iIndex: Integer): TMatchingResource;
begin
  result := TMatchingResource(ObjectByIndex[iIndex]);
end;

{ TFhirProcessClaimOperation }

function TFhirProcessClaimOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirProcessClaimOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : integer;
  params : TFhirParameters;
  claim : TFhirClaim;
  resp : TFhirClaimResponse;
  needSecure : boolean;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        params := makeParams(request);
        claim := nil;
        try
          // first, we have to identify the value set.
          if request.Id <> '' then // and it must exist, because of the check above
            claim := manager.GetResourceById(request, 'Claim', request.Id, request.baseUrl, needSecure) as TFhirClaim
//          else if request.Parameters.VarExists('identifier') then
//          begin
//            url := request.Parameters.getvar('identifier');
//            if (url.startsWith('ValueSet/')) then
//              vs := manager.GetValueSetById(request, url.substring(9), baseUrlrequest.request.request.request.request.)
//            else if (url.startsWith(baseURLrequest.request.request.request.request.request.+'ValueSet/')) then
//              vs := manager.GetValueSetById(request, url.substring(9), baseUrlrequest.request.request.request.request.)
//            else if not manager.FRepository.TerminologyServer.isKnownValueSet(url, vs) then
//              vs := manager.GetValueSetByIdentity(request.Parameters.getvar('identifier'), request.Parameters.getvar('version'));
//            cacheId := vs.url;
//          end
          else if (request.form <> nil) and request.form.hasParam('claim') then
            claim := LoadFromFormParam(request.Context, request.form.getparam('valueSet'), request.Lang) as TFhirClaim
          else if (request.Resource <> nil) and (request.Resource is TFHIRClaim) then
            claim := request.Resource.Link as TFHIRClaim
          else
            raise Exception.Create('Unable to find claim to process (not provided by id, identifier, or directly)');

          claim.checkNoImplicitRules('ProcessClaim', 'claim');
          claim.checkNoModifiers('ProcessClaim', 'claim');
          resp := manager.FRepository.GenerateClaimResponse(claim);
          try
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.Resource := resp.Link;
            // response.categories.... no tags to go on this resource
          finally
            resp.free;
          end;
        finally
          claim.free;
          params.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirProcessClaimOperation.isWrite: boolean;
begin
  result := true;
end;

function TFhirProcessClaimOperation.Name: String;
begin
  result := 'process';
end;

function TFhirProcessClaimOperation.owningResource: TFhirResourceType;
begin
  result := frtClaim;
end;

function TFhirProcessClaimOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtClaim];
end;

{ TFhirConceptMapTranslationOperation }

function TFhirConceptMapTranslationOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtConceptMap];
end;

function TFhirConceptMapTranslationOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

function TFhirConceptMapTranslationOperation.Name: String;
begin
  result := 'translate';
end;

function TFhirConceptMapTranslationOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirConceptMapTranslationOperation.owningResource: TFhirResourceType;
begin
  result := frtConceptMap;
end;

procedure TFhirConceptMapTranslationOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  cm : TLoadedConceptMap;
//  op : TFhirOperationOutcome;
//  resourceKey : integer;
  coded : TFhirCodeableConcept;
  coding : TFhirCoding;
//  abstractOk : boolean;
  params : TFhirParameters;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
        params := makeParams(request);
        try
          // we have to find the right concept map
          // it doesn't matter whether the value sets are actually defined or not
          if request.id <> '' then
            cm := manager.ServerContext.TerminologyServer.getConceptMapById(request.id)
          else
            cm := manager.ServerContext.TerminologyServer.getConceptMapBySrcTgt(params.str['valueset'], params.str['target']);
          if cm = nil then
            raise Exception.Create('Unable to find concept map to use');
          try
            // ok, now we need to find the source code to validate
            coded := nil;
            if params.hasParameter('coding') then
            begin
              coded := TFhirCodeableConcept.Create;
              coded.codingList.add(LoadDTFromParam(request.Context, params.str['coding'], request.lang, 'coding', TFhirCoding) as TFhirCoding)
            end
            else if params.hasParameter('codeableConcept') then
              coded := LoadDTFromParam(request.Context, params.str['codeableConcept'], request.lang, 'codeableConcept', TFhirCodeableConcept) as TFhirCodeableConcept
            else if params.hasParameter('code') and params.hasParameter('system') then
            begin
              coded := TFhirCodeableConcept.Create;
              coding := coded.codingList.Append;
              coding.system := params.str['system'];
              coding.version := params.str['version'];
              coding.code := params.str['code'];
              coding.display := params.str['display'];
            end
            else
              raise Exception.Create('Unable to find code to translate (coding | codeableConcept | code');
            try
              response.resource := manager.ServerContext.TerminologyServer.translate(cm, coded.codingList[0]);
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
            finally
              coded.Free;
            end;
          finally
            cm.free;
          end;
        finally
          params.free;
        end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirConceptMapTranslationOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/ConceptMap-translate';
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

{ TFhirGenerateSnapshotOperation }

function TFhirGenerateSnapshotOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirGenerateSnapshotOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  params : TFhirParameters;
  sdParam, sdBase : TFhirStructureDefinition;
  utils : TProfileUtilities;
  op : TFhirOperationOutcome;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      sdParam := nil;
      sdBase := nil;
      params := makeParams(request);
      try
        if params.hasParameter('profile') then
          sdParam := params['profile'] as TFhirStructureDefinition
        else if (request.Resource <> nil) and (request.Resource is TFhirStructureDefinition) then
          sdParam := request.Resource.Link as TFhirStructureDefinition
        else
          raise Exception.Create('Unable to find profile to generate snapshot for');

        sdParam.checkNoImplicitRules('GenerateSnapshot', 'profile');
        sdParam.checkNoModifiers('GenerateSnapshot', 'profile');
        if sdParam.baseDefinition <> '' then
        begin
          if not manager.ServerContext.ValidatorContext.Profiles.getProfileStructure(nil, sdParam.baseDefinition, sdBase) then
            raise Exception.Create('StructureDefinition base profile "'+sdParam.baseDefinition+'" not found');
        end
        else if params.hasParameter('base') then
        begin
          if not manager.ServerContext.ValidatorContext.Profiles.getProfileStructure(nil, params.str['base'], sdBase) then
            raise Exception.Create('Nominated base profile "'+params.str['base']+'" not found');
        end
        else
        begin
          if not manager.ServerContext.ValidatorContext.Profiles.getProfileStructure(nil, sdBase.baseDefinition, sdBase) then
            raise Exception.Create('Implicit base definition "'+sdBase.baseDefinition+'" not found');
        end;

        op := TFhirOperationOutcome.Create;
        utils := TProfileUtilities.create(manager.ServerContext.ValidatorContext.link, op.issueList.Link);
        try
          try
            utils.generateSnapshot(sdBase, sdParam, sdParam.url, sdParam.name);
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.Resource := sdParam.Link;
          except
            on e : exception do
            begin
              op.text := TFhirNarrative.Create;
              op.text.status := NarrativeStatusGenerated;
              op.text.div_ := TFhirXHtmlNode.Create('div');
              op.text.div_.AddText(e.Message);
              response.HTTPCode := 400;
              response.Message := 'Bad Request';
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := op.Link;
            end;
          end;
        finally
          op.Free;
          utils.Free;
        end;
      finally
        sdBase.Free;
        sdParam.free;
        params.Free;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirGenerateSnapshotOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateSnapshotOperation.Name: String;
begin
  result := 'snapshot';

end;

function TFhirGenerateSnapshotOperation.owningResource: TFhirResourceType;
begin
  result := frtStructureDefinition;
end;

function TFhirGenerateSnapshotOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtStructureDefinition];
end;

{ TFhirGenerateCDSHookOperation }

procedure TFhirGenerateCDSHookOperation.addNamingSystemInfo(ns: TFHIRNamingSystem; baseURL : String; resp: TCDSHookResponse);
var
  card : TCDSHookCard;
  b : TStringBuilder;
  cp : TFhirNamingSystemContact;
  {$IFNDEF FHIR2}
  uc : TFhirUsageContext;
  {$ENDIF}
  cc : TFhirCodeableConcept;
begin
  card := resp.addCard;
  card.addLink('Further Detail', baseURL+'/open/NamingSystem/'+ns.id);
  b := TStringBuilder.Create;
  try
    b.append('* Identifier System Name: '+ns.name+#13#10);
    if ns.publisher <> '' then
      b.append('* Publisher: '+ns.publisher+#13#10);
    if ns.responsible <> '' then
      b.append('* Responsible: '+ns.responsible+#13#10);
    if ns.type_ <> nil then
      b.append('* Type: '+gen(ns.type_)+#13#10);
    if ns.usage <> '' then
      b.append('* Usage Notes: '+ns.usage+#13#10);

    b.append(#13#10);

    if (ns.useContextList.Count > 0) {$IFNDEF FHIR2}or (ns.jurisdictionList.Count > 0){$ENDIF} then
    begin
      b.Append('Contexts of Use'#13#10#13#10);
      {$IFNDEF FHIR2}
      for uc in ns.useContextList do
        b.Append('* '+gen(uc.code)+':'+gen(uc.value)+#13#10);
      for cc in ns.jurisdictionList do
        b.Append('* Jurisdiction: '+gen(cc)+#13#10);
      {$ELSE}
      for cc in ns.useContextList do
        b.Append('* '+gen(cc)+#13#10);
      {$ENDIF}
      b.append(#13#10);
    end;

    if ns.contactList.Count > 0 then
    begin
      b.Append('Contacts'#13#10#13#10);
      for cp in ns.contactList do
        b.Append('* '+cp.name+#13#10);
      b.append(#13#10);
    end;
    card.detail := b.ToString;
  finally
    b.Free;
  end;
end;

procedure TFhirGenerateCDSHookOperation.addSystemCard(resp: TCDSHookResponse; name, publisher, responsible, type_, usage, realm : String);
var
  card : TCDSHookCard;
  b : TStringBuilder;
begin
  card := resp.addCard;
  b := TStringBuilder.Create;
  try
    b.append('* Identifier System Name: '+name+#13#10);
    if publisher <> '' then
      b.append('* Publisher: '+publisher+#13#10);
    if responsible <> '' then
      b.append('* Responsible: '+responsible+#13#10);
    if type_ <> '' then
      b.append('* Type: '+type_+#13#10);
    if usage <> '' then
      b.append('* Usage Notes: '+usage+#13#10);

    b.append(#13#10);

    if realm > '' then
    begin
      b.Append('Contexts of Use'#13#10#13#10);
      b.Append('* '+realm+#13#10);
      b.append(#13#10);
    end;

    card.detail := b.ToString;
  finally
    b.Free;
  end;

end;

function TFhirGenerateCDSHookOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

function TFhirGenerateCDSHookOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateCDSHookOperation.Name: String;
begin
  result := 'cds-hook';
end;

function TFhirGenerateCDSHookOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirGenerateCDSHookOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtNull];
end;

procedure TFhirGenerateCDSHookOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TCDSHookRequest;
  resp : TCDSHookResponse;
begin
  if not(request.Resource is TFhirParameters) then
    raise Exception.Create('Expected Parameters Resource for a cds-hook operation ');

  req := TCDSHookRequest.Create(request.Resource as TFhirParameters);
  try
    if req.activity = nil then
      raise Exception.Create('No activity found');

    resp := TCDSHookResponse.Create;
    try
      if TCDSHooks.isIdentifierView(req.activity) then
        executeIdentifierView(manager, request, req, resp)
      else if TCDSHooks.isCodeView(req.activity) then
        executeCodeView(manager, request, req, resp)
      else if TCDSHooks.isPatientView(req.activity) then
        executePatientView(manager, request, req, resp)
      else
        raise Exception.Create('Unsupported activity: '+req.activity.system+'##'+req.activity.code);
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Resource := resp.AsParams;
      response.LastModifiedDate := now;
    finally
      resp.Free;
    end;
  finally
    req.Free;
  end;
end;

procedure TFhirGenerateCDSHookOperation.executeIdentifierView(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; req: TCDSHookRequest; resp: TCDSHookResponse);
var
  systems : TAdvList<TFHIRResource>;
  id : TFhirIdentifier;
  r : TFHIRResource;
  card : TCDSHookCard;
  needSecure : boolean;
begin
  id := nil;
  for r in req.context do
    if r is TFHIRParameters then
      id := TFHIRParameters(r).param['identifier'].value as TFhirIdentifier;
  if id = nil then
    raise Exception.Create('No Code found for terminology-info');

  if (id.type_ <> nil) then
    manager.ServerContext.TerminologyServer.getCodeView(id.type_, resp);

  if (id.system <> '') then
  begin
    systems := Manager.getResourcesByParam(frtNamingSystem, 'value', id.system, needSecure);
    try
      for r in systems do
        addNamingSystemInfo(r as TFHIRNamingSystem, request.baseUrl, resp);
    finally
      systems.Free;
    end;
  end;
  if (id.system = 'urn:ietf:rfc:3986') then
    addSystemCard(resp, 'URI', '', 'W3C', '(any)', 'For when the identifier is any valid URI', '');

  for card in resp.cards do
  begin
    card.sourceLabel := manager.ServerContext.OwnerName;
    card.sourceURL := request.baseUrl;
    card.indicator := 'info';
  end;
end;

procedure TFhirGenerateCDSHookOperation.executePatientView(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; req: TCDSHookRequest; resp: TCDSHookResponse);
var
  pat : TFhirPatient;
  entry : TFhirBundleEntry;
begin
  pat := nil;

  if req.preFetchData <> nil then
    for entry in req.preFetchData.entryList do
      if (entry.resource <> nil) and (entry.resource is TFhirPatient) and (entry.resource.id = req.patient) then
        pat := entry.resource as TFhirPatient;
  processPatientView(manager, request, req, pat, resp);
end;

procedure TFhirGenerateCDSHookOperation.executeCodeView(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; req: TCDSHookRequest; resp: TCDSHookResponse);
var
  code : TFhirType;
  r : TFhirResource;
  card : TCDSHookCard;
begin
  code := nil;
  for r in req.context do
    if r is TFHIRParameters then
      code := TFHIRParameters(r).param['code'].value;
  if code = nil then
    raise Exception.Create('No Code found for terminology-info');
  if code is TFhirCoding then
    manager.ServerContext.TerminologyServer.getCodeView(code as TFHIRCoding, resp)
  else
    manager.ServerContext.TerminologyServer.getCodeView(code as TFHIRCodeableConcept, resp);
  for card in resp.cards do
  begin
    card.sourceLabel := manager.ServerContext.OwnerName;
    card.sourceURL := request.baseUrl;
    card.indicator := 'info';
  end;
end;

procedure TFhirGenerateCDSHookOperation.processPatientView(manager: TFHIRNativeOperationEngine; request: TFHIRRequest; req : TCDSHookRequest; context : TFHIRPatient; resp : TCDSHookResponse);
var
  patient : TFhirPatient;
  resourceKey : integer;
  matches, m : TMatchingResourceList;
  id : TFhirIdentifier;
  flag : TFhirFlag;
  i : integer;
  card : TCDSHookCard;
  needSecure : boolean;
begin
  patient := nil;
  try
    // first, do we know the patient?
    if manager.FindResource('Patient', context.id, false, resourceKey, request, nil, request.compartments) then
      patient := manager.GetResourceById(request, 'Patient', context.Id, request.baseUrl, needSecure) as TFHIRPatient
    else if context <> nil then
    begin
      matches := TMatchingResourceList.create;
      try
        for id in context.identifierList do
        begin
          m := manager.ResolveSearchId('Patient', request.compartmentId, request.compartments, request.baseURL, 'identifier='+id.system+'|'+id.value);
          try
            matches.AddAll(m);
          finally
            m.Free;
          end;
        end;
        if matches.Count = 1 then
          patient := manager.GetResourceByKey(matches[0].key, needSecure) as TFhirPatient;
      finally
        matches.Free;
      end;
    end;
    if (patient <> nil) and (request.secure or not needSecure)  then
    begin
      m := manager.ResolveSearchId('Flag', request.compartmentId, request.compartments, request.baseURL, 'patient='+patient.id);
      try
        for i := 0 to m.Count - 1 do
        begin
          flag := manager.GetResourceByKey(m[i].key, needSecure) as TFhirFlag;
          if (flag.status = FlagStatusActive) and (request.secure or not needSecure) then
          begin
            card := resp.addCard;
            card.indicator := 'info';
            if flag.author <> nil then
            begin
              card.sourceLabel := flag.author.display;
              card.sourceURL := flag.author.reference;
            end;
            if card.sourceLabel = '' then
              card.sourceLabel := manager.ServerContext.OwnerName;
            if card.sourceURL = '' then
              card.sourceURL := request.baseUrl;
            if flag.code.text <> '' then
              card.summary := flag.code.text
            else if flag.code.codingList.Count > 0 then
              card.summary := flag.code.codingList[0].display
          end;
        end;
      finally
        m.Free;
      end;
    end;
  finally
    patient.Free;
  end;
end;


{ TFhirGenerateTemplateOperation }

function TFhirGenerateTemplateOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirGenerateTemplateOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  profile : TFHirStructureDefinition;
  resourceKey : integer;
  id : String;
  builder : TProfileUtilities;
  template : TFHIRResource;
  narr : TFHIRNarrativeGenerator;
  needSecure : boolean;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, request.Session.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, request.CommandType), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource('StructureDefinition', request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        profile := nil;
        try
          // first, we have to identify the structure definition
          id := request.Id;
          if request.Id <> '' then // and it must exist, because of the check above
            profile := manager.GetResourceById(request, 'StructureDefinition', request.Id, request.baseUrl, needSecure) as TFhirStructureDefinition
          else if request.Parameters.VarExists('identifier') then
            profile := manager.GetResourceByURL(frtStructureDefinition, request.Parameters.getvar('identifier'), '', false, needSecure) as TFhirStructureDefinition
          else if (request.form <> nil) and request.form.hasParam('profile') then
            profile := LoadFromFormParam(request.Context, request.form.getparam('profile'), request.Lang) as TFHirStructureDefinition
          else if (request.Resource <> nil) and (request.Resource is TFHirStructureDefinition) then
            profile := request.Resource.Link as TFHirStructureDefinition
          else
            raise Exception.Create('Unable to find profile to convert (not provided by id, identifier, or directly)');

          profile.checkNoImplicitRules('GenerateTemplate', 'profile');
          profile.checkNoModifiers('GenerateTemplate', 'profile');

          template := nil;
          try
            builder := TProfileUtilities.create(manager.ServerContext.ValidatorContext.Link, nil);
            try
              template := builder.populateByProfile(profile);
              if template is TFhirDomainResource then
              begin
                narr := TFHIRNarrativeGenerator.create(manager.ServerContext.ValidatorContext.Link);
                try
                  narr.generate(template as TFhirDomainResource);
                finally
                  narr.Free;
                end;
              end;
            finally
              builder.Free;
            end;
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.Resource := template.Link;
          finally
            template.Free;
          end;
        finally
          profile.free;
        end;
      end;
    end;
    inc(iCount);
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirGenerateTemplateOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateTemplateOperation.Name: String;
begin
  result := 'generate-template';
end;

function TFhirGenerateTemplateOperation.owningResource: TFhirResourceType;
begin
  result := frtStructureDefinition;
end;

function TFhirGenerateTemplateOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtStructureDefinition];
end;

{ TFhirGenerateNarrativeOperation }

function TFhirGenerateNarrativeOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirGenerateNarrativeOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  narr : TFHIRNarrativeGenerator;
  r : TFHIRResource;
begin
  try
    r := request.Resource;
    if (r = nil) then
      raise Exception.Create('No resource found');
    if r is TFhirDomainResource then
    begin
      r.checkNoImplicitRules('GenerateNarrative', 'resource');
      TFhirDomainResource(r).checkNoModifiers('GenerateNarrative', 'resource');
      (r as TFhirDomainResource).text := nil;
      narr := TFHIRNarrativeGenerator.create(manager.ServerContext.ValidatorContext.Link);
      try
        narr.generate(r as TFhirDomainResource);
      finally
        narr.Free;
      end;
    end;
    response.HTTPCode := 200;
    response.Message := 'OK';
    response.Body := '';
    response.LastModifiedDate := now;
    response.Resource := r.Link;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirGenerateNarrativeOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGenerateNarrativeOperation.Name: String;
begin
  result := 'generate-narrative';
end;

function TFhirGenerateNarrativeOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirGenerateNarrativeOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtNull];
end;

{ TFhirConceptMapClosureOperation }

function TFhirConceptMapClosureOperation.checkName(request: TFHIRRequest; response: TFHIRResponse; var name: String) : boolean;
begin
  if request.Session.anonymous then
    result := IsGuid(name)
  else
  begin
    result := IsId(name);
    if result and not IsGUID(name) then
      name := inttostr(request.Session.UserKey)+'|'+name;
  end;
  if not result then
  begin
    response.HTTPCode := 400;
    response.Message := StringFormat('invalid closure name %s', [request.ResourceName+':'+request.Id]);
    response.Body := response.Message;
    response.Resource := BuildOperationOutcome(request.lang, response.Message);
  end;
end;

function TFhirConceptMapClosureOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirConceptMapClosureOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  params : TFhirParameters;
  p : TFhirParametersParameter;
  n, v : String;
  cm : TClosureManager;
  map : TFhirConceptMap;
  concepts : TAdvList<TFHIRCoding>;
  procedure errorResp(code : integer; message : String);
  begin
    response.HTTPCode := code;
    response.Message := message;
    response.Body := response.Message;
    response.Resource := BuildOperationOutcome(request.lang, response.Message);
  end;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      params := makeParams(request);
      cm := nil;
      map := nil;
      try
        n := params.str['name'];
        if checkName(request, response, n) then
        begin
          v := params.str['version'];
          if (v = '') and not params.hasParameter('concept') then
          begin
            v := manager.ServerContext.TerminologyServer.InitClosure(n);
            map := TFhirConceptMap.Create;
            response.resource := map.Link;
            map.id := NewGuidId;
            map.version := v;
            map.status := PublicationStatusActive;
            map.experimental := true; // for now
            map.date := NowUTC;
            map.name := 'Closure Table '+n+' initialized';
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
          end
          else
          begin
            if not manager.ServerContext.TerminologyServer.UseClosure(n, cm) then
              errorResp(404, StringFormat('closure name ''%s'' not known', [n]))
            else if (v <> '') and params.hasParameter('concept') then
             errorResp(404, StringFormat('closure ''%s'': cannot combine version and concept', [n]))
            else if (v <> '') and not StringIsInteger32(v) then
              errorResp(404, StringFormat('closure ''%s'': version %s is not valid', [n, v]))
            else
            begin
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              map := TFhirConceptMap.Create;
              response.resource := map.Link;
              map.id := NewGuidId;
              map.version := inttostr(cm.version);
              map.status := PublicationStatusActive;
              map.experimental := true; // for now
              map.date := NowUTC;
              map.name := 'Updates for Closure Table '+n;
              if (v <> '') then
              begin
                map.name := 'Replay for Closure Table '+n+' from version '+v;
                cm.rerun(manager.connection, map, StrToInt(v))
              end
              else
              begin
                map.name := 'Updates for Closure Table '+n;
                concepts := TAdvList<TFHIRCoding>.create;
                try
                  for p in params.parameterList do
                    if p.Name = 'concept' then
                      concepts.Add((p.value as TFHIRCoding).link);
                  cm.processConcepts(manager.Connection, concepts, map);
                finally
                  concepts.Free;
                end;
              end;
            end;
          end;
        end;
      finally
        params.free;
        cm.Free;
        map.Free;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirConceptMapClosureOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/ConceptMap-closure';
end;

function TFhirConceptMapClosureOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirConceptMapClosureOperation.Name: String;
begin
  result := 'closure';
end;

function TFhirConceptMapClosureOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirConceptMapClosureOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtNull];
end;

{ TFhirSuggestKeyWordsOperation }

function TFhirSuggestKeyWordsOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirSuggestKeyWordsOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('Not done yet');
end;

function TFhirSuggestKeyWordsOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirSuggestKeyWordsOperation.Name: String;
begin
  result := 'suggest-keywords';
end;

function TFhirSuggestKeyWordsOperation.owningResource: TFhirResourceType;
begin
  result := frtSearchParameter;
end;

function TFhirSuggestKeyWordsOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtSearchParameter];
end;

{ TFhirGetMetaDataOperation }

function TFhirGetMetaDataOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil
end;

procedure TFhirGetMetaDataOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  ok : boolean;
  meta : TFhirMeta;
  coding : TFhirCoding;
  uri : TFhirUri;
  params : TFHIRParameters;
begin
  try
    ok := true;
    if request.ResourceName = '' then
    begin
    // well, this operation is always allowed?
      manager.Connection.sql := 'Select Kind, Uri, Code, Display, (select count(*) from VersionTags where Tags.TagKey = VersionTags.TagKey and ResourceVersionKey in (select MostRecent from Ids)) as usecount from Tags where TagKey in (Select '+'TagKey from VersionTags where ResourceVersionKey in (select MostRecent from Ids)) order by Kind, Uri, Code'
    end
    else if request.Id = '' then
    begin
      if not manager.check(response, request.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdRead) and manager.ServerContext.ResConfig[request.ResourceName].Supported, 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
        ok := false
      else
        manager.Connection.sql := 'Select Kind, Uri, Code, Display, (select count(*) from VersionTags where Tags.TagKey = VersionTags.TagKey and ResourceVersionKey in (select MostRecent from Ids where ResourceTypeKey = '+inttostr(manager.ServerContext.ResConfig[request.ResourceName].Key)+')) as usecount  from Tags where TagKey in (Select TagKey from VersionTags where ResourceVersionKey in (select MostRecent from Ids where ResourceTypeKey = '+inttostr(manager.ServerContext.ResConfig[request.ResourceName].Key)+')) order by Kind, Uri, Code'
    end
    else if request.SubId = '' then
    begin
      if not manager.check(response, request.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdRead), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
         Ok := false
      else
        manager.Connection.sql := 'Select Kind, Uri, Code, VersionTags.Display, 1 as UseCount from Tags, VersionTags '+
        'where Tags.TagKey = VersionTags.TagKey and VersionTags.ResourceVersionKey in (Select ResourceVersionKey from Versions where ResourceVersionKey in (select MostRecent from Ids where Id = :id and ResourceTypeKey = '+inttostr(manager.ServerContext.ResConfig[request.ResourceName].Key)+')) order by Kind, Uri, Code'
    end
    else
    begin
      if not manager.check(response, request.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdVersionRead), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
        ok := false
      else
        manager.FConnection.sql := 'Select Kind, Uri, Code, VersionTags.Display, 1 as UseCount  from Tags, VersionTags '+
        'where Tags.TagKey = VersionTags.TagKey and VersionTags.ResourceVersionKey in (Select ResourceVersionKey from Versions where VersionId = :vid and ResourceKey in (select ResourceKey from Ids where Id = :id and ResourceTypeKey = '+inttostr(manager.ServerContext.ResConfig[request.ResourceName].Key)+')) order by Kind, Uri, Code';
    end;

    if ok then
    begin
      manager.Connection.Prepare;
      if request.Id <> '' then
      begin
        manager.Connection.BindString('id', request.Id);
        if request.SubId <> '' then
          manager.Connection.BindString('vid', request.SubId);
      end;
      manager.Connection.execute;
      meta := TFhirMeta.Create;
      try
        while manager.Connection.FetchNext do
        begin
          if TFHIRTagCategory(manager.Connection.ColIntegerByName['Kind']) = tcProfile then
          begin
            uri := meta.profileList.Append;
            uri.value := manager.Connection.ColStringByName['Code'];
            if request.Id = '' then
              uri.AddExtension('http://www.healthintersections.com.au/fhir/ExtensionDefinition/usecount', TFhirInteger.Create(manager.Connection.ColStringByName['UseCount']));
          end
          else
          begin
            coding := TFhirCoding.create;
            try
              coding.system := manager.Connection.ColStringByName['Uri'];
              coding.code := manager.Connection.ColStringByName['Code'];
              coding.display := manager.Connection.ColStringByName['Display'];
              if request.Id = '' then
                coding.AddExtension('http://www.healthintersections.com.au/fhir/ExtensionDefinition/usecount', TFhirInteger.Create(manager.Connection.ColStringByName['UseCount']));
              if TFHIRTagCategory(manager.Connection.ColIntegerByName['Kind']) = tcTag then
                meta.tagList.add(coding.Link)
              else
                meta.securityList.add(coding.Link)
            finally
              coding.Free;
            end;
          end;
        end;
        params := TFhirParameters.create;
        try
          if meta.tagList.count + meta.securityList.count + meta.profileList.count = 0 then
            params.AddParameter('return')
          else
            params.AddParameter('return', meta.link);
          response.resource := params.Link;
        finally
          params.Free;
        end;
      finally
        meta.Free;
      end;

      manager.Connection.terminate;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Body := '';
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message);
      recordStack(e);
 raise;
    end;
  end;
end;

function TFhirGetMetaDataOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-meta';
end;

function TFhirGetMetaDataOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirGetMetaDataOperation.Name: String;
begin
  result := 'meta';
end;

function TFhirGetMetaDataOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirGetMetaDataOperation.Types: TFhirResourceTypeSet;
begin
  result := ALL_RESOURCE_TYPES + [frtNull];
end;

{ TFhirAddMetaDataOperation }

function TFhirAddMetaDataOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirAddMetaDataOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  resourceVersionKey : Integer;
  currentResourceVersionKey : Integer;
  i : integer;
  tags : TFHIRTagList;
  t : string;
  ok : boolean;
  blob : TBytes;
  parser : TFHIRParser;
  deleted : boolean;
  meta : TFhirMeta;
begin
  meta := nil;

  currentResourceVersionKey := 0;
  try
    ok := true;
    if not manager.check(response, request.canWrite(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdUpdate), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      ok := false;
    if ok then
      manager.NotFound(request, response);
    if ok and not manager.FindResource(request.ResourceName, request.Id, true, resourceKey, request, response, request.compartments) then
      ok := false;
    if not ok then
      // nothing
    else
    begin
      CurrentResourceVersionKey := StrToInt(manager.Connection.Lookup('Ids', 'ResourceKey', inttostr(Resourcekey), 'MostRecent', '0'));
      if request.SubId <> '' then
      begin
        if not manager.check(response, manager.opAllowed(request.ResourceName, fcmdUpdate), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
          ok := false;
       if ok and Not manager.FindResourceVersion(request.ResourceName, request.Id, request.SubId, false, resourceVersionKey, request, response) then
         ok := false;
      end
      else
        resourceVersionKey := currentResourceVersionKey;
    end;

    if ok then
      if (request.Resource is TFHIRParameters) and (TFHIRParameters(request.Resource).hasParameter('meta')) and (TFHIRParameters(request.Resource)['meta'] is TFHIRMeta) then
        meta := TFHIRParameters(request.Resource)['meta'] as TFHIRMeta
      else
        ok := false;

    if ok then
    begin
      tags := TFHIRTagList.create;
      try
        manager.LoadTags(tags, ResourceKey);
        tags.readTags(meta);
        for i := 0 to tags.count - 1 do
          manager.FRepository.RegisterTag(tags[i], manager.FConnection);

        manager.FConnection.ExecSQL('delete from VersionTags where ResourceVersionKey = '+inttostr(resourceVersionKey));
        manager.CommitTags(tags, resourceVersionKey);

        manager.FConnection.SQL := 'Select Status, JsonContent from Versions where ResourceVersionKey = '+inttostr(resourceVersionKey);
        manager.FConnection.prepare;
        manager.FConnection.Execute;
        if not manager.FConnection.FetchNext then
          raise Exception.Create('Internal Error fetching current content');
        blob := manager.FConnection.ColBlobByName['JsonContent'];
        deleted := manager.FConnection.ColIntegerByName['Status'] = 2;
        manager.FConnection.Terminate;
        parser := MakeParser(request.Context, 'en', ffJson, blob, xppDrop);
        try
          manager.FConnection.SQL := 'Update Versions set XmlContent = :xc, XmlSummary = :xs, JsonContent = :jc, JsonSummary = :js, Tags = :tb where ResourceVersionKey = '+inttostr(resourceVersionKey);
          manager.FConnection.prepare;
          manager.FConnection.BindBlobFromBytes('tb', tags.json);
          response.resource := TFHIRParameters.create;
          if deleted then
          begin
            manager.FConnection.BindNull('xc');
            manager.FConnection.BindNull('jc');
            manager.FConnection.BindNull('xs');
            manager.FConnection.BindNull('js');
            meta := TFHIRMeta.Create;
            TFHIRParameters(response.Resource).AddParameter('return', meta);
            tags.writeTags(meta);
          end
          else
          begin
            if parser.resource.meta = nil then
              parser.resource.meta := TFHIRMeta.Create;
            tags.writeTags(parser.resource.meta);
            manager.FConnection.BindBlobFromBytes('xc', manager.EncodeResource(parser.Resource, true, soFull));
            manager.FConnection.BindBlobFromBytes('jc', manager.EncodeResource(parser.Resource, false, soFull));
            manager.markRedacted(parser.resource.meta);
            manager.FConnection.BindBlobFromBytes('xs', manager.EncodeResource(parser.Resource, true, soSummary));
            manager.FConnection.BindBlobFromBytes('js', manager.EncodeResource(parser.Resource, false, soSummary));
            manager.unmarkRedacted(parser.resource.meta);
            TFHIRParameters(response.Resource).AddParameter('return', parser.resource.meta.link);
          end;
          manager.FConnection.Execute;
          manager.FConnection.Terminate;
          if not deleted and (resourceVersionKey = currentResourceVersionKey) then
          begin
            manager.CreateIndexer;
            manager.FIndexer.execute(resourceKey, request.Id, parser.resource, tags);
          end;
        finally
          parser.free;
        end;
        response.HTTPCode := 200;
        response.Message := 'OK';
      finally
        tags.free;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, response.httpCode, t, response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, 500, t, e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirAddMetaDataOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-meta-add';
end;

function TFhirAddMetaDataOperation.isWrite: boolean;
begin
  result := true;
end;

function TFhirAddMetaDataOperation.Name: String;
begin
  result := 'meta-add';
end;

function TFhirAddMetaDataOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirAddMetaDataOperation.Types: TFhirResourceTypeSet;
begin
  result := ALL_RESOURCE_TYPES;
end;

{ TFhirDeleteMetaDataOperation }

function TFhirDeleteMetaDataOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirDeleteMetaDataOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  resourceVersionKey : Integer;
  currentResourceVersionKey : Integer;
  i : integer;
  tags : TFHIRTagList;
  t : string;
  ok : boolean;
  blob : TBytes;
  parser : TFHIRParser;
  deleted : boolean;
  meta : TFhirMeta;
  c : TFhirCoding;
begin
  meta := nil;
  currentResourceVersionKey := 0;
  try
    ok := true;
    if not manager.check(response, request.canWrite(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdUpdate), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      ok := false;
    if ok then
      manager.NotFound(request, response);
    if ok and not manager.FindResource(request.ResourceName, request.Id, true, resourceKey, request, response, request.compartments) then
      ok := false;
    if not ok then
      // nothing
    else
    begin
      CurrentResourceVersionKey := StrToInt(manager.Connection.Lookup('Ids', 'ResourceKey', inttostr(Resourcekey), 'MostRecent', '0'));
      if request.SubId <> '' then
      begin
        if not manager.check(response, manager.opAllowed(request.ResourceName, fcmdUpdate), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
          ok := false;
       if ok and Not manager.FindResourceVersion(request.ResourceName, request.Id, request.SubId, false, resourceVersionKey, request, response) then
         ok := false;
      end
      else
        resourceVersionKey := currentResourceVersionKey;
    end;

    if ok then
      if (request.Resource is TFHIRParameters) and (TFHIRParameters(request.Resource).hasParameter('meta')) and (TFHIRParameters(request.Resource)['meta'] is TFHIRMeta) then
        meta := TFHIRParameters(request.Resource)['meta'] as TFHIRMeta
      else
        ok := false;

    if ok then
      for c in meta.securityList do
        if ok and not manager.isOkToDeleteSecurityLabel(request, response, c) then
          ok := false;

    if ok then
    begin
      tags := TFHIRTagList.create;
      try
        manager.LoadTags(tags, ResourceKey);
        tags.removeTags(meta);
        for i := 0 to tags.count - 1 do
          manager.FRepository.RegisterTag(tags[i], manager.FConnection);

        manager.FConnection.ExecSQL('delete from VersionTags where ResourceVersionKey = '+inttostr(resourceVersionKey));
        manager.CommitTags(tags, resourceVersionKey);

        manager.FConnection.SQL := 'Select Status, JsonContent from Versions where ResourceVersionKey = '+inttostr(resourceVersionKey);
        manager.FConnection.prepare;
        manager.FConnection.Execute;
        if not manager.FConnection.FetchNext then
          raise Exception.Create('Internal Error fetching current content');
        blob := manager.FConnection.ColBlobByName['JsonContent'];
        deleted := manager.FConnection.ColIntegerByName['Status'] = 2;
        manager.FConnection.Terminate;
        parser := MakeParser(request.Context, 'en', ffJson, blob, xppDrop);
        try
          manager.FConnection.SQL := 'Update Versions set XmlContent = :xc, XmlSummary = :xs, JsonContent = :jc, JsonSummary = :js, Tags = :tb where ResourceVersionKey = '+inttostr(resourceVersionKey);
          manager.FConnection.prepare;
          manager.FConnection.BindBlobFromBytes('tb', tags.json);
          response.resource := TFHIRParameters.create;
          if deleted then
          begin
            manager.FConnection.BindNull('xc');
            manager.FConnection.BindNull('jc');
            manager.FConnection.BindNull('xs');
            manager.FConnection.BindNull('js');
            meta := TFHIRMeta.Create;
            TFHIRParameters(response.Resource).AddParameter('return', meta);
            tags.writeTags(meta);
          end
          else
          begin
            if parser.resource.meta = nil then
              parser.resource.meta := TFHIRMeta.Create;
            tags.writeTags(parser.resource.meta);
            manager.FConnection.BindBlobFromBytes('xc', manager.EncodeResource(parser.Resource, true, soFull));
            manager.FConnection.BindBlobFromBytes('jc', manager.EncodeResource(parser.Resource, false, soFull));
            manager.markRedacted(parser.resource.meta);
            manager.FConnection.BindBlobFromBytes('xs', manager.EncodeResource(parser.Resource, true, soSummary));
            manager.FConnection.BindBlobFromBytes('js', manager.EncodeResource(parser.Resource, false, soSummary));
            manager.unmarkRedacted(parser.resource.meta);
            TFHIRParameters(response.Resource).AddParameter('return', parser.resource.meta.link);
          end;
          manager.FConnection.Execute;
          manager.FConnection.Terminate;
          if not deleted and (resourceVersionKey = currentResourceVersionKey) then
          begin
            manager.CreateIndexer;
            manager.FIndexer.execute(resourceKey, request.Id, parser.resource, tags);
          end;
        finally
          parser.free;
        end;
        response.HTTPCode := 200;
        response.Message := 'OK';
      finally
        tags.free;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, response.httpCode, t, response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, 500, t, e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirDeleteMetaDataOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-meta-add';
end;

function TFhirDeleteMetaDataOperation.isWrite: boolean;
begin
  result := true;
end;

function TFhirDeleteMetaDataOperation.Name: String;
begin
  result := 'meta-delete';
end;

function TFhirDeleteMetaDataOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirDeleteMetaDataOperation.Types: TFhirResourceTypeSet;
begin
  result := ALL_RESOURCE_TYPES;
end;

{ TFhirDiffOperation }

function TFhirDiffOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirDiffOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  resourceVersionKey : Integer;
//  i : integer;
//  tags : TFHIRTagList;
//  t : string;
  ok : boolean;
  blob : TBytes;
  diff : TDifferenceEngine;
  parser : TFHIRParser;
  html : String;
//  deleted : boolean;
//  meta : TFhirMeta;
//  c : TFhirCoding;
begin
  try
    ok := true;
    if not manager.check(response, request.canRead(request.ResourceName) and manager.opAllowed(request.ResourceName, fcmdRead), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
      ok := false;
    if ok then
      manager.NotFound(request, response);
    if ok and not manager.FindResource(request.ResourceName, request.Id, false, resourceKey, request, response, request.compartments) then
      ok := false;
    if ok and not manager.check(response, request.Resource <> nil, 400, request.lang, 'A resource to compare must be posted', IssueTypeRequired) then
      ok := false;
    if not ok then
      // nothing
    else
    begin
      resourceVersionKey := StrToInt(manager.Connection.Lookup('Ids', 'ResourceKey', inttostr(Resourcekey), 'MostRecent', '0'));
      manager.FConnection.SQL := 'Select Status, JsonContent from Versions where ResourceVersionKey = '+inttostr(resourceVersionKey);
      manager.FConnection.prepare;
      manager.FConnection.Execute;
      if not manager.FConnection.FetchNext then
        raise Exception.Create('Internal Error fetching content');
      blob := manager.FConnection.ColBlobByName['JsonContent'];
      manager.FConnection.Terminate;
      parser := MakeParser(request.Context, 'en', ffJson, blob, xppDrop);
      try
        diff := TDifferenceEngine.create(manager.ServerContext.ValidatorContext.Link);
        try
          response.Resource := diff.generateDifference(parser.resource, request.Resource, html);
          response.HTTPCode := 200;
          response.Message := 'OK';
        finally
          diff.Free;
        end;
      finally
        parser.free;
      end;
    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, response.httpCode, 'diff', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, request.subid, 0, request.CommandType, request.Provenance, 500, 'diff', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirDiffOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-diff';
end;

function TFhirDiffOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirDiffOperation.Name: String;
begin
  result := 'diff';
end;

function TFhirDiffOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirDiffOperation.Types: TFhirResourceTypeSet;
begin
  result := ALL_RESOURCE_TYPES;
end;

{ TFhirCurrentTestScriptOperation }

function TFhirCurrentTestScriptOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirCurrentTestScriptOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
begin
  if request.Session = nil then
  begin
    response.HTTPCode := 404;
    response.Resource := BuildOperationOutcome('en', 'no Session, so no observed test script')
  end
  else
  begin
    if request.Session.TestScript = nil then
      request.Session.TestScript := TFhirTestScript.Create;
    request.Session.TestScript.id := inttostr(request.Session.Key);
    request.Session.TestScript.name := 'Observed Test Script for Session for '+request.Session.Name;
    request.Session.TestScript.status := PublicationStatusActive;
    request.Session.TestScript.publisher := manager.ServerContext.OwnerName;
    request.Session.TestScript.date := NowLocal;
    response.HTTPCode := 200;
    response.Resource := request.Session.TestScript.Link;
  end;
end;

function TFhirCurrentTestScriptOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirCurrentTestScriptOperation.Name: String;
begin
  result := 'current';
end;

function TFhirCurrentTestScriptOperation.owningResource: TFhirResourceType;
begin
  result := frtTestScript;

end;

function TFhirCurrentTestScriptOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtTestScript];
end;


{$IFNDEF FHIR2}
{ TFhirTransformOperation }

function TFhirTransformOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirTransformOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  params : TFHIRTransformOpRequest;
  rkey : integer;
  needSecure : boolean;
  lib : TAdvMap<TFHIRStructureMap>;
  map : TFHIRStructureMap;
  utils : TFHIRStructureMapUtilities;
  outcome : TFHIRObject;
//  params : TFhirParameters;
//  sdParam, sdBase : TFhirStructureDefinition;
//  utils : TProfileUtilities;
//  op : TFhirOperationOutcome;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      params := TFHIRTransformOpRequest.Create;
      try
        if (request.Resource <> nil) and (request.Resource is TFHIRParameters) then
          params.load(request.Resource.Link as TFHIRParameters)
        else
        begin
          params.load(request.Parameters);
          if (request.Resource <> nil) then
            params.content := request.Resource.Link;
        end;

        lib := manager.ServerContext.getMaps;
        try
          map := nil;
          if request.Id <> '' then
          begin
            if manager.FindResource('StructureMap', request.Id, false, rkey, request, response, '') then
            begin
              map := manager.GetResourceByKey(rkey, needSecure) as TFHIRStructureMap;
              if needSecure and not request.secure then
                raise ERestfulException.Create('TFhirGenerateDocumentOperation', 'Execute', 'This document contains resources labelled with a security tag that means this server will only send it if the connection is secure', 403, IssueTypeSuppressed);
            end;
          end
          else if params.source <> '' then
          begin
            map := lib[params.source].link;
            manager.check(response, map <> nil, 404 , manager.lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', manager.lang), [params.source]), IssueTypeNotFound);
          end
          else
            manager.check(response, false, 404, manager.lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', manager.lang), ['no id provided']), IssueTypeNotFound);
          if (map <> nil) then
          begin
            try
              outcome := manager.FFactory.makeByName(map.targetType);
              try
                utils := TFHIRStructureMapUtilities.Create(manager.ServerContext.Validator.Context.link, lib.Link, TServerTransformerServices.create(manager.ServerContext.link));
                try
                  try
                    utils.transform(nil, params.content, map, outcome);
                    response.HTTPCode := 200;
                    response.Message := 'OK';
                    response.Body := '';
                    response.LastModifiedDate := now;
                    if outcome is TFHIRResource then
                      response.Resource := (outcome as TFHIRResource).link
                    else
                      response.Resource := TFHIRCustomResource.createFromBase(manager.ServerContext.ValidatorContext, outcome);
                  except
                    on e : exception do
                      manager.check(response, false, 500, manager.lang, e.Message, IssueTypeProcessing);
                  end;
                finally
                  utils.Free;
                end;
              finally
                outcome.Free;
              end;
            finally
              map.Free;
            end;
          end;
        finally
          lib.Free;
        end;
      finally
        params.Free
      end;

    end;
    manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirTransformOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/StructureMap-transform';
end;

function TFhirTransformOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirTransformOperation.Name: String;
begin
  result := 'transform';

end;

function TFhirTransformOperation.owningResource: TFhirResourceType;
begin
  result := frtStructureMap;
end;

function TFhirTransformOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtStructureMap];
end;

{ TServerTransformerServices }

constructor TServerTransformerServices.create(ServerContext : TFHIRServerContext);
begin
  Inherited Create;
  FServerContext := ServerContext;
end;

destructor TServerTransformerServices.Destroy;
begin
  FServerContext.Free;
  inherited;
end;

procedure TServerTransformerServices.log(s: String);
begin
  // nothing right now
end;

function TServerTransformerServices.oid2Uri(oid: String): String;
begin
  result := ServerContext.oid2URI(oid);
end;

function TServerTransformerServices.translate(appInfo: TAdvObject; src: TFHIRCoding; conceptMapUrl: String): TFHIRCoding;
begin
  raise Exception.Create('Not Done Yet');
end;

{$ENDIF}

{$IFNDEF FHIR2}

{ TFhirActivateOperation }

function TFhirActivateOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirActivateOperation.Execute(context : TOperationContext; manager: TFHIRNativeOperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  rKey : integer;
  op : TFHIROperationOutcome;
  name : String;
  names : TStringList;
  ok : boolean;
  key : integer;
  cfg : TFHIRResourceConfig;
begin
  names := TStringList.create;
  try
    try
      manager.NotFound(request, response);
      if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) and
        manager.FindResource('ImplementationGuide', request.Id, false, rkey, request, response, '') then
      begin
        ok := manager.loadCustomResources(response, rkey, false, names);
        if ok then
        begin
          for name in names do
          begin
            key := manager.Connection.CountSQL('Select max(ResourceTypeKey) from Types')+1;
            manager.Connection.ExecSql('insert into Types (ResourceTypeKey, ResourceName, ImplementationGuide, Supported, LastId, IdGuids, IdClient, IdServer, cmdRead, cmdUpdate, cmdVersionRead, cmdDelete, cmdValidate, '+'cmdHistoryInstance, cmdHistoryType, cmdSearch, cmdCreate, cmdOperation, versionUpdates) values '+
              '('+inttostr(key)+', '''+name+''', '''+request.Id+''', 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)');
            cfg := TFHIRResourceConfig.Create();
            cfg.name := name;
            cfg.key := key;
            manager.ServerContext.ResConfig.Add(name, cfg);
          end;
        end;

        op := TFhirOperationOutcome.Create;
        try
          op.text := TFhirNarrative.Create;
          op.text.div_ := TFHIRXhtmlParser.parse(manager.lang, xppReject, [], '<div><p>The custom resources '+names.CommaText+' are now active</p></div>');
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.Body := '';
          response.LastModifiedDate := now;
          response.Resource := op.Link;
        finally
          op.Free;
        end;
      end;
      manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
    except
      on e: exception do
      begin
        manager.AuditRest(request.session, request.requestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
        recordStack(e);
        raise;
      end;
    end;
  finally
    names.free;
  end;
end;

function TFhirActivateOperation.formalURL: String;
begin
  result := 'not done yet';
end;

function TFhirActivateOperation.isWrite: boolean;
begin
  result := true;
end;

function TFhirActivateOperation.Name: String;
begin
  result := 'activate';
end;

function TFhirActivateOperation.owningResource: TFhirResourceType;
begin
  result := frtImplementationGuide;
end;

function TFhirActivateOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtImplementationGuide];
end;
{$ENDIF}

{ TFHIRRepository }

constructor TFHIRNativeStorageService.Create(DB: TKDBManager; AppFolder: String);
begin
  inherited Create;
  LoadMessages; // load while thread safe
  FAppFolder := AppFolder;
  FDB := DB;
  FLock := TCriticalSection.Create('fhir-store');
  FAudits := TFhirResourceList.Create;

  FClaimQueue := TFHIRClaimList.Create;
End;

procedure TFHIRNativeStorageService.Initialise(ini: TFHIRServerIniFile);
var
  conn: TKDBConnection;
  rn, fn : String;
  implGuides : TAdvStringSet;
  cfg : TFHIRResourceConfig;
begin
  ServerContext.SubscriptionManager := TSubscriptionManager.Create(ServerContext);
  ServerContext.SubscriptionManager.dataBase := FDB.Link;
  ServerContext.SubscriptionManager.Base := 'http://localhost/';
  ServerContext.SubscriptionManager.SMTPHost := ini.ReadString(voVersioningNotApplicable, 'email', 'Host', '');
  ServerContext.SubscriptionManager.SMTPPort := ini.ReadString(voVersioningNotApplicable, 'email', 'Port', '');
  ServerContext.SubscriptionManager.SMTPUsername := ini.ReadString(voVersioningNotApplicable, 'email', 'Username', '');
  ServerContext.SubscriptionManager.SMTPPassword := ini.ReadString(voVersioningNotApplicable, 'email', 'Password', '');
  ServerContext.SubscriptionManager.SMTPUseTLS := ini.ReadBool(voVersioningNotApplicable, 'email', 'Secure', false);
  ServerContext.SubscriptionManager.SMTPSender := ini.ReadString(voVersioningNotApplicable, 'email', 'Sender', '');
  ServerContext.SubscriptionManager.SMSAccount := ini.ReadString(voVersioningNotApplicable, 'sms', 'account', '');
  ServerContext.SubscriptionManager.SMSToken := ini.ReadString(voVersioningNotApplicable, 'sms', 'token', '');
  ServerContext.SubscriptionManager.SMSFrom := ini.ReadString(voVersioningNotApplicable, 'sms', 'from', '');
  ServerContext.SubscriptionManager.OnExecuteOperation := DoExecuteOperation;
  ServerContext.SubscriptionManager.OnExecuteSearch := DoExecuteSearch;
  ServerContext.SubscriptionManager.OnGetSessionEvent := ServerContext.SessionManager.GetSessionByKey;

  implGuides := TAdvStringSet.create;
  try
    conn := FDB.GetConnection('setup');
    try
      ServerContext.SessionManager.LastSessionKey := conn.CountSQL('Select max(SessionKey) from Sessions');
      FLastVersionKey := conn.CountSQL('Select Max(ResourceVersionKey) from Versions');
      ServerContext.TagManager.LastTagVersionKey := conn.CountSQL('Select Max(ResourceTagKey) from VersionTags');
      ServerContext.TagManager.LastTagKey := conn.CountSQL('Select Max(TagKey) from Tags');
      FLastSearchKey := conn.CountSQL('Select Max(SearchKey) from Searches');
      FLastResourceKey := conn.CountSQL('select Max(ResourceKey) from Ids');
      FLastEntryKey := conn.CountSQL('select max(EntryKey) from indexEntries');
      FLastCompartmentKey := conn.CountSQL('select max(ResourceCompartmentKey) from Compartments');
      FLastObservationKey := conn.CountSQL('select max(ObservationKey) from Observations');
      FLastObservationQueueKey := conn.CountSQL('select max(ObservationQueueKey) from ObservationQueue');
      conn.execSQL('Update Sessions set Closed = ' +DBGetDate(conn.Owner.Platform) + ' where Closed = null');

      conn.SQL := 'Select TagKey, Kind, Uri, Code, Display from Tags';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
      begin
        ServerContext.TagManager.add(conn.ColIntegerByName['TagKey'], TFHIRTagCategory(conn.ColIntegerByName['Kind']), conn.ColStringByName['Uri'], conn.ColStringByName['Code'], conn.ColStringByName['Display']).ConfirmedStored := true;
      end;
      conn.terminate;

      conn.SQL := 'Select * from Config';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
        if conn.ColIntegerByName['ConfigKey'] = 1 then
          ServerContext.SupportTransaction := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 2 then
          ServerContext.Bases.add(AppendForwardSlash(conn.ColStringByName['Value']))
        else if conn.ColIntegerByName['ConfigKey'] = 3 then
          ServerContext.SupportSystemHistory := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 4 then
          ServerContext.DoAudit := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 6 then
          ServerContext.SystemId := conn.ColStringByName['Value']
        else if conn.ColIntegerByName['ConfigKey'] = 7 then
          ServerContext.ResConfig[''].cmdSearch := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 8 then
        begin
          if conn.ColStringByName['Value'] <> FHIR_GENERATED_VERSION then
            raise Exception.Create('Database FHIR Version mismatch. The database contains DSTU'+conn.ColStringByName['Value']+' resources, but this server is based on DSTU'+FHIR_GENERATED_VERSION)
        end
        else if conn.ColIntegerByName['ConfigKey'] <> 5 then
          raise Exception.Create('Unknown Configuration Item '+conn.ColStringByName['ConfigKey']);

      conn.terminate;
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
      end;
      conn.terminate;
      conn.SQL :=
        'select ResourceTypeKey, max(CASE WHEN ISNUMERIC(RTRIM(Id) + ''.0e0'') = 1 THEN CAST(Id AS bigINT) ELSE 0 end) as MaxId from Ids group by ResourceTypeKey';
      conn.Prepare;
      conn.Execute;
      While conn.FetchNext do
      begin
        rn := getTypeForKey(conn.ColIntegerByName['ResourceTypeKey']);
        if StringIsInteger32(conn.ColStringByName['MaxId']) and (conn.ColIntegerByName['MaxId'] > ServerContext.ResConfig[rn].LastResourceId) then
          raise Exception.Create('Error in database - LastResourceId (' +
            inttostr(ServerContext.ResConfig[rn].LastResourceId) + ') < MaxId (' +
            inttostr(conn.ColIntegerByName['MaxId']) + ') found for ' +
            rn);
      end;
      conn.terminate;

      ServerContext.TagManager.crosslink;
      ServerContext.Indexes.ReconcileIndexes(conn);


      if ServerContext.TerminologyServer <> nil then
      begin
        // the order here is important: specification resources must be loaded prior to stored resources
        {$IFDEF FHIR4}
        fn := ChooseFile(IncludeTrailingPathDelimiter(FAppFolder) + 'definitions.json.zip', 'C:\work\org.hl7.fhir\build\publish\definitions.json.zip');
        {$ELSE}
        {$IFDEF FHIR3}
        fn := ChooseFile(IncludeTrailingPathDelimiter(FAppFolder) + 'definitions.json.zip', 'C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu3\build\publish\definitions.json.zip');
        {$ELSE} // fhir2
        fn := ChooseFile(IncludeTrailingPathDelimiter(FAppFolder) + 'validation.json.zip', 'C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu2\build\publish\validation.json.zip');
        {$ENDIF}
        {$ENDIF}

        logt('Load Validation Pack from ' + fn);
        ServerContext.ValidatorContext.LoadFromDefinitions(fn);
        if ServerContext.forLoad then
        begin
          logt('Load Custom Resources');
          LoadCustomResources(implGuides);
          logt('Load Store');
          LoadExistingResources(conn);
          logt('Check Definitions');
          checkDefinitions();
        end;
        logt('Load Subscription Queue');
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
  conn: TKDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL :=
      'insert into Sessions (SessionKey, UserKey, Created, Provider, Id, Name, Email, Expiry) values (:sk, :uk, :d, :p, :i, :n, :e, :ex)';
    conn.Prepare;
    conn.BindInteger('sk', session.key);
    conn.BindInteger('uk', StrToInt(session.User.id));
    conn.BindTimeStamp('d', DateTimeToTS(now));
    conn.BindInteger('p', integer(session.provider));
    conn.BindString('i', session.id);
    conn.BindString('n', session.name);
    conn.BindString('e', session.email);
    conn.BindTimeStamp('ex', DateTimeToTS(session.expires));
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
  conn : TKDBConnection;
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

procedure TFHIRNativeStorageService.recordOAuthLogin(id, client_id, scope, redirect_uri, state: String);
var
  conn : TKDBConnection;
begin
  conn := DB.GetConnection('oauth2');
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
end;

destructor TFHIRNativeStorageService.Destroy;
begin
  FAudits.free;
  FClaimQueue.free;
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
    request.session := ServerContext.SessionManager.CreateImplicitSession('server', true);
  context := TOperationContext.create;
  try
    storage := TFHIRNativeOperationEngine.Create('en', FServerContext, self.Link);
    try
      storage.Connection := FDB.GetConnection('fhir');
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

function TFHIRNativeStorageService.DoExecuteSearch(typekey: integer;
  compartmentId, compartments: String; params: TParseMap;
  conn: TKDBConnection): String;
var
  sp: TSearchProcessor;
  spaces: TFHIRIndexSpaces;
begin
  spaces := TFHIRIndexSpaces.Create(conn);
  try
    sp := TSearchProcessor.Create(ServerContext);
    try
      sp.typekey := typekey;
      sp.type_ := getTypeForKey(typekey);
      sp.compartmentId := compartmentId;
      sp.compartments := compartments;
      sp.baseURL := ServerContext.FormalURLPlainOpen; // todo: what?
      sp.lang := 'en';
      sp.params := params;
      sp.indexes := ServerContext.Indexes.Link;
      sp.countAllowed := false;
      sp.Connection := conn.link;
      sp.build;
      result := sp.filter;
    finally
      sp.free;
    end;
  finally
    spaces.free;
  end;
end;

function TFHIRNativeStorageService.ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList) : TFHIRValueSet;
var
  profile : TFhirExpansionProfile;
begin
  profile := TFhirExpansionProfile.Create;
  try
    profile.limitedExpansion := allowIncomplete;
    if (vs <> nil) then
      result := ServerContext.TerminologyServer.ExpandVS(vs, '', profile, '', dependencies, limit, count, offset)
    else
    begin
      if ServerContext.TerminologyServer.isKnownValueSet(ref.reference, vs) then
        result := ServerContext.TerminologyServer.ExpandVS(vs, ref.reference, profile, '', dependencies, limit, count, offset)
      else
      begin
        vs := ServerContext.TerminologyServer.getValueSetByUrl(ref.reference);
        if vs = nil then
          vs := ServerContext.TerminologyServer.getValueSetByid(ref.reference);
        if vs = nil then
          result := nil
        else
          result := ServerContext.TerminologyServer.ExpandVS(vs, ref.reference, profile, '', dependencies, limit, count, offset)
      end;
    end;
  finally
    profile.free;
  end;
end;

function TFHIRNativeStorageService.fetchOAuthDetails(key, status: integer; var client_id, name, redirect, state, scope: String): boolean;
var
  conn : TKDBConnection;
begin
  conn := DB.GetConnection('oauth2');
  try
    conn.SQL := 'select Client, Name, Scope, Redirect, ClientState from OAuthLogins, Sessions where OAuthLogins.SessionKey = '+inttostr(key)+' and Status = '+inttostr(status)+' and OAuthLogins.SessionKey = Sessions.SessionKey';
    conn.Prepare;
    conn.Execute;
    result := conn.FetchNext;
    if result then
    begin
      client_id := conn.ColStringByName['Client'];
      name := conn.ColStringByName['Name'];
      redirect := conn.ColStringByName['Redirect'];
      state := conn.ColStringByName['ClientState'];
      scope := conn.ColStringByName['Scope'];
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

function TFHIRNativeStorageService.FetchResourceCounts(comps : String): TStringList;
var
  conn : TKDBConnection;
  j : integer;
  cmp : String;
begin
  if (comps <> '') then
    cmp := ' and Ids.ResourceKey in (select ResourceKey from Compartments where TypeKey = '+inttostr(ServerContext.ResConfig['Patient'].key)+' and Id in ('+comps+'))'
  else
    cmp := '';

  result := TStringList.create;
  conn := DB.GetConnection('fhir-home-page');
  try
    conn.sql := 'select ResourceName, count(*) as Count from Ids,  Types where Ids.ResourceTypeKey = Types.ResourceTypeKey '+cmp+' group by ResourceName';
    conn.prepare;
    conn.execute;
    while conn.fetchNext do
    begin
      j := result.IndexOf(conn.ColStringByname['ResourceName']);
      if j = -1 then
        j := result.add(conn.ColStringByname['ResourceName']);
      result.objects[j] := TObject(conn.ColIntegerByName['Count']);
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

procedure TFHIRNativeStorageService.CloseFhirSession(key: integer);
var
  conn: TKDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL := 'Update Sessions set closed = :d where SessionKey = ' +
      inttostr(key);
    conn.Prepare;
    conn.BindTimeStamp('d', DateTimeToTS(UniversalDateTime));
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
  conn : TKDBConnection;
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
  conn : TKDBConnection;
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

procedure TFHIRNativeStorageService.RegisterConsentRecord(session: TFhirSession);
{$IFNDEF FHIR2}
var
  pc: TFhirConsent;
begin
  if session.PatientList.Count = 1 then
  begin
    pc := TFhirConsent.Create;
    try
      pc.status := ConsentStateCodesActive;
      with pc.categoryList.Append.codingList.append do
      begin
        system := 'http://hl7.org/fhir/consentcategorycodes';
        code := 'smart-on-fhir';
      end;
      pc.dateTime := NowUTC;
      pc.period := TFHIRPeriod.Create;
      pc.period.start := pc.dateTime.Link;
      pc.period.end_ := TDateAndTime.CreateUTC(session.expires);
      pc.patient := TFHIRReference.Create;
      pc.patient.reference := 'Patient/'+session.PatientList[0];
      // todo: do we have a reference for the consentor?
      // todo: do we have an identity for the organization?
  //    for
  //
  //    with pc.except_List.Append do
  //    begin
  //      type_ := ConsentExceptTypePermit;
  //      action := TFHIRCodeableConcept.Create;
  //      action.codingList.add(TFHIRCoding.Create('http://hl7.org/fhir/consentaction', 'read')));
  //    end;
  //  finally
  //
  //  end;
    finally
      pc.Free;
    end;
  end;
{$ELSE}
var
  ct: TFhirContract;
  s: String;
begin
  ct := TFhirContract.Create;
  try
    ct.issued := NowUTC;
    ct.applies := TFHIRPeriod.Create;
    ct.applies.start := ct.issued.Link;
    ct.applies.end_ := TDateAndTime.CreateUTC(session.expires);
    // need to figure out who this is...   ct.subjectList.Append.reference := '
    ct.type_ := TFhirCodeableConcept.Create;
    with ct.type_.codingList.append do
    begin
      code := 'disclosure';
      system := 'http://hl7.org/fhir/contracttypecodes';
    end;
    ct.subtypeList.append.text := 'Smart on FHIR Authorization';
    with ct.actionReasonList.append.codingList.append do
    begin
      code := 'PATRQT';
      system := 'http://hl7.org/fhir/v3/ActReason';
      Display := 'patient requested';
    end;
    with ct.actorList.append do
    begin
      roleList.append.text := 'Server Host';
      {$IFNDEF FHIR2}
      entity := TFhirReference.Create;
      entity.reference := 'Device/this-server';
      {$ENDIF}
    end;
    for s in session.scopes.Split([' ']) do
      with ct.actionList.append.codingList.append do
      begin
        code := UriForScope(s);
        system := 'urn:ietf:rfc:3986';
      end;
    QueueResource(ct, ct.issued);
  finally
    ct.free;
  end;
{$ENDIF}
end;

procedure TFHIRNativeStorageService.RegisterAuditEvent(session: TFhirSession; ip: String);
var
  se: TFhirAuditEvent;
  C: TFHIRCoding;
  p: TFhirAuditEventParticipant;
begin
  se := TFhirAuditEvent.Create;
  try
    se.event := TFhirAuditEventEvent.Create;
    se.event.type_ := TFHIRCoding.Create;
    C := se.event.type_;
    C.code := '110114';
    C.system := 'http://nema.org/dicom/dcid';
    C.Display := 'User Authentication';
    C := se.event.subtypeList.append;
    C.code := '110122';
    C.system := 'http://nema.org/dicom/dcid';
    C.Display := 'Login';
    se.event.action := AuditEventActionE;
    se.event.outcome := AuditEventOutcome0;
    se.event.dateTime := NowUTC;
    se.source := TFhirAuditEventSource.Create;
    se.source.site := ServerContext.OwnerName;
    se.source.identifier := TFhirIdentifier.Create;
    se.source.identifier.system := 'urn:ietf:rfc:3986';
    se.source.identifier.value := ServerContext.SystemId;
    C := se.source.type_List.append;
    C.code := '3';
    C.Display := 'Web Server';
    C.system := 'http://hl7.org/fhir/security-source-type';

    // participant - the web browser / user proxy
    p := se.participantList.append;
    p.userId := TFhirIdentifier.Create;
    p.userId.system := ServerContext.SystemId;
    p.userId.value := inttostr(session.key);
    p.altId := session.id;
    p.name := session.name;
    if (ip <> '') then
    begin
      p.network := TFhirAuditEventParticipantNetwork.Create;
      p.network.address := ip;
      p.network.type_ := NetworkType2;
      p.requestor := true;
    end;

    QueueResource(se, se.event.dateTime);
  finally
    se.free;
  end;
end;

procedure TFHIRNativeStorageService.RegisterTag(tag: TFHIRTag; conn: TKDBConnection);
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

procedure TFHIRNativeStorageService.doRegisterTag(tag: TFHIRTag; conn: TKDBConnection);
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

procedure TFHIRNativeStorageService.checkDefinitions;
var
  s, sx : string;
  c, t : integer;
  fpe : TFHIRExpressionEngine;
  sd : TFhirStructureDefinition;
  ed: TFhirElementDefinition;
  inv : TFhirElementDefinitionConstraint;
  td : TFHIRTypeDetails;
  expr : TFHIRExpressionNode;
begin
  s := '';
  c := 0;
  t := 0;
  fpe:= TFHIRExpressionEngine.create(ServerContext.ValidatorContext.Link);
  try
    for sd in ServerContext.ValidatorContext.Profiles.ProfilesByURL.Values do
      {$IFDEF FHIR2}
      if sd.constrainedType = '' then
      {$ENDIF}

      if sd.snapshot <> nil then
      begin
        for ed in sd.snapshot.elementList do
          for inv in ed.constraintList do
          begin
            sx := {$IFNDEF FHIR2} inv.expression {$ELSE} inv.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-expression') {$ENDIF};
            if (sx <> '') and not sx.contains('$parent') then
            begin
              inc(t);
              try
                expr := fpe.parse(sx);
                try
                  if sd.kind = StructureDefinitionKindResource then
                    td := fpe.check(nil, sd.id, ed.path, '', expr, false)
                  else
                    td := fpe.check(nil, 'DomainResource', ed.path, '', expr, false);
                  try
                    if (td.hasNoTypes) then
                      s := s + inv.key+' @ '+ed.path+' ('+sd.name+'): no possible result from '+sx + #13#10
                    else
                      inc(c);
                  finally
                    td.free;
                  end;
                finally
                  expr.Free;

                end;
              except
                on e : Exception do
                  s := s + inv.key+' @ '+ed.path+' ('+sd.name+'): exception "'+e.message+'" ('+sx+')' + #13#10;
              end;
            end;
          end;
        end;
  finally
    fpe.Free;
  end;
end;

procedure TFHIRNativeStorageService.checkRegisterTag(tag: TFHIRTag; conn: TKDBConnection);
begin
  if tag.ConfirmedStored then
    exit;

  if conn.CountSQL('select Count(*) from Tags where TagKey = '+inttostr(tag.key)) = 0 then
    doRegisterTag(tag, conn)
  else if conn.transactionId <> tag.TransactionId then
    tag.ConfirmedStored := true;
end;

procedure TFHIRNativeStorageService.RegisterTag(tag: TFHIRTag);
var
  conn: TKDBConnection;
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

function TFHIRNativeStorageService.resolveConcept(conn: TKDBConnection; c: TFHIRCoding): Integer;
begin
  if (c.system <> '') and (c.code <> '') then
    result := resolveConcept(conn, c.system, c.code)
  else
    result := 0;
end;

function TFHIRNativeStorageService.resolveConcept(conn: TKDBConnection; sys, code : String): Integer;
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

function TFHIRNativeStorageService.resolveReference(conn: TKDBConnection; ref: string): Integer;
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

procedure TFHIRNativeStorageService.RunValidateResource(i : integer; rtype, id: String; bufJson, bufXml: TAdvBuffer; b : TStringBuilder);
var
  ctxt : TFHIRValidatorContext;
  issue : TFHIROperationOutcomeIssue;
begin
  try
    ctxt := TFHIRValidatorContext.Create;
    try
      ServerContext.Validator.validate(ctxt, bufXml, ffXml);
      ServerContext.Validator.validate(ctxt, bufJson, ffJson);
      if (ctxt.Errors.Count = 0) then
        writeln(inttostr(i)+': '+rtype+'/'+id+': passed validation')
      else
      begin
        writeln(inttostr(i)+': '+rtype+'/'+id+': failed validation');
        b.Append(inttostr(i)+': '+'http://local.healthintersections.com.au:960/open/'+rtype+'/'+id+' : failed validation'+#13#10);
        for issue in ctxt.Errors do
          if (issue.severity in [IssueSeverityFatal, IssueSeverityError]) then
            b.Append('  '+issue.Summary+#13#10);
      end;
    finally
      ctxt.Free;
    end;
  except
    on e:exception do
    begin
      recordStack(e);
      writeln(inttostr(i)+': '+rtype+'/'+id+': exception validating: '+e.message);
      b.Append(inttostr(i)+': '+'http://fhir2.healthintersections.com.au/open/'+rtype+'/'+id+' : exception validating: '+e.message+#13#10);
    end;
  end;
end;

procedure TFHIRNativeStorageService.RunValidation;
var
  conn : TKDBConnection;
  bufJ, bufX : TAdvBuffer;
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
          bufJ := TAdvBuffer.create;
          bufX := TAdvBuffer.create;
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
    bufJ := TAdvBuffer.Create;
    try
      bufJ.AsUnicode := b.ToString;
      bufJ.SaveToFileName('c:\temp\validation.txt');
    finally
      bufJ.free;
    end;
  finally
    b.Free;
  end;
end;

procedure TFHIRNativeStorageService.Sweep;
var
  key : integer;
  d: TDateTime;
  list: TFhirResourceList;
  storage: TFHIRNativeOperationEngine;
  claim: TFhirClaim;
  resp: TFhirClaimResponse;
  conn: TKDBConnection;
begin
  key := 0;
  list := nil;
  claim := nil;
  d := UniversalDateTime;
  ServerContext.TerminologyServer.BackgroundThreadStatus := 'Sweeping Sessions';
  ServerContext.SessionManager.Sweep;
  FLock.Lock('sweep2');
  try
    if FAudits.Count > 0 then
    begin
      list := FAudits;
      FAudits := TFhirResourceList.Create;
    end;
    if (list = nil) and (FClaimQueue.Count > 0) then
    begin
      claim := FClaimQueue[0].Link;
      FClaimQueue.DeleteByIndex(0);
    end;
  finally
    FLock.Unlock;
  end;
  ServerContext.TerminologyServer.BackgroundThreadStatus := 'Sweeping Search';
  if FNextSearchSweep < d then
  begin
    conn := FDB.GetConnection('Sweep.search');
    try
      conn.SQL :=
        'Delete from SearchEntries where SearchKey in (select SearchKey from Searches where Date < :d)';
      conn.Prepare;
      conn.BindTimeStamp('d', DateTimeToTS(d - 0.3));
      conn.Execute;
      conn.terminate;

      conn.SQL := 'Delete from Searches where Date < :d';
      conn.Prepare;
      conn.BindTimeStamp('d', DateTimeToTS(d - 0.3));
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
    FNextSearchSweep := d + 10 * MINUTE_LENGTH;
  end;

  ServerContext.TerminologyServer.BackgroundThreadStatus := 'Sweeping - Closing';
  try
    if list <> nil then
    begin
      ServerContext.TerminologyServer.BackgroundThreadStatus := 'Sweeping - audits';
      storage := TFHIRNativeOperationEngine.Create('en', FServerContext, self.Link);
      try
        storage.Connection := FDB.GetConnection('fhir.sweep');
        try
          storage.storeResources(list, roSweep, false);
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
    if (claim <> nil) then
    begin
      ServerContext.TerminologyServer.BackgroundThreadStatus := 'Sweeping - claims';
      resp := GenerateClaimResponse(claim);
      try
        QueueResource(resp, resp.created);
      finally
        resp.free;
      end;
    end;
  finally
    list.free;
  end;
end;

procedure TFHIRNativeStorageService.UnStoreObservation(conn: TKDBConnection; key: integer);
begin
  inc(FLastObservationQueueKey);
  conn.ExecSQL('Insert into ObservationQueue (ObservationQueueKey, ResourceKey, Status) values ('+inttostr(FLastObservationQueueKey)+', '+inttostr(key)+', 0)');
end;

procedure TFHIRNativeStorageService.updateOAuthSession(id : String; state, key: integer);
var
  conn : TKDBConnection;
begin
  conn := DB.GetConnection('oauth2');
  try
    conn.ExecSQL('Update OAuthLogins set Status = '+inttostr(state)+', SessionKey = '+inttostr(Key)+', DateSignedIn = '+DBGetDate(conn.Owner.Platform)+' where Id = '''+SQLWrapString(id)+'''');
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

procedure TFHIRNativeStorageService.SeeResource(key, vkey: integer; id: string; needsSecure, created : boolean; resource: TFhirResource; conn: TKDBConnection; reload: Boolean; session: TFhirSession);
begin
  if (resource.ResourceType in [frtValueSet, frtConceptMap, frtStructureDefinition, frtQuestionnaire, frtSubscription]) and (needsSecure or ((resource.meta <> nil) and not resource.meta.securityList.IsEmpty)) then
    raise ERestfulException.Create('TFHIRNativeStorageService', 'SeeResource', 'Resources of type '+CODES_TFHIRResourceType[resource.ResourceType]+' are not allowed to have a security label on them', 400, IssueTypeBusinessRule);

  FLock.Lock('SeeResource');
  try
    if resource.ResourceType in [frtValueSet, frtConceptMap {$IFNDEF FHIR2}, frtCodeSystem {$ENDIF}] then
      ServerContext.TerminologyServer.SeeTerminologyResource(resource)
    else if resource.ResourceType = frtStructureDefinition then
      ServerContext.ValidatorContext.seeResource(resource as TFhirStructureDefinition)
    else if resource.ResourceType = frtQuestionnaire then
      ServerContext.ValidatorContext.seeResource(resource as TFhirQuestionnaire);
    FServerContext.SubscriptionManager.SeeResource(key, vkey, id, created, resource, conn, reload, session);
    FServerContext.QuestionnaireCache.clear(resource.ResourceType, id);
    if resource.ResourceType = frtValueSet then
      FServerContext.QuestionnaireCache.clearVS(TFHIRValueSet(resource).url);
    if resource.ResourceType = frtClaim then
      FClaimQueue.add(resource.Link);
    {$IFNDEF FHIR2}
    if resource.ResourceType = frtStructureMap then
      FServerContext.seeMap(TFHIRStructureMap(resource));
    {$ENDIF}
    if resource.ResourceType = frtNamingSystem then
      FServerContext.seeNamingSystem(key, TFHIRNamingSystem(resource));
    if not reload and (resource.ResourceType = frtObservation) then
      StoreObservation(conn, key);
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRNativeStorageService.StoreObservation(conn: TKDBConnection; key: integer);
begin
  inc(FLastObservationQueueKey);
  conn.ExecSQL('Insert into ObservationQueue (ObservationQueueKey, ResourceKey, Status) values ('+inttostr(FLastObservationQueueKey)+', '+inttostr(key)+', 1)');
end;

procedure TFHIRNativeStorageService.DropResource(key, vkey: integer; id: string; resource: String; indexer: TFhirIndexManager; conn: TKDBConnection);
var
  i: integer;
  aType : TFhirResourceType;
begin
  i := StringArrayIndexOfSensitive(CODES_TFhirResourceType, resource);
  if i > -1 then
  begin
    aType := TFhirResourceType(i);
    FLock.Lock('DropResource');
    try
      if aType in [frtValueSet, frtConceptMap] then
        ServerContext.TerminologyServer.DropTerminologyResource(aType, id)
      else if aType = frtStructureDefinition then
        ServerContext.ValidatorContext.Profiles.DropProfile(aType, id);
      FServerContext.SubscriptionManager.DropResource(key, vkey);
      FServerContext.QuestionnaireCache.clear(aType, id);
      for i := FClaimQueue.Count - 1 downto 0 do
        if FClaimQueue[i].id = id then
          FClaimQueue.DeleteByIndex(i);
    finally
      FLock.Unlock;
    end;
    if (aType = frtObservation) then
      UnstoreObservation(conn, key);
  end;
end;

procedure TFHIRNativeStorageService.SaveResource(res: TFhirResource; dateTime: TDateAndTime; origin : TFHIRRequestOrigin);
var
  request: TFHIRRequest;
  response: TFHIRResponse;
begin
  request := TFHIRRequest.Create(ServerContext.ValidatorContext.Link, origin, ServerContext.Indexes.Compartments.Link);
  try
    request.ResourceName := res.fhirType;
    request.CommandType := fcmdCreate;
    request.resource := res.Link;
    request.lastModifiedDate := dateTime.AsUTCDateTime;
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

procedure TFHIRNativeStorageService.ProcessObservation(conn: TKDBConnection; key: integer);
var
  rk : integer;
  deleted : boolean;
  obs : TFHIRObservation;
  cmp : TFhirObservationComponent;
  c, c1 : TFHIRCoding;
  subj, concept, subConcept : integer;
  dt, dtMin, dtMax : TDateTime;
begin
  conn.sql := 'Select ResourceKey, Status from ObservationQueue where ObservationQueueKey = '+inttostr(key);
  conn.prepare;
  conn.Execute;
  conn.FetchNext;
  rk := conn.ColIntegerByName['ResourceKey'];
  deleted := conn.ColIntegerByName['Status'] = 0;
  conn.Terminate;
  conn.ExecSQL('Delete from Observations where ResourceKey = '+inttostr(rk));
  if not deleted then
  begin
    obs := loadResource(conn, rk) as TFHIRObservation;
    try
      if (obs.subject <> nil) and (obs.subject.reference <> '') and not isAbsoluteUrl(obs.subject.reference) and
        (obs.effective <> nil) then
      begin
        subj := resolveReference(conn, obs.subject.reference);
        if (subj <> 0) then
        begin
          for c in obs.code.codingList do
          begin
            concept := resolveConcept(conn, c);
            if (concept <> 0) then
            begin
              if obs.effective is TFHIRDateTime then
              begin
                dt := (obs.effective as TFHIRDateTime).value.AsUTCDateTime;
                dtMin := (obs.effective as TFHIRDateTime).value.AsUTCDateTimeMin;
                dtMax := (obs.effective as TFHIRDateTime).value.AsUTCDateTimeMax;
              end
              else
              begin
                dt := 0;
                if (obs.effective as TFHIRPeriod).start = nil then
                  dtMin := 0
                else
                  dtMin := (obs.effective as TFHIRPeriod).start.AsUTCDateTimeMin;
                if (obs.effective as TFHIRPeriod).end_ = nil then
                  dtMax := MAXSQLDATE
                else
                  dtMax := (obs.effective as TFHIRPeriod).end_.AsUTCDateTimeMax;
              end;
              if (obs.value <> nil) then
                ProcessObservationValue(conn, rk, subj, concept, 0, dt, dtMin, dtMax, obs.value)
              else if (obs.dataAbsentReason <> nil) then
                ProcessObservationValue(conn, rk, subj, concept, 0, dt, dtMin, dtMax, obs.dataAbsentReason);
              for cmp in obs.componentList do
                for c1 in cmp.code.codingList do
                begin
                  subConcept := resolveConcept(conn, c1);
                  if (subConcept <> 0) then
                    if (cmp.value <> nil) then
                      ProcessObservationValue(conn, rk, subj, concept, subConcept, dt, dtMin, dtMax, cmp.value)
                    else if (cmp.dataAbsentReason <> nil) then
                      ProcessObservationValue(conn, rk, subj, concept, subConcept, dt, dtMin, dtMax, cmp.dataAbsentReason);
              end;
            end;
          end;
        end;
      end;

//             ' ObservationKey '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  // internal primary key
//       ' ResourceKey    '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // id of resource this came from
//       ' SubjectKey     '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+      // id of resource this observation is about
//       ' ConceptKey     '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+      // observation.code
//       ' SubConceptKey  '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+      // observation.code
//       ' DateTime       '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+        // observation.effectiveTime Stated (null = range)
//       ' DateTimeMin    '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // observation.effectiveTime Min
//       ' DateTimeMax    '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // observation.effectiveTime Max
//       ' Value          '+DBFloatType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+                 // stated value (if available)
//       ' ValueUnit      '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+             // stated units (if available)
//       ' Canonical      '+DBFloatType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+             // canonical value (if units)
//       ' CanonicalUnit  '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+         // canonical units (if canonical value)
//       ' ValueConcept   '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+          // if observation is a concept (or a data missing value)

    finally
      obs.Free;
    end;
  end;
end;

procedure TFHIRNativeStorageService.ProcessObservations;
var
  conn : TKDBConnection;
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
        processObservation(conn, key);
        conn.ExecSQL('Delete from ObservationQueue where ObservationQueueKey = '+inttostr(key));
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

procedure TFHIRNativeStorageService.ProcessObservationValue(conn: TKDBConnection; key, subj, concept, subconcept: integer; dt, dtMin, dtMax: TDateTime; value: TFHIRType);
begin
  if value is TFHIRQuantity then
    ProcessObservationValueQty(conn, key, subj, concept, subconcept, dt, dtMin, dtMax, value as TFhirQuantity)
  else if value is TFhirCodeableConcept then
    ProcessObservationValueCode(conn, key, subj, concept, subconcept, dt, dtMin, dtMax, value as TFhirCodeableConcept)
end;

procedure TFHIRNativeStorageService.ProcessObservationValueCode(conn: TKDBConnection; key, subj, concept, subconcept: integer; dt, dtMin, dtMax: TDateTime; value: TFHIRCodeableConcept);
var
  c : TFHIRCoding;
  ck : Integer;
begin
  for c in value.codingList do
  begin
    ck := resolveConcept(conn, c);
    if (ck <> 0) then
    begin
      conn.SQL := 'INSERT INTO Observations (ObservationKey, ResourceKey, SubjectKey, ConceptKey, SubConceptKey, DateTime, DateTimeMin, DateTimeMax, ValueConcept) VALUES' +
                   '                         (:key, :rkey, :subj, :concept, :subConcept, :dt, :dtMin, :dtMax, :val)';
      conn.Prepare;
      conn.BindInteger('key', nextObservationKey);
      conn.BindInteger('rkey', key);
      conn.BindInteger('subj', subj);
      conn.BindInteger('concept', concept);
      if subconcept = 0 then
        conn.BindNull('subConcept')
      else
        conn.BindInteger('subConcept', subconcept);
      if dt = 0 then
        conn.BindNull('dt')
      else
        conn.BindTimeStamp('dt', DateTimeToTS(dt));
      conn.BindTimeStamp('dtMin', DateTimeToTS(dtMin));
      conn.BindTimeStamp('dtMax', DateTimeToTS(dtMax));
      conn.BindInteger('val', ck);
      conn.Execute;
      conn.Terminate;
    end;
  end;
end;

procedure TFHIRNativeStorageService.ProcessObservationValueQty(conn: TKDBConnection; key, subj, concept, subconcept: integer; dt, dtMin, dtMax: TDateTime; value: TFHIRQuantity);
var
  val, cval : TSmartDecimal;
  upS, upC : TUcumPair;
  vU, cU : Integer;
begin
  if (value.value <> '') and (value.code <> '') and (value.system <> '') then
  begin
    val := TSmartDecimal.ValueOf(value.value);
    vu := resolveConcept(conn, value.system, value.code);
    if (value.system = 'http://unitsofmeasure.org') then
    begin
      upS := TUcumPair.Create(val, value.code);
      try
        upC := ServerContext.TerminologyServer.Ucum.getCanonicalForm(upS);
        cval := upC.Value;
        cu := resolveConcept(conn, 'http://unitsofmeasure.org', upC.UnitCode);
      finally
        upS.Free;
        upC.Free;
      end;
    end
    else
      Cu := 0;
    conn.SQL := 'INSERT INTO Observations (ObservationKey, ResourceKey, SubjectKey, ConceptKey, SubConceptKey, DateTime, DateTimeMin, DateTimeMax, Value, ValueUnit, Canonical, CanonicalUnit) VALUES' +
                 '                         (:key, :rkey, :subj, :concept, :subConcept, :dt, :dtMin, :dtMax, :v, :vu, :c, :cu)';
    conn.Prepare;
    conn.BindInteger('key', nextObservationKey);
    conn.BindInteger('rkey', key);
    conn.BindInteger('subj', subj);
    conn.BindInteger('concept', concept);
    if subconcept = 0 then
      conn.BindNull('subConcept')
    else
      conn.BindInteger('subConcept', subconcept);
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
    conn.Execute;
    conn.Terminate;
  end;
end;

procedure TFHIRNativeStorageService.ProcessSubscriptions;
begin
  FServerContext.SubscriptionManager.Process;
end;

function TFHIRNativeStorageService.ProfilesAsOptionList: String;
var
  i: integer;
  builder: TAdvStringBuilder;
  Profiles: TAdvStringMatch;
begin
  builder := TAdvStringBuilder.Create;
  try
    Profiles := ServerContext.ValidatorContext.Profiles.getLinks(false);
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

procedure TFHIRNativeStorageService.QueueResource(r: TFhirResource; dateTime: TDateAndTime);
begin
  QueueResource(r);
end;

procedure TFHIRNativeStorageService.QueueResource(r: TFhirResource);
begin
  FLock.Lock;
  try
    FAudits.add(r.Link);
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

function TFHIRNativeStorageService.NextResourceKeyGetId(aType: String; var id: string): integer;
begin
  FLock.Lock('NextResourceKey');
  try
    inc(FLastResourceKey);
    result := FLastResourceKey;
    inc(ServerContext.ResConfig[aType].LastResourceId);
    id := inttostr(ServerContext.ResConfig[aType].LastResourceId);
  finally
    FLock.Unlock;
  end;
end;

function TFHIRNativeStorageService.NextResourceKeySetId(aType: String; id: string): integer;
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
        ServerContext.ResConfig[aType].LastResourceId := i;
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

function TFHIRNativeStorageService.GenerateClaimResponse(claim: TFhirClaim)
  : TFhirClaimResponse;
var
  resp: TFhirClaimResponse;
begin
  resp := TFhirClaimResponse.Create;
  try
    resp.created := NowUTC;
    with resp.identifierList.append do
    begin
      system := ServerContext.Bases[0] + '/claimresponses';
      value := claim.id;
    end;
    resp.request := TFhirReference.Create;
    TFhirReference(resp.request).reference := 'Claim/' + claim.id;
//    resp.outcome := RemittanceOutcomeComplete;
    resp.disposition := 'Automatic Response';
//    resp.paymentAmount := {$IFDEF FHIR2}TFHIRQuantity{$ELSE}TFhirMoney{$ENDIF}.Create;
//    resp.paymentAmount.value := '0';
//    resp.paymentAmount.unit_ := '$';
//    resp.paymentAmount.system := 'urn:iso:std:4217';
//    resp.paymentAmount.code := 'USD';
    result := resp.Link;
  finally
    resp.free;
  end;
end;

function TFHIRNativeStorageService.GetNextKey(keytype: TKeyType; aType: string; var id: string): integer;
begin
  case keytype of
    ktResource:
      result := NextResourceKeyGetId(aType, id);
    ktEntries:
      result := NextEntryKey;
    ktCompartment:
      result := NextCompartmentKey;
  else
    raise Exception.Create('not done');
  end;
end;

function TFHIRNativeStorageService.Link: TFHIRNativeStorageService;
begin
  result := TFHIRNativeStorageService(Inherited Link);
end;


procedure TFHIRNativeStorageService.loadCustomResources(guides: TAdvStringSet);
var
  storage: TFHIRNativeOperationEngine;
  s : String;
  names : TStringList;
begin
  names := TStringList.create;
  try
    storage := TFHIRNativeOperationEngine.Create('en', FServerContext, self.Link);
    try
      storage.Connection := FDB.GetConnection('fhir');
      try
        for s in guides do
          if not storage.loadCustomResources(nil, s, true, names) then
            raise Exception.Create('Error Loading Custom resources');
        storage.Connection.Release;
      except
        on e : exception do
        begin
          storage.Connection.Error(e);
          raise;
        end;
      end;
    finally
      storage.free;
    end;
  finally
    names.Free;
  end;
end;

procedure TFHIRNativeStorageService.LoadExistingResources(conn: TKDBConnection);
var
  parser: TFHIRParser;
  mem: TBytes;
  i: integer;
  cback: TKDBConnection;
begin
  ServerContext.TerminologyServer.Loading := true;
  conn.SQL :=
    'select Ids.ResourceKey, Versions.ResourceVersionKey, Ids.Id, Secure, JsonContent from Ids, Types, Versions where '
    + 'Versions.ResourceVersionKey = Ids.MostRecent and ' +
    'Ids.ResourceTypeKey = Types.ResourceTypeKey and ' +
    '(Types.ResourceName = ''ValueSet'' or Types.ResourceName = ''CodeSystem'' or Types.ResourceName = ''ConceptMap'' or '+
    'Types.ResourceName = ''StructureDefinition'' or Types.ResourceName = ''Questionnaire'' or Types.ResourceName = ''StructureMap'' or Types.ResourceName = ''Subscription'') and Versions.Status < 2';
  conn.Prepare;
  try
    cback := FDB.GetConnection('load2');
    try
      i := 0;
      conn.Execute;
      while conn.FetchNext do
      begin
        inc(i);
        mem := conn.ColBlobByName['JsonContent'];

        parser := MakeParser(ServerContext.Validator.Context, 'en', ffJson, mem, xppDrop);
        try
          SeeResource(conn.ColIntegerByName['ResourceKey'],
            conn.ColIntegerByName['ResourceVersionKey'],
            conn.ColStringByName['Id'],
            conn.ColIntegerByName['Secure'] = 1,
            false, parser.resource, cback, true, nil);
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
  ServerContext.TerminologyServer.Loading := false;
end;

function TFHIRNativeStorageService.loadResource(conn: TKDBConnection; key: integer): TFhirResource;
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
    raise Exception.Create('unable to find resource '+inttostr(key));
  mem := conn.ColBlobByName['JsonContent'];
  parser := MakeParser(ServerContext.Validator.Context, 'en', ffJson, mem, xppDrop);
  try
    result := parser.resource.Link;
  finally
    parser.Free;
  end;
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
        result := prov.getDisplay(code, '');
    finally
      prov.free;
    end;
  except
    result := '';
  end;
end;


end.
