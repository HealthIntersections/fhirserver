unit FHIROperation;

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

{
todo:

Grahame, I see you don't respond to either of the following:
http://hl7connect.healthintersections.com.au/svc/fhir/condition/search?subject=patient/350
http://hl7connect.healthintersections.com.au/svc/fhir/condition/search?subject=patient/@350

cross resource search
ucum search

To check:

    procedure ExecuteTransaction(upload : boolean; request: TFHIRRequest; response : TFHIRResponse);

  TFhirGenerateQAOperation = class (TFHIROperation)
  TFhirHandleQAPostOperation = class (TFHIROperation)
  TFhirQuestionnaireGenerationOperation = class (TFHIROperation)
  TFhirExpandValueSetOperation = class (TFHIROperation)
  TFhirValueSetValidationOperation = class (TFHIROperation)
  TFhirPatientEverythingOperation = class (TFHIROperation)
  TFhirGenerateDocumentOperation = class (TFHIROperation)
  TFhirValidationOperation = class (TFHIROperation)
}

uses
  SysUtils, Classes,
  RegExpr, KDate, HL7V2DateSupport, DateAndTime, ParseMap, KCritSct, TextUtilities, ZLib,
  DateSupport, StringSupport, EncodeSupport, GuidSupport, BytesSupport,
  KDBManager, KDBDialects,
  AdvObjects, AdvIntegerObjectMatches, AdvMemories, AdvBuffers, AdvVclStreams, AdvStringObjectMatches, AdvStringMatches, AdvGenerics, AdvExceptions,
  AdvStringBuilders, AdvObjectLists, AdvNames, AdvXmlBuilders,

  FHIRBase, FHIRSupport, FHIRResources, FHIRConstants, FHIRTypes, FHIRParserBase,
  FHIRParser, FHIRUtilities, FHIRLang, FHIRIndexManagers, FHIRValidator, FHIRValueSetExpander, FHIRTags, FHIRDataStore,
  FHIRServerConstants, FHIRServerUtilities, NarrativeGenerator, FHIRProfileUtilities, FHIRNarrativeGenerator,
  ServerValidator, QuestionnaireBuilder, SearchProcessor, ClosureManager;

const
  MAGIC_NUMBER = 941364592;

  CURRENT_FHIR_STORAGE_VERSION = 2;

  RELEASE_DATE = '20131103';

type
  TCreateIdState = (idNoNew, idMaybeNew, idIsNew);

  TKeyPair = class (TAdvObject)
  private
    type_ : TFhirResourceType;
    key : string;
  public
    constructor create(t_ : TFhirResourceType; key : string);
  end;

  TKeyList = class (TAdvList<TKeyPair>)
  private
  public
    function forType(t_: TFhirResourceType) : String;
    function forAll: String;
  end;

  TFHIRTransactionEntryState = (tesIgnore, tesRead, tesCreate, tesUpdate, tesDelete);

  TFHIRTransactionEntry = class (TAdvName)
  private
    state : TFHIRTransactionEntryState;
    id : String;
    originalId : String;
    key : integer;
    resType : TFhirResourceType;
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

  TPopulateConformanceEvent = procedure (sender : TObject; conf : TFhirConformance) of object;

  TFhirOperationManager = class;

  TFhirOperation = {abstract} class (TAdvObject)
  protected
    function CreateBaseDefinition(base : String) : TFHIROperationDefinition;
    function isWrite : boolean; virtual;
    function owningResource : TFhirResourceType; virtual; // for security purposes
    function makeParams(request : TFHIRRequest) : TFhirParameters;
  public
    function Name : String; virtual;
    function Types : TFhirResourceTypeSet; virtual;
    function HandlesRequest(request : TFHIRRequest) : boolean; virtual;
    function CreateDefinition(base : String) : TFHIROperationDefinition; virtual;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); virtual;
    function formalURL : String; virtual;
  end;

  TFhirOperationManager = class (TAdvObject)
  private
    FRepository : TFHIRDataStore;
    FConnection : TKDBConnection;
    FFactory : TFHIRFactory;
    FIndexer : TFHIRIndexManager;
    FOperations : TAdvObjectList;
    FLang : String;
    FTestServer : Boolean;
    FOwnerName : String;

    FSpaces: TFHIRIndexSpaces;
    FOnPopulateConformance : TPopulateConformanceEvent;

    procedure checkNotRedacted(meta : TFhirMeta; msg : String);
    procedure markRedacted(meta : TFhirMeta);
    procedure unmarkRedacted(meta : TFhirMeta);

    procedure chooseField(aFormat : TFHIRFormat; summary : TFHIRSummaryOption; out fieldName : String; out comp : TFHIRParserClass; out needsObject : boolean); overload;
    function opAllowed(resource : TFHIRResourceType; command : TFHIRCommandType) : Boolean;
    function FindSavedSearch(const sId : String; Session : TFHIRSession; typeKey : integer; var id, link, sql, title, base : String; var total : Integer; var summaryStatus : TFHIRSummaryOption): boolean;
    function BuildSearchResultSet(typekey : integer; session : TFHIRSession; aType : TFHIRResourceType; params : TParseMap; baseURL, compartments, compartmentId : String; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption):String;
    function BuildHistoryResultSet(request: TFHIRRequest; response: TFHIRResponse; var searchKey, link, sql, title, base : String; var total : Integer) : boolean;
    procedure ProcessDefaultSearch(typekey : integer; session : TFHIRSession; aType : TFHIRResourceType; params : TParseMap; baseURL, compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption);
    procedure BuildSearchForm(request: TFHIRRequest; response : TFHIRResponse);
    function GetResourceByKey(key : integer): TFHIRResource;
    function getResourceByReference(url, compartments : string): TFHIRResource;
    function GetProfileByURL(url: String; var id : String) : TFHirStructureDefinition;
    function GetValueSetById(request: TFHIRRequest; id, base : String) : TFHIRValueSet;
    function GetResourceById(request: TFHIRRequest; aType : TFhirResourceType; id, base : String) : TFHIRResource;
    function GetProfileById(request: TFHIRRequest; id, base : String) : TFHirStructureDefinition;
    function GetValueSetByIdentity(id, version : String) : TFHIRValueSet;
//    function constructValueSet(params : TParseMap; var used : String; allowNull : Boolean) : TFhirValueset;

    procedure updateProvenance(prv : TFHIRProvenance; rtype, id, vid : String);

    function FindResource(aType : TFHIRResourceType; sId : String; bAllowDeleted : boolean; var resourceKey : integer; request: TFHIRRequest; response: TFHIRResponse; compartments : String): boolean;
//    function FindResourceVersion(aType : TFHIRResourceType; sId, sVersionId : String; bAllowDeleted : boolean; var resourceVersionKey : integer; request: TFHIRRequest; response: TFHIRResponse): boolean;
    procedure NoMatch(request: TFHIRRequest; response: TFHIRResponse);
    procedure NotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure VersionNotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure TypeNotFound(request: TFHIRRequest; response : TFHIRResponse);
    function GetNewResourceId(aType : TFHIRResourceType; var id : string; var key : integer):Boolean;
    function AddNewResourceId(aType : TFHIRResourceType; id : string; var resourceKey : integer) : Boolean;
    Procedure CollectIncludes(session : TFhirSession; includes : TReferenceList; resource : TFHIRResource; path : String);
//    Procedure CollectReverseIncludes(session : TFHIRSession; includes : TReferenceList; keys : TStringList; types : String; bundle : TFHIRBundle; request : TFHIRRequest; response : TFHIRResponse; wantsummary : TFHIRSummaryOption);
//    Function TypeForKey(key : integer):TFHIRResourceType;
    Procedure LoadTags(tags : TFHIRTagList; ResourceKey : integer);
    procedure CommitTags(tags : TFHIRTagList; key : integer);
    Procedure ProcessBlob(request: TFHIRRequest; response : TFHIRResponse; field : String; comp : TFHIRParserClass);
    function ResolveSearchId(atype : TFHIRResourceType; compartmentId, compartments : String; baseURL, params : String) : TMatchingResourceList;
//    function ResolveSearchIdCount(atype : TFHIRResourceType; compartmentId, compartments : String; baseURL, params : String) : integer;
    function ScanId(request : TFHIRRequest; entry : TFHIRBundleEntry; ids : TFHIRTransactionEntryList; index : integer) : TFHIRTransactionEntry;
    procedure FixXhtmlUrls(lang, base: String; ids: TFHIRTransactionEntryList; node: TFhirXHtmlNode);
    procedure adjustReferences(te : TFHIRTransactionEntry; base : String; entry : TFHIRBundleEntry; ids : TFHIRTransactionEntryList);
    function commitResource(request: TFHIRRequest; response : TFHIRResponse; upload : boolean; entry : TFHIRBundleEntry; i : integer; id : TFHIRTransactionEntry; session : TFhirSession; resp : TFHIRBundle) : boolean;

    procedure checkProposedContent(request : TFHIRRequest; resource : TFhirResource; tags : TFHIRTagList);
    procedure checkProposedDeletion(request : TFHIRRequest; resource : TFhirResource; tags : TFHIRTagList);

    function BuildResponseMessage(request : TFHIRRequest; incoming : TFhirMessageHeader) : TFhirMessageHeader;
    procedure ProcessMessage(request: TFHIRRequest; response : TFHIRResponse; msg, resp : TFhirMessageHeader; bundle : TFHIRBundle);
    procedure ProcessMsgQuery(request: TFHIRRequest; response : TFHIRResponse; bundle : TFHIRBundle);
//    procedure ProcessMsgClaim(request : TFHIRRequest; incoming, outgoing: TFhirMessageHeader; inbundle, outbundle: TFHIRBundle);
//    function MessageCreateResource(context : TFHIRValidatorContext; request : TFHIRRequest; res : TFHIRResource) : string;
    function EncodeResource(r : TFhirResource; xml : boolean; summary : TFHIRSummaryOption) : TBytes;

    // operations
//    procedure ExecuteQuestionnaireGeneration(request: TFHIRRequest; response : TFHIRResponse);
//    procedure ExecuteGenerateQA(request: TFHIRRequest; response : TFHIRResponse);
//    procedure ExecuteHandleQAPost(request: TFHIRRequest; response : TFHIRResponse);

    function CreateDocumentAsBinary(mainRequest : TFhirRequest) : String;
    procedure CreateDocumentReference(mainRequest : TFhirRequest; binaryId : String);

    procedure AuditRest(session : TFhirSession; ip : string; resourceType : TFhirResourceType; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenance; httpCode : Integer; name, message : String); overload;
    procedure AuditRest(session : TFhirSession; ip : string; resourceType : TFhirResourceType; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenance; opName : String; httpCode : Integer; name, message : String); overload;
    procedure SaveProvenance(session : TFhirSession; prv : TFHIRProvenance);

    procedure CheckCompartments(actual, allowed : String);
    procedure ExecuteRead(request: TFHIRRequest; response : TFHIRResponse);
    procedure executeReadInTransaction(entry : TFhirBundleEntryRequest; request: TFHIRRequest; response : TFHIRResponse);

    procedure processIncludes(session : TFhirSession; _includes, _reverseIncludes : String; bundle : TFHIRBundle; keys : TKeyList; field : String; comp : TFHIRParserClass);

    function  ExecuteUpdate(upload : boolean; request: TFHIRRequest; response : TFHIRResponse) : Boolean;
    procedure ExecuteVersionRead(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteDelete(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteHistory(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse);
    Function  ExecuteCreate(upload : boolean; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
    procedure ExecuteConformanceStmt(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteUpload(request: TFHIRRequest; response : TFHIRResponse);
    function  ExecuteValidation(request: TFHIRRequest; response : TFHIRResponse; opDesc : String) : boolean;
    procedure ExecuteTransaction(upload : boolean; request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteBatch(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteOperation(request: TFHIRRequest; response : TFHIRResponse);
    procedure SetConnection(const Value: TKDBConnection);
    procedure ReIndex;
//    function  IdentifyValueset(request: TFHIRRequest; resource : TFHIRResource; params: TParseMap; base: String; var used, cacheId: string; allowNull : boolean = false): TFHIRValueSet;
    procedure CheckCreateNarrative(request : TFHIRRequest);
    procedure CreateIndexer;
  public
    Constructor Create(lang : String; repository : TFHIRDataStore);
    Destructor Destroy; Override;
    Property Connection : TKDBConnection read FConnection write SetConnection;
    Property Repository : TFHIRDataStore read FRepository;

    // internal utility functions
    procedure addParam(srch : TFhirConformanceRestResourceSearchParamList; html : TAdvStringBuilder; n, url, d : String; t : TFhirSearchParamTypeEnum; tgts : TFhirResourceTypeSet);
//    function AddDeletedResourceToFeed(bundle : TFHIRBundle; sId, sType, base : String) : TFHIRBundleEntry;

    function AddResourceTobundle(bundle : TFHIRBundle; base : String; updated : TDateTime; resource : TFHIRResource; purpose : TFhirSearchEntryModeEnum) : TFHIRBundleEntry; overload;
    function AddResourceTobundle(bundle : TFHIRBundle; base : String; field : String; comp : TFHIRParserClass; purpose : TFhirSearchEntryModeEnum; makeRequest : boolean; var type_ : TFhirResourceType) : TFHIRBundleEntry; overload;
    function check(response : TFHIRResponse; test : boolean; code : Integer; lang, message : String) : Boolean;
    procedure DefineConformanceResources(base : String); // called after database is created

    // when want an MRN to store the resource against
    function GetPatientId : String; virtual;

    // called when kernel actually wants to process against the store
    Function Execute(request: TFHIRRequest; response : TFHIRResponse; upload : Boolean) : String;  virtual;

    function  LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference;

    property lang : String read FLang write FLang;
    Property TestServer : boolean read FTestServer write FTestServer;
    Property OwnerName : String read FOwnerName write FOwnerName;
    Property OnPopulateConformance : TPopulateConformanceEvent read FOnPopulateConformance write FOnPopulateConformance;

    // index maintenance
    procedure clear(a : TFhirResourceTypeSet);
    procedure storeResources(list: TFHIRResourceList; origin : TFHIRRequestOrigin; upload : boolean);
  end;



  TFhirGenerateQAOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
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
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirQuestionnaireGenerationOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirExpandValueSetOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirLookupValueSetOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirValueSetValidationOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
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
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
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
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
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
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirGenerateDocumentOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  private
    procedure addResource(manager: TFhirOperationManager; bundle : TFHIRBundle; reference : TFhirReference; required : boolean; compartments : String);
    procedure addSections(manager: TFhirOperationManager; bundle : TFHIRBundle; sections : TFhirCompositionSectionList; compartments : String);
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirGenerateCDSHookOperation = class (TFHIROperation)
  private
    procedure addCard(params : TFhirParameters; base, message, source, sourceurl, indicator : String);
    procedure processPatientView(manager: TFhirOperationManager; request : TFHIRRequest; context : TFHIRPatient; params : TFhirParameters);
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirValidationOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
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
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirGenerateSnapshotOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirGenerateTemplateOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TFhirGenerateNarrativeOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
  end;

implementation

uses
  SystemService;

function booleanToSQL(b : boolean): string;
begin
  if b then
    result := '1'
  else
    result := '0';
end;


{ TFhirOperationManager }

constructor TFhirOperationManager.Create(lang : String; repository : TFHIRDataStore);
begin
  inherited Create;
  FLang := lang;
  FRepository := repository;
  FFactory := TFHIRFactory.create(lang);
  FOperations := TAdvObjectList.create;


  // order of registration matters - general validation must be after value set validation
  FOperations.add(TFhirExpandValueSetOperation.create);
  FOperations.add(TFhirLookupValueSetOperation.create);
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
  FOperations.add(TFhirGenerateSnapshotOperation.create);
  FOperations.add(TFhirGenerateCDSHookOperation.create);
  FOperations.add(TFhirGenerateTemplateOperation.create);
  FOperations.add(TFhirGenerateNarrativeOperation.create);
end;

function TFhirOperationManager.CreateDocumentAsBinary(mainRequest : TFhirRequest): String;
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
begin
  req := TFHIRRequest.Create(roOperation);
  try
    req.Session := mainRequest.session.Link;
    req.ResourceType := frtBinary;
    req.CommandType := fcmdCreate;
//    req.categories.AddAll(mainRequest.bundle.categories);
    raise Exception.Create('TODO');
    req.Lang := mainRequest.Lang;
    if mainRequest.PostFormat = ffJson then
      req.Resource := FFactory.makeBinaryContent(mainRequest.Source, 'application/json+fhir')
    else
      req.Resource := FFactory.makeBinaryContent(mainRequest.Source, 'application/atom+xml');
    resp := TFHIRResponse.Create;
    try
      ExecuteCreate(false, req, resp, idNoNew, 0);
      if resp.HTTPCode >= 300 then
        raise Exception.Create(resp.Message);
      result := resp.id;
    finally
      resp.Free;
    end;
  finally
    req.Free;
  end;
end;


procedure TFhirOperationManager.CreateDocumentReference(mainRequest : TFhirRequest; binaryId: String);
var
  ref : TFhirDocumentReference;
  comp : TFhirComposition;
  i : integer;
  att : TFhirCompositionAttester;
  req : TFHIRRequest;
  resp : TFHIRResponse;
  attach : TFhirAttachment;
begin
  comp := mainRequest.bundle.entryList[0].resource as TFhirComposition;

  ref := TFhirDocumentReference.Create;
  try
    ref.masterIdentifier := FFactory.makeIdentifier('urn:ietf:rfc:3986', mainRequest.bundle.id);
    if (comp.identifier <> nil) then
      ref.identifierList.Add(comp.identifier.Clone);
    ref.subject := comp.subject.Clone;
    ref.type_ := comp.type_.Clone;
    ref.class_ := comp.class_.Clone;
    ref.authorList.AddAll(comp.authorList);
    ref.custodian := comp.custodian.Clone;
    // we don't have a use for policyManager at this point
    for i := 0 to comp.attesterList.Count - 1 do
    begin
      att := comp.attesterList[i];
      if (att.mode * [CompositionAttestationModeProfessional, CompositionAttestationModeLegal] <> []) then
        ref.authenticator := att.party.Clone; // which means that last one is the one
    end;
    ref.created := comp.date.Clone;
    ref.indexed := NowUTC;
    ref.status := DocumentReferenceStatusCurrent;
    ref.docStatus := FFactory.makeCodeableConcept(FFactory.makeCoding('http://hl7.org/fhir/composition-status', comp.statusElement.value, comp.statusElement.value), '');
    // no relationships to other documents
    ref.description := comp.title;
    ref.securityLabelList.add(FFactory.makeCodeableConcept(FFactory.makeCoding('http://hl7.org/fhir/v3/Confidentiality', comp.statusElement.value, ''), ''));
    // populating DocumentReference.format:
    // we take any tags on the document. We ignore security tags. Always will be at least one - the document tag itself
//    for i := 0 to mainRequest.bundle.categories.Count - 1 do
//      if (mainRequest.bundle.categories[i].scheme <> 'http://hl7.org/fhir/tag/security') then
//        ref.formatList.Add(FFactory.makeUri(mainRequest.bundle.categories[i].term));
    raise Exception.Create('TODO');
    // todo: ref.hash (HexBinary representation of SHA1)
    attach := TFhirAttachment.create;
    ref.contentList.Append.attachment := attach;
    attach.url := 'Binary/'+binaryId;
    attach.languageElement := comp.languageElement.Clone;
    if mainRequest.PostFormat = ffJson then
      attach.contentType := 'application/json+fhir'
    else
      attach.contentType := 'application/atom+xml';
    if comp.eventlist.Count > 0 then
    begin
      ref.context := TFhirDocumentReferenceContext.Create;
      ref.context.eventList.AddAll(comp.eventList[0].codeList);
      ref.context.period := comp.eventList[0].period.Clone;
    end;

    req := TFHIRRequest.Create(roOperation);
    try
      req.Session := mainRequest.session.Link;
      req.ResourceType := frtDocumentReference;
      req.CommandType := fcmdCreate;
//      req.categories.AddAll(mainRequest.bundle.categories);
      raise Exception.Create('TODO');

      req.Lang := mainRequest.Lang;
      req.Resource := ref.Link;
      resp := TFHIRResponse.Create;
      try
        ExecuteCreate(false, req, resp, idNoNew, 0);
        if resp.HTTPCode >= 300 then
          raise Exception.Create(resp.Message);
      finally
        resp.Free;
      end;
    finally
      req.Free;
    end;
  finally
    ref.Free;
  end;
end;


procedure TFhirOperationManager.DefineConformanceResources(base: String);

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
    storeResources(list, roConfig, true);
  finally
    list.Free;
  end;

end;

destructor TFhirOperationManager.Destroy;
begin
  FOperations.Free;
  FIndexer.Free;
  FSpaces.free;
  FFactory.Free;
  FRepository.Free;
  inherited;
end;


function TFhirOperationManager.AddResourceTobundle(bundle : TFHIRBundle; base : String; updated : TDateTime; resource : TFHIRResource; purpose : TFhirSearchEntryModeEnum) : TFHIRBundleEntry;
var
  entry : TFHIRBundleEntry;
begin
  entry := TFHIRBundleEntry.Create;
  try
    entry.resource := resource.Link;
    entry.fullUrl := AppendForwardSlash(base)+CODES_TFhirResourceType[resource.ResourceType]+'/'+resource.id;
    if (purpose <> SearchEntryModeNull) then
    begin
      entry.search := TFhirBundleEntrySearch.Create;
      entry.search.mode := purpose;
    end;
    bundle.entryList.add(entry.Link);
    result := entry;
  finally
    entry.Free;
  end;
end;


function TFhirOperationManager.AddResourceTobundle(bundle : TFHIRBundle; base : String; field : String; comp : TFHIRParserClass; purpose : TFhirSearchEntryModeEnum; makeRequest : boolean; var type_ : TFhirResourceType) : TFHIRBundleEntry;
var
  parser : TFhirParser;
  mem : TBytesStream;
  sId, sType, sAud : String;
begin
  sId := FConnection.ColStringByName['Id'];
  sType := FConnection.colStringByName['ResourceName'];
  type_ := ResourceTypeByName(sType);
  if (FConnection.ColIntegerByName['Status'] = 2) then
  begin
    result := TFHIRBundleEntry.Create;
    try
      result.fullUrl := AppendForwardSlash(base)+sType+'/'+sId;
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
  else
  begin
    if comp = nil then
    begin
      result := bundle.entryList.Append;
      result.Tag := TAdvBuffer.create;
      result.fullUrl := AppendForwardSlash(base)+sType+'/'+sId;
      TAdvBuffer(result.Tag).AsBytes := FConnection.ColBlobByName[field];
      if (purpose <> SearchEntryModeNull) then
      begin
        result.search := TFhirBundleEntrySearch.Create;
        result.search.mode := purpose;
      end;
    end
    else
    begin
      mem := TBytesStream.Create(FConnection.ColBlobByName[field]);
      try
        parser := comp.Create(lang);
        try
          parser.source := mem;
          parser.ParserPolicy := xppDrop;
          parser.Parse;
          result := AddResourceTobundle(bundle, base, TSToDateTime(FConnection.ColTimeStampByName['StatedDate']), parser.resource, purpose);
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
        result.request.url := AppendForwardSlash(base)+sType+'/'+sId;
        result.request.method := HttpVerbPUT;
        result.Tags['opdesc'] := 'Updated by '+FConnection.ColStringByName['Name']+' at '+result.response.lastModified.AsString+ '(UTC)';
      end
      else
      begin
        result.request.method := HttpVerbPOST;
        result.request.url := AppendForwardSlash(base)+sType;
        result.Tags['opdesc'] := 'Created by '+FConnection.ColStringByName['Name']+' at '+result.response.lastModified.AsString+ '(UTC)';
      end;
    end;
  end;
end;

function TFhirOperationManager.check(response: TFHIRResponse; test: boolean; code : Integer; lang, message: String): Boolean;
begin
  result := test;
  if not test then
  begin
    response.HTTPCode := code;
    response.Message := message;
    response.ContentType := 'text/plain';
    response.Body := message;
    response.Resource := BuildOperationOutcome(lang, message);
  end;
end;


function TFhirOperationManager.EncodeResource(r: TFhirResource; xml : boolean; summary : TFHIRSummaryOption): TBytes;
var
  b : TBytesStream;
  comp : TFHIRComposer;
begin
  b :=  TBytesStream.Create;
  try
    if (xml) then
      comp := TFHIRXmlComposer.Create('en')
    else
      comp := TFHIRJsonComposer.Create('en');
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

Function TFhirOperationManager.Execute(request: TFHIRRequest; response : TFHIRResponse; upload : Boolean) : String;
begin
 // assert(FConnection.InTransaction);
  result := Request.Id;
  case request.CommandType of
    fcmdRead : ExecuteRead(request, response);
    fcmdUpdate : ExecuteUpdate(upload, request, response);
    fcmdVersionRead : ExecuteVersionRead(request, response);
    fcmdDelete : ExecuteDelete(request, response);
    fcmdHistoryInstance, fcmdHistoryType, fcmdHistorySystem : ExecuteHistory(request, response);
    fcmdSearch : ExecuteSearch(request, response);
    fcmdCreate : result := ExecuteCreate(false, request, response, idNoNew, 0);
    fcmdConformanceStmt : ExecuteConformanceStmt(request, response);
    fcmdTransaction : ExecuteTransaction(upload, request, response);
    fcmdOperation : ExecuteOperation(request, response);
    fcmdUpload : ExecuteUpload(request, response);
  else
    Raise Exception.Create(GetFhirMessage('MSG_UNKNOWN_OPERATION', lang));
  End;
end;

procedure TFhirOperationManager.addParam(srch : TFhirConformanceRestResourceSearchParamList; html : TAdvStringBuilder; n, url, d : String; t : TFhirSearchParamTypeEnum; tgts : TFhirResourceTypeSet);
var
  param : TFhirConformanceRestResourceSearchParam;
  a : TFhirResourceType;
begin
  param := TFhirConformanceRestResourceSearchParam.create;
  try
    param.name := n;
    param.definition := url;
    param.documentation := d;
    param.type_ := t;
    for a := Low(TFhirResourceType) to High(TFhirResourceType) do
      if a in tgts then
        param.targetList.Append.value := CODES_TFhirResourceType[a];
    srch.add(param.link);
  finally
    param.free;
  end;
//  html.append('<li>'+n+' : '+FormatTextToHTML(d)+'</li>');
end;


procedure TFhirOperationManager.ExecuteConformanceStmt(request: TFHIRRequest; response: TFHIRResponse);
var
  oConf : TFhirConformance;
  res : TFhirConformanceRestResource;
  a : TFHIRResourceType;
  html : TAdvStringBuilder;
  c : TFhirContactPoint;
  i : integer;
  op : TFhirConformanceRestOperation;
  ct : TFhirConformanceContact;
begin
  try

    response.HTTPCode := 200;
    oConf := TFhirConformance.Create;
    response.Resource := oConf;

    oConf.id := 'FhirServer';
    ct := oConf.contactList.Append;
    c := ct.telecomList.Append;
    c.system := ContactPointSystemOther;
    c.value := 'http://healthintersections.com.au/';
    if FRepository.FormalURLPlain <> '' then
      oConf.url := AppendForwardSlash(FRepository.FormalURLPlainOpen)+'metadata'
    else
      oConf.url := 'http://fhir.healthintersections.com.au/open/metadata';

    oConf.version := FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+'-'+SERVER_VERSION; // this conformance statement is versioned by both
    oConf.name := 'Health Intersections FHIR Server Conformance Statement';
    oConf.publisher := 'Health Intersections'; //
    oConf.description := 'Standard Conformance Statement for the open source Reference FHIR Server provided by Health Intersections';
    oConf.status := ConformanceResourceStatusActive;
    oConf.experimental := false;
    oConf.date := TDateAndTime.CreateUTC(UniversalDateTime);
    oConf.software := TFhirConformanceSoftware.Create;
    oConf.software.name := 'Reference Server';
    oConf.software.version := SERVER_VERSION;
    oConf.software.releaseDate := TDateAndTime.createXML(SERVER_RELEASE_DATE);
    if FRepository.FormalURLPlainOpen <> '' then
    begin
      oConf.implementation_ := TFhirConformanceImplementation.Create;
      oConf.implementation_.description := 'FHIR Server running at '+FRepository.FormalURLPlainOpen;
      oConf.implementation_.url := FRepository.FormalURLPlainOpen;
    end;
    if assigned(FOnPopulateConformance) then
      FOnPopulateConformance(self, oConf);

    oConf.acceptUnknown := UnknownContentCodeBoth;
    oConf.formatList.Append.value := 'application/xml+fhir';
    oConf.formatList.Append.value := 'application/json+fhir';


    oConf.fhirVersion := FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION;
    oConf.restList.add(TFhirConformanceRest.Create);
    oConf.restList[0].mode := RestfulConformanceModeServer;
    oConf.restList[0].addExtension('http://hl7.org/fhir/StructureDefinition/conformance-websockets', request.baseUrl+'websockets');
    oConf.restList[0].interactionList.Append.code := SystemRestfulInteractionTransaction;
    oConf.restList[0].interactionList.Append.code := SystemRestfulInteractionSearchSystem;
    oConf.restList[0].interactionList.Append.code := SystemRestfulInteractionHistorySystem;
    oConf.restList[0].transactionMode := TransactionModeBoth;
    oConf.text := TFhirNarrative.create;
    oConf.text.status := NarrativeStatusGenerated;

    FRepository.TerminologyServer.declareSystems(oConf);
    if assigned(FOnPopulateConformance) and request.secure then // only add smart on fhir things on a secure interface
      FOnPopulateConformance(self, oConf);

    html := TAdvStringBuilder.Create;
    try
      html.append('<div><h2>FHIR Reference Server Conformance Statement</h2><p>FHIR v'+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+' released '+RELEASE_DATE+'. '+
       'Reference Server version '+SERVER_VERSION+' built '+SERVER_RELEASE_DATE+'</p><table class="grid"><tr><th>Resource Type</th><th>Profile</th><th>Read</th><th>V-Read</th><th>Search</th><th>Update</th><th>Updates</th><th>Create</th><th>Delete</th><th>History</th><th>Validate</th></tr>'+#13#10);
      for a := TFHIRResourceType(1)  to High(TFHIRResourceType) do
      begin
        if FRepository.ResConfig[a].Supported and (a <> frtMessageHeader) then
        begin
          if a = frtBinary then
            html.append('<tr><td>'+CODES_TFHIRResourceType[a]+'</td>'+
            '<td>--</td>')
          else
            html.append('<tr><td>'+CODES_TFHIRResourceType[a]+'</td>'+
            '<td><a href="'+request.baseUrl+'profile/'+lowercase(CODES_TFHIRResourceType[a])+'?format=text/html">'+lowercase(CODES_TFHIRResourceType[a])+'</a></td>');
          res := TFhirConformanceRestResource.create;
          try
            res.type_ := CODES_TFHIRResourceType[a];
            if a <> frtBinary then
              res.profile := FFactory.makeReference(request.baseUrl+'Profile/'+lowercase(CODES_TFHIRResourceType[a]));
            if not (a in [frtMessageHeader, frtParameters]) Then
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
              if FRepository.ResConfig[a].cmdSearch and request.canRead(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionSearchType;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdUpdate and request.canWrite(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionUpdate;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdHistoryType and request.canRead(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionHistoryType;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdCreate and request.canWrite(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionCreate;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdDelete and request.canWrite(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionDelete;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdHistoryInstance and request.canRead(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionHistoryInstance;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdValidate and request.canRead(a) then
              begin
                res.interactionList.Append.code := TypeRestfulInteractionValidate;
                html.append('<td align="center"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
//                html.append('<br/>search</td><td><ul>');
              for i := 0 to FRepository.Indexes.Indexes.count - 1 do
                if (FRepository.Indexes.Indexes[i].ResourceType = a) then
                  addParam(res.searchParamList, html, FRepository.Indexes.Indexes[i].Name, FRepository.Indexes.Indexes[i].uri, FRepository.Indexes.Indexes[i].Description, FRepository.Indexes.Indexes[i].SearchType, FRepository.Indexes.Indexes[i].TargetTypes);

//              addParam(res.searchParamList, html, '_id', 'http://hl7.org/fhir/search', 'Resource Logical ID', SearchParamTypeToken, []);
              addParam(res.searchParamList, html, '_text', 'http://hl7.org/fhir/search', 'General Text Search of the narrative portion', SearchParamTypeString, []);
              addParam(res.searchParamList, html, '_profile', 'http://hl7.org/fhir/search', 'Search for resources that conform to a profile', SearchParamTypeReference, []);
              addParam(res.searchParamList, html, '_security', 'http://hl7.org/fhir/search', 'Search for resources that have a particular security tag', SearchParamTypeReference, []);
              addParam(res.searchParamList, html, '_sort', 'http://hl7.org/fhir/search', 'Specify one or more other parameters to use as the sort order', SearchParamTypeToken, []);
              addParam(res.searchParamList, html, '_count', 'http://hl7.org/fhir/search', 'Number of records to return', SearchParamTypeNumber, []);
              addParam(res.searchParamList, html, '_summary', 'http://hl7.org/fhir/search', 'Return just a summary for resources that define a summary view', SearchParamTypeNumber, []);
              addParam(res.searchParamList, html, '_include', 'http://hl7.org/fhir/search', 'Additional resources to return - other resources that matching resources refer to', SearchParamTypeToken, ALL_RESOURCE_TYPES);
              addParam(res.searchParamList, html, '_reverseInclude', 'http://hl7.org/fhir/search', 'Additional resources to return - other resources that refer to matching resources (this is trialing an extension to the specification)', SearchParamTypeToken, ALL_RESOURCE_TYPES);
              addParam(res.searchParamList, html, '_filter', 'http://hl7.org/fhir/search', 'filter parameter as documented in the specification', SearchParamTypeToken, ALL_RESOURCE_TYPES);

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
      oConf.text.div_ := ParseXhtml(lang, html.asString, xppReject);
    finally
      html.free;
    end;

    AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

Function TFhirOperationManager.ExecuteCreate(upload : boolean; request: TFHIRRequest; response: TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
var
  sId, s : String;
  resourceKey, i : Integer;
  key : Integer;
  tags : TFHIRTagList;
  ok : boolean;
  tnow : TDateTime;
  list : TMatchingResourceList;
begin
  key := 0;
  CheckCreateNarrative(request);
  try
    ok := true;
    if not check(response, request.canWrite(request.ResourceType) and opAllowed(request.ResourceType, fcmdCreate), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;

    if ok and not check(response, request.Resource <> nil, 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) or not check(response, request.ResourceType = request.resource.ResourceType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang)) then
      ok := false;

    if ok and FRepository.Validate and not upload and (request.Session <> nil) then
    begin
      if not ExecuteValidation(request, response, 'Create Resource '+CODES_TFHIRResourceType[request.ResourceType]+'/'+request.Id+' ('+request.originalId+')') then
        ok := false
      else
        response.Resource := nil;
    end;

    if request.IfNoneExist <> '' then
    begin
      s := request.IfNoneExist;
      if (s.Contains('?')) then
        s := s.Substring(s.IndexOf('?')+1);
      list := ResolveSearchId(request.ResourceType, request.compartmentId, request.compartments, request.baseUrl, s);
      try
        ok := false;
        if list.Count = 1 then
        begin
          response.HTTPCode := 200;
        end
        else if list.Count > 1 then
          check(response, false, 412, lang, GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang))
        else
          ok := true;
      finally
        list.Free;
      end;
    end;

    if not ok then
      // nothing
    else if (idState = idMaybeNew) and (request.Id <> '') then
    begin
      sId := request.Id;
      if not check(response, (Length(sId) <= ID_LENGTH) and AddNewResourceId(request.resourceType, sId, resourceKey), 404, lang, GetFhirMessage('MSG_INVALID_ID', lang)) then
        ok := false;
    end
    else if (idState = idIsNew) then
    begin
      sid := request.id;
      resourceKey := iAssignedKey;
    end
    else if not check(response, GetNewResourceId(request.ResourceType, sId, resourceKey), 404, lang, StringFormat(GetFhirMessage('MSG_DUPLICATE_ID', lang), [sId, CODES_TFHIRResourceType[request.resourceType]])) then
       ok := false

    else
      request.resource.id := sId;

    if ok then
      if not check(response, request.Resource.id = sId, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.Resource.id+'/'+sId+' (1)') then
        ok := false;

    if ok then
    begin

      if request.resource.meta = nil then
        request.resource.meta := TFhirMeta.Create;
      request.Resource.meta.lastUpdated := NowUTC;
      request.Resource.meta.versionId := '1';
      updateProvenance(request.Provenance, CODES_TFHIRResourceType[request.ResourceType], sid, '1');
      tnow := request.Resource.meta.lastUpdated.AsUTC.GetDateTime;

      checkNotRedacted(request.Resource.meta, 'Creating resource');
      tags := TFHIRTagList.create;
      try
        tags.readTags(request.resource.meta);

        checkProposedContent(request, request.Resource, tags);
        result := sId;
        request.id := sId;
        key := FRepository.NextVersionKey;
        for i := 0 to tags.count - 1 do
          FRepository.RegisterTag(tags[i], FConnection);

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Status, SessionKey, '+
                'Tags, XmlContent, XmlSummary, JsonContent, JsonSummary) values (:k, :rk, :sd, :td, :v, :f, 0, :s, :tb, :xc, :xs, :jc, :js)';
        FConnection.prepare;
        try
          FConnection.BindInteger('k', key);
          FConnection.BindInteger('rk', resourceKey);
          FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
          FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
          request.SubId := '1';
          FConnection.BindString('v', '1');
          if request.Session <> nil then
            FConnection.BindInteger('s', request.Session.Key)
          else
            FConnection.BindInteger('s', 0);
          FConnection.BindInteger('f', 2);
          FConnection.BindBlobFromBytes('tb', tags.json);
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
        if ((request.ResourceType = frtAuditEvent) and request.Resource.hasTag('verkey')) then
          FConnection.ExecSQL('update Versions set AuditKey = '+inttostr(resourceKey)+' where ResourceVersionKey = '+request.Resource.Tags['verkey']);

        CreateIndexer;
        CheckCompartments(FIndexer.execute(resourceKey, sId, request.resource, tags), request.compartments);
        FRepository.SeeResource(resourceKey, key, sId, true, request.Resource, FConnection, false, request.Session);
        if request.resourceType = frtPatient then
          FConnection.execSQL('update Compartments set CompartmentKey = '+inttostr(resourceKey)+' where Id = '''+sid+''' and CompartmentKey is null');
        response.HTTPCode := 201;
        response.Message := 'Created';
        response.Location := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+sId+'/_history/1';
        response.Resource := request.Resource.Link;

        response.id := sId;
        response.versionId := '1';
      finally
        tags.free;
      end;
      if request.Provenance <> nil then
        SaveProvenance(request.Session, request.Provenance);
    end;
    if request.ResourceType <> frtAuditEvent then // else you never stop
      AuditRest(request.session, request.ip, request.ResourceType, sid, '1', key, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceType <> frtAuditEvent then // else you never stop
        AuditRest(request.session, request.ip, request.ResourceType, sid, '1', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFhirOperationManager.ExecuteDelete(request: TFHIRRequest; response: TFHIRResponse);
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
    if not check(response, request.canWrite(request.ResourceType) and opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;

      if (request.DefaultSearch) then
      begin
        list := ResolveSearchId(request.ResourceType, request.compartmentId, request.compartments, request.baseUrl, request.Parameters.Source);
        try
          ok := false;
          if list.count = 0 then
            NoMatch(request, response)
          else if list.Count > 1 then
            check(response, false, 412, lang, GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang))
          else
          begin
            request.Id := list[0].name;
            ok := true;
          end;
        finally
          list.Free;
        end;
      end;


    if ok and (not check(response, length(request.id) <= ID_LENGTH, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id])) or
       not FindResource(request.ResourceType, request.Id, true, resourceKey, request, response, request.compartments)) then
      ok := false;

    if ok and FTestServer and not check(response, request.id <> 'example', 400, lang, GetFhirMessage('MSG_RESOURCE_EXAMPLE_PROTECTED', lang)) then
      ok := false;


    if ok and (request.Resource <> nil) and not check(response, request.id = request.Resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.id+'/'+request.resource.id+' (2)') Then
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

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Status, SessionKey, Tags) values '+
                                                             '(:k,:rk, :sd, :td, :v, :f, 2, :s, :t)';
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
        FRepository.DropResource(ResourceKey, key, request.Id, request.ResourceType, FIndexer);
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
    AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), key, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirOperationManager.BuildHistoryResultSet(request: TFHIRRequest; response: TFHIRResponse; var searchKey, link, sql, title, base : String; var total : Integer) : boolean;
var
  cmp : String;
  resourceKey : integer;
  lt : string;
  id : String;
  i : integer;
  since, prior : TDateTime;
  a : TFhirResourceType;
begin
  // todo: restrict to readable resources
  id := FhirGUIDToString(CreateGuid); // this the id of the search
  searchKey := inttostr(FRepository.NextSearchKey);
  FConnection.ExecSQL('insert into Searches (SearchKey, Id, Count, Type, Date, Summary) values ('+searchKey+', '''+id+''', 0, 2, '+DBGetDate(FConnection.Owner.Platform)+', '+booleanToSQl(false)+')');

  if (request.compartments <> '') then
    cmp := ' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id in ('+request.compartments+'))'
  else
    cmp := '';

  result := true;
  case request.CommandType of
    fcmdHistoryInstance :
      begin
        TypeNotFound(request, response);
        if not check(response, request.canRead(request.ResourceType) and opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
          result := false
        else if (length(request.id) > ID_LENGTH) or not FindResource(request.ResourceType, request.Id, true, resourceKey, request, response, request.compartments) then
          result := false
        else
          FConnection.SQL := 'Insert into SearchEntries Select '+searchkey+', Ids.ResourceKey, Versions.ResourceVersionKey, RIGHT (''0000000000000''+CAST(Versions.ResourceVersionKey AS VARCHAR(14)),14) as sort ' +
           'from Versions, Ids, Sessions '+
           'where Versions.ResourceKey = '+inttostr(resourceKey)+' '+cmp+' and Versions.ResourceKey = Ids.ResourceKey and Versions.SessionKey = Sessions.SessionKey';
        title := StringFormat(GetFhirMessage('MSG_HISTORY', lang), [CODES_TFHIRResourceType[request.ResourceType]+' '+GetFhirMessage('NAME_RESOURCE', lang)+' '+request.Id]);
        base := request.baseUrl+''+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/_history?';
      end;
    fcmdHistoryType :
      begin
        TypeNotFound(request, response);
        if not check(response, request.canRead(request.ResourceType) and opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
          result := false
        else
        begin
          FConnection.SQL := 'Insert into SearchEntries Select '+searchkey+', Ids.ResourceKey, Versions.ResourceVersionKey, RIGHT (''0000000000000''+CAST(Versions.ResourceVersionKey AS VARCHAR(14)),14) as sort ' +
          'from Versions, Ids, Sessions '+
          'where Ids.ResourceTypeKey = '+inttostr(FRepository.ResConfig[request.ResourceType].key)+' '+cmp+' and Versions.ResourceKey = Ids.ResourceKey and Versions.SessionKey = Sessions.SessionKey';
          title := StringFormat(GetFhirMessage('MSG_HISTORY', lang), [CODES_TFHIRResourceType[request.ResourceType]]);
          base := request.baseUrl+''+CODES_TFhirResourceType[request.ResourceType]+'/_history?';
        end;
      end;
    fcmdHistorySystem :
      begin
        if request.Session.canReadAll then
          FConnection.SQL := 'Insert into SearchEntries Select '+searchkey+', Ids.ResourceKey, Versions.ResourceVersionKey, RIGHT (''0000000000000''+CAST(Versions.ResourceVersionKey AS VARCHAR(14)),14) as sort ' +
            'from Versions, Ids, Sessions '+
            'where Versions.ResourceKey = Ids.ResourceKey '+cmp+' and Versions.SessionKey = Sessions.SessionKey'
        else
        begin
          lt := '';
          for a in ALL_RESOURCE_TYPES do
            if request.canRead(a) then
              lt := lt+','+inttostr(FRepository.ResourceTypeKeyForName(CODES_TFHIRResourceType[a]));
          FConnection.SQL := 'Insert into SearchEntries Select '+searchkey+', Ids.ResourceKey, Versions.ResourceVersionKey, RIGHT (''0000000000000''+CAST(Versions.ResourceVersionKey AS VARCHAR(14)),14) as sort ' +
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
        base := base +'_prior='+request.Parameters.Value[request.Parameters.VarName(i)];
      end;
    if (prior = MIN_DATE) then
      base := base +'_prior='+FormatDateTime('yyyy-mm-dd''T''hh:nn:ss''Z''', UniversalDateTime, FormatSettings);

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
  if s.StartsWith(base+'Profile/') and s.EndsWith('/$questionnaire') then
    result := s.Substring(0, s.Length-15).Substring(base.Length+8)
  else
    raise Exception.Create('Did not understand Questionnaire identifier');
end;

//procedure TFhirOperationManager.ExecuteHandleQAPost(request: TFHIRRequest; response: TFHIRResponse);
//var
//  builder : TQuestionnaireBuilder;
//begin
//  // first, we convert to a resource.
//  builder := TQuestionnaireBuilder.Create;
//  try
//    builder.Answers := request.Resource.link as TFhirQuestionnaireResponse;
//    builder.Profiles := FRepository.Profiles.Link;
//    builder.Profile := GetProfileById(request, extractProfileId(request.baseUrl, builder.Answers.questionnaire.reference), '');
//    builder.onLookupReference := LookupReference;
//    builder.onLookupCode := FRepository.LookupCode;
//    builder.UnBuild;
//
//    // now, we handle it.
//    // todo: tag the resource with a profile
////    if not (builder.Profile.url.StartsWith('http://hl7.org/fhir/Profile') and StringArrayExistsSensitive(CODES_TFhirResourceType, builder.Profile.url.Substring(28)))  then
////      request.categories.AddValue('http://hl7.org/fhir/tag/profile', builder.Profile.url, builder.Profile.name);
//    raise Exception.Create('TODO');
//
//
//    request.Resource := builder.Resource.Link;
//  finally
//    builder.Free;
//  end;
//  if request.ResourceType = frtNull then
//  begin
//    request.ResourceType := request.Resource.ResourceType;
//    ExecuteCreate(nil, false, request, response, idNoNew, 0);
//  end
//  else if (request.id <> '') and (request.subId = '') then
//    ExecuteUpdate(nil, false, request, response)
//  else
//    raise Exception.Create('Error in call - can only be called against the system, or a specific resource');
//end;


procedure TFhirOperationManager.ExecuteHistory(request: TFHIRRequest; response: TFHIRResponse);
var
//  resourceKey : Integer;
//  bundle : TFHIRBundle;
//  originalId : String;
  offset, count, i : integer;
//  ok : boolean;
//  base : String;
//  cmp : String;
  bundle : TFHIRBundle;
//  entry : TFHIRBundleEntry;
//  includes : TReferenceList;
//  id, link, base, sql : String;
//  i, total, t : Integer;
//  key : integer;
//  offset, count : integer;
  ok : boolean;
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
  type_ : TFhirResourceType;
begin
  try
    if request.parameters.value[HISTORY_PARAM_NAME_ID] <> '' then
    begin
      ok := FindSavedSearch(request.parameters.value[HISTORY_PARAM_NAME_ID], request.Session, 2, id, link, sql, title, base, total, dummy);
      if check(response, ok, 400, lang, StringFormat(GetFhirMessage('MSG_HISTORY_EXPIRED', lang), [request.parameters.value[HISTORY_PARAM_NAME_ID]])) then
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

      chooseField(response.Format, soFull, field, comp, needsObject);
      if (not needsObject) then
        comp := nil;

      bundle := TFHIRBundle.Create(BundleTypeHistory);
      try
        if response.Format <> ffAsIs then
          base := base + '&_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
        bundle.total := inttostr(total);
        bundle.Tags['sql'] := sql;
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
          '  Ids.ResourceKey, ResourceName, Ids.Id, v.ResourceVersionKey, Audits.Id as AuditId, Ids.MostRecent, VersionId, StatedDate, Name, v.Status, Tags, '+field+' '+
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
            AddResourceTobundle(bundle, request.baseUrl, field, comp, SearchEntryModeNull, true, type_);
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
      AuditRest(request.session, request.ip, request.ResourceType, request.id, '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
    end;
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;


procedure TFhirOperationManager.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : integer;
  field : String;
  comp : TFHIRParserClass;
  needsObject : boolean;
begin
  try
    NotFound(request, response);
    if request.canRead(request.ResourceType) and check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (length(request.id) <= ID_LENGTH) and FindResource(request.ResourceType, request.Id, false, resourceKey, request, response, request.compartments) then
      begin
        chooseField(response.Format, request.Summary, field, comp, needsObject);
        FConnection.SQL := 'Select StatedDate, VersionId, '+field+' from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
        FConnection.Prepare;
        try
          FConnection.Execute;
          if FConnection.FetchNext then
          Begin
            if (request.IfNoneMatch <> '') and (request.IfNoneMatch = FConnection.GetColStringByName('VersionId')) then
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

              if not (request.ResourceType in [frtStructureDefinition]) then
                response.link_List.AddRelRef('edit-form', request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/$qa-edit');

              response.link_List.AddRelRef('z-edit-src', request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/$edit');
              processBlob(request, Response, field, comp);
            end;
          end;
        finally
          FConnection.Terminate;
        end;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;


procedure TFhirOperationManager.executeReadInTransaction(entry : TFhirBundleEntryRequest; request: TFHIRRequest; response: TFHIRResponse);
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
    request.ResourceType := TFhirResourceType(StringArrayIndexOfSensitive(CODES_TFhirResourceType, s));
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
        ExecuteRead(request, response)
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

function TFhirOperationManager.FindSavedSearch(const sId : String; Session : TFHIRSession; typeKey : integer; var id, link, sql, title, base : String; var total : Integer; var summaryStatus : TFHIRSummaryOption): boolean;
begin
  // todo: check sesseion...
  if sId = '' then
    result := false
  else
  begin
    FConnection.sql := 'Select SearchKey, SessionKey, Count, Summary, Title, Base, Link, SqlCode from Searches where Id = :s and Type = '+inttostr(typeKey);
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

function TFhirOperationManager.BuildSearchResultSet(typekey : integer; session : TFHIRSession; aType : TFHIRResourceType; params : TParseMap; baseURL, compartments, compartmentId : String; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption):String;
var
  id : string;
begin
  id := FhirGUIDToString(CreateGuid);
  result := inttostr(FRepository.NextSearchKey);
  if params.VarExists('_query') and (params.getVar('_query') <> '') then
  begin
    raise exception.create('The query "'+params.getVar('_query')+'" is not known');
  end
  else
    ProcessDefaultSearch(typekey, session, aType, params, baseURL, compartments, compartmentId, id, result, link, sql, total, summaryStatus);
end;

procedure TFhirOperationManager.ProcessDefaultSearch(typekey : integer; session : TFHIRSession; aType : TFHIRResourceType; params : TParseMap; baseURL, compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; summaryStatus : TFHIRSummaryOption);
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
  sp := TSearchProcessor.create;
  try
    sp.typekey := typekey;
    sp.type_ := aType;
    sp.compartmentId := compartmentId;
    sp.compartments := compartments;
    sp.baseURL := baseURL;
    sp.lang := lang;
    sp.params := params;
    CreateIndexer;
    sp.indexes := FRepository.Indexes.Link;
    sp.repository := FRepository.Link;
    sp.session := session.link;
    sp.countAllowed := true;

    sp.build;
    sql := 'Insert into SearchEntries Select '+key+', ResourceKey, MostRecent as ResourceVersionKey, '+sp.sort+' from Ids where Deleted = 0 and '+sp.filter+' order by ResourceKey DESC';
    csql := 'Select count(ResourceKey) from Ids where Deleted = 0 and '+sp.filter;
    link := SEARCH_PARAM_NAME_ID+'='+id+'&'+sp.link_;
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


procedure TFhirOperationManager.processIncludes(session: TFhirSession; _includes, _reverseIncludes: String; bundle: TFHIRBundle; keys : TKeyList; field: String; comp: TFHIRParserClass);
var
  s, sql : String;
  p : TArray<String>;
  sel : TStringList;
  key2, i: integer;
  t_, t1 : TFhirResourceType;
  type_ : TFhirResourceType;
begin
  if (_includes = '') and (_reverseIncludes = '') then
    exit;

  sel := TStringList.Create;
  try
    for s in _includes.Split([';']) do
    begin
      p := s.Split([':']);
      if (length(p) >= 2) and (length(p) <= 3) then
      begin
        t_ := ResourceTypeByName(p[0]);
        if t_ = frtNull then
          raise Exception.Create('Unknown Resource Type '+p[0]);
        key2 := FRepository.Indexes.GetKeyByName(p[1]);
        if (key2 = 0) then
          raise Exception.Create('Unknown Resource Parameter '+p[0]);
        if (length(p) = 3) then
        begin
          t1 := ResourceTypeByName(p[2]);
          if t1 = frtNull then
            raise Exception.Create('Unknown Resource Type '+p[2]);
          sel.Add('Ids.ResourceKey in (select Target from IndexEntries, Ids where IndexKey = '+inttostr(key2)+' and ResourceKey in ('+keys.forType(t_)+') and Ids.ResourceKey = IndexEntries.Target and Ids.ResoureTypeKey = '+inttostr(Repository.ResConfig[t1].key)+')');
        end
        else
          sel.Add('Ids.ResourceKey in (select Target from IndexEntries where IndexKey = '+inttostr(key2)+' and ResourceKey in ('+keys.forType(t_)+')');
      end
      else
        raise Exception.Create('Unable to process include '+s);
    end;

    for s in _reverseIncludes.Split([';']) do
    begin
      p := s.Split([':']);
      if (length(p) = 2) then
      begin
        t_ := ResourceTypeByName(p[0]);
        if t_ = frtNull then
          raise Exception.Create('Unknown Resource Type '+p[0]);
        key2 := FRepository.Indexes.GetKeyByName(p[1]);
        if (key2 = 0) then
          raise Exception.Create('Unknown Resource Parameter '+p[0]);
        sel.Add('Ids.ResourceKey in (select IndexEntries.ResourceKey from IndexEntries, Ids where IndexKey = '+inttostr(key2)+' and Target in ('+keys.forAll+') and Ids.ResourceKey = IndexEntries.ResourceKey and Ids.ResourceTypeKey = '+inttostr(Repository.ResConfig[t_].key)+')');
      end
      else
        raise Exception.Create('Unable to process include '+s);
    end;
    sql := '('+sel[0]+')';
    for i := 1 to sel.Count - 1 do
      sql := sql +' or ('+sel[i]+')';

    sql := 'Select Ids.ResourceKey, Types.ResourceName, Ids.Id, VersionId, StatedDate, Name, Versions.Status, Tags, '+field+' from Versions, Ids, Sessions, Types '+
          'where Ids.Deleted = 0 and Types.ResourceTypeKey = Ids.ResourceTypeKey and Versions.SessionKey = Sessions.SessionKey and Versions.ResourceVersionKey = Ids.MostRecent and ('+sql+'))';
  finally
    sel.Free;
  end;

  FConnection.SQL := sql;
  FConnection.Prepare;
  try
    FConnection.Execute;
    while FConnection.FetchNext do
      AddResourceTobundle(bundle, '', field, comp, SearchEntryModeInclude, false, type_);
  finally
    FConnection.Terminate;
  end;
end;

procedure TFhirOperationManager.ExecuteSearch(request: TFHIRRequest; response: TFHIRResponse);
var
  bundle : TFHIRBundle;
  id, link, base, sql, field : String;
  i, total, t : Integer;
  key : integer;
  offset, count : integer;
  ok : boolean;
  summaryStatus : TFHIRSummaryOption;
  title: string;
  keys : TKeyList;
  comp : TFHIRParserClass;
  needsObject : boolean;
  type_ : TFhirResourceType;
begin
  try
    ok := true;
    count := 0;
    offset := 0;
    { todo:
     conformance
     quantity searches
    }
    if not check(response, request.canRead(request.ResourceType) and opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      // ok := false
    else if (request.Parameters.getItemCount = 0) and (response.Format = ffXhtml) and (request.compartmentId = '') then
      BuildSearchForm(request, response)
    else
    begin
      TypeNotFound(request, response);
      if request.resourceType <> frtNull then
      begin
        key := FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[request.resourceType]+'''');
        if not check(response, key > 0, 404, lang, 'Resource Type '+CODES_TFHIRResourceType[request.resourceType]+' not known') then
          ok := false;
      end
      else
        key := 0;

      if ok then
      begin
        bundle := TFHIRBundle.Create(BundleTypeSearchset);
        keys := TKeyList.Create;
        try
//          bundle.base := request.baseUrl;
          bundle.meta := TFhirMeta.Create;
          bundle.meta.lastUpdated := NowUTC;

          summaryStatus := request.Summary;
          if FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], request.Session, 1, id, link, sql, title, base, total, summaryStatus) then
            link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
          else
            id := BuildSearchResultSet(key, request.Session, request.resourceType, request.Parameters, request.baseUrl, request.compartments, request.compartmentId, link, sql, total, summaryStatus);

          bundle.total := inttostr(total);
          bundle.Tags['sql'] := sql;

          base := AppendForwardSlash(Request.baseUrl)+CODES_TFhirResourceType[request.ResourceType]+'/_search?';
          if response.Format <> ffAsIs then
            base := base + '_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
          bundle.link_List.AddRelRef('self', base+link);

          if (summaryStatus <> soCount) then
          begin
            offset := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_OFFSET), 0);
            if request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT) = 'all' then
              count := SUMMARY_SEARCH_PAGE_LIMIT
            else
              count := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT), 0);
            if (count < 2) then
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

            chooseField(response.Format, summaryStatus, field, comp, needsObject);
            if (not needsObject) and not request.Parameters.VarExists('__wantObject') then // param __wantObject is for internal use only
              comp := nil;

            FConnection.SQL := 'Select Ids.ResourceKey, Types.ResourceName, Ids.Id, VersionId, StatedDate, Name, Versions.Status, Tags, '+field+' from Versions, Ids, Sessions, SearchEntries, Types '+
                'where Ids.Deleted = 0 and SearchEntries.ResourceVersionKey = Versions.ResourceVersionKey and Types.ResourceTypeKey = Ids.ResourceTypeKey and '+'Versions.SessionKey = Sessions.SessionKey and SearchEntries.ResourceKey = Ids.ResourceKey and SearchEntries.SearchKey = '+id+' '+
                'order by SortValue ASC';
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
                  AddResourceTobundle(bundle, request.baseUrl, field, comp, SearchEntryModeMatch, false, type_);
                  keys.Add(TKeyPair.Create(type_, FConnection.ColStringByName['ResourceKey']));
                  inc(t);
                end;
                if (t = count) then
                  break;
              End;
            finally
              FConnection.Terminate;
            end;

            processIncludes(request.session, request.Parameters.GetVar('_include'), request.Parameters.GetVar('_revinclude'), bundle, keys, field, comp);
          end;

          bundle.id := FhirGUIDToString(CreateGUID);

          //bundle.link_List['self'] := request.url;
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.Body := '';
          response.Resource := nil;
          response.bundle := bundle.Link;
        finally
          keys.Free;
          bundle.Free;
        end;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message);
      recordStack(e);
      raise;
    end;
  end;
end;


function TFhirOperationManager.ExecuteUpdate(upload : boolean; request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  resourceKey : Integer;
  key, nvid, i : Integer;
  s : String;
  tags : TFHIRTagList;
  ok : boolean;
  tnow : TDateTime;
  list : TMatchingResourceList;
begin
  CheckCreateNarrative(request);

  nvid := 0;
  key := 0;
  try
    ok := true;
    if ok and not check(response, request.canWrite(request.ResourceType) or opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;

    if ok and (not check(response, request.Resource <> nil, 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) or
       not check(response, request.ResourceType = request.resource.ResourceType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang)) or
       not check(response, request.id = request.resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+'('+request.id +' / '+request.resource.id+')') or
       not check(response, length(request.id) <= ID_LENGTH, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id]))) then
      ok := false;

    if ok and FRepository.Validate and not upload then
    begin
      if not ExecuteValidation(request, response, 'Update Resource '+CODES_TFHIRResourceType[request.ResourceType]+'/'+request.Id) then
        ok := false
      else
        response.Resource := nil;
    end;


    if request.DefaultSearch then // conditional update
    begin
      list := ResolveSearchId(request.ResourceType, request.compartmentId, request.compartments, request.baseUrl, request.Parameters.Source);
      try
        if (list.Count = 0) then
        begin
          ExecuteCreate(upload, request, response, idMaybeNew, 0);
          result := false;
          ok := false;
        end
        else
        if (list.Count > 1) then
        begin
          ok := false;
          check(response, false, 412, lang, GetFhirMessage('UPDATE_MULTIPLE_MATCHES', lang));
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

    if ok and not check(response, request.Resource.id <> '', 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISSING', lang)+' '+request.id+'/'+request.resource.id+' (3)') Then
      ok := false;
    if ok and not check(response, request.id = request.Resource.id, 400, lang, GetFhirMessage('MSG_RESOURCE_ID_MISMATCH', lang)+' '+request.id+'/'+request.resource.id+' (3)') Then
      ok := false;


    result := true;

    if ok and not FindResource(request.ResourceType, request.Id, true, resourceKey, request, response, request.compartments) Then
    begin
      ExecuteCreate(upload, request, response, idMaybeNew, 0);
      result := false;
      ok := false;
    end;


    if ok and not check(response, not FRepository.ResConfig[request.ResourceType].versionUpdates or (request.ifMatch <> ''), 412, lang, GetFhirMessage('MSG_VERSION_AWARE', lang)) then
      ok := false;


    if ok then
    begin
      key := FRepository.NextVersionKey;
      nvid := FConnection.CountSQL('Select Max(Cast(VersionId as '+DBIntType(FConnection.Owner.Platform)+')) from Versions where ResourceKey = '+IntToStr(resourceKey)) + 1;

      if  (request.IfMatch <> '') or  FRepository.ResConfig[request.ResourceType].versionUpdates then
      begin
        s := request.IfMatch;

        if not check(response, s = inttostr(nvid-1), 412, lang, StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), [inttostr(nvid-1), s])) then
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
        tags.writeTags(request.resource.meta);
        request.Resource.meta.lastUpdated := NowUTC;
        request.Resource.meta.versionId := inttostr(nvid);
        CheckNotRedacted(request.Resource.meta, 'Updating Resource');
        updateProvenance(request.Provenance, CODES_TFHIRResourceType[request.ResourceType], request.Id, inttostr(nvid));
        tnow := request.Resource.meta.lastUpdated.AsUTCDateTime;


        checkProposedContent(request, request.Resource, tags);

        for i := 0 to tags.count - 1 do
          FRepository.RegisterTag(tags[i], FConnection);

        FConnection.execSQL('Update IndexEntries set Flag = 2 where ResourceKey = '+IntToStr(resourceKey));

        // check whether originalId matches?
        // to do: merging

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, TransactionDate, StatedDate, Format, VersionId, Status, '+
          'SessionKey, Tags, XmlContent, XmlSummary, JsonContent, JsonSummary) values (:k, :rk, :td, :sd, :f, :v, 1, :s, :tb, :xc, :xs, :jc, :js)';
        FConnection.prepare;
        try
          FConnection.BindInteger('k', key);
          FConnection.BindInteger('rk', resourceKey);
          FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
          FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
          FConnection.BindString('v', inttostr(nvid));
          request.SubId := inttostr(nvid);
          if request.Session = nil then
            FConnection.BindNull('s')
          else
            FConnection.BindInteger('s', request.Session.Key);
          FConnection.BindInteger('f', 2);
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
        FRepository.SeeResource(resourceKey, key, request.id, false, request.Resource, FConnection, false, request.Session);

        if (response.Resource <> nil) and (response.Resource is TFhirBundle)  then
          response.bundle.entryList.add(request.Resource.Link)
        else
          response.Resource := request.Resource.Link;
        response.versionId := inttostr(nvid);

      finally
        tags.free;
      end;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.lastModifiedDate := tnow;
      response.Location := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.Id+'/_history/'+inttostr(nvid);
      if request.Provenance <> nil then
        SaveProvenance(request.Session, request.Provenance);
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), key, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;


function TFhirOperationManager.ExecuteValidation(request: TFHIRRequest; response: TFHIRResponse; opDesc : String) : boolean;
var
  outcome : TFhirOperationOutcome;
  i : integer;
  buffer : TAdvBuffer;
  mem : TAdvMemoryStream;
  xml : TFHIRComposer;
  vcl : TVclStream;
begin
  try
    if opDesc = '' then
      opDesc := 'Validate resource '+request.id;

    if request.resourceType = frtBinary then
    begin
      outcome := TFhirOperationOutcome.create;
    end
    else if (request.Source <> nil) then
      outcome := FRepository.validator.validateInstance(request.Source, request.PostFormat, risOptional, opDesc, nil)
    else
    begin
      buffer := TAdvBuffer.create;
      try
        mem := TAdvMemoryStream.create;
        vcl := TVclStream.create;
        try
          mem.Buffer := buffer.Link;
          vcl.stream := mem.Link;

          xml := TFHIRXmlComposer.create(request.Lang);
          try
            if request.Resource <> nil then
              xml.Compose(vcl, request.resource, true, nil)
            else if request.bundle <> nil then
              xml.Compose(vcl, request.bundle, true)
            else
              raise Exception.Create('Error: '+response.Message);
          finally
            xml.free;
          end;
        finally
          vcl.free;
          mem.free;
        end;
        outcome := FRepository.validator.validateInstance(buffer, ffXml, risOptional, opDesc, nil);
      finally
        buffer.free;
      end;
    end;

    response.Resource := outcome;
    result := true;
    for i := 0 to outcome.issueList.count - 1 do
      result := result and (outcome.issueList[i].severity in [IssueSeverityInformation, IssueSeverityWarning]);
    if result then
      response.HTTPCode := 200
    else
      response.HTTPCode := 400;
    if request.ResourceType <> frtAuditEvent then
      AuditRest(request.session, request.ip, request.ResourceType, request.id, '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceType <> frtAuditEvent then
        AuditRest(request.session, request.ip, request.ResourceType, request.id, '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFhirOperationManager.ExecuteVersionRead(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : integer;
  field : String;
  comp : TFHIRParserClass;
  needsObject : boolean;
begin
  try
    NotFound(request, response);
    if check(response, request.canRead(request.ResourceType) and opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (length(request.id) <= ID_LENGTH) and (length(request.subid) <= ID_LENGTH) and FindResource(request.ResourceType, request.Id, true, resourceKey, request, response, request.compartments) then
      begin
        VersionNotFound(request, response);
        chooseField(response.Format, request.Summary, field, comp, needsObject);

        FConnection.SQL := 'Select VersionId, StatedDate, '+field+' from Versions where ResourceKey = '+inttostr(resourceKey)+' and VersionId = :v';
        FConnection.Prepare;
        try
          FConnection.BindString('v', request.SubId);
          FConnection.Execute;
          if FConnection.FetchNext then
          Begin
            if (request.IfNoneMatch <> '') and (request.IfNoneMatch = FConnection.GetColStringByName('VersionId')) then
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
    AuditRest(request.session, request.ip, request.ResourceType, request.id, request.SubId, 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, request.SubId, 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirOperationManager.FindResource(aType : TFHIRResourceType; sId: String; bAllowDeleted : boolean; var resourceKey: integer; request: TFHIRRequest; response : TFhirResponse; compartments : String): boolean;
var
  cmp : String;
begin
  if (compartments <> '') then
    cmp := ' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id in ('+compartments+'))'
  else
    cmp := '';

  FConnection.sql := 'select Ids.ResourceKey, Deleted from Ids, Types where Ids.id = :s '+cmp+' and Ids.ResourceTypeKey = Types.ResourceTypeKey and Supported = 1 and ResourceName = :n';
  FConnection.Prepare;
  FConnection.BindString('s', sId);
  FConnection.BindString('n', CODES_TFHIRResourceType[aType]);
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
        check(response, false, 410, lang, StringFormat(GetFhirMessage('MSG_DELETED_ID', lang), [request.id]));
      result := false;
    end;
  end
  else if response <> nil then
    check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [CODES_TFHIRResourceType[request.ResourceType]+'/'+request.id]));
  FConnection.terminate;
end;

function TFhirOperationManager.AddNewResourceId(aType : TFHIRResourceType; id : string; var resourceKey : integer) : Boolean;
var
  itype : integer;
begin
  iType := FRepository.ResConfig[aType].key;// FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[aType]+'''');
  result := iType > 0;
  if result then
  begin
    resourceKey := FRepository.NextResourceKeySetId(aType, id);
    FConnection.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, Deleted) values (:k, :r, :i, null, 0)';
    FConnection.Prepare;
    FConnection.BindInteger('k', resourceKey);
    FConnection.BindInteger('r', itype);
    FConnection.BindString('i', id);
    FConnection.Execute;
    FConnection.Terminate;
    if IsNumericString(id) and StringIsInteger32(id) then
      FConnection.ExecSQL('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(iType)+' and LastId < '+id);
    FConnection.ExecSQL('update IndexEntries set target = '+inttostr(resourceKey)+' where SpaceKey = '+inttostr(iType)+' and value = '''+id+'''');
  end;
End;

function TFhirOperationManager.getNewResourceId(aType: TFHIRResourceType; var id: string; var key: integer): Boolean;
var
  itype : integer;
  guid : boolean;
begin
  guid := false;
  iType := 0;
  FConnection.SQL := 'select ResourceTypeKey, LastId, IdGuids from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[aType]+'''';
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
    FConnection.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, Deleted) values (:k, :r, :i, null, 0)';
    FConnection.Prepare;
    FConnection.BindInteger('k', key);
    FConnection.BindInteger('r', itype);
    FConnection.BindString('i', id);
    FConnection.Execute;
    FConnection.Terminate;
    FConnection.ExecSQL('update IndexEntries set target = '+inttostr(key)+' where SpaceKey = '+inttostr(iType)+' and value = '''+id+'''');
  end;
end;

procedure TFhirOperationManager.NoMatch(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_MATCH', lang), [CODES_TFHIRResourceType[request.ResourceType]+'?'+request.parameters.source]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

procedure TFhirOperationManager.NotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [CODES_TFHIRResourceType[request.ResourceType]+':'+request.Id]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

procedure TFhirOperationManager.VersionNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [CODES_TFHIRResourceType[request.ResourceType]+':'+request.Id+'/_history/'+request.subId]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

function TFhirOperationManager.GetPatientId(): String;
begin

end;

procedure TFhirOperationManager.TypeNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_UNKNOWN_TYPE', lang), [CODES_TFHIRResourceType[request.ResourceType]]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;


procedure TFhirOperationManager.checkNotRedacted(meta: TFhirMeta; msg: String);
begin
  if meta.HasTag('http://hl7.org/fhir/v3/ObservationValue', 'REDACTED') then
    raise Exception.Create('Error '+msg+': This resource has been redacted, and cannot be used as the basis for this operation');
end;

procedure TFhirOperationManager.markRedacted(meta: TFhirMeta);
begin
  meta.addTag('http://hl7.org/fhir/v3/ObservationValue', 'REDACTED', 'redacted');
end;

procedure TFhirOperationManager.unmarkRedacted(meta: TFhirMeta);
begin
  meta.removeTag('http://hl7.org/fhir/v3/ObservationValue', 'REDACTED');
end;

procedure TFhirOperationManager.updateProvenance(prv: TFHIRProvenance; rtype, id, vid: String);
begin
  if prv <> nil then
  begin
    prv.targetList.Clear;
    prv.targetList.Append.reference := rtype+'/'+id+'/_history/'+vid;

    prv.signatureList.Clear;

    // todo: check this....
  end;
end;

function describeResourceTypes(aTypes : TFHIRResourceTypeSet) : String;
var
  a : TFHIRResourceType;
begin
  result := '';
  for a := High(TFHIRResourceType) downto low(TFHIRResourceType) do
    if a in aTypes then
      if result = '' then
        result := CODES_TFHIRResourceType[a]
      else
        result := result+' / '+CODES_TFHIRResourceType[a];
end;

procedure TFhirOperationManager.BuildSearchForm(request: TFHIRRequest; response : TFHIRResponse);
var
  i, j : integer;
  s, pfx, desc : String;
  ix, ix2 : TFhirIndex;
  types : TFHIRResourceTypeSet;
  m : TStringList;
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
'    <title>FHIR RESTful Server - FHIR v'+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+'</title>'#13#10+
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
'<form action="'+CODES_TFhirResourceType[request.ResourceType]+'/_search" method="GET">'#13#10+
'<table class="lines">'#13#10
else
s := s +
'<form action="_search" method="GET">'#13#10+
'<table class="lines">'#13#10;

  if request.ResourceType = frtNull then
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
    s := s +'<tr><td colspan="4"><b>'+CODES_TFHIRResourceType[request.ResourceType]+':</b></td></tr>'+#13#10;
    for i := 0 to FRepository.Indexes.Indexes.Count - 1 Do
    begin
      ix := FRepository.Indexes.Indexes[i];
      if (ix.ResourceType = request.ResourceType) and (ix.TargetTypes = []) then
      begin
        desc := FormatTextToHTML(GetFhirMessage('ndx-'+CODES_TFHIRResourceType[request.ResourceType]+'-'+ix.name, lang, ix.Description));
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

    for i := 0 to FRepository.Indexes.Indexes.Count - 1 Do
    begin
      ix := FRepository.Indexes.Indexes[i];
      if (ix.ResourceType = request.ResourceType) and (ix.TargetTypes <> []) then
      begin
        pfx := ix.Name;
        types := ix.TargetTypes;
        s := s +'<tr><td colspan="4"><b>'+ix.Name+'</b> ('+describeResourceTypes(types)+')<b>:</b></td></tr>'+#13#10;
        m := TStringList.create;
        try
          for j := 0 to FRepository.Indexes.Indexes.Count - 1 Do
          begin
            ix2 := FRepository.Indexes.Indexes[j];
            if (ix2.ResourceType in types) and (m.IndexOf(ix2.Name) = -1) then
            begin
              desc := FormatTextToHTML(GetFhirMessage('ndx-'+CODES_TFHIRResourceType[request.ResourceType]+'-'+ix2.name, lang, ix2.Description));
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
  for i := 0 to FRepository.Indexes.Indexes.Count - 1 Do
  begin
    ix := FRepository.Indexes.Indexes[i];
    if (ix.ResourceType = request.ResourceType) or ((request.ResourceType = frtNull) and (ix.Name.startsWith('_'))) then
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


procedure TFhirOperationManager.CreateIndexer;
begin
  if FIndexer = nil then
  begin
    FSpaces := TFHIRIndexSpaces.Create(FConnection);
    FIndexer := TFHIRIndexManager.Create(FSpaces.Link as TFhirIndexSpaces, FRepository.Indexes.Link);
    FIndexer.TerminologyServer := FRepository.TerminologyServer.Link;
    FIndexer.Bases := FRepository.Bases;
    FIndexer.KeyEvent := FRepository.GetNextKey;
  end;
end;

procedure TFhirOperationManager.SetConnection(const Value: TKDBConnection);
begin
  FConnection := Value;
end;

procedure TFhirOperationManager.ExecuteUpload(request: TFHIRRequest; response: TFHIRResponse);
var
  s : String;
  ok : boolean;
begin
  try
    ok := true;
    if not check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;

    if ok and not check(response, request.canWrite(request.ResourceType) and (request.Resource <> nil) or ((request.bundle <> nil) and (request.bundle.entryList.count > 0)), 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) then
      ok := false;
    if ok and (request.Resource <> nil) then
      if not check(response, request.ResourceType = request.resource.ResourceType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang)) then
        ok := false;
    // todo: check version id integrity
    // todo: check version integrity

    if not ok then
      // do nothing
    else if request.bundle <> nil then
    begin
      ExecuteTransaction(true, request, response);
      exit;
    end
    else
    begin
      ExecuteCreate(true, request, response, idMaybeNew, 0);
      s := '1 new resource created @'+request.id;
    end;

    if ok then
    begin
      response.HTTPCode := 202;
      response.Message := 'Accepted';
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
    '  '+FOwnerName+' FHIR '+GetFhirMessage('NAME_IMPLEMENTATION', lang)+#13#10+
    '  &nbsp;'#13#10+
    '  '+GetFhirMessage('NAME_VERSION', lang)+' '+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+#13#10;

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
    AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, 500, '', e.message);
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

function TFhirOperationManager.ResolveSearchId(atype : TFHIRResourceType; compartmentId, compartments : String; baseURL, params : String) : TMatchingResourceList;
var
  sp : TSearchProcessor;
  p : TParseMap;
  key : integer;
  item : TMatchingResource;
begin
  if aType = frtNull then
    key := 0
  else
  begin
    key := FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[aType]+'''');
    assert(key > 0);
  end;
  p := TParseMap.create(params);
  result := TMatchingResourceList.Create;
  try
    sp := TSearchProcessor.create;
    try
      sp.typekey := key;
      sp.type_ := atype;
      sp.compartmentId := compartmentId;
      sp.compartments := compartments;
      sp.baseURL := baseURL;
      sp.lang := lang;
      sp.params := p;
      CreateIndexer;
      sp.indexes := FRepository.Indexes.Link;
      sp.repository := FRepository.Link;
      sp.countAllowed := false;

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


//function TFhirOperationManager.ResolveSearchIdCount(atype: TFHIRResourceType; compartmentId, compartments, baseURL, params: String): integer;
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
procedure TFhirOperationManager.SaveProvenance(session: TFhirSession; prv: TFHIRProvenance);
begin
  prv.id := '';

  FRepository.QueueResource(prv);
end;

function TFhirOperationManager.scanId(request : TFHIRRequest; entry : TFHIRBundleEntry; ids : TFHIRTransactionEntryList; index : integer) : TFHIRTransactionEntry;
var
  id : TFHIRTransactionEntry;
  i, k : integer;
  sId, s, b : String;
  sParts : TArray<String>;
  baseok : boolean;
  aType : TFHIRResourceType;
  list : TMatchingResourceList;
begin
  if entry.resource <> nil then
    aType := entry.resource.ResourceType
  else if (entry.request = nil) or not (entry.request.method in [HttpVerbDELETE, HttpVerbGET]) or (entry.request.url = '') then // must be deleting something
    raise Exception.Create('A resource must be provided unless the entry has a transaction with method = delete and a url (entry '+inttostr(index+1)+')')
  else
  begin
    s := entry.request.url;
    if s.contains('?') then
      s := s.subString(0, s.indexOf('?'));
    sParts := s.Split(['/']);
    i := StringArrayIndexOfSensitive(CODES_TFhirResourceType, sParts[0]);
    if i = -1 then
      raise Exception.Create('Unknown resource type '+sParts[0]+' in deletion URL');
    aType := TFhirResourceType(i);
  end;

  if (entry.request <> nil) and (entry.request.method = HttpVerbDELETE) and not FRepository.ResConfig[aType].cmdDelete then
    Raise Exception.create('Deleting Resource '+CODES_TFHIRResourceType[entry.resource.ResourceType]+' is not supported in Transactions (entry '+inttostr(index+1)+')');


  if not FRepository.ResConfig[aType].cmdUpdate and not FRepository.ResConfig[aType].cmdCreate then
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
              if not GetNewResourceId(entry.resource.resourceType,  sId, k) then
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
                  if not GetNewResourceId(entry.resource.resourceType,  sId, k) then
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
              raise Exception.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [CODES_TFHIRResourceType[id.restype]+'/'+id.id])+' (entry '+inttostr(index+1)+')');

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
                  raise Exception.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [CODES_TFHIRResourceType[id.restype]+'/'+id.id])+' (entry '+inttostr(index+1)+')');

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
        for b in FRepository.bases do
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
            raise Exception.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [CODES_TFHIRResourceType[id.restype]+'/'+id.id])+' (entry '+inttostr(index+1)+')');
      end;
      case id.state of
        tesIgnore: ; // yup, ignore it
        tesCreate:
          begin
            if not GetNewResourceId(entry.resource.resourceType,  sId, k) then
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


procedure TFhirOperationManager.FixXhtmlUrls(lang, base : String; ids : TFHIRTransactionEntryList; node : TFhirXHtmlNode);
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
      while (j = -1) and (k < FRepository.Bases.Count - 1) do
      begin
        j := ids.IndexByName(FRepository.Bases[k]+s);
        inc(k);
      end;

      if (j > -1) then
      begin
        if (vhist = '') then
          node.Attributes.SetValue('href', CODES_TFHIRResourceType[ids[j].resType]+'/'+ids[j].id)
        else if (ids[j].version <> '') and (ids[j].version <> vHist) then
          Raise Exception.create(StringFormat(GetFhirMessage('Version ID Mismatch for '+url+' in narrative: reference to version '+vHist+', reference is '+ids[j].version, lang), [s]))
        else
          node.Attributes.SetValue('href', CODES_TFHIRResourceType[ids[j].resType]+'/'+ids[j].id+'/_history/'+inttostr(ids[j].outcomeVersion));
      end
      else if isLogicalReference(s) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [s]));
    end;
    if (node.Name = 'img') and (node.Attributes.Get('src') <> '') then
    begin
      s := node.Attributes.Get('src');
      j := ids.IndexByName(s);
      if (j > -1) then
        node.Attributes.SetValue('src', base+CODES_TFHIRResourceType[ids[j].resType]+'/'+ids[j].id)
      else if isLogicalReference(s) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [s]));
    end;
    for i := 0 to node.ChildNodes.count - 1 do
      FixXhtmlUrls(lang, base, ids, node.childNodes[i]);
  end;
end;


procedure TFhirOperationManager.adjustReferences(te : TFHIRTransactionEntry; base : String; entry : TFHIRBundleEntry; ids : TFHIRTransactionEntryList);
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

      if (isHistoryURL(url)) then
        splitHistoryUrl(url, vhist)
      else
        vHist := '';

      j := ids.IndexByName(url);
      k := 0;
      while (j = -1) and (k < FRepository.Bases.Count - 1) do
      begin
        j := ids.IndexByName(FRepository.Bases[k]+ref.reference);
        inc(k);
      end;
      if (j <> -1) then
      begin
        if (vhist = '') then
          ref.reference := CODES_TFHIRResourceType[ids[j].resType]+'/'+ids[j].id
        else if (ids[j].version <> '') and (ids[j].version <> vHist) then
          Raise Exception.create(StringFormat(GetFhirMessage('Version ID Mismatch for '+url+': reference to version '+vHist+', reference is '+ids[j].version, lang), [ref.reference]))
        else
          ref.reference :=  CODES_TFHIRResourceType[ids[j].resType]+'/'+ids[j].id+'/_history/'+inttostr(ids[j].outcomeVersion);
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
        attachment.url := CODES_TFHIRResourceType[ids[j].resType]+'/'+ids[j].id
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
        extension.url := base+CODES_TFHIRResourceType[ids[j].resType]+'/'+ids[j].id
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
          attachment.url := base+CODES_TFHIRResourceType[ids[j].resType]+'/'+ids[j].id
        else if isLogicalReference(Attachment.url) then
          Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [attachment.url]));
      end;
    end;

  if (TFhirDomainResource(entry.resource).text <> nil) then
    FixXhtmlUrls(lang, base, ids, TFhirDomainResource(entry.resource).text.div_);
end;

function TFhirOperationManager.commitResource(request: TFHIRRequest; response : TFHIRResponse; upload : boolean; entry: TFHIRBundleEntry; i : integer; id: TFHIRTransactionEntry; session : TFHIRSession; resp : TFHIRBundle) : Boolean;
var
  ne : TFhirBundleEntry;
begin
  if (entry.request <> nil) then
    writelnt(inttostr(i)+': '+entry.request.methodElement.value+' '+id.summary)
  else
    writelnt(inttostr(i)+': '+id.summary);

  request.Id := id.id;
  if entry.resource <> nil then
    entry.resource.id := id.id;;

  request.originalId := id.originalId;
  request.SubId := '';
  request.ResourceType := ID.resType;
  request.resource := entry.resource.link;
  response.Location := '';
  response.versionId := '';

  //todo: check session

  case id.state of
    tesIgnore: ;  // yup, ignore it
    tesRead: executeReadInTransaction(entry.request, request, response);
    tesCreate: ExecuteCreate(upload, request, response, idIsNew, id.key);
    tesUpdate:
      begin
//        if (entry.request <> nil) and (entry.request.url.contains('?')) then
//        begin
//          if Id.count <> ResolveSearchIdCount(id.resType, request.compartmentId, request.compartments, request.baseUrl, entry.request.url.substring(entry.request.url.IndexOf('?')+1)) then
//            raise Exception.Create('error processing batch - id clash: one of the create statements altered the processing of a conditional update: '+entry.request.url);
//        end;
        ExecuteUpdate(upload, request, response);
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
    result := check(response, response.HTTPCode < 300, response.HTTPCode, lang, response.Message);
  ne := id.entry;
  ne.resource := response.resource.Link;
  ne.response := TFhirBundleEntryResponse.Create;
  ne.response.status := inttostr(response.HTTPCode);
  ne.response.location := response.Location;
  ne.response.etag := 'W/'+response.versionId;
  ne.response.lastModified := TDateAndTime.CreateUTC(response.lastModifiedDate);
end;

procedure ignoreEntry(req, resp : TFHIRBundleEntry);
begin
  resp.response := TFhirBundleEntryResponse.Create;
  resp.response.status := '200';
  if (req.request <> nil) and (req.request.method = HttpVerbDELETE) then
    resp.response.status := '404';
end;



procedure TFhirOperationManager.ExecuteTransaction(upload : boolean; request: TFHIRRequest; response: TFHIRResponse);
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
    if not check(response, FRepository.SupportTransaction, 405, lang, 'Transaction Operations not allowed') then
      ok := false;
    if ok and not check(response, request.Resource is TFhirBundle, 400, lang, 'A bundle is required for a Transaction operation') then
      ok := false;

    if ok then
    if request.Bundle.type_ = BundleTypeBatch then
      executeBatch(request, response)
    else
    begin
      request.Source := nil; // ignore that now
      resp := TFHIRBundle.create(BundleTypeTransactionResponse);
      ids := TFHIRTransactionEntryList.create;
      try
        ids.FDropDuplicates := request.bundle.Tags['duplicates'] = 'ignore';
//        resp.base := request.baseUrl;
        ids.SortedByName;
        resp.id := FhirGUIDToString(CreateGuid);


        // first pass: scan ids
        for i := 0 to request.bundle.entryList.count - 1 do
        begin
          request.bundle.entryList[i].Tag := scanId(request, request.bundle.entryList[i], ids, i).Link;
        end;

        // third pass: reassign references
        for i := 0 to request.bundle.entryList.count - 1 do
        begin
          entry := request.bundle.entryList[i].Tag as TFHIRTransactionEntry;

          if not entry.ignore and not entry.deleted then
            adjustReferences(request.bundle.entryList[i].Tag as TFHIRTransactionEntry, request.baseUrl, request.bundle.entryList[i], ids);
        end;

        // four pass: commit resources
         bundle := request.bundle.Link;
        try
          for i := 0 to bundle.entryList.count - 1 do
          begin
             ne := resp.entryList.Append;
//             ne.request := bundle.entryList[i].request.Link;
            (bundle.entryList[i].Tag as TFHIRTransactionEntry).entry := ne;
          end;

          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesCreate then
              ignoreEntry(bundle.entryList[i], resp.entryList[i]);

          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesCreate then
              if not commitResource(request, response, upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesUpdate then
              if not commitResource(request, response, upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesDelete then
              if not commitResource(request, response, upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesRead then
              if not commitResource(request, response, upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;

        finally
          bundle.free;
        end;
        response.HTTPCode := 202;
        response.Message := 'Accepted';
        response.bundle := resp.Link;
      finally
        ids.free;
        resp.free;
      end;
    end;
    if request.Resource <> nil then // batch
      AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.Resource <> nil then // batch
        AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFhirOperationManager.ExecuteBatch(request: TFHIRRequest; response: TFHIRResponse);
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
    req := request.Bundle.Link;
    resp := TFHIRBundle.create(BundleTypeBatchResponse);
    try
      resp.id := GUIDToString(CreateGUID).ToLower;
      for src in req.entryList do
      begin
        dest := resp.entryList.Append;
        try
          write('.');
          if src.request = nil then
            raise Exception.Create('No request details');
          dest.request := src.request.Clone;
          request.reset;
          url := request.preAnalyse(src.request.url);
          request.analyse(CODES_TFhirHttpVerbEnum[src.request.method], url, dummy);
          request.IfNoneMatch := src.request.ifNoneMatch;
          if src.request.ifModifiedSince <> nil then
            request.IfModifiedSince := src.request.ifModifiedSince.AsUTCDateTime;
          request.IfMatch := src.request.ifMatch;
          request.IfNoneExist := src.request.ifNoneExist;
          request.resource := src.resource.link;
          request.Source := TAdvBuffer.Create;
          request.PostFormat := ffXml;
          if FRepository.validate and (request.resource <> nil) then
          begin
            comp := TFHIRXmlComposer.Create('en');
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
          Execute(request, response, false);
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
      response.HTTPCode := 202;
      response.Message := 'Accepted';
      response.bundle := resp.Link;
      writelnt('done');
    finally
      req.free;
      resp.free;
    end;
    AuditRest(request.session, request.ip, frtNull, '', '', 0, fcmdBatch, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, frtNull, '', '', 0, fcmdBatch, request.Provenance, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFhirOperationManager.ExecuteOperation(request: TFHIRRequest; response: TFHIRResponse);

var
  i : integer;
  op : TFhirOperation;
begin
  for i := 0 to FOperations.count - 1 do
  begin
    op := TFhirOperation(FOperations[i]);
    if (op.HandlesRequest(request)) then
    begin
      op.Execute(self, request, response);
      exit;
    end;
  end;
  raise Exception.Create('Unknown operation '+Codes_TFHIRResourceType[request.ResourceType]+'/$'+request.OperationName);

end;

function typeForReference(ref : String) : TFhirResourceType;
var
  list : TArray<String>;
  i : integer;
begin
  list := ref.Split(['/']);
  i := StringArrayIndexOfSensitive(CODES_TFhirResourceType, list[0]);
  if (i > 0) then
    result := TFhirResourceType(i)
  else
    result := frtNull;
end;

procedure TFhirOperationManager.CollectIncludes(session : TFhirSession; includes: TReferenceList; resource: TFHIRResource; path: String);
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


function TFhirOperationManager.opAllowed(resource: TFHIRResourceType; command: TFHIRCommandType): Boolean;
begin
  case command of
    fcmdUnknown : result := false;
    fcmdRead : result := FRepository.ResConfig[resource].Supported;
    fcmdVersionRead : result := FRepository.ResConfig[resource].Supported;
    fcmdUpdate : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdUpdate;
    fcmdDelete : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdDelete;
    fcmdHistoryInstance : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdHistoryInstance;
    fcmdHistoryType : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdHistoryType;
    fcmdHistorySystem : result := FRepository.SupportSystemHistory;
    fcmdValidate : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdValidate;
    fcmdSearch : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdSearch;
    fcmdCreate : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdCreate;
    fcmdConformanceStmt : result := true;
    fcmdUpload, fcmdTransaction : result := FRepository.SupportTransaction;
    fcmdUpdateMeta : result := true;
    fcmdDeleteMeta : result := true;
    fcmdOperation : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdOperation;
  else
    result := false;
  end;
end;

//function TFhirOperationManager.TypeForKey(key: integer): TFHIRResourceType;
//var
//  a : TFHIRResourceType;
//  b : boolean;
//begin
//  result := frtNull;
//  b := false;
//  for a := low(TFHIRResourceType) to high(TFHIRResourceType) do
//    if  FRepository.ResConfig[a].key = key then
//    begin
//      result := a;
//      b := true;
//    end;
//  if not b then
//    raise exception.create('key '+inttostr(key)+' not found');
//end;

procedure TFhirOperationManager.CommitTags(tags: TFHIRTagList; key: integer);
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
      FConnection.BindInteger('k', FRepository.NextTagVersionKey);
      FConnection.BindInteger('t', tags[i].Key);
      FConnection.BindString('l', tags[i].Display);

      FConnection.Execute;
    end;
  finally
    FConnection.terminate;
  end;
end;

procedure TFhirOperationManager.ProcessBlob(request: TFHIRRequest; response: TFHIRResponse; field : String; comp : TFHIRParserClass);
var
  parser : TFHIRParser;
  mem : TBytesStream;
begin
  mem := TBytesStream.Create(FConnection.ColBlobByName[field]);
  try
    parser := comp.Create(lang);
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


procedure TFhirOperationManager.LoadTags(tags: TFHIRTagList; ResourceKey: integer);
var
  t, tf : TFhirTag;
  lbl : String;
begin
  FConnection.SQL := 'Select * from VersionTags where ResourceVersionKey = (select MostRecent from Ids where ResourceKey = '+inttostr(resourcekey)+')';
  FConnection.Prepare;
  FConnection.Execute;
  while FConnection.FetchNext do
  begin
    t := FRepository.GetTagByKey(FConnection.ColIntegerByName['TagKey']);
    if FConnection.ColStringByName['Display'] <> '' then
      lbl := FConnection.ColStringByName['Display']
    else
      lbl := t.Display;

    tf := tags.findTag(t.system, t.code);
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

function TFhirOperationManager.LookupReference(context : TFHIRRequest; id: String): TResourceWithReference;
var
  a, atype : TFhirResourceType;
  resourceKey : integer;
  parser : TFHIRParser;
  b, base : String;
  s : TBytes;
begin
  base := context.baseUrl;
  if id.StartsWith(base) then
    id := id.Substring(base.Length)
  else for b in FRepository.Bases do
    if id.StartsWith(b) then
      id := id.Substring(b.Length);
  aType := frtNull;
  for a := low(TFHIRResourceType) to High(TFHIRResourceType) do
    if id.startsWith(CODES_TFhirResourceType[a] + '/') then
    begin
      aType:= a;
      id := id.Substring(9);
    end;

  result := nil;
  if (aType <> frtNull) and (length(id) <= ID_LENGTH) and FindResource(aType, id, false, resourceKey, nil, nil, '') then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['XmlContent'];
        parser := MakeParser(lang, ffXml, s, xppDrop);
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

//procedure TFhirOperationManager.ExecuteDeleteMeta(request: TFHIRRequest; response: TFHIRResponse);
//var
//  resourceKey : Integer;
//  resourceVersionKey : Integer;
//  currentResourceVersionKey : Integer;
//  originalId : String;
//  i, n : integer;
//  tags : TFHIRCodingList;
//  t : String;
//  ok, deleted : boolean;
//  blob : TBytes;
//  parser : TFHIRParser;
//
//  rp : TFhirParameters;
//  meta : TFhirMeta;
//
//begin
//  currentResourceVersionKey := 0;
//  try
//    ok := true;
//    if not check(response, request.canWrite(request.ResourceType) and opAllowed(request.ResourceType, fcmdDeleteMeta), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
//      ok := false;
//    if ok then
//      NotFound(request, response);
//    if ok and not FindResource(request.ResourceType, request.Id, true, resourceKey, originalId, request, response, request.compartments) then
//      ok := false;
//    if not ok then
//      // nothing
//    else
//    begin
//      currentResourceVersionKey := StrToInt(FConnection.Lookup('Ids', 'ResourceKey', inttostr(Resourcekey), 'MostRecent', '0'));
//      if request.SubId <> '' then
//      begin
//        if not check(response, opAllowed(request.ResourceType, fcmdVersionRead {todo}), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
//          exit;
//        if Not FindResourceVersion(request.ResourceType, request.Id, request.SubId, false, resourceVersionKey, request, response) then
//          exit;
//      end
//      else
//        ResourceVersionKey := currentResourceVersionKey;
//    end;
//
//    if ok then
//    begin
//      tags := TFHIRCodingList.create;
//      try
//        LoadTags(tags, ResourceKey);
//        tags.EraseTags(request.meta);
//
//
//        FConnection.ExecSQL('delete from VersionTags where ResourceVersionKey = '+inttostr(resourceVersionKey));
//        CommitTags(tags, resourceVersionKey);
//
//          // changing the tags might change the indexing
//          FConnection.SQL := 'Select Deleted, Content from Versions where ResourceVersionKey = '+inttostr(resourceVersionKey);
//          FConnection.prepare;
//          FConnection.Execute;
//          if not FConnection.FetchNext then
//            raise Exception.Create('Internal Error fetching current content');
//          blob := FConnection.ColBlobByName['Content'];
//          deleted := FConnection.ColIntegerByName['Deleted'] = 1;
//          FConnection.Terminate;
//          parser := MakeParser('en', ffXml, blob, xppDrop);
//          try
//
//            FConnection.SQL := 'Update Versions set Content = :c where ResourceVersionKey = '+inttostr(resourceVersionKey);
//            FConnection.prepare;
//            if deleted then
//            begin
//              tags.WriteTags(parser.meta);
//              FConnection.BindBlobFromBytes('c', EncodeMeta(parser.meta));
//              response.meta := parser.meta.Link;
//            end
//            else
//            begin
//              tags.WriteTags(parser.resource.meta);
//              FConnection.BindBlobFromBytes('c', EncodeResource(parser.resource));
//              response.meta := parser.resource.meta.Link;
//            end;
//            FConnection.Execute;
//            FConnection.Terminate;
//            if not deleted and (resourceVersionKey = currentResourceVersionKey) then
//
//              FIndexer.execute(resourceKey, request.Id, parser.resource, tags);
//          finally
//            parser.free;
//          end;
//      finally
//        tags.free;
//      end;
//      response.HTTPCode := 200;
//      response.Message := 'OK';
//      response.resource := nil; // clear the error message
//    end;
//    AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, request.Provenance, response.httpCode, t, response.message);
//  except
//    on e: exception do
//    begin
//      AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, request.Provenance, 500, t, e.message);
//      recordStack(e);
// raise;
//    end;
//  end;
//end;


//procedure TFhirOperationManager.ExecuteGenerateQA(request: TFHIRRequest; response: TFHIRResponse);
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
//    if check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
//    begin
//      if FindResource(request.ResourceType, request.Id, false, resourceKey, request, response, request.compartments) then
//      begin
//        source := GetResourceByKey(resourceKey);
//        try
//          // for now, we simply get the base profile
//          if source is TFHirStructureDefinition then
//            raise Exception.Create('Editing a profile via a profile questionnaire is not supported');
//
//          profile := GetProfileByURL('http://hl7.org/fhir/Profile/'+Codes_TFHIRResourceType[source.ResourceType], id);
//          try
//            fid := request.baseUrl+'Profile/'+id+'/$questionnaire';
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
//                response.link_List.AddRelRef('edit-post', request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/$qa-post');
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
//    AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
//  except
//    on e: exception do
//    begin
//      AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
//      recordStack(e);
// raise;
//    end;
//  end;
//end;
//
//

//procedure TFhirOperationManager.ExecuteGetMeta(request: TFHIRRequest;  response: TFHIRResponse);
//var
//  tag : TFHIRAtomCategory;
//  ok : boolean;
//  {$IFNDEF FHIR-META}
//  meta : TFhirMeta;
//  coding : TFhirCoding;
//  uri : TFhirUri;
//
//begin
//  try
//    ok := true;
//    if request.ResourceType = frtNull then
//    begin
//    // well, this operation is always allowed?
//      FConnection.sql := 'Select Kind, Uri, Code, Display, (select count(*) from VersionTags where Tags.TagKey = VersionTags.TagKey and ResourceVersionKey in (select MostRecent from Ids)) as usecount from Tags  where TagKey in (Select '+'TagKey from VersionTags where ResourceVersionKey in (select MostRecent from Ids)) order by Kind, Uri, Code'
//    end
//    else if request.Id = '' then
//    begin
//      if not check(response, request.canRead(request.ResourceType) and false {todo: security} and FRepository.ResConfig[request.ResourceType].Supported, 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
//        ok := false
//      else
//        FConnection.sql := 'Select Kind, Uri, Code, Display, (select count(*) from VersionTags where Tags.TagKey = VersionTags.TagKey and ResourceVersionKey in (select MostRecent from Ids where ResourceTypeKey = '+inttostr(FRepository.ResConfig[request.ResourceType].Key)+')) as usecount  from Tags where TagKey in (Select TagKey from VersionTags where ResourceVersionKey in (select MostRecent from Ids where ResourceTypeKey = '+inttostr(FRepository.ResConfig[request.ResourceType].Key)+')) order by Kind, Uri, Code'
//    end
//    else if request.SubId = '' then
//    begin
//      if not check(response, request.canRead(request.ResourceType) and opAllowed(request.ResourceType, fcmdRead), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
//         Ok := false
//      else
//        FConnection.sql := 'Select Kind, Uri, Code, VersionTags.Display, 1 as UseCount from Tags, VersionTags '+
//        'where Tags.TagKey = VersionTags.TagKey and VersionTags.ResourceVersionKey in (Select ResourceVersionKey from Versions where ResourceVersionKey in (select MostRecent from Ids where Id = :id and ResourceTypeKey = '+inttostr(FRepository.ResConfig[request.ResourceType].Key)+')) order by Kind, Uri, Code'
//    end
//    else
//    begin
//      if not check(response, request.canRead(request.ResourceType) and opAllowed(request.ResourceType, fcmdVersionRead), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
//        ok := false
//      else
//        FConnection.sql := 'Select Kind, Uri, Code, VersionTags.Display, 1 as UseCount  from Tags, VersionTags '+
//        'where Tags.TagKey = VersionTags.TagKey and VersionTags.ResourceVersionKey in (Select ResourceVersionKey from Versions where VersionId = :vid and ResourceKey in (select ResourceKey from Ids where Id = :id and ResourceTypeKey = '+inttostr(FRepository.ResConfig[request.ResourceType].Key)+')) order by Kind, Uri, Code';
//    end;
//
//    if ok then
//    begin
//      FConnection.Prepare;
//      if request.Id <> '' then
//      begin
//        FConnection.BindString('id', request.Id);
//        if request.SubId <> '' then
//          FConnection.BindString('vid', request.SubId);
//      end;
//      FConnection.execute;
//      meta := TFhirMeta.Create;
//      try
//        while FConnection.FetchNext do
//        begin
//          if TFHIRTagKind(FConnection.ColIntegerByName['Kind']) = tkProfile then
//          begin
//            uri := meta.profileList.Append;
//            uri.value := FConnection.ColStringByName['Code'];
//            if request.Id = '' then
//              uri.AddExtension('http://www.healthintersections.com.au/fhir/ExtensionDefinition/usecount', TFhirInteger.Create(FConnection.ColStringByName['UseCount']));
//          end
//          else
//          begin
//            coding := TFhirCoding.create;
//            try
//              coding.system := FConnection.ColStringByName['Uri'];
//              coding.code := FConnection.ColStringByName['Code'];
//              coding.display := FConnection.ColStringByName['Display'];
//              if request.Id = '' then
//                coding.AddExtension('http://www.healthintersections.com.au/fhir/ExtensionDefinition/usecount', TFhirInteger.Create(FConnection.ColStringByName['UseCount']));
//              if TFHIRTagKind(FConnection.ColIntegerByName['Kind']) = tkTag then
//                meta.tagList.add(coding.Link)
//              else
//                meta.securityList.add(coding.Link)
//            finally
//              coding.Free;
//            end;
//          end;
//        end;
//        response.meta := meta.Link;
//      finally
//        meta.Free;
//      end;
//
//      FConnection.terminate;
//      response.HTTPCode := 200;
//      response.Message := 'OK';
//      response.Body := '';
//    end;
//    AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message);
//  except
//    on e: exception do
//    begin
//      AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message);
//      recordStack(e);
// raise;
//    end;
//  end;
//end;
//
//
//procedure TFhirOperationManager.ExecuteUpdateMeta(request: TFHIRRequest; response: TFHIRResponse);
//var
//  resourceKey : Integer;
//  resourceVersionKey : Integer;
//  currentResourceVersionKey : Integer;
//  originalId : String;
//  i : integer;
//  tags : TFHIRCodingList;
//  t : string;
//  ok : boolean;
//  blob : TBytes;
//  parser : TFHIRParser;
//  xml : TFHIRXmlComposer;
//  stream : TBytesStream;
//  deleted : boolean;
//begin
//  currentResourceVersionKey := 0;
//  try
//    ok := true;
//    if not check(response, request.canWrite(request.ResourceType) and opAllowed(request.ResourceType, fcmdUpdateMeta), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
//      ok := false;
//    if ok then
//      NotFound(request, response);
//    if ok and not FindResource(request.ResourceType, request.Id, true, resourceKey, originalId, request, response, request.compartments) then
//      ok := false;
//    if not ok then
//      // nothing
//    else
//    begin
//      CurrentResourceVersionKey := StrToInt(FConnection.Lookup('Ids', 'ResourceKey', inttostr(Resourcekey), 'MostRecent', '0'));
//      if request.SubId <> '' then
//      begin
//        if not check(response, opAllowed(request.ResourceType, fcmdVersionRead {todo}), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
//          ok := false;
//       if ok and Not FindResourceVersion(request.ResourceType, request.Id, request.SubId, false, resourceVersionKey, request, response) then
//         ok := false;
//      end
//      else
//        resourceVersionKey := currentResourceVersionKey;
//    end;
//
//    if ok then
//    begin
//      tags := TFHIRCodingList.create;
//      try
//        tags.CopyTags(request.meta);
//        LoadTags(tags, ResourceKey);
//
//        for i := 0 to tags.count - 1 do
//          FRepository.RegisterTag(tags[i], FConnection);
//
//        FConnection.ExecSQL('delete from VersionTags where ResourceVersionKey = '+inttostr(resourceVersionKey));
//        CommitTags(tags, resourceVersionKey);
//
//          FConnection.SQL := 'Select Deleted, Content from Versions where ResourceVersionKey = '+inttostr(resourceVersionKey);
//          FConnection.prepare;
//          FConnection.Execute;
//          if not FConnection.FetchNext then
//            raise Exception.Create('Internal Error fetching current content');
//          blob := FConnection.ColBlobByName['Content'];
//          deleted := FConnection.ColIntegerByName['Deleted'] = 1;
//          FConnection.Terminate;
//          parser := MakeParser('en', ffXml, blob, xppDrop);
//          try
//
//            FConnection.SQL := 'Update Versions set Content = :c where ResourceVersionKey = '+inttostr(resourceVersionKey);
//            FConnection.prepare;
//            if deleted then
//            begin
//              tags.WriteTags(parser.meta);
//              FConnection.BindBlobFromBytes('c', EncodeMeta(parser.meta));
//              response.meta := parser.meta.Link;
//            end
//            else
//            begin
//              tags.WriteTags(parser.resource.meta);
//              FConnection.BindBlobFromBytes('c', EncodeResource(parser.resource));
//              response.meta := parser.resource.meta.Link;
//            end;
//            FConnection.Execute;
//            FConnection.Terminate;
//            if not deleted and (resourceVersionKey = currentResourceVersionKey) then
//
//              FIndexer.execute(resourceKey, request.Id, parser.resource, tags);
//          finally
//            parser.free;
//          end;
//      finally
//        tags.free;
//      end;
//      response.HTTPCode := 200;
//      response.Message := 'OK';
//      response.resource := nil; // clear the error message
//    end;
//    AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, request.Provenance, response.httpCode, t, response.message);
//  except
//    on e: exception do
//    begin
//      AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, request.Provenance, 500, t, e.message);
//      recordStack(e);
// raise;
//    end;
//  end;
//end;


//function TFhirOperationManager.FindResourceVersion(aType: TFHIRResourceType; sId, sVersionId: String; bAllowDeleted: boolean; var resourceVersionKey: integer; request: TFHIRRequest; response: TFHIRResponse): boolean;
//begin
//  FConnection.sql := 'select ResourceVersionKey from Ids, Types, Versions '+
//    'where Ids.Id = :id and Ids.ResourceTypeKey = Types.ResourceTypeKey '+
//    ' and Supported = 1 and Types.ResourceName = :n and Ids.ResourceKey = Versions.ResourceKey and Versions.Status < 2 and Versions.VersionId = :vid';
//  FConnection.Prepare;
//  FConnection.BindString('id', sId);
//  FConnection.BindString('vid', sVersionId);
//  FConnection.BindString('n', CODES_TFHIRResourceType[aType]);
//  FConnection.execute;
//  result := FConnection.FetchNext;
//  if result then
//    resourceVersionKey := FConnection.ColIntegerByName['ResourceVersionKey']
//  else
//    check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.subid]));
//  FConnection.terminate;
//end;
//
//function TFhirOperationManager.IdentifyValueset(request: TFHIRRequest; resource : TFHIRResource; params: TParseMap; base : String; var used, cacheId : string; allowNull : boolean = false) : TFHIRValueSet;
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

//procedure TFhirOperationManager.ExecuteQuestionnaireGeneration(request: TFHIRRequest; response : TFHIRResponse);
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
//    if check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
//    begin
//      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and FindResource(frtStructureDefinition, request.Id, false, resourceKey, request, response, request.compartments)) then
//      begin
//        profile := nil;
//        try
//          // first, we have to identify the value set.
//          id := request.Id;
//          if request.Id <> '' then // and it must exist, because of the check above
//            profile := GetProfileById(request, request.Id, request.baseUrl)
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
//            fid := request.baseUrl+'Profile/'+id+'/$questionnaire';
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
//        op := FRepository.validator.validateInstance(nil, response.Resource, 'Produce Questionnaire', nil);
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
//    AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
//  except
//    on e: exception do
//    begin
//      AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
//      recordStack(e);
// raise;
//    end;
//  end;
//end;
//
function TFhirOperationManager.GetValueSetById(request: TFHIRRequest; id, base: String): TFHIRValueSet;
var
  resourceKey : integer;
  parser : TFHIRParser;
  b : String;
  s : TBytes;
begin
  if id.StartsWith(base) then
    id := id.Substring(base.Length)
  else for b in FRepository.Bases do
    if id.StartsWith(b) then
      id := id.Substring(b.Length);
  if id.startsWith('ValueSet/') then
    id := id.Substring(9);

  if (length(request.id) <= ID_LENGTH) and FindResource(frtValueSet, id, false, resourceKey, request, nil, '') then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['XmlContent'];
        parser := MakeParser(lang, ffXml, s, xppDrop);
        try
          result := parser.resource.Link as TFHIRValueSet;
        finally
          parser.free;
        end;
      end
      else
        raise Exception.Create('Unable to find value set '+id);
    finally
      FConnection.Terminate;
    end;
  end
  else
    raise Exception.create('Unknown Value Set '+id);
end;

function TFhirOperationManager.GetValueSetByIdentity(id, version: String): TFHIRValueSet;
var
  s : TBytes;
  parser : TFHIRParser;
begin
  if version <> '' then
    FConnection.SQL := 'Select * from Versions where '+
      'ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and IndexKey in (Select IndexKey from Indexes where name = ''identifier'') and Value = :id) '+
      'and ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and IndexKey in (Select IndexKey from Indexes where name = ''version'') and Value = :ver) '+
      'and ResourceVersionKey in (select MostRecent from Ids) '+
      'order by ResourceVersionKey desc'
  else
    FConnection.SQL := 'Select * from Versions where '+
      'ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and IndexKey in (Select IndexKey from Indexes where name = ''url'') and Value = :id) '+
      'and ResourceVersionKey in (select MostRecent from Ids) '+
      'order by ResourceVersionKey desc';
  FConnection.Prepare;
  try
    FConnection.BindString('id', id);
    if version <> '' then
      FConnection.BindString('ver', version);
    FConnection.Execute;
    if not FConnection.FetchNext then
      raise Exception.create('Unknown Value Set '+id);
    s := FConnection.ColBlobByName['XmlContent'];
    parser := MakeParser(lang, ffXml, s, xppDrop);
    try
      result := parser.resource.Link as TFHIRValueSet;
      try
        if FConnection.FetchNext then
          raise Exception.create('Found multiple matches for ValueSet '+id+'. Pick one by the resource id');
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


function TFhirOperationManager.GetProfileByURL(url: String; var id : String): TFHirStructureDefinition;
var
  s : TBytes;
  parser : TFHIRParser;
begin
  FConnection.SQL := 'Select Id, XmlContent from Ids, Versions where Ids.Resourcekey = Versions.ResourceKey and Versions.ResourceKey in (select ResourceKey from '+
    'IndexEntries where Flag <> 2 and IndexKey in (Select IndexKey from Indexes where name = ''url'') and Value = :url) order by ResourceVersionKey desc';
  FConnection.Prepare;
  try
    FConnection.BindString('url', url);
    FConnection.Execute;
    if not FConnection.FetchNext then
      raise Exception.create('Unknown Profile '+url);
    id := FConnection.ColStringByName['Id'];
    s := FConnection.ColBlobByName['XmlContent'];
    parser := MakeParser(lang, ffXml, s, xppDrop);
    try
      result := parser.resource.Link as TFHirStructureDefinition;
      try
        if FConnection.FetchNext then
          raise Exception.create('Found multiple matches for Profile '+url+'. Pick one by the resource id');
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

function TFhirOperationManager.GetResourceById(request: TFHIRRequest; aType : TFhirResourceType; id, base: String): TFHIRResource;
var
  resourceKey : integer;
  parser : TFHIRParser;
  b : String;
  s : TBytes;
begin
  if id.StartsWith(base) then
    id := id.Substring(base.Length)
  else for b in FRepository.Bases do
    if id.StartsWith(b) then
      id := id.Substring(b.Length);
  if id.startsWith('ValueSet/') then
    id := id.Substring(9);

  if (length(request.id) <= ID_LENGTH) and FindResource(aType, id, false, resourceKey, request, nil, '') then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['XmlContent'];
        parser := MakeParser(lang, ffXml, s, xppDrop);
        try
          result := parser.resource.Link;
        finally
          parser.free;
        end;
      end
      else
        raise Exception.Create('Unable to find '+Codes_TFHIRResourceType[aType]+' '+id);
    finally
      FConnection.Terminate;
    end;
  end
  else
    raise Exception.create('Unknown '+Codes_TFHIRResourceType[aType]+' '+id);

end;

function TFhirOperationManager.GetResourceByKey(key: integer): TFHIRResource;
var
  parser : TFHIRParser;
  s : TBytes;
begin
  FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(key)+' order by ResourceVersionKey desc';
  FConnection.Prepare;
  try
    FConnection.Execute;
    if FConnection.FetchNext then
    begin
      s := FConnection.ColBlobByName['XmlContent'];
      parser := MakeParser(lang, ffXml, s, xppDrop);
      try
        result := parser.resource.Link;
      finally
        parser.free;
      end;
    end
    else
      raise Exception.Create('Unable to find resource '+inttostr(key));
  finally
    FConnection.Terminate;
  end;
end;

function TFhirOperationManager.getResourceByReference(url, compartments: string): TFHIRResource;
var
  parser : TFHIRParser;
  s : TBytes;
  i, key : integer;
  id : String;
  rtype : TFhirResourceType;
  parts : TArray<String>;
begin
  for i := 0 to FRepository.Bases.Count - 1 do
    if url.StartsWith(FRepository.Bases[i]) then
      url := url.Substring(FRepository.Bases[i].Length);

  if (url.StartsWith('http://') or url.StartsWith('https://')) then
    raise Exception.Create('Cannot resolve external reference: '+url);

  parts := url.Split(['/']);
  if length(parts) = 2 then
  begin
    i := StringArrayIndexOfSensitive(CODES_TFhirResourceType, parts[0]);
    if (i = -1) then
      raise Exception.Create('URL not understood: '+url);
    rType := TFhirResourceType(i);
    id := parts[1];
  end
  else
    raise Exception.Create('URL not understood: '+url);

  if FindResource(rtype, id, false, key, nil, nil, compartments) then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(key)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColBlobByName['XmlContent'];
        parser := MakeParser(lang, ffXml, s, xppDrop);
        try
          result := parser.resource.Link;
        finally
          parser.free;
        end;
      end
      else
        raise Exception.Create('Unable to find resource '+inttostr(key));
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

//function TFhirOperationManager.constructValueSet(params: TParseMap; var used: String; allowNull : Boolean): TFhirValueset;
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
procedure TFhirOperationManager.AuditRest(session: TFhirSession; ip: string; resourceType: TFhirResourceType; id, ver: String; verkey : integer; op: TFHIRCommandType; provenance : TFhirProvenance; httpCode: Integer; name, message: String);
begin
  AuditRest(session, ip, resourceType, id, ver, verkey, op, provenance, '', httpCode, name, message);
end;

procedure TFhirOperationManager.AuditRest(session: TFhirSession; ip: string; resourceType: TFhirResourceType; id, ver: String; verkey : integer; op: TFHIRCommandType; provenance : TFhirProvenance; opName : String; httpCode: Integer; name, message: String);
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
  if not FRepository.DoAudit then
    exit;
  se := TFhirAuditEvent.create;
  try
    if verkey <> 0 then
      se.Tags['verkey'] := inttostr(verkey);
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
      fcmdBatch:           event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'transaction', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdHistorySystem:   event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-system', 'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdUpload:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'upload', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
      fcmdGetMeta:         event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'tags-get', 'http://hl7.org/fhir/restful-operation', AuditEventActionR);
      fcmdUpdateMeta:      event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'tags-update', 'http://hl7.org/fhir/restful-operation', AuditEventActionU);
      fcmdDeleteMeta:      event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'tags-delete', 'http://hl7.org/fhir/restful-operation', AuditEventActionD);
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
    se.source.site := FOwnerName;
    se.source.identifier := TFhirIdentifier.Create;
    se.source.identifier.value := FRepository.SystemId;
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
      p.userId.system := FRepository.SystemId;
      p.altId := session.Id;
      p.name := session.Name;
    end;
    p.requestor := true;
    p.network := TFhirAuditEventParticipantNetwork.create;
    p.network.address := ip;
    p.network.type_ := NetworkType2;

    if resourceType <> frtNull then
    begin
      o := se.object_List.Append;
      o.reference := TFhirReference.create;
      if ver <> '' then
        o.reference.reference := CODES_TFHIRResourceType[resourceType]+'/'+id+'/_history/'+ver
      else if id <> '' then
        o.reference.reference := CODES_TFHIRResourceType[resourceType]+'/'+id;
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
        fcmdGetMeta:         o.lifecycle.code := '6';
        fcmdUpdateMeta:      o.lifecycle.code := '3';
        fcmdDeleteMeta:      o.lifecycle.code := '14';
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

procedure TFhirOperationManager.storeResources(list : TFHIRResourceList; origin : TFHIRRequestOrigin; upload : boolean);
var
  i : integer;
  request: TFHIRRequest;
  response : TFHIRResponse;
begin
  Connection.StartTransact;
  try
    // cut us off from the external request
    request := TFHIRRequest.create(origin);
    response := TFHIRResponse.create;
    try
      for i := 0 to list.count - 1 do
      begin
        request.ResourceType := list[i].ResourceType;
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
        Execute(request, response, upload);
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
end;


procedure TFhirOperationManager.ReIndex;
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
  for i := 0 to FRepository.Indexes.Indexes.count - 1 do
  begin
    if Connection.CountSQL('select Count(IndexKey) from Indexes where Name = '''+ FRepository.Indexes.indexes[i].Name+'''') = 0 then
    begin
      Connection.Sql := 'insert into Indexes (IndexKey, Name) values (:k, :d)';
      Connection.prepare;
      Connection.bindInteger('k', k);
      Connection.bindString('d', FRepository.Indexes.indexes[i].Name);
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
          parser := MakeParser('en', ffXml, Connection.ColMemoryByName['XmlContent'], xppDrop);
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


procedure TFhirOperationManager.clear(a: TFhirResourceTypeSet);
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

function TFhirOperationManager.GetProfileById(request: TFHIRRequest; id, base: String): TFHirStructureDefinition;
var
  resourceKey : integer;
  parser : TFHIRParser;
  b : String;
  blob : TBytes;
begin
  result := nil;

  if id.StartsWith(base) then
    id := id.Substring(base.Length)
  else for b in FRepository.Bases do
    if id.StartsWith(b) then
      id := id.Substring(b.Length);
  if id.startsWith('StructureDefinition/') then
    id := id.Substring(20);

  if id <> '' then
  begin
    if (length(request.id) <= ID_LENGTH) and FindResource(frtStructureDefinition, id, false, resourceKey, request, nil, '') then
    begin
      FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
      FConnection.Prepare;
      try
        FConnection.Execute;
        FConnection.FetchNext;
        blob := FConnection.ColBlobByName['XmlContent'];
        parser := MakeParser(lang, ffXml, blob, xppDrop);
        try
          result := parser.resource.Link as TFHirStructureDefinition;
        finally
          parser.free;
        end;
      finally
        FConnection.Terminate;
      end;
    end
    else
      raise Exception.create('Unknown Profile '+id);
  end;
end;

procedure TFhirOperationManager.CheckCompartments(actual, allowed: String);
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

procedure TFhirOperationManager.CheckCreateNarrative(request : TFHIRRequest);
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
      profile := FRepository.ValidatorContext.Profiles.ProfileByType[r.ResourceType].Link;
      try
        if profile = nil then
          r.text := nil
        else
        begin
          gen := TNarrativeGenerator.Create('', FRepository.ValidatorContext.Profiles.Link, FRepository.LookupCode, LookupReference, request.Link);
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

procedure TFhirOperationManager.ProcessMsgQuery(request: TFHIRRequest; response: TFHIRResponse; bundle : TFHIRBundle);
begin
  raise exception.create('query-response is not yet supported');
end;

function TFhirOperationManager.BuildResponseMessage(request: TFHIRRequest; incoming: TFhirMessageHeader): TFhirMessageHeader;
var
  dst : TFhirMessageHeaderDestination;
begin
  result := TFhirMessageHeader.create;
  try
    result.id := GUIDToString(CreateGUID);
    result.timestamp := NowUTC;
    result.event := incoming.event.Clone;
    result.response := TFhirMessageHeaderResponse.create;
    result.response.identifier := NewGuidId;
    result.response.code := ResponseCodeOk;
    dst := result.destinationList.Append;
    if incoming.source <> nil then
    begin
      dst.name := incoming.source.name;
      dst.endpoint := incoming.source.endpoint;
    end
    else
    begin
      dst.name := 'No Source Provided';
      dst.endpoint := 'http://example.com/unknown';
    end;
    result.source := TFhirMessageHeaderSource.create;
    result.source.endpoint := request.baseUrl+'/mailbox';
    result.source.name := 'Health Intersections';
    result.source.software := FOwnerName;
    result.source.version := FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION;
    result.source.contact := FFactory.makeContactPoint('email', 'grahame@healthintersections.com.au', '');
    result.link;
  finally
    result.free;
  end;
end;

procedure TFhirOperationManager.ProcessMessage(request: TFHIRRequest; response : TFHIRResponse; msg, resp: TFhirMessageHeader; bundle: TFHIRBundle);
var
  s : String;
begin
  try
    s := msg.event.code;
    if s = 'MedicationAdministration-Complete' then
      raise exception.create('MedicationAdministration-Complete is not yet supported')
    else if s = 'MedicationAdministration-Nullification' then
      raise exception.create('MedicationAdministration-Nullification is not yet supported')
    else if s = 'MedicationAdministration-Recording' then
      raise exception.create('MedicationAdministration-Recording is not yet supported')
    else if s = 'MedicationAdministration-Update' then
      raise exception.create('MedicationAdministration-Update is not yet supported')
    else if s = 'admin-notify' then
      raise exception.create('admin-notify is not yet supported')
    else if s = 'diagnosticreport-provide' then
      raise exception.create('diagnosticreport-provide is not yet supported')
    else if s = 'observation-provide' then
      raise exception.create('observation-provide is not yet supported')
    else if s = 'query' then
      ProcessMsgQuery(request, response, bundle)
    else if s = 'query-response' then
      raise exception.create('query-response is not yet supported')
//    else if s = 'make-claim' then
//      ProcessMsgClaim(request, msg, resp, request.bundle, bundle)
    else
      raise exception.create('Unknown message event: "'+s+'"');

  except
    on e:exception do
    begin
      resp.response.code := ResponseCodeFatalError;
      resp.response.details := FFactory.makeReferenceText(e.message);
    end;
  end;
end;

{
procedure TFhirOperationManager.ProcessMsgClaim(request : TFHIRRequest; incoming, outgoing : TFhirMessageHeader; inbundle, outbundle: TFHIRBundle);
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
  context := FRepository.Validator.AcquireContext;
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
      id := request.BaseURL+'/remittance/'+MessageCreateResource(context, request, rem);
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
    FRepository.Validator.YieldContext(context);
  end;
end;

function TFhirOperationManager.MessageCreateResource(context : TFHIRValidatorContext; request : TFHIRRequest; res: TFHIRResource): string;
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
    req.baseUrl := request.BaseUrl;
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
function TFhirOperationManager.AddDeletedResourceTobundle(bundle: TFHIRBundle; sId, sType, base : String): TFHIRBundleEntry;
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


procedure TFhirOperationManager.checkProposedContent(request : TFHIRRequest; resource: TFhirResource; tags: TFHIRTagList);
begin

  if resource is TFhirSubscription then
  begin
    if (TFhirSubscription(resource).status <> SubscriptionStatusRequested) and (request.origin = roRest) then // nil = from the internal system, which is allowed to
      raise Exception.Create('Subscription status must be "requested", not '+TFhirSubscription(resource).statusElement.value);
    if (TFhirSubscription(resource).status = SubscriptionStatusRequested) then
      TFhirSubscription(resource).status := SubscriptionStatusActive; // well, it will be, or it will be rejected later
  end;
  if (resource is TFhirOperationDefinition) then
  begin
    if resource.id.StartsWith('fso-') and (resource.tags['internal'] <> '1') then
      raise Exception.Create('operation Definitions that start with "fso-" are managed by the system directly, and cannot be changed through the REST API');
  end;

end;

procedure TFhirOperationManager.checkProposedDeletion(request: TFHIRRequest; resource: TFhirResource; tags: TFHIRTagList);
begin

  if (resource is TFhirOperationDefinition) then
  begin
    if resource.id.StartsWith('fso-') then
      raise Exception.Create('operation Definitions that start with "fso-" are managed by the system directly, and cannot be changed through the REST API');
  end;

end;

procedure TFhirOperationManager.chooseField(aFormat : TFHIRFormat; summary : TFHIRSummaryOption; out fieldName : String; out comp : TFHIRParserClass; out needsObject : boolean);
var
  s : String;
begin
  fieldName := '';
  comp := nil;
  needsObject := false;

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
  result := CODES_TFHIRResourceType[restype]+'/'+id;

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
    result.meta.lastUpdated := TDateAndTime.CreateHL7(FHIR_GENERATED_DATE);
    result.meta.versionId := FHIR_GENERATED_REVISION;
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
    result.status := ConformanceResourceStatusDraft;
    result.experimental := false;
    result.date := TDateAndTime.CreateHL7(FHIR_GENERATED_DATE+'Z');
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

procedure TFhirOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
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
  result := (request.OperationName = Name) and (request.ResourceType in Types);
  if result then
  begin
    t := owningResource;
    if t = frtNull then
      t := request.ResourceType;
    if t = frtNull then
      result := ((isWrite and request.canWrite(frtNull) or (not isWrite and request.canRead(frtNull)))) // todo: what should it be?
    else
      result := ((isWrite and request.canWrite(t)) or (not isWrite and request.canRead(t)));
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

procedure TFhirGenerateQAOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
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

procedure TFhirHandleQAPostOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
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

procedure TFhirQuestionnaireGenerationOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  profile : TFHirStructureDefinition;
  op : TFhirOperationOutcome;
  resourceKey : integer;
  id, fid : String;
  builder : TQuestionnaireBuilder;
  questionnaire : TFHIRQuestionnaire;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, request.Session.canRead(request.ResourceType) and manager.opAllowed(request.ResourceType, request.CommandType), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(frtStructureDefinition, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        profile := nil;
        try
          // first, we have to identify the structure definition
          id := request.Id;
          if request.Id <> '' then // and it must exist, because of the check above
            profile := manager.GetProfileById(request, request.Id, request.baseUrl)
          else if request.Parameters.VarExists('identifier') then
            profile := manager.GetProfileByURL(request.Parameters.getvar('identifier'), id)
          else if (request.form <> nil) and request.form.hasParam('profile') then
            profile := LoadFromFormParam(request.form.getparam('profile'), request.Lang) as TFHirStructureDefinition
          else if (request.Resource <> nil) and (request.Resource is TFHirStructureDefinition) then
            profile := request.Resource.Link as TFHirStructureDefinition
          else
            raise Exception.Create('Unable to find profile to convert (not provided by id, identifier, or directly)');

          profile.checkNoImplicitRules('QuestionnaireGeneration', 'profile');
          profile.checkNoModifiers('QuestionnaireGeneration', 'profile');

          if id <> '' then
          begin
            fid := request.baseUrl+'Profile/'+id+'/$questionnaire';
            questionnaire := manager.FRepository.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
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
                builder.Profiles := manager.FRepository.ValidatorContext.Profiles.Link;
                builder.build;
                questionnaire := builder.questionnaire.Link;
                if id <> '' then
                  manager.FRepository.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.dependencies);
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
        op := manager.FRepository.validator.validateInstance(response.Resource, risOptional, 'Produce Questionnaire', nil);
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
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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

procedure TFhirExpandValueSetOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  vs, dst : TFHIRValueSet;
  resourceKey : integer;
  url, cacheId, profile, filter : String;
  limit, count, offset : integer;
  incomplete : boolean;
  params : TFhirParameters;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceType, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        cacheId := '';
        params := makeParams(request);
        vs := nil;
        try
          // first, we have to identify the value set.
          if request.Id <> '' then // and it must exist, because of the check above
          begin
            vs := manager.GetValueSetById(request, request.Id, request.baseUrl);
            cacheId := vs.url;
          end
          else if params.hasParameter('identifier') then
          begin
            url := params.str['identifier'];
            if (url.startsWith('ValueSet/')) then
              vs := manager.GetValueSetById(request, url.substring(9), request.baseUrl)
            else if (url.startsWith(request.baseURL+'ValueSet/')) then
              vs := manager.GetValueSetById(request, url.substring(9), request.baseUrl)
            else if not manager.FRepository.TerminologyServer.isKnownValueSet(url, vs) then
              vs := manager.GetValueSetByIdentity(request.Parameters.getvar('identifier'), request.Parameters.getvar('version'));
            cacheId := vs.url;
          end
          else if params.hasParameter('valueSet') then
            vs := params['valueSet'] as TFhirValueSet
          else if (request.Resource <> nil) and (request.Resource is TFHIRValueSet) then
            vs := request.Resource.Link as TFhirValueSet
          else if params.hasParameter('context') then
            raise Exception.Create('the "context" parameter is not yet supported')
          else
            raise Exception.Create('Unable to find value set to expand (not provided by id, identifier, or directly)');

          vs.checkNoImplicitRules('ExpandValueSet', 'ValueSet');
          vs.checkNoModifiers('ExpandValueSet', 'ValueSet');

          profile := params.str['profile'];
          filter := params.str['filter'];
          count := StrToIntDef(params.str['count'], 0);
          offset := StrToIntDef(params.str['offset'], 0);
          limit := StrToIntDef(params.str['_limit'], 0);
          incomplete := StrToBoolDef(params.str['_incomplete'], false);

          dst := manager.FRepository.TerminologyServer.expandVS(vs, cacheId, profile, filter, limit, count, offset, incomplete);
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
          vs.free;
          params.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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

{ TFhirLookupValueSetOperation }

function TFhirLookupValueSetOperation.Name: String;
begin
  result := 'lookup';
end;

function TFhirLookupValueSetOperation.owningResource: TFhirResourceType;
begin
  result := frtValueSet;
end;

function TFhirLookupValueSetOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtValueSet];
end;

function TFhirLookupValueSetOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirLookupValueSetOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  coding : TFhirCoding;
  params : TFhirParameters;
//  vs, dst : TFHIRValueSet;
//  resourceKey : integer;
//  url, cacheId : String;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id <> '') then
        raise Exception.Create('Lookup does not take an identified value set');
      params := makeParams(request);
      try
        coding := nil;
        try
          if params.hasParameter('system') then
          begin
            coding := TFhirCoding.Create;
            coding.system := params.str['system'];
            coding.code := params.str['code'];
            coding.version := params.str['version'];
          end
          else if params.hasParameter('coding') then
            coding := params['coding'].Link as TFhirCoding
          else
            raise Exception.Create('Unable to find a code to lookup (need coding or system/code)');
          response.Resource := manager.FRepository.TerminologyServer.lookupCode(coding);  // currently, we ignore the date
          if response.Resource is TFhirOperationOutcome then
          begin
            response.HTTPCode := 500;
            response.Message := 'Error';
          end
          else
          begin
            response.HTTPCode := 200;
            response.Message := 'OK';
          end;
          response.Body := '';
          response.LastModifiedDate := now;
        finally
          coding.Free;
        end;
      finally
        params.Free;
      end;
    end;
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirLookupValueSetOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/ValueSet-lookup';
end;

function TFhirLookupValueSetOperation.isWrite: boolean;
begin
  result := false;
end;

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

procedure TFhirValidationOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);

type TValidationOperationMode = (vomGeneral, vomCreate, vomUpdate, vomDelete);
var
  outcome : TFhirOperationOutcome;
  i : integer;
  buffer : TAdvBuffer;
  mem : TAdvMemoryStream;
  xml : TFHIRComposer;
  vcl : TVclStream;
  profileId, id : String;
  profile : TFHirStructureDefinition;
  opDesc : string;
  result : boolean;
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

    if StringStartsWith(ProfileId, 'http://localhost/Profile/') then
      profile := manager.GetProfileById(request, copy(ProfileId, 27, $FF), request.baseUrl)
    else if StringStartsWith(ProfileId, 'Profile/') then
      profile := manager.GetProfileById(request, copy(ProfileId, 9, $FF), request.baseUrl)
    else if StringStartsWith(ProfileId, request.baseUrl+'Profile/') then
      profile := manager.GetProfileById(request, copy(ProfileId, length(request.baseUrl)+9, $FF), request.baseUrl)
    else if (profileId <> '') then
      profile := manager.GetProfileByURL(profileId, id);

    if Profile <> nil then
      opDesc := 'Validate resource '+request.id+' against profile '+profileId
    else if (profileId <> '') then
      raise Exception.Create('The profile "'+profileId+'" could not be resolved')
    else
      opDesc := 'Validate resource '+request.id;

    if (request.Source <> nil) and not (request.Resource is TFhirParameters) then
      outcome := manager.FRepository.validator.validateInstance(request.Source, request.PostFormat, risOptional, opDesc, profile)
    else
    begin
      buffer := TAdvBuffer.create;
      try
        mem := TAdvMemoryStream.create;
        vcl := TVclStream.create;
        try
          mem.Buffer := buffer.Link;
          vcl.stream := mem.Link;
          xml := TFHIRXmlComposer.create(request.Lang);
          try
            if request.Resource = nil then
              raise Exception.Create('No resource found to validate');
            if request.Resource is TFhirParameters then
              xml.Compose(vcl, TFhirParameters(request.resource).NamedParameter['resource'] as TFhirResource, true, nil)
            else // request.Resource <> nil
              xml.Compose(vcl, request.resource, true, nil);
          finally
            xml.free;
          end;
        finally
          vcl.free;
          mem.free;
        end;
        outcome := manager.FRepository.validator.validateInstance(buffer, ffXml, risOptional, opDesc, profile);
      finally
        buffer.free;
      end;
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
    if request.ResourceType <> frtAuditEvent then
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, '', 0, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceType <> frtAuditEvent then
        manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, '', 0, request.CommandType, request.Provenance, 500, '', e.message);
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

procedure TFhirValueSetValidationOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  vs : TFHIRValueSet;
  resourceKey : integer;
  cacheId  : String;
  coded : TFhirCodeableConcept;
  coding : TFhirCoding;
  abstractOk : boolean;
  params : TFhirParameters;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceType, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        cacheId := '';
        params := makeParams(request);
        try
          vs := nil;
          try
            // first, we have to identify the value set.
            if request.Id <> '' then // and it must exist, because of the check above
            begin
              vs := manager.GetValueSetById(request, request.Id, request.baseUrl);
              cacheId := vs.url;
            end
            else if params.hasParameter('identifier') then
            begin
              if not manager.FRepository.TerminologyServer.isKnownValueSet(params.str['identifier'], vs) then
                vs := manager.GetValueSetByIdentity(params.str['identifier'], params.str['version']);
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
                coded.codingList.add(LoadDTFromFormParam(request.form.getParam('coding'), request.lang, 'coding', TFhirCoding) as TFhirCoding)
              end
              else if (request.form <> nil) and request.form.hasParam('codeableConcept') then
                coded := LoadDTFromFormParam(request.form.getParam('codeableConcept'), request.lang, 'codeableConcept', TFhirCodeableConcept) as TFhirCodeableConcept
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

              response.resource := manager.FRepository.TerminologyServer.validate(vs, coded, abstractOk);
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
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
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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
    result.notes := 'This server has little idea what a valid patient record is; it returns everything in the patient compartment, and any resource directly referred to from one of these';
    result.system := False;
    result.type_List.Append.value := 'Patient';
    result.instance := true;
    with result.parameterList.Append do
    begin
      name := 'return';
      use := OperationParameterUseOut;
      min := '1';
      max := '1';
      documentation := 'Patient record as a bundle';
      type_ := ValuesetOperationParameterTypeBundle;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFhirPatientEverythingOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  bundle : TFHIRBundle;
  entry : TFHIRBundleEntry;
  includes : TReferenceList;
  id, link, base, sql, field : String;
  total : Integer;
  rkey : integer;
//  offset, count : integer;
//  ok : boolean;
  wantsummary : TFHIRSummaryOption;
  title: string;
  keys : TKeyList;
  params : TParseMap;
  patient : TFHIRResource;
  list : TFhirList;
  prsr : TFHIRParserClass;
  needsObject : boolean;
  type_ : TFhirResourceType;
begin
  try
    patient := nil;
    try

      // first, we have toi convert from the patient id to a compartment id
      if manager.FindResource(frtPatient, request.Id, false, rkey, request, response, '') then
      begin
        request.compartmentId := request.Id;
        bundle := TFHIRBundle.Create(BundleTypeSearchset);
        includes := TReferenceList.create;
        keys := TKeyList.Create;
        params := TParseMap.Create('');
        try
//          bundle.base := request.baseUrl;
          if manager.FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], request.Session, 1, id, link, sql, title, base, total, wantSummary) then
            link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
          else
            id := manager.BuildSearchResultSet(0, request.Session, request.resourceType, params, request.baseUrl, request.compartments, request.compartmentId, link, sql, total, wantSummary);
          bundle.total := inttostr(total);
          bundle.Tags['sql'] := sql;
          manager.chooseField(response.Format, wantsummary, field, prsr, needsObject);

          manager.FConnection.SQL := 'Select Ids.ResourceKey, Types.ResourceName, Ids.Id, VersionId, StatedDate, Name, Versions.Status, Tags, '+field+' from Versions, Ids, Sessions, SearchEntries, Types '+
              'where Ids.Deleted = 0 and SearchEntries.ResourceVersionKey = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey '+'and SearchEntries.ResourceKey = Ids.ResourceKey and Types.ResourceTypeKey = Ids.ResourceTypeKey and SearchEntries.SearchKey = '+id+' '+
              'order by SortValue ASC';
          manager.FConnection.Prepare;
          try
            manager.FConnection.Execute;
            while manager.FConnection.FetchNext do
            Begin
              entry := manager.AddResourceTobundle(bundle, request.baseUrl, field, prsr, SearchEntryModeNull, false, type_);
              keys.Add(TKeyPair.create(type_, manager.FConnection.ColStringByName['ResourceKey']));

              if request.Parameters.VarExists('_include') then
                manager.CollectIncludes(request.session, includes, entry.resource, request.Parameters.GetVar('_include'));
              if entry.resource.ResourceType = frtPatient then
              begin
                if patient = nil then
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
//            manager.FConnection.SQL := 'Select ResourceTypeKey, Ids.Id, VersionId, StatedDate, Name, Versions.Status, Tags, '+field+' from Versions, Sessions, Ids '+
//                'where Ids.Deleted = 0 and Ids.MostRecent = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey '+
//                'and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and '+includes.asSql+') order by ResourceVersionKey DESC';
//            manager.FConnection.Prepare;
//            try
//              manager.FConnection.Execute;
//              while manager.FConnection.FetchNext do
//                manager.AddResourceTobundle(bundle, request.baseUrl, field, prsr);
//            finally
//              manager.FConnection.Terminate;
//            end;
//          end;

          if patient = nil then
            raise Exception.Create('No Patient resource found in patient compartment');
          bundle.deleteEntry(patient);
          bundle.entryList.Insert(0).resource := patient.Link;
          bundle.entryList[0].fullurl := AppendForwardSlash(base)+'Patient/'+id;

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
    manager.AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, '', '', 0, request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message);
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

procedure TFhirGenerateDocumentOperation.addResource(manager: TFhirOperationManager; bundle: TFHIRBundle; reference: TFhirReference; required: boolean; compartments : String);
var
  res : TFHIRResource;
begin
  if reference = nil then
    exit;
  res := manager.getResourceByReference(reference.reference, compartments);
  try
    if res <> nil then
      bundle.entryList.Append.resource := res.Link
    else if required then
      raise Exception.Create('Unable to resolve reference '''+reference.reference+'''');
  finally
    res.Free;
  end;
end;

procedure TFhirGenerateDocumentOperation.addSections(manager: TFhirOperationManager; bundle: TFHIRBundle; sections: TFhirCompositionSectionList; compartments : String);
var
  i, j : integer;
begin
  for i := 0 to sections.Count - 1 do
  begin
    for j := 0 to sections[i].entryList.Count - 1 do
      addResource(manager, bundle, sections[i].entryList[j], true, compartments);
    if (sections[i].hasSectionList) then
      addSections(manager, bundle, sections[i].sectionList, compartments);
  end;
end;

function TFhirGenerateDocumentOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := CreateBaseDefinition(base);
  try
    result.notes := '';
    result.system := False;
    result.type_List.Append.value := 'Composition';
    result.instance := true;
    with result.parameterList.Append do
    begin
      name := 'return';
      use := OperationParameterUseOut;
      min := '1';
      max := '1';
      documentation := 'Composition as a bundle (document)';
      type_ := ValuesetOperationParameterTypeBundle;;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFhirGenerateDocumentOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  composition : TFhirComposition;
  bundle : TFhirBundle;
  resourceKey : integer;
  i, j : integer;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if manager.FindResource(request.ResourceType, request.Id, false, resourceKey, request, response, request.compartments) then
      begin
        composition := manager.GetResourceByKey(resourceKey) as TFhirComposition;
        try
          composition.checkNoImplicitRules('GenerateDocument', 'composition');
          composition.checkNoModifiers('GenerateDocument', 'composition');

          bundle := TFhirBundle.Create(BundleTypeDocument);
          try
            bundle.id := copy(GUIDToString(CreateGUID), 2, 36).ToLower;
            bundle.meta := TFhirMeta.Create;
            bundle.meta.lastUpdated := NowUTC;
//            bundle.base := manager.FRepository.FormalURLPlain;
            bundle.entryList.Append.resource := composition.Link;
            addResource(manager, bundle, composition.subject, true, request.compartments);
            addSections(manager, bundle, composition.sectionList, request.compartments);

            for i := 0 to composition.authorList.Count - 1 do
              addResource(manager, bundle, composition.authorList[i], false, request.compartments);
            for i := 0 to composition.attesterList.Count - 1 do
              addResource(manager, bundle, composition.attesterList[i].party, false, request.compartments);
            addResource(manager, bundle, composition.custodian, false, request.compartments);
            for i := 0 to composition.eventList.Count - 1 do
              for j := 0 to composition.eventList[i].detailList.Count - 1 do
                addResource(manager, bundle, composition.eventList[i].detailList[j], false, request.compartments);
            addResource(manager, bundle, composition.encounter, false, request.compartments);


            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.Resource := bundle.Link;
          finally
            bundle.Free;
          end;
        finally
          composition.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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

procedure TFhirProcessClaimOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : integer;
  params : TFhirParameters;
  claim : TFhirClaim;
  resp : TFhirClaimResponse;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceType, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        params := makeParams(request);
        claim := nil;
        try
          // first, we have to identify the value set.
          if request.Id <> '' then // and it must exist, because of the check above
            claim := manager.GetResourceById(request, frtClaim, request.Id, request.baseUrl) as TFhirClaim
//          else if request.Parameters.VarExists('identifier') then
//          begin
//            url := request.Parameters.getvar('identifier');
//            if (url.startsWith('ValueSet/')) then
//              vs := manager.GetValueSetById(request, url.substring(9), request.baseUrl)
//            else if (url.startsWith(request.baseURL+'ValueSet/')) then
//              vs := manager.GetValueSetById(request, url.substring(9), request.baseUrl)
//            else if not manager.FRepository.TerminologyServer.isKnownValueSet(url, vs) then
//              vs := manager.GetValueSetByIdentity(request.Parameters.getvar('identifier'), request.Parameters.getvar('version'));
//            cacheId := vs.url;
//          end
          else if (request.form <> nil) and request.form.hasParam('claim') then
            claim := LoadFromFormParam(request.form.getparam('valueSet'), request.Lang) as TFhirClaim
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
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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

procedure TFhirConceptMapTranslationOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  vsS, vsT : TFHIRValueSet;
//  op : TFhirOperationOutcome;
//  resourceKey : integer;
  coded : TFhirCodeableConcept;
  coding : TFhirCoding;
//  abstractOk : boolean;
  params : TFhirParameters;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') then // or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceType, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        params := makeParams(request);
        try
          vsS := nil;
          vsT := nil;
          try
            // first, we have to identify the value sets
            if params.hasParameter('valueset') then
              if not manager.FRepository.TerminologyServer.isKnownValueSet(params.str['valueset'], vsS) then
                vsS := manager.GetValueSetByIdentity(params.str['valueset'], params.str['version']);
            if params.hasParameter('target') then
              if not manager.FRepository.TerminologyServer.isKnownValueSet(params.str['target'], vsS) then
                vsS := manager.GetValueSetByIdentity(params.str['target'], params.str['targetversion']);
            if vst = nil then
              raise Exception.Create('Unable to find target value set (not provided by id, identifier, or directly)');

            coded := nil;
            try
              // ok, now we need to find the source code to validate
              if params.hasParameter('coding') then
              begin
                coded := TFhirCodeableConcept.Create;
                coded.codingList.add(LoadDTFromParam(params.str['coding'], request.lang, 'coding', TFhirCoding) as TFhirCoding)
              end
              else if params.hasParameter('codeableConcept') then
                coded := LoadDTFromParam(params.str['codeableConcept'], request.lang, 'codeableConcept', TFhirCodeableConcept) as TFhirCodeableConcept
              else if request.Parameters.VarExists('code') and request.Parameters.VarExists('system') then
              begin
                coded := TFhirCodeableConcept.Create;
                coding := coded.codingList.Append;
                coding.system := params.str['system'];
                coding.version := params.str['version'];
                coding.code := params.str['code'];
                coding.display := params.str['display'];
              end
              else
                raise Exception.Create('Unable to find code to validate (coding | codeableConcept | code');
              vsS.checkNoImplicitRules('ConceptMapTranslation', 'Source ValueSet');
              vsS.checkNoModifiers('ConceptMapTranslation', 'Source ValueSet');
              vsT.checkNoImplicitRules('ConceptMapTranslation', 'Target ValueSet');
              vsT.checkNoModifiers('ConceptMapTranslation', 'Target ValueSet');
              response.resource := manager.FRepository.TerminologyServer.translate(vsS, coded, vsT);
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
            finally
              coded.Free;
            end;
          finally
            vsS.free;
            vsT.free;
          end;
        finally
          params.free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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

constructor TKeyPair.create(t_: TFhirResourceType; key: string);
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

function TKeyList.forType(t_: TFhirResourceType): String;
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

procedure TFhirGenerateSnapshotOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  params : TFhirParameters;
  sdParam, sdBase : TFhirStructureDefinition;
  utils : TProfileUtilities;
  op : TFhirOperationOutcome;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
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
        if sdParam.base <> '' then
        begin
          if not manager.Repository.ValidatorContext.Profiles.getProfileStructure(nil, sdParam.base, sdBase) then
            raise Exception.Create('StructureDefinition base profile "'+sdParam.base+'" not found');
        end
        else if params.hasParameter('base') then
        begin
          if not manager.Repository.ValidatorContext.Profiles.getProfileStructure(nil, params.str['base'], sdBase) then
            raise Exception.Create('Nominated base profile "'+params.str['base']+'" not found');
        end
        else
        begin
          if not manager.Repository.ValidatorContext.Profiles.getProfileStructure(nil, 'http://hl7.org/fhir/StructureDefinition/'+sdBase.constrainedType, sdBase) then
            raise Exception.Create('Implicit base type "'+sdBase.constrainedType+'" not found');
        end;

        op := TFhirOperationOutcome.Create;
        utils := TProfileUtilities.create(manager.Repository.ValidatorContext.link, op.issueList.Link);
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
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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

procedure TFhirGenerateCDSHookOperation.addCard(params: TFhirParameters; base, message, source, sourceurl, indicator: String);
var
  p, p2 : TFhirParametersParameter;
begin
  p := TFhirParametersParameter.create;
  try
    p.Name := 'card';
    params.parameterList.Add(p.Link);
    p.addParameter('summary', message);
    if (source <> '') or (sourceurl <> '') then
    begin
      p2 := p.addParameter('source');
      if (source <> '') then
        p2.addParameter('label', source);
      if (sourceurl <> '') then
        p2.addParameter('url', sourceurl);
    end;
    if (indicator <> '') then
      p.addParameter('indicater', TFHIRCode.create(indicator));
  finally
    p.Free;
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

procedure TFhirGenerateCDSHookOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  pIn, pOut : TFhirParameters;
  activityInstance : String;
  activity : TFHIRCoding;
  patient : String;
  preFetch : TFHIRBundle;
  entry : TFhirBundleEntry;
  pat : TFhirPatient;
begin
  if not(request.Resource is TFhirParameters) then
    raise Exception.Create('Expected Parameters Resource for a cds-hook operation ');

  pIn := request.Resource as TFhirParameters;
  activity := pIn['activity'] as TFHIRCoding;
  if activity = nil then
    raise Exception.Create('No activity found');
  activityInstance := pIn.str['activityInstance'];
  patient := pIn.str['patient'];
  preFetch := pIn['preFetchData'] as TFHIRBundle;

  pOut := TFhirParameters.Create;
  response.Resource := pOut;
  if (activity.system = 'http://cds-hooks.smarthealthit.org/activity') and (activity.code = 'patient-view') then
  begin
    pat := nil;
    for entry in preFetch.entryList do
      if (entry.resource <> nil) and (entry.resource is TFhirPatient) and (entry.resource.id = patient) then
        pat := entry.resource as TFhirPatient;
    if pat = nil then
      raise Exception.Create('Expected a patient in the pre-fetch for a cds-hook operation with activity = patient-view');
    processPatientView(manager, request, pat, pOut);
  end;
  response.HTTPCode := 200;
  response.Message := 'OK';
  response.Body := '';
  response.LastModifiedDate := now;
end;

procedure TFhirGenerateCDSHookOperation.processPatientView(manager: TFhirOperationManager; request : TFHIRRequest; context: TFHIRPatient; params: TFhirParameters);
var
  patient : TFhirPatient;
  resourceKey : integer;
  matches, m : TMatchingResourceList;
  id : TFhirIdentifier;
  flag : TFhirFlag;
  i : integer;
  su, sl : String;
begin
  patient := nil;
  try
    // first, do we know the patient?
    if manager.FindResource(frtPatient, context.id, false, resourceKey, request, nil, request.compartments) then
      patient := manager.GetResourceById(request, frtPatient, context.Id, request.baseUrl) as TFHIRPatient
    else
    begin
      matches := TMatchingResourceList.create;
      try
        for id in context.identifierList do
        begin
          m := manager.ResolveSearchId(frtPatient, request.compartmentId, request.compartments, request.baseURL, 'identifier='+id.system+'|'+id.value);
          try
            matches.AddAll(m);
          finally
            m.Free;
          end;
        end;
        if matches.Count = 1 then
          patient := manager.GetResourceByKey(matches[0].key) as TFhirPatient;
      finally
        matches.Free;
      end;
    end;
    if (patient <> nil) then
    begin
      m := manager.ResolveSearchId(frtFlag, request.compartmentId, request.compartments, request.baseURL, 'patient='+patient.id);
      try
        for i := 0 to m.Count - 1 do
        begin
          flag := manager.GetResourceByKey(m[i].key) as TFhirFlag;
          if flag.status = FlagStatusActive then
          begin
            if flag.author <> nil then
            begin
              sl := flag.author.display;
              su := flag.author.reference;
            end
            else
            begin
              sl := '';
              su := '';
            end;
            if flag.code.text <> '' then
              addCard(params, request.baseUrl, flag.code.text, sl, su, 'info')
            else if flag.code.codingList.Count > 0 then
              addCard(params, request.baseUrl, flag.code.codingList[0].display, sl, su, 'info');
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

procedure TFhirGenerateTemplateOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  profile : TFHirStructureDefinition;
  op : TFhirOperationOutcome;
  resourceKey : integer;
  id, fid : String;
  builder : TProfileUtilities;
  template : TFHIRResource;
  narr : TFHIRNarrativeGenerator;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, request.Session.canRead(request.ResourceType) and manager.opAllowed(request.ResourceType, request.CommandType), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(frtStructureDefinition, request.Id, false, resourceKey, request, response, request.compartments)) then
      begin
        profile := nil;
        try
          // first, we have to identify the structure definition
          id := request.Id;
          if request.Id <> '' then // and it must exist, because of the check above
            profile := manager.GetProfileById(request, request.Id, request.baseUrl)
          else if request.Parameters.VarExists('identifier') then
            profile := manager.GetProfileByURL(request.Parameters.getvar('identifier'), id)
          else if (request.form <> nil) and request.form.hasParam('profile') then
            profile := LoadFromFormParam(request.form.getparam('profile'), request.Lang) as TFHirStructureDefinition
          else if (request.Resource <> nil) and (request.Resource is TFHirStructureDefinition) then
            profile := request.Resource.Link as TFHirStructureDefinition
          else
            raise Exception.Create('Unable to find profile to convert (not provided by id, identifier, or directly)');

          profile.checkNoImplicitRules('GenerateTemplate', 'profile');
          profile.checkNoModifiers('GenerateTemplate', 'profile');

          template := nil;
          try
            builder := TProfileUtilities.create(manager.FRepository.ValidatorContext.Link, nil);
            try
              template := builder.populateByProfile(profile);
              if template is TFhirDomainResource then
              begin
                narr := TFHIRNarrativeGenerator.create(manager.FRepository.ValidatorContext.Link);
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
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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

procedure TFhirGenerateNarrativeOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
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
      narr := TFHIRNarrativeGenerator.create(manager.FRepository.ValidatorContext.Link);
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
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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
    response.Message := StringFormat('invalid closure name %s', [CODES_TFHIRResourceType[request.ResourceType]+':'+request.Id]);
    response.Body := response.Message;
    response.Resource := BuildOperationOutcome(request.lang, response.Message);
  end;
end;

function TFhirConceptMapClosureOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirConceptMapClosureOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  params, res : TFhirParameters;
  p : TFhirParametersParameter;
  n, v : String;
  init : boolean;
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
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      res := TFHIRParameters.Create;
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
            manager.FRepository.TerminologyServer.InitClosure(n);
            res.AddParameter('outcome', true);
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.resource := res.Link;
          end
          else
          begin
            if not manager.FRepository.TerminologyServer.UseClosure(n, cm) then
              errorResp(404, StringFormat('closure name ''%s'' not known', [CODES_TFHIRResourceType[request.ResourceType]+':'+request.Id]))
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
              map.status := ConformanceResourceStatusActive;
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
        res.free;
        cm.Free;
        map.Free;
      end;
    end;
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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
  result := frtConceptMap;
end;

function TFhirConceptMapClosureOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtConceptMap];
end;

end.

