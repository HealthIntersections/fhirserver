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
    procedure ExecuteMailBox(request: TFHIRRequest; response : TFHIRResponse);

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
  AdvObjects, AdvIntegerObjectMatches, AdvMemories, AdvBuffers, AdvVclStreams, AdvStringObjectMatches, AdvStringMatches,
  AdvStringBuilders, AdvObjectLists, AdvNames, AdvXmlBuilders,

  FHIRBase, FHIRSupport, FHIRResources, FHIRConstants, FHIRComponents, FHIRTypes, FHIRAtomFeed, FHIRParserBase,
  FHIRParser, FHIRUtilities, FHIRLang, FHIRIndexManagers, FHIRValidator, FHIRValueSetExpander, FHIRTags, FHIRDataStore,
  FHIRServerConstants, FHIRServerUtilities, NarrativeGenerator,
  QuestionnaireBuilder, SearchProcessor;

const
  MAGIC_NUMBER = 941364592;

  CURRENT_FHIR_STORAGE_VERSION = 2;

  RELEASE_DATE = '20131103';

type
  TCreateIdState = (idNoNew, idMaybeNew, idIsNew);

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


    function opAllowed(resource : TFHIRResourceType; command : TFHIRCommandType) : Boolean;

    function FindSavedSearch(const sId : String; Session : TFHIRSession; typeKey : integer; var id, link, sql, title, base : String; var total : Integer; var wantSummary : TFHIRSearchSummary): boolean;
    function BuildSearchResultSet(typekey : integer; session : TFHIRSession; aType : TFHIRResourceType; params : TParseMap; baseURL, compartments, compartmentId : String; var link, sql : String; var total : Integer; var wantSummary : TFHIRSearchSummary):String;
    function BuildHistoryResultSet(request: TFHIRRequest; response: TFHIRResponse; var searchKey, link, sql, title, base : String; var total : Integer) : boolean;
    procedure ProcessDefaultSearch(typekey : integer; session : TFHIRSession; aType : TFHIRResourceType; params : TParseMap; baseURL, compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; var wantSummary : TFHIRSearchSummary);
    procedure BuildSearchForm(request: TFHIRRequest; response : TFHIRResponse);
    function GetResourceByKey(key : integer): TFHIRResource;
    function getResourceByReference(url, compartments : string): TFHIRResource;
    function GetProfileByURL(url: String; var id : String) : TFHirStructureDefinition;
    function GetValueSetById(request: TFHIRRequest; id, base : String) : TFHIRValueSet;
    function GetResourceById(request: TFHIRRequest; aType : TFhirResourceType; id, base : String) : TFHIRResource;
    function GetProfileById(request: TFHIRRequest; id, base : String) : TFHirStructureDefinition;
    function GetValueSetByIdentity(id, version : String) : TFHIRValueSet;
    function constructValueSet(params : TParseMap; var used : String; allowNull : Boolean) : TFhirValueset;

    procedure updateProvenance(prv : TFHIRProvenance; rtype, id, vid : String);

    function TextSummaryForResource(resource : TFhirResource) : String;
    function FindResource(aType : TFHIRResourceType; sId : String; bAllowDeleted : boolean; var resourceKey : integer; var originalId : String; request: TFHIRRequest; response: TFHIRResponse; compartments : String): boolean;
    function FindResourceVersion(aType : TFHIRResourceType; sId, sVersionId : String; bAllowDeleted : boolean; var resourceVersionKey : integer; request: TFHIRRequest; response: TFHIRResponse): boolean;
    procedure NoMatch(request: TFHIRRequest; response: TFHIRResponse);
    procedure NotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure VersionNotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure TypeNotFound(request: TFHIRRequest; response : TFHIRResponse);
    function GetNewResourceId(aType : TFHIRResourceType; var id : string; var key : integer):Boolean;
    function AddNewResourceId(aType : TFHIRResourceType; id : string; var resourceKey : integer) : Boolean;
    Procedure CollectIncludes(session : TFhirSession; includes : TReferenceList; resource : TFHIRResource; path : String);
    Procedure CollectReverseIncludes(session : TFHIRSession; includes : TReferenceList; keys : TStringList; types : String; feed : TFHIRAtomFeed; request : TFHIRRequest; wantsummary : TFHIRSearchSummary);
    Function TypeForKey(key : integer):TFHIRResourceType;
    Procedure LoadTags(tags : TFHIRAtomCategoryList; ResourceKey : integer);
    procedure CommitTags(tags : TFHIRAtomCategoryList; key : integer);
    Procedure ProcessBlob(request: TFHIRRequest; response : TFHIRResponse; wantSummary : boolean);
    function ResolveSearchId(atype : TFHIRResourceType; compartmentId, compartments : String; baseURL, params : String) : TMatchingResourceList;
    function ResolveSearchIdCount(atype : TFHIRResourceType; compartmentId, compartments : String; baseURL, params : String) : integer;
    function ScanId(base : String; request : TFHIRRequest; entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList; index : integer) : TFHIRTransactionEntry;
    procedure FixXhtmlUrls(lang, base: String; ids: TFHIRTransactionEntryList; node: TFhirXHtmlNode);
    procedure adjustReferences(te : TFHIRTransactionEntry; base : String; entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList);
    function commitResource(context: TFHIRValidatorContext; request: TFHIRRequest; response : TFHIRResponse; upload : boolean; entry : TFHIRAtomEntry; i : integer; id : TFHIRTransactionEntry; session : TFhirSession; resp : TFHIRAtomFeed) : boolean;

    procedure checkProposedContent(request : TFHIRRequest; resource : TFhirResource; tags : TFHIRAtomCategoryList);
    procedure checkProposedDeletion(request : TFHIRRequest; resource : TFhirResource; tags : TFHIRAtomCategoryList);

    function BuildResponseMessage(request : TFHIRRequest; incoming : TFhirMessageHeader) : TFhirMessageHeader;
    procedure ProcessMessage(request: TFHIRRequest; response : TFHIRResponse; msg, resp : TFhirMessageHeader; feed : TFHIRAtomFeed);
    procedure ProcessMsgQuery(request: TFHIRRequest; response : TFHIRResponse; feed : TFHIRAtomFeed);
//    procedure ProcessMsgClaim(request : TFHIRRequest; incoming, outgoing: TFhirMessageHeader; infeed, outfeed: TFHIRAtomFeed);
//    function MessageCreateResource(context : TFHIRValidatorContext; request : TFHIRRequest; res : TFHIRResource) : string;
    function EncodeResource(r : TFhirResource) : TBytes;
    function EncodeResourceSummary(r : TFhirResource) : TBytes;
    function EncodeFeed(r : TFHIRAtomFeed) : TBytes;

    // operations
    procedure ExecuteQuestionnaireGeneration(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteGenerateQA(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteHandleQAPost(request: TFHIRRequest; response : TFHIRResponse);

    function CreateDocumentAsBinary(mainRequest : TFhirRequest) : String;
    procedure CreateDocumentReference(mainRequest : TFhirRequest; binaryId : String);

    procedure AuditRest(session : TFhirSession; ip : string; resourceType : TFhirResourceType; id, ver : String; op : TFHIRCommandType; provenance : TFhirProvenance; httpCode : Integer; name, message : String); overload;
    procedure AuditRest(session : TFhirSession; ip : string; resourceType : TFhirResourceType; id, ver : String; op : TFHIRCommandType; provenance : TFhirProvenance; opName : String; httpCode : Integer; name, message : String); overload;
    procedure SaveProvenance(session : TFhirSession; prv : TFHIRProvenance);

    procedure CheckCompartments(actual, allowed : String);
    procedure ExecuteRead(request: TFHIRRequest; response : TFHIRResponse);
    procedure executeReadInTransaction(entry : TFhirBundleEntryTransaction; request: TFHIRRequest; response : TFHIRResponse);

    function ExecuteUpdate(context: TFHIRValidatorContext; upload : boolean; request: TFHIRRequest; response : TFHIRResponse) : Boolean;
    procedure ExecuteVersionRead(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteDelete(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteHistory(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse);
    Function ExecuteCreate(context: TFHIRValidatorContext; upload : boolean; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
    procedure ExecuteConformanceStmt(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteUpload(request: TFHIRRequest; response : TFHIRResponse);
    function ExecuteValidation(context: TFHIRValidatorContext; request: TFHIRRequest; response : TFHIRResponse; opDesc : String) : boolean;
    procedure ExecuteTransaction(upload : boolean; request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteMailBox(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteOperation(request: TFHIRRequest; response : TFHIRResponse);
    procedure SetConnection(const Value: TKDBConnection);
    procedure ReIndex;
    function IdentifyValueset(request: TFHIRRequest; resource : TFHIRResource; params: TParseMap; base: String; var used, cacheId: string; allowNull : boolean = false): TFHIRValueSet;
    procedure CheckCreateNarrative(request : TFHIRRequest);
  public
    Constructor Create(lang : String; repository : TFHIRDataStore);
    Destructor Destroy; Override;
    Property Connection : TKDBConnection read FConnection write SetConnection;
    Property Repository : TFHIRDataStore read FRepository;

    // internal utility functions
    procedure addParam(srch : TFhirConformanceRestResourceSearchParamList; html : TAdvStringBuilder; n, url, d : String; t : TFhirSearchParamType; tgts : TFhirResourceTypeSet);
    function AddResourceToFeed(feed: TFHIRAtomFeed; sId, sType, title, link, author, base, text : String; updated : TDateTime; resource : TFHIRResource; originalId : String; tags : TFHIRAtomCategoryList; current : boolean) : TFHIRAtomEntry; overload;
    function AddResourceToFeed(feed : TFHIRAtomFeed; sId, sType, base : String; textsummary, originalId : String; WantSummary : TFHIRSearchSummary; current : boolean) : TFHIRAtomEntry; overload;
    function AddDeletedResourceToFeed(feed : TFHIRAtomFeed; sId, sType, base : String; originalId : String) : TFHIRAtomEntry;
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
    procedure storeResources(list: TFHIRResourceList; upload : boolean);
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
  end;

  TFhirPatientEverythingOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  private
    function CreateMissingList(lcode, name, plural, pid : string): TFHIRList;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
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

  TFhirValidationOperation = class (TFHIROperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(manager: TFhirOperationManager; request: TFHIRRequest; response : TFHIRResponse); override;
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
  FOperations.add(TFhirValidationOperation.create);
  FOperations.add(TFhirGenerateDocumentOperation.create);
  FOperations.add(TFhirPatientEverythingOperation.create);
  FOperations.add(TFhirGenerateQAOperation.create);
  FOperations.add(TFhirHandleQAPostOperation.create);
  FOperations.add(TFhirQuestionnaireGenerationOperation.create);
  FOperations.add(TFhirProcessClaimOperation.create);

end;

function TFhirOperationManager.CreateDocumentAsBinary(mainRequest : TFhirRequest): String;
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
begin
  req := TFHIRRequest.Create;
  try
    req.Session := mainRequest.session.Link;
    req.ResourceType := frtBinary;
    req.CommandType := fcmdCreate;
//    req.categories.AddAll(mainRequest.Feed.categories);
    raise Exception.Create('TODO');
    req.Lang := mainRequest.Lang;
    if mainRequest.PostFormat = ffJson then
      req.Resource := FFactory.makeBinaryContent(mainRequest.Source, 'application/json+fhir')
    else
      req.Resource := FFactory.makeBinaryContent(mainRequest.Source, 'application/atom+xml');
    resp := TFHIRResponse.Create;
    try
      ExecuteCreate(nil, false, req, resp, idNoNew, 0);
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
  comp := mainRequest.Feed.entryList[0].resource as TFhirComposition;

  ref := TFhirDocumentReference.Create;
  try
    ref.masterIdentifier := FFactory.makeIdentifier('urn:ietf:rfc:3986', mainRequest.Feed.id);
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
    ref.confidentialityList.add(FFactory.makeCodeableConcept(FFactory.makeCoding('http://hl7.org/fhir/v3/Confidentiality', comp.statusElement.value, ''), ''));
    // populating DocumentReference.format:
    // we take any tags on the document. We ignore security tags. Always will be at least one - the document tag itself
//    for i := 0 to mainRequest.Feed.categories.Count - 1 do
//      if (mainRequest.Feed.categories[i].scheme <> 'http://hl7.org/fhir/tag/security') then
//        ref.formatList.Add(FFactory.makeUri(mainRequest.Feed.categories[i].term));
    raise Exception.Create('TODO');
    // todo: ref.hash (HexBinary representation of SHA1)
    attach := ref.contentList.Append;
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

    req := TFHIRRequest.Create;
    try
      req.Session := mainRequest.session.Link;
      req.ResourceType := frtDocumentReference;
      req.CommandType := fcmdCreate;
//      req.categories.AddAll(mainRequest.Feed.categories);
      raise Exception.Create('TODO');

      req.Lang := mainRequest.Lang;
      req.Resource := ref.Link;
      resp := TFHIRResponse.Create;
      try
        ExecuteCreate(nil, false, req, resp, idNoNew, 0);
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
    storeResources(list, true);
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


function TFhirOperationManager.AddResourceToFeed(feed: TFHIRAtomFeed; sId, sType, title, link, author, base, text : String; updated : TDateTime; resource : TFHIRResource; originalId : String; tags : TFHIRAtomCategoryList; current : boolean) : TFHIRAtomEntry;
var
  entry : TFHIRAtomEntry;
begin
  entry := TFHIRAtomEntry.Create;
  try
    if resource.meta = nil then
      resource.meta := TFhirMeta.create;
    tags.AddToList(resource.meta.tagList);
    entry.resource := resource.Link;
    feed.entryList.add(entry.Link);
    result := entry;
  finally
    entry.Free;
  end;
end;



function TFhirOperationManager.AddResourceToFeed(feed: TFHIRAtomFeed; sId, sType, base, textsummary, originalId : String; WantSummary : TFHIRSearchSummary; current : boolean) : TFHIRAtomEntry;
var
  parser : TFhirParser;
  blob : TBytes;
  binary : TFhirBinary;
  tags : TFHIRAtomCategoryList;
begin
  if (sId = '') Then
    sId := FConnection.ColStringByName['Id'];

  tags := TFHIRAtomCategoryList.create;
  try
    if (FConnection.ColIntegerByName['Deleted'] = 1)  then
       result := AddDeletedResourceToFeed(feed, sId, sType, base, originalId)
    else if WantSummary = ssText then
      result := AddResourceToFeed(feed, sId, sType, GetFhirMessage(sType, lang)+' "'+sId+'" '+GetFhirMessage('NAME_VERSION', lang)+' "'+FConnection.ColStringByName['VersionId']+'"',
                      base+sType+'/'+sId+'/_history/'+FConnection.ColStringByName['VersionId'],
                      FConnection.ColStringByName['Name'], base, textsummary, TSToDateTime(FConnection.ColTimeStampByName['StatedDate']), nil, originalId, tags, current)
    else
    begin
      if (WantSummary = ssFull) then
        blob := TryZDecompressBytes(FConnection.ColBlobByName['Content'])
      else
      begin
        blob := TryZDecompressBytes(FConnection.ColBlobByName['Summary']);
        tags.AddValue(tkTag, 'http://hl7.org/fhir/v3/ObservationValue', 'REDACTED', 'redacted');
        tags.AddValue(tkTag, 'http://hl7.org/fhir/v3/ActCode', 'NOREUSE', 'no reuse beyond purpose of use'); // e.g. don't update based on it

      end;

      parser := MakeParser(lang, ffXml, blob, xppDrop);
      try
        result := AddResourceToFeed(feed, sId, sType, GetFhirMessage(CODES_TFhirResourceType[parser.resource.resourceType], lang)+' "'+sId+'" '+GetFhirMessage('NAME_VERSION', lang)+' "'+FConnection.ColStringByName['VersionId']+'"',
                      base+CODES_TFhirResourceType[parser.resource.resourceType]+'/'+sId+'/_history/'+FConnection.ColStringByName['VersionId'],
                      FConnection.ColStringByName['Name'], base, textsummary, TSToDateTime(FConnection.ColTimeStampByName['StatedDate']), parser.resource, originalId, tags, current);
      finally
        parser.Free;
      end;
    end;
  finally
    tags.free;
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

(*
function TFhirOperationManager.EncodeFeedForStorage(feed: TFHIRAtomFeed; categories : TFHIRAtomCategoryList): String;
var
  s : TStringStream;
  xml : TFHIRXmlComposer;
begin
  s := TStringStream.create('');
  try
    xml := TFHIRXmlComposer.create(lang);
    try
      xml.Compose(s, feed, false);
    finally
      xml.free;
    end;
    result := #1+ZCompressStr(s.DataString) + #0 + categories.encode;
  finally
    s.Free;
  end;
end;

function TFhirOperationManager.EncodeResourceForStorage(resource: TFHIRResource; categories : TFHIRAtomCategoryList): String;
var
  s, sSum : TStringStream;
  xml : TFHIRXmlComposer;
begin
  s := TStringStream.create('');
  sSum := TStringStream.create('');
  try
    if resource is TFhirBinary then
    begin
      TFhirBinary(resource).Content.SaveToStream(s);
      result := #2+ZCompressStr(TFhirBinary(resource).ContentType + #0 + categories.encode+#0+s.DataString);
    end
    else
    begin
      xml := TFHIRXmlComposer.create(lang);
      try
        xml.Compose(s, '', '', resource, false);
        if Resource.hasASummary then
        begin
          xml.SummaryOnly := true;
          xml.Compose(sSum, '', '', resource, false);
          result := #4+sSum.DataString+#0+s.DataString + #0 + categories.encode;
        end
        else
          result := #3+s.DataString + #0 + categories.encode;
      finally
        xml.free;
      end;
    end;
  finally
    s.Free;
  end;
end;
*)


function TFhirOperationManager.EncodeFeed(r: TFHIRAtomFeed): TBytes;
var
  b : TBytesStream;
  xml : TFHIRXmlComposer;
begin
  b :=  TBytesStream.Create;
  try
    xml := TFHIRXmlComposer.Create('en');
    try
      xml.Compose(b, r, false);
    finally
      xml.Free;
    end;
    result := ZCompressBytes(copy(b.Bytes, 0, b.size));
  finally
    b.free;
  end;
end;

function TFhirOperationManager.EncodeResource(r: TFhirResource): TBytes;
var
  b : TBytesStream;
  xml : TFHIRXmlComposer;
  bin : TFhirBinary;
  i : integer;
  s : AnsiString;
begin
  b :=  TBytesStream.Create;
  try
      xml := TFHIRXmlComposer.Create('en');
      try
        xml.Compose(b, r, false, nil);
      finally
        xml.Free;
      end;
    result := ZCompressBytes(copy(b.Bytes, 0, b.size));
  finally
    b.free;
  end;
end;

function TFhirOperationManager.EncodeResourceSummary(r: TFhirResource): TBytes;
var
  b : TBytesStream;
  xml : TFHIRXmlComposer;
begin
  b :=  TBytesStream.Create;
  try
    xml := TFHIRXmlComposer.Create('en');
    try
      xml.SummaryOnly := true;
      xml.Compose(b, r, false, nil);
    finally
      xml.Free;
    end;
    result := ZCompressBytes(copy(b.Bytes, 0, b.size));
  finally
    b.free;
  end;
end;

Function TFhirOperationManager.Execute(request: TFHIRRequest; response : TFHIRResponse; upload : Boolean) : String;
begin
 // assert(FConnection.InTransaction);
  result := Request.Id;
  case request.CommandType of
    fcmdMailbox : ExecuteMailBox(request, response);
    fcmdRead : ExecuteRead(request, response);
    fcmdUpdate : ExecuteUpdate(nil, upload, request, response);
    fcmdVersionRead : ExecuteVersionRead(request, response);
    fcmdDelete : ExecuteDelete(request, response);
    fcmdHistoryInstance, fcmdHistoryType, fcmdHistorySystem : ExecuteHistory(request, response);
    fcmdSearch : ExecuteSearch(request, response);
    fcmdCreate : result := ExecuteCreate(nil, false, request, response, idNoNew, 0);
    fcmdConformanceStmt : ExecuteConformanceStmt(request, response);
    fcmdTransaction : ExecuteTransaction(upload, request, response);
    fcmdOperation : ExecuteOperation(request, response);
    fcmdUpload : ExecuteUpload(request, response);
  else
    Raise Exception.Create(GetFhirMessage('MSG_UNKNOWN_OPERATION', lang));
  End;
end;

procedure TFhirOperationManager.addParam(srch : TFhirConformanceRestResourceSearchParamList; html : TAdvStringBuilder; n, url, d : String; t : TFhirSearchParamType; tgts : TFhirResourceTypeSet);
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
    c.system := ContactPointSystemUrl;
    c.value := 'http://healthintersections.com.au/';
    if FRepository.FormalURLPlain <> '' then
      oConf.url := AppendForwardSlash(FRepository.FormalURLPlainOpen)+'metadata'
    else
      oConf.url := 'http://fhir.healthintersections.com.au/open/metadata';

    oConf.version := FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+'-'+SERVER_VERSION; // this conformance statement is versioned by both
    oConf.name := 'Health Intersections FHIR Server Conformance Statement';
    oConf.publisher := 'Health Intersections'; //
    oConf.description := 'Standard Conformance Statement for the open source Reference FHIR Server provided by Health Intersections';
    oConf.status := ConformanceStatementStatusActive;
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

    oConf.acceptUnknown := true;
    oConf.formatList.Append.value := 'application/xml+fhir';
    oConf.formatList.Append.value := 'application/json+fhir';


    oConf.fhirVersion := FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION;
    oConf.restList.add(TFhirConformanceRest.Create);
    oConf.restList[0].mode := RestfulConformanceModeServer;
    oConf.text := TFhirNarrative.create;
    oConf.text.status := NarrativeStatusGenerated;

    FRepository.TerminologyServer.declareSystems(oConf);
    if assigned(FOnPopulateConformance) then
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
              for i := 0 to FIndexer.Indexes.count - 1 do
                if (FIndexer.Indexes[i].ResourceType = a) then
                  addParam(res.searchParamList, html, FIndexer.Indexes[i].Name, FIndexer.Indexes[i].uri, FIndexer.Indexes[i].Description, FIndexer.Indexes[i].SearchType, FIndexer.Indexes[i].TargetTypes);

//              addParam(res.searchParamList, html, '_id', 'http://hl7.org/fhir/search', 'Resource Logical ID', SearchParamTypeToken, []);
              addParam(res.searchParamList, html, '_text', 'http://hl7.org/fhir/search', 'General Text Search of the narrative portion', SearchParamTypeString, []);
              addParam(res.searchParamList, html, '_profile', 'http://hl7.org/fhir/search', 'Search for resources that conform to a profile', SearchParamTypeReference, []);
              addParam(res.searchParamList, html, '_security', 'http://hl7.org/fhir/search', 'Search for resources that have a particular security tag', SearchParamTypeReference, []);
              addParam(res.searchParamList, html, '_sort', 'http://hl7.org/fhir/search', 'Specify one or more other parameters to use as the sort order', SearchParamTypeToken, []);
              addParam(res.searchParamList, html, '_count', 'http://hl7.org/fhir/search', 'Number of records to return', SearchParamTypeNumber, []);
              addParam(res.searchParamList, html, '_summary', 'http://hl7.org/fhir/search', 'Return just a summary for resources that define a summary view', SearchParamTypeNumber, []);
              addParam(res.searchParamList, html, '_include', 'http://hl7.org/fhir/search', 'Additional resources to return - other resources that matching resources refer to', SearchParamTypeToken, ALL_RESOURCE_TYPES);
              addParam(res.searchParamList, html, '_reverseInclude', 'http://hl7.org/fhir/search', 'Additional resources to return - other resources that refer to matching resources (this is trialing an extension to the specification)', SearchParamTypeToken, ALL_RESOURCE_TYPES);
              if a in [frtValueSet, frtConceptMap] then
                addParam(res.searchParamList, html, '_query', 'http://hl7.org/fhir/search', 'Specified Named Query', SearchParamTypeToken, []);

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
    AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
end;

Function TFhirOperationManager.ExecuteCreate(context: TFHIRValidatorContext; upload : boolean; request: TFHIRRequest; response: TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
var
  sId, s : String;
  resourceKey, i : Integer;
  key : Integer;
  tags : TFHIRAtomCategoryList;
  ok : boolean;
  tnow : TDateTime;
  list : TMatchingResourceList;
begin
  CheckCreateNarrative(request);
  try
    ok := true;
    if not check(response, request.canWrite(request.ResourceType) and opAllowed(request.ResourceType, fcmdCreate), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;

    if ok and not check(response, request.Resource <> nil, 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) or not check(response, request.ResourceType = request.resource.ResourceType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang)) then
      ok := false;

    if ok and FRepository.Validate and not upload and (request.Session <> nil) then
    begin
      if not ExecuteValidation(context, request, response, 'Create Resource '+CODES_TFHIRResourceType[request.ResourceType]+'/'+request.Id+' ('+request.originalId+')') then
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
        ok := false
    ;


    if ok then
    begin

      if request.resource.meta = nil then
        request.resource.meta := TFhirMeta.Create;
      request.Resource.meta.lastUpdated := NowUTC;
      request.Resource.meta.versionId := '1';
      updateProvenance(request.Provenance, CODES_TFHIRResourceType[request.ResourceType], sid, '1');
      tnow := request.Resource.meta.lastUpdated.AsUTC.GetDateTime;


      tags := TFHIRAtomCategoryList.create;
      try
        tags.CopyTags(request.resource.meta);

        checkProposedContent(request, request.Resource, tags);
        result := sId;
        request.id := sId;
        key := FRepository.NextVersionKey;
        for i := 0 to tags.count - 1 do
          FRepository.RegisterTag(tags[i], FConnection);

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Deleted, SessionKey, TextSummary, Tags, Content, Summary) values (:k, :rk, :sd, :td, :v, :f, 0, :s, :tx, :tb, :cb, :sb)';
        FConnection.prepare;
        try
          FConnection.BindInteger('k', key);
          FConnection.BindInteger('rk', resourceKey);
          FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
          FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
          request.SubId := '1';
          FConnection.BindString('v', '1');
          FConnection.BindString('tx', TextSummaryForResource(request.Resource));
          if request.Session <> nil then
            FConnection.BindInteger('s', request.Session.Key)
          else
            FConnection.BindInteger('s', 0);
          if request.Resource <> nil then
          begin
            FConnection.BindInteger('f', 2);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('cb', EncodeResource(request.Resource));
            FConnection.BindBlobFromBytes('sb', EncodeResourceSummary(request.Resource));
          end
          else
          begin
            FConnection.BindInteger('f', 1);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('cb', EncodeFeed(request.Feed));
            FConnection.BindNull('sb');
            Raise Exception.Create('not supported at this time');
          end;
          FConnection.Execute;
          CommitTags(tags, key);
        finally
          FConnection.Terminate;
        end;
        FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 0 where ResourceKey = '+inttostr(resourceKey));
        CheckCompartments(FIndexer.execute(resourceKey, sId, request.resource, tags), request.compartments);
        FRepository.SeeResource(resourceKey, key, sId, request.Resource, FConnection, false, request.Session);
        if request.resourceType = frtPatient then
          FConnection.execSQL('update Compartments set CompartmentKey = '+inttostr(resourceKey)+' where Id = '''+sid+''' and CompartmentKey is null');
        response.HTTPCode := 201;
        response.Message := 'Created';
        response.ContentLocation := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+sId+'/_history/1';
        response.Location := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+sId+'/_history/1';
        response.Resource := request.Resource.Link;

        response.id := sId;
      finally
        tags.free;
      end;
      if request.Provenance <> nil then
        SaveProvenance(request.Session, request.Provenance);
    end;
    if request.ResourceType <> frtAuditEvent then // else you never stop
      AuditRest(request.session, request.ip, request.ResourceType, sid, '1', request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceType <> frtAuditEvent then // else you never stop
        AuditRest(request.session, request.ip, request.ResourceType, sid, '1', request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperationManager.ExecuteDelete(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  key, nvid, i : Integer;
  originalId : String;
  tnow : TDateTime;
  tags : TFHIRAtomCategoryList;
  list : TMatchingResourceList;
  ok : boolean;

  meta : TFhirMeta;
  rp : TFhirParameters;

begin
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
       not FindResource(request.ResourceType, request.Id, true, resourceKey, originalId, request, response, request.compartments)) then
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

      tags := TFHIRAtomCategoryList.create;

      meta := nil;
      rp := TFhirParameters.create;

      try

        if request.resource <> nil then
        begin
          meta := request.resource.meta.Link;
          tags.CopyTags(request.resource.meta);
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

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Deleted, SessionKey, Tags, Content) values '+
                                                             '(:k,:rk, :sd, :td, :v, :f, 1, :s, :t, :c)';
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
          FConnection.BindBlobFromBytes('c', EncodeResource(rp));

          FConnection.Execute;
        finally
          FConnection.Terminate;
        end;
        FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 1 where ResourceKey = '+inttostr(resourceKey));
        CommitTags(tags, key);
        FConnection.execSQL('Delete from IndexEntries where ResourceKey = '+IntToStr(resourceKey));
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
    AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
end;

function TFhirOperationManager.BuildHistoryResultSet(request: TFHIRRequest; response: TFHIRResponse; var searchKey, link, sql, title, base : String; var total : Integer) : boolean;
var
  cmp : String;
  resourceKey : integer;
  originalId, lt : string;
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
        else if (length(request.id) > ID_LENGTH) or not FindResource(request.ResourceType, request.Id, true, resourceKey, originalId, request, response, request.compartments) then
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

procedure TFhirOperationManager.ExecuteHandleQAPost(request: TFHIRRequest; response: TFHIRResponse);
var
  builder : TQuestionnaireBuilder;
begin
  // first, we convert to a resource.
  builder := TQuestionnaireBuilder.Create;
  try
    builder.Answers := request.Resource.link as TFhirQuestionnaireAnswers;
    builder.Profiles := FRepository.Profiles.Link;
    builder.Profile := GetProfileById(request, extractProfileId(request.baseUrl, builder.Answers.questionnaire.reference), '');
    builder.onLookupReference := LookupReference;
    builder.onLookupCode := FRepository.LookupCode;
    builder.UnBuild;

    // now, we handle it.
    // todo: tag the resource with a profile
//    if not (builder.Profile.url.StartsWith('http://hl7.org/fhir/Profile') and StringArrayExistsSensitive(CODES_TFhirResourceType, builder.Profile.url.Substring(28)))  then
//      request.categories.AddValue('http://hl7.org/fhir/tag/profile', builder.Profile.url, builder.Profile.name);
    raise Exception.Create('TODO');


    request.Resource := builder.Resource.Link;
  finally
    builder.Free;
  end;
  if request.ResourceType = frtNull then
  begin
    request.ResourceType := request.Resource.ResourceType;
    ExecuteCreate(nil, false, request, response, idNoNew, 0);
  end
  else if (request.id <> '') and (request.subId = '') then
    ExecuteUpdate(nil, false, request, response)
  else
    raise Exception.Create('Error in call - can only be called against the system, or a specific resource');
end;


procedure TFhirOperationManager.ExecuteHistory(request: TFHIRRequest; response: TFHIRResponse);
var
//  resourceKey : Integer;
//  feed : TFHIRAtomFeed;
//  originalId : String;
  offset, count, i : integer;
//  ok : boolean;
//  base : String;
//  cmp : String;
  feed : TFHIRAtomFeed;
//  entry : TFHIRAtomEntry;
//  includes : TReferenceList;
//  id, link, base, sql : String;
//  i, total, t : Integer;
//  key : integer;
//  offset, count : integer;
  ok : boolean;
  id : String;
  wantSummary : TFHIRSearchSummary;
  link : string;
  sql : String;
  title : String;
  base : String;
//  keys : TStringList;
  total : integer;
begin
  try
    if request.parameters.value[HISTORY_PARAM_NAME_ID] <> '' then
    begin
      ok := FindSavedSearch(request.parameters.value[HISTORY_PARAM_NAME_ID], request.Session, 2, id, link, sql, title, base, total, wantSummary);
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

      feed := TFHIRAtomFeed.Create(BundleTypeHistory);
      try
        feed.Base := request.baseUrl;
        if response.Format <> ffAsIs then
          base := base + '&_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
        feed.total := inttostr(total);
        feed.Tags['sql'] := sql;
        feed.link_List.AddRelRef('self', base+link);

        feed.link_List.AddRelRef('self', base+link);

        if (offset > 0) or (Count < total) then
        begin
          feed.link_List.AddRelRef('first', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
          if offset - count >= 0 then
            feed.link_List.AddRelRef('previous', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
          if offset + count < total then
            feed.link_List.AddRelRef('next', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
          if count < total then
            feed.link_List.AddRelRef('last', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((total div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
        end;

        FConnection.SQL := 'Select Ids.ResourceKey, ResourceName, Ids.Id, Versions.ResourceVersionKey, MostRecent, VersionId, StatedDate, Name, Versions.Deleted, originalId, TextSummary, Tags, Content, Summary from Versions, Ids, Sessions, SearchEntries, Types '+
            'where SearchEntries.ResourceVersionKey = Versions.ResourceVersionKey and Types.ResourceTypeKey = Ids.ResourceTypeKey and '+'Versions.SessionKey = Sessions.SessionKey and SearchEntries.ResourceKey = Ids.ResourceKey and SearchEntries.SearchKey = '+id+' '+
            'order by SearchEntries.ResourceVersionKey DESC OFFSET '+inttostr(offset)+' ROWS FETCH NEXT '+IntToStr(count+1)+' ROWS ONLY';
        FConnection.Prepare;
        try
          FConnection.Execute;
          while FConnection.FetchNext do
            AddResourceToFeed(feed, '', FConnection.colStringByName['ResourceName'], request.baseUrl, FConnection.colstringByName['TextSummary'], FConnection.colstringByName['originalId'], wantsummary, FConnection.ColIntegerByName['MostRecent'] = FConnection.ColIntegerByName['ResourceVersionKey']);
        finally
          FConnection.Terminate;
        end;

        feed.id := FhirGUIDToString(CreateGUID);
        feed.Tags['sql'] := sql;

        response.HTTPCode := 200;
        response.Message := 'OK';
        response.Body := '';
        response.Feed := feed.Link;
      finally
        feed.Free;
      end;
      AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, request.Provenance, response.httpCode, '', response.message);
    end;
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
end;


procedure TFhirOperationManager.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : integer;
  originalId : String;
begin
  try
    NotFound(request, response);
    if request.canRead(request.ResourceType) and check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (length(request.id) <= ID_LENGTH) and FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, request, response, request.compartments) then
      begin
        FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
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
              response.originalId := originalId;
              response.LastModifiedDate := TSToDateTime(FConnection.ColTimeStampByName['StatedDate']);
              response.ContentLocation := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/_history/'+response.versionId;

              if not (request.ResourceType in [frtStructureDefinition]) then
                response.link_List.AddRelRef('edit-form', request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/$qa-edit');

              response.link_List.AddRelRef('z-edit-src', request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/$edit');
              processBlob(request, Response, false);
            end;
          end;
        finally
          FConnection.Terminate;
        end;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
end;


procedure TFhirOperationManager.executeReadInTransaction(entry : TFhirBundleEntryTransaction; request: TFHIRRequest; response: TFHIRResponse);
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

function TFhirOperationManager.FindSavedSearch(const sId : String; Session : TFHIRSession; typeKey : integer; var id, link, sql, title, base : String; var total : Integer; var wantSummary : TFHIRSearchSummary): boolean;
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
          wantSummary := TFHIRSearchSummary(FConnection.GetColIntegerByName('Summary'));
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

function TFhirOperationManager.BuildSearchResultSet(typekey : integer; session : TFHIRSession; aType : TFHIRResourceType; params : TParseMap; baseURL, compartments, compartmentId : String; var link, sql : String; var total : Integer; var wantSummary : TFHIRSearchSummary):String;
var
  id : string;
begin
  id := FhirGUIDToString(CreateGuid);
  result := inttostr(FRepository.NextSearchKey);
  if params.VarExists('_query') then
  begin
    raise exception.create('The query "'+params.getVar('_query')+'" is not known');
  end
  else
    ProcessDefaultSearch(typekey, session, aType, params, baseURL, compartments, compartmentId, id, result, link, sql, total, wantSummary);
end;

procedure TFhirOperationManager.ProcessDefaultSearch(typekey : integer; session : TFHIRSession; aType : TFHIRResourceType; params : TParseMap; baseURL, compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; var wantSummary : TFHIRSearchSummary);
var
  sp : TSearchProcessor;
  s : String;
begin
  if session.canReadAll then
    s := 'null'
  else
    s := inttostr(Session.Key);
  FConnection.ExecSQL('insert into Searches (SearchKey, Id, Count, Type, Date, Summary, SessionKey) values ('+key+', '''+id+''', 0, 1, '+DBGetDate(FConnection.Owner.Platform)+', '+inttostr(ord(wantSummary))+', '+s+')');
  sp := TSearchProcessor.create;
  try
    sp.typekey := typekey;
    sp.type_ := aType;
    sp.compartmentId := compartmentId;
    sp.compartments := compartments;
    sp.baseURL := baseURL;
    sp.lang := lang;
    sp.params := params;
    sp.indexer := FIndexer.Link;
    sp.repository := FRepository.Link;
    sp.session := session.link;

    sp.build;
    sql := 'Insert into SearchEntries Select '+key+', ResourceKey, MostRecent as ResourceVersionKey, '+sp.sort+' from Ids where Deleted = 0 and '+sp.filter+' order by ResourceKey DESC';
    link := SEARCH_PARAM_NAME_ID+'='+id+'&'+sp.link_;
    wantSummary := sp.wantSummary;
  finally
    sp.free;
  end;

  FConnection.ExecSQL(sql);
  total := FConnection.CountSQL('Select count(*) from SearchEntries where SearchKey = '+key);
  FConnection.Sql := 'update Searches set Link = :l, SqlCode = :s, count = '+inttostr(total)+', Summary = '+inttostr(ord(wantSummary))+' where SearchKey = '+key;
  FConnection.Prepare;
  try
    FConnection.BindBlobFromString('l', link);
    FConnection.BindBlobFromString('s', sql);
    FConnection.Execute;
  finally
    FConnection.Terminate;
  end;
end;


procedure TFhirOperationManager.ExecuteSearch(request: TFHIRRequest; response: TFHIRResponse);
var
  feed : TFHIRAtomFeed;
  entry : TFHIRAtomEntry;
  includes : TReferenceList;
  id, link, base, sql : String;
  i, total, t : Integer;
  key : integer;
  offset, count : integer;
  ok : boolean;
  wantsummary : TFHIRSearchSummary;
  title: string;
  keys : TStringList;
begin
  try
    ok := true;
    wantsummary := ssFull;
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
        feed := TFHIRAtomFeed.Create(BundleTypeSearchset);
        includes := TReferenceList.create;
        keys := TStringList.Create;
        try
          feed.base := request.baseUrl;

            if FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], request.Session, 1, id, link, sql, title, base, total, wantSummary) then
              link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
            else
              id := BuildSearchResultSet(key, request.Session, request.resourceType, request.Parameters, request.baseUrl, request.compartments, request.compartmentId, link, sql, total, wantSummary);

            feed.total := inttostr(total);
            feed.Tags['sql'] := sql;

            base := AppendForwardSlash(Request.baseUrl)+CODES_TFhirResourceType[request.ResourceType]+'/_search?';
            if response.Format <> ffAsIs then
              base := base + '_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
            feed.link_List.AddRelRef('self', base+link);

            offset := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_OFFSET), 0);
            if request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT) = 'all' then
              count := SUMMARY_SEARCH_PAGE_LIMIT
            else
              count := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT), 0);
            if (count < 2) then
              count := SEARCH_PAGE_DEFAULT
            else if (wantsummary = ssSummary) and (Count > SUMMARY_SEARCH_PAGE_LIMIT) then
              count := SUMMARY_SEARCH_PAGE_LIMIT
            else if (wantsummary = ssText) and (Count > SUMMARY_TEXT_SEARCH_PAGE_LIMIT) then
              count := SUMMARY_TEXT_SEARCH_PAGE_LIMIT
            else if (Count > SEARCH_PAGE_LIMIT) then
              count := SEARCH_PAGE_LIMIT;

            if (offset > 0) or (Count < total) then
            begin
              feed.link_List.AddRelRef('first', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              if offset - count >= 0 then
                feed.link_List.AddRelRef('previous', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              if offset + count < total then
                feed.link_List.AddRelRef('next', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              if count < total then
                feed.link_List.AddRelRef('last', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((total div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
            end;

            FConnection.SQL := 'Select Ids.ResourceKey, Ids.Id, VersionId, StatedDate, Name, originalId, Versions.Deleted, TextSummary, Tags, Content, Summary from Versions, Ids, Sessions, SearchEntries '+
                'where Ids.Deleted = 0 and SearchEntries.ResourceVersionKey = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey and SearchEntries.ResourceKey = Ids.ResourceKey and SearchEntries.SearchKey = '+id+' '+
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
                  entry := AddResourceToFeed(feed, '', CODES_TFHIRResourceType[request.ResourceType], request.baseUrl, FConnection.colstringByName['TextSummary'], FConnection.colstringByName['originalId'], wantsummary, true);
                  keys.Add(FConnection.ColStringByName['ResourceKey']);

                  if request.Parameters.VarExists('_include') then
                    CollectIncludes(request.session, includes, entry.resource, request.Parameters.GetVar('_include'));
                  inc(t);
                end;
                if (t = count) then
                  break;
              End;
            finally
              FConnection.Terminate;
            end;
          // process reverse includes
          if request.Parameters.VarExists('_reverseInclude') then
            CollectReverseIncludes(request.session, includes, keys, request.Parameters.GetVar('_reverseInclude'), feed, request, wantsummary);

          //now, add the includes
          if includes.Count > 0 then
          begin
            FConnection.SQL := 'Select ResourceTypeKey, Ids.Id, VersionId, StatedDate, Name, originalId, Versions.Deleted, TextSummary, Tags, Content from Versions, Sessions, Ids '+
                'where Ids.Deleted = 0 and Ids.MostRecent = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey '+
                'and Ids.ResourceKey in (select ResourceKey from IndexEntries where '+includes.asSql+') order by ResourceVersionKey DESC';
            FConnection.Prepare;
            try
              FConnection.Execute;
              while FConnection.FetchNext do
                AddResourceToFeed(feed, '', CODES_TFHIRResourceType[TypeForKey(FConnection.ColIntegerByName['ResourceTypeKey'])], request.baseUrl, FConnection.ColStringByName['TextSummary'], FConnection.ColStringByName['originalId'], wantSummary, true);
            finally
              FConnection.Terminate;
            end;
          end;

          feed.id := FhirGUIDToString(CreateGUID);

          //feed.link_List['self'] := request.url;
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.Body := '';
          response.Resource := nil;
          response.Feed := feed.Link;
        finally
          includes.free;
          keys.Free;
          feed.Free;
        end;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message);
      raise;
    end;
  end;
end;


function TFhirOperationManager.ExecuteUpdate(context: TFHIRValidatorContext; upload : boolean; request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  resourceKey : Integer;
  key, nvid, i : Integer;
  s, originalId : String;
  tags : TFHIRAtomCategoryList;
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
       not check(response, request.ResourceType = request.resource.ResourceType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang)) or
       not check(response, length(request.id) <= ID_LENGTH, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id]))) then
      ok := false;

    if ok and FRepository.Validate and not upload then
    begin
      if not ExecuteValidation(context, request, response, 'Update Resource '+CODES_TFHIRResourceType[request.ResourceType]+'/'+request.Id) then
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
          ExecuteCreate(context, upload, request, response, idMaybeNew, 0);
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

    if ok and not FindResource(request.ResourceType, request.Id, true, resourceKey, originalId, request, response, request.compartments) Then
    begin
      ExecuteCreate(context, upload, request, response, idMaybeNew, 0);
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
      tags := TFHIRAtomCategoryList.create;
      try
        if request.resource.meta = nil then
          request.resource.meta := TFhirMeta.Create;
        tags.CopyTags(request.resource.meta);
        LoadTags(tags, ResourceKey);
        tags.writeTags(request.resource.meta);
        request.Resource.meta.lastUpdated := NowUTC;
        request.Resource.meta.versionId := inttostr(nvid);
        updateProvenance(request.Provenance, CODES_TFHIRResourceType[request.ResourceType], request.Id, inttostr(nvid));
        tnow := request.Resource.meta.lastUpdated.AsUTCDateTime;


        checkProposedContent(request, request.Resource, tags);

        for i := 0 to tags.count - 1 do
          FRepository.RegisterTag(tags[i], FConnection);

        FConnection.execSQL('Delete from IndexEntries where ResourceKey = '+IntToStr(resourceKey));

        // check whether originalId matches?
        // to do: merging

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, TransactionDate, StatedDate, Format, VersionId, Deleted, SessionKey, TextSummary, Tags, Content, Summary) values (:k, :rk, :td, :sd, :f, :v, 0, :s, :tx, :tb, :cb, :sb)';
        FConnection.prepare;
        try
          FConnection.BindInteger('k', key);
          FConnection.BindInteger('rk', resourceKey);
          FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
          FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
          FConnection.BindString('v', inttostr(nvid));
          FConnection.BindString('tx', TextSummaryForResource(request.Resource));
          request.SubId := inttostr(nvid);
          if request.Session = nil then
            FConnection.BindNull('s')
          else
            FConnection.BindInteger('s', request.Session.Key);
          if request.Resource <> nil then
          begin
            FConnection.BindInteger('f', 2);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('cb', EncodeResource(request.Resource));
            FConnection.BindBlobFromBytes('sb', EncodeResourceSummary(request.Resource));
          end
          else
          begin
            FConnection.BindInteger('f', 1);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('cb', EncodeFeed(request.Feed));
            FConnection.BindNull('sb');
          end;
          FConnection.Execute;
        finally
          FConnection.Terminate;
        end;
        FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+', Deleted = 0 where ResourceKey = '+inttostr(resourceKey));
        CommitTags(tags, key);
        FIndexer.execute(resourceKey, request.id, request.resource, tags);
        FRepository.SeeResource(resourceKey, key, request.id, request.Resource, FConnection, false, request.Session);

        if (response.Resource <> nil) and (response.Resource is TFhirBundle)  then
          response.Feed.entryList.add(request.Resource.Link)
        else
          response.Resource := request.Resource.Link;
        response.versionId := inttostr(nvid);

      finally
        tags.free;
      end;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.lastModifiedDate := tnow;
      response.ContentLocation := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.Id+'/_history/'+inttostr(nvid);
      if request.Provenance <> nil then
        SaveProvenance(request.Session, request.Provenance);
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
end;


function TFhirOperationManager.ExecuteValidation(context: TFHIRValidatorContext; request: TFHIRRequest; response: TFHIRResponse; opDesc : String) : boolean;
var
  outcome : TFhirOperationOutcome;
  i : integer;
  buffer : TAdvBuffer;
  mem : TAdvMemoryStream;
  xml : TFHIRComposer;
  vcl : TVclStream;
  profileId : String;
  profile : TFHirStructureDefinition;
begin
  profileId := '';
  profile := nil;

  try
    if request.resource.meta <> nil then
      for i := 0 to request.resource.meta.profileList.Count - 1 do
        profileId := request.resource.meta.profileList[i].value;


    if StringStartsWith(ProfileId, 'http://localhost/profile/') then
      profile := GetProfileById(request, copy(ProfileId, 27, $FF), request.baseUrl)
    else if (request.baseUrl <> '') and StringStartsWith(ProfileId, request.baseUrl) then
      profile := GetProfileById(request, copy(ProfileId, length(request.baseUrl), $FF), request.baseUrl);

    if opDesc = '' then
      if Profile = nil then
        opDesc := 'Validate resource '+request.id
      else
        opDesc := 'Validate resource '+request.id+' against profile '+profileId;

    if request.resourceType = frtBinary then
    begin
      outcome := TFhirOperationOutcome.create;
    end
    else if (request.Source <> nil) and (request.PostFormat = ffXml) then
      outcome := FRepository.validator.validateInstance(context, request.Source, opDesc, profile)
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
            else if request.feed <> nil then
              xml.Compose(vcl, request.feed, true)
            else
              raise Exception.Create('Error: '+response.Message);
          finally
            xml.free;
          end;
        finally
          vcl.free;
          mem.free;
        end;
        outcome := FRepository.validator.validateInstance(context, buffer, opDesc, profile);
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
      AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceType <> frtAuditEvent then
        AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperationManager.ExecuteVersionRead(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : integer;
  originalId : String;
begin
  try
    NotFound(request, response);
    if check(response, request.canRead(request.ResourceType) and opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (length(request.id) <= ID_LENGTH) and (length(request.subid) <= ID_LENGTH) and FindResource(request.ResourceType, request.Id, true, resourceKey, originalId, request, response, request.compartments) then
      begin
        VersionNotFound(request, response);
        FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' and VersionId = :v';
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
              response.originalId := originalId;
              response.LastModifiedDate := TSToDateTime(FConnection.ColTimeStampByName['StatedDate']);
              processBlob(request, response, false);
            end;
          end;
        finally
          FConnection.Terminate;
        end;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, request.SubId, request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, request.SubId, request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
end;

function TFhirOperationManager.FindResource(aType : TFHIRResourceType; sId: String; bAllowDeleted : boolean; var resourceKey: integer; var originalId : String; request: TFHIRRequest; response : TFhirResponse; compartments : String): boolean;
var
  cmp : String;
begin
  if (compartments <> '') then
    cmp := ' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id in ('+compartments+'))'
  else
    cmp := '';

  FConnection.sql := 'select Ids.ResourceKey, OriginalId, Deleted from Ids, Types where Ids.id = :s '+cmp+' and Ids.ResourceTypeKey = Types.ResourceTypeKey and Supported = 1 and ResourceName = :n';
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
      originalId := FConnection.ColStringByName['OriginalId'];
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
    resourceKey := FRepository.NextResourceKey;
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
    id := inttostr(FConnection.ColIntegerByName['LastId']+1);
    guid := FConnection.ColIntegerByName['IdGuids'] = 1;
  end;
  FConnection.Terminate;
  result := iType > 0;
  if result then
  begin
    if guid then
      id := FhirGUIDToString(CreateGUID)
    else
      FConnection.ExecSQL('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(iType));
    key := FRepository.NextResourceKey;
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
TFHIRXhtmlComposer.Header(request.Session, request.baseUrl, request.lang)+
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
    m := TStringList.Create;
    try
      for i := 0 to FIndexer.Indexes.Count - 1 Do
        if m.IndexOf(FIndexer.Indexes[i].Name) = -1 then
          m.addObject(FIndexer.Indexes[i].Name, FIndexer.Indexes[i]); // first one wins!
      m.Sort;
      for i := 0 to m.count - 1 do
      begin
        ix := TFhirIndex(m.objects[i]);
        if (ix.TargetTypes = []) then
          if ix.SearchType = SearchParamTypeDate then
          begin
            s := s + '<tr><td align="left">'+ix.Name+' </td><td><input type="text" name="'+ix.Name+'"></td><td>(Date)</td>'+
            '<td><select title="Missing?" size="1" name="'+ix.Name+'-missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10;
            s := s + '<tr><td align="right"> (before)</td><td><input type="text" name="'+ix.Name+':before"></td><td> (before given date)</td><td></td></tr>'#13#10;
            s := s + '<tr><td align="right"> (after)</td><td><input type="text" name="'+ix.Name+':after"></td><td> (after given date)</td><td></td></tr>'#13#10
          end
          else
            s := s + '<tr><td align="left">'+ix.Name+'</td><td><input type="text" name="'+ix.Name+'"></td><td></td>'+
            '<td><select title="Missing?" size="1" name="'+ix.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10


      end;
    finally
      m.free;
    end;
  end
  else
  begin
    s := s +'<tr><td colspan="4"><b>'+CODES_TFHIRResourceType[request.ResourceType]+':</b></td></tr>'+#13#10;
    for i := 0 to FIndexer.Indexes.Count - 1 Do
    begin
      ix := FIndexer.Indexes[i];
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

    for i := 0 to FIndexer.Indexes.Count - 1 Do
    begin
      ix := FIndexer.Indexes[i];
      if (ix.ResourceType = request.ResourceType) and (ix.TargetTypes <> []) then
      begin
        pfx := ix.Name;
        types := ix.TargetTypes;
        s := s +'<tr><td colspan="4"><b>'+ix.Name+'</b> ('+describeResourceTypes(types)+')<b>:</b></td></tr>'+#13#10;
        m := TStringList.create;
        try
          for j := 0 to FIndexer.Indexes.Count - 1 Do
          begin
            ix2 := FIndexer.Indexes[j];
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
  for i := 0 to FIndexer.Indexes.Count - 1 Do
  begin
    ix := FIndexer.Indexes[i];
    if (ix.ResourceType = request.ResourceType) then
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


procedure TFhirOperationManager.SetConnection(const Value: TKDBConnection);
begin
  FConnection := Value;

  if (Value <> nil) then
  begin
    FSpaces := TFHIRIndexSpaces.Create(Value);
    FIndexer := TFHIRIndexManager.Create(FSpaces);
    FIndexer.TerminologyServer := FRepository.TerminologyServer.Link;
    FIndexer.Bases := FRepository.Bases;
    FIndexer.KeyEvent := FRepository.GetNextKey;
  end;
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

    if ok and not check(response, request.canWrite(request.ResourceType) and (request.Resource <> nil) or ((request.feed <> nil) and (request.feed.entryList.count > 0)), 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) then
      ok := false;
    if ok and (request.Resource <> nil) then
      if not check(response, request.ResourceType = request.resource.ResourceType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang)) then
        ok := false;
    // todo: check version id integrity
    // todo: check version integrity

    if not ok then
      // do nothing
    else if request.Feed <> nil then
    begin
      ExecuteTransaction(true, request, response);
      exit;
    end
    else
    begin
      ExecuteCreate(nil, true, request, response, idMaybeNew, 0);
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
    AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, 500, '', e.message);
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
      sp.indexer := FIndexer.Link;
      sp.repository := FRepository.Link;

      sp.build;

      FConnection.SQL := 'Select Ids.ResourceKey, Id, VersionId from Ids, Versions where ResourceTypeKey = '+inttostr(key)+' and MostRecent = ResourceVersionKey and Ids.ResourceKey in (select ResourceKey from IndexEntries where '+sp.filter+')';
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


function TFhirOperationManager.ResolveSearchIdCount(atype: TFHIRResourceType; compartmentId, compartments, baseURL, params: String): integer;
var
  list : TMatchingResourceList;
begin
  list := ResolveSearchId(atype, compartmentId, compartments, baseURL, params);
  try
    result := list.Count;
  finally
    list.free;
  end;
end;

procedure TFhirOperationManager.SaveProvenance(session: TFhirSession; prv: TFHIRProvenance);
begin
  prv.id := '';

  FRepository.QueueResource(prv);
end;

function TFhirOperationManager.scanId(base : String; request : TFHIRRequest; entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList; index : integer) : TFHIRTransactionEntry;
var
  match : boolean;
  id : TFHIRTransactionEntry;
  i, k : integer;
  sId, oId, s, cver : String;
  sParts : TArray<String>;
  baseok : boolean;
  aType : TFHIRResourceType;
  list : TMatchingResourceList;
begin

  if entry.resource <> nil then
    aType := entry.resource.ResourceType
  else if (entry.transaction = nil) or not (entry.transaction.method in [HttpVerbDELETE, HttpVerbGET]) or (entry.transaction.url = '') then // must be deleting something
    raise Exception.Create('A resource must be provided unless the entry has a transaction with method = delete and a url (entry '+inttostr(index+1)+')')
  else
  begin
    s := entry.transaction.url;
    if s.contains('?') then
      s := s.subString(0, s.indexOf('?'));
    sParts := s.Split(['/']);
    i := StringArrayIndexOfSensitive(CODES_TFhirResourceType, sParts[0]);
    if i = -1 then
      raise Exception.Create('Unknown resource type '+sParts[0]+' in deletion URL');
    aType := TFhirResourceType(i);
  end;

  if (entry.transaction <> nil) and (entry.transaction.method = HttpVerbDELETE) and not FRepository.ResConfig[aType].cmdDelete then
    Raise Exception.create('Deleting Resource '+CODES_TFHIRResourceType[entry.resource.ResourceType]+' is not supported in Transactions (entry '+inttostr(index+1)+')');


  if not FRepository.ResConfig[aType].cmdUpdate and not FRepository.ResConfig[aType].cmdCreate then
    Raise Exception.create('Resource '+CODES_TFHIRResourceType[entry.resource.ResourceType]+' is not supported in Transactions (entry '+inttostr(index+1)+')');

  if (entry.resource <> nil) and  (entry.resource.id.Contains('[x]')) then
    raise Exception.Create('not handled - error in transaction (entry '+inttostr(index+1)+')');
    // this was a work around an for an error in the build tool - not needed any more
//    entry.resource.id := entry.resource.id.replace('[x]',CODES_TFHIRResourceType[entry.resource.ResourceType]);

  id := TFHIRTransactionEntry.create;
  try
    id.ResType := aType;

    // first, sort out the base
    if entry.base <> '' then
      base := entry.base;
    if base = '' then
      base := request.baseUrl;
    if base = '' then
      raise Exception.Create('Unable to process transaction - no base found or could be implied (entry '+inttostr(index+1)+')');
    baseok := (AppendForwardSlash(base) = AppendForwardSlash(request.baseUrl)) or (FRepository.Bases.IndexOf(AppendForwardSlash(base)) > -1);

    if (entry.resource <> nil) then
      id.Name := fullResourceUri(base, entry.resource.ResourceType, entry.resource.id)
    else if entry.transaction <> nil then
      id.Name := fullResourceUri(base, entry.transaction.url)
    else
      id.Name := '??';
    id.outcomeVersion := 1;

    // figure out what kind of operation is involved here
    if entry.transaction <> nil then
    begin
      // does base matter?
      case entry.transaction.method of
        HttpVerbGET :
          begin
            id.state := tesRead; // at this point, this is a null operation;
          end;
        HttpVerbPOST :
          begin
            id.state := tesCreate;
            if entry.transaction.ifNoneExist <> '' then
            begin
              s := entry.transaction.IfNoneExist;
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
            if entry.transaction.url.Contains('?') then
            begin
              s := entry.transaction.url.substring(entry.transaction.url.IndexOf('?')+1);
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
              if (FindResource(aType, id.id, true, id.key, oid, nil, nil, request.compartments)) then
                id.outcomeVersion := FConnection.CountSQL('select Max(VersionId) from Versions where ResourceKey = '+inttostr(id.key))+1;
            end;
            if ids.ExistsByTypeAndId(id) then
              raise Exception.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [CODES_TFHIRResourceType[id.restype]+'/'+id.id])+' (entry '+inttostr(index+1)+')');

            if (id.state = tesUpdate) and (id.key <> 0) then
            begin
              if entry.transaction.ifMatch <> '' then
              begin
                if 'W/"'+inttostr(id.outcomeVersion)+'"' <> entry.transaction.ifMatch then
                  raise Exception.Create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['W/"'+inttostr(id.outcomeVersion)+'"', entry.transaction.ifMatch])+' (entry '+inttostr(index+1)+')');
              end;
            end
            else if entry.transaction.ifMatch <> '' then
               raise Exception.Create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['(n/a)', entry.transaction.ifMatch])+' (entry '+inttostr(index+1)+')');
          end;
        HttpVerbDELETE :
          begin
            id.state := tesDelete;
            if entry.transaction.url.Contains('?') then
            begin
              s := entry.transaction.url.substring(entry.transaction.url.IndexOf('?')+1);
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
              if (FindResource(aType, id.id, true, id.key, oid, nil, nil, request.compartments)) then
                id.outcomeVersion := FConnection.CountSQL('select Max(VersionId) from Versions where ResourceKey = '+inttostr(id.key)) + 1;
            end;
            if (id.state = tesDelete) and ids.ExistsByTypeAndId(id) then
                  raise Exception.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [CODES_TFHIRResourceType[id.restype]+'/'+id.id])+' (entry '+inttostr(index+1)+')');

            // version check
            if (id.state = tesDelete) and (id.key <> 0) then
            begin
              if entry.transaction.ifMatch <> '' then
              begin
                if 'W/"'+inttostr(id.outcomeVersion)+'"' <> entry.transaction.ifMatch then
                 raise Exception.Create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['W/"'+inttostr(id.outcomeVersion)+'"', entry.transaction.ifMatch])+' (entry '+inttostr(index+1)+')');
              end;
            end
            else if entry.transaction.ifMatch <> '' then
              raise Exception.Create(StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), ['(n/a)', entry.transaction.ifMatch])+' (entry '+inttostr(index+1)+')')
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
          if (FindResource(aType, id.id, true, id.key, oid, nil, nil, request.compartments)) then
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


procedure TFhirOperationManager.adjustReferences(te : TFHIRTransactionEntry; base : String; entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList);
var
  refs : TFhirReferenceList;
  ref : TFhirReference;
  url : String;
  vhist : String;
  i, j, k : integer;
  attachments : TFhirAttachmentList;
  attachment : TFhirAttachment;
  extension : TFhirExtension;
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
    for attachment in TFhirDocumentReference(entry.resource).contentList do
    begin
      j := ids.IndexByName(attachment.url);
      if (j > -1) then
        attachment.url := base+CODES_TFHIRResourceType[ids[j].resType]+'/'+ids[j].id
      else if isLogicalReference(Attachment.url) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [attachment.url]));
    end;

  if TFhirDomainResource(entry.resource).text <> nil then
    FixXhtmlUrls(lang, base, ids, TFhirDomainResource(entry.resource).text.div_);
end;

function TFhirOperationManager.commitResource(context: TFHIRValidatorContext; request: TFHIRRequest; response : TFHIRResponse; upload : boolean; entry: TFHIRAtomEntry; i : integer; id: TFHIRTransactionEntry; session : TFHIRSession; resp : TFHIRAtomFeed) : Boolean;
var
  ne : TFhirBundleEntry;
begin
  if (entry.transaction <> nil) then
    writelnt(inttostr(i)+': '+entry.transaction.methodElement.value+' '+id.summary)
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
    tesRead: executeReadInTransaction(entry.transaction, request, response);
    tesCreate: ExecuteCreate(context, upload, request, response, idIsNew, id.key);
    tesUpdate:
      begin
//        if (entry.transaction <> nil) and (entry.transaction.url.contains('?')) then
//        begin
//          if Id.count <> ResolveSearchIdCount(id.resType, request.compartmentId, request.compartments, request.baseUrl, entry.transaction.url.substring(entry.transaction.url.IndexOf('?')+1)) then
//            raise Exception.Create('error processing batch - id clash: one of the create statements altered the processing of a conditional update: '+entry.transaction.url);
//        end;
        ExecuteUpdate(context, upload, request, response);
      end;
    tesDelete:
      begin
//        if (entry.transaction <> nil) and (entry.transaction.url.contains('?')) then
//        begin
//          if Id.count <> ResolveSearchIdCount(id.resType, request.compartmentId, request.compartments, request.baseUrl, entry.transaction.url.substring(entry.transaction.url.IndexOf('?')+1)) then
//            raise Exception.Create('error processing batch - id clash: one of the create or update statements altered the processing of a conditional delete: '+entry.transaction.url);
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
  ne.transactionResponse := TFhirBundleEntryTransactionResponse.Create;
  ne.transactionResponse.status := inttostr(response.HTTPCode);
  ne.transactionResponse.location := response.Location;
  ne.transactionResponse.etag := 'W/'+response.versionId;
  ne.transactionResponse.lastModified := TDateAndTime.CreateUTC(response.lastModifiedDate);
end;

procedure ignoreEntry(req, resp : TFHIRAtomEntry);
begin
  resp.transactionResponse := TFhirBundleEntryTransactionResponse.Create;
  resp.transactionResponse.status := '200';
  if (req.transaction <> nil) and (req.transaction.method = HttpVerbDELETE) then
    resp.transactionResponse.status := '404';
end;



procedure TFhirOperationManager.ExecuteTransaction(upload : boolean; request: TFHIRRequest; response: TFHIRResponse);
var
//  match : boolean;
  i : integer;
  resp : TFHIRAtomFeed;
  ids : TFHIRTransactionEntryList;
  id : TFHIRTransactionEntry;
  ok : boolean;
  context: TFHIRValidatorContext;
  bundle : TFHIRBundle;
  base : String;
  ne : TFHIRAtomEntry;
begin
  try
    ok := true;
    if not check(response, FRepository.SupportTransaction, 405, lang, 'Transaction Operations not allowed') then
      ok := false;
    if ok and not check(response, request.Resource is TFhirBundle, 400, lang, 'A bundle is required for a Transaction operation') then
      ok := false;

    if ok then
    begin
      request.Source := nil; // ignore that now
      resp := TFHIRAtomFeed.create(BundleTypeTransactionResponse);
      ids := TFHIRTransactionEntryList.create;
      base := request.feed.base;
      if (base = '') then
        base := request.baseUrl;


      context := FRepository.Validator.AcquireContext;
      try
        ids.FDropDuplicates := request.Feed.Tags['duplicates'] = 'ignore';
        resp.base := request.baseUrl;
        ids.SortedByName;
        resp.id := FhirGUIDToString(CreateGuid);


        // first pass: scan ids
        for i := 0 to request.feed.entryList.count - 1 do
        begin
          request.feed.entryList[i].Tag := scanId(base, request, request.feed.entryList[i], ids, i).Link;
        end;

        // third pass: reassign references
        for i := 0 to request.feed.entryList.count - 1 do
          if not (request.feed.entryList[i].Tag as TFHIRTransactionEntry).ignore and not (request.feed.entryList[i].Tag as TFHIRTransactionEntry).deleted then
            adjustReferences(request.feed.entryList[i].Tag as TFHIRTransactionEntry, base, request.feed.entryList[i], ids);

        // four pass: commit resources
        bundle := request.feed.Link;
        try
          for i := 0 to bundle.entryList.count - 1 do
          begin
             ne := resp.entryList.Append;
//             ne.transaction := bundle.entryList[i].transaction.Link;
            (bundle.entryList[i].Tag as TFHIRTransactionEntry).entry := ne;
          end;

          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesCreate then
              ignoreEntry(bundle.entryList[i], resp.entryList[i]);

          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesCreate then
              if not commitResource(context, request, response, upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesUpdate then
              if not commitResource(context, request, response, upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesDelete then
              if not commitResource(context, request, response, upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;
          for i := 0 to bundle.entryList.count - 1 do
            if (bundle.entryList[i].Tag as TFHIRTransactionEntry).state = tesRead then
              if not commitResource(context, request, response, upload, bundle.entryList[i], i, bundle.entryList[i].Tag as TFHIRTransactionEntry, request.Session, resp) then
                Abort;

        finally
          bundle.free;
        end;
        response.HTTPCode := 202;
        response.Message := 'Accepted';
        response.feed := resp.Link;
        response.ContentLocation := '';
      finally
        FRepository.Validator.YieldContext(context);
        ids.free;
        resp.free;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperationManager.ExecuteMailBox(request: TFHIRRequest; response: TFHIRResponse);
var
  ok : boolean;
  msg, resp : TFhirMessageHeader;
  sId : String;
begin
 // todo: security
  try
    ok := true;
    if ok and not check(response, (request.Resource <> nil) or ((request.feed <> nil) and (request.feed.entryList.count > 0)), 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) then
      ok := false;

    if ok then
    begin
      if request.Feed <> nil then
      begin
        if request.Feed.type_ = BundleTypeMessage  then
        begin
          if (request.feed.entryList.Count = 0) or not (request.feed.entryList[0].resource is TFhirMessageHeader) then
            raise Exception.Create('Invalid Message - first resource must be message header');
          response.Feed := TFHIRAtomFeed.create(BundleTypeMessage);
          response.feed.base := request.baseUrl;
          response.HTTPCode := 200;
          msg := TFhirMessageHeader(request.feed.entryList[0].resource);
          response.Feed.id := FhirGUIDToString(CreateGUID);

          // todo: check message and envelope ids
          resp := BuildResponseMessage(request, msg);
          response.Feed.entryList.Append.resource := resp;

          ProcessMessage(request, response, msg, resp, response.Feed);
        end
        else if request.Feed.type_ = BundleTypeDocument  then
        begin
          // Connectathon 5:
          // The Document Server creates and registers the document as a Binary resource
          sId := CreateDocumentAsBinary(request);
          // The Document Server creates and registers a DocumentReference instance using information from the Composition resource
          // in the bundle and pointing to the Binary resource created in the preceding step
          CreateDocumentReference(request, sId);
        end
        else
          Raise Exception.create('Unknown content for mailbox');
      end
      else
        Raise Exception.create('Unknown content for mailbox');
      response.HTTPCode := 202;
      response.Message := 'Accepted';
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, response.httpCode, '', response.message);
    end;
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, 500, '', e.message);
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


procedure TFhirOperationManager.CollectReverseIncludes(session : TFHIRSession; includes: TReferenceList; keys: TStringList; types: String; feed : TFHIRAtomFeed; request : TFHIRRequest; wantsummary : TFHIRSearchSummary);
var
  entry : TFHIRAtomEntry;
  s : string;
  rt : TStringList;
  a : TFhirResourceType;
begin
  rt := TStringList.create;
  try
    if types = '*' then
      for a in ALL_RESOURCE_TYPES do
        if Session.canRead(a) then
          rt.add(inttostr(FRepository.ResourceTypeKeyForName(CODES_TFHIRResourceType[a])))
    else
      while (types <> '') do
      begin
        StringSplit(types, ';', s, types);
        if Session.canRead(typeForReference(s)) then
          rt.add(inttostr(FRepository.ResourceTypeKeyForName(s)));
      end;

    FConnection.sql := 'Select Ids.Id, VersionId, StatedDate, Name, originalId, TextSummary, Tags, Content, Summary from Versions, Ids, Sessions '+#13#10+ // standard search
      'where Ids.Deleted = 0 and Ids.MostRecent = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey and '+ // link tables
      'Ids.ResourceKey in (select ResourceKey from IndexEntries where target in ('+keys.CommaText+') and ResourceKey in (select ResourceKey from Ids where ResourceTypeKey in ('+rt.commaText+')))';
  finally
    rt.free;
  end;

  FConnection.Prepare;
  try
    FConnection.Execute;
    while FConnection.FetchNext do
    Begin
      entry := AddResourceToFeed(feed, '', CODES_TFHIRResourceType[request.ResourceType], request.baseUrl, FConnection.colstringByName['TextSummary'], FConnection.colstringByName['originalId'], wantsummary, true);
      if request.Parameters.VarExists('_include') then
        CollectIncludes(session, includes, entry.resource, request.Parameters.GetVar('_include'));
    end;
  finally
    FConnection.Terminate;
  end;

end;

function TFhirOperationManager.opAllowed(resource: TFHIRResourceType; command: TFHIRCommandType): Boolean;
begin
  case command of
    fcmdUnknown : result := false;
    fcmdMailbox : result := false;
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

function TFhirOperationManager.TextSummaryForResource(resource: TFhirResource): String;
begin
  if resource = nil then
    result := 'Null Resource'
  else if not (resource is TFhirDomainResource) then
    result := '(Binary)'
  else if (TFhirDomainResource(resource).text = nil) or (TFhirDomainResource(resource).text.div_ = nil) then
    result := 'Undescribed resource of type '+CODES_TFhirResourceType[resource.ResourceType]
  else
    result := FhirHtmlToText(TFhirDomainResource(resource).text.div_).Trim;
  if length(result) > 254 then
    result := copy(result, 1, 254) + '…';
end;

function TFhirOperationManager.TypeForKey(key: integer): TFHIRResourceType;
var
  a : TFHIRResourceType;
  b : boolean;
begin
  result := frtNull;
  b := false;
  for a := low(TFHIRResourceType) to high(TFHIRResourceType) do
    if  FRepository.ResConfig[a].key = key then
    begin
      result := a;
      b := true;
    end;
  if not b then
    raise exception.create('key '+inttostr(key)+' not found');
end;

procedure TFhirOperationManager.CommitTags(tags: TFHIRAtomCategoryList; key: integer);
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

procedure TFhirOperationManager.ProcessBlob(request: TFHIRRequest; response: TFHIRResponse; wantSummary : boolean);
var
  parser : TFHIRParser;
  blob : TBytes;
begin
    begin
    // raise Exception.Create('todo');

    if wantSummary then
      blob := ZDecompressBytes(FConnection.ColBlobByName['Summary'])
    else
      blob := ZDecompressBytes(FConnection.ColBlobByName['Content']);

    parser := MakeParser(lang, ffXml, blob, xppDrop);
    try
      response.Resource := parser.resource.Link;
    finally
      parser.free;
    end;
  end;
end;

//procedure TFhirOperationManager.ProcessConceptMapTranslation(request: TFHIRRequest; resource : TFHIRResource; params: TParseMap; feed: TFHIRAtomFeed; includes: TReferenceList; base, lang: String);
//var
//  src : TFhirValueset;
//  coding : TFhirCoding;
//  op : TFhirOperationOutcome;
//  used : string;
//  dest : String;
//  cacheId : String;
//begin
//  used := '_query=translate';
//
//  src := IdentifyValueset(request, nil, params, base, used, cacheId, true);
//  try
//    coding := nil;
//    try
//      if params.VarExists('coding') then
//        coding := TFHIRJsonParser.parseFragment(params.GetVar('coding'), 'Coding', lang) as TFhirCoding
//      else
//      begin
//        coding := TFhirCoding.Create;
//        coding.system := params.GetVar('system');
//        coding.version := params.GetVar('version');
//        coding.code := params.GetVar('code');
//        coding.display := params.GetVar('display');
//      end;
//
//      dest := params.GetVar('dest');
//
////      feed.title := 'Concept Translation';
////      feed.link_List.Rel['self'] := used;
////      feed.SearchTotal := 1;
////      op := FRepository.TerminologyServer.translate(src, coding, dest);
////      try
////        AddResourceToFeed(feed, NewGuidURN, 'Translation Outcome', 'Translation Outcome', '', 'Health Intersections', '??base', '', now, op, '', nil, false);
////      finally
////        op.free;
////      end;
//      raise Exception.Create('todo');
//    finally
//      coding.Free;
//    end;
//  finally
//    src.free;
//  end;
//end;

procedure TFhirOperationManager.LoadTags(tags: TFHIRAtomCategoryList; ResourceKey: integer);
var
  t : TFhirTag;
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

    if not tags.HasTag(t.kind, t.Uri, t.code) then
      tags.AddValue(t.kind, t.Uri, t.code, lbl).Key := t.Key
    else
    begin
      t := tags.GetTag(t.kind, t.Uri, t.code);
      if t.display = '' then
        t.display := lbl;
    end;
  end;
  FConnection.Terminate;
end;

function TFhirOperationManager.LookupReference(context : TFHIRRequest; id: String): TResourceWithReference;
var
  a, atype : TFhirResourceType;
  resourceKey : integer;
  originalId : String;
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
  if (aType <> frtNull) and (length(id) <= ID_LENGTH) and FindResource(aType, id, false, resourceKey, originalId, nil, nil, '') then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := ZDecompressBytes(FConnection.ColBlobByName['Content']);
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
//  tags : TFHIRAtomCategoryList;
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
//      tags := TFHIRAtomCategoryList.create;
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
//          blob := ZDecompressBytes(FConnection.ColBlobByName['Content']);
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
//      raise;
//    end;
//  end;
//end;


procedure TFhirOperationManager.ExecuteGenerateQA(request: TFHIRRequest; response: TFHIRResponse);
var
  profile : TFHirStructureDefinition;
  source : TFhirResource;
  resourceKey : integer;
  originalId : String;
  id, fid : String;
  builder : TQuestionnaireBuilder;
  questionnaire : TFHIRQuestionnaire;
begin
  try
    NotFound(request, response);
    if check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, request, response, request.compartments) then
      begin
        source := GetResourceByKey(resourceKey);
        try
          // for now, we simply get the base profile
          if source is TFHirStructureDefinition then
            raise Exception.Create('Editing a profile via a profile questionnaire is not supported');

          profile := GetProfileByURL('http://hl7.org/fhir/Profile/'+Codes_TFHIRResourceType[source.ResourceType], id);
          try
            fid := request.baseUrl+'Profile/'+id+'/$questionnaire';
            questionnaire := FRepository.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
            try
              builder := TQuestionnaireBuilder.Create;
              try
                builder.Profiles := FRepository.Profiles.link;
                builder.OnExpand := FRepository.ExpandVS;
                builder.OnLookupCode := FRepository.LookupCode;
                builder.onLookupReference := LookupReference;
                builder.Context := request.Link;
                builder.QuestionnaireId := fid;

                builder.Profile := profile.link;
                builder.Resource := source.Link as TFhirDomainResource;
                if questionnaire <> nil then
                  builder.PrebuiltQuestionnaire := questionnaire.Link;
                builder.build;
                if questionnaire = nil then
                  FRepository.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, builder.Questionnaire, builder.Dependencies);
                response.HTTPCode := 200;
                response.Message := 'OK';
                response.Body := '';
                response.LastModifiedDate := now;
                response.ContentLocation := ''; // does not exist as returned
                response.link_List.AddRelRef('edit-post', request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/$qa-post');
                response.Resource := builder.Answers.Link;
              finally
                builder.Free;
              end;
            finally
              questionnaire.free;
            end;
          finally
            profile.free;
          end;
        finally
          source.Free;
        end;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      raise;
    end;
  end;
end;



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
//      raise;
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
//  tags : TFHIRAtomCategoryList;
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
//      tags := TFHIRAtomCategoryList.create;
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
//          blob := ZDecompressBytes(FConnection.ColBlobByName['Content']);
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
//      raise;
//    end;
//  end;
//end;


function TFhirOperationManager.FindResourceVersion(aType: TFHIRResourceType; sId, sVersionId: String; bAllowDeleted: boolean; var resourceVersionKey: integer; request: TFHIRRequest; response: TFHIRResponse): boolean;
begin
  FConnection.sql := 'select ResourceVersionKey from Ids, Types, Versions '+
    'where Ids.Id = :id and Ids.ResourceTypeKey = Types.ResourceTypeKey '+
    ' and Supported = 1 and Types.ResourceName = :n and Ids.ResourceKey = Versions.ResourceKey and Versions.Deleted = 0 and Versions.VersionId = :vid';
  FConnection.Prepare;
  FConnection.BindString('id', sId);
  FConnection.BindString('vid', sVersionId);
  FConnection.BindString('n', CODES_TFHIRResourceType[aType]);
  FConnection.execute;
  result := FConnection.FetchNext;
  if result then
    resourceVersionKey := FConnection.ColIntegerByName['ResourceVersionKey']
  else
    check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.subid]));
  FConnection.terminate;
end;

function TFhirOperationManager.IdentifyValueset(request: TFHIRRequest; resource : TFHIRResource; params: TParseMap; base : String; var used, cacheId : string; allowNull : boolean = false) : TFHIRValueSet;
begin
  cacheId := '';
  if (resource <> nil) and (resource is TFHIRValueSet) then
    result := resource.Link as TFhirValueSet
  else if params.VarExists('_id') then
  begin
    result := GetValueSetById(request, params.getvar('_id'), base);
    cacheId := result.url;
    used := used+'&_id='+params.getvar('_id')
  end
  else if params.VarExists('id') then
  begin
    result := GetValueSetById(request, params.getvar('id'), base);
    cacheId := result.url;
    used := used+'&id='+params.getvar('id')
  end
  else if params.VarExists('identifier') then
  begin
    if not FRepository.TerminologyServer.isKnownValueSet(params.getvar('identifier'), result) then
      result := GetValueSetByIdentity(params.getvar('identifier'), params.getvar('version'));
    cacheId := result.url;
    used := used+'&identifier='+params.getvar('identifier')
  end
  else
    result := constructValueSet(params, used, allowNull);
  if params.varExists('nocache') then
    cacheId := '';
end;


var
  icount : integer;

procedure TFhirOperationManager.ExecuteQuestionnaireGeneration(request: TFHIRRequest; response : TFHIRResponse);
var
  profile : TFHirStructureDefinition;
  op : TFhirOperationOutcome;
  resourceKey : integer;
  originalId, id, fid : String;
  builder : TQuestionnaireBuilder;
  questionnaire : TFHIRQuestionnaire;
begin
  try
    NotFound(request, response);
    if check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and FindResource(frtStructureDefinition, request.Id, false, resourceKey, originalId, request, response, request.compartments)) then
      begin
        profile := nil;
        try
          // first, we have to identify the value set.
          id := request.Id;
          if request.Id <> '' then // and it must exist, because of the check above
            profile := GetProfileById(request, request.Id, request.baseUrl)
          else if request.Parameters.VarExists('identifier') then
            profile := GetProfileByURL(request.Parameters.getvar('identifier'), id)
          else if (request.form <> nil) and request.form.hasParam('profile') then
            profile := LoadFromFormParam(request.form.getparam('profile'), request.Lang) as TFHirStructureDefinition
          else if (request.Resource <> nil) and (request.Resource is TFHirStructureDefinition) then
            profile := request.Resource.Link as TFHirStructureDefinition
          else
            raise Exception.Create('Unable to find profile to convert (not provided by id, identifier, or directly');

          if id <> '' then
          begin
            fid := request.baseUrl+'Profile/'+id+'/$questionnaire';
            questionnaire := FRepository.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
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
                builder.OnExpand := FRepository.ExpandVS;
                builder.onLookupCode := FRepository.LookupCode;
                builder.Context := request.Link;
                builder.onLookupReference := LookupReference;
                builder.QuestionnaireId := fid;
                builder.build;
                questionnaire := builder.questionnaire.Link;
                if id <> '' then
                  FRepository.QuestionnaireCache.putQuestionnaire(frtStructureDefinition, id, questionnaire, builder.dependencies);
              finally
                builder.Free;
              end;
            end;
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.ContentLocation := ''; // does not exist as returned
            response.Resource := questionnaire.Link;
          finally
            questionnaire.Free;
          end;
        finally
          profile.free;
        end;
        op := FRepository.validator.validateInstance(nil, response.Resource, 'Produce Questionnaire', nil);
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
    TFHIRXhtmlComposer.Create('en').Compose(TFileStream.Create('c:\temp\q'+inttostr(iCount)+'.xml', fmCreate), response.Resource, true, nil);
    AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      raise;
    end;
  end;
end;


//// need to set feed title, self link, search total. may add includes
//procedure TFhirOperationManager.ProcessValueSetValidation(request: TFHIRRequest; resource : TFHIRResource; params: TParseMap; feed: TFHIRAtomFeed; includes: TReferenceList; base, lang : String);
//var
//  src : TFhirValueset;
//  coding : TFhirCoding;
//  coded : TFhirCodeableConcept;
//  op : TFhirOperationOutcome;
//  used, cacheId : string;
//begin
//  used := '_query=validate';
//
//  src := IdentifyValueset(request, resource, params, base, used, cacheId);
//  try
//    coding := nil;
//    coded := nil;
//    try
//      if params.VarExists('coding') then
//        coding := TFHIRJsonParser.parseFragment(params.GetVar('coding'), 'Coding', lang) as TFhirCoding
//      else if params.VarExists('codeableconcept') then
//        coded := TFHIRJsonParser.parseFragment(params.GetVar('codeableconcept'), 'CodeableConcept', lang) as TFhirCodeableConcept
//      else
//      begin
//        coding := TFhirCoding.Create;
//        coding.system := params.GetVar('system');
//        coding.version := params.GetVar('version');
//        coding.code := params.GetVar('code');
//        coding.display := params.GetVar('display');
//      end;
//
//
////      feed.title := 'ValueSet Validation';
////      feed.link_List.Rel['self'] := used;
////      feed.SearchTotal := 1;
////      if coding = nil then
////        op := FRepository.TerminologyServer.validate(src, coded)
////      else
////        op := FRepository.TerminologyServer.validate(src, coding);
////      try
////        AddResourceToFeed(feed, NewGuidURN, 'Validation Outcome', 'Validation Outcome', '', 'Health Intersections', '??base', '', now, op, '', nil, false);
////      finally
////        op.free;
////      end;
//  raise Exception.Create('todo');
//
//    finally
//      coding.Free;
//      coded.Free;
//    end;
//  finally
//    src.free;
//  end;
//end;
//
function TFhirOperationManager.GetValueSetById(request: TFHIRRequest; id, base: String): TFHIRValueSet;
var
  resourceKey : integer;
  originalId : String;
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

  if (length(request.id) <= ID_LENGTH) and FindResource(frtValueSet, id, false, resourceKey, originalId, request, nil, '') then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := ZDecompressBytes(FConnection.ColBlobByName['Content']);
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
  if (id.startsWith('ValueSet/')) then
    id := id.substring(9);

  if version <> '' then
    FConnection.SQL := 'Select * from Versions where '+
      'ResourceKey in (select ResourceKey from IndexEntries where IndexKey in (Select IndexKey from Indexes where name = ''identifier'') and Value = :id) '+
      'and ResourceKey in (select ResourceKey from IndexEntries where IndexKey in (Select IndexKey from Indexes where name = ''version'') and Value = :ver) '+
      'and ResourceVersionKey in (select MostRecent from Ids) '+
      'order by ResourceVersionKey desc'
  else
    FConnection.SQL := 'Select * from Versions where '+
      'ResourceKey in (select ResourceKey from IndexEntries where IndexKey in (Select IndexKey from Indexes where name = ''identifier'') and Value = :id) '+
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
    s := ZDecompressBytes(FConnection.ColBlobByName['Content']);
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
  FConnection.SQL := 'Select Id, Content from Ids, Versions where Ids.Resourcekey = Versions.ResourceKey and Versions.ResourceKey in (select ResourceKey from '+
    'IndexEntries where IndexKey in (Select IndexKey from Indexes where name = ''url'') and Value = :id) order by ResourceVersionKey desc';
  FConnection.Prepare;
  try
    FConnection.BindString('id', url);
    FConnection.Execute;
    if not FConnection.FetchNext then
      raise Exception.create('Unknown Profile '+url);
    id := FConnection.ColStringByName['Id'];
    s := ZDecompressBytes(FConnection.ColBlobByName['Content']);
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
  originalId : String;
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

  if (length(request.id) <= ID_LENGTH) and FindResource(aType, id, false, resourceKey, originalId, request, nil, '') then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := ZDecompressBytes(FConnection.ColBlobByName['Content']);
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
      s := ZDecompressBytes(FConnection.ColBlobByName['Content']);
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
  id, oid : String;
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

  if FindResource(rtype, id, false, key, oid, nil, nil, compartments) then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(key)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := ZDecompressBytes(FConnection.ColBlobByName['Content']);
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



function ReadOperator(s : String) : TFhirFilterOperator;
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

function TFhirOperationManager.constructValueSet(params: TParseMap; var used: String; allowNull : Boolean): TFhirValueset;
var
  empty : boolean;
  function UseParam(name : String; var value : String) : boolean; overload;
  begin
    result := params.VarExists(name);
    value := params.GetVar(name);
    used := used + '&'+name+'='+EncodeMime(value);
    empty := value <> '';
  end;
  function UseParam(name : String): String; overload;
  begin
    result := params.GetVar(name);
    used := used + '&'+name+'='+EncodeMime(result);
    empty := result <> '';
  end;
var
  s, l : String;
  inc : TFhirValueSetComposeInclude;
  filter : TFhirValueSetComposeIncludeFilter;
begin
  empty := true;

  result := TFhirValueSet.create;
  try
    result.Name := useParam('name');
    result.url := useParam('vs-identifier');
    if result.url = '' then
      result.url := NewGuidURN;
    result.version := useParam('vs-version');
    result.compose := TFhirValueSetCompose.create;
    if useParam('import', s) then
      result.compose.importList.Append.value := s;
    if UseParam('system', s) then
    begin
      inc := result.compose.includeList.append;
      if (s = 'snomed') then
        inc.system := 'http://snomed.info/sct'
      else if (s = 'loinc') then
        inc.system := 'http://loinc.org'
      else
        inc.system := s;
      if UseParam('code', s) then
      begin
        while (s <> '') do
        begin
          StringSplit(s, ',', l, s);
          inc.conceptList.Append.code := l;
        end;
      end;
      s := useParam('property');
      l := useParam('value');
      if (s <> '') or (l <> '') then
      begin
        filter := inc.filterList.Append;
        filter.property_ := s;
        if filter.property_ = '' then
          filter.property_ := 'concept';
        filter.value := l;
        filter.op := ReadOperator(UseParam('op'));
      end;
    end;
    if not empty then
      result.link
    else if not allowNull then
      raise Exception.Create('Not value set details found');
  finally
    result.free;
  end;
  if empty then
    result := nil;
end;

procedure TFhirOperationManager.AuditRest(session: TFhirSession; ip: string; resourceType: TFhirResourceType; id, ver: String; op: TFHIRCommandType; provenance : TFhirProvenance; httpCode: Integer; name, message: String);
begin
  AuditRest(session, ip, resourceType, id, ver, op, provenance, '', httpCode, name, message);
end;

procedure TFhirOperationManager.AuditRest(session: TFhirSession; ip: string; resourceType: TFhirResourceType; id, ver: String; op: TFHIRCommandType; provenance : TFhirProvenance; opName : String; httpCode: Integer; name, message: String);
var
  se : TFhirAuditEvent;
  c : TFhirCoding;
  p : TFhirAuditEventParticipant;
  o : TFhirAuditEventObject;
  procedure event(t, ts, td, s, sc : String; a : TFhirAuditEventAction);
  begin
    se.event.type_ := TFhirCodeableConcept.create;
    c := se.event.type_.codingList.Append;
    c.code := t;
    c.system := ts;
    c.display := td;
    c := se.event.subtypeList.append.codingList.Append;
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
    se.event := TFhirAuditEventEvent.create;
    case op of
      fcmdMailbox :        event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'mailbox', 'http://hl7.org/fhir/restful-operation', AuditEventActionE);
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
      se.event.subtypeList.Append.text := opName;
    if httpCode < 300 then
      se.event.outcome := AuditEventOutcome0
    else if httpCode < 500 then
      se.event.outcome := AuditEventOutcome4
    else
      se.event.outcome := AuditEventOutcome8; // no way we are going down...
    se.event.dateTime := NowUTC;
    se.Tag := se.event.dateTime.Link;

    se.source := TFhirAuditEventSource.create;
    se.source.site := 'Cloud';
    se.source.identifier := FOwnerName;
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
      p.userId := inttostr(session.Key);
      p.altId := session.Id;
      p.name := session.Name;
    end;
    p.requestor := true;
    p.network := TFhirAuditEventParticipantNetwork.create;
    p.network.identifier := ip;
    p.network.type_ := NetworkType2;

    if resourceType <> frtNull then
    begin
      o := se.object_List.Append;
      o.reference := TFhirReference.create;
      if ver <> '' then
        o.reference.reference := CODES_TFHIRResourceType[resourceType]+'/'+id+'/_history/'+ver
      else if id <> '' then
        o.reference.reference := CODES_TFHIRResourceType[resourceType]+'/'+id;
      o.type_ := ObjectType2;
      case op of
        fcmdMailbox :        o.lifecycle := ObjectLifecycle6;
        fcmdRead:            o.lifecycle := ObjectLifecycle6;
        fcmdVersionRead:     o.lifecycle := ObjectLifecycle6;
        fcmdUpdate:          o.lifecycle := ObjectLifecycle3;
        fcmdDelete:          o.lifecycle := ObjectLifecycle14;
        fcmdHistoryInstance: o.lifecycle := ObjectLifecycle9;
        fcmdCreate:          o.lifecycle := ObjectLifecycle1;
        fcmdSearch:          o.lifecycle := ObjectLifecycle6;
        fcmdHistoryType:     o.lifecycle := ObjectLifecycle9;
        fcmdValidate:        o.lifecycle := ObjectLifecycle4;
        fcmdConformanceStmt: o.lifecycle := ObjectLifecycle6;
        fcmdTransaction:     o.lifecycle := ObjectLifecycle3;
        fcmdHistorySystem:   o.lifecycle := ObjectLifecycle9;
        fcmdUpload:          o.lifecycle := ObjectLifecycle9;
        fcmdGetMeta:         o.lifecycle := ObjectLifecycle6;
        fcmdUpdateMeta:      o.lifecycle := ObjectLifecycle3;
        fcmdDeleteMeta:      o.lifecycle := ObjectLifecycle14;
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

procedure TFhirOperationManager.storeResources(list : TFHIRResourceList; upload : boolean);
var
  i : integer;
  request: TFHIRRequest;
  response : TFHIRResponse;
begin
  Connection.StartTransact;
  try
    // cut us off from the external request
    request := TFHIRRequest.create;
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
  m : TFHIRIndexManager;
  k : integer;
  tags : TFHIRAtomCategoryList;
begin
  Connection.ExecSQL('delete from SearchEntries');
  Connection.ExecSQL('delete from Searches');
  Connection.ExecSQL('delete from IndexEntries');

  k := Connection.CountSQL('select Max(IndexKey) from Indexes');
  m := TFHIRIndexManager.create(nil);
  try
    m.TerminologyServer := FRepository.TerminologyServer.Link;
    m.KeyEvent := FRepository.GetNextKey;
    for i := 0 to m.Indexes.count - 1 do
    begin
      if Connection.CountSQL('select Count(IndexKey) from Indexes where Name = '''+ m.indexes[i].Name+'''') = 0 then
      begin
        Connection.Sql := 'insert into Indexes (IndexKey, Name) values (:k, :d)';
        Connection.prepare;
        Connection.bindInteger('k', k);
        Connection.bindString('d', m.indexes[i].Name);
        Connection.execute;
        inc(k);
        Connection.terminate;
      end;
    end;
  finally
    m.free;
  end;

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
      FConnection.sql := 'select Tags, Content from Versions where Deleted != 1 and resourceVersionkey in (Select MostRecent from  Ids where ResourceKey = '+inttostr(Integer(list.objects[i]))+')';
      FConnection.prepare;
      FConnection.execute;
      if FConnection.FetchNext then
      begin
        tags := TFHIRAtomCategoryList.create;
        try
          parser := MakeParser('en', ffXml, Connection.ColMemoryByName['Content'], xppDrop);
          try
            r := parser.resource;
            FConnection.terminate;
            Connection.StartTransact;
            try
              FIndexer.execute(Integer(list.objects[i]), list[i], r, tags);
              Connection.Commit;
            except
              Connection.Rollback;
              raise;
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
  originalId : String;
  parser : TFHIRParser;
  b : String;
  blob : TBytes;
begin
  if id.StartsWith(base) then
    id := id.Substring(base.Length)
  else for b in FRepository.Bases do
    if id.StartsWith(b) then
      id := id.Substring(b.Length);
  if id.startsWith('Profile/') then
    id := id.Substring(8);

  if id <> '' then
  begin
    if (length(request.id) <= ID_LENGTH) and FindResource(frtStructureDefinition, id, false, resourceKey, originalId, request, nil, '') then
    begin
      FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
      FConnection.Prepare;
      try
        FConnection.Execute;
        FConnection.FetchNext;
        blob := TryZDecompressBytes(FConnection.ColBlobByName['Content']);
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
    if (r <> nil) and ((r.text = nil) or (r.text.div_ = nil)) and (FRepository.Profiles <> nil) then
    begin
      profile := FRepository.Profiles.ProfileByType[r.ResourceType].Link;
      try
        if profile = nil then
          r.text := nil
        else
        begin
          gen := TNarrativeGenerator.Create('', FRepository.Profiles.Link, FRepository.LookupCode, LookupReference, request.Link);
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

procedure TFhirOperationManager.ProcessMsgQuery(request: TFHIRRequest; response: TFHIRResponse; feed : TFHIRAtomFeed);
begin
  raise exception.create('query-response is not yet supported');
end;

function TFhirOperationManager.BuildResponseMessage(request: TFHIRRequest; incoming: TFhirMessageHeader): TFhirMessageHeader;
var
  dst : TFhirMessageHeaderDestination;
begin
  result := TFhirMessageHeader.create;
  try
    result.identifier := GUIDToString(CreateGUID);
    result.timestamp := NowUTC;
    result.event := incoming.event.Clone;
    result.response := TFhirMessageHeaderResponse.create;
    result.response.identifier := GUIDToString(CreateGUID);;
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

procedure TFhirOperationManager.ProcessMessage(request: TFHIRRequest; response : TFHIRResponse; msg, resp: TFhirMessageHeader; feed: TFHIRAtomFeed);
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
      ProcessMsgQuery(request, response, feed)
    else if s = 'query-response' then
      raise exception.create('query-response is not yet supported')
//    else if s = 'make-claim' then
//      ProcessMsgClaim(request, msg, resp, request.feed, feed)
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
procedure TFhirOperationManager.ProcessMsgClaim(request : TFHIRRequest; incoming, outgoing : TFhirMessageHeader; infeed, outfeed: TFHIRAtomFeed);
var
  id : string;
  claim : TFhirClaim;
  rem : TFhirRemittance;
  i : integer;
  svc : TFhirRemittanceService;
  entry : TFHIRAtomEntry;
  utc : TDateAndTime;
  context : TFHIRValidatorContext;
begin
  context := FRepository.Validator.AcquireContext;
  try
    claim := GetResourceFromFeed(infeed, incoming.dataList[0]) as TFhirClaim;
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
        entry := outfeed.addEntry('remittance', utc, id, id, rem);
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


function TFhirOperationManager.AddDeletedResourceToFeed(feed: TFHIRAtomFeed; sId, sType, base, originalId: String): TFHIRAtomEntry;
var
  entry : TFHIRAtomEntry;
begin
//  entry := TFHIRAtomEntry.Create;
//  try
//    entry.title := GetFhirMessage('NAME_RESOURCE', lang)+' '+sId+' '+GetFhirMessage('NAME_VERSION', lang)+' '+FConnection.ColStringByName['VersionId']+' ('+GetFhirMessage('NAME_DELETED', lang)+')';
//    entry.deleted := true;
//    entry.link_List['self'] := base+lowercase(sType)+'/'+sId+'/_history/'+FConnection.ColStringByName['VersionId'];
//    entry.updated := TDateAndTime.CreateUTC(TSToDateTime(FConnection.ColTimeStampByName['StatedDate']));
//    entry.resource.id := base+sId;
//    entry.published_ := NowUTC;
//    entry.authorName := FConnection.ColStringByName['Name'];
//    entry.originalId := originalId;
//    entry.categories.decodeJson(FConnection.ColBlobByName['Tags']);
//    feed.entryList.add(entry.Link);
//    result := entry;
//  finally
//    entry.Free;
//  end;
  raise Exception.Create('To do');
  (*
  entry := TFHIRAtomEntry.Create;
  try
    entry.deleted := TFhirBundleEntryDeleted.Create;
    entry.deleted.type_ := sType;
    entry.deleted.resourceId := sId;
    entry.deleted.versionId := FConnection.ColStringByName['VersionId'];
    entry.deleted.instant := TDateAndTime.CreateUTC(TSToDateTime(FConnection.ColTimeStampByName['StatedDate']));
    feed.entryList.add(entry.Link);
    result := entry;
  finally
    entry.Free;
  end;
  *)

end;

procedure TFhirOperationManager.checkProposedContent(request : TFHIRRequest; resource: TFhirResource; tags: TFHIRAtomCategoryList);
begin

  if resource is TFhirSubscription then
  begin
    if (TFhirSubscription(resource).status <> SubscriptionStatusRequested) and (request.session.name <> 'Anonymous (server)') then // nil = from the internal system, which is allowed to
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

procedure TFhirOperationManager.checkProposedDeletion(request: TFHIRRequest; resource: TFhirResource; tags: TFHIRAtomCategoryList);
begin

  if (resource is TFhirOperationDefinition) then
  begin
    if resource.id.StartsWith('fso-') then
      raise Exception.Create('operation Definitions that start with "fso-" are managed by the system directly, and cannot be changed through the REST API');
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
    result := result + '((ResourceTypeKey = (Select ResourceTypeKey from Types where ResourceName = '''+SQLWrapString(Strings[i])+''')) and (Ids.Id in ('+copy(s, 3, $FFFF)+')))';
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
    result.date := TDateAndTime.CreateHL7(FHIR_GENERATED_DATE);
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
      result := false // todo: what should it be?
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
  originalId, id, fid : String;
  builder : TQuestionnaireBuilder;
  questionnaire : TFHIRQuestionnaire;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, request.Session.canRead(request.ResourceType) and manager.opAllowed(request.ResourceType, request.CommandType), 400, request.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', request.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(frtStructureDefinition, request.Id, false, resourceKey, originalId, request, response, request.compartments)) then
      begin
        profile := nil;
        try
          // first, we have to identify the value set.
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
            raise Exception.Create('Unable to find profile to convert (not provided by id, identifier, or directly');

          if id <> '' then
          begin
            fid := request.baseUrl+'Profile/'+id+'/$questionnaire';
            questionnaire := manager.FRepository.QuestionnaireCache.getQuestionnaire(frtStructureDefinition, id);
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
                builder.Profiles := manager.FRepository.Validator.Profiles.Link;
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
            response.ContentLocation := ''; // does not exist as returned
            response.Resource := questionnaire.Link;
          finally
            questionnaire.Free;
          end;
        finally
          profile.free;
        end;
        op := manager.FRepository.validator.validateInstance(nil, response.Resource, 'Produce Questionnaire', nil);
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
    TFHIRXhtmlComposer.Create('en').Compose(TFileStream.Create('c:\temp\q'+inttostr(iCount)+'.xml', fmCreate), response.Resource, true, nil);
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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
  url, cacheId, originalId, profile, filter : String;
  _limit : integer;
  incomplete : boolean;
  params : TFhirParameters;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, request, response, request.compartments)) then
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
          else if request.Parameters.VarExists('identifier') then
          begin
            url := request.Parameters.getvar('identifier');
            if (url.startsWith('ValueSet/')) then
              vs := manager.GetValueSetById(request, url.substring(9), request.baseUrl)
            else if (url.startsWith(request.baseURL+'ValueSet/')) then
              vs := manager.GetValueSetById(request, url.substring(9), request.baseUrl)
            else if not manager.FRepository.TerminologyServer.isKnownValueSet(url, vs) then
              vs := manager.GetValueSetByIdentity(request.Parameters.getvar('identifier'), request.Parameters.getvar('version'));
            cacheId := vs.url;
          end
          else if (request.form <> nil) and request.form.hasParam('valueSet') then
            vs := LoadFromFormParam(request.form.getparam('valueSet'), request.Lang) as TFhirValueSet
          else if (request.Resource <> nil) and (request.Resource is TFHIRValueSet) then
            vs := request.Resource.Link as TFhirValueSet
          else
            raise Exception.Create('Unable to find value to expand (not provided by id, identifier, or directly');

          profile := params.str['profile'];
          filter := params.str['filter'];
          _limit := StrToIntDef(params.str['_limit'], 0);
          incomplete := StrToBoolDef(params.str['_incomplete'], false);

          dst := manager.FRepository.TerminologyServer.expandVS(vs, cacheId, profile, filter, _limit, incomplete);
          try
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.ContentLocation := ''; // does not exist as returned
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
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      raise;
    end;
  end;
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
//  url, cacheId, originalId : String;
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
          response.ContentLocation := ''; // does not exist as returned
        finally
          coding.Free;
        end;
      finally
        params.Free;
      end;
    end;
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      raise;
    end;
  end;
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
  s, opDesc : string;
  mode : TValidationOperationMode;
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

    if (request.Source <> nil) and (request.PostFormat = ffXml) and not (request.Resource is TFhirParameters) then
      outcome := manager.FRepository.validator.validateInstance(nil, request.Source, opDesc, profile)
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
        outcome := manager.FRepository.validator.validateInstance(nil, buffer, opDesc, profile);
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
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, request.Provenance, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceType <> frtAuditEvent then
        manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, request.Provenance, 500, '', e.message);
      raise;
    end;
  end;
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
  op : TFhirOperationOutcome;
  resourceKey : integer;
  cacheId, originalId : String;
  coded : TFhirCodeableConcept;
  coding : TFhirCoding;

  params : TFhirParameters;

begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, request, response, request.compartments)) then
      begin
        cacheId := '';
        vs := nil;
        try
          // first, we have to identify the value set.
          if request.Id <> '' then // and it must exist, because of the check above
          begin
            vs := manager.GetValueSetById(request, request.Id, request.baseUrl);
            cacheId := vs.url;
          end
          else if request.Parameters.VarExists('identifier') then
          begin
            if not manager.FRepository.TerminologyServer.isKnownValueSet(request.Parameters.getvar('identifier'), vs) then
              vs := manager.GetValueSetByIdentity(request.Parameters.getvar('identifier'), request.Parameters.getvar('version'));
            cacheId := vs.url;
          end
          else if (request.form <> nil) and request.form.hasParam('valueSet') then
            vs := LoadFromFormParam(request.form.getparam('valueSet'), request.Lang) as TFhirValueSet
          else if (request.Resource <> nil) and (request.Resource is TFHIRValueSet) then
            vs := request.Resource.Link as TFhirValueSet
          else
            raise Exception.Create('Unable to find value to expand (not provided by id, identifier, or directly');

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
                coding.version := TFHIRPrimitiveType(params['version']).StringValue;
                coding.code := TFHIRPrimitiveType(params['code']).StringValue;
                coding.display := TFHIRPrimitiveType(params['display']).StringValue;
              end
              else
                raise Exception.Create('Unable to find code to validate (params. coding | codeableConcept | code');

            end

            else
              raise Exception.Create('Unable to find code to validate (coding | codeableConcept | code');

            params := TFhirParameters.Create;
            try
              response.Resource := params.link;
              try
                op := manager.FRepository.TerminologyServer.validate(vs, coded);
                try
                  params.addParameter('result', not op.hasErrors);
                  if op.issueList.count > 0 then
                    params.AddParameter('message', op.issueList[0].details);
                finally
                  op.free;
                end;
              except
                on e : Exception do
                begin
                  params.parameterList.Clear;
                  params.addParameter('result', false);
                  params.AddParameter('message', e.Message);
                end;
              end;
            finally
              params.free;
            end;
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
              response.ContentLocation := ''; // does not exist as returned
          finally
            coded.Free;
          end;
        finally
          vs.free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      raise;
    end;
  end;
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
      type_ := 'Bundle';
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TFhirPatientEverythingOperation.CreateMissingList(lcode, name, plural, pid : string) : TFHIRList;
begin
  result := TFhirList.Create;
  try
    // result.id := '' no id
    result.code := TFhirCodeableConcept.Create;
    with result.code.codingList.Append do
    begin
      system := 'http://loinc.org';
      code := lcode;
    end;
    result.code.text := name+' List';
    result.subject := TFhirReference.Create;
    result.subject.reference := 'Patient/'+pid;
    result.source := TFhirReference.Create;
    result.source.display := 'Test FHIR Server';
    result.date := NowUTC;
    result.emptyReason := TFhirCodeableConcept.Create;
    result.emptyReason.text := 'The resources in the patient compartment did not include this list, so the contents of the '+name+' list are not known';
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFhirPatientEverythingOperation.Execute(manager: TFhirOperationManager; request: TFHIRRequest; response: TFHIRResponse);
var
  feed : TFHIRAtomFeed;
  entry : TFHIRAtomEntry;
  includes : TReferenceList;
  id, link, base, sql, oldid : String;
  total : Integer;
  key, rkey : integer;
//  offset, count : integer;
//  ok : boolean;
  wantsummary : TFHIRSearchSummary;
  title: string;
  keys : TStringList;
  params : TParseMap;
  patient, listProblems, listAllergies, listMedications : TFHIRResource;
  list : TFhirList;
begin
  try
    patient := nil;
    listProblems := nil;
    listAllergies := nil;
    listMedications := nil;
    try

      // first, we have toi convert from the patient id to a compartment id
      if manager.FindResource(frtPatient, request.Id, false, rkey, oldid, request, response, '') then
      begin
        request.compartmentId := request.Id;
        feed := TFHIRAtomFeed.Create(BundleTypeSearchset);
        includes := TReferenceList.create;
        keys := TStringList.Create;
        params := TParseMap.Create('');
        try
          feed.base := request.baseUrl;
          if manager.FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], request.Session, 1, id, link, sql, title, base, total, wantSummary) then
            link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
          else
            id := manager.BuildSearchResultSet(0, request.Session, request.resourceType, params, request.baseUrl, request.compartments, request.compartmentId, link, sql, total, wantSummary);
          feed.total := inttostr(total);
          feed.Tags['sql'] := sql;

          manager.FConnection.SQL := 'Select Ids.ResourceKey, Ids.Id, VersionId, StatedDate, Name, originalId, Versions.Deleted, TextSummary, Tags, Content, Summary from Versions, Ids, Sessions, SearchEntries '+
              'where Ids.Deleted = 0 and SearchEntries.ResourceVersionKey = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey and SearchEntries.ResourceKey = Ids.ResourceKey and SearchEntries.SearchKey = '+id+' '+
              'order by SortValue ASC';
          manager.FConnection.Prepare;
          try
            manager.FConnection.Execute;
            while manager.FConnection.FetchNext do
            Begin
              entry := manager.AddResourceToFeed(feed, '', CODES_TFHIRResourceType[request.ResourceType], request.baseUrl, manager.FConnection.colstringByName['TextSummary'], manager.FConnection.colstringByName['originalId'], wantsummary, true);
              keys.Add(manager.FConnection.ColStringByName['ResourceKey']);

              if request.Parameters.VarExists('_include') then
                manager.CollectIncludes(request.session, includes, entry.resource, request.Parameters.GetVar('_include'));
              if entry.resource.ResourceType = frtPatient then
              begin
                if patient = nil then
                  patient := entry.resource.link
                else
                  raise Exception.Create('Multiple patient resources found in patient compartment');
              end;
              if entry.resource.ResourceType = frtList then
              begin
                list := entry.resource.link as TFhirList;
                if list.code.hasCode('http://loinc.org', '11450-4') then
                  if listProblems = nil then listProblems := list.link else raise Exception.Create('Multiple problem lists found in patient compartment')
                else if list.code.hasCode('http://loinc.org', '48765-2') then
                  if listAllergies = nil then listAllergies := list.link else raise Exception.Create('Multiple meidication allergy lists found in patient compartment')
                else if list.code.hasCode('http://loinc.org', '10160-0') then
                  if listMedications = nil then listMedications := list.link else raise Exception.Create('Multiple meidication lists found in patient compartment')
              end;
            End;
          finally
            manager.FConnection.Terminate;
          end;

          // process reverse includes
          if request.Parameters.VarExists('_reverseInclude') then
            manager.CollectReverseIncludes(request.Session, includes, keys, request.Parameters.GetVar('_reverseInclude'), feed, request, wantsummary);

          //now, add the includes
          if includes.Count > 0 then
          begin
            manager.FConnection.SQL := 'Select ResourceTypeKey, Ids.Id, VersionId, StatedDate, Name, originalId, Versions.Deleted, TextSummary, Tags, Content from Versions, Sessions, Ids '+
                'where Ids.Deleted = 0 and Ids.MostRecent = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey '+
                'and Ids.ResourceKey in (select ResourceKey from IndexEntries where '+includes.asSql+') order by ResourceVersionKey DESC';
            manager.FConnection.Prepare;
            try
              manager.FConnection.Execute;
              while manager.FConnection.FetchNext do
                manager.AddResourceToFeed(feed, '', CODES_TFHIRResourceType[manager.TypeForKey(manager.FConnection.ColIntegerByName['ResourceTypeKey'])], request.baseUrl, manager.FConnection.ColStringByName['TextSummary'], manager.FConnection.ColStringByName['originalId'], wantSummary, true);
            finally
              manager.FConnection.Terminate;
            end;
          end;

          if patient = nil then
            raise Exception.Create('Patient resources not found in patient compartment');
          feed.deleteEntry(patient);
          feed.entryList.Insert(0).resource := patient.Link;
          if listProblems <> nil then
          begin
            feed.deleteEntry(listProblems);
            feed.entryList.Insert(1).resource := listProblems.Link;
          end
          else
            feed.entryList.Insert(1).resource := CreateMissingList('11450-4', 'Problem', 'Problems', request.compartmentId);
          if listAllergies <> nil then
          begin
            feed.deleteEntry(listAllergies);
            feed.entryList.Insert(1).resource := listAllergies.Link;
          end
          else
            feed.entryList.Insert(1).resource := CreateMissingList('48765-2', 'Medication Allergy', 'Medication Allergies', request.compartmentId);
          if listMedications <> nil then
          begin
            feed.deleteEntry(listMedications);
            feed.entryList.Insert(1).resource := listMedications.Link;
          end
          else
            feed.entryList.Insert(1).resource := CreateMissingList('10160-0', 'Allergy', 'Allergies', request.compartmentId);

          feed.meta := TFhirMeta.Create;
          feed.meta.lastUpdated := NowUTC;
          feed.id := NewGuidURN;
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.Body := '';
          response.Feed := feed.Link;
        finally
          params.free;
          includes.free;
          keys.Free;
          feed.Free;
        end;
    end;
    finally
      patient.free;
      listProblems.free;
      listAllergies.free;
      listMedications.free;
    end;
    manager.AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, response.httpCode, request.Parameters.Source, response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, request.Provenance, 500, request.Parameters.Source, e.message);
      raise;
    end;
  end;
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
  i : integer;
begin
  for i := 0 to sections.Count - 1 do
  begin
    addResource(manager, bundle, sections[i].content, true, compartments);
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
      type_ := 'Bundle';
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
  originalId : String;
  i, j : integer;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if manager.FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, request, response, request.compartments) then
      begin
        composition := manager.GetResourceByKey(resourceKey) as TFhirComposition;
        try
          bundle := TFhirBundle.Create(BundleTypeDocument);
          try
            bundle.id := copy(GUIDToString(CreateGUID), 2, 36).ToLower;
            bundle.meta := TFhirMeta.Create;
            bundle.meta.lastUpdated := NowUTC;
            bundle.base := manager.FRepository.FormalURLPlain;
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
            response.ContentLocation := ''; // does not exist as returned
            response.Resource := bundle.Link;
          finally
            bundle.Free;
          end;
        finally
          composition.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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
  originalId : String;
  claim : TFhirClaim;
  resp : TFhirClaimResponse;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceType, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, request, response, request.compartments)) then
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
            raise Exception.Create('Unable to find claim to expand (not provided by id, identifier, or directly');

          resp := manager.FRepository.GenerateClaimResponse(claim);
          try
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.LastModifiedDate := now;
            response.ContentLocation := ''; // does not exist as returned
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
    manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
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

end.

