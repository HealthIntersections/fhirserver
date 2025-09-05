unit ftx_sct_services;

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

// URL: http://snomed.info/sct/[module]/version/[e.g. 20150131]'
// see https://digital.nhs.uk/services/terminology-server/training-guides/use-the-terminology-server-in-a-community/content-release-process/appendix-g

Interface

{
This works by pre-processing a snomed distribution into a tightly
condensed set of structures that capture a ready to use analysis
of the structure. The content is stored in strings which are arrays
of words, cardinals, or seperated character values.

The content loads and works extremely quickly.
}

Uses
  SysUtils, Classes, Generics.Collections, Character,
  fsl_base, fsl_utilities, fsl_collections, fsl_http, fsl_fpc, fsl_threads, fsl_lang, fsl_logging, fsl_i18n,
  fhir_objects, fhir_common, fhir_factory, fhir_utilities, fhir_features, fhir_uris,
  fhir_cdshooks,
  ftx_sct_expressions, ftx_service;

Const
  SNOMED_CACHE_VERSION_CURRENT = '17';
    // 15: add default language refset
    // 16: change to 8 byte strings not 16 byte strings
    // 17: track refset languages
    SNOMED_CACHE_VERSION_UTF8 = '16';
    SNOMED_CACHE_VERSION_UTF16 = '15';
  IS_A_MAGIC : UInt64 = 116680003;
  ALL_DISPLAY_NAMES = $FF;
  ASSUME_CLASSIFIED = true;

var
  SNOMED_DATE_FORMAT : TFormatSettings;

const
  RF2_MAGIC_DEFINED = 900000000000073002;
  RF2_MAGIC_PRIMITIVE = 900000000000074008;
  RF2_MAGIC_FSN = 900000000000003001;
  RF2_MAGIC_PREFERRED : UInt64 = 900000000000548007;
  RF2_MAGIC_RELN_DEFINING = 900000000000006009;
  RF2_MAGIC_RELN_STATED = 900000000000010007;
  RF2_MAGIC_RELN_INFERRED = 900000000000011006;

  // This Namespace Identifier Issued to Health Intersections to support
  // FHIR build tooling. It is not to be used for defining concepts and
  // distributing
  SCT_FHIR_NS = 1000201;
  // sct id = [assigned id] [namespace] [1] [type] [check]
  // types:
  // 0 A Concept
  // 1 A Description
  // 2 A Relationship
  // 3 A Subset
  // 4 A Cross Map Set
  // 5 A Cross Map Target

  COMBINED_MODULE_ID = 11000201108;

type
  UInt64Array = Array of UInt64;
  TCardinalArray = packed array of Cardinal;
  TMatch = record
    index : cardinal;
    term : UInt64;
    Priority : Double;
  End;

  TMatchArray = Array of TMatch;

  TSnomedDate = word; // whole number of days matching whole number portion of TDateTime

  // We store snomed as four structures.
  //   the first structure is a simply a list of strings which are variable length names - referred to from the other structures
  //   the second structure is a list of lists of cardinal references
  //   the third structure is a list of descriptions. Each has some a flag for type and initial status, and a description. we do not store the identity at this time
  //   the fourth structure is a list of concepts, stored in order of their identity, along with a flag for (status, primitive, single parent), a parent reference, a description, and a source and target reference list
  //   the fifh structure is a list of relationships - source, type, target, and a flag


  //   the second structure is a list of lists of word or cardinal references.
  //   the third structure is a list of concepts. each concept has a refernce to a name and a contained list of references which are either children
  //   the fourth structure is the code list - a list of loinc concepts, with codes and references to names and properties

  // We store snomed as three Strings
  //   each entry in the String starts with a byte length, and then the series of characters as bytes
  TSnomedStrings = class (TFslObject)
  Private
    FIsUTF16 : boolean;
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    procedure clear;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    property isUTF16 : boolean read FIsUTF16 write FIsUTF16;
    Function GetEntry(iIndex : Cardinal):String;

    Procedure StartBuild;
    Procedure Reopen;
    Function AddString(Const s : String) : Cardinal;
    Procedure DoneBuild;
  End;

const
  FLAG_WORD_FSN = $01;
  FLAG_WORD_DEP = $04;

Type
  // word index. Every word is 5 bytes - a 4 byte index into the strings, and a 1 byte flag
  TSnomedWords = class (TFslObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    procedure clear;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
 Public
    Procedure GetEntry(iIndex : Cardinal; var index : Cardinal; var flags : Byte);
    Function Count : Integer;
    Function GetString(iIndex : Cardinal) : Cardinal;

    Procedure StartBuild;
    Procedure AddWord(index : Cardinal; Flags : Byte);
    Procedure DoneBuild;
  End;

  // stem word index. Every word is 8 bytes - a 4 byte index into the strings, and a 4 byte index into the references
  TSnomedStems = class (TFslObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    procedure clear;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
 Public
    Procedure GetEntry(iIndex : Cardinal; var index : Cardinal; var reference : Cardinal);
    Function Count : Integer;
    Function GetString(iIndex : Cardinal) : Cardinal;

    Procedure StartBuild;
    Procedure AddStem(index, reference : Cardinal);
    Procedure DoneBuild;
  End;


  // 2. a list of list of references
  TSnomedReferences = class (TFslObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    procedure clear;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    Function GetReferences(iIndex : Cardinal) : TCardinalArray;
    Function Getlength(iIndex : Cardinal) : Cardinal;

    Procedure StartBuild;
    Function AddReferences(Const a : TCardinalArray) : Cardinal;
    Procedure DoneBuild;
    Procedure Post;
  End;

  // 3. a list of descriptions with flag. we do not store description id
const
  DESC_SIZE = 40;
//  MASK_DESC_STATUS = $0F; // bits 1-4
//  MASK_DESC_STYLE = $30;  // bits 5-6
//  MASK_DESC_CAPS_MASK = $C0;   // bits 7-8
//  MASK_DESC_CAPS_NONE = $00;
//  MASK_DESC_CAPS_FIRST = $40;
//  MASK_DESC_CAPS_ALL = $80;
//
//  VAL_DESC_Unspecified = 0;
//  VAL_DESC_Preferred = 1;
//  VAL_DESC_Synonym = 2;
//  VAL_DESC_FullySpecifiedName = 3;
//
  FLAG_Active = 0;
  FLAG_RetiredWithoutStatedReason = 1;
  FLAG_Duplicate = 2;
  FLAG_Outdated = 3;
  FLAG_Ambiguous = 4;
  FLAG_Erroneous = 5;
  FLAG_Limited = 6;
  FLAG_Inappropriate = 7;
  FLAG_ConceptInactive = 8;
  FLAG_MovedElswhere = 10;
  FLAG_PendingMove = 11;

Type
  TSnomedDescriptions = class (TFslObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    procedure clear;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    function Count : Cardinal;
    Procedure GetDescription(iIndex : Cardinal; out iDesc : Cardinal; out id : UInt64; out date : TSnomedDate; out concept, module, kind, caps, refsets, valueses : Cardinal; out active : Boolean; out lang : byte);
    function ConceptByIndex(iIndex : Cardinal) : cardinal;


    Procedure StartBuild;
    Function AddDescription(iDesc : Cardinal; id : UInt64; date : TSnomedDate; concept, module, kind, caps : Cardinal; active : Boolean; lang : byte) : Cardinal;
    Procedure UpdateDetails(iIndex : Cardinal; id : UInt64; concept : Cardinal);
    Procedure DoneBuild;
    Procedure SetRefsets(iIndex : Cardinal; refsets, valueses : Cardinal);
  End;

  TSnomedDescriptionIndex = class (TFslObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    procedure clear;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    Function FindDescription(iIdentity : UInt64; out IIndex : Cardinal) : boolean;

    Procedure StartBuild;
    procedure AddDescription(id : UInt64; reference : Cardinal);
    Procedure DoneBuild;
  End;

  // 4. a list of list of terms
  //   id as a 8 byte UInt64
  //   flag is a byte
  //   descriptions as a cardinal index of cardinal indexes
  //   parents as a cardinal index of cardinal indexes
  //   inbounds as a cardinal index of cardinal indexes
  //   outbounds as a cardinal index of cardinal indexes
  //   descendants (closure) as either 0 - not indexed, $FFFFFFFF - no children, or index of cardinals = all descendants
  //   depth as a byte
  //   stems as a cardinal index of stems
  //   effective date as a word
  //   moduleId as a cardinal (concept reference)
  //   status as a cardinal (concept reference)

Const
  MASK_CONCEPT_STATUS = $0F;
  MASK_CONCEPT_PRIMITIVE = $10; // this leaves three bits unused (6-8)
  MAGIC_NO_CHILDREN = $FFFFFFFF;

  CONCEPT_SIZE = 56;

Type
  TSnomedConceptList = class (TFslObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    procedure clear;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    Function FindConcept(iIdentity : UInt64; var IIndex : Cardinal) : boolean;
    function getConceptId(iIndex : Cardinal) : UInt64;
    procedure GetConcept(iIndex : Cardinal; out Identity : UInt64; out Flags : Byte; out effectiveTime : TSnomedDate; out Parents : Cardinal; out Descriptions : Cardinal; out Inbounds : Cardinal; out outbounds : Cardinal; out refsets : Cardinal);
    Function GetParent(iIndex : Cardinal): Cardinal;
    Function GetIdentity(iIndex : Cardinal): UInt64;
    Function Count : Cardinal;

    procedure SetParents(iIndex: Cardinal; const Active, Inactive: Cardinal);
    procedure SetDescriptions(iIndex: Cardinal; const Value: Cardinal);
    Function GetDescriptions(iIndex: Cardinal) : Cardinal;
    procedure SetInbounds(iIndex: Cardinal; const Value: Cardinal);
    Function GetInbounds(iIndex: Cardinal) : Cardinal;
    procedure SetOutbounds(iIndex: Cardinal; const Value: Cardinal);
    Function GetOutbounds(iIndex: Cardinal): Cardinal;

    Function GetAllDesc(iIndex: Cardinal) : Cardinal;
    procedure SetAllDesc(iIndex: Cardinal; Value: Cardinal);
    Function GetDepth(iIndex: Cardinal) : Byte;
    procedure SetDepth(iIndex: Cardinal; Value: Byte);
    Function GetStems(iIndex: Cardinal) : Cardinal;
    procedure SetStems(iIndex: Cardinal; Value: Cardinal);

    Function GetModuleId(iIndex: Cardinal) : Cardinal;
    procedure SetModuleId(iIndex: Cardinal; Value: Cardinal);
    Function GetStatus(iIndex: Cardinal) : Cardinal;
    procedure SetStatus(iIndex: Cardinal; Value: Cardinal);
    Function GetRefsets(iIndex: Cardinal) : Cardinal;
    procedure SetRefsets(iIndex: Cardinal; Value: Cardinal);

    procedure SetFlag(iIndex: Cardinal; iFlags: Byte);
    procedure SetDate(iIndex: Cardinal; effectiveTime: TSnomedDate);

    procedure SetNormalForm(iIndex: Cardinal; const Value: Cardinal);
    function GetNormalForm(iIndex: Cardinal) : Cardinal;

    // these presume that the terms are registered in order
    Procedure StartBuild;
    Function AddConcept(iIdentity : UInt64; effectiveTime : TSnomedDate; iFlags : Byte) : Cardinal;
    Procedure DoneBuild;
  End;

// 5. a list of relationships
Const
  RELATIONSHIP_SIZE = 40;
  REFSET_SIZE_NOLANG = 28;
  REFSET_SIZE_LANG = 32;

Type
  TSnomedRelationshipList = class (TFslObject)
  private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    procedure clear;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    // for Persistence
    Procedure GetRelationship(iIndex: Cardinal; out identity : UInt64; out Source, Target, RelType, module, kind, modifier : Cardinal; out date : TSnomedDate; out Active, Defining : Boolean; out Group : Integer);

    Procedure StartBuild;
    Function AddRelationship(identity : UInt64; Source, Target, RelType, module, kind, modifier : Cardinal; date : TSnomedDate; Active, Defining : Boolean; Group : integer) : Cardinal;
    Procedure DoneBuild;

    function count : cardinal;
  End;

Type
  TSnomedReferenceSetMember = record
    id : TGuid;
    module : Cardinal;
    date : TSnomedDate;
    kind : byte; // 0 = concept, 1 = desc, 2 = relationship
    Ref : Cardinal; // desc or term depending on ref set type
    values : Cardinal;
  End;
  TSnomedReferenceSetMemberArray = array of TSnomedReferenceSetMember;
  TRefSetMemberEntry = record
    refset : cardinal;
    types : cardinal;
    values : cardinal;
  end;
  TRefSetMemberEntryArray = array of TRefSetMemberEntry;

  // 2. a list of list of references
  TSnomedReferenceSetMembers = class (TFslObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    procedure clear;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    Function GetMembers(iIndex : Cardinal) : TSnomedReferenceSetMemberArray;
    Function GetMemberCount(iIndex : Cardinal) : cardinal;

    Procedure StartBuild;
    Function AddMembers(ids : boolean; Const a : TSnomedReferenceSetMemberArray) : Cardinal;
    Procedure DoneBuild;
  End;


  { TSnomedReferenceSetIndex }

  TSnomedReferenceSetIndex = class (TFslObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
    FHasLangs : boolean;
    FRecordSize : integer;
    procedure clear;
    procedure SetHasLangs(AValue: boolean);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    property HasLangs : boolean read FHasLangs write SetHasLangs;
    Procedure GetReferenceSet(iIndex: Cardinal; out iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iFieldTypes, iFieldNames, iLangs: Cardinal);
    Function GetMembersByConcept(iIndex : Cardinal; bByName : Boolean) : Cardinal;
    Function GetRefSetByConcept(iIndex : Cardinal) : Cardinal;
    Function Count : Integer;

    Procedure StartBuild;
    Procedure AddReferenceSet(iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iFieldTypes, iFieldNames, iLangs: Cardinal);
    Procedure DoneBuild;
  End;

(*
{
operations
  isSubsumedBy(term, term)
  getDesc : String
  is valid (id)
}

  TSnomedConceptSummary = class (TFslObject)
  private
    FIsPrimitive: boolean;
    FConcept: String;
    FDescription: String;
    FParents: TFslStringList;
    FStatus: TSnomedConceptStatus;
    FChildren: TFslStringList;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Property Concept : String read FConcept;
    Property Description : String read FDescription;
    Property IsPrimitive : boolean read FIsPrimitive;
    Property Status : TSnomedConceptStatus read FStatus;
    Property Children : TFslStringList read FChildren;
    Property Parents : TFslStringList read FParents;
  End;

*)
//  TSnomedReferenceSet = class;

  TSnomedFilterContext = class (TCodeSystemProviderFilterContext)
  private
    ndx : integer;
    matches : TMatchArray;
    members : TSnomedReferenceSetMemberArray;
    descendants : TCardinalArray;
  end;

  // note: source is not always populated
  TSnomedExpressionContext = class (TCodeSystemProviderContext)
  private
    FSource: String;
    FExpression : TSnomedExpression;
    procedure SetExpression(value : TSnomedExpression);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(reference : cardinal); overload;
    constructor Create(source : String; reference : cardinal); overload;
    constructor Create(source : String; expression : TSnomedExpression); overload;
    destructor Destroy; override;
    function Link : TSnomedExpressionContext; overload;

    Property source : String read FSource write FSource;
    Property Expression : TSnomedExpression read FExpression write SetExpression;

    function isComplex : boolean;
    function reference : cardinal;
  end;

  TSnomedServicesRenderOption = (sroMinimal, sroAsIs, sroFillMissing, sroReplaceAll);

  TSnomedRefinementGroupMatchState = (gmsNoMatch, gmsIdentical, gmsSubsumed);

  TMatchingConcept = class (TFslObject)
  private
    FMatched : String;
    FUnmatched : TFslList<TSnomedRefinementGroup>;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(match : String); overload;
    constructor Create(match : String; nonmatched : TFslList<TSnomedRefinementGroup>); overload;
    destructor Destroy; override;

    property matched : String read FMatched;
    property Unmatched : TFslList<TSnomedRefinementGroup> read FUnmatched;
  end;

  { TSnomedServices }

  TSnomedServices = class (TFslObject)
  Private
    FLock : TFslLock;
    FLanguages : TIETFLanguageDefinitions;
    FLastUse : TDateTime;
    FSourceFile : String;
    FBuilding : boolean;
    FUseCount : integer;

    FEdition : String;
    FVersion : String;
    FTotalCount : Integer;
    FActiveRoots : UInt64Array;
    FInactiveRoots : UInt64Array;
    FIs_a_Index, FFSN, FPreferredTerm, FDefaultLanguage : Cardinal;
    FStrings : TSnomedStrings;
    FRefs : TSnomedReferences;
    FDesc : TSnomedDescriptions;
    FDescRef : TSnomedDescriptionIndex;
    FConcept : TSnomedConceptList;
    FRel : TSnomedRelationshipList;
    FRefSetIndex : TSnomedReferenceSetIndex;
    FRefSetMembers : TSnomedReferenceSetMembers;
    FDefLangSet : TSnomedReferenceSetMemberArray;
    FVersionUri : String;
    FVersionDate : String;

    FWords : TSnomedWords;
    FStems : TSnomedStems;
    FEditionUri : String;
    FEditionId : String;
    FEditionName : String;

    function filterIn(id : UInt64): TCodeSystemProviderFilterContext;
    function filterEquals(id : UInt64): TCodeSystemProviderFilterContext;
    function filterIsA(id : UInt64; includeBase : boolean): TCodeSystemProviderFilterContext;

  //  Function FindWord(s : String; var index : Integer) : Boolean;
    Function FindStem(s : String; var index : Integer) : Boolean;
    procedure SetVersionUri(const Value: String);

    // expression support
    function normalise(s : String) : String; overload;

    procedure checkExpr(expression : TSnomedExpression); overload;
    procedure checkExpr(concept : TSnomedConcept); overload;
    procedure checkExpr(refinement : TSnomedRefinement); overload;

    procedure renderExpr(b : TFslStringBuilder; expr : TSnomedExpression; option : TSnomedServicesRenderOption); overload;
    procedure renderExpr(b : TFslStringBuilder; expr : TSnomedConcept; option : TSnomedServicesRenderOption); overload;
    procedure renderExpr(b : TFslStringBuilder; expr : TSnomedRefinement; option : TSnomedServicesRenderOption); overload;

    procedure displayExpr(b : TFslStringBuilder; expr : TSnomedExpression); overload;
    procedure displayExpr(b : TFslStringBuilder; expr : TSnomedConcept); overload;
    procedure displayExpr(b : TFslStringBuilder; expr : TSnomedRefinement); overload;

//    function findRefinement(ref: cardinal; b : TSnomedExpression): TSnomedExpression;
//    procedure findRefinements(exp: TSnomedExpression; relationship, focus: Cardinal);
    function findMatchingGroup(r : TSnomedRefinementGroup; exp : TSnomedExpression) : TSnomedRefinementGroup; overload;
    function findMatchingGroup(r : TSnomedRefinementGroup; grps : TFslList<TSnomedRefinementGroup>) : TSnomedRefinementGroup; overload;
    function groupsMatch(a, b : TSnomedRefinementGroup): boolean;
    function subsumesGroup(a, b : TSnomedRefinementGroup): boolean;
    function subsumesConcept(a, b : TSnomedConcept): boolean;

    procedure createDefinedExpr(reference : Cardinal; exp : TSnomedExpression; ancestor : boolean); overload;
    procedure findMatchingConcepts(list : TFslList<TMatchingConcept>; reference : cardinal; refinements : TFslList<TSnomedRefinementGroup>);
    function checkGroupStateInRefinements(grp : TSnomedRefinementGroup; refinements : TFslList<TSnomedRefinementGroup>; grps : TFslList<TSnomedRefinementGroup>) : TSnomedRefinementGroupMatchState;
    function listNonMatchingGroups(tgt, src : TFslList<TSnomedRefinementGroup>) : TFslList<TSnomedRefinementGroup>;
    procedure mergeRefinements(list : TSnomedRefinementList);
    function mergeGroups(grp1, grp2 : TSnomedRefinementGroup) : boolean;
    procedure rationaliseExpr(exp: TSnomedExpression);
    function GetEditionName: String;
    function GetEditionId: String;

    function loadLang(iLang : cardinal) : TSnomedReferenceSetMemberArray;
    function GetConcept: TSnomedConceptList;
    function GetDesc: TSnomedDescriptions;
    function GetDescRef: TSnomedDescriptionIndex;
    function GetRefs: TSnomedReferences;
    function GetRefsetIndex: TSnomedReferenceSetIndex;
    function GetRefsetMembers: TSnomedReferenceSetMembers;
    function GetRel: TSnomedRelationshipList;
    function GetStems: TSnomedStems;
    function GetStrings: TSnomedStrings;
    function GetWords: TSnomedWords;
    procedure LoadFromSource;
    function GetActiveRoots: UInt64Array;
    function GetDefaultLanguage: Cardinal;
    function GetInActiveRoots: UInt64Array;
    function GetIs_a_Index: Cardinal;
    function getDefaultLangRefSetName() : String;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(languages : TIETFLanguageDefinitions);
    destructor Destroy; Override;
    Function Link : TSnomedServices; Overload;
    Procedure Load(Const sFilename : String);
    class function checkFile(Const sFilename : String) : String;
    Procedure Save(Const sFilename : String);
    property Building : boolean read FBuilding write FBuilding;

    // helper functions
    Function StringToId(Const s : String) : UInt64;
    Function StringToIdOrZero(Const s : String) : UInt64;
    Function StringIsId(Const s : String; var iId : UInt64) : Boolean;

    // direct access to the raw data
    Property Strings : TSnomedStrings read GetStrings;
    Property Words : TSnomedWords Read GetWords;
    Property Stems : TSnomedStems Read GetStems;
    Property Refs : TSnomedReferences read GetRefs;
    Property Desc : TSnomedDescriptions read GetDesc;
    Property Concept : TSnomedConceptList read GetConcept;
    Property Rel : TSnomedRelationshipList read GetRel;
    Property RefSetIndex : TSnomedReferenceSetIndex read GetRefsetIndex;
    Property RefSetMembers : TSnomedReferenceSetMembers read GetRefsetMembers;
    Property DescRef : TSnomedDescriptionIndex read GetDescRef;
    function ChildRelationshipCount : cardinal; overload;
    function RefSetCount : cardinal; overload;

    // low level access for service providers
    Property ActiveRoots : UInt64Array read GetActiveRoots write FActiveRoots;
    Property InactiveRoots : UInt64Array read GetInActiveRoots write FInActiveRoots;
    Property Is_a_Index : Cardinal read GetIs_a_Index write FIs_a_Index;
    Property DefaultLanguageRefSet : Cardinal read GetDefaultLanguage write FDefaultLanguage;
    function Subsumes(iParent, iChild: Cardinal): Boolean; Overload;
    Function GetDisplayName(Const iConcept, iLang : Cardinal) : String; Overload;
    Procedure ListDisplayNames(list : TConceptDesignations; Const iConcept, iLang : Cardinal; FlagMask : Byte); Overload;
    Function GetConceptId(Const iConcept : Cardinal) : String; Overload;
    function GetDescriptionId(Const iDesc : Cardinal) : String; Overload;
    function GetRelationshipId(Const iRel : Cardinal) : String; Overload;
    Procedure GetMatchInfo(iConcept : Cardinal; var sTerm, sFSN, sPreferred : String);
    Function GetConceptRefSet(iConcept : Cardinal; bByName : Boolean; var iName, iMembers, iTypes, iFieldNames : cardinal) : Cardinal;
    Function GetDescRefsets(iDesc : Cardinal) : TRefSetMemberEntryArray;
    Function GetConceptRefsets(iDesc : Cardinal) : TRefSetMemberEntryArray;
    Function CheckLangSet(sTerm : String) : Cardinal;
    function GetConceptDescendants(index : Cardinal) : TCardinalArray;
    Function GetPN(iDescriptions : TCardinalArray) : String;
    Function GetFSN(iDescriptions : TCardinalArray) : String;
    function GetPNForConcept(iIndex: Cardinal): String;
    function GetConceptParents(index : Cardinal) : TCardinalArray;
    function GetConceptChildren(index : Cardinal) : TCardinalArray;
    function GetDefiningRelationships(index : Cardinal) : TCardinalArray; // but not is_a - the idea is that they'll be processed differently, since they're transitive
    function isPrimitive(index : Cardinal) : boolean;
    function IsActive(index : Cardinal) : boolean;
    function DebugDesc(index : cardinal) : String;
    function getRelationshipValues(index : cardinal) : String;

    // simplified interface for consumers
    Function ConceptExists(conceptId : String) : Boolean; overload;
    Function ConceptExists(conceptId : String; var index : cardinal) : Boolean; overload;
    Function Subsumes(Const sParent, sChild : String) : Boolean; Overload;
    Function Search(iRoot : UInt64; sText : String; iLang : Cardinal; bInactive : Boolean; bAll : boolean = false) : TMatchArray; overload;
    Function IsValidConcept(Const sTerm : String):Boolean;
    Function IsValidDescription(Const sTerm : String; var concept : UInt64; var description : String):Boolean;
    Function GetDisplayName(Const sTerm, sLangSet : String) : String; Overload;
    Procedure ListDisplayNames(list : TConceptDesignations; Const sTerm, sLangSet : String; FlagMask : Byte); Overload;
    function ReferenceSetExists(sid : String) : Boolean;
    function getRefSet(id : int64) : TSnomedReferenceSetMemberArray;
    function systemUri : String;

    // status stuff
    Property VersionUri : String read FVersionUri write SetVersionUri;
    Property EditionUri : String read FEditionUri;
    Property VersionDate : String read FVersionDate write FVersionDate;
    Property EditionName : String read GetEditionName;
    Property EditionId : String read GetEditionId;
    Property TotalCount : integer read FTotalCount;
    function LastUseStatus : String;
    property UseCount : integer read FUseCount;
    procedure RecordUse(count : integer = 1);
    function buildValueSet(factory : TFHIRFactory; url : String) : TFhirValueSetW;

    // expressions functionality
    function parseExpression(source : String) : TSnomedExpression;
    function renderExpression(source : TSnomedExpression; option : TSnomedServicesRenderOption) : String;
    function displayExpression(source : TSnomedExpression) : String;  overload;
    function expressionsEquivalent(a, b : TSnomedExpression; var msg : String) : boolean;  overload;
    function normaliseExpression(exp : TSnomedExpression) : TSnomedExpression;  overload;
    function condenseExpression(exp : TSnomedExpression) : TFslList<TMatchingConcept>;  overload;
    function expressionSubsumes(a, b : TSnomedExpression) : boolean;  overload;
    function createNormalForm(reference : Cardinal) : TSnomedExpression; overload;

  End;

  TSnomedServiceList = class (TFslObjectList)
  Private
    FDefinition: TSnomedServices;
    function GetDefinition(iIndex: Integer): TSnomedServices;
    procedure SetDefinition(const Value: TSnomedServices);
  Protected
    Function ItemClass : TFslObjectClass; Override;
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    destructor Destroy; Override;

    Function GetDefinitionByName(sName : String) : TSnomedServices;

    Property DefaultDefinition : TSnomedServices Read FDefinition write SetDefinition;
    Function HasDefaultDefinition : Boolean;

    Property Definition[iIndex : Integer] : TSnomedServices read GetDefinition; Default;
  End;

  { TSnomedProvider }

  TSnomedProvider = class (TCodeSystemProvider)
  public
    FSct : TSnomedServices;
    FSupplements : TFslList<TFHIRCodeSystemW>;
  public
    constructor Create(sct : TSnomedServices; i18n : TI18nSupport; supplements : TFslList<TFHIRCodeSystemW>);
    destructor Destroy; override;


    function description : String; override;
    function TotalCount : integer; override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri : String; override;
    function version : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function sameContext(opContext : TTxOperationContext; a, b : TCodeSystemProviderContext) : boolean; override;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    function subsumesTest(opContext : TTxOperationContext; codeA, codeB : String) : String; overload; override;
    procedure getCDSInfo(opContext : TTxOperationContext; card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); override;
    function IsInactive(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function getCodeStatus(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : String; override;
    function incompleteValidationMessage(context : TCodeSystemProviderContext; langs : THTTPLanguageList) : String; override;

    function defToThisVersion(specifiedVersion : String) : boolean; override;
    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
    function versionIsMoreDetailed(v1, v2 : String): boolean; override;

    property Services : TSnomedServices read FSct;
  end;


{
reference set

header....
  id  UInt64
members
  concept - cardinal
  desc - cardinal (or 0 if one is not specified)
  attribute - cardinal (0r 0 is one is not specified)

}
function FindMember(aMembers : TSnomedReferenceSetMemberArray; iRef : Cardinal; var iIndex: integer): boolean;
function FindCardinalInArray(a : TCardinalArray; iValue : Cardinal; var iIndex : Integer):Boolean;
function readDate(s : String) : TSnomedDate;
function readLang(s : String) : byte;
function codeForLang(lang : byte):String;
function langForCode(s : String) : byte;
function describeLangs(langs : integer) : string;

function genCheckDigit(s : String): char;

Implementation

uses
  ftx_loinc_services;

{ TCardinalArray }

function mergeCardinals(c1, c2 : Cardinal) : TCardinalArray; overload;
begin
  SetLength(result, 2);
  result[0] := c1;
  result[1] := c2;
end;

function mergeCardinals(c1 : Cardinal; c2 : TCardinalArray) : TCardinalArray; overload;
var
   i : integer;
begin
  setLength(result, length(c2)+1);
  result[0] := c1;
  for i := 0 to length(c2) - 1 do
    result[i+1] := c2[i];
end;

function mergeCardinals(c1, c2 : TCardinalArray) : TCardinalArray; overload;
var
   i : integer;
begin
  setLength(result, length(c1)+length(c2));
  for i := 0 to length(c2) - 1 do
    result[0] := c1[i];
  for i := 0 to length(c2) - 1 do
    result[i+length(c1)] := c2[i];
end;

{ TSnomedStrings }

function TSnomedStrings.GetEntry(iIndex: Cardinal): String;
var
  i : Word;
begin
  if (iIndex > FLength) then
    Raise ETerminologySetup.Create('Wrong length index getting snomed name');
  Move(FMaster[iIndex], i, 2);
  if (iIndex + 2 + (i * 2) > FLength) then
    Raise ETerminologySetup.Create('Wrong length index getting snomed name (2)');
  if FIsUTF16 then
    result := memU16ToString(FMaster, iIndex+2, i)
  else
  result := memU8ToString(FMaster, iIndex+2, i)
end;

procedure TSnomedStrings.Reopen;
begin
  FBuilder := TFslBytesBuilder.Create;
  FBuilder.Append(FMaster);
end;

function TSnomedStrings.AddString(const s: String): Cardinal;
var
  i : word;
  b : TArray<Byte>;
begin
  if Length(s) > 65535 Then
    raise ETerminologySetup.Create('Snomed Description too long: '+String(s));
  result := FBuilder.Length;
  if FIsUTF16 then
    b := TEncoding.BigEndianUnicode.GetBytes(s)
  else
    b := TEncoding.UTF8.GetBytes(s);
  i := length(b);
  FBuilder.AddWord(i);
  FBuilder.Append(b);
end;

procedure TSnomedStrings.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

procedure TSnomedStrings.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

procedure TSnomedStrings.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TSnomedStrings.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

{ TSnomedReferences }

Function TSnomedReferences.GetReferences(iIndex: Cardinal) : TCardinalArray;
var
  i : integer;
  c : Cardinal;
begin
  if (iIndex = MAGIC_NO_CHILDREN) or (iIndex = 0) then
    result := nil
  Else
  Begin
    if (FBuilder <> nil) and (iIndex >= FLength) then
      Post;
    if (iIndex >= FLength) then
      Raise ETerminologyError.Create('Wrong length index getting Snomed list. asked for '+inttostr(iIndex)+', limit is '+inttostr(FLength), itException);
    move(FMaster[iIndex], c, 4);
    SetLength(Result, c);
    if (iIndex + 4 + length(result) * 4 > FLength) then
      Raise ETerminologyError.Create('Wrong length index ('+inttostr(iIndex)+', '+inttostr(length(result))+') getting Snomed list (length = '+inttostr(FLength)+')', itException);
    inc(iIndex, 4);
    for i := 0 to Length(result)-1 Do
    Begin
      move(FMaster[iIndex], result[i], 4);
      inc(iIndex, 4);
    End;
  End;
end;

Function TSnomedReferences.AddReferences(Const a : TCardinalArray) : Cardinal;
var
  iLoop : Integer;
Begin
  result := FBuilder.Length;
  FBuilder.AddCardinal(length(a));
  for iLoop := Low(a) to High(a) Do
    FBuilder.AddCardinal(a[iLoop]);
End;

procedure TSnomedReferences.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

procedure TSnomedReferences.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

procedure TSnomedReferences.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TSnomedReferences.Getlength(iIndex: Cardinal): Cardinal;
begin
  if (iIndex > FLength) then
    Raise ETerminologyError.Create('Wrong length index getting Snomed list', itException);
  move(FMaster[iIndex], result, 4);
end;

procedure TSnomedReferences.Post;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
end;

function TSnomedReferences.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

{ TSnomedDescriptions }

procedure TSnomedDescriptions.SetRefsets(iIndex, refsets, valueses: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index getting snomed Desc Details', itException);
  assert(iIndex mod DESC_SIZE = 0);
  Move(refsets, FMaster[iIndex+32], 4);
  Move(valueses, FMaster[iIndex+36], 4);
end;

procedure TSnomedDescriptions.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

Function TSnomedDescriptions.AddDescription(iDesc : Cardinal; id : UInt64; date : TSnomedDate; concept, module, kind, caps : Cardinal; active : Boolean; lang : byte) : Cardinal;
begin
  result := FBuilder.Length;
  FBuilder.AddCardinal(iDesc);   // 4
  FBuilder.Append(byte(active)); // 5
  FBuilder.AddUInt64(id);        // 13
  FBuilder.AddCardinal(concept); // 17
  FBuilder.AddCardinal(module);  // 21
  FBuilder.AddCardinal(kind);    // 25
  FBuilder.AddCardinal(caps);    // 29
  FBuilder.AddWord(date);        // 31
  FBuilder.Append(lang);         // 32
  FBuilder.AddCardinal(0); // 36 refsets
  FBuilder.AddCardinal(0); // 40 refsets values
end;

procedure TSnomedDescriptions.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

function TSnomedDescriptions.ConceptByIndex(iIndex: Cardinal): cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index getting snomed Desc Details', itException);
  Move(FMaster[iIndex+13], result, 4);
end;

function TSnomedDescriptions.Count: Cardinal;
begin
  result := FLength div DESC_SIZE;
end;

procedure TSnomedDescriptions.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

procedure TSnomedDescriptions.GetDescription(iIndex : Cardinal; out iDesc : Cardinal; out id : UInt64; out date : TSnomedDate; out concept, module, kind, caps, refsets, valueses : Cardinal; out active : Boolean; out lang : byte);
Begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index getting snomed Desc Details', itException);
  Move(FMaster[iIndex+0], iDesc, 4);
  Move(FMaster[iIndex+4], active, 1);
  Move(FMaster[iIndex+5], ID, 8);
  Move(FMaster[iIndex+13], concept, 4);
  Move(FMaster[iIndex+17], module, 4);
  Move(FMaster[iIndex+21], kind, 4);
  Move(FMaster[iIndex+25], caps, 4);
  Move(FMaster[iIndex+29], date, 2);
  Move(FMaster[iIndex+31], lang, 1);
  Move(FMaster[iIndex+32], refsets, 4);
  Move(FMaster[iIndex+36], valueses, 4);
End;


procedure TSnomedDescriptions.UpdateDetails(iIndex: Cardinal; id: UInt64; concept: Cardinal);
var
  t1 : UInt64;
  t2 : Cardinal;
begin
  FBuilder.Read(iIndex+13, t2, 4);
  if (t2 <> concept) Then
  Begin
    assert(t2 = 0);

    FBuilder.Read(iIndex+5, t1, 8);
    assert(t1 = 0);
    FBuilder.WriteUInt64(iIndex+5, id);
    FBuilder.Read(iIndex+5, t1, 8);
    assert(t1 = id);

    FBuilder.WriteCardinal(iIndex+13, concept);
    FBuilder.Read(iIndex+13, t2, 4);
    assert(t2 = concept);
  End;
end;


function TSnomedDescriptions.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

{ TSnomedConceptList }

procedure TSnomedConceptList.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

Function TSnomedConceptList.AddConcept(iIdentity : UInt64; effectiveTime : TSnomedDate; iFlags : Byte) : Cardinal;
begin
  result := FBuilder.Length;
  FBuilder.AddUInt64(iIdentity); // 0
  FBuilder.Append(iFlags); // 8
  FBuilder.AddCardinal(0); // 9 active parents
  FBuilder.AddCardinal(0); // 13 descriptions
  FBuilder.AddCardinal(0); // 17 inbounds
  FBuilder.AddCardinal(0); // 21 outbounds
  FBuilder.AddCardinal(0); // 25 closures
  FBuilder.Append(0); // 29 depth
  FBuilder.AddCardinal(0); // 30 stems
  FBuilder.AddWord(effectiveTime); // 34 date
  FBuilder.AddCardinal(0); // 36 moduleId (rf2)
  FBuilder.AddCardinal(0); // 40 status (rf2)
  FBuilder.AddCardinal(0); // 44 refsets (rf2)
  FBuilder.AddCardinal(0); // 48 normal form
  FBuilder.AddCardinal(0); // 52 inactive parents
end;

procedure TSnomedConceptList.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

{$R-}

function TSnomedConceptList.FindConcept(iIdentity: UInt64; var IIndex: Cardinal): boolean;
var
  aConcept : UInt64;
  L, H, I: Integer;
begin
  Result := False;
  L := 0;
  H := FLength;
  H := (H div CONCEPT_SIZE) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Move(FMaster[(i*CONCEPT_SIZE)], aConcept, 8);
    if aConcept < iIdentity then L := I + 1 else
    begin
      H := I - 1;
      if aConcept = iIdentity then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  iIndex := (L*CONCEPT_SIZE);
end;

Function TSnomedConceptList.getParent(iIndex : Cardinal): Cardinal;
Begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+9], result, 4);
End;

function TSnomedConceptList.GetRefsets(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+44], result, 4);
end;

Function TSnomedConceptList.getIdentity(iIndex : Cardinal): UInt64;
Begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+0], result, 8);
End;

procedure TSnomedConceptList.GetConcept(iIndex : Cardinal; out Identity : UInt64; out Flags : Byte; out effectiveTime : TSnomedDate; out Parents : Cardinal; out Descriptions : Cardinal; out Inbounds : Cardinal; out outbounds : Cardinal; out refsets : Cardinal);
Begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details (mod = '+inttostr(iIndex mod CONCEPT_SIZE)+')', itException);

  Move(FMaster[iIndex+0], Identity, 8);
  Move(FMaster[iIndex+8], Flags, 1);
  Move(FMaster[iIndex+9], Parents, 4);
  Move(FMaster[iIndex+13], Descriptions, 4);
  Move(FMaster[iIndex+17], Inbounds, 4);
  Move(FMaster[iIndex+21], Outbounds, 4);
  Move(FMaster[iIndex+34], effectiveTime, 2);
  Move(FMaster[iIndex+44], refsets, 4);
End;

function TSnomedConceptList.getConceptId(iIndex : Cardinal): UInt64;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index getting snomed Concept Details', itException);
  Move(FMaster[iIndex+0], result, 8);
end;

procedure TSnomedConceptList.SetParents(iIndex: Cardinal; const Active, Inactive: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Active, FMaster[iIndex+9], 4);
  Move(Inactive, FMaster[iIndex+52], 4);
end;

procedure TSnomedConceptList.SetRefsets(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+44], 4);
end;

procedure TSnomedConceptList.SetDescriptions(iIndex: Cardinal; const Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+13], 4);
end;

procedure TSnomedConceptList.SetFlag(iIndex: Cardinal; iFlags: Byte);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(iFlags, FMaster[iIndex+8], 1);
end;

procedure TSnomedConceptList.SetInbounds(iIndex: Cardinal; const Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+17], 4);
end;

procedure TSnomedConceptList.SetModuleId(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+36], 4);
end;

procedure TSnomedConceptList.SetNormalForm(iIndex: Cardinal; const Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+48], 4);
end;

procedure TSnomedConceptList.SetOutbounds(iIndex: Cardinal; const Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+21], 4);
end;


procedure TSnomedConceptList.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

function TSnomedConceptList.Count: Cardinal;
begin
  result := FLength div CONCEPT_SIZE;
end;

Function TSnomedConceptList.GetAllDesc(iIndex: Cardinal) : Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+25], result, 4);
end;

procedure TSnomedConceptList.SetAllDesc(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+25], 4);
end;

function TSnomedConceptList.GetOutbounds(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+21], result, 4);
end;

function TSnomedConceptList.GetInbounds(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+17], result, 4);
end;

function TSnomedConceptList.GetModuleId(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+36], result, 4);
end;

function TSnomedConceptList.GetNormalForm(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+48], result, 4);
end;

function TSnomedConceptList.GetDepth(iIndex: Cardinal): Byte;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+29], result, 1);
end;

procedure TSnomedConceptList.SetDate(iIndex: Cardinal; effectiveTime: TSnomedDate);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(effectiveTime, FMaster[iIndex+34], 2);
end;

procedure TSnomedConceptList.SetDepth(iIndex: Cardinal; Value: Byte);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+29], 1);
end;

function TSnomedConceptList.GetStatus(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+40], result, 4);
end;

function TSnomedConceptList.GetStems(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+30], result, 4);
end;

procedure TSnomedConceptList.SetStatus(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+40], 4);
end;

procedure TSnomedConceptList.SetStems(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(Value, FMaster[iIndex+30], 4);
end;

function TSnomedConceptList.GetDescriptions(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength), itException);
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ETerminologyError.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details', itException);
  Move(FMaster[iIndex+13], Result, 4);
end;

function TSnomedConceptList.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

{ TSnomedServices }

constructor TSnomedServices.Create(languages : TIETFLanguageDefinitions);
begin
  inherited Create;
  FLanguages := languages;
  FLock := TFslLock.create('Snomed');
  FLastUse := 0;
  FStrings := TSnomedStrings.Create;
  FRefs := TSnomedReferences.Create;
  FDesc := TSnomedDescriptions.Create;
  FConcept := TSnomedConceptList.Create;
  FRel := TSnomedRelationshipList.Create;
  FWords := TSnomedWords.Create;
  FStems := TSnomedStems.Create;
  FRefSetIndex := TSnomedReferenceSetIndex.Create;
  FRefSetMembers := TSnomedReferenceSetMembers.Create;
  FDescRef := TSnomedDescriptionIndex.Create;
end;

function TSnomedServices.DebugDesc(index: cardinal): String;
begin
  result := GetConceptId(index)+'|'+GetDisplayName(index, FDefaultLanguage)+'|';
end;

function versionSplit(s : String) : TArray<string>;
var
  i : integer;
begin
  i := s.IndexOf('/version/');
  if i = 0 then
    result := TArray<String>.Create(s)
  else
    result := TArray<String>.Create(s.Substring(0, i), s.Substring(i+9));
end;


destructor TSnomedServices.Destroy;
begin
  FDescRef.free;
  FRefSetIndex.free;
  FRefSetMembers.free;
  FWords.free;
  FStems.free;
  FRefs.free;
  FStrings.free;
  FRel.free;
  FDesc.free;
  FConcept.free;
  FLanguages.free;
  FLock.free;
  inherited;
end;

procedure TSnomedServices.Load(const sFilename: String);
begin
  FSourceFile := sFilename;
  if not FileExists(FSourceFile) then
    raise ETerminologySetup.create('The SNOMED CT Source File '+sFilename+' does not exist');

  LoadFromSource;
end;

class function TSnomedServices.checkFile(const sFilename: String): String;
var
  oFile : Tfilestream;
  oread : TReader;
  v : String;
  s : TArray<string>;
begin
  try
    oFile := TFileStream.Create(sFilename, fmOpenread+fmShareDenyWrite);
    try
      oread := TReader.Create(oFile, 8192);
      try
        v := oRead.ReadString;
        if (v = SNOMED_CACHE_VERSION_CURRENT) or (v = SNOMED_CACHE_VERSION_UTF8) or (v = SNOMED_CACHE_VERSION_UTF16) Then
        begin
          v := oRead.ReadString;
          s := v.split(['/']);
          v := oread.ReadString;
          result := 'Ok (edition = '+s[4]+', version = '+s[6]+', date = '+v+')'
        end
        else
          result := 'Needs rebuilding';
      Finally
        oread.free;
      End;
    Finally
      oFile.free;
    End;
  except
    on e : Exception do
      result := 'Error: '+e.message;
  end;
end;

procedure TSnomedServices.LoadFromSource;
var
  oFile : Tfilestream;
  oread : TReader;
  i : Integer;
  s : TArray<String>;
  v : String;
  function readBytes : TBytes;
  begin
    SetLength(result, oRead.ReadInteger);
    oread.Read(result[0], length(result));
  end;
  function ReadUInt64: UInt64;
  begin
    oread.Read(result, 8);
  end;
begin
  oFile := TFileStream.Create(FSourceFile, fmOpenread+fmShareDenyWrite);
  try
    oread := TReader.Create(oFile, 8192);
    try
      FRefSetIndex.hasLangs := false;
      v := oRead.ReadString;
      if v = SNOMED_CACHE_VERSION_CURRENT Then
        FRefSetIndex.hasLangs := true
      else if v = SNOMED_CACHE_VERSION_UTF8 then
        FStrings.IsUTF16 := false
      else if v = SNOMED_CACHE_VERSION_UTF16 Then
        FStrings.IsUTF16 := true
      else
        raise ETerminologyError.create('The Snomed cache "'+FSourceFile+'" must be rebuilt using the server utilities ('+v+'/'+SNOMED_CACHE_VERSION_CURRENT+')', itException);
      VersionUri := oread.ReadString;
      VersionDate := oread.ReadString;
      s := VersionUri.split(['/']);
      FEdition := s[4];
      FVersion := s[6];
      FLock.Name := 'SCT '+FVersion;

      FStrings.FMaster := ReadBytes;
      FStrings.FLength := Length(FStrings.FMaster);
      FRefs.FMaster := ReadBytes;
      FRefs.FLength := Length(FRefs.FMaster);
      FDesc.FMaster := ReadBytes;
      FDesc.FLength := Length(FDesc.FMaster);
      FWords.FMaster := ReadBytes;
      FWords.FLength := Length(FWords.FMaster);
      FStems.FMaster := ReadBytes;
      FStems.FLength := Length(FStems.FMaster);
      FConcept.FMaster := ReadBytes;
      FConcept.FLength := Length(FConcept.FMaster);
      FTotalCount := FConcept.Count;
      FRel.FMaster := ReadBytes;
      FRel.FLength := Length(FRel.FMaster);
      FRefSetIndex.FMaster := ReadBytes;
      FRefSetIndex.FLength := Length(FRefSetIndex.FMaster);
      FRefSetMembers.FMaster := ReadBytes;
      FRefSetMembers.FLength := Length(FRefSetMembers.FMaster);
      FDescRef.FMaster := ReadBytes;
      FDescRef.FLength := Length(FDescRef.FMaster);
      FIs_a_Index := oread.ReadInteger;
      SetLength(FInactiveRoots, oRead.ReadInteger);
      for i := 0 to Length(FInactiveRoots) - 1 Do
        FInactiveRoots[i] := ReadUInt64;
      SetLength(FActiveRoots, oRead.ReadInteger);
      for i := 0 to Length(FActiveRoots) - 1 Do
        FActiveRoots[i] := ReadUInt64;
      FDefaultLanguage := oread.ReadInteger;
    Finally
      oread.free;
    End;
  Finally
    oFile.free;
  End;        
  if not Concept.FindConcept(RF2_MAGIC_PREFERRED, FPreferredTerm) then
    FPreferredTerm := 0;
  if not Concept.FindConcept(RF2_MAGIC_FSN, FFSN) then
    FFSN := 0;
  FDefLangSet := FRefSetMembers.GetMembers(FDefaultLanguage);
end;

function TSnomedServices.loadLang(iLang: cardinal): TSnomedReferenceSetMemberArray;
begin
  if iLang = FDefaultLanguage then
    result := FDefLangSet
  else
    result := FRefSetMembers.GetMembers(iLang);
end;

function TSnomedServices.ReferenceSetExists(sid: String): Boolean;
var
  iName, index, members, types, iFieldNames : Cardinal;
begin
  result := FConcept.FindConcept(StringToId(sid), index);
  if result then
    result := GetConceptRefSet(index, true, iName, members, types, iFieldNames) > 0;
end;

function TSnomedServices.RefSetCount: cardinal;
var
  i : integer;
  iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iFieldTypes, iFieldNames, iLangs: Cardinal;
begin
  result := RefSetIndex.Count;
  for i := 0 to RefSetIndex.Count - 1 do
  begin
    RefSetIndex.GetReferenceSet(i, iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iFieldTypes, iFieldNames, iLangs);
    result := result + RefSetMembers.GetMemberCount(iMembersByRef);
  end;
end;

procedure TSnomedServices.Save(const sFilename: String);
var
  oFile : Tfilestream;
  oWrite : TWriter;
  i : integer;
  procedure WriteBytes(b : TBytes);
  begin
   oWrite.WriteInteger(length(b));
   oWrite.Write(b[0], length(b));
  end;
  procedure WriteUInt64(v : UInt64);
  begin
    oWrite.Write(v, 8);
  end;
begin
  if FileExists(sFilename) Then
  begin
    FileSetReadOnly(sFilename, False);
    DeleteFile(sFilename);
  End;

  oFile := TFileStream.Create(sFilename, fmCreate);
  try
    oWrite := TWriter.Create(oFile, 8192);
    try
      oWrite.WriteString(SNOMED_CACHE_VERSION_CURRENT);
      oWrite.WriteString(VersionUri);
      oWrite.WriteString(VersionDate);
      WriteBytes(FStrings.FMaster);
      WriteBytes(FRefs.FMaster);
      WriteBytes(FDesc.FMaster);
      WriteBytes(FWords.FMaster);
      WriteBytes(FStems.FMaster);
      WriteBytes(FConcept.FMaster);
      WriteBytes(FRel.FMaster);
      WriteBytes(FRefSetIndex.FMaster);
      WriteBytes(FRefSetMembers.FMaster);
      WriteBytes(FDescRef.FMaster);
      oWrite.writeInteger(FIs_a_Index);
      oWrite.writeInteger(length(FInactiveRoots));
      for i := 0 to Length(FInactiveRoots) - 1 Do
        writeUInt64(FInactiveRoots[i]);
      oWrite.writeInteger(length(FActiveRoots));
      for i := 0 to Length(FActiveRoots) - 1 Do
        writeUInt64(FActiveRoots[i]);
      oWrite.writeInteger(FDefaultLanguage);
    Finally
      oWrite.free;
    End;
  Finally
    oFile.free;
  End;
end;


function FindCardinalInArray(a : TCardinalArray; iValue : Cardinal; var iIndex : Integer):Boolean;
Var
  L, H, I : Integer;
Begin
  Result := False;
  L := 0;
  H := Length(a) - 1;

  While L <= H Do
  Begin
    I := (L + H) Shr 1;

    If a[i] < iValue Then
      L := I + 1
    Else
    Begin
      H := I - 1;

      If a[i] = iValue Then
      Begin
        Result := True;
        L := I;
      End;
    End;
  End;

  iIndex := L;
End;

function TSnomedServices.Subsumes(iParent, iChild: Cardinal): Boolean;
var
  iAllDesc : TCardinalArray;
  iIndex : integer;
Begin
  if iParent = iChild then
    result := true
  else
  begin
    iAllDesc := FRefs.GetReferences(FConcept.GetAllDesc(iParent));
    result := FindCardinalInArray(iAllDesc, iChild, iIndex);
  end;
End;

type
  TSearchWord = record
    original : String;
    stem : Cardinal;
  End;
  TSearchWordArray = array of TSearchWord;

Procedure QuickSortArray(var a : TMatchArray);

  Procedure QuickSort(L, R: Integer);
  Var
    I, J, K : Integer;
    t : TMatch;
  Begin
    // QuickSort routine (Recursive)
    // * Items is the default indexed property that returns a pointer, subclasses
    //   specify these return values as their default type.
    // * The Compare routine used must be aware of what this pointer actually means.

    Repeat
      I := L;
      J := R;
      K := (L + R) Shr 1;

      Repeat
        While a[I].Priority > a[K].Priority Do
          Inc(I);

        While a[J].Priority < a[K].Priority Do
          Dec(J);

        If I <= J Then
        Begin
          t := a[i];
          a[i] := a[j];
          a[j] := t;

          // Keep K as the index of the original middle element as it might get exchanged.
          If I = K Then
            K := J
          Else If J = K Then
            K := I;

          Inc(I);
          Dec(J);
        End;
      Until I > J;

      If L < J Then
        QuickSort(L, J);

      L := I;
    Until I >= R;
  End;

Begin
  If length(a) > 1 Then
    QuickSort(0, length(a) - 1);
End;



function TSnomedServices.Search(iRoot : UInt64; sText: String; iLang : Cardinal; bInactive : Boolean; bAll : boolean): TMatchArray;
var
  aLangMembers : TSnomedReferenceSetMemberArray;

  Function Match(const words : TSearchWordArray; s : String; iDepth : byte):Double;
  var
    i : integer;
  Begin
    result := 0;
    s := lowercase(s);
    for i := 0 to length(words) - 1 Do
      if pos(words[i].original, s) > 0 Then
      Begin
        result := result + 10;
        if s = words[i].original then
          result := result + 12
        Else if StringStartsWith(s, words[i].original) Then
          result := result + 3
        Else If StringEndsWith(s, words[i].original) Then
          result := result + 1.5
        Else If (pos(' '+words[i].original+' ', s) > 0) Then
          result := result + 0.6;
        result := result + result / length(s);
        if iDepth > 0 Then
          result := result + result / iDepth;
      End;
  End;

  Procedure AddResult(var iCount : Integer; iindex : cardinal; id : UInt64; priority : Double);
  Begin
    if iCount = length(result) then
      SetLength(result, Length(result)+100);
    result[iCount].index := iIndex;
    result[iCount].term := id;
    result[iCount].Priority := priority;
    Inc(iCount);
  End;

  Function FlagFactor(active : boolean): double;
  Begin
    if (active) then
      result := 4
    Else
      result := 1;
  End;

  Procedure CheckConcept(const words : TSearchWordArray; var iCount : Integer; iConceptIndex : Integer);
  var
    i, j : cardinal;
    r1, r2, t : Double;
    iDepth : Byte;
    Identity, iID2 : UInt64;
    Flags, lang : Byte;
    Parents : Cardinal;
    Descriptions : Cardinal;
    Inbounds : Cardinal;
    outbounds : Cardinal;
    refsets, valueses : Cardinal;
    Desc : TCardinalArray;
    iWork, iDummy, module, kind, caps : Cardinal;
    date : TSnomedDate;
    ok : boolean;
    s : String;
    active : boolean;
  Begin
    SetLength(desc, 0);
    Concept.GetConcept(iConceptIndex, Identity, Flags, date, Parents, Descriptions, outbounds, Inbounds, refsets);
    iDepth := Concept.GetDepth(iConceptIndex);
    if bInactive Or ((Flags and MASK_CONCEPT_STATUS) = FLAG_Active) Then
    Begin
      r1 := 0;
      r2 := 0;
      t := 0;
      Desc := Refs.GetReferences(FConcept.GetStems(iConceptIndex));
      For i := 0 to length(words) - 1 do
      begin
        if words[i].stem <> 0 Then
          For j := 0 to length(Desc) - 1 do
            if (words[i].stem = desc[j]) Then
              r1 := r1 + 20 + (20 / length(desc))
            else
              assert(FStrings.GetEntry(words[i].stem) <> FStrings.GetEntry(desc[j]));
      end;

      Desc := Refs.GetReferences(Descriptions);
      for j := Low(Desc) to High(Desc) Do
      Begin
        FDesc.GetDescription(Desc[j], iWork, iID2, date, iDummy, module, kind, caps, refsets, valueses, active, lang);
        t := t + FlagFactor(active);
        r2 := r2 + Match(words, Strings.GetEntry(iWork), iDepth) * FlagFactor(active);
      End;
      if (r1 + r2 > 0) Then
      begin
        if not bAll then
          AddResult(iCount, iConceptIndex, Identity, r1 + r2 / t)
        else
        begin
          ok := false;
          for j := Low(Desc) to High(Desc) Do
          Begin
            FDesc.GetDescription(Desc[j], iWork, iID2, date, iDummy, module, kind, caps, refsets, valueses, active, lang);
            s := lowercase(Strings.GetEntry(iWork));
            ok := true;
            For i := 0 to length(words) - 1 do
              if not ((copy(s, 1, length(words[i].original)) = words[i].original) or (ansipos(' '+words[i].original, s) > 0)) then
                ok := false;
            if ok then
              break;
          End;
          if ok then
            AddResult(iCount, iConceptIndex, Identity, r1 + r2 / t)
        end;
      end;
    End;
  End;
var
  iCount : Integer;
  words : TSearchWordArray;
  iAll : TCardinalArray;
  i : integer;
  iC, iMembers : Cardinal;
  aMembers : TSnomedReferenceSetMemberArray;
  index : integer;
  oStemmer : TFslWordStemmer;
  s : String;
  s1 : String;
begin
  SetLength(words, 0);
  SetLength(aMembers, 0);
  SetLength(aLangMembers, 0);
  sText := LowerCase(sText);
  oStemmer := TFslWordStemmer.create('english');
  Try
    while (sText <> '') Do
    Begin
      StringSplit(sText, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='], s, sText);
      if (s <> '') Then
      Begin
        SetLength(words, length(words)+1);
        words[length(words)-1].original := lowercase(s);
        s1 := oStemmer.Stem(s);
        if FindStem(s1, index) Then
          words[length(words)-1].stem := FStems.GetString(index);
      End;
    End;
  Finally
    oStemmer.free;
  End;

  if Length(words) = 0 then
    Raise ETerminologyError.Create('no usable search text found', itException);

  if iLang <> 0 Then
    aLangMembers := loadLang(iLang);

  iCount := 0;
  SetLength(result, 100);
  sText := lowercase(sText);
  if iRoot = 0 Then
  Begin
    {if iLang <> 0 Then
      for i := 0 to Length(aLangMembers) - 1 Do
        CheckDescription(words, iCount, aLangMembers[i].Ref)
    else}
      for i := 0 to Concept.Count - 1 Do
        CheckConcept(words, iCount, i * CONCEPT_SIZE)
  end
  Else if Concept.FindConcept(iRoot, iC) Then
  Begin
    iMembers := FRefSetIndex.GetMembersByConcept(iC, true);
    if iMembers = 0 then
      iAll := FRefs.GetReferences(FConcept.GetAllDesc(iC))
    else
    Begin
      aMembers := FRefSetMembers.GetMembers(iMembers);
      SetLength(iAll, length(aMembers));
      For i := 0 to Length(iAll) - 1 Do
        iAll[i] := aMembers[i].Ref;
    End;
    for i := 0 to length(iAll) - 1 Do
      CheckConcept(words, iCount, iAll[i]);
  End;

  SetLength(result, iCount);
  QuickSortArray(result);
end;

function TSnomedServices.Subsumes(const sParent, sChild: String): Boolean;
var
  iParent : Cardinal;
  iChild : Cardinal;
begin
  if not StringIsInteger64(sParent) or not StringIsInteger64(sChild) then
    exit(false);

  Result := Concept.FindConcept(StringToId(sParent), iParent);
  if not result then
  begin
    result := DescRef.FindDescription(StringToId(sParent), iParent);
    if result then
      iParent := Desc.ConceptByIndex(iParent);
  end;
  if result then
  begin
    Result := Concept.FindConcept(StringToId(sChild), iChild);
    if not result then
    begin
      result := DescRef.FindDescription(StringToId(sChild), iChild);
      if result then
        iChild := Desc.ConceptByIndex(iChild);
    end;
    if Result Then
      result := Subsumes(iParent, iChild);
  end;
end;


function FindMember(aMembers : TSnomedReferenceSetMemberArray; iRef : Cardinal; var iIndex: integer): boolean;
var
  L, H, I: Integer;
  c : UInt64;
begin
  Result := False;
  L := 0;
  H := length(aMembers) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := aMembers[i].Ref;
    if C < iRef then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = iRef then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  iIndex := L;
end;

type
  TDescInfo = record
    index : cardinal;
    active : boolean;
    langok : boolean;
    kind : cardinal;
  end;

function TSnomedServices.GetDisplayName(const iConcept, iLang: Cardinal): String;
var
  iLoop : integer;
  Identity, iId2 : UInt64;
  Flags, lang : Byte;
  Parents : Cardinal;
  Descriptions : Cardinal;
  Descs : TCardinalArray;
  Inbounds : Cardinal;
  outbounds : Cardinal;
  iDesc, iDummy, module, kind, caps, refsets, valueses : Cardinal;
  iInt : integer;
  date : TSnomedDate;
  aMembers : TSnomedReferenceSetMemberArray;
  active : boolean;
  dlist : Array of TDescInfo;
begin
  SetLength(aMembers, 0);
  result := '';
  if iLang <> 0 then
    aMembers := loadLang(iLang);
  Concept.GetConcept(iConcept, Identity, Flags, date, Parents, Descriptions, Inbounds, outbounds, refsets);
  Descs := Refs.GetReferences(Descriptions);
  SetLength(dlist, length(descs));
  For iLoop := 0 to High(descs) Do
  Begin
    Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, caps, refsets, valueses, active, lang);
    dlist[iLoop].index := iDesc;
    dlist[iLoop].active := Active;
    dlist[iLoop].langok := (iLang = 0) or (FindMember(aMembers, descs[iLoop], iint));
    dlist[iLoop].kind := kind;
  end;

  // look for a preferred term in the lang set
  For iLoop := 0 to High(descs) Do
    if (dlist[iLoop].active and dlist[iLoop].langok and (dlist[iloop].kind = FPreferredTerm)) then
      exit(Strings.GetEntry(dlist[iLoop].index));

  // look for a synonym in the lang set
  For iLoop := 0 to High(descs) Do
    if (dlist[iLoop].active and dlist[iLoop].langok and (dlist[iloop].kind <> FFSN)) then
      exit(Strings.GetEntry(dlist[iLoop].index));

  // look for a preferred term
  For iLoop := 0 to High(descs) Do
    if (dlist[iLoop].active and (dlist[iloop].kind = FPreferredTerm)) then
      exit(Strings.GetEntry(dlist[iLoop].index));

  // look for a synonym
  For iLoop := 0 to High(descs) Do
    if (dlist[iLoop].active and (dlist[iloop].kind <> FFSN)) then
      exit(Strings.GetEntry(dlist[iLoop].index));

  // ok, still nothing,
  For iLoop := 0 to High(descs) Do
    if (dlist[iLoop].active and dlist[iLoop].langok) then
      exit(Strings.GetEntry(dlist[iLoop].index));

  For iLoop := 0 to High(descs) Do
    if (dlist[iLoop].active) then
      exit(Strings.GetEntry(dlist[iLoop].index));
  // if we still haven't found, then we are... lost
  result := GetPN(Descs);
end;

function TSnomedServices.GetDisplayName(const sTerm, sLangSet: String): String;
var
  iTerm, iLang : Cardinal;
begin
  iLang := CheckLangSet(sLangSet);
  if not Concept.FindConcept(StringToId(sTerm), iTerm) Then
    raise ETerminologyError.Create('Concept '+sTerm+' not found', itInvalid);
  result := GetDisplayName(iTerm, iLang);
end;

function TSnomedServices.GetEditionId: String;
begin
  if FEditionId = '' then
    GetEditionName;
  result := FEditionId;
end;

function TSnomedServices.GetEditionName: String;
var
  s : TArray<String>;
begin
  if FEditionName <> '' then
    exit(FEditionName);
  s := EditionUri.split(['/']);
  FEditionId := s[length(s)-1];
  result := '??';
  if FEditionId = '900000000000207008' then result := 'International Edition'
  else if FEditionId = '449081005' then result := 'International Spanish Edition'
  else if FEditionId = '11000221109' then result := 'Argentinian Edition'
  else if FEditionId = '32506021000036107' then result := 'Australian Edition (with drug extension)'
  else if FEditionId = '11000234105' then result := 'Austrian Edition'
  else if FEditionId = '11000172109' then result := 'Belgian Edition'
  else if FEditionId = '20621000087109' then result := 'Canadian English Edition'
  else if FEditionId = '20611000087101' then result := 'Canadian Canadian French Edition'
  else if FEditionId = '11000279109' then result := 'Czech Edition'
  else if FEditionId = '554471000005108' then result := 'Danish Edition'
  else if FEditionId = '11000181102' then result := 'Estonian Edition'
  else if FEditionId = '11000229106' then result := 'Finnish Edition'
  else if FEditionId = '11000274103' then result := 'German Edition'
  else if FEditionId = '1121000189102' then result := 'Indian Edition'
  else if FEditionId = '827022005' then result := 'IPS Terminology'
  else if FEditionId = '11000220105' then result := 'Irish Edition'
  else if FEditionId = '11000146104' then result := 'Netherlands Edition'
  else if FEditionId = '21000210109' then result := 'New Zealand Edition'
  else if FEditionId = '51000202101' then result := 'Norwegian Edition'
  else if FEditionId = '11000267109' then result := 'Republic of Korea Edition (South Korea)'
  else if FEditionId = '900000001000122104' then result := 'Spanish National Edition'
  else if FEditionId = '45991000052106' then result := 'Swedish Edition'
  else if FEditionId = '2011000195101' then result := 'Swiss Edition'
  else if FEditionId = '83821000000107' then result := 'UK Edition'
  else if FEditionId = '999000021000000109' then result := 'UK Clinical Edition'
  else if FEditionId = '5631000179106' then result := 'Uruguay Edition'
  else if FEditionId = '21000325107' then result := 'Chilean Edition'
  else if FEditionId = '731000124108' then result := 'US Edition'
  else if FEditionId = '5991000124107' then result := 'US Edition (with ICD-10-CM maps)'
  else if FEditionId = inttostr(COMBINED_MODULE_ID) then
    result := 'Combined View';
  FEditionName := result;
end;

function TSnomedServices.GetFSN(iDescriptions: TCardinalArray): String;
var
  iLoop : Integer;
  iid : UInt64;
  iString, iDummy, module, valueses, refsets, kind, caps : Cardinal;
  active : boolean;
  date : TSnomedDate;
  lang : byte;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, caps, refsets, valueses, active, lang);
    if (active) And (kind = FFSN) Then
      result := Strings.GetEntry(iString);
  End;
End;


function TSnomedServices.GetInActiveRoots: UInt64Array;
begin
  Result := FInActiveRoots;
end;

function TSnomedServices.GetIs_a_Index: Cardinal;
begin
  Result := FIs_a_Index;
end;

function TSnomedServices.getDefaultLangRefSetName(): String;
var
  i, j, iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iFieldTypes, iFieldNames, iLangs: Cardinal;
begin
  for i := 0 to FRefSetIndex.Count - 1 do
  begin
    FRefSetIndex.GetReferenceSet(i, iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iFieldTypes, iFieldNames, iLangs);
    if iMembersByRef = FDefaultLanguage then
      exit(FStrings.GetEntry(iName)+ ': '+describeLangs(iLangs)+' ('+inttostr(FDefaultLanguage)+' )');
  end;
  result := '?? ('+inttostr(FDefaultLanguage)+')';
end;

procedure TSnomedServices.ListDisplayNames(list: TConceptDesignations; const iConcept, iLang: Cardinal; FlagMask: Byte);
var
  aMembers : TSnomedReferenceSetMemberArray;
  iLoop : integer;
  Identity, iID2 : UInt64;
  Flags, lang : Byte;
  active : boolean;
  Parents, Descriptions, Inbounds, outbounds, valueses, refsets : Cardinal;
  Descs : TCardinalArray;
  iDesc, iDummy, module, kind, caps : Cardinal;
  iInt : Integer;
  date : TSnomedDate;
  first : boolean;
begin
  if iLang <> 0 then
    aMembers := loadLang(iLang);
  Concept.GetConcept(iConcept, Identity, Flags, date, Parents, Descriptions, Inbounds, outbounds, refsets);
  Descs := Refs.GetReferences(Descriptions);
  first := true;
  For iLoop := 0 to High(descs) Do
  Begin
    Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, caps, refsets, valueses, active, lang);
    if ((iLang = 0) or (FindMember(aMembers, descs[iLoop], iInt))) Then
    begin
      if active then
      begin                                                                                         
        list.addDesignation(first, true, '', codeForLang(lang), Strings.GetEntry(iDesc).trim).use :=
          list.factory.wrapCoding(list.factory.makeCoding('http://snomed.info/sct', IntToStr(Concept.getConceptId(kind)), GetPNForConcept(kind)));
        first := false;
      end
      else
        list.addDesignation(false, true, 'inactive', codeForLang(lang), Strings.GetEntry(iDesc).trim).use :=
           list.factory.wrapCoding(list.factory.makeCoding('http://snomed.info/sct', '900000000000546006', 'Inactive value'));
    end;
  End;
end;

procedure TSnomedServices.ListDisplayNames(list: TConceptDesignations; const sTerm, sLangSet: String; FlagMask: Byte);
var
  iTerm, iLang : Cardinal;
begin
  iLang := CheckLangSet(sLangSet);
  if not Concept.FindConcept(StringToId(sTerm), iTerm) Then
    raise ETerminologyError.Create('Concept '+sTerm+' not found', itInvalid);
  ListDisplayNames(list, iTerm, iLang, flagmask);
end;

procedure TSnomedServices.GetMatchInfo(iConcept: Cardinal; var sTerm, sFSN, sPreferred: String);
var
  iLoop : integer;
  Identity, iId2 : UInt64;
  Flags, lang : Byte;
  Parents : Cardinal;
  Descriptions : Cardinal;
  Descs : TCardinalArray;
  Inbounds : Cardinal;
  outbounds : Cardinal;
  iDesc, iDummy, module, kind, caps, valueses, refsets : Cardinal;
  date : TSnomedDate;
  active : boolean;
begin
  sTerm := '';
  sFSN := '';
  sPreferred := '';
  Concept.GetConcept(iConcept, Identity, Flags, date, Parents, Descriptions, Inbounds, outbounds, refsets);
  sTerm := IntToStr(Identity);
  Descs := Refs.GetReferences(Descriptions);
  For iLoop := 0 to High(descs) Do
  Begin
    Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, caps, refsets, valueses, active, lang);
      if (active) And (kind = FFSN) Then
        sFSN := Strings.GetEntry(iDesc)
      else if (active) And (kind = FPreferredTerm) Then
        sPreferred := Strings.GetEntry(iDesc);
      if (sPreferred <> '') and (sFSN <> '') then
        break;
  End;
end;
function TSnomedServices.GetPN(iDescriptions: TCardinalArray): String;
var
  iLoop : Integer;
  iid : UInt64;
  iString, iDummy, module, valueses, refsets, kind, caps : Cardinal;
  lang : Byte;
  date : TSnomedDate;
  iList, values : TCardinalArray;
  fsn, v, d : String;
  active : boolean;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, caps, refsets, valueses, active, lang);
    if (active) And (kind = FPreferredTerm) Then
      exit(Strings.GetEntry(iString));
  End;
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, caps, refsets, valueses, active, lang);
    // check the language reference set
    iList := Refs.GetReferences(refsets);
    v := Strings.GetEntry(iString);
    if valueses <> 0 then
    begin
      values := Refs.GetReferences(valueses); // get the list of all value lists
      values := Refs.GetReferences(values[0]); // get the first value set
      if (length(values) > 0) and (active) and (values[0] = FPreferredTerm) then
        exit(v);
    end
    else if ((result = '') or (length(result) > length(v))) and (active) And (Length(iList) > 0) Then
      result := v;
  End;

  if result = '' Then // ok, give up. and use the FSN
  begin
    fsn := GetFSN(iDescriptions);
    v := '';
    For iLoop := Low(iDescriptions) To High(iDescriptions) Do
    Begin
      Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, caps, refsets, valueses, active, lang);
      d := Strings.GetEntry(iString);
      if fsn.StartsWith(d+' (') then
        exit(d);
      if (active) Then
        if v = '' then
          v := d
        else if length(v) < length(d) then
          v := d;
    End;
    if (v = '') then
      exit(fsn)
    else
      exit(v);
  end;
end;

{
function TSnomedServices.FindWord(s: String; var index : integer): Boolean;
var
  L, H, I, c: Integer;
begin
  Result := False;
  L := 0;
  H := FWords.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr(FStrings.GetEntry(FWords.GetString(i)), s);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  index := L;
end;
    }
function TSnomedServices.ConceptExists(conceptId: String): Boolean;
var
  i : cardinal;
begin
  result := FConcept.FindConcept(StringToIdOrZero(conceptId), i);
end;

function TSnomedServices.condenseExpression(exp: TSnomedExpression): TFslList<TMatchingConcept>;
var
  grps : TFslList<TSnomedRefinementGroup>;
  ref : TSnomedRefinement;
  grp : TSnomedRefinementGroup;
begin
  grps := TFslList<TSnomedRefinementGroup>.Create;
  try
    grps.AddAll(exp.refinementGroups);
    for ref in exp.refinements do
    begin
      grp := TSnomedRefinementGroup.Create;
      grps.Add(grp);
      grp.refinements.Add(ref.Link);
    end;

    result := TFslList<TMatchingConcept>.Create;
    try
      if (exp.concepts.Count = 1) then
      begin
        if grps.Count = 0 then
          result.Add(TMatchingConcept.Create(exp.concepts[0].code))
        else
          findMatchingConcepts(result, exp.concepts[0].reference, grps);
      end;
      if result.Empty then
        raise ETerminologyError.create('No matches found for '+exp.ToString, itInvalid);
      result.link;
    finally
      result.free;
    end;
  finally
    grps.free;
  end;
end;

function TSnomedServices.FindStem(s: String; var index: Integer): Boolean;
var
  L, H, I, c: Integer;
begin
  Result := False;
  L := 0;
  H := FStems.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr(FStrings.GetEntry(FStems.GetString(i)), s);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  index := L;
end;

function TSnomedServices.GetActiveRoots: UInt64Array;
begin
  Result := FActiveRoots;
end;

function TSnomedServices.LastUseStatus: String;
begin
  if FLastUse = 0 then
    result := 'Never Used'
  else
    result := DescribePeriod(now - FLastUse);
end;

procedure TSnomedServices.RecordUse(count: integer);
begin
  FLock.lock('use');
  try
    FUseCount := FUseCount + count;
    FLastUse := now;
  finally
    FLock.unlock;
  end;
end;

function TSnomedServices.buildValueSet(factory : TFHIRFactory; url : String): TFhirValueSetW;
var
  inc : TFhirValueSetComposeIncludeW;
  filt :  TFhirValueSetComposeIncludeFilterW;
  cc : TFhirValueSetComposeIncludeConceptW;
  i : integer;
  code, iDummy : Cardinal;
  id : String;
begin
  // is this a correct reference?
  if (url.StartsWith('http://snomed.info/sct?fhir_vs')) then
    id := url.Substring(url.IndexOf('?'))
  else if (url.StartsWith('http://snomed.info/sct/'+FEdition+'?fhir_vs')) then
    id := url.Substring(url.IndexOf('?'))
  else if (url.StartsWith('http://snomed.info/sct/'+FEdition+'/version/'+FVersion+'?fhir_vs')) then
    id := url.Substring(url.IndexOf('?'))
  else
    exit(nil);

  if id = '?fhir_vs=refset' then
  begin
    result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
    try
      result.url := url;
      result.status := psActive;
      result.version := VersionDate;
      result.name := 'SNOMEDCTReferenceSetList';
      result.title := 'SNOMED CT Reference Set List';
      result.description := 'Reference Sets defined in this SNOMED-CT version';
      result.date := TFslDateTime.makeUTC;
      inc := result.addInclude;
      try
        inc.systemUri := URI_SNOMED;
        // get the list of reference sets
        for i := 0 to RefSetIndex.Count - 1 Do
        begin
          cc := inc.addConcept;
          try
            RefSetIndex.GetReferenceSet(i, code, iDummy, iDummy, iDummy, iDummy, iDummy, iDummy, iDummy);
            cc.code := GetConceptId(code);
          finally
            cc.free;
          end;
        end;
      finally
        inc.free;
      end;
      result.link;
    finally
      result.free;
    end;
  end
  else if id = '?fhir_vs' then
  begin
    result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
    try
      result.url := url;
      result.status := psActive;
      result.version := VersionDate;
      result.name := 'ALLSNOMEDCT';
      result.title := 'SNOMED CT Reference Set (All of SNOMED CT)';
      result.description := 'SNOMED CT Reference Set (All of SNOMED CT)';
      result.date := TFslDateTime.makeUTC;
      inc := result.addInclude;
      try
        inc.systemUri := URI_SNOMED;
      finally
        inc.free;
      end;
      result.link;
    finally
      result.free;
    end;
  end
  else if id.StartsWith('?fhir_vs=refset/') And ReferenceSetExists(id.Substring(16)) then
  begin
    result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
    try
      result.url := url;
      result.status := psActive;
      result.version := VersionDate;
      result.name := 'SNOMEDCTRefSet'+id.Substring(16);
      result.title := 'SNOMED CT Reference Set '+id.Substring(16);
      result.description := GetDisplayName(id.Substring(16), '');
      result.date := TFslDateTime.makeUTC;
      inc := result.addInclude;
      try
        inc.systemUri := URI_SNOMED;
        filt := inc.addFilter;
        try
          filt.prop := 'concept';
          filt.op := foIn;
          filt.value := id.Substring(16);
        finally
          filt.free;
        end;
      finally
        inc.free;
      end;
      result.link;
    finally
      result.free;
    end;
  end
  else if id.StartsWith('?fhir_vs=isa/') And ConceptExists(id.Substring(13)) then
  begin
    result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
    try
      result.url := url;
      result.status := psActive;
      result.version := VersionDate;
      result.name := 'SNOMEDCTConcept'+id.Substring(13);
      result.title := 'SNOMED CT Concept '+id.Substring(13)+' and descendants';
      result.description := 'All Snomed CT concepts for '+GetDisplayName(id.Substring(13), '');
      result.date := TFslDateTime.makeUTC;
      inc := result.addInclude;
      inc.systemUri := URI_SNOMED;
      filt := inc.addFilter;
      filt.prop := 'concept';
      filt.op := foIsA;
      filt.value := id.Substring(13);
      result.link;
    finally
      result.free;
    end;
  end
  else
    result := nil;
end;


function TSnomedServices.Link: TSnomedServices;
begin
  result := TSnomedServices(inherited link);
end;

function TSnomedServices.GetConceptRefSet(iConcept: Cardinal; bByName : Boolean; var iName, iMembers, iTypes, iFieldNames : cardinal): Cardinal;
var
  i : integer;
  c : Cardinal;
  iFilename, iDummy, iLangs : cardinal;
begin
  result := 0;
  For i := 0 to FRefSetIndex.Count - 1 do
  Begin
    if bByName Then
      FRefSetIndex.GetReferenceSet(i, iName, iFilename, c, iDummy, iMembers, iTypes, iFieldNames, iLangs)
    else
      FRefSetIndex.GetReferenceSet(i, iName, iFilename, c, iMembers, iDummy, iTypes, iFieldNames, iLangs);
    if c = iConcept Then
    Begin
      result := c;
      exit;
    End;
  End;
end;

function TSnomedServices.GetDesc: TSnomedDescriptions;
begin
  result := FDesc;
end;

function TSnomedServices.GetDescRef: TSnomedDescriptionIndex;
begin
  result := FDescRef;
end;

function TSnomedServices.GetDescRefsets(iDesc: Cardinal): TRefSetMemberEntryArray;
var
  i : integer;
  iDefinition, iFilename, iMembersByRef, iMembersByName, iLangs: Cardinal;
  aMembers : TSnomedReferenceSetMemberArray;
  iIndex : Integer;
  iName, iTypes, iFieldNames : Cardinal;
begin
  SetLength(result, 0);
  SetLength(aMembers, 0);
  for i := 0 to FRefSetIndex.Count - 1 Do
  Begin
    FRefSetIndex.GetReferenceSet(i, iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iTypes, iFieldNames, iLangs);
    aMembers := FRefSetMembers.GetMembers(iMembersByRef);
    if FindMember(aMembers, iDesc, iIndex) Then
    begin
      SetLength(result, length(result)+1);
      result[length(result)-1].refset := iDefinition;
      result[length(result)-1].types := iTypes;
      result[length(result)-1].values := aMembers[iIndex].values;
    End;
  End;
end;

function TSnomedServices.GetDescriptionId(const iDesc: Cardinal): String;
var
  descNdx : Cardinal;
  id : UInt64;
  date : TSnomedDate;
  concept, module, kind, caps, refsets, valueses : Cardinal;
  active : Boolean;
  lang : byte;
begin
  Desc.GetDescription(iDesc, descNdx, id, date, concept, module, kind, caps, refsets, valueses, active, lang);
  result := inttostr(id);
end;

function TSnomedServices.GetConceptRefsets(iDesc: Cardinal): TRefSetMemberEntryArray;
var
  i : integer;
  iDefinition, iFilename, iMembersByRef, iMembersByName, iLangs: Cardinal;
  aMembers : TSnomedReferenceSetMemberArray;
  iIndex : Integer;
  iName, iTypes, iFieldNames : Cardinal;
begin
  SetLength(result, 0);
  SetLength(aMembers, 0);
  for i := 0 to FRefSetIndex.Count - 1 Do
  Begin
    FRefSetIndex.GetReferenceSet(i, iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iTypes, iFieldNames, iLangs);
    aMembers := FRefSetMembers.GetMembers(iMembersByRef);
    if FindMember(aMembers, iDesc, iIndex) Then
    begin
      SetLength(result, length(result)+1);
      result[length(result)-1].refset := iDefinition;
      result[length(result)-1].types := iTypes;
      result[length(result)-1].values := aMembers[iIndex].values;
    End;
  End;
end;

procedure TSnomedServices.SetVersionUri(const Value: String);
begin
  FVersionUri := Value;
  FEditionUri := Value.Substring(0, value.IndexOf('/version/'));
end;

function TSnomedServices.StringIsId(const s: String; var iId: UInt64): Boolean;
begin
  result := TryStrToUINT64(s, iId);
end;

function TSnomedServices.StringToId(const s: String): UInt64;
begin
  result :=  StrToUInt64(s);
end;

function TSnomedServices.CheckLangSet(sTerm: String): Cardinal;
var
  iId  : UInt64;
begin
  result := 0;
  if sTerm <> '' Then
  Begin
    if StringIsId(sterm, iId) And Concept.FindConcept(iId, result) Then
      result := FRefSetIndex.GetMembersByConcept(result, false);
    if result = 0 Then
      Raise ETerminologyError.Create('Unable to resolve the language reference set '+sTerm, itInvalid);
  End
end;

function TSnomedServices.ChildRelationshipCount: cardinal;
var
  i, j : integer;
  Identity : UInt64;
  Flags : Byte;
  Active, Defining : boolean;
  date : TSnomedDate;
  ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsetIndex : Cardinal;
  Group : integer;
  inbounds : TCardinalArray;
  iWork, iWork2, iWork3, iWork4, iWork5, iWork6 : Cardinal;
begin
  result := 0;
  for i := 0 to FConcept.Count - 1 do
  begin
    FConcept.GetConcept(i * CONCEPT_SIZE, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsetIndex);
    for j := 0 to High(Inbounds) Do
    begin
      inbounds := FRefs.GetReferences(InboundIndex);
      FRel.GetRelationship(Inbounds[i], Identity, iWork, iWork2, iWork3, iWork4, iWork5, iWork6, date, Active, Defining, Group);
      if (iWork3 = Is_a_Index) and Active Then
        inc(result);
    end;
  end;
end;

function TSnomedServices.GetConceptDescendants(index: Cardinal): TCardinalArray;
begin
  result := FRefs.GetReferences(FConcept.GetAllDesc(index));
end;

function TSnomedServices.GetConceptId(const iConcept : Cardinal): String;
var
  Identity : UInt64;
  Flags : Byte;
  Parents, Inbounds, outbounds, descriptions, refsets : Cardinal;
  date : TSnomedDate;
begin
  Concept.GetConcept(iConcept, Identity, Flags, date, Parents, Descriptions, Inbounds, outbounds, refsets);
  result := inttostr(Identity);
end;

function TSnomedServices.GetConceptParents(index: Cardinal): TCardinalArray;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  refsets : Cardinal;
  date : word;
begin
  Concept.GetConcept(index, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  if ParentIndex = 0 Then
    SetLength(result, 0)
  else
    result := Refs.GetReferences(ParentIndex);
end;

function TSnomedServices.GetConcept: TSnomedConceptList;
begin
  result := FConcept;
end;

function TSnomedServices.GetConceptChildren(index: Cardinal): TCardinalArray;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  refsets, c : Cardinal;
  date : word;
  inbounds : TCardinalArray;
  Source, Target, RelType, module, kind, modifier : Cardinal;
  Active, Defining : Boolean;
  Group : Integer;
begin
  Concept.GetConcept(index, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  SetLength(result, 0);
  if InboundIndex > 0 then
  begin
    inbounds := Refs.GetReferences(InboundIndex);
    for c in inbounds do
    begin
      Rel.GetRelationship(c, identity, Source, Target, RelType, module, kind, modifier, date, Active, Defining, Group);
      if (group = 0) and active and (relType = Is_a_Index) then
      begin
        SetLength(result, length(result)+1);
        result[length(result)-1] := source;
      end;
    end;
  end;
end;

function TSnomedServices.StringToIdOrZero(const s: String): UInt64;
begin
  if StringIsInteger64(s) then
    result := StrToUInt64(s)
  Else
    result := 0;
end;

function TSnomedServices.IsValidConcept(const sTerm: String): Boolean;
var
  iTerm : Cardinal;
begin
  if (sTerm = '') or not StringIsInteger64(sTerm) then
    result := false
  else
    result := Concept.FindConcept(StringToId(sTerm), iTerm);
end;

function TSnomedServices.IsValidDescription(const sTerm: String; var concept: UInt64; var description: String): Boolean;
var
  iTerm, desc, cId, module, kind, caps, valueses, refsets : Cardinal;
  id : UInt64;
  date : TSnomedDate;
  active : boolean;
  lang : byte;
begin
  result := (sTerm <> '') and DescRef.FindDescription(StringToId(sTerm), iTerm);
  if result then
  begin
    FDesc.GetDescription(iTerm, desc, id, date, cId, module, kind, caps, refsets, valueses, active, lang);
    concept := FConcept.getConceptId(cId);
    description := FStrings.GetEntry(desc);
  end;
end;

function TSnomedServices.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FSourceFile.length * sizeof(char)) + 12);
  inc(result, (FEdition.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, FStrings.sizeInBytes(magic));
  inc(result, FRefs.sizeInBytes(magic));
  inc(result, FDesc.sizeInBytes(magic));
  inc(result, FDescRef.sizeInBytes(magic));
  inc(result, FConcept.sizeInBytes(magic));
  inc(result, FRel.sizeInBytes(magic));
  inc(result, FRefSetIndex.sizeInBytes(magic));
  inc(result, FRefSetMembers.sizeInBytes(magic));
  inc(result, (FVersionUri.length * sizeof(char)) + 12);
  inc(result, (FVersionDate.length * sizeof(char)) + 12);
  inc(result, FWords.sizeInBytes(magic));
  inc(result, FStems.sizeInBytes(magic));
  inc(result, (FEditionUri.length * sizeof(char)) + 12);
  inc(result, (FEditionId.length * sizeof(char)) + 12);
  inc(result, (FEditionName.length * sizeof(char)) + 12);
end;

{ TSnomedRelationshipList }

procedure TSnomedRelationshipList.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

Function TSnomedRelationshipList.AddRelationship(identity : UInt64; Source, Target, RelType, module, kind, modifier : Cardinal; date : TSnomedDate; Active, Defining : Boolean; Group : integer) : Cardinal;
begin
  Result := FBuilder.Length;
  FBuilder.AddCardinal(Source);
  FBuilder.AddCardinal(Target);
  FBuilder.AddCardinal(RelType);
  FBuilder.AddCardinal(module);
  FBuilder.AddCardinal(kind);
  FBuilder.AddCardinal(modifier);
  FBuilder.AddWord(date);
  FBuilder.Append(byte(active));
  FBuilder.Append(byte(Defining));
  FBuilder.AddInteger(Group);
  FBuilder.AddUInt64(identity);
End;

procedure TSnomedRelationshipList.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

function TSnomedRelationshipList.count: cardinal;
begin
  result := FLength div RELATIONSHIP_SIZE;
end;

procedure TSnomedRelationshipList.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

procedure TSnomedRelationshipList.GetRelationship(iIndex: Cardinal; out identity : UInt64; out Source, Target, RelType, module, kind, modifier : Cardinal; out date : TSnomedDate; out Active, Defining : Boolean; out Group : integer);
// (iIndex: Cardinal; var Source, Target, RelType: Cardinal; var Flags, Group : Byte);
begin
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index getting snomed relationship Details', itException);
  Move(FMaster[iIndex+0], Source, 4);
  Move(FMaster[iIndex+4], Target, 4);
  Move(FMaster[iIndex+8], RelType, 4);
  Move(FMaster[iIndex+12], module, 4);
  Move(FMaster[iIndex+16], kind, 4);
  Move(FMaster[iIndex+20], modifier, 4);
  Move(FMaster[iIndex+24], date, 2);
  Move(FMaster[iIndex+26], Active, 1);
  Move(FMaster[iIndex+27], Defining, 1);
  Move(FMaster[iIndex+28], Group, 4);
  Move(FMaster[iIndex+32], identity, 8);
end;

function TSnomedRelationshipList.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

{ TSnomedWords }

procedure TSnomedWords.AddWord(index: Cardinal; Flags: Byte);
begin
  FBuilder.AddCardinal(index);
  FBuilder.Append(flags);
end;

procedure TSnomedWords.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

function TSnomedWords.Count: Integer;
begin
  result := FLength div 5;
end;

procedure TSnomedWords.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

procedure TSnomedWords.GetEntry(iIndex: Cardinal; var index: Cardinal; var flags: Byte);
var
  l : Cardinal;
begin
  l := (iIndex * 5) + 1;
  if l > FLength - 4 Then
    raise ETerminologyError.create('invalid index', itException);
  move(FMaster[l], index, 4);
  move(FMaster[l+4], flags, 1);
end;

function TSnomedWords.GetString(iIndex: Cardinal): Cardinal;
var
  f : byte;
begin
  GetEntry(iIndex, result, f);
end;

procedure TSnomedWords.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TSnomedWords.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

{ TSnomedStems }

procedure TSnomedStems.AddStem(index: Cardinal; reference : Cardinal);
begin
  FBuilder.AddCardinal(index);
  FBuilder.AddCardinal(reference);
end;

procedure TSnomedStems.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

function TSnomedStems.Count: Integer;
begin
  result := FLength div 8;
end;

procedure TSnomedStems.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

procedure TSnomedStems.GetEntry(iIndex: Cardinal; var index: Cardinal; var reference: Cardinal);
var
  l : Cardinal;
begin
  l := (iIndex * 8);
  if l > FLength - 7 Then
    raise ETerminologyError.create('invalid index', itException);
  move(FMaster[l], index, 4);
  move(FMaster[l+4], reference, 4);
end;

function TSnomedStems.GetString(iIndex: Cardinal): Cardinal;
var
  f : cardinal;
begin
  GetEntry(iIndex, result, f);
end;

procedure TSnomedStems.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TSnomedStems.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

{ TSnomedServiceList }

destructor TSnomedServiceList.Destroy;
begin
  FDefinition.free;
  inherited;
end;

function TSnomedServiceList.GetDefinition(iIndex: Integer): TSnomedServices;
begin
  result := TSnomedServices(ObjectByIndex[iIndex]);
end;

function TSnomedServiceList.GetDefinitionByName(sName: String): TSnomedServices;
var
  i : integer;
begin
  if sName = '' then
    result := DefaultDefinition
  Else
  Begin
    Result := nil;
    i := 0;
    While (i < Count) and (result = nil) do
    Begin
      if SameText(Definition[i].VersionUri, sName) then
        result := Definition[i];
      inc(i);
    End;
  End;
end;

function TSnomedServiceList.HasDefaultDefinition: Boolean;
begin
  result := FDefinition <> Nil;
end;

function TSnomedServiceList.ItemClass: TFslObjectClass;
begin
  result := TSnomedServices;
end;

procedure TSnomedServiceList.SetDefinition(const Value: TSnomedServices);
begin
  FDefinition.free;
  FDefinition := Value;
end;

function TSnomedServiceList.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FDefinition.sizeInBytes(magic));
end;

{ TSnomedReferenceSetIndex }

procedure TSnomedReferenceSetIndex.AddReferenceSet(iName, iFilename,
  iDefinition, iMembersByRef, iMembersByName, iFieldTypes,
  iFieldNames, iLangs: Cardinal);
begin
  FBuilder.AddCardinal(iDefinition);
  FBuilder.AddCardinal(iFilename);
  FBuilder.AddCardinal(iMembersByRef);
  FBuilder.AddCardinal(iMembersByName);
  FBuilder.AddCardinal(iFieldTypes);
  FBuilder.AddCardinal(iName);
  FBuilder.AddCardinal(iFieldNames);
  FBuilder.AddCardinal(iLangs);
end;

procedure TSnomedReferenceSetIndex.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

procedure TSnomedReferenceSetIndex.SetHasLangs(AValue: boolean);
begin
  FHasLangs:=AValue;
  if FHasLangs then
    FRecordSize := REFSET_SIZE_LANG
  else
    FRecordSize := REFSET_SIZE_NOLANG;
end;

function TSnomedReferenceSetIndex.Count: Integer;
begin
  result := FLength div FRecordSize;
end;

procedure TSnomedReferenceSetIndex.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

function TSnomedReferenceSetIndex.GetMembersByConcept(iIndex: Cardinal; bByName : Boolean): Cardinal;
var
  i, v : Cardinal;
begin
  result := 0;
  For i := 0 to Count - 1 Do
  begin
    Move(FMaster[i * FRecordSize], v, 4);
    if v = iIndex Then
    Begin
      if bByName Then
        Move(FMaster[i * FRecordSize + 8], result, 4)
      Else
        Move(FMaster[i * FRecordSize + 4], result, 4);
      exit;
    End;
  End;
end;

procedure TSnomedReferenceSetIndex.GetReferenceSet(iIndex: Cardinal; out iName,
  iFilename, iDefinition, iMembersByRef, iMembersByName, iFieldTypes,
  iFieldNames, iLangs: Cardinal);
begin
  iIndex := iIndex * FRecordSize;
  if (iIndex >= FLength) then
    Raise ETerminologyError.Create('Wrong length index getting snomed relationship Details', itException);
  Move(FMaster[iIndex+0], iDefinition, 4);
  Move(FMaster[iIndex+4], iFilename, 4);
  Move(FMaster[iIndex+8], iMembersByRef, 4);
  Move(FMaster[iIndex+12], iMembersByName, 4);
  Move(FMaster[iIndex+16], iFieldTypes, 4);
  Move(FMaster[iIndex+20], iName, 4);
  Move(FMaster[iIndex+24], iFieldNames, 4);
  if (FHasLangs) then
    Move(FMaster[iIndex+28], iLangs, 4)
  else
    iLangs := 0;
end;

function TSnomedReferenceSetIndex.GetRefSetByConcept(iIndex: Cardinal): Cardinal;
var
  i, v : Cardinal;
begin
  result := 0;
  For i := 0 to Count - 1 Do
  begin
    Move(FMaster[i * FRecordSize], v, 4);
    if v = iIndex Then
      exit(i);
  End;
end;

procedure TSnomedReferenceSetIndex.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TSnomedReferenceSetIndex.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

{ TSnomedDescriptionIndex }

procedure TSnomedDescriptionIndex.AddDescription(id: UInt64; reference: Cardinal);
begin
  FBuilder.AddUInt64(id);
  FBuilder.AddCardinal(reference);
end;

procedure TSnomedDescriptionIndex.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

procedure TSnomedDescriptionIndex.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

function TSnomedDescriptionIndex.FindDescription(iIdentity: UInt64; out IIndex: Cardinal): boolean;
var
  aConcept : UInt64;
  L, H, I: Integer;
begin
  Result := False;
  L := 0;
  H := FLength;
  H := (H div 12) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Move(FMaster[(i*12)], aConcept, 8);
    if aConcept < iIdentity then L := I + 1 else
    begin
      H := I - 1;
      if aConcept = iIdentity then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  if result then
    Move(FMaster[L*12+8], iIndex, 4);
end;

procedure TSnomedDescriptionIndex.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TSnomedDescriptionIndex.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

{ TSnomedReferenceSetMembers }

function TSnomedReferenceSetMembers.AddMembers(ids : boolean; const a: TSnomedReferenceSetMemberArray): Cardinal;
var
  iLoop : Integer;
  b : byte;
  l : cardinal;
Begin
  result := FBuilder.Length;
  if (ids) then
    b := 1
  else
    b := 0;
  l := length(a);
  FBuilder.AddCardinal(l);
  FBuilder.Append(b);
  for iLoop := Low(a) to High(a) Do
  begin
    if (ids) then
    begin
      FBuilder.Append(@a[iLoop].id, 16);
      FBuilder.AddCardinal(a[iLoop].module);
      FBuilder.AddWord(a[iLoop].date);
    end;
    FBuilder.Append(a[iLoop].kind);
    FBuilder.AddCardinal(a[iLoop].Ref);
    FBuilder.AddCardinal(a[iLoop].Values);
  end;
end;

procedure TSnomedReferenceSetMembers.clear;
begin
  SetLength(FMaster, 0);
  FLength := 0;
end;

procedure TSnomedReferenceSetMembers.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.free;
end;

function TSnomedReferenceSetMembers.GetMemberCount(iIndex : Cardinal) : cardinal;
begin
  if iIndex = MAGIC_NO_CHILDREN then
    result := 0
  Else
  Begin
    if (iIndex > FLength) then
      Raise ETerminologyError.Create('Wrong length index getting Snomed list', itException);
    move(FMaster[iIndex], result, 4);
  End;
end;

function TSnomedReferenceSetMembers.GetMembers(iIndex: Cardinal): TSnomedReferenceSetMemberArray;
var
  i : integer;
  c : Cardinal;
  ids : boolean;
begin
  if iIndex = MAGIC_NO_CHILDREN then
    result := nil
  Else
  Begin
    if (iIndex > FLength) then
      Raise ETerminologyError.Create('Wrong length index getting Snomed list. asked for '+inttostr(iIndex)+', limit is '+inttostr(FLength), itException);
    move(FMaster[iIndex], c, 4);
    SetLength(Result, c);
    inc(iIndex, 4);
    move(FMaster[iIndex], ids, 1);
    inc(iIndex, 1);
    for i := 0 to Length(result)-1 Do
    Begin
      if (ids) then
      begin
        move(FMaster[iIndex], result[i].id, 16);
        inc(iIndex, 16);
        move(FMaster[iIndex], result[i].module, 4);
        inc(iIndex, 4);
        move(FMaster[iIndex], result[i].date, 2);
        inc(iIndex, 2);
      end;
      move(FMaster[iIndex], result[i].kind, 1);
      inc(iIndex, 1);
      move(FMaster[iIndex], result[i].Ref, 4);
      inc(iIndex, 4);
      move(FMaster[iIndex], result[i].values, 4);
      inc(iIndex, 4);
    End;
  End;
end;

procedure TSnomedReferenceSetMembers.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TSnomedReferenceSetMembers.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, length(FMaster));
  inc(result, FBuilder.sizeInBytes(magic));
end;

function TSnomedServices.ConceptExists(conceptId: String; var index: cardinal): Boolean;
begin
  result := (conceptId <> '') and FConcept.FindConcept(StringToIdOrZero(conceptId), index);
end;


function TSnomedServices.GetDefaultLanguage: Cardinal;
begin
  Result := FDefaultLanguage;
end;

function TSnomedServices.GetDefiningRelationships(index: Cardinal): TCardinalArray;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  refsets : Cardinal;
  date : word;
  outbounds : TCardinalArray;
  i, t : cardinal;
  Group : integer;
  iWork, iWork2, iWork3, kind, module, modifier : Cardinal;
  did : UInt64;
  Active, Defining : boolean;
begin
  Concept.GetConcept(index, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  if InboundIndex = 0 Then
    SetLength(result, 0)
  else
  begin
    SetLength(result, 100);
    t := 0;
    outbounds := Refs.GetReferences(OutboundIndex);
    for i in outbounds do
    begin
      Rel.GetRelationship(i, did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
      if Active and (iWork3 <> Is_a_Index) and (Defining) then
      begin
        result[t] := i;
        inc(t);
      end;
    end;
    SetLength(result, t);
  end;
end;


(*
not used?

Function TSnomedServices.GetFSN(iDescriptions : TCardinalArray) : String;
var
  iLoop : Integer;
  iid : UInt64;
  iString, iDummy, module, refsets, kind : Cardinal;
  iFlag : Byte;
  date : TSnomedDate;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy, module, kind, refsets, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE = VAL_DESC_FullySpecifiedName shl 4) Then
      result := Strings.GetEntry(iString);
  End;
End;


Function TSnomedServices.GetPN(iDescriptions : TCardinalArray) : String;
var
  iLoop : Integer;
  iid : UInt64;
  iString, iDummy, module, refsets, kind : Cardinal;
  date : TSnomedDate;
  iFlag : Byte;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy, module, kind, refsets, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) Then
      result := Strings.GetEntry(iString);
  End;
  if result = '' Then
    result := GetFSN(iDescriptions);
End;
*)

function TSnomedServices.IsActive(index: Cardinal): boolean;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  refsets : Cardinal;
  date : word;
begin
  Concept.GetConcept(index, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  result := (flags and MASK_CONCEPT_STATUS in [FLAG_Active, FLAG_PendingMove]);
end;

function TSnomedServices.isPrimitive(index: Cardinal): boolean;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  refsets : Cardinal;
  date : word;
begin
  Concept.GetConcept(index, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  result := (flags and MASK_CONCEPT_PRIMITIVE > 0);
end;

function TSnomedServices.filterEquals(id : UInt64): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
  index : cardinal;
begin
  res := TSnomedFilterContext.Create;
  try
    if not Concept.FindConcept(id, index) then
      raise ETerminologyError.Create('The SNOMED CT Concept '+inttostr(id)+' is not known', itInvalid);
    Setlength(res.descendants, 1);
    res.descendants[0] := index;
    result := TSnomedFilterContext(res.link);
  finally
    res.free;
  end;
end;

function TSnomedServices.filterIsA(id : UInt64; includeBase : boolean): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
  index : cardinal;
begin
  res := TSnomedFilterContext.Create;
  try
    if not Concept.FindConcept(id, index) then
      raise ETerminologyError.Create('The SNOMED CT Concept '+inttostr(id)+' is not known', itInvalid);
    if includeBase then
      res.descendants := mergeCardinals(index, GetConceptDescendants(index))
    else
      res.descendants := GetConceptDescendants(index);
    result := TSnomedFilterContext(res.link);
  finally
    res.free;
  end;
end;

function TSnomedServices.filterIn(id : UInt64): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
  name, index, members, types, iFieldNames : cardinal;
begin
  res := TSnomedFilterContext.Create;
  try
    if not Concept.FindConcept(id, index) then
      raise ETerminologyError.Create('The SNOMED CT Concept '+inttostr(id)+' is not known', itInvalid);
    if GetConceptRefSet(index, false, name, members, types, iFieldNames) = 0 then
      raise ETerminologyError.Create('The SNOMED CT Concept '+inttostr(id)+' is not a reference set', itInvalid);
    res.members := RefSetMembers.GetMembers(members);
    result := TSnomedFilterContext(res.link);
  finally
    res.free;
  end;
end;

function TSnomedServices.GetPNForConcept(iIndex: Cardinal): String;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex, refsets : Cardinal;
  Descriptions : TCardinalArray;
  date : TSnomedDate;
Begin
  Concept.GetConcept(iIndex, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  Descriptions := Refs.GetReferences(DescriptionIndex);
  result := GetPN(Descriptions);
End;

function TSnomedServices.GetRefs: TSnomedReferences;
begin
  result := FRefs;
end;

function TSnomedServices.getRefSet(id: int64): TSnomedReferenceSetMemberArray;
var
  name, index, members, types, iFieldNames : cardinal;
begin
  if not Concept.FindConcept(id, index) then
    raise ETerminologyError.Create('The SNOMED CT Concept '+inttostr(id)+' is not known', itInvalid);
  if GetConceptRefSet(index, false, name, members, types, iFieldNames) = 0 then
    raise ETerminologyError.Create('The SNOMED CT Concept '+inttostr(id)+' is not a reference set', itInvalid);
  result := RefSetMembers.GetMembers(members);
end;

function TSnomedServices.systemUri: String;
begin
  result := URI_SNOMED;
end;

function TSnomedServices.GetRefsetIndex: TSnomedReferenceSetIndex;
begin
  result := FRefSetIndex;
end;

function TSnomedServices.GetRefsetMembers: TSnomedReferenceSetMembers;
begin
  result := FRefSetMembers;
end;

function TSnomedServices.GetRel: TSnomedRelationshipList;
begin
  result := FRel;
end;

function TSnomedServices.GetRelationshipId(const iRel: Cardinal): String;
var
  identity : UInt64;
  Source, Target, RelType, module, kind, modifier : Cardinal;
  date : TSnomedDate;
  Active, Defining : Boolean;
  Group : integer;
begin
  Rel.GetRelationship(iRel, identity, Source, Target, RelType, module, kind, modifier, date, Active, Defining, Group);
  result := inttostr(identity);
end;

function TSnomedServices.getRelationshipValues(index: cardinal): String;
var
  i, j : integer;
  iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iFieldTypes, iFieldNames, iLangs: Cardinal;
  members : TSnomedReferenceSetMemberArray;
  member : TSnomedReferenceSetMember;
  tl, vl : TCardinalArray;
begin
  result := '';
  for i := 0 to FRefSetIndex.Count - 1 do
  begin
    FRefSetIndex.GetReferenceSet(i, iName, iFilename, iDefinition, iMembersByRef, iMembersByName, iFieldTypes, iFieldNames, iLangs);
    members := FRefSetMembers.GetMembers(iMembersByRef);
    for member in members do
      if (member.kind = 2) and (member.Ref = index) then
      begin
        tl := Refs.GetReferences(iFieldTypes);
        vl := Refs.GetReferences(member.values);
        for j := 0 to length(tl) - 1 do
          case vl[j*2+1] of
            1 {concept} : result := result + ' '+GetConceptId(vl[j*2]);
            2 {desc}    : result := result + ' '+GetDescriptionId(vl[j*2]);
            3 {rel}     : result := result + ' '+GetRelationshipId(vl[j*2]);
            4 {integer} : result := result + ' '+inttostr(vl[j*2]);
            5 {string}  : result := result + ' '+Strings.GetEntry(vl[j*2]);
          else
            raise ETerminologyError.create('Unknown Cell Type '+inttostr(vl[j*2+1]), itException);
          end;
        result := result.trim;
        exit();
      end;
  end;
end;

function TSnomedServices.GetStems: TSnomedStems;
begin
  result := FStems;
end;

function TSnomedServices.GetStrings: TSnomedStrings;
begin
  result := FStrings;
end;

function TSnomedServices.GetWords: TSnomedWords;
begin
  result := FWords;
end;

function TSnomedServices.groupsMatch(a, b: TSnomedRefinementGroup): boolean;
var
  refs, reft, t : TSnomedRefinement;
  msg, e1, e2 : String;
begin
  for refs in a.refinements do
  begin
    reft := nil;
    for t in b.refinements do
      if refs.name.matches(t.name) then
        reft := t;
    if reft = nil then
      exit(false);
    e1 := renderExpression(refs.value, sroAsIs);
    e2 := renderExpression(reft.value, sroAsIs);
    if not expressionsEquivalent(refs.value, reft.value, msg) then
      exit(false);
  end;
  result := true;
end;

function TSnomedServices.parseExpression(source: String): TSnomedExpression;
var
  prsr : TSnomedExpressionParser;
begin
  prsr := TSnomedExpressionParser.Create;
  try
    result := prsr.parse(source);
    try
      checkExpr(result);
      result.Link;
    finally
      result.free;
    end;
  finally
    prsr.free;
  end;
end;

function TSnomedServices.expressionsEquivalent(a, b: TSnomedExpression; var msg: String): boolean;
var
  e1, e2 : TSnomedExpression;
begin
  e1 := a.canonical;
  try
    e2 := b.canonical;
    try
      msg := e1.matches(e2);
      result := msg = '';
    finally
      e2.free;
    end;
  finally
    e1.free;
  end;
end;

procedure TSnomedServices.createDefinedExpr(reference: Cardinal; exp: TSnomedExpression; ancestor : boolean);
var
  c, r : Cardinal;
  did : UInt64;
  iSource, iTarget, iType, kind, module, modifier : Cardinal;
  date : word;
  Group : integer;
  Active, Defining : boolean;
  ref : TSnomedRefinement;
  groups : TFslMap<TSnomedRefinementGroup>;
  grp : TSnomedRefinementGroup;
begin
  if isPrimitive(reference) then
  begin
    if not exp.hasConcept(reference) then
      exp.concepts.add(TSnomedConcept.Create(reference))
  end
  else
  begin
    for c in GetConceptParents(reference) do
      createDefinedExpr(c, exp, true);
    if not ancestor or not ASSUME_CLASSIFIED then
    begin
      groups := TFslMap<TSnomedRefinementGroup>.create('Snomed.refinements');
      try
        for r in GetDefiningRelationships(reference) do
        begin
          Rel.GetRelationship(r, did, iSource, iTarget, iType, module, kind, modifier, date, Active, Defining, Group);
          ref := TSnomedRefinement.Create;
          try
            ref.name := TSnomedConcept.Create(iType);
            ref.value := TSnomedExpression.Create;
            ref.value.concepts.Add(TSnomedConcept.Create(iTarget));
            if (group = 0) then
            begin
              if not exp.hasRefinement(ref) then
                exp.refinements.Add(ref.Link);
            end
            else
            begin
              if not groups.ContainsKey(inttostr(group)) then
                groups.Add(inttostr(group), TSnomedRefinementGroup.Create);
              groups[inttostr(group)].refinements.Add(ref.Link);
            end;
          finally
            ref.free;
          end;
        end;
        for grp in groups.Values do
          if not exp.hasRefinementGroup(grp) then
            exp.refinementGroups.Add(grp.Link);
      finally
        groups.free;
      end;
    end;
  end;
end;

function TSnomedServices.createNormalForm(reference: Cardinal): TSnomedExpression;
var
  exp : TSnomedExpression;
  c : cardinal;
  parser : TSnomedExpressionParser;
begin
  if FBuilding then
  begin
    exp := TSnomedExpression.Create;
    try
      createDefinedExpr(reference, exp, false);
      result := normaliseExpression(exp);
    finally
      exp.free;
    end;
  end
  else
  begin
    c := Concept.GetNormalForm(reference);
    parser := TSnomedExpressionParser.Create;
    try
      if c = 0 then
        result := parser.parse(GetConceptId(reference))
      else
        result := parser.parse(Strings.GetEntry(c));
      try
        checkExpr(result);
        result.Link;
      finally
        result.free;
      end;
    finally
      parser.free;
    end;
  end;
end;

//function TSnomedServices.debugExpr(expr: TSnomedConcept): String;
//var
//  b : TFslStringBuilder;
//begin
//  b := TFslStringBuilder.Create;
//  try
//    renderExpr(b, expr, sroFillMissing);
//    result := b.ToString;
//  finally
//    b.free;
//  end;
//end;
//
//function TSnomedServices.debugExpr(expr: TSnomedRefinement): String;
//var
//  b : TFslStringBuilder;
//begin
//  b := TFslStringBuilder.Create;
//  try
//    renderExpr(b, expr, sroFillMissing);
//    result := b.ToString;
//  finally
//    b.free;
//  end;
//end;

procedure TSnomedServices.rationaliseExpr(exp: TSnomedExpression);
var
  group, grp1, grp2 : TSnomedRefinementGroup;
  c1, c2 : TSnomedConcept;
  i, j : integer;
begin
  i := 0;
  while i < exp.concepts.Count do
  begin
    c1 := exp.concepts[i];
    j := i + 1;
    while j < exp.concepts.Count do
    begin
      c2 := exp.concepts[j];
      if (c1.reference <> NO_REFERENCE) and (c2.reference <> NO_REFERENCE)  then
      begin
        if subsumes(c1.reference, c2.reference) then
        begin
          c1.copyFrom(c2);
          exp.concepts.Remove(c2);
        end
        else if subsumes(c2.reference, c1.reference) then
          exp.concepts.Remove(c2)
        else
          inc(j);
      end
      else
        inc(j);
    end;
    inc(i);
  end;

  mergeRefinements(exp.refinements);
  for group in exp.refinementGroups do
    mergeRefinements(group.refinements);

  i := 0;
  while i < exp.refinementGroups.Count do
  begin
    grp1 := exp.refinementGroups[i];
    j := i + 1;
    while j < exp.refinementGroups.Count do
    begin
      grp2 := exp.refinementGroups[j];
      if mergeGroups(grp1, grp2) then
        exp.refinementGroups.Remove(grp2)
      else
        inc(j);
    end;
    inc(i);
  end;
end;

function debugArr(arr : TArray<Cardinal>) : String;
var
  b : TFslStringBuilder;
  c : cardinal;
  f : boolean;
begin
  b := TFslStringBuilder.Create;
  try
    f := true;
    for c in arr do
    begin
      if f then
        f := false
      else
        b.Append(',');
      b.Append(c);
    end;
    result := b.ToString;
  finally
    b.free;
  end;
end;

function TSnomedServices.mergeGroups(grp1, grp2: TSnomedRefinementGroup): boolean;
  function getRef(c : cardinal; list: TSnomedRefinementList) : TSnomedRefinement;
  var
    t : TSnomedRefinement;
  begin
    result := nil;
    for t in list do
      if t.name.reference = c then
        exit(t);
  end;
var
  matches : TArray<Cardinal>;
  targets : TArray<boolean>;
  t : integer;
  c : cardinal;
  ref1, ref2 : TSnomedRefinement;
  function matchIndex(c : cardinal) : integer;
  var
    i : integer;
  begin
    for i := 0 to length(matches) - 1 do
      if matches[i] = c then
        exit(i);
    result := -1;
  end;
begin
  // we want to merge the groups if
  // - At least one attribute in one of the groups is name-matched by an attribute in the other group
  SetLength(matches, IntegerMax(grp1.refinements.Count, grp2.refinements.Count));
  t := 0;
  for ref1 in grp1.refinements do
    for ref2 in grp2.refinements do
      if (ref1.name.reference = ref2.name.reference) then
      begin
        matches[t] := ref1.name.reference;
        inc(t);
        break; //don't look for more matches of this name
      end;
  if (t = 0) then
      exit(false);
  // - For each name-matched pair of attributes, the value of that attribute in one group either subsumes or is identical to the value of the name-matched attribute in the other group;
  setLength(matches, t);
  setLength(targets, t);
  t := 0;
  result := true;
  for c in matches do
  begin
    ref1 := getRef(c, grp1.refinements);
    ref2 := getRef(c, grp2.refinements);
    if (ref1 = nil) or (ref2 = nil) then
      raise ETerminologyError.create('No match for c = '+inttostr(c), itInvalid);
    if expressionSubsumes(ref1.value, ref2.value) then
      targets[t] := true
    else if expressionSubsumes(ref2.value, ref1.value) then
      targets[t] := false
    else
      result := false;
    inc(t);
  end;
  // - Groups that meet the criteria for merging are merged by adding all attributes present in both source groups to the same group in the merged target definition;
  if result then
  begin
    for ref1 in grp1.refinements do
    begin
      t := matchIndex(ref1.name.reference);
      if (t > -1) and targets[t] then
      begin
        ref2 := getRef(matches[t], grp2.refinements);
        ref1.value := ref2.value.Link;
      end;
    end;
    for ref2 in grp2.refinements do
    begin
      t := matchIndex(ref2.name.reference);
      if (t = -1) then
        grp1.refinements.Add(ref2.Link)
    end;
  end;
end;

procedure TSnomedServices.mergeRefinements(list : TSnomedRefinementList);
var
  i, j : integer;
  ref1, ref2 : TSnomedRefinement;
begin
  i := 0;
  while i < list.Count do
  begin
    ref1 := list[i];
    j := i + 1;
    while j < list.Count do
    begin
      ref2 := list[j];
      if ref1.name.matches(ref2.name) then
      begin
        if expressionSubsumes(ref1.value, ref2.value) then
        begin
          ref1.value := ref2.value.link;
          list.Remove(ref2);
        end
        else if expressionSubsumes(ref2.value, ref1.value) then
          list.Remove(ref2)
        else
          inc(j);
      end
      else
        inc(j);
    end;
    inc(i);
  end;
end;


function TSnomedServices.normaliseExpression(exp: TSnomedExpression): TSnomedExpression;
var
  concept : TSnomedConcept;
  ex, work, work2: TSnomedExpression;
  refSrc, refDst : TSnomedRefinement;
  grpSrc, grpDst : TSnomedRefinementGroup;
begin
  work := TSnomedExpression.Create;
  try
    for concept in exp.concepts do
    begin
      if (concept.reference = NO_REFERENCE) or isPrimitive(concept.reference) then
        work.concepts.Add(concept.Link)
      else
      begin
        ex := createNormalForm(concept.reference);
        try
          work.merge(ex);
        finally
          ex.free;
        end;
      end;
    end;
    for refSrc in exp.refinements do
    begin
      refDst := TSnomedRefinement.Create;
      work.refinements.add(refDst);
      refDst.name := refSrc.name.Link;
      refDst.value := normaliseExpression(refSrc.value);
    end;
    for grpSrc in exp.refinementGroups do
    begin
      grpDst := TSnomedRefinementGroup.Create;
      work.refinementGroups.add(grpDst);
      for refSrc in grpSrc.refinements do
      begin
        refDst := TSnomedRefinement.Create;
        grpDst.refinements.add(refDst);
        refDst.name := refSrc.name.Link;
        refDst.value := normaliseExpression(refSrc.value);
      end;
    end;

    work2 := work.canonical;
    try
      rationaliseExpr(work2);
      result := work2.canonical;
    finally
      work2.free;
    end;
  finally
    work.free;
  end;
end;

function TSnomedServices.normalise(s: String): String;
var
  b : TFslStringBuilder;
  ws : boolean;
  ch : char;
begin
  b := TFslStringBuilder.Create;
  try
    ws := false;
    for ch in s do
      if not ch.IsWhiteSpace then
      begin
        b.Append(ch.ToLower);
        ws := false;
      end
      else if not ws then
      begin
        ws := true;
        b.Append(' ');
      end;
    result := b.ToString;
  finally
    b.free;
  end;
end;

function TSnomedServices.expressionSubsumes(a, b: TSnomedExpression): boolean;
var
  e1, e2 : TSnomedExpression;
  c, ct : TSnomedConcept;
  r, rt : TSnomedRefinementGroup;
  ok : boolean;
begin
  // subsumes is true if all of the things you can say about a are also things you can say about b
  // b is allowed to say additional things; it's a separate issue as to whether those are coherent
  // (that may be handled elsewhere)

  if (a.isSimple and b.isSimple) then
    result := Subsumes(a.concepts[0].reference, b.concepts[0].reference)
  else
  begin
    e1 := normaliseExpression(a);
    try
      e2 := normaliseExpression(b);
      try

        // ok, now check the normal forms - everything that is said about a must be said about b

        // firstly, all the root concepts:
        for c in e1.concepts do
        begin
          ok := false;
          for ct in e2.concepts do
            if subsumesConcept(c, ct) then
              ok := true;
          if not ok then
            exit(false);
        end;

        // ok, now we look at the refinement groups
        for r in e1.refinementGroups do
        begin
          rt := findMatchingGroup(r, e2);
          if (rt = nil) or not (subsumesGroup(r, rt)) then
            exit(false);
        end;
        result := true;
      finally
        e2.free;
      end;
    finally
      e1.free;
    end;
  end;
end;

function TSnomedServices.subsumesConcept(a, b: TSnomedConcept): boolean;
begin
  // given concept a, does it subsume b?
  if a.matches(b) then
    exit(true);

  // it might do that directly:
  result := (a.reference <> NO_REFERENCE) and (b.reference <> NO_REFERENCE) and Subsumes(a.reference, b.reference);
end;

procedure TSnomedServices.findMatchingConcepts(list: TFslList<TMatchingConcept>; reference: cardinal; refinements: TFslList<TSnomedRefinementGroup>);
var
  children : TCardinalArray;
  child : Cardinal;
  exp : TSnomedExpression;
  allMatched, oneUnMatched : boolean;
  grp : TSnomedRefinementGroup;
  state : TSnomedRefinementGroupMatchState;
  ref : TSnomedRefinement;
  grps, grpNM : TFslList<TSnomedRefinementGroup>;
  s : String;
begin
  children := GetConceptChildren(reference);
  for child in children do
  begin
    s := GetConceptId(child);
    exp := TSnomedExpression.Create;
    try
      createDefinedExpr(child, exp, false);
      for ref in exp.refinements do
      begin
        grp := TSnomedRefinementGroup.Create;
        exp.refinementGroups.Add(grp);
        grp.refinements.add(ref.Link);
      end;
      exp.refinements.clear;

      allMatched := true;
      oneUnMatched := false;
      grps := TFslList<TSnomedRefinementGroup>.Create;
      try
        for grp in exp.refinementGroups do
        begin
          state := checkGroupStateInRefinements(grp, refinements, grps);
          if (state = gmsNoMatch) then
            oneUnMatched := true
          else if state <> gmsIdentical then
            allMatched := false;
        end;

        if (oneUnMatched) then
          // neither this nor any of the children will ever match
        else if (allMatched) and (grps.count > 0) then
        begin
          // this reference is a complete match. stop here
          list.Add(TMatchingConcept.Create(s));
        end
        else
        begin
          // get the list of non-matches
          grpNM := listNonMatchingGroups(refinements, grps);
          try
            if (grpNM.Count < refinements.Count) then
              list.Add(TMatchingConcept.Create(s, grpNM));
            findMatchingConcepts(list, child, grpNM);
          finally
            grpNM.free;
          end;
        end;
      finally
        grps.free;
      end;
    finally
      exp.free;
    end;
  end;
end;

function TSnomedServices.checkGroupStateInRefinements(grp : TSnomedRefinementGroup; refinements : TFslList<TSnomedRefinementGroup>; grps : TFslList<TSnomedRefinementGroup>) : TSnomedRefinementGroupMatchState;
var
  g : TSnomedRefinementGroup;
begin
  result := gmsNoMatch;
  for g in refinements do
  begin
    if groupsMatch(grp, g) then
    begin
      grps.add(g.link);
      exit(gmsIdentical);
    end
    else if subsumesGroup(grp, g) then
      exit(gmsSubsumed);
  end;
end;

function TSnomedServices.listNonMatchingGroups(tgt, src : TFslList<TSnomedRefinementGroup>) : TFslList<TSnomedRefinementGroup>;
var
  g, r : TSnomedRefinementGroup;
begin
  result := TFslList<TSnomedRefinementGroup>.Create;
  try
    for g in tgt do
    begin
      r := findMatchingGroup(g, src);
      if (r = nil) then
        result.Add(g.Link);
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TSnomedServices.findMatchingGroup(r : TSnomedRefinementGroup; exp : TSnomedExpression) : TSnomedRefinementGroup;
var
  t : TSnomedRefinementGroup;
  refs, reft : TSnomedRefinement;
  all, match : boolean;
begin
  //  a matching group is where the target contains all the attributes of the source group
  result := nil;
  for t in exp.refinementGroups do
  begin
    all := true;
    for refs in r.refinements do
    begin
      match := false;
      for reft in t.refinements do
        if refs.name.matches(reft.name) then
          match := true;
      if not match then
        all := false;
    end;
    if (all) then
      exit(t);
  end;
end;

function TSnomedServices.findMatchingGroup(r : TSnomedRefinementGroup; grps : TFslList<TSnomedRefinementGroup>) : TSnomedRefinementGroup;
var
  t : TSnomedRefinementGroup;
  refs, reft : TSnomedRefinement;
  all, match : boolean;
begin
  //  a matching group is where the target contains all the attributes of the source group
  result := nil;
  for t in grps do
  begin
    all := true;
    for refs in r.refinements do
    begin
      match := false;
      for reft in t.refinements do
        if refs.name.matches(reft.name) then
          match := true;
      if not match then
        all := false;
    end;
    if (all) then
      exit(t);
  end;
end;


function TSnomedServices.subsumesGroup(a, b : TSnomedRefinementGroup): boolean;
var
  refs, reft, t : TSnomedRefinement;
begin
  for refs in a.refinements do
  begin
    reft := nil;
    for t in b.refinements do
      if refs.name.matches(t.name) then
        reft := t;
    if reft = nil then
      exit(false);
    if not expressionSubsumes(refs.value, reft.value) then
      exit(false);
  end;
  result := true;
end;

procedure TSnomedServices.checkExpr(expression: TSnomedExpression);
var
  i, j : integer;
begin
  for i := 0 to expression.concepts.count - 1 do
     checkExpr(expression.concepts[i]);
  if expression.HasRefinements then
    for i := 0 to expression.refinements.count - 1 do
     checkExpr(expression.refinements[i]);
  if expression.HasRefinementGroups then
    for j := 0 to expression.refinementGroups.count - 1 do
      for i := 0 to expression.refinementGroups[j].refinements.count - 1 do
       checkExpr(expression.refinementGroups[j].refinements[i]);
end;

procedure TSnomedServices.checkExpr(concept: TSnomedConcept);
var
  list : TConceptDesignations;
  i : integer;
  ok : boolean;
  iTerm : Cardinal;
  d : String;
begin
  if (concept.code = '') then
    // nothing to check here...
  else if self.Concept.FindConcept(StringToId(concept.code), iTerm) Then
    concept.reference := iTerm
  else if (concept.code <> '111115') then
    raise ETerminologyError.Create('Concept '+concept.code+' not found in '+systemUri, itInvalid);

  if (concept.reference <> NO_REFERENCE) and (concept.description <> '') then
  begin
    list := TConceptDesignations.create(TFHIRFactoryX.create, FLanguages.link);
    try
      ListDisplayNames(list, iTerm, 0, $FF);
      d := normalise(concept.description);
      ok := (list.preferredDisplay(nil) <> '') and (normalise(list.preferredDisplay(nil)) = d);
      for i := 0 to list.designations.count - 1 do
        if not ok and (normalise(list.designations[i].value.asString) = d) then
        begin
          ok := true;
          break;
        end;
      if not ok then
        raise ETerminologyError.Create('Term "'+concept.description+'" doesn''t match a defined term at '+inttostr(concept.start)+' (valid terms would be from this list: "'+list.present(nil, nil, true)+'")', itInvalid);
    finally
      list.free;
    end;
  end;
end;

procedure TSnomedServices.checkExpr(refinement: TSnomedRefinement);
begin
  checkExpr(refinement.name);
  checkExpr(refinement.value);
end;

function TSnomedServices.renderExpression(source : TSnomedExpression; option : TSnomedServicesRenderOption): String;
var
  b : TFslStringBuilder;
begin
  b := TFslStringBuilder.Create;
  try
    RenderExpr(b, source, option);
    result := b.ToString;
  finally
    b.free;
  end;
end;

procedure TSnomedServices.renderExpr(b: TFslStringBuilder; expr: TSnomedExpression; option : TSnomedServicesRenderOption);
var
  i, j : integer;
begin
  for i := 0 to expr.concepts.Count - 1 do
  begin
    if (i > 0) then
      b.Append('+');
    renderExpr(b, expr.concepts[i], option);
  end;
  if (expr.hasrefinements) or (expr.HasRefinementGroups) then
  begin
    b.Append(':');
    if (expr.hasrefinements) then
      for i := 0 to expr.refinements.Count - 1 do
      begin
        if (i > 0) then
          b.Append(',');
        renderExpr(b, expr.refinements[i], option);
      end;
    if (expr.hasrefinementGroups) then
      for j := 0 to expr.refinementGroups.Count - 1 do
      begin
        if (j > 0) then
          b.Append(',');
        b.Append('{');
        for i := 0 to expr.refinementGroups[j].refinements.Count - 1 do
        begin
          if (i > 0) then
            b.Append(',');
          renderExpr(b, expr.refinementGroups[j].refinements[i], option);
        end;

        b.Append('}');
      end;
  end;
end;

//function TSnomedServices.debugExpr(expr: TSnomedExpression) : String;
//var
//  b : TFslStringBuilder;
//begin
//  b := TFslStringBuilder.Create;
//  try
//    renderExpr(b, expr, sroFillMissing);
//    result := b.ToString;
//  finally
//    b.free;
//  end;
//end;


procedure TSnomedServices.renderExpr(b: TFslStringBuilder; expr: TSnomedConcept;
  option: TSnomedServicesRenderOption);
var
  s : String;
begin
  if (expr.reference <> NO_REFERENCE) and (expr.code = '') then
  begin
    expr.code := GetConceptId(expr.reference);
    b.Append(expr.code)
  end
  else
    b.Append(expr.describe);
  s := '';
  case option of
    sroMinimal : s := '';
    sroAsIs : s := expr.description;
    sroFillMissing :
      begin
        s := expr.description;
        if (s = '') then
          if expr.reference <> NO_REFERENCE then
            s := GetDisplayName(expr.reference, FDefaultLanguage)
          else if (expr.code <> '') then
            s := GetDisplayName(expr.code, '');
      end;
    sroReplaceAll :
      if (expr.code <> '') then
        s := GetDisplayName(expr.code, '');
  end;
  if s <> '' then
  begin
    b.Append('|');
    b.Append(s);
    b.Append('|');
  end;
end;


procedure TSnomedServices.renderExpr(b: TFslStringBuilder;
  expr: TSnomedRefinement; option: TSnomedServicesRenderOption);
begin
  renderExpr(b, expr.name, option);
  b.Append('=');
  RenderExpr(b, expr.value, option);
end;


function TSnomedServices.displayExpression(source: TSnomedExpression): String;
var
  b : TFslStringBuilder;
begin
  b := TFslStringBuilder.Create;
  try
    DisplayExpr(b, source);
    result := b.ToString;
  finally
    b.free;
  end;
end;

procedure TSnomedServices.displayExpr(b: TFslStringBuilder; expr: TSnomedExpression
  );
var
  i, j : integer;
  done : boolean;
begin
  for i := 0 to expr.concepts.Count - 1 do
  begin
    if (i > 0) then
      b.Append(', ');
    if (i > 0) and (i = expr.concepts.Count - 1) then
      b.Append('and ');
    DisplayExpr(b, expr.concepts[i]);
  end;
  done := false;
  if (expr.hasrefinements) then
    for i := 0 to expr.refinements.Count - 1 do
    begin
      if (i = 0) then
      begin
        b.Append(' where ');
        done := true;
      end;
      if (i > 0) then
        b.Append(', ');
      if (i > 0) and (i = expr.refinements.Count - 1) then
        b.Append('and ');
      DisplayExpr(b, expr.refinements[i]);
    end;
  if (expr.hasrefinementGroups) then
    for j := 0 to expr.refinementGroups.Count - 1 do
    begin
      if not done and (j = 0) then
        b.Append(' where ');
      if (j > 0) then
        b.Append(', ');
      if (j > 0) and (j = expr.refinementGroups.Count - 1) then
        b.Append('and ');
      b.Append('(');
      for i := 0 to expr.refinementGroups[j].refinements.Count - 1 do
      begin
        if (i > 0) then
          b.Append(', ');
        if (i = expr.refinementGroups[j].refinements.Count - 1) then
          b.Append('and ');
        DisplayExpr(b, expr.refinementGroups[j].refinements[i]);
      end;

      b.Append(')');
    end;
end;

procedure TSnomedServices.displayExpr(b: TFslStringBuilder; expr: TSnomedConcept);
var
  s : String;
begin
  if (expr.code = '') then
    b.append('<err>')
  else
  begin
    s := GetDisplayName(expr.code, '');
    if (s = '') then
      s := expr.description;
    b.Append(s);
  end;
end;


procedure TSnomedServices.displayExpr(b: TFslStringBuilder; expr: TSnomedRefinement
  );
begin
  DisplayExpr(b, expr.name);
  b.Append(' = ');
  DisplayExpr(b, expr.value);
end;



{ TSnomedExpressionContext }

constructor TSnomedExpressionContext.create(source: String; reference: cardinal);
begin
  inherited Create;
  FSource := source;
  FExpression := TSnomedExpression.Create;
  FExpression.concepts.Add(TSnomedConcept.Create(reference))
end;

constructor TSnomedExpressionContext.Create(reference: cardinal);
begin
  inherited Create;
  FExpression := TSnomedExpression.Create;
  FExpression.concepts.Add(TSnomedConcept.Create(reference))
end;

constructor TSnomedExpressionContext.Create(source: String; expression: TSnomedExpression);
begin
  inherited Create;
  FSource := source;
  FExpression := expression;
end;

destructor TSnomedExpressionContext.Destroy;
begin
  FExpression.free;
  inherited;
end;

function TSnomedExpressionContext.isComplex: boolean;
begin
  result := FExpression.isComplex;
end;

function TSnomedExpressionContext.Link: TSnomedExpressionContext;
begin
  result := TSnomedExpressionContext(inherited Link);
end;

function TSnomedExpressionContext.reference: cardinal;
begin
  result := FExpression.concepts[0].reference;
end;

procedure TSnomedExpressionContext.SetExpression(value: TSnomedExpression);
begin
  FExpression.free;
  FExpression := value.Link;
end;

function readDate(s : String) : TSnomedDate;
var
  d : TDateTime;
begin
  assert(s.Length = 8, 'Date length is not 8 ('+s+')');
  s := copy(s, 7, 2)+'/'+copy(s, 5, 2)+'/'+copy(s, 1, 4);
  d := StrToDate(s, SNOMED_DATE_FORMAT);
  assert(trunc(d) < 65535);
  result := trunc(d);
end;


const
  verhoeff_d : array[0..9,0..9] of integer  = (
  (0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  (1, 2, 3, 4, 0, 6, 7, 8, 9, 5),
  (2, 3, 4, 0, 1, 7, 8, 9, 5, 6),
  (3, 4, 0, 1, 2, 8, 9, 5, 6, 7),
  (4, 0, 1, 2, 3, 9, 5, 6, 7, 8),
  (5, 9, 8, 7, 6, 0, 4, 3, 2, 1),
  (6, 5, 9, 8, 7, 1, 0, 4, 3, 2),
  (7, 6, 5, 9, 8, 2, 1, 0, 4, 3),
  (8, 7, 6, 5, 9, 3, 2, 1, 0, 4),
  (9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  );
// The permutation table
  verhoeff_p : array[0..7,0..9] of integer  = (
  (0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  (1, 5, 7, 6, 2, 8, 3, 0, 9, 4),
  (5, 8, 0, 3, 7, 9, 6, 1, 4, 2),
  (8, 9, 1, 6, 0, 4, 3, 5, 2, 7),
  (9, 4, 5, 3, 1, 2, 6, 8, 7, 0),
  (4, 2, 8, 6, 5, 7, 3, 9, 0, 1),
  (2, 7, 9, 3, 8, 0, 6, 4, 1, 5),
  (7, 0, 4, 6, 9, 1, 3, 2, 5, 8)
 );
 //The inverse table
  verhoeff_inv : array [0..9] of char = ('0', '4', '3', '2', '1', '5', '6', '7', '8', '9');

//For a given number generates a Verhoeff digit
function genCheckDigit(s : String): char;
var
  i, c, len : integer;
begin
  c := 0;
  s := s + '0';
  len := length(s);

  for i := len downto 1 do
    c := verhoeff_d[c][verhoeff_p[((len-i) mod 8)][ord(s[i]) - ord('0')]];

  result := verhoeff_inv[c];
end;

////Validates that an entered number is Verhoeff compliant.
////The check digit must be the last one.
procedure validateCheckDigit(s : String);
var
  i, c, len : integer;
begin
  c := 0;
  len := length(s);

  for i := len downto 1 do
    c := verhoeff_d[c][verhoeff_p[((len-i) mod 8)][ord(s[i]) - ord('0')]];

  if c <> 0 then
    raise ETerminologyError.create('Check digit error: "'+s+'" is not valid by Verhoeff algorithm', itInvalid);
end;

function readLang(s : String) : byte;
begin
  if (s = 'en') then
    result := 1
  else if (s = 'fr') then
    result := 2
  else if (s = 'nl') then
    result := 3
  else if (s = 'es') then
    result := 4
  else if (s = 'sv') then
    result := 5
  else if (s = 'da') then
    result := 6
  else if (s = 'de') then
    result := 7
  else if (s = 'it') then
    result := 8
  else if (s = 'cs') then
    result := 9
  else
    raise ETerminologyError.create('Unknown SCT Lang "'+s+'"', itInvalid);
end;

function codeForLang(lang : byte):String;
begin
  case lang of
    1 : result := 'en';
    2 : result := 'fr';
    3 : result := 'nl';
    4 : result := 'es';
    5 : result := 'sv';
    6 : result := 'da';
    7 : result := 'de';
    8 : result := 'it';
    9 : result := 'cs';
  else
    result := '??';
  end;
end;

function langForCode(s: String): byte;
begin
  if (s = 'en') or (s.startsWith('en-')) then
    result := 1
  else if (s = 'fr') or (s.startsWith('fr-')) then
    result := 2
  else if (s = 'nl') or (s.startsWith('nl-')) then
    result := 3
  else if (s = 'es') or (s.startsWith('es-')) then
    result := 4
  else if (s = 'sv') or (s.startsWith('sv-')) then
    result := 5
  else if (s = 'da') or (s.startsWith('da-')) then
    result := 6
  else if (s = 'de') or (s.startsWith('de-')) then
    result := 7
  else if (s = 'it') or (s.startsWith('it-')) then
    result := 8
  else if (s = 'cs') or (s.startsWith('cs-')) then
    result := 9
  else
    result := 0;
end;

function describeLangs(langs: integer): string;
begin
  result := '';
  if (langs and (1 shl 1) > 0) then CommaAdd(result, 'en');
  if (langs and (1 shl 2) > 0) then CommaAdd(result, 'fr');
  if (langs and (1 shl 3) > 0) then CommaAdd(result, 'nl');
  if (langs and (1 shl 4) > 0) then CommaAdd(result, 'es');
  if (langs and (1 shl 5) > 0) then CommaAdd(result, 'sv');
  if (langs and (1 shl 6) > 0) then CommaAdd(result, 'da');
  if (langs and (1 shl 7) > 0) then CommaAdd(result, 'de');
  if (langs and (1 shl 8) > 0) then CommaAdd(result, 'it');
  if (langs and (1 shl 9) > 0) then CommaAdd(result, 'cs');
end;

function TSnomedExpressionContext.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, FExpression.sizeInBytes(magic));
end;

{ TMatchingConcept }

constructor TMatchingConcept.Create(match : String);
begin
  inherited Create;
  FMatched := match;
  FUnmatched := TFslList<TSnomedRefinementGroup>.Create;
end;

constructor TMatchingConcept.Create(match : String; nonmatched: TFslList<TSnomedRefinementGroup>);
begin
  inherited Create;
  FMatched := match;
  FUnmatched := TFslList<TSnomedRefinementGroup>.Create;
  FUnmatched.AddAll(nonmatched);
end;

destructor TMatchingConcept.Destroy;
begin
  FUnmatched.free;
  inherited;
end;

function TMatchingConcept.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FMatched.length * sizeof(char)) + 12);
  inc(result, FUnmatched.sizeInBytes(magic));
end;


{ TSnomedProvider }

constructor TSnomedProvider.Create(sct: TSnomedServices; i18n : TI18nSupport; supplements: TFslList<
  TFHIRCodeSystemW>);
begin
  inherited Create(sct.FLanguages.link, i18n);
  FSct := sct;
  FSupplements := supplements;
end;

destructor TSnomedProvider.Destroy;
begin
  FSct.FUseCount := FSct.FUseCount + UseCount;
  FSct.free;
  FSupplements.free;
  inherited Destroy;
end;

function TSnomedProvider.description: String;
begin
  result := 'SNOMED CT '+FSCt.EditionName;
end;

procedure TSnomedProvider.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'concept:is-a'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'concept:descends'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'concept:in'));
end;

function TSnomedProvider.versionIsMoreDetailed(v1, v2: String): boolean;
begin
  result := (v2 <> '') and v2.startsWith(v1);
end;

function TSnomedProvider.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TSnomedProvider.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function TSnomedProvider.defToThisVersion(specifiedVersion: String): boolean;
var
  tv, sv : TArray<String>;
begin
  tv := versionSplit(FSct.VersionUri);
  sv := versionSplit(specifiedVersion);
  if (tv[0] <> sv[0]) then
    result := false
  else if (length(sv) = 0) then
    result := true
  else
    result := sv[1] < tv[1];
end;

function TSnomedProvider.searchFilter(opContext : TTxOperationContext; filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
begin
  res := TSnomedFilterContext.Create;
  try
    res.matches := FSct.Search(0, filter.filter, FSct.FDefaultLanguage, false, true);
    result := res.Link;
  finally
    res.free;
  end;
end;

procedure TSnomedProvider.getCDSInfo(opContext : TTxOperationContext; card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
var
  b : TFslStringBuilder;
  Identity : UInt64;
  Flags, bLang: Byte;
  Active, Defining : boolean;
  ParentIndex, iWork, iWork2, iWork3, module, modifier, kind, caps, iDummy : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex, InboundIndex2 : Cardinal;
  outboundIndex, valueses, refsets : Cardinal;
  Inbounds : TCardinalArray;
  date : TSnomedDate;
  Descriptions : TCardinalArray;
  Parents : TCardinalArray;
  i, group : integer;
  iId : UInt64;
  iIndex : cardinal;
  first : boolean;
  did : UInt64;
begin
  b := TFslStringBuilder.Create;
  try
    SetLength(inbounds, 0);
    iId := StrToUInt64Def(code, 0);
    if not FSct.Concept.FindConcept(iId, iIndex) then
    begin
      b.Append('Snomed Code '+code+#13#10#13#10);
      b.Append('* Error: Code not known')
    end
    else
    begin
      card.addLink('Further Detail', baseURL+'/snomed/doco/?srch='+code);

      FSct.Concept.GetConcept(IIndex, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
      Inbounds := FSct.Refs.GetReferences(InboundIndex);

      b.Append('Snomed Code '+code+' : '+FSct.GetPNForConcept(iIndex)+#13#10#13#10);

      Descriptions := FSct.Refs.GetReferences(DescriptionIndex);
      b.Append('Descriptions: '+#13#10#13#10);
      for i := Low(Descriptions) To High(Descriptions) Do
      Begin
        FSct.Desc.GetDescription(Descriptions[i], iWork, Identity, date, iDummy, module, kind, caps, refsets, valueses, active, bLang);
        if Active Then
          if (kind <> 0) then
            b.Append('* '+FSct.Strings.GetEntry(iWork)+' ('+FSct.GetPNForConcept(kind)+')'+#13#10)
          else
            b.Append('* '+FSct.Strings.GetEntry(iWork)+' (??)'+#13#10);
      End;
      b.Append(#13#10);

      // parents:
      if ParentIndex <> 0 Then
      begin
        Parents := FSct.Refs.GetReferences(ParentIndex);
        b.Append('Parents: '+#13#10#13#10);
        for i := 0 to Length(Parents)-1 do
        begin
          FSct.Concept.GetConcept(Parents[i], Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex2, outboundIndex, refsets);
          Descriptions := FSct.Refs.GetReferences(DescriptionIndex);
          b.Append('* '+FSct.GetPN(Descriptions)+' ('+IntToStr(Identity)+')'+#13#10);
        end;
        b.Append(#13#10);
      end;

      // children: (inbound relationships with type is-a)
      first := true;
      For i := 0 to High(Inbounds) Do
      Begin
        FSct.Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
        if active and (iWork3 = FSct.FIs_a_Index) then
        begin
          if first then
          begin
            b.Append('Children: '+#13#10#13#10);
            first := false;
          end;
          FSct.Concept.GetConcept(iWork, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
          Descriptions := FSct.Refs.GetReferences(DescriptionIndex);
          b.Append('* '+FSct.GetPN(Descriptions)+' ('+IntToStr(Identity)+')'+#13#10);
        end;
      End;
      b.Append(#13#10);
    End;
    b.Append(#13#10+'This term definition is derived from SNOMED CT, which is copyright &copy; 2002+ International Health Terminology Standards Development Organisation (IHTSDO)'#13#10);
    card.detail := b.ToString;
  finally
    b.free;
  end;
end;

function TSnomedProvider.subsumesTest(opContext : TTxOperationContext; codeA, codeB: String): String;
var
  exprA, exprB : TSnomedExpression;
  b1, b2 : boolean;
begin
  exprA := FSct.parseExpression(codeA);
  try
    exprB := FSct.parseExpression(codeB);
    try
      if exprA.isSimple and exprB.isSimple then
      begin
        if exprA.concepts[0].reference = exprB.concepts[0].reference then
          result := 'equivalent'
        else if FSct.Subsumes(exprA.concepts[0].reference, exprB.concepts[0].reference) then
          result := 'subsumes'
        else if FSct.Subsumes(exprB.concepts[0].reference, exprA.concepts[0].reference) then
          result := 'subsumed-by'
        else
           result := 'not-subsumed';
      end
      else
      begin
        b1 := FSct.expressionSubsumes(exprA, exprB);
        b2 := FSct.expressionSubsumes(exprB, exprA);
        if b1 and b2 then
          result := 'equivalent'
        else if b1 then
          result := 'subsumes'
        else if b2 then
          result := 'subsumed-by'
        else
          result := 'not-subsumed';
      end;
    finally
      exprB.free;
    end;
  finally
    exprA.free;
  end;
end;

function TSnomedProvider.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
var
  i : integer;
  Identity : UInt64;
  Flags : Byte;
  Group : integer;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  iWork, iWork2, iWork3, iWork4, iWork5, iWork6, refsets : Cardinal;
  date : TSnomedDate;
  Inbounds : TCardinalArray;
  did : UInt64;
  Active, Defining : boolean;
  ndx : integer;
begin
  SetLength(inbounds, 0);
  if (context = nil) then // root
    ndx := length(FSct.FActiveRoots)
  else if (TSnomedExpressionContext(context).isComplex) then
    ndx := 0 // no children on expressions
  else
  begin
    FSct.Concept.GetConcept(TSnomedExpressionContext(context).reference, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
    Inbounds := FSct.Refs.GetReferences(InboundIndex);
    ndx := 0;
    For i := 0 to High(Inbounds) Do
    Begin
      FSct.Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, iWork4, iWork5, iWork6, date, Active, Defining, Group);
      if (iWork3 = FSct.Is_a_Index) and Active Then
        inc(ndx);
    End;
  end;
  result := TCodeSystemIteratorContext.Create(context.Link, ndx);
end;

function TSnomedProvider.Code(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex, refsets : Cardinal;
  Inbounds : TCardinalArray;
  date : TSnomedDate;
begin
  if TSnomedExpressionContext(context).isComplex then
    result := FSct.renderExpression(TSnomedExpressionContext(context).Expression, sroMinimal)
  else
  begin
    SetLength(inbounds, 0);
    FSct.Concept.GetConcept(TSnomedExpressionContext(context).reference, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
    result := inttostr(identity);
  end;
end;

function TSnomedProvider.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
var
  i, c : integer;
  Identity : UInt64;
  Flags : Byte;
  Group : integer;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  iWork, iWork2, iWork3, iWork4, iWork5, iWork6, refsets : Cardinal;
  Inbounds : TCardinalArray;
  date : TSnomedDate;
  did : UInt64;
  Active, Defining : boolean;
  ndx : integer;
begin
  result := nil;
  ndx := context.current;
  context.next;
  SetLength(inbounds, 0);
  if (context = nil) then
    raise ETerminologyError.create('check this code? [2]', itException)
    // I don't understand why we return is_a here?
    // result := TSnomedExpressionContext.create(Is_a_Index)
  else
  begin
    FSct.Concept.GetConcept(TSnomedExpressionContext(context).reference, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
    Inbounds := FSct.Refs.GetReferences(InboundIndex);
    c := -1;
    For i := 0 to High(Inbounds) Do
    Begin
      FSct.Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, iWork4, iWork5, iWork6, date, Active, Defining, Group);
      if Active and (iWork3 = FSct.Is_a_Index) Then
      begin
        inc(c);
        if (c = ndx) then
        begin
          result := TSnomedExpressionContext.create(iWork);
          exit;
        end;
      end;
    End;
  end;
end;

function TSnomedProvider.Display(opContext : TTxOperationContext; context: TCodeSystemProviderContext; langList : THTTPLanguageList): string;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex, refsets : Cardinal;
  date : TSnomedDate;
begin
  if TSnomedExpressionContext(context).isComplex then
    result := FSct.displayExpression(TSnomedExpressionContext(context).Expression)
  else
  begin
//    if lang <> nil then

    result := FSct.GetDisplayName(TSnomedExpressionContext(context).reference, FSct.FDefaultLanguage);
    FSct.Concept.GetConcept(TSnomedExpressionContext(context).reference, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
    if DebugConsoleMessages and (result = FSct.GetFSN(FSct.Refs.GetReferences(DescriptionIndex))) then
      writeln('returning FSN');
  end;
end;

procedure TSnomedProvider.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
var
  ctxt : TSnomedExpressionContext;
begin
  ctxt := context as TSnomedExpressionContext;
  if (ctxt = nil) then
    raise ETerminologyError.create('Unable to find context in '+systemUri, itInvalid)
  else if ctxt.isComplex then
    // there's only one display name - for now?
    list.addDesignation(true, true, '', '', FSct.displayExpression(ctxt.FExpression).Trim)
  else
    FSct.ListDisplayNames(list, TSnomedExpressionContext(ctxt).reference, 0, $FF);
end;

procedure TSnomedProvider.extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW);
var
  Identity : UInt64;
  Flags, bLang : Byte;
  ParentIndex, iWork, iWork2, iWork3, module, modifier, kind, caps, iDummy : Cardinal;
  Active, Defining : boolean;
  DescriptionIndex : Cardinal;
  InboundIndex, InboundIndex2 : Cardinal;
  outboundIndex, valueses, refsets : Cardinal;
  Inbounds : TCardinalArray;
  date : TSnomedDate;
  Descriptions : TCardinalArray;
  Parents : TCardinalArray;
  i, group : integer;
  p : TFHIRLookupOpRespPropertyW;
  did : UInt64;
  exp : TSnomedExpression;
begin
  if TSnomedExpressionContext(ctxt).Expression.isSimple then
  begin
    SetLength(inbounds, 0);
    FSct.Concept.GetConcept(TSnomedExpressionContext(ctxt).reference, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
    Inbounds := FSct.Refs.GetReferences(InboundIndex);

    p := resp.addProp('copyright');
    p.value := factory.makeString('This response content from SNOMED CT, which is copyright ) 2002+ International Health Terminology Standards Development Organisation (IHTSDO), and distributed '+'by agreement between IHTSDO and HL7. Implementer use of SNOMED CT is not covered by this agreement');

    if hasProp(props, 'moduleId', true) then
    begin
      p := resp.addProp('moduleId');
      p.value := factory.makeCode(FSct.getConceptId(FSct.Concept.GetModuleId(TSnomedExpressionContext(ctxt).reference)));
    end;

    if hasProp(props, 'normalForm', true) then
    begin
      try
        exp := FSct.createNormalForm(TSnomedExpressionContext(ctxt).reference);
        try
          p := resp.addProp('normalForm');
          p.value := factory.makeString(FSct.renderExpression(exp, sroFillMissing));
          p := resp.addProp('normalFormTerse');
          p.value := factory.makeString(FSct.renderExpression(exp, sroMinimal));

        finally
          exp.free;
        end;
      except
      end;
    end;

    if hasProp(props, 'designation', true) then
    begin
      // descriptions
      Descriptions := FSct.Refs.GetReferences(DescriptionIndex);
      for i := Low(Descriptions) To High(Descriptions) Do
      Begin
        FSct.Desc.GetDescription(Descriptions[i], iWork, Identity, date, iDummy, module, kind, caps, refsets, valueses, active,blang);
        if (kind <> 0) Then
          if active then
            resp.addDesignation(codeForLang(blang), URI_SNOMED, FSct.GetConceptId(kind), FSct.GetPNForConcept(kind), FSct.Strings.GetEntry(iWork))
          else
            resp.addDesignation(codeForLang(blang), URI_SNOMED, '73425007', 'Inactive', FSct.Strings.GetEntry(iWork))
       End;
    End;

    if hasProp(props, 'parent', true) then
    begin
      // parents:
      if ParentIndex <> 0 Then
      begin
        Parents := FSct.Refs.GetReferences(ParentIndex);
        for i := 0 to Length(Parents)-1 do
        begin
          FSct.Concept.GetConcept(Parents[i], Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex2, outboundIndex, refsets);
          Descriptions := FSct.Refs.GetReferences(DescriptionIndex);
          p := resp.addProp('parent');
          p.value := factory.makeCode(IntToStr(Identity));
          p.description := FSct.GetPN(Descriptions);
        end;
      end;
    end;

    if hasProp(props, 'child', true) then
    begin
      // children: (inbound relationships with type is-a)
      For i := 0 to High(Inbounds) Do
      Begin
        FSct.Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
        if (active) and (iWork3 = FSct.FIs_a_Index) then
        begin
          FSct.Concept.GetConcept(iWork, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
          Descriptions := FSct.Refs.GetReferences(DescriptionIndex);
          p := resp.addProp('child');
          p.value := factory.makeCode(IntToStr(Identity));
          p.description := FSct.GetPN(Descriptions);
        End;
      End;
    End;
  end
  else
  begin
    exp := FSct.normaliseExpression(TSnomedExpressionContext(ctxt).Expression);
    try
      p := resp.addProp('normalForm');
      p.value := factory.makeString(FSct.renderExpression(exp, sroFillMissing));
      p := resp.addProp('normalFormTerse');
      p.value := factory.makeString(FSct.renderExpression(exp, sroMinimal));
    finally
      exp.free;
    end;
  end;
end;

function TSnomedProvider.getDisplay(opContext : TTxOperationContext; code: String; langList : THTTPLanguageList): String;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(opContext, code);
  try
    if (ctxt = nil) then
      raise ETerminologyError.create('Unable to find '+code+' in '+systemUri, itInvalid)
    else
      result := Display(opContext, ctxt, langList);
  finally
    ctxt.free;
  end;
end;

function TSnomedProvider.IsAbstract(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  result := false; // snomed don't do abstract?
end;

function TSnomedProvider.IsInactive(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  if TSnomedExpressionContext(context).isComplex then
    result := false // not sure what to do here?
  else
    result := not FSct.IsActive(TSnomedExpressionContext(context).reference);
end;

function TSnomedProvider.getCodeStatus(opContext : TTxOperationContext; context: TCodeSystemProviderContext): String;
begin
  if TSnomedExpressionContext(context).isComplex then
    result := ''
  else if FSct.IsActive(TSnomedExpressionContext(context).reference) then
    result := 'active'
  else
    result := 'inactive';
end;

function TSnomedProvider.incompleteValidationMessage(context : TCodeSystemProviderContext; langs : THTTPLanguageList): String;
begin
  if TSnomedExpressionContext(context).isComplex then
    result := FI18n.translate('SCT_NO_MRCM', langs, [])
  else
    result := '';
end;

function TSnomedProvider.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := true;
end;

function TSnomedProvider.locate(opContext : TTxOperationContext; code: String; altOpt : TAlternateCodeOptions; var message : String): TCodeSystemProviderContext;
var
  iId : UInt64;
  index : cardinal;
begin
  iId := FSct.StringToIdOrZero(code);
  if iId = 0 then
  begin
    try
      result := TSnomedExpressionContext.Create(code, FSct.parseExpression(code))
    except
      on e : Exception do
      begin
        Message := 'Code '+code+' is not a valid SNOMED CT Term, and neither could it be parsed as an expression ('+e.message+')';
        result := nil;
      end;
    end
  end
  else if FSct.Concept.FindConcept(iId, index) Then
    result := TSnomedExpressionContext.create(code, index)
  else
  begin
    Message := ''; // 'Unable to find code '+code+' in '+systemUri+' (version '+version+')'; it's not useful to say anything more
    result := nil;
  end;
end;

function TSnomedProvider.systemUri: String;
begin
  result := URI_SNOMED;
end;

function TSnomedProvider.TotalCount: integer;
begin
  result := FSct.FTotalCount;
end;

function TSnomedProvider.version: String;
begin
  result := FSct.FVersionUri;
end;

function TSnomedProvider.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop: String; op: TFhirFilterOperator; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  id : UInt64;
begin
  SetThreadStatus(ClassName+'.filter('+prop+CODES_TFhirFilterOperator[op]+value+')');
  result := nil;
  if (prop = 'concept') and FSct.StringIsId(value, id) then
    if op = foIsA then
      result := FSct.filterIsA(id, true)
    else if op = foDescendentOf then
      result := FSct.filterIsA(id, false)
    else if op = foIn then
      result := FSct.filterIn(id)
    else if op = foEqual then
      result := FSct.filterEquals(id)
end;

function TSnomedProvider.FilterConcept(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  if Length(TSnomedFilterContext(ctxt).matches) > 0 then
    result := TSnomedExpressionContext.create(TSnomedFilterContext(ctxt).Matches[TSnomedFilterContext(ctxt).ndx-1].index)
  else if Length(TSnomedFilterContext(ctxt).members) > 0 then
    result := TSnomedExpressionContext.create(TSnomedFilterContext(ctxt).Members[TSnomedFilterContext(ctxt).ndx-1].Ref)
  else
    result := TSnomedExpressionContext.create(TSnomedFilterContext(ctxt).descendants[TSnomedFilterContext(ctxt).ndx-1]);
end;

function TSnomedProvider.InFilter(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
var
  index : integer;
begin
  if Length(TSnomedFilterContext(ctxt).members) > 0 then
    result := FindMember(TSnomedFilterContext(ctxt).Members, TSnomedExpressionContext(concept).reference, index)
  else
    result := FindCardinalInArray(TSnomedFilterContext(ctxt).descendants, TSnomedExpressionContext(concept).reference, index)
end;

function TSnomedProvider.FilterMore(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  inc(TSnomedFilterContext(ctxt).ndx);
  if Length(TSnomedFilterContext(ctxt).matches) > 0 then
    result := TSnomedFilterContext(ctxt).ndx <= Length(TSnomedFilterContext(ctxt).matches)
  else if Length(TSnomedFilterContext(ctxt).members) > 0 then
    result := TSnomedFilterContext(ctxt).ndx <= Length(TSnomedFilterContext(ctxt).members)
  else
    result := TSnomedFilterContext(ctxt).ndx <= Length(TSnomedFilterContext(ctxt).descendants);
end;

function TSnomedProvider.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  if Length(TSnomedFilterContext(ctxt).matches) > 0 then
    result := Length(TSnomedFilterContext(ctxt).matches)
  else if Length(TSnomedFilterContext(ctxt).members) > 0 then
    result := Length(TSnomedFilterContext(ctxt).members)
  else
    result := Length(TSnomedFilterContext(ctxt).descendants);
end;

function TSnomedProvider.filterLocate(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; code: String; var message : String): TCodeSystemProviderContext;
var
  c : TSnomedFilterContext;
  index : integer;
  concept : TCodeSystemProviderContext;
  ok : boolean;
begin
  c := TSnomedFilterContext(ctxt);
  concept := locate(opContext, code, nil, message);
  try
    message := '';
    if concept = nil then
      ok := false
    else if Length(TSnomedFilterContext(ctxt).members) > 0 then
      ok := FindMember(TSnomedFilterContext(ctxt).Members, TSnomedExpressionContext(concept).reference, index)
    else
      ok := FindCardinalInArray(TSnomedFilterContext(ctxt).descendants, TSnomedExpressionContext(concept).reference, index);
    if (ok) then
      result := concept.link
    else
      result := nil;
  finally
    concept.free;
  end;
end;

function TSnomedProvider.locateIsA(opContext : TTxOperationContext; code, parent: String; disallowParent : boolean = false): TCodeSystemProviderContext;
var
  ic, ip : Cardinal;
begin
  if FSct.Concept.FindConcept(FSct.StringToIdOrZero(parent), ip) And
       FSct.Concept.FindConcept(FSct.StringToIdOrZero(code), ic) And FSct.Subsumes(ip, ic) And (not disallowParent or (ic <> ip)) then
    result := TSnomedExpressionContext.create(code, ic)
  else
    result := nil;
end;


function TSnomedProvider.name(context: TCodeSystemProviderContext): String;
begin
  result := 'SNOMED CT';
end;

function TSnomedProvider.sameContext(opContext : TTxOperationContext; a, b: TCodeSystemProviderContext): boolean;
begin
  result := (a is TSnomedExpressionContext) and (b is TSnomedExpressionContext) and ((a as TSnomedExpressionContext).FSource = (b as TSnomedExpressionContext).FSource);
end;


initialization
  {$IFDEF FPC}
  SNOMED_DATE_FORMAT := DefaultFormatSettings;
  SNOMED_DATE_FORMAT.ShortDateFormat := 'dd/mm/yyyy';
  {$ELSE}
  SNOMED_DATE_FORMAT := TFormatSettings.Create('en-AU');
  {$ENDIF}
End.

