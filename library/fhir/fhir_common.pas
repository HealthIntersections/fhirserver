unit fhir_common;

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

{

Using the Common Layer

This is a version independent interface to the FHIR version specific objects.
This is pretty useful, but like all good things, it comes with a price: you
have to be super careful how you use this layer.

This layer is a facade over the actual underlying objects.



}
uses
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_versions, fsl_http, fsl_logging, fsl_lang,
  fhir_objects, fhir_utilities, fhir_features, fhir_uris, fhir_parser;

Type
  TFilterOperator = (foNull, foEqual, foIsA, foDescendentOf, foIsNotA, foRegex, foIn, foNotIn, foGeneralizes, foExists, foChildOf, foDescendentLeaf);
  TPublicationStatus = (psNull, psDraft, psActive, psRetired);
  TBundleType = (btNull, btDocument, btMessage, btTransaction, btTransactionResponse, btBatch, btBatchResponse, btHistory, btSearchset, btCollection);
  TTriggerType = (ttNull, ttNamedEvent, ttPeriodic, ttDataChanged, ttDataAdded, ttDataModified, ttDataRemoved, ttDataAccessed, ttDataAccessEnded);
  TContactType = (cpsNull, cpsPhone, cpsFax, cpsEmail, cpsPager, cpsUrl, cpsSms, cpsOther);
  TSubscriptionStatus = (ssNull, ssRequested, ssActive, ssError, ssOff, ssEnteredInError);
  TSubscriptionMethod = (smNull, smRestHook, smEmail, smSms, smWebsocket, smChangeScript);
  TObservationStatus = (obssNull, obssRegistered, obssPreliminary, obssFinal, obssAmended, obssCorrected, obssCancelled, obssEnteredInError, obssUnknown);
  TTokenCategory = (tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcMedicationDefinition, tcOther);
  TIdentifierUse = (iuNull, iuUsual, iuOfficial, iuTemp, iuSecondary, iuOld);
  TOpIssueCode = (oicVoid, oicNotInVS, oicThisNotInVS, oicInvalidCode, oicDisplay, oicDisplayComment, oicNotFound, oicCodeRule, oicVSProcessing, oicInferFailed, oicStatusCheck, oicInvalidData, oicProcessingNote);

const
  CODES_TFhirFilterOperator: Array[TFilterOperator] of String = ('', '=', 'is-a', 'descendent-of', 'is-not-a', 'regex', 'in', 'not-in', 'generalizes', 'exists', 'child-of', 'descendent-leaf');
  CODES_TPublicationStatus: Array[TPublicationStatus] of String = ('', 'draft', 'active', 'retired');
  CODES_TTokenCategory : array [TTokenCategory] of String = ('Clinical', 'Data', 'Meds', 'Schedule', 'Audit', 'Documents', 'Financial', 'MedicationDefinitions', 'Other');
  CODES_TOpIssueCode : array [TOpIssueCode] of String = ('', 'not-in-vs', 'this-code-not-in-vs', 'invalid-code', 'invalid-display', 'display-comment', 'not-found', 'code-rule', 'vs-invalid', 'cannot-infer', 'status-check', 'invalid-data', 'process-note');

type
  EFHIROperationException = class (EFslException)
  private
    FMsgId : String;
    FParams : TStringArray;
    FLevel : TIssueSeverity;
    FCause : TFHIRIssueType;
    FCode : TOpIssueCode;
    FPath : String;
  public
    constructor create(level : TIssueSeverity; cause : TFHIRIssueType; code : TOpIssueCode; path, message : String);
    constructor createMsg(level : TIssueSeverity; cause : TFHIRIssueType; code : TOpIssueCode; path, messageId : String; params : TStringArray);

    property MsgId : String read FMsgId;
    property Params : TStringArray read FParams;
    property Level : TIssueSeverity read FLevel;
    property Cause : TFHIRIssueType read FCause;
    property Code : TOpIssueCode read FCode;
    property Path : String read FPath;
  end;

  // base wrappers.....
  TFhirExtensionW = class;

  { TFHIRXVersionElementWrapper }

  TFHIRXVersionElementWrapper = class abstract (TFHIRObject)
  protected
    FElement : TFHIRObject;
    function GetFhirObjectVersion: TFHIRVersion; override;
    function NoElementOk : boolean; virtual;
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; virtual; abstract; // this must be overridden in *every concrete subclass*
  public
    constructor Create(elem : TFHIRObject);
    destructor Destroy; override;

    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function fhirType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function hasExtensions : boolean; override;

    property Element : TFHIRObject read FElement;

    function AsJson : String; override;

    // extensions:
    function hasExtension(url : String) : boolean; override;
    function getExtensionString(url : String) : String; override;
    function getExtensionValue(url : String) : TFHIRObject; override;
    function extensionCount(url : String) : integer; override;
    function getExtensionsV(url : String) : TFslList<TFHIRObject>; overload; override;
    function getExtensionsV : TFslList<TFHIRObject>; overload; override;
    procedure addExtensionV(url : String; value : TFHIRObject); overload; override;
    procedure addExtensionV(extension : TFHIRObject); overload; override;
    function getExtensionsW(url : String) : TFslList<TFhirExtensionW>;
    function getAllExtensionsW : TFslList<TFhirExtensionW>;
    function getExtensionW(url : String) : TFhirExtensionW;
    procedure addExtensionW(url : String; value : TFHIRXVersionElementWrapper);
    procedure deleteExtensionV(extension : TFHIRObject); override;
    procedure deleteExtensionW(extension : TFHIRExtensionW);
    procedure deleteExtensionByUrl(url : String); override;
    procedure stripExtensions(exemptUrls : TStringArray); override;
    procedure copyExtensions(src : TFHIRObject; exemptUrls : TStringArray); override;
  end;
  TFHIRXVersionElementWrapperClass = class of TFHIRXVersionElementWrapper;

  TFhirDataTypeW = class (TFHIRXVersionElementWrapper)
  public
    function renderText : String; virtual; abstract;
  end;

  { TFHIRPrimitiveW }

  TFHIRPrimitiveW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFHIRPrimitiveW; overload;
    function GetAsString : String; virtual; abstract;
    procedure SetAsString(value : String); virtual; abstract;
    property AsString : String read GetAsString write SetAsString;
  end;

  TFhirCodingW = class (TFhirDataTypeW)
  protected
    function getCode: String; virtual; abstract;
    function getDisplay: String; virtual; abstract;
    function getSystem: String; virtual; abstract;
    function getVersion: String; virtual; abstract;
    procedure setCode(Value: String); virtual; abstract;
    procedure setDisplay(Value: String); virtual; abstract;
    procedure setSystem(Value: String); virtual; abstract;
    procedure setVersion(Value: String); virtual; abstract;
  public
    function link : TFhirCodingW; overload;
    property systemUri : String read GetSystem write SetSystem;
    property version : String read GetVersion write SetVersion;
    property code : String read GetCode write SetCode;
    property display : String read GetDisplay write SetDisplay;
  end;

  TFhirQuantityW = class (TFhirDataTypeW)
  protected
    function getCode: String; virtual; abstract;
    function getSystem: String; virtual; abstract;
    function getUnit: String; virtual; abstract;
    function getValue: String; virtual; abstract;
    procedure setCode(Value: String); virtual; abstract;
    procedure setSystem(Value: String); virtual; abstract;
    procedure setUnit(Value: String); virtual; abstract;
    procedure setValue(Value: String); virtual; abstract;
  public
    function link : TFhirQuantityW; overload;
    property value : String read GetValue write SetValue;
    property units : String read GetUnit write SetUnit;
    property systemUri : String read GetSystem write SetSystem;
    property code : String read GetCode write SetCode;
    function asDuration : TDateTime; virtual; abstract;
  end;

  TFHIRPeriodW = class (TFhirDataTypeW)
  protected
    function GetEnd: TFslDateTime; virtual; abstract;
    function GetStart: TFslDateTime; virtual; abstract;
    procedure SetEnd(const Value: TFslDateTime); virtual; abstract;
    procedure SetStart(const Value: TFslDateTime); virtual; abstract;
  public
    function link : TFHIRPeriodW; overload;
    property start : TFslDateTime read GetStart write SetStart;
    property end_ : TFslDateTime read GetEnd write SetEnd;
  end;

  TFHIRAttachmentW = class abstract (TFhirDataTypeW)
  protected
    function GetContentType: String; virtual; abstract;
    function GetData: TBytes; virtual; abstract;
  public
    property data : TBytes read GetData;
    property contentType : String read GetContentType;
  end;

  TFhirMetaW = class (TFHIRXVersionElementWrapper)
  protected
    function getVersionId: String; virtual; abstract;
    procedure setVersionId(Value: String); virtual; abstract;
    function getLastUpdated: TFslDateTime; virtual; abstract;
    procedure setLastUpdated(Value: TFslDateTime); virtual; abstract;
  public
    function link : TFhirMetaW; overload;
    property versionid : String read GetVersionId write SetVersionId;
    property lastUpdated : TFslDateTime read GetLastUpdated write SetLastUpdated;
    function tags : TFslList<TFHIRCodingW>; virtual; abstract;
    function labels : TFslList<TFHIRCodingW>; virtual; abstract;
    function profiles : TArray<String>; virtual; abstract;
    function hasTag(systemUri, code : String) : boolean; virtual; abstract;
    function hasLabel(systemUri, code : String) : boolean; virtual; abstract;
    procedure addTag(systemUri, code, display : String); virtual; abstract;
    procedure addLabel(systemUri, code, display : String); virtual; abstract;
    procedure addProfile(uri : String); virtual; abstract;
    procedure clearTags; virtual; abstract;
    procedure clearLabels; virtual; abstract;
    procedure clearProfiles; virtual; abstract;
    procedure removeTag(systemUri, code : String); virtual; abstract;
    procedure removeLabel(systemUri, code : String); virtual; abstract;
    procedure removeProfile(uri : String); virtual; abstract;
  end;

  { TFHIRXVersionResourceWrapper }

  TFHIRXVersionResourceWrapper = class (TFHIRObject)
  protected
    FRes : TFHIRResourceV;
    function GetFhirObjectVersion: TFHIRVersion; override;
    function sizeInBytesV(magic : integer) : cardinal; override;
    function GetLanguage: String;  virtual; abstract;
    procedure SetLanguage(const Value: String); virtual; abstract;

    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; virtual; abstract; // this must be overridden in *every concrete subclass*
  public
    constructor Create(res : TFHIRResourceV);
    destructor Destroy; override;

    function link : TFHIRXVersionResourceWrapper; overload;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function fhirType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    procedure checkNoImplicitRules(place, role : String); virtual;
    function hasExtensions : boolean; override;
    property language : String read GetLanguage write SetLanguage;

    function AsJson : String; override;
    function SourcePackage : String;

    // extensions:
    function hasExtension(url : String) : boolean; override;
    function getExtensionString(url : String) : String; override;
    function getExtensionValue(url : String) : TFHIRObject; override;
    function getExtensionsV : TFslList<TFHIRObject>; override;
    function extensionCount(url : String) : integer; override;
    function getExtensionsV(url : String) : TFslList<TFHIRObject>; override;
    procedure addExtensionV(url : String; value : TFHIRObject); overload; override;
    function getExtensionsW(url : String) : TFslList<TFhirExtensionW>;
    function getExtensionW(url : String) : TFhirExtensionW;
    procedure addExtensionW(url : String; value : TFHIRXVersionElementWrapper);

    property Resource : TFHIRResourceV read FRes;
  end;

  { TFHIRXVersionOperationObjectWrapper }

  TFHIRXVersionOperationObjectWrapper = class (TFslObject)
  private
     FList : TFslList<TFslObject>;
  protected
    FObj : TFslObject;

    function sizeInBytesV(magic : integer) : cardinal; override;
    function List : TFslList<TFslObject>;
  public
    constructor Create(res : TFslObject);
    destructor Destroy; override;
    property Obj : TFslObject read FObj;
  end;

  { TFHIRXVersionOperationWrapper }

  TFHIRXVersionOperationWrapper = class (TFslObject)
  private
     FList : TFslList<TFslObject>;
  protected
    FOp : TFslObject;
    function sizeInBytesV(magic : integer) : cardinal; override;
    function List : TFslList<TFslObject>;
  public
    constructor Create(res : TFslObject);
    destructor Destroy; override;
    procedure load(params : TFHIRResourceV); overload; virtual; abstract;
    procedure load(params : THTTPParameters); overload; virtual; abstract;
    function asParams : TFHIRResourceV; virtual; abstract;

    property Op : TFslObject read FOp;
  end;

  TFhirCodeableConceptW = class (TFhirDataTypeW)
  protected
    function GetText: String; virtual; abstract;
    procedure SetText(const Value: String); virtual; abstract;
  public
    function link : TFhirCodeableConceptW; overload;
    function codingCount : integer; virtual; abstract;
    function codings : TFslList<TFhirCodingW>; virtual; abstract;
    procedure clearCodings; virtual; abstract;
    procedure addCoding(coding : TFHIRCodingW); overload; virtual; abstract;
    function addCoding : TFHIRCodingW; overload; virtual; abstract;
    procedure addCoding(systemUri, version, code, display : String); overload; virtual; abstract;
    procedure removeCoding(systemUri, version, code : String); virtual; abstract;
    function summary : String; virtual; abstract;
    function hasCode(systemUri, code : String) : boolean; overload; virtual; abstract;
    function hasCode(systemUri, version, code : String) : boolean; overload; virtual; abstract;
    function fromSystem(systemUri : String; required : boolean = false) : String; overload; virtual; abstract;
    function fromSystem(systems : TArray<String>; required : boolean = false) : String; overload; virtual; abstract;
    property text : String read GetText write SetText;
  end;

  TFhirIdentifierW = class (TFhirDataTypeW)
  private
    FType: TFhirCodeableConceptW;
    function GetType: TFhirCodeableConceptW;
    procedure SetType(const Value: TFhirCodeableConceptW);
  protected
    function GetSystem: String; virtual; abstract;
    function GetUse: TIdentifierUse; virtual; abstract;
    function GetValue: String; virtual; abstract;
    procedure SetSystem(const Value: String); virtual; abstract;
    procedure SetUse(const Value: TIdentifierUse); virtual; abstract;
    procedure SetValue(const Value: String); virtual; abstract;
    function GetTypeV: TFhirCodeableConceptW; virtual; abstract;
    procedure SetTypeV(const Value: TFhirCodeableConceptW); virtual; abstract;
  public
    destructor Destroy; override;
    function link : TFhirIdentifierW; overload;
    property type_ : TFhirCodeableConceptW read GetType write SetType;
    property use : TIdentifierUse read GetUse write SetUse;
    property systemUri : String read GetSystem write SetSystem;
    property value : String read GetValue write SetValue;
  end;

  TFhirExtensionW = class (TFhirDataTypeW)
  public
    function link : TFhirExtensionW; overload;
    function url : String; virtual; abstract;
    function value : TFHIRObject; virtual; abstract;

    procedure setValueW(value : TFhirDataTypeW); virtual; abstract;
    procedure setValueV(value : TFhirObject); virtual; abstract;
    function valueAsCodeableConcept : TFhirCodeableConceptW; virtual; abstract;
    function valueAsCoding : TFhirCodingW; virtual; abstract;
    function valueAsPeriod : TFhirPeriodW; virtual; abstract;
    function valueAsQuantity : TFhirQuantityW; virtual; abstract;
    function valueAsIdentifier : TFhirIdentifierW; virtual; abstract;
    function valueAsAttachment : TFhirAttachmentW; virtual; abstract;
    function valueAsString : string; virtual; abstract;
  end;

  TFhirOperationOutcomeIssueW = class (TFHIRXVersionElementWrapper)
  protected
    function getDiagnostics: String; virtual; abstract;
    procedure setDiagnostics(Value: String); virtual; abstract;
  public
    function link : TFhirOperationOutcomeIssueW; overload;
    function display : String; virtual; abstract;
    function severity : TIssueSeverity; virtual; abstract;
    property diagnostics : String read GetDiagnostics write SetDiagnostics;
  end;

  TFhirOperationOutcomeW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirOperationOutcomeW; overload;

    function hasText : boolean; virtual; abstract;
    function text : String; virtual; abstract;
    function issueCount : integer; virtual; abstract;
    function severity : TIssueSeverity; virtual; abstract;
    function code :  TFhirIssueType; virtual; abstract;

    procedure addIssue(issue : TFhirOperationOutcomeIssueW; free : boolean); overload; virtual; abstract;
    procedure addIssue(level : TIssueSeverity; cause : TFhirIssueType; path, message : String; issueCode : TOpIssueCode; addIfDuplicate : boolean = false); overload; virtual; abstract;
    procedure addIssue(level : TIssueSeverity; cause : TFhirIssueType; path, msgId, message : String; issueCode : TOpIssueCode; addIfDuplicate : boolean = false); overload; virtual; abstract;
    function hasIssues : boolean; virtual; abstract;
    function issues : TFslList<TFhirOperationOutcomeIssueW>; virtual; abstract;
    function rule(level : TIssueSeverity; source : String; typeCode : TFhirIssueType; path : string; test : boolean; msg : string) : boolean; virtual; abstract;
    function error(source : String; typeCode : TFhirIssueType; path : string; test : boolean; msg : string) : boolean;
    function warning(source : String; typeCode : TFhirIssueType; path : string; test : boolean; msg : string) : boolean;
    function hint(source : String; typeCode : TFhirIssueType; path : string; test : boolean; msg : string) : boolean;
    function hasErrors : boolean; virtual; abstract;
  end;
  TFhirOperationOutcomeWClass = class of TFhirOperationOutcomeW;

  TFHIRBinaryW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFHIRBinaryW; overload;

    function content : TBytes; virtual; abstract;
    function ContentType : String; virtual; abstract;
  end;

  TFHIRBundleEntrySearchMode = (smUnknown, smMatch, smInclude, smOutcome);

  TFhirBundleEntryW = class (TFHIRXVersionElementWrapper)
  protected
    function getLink(rel: String): String; virtual; abstract;
    procedure setLink(rel: String; const Value: String); virtual; abstract;
    function getRequestMethod: String; virtual; abstract;
    function getRequestUrl: String; virtual; abstract;
    function getResource: TFHIRResourceV; virtual; abstract;
    function getResponseDate: TFslDateTime; virtual; abstract;
    function getResponseStatus: String; virtual; abstract;
    function getSearchMode: TFHIRBundleEntrySearchMode; virtual; abstract;
    function getSearchMpiMatch: String; virtual; abstract;
    function getSearchScore: String; virtual; abstract;
    procedure setRequestMethod(Value: String); virtual; abstract;
    procedure setRequestUrl(Value: String); virtual; abstract;
    procedure setResource(Value: TFHIRResourceV); virtual; abstract;
    procedure setResponseDate(Value: TFslDateTime); virtual; abstract;
    procedure setResponseStatus(Value: String); virtual; abstract;
    procedure setSearchMode(Value: TFHIRBundleEntrySearchMode); virtual; abstract;
    procedure setSearchMpiMatch(Value: String); virtual; abstract;
    procedure setSearchScore(Value: String); virtual; abstract;
    function getURL: String; virtual; abstract;
    procedure setUrl(Value: String);  virtual; abstract;
    function getrequestIfNoneExist: String; virtual; abstract;
    procedure setrequestIfNoneExist(Value: String); virtual; abstract;
    function getrequestIfMatch: String; virtual; abstract;
    procedure setrequestIfMatch(Value: String); virtual; abstract;
    function getrequestIfNoneMatch: String; virtual; abstract;
    procedure setrequestIfNoneMatch(Value: String); virtual; abstract;
    function getResponseETag: string; virtual; abstract;
    procedure setResponseETag(Value: string); virtual; abstract;
    function getResponseLocation: string; virtual; abstract;
    procedure setResponseLocation(Value: string); virtual; abstract;
    function getrequestIfModifiedSince: TFslDateTime; virtual; abstract;
    procedure setrequestIfModifiedSince(Value: TFslDateTime); virtual; abstract;
  public
    function Link : TFhirBundleEntryW; overload;
    property links[rel : String] : String read GetLink write SetLink;
    property url : String read getURL write SetUrl;
    property searchMode : TFHIRBundleEntrySearchMode read getSearchMode write SetSearchMode;
    property searchScore : String read getSearchScore write SetSearchScore;
    property searchMpiMatch : String read getSearchMpiMatch write SetSearchMpiMatch;
    property resource : TFHIRResourceV read getResource write SetResource;
    property requestMethod : String read getRequestMethod write SetRequestMethod;
    property requestUrl : String read getRequestUrl write SetRequestUrl;
    property requestIfNoneExist : String read GetrequestIfNoneExist write SetrequestIfNoneExist;
    property requestIfMatch : String read GetrequestIfMatch write SetrequestIfMatch;
    property requestIfNoneMatch : String read GetrequestIfNoneMatch write SetrequestIfNoneMatch;
    property requestIfModifiedSince : TFslDateTime read GetrequestIfModifiedSince write SetrequestIfModifiedSince;
    property responseDate : TFslDateTime read GetResponseDate write SetResponseDate;
    property responseStatus : String read GetResponseStatus write SetResponseStatus;
    property responseETag : string read GetResponseETag write SetResponseETag;
    property responseLocation : string read GetResponseLocation write SetResponseLocation;
  end;

  TFHIRBundleW = class (TFHIRXVersionResourceWrapper)
  protected
    function getLink(rel: String): String; virtual; abstract;
    procedure setLink(rel: String; const Value: String); virtual; abstract;
    function getLastUpdated : TFslDateTime; virtual; abstract;
    procedure setLastUpdated(Value: TFslDateTime); virtual; abstract;
    function getTotal: integer; virtual; abstract;
    procedure setTotal(Value: integer); virtual; abstract;
    function getType: TBundleType; virtual; abstract;
    procedure setType(Value: TBundleType); virtual; abstract;
    function getTimestamp: TFslDateTime; virtual; abstract;
    procedure setTimestamp(Value: TFslDateTime); virtual; abstract;
  public
    function link : TFHIRBundleW;
    function next : String; overload;
    function next(bnd : TFHIRResourceV) : String; overload; virtual; abstract;
    procedure addEntries(bnd : TFHIRResourceV); virtual; abstract;
    procedure addEntry(bnd : TFhirBundleEntryW; first : boolean); overload; virtual; abstract;
    procedure addEntry(url : String; bnd : TFhirResourceV); overload; virtual; abstract;
    function addEntry : TFhirBundleEntryW; overload; virtual; abstract;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; virtual; abstract;
    function count(rtype : String = '') : Integer; virtual; abstract;
    procedure clearLinks; virtual; abstract;
    function entries : TFslList<TFhirBundleEntryW>; virtual; abstract;
    procedure listLinks(links : TFslStringDictionary); virtual; abstract;
    property links[rel : String] : String read GetLink write SetLink;
    property total : integer read GetTotal write SetTotal;
    function title : String; virtual; abstract;
    property type_ : TBundleType read GetType write SetType;
    property timestamp : TFslDateTime read GetTimestamp write SetTimestamp;
    property lastUpdated : TFslDateTime read GetLastUpdated write SetLastUpdated;
  end;
  TFHIRBundleWClass = class of TFHIRBundleW;

  TFHIRSearchParamType = (sptNull, sptString, sptToken, sptComposite, sptUri, sptReference, sptNumber, sptDate, sptQuantity, sptSpecial);
  TFhirSearchParamTypeList = set of TFhirSearchParamType;
  TFhirSearchXpathUsage = (sxpNull,  sxpNormal, sxpPhonetic, sxpNearby, sxpDistance, sxpOther);

const
  CODES_TFhirSearchParamType : Array[TFhirSearchParamType] of String = ('', 'string', 'token', 'composite', 'uri', 'reference', 'number', 'date', 'quantity', 'special');

type
  TFhirSearchParameterW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirSearchParameterW;
    function name : String;  virtual; abstract;
    function description : String;  virtual; abstract;
    function type_ : TFHIRSearchParamType;  virtual; abstract;
    function xpathUsage : TFhirSearchXpathUsage;  virtual; abstract;
    function targets : TArray<String>; virtual; abstract;{var
  targets : TArray<String>;
  i : integer;
  SetLength(targets, sp.targetList.Count);
  for i := 0 to sp.targetList.Count - 1 do
    targets[i] := sp.targetList[i].value;

}
  end;

  TFHIRSearchParamDefinitionW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFHIRSearchParamDefinitionW; overload;

    function name : String; virtual; abstract;
    function documentation : String; virtual; abstract;
    function type_ : TFHIRSearchParamType; virtual; abstract;
  end;

  TFHIRInteraction = (fiRead, fiSearch, fiHistory, fiCreate, fiUpdate, fiDelete, fiPatch);
  TFHIRInteractions = set of TFHIRInteraction;

const
  ALL_INTERACTIONS = [fiRead..fiDelete];
  CODES_TFHIRBundleEntrySearchMode : array [TFHIRBundleEntrySearchMode] of String = ('', 'match', 'include', 'outcome');
  All_TFHIRBundleEntrySearchMode = [smUnknown..smOutcome];

type
  TFhirCapabilityStatementRestResourceW = class (TFHIRXVersionElementWrapper)
  protected
    function getCode: String; virtual; abstract;
    procedure setCode(Value: String); virtual; abstract;
    function getProfile: String; virtual; abstract;
    procedure setProfile(Value: String); virtual; abstract;
    function getReadHistory: boolean; virtual; abstract;
    procedure setReadHistory(Value: boolean); virtual; abstract;
  public
    function link : TFhirCapabilityStatementRestResourceW; overload;
    property code : String read GetCode write SetCode;
    property profile : String read GetProfile write SetProfile;
    property readHistory : boolean read GetReadHistory write SetReadHistory;
    function hasInteraction : boolean; virtual; abstract;
    procedure addInteraction(code : String); virtual; abstract;
    procedure addParam(html, n, url, d : String; t : TFHIRSearchParamType; tgts : Array of String); virtual; abstract;
  end;

  TCapabilityStatementKind = (cskNull, cskInstance, cskCapability, cskRequirements);
  TCapabilityStatementAcceptUnknown = (csauNull, csauNo, csauExtensions, csauElements, csauBoth);


  TFHIRCapabilityStatementW = class (TFHIRXVersionResourceWrapper)
  protected
    function getURL: String; virtual; abstract;
    procedure setUrl(Value: String); virtual; abstract;
    function getName : String; virtual; abstract;
    procedure setName(value : String); virtual; abstract;
    function getVersion : String; virtual; abstract;
    procedure setVersion(value : String); virtual; abstract;
    function getDescription : String; virtual; abstract;
    procedure setDescription(value : String); virtual; abstract;
    function getStatus: TPublicationStatus; virtual; abstract;
    procedure setStatus(Value: TPublicationStatus); virtual; abstract;
    function getKind: TCapabilityStatementKind; virtual; abstract;
    procedure setKind(Value: TCapabilityStatementKind); virtual; abstract;
    function getAcceptUnknown: TCapabilityStatementAcceptUnknown; virtual; abstract;
    procedure setAcceptUnknown(const Value: TCapabilityStatementAcceptUnknown); virtual; abstract;
    function getDate: TFslDateTime; virtual; abstract;
    procedure setDate(Value: TFslDateTime); virtual; abstract;
    function getFhirVersion: string; virtual; abstract;
    procedure setFhirVersion(Value: string); virtual; abstract;
  public
    function link : TFHIRCapabilityStatementW; overload;

    property url : String read getURL write SetUrl;
    property name : String read GetName write SetName;
    property version : String read GetVersion write SetVersion;
    property status : TPublicationStatus read GetStatus write SetStatus;
    property kind : TCapabilityStatementKind read getKind write setKind;
    property acceptUnknown : TCapabilityStatementAcceptUnknown read getAcceptUnknown write setAcceptUnknown;
    property description : String read GetDescription write SetDescription;
    property date : TFslDateTime read GetDate write SetDate;
    property fhirVersion : string read GetFhirVersion write SetFhirVersion;

    function hasRest : boolean; virtual; abstract;
    function hasSecurity(systemUri, code : String) : boolean; virtual; abstract;
    procedure readSmartExtension(var authorize, token, register: String); virtual; abstract;
    procedure addSmartExtensions(authorize, token, register, manage: String; caps : Array of String); virtual; abstract;
    function hasFormat(fmt : String) : boolean; virtual; abstract;

    procedure contact(kind : TContactType; value : String); virtual; abstract;
    procedure software(name, version, release : String); virtual; abstract;
    procedure impl(url, desc : String); virtual; abstract;
    procedure fmt(mt : String); virtual; abstract;
    procedure standardServer(ts, ws, pv, cv, iv : String; transactions, search, history : boolean); virtual; abstract;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); virtual; abstract;
    function addResource(code : String) : TFhirCapabilityStatementRestResourceW; virtual; abstract;
    procedure addOperation(name, url : String); virtual; abstract;

    function supportsType(name : String; interaction : TFHIRInteraction) : boolean; virtual; abstract;
    procedure listTypes(interactions : TFHIRInteractions; names : TStrings); virtual; abstract;
    procedure listSearchParams(name : String; list : TFslList<TFHIRSearchParamDefinitionW>); virtual; abstract;
    procedure addInstantiates(url : String); virtual; abstract;
  end;

  TFhirParametersParameterW = class (TFHIRXVersionElementWrapper)
  protected
    FList : TFslList<TFhirParametersParameterW>;

    function getValue: TFHIRObject; virtual; abstract;
    procedure setValue(Value: TFHIRObject); virtual; abstract;
    function getResource: TFHIRResourceV; virtual; abstract;
    procedure setResource(Value: TFHIRResourceV); virtual; abstract;
    procedure populateList; virtual;
    function getParameterParameter(name: String): TFhirParametersParameterW;  virtual; abstract;
    function getResourceParameter(name: String): TFHIRResourceV;  virtual; abstract;
    function getStringParameter(name: String): String;  virtual; abstract;
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    destructor Destroy; override;
    function link : TFhirParametersParameterW; overload;

    function name : String; virtual; abstract;
    function hasValue : boolean;  virtual; abstract;
    property value : TFHIRObject read GetValue write SetValue;
    function valueString : String; virtual; abstract;
    function hasResource : boolean;  virtual; abstract;
    property resource : TFHIRResourceV read GetResource write SetResource;

    property res[name : String] : TFHIRResourceV read GetResourceParameter;
    property str[name : String] : String read GetStringParameter;
    property param[name : String] : TFhirParametersParameterW read GetParameterParameter;

    function partList : TFslList<TFhirParametersParameterW>;

    function addParam(name : String) : TFhirParametersParameterW; overload; virtual; abstract;
    procedure addParamBool(name : String; value : boolean); virtual; abstract;
    procedure addParam(name : String; value : TFHIRObject); overload; virtual; abstract;
    procedure addParamStr(name : String; value : string); virtual; abstract;
    procedure addParamCode(name : String; value : string); virtual; abstract;
    procedure addParamUri(name : String; value : string); virtual; abstract;
    procedure addParamCanonical(name : String; value : string); virtual; abstract;
  end;

  TFHIRParametersW = class (TFHIRXVersionResourceWrapper)
  protected
    FList : TFslList<TFhirParametersParameterW>;
    function getParameter(name: String): TFhirParametersParameterW;  virtual; abstract;
    procedure populateList; virtual;
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    destructor Destroy; override;
    function link : TFHIRParametersW; overload;

    function names : String; virtual; abstract;
    function has(name : String) : boolean; virtual; abstract;
    function bool(name : String) : boolean; virtual; abstract;
    function str(name : String) : String; virtual; abstract;
    function obj(name : String) : TFHIRObject; virtual; abstract;
    property param[name : String] : TFhirParametersParameterW read GetParameter;

    function addParam(name : String) : TFhirParametersParameterW; overload; virtual; abstract;
    procedure addParamBool(name : String; value : boolean); virtual; abstract;
    procedure addParam(name : String; value : TFHIRObject); overload; virtual; abstract;
    procedure addParamStr(name : String; value : string); virtual; abstract;
    procedure addParamCode(name : String; value : string); virtual; abstract;
    procedure addParamUri(name : String; value : string); virtual; abstract;
    procedure addParamCanonical(name : String; value : string); virtual; abstract;

    function parameterList : TFslList<TFhirParametersParameterW>;
  end;

  TFhirCodeSystemConceptPropertyW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodeSystemConceptPropertyW; overload;
    function code : String; virtual; abstract;
    function value : TFHIRObject; virtual; abstract;
  end;

  TFhirCodeSystemConceptDesignationW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodeSystemConceptDesignationW; overload;
    function language : String; virtual; abstract;
    function useGen : String; virtual; abstract;
    function use : TFHIRCodingW; virtual; abstract;
    function value : String; virtual; abstract;
    function valueElement : TFHIRPrimitiveW; virtual; abstract;
  end;

  TFhirCodeSystemConceptW = class;
  TFhirCodeSystemConceptListW = TFslList<TFhirCodeSystemConceptW>;

  TFhirCodeSystemConceptW = class (TFHIRXVersionElementWrapper)
  protected
    FConceptList : TFhirCodeSystemConceptListW;
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    destructor Destroy; override;
  public
    function link : TFhirCodeSystemConceptW; overload;
    function code : String; virtual; abstract;
    function display : String; virtual; abstract;
    function displayElement : TFHIRPrimitiveW; virtual; abstract;
    function definition : String; virtual; abstract;
    function itemWeight : String; virtual; abstract;
    function conceptList : TFhirCodeSystemConceptListW; virtual; abstract;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; virtual; abstract;
    function conceptCount : integer; virtual; abstract;
    function hasConcepts : boolean; virtual; abstract;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function designationCount : integer; virtual; abstract;
    function designations : TFslList<TFhirCodeSystemConceptDesignationW>; virtual; abstract;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; virtual; abstract;
    function displayTag(tag : String) : String; virtual; abstract;
    procedure setDisplayTag(tag, value : String); virtual; abstract;
    function getCode(code : String) : TFhirCodeSystemConceptW; virtual; abstract;
  end;
  TFhirCodeSystemConceptMapW = TFslMap<TFhirCodeSystemConceptW>;

  TFhirCodeSystemPropertyType = (cptNull, cptCode, cptCoding, cptString, cptInteger, cptBoolean, cptDateTime, cptDecimal);
  TFhirCodeSystemPropertyW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodeSystemPropertyW; overload;
    function code : String; virtual; abstract;
    function uri : String; virtual; abstract;
    function type_ : TFhirCodeSystemPropertyType; virtual; abstract;
  end;

  TFhirCodeSystemContentMode = (cscmNull, cscmNotPresent, cscmExample, cscmFragment, cscmComplete, cscmSupplement);
  TFhirCodeSystemContentModeSet = set of TFhirCodeSystemContentMode;

const
  CODES_TFhirCodeSystemContentMode : Array[TFhirCodesystemContentMode] of String = ('null', 'not-present', 'example', 'fragment', 'complete', 'supplement');
  ALL_TFhirCodeSystemContentMode = [cscmNull..cscmSupplement];

type
  TFHIRValueSetW = class;

  TFHIRMetadataResourceW = class (TFHIRXVersionResourceWrapper)
  private
    function GetVUrl: String;
  protected
    function getURL: String; virtual; abstract;
    function getName: String; virtual; abstract;
    function getTitle: String; virtual; abstract;
    function getStatus: TPublicationStatus; virtual; abstract;
    function getVersion: String; virtual; abstract;
    function getDescription: String; virtual; abstract;
    function getDate: TFslDateTime; virtual; abstract;
    function getPublisher: String; virtual; abstract;
    function getExperimental : boolean; virtual; abstract;
    procedure setPublisher(Value: String); virtual; abstract;
    procedure setDate(Value: TFslDateTime); virtual; abstract;
    procedure setUrl(Value: String); virtual; abstract;
    procedure setVersion(Value: String); virtual; abstract;
    procedure setName(Value: String); virtual; abstract;
    procedure setTitle(Value: String); virtual; abstract;
    procedure setStatus(Value: TPublicationStatus); virtual; abstract;
    procedure setDescription(Value: String); virtual; abstract;
    function getContext: String; virtual; abstract;
    procedure setExperimental(value : boolean); virtual; abstract;
  public
    function link : TFHIRMetadataResourceW; overload;

    property url : String read getURL write SetUrl;
    property name : String read GetName write SetName;
    property title : String read GetTitle write SetTitle;
    property version : String read GetVersion write SetVersion;
    property vurl : String read GetVUrl;
    property status : TPublicationStatus read GetStatus write SetStatus;
    property description : String read GetDescription write SetDescription;
    property date : TFslDateTime read GetDate write SetDate;
    property context : String read getContext;
    property publisher : String read GetPublisher write SetPublisher;
    property experimental : boolean read getExperimental write setExperimental;
  end;

  TFslMetadataResourceList = class (TFslList<TFHIRMetadataResourceW>)
  public
    function link : TFslMetadataResourceList; overload;
  end;

  { TFhirCodeSystemW }

  TFhirCodeSystemW = class (TFHIRMetadataResourceW)
  protected
    FConceptList : TFhirCodeSystemConceptListW;
    function GetCaseSensitive: boolean; virtual; abstract;
    function getContent: TFhirCodeSystemContentMode; virtual; abstract;
    procedure setContent(Value: TFhirCodeSystemContentMode); virtual; abstract;
    function getCount: integer; virtual; abstract;
    procedure setCount(Value: integer); virtual; abstract;
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    destructor Destroy; override;
    function link : TFhirCodeSystemW; overload;
    property content : TFhirCodeSystemContentMode read getContent write SetContent;
    property count : integer read GetCount write SetCount;

    function valueSet : String; virtual; abstract;
    function supplements : String; virtual; abstract;
    function copyright : String; virtual; abstract;
    function language : String; virtual; abstract;
    property caseSensitive : boolean read GetCaseSensitive;

    function properties : TFslList<TFhirCodeSystemPropertyW>;  virtual; abstract;
    function propertyCode(uri : String) : String;

    // this is special because it's owned
    function conceptList : TFhirCodeSystemConceptListW; virtual; abstract;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; virtual; abstract;
    function hasConcepts : boolean; virtual; abstract;
    function conceptCount : integer; virtual; abstract;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function getCode(code : String) : TFhirCodeSystemConceptW; virtual; abstract;

    function isAbstract(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function isInactive(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function codeStatus(c : TFhirCodeSystemConceptW) : String; virtual; abstract;
    function isDeprecated(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function getParents(c : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptListW; virtual; abstract;
    function getChildren(c : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptListW; virtual; abstract;

    function buildImplicitValueSet : TFHIRValueSetW; virtual; abstract;
    function hasAnyDisplays(langs : THTTPLanguageList) : boolean; virtual; abstract;
  end;

  { TFhirValueSetExpansionContainsPropertyW }

  TFhirValueSetExpansionContainsPropertyW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetExpansionContainsPropertyW; overload;
    function code : String; virtual; abstract;
    function value : TFHIRObject; virtual; abstract;
  end;


  TFhirValueSetExpansionContainsW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetExpansionContainsW; overload;
    function getSystem : String; virtual; abstract;
    function getVersion: String; virtual; abstract;
    function getCode : String; virtual; abstract;
    function getDisplay : String; virtual; abstract;
    function GetItemWeight : String; virtual; abstract;
    function GetAbstract : boolean; virtual; abstract;
    function GetInactive : boolean; virtual; abstract;
    procedure setCode(Value: String); virtual; abstract;
    procedure setDisplay(Value: String); virtual; abstract;
    procedure setSystem(Value: String); virtual; abstract;
    procedure setVersion(Value: String); virtual; abstract;
    procedure SetItemWeight(Value: String); virtual; abstract;
    procedure SetAbstract(Value: boolean); virtual; abstract;
    procedure SetInactive(Value: boolean); virtual; abstract;

    property systemUri : String read GetSystem write SetSystem;
    property version : String read GetVersion write SetVersion;
    property code : String read GetCode write SetCode;
    property display : String read GetDisplay write SetDisplay;
    property abstract_ : boolean read GetAbstract write SetAbstract;
    property inactive : boolean read GetInactive write SetInactive;
    property itemWeight : String read GetItemWeight write SetItemWeight;

    procedure addDesignation(lang, use, value : String); overload; virtual; abstract;
    procedure addDesignation(lang : TIETFLang; use : TFHIRCodingW; value : TFHIRPrimitiveW; extensions : TFslList<TFHIRExtensionW>); overload; virtual; abstract;
    procedure addProperty(code : String; value : TFHIRObject); overload; virtual; abstract;
    procedure addProperty(code : String; value : TFhirCodeSystemConceptPropertyW); overload; virtual; abstract;
    procedure addContains(contained : TFhirValueSetExpansionContainsW); virtual; abstract;
    procedure clearContains(); virtual; abstract;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; virtual; abstract;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; virtual; abstract;
  end;

  TFhirValueSetExpansionW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetExpansionW; overload;
    // todo: decimal and datetime
    procedure addParamStr(name, value : String); overload; virtual; abstract;
    procedure addParamCode(name, value : String); overload; virtual; abstract;
    procedure addParamUri(name, value : String); overload; virtual; abstract;
    procedure addParamBool(name : String; value : boolean); overload; virtual; abstract;
    procedure addParamInt(name : String; value : integer); overload; virtual; abstract;

    function hasParam(name : string) : boolean; overload; virtual; abstract;
    function hasParam(name, value : string) : boolean; overload; virtual; abstract;
    procedure copyParams(source : TFhirValueSetExpansionW); virtual; abstract;
    procedure addContains(item : TFhirValueSetExpansionContainsW); overload; virtual; abstract;
    function makeContains : TFhirValueSetExpansionContainsW; overload; virtual; abstract;
    function addContains : TFhirValueSetExpansionContainsW; overload; virtual; abstract;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; virtual; abstract;

    function getTotal : integer; virtual; abstract;
    procedure setTotal(value : integer) ; virtual; abstract;

    function getOffset : integer; virtual; abstract;
    procedure setOffset(value : integer) ; virtual; abstract;

    property total : integer read getTotal write setTotal;
    property offset : integer read getOffset write setOffset;

    procedure defineProperty(focus : TFhirValueSetExpansionContainsW; url, code : String; value : TFHIRObject {link if needed}); virtual; abstract;
  end;

  TFhirValueSetComposeIncludeFilterW = class (TFHIRXVersionElementWrapper)
  protected
    function getProp : String; virtual; abstract;
    function getOp : TFilterOperator; virtual; abstract;
    function getValue : String; virtual; abstract;
    procedure setOp(Value: TFilterOperator); virtual; abstract;
    procedure setProp(Value: String); virtual; abstract;
    procedure setValue(Value: String); virtual; abstract;
  public
    function link : TFhirValueSetComposeIncludeFilterW; overload;
    property prop : String read GetProp write SetProp;
    property op : TFilterOperator read GetOp write SetOp;
    property value : String read GetValue write SetValue;
  end;

  TFhirValueSetComposeIncludeConceptDesignationW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetComposeIncludeConceptDesignationW; overload;
    function language : String; virtual; abstract;
    function use : TFHIRCodingW; virtual; abstract;
    function value : String; virtual; abstract;
    function valueElement : TFHIRPrimitiveW; virtual; abstract;
  end;

  TFhirValueSetComposeIncludeConceptW = class (TFHIRXVersionElementWrapper)
  protected
    function getCode : String; virtual; abstract;
    function getDisplay : String; virtual; abstract;
    function GetItemWeight : String; virtual; abstract;
    procedure setCode(Value: String); virtual; abstract;
    procedure setDisplay(Value: String); virtual; abstract;
    procedure SetItemWeight(Value: String); virtual; abstract;
  public
    function link : TFhirValueSetComposeIncludeConceptW; overload;
    property code : String read getCode write SetCode;
    property display : String read GetDisplay write SetDisplay;
    function displayElement : TFHIRPrimitiveW; virtual; abstract;
    property itemWeight : String read GetItemWeight write SetItemWeight;
    function designations : TFslList<TFhirValueSetComposeIncludeConceptDesignationW>; virtual; abstract;
  end;

  TFhirValueSetComposeIncludeW = class (TFHIRXVersionElementWrapper)
  protected
    function getSystem : String; virtual; abstract;
    function getVersion : String; virtual; abstract;
    procedure setSystem(Value: String); virtual; abstract;
    procedure setVersion(Value: String); virtual; abstract;
  public
    function link : TFhirValueSetComposeIncludeW; overload;

    property systemUri : String read GetSystem write SetSystem;
    property version : String read GetVersion write SetVersion;
    function hasValueSets : boolean; virtual; abstract;
    function valueSets : TArray<String>; virtual; abstract;
    function hasConcepts : boolean; virtual; abstract;
    function concepts : TFslList<TFhirValueSetComposeIncludeConceptW>; virtual; abstract;
    function addConcept : TFhirValueSetComposeIncludeConceptW; virtual; abstract;
    function hasFilters : boolean; virtual; abstract;
    function filterCount : integer; virtual; abstract;
    function conceptCount : integer; virtual; abstract;
    function filters : TFslList<TFhirValueSetComposeIncludeFilterW>; virtual; abstract;
    function addFilter : TFhirValueSetComposeIncludeFilterW; virtual; abstract;
  end;

  TFHIRValueSetCodeSystemW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFHIRValueSetCodeSystemW; overload;
    function systemUri : String; virtual; abstract;
    function concepts : TFhirCodeSystemConceptListW; virtual; abstract;
  end;

  TFhirValueSetW =  class (TFHIRMetadataResourceW)
  public
    function link : TFhirValueSetW; overload;
    function source : String; virtual; abstract;

    function checkCompose(place, role : String) : boolean; virtual; abstract;
    function getComposeExtensions : TFslList<TFHIRExtensionW>; virtual; abstract;
    function checkExpansion(place, role : String) : boolean; virtual; abstract;
    function imports : TArray<String>; virtual; abstract; // only in R2
    function excludeInactives : boolean; virtual; abstract;
    function hasInlineCS : boolean; virtual; abstract;
    function inlineCS : TFHIRValueSetCodeSystemW; virtual; abstract;
    function includes : TFslList<TFhirValueSetComposeIncludeW>; virtual; abstract;
    function addInclude : TFhirValueSetComposeIncludeW; virtual; abstract; {      result.compose := TFhirValueSetCompose.Create;     inc := result.addInclude; compose.includeList.Append; }
    function excludes : TFslList<TFhirValueSetComposeIncludeW>; virtual; abstract;

    procedure clearDefinition; virtual; abstract;
    procedure clearDefinitionExtensions(exemptUrls : TStringArray); virtual; abstract;
    function hasExpansion : boolean; virtual; abstract;
    function expansion : TFhirValueSetExpansionW; virtual; abstract;
    function forceExpansion : TFhirValueSetExpansionW; virtual; abstract;
    function findContains(systemUri, version, code : String) : TFhirValueSetExpansionContainsW; virtual; abstract;
  end;

  TElementDefinitionBinding = (edbNone, edbRequired, edbExtensible, edbPreferred, edpExample);

  TFHIRElementDefinitionW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFHIRElementDefinitionW; overload;
    function path : String; virtual; abstract;
    function min : integer; virtual; abstract;
    function max : integer; virtual; abstract;
    function defn : String; virtual; abstract;
    function types : String; virtual; abstract;
    function typeList : TArray<String>; virtual; abstract;
    function explicitTypeName : String; virtual; abstract;
    function isSummary : boolean; virtual; abstract;
    function binding : TElementDefinitionBinding; virtual; abstract;
    function valueSet : String; virtual; abstract;
  end;

  TStructureDefinitionKind = (sdkPrimitive, sdkDataType, sdkExtension, sdkResource);
  TElementDefinitionSourceOption  = (edsSNAPSHOT, edsDIFF, edsEITHER);

  TFhirStructureDefinitionW =  class (TFHIRXVersionResourceWrapper)
  public
    function kind : TStructureDefinitionKind; virtual; abstract;
    function name : String; virtual; abstract;
    function url : String; virtual; abstract;
    function type_ : String; virtual; abstract;
    function elements : TFslList<TFHIRElementDefinitionW>; virtual; abstract;
    function getDefinition(id : String; source : TElementDefinitionSourceOption) : TFHIRElementDefinitionW; virtual; abstract;
  end;

  TFHIRGroupCharacteristicW = class (TFHIRXVersionElementWrapper)
  public
    function Link : TFHIRGroupCharacteristicW; overload;
    function code : TFhirCodeableConceptW; virtual; abstract;
    function value : TFhirCodeableConceptW; virtual; abstract;
  end;

  TFHIRGroupW = class (TFHIRXVersionResourceWrapper)
  public
    function Link : TFHIRGroupW; overload;
    function name : String; virtual; abstract;
    function hasMembers : boolean; virtual; abstract;
    function hasCharacteristics : boolean; virtual; abstract;
    function characteristics : TFslList<TFHIRGroupCharacteristicW>; virtual; abstract;
  end;

  TFhirPatientW = class (TFHIRXVersionResourceWrapper)
  protected
    function GetActive: boolean; virtual; abstract;
    procedure SetActive(const Value: boolean); virtual; abstract;
    function GetFamily: String; virtual; abstract;
    procedure SetFamily(const Value: String); virtual; abstract;
    function GetDob: String; virtual; abstract;
    procedure SetDob(const Value: String); virtual; abstract;
    function GetIdentifier(systemUri: String): String; virtual; abstract;
    procedure SetIdentifier(systemUri: String; const Value: String); virtual; abstract;
  public
    function Link : TFhirPatientW; overload;
    property family : String read GetFamily write SetFamily;
    procedure addGiven(name : String); virtual; abstract;
    function nameSummary : String; virtual; abstract;
    property active : boolean read GetActive write SetActive;
    function activeStr : String; virtual; abstract;
    function gender : String; virtual; abstract;
    function genderPlus : String; virtual; abstract;
    property dob : String read GetDob write SetDob;
    property identifier[systemUri : String] : String read GetIdentifier write SetIdentifier;

    function identifierSummary : String; virtual; abstract;
    function contactSummary : String; virtual; abstract;
  end;

  TFhirEncounterW = class (TFHIRXVersionResourceWrapper)
  public
    function Link : TFhirEncounterW; overload;
    function patientId : String; virtual; abstract;
    function summary : String; virtual; abstract;
  end;

  { TFHIRLookupOpRespSubPropertyW }

  TFHIRLookupOpRespSubPropertyW = class (TFHIRXVersionOperationObjectWrapper)
  public
    function link : TFHIRLookupOpRespSubPropertyW; overload;
    function getDescription: string; virtual; abstract;
    procedure setDescription(Value: string); virtual; abstract;
    function getValue: String; virtual; abstract;
    procedure setValue(Value: String); virtual; abstract;

    property description : string read GetDescription write SetDescription;
    property value : String read GetValue write SetValue;
  end;

  TFHIRLookupOpRespPropertyW = class (TFHIRXVersionOperationObjectWrapper)
  public
    function link : TFHIRLookupOpRespPropertyW; overload;
    function getDescription: string; virtual; abstract;
    procedure setDescription(Value: string); virtual; abstract;
    function getValue: TFHIRObject; virtual; abstract;
    procedure setValue(Value: TFHIRObject); virtual; abstract;

    property description : string read GetDescription write SetDescription;
    property value : TFHIRObject read GetValue write SetValue;

    function addSubProp(name : String) : TFHIRLookupOpRespSubPropertyW; virtual; abstract;
  end;

  TFHIRLookupOpRespDesignationW = class (TFHIRXVersionOperationObjectWrapper)
  public
    function link : TFHIRLookupOpRespDesignationW; overload;
    function getUse: TFHIRObject; virtual; abstract;
    procedure setUse(Value: TFHIRObject); virtual; abstract;

    property use : TFHIRObject read GetUse write SetUse;
  end;

  TFHIRLookupOpRequestW = class (TFHIRXVersionOperationWrapper)
  public
    function link : TFHIRLookupOpRequestW; overload;
    procedure loadCoding; virtual; abstract;
    function coding : TFHIRCodingW; virtual; abstract;
    function propList : TArray<String>; virtual; abstract;
    function displayLanguage : String; virtual; abstract;
  end;

  { TFHIRLookupOpResponseW }

  TFHIRLookupOpResponseW = class (TFHIRXVersionOperationWrapper)
  protected
    function getName: String; virtual; abstract;
    procedure setName(Value: String); virtual; abstract;
    function getCode: String; virtual; abstract;
    procedure setCode(Value: String); virtual; abstract;
    function getSystem: String; virtual; abstract;
    procedure setSystem(Value: String); virtual; abstract;
    function getDisplay: String; virtual; abstract;
    procedure setDisplay(Value: String); virtual; abstract;
    function getIsAbstract: boolean; virtual; abstract;
    procedure setIsAbstract(Value: boolean); virtual; abstract;
  public
    function link : TFHIRLookupOpResponseW; overload;

    function addProp(name : string) : TFHIRLookupOpRespPropertyW; virtual; abstract;
    function addDesignation(lang, systemUri, code, display, value : string) : TFHIRLookupOpRespDesignationW; overload; virtual; abstract;
    function addDesignation(lang, value : string) : TFHIRLookupOpRespDesignationW; overload; virtual; abstract;
    function getVersion: String; virtual; abstract;
    procedure setVersion(Value: String); virtual; abstract;
    procedure addExtension(name, value : String); overload; virtual; abstract;
    procedure addExtension(name : String; value : boolean); overload; virtual; abstract;

    property isAbstract : boolean read GetIsAbstract write SetIsAbstract;
    property systemUri : String read GetSystem write SetSystem;
    property version : String read GetVersion write SetVersion;
    property name : String read GetName write SetName;
    property code : String read GetCode write SetCode;
    property display : String read GetDisplay write SetDisplay;
  end;

  TFHIRSubsumesOpRequestW = class (TFHIRXVersionOperationWrapper)
  public
    function systemUri : String; virtual; abstract;
    function codeA : String; virtual; abstract;
    function codeB : String; virtual; abstract;
    function version : String; virtual; abstract;
    function hasCodingA : boolean; virtual; abstract;
    function hasCodingB : boolean; virtual; abstract;
    function codingA : TFHIRCodingW; virtual; abstract;
    function codingB : TFHIRCodingW; virtual; abstract;
  end;

  TFHIRSubsumesOpResponseW = class (TFHIRXVersionOperationWrapper)
  protected
    function getOutcome: String; virtual; abstract;
    procedure setOutcome(Value: String); virtual; abstract;
  public
    property outcome : String read GetOutcome write SetOutcome;
  end;

  TFhirTestScriptW = class (TFHIRMetadataResourceW)
  end;

  TFhirProvenanceW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirProvenanceW; overload;

    procedure clearTargets; virtual; abstract;
    procedure clearSignatures; virtual; abstract;
    procedure addTarget(url : String); virtual; abstract;
  end;

  TFHIRConceptEquivalence = (cmeNull, cmeRelatedto, cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact, cmeUnmatched, cmeDisjoint);

const
  CODES_TFHIRConceptEquivalence : Array [TFHIRConceptEquivalence] of String = ('Null', 'Relatedto', 'Equivalent', 'Equal', 'Wider', 'Subsumes', 'Narrower', 'Specializes', 'Inexact', 'Unmatched', 'Disjoint');
  ALL_TFHIRConceptEquivalence = [cmeNull..cmeDisjoint];

type
  TFhirConceptMapGroupElementDependsOnW = class (TFHIRXVersionElementWrapper)
  public
    function property_ : String; virtual; abstract;
    function system_ : String; virtual; abstract;
    function value : String; virtual; abstract;
    function display : String; virtual; abstract;
  end;

  TFhirConceptMapGroupElementTargetW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirConceptMapGroupElementTargetW; overload;
    function code: String; virtual; abstract;
    function equivalence : TFHIRConceptEquivalence; virtual; abstract;
    function comments : String; virtual; abstract;
    function products : TFslList<TFhirConceptMapGroupElementDependsOnW>; virtual; abstract;
  end;

  TFhirConceptMapGroupElementW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirConceptMapGroupElementW; overload;
    function code: String; virtual; abstract;
    function targets : TFslList<TFhirConceptMapGroupElementTargetW>; virtual; abstract;
    function targetCount : integer; virtual; abstract;
    function addTarget(code : String; eq : TFHIRConceptEquivalence) : TFhirConceptMapGroupElementTargetW; virtual; abstract;
  end;

  TFhirConceptMapGroupW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirConceptMapGroupW; overload;
    function elements : TFslList<TFhirConceptMapGroupElementW>; virtual; abstract;
    function addElement(code : String) : TFhirConceptMapGroupElementW; virtual; abstract;
    function source : String; virtual; abstract;
    function target : String; virtual; abstract;
  end;

  TFhirConceptMapW = class (TFHIRMetadataResourceW)
  public
    function link : TFhirConceptMapW; overload;

    function sourceDesc : String; virtual; abstract;
    function targetDesc : String; virtual; abstract;

    function source : String; virtual; abstract;
    function target : String; virtual; abstract;

    function groups : TFslList<TFhirConceptMapGroupW>; virtual; abstract;
    function addGroup(source, target : String) : TFhirConceptMapGroupW; virtual; abstract;
  end;

  TFHIROperationDefinitionW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFHIROperationDefinitionW; overload;
  end;

  TFhirQuestionnaireW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirQuestionnaireW; overload;
  end;

  TFHIRNamingSystemW = class (TFHIRMetadataResourceW)
  public
    function link : TFHIRNamingSystemW; overload;
    function getUri : String; virtual; abstract;
    function hasOid(oid : String) : boolean; virtual; abstract;
  end;

  TFHIRStructureMapW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFHIRStructureMapW; overload;
    function url : String; virtual; abstract;
  end;

  TFhirEventDefinitionW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirEventDefinitionW; overload;

    function language : String; virtual; abstract;
    function triggerType : TTriggerType; virtual; abstract;
    function expression : String; virtual; abstract;
    function dataType : String; virtual; abstract;
  end;

  TFhirAuditEventW  = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirAuditEventW; overload;
    procedure success; virtual; abstract;
    procedure eventType(systemUri, code, display : String); virtual; abstract;
    procedure eventSubType(systemUri, code, display : String); virtual; abstract;
    procedure source(name, systemUri, value : String); virtual; abstract;
    procedure sourceType(systemUri, code, display : String); virtual; abstract;
    procedure participantIp(ip : String); virtual; abstract;
    procedure participantId(systemUri, value, alt, name : String); virtual; abstract;

    function dateTime : TFslDateTime; virtual; abstract;
  end;

  TFHIRSubscriptionW = class (TFHIRXVersionResourceWrapper)
  protected
    function getTopic: string; virtual; abstract;
    function getCriteria: String; virtual; abstract;
    function getDirect: boolean; virtual; abstract;
    function getEndpoint: String; virtual; abstract;
    function getError: String; virtual; abstract;
    function getMethod: TSubscriptionMethod; virtual; abstract;
    function getPayload: String; virtual; abstract;
    function getStatus: TSubscriptionStatus; virtual; abstract;
    function getSummary: String; virtual; abstract;
    function getHeaders: TArray<String>; virtual; abstract;
    procedure setCriteria(Value: String); virtual; abstract;
    procedure setDirect(Value: boolean); virtual; abstract;
    procedure setEndpoint(Value: String); virtual; abstract;
    procedure setError(Value: String); virtual; abstract;
    procedure setheaders(Value: TArray<String>); virtual; abstract;
    procedure setMethod(Value: TSubscriptionMethod); virtual; abstract;
    procedure setPayload(Value: String); virtual; abstract;
    procedure setStatus(Value: TSubscriptionStatus); virtual; abstract;
  public
    function link : TFHIRSubscriptionW; overload;
    property topic : String read getTopic;
    property criteria : String read GetCriteria write SetCriteria;
    property summary : String read GetSummary;
    property status : TSubscriptionStatus read GetStatus write SetStatus;
    property headers : TArray<String> read GetHeaders write Setheaders;
    property payload : String read GetPayload write SetPayload;
    property endpoint : String read GetEndpoint write SetEndpoint;
    property direct : boolean read GetDirect write SetDirect;
    property method : TSubscriptionMethod read GetMethod write SetMethod;
    property error : String read GetError write SetError;
  end;

  TFHIRSubscriptionTopicW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFHIRSubscriptionTopicW; overload;
  end;

  TFHIRStatsOpResponseW = class (TFHIRXVersionOperationWrapper)
  public
    function link : TFHIRStatsOpResponseW; overload;
    procedure addObs(obs : TFHIRResourceV); virtual; abstract;
  end;

  TFhirObservationComponentW = class (TFHIRXVersionElementWrapper)
  protected
    function getValue: TFHIRObject;  virtual; abstract;
    procedure setValue(Value: TFHIRObject); virtual; abstract;
  public
    function link : TFhirObservationComponentW; overload;
    function codings : TFslList<TFHIRCodingW>; virtual; abstract;
    property value : TFHIRObject read GetValue write SetValue;
    function valueW : TFHIRXVersionElementWrapper; virtual; abstract;
    function valueString : String; virtual; abstract;
    function dataAbsentReason : TFhirCodeableConceptW; virtual; abstract;
  end;

  TFhirObservationW = class (TFHIRXVersionResourceWrapper)
  private
  protected
    function GetDevice: String; virtual; abstract;
    procedure SetDevice(const Value: String); virtual; abstract;
    function GetDeviceName: String; virtual; abstract;
    procedure SetDeviceName(const Value: String); virtual; abstract;
    function GetSubject: String; virtual; abstract;
    procedure SetSubject(const Value: String); virtual; abstract;
    function getStatus: TObservationStatus; virtual; abstract;
    procedure setStatus(Value: TObservationStatus); virtual; abstract;
    function GetIssued: TFslDateTime; virtual; abstract;
    procedure SetIssued(const Value: TFslDateTime); virtual; abstract;
    function getValue: TFHIRObject;  virtual; abstract;
    procedure setValue(Value: TFHIRObject); virtual; abstract;
    function GetEffective: TFHIRObject; virtual; abstract;
    function GetEffectiveDateTime: TFslDateTime; virtual; abstract;
    function GetEffectivePeriod: TFHIRPeriodW; virtual; abstract;
    procedure SetEffective(const Value: TFHIRObject); virtual; abstract;
    procedure SetEffectiveDateTime(const Value: TFslDateTime); virtual; abstract;
    procedure SetEffectivePeriod(const Value: TFHIRPeriodW); virtual; abstract;
    function GetCodeText: String; virtual; abstract;
    procedure SetCodeText(const Value: String); virtual; abstract;
    function GetComment: String; virtual; abstract;
    procedure SetComment(const Value: String); virtual; abstract;
  public
    function link : TFhirObservationW; overload;
    property status : TObservationStatus read GetStatus write SetStatus;
    procedure setCode(c : TFHIRCodingW); overload; virtual; abstract;
    procedure setCode(systemUri, code, display : String); overload; virtual; abstract;
    procedure setCode(text : String); overload; virtual; abstract;
    procedure addCategory(c : TFHIRCodingW); overload; virtual; abstract;
    procedure addCategory(systemUri, code, display : String); overload; virtual; abstract;
    procedure setPeriod(start, finish : TDateTime); virtual; abstract;

    function hasDevice : boolean; virtual; abstract;
    function hasIssued : boolean; virtual; abstract;
    function hasMethod : boolean; virtual; abstract;
    function hasSubject : boolean; virtual; abstract;
    function hasEffective : boolean; virtual; abstract;

    property subject : String read GetSubject write SetSubject;
    property device : String read GetDevice write SetDevice;
    property deviceName : String read GetDeviceName write SetDeviceName;
    property issued : TFslDateTime read GetIssued write SetIssued;
    property effective : TFHIRObject read GetEffective write SetEffective;
    property effectiveDateTime : TFslDateTime read GetEffectiveDateTime write SetEffectiveDateTime;
    property effectivePeriod : TFHIRPeriodW read GetEffectivePeriod write SetEffectivePeriod;
    property codeText : String read GetCodeText write SetCodeText;
    function method(force : boolean) : TFhirCodeableConceptW; virtual; abstract;
    function categories : TFslList<TFHIRCodingW>; virtual; abstract;
    function codings : TFslList<TFHIRCodingW>; virtual; abstract;
    function hasTime : boolean;  virtual; abstract;
    procedure getDates(var dt, dtMin, dtMax : TDateTime); virtual; abstract;
    property value : TFHIRObject read GetValue write SetValue;
    function valueW : TFHIRXVersionElementWrapper; virtual; abstract;
    function dataAbsentReason : TFhirCodeableConceptW; virtual; abstract;
    property comment : String read GetComment write SetComment;

    function components : TFslList<TFhirObservationComponentW>; virtual; abstract;
    function getComponent(systemUri, code: String; var comp : TFhirObservationComponentW) : boolean; overload; virtual; abstract;
    function getComponent(systemUri : String; var comp : TFhirObservationComponentW) : boolean; overload; virtual; abstract;
    function addComp(systemUri, code : String) : TFhirObservationComponentW; virtual; abstract;
  end;

  TFhirGraphDefinitionW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirGraphDefinitionW; overload;
  end;

  // sometimes you just need an object, but with no functionality on it. that's what this is for
  TFHIRNullObject = class (TFHIRObject)
  public
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    function getPropertyValue(propName : string): TFHIRProperty; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function fhirType : String; override;
    function getId : String; override;
    function hasExtensions : boolean; override;
    procedure setIdValue(id : String); override;
  end;

  TFhirTerminologyCapabilitiesW = class (TFHIRXVersionResourceWrapper)
  protected
    function getDate: TFslDateTime; virtual; abstract;
    function getDescription: String; virtual; abstract;
    function getName: String; virtual; abstract;          
    function getTitle: String; virtual; abstract;
    function getStatus: TPublicationStatus; virtual; abstract;
    function getURL: String; virtual; abstract;
    procedure setDate(Value: TFslDateTime); virtual; abstract;
    procedure setDescription(Value: String); virtual; abstract;
    procedure setName(Value: String); virtual; abstract;
    procedure setTitle(Value: String); virtual; abstract;
    procedure setStatus(Value: TPublicationStatus); virtual; abstract;
    procedure setUrl(Value: String); virtual; abstract;
    function getContext: String; virtual; abstract;
    procedure setContext(Value: String); virtual; abstract;
    function getPublisher: String; virtual; abstract;
    procedure setPublisher(Value: String); virtual; abstract;
    function getVersion: String; virtual; abstract;
    procedure setVersion(Value: String); virtual; abstract;
  public
    function link : TFhirTerminologyCapabilitiesW; overload;

    property url : String read getURL write SetUrl;
    property name : String read GetName write SetName;      
    property title : String read GetTitle write SetTitle;
    property version : String read GetVersion write SetVersion;
    property status : TPublicationStatus read GetStatus write SetStatus;
    property description : String read GetDescription write SetDescription;
    property date : TFslDateTime read GetDate write SetDate;
    property context : String read getContext write SetContext;
    property publisher : String read GetPublisher write SetPublisher;

    procedure contact(kind : TContactType; value : String); virtual; abstract;
    procedure systemUri(url : String); virtual; abstract;
    procedure addExpansionParameter(code, doco : String); virtual; abstract;
  end;

  TFhirConsentProvisionAction = (cpaRead, capStore, cpaChange);

  TFhirConsentProvisionW = class abstract (TFHIRXVersionElementWrapper)
  private
    function GetAction: TFhirConsentProvisionAction; virtual; abstract;
    function GetAuthor: String; virtual; abstract;
    function GetDataPeriodEnd: TFslDateTime; virtual; abstract;
    function GetDataPeriodStart: TFslDateTime; virtual; abstract;
    function GetHasDataPeriod: boolean; virtual; abstract;
    function GetHasProvisionPeriod: boolean; virtual; abstract;
    function GetPermit: boolean; virtual; abstract;
    function GetProvisionPeriodEnd: TFslDateTime; virtual; abstract;
    function GetProvisionPeriodStart: TFslDateTime; virtual; abstract;
    function GetRecipient: String; virtual; abstract;
  public
    function link : TFhirConsentProvisionW; overload;
    property permit : boolean read GetPermit;
    property hasProvisionPeriod : boolean read GetHasProvisionPeriod;
    property provisionPeriodStart : TFslDateTime read GetProvisionPeriodStart;
    property provisionPeriodEnd : TFslDateTime read GetProvisionPeriodEnd;
    property author : String read GetAuthor;
    property recipient : String read GetRecipient;
    property action : TFhirConsentProvisionAction read GetAction;
    property hasDataPeriod : boolean read GetHasDataPeriod;
    property dataPeriodStart : TFslDateTime read GetDataPeriodStart;
    property dataPeriodEnd : TFslDateTime read GetDataPeriodEnd;
    function listProvisions : TFslList<TFhirConsentProvisionW>; virtual; abstract;
  end;

  TFhirConsentW = class abstract (TFHIRXVersionResourceWrapper)
  protected
    function GetActive: boolean; virtual; abstract;
    function GetPatient: String; virtual; abstract;
    function GetDateTime: TFslDateTime; virtual; abstract;
  public
    function link : TFhirConsentW; overload;

    property active : boolean read GetActive;
    property patient : String read GetPatient;
    property dateTime : TFslDateTime read GetDateTime;
    function listProvisions : TFslList<TFhirConsentProvisionW>; virtual; abstract;
  end;

  { TFHIRResourceProxyV }

  TFHIRResourceProxyV = class abstract (TFslObject)
  private
    FContent: String;
    FFhirObjectVersion : TFHIRVersion;
    FFhirType : String;
    FId  : String;
    FPackageId: String;
    FSupplements: String;
    FUrl : String;
    FVersion : String;
    FValueSet : String;
    FResourceW : TFHIRXVersionResourceWrapper;
    function GetResourceV : TFHIRResourceV;
    function GetResourceW : TFHIRXVersionResourceWrapper;
  protected
    FResourceV : TFHIRResourceV;
    procedure loadResource; virtual; abstract;
    function wrapResource : TFHIRXVersionResourceWrapper; virtual; abstract;
    procedure SetResourceV(value : TFHIRResourceV);
  public
    constructor Create(packageId : String; fhirObjectVersion : TFHIRVersion; fhirType, id : String; url, version, supplements, content, valueSet : String); overload;
    constructor Create(packageId : String; resource : TFHIRResourceV; url, version : String); overload;
    destructor Destroy; override;
    function link : TFHIRResourceProxyV; overload;

    property packageId : String read FPackageId;
    property fhirObjectVersion : TFHIRVersion read FFhirObjectVersion;
    property fhirType : String read FFhirType;
    property id  : String read FId write FId;
    property url : String read FUrl;
    property version : String read FVersion;
    property supplements : String read FSupplements;
    property content : String read FContent;
    property valueSet : String read FValueSet;

    property resourceV : TFHIRResourceV read GetResourceV;
    property resourceW : TFHIRXVersionResourceWrapper read getResourceW;
  end;

  { TFHIRResourceProxyW }

  TFHIRResourceProxyW = class (TFHIRResourceProxyV)
  protected
    procedure loadResource; override;
    function wrapResource : TFHIRXVersionResourceWrapper; override;
  public
    constructor Create(packageId : String; resource : TFHIRXVersionResourceWrapper; url, version : String); overload;
  end;

  TFHIRMetadataResourceManagerW<T : TFHIRMetadataResourceW> = class (TFslObject)
  private
    FMap : TFslMap<TFHIRResourceProxyV>;
    FList : TFslList<TFHIRResourceProxyV>;
    procedure updateList(url, version: String);
    {$IFDEF FPC}
    function Compare(sender : TObject; const l, r : TFHIRResourceProxyV) : integer;
    {$ENDIF}
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function link : TFHIRMetadataResourceManagerW<T>; overload;
    function clone : TFHIRMetadataResourceManagerW<T>; overload;
    procedure Assign(oSource : TFslObject); override;

    procedure see(res: TFHIRResourceProxyV);
    procedure drop(id : String);
    function get(url: String): T; overload;
    function getP(url: String): TFHIRResourceProxyV; overload;
    function get(url, version: String): T; overload;
    function has(url: String): boolean; overload;
    function has(url, version: String): boolean; overload;
    function has(url: String; var res : T): boolean; overload;
    function has(url, version: String; var res : T): boolean; overload;
    function count: integer;
    procedure clear;
    procedure listAll(list: TFslList<T>);
    procedure listAllM(list: TFslMetadataResourceList);
  end;

  TFHIRImmunizationW = class abstract (TFHIRXVersionResourceWrapper)
  private
    function  GetCvxCode: String;
    procedure SetCvxCode(const Value: String);
    function  GetIcd11Code: String;
    procedure SetIcd11Code(const Value: String);
    function  GetSctCode: String;
    procedure SetSctCode(const Value: String);
  protected
    function GetLotNumber: String; virtual; abstract;
    function GetPatient: String; virtual; abstract;
    function GetPerformerDisplay: String; virtual; abstract;
    function GetStatus: string; virtual; abstract;
    procedure SetLotNumber(const Value: String); virtual; abstract;
    procedure SetPatient(const Value: String); virtual; abstract;
    procedure SetPerformerDisplay(const Value: String); virtual; abstract;
    procedure SetStatus(const Value: string); virtual; abstract;
    function GetManufacturerIdSystem: String; virtual; abstract;
    function GetManufacturerIdValue: String; virtual; abstract;
    procedure SetManufacturerIdSystem(const Value: String); virtual; abstract;
    procedure SetManufacturerIdValue(const Value: String); virtual; abstract;
    function GetDate: TFslDateTime; virtual; abstract;
    procedure SetDate(const Value: TFslDateTime); virtual; abstract;
  public
    property status : string read GetStatus write SetStatus;
    function code(systemUri : String) : String; virtual; abstract;
    function hasCode(systemUri, code : String) : boolean; virtual; abstract;
    procedure setCodeBySystem(systemUri : String; code : String); virtual; abstract;

    property cvxCode : String read GetCvxCode write SetCvxCode;
    property sctCode : String read GetCvxCode write SetSctCode;
    property icd11Code : String read GetIcd11Code write SetIcd11Code;

    property date : TFslDateTime read GetDate write SetDate;
    property patient : String read GetPatient write SetPatient;
    property manufacturerIdSystem : String read GetManufacturerIdSystem write SetManufacturerIdSystem;
    property manufacturerIdValue : String read GetManufacturerIdValue write SetManufacturerIdValue;

    property lotNumber : String read GetLotNumber write SetLotNumber;
    property performerDisplay : String read GetPerformerDisplay write SetPerformerDisplay;


  end;

  TFHIRPrimitiveX = class (TFHIRPrimitiveW)
   public
     function GetAsString : String; override;
     procedure SetAsString(value : String); override;
     function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
   end;

implementation

{ TFHIRResourceProxyW }

procedure TFHIRResourceProxyW.loadResource;
begin
  // nothing
end;

function TFHIRResourceProxyW.wrapResource: TFHIRXVersionResourceWrapper;
begin
  // nothing

end;

constructor TFHIRResourceProxyW.Create(packageId : String; resource: TFHIRXVersionResourceWrapper; url, version: String);
begin
  inherited create(packageId, resource.Resource.link, url, version);
  FResourceW := resource.link;
end;

{ TFHIRLookupOpRespSubPropertyW }

function TFHIRLookupOpRespSubPropertyW.link: TFHIRLookupOpRespSubPropertyW;
begin
  result := TFHIRLookupOpRespSubPropertyW(inherited Link);
end;

{ TFhirValueSetExpansionContainsPropertyW }

function TFhirValueSetExpansionContainsPropertyW.link: TFhirValueSetExpansionContainsPropertyW;
begin
  result := TFhirValueSetExpansionContainsPropertyW(inherited link);
end;

{ TFHIRPrimitiveW }

function TFHIRPrimitiveW.link: TFHIRPrimitiveW;
begin
  result := TFHIRPrimitiveW(inherited link);
end;

{ TFHIRXVersionResourceWrapper }

procedure TFHIRXVersionResourceWrapper.checkNoImplicitRules(place, role: String);
begin
  FRes.checkNoImplicitRules(place, role);
end;

constructor TFHIRXVersionResourceWrapper.Create(res: TFHIRResourceV);
begin
  inherited Create;
  if (res = nil) then
    raise EFHIRException.Create('A value must be provided');
  FRes := res;
end;

destructor TFHIRXVersionResourceWrapper.Destroy;
begin
  FRes.free;
  inherited;
end;

function TFHIRXVersionResourceWrapper.link: TFHIRXVersionResourceWrapper;
begin
  result := TFHIRXVersionResourceWrapper(inherited Link);
end;

function TFHIRXVersionResourceWrapper.createPropertyValue(propName: string): TFHIRObject;
begin
  result := FRes.createPropertyValue(propName);
end;

function TFHIRXVersionResourceWrapper.fhirType: String;
begin
  result := FRes.fhirType;
end;

function TFHIRXVersionResourceWrapper.getId: String;
begin
  result := FRes.getId;
end;

function TFHIRXVersionResourceWrapper.getTypesForProperty(propName : string): String;
begin
  result := FRes.getTypesForProperty(propName);
end;

function TFHIRXVersionResourceWrapper.hasExtensions: boolean;
begin
  result := FRes.hasExtensions;
end;

function TFHIRXVersionResourceWrapper.AsJson: String;
begin
  Result := FRes.asJson;
end;

function TFHIRXVersionResourceWrapper.SourcePackage: String;
begin
  result := FRes.SourcePackage;
end;

function TFHIRXVersionResourceWrapper.hasExtension(url: String): boolean;
begin
  Result:= FRes.hasExtension(url);
end;

function TFHIRXVersionResourceWrapper.getExtensionString(url: String): String;
begin
  Result:= FRes.getExtensionString(url);
end;

function TFHIRXVersionResourceWrapper.getExtensionValue(url: String): TFHIRObject;
begin
  Result:= FRes.getExtensionValue(url);
end;

function TFHIRXVersionResourceWrapper.getExtensionsV: TFslList<TFHIRObject>;
begin
  Result:= FRes.getExtensionsV();
end;

function TFHIRXVersionResourceWrapper.extensionCount(url: String): integer;
begin
  Result:= FRes.extensionCount(url);
end;

function TFHIRXVersionResourceWrapper.getExtensionsV(url: String): TFslList<TFHIRObject>;
begin
  Result:= FRes.getExtensionsV(url);
end;

procedure TFHIRXVersionResourceWrapper.addExtensionV(url: String; value: TFHIRObject);
begin
  FRes.addExtensionV(url, value);
end;

function TFHIRXVersionResourceWrapper.getExtensionsW(url: String): TFslList<TFhirExtensionW>;
var
  list : TFslList<TFHIRObject>;
  o : TFHIRObject;
begin
  result := TFslList<TFhirExtensionW>.Create;
  try
    list := getExtensionsV(url);
    try
      for o in list do
        result.add(wrapExtension(o));
    finally
      list.free;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRXVersionResourceWrapper.getExtensionW(url: String): TFhirExtensionW;
var
  list : TFslList<TFhirExtensionW>;
begin
  list := getExtensionsW(url);
  try
    if list.count > 1 then
      raise EFHIRException.Create('Multiple matches for extension "'+url+'"')
    else if list.count = 1 then
      result := list[0].link
    else
      result := nil;
  finally
    list.free;
  end;
end;

procedure TFHIRXVersionResourceWrapper.addExtensionW(url: String; value: TFHIRXVersionElementWrapper);
begin
  AddExtensionV(url, value.Element);
end;

function TFHIRXVersionResourceWrapper.GetFhirObjectVersion: TFHIRVersion;
begin
  result := FRes.FhirObjectVersion;
end;

function TFHIRXVersionResourceWrapper.makeCodeValue(v: String): TFHIRObject;
begin
  result := FRes.makeCodeValue(v);
end;

function TFHIRXVersionResourceWrapper.makeIntValue(v: String): TFHIRObject;
begin
  result := FRes.makeIntValue(v);
end;

function TFHIRXVersionResourceWrapper.makeStringValue(v: String): TFHIRObject;
begin
  result := FRes.makeStringValue(v);
end;

procedure TFHIRXVersionResourceWrapper.setIdValue(id: String);
begin
  FRes.SetIdValue(id);
end;

function TFHIRXVersionResourceWrapper.setProperty(propName: string; propValue: TFHIRObject): TFHIRObject;
begin
  result := FRes.setProperty(propName, propValue);
end;


function TFHIRXVersionResourceWrapper.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FRes.sizeInBytes(magic));
end;

{ TFhirOperationOutcomeW }

function TFhirOperationOutcomeW.error(source: String; typeCode: TFhirIssueType; path: string; test: boolean; msg: string): boolean;
begin
  result := rule(isError, source, typeCode, path, test, msg);
end;

function TFhirOperationOutcomeW.warning(source: String; typeCode: TFhirIssueType; path: string; test: boolean; msg: string): boolean;
begin
  result := rule(isWarning, source, typeCode, path, test, msg);
end;

function TFhirOperationOutcomeW.hint(source: String; typeCode: TFhirIssueType; path: string; test: boolean; msg: string): boolean;
begin
  result := rule(isInformation, source, typeCode, path, test, msg);
end;

function TFhirOperationOutcomeW.link: TFhirOperationOutcomeW;
begin
  result := TFhirOperationOutcomeW(inherited Link);
end;

function TFhirOperationOutcomeIssueW.link: TFhirOperationOutcomeIssueW;
begin
  result := TFhirOperationOutcomeIssueW(inherited link);
end;


{ TFHIRCapabilitiesStatementW }

function TFHIRCapabilityStatementW.link: TFHIRCapabilityStatementW;
begin
  result := TFHIRCapabilityStatementW(inherited Link);
end;

{ TFHIRBundleW }

function TFHIRBundleW.next: String;
begin
  result := next(resource);
end;

function TFHIRBundleW.link: TFHIRBundleW;
begin
  result := TFHIRBundleW(inherited link);
end;

{ TFhirParametersParameterW }

destructor TFhirParametersParameterW.Destroy;
begin
  FList.free;
  inherited;
end;



function TFhirParametersParameterW.link: TFhirParametersParameterW;
begin
  result := TFhirParametersParameterW(inherited link);
end;

function TFhirParametersParameterW.partList: TFslList<TFhirParametersParameterW>;
begin
  populateList;
  result := FList;
end;

procedure TFhirParametersParameterW.populateList;
begin
  if FList = nil then
    FList := TFslList<TFhirParametersParameterW>.Create;
  FList.Clear;
end;

function TFhirParametersParameterW.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FList.sizeInBytes(magic));
end;

{ TFHIRXVersionElementWrapper }

procedure TFHIRXVersionElementWrapper.addExtensionV(url: String; value: TFHIRObject);
begin
  FElement.addExtensionV(url, value);
end;

procedure TFHIRXVersionElementWrapper.addExtensionV(extension: TFHIRObject);
begin
  FElement.addExtensionV(extension);
end;

function TFHIRXVersionElementWrapper.getExtensionsW(url: String): TFslList<TFhirExtensionW>;
var
  list : TFslList<TFHIRObject>;
  o : TFHIRObject;
begin
  result := TFslList<TFhirExtensionW>.Create;
  try
    if (hasExtensions) then
    begin
      list := getExtensionsV(url);
      try
        for o in list do
          result.add(wrapExtension(o));
      finally
        list.free;
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRXVersionElementWrapper.getAllExtensionsW: TFslList<TFhirExtensionW>;
var
  list : TFslList<TFHIRObject>;
  o : TFHIRObject;
begin
  result := TFslList<TFhirExtensionW>.Create;
  try
    list := getExtensionsV;
    try
      for o in list do
        result.add(wrapExtension(o));
    finally
      list.free;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRXVersionElementWrapper.getExtensionW(url: String): TFhirExtensionW;
var
  list : TFslList<TFhirExtensionW>;
begin
  list := getExtensionsW(url);
  try
    if list.count > 1 then
      raise EFHIRException.Create('Multiple matches for extension "'+url+'"')
    else if list.count = 1 then
      result := list[0].link
    else
      result := nil;
  finally
    list.free;
  end;
end;

procedure TFHIRXVersionElementWrapper.addExtensionW(url: String; value: TFHIRXVersionElementWrapper);
begin
  addExtensionV(url, value.Element);
end;

procedure TFHIRXVersionElementWrapper.deleteExtensionV(extension: TFHIRObject);
begin
  FElement.deleteExtensionV(extension);
end;

procedure TFHIRXVersionElementWrapper.deleteExtensionW(extension: TFHIRExtensionW);
begin
  FElement.deleteExtensionV(extension.Element);
end;

procedure TFHIRXVersionElementWrapper.deleteExtensionByUrl(url: String);
begin
  FElement.deleteExtensionByUrl(url);
end;

procedure TFHIRXVersionElementWrapper.stripExtensions(exemptUrls: TStringArray);
begin
  FElement.stripExtensions(exemptUrls);
end;

procedure TFHIRXVersionElementWrapper.copyExtensions(src: TFHIRObject; exemptUrls: TStringArray);
begin
  FElement.copyExtensions(src, exemptUrls);
end;

constructor TFHIRXVersionElementWrapper.Create(elem : TFHIRObject);
begin
  inherited Create;
  if (elem = nil) and not NoElementOk then
    raise EFHIRException.Create('A value must be provided');
  FElement := elem;
end;

destructor TFHIRXVersionElementWrapper.Destroy;
begin
  FElement.free;
  inherited;
end;

function TFHIRXVersionElementWrapper.extensionCount(url: String): integer;
begin
  result := FElement.extensionCount(url);
end;

function TFHIRXVersionElementWrapper.getExtensionsV(url: String): TFslList<TFHIRObject>;
begin
  result := FElement.getExtensionsV(url);
end;

function TFHIRXVersionElementWrapper.getExtensionsV: TFslList<TFHIRObject>;
begin
  result := FElement.getExtensionsV();
end;

function TFHIRXVersionElementWrapper.createPropertyValue(propName: string): TFHIRObject;
begin
  result := FElement.createPropertyValue(propName);
end;

function TFHIRXVersionElementWrapper.fhirType: String;
begin
  result := FElement.fhirType;
end;

function TFHIRXVersionElementWrapper.getExtensionString(url: String): String;
begin
  result := FElement.getExtensionString(url);
end;

function TFHIRXVersionElementWrapper.getExtensionValue(url: String): TFHIRObject;
begin
  Result := FElement.getExtensionValue(url);
end;

function TFHIRXVersionElementWrapper.getId: String;
begin
  result := FElement.getId;
end;

function TFHIRXVersionElementWrapper.getTypesForProperty(propName : string): String;
begin
  result := FElement.getTypesForProperty(propName);
end;

function TFHIRXVersionElementWrapper.GetFhirObjectVersion: TFHIRVersion;
begin
  result := FElement.FhirObjectVersion;
end;

function TFHIRXVersionElementWrapper.hasExtension(url: String): boolean;
begin
  result := FElement.hasExtension(url);
end;

function TFHIRXVersionElementWrapper.hasExtensions: boolean;
begin
  result := FElement.hasExtensions;
end;

function TFHIRXVersionElementWrapper.AsJson: String;
begin
  Result := FElement.asJson;
end;

function TFHIRXVersionElementWrapper.makeCodeValue(v: String): TFHIRObject;
begin
  result := FElement.makeCodeValue(v);
end;

function TFHIRXVersionElementWrapper.makeIntValue(v: String): TFHIRObject;
begin
  result := FElement.makeIntValue(v);
end;

function TFHIRXVersionElementWrapper.makeStringValue(v: String): TFHIRObject;
begin
  result := FElement.makeStringValue(v);
end;

function TFHIRXVersionElementWrapper.NoElementOk: boolean;
begin
  result := false;
end;

procedure TFHIRXVersionElementWrapper.setIdValue(id: String);
begin
  FElement.SetIdValue(id);
end;

function TFHIRXVersionElementWrapper.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  result := FElement.setProperty(propName, propValue);
end;

{ TFHIRParametersW }

destructor TFHIRParametersW.Destroy;
begin
  FList.free;
  inherited;
end;

function TFHIRParametersW.link: TFHIRParametersW;
begin
  result := TFHIRParametersW(inherited Link);

end;

function TFHIRParametersW.parameterList: TFslList<TFhirParametersParameterW>;
begin
  populateList;
  result := FList;
end;

procedure TFHIRParametersW.populateList;
begin
  if FList = nil then
    FList := TFslList<TFhirParametersParameterW>.Create;
  FList.Clear;
end;

function TFHIRParametersW.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FList.sizeInBytes(magic));
end;

function TFHIRSearchParamDefinitionW.link: TFHIRSearchParamDefinitionW;
begin
  result := TFHIRSearchParamDefinitionW(inherited link);
end;

{ TFHIRElementDefinitionW }

{ TFhirBundleEntryW }

{ TFHIRElementDefinitionW }

function TFHIRElementDefinitionW.link: TFHIRElementDefinitionW;
begin
  result := TFHIRElementDefinitionW(inherited link);
end;

{ TFhirCodingW }

function TFhirCodingW.link: TFhirCodingW;
begin
  result := TFhirCodingW(inherited Link);
end;

{ TFhirCodeableConceptW }

function TFhirCodeableConceptW.link: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConceptW(inherited Link);
end;

{ TFhirValueSetComposeIncludeW }

function TFhirValueSetComposeIncludeW.link: TFhirValueSetComposeIncludeW;
begin
  result := TFhirValueSetComposeIncludeW(inherited link);
end;

{ TFhirValueSetW }

function TFhirValueSetW.link: TFhirValueSetW;
begin
  result := TFhirValueSetW(inherited link);
end;

{ TFhirValueSetComposeIncludeFilterW }

function TFhirValueSetComposeIncludeFilterW.link: TFhirValueSetComposeIncludeFilterW;
begin
  result := TFhirValueSetComposeIncludeFilterW(inherited link);
end;

{ TFhirCodeSystemW }

destructor TFhirCodeSystemW.Destroy;
begin
  FConceptList.free;
  inherited;
end;

function TFhirCodeSystemW.link: TFhirCodeSystemW;
begin
  result := TFhirCodeSystemW(inherited link);
end;

function TFhirCodeSystemW.propertyCode(uri : String): String;
var
  p : TFhirCodeSystemPropertyW;
begin
  result := '';
  for p in properties.forEnum do
    if p.uri = uri then
      exit(p.code);
end;

function TFhirCodeSystemW.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FConceptList.sizeInBytes(magic));
end;

{ TFhirCodeSystemConceptW }

destructor TFhirCodeSystemConceptW.Destroy;
begin
  FConceptList.free;
  inherited;
end;

function TFhirCodeSystemConceptW.link: TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConceptW(inherited link);
end;

function TFhirCodeSystemConceptW.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FConceptList.sizeInBytes(magic));
end;

{ TFHIRValueSetCodeSystemW }

function TFHIRValueSetCodeSystemW.link: TFHIRValueSetCodeSystemW;
begin
  result := TFHIRValueSetCodeSystemW(inherited Link);

end;

{ TFhirValueSetComposeIncludeConceptW }

function TFhirValueSetComposeIncludeConceptW.link: TFhirValueSetComposeIncludeConceptW;
begin
  result := TFhirValueSetComposeIncludeConceptW(inherited link);
end;


{ TFHIRXVersionOperationWrapper }

constructor TFHIRXVersionOperationWrapper.Create(res: TFslObject);
begin
  inherited Create;
  if (res = nil) then
    raise EFHIRException.Create('A value must be provided');
  FOp := res;
end;

destructor TFHIRXVersionOperationWrapper.Destroy;
begin
  FList.free;
  FOp.free;
  inherited;
end;

function TFHIRXVersionOperationWrapper.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FOp.sizeInBytes(magic));
end;

function TFHIRXVersionOperationWrapper.List: TFslList<TFslObject>;
begin
  if FList = nil then
    FList := TFslList<TFslObject>.Create;
  result := FList;
end;

{ TFHIRLookupOpResponseW }

function TFHIRLookupOpResponseW.link: TFHIRLookupOpResponseW;
begin
  result := TFHIRLookupOpResponseW(inherited link);
end;

{ TFHIRLookupOpRespPropertyW }

function TFHIRLookupOpRespPropertyW.link: TFHIRLookupOpRespPropertyW;
begin
  result := TFHIRLookupOpRespPropertyW(inherited link);
end;

{ TFHIRLookupOpRespDesignationW }

function TFHIRLookupOpRespDesignationW.link: TFHIRLookupOpRespDesignationW;
begin
  result := TFHIRLookupOpRespDesignationW(inherited link);
end;

{ TFhirCodeSystemPropertyW }

function TFhirCodeSystemPropertyW.link: TFhirCodeSystemPropertyW;
begin
  result := TFhirCodeSystemPropertyW(inherited link);
end;

{ TFhirCodeSystemConceptDesignationW }

function TFhirCodeSystemConceptDesignationW.link: TFhirCodeSystemConceptDesignationW;
begin
  result := TFhirCodeSystemConceptDesignationW(inherited link);
end;

{ TFhirCodeSystemConceptPropertyW }

function TFhirCodeSystemConceptPropertyW.link: TFhirCodeSystemConceptPropertyW;
begin
  result := TFhirCodeSystemConceptPropertyW(inherited link);
end;

{ TFhirExtensionW }

function TFhirExtensionW.link: TFhirExtensionW;
begin
  result := TFhirExtensionW(inherited link);
end;


{ TFhirValueSetExpansionContainsW }

function TFhirValueSetExpansionContainsW.link: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContainsW(inherited link);
end;

{ TFhirValueSetExpansionW }

function TFhirValueSetExpansionW.link: TFhirValueSetExpansionW;
begin
  result := TFhirValueSetExpansionW(inherited Link);
end;

{ TFhirValueSetComposeIncludeConceptDesignationW }

function TFhirValueSetComposeIncludeConceptDesignationW.link: TFhirValueSetComposeIncludeConceptDesignationW;
begin
  result := TFhirValueSetComposeIncludeConceptDesignationW(inherited Link);
end;

{ TFhirPatientW }

function TFhirPatientW.Link: TFhirPatientW;
begin
  result := TFhirPatientW(inherited Link);
end;

{ TFhirBundleEntryW }


function TFhirBundleEntryW.Link: TFhirBundleEntryW;
begin
  result := TFhirBundleEntryW(inherited link);
end;

{ TFhirSearchParameterW }

function TFhirSearchParameterW.link: TFhirSearchParameterW;
begin
  result := TFhirSearchParameterW(inherited link);
end;

{ TFhirMetaW }

function TFhirMetaW.link: TFhirMetaW;
begin
  result := TFhirMetaW(inherited link);
end;

{ TFhirConceptMapW }

function TFhirConceptMapW.link: TFhirConceptMapW;
begin
  result := TFhirConceptMapW(inherited Link);
end;

{ TFhirConceptMapGroupW }

function TFhirConceptMapGroupW.link: TFhirConceptMapGroupW;
begin
  result := TFhirConceptMapGroupW(inherited Link);
end;

{ TFhirConceptMapGroupElementTargetW }

function TFhirConceptMapGroupElementTargetW.link: TFhirConceptMapGroupElementTargetW;
begin
  result := TFhirConceptMapGroupElementTargetW(inherited link);
end;

{ TFhirConceptMapGroupElementW }

function TFhirConceptMapGroupElementW.link: TFhirConceptMapGroupElementW;
begin
  result := TFhirConceptMapGroupElementW(inherited link);
end;

{ TFHIRBinaryW }

function TFHIRBinaryW.link: TFHIRBinaryW;
begin
  result := TFHIRBinaryW(inherited link);
end;

{ TFHIROperationDefinitionW }

function TFHIROperationDefinitionW.link: TFHIROperationDefinitionW;
begin
  result := TFHIROperationDefinitionW(inherited link);
end;

{ TFhirQuestionnaireW }

function TFhirQuestionnaireW.link: TFhirQuestionnaireW;
begin
  result := TFhirQuestionnaireW(inherited link);
end;

{ TFHIRNamingSystemW }

function TFHIRNamingSystemW.link: TFHIRNamingSystemW;
begin
  result := TFHIRNamingSystemW(inherited link);
end;

{ TFHIRStructureMapW }

function TFHIRStructureMapW.link: TFHIRStructureMapW;
begin
  result := TFHIRStructureMapW(inherited link);
end;

{ TFhirEventDefinitionW }

function TFhirEventDefinitionW.link: TFhirEventDefinitionW;
begin
  result := TFhirEventDefinitionW(inherited link);
end;

{ TFhirAuditEventW }

function TFhirAuditEventW.link: TFhirAuditEventW;
begin
  result := TFhirAuditEventW(inherited link);
end;

{ TFhirCapabilityStatementRestResourceW }

function TFhirCapabilityStatementRestResourceW.link: TFhirCapabilityStatementRestResourceW;
begin
  result := TFhirCapabilityStatementRestResourceW(inherited link);
end;


{ TFHIRSubscriptionW }

function TFHIRSubscriptionW.link: TFHIRSubscriptionW;
begin
  result := TFHIRSubscriptionW(inherited link);
end;

{ TFHIRSubscriptionW }

{function TFHIRSubscriptionW.GetIsTopicBased: boolean;
begin

end;

 function TFHIRSubscriptionW.link: TFHIRSubscriptionW;
begin

end;

TFHIRStatsOpResponseW }

function TFHIRStatsOpResponseW.link: TFHIRStatsOpResponseW;
begin
  result := TFHIRStatsOpResponseW(inherited link);
end;

{ TFhirQuantityW }

function TFhirQuantityW.link: TFhirQuantityW;
begin
  result := TFhirQuantityW(inherited link);
end;

{ TFhirObservationComponentW }

function TFhirObservationComponentW.link: TFhirObservationComponentW;
begin
  result := TFhirObservationComponentW(inherited link);
end;

{ TFhirObservationW }

function TFhirObservationW.link: TFhirObservationW;
begin
  result := TFhirObservationW(inherited link);
end;

{ TFhirGraphDefinitionW }

function TFhirGraphDefinitionW.link: TFhirGraphDefinitionW;
begin
  result := TFhirGraphDefinitionW(inherited link);
end;

{ TFHIRLookupOpRequestW }

function TFHIRLookupOpRequestW.link: TFHIRLookupOpRequestW;
begin
  result := TFHIRLookupOpRequestW(inherited link);
end;

{ TFHIRXVersionOperationObjectWrapper }

constructor TFHIRXVersionOperationObjectWrapper.Create(res: TFslObject);
begin
  inherited Create;
  if (res = nil) then
    raise EFHIRException.Create('An object must be provided');
  FObj := res;
end;

destructor TFHIRXVersionOperationObjectWrapper.Destroy;
begin
  FList.free;
  FObj.free;
  inherited;
end;

function TFHIRXVersionOperationObjectWrapper.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FObj.sizeInBytes(magic));
end;

function TFHIRXVersionOperationObjectWrapper.List: TFslList<TFslObject>;
begin
  if FList = nil then
    FList := TFslList<TFslObject>.Create;
  result := FList;
end;

{ TFHIRGroupW }

function TFHIRGroupW.Link: TFHIRGroupW;
begin
  result := TFHIRGroupW(inherited link);
end;

{ TFHIRGroupCharacteristicW }

function TFHIRGroupCharacteristicW.Link: TFHIRGroupCharacteristicW;
begin
  result := TFHIRGroupCharacteristicW(inherited link);
end;


{ TFHIRMetadataResourceW }

function TFHIRMetadataResourceW.GetVUrl: String;
begin
  if version = '' then
    result := url
  else
    result := url + '|'+version;
end;

function TFHIRMetadataResourceW.link: TFHIRMetadataResourceW;
begin
  result := TFHIRMetadataResourceW(inherited link);
end;

{ TFHIRNullObject }

function TFHIRNullObject.createPropertyValue(propName: string): TFHIRObject;
begin
  result := nil;
end;

function TFHIRNullObject.fhirType: String;
begin
  result := 'null';
end;

function TFHIRNullObject.getId: String;
begin
  result := '';
end;

function TFHIRNullObject.getPropertyValue(propName: string): TFHIRProperty;
begin
  result := nil;
end;

function TFHIRNullObject.getTypesForProperty(propName : string): String;
begin
  result := '';
end;

function TFHIRNullObject.hasExtensions: boolean;
begin
  result := false;
end;

function TFHIRNullObject.makeCodeValue(v: String): TFHIRObject;
begin
  result := nil;
end;

function TFHIRNullObject.makeIntValue(v: String): TFHIRObject;
begin
  result := nil;
end;


function TFHIRNullObject.makeStringValue(v: String): TFHIRObject;
begin
  result := nil;
end;


procedure TFHIRNullObject.setIdValue(id: String);
begin
end;

function TFHIRNullObject.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  result := nil;
end;

{ TFhirTerminologyCapabilitiesW }

function TFhirTerminologyCapabilitiesW.link: TFhirTerminologyCapabilitiesW;
begin
  result := TFhirTerminologyCapabilitiesW(inherited link);
end;

{ TFHIRPeriodW }


{ TFHIRPeriodW }

function TFHIRPeriodW.link: TFHIRPeriodW;
begin
  result := TFHIRPeriodW(inherited link);
end;

{ TFhirConsentProvisionW }

{ TFhirConsentW }

function TFhirConsentW.link: TFhirConsentW;
begin
  result := TFhirConsentW(inherited link);
end;

{ TFhirConsentProvisionW }

function TFhirConsentProvisionW.link: TFhirConsentProvisionW;
begin
  result := TFhirConsentProvisionW(inherited link);
end;

{ TFHIRMetadataResourceManagerW<T> }

constructor TFHIRMetadataResourceManagerW<T>.Create;
begin
  inherited;
  FMap := TFslMap<TFHIRResourceProxyV>.Create('Metadata Resource Manager ('+T.className+')');
//  FMap.defaultValue := nil;
  FList := TFslList<TFHIRResourceProxyV>.Create;
end;

destructor TFHIRMetadataResourceManagerW<T>.Destroy;
begin
  FMap.free;
  FList.free;
  inherited;
end;

procedure TFHIRMetadataResourceManagerW<T>.Assign(oSource: TFslObject);
var
  src : TFHIRMetadataResourceManagerW<T>;
begin
  inherited;
  src := oSource as TFHIRMetadataResourceManagerW<T>;
  FMap.Clear;
  FList.Clear;
  FMap.addAll(src.FMap);
  Flist.addAll(src.FList);
end;

function TFHIRMetadataResourceManagerW<T>.clone: TFHIRMetadataResourceManagerW<T>;
begin
  result := TFHIRMetadataResourceManagerW<T>(inherited clone);
end;

function TFHIRMetadataResourceManagerW<T>.link: TFHIRMetadataResourceManagerW<T>;
begin
  result := TFHIRMetadataResourceManagerW<T>(inherited link);
end;

procedure TFHIRMetadataResourceManagerW<T>.see(res : TFHIRResourceProxyV);
begin
  if (res = nil) then
    exit;
  if (res.id = '') then
    res.id := newGUIDId;
  if (FMap.containsKey(res.id)) then
    drop(res.id);

  FList.add(res.link);
  FMap.addOrSetValue(res.id, res.link); // we do this so we can drop by id

  if (res.url <> '') then
  begin
    // first, this is the correct resource for this version (if it has a version)
    if (res.version <> '') then
    begin
      FMap.addOrSetValue(res.url+'|'+res.version, res.link);
    end;
    updateList(res.url, res.version);
  end;
end;

function TFHIRMetadataResourceManagerW<T>.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FMap.sizeInBytes(magic));
  inc(result, FList.sizeInBytes(magic));
end;

procedure TFHIRMetadataResourceManagerW<T>.updateList(url, version : String);
var
  rl : TFslList<TFHIRResourceProxyV>;
  tt, latest : TFHIRResourceProxyV;
  lv : String;
begin
  rl := TFslList<TFHIRResourceProxyV>.Create;
  try
    for tt in FList do
    begin
      if (tt <> nil) and (url = tt.url) and not rl.contains(tt) then
        rl.add(tt.link);
    end;

    if (rl.count > 0) then
    begin
      // sort by version as much as we are able
      {$IFDEF FPC}
      rl.sortE(self.compare);
      {$ELSE}
      rl.sortF(function (const L, R: TFHIRResourceProxyV): Integer
        var v1, v2, mm1, mm2 : string;
        begin
          v1 := l.version;
          v2 := r.version;
          if (v1 = '') and (v2 = '') then
            result := FList.indexOf(l) - FList.indexOf(r)
          else if (v1 = '') then
            result := -1
          else if (v2 = '') then
            result := 1
          else
          begin
            mm1 := TFHIRVersions.getMajMin(v1, false);
            mm2 := TFHIRVersions.getMajMin(v2, false);
            if (mm1 = '') or (mm2 = '') then
              result := v1.compareTo(v2)
            else
              result := CompareText(mm1, mm2);
          end;
        end);
      {$ENDIF}

      // the current is the latest
      FMap.AddOrSetValue(url, rl[rl.count-1].link);
      // now, also, the latest for major/minor
      if (version <> '') then
      begin
        latest := nil;
        for tt in rl do
        begin
          if (TFHIRVersions.matches(tt.version, version, semverMinor)) then
            latest := tt;
        end;
        if (latest <> nil) then // might be null if it's not using semver
        begin
          lv := TSemanticVersion.getMajMin(latest.version, false);
          if (lv <> version) then
            FMap.addOrSetValue(url+'|'+lv, rl[rl.count-1].link);
        end;
      end;
    end;
  finally
   rl.free;
  end;
end;

function TFHIRMetadataResourceManagerW<T>.get(url : String) : T;
var
  p : TFHIRResourceProxyV;
begin
  result := T(nil);
  if (FMap.containsKey(url)) then
  begin
    p := FMap[url];
    if p <> nil then
      result := p.resourceW as T
  end
end;

function TFHIRMetadataResourceManagerW<T>.getP(url : String) : TFHIRResourceProxyV;
begin
  result := FMap[url];
end;

function TFHIRMetadataResourceManagerW<T>.get(url, version : string) : T;
var
  mm : String;
  p : TFHIRResourceProxyV;
begin
  result := T(nil);
  if (FMap.containsKey(url+'|'+version)) then
  begin
    p := FMap[url+'|'+version];
    result := p.resourceW as T
  end
  else
  begin
    mm := TFHIRVersions.getMajMin(version, false);
    if (mm <> '') and FMap.containsKey(url+'|'+mm) then
    begin
      p := FMap[url+'|'+mm];
      result := p.resourceW as T
    end;
  end;
end;

function TFHIRMetadataResourceManagerW<T>.has(url : String) : boolean;
begin
  result := FMap.containsKey(url);
end;

function TFHIRMetadataResourceManagerW<T>.has(url, version : string) : boolean;
var
  mm : String;
begin
  if (FMap.containsKey(url+'|'+version)) then
    result := true
  else
  begin
    mm := TFHIRVersions.getMajMin(version, false);
    if (mm <> '') then
      result := FMap.containsKey(url+'|'+mm)
    else
     result := false;
  end;
end;

function TFHIRMetadataResourceManagerW<T>.has(url : String; var res : T) : boolean;
var
  r : TFHIRResourceProxyV;
begin
  r := nil;
  result := FMap.TryGetValue(url, r);
  if result and (r <> nil) then
    res := r.resourceW as T
  else
    res := nil;
end;

function TFHIRMetadataResourceManagerW<T>.has(url, version : string; var res : T) : boolean;
var
  mm : String;
  r : TFHIRResourceProxyV;
begin
  res := T(nil);
  if (FMap.containsKey(url+'|'+version)) then
  begin
    r := FMap[url+'|'+version];
    if (r <> nil) then
      res := r.resourceW as T
  end
  else
  begin
    mm := TFHIRVersions.getMajMin(version, false);
    if (mm <> '') then
    begin
      result := FMap.TryGetValue(url+'|'+mm, r)  ;
      if result and (r <> nil) then
        res := r.resourceW as T;
    end
    else
     result := false;
  end;
  result := res <> nil;
end;

function TFHIRMetadataResourceManagerW<T>.count : integer;
begin
  result := FList.count;
end;

procedure TFHIRMetadataResourceManagerW<T>.drop(id : String);
var
  res : TFHIRResourceProxyV;
  mm : String;
begin
  res := FMap[id];
  if (res <> nil) then
  begin
    FList.remove(res);
    FMap.remove(id);
    FMap.remove(res.url);
    if (res.version <> '') then
    begin
      FMap.remove(res.url+'|'+res.version);
      mm := TFHIRVersions.getMajMin(res.version, false);
      if (mm <> '') then
        FMap.remove(res.url+'|'+mm);
    end;
    updateList(res.url, res.version);
  end;
end;

procedure TFHIRMetadataResourceManagerW<T>.listAll(list : TFslList<T>);
var
  tt : TFHIRResourceProxyV;
begin
  for tt in FList do
    list.add((tt.resourceW as T).link);
end;

procedure TFHIRMetadataResourceManagerW<T>.listAllM(list : TFslMetadataResourceList);
var
  tt : TFHIRResourceProxyV;
begin
  for tt in FList do
    list.add((tt.resourceW as T).link);
end;

procedure TFHIRMetadataResourceManagerW<T>.clear();
begin
  FList.clear();
  FMap.clear();
end;

{$IFDEF FPC}
function TFHIRMetadataResourceManagerW<T>.Compare(sender : TObject; const l, r : TFHIRResourceProxyV) : integer;
var
  v1, v2, mm1, mm2 : string;
begin
  v1 := l.version;
  v2 := r.version;
  if (v1 = '') and (v2 = '') then
    result := FList.indexOf(l) - FList.indexOf(r)
  else if (v1 = '') then
    result := -1
  else if (v2 = '') then
    result := 1
  else
  begin
    mm1 := TFHIRVersions.getMajMin(v1, false);
    mm2 := TFHIRVersions.getMajMin(v2, false);
    if (mm1 = '') or (mm2 = '') then
      result := v1.compareTo(v2)
    else
      result := CompareText(mm1, mm2);
  end;
end;
{$ENDIF}

{ TFhirEncounterW }

function TFhirEncounterW.Link: TFhirEncounterW;
begin
  result := TFhirEncounterW(inherited Link);
end;

{ TFhirProvenanceW }

function TFhirProvenanceW.link: TFhirProvenanceW;
begin
  result := TFhirProvenanceW(inherited link);
end;

{ TFHIRSubscriptionTopicW }

function TFHIRSubscriptionTopicW.link: TFHIRSubscriptionTopicW;
begin
  result := TFHIRSubscriptionTopicW(inherited link);
end;

{ TFslMetadataResourceList }

function TFslMetadataResourceList.link: TFslMetadataResourceList;
begin
  result := TFslMetadataResourceList(inherited link);
end;

{ TFHIRImmunizationW }

function TFHIRImmunizationW.GetCvxCode: String;
begin
  result := code(URI_CVX);
end;

function TFHIRImmunizationW.GetIcd11Code: String;
begin
  result := code('http://id.who.int/icd/release/11/mms');
end;

function TFHIRImmunizationW.GetSctCode: String;
begin
  result := code(URI_SNOMED);
end;

procedure TFHIRImmunizationW.SetCvxCode(const Value: String);
begin
  setCodeBySystem(URI_CVX, Value);
end;

procedure TFHIRImmunizationW.SetIcd11Code(const Value: String);
begin
  setCodeBySystem('http://id.who.int/icd/release/11/mms', Value);
end;

procedure TFHIRImmunizationW.SetSctCode(const Value: String);
begin
  setCodeBySystem(URI_SNOMED, Value);
end;


{ TFhirIdentifierW }

destructor TFhirIdentifierW.Destroy;
begin
  FType.free;
  inherited;
end;

function TFhirIdentifierW.GetType: TFhirCodeableConceptW;
begin
  if FType = nil then
    FType := GetTypeV;
  result := FType;
end;

function TFhirIdentifierW.link: TFhirIdentifierW;
begin
  result := TFhirIdentifierW(inherited Link);
end;

procedure TFhirIdentifierW.SetType(const Value: TFhirCodeableConceptW);
begin
  FType.free;
  FType := Value;
  SetTypeV(value);
end;

{ TFHIRResourceProxyV }

constructor TFHIRResourceProxyV.Create(packageId : String; fhirObjectVersion : TFHIRVersion; fhirType, id : String; url, version, supplements, content, valueSet : String);
begin
  inherited Create;
  FPackageId := packageId;
  FFhirObjectVersion := fhirObjectVersion;
  FFhirType := fhirType;
  FId := id;
  FUrl := url;
  FVersion := version;
  FSupplements := supplements;
  FContent := content;
  FValueSet := valueSet;
end;

constructor TFHIRResourceProxyV.Create(packageId : String; resource : TFHIRResourceV; url, version : String);
begin
  inherited Create;
  FPackageId := packageId;
  FFhirObjectVersion := resource.fhirObjectVersion;
  FFhirType := resource.fhirType;
  FId := resource.id;
  FResourceV := resource;
  FUrl := url;
  FVersion := version;
end;

destructor TFHIRResourceProxyV.Destroy;
begin
  FResourceV.free;
  FResourceW.free;
  inherited;
end;

function TFHIRResourceProxyV.link : TFHIRResourceProxyV;
begin
  result := TFHIRResourceProxyV(inherited link);
end;

function TFHIRResourceProxyV.GetResourceV : TFHIRResourceV;
begin
  if FResourceV = nil then
    loadResource;
  result := FResourceV;
end;

function TFHIRResourceProxyV.GetResourceW : TFHIRXVersionResourceWrapper;
begin
  if self = nil then
    exit(nil);

  if FResourceW = nil then
    FResourceW := wrapResource;
  result := FResourceW;
end;

procedure TFHIRResourceProxyV.SetResourceV(value: TFHIRResourceV);
begin
  FResourceV := value;
end;

{ TFHIRPrimitiveX }

function TFHIRPrimitiveX.GetAsString: String;
begin
  result := FElement.primitiveValue;
end;

procedure TFHIRPrimitiveX.SetAsString(value: String);
begin
  raise EFSLException.Create('SetAsString is not supported in a version-less context');
end;

function TFHIRPrimitiveX.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  raise EFSLException.Create('Extensions are not supported in a version-less context');
end;

{ EFHIROperationException }

constructor EFHIROperationException.create(level : TIssueSeverity; cause : TFHIRIssueType; code : TOpIssueCode; path, message : String);
begin
  inherited create(message);
  FLevel := level;
  FCause := cause;
  FCode := code;
  FPath := path;
end;

constructor EFHIROperationException.createMsg(level : TIssueSeverity; cause : TFHIRIssueType; code : TOpIssueCode; path, messageId : String; params : TStringArray);
begin
  inherited create(messageId);
  FLevel := level;
  FCause := cause;
  FCode := code;
  FPath := path;
  FMsgId := messageId;
  FParams := params;
end;

end.


