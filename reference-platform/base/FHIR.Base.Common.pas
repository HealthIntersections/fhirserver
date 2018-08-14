unit FHIR.Base.Common;

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

interface

uses
  SysUtils, Classes, Generics.Collections,
  FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Web.Parsers,
  FHIR.Base.Objects;

Type
  TFilterOperator = (foNull, foEqual, foIsA, foDescendentOf, foIsNotA, foRegex, foIn, foNotIn, foGeneralizes, foExists);
  TPublicationStatus = (psNull, psDraft, psActive, psRetired);
  TBundleType = (btNull, btDocument, btMessage, btTransaction, btTransactionResponse, btBatch, btBatchResponse, btHistory, btSearchset, btCollection);
  TTriggerType = (ttNull, ttNamedEvent, ttPeriodic, ttDataChanged, ttDataAdded, ttDataModified, ttDataRemoved, ttDataAccessed, ttDataAccessEnded);
  TContactType = (cpsNull, cpsPhone, cpsFax, cpsEmail, cpsPager, cpsUrl, cpsSms, cpsOther);
  TSubscriptionStatus = (ssNull, ssRequested, ssActive, ssError, ssOff);
  TSubscriptionMethod = (smNull, smRestHook, smEmail, smSms, smWebsocket, smChangeScript);
  TObservationStatus = (obssNull, obssRegistered, obssPreliminary, obssFinal, obssAmended, obssCorrected, obssCancelled, obssEnteredInError, obssUnknown);
  TTokenCategory = (tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcMedicationDefinition, tcOther);

const
  CODES_TFhirFilterOperator: Array[TFilterOperator] of String = ('', '=', 'is-a', 'descendent-of', 'is-not-a', 'regex', 'in', 'not-in', 'generalizes', 'exists');
  CODES_TPublicationStatus: Array[TPublicationStatus] of String = ('', 'draft', 'active', 'retired');
  CODES_TTokenCategory : array [TTokenCategory] of String = ('Clinical', 'Data', 'Meds', 'Schedule', 'Audit', 'Documents', 'Financial', 'MedicationDefinitions', 'Other');

type
  // base wrappers.....
  TFHIRXVersionElementWrapper = class (TFHIRObject)
  protected
    FElement : TFHIRObject;
    function GetFhirObjectVersion: TFHIRVersion; override;
  public
    constructor Create(elem : TFHIRObject);
    destructor Destroy; override;

    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function fhirType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;

    property Element : TFHIRObject read FElement;

    // extensions:
    function hasExtension(url : String) : boolean; override;
    function getExtensionString(url : String) : String; override;
    function extensionCount(url : String) : integer; override;
    function extensions(url : String) : TFslList<TFHIRObject>; override;
    procedure addExtension(url : String; value : TFHIRObject); override;
  end;
  TFHIRXVersionElementWrapperClass = class of TFHIRXVersionElementWrapper;

  TFhirCodingW = class (TFHIRXVersionElementWrapper)
  protected
    function GetCode: String; virtual; abstract;
    function GetDisplay: String; virtual; abstract;
    function GetSystem: String; virtual; abstract;
    function GetVersion: String; virtual; abstract;
    procedure SetCode(Value: String); virtual; abstract;
    procedure SetDisplay(Value: String); virtual; abstract;
    procedure SetSystem(Value: String); virtual; abstract;
    procedure SetVersion(Value: String); virtual; abstract;
  public
    function link : TFhirCodingW; overload;
    property system : String read GetSystem write SetSystem;
    property version : String read GetVersion write SetVersion;
    property code : String read GetCode write SetCode;
    property display : String read GetDisplay write SetDisplay;
  end;

  TFhirQuantityW = class (TFHIRXVersionElementWrapper)
  protected
    function GetCode: String; virtual; abstract;
    function GetSystem: String; virtual; abstract;
    function GetUnit: String; virtual; abstract;
    function GetValue: String; virtual; abstract;
    procedure SetCode(Value: String); virtual; abstract;
    procedure SetSystem(Value: String); virtual; abstract;
    procedure SetUnit(Value: String); virtual; abstract;
    procedure SetValue(Value: String); virtual; abstract;
  public
    function link : TFhirQuantityW; overload;
    property value : String read GetValue write SetValue;
    property units : String read GetUnit write SetUnit;
    property system : String read GetSystem write SetSystem;
    property code : String read GetCode write SetCode;
  end;

  TFhirMetaW = class (TFHIRXVersionElementWrapper)
  protected
    function GetVersionId: String; virtual; abstract;
    procedure SetVersionId(Value: String); virtual; abstract;
    function GetLastUpdated: TDateTimeEx; virtual; abstract;
    procedure SetLastUpdated(Value: TDateTimeEx); virtual; abstract;
  public
    function link : TFhirMetaW; overload;
    property versionid : String read GetVersionId write SetVersionId;
    property lastUpdated : TDateTimeEx read GetLastUpdated write SetLastUpdated;
    function tags : TFslList<TFHIRCodingW>; virtual; abstract;
    function labels : TFslList<TFHIRCodingW>; virtual; abstract;
    function profiles : TArray<String>; virtual; abstract;
    function hasTag(system, code : String) : boolean; virtual; abstract;
    function hasLabel(system, code : String) : boolean; virtual; abstract;
    procedure addTag(system, code, display : String); virtual; abstract;
    procedure addLabel(system, code, display : String); virtual; abstract;
    procedure addProfile(uri : String); virtual; abstract;
    procedure clearTags; virtual; abstract;
    procedure clearLabels; virtual; abstract;
    procedure clearProfiles; virtual; abstract;
    procedure removeTag(system, code : String); virtual; abstract;
    procedure removeLabel(system, code : String); virtual; abstract;
    procedure removeProfile(uri : String); virtual; abstract;
  end;

  TFHIRXVersionResourceWrapper = class (TFHIRObject)
  protected
    FRes : TFHIRResourceV;
    function GetFhirObjectVersion: TFHIRVersion; override;
  public
    constructor Create(res : TFHIRResourceV);
    destructor Destroy; override;

    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function fhirType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    procedure checkNoImplicitRules(place, role : String); virtual;

    property Resource : TFHIRResourceV read FRes;
  end;

  TFHIRXVersionOperationObjectWrapper = class (TFsLObject)
  protected
    FObj : TFslObject;
  public
    constructor Create(res : TFslObject);
    destructor Destroy; override;
    property Obj : TFslObject read FObj;
  end;

  TFHIRXVersionOperationWrapper = class (TFsLObject)
  protected
    FOp : TFslObject;
  public
    constructor Create(res : TFslObject);
    destructor Destroy; override;
    procedure load(params : TFHIRResourceV); overload; virtual; abstract;
    procedure load(params : TParseMap); overload; virtual; abstract;
    function asParams : TFHIRResourceV; virtual; abstract;

    property Op : TFslObject read FOp;
  end;

  // types....
  TFhirExtensionW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirExtensionW; overload;
    function url : String; virtual; abstract;
    function value : TFHIRObject; virtual; abstract;
  end;

  TFhirCodeableConceptW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodeableConceptW; overload;
    function codingCount : integer; virtual; abstract;
    function codings : TFslList<TFhirCodingW>; virtual; abstract;
    procedure addCoding(coding : TFHIRCodingW); overload; virtual; abstract;
    function addCoding : TFHIRCodingW; overload; virtual; abstract;
    function summary : String; virtual; abstract;
    function fromSystem(System : String; required : boolean = false) : String; overload; virtual; abstract;
    function fromSystem(Systems : TArray<String>; required : boolean = false) : String; overload; virtual; abstract;
  end;

  TFhirOperationOutcomeIssueW = class (TFHIRXVersionElementWrapper)
  protected
    function GetDiagnostics: String; virtual; abstract;
    procedure SetDiagnostics(Value: String); virtual; abstract;
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

    procedure addIssue(issue : TFhirOperationOutcomeIssueW; free : boolean); virtual; abstract;
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
    function GetLink(rel: String): String; virtual; abstract;
    procedure SetLink(rel: String; const Value: String); virtual; abstract;
    function getRequestMethod: String; virtual; abstract;
    function getRequestUrl: String; virtual; abstract;
    function getResource: TFHIRResourceV; virtual; abstract;
    function GetResponseDate: TDateTimeEx; virtual; abstract;
    function GetResponseStatus: String; virtual; abstract;
    function getSearchMode: TFHIRBundleEntrySearchMode; virtual; abstract;
    function getSearchMpiMatch: String; virtual; abstract;
    function getSearchScore: String; virtual; abstract;
    procedure SetRequestMethod(Value: String); virtual; abstract;
    procedure SetRequestUrl(Value: String); virtual; abstract;
    procedure SetResource(Value: TFHIRResourceV); virtual; abstract;
    procedure SetResponseDate(Value: TDateTimeEx); virtual; abstract;
    procedure SetResponseStatus(Value: String); virtual; abstract;
    procedure SetSearchMode(Value: TFHIRBundleEntrySearchMode); virtual; abstract;
    procedure SetSearchMpiMatch(Value: String); virtual; abstract;
    procedure SetSearchScore(Value: String); virtual; abstract;
    function GetUrl: String; virtual; abstract;
    procedure SetUrl(Value: String);  virtual; abstract;
    function GetrequestIfNoneExist: String; virtual; abstract;
    procedure SetrequestIfNoneExist(Value: String); virtual; abstract;
    function GetrequestIfMatch: String; virtual; abstract;
    procedure SetrequestIfMatch(Value: String); virtual; abstract;
    function GetrequestIfNoneMatch: String; virtual; abstract;
    procedure SetrequestIfNoneMatch(Value: String); virtual; abstract;
    function GetResponseETag: string; virtual; abstract;
    procedure SetResponseETag(Value: string); virtual; abstract;
    function GetResponseLocation: string; virtual; abstract;
    procedure SetResponseLocation(Value: string); virtual; abstract;
    function GetrequestIfModifiedSince: TDateTimeEx; virtual; abstract;
    procedure SetrequestIfModifiedSince(Value: TDateTimeEx); virtual; abstract;
  public
    function Link : TFhirBundleEntryW; overload;
    property links[rel : String] : String read GetLink write SetLink;
    property url : String read GetUrl write SetUrl;
    property searchMode : TFHIRBundleEntrySearchMode read getSearchMode write SetSearchMode;
    property searchScore : String read getSearchScore write SetSearchScore;
    property searchMpiMatch : String read getSearchMpiMatch write SetSearchMpiMatch;
    property resource : TFHIRResourceV read getResource write SetResource;
    property requestMethod : String read getRequestMethod write SetRequestMethod;
    property requestUrl : String read getRequestUrl write SetRequestUrl;
    property requestIfNoneExist : String read GetrequestIfNoneExist write SetrequestIfNoneExist;
    property requestIfMatch : String read GetrequestIfMatch write SetrequestIfMatch;
    property requestIfNoneMatch : String read GetrequestIfNoneMatch write SetrequestIfNoneMatch;
    property requestIfModifiedSince : TDateTimeEx read GetrequestIfModifiedSince write SetrequestIfModifiedSince;
    property responseDate : TDateTimeEx read GetResponseDate write SetResponseDate;
    property responseStatus : String read GetResponseStatus write SetResponseStatus;
    property responseETag : string read GetResponseETag write SetResponseETag;
    property responseLocation : string read GetResponseLocation write SetResponseLocation;
  end;

  TFHIRBundleW = class (TFHIRXVersionResourceWrapper)
  protected
    function GetLink(rel: String): String; virtual; abstract;
    procedure SetLink(rel: String; const Value: String); virtual; abstract;
    function GetLastUpdated : TDateTimeEx; virtual; abstract;
    procedure SetLastUpdated(Value: TDateTimeEx); virtual; abstract;
    function GetTotal: integer; virtual; abstract;
    procedure SetTotal(Value: integer); virtual; abstract;
    function GetType: TBundleType; virtual; abstract;
    procedure SetType(Value: TBundleType); virtual; abstract;
    function GetTimestamp: TDateTimeEx; virtual; abstract;
    procedure SetTimestamp(Value: TDateTimeEx); virtual; abstract;
  public
    function link : TFHIRBundleW;
    function next : String; overload;
    function next(bnd : TFHIRResourceV) : String; overload; virtual; abstract;
    procedure addEntries(bnd : TFHIRResourceV); virtual; abstract;
    procedure addEntry(bnd : TFhirBundleEntryW; first : boolean); overload; virtual; abstract;
    procedure addEntry(url : String; bnd : TFhirResourceV); overload; virtual; abstract;
    function addEntry : TFhirBundleEntryW; overload; virtual; abstract;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; virtual; abstract;
    procedure clearLinks; virtual; abstract;
    function entries : TFslList<TFhirBundleEntryW>; virtual; abstract;
    procedure listLinks(links : TFslStringDictionary); virtual; abstract;
    property links[rel : String] : String read GetLink write SetLink;
    property total : integer read GetTotal write SetTotal;
    function title : String; virtual; abstract;
    property type_ : TBundleType read GetType write SetType;
    property timestamp : TDateTimeEx read GetTimestamp write SetTimestamp;
    property lastUpdated : TDateTimeEx read GetLastUpdated write SetLastUpdated;
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
  CODES_TFHIRBundleEntrySearchMode : array [TFHIRBundleEntrySearchMode] of String = ('Unknown', 'Match', 'Include', 'Outcome');
  All_TFHIRBundleEntrySearchMode = [smUnknown..smOutcome];

type
  TFhirCapabilityStatementRestResourceW = class (TFHIRXVersionElementWrapper)
  protected
    function GetCode: String; virtual; abstract;
    procedure SetCode(Value: String); virtual; abstract;
    function GetProfile: String; virtual; abstract;
    procedure SetProfile(Value: String); virtual; abstract;
    function GetReadHistory: boolean; virtual; abstract;
    procedure SetReadHistory(Value: boolean); virtual; abstract;
  public
    function link : TFhirCapabilityStatementRestResourceW; overload;
    property code : String read GetCode write SetCode;
    property profile : String read GetProfile write SetProfile;
    property readHistory : boolean read GetReadHistory write SetReadHistory;
    procedure addInteraction(code : String); virtual; abstract;
    procedure addParam(html, n, url, d : String; t : TFHIRSearchParamType; tgts : Array of String); virtual; abstract;
  end;

  TFHIRCapabilityStatementW = class (TFHIRXVersionResourceWrapper)
  protected
    function GetUrl: String; virtual; abstract;
    procedure SetUrl(Value: String); virtual; abstract;
    function getName : String; virtual; abstract;
    procedure setName(value : String); virtual; abstract;
    function getVersion : String; virtual; abstract;
    procedure setVersion(value : String); virtual; abstract;
    function getDescription : String; virtual; abstract;
    procedure setDescription(value : String); virtual; abstract;
    function GetStatus: TPublicationStatus; virtual; abstract;
    procedure SetStatus(Value: TPublicationStatus); virtual; abstract;
    function GetDate: TDateTimeEx; virtual; abstract;
    procedure SetDate(Value: TDateTimeEx); virtual; abstract;
    function GetFhirVersion: string; virtual; abstract;
    procedure SetFhirVersion(Value: string); virtual; abstract;
  public
    function link : TFHIRCapabilityStatementW; overload;

    property url : String read GetUrl write SetUrl;
    property name : String read GetName write SetName;
    property version : String read GetVersion write SetVersion;
    property status : TPublicationStatus read GetStatus write SetStatus;
    property description : String read GetDescription write SetDescription;
    property date : TDateTimeEx read GetDate write SetDate;
    property fhirVersion : string read GetFhirVersion write SetFhirVersion;

    function hasRest : boolean; virtual; abstract;
    function hasSecurity(system, code : String) : boolean; virtual; abstract;
    procedure readSmartExtension(var authorize, token, register: String); virtual; abstract;
    procedure addSmartExtensions(authorize, token, register: String); virtual; abstract;
    function hasFormat(fmt : String) : boolean; virtual; abstract;

    procedure contact(kind : TContactType; value : String); virtual; abstract;
    procedure software(name, version, release : String); virtual; abstract;
    procedure impl(url, desc : String); virtual; abstract;
    procedure fmt(mt : String); virtual; abstract;
    procedure standardServer(ts, ws, pv, cv, iv : String); virtual; abstract;
    function addResource(code : String) : TFhirCapabilityStatementRestResourceW; virtual; abstract;
    procedure addOperation(name, url : String); virtual; abstract;

    function supportsType(name : String; interaction : TFHIRInteraction) : boolean; virtual; abstract;
    procedure listTypes(interactions : TFHIRInteractions; names : TStrings); virtual; abstract;
    procedure listSearchParams(name : String; list : TFslList<TFHIRSearchParamDefinitionW>); virtual; abstract;
  end;

  TFhirParametersParameterW = class (TFHIRXVersionElementWrapper)
  protected
    FList : TFslList<TFhirParametersParameterW>;
    function GetValue: TFHIRObject; virtual; abstract;
    procedure SetValue(Value: TFHIRObject); virtual; abstract;
    function GetResource: TFHIRResourceV; virtual; abstract;
    procedure SetResource(Value: TFHIRResourceV); virtual; abstract;
    procedure populateList; virtual;
    function GetParameterParameter(name: String): TFhirParametersParameterW;  virtual; abstract;
    function GetResourceParameter(name: String): TFHIRResourceV;  virtual; abstract;
    function GetStringParameter(name: String): String;  virtual; abstract;
  public
    destructor Destroy; override;
    function link : TFhirParametersParameterW; overload;

    function name : String; virtual; abstract;
    function hasValue : boolean;  virtual; abstract;
    property value : TFHIRObject read GetValue write SetValue;
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
  end;

  TFHIRParametersW = class (TFHIRXVersionResourceWrapper)
  protected
    FList : TFslList<TFhirParametersParameterW>;
    function GetParameter(name: String): TFhirParametersParameterW;  virtual; abstract;
    procedure populateList; virtual;
  public
    destructor Destroy; override;
    function link : TFHIRParametersW; overload;

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
    function use : TFHIRObject; virtual; abstract;
    function value : String; virtual; abstract;
  end;

  TFhirCodeSystemConceptW = class (TFHIRXVersionElementWrapper)
  protected
    FConceptList : TFslList<TFhirCodeSystemConceptW>;
  public
    destructor Destroy; override;
  public
    function link : TFhirCodeSystemConceptW; overload;
    function code : String; virtual; abstract;
    function display : String; virtual; abstract;
    function definition : String; virtual; abstract;
    function conceptList : TFslList<TFhirCodeSystemConceptW>; virtual; abstract;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; virtual; abstract;
    function conceptCount : integer; virtual; abstract;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function designationCount : integer; virtual; abstract;
    function designations : TFslList<TFhirCodeSystemConceptDesignationW>; virtual; abstract;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; virtual; abstract;
    function displayTag(tag : String) : String; virtual; abstract;
    procedure setDisplayTag(tag, value : String); virtual; abstract;
    function getCode(code : String) : TFhirCodeSystemConceptW; virtual; abstract;
  end;

  TFhirCodeSystemPropertyType = (cptNull, cptCode, cptCoding, cptString, cptInteger, cptBoolean, cptDateTime, cptDecimal);
  TFhirCodeSystemPropertyW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodeSystemPropertyW; overload;
    function code : String; virtual; abstract;
    function type_ : TFhirCodeSystemPropertyType; virtual; abstract;
  end;

  TFhirCodeSystemContentMode = (cscmNull, cscmNotPresent, cscmExample, cscmFragment, cscmComplete, cscmSupplement);

const
  CODES_TFhirCodeSystemContentMode : Array[TFhirCodesystemContentMode] of String = ('null', 'not-present', 'example', 'fragment', 'complete', 'supplement');
  ALL_TFhirCodeSystemContentMode = [cscmNull..cscmSupplement];

type
  TFHIRValueSetW = class;

  TFHIRMetadataResourceW = class (TFHIRXVersionResourceWrapper)
  protected
    function GetURL: String; virtual; abstract;
    function GetName: String; virtual; abstract;
    function GetStatus: TPublicationStatus; virtual; abstract;
    function GetVersion: String; virtual; abstract;
    function GetDescription: String; virtual; abstract;
    function GetDate: TDateTimeEx; virtual; abstract;
    function GetPublisher: String; virtual; abstract;
    procedure SetPublisher(Value: String); virtual; abstract;
    procedure SetDate(Value: TDateTimeEx); virtual; abstract;
    procedure SetUrl(Value: String); virtual; abstract;
    procedure SetVersion(Value: String); virtual; abstract;
    procedure SetName(Value: String); virtual; abstract;
    procedure SetStatus(Value: TPublicationStatus); virtual; abstract;
    procedure SetDescription(Value: String); virtual; abstract;
    function getContext: String; virtual; abstract;
  public
    function link : TFHIRMetadataResourceW; overload;

    property url : String read GetURL write SetUrl;
    property name : String read GetName write SetName;
    property version : String read GetVersion write SetVersion;
    property status : TPublicationStatus read GetStatus write SetStatus;
    property description : String read GetDescription write SetDescription;
    property date : TDateTimeEx read GetDate write SetDate;
    property context : String read getContext;
    property publisher : String read GetPublisher write SetPublisher;
  end;

  TFhirCodeSystemW = class (TFHIRMetadataResourceW)
  protected
    FConceptList : TFslList<TFhirCodeSystemConceptW>;
    function getContent: TFhirCodeSystemContentMode; virtual; abstract;
    procedure SetContent(Value: TFhirCodeSystemContentMode); virtual; abstract;
    function GetCount: integer; virtual; abstract;
    procedure SetCount(Value: integer); virtual; abstract;
  public
    destructor Destroy; override;
    function link : TFhirCodeSystemW; overload;
    property content : TFhirCodeSystemContentMode read getContent write SetContent;
    property count : integer read GetCount write SetCount;

    function valueSet : String; virtual; abstract;
    function supplements : String; virtual; abstract;
    function copyright : String; virtual; abstract;
    function language : String; virtual; abstract;

    function properties : TFslList<TFhirCodeSystemPropertyW>;  virtual; abstract;
    // this is special because it's owned
    function conceptList : TFslList<TFhirCodeSystemConceptW>; virtual; abstract;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; virtual; abstract;
    function conceptCount : integer; virtual; abstract;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function getCode(code : String) : TFhirCodeSystemConceptW; virtual; abstract;

    function isAbstract(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function getParents(c : TFhirCodeSystemConceptW) : TFslList<TFhirCodeSystemConceptW>; virtual; abstract;
    function getChildren(c : TFhirCodeSystemConceptW) : TFslList<TFhirCodeSystemConceptW>; virtual; abstract;

    function buildImplicitValueSet : TFHIRValueSetW; virtual; abstract;
  end;

  TFhirValueSetExpansionContainsW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetExpansionContainsW; overload;
    function getSystem : String; virtual; abstract;
    function getCode : String; virtual; abstract;
    function getDisplay : String; virtual; abstract;
    procedure SetCode(Value: String); virtual; abstract;
    procedure SetDisplay(Value: String); virtual; abstract;
    procedure SetSystem(Value: String); virtual; abstract;

    property system : String read GetSystem write SetSystem;
    property code : String read GetCode write SetCode;
    property display : String read GetDisplay write SetDisplay;

    function contains : TFslList<TFhirValueSetExpansionContainsW>; virtual; abstract;
  end;

  TFhirValueSetExpansionW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetExpansionW; overload;
    procedure addParam(name, value : String); virtual; abstract;
    function hasParam(name : string) : boolean; overload; virtual; abstract;
    function hasParam(name, value : string) : boolean; overload; virtual; abstract;
    procedure copyParams(source : TFhirValueSetExpansionW); virtual; abstract;
    procedure addContains(item : TFhirValueSetExpansionContainsW); overload; virtual; abstract;
    function makeContains : TFhirValueSetExpansionContainsW; overload; virtual; abstract;
    function addContains : TFhirValueSetExpansionContainsW; overload; virtual; abstract;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; virtual; abstract;
  end;

  TFhirValueSetComposeIncludeFilterW = class (TFHIRXVersionElementWrapper)
  protected
    function getProp : String; virtual; abstract;
    function getOp : TFilterOperator; virtual; abstract;
    function getValue : String; virtual; abstract;
    procedure SetOp(Value: TFilterOperator); virtual; abstract;
    procedure SetProp(Value: String); virtual; abstract;
    procedure SetValue(Value: String); virtual; abstract;
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
    function value : String; virtual; abstract;
  end;

  TFhirValueSetComposeIncludeConceptW = class (TFHIRXVersionElementWrapper)
  protected
    function getCode : String; virtual; abstract;
    function getDisplay : String; virtual; abstract;
    procedure SetCode(Value: String); virtual; abstract;
    procedure SetDisplay(Value: String); virtual; abstract;
  public
    function link : TFhirValueSetComposeIncludeConceptW; overload;
    property code : String read getCode write SetCode;
    property display : String read GetDisplay write SetDisplay;
    function designations : TFslList<TFhirValueSetComposeIncludeConceptDesignationW>; virtual; abstract;
  end;

  TFhirValueSetComposeIncludeW = class (TFHIRXVersionElementWrapper)
  protected
    function getSystem : String; virtual; abstract;
    function getVersion : String; virtual; abstract;
    procedure SetSystem(Value: String); virtual; abstract;
    procedure SetVersion(Value: String); virtual; abstract;
  public
    function link : TFhirValueSetComposeIncludeW; overload;

    property system : String read GetSystem write SetSystem;
    property version : String read GetVersion write SetVersion;
    function valueSets : TArray<String>; virtual; abstract;
    function hasConcepts : boolean; virtual; abstract;
    function concepts : TFslList<TFhirValueSetComposeIncludeConceptW>; virtual; abstract;
    function addConcept : TFhirValueSetComposeIncludeConceptW; virtual; abstract;
    function hasFilters : boolean; virtual; abstract;
    function filters : TFslList<TFhirValueSetComposeIncludeFilterW>; virtual; abstract;
    function addFilter : TFhirValueSetComposeIncludeFilterW; virtual; abstract;
  end;

  TFHIRValueSetCodeSystemW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFHIRValueSetCodeSystemW; overload;
    function system : String; virtual; abstract;
    function concepts : TFslList<TFHIRCodeSystemConceptW>; virtual; abstract;
  end;

  TFhirValueSetW =  class (TFHIRMetadataResourceW)
  public
    function link : TFhirValueSetW; overload;
    function source : String; virtual; abstract;

    function checkCompose(place, role : String) : boolean; virtual; abstract;
    function imports : TArray<String>; virtual; abstract; // only in R2
    function hasInlineCS : boolean; virtual; abstract;
    function inlineCS : TFHIRValueSetCodeSystemW; virtual; abstract;
    function includes : TFslList<TFhirValueSetComposeIncludeW>; virtual; abstract;
    function addInclude : TFhirValueSetComposeIncludeW; virtual; abstract; {      result.compose := TFhirValueSetCompose.Create;     inc := result.addInclude; compose.includeList.Append; }
    function excludes : TFslList<TFhirValueSetComposeIncludeW>; virtual; abstract;

    procedure clearDefinition; virtual; abstract;
    function hasExpansion : boolean; virtual; abstract;
    function expansion : TFhirValueSetExpansionW; virtual; abstract;
    function forceExpansion : TFhirValueSetExpansionW; virtual; abstract;
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

  TFhirStructureDefinitionW =  class (TFHIRXVersionResourceWrapper)
  public
    function kind : TStructureDefinitionKind; virtual; abstract;
    function name : String; virtual; abstract;
    function url : String; virtual; abstract;
    function type_ : String; virtual; abstract;
    function elements : TFslList<TFHIRElementDefinitionW>; virtual; abstract;
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
  public
    function Link : TFhirPatientW; overload;
    function nameSummary : String; virtual; abstract;
  end;

  TFHIRLookupOpRespPropertyW = class (TFHIRXVersionOperationObjectWrapper)
  public
    function link : TFHIRLookupOpRespPropertyW; overload;
    function GetDescription: string; virtual; abstract;
    procedure SetDescription(Value: string); virtual; abstract;
    function GetValue: TFHIRObject; virtual; abstract;
    procedure SetValue(Value: TFHIRObject); virtual; abstract;

    property description : string read GetDescription write SetDescription;
    property value : TFHIRObject read GetValue write SetValue;
  end;

  TFHIRLookupOpRespDesignationW = class (TFHIRXVersionOperationObjectWrapper)
  public
    function link : TFHIRLookupOpRespDesignationW; overload;
    function GetUse: TFHIRObject; virtual; abstract;
    procedure SetUse(Value: TFHIRObject); virtual; abstract;

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

  TFHIRLookupOpResponseW = class (TFHIRXVersionOperationWrapper)
  protected
    function GetName: String; virtual; abstract;
    procedure SetName(Value: String); virtual; abstract;
    function GetDisplay: String; virtual; abstract;
    procedure SetDisplay(Value: String); virtual; abstract;
  public
    function link : TFHIRLookupOpResponseW; overload;
    function addProp(name : string) : TFHIRLookupOpRespPropertyW; virtual; abstract;
    function addDesignation(system, code, display, value : string) : TFHIRLookupOpRespDesignationW; overload; virtual; abstract;
    function addDesignation(lang, value : string) : TFHIRLookupOpRespDesignationW; overload; virtual; abstract;
    function GetVersion: String; virtual; abstract;
    procedure SetVersion(Value: String); virtual; abstract;
    procedure addExtension(name, value : String); overload; virtual; abstract;
    procedure addExtension(name : String; value : boolean); overload; virtual; abstract;

    property version : String read GetVersion write SetVersion;
    property name : String read GetName write SetName;
    property display : String read GetDisplay write SetDisplay;
  end;

  TFHIRSubsumesOpRequestW = class (TFHIRXVersionOperationWrapper)
  public
    function system : String; virtual; abstract;
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
    function GetOutcome: String; virtual; abstract;
    procedure SetOutcome(Value: String); virtual; abstract;
  public
    property outcome : String read GetOutcome write SetOutcome;
  end;

  TFhirTestScriptW = class (TFHIRXVersionResourceWrapper);
  TFhirProvenanceW = class (TFHIRXVersionResourceWrapper);

  TFHIRConceptEquivalence = (cmeNull, cmeRelatedto, cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact, cmeUnmatched, cmeDisjoint);

const
  CODES_TFHIRConceptEquivalence : Array [TFHIRConceptEquivalence] of String = ('Null', 'Relatedto', 'Equivalent', 'Equal', 'Wider', 'Subsumes', 'Narrower', 'Specializes', 'Inexact', 'Unmatched', 'Disjoint');
  ALL_TFHIRConceptEquivalence = [cmeNull..cmeDisjoint];

type
  TFhirConceptMapGroupElementTargetW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirConceptMapGroupElementTargetW; overload;
    function code: String; virtual; abstract;
    function equivalence : TFHIRConceptEquivalence; virtual; abstract;
    function comments : String; virtual; abstract;
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

  TFhirConceptMapW = class (TFHIRXVersionResourceWrapper)
  protected
    function GetDate: TDateTimeEx; virtual; abstract;
    function GetDescription: String; virtual; abstract;
    function GetName: String; virtual; abstract;
    function GetStatus: TPublicationStatus; virtual; abstract;
    function GetURL: String; virtual; abstract;
    procedure SetDate(Value: TDateTimeEx); virtual; abstract;
    procedure SetDescription(Value: String); virtual; abstract;
    procedure SetName(Value: String); virtual; abstract;
    procedure SetStatus(Value: TPublicationStatus); virtual; abstract;
    procedure SetUrl(Value: String); virtual; abstract;
    function getContext: String; virtual; abstract;
    function GetPublisher: String; virtual; abstract;
    procedure SetPublisher(Value: String); virtual; abstract;
  protected
    function GetVersion: String; virtual; abstract;
    procedure SetVersion(Value: String); virtual; abstract;
  public
    function link : TFhirConceptMapW; overload;

    property url : String read GetURL write SetUrl;
    property name : String read GetName write SetName;
    property version : String read GetVersion write SetVersion;
    property status : TPublicationStatus read GetStatus write SetStatus;
    property description : String read GetDescription write SetDescription;
    property date : TDateTimeEx read GetDate write SetDate;
    property context : String read getContext;
    property publisher : String read GetPublisher write SetPublisher;
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

  TFHIRNamingSystemW = class (TFHIRXVersionResourceWrapper)
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
    procedure eventType(system, code, display : String); virtual; abstract;
    procedure eventSubType(system, code, display : String); virtual; abstract;
    procedure source(name, system, value : String); virtual; abstract;
    procedure sourceType(system, code, display : String); virtual; abstract;
    procedure participantIp(ip : String); virtual; abstract;
    procedure participantId(system, value, alt, name : String); virtual; abstract;

    function dateTime : TDateTimeEx; virtual; abstract;
  end;

  TFHIRSubscriptionW = class (TFHIRXVersionResourceWrapper)
  protected
    function GetCriteria: String; virtual; abstract;
    function GetDirect: boolean; virtual; abstract;
    function GetEndpoint: String; virtual; abstract;
    function GetError: String; virtual; abstract;
    function GetMethod: TSubscriptionMethod; virtual; abstract;
    function GetPayload: String; virtual; abstract;
    function GetStatus: TSubscriptionStatus; virtual; abstract;
    function GetSummary: String; virtual; abstract;
    function GetHeaders: TArray<String>; virtual; abstract;
    procedure SetCriteria(Value: String); virtual; abstract;
    procedure SetDirect(Value: boolean); virtual; abstract;
    procedure SetEndpoint(Value: String); virtual; abstract;
    procedure SetError(Value: String); virtual; abstract;
    procedure Setheaders(Value: TArray<String>); virtual; abstract;
    procedure SetMethod(Value: TSubscriptionMethod); virtual; abstract;
    procedure SetPayload(Value: String); virtual; abstract;
    procedure SetStatus(Value: TSubscriptionStatus); virtual; abstract;
  public
    function link : TFHIRSubscriptionW; overload;
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

  TFHIRStatsOpResponseW = class (TFHIRXVersionOperationWrapper)
  public
    function link : TFHIRStatsOpResponseW; overload;
    procedure addObs(obs : TFHIRResourceV); virtual; abstract;
  end;

  TFhirObservationComponentW = class (TFHIRXVersionElementWrapper)
  protected
    function GetValue: TFHIRObject;  virtual; abstract;
    procedure SetValue(Value: TFHIRObject); virtual; abstract;
  public
    function link : TFhirObservationComponentW; overload;
    function codings : TFslList<TFHIRCodingW>; virtual; abstract;
    property value : TFHIRObject read GetValue write SetValue;
    function valueW : TFHIRXVersionElementWrapper; virtual; abstract;
    function dataAbsentReason : TFhirCodeableConceptW; virtual; abstract;
  end;

  TFhirObservationW = class (TFHIRXVersionResourceWrapper)
  protected
    function GetStatus: TObservationStatus;  virtual; abstract;
    procedure SetStatus(Value: TObservationStatus);  virtual; abstract;
    function GetValue: TFHIRObject;  virtual; abstract;
    procedure SetValue(Value: TFHIRObject); virtual; abstract;
  public
    function link : TFhirObservationW; overload;
    property status : TObservationStatus read GetStatus write SetStatus;
    procedure addCode(c : TFHIRCodingW); virtual; abstract;
    procedure setSubj(url : String); virtual; abstract;
    procedure setPeriod(start, finish : TDateTime); virtual; abstract;

    function subject : String; virtual; abstract;
    function categories : TFslList<TFHIRCodingW>; virtual; abstract;
    function codings : TFslList<TFHIRCodingW>; virtual; abstract;
    function hasTime : boolean;  virtual; abstract;
    procedure getDates(var dt, dtMin, dtMax : TDateTime); virtual; abstract;
    property value : TFHIRObject read GetValue write SetValue;
    function valueW : TFHIRXVersionElementWrapper; virtual; abstract;
    function dataAbsentReason : TFhirCodeableConceptW; virtual; abstract;

    function components : TFslList<TFhirObservationComponentW>; virtual; abstract;
    function addComp(system, code : String) : TFhirObservationComponentW; virtual; abstract;
  end;

  TFhirGraphDefinitionW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirGraphDefinitionW; overload;
  end;

implementation

uses
  FHIR.Base.Lang;

{ TFHIRXVersionResourceWrapper }

procedure TFHIRXVersionResourceWrapper.checkNoImplicitRules(place, role: String);
begin
  FRes.checkNoImplicitRules(place, role);
end;

constructor TFHIRXVersionResourceWrapper.create(res: TFHIRResourceV);
begin
  inherited create;
  if (res = nil) then
    raise EFHIRException.create('A value must be provided');
  FRes := res;
end;

destructor TFHIRXVersionResourceWrapper.Destroy;
begin
  FRes.Free;
  inherited;
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

procedure TFHIRXVersionResourceWrapper.setProperty(propName: string; propValue: TFHIRObject);
begin
  FRes.setProperty(propName, propValue);
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
  FList.Free;
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
    FList := TFslList<TFhirParametersParameterW>.create;
  FList.Clear;
end;

{ TFHIRXVersionElementWrapper }

procedure TFHIRXVersionElementWrapper.addExtension(url: String; value: TFHIRObject);
begin
  FElement.addExtension(url, value);
end;

constructor TFHIRXVersionElementWrapper.Create(elem : TFHIRObject);
begin
  inherited create;
  if (elem = nil) then
    raise EFHIRException.create('A value must be provided');
  FElement := elem;
end;

destructor TFHIRXVersionElementWrapper.Destroy;
begin
  FElement.Free;
  inherited;
end;

function TFHIRXVersionElementWrapper.extensionCount(url: String): integer;
begin
  result := FElement.extensionCount(url);
end;

function TFHIRXVersionElementWrapper.extensions(url: String): TFslList<TFHIRObject>;
begin
  result := FElement.extensions(url);
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

function TFHIRXVersionElementWrapper.getId: String;
begin
  result := FElement.getId;
end;

function TFHIRXVersionElementWrapper.GetFhirObjectVersion: TFHIRVersion;
begin
  result := FElement.FhirObjectVersion;
end;

function TFHIRXVersionElementWrapper.hasExtension(url: String): boolean;
begin
  result := FElement.hasExtension(url);
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

procedure TFHIRXVersionElementWrapper.setIdValue(id: String);
begin
  FElement.SetIdValue(id);
end;

procedure TFHIRXVersionElementWrapper.setProperty(propName: string; propValue: TFHIRObject);
begin
  FElement.setProperty(propName, propValue);
end;

{ TFHIRParametersW }

destructor TFHIRParametersW.Destroy;
begin
  FList.Free;
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
    FList := TFslList<TFhirParametersParameterW>.create;
  FList.Clear;
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
  FConceptList.Free;
  inherited;
end;

function TFhirCodeSystemW.link: TFhirCodeSystemW;
begin
  result := TFhirCodeSystemW(inherited link);
end;

{ TFhirCodeSystemConceptW }

destructor TFhirCodeSystemConceptW.Destroy;
begin
  FConceptList.Free;
  inherited;
end;

function TFhirCodeSystemConceptW.link: TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConceptW(inherited link);
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
  inherited create;
  if (res = nil) then
    raise EFHIRException.create('A value must be provided');
  FOp := res;
end;

destructor TFHIRXVersionOperationWrapper.Destroy;
begin
  FOp.Free;
  inherited;
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

{ TFHIRStatsOpResponseW }

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
  inherited create;
  if (res = nil) then
    raise EFHIRException.create('An object must be provided');
  FObj := res;
end;

destructor TFHIRXVersionOperationObjectWrapper.Destroy;
begin
  FObj.Free;
  inherited;
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

function TFHIRMetadataResourceW.link: TFHIRMetadataResourceW;
begin
  result := TFHIRMetadataResourceW(inherited link);
end;

end.




