unit fhir3_common;

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
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities,
  fsl_http,
  fhir_objects, fhir_common, 
  fhir3_types, fhir3_resources, fhir3_operations, fhir3_opbase;

const
  ExceptionTypeTranslations : array [TFhirIssueType] of TFhirIssueTypeEnum = (IssueTypeNull, IssueTypeInvalid, IssueTypeStructure, IssueTypeRequired, IssueTypeValue,
    IssueTypeInvariant, IssueTypeSecurity, IssueTypeLogin, IssueTypeUnknown, IssueTypeExpired, IssueTypeForbidden, IssueTypeSuppressed, IssueTypeProcessing,
    IssueTypeNotSupported, IssueTypeDuplicate, IssueTypeNotFound, IssueTypeTooLong, IssueTypeCodeInvalid, IssueTypeExtension, IssueTypeTooCostly, IssueTypeBusinessRule,
    IssueTypeConflict, IssueTypeIncomplete, IssueTypeTransient, IssueTypeLockError, IssueTypeNoStore, IssueTypeException, IssueTypeTimeout, IssueTypeThrottled, IssueTypeInformational);

  ISSUE_SEVERITY_MAP : array [TFhirIssueSeverityEnum] of TIssueSeverity = (isNull, isFatal, isError, isWarning, isInformation);
  ISSUE_SEVERITY_MAP2 : array [TIssueSeverity] of TFhirIssueSeverityEnum = (IssueSeverityNull, IssueSeverityFatal, IssueSeverityError, IssueSeverityWarning, IssueSeverityInformation);
  INTERACTION_MAP : array [TFHIRInteraction] of TFhirTypeRestfulInteractionEnum = (TypeRestfulInteractionRead, TypeRestfulInteractionSearchType, TypeRestfulInteractionHistoryType, TypeRestfulInteractionCreate, TypeRestfulInteractionUpdate, TypeRestfulInteractionDelete, TypeRestfulInteractionPatch);
  INTERACTION_MAP2 : array [TFHIRInteraction] of TFhirSystemRestfulInteractionEnum = (SystemRestfulInteractionNull, SystemRestfulInteractionSearchSystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem);
  MAP_SearchParamType : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptString, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri);
  MAP_SEARCH_MODE : array [TFhirSearchEntryModeEnum] of TFHIRBundleEntrySearchMode = (smUnknown, smMatch, smInclude, smOutcome);
  MAP_SEARCH_MODE2 : array [TFHIRBundleEntrySearchMode] of TFhirSearchEntryModeEnum = (SearchEntryModeNull, SearchEntryModeMatch, searchEntryModeInclude, searchEntryModeOutcome);
  MAP_ELEMENT_DEFINITION_BINDING : array [TFhirBindingStrengthEnum] of TElementDefinitionBinding = (edbNone, edbRequired, edbExtensible, edbPreferred, edpExample);
  MAP_TFilterOperator : array [TFhirFilterOperatorEnum] of TFilterOperator = (foNull, foEqual, foIsA, foDescendentOf, foIsNotA, foRegex, foIn, foNotIn, foGeneralizes, foExists);
  MAP_TFilterOperatorR : array [TFilterOperator] of TFhirFilterOperatorEnum = (filterOperatorNull, filterOperatorEqual, filterOperatorIsA, filterOperatorNull, filterOperatorIsNotA, filterOperatorRegex, filterOperatorIn, filterOperatorNotIn, filterOperatorNull, filterOperatorNull);
  MAP_TFhirConceptPropertyTypeEnum : array [TFhirConceptPropertyTypeEnum] of TFhirCodeSystemPropertyType = (cptNull, cptCode, cptCoding, cptString, cptInteger, cptBoolean, cptDateTime);
  MAP_TFHIRSearchParamType1 : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptNull, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri);
  MAP_TFHIRSearchParamType2 : array [TFhirSearchParamType] of TFHIRSearchParamTypeEnum = (SearchParamTypeNull, SearchParamTypeNumber, SearchParamTypeDate, SearchParamTypeString, SearchParamTypeToken, SearchParamTypeReference, SearchParamTypeComposite, SearchParamTypeQuantity, SearchParamTypeUri, SearchParamTypeNull);
  MAP_TPublicationStatus : array [TPublicationStatus] of TFHIRPublicationStatusEnum = (PublicationStatusNull, PublicationStatusDraft, PublicationStatusActive, PublicationStatusRetired);
  MAP_TPublicationStatusR : array [TFHIRPublicationStatusEnum] of TPublicationStatus = (psNull, psDraft, psActive, psRetired, psNull);
  MAP_TFhirCodeSystemContentMode : array [TFhirCodeSystemContentMode] of TFhirCodeSystemContentModeEnum = (CodesystemContentModeNull, CodesystemContentModeNotPresent, CodesystemContentModeExample, CodesystemContentModeFragment, CodesystemContentModeComplete, CodesystemContentModeNull);
  MAP_TFhirCodeSystemContentModeR : array [TFhirCodeSystemContentModeEnum] of TFhirCodeSystemContentMode = (cscmNull, cscmNotPresent, cscmExample, cscmFragment, cscmComplete);
  MAP_TFHIRConceptEquivalence : array [TFhirConceptMapEquivalenceEnum] of TFHIRConceptEquivalence = (cmeNull, cmeRelatedto, cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact, cmeUnmatched, cmeDisjoint);
  MAP_TFHIRConceptEquivalenceR : array [TFHIRConceptEquivalence] of TFHIRConceptMapEquivalenceEnum = (ConceptMapEquivalenceNull, ConceptMapEquivalenceRelatedto, ConceptMapEquivalenceEquivalent, ConceptMapEquivalenceEqual, ConceptMapEquivalenceWider, ConceptMapEquivalenceSubsumes, ConceptMapEquivalenceNarrower, ConceptMapEquivalenceSpecializes, ConceptMapEquivalenceInexact, ConceptMapEquivalenceUnmatched, ConceptMapEquivalenceDisjoint);
  BUNDLE_TYPE_TITLE : Array[TFhirBundleTypeEnum] of String = ('', 'Document', 'Message', 'Transaction', 'Transaction Response', 'Batch', 'Batch Response', 'History Record', 'Search Results', 'Resource Collection');

  MAP_TFHIRBundleType  : array [TBundleType] of TFhirBundleTypeEnum = (BundleTypeNull, BundleTypeDocument, BundleTypeMessage, BundleTypeTransaction, BundleTypeTransactionResponse, BundleTypeBatch, BundleTypeBatchResponse, BundleTypeHistory, BundleTypeSearchset, BundleTypeCollection);
  MAP_TFHIRBundleTypeR : array [TFhirBundleTypeEnum] of TBundleType = (btNull, btDocument, btMessage, btTransaction, btTransactionResponse, btBatch, btBatchResponse, btHistory, btSearchset, btCollection);
  MAP_TContactType : array [TContactType] of TFhirContactPointSystemEnum = (ContactPointSystemNull, ContactPointSystemPhone, ContactPointSystemFax, ContactPointSystemEmail, ContactPointSystemPager, ContactPointSystemUrl, ContactPointSystemSms, ContactPointSystemOther);
  MAP_TContactType2 : array [TFhirContactPointSystemEnum] of TContactType = (cpsNull, cpsPhone, cpsFax, cpsEmail, cpsPager, cpsUrl, cpsSms, cpsOther);

  MAP_TSubscriptionMethod : array [TFhirSubscriptionChannelTypeEnum] of TSubscriptionMethod = (smNull, smRestHook, smEmail, smSms, smWebsocket, smChangeScript);
  MAP_TSubscriptionMethod2 : array [TSubscriptionMethod] of TFhirSubscriptionChannelTypeEnum = (SubscriptionChannelTypeNull, SubscriptionChannelTypeRestHook, SubscriptionChannelTypeWebsocket, SubscriptionChannelTypeEmail, SubscriptionChannelTypeSms, SubscriptionChannelTypeMessage);
  MAP_TSubscriptionStatus : array [TFhirSubscriptionStatusEnum] of TSubscriptionStatus = (ssNull, ssRequested, ssActive, ssError, ssOff);
  MAP_TSubscriptionStatus2 : array [TSubscriptionStatus] of TFhirSubscriptionStatusEnum = (SubscriptionStatusNull, SubscriptionStatusRequested, SubscriptionStatusActive, SubscriptionStatusError, SubscriptionStatusOff);
  MAP_TObservationStatus : array [TObservationStatus] of TFhirObservationStatusEnum = (ObservationStatusNull, ObservationStatusRegistered, ObservationStatusPreliminary, ObservationStatusFinal, ObservationStatusAmended, ObservationStatusCorrected, ObservationStatusCancelled, ObservationStatusEnteredInError, ObservationStatusUnknown);
  MAP_TObservationStatus2 : array [TFhirObservationStatusEnum] of TObservationStatus = (obssNull, obssRegistered, obssPreliminary, obssFinal, obssAmended, obssCorrected, obssCancelled, obssEnteredInError, obssUnknown);

type
  TFHIRExtension3 = class (TFHIRExtensionW)
  public
    function url : String; override;
    function value : TFHIRObject; override;
    function renderText : String; override;
  end;

  TFHIRCoding3 = class (TFHIRCodingW)
  public
    function getSystem : String; override;
    function getCode : String; override;
    function getVersion : String; override;
    function getDisplay : String; override;
    procedure setCode(Value: String); override;
    procedure setDisplay(Value: String); override;
    procedure setSystem(Value: String); override;
    procedure setVersion(Value: String); override;
    function renderText : String; override;
  end;

  TFhirCodeableConcept3 = class (TFhirCodeableConceptW)
  protected
    function GetText: String; override;
    procedure SetText(const Value: String); override;
  public
    function codingCount : integer; override;
    function codings : TFslList<TFhirCodingW>; override;
    procedure addCoding(coding : TFHIRCodingW); override;
    function addCoding : TFHIRCodingW; override;
    function summary : String; override;
    function fromSystem(System : String; required : boolean = false) : String; overload; override;
    function fromSystem(Systems : TArray<String>; required : boolean = false) : String; overload; override;
    function renderText : String; override;
  end;

  TFhirOperationOutcome3 = class (TFhirOperationOutcomeW)
  public
    function hasText : boolean; override;
    function text : String; override;
    function code : TFhirIssueType; override;
    procedure addIssue(issue : TFhirOperationOutcomeIssueW; free : boolean); override;
    function issues : TFslList<TFhirOperationOutcomeIssueW>; override;
    function rule(level : TIssueSeverity; source : String; typeCode : TFhirIssueType; path : string; test : boolean; msg : string) : boolean; override;
    function severity : TIssueSeverity; override;
    function issueCount : integer; override;
    function hasErrors : boolean; override;
  end;

  TFHIRBundleEntry3 = class (TFHIRBundleEntryW)
  private
    function entry : TFhirBundleEntry;
  protected
    function getRequestMethod: String; override;
    function getRequestUrl: String; override;
    function getResource: TFHIRResourceV; override;
    function getResponseDate: TFslDateTime; override;
    function getResponseETag: string; override;
    function getResponseLocation: string; override;
    function getResponseStatus: String; override;
    function getSearchMode: TFHIRBundleEntrySearchMode; override;
    function getSearchMpiMatch: String; override;
    function getSearchScore: String; override;
    procedure setRequestMethod(Value: String); override;
    procedure setRequestUrl(Value: String); override;
    procedure setResource(Value: TFHIRResourceV); override;
    procedure setResponseDate(Value: TFslDateTime); override;
    procedure setResponseETag(Value: string); override;
    procedure setResponseLocation(Value: string); override;
    procedure setResponseStatus(Value: String); override;
    procedure setSearchMode(Value: TFHIRBundleEntrySearchMode); override;
    procedure setSearchMpiMatch(Value: String); override;
    procedure setSearchScore(Value: String); override;
    function getURL: String; override;
    procedure setUrl(Value: String);  override;
    function getrequestIfNoneExist: String; override;
    procedure setrequestIfNoneExist(Value: String); override;
    function getrequestIfMatch: String; override;
    procedure setrequestIfMatch(Value: String); override;
    function getrequestIfNoneMatch: String; override;
    procedure setrequestIfNoneMatch(Value: String); override;
    function getrequestIfModifiedSince: TFslDateTime; override;
    procedure setrequestIfModifiedSince(Value: TFslDateTime); override;
  public
    function getLink(rel: String): String; override;
    procedure setLink(rel: String; const Value: String); override;
  end;

  TFhirBinary3 = class (TFhirBinaryW)
  public
    function ContentType : String; override;
    function content : TBytes; override;
  end;

  TFHIRBundle3 = class (TFHIRBundleW)
  private
    function bundle : TFhirBundle;
  public
    function next(bnd : TFHIRResourceV) : String; overload; override;
    procedure addEntries(bnd : TFHIRResourceV); override;
    procedure addEntry(bnd : TFhirBundleEntryW; first : boolean); overload; override;
    procedure addEntry(url : String; bnd : TFhirResourceV); overload; override;
    function addEntry : TFhirBundleEntryW; overload; override;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; override;
    procedure clearLinks; override;
    function entries : TFslList<TFhirBundleEntryW>; override;
    procedure listLinks(links : TFslStringDictionary); override;
    function getLink(rel: String): String; override;
    procedure setLink(rel: String; const Value: String); override;
    function getTotal: integer; override;
    procedure setTotal(Value: integer); override;
    function title : String; override;
    function getType : TBundleType; override;
    procedure setType(value: TBundleType); override;
    function getLastUpdated : TFslDateTime; override;
    procedure setLastUpdated(Value: TFslDateTime); override;
    function getTimestamp: TFslDateTime; override;
    procedure setTimestamp(Value: TFslDateTime); override;
  end;

  TFHIROperationOutcomeIssue3 = class (TFHIROperationOutcomeIssueW)
  private
    function issue : TFHIROperationOutcomeIssue;
  public
    function display : String; override;
    function severity : TIssueSeverity; override;
    function getDiagnostics: String; override;
    procedure setDiagnostics(Value: String); override;
  end;

  TFHIRSearchParamDefinition3 = class (TFHIRSearchParamDefinitionW)
  private
    function param : TFhirCapabilityStatementRestResourceSearchParam;
  public
    function name : String; override;
    function documentation : String; override;
    function type_ : TFHIRSearchParamType; override;
  end;

  TFhirCapabilityStatementRestResource3 = class (TFhirCapabilityStatementRestResourceW)
  public
    function getCode: String; override;
    procedure setCode(Value: String); override;
    function getProfile: String; override;
    function hasInteraction : boolean; override;
    procedure setProfile(Value: String); override;
    procedure addInteraction(code : String);  override;
    function getReadHistory: boolean; override;
    procedure setReadHistory(Value: boolean); override;
    procedure addParam(html, n, url, d : String; t : TFHIRSearchParamType; tgts : Array of String); override;
  end;

  TFHIRCapabilityStatement3 = class (TFHIRCapabilityStatementW)
  private
    function statement : TFhirCapabilityStatement;
  public
    function hasRest : boolean; override;
    function hasSecurity(system, code : String) : boolean; override;

    procedure readSmartExtension(var authorize, token, register: String); override;
    procedure addSmartExtensions(authorize, token, register, manage: String; caps : Array of String); override;
    function hasFormat(fmt : String) : boolean; override;

    procedure contact(kind : TContactType; value : String); override;
    procedure software(name, version, release : String); override;
    procedure impl(url, desc : String); override;
    procedure fmt(mt : String); override;
    procedure standardServer(ts, ws, pv, cv, iv : String; transactions, search, history : boolean); override;
    function addResource(code : String) : TFhirCapabilityStatementRestResourceW; override;
    procedure addOperation(name, url : String); override;

    function getURL: String; override;
    procedure setUrl(Value: String); override;
    function getName : String; override;
    procedure setName(value : String); override;
    function getVersion : String; override;
    procedure setVersion(value : String); override;
    function getDescription : String; override;
    procedure setDescription(value : String); override;
    function getStatus: TPublicationStatus; override;
    procedure setStatus(Value: TPublicationStatus); override;
    function getDate: TFslDateTime; override;
    procedure setDate(Value: TFslDateTime); override;
    function getFhirVersion: string; override;
    procedure setFhirVersion(Value: string); override;
    function getKind: TCapabilityStatementKind; override;
    procedure setKind(Value: TCapabilityStatementKind); override;
    function getAcceptUnknown: TCapabilityStatementAcceptUnknown; override;
    procedure setAcceptUnknown(const Value: TCapabilityStatementAcceptUnknown); override;

    function supportsType(name : String; interaction : TFHIRInteraction) : boolean; override;
    procedure listTypes(interactions : TFHIRInteractions; names : TStrings); override;
    procedure listSearchParams(name : String; list : TFslList<TFHIRSearchParamDefinitionW>); override;
    procedure addInstantiates(url : String); override;
  end;

  TFhirElementDefinition3 = class (TFhirElementDefinitionW)
  private
    function edefn : TFhirElementDefinition;
  public
    function path : String; override;
    function min : integer; override;
    function max : integer; override;
    function defn : String; override;
    function types : String; override;
    function typeList : TArray<String>; override;
    function explicitTypeName : String; override;
    function binding : TElementDefinitionBinding; override;
    function isSummary : boolean; override;
    function valueSet : String; override;
  end;

  TFHIRStructureDefinition3 = class (TFhirStructureDefinitionW)
  private
    function sd : TFhirStructureDefinition;
  public
    function kind : TStructureDefinitionKind; override;
    function name : String; override;
    function url : String; override;
    function type_ : String; override;
    function elements : TFslList<TFHIRElementDefinitionW>; override;
    function getDefinition(id : String; source : TElementDefinitionSourceOption) : TFHIRElementDefinitionW; override;
  end;

  TFhirParametersParameter3 = class (TFhirParametersParameterW)
  private
    function parameter : TFhirParametersParameter;
  protected
    function getValue: TFHIRObject; override;
    procedure setValue(Value: TFHIRObject); override;
    procedure populateList; override;
    function getParameterParameter(name: String): TFhirParametersParameterW; override;
    function getResourceParameter(name: String): TFHIRResourceV; override;
    function getStringParameter(name: String): String; override;
  public
    function name : String; override;
    function hasValue : boolean; override;
    function valueString : String; override;
    function getResource: TFHIRResourceV; override;
    procedure setResource(Value: TFHIRResourceV); override;
    function hasResource : boolean; override;
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamStr(name : String; value : string); override;
    procedure addParamCode(name : String; value : string); override;
    procedure addParam(name : String; value : TFHIRObject); override;
    function addParam(name : String) : TFhirParametersParameterW; override;
  end;

  TFHIRParameters3 = class (TFHIRParametersW)
  private
    function parameter : TFhirParameters;
  protected
    procedure populateList; override;
  public
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamStr(name : String; value : string); override;
    procedure addParam(name : String; value : TFHIRObject); override;
    procedure addParamCode(name : String; value : string); override;
    function addParam(name : String) : TFhirParametersParameterW; override;
    function bool(name : String) : boolean; override;
    function str(name : String) : String; override;
    function has(name : String) : boolean; override;
    function obj(name : String) : TFHIRObject; override;
    function getParameter(name: String): TFhirParametersParameterW;  override;
  end;

  TFHIRExpansionProfile3 = class (TFHIRParametersW)
  private
    function profile : TFhirExpansionProfile;
  protected
    procedure populateList; override;
  public
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamStr(name : String; value : string); override;
    procedure addParam(name : String; value : TFHIRObject); override;
    procedure addParamCode(name : String; value : string); override;
    function addParam(name : String) : TFhirParametersParameterW; override;
    function bool(name : String) : boolean; override;
    function str(name : String) : String; override;
    function has(name : String) : boolean; override;
    function obj(name : String) : TFHIRObject; override;
    function getParameter(name: String): TFhirParametersParameterW;  override;
  end;

  TFhirExpansionProfileFixedVersion3 = class (TFhirParametersParameterW)
  private
    function entry : TFhirExpansionProfileFixedVersion;
  protected
    function getValue: TFHIRObject; override;
    procedure setValue(Value: TFHIRObject); override;
    procedure populateList; override;
    function getParameterParameter(name: String): TFhirParametersParameterW; override;
    function getResourceParameter(name: String): TFHIRResourceV; override;
    function getStringParameter(name: String): String; override;
  public
    function name : String; override;
    function hasValue : boolean; override;
    function valueString : String; override;
    function getResource: TFHIRResourceV; override;
    procedure setResource(Value: TFHIRResourceV); override;
    function hasResource : boolean; override;
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamStr(name : String; value : string); override;
    procedure addParamCode(name : String; value : string); override;
    procedure addParam(name : String; value : TFHIRObject); override;
    function addParam(name : String) : TFhirParametersParameterW; override;
  end;

  TFhirValueSetExpansionContains3 = class (TFhirValueSetExpansionContainsW)
  public
    function getSystem : String; override;
    function getCode : String; override;
    function getDisplay : String; override;
    procedure setCode(Value: String); override;
    procedure setDisplay(Value: String); override;
    procedure setSystem(Value: String); override;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; override;
  end;

  TFhirValueSetExpansion3 = class (TFhirValueSetExpansionW)
  private
    function exp : TFhirValueSetExpansion;
  public
    procedure addParam(name, value : String); override;
    procedure addParam(name : String; value : boolean); override;
    function hasParam(name : string) : boolean; overload; override;
    function hasParam(name, value : string) : boolean; overload; override;
    procedure copyParams(source : TFhirValueSetExpansionW); override;
    procedure addContains(item : TFhirValueSetExpansionContainsW); override;
    function addContains : TFhirValueSetExpansionContainsW; override;
    function makeContains : TFhirValueSetExpansionContainsW; override;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; override;
  end;

  TFhirValueSetComposeIncludeFilter3 = class (TFhirValueSetComposeIncludeFilterW)
  public
    function getProp : String; override;
    function getOp : TFilterOperator; override;
    function getValue : String; override;
    procedure setOp(Value: TFilterOperator); override;
    procedure setProp(Value: String); override;
    procedure setValue(Value: String); override;
  end;

  TFhirValueSetComposeIncludeConceptDesignation3 = class (TFhirValueSetComposeIncludeConceptDesignationW)
  public
    function language : String; override;
    function value : String; override;
  end;

  TFhirValueSetComposeIncludeConcept3 = class (TFhirValueSetComposeIncludeConceptW)
  public
    function getCode : String; override;
    function getDisplay : String; override;
    procedure setCode(Value: String); override;
    procedure setDisplay(Value: String); override;
    function designations : TFslList<TFhirValueSetComposeIncludeConceptDesignationW>; override;
  end;

  TFhirValueSetComposeInclude3 = class (TFhirValueSetComposeIncludeW)
  public
    function valueSets : TArray<String>; override;
    function getSystem : String; override;
    function getVersion : String; override;
    procedure setSystem(Value: String); override;
    procedure setVersion(Value: String); override;
    function hasConcepts : boolean; override;
    function concepts : TFslList<TFhirValueSetComposeIncludeConceptW>; override;
    function hasFilters : boolean; override;
    function filters : TFslList<TFhirValueSetComposeIncludeFilterW>; override;
    function addConcept : TFhirValueSetComposeIncludeConceptW; override;
    function addFilter : TFhirValueSetComposeIncludeFilterW; override;
  end;

  TFHIRValueSet3 = class (TFHIRValueSetW)
  private
    function vs : TFhirValueSet;
  public
    function getName : String; override;
    function getURL : String; override;
    function checkCompose(place, role : String) : boolean; override;
    function imports : TArray<String>; override;
    function inlineCS : TFHIRValueSetCodeSystemW; override;
    function includes : TFslList<TFhirValueSetComposeIncludeW>; override;
    function excludes : TFslList<TFhirValueSetComposeIncludeW>; override;
    procedure clearDefinition; override;
    function hasExpansion : boolean; override;
    function expansion : TFhirValueSetExpansionW; override;
    function forceExpansion : TFhirValueSetExpansionW; override;
    procedure setUrl(value : String); override;
    procedure setName(value : String); override;
    function getVersion : String; override;
    procedure setVersion(value : String); override;
    function getDescription : String; override;
    procedure setDescription(value : String); override;
    function getStatus: TPublicationStatus; override;
    procedure setStatus(Value: TPublicationStatus); override;
    function getDate: TFslDateTime; override;
    procedure setDate(Value: TFslDateTime); override;
    function hasInlineCS : boolean; override;
    function addInclude : TFhirValueSetComposeIncludeW; override;
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function source : String; override;
  end;

  TFhirCodeSystemConceptProperty3 = class (TFhirCodeSystemConceptPropertyW)
  public
    function code : String; override;
    function value : TFHIRObject; override;
  end;

  TFhirCodeSystemConceptDesignation3 = class (TFhirCodeSystemConceptDesignationW)
  public
    function language : String; override;
    function useGen : String; override;
    function use : TFHIRObject; override;
    function value : String; override;
  end;

  TFhirCodeSystemConcept3 = class (TFhirCodeSystemConceptW)
  private
    function c : TFhirCodeSystemConcept;
  public
    function code : String; override;
    function display : String; override;
    function definition : String; override;
    function conceptList : TFhirCodeSystemConceptListW; override;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; override;
    function conceptCount : integer; override;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; override;
    function designationCount : integer; override;
    function designations : TFslList<TFhirCodeSystemConceptDesignationW>; override;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; override;
    function displayTag(tag : String) : String; override;
    procedure setDisplayTag(tag, value : String); override;
    function getCode(code : String) : TFhirCodeSystemConceptW; override;
  end;

  TFhirCodeSystemProperty3 = class (TFhirCodeSystemPropertyW)
  public
    function code : String; override;
    function type_ : TFhirCodeSystemPropertyType; override;
  end;

  TFhirCodeSystem3 = class (TFhirCodeSystemW)
  private
    function cs : TFhirCodeSystem;
  public
    function getURL : String; override;
    function getName : String; override;
    function getVersion : String; override;
    function getDescription : String; override;
    function copyright : String; override;
    function language : String; override;
    procedure setDate(Value: TFslDateTime); override;
    procedure setDescription(Value: String); override;
    procedure setName(Value: String); override;
    procedure setStatus(Value: TPublicationStatus); override;
    procedure setUrl(Value: String); override;
    procedure setVersion(Value: String); override;
    function getContent: TFhirCodeSystemContentMode; override;
    procedure setContent(Value: TFhirCodeSystemContentMode); override;
    function getCount: integer; override;
    procedure setCount(Value: integer); override;
    function getDate: TFslDateTime; override;
    function getStatus: TPublicationStatus; override;
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;

    function valueSet : String; override;
    function supplements : String; override;

    function properties : TFslList<TFhirCodeSystemPropertyW>;  override;
    // this is special because it's owned
    function conceptList : TFhirCodeSystemConceptListW; override;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; override;
    function conceptCount : integer; override;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; override;

    function isAbstract(c : TFhirCodeSystemConceptW) : boolean; override;
    function getParents(c : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptListW; override;
    function getChildren(c : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptListW; override;
    function getCode(code : String) : TFhirCodeSystemConceptW; override;
    function buildImplicitValueSet : TFHIRValueSetW; override;
  end;

  TFHIRLookupOpRequest3 = class (TFHIRLookupOpRequestW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    procedure loadCoding; override;
    function coding : TFHIRCodingW; override;
    function propList : TArray<String>; override;
    function displayLanguage : String; override;
  end;

  TFHIRLookupOpRespProperty3 = class (TFHIRLookupOpRespPropertyW)
  public
    function getDescription: string; override;
    procedure setDescription(Value: string); override;
    function getValue: TFHIRObject; override;
    procedure setValue(Value: TFHIRObject); override;
  end;

  TFHIRLookupOpRespDesignation3 = class (TFHIRLookupOpRespDesignationW)
  public
    function getUse: TFHIRObject; override;
    procedure setUse(Value: TFHIRObject); override;
  end;

  TFHIRLookupOpResponse3 = class (TFHIRLookupOpResponseW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    function addProp(name : string) : TFHIRLookupOpRespPropertyW; override;
    function addDesignation(system, code, display, value : string) : TFHIRLookupOpRespDesignationW; overload; override;
    function addDesignation(lang, value : string) : TFHIRLookupOpRespDesignationW; overload; override;
    function getVersion: String; override;
    procedure setVersion(Value: String); override;
    procedure addExtension(name, value : String); overload; override;
    procedure addExtension(name : String; value : boolean); overload; override;
    function getName: String; override;
    procedure setName(Value: String); override;
    function getDisplay: String; override;
    procedure setDisplay(Value: String); override;
  end;

  TFhirConceptMapGroupElementDependsOn3 = class (TFhirConceptMapGroupElementDependsOnW)
  public
    function property_ : String; override;
    function system_ : String; override;
    function value : String; override;
    function display : String; override;
  end;

  TFhirConceptMapGroupElementTarget3 = class (TFhirConceptMapGroupElementTargetW)
  public
    function code: String; override;
    function equivalence : TFHIRConceptEquivalence; override;
    function comments : String; override;
    function products : TFslList<TFhirConceptMapGroupElementDependsOnW>; override;
  end;

  TFhirConceptMapGroupElement3 = class (TFhirConceptMapGroupElementW)
  public
    function code: String; override;
    function targets : TFslList<TFhirConceptMapGroupElementTargetW>; override;
    function targetCount : integer; override;
    function addTarget(code : String; eq : TFHIRConceptEquivalence) : TFhirConceptMapGroupElementTargetW; override;
  end;

  TFhirConceptMapGroup3 = class (TFhirConceptMapGroupW)
  public
    function elements : TFslList<TFhirConceptMapGroupElementW>; override;
    function addElement(code : String) : TFhirConceptMapGroupElementW; override;
    function source : String; override;
    function target : String; override;
  end;

  TFhirConceptMap3 = class (TFhirConceptMapW)
  private
    function cm : TFhirConceptMap;
  protected
    function getVersion: String; override;
    procedure setVersion(Value: String); override;
  public
    function getURL : String; override;
    function getDate: TFslDateTime; override;
    function getDescription: String; override;
    function getName: String; override;
    function getStatus: TPublicationStatus; override;
    procedure setDate(Value: TFslDateTime); override;
    procedure setDescription(Value: String); override;
    procedure setName(Value: String); override;
    procedure setStatus(Value: TPublicationStatus); override;
    procedure setUrl(Value: String); override;
    function source : String; override;
    function target : String; override;
    function groups : TFslList<TFhirConceptMapGroupW>; override;
    function addGroup(source, target : String) : TFhirConceptMapGroupW; override;
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function sourceDesc : String; override;
    function targetDesc : String; override;
  end;

  TFHIRMeta3 = class (TFHIRMetaW)
  private
    FResource : TFHIRResource;
    procedure force;
    procedure setResource(value : TFHIRResource);
    function m : TFhirMeta;
  protected
    function NoElementOk : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    property Resource : TFHIRResource read FResource write SetResource;
    function getVersionId: String; override;
    procedure setVersionId(Value: String); override;
    function getLastUpdated: TFslDateTime; override;
    procedure setLastUpdated(Value: TFslDateTime); override;
    function tags : TFslList<TFHIRCodingW>; override;
    function labels : TFslList<TFHIRCodingW>; override;
    function profiles : TArray<String>; override;
    function hasTag(system, code : String) : boolean; override;
    function hasLabel(system, code : String) : boolean; override;
    procedure addTag(system, code, display : String); override;
    procedure addLabel(system, code, display : String); override;
    procedure addProfile(uri : String); override;
    procedure clearTags; override;
    procedure clearLabels; override;
    procedure clearProfiles; override;
    procedure removeTag(system, code : String); override;
    procedure removeLabel(system, code : String); override;
    procedure removeProfile(uri : String); override;
  end;

  TFHIRAuditEvent3 = class (TFhirAuditEventW)
  private
    function ae : TFHIRAuditEvent;
  public
    procedure success; override;
    procedure eventType(system, code, display : String); override;
    procedure eventSubType(system, code, display : String); override;
    procedure source(name, system, value : String); override;
    procedure sourceType(system, code, display : String); override;
    procedure participantIp(ip : String); override;
    procedure participantId(system, value, alt, name : String); override;
    function dateTime : TFslDateTime; override;
  end;

  TFHIRSubscription3 = class (TFHIRSubscriptionW)
  private
    function sub : TFhirSubscription;
  protected
    function getTopic: string; override;
    function getCriteria: String; override;
    function getDirect: boolean; override;
    function getEndpoint: String; override;
    function getError: String; override;
    function getMethod: TSubscriptionMethod; override;
    function getPayload: String; override;
    function getStatus: TSubscriptionStatus; override;
    function getSummary: String; override;
    function getHeaders: TArray<String>; override;
    procedure setCriteria(Value: String); override;
    procedure setDirect(Value: boolean); override;
    procedure setEndpoint(Value: String); override;
    procedure setError(Value: String); override;
    procedure setheaders(Value: TArray<String>); override;
    procedure setMethod(Value: TSubscriptionMethod); override;
    procedure setPayload(Value: String); override;
    procedure setStatus(Value: TSubscriptionStatus); override;
  end;

  TFhirObservationComponent3 = class (TFhirObservationComponentW)
  private
    function comp : TFhirObservationComponent;
  public
    function getValue: TFHIRObject; override;
    procedure setValue(Value: TFHIRObject); override;
    function codings : TFslList<TFHIRCodingW>; override;
    function valueW : TFHIRXVersionElementWrapper; override;
    function valueString : String; override;
    function dataAbsentReason : TFhirCodeableConceptW; override;
  end;

  TFhirObservation3 = class (TFhirObservationW)
  private
    function obs : TFHIRObservation;
  protected
    function getValue: TFHIRObject;  override;
    procedure setValue(Value: TFHIRObject); override;
    function GetDevice: String; override;
    procedure SetDevice(const Value: String); override;
    function GetDeviceName: String; override;
    procedure SetDeviceName(const Value: String); override;
    function GetSubject: String; override;
    procedure SetSubject(const Value: String); override;
    function GetIssued: TFslDateTime; override;
    procedure SetIssued(const Value: TFslDateTime); override;
    function GetEffective: TFHIRObject; override;
    function GetEffectiveDateTime: TFslDateTime; override;
    function GetEffectivePeriod: TFHIRPeriodW; override;
    procedure SetEffective(const Value: TFHIRObject); override;
    procedure SetEffectiveDateTime(const Value: TFslDateTime); override;
    procedure SetEffectivePeriod(const Value: TFHIRPeriodW); override;
    function GetCodeText: String; override;
    procedure SetCodeText(const Value: String); override;
    function GetComment: String; override;
    procedure SetComment(const Value: String); override;
  public
    function getStatus: TObservationStatus;  override;
    procedure setStatus(Value: TObservationStatus);  override;
    procedure setCode(c : TFHIRCodingW); overload; override;
    procedure setCode(system, code, display : String); overload; override;
    procedure setCode(text : String); overload; override;
    procedure addCategory(c : TFHIRCodingW); overload; override;
    procedure addCategory(system, code, display : String); overload; override;
    procedure setPeriod(start, finish : TDateTime); override;
    function addComp(system, code : String) : TFhirObservationComponentW; override;
    function codings : TFslList<TFHIRCodingW>; override;
    procedure getDates(var dt, dtMin, dtMax : TDateTime); override;
    function components : TFslList<TFhirObservationComponentW>; override;
    function valueW : TFHIRXVersionElementWrapper; override;
    function dataAbsentReason : TFhirCodeableConceptW; override;
    function categories : TFslList<TFHIRCodingW>; override;
    function hasTime : boolean; override;
    function method(force : boolean) : TFhirCodeableConceptW; override;
    function getComponent(system, code: String; var comp : TFhirObservationComponentW) : boolean; overload; override;
    function getComponent(system : String; var comp : TFhirObservationComponentW) : boolean; overload; override;
    function hasDevice : boolean; override;
    function hasIssued : boolean; override;
    function hasMethod : boolean; override;
    function hasSubject : boolean; override;
    function hasEffective : boolean; override;
  end;

  TFHIRQuantity3 = class (TFHIRQuantityW)
  private
    function qty : TFHIRQuantity;
  protected
    function getCode: String; override;
    function getSystem: String; override;
    function getUnit: String; override;
    function getValue: String; override;
    procedure setCode(Value: String); override;
    procedure setSystem(Value: String); override;
    procedure setUnit(Value: String); override;
    procedure setValue(Value: String); override;
  public
    function asDuration : TDateTime; override;
    function renderText : String; override;
  end;

  TFHIRPeriod3 = class (TFHIRPeriodW)
  private
    function period : TFHIRPeriod;
  protected
    function GetEnd: TFslDateTime; override;
    function GetStart: TFslDateTime; override;
    procedure SetEnd(const Value: TFslDateTime); override;
    procedure SetStart(const Value: TFslDateTime); override;
  public
    function renderText : String; override;
  end;

  TFHIRSubsumesOpRequest3 = class (TFHIRSubsumesOpRequestW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    function systemUri : String; override;
    function codeA : String; override;
    function codeB : String; override;
    function version : String; override;
    function hasCodingA : boolean; override;
    function hasCodingB : boolean; override;
    function codingA : TFHIRCodingW; override;
    function codingB : TFHIRCodingW; override;
  end;

  TFHIRSubsumesOpResponse3 = class (TFHIRSubsumesOpResponseW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    function getOutcome: String; override;
    procedure setOutcome(Value: String); override;
  end;

  TFHIRGroupCharacteristic3 = class (TFHIRGroupCharacteristicW)
  public
    function code : TFhirCodeableConceptW; override;
    function value : TFhirCodeableConceptW; override;
  end;

  TFHIRGroup3 = class (TFHIRGroupW)
  public
    function name : String; override;
    function hasMembers : boolean; override;
    function hasCharacteristics : boolean; override;
    function characteristics : TFslList<TFHIRGroupCharacteristicW>; override;
  end;

  TFhirPatient3 = class (TFhirPatientW)
  public
    function nameSummary : String; override;
  end;

  TFhirEncounter3 = class (TFhirEncounterW)
  public
    function patientId : String; override;
    function summary : String; override;
  end;

  TFHIRStatsOpResponse3 = class (TFHIRStatsOpResponseW)
  public
    procedure addObs(obs : TFHIRResourceV); override;
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
  end;

  TFHIRNamingSystem3 = class (TFHIRNamingSystemW)
  private
    function nm : TFHIRNamingSystem;
  protected
    function getDate: TFslDateTime; override;
    function getDescription: String; override;
    function getName: String; override;
    function getStatus: TPublicationStatus; override;
    function getURL: String; override;
    procedure setDate(Value: TFslDateTime); override;
    procedure setDescription(Value: String); override;
    procedure setName(Value: String); override;
    procedure setStatus(Value: TPublicationStatus); override;
    procedure setUrl(Value: String); override;
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function getVersion: String; override;
    procedure setVersion(Value: String); override;
  public
    function getUri : String; override;
    function hasOid(oid : String) : boolean; override;
  end;

  TFHIRStructureMap3 = class (TFHIRStructureMapW)
  public
    function url : String; override;
  end;

  TFhirTerminologyCapabilities3 = class (TFhirTerminologyCapabilitiesW)
  protected
    function getDate: TFslDateTime; override;
    function getDescription: String; override;
    function getName: String; override;
    function getStatus: TPublicationStatus; override;
    function getURL: String; override;
    procedure setDate(Value: TFslDateTime); override;
    procedure setDescription(Value: String); override;
    procedure setName(Value: String); override;
    procedure setStatus(Value: TPublicationStatus); override;
    procedure setUrl(Value: String); override;
    function getContext: String; override;
    procedure setContext(Value: String); override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function getVersion: String; override;
    procedure setVersion(Value: String); override;
  public
    procedure contact(kind : TContactType; value : String); override;
    procedure systemUri(url : String); override;
    procedure addExpansionParameter(code, doco : String); override;
  end;

  TFHIRConsent3 = class (TFHIRConsentW)
  private
    function consent : TFHIRConsent;
  protected
    function GetActive: boolean; override;
    function GetPatient: String; override;
    function GetDateTime: TFslDateTime; override;
  public
    function listProvisions : TFslList<TFhirConsentProvisionW>; override;
  end;

  TFHIRTestScript3 = class (TFHIRTestScriptW)
  private
    function ts : TFHIRTestScript;
  protected
    function getURL: String; override;
    function getName: String; override;
    function getStatus: TPublicationStatus; override;
    function getVersion: String; override;
    function getDescription: String; override;
    function getDate: TFslDateTime; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    procedure setDate(Value: TFslDateTime); override;
    procedure setUrl(Value: String); override;
    procedure setVersion(Value: String); override;
    procedure setName(Value: String); override;
    procedure setStatus(Value: TPublicationStatus); override;
    procedure setDescription(Value: String); override;
    function getContext: String; override;
  end;

  TFhirProvenance3 = class (TFhirProvenanceW)
  private
    function p : TFhirProvenance;
  public
    procedure clearTargets; override;
    procedure clearSignatures; override;
    procedure addTarget(url : String); override;
  end;

implementation

uses
  fhir3_utilities;

{ TFhirOperationOutcome3 }

procedure TFhirOperationOutcome3.addIssue(issue: TFhirOperationOutcomeIssueW; free : boolean);
begin
  (Fres as TFhirOperationOutcome).issueList.Add((issue.Element as TFhirOperationOutcomeIssue).link);
  if free then
    issue.Free;
end;

function TFhirOperationOutcome3.code: TFhirIssueType;
var
  a : TFhirIssueType;
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  result := itNull;
  if not op.issueList.IsEmpty then
    for a := low(TFhirIssueType) to High(TFhirIssueType) do
      if ExceptionTypeTranslations[a] = op.issueList[0].code then
       exit(a);
end;

function TFhirOperationOutcome3.hasErrors: boolean;
begin
  result := (Fres as TFhirOperationOutcome).hasErrors;
end;

function TFhirOperationOutcome3.hasText: boolean;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  if (op.text <> nil) and (op.text.div_ <> nil) then
    result := true
  else if (op.issueList.Count > 0) and (op.issueList[0].diagnostics <> '') then
    result := true
  else
    result := false;
end;

function TFhirOperationOutcome3.issueCount: integer;
begin
  result := (resource as TFhirOperationOutcome).issueList.Count;
end;

function TFhirOperationOutcome3.issues: TFslList<TFhirOperationOutcomeIssueW>;
var
  iss : TFhirOperationOutcomeIssue;
begin
  result := TFslList<TFhirOperationOutcomeIssueW>.create;
  for iss in (resource as TFhirOperationOutcome).issueList do
    result.Add(TFHIROperationOutcomeIssue3.Create(iss.Link));
end;

function TFhirOperationOutcome3.rule(level: TIssueSeverity; source: String; typeCode: TFhirIssueType; path: string; test: boolean; msg: string): boolean;
begin
  result := (resource as TFhirOperationOutcome).rule(ISSUE_SEVERITY_MAP2[level], source, ExceptionTypeTranslations[typeCode], path, test, msg);
end;

function TFhirOperationOutcome3.severity: TIssueSeverity;
begin
  if (resource as TFhirOperationOutcome).issueList.Count > 0 then
    result := ISSUE_SEVERITY_MAP[(resource as TFhirOperationOutcome).issueList[0].severity]
  else
    result := isFatal;
end;

function TFhirOperationOutcome3.text: String;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  if (op.text <> nil) and (op.text.div_ <> nil) then
    result := op.text.div_.AsPlainText
  else if (op.issueList.Count > 0) and (op.issueList[0].diagnostics <> '') then
    result := op.issueList[0].diagnostics
  else
    result := '';
end;

{ TFHIRBundle3 }

procedure TFHIRBundle3.addEntries(bnd: TFHIRResourceV);
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  bundle.entryList.AddAll(b.entryList);
end;

procedure TFHIRBundle3.addEntry(bnd: TFhirBundleEntryW; first: boolean);
begin
  if first then
    bundle.entryList.InsertItem(0, bnd.element.link as TFHIRBundleEntry)
  else
    bundle.entryList.AddItem(bnd.element.link as TFHIRBundleEntry);
end;

procedure TFHIRBundle3.addEntry(url: String; bnd: TFhirResourceV);
var
  e : TFHIRBundleEntry;
begin
  e := bundle.entryList.Append;
  e.fullUrl := url;
  e.resource := bnd as TFHIRResource;
end;

function TFHIRBundle3.addEntry: TFhirBundleEntryW;
begin
  result := TFhirBundleEntry3.create(bundle.entryList.append.link);
end;

function TFHIRBundle3.bundle: TFhirBundle;
begin
  result := resource as TFHIRBundle;
end;

procedure TFHIRBundle3.clearLinks;
begin
  bundle.link_List.Clear;
end;

function TFHIRBundle3.entries: TFslList<TFhirBundleEntryW>;
var
  be : TFHIRBundleEntry;
begin
  result := TFslList<TFhirBundleEntryW>.create;
  try
    for be in bundle.entryList do
      result.Add(TFhirBundleEntry3.create(be.Link));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRBundle3.GetLastUpdated: TFslDateTime;
begin
  if bundle.meta <> nil then
    result := TFslDateTime.makeNull
  else
    result := bundle.meta.lastUpdated;
end;

function TFHIRBundle3.GetLink(rel: String): String;
begin
  result := bundle.Links[rel];
end;

procedure TFHIRBundle3.listLinks(links: TFslStringDictionary);
var
  bl : TFhirBundleLink;
begin
  links.Clear;
  for bl in bundle.link_List do
    links.AddOrSetValue(bl.relation, bl.url);
end;

function TFHIRBundle3.moveToFirst(res: TFhirResourceV): TFhirBundleEntryW;
var
  fu : String;
  i : integer;
begin
  for i := bundle.entryList.Count -1 downto 0 do
    if bundle.entryList[i].resource = res then
    begin
      fu := bundle.entryList[i].fullurl;
      bundle.entrylist.DeleteByIndex(i);
    end;
  bundle.entryList.Insert(0).resource := res.Link as TFHIRResource;
  bundle.entryList[0].fullurl := fu;
  result := TFHIRBundleEntry3.Create(bundle.entryList[0].Link);
end;

function TFHIRBundle3.next(bnd: TFHIRResourceV): String;
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  result := b.Links['next'];
end;

procedure TFHIRBundle3.SetLastUpdated(Value: TFslDateTime);
begin
  if bundle.meta = nil then
    bundle.meta := TFHIRMeta.Create;
  bundle.meta.lastUpdated := value;
end;

procedure TFHIRBundle3.SetLink(rel: String; const Value: String);
begin
  bundle.Links[rel] := value;
end;

procedure TFHIRBundle3.SetTimestamp(Value: TFslDateTime);
begin
end;

procedure TFHIRBundle3.SetTotal(Value: integer);
begin
  bundle.total := inttostr(value);
end;

function TFHIRBundle3.title: String;
begin
  result := BUNDLE_TYPE_TITLE[bundle.type_];
end;

function TFHIRBundle3.GetTimestamp: TFslDateTime;
begin
  result := TFslDateTime.makeUTC;
end;

function TFHIRBundle3.gettotal: integer;
begin
  result := StrToIntDef(bundle.total, -1);
end;

function TFHIRBundle3.gettype: TBundleType;
begin
  result := MAP_TFHIRBundleTypeR[bundle.type_];
end;

procedure TFHIRBundle3.settype(value : TBundleType);
begin
  bundle.type_ := MAP_TFHIRBundleType[value];
end;

{ TFHIROperationOutcomeIssue3 }

function TFHIROperationOutcomeIssue3.display: String;
var
  i : TFHIROperationOutcomeIssue;
begin
  i := issue;
  result := i.diagnostics;
  if (i.details <> nil) and (i.details.text <> '') then
    result := i.details.text;
end;

function TFHIROperationOutcomeIssue3.issue: TFHIROperationOutcomeIssue;
begin
  result := Element as TFHIROperationOutcomeIssue;
end;

function TFHIROperationOutcomeIssue3.severity: TIssueSeverity;
begin
  result := ISSUE_SEVERITY_MAP[issue.severity];
end;

function TFHIROperationOutcomeIssue3.GetDiagnostics: String;
begin
  result := issue.diagnostics;
end;

procedure TFHIROperationOutcomeIssue3.SetDiagnostics(Value: String);
begin
  issue.diagnostics := value;
end;

{ TFHIRCapabilityStatement3 }

procedure TFHIRCapabilityStatement3.addInstantiates(url: String);
begin
  statement.instantiatesList.Append.value := url;
end;

procedure TFHIRCapabilityStatement3.addOperation(name, url: String);
var
  t : TFhirCapabilityStatementRestOperation;
begin
  t := statement.restList[0].operationList.append;
  t.name := name;
  t.definition := TFHIRReference.Create(url);
end;

function TFHIRCapabilityStatement3.addResource(code: String): TFhirCapabilityStatementRestResourceW;
begin
  result := TFhirCapabilityStatementRestResource3.create(statement.restList[0].resourceList.append.link);
  result.code := code;
end;

procedure TFHIRCapabilityStatement3.addSmartExtensions(authorize, token, register, manage: String; caps : Array of String);
var
  c: TFHIRCoding;
  ext: TFhirExtension;
  s : string;
begin
  if statement.restList.isEmpty then
    statement.restList.append.mode := RestfulCapabilityModeServer;

  if statement.restList[0].security = nil then
    statement.restList[0].security := TFhirCapabilityStatementRestSecurity.Create;
  statement.restList[0].security.cors := true;
  if authorize <> '' then
  begin
    c := statement.restList[0].security.serviceList.Append.codingList.Append;
    c.System := 'http://hl7.org/fhir/restful-security-service';
    c.code := 'SMART-on-FHIR';
    c.display := 'SMART-on-FHIR';
    statement.restList[0].security.description := 'This server implements OAuth2 for login using the Smart App Launch profile';

    ext := statement.restList[0].security.extensionList.Append;
    ext.url := 'http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris';
    // ext.addExtension('dscovery', TFhirUri.Create(ExcludeTrailingPathDelimiter(FServerContext.FormalURLSecure)+FAuthServer.AuthPath+'/discovery'));
    ext.addExtensionUri('register', register);
    ext.addExtensionUri('authorize', authorize);
    ext.addExtensionUri('token', token);
    ext.addExtensionUri('manage', manage);
    for s in caps do
      statement.restList[0].security.addExtension('http://fhir-registry.smarthealthit.org/StructureDefinition/capabilities', s);
  end;
end;

procedure TFHIRCapabilityStatement3.contact(kind: TContactType; value: String);
var
  c : TFhirContactPoint;
  ct : TFhirConformanceContact;
begin
  ct := statement.contactList.Append;
  c := ct.telecomList.Append;
  c.system := MAP_TContactType[kind];
  c.value := 'http://healthintersections.com.au/';
end;

function TFHIRCapabilityStatement3.GetURL: String;
begin
  result := statement.url;
end;

function TFHIRCapabilityStatement3.hasFormat(fmt: String): boolean;
begin
  result := statement.formatList.hasCode(fmt);
end;

function TFHIRCapabilityStatement3.hasRest: boolean;
begin
  result := statement.restList.Count > 0;
end;

function TFHIRCapabilityStatement3.hasSecurity(system, code: String): boolean;
var
  cs : TFHIRCapabilityStatement;
  cc : TFhirCodeableConcept;
begin
  cs := statement;
  result := false;
  if (cs.restList[0].security <> nil) then
    for cc in cs.restList[0].security.serviceList do
      if cc.hasCode(system, code) then
        exit(true);
end;

procedure TFHIRCapabilityStatement3.impl(url, desc: String);
begin
  if statement.implementation_ = nil then
    statement.implementation_ := TFhirCapabilityStatementImplementation.Create;
  statement.implementation_.description := desc;
  statement.implementation_.url := url;
end;

procedure TFHIRCapabilityStatement3.listSearchParams(name: String; list: TFslList<TFHIRSearchParamDefinitionW>);
var
  r : TFhirCapabilityStatementRest;
  rr : TFhirCapabilityStatementRestResource;
  sp : TFhirCapabilityStatementRestResourceSearchParam;
begin
  for r in statement.restList do
  begin
    if r.mode = RestfulCapabilityModeServer then
    begin
      if name = 'All Types' then
        for sp in r.searchParamList do
          list.Add(TFHIRSearchParamDefinition3.create(sp.Link))
      else
      begin
        for rr in r.resourceList do
        begin
          if CODES_TFHIRResourceTypesEnum[rr.type_] = name then
            for sp in rr.searchParamList do
              list.Add(TFHIRSearchParamDefinition3.create(sp.Link))
        end;
      end;
    end;
  end;
end;

procedure TFHIRCapabilityStatement3.listTypes(interactions: TFHIRInteractions; names: TStrings);
var
  r : TFhirCapabilityStatementRest;
  it : TFhirCapabilityStatementRestInteraction;
  rr : TFhirCapabilityStatementRestResource;
  ok : boolean;
  int : TFhirCapabilityStatementRestResourceInteraction;
  i : TFHIRInteraction;
begin
  for r in statement.restList do
  begin
    if r.mode = RestfulCapabilityModeServer then
    begin
      ok := false;
      for it in r.interactionList do
        for i in ALL_INTERACTIONS do
          if (i in interactions) and (it.code = INTERACTION_MAP2[i]) then
            ok := true;
      if ok then
        names.Add('All Types');
      for rr in r.resourceList do
      begin
        ok := false;
        for int in rr.interactionList do
          for i in ALL_INTERACTIONS do
            if (i in interactions) and (int.code = INTERACTION_MAP[i]) then
              ok := true;
        if ok then
          names.Add(CODES_TFHIRResourceTypesEnum[rr.type_]);
      end;
    end;
  end;
end;

procedure TFHIRCapabilityStatement3.readSmartExtension(var authorize, token, register: String);
var
  ex1, ex2 : TFhirExtension;
begin
  for ex1 in statement.restList[0].security.extensionList do
    if ex1.url = 'http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris' then
      for ex2 in ex1.extensionList do
        if ex2.url = 'authorize' then
          authorize := TFHIRUri(ex2.value).value
        else if ex2.url = 'token' then
          token := TFHIRUri(ex2.value).value
        else if ex2.url = 'register' then
          register := TFHIRUri(ex2.value).value;
end;

procedure TFHIRCapabilityStatement3.SetUrl(Value: String);
begin
  statement.url := value;
end;

function TFHIRCapabilityStatement3.getName : String;
begin
  result := statement.Name;
end;

procedure TFHIRCapabilityStatement3.setName(value : String);
begin
  statement.Name := value;
end;

function TFHIRCapabilityStatement3.getVersion : String;
begin
  result := statement.Version;
end;

procedure TFHIRCapabilityStatement3.setVersion(value : String);
begin
  statement.Version := value;
end;

procedure TFHIRCapabilityStatement3.software(name, version, release: String);
begin
  if statement.software = nil then
    statement.software := TFhirCapabilityStatementSoftware.Create;
  statement.software.name := name;
  statement.software.version := version;
  statement.software.releaseDate := TFslDateTime.fromXml(release);
end;

function TFHIRCapabilityStatement3.getDescription : String;
begin
  result := statement.Description;
end;

function TFHIRCapabilityStatement3.GetFhirVersion: string;
begin
  result := statement.fhirVersion;
end;

procedure TFHIRCapabilityStatement3.setDescription(value : String);
begin
  statement.Description := value;
end;

procedure TFHIRCapabilityStatement3.SetFhirVersion(Value: string);
begin
  statement.fhirVersion := value;
end;

function TFHIRCapabilityStatement3.GetStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[statement.Status];
end;

procedure TFHIRCapabilityStatement3.SetStatus(Value: TPublicationStatus);
begin
  statement.Status := MAP_TPublicationStatus[value];
end;

procedure TFHIRCapabilityStatement3.fmt(mt: String);
begin
  statement.formatList.Append.value := mt;
end;

function TFHIRCapabilityStatement3.GetDate: TFslDateTime;
begin
  result := statement.Date;
end;

procedure TFHIRCapabilityStatement3.SetDate(Value: TFslDateTime);
begin
  statement.Date := value;
end;

procedure TFHIRCapabilityStatement3.standardServer(ts, ws, pv, cv, iv: String; transactions, search, history : boolean);
//var
//  ext : TFhirExtension;
begin
  if statement.restList.isEmpty then
    statement.restList.append.mode := RestfulCapabilityModeServer;
  statement.restList[0].mode := RestfulCapabilityModeServer;
  if (ws <> '') then
    statement.restList[0].addExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-websocket', ws);
  if transactions then
    statement.restList[0].interactionList.Append.code := SystemRestfulInteractionTransaction;
  if search then
    statement.restList[0].interactionList.Append.code := SystemRestfulInteractionSearchSystem;
  if history then
    statement.restList[0].interactionList.Append.code := SystemRestfulInteractionHistorySystem;
  statement.text := TFhirNarrative.create;
  statement.text.status := NarrativeStatusGenerated;
  statement.instantiatesList.AddItem(TFHIRUri.Create('http://hl7.org/fhir/Conformance/terminology-server'));
  // commented out until we sort out cds-hooks
//  ext := statement.restList[0].addExtension('http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity');
//  ext.addExtension('name', 'Fetch Patient Alerts');
//  ext.addExtension('activity', pv);
//  ext.addExtension('preFetchOptional', 'Patient/{{Patient.id}}');
//  ext := statement.restList[0].addExtension('http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity');
//  ext.addExtension('name', 'Get Terminology Information');
//  ext.addExtension('activity', cv);
//  ext := statement.restList[0].addExtension('http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity');
//  ext.addExtension('name', 'Get identifier Information');
//  ext.addExtension('activity', iv);
end;

function TFHIRCapabilityStatement3.statement: TFhirCapabilityStatement;
begin
  result := FRes as TFHIRCapabilityStatement;
end;

function TFHIRCapabilityStatement3.supportsType(name: String; interaction: TFHIRInteraction): boolean;
var
  r : TFhirCapabilityStatementRest;
  it : TFhirCapabilityStatementRestInteraction;
  rr : TFhirCapabilityStatementRestResource;
  ok : boolean;
  int : TFhirCapabilityStatementRestResourceInteraction;
  i : TFHIRInteraction;
begin
  result := false;
  for r in statement.restList do
  begin
    if r.mode = RestfulCapabilityModeServer then
    begin
      if name = 'All Types' then
      begin
        ok := false;
        for it in r.interactionList do
          for i in ALL_INTERACTIONS do
            if (i = interaction) and (it.code = INTERACTION_MAP2[i]) then
              ok := true;
        exit(ok);
      end
      else
      begin
        for rr in r.resourceList do
        begin
          if CODES_TFHIRResourceTypesEnum[rr.type_] = name then
          begin
            ok := false;
            for int in rr.interactionList do
              for i in ALL_INTERACTIONS do
                if (i = interaction) and (int.code = INTERACTION_MAP[i]) then
                  ok := true;
            if ok then
              exit(ok);
          end;
        end;
      end;
    end;
  end;
end;

function TFHIRCapabilityStatement3.getKind: TCapabilityStatementKind;
begin
  case statement.kind of
    CapabilityStatementKindInstance : result := cskInstance;
    CapabilityStatementKindCapability : result := cskCapability;
    CapabilityStatementKindRequirements : result := cskRequirements;
  else
    result := cskNull;
  end;
end;

procedure TFHIRCapabilityStatement3.setKind(Value: TCapabilityStatementKind);
begin
  case value of
    cskInstance : statement.kind := CapabilityStatementKindInstance;
    cskCapability : statement.kind := CapabilityStatementKindCapability;
    cskRequirements : statement.kind := CapabilityStatementKindRequirements;
  else
    statement.kind := CapabilityStatementKindNull;
  end;
end;

function TFHIRCapabilityStatement3.getAcceptUnknown: TCapabilityStatementAcceptUnknown;
begin
  case statement.acceptUnknown of
    UnknownContentCodeNo : result := csauNo;
    UnknownContentCodeExtensions : result := csauExtensions;
    UnknownContentCodeElements : result := csauElements;
    UnknownContentCodeBoth : result := csauBoth;
  else
    result := csauNull;
  end;
end;

procedure TFHIRCapabilityStatement3.setAcceptUnknown(const Value: TCapabilityStatementAcceptUnknown);
begin
  case value of
    csauNo : statement.acceptUnknown := UnknownContentCodeNo;
    csauExtensions : statement.acceptUnknown := UnknownContentCodeExtensions;
    csauElements : statement.acceptUnknown := UnknownContentCodeElements;
    csauBoth : statement.acceptUnknown := UnknownContentCodeBoth;
  else
    statement.acceptUnknown := UnknownContentCodeNull;
  end;
end;

{ TFhirParametersParameter3 }

function TFhirParametersParameter3.addParam(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter3.Create(parameter.partList.Append.link);
  TFhirParametersParameter3(result).parameter.name := name;
  PartList.Add(result);
end;

procedure TFhirParametersParameter3.addParam(name: String; value: TFHIRObject);
begin
  parameter.AddParameter(name).value := value as TFHIRType;
end;

procedure TFhirParametersParameter3.addParamBool(name: String; value: boolean);
begin
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFhirParametersParameter3.addParamCode(name, value: string);
begin
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFhirParametersParameter3.addParamStr(name, value: string);
begin
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

function TFhirParametersParameter3.GetParameterParameter(name: String): TFhirParametersParameterW;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(t);
end;

function TFhirParametersParameter3.GetResource: TFHIRResourceV;
begin
  result := parameter.resource;
end;

function TFhirParametersParameter3.GetResourceParameter(name: String): TFHIRResourceV;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter3(t).parameter.resource);
end;

function TFhirParametersParameter3.GetStringParameter(name: String): String;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := '';
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter3(t).parameter.value.primitiveValue);
end;

function TFhirParametersParameter3.GetValue: TFHIRObject;
begin
  result := parameter.value;
end;

function TFhirParametersParameter3.hasResource: boolean;
begin
  result := parameter.resource <> nil;
end;

function TFhirParametersParameter3.hasValue: boolean;
begin
  result := parameter.value <> nil;
end;

function TFhirParametersParameter3.name: String;
begin
  result := parameter.name;
end;

function TFhirParametersParameter3.parameter: TFhirParametersParameter;
begin
  result := Element as TFhirParametersParameter;
end;

procedure TFhirParametersParameter3.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.partList do
    FList.Add(TFhirParametersParameter3.Create(t.Link));
end;

procedure TFhirParametersParameter3.SetResource(Value: TFHIRResourceV);
begin
  parameter.resource := value as TFhirResource;
end;

procedure TFhirParametersParameter3.SetValue(Value: TFHIRObject);
begin
  parameter.value := value as TFHIRType;
end;

function TFhirParametersParameter3.valueString: String;
begin
  result := parameter.value.primitiveValue;
end;

{ TFHIRParameters3 }

function TFhirParameters3.addParam(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter3.Create(parameter.parameterList.Append.link);
  TFhirParametersParameter3(result).parameter.name := name;
  ParameterList.Add(result);
end;

procedure TFhirParameters3.addParam(name: String; value: TFHIRObject);
begin
  parameter.AddParameter(name).value := value as TFHIRType;
end;

procedure TFhirParameters3.addParamBool(name: String; value: boolean);
begin
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFhirParameters3.addParamCode(name, value: string);
begin
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFhirParameters3.addParamStr(name, value: string);
begin
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

function TFHIRParameters3.bool(name: String): boolean;
begin
  result := parameter.bool[name];
end;

function TFHIRParameters3.GetParameter(name: String): TFhirParametersParameterW;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(t);
end;

function TFHIRParameters3.has(name: String): boolean;
begin
  result := parameter.hasParameter(name);
end;

function TFHIRParameters3.obj(name: String): TFHIRObject;
var
  p : TFhirParametersParameter;
begin
  if has(name) then
  begin
    p := parameter.param[name];
    if p.resource <> nil then
      result := p.resource
    else
      result := p.value;
  end
  else
    result := nil;
end;

function TFHIRParameters3.parameter: TFhirParameters;
begin
  result := Resource as TFhirParameters;
end;

procedure TFHIRParameters3.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.parameterList do
    Flist.Add(TFhirParametersParameter3.Create(t.Link));
end;

function TFHIRParameters3.str(name: String): String;
begin
  result := parameter.str[name];
end;

{ TFHIRExpansionProfile3 }

function TFHIRExpansionProfile3.addParam(name: String): TFhirParametersParameterW;
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

procedure TFHIRExpansionProfile3.addParam(name: String; value: TFHIRObject);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

procedure TFHIRExpansionProfile3.addParamBool(name: String; value: boolean);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

procedure TFHIRExpansionProfile3.addParamCode(name, value: string);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

procedure TFHIRExpansionProfile3.addParamStr(name, value: string);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

function TFHIRExpansionProfile3.bool(name: String): boolean;
begin
  if name = '_incomplete' then
    result := profile.limitedExpansion
  else if name = 'limitedExpansion' then
    result := profile.limitedExpansion
  else if name = 'includeDesignations' then
    result := profile.includeDesignations
  else if name = 'includeDefinition' then
    result := profile.includeDefinition
  else if name = 'activeOnly' then
    result := profile.activeOnly
  else if name = 'excludeNested' then
    result := profile.excludeNested
  else if name = 'excludeNotForUI' then
    result := profile.excludeNotForUI
  else if name = 'excludePostCoordinated' then
    result := profile.excludePostCoordinated
  else
    result := false;
end;

function TFHIRExpansionProfile3.str(name: String): String;
begin
  if name = '_incomplete' then
    result := BoolToStr(profile.limitedExpansion)
  else if name = 'limitedExpansion' then
    result := BoolToStr(profile.limitedExpansion)
  else if name = 'includeDesignations' then
    result := BoolToStr(profile.includeDesignations)
  else if name = 'includeDefinition' then
    result := BoolToStr(profile.includeDefinition)
  else if name = 'activeOnly' then
    result := BoolToStr(profile.activeOnly)
  else if name = 'excludeNested' then
    result := BoolToStr(profile.excludeNested)
  else if name = 'excludeNotForUI' then
    result := BoolToStr(profile.excludeNotForUI)
  else if name = 'excludePostCoordinated' then
    result := BoolToStr(profile.excludePostCoordinated)
  else if name = 'displayLanguage' then
    result := profile.displayLanguage
  else
    result := '';
end;

function TFHIRExpansionProfile3.GetParameter(name: String): TFhirParametersParameterW;
begin
  raise EFHIRException.create('Not supported for Expansion Profile');
end;

function TFHIRExpansionProfile3.has(name: String): boolean;
begin
 if name = '_incomplete' then
    result := profile.LimitedExpansionElement <> nil
  else if name = 'limitedExpansion' then
    result := profile.limitedExpansionElement <> nil
  else if name = 'includeDesignations' then
    result := profile.includeDesignationsElement <> nil
  else if name = 'includeDefinition' then
    result := profile.includeDefinitionElement <> nil
  else if name = 'activeOnly' then
    result := profile.activeOnlyElement <> nil
  else if name = 'excludeNested' then
    result := profile.excludeNestedElement <> nil
  else if name = 'excludeNotForUI' then
    result := profile.excludeNotForUIElement <> nil
  else if name = 'excludePostCoordinated' then
    result := profile.excludePostCoordinatedElement <> nil
  else if name = 'displayLanguage' then
    result := profile.displayLanguageElement <> nil
  else
    result := false;
end;

function TFHIRExpansionProfile3.obj(name: String): TFHIRObject;
begin
//  if has(name) then
//  begin
//    p := parameter.param[name];
//    if p.resource <> nil then
//      result := p.resource
//    else
//      result := p.value;
//  end
//  else
//    result := nil;
//  for p in params.parameterList do
//  begin
//    if (p.name = 'system-version') then
//    begin
//      sl := p.valueString.split(['|']);
//      if (Length(sl) = 2) then
//        result.fixedVersions.Add(TFhirExpansionParamsFixedVersion.Create(sl[0], sl[1]))
//      else if (Length(sl) = 3) and StringArrayExistsInsensitive(CODES_TFhirExpansionParamsFixedVersionMode, sl[2]) then
//        result.fixedVersions.Add(TFhirExpansionParamsFixedVersion.Create(sl[0], sl[1], TFhirExpansionParamsFixedVersionMode(StringArrayIndexOfInsensitive(CODES_TFhirExpansionParamsFixedVersionMode, sl[2]))))
//      else
//        raise ETerminologyError.Create('Unable to understand fixed system version "'+p.valueString+'"');
//    end;
//  end;
  result := nil;
end;

function TFHIRExpansionProfile3.profile: TFhirExpansionProfile;
begin
  result := Resource as TFhirExpansionProfile;
end;

procedure TFHIRExpansionProfile3.populateList;
var
  t : TFhirExpansionProfileFixedVersion;
begin
  inherited;
  for t in profile.fixedVersionList do
    if t.mode <> SystemVersionProcessingModeOverride then
      FList.Add(TFhirExpansionProfileFixedVersion3.Create(t.Link));
end;

{ TFhirExpansionProfileFixedVersion3 }

function TFhirExpansionProfileFixedVersion3.addParam(name: String): TFhirParametersParameterW;
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParam(name: String; value: TFHIRObject);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParamBool(name: String; value: boolean);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParamCode(name, value: string);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParamStr(name, value: string);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

function TFhirExpansionProfileFixedVersion3.GetParameterParameter(name: String): TFhirParametersParameterW;
begin
  result := nil;
end;

function TFhirExpansionProfileFixedVersion3.GetResource: TFHIRResourceV;
begin
  result := nil;
end;

function TFhirExpansionProfileFixedVersion3.GetResourceParameter(name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TFhirExpansionProfileFixedVersion3.GetStringParameter(name: String): String;
begin
  result := '';
end;

function TFhirExpansionProfileFixedVersion3.GetValue: TFHIRObject;
begin
  result := entry;
end;

function TFhirExpansionProfileFixedVersion3.hasResource: boolean;
begin
  result := false;
end;

function TFhirExpansionProfileFixedVersion3.hasValue: boolean;
begin
  result := true;
end;

function TFhirExpansionProfileFixedVersion3.name: String;
begin
  result := 'system-version';
end;

function TFhirExpansionProfileFixedVersion3.entry : TFhirExpansionProfileFixedVersion;
begin
  result := FElement as TFhirExpansionProfileFixedVersion;
end;

procedure TFhirExpansionProfileFixedVersion3.populateList;
begin
end;

procedure TFhirExpansionProfileFixedVersion3.SetResource(Value: TFHIRResourceV);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.SetValue(Value: TFHIRObject);
begin
  raise EFHIRException.create('Expansion Profile is read only');
end;

function TFhirExpansionProfileFixedVersion3.valueString: String;
begin
  result := entry.system+'|'+entry.version;
end;

{ TFHIRStructureDefinition3 }

function TFHIRStructureDefinition3.elements: TFslList<TFHIRElementDefinitionW>;
var
  ed : TFhirElementDefinition;
begin
  result := TFslList<TFHIRElementDefinitionW>.create;
  try
    for ed in sd.snapshot.elementList do
      result.Add(TFhirElementDefinition3.create(ed.Link));
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRStructureDefinition3.getDefinition(id: String; source: TElementDefinitionSourceOption): TFHIRElementDefinitionW;
var
  ed : TFhirElementDefinition;
begin
  result := nil;
  if (source in [edsSNAPSHOT, edsEITHER]) and (sd.snapshot <> nil) then
    for ed in sd.snapshot.elementList do
      if ed.id = id then
        exit(TFHIRElementDefinition3.Create(ed.Link));

  if (source in [edsDIFF, edsEITHER]) and (sd.differential <> nil) then
    for ed in sd.differential.elementList do
      if ed.id = id then
        exit(TFHIRElementDefinition3.Create(ed.Link));
end;

function TFHIRStructureDefinition3.kind: TStructureDefinitionKind;
begin
  case sd.kind of
    StructureDefinitionKindPrimitiveType : result := sdkPrimitive;
    StructureDefinitionKindComplexType :
      if type_ = 'Extension' then
          result := sdkExtension
        else
          result := sdkDataType;
    StructureDefinitionKindResource :result := sdkResource;
    StructureDefinitionKindLogical : result := sdkResource;
  else
    raise EFHIRException.create('Unhandled value');
  end;
end;

function TFHIRStructureDefinition3.name: String;
begin
  result := sd.name;
end;

function TFHIRStructureDefinition3.sd: TFhirStructureDefinition;
begin
  result := resource as TFhirStructureDefinition;
end;

function TFHIRStructureDefinition3.type_: String;
begin
  result := sd.type_;
end;

function TFHIRStructureDefinition3.url: String;
begin
  result := sd.url;
end;

{ TFHIRSearchParamDefinition3 }

function TFHIRSearchParamDefinition3.documentation: String;
begin
  result := param.documentation;
end;

function TFHIRSearchParamDefinition3.name: String;
begin
  result := param.name;
end;

function TFHIRSearchParamDefinition3.param: TFhirCapabilityStatementRestResourceSearchParam;
begin
  result := FElement as TFhirCapabilityStatementRestResourceSearchParam;
end;

function TFHIRSearchParamDefinition3.type_: TFHIRSearchParamType;
begin
  result := MAP_SearchParamType[param.type_];
end;

{ TFhirElementDefinition3 }

function TFhirElementDefinition3.binding: TElementDefinitionBinding;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  if b = nil then
    result := edbNone
  else
    result := MAP_ELEMENT_DEFINITION_BINDING[b.strength];
end;

function TFhirElementDefinition3.defn: String;
begin
  result := edefn.definition;
end;

function TFhirElementDefinition3.edefn: TFhirElementDefinition;
begin
  result := element as TFhirElementDefinition;
end;

function TFhirElementDefinition3.explicitTypeName: String;
begin
  result := edefn.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-explicit-type-name');
end;

function TFhirElementDefinition3.isSummary: boolean;
begin
  result := edefn.isSummary;
end;

function TFhirElementDefinition3.max: integer;
begin
  if edefn.max = '*' then
    result := MaxInt
  else
    result := StrToInt(edefn.max);
end;

function TFhirElementDefinition3.min: integer;
begin
  result := StrToInt(edefn.min);
end;

function TFhirElementDefinition3.path: String;
begin
  result := edefn.path;
end;

function TFhirElementDefinition3.typeList: TArray<String>;
var
  ed : TFhirElementDefinition;
  i : integer;
begin
  ed := edefn;
  Setlength(result, ed.type_List.Count);
  for i := 0 to ed.type_List.Count - 1 do
    result[i] := ed.type_List[i].code;
end;

function TFhirElementDefinition3.types: String;
var
  t : TFhirElementDefinitionType;
begin
  result := '';
  for t in edefn.type_List do
  begin
    if result <> '' then
      result := result+'|';
    result := result + t.code;
  end;
end;

function TFhirElementDefinition3.valueSet: String;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  result := '';
  if b <> nil then
    if b.valueSet is TFhirUri then
      result := TFhirUri(b.valueSet).value
    else if b.valueSet is TFhirReference then
      result := TFhirReference(b.valueSet).reference
end;

{ TFHIRBundleEntry3 }

function TFHIRBundleEntry3.entry: TFhirBundleEntry;
begin
  result := element as TFhirBundleEntry;
end;

function TFHIRBundleEntry3.getRequestMethod: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := CODES_TFhirHttpVerbEnum[entry.request.method];
end;

function TFHIRBundleEntry3.getRequestUrl: String;
begin
  if entry.request = nil then
    result := ''
  else
    result :=  entry.request.url;
end;

function TFHIRBundleEntry3.getResource: TFHIRResourceV;
begin
  result :=  entry.resource;
end;

function TFHIRBundleEntry3.GetResponseDate: TFslDateTime;
begin
  if entry.response = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.response.lastModified;
end;

function TFHIRBundleEntry3.GetResponseETag: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.etag;
end;

function TFHIRBundleEntry3.GetResponseLocation: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.Location;
end;

function TFHIRBundleEntry3.GetResponseStatus: String;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.status;
end;

function TFHIRBundleEntry3.getSearchMode: TFHIRBundleEntrySearchMode;
begin
  if entry.search = nil then
    result := smUnknown
  else
    result := MAP_SEARCH_MODE[entry.search.mode];
end;

function TFHIRBundleEntry3.getSearchMpiMatch: String;
begin
  if entry.search = nil then
    result := ''
  else
    result := entry.search.getExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match')
end;

function TFHIRBundleEntry3.getSearchScore: String;
begin
  if entry.search = nil then
    result := ''
  else
    result := entry.search.score;
end;

function TFHIRBundleEntry3.GetURL: String;
begin
  result := entry.fullUrl;
end;

procedure TFHIRBundleEntry3.SetRequestMethod(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.create;
  entry.request.method := TFhirHttpVerbEnum(ord(StringArrayIndexOfSensitive(CODES_TFhirHttpVerbEnum, value)));
end;

procedure TFHIRBundleEntry3.SetRequestUrl(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.create;
  entry.request.url := value;
end;

procedure TFHIRBundleEntry3.SetResource(Value: TFHIRResourceV);
begin
  entry.resource := value as TFHIRResource;
end;

procedure TFHIRBundleEntry3.SetResponseDate(Value: TFslDateTime);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.create;
  entry.response.lastModified := value;
end;

procedure TFHIRBundleEntry3.SetResponseETag(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.create;
  entry.response.ETag := value;
end;

procedure TFHIRBundleEntry3.SetResponseLocation(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.create;
  entry.response.Location := value;
end;

procedure TFHIRBundleEntry3.SetResponseStatus(Value: String);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.create;
  entry.response.status := value;
end;

procedure TFHIRBundleEntry3.SetSearchMode(Value: TFHIRBundleEntrySearchMode);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.create;
  entry.search.mode := MAP_SEARCH_MODE2[value];
end;

procedure TFHIRBundleEntry3.SetSearchMpiMatch(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.create;
  entry.search.setExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match', value)
end;

procedure TFHIRBundleEntry3.SetSearchScore(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.create;
  entry.search.score := value;
end;

procedure TFHIRBundleEntry3.SetUrl(Value: String);
begin
  entry.fullUrl := value;
end;

function TFHIRBundleEntry3.GetLink(rel: String): String;
begin
  result := entry.Links[rel];
end;

procedure TFHIRBundleEntry3.SetLink(rel: String; const Value: String);
begin
  entry.Links[rel] := value;
end;

function TFHIRBundleEntry3.GetrequestIfNoneExist: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.ifNoneExist;
end;

procedure TFHIRBundleEntry3.SetrequestIfNoneExist(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.create;
  entry.request.ifNoneExist := value;
end;

function TFHIRBundleEntry3.GetrequestIfMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfMatch;
end;

procedure TFHIRBundleEntry3.SetrequestIfMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.create;
  entry.request.IfMatch := value;
end;

function TFHIRBundleEntry3.GetrequestIfNoneMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfNoneMatch;
end;

procedure TFHIRBundleEntry3.SetrequestIfNoneMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.create;
  entry.request.IfNoneMatch := value;
end;

function TFHIRBundleEntry3.GetrequestIfModifiedSince: TFslDateTime;
begin
  if entry.request = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.request.ifModifiedSince;
end;

procedure TFHIRBundleEntry3.SetrequestIfModifiedSince(Value: TFslDateTime);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.create;
  entry.request.ifModifiedSince := value;
end;

{ TFHIRValueSet3 }

function TFHIRValueSet3.addInclude: TFhirValueSetComposeIncludeW;
begin
  if vs.compose = nil then
    vs.compose := TFhirValueSetCompose.Create;
  result := TFhirValueSetComposeInclude3.Create(vs.compose.includeList.Append.link);
end;

function TFHIRValueSet3.checkCompose(place, role: String): boolean;
begin
  result := vs.compose <> nil;
  if result then
    vs.compose.checkNoModifiers(place, role, nil);
end;

procedure TFHIRValueSet3.clearDefinition;
begin
  vs.purpose := '';
  vs.compose := nil;
  vs.description := '';
  vs.contactList.Clear;
  vs.copyright := '';
  vs.publisher := '';
  vs.extensionList.Clear;
  vs.text := nil;
end;

function TFHIRValueSet3.excludes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  result := TFslList<TFhirValueSetComposeIncludeW>.create(vs.compose.excludeList.Count);
  for c in vs.compose.excludeList do
    result.Add(TFhirValueSetComposeInclude3.Create(c.Link));
end;

function TFHIRValueSet3.expansion: TFhirValueSetExpansionW;
begin
  result := TFhirValueSetExpansion3.create(vs.expansion.Link);
end;

function TFHIRValueSet3.forceExpansion: TFhirValueSetExpansionW;
begin
  vs.expansion := TFhirValueSetExpansion.create;
  vs.expansion.timestamp := TFslDateTime.makeUTC;
  vs.expansion.identifier := NewGuidURN;
  result := TFhirValueSetExpansion3.create(vs.expansion.Link);
end;

function TFHIRValueSet3.getContext: String;
begin
  result := vs.context;
end;

function TFHIRValueSet3.getDescription: String;
begin
  result := vs.description;
end;

function TFHIRValueSet3.hasExpansion: boolean;
begin
  result := vs.expansion <> nil;
end;

function TFHIRValueSet3.hasInlineCS: boolean;
begin
  result := false;
end;

function TFHIRValueSet3.imports: TArray<String>;
begin
  SetLength(result, 0);
end;

function TFHIRValueSet3.includes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  result := TFslList<TFhirValueSetComposeIncludeW>.create(vs.compose.includeList.Count);
  for c in vs.compose.includeList do
    result.Add(TFhirValueSetComposeInclude3.Create(c.Link));
end;

function TFHIRValueSet3.inlineCS: TFHIRValueSetCodeSystemW;
begin
  result := nil;
end;

function TFHIRValueSet3.GetDate: TFslDateTime;
begin
  result := vs.date;
end;

procedure TFHIRValueSet3.SetDate(Value: TFslDateTime);
begin
  vs.date := value;
end;

procedure TFHIRValueSet3.setDescription(value: String);
begin
  vs.description := value;
end;

procedure TFHIRValueSet3.setName(value: String);
begin
  vs.name := value;
end;

procedure TFHIRValueSet3.SetPublisher(Value: String);
begin
  vs.publisher := value;
end;

procedure TFHIRValueSet3.SetStatus(Value: TPublicationStatus);
begin
  vs.status := MAP_TPublicationStatus[value];
end;

procedure TFHIRValueSet3.setUrl(value: String);
begin
  vs.url := value;
end;

procedure TFHIRValueSet3.setVersion(value: String);
begin
  vs.version := value;
end;

function TFHIRValueSet3.source: String;
begin
  result := vs.source;
end;

function TFHIRValueSet3.getName: String;
begin
  result := vs.name;
end;

function TFHIRValueSet3.GetPublisher: String;
begin
  result := vs.publisher;
end;

function TFHIRValueSet3.GetStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[vs.status];
end;

function TFHIRValueSet3.GetURL: String;
begin
  result := vs.url;
end;

function TFHIRValueSet3.getVersion: String;
begin
  result := vs.version;
end;

function TFHIRValueSet3.vs: TFhirValueSet;
begin
  result := Resource as TFHIRValueSet;
end;

{ TFhirValueSetComposeInclude3 }

function TFhirValueSetComposeInclude3.addConcept: TFhirValueSetComposeIncludeConceptW;
begin
  result := TFhirValueSetComposeIncludeConcept3.create((Element as TFhirValueSetComposeInclude).conceptList.Append.link);
end;

function TFhirValueSetComposeInclude3.addFilter: TFhirValueSetComposeIncludeFilterW;
begin
  result := TFhirValueSetComposeIncludeFilter3.create((Element as TFhirValueSetComposeInclude).filterList.Append.link);
end;

function TFhirValueSetComposeInclude3.concepts: TFslList<TFhirValueSetComposeIncludeConceptW>;
var
  i : TFhirValueSetComposeIncludeConcept;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptW>.create((Element as TFhirValueSetComposeInclude).ConceptList.Count);
  for i in (Element as TFhirValueSetComposeInclude).ConceptList do
    result.Add(TFhirValueSetComposeIncludeConcept3.Create(i.Link));
end;

function TFhirValueSetComposeInclude3.filters: TFslList<TFhirValueSetComposeIncludeFilterW>;
var
  i : TFhirValueSetComposeIncludeFilter;
begin
  result := TFslList<TFhirValueSetComposeIncludeFilterW>.create((Element as TFhirValueSetComposeInclude).filterList.Count);
  for i in (Element as TFhirValueSetComposeInclude).filterList do
    result.Add(TFhirValueSetComposeIncludeFilter3.Create(i.Link));
end;

function TFhirValueSetComposeInclude3.hasConcepts: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count > 0;
end;

function TFhirValueSetComposeInclude3.hasFilters: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count > 0;
end;

procedure TFhirValueSetComposeInclude3.SetSystem(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).system := value;
end;

procedure TFhirValueSetComposeInclude3.SetVersion(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).version := value;
end;

function TFhirValueSetComposeInclude3.getSystem: String;
begin
  result := (Element as TFhirValueSetComposeInclude).system;
end;

function TFhirValueSetComposeInclude3.valueSets: TArray<String>;
var
  i : integer;
begin
  SetLength(result, TFhirValueSetComposeInclude(element).valueSetList.count);
  for i := 0 to TFhirValueSetComposeInclude(element).valueSetList.count - 1 do
    result[i] :=  TFhirValueSetComposeInclude(element).valueSetList[i].value;
end;

function TFhirValueSetComposeInclude3.getVersion: String;
begin
  result := (Element as TFhirValueSetComposeInclude).version;
end;

{ TFhirValueSetComposeIncludeFilter3 }

function TFhirValueSetComposeIncludeFilter3.getOp: TFilterOperator;
begin
  result := MAP_TFilterOperator[(Element as TFhirValueSetComposeIncludeFilter).op];
end;

function TFhirValueSetComposeIncludeFilter3.getProp: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).property_;
end;

function TFhirValueSetComposeIncludeFilter3.getValue: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).value;
end;

procedure TFhirValueSetComposeIncludeFilter3.SetOp(Value: TFilterOperator);
begin
  (Element as TFhirValueSetComposeIncludeFilter).op := MAP_TFilterOperatorR[value];
end;

procedure TFhirValueSetComposeIncludeFilter3.SetProp(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).property_ := value;
end;

procedure TFhirValueSetComposeIncludeFilter3.SetValue(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).value := value;
end;

{ TFhirValueSetComposeIncludeConcept3 }

function TFhirValueSetComposeIncludeConcept3.getCode: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).code;
end;

function TFhirValueSetComposeIncludeConcept3.designations: TFslList<TFhirValueSetComposeIncludeConceptDesignationW>;
var
  item : TFhirValueSetComposeIncludeConceptDesignation;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptDesignationW>.create;
  for item in (Element as TFhirValueSetComposeIncludeConcept).designationList do
    result.Add(TFhirValueSetComposeIncludeConceptDesignation3.create(item.Link));
end;

function TFhirValueSetComposeIncludeConcept3.getDisplay: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).display;
end;

procedure TFhirValueSetComposeIncludeConcept3.SetCode(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).code := Value;
end;

procedure TFhirValueSetComposeIncludeConcept3.SetDisplay(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).display := Value;
end;

{ TFHIRLookupOpResponse3 }

function TFHIRLookupOpResponse3.addDesignation(system, code, display, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.create;
  try
    p.use := TFHIRCoding.Create;
    p.use.system := system;
    p.use.display := display;
    p.use.code := code;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation3.create(p.Link);
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse3.addDesignation(lang, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.create;
  try
    p.language := lang;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation3.create(p.Link);
  finally
    p.free;
  end;
end;

procedure TFHIRLookupOpResponse3.addExtension(name: String; value: boolean);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

procedure TFHIRLookupOpResponse3.addExtension(name, value: String);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

function TFHIRLookupOpResponse3.addProp(name: string): TFHIRLookupOpRespPropertyW;
var
  p : TFHIRLookupOpRespProperty_;
begin
  p := TFHIRLookupOpRespProperty_.create;
  try
    p.code := name;
    (op as TFHIRLookupOpResponse).property_List.Add(p.link as TFHIRLookupOpRespProperty_);
    result := TFHIRLookupOpRespProperty3.create(p.Link);
  finally
    p.Free;
  end;
end;

function TFHIRLookupOpResponse3.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationResponse).asParams;
end;

function TFHIRLookupOpResponse3.GetDisplay: String;
begin
  result := (op as TFHIRLookupOpResponse).display;
end;

function TFHIRLookupOpResponse3.GetName: String;
begin
  result := (op as TFHIRLookupOpResponse).name;
end;

function TFHIRLookupOpResponse3.GetVersion: String;
begin
  result := (op as TFHIRLookupOpResponse).version;
end;

procedure TFHIRLookupOpResponse3.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpResponse).load(params);
end;

procedure TFHIRLookupOpResponse3.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpResponse).load(params as TFHIRParameters);
end;

procedure TFHIRLookupOpResponse3.SetDisplay(Value: String);
begin
  (op as TFHIRLookupOpResponse).display := value;
end;

procedure TFHIRLookupOpResponse3.SetName(Value: String);
begin
  (op as TFHIRLookupOpResponse).name := value;
end;

procedure TFHIRLookupOpResponse3.SetVersion(Value: String);
begin
  (op as TFHIRLookupOpResponse).version := value;
end;

{ TFHIRLookupOpRespDesignation3 }

function TFHIRLookupOpRespDesignation3.GetUse: TFHIRObject;
begin
  result := (obj as TFHIRLookupOpRespDesignation).use;
end;

procedure TFHIRLookupOpRespDesignation3.SetUse(Value: TFHIRObject);
begin
  (obj as TFHIRLookupOpRespDesignation).use := value as TFhirCoding;
end;

{ TFHIRLookupOpRespProperty3 }

function TFHIRLookupOpRespProperty3.GetDescription: string;
begin
  result := (obj as TFHIRLookupOpRespProperty_).description;
end;

function TFHIRLookupOpRespProperty3.GetValue: TFHIRObject;
begin
  result := (obj as TFHIRLookupOpRespProperty_).value;
end;

procedure TFHIRLookupOpRespProperty3.SetDescription(Value: string);
begin
  (obj as TFHIRLookupOpRespProperty_).description := value;
end;

procedure TFHIRLookupOpRespProperty3.SetValue(Value: TFHIRObject);
begin
  (obj as TFHIRLookupOpRespProperty_).value := value as TFhirType;
end;

{ TFHIRExtension3 }

function TFHIRExtension3.renderText: String;
begin
  result := gen(element as TFhirExtension);
end;

function TFHIRExtension3.url: String;
begin
  result := (Element as TFHIRExtension).url;
end;

function TFHIRExtension3.value: TFHIRObject;
begin
  result := (Element as TFHIRExtension).value;
end;

{ TFHIRCoding3 }

function TFHIRCoding3.GetCode: String;
begin
  result := (element as TFHIRCoding).code;
end;

function TFHIRCoding3.GetDisplay: String;
begin
  result := (element as TFHIRCoding).display;
end;

function TFHIRCoding3.GetSystem: String;
begin
  result := (element as TFHIRCoding).system;
end;

function TFHIRCoding3.GetVersion: String;
begin
  result := (element as TFHIRCoding).version;
end;

function TFHIRCoding3.renderText: String;
begin
  result := gen(element as TFhirCoding);
end;

procedure TFHIRCoding3.SetCode(Value: String);
begin
  (element as TFHIRCoding).code := value;
end;

procedure TFHIRCoding3.SetDisplay(Value: String);
begin
  (element as TFHIRCoding).display := value;
end;

procedure TFHIRCoding3.SetSystem(Value: String);
begin
  (element as TFHIRCoding).system := value;
end;

procedure TFHIRCoding3.SetVersion(Value: String);
begin
  (element as TFHIRCoding).version := value;
end;

{ TFhirCodeSystemProperty3 }

function TFhirCodeSystemProperty3.code: String;
begin
  result := (Element as TFhirCodeSystemProperty).code;
end;

function TFhirCodeSystemProperty3.type_: TFhirCodeSystemPropertyType;
begin
  result := MAP_TFhirConceptPropertyTypeEnum[(Element as TFhirCodeSystemProperty).type_];
end;

{ TFhirCodeSystemConceptProperty3 }

function TFhirCodeSystemConceptProperty3.code: String;
begin
  result := (Element as TFhirCodeSystemConceptProperty).code;
end;

function TFhirCodeSystemConceptProperty3.value: TFHIRObject;
begin
  result := (Element as TFhirCodeSystemConceptProperty).value;
end;

{ TFhirCodeSystemConceptDesignation3 }

function TFhirCodeSystemConceptDesignation3.language: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).language;
end;

function TFhirCodeSystemConceptDesignation3.use: TFHIRObject;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).use;
end;

function TFhirCodeSystemConceptDesignation3.useGen: String;
begin
  result := gen((Element as TFhirCodeSystemConceptDesignation).use)
end;

function TFhirCodeSystemConceptDesignation3.value: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).value;
end;

{ TFhirCodeSystemConcept3 }

function TFhirCodeSystemConcept3.c: TFhirCodeSystemConcept;
begin
  result := Element as TFhirCodeSystemConcept;
end;

function TFhirCodeSystemConcept3.code: String;
begin
  result := c.code;
end;

function TFhirCodeSystemConcept3.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept3.create((element as TFhirCodeSystemConcept).conceptList[ndx].Link);
end;

function TFhirCodeSystemConcept3.conceptCount: integer;
begin
  result := c.conceptList.Count;
end;

function TFhirCodeSystemConcept3.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.create;
    for i in (element as TFhirCodeSystemConcept).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept3.create(i.Link));
  end;
  result := FConceptList;
end;

function TFhirCodeSystemConcept3.definition: String;
begin
  result := c.definition;
end;

function TFhirCodeSystemConcept3.designationCount: integer;
begin
  result := c.designationList.Count;
end;

function TFhirCodeSystemConcept3.designations: TFslList<TFhirCodeSystemConceptDesignationW>;
var
  i : TFhirCodeSystemConceptDesignation;
begin
  result := TFslList<TFhirCodeSystemConceptDesignationW>.create;
  for i in c.designationList do
    result.Add(TFhirCodeSystemConceptDesignation3.Create(i.Link));
end;

function TFhirCodeSystemConcept3.display: String;
begin
  result := c.display;
end;

function TFhirCodeSystemConcept3.displayTag(tag: String): String;
begin
  result := c.displayElement.Tags[tag];
end;

function getCodeWrapper(list : TFhirCodeSystemConceptList; code : String) : TFhirCodeSystemConceptW;
var
  cc : TFhirCodeSystemConcept;
begin
  result := nil;
  for cc in list do
  begin
    if cc.code = code then
      result := TFhirCodeSystemConcept3.Create(cc.Link);
    if cc.hasConceptList then
    begin
      result := getCodeWrapper(cc.conceptList, code);
      if result <> nil then
        exit;
    end;
  end;
end;

function TFhirCodeSystemConcept3.getCode(code: String): TFhirCodeSystemConceptW;
begin
  if code = self.code then
    result := self.link
  else
    result := getCodeWrapper((element as TFhirCodeSystemConcept).conceptList, code);
end;

function TFhirCodeSystemConcept3.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (element as TFhirCodeSystemConcept).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystemConcept3.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
var
  i : TFhirCodeSystemConceptProperty;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.create;
  for i in c.property_List do
    result.Add(TFhirCodeSystemConceptProperty3.Create(i.Link));
end;

procedure TFhirCodeSystemConcept3.setDisplayTag(tag, value: String);
begin
  c.displayElement.Tags[tag] := value;
end;

{ TFhirCodeSystem3 }

function TFhirCodeSystem3.buildImplicitValueSet: TFHIRValueSetW;
begin
  result := TFHIRValueSet3.Create(cs.buildImplicitValueSet);
end;

function TFhirCodeSystem3.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept3.create(cs.conceptList[ndx].Link);
end;

function TFhirCodeSystem3.conceptCount: integer;
begin
  result := cs.conceptList.Count;
end;

function TFhirCodeSystem3.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.create;
    for i in (resource as TFhirCodeSystem).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept3.create(i.Link));
  end;
  result := FConceptList;
end;

function TFhirCodeSystem3.copyright: String;
begin
  result := cs.copyright;
end;

function TFhirCodeSystem3.cs: TFhirCodeSystem;
begin
  result := Resource as TFhirCodeSystem;
end;

function TFhirCodeSystem3.GetCount: integer;
begin
  result := StrToInt(cs.count);
end;

function TFhirCodeSystem3.GetDate: TFslDateTime;
begin
  result := cs.date;
end;

function TFhirCodeSystem3.getDescription: String;
begin
  result := cs.description;
end;

function TFhirCodeSystem3.getChildren(c: TFhirCodeSystemConceptW): TFhirCodeSystemConceptListW;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getChildren(c.element as TFhirCodeSystemConcept);
  try
    result := TFhirCodeSystemConceptListW.create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept3.Create(i.Link));
      result.link;
    finally
      result.Free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem3.getCode(code: String): TFhirCodeSystemConceptW;
begin
  result := getCodeWrapper(cs.conceptList, code);
end;

function TFhirCodeSystem3.getContent: TFhirCodeSystemContentMode;
begin
  result := MAP_TFhirCodeSystemContentModeR[cs.Content];
end;

function TFhirCodeSystem3.getContext: String;
begin
  result := cs.context;
end;

function TFhirCodeSystem3.getParents(c: TFhirCodeSystemConceptW): TFhirCodeSystemConceptListW;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getParents(c.element as TFhirCodeSystemConcept);
  try
    result := TFhirCodeSystemConceptListW.create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept3.Create(i.Link));
      result.link;
    finally
      result.Free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem3.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (resource as TFhirCodeSystem).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystem3.isAbstract(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isAbstract(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem3.language: String;
begin
  result := cs.language;
end;

function TFhirCodeSystem3.GetName: String;
begin
  result := cs.name;
end;

function TFhirCodeSystem3.properties: TFslList<TFhirCodeSystemPropertyW>;
var
  i : TFhirCodeSystemProperty;
begin
  result := TFslList<TFhirCodeSystemPropertyW>.create;
  for i in cs.property_List do
    result.Add(TFhirCodeSystemProperty3.Create(i.Link));
end;

procedure TFhirCodeSystem3.SetContent(Value: TFhirCodeSystemContentMode);
begin
  cs.content := MAP_TFhirCodeSystemContentMode[Value];
end;

procedure TFhirCodeSystem3.SetCount(Value: integer);
begin
  cs.count := inttostr(Value);
end;

procedure TFhirCodeSystem3.SetDate(Value: TFslDateTime);
begin
  cs.date := Value;
end;

procedure TFhirCodeSystem3.SetDescription(Value: String);
begin
  cs.description := Value;
end;

procedure TFhirCodeSystem3.SetName(Value: String);
begin
  cs.name := Value;
end;

procedure TFhirCodeSystem3.SetPublisher(Value: String);
begin
  cs.publisher := value;
end;

procedure TFhirCodeSystem3.SetStatus(Value: TPublicationStatus);
begin
  cs.status := MAP_TPublicationStatus[Value];
end;

procedure TFhirCodeSystem3.SetUrl(Value: String);
begin
  cs.url := Value;
end;

procedure TFhirCodeSystem3.SetVersion(Value: String);
begin
  cs.version := Value;
end;

function TFhirCodeSystem3.supplements: String;
begin
  result := '';
end;

function TFhirCodeSystem3.valueSet: String;
begin
  result := cs.valueSet;
end;

function TFhirCodeSystem3.GetPublisher: String;
begin
  result := cs.publisher;
end;

function TFhirCodeSystem3.GetStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[cs.status];
end;

function TFhirCodeSystem3.GetURL: String;
begin
  result := cs.url;
end;

function TFhirCodeSystem3.getVersion: String;
begin
  result := cs.version;
end;

{ TFhirValueSetExpansion3 }

procedure TFhirValueSetExpansion3.addContains(item: TFhirValueSetExpansionContainsW);
begin
  exp.containsList.Add((item.Element as TFhirValueSetExpansionContains).link);
end;

function TFhirValueSetExpansion3.addContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains3.Create((element as TFhirValueSetExpansion).containsList.Append.Link);
end;

procedure TFhirValueSetExpansion3.addParam(name: String; value: boolean);
begin
  exp.AddParam(name, value);
end;

procedure TFhirValueSetExpansion3.addParam(name, value: String);
begin
  exp.AddParam(name, value);
end;

function TFhirValueSetExpansion3.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.create;
  for item in (Element as TFhirValueSetExpansion).containsList do
    result.Add(TFhirValueSetExpansionContains3.Create(item.Link));
end;

procedure TFhirValueSetExpansion3.copyParams(source: TFhirValueSetExpansionW);
var
  param : TFhirValueSetExpansionParameter;
begin
  for param in (source.Element as TFhirValueSetExpansion).parameterList do
    (Element as TFhirValueSetExpansion).parameterList.Add(param.Link);
end;

function TFhirValueSetExpansion3.exp: TFhirValueSetExpansion;
begin
  result := element as TFhirValueSetExpansion;
end;

function TFhirValueSetExpansion3.hasParam(name, value: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) and (param.value.primitiveValue = value) then
      exit(true);
end;

function TFhirValueSetExpansion3.makeContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains3.Create(TFhirValueSetExpansionContains.create);
end;

function TFhirValueSetExpansion3.hasParam(name: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) then
      exit(true);
end;

{ TFhirValueSetExpansionContains3 }

function TFhirValueSetExpansionContains3.getcode: String;
begin
  result := (Element as TFhirValueSetExpansionContains).code;
end;

function TFhirValueSetExpansionContains3.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.create;
  for item in (Element as TFhirValueSetExpansionContains).containsList do
    result.Add(TFhirValueSetExpansionContains3.Create(item.Link));
end;

function TFhirValueSetExpansionContains3.getdisplay: String;
begin
  result := (Element as TFhirValueSetExpansionContains).display;
end;

function TFhirValueSetExpansionContains3.getsystem: String;
begin
  result := (Element as TFhirValueSetExpansionContains).system;
end;

procedure TFhirValueSetExpansionContains3.SetCode(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).code := value;
end;

procedure TFhirValueSetExpansionContains3.SetDisplay(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).display := value;
end;

procedure TFhirValueSetExpansionContains3.SetSystem(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).system := value;
end;

{ TFhirValueSetComposeIncludeConceptDesignation3 }

function TFhirValueSetComposeIncludeConceptDesignation3.language: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).language;
end;

function TFhirValueSetComposeIncludeConceptDesignation3.value: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).value;
end;

{ TFhirConceptMap3 }

function TFhirConceptMap3.addGroup(source, target: String): TFhirConceptMapGroupW;
var
  g : TFhirConceptMapGroup;
begin
  g := cm.groupList.Append;
  g.source := source;
  g.target := target;
  result := TFhirConceptMapGroup3.create(g.Link);
end;

function TFhirConceptMap3.cm: TFhirConceptMap;
begin
  result := Resource as TFhirConceptMap;
end;

function TFhirConceptMap3.GetVersion: String;
begin
  result := cm.version;
end;

function TFhirConceptMap3.groups: TFslList<TFhirConceptMapGroupW>;
var
  g : TFhirConceptMapGroup;
begin
  result := TFslList<TFhirConceptMapGroupW>.create;
  for g in cm.groupList do
    result.Add(TFhirConceptMapGroup3.create(g.Link))
end;

procedure TFhirConceptMap3.SetVersion(Value: String);
begin
  cm.version := value;
end;

function TFhirConceptMap3.source: String;
begin
  if (cm.source is TFhirReference) then
    result := (cm.source as TFhirReference).reference
  else if cm.source <> nil then
    result := cm.source.primitiveValue;
end;

function TFhirConceptMap3.sourceDesc: String;
begin
  result := cm.sourceDesc;
end;

function TFhirConceptMap3.target: String;
begin
  if (cm.target is TFhirReference) then
    result := (cm.target as TFhirReference).reference
  else if cm.target <> nil then
    result := cm.target.primitiveValue;
end;

function TFhirConceptMap3.targetDesc: String;
begin
  result := cm.targetDesc;
end;

function TFhirConceptMap3.GetURL: String;
begin
  result := cm.url;
end;

function TFhirConceptMap3.GetDate: TFslDateTime;
begin
  result := cm.Date;
end;

function TFhirConceptMap3.GetDescription: String;
begin
  result := cm.Description;
end;

function TFhirConceptMap3.GetName: String;
begin
  result := cm.Name;
end;

function TFhirConceptMap3.GetStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[cm.Status];
end;

procedure TFhirConceptMap3.SetDate(Value: TFslDateTime);
begin
  cm.Date := value;
end;

procedure TFhirConceptMap3.SetDescription(Value: String);
begin
  cm.Description := value;
end;

procedure TFhirConceptMap3.SetName(Value: String);
begin
  cm.Name := value;
end;

procedure TFhirConceptMap3.SetStatus(Value: TPublicationStatus);
begin
  cm.Status := MAP_TPublicationStatus[value];
end;

procedure TFhirConceptMap3.SetUrl(Value: String);
begin
  cm.Url := value;
end;

procedure TFhirConceptMap3.SetPublisher(Value: String);
begin
  cm.publisher := value;
end;

function TFhirConceptMap3.getContext: String;
begin
  result := cm.context;
end;

function TFhirConceptMap3.GetPublisher: String;
begin
  result := cm.publisher;
end;

{ TFhirConceptMapGroupElementTarget3 }

function TFhirConceptMapGroupElementTarget3.code: String;
begin
  result := (Element as TFhirConceptMapGroupElementTarget).code;
end;

function TFhirConceptMapGroupElementTarget3.comments: String;
begin
  result := (Element as TFhirConceptMapGroupElementTarget).comment;
end;

function TFhirConceptMapGroupElementTarget3.equivalence: TFHIRConceptEquivalence;
begin
  result := MAP_TFHIRConceptEquivalence[(Element as TFhirConceptMapGroupElementTarget).equivalence];
end;

function TFhirConceptMapGroupElementTarget3.products: TFslList<TFhirConceptMapGroupElementDependsOnW>;
var
  i : TFhirConceptMapGroupElementTargetDependsOn;
begin
  result := TFslList<TFhirConceptMapGroupElementDependsOnW>.create;
  for i in (Element as TFhirConceptMapGroupElementTarget).productList do
    result.Add(TFhirConceptMapGroupElementDependsOn3.Create(i.link));
end;

{ TFhirConceptMapGroupElement3 }

function TFhirConceptMapGroupElement3.addTarget(code: String; eq: TFHIRConceptEquivalence): TFhirConceptMapGroupElementTargetW;
var
  t : TFhirConceptMapGroupElementTarget;
begin
  t := (Element as TFhirConceptMapGroupElement).targetList.Append;
  t.code := code;
  t.equivalence := MAP_TFHIRConceptEquivalenceR[eq];
  result := TFhirConceptMapGroupElementTarget3.Create(t.link);
end;

function TFhirConceptMapGroupElement3.code: String;
begin
  result := (Element as TFhirConceptMapGroupElement).code;
end;

function TFhirConceptMapGroupElement3.targetCount: integer;
begin
  result := (Element as TFhirConceptMapGroupElement).targetList.Count;
end;

function TFhirConceptMapGroupElement3.targets: TFslList<TFhirConceptMapGroupElementTargetW>;
var
  i : TFhirConceptMapGroupElementTarget;
begin
  result := TFslList<TFhirConceptMapGroupElementTargetW>.create;
  for i in (Element as TFhirConceptMapGroupElement).targetList do
    result.Add(TFhirConceptMapGroupElementTarget3.Create(i.link));
end;

{ TFhirConceptMapGroup3 }

function TFhirConceptMapGroup3.addElement(code: String): TFhirConceptMapGroupElementW;
var
  t : TFhirConceptMapGroupElement;
begin
  t := (Element as TFhirConceptMapGroup).elementList.Append;
  t.code := code;
  result := TFhirConceptMapGroupElement3.Create(t.link);
end;

function TFhirConceptMapGroup3.elements: TFslList<TFhirConceptMapGroupElementW>;
var
  t : TFhirConceptMapGroupElement;
begin
  result := TFslList<TFhirConceptMapGroupElementW>.create;
  for t in (Element as TFhirConceptMapGroup).elementList do
    result.Add(TFhirConceptMapGroupElement3.Create(t.link))
end;

function TFhirConceptMapGroup3.source: String;
begin
  result := (Element as TFhirConceptMapGroup).source;
end;

function TFhirConceptMapGroup3.target: String;
begin
  result := (Element as TFhirConceptMapGroup).target;
end;

{ TFHIRMeta3 }

procedure TFHIRMeta3.addLabel(system, code, display: String);
var
  c : TFHIRCoding;
begin
  force;
  c := TFHIRCoding.create;
  try
    c.system := system;
    c.code := code;
    c.display := display;
    m.securityList.Add(c.link);
  finally
    c.Free;
  end;
end;

procedure TFHIRMeta3.addProfile(uri: String);
begin
  force;
  m.profileList.Add(TFhirUri.Create(uri));
end;

procedure TFHIRMeta3.addTag(system, code, display: String);
var
  c : TFHIRCoding;
begin
  force;
  c := TFHIRCoding.create;
  try
    c.system := system;
    c.code := code;
    c.display := display;
    m.tagList.Add(c.link);
  finally
    c.Free;
  end;
end;

procedure TFHIRMeta3.clearLabels;
begin
  if Element <> nil then
    m.securityList.Clear;
end;

procedure TFHIRMeta3.clearProfiles;
begin
  if m <> nil then
    m.profileList.Clear;
end;

procedure TFHIRMeta3.clearTags;
begin
  if Element <> nil then
    m.tagList.Clear;
end;

destructor TFHIRMeta3.destroy;
begin
  FResource.Free;
  inherited;
end;

procedure TFHIRMeta3.force;
begin
  if Element = nil then
  begin
    FElement := TFHIRMeta.Create;
    Resource.meta := m.Link;
  end;
end;

function TFHIRMeta3.GetLastUpdated: TFslDateTime;
begin
  if Element = nil then
    result := TFslDateTime.makeNull
  else
    result := m.lastUpdated;
end;

function TFHIRMeta3.GetVersionId: String;
begin
  if Element = nil then
    result := ''
  else
    result := m.versionId;
end;

function TFHIRMeta3.hasLabel(system, code: String): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if Element <> nil then
    for c in m.securityList do
      if (c.system = system) and (c.code = code) then
        exit(true);
end;

function TFHIRMeta3.hasTag(system, code: String): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if Element <> nil then
    for c in m.tagList do
       if (c.system = system) and (c.code = code) then
        exit(true);
end;

function TFHIRMeta3.labels: TFslList<TFHIRCodingW>;
var
  i : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.create;
  if Element <> nil then
    for i in m.securityList do
      result.Add(TFHIRCoding3.create(i.Link));
end;

function TFHIRMeta3.m: TFhirMeta;
begin
  result := Element as TFHIRMeta;
end;

function TFHIRMeta3.NoElementOk: boolean;
begin
  result := true;
end;

function TFHIRMeta3.profiles: TArray<String>;
var
  i : integer;
begin
  if m <> nil then
  begin
    SetLength(result, m.profileList.Count);
    if Element <> nil then
      for i := 0 to m.profileList.Count - 1 do
        result[i] := m.profileList[i].value;
  end;
end;

procedure TFHIRMeta3.removeLabel(system, code: String);
begin
  if Element <> nil then
    m.removeLabel(system, code);
end;

procedure TFHIRMeta3.removeProfile(uri: String);
begin
  if Element <> nil then
    m.profileList.removeUri(uri);
end;

procedure TFHIRMeta3.removeTag(system, code: String);
begin
  if Element <> nil then
    m.removeTag(system, code);
end;

procedure TFHIRMeta3.SetLastUpdated(Value: TFslDateTime);
begin
  force;
  m.lastUpdated := value;
end;

procedure TFHIRMeta3.setResource(value: TFHIRResource);
begin
  FResource.Free;
  FResource := value;
end;

procedure TFHIRMeta3.SetVersionId(Value: String);
begin
  force;
  m.versionId := value;
end;

function TFHIRMeta3.tags: TFslList<TFHIRCodingW>;
var
  i : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.create;
  if Element <> nil then
    for i in m.tagList do
      result.Add(TFHIRCoding3.create(i.Link));
end;

function TFHIRMeta3.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FResource.sizeInBytes);
end;

{ TFhirBinary3 }

function TFhirBinary3.content: TBytes;
begin
  result := (resource as TFHIRBinary).content;
end;

function TFhirBinary3.ContentType: String;
begin
  result := (resource as TFHIRBinary).contentType;
end;

{ TFHIRAuditEvent3 }

function TFHIRAuditEvent3.ae: TFHIRAuditEvent;
begin
  result := Resource as TFhirAuditEvent;
end;

function TFHIRAuditEvent3.dateTime: TFslDateTime;
begin
  result := ae.event.dateTime;
end;

procedure TFHIRAuditEvent3.eventSubType(system, code, display: String);
var
  c : TFHIRCoding;
begin
  if ae.event = nil then
    ae.event := TFhirAuditEventEvent.Create;
  c := ae.event.subtypeList.Append;;
  c.code := code;
  c.system := system;
  c.Display := display;
end;

procedure TFHIRAuditEvent3.eventType(system, code, display: String);
var
  c : TFHIRCoding;
begin
  if ae.event = nil then
    ae.event := TFhirAuditEventEvent.Create;
  c := TFHIRCoding.Create;
  ae.event.type_ := c;
  c.code := code;
  c.system := system;
  c.Display := display;
end;

procedure TFHIRAuditEvent3.participantId(system, value, alt, name: String);
var
  p: TFhirAuditEventParticipant;
begin
  p := ae.participantList.append;
  p.userId := TFhirIdentifier.Create;
  p.userId.system := system;
  p.userId.value := value;
  p.altId := alt;
  p.name := name;
end;

procedure TFHIRAuditEvent3.participantIp(ip: String);
var
  p: TFhirAuditEventParticipant;
begin
  p := ae.participantList.append;
  p.network := TFhirAuditEventParticipantNetwork.Create;
  p.network.address := ip;
  p.network.type_ := NetworkType2;
end;

procedure TFHIRAuditEvent3.source(name, system, value: String);
begin
  if ae.source = nil then
    ae.source := TFhirAuditEventSource.Create;
  ae.source.site := name;
  ae.source.identifier := TFhirIdentifier.Create;
  ae.source.identifier.system := system;
  ae.source.identifier.value := value;
end;

procedure TFHIRAuditEvent3.sourceType(system, code, display: String);
var
  c : TFHIRCoding;
begin
  if ae.source = nil then
    ae.source := TFhirAuditEventSource.Create;
  c := ae.source.type_List.Append;
  c.code := code;
  c.system := system;
  c.Display := display;
end;

procedure TFHIRAuditEvent3.success;
begin
  if ae.event = nil then
    ae.event := TFhirAuditEventEvent.Create;
  ae.event.action := AuditEventActionE;
  ae.event.outcome := AuditEventOutcome0;
  ae.event.dateTime := TFslDateTime.makeUTC;
end;

{ TFhirCapabilityStatementRestResource3 }

procedure TFhirCapabilityStatementRestResource3.addParam(html, n, url,d: String; t: TFHIRSearchParamType; tgts: array of String);
var
  param : TFhirCapabilityStatementRestResourceSearchParam;
begin
  param := TFhirCapabilityStatementRestResourceSearchParam.create;
  try
    param.name := n;
    param.definition := url;
    param.documentation := d;
    param.type_ := MAP_TFHIRSearchParamType2[t];
    (Element as TFhirCapabilityStatementRestResource).searchParamList.add(param.link);
  finally
    param.free;
  end;
//  html.append('<li>'+n+' : '+FormatTextToHTML(d)+'</li>');
end;

function TFhirCapabilityStatementRestResource3.GetCode: String;
begin
  result := CODES_TFhirResourceTypesEnum[(Element as TFhirCapabilityStatementRestResource).type_];
end;

procedure TFhirCapabilityStatementRestResource3.SetCode(Value: String);
begin
  (Element as TFhirCapabilityStatementRestResource).type_Element := TFhirEnum.create('http://hl7.org/fhir/resource-types', value);
end;

function TFhirCapabilityStatementRestResource3.GetProfile: String;
begin
  if (Element as TFhirCapabilityStatementRestResource).profile <> nil then
    result := (Element as TFhirCapabilityStatementRestResource).profile.reference;
end;

procedure TFhirCapabilityStatementRestResource3.SetProfile(Value: String);
begin
  if value = '' then
    (Element as TFhirCapabilityStatementRestResource).profile := nil
  else
    (Element as TFhirCapabilityStatementRestResource).profile := TFhirReference.Create(value);
end;

procedure TFhirCapabilityStatementRestResource3.addInteraction(code: String);
begin
  (Element as TFhirCapabilityStatementRestResource).interactionList.Append.codeElement := TFhirEnum.create('http://hl7.org/fhir/ValueSet/type-restful-interaction', code);
end;

function TFhirCapabilityStatementRestResource3.GetReadHistory: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).readHistory;
end;

function TFhirCapabilityStatementRestResource3.hasInteraction: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).interactionList.Count > 0;
end;

procedure TFhirCapabilityStatementRestResource3.SetReadHistory(Value: boolean);
begin
  (Element as TFhirCapabilityStatementRestResource).readHistory := Value;
end;

{ TFHIRSubscription3 }

function TFHIRSubscription3.GetCriteria: String;
begin
  result := sub.criteria;
end;

function TFHIRSubscription3.GetDirect: boolean;
begin
  result := sub.channel.endpointElement.hasExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct')
    and (sub.channel.endpointElement.getExtensionString('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct') = 'true');
end;

function TFHIRSubscription3.GetEndpoint: String;
begin
  result := sub.channel.endpoint;
end;

function TFHIRSubscription3.GetError: String;
begin
  result := sub.error;
end;

function TFHIRSubscription3.GetHeaders: TArray<String>;
var
  i : integer;
begin
  setLength(result, sub.channel.headerList.Count);
  for i := 0 to sub.channel.headerList.Count - 1 do
    result[i] := sub.channel.headerList[i].value;
end;

function TFHIRSubscription3.GetMethod: TSubscriptionMethod;
begin
  result := MAP_TSubscriptionMethod[sub.channel.type_];
end;

function TFHIRSubscription3.GetPayload: String;
begin
  result := sub.channel.payload;
end;

function TFHIRSubscription3.GetStatus: TSubscriptionStatus;
begin
  result := MAP_TSubscriptionStatus[sub.status];
end;

function TFHIRSubscription3.GetSummary: String;
var
  s : TFhirString;
begin
  result := sub.channel.type_Element.value+#1+sub.channel.endpoint+#1+sub.channel.payload;
  for s in sub.channel.headerList do
    result := result+#0+s.value;
//  result := result+#0+subst.channel.header;
end;

function TFHIRSubscription3.getTopic: string;
begin
  result := '';
end;

procedure TFHIRSubscription3.SetCriteria(Value: String);
begin
  sub.criteria := value;
end;

procedure TFHIRSubscription3.SetDirect(Value: boolean);
begin
  if value then
    sub.channel.endpointElement.setExtensionBoolean('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct', 'true')
  else
    sub.channel.endpointElement.removeExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct');
end;

procedure TFHIRSubscription3.SetEndpoint(Value: String);
begin
  sub.channel.endpoint := value;
end;

procedure TFHIRSubscription3.SetError(Value: String);
begin
  sub.error := value;
end;

procedure TFHIRSubscription3.Setheaders(Value: TArray<String>);
var
  s : String;
begin
  sub.channel.headerList.Clear;
  for s in value do
    sub.channel.headerList.Append.value := s;
end;

procedure TFHIRSubscription3.SetMethod(Value: TSubscriptionMethod);
begin
  sub.channel.type_ := MAP_TSubscriptionMethod2[value];
end;

procedure TFHIRSubscription3.SetPayload(Value: String);
begin
  sub.channel.payload := value;
end;

procedure TFHIRSubscription3.SetStatus(Value: TSubscriptionStatus);
begin
  sub.status := MAP_TSubscriptionStatus2[value];
end;

function TFHIRSubscription3.sub: TFhirSubscription;
begin
  result := resource as TFhirSubscription;
end;

{ TFhirObservationComponent3 }

function TFhirObservationComponent3.codings: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.create;
  if comp.code <> nil then
    for c in comp.code.codingList do
      result.Add(TFHIRCoding3.Create(c.Link));
end;

function TFhirObservationComponent3.comp: TFhirObservationComponent;
begin
  result := (Element as TFhirObservationComponent);
end;

function TFhirObservationComponent3.dataAbsentReason: TFhirCodeableConceptW;
begin
  if comp.dataAbsentReason = nil then
    result := nil
  else
    result := TFhirCodeableConcept3.Create(comp.dataAbsentReason.link);
end;

function TFhirObservationComponent3.GetValue: TFHIRObject;
begin
  result := comp.value;
end;

procedure TFhirObservationComponent3.SetValue(Value: TFHIRObject);
begin
  comp.value := value as TFHIRType;
end;

function TFhirObservationComponent3.valueString: String;
begin
  if (comp.value <> nil) and (comp.value.isPrimitive) then
    result := comp.value.primitiveValue
  else
    result := '';
end;

function TFhirObservationComponent3.valueW: TFHIRXVersionElementWrapper;
begin
  if comp.value is TFhirCodeableConcept then
    result := TFhirCodeableConcept3.Create(comp.value.Link)
  else if comp.value is TFHIRQuantity then
    result := TFHIRQuantity3.Create(comp.value.Link)
  else
    result := nil;
end;

{ TFhirObservation3 }

procedure TFhirObservation3.setCode(c: TFHIRCodingW);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.codingList.add((c.Element as TFHIRCoding).Link);
end;

procedure TFhirObservation3.setCode(system, code, display: String);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.codingList.add(TFHIRCoding.Create(system, code, display));
end;

function TFhirObservation3.addComp(system, code: String): TFhirObservationComponentW;
var
  c : TFhirObservationComponent;
begin
  c := TFhirObservationComponent.Create;
  c.code := TFhirCodeableConcept.Create;
  c.code.codingList.add(TFhirCoding.Create(system, code));
  result := TFhirObservationComponent3.Create(c.link);
end;

function TFhirObservation3.GetStatus: TObservationStatus;
begin
  result := MAP_TObservationStatus2[obs.status];
end;

function TFhirObservation3.obs: TFHIRObservation;
begin
  result := resource as TFhirObservation;
end;

procedure TFhirObservation3.SetCodeText(const Value: String);
begin
  if value <> '' then
  begin
    if obs.code = nil then
      obs.code := TFhirCodeableConcept.create;
    obs.code.text := Value
  end
  else if (obs.code <> nil) and not obs.code.hasCoding then
    obs.code := nil;
end;

procedure TFhirObservation3.SetComment(const Value: String);
begin
  obs.comment := value;
end;

procedure TFhirObservation3.SetDevice(const Value: String);
begin
  if (value = '') then
  begin
    if (obs.device <> nil) and (obs.device.display <> '') then
      obs.device.reference := ''
    else
      obs.device := nil
  end
  else
  begin
    if obs.device = nil then
      obs.device := TFhirReference.Create;
    obs.device.reference := value;
  end;
end;

procedure TFhirObservation3.SetDeviceName(const Value: String);
begin
  if (value = '') then
  begin
    if (obs.device <> nil) and (obs.device.reference <> '') then
      obs.device.display := ''
    else
      obs.device := nil
  end
  else
  begin
    if obs.device = nil then
      obs.device := TFhirReference.Create;
    obs.device.display := value;
  end;
end;

procedure TFhirObservation3.SetEffective(const Value: TFHIRObject);
begin
  obs.effective := Value as TFhirType;
end;

procedure TFhirObservation3.SetEffectiveDateTime(const Value: TFslDateTime);
begin
  SetEffective(TFhirDateTime.Create(value));
end;

procedure TFhirObservation3.SetEffectivePeriod(const Value: TFHIRPeriodW);
begin
  obs.effective := (value.Element as TFHIRType).Link;
end;

procedure TFhirObservation3.SetIssued(const Value: TFslDateTime);
begin
  obs.issued := Value;
end;

procedure TFhirObservation3.setPeriod(start, finish: TDateTime);
begin
  obs.effective := TFhirPeriod.Create;
  TFhirPeriod(obs.effective).start := TFslDateTime.makeUTC(start);
  TFhirPeriod(obs.effective).end_ := TFslDateTime.makeUTC(finish);
end;

procedure TFhirObservation3.SetStatus(Value: TObservationStatus);
begin
  obs.status := MAP_TObservationStatus[value];
end;

procedure TFhirObservation3.SetSubject(const Value: String);
begin
  if value = '' then
    obs.subject := nil
  else
  begin
    if obs.subject = nil then
      obs.subject := TFhirReference.Create;
    obs.subject.reference := value;
  end;
end;

function TFhirObservation3.codings: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.create;
  if obs.code <> nil then
    for c in obs.code.codingList do
      result.Add(TFHIRCoding3.Create(c.Link));
end;

function TFhirObservation3.components: TFslList<TFhirObservationComponentW>;
var
  c : TFhirObservationComponent;
begin
  result := TFslList<TFhirObservationComponentW>.create;
  for c in obs.componentList do
    result.Add(TFhirObservationComponent3.Create(c.Link));
end;

function TFhirObservation3.dataAbsentReason: TFhirCodeableConceptW;
begin
  if obs.dataAbsentReason = nil then
    result := nil
  else
    result := TFhirCodeableConcept3.Create(obs.dataAbsentReason.link);
end;

function TFhirObservation3.GetCodeText: String;
begin
  if obs.code = nil then
    result := ''
  else
    result := obs.code.text;
end;

function TFhirObservation3.GetComment: String;
begin
  result := obs.comment;
end;

function TFhirObservation3.getComponent(system, code: String; var comp: TFhirObservationComponentW): boolean;
var
  c : TFHIRObservationComponent;
begin
  comp := nil;
  result := obs.getComponent(system, code, c);
  if result then
    comp := TFHIRObservationComponent3.create(c.link);
end;

function TFhirObservation3.getComponent(system: String; var comp: TFhirObservationComponentW): boolean;
var
  c : TFHIRObservationComponent;
begin
  comp := nil;
  result := obs.getComponent(system, c);
  if result then
    comp := TFHIRObservationComponent3.create(c.link);
end;

procedure TFhirObservation3.getDates(var dt, dtMin, dtMax: TDateTime);
begin
  if obs.effective is TFHIRDateTime then
  begin
    dt := (obs.effective as TFHIRDateTime).value.UTC.DateTime;
    dtMin := (obs.effective as TFHIRDateTime).value.Min.UTC.DateTime;
    dtMax := (obs.effective as TFHIRDateTime).value.Max.UTC.DateTime;
  end
  else
  begin
    dt := 0;
    if (obs.effective as TFHIRPeriod).start.null then
      dtMin := 0
    else
      dtMin := (obs.effective as TFHIRPeriod).start.Min.UTC.DateTime;
    if (obs.effective as TFHIRPeriod).end_.null then
//      dtMax := MAXSQLDATE
    else
      dtMax := (obs.effective as TFHIRPeriod).end_.Max.UTC.DateTime;
  end;
end;

function TFhirObservation3.GetDevice: String;
begin
  if obs.device <> nil then
    result := obs.device.reference
  else
    result := '';
end;

function TFhirObservation3.GetDeviceName: String;
begin
  if obs.device <> nil then
    result := obs.device.display
  else
    result := '';
end;

function TFhirObservation3.GetEffective: TFHIRObject;
begin
  result := obs.effective;
end;

function TFhirObservation3.GetEffectiveDateTime: TFslDateTime;
begin
  if obs.effective is TFhirDateTime then
    result := (obs.effective as TFhirDateTime).value
  else
    result := TFslDateTime.makeNull;
end;

function TFhirObservation3.GetEffectivePeriod: TFHIRPeriodW;
begin
  if obs.effective is TFhirPeriod then
    result := TFHIRPeriod3.create(obs.effective.Link)
  else
    result := nil;
end;

function TFhirObservation3.GetIssued: TFslDateTime;
begin
  result := obs.issued;
end;

function TFhirObservation3.GetValue: TFHIRObject;
begin
  result := obs.value;
end;

procedure TFhirObservation3.SetValue(Value: TFHIRObject);
begin
  obs.value := value as TFhirType;
end;

function TFhirObservation3.valueW: TFHIRXVersionElementWrapper;
begin
  if obs.value is TFhirCodeableConcept then
    result := TFhirCodeableConcept3.Create(obs.value.Link)
  else if obs.value is TFHIRQuantity then
    result := TFHIRQuantity3.Create(obs.value.Link)
  else
    result := nil;
end;

function TFhirObservation3.hasDevice: boolean;
begin
  result := obs.Device <> nil;
end;

function TFhirObservation3.hasEffective: boolean;
begin
  result := obs.Effective <> nil;
end;

function TFhirObservation3.hasIssued: boolean;
begin
  result := obs.IssuedElement <> nil;
end;

function TFhirObservation3.hasMethod: boolean;
begin
  result := obs.Method <> nil;
end;

function TFhirObservation3.hasSubject: boolean;
begin
  result := obs.Subject <> nil;
end;

function TFhirObservation3.hasTime: boolean;
begin
  result := obs.effective <> nil;
end;

function TFhirObservation3.method(force : boolean) : TFhirCodeableConceptW;
begin
  if (obs.method = nil) and force then
    obs.method := TFhirCodeableConcept.Create;

  if obs.method = nil then
    result := nil
  else
    result := TFhirCodeableConcept3.Create(obs.method.link);
end;

function TFhirObservation3.categories: TFslList<TFHIRCodingW>;
var
  cc : TFHIRCodeableConcept;
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.create;
  for cc in obs.categoryList do
    for c in cc.codingList do
      result.Add(TFHIRCoding3.Create(c.Link));
end;

function TFhirObservation3.GetSubject: String;
begin
  if obs.subject <> nil then
    result := obs.subject.reference
  else
    result := '';
end;

procedure TFhirObservation3.addCategory(c: TFHIRCodingW);
begin
  obs.categoryList.Append.codingList.add((c.Element as TFHIRCoding).Link);
end;

procedure TFhirObservation3.addCategory(system, code, display: String);
begin
  obs.categoryList.Append.codingList.add(TFHIRCoding.Create(system, code, display));
end;

procedure TFhirObservation3.setCode(text: String);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.text := text;
end;

{ TFHIRQuantity3 }

function TFHIRQuantity3.asDuration: TDateTime;
begin
  result := qty.asDuration;
end;

function TFHIRQuantity3.GetCode: String;
begin
  result := qty.code;
end;

function TFHIRQuantity3.GetSystem: String;
begin
  result := qty.system;
end;

function TFHIRQuantity3.GetUnit: String;
begin
  result := qty.unit_;
end;

function TFHIRQuantity3.GetValue: String;
begin
  result := qty.value;
end;

function TFHIRQuantity3.qty: TFHIRQuantity;
begin
  result := Element as TFHIRQuantity;
end;

function TFHIRQuantity3.renderText: String;
begin
  result := gen(element as TFhirQuantity);
end;

procedure TFHIRQuantity3.SetCode(Value: String);
begin
  qty.code := Value;
end;

procedure TFHIRQuantity3.SetSystem(Value: String);
begin
  qty.system := Value;
end;

procedure TFHIRQuantity3.SetUnit(Value: String);
begin
  qty.unit_ := Value;
end;

procedure TFHIRQuantity3.SetValue(Value: String);
begin
  qty.value := Value;
end;

{ TFHIRLookupOpRequest3 }

function TFHIRLookupOpRequest3.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationRequest).asParams;
end;

function TFHIRLookupOpRequest3.coding: TFHIRCodingW;
begin
  result := TFHIRCoding3.Create((op as TFHIRLookupOpRequest).coding.Link);
end;

function TFHIRLookupOpRequest3.displayLanguage: String;
begin
  result := (op as TFHIRLookupOpRequest).displayLanguage;
end;

procedure TFHIRLookupOpRequest3.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpRequest).load(params);
end;

procedure TFHIRLookupOpRequest3.loadCoding;
var
  req : TFHIRLookupOpRequest;
begin
  req := (Op as TFHIRLookupOpRequest);
  if (req.coding = nil) and (req.system <> '') then
  begin
    req.coding := TFhirCoding.Create;
    req.coding.system := req.system;
    req.coding.code := req.code;
    req.coding.version := req.version;
  end;
  if req.coding = nil then
    raise ETerminologyError.create('Unable to find a code to lookup (need coding or system/code)');
end;

function TFHIRLookupOpRequest3.propList: TArray<String>;
var
  i : integer;
begin
  SetLength(result, (op as TFHIRLookupOpRequest).property_List.Count);
  for i := 0 to (op as TFHIRLookupOpRequest).property_List.Count -1 do
    result[i] := (op as TFHIRLookupOpRequest).property_List[i];
end;

procedure TFHIRLookupOpRequest3.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpRequest).load(params as TFHIRParameters);
end;

{ TFHIRSubsumesOpRequest3 }

function TFHIRSubsumesOpRequest3.asParams: TFHIRResourceV;
begin
  result := (op as TFHIRSubsumesOpRequest).asParams;
end;

procedure TFHIRSubsumesOpRequest3.load(params: TFHIRResourceV);
begin
  (op as TFHIRSubsumesOpRequest).load(params as TFHIRParameters);
end;

function TFHIRSubsumesOpRequest3.codeA: String;
begin
  result := (op as TFHIRSubsumesOpRequest).codeA;
end;

function TFHIRSubsumesOpRequest3.codeB: String;
begin
  result := (op as TFHIRSubsumesOpRequest).codeB;
end;

procedure TFHIRSubsumesOpRequest3.load(params: THTTPParameters);
begin
  (op as TFHIRSubsumesOpRequest).load(params);
end;

function TFHIRSubsumesOpRequest3.systemUri: String;
begin
  result := (op as TFHIRSubsumesOpRequest).system;
end;

function TFHIRSubsumesOpRequest3.codingA: TFHIRCodingW;
begin
  result := TFHIRCoding3.Create((op as TFHIRSubsumesOpRequest).codingA);
end;

function TFHIRSubsumesOpRequest3.codingB: TFHIRCodingW;
begin
  result := TFHIRCoding3.Create((op as TFHIRSubsumesOpRequest).codingB);
end;

function TFHIRSubsumesOpRequest3.hasCodingA: boolean;
begin
  result := (op as TFHIRSubsumesOpRequest).codingA <> nil;
end;

function TFHIRSubsumesOpRequest3.hasCodingB: boolean;
begin
  result := (op as TFHIRSubsumesOpRequest).codingB <> nil;
end;

function TFHIRSubsumesOpRequest3.version: String;
begin
  result :=(op as TFHIRSubsumesOpRequest).version;
end;

{ TFHIRSubsumesOpResponse3 }

function TFHIRSubsumesOpResponse3.asParams: TFHIRResourceV;
begin
  result := (op as TFHIRSubsumesOpResponse).asParams;
end;

procedure TFHIRSubsumesOpResponse3.load(params: TFHIRResourceV);
begin
  (op as TFHIRSubsumesOpResponse).load(params as TFHIRParameters);
end;

function TFHIRSubsumesOpResponse3.GetOutcome: String;
begin
  result := (op as TFHIRSubsumesOpResponse).outcome;
end;

procedure TFHIRSubsumesOpResponse3.load(params: THTTPParameters);
begin
  (op as TFHIRSubsumesOpResponse).load(params);
end;

procedure TFHIRSubsumesOpResponse3.SetOutcome(Value: String);
begin
  (op as TFHIRSubsumesOpResponse).outcome := value;
end;

{ TFhirCodeableConcept3 }

procedure TFhirCodeableConcept3.addCoding(coding: TFHIRCodingW);
begin
  (Element as TFhirCodeableConcept).codingList.Add((coding.Element as TFHIRCoding).link);
end;

function TFhirCodeableConcept3.addCoding: TFHIRCodingW;
begin
  result := TFHIRCoding3.create((Element as TFhirCodeableConcept).codingList.Append.link);
end;

function TFhirCodeableConcept3.codingCount: integer;
begin
  result := (Element as TFhirCodeableConcept).codingList.Count;
end;

function TFhirCodeableConcept3.codings: TFslList<TFhirCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFhirCodingW>.create;
  for c in (Element as TFhirCodeableConcept).codingList do
    result.Add(TFHIRCoding3.Create(c.Link));
end;

function TFhirCodeableConcept3.fromSystem(Systems: TArray<String>; required: boolean): String;
begin
  result := (Element as TFhirCodeableConcept).fromSystem(Systems, required);
end;

function TFhirCodeableConcept3.GetText: String;
begin
  result := (Element as TFhirCodeableConcept).text;
end;

function TFhirCodeableConcept3.renderText: String;
begin
  result := gen(element as TFhirCodeableConcept);
end;

procedure TFhirCodeableConcept3.SetText(const Value: String);
begin
  (Element as TFhirCodeableConcept).text := Value;
end;

function TFhirCodeableConcept3.fromSystem(System: String; required: boolean): String;
begin
  result := (Element as TFhirCodeableConcept).fromSystem(System, required);
end;

function TFhirCodeableConcept3.summary: String;
begin
  result := summarise(Element as TFhirCodeableConcept);
end;

{ TFHIRGroup3 }

function TFHIRGroup3.characteristics: TFslList<TFHIRGroupCharacteristicW>;
var
  gc : TFHIRGroupCharacteristic;
begin
  result := TFslList<TFHIRGroupCharacteristicW>.create;
  for gc in (Resource as TFHIRGroup).characteristicList do
    result.add(TFHIRGroupCharacteristic3.create(gc.link));
end;

function TFHIRGroup3.hasCharacteristics: boolean;
begin
  result := (Resource as TFHIRGroup).characteristicList.count > 0;
end;

function TFHIRGroup3.hasMembers: boolean;
begin
  result := (Resource as TFHIRGroup).memberList.count > 0;
end;

function TFHIRGroup3.name: String;
begin
  result := (Resource as TFHIRGroup).name;
end;

{ TFHIRGroupCharacteristic3 }

function TFHIRGroupCharacteristic3.code: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConcept3.Create((element as TFHIRGroupCharacteristic).code);
end;

function TFHIRGroupCharacteristic3.value: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConcept3.Create((element as TFHIRGroupCharacteristic).value);
end;

{ TFHIRStatsOpResponse3 }

procedure TFHIRStatsOpResponse3.addObs(obs: TFHIRResourceV);
begin
  (Op as TFHIRStatsOpResponse).sourceList.Add(obs as TFhirObservation);
end;

function TFHIRStatsOpResponse3.asParams: TFHIRResourceV;
begin
  result := (op as TFHIRLookupOpResponse).asParams;
end;

procedure TFHIRStatsOpResponse3.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpResponse).load(params);
end;

procedure TFHIRStatsOpResponse3.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpResponse).load(params as TFHIRParameters);
end;

{ TFHIRNamingSystem3 }

function TFHIRNamingSystem3.getContext: String;
begin
  result := '';
end;

function TFHIRNamingSystem3.getDate: TFslDateTime;
begin
  result := nm.date;
end;

function TFHIRNamingSystem3.getDescription: String;
begin
  result := nm.description;
end;

function TFHIRNamingSystem3.getName: String;
begin
  result := nm.name;
end;

function TFHIRNamingSystem3.getPublisher: String;
begin
  result := nm.publisher;
end;

function TFHIRNamingSystem3.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[nm.status];
end;

function TFHIRNamingSystem3.getUri: String;
begin
  result := nm.getUri;
end;

function TFHIRNamingSystem3.getURL: String;
begin
  result := '';
end;

function TFHIRNamingSystem3.getVersion: String;
begin
  result := '';
end;

function TFHIRNamingSystem3.hasOid(oid: String): boolean;
begin
  result := nm.hasOid(oid);
end;

function TFHIRNamingSystem3.nm: TFHIRNamingSystem;
begin
  result := (resource as TFHIRNamingSystem);
end;

procedure TFHIRNamingSystem3.setDate(Value: TFslDateTime);
begin
  nm.date := value;
end;

procedure TFHIRNamingSystem3.setDescription(Value: String);
begin
  nm.description := value;
end;

procedure TFHIRNamingSystem3.setName(Value: String);
begin
  nm.name := value;
end;

procedure TFHIRNamingSystem3.setPublisher(Value: String);
begin
  nm.publisher := value;
end;

procedure TFHIRNamingSystem3.setStatus(Value: TPublicationStatus);
begin
  nm.status := MAP_TPublicationStatus[value];
end;

procedure TFHIRNamingSystem3.setUrl(Value: String);
begin
  // nothing
end;

procedure TFHIRNamingSystem3.setVersion(Value: String);
begin
  // nothing
end;

{ TFHIRStructureMap3 }

function TFHIRStructureMap3.url: String;
begin
  result := (Resource as TFHIRStructureMap).url;
end;

{ TFhirPatient3 }

function TFhirPatient3.nameSummary: String;
begin
  result := HumanNamesAsText((resource as TFhirPatient).nameList);
end;

{ TFhirTerminologyCapabilities3 }

procedure TFhirTerminologyCapabilities3.contact(kind: TContactType; value: String);
begin
end;

function TFhirTerminologyCapabilities3.getContext: String;
begin
end;

function TFhirTerminologyCapabilities3.getDate: TFslDateTime;
begin
end;

function TFhirTerminologyCapabilities3.getDescription: String;
begin
end;

function TFhirTerminologyCapabilities3.getName: String;
begin
end;

function TFhirTerminologyCapabilities3.getPublisher: String;
begin
end;

function TFhirTerminologyCapabilities3.getStatus: TPublicationStatus;
begin
  result := psDraft;
end;

function TFhirTerminologyCapabilities3.getURL: String;
begin
end;

function TFhirTerminologyCapabilities3.getVersion: String;
begin
end;

procedure TFhirTerminologyCapabilities3.setContext(Value: String);
begin
  raise Exception.Create('Not done yet');
end;

procedure TFhirTerminologyCapabilities3.setDate(Value: TFslDateTime);
begin
  (FRes as TFhirParameters).AddParameter('date', TFhirDateTime.create(Value));
end;

procedure TFhirTerminologyCapabilities3.setDescription(Value: String);
begin
end;

procedure TFhirTerminologyCapabilities3.setName(Value: String);
begin
end;

procedure TFhirTerminologyCapabilities3.setPublisher(Value: String);
begin
end;

procedure TFhirTerminologyCapabilities3.setStatus(Value: TPublicationStatus);
begin
end;

procedure TFhirTerminologyCapabilities3.setUrl(Value: String);
begin
  (FRes as TFhirParameters).AddParameter('url', TFhirUri.create(Value));
end;

procedure TFhirTerminologyCapabilities3.setVersion(Value: String);
begin
  (FRes as TFhirParameters).AddParameter('version', TFhirCode.create(Value));
end;

procedure TFhirTerminologyCapabilities3.systemUri(url: String);
begin
  (FRes as TFhirParameters).AddParameter('system', TFhirUri.create(url));
end;

procedure TFhirTerminologyCapabilities3.addExpansionParameter(code, doco : String);
begin
  (FRes as TFhirParameters).AddParameter('expansion.parameter', TFhirCode.create(code));
end;

{ TFHIRPeriod3 }

function TFHIRPeriod3.GetEnd: TFslDateTime;
begin
  result := period.end_;
end;

function TFHIRPeriod3.GetStart: TFslDateTime;
begin
  result := period.start;
end;

function TFHIRPeriod3.period: TFHIRPeriod;
begin
  result := Element as TFHIRPeriod;
end;

function TFHIRPeriod3.renderText: String;
begin
  result := gen(element as TFhirPeriod);
end;

procedure TFHIRPeriod3.SetEnd(const Value: TFslDateTime);

begin
  period.end_ := value;
end;

procedure TFHIRPeriod3.SetStart(const Value: TFslDateTime);

begin
  period.start := value;
end;

{ TFHIRConsent3 }

function TFHIRConsent3.consent: TFHIRConsent;
begin
  result := resource as TFHIRConsent;
end;

function TFHIRConsent3.GetActive: boolean;
begin
  result := consent.status = ConsentStateCodesActive;
end;

function TFHIRConsent3.GetDateTime: TFslDateTime;
begin
  result := consent.dateTime;
end;

function TFHIRConsent3.GetPatient: String;
begin
  if consent.patient <> nil then
    result := consent.patient.reference
  else
    result := '';
end;

function TFHIRConsent3.listProvisions: TFslList<TFhirConsentProvisionW>;
begin
  result := nil; // for now
end;

{ TFhirEncounter3 }

function TFhirEncounter3.patientId: String;
begin
  result := (FRes as TFHIREncounter).subject.getId;
end;

function TFhirEncounter3.summary: String;
begin
  result := CODES_TFhirEncounterStatusEnum[(FRes as TFHIREncounter).status];
end;

{ TFhirTestScript3 }

function TFHIRTestScript3.getContext: String;
begin
  result := '';
end;

function TFhirTestScript3.getDate: TFslDateTime;
begin
  result := ts.date;
end;

function TFhirTestScript3.getDescription: String;
begin
  result := ts.description;
end;

function TFhirTestScript3.getName: String;
begin
  result := ts.name;
end;

function TFhirTestScript3.getPublisher: String;
begin
  result := ts.publisher;
end;

function TFhirTestScript3.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[ts.Status];
end;

function TFhirTestScript3.getURL: String;
begin
  result := ts.url;
end;

function TFhirTestScript3.getVersion: String;
begin
  result := ts.version;
end;

procedure TFhirTestScript3.setDate(Value: TFslDateTime);
begin
  ts.date := value;
end;

procedure TFhirTestScript3.setDescription(Value: String);
begin
  ts.description := value;
end;

procedure TFhirTestScript3.setName(Value: String);
begin
  ts.name := value;
end;

procedure TFhirTestScript3.setPublisher(Value: String);
begin
  ts.publisher := value;
end;

procedure TFhirTestScript3.setStatus(Value: TPublicationStatus);
begin
  ts.Status := MAP_TPublicationStatus[Value];
end;

procedure TFhirTestScript3.setUrl(Value: String);
begin
  ts.url := value;
end;

procedure TFhirTestScript3.setVersion(Value: String);
begin
  ts.version := value;
end;

function TFhirTestScript3.ts : TFhirTestScript;
begin
  result := (Fres as TFhirTestScript);
end;

{ TFhirProvenance3 }

procedure TFhirProvenance3.addTarget(url: String);
begin
  p.targetList.Append.reference := url;
end;

procedure TFhirProvenance3.clearSignatures;
begin
  p.signatureList.Clear;
end;

procedure TFhirProvenance3.clearTargets;
begin
  p.targetList.Clear;
end;

function TFhirProvenance3.p: TFhirProvenance;
begin
  result := (Fres as TFhirProvenance);
end;

{ TFhirConceptMapGroupElementDependsOn3 }

function TFhirConceptMapGroupElementDependsOn3.display: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).display;
end;

function TFhirConceptMapGroupElementDependsOn3.property_: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).property_;
end;

function TFhirConceptMapGroupElementDependsOn3.system_: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).system;
end;

function TFhirConceptMapGroupElementDependsOn3.value: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).code;
end;

end.

