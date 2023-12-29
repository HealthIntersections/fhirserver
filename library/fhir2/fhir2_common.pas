unit fhir2_common;

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
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_http, fsl_lang,
  fhir_objects, fhir_common, fhir_extensions,
  fhir2_types, fhir2_operations, fhir2_opbase, fhir_features,
  fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_other;

const
  ExceptionTypeTranslations : array [TFhirIssueType] of TFhirIssueTypeEnum = (IssueTypeNull, IssueTypeInvalid, IssueTypeStructure, IssueTypeRequired, IssueTypeValue,
    IssueTypeInvariant, IssueTypeSecurity, IssueTypeLogin, IssueTypeUnknown, IssueTypeExpired, IssueTypeForbidden, IssueTypeSuppressed, IssueTypeProcessing,
    IssueTypeNotSupported, IssueTypeDuplicate, IssueTypeNotFound, IssueTypeTooLong, IssueTypeCodeInvalid, IssueTypeExtension, IssueTypeTooCostly, IssueTypeBusinessRule,
    IssueTypeConflict, IssueTypeIncomplete, IssueTypeTransient, IssueTypeLockError, IssueTypeNoStore, IssueTypeException, IssueTypeTimeout, IssueTypeThrottled, IssueTypeInformational);

  ISSUE_SEVERITY_MAP : array [TFhirIssueSeverityEnum] of TIssueSeverity = (isNull, isFatal, isError, isWarning, isInformation);
  ISSUE_SEVERITY_MAP2 : array [TIssueSeverity] of TFhirIssueSeverityEnum = (IssueSeverityNull, IssueSeverityFatal, IssueSeverityError, IssueSeverityWarning, IssueSeverityInformation);
  INTERACTION_MAP : array [TFHIRInteraction] of TFhirTypeRestfulInteractionEnum = (TypeRestfulInteractionRead, TypeRestfulInteractionSearchType, TypeRestfulInteractionHistoryType, TypeRestfulInteractionCreate, TypeRestfulInteractionUpdate, TypeRestfulInteractionDelete, TypeRestfulInteractionNull);
  INTERACTION_MAP2 : array [TFHIRInteraction] of TFhirSystemRestfulInteractionEnum = (SystemRestfulInteractionNull, SystemRestfulInteractionSearchSystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem);
  MAP_SearchParamType : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptString, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri);
  MAP_SEARCH_MODE : array [TFhirSearchEntryModeEnum] of TFHIRBundleEntrySearchMode = (smUnknown, smMatch, smInclude, smOutcome);
  MAP_SEARCH_MODE2 : array [TFHIRBundleEntrySearchMode] of TFhirSearchEntryModeEnum = (SearchEntryModeNull, SearchEntryModeMatch, searchEntryModeInclude, searchEntryModeOutcome);
  MAP_ELEMENT_DEFINITION_BINDING : array [TFhirBindingStrengthEnum] of TElementDefinitionBinding = (edbNone, edbRequired, edbExtensible, edbPreferred, edpExample);
  MAP_TFilterOperator : array [TFhirFilterOperatorEnum] of TFilterOperator = (foNull, foEqual, foIsA, foIsNotA, foRegex, foIn, foNotIn);
  MAP_TFilterOperator2 : array [TFilterOperator] of TFHIRFilterOperatorEnum = (filterOperatorNull, filterOperatorEqual, filterOperatorIsA, filterOperatorNull, filterOperatorIsNotA, filterOperatorRegex, filterOperatorIn, filterOperatorNotIn, filterOperatorNull, filterOperatorNull, filterOperatorNull, filterOperatorNull);
  MAP_TFHIRSearchParamType1 : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptNull, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri);
  MAP_TFHIRSearchParamType2 : array [TFhirSearchParamType] of TFHIRSearchParamTypeEnum = (SearchParamTypeNull, SearchParamTypeNumber, SearchParamTypeDate, SearchParamTypeString, SearchParamTypeToken, SearchParamTypeReference, SearchParamTypeComposite, SearchParamTypeQuantity, SearchParamTypeUri, SearchParamTypeNull);
  MAP_TPublicationStatus : array [TPublicationStatus] of TFhirConformanceResourceStatusEnum = (ConformanceResourceStatusNull, ConformanceResourceStatusDraft, ConformanceResourceStatusActive, ConformanceResourceStatusRetired);
  MAP_TPublicationStatusR : array [TFhirConformanceResourceStatusEnum] of TPublicationStatus = (psNull, psDraft, psActive, psRetired);
  MAP_TFHIRConceptEquivalence : array [TFhirConceptMapEquivalenceEnum] of TFHIRConceptEquivalence = (cmeNull, cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact, cmeUnmatched, cmeDisjoint);
  MAP_TFHIRConceptEquivalenceR : array [TFHIRConceptEquivalence] of TFHIRConceptMapEquivalenceEnum = (ConceptMapEquivalenceNull, ConceptMapEquivalenceNull, ConceptMapEquivalenceEquivalent, ConceptMapEquivalenceEqual, ConceptMapEquivalenceWider, ConceptMapEquivalenceSubsumes, ConceptMapEquivalenceNarrower, ConceptMapEquivalenceSpecializes, ConceptMapEquivalenceInexact, ConceptMapEquivalenceUnmatched, ConceptMapEquivalenceDisjoint);
  MAP_TContactType : array [TContactType] of TFhirContactPointSystemEnum = (ContactPointSystemNull, ContactPointSystemPhone, ContactPointSystemFax, ContactPointSystemEmail, ContactPointSystemPager, ContactPointSystemOther, ContactPointSystemOther, ContactPointSystemOther);
  MAP_TContactType2 : array [TFhirContactPointSystemEnum] of TContactType = (cpsNull, cpsPhone, cpsFax, cpsEmail, cpsPager, cpsOther);
  MAP_TSubscriptionMethod : array [TFhirSubscriptionChannelTypeEnum] of TSubscriptionMethod = (smNull, smRestHook, smEmail, smSms, smWebsocket, smChangeScript);
  MAP_TSubscriptionMethod2 : array [TSubscriptionMethod] of TFhirSubscriptionChannelTypeEnum = (SubscriptionChannelTypeNull, SubscriptionChannelTypeRestHook, SubscriptionChannelTypeWebsocket, SubscriptionChannelTypeEmail, SubscriptionChannelTypeSms, SubscriptionChannelTypeMessage);
  MAP_TSubscriptionStatus : array [TFhirSubscriptionStatusEnum] of TSubscriptionStatus = (ssNull, ssRequested, ssActive, ssError, ssOff);
  MAP_TSubscriptionStatus2 : array [TSubscriptionStatus] of TFhirSubscriptionStatusEnum = (SubscriptionStatusNull, SubscriptionStatusRequested, SubscriptionStatusActive, SubscriptionStatusError, SubscriptionStatusOff, SubscriptionStatusNull);
  BUNDLE_TYPE_TITLE : Array[TFhirBundleTypeEnum] of String = ('', 'Document', 'Message', 'Transaction', 'Transaction Response', 'Batch', 'Batch Response', 'History Record', 'Search Results', 'Resource Collection');
  MAP_TFHIRBundleType  : array [TBundleType] of TFhirBundleTypeEnum = (BundleTypeNull, BundleTypeDocument, BundleTypeMessage, BundleTypeTransaction, BundleTypeTransactionResponse, BundleTypeBatch, BundleTypeBatchResponse, BundleTypeHistory, BundleTypeSearchset, BundleTypeCollection);
  MAP_TFHIRBundleTypeR : array [TFhirBundleTypeEnum] of TBundleType = (btNull, btDocument, btMessage, btTransaction, btTransactionResponse, btBatch, btBatchResponse, btHistory, btSearchset, btCollection);
  MAP_TObservationStatus : array [TObservationStatus] of TFhirObservationStatusEnum = (ObservationStatusNull, ObservationStatusRegistered, ObservationStatusPreliminary, ObservationStatusFinal, ObservationStatusAmended, ObservationStatusNull, ObservationStatusCancelled, ObservationStatusEnteredInError, ObservationStatusUnknown);
  MAP_TObservationStatus2 : array [TFhirObservationStatusEnum] of TObservationStatus = (obssNull, obssRegistered, obssPreliminary, obssFinal, obssAmended, obssCancelled, obssEnteredInError, obssUnknown);


type

  { TFHIRPrimitive2 }

  TFHIRPrimitive2 = class (TFHIRPrimitiveW)
  public
    function GetAsString : String; override;
    procedure SetAsString(value : String); override;
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
  end;

  { TFHIRExtension2 }

  TFHIRExtension2 = class (TFHIRExtensionW)
  private
    function ext : TFHIRExtension;
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
  public
    function url : String; override;
    function value : TFHIRObject; override;
    function renderText : String; override;
    function valueAsCodeableConcept : TFhirCodeableConceptW; override;
    function valueAsCoding : TFhirCodingW; override;
    function valueAsPeriod : TFhirPeriodW; override;
    function valueAsQuantity : TFhirQuantityW; override;
    function valueAsIdentifier : TFhirIdentifierW; override;
    function valueAsAttachment : TFhirAttachmentW; override;
    function valueAsString : string; override;
    procedure setValueW(value : TFhirDataTypeW); override;
    procedure setValueV(value : TFhirObject); override;
  end;

  { TFHIRCoding2 }

  TFHIRCoding2 = class (TFHIRCodingW)
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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

  { TFhirCodeableConcept2 }

  TFhirCodeableConcept2 = class (TFhirCodeableConceptW)
  protected
    function GetText: String; override;
    procedure SetText(const Value: String); override;
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
  public
    function codingCount : integer; override;
    function codings : TFslList<TFhirCodingW>; override;
    procedure addCoding(coding : TFHIRCodingW); override;
    function addCoding : TFHIRCodingW; override;
    procedure removeCoding(systemUri, version, code : String); override;
    function summary : String; override;
    function fromSystem(System : String; required : boolean = false) : String; overload; override;
    function fromSystem(Systems : TArray<String>; required : boolean = false) : String; overload; override;
    function renderText : String; override;
    function hasCode(systemUri, code : String) : boolean; override;
    function hasCode(systemUri, version, code : String) : boolean; override;
    procedure clearCodings; override;
    procedure addCoding(systemUri, version, code, display : String); overload; override;

  end;

  { TFhirIdentifier2 }

  TFhirIdentifier2 = class (TFhirIdentifierW)
  private
    function id : TFHIRIdentifier;
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetSystem: String; override;
    function GetUse: TIdentifierUse; override;
    function GetValue: String; override;
    procedure SetSystem(const Value: String); override;
    procedure SetUse(const Value: TIdentifierUse); override;
    procedure SetValue(const Value: String); override;
    function GetTypeV: TFhirCodeableConceptW; override;
    procedure SetTypeV(const Value: TFhirCodeableConceptW); override;
  public
    function renderText : String; override;
  end;

  { TFhirOperationOutcome2 }

  TFhirOperationOutcome2 = class (TFhirOperationOutcomeW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function hasText : boolean; override;
    function text : String; override;
    function code : TFhirIssueType; override;
    procedure addIssue(issue : TFhirOperationOutcomeIssueW; owns : boolean); override;
    procedure addIssue(level : TIssueSeverity; cause : TFHIRIssueType; path, message : String; code : TOpIssueCode; addIfDuplicate : boolean); override;
    function hasIssues : boolean; override;
    function issues : TFslList<TFhirOperationOutcomeIssueW>; override;
    function rule(level : TIssueSeverity; source : String; typeCode : TFhirIssueType; path : string; test : boolean; msg : string) : boolean; override;
    function severity : TIssueSeverity; override;
    function issueCount : integer; override;
    function hasErrors : boolean; override;
  end;

  { TFHIRBundleEntry2 }

  TFHIRBundleEntry2 = class (TFHIRBundleEntryW)
  private
    function entry : TFhirBundleEntry;
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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


  { TFHIRBundle2 }

  TFHIRBundle2 = class (TFHIRBundleW)
  private
    function bundle : TFhirBundle;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function next(bnd : TFHIRResourceV) : String; overload; override;
    procedure addEntries(bnd : TFHIRResourceV); override;
    procedure addEntry(bnd : TFhirBundleEntryW; first : boolean); overload; override;
    procedure addEntry(url : String; bnd : TFhirResourceV); overload; override;
    function addEntry : TFhirBundleEntryW; overload; override;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; override;
    function count(rtype : String = '') : Integer; override;
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

  { TFHIROperationOutcomeIssue2 }

  TFHIROperationOutcomeIssue2 = class (TFHIROperationOutcomeIssueW)
  private
    function issue : TFHIROperationOutcomeIssue;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function display : String; override;
    function severity : TIssueSeverity; override;
    function getDiagnostics: String; override;
    procedure setDiagnostics(Value: String); override;
  end;

  { TFHIRSearchParamDefinition2 }

  TFHIRSearchParamDefinition2 = class (TFHIRSearchParamDefinitionW)
  private
    function param : TFhirConformanceRestResourceSearchParam;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function name : String; override;
    function documentation : String; override;
    function type_ : TFHIRSearchParamType; override;
  end;

  { TFhirCapabilityStatementRestResource2 }

  TFhirCapabilityStatementRestResource2 = class (TFhirCapabilityStatementRestResourceW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getCode: String; override;
    procedure setCode(Value: String); override;
    function getProfile: String; override;
    procedure setProfile(Value: String); override;
    procedure addInteraction(code : String);  override;
    function getReadHistory: boolean; override;
    procedure setReadHistory(Value: boolean); override;
    function hasInteraction : boolean; override;
    procedure addParam(html, n, url, d : String; t : TFHIRSearchParamType; tgts : Array of String); override;
  end;

  { TFHIRCapabilityStatement2 }

  TFHIRCapabilityStatement2 = class (TFHIRCapabilityStatementW)
  private
    function statement : TFhirConformance;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function hasRest : boolean; override;
    function hasSecurity(system, code : String) : boolean; override;

    procedure readSmartExtension(var authorize, token, register: String); override;
    procedure addSmartExtensions(authorize, token, register, manage: String; caps : Array of String); override;
    function hasFormat(fmt : String) : boolean; override;

    procedure contact(kind : TContactType; value : String); override;
    procedure software(name, version, release : String); override;
    procedure impl(url, desc : String); override;
    procedure fmt(mt : String); override;

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

    procedure standardServer(ts, ws, pv, cv, iv : String; transactions, search, history : boolean); override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
    function addResource(code : String) : TFhirCapabilityStatementRestResourceW; override;
    procedure addOperation(name, url : String); override;

    function supportsType(name : String; interaction : TFHIRInteraction) : boolean; override;
    procedure listTypes(interactions : TFHIRInteractions; names : TStrings); override;
    procedure listSearchParams(name : String; list : TFslList<TFHIRSearchParamDefinitionW>); override;
    procedure addInstantiates(url : String); override;
  end;

  { TFhirElementDefinition2 }

  TFhirElementDefinition2 = class (TFhirElementDefinitionW)
  private
    function edefn : TFhirElementDefinition;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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

  { TFHIRStructureDefinition2 }

  TFHIRStructureDefinition2 = class (TFhirStructureDefinitionW)
  private
    function sd : TFhirStructureDefinition;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function kind : TStructureDefinitionKind; override;
    function name : String; override;
    function url : String; override;
    function type_ : String; override;
    function elements : TFslList<TFHIRElementDefinitionW>; override;
    function getDefinition(id : String; source : TElementDefinitionSourceOption) : TFHIRElementDefinitionW; override;
  end;

  { TFhirParametersParameter2 }

  TFhirParametersParameter2 = class (TFhirParametersParameterW)
  private
    function parameter : TFhirParametersParameter;
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getValue: TFHIRObject; override;
    procedure setValue(Value: TFHIRObject); override;
    procedure populateList; override;
    function getParameterParameter(name: String): TFhirParametersParameterW; override;
    function getResourceParameter(name: String): TFHIRResourceV; override;
    function getStringParameter(name: String): String; override;
  public
    function name : String; override;
    function hasValue : boolean; override;
    property value : TFHIRObject read GetValue write SetValue;
    function valueString : String; override;
    function getResource: TFHIRResourceV; override;
    procedure setResource(Value: TFHIRResourceV); override;
    function hasResource : boolean; override;
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamStr(name : String; value : string); override;
    procedure addParamCode(name : String; value : string); override;
    procedure addParamUri(name : String; value : string); override;
    procedure addParamCanonical(name : String; value : string); override;
    procedure addParam(name : String; value : TFHIRObject); override;
    function addParam(name : String) : TFhirParametersParameterW; override;
  end;

  { TFHIRParameters2 }

  TFHIRParameters2 = class (TFHIRParametersW)
  private
    function parameter : TFhirParameters;
  protected
    procedure populateList; override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamStr(name : String; value : string); override;
    procedure addParamCode(name : String; value : string); override;
    procedure addParamUri(name : String; value : string); override;
    procedure addParamCanonical(name : String; value : string); override;
    procedure addParam(name : String; value : TFHIRObject); override;
    function addParam(name : String) : TFhirParametersParameterW; override;
    function bool(name : String) : boolean; override;
    function str(name : String) : String; override;
    function has(name : String) : boolean; override;
    function obj(name : String) : TFHIRObject; override;
    function getParameter(name: String): TFhirParametersParameterW;  override;
    function names : String; override;
  end;

  { TFhirValueSetExpansionContains2 }

  TFhirValueSetExpansionContains2 = class (TFhirValueSetExpansionContainsW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getSystem : String; override;
    function getCode : String; override;
    function getDisplay : String; override;
    procedure setCode(Value: String); override;
    procedure setDisplay(Value: String); override;
    procedure setSystem(Value: String); override;
    function GetItemWeight : String; override;
    procedure SetItemWeight(Value: String); override;
    function GetAbstract : boolean; override;
    function GetInactive : boolean; override;
    procedure SetAbstract(Value: boolean); override;
    procedure SetInactive(Value: boolean); override;
    function getVersion : String; override;
    procedure setVersion(Value: String); override;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; override;
    procedure addDesignation(lang, use, value : String); override;
    procedure addDesignation(lang : TIETFLang; use : TFHIRCodingW; value : TFHIRPrimitiveW; extensions : TFslList<TFHIRExtensionW>); override;
    procedure addProperty(code : String; value : TFHIRObject); override; overload;
    procedure addProperty(code : String; prop : TFhirCodeSystemConceptPropertyW); override; overload;
    procedure addContains(contained : TFhirValueSetExpansionContainsW); override;
    procedure clearContains(); override;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; override;
  end;

  { TFhirValueSetExpansion2 }

  TFhirValueSetExpansion2 = class (TFhirValueSetExpansionW)
  private
    function exp : TFhirValueSetExpansion;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    procedure addParamStr(name, value : String); override;
    procedure addParamCode(name, value : String); override;
    procedure addParamUri(name, value : String); override;
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamInt(name : String; value : integer); override;
    function hasParam(name : string) : boolean; overload; override;
    function hasParam(name, value : string) : boolean; overload; override;
    procedure copyParams(source : TFhirValueSetExpansionW); override;
    procedure addContains(item : TFhirValueSetExpansionContainsW); overload; override;
    function addContains : TFhirValueSetExpansionContainsW; overload; override;
    function makeContains : TFhirValueSetExpansionContainsW; overload; override;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; override;
    function getTotal : integer; override;
    procedure setTotal(value : integer) ; override;
    function getOffset : integer; override;
    procedure setOffset(value : integer) ; override;
    procedure defineProperty(focus : TFhirValueSetExpansionContainsW; url, code : String; value : TFHIRObject); override;
  end;

  { TFhirValueSetComposeIncludeFilter2 }

  TFhirValueSetComposeIncludeFilter2 = class (TFhirValueSetComposeIncludeFilterW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getProp : String; override;
    function getOp : TFilterOperator; override;
    function getValue : String; override;
    procedure setOp(Value: TFilterOperator); override;
    procedure setProp(Value: String); override;
    procedure setValue(Value: String); override;
  end;

  { TFhirValueSetComposeIncludeConcept2 }

  TFhirValueSetComposeIncludeConcept2 = class (TFhirValueSetComposeIncludeConceptW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getCode : String; override;
    function getDisplay : String; override;
    procedure setCode(Value: String); override;
    procedure setDisplay(Value: String); override;
    function displayElement : TFHIRPrimitiveW; override;
    function GetItemWeight : String; override;
    procedure SetItemWeight(Value: String); override;
    function designations : TFslList<TFhirValueSetComposeIncludeConceptDesignationW>; override;
  end;

  { TFhirValueSetComposeInclude2 }

  TFhirValueSetComposeInclude2 = class (TFhirValueSetComposeIncludeW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getSystem : String; override;
    function getVersion : String; override;
    function valueSets : TArray<String>; override;
    function hasConcepts : boolean; override;
    function concepts : TFslList<TFhirValueSetComposeIncludeConceptW>; override;
    function hasFilters : boolean; override;
    function hasValueSets : boolean; override;
    function filterCount : integer; override;
    function conceptCount : integer; override;
    function filters : TFslList<TFhirValueSetComposeIncludeFilterW>; override;
    procedure setSystem(Value: String); override;
    procedure setVersion(Value: String); override;
    function addConcept : TFhirValueSetComposeIncludeConceptW; override;
    function addFilter : TFhirValueSetComposeIncludeFilterW; override;
  end;

  { TFhirCodeSystemConceptDesignation2 }

  TFhirCodeSystemConceptDesignation2 = class (TFhirCodeSystemConceptDesignationW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function language : String; override;
    function useGen : String; override;
    function use : TFHIRCodingW; override;
    function value : String; override;
    function valueElement : TFHIRPrimitiveW; override;
  end;

  { TFhirCodeSystemConcept2 }

  TFhirCodeSystemConcept2 = class (TFhirCodeSystemConceptW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : String; override;
    function display : String; override;
    function displayElement : TFHIRPrimitiveW; override;
    function definition : String; override;
    function conceptList : TFhirCodeSystemConceptListW; override;
    function conceptCount : integer; override;
    function designationCount : integer; override;
    function designations : TFslList<TFhirCodeSystemConceptDesignationW>; override;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; override;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; override;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; override;
    function displayTag(tag : String) : String; override;
    procedure setDisplayTag(tag, value : String); override;
    function getCode(code : String) : TFhirCodeSystemConceptW; override;
    function itemWeight : String; override;
  end;

  { TFHIRValueSetCodeSystem2 }

  TFHIRValueSetCodeSystem2 = class (TFHIRValueSetCodeSystemW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function systemUri : String; override;
    function concepts : TFhirCodeSystemConceptListW; override;
//    function version : String; override;
  end;

  { TFhirCodeSystem2 }

  TFhirCodeSystem2 = class (TFhirCodeSystemW)
  private
    function vs : TFhirValueSet;
    function cs : TFHIRValueSetCodeSystem;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function getName : String; override;
    function getURL : String; override;
    function getVersion : String; override;
    function getDescription : String; override;
    function copyright : String; override;
    function language : String; override;

    function properties : TFslList<TFhirCodeSystemPropertyW>;  override;
    // this is special because it's owned
    function conceptList : TFhirCodeSystemConceptListW; override;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; override;
    function conceptCount : integer; override;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; override;

    function isInactive(c : TFhirCodeSystemConceptW) : boolean; override;    
    function codeStatus(c : TFhirCodeSystemConceptW) : String; override;
    function isAbstract(c : TFhirCodeSystemConceptW) : boolean; override;
    function isDeprecated(c : TFhirCodeSystemConceptW) : boolean; override;
    function getParents(c : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptListW; override;
    function getChildren(c : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptListW; override;
    function getCode(code : String) : TFhirCodeSystemConceptW; override;
    function buildImplicitValueSet : TFHIRValueSetW; override;

    function getDate: TFslDateTime; override;
    function getStatus: TPublicationStatus; override;
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
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function valueSet : String; override;
    function supplements : String; override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFHIRValueSet2 }

  TFHIRValueSet2 = class (TFHIRValueSetW)
  private
    FExp : TFhirValueSetExpansionW;
    function vs : TFhirValueSet;
  public
    destructor Destroy; override;

    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function getName : String; override;
    function getURL : String; override;
    function checkCompose(place, role : String) : boolean; override;
    function getComposeExtensions : TFslList<TFHIRExtensionW>; override;
    function checkExpansion(place, role : String) : boolean; override;
    function imports : TArray<String>; override;
    function inlineCS : TFHIRValueSetCodeSystemW; override;
    function includes : TFslList<TFhirValueSetComposeIncludeW>; override;
    function excludes : TFslList<TFhirValueSetComposeIncludeW>; override;
    procedure clearDefinition; override;
    procedure clearDefinitionExtensions(exemptUrls : TStringArray); override;
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
    function excludeInactives : boolean; override;
    function addInclude : TFhirValueSetComposeIncludeW; override;
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function source : String; override;
    function findContains(systemUri, version, code : String) : TFhirValueSetExpansionContainsW; override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFHIRLookupOpRequest2 }

  TFHIRLookupOpRequest2 = class (TFHIRLookupOpRequestW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    procedure loadCoding; override;
    function coding : TFHIRCodingW; override;
    function propList : TArray<String>; override;
    function displayLanguage : String; override;
  end;

  TFHIRLookupOpRespDesignation2 = class (TFHIRLookupOpRespDesignationW)
  public
    function getUse: TFHIRObject; override;
    procedure setUse(Value: TFHIRObject); override;
  end;

  TFHIRLookupOpResponse2 = class (TFHIRLookupOpResponseW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    function addProp(name : string) : TFHIRLookupOpRespPropertyW; override;
    function addDesignation(lang, system, code, display, value : string) : TFHIRLookupOpRespDesignationW; overload; override;
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

  { TFhirConceptMapGroupElementDependsOn2 }

  TFhirConceptMapGroupElementDependsOn2 = class (TFhirConceptMapGroupElementDependsOnW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function property_ : String; override;
    function system_ : String; override;
    function value : String; override;
    function display : String; override;
  end;

  { TFhirConceptMapGroupElementTarget2 }

  TFhirConceptMapGroupElementTarget2 = class (TFhirConceptMapGroupElementTargetW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code: String; override;
    function equivalence : TFHIRConceptEquivalence; override;
    function comments : String; override;
    function products : TFslList<TFhirConceptMapGroupElementDependsOnW>; override;
  end;

  { TFhirConceptMapGroupElement2 }

  TFhirConceptMapGroupElement2 = class (TFhirConceptMapGroupElementW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code: String; override;
    function targets : TFslList<TFhirConceptMapGroupElementTargetW>; override;
    function targetCount : integer; override;
    function addTarget(code : String; eq : TFHIRConceptEquivalence) : TFhirConceptMapGroupElementTargetW; override;
  end;

  { TFhirConceptMapGroup2 }

  TFhirConceptMapGroup2 = class (TFhirConceptMapGroupW)
  private
    Fsource, Ftarget : String;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(res : TFhirResource; source, target : String);
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function elements : TFslList<TFhirConceptMapGroupElementW>; override;
    function addElement(code : String) : TFhirConceptMapGroupElementW; override;
    function source : String; override;
    function target : String; override;
  end;

  { TFhirConceptMap2 }

  TFhirConceptMap2 = class (TFhirConceptMapW)
  private
    function cm : TFhirConceptMap;
  protected
    function getVersion: String; override;
    procedure setVersion(Value: String); override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
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
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFHIRMeta2 }

  TFHIRMeta2 = class (TFHIRMetaW)
  private
    FResource : TFHIRResource;
    procedure force;
    procedure setResource(value : TFHIRResource);
    function m : TFhirMeta;
  protected
    function NoElementOk : boolean; override;
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    destructor Destroy; override;
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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

  { TFHIRAuditEvent2 }

  TFHIRAuditEvent2 = class (TFhirAuditEventW)
  private
    function ae : TFHIRAuditEvent;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    procedure success; override;
    procedure eventType(system, code, display : String); override;
    procedure eventSubType(system, code, display : String); override;
    procedure source(name, system, value : String); override;
    procedure sourceType(system, code, display : String); override;
    procedure participantId(system, value, alt, name : String); override;
    procedure participantIp(ip : String); override;
    function dateTime : TFslDateTime; override;
  end;

  { TFHIRSubscription2 }

  TFHIRSubscription2 = class (TFHIRSubscriptionW)
  private
    function sub : TFhirSubscription;
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
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

  { TFhirObservationComponent2 }

  TFhirObservationComponent2 = class (TFhirObservationComponentW)
  private
    function comp : TFhirObservationComponent;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getValue: TFHIRObject; override;
    procedure setValue(Value: TFHIRObject); override;
    function codings : TFslList<TFHIRCodingW>; override;
    function valueW : TFHIRXVersionElementWrapper; override;
    function valueString : String; override;
    function dataAbsentReason : TFhirCodeableConceptW; override;
  end;

  { TFhirObservation2 }

  TFhirObservation2 = class (TFhirObservationW)
  private
    function obs : TFHIRObservation;
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
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

  { TFHIRQuantity2 }

  TFHIRQuantity2 = class (TFHIRQuantityW)
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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function asDuration : TDateTime; override;
    function renderText : String; override;
  end;

  { TFHIRPeriod2 }

  TFHIRPeriod2 = class (TFHIRPeriodW)
  private
    function period : TFHIRPeriod;
  protected
    function GetEnd: TFslDateTime; override;
    function GetStart: TFslDateTime; override;
    procedure SetEnd(const Value: TFslDateTime); override;
    procedure SetStart(const Value: TFslDateTime); override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function renderText : String; override;
  end;

  { TFHIRAttachment2 }

  TFHIRAttachment2 = class (TFHIRAttachmentW)
  private
    function att : TFhirAttachment;
  protected
    function GetContentType: String; override;
    function GetData: TBytes; override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function renderText : String; override;
  end;

  { TFHIRGroupCharacteristic2 }

  TFHIRGroupCharacteristic2 = class (TFHIRGroupCharacteristicW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : TFhirCodeableConceptW; override;
    function value : TFhirCodeableConceptW; override;
  end;

  { TFHIRGroup2 }

  TFHIRGroup2 = class (TFHIRGroupW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function name : String; override;
    function hasMembers : boolean; override;
    function hasCharacteristics : boolean; override;
    function characteristics : TFslList<TFHIRGroupCharacteristicW>; override;
  end;

  { TFhirPatient2 }

  TFhirPatient2 = class (TFhirPatientW)
  protected
    function GetActive: boolean; override;
    procedure SetActive(const Value: boolean); override;
    function pat : TFHIRPatient;
    function GetFamily: String; override;
    procedure SetFamily(const Value: String); override;
    function GetDob: String; override;
    procedure SetDob(const Value: String); override;
    function GetIdentifier(systemUri: String): String; override;
    procedure SetIdentifier(systemUri: String; const Value: String); override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    procedure addGiven(name : String); override;
    function nameSummary : String; override;
    function activeStr : String; override;
    function gender : String; override;
    function genderPlus : String; override;
    function identifierSummary : String; override;
    function contactSummary : String; override;
  end;

  { TFhirEncounter2 }

  TFhirEncounter2 = class (TFhirEncounterW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function summary : String; override;
    function patientId : String; override;
  end;

  { TFhirBinary2 }

  TFhirBinary2 = class (TFhirBinaryW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function ContentType : String; override;
    function content : TBytes; override;
  end;

  { TFHIRNamingSystem2 }

  TFHIRNamingSystem2 = class (TFHIRNamingSystemW)
  private
    function nm : TFHIRNamingSystem;
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  public
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function getUri : String; override;
    function hasOid(oid : String) : boolean; override;
  end;

  { TFhirTerminologyCapabilities2 }

  TFhirTerminologyCapabilities2 = class (TFhirTerminologyCapabilitiesW)
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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    procedure contact(kind : TContactType; value : String); override;
    procedure systemUri(url : String); override;
    procedure addExpansionParameter(code, doco : String); override;
  end;

  { TFHIRTestScript2 }

  TFHIRTestScript2 = class (TFHIRTestScriptW)
  private
    function ts : TFHIRTestScript;
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
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
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFhirProvenance2 }

  TFhirProvenance2 = class (TFhirProvenanceW)
  private
    function p : TFhirProvenance;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    procedure clearTargets; override;
    procedure clearSignatures; override;
    procedure addTarget(url : String); override;
  end;

  { TFhirImmunization2 }

  TFhirImmunization2 = class (TFhirImmunizationW)
  private
    function imm : TFhirImmunization;
  protected
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLotNumber: String; override;
    function GetPatient: String; override;
    function GetPerformerDisplay: String; override;
    function GetStatus: string; override;
    procedure SetLotNumber(const Value: String); override;
    procedure SetPatient(const Value: String); override;
    procedure SetPerformerDisplay(const Value: String); override;
    procedure SetStatus(const Value: string); override;
    function GetManufacturerIdSystem: String; override;
    function GetManufacturerIdValue: String; override;
    procedure SetManufacturerIdSystem(const Value: String); override;
    procedure SetManufacturerIdValue(const Value: String); override;
    function GetDate: TFslDateTime; override;
    procedure SetDate(const Value: TFslDateTime); override;
  public
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function code(systemUri : String) : String; override;
    function hasCode(systemUri, code : String) : boolean; override;
    procedure setCodeBySystem(systemUri : String; code : String); override;
  end;


implementation

uses
  fhir2_utilities;

{ TFHIRPrimitive2 }

function TFHIRPrimitive2.GetAsString: String;
begin
  result := (FElement as TFHIRPrimitiveType).StringValue;
end;

procedure TFHIRPrimitive2.SetAsString(value: String);
begin
  (FElement as TFHIRPrimitiveType).StringValue := value;
end;

function TFHIRPrimitive2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

{ TFhirOperationOutcome2 }

procedure TFhirOperationOutcome2.addIssue(issue: TFhirOperationOutcomeIssueW; owns : boolean);
begin
  (Fres as TFhirOperationOutcome).issueList.Add((issue.Element as TFhirOperationOutcomeIssue).link);
  if owns then
    issue.free;
end;

procedure TFhirOperationOutcome2.addIssue(level: TIssueSeverity; cause: TFHIRIssueType; path, message : String; code : TOpIssueCode; addIfDuplicate : boolean);
var
  iss : TFhirOperationOutcomeIssue;
begin
  if not addIfDuplicate then
  begin
    for iss in (Fres as TFhirOperationOutcome).issueList do
      if (iss.details <> nil) and (iss.details.text = message) then
        exit();
  end;

  iss := (Fres as TFhirOperationOutcome).issueList.Append;
  iss.code:= ExceptionTypeTranslations[cause];
  iss.severity := ISSUE_SEVERITY_MAP2[level];
  iss.details := TFHIRCodeableConcept.Create;
  if (code <> oicVoid) then
    iss.details.addCoding('http://hl7.org/fhir/tools/CodeSystem/tx-issue-type', '', CODES_TOpIssueCode[code], '');
  iss.details.text := message;
  iss.locationList.Add(path);
end;

function TFhirOperationOutcome2.code: TFhirIssueType;
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

function TFhirOperationOutcome2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirOperationOutcome2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirOperationOutcome2.hasErrors: boolean;
begin
  result := (Fres as TFhirOperationOutcome).hasErrors;
end;

function TFhirOperationOutcome2.hasIssues: boolean;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  result := not op.issueList.IsEmpty;
end;

function TFhirOperationOutcome2.hasText: boolean;
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

function TFhirOperationOutcome2.issueCount: integer;
begin
  result := (resource as TFhirOperationOutcome).issueList.Count;
end;

function TFhirOperationOutcome2.issues: TFslList<TFhirOperationOutcomeIssueW>;
var
  iss : TFhirOperationOutcomeIssue;
begin
  result := TFslList<TFhirOperationOutcomeIssueW>.Create;
  for iss in (resource as TFhirOperationOutcome).issueList do
    result.Add(TFHIROperationOutcomeIssue2.Create(iss.Link));
end;

function TFhirOperationOutcome2.rule(level: TIssueSeverity; source: String; typeCode: TFhirIssueType; path: string; test: boolean; msg: string): boolean;
begin
  result := (resource as TFhirOperationOutcome).rule(ISSUE_SEVERITY_MAP2[level], source, ExceptionTypeTranslations[typeCode], path, test, msg);
end;

procedure TFhirOperationOutcome2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFhirOperationOutcome2.severity: TIssueSeverity;
begin
  if (resource as TFhirOperationOutcome).issueList.Count > 0 then
    result := ISSUE_SEVERITY_MAP[(resource as TFhirOperationOutcome).issueList[0].severity]
  else
    result := isFatal;
end;

function TFhirOperationOutcome2.text: String;
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

{ TFHIRBundle2 }

procedure TFHIRBundle2.addEntries(bnd: TFHIRResourceV);
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  bundle.entryList.AddAll(b.entryList);
end;

procedure TFHIRBundle2.addEntry(bnd: TFhirBundleEntryW; first: boolean);
begin
  if first then
    bundle.entryList.InsertItem(0, bnd.element.link as TFHIRBundleEntry)
  else
    bundle.entryList.AddItem(bnd.element.link as TFHIRBundleEntry);
end;

procedure TFHIRBundle2.addEntry(url: String; bnd: TFhirResourceV);
var
  e : TFHIRBundleEntry;
begin
  e := bundle.entryList.Append;
  e.fullUrl := url;
  e.resource := bnd as TFHIRResource;
end;

function TFHIRBundle2.addEntry: TFhirBundleEntryW;
begin
  result := TFhirBundleEntry2.Create(bundle.entryList.append.link);
end;

function TFHIRBundle2.bundle: TFhirBundle;
begin
  result := resource as TFHIRBundle;
end;

function TFHIRBundle2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

procedure TFHIRBundle2.clearLinks;
begin
  bundle.link_List.Clear;
end;

function TFHIRBundle2.count(rtype: String): Integer;
var
  be : TFhirBundleEntry;
begin
  result := 0;
  for be in bundle.entryList do
    if (be.resource <> nil) and ((rtype = '') or (rtype = be.resource.fhirType)) then
      inc(result);
end;

function TFHIRBundle2.entries: TFslList<TFhirBundleEntryW>;
var
  be : TFHIRBundleEntry;
begin
  result := TFslList<TFhirBundleEntryW>.Create;
  try
    for be in bundle.entryList do
      result.Add(TFhirBundleEntry2.Create(be.Link));
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRBundle2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRBundle2.getLastUpdated: TFslDateTime;
begin
  if bundle.meta <> nil then
    result := TFslDateTime.makeNull
  else
    result := bundle.meta.lastUpdated;
end;

function TFHIRBundle2.getLink(rel: String): String;
begin
  result := bundle.Links[rel];
end;

procedure TFHIRBundle2.listLinks(links: TFslStringDictionary);
var
  bl : TFhirBundleLink;
begin
  links.Clear;
  for bl in bundle.link_List do
    links.AddOrSetValue(bl.relation, bl.url);
end;

function TFHIRBundle2.moveToFirst(res: TFhirResourceV): TFhirBundleEntryW;
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
  result := TFHIRBundleEntry2.Create(bundle.entryList[0].Link);
end;

function TFHIRBundle2.next(bnd: TFHIRResourceV): String;
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  result := b.Links['next'];
end;

procedure TFHIRBundle2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRBundle2.setLastUpdated(Value: TFslDateTime);
begin
  if bundle.meta = nil then
    bundle.meta := TFHIRMeta.Create;
  bundle.meta.lastUpdated := value;
end;

procedure TFHIRBundle2.setLink(rel: String; const Value: String);
begin
  bundle.Links[rel] := value;
end;

procedure TFHIRBundle2.setTimestamp(Value: TFslDateTime);
begin
end;

procedure TFHIRBundle2.setTotal(Value: integer);
begin
  bundle.total := inttostr(value);
end;

procedure TFHIRBundle2.setType(value: TBundleType);
begin
  bundle.type_ := MAP_TFHIRBundleType[value];
end;

function TFHIRBundle2.title: String;
begin
  result := BUNDLE_TYPE_TITLE[bundle.type_];
end;

function TFHIRBundle2.getTimestamp: TFslDateTime;
begin
  result := TFslDateTime.makeUTC;
end;

function TFHIRBundle2.getTotal: integer;
begin
  result := StrToIntDef(bundle.total, -1);
end;

function TFHIRBundle2.getType: TBundleType;
begin
  result := MAP_TFHIRBundleTypeR[bundle.type_];
end;

{ TFHIROperationOutcomeIssue2 }

function TFHIROperationOutcomeIssue2.display: String;
var
  i : TFHIROperationOutcomeIssue;
begin
  i := issue;
  result := i.diagnostics;
  if (i.details <> nil) and (i.details.text <> '') then
    result := i.details.text;
end;

function TFHIROperationOutcomeIssue2.issue: TFHIROperationOutcomeIssue;
begin
  result := Element as TFHIROperationOutcomeIssue;
end;

function TFHIROperationOutcomeIssue2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIROperationOutcomeIssue2.severity: TIssueSeverity;
begin
  result := ISSUE_SEVERITY_MAP[issue.severity];
end;

function TFHIROperationOutcomeIssue2.getDiagnostics: String;
begin
  result := issue.diagnostics;
end;

procedure TFHIROperationOutcomeIssue2.setDiagnostics(Value: String);
begin
  issue.diagnostics := value;
end;


{ TFHIRCapabilityStatement2 }

procedure TFHIRCapabilityStatement2.addInstantiates(url: String);
begin
  // ignored here
end;

procedure TFHIRCapabilityStatement2.addOperation(name, url: String);
var
  t : TFhirConformanceRestOperation;
begin
  t := statement.restList[0].operationList.append;
  t.name := name;
  t.definition := TFHIRReference.Create(url);
end;

function TFHIRCapabilityStatement2.addResource(code: String): TFhirCapabilityStatementRestResourceW;
begin
  result := TFhirCapabilityStatementRestResource2.Create(statement.restList[0].resourceList.append.link);
  result.code := code;
end;

procedure TFHIRCapabilityStatement2.addSmartExtensions(authorize, token,
  register, manage: String; caps: array of String);
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
    ext.addExtension('register', register);
    ext.addExtensionUri('authorize', authorize);
    ext.addExtensionUri('token', token);
    ext.addExtensionUri('manage', manage);

    for s in caps do
      statement.restList[0].security.addExtension('http://fhir-registry.smarthealthit.org/StructureDefinition/capabilities', s);
  end;
end;

procedure TFHIRCapabilityStatement2.contact(kind: TContactType; value: String);
var
  c : TFhirContactPoint;
  ct : TFhirConformanceContact;
begin
  ct := statement.contactList.Append;
  c := ct.telecomList.Append;
  c.system := MAP_TContactType[kind];
  c.value := 'http://healthintersections.com.au/';
end;

procedure TFHIRCapabilityStatement2.defineFeatures(features: TFslList<TFHIRFeature>);
begin
end;

function TFHIRCapabilityStatement2.getURL: String;
begin
  result := statement.url;
end;

function TFHIRCapabilityStatement2.hasFormat(fmt: String): boolean;
begin
  result := statement.formatList.hasCode(fmt);
end;

function TFHIRCapabilityStatement2.hasRest: boolean;
begin
  result := statement.restList.Count > 0;
end;

function TFHIRCapabilityStatement2.hasSecurity(system, code: String): boolean;
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

procedure TFHIRCapabilityStatement2.impl(url, desc: String);
begin
  if statement.implementation_ = nil then
    statement.implementation_ := TFhirCapabilityStatementImplementation.Create;
  statement.implementation_.description := desc;
  statement.implementation_.url := url;
end;

procedure TFHIRCapabilityStatement2.listSearchParams(name: String; list: TFslList<TFHIRSearchParamDefinitionW>);
var
  r : TFhirConformanceRest;
  rr : TFhirConformanceRestResource;
  sp : TFhirConformanceRestResourceSearchParam;
begin
  for r in statement.restList do
  begin
    if r.mode = RestfulCapabilityModeServer then
    begin
      if name = 'All Types' then
        for sp in r.searchParamList do
          list.Add(TFHIRSearchParamDefinition2.Create(sp.Link))
      else
      begin
        for rr in r.resourceList do
        begin
          if CODES_TFHIRResourceTypesEnum[rr.type_] = name then
            for sp in rr.searchParamList do
              list.Add(TFHIRSearchParamDefinition2.Create(sp.Link))
        end;
      end;
    end;
  end;
end;

procedure TFHIRCapabilityStatement2.listTypes(interactions: TFHIRInteractions; names: TStrings);
var
  r : TFhirConformanceRest;
  it : TFhirConformanceRestInteraction;
  rr : TFhirConformanceRestResource;
  ok : boolean;
  int : TFhirConformanceRestResourceInteraction;
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

procedure TFHIRCapabilityStatement2.readSmartExtension(var authorize, token, register: String);
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

procedure TFHIRCapabilityStatement2.setUrl(Value: String);
begin
  statement.url := value;
end;

function TFHIRCapabilityStatement2.getName : String;
begin
  result := statement.Name;
end;

procedure TFHIRCapabilityStatement2.setName(value : String);
begin
  statement.Name := value;
end;

function TFHIRCapabilityStatement2.getVersion : String;
begin
  result := statement.Version;
end;

procedure TFHIRCapabilityStatement2.setVersion(value : String);
begin
  statement.Version := value;
end;

procedure TFHIRCapabilityStatement2.software(name, version, release: String);
begin
  if statement.software = nil then
    statement.software := TFhirCapabilityStatementSoftware.Create;
  statement.software.name := name;
  statement.software.version := version;
  statement.software.releaseDate := TFslDateTime.fromHL7(release);
end;

function TFHIRCapabilityStatement2.getDescription : String;
begin
  result := statement.Description;
end;

function TFHIRCapabilityStatement2.getFhirVersion: string;
begin
  result := statement.fhirVersion;
end;

procedure TFHIRCapabilityStatement2.setDescription(value : String);
begin
  statement.Description := value;
end;

procedure TFHIRCapabilityStatement2.setFhirVersion(Value: string);
begin
  statement.fhirVersion := value;
end;

function TFHIRCapabilityStatement2.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[statement.Status];
end;

procedure TFHIRCapabilityStatement2.setStatus(Value: TPublicationStatus);
begin
  statement.Status := MAP_TPublicationStatus[value];
end;

procedure TFHIRCapabilityStatement2.fmt(mt: String);
begin
  statement.formatList.Append.value := mt;
end;

function TFHIRCapabilityStatement2.getDate: TFslDateTime;
begin
  result := statement.Date;
end;

procedure TFHIRCapabilityStatement2.setDate(Value: TFslDateTime);
begin
  statement.Date := value;
end;

procedure TFHIRCapabilityStatement2.standardServer(ts, ws, pv, cv, iv: String; transactions, search, history : boolean);
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
  statement.restList[0].transactionMode := TransactionModeBoth;
  statement.text := TFhirNarrative.Create;
  statement.text.status := NarrativeStatusGenerated;
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

function TFHIRCapabilityStatement2.statement: TFhirConformance;
begin
  result := FRes as TFhirConformance;
end;

function TFHIRCapabilityStatement2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRCapabilityStatement2.supportsType(name: String; interaction: TFHIRInteraction): boolean;
var
  r : TFhirConformanceRest;
  it : TFhirConformanceRestInteraction;
  rr : TFhirConformanceRestResource;
  ok : boolean;
  int : TFhirConformanceRestResourceInteraction;
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

function TFHIRCapabilityStatement2.getKind: TCapabilityStatementKind;
begin
  case statement.kind of
    ConformanceStatementKindInstance : result := cskInstance;
    ConformanceStatementKindCapability : result := cskCapability;
    ConformanceStatementKindRequirements : result := cskRequirements;
  else
    result := cskNull;
  end;
end;

function TFHIRCapabilityStatement2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRCapabilityStatement2.setKind(Value: TCapabilityStatementKind);
begin
  case value of
    cskInstance : statement.kind := ConformanceStatementKindInstance;
    cskCapability : statement.kind := ConformanceStatementKindCapability;
    cskRequirements : statement.kind := ConformanceStatementKindRequirements;
  else
    statement.kind := ConformanceStatementKindNull;
  end;
end;

procedure TFHIRCapabilityStatement2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRCapabilityStatement2.getAcceptUnknown: TCapabilityStatementAcceptUnknown;
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

procedure TFHIRCapabilityStatement2.setAcceptUnknown(const Value: TCapabilityStatementAcceptUnknown);
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

{ TFhirParametersParameter2 }

function TFhirParametersParameter2.addParam(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter2.Create(parameter.partList.Append.link);
  TFhirParametersParameter2(result).parameter.name := name;
  PartList.Add(result);
end;

procedure TFhirParametersParameter2.addParam(name: String; value: TFHIRObject);
begin
  parameter.AddParameter(name).value := value as TFHIRType;
end;

procedure TFhirParametersParameter2.addParamBool(name: String; value: boolean);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = LCBooleanToString(value)) then
      exit;
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFhirParametersParameter2.addParamCode(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFhirParametersParameter2.addParamUri(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFhirParametersParameter2.addParamCanonical(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFhirParametersParameter2.addParamStr(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

function TFhirParametersParameter2.getParameterParameter(name: String): TFhirParametersParameterW;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(t);
end;

function TFhirParametersParameter2.getResource: TFHIRResourceV;
begin
  result := parameter.resource;
end;

function TFhirParametersParameter2.getResourceParameter(name: String): TFHIRResourceV;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter2(t).parameter.resource);
end;

function TFhirParametersParameter2.getStringParameter(name: String): String;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := '';
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter2(t).parameter.value.primitiveValue);
end;

function TFhirParametersParameter2.getValue: TFHIRObject;
begin
  result := parameter.value;
end;

function TFhirParametersParameter2.hasResource: boolean;
begin
  result := parameter.resource <> nil;
end;

function TFhirParametersParameter2.hasValue: boolean;
begin
  result := parameter.value <> nil;
end;

function TFhirParametersParameter2.name: String;
begin
  result := parameter.name;
end;

function TFhirParametersParameter2.parameter: TFhirParametersParameter;
begin
  result := Element as TFhirParametersParameter;
end;

function TFhirParametersParameter2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

procedure TFhirParametersParameter2.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.partList do
    FList.Add(TFhirParametersParameter2.Create(t.Link));
end;

procedure TFhirParametersParameter2.setResource(Value: TFHIRResourceV);
begin
  parameter.resource := value as TFhirResource;
end;

procedure TFhirParametersParameter2.setValue(Value: TFHIRObject);
begin
  parameter.value := value as TFHIRType;
end;

function TFhirParametersParameter2.valueString: String;
begin
  if (parameter.value = nil) or (not parameter.value.isPrimitive) then
    result := ''
  else
    result := parameter.value.primitiveValue;
end;

{ TFHIRParameters2 }

function TFHIRParameters2.addParam(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter2.Create(parameter.parameterList.Append.link);
  TFhirParametersParameter2(result).parameter.name := name;
  ParameterList.Add(result);
end;

procedure TFHIRParameters2.addParam(name: String; value: TFHIRObject);
begin
  parameter.AddParameter(name).value := value as TFHIRType;
end;

procedure TFHIRParameters2.addParamBool(name: String; value: boolean);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = LCBooleanToString(value)) then
      exit;
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFHIRParameters2.addParamCode(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFHIRParameters2.addParamUri(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFHIRParameters2.addParamCanonical(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFHIRParameters2.addParamStr(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

function TFHIRParameters2.bool(name: String): boolean;
begin
  result := parameter.bool[name];
end;

function TFHIRParameters2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRParameters2.getParameter(name: String): TFhirParametersParameterW;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(t);
end;

function TFHIRParameters2.names: String;
var
  ts : TStringList;
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  ts := TStringList.Create;
  try
    ts.sorted := true;
    ts.duplicates := dupIgnore;
    for t in FList do
      if (t.value = nil) then
        ts.add(t.name+':nil')
      else
        ts.add(t.name+':'+t.value.fhirType);
    result := ts.commaText;
  finally
    ts.free;
  end; end;

function TFHIRParameters2.has(name: String): boolean;
begin
  result := parameter.hasParameter(name);
end;

function TFHIRParameters2.obj(name: String): TFHIRObject;
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

function TFHIRParameters2.parameter: TFhirParameters;
begin
  result := Resource as TFhirParameters;
end;

procedure TFHIRParameters2.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.parameterList do
    FList.Add(TFhirParametersParameter2.Create(t.Link));
end;

function TFHIRParameters2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

procedure TFHIRParameters2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRParameters2.str(name: String): String;
begin
  result := parameter.str[name];
end;

{ TFHIRStructureDefinition2 }

function TFHIRStructureDefinition2.elements: TFslList<TFHIRElementDefinitionW>;
var
  ed : TFhirElementDefinition;
begin
  result := TFslList<TFHIRElementDefinitionW>.Create;
  try
    for ed in sd.snapshot.elementList do
      result.Add(TFhirElementDefinition2.Create(ed.Link));
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStructureDefinition2.getDefinition(id: String; source: TElementDefinitionSourceOption): TFHIRElementDefinitionW;
var
  ed : TFhirElementDefinition;
begin
  result := nil;
  if (source in [edsSNAPSHOT, edsEITHER]) and (sd.snapshot <> nil) then
    for ed in sd.snapshot.elementList do
      if ed.id = id then
        exit(TFHIRElementDefinition2.Create(ed.Link));

  if (source in [edsDIFF, edsEITHER]) and (sd.differential <> nil) then
    for ed in sd.differential.elementList do
      if ed.id = id then
        exit(TFHIRElementDefinition2.Create(ed.Link));
end;

function TFHIRStructureDefinition2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRStructureDefinition2.kind: TStructureDefinitionKind;
begin
  case sd.kind of
    StructureDefinitionKindDatatype :
      if type_ = 'Extension' then
        result := sdkExtension
      else if StringArrayExistsSensitive(['boolean', 'integer', 'string', 'decimal', 'uri', 'base62Binary', 'instant', 'date', 'dateTime', 'time'], type_) then
        result := sdkPrimitive
      else
        result := sdkDataType;
    StructureDefinitionKindResource : result := sdkResource;
    StructureDefinitionKindLogical : result := sdkResource;
  else
    raise EFHIRException.Create('Unknown value');
  end;
end;

function TFHIRStructureDefinition2.name: String;
begin
  result := sd.name;
end;

function TFHIRStructureDefinition2.sd: TFhirStructureDefinition;
begin
  result := resource as TFhirStructureDefinition;
end;

function TFHIRStructureDefinition2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

procedure TFHIRStructureDefinition2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRStructureDefinition2.type_: String;
begin
  result := sd.type_;
end;

function TFHIRStructureDefinition2.url: String;
begin
  result := sd.url;
end;

{ TFHIRSearchParamDefinition2 }

function TFHIRSearchParamDefinition2.documentation: String;
begin
  result := param.documentation;
end;

function TFHIRSearchParamDefinition2.name: String;
begin
  result := param.name;
end;

function TFHIRSearchParamDefinition2.param: TFhirConformanceRestResourceSearchParam;
begin
  result := FElement as TFhirConformanceRestResourceSearchParam;
end;

function TFHIRSearchParamDefinition2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;


function TFHIRSearchParamDefinition2.type_: TFHIRSearchParamType;
begin
  result := MAP_SearchParamType[param.type_];
end;

{ TFhirElementDefinition2 }

function TFhirElementDefinition2.binding: TElementDefinitionBinding;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  if b = nil then
    result := edbNone
  else
    result := MAP_ELEMENT_DEFINITION_BINDING[b.strength];
end;

function TFhirElementDefinition2.defn: String;
begin
  result := edefn.definition;
end;

function TFhirElementDefinition2.edefn: TFhirElementDefinition;
begin
  result := element as TFhirElementDefinition;
end;

function TFhirElementDefinition2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirElementDefinition2.explicitTypeName: String;
begin
  result := edefn.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-explicit-type-name');
end;

function TFhirElementDefinition2.isSummary: boolean;
begin
  result := edefn.isSummary;
end;

function TFhirElementDefinition2.max: integer;
begin
  if edefn.max = '*' then
    result := MaxInt
  else
    result := StrToInt(edefn.max);
end;

function TFhirElementDefinition2.min: integer;
begin
  result := StrToInt(edefn.min);
end;

function TFhirElementDefinition2.path: String;
begin
  result := edefn.path;
end;

function TFhirElementDefinition2.typeList: TArray<String>;
var
  ed : TFhirElementDefinition;
  i : integer;
begin
  ed := edefn;
  Setlength(result, ed.type_List.Count);
  for i := 0 to ed.type_List.Count - 1 do
    result[i] := ed.type_List[i].code;
end;

function TFhirElementDefinition2.types: String;
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

function TFhirElementDefinition2.valueSet: String;
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

{ TFHIRBundleEntry2 }

function TFHIRBundleEntry2.entry: TFhirBundleEntry;
begin
  result := element as TFhirBundleEntry;
end;

function TFHIRBundleEntry2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRBundleEntry2.getRequestMethod: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := CODES_TFhirHttpVerbEnum[entry.request.method];
end;

function TFHIRBundleEntry2.getRequestUrl: String;
begin
  if entry.request = nil then
    result := ''
  else
    result :=  entry.request.url;
end;

function TFHIRBundleEntry2.getResource: TFHIRResourceV;
begin
  result :=  entry.resource;
end;

function TFHIRBundleEntry2.getResponseDate: TFslDateTime;
begin
  if entry.response = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.response.lastModified;
end;

function TFHIRBundleEntry2.getResponseETag: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.etag;
end;

function TFHIRBundleEntry2.getResponseLocation: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.Location;
end;

function TFHIRBundleEntry2.getResponseStatus: String;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.status;
end;

function TFHIRBundleEntry2.getSearchMode: TFHIRBundleEntrySearchMode;
begin
  if entry.search = nil then
    result := smUnknown
  else
    result := MAP_SEARCH_MODE[entry.search.mode];
end;

function TFHIRBundleEntry2.getSearchMpiMatch: String;
begin
  if entry.search = nil then
    result := ''
  else
    result := entry.search.getExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match')
end;

function TFHIRBundleEntry2.getSearchScore: String;
begin
  if entry.search = nil then
    result := ''
  else
    result := entry.search.score;
end;

function TFHIRBundleEntry2.getURL: String;
begin
  result := entry.fullUrl;
end;

procedure TFHIRBundleEntry2.setRequestMethod(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.Create;
  entry.request.method := TFhirHttpVerbEnum(ord(StringArrayIndexOfSensitive(CODES_TFhirHttpVerbEnum, value)));
end;

procedure TFHIRBundleEntry2.setRequestUrl(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.Create;
  entry.request.url := value;
end;

procedure TFHIRBundleEntry2.setResource(Value: TFHIRResourceV);
begin
  entry.resource := value as TFHIRResource;
end;

procedure TFHIRBundleEntry2.setResponseDate(Value: TFslDateTime);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.lastModified := value;
end;

procedure TFHIRBundleEntry2.setResponseETag(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.ETag := value;
end;

procedure TFHIRBundleEntry2.setResponseLocation(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.Location := value;
end;

procedure TFHIRBundleEntry2.setResponseStatus(Value: String);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.status := value;
end;

procedure TFHIRBundleEntry2.setSearchMode(Value: TFHIRBundleEntrySearchMode);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.mode := MAP_SEARCH_MODE2[value];
end;

procedure TFHIRBundleEntry2.setSearchMpiMatch(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.setExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match', value)
end;

procedure TFHIRBundleEntry2.setSearchScore(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.score := value;
end;

procedure TFHIRBundleEntry2.setUrl(Value: String);
begin
  entry.fullUrl := value;
end;

function TFHIRBundleEntry2.getLink(rel: String): String;
begin
  result := entry.Links[rel];
end;

procedure TFHIRBundleEntry2.setLink(rel: String; const Value: String);
begin
  entry.Links[rel] := value;
end;

function TFHIRBundleEntry2.getrequestIfNoneExist: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.ifNoneExist;
end;

procedure TFHIRBundleEntry2.setrequestIfNoneExist(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.ifNoneExist := value;
end;

function TFHIRBundleEntry2.getrequestIfMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfMatch;
end;

procedure TFHIRBundleEntry2.setrequestIfMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.IfMatch := value;
end;

function TFHIRBundleEntry2.getrequestIfNoneMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfNoneMatch;
end;

procedure TFHIRBundleEntry2.setrequestIfNoneMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.IfNoneMatch := value;
end;

function TFHIRBundleEntry2.getrequestIfModifiedSince: TFslDateTime;
begin
  if entry.request = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.request.ifModifiedSince;
end;

procedure TFHIRBundleEntry2.setrequestIfModifiedSince(Value: TFslDateTime);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.ifModifiedSince := value;
end;

{ TFHIRValueSet2 }

function TFHIRValueSet2.addInclude: TFhirValueSetComposeIncludeW;
begin
  if vs.compose = nil then
    vs.compose := TFhirValueSetCompose.Create;
  result := TFhirValueSetComposeInclude2.Create(vs.compose.includeList.Append.Link);
end;

function TFHIRValueSet2.checkCompose(place, role: String): boolean;
begin
  result := vs.compose <> nil;
  if result then
    vs.compose.checkNoModifiers(place, role, []);
end;

function TFHIRValueSet2.getComposeExtensions: TFslList<TFHIRExtensionW>;
var
  ext : TFHIRObject;
  list : TFslList<TFHIRObject>;
begin
  result := TFslList<TFHIRExtensionW>.Create;
  try
    if (vs.compose <> nil) then
    begin
      list := vs.compose.getExtensionsV;
      try
        for ext in list do
          result.add(TFHIRExtension2.Create(ext.link));
      finally
        list.free;
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValueSet2.checkExpansion(place, role: String): boolean;
begin
  result := vs.expansion <> nil;
  if result then
    vs.expansion.checkNoModifiers(place, role, []);
end;

procedure TFHIRValueSet2.clearDefinition;
begin
  vs.codeSystem := nil;
  vs.requirements := '';
  vs.compose := nil;
  vs.description := '';
  vs.contactList.Clear;
  vs.copyright := '';
  vs.publisher := '';
  vs.extensionList.Clear;
  vs.text := nil;
end;

procedure TFHIRValueSet2.clearDefinitionExtensions(exemptUrls : TStringArray);
begin
  if vs.codeSystem <> nil then
    vs.codeSystem.stripExtensions(exemptUrls);
  if vs.requirementsElement <> nil then
    vs.requirementsElement.stripExtensions(exemptUrls);
  if vs.compose <> nil then
    vs.compose.stripExtensions(exemptUrls);
  if vs.descriptionElement <> nil then
    vs.descriptionElement.stripExtensions(exemptUrls);
  vs.contactList.stripExtensions(exemptUrls);
  if vs.copyrightElement <> nil then
    vs.copyrightElement.stripExtensions(exemptUrls);
  if vs.publisherElement <> nil then
    vs.publisherElement.stripExtensions(exemptUrls);
  if vs.text <> nil then
    vs.text.stripExtensions(exemptUrls);
end;

destructor TFHIRValueSet2.Destroy;
begin
  FExp.free;
  inherited;
end;

function TFHIRValueSet2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRValueSet2.excludes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  if vs.compose = nil then
    result := TFslList<TFhirValueSetComposeIncludeW>.create
  else
  begin
    result := TFslList<TFhirValueSetComposeIncludeW>.Create(vs.compose.excludeList.Count);
    for c in vs.compose.excludeList do
      result.Add(TFhirValueSetComposeInclude2.Create(c.Link));
  end;
end;

function TFHIRValueSet2.expansion: TFhirValueSetExpansionW;
begin
  if (FExp = nil) and (vs.expansion <> nil) then
    FExp := TFhirValueSetExpansion2.Create(vs.expansion.Link);
  result := FExp;
end;

function TFHIRValueSet2.forceExpansion: TFhirValueSetExpansionW;
begin
  if (vs.expansion = nil) then
    vs.expansion := TFhirValueSetExpansion.Create;
  vs.expansion.timestamp := TFslDateTime.makeUTC;
  vs.expansion.identifier := NewGuidURN;
  vs.expansion.parameterList.Clear;
  vs.expansion.containsList.Clear;
  result := expansion;
end;

function TFHIRValueSet2.getContext: String;
begin
  result := vs.context;
end;

function TFHIRValueSet2.getDate: TFslDateTime;
begin
  result := vs.date;
end;

function TFHIRValueSet2.hasExpansion: boolean;
begin
  result := vs.expansion <> nil;
end;

function TFHIRValueSet2.hasInlineCS: boolean;
begin
  result := vs.codeSystem <> nil;
end;

function TFHIRValueSet2.excludeInactives: boolean;
begin
  result := false;
end;

function TFHIRValueSet2.imports: TArray<String>;
var
  i : integer;
begin
  SetLength(result, vs.compose.importList.count);
  for i := 0 to vs.compose.importList.count - 1 do
    result[i] :=  vs.compose.importList[i].value;
end;

function TFHIRValueSet2.includes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  if vs.compose = nil then
    result := TFslList<TFhirValueSetComposeIncludeW>.create
  else
  begin
    result := TFslList<TFhirValueSetComposeIncludeW>.Create(vs.compose.includeList.Count);
    for c in vs.compose.includeList do
      result.Add(TFhirValueSetComposeInclude2.Create(c.Link));
  end;
end;

function TFHIRValueSet2.inlineCS: TFHIRValueSetCodeSystemW;
begin
  if vs.codeSystem = nil then
    result := nil
  else
    result := TFHIRValueSetCodeSystem2.Create(vs.codeSystem.link);
end;

function TFHIRValueSet2.getDescription: String;
begin
  result := vs.description;
end;

function TFHIRValueSet2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRValueSet2.setDate(Value: TFslDateTime);
begin
  vs.date := value;
end;

procedure TFHIRValueSet2.setDescription(value: String);
begin
  vs.description := value;
end;

procedure TFHIRValueSet2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRValueSet2.setName(value: String);
begin
  vs.name := value;
end;

procedure TFHIRValueSet2.setPublisher(Value: String);
begin
  vs.publisher := value;
end;

procedure TFHIRValueSet2.setStatus(Value: TPublicationStatus);
begin
  vs.status := MAP_TPublicationStatus[value];
end;

procedure TFHIRValueSet2.setUrl(value: String);
begin
  vs.url := value;
end;

procedure TFHIRValueSet2.setVersion(value: String);
begin
  vs.version := value;
end;

function TFHIRValueSet2.source: String;
begin
  result := vs.source;
end;

function TFHIRValueSet2.findContains(systemUri, version, code: String): TFhirValueSetExpansionContainsW;
var
  cc : TFhirValueSetExpansionContains;
begin
  cc := vs.findContains(systemuri, version, code);
  if (cc) = nil then
    result := nil
  else
    result := TFhirValueSetExpansionContains2.Create(cc.link);
end;

function TFHIRValueSet2.getExperimental: boolean;
begin
  result := vs.experimental;
end;

procedure TFHIRValueSet2.setExperimental(value: boolean);
begin
  vs.experimental := value;
end;

function TFHIRValueSet2.getName: String;
begin
  result := vs.name;
end;

function TFHIRValueSet2.getPublisher: String;
begin
  result := vs.publisher;
end;

function TFHIRValueSet2.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[vs.status];
end;

function TFHIRValueSet2.getURL: String;
begin
  result := vs.url;
end;

function TFHIRValueSet2.getVersion: String;
begin
  result := vs.version;
end;

function TFHIRValueSet2.vs: TFhirValueSet;
begin
  result := Resource as TFHIRValueSet;
end;


{ TFhirValueSetComposeInclude2 }

function TFhirValueSetComposeInclude2.addConcept: TFhirValueSetComposeIncludeConceptW;
begin
  result := TFhirValueSetComposeIncludeConcept2.Create((Element as TFhirValueSetComposeInclude).conceptList.Append.Link);
end;

function TFhirValueSetComposeInclude2.addFilter: TFhirValueSetComposeIncludeFilterW;
begin
  result := TFhirValueSetComposeIncludeFilter2.Create((Element as TFhirValueSetComposeInclude).FilterList.Append.Link);
end;

function TFhirValueSetComposeInclude2.concepts: TFslList<TFhirValueSetComposeIncludeConceptW>;
var
  i : TFhirValueSetComposeIncludeConcept;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptW>.Create((Element as TFhirValueSetComposeInclude).ConceptList.Count);
  for i in (Element as TFhirValueSetComposeInclude).ConceptList do
    result.Add(TFhirValueSetComposeIncludeConcept2.Create(i.Link));
end;

function TFhirValueSetComposeInclude2.filterCount: integer;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count;
end;

function TFhirValueSetComposeInclude2.conceptCount: integer;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count;
end;

function TFhirValueSetComposeInclude2.filters: TFslList<TFhirValueSetComposeIncludeFilterW>;
var
  i : TFhirValueSetComposeIncludeFilter;
begin
  result := TFslList<TFhirValueSetComposeIncludeFilterW>.Create((Element as TFhirValueSetComposeInclude).filterList.Count);
  for i in (Element as TFhirValueSetComposeInclude).filterList do
    result.Add(TFhirValueSetComposeIncludeFilter2.Create(i.Link));
end;

function TFhirValueSetComposeInclude2.hasConcepts: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count > 0;
end;

function TFhirValueSetComposeInclude2.hasFilters: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count > 0;
end;

function TFhirValueSetComposeInclude2.hasValueSets: boolean;
begin
  result := false;
end;

procedure TFhirValueSetComposeInclude2.setSystem(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).system := Value;
end;

procedure TFhirValueSetComposeInclude2.setVersion(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).version := Value;
end;

function TFhirValueSetComposeInclude2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirValueSetComposeInclude2.getSystem: String;
begin
  result := (Element as TFhirValueSetComposeInclude).system;
end;

function TFhirValueSetComposeInclude2.valueSets: TArray<String>;
begin
  SetLength(result, 0);
end;

function TFhirValueSetComposeInclude2.getVersion: String;
begin
  result := (Element as TFhirValueSetComposeInclude).version;
end;

{ TFhirValueSetComposeIncludeFilter2 }

function TFhirValueSetComposeIncludeFilter2.getOp: TFilterOperator;
begin
  result := MAP_TFilterOperator[(Element as TFhirValueSetComposeIncludeFilter).op];
end;

function TFhirValueSetComposeIncludeFilter2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirValueSetComposeIncludeFilter2.getProp: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).property_;
end;

function TFhirValueSetComposeIncludeFilter2.getValue: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).value;
end;

procedure TFhirValueSetComposeIncludeFilter2.setOp(Value: TFilterOperator);
begin
  (Element as TFhirValueSetComposeIncludeFilter).op := MAP_TFilterOperator2[value];
end;

procedure TFhirValueSetComposeIncludeFilter2.setProp(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).property_ := value;
end;

procedure TFhirValueSetComposeIncludeFilter2.setValue(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).value := value;
end;

{ TFHIRValueSetCodeSystem2 }

function TFHIRValueSetCodeSystem2.concepts: TFhirCodeSystemConceptListW;
var
  i : TFHIRValueSetCodeSystemConcept;
begin
  result := TFhirCodeSystemConceptListW.Create;
  for i in (element as TFHIRValueSetCodeSystem).conceptList do
    result.Add(TFhirCodeSystemConcept2.Create(i.Link));
end;

function TFHIRValueSetCodeSystem2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRValueSetCodeSystem2.systemUri: String;
begin
  result := (element as TFHIRValueSetCodeSystem ).system;
end;

{ TFhirCodeSystemConcept2 }

function TFhirCodeSystemConcept2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirCodeSystemConcept2.code: String;
begin
  result := (Element as TFhirCodeSystemConcept).code;
end;

function TFhirCodeSystemConcept2.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept2.Create((element as TFhirCodeSystemConcept).conceptList[ndx]);
end;

function TFhirCodeSystemConcept2.conceptCount: integer;
begin
  result := (element as TFhirCodeSystemConcept).conceptList.Count;
end;

function TFhirCodeSystemConcept2.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRValueSetCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.Create;
    for i in (element as TFhirCodeSystemConcept).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept2.Create(i.Link));
  end;
  result := FConceptList;
end;

function TFhirCodeSystemConcept2.definition: String;
begin
  result := (Element as TFhirCodeSystemConcept).definition;
end;

function TFhirCodeSystemConcept2.designationCount: integer;
begin
  result := (element as TFhirCodeSystemConcept).designationList.Count;
end;

function TFhirCodeSystemConcept2.designations: TFslList<TFhirCodeSystemConceptDesignationW>;
var
  i : TFhirValueSetCodeSystemConceptDesignation;
begin
  result := TFslList<TFhirCodeSystemConceptDesignationW>.Create;
  for i in (element as TFhirCodeSystemConcept).designationList do
    result.Add(TFhirCodeSystemConceptDesignation2.Create(i.Link));
end;

function TFhirCodeSystemConcept2.display: String;
begin
  result := (Element as TFhirCodeSystemConcept).display;
end;

function TFhirCodeSystemConcept2.displayElement: TFHIRPrimitiveW;
begin
  if (Element as TFhirCodeSystemConcept).displayElement = nil then
    result := nil
  else
    result := TFHIRPrimitive2.Create((Element as TFhirCodeSystemConcept).displayElement.link);
end;

function TFhirCodeSystemConcept2.displayTag(tag: String): String;
begin
  result := (element as TFhirCodeSystemConcept).displayElement.Tags[tag];
end;

function getCodeWrapper(list : TFhirCodeSystemConceptList; code : String) : TFhirCodeSystemConceptW;
var
  cc : TFhirCodeSystemConcept;
begin
  result := nil;
  for cc in list do
  begin
    if cc.code = code then
      result := TFhirCodeSystemConcept2.Create(cc.Link);
    if cc.hasConceptList then
    begin
      result := getCodeWrapper(cc.conceptList, code);
      if result <> nil then
        exit;
    end;
  end;
end;

function TFhirCodeSystemConcept2.getCode(code: String): TFhirCodeSystemConceptW;
begin
  if code = self.code then
    result := self.link
  else
    result := getCodeWrapper((element as TFhirCodeSystemConcept).conceptList, code);
end;

function TFhirCodeSystemConcept2.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFhirValueSetCodeSystemConcept;
begin
  result := false;
  for i in (element as TFhirCodeSystemConcept).conceptList do
    if i.code = c.code then
      exit(true);
end;

function TFhirCodeSystemConcept2.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.Create;
end;

procedure TFhirCodeSystemConcept2.setDisplayTag(tag, value: String);
begin
  if (element as TFhirCodeSystemConcept).displayElement <> Nil then
    (element as TFhirCodeSystemConcept).displayElement.Tags[tag] := value;
end;

function TFhirCodeSystemConcept2.itemWeight : String;       
begin
  result := (Element as TFhirCodeSystemConcept).getExtensionString(EXT_ORDINAL_VALUE);
  if result = '' then
    result := (Element as TFhirCodeSystemConcept).getExtensionString(EXT_ITEM_WEIGHT);
end;


{ TFhirValueSetComposeIncludeConcept2 }

function TFhirValueSetComposeIncludeConcept2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirValueSetComposeIncludeConcept2.getCode: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).code;
end;

function TFhirValueSetComposeIncludeConcept2.designations: TFslList<TFhirValueSetComposeIncludeConceptDesignationW>;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptDesignationW>.Create;
end;

function TFhirValueSetComposeIncludeConcept2.getDisplay: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).display;
end;

procedure TFhirValueSetComposeIncludeConcept2.setCode(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).code := Value;
end;

procedure TFhirValueSetComposeIncludeConcept2.setDisplay(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).display := Value;
end;

function TFhirValueSetComposeIncludeConcept2.displayElement: TFHIRPrimitiveW;
begin
  if (Element as TFhirValueSetComposeIncludeConcept).displayElement = nil then
    result := nil
  else
    result := TFHIRPrimitive2.Create((Element as TFhirValueSetComposeIncludeConcept).displayElement.link);
end;

function TFhirValueSetComposeIncludeConcept2.GetItemWeight: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).getExtensionString(EXT_ORDINAL_VALUE);
  if result = '' then
    result := (Element as TFhirValueSetComposeIncludeConcept).getExtensionString(EXT_ITEM_WEIGHT);
end;

procedure TFhirValueSetComposeIncludeConcept2.SetItemWeight(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).setExtensionString(EXT_ORDINAL_VALUE, value);
end;

{ TFHIRLookupOpResponse2 }

function TFHIRLookupOpResponse2.addDesignation(lang, system, code, display, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.Create;
  try
    if (system <> '') then
    begin
      p.use := TFHIRCoding.Create;
      p.use.system := system;
      p.use.display := display;
      p.use.code := code;
    end;
    p.language := lang;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation2.Create(p.Link);
    list.add(result);
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse2.addDesignation(lang, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.Create;
  try
    p.language := lang;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation2.Create(p.Link);
    list.add(result);
  finally
    p.free;
  end;
end;

procedure TFHIRLookupOpResponse2.addExtension(name: String; value: boolean);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

procedure TFHIRLookupOpResponse2.addExtension(name, value: String);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

function TFHIRLookupOpResponse2.addProp(name: string): TFHIRLookupOpRespPropertyW;
begin
  raise EFHIRException.Create('Properties are not supported in R2');
end;

function TFHIRLookupOpResponse2.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationResponse).asParams;
end;

function TFHIRLookupOpResponse2.GetDisplay: String;
begin
  result := (op as TFHIRLookupOpResponse).display;
end;

function TFHIRLookupOpResponse2.GetName: String;
begin
  result := (op as TFHIRLookupOpResponse).name;
end;

function TFHIRLookupOpResponse2.GetVersion: String;
begin
  result := (op as TFHIRLookupOpResponse).version;
end;

procedure TFHIRLookupOpResponse2.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpResponse).load(params);
end;

procedure TFHIRLookupOpResponse2.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpResponse).load(params as TFhirParameters);
end;

procedure TFHIRLookupOpResponse2.SetDisplay(Value: String);
begin
  (op as TFHIRLookupOpResponse).display := value;
end;

procedure TFHIRLookupOpResponse2.SetName(Value: String);
begin
  (op as TFHIRLookupOpResponse).name := value;
end;

procedure TFHIRLookupOpResponse2.SetVersion(Value: String);
begin
  (op as TFHIRLookupOpResponse).version := value;
end;

{ TFHIRLookupOpRespDesignation2 }

function TFHIRLookupOpRespDesignation2.GetUse: TFHIRObject;
begin
  result := (obj as TFHIRLookupOpRespDesignation).use;
end;

procedure TFHIRLookupOpRespDesignation2.SetUse(Value: TFHIRObject);
begin
  (obj as TFHIRLookupOpRespDesignation).use := value as TFhirCoding;
end;

{ TFhirCodeSystemConceptDesignation2 }

function TFhirCodeSystemConceptDesignation2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirCodeSystemConceptDesignation2.language: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).language;
end;

function TFhirCodeSystemConceptDesignation2.use: TFHIRCodingW;
begin
  if (Element as TFhirCodeSystemConceptDesignation).use = nil then
    result := nil
  else
    result := TFHIRCoding2.Create((Element as TFhirCodeSystemConceptDesignation).use.link);
end;

function TFhirCodeSystemConceptDesignation2.useGen: String;
begin
  result := gen((Element as TFhirCodeSystemConceptDesignation).use);
end;

function TFhirCodeSystemConceptDesignation2.value: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).value;
end;

function TFhirCodeSystemConceptDesignation2.valueElement: TFHIRPrimitiveW;
begin
  if (Element as TFhirCodeSystemConceptDesignation).valueElement = nil then
    result := nil
  else
    result := TFHIRPrimitive2.Create((Element as TFhirCodeSystemConceptDesignation).valueElement.link);
end;

{ TFHIRExtension2 }

function TFHIRExtension2.ext: TFHIRExtension;
begin
  result := (Element as TFHIRExtension);
end;

function TFHIRExtension2.renderText: String;
begin
  result := gen(ext.value);
end;

function TFHIRExtension2.valueAsCodeableConcept: TFhirCodeableConceptW;
begin
  if ext.value is TFHIRCodeableConcept then
    result := TFHIRCodeableConcept2.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension2.valueAsCoding: TFhirCodingW;
begin
  if ext.value is TFHIRCoding then
    result := TFHIRCoding2.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension2.valueAsPeriod: TFhirPeriodW;
begin
  if ext.value is TFHIRPeriod then
    result := TFHIRPeriod2.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension2.valueAsQuantity: TFhirQuantityW;
begin
  if ext.value is TFHIRQuantity then
    result := TFHIRQuantity2.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension2.valueAsIdentifier: TFhirIdentifierW;
begin
  if ext.value is TFHIRIdentifier then
    result := TFHIRIdentifier2.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension2.valueAsAttachment: TFhirAttachmentW;
begin
  if ext.value is TFHIRAttachment then
    result := TFHIRAttachment2.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension2.valueAsString: string;
begin
  if ext.value is TFhirPrimitiveType then
    result := ext.value.primitiveValue
  else
    result := '';
end;

procedure TFHIRExtension2.setValueW(value: TFhirDataTypeW);
begin
  setValueV(value.Element);
end;

procedure TFHIRExtension2.setValueV(value: TFhirObject);
begin
  if not (value is TFHIRType) then
    raise EFHIRException.Create('Wrong type at TFHIRExtension2.setValueV: '+value.ClassName+' ('+Codes_TFHIRVersion[value.fhirObjectVersion]);
  ext.value := (value as TFHIRType).link;
end;

function TFHIRExtension2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRExtension2.url: String;
begin
  result := ext.url;
end;

function TFHIRExtension2.value: TFHIRObject;
begin
  result := ext.value;
end;

{ TFHIRCoding2 }

function TFHIRCoding2.getCode: String;
begin
  result := (element as TFHIRCoding).code;
end;

function TFHIRCoding2.getDisplay: String;
begin
  result := (element as TFHIRCoding).display;
end;

function TFHIRCoding2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRCoding2.getSystem: String;
begin
  result := (element as TFHIRCoding).system;
end;

function TFHIRCoding2.getVersion: String;
begin
  result := (element as TFHIRCoding).version;
end;

function TFHIRCoding2.renderText: String;
begin
  result := gen(element as TFHIRCoding);
end;

procedure TFHIRCoding2.setCode(Value: String);
begin
  (element as TFHIRCoding).code := value;
end;

procedure TFHIRCoding2.setDisplay(Value: String);
begin
  (element as TFHIRCoding).display := value;
end;

procedure TFHIRCoding2.setSystem(Value: String);
begin
  (element as TFHIRCoding).system := value;
end;

procedure TFHIRCoding2.setVersion(Value: String);
begin
  (element as TFHIRCoding).version := value;
end;

{ TFhirCodeSystem2 }

function TFhirCodeSystem2.buildImplicitValueSet: TFHIRValueSetW;
begin
  result := TFHIRValueSet2.Create(vs.Link);
end;

function TFhirCodeSystem2.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept2.Create(cs.conceptList[ndx].Link);
end;

function TFhirCodeSystem2.conceptCount: integer;
begin
  result := cs.conceptList.Count;
end;

function TFhirCodeSystem2.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.Create;
    for i in (resource as TFhirCodeSystem).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept2.Create(i.Link));
  end;
  result := FConceptList;
end;

function TFhirCodeSystem2.copyright: String;
begin
  result := vs.copyright;
end;

function TFhirCodeSystem2.cs: TFHIRValueSetCodeSystem;
begin
  result := vs.codeSystem;
  assert(result <> nil);
end;

function TFhirCodeSystem2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirCodeSystem2.getDescription: String;
begin
  result := vs.description;
end;

function TFhirCodeSystem2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirCodeSystem2.getChildren(c: TFhirCodeSystemConceptW): TFhirCodeSystemConceptListW;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getChildren(c.element as TFhirCodeSystemConcept);
  try
    result := TFhirCodeSystemConceptListW.Create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept2.Create(i.Link));
      result.link;
    finally
      result.free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem2.getCode(code: String): TFhirCodeSystemConceptW;
begin
  result := getCodeWrapper(cs.conceptList, code);
end;

function TFhirCodeSystem2.getContent: TFhirCodeSystemContentMode;
begin
  result := cscmComplete;
end;

function TFhirCodeSystem2.getContext: String;
begin
  result := vs.context;
end;

function TFhirCodeSystem2.getCount: integer;
begin
  result := 0;
end;

function TFhirCodeSystem2.getDate: TFslDateTime;
begin
  result := vs.date;
end;

function TFhirCodeSystem2.getParents(c: TFhirCodeSystemConceptW): TFhirCodeSystemConceptListW;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getParents(c.element as TFhirCodeSystemConcept);
  try
    result := TFhirCodeSystemConceptListW.Create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept2.Create(i.Link));
      result.link;
    finally
      result.free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem2.getPublisher: String;
begin
  result := vs.publisher;
end;

function TFhirCodeSystem2.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[vs.status];
end;

function TFhirCodeSystem2.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (resource as TFhirCodeSystem).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystem2.isInactive(c: TFhirCodeSystemConceptW): boolean;
begin
  result := false; // cs.isInactive(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem2.codeStatus(c: TFhirCodeSystemConceptW): String;
begin
  result := '';
end;

function TFhirCodeSystem2.isAbstract(c: TFhirCodeSystemConceptW): boolean;
begin
  result := (c.Element as TFhirCodeSystemConcept).abstract;
end;

function TFhirCodeSystem2.isDeprecated(c: TFhirCodeSystemConceptW): boolean;
begin
  result := false;
end;

function TFhirCodeSystem2.language: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirCodeSystem2.getName: String;
begin
  result := vs.name;
end;

function TFhirCodeSystem2.properties: TFslList<TFhirCodeSystemPropertyW>;
begin
  result := TFslList<TFhirCodeSystemPropertyW>.Create;
end;

procedure TFhirCodeSystem2.setContent(Value: TFhirCodeSystemContentMode);
begin
end;

procedure TFhirCodeSystem2.setCount(Value: integer);
begin
end;

procedure TFhirCodeSystem2.setDate(Value: TFslDateTime);
begin
  vs.date := value;
end;

procedure TFhirCodeSystem2.setDescription(Value: String);
begin
  vs.description := value;
end;

procedure TFhirCodeSystem2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirCodeSystem2.setName(Value: String);
begin
  vs.name := value;
end;

procedure TFhirCodeSystem2.setPublisher(Value: String);
begin
  vs.publisher := value;
end;

procedure TFhirCodeSystem2.setStatus(Value: TPublicationStatus);
begin
  vs.status := MAP_TPublicationStatus[status];
end;

procedure TFhirCodeSystem2.setUrl(Value: String);
begin
  cs.system := value;
end;

procedure TFhirCodeSystem2.setVersion(Value: String);
begin
  vs.version := value;
end;

function TFhirCodeSystem2.supplements: String;
begin
  result := '';
end;

function TFhirCodeSystem2.getExperimental: boolean;
begin
  result := vs.experimental;
end;

procedure TFhirCodeSystem2.setExperimental(value: boolean);
begin
  vs.experimental := true;
end;

function TFhirCodeSystem2.getURL: String;
begin
  result := cs.system;
end;

function TFhirCodeSystem2.getVersion: String;
begin
  result := vs.version;
end;

function TFhirCodeSystem2.valueSet: String;
begin
  result := vs.url;
end;

function TFhirCodeSystem2.vs: TFhirValueSet;
begin
  result := resource as TFHIRValueSet;
end;

{ TFhirValueSetExpansion2 }

procedure TFhirValueSetExpansion2.addContains(item: TFhirValueSetExpansionContainsW);
begin
  exp.containsList.Add((item.Element as TFhirValueSetExpansionContains).link);
end;

function TFhirValueSetExpansion2.addContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains2.Create((element as TFhirValueSetExpansion).containsList.Append.Link);
end;

procedure TFhirValueSetExpansion2.addParamBool(name: String; value: boolean);
begin
  exp.AddParamBool(name, value);
end;

procedure TFhirValueSetExpansion2.addParamInt(name: String; value: integer);
begin
  exp.AddParamInt(name, value);
end;

procedure TFhirValueSetExpansion2.addParamStr(name, value: String);
begin
  exp.addParamStr(name, value);
end;

procedure TFhirValueSetExpansion2.addParamCode(name, value: String);
begin
   exp.addParamCode(name, value);
end;

procedure TFhirValueSetExpansion2.addParamUri(name, value: String);
begin
  exp.addParamUri(name, value);
end;

function TFhirValueSetExpansion2.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.Create;
  for item in (Element as TFhirValueSetExpansion).containsList do
    result.Add(TFhirValueSetExpansionContains2.Create(item.Link));
end;

function TFhirValueSetExpansion2.getTotal: integer;
begin
  result := StrToIntDef(exp.total, 0);
end;

procedure TFhirValueSetExpansion2.setTotal(value: integer);
begin
  exp.total := inttostr(value);
end;

function TFhirValueSetExpansion2.getOffset: integer;
begin
  result := StrToIntDef(exp.offset, 0);
end;

procedure TFhirValueSetExpansion2.setOffset(value: integer);
begin
  exp.offset := inttostr(value);
end;

procedure TFhirValueSetExpansion2.defineProperty(focus: TFhirValueSetExpansionContainsW; url, code: String; value: TFHIRObject);
begin
  // nothing
end;

procedure TFhirValueSetExpansion2.copyParams(source: TFhirValueSetExpansionW);
var
  param, t : TFhirValueSetExpansionParameter;
  found : boolean;
begin
  for param in (source.Element as TFhirValueSetExpansion).parameterList do
  begin
    found := false;
    for t in (Element as TFhirValueSetExpansion).parameterList do
    begin
      if (t.name = param.name) and (t.valueElement.ToString = param.valueElement.ToString) then
      begin
        found := true;
        break;
      end;
    end;
    if not (found) then
      (Element as TFhirValueSetExpansion).parameterList.Add(param.Link);
  end;

end;

function TFhirValueSetExpansion2.exp: TFhirValueSetExpansion;
begin
  result := element as TFhirValueSetExpansion;
end;

function TFhirValueSetExpansion2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirValueSetExpansion2.hasParam(name, value: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) and (param.value.primitiveValue = value) then
      exit(true);
end;

function TFhirValueSetExpansion2.makeContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains2.Create(TFhirValueSetExpansionContains.create);
end;

function TFhirValueSetExpansion2.hasParam(name: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) then
      exit(true);
end;

{ TFhirValueSetExpansionContains2 }

function TFhirValueSetExpansionContains2.getCode: String;
begin
  result := (Element as TFhirValueSetExpansionContains).code;
end;

procedure TFhirValueSetExpansionContains2.addDesignation(lang, use, value: String);
begin
  // nothing in R2
end;

procedure TFhirValueSetExpansionContains2.addDesignation(lang : TIETFLang; use: TFHIRCodingW; value: TFHIRPrimitiveW; extensions : TFslList<TFHIRExtensionW>);
begin
  // nothing in R2
end;

procedure TFhirValueSetExpansionContains2.addProperty(code: String; value: TFHIRObject);
begin
  // nothing in R2
end;

procedure TFhirValueSetExpansionContains2.addProperty(code: String; prop: TFhirCodeSystemConceptPropertyW);
begin
  // nothing in R2
end;

procedure TFhirValueSetExpansionContains2.addContains(contained: TFhirValueSetExpansionContainsW);
begin
  (Element as TFhirValueSetExpansionContains).containsList.add(contained.Element.link);
end;

procedure TFhirValueSetExpansionContains2.clearContains;
begin
  (Element as TFhirValueSetExpansionContains).containsList.clear;
end;

function TFhirValueSetExpansionContains2.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.Create;
  for item in (Element as TFhirValueSetExpansionContains).containsList do
    result.Add(TFhirValueSetExpansionContains2.Create(item.Link));
end;

function TFhirValueSetExpansionContains2.getDisplay: String;
begin
  result := (Element as TFhirValueSetExpansionContains).display;
end;

function TFhirValueSetExpansionContains2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirValueSetExpansionContains2.getSystem: String;
begin
  result := (Element as TFhirValueSetExpansionContains).system;
end;

function TFhirValueSetExpansionContains2.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.Create;
end;

procedure TFhirValueSetExpansionContains2.setCode(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).code := value;
end;

procedure TFhirValueSetExpansionContains2.setDisplay(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).display := value;
end;

procedure TFhirValueSetExpansionContains2.setSystem(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).system := value;
end;

function TFhirValueSetExpansionContains2.GetItemWeight: String;
begin
  result := (Element as TFhirValueSetExpansionContains).getExtensionString(EXT_ORDINAL_VALUE);
  if result = '' then
    result := (Element as TFhirValueSetExpansionContains).getExtensionString(EXT_ITEM_WEIGHT);
end;

procedure TFhirValueSetExpansionContains2.SetItemWeight(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).setExtensionString(EXT_ORDINAL_VALUE, value);
end;

function TFhirValueSetExpansionContains2.GetAbstract: boolean;
begin
  result := (Element as TFhirValueSetExpansionContains).abstract;
end;

function TFhirValueSetExpansionContains2.GetInactive: boolean;
begin
  result := false;
end;

procedure TFhirValueSetExpansionContains2.SetAbstract(Value: boolean);
begin
  (Element as TFhirValueSetExpansionContains).abstract := value;
end;

procedure TFhirValueSetExpansionContains2.SetInactive(Value: boolean);
begin
  // nothing
end;

function TFhirValueSetExpansionContains2.getVersion: String;
begin
  result := (Element as TFhirValueSetExpansionContains).version;
end;

procedure TFhirValueSetExpansionContains2.setVersion(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).version := value
end;

{ TFhirConceptMap2 }

function TFhirConceptMap2.addGroup(source, target: String): TFhirConceptMapGroupW;
begin
  result := TFhirConceptMapGroup2.Create(cm.Link, source, target);
end;

function TFhirConceptMap2.cm: TFhirConceptMap;
begin
  result := Resource as TFhirConceptMap;
end;

function TFhirConceptMap2.getVersion: String;
begin
  result := cm.version;
end;

function TFhirConceptMap2.groups: TFslList<TFhirConceptMapGroupW>;
var
  e : TFhirConceptMapElement;
  t : TFhirConceptMapElementTarget;
  map : TFslMap<TFhirConceptMapGroupW>;
  g : TFhirConceptMapGroupW;
begin
  result := TFslList<TFhirConceptMapGroupW>.Create;
  try
    map := TFslMap<TFhirConceptMapGroupW>.Create('cm2.group');
    try
      for e in cm.elementList do
        for t in e.targetList do
        begin
          if not map.ContainsKey(e.codeSystem+'|'+t.codeSystem) then
            map.Add(e.codeSystem+'|'+t.codeSystem, TFhirConceptMapGroup2.Create(cm.Link, e.codeSystem, t.codeSystem));
        end;
    finally
      map.free;
    end;
    for g in map.Values do
      result.Add(g.link);
    result.link;
  finally
    result.free;
  end;
end;

procedure TFhirConceptMap2.setVersion(Value: String);
begin
  cm.version := value;
end;

function TFhirConceptMap2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirConceptMap2.source: String;
begin
  if (cm.source is TFhirReference) then
    result := (cm.source as TFhirReference).reference
  else if cm.source <> nil then
    result := cm.source.primitiveValue;
end;

function TFhirConceptMap2.sourceDesc: String;
begin
  result := cm.sourceDesc;
end;

function TFhirConceptMap2.target: String;
begin
  if (cm.target is TFhirReference) then
    result := (cm.target as TFhirReference).reference
  else if cm.target <> nil then
    result := cm.target.primitiveValue;
end;

function TFhirConceptMap2.targetDesc: String;
begin
  result := cm.targetDesc;
end;

function TFhirConceptMap2.getExperimental: boolean;
begin
  result := cm.experimental;
end;

procedure TFhirConceptMap2.setExperimental(value: boolean);
begin
  cm.experimental := value;
end;

function TFhirConceptMap2.getURL: String;
begin
  result := cm.url;
end;

function TFhirConceptMap2.getDate: TFslDateTime;
begin
  result := cm.Date;
end;

function TFhirConceptMap2.getDescription: String;
begin
  result := cm.Description;
end;

function TFhirConceptMap2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirConceptMap2.getName: String;
begin
  result := cm.Name;
end;

function TFhirConceptMap2.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[cm.Status];
end;

procedure TFhirConceptMap2.setDate(Value: TFslDateTime);
begin
  cm.Date := value;
end;

procedure TFhirConceptMap2.setDescription(Value: String);
begin
  cm.Description := value;
end;

procedure TFhirConceptMap2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirConceptMap2.setName(Value: String);
begin
  cm.Name := value;
end;

procedure TFhirConceptMap2.setStatus(Value: TPublicationStatus);
begin
  cm.Status := MAP_TPublicationStatus[value];
end;

procedure TFhirConceptMap2.setUrl(Value: String);
begin
  cm.Url := value;
end;

procedure TFhirConceptMap2.setPublisher(Value: String);
begin
  cm.publisher := value;
end;

function TFhirConceptMap2.getContext: String;
begin
  result := cm.context;
end;

function TFhirConceptMap2.getPublisher: String;
begin
  result := cm.publisher;
end;

{ TFhirConceptMapGroupElementTarget2 }

function TFhirConceptMapGroupElementTarget2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirConceptMapGroupElementTarget2.code: String;
begin
  result := (Element as TFhirConceptMapGroupElementTarget).code;
end;

function TFhirConceptMapGroupElementTarget2.comments: String;
begin
  result := (Element as TFhirConceptMapGroupElementTarget).comments;
end;

function TFhirConceptMapGroupElementTarget2.equivalence: TFHIRConceptEquivalence;
begin
  result := MAP_TFHIRConceptEquivalence[(Element as TFhirConceptMapGroupElementTarget).equivalence];
end;

function TFhirConceptMapGroupElementTarget2.products: TFslList<TFhirConceptMapGroupElementDependsOnW>;
var
  i : TFhirConceptMapElementTargetDependsOn;
begin
  result := TFslList<TFhirConceptMapGroupElementDependsOnW>.Create;
  for i in (Element as TFhirConceptMapGroupElementTarget).productList do
    result.Add(TFhirConceptMapGroupElementDependsOn2.Create(i.link));
end;

{ TFhirConceptMapGroupElement2 }

function TFhirConceptMapGroupElement2.addTarget(code: String; eq: TFHIRConceptEquivalence): TFhirConceptMapGroupElementTargetW;
var
  t : TFhirConceptMapGroupElementTarget;
begin
  t := (Element as TFhirConceptMapGroupElement).targetList.Append;
  t.code := code;
  t.equivalence := MAP_TFHIRConceptEquivalenceR[eq];
  result := TFhirConceptMapGroupElementTarget2.Create(t.link);
end;

function TFhirConceptMapGroupElement2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirConceptMapGroupElement2.code: String;
begin
  result := (Element as TFhirConceptMapGroupElement).code;
end;

function TFhirConceptMapGroupElement2.targetCount: integer;
begin
  result := (Element as TFhirConceptMapGroupElement).targetList.Count;
end;

function TFhirConceptMapGroupElement2.targets: TFslList<TFhirConceptMapGroupElementTargetW>;
var
  i : TFhirConceptMapGroupElementTarget;
begin
  result := TFslList<TFhirConceptMapGroupElementTargetW>.Create;
  for i in (Element as TFhirConceptMapGroupElement).targetList do
    result.Add(TFhirConceptMapGroupElementTarget2.Create(i.link));
end;

{ TFhirConceptMapGroup2 }

function TFhirConceptMapGroup2.addElement(code: String): TFhirConceptMapGroupElementW;
var
  t : TFhirConceptMapGroupElement;
begin
  t := (Element as TFhirConceptMapGroup).elementList.Append;
  t.code := code;
  result := TFhirConceptMapGroupElement2.Create(t.link);
end;

constructor TFhirConceptMapGroup2.Create(res: TFhirResource; source, target: String);
begin
  inherited Create(res);
  FSource := source;
  FTarget := target;
end;

function TFhirConceptMapGroup2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirConceptMapGroup2.elements: TFslList<TFhirConceptMapGroupElementW>;
var
  t : TFhirConceptMapGroupElement;
begin
  result := TFslList<TFhirConceptMapGroupElementW>.Create;
  for t in (Element as TFhirConceptMapGroup).elementList do
    result.Add(TFhirConceptMapGroupElement2.Create(t.link))
end;

function TFhirConceptMapGroup2.source: String;
begin
  result := Fsource;
end;

function TFhirConceptMapGroup2.target: String;
begin
  result := Ftarget;
end;

function TFhirConceptMapGroup2.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (Fsource.length * sizeof(char)) + 12);
  inc(result, ( Ftarget.length * sizeof(char)) + 12);
end;

{ TFHIRMeta2 }

procedure TFHIRMeta2.addLabel(system, code, display: String);
var
  c : TFHIRCoding;
begin
  force;
  c := TFHIRCoding.Create;
  try
    c.system := system;
    c.code := code;
    c.display := display;
    m.securityList.Add(c.link);
  finally
    c.free;
  end;
end;

procedure TFHIRMeta2.addProfile(uri: String);
begin
  force;
  m.profileList.Add(TFhirUri.Create(uri));
end;

procedure TFHIRMeta2.addTag(system, code, display: String);
var
  c : TFHIRCoding;
begin
  force;
  c := TFHIRCoding.Create;
  try
    c.system := system;
    c.code := code;
    c.display := display;
    m.tagList.Add(c.link);
  finally
    c.free;
  end;
end;

procedure TFHIRMeta2.clearLabels;
begin
  if Element <> nil then
    m.securityList.Clear;
end;

procedure TFHIRMeta2.clearProfiles;
begin
  if m <> nil then
    m.profileList.Clear;
end;

procedure TFHIRMeta2.clearTags;
begin
  if Element <> nil then
    m.tagList.Clear;
end;

destructor TFHIRMeta2.Destroy;
begin
  FResource.free;
  inherited;
end;

function TFHIRMeta2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

procedure TFHIRMeta2.force;
begin
  if Element = nil then
  begin
    FElement := TFHIRMeta.Create;
    Resource.meta := m.Link;
  end;
end;

function TFHIRMeta2.getLastUpdated: TFslDateTime;
begin
  if Element = nil then
    result := TFslDateTime.makeNull
  else
    result := m.lastUpdated;
end;

function TFHIRMeta2.getVersionId: String;
begin
  if Element = nil then
    result := ''
  else
    result := m.versionId;
end;

function TFHIRMeta2.hasLabel(system, code: String): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if Element <> nil then
    for c in m.securityList do
      if (c.system = system) and (c.code = code) then
        exit(true);
end;

function TFHIRMeta2.hasTag(system, code: String): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if Element <> nil then
    for c in m.tagList do
       if (c.system = system) and (c.code = code) then
        exit(true);
end;

function TFHIRMeta2.labels: TFslList<TFHIRCodingW>;
var
  i : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if Element <> nil then
    for i in m.securityList do
      result.Add(TFHIRCoding2.Create(i.Link));
end;

function TFHIRMeta2.m: TFhirMeta;
begin
  result := Element as TFHIRMeta;
end;

function TFHIRMeta2.NoElementOk: boolean;
begin
  result := true;
end;

function TFHIRMeta2.profiles: TArray<String>;
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

procedure TFHIRMeta2.removeLabel(system, code: String);
begin
  if Element <> nil then
    m.removeLabel(system, code);
end;

procedure TFHIRMeta2.removeProfile(uri: String);
begin
  if Element <> nil then
    m.profileList.removeUri(uri);
end;

procedure TFHIRMeta2.removeTag(system, code: String);
begin
  if Element <> nil then
    m.removeTag(system, code);
end;

procedure TFHIRMeta2.setLastUpdated(Value: TFslDateTime);
begin
  force;
  m.lastUpdated := value;
end;

procedure TFHIRMeta2.setResource(value: TFHIRResource);
begin
  FResource.free;
  FResource := value;
end;

procedure TFHIRMeta2.setVersionId(Value: String);
begin
  force;
  m.versionId := value;
end;

function TFHIRMeta2.tags: TFslList<TFHIRCodingW>;
var
  i : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if Element <> nil then
    for i in m.tagList do
      result.Add(TFHIRCoding2.Create(i.Link));
end;


function TFHIRMeta2.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FResource.sizeInBytes(magic));
end;

{ TFHIRAuditEvent2 }

function TFHIRAuditEvent2.ae: TFHIRAuditEvent;
begin
  result := Resource as TFhirAuditEvent;
end;

function TFHIRAuditEvent2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRAuditEvent2.dateTime: TFslDateTime;
begin
  result := ae.event.dateTime;
end;

procedure TFHIRAuditEvent2.eventSubType(system, code, display: String);
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

procedure TFHIRAuditEvent2.eventType(system, code, display: String);
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

function TFHIRAuditEvent2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRAuditEvent2.participantId(system, value, alt, name: String);
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

procedure TFHIRAuditEvent2.participantIp(ip: String);
var
  p: TFhirAuditEventParticipant;
begin
  p := ae.participantList.append;
  p.network := TFhirAuditEventParticipantNetwork.Create;
  p.network.address := ip;
  p.network.type_ := NetworkType2;
end;

procedure TFHIRAuditEvent2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRAuditEvent2.source(name, system, value: String);
begin
  if ae.source = nil then
    ae.source := TFhirAuditEventSource.Create;
  ae.source.site := name;
  ae.source.identifier := TFhirIdentifier.Create;
  ae.source.identifier.system := system;
  ae.source.identifier.value := value;
end;

procedure TFHIRAuditEvent2.sourceType(system, code, display: String);
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

procedure TFHIRAuditEvent2.success;
begin
  if ae.event = nil then
    ae.event := TFhirAuditEventEvent.Create;
  ae.event.action := AuditEventActionE;
  ae.event.outcome := AuditEventOutcome0;
  ae.event.dateTime := TFslDateTime.makeUTC;
end;

{ TFhirCapabilityStatementRestResource2 }

procedure TFhirCapabilityStatementRestResource2.addParam(html, n, url, d: String; t: TFHIRSearchParamType; tgts: array of String);
var
  param : TFhirCapabilityStatementRestResourceSearchParam;
begin
  param := TFhirCapabilityStatementRestResourceSearchParam.Create;
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

function TFhirCapabilityStatementRestResource2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirCapabilityStatementRestResource2.getCode: String;
begin
  result := CODES_TFhirResourceTypesEnum[(Element as TFhirCapabilityStatementRestResource).type_];
end;

procedure TFhirCapabilityStatementRestResource2.setCode(Value: String);
begin
  (Element as TFhirCapabilityStatementRestResource).type_Element := TFhirEnum.Create('http://hl7.org/fhir/resource-types', value);
end;

function TFhirCapabilityStatementRestResource2.getProfile: String;
begin
  if (Element as TFhirCapabilityStatementRestResource).profile <> nil then
    result := (Element as TFhirCapabilityStatementRestResource).profile.reference;
end;

procedure TFhirCapabilityStatementRestResource2.setProfile(Value: String);
begin
  if value = '' then
    (Element as TFhirCapabilityStatementRestResource).profile := nil
  else
    (Element as TFhirCapabilityStatementRestResource).profile := TFhirReference.Create(value);
end;


procedure TFhirCapabilityStatementRestResource2.addInteraction(code: String);
begin
  (Element as TFhirCapabilityStatementRestResource).interactionList.Append.codeElement := TFhirEnum.Create('http://hl7.org/fhir/ValueSet/type-restful-interaction', code);
end;

function TFhirCapabilityStatementRestResource2.getReadHistory: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).readHistory;
end;

function TFhirCapabilityStatementRestResource2.hasInteraction: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).interactionList.Count > 0;
end;

procedure TFhirCapabilityStatementRestResource2.setReadHistory(Value: boolean);
begin
  (Element as TFhirCapabilityStatementRestResource).readHistory := Value;
end;


{ TFHIRSubscription2 }

function TFHIRSubscription2.getCriteria: String;
begin
  result := sub.criteria;
end;

function TFHIRSubscription2.getDirect: boolean;
begin
  result := sub.channel.endpointElement.hasExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct')
    and (sub.channel.endpointElement.getExtensionString('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct') = 'true');
end;

function TFHIRSubscription2.getEndpoint: String;
begin
  result := sub.channel.endpoint;
end;

function TFHIRSubscription2.getError: String;
begin
  result := sub.error;
end;

function TFHIRSubscription2.getHeaders: TArray<String>;
begin
  if sub.channel.header <> '' then
  begin
    setLength(result, 1);
    result[0] := sub.channel.header;
  end
  else
    setLength(result, 0);
end;

function TFHIRSubscription2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRSubscription2.getMethod: TSubscriptionMethod;
begin
  result := MAP_TSubscriptionMethod[sub.channel.type_];
end;

function TFHIRSubscription2.getPayload: String;
begin
  result := sub.channel.payload;
end;

function TFHIRSubscription2.getStatus: TSubscriptionStatus;
begin
  result := MAP_TSubscriptionStatus[sub.status];
end;

function TFHIRSubscription2.getSummary: String;
begin
  result := sub.channel.type_Element.value+#1+sub.channel.endpoint+#1+sub.channel.payload;
  result := result+#0+sub.channel.header;
end;

function TFHIRSubscription2.getTopic: string;
begin
  result := '';
end;

procedure TFHIRSubscription2.setCriteria(Value: String);
begin
  sub.criteria := value;
end;

procedure TFHIRSubscription2.setDirect(Value: boolean);
begin
  if value then
    sub.channel.endpointElement.setExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct', TFHIRBoolean.Create(true))
  else
    sub.channel.endpointElement.removeExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct');
end;

procedure TFHIRSubscription2.setEndpoint(Value: String);
begin
  sub.channel.endpoint := value;
end;

procedure TFHIRSubscription2.setError(Value: String);
begin
  sub.error := value;
end;

procedure TFHIRSubscription2.setheaders(Value: TArray<String>);
begin
  if length(value) = 0 then
    sub.channel.header := ''
  else
    sub.channel.header := value[0];
end;

procedure TFHIRSubscription2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRSubscription2.setMethod(Value: TSubscriptionMethod);
begin
  sub.channel.type_ := MAP_TSubscriptionMethod2[value];
end;

procedure TFHIRSubscription2.setPayload(Value: String);
begin
  sub.channel.payload := value;
end;

procedure TFHIRSubscription2.setStatus(Value: TSubscriptionStatus);
begin
  sub.status := MAP_TSubscriptionStatus2[value];
end;

function TFHIRSubscription2.sub: TFhirSubscription;
begin
  result := resource as TFhirSubscription;
end;

function TFHIRSubscription2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

{ TFhirObservationComponent2 }

function TFhirObservationComponent2.codings: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if comp.code <> nil then
    for c in comp.code.codingList do
      result.Add(TFHIRCoding2.Create(c.Link));
end;

function TFhirObservationComponent2.comp: TFhirObservationComponent;
begin
  result := (Element as TFhirObservationComponent);
end;

function TFhirObservationComponent2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirObservationComponent2.dataAbsentReason: TFhirCodeableConceptW;
begin
  if comp.dataAbsentReason = nil then
    result := nil
  else
    result := TFhirCodeableConcept2.Create(comp.dataAbsentReason.link);
end;

function TFhirObservationComponent2.getValue: TFHIRObject;
begin
  result := comp.value;
end;

procedure TFhirObservationComponent2.setValue(Value: TFHIRObject);
begin
  comp.value := value as TFHIRType;
end;

function TFhirObservationComponent2.valueString: String;
begin
  if (comp.value <> nil) and (comp.value.isPrimitive) then
    result := comp.value.primitiveValue
  else
    result := '';
end;

function TFhirObservationComponent2.valueW: TFHIRXVersionElementWrapper;
begin
  if comp.value is TFhirCodeableConcept then
    result := TFhirCodeableConcept2.Create(comp.value.Link)
  else if comp.value is TFHIRQuantity then
    result := TFHIRQuantity2.Create(comp.value.Link)
  else
    result := nil;
end;

{ TFhirObservation2 }

procedure TFhirObservation2.setCode(c: TFHIRCodingW);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.codingList.add((c.Element as TFHIRCoding).Link);
end;

function TFhirObservation2.addComp(system, code: String): TFhirObservationComponentW;
var
  c : TFhirObservationComponent;
begin
  c := TFhirObservationComponent.Create;
  c.code := TFhirCodeableConcept.Create;
  c.code.codingList.add(TFhirCoding.Create(system, code));
  result := TFhirObservationComponent2.Create(c.link);
end;

function TFhirObservation2.getStatus: TObservationStatus;
begin
  result := MAP_TObservationStatus2[obs.status];
end;

function TFhirObservation2.obs: TFHIRObservation;
begin
  result := resource as TFhirObservation;
end;

function TFhirObservation2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

procedure TFhirObservation2.setPeriod(start, finish: TDateTime);
begin
  obs.effective := TFhirPeriod.Create;
  TFhirPeriod(obs.effective).start := TFslDateTime.makeUTC(start);
  TFhirPeriod(obs.effective).end_ := TFslDateTime.makeUTC(finish);
end;

procedure TFhirObservation2.setStatus(Value: TObservationStatus);
begin
  obs.status := MAP_TObservationStatus[value];
end;

function TFhirObservation2.codings: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if obs.code <> nil then
    for c in obs.code.codingList do
      result.Add(TFHIRCoding2.Create(c.Link));
end;

function TFhirObservation2.components: TFslList<TFhirObservationComponentW>;
var
  c : TFhirObservationComponent;
begin
  result := TFslList<TFhirObservationComponentW>.Create;
  for c in obs.componentList do
    result.Add(TFhirObservationComponent2.Create(c.Link));
end;

function TFhirObservation2.dataAbsentReason: TFhirCodeableConceptW;
begin
  if obs.dataAbsentReason = nil then
    result := nil
  else
    result := TFhirCodeableConcept2.Create(obs.dataAbsentReason.link);
end;

procedure TFhirObservation2.getDates(var dt, dtMin, dtMax: TDateTime);
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
      // nothing
    else
      dtMax := (obs.effective as TFHIRPeriod).end_.Max.UTC.DateTime;
  end;
end;

function TFhirObservation2.getValue: TFHIRObject;
begin
  result := obs.value;
end;

procedure TFhirObservation2.setValue(Value: TFHIRObject);
begin
  obs.value := value as TFhirType;
end;

function TFhirObservation2.valueW: TFHIRXVersionElementWrapper;
begin
  if obs.value is TFhirCodeableConcept then
    result := TFhirCodeableConcept2.Create(obs.value.Link)
  else if obs.value is TFHIRQuantity then
    result := TFHIRQuantity2.Create(obs.value.Link)
  else
    result := nil;
end;

function TFhirObservation2.hasTime: boolean;
begin
  result := obs.effective <> nil;
end;

function TFhirObservation2.categories: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if obs.category <> nil then
    for c in obs.category.codingList do
      result.Add(TFHIRCoding2.Create(c.Link));
end;


procedure TFhirObservation2.setCode(system, code, display: String);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.codingList.add(TFHIRCoding.Create(system, code, display));
end;

procedure TFhirObservation2.SetCodeText(const Value: String);
begin
  if value <> '' then
  begin
    if obs.code = nil then
      obs.code := TFhirCodeableConcept.Create;
    obs.code.text := Value
  end
  else if (obs.code <> nil) and (obs.code.codingList.Count = 0) then
    obs.code := nil;
end;

procedure TFhirObservation2.SetComment(const Value: String);
begin
  obs.comments := value;
end;

procedure TFhirObservation2.SetDevice(const Value: String);
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

procedure TFhirObservation2.SetDeviceName(const Value: String);
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

procedure TFhirObservation2.SetEffective(const Value: TFHIRObject);
begin
  obs.effective := Value as TFhirType;
end;

procedure TFhirObservation2.SetEffectiveDateTime(const Value: TFslDateTime);
begin
  SetEffective(TFhirDateTime.Create(value));
end;

procedure TFhirObservation2.SetEffectivePeriod(const Value: TFHIRPeriodW);
begin
  obs.effective := (value.Element as TFHIRType).Link;
end;

procedure TFhirObservation2.SetIssued(const Value: TFslDateTime);
begin
  obs.issued := Value;
end;

procedure TFhirObservation2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirObservation2.SetSubject(const Value: String);
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

function TFhirObservation2.GetCodeText: String;
begin
  if obs.code = nil then
    result := ''
  else
    result := obs.code.text;
end;

function TFhirObservation2.GetComment: String;
begin
  result := obs.comments;
end;

function TFhirObservation2.getComponent(system, code: String; var comp: TFhirObservationComponentW): boolean;
var
  c : TFHIRObservationComponent;
begin
  comp := nil;
  result := obs.getComponent(system, code, c);
  if result then
    comp := TFHIRObservationComponent2.Create(c.link);
end;

function TFhirObservation2.getComponent(system: String; var comp: TFhirObservationComponentW): boolean;
var
  c : TFHIRObservationComponent;
begin
  comp := nil;
  result := obs.getComponent(system, c);
  if result then
    comp := TFHIRObservationComponent2.Create(c.link);
end;

function TFhirObservation2.GetDevice: String;
begin
  if obs.device <> nil then
    result := obs.device.reference
  else
    result := '';
end;

function TFhirObservation2.GetDeviceName: String;
begin
  if obs.device <> nil then
    result := obs.device.display
  else
    result := '';
end;

function TFhirObservation2.GetEffective: TFHIRObject;
begin
  result := obs.effective;
end;

function TFhirObservation2.GetEffectiveDateTime: TFslDateTime;
begin
  if obs.effective is TFhirDateTime then
    result := (obs.effective as TFhirDateTime).value
  else
    result := TFslDateTime.makeNull;
end;

function TFhirObservation2.GetEffectivePeriod: TFHIRPeriodW;
begin
  if obs.effective is TFhirPeriod then
    result := TFHIRPeriod2.Create(obs.effective.Link)
  else
    result := nil;
end;

function TFhirObservation2.GetIssued: TFslDateTime;
begin
  result := obs.issued;
end;

function TFhirObservation2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirObservation2.hasDevice: boolean;
begin
  result := obs.Device <> nil;
end;

function TFhirObservation2.hasEffective: boolean;
begin
  result := obs.Effective <> nil;
end;

function TFhirObservation2.hasIssued: boolean;
begin
  result := obs.IssuedElement <> nil;
end;

function TFhirObservation2.hasMethod: boolean;
begin
  result := obs.Method <> nil;
end;

function TFhirObservation2.hasSubject: boolean;
begin
  result := obs.Subject <> nil;
end;

function TFhirObservation2.method(force : boolean) : TFhirCodeableConceptW;
begin
  if (obs.method = nil) and force then
    obs.method := TFhirCodeableConcept.Create;

  if obs.method = nil then
    result := nil
  else
    result := TFhirCodeableConcept2.Create(obs.method.link);
end;

function TFhirObservation2.GetSubject: String;
begin
  if obs.subject <> nil then
    result := obs.subject.reference
  else
    result := '';
end;

procedure TFhirObservation2.addCategory(c: TFHIRCodingW);
begin
  if obs.category = nil then
    obs.category := TFhirCodeableConcept.Create;
  obs.category.codingList.add((c.Element as TFHIRCoding).Link);
end;

procedure TFhirObservation2.addCategory(system, code, display: String);
begin
  if obs.category = nil then
    obs.category := TFhirCodeableConcept.Create;
  obs.category.codingList.add(TFHIRCoding.Create(system, code, display));
end;

procedure TFhirObservation2.setCode(text: String);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.text := text;
end;

{ TFHIRQuantity2 }

function TFHIRQuantity2.asDuration: TDateTime;
begin
  result := qty.asDuration;
end;

function TFHIRQuantity2.getCode: String;
begin
  result := qty.code;
end;

function TFHIRQuantity2.getSystem: String;
begin
  result := qty.system;
end;

function TFHIRQuantity2.getUnit: String;
begin
  result := qty.unit_;
end;

function TFHIRQuantity2.getValue: String;
begin
  result := qty.value;
end;

function TFHIRQuantity2.qty: TFHIRQuantity;
begin
  result := Element as TFHIRQuantity;
end;

function TFHIRQuantity2.renderText: String;
begin
  result := gen(element as TFhirQuantity);
end;

procedure TFHIRQuantity2.setCode(Value: String);
begin
  qty.code := Value;
end;

procedure TFHIRQuantity2.setSystem(Value: String);
begin
  qty.system := Value;
end;

procedure TFHIRQuantity2.setUnit(Value: String);
begin
  qty.unit_ := Value;
end;

procedure TFHIRQuantity2.setValue(Value: String);
begin
  qty.value := Value;
end;

function TFHIRQuantity2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

{ TFHIRLookupOpRequest2 }

function TFHIRLookupOpRequest2.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationRequest).asParams;
end;

function TFHIRLookupOpRequest2.coding: TFHIRCodingW;
begin
  if (op as TFHIRLookupOpRequest).coding = nil then
    result := nil
  else
    result := TFHIRCoding2.Create((op as TFHIRLookupOpRequest).coding.Link);
end;

function TFHIRLookupOpRequest2.displayLanguage: String;
begin
  result := '';
end;

procedure TFHIRLookupOpRequest2.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpRequest).load(params);
end;

procedure TFHIRLookupOpRequest2.loadCoding;
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
    raise EFSLException.Create('Unable to find a code to lookup (need coding or system/code)');
end;

function TFHIRLookupOpRequest2.propList: TArray<String>;
begin
  SetLength(result, 0);
end;

procedure TFHIRLookupOpRequest2.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpRequest).load(params as TFHIRParameters);
end;

{ TFhirCodeableConcept2 }

procedure TFhirCodeableConcept2.addCoding(coding: TFHIRCodingW);
begin
  (Element as TFhirCodeableConcept).codingList.Add((coding.Element as TFHIRCoding).link);
end;

function TFhirCodeableConcept2.addCoding: TFHIRCodingW;
begin
  result := TFHIRCoding2.Create((Element as TFhirCodeableConcept).codingList.Append.link);
end;

procedure TFhirCodeableConcept2.removeCoding(systemUri, version, code: String);
var
  i : integer;
  c : TFHIRCoding;
begin
  for i := (Element as TFhirCodeableConcept).codingList.count - 1 downto 0 do
  begin
    c := (Element as TFhirCodeableConcept).codingList[i];
    if ((systemUri = '') or (systemUri = c.system)) and
        ((version = '') or (version = c.system)) and
        ((code = '') or (code = c.system)) then
      (Element as TFhirCodeableConcept).codingList.remove(i);
  end;
end;

function TFhirCodeableConcept2.codingCount: integer;
begin
  result := (Element as TFhirCodeableConcept).codingList.Count;
end;

function TFhirCodeableConcept2.codings: TFslList<TFhirCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFhirCodingW>.Create;
  for c in (Element as TFhirCodeableConcept).codingList do
    result.Add(TFHIRCoding2.Create(c.Link));
end;

function TFhirCodeableConcept2.fromSystem(Systems: TArray<String>; required: boolean): String;
begin
  result := (Element as TFhirCodeableConcept).fromSystem(Systems, required);
end;

function TFhirCodeableConcept2.GetText: String;
begin
  result := (Element as TFhirCodeableConcept).text;
end;

function TFhirCodeableConcept2.renderText: String;
begin
  result := gen(element as TFhirCodeableConcept);
end;

procedure TFhirCodeableConcept2.SetText(const Value: String);
begin
  (Element as TFhirCodeableConcept).text := Value;
end;

function TFhirCodeableConcept2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirCodeableConcept2.fromSystem(System: String; required: boolean): String;
begin
  result := (Element as TFhirCodeableConcept).fromSystem(System, required);
end;

function TFhirCodeableConcept2.summary: String;
begin
  result := summarise(Element as TFhirCodeableConcept);
end;

function TFhirCodeableConcept2.hasCode(systemUri, code: String): boolean;
begin
  result := (Element as TFhirCodeableConcept).hasCode(systemUri, code);
end;

function TFhirCodeableConcept2.hasCode(systemUri, version, code: String): boolean;
begin
  result := (Element as TFhirCodeableConcept).hasCode(systemUri, version, code);
end;

procedure TFhirCodeableConcept2.clearCodings;
begin
  (Element as TFhirCodeableConcept).codingList.Clear;
end;

procedure TFhirCodeableConcept2.addCoding(systemUri, version, code, display : String);
begin
  (Element as TFhirCodeableConcept).addCoding(systemUri, version, code, display);
end;

{ TFHIRGroup2 }

function TFHIRGroup2.characteristics: TFslList<TFHIRGroupCharacteristicW>;
var
  gc : TFHIRGroupCharacteristic;
begin
  result := TFslList<TFHIRGroupCharacteristicW>.Create;
  for gc in (Resource as TFHIRGroup).characteristicList do
    result.add(TFHIRGroupCharacteristic2.Create(gc.link));
end;

function TFHIRGroup2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRGroup2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRGroup2.hasCharacteristics: boolean;
begin
  result := (Resource as TFHIRGroup).characteristicList.count > 0;
end;

function TFHIRGroup2.hasMembers: boolean;
begin
  result := (Resource as TFHIRGroup).memberList.count > 0;
end;

function TFHIRGroup2.name: String;
begin
  result := (Resource as TFHIRGroup).name;
end;

procedure TFHIRGroup2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFHIRGroupCharacteristic2 }

function TFHIRGroupCharacteristic2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRGroupCharacteristic2.code: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConcept2.Create((element as TFHIRGroupCharacteristic).code);
end;

function TFHIRGroupCharacteristic2.value: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConcept2.Create((element as TFHIRGroupCharacteristic).value);
end;

{ TFhirBinary2 }

function TFhirBinary2.content: TBytes;
begin
  result := (resource as TFHIRBinary).content;
end;

function TFhirBinary2.ContentType: String;
begin
  result := (resource as TFHIRBinary).contentType;
end;

function TFhirBinary2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirBinary2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFhirBinary2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFHIRNamingSystem2 }

function TFHIRNamingSystem2.getContext: String;
begin
  result := '';
end;

function TFHIRNamingSystem2.getDate: TFslDateTime;
begin
  result := nm.date;
end;

function TFHIRNamingSystem2.getDescription: String;
begin
  result := nm.description;
end;

function TFHIRNamingSystem2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRNamingSystem2.getName: String;
begin
  result := nm.name;
end;

function TFHIRNamingSystem2.getPublisher: String;
begin
  result := nm.publisher;
end;

function TFHIRNamingSystem2.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[nm.status];
end;

function TFHIRNamingSystem2.getUri: String;
begin
  result := nm.getUri;
end;

function TFHIRNamingSystem2.getURL: String;
begin
  result := '';
end;

function TFHIRNamingSystem2.getVersion: String;
begin
  result := '';
end;

function TFHIRNamingSystem2.hasOid(oid: String): boolean;
begin
  result := nm.hasOid(oid);
end;

function TFHIRNamingSystem2.nm: TFHIRNamingSystem;
begin
  result := (resource as TFHIRNamingSystem);
end;

function TFHIRNamingSystem2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

procedure TFHIRNamingSystem2.setDate(Value: TFslDateTime);
begin
  nm.date := value;
end;

procedure TFHIRNamingSystem2.setDescription(Value: String);
begin
  nm.description := value;
end;

procedure TFHIRNamingSystem2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRNamingSystem2.setName(Value: String);
begin
  nm.name := value;
end;

procedure TFHIRNamingSystem2.setPublisher(Value: String);
begin
  nm.publisher := value;
end;

procedure TFHIRNamingSystem2.setStatus(Value: TPublicationStatus);
begin
  nm.status := MAP_TPublicationStatus[value];
end;

procedure TFHIRNamingSystem2.setUrl(Value: String);
begin
  // nothing
end;

procedure TFHIRNamingSystem2.setVersion(Value: String);
begin
  // nothing
end;

function TFHIRNamingSystem2.getExperimental: boolean;
begin
  result := false;
end;

procedure TFHIRNamingSystem2.setExperimental(value: boolean);
begin
end;

{ TFhirPatient2 }

function TFhirPatient2.pat: TFHIRPatient;
begin
  result := resource as TFhirPatient;
end;

function TFhirPatient2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirPatient2.nameSummary: String;
begin
  result := HumanNamesAsText(pat.nameList);
end;

function TFhirPatient2.activeStr: String;
begin
  if pat.activeElement = nil then
    result := ''
  else if pat.active then
    result := 'true'
  else
    result := 'false';
end;

function TFhirPatient2.GetActive: boolean;
begin
  result := pat.active;
end;

procedure TFhirPatient2.SetActive(const Value: boolean);
begin
  pat.active := value;
end;

function TFhirPatient2.gender: String;
begin
  result := CODES_TFhirAdministrativeGenderEnum[pat.gender];
end;

function TFhirPatient2.genderPlus: String;
begin
  result := Gender;
end;

function TFhirPatient2.GetDob: String;
begin
  result := pat.birthDate.toXML;
end;

function TFhirPatient2.GetFamily: String;
var
  n : TFhirHumanName;
begin
  result := '';
  for n in pat.nameList do
    if (n.use = NameUseNull) and (n.familyList.Count > 0) then
      exit(n.familyList[0].value);
  for n in pat.nameList do
    if (n.use = NameUseUsual) and (n.familyList.Count > 0) then
      exit(n.familyList[0].value);
  for n in pat.nameList do
    if (n.use = NameUseOfficial) and (n.familyList.Count > 0) then
      exit(n.familyList[0].value);
  for n in pat.nameList do
    if (n.familyList.Count > 0) then
      exit(n.familyList[0].value);
end;

procedure TFhirPatient2.SetFamily(const Value: String);
var
  n : TFhirHumanName;
begin
  for n in pat.nameList do
    if (n.use = NameUseNull) then
    begin
      n.familyList.Clear;
      n.familyList.add(value);
      exit();
    end;
  for n in pat.nameList do
    if (n.use = NameUseUsual) then
    begin
      n.familyList.Clear;
      n.familyList.add(value);
      exit();
    end;
  for n in pat.nameList do
    if (n.use = NameUseOfficial) then
    begin
      n.familyList.Clear;
      n.familyList.add(value);
      exit();
    end;
  n := pat.nameList.Append;
  n.familyList.Clear;
  n.familyList.add(value);
end;

procedure TFhirPatient2.SetDob(const Value: String);
begin
  pat.birthDate := TFslDateTime.fromXML(value);
end;

function TFhirPatient2.GetIdentifier(systemUri: String): String;
var
  id : TFhirIdentifier;
begin
  result := '';
  for id in pat.identifierList do
    if id.system = systemUri then
      exit(id.value);
end;

procedure TFhirPatient2.SetIdentifier(systemUri: String; const Value: String);
var
  id : TFhirIdentifier;
begin
  for id in pat.identifierList do
    if id.system = systemUri then
    begin
      id.value := value;
      exit();
    end;
  id := pat.identifierList.Append;
  id.system := systemUri;
  id.value := Value;
end;

function TFhirPatient2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirPatient2.identifierSummary: String;
begin
  result := IdentifiersAsText(pat.identifierList);
end;

procedure TFhirPatient2.addGiven(name: String);
var
  n : TFhirHumanName;
begin
  for n in pat.nameList do
    if (n.use = NameUseNull) then
    begin
      n.givenList.add(name);
      exit();
    end;
  for n in pat.nameList do
    if (n.use = NameUseUsual) then
    begin
      n.givenList.add(name);
      exit();
    end;
  for n in pat.nameList do
    if (n.use = NameUseOfficial) then
    begin
      n.givenList.add(name);
      exit();
    end;
  n := pat.nameList.Append;
  n.givenList.add(name);
end;

function TFhirPatient2.contactSummary: String;
begin
  result := ContactsAsText(pat.telecomList);
end;

procedure TFhirPatient2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirTerminologyCapabilities2 }

procedure TFhirTerminologyCapabilities2.contact(kind: TContactType; value: String);
begin
end;

function TFhirTerminologyCapabilities2.getContext: String;
begin
end;

function TFhirTerminologyCapabilities2.getDate: TFslDateTime;
begin
end;

function TFhirTerminologyCapabilities2.getDescription: String;
begin
end;


function TFhirTerminologyCapabilities2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirTerminologyCapabilities2.getName: String;
begin
end;


function TFhirTerminologyCapabilities2.getPublisher: String;
begin
end;


function TFhirTerminologyCapabilities2.getStatus: TPublicationStatus;
begin
  result := psDraft;
end;


function TFhirTerminologyCapabilities2.getURL: String;
begin
end;


function TFhirTerminologyCapabilities2.getVersion: String;
begin
end;


procedure TFhirTerminologyCapabilities2.setContext(Value: String);
begin
end;


procedure TFhirTerminologyCapabilities2.setDate(Value: TFslDateTime);
begin
end;


procedure TFhirTerminologyCapabilities2.setDescription(Value: String);
begin
end;


procedure TFhirTerminologyCapabilities2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirTerminologyCapabilities2.setName(Value: String);
begin
end;


procedure TFhirTerminologyCapabilities2.setPublisher(Value: String);
begin
end;


procedure TFhirTerminologyCapabilities2.setStatus(Value: TPublicationStatus);
begin
end;


procedure TFhirTerminologyCapabilities2.setUrl(Value: String);
begin
end;


procedure TFhirTerminologyCapabilities2.setVersion(Value: String);
begin
end;

function TFhirTerminologyCapabilities2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;


procedure TFhirTerminologyCapabilities2.systemUri(url: String);
begin
  (FRes as TFhirParameters).AddParameter('system', TFhirUri.Create(url));
end;

procedure TFhirTerminologyCapabilities2.addExpansionParameter(code, doco : String);
begin
  (FRes as TFhirParameters).AddParameter('expansion.parameter', TFhirCode.Create(code));
end;

{ TFHIRPeriod2 }

function TFHIRPeriod2.GetEnd: TFslDateTime;
begin
  result := period.end_;
end;

function TFHIRPeriod2.GetStart: TFslDateTime;
begin
  result := period.start;
end;

function TFHIRPeriod2.period: TFHIRPeriod;
begin
  result := Element as TFHIRPeriod;
end;

function TFHIRPeriod2.renderText: String;
begin
  result := gen(element as TFhirPeriod);
end;

procedure TFHIRPeriod2.SetEnd(const Value: TFslDateTime);

begin
  period.end_ := value;
end;

procedure TFHIRPeriod2.SetStart(const Value: TFslDateTime);
begin
  period.start := value;
end;

function TFHIRPeriod2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;


{ TFhirEncounter2 }

function TFhirEncounter2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirEncounter2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirEncounter2.patientId: String;
begin
  result := (FRes as TFHIREncounter).patient.getId;
end;

procedure TFhirEncounter2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFhirEncounter2.summary: String;
begin
  result := CODES_TFhirEncounterStateEnum[(FRes as TFHIREncounter).status];
end;

{ TFhirTestScript2 }

function TFHIRTestScript2.getContext: String;
begin
  result := '';
end;

function TFHIRTestScript2.getExperimental: boolean;
begin
  result := ts.experimental;
end;

procedure TFHIRTestScript2.setExperimental(value: boolean);
begin
  ts.experimental := value;
end;

function TFHIRTestScript2.getDate: TFslDateTime;
begin
  result := ts.date;
end;

function TFHIRTestScript2.getDescription: String;
begin
  result := ts.description;
end;

function TFHIRTestScript2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRTestScript2.getName: String;
begin
  result := ts.name;
end;

function TFHIRTestScript2.getPublisher: String;
begin
  result := ts.publisher;
end;

function TFHIRTestScript2.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[ts.Status];
end;

function TFHIRTestScript2.getURL: String;
begin
  result := ts.url;
end;

function TFHIRTestScript2.getVersion: String;
begin
  result := ts.version;
end;

procedure TFHIRTestScript2.setDate(Value: TFslDateTime);
begin
  ts.date := value;
end;


procedure TFHIRTestScript2.setDescription(Value: String);
begin
  ts.description := value;
end;


procedure TFHIRTestScript2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRTestScript2.setName(Value: String);
begin
  ts.name := value;
end;


procedure TFHIRTestScript2.setPublisher(Value: String);
begin
  ts.publisher := value;
end;


procedure TFHIRTestScript2.setStatus(Value: TPublicationStatus);
begin
  ts.Status := MAP_TPublicationStatus[Value];
end;


procedure TFHIRTestScript2.setUrl(Value: String);
begin
  ts.url := value;
end;

procedure TFHIRTestScript2.setVersion(Value: String);
begin
  ts.version := value;
end;

function TFHIRTestScript2.ts: TFHIRTestScript;
begin
  result := (Fres as TFhirTestScript);
end;

function TFHIRTestScript2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

{ TFhirProvenance2 }

procedure TFhirProvenance2.addTarget(url: String);
begin
  p.targetList.Append.reference := url;
end;

procedure TFhirProvenance2.clearSignatures;
begin
  p.signatureList.Clear;
end;

procedure TFhirProvenance2.clearTargets;
begin
  p.targetList.Clear;
end;

function TFhirProvenance2.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirProvenance2.p: TFhirProvenance;
begin
  result := (Fres as TFhirProvenance);
end;

function TFhirProvenance2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

procedure TFhirProvenance2.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirConceptMapGroupElementDependsOn2 }

function TFhirConceptMapGroupElementDependsOn2.display: String;
begin
  result := '';
end;

function TFhirConceptMapGroupElementDependsOn2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirConceptMapGroupElementDependsOn2.property_: String;
begin
  result := (Element as TFhirConceptMapElementTargetDependsOn).element;
end;

function TFhirConceptMapGroupElementDependsOn2.system_: String;
begin
  result := (Element as TFhirConceptMapElementTargetDependsOn).codeSystem;
end;

function TFhirConceptMapGroupElementDependsOn2.value: String;
begin
  result := (Element as TFhirConceptMapElementTargetDependsOn).code;
end;

{ TFHIRAttachment2 }

function TFHIRAttachment2.att: TFhirAttachment;
begin
  result := Element as TFhirAttachment;
end;

function TFHIRAttachment2.GetContentType: String;
begin
  result := att.contentType;
end;

function TFHIRAttachment2.GetData: TBytes;
begin
  result := att.data;
end;

function TFHIRAttachment2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFHIRAttachment2.renderText: String;
begin
  result := '??';
end;

{ TFhirImmunization2 }

function TFhirImmunization2.code(systemUri: String): String;
var
  c : TFHIRCoding;
begin
  result := '';
  for c in imm.vaccineCode.codingList do
    if (c.system = systemUri) then
      exit(c.code);
end;

function TFhirImmunization2.GetDate: TFslDateTime;
begin
  result := imm.date;
end;

function TFhirImmunization2.GetLanguage: String;
begin
  result := imm.language;
end;

function TFhirImmunization2.GetLotNumber: String;
begin
  result := imm.lotNumber;
end;

function TFhirImmunization2.GetManufacturerIdSystem: String;
begin
  result := '';
end;

function TFhirImmunization2.GetManufacturerIdValue: String;
begin
  result := '';
end;

function TFhirImmunization2.GetPatient: String;
begin
  if imm.patient = nil then
    result := ''
  else
    result := imm.patient.reference;
end;

function TFhirImmunization2.GetPerformerDisplay: String;
begin
  result := '';
  if (imm.performer <> nil) then
    result := imm.performer.display;
end;

function TFhirImmunization2.GetStatus: string;
begin
  result := imm.statusElement.value;
end;

function TFhirImmunization2.hasCode(systemUri, code: String): boolean;
var
  c : TFHIRCoding;
begin
  result := false;
  for c in imm.vaccineCode.codingList do
    if (c.system = systemUri) and (c.code = code) then
      exit(true);
end;

function TFhirImmunization2.imm: TFhirImmunization;
begin
  result := resource as TFhirImmunization;
end;

function TFhirImmunization2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

procedure TFhirImmunization2.setCodeBySystem(systemUri: String; code: String);
var
  c : TFHIRCoding;
begin
  for c in imm.vaccineCode.codingList do
    if (c.system = systemUri) then
    begin
      c.code := code;
      exit;
    end;
  c := imm.vaccineCode.codingList.Append;
  c.system := systemUri;
  c.code := code;
end;

procedure TFhirImmunization2.SetDate(const Value: TFslDateTime);
begin
  imm.date := value;
end;

procedure TFhirImmunization2.SetLanguage(const Value: String);
begin
  imm.language := value;
end;

procedure TFhirImmunization2.SetLotNumber(const Value: String);
begin
  imm.lotNumber := value;
end;

procedure TFhirImmunization2.SetManufacturerIdSystem(const Value: String);
begin
end;

procedure TFhirImmunization2.SetManufacturerIdValue(const Value: String);
begin
end;

procedure TFhirImmunization2.SetPatient(const Value: String);
begin
  if (imm.patient = nil) then
    imm.patient := TFhirReference.Create;
  imm.patient.reference := value;
end;

procedure TFhirImmunization2.SetPerformerDisplay(const Value: String);
begin
  if imm.performer = nil then
    imm.performer := TFhirReference.Create;
  imm.performer.display := value;
end;

procedure TFhirImmunization2.SetStatus(const Value: string);
begin
  imm.statusElement.value := value;
end;

{ TFhirIdentifier2 }

function TFhirIdentifier2.id: TFHIRIdentifier;
begin
  result := FElement as TFHIRIdentifier;
end;

function TFhirIdentifier2.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension2.Create(extension.link);
end;

function TFhirIdentifier2.renderText: String;
begin
  result := gen(element as TFhirIdentifier);
end;

function TFhirIdentifier2.GetSystem: String;
begin
  result := id.system;
end;

function TFhirIdentifier2.GetValue: String;
begin
  result := id.value;
end;

procedure TFhirIdentifier2.SetSystem(const Value: String);
begin
  id.system := value;
end;

procedure TFhirIdentifier2.SetValue(const Value: String);
begin
  id.value := value;
end;

const
  identifier_use_version_to_general : array [TFhirIdentifierUseEnum] of TIdentifierUse = (iuNull, iuUsual, iuOfficial, iuTemp, iuSecondary);
  identifier_use_general_to_version : array [TIdentifierUse] of TFhirIdentifierUseEnum = (IdentifierUseNull, IdentifierUseUsual, IdentifierUseOfficial, IdentifierUseTemp, IdentifierUseSecondary, IdentifierUseTemp);

function TFhirIdentifier2.GetUse: TIdentifierUse;
begin
  result := identifier_use_version_to_general[id.use];
end;

procedure TFhirIdentifier2.SetUse(const Value: TIdentifierUse);
begin
  id.use := identifier_use_general_to_version[value];
end;

function TFhirIdentifier2.GetTypeV: TFhirCodeableConceptW;
begin
  if id.type_ = nil then
    result := nil
  else
    result := TFhirCodeableConcept2.Create(id.type_.Link);
end;

procedure TFhirIdentifier2.SetTypeV(const Value: TFhirCodeableConceptW);
begin
  if value = nil then
    id.type_ := nil
  else
    id.type_ := ((Value as TFhirCodeableConcept2).FElement as TFhirCodeableConcept).link;
end;

end.
