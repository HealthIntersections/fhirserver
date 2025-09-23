unit fhir5_common;

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
{$I fhir5.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_http, fsl_lang,
  fhir_objects, fhir_common, fhir_extensions, fhir_features, fhir_uris,
  fhir5_types, fhir5_resources, fhir5_operations, fhir5_opbase, fhir5_enums;

const
  ExceptionTypeTranslations : array [TFhirIssueType] of TFhirIssueTypeEnum = (IssueTypeNull, IssueTypeInvalid, IssueTypeStructure, IssueTypeRequired, IssueTypeValue,
    IssueTypeInvariant, IssueTypeSecurity, IssueTypeLogin, IssueTypeUnknown, IssueTypeExpired, IssueTypeForbidden, IssueTypeSuppressed, IssueTypeProcessing,
    IssueTypeNotSupported, IssueTypeDuplicate, IssueTypeNotFound, IssueTypeTooLong, IssueTypeCodeInvalid, IssueTypeExtension, IssueTypeTooCostly, IssueTypeBusinessRule,
    IssueTypeConflict, IssueTypeIncomplete, IssueTypeTransient, IssueTypeLockError, IssueTypeNoStore, IssueTypeException, IssueTypeTimeout, IssueTypeThrottled, IssueTypeInformational);

  ISSUE_SEVERITY_MAP : array [TFhirIssueSeverityEnum] of TIssueSeverity = (isNull, isFatal, isError, isWarning, isInformation);
  ISSUE_SEVERITY_MAP2 : array [TIssueSeverity] of TFhirIssueSeverityEnum = (IssueSeverityNull, IssueSeverityFatal, IssueSeverityError, IssueSeverityWarning, IssueSeverityInformation);
  INTERACTION_MAP : array [TFHIRInteraction] of TFhirTypeRestfulInteractionEnum = (TypeRestfulInteractionRead, TypeRestfulInteractionSearchType, TypeRestfulInteractionHistoryType, TypeRestfulInteractionCreate, TypeRestfulInteractionUpdate, TypeRestfulInteractionDelete, TypeRestfulInteractionPatch);
  INTERACTION_MAP2 : array [TFHIRInteraction] of TFhirSystemRestfulInteractionEnum = (SystemRestfulInteractionNull, SystemRestfulInteractionSearchSystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem);
  MAP_SearchParamType : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptString, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri, sptSpecial);
  MAP_SEARCH_MODE : array [TFhirSearchEntryModeEnum] of TFHIRBundleEntrySearchMode = (smUnknown, smMatch, smInclude, smOutcome);
  MAP_SEARCH_MODE2 : array [TFHIRBundleEntrySearchMode] of TFhirSearchEntryModeEnum = (SearchEntryModeNull, SearchEntryModeMatch, searchEntryModeInclude, searchEntryModeOutcome);
  MAP_ELEMENT_DEFINITION_BINDING : array [TFhirBindingStrengthEnum] of TElementDefinitionBinding = (edbNone, edbRequired, edbExtensible, edbPreferred, edpExample);
  MAP_TFilterOperator : array [TFhirFilterOperatorEnum] of TFilterOperator = (foNull, foEqual, foIsA, foDescendentOf, foIsNotA, foRegex, foIn, foNotIn, foGeneralizes, foChildOf, foDescendentLeaf, foExists);
  MAP_TFilterOperatorR : array [TFilterOperator] of TFhirFilterOperatorEnum = (filterOperatorNull, filterOperatorEqual, filterOperatorIsA, filterOperatorDescendentOf, filterOperatorIsNotA, filterOperatorRegex, filterOperatorIn, filterOperatorNotIn, filterOperatorGeneralizes, filterOperatorExists, filterOperatorNull, filterOperatorNull, filterOperatorNull);
  MAP_TFhirConceptPropertyTypeEnum : array [TFhirConceptPropertyTypeEnum] of TFhirCodeSystemPropertyType = (cptNull, cptCode, cptCoding, cptString, cptInteger, cptBoolean, cptDateTime, cptDecimal);
  MAP_TFHIRSearchParamType1 : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptNull, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri, sptSpecial);
  MAP_TFHIRSearchParamType2 : array [TFhirSearchParamType] of TFHIRSearchParamTypeEnum = (SearchParamTypeNull, SearchParamTypeNumber, SearchParamTypeDate, SearchParamTypeString, SearchParamTypeToken, SearchParamTypeReference, SearchParamTypeComposite, SearchParamTypeQuantity, SearchParamTypeUri, SearchParamTypeNull);
  MAP_TPublicationStatus : array [TPublicationStatus] of TFHIRPublicationStatusEnum = (PublicationStatusNull, PublicationStatusDraft, PublicationStatusActive, PublicationStatusRetired);
  MAP_TPublicationStatusR : array [TFHIRPublicationStatusEnum] of TPublicationStatus = (psNull, psDraft, psActive, psRetired, psNull);
  MAP_TFhirCodeSystemContentMode : array [TFhirCodeSystemContentMode] of TFhirCodeSystemContentModeEnum = (CodesystemContentModeNull, CodesystemContentModeNotPresent, CodesystemContentModeExample, CodesystemContentModeFragment, CodesystemContentModeComplete, CodesystemContentModeSupplement);
  MAP_TFhirCodeSystemContentModeR : array [TFhirCodeSystemContentModeEnum] of TFhirCodeSystemContentMode = (cscmNull, cscmNotPresent, cscmExample, cscmFragment, cscmComplete, cscmSupplement);
  MAP_TFHIRConceptEquivalence : array [TFhirConceptMapRelationshipEnum] of TFHIRConceptEquivalence = (cmeNull, cmeRelatedto, cmeEquivalent, cmeWider, cmeNarrower, cmeUnmatched);
  MAP_TFHIRConceptEquivalenceR : array [TFHIRConceptEquivalence] of TFhirConceptMapRelationshipEnum = (ConceptMapRelationshipNull, ConceptMapRelationshipRelatedto, ConceptMapRelationshipRelatedto, ConceptMapRelationshipRelatedto, ConceptMapRelationshipSourceIsNarrowerThanTarget, ConceptMapRelationshipSourceIsNarrowerThanTarget, ConceptMapRelationshipSourceIsBroaderThanTarget, ConceptMapRelationshipSourceIsNarrowerThanTarget, ConceptMapRelationshipNotRelatedTo, ConceptMapRelationshipNotRelatedTo, ConceptMapRelationshipNotRelatedTo);
  MAP_TContactType : array [TContactType] of TFhirContactPointSystemEnum = (ContactPointSystemNull, ContactPointSystemPhone, ContactPointSystemFax, ContactPointSystemEmail, ContactPointSystemPager, ContactPointSystemUrl, ContactPointSystemSms, ContactPointSystemOther);
  MAP_TContactType2 : array [TFhirContactPointSystemEnum] of TContactType = (cpsNull, cpsPhone, cpsFax, cpsEmail, cpsPager, cpsUrl, cpsSms, cpsOther);
  MAP_TSubscriptionStatus : array [TFhirSubscriptionStatusCodesEnum] of TSubscriptionStatus = (ssNull, ssRequested, ssActive, ssError, ssOff, ssEnteredInError);
  MAP_TSubscriptionStatus2 : array [TSubscriptionStatus] of TFhirSubscriptionStatusCodesEnum = (SubscriptionStatusCodesNull, SubscriptionStatusCodesRequested, SubscriptionStatusCodesActive, SubscriptionStatusCodesError, SubscriptionStatusCodesOff, SubscriptionStatusCodesEnteredInError);
  BUNDLE_TYPE_TITLE : Array[TFhirBundleTypeEnum] of String = ('', 'Document', 'Message', 'Transaction', 'Transaction Response', 'Batch', 'Batch Response', 'History Record', 'Search Results', 'Resource Collection', 'Subscription Notification');
  MAP_TFHIRBundleType  : array [TBundleType] of TFhirBundleTypeEnum = (BundleTypeNull, BundleTypeDocument, BundleTypeMessage, BundleTypeTransaction, BundleTypeTransactionResponse, BundleTypeBatch, BundleTypeBatchResponse, BundleTypeHistory, BundleTypeSearchset, BundleTypeCollection);
  MAP_TFHIRBundleTypeR : array [TFhirBundleTypeEnum] of TBundleType = (btNull, btDocument, btMessage, btTransaction, btTransactionResponse, btBatch, btBatchResponse, btHistory, btSearchset, btCollection, btCollection);
  MAP_TObservationStatus : array [TObservationStatus] of TFhirObservationStatusEnum = (ObservationStatusNull, ObservationStatusRegistered, ObservationStatusPreliminary, ObservationStatusFinal, ObservationStatusAmended, ObservationStatusCorrected, ObservationStatusCancelled, ObservationStatusEnteredInError, ObservationStatusUnknown);
  MAP_TObservationStatus2 : array [TFhirObservationStatusEnum] of TObservationStatus = (obssNull, obssRegistered, obssPreliminary, obssFinal, obssAmended, obssCorrected, obssCancelled, obssEnteredInError, obssUnknown);


type

  { TFHIRPrimitive5 }

  TFHIRPrimitive5 = class (TFHIRPrimitiveW)
  public
    function GetAsString : String; override;
    procedure SetAsString(value : String); override;
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
  end;

  { TFHIRExtension5 }

  TFHIRExtension5 = class (TFHIRExtensionW)
  private
    function ext : TFHIRExtension;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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

  { TFHIRCoding5 }

  TFHIRCoding5 = class (TFHIRCodingW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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

  { TFhirCodeableConcept5 }

  TFhirCodeableConcept5 = class (TFhirCodeableConceptW)
  protected
    function GetText: String; override;
    procedure SetText(const Value: String); override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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

  { TFhirIdentifier5 }

  TFhirIdentifier5 = class (TFhirIdentifierW)
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
    function renderText : String; override;
  end;

  { TFhirOperationOutcome5 }

  TFhirOperationOutcome5 = class (TFhirOperationOutcomeW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function hasText : boolean; override;
    function text : String; override;
    function code : TFhirIssueType; override;
    procedure addIssue(issue : TFhirOperationOutcomeIssueW; free : boolean); override;
    procedure addIssueNoId(level : TIssueSeverity; cause : TFHIRIssueType; path, message : String; code : TOpIssueCode; addIfDuplicate : boolean); override;
    procedure addIssue(level : TIssueSeverity; cause : TFhirIssueType; path, msgId, message : String; code : TOpIssueCode; addIfDuplicate : boolean = false); overload; override;
    procedure addDiagsIssue(message : string); override;
    function hasIssues : boolean; override;
    function issues : TFslList<TFhirOperationOutcomeIssueW>; override;
    function rule(level : TIssueSeverity; source : String; typeCode : TFhirIssueType; path : string; test : boolean; msg : string) : boolean; override;
    function severity : TIssueSeverity; override;
    function issueCount : integer; override;
    function hasErrors : boolean; override;
  end;

  { TFHIRBundleEntry5 }

  TFHIRBundleEntry5 = class (TFHIRBundleEntryW)
  private
    function entry : TFhirBundleEntry;
  protected
    function getRequestMethod: String; override;
    function getRequestUrl: String; override;
    function getResource: TFHIRResourceV; override;
    function getResponseDate: TFslDateTime; override;
    function getResponseStatus: String; override;
    function getSearchMode: TFHIRBundleEntrySearchMode; override;
    function getSearchMpiMatch: String; override;
    function getSearchScore: String; override;
    procedure setRequestMethod(Value: String); override;
    procedure setRequestUrl(Value: String); override;
    procedure setResource(Value: TFHIRResourceV); override;
    procedure setResponseDate(Value: TFslDateTime); override;
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
    function getResponseETag: string; override;
    procedure setResponseETag(Value: string); override;
    function getResponseLocation: string; override;
    procedure setResponseLocation(Value: string); override;
    function getrequestIfModifiedSince: TFslDateTime; override;
    procedure setrequestIfModifiedSince(Value: TFslDateTime); override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getLink(rel: String): String; override;
    procedure setLink(rel: String; const Value: String); override;
  end;

  { TFhirBinary5 }

  TFhirBinary5 = class (TFhirBinaryW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function ContentType : String; override;
    function content : TBytes; override;
  end;


  { TFHIRBundle5 }

  TFHIRBundle5 = class (TFHIRBundleW)
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

  { TFHIROperationOutcomeIssue5 }

  TFHIROperationOutcomeIssue5 = class (TFHIROperationOutcomeIssueW)
  private
    function issue : TFHIROperationOutcomeIssue;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function display : String; override;
    procedure addCode(systemUri, code : String); override;
    function severity : TIssueSeverity; override;
    function getDiagnostics: String; override;
    procedure setDiagnostics(Value: String); override;
  end;

  { TFHIRSearchParamDefinition5 }

  TFHIRSearchParamDefinition5 = class (TFHIRSearchParamDefinitionW)
  private
    function param : TFhirCapabilityStatementRestResourceSearchParam;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function name : String; override;
    function documentation : String; override;
    function type_ : TFHIRSearchParamType; override;
  end;

  { TFhirCapabilityStatementRestResource5 }

  TFhirCapabilityStatementRestResource5 = class (TFhirCapabilityStatementRestResourceW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getCode: String; override;
    procedure setCode(Value: String); override;
    function getProfile: String; override;
    function hasInteraction : boolean; override;
    procedure setProfile(Value: String); override;
    procedure addInteraction(codeV, doco : String);  override;
    procedure addOperation(codeV, defn, doco : String); override;
    function getReadHistory: boolean; override;
    procedure setReadHistory(Value: boolean); override;
    procedure addParam(html, n, url, d : String; t : TFHIRSearchParamType; tgts : Array of String); override;
  end;

  { TFHIRCapabilityStatement5 }

  TFHIRCapabilityStatement5 = class (TFHIRCapabilityStatementW)
  private
    function statement : TFhirCapabilityStatement;
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
    procedure standardServer(ts, ws, pv, cv, iv : String; transactions, search, history : boolean); override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
    function addResource(code : String) : TFhirCapabilityStatementRestResourceW; override;
    procedure addOperation(name, url : String); override;

    function getURL: String; override;
    procedure setUrl(Value: String); override;
    function getName : String; override;
    procedure setName(value : String); override;
    function getTitle : String; override;
    procedure setTitle(value : String); override;
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
    procedure addTxFeature(version : String); override;
  end;

  { TFhirElementDefinition5 }

  TFhirElementDefinition5 = class (TFhirElementDefinitionW)
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

  { TFHIRStructureDefinition5 }

  TFHIRStructureDefinition5 = class (TFhirStructureDefinitionW)
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

  { TFhirParametersParameter5 }

  TFhirParametersParameter5 = class (TFhirParametersParameterW)
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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function name : String; override;
    function hasValue : boolean; override;
    function valueString : String; override;
    property value : TFHIRObject read GetValue write SetValue;
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

  { TFHIRParameters5 }

  TFHIRParameters5 = class (TFHIRParametersW)
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
    procedure addParam(name : String; value : TFHIRObject); overload; override;
    procedure addParamCode(name : String; value : string); override;
    procedure addParamUri(name : String; value : string); override;
    procedure addParamCanonical(name : String; value : string); override;
    function addParam(name : String) : TFhirParametersParameterW; overload; override;
    function bool(name : String) : boolean; override;
    function str(name : String) : String; override;
    function has(name : String) : boolean; override;
    function obj(name : String) : TFHIRObject; override;
    function getParameter(name: String): TFhirParametersParameterW;  override;
    function names : String; override;
  end;

  { TFhirValueSetExpansionContains5 }

  TFhirValueSetExpansionContains5 = class (TFhirValueSetExpansionContainsW)
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
    procedure addProperty(code : String; value : TFHIRObject); overload; override;
    procedure addProperty(code : String; prop : TFhirCodeSystemConceptPropertyW); overload; override;
    procedure addContains(contained : TFhirValueSetExpansionContainsW); override;
    procedure clearContains(); override;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; override;
  end;

  { TFhirValueSetExpansion5 }

  TFhirValueSetExpansion5 = class (TFhirValueSetExpansionW)
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

  { TFhirValueSetComposeIncludeFilter5 }

  TFhirValueSetComposeIncludeFilter5 = class (TFhirValueSetComposeIncludeFilterW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getProp : String; override;
    function getOp : TFilterOperator; override;
    function getValue : String; override;
    procedure setOp(Value: TFilterOperator); override;
    procedure setProp(Value: String); override;
    procedure setValue(Value: String); override;
  end;

  { TFhirValueSetComposeIncludeConceptDesignation5 }

  TFhirValueSetComposeIncludeConceptDesignation5 = class (TFhirValueSetComposeIncludeConceptDesignationW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function language : String; override;
    function value : String; override;
    function use : TFHIRCodingW; override;
    function valueElement : TFHIRPrimitiveW; override;
  end;

  { TFhirValueSetComposeIncludeConcept5 }

  TFhirValueSetComposeIncludeConcept5 = class (TFhirValueSetComposeIncludeConceptW)
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

  { TFhirValueSetComposeInclude5 }

  TFhirValueSetComposeInclude5 = class (TFhirValueSetComposeIncludeW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getSystem : String; override;
    procedure setSystem(Value: String); override;
    function getVersion : String; override;
    procedure setVersion(Value: String); override;

    function valueSets : TArray<String>; override;
    function hasConcepts : boolean; override;
    function concepts : TFslList<TFhirValueSetComposeIncludeConceptW>; override;
    function addConcept : TFhirValueSetComposeIncludeConceptW; override;
    function hasFilters : boolean; override;
    function hasValueSets : boolean; override;
    function filterCount : integer; override;
    function conceptCount : integer; override;
    function filters : TFslList<TFhirValueSetComposeIncludeFilterW>; override;
    function addFilter : TFhirValueSetComposeIncludeFilterW; override;        
    procedure addValueSet(value : String); override;
  end;

  { TFHIRValueSet5 }

  TFHIRValueSet5 = class (TFHIRValueSetW)
  private
    FExp : TFhirValueSetExpansionW;
    function vs : TFhirValueSet;
  public
    destructor Destroy; override;
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function getURL : String; override;
    procedure setUrl(value : String); override;
    function getName : String; override;
    procedure setName(value : String); override;
    function getVersion : String; override;
    procedure setVersion(value : String); override;
    function getVersionAlgorithm : TFHIRVersionAlgorithm; override;
    function getDescription : String; override;
    procedure setDescription(value : String); override;
    function checkCompose(place, role : String) : boolean; override;
    function getComposeExtensions : TFslList<TFHIRExtensionW>; override;
    function checkExpansion(place, role : String) : boolean; override;
    function imports : TArray<String>; override;
    function includes : TFslList<TFhirValueSetComposeIncludeW>; override;
    function excludes : TFslList<TFhirValueSetComposeIncludeW>; override;
    procedure clearDefinition; override;
    procedure clearDefinitionExtensions(exemptUrls : TStringArray); override;
    function hasExpansion : boolean; override;
    function expansion : TFhirValueSetExpansionW; override;
    function forceExpansion : TFhirValueSetExpansionW; override;

    function getStatus: TPublicationStatus; override;
    procedure setStatus(Value: TPublicationStatus); override;
    function getDate: TFslDateTime; override;
    procedure setDate(Value: TFslDateTime); override;
    function excludeInactives : boolean; override;
    function addInclude : TFhirValueSetComposeIncludeW; override;
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(value : String); override;   
    function getTitle : String; override;
    procedure setTitle(value : String); override;
    function source : String; override;
    function findContains(systemUri, version, code : String) : TFhirValueSetExpansionContainsW; override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFHIRLookupOpRequest5 }

  TFHIRLookupOpRequest5 = class (TFHIRLookupOpRequestW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    procedure loadCoding; override;
    function coding : TFHIRCodingW; override;
    function propList : TArray<String>; override;
    function displayLanguage : String; override;
  end;

  TFHIRLookupOpRespSubProperty5 = class (TFHIRLookupOpRespSubPropertyW)
  public
    function getDescription: string; override;
    procedure setDescription(Value: string); override;
    function getValue: String; override;
    procedure setValue(Value: String); override;
  end;

  { TFHIRLookupOpRespProperty5 }

  TFHIRLookupOpRespProperty5 = class (TFHIRLookupOpRespPropertyW)
  public
    function getDescription: string; override;
    procedure setDescription(Value: string); override;
    function getValue: TFHIRObject; override;
    procedure setValue(Value: TFHIRObject); override;
    function addSubProp(name : String) : TFHIRLookupOpRespSubPropertyW; override;
  end;

  TFHIRLookupOpRespDesignation5 = class (TFHIRLookupOpRespDesignationW)
  public
    function getUse: TFHIRObject; override;
    procedure setUse(Value: TFHIRObject); override;
  end;

  { TFHIRLookupOpResponse5 }

  TFHIRLookupOpResponse5 = class (TFHIRLookupOpResponseW)
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
    function getCode: String; override;
    procedure setCode(Value: String); override;
    function getSystem: String; override;
    procedure setSystem(Value: String); override;
    function getDisplay: String; override;
    procedure setDisplay(Value: String); override;    
    function getIsAbstract: boolean; override;
    procedure setIsAbstract(Value: boolean); override;
  end;

  { TFhirCodeSystemConceptProperty5 }

  TFhirCodeSystemConceptProperty5 = class (TFhirCodeSystemConceptPropertyW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : String; override;
    function value : TFHIRObject; override;
  end;

  { TFhirCodeSystemConceptDesignation5 }

  TFhirCodeSystemConceptDesignation5 = class (TFhirCodeSystemConceptDesignationW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function language : String; override;
    function useGen : String; override;
    function use : TFHIRCodingW; override;
    function hasUse : boolean; override;
    function value : String; override;
    function valueElement : TFHIRPrimitiveW; override;
  end;

  { TFhirCodeSystemConcept5 }

  TFhirCodeSystemConcept5 = class (TFhirCodeSystemConceptW)
  private
    FCodeSystem : TFHIRCodeSystem; // not owned
    function c : TFhirCodeSystemConcept;
  public
    constructor Create(elem : TFHIRObject; cs : TFHIRCodeSystem);
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : String; override;
    function display : String; override;
    function displayElement : TFHIRPrimitiveW; override;
    function definition : String; override;
    function conceptList : TFhirCodeSystemConceptListW; override;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; override;
    function conceptCount : integer; override;
    function hasConcepts : boolean; override;    
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; override;
    function designationCount : integer; override;
    function designations : TFslList<TFhirCodeSystemConceptDesignationW>; override;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; override;
    function displayTag(tag : String) : String; override;
    procedure setDisplayTag(tag, value : String); override;
    function getCode(code : String) : TFhirCodeSystemConceptW; override;
    function itemWeight : String; override;
  end;

  { TFhirCodeSystemProperty5 }

  TFhirCodeSystemProperty5 = class (TFhirCodeSystemPropertyW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : String; override;
    function uri : String; override;
    function type_ : TFhirCodeSystemPropertyType; override;
  end;

  { TFhirCodeSystem5 }

  TFhirCodeSystem5 = class (TFhirCodeSystemW)
  private
    function cs : TFhirCodeSystem;
    function hasLanguage(cc: TFhirCodeSystemConcept; langs: THTTPLanguageList
      ): boolean;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function getURL : String; override;
    function getName : String; override;
    function getVersion : String; override;
    function getVersionAlgorithm : TFHIRVersionAlgorithm; override;
    function getDescription : String; override;
    procedure setDate(Value: TFslDateTime); override;
    procedure setDescription(Value: String); override;
    procedure setName(Value: String); override;
    procedure setStatus(Value: TPublicationStatus); override;
    procedure setUrl(Value: String); override;
    procedure setVersion(Value: String); override;
    function GetCaseSensitive: boolean; override;
    function getContent: TFhirCodeSystemContentMode; override;
    procedure setContent(Value: TFhirCodeSystemContentMode); override;
    function getCount: integer; override;
    procedure setCount(Value: integer); override;

    function copyright : String; override;
    function language : String; override;
    function valueSet : String; override;
    function supplements : String; override;

    function properties : TFslList<TFhirCodeSystemPropertyW>;  override;
    // this is special because it's owned
    function conceptList : TFhirCodeSystemConceptListW; override;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; override;
    function conceptCount : integer; override;
    function hasConcepts : boolean; override;    
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; override;

    function isInactive(c : TFhirCodeSystemConceptW) : boolean; override;
    function codeStatus(c : TFhirCodeSystemConceptW) : String; override;
    function isAbstract(c : TFhirCodeSystemConceptW) : boolean; override;
    function isDeprecated(c : TFhirCodeSystemConceptW) : boolean; override;
    function getParents(c : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptListW; override;
    function getChildren(c : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptListW; override;
    function getCode(code : String) : TFhirCodeSystemConceptW; override;

    function getDate: TFslDateTime; override;
    function getStatus: TPublicationStatus; override;
    function buildImplicitValueSet : TFHIRValueSetW; override;
    function hasAnyDisplays(langs : THTTPLanguageList) : boolean; override;
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function getTitle : String; override;
    procedure setTitle(value : String); override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFhirConceptMapGroupElementDependsOn5 }

  TFhirConceptMapGroupElementDependsOn5 = class (TFhirConceptMapGroupElementDependsOnW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function property_ : String; override;
    function system_ : String; override;
    function value : String; override;
    function display : String; override;
  end;

  { TFhirConceptMapGroupElementTarget5 }

  TFhirConceptMapGroupElementTarget5 = class (TFhirConceptMapGroupElementTargetW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code: String; override;
    function equivalence : TFHIRConceptEquivalence; override;
    function comments : String; override;
    function products : TFslList<TFhirConceptMapGroupElementDependsOnW>; override;
  end;

  { TFhirConceptMapGroupElement5 }

  TFhirConceptMapGroupElement5 = class (TFhirConceptMapGroupElementW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code: String; override;
    function targets : TFslList<TFhirConceptMapGroupElementTargetW>; override;
    function targetCount : integer; override;
    function addTarget(code : String; eq : TFHIRConceptEquivalence) : TFhirConceptMapGroupElementTargetW; override;
  end;

  { TFhirConceptMapGroup5 }

  TFhirConceptMapGroup5 = class (TFhirConceptMapGroupW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function elements : TFslList<TFhirConceptMapGroupElementW>; override;
    function addElement(code : String) : TFhirConceptMapGroupElementW; override;
    function source : String; override;
    function target : String; override;
  end;

  { TFhirConceptMap5 }

  TFhirConceptMap5 = class (TFhirConceptMapW)
  private
    function cm : TFhirConceptMap;
  protected
    function getVersion: String; override;
    function getVersionAlgorithm : TFHIRVersionAlgorithm; override;
    procedure setVersion(Value: String); override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function getURL: String; override;
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
    function getTitle : String; override;
    procedure setTitle(value : String); override;
    function sourceDesc : String; override;
    function targetDesc : String; override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFHIRMeta5 }

  TFHIRMeta5 = class (TFHIRMetaW)
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

  { TFHIRAuditEvent5 }

  TFHIRAuditEvent5 = class (TFhirAuditEventW)
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
    procedure participantIp(ip : String); override;
    procedure participantId(system, value, alt, name : String); override;
    function dateTime : TFslDateTime; override;
  end;

  { TFHIRSubscription5 }

  TFHIRSubscription5 = class (TFHIRSubscriptionW)
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

  { TFHIRSubscriptionTopic5 }

  TFHIRSubscriptionTopic5 = class (TFHIRSubscriptionTopicW)
  private
//    function sub : TFhirSubscriptionTopic;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
  end;

  { TFhirObservationComponent5 }

  TFhirObservationComponent5 = class (TFhirObservationComponentW)
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

  { TFhirObservation5 }

  TFhirObservation5 = class (TFhirObservationW)
  private
    function obs : TFHIRObservation;
  protected
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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

  { TFHIRQuantity5 }

  TFHIRQuantity5 = class (TFHIRQuantityW)
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

  { TFHIRPeriod5 }

  TFHIRPeriod5 = class (TFHIRPeriodW)
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

  { TFHIRAttachment5 }

  TFHIRAttachment5 = class (TFHIRAttachmentW)
  private
    function att : TFhirAttachment;
  protected
    function GetContentType: String; override;
    function GetData: TBytes; override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function renderText : String; override;
  end;

  TFHIRSubsumesOpRequest5 = class (TFHIRSubsumesOpRequestW)
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

  TFHIRSubsumesOpResponse5 = class (TFHIRSubsumesOpResponseW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    function getOutcome: String; override;
    procedure setOutcome(Value: String); override;
  end;

  { TFHIRGroupCharacteristic5 }

  TFHIRGroupCharacteristic5 = class (TFHIRGroupCharacteristicW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : TFhirCodeableConceptW; override;
    function value : TFhirCodeableConceptW; override;
  end;

  { TFHIRGroup5 }

  TFHIRGroup5 = class (TFHIRGroupW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function name : String; override;
    function hasMembers : boolean; override;
    function hasCharacteristics : boolean; override;
    function characteristics : TFslList<TFHIRGroupCharacteristicW>; override;
  end;

  { TFhirPatient5 }

  TFhirPatient5 = class (TFhirPatientW)
  protected
    function pat : TFHIRPatient;
    function GetActive: boolean; override;
    procedure SetActive(const Value: boolean); override;
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

  { TFhirEncounter5 }

  TFhirEncounter5 = class (TFhirEncounterW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function patientId : String; override;
    function summary : String; override;
  end;

  TFHIRStatsOpResponse5 = class (TFHIRStatsOpResponseW)
  public
    procedure addObs(obs : TFHIRResourceV); override;
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
  end;

  { TFHIRNamingSystem5 }

  TFHIRNamingSystem5 = class (TFHIRNamingSystemW)
  private
    function nm : TFHIRNamingSystem;
  protected
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
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
    function getTitle : String; override;
    procedure setTitle(value : String); override;
    function getVersion: String; override;
    function getVersionAlgorithm : TFHIRVersionAlgorithm; override;
    procedure setVersion(Value: String); override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getUri : String; override;
    function hasOid(oid : String) : boolean; override;
  end;

  { TFHIRStructureMap5 }

  TFHIRStructureMap5 = class (TFHIRStructureMapW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function url : String; override;
  end;

  { TFHIRConsent5 }

  TFHIRConsent5 = class (TFHIRConsentW)
  private
    function consent : TFHIRConsent;
  protected
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function GetActive: boolean; override;
    function GetPatient: String; override;
    function GetDateTime: TFslDateTime; override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function listProvisions : TFslList<TFhirConsentProvisionW>; override;
  end;

  { TFHIREventDefinition5 }

  TFHIREventDefinition5 = class (TFHIREventDefinitionW)
  private
    function ed : TFHIREventDefinition;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function url : String;
    function language : String; override;
    function triggerType : TTriggerType; override;
    function expression : String; override;
    function dataType : String; override;
  end;

  { TFhirTerminologyCapabilities5 }

  TFhirTerminologyCapabilities5 = class (TFhirTerminologyCapabilitiesW)
  private
    function tc : TFhirTerminologyCapabilities;
  protected
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
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
    function getTitle : String; override;
    procedure setTitle(value : String); override;
    function getVersion: String; override;
    procedure setVersion(Value: String); override;
    function getKind: TCapabilityStatementKind; override;
    procedure setKind(Value: TCapabilityStatementKind); override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    procedure contact(kind : TContactType; value : String); override;
    procedure systemUri(url : String); override;
    procedure systemVersion(url : String); override;
    procedure addExpansionParameter(code, doco : String); override;
  end;

  { TFHIRTestScript5 }

  TFHIRTestScript5 = class (TFHIRTestScriptW)
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
    function getVersionAlgorithm : TFHIRVersionAlgorithm; override;
    function getDescription: String; override;
    function getDate: TFslDateTime; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function getTitle : String; override;
    procedure setTitle(value : String); override;
    procedure setDate(Value: TFslDateTime); override;
    procedure setUrl(Value: String); override;
    procedure setVersion(Value: String); override;
    procedure setName(Value: String); override;
    procedure setStatus(Value: TPublicationStatus); override;
    procedure setDescription(Value: String); override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;


    function getContext: String; override;
  end;

  { TFhirProvenance5 }

  TFhirProvenance5 = class (TFhirProvenanceW)
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

  { TFhirImmunization5 }

  TFhirImmunization5 = class (TFhirImmunizationW)
  private
    function imm : TFhirImmunization;
  protected
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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function code(systemUri : String) : String; override;
    function hasCode(systemUri, code : String) : boolean; override;
    procedure setCodeBySystem(systemUri : String; code : String); override;
  end;

implementation

uses
  fhir5_utilities;

function interpretVersionAlgorithm(dt : TFhirDataType) : TFHIRVersionAlgorithm;
var
  s : String;
  c : TFHIRCoding;         
  i : integer;
begin
  if (dt = nil) then
    s := ''
  else if (dt.fhirType = 'Coding') then
  begin
    c := dt as TFHIRCoding;
    s := c.code;
  end
  else
    s := dt.primitiveValue;  
  i := StringArrayIndexOf(CODES_TFHIRVersionAlgorithm, s);
  if i < 0 then
    result := vaUnknown
  else
    result := TFHIRVersionAlgorithm(i);
end;

{ TFHIRPrimitive5 }

function TFHIRPrimitive5.GetAsString: String;
begin
  result := (FElement as TFHIRPrimitiveType).StringValue;
end;

procedure TFHIRPrimitive5.SetAsString(value: String);
begin
  (FElement as TFHIRPrimitiveType).StringValue := value;
end;

function TFHIRPrimitive5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

{ TFhirOperationOutcome5 }

procedure TFhirOperationOutcome5.addIssue(issue: TFhirOperationOutcomeIssueW; free : boolean);
begin
  (Fres as TFhirOperationOutcome).issueList.Add((issue.Element as TFhirOperationOutcomeIssue).link);
  if free then
    issue.free;
end;

procedure TFhirOperationOutcome5.addIssueNoId(level: TIssueSeverity; cause: TFHIRIssueType; path, message: String; code : TOpIssueCode; addIfDuplicate : boolean);
var
  iss : TFhirOperationOutcomeIssue;
begin
  if (message = '') then
    raise EFslException.Create('Attempt to create an issue with no message');
  if (cause = itNull) then
    raise EFslException.Create('Attempt to create an issue with no cause');

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
  iss.expressionList.Add(path);
end;

procedure TFhirOperationOutcome5.addIssue(level: TIssueSeverity;
  cause: TFhirIssueType; path, msgId, message: String; code: TOpIssueCode;
  addIfDuplicate: boolean);
var
  iss : TFhirOperationOutcomeIssue;
begin
  if (message = '') then
    raise EFslException.Create('Attempt to create an issue with no message');
  if (cause = itNull) then
    raise EFslException.Create('Attempt to create an issue with no cause');

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
  iss.expressionList.Add(path);
  iss.addExtension('http://hl7.org/fhir/StructureDefinition/operationoutcome-message-id', msgid);
end;

procedure TFhirOperationOutcome5.addDiagsIssue(message: string);
var
  iss : TFhirOperationOutcomeIssue;
begin
  iss := (Fres as TFhirOperationOutcome).issueList.Append;
  iss.code := IssueTypeInformational;
  iss.severity := IssueSeverityInformation;
  iss.diagnostics := message;
end;

function TFhirOperationOutcome5.code: TFhirIssueType;
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

function TFhirOperationOutcome5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirOperationOutcome5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirOperationOutcome5.hasErrors: boolean;
begin
  result := (Fres as TFhirOperationOutcome).hasErrors;
end;

function TFhirOperationOutcome5.hasIssues: boolean;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  result := not op.issueList.IsEmpty;
end;

function TFhirOperationOutcome5.hasText: boolean;
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

function TFhirOperationOutcome5.issueCount: integer;
begin
  result := (resource as TFhirOperationOutcome).issueList.Count;
end;

function TFhirOperationOutcome5.issues: TFslList<TFhirOperationOutcomeIssueW>;
var
  iss : TFhirOperationOutcomeIssue;
begin
  result := TFslList<TFhirOperationOutcomeIssueW>.Create;
  for iss in (resource as TFhirOperationOutcome).issueList do
    result.Add(TFHIROperationOutcomeIssue5.Create(iss.Link));
end;

function TFhirOperationOutcome5.rule(level: TIssueSeverity; source: String; typeCode: TFhirIssueType; path: string; test: boolean; msg: string): boolean;
begin
  result := (resource as TFhirOperationOutcome).rule(ISSUE_SEVERITY_MAP2[level], source, ExceptionTypeTranslations[typeCode], path, test, msg);
end;

procedure TFhirOperationOutcome5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFhirOperationOutcome5.severity: TIssueSeverity;
begin
  if (resource as TFhirOperationOutcome).issueList.Count > 0 then
    result := ISSUE_SEVERITY_MAP[(resource as TFhirOperationOutcome).issueList[0].severity]
  else
    result := isFatal;
end;

function TFhirOperationOutcome5.text: String;
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

{ TFHIRBundle5 }

procedure TFHIRBundle5.addEntries(bnd: TFHIRResourceV);
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  bundle.entryList.AddAll(b.entryList);
end;

procedure TFHIRBundle5.addEntry(bnd: TFhirBundleEntryW; first: boolean);
begin
  if first then
    bundle.entryList.InsertItem(0, bnd.element.link as TFHIRBundleEntry)
  else
    bundle.entryList.AddItem(bnd.element.link as TFHIRBundleEntry);
end;

procedure TFHIRBundle5.addEntry(url: String; bnd: TFhirResourceV);
var
  e : TFHIRBundleEntry;
begin
  e := bundle.entryList.Append;
  e.fullUrl := url;
  e.resource := bnd as TFhirResource;
end;

function TFHIRBundle5.addEntry: TFhirBundleEntryW;
begin
  result := TFhirBundleEntry5.create(bundle.entryList.append.link);
end;

function TFHIRBundle5.bundle: TFhirBundle;
begin
  result := resource as TFHIRBundle;
end;

function TFHIRBundle5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

procedure TFHIRBundle5.clearLinks;
begin
  bundle.link_List.Clear;
end;

function TFHIRBundle5.count(rtype: String): Integer;
var
  be : TFhirBundleEntry;
begin
  result := 0;
  for be in bundle.entryList do
    if (be.resource <> nil) and ((rtype = '') or (rtype = be.resource.fhirType)) then
      inc(result);
end;

function TFHIRBundle5.entries: TFslList<TFhirBundleEntryW>;
var
  be : TFHIRBundleEntry;
begin
  result := TFslList<TFhirBundleEntryW>.Create;
  try
    for be in bundle.entryList do
      result.Add(TFhirBundleEntry5.create(be.Link));
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRBundle5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRBundle5.getLastUpdated: TFslDateTime;
begin
  if bundle.meta <> nil then
    result := TFslDateTime.makeNull
  else
    result := bundle.meta.lastUpdated;
end;

function TFHIRBundle5.getLink(rel: String): String;
begin
  result := bundle.Links[rel];
end;

procedure TFHIRBundle5.listLinks(links: TFslStringDictionary);
var
  bl : TFhirBundleLink;
begin
  links.Clear;
  for bl in bundle.link_List do
    links.AddOrSetValue(CODES_TFhirLinkRelationTypesEnum[bl.relation], bl.url);
end;

function TFHIRBundle5.moveToFirst(res: TFhirResourceV): TFhirBundleEntryW;
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
  result := TFHIRBundleEntry5.Create(bundle.entryList[0].Link);
end;

function TFHIRBundle5.next(bnd: TFHIRResourceV): String;
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  result := b.Links['next'];
end;

procedure TFHIRBundle5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRBundle5.setLastUpdated(Value: TFslDateTime);
begin
  if bundle.meta = nil then
    bundle.meta := TFHIRMeta.Create;
  bundle.meta.lastUpdated := value;
end;

procedure TFHIRBundle5.setLink(rel: String; const Value: String);
begin
  bundle.Links[rel] := value;
end;

procedure TFHIRBundle5.setTimestamp(Value: TFslDateTime);
begin
  bundle.timestamp := value;
end;

procedure TFHIRBundle5.setTotal(Value: integer);
begin
  bundle.total := inttostr(value);
end;

procedure TFHIRBundle5.setType(value: TBundleType);
begin
  bundle.type_ := MAP_TFHIRBundleType[value];
end;

function TFHIRBundle5.title: String;
begin
  result := BUNDLE_TYPE_TITLE[bundle.type_];
end;

function TFHIRBundle5.getTimestamp: TFslDateTime;
begin
  if bundle.timestampElement <> nil then
    result := bundle.timestamp
  else if bundle.meta <> nil then
    result := bundle.meta.lastUpdated
  else
    result := TFslDateTime.makeNull;
end;

function TFHIRBundle5.getTotal: integer;
begin
  result := StrToIntDef(bundle.total, -1);
end;

function TFHIRBundle5.getType: TBundleType;
begin
  result := MAP_TFHIRBundleTypeR[bundle.type_];
end;

{ TFHIROperationOutcomeIssue5 }

function TFHIROperationOutcomeIssue5.display: String;
var
  i : TFHIROperationOutcomeIssue;
begin
  i := issue;
  result := i.diagnostics;
  if (i.details <> nil) and (i.details.text <> '') then
    result := i.details.text;
end;

procedure TFHIROperationOutcomeIssue5.addCode(systemUri, code: String);
begin
  issue.details.addCoding(systemUri, '', code, '');
end;

function TFHIROperationOutcomeIssue5.issue: TFHIROperationOutcomeIssue;
begin
  result := Element as TFHIROperationOutcomeIssue;
end;

function TFHIROperationOutcomeIssue5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIROperationOutcomeIssue5.severity: TIssueSeverity;
begin
  result := ISSUE_SEVERITY_MAP[issue.severity];
end;

function TFHIROperationOutcomeIssue5.getDiagnostics: String;
begin
  result := issue.diagnostics;
end;

procedure TFHIROperationOutcomeIssue5.setDiagnostics(Value: String);
begin
  issue.diagnostics := value;
end;


{ TFHIRCapabilityStatement5 }

procedure TFHIRCapabilityStatement5.addInstantiates(url: String);
begin
  statement.instantiatesList.Append.value := url;
end;

procedure TFHIRCapabilityStatement5.addTxFeature(version: String);
var
  ext1, ext2, ext3 : TFHIRExtension;
begin
  ext1 := statement.addExtension('http://hl7.org/fhir/uv/application-feature/StructureDefinition/feature');
  ext2 := ext1.addExtension('definition');
  ext2.value := TFHIRCanonical.create('http://hl7.org/fhir/uv/tx-tests/FeatureDefinition/test-version');
  ext2 := ext1.addExtension('value');
  ext2.value := TFHIRCode.create(version);
  ext2 := ext1.addExtension('qualifier');
  ext3 := ext2.addExtension('name');
  ext3.value := TFHIRCode.create('mode');
  ext3 := ext2.addExtension('value');
  ext3.value := TFHIRCode.create('tx.fhir.org');  

  ext1 := statement.addExtension('http://hl7.org/fhir/uv/application-feature/StructureDefinition/feature');
  ext2 := ext1.addExtension('definition');
  ext2.value := TFHIRCanonical.create('http://hl7.org/fhir/uv/tx-ecosystem/FeatureDefinition/CodeSystemAsParameter');
  ext2 := ext1.addExtension('value');
  ext2.value := TFHIRBoolean.create(true);
end;

procedure TFHIRCapabilityStatement5.addOperation(name, url: String);
var
  t : TFhirCapabilityStatementRestResourceOperation;
begin
  t := statement.restList[0].operationList.append;
  t.name := name;
  t.definition := url;
end;

function TFHIRCapabilityStatement5.addResource(code: String): TFhirCapabilityStatementRestResourceW;
begin
  result := TFhirCapabilityStatementRestResource5.create(statement.restList[0].resourceList.append.link);
  result.code := code;
end;

procedure TFHIRCapabilityStatement5.addSmartExtensions(authorize, token, register, manage: String; caps: array of String
  );
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

procedure TFHIRCapabilityStatement5.contact(kind: TContactType; value: String);
var
  c : TFhirContactPoint;
  ct : TFhirConformanceContact;
begin
  ct := statement.contactList.Append;
  c := ct.telecomList.Append;
  c.system := MAP_TContactType[kind];
  c.value := 'http://healthintersections.com.au/';
end;

procedure TFHIRCapabilityStatement5.defineFeatures(features: TFslList<TFHIRFeature>);
begin
end;

function TFHIRCapabilityStatement5.getURL: String;
begin
  result := statement.url;
end;

function TFHIRCapabilityStatement5.hasFormat(fmt: String): boolean;
begin
  result := statement.formatList.hasCode(fmt);
end;

function TFHIRCapabilityStatement5.hasRest: boolean;
begin
  result := statement.restList.Count > 0;
end;

function TFHIRCapabilityStatement5.hasSecurity(system, code: String): boolean;
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

procedure TFHIRCapabilityStatement5.impl(url, desc: String);
begin
  if statement.implementation_ = nil then
    statement.implementation_ := TFhirCapabilityStatementImplementation.Create;
  statement.implementation_.description := desc;
  statement.implementation_.url := url;
end;

procedure TFHIRCapabilityStatement5.listSearchParams(name: String; list: TFslList<TFHIRSearchParamDefinitionW>);
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
          list.Add(TFHIRSearchParamDefinition5.create(sp.Link))
      else
      begin
        for rr in r.resourceList do
        begin
          if CODES_TFhirResourceTypesEnum[rr.type_] = name then
            for sp in rr.searchParamList do
              list.Add(TFHIRSearchParamDefinition5.create(sp.Link))
        end;
      end;
    end;
  end;
end;

procedure TFHIRCapabilityStatement5.listTypes(interactions: TFHIRInteractions; names: TStrings);
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

procedure TFHIRCapabilityStatement5.readSmartExtension(var authorize, token, register: String);
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

procedure TFHIRCapabilityStatement5.setUrl(Value: String);
begin
  statement.url := value;
end;

function TFHIRCapabilityStatement5.getName : String;
begin
  result := statement.Name;
end;

procedure TFHIRCapabilityStatement5.setName(value : String);
begin
  statement.Name := value;
end;

function TFHIRCapabilityStatement5.getTitle : String;
begin
  result := statement.Title;
end;

procedure TFHIRCapabilityStatement5.setTitle(value : String);
begin
  statement.Title := value;
end;

function TFHIRCapabilityStatement5.getVersion : String;
begin
  result := statement.Version;
end;

procedure TFHIRCapabilityStatement5.setVersion(value : String);
begin
  statement.Version := value;
end;

procedure TFHIRCapabilityStatement5.software(name, version, release: String);
begin
  if statement.software = nil then
    statement.software := TFhirCapabilityStatementSoftware.Create;
  statement.software.name := name;
  statement.software.version := version;
  statement.software.releaseDate := TFslDateTime.fromHL7(release);
end;

function TFHIRCapabilityStatement5.getDescription : String;
begin
  result := statement.Description;
end;

function TFHIRCapabilityStatement5.getFhirVersion: string;
begin
  result := CODES_TFhirFHIRVersionEnum[statement.fhirVersion];
end;

procedure TFHIRCapabilityStatement5.setDescription(value : String);
begin
  statement.Description := value;
end;

procedure TFHIRCapabilityStatement5.setFhirVersion(Value: string);
begin
  statement.fhirVersion := TFhirFHIRVersionEnum(StringArrayIndexOfSensitive(CODES_TFhirFHIRVersionEnum, value));
end;

function TFHIRCapabilityStatement5.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[statement.Status];
end;

procedure TFHIRCapabilityStatement5.setStatus(Value: TPublicationStatus);
begin
  statement.Status := MAP_TPublicationStatus[value];
end;

procedure TFHIRCapabilityStatement5.fmt(mt: String);
begin
  statement.formatList.Append.value := mt;
end;

function TFHIRCapabilityStatement5.getDate: TFslDateTime;
begin
  result := statement.Date;
end;

procedure TFHIRCapabilityStatement5.setDate(Value: TFslDateTime);
begin
  statement.Date := value;
end;


procedure TFHIRCapabilityStatement5.standardServer(ts, ws, pv, cv, iv: String; transactions, search, history : boolean);
//var
//  ext : TFhirExtension;
begin
  if statement.restList.isEmpty then
    statement.restList.append.mode := RestfulCapabilityModeServer;
  if (ws <> '') then
    statement.restList[0].addExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-websocket', ws);
  if transactions then
    statement.restList[0].interactionList.Append.code := SystemRestfulInteractionTransaction;
  if search then
    statement.restList[0].interactionList.Append.code := SystemRestfulInteractionSearchSystem;
  if history then
    statement.restList[0].interactionList.Append.code := SystemRestfulInteractionHistorySystem;
  statement.text := TFhirNarrative.Create;
  statement.text.status := NarrativeStatusGenerated;
  if (ts <> '') then
    statement.instantiatesList.AddItem(TFHIRCanonical.Create(ts));
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

function TFHIRCapabilityStatement5.statement: TFhirCapabilityStatement;
begin
  result := FRes as TFHIRCapabilityStatement;
end;

function TFHIRCapabilityStatement5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRCapabilityStatement5.supportsType(name: String; interaction: TFHIRInteraction): boolean;
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

function TFHIRCapabilityStatement5.getKind: TCapabilityStatementKind;
begin
  case statement.kind of
    CapabilityStatementKindInstance : result := cskInstance;
    CapabilityStatementKindCapability : result := cskCapability;
    CapabilityStatementKindRequirements : result := cskRequirements;
  else
    result := cskNull;
  end;
end;

function TFHIRCapabilityStatement5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRCapabilityStatement5.setKind(Value: TCapabilityStatementKind);
begin
  case value of
    cskInstance : statement.kind := CapabilityStatementKindInstance;
    cskCapability : statement.kind := CapabilityStatementKindCapability;
    cskRequirements : statement.kind := CapabilityStatementKindRequirements;
  else
    statement.kind := CapabilityStatementKindNull;
  end;
end;

procedure TFHIRCapabilityStatement5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRCapabilityStatement5.getAcceptUnknown: TCapabilityStatementAcceptUnknown;
begin
  result := csauNull;
end;

procedure TFHIRCapabilityStatement5.setAcceptUnknown(const Value: TCapabilityStatementAcceptUnknown);
begin
end;


{ TFhirParametersParameter5 }

function TFhirParametersParameter5.addParam(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter5.Create(parameter.partList.Append.link);
  TFhirParametersParameter5(result).parameter.name := name;
  PartList.Add(result);
end;

procedure TFhirParametersParameter5.addParam(name: String; value: TFHIRObject);
begin
  parameter.AddParameter(name).value := value as TFHIRDataType;
end;

procedure TFhirParametersParameter5.addParamBool(name: String; value: boolean);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = LCBooleanToString(value)) then
      exit;
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFhirParametersParameter5.addParamCode(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFhirParametersParameter5.addParamStr(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

procedure TFhirParametersParameter5.addParamUri(name, value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFhirParametersParameter5.addParamCanonical(name, value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCanonical.Create(value);
end;

function TFhirParametersParameter5.getParameterParameter(name: String): TFhirParametersParameterW;
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

function TFhirParametersParameter5.getResource: TFHIRResourceV;
begin
  result := parameter.resource;
end;

function TFhirParametersParameter5.getResourceParameter(name: String): TFHIRResourceV;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter5(t).parameter.resource);
end;

function TFhirParametersParameter5.getStringParameter(name: String): String;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := '';
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter5(t).parameter.value.primitiveValue);
end;

function TFhirParametersParameter5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirParametersParameter5.getValue: TFHIRObject;
begin
  result := parameter.value;
end;

function TFhirParametersParameter5.hasResource: boolean;
begin
  result := parameter.resource <> nil;
end;

function TFhirParametersParameter5.hasValue: boolean;
begin
  result := parameter.value <> nil;
end;

function TFhirParametersParameter5.name: String;
begin
  result := parameter.name;
end;

function TFhirParametersParameter5.parameter: TFhirParametersParameter;
begin
  result := Element as TFhirParametersParameter;
end;

procedure TFhirParametersParameter5.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.partList do
    FList.Add(TFhirParametersParameter5.Create(t.Link));
end;

procedure TFhirParametersParameter5.setResource(Value: TFHIRResourceV);
begin
  parameter.resource := value as TFhirResource;
end;

procedure TFhirParametersParameter5.setValue(Value: TFHIRObject);
begin
  parameter.value := value as TFHIRDataType;
end;

function TFhirParametersParameter5.valueString: String;
begin
  if (parameter.value = nil) or (not parameter.value.isPrimitive) then
    result := ''
  else
    result := parameter.value.primitiveValue;
end;

{ TFHIRParameters5 }

function TFHIRParameters5.addParam(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter5.Create(parameter.parameterList.Append.link);
  TFhirParametersParameter5(result).parameter.name := name;
  ParameterList.Add(result);
end;

procedure TFHIRParameters5.addParam(name: String; value: TFHIRObject);
begin
  parameter.AddParameter(name).value := value as TFHIRDataType;
end;

procedure TFHIRParameters5.addParamBool(name: String; value: boolean);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = LCBooleanToString(value)) then
      exit;
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFHIRParameters5.addParamCode(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFHIRParameters5.addParamUri(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFHIRParameters5.addParamCanonical(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCanonical.Create(value);
end;

procedure TFHIRParameters5.addParamStr(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

function TFHIRParameters5.bool(name: String): boolean;
begin
  result := parameter.bool[name];
end;

function TFHIRParameters5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRParameters5.getParameter(name: String): TFhirParametersParameterW;
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

function TFHIRParameters5.names: String;
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
  end;
end;

function TFHIRParameters5.has(name: String): boolean;
begin
  result := parameter.hasParameter(name);
end;

function TFHIRParameters5.obj(name: String): TFHIRObject;
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

function TFHIRParameters5.parameter: TFhirParameters;
begin
  result := Resource as TFhirParameters;
end;

procedure TFHIRParameters5.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.parameterList do
    FList.Add(TFhirParametersParameter5.Create(t.Link));
end;

function TFHIRParameters5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

procedure TFHIRParameters5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRParameters5.str(name: String): String;
begin
  result := parameter.str[name];
end;

{ TFHIRStructureDefinition5 }

function TFHIRStructureDefinition5.elements: TFslList<TFHIRElementDefinitionW>;
var
  ed : TFhirElementDefinition;
begin
  result := TFslList<TFHIRElementDefinitionW>.Create;
  try
    for ed in sd.snapshot.elementList do
      result.Add(TFhirElementDefinition5.create(ed.Link));
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStructureDefinition5.getDefinition(id: String; source: TElementDefinitionSourceOption): TFHIRElementDefinitionW;
var
  ed : TFhirElementDefinition;
begin
  result := nil;
  if (source in [edsSNAPSHOT, edsEITHER]) and (sd.snapshot <> nil) then
    for ed in sd.snapshot.elementList do
      if ed.id = id then
        exit(TFHIRElementDefinition5.Create(ed.Link));

  if (source in [edsDIFF, edsEITHER]) and (sd.differential <> nil) then
    for ed in sd.differential.elementList do
      if ed.id = id then
        exit(TFHIRElementDefinition5.Create(ed.Link));
end;

function TFHIRStructureDefinition5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRStructureDefinition5.kind: TStructureDefinitionKind;
begin
  case sd.kind of
    StructureDefinitionKindPrimitiveType : result := sdkPrimitive;
    StructureDefinitionKindComplexType :
      if type_ = 'Extension' then
          result := sdkExtension
        else
          result := sdkDataType;
    StructureDefinitionKindResource : result := sdkResource;
    StructureDefinitionKindLogical : result := sdkResource;
  else
    raise EFHIRException.create('unhandled value');
  end;
end;

function TFHIRStructureDefinition5.name: String;
begin
  result := sd.name;
end;

function TFHIRStructureDefinition5.sd: TFhirStructureDefinition;
begin
  result := resource as TFhirStructureDefinition;
end;

function TFHIRStructureDefinition5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

procedure TFHIRStructureDefinition5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRStructureDefinition5.type_: String;
begin
  result := sd.type_;
end;

function TFHIRStructureDefinition5.url: String;
begin
  result := sd.url;
end;

{ TFHIRSearchParamDefinition5 }

function TFHIRSearchParamDefinition5.documentation: String;
begin
  result := param.documentation;
end;

function TFHIRSearchParamDefinition5.name: String;
begin
  result := param.name;
end;

function TFHIRSearchParamDefinition5.param: TFhirCapabilityStatementRestResourceSearchParam;
begin
  result := FElement as TFhirCapabilityStatementRestResourceSearchParam;
end;

function TFHIRSearchParamDefinition5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRSearchParamDefinition5.type_: TFHIRSearchParamType;
begin
  result := MAP_SearchParamType[param.type_];
end;

{ TFhirElementDefinition5 }

function TFhirElementDefinition5.binding: TElementDefinitionBinding;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  if b = nil then
    result := edbNone
  else
    result := MAP_ELEMENT_DEFINITION_BINDING[b.strength];
end;

function TFhirElementDefinition5.defn: String;
begin
  result := edefn.definition;
end;

function TFhirElementDefinition5.edefn: TFhirElementDefinition;
begin
  result := element as TFhirElementDefinition;
end;

function TFhirElementDefinition5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirElementDefinition5.explicitTypeName: String;
begin
  result := edefn.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-explicit-type-name');
end;

function TFhirElementDefinition5.isSummary: boolean;
begin
  result := edefn.isSummary;
end;

function TFhirElementDefinition5.max: integer;
begin
  if edefn.max = '*' then
    result := MaxInt
  else
    result := StrToInt(edefn.max);
end;

function TFhirElementDefinition5.min: integer;
begin
  result := StrToInt(edefn.min);
end;

function TFhirElementDefinition5.path: String;
begin
  result := edefn.path;
end;

function TFhirElementDefinition5.typeList: TArray<String>;
var
  ed : TFhirElementDefinition;
  i : integer;
begin
  ed := edefn;
  Setlength(result, ed.type_List.Count);
  for i := 0 to ed.type_List.Count - 1 do
    result[i] := ed.type_List[i].code;
end;

function TFhirElementDefinition5.types: String;
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

function TFhirElementDefinition5.valueSet: String;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  if b <> nil then
    result := b.valueSet
  else
    result := '';
end;

{ TFHIRBundleEntry5 }

function TFHIRBundleEntry5.entry: TFhirBundleEntry;
begin
  result := element as TFhirBundleEntry;
end;

function TFHIRBundleEntry5.getRequestMethod: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := CODES_TFhirHttpVerbEnum[entry.request.method];
end;

function TFHIRBundleEntry5.getRequestUrl: String;
begin
  if entry.request = nil then
    result := ''
  else
    result :=  entry.request.url;
end;

function TFHIRBundleEntry5.getResource: TFHIRResourceV;
begin
  result :=  entry.resource;
end;

function TFHIRBundleEntry5.getResponseDate: TFslDateTime;
begin
  if entry.response = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.response.lastModified;
end;

function TFHIRBundleEntry5.getResponseStatus: String;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.status;
end;

function TFHIRBundleEntry5.getSearchMode: TFHIRBundleEntrySearchMode;
begin
  if entry.search = nil then
    result := smUnknown
  else
    result := MAP_SEARCH_MODE[entry.search.mode];
end;

function TFHIRBundleEntry5.getSearchMpiMatch: String;
begin
  if entry.search = nil then
    result := ''
  else
    result := entry.search.getExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match')
end;

function TFHIRBundleEntry5.getSearchScore: String;
begin
  if entry.search = nil then
    result := ''
  else
    result := entry.search.score;
end;

function TFHIRBundleEntry5.getURL: String;
begin
  result := entry.fullUrl;
end;

procedure TFHIRBundleEntry5.setRequestMethod(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.Create;
  entry.request.method := TFhirHttpVerbEnum(ord(StringArrayIndexOfSensitive(CODES_TFhirHttpVerbEnum, value)));
end;

procedure TFHIRBundleEntry5.setRequestUrl(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.Create;
  entry.request.url := value;
end;

procedure TFHIRBundleEntry5.setResource(Value: TFHIRResourceV);
begin
  entry.resource := value as TFHIRResource;
end;

procedure TFHIRBundleEntry5.setResponseDate(Value: TFslDateTime);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.lastModified := value;
end;

procedure TFHIRBundleEntry5.setResponseStatus(Value: String);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.status := value;
end;

procedure TFHIRBundleEntry5.setSearchMode(Value: TFHIRBundleEntrySearchMode);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.mode := MAP_SEARCH_MODE2[value];
end;

procedure TFHIRBundleEntry5.setSearchMpiMatch(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.setExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match', value)
end;

procedure TFHIRBundleEntry5.setSearchScore(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.score := value;
end;

procedure TFHIRBundleEntry5.setUrl(Value: String);
begin
  entry.fullUrl := value;
end;

function TFHIRBundleEntry5.getLink(rel: String): String;
begin
  result := entry.Links[rel];
end;

procedure TFHIRBundleEntry5.setLink(rel: String; const Value: String);
begin
  entry.Links[rel] := value;
end;

function TFHIRBundleEntry5.getrequestIfNoneExist: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.ifNoneExist;
end;

procedure TFHIRBundleEntry5.setrequestIfNoneExist(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.ifNoneExist := value;
end;

function TFHIRBundleEntry5.getrequestIfMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfMatch;
end;

function TFHIRBundleEntry5.getrequestIfModifiedSince: TFslDateTime;
begin
  if entry.request = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.request.ifModifiedSince;
end;

procedure TFHIRBundleEntry5.setrequestIfMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.IfMatch := value;
end;

procedure TFHIRBundleEntry5.setrequestIfModifiedSince(Value: TFslDateTime);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.ifModifiedSince := value;
end;

function TFHIRBundleEntry5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRBundleEntry5.getrequestIfNoneMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfNoneMatch;
end;

procedure TFHIRBundleEntry5.setrequestIfNoneMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.IfNoneMatch := value;
end;

function TFHIRBundleEntry5.getResponseETag: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.etag;
end;

procedure TFHIRBundleEntry5.setResponseETag(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.ETag := value;
end;

function TFHIRBundleEntry5.getResponseLocation: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.Location;
end;

procedure TFHIRBundleEntry5.setResponseLocation(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.Location := value;
end;

{ TFHIRValueSet5 }

function TFHIRValueSet5.addInclude: TFhirValueSetComposeIncludeW;
begin
  if vs.compose = nil then
    vs.compose := TFhirValueSetCompose.Create;
  result := TFhirValueSetComposeInclude5.Create(vs.compose.includeList.Append.Link);
end;

function TFHIRValueSet5.checkCompose(place, role: String): boolean;
begin
  result := vs.compose <> nil;
  if result then
    vs.compose.checkNoModifiers(place, role, []);
end;

function TFHIRValueSet5.getComposeExtensions: TFslList<TFHIRExtensionW>;
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
          result.add(TFHIRExtension5.Create(ext.link));
      finally
        list.free;
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValueSet5.checkExpansion(place, role: String): boolean;
begin
  result := vs.expansion <> nil;
  if result then
    vs.expansion.checkNoModifiers(place, role, []);
end;

procedure TFHIRValueSet5.clearDefinition;
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

procedure TFHIRValueSet5.clearDefinitionExtensions(exemptUrls: TStringArray);
begin
  if vs.purposeElement <> nil then
    vs.purposeElement.stripExtensions(exemptUrls);
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

destructor TFHIRValueSet5.Destroy;
begin
  FExp.free;
  inherited;
end;

function TFHIRValueSet5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRValueSet5.excludes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  if vs.compose = nil then
    result := TFslList<TFhirValueSetComposeIncludeW>.create
  else
  begin
    result := TFslList<TFhirValueSetComposeIncludeW>.create(vs.compose.excludeList.Count);
    for c in vs.compose.excludeList do
      result.Add(TFhirValueSetComposeInclude5.Create(c.Link));
  end;
end;

function TFHIRValueSet5.expansion: TFhirValueSetExpansionW;
begin
  if (FExp = nil) and (vs.expansion <> nil) then
    FExp := TFhirValueSetExpansion5.create(vs.expansion.Link);
  result := FExp;
end;

function TFHIRValueSet5.forceExpansion: TFhirValueSetExpansionW;
begin
  if (vs.expansion = nil) then
    vs.expansion := TFhirValueSetExpansion.Create;
  vs.expansion.timestamp := TFslDateTime.makeUTC;
  vs.expansion.identifier := NewGuidURN;
  vs.expansion.parameterList.Clear;
  vs.expansion.containsList.Clear;
  result := expansion;
end;

function TFHIRValueSet5.getContext: String;
begin
  result := vs.context;
end;

function TFHIRValueSet5.getDate: TFslDateTime;
begin
  result := vs.date;
end;

function TFHIRValueSet5.hasExpansion: boolean;
begin
  result := vs.expansion <> nil;
end;

function TFHIRValueSet5.excludeInactives: boolean;
begin
  result := (vs.compose.inactiveElement <> nil) and not vs.compose.inactive;
end;

function TFHIRValueSet5.imports: TArray<String>;
begin
  SetLength(result, 0);
end;

function TFHIRValueSet5.includes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  if vs.compose = nil then
    result := TFslList<TFhirValueSetComposeIncludeW>.create
  else
  begin
    result := TFslList<TFhirValueSetComposeIncludeW>.create(vs.compose.includeList.Count);
    for c in vs.compose.includeList do
      result.Add(TFhirValueSetComposeInclude5.Create(c.Link));
  end;
end;

procedure TFHIRValueSet5.setDate(Value: TFslDateTime);
begin
  vs.date := value;
end;

procedure TFHIRValueSet5.setUrl(value: String);
begin
  vs.url := value;
end;

function TFHIRValueSet5.getURL: String;
begin
  result := vs.url;
end;

procedure TFHIRValueSet5.setName(value: String);
begin
  vs.Name := value;
end;

procedure TFHIRValueSet5.setPublisher(value: String);
begin
  vs.publisher := value;
end;

function TFHIRValueSet5.getTitle: String;
begin
  result := vs.title;
end;

procedure TFHIRValueSet5.setTitle(value: String);
begin
  vs.title := value;
end;

procedure TFHIRValueSet5.setStatus(Value: TPublicationStatus);
begin
  vs.status := MAP_TPublicationStatus[value];
end;

function TFHIRValueSet5.getName: String;
begin
  result := vs.Name;
end;

function TFHIRValueSet5.getPublisher: String;
begin
  result := vs.publisher;
end;

function TFHIRValueSet5.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[vs.status];
end;

procedure TFHIRValueSet5.setDescription(value: String);
begin
  vs.Description := value;
end;

procedure TFHIRValueSet5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRValueSet5.getDescription: String;
begin
  result := vs.Description;
end;

function TFHIRValueSet5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRValueSet5.setVersion(value: String);
begin
  vs.Version := value;
end;

function TFHIRValueSet5.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  result := interpretVersionAlgorithm(vs.versionAlgorithm);
end;

function TFHIRValueSet5.source: String;
begin
  result := vs.source;
end;

function TFHIRValueSet5.findContains(systemUri, version, code: String): TFhirValueSetExpansionContainsW;
var
  cc : TFhirValueSetExpansionContains;
begin
  cc := vs.findContains(systemuri, version, code);
  if (cc) = nil then
    result := nil
  else
    result := TFhirValueSetExpansionContains5.create(cc.link);
end;

function TFHIRValueSet5.getExperimental: boolean;
begin
  result := vs.experimental;
end;

procedure TFHIRValueSet5.setExperimental(value: boolean);
begin
  vs.experimental := value;
end;

function TFHIRValueSet5.getVersion: String;
begin
  result := vs.Version;
end;

function TFHIRValueSet5.vs: TFhirValueSet;
begin
  result := Resource as TFHIRValueSet;
end;


{ TFhirValueSetComposeInclude5 }

function TFhirValueSetComposeInclude5.addConcept: TFhirValueSetComposeIncludeConceptW;
begin
  result := TFhirValueSetComposeIncludeConcept5.Create((Element as TFhirValueSetComposeInclude).ConceptList.Append.Link);
end;

function TFhirValueSetComposeInclude5.addFilter: TFhirValueSetComposeIncludeFilterW;
begin
  result := TFhirValueSetComposeIncludeFilter5.Create((Element as TFhirValueSetComposeInclude).FilterList.Append.Link);
end;

procedure TFhirValueSetComposeInclude5.addValueSet(value: String);
begin
  TFhirValueSetComposeInclude(element).valueSetList.AddItem(value);
end;

function TFhirValueSetComposeInclude5.concepts: TFslList<TFhirValueSetComposeIncludeConceptW>;
var
  i : TFhirValueSetComposeIncludeConcept;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptW>.create((Element as TFhirValueSetComposeInclude).ConceptList.Count);
  for i in (Element as TFhirValueSetComposeInclude).ConceptList do
    result.Add(TFhirValueSetComposeIncludeConcept5.Create(i.Link));
end;

function TFhirValueSetComposeInclude5.filterCount: integer;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count;
end;

function TFhirValueSetComposeInclude5.conceptCount: integer;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count;
end;

function TFhirValueSetComposeInclude5.filters: TFslList<TFhirValueSetComposeIncludeFilterW>;
var
  i : TFhirValueSetComposeIncludeFilter;
begin
  result := TFslList<TFhirValueSetComposeIncludeFilterW>.create((Element as TFhirValueSetComposeInclude).filterList.Count);
  for i in (Element as TFhirValueSetComposeInclude).filterList do
    result.Add(TFhirValueSetComposeIncludeFilter5.Create(i.Link));
end;

function TFhirValueSetComposeInclude5.hasConcepts: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count > 0;
end;

function TFhirValueSetComposeInclude5.hasFilters: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count > 0;
end;

function TFhirValueSetComposeInclude5.hasValueSets: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).valueSetList.Count > 0;
end;

procedure TFhirValueSetComposeInclude5.setSystem(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).system := value;
end;

procedure TFhirValueSetComposeInclude5.setVersion(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).version := value;
end;

function TFhirValueSetComposeInclude5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirValueSetComposeInclude5.getSystem: String;
begin
  result := (Element as TFhirValueSetComposeInclude).system;
end;

function TFhirValueSetComposeInclude5.valueSets: TArray<String>;
var
  i : integer;
begin
  SetLength(result, TFhirValueSetComposeInclude(element).valueSetList.count);
  for i := 0 to TFhirValueSetComposeInclude(element).valueSetList.count - 1 do
    result[i] :=  TFhirValueSetComposeInclude(element).valueSetList[i].value;
end;

function TFhirValueSetComposeInclude5.getVersion: String;
begin
  result := (Element as TFhirValueSetComposeInclude).version;
end;

{ TFhirValueSetComposeIncludeFilter5 }

function TFhirValueSetComposeIncludeFilter5.getOp: TFilterOperator;
begin
  result := MAP_TFilterOperator[(Element as TFhirValueSetComposeIncludeFilter).op];
end;

function TFhirValueSetComposeIncludeFilter5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirValueSetComposeIncludeFilter5.getProp: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).property_;
end;

function TFhirValueSetComposeIncludeFilter5.getValue: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).value;
end;

procedure TFhirValueSetComposeIncludeFilter5.setOp(Value: TFilterOperator);
begin
  (Element as TFhirValueSetComposeIncludeFilter).op := MAP_TFilterOperatorR[Value];

end;

procedure TFhirValueSetComposeIncludeFilter5.setProp(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).property_ := value;
end;

procedure TFhirValueSetComposeIncludeFilter5.setValue(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).value := value;
end;

{ TFhirValueSetComposeIncludeConcept5 }

function TFhirValueSetComposeIncludeConcept5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirValueSetComposeIncludeConcept5.getCode: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).code;
end;

function TFhirValueSetComposeIncludeConcept5.designations: TFslList<TFhirValueSetComposeIncludeConceptDesignationW>;
var
  item : TFhirValueSetComposeIncludeConceptDesignation;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptDesignationW>.Create;
  for item in (Element as TFhirValueSetComposeIncludeConcept).designationList do
    result.Add(TFhirValueSetComposeIncludeConceptDesignation5.create(item.Link));
end;

function TFhirValueSetComposeIncludeConcept5.getDisplay: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).display;
end;

procedure TFhirValueSetComposeIncludeConcept5.setCode(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).code := Value;
end;

procedure TFhirValueSetComposeIncludeConcept5.setDisplay(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).display := Value;
end;

function TFhirValueSetComposeIncludeConcept5.displayElement: TFHIRPrimitiveW;
begin
  if (Element as TFhirValueSetComposeIncludeConcept).displayElement = nil then
    result := nil
  else
    result := TFHIRPrimitive5.create((Element as TFhirValueSetComposeIncludeConcept).displayELement.link);
end;

function TFhirValueSetComposeIncludeConcept5.GetItemWeight: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).getExtensionString(EXT_ITEM_WEIGHT);
  if result = '' then
    result := (Element as TFhirValueSetComposeIncludeConcept).getExtensionString(EXT_ORDINAL_VALUE);
end;

procedure TFhirValueSetComposeIncludeConcept5.SetItemWeight(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).setExtensionString(EXT_ITEM_WEIGHT, value);
end;

{ TFHIRLookupOpResponse5 }

function TFHIRLookupOpResponse5.addDesignation(lang, system, code, display, value: string): TFHIRLookupOpRespDesignationW;
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
    result := TFHIRLookupOpRespDesignation5.create(p.Link);
    list.add(result);
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse5.addDesignation(lang, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.Create;
  try
    p.language := lang;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation5.create(p.Link);
    list.add(result);
  finally
    p.free;
  end;
end;

procedure TFHIRLookupOpResponse5.addExtension(name: String; value: boolean);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

procedure TFHIRLookupOpResponse5.addExtension(name, value: String);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

function TFHIRLookupOpResponse5.addProp(name: string): TFHIRLookupOpRespPropertyW;
var
  p : TFHIRLookupOpRespProperty_;
begin
  p := TFHIRLookupOpRespProperty_.Create;
  try
    p.code := name;
    (op as TFHIRLookupOpResponse).property_List.Add(p.link as TFHIRLookupOpRespProperty_);
    result := TFHIRLookupOpRespProperty5.create(p.Link);
    List.add(result); // make sure it gets cleaned up
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse5.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationResponse).asParams;
end;

function TFHIRLookupOpResponse5.getDisplay: String;
begin
  result := (op as TFHIRLookupOpResponse).display;
end;

function TFHIRLookupOpResponse5.getName: String;
begin
  result := (op as TFHIRLookupOpResponse).name;
end;

function TFHIRLookupOpResponse5.getVersion: String;
begin
  result := (op as TFHIRLookupOpResponse).version;
end;

procedure TFHIRLookupOpResponse5.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpResponse).load(params);
end;

procedure TFHIRLookupOpResponse5.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpResponse).load(params as TFhirParameters);
end;

procedure TFHIRLookupOpResponse5.setDisplay(Value: String);
begin
  (op as TFHIRLookupOpResponse).display := value;
end;

function TFHIRLookupOpResponse5.getIsAbstract: boolean;
begin
  result := (op as TFHIRLookupOpResponse).abstract;
end;

procedure TFHIRLookupOpResponse5.setIsAbstract(Value: boolean);
begin
   (op as TFHIRLookupOpResponse).abstract := value;
end;

procedure TFHIRLookupOpResponse5.setName(Value: String);
begin
  (op as TFHIRLookupOpResponse).name := value;
end;

function TFHIRLookupOpResponse5.getCode: String;
begin
  result := (op as TFHIRLookupOpResponse).code;
end;

procedure TFHIRLookupOpResponse5.setCode(Value: String);
begin
  (op as TFHIRLookupOpResponse).code := value;
end;

function TFHIRLookupOpResponse5.getSystem: String;
begin
  result := (op as TFHIRLookupOpResponse).systemUri;
end;

procedure TFHIRLookupOpResponse5.setSystem(Value: String);
begin
  (op as TFHIRLookupOpResponse).systemUri := value;
end;

procedure TFHIRLookupOpResponse5.setVersion(Value: String);
begin
  (op as TFHIRLookupOpResponse).version := value;
end;

{ TFHIRLookupOpRespDesignation5 }

function TFHIRLookupOpRespDesignation5.GetUse: TFHIRObject;
begin
  result := (obj as TFHIRLookupOpRespDesignation).use;
end;

procedure TFHIRLookupOpRespDesignation5.SetUse(Value: TFHIRObject);
begin
  (obj as TFHIRLookupOpRespDesignation).use := value as TFhirCoding;
end;

{ TFHIRLookupOpRespSubProperty5 }

function TFHIRLookupOpRespSubProperty5.GetDescription: string;
begin
  result := (obj as TFHIRLookupOpRespSubProperty).description;
end;

function TFHIRLookupOpRespSubProperty5.GetValue: String;
begin
  result := (obj as TFHIRLookupOpRespSubProperty).value.primitiveValue;
end;

procedure TFHIRLookupOpRespSubProperty5.SetDescription(Value: string);
begin
  (obj as TFHIRLookupOpRespSubProperty).description := value;
end;

procedure TFHIRLookupOpRespSubProperty5.SetValue(Value: String);
begin
  (obj as TFHIRLookupOpRespSubProperty).value := TFHIRString.create(value);
end;

{ TFHIRLookupOpRespProperty5 }

function TFHIRLookupOpRespProperty5.getDescription: string;
begin
  result := (obj as TFHIRLookupOpRespProperty_).description;
end;

function TFHIRLookupOpRespProperty5.getValue: TFHIRObject;
begin
  result := (obj as TFHIRLookupOpRespProperty_).value;
end;

procedure TFHIRLookupOpRespProperty5.setDescription(Value: string);
begin
  (obj as TFHIRLookupOpRespProperty_).description := value;
end;

procedure TFHIRLookupOpRespProperty5.setValue(Value: TFHIRObject);
begin
  (obj as TFHIRLookupOpRespProperty_).value := value as TFHIRDataType;
end;

function TFHIRLookupOpRespProperty5.addSubProp(name: String): TFHIRLookupOpRespSubPropertyW;
var
  p : TFHIRLookupOpRespSubProperty;
begin
  p := TFHIRLookupOpRespSubProperty.Create;
  try
    p.code := name;
    (obj as TFHIRLookupOpRespProperty_).subpropertyList.Add(p.link as TFHIRLookupOpRespSubProperty);
    result := TFHIRLookupOpRespSubProperty5.create(p.Link);
    List.add(result); // make sure it gets cleaned up
  finally
    p.free;
  end;
end;

{ TFHIRExtension5 }

function TFHIRExtension5.ext: TFHIRExtension;
begin
  result := (Element as TFHIRExtension);
end;

function TFHIRExtension5.renderText: String;
begin
  result := gen(ext.value);
end;

function TFHIRExtension5.valueAsCodeableConcept: TFhirCodeableConceptW;
begin
  if ext.value is TFHIRCodeableConcept then
    result := TFHIRCodeableConcept5.create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension5.valueAsCoding: TFhirCodingW;
begin
  if ext.value is TFHIRCoding then
    result := TFHIRCoding5.create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension5.valueAsPeriod: TFhirPeriodW;
begin
  if ext.value is TFHIRPeriod then
    result := TFHIRPeriod5.create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension5.valueAsQuantity: TFhirQuantityW;
begin
  if ext.value is TFHIRQuantity then
    result := TFHIRQuantity5.create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension5.valueAsIdentifier: TFhirIdentifierW;
begin
  if ext.value is TFHIRIdentifier then
    result := TFHIRIdentifier5.create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension5.valueAsAttachment: TFhirAttachmentW;
begin
  if ext.value is TFHIRAttachment then
    result := TFHIRAttachment5.create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension5.valueAsString: string;
begin
  if ext.value is TFHIRPrimitiveType then
    result := ext.value.primitiveValue
  else
    result := '';
end;

function TFHIRExtension5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRExtension5.url: String;
begin
  result := ext.url;
end;

function TFHIRExtension5.value: TFHIRObject;
begin
  result := ext.value;
end;

procedure TFHIRExtension5.setValueW(value: TFhirDataTypeW);
begin
  setValueV(value.Element);
end;

procedure TFHIRExtension5.setValueV(value: TFhirObject);
begin
  if not (value is TFHIRDataType) then
    raise EFHIRException.create('Wrong type at TFHIRExtension5.setValueV: '+value.ClassName+' ('+Codes_TFHIRVersion[value.fhirObjectVersion]);
  ext.value := (value as TFHIRDataType).link;
end;


{ TFHIRCoding5 }

function TFHIRCoding5.getCode: String;
begin
  result := (element as TFHIRCoding).code;
end;

function TFHIRCoding5.getDisplay: String;
begin
  result := (element as TFHIRCoding).display;
end;

function TFHIRCoding5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRCoding5.getSystem: String;
begin
  result := (element as TFHIRCoding).system;
end;

function TFHIRCoding5.getVersion: String;
begin
  result := (element as TFHIRCoding).version;
end;

function TFHIRCoding5.renderText: String;
begin
  result := gen(element as TFhirCoding);
end;

procedure TFHIRCoding5.setCode(Value: String);
begin
  (element as TFHIRCoding).code := value;
end;

procedure TFHIRCoding5.setDisplay(Value: String);
begin
  (element as TFHIRCoding).display := value;
end;

procedure TFHIRCoding5.setSystem(Value: String);
begin
    (element as TFHIRCoding).system := value;
end;

procedure TFHIRCoding5.setVersion(Value: String);
begin
  (element as TFHIRCoding).version := value;
end;

{ TFhirCodeSystemProperty5 }

function TFhirCodeSystemProperty5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirCodeSystemProperty5.code: String;
begin
  result := (Element as TFhirCodeSystemProperty).code;
end;

function TFhirCodeSystemProperty5.type_: TFhirCodeSystemPropertyType;
begin
  result := MAP_TFhirConceptPropertyTypeEnum[(Element as TFhirCodeSystemProperty).type_];
end;

function TFhirCodeSystemProperty5.uri: String;
begin
  result := (Element as TFhirCodeSystemProperty).uri;
end;

{ TFhirCodeSystemConceptProperty5 }

function TFhirCodeSystemConceptProperty5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirCodeSystemConceptProperty5.code: String;
begin
  result := (Element as TFhirCodeSystemConceptProperty).code;
end;

function TFhirCodeSystemConceptProperty5.value: TFHIRObject;
begin
  result := (Element as TFhirCodeSystemConceptProperty).value;
end;

{ TFhirCodeSystemConceptDesignation5 }

function TFhirCodeSystemConceptDesignation5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirCodeSystemConceptDesignation5.language: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).language;
end;

function TFhirCodeSystemConceptDesignation5.use: TFHIRCodingW;
begin
  if (Element as TFhirCodeSystemConceptDesignation).use = nil then
    result := nil
  else
    result := TFHIRCoding5.create((Element as TFhirCodeSystemConceptDesignation).use.link);
end;

function TFhirCodeSystemConceptDesignation5.hasUse: boolean;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).use <> nil;
end;

function TFhirCodeSystemConceptDesignation5.useGen: String;
begin
  result := gen((Element as TFhirCodeSystemConceptDesignation).use)
end;

function TFhirCodeSystemConceptDesignation5.value: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).value;
end;

function TFhirCodeSystemConceptDesignation5.valueElement: TFHIRPrimitiveW;
begin
  if (Element as TFhirCodeSystemConceptDesignation).valueElement = nil then
    result := nil
  else
    result := TFHIRPrimitive5.create((Element as TFhirCodeSystemConceptDesignation).valueElement.link);
end;

{ TFhirCodeSystemConcept5 }

function TFhirCodeSystemConcept5.c: TFhirCodeSystemConcept;
begin
  result := Element as TFhirCodeSystemConcept;
end;

constructor TFhirCodeSystemConcept5.Create(elem: TFHIRObject; cs: TFHIRCodeSystem);
begin
  inherited Create(elem);
  FCodeSystem := cs;
end;

function TFhirCodeSystemConcept5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirCodeSystemConcept5.code: String;
begin
  result := c.code;
end;

function TFhirCodeSystemConcept5.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept5.create((element as TFhirCodeSystemConcept).conceptList[ndx].Link, FCodeSystem);
end;

function TFhirCodeSystemConcept5.conceptCount: integer;
begin
  result := c.conceptList.Count;
end;

function TFhirCodeSystemConcept5.hasConcepts: boolean;
begin
  result := c.hasConceptList;
end;

function TFhirCodeSystemConcept5.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.Create;
    for i in (element as TFhirCodeSystemConcept).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept5.create(i.Link, FCodeSystem));
  end;
  result := FConceptList;
end;

function TFhirCodeSystemConcept5.definition: String;
begin
  result := c.definition;
end;

function TFhirCodeSystemConcept5.designationCount: integer;
begin
  result := c.designationList.Count;
end;

function TFhirCodeSystemConcept5.designations: TFslList<TFhirCodeSystemConceptDesignationW>;
var
  i : TFhirCodeSystemConceptDesignation;
begin
  result := TFslList<TFhirCodeSystemConceptDesignationW>.Create;
  for i in c.designationList do
    result.Add(TFhirCodeSystemConceptDesignation5.Create(i.Link));
end;

function TFhirCodeSystemConcept5.display: String;
begin
  result := c.display;
end;

function TFhirCodeSystemConcept5.displayElement: TFHIRPrimitiveW;
begin
  if c.displayElement = nil then
    result := nil
  else
    result := TFHIRPrimitive5.create(c.displayElement.link);
end;

function TFhirCodeSystemConcept5.displayTag(tag: String): String;
begin
  result := c.displayElement.Tags[tag];
end;

function getCodeWrapper(codeSystem : TFHIRCodesystem; list : TFhirCodeSystemConceptList; code : String) : TFhirCodeSystemConceptW;
var
  cc : TFhirCodeSystemConcept;
begin
  result := nil;
  for cc in list do
  begin
    if cc.code = code then
      result := TFhirCodeSystemConcept5.Create(cc.Link, codeSystem)
    else if cc.hasConceptList then
    begin
      result := getCodeWrapper(codeSystem, cc.conceptList, code);
      if result <> nil then
        exit;
    end;
  end;
end;

function TFhirCodeSystemConcept5.getCode(code: String): TFhirCodeSystemConceptW;
begin
  if (code = c.Code) then
    result := self.link
  else
    result := getCodeWrapper(FCodeSystem, c.conceptList, code);
end;

function TFhirCodeSystemConcept5.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (element as TFhirCodeSystemConcept).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystemConcept5.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
var
  i : TFhirCodeSystemConceptProperty;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.Create;
  for i in c.property_List do
    result.Add(TFhirCodeSystemConceptProperty5.Create(i.Link));
end;

procedure TFhirCodeSystemConcept5.setDisplayTag(tag, value: String);
begin
  c.displayElement.Tags[tag] := value;
end;                                  

function TFhirCodeSystemConcept5.itemWeight : String;
begin
  result := (Element as TFhirCodeSystemConcept).getExtensionString(EXT_ITEM_WEIGHT);
  if result = '' then
    result := (Element as TFhirCodeSystemConcept).getExtensionString(EXT_ORDINAL_VALUE);
end;

{ TFhirCodeSystem5 }

function TFhirCodeSystem5.buildImplicitValueSet: TFHIRValueSetW;
begin
  result := TFHIRValueSet5.Create(cs.buildImplicitValueSet);
end;

function TFhirCodeSystem5.hasLanguage(cc : TFhirCodeSystemConcept; langs: THTTPLanguageList): boolean;
var
  cc1 : TFhirCodeSystemConcept;
  d : TFhirCodeSystemConceptDesignation;
  hl : boolean;
begin
  if langs.matches(cs.Language, false) and (cc.display <> '') then
    exit(true);

  result := false;

  for d in cc.designationList do
    if langs.matches(d.language, false) then
      exit(true);

  for cc1 in cc.conceptList do
  begin
    hl := hasLanguage(cc1, langs);
    if (hl) then
      exit(true);
  end;
end;

function TFhirCodeSystem5.hasAnyDisplays(langs: THTTPLanguageList): boolean;
var
  cc : TFhirCodeSystemConcept;
  hl : boolean;
begin
  result := false;
  if (langs.count > 0) then
  begin
    for cc in cs.conceptList do
    begin
      hl := hasLanguage(cc, langs);
      if (hl) then
        exit(true);
    end;
  end;
end;

function TFhirCodeSystem5.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept5.create(cs.conceptList[ndx].Link, cs);
end;

function TFhirCodeSystem5.conceptCount: integer;
begin
  result := cs.conceptList.Count;
end;

function TFhirCodeSystem5.hasConcepts: boolean;
begin
  result := cs.hasConceptList;
end;

function TFhirCodeSystem5.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.Create;
    for i in (resource as TFhirCodeSystem).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept5.create(i.Link, cs));
  end;
  result := FConceptList;
end;

function TFhirCodeSystem5.copyright: String;
begin
  result := cs.copyright;
end;

function TFhirCodeSystem5.cs: TFhirCodeSystem;
begin
  result := Resource as TFhirCodeSystem;
end;

function TFhirCodeSystem5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirCodeSystem5.getCount: integer;
begin
  result := StrToInt(cs.count);
end;

function TFhirCodeSystem5.getDate: TFslDateTime;
begin
  result := cs.date;
end;

function TFhirCodeSystem5.getDescription: String;
begin
  result := cs.description;
end;

function TFhirCodeSystem5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirCodeSystem5.getChildren(c: TFhirCodeSystemConceptW): TFhirCodeSystemConceptListW;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getChildren(c.element as TFhirCodeSystemConcept);
  try
    result := TFhirCodeSystemConceptListW.Create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept5.Create(i.Link, cs));
      result.link;
    finally
      result.free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem5.getCode(code: String): TFhirCodeSystemConceptW;
begin
  result := getCodeWrapper(cs, cs.conceptList, code);
end;

function TFhirCodeSystem5.getContent: TFhirCodeSystemContentMode;
begin
  result := MAP_TFhirCodeSystemContentModeR[cs.content];
end;


function TFhirCodeSystem5.getContext: String;
begin
  result := cs.context;
end;

function TFhirCodeSystem5.getParents(c: TFhirCodeSystemConceptW): TFhirCodeSystemConceptListW;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getParents(c.element as TFhirCodeSystemConcept);
  try
    result := TFhirCodeSystemConceptListW.Create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept5.Create(i.Link, cs));
      result.link;
    finally
      result.free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem5.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (resource as TFhirCodeSystem).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystem5.isInactive(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isInactive(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem5.codeStatus(c: TFhirCodeSystemConceptW): String;
begin
  result := cs.codeStatus(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem5.isAbstract(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isAbstract(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem5.isDeprecated(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isDeprecated(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem5.language: String;
begin
  result := cs.language;
end;

function TFhirCodeSystem5.getName: String;
begin
  result := cs.name;
end;

function TFhirCodeSystem5.properties: TFslList<TFhirCodeSystemPropertyW>;
var
  i : TFhirCodeSystemProperty;
begin
  result := TFslList<TFhirCodeSystemPropertyW>.Create;
  for i in cs.property_List do
    result.Add(TFhirCodeSystemProperty5.Create(i.Link));
end;

procedure TFhirCodeSystem5.setContent(Value: TFhirCodeSystemContentMode);
begin
  cs.content := MAP_TFhirCodeSystemContentMode[value];
end;

procedure TFhirCodeSystem5.setCount(Value: integer);
begin
  cs.count := inttostr(value);
end;

procedure TFhirCodeSystem5.setDate(Value: TFslDateTime);
begin
  cs.date := value;
end;

procedure TFhirCodeSystem5.setDescription(Value: String);
begin
  cs.description := value;
end;

procedure TFhirCodeSystem5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirCodeSystem5.setName(Value: String);
begin
  cs.name := value;
end;

procedure TFhirCodeSystem5.setPublisher(Value: String);
begin
  cs.publisher := value;
end;

function TFhirCodeSystem5.getTitle: String;
begin
  result := cs.title;
end;

procedure TFhirCodeSystem5.setTitle(value: String);
begin
  cs.title := value;
end;

function TFhirCodeSystem5.getExperimental: boolean;
begin
  result := cs.experimental;
end;

procedure TFhirCodeSystem5.setExperimental(value: boolean);
begin
  cs.experimental := value;
end;

procedure TFhirCodeSystem5.setStatus(Value: TPublicationStatus);
begin
  cs.status := MAP_TPublicationStatus[value];
end;

procedure TFhirCodeSystem5.setUrl(Value: String);
begin
  cs.url := value;
end;

procedure TFhirCodeSystem5.setVersion(Value: String);
begin
  cs.version := value;
end;

function TFhirCodeSystem5.GetCaseSensitive: boolean;
begin
  result := cs.caseSensitive;
end;

function TFhirCodeSystem5.supplements: String;
begin
  result := cs.supplements;
end;

function TFhirCodeSystem5.valueSet: String;
begin
  result := cs.valueset;
end;

function TFhirCodeSystem5.getPublisher: String;
begin
  result := cs.publisher;
end;

function TFhirCodeSystem5.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[cs.status];
end;

function TFhirCodeSystem5.getURL: String;
begin
  result := cs.url;
end;

function TFhirCodeSystem5.getVersion: String;
begin
  result := cs.version;
end;

function TFhirCodeSystem5.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  result := interpretVersionAlgorithm(cs.versionAlgorithm);
end;

{ TFhirValueSetExpansion5 }

procedure TFhirValueSetExpansion5.addContains(item: TFhirValueSetExpansionContainsW);
begin
  exp.containsList.Add((item.Element as TFhirValueSetExpansionContains).link);
end;

function TFhirValueSetExpansion5.addContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains5.Create((element as TFhirValueSetExpansion).containsList.Append.Link);
end;

procedure TFhirValueSetExpansion5.addParamBool(name: String; value: boolean);
begin
  exp.addParamBool(name, value);
end;

procedure TFhirValueSetExpansion5.addParamInt(name: String; value: integer);
begin
  exp.AddParamInt(name, value);
end;

procedure TFhirValueSetExpansion5.addParamStr(name, value: String);
begin
  exp.AddParamStr(name, value);
end;

procedure TFhirValueSetExpansion5.addParamCode(name, value: String);
begin
  exp.AddParamCode(name, value);
end;

procedure TFhirValueSetExpansion5.addParamUri(name, value: String);
begin
  exp.AddParamUri(name, value);
end;

function TFhirValueSetExpansion5.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.Create;
  for item in (Element as TFhirValueSetExpansion).containsList do
    result.Add(TFhirValueSetExpansionContains5.Create(item.Link));
end;

function TFhirValueSetExpansion5.getTotal: integer;
begin
  result := StrToIntDef(exp.total, 0);
end;

procedure TFhirValueSetExpansion5.setTotal(value: integer);
begin
  exp.total := inttostr(value);
end;

function TFhirValueSetExpansion5.getOffset: integer;
begin
  result := StrToIntDef(exp.offset, 0);
end;

procedure TFhirValueSetExpansion5.setOffset(value: integer);
begin
  exp.offset := inttostr(value);
end;

procedure TFhirValueSetExpansion5.defineProperty(focus: TFhirValueSetExpansionContainsW; url, code: String; value: TFHIRObject);
var
  pdef, t1 : TFhirValueSetExpansionProperty;
  pdv, t2 : TFhirValueSetExpansionContainsProperty;
begin
  try
    pdef := nil;
    for t1 in exp.property_List do
    begin
      if ((t1.uri = url) or (t1.code = code)) then
      begin
        pdef := t1;
        break;
      end;
    end;
    if (pdef = nil) then
    begin
      pdef := exp.property_List.append;
      pdef.uri := url;
      pdef.code := code;
    end
    else if (pdef.uri = '') then
      pdef.uri := url
    else if (pdef.uri <> url) then
      raise EFHIRException.create('URL mismatch on expansion: '+pdef.uri+' vs '+url+' for code '+code);
    code := pdef.code;

    pdv := nil;
    for t2 in ((focus as TFhirValueSetExpansionContains5).element as TFhirValueSetExpansionContains).property_list do
    begin
      if (t2.code = code) then
      begin
        pdv := t2;
        break;
      end;
    end;
    if (pdv = nil) then
    begin
      pdv := ((focus as TFhirValueSetExpansionContains5).element as TFhirValueSetExpansionContains).property_list.append;
      pdv.code := code;
      pdv.value := (value as TFHIRDataType).link;
    end
    else
      pdv.value := (value as TFHIRDataType).link;
  finally
    value.free;
  end;
end;

procedure TFhirValueSetExpansion5.copyParams(source: TFhirValueSetExpansionW);
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

function TFhirValueSetExpansion5.exp: TFhirValueSetExpansion;
begin
  result := element as TFhirValueSetExpansion;
end;

function TFhirValueSetExpansion5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirValueSetExpansion5.hasParam(name, value: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) and (param.value.primitiveValue = value) then
      exit(true);
end;

function TFhirValueSetExpansion5.makeContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains5.Create(TFhirValueSetExpansionContains.create);
end;

function TFhirValueSetExpansion5.hasParam(name: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) then
      exit(true);
end;

{ TFhirValueSetExpansionContains5 }

function TFhirValueSetExpansionContains5.getCode: String;
begin
  result := (Element as TFhirValueSetExpansionContains).code;
end;

procedure TFhirValueSetExpansionContains5.addDesignation(lang, use, value: String);
var
  d : TFhirValueSetComposeIncludeConceptDesignation;
  l,r : String;
begin
  d := (Element as TFhirValueSetExpansionContains).designationList.Append;
  d.language := lang;
  d.value := value;
  if use <> '' then
  begin
    StringSplit(use, '#', l, r);
    d.use := TFHIRCoding.Create(l, r);
  end;
end;

procedure TFhirValueSetExpansionContains5.addDesignation(lang : TIETFLang; use: TFHIRCodingW; value: TFHIRPrimitiveW; extensions : TFslList<TFHIRExtensionW>);
var
  d : TFhirValueSetComposeIncludeConceptDesignation;
  ext : TFHIRExtensionW;
begin
  d := (Element as TFhirValueSetExpansionContains).designationList.Append;
  if (lang <> nil) then
    d.language := lang.code;
  if (value <> nil) then
    d.valueElement := value.Element.link as TFHIRString;
  if use <> nil then
    d.use := use.Element.link as TFHIRCoding;
  if extensions <> nil then
    for ext in Extensions do
      d.addExtension(ext.element.link as TFHIRExtension);
end;

procedure TFhirValueSetExpansionContains5.addProperty(code: String; value: TFHIRObject);
var
  p : TFhirValueSetExpansionContainsProperty;
begin
  p := (Element as TFhirValueSetExpansionContains).property_list.Append;
  p.code := code;
  p.value := value.link as TFhirDataType;
end;

procedure TFhirValueSetExpansionContains5.addProperty(code: String; prop: TFhirCodeSystemConceptPropertyW);
var
  p : TFhirValueSetExpansionContainsProperty;
begin
  p := (Element as TFhirValueSetExpansionContains).property_list.Append;
  p.code := code;
  p.value := prop.value.link as TFhirDataType;
  p.copyExtensions(prop.element, []);
end;

procedure TFhirValueSetExpansionContains5.addContains(contained: TFhirValueSetExpansionContainsW);
begin
  (Element as TFhirValueSetExpansionContains).containsList.add(contained.Element.link);
end;

procedure TFhirValueSetExpansionContains5.clearContains;
begin
  (Element as TFhirValueSetExpansionContains).containsList.clear;
end;

function TFhirValueSetExpansionContains5.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.Create;
  for item in (Element as TFhirValueSetExpansionContains).containsList do
    result.Add(TFhirValueSetExpansionContains5.Create(item.Link));
end;

function TFhirValueSetExpansionContains5.getDisplay: String;
begin
  result := (Element as TFhirValueSetExpansionContains).display;
end;

function TFhirValueSetExpansionContains5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirValueSetExpansionContains5.getSystem: String;
begin
  result := (Element as TFhirValueSetExpansionContains).system;
end;

function TFhirValueSetExpansionContains5.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
var
  item : TFhirValueSetExpansionContainsProperty;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.Create;
  for item in (Element as TFhirValueSetExpansionContains).property_List do
    result.Add(TFhirCodeSystemConceptProperty5.Create(item.Link));
end;

procedure TFhirValueSetExpansionContains5.setCode(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).code := value;
end;

procedure TFhirValueSetExpansionContains5.setDisplay(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).display := value;
end;

procedure TFhirValueSetExpansionContains5.setSystem(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).system := value;
end;                        

function TFhirValueSetExpansionContains5.GetItemWeight: String;
begin
  result := (Element as TFhirValueSetExpansionContains).getExtensionString(EXT_ITEM_WEIGHT);
  if result = '' then
    result := (Element as TFhirValueSetExpansionContains).getExtensionString(EXT_ORDINAL_VALUE);
end;

procedure TFhirValueSetExpansionContains5.SetItemWeight(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).setExtensionString(EXT_ITEM_WEIGHT, value);
end;


function TFhirValueSetExpansionContains5.GetAbstract: boolean;
begin
  result := (Element as TFhirValueSetExpansionContains).abstract;
end;

function TFhirValueSetExpansionContains5.GetInactive: boolean;
begin
  result := (Element as TFhirValueSetExpansionContains).inactive;
end;

procedure TFhirValueSetExpansionContains5.SetAbstract(Value: boolean);
begin
  (Element as TFhirValueSetExpansionContains).abstract := value;
end;

procedure TFhirValueSetExpansionContains5.SetInactive(Value: boolean);
begin
  (Element as TFhirValueSetExpansionContains).inactive := value;
end;

function TFhirValueSetExpansionContains5.getVersion: String;
begin
  result := (Element as TFhirValueSetExpansionContains).version;
end;

procedure TFhirValueSetExpansionContains5.setVersion(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).version := value
end;


{ TFhirValueSetComposeIncludeConceptDesignation5 }

function TFhirValueSetComposeIncludeConceptDesignation5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirValueSetComposeIncludeConceptDesignation5.language: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).language;
end;

function TFhirValueSetComposeIncludeConceptDesignation5.value: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).value;
end;

function TFhirValueSetComposeIncludeConceptDesignation5.use: TFHIRCodingW;
begin
  if (element as TFhirValueSetComposeIncludeConceptDesignation).use = nil then
    result := nil
  else
    result := TFHIRCoding5.create((element as TFhirValueSetComposeIncludeConceptDesignation).use.link);
end;

function TFhirValueSetComposeIncludeConceptDesignation5.valueElement: TFHIRPrimitiveW;
begin
  if (element as TFhirValueSetComposeIncludeConceptDesignation).valueElement = nil then
    result := nil
  else
    result := TFHIRPrimitive5.create((element as TFhirValueSetComposeIncludeConceptDesignation).valueElement.link);
end;

{ TFhirConceptMap5 }

function TFhirConceptMap5.addGroup(source, target: String): TFhirConceptMapGroupW;
var
  g : TFhirConceptMapGroup;
begin
  g := cm.groupList.Append;
  g.source := source;
  g.target := target;
  result := TFhirConceptMapGroup5.create(g.Link);
end;

function TFhirConceptMap5.cm: TFhirConceptMap;
begin
  result := Resource as TFhirConceptMap;
end;

function TFhirConceptMap5.getVersion: String;
begin
  result := cm.version;
end;

function TFhirConceptMap5.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  result := interpretVersionAlgorithm(cm.versionAlgorithm);
end;

function TFhirConceptMap5.groups: TFslList<TFhirConceptMapGroupW>;
var
  g : TFhirConceptMapGroup;
begin
  result := TFslList<TFhirConceptMapGroupW>.Create;
  for g in cm.groupList do
    result.Add(TFhirConceptMapGroup5.create(g.Link))
end;

procedure TFhirConceptMap5.setVersion(Value: String);
begin
  cm.version := value;
end;

function TFhirConceptMap5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirConceptMap5.source: String;
begin
  if (cm.sourceScope is TFhirReference) then
    result := (cm.sourceScope as TFhirReference).reference
  else if cm.sourceScope <> nil then
    result := cm.sourceScope.primitiveValue;
end;

function TFhirConceptMap5.sourceDesc: String;
begin
  result := cm.sourceDesc;
end;

function TFhirConceptMap5.target: String;
begin
  if (cm.targetScope is TFhirReference) then
    result := (cm.targetScope as TFhirReference).reference
  else if cm.targetScope <> nil then
    result := cm.targetScope.primitiveValue;
end;

function TFhirConceptMap5.targetDesc: String;
begin
  result := cm.targetDesc;
end;

function TFhirConceptMap5.getExperimental: boolean;
begin
  result := cm.experimental;
end;

procedure TFhirConceptMap5.setExperimental(value: boolean);
begin
  cm.experimental := value;
end;

function TFhirConceptMap5.getURL: String;
begin
  result := cm.url;
end;

function TFhirConceptMap5.getDate: TFslDateTime;
begin
  result := cm.Date;
end;

function TFhirConceptMap5.getDescription: String;
begin
  result := cm.Description;
end;

function TFhirConceptMap5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirConceptMap5.getName: String;
begin
  result := cm.Name;
end;

function TFhirConceptMap5.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[cm.Status];
end;

procedure TFhirConceptMap5.setDate(Value: TFslDateTime);
begin
  cm.Date := value;
end;

procedure TFhirConceptMap5.setDescription(Value: String);
begin
  cm.Description := value;
end;

procedure TFhirConceptMap5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirConceptMap5.setName(Value: String);
begin
  cm.Name := value;
end;

procedure TFhirConceptMap5.setStatus(Value: TPublicationStatus);
begin
  cm.Status := MAP_TPublicationStatus[value];
end;

procedure TFhirConceptMap5.setUrl(Value: String);
begin
  cm.Url := value;
end;

procedure TFhirConceptMap5.setPublisher(Value: String);
begin
  cm.publisher := value;
end;

function TFhirConceptMap5.getTitle: String;
begin
  result := cm.title;
end;

procedure TFhirConceptMap5.setTitle(value: String);
begin
  cm.title := value;
end;

function TFhirConceptMap5.getContext: String;
begin
  result := cm.context;
end;

function TFhirConceptMap5.getPublisher: String;
begin
  result := cm.publisher;
end;

{ TFhirConceptMapGroupElementTarget5 }

function TFhirConceptMapGroupElementTarget5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirConceptMapGroupElementTarget5.code: String;
begin
  result := (Element as TFhirConceptMapGroupElementTarget).code;
end;

function TFhirConceptMapGroupElementTarget5.comments: String;
begin
  result := (Element as TFhirConceptMapGroupElementTarget).comment;
end;

function TFhirConceptMapGroupElementTarget5.equivalence: TFHIRConceptEquivalence;
begin
  result := MAP_TFHIRConceptEquivalence[(Element as TFhirConceptMapGroupElementTarget).relationship];
end;

function TFhirConceptMapGroupElementTarget5.products: TFslList<TFhirConceptMapGroupElementDependsOnW>;
var
  i : TFhirConceptMapGroupElementTargetDependsOn;
begin
  result := TFslList<TFhirConceptMapGroupElementDependsOnW>.Create;
  for i in (Element as TFhirConceptMapGroupElementTarget).productList do
    result.Add(TFhirConceptMapGroupElementDependsOn5.Create(i.link));
end;

{ TFhirConceptMapGroupElement5 }

function TFhirConceptMapGroupElement5.addTarget(code: String; eq: TFHIRConceptEquivalence): TFhirConceptMapGroupElementTargetW;
var
  t : TFhirConceptMapGroupElementTarget;
begin
  t := (Element as TFhirConceptMapGroupElement).targetList.Append;
  t.code := code;
  t.relationship := MAP_TFHIRConceptEquivalenceR[eq];
  result := TFhirConceptMapGroupElementTarget5.Create(t.link);
end;

function TFhirConceptMapGroupElement5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirConceptMapGroupElement5.code: String;
begin
  result := (Element as TFhirConceptMapGroupElement).code;
end;

function TFhirConceptMapGroupElement5.targetCount: integer;
begin
  result := (Element as TFhirConceptMapGroupElement).targetList.Count;
end;

function TFhirConceptMapGroupElement5.targets: TFslList<TFhirConceptMapGroupElementTargetW>;
var
  i : TFhirConceptMapGroupElementTarget;
begin
  result := TFslList<TFhirConceptMapGroupElementTargetW>.Create;
  for i in (Element as TFhirConceptMapGroupElement).targetList do
    result.Add(TFhirConceptMapGroupElementTarget5.Create(i.link));
end;


{ TFhirConceptMapGroup5 }

function TFhirConceptMapGroup5.addElement(code: String): TFhirConceptMapGroupElementW;
var
  t : TFhirConceptMapGroupElement;
begin
  t := (Element as TFhirConceptMapGroup).elementList.Append;
  t.code := code;
  result := TFhirConceptMapGroupElement5.Create(t.link);
end;

function TFhirConceptMapGroup5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirConceptMapGroup5.elements: TFslList<TFhirConceptMapGroupElementW>;
var
  t : TFhirConceptMapGroupElement;
begin
  result := TFslList<TFhirConceptMapGroupElementW>.Create;
  for t in (Element as TFhirConceptMapGroup).elementList do
    result.Add(TFhirConceptMapGroupElement5.Create(t.link))
end;

function TFhirConceptMapGroup5.source: String;
begin
  result := (Element as TFhirConceptMapGroup).source;
end;

function TFhirConceptMapGroup5.target: String;
begin
  result := (Element as TFhirConceptMapGroup).target;
end;

{ TFHIRMeta5 }


procedure TFHIRMeta5.addLabel(system, code, display: String);
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

procedure TFHIRMeta5.addProfile(uri: String);
begin
  force;
  m.profileList.Add(TFhirUri.Create(uri));
end;

procedure TFHIRMeta5.addTag(system, code, display: String);
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

procedure TFHIRMeta5.clearLabels;
begin
  if Element <> nil then
    m.securityList.Clear;
end;

procedure TFHIRMeta5.clearProfiles;
begin
  if Element <> nil then
    m.profileList.Clear;
end;

procedure TFHIRMeta5.clearTags;
begin
  if Element <> nil then
    m.tagList.Clear;
end;

destructor TFHIRMeta5.Destroy;
begin
  FResource.free;
  inherited;
end;

function TFHIRMeta5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

procedure TFHIRMeta5.force;
begin
  if Element = nil then
  begin
    FElement := TFHIRMeta.Create;
    Resource.meta := m.Link;
  end;
end;

function TFHIRMeta5.getLastUpdated: TFslDateTime;
begin
  if Element = nil then
    result := TFslDateTime.makeNull
  else
    result := m.lastUpdated;
end;

function TFHIRMeta5.getVersionId: String;
begin
  if Element = nil then
    result := ''
  else
    result := m.versionId;
end;

function TFHIRMeta5.hasLabel(system, code: String): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if Element <> nil then
    for c in m.securityList do
      if (c.system = system) and (c.code = code) then
        exit(true);
end;

function TFHIRMeta5.hasTag(system, code: String): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if Element <> nil then
    for c in m.tagList do
       if (c.system = system) and (c.code = code) then
        exit(true);
end;

function TFHIRMeta5.labels: TFslList<TFHIRCodingW>;
var
  i : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if Element <> nil then
    for i in m.securityList do
      result.Add(TFHIRCoding5.create(i.Link));
end;

function TFHIRMeta5.m: TFhirMeta;
begin
  result := Element as TFHIRMeta;
end;

function TFHIRMeta5.NoElementOk: boolean;
begin
  result := true;
end;

function TFHIRMeta5.profiles: TArray<String>;
var
  i : integer;
begin
  if Element <> nil then
  begin
    SetLength(result, m.profileList.Count);
    for i := 0 to m.profileList.Count - 1 do
      result[i] := m.profileList[i].value;
  end
  else
    SetLength(result, 0);
end;

procedure TFHIRMeta5.removeLabel(system, code: String);
begin
  if Element <> nil then
    m.removeLabel(system, code);
end;

procedure TFHIRMeta5.removeProfile(uri: String);
begin
  if Element <> nil then
    m.profileList.removeUri(uri);
end;

procedure TFHIRMeta5.removeTag(system, code: String);
begin
  if Element <> nil then
    m.removeTag(system, code);
end;

procedure TFHIRMeta5.setLastUpdated(Value: TFslDateTime);
begin
  force;
  m.lastUpdated := value;
end;

procedure TFHIRMeta5.setResource(value: TFHIRResource);
begin
  FResource.free;
  FResource := value;
end;

procedure TFHIRMeta5.setVersionId(Value: String);
begin
  force;
  m.versionId := value;
end;

function TFHIRMeta5.tags: TFslList<TFHIRCodingW>;
var
  i : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if Element <> nil then
    for i in m.tagList do
      result.Add(TFHIRCoding5.create(i.Link));
end;


function TFHIRMeta5.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FResource.sizeInBytes(magic));
end;

{ TFHIRAuditEvent5 }

function TFHIRAuditEvent5.ae: TFHIRAuditEvent;
begin
  result := Resource as TFhirAuditEvent;
end;

function TFHIRAuditEvent5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRAuditEvent5.dateTime: TFslDateTime;
begin
  result := ae.event.dateTime;
end;

procedure TFHIRAuditEvent5.eventSubType(system, code, display: String);
var
  c : TFHIRCoding;
begin
  if ae.event = nil then
    ae.event := TFhirAuditEventEvent.Create;
  if ae.event.code = nil then
    ae.event.code := TFhirCodeableConcept.Create;
  c := ae.event.code.codingList.Append;
  c.code := code;
  c.system := system;
  c.Display := display;
end;

procedure TFHIRAuditEvent5.eventType(system, code, display: String);
var
  c : TFHIRCoding;
begin
  if ae.event = nil then
    ae.event := TFhirAuditEventEvent.Create;
  if ae.event.categoryList.IsEmpty then
    ae.event.categoryList.Append;
  c := TFHIRCoding.Create;
  ae.event.categoryList[0].codingList.add(c);
  c.code := code;
  c.system := system;
  c.Display := display;
end;

function TFHIRAuditEvent5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRAuditEvent5.participantId(system, value, alt, name: String);
var
  p: TFhirAuditEventParticipant;
begin
  p := ae.participantList.append;
  p.who := TFhirReference.Create;
  p.who.identifier := TFhirIdentifier.Create;
  p.who.identifier.system := system;
  p.who.identifier.value := value;
  p.who.display := name;
end;

procedure TFHIRAuditEvent5.participantIp(ip: String);
var
  p: TFhirAuditEventParticipant;
begin
  p := ae.participantList.append;
  p.network := TFhirString.create(ip);
end;

procedure TFHIRAuditEvent5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRAuditEvent5.source(name, system, value: String);
begin
  if ae.source = nil then
    ae.source := TFhirAuditEventSource.Create;
  if ae.source.site = nil then
    ae.source.site := TFhirReference.Create;
  ae.source.site.display := name;
  ae.source.observer := TFhirReference.Create();
  ae.source.observer.identifier := TFhirIdentifier.Create;
  ae.source.observer.identifier.system := system;
  ae.source.observer.identifier.value := value;
end;

procedure TFHIRAuditEvent5.sourceType(system, code, display: String);
var
  c : TFHIRCoding;
begin
  if ae.source = nil then
    ae.source := TFhirAuditEventSource.Create;
  if ae.source.type_List.IsEmpty then
    ae.source.type_List.Append;
  c := ae.source.type_List[0].codingList.Append;
  c.code := code;
  c.system := system;
  c.Display := display;
end;

procedure TFHIRAuditEvent5.success;
begin
  if ae.event = nil then
    ae.event := TFhirAuditEventEvent.Create;
  ae.event.action := AuditEventActionE;
  if ae.event.outcome = nil then
    ae.event.outcome := TFhirAuditEventOutcome.Create;
  ae.event.outcome.code := TFhirCoding.Create(URI_FHIR_AUDIT_EVENT_OUTCOME, '0');
  ae.event.dateTime := TFslDateTime.makeUTC;
end;

{ TFhirCapabilityStatementRestResource5 }

procedure TFhirCapabilityStatementRestResource5.addParam(html, n, url, d: String; t: TFHIRSearchParamType; tgts: array of String);
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

function TFhirCapabilityStatementRestResource5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirCapabilityStatementRestResource5.getCode: String;
begin
  result := CODES_TFhirResourceTypesEnum[(Element as TFhirCapabilityStatementRestResource).type_];
end;

procedure TFhirCapabilityStatementRestResource5.setCode(Value: String);
begin
  (Element as TFhirCapabilityStatementRestResource).type_Element := TFhirEnum.create('http://hl7.org/fhir/resource-types', value);
end;

function TFhirCapabilityStatementRestResource5.getProfile: String;
begin
  result := (Element as TFhirCapabilityStatementRestResource).profile;
end;

procedure TFhirCapabilityStatementRestResource5.setProfile(Value: String);
begin
  (Element as TFhirCapabilityStatementRestResource).profile := value;
end;

procedure TFhirCapabilityStatementRestResource5.addInteraction(codeV, doco: String);
begin
  With (Element as TFhirCapabilityStatementRestResource).interactionList.Append do
  begin
    codeElement := TFhirEnum.Create('http://hl7.org/fhir/ValueSet/type-restful-interaction', codeV);
    documentation := doco;
  end;
end;

procedure TFhirCapabilityStatementRestResource5.addOperation(codeV, defn, doco: String);
begin
  With (Element as TFhirCapabilityStatementRestResource).operationList.Append do
  begin
    name := codeV;
    definition := defn;
    documentation := doco;
  end;
end;

function TFhirCapabilityStatementRestResource5.getReadHistory: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).readHistory;
end;

function TFhirCapabilityStatementRestResource5.hasInteraction: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).interactionList.Count > 0;
end;

procedure TFhirCapabilityStatementRestResource5.setReadHistory(Value: boolean);
begin
  (Element as TFhirCapabilityStatementRestResource).readHistory := Value;
end;


{ TFHIRSubscription5 }

function TFHIRSubscription5.getCriteria: String;
begin
  result := ''; // sub.criteria; - moved to Topic, needs rewrite....
end;

function TFHIRSubscription5.getDirect: boolean;
begin
  result := sub.endpointElement.hasExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct')
    and (sub.endpointElement.getExtensionString('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct') = 'true');
end;

function TFHIRSubscription5.getEndpoint: String;
begin
  result := sub.endpoint;
end;

function TFHIRSubscription5.getError: String;
begin
  result := ''; // sub.error;
end;

function TFHIRSubscription5.getHeaders: TArray<String>;
var
  i : integer;
begin
  setLength(result, sub.headerList.Count);
  for i := 0 to sub.headerList.Count - 1 do
    result[i] := sub.headerList[i].value;
end;

function TFHIRSubscription5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRSubscription5.getTopic: string;
begin
  result := sub.topic;
end;

function TFHIRSubscription5.getMethod: TSubscriptionMethod;
begin
  result := smNull; // MAP_TSubscriptionMethod[sub.channel.type_];
end;

function TFHIRSubscription5.getPayload: String;
begin
  result := ''; // sub.channel.payload;
end;

function TFHIRSubscription5.getStatus: TSubscriptionStatus;
begin
  result := MAP_TSubscriptionStatus[sub.status];
end;

function TFHIRSubscription5.getSummary: String;
//var
//  s : TFhirString;
begin
  result := 'todo';
//  sub.channel.type_Element.value+#1+sub.channel.endpoint+#1+sub.channel.payload;
//  for s in sub.channel.headerList do
//    result := result+#0+s.value;
//  result := result+#0+subst.channel.header;
end;

procedure TFHIRSubscription5.setCriteria(Value: String);
begin
//  sub.criteria := value;
end;

procedure TFHIRSubscription5.setDirect(Value: boolean);
begin
  if value then
    sub.endpointElement.setExtensionBoolean('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct', 'true')
  else
    sub.endpointElement.removeExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct');
end;

procedure TFHIRSubscription5.setEndpoint(Value: String);
begin
  sub.endpoint := value;
end;

procedure TFHIRSubscription5.setError(Value: String);
begin
//  sub.error := value;
end;

procedure TFHIRSubscription5.setheaders(Value: TArray<String>);
var
  s : String;
begin
  sub.headerList.Clear;
  for s in value do
    sub.headerList.Append.value := s;
end;

procedure TFHIRSubscription5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRSubscription5.setMethod(Value: TSubscriptionMethod);
begin
//  sub.channel.type_ := MAP_TSubscriptionMethod2[value];
end;

procedure TFHIRSubscription5.setPayload(Value: String);
begin
//  sub.channel.payload := value;
end;

procedure TFHIRSubscription5.setStatus(Value: TSubscriptionStatus);
begin
  sub.status := MAP_TSubscriptionStatus2[value];
end;

function TFHIRSubscription5.sub: TFhirSubscription;
begin
  result := resource as TFhirSubscription;
end;

function TFHIRSubscription5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

{ TFhirBinary5 }

function TFhirBinary5.content: TBytes;
begin
  result := (resource as TFHIRBinary).data;
end;

function TFhirBinary5.ContentType: String;
begin
  result := (resource as TFHIRBinary).contentType;
end;

function TFhirBinary5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirBinary5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFhirBinary5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirObservationComponent5 }

function TFhirObservationComponent5.codings: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if comp.code <> nil then
    for c in comp.code.codingList do
      result.Add(TFHIRCoding5.Create(c.Link));
end;

function TFhirObservationComponent5.comp: TFhirObservationComponent;
begin
  result := (Element as TFhirObservationComponent);
end;

function TFhirObservationComponent5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirObservationComponent5.dataAbsentReason: TFhirCodeableConceptW;
begin
  if comp.dataAbsentReason = nil then
    result := nil
  else
    result := TFhirCodeableConcept5.Create(comp.dataAbsentReason.link);
end;

function TFhirObservationComponent5.getValue: TFHIRObject;
begin
  result := comp.value;
end;

procedure TFhirObservationComponent5.setValue(Value: TFHIRObject);
begin
  comp.value := value as TFHIRDataType;
end;

function TFhirObservationComponent5.valueString: String;
begin
  if (comp.value <> nil) and (comp.value.isPrimitive) then
    result := comp.value.primitiveValue
  else
    result := '';
end;

function TFhirObservationComponent5.valueW: TFHIRXVersionElementWrapper;
begin
  if comp.value is TFhirCodeableConcept then
    result := TFhirCodeableConcept5.Create(comp.value.Link)
  else if comp.value is TFHIRQuantity then
    result := TFHIRQuantity5.Create(comp.value.Link)
  else
    result := nil;
end;

{ TFhirObservation5 }

procedure TFhirObservation5.setCode(c: TFHIRCodingW);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.codingList.add((c.Element as TFHIRCoding).Link);
end;

function TFhirObservation5.addComp(system, code: String): TFhirObservationComponentW;
var
  c : TFhirObservationComponent;
begin
  c := TFhirObservationComponent.Create;
  c.code := TFhirCodeableConcept.Create;
  c.code.codingList.add(TFhirCoding.Create(system, code));
  result := TFhirObservationComponent5.Create(c.link);
end;

function TFhirObservation5.getStatus: TObservationStatus;
begin
  result := MAP_TObservationStatus2[obs.status];
end;

function TFhirObservation5.obs: TFHIRObservation;
begin
  result := resource as TFhirObservation;
end;

procedure TFhirObservation5.setPeriod(start, finish: TDateTime);
begin
  obs.effective := TFhirPeriod.Create;
  TFhirPeriod(obs.effective).start := TFslDateTime.makeUTC(start);
  TFhirPeriod(obs.effective).end_ := TFslDateTime.makeUTC(finish);
end;

procedure TFhirObservation5.setStatus(Value: TObservationStatus);
begin
  obs.status := MAP_TObservationStatus[value];
end;

function TFhirObservation5.codings: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if obs.code <> nil then
    for c in obs.code.codingList do
      result.Add(TFHIRCoding5.Create(c.Link));
end;

function TFhirObservation5.components: TFslList<TFhirObservationComponentW>;
var
  c : TFhirObservationComponent;
begin
  result := TFslList<TFhirObservationComponentW>.Create;
  for c in obs.componentList do
    result.Add(TFhirObservationComponent5.Create(c.Link));
end;

function TFhirObservation5.dataAbsentReason: TFhirCodeableConceptW;
begin
  if obs.dataAbsentReason = nil then
    result := nil
  else
    result := TFhirCodeableConcept5.Create(obs.dataAbsentReason.link);
end;

procedure TFhirObservation5.getDates(var dt, dtMin, dtMax: TDateTime);
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
      // nothing;
    else
      dtMax := (obs.effective as TFHIRPeriod).end_.Max.UTC.DateTime;
  end;
end;

function TFhirObservation5.getValue: TFHIRObject;
begin
  result := obs.value;
end;

procedure TFhirObservation5.setValue(Value: TFHIRObject);
begin
  obs.value := value as TFHIRDataType;
end;

function TFhirObservation5.valueW: TFHIRXVersionElementWrapper;
begin
  if obs.value is TFhirCodeableConcept then
    result := TFhirCodeableConcept5.Create(obs.value.Link)
  else if obs.value is TFHIRQuantity then
    result := TFHIRQuantity5.Create(obs.value.Link)
  else
    result := nil;
end;

function TFhirObservation5.hasTime: boolean;
begin
  result := obs.effective <> nil;
end;

function TFhirObservation5.categories: TFslList<TFHIRCodingW>;
var
  cc : TFHIRCodeableConcept;
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  for cc in obs.categoryList do
    for c in cc.codingList do
      result.Add(TFHIRCoding5.Create(c.Link));
end;

procedure TFhirObservation5.setCode(system, code, display: String);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.codingList.add(TFHIRCoding.Create(system, code, display));
end;

procedure TFhirObservation5.SetCodeText(const Value: String);
begin
  if value <> '' then
  begin
    if obs.code = nil then
      obs.code := TFhirCodeableConcept.Create;
    obs.code.text := Value
  end
  else if (obs.code <> nil) and not obs.code.hasCoding then
    obs.code := nil;
end;

procedure TFhirObservation5.SetComment(const Value: String);
begin
  obs.noteList.Append.text := value;
end;

function TFhirObservation5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

procedure TFhirObservation5.SetDevice(const Value: String);
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

procedure TFhirObservation5.SetDeviceName(const Value: String);
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

procedure TFhirObservation5.SetEffective(const Value: TFHIRObject);
begin
  obs.effective := Value as TFHIRDataType;
end;

procedure TFhirObservation5.SetEffectiveDateTime(const Value: TFslDateTime);
begin
  SetEffective(TFhirDateTime.Create(value));
end;

procedure TFhirObservation5.SetEffectivePeriod(const Value: TFHIRPeriodW);
begin
  obs.effective := (value.Element as TFHIRDataType).Link;
end;

procedure TFhirObservation5.SetIssued(const Value: TFslDateTime);
begin
  obs.issued := Value;
end;

procedure TFhirObservation5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirObservation5.SetSubject(const Value: String);
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

function TFhirObservation5.GetCodeText: String;
begin
  if obs.code = nil then
    result := ''
  else
    result := obs.code.text;
end;

function TFhirObservation5.GetComment: String;
begin
  if obs.noteList.Count = 0 then
    result := ''
  else
    result := obs.noteList[0].text;
end;

function TFhirObservation5.getComponent(system, code: String; var comp: TFhirObservationComponentW): boolean;
var
  c : TFHIRObservationComponent;
begin
  comp := nil;
  result := obs.getComponent(system, code, c);
  if result then
    comp := TFHIRObservationComponent5.create(c.link);
end;

function TFhirObservation5.getComponent(system: String; var comp: TFhirObservationComponentW): boolean;
var
  c : TFHIRObservationComponent;
begin
  comp := nil;
  result := obs.getComponent(system, c);
  if result then
    comp := TFHIRObservationComponent5.create(c.link);
end;

function TFhirObservation5.GetDevice: String;
begin
  if obs.device <> nil then
    result := obs.device.reference
  else
    result := '';
end;

function TFhirObservation5.GetDeviceName: String;
begin
  if obs.device <> nil then
    result := obs.device.display
  else
    result := '';
end;

function TFhirObservation5.GetEffective: TFHIRObject;
begin
  result := obs.effective;
end;

function TFhirObservation5.GetEffectiveDateTime: TFslDateTime;
begin
  if obs.effective is TFhirDateTime then
    result := (obs.effective as TFhirDateTime).value
  else
    result := TFslDateTime.makeNull;
end;

function TFhirObservation5.GetEffectivePeriod: TFHIRPeriodW;
begin
  if obs.effective is TFhirPeriod then
    result := TFHIRPeriod5.create(obs.effective.Link)
  else
    result := nil;
end;

function TFhirObservation5.GetIssued: TFslDateTime;
begin
  result := obs.issued;
end;

function TFhirObservation5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirObservation5.hasDevice: boolean;
begin
  result := obs.Device <> nil;
end;

function TFhirObservation5.hasEffective: boolean;
begin
  result := obs.Effective <> nil;
end;

function TFhirObservation5.hasIssued: boolean;
begin
  result := obs.IssuedElement <> nil;
end;

function TFhirObservation5.hasMethod: boolean;
begin
  result := obs.Method <> nil;
end;

function TFhirObservation5.hasSubject: boolean;
begin
  result := obs.Subject <> nil;
end;

function TFhirObservation5.method(force : boolean) : TFhirCodeableConceptW;
begin
  if (obs.method = nil) and force then
    obs.method := TFhirCodeableConcept.Create;

  if obs.method = nil then
    result := nil
  else
    result := TFhirCodeableConcept5.Create(obs.method.link);
end;

function TFhirObservation5.GetSubject: String;
begin
  if obs.subject <> nil then
    result := obs.subject.reference
  else
    result := '';
end;

procedure TFhirObservation5.addCategory(c: TFHIRCodingW);
begin
  obs.categoryList.Append.codingList.add((c.Element as TFHIRCoding).Link);
end;

procedure TFhirObservation5.addCategory(system, code, display: String);
begin
  obs.categoryList.Append.codingList.add(TFHIRCoding.Create(system, code, display));
end;

procedure TFhirObservation5.setCode(text: String);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.text := text;
end;

{ TFHIRQuantity5 }

function TFHIRQuantity5.asDuration: TDateTime;
begin
  result := qty.asDuration;
end;

function TFHIRQuantity5.getCode: String;
begin
  result := qty.code;
end;

function TFHIRQuantity5.getSystem: String;
begin
  result := qty.system;
end;

function TFHIRQuantity5.getUnit: String;
begin
  result := qty.unit_;
end;

function TFHIRQuantity5.getValue: String;
begin
  result := qty.value;
end;

function TFHIRQuantity5.qty: TFHIRQuantity;
begin
  result := Element as TFHIRQuantity;
end;

function TFHIRQuantity5.renderText: String;
begin
  result := gen(element as TFhirQuantity);
end;

procedure TFHIRQuantity5.setCode(Value: String);
begin
  qty.code := Value;
end;

procedure TFHIRQuantity5.setSystem(Value: String);
begin
  qty.system := Value;
end;

procedure TFHIRQuantity5.setUnit(Value: String);
begin
  qty.unit_ := Value;
end;

procedure TFHIRQuantity5.setValue(Value: String);
begin
  qty.value := Value;
end;

function TFHIRQuantity5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

{ TFHIRLookupOpRequest5 }

function TFHIRLookupOpRequest5.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationRequest).asParams;
end;

function TFHIRLookupOpRequest5.coding: TFHIRCodingW;
begin
  result := TFHIRCoding5.Create((op as TFHIRLookupOpRequest).coding.Link);
end;

function TFHIRLookupOpRequest5.displayLanguage: String;
begin
  result := (op as TFHIRLookupOpRequest).displayLanguage;
end;

procedure TFHIRLookupOpRequest5.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpRequest).load(params);
end;

procedure TFHIRLookupOpRequest5.loadCoding;
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
    raise EFSLException.create('Unable to find a code to lookup (need coding or system/code)');
end;

function TFHIRLookupOpRequest5.propList: TArray<String>;
var
  i : integer;
begin
  SetLength(result, (op as TFHIRLookupOpRequest).property_List.Count);
  for i := 0 to (op as TFHIRLookupOpRequest).property_List.Count -1 do
    result[i] := (op as TFHIRLookupOpRequest).property_List[i];
end;

procedure TFHIRLookupOpRequest5.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpRequest).load(params as TFhirParameters);
end;

{ TFHIRSubsumesOpRequest5 }

function TFHIRSubsumesOpRequest5.asParams: TFHIRResourceV;
begin
  result := (op as TFHIRSubsumesOpRequest).asParams;
end;

procedure TFHIRSubsumesOpRequest5.load(params: TFHIRResourceV);
begin
  (op as TFHIRSubsumesOpRequest).load(params as TFHIRParameters);
end;

function TFHIRSubsumesOpRequest5.codeA: String;
begin
  result := (op as TFHIRSubsumesOpRequest).codeA;
end;

function TFHIRSubsumesOpRequest5.codeB: String;
begin
  result := (op as TFHIRSubsumesOpRequest).codeB;
end;

function TFHIRSubsumesOpRequest5.codingA: TFHIRCodingW;
begin
  result := TFHIRCoding5.Create((op as TFHIRSubsumesOpRequest).codingA);
end;

function TFHIRSubsumesOpRequest5.codingB: TFHIRCodingW;
begin
  result := TFHIRCoding5.Create((op as TFHIRSubsumesOpRequest).codingB);
end;

function TFHIRSubsumesOpRequest5.hasCodingA: boolean;
begin
  result := (op as TFHIRSubsumesOpRequest).codingA <> nil;
end;

function TFHIRSubsumesOpRequest5.hasCodingB: boolean;
begin
  result := (op as TFHIRSubsumesOpRequest).codingB <> nil;
end;

function TFHIRSubsumesOpRequest5.version: String;
begin
  result := (op as TFHIRSubsumesOpRequest).version;
end;

procedure TFHIRSubsumesOpRequest5.load(params: THTTPParameters);
begin
  (op as TFHIRSubsumesOpRequest).load(params);
end;

function TFHIRSubsumesOpRequest5.systemUri: String;
begin
  result := (op as TFHIRSubsumesOpRequest).system;
end;

{ TFHIRSubsumesOpResponse5 }

function TFHIRSubsumesOpResponse5.asParams: TFHIRResourceV;
begin
  result := (op as TFHIRSubsumesOpResponse).asParams;
end;

procedure TFHIRSubsumesOpResponse5.load(params: TFHIRResourceV);
begin
  (op as TFHIRSubsumesOpResponse).load(params as TFHIRParameters);
end;

function TFHIRSubsumesOpResponse5.GetOutcome: String;
begin
  result := (op as TFHIRSubsumesOpResponse).outcome;
end;

procedure TFHIRSubsumesOpResponse5.load(params: THTTPParameters);
begin
  (op as TFHIRSubsumesOpResponse).load(params);
end;

procedure TFHIRSubsumesOpResponse5.SetOutcome(Value: String);
begin
  (op as TFHIRSubsumesOpResponse).outcome := value;
end;

{ TFhirCodeableConcept5 }

procedure TFhirCodeableConcept5.addCoding(coding: TFHIRCodingW);
var
  list : TFHIRCodingList;
  c : TFHIRCoding;
begin
  list := (Element as TFhirCodeableConcept).codingList;
  c := (coding.Element as TFHIRCoding).link;
  list.Add(c);
end;

function TFhirCodeableConcept5.addCoding: TFHIRCodingW;
begin
  result := TFHIRCoding5.create((Element as TFhirCodeableConcept).codingList.Append.link);
end;

procedure TFhirCodeableConcept5.removeCoding(systemUri, version, code: String);
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

function TFhirCodeableConcept5.codingCount: integer;
begin
  result := (Element as TFhirCodeableConcept).codingList.Count;
end;

function TFhirCodeableConcept5.codings: TFslList<TFhirCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFhirCodingW>.Create;
  for c in (Element as TFhirCodeableConcept).codingList do
    result.Add(TFHIRCoding5.Create(c.Link));
end;

function TFhirCodeableConcept5.fromSystem(Systems: TArray<String>; required: boolean): String;
begin
  result := (Element as TFhirCodeableConcept).fromSystem(Systems, required);
end;

function TFhirCodeableConcept5.GetText: String;
begin
  result := (Element as TFhirCodeableConcept).text;
end;

function TFhirCodeableConcept5.renderText: String;
begin
  result := gen(element as TFhirCodeableConcept);
end;

procedure TFhirCodeableConcept5.SetText(const Value: String);
begin
  (Element as TFhirCodeableConcept).text := value;
end;

function TFhirCodeableConcept5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirCodeableConcept5.fromSystem(System: String; required: boolean): String;
begin
  result := (Element as TFhirCodeableConcept).fromSystem(System, required);
end;

function TFhirCodeableConcept5.summary: String;
begin
  result := summarise(Element as TFhirCodeableConcept);
end;

function TFhirCodeableConcept5.hasCode(systemUri, code: String): boolean;
begin
  result := (Element as TFhirCodeableConcept).hasCode(systemUri, code);
end;

function TFhirCodeableConcept5.hasCode(systemUri, version, code: String): boolean;
begin
  result := (Element as TFhirCodeableConcept).hasCode(systemUri, version, code);
end;

procedure TFhirCodeableConcept5.clearCodings;
begin
  (Element as TFhirCodeableConcept).codingList.Clear;
end;

procedure TFhirCodeableConcept5.addCoding(systemUri, version, code, display : String);
begin
  (Element as TFhirCodeableConcept).addCoding(systemUri, version, code, display);
end;


{ TFHIRGroup5 }

function TFHIRGroup5.characteristics: TFslList<TFHIRGroupCharacteristicW>;
var
  gc : TFHIRGroupCharacteristic;
begin
  result := TFslList<TFHIRGroupCharacteristicW>.Create;
  for gc in (Resource as TFHIRGroup).characteristicList do
    result.add(TFHIRGroupCharacteristic5.create(gc.link));
end;

function TFHIRGroup5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRGroup5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRGroup5.hasCharacteristics: boolean;
begin
  result := (Resource as TFHIRGroup).characteristicList.count > 0;
end;

function TFHIRGroup5.hasMembers: boolean;
begin
  result := (Resource as TFHIRGroup).memberList.count > 0;
end;

function TFHIRGroup5.name: String;
begin
  result := (Resource as TFHIRGroup).name;
end;

procedure TFHIRGroup5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFHIRGroupCharacteristic5 }

function TFHIRGroupCharacteristic5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRGroupCharacteristic5.code: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConcept5.Create((element as TFHIRGroupCharacteristic).code);
end;

function TFHIRGroupCharacteristic5.value: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConcept5.Create((element as TFHIRGroupCharacteristic).value);
end;

{ TFHIRStatsOpResponse5 }

procedure TFHIRStatsOpResponse5.addObs(obs: TFHIRResourceV);
begin
  (Op as TFHIRStatsOpResponse).sourceList.Add(obs as TFhirObservation);
end;

function TFHIRStatsOpResponse5.asParams: TFHIRResourceV;
begin
  result := (op as TFHIRLookupOpResponse).asParams;
end;

procedure TFHIRStatsOpResponse5.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpResponse).load(params);
end;

procedure TFHIRStatsOpResponse5.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpResponse).load(params as TFHIRParameters);
end;

{ TFHIRNamingSystem5 }

function TFHIRNamingSystem5.getContext: String;
begin
  result := '';
end;

function TFHIRNamingSystem5.getDate: TFslDateTime;
begin
  result := nm.date;
end;

function TFHIRNamingSystem5.getDescription: String;
begin
  result := nm.description;
end;

function TFHIRNamingSystem5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRNamingSystem5.getName: String;
begin
  result := nm.name;
end;

function TFHIRNamingSystem5.getPublisher: String;
begin
  result := nm.publisher;
end;

function TFHIRNamingSystem5.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[nm.status];
end;

function TFHIRNamingSystem5.getUri: String;
begin
  result := nm.getUri;
end;

function TFHIRNamingSystem5.getURL: String;
begin
  result := '';
end;

function TFHIRNamingSystem5.getVersion: String;
begin
  result := '';
end;

function TFHIRNamingSystem5.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  result := interpretVersionAlgorithm(nm.versionAlgorithm);
end;

function TFHIRNamingSystem5.hasOid(oid: String): boolean;
begin
  result := nm.hasOid(oid);
end;

function TFHIRNamingSystem5.nm: TFHIRNamingSystem;
begin
  result := (resource as TFHIRNamingSystem);
end;

procedure TFHIRNamingSystem5.setDate(Value: TFslDateTime);
begin
  nm.date := value;
end;

procedure TFHIRNamingSystem5.setDescription(Value: String);
begin
  nm.description := value;
end;

procedure TFHIRNamingSystem5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRNamingSystem5.setName(Value: String);
begin
  nm.name := value;
end;

procedure TFHIRNamingSystem5.setPublisher(Value: String);
begin
  nm.publisher := value;
end;

function TFHIRNamingSystem5.getTitle: String;
begin
  result := nm.title;
end;

procedure TFHIRNamingSystem5.setTitle(value: String);
begin
  nm.title := value;
end;

procedure TFHIRNamingSystem5.setStatus(Value: TPublicationStatus);
begin
  nm.status := MAP_TPublicationStatus[value];
end;

procedure TFHIRNamingSystem5.setUrl(Value: String);
begin
  // nothing
end;

procedure TFHIRNamingSystem5.setVersion(Value: String);
begin
  // nothing
end;

function TFHIRNamingSystem5.getExperimental: boolean;
begin
  result := nm.experimental;
end;

procedure TFHIRNamingSystem5.setExperimental(value: boolean);
begin
  nm.experimental := value;
end;

function TFHIRNamingSystem5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

{ TFHIRStructureMap5 }

function TFHIRStructureMap5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRStructureMap5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRStructureMap5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRStructureMap5.url: String;
begin
  result := (Resource as TFHIRStructureMap).url;
end;

{ TFHIREventDefinition5 }

function TFHIREventDefinition5.dataType: String;
begin
  if ed.triggerList.Count = 0 then
    result := ''
  else if ed.triggerList[0].dataList.Count = 0 then
    result := ''
  else
    result := CODES_TFhirFHIRTypesEnum[ed.triggerList[0].dataList[0].type_];
end;

function TFHIREventDefinition5.ed: TFHIREventDefinition;
begin
  result := resource as TFHIREventDefinition;
end;

function TFHIREventDefinition5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIREventDefinition5.expression: String;
begin
  if ed.triggerList.Count = 0 then
    result := ''
  else if ed.triggerList[0].condition = nil then
    result := ''
  else
    result := ed.triggerList[0].condition.expression;
end;

function TFHIREventDefinition5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIREventDefinition5.language: String;
begin
  if ed.triggerList.Count = 0 then
    result := ''
  else if ed.triggerList[0].condition = nil then
    result := ''
  else
    result := ed.triggerList[0].condition.language;
end;

procedure TFHIREventDefinition5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

//const
//  MAP_TTriggerType : array [TFhirTriggerTypeEnum] of TTriggerType = (ttNull, ttNamedEvent, ttPeriodic, ttDataChanged, ttDataAdded, ttDataModified, ttDataRemoved, ttDataAccessed, ttDataAccessEnded);

function TFHIREventDefinition5.triggerType: TTriggerType;
begin
//  result := MAP_TTriggerType[ed.trigger.type_];
  result := ttNull;
end;

function TFHIREventDefinition5.url: String;
begin
  result := ed.url;
end;

{ TFhirPatient5 }

function TFhirPatient5.pat: TFHIRPatient;
begin
  result := resource as TFhirPatient;
end;

function TFhirPatient5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirPatient5.nameSummary: String;
begin
  result := HumanNamesAsText(pat.nameList);
end;

function TFhirPatient5.activeStr: String;
begin
  if pat.activeElement = nil then
    result := ''
  else if pat.active then
    result := 'true'
  else
    result := 'false';
end;

function TFhirPatient5.GetActive: boolean;
begin
  result := pat.active;
end;

procedure TFhirPatient5.SetActive(const Value: boolean);
begin
  pat.active := value;
end;


function TFhirPatient5.gender: String;
begin
  result := CODES_TFhirAdministrativeGenderEnum[pat.gender];
end;

function TFhirPatient5.genderPlus: String;
begin
  result := pat.genderPlus;
end;

function TFhirPatient5.GetFamily: String;
var
  n : TFhirHumanName;
begin
  result := '';
  for n in pat.nameList do
    if (n.use = NameUseNull) and (n.family <> '') then
      exit(n.family);
  for n in pat.nameList do
    if (n.use = NameUseUsual) and (n.family <> '') then
      exit(n.family);
  for n in pat.nameList do
    if (n.use = NameUseOfficial) and (n.family <> '') then
      exit(n.family);
  for n in pat.nameList do
    if (n.family <> '') then
      exit(n.family);
end;

procedure TFhirPatient5.SetFamily(const Value: String);
var
  n : TFhirHumanName;
begin
  for n in pat.nameList do
    if (n.use = NameUseNull) then
    begin
      n.family := value;
      exit();
    end;
  for n in pat.nameList do
    if (n.use = NameUseUsual) then
    begin
      n.family := value;
      exit();
    end;
  for n in pat.nameList do
    if (n.use = NameUseOfficial) then
    begin
      n.family := value;
      exit();
    end;
  pat.nameList.append.family := value;
end;

function TFhirPatient5.GetDob: String;
begin
  result := pat.birthDate.toXML;
end;

procedure TFhirPatient5.SetDob(const Value: String);
begin
  pat.birthDate := TFslDateTime.fromXML(value);
end;

function TFhirPatient5.GetIdentifier(systemUri: String): String;
var
  id : TFhirIdentifier;
begin
  result := '';
  for id in pat.identifierList do
    if id.system = systemUri then
      exit(id.value);
end;

procedure TFhirPatient5.SetIdentifier(systemUri: String; const Value: String);
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

function TFhirPatient5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirPatient5.identifierSummary: String;
begin
  result := IdentifiersAsText(pat.identifierList);
end;

procedure TFhirPatient5.addGiven(name: String);
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

function TFhirPatient5.contactSummary: String;
begin
  result := ContactsAsText(pat.telecomList);
end;

procedure TFhirPatient5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirTerminologyCapabilities5 }

procedure TFhirTerminologyCapabilities5.addExpansionParameter(code, doco: String);
begin
  if (tc.expansion = nil) then
    tc.expansion := TFhirTerminologyCapabilitiesExpansion.Create;
  with tc.expansion.parameterList.Append do
  begin
    name := code;
    documentation := doco;
  end;
end;

procedure TFhirTerminologyCapabilities5.contact(kind: TContactType; value: String);
var
  c : TFhirContactPoint;
  ct : TFhirConformanceContact;
begin
  ct := tc.contactList.Append;
  c := ct.telecomList.Append;
  c.system := MAP_TContactType[kind];
  c.value := 'http://healthintersections.com.au/';
end;

function TFhirTerminologyCapabilities5.getContext: String;
begin
  result := tc.context;
end;

function TFhirTerminologyCapabilities5.getDate: TFslDateTime;
begin
  result := tc.date;
end;

function TFhirTerminologyCapabilities5.getDescription: String;
begin
  result := tc.description;
end;

function TFhirTerminologyCapabilities5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirTerminologyCapabilities5.getName: String;
begin
  result := tc.name;
end;

function TFhirTerminologyCapabilities5.getPublisher: String;
begin
  result := tc.publisher;
end;

function TFhirTerminologyCapabilities5.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[tc.Status];
end;

function TFhirTerminologyCapabilities5.getURL: String;
begin
  result := tc.url;
end;

function TFhirTerminologyCapabilities5.getVersion: String;
begin
  result := tc.version;
end;


procedure TFhirTerminologyCapabilities5.setContext(Value: String);
begin
end;


procedure TFhirTerminologyCapabilities5.setDate(Value: TFslDateTime);
begin
  tc.date := value;
end;


procedure TFhirTerminologyCapabilities5.setDescription(Value: String);
begin
  tc.description := value;
end;


procedure TFhirTerminologyCapabilities5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirTerminologyCapabilities5.setName(Value: String);
begin
  tc.name := value;
end;


procedure TFhirTerminologyCapabilities5.setPublisher(Value: String);
begin
  tc.publisher := value;
end;

function TFhirTerminologyCapabilities5.getTitle: String;
begin
  result := tc.title;
end;

procedure TFhirTerminologyCapabilities5.setTitle(value: String);
begin
  tc.title := value;
end;


procedure TFhirTerminologyCapabilities5.setStatus(Value: TPublicationStatus);
begin
  tc.Status := MAP_TPublicationStatus[Value];
end;


procedure TFhirTerminologyCapabilities5.setUrl(Value: String);
begin
  tc.url := value;
end;


procedure TFhirTerminologyCapabilities5.setVersion(Value: String);
begin
  tc.version := value;
end;

function TFhirTerminologyCapabilities5.getKind: TCapabilityStatementKind;
begin
  case tc.kind of
    CapabilityStatementKindInstance : result := cskInstance;
    CapabilityStatementKindCapability : result := cskCapability;
    CapabilityStatementKindRequirements : result := cskRequirements;
  else
    result := cskNull;
  end;
end;

procedure TFhirTerminologyCapabilities5.setKind(Value: TCapabilityStatementKind);
begin
  case value of
    cskInstance : tc.kind := CapabilityStatementKindInstance;
    cskCapability : tc.kind := CapabilityStatementKindCapability;
    cskRequirements : tc.kind := CapabilityStatementKindRequirements;
  else
    tc.kind := CapabilityStatementKindNull;
  end;
end;

function TFhirTerminologyCapabilities5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;


procedure TFhirTerminologyCapabilities5.systemUri(url: String);
begin
  tc.codeSystemList.Append.uri := url;
end;

procedure TFhirTerminologyCapabilities5.systemVersion(url: String);
var
  cs : TFhirTerminologyCapabilitiesCodeSystem;
  u, v : String;
begin
  StringSplit(url, '|', u, v);
  cs := tc.codeSystemList.Append;
  cs.uri := u;
  if (v <> '') then
    cs.versionList.Append.code := v;
end;

function TFhirTerminologyCapabilities5.tc: TFhirTerminologyCapabilities;
begin
  result := (Fres as TFhirTerminologyCapabilities);
end;


{ TFHIRPeriod5 }

function TFHIRPeriod5.GetEnd: TFslDateTime;
begin
  result := period.end_;
end;

function TFHIRPeriod5.GetStart: TFslDateTime;
begin
  result := period.start;
end;

function TFHIRPeriod5.period: TFHIRPeriod;
begin
  result := Element as TFHIRPeriod;
end;

function TFHIRPeriod5.renderText: String;
begin
  result := gen(element as TFhirPeriod);
end;

procedure TFHIRPeriod5.SetEnd(const Value: TFslDateTime);

begin
  period.end_ := value;
end;

procedure TFHIRPeriod5.SetStart(const Value: TFslDateTime);

begin
  period.start := value;
end;

function TFHIRPeriod5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

{ TFHIRConsent5 }

function TFHIRConsent5.consent: TFHIRConsent;
begin
  result := resource as TFHIRConsent;
end;

function TFHIRConsent5.GetActive: boolean;
begin
  result := consent.status = ConsentStateActive;
end;

function TFHIRConsent5.GetDateTime: TFslDateTime;
begin
  result := consent.dateTime;
end;

function TFHIRConsent5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRConsent5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRConsent5.GetPatient: String;
begin
  if consent.subject <> nil then
    result := consent.subject.reference
  else
    result := '';
end;

function TFHIRConsent5.listProvisions: TFslList<TFhirConsentProvisionW>;
begin
  result := nil; // for now
end;

procedure TFHIRConsent5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirEncounter5 }

function TFhirEncounter5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirEncounter5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirEncounter5.patientId: String;
begin
  result := (FRes as TFHIREncounter).subject.getId;
end;

procedure TFhirEncounter5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFhirEncounter5.summary: String;
begin
  result := CODES_TFhirEncounterStatusEnum[(FRes as TFHIREncounter).status];
end;

{ TFhirTestScript5 }

function TFHIRTestScript5.getContext: String;
begin
  result := '';
end;

function TFHIRTestScript5.getDate: TFslDateTime;
begin
  result := ts.date;
end;

function TFHIRTestScript5.getDescription: String;
begin
  result := ts.description;
end;

function TFHIRTestScript5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRTestScript5.getName: String;
begin
  result := ts.name;
end;

function TFHIRTestScript5.getPublisher: String;
begin
  result := ts.publisher;
end;

function TFHIRTestScript5.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[ts.Status];
end;

function TFHIRTestScript5.getURL: String;
begin
  result := ts.url;
end;

function TFHIRTestScript5.getVersion: String;
begin
  result := ts.version;
end;

function TFHIRTestScript5.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  result := interpretVersionAlgorithm(ts.versionAlgorithm);
end;

procedure TFHIRTestScript5.setDate(Value: TFslDateTime);
begin
  ts.date := value;
end;


procedure TFHIRTestScript5.setDescription(Value: String);
begin
  ts.description := value;
end;

function TFHIRTestScript5.getExperimental: boolean;
begin
  result := ts.experimental;
end;

procedure TFHIRTestScript5.setExperimental(value: boolean);
begin
  ts.experimental := value;
end;

procedure TFHIRTestScript5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRTestScript5.setName(Value: String);
begin
  ts.name := value;
end;


procedure TFHIRTestScript5.setPublisher(Value: String);
begin
  ts.publisher := value;
end;

function TFHIRTestScript5.getTitle: String;
begin
  result := ts.title;
end;

procedure TFHIRTestScript5.setTitle(value: String);
begin
  ts.title := value;
end;


procedure TFHIRTestScript5.setStatus(Value: TPublicationStatus);
begin
  ts.Status := MAP_TPublicationStatus[Value];
end;


procedure TFHIRTestScript5.setUrl(Value: String);
begin
  ts.url := value;
end;

procedure TFHIRTestScript5.setVersion(Value: String);
begin
  ts.version := value;
end;

function TFHIRTestScript5.ts: TFHIRTestScript;
begin
  result := (Fres as TFhirTestScript);
end;

function TFHIRTestScript5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

{ TFhirProvenance5 }

procedure TFhirProvenance5.addTarget(url: String);
begin
  p.targetList.Append.reference := url;
end;

procedure TFhirProvenance5.clearSignatures;
begin
  p.signatureList.Clear;
end;

procedure TFhirProvenance5.clearTargets;
begin
  p.targetList.Clear;
end;

function TFhirProvenance5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirProvenance5.p: TFhirProvenance;
begin
  result := (Fres as TFhirProvenance);
end;

function TFhirProvenance5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

procedure TFhirProvenance5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirConceptMapGroupElementDependsOn5 }

function TFhirConceptMapGroupElementDependsOn5.display: String;
var
  e : TFhirConceptMapGroupElementTargetDependsOn;
begin
  e := Element as TFhirConceptMapGroupElementTargetDependsOn;
  if e.value is TFhirCoding then
    result := TFhirCoding(e.value).display
  else
    result := '';
end;

function TFhirConceptMapGroupElementDependsOn5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFhirConceptMapGroupElementDependsOn5.property_: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).property_;
end;

function TFhirConceptMapGroupElementDependsOn5.system_: String;
var
  e : TFhirConceptMapGroupElementTargetDependsOn;
begin
  e := Element as TFhirConceptMapGroupElementTargetDependsOn;
  if e.value is TFhirCoding then
    result := TFhirCoding(e.value).system
  else
    result := '';
end;

function TFhirConceptMapGroupElementDependsOn5.value: String;
var
  e : TFhirConceptMapGroupElementTargetDependsOn;
begin
  e := Element as TFhirConceptMapGroupElementTargetDependsOn;
  if e.value is TFhirCoding then
    result := TFhirCoding(e.value).system
  else if e.value <> nil then
    result := e.value.ToString
  else
    result := '';
end;

{ TFHIRSubscriptionTopic5 }

function TFHIRSubscriptionTopic5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRSubscriptionTopic5.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRSubscriptionTopic5.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFHIRAttachment5 }

function TFHIRAttachment5.att: TFhirAttachment;
begin
  result := Element as TFhirAttachment;
end;

function TFHIRAttachment5.GetContentType: String;
begin
  result := att.contentType;
end;

function TFHIRAttachment5.GetData: TBytes;
begin
  result := att.data;
end;

function TFHIRAttachment5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

function TFHIRAttachment5.renderText: String;
begin
  result := '??';
end;

{ TFhirImmunization5 }

function TFhirImmunization5.code(systemUri: String): String;
var
  c : TFHIRCoding;
begin
  result := '';
  for c in imm.vaccineCode.codingList do
    if (c.system = systemUri) then
      exit(c.code);
end;

function TFhirImmunization5.GetDate: TFslDateTime;
begin
  if imm.occurrence is TFhirDateTime then
    result := (imm.occurrence as TFhirDateTime).value
  else
    result := TFslDateTime.makeNull;
end;

function TFhirImmunization5.GetLanguage: String;
begin
  result := imm.language;
end;

function TFhirImmunization5.GetLotNumber: String;
begin
  result := imm.lotNumber;
end;

function TFhirImmunization5.GetManufacturerIdSystem: String;
begin
  if (imm.manufacturer <> nil) and (imm.manufacturer.reference <> nil) and (imm.manufacturer.reference.identifier <> nil) then
    result := imm.manufacturer.reference.identifier.system
  else
    result := '';
end;

function TFhirImmunization5.GetManufacturerIdValue: String;
begin
  if (imm.manufacturer <> nil) and (imm.manufacturer.reference <> nil) and (imm.manufacturer.reference.identifier <> nil) then
    result := imm.manufacturer.reference.identifier.value
  else
    result := '';
end;

function TFhirImmunization5.GetPatient: String;
begin
  if imm.patient = nil then
    result := ''
  else
    result := imm.patient.reference;
end;

function TFhirImmunization5.GetPerformerDisplay: String;
begin
  result := '';
  if (imm.performerList.Count > 0) and (imm.performerList[0].actor <> nil) then
    result := imm.performerList[0].actor.display;
end;

function TFhirImmunization5.GetStatus: string;
begin
  result := imm.statusElement.value;
end;

function TFhirImmunization5.hasCode(systemUri, code: String): boolean;
var
  c : TFHIRCoding;
begin
  result := false;
  for c in imm.vaccineCode.codingList do
    if (c.system = systemUri) and (c.code = code) then
      exit(true);
end;

function TFhirImmunization5.imm: TFhirImmunization;
begin
  result := resource as TFhirImmunization;
end;

procedure TFhirImmunization5.setCodeBySystem(systemUri: String; code: String);
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

procedure TFhirImmunization5.SetDate(const Value: TFslDateTime);
begin
  imm.occurrence := TFHIRDateTime.create(value);
end;

function TFhirImmunization5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

procedure TFhirImmunization5.SetLanguage(const Value: String);
begin
  imm.language := value;
end;

procedure TFhirImmunization5.SetLotNumber(const Value: String);
begin
  imm.lotNumber := value;
end;

procedure TFhirImmunization5.SetManufacturerIdSystem(const Value: String);
begin
  if imm.manufacturer = nil then
    imm.manufacturer := TFhirCodeableReference.Create;
  if imm.manufacturer.reference = nil then
    imm.manufacturer.reference := TFhirReference.Create;
  if imm.manufacturer.reference.identifier = nil then
    imm.manufacturer.reference.identifier := TFhirIdentifier.Create;
  imm.manufacturer.reference.identifier.system := value;
end;

procedure TFhirImmunization5.SetManufacturerIdValue(const Value: String);
begin
  if imm.manufacturer = nil then
    imm.manufacturer := TFhirCodeableReference.Create;
  if imm.manufacturer.reference = nil then
    imm.manufacturer.reference := TFhirReference.Create;
  if imm.manufacturer.reference.identifier = nil then
    imm.manufacturer.reference.identifier := TFhirIdentifier.Create;
  imm.manufacturer.reference.identifier.value := value;
end;

procedure TFhirImmunization5.SetPatient(const Value: String);
begin
  if (imm.patient = nil) then
    imm.patient := TFhirReference.Create;
  imm.patient.reference := value;
end;

procedure TFhirImmunization5.SetPerformerDisplay(const Value: String);
var
  p : TFhirImmunizationPerformer;
begin
  if imm.performerList.Count > 0 then
    p := imm.performerList[0]
  else
    p := imm.performerList.Append;
  if p.actor = nil then
    p.actor := TFhirReference.Create;
  p.actor.display := value;
end;

procedure TFhirImmunization5.SetStatus(const Value: string);
begin
  imm.statusElement.value := value;
end;

{ TFhirIdentifier5 }

function TFhirIdentifier5.id: TFHIRIdentifier;
begin
  result := FElement as TFHIRIdentifier;
end;

function TFhirIdentifier5.renderText: String;
begin
  result := gen(element as TFhirIdentifier);
end;

function TFhirIdentifier5.GetSystem: String;
begin
  result := id.system;
end;

function TFhirIdentifier5.GetValue: String;
begin
  result := id.value;
end;

procedure TFhirIdentifier5.SetSystem(const Value: String);
begin
  id.system := value;
end;

procedure TFhirIdentifier5.SetValue(const Value: String);
begin
  id.value := value;
end;

function TFhirIdentifier5.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension5.create(extension.link);
end;

const
  identifier_use_version_to_general : array [TFhirIdentifierUseEnum] of TIdentifierUse = (iuNull, iuUsual, iuOfficial, iuTemp, iuSecondary, iuOld);
  identifier_use_general_to_version : array [TIdentifierUse] of TFhirIdentifierUseEnum = (IdentifierUseNull, IdentifierUseUsual, IdentifierUseOfficial, IdentifierUseTemp, IdentifierUseSecondary, IdentifierUseOld);

function TFhirIdentifier5.GetUse: TIdentifierUse;
begin
  result := identifier_use_version_to_general[id.use];
end;

procedure TFhirIdentifier5.SetUse(const Value: TIdentifierUse);
begin
  id.use := identifier_use_general_to_version[value];
end;

function TFhirIdentifier5.GetTypeV: TFhirCodeableConceptW;
begin
  if id.type_ = nil then
    result := nil
  else
    result := TFhirCodeableConcept5.Create(id.type_.Link);
end;

procedure TFhirIdentifier5.SetTypeV(const Value: TFhirCodeableConceptW);
begin
  if value = nil then
    id.type_ := nil
  else
    id.type_ := ((Value as TFhirCodeableConcept5).FElement as TFhirCodeableConcept).link;
end;

end.
