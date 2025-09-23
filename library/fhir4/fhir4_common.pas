unit fhir4_common;

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
  fhir_objects, fhir_common, fhir_extensions, fhir_features,
  fhir4_types, fhir4_resources, fhir4_operations, fhir4_opbase;

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
  MAP_TFilterOperator : array [TFhirFilterOperatorEnum] of TFilterOperator = (foNull, foEqual, foIsA, foDescendentOf, foIsNotA, foRegex, foIn, foNotIn, foGeneralizes, foExists);
  MAP_TFilterOperatorR : array [TFilterOperator] of TFhirFilterOperatorEnum = (filterOperatorNull, filterOperatorEqual, filterOperatorIsA, filterOperatorDescendentOf, filterOperatorIsNotA, filterOperatorRegex, filterOperatorIn, filterOperatorNotIn, filterOperatorGeneralizes, filterOperatorExists, filterOperatorNull, filterOperatorNull, filterOperatorNull);
  MAP_TFhirConceptPropertyTypeEnum : array [TFhirConceptPropertyTypeEnum] of TFhirCodeSystemPropertyType = (cptNull, cptCode, cptCoding, cptString, cptInteger, cptBoolean, cptDateTime, cptDecimal);
  MAP_TFHIRSearchParamType1 : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptNull, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri, sptSpecial);
  MAP_TFHIRSearchParamType2 : array [TFhirSearchParamType] of TFHIRSearchParamTypeEnum = (SearchParamTypeNull, SearchParamTypeNumber, SearchParamTypeDate, SearchParamTypeString, SearchParamTypeToken, SearchParamTypeReference, SearchParamTypeComposite, SearchParamTypeQuantity, SearchParamTypeUri, SearchParamTypeNull);
  MAP_TPublicationStatus : array [TPublicationStatus] of TFHIRPublicationStatusEnum = (PublicationStatusNull, PublicationStatusDraft, PublicationStatusActive, PublicationStatusRetired);
  MAP_TPublicationStatusR : array [TFHIRPublicationStatusEnum] of TPublicationStatus = (psNull, psDraft, psActive, psRetired, psNull);
  MAP_TFhirCodeSystemContentMode : array [TFhirCodeSystemContentMode] of TFhirCodeSystemContentModeEnum = (CodesystemContentModeNull, CodesystemContentModeNotPresent, CodesystemContentModeExample, CodesystemContentModeFragment, CodesystemContentModeComplete, CodesystemContentModeSupplement);
  MAP_TFhirCodeSystemContentModeR : array [TFhirCodeSystemContentModeEnum] of TFhirCodeSystemContentMode = (cscmNull, cscmNotPresent, cscmExample, cscmFragment, cscmComplete, cscmSupplement);
  MAP_TFHIRConceptEquivalence : array [TFhirConceptMapEquivalenceEnum] of TFHIRConceptEquivalence = (cmeNull, cmeRelatedto, cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact, cmeUnmatched, cmeDisjoint);
  MAP_TFHIRConceptEquivalenceR : array [TFHIRConceptEquivalence] of TFHIRConceptMapEquivalenceEnum = (ConceptMapEquivalenceNull, ConceptMapEquivalenceRelatedto, ConceptMapEquivalenceEquivalent, ConceptMapEquivalenceEqual, ConceptMapEquivalenceWider, ConceptMapEquivalenceSubsumes, ConceptMapEquivalenceNarrower, ConceptMapEquivalenceSpecializes, ConceptMapEquivalenceInexact, ConceptMapEquivalenceUnmatched, ConceptMapEquivalenceDisjoint);
  MAP_TContactType : array [TContactType] of TFhirContactPointSystemEnum = (ContactPointSystemNull, ContactPointSystemPhone, ContactPointSystemFax, ContactPointSystemEmail, ContactPointSystemPager, ContactPointSystemUrl, ContactPointSystemSms, ContactPointSystemOther);
  MAP_TContactType2 : array [TFhirContactPointSystemEnum] of TContactType = (cpsNull, cpsPhone, cpsFax, cpsEmail, cpsPager, cpsUrl, cpsSms, cpsOther);
  MAP_TSubscriptionMethod : array [TFhirSubscriptionChannelTypeEnum] of TSubscriptionMethod = (smNull, smRestHook, smWebsocket, smEmail, smSms, smChangeScript);
  MAP_TSubscriptionMethod2 : array [TSubscriptionMethod] of TFhirSubscriptionChannelTypeEnum = (SubscriptionChannelTypeNull, SubscriptionChannelTypeRestHook, SubscriptionChannelTypeWebsocket, SubscriptionChannelTypeEmail, SubscriptionChannelTypeSms, SubscriptionChannelTypeMessage);
  MAP_TSubscriptionStatus : array [TFhirSubscriptionStatusEnum] of TSubscriptionStatus = (ssNull, ssRequested, ssActive, ssError, ssOff);
  MAP_TSubscriptionStatus2 : array [TSubscriptionStatus] of TFhirSubscriptionStatusEnum = (SubscriptionStatusNull, SubscriptionStatusRequested, SubscriptionStatusActive, SubscriptionStatusError, SubscriptionStatusOff, SubscriptionStatusNull);
  BUNDLE_TYPE_TITLE : Array[TFhirBundleTypeEnum] of String = ('', 'Document', 'Message', 'Transaction', 'Transaction Response', 'Batch', 'Batch Response', 'History Record', 'Search Results', 'Resource Collection');
  MAP_TFHIRBundleType  : array [TBundleType] of TFhirBundleTypeEnum = (BundleTypeNull, BundleTypeDocument, BundleTypeMessage, BundleTypeTransaction, BundleTypeTransactionResponse, BundleTypeBatch, BundleTypeBatchResponse, BundleTypeHistory, BundleTypeSearchset, BundleTypeCollection);
  MAP_TFHIRBundleTypeR : array [TFhirBundleTypeEnum] of TBundleType = (btNull, btDocument, btMessage, btTransaction, btTransactionResponse, btBatch, btBatchResponse, btHistory, btSearchset, btCollection);
  MAP_TObservationStatus : array [TObservationStatus] of TFhirObservationStatusEnum = (ObservationStatusNull, ObservationStatusRegistered, ObservationStatusPreliminary, ObservationStatusFinal, ObservationStatusAmended, ObservationStatusCorrected, ObservationStatusCancelled, ObservationStatusEnteredInError, ObservationStatusUnknown);
  MAP_TObservationStatus2 : array [TFhirObservationStatusEnum] of TObservationStatus = (obssNull, obssRegistered, obssPreliminary, obssFinal, obssAmended, obssCorrected, obssCancelled, obssEnteredInError, obssUnknown);


type

  { TFHIRPrimitive4 }

  TFHIRPrimitive4 = class (TFHIRPrimitiveW)
  public
    function GetAsString : String; override;
    procedure SetAsString(value : String); override;
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
  end;

  { TFHIRExtension4 }

  TFHIRExtension4 = class (TFHIRExtensionW)
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

  { TFHIRCoding4 }

  TFHIRCoding4 = class (TFHIRCodingW)
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

  { TFhirCodeableConcept4 }

  TFhirCodeableConcept4 = class (TFhirCodeableConceptW)
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

  { TFhirIdentifier4 }

  TFhirIdentifier4 = class (TFhirIdentifierW)
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

  { TFhirOperationOutcome4 }

  TFhirOperationOutcome4 = class (TFhirOperationOutcomeW)
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

  { TFHIRBundleEntry4 }

  TFHIRBundleEntry4 = class (TFHIRBundleEntryW)
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

  { TFhirBinary4 }

  TFhirBinary4 = class (TFhirBinaryW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function ContentType : String; override;
    function content : TBytes; override;
  end;


  { TFHIRBundle4 }

  TFHIRBundle4 = class (TFHIRBundleW)
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

  { TFHIROperationOutcomeIssue4 }

  TFHIROperationOutcomeIssue4 = class (TFHIROperationOutcomeIssueW)
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

  { TFHIRSearchParamDefinition4 }

  TFHIRSearchParamDefinition4 = class (TFHIRSearchParamDefinitionW)
  private
    function param : TFhirCapabilityStatementRestResourceSearchParam;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function name : String; override;
    function documentation : String; override;
    function type_ : TFHIRSearchParamType; override;
  end;

  { TFhirCapabilityStatementRestResource4 }

  TFhirCapabilityStatementRestResource4 = class (TFhirCapabilityStatementRestResourceW)
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

  { TFHIRCapabilityStatement4 }

  TFHIRCapabilityStatement4 = class (TFHIRCapabilityStatementW)
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

  { TFhirElementDefinition4 }

  TFhirElementDefinition4 = class (TFhirElementDefinitionW)
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

  { TFHIRStructureDefinition4 }

  TFHIRStructureDefinition4 = class (TFhirStructureDefinitionW)
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

  { TFhirParametersParameter4 }

  TFhirParametersParameter4 = class (TFhirParametersParameterW)
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

  { TFHIRParameters4 }

  TFHIRParameters4 = class (TFHIRParametersW)
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

  { TFhirValueSetExpansionContains4 }

  TFhirValueSetExpansionContains4 = class (TFhirValueSetExpansionContainsW)
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

  { TFhirValueSetExpansion4 }

  TFhirValueSetExpansion4 = class (TFhirValueSetExpansionW)
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

  { TFhirValueSetComposeIncludeFilter4 }

  TFhirValueSetComposeIncludeFilter4 = class (TFhirValueSetComposeIncludeFilterW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getProp : String; override;
    function getOp : TFilterOperator; override;
    function getValue : String; override;
    procedure setOp(Value: TFilterOperator); override;
    procedure setProp(Value: String); override;
    procedure setValue(Value: String); override;
  end;

  { TFhirValueSetComposeIncludeConceptDesignation4 }

  TFhirValueSetComposeIncludeConceptDesignation4 = class (TFhirValueSetComposeIncludeConceptDesignationW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function language : String; override;
    function value : String; override;
    function use : TFHIRCodingW; override;
    function valueElement : TFHIRPrimitiveW; override;
  end;

  { TFhirValueSetComposeIncludeConcept4 }

  TFhirValueSetComposeIncludeConcept4 = class (TFhirValueSetComposeIncludeConceptW)
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

  { TFhirValueSetComposeInclude4 }

  TFhirValueSetComposeInclude4 = class (TFhirValueSetComposeIncludeW)
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
  end;

  { TFHIRValueSet4 }

  TFHIRValueSet4 = class (TFHIRValueSetW)
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
    function getVersionAlgorithm : TFHIRVersionAlgorithm; override;
     procedure setVersion(value : String); override;
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

  { TFHIRLookupOpRequest4 }

  TFHIRLookupOpRequest4 = class (TFHIRLookupOpRequestW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    procedure loadCoding; override;
    function coding : TFHIRCodingW; override;
    function propList : TArray<String>; override;
    function displayLanguage : String; override;
  end;

  TFHIRLookupOpRespSubProperty4 = class (TFHIRLookupOpRespSubPropertyW)
  public
    function getDescription: string; override;
    procedure setDescription(Value: string); override;
    function getValue: String; override;
    procedure setValue(Value: String); override;
  end;

  { TFHIRLookupOpRespProperty4 }

  TFHIRLookupOpRespProperty4 = class (TFHIRLookupOpRespPropertyW)
  public
    function getDescription: string; override;
    procedure setDescription(Value: string); override;
    function getValue: TFHIRObject; override;
    procedure setValue(Value: TFHIRObject); override;
    function addSubProp(name : String) : TFHIRLookupOpRespSubPropertyW; override;
  end;

  TFHIRLookupOpRespDesignation4 = class (TFHIRLookupOpRespDesignationW)
  public
    function getUse: TFHIRObject; override;
    procedure setUse(Value: TFHIRObject); override;
  end;

  { TFHIRLookupOpResponse4 }

  TFHIRLookupOpResponse4 = class (TFHIRLookupOpResponseW)
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

  { TFhirCodeSystemConceptProperty4 }

  TFhirCodeSystemConceptProperty4 = class (TFhirCodeSystemConceptPropertyW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : String; override;
    function value : TFHIRObject; override;
  end;

  { TFhirCodeSystemConceptDesignation4 }

  TFhirCodeSystemConceptDesignation4 = class (TFhirCodeSystemConceptDesignationW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function language : String; override;
    function useGen : String; override;
    function use : TFHIRCodingW; override;
    function hasUse : boolean; override;
    function value : String; override;
    function valueElement : TFHIRPrimitiveW; override;
  end;

  { TFhirCodeSystemConcept4 }

  TFhirCodeSystemConcept4 = class (TFhirCodeSystemConceptW)
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


  { TFhirCodeSystemProperty4 }

  TFhirCodeSystemProperty4 = class (TFhirCodeSystemPropertyW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : String; override;
    function uri : String; override;
    function type_ : TFhirCodeSystemPropertyType; override;
  end;

  { TFhirCodeSystem4 }

  TFhirCodeSystem4 = class (TFhirCodeSystemW)
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

  { TFhirConceptMapGroupElementDependsOn4 }

  TFhirConceptMapGroupElementDependsOn4 = class (TFhirConceptMapGroupElementDependsOnW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function property_ : String; override;
    function system_ : String; override;
    function value : String; override;
    function display : String; override;
  end;

  { TFhirConceptMapGroupElementTarget4 }

  TFhirConceptMapGroupElementTarget4 = class (TFhirConceptMapGroupElementTargetW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code: String; override;
    function equivalence : TFHIRConceptEquivalence; override;
    function comments : String; override;
    function products : TFslList<TFhirConceptMapGroupElementDependsOnW>; override;
  end;

  { TFhirConceptMapGroupElement4 }

  TFhirConceptMapGroupElement4 = class (TFhirConceptMapGroupElementW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code: String; override;
    function targets : TFslList<TFhirConceptMapGroupElementTargetW>; override;
    function targetCount : integer; override;
    function addTarget(code : String; eq : TFHIRConceptEquivalence) : TFhirConceptMapGroupElementTargetW; override;
  end;

  { TFhirConceptMapGroup4 }

  TFhirConceptMapGroup4 = class (TFhirConceptMapGroupW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function elements : TFslList<TFhirConceptMapGroupElementW>; override;
    function addElement(code : String) : TFhirConceptMapGroupElementW; override;
    function source : String; override;
    function target : String; override;
  end;

  { TFhirConceptMap4 }

  TFhirConceptMap4 = class (TFhirConceptMapW)
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
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
    function targetDesc : String; override;
  end;

  { TFHIRMeta4 }

  TFHIRMeta4 = class (TFHIRMetaW)
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

  { TFHIRAuditEvent4 }

  TFHIRAuditEvent4 = class (TFhirAuditEventW)
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

  { TFHIRSubscription4 }

  TFHIRSubscription4 = class (TFHIRSubscriptionW)
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

  { TFhirObservationComponent4 }

  TFhirObservationComponent4 = class (TFhirObservationComponentW)
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

  { TFhirObservation4 }

  TFhirObservation4 = class (TFhirObservationW)
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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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

  { TFHIRQuantity4 }

  TFHIRQuantity4 = class (TFHIRQuantityW)
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

  { TFHIRPeriod4 }

  TFHIRPeriod4 = class (TFHIRPeriodW)
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

  { TFHIRAttachment4 }

  TFHIRAttachment4 = class (TFHIRAttachmentW)
  private
    function att : TFhirAttachment;
  protected
    function GetContentType: String; override;
    function GetData: TBytes; override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function renderText : String; override;
  end;


  TFHIRSubsumesOpRequest4 = class (TFHIRSubsumesOpRequestW)
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

  TFHIRSubsumesOpResponse4 = class (TFHIRSubsumesOpResponseW)
  public
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
    function getOutcome: String; override;
    procedure setOutcome(Value: String); override;
  end;

  { TFHIRGroupCharacteristic4 }

  TFHIRGroupCharacteristic4 = class (TFHIRGroupCharacteristicW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : TFhirCodeableConceptW; override;
    function value : TFhirCodeableConceptW; override;
  end;

  { TFHIRGroup4 }

  TFHIRGroup4 = class (TFHIRGroupW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function name : String; override;
    function hasMembers : boolean; override;
    function hasCharacteristics : boolean; override;
    function characteristics : TFslList<TFHIRGroupCharacteristicW>; override;
  end;

  { TFhirPatient4 }

  TFhirPatient4 = class (TFhirPatientW)
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

  { TFhirEncounter4 }

  TFhirEncounter4 = class (TFhirEncounterW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function patientId : String; override;
    function summary : String; override;
  end;

  TFHIRStatsOpResponse4 = class (TFHIRStatsOpResponseW)
  public
    procedure addObs(obs : TFHIRResourceV); override;
    procedure load(params : TFHIRResourceV); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRResourceV; override;
  end;

  { TFHIRNamingSystem4 }

  TFHIRNamingSystem4 = class (TFHIRNamingSystemW)
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

  { TFHIRStructureMap4 }

  TFHIRStructureMap4 = class (TFHIRStructureMapW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function url : String; override;
  end;

  { TFHIRConsent4 }

  TFHIRConsent4 = class (TFHIRConsentW)
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

  { TFHIREventDefinition4 }

  TFHIREventDefinition4 = class (TFHIREventDefinitionW)
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

  { TFhirTerminologyCapabilities4 }

  TFhirTerminologyCapabilities4 = class (TFhirTerminologyCapabilitiesW)
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

  { TFHIRTestScript4 }

  TFHIRTestScript4 = class (TFHIRTestScriptW)
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
    function getContext: String; override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFhirProvenance4 }

  TFhirProvenance4 = class (TFhirProvenanceW)
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

  { TFhirImmunization4 }

  TFhirImmunization4 = class (TFhirImmunizationW)
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
  fhir4_utilities;

function interpretVersionAlgorithm(dt : TFhirType) : TFHIRVersionAlgorithm;
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


{ TFHIRPrimitive4 }

function TFHIRPrimitive4.GetAsString: String;
begin
  result := (FElement as TFHIRPrimitiveType).StringValue;
end;

procedure TFHIRPrimitive4.SetAsString(value: String);
begin
  (FElement as TFHIRPrimitiveType).StringValue := value;
end;

function TFHIRPrimitive4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

{ TFhirOperationOutcome4 }

procedure TFhirOperationOutcome4.addIssue(issue: TFhirOperationOutcomeIssueW; free : boolean);
begin
  (Fres as TFhirOperationOutcome).issueList.Add((issue.Element as TFhirOperationOutcomeIssue).link);
  if free then
    issue.free;
end;

procedure TFhirOperationOutcome4.addIssueNoId(level: TIssueSeverity; cause: TFHIRIssueType; path, message : String; code : TOpIssueCode; addIfDuplicate : boolean);
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

procedure TFhirOperationOutcome4.addIssue(level: TIssueSeverity;
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
  if (msgId <> '') then
    iss.addExtension('http://hl7.org/fhir/StructureDefinition/operationoutcome-message-id', msgid);
end;

procedure TFhirOperationOutcome4.addDiagsIssue(message: string);
var
  iss : TFhirOperationOutcomeIssue;
begin
  iss := (Fres as TFhirOperationOutcome).issueList.Append;
  iss.code := IssueTypeInformational;
  iss.severity := IssueSeverityInformation;
  iss.diagnostics := message;
end;

function TFhirOperationOutcome4.code: TFhirIssueType;
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

function TFhirOperationOutcome4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirOperationOutcome4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirOperationOutcome4.hasErrors: boolean;
begin
  result := (Fres as TFhirOperationOutcome).hasErrors;
end;

function TFhirOperationOutcome4.hasIssues: boolean;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  result := not op.issueList.IsEmpty;
end;

function TFhirOperationOutcome4.hasText: boolean;
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

function TFhirOperationOutcome4.issueCount: integer;
begin
  result := (resource as TFhirOperationOutcome).issueList.Count;
end;

function TFhirOperationOutcome4.issues: TFslList<TFhirOperationOutcomeIssueW>;
var
  iss : TFhirOperationOutcomeIssue;
begin
  result := TFslList<TFhirOperationOutcomeIssueW>.Create;
  for iss in (resource as TFhirOperationOutcome).issueList do
    result.Add(TFHIROperationOutcomeIssue4.Create(iss.Link));
end;

function TFhirOperationOutcome4.rule(level: TIssueSeverity; source: String; typeCode: TFhirIssueType; path: string; test: boolean; msg: string): boolean;
begin
  result := (resource as TFhirOperationOutcome).rule(ISSUE_SEVERITY_MAP2[level], source, ExceptionTypeTranslations[typeCode], path, test, msg);
end;

procedure TFhirOperationOutcome4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFhirOperationOutcome4.severity: TIssueSeverity;
begin
  if (resource as TFhirOperationOutcome).issueList.Count > 0 then
    result := ISSUE_SEVERITY_MAP[(resource as TFhirOperationOutcome).issueList[0].severity]
  else
    result := isFatal;
end;

function TFhirOperationOutcome4.text: String;
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

{ TFHIRBundle4 }

procedure TFHIRBundle4.addEntries(bnd: TFHIRResourceV);
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  bundle.entryList.AddAll(b.entryList);
end;

procedure TFHIRBundle4.addEntry(bnd: TFhirBundleEntryW; first: boolean);
begin
  if first then
    bundle.entryList.InsertItem(0, bnd.element.link as TFHIRBundleEntry)
  else
    bundle.entryList.AddItem(bnd.element.link as TFHIRBundleEntry);
end;

procedure TFHIRBundle4.addEntry(url: String; bnd: TFhirResourceV);
var
  e : TFHIRBundleEntry;
begin
  e := bundle.entryList.Append;
  e.fullUrl := url;
  e.resource := bnd as TFhirResource;
end;

function TFHIRBundle4.addEntry: TFhirBundleEntryW;
begin
  result := TFhirBundleEntry4.Create(bundle.entryList.append.link);
end;

function TFHIRBundle4.bundle: TFhirBundle;
begin
  result := resource as TFHIRBundle;
end;

function TFHIRBundle4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

procedure TFHIRBundle4.clearLinks;
begin
  bundle.link_List.Clear;
end;

function TFHIRBundle4.count(rtype: String): Integer;
var
  be : TFhirBundleEntry;
begin
  result := 0;
  for be in bundle.entryList do
    if (be.resource <> nil) and ((rtype = '') or (rtype = be.resource.fhirType)) then
      inc(result);
end;

function TFHIRBundle4.entries: TFslList<TFhirBundleEntryW>;
var
  be : TFHIRBundleEntry;
begin
  result := TFslList<TFhirBundleEntryW>.Create;
  try
    for be in bundle.entryList do
      result.Add(TFhirBundleEntry4.Create(be.Link));
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRBundle4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRBundle4.getLastUpdated: TFslDateTime;
begin
  if bundle.meta <> nil then
    result := TFslDateTime.makeNull
  else
    result := bundle.meta.lastUpdated;
end;

function TFHIRBundle4.getLink(rel: String): String;
begin
  result := bundle.Links[rel];
end;

procedure TFHIRBundle4.listLinks(links: TFslStringDictionary);
var
  bl : TFhirBundleLink;
begin
  links.Clear;
  for bl in bundle.link_List do
    links.AddOrSetValue(bl.relation, bl.url);
end;

function TFHIRBundle4.moveToFirst(res: TFhirResourceV): TFhirBundleEntryW;
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
  result := TFHIRBundleEntry4.Create(bundle.entryList[0].Link);
end;

function TFHIRBundle4.next(bnd: TFHIRResourceV): String;
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  result := b.Links['next'];
end;

procedure TFHIRBundle4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRBundle4.setLastUpdated(Value: TFslDateTime);
begin
  if bundle.meta = nil then
    bundle.meta := TFHIRMeta.Create;
  bundle.meta.lastUpdated := value;
end;

procedure TFHIRBundle4.setLink(rel: String; const Value: String);
begin
  bundle.Links[rel] := value;
end;

procedure TFHIRBundle4.setTimestamp(Value: TFslDateTime);
begin
  bundle.timestamp := value;
end;

procedure TFHIRBundle4.setTotal(Value: integer);
begin
  bundle.total := inttostr(value);
end;

procedure TFHIRBundle4.setType(value: TBundleType);
begin
  bundle.type_ := MAP_TFHIRBundleType[value];
end;

function TFHIRBundle4.title: String;
begin
  result := BUNDLE_TYPE_TITLE[bundle.type_];
end;

function TFHIRBundle4.getTimestamp: TFslDateTime;
begin
  if bundle.timestampElement <> nil then
    result := bundle.timestamp
  else if bundle.meta <> nil then
    result := bundle.meta.lastUpdated
  else
    result := TFslDateTime.makeNull;
end;

function TFHIRBundle4.getTotal: integer;
begin
  result := StrToIntDef(bundle.total, -1);
end;

function TFHIRBundle4.getType: TBundleType;
begin
  result := MAP_TFHIRBundleTypeR[bundle.type_];
end;

{ TFHIROperationOutcomeIssue4 }

function TFHIROperationOutcomeIssue4.display: String;
var
  i : TFHIROperationOutcomeIssue;
begin
  i := issue;
  result := i.diagnostics;
  if (i.details <> nil) and (i.details.text <> '') then
    result := i.details.text;
end;

procedure TFHIROperationOutcomeIssue4.addCode(systemUri, code: String);
begin
  issue.details.addCoding(systemUri, '', code, '');
end;

function TFHIROperationOutcomeIssue4.issue: TFHIROperationOutcomeIssue;
begin
  result := Element as TFHIROperationOutcomeIssue;
end;

function TFHIROperationOutcomeIssue4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIROperationOutcomeIssue4.severity: TIssueSeverity;
begin
  result := ISSUE_SEVERITY_MAP[issue.severity];
end;

function TFHIROperationOutcomeIssue4.getDiagnostics: String;
begin
  result := issue.diagnostics;
end;

procedure TFHIROperationOutcomeIssue4.setDiagnostics(Value: String);
begin
  issue.diagnostics := value;
end;


{ TFHIRCapabilityStatement4 }

procedure TFHIRCapabilityStatement4.addInstantiates(url: String);
begin
  statement.instantiatesList.Append.value := url;
end;

procedure TFHIRCapabilityStatement4.addTxFeature(version: String);
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

procedure TFHIRCapabilityStatement4.addOperation(name, url: String);
var
  t : TFhirCapabilityStatementRestResourceOperation;
begin
  t := statement.restList[0].operationList.append;
  t.name := name;
  t.definition := url;
end;

function TFHIRCapabilityStatement4.addResource(code: String): TFhirCapabilityStatementRestResourceW;
begin
  result := TFhirCapabilityStatementRestResource4.Create(statement.restList[0].resourceList.append.link);
  result.code := code;
end;

procedure TFHIRCapabilityStatement4.addSmartExtensions(authorize, token, register, manage: String; caps: array of String
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

procedure TFHIRCapabilityStatement4.contact(kind: TContactType; value: String);
var
  c : TFhirContactPoint;
  ct : TFhirConformanceContact;
begin
  ct := statement.contactList.Append;
  c := ct.telecomList.Append;
  c.system := MAP_TContactType[kind];
  c.value := 'http://healthintersections.com.au/';
end;

procedure TFHIRCapabilityStatement4.defineFeatures(features: TFslList<TFHIRFeature>);
begin
end;

function TFHIRCapabilityStatement4.getURL: String;
begin
  result := statement.url;
end;

function TFHIRCapabilityStatement4.hasFormat(fmt: String): boolean;
begin
  result := statement.formatList.hasCode(fmt);
end;

function TFHIRCapabilityStatement4.hasRest: boolean;
begin
  result := statement.restList.Count > 0;
end;

function TFHIRCapabilityStatement4.hasSecurity(system, code: String): boolean;
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

procedure TFHIRCapabilityStatement4.impl(url, desc: String);
begin
  if statement.implementation_ = nil then
    statement.implementation_ := TFhirCapabilityStatementImplementation.Create;
  statement.implementation_.description := desc;
  statement.implementation_.url := url;
end;

procedure TFHIRCapabilityStatement4.listSearchParams(name: String; list: TFslList<TFHIRSearchParamDefinitionW>);
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
          list.Add(TFHIRSearchParamDefinition4.Create(sp.Link))
      else
      begin
        for rr in r.resourceList do
        begin
          if CODES_TFHIRResourceTypesEnum[rr.type_] = name then
            for sp in rr.searchParamList do
              list.Add(TFHIRSearchParamDefinition4.Create(sp.Link))
        end;
      end;
    end;
  end;
end;

procedure TFHIRCapabilityStatement4.listTypes(interactions: TFHIRInteractions; names: TStrings);
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

procedure TFHIRCapabilityStatement4.readSmartExtension(var authorize, token, register: String);
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

procedure TFHIRCapabilityStatement4.setUrl(Value: String);
begin
  statement.url := value;
end;

function TFHIRCapabilityStatement4.getName : String;
begin
  result := statement.Name;
end;

procedure TFHIRCapabilityStatement4.setName(value : String);
begin
  statement.Name := value;
end;

function TFHIRCapabilityStatement4.getTitle : String;
begin
  result := statement.Title;
end;

procedure TFHIRCapabilityStatement4.setTitle(value : String);
begin
  statement.Title := value;
end;

function TFHIRCapabilityStatement4.getVersion : String;
begin
  result := statement.Version;
end;

procedure TFHIRCapabilityStatement4.setVersion(value : String);
begin
  statement.Version := value;
end;

procedure TFHIRCapabilityStatement4.software(name, version, release: String);
begin
  if statement.software = nil then
    statement.software := TFhirCapabilityStatementSoftware.Create;
  statement.software.name := name;
  statement.software.version := version;
  statement.software.releaseDate := TFslDateTime.fromHL7(release);
end;

function TFHIRCapabilityStatement4.getDescription : String;
begin
  result := statement.Description;
end;

function TFHIRCapabilityStatement4.getFhirVersion: string;
begin
  result := CODES_TFhirFHIRVersionEnum[statement.fhirVersion];
end;

procedure TFHIRCapabilityStatement4.setDescription(value : String);
begin
  statement.Description := value;
end;

procedure TFHIRCapabilityStatement4.setFhirVersion(Value: string);
begin
  statement.fhirVersion := TFhirFHIRVersionEnum(StringArrayIndexOfSensitive(CODES_TFhirFHIRVersionEnum, value));
end;

function TFHIRCapabilityStatement4.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[statement.Status];
end;

procedure TFHIRCapabilityStatement4.setStatus(Value: TPublicationStatus);
begin
  statement.Status := MAP_TPublicationStatus[value];
end;

procedure TFHIRCapabilityStatement4.fmt(mt: String);
begin
  statement.formatList.Append.value := mt;
end;

function TFHIRCapabilityStatement4.getDate: TFslDateTime;
begin
  result := statement.Date;
end;

procedure TFHIRCapabilityStatement4.setDate(Value: TFslDateTime);
begin
  statement.Date := value;
end;


procedure TFHIRCapabilityStatement4.standardServer(ts, ws, pv, cv, iv: String; transactions, search, history : boolean);
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

function TFHIRCapabilityStatement4.statement: TFhirCapabilityStatement;
begin
  result := FRes as TFHIRCapabilityStatement;
end;

function TFHIRCapabilityStatement4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRCapabilityStatement4.supportsType(name: String; interaction: TFHIRInteraction): boolean;
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

function TFHIRCapabilityStatement4.getKind: TCapabilityStatementKind;
begin
  case statement.kind of
    CapabilityStatementKindInstance : result := cskInstance;
    CapabilityStatementKindCapability : result := cskCapability;
    CapabilityStatementKindRequirements : result := cskRequirements;
  else
    result := cskNull;
  end;
end;

function TFHIRCapabilityStatement4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRCapabilityStatement4.setKind(Value: TCapabilityStatementKind);
begin
  case value of
    cskInstance : statement.kind := CapabilityStatementKindInstance;
    cskCapability : statement.kind := CapabilityStatementKindCapability;
    cskRequirements : statement.kind := CapabilityStatementKindRequirements;
  else
    statement.kind := CapabilityStatementKindNull;
  end;
end;

procedure TFHIRCapabilityStatement4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRCapabilityStatement4.getAcceptUnknown: TCapabilityStatementAcceptUnknown;
begin
  result := csauNull;
end;

procedure TFHIRCapabilityStatement4.setAcceptUnknown(const Value: TCapabilityStatementAcceptUnknown);
begin
end;


{ TFhirParametersParameter4 }

function TFhirParametersParameter4.addParam(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter4.Create(parameter.partList.Append.link);
  TFhirParametersParameter4(result).parameter.name := name;
  PartList.Add(result);
end;

procedure TFhirParametersParameter4.addParam(name: String; value: TFHIRObject);
begin
  parameter.AddParameter(name).value := value as TFHIRType;
end;

procedure TFhirParametersParameter4.addParamBool(name: String; value: boolean);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = LCBooleanToString(value)) then
      exit;
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFhirParametersParameter4.addParamCode(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFhirParametersParameter4.addParamStr(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

procedure TFhirParametersParameter4.addParamUri(name, value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFhirParametersParameter4.addParamCanonical(name, value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCanonical.Create(value);
end;

function TFhirParametersParameter4.getParameterParameter(name: String): TFhirParametersParameterW;
var
  t : TFhirParametersParameterW;
  s : String;
begin
  if FList = nil then
    populateList;
  result := nil;
  for t in FList do
    if t.name = name then
    begin
      result := t;
      break;
    end;
  s := result.asJson;
end;

function TFhirParametersParameter4.getResource: TFHIRResourceV;
begin
  result := parameter.resource;
end;

function TFhirParametersParameter4.getResourceParameter(name: String): TFHIRResourceV;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter4(t).parameter.resource);
end;

function TFhirParametersParameter4.getStringParameter(name: String): String;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := '';
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter4(t).parameter.value.primitiveValue);
end;

function TFhirParametersParameter4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirParametersParameter4.getValue: TFHIRObject;
begin
  result := parameter.value;
end;

function TFhirParametersParameter4.hasResource: boolean;
begin
  result := parameter.resource <> nil;
end;

function TFhirParametersParameter4.hasValue: boolean;
begin
  result := parameter.value <> nil;
end;

function TFhirParametersParameter4.name: String;
begin
  result := parameter.name;
end;

function TFhirParametersParameter4.parameter: TFhirParametersParameter;
begin
  result := Element as TFhirParametersParameter;
end;

procedure TFhirParametersParameter4.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.partList do
    FList.Add(TFhirParametersParameter4.Create(t.Link));
end;

procedure TFhirParametersParameter4.setResource(Value: TFHIRResourceV);
begin
  parameter.resource := value as TFhirResource;
end;

procedure TFhirParametersParameter4.setValue(Value: TFHIRObject);
begin
  parameter.value := value as TFHIRType;
end;

function TFhirParametersParameter4.valueString: String;
begin
  if (parameter.value = nil) or (not parameter.value.isPrimitive) then
    result := ''
  else
    result := parameter.value.primitiveValue;
end;

{ TFHIRParameters4 }

function TFHIRParameters4.addParam(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter4.Create(parameter.parameterList.Append.link);
  TFhirParametersParameter4(result).parameter.name := name;
  ParameterList.Add(result);
end;

procedure TFHIRParameters4.addParam(name: String; value: TFHIRObject);
begin
  parameter.AddParameter(name).value := value as TFHIRType;
end;

procedure TFHIRParameters4.addParamBool(name: String; value: boolean);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = LCBooleanToString(value)) then
      exit;
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFHIRParameters4.addParamCode(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFHIRParameters4.addParamUri(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFHIRParameters4.addParamCanonical(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCanonical.Create(value);
end;

procedure TFHIRParameters4.addParamStr(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

function TFHIRParameters4.bool(name: String): boolean;
begin
  result := parameter.bool[name];
end;

function TFHIRParameters4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRParameters4.getParameter(name: String): TFhirParametersParameterW;
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

function TFHIRParameters4.names: String;
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

function TFHIRParameters4.has(name: String): boolean;
begin
  result := parameter.hasParameter(name);
end;

function TFHIRParameters4.obj(name: String): TFHIRObject;
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

function TFHIRParameters4.parameter: TFhirParameters;
begin
  result := Resource as TFhirParameters;
end;

procedure TFHIRParameters4.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.parameterList do
    FList.Add(TFhirParametersParameter4.Create(t.Link));
end;

function TFHIRParameters4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

procedure TFHIRParameters4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRParameters4.str(name: String): String;
begin
  result := parameter.str[name];
end;

{ TFHIRStructureDefinition4 }

function TFHIRStructureDefinition4.elements: TFslList<TFHIRElementDefinitionW>;
var
  ed : TFhirElementDefinition;
begin
  result := TFslList<TFHIRElementDefinitionW>.Create;
  try
    for ed in sd.snapshot.elementList do
      result.Add(TFhirElementDefinition4.Create(ed.Link));
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStructureDefinition4.getDefinition(id: String; source: TElementDefinitionSourceOption): TFHIRElementDefinitionW;
var
  ed : TFhirElementDefinition;
begin
  result := nil;
  if (source in [edsSNAPSHOT, edsEITHER]) and (sd.snapshot <> nil) then
    for ed in sd.snapshot.elementList do
      if ed.id = id then
        exit(TFHIRElementDefinition4.Create(ed.Link));

  if (source in [edsDIFF, edsEITHER]) and (sd.differential <> nil) then
    for ed in sd.differential.elementList do
      if ed.id = id then
        exit(TFHIRElementDefinition4.Create(ed.Link));
end;

function TFHIRStructureDefinition4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRStructureDefinition4.kind: TStructureDefinitionKind;
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
    raise EFHIRException.Create('unhandled value');
  end;
end;

function TFHIRStructureDefinition4.name: String;
begin
  result := sd.name;
end;

function TFHIRStructureDefinition4.sd: TFhirStructureDefinition;
begin
  result := resource as TFhirStructureDefinition;
end;

function TFHIRStructureDefinition4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

procedure TFHIRStructureDefinition4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRStructureDefinition4.type_: String;
begin
  result := sd.type_;
end;

function TFHIRStructureDefinition4.url: String;
begin
  result := sd.url;
end;

{ TFHIRSearchParamDefinition4 }

function TFHIRSearchParamDefinition4.documentation: String;
begin
  result := param.documentation;
end;

function TFHIRSearchParamDefinition4.name: String;
begin
  result := param.name;
end;

function TFHIRSearchParamDefinition4.param: TFhirCapabilityStatementRestResourceSearchParam;
begin
  result := FElement as TFhirCapabilityStatementRestResourceSearchParam;
end;

function TFHIRSearchParamDefinition4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRSearchParamDefinition4.type_: TFHIRSearchParamType;
begin
  result := MAP_SearchParamType[param.type_];
end;

{ TFhirElementDefinition4 }

function TFhirElementDefinition4.binding: TElementDefinitionBinding;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  if b = nil then
    result := edbNone
  else
    result := MAP_ELEMENT_DEFINITION_BINDING[b.strength];
end;

function TFhirElementDefinition4.defn: String;
begin
  result := edefn.definition;
end;

function TFhirElementDefinition4.edefn: TFhirElementDefinition;
begin
  result := element as TFhirElementDefinition;
end;

function TFhirElementDefinition4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirElementDefinition4.explicitTypeName: String;
begin
  result := edefn.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-explicit-type-name');
end;

function TFhirElementDefinition4.isSummary: boolean;
begin
  result := edefn.isSummary;
end;

function TFhirElementDefinition4.max: integer;
begin
  if edefn.max = '*' then
    result := MaxInt
  else
    result := StrToInt(edefn.max);
end;

function TFhirElementDefinition4.min: integer;
begin
  result := StrToInt(edefn.min);
end;

function TFhirElementDefinition4.path: String;
begin
  result := edefn.path;
end;

function TFhirElementDefinition4.typeList: TArray<String>;
var
  ed : TFhirElementDefinition;
  i : integer;
begin
  ed := edefn;
  Setlength(result, ed.type_List.Count);
  for i := 0 to ed.type_List.Count - 1 do
    result[i] := ed.type_List[i].code;
end;

function TFhirElementDefinition4.types: String;
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

function TFhirElementDefinition4.valueSet: String;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  if b <> nil then
    result := b.valueSet
  else
    result := '';
end;

{ TFHIRBundleEntry4 }

function TFHIRBundleEntry4.entry: TFhirBundleEntry;
begin
  result := element as TFhirBundleEntry;
end;

function TFHIRBundleEntry4.getRequestMethod: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := CODES_TFhirHttpVerbEnum[entry.request.method];
end;

function TFHIRBundleEntry4.getRequestUrl: String;
begin
  if entry.request = nil then
    result := ''
  else
    result :=  entry.request.url;
end;

function TFHIRBundleEntry4.getResource: TFHIRResourceV;
begin
  result := entry.resource;
end;

function TFHIRBundleEntry4.getResponseDate: TFslDateTime;
begin
  if entry.response = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.response.lastModified;
end;

function TFHIRBundleEntry4.getResponseStatus: String;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.status;
end;

function TFHIRBundleEntry4.getSearchMode: TFHIRBundleEntrySearchMode;
begin
  if entry.search = nil then
    result := smUnknown
  else
    result := MAP_SEARCH_MODE[entry.search.mode];
end;

function TFHIRBundleEntry4.getSearchMpiMatch: String;
begin
  if entry.search = nil then
    result := ''
  else
    result := entry.search.getExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match')
end;

function TFHIRBundleEntry4.getSearchScore: String;
begin
  if entry.search = nil then
    result := ''
  else
    result := entry.search.score;
end;

function TFHIRBundleEntry4.getURL: String;
begin
  result := entry.fullUrl;
end;

procedure TFHIRBundleEntry4.setRequestMethod(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.Create;
  entry.request.method := TFhirHttpVerbEnum(ord(StringArrayIndexOfSensitive(CODES_TFhirHttpVerbEnum, value)));
end;

procedure TFHIRBundleEntry4.setRequestUrl(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.Create;
  entry.request.url := value;
end;

procedure TFHIRBundleEntry4.setResource(Value: TFHIRResourceV);
begin
  entry.resource := value as TFHIRResource;
end;

procedure TFHIRBundleEntry4.setResponseDate(Value: TFslDateTime);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.lastModified := value;
end;

procedure TFHIRBundleEntry4.setResponseStatus(Value: String);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.status := value;
end;

procedure TFHIRBundleEntry4.setSearchMode(Value: TFHIRBundleEntrySearchMode);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.mode := MAP_SEARCH_MODE2[value];
end;

procedure TFHIRBundleEntry4.setSearchMpiMatch(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.setExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match', value)
end;

procedure TFHIRBundleEntry4.setSearchScore(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.score := value;
end;

procedure TFHIRBundleEntry4.setUrl(Value: String);
begin
  entry.fullUrl := value;
end;

function TFHIRBundleEntry4.getLink(rel: String): String;
begin
  result := entry.Links[rel];
end;

procedure TFHIRBundleEntry4.setLink(rel: String; const Value: String);
begin
  entry.Links[rel] := value;
end;

function TFHIRBundleEntry4.getrequestIfNoneExist: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.ifNoneExist;
end;

procedure TFHIRBundleEntry4.setrequestIfNoneExist(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.ifNoneExist := value;
end;

function TFHIRBundleEntry4.getrequestIfMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfMatch;
end;

function TFHIRBundleEntry4.getrequestIfModifiedSince: TFslDateTime;
begin
  if entry.request = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.request.ifModifiedSince;
end;

procedure TFHIRBundleEntry4.setrequestIfMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.IfMatch := value;
end;

procedure TFHIRBundleEntry4.setrequestIfModifiedSince(Value: TFslDateTime);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.ifModifiedSince := value;
end;

function TFHIRBundleEntry4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRBundleEntry4.getrequestIfNoneMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfNoneMatch;
end;

procedure TFHIRBundleEntry4.setrequestIfNoneMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.IfNoneMatch := value;
end;

function TFHIRBundleEntry4.getResponseETag: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.etag;
end;

procedure TFHIRBundleEntry4.setResponseETag(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.ETag := value;
end;

function TFHIRBundleEntry4.getResponseLocation: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.Location;
end;

procedure TFHIRBundleEntry4.setResponseLocation(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.Location := value;
end;

{ TFHIRValueSet4 }

function TFHIRValueSet4.addInclude: TFhirValueSetComposeIncludeW;
begin
  if vs.compose = nil then
    vs.compose := TFhirValueSetCompose.Create;
  result := TFhirValueSetComposeInclude4.Create(vs.compose.includeList.Append.Link);
end;

function TFHIRValueSet4.checkCompose(place, role: String): boolean;
begin
  result := vs.compose <> nil;
  if result then
    vs.compose.checkNoModifiers(place, role, []);
end;

function TFHIRValueSet4.getComposeExtensions: TFslList<TFHIRExtensionW>;
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
          result.add(TFHIRExtension4.Create(ext.link));
      finally
        list.free;
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValueSet4.checkExpansion(place, role: String): boolean;
begin
  result := vs.expansion <> nil;
  if result then
    vs.expansion.checkNoModifiers(place, role, []);
end;

procedure TFHIRValueSet4.clearDefinition;
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

procedure TFHIRValueSet4.clearDefinitionExtensions(exemptUrls: TStringArray);
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

destructor TFHIRValueSet4.Destroy;
begin
  FExp.free;
  inherited;
end;

function TFHIRValueSet4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRValueSet4.excludes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  if vs.compose = nil then
    result := TFslList<TFhirValueSetComposeIncludeW>.create
  else
  begin
    result := TFslList<TFhirValueSetComposeIncludeW>.Create(vs.compose.excludeList.Count);
    for c in vs.compose.excludeList do
      result.Add(TFhirValueSetComposeInclude4.Create(c.Link));
  end;
end;

function TFHIRValueSet4.expansion: TFhirValueSetExpansionW;
begin
  if (FExp = nil) and (vs.expansion <> nil) then
    FExp := TFhirValueSetExpansion4.Create(vs.expansion.Link);
  result := FExp;
end;

function TFHIRValueSet4.forceExpansion: TFhirValueSetExpansionW;
begin
  if (vs.expansion = nil) then
    vs.expansion := TFhirValueSetExpansion.Create;
  vs.expansion.timestamp := TFslDateTime.makeUTC;
  vs.expansion.identifier := NewGuidURN;
  vs.expansion.parameterList.Clear;
  vs.expansion.containsList.Clear;
  result := expansion;
end;

function TFHIRValueSet4.getContext: String;
begin
  result := vs.context;
end;

function TFHIRValueSet4.getDate: TFslDateTime;
begin
  result := vs.date;
end;

function TFHIRValueSet4.hasExpansion: boolean;
begin
  result := vs.expansion <> nil;
end;

function TFHIRValueSet4.excludeInactives: boolean;
begin
  result := (vs.compose.inactiveElement <> nil) and not vs.compose.inactive;
end;

function TFHIRValueSet4.imports: TArray<String>;
begin
  SetLength(result, 0);
end;

function TFHIRValueSet4.includes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  if vs.compose = nil then
    result := TFslList<TFhirValueSetComposeIncludeW>.create
  else
  begin
    result := TFslList<TFhirValueSetComposeIncludeW>.Create(vs.compose.includeList.Count);
    for c in vs.compose.includeList do
      result.Add(TFhirValueSetComposeInclude4.Create(c.Link));
  end;
end;

procedure TFHIRValueSet4.setDate(Value: TFslDateTime);
begin
  vs.date := value;
end;

procedure TFHIRValueSet4.setUrl(value: String);
begin
  vs.url := value;
end;

function TFHIRValueSet4.getURL: String;
begin
  result := vs.url;
end;

procedure TFHIRValueSet4.setName(value: String);
begin
  vs.Name := value;
end;

procedure TFHIRValueSet4.setPublisher(value: String);
begin
  vs.publisher := value;
end;

function TFHIRValueSet4.getTitle: String;
begin
  result := vs.title;
end;

procedure TFHIRValueSet4.setTitle(value: String);
begin
  vs.title := value;
end;

procedure TFHIRValueSet4.setStatus(Value: TPublicationStatus);
begin
  vs.status := MAP_TPublicationStatus[value];
end;

function TFHIRValueSet4.getName: String;
begin
  result := vs.Name;
end;

function TFHIRValueSet4.getPublisher: String;
begin
  result := vs.publisher;
end;

function TFHIRValueSet4.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[vs.status];
end;

procedure TFHIRValueSet4.setDescription(value: String);
begin
  vs.Description := value;
end;

procedure TFHIRValueSet4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRValueSet4.getDescription: String;
begin
  result := vs.Description;
end;

function TFHIRValueSet4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRValueSet4.setVersion(value: String);
begin
  vs.Version := value;
end;

function TFHIRValueSet4.source: String;
begin
  result := vs.source;
end;

function TFHIRValueSet4.findContains(systemUri, version, code: String): TFhirValueSetExpansionContainsW;
var
  cc : TFhirValueSetExpansionContains;
begin
  cc := vs.findContains(systemuri, version, code);
  if (cc) = nil then
    result := nil
  else
    result := TFhirValueSetExpansionContains4.Create(cc.link);
end;

function TFHIRValueSet4.getExperimental: boolean;
begin
  result := vs.experimental;
end;

procedure TFHIRValueSet4.setExperimental(value: boolean);
begin
  vs.experimental := value;
end;

function TFHIRValueSet4.getVersion: String;
begin
  result := vs.Version;
end;

function TFHIRValueSet4.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (vs.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(vs.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
end;

function TFHIRValueSet4.vs: TFhirValueSet;
begin
  result := Resource as TFHIRValueSet;
end;


{ TFhirValueSetComposeInclude4 }

function TFhirValueSetComposeInclude4.addConcept: TFhirValueSetComposeIncludeConceptW;
begin
  result := TFhirValueSetComposeIncludeConcept4.Create((Element as TFhirValueSetComposeInclude).ConceptList.Append.Link);
end;

function TFhirValueSetComposeInclude4.addFilter: TFhirValueSetComposeIncludeFilterW;
begin
  result := TFhirValueSetComposeIncludeFilter4.Create((Element as TFhirValueSetComposeInclude).FilterList.Append.Link);
end;

function TFhirValueSetComposeInclude4.concepts: TFslList<TFhirValueSetComposeIncludeConceptW>;
var
  i : TFhirValueSetComposeIncludeConcept;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptW>.Create((Element as TFhirValueSetComposeInclude).ConceptList.Count);
  for i in (Element as TFhirValueSetComposeInclude).ConceptList do
    result.Add(TFhirValueSetComposeIncludeConcept4.Create(i.Link));
end;

function TFhirValueSetComposeInclude4.filterCount: integer;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count;
end;

function TFhirValueSetComposeInclude4.conceptCount: integer;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count;
end;

function TFhirValueSetComposeInclude4.filters: TFslList<TFhirValueSetComposeIncludeFilterW>;
var
  i : TFhirValueSetComposeIncludeFilter;
begin
  result := TFslList<TFhirValueSetComposeIncludeFilterW>.Create((Element as TFhirValueSetComposeInclude).filterList.Count);
  for i in (Element as TFhirValueSetComposeInclude).filterList do
    result.Add(TFhirValueSetComposeIncludeFilter4.Create(i.Link));
end;

function TFhirValueSetComposeInclude4.hasConcepts: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count > 0;
end;

function TFhirValueSetComposeInclude4.hasFilters: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count > 0;
end;

function TFhirValueSetComposeInclude4.hasValueSets: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).valueSetList.Count > 0;
end;

procedure TFhirValueSetComposeInclude4.setSystem(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).system := value;
end;

procedure TFhirValueSetComposeInclude4.setVersion(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).version := value;
end;

function TFhirValueSetComposeInclude4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirValueSetComposeInclude4.getSystem: String;
begin
  result := (Element as TFhirValueSetComposeInclude).system;
end;

function TFhirValueSetComposeInclude4.valueSets: TArray<String>;
var
  i : integer;
begin
  SetLength(result, TFhirValueSetComposeInclude(element).valueSetList.count);
  for i := 0 to TFhirValueSetComposeInclude(element).valueSetList.count - 1 do
    result[i] :=  TFhirValueSetComposeInclude(element).valueSetList[i].value;
end;

function TFhirValueSetComposeInclude4.getVersion: String;
begin
  result := (Element as TFhirValueSetComposeInclude).version;
end;

{ TFhirValueSetComposeIncludeFilter4 }

function TFhirValueSetComposeIncludeFilter4.getOp: TFilterOperator;
begin
  result := MAP_TFilterOperator[(Element as TFhirValueSetComposeIncludeFilter).op];
end;

function TFhirValueSetComposeIncludeFilter4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirValueSetComposeIncludeFilter4.getProp: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).property_;
end;

function TFhirValueSetComposeIncludeFilter4.getValue: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).value;
end;

procedure TFhirValueSetComposeIncludeFilter4.setOp(Value: TFilterOperator);
begin
  (Element as TFhirValueSetComposeIncludeFilter).op := MAP_TFilterOperatorR[Value];

end;

procedure TFhirValueSetComposeIncludeFilter4.setProp(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).property_ := value;
end;

procedure TFhirValueSetComposeIncludeFilter4.setValue(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).value := value;
end;

{ TFhirValueSetComposeIncludeConcept4 }

function TFhirValueSetComposeIncludeConcept4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirValueSetComposeIncludeConcept4.getCode: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).code;
end;

function TFhirValueSetComposeIncludeConcept4.designations: TFslList<TFhirValueSetComposeIncludeConceptDesignationW>;
var
  item : TFhirValueSetComposeIncludeConceptDesignation;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptDesignationW>.Create;
  for item in (Element as TFhirValueSetComposeIncludeConcept).designationList do
    result.Add(TFhirValueSetComposeIncludeConceptDesignation4.Create(item.Link));
end;

function TFhirValueSetComposeIncludeConcept4.getDisplay: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).display;
end;

procedure TFhirValueSetComposeIncludeConcept4.setCode(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).code := Value;
end;

procedure TFhirValueSetComposeIncludeConcept4.setDisplay(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).display := Value;
end;

function TFhirValueSetComposeIncludeConcept4.displayElement: TFHIRPrimitiveW;
begin
  if (Element as TFhirValueSetComposeIncludeConcept).displayElement = nil then
    result := nil
  else
  begin
    result := TFHIRPrimitive4.Create((Element as TFhirValueSetComposeIncludeConcept).displayElement.link);
  end;
end;

function TFhirValueSetComposeIncludeConcept4.GetItemWeight: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).getExtensionString(EXT_ORDINAL_VALUE);
  if result = '' then
    result := (Element as TFhirValueSetComposeIncludeConcept).getExtensionString(EXT_ITEM_WEIGHT);
end;

procedure TFhirValueSetComposeIncludeConcept4.SetItemWeight(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).setExtensionString(EXT_ORDINAL_VALUE, value);
end;

{ TFHIRLookupOpResponse4 }

function TFHIRLookupOpResponse4.addDesignation(lang, system, code, display, value: string): TFHIRLookupOpRespDesignationW;
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
    result := TFHIRLookupOpRespDesignation4.Create(p.Link);
    list.add(result);
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse4.addDesignation(lang, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.Create;
  try
    p.language := lang;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation4.Create(p.Link);
    list.add(result);
  finally
    p.free;
  end;
end;

procedure TFHIRLookupOpResponse4.addExtension(name: String; value: boolean);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

procedure TFHIRLookupOpResponse4.addExtension(name, value: String);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

function TFHIRLookupOpResponse4.addProp(name: string): TFHIRLookupOpRespPropertyW;
var
  p : TFHIRLookupOpRespProperty_;
begin
  p := TFHIRLookupOpRespProperty_.Create;
  try
    p.code := name;
    (op as TFHIRLookupOpResponse).property_List.Add(p.link as TFHIRLookupOpRespProperty_);
    result := TFHIRLookupOpRespProperty4.Create(p.Link);
    List.add(result); // make sure it gets cleaned up
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse4.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationResponse).asParams;
end;

function TFHIRLookupOpResponse4.getDisplay: String;
begin
  result := (op as TFHIRLookupOpResponse).display;
end;

function TFHIRLookupOpResponse4.getName: String;
begin
  result := (op as TFHIRLookupOpResponse).name;
end;

function TFHIRLookupOpResponse4.getVersion: String;
begin
  result := (op as TFHIRLookupOpResponse).version;
end;

procedure TFHIRLookupOpResponse4.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpResponse).load(params);
end;

procedure TFHIRLookupOpResponse4.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpResponse).load(params as TFhirParameters);
end;

procedure TFHIRLookupOpResponse4.setDisplay(Value: String);
begin
  (op as TFHIRLookupOpResponse).display := value;
end;

function TFHIRLookupOpResponse4.getIsAbstract: boolean;
begin
  result := (op as TFHIRLookupOpResponse).abstract;
end;

procedure TFHIRLookupOpResponse4.setIsAbstract(Value: boolean);
begin
   (op as TFHIRLookupOpResponse).abstract := value;
end;


procedure TFHIRLookupOpResponse4.setName(Value: String);
begin
  (op as TFHIRLookupOpResponse).name := value;
end;

function TFHIRLookupOpResponse4.getCode: String;
begin
  result := (op as TFHIRLookupOpResponse).code;
end;

procedure TFHIRLookupOpResponse4.setCode(Value: String);
begin
  (op as TFHIRLookupOpResponse).code := value;
end;

function TFHIRLookupOpResponse4.getSystem: String;
begin
  result := (op as TFHIRLookupOpResponse).systemUri;
end;

procedure TFHIRLookupOpResponse4.setSystem(Value: String);
begin
  (op as TFHIRLookupOpResponse).systemUri := value;
end;

procedure TFHIRLookupOpResponse4.setVersion(Value: String);
begin
  (op as TFHIRLookupOpResponse).version := value;
end;

{ TFHIRLookupOpRespDesignation4 }

function TFHIRLookupOpRespDesignation4.GetUse: TFHIRObject;
begin
  result := (obj as TFHIRLookupOpRespDesignation).use;
end;

procedure TFHIRLookupOpRespDesignation4.SetUse(Value: TFHIRObject);
begin
  (obj as TFHIRLookupOpRespDesignation).use := value as TFhirCoding;
end;

{ TFHIRLookupOpRespSubProperty4 }

function TFHIRLookupOpRespSubProperty4.GetDescription: string;
begin
  result := (obj as TFHIRLookupOpRespSubproperty).description;
end;

function TFHIRLookupOpRespSubProperty4.GetValue: String;
begin
  result := (obj as TFHIRLookupOpRespSubproperty).value;
end;

procedure TFHIRLookupOpRespSubProperty4.SetDescription(Value: string);
begin
  (obj as TFHIRLookupOpRespSubproperty).description := value;
end;

procedure TFHIRLookupOpRespSubProperty4.SetValue(Value: String);
begin
  (obj as TFHIRLookupOpRespSubproperty).value := value;
end;

{ TFHIRLookupOpRespProperty4 }

function TFHIRLookupOpRespProperty4.getDescription: string;
begin
  result := (obj as TFHIRLookupOpRespProperty_).description;
end;

function TFHIRLookupOpRespProperty4.getValue: TFHIRObject;
begin
  result := (obj as TFHIRLookupOpRespProperty_).value;
end;

procedure TFHIRLookupOpRespProperty4.setDescription(Value: string);
begin
  (obj as TFHIRLookupOpRespProperty_).description := value;
end;

procedure TFHIRLookupOpRespProperty4.setValue(Value: TFHIRObject);
begin
  (obj as TFHIRLookupOpRespProperty_).value := value as TFhirType;
end;

function TFHIRLookupOpRespProperty4.addSubProp(name: String): TFHIRLookupOpRespSubPropertyW;
var
  p : TFHIRLookupOpRespSubproperty;
begin
  p := TFHIRLookupOpRespSubproperty.Create;
  try
    p.code := name;
    (obj as TFHIRLookupOpRespProperty_).subpropertyList.Add(p.link as TFHIRLookupOpRespSubproperty);
    result := TFHIRLookupOpRespSubProperty4.Create(p.Link);
    List.add(result); // make sure it gets cleaned up
  finally
    p.free;
  end;
end;

{ TFHIRExtension4 }

function TFHIRExtension4.ext: TFHIRExtension;
begin
  result := (Element as TFHIRExtension);
end;

function TFHIRExtension4.renderText: String;
begin
  result := gen(ext.value);
end;

function TFHIRExtension4.valueAsCodeableConcept: TFhirCodeableConceptW;
begin
  if ext.value is TFHIRCodeableConcept then
    result := TFHIRCodeableConcept4.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension4.valueAsCoding: TFhirCodingW;
begin
  if ext.value is TFHIRCoding then
    result := TFHIRCoding4.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension4.valueAsPeriod: TFhirPeriodW;
begin
  if ext.value is TFHIRPeriod then
    result := TFHIRPeriod4.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension4.valueAsQuantity: TFhirQuantityW;
begin
  if ext.value is TFHIRQuantity then
    result := TFHIRQuantity4.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension4.valueAsIdentifier: TFhirIdentifierW;
begin
  if ext.value is TFHIRIdentifier then
    result := TFHIRIdentifier4.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension4.valueAsAttachment: TFhirAttachmentW;
begin
  if ext.value is TFHIRAttachment then
    result := TFHIRAttachment4.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension4.valueAsString: string;
begin
  if ext.value is TFHIRPrimitiveType then
    result := ext.value.primitiveValue
  else
    result := '';
end;

function TFHIRExtension4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRExtension4.url: String;
begin
  result := ext.url;
end;

function TFHIRExtension4.value: TFHIRObject;
begin
  result := ext.value;
end;

procedure TFHIRExtension4.setValueW(value: TFhirDataTypeW);
begin
  setValueV(value.Element);
end;

procedure TFHIRExtension4.setValueV(value: TFhirObject);
begin
  if not (value is TFHIRType) then
    raise EFHIRException.Create('Wrong type at TFHIRExtension4.setValueV: '+value.ClassName+' ('+Codes_TFHIRVersion[value.fhirObjectVersion]);
  ext.value := (value as TFHIRType).link;
end;

{ TFHIRCoding4 }

function TFHIRCoding4.getCode: String;
begin
  result := (element as TFHIRCoding).code;
end;

function TFHIRCoding4.getDisplay: String;
begin
  result := (element as TFHIRCoding).display;
end;

function TFHIRCoding4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRCoding4.getSystem: String;
begin
  result := (element as TFHIRCoding).system;
end;

function TFHIRCoding4.getVersion: String;
begin
  result := (element as TFHIRCoding).version;
end;

function TFHIRCoding4.renderText: String;
begin
  result := gen(element as TFhirCoding);
end;

procedure TFHIRCoding4.setCode(Value: String);
begin
  (element as TFHIRCoding).code := value;
end;

procedure TFHIRCoding4.setDisplay(Value: String);
begin
  (element as TFHIRCoding).display := value;
end;

procedure TFHIRCoding4.setSystem(Value: String);
begin
    (element as TFHIRCoding).system := value;
end;

procedure TFHIRCoding4.setVersion(Value: String);
begin
  (element as TFHIRCoding).version := value;
end;

{ TFhirCodeSystemProperty4 }

function TFhirCodeSystemProperty4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirCodeSystemProperty4.code: String;
begin
  result := (Element as TFhirCodeSystemProperty).code;
end;

function TFhirCodeSystemProperty4.type_: TFhirCodeSystemPropertyType;
begin
  result := MAP_TFhirConceptPropertyTypeEnum[(Element as TFhirCodeSystemProperty).type_];
end;

function TFhirCodeSystemProperty4.uri: String;
begin
  result := (Element as TFhirCodeSystemProperty).uri;
end;

{ TFhirCodeSystemConceptProperty4 }

function TFhirCodeSystemConceptProperty4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirCodeSystemConceptProperty4.code: String;
begin
  result := (Element as TFhirCodeSystemConceptProperty).code;
end;

function TFhirCodeSystemConceptProperty4.value: TFHIRObject;
begin
  result := (Element as TFhirCodeSystemConceptProperty).value;
end;

{ TFhirCodeSystemConceptDesignation4 }

function TFhirCodeSystemConceptDesignation4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirCodeSystemConceptDesignation4.language: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).language;
end;

function TFhirCodeSystemConceptDesignation4.use: TFHIRCodingW;
begin
  if (Element as TFhirCodeSystemConceptDesignation).use = nil then
    result := nil
  else
    result := TFHIRCoding4.Create((Element as TFhirCodeSystemConceptDesignation).use.link);
end;

function TFhirCodeSystemConceptDesignation4.hasUse: boolean;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).use <> nil;
end;

function TFhirCodeSystemConceptDesignation4.useGen: String;
begin
  result := gen((Element as TFhirCodeSystemConceptDesignation).use)
end;

function TFhirCodeSystemConceptDesignation4.value: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).value;
end;

function TFhirCodeSystemConceptDesignation4.valueElement: TFHIRPrimitiveW;
begin
  if (Element as TFhirCodeSystemConceptDesignation).valueElement = nil then
    result := nil
  else
  begin
    result := TFHIRPrimitive4.Create((Element as TFhirCodeSystemConceptDesignation).valueElement.Link);
  end;
end;

{ TFhirCodeSystemConcept4 }

function TFhirCodeSystemConcept4.c: TFhirCodeSystemConcept;
begin
  result := Element as TFhirCodeSystemConcept;
end;

constructor TFhirCodeSystemConcept4.Create(elem: TFHIRObject; cs: TFHIRCodeSystem);
begin
  inherited Create(elem);
  FCodeSystem := cs;
end;

function TFhirCodeSystemConcept4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirCodeSystemConcept4.code: String;
begin
  result := c.code;
end;

function TFhirCodeSystemConcept4.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept4.Create((element as TFhirCodeSystemConcept).conceptList[ndx].Link, FCodeSystem);
end;

function TFhirCodeSystemConcept4.conceptCount: integer;
begin
  result := c.conceptList.Count;
end;

function TFhirCodeSystemConcept4.hasConcepts: boolean;
begin
  result := c.hasConceptList;
end;

function TFhirCodeSystemConcept4.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.Create;
    for i in (element as TFhirCodeSystemConcept).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept4.Create(i.Link, FCodeSystem));
  end;
  result := FConceptList;
end;

function TFhirCodeSystemConcept4.definition: String;
begin
  result := c.definition;
end;

function TFhirCodeSystemConcept4.designationCount: integer;
begin
  result := c.designationList.Count;
end;

function TFhirCodeSystemConcept4.designations: TFslList<TFhirCodeSystemConceptDesignationW>;
var
  i : TFhirCodeSystemConceptDesignation;
begin
  result := TFslList<TFhirCodeSystemConceptDesignationW>.Create;
  for i in c.designationList do
    result.Add(TFhirCodeSystemConceptDesignation4.Create(i.Link));
end;

function TFhirCodeSystemConcept4.display: String;
begin
  result := c.display;
end;

function TFhirCodeSystemConcept4.displayElement: TFHIRPrimitiveW;
begin
  if c.displayElement = nil then
    result := nil
  else
  begin
    result := TFHIRPrimitive4.Create(c.displayElement.link);
  end;
end;

function TFhirCodeSystemConcept4.displayTag(tag: String): String;
begin
  result := c.displayElement.Tags[tag];
end;

function getCodeWrapper(cs : TFhirCodeSystem; list : TFhirCodeSystemConceptList; code : String) : TFhirCodeSystemConceptW;
var
  cc : TFhirCodeSystemConcept;
begin
  result := nil;
  for cc in list do
  begin
    if cc.code = code then
      result := TFhirCodeSystemConcept4.Create(cc.Link, cs);
    if cc.hasConceptList then
    begin
      result := getCodeWrapper(cs, cc.conceptList, code);
      if result <> nil then
        exit;
    end;
  end;
end;

function TFhirCodeSystemConcept4.getCode(code: String): TFhirCodeSystemConceptW;
begin
  if (code = c.Code) then
    result := self.link
  else
    result := getCodeWrapper(FCodeSystem, c.conceptList, code);
end;

function TFhirCodeSystemConcept4.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (element as TFhirCodeSystemConcept).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystemConcept4.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
var
  i : TFhirCodeSystemConceptProperty;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.Create;
  for i in c.property_List do
    result.Add(TFhirCodeSystemConceptProperty4.Create(i.Link));
end;

procedure TFhirCodeSystemConcept4.setDisplayTag(tag, value: String);
begin
  c.displayElement.Tags[tag] := value;
end;                  

function TFhirCodeSystemConcept4.itemWeight : String;
begin
//  result :=
  result := (Element as TFhirCodeSystemConcept).getExtensionString(EXT_ORDINAL_VALUE);
  if result = '' then
    result := (Element as TFhirCodeSystemConcept).getExtensionString(EXT_ITEM_WEIGHT);
end;

{ TFhirCodeSystem4 }

function TFhirCodeSystem4.buildImplicitValueSet: TFHIRValueSetW;
begin
  result := TFHIRValueSet4.Create(cs.buildImplicitValueSet);
end;

function TFhirCodeSystem4.hasLanguage(cc : TFhirCodeSystemConcept; langs: THTTPLanguageList): boolean;  
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

function TFhirCodeSystem4.hasAnyDisplays(langs: THTTPLanguageList): boolean;
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

function TFhirCodeSystem4.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept4.Create(cs.conceptList[ndx].Link, cs);
end;

function TFhirCodeSystem4.conceptCount: integer;
begin
  result := cs.conceptList.Count;
end;

function TFhirCodeSystem4.hasConcepts: boolean;
begin
  result := cs.hasConceptList;
end;

function TFhirCodeSystem4.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.Create;
    for i in (resource as TFhirCodeSystem).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept4.Create(i.Link, cs));
  end;
  result := FConceptList;
end;

function TFhirCodeSystem4.copyright: String;
begin
  result := cs.copyright;
end;

function TFhirCodeSystem4.cs: TFhirCodeSystem;
begin
  result := Resource as TFhirCodeSystem;
end;

function TFhirCodeSystem4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirCodeSystem4.getCount: integer;
begin
  result := StrToInt(cs.count);
end;

function TFhirCodeSystem4.getDate: TFslDateTime;
begin
  result := cs.date;
end;

function TFhirCodeSystem4.getDescription: String;
begin
  result := cs.description;
end;

function TFhirCodeSystem4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirCodeSystem4.getChildren(c: TFhirCodeSystemConceptW): TFhirCodeSystemConceptListW;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getChildren(c.element as TFhirCodeSystemConcept);
  try
    result := TFhirCodeSystemConceptListW.Create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept4.Create(i.Link, cs));
      result.link;
    finally
      result.free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem4.getCode(code: String): TFhirCodeSystemConceptW;
begin
  result := getCodeWrapper(cs, cs.conceptList, code);
end;

function TFhirCodeSystem4.getContent: TFhirCodeSystemContentMode;
begin
  result := MAP_TFhirCodeSystemContentModeR[cs.content];
end;


function TFhirCodeSystem4.getContext: String;
begin
  result := cs.context;
end;

function TFhirCodeSystem4.getParents(c: TFhirCodeSystemConceptW): TFhirCodeSystemConceptListW;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getParents(c.element as TFhirCodeSystemConcept);
  try
    result := TFhirCodeSystemConceptListW.Create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept4.Create(i.Link, cs));
      result.link;
    finally
      result.free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem4.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (resource as TFhirCodeSystem).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystem4.isInactive(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isInactive(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem4.codeStatus(c: TFhirCodeSystemConceptW): String;
begin
  result := cs.codeStatus(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem4.isAbstract(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isAbstract(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem4.isDeprecated(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isDeprecated(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem4.language: String;
begin
  result := cs.language;
end;

function TFhirCodeSystem4.getName: String;
begin
  result := cs.name;
end;

function TFhirCodeSystem4.properties: TFslList<TFhirCodeSystemPropertyW>;
var
  i : TFhirCodeSystemProperty;
begin
  result := TFslList<TFhirCodeSystemPropertyW>.Create;
  for i in cs.property_List do
    result.Add(TFhirCodeSystemProperty4.Create(i.Link));
end;

procedure TFhirCodeSystem4.setContent(Value: TFhirCodeSystemContentMode);
begin
  cs.content := MAP_TFhirCodeSystemContentMode[value];
end;

procedure TFhirCodeSystem4.setCount(Value: integer);
begin
  cs.count := inttostr(value);
end;

procedure TFhirCodeSystem4.setDate(Value: TFslDateTime);
begin
  cs.date := value;
end;

procedure TFhirCodeSystem4.setDescription(Value: String);
begin
  cs.description := value;
end;

procedure TFhirCodeSystem4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirCodeSystem4.setName(Value: String);
begin
  cs.name := value;
end;

procedure TFhirCodeSystem4.setPublisher(Value: String);
begin
  cs.publisher := value;
end;

function TFhirCodeSystem4.getTitle: String;
begin
  result := cs.title;
end;

procedure TFhirCodeSystem4.setTitle(value: String);
begin
  cs.title := value;
end;

function TFhirCodeSystem4.getExperimental: boolean;
begin
  result := cs.experimental;
end;

procedure TFhirCodeSystem4.setExperimental(value: boolean);
begin
  cs.experimental := value;
end;

procedure TFhirCodeSystem4.setStatus(Value: TPublicationStatus);
begin
  cs.status := MAP_TPublicationStatus[value];
end;

procedure TFhirCodeSystem4.setUrl(Value: String);
begin
  cs.url := value;
end;

procedure TFhirCodeSystem4.setVersion(Value: String);
begin
  cs.version := value;
end;

function TFhirCodeSystem4.GetCaseSensitive: boolean;
begin
  result := cs.caseSensitive;
end;

function TFhirCodeSystem4.supplements: String;
begin
  result := cs.supplements;
end;

function TFhirCodeSystem4.valueSet: String;
begin
  result := cs.valueset;
end;

function TFhirCodeSystem4.getPublisher: String;
begin
  result := cs.publisher;
end;

function TFhirCodeSystem4.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[cs.status];
end;

function TFhirCodeSystem4.getURL: String;
begin
  result := cs.url;
end;

function TFhirCodeSystem4.getVersion: String;
begin
  result := cs.version;
end;

function TFhirCodeSystem4.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (cs.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(cs.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
end;

{ TFhirValueSetExpansion4 }

procedure TFhirValueSetExpansion4.addContains(item: TFhirValueSetExpansionContainsW);
begin
  exp.containsList.Add((item.Element as TFhirValueSetExpansionContains).link);
end;

function TFhirValueSetExpansion4.addContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains4.Create((element as TFhirValueSetExpansion).containsList.Append.Link);
end;

procedure TFhirValueSetExpansion4.addParamBool(name: String; value: boolean);
begin
  exp.addParamBool(name, value);
end;

procedure TFhirValueSetExpansion4.addParamInt(name: String; value: integer);
begin
  exp.AddParamInt(name, value);
end;

procedure TFhirValueSetExpansion4.addParamStr(name, value: String);
begin
  exp.addParamStr(name, value);
end;

procedure TFhirValueSetExpansion4.addParamCode(name, value: String);
begin
  exp.addParamCode(name, value);
end;

procedure TFhirValueSetExpansion4.addParamUri(name, value: String);
begin
  exp.addParamUri(name, value);
end;

function TFhirValueSetExpansion4.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.Create;
  for item in (Element as TFhirValueSetExpansion).containsList do
    result.Add(TFhirValueSetExpansionContains4.Create(item.Link));
end;

function TFhirValueSetExpansion4.getTotal: integer;
begin
  result := StrToIntDef(exp.total, 0);
end;

procedure TFhirValueSetExpansion4.setTotal(value: integer);
begin
  exp.total := inttostr(value);
end;

function TFhirValueSetExpansion4.getOffset: integer;
begin
  result := StrToIntDef(exp.offset, 0);
end;

procedure TFhirValueSetExpansion4.setOffset(value: integer);
begin
  exp.offset := inttostr(value);
end;

procedure TFhirValueSetExpansion4.defineProperty(focus: TFhirValueSetExpansionContainsW; url, code: String; value: TFHIRObject);
var
  pd, ext : TFhirExtension;
begin
  try
    pd := nil;
    for ext in exp.extensionList do
    begin
      if (ext.url = 'http://hl7.org/fhir/5.0/StructureDefinition/extension-ValueSet.expansion.property') and
        ((ext.getExtensionString('uri') = url) or (ext.getExtensionString('code') = code)) then
      begin
        pd := ext;
        break;
      end;
    end;
    if (pd = nil) then
    begin
      pd := TFHIRExtension.Create;
      exp.extensionList.add(pd);
      pd.url := 'http://hl7.org/fhir/5.0/StructureDefinition/extension-ValueSet.expansion.property';
      pd.setExtensionUri('uri', url);
      pd.setExtensionCode('code',code);
    end
    else if (pd.getExtensionString('uri') = '') then
      pd.setExtensionUri('uri', url)
    else if (pd.getExtensionString('uri') <> url) then
      raise EFHIRException.Create('URL mismatch on expansion: '+pd.getExtensionString('uri')+' vs '+url+' for code '+code);
    code := pd.getExtensionString('code');

    pd := nil;
    for ext in ((focus as TFhirValueSetExpansionContains4).element as TFhirValueSetExpansionContains).extensionList do
    begin
      if (ext.url = 'http://hl7.org/fhir/5.0/StructureDefinition/extension-ValueSet.expansion.contains.property') and
        (ext.getExtensionString('code') = code) then
      begin
        pd := ext;
        break;
      end;
    end;
    if (pd = nil) then
    begin
      pd := TFHIRExtension.Create;
      ((focus as TFhirValueSetExpansionContains4).element as TFhirValueSetExpansionContains).extensionList.add(pd);
      pd.url := 'http://hl7.org/fhir/5.0/StructureDefinition/extension-ValueSet.expansion.contains.property';
      pd.setExtensionCode('code',code);
      pd.setExtension('value',value as TFHIRType);
    end
    else
      pd.setExtension('value',value as TFHIRType);
  finally
    value.free;
  end;
end;


procedure TFhirValueSetExpansion4.copyParams(source: TFhirValueSetExpansionW);
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

function TFhirValueSetExpansion4.exp: TFhirValueSetExpansion;
begin
  result := element as TFhirValueSetExpansion;
end;

function TFhirValueSetExpansion4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirValueSetExpansion4.hasParam(name, value: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) and (param.value.primitiveValue = value) then
      exit(true);
end;

function TFhirValueSetExpansion4.makeContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains4.Create(TFhirValueSetExpansionContains.create);
end;

function TFhirValueSetExpansion4.hasParam(name: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) then
      exit(true);
end;

{ TFhirValueSetExpansionContains4 }

function TFhirValueSetExpansionContains4.getCode: String;
begin
  result := (Element as TFhirValueSetExpansionContains).code;
end;

procedure TFhirValueSetExpansionContains4.addDesignation(lang, use, value: String);
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

procedure TFhirValueSetExpansionContains4.addDesignation(lang : TIETFLang; use: TFHIRCodingW; value: TFHIRPrimitiveW; extensions : TFslList<TFHIRExtensionW>);
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

procedure TFhirValueSetExpansionContains4.addProperty(code: String; value: TFHIRObject);
var
  p : TFhirExtension;
begin
  p := (Element as TFhirValueSetExpansionContains).extensionList.append;
  p.url := 'http://hl7.org/fhir/5.0/StructureDefinition/extension-ValueSet.expansion.contains.property';
  p.addExtension('code', code);
  p.addExtension('value', value.link as TFhirType);
end;

procedure TFhirValueSetExpansionContains4.addProperty(code: String; prop: TFhirCodeSystemConceptPropertyW);
var
  p : TFhirExtension;
begin
  p := (Element as TFhirValueSetExpansionContains).extensionList.append;
  p.url := 'http://hl7.org/fhir/5.0/StructureDefinition/extension-ValueSet.expansion.contains.property';
  p.addExtension('code', code);
  p.addExtension('value', prop.value.link as TFhirType);
  p.copyExtensions(prop.element, []);
end;

procedure TFhirValueSetExpansionContains4.addContains(contained: TFhirValueSetExpansionContainsW);
begin
  (Element as TFhirValueSetExpansionContains).containsList.add(contained.Element.link);
end;

procedure TFhirValueSetExpansionContains4.clearContains;
begin
  (Element as TFhirValueSetExpansionContains).containsList.clear;
end;

function TFhirValueSetExpansionContains4.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.Create;
  for item in (Element as TFhirValueSetExpansionContains).containsList do
    result.Add(TFhirValueSetExpansionContains4.Create(item.Link));
end;

function TFhirValueSetExpansionContains4.getDisplay: String;
begin
  result := (Element as TFhirValueSetExpansionContains).display;
end;

function TFhirValueSetExpansionContains4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirValueSetExpansionContains4.getSystem: String;
begin
  result := (Element as TFhirValueSetExpansionContains).system;
end;

function TFhirValueSetExpansionContains4.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.Create;
end;

procedure TFhirValueSetExpansionContains4.setCode(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).code := value;
end;

procedure TFhirValueSetExpansionContains4.setDisplay(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).display := value;
end;

procedure TFhirValueSetExpansionContains4.setSystem(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).system := value;
end;              

function TFhirValueSetExpansionContains4.GetItemWeight: String;
begin
  result := (Element as TFhirValueSetExpansionContains).getExtensionString(EXT_ORDINAL_VALUE);
  if result = '' then
    result := (Element as TFhirValueSetExpansionContains).getExtensionString(EXT_ITEM_WEIGHT);
end;

procedure TFhirValueSetExpansionContains4.SetItemWeight(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).setExtensionString(EXT_ORDINAL_VALUE, value);
end;


function TFhirValueSetExpansionContains4.GetAbstract: boolean;
begin
  result := (Element as TFhirValueSetExpansionContains).abstract;
end;

function TFhirValueSetExpansionContains4.GetInactive: boolean;
begin
  result := (Element as TFhirValueSetExpansionContains).inactive;
end;

procedure TFhirValueSetExpansionContains4.SetAbstract(Value: boolean);
begin
  (Element as TFhirValueSetExpansionContains).abstract := value;
end;

procedure TFhirValueSetExpansionContains4.SetInactive(Value: boolean);
begin
  (Element as TFhirValueSetExpansionContains).inactive := value;
end;

function TFhirValueSetExpansionContains4.getVersion: String;
begin
  result := (Element as TFhirValueSetExpansionContains).version;
end;

procedure TFhirValueSetExpansionContains4.setVersion(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).version := value
end;


{ TFhirValueSetComposeIncludeConceptDesignation4 }

function TFhirValueSetComposeIncludeConceptDesignation4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirValueSetComposeIncludeConceptDesignation4.language: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).language;
end;

function TFhirValueSetComposeIncludeConceptDesignation4.value: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).value;
end;

function TFhirValueSetComposeIncludeConceptDesignation4.use: TFHIRCodingW;
begin
  if (element as TFhirValueSetComposeIncludeConceptDesignation).use = nil then
    result := nil
  else
    result := TFHIRCoding4.Create((element as TFhirValueSetComposeIncludeConceptDesignation).use.link);
end;

function TFhirValueSetComposeIncludeConceptDesignation4.valueElement: TFHIRPrimitiveW;
begin
  if (element as TFhirValueSetComposeIncludeConceptDesignation).valueElement = nil then
    result := nil
  else
  begin
    result := TFHIRPrimitive4.Create((element as TFhirValueSetComposeIncludeConceptDesignation).valueElement.link);
  end;
end;

{ TFhirConceptMap4 }

function TFhirConceptMap4.addGroup(source, target: String): TFhirConceptMapGroupW;
var
  g : TFhirConceptMapGroup;
begin
  g := cm.groupList.Append;
  g.source := source;
  g.target := target;
  result := TFhirConceptMapGroup4.Create(g.Link);
end;

function TFhirConceptMap4.cm: TFhirConceptMap;
begin
  result := Resource as TFhirConceptMap;
end;

function TFhirConceptMap4.getVersion: String;
begin
  result := cm.version;
end;

function TFhirConceptMap4.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (cm.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(cm.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
end;

function TFhirConceptMap4.groups: TFslList<TFhirConceptMapGroupW>;
var
  g : TFhirConceptMapGroup;
begin
  result := TFslList<TFhirConceptMapGroupW>.Create;
  for g in cm.groupList do
    result.Add(TFhirConceptMapGroup4.Create(g.Link))
end;

procedure TFhirConceptMap4.setVersion(Value: String);
begin
  cm.version := value;
end;

function TFhirConceptMap4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirConceptMap4.source: String;
begin
  if (cm.source is TFhirReference) then
    result := (cm.source as TFhirReference).reference
  else if cm.source <> nil then
    result := cm.source.primitiveValue;
end;

function TFhirConceptMap4.sourceDesc: String;
begin
  result := cm.sourceDesc;
end;

function TFhirConceptMap4.getExperimental: boolean;
begin
  result := cm.experimental;
end;

procedure TFhirConceptMap4.setExperimental(value: boolean);
begin
  cm.experimental := value;
end;

function TFhirConceptMap4.target: String;
begin
  if (cm.target is TFhirReference) then
    result := (cm.target as TFhirReference).reference
  else if cm.target <> nil then
    result := cm.target.primitiveValue;
end;

function TFhirConceptMap4.targetDesc: String;
begin
  result := cm.targetDesc;
end;

function TFhirConceptMap4.getURL: String;
begin
  result := cm.url;
end;

function TFhirConceptMap4.getDate: TFslDateTime;
begin
  result := cm.Date;
end;

function TFhirConceptMap4.getDescription: String;
begin
  result := cm.Description;
end;

function TFhirConceptMap4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirConceptMap4.getName: String;
begin
  result := cm.Name;
end;

function TFhirConceptMap4.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[cm.Status];
end;

procedure TFhirConceptMap4.setDate(Value: TFslDateTime);
begin
  cm.Date := value;
end;

procedure TFhirConceptMap4.setDescription(Value: String);
begin
  cm.Description := value;
end;

procedure TFhirConceptMap4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirConceptMap4.setName(Value: String);
begin
  cm.Name := value;
end;

procedure TFhirConceptMap4.setStatus(Value: TPublicationStatus);
begin
  cm.Status := MAP_TPublicationStatus[value];
end;

procedure TFhirConceptMap4.setUrl(Value: String);
begin
  cm.Url := value;
end;

procedure TFhirConceptMap4.setPublisher(Value: String);
begin
  cm.publisher := value;
end;

function TFhirConceptMap4.getTitle: String;
begin
  result := cm.title;
end;

procedure TFhirConceptMap4.setTitle(value: String);
begin
  cm.title := value;
end;

function TFhirConceptMap4.getContext: String;
begin
  result := cm.context;
end;

function TFhirConceptMap4.getPublisher: String;
begin
  result := cm.publisher;
end;

{ TFhirConceptMapGroupElementTarget4 }

function TFhirConceptMapGroupElementTarget4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirConceptMapGroupElementTarget4.code: String;
begin
  result := (Element as TFhirConceptMapGroupElementTarget).code;
end;

function TFhirConceptMapGroupElementTarget4.comments: String;
begin
  result := (Element as TFhirConceptMapGroupElementTarget).comment;
end;

function TFhirConceptMapGroupElementTarget4.equivalence: TFHIRConceptEquivalence;
begin
  result := MAP_TFHIRConceptEquivalence[(Element as TFhirConceptMapGroupElementTarget).equivalence];
end;

function TFhirConceptMapGroupElementTarget4.products: TFslList<TFhirConceptMapGroupElementDependsOnW>;
var
  i : TFhirConceptMapGroupElementTargetDependsOn;
begin
  result := TFslList<TFhirConceptMapGroupElementDependsOnW>.Create;
  for i in (Element as TFhirConceptMapGroupElementTarget).productList do
    result.Add(TFhirConceptMapGroupElementDependsOn4.Create(i.link));
end;

{ TFhirConceptMapGroupElement4 }

function TFhirConceptMapGroupElement4.addTarget(code: String; eq: TFHIRConceptEquivalence): TFhirConceptMapGroupElementTargetW;
var
  t : TFhirConceptMapGroupElementTarget;
begin
  t := (Element as TFhirConceptMapGroupElement).targetList.Append;
  t.code := code;
  t.equivalence := MAP_TFHIRConceptEquivalenceR[eq];
  result := TFhirConceptMapGroupElementTarget4.Create(t.link);
end;

function TFhirConceptMapGroupElement4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirConceptMapGroupElement4.code: String;
begin
  result := (Element as TFhirConceptMapGroupElement).code;
end;

function TFhirConceptMapGroupElement4.targetCount: integer;
begin
  result := (Element as TFhirConceptMapGroupElement).targetList.Count;
end;

function TFhirConceptMapGroupElement4.targets: TFslList<TFhirConceptMapGroupElementTargetW>;
var
  i : TFhirConceptMapGroupElementTarget;
begin
  result := TFslList<TFhirConceptMapGroupElementTargetW>.Create;
  for i in (Element as TFhirConceptMapGroupElement).targetList do
    result.Add(TFhirConceptMapGroupElementTarget4.Create(i.link));
end;


{ TFhirConceptMapGroup4 }

function TFhirConceptMapGroup4.addElement(code: String): TFhirConceptMapGroupElementW;
var
  t : TFhirConceptMapGroupElement;
begin
  t := (Element as TFhirConceptMapGroup).elementList.Append;
  t.code := code;
  result := TFhirConceptMapGroupElement4.Create(t.link);
end;

function TFhirConceptMapGroup4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirConceptMapGroup4.elements: TFslList<TFhirConceptMapGroupElementW>;
var
  t : TFhirConceptMapGroupElement;
begin
  result := TFslList<TFhirConceptMapGroupElementW>.Create;
  for t in (Element as TFhirConceptMapGroup).elementList do
    result.Add(TFhirConceptMapGroupElement4.Create(t.link))
end;

function TFhirConceptMapGroup4.source: String;
begin
  result := (Element as TFhirConceptMapGroup).source;
end;

function TFhirConceptMapGroup4.target: String;
begin
  result := (Element as TFhirConceptMapGroup).target;
end;

{ TFHIRMeta4 }


procedure TFHIRMeta4.addLabel(system, code, display: String);
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

procedure TFHIRMeta4.addProfile(uri: String);
begin
  force;
  m.profileList.Add(TFhirCanonical.Create(uri));
end;

procedure TFHIRMeta4.addTag(system, code, display: String);
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

procedure TFHIRMeta4.clearLabels;
begin
  if Element <> nil then
    m.securityList.Clear;
end;

procedure TFHIRMeta4.clearProfiles;
begin
  if Element <> nil then
    m.profileList.Clear;
end;

procedure TFHIRMeta4.clearTags;
begin
  if Element <> nil then
    m.tagList.Clear;
end;

destructor TFHIRMeta4.Destroy;
begin
  FResource.free;
  inherited;
end;

function TFHIRMeta4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

procedure TFHIRMeta4.force;
begin
  if Element = nil then
  begin
    FElement := TFHIRMeta.Create;
    Resource.meta := m.Link;
  end;
end;

function TFHIRMeta4.getLastUpdated: TFslDateTime;
begin
  if Element = nil then
    result := TFslDateTime.makeNull
  else
    result := m.lastUpdated;
end;

function TFHIRMeta4.getVersionId: String;
begin
  if Element = nil then
    result := ''
  else
    result := m.versionId;
end;

function TFHIRMeta4.hasLabel(system, code: String): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if Element <> nil then
    for c in m.securityList do
      if (c.system = system) and (c.code = code) then
        exit(true);
end;

function TFHIRMeta4.hasTag(system, code: String): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if Element <> nil then
    for c in m.tagList do
       if (c.system = system) and (c.code = code) then
        exit(true);
end;

function TFHIRMeta4.labels: TFslList<TFHIRCodingW>;
var
  i : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if Element <> nil then
    for i in m.securityList do
      result.Add(TFHIRCoding4.Create(i.Link));
end;

function TFHIRMeta4.m: TFhirMeta;
begin
  result := Element as TFHIRMeta;
end;

function TFHIRMeta4.NoElementOk: boolean;
begin
  result := true;
end;

function TFHIRMeta4.profiles: TArray<String>;
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

procedure TFHIRMeta4.removeLabel(system, code: String);
begin
  if Element <> nil then
    m.removeLabel(system, code);
end;

procedure TFHIRMeta4.removeProfile(uri: String);
begin
  if Element <> nil then
    m.profileList.removeUri(uri);
end;

procedure TFHIRMeta4.removeTag(system, code: String);
begin
  if Element <> nil then
    m.removeTag(system, code);
end;

procedure TFHIRMeta4.setLastUpdated(Value: TFslDateTime);
begin
  force;
  m.lastUpdated := value;
end;

procedure TFHIRMeta4.setResource(value: TFHIRResource);
begin
  FResource.free;
  FResource := value;
end;

procedure TFHIRMeta4.setVersionId(Value: String);
begin
  force;
  m.versionId := value;
end;

function TFHIRMeta4.tags: TFslList<TFHIRCodingW>;
var
  i : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if Element <> nil then
    for i in m.tagList do
      result.Add(TFHIRCoding4.Create(i.Link));
end;


function TFHIRMeta4.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FResource.sizeInBytes(magic));
end;

{ TFHIRAuditEvent4 }

function TFHIRAuditEvent4.ae: TFHIRAuditEvent;
begin
  result := Resource as TFhirAuditEvent;
end;

function TFHIRAuditEvent4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRAuditEvent4.dateTime: TFslDateTime;
begin
  result := ae.event.dateTime;
end;

procedure TFHIRAuditEvent4.eventSubType(system, code, display: String);
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

procedure TFHIRAuditEvent4.eventType(system, code, display: String);
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

function TFHIRAuditEvent4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRAuditEvent4.participantId(system, value, alt, name: String);
var
  p: TFhirAuditEventParticipant;
begin
  p := ae.participantList.append;
  p.who := TFhirReference.Create;
  p.who.identifier := TFhirIdentifier.Create;
  p.who.identifier.system := system;
  p.who.identifier.value := value;
  p.altId := alt;
  p.name := name;
end;

procedure TFHIRAuditEvent4.participantIp(ip: String);
var
  p: TFhirAuditEventParticipant;
begin
  p := ae.participantList.append;
  p.network := TFhirAuditEventParticipantNetwork.Create;
  p.network.address := ip;
  p.network.type_ := NetworkType2;
end;

procedure TFHIRAuditEvent4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRAuditEvent4.source(name, system, value: String);
begin
  if ae.source = nil then
    ae.source := TFhirAuditEventSource.Create;
  ae.source.site := name;
  ae.source.observer := TFhirReference.Create();
  ae.source.observer.identifier := TFhirIdentifier.Create;
  ae.source.observer.identifier.system := system;
  ae.source.observer.identifier.value := value;
end;

procedure TFHIRAuditEvent4.sourceType(system, code, display: String);
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

procedure TFHIRAuditEvent4.success;
begin
  if ae.event = nil then
    ae.event := TFhirAuditEventEvent.Create;
  ae.event.action := AuditEventActionE;
  ae.event.outcome := AuditEventOutcome0;
  ae.event.dateTime := TFslDateTime.makeUTC;
end;

{ TFhirCapabilityStatementRestResource4 }

procedure TFhirCapabilityStatementRestResource4.addParam(html, n, url, d: String; t: TFHIRSearchParamType; tgts: array of String);
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

function TFhirCapabilityStatementRestResource4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirCapabilityStatementRestResource4.getCode: String;
begin
  result := CODES_TFhirResourceTypesEnum[(Element as TFhirCapabilityStatementRestResource).type_];
end;

procedure TFhirCapabilityStatementRestResource4.setCode(Value: String);
begin
  (Element as TFhirCapabilityStatementRestResource).type_Element := TFhirEnum.Create('http://hl7.org/fhir/resource-types', value);
end;

function TFhirCapabilityStatementRestResource4.getProfile: String;
begin
  result := (Element as TFhirCapabilityStatementRestResource).profile;
end;

procedure TFhirCapabilityStatementRestResource4.setProfile(Value: String);
begin
  (Element as TFhirCapabilityStatementRestResource).profile := value;
end;

procedure TFhirCapabilityStatementRestResource4.addInteraction(codeV, doco: String);
begin
  With (Element as TFhirCapabilityStatementRestResource).interactionList.Append do
  begin
    codeElement := TFhirEnum.Create('http://hl7.org/fhir/ValueSet/type-restful-interaction', codeV);
    documentation := doco;
  end;
end;

procedure TFhirCapabilityStatementRestResource4.addOperation(codeV, defn, doco: String);
begin
  With (Element as TFhirCapabilityStatementRestResource).operationList.Append do
  begin
    name := codeV;
    definition := defn;
    documentation := doco;
  end;
end;

function TFhirCapabilityStatementRestResource4.getReadHistory: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).readHistory;
end;

function TFhirCapabilityStatementRestResource4.hasInteraction: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).interactionList.Count > 0;
end;

procedure TFhirCapabilityStatementRestResource4.setReadHistory(Value: boolean);
begin
  (Element as TFhirCapabilityStatementRestResource).readHistory := Value;
end;


{ TFHIRSubscription4 }

function TFHIRSubscription4.getCriteria: String;
begin
  result := sub.criteria;
end;

function TFHIRSubscription4.getDirect: boolean;
begin
  result := sub.channel.endpointElement.hasExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct')
    and (sub.channel.endpointElement.getExtensionString('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct') = 'true');
end;

function TFHIRSubscription4.getEndpoint: String;
begin
  result := sub.channel.endpoint;
end;

function TFHIRSubscription4.getError: String;
begin
  result := sub.error;
end;

function TFHIRSubscription4.getHeaders: TArray<String>;
var
  i : integer;
begin
  setLength(result, sub.channel.headerList.Count);
  for i := 0 to sub.channel.headerList.Count - 1 do
    result[i] := sub.channel.headerList[i].value;
end;

function TFHIRSubscription4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRSubscription4.getMethod: TSubscriptionMethod;
begin
  result := MAP_TSubscriptionMethod[sub.channel.type_];
end;

function TFHIRSubscription4.getPayload: String;
begin
  result := sub.channel.payload;
end;

function TFHIRSubscription4.getStatus: TSubscriptionStatus;
begin
  result := MAP_TSubscriptionStatus[sub.status];
end;

function TFHIRSubscription4.getSummary: String;
var
  s : TFhirString;
begin
  result := sub.channel.type_Element.value+#1+sub.channel.endpoint+#1+sub.channel.payload;
  for s in sub.channel.headerList do
    result := result+#0+s.value;
//  result := result+#0+subst.channel.header;
end;

function TFHIRSubscription4.getTopic: string;
begin
  result := sub.getExtensionString('http://hl7.org/fhir/uv/subscriptions-backport/StructureDefinition/backport-topic-canonical');
end;

procedure TFHIRSubscription4.setCriteria(Value: String);
begin
  sub.criteria := value;
end;

procedure TFHIRSubscription4.setDirect(Value: boolean);
begin
  if value then
    sub.channel.endpointElement.setExtensionBoolean('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct', 'true')
  else
    sub.channel.endpointElement.removeExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct');
end;

procedure TFHIRSubscription4.setEndpoint(Value: String);
begin
  sub.channel.endpoint := value;
end;

procedure TFHIRSubscription4.setError(Value: String);
begin
  sub.error := value;
end;

procedure TFHIRSubscription4.setheaders(Value: TArray<String>);
var
  s : String;
begin
  sub.channel.headerList.Clear;
  for s in value do
    sub.channel.headerList.Append.value := s;
end;

procedure TFHIRSubscription4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRSubscription4.setMethod(Value: TSubscriptionMethod);
begin
  sub.channel.type_ := MAP_TSubscriptionMethod2[value];
end;

procedure TFHIRSubscription4.setPayload(Value: String);
begin
  sub.channel.payload := value;
end;

procedure TFHIRSubscription4.setStatus(Value: TSubscriptionStatus);
begin
  sub.status := MAP_TSubscriptionStatus2[value];
end;

function TFHIRSubscription4.sub: TFhirSubscription;
begin
  result := resource as TFhirSubscription;
end;

function TFHIRSubscription4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

{ TFhirBinary4 }

function TFhirBinary4.content: TBytes;
begin
  result := (resource as TFHIRBinary).data;
end;

function TFhirBinary4.ContentType: String;
begin
  result := (resource as TFHIRBinary).contentType;
end;

function TFhirBinary4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirBinary4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFhirBinary4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirObservationComponent4 }

function TFhirObservationComponent4.codings: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if comp.code <> nil then
    for c in comp.code.codingList do
      result.Add(TFHIRCoding4.Create(c.Link));
end;

function TFhirObservationComponent4.comp: TFhirObservationComponent;
begin
  result := (Element as TFhirObservationComponent);
end;

function TFhirObservationComponent4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirObservationComponent4.dataAbsentReason: TFhirCodeableConceptW;
begin
  if comp.dataAbsentReason = nil then
    result := nil
  else
    result := TFhirCodeableConcept4.Create(comp.dataAbsentReason.link);
end;

function TFhirObservationComponent4.getValue: TFHIRObject;
begin
  result := comp.value;
end;

procedure TFhirObservationComponent4.setValue(Value: TFHIRObject);
begin
  comp.value := value as TFHIRType;
end;

function TFhirObservationComponent4.valueString: String;
begin
  if (comp.value <> nil) and (comp.value.isPrimitive) then
    result := comp.value.primitiveValue
  else
    result := '';
end;

function TFhirObservationComponent4.valueW: TFHIRXVersionElementWrapper;
begin
  if comp.value is TFhirCodeableConcept then
    result := TFhirCodeableConcept4.Create(comp.value.Link)
  else if comp.value is TFHIRQuantity then
    result := TFHIRQuantity4.Create(comp.value.Link)
  else
    result := nil;
end;

{ TFhirObservation4 }

procedure TFhirObservation4.setCode(c: TFHIRCodingW);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.codingList.add((c.Element as TFHIRCoding).Link);
end;

function TFhirObservation4.addComp(system, code: String): TFhirObservationComponentW;
var
  c : TFhirObservationComponent;
begin
  c := TFhirObservationComponent.Create;
  c.code := TFhirCodeableConcept.Create;
  c.code.codingList.add(TFhirCoding.Create(system, code));
  result := TFhirObservationComponent4.Create(c.link);
end;

function TFhirObservation4.getStatus: TObservationStatus;
begin
  result := MAP_TObservationStatus2[obs.status];
end;

function TFhirObservation4.obs: TFHIRObservation;
begin
  result := resource as TFhirObservation;
end;

procedure TFhirObservation4.setPeriod(start, finish: TDateTime);
begin
  obs.effective := TFhirPeriod.Create;
  TFhirPeriod(obs.effective).start := TFslDateTime.makeUTC(start);
  TFhirPeriod(obs.effective).end_ := TFslDateTime.makeUTC(finish);
end;

procedure TFhirObservation4.setStatus(Value: TObservationStatus);
begin
  obs.status := MAP_TObservationStatus[value];
end;

function TFhirObservation4.codings: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if obs.code <> nil then
    for c in obs.code.codingList do
      result.Add(TFHIRCoding4.Create(c.Link));
end;

function TFhirObservation4.components: TFslList<TFhirObservationComponentW>;
var
  c : TFhirObservationComponent;
begin
  result := TFslList<TFhirObservationComponentW>.Create;
  for c in obs.componentList do
    result.Add(TFhirObservationComponent4.Create(c.Link));
end;

function TFhirObservation4.dataAbsentReason: TFhirCodeableConceptW;
begin
  if obs.dataAbsentReason = nil then
    result := nil
  else
    result := TFhirCodeableConcept4.Create(obs.dataAbsentReason.link);
end;

procedure TFhirObservation4.getDates(var dt, dtMin, dtMax: TDateTime);
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

function TFhirObservation4.getValue: TFHIRObject;
begin
  result := obs.value;
end;

procedure TFhirObservation4.setValue(Value: TFHIRObject);
begin
  obs.value := value as TFhirType;
end;

function TFhirObservation4.valueW: TFHIRXVersionElementWrapper;
begin
  if obs.value is TFhirCodeableConcept then
    result := TFhirCodeableConcept4.Create(obs.value.Link)
  else if obs.value is TFHIRQuantity then
    result := TFHIRQuantity4.Create(obs.value.Link)
  else
    result := nil;
end;

function TFhirObservation4.hasTime: boolean;
begin
  result := obs.effective <> nil;
end;

function TFhirObservation4.categories: TFslList<TFHIRCodingW>;
var
  cc : TFHIRCodeableConcept;
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  for cc in obs.categoryList do
    for c in cc.codingList do
      result.Add(TFHIRCoding4.Create(c.Link));
end;

procedure TFhirObservation4.setCode(system, code, display: String);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.codingList.add(TFHIRCoding.Create(system, code, display));
end;

procedure TFhirObservation4.SetCodeText(const Value: String);
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

procedure TFhirObservation4.SetComment(const Value: String);
begin
  obs.noteList.Append.text := value;
end;

function TFhirObservation4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

procedure TFhirObservation4.SetDevice(const Value: String);
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

procedure TFhirObservation4.SetDeviceName(const Value: String);
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

procedure TFhirObservation4.SetEffective(const Value: TFHIRObject);
begin
  obs.effective := Value as TFhirType;
end;

procedure TFhirObservation4.SetEffectiveDateTime(const Value: TFslDateTime);
begin
  SetEffective(TFhirDateTime.Create(value));
end;

procedure TFhirObservation4.SetEffectivePeriod(const Value: TFHIRPeriodW);
begin
  obs.effective := (value.Element as TFHIRType).Link;
end;

procedure TFhirObservation4.SetIssued(const Value: TFslDateTime);
begin
  obs.issued := Value;
end;

procedure TFhirObservation4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirObservation4.SetSubject(const Value: String);
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

function TFhirObservation4.GetCodeText: String;
begin
  if obs.code = nil then
    result := ''
  else
    result := obs.code.text;
end;

function TFhirObservation4.GetComment: String;
begin
  if obs.noteList.Count = 0 then
    result := ''
  else
    result := obs.noteList[0].text;
end;

function TFhirObservation4.getComponent(system, code: String; var comp: TFhirObservationComponentW): boolean;
var
  c : TFHIRObservationComponent;
begin
  comp := nil;
  result := obs.getComponent(system, code, c);
  if result then
    comp := TFHIRObservationComponent4.Create(c.link);
end;

function TFhirObservation4.getComponent(system: String; var comp: TFhirObservationComponentW): boolean;
var
  c : TFHIRObservationComponent;
begin
  comp := nil;
  result := obs.getComponent(system, c);
  if result then
    comp := TFHIRObservationComponent4.Create(c.link);
end;

function TFhirObservation4.GetDevice: String;
begin
  if obs.device <> nil then
    result := obs.device.reference
  else
    result := '';
end;

function TFhirObservation4.GetDeviceName: String;
begin
  if obs.device <> nil then
    result := obs.device.display
  else
    result := '';
end;

function TFhirObservation4.GetEffective: TFHIRObject;
begin
  result := obs.effective;
end;

function TFhirObservation4.GetEffectiveDateTime: TFslDateTime;
begin
  if obs.effective is TFhirDateTime then
    result := (obs.effective as TFhirDateTime).value
  else
    result := TFslDateTime.makeNull;
end;

function TFhirObservation4.GetEffectivePeriod: TFHIRPeriodW;
begin
  if obs.effective is TFhirPeriod then
    result := TFHIRPeriod4.Create(obs.effective.Link)
  else
    result := nil;
end;

function TFhirObservation4.GetIssued: TFslDateTime;
begin
  result := obs.issued;
end;

function TFhirObservation4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirObservation4.hasDevice: boolean;
begin
  result := obs.Device <> nil;
end;

function TFhirObservation4.hasEffective: boolean;
begin
  result := obs.Effective <> nil;
end;

function TFhirObservation4.hasIssued: boolean;
begin
  result := obs.IssuedElement <> nil;
end;

function TFhirObservation4.hasMethod: boolean;
begin
  result := obs.Method <> nil;
end;

function TFhirObservation4.hasSubject: boolean;
begin
  result := obs.Subject <> nil;
end;

function TFhirObservation4.method(force : boolean) : TFhirCodeableConceptW;
begin
  if (obs.method = nil) and force then
    obs.method := TFhirCodeableConcept.Create;

  if obs.method = nil then
    result := nil
  else
    result := TFhirCodeableConcept4.Create(obs.method.link);
end;

function TFhirObservation4.GetSubject: String;
begin
  if obs.subject <> nil then
    result := obs.subject.reference
  else
    result := '';
end;

procedure TFhirObservation4.addCategory(c: TFHIRCodingW);
begin
  obs.categoryList.Append.codingList.add((c.Element as TFHIRCoding).Link);
end;

procedure TFhirObservation4.addCategory(system, code, display: String);
begin
  obs.categoryList.Append.codingList.add(TFHIRCoding.Create(system, code, display));
end;

procedure TFhirObservation4.setCode(text: String);
begin
  if obs.code = nil then
    obs.code := TFhirCodeableConcept.Create;
  obs.code.text := text;
end;

{ TFHIRQuantity4 }

function TFHIRQuantity4.asDuration: TDateTime;
begin
  result := qty.asDuration;
end;

function TFHIRQuantity4.getCode: String;
begin
  result := qty.code;
end;

function TFHIRQuantity4.getSystem: String;
begin
  result := qty.system;
end;

function TFHIRQuantity4.getUnit: String;
begin
  result := qty.unit_;
end;

function TFHIRQuantity4.getValue: String;
begin
  result := qty.value;
end;

function TFHIRQuantity4.qty: TFHIRQuantity;
begin
  result := Element as TFHIRQuantity;
end;

function TFHIRQuantity4.renderText: String;
begin
  result := gen(element as TFhirQuantity);
end;

procedure TFHIRQuantity4.setCode(Value: String);
begin
  qty.code := Value;
end;

procedure TFHIRQuantity4.setSystem(Value: String);
begin
  qty.system := Value;
end;

procedure TFHIRQuantity4.setUnit(Value: String);
begin
  qty.unit_ := Value;
end;

procedure TFHIRQuantity4.setValue(Value: String);
begin
  qty.value := Value;
end;

function TFHIRQuantity4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

{ TFHIRLookupOpRequest4 }

function TFHIRLookupOpRequest4.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationRequest).asParams;
end;

function TFHIRLookupOpRequest4.coding: TFHIRCodingW;
begin
  if (op as TFHIRLookupOpRequest).coding = nil then
    result := nil
  else
  begin
    result := TFHIRCoding4.Create((op as TFHIRLookupOpRequest).coding.Link);
    list.add(result);
  end;
end;

function TFHIRLookupOpRequest4.displayLanguage: String;
begin
  result := (op as TFHIRLookupOpRequest).displayLanguage;
end;

procedure TFHIRLookupOpRequest4.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpRequest).load(params);
end;

procedure TFHIRLookupOpRequest4.loadCoding;
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

function TFHIRLookupOpRequest4.propList: TArray<String>;
var
  i : integer;
begin
  SetLength(result, (op as TFHIRLookupOpRequest).property_List.Count);
  for i := 0 to (op as TFHIRLookupOpRequest).property_List.Count -1 do
    result[i] := (op as TFHIRLookupOpRequest).property_List[i];
end;

procedure TFHIRLookupOpRequest4.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpRequest).load(params as TFhirParameters);
end;

{ TFHIRSubsumesOpRequest4 }

function TFHIRSubsumesOpRequest4.asParams: TFHIRResourceV;
begin
  result := (op as TFHIRSubsumesOpRequest).asParams;
end;

procedure TFHIRSubsumesOpRequest4.load(params: TFHIRResourceV);
begin
  (op as TFHIRSubsumesOpRequest).load(params as TFHIRParameters);
end;

function TFHIRSubsumesOpRequest4.codeA: String;
begin
  result := (op as TFHIRSubsumesOpRequest).codeA;
end;

function TFHIRSubsumesOpRequest4.codeB: String;
begin
  result := (op as TFHIRSubsumesOpRequest).codeB;
end;

function TFHIRSubsumesOpRequest4.codingA: TFHIRCodingW;
begin
  result := TFHIRCoding4.Create((op as TFHIRSubsumesOpRequest).codingA);
end;

function TFHIRSubsumesOpRequest4.codingB: TFHIRCodingW;
begin
  result := TFHIRCoding4.Create((op as TFHIRSubsumesOpRequest).codingB);
end;

function TFHIRSubsumesOpRequest4.hasCodingA: boolean;
begin
  result := (op as TFHIRSubsumesOpRequest).codingA <> nil;
end;

function TFHIRSubsumesOpRequest4.hasCodingB: boolean;
begin
  result := (op as TFHIRSubsumesOpRequest).codingB <> nil;
end;

function TFHIRSubsumesOpRequest4.version: String;
begin
  result := (op as TFHIRSubsumesOpRequest).version;
end;

procedure TFHIRSubsumesOpRequest4.load(params: THTTPParameters);
begin
  (op as TFHIRSubsumesOpRequest).load(params);
end;

function TFHIRSubsumesOpRequest4.systemUri: String;
begin
  result := (op as TFHIRSubsumesOpRequest).system;
end;

{ TFHIRSubsumesOpResponse4 }

function TFHIRSubsumesOpResponse4.asParams: TFHIRResourceV;
begin
  result := (op as TFHIRSubsumesOpResponse).asParams;
end;

procedure TFHIRSubsumesOpResponse4.load(params: TFHIRResourceV);
begin
  (op as TFHIRSubsumesOpResponse).load(params as TFHIRParameters);
end;

function TFHIRSubsumesOpResponse4.GetOutcome: String;
begin
  result := (op as TFHIRSubsumesOpResponse).outcome;
end;

procedure TFHIRSubsumesOpResponse4.load(params: THTTPParameters);
begin
  (op as TFHIRSubsumesOpResponse).load(params);
end;

procedure TFHIRSubsumesOpResponse4.SetOutcome(Value: String);
begin
  (op as TFHIRSubsumesOpResponse).outcome := value;
end;

{ TFhirCodeableConcept4 }

procedure TFhirCodeableConcept4.addCoding(coding: TFHIRCodingW);
var
  list : TFHIRCodingList;
  c : TFHIRCoding;
begin
  list := (Element as TFhirCodeableConcept).codingList;
  c := (coding.Element as TFHIRCoding).link;
  list.Add(c);
end;

function TFhirCodeableConcept4.addCoding: TFHIRCodingW;
begin
  result := TFHIRCoding4.Create((Element as TFhirCodeableConcept).codingList.Append.link);
end;

procedure TFhirCodeableConcept4.removeCoding(systemUri, version, code: String);
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

function TFhirCodeableConcept4.codingCount: integer;
begin
  result := (Element as TFhirCodeableConcept).codingList.Count;
end;

function TFhirCodeableConcept4.codings: TFslList<TFhirCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFhirCodingW>.Create;
  for c in (Element as TFhirCodeableConcept).codingList do
    result.Add(TFHIRCoding4.Create(c.Link));
end;

function TFhirCodeableConcept4.fromSystem(Systems: TArray<String>; required: boolean): String;
begin
  result := (Element as TFhirCodeableConcept).fromSystem(Systems, required);
end;

function TFhirCodeableConcept4.GetText: String;
begin
  result := (Element as TFhirCodeableConcept).text;
end;

function TFhirCodeableConcept4.renderText: String;
begin
  result := gen(element as TFhirCodeableConcept);
end;

function TFhirCodeableConcept4.hasCode(systemUri, code: String): boolean;
begin
  result := (Element as TFhirCodeableConcept).hasCode(systemUri, code);
end;

function TFhirCodeableConcept4.hasCode(systemUri, version, code: String): boolean;
begin
  result := (Element as TFhirCodeableConcept).hasCode(systemUri, version, code);
end;

procedure TFhirCodeableConcept4.SetText(const Value: String);
begin
  (Element as TFhirCodeableConcept).text := value;
end;

function TFhirCodeableConcept4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirCodeableConcept4.fromSystem(System: String; required: boolean): String;
begin
  result := (Element as TFhirCodeableConcept).fromSystem(System, required);
end;

function TFhirCodeableConcept4.summary: String;
begin
  result := summarise(Element as TFhirCodeableConcept);
end;

procedure TFhirCodeableConcept4.clearCodings;
begin
  (Element as TFhirCodeableConcept).codingList.Clear;
end;

procedure TFhirCodeableConcept4.addCoding(systemUri, version, code, display : String);
begin
  (Element as TFhirCodeableConcept).addCoding(systemUri, version, code, display);
end;


{ TFHIRGroup4 }

function TFHIRGroup4.characteristics: TFslList<TFHIRGroupCharacteristicW>;
var
  gc : TFHIRGroupCharacteristic;
begin
  result := TFslList<TFHIRGroupCharacteristicW>.Create;
  for gc in (Resource as TFHIRGroup).characteristicList do
    result.add(TFHIRGroupCharacteristic4.Create(gc.link));
end;

function TFHIRGroup4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRGroup4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRGroup4.hasCharacteristics: boolean;
begin
  result := (Resource as TFHIRGroup).characteristicList.count > 0;
end;

function TFHIRGroup4.hasMembers: boolean;
begin
  result := (Resource as TFHIRGroup).memberList.count > 0;
end;

function TFHIRGroup4.name: String;
begin
  result := (Resource as TFHIRGroup).name;
end;

procedure TFHIRGroup4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFHIRGroupCharacteristic4 }

function TFHIRGroupCharacteristic4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRGroupCharacteristic4.code: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConcept4.Create((element as TFHIRGroupCharacteristic).code);
end;

function TFHIRGroupCharacteristic4.value: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConcept4.Create((element as TFHIRGroupCharacteristic).value);
end;

{ TFHIRStatsOpResponse4 }

procedure TFHIRStatsOpResponse4.addObs(obs: TFHIRResourceV);
begin
  (Op as TFHIRStatsOpResponse).sourceList.Add(obs as TFhirObservation);
end;

function TFHIRStatsOpResponse4.asParams: TFHIRResourceV;
begin
  result := (op as TFHIRLookupOpResponse).asParams;
end;

procedure TFHIRStatsOpResponse4.load(params: THTTPParameters);
begin
  (op as TFHIRLookupOpResponse).load(params);
end;

procedure TFHIRStatsOpResponse4.load(params: TFHIRResourceV);
begin
  (op as TFHIRLookupOpResponse).load(params as TFHIRParameters);
end;

{ TFHIRNamingSystem4 }

function TFHIRNamingSystem4.getContext: String;
begin
  result := '';
end;

function TFHIRNamingSystem4.getDate: TFslDateTime;
begin
  result := nm.date;
end;

function TFHIRNamingSystem4.getDescription: String;
begin
  result := nm.description;
end;

function TFHIRNamingSystem4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRNamingSystem4.getName: String;
begin
  result := nm.name;
end;

function TFHIRNamingSystem4.getPublisher: String;
begin
  result := nm.publisher;
end;

function TFHIRNamingSystem4.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[nm.status];
end;

function TFHIRNamingSystem4.getUri: String;
begin
  result := nm.getUri;
end;

function TFHIRNamingSystem4.getURL: String;
begin
  result := '';
end;

function TFHIRNamingSystem4.getVersion: String;
begin
  result := '';
end;

function TFHIRNamingSystem4.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (nm.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(nm.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
end;

function TFHIRNamingSystem4.hasOid(oid: String): boolean;
begin
  result := nm.hasOid(oid);
end;

function TFHIRNamingSystem4.nm: TFHIRNamingSystem;
begin
  result := (resource as TFHIRNamingSystem);
end;

procedure TFHIRNamingSystem4.setDate(Value: TFslDateTime);
begin
  nm.date := value;
end;

procedure TFHIRNamingSystem4.setDescription(Value: String);
begin
  nm.description := value;
end;

procedure TFHIRNamingSystem4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRNamingSystem4.setName(Value: String);
begin
  nm.name := value;
end;

procedure TFHIRNamingSystem4.setPublisher(Value: String);
begin
  nm.publisher := value;
end;

function TFHIRNamingSystem4.getTitle: String;
begin
  result := nm.title;
end;

procedure TFHIRNamingSystem4.setTitle(value: String);
begin
  nm.title := value;
end;

procedure TFHIRNamingSystem4.setStatus(Value: TPublicationStatus);
begin
  nm.status := MAP_TPublicationStatus[value];
end;

procedure TFHIRNamingSystem4.setUrl(Value: String);
begin
  // nothing
end;

procedure TFHIRNamingSystem4.setVersion(Value: String);
begin
  // nothing
end;

function TFHIRNamingSystem4.getExperimental: boolean;
begin
  result := nm.experimental;
end;

procedure TFHIRNamingSystem4.setExperimental(value: boolean);
begin
  nm.experimental := value;
end;

function TFHIRNamingSystem4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;


{ TFHIRStructureMap4 }

function TFHIRStructureMap4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRStructureMap4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRStructureMap4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRStructureMap4.url: String;
begin
  result := (Resource as TFHIRStructureMap).url;
end;

{ TFHIREventDefinition4 }

function TFHIREventDefinition4.dataType: String;
begin
  if ed.triggerList.Count = 0 then
    result := ''
  else if ed.triggerList[0].dataList.Count = 0 then
    result := ''
  else
    result := CODES_TFhirAllTypesEnum[ed.triggerList[0].dataList[0].type_];
end;

function TFHIREventDefinition4.ed: TFHIREventDefinition;
begin
  result := resource as TFHIREventDefinition;
end;

function TFHIREventDefinition4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIREventDefinition4.expression: String;
begin
  if ed.triggerList.Count = 0 then
    result := ''
  else if ed.triggerList[0].condition = nil then
    result := ''
  else
    result := ed.triggerList[0].condition.expression;
end;

function TFHIREventDefinition4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIREventDefinition4.language: String;
begin
  if ed.triggerList.Count = 0 then
    result := ''
  else if ed.triggerList[0].condition = nil then
    result := ''
  else
    result := ed.triggerList[0].condition.language;
end;

procedure TFHIREventDefinition4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

//const
//  MAP_TTriggerType : array [TFhirTriggerTypeEnum] of TTriggerType = (ttNull, ttNamedEvent, ttPeriodic, ttDataChanged, ttDataAdded, ttDataModified, ttDataRemoved, ttDataAccessed, ttDataAccessEnded);

function TFHIREventDefinition4.triggerType: TTriggerType;
begin
//  result := MAP_TTriggerType[ed.trigger.type_];
  result := ttNull;
end;

function TFHIREventDefinition4.url: String;
begin
  result := ed.url;
end;

{ TFhirPatient4 }

function TFhirPatient4.pat: TFHIRPatient;
begin
  result := resource as TFhirPatient;
end;

function TFhirPatient4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirPatient4.nameSummary: String;
begin
  result := HumanNamesAsText(pat.nameList);
end;

function TFhirPatient4.activeStr: String;
begin
  if pat.activeElement = nil then
    result := ''
  else if pat.active then
    result := 'true'
  else
    result := 'false';
end;

function TFhirPatient4.GetActive: boolean;
begin
  result := pat.active;
end;

procedure TFhirPatient4.SetActive(const Value: boolean);
begin
  pat.active := value;
end;

function TFhirPatient4.gender: String;
begin
  result := CODES_TFhirAdministrativeGenderEnum[pat.gender];
end;

function TFhirPatient4.genderPlus: String;
begin
  result := pat.genderPlus;
end;

function TFhirPatient4.GetFamily: String;
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

procedure TFhirPatient4.SetFamily(const Value: String);
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
function TFhirPatient4.GetDob: String;
begin
  result := pat.birthDate.toXML;
end;

procedure TFhirPatient4.SetDob(const Value: String);
begin
  pat.birthDate := TFslDateTime.fromXML(value);
end;

function TFhirPatient4.GetIdentifier(systemUri: String): String;
var
  id : TFhirIdentifier;
begin
  result := '';
  for id in pat.identifierList do
    if id.system = systemUri then
      exit(id.value);
end;

procedure TFhirPatient4.SetIdentifier(systemUri: String; const Value: String);
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

function TFhirPatient4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirPatient4.identifierSummary: String;
begin
  result := IdentifiersAsText(pat.identifierList);
end;

procedure TFhirPatient4.addGiven(name: String);
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

function TFhirPatient4.contactSummary: String;
begin
  result := ContactsAsText(pat.telecomList);
end;

procedure TFhirPatient4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirTerminologyCapabilities4 }

procedure TFhirTerminologyCapabilities4.addExpansionParameter(code, doco: String);
begin
  if (tc.expansion = nil) then
    tc.expansion := TFhirTerminologyCapabilitiesExpansion.Create;
  with tc.expansion.parameterList.Append do
  begin
    name := code;
    documentation := doco;
  end;
end;

procedure TFhirTerminologyCapabilities4.contact(kind: TContactType; value: String);
var
  c : TFhirContactPoint;
  ct : TFhirConformanceContact;
begin
  ct := tc.contactList.Append;
  c := ct.telecomList.Append;
  c.system := MAP_TContactType[kind];
  c.value := 'http://healthintersections.com.au/';
end;

function TFhirTerminologyCapabilities4.getContext: String;
begin
  result := tc.context;
end;

function TFhirTerminologyCapabilities4.getDate: TFslDateTime;
begin
  result := tc.date;
end;

function TFhirTerminologyCapabilities4.getDescription: String;
begin
  result := tc.description;
end;

function TFhirTerminologyCapabilities4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirTerminologyCapabilities4.getName: String;
begin
  result := tc.name;
end;

function TFhirTerminologyCapabilities4.getPublisher: String;
begin
  result := tc.publisher;
end;

function TFhirTerminologyCapabilities4.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[tc.Status];
end;

function TFhirTerminologyCapabilities4.getURL: String;
begin
  result := tc.url;
end;

function TFhirTerminologyCapabilities4.getVersion: String;
begin
  result := tc.version;
end;


procedure TFhirTerminologyCapabilities4.setContext(Value: String);
begin
end;


procedure TFhirTerminologyCapabilities4.setDate(Value: TFslDateTime);
begin
  tc.date := value;
end;


procedure TFhirTerminologyCapabilities4.setDescription(Value: String);
begin
  tc.description := value;
end;


procedure TFhirTerminologyCapabilities4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirTerminologyCapabilities4.setName(Value: String);
begin
  tc.name := value;
end;


procedure TFhirTerminologyCapabilities4.setPublisher(Value: String);
begin
  tc.publisher := value;
end;

function TFhirTerminologyCapabilities4.getTitle: String;
begin
  result := tc.title;
end;

procedure TFhirTerminologyCapabilities4.setTitle(value: String);
begin
  tc.title := value;
end;


procedure TFhirTerminologyCapabilities4.setStatus(Value: TPublicationStatus);
begin
  tc.Status := MAP_TPublicationStatus[Value];
end;


procedure TFhirTerminologyCapabilities4.setUrl(Value: String);
begin
  tc.url := value;
end;


procedure TFhirTerminologyCapabilities4.setVersion(Value: String);
begin
  tc.version := value;
end;

function TFhirTerminologyCapabilities4.getKind: TCapabilityStatementKind;
begin
  case tc.kind of
    CapabilityStatementKindInstance : result := cskInstance;
    CapabilityStatementKindCapability : result := cskCapability;
    CapabilityStatementKindRequirements : result := cskRequirements;
  else
    result := cskNull;
  end;
end;

procedure TFhirTerminologyCapabilities4.setKind(Value: TCapabilityStatementKind);
begin
  case value of
    cskInstance : tc.kind := CapabilityStatementKindInstance;
    cskCapability : tc.kind := CapabilityStatementKindCapability;
    cskRequirements : tc.kind := CapabilityStatementKindRequirements;
  else
    tc.kind := CapabilityStatementKindNull;
  end;
end;

function TFhirTerminologyCapabilities4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;


procedure TFhirTerminologyCapabilities4.systemUri(url: String);
begin
  tc.codeSystemList.Append.uri := url;
end;

procedure TFhirTerminologyCapabilities4.systemVersion(url: String);
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

function TFhirTerminologyCapabilities4.tc: TFhirTerminologyCapabilities;
begin
  result := (Fres as TFhirTerminologyCapabilities);
end;


{ TFHIRPeriod4 }

function TFHIRPeriod4.GetEnd: TFslDateTime;
begin
  result := period.end_;
end;

function TFHIRPeriod4.GetStart: TFslDateTime;
begin
  result := period.start;
end;

function TFHIRPeriod4.period: TFHIRPeriod;
begin
  result := Element as TFHIRPeriod;
end;

function TFHIRPeriod4.renderText: String;
begin
  result := gen(element as TFhirPeriod);
end;

procedure TFHIRPeriod4.SetEnd(const Value: TFslDateTime);

begin
  period.end_ := value;
end;

procedure TFHIRPeriod4.SetStart(const Value: TFslDateTime);

begin
  period.start := value;
end;

function TFHIRPeriod4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

{ TFHIRConsent4 }

function TFHIRConsent4.consent: TFHIRConsent;
begin
  result := resource as TFHIRConsent;
end;

function TFHIRConsent4.GetActive: boolean;
begin
  result := consent.status = ConsentStateCodesActive;
end;

function TFHIRConsent4.GetDateTime: TFslDateTime;
begin
  result := consent.dateTime;
end;

function TFHIRConsent4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRConsent4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRConsent4.GetPatient: String;
begin
  if consent.patient <> nil then
    result := consent.patient.reference
  else
    result := '';
end;

function TFHIRConsent4.listProvisions: TFslList<TFhirConsentProvisionW>;
begin
  result := nil; // for now
end;

procedure TFHIRConsent4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirEncounter4 }

function TFhirEncounter4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirEncounter4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirEncounter4.patientId: String;
begin
  result := (FRes as TFHIREncounter).subject.getId;
end;

procedure TFhirEncounter4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFhirEncounter4.summary: String;
begin
  result := CODES_TFhirEncounterStatusEnum[(FRes as TFHIREncounter).status];
end;

{ TFhirTestScript4 }

function TFHIRTestScript4.getContext: String;
begin
  result := '';
end;

function TFHIRTestScript4.getExperimental: boolean;
begin
  result := ts.experimental;
end;

procedure TFHIRTestScript4.setExperimental(value: boolean);
begin
  ts.experimental := value;
end;

function TFHIRTestScript4.getDate: TFslDateTime;
begin
  result := ts.date;
end;

function TFHIRTestScript4.getDescription: String;
begin
  result := ts.description;
end;

function TFHIRTestScript4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRTestScript4.getName: String;
begin
  result := ts.name;
end;

function TFHIRTestScript4.getPublisher: String;
begin
  result := ts.publisher;
end;

function TFHIRTestScript4.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[ts.Status];
end;

function TFHIRTestScript4.getURL: String;
begin
  result := ts.url;
end;

function TFHIRTestScript4.getVersion: String;
begin
  result := ts.version;
end;

function TFHIRTestScript4.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (ts.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(ts.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
end;

procedure TFHIRTestScript4.setDate(Value: TFslDateTime);
begin
  ts.date := value;
end;


procedure TFHIRTestScript4.setDescription(Value: String);
begin
  ts.description := value;
end;


procedure TFHIRTestScript4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRTestScript4.setName(Value: String);
begin
  ts.name := value;
end;


procedure TFHIRTestScript4.setPublisher(Value: String);
begin
  ts.publisher := value;
end;

function TFHIRTestScript4.getTitle: String;
begin
  result := ts.title;
end;

procedure TFHIRTestScript4.setTitle(value: String);
begin
  ts.title := value;
end;


procedure TFHIRTestScript4.setStatus(Value: TPublicationStatus);
begin
  ts.Status := MAP_TPublicationStatus[Value];
end;


procedure TFHIRTestScript4.setUrl(Value: String);
begin
  ts.url := value;
end;

procedure TFHIRTestScript4.setVersion(Value: String);
begin
  ts.version := value;
end;

function TFHIRTestScript4.ts: TFHIRTestScript;
begin
  result := (Fres as TFhirTestScript);
end;


function TFHIRTestScript4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

{ TFhirProvenance4 }

procedure TFhirProvenance4.addTarget(url: String);
begin
  p.targetList.Append.reference := url;
end;

procedure TFhirProvenance4.clearSignatures;
begin
  p.signatureList.Clear;
end;

procedure TFhirProvenance4.clearTargets;
begin
  p.targetList.Clear;
end;

function TFhirProvenance4.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirProvenance4.p: TFhirProvenance;
begin
  result := (Fres as TFhirProvenance);
end;

function TFhirProvenance4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

procedure TFhirProvenance4.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirConceptMapGroupElementDependsOn4 }

function TFhirConceptMapGroupElementDependsOn4.display: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).display;
end;

function TFhirConceptMapGroupElementDependsOn4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFhirConceptMapGroupElementDependsOn4.property_: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).property_;
end;

function TFhirConceptMapGroupElementDependsOn4.system_: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).system;
end;

function TFhirConceptMapGroupElementDependsOn4.value: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).code;
end;


{ TFHIRAttachment4 }

function TFHIRAttachment4.att: TFhirAttachment;
begin
  result := Element as TFhirAttachment;
end;

function TFHIRAttachment4.GetContentType: String;
begin
  result := att.contentType;
end;

function TFHIRAttachment4.GetData: TBytes;
begin
  result := att.data;
end;

function TFHIRAttachment4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

function TFHIRAttachment4.renderText: String;
begin
  result := '??';
end;

{ TFhirImmunization4 }

function TFhirImmunization4.code(systemUri: String): String;
var
  c : TFHIRCoding;
begin
  result := '';
  if imm.vaccineCode <> nil then
    for c in imm.vaccineCode.codingList do
      if (c.system = systemUri) then
        exit(c.code);
end;

function TFhirImmunization4.GetDate: TFslDateTime;
begin
  if imm.occurrence is TFhirDateTime then
    result := (imm.occurrence as TFhirDateTime).value
  else
    result := TFslDateTime.makeNull;
end;

function TFhirImmunization4.GetLanguage: String;
begin
  result := imm.language;
end;

function TFhirImmunization4.GetLotNumber: String;
begin
  result := imm.lotNumber;
end;

function TFhirImmunization4.GetManufacturerIdSystem: String;
begin
  if (imm.manufacturer <> nil) and (imm.manufacturer.identifier <> nil) then
    result := imm.manufacturer.identifier.system
  else
    result := '';
end;

function TFhirImmunization4.GetManufacturerIdValue: String;
begin
  if (imm.manufacturer <> nil) and (imm.manufacturer.identifier <> nil) then
    result := imm.manufacturer.identifier.value
  else
    result := '';
end;

function TFhirImmunization4.GetPatient: String;
begin
  if imm.patient = nil then
    result := ''
  else
    result := imm.patient.reference;
end;

function TFhirImmunization4.GetPerformerDisplay: String;
begin
  result := '';
  if (imm.performerList.Count > 0) and (imm.performerList[0].actor <> nil) then
    result := imm.performerList[0].actor.display;
end;

function TFhirImmunization4.GetStatus: string;
begin
  result := imm.statusElement.value;
end;

function TFhirImmunization4.hasCode(systemUri, code: String): boolean;
var
  c : TFHIRCoding;
begin
  result := false;
  if imm.vaccineCode <> nil then
    for c in imm.vaccineCode.codingList do
      if (c.system = systemUri) and (c.code = code) then
        exit(true);
end;

function TFhirImmunization4.imm: TFhirImmunization;
begin
  result := resource as TFhirImmunization;
end;

procedure TFhirImmunization4.setCodeBySystem(systemUri: String; code: String);
var
  c : TFHIRCoding;
begin
  if imm.vaccineCode = nil then
    imm.vaccineCode := TFhirCodeableConcept.Create;
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

procedure TFhirImmunization4.SetDate(const Value: TFslDateTime);
begin
  imm.occurrence := TFHIRDateTime.Create(value);
end;

function TFhirImmunization4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

procedure TFhirImmunization4.SetLanguage(const Value: String);
begin
  imm.language := value;
end;

procedure TFhirImmunization4.SetLotNumber(const Value: String);
begin
  imm.lotNumber := value;
end;

procedure TFhirImmunization4.SetManufacturerIdSystem(const Value: String);
begin
  if imm.manufacturer = nil then
    imm.manufacturer := TFhirReference.Create;
  if imm.manufacturer.identifier = nil then
    imm.manufacturer.identifier := TFhirIdentifier.Create;
  imm.manufacturer.identifier.system := value;
end;

procedure TFhirImmunization4.SetManufacturerIdValue(const Value: String);
begin
  if imm.manufacturer = nil then
    imm.manufacturer := TFhirReference.Create;
  if imm.manufacturer.identifier = nil then
    imm.manufacturer.identifier := TFhirIdentifier.Create;
  imm.manufacturer.identifier.value := value;
end;

procedure TFhirImmunization4.SetPatient(const Value: String);
begin
  if (imm.patient = nil) then
    imm.patient := TFhirReference.Create;
  imm.patient.reference := value;
end;

procedure TFhirImmunization4.SetPerformerDisplay(const Value: String);
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

procedure TFhirImmunization4.SetStatus(const Value: string);
begin
  if imm.statusElement = nil then
    imm.status := ImmunizationStatusCompleted; // force it to exist
  imm.statusElement.value := value;
end;

{ TFhirIdentifier4 }

function TFhirIdentifier4.id: TFHIRIdentifier;
begin
  result := FElement as TFHIRIdentifier;
end;

function TFhirIdentifier4.renderText: String;
begin
  result := gen(element as TFhirIdentifier);
end;

function TFhirIdentifier4.GetSystem: String;
begin
  result := id.system;
end;

function TFhirIdentifier4.GetValue: String;
begin
  result := id.value;
end;

procedure TFhirIdentifier4.SetSystem(const Value: String);
begin
  id.system := value;
end;

procedure TFhirIdentifier4.SetValue(const Value: String);
begin
  id.value := value;
end;

function TFhirIdentifier4.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension4.Create(extension.link);
end;

const
  identifier_use_version_to_general : array [TFhirIdentifierUseEnum] of TIdentifierUse = (iuNull, iuUsual, iuOfficial, iuTemp, iuSecondary, iuOld);
  identifier_use_general_to_version : array [TIdentifierUse] of TFhirIdentifierUseEnum = (IdentifierUseNull, IdentifierUseUsual, IdentifierUseOfficial, IdentifierUseTemp, IdentifierUseSecondary, IdentifierUseOld);

function TFhirIdentifier4.GetUse: TIdentifierUse;
begin
  result := identifier_use_version_to_general[id.use];
end;

procedure TFhirIdentifier4.SetUse(const Value: TIdentifierUse);
begin
  id.use := identifier_use_general_to_version[value];
end;

function TFhirIdentifier4.GetTypeV: TFhirCodeableConceptW;
begin
  if id.type_ = nil then
    result := nil
  else
    result := TFhirCodeableConcept4.Create(id.type_.Link);
end;

procedure TFhirIdentifier4.SetTypeV(const Value: TFhirCodeableConceptW);
begin
  if value = nil then
    id.type_ := nil
  else
    id.type_ := ((Value as TFhirCodeableConcept4).FElement as TFhirCodeableConcept).link;
end;

end.
