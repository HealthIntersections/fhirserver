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
  fsl_base, fsl_utilities, fsl_http, fsl_lang,
  fhir_objects, fhir_common, fhir_extensions, fhir_features,
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
  MAP_TFilterOperatorR : array [TFilterOperator] of TFhirFilterOperatorEnum = (filterOperatorNull, filterOperatorEqual, filterOperatorIsA, filterOperatorNull, filterOperatorIsNotA, filterOperatorRegex, filterOperatorIn, filterOperatorNotIn, filterOperatorNull, filterOperatorNull, filterOperatorNull, filterOperatorNull, filterOperatorNull);
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
  MAP_TSubscriptionStatus2 : array [TSubscriptionStatus] of TFhirSubscriptionStatusEnum = (SubscriptionStatusNull, SubscriptionStatusRequested, SubscriptionStatusActive, SubscriptionStatusError, SubscriptionStatusOff, SubscriptionStatusNull);
  MAP_TObservationStatus : array [TObservationStatus] of TFhirObservationStatusEnum = (ObservationStatusNull, ObservationStatusRegistered, ObservationStatusPreliminary, ObservationStatusFinal, ObservationStatusAmended, ObservationStatusCorrected, ObservationStatusCancelled, ObservationStatusEnteredInError, ObservationStatusUnknown);
  MAP_TObservationStatus2 : array [TFhirObservationStatusEnum] of TObservationStatus = (obssNull, obssRegistered, obssPreliminary, obssFinal, obssAmended, obssCorrected, obssCancelled, obssEnteredInError, obssUnknown);

type
  { TFHIRPrimitive3 }

  TFHIRPrimitive3 = class (TFHIRPrimitiveW)
  public
    function GetAsString : String; override;
    procedure SetAsString(value : String); override;
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
  end;

  { TFHIRExtension3 }

  TFHIRExtension3 = class (TFHIRExtensionW)
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

  { TFHIRCoding3 }

  TFHIRCoding3 = class (TFHIRCodingW)
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

  { TFhirCodeableConcept3 }

  TFhirCodeableConcept3 = class (TFhirCodeableConceptW)
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

  { TFhirIdentifier3 }

  TFhirIdentifier3 = class (TFhirIdentifierW)
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

  { TFhirOperationOutcome3 }

  TFhirOperationOutcome3 = class (TFhirOperationOutcomeW)
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

  { TFHIRBundleEntry3 }

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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getLink(rel: String): String; override;
    procedure setLink(rel: String; const Value: String); override;
  end;

  { TFhirBinary3 }

  TFhirBinary3 = class (TFhirBinaryW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function ContentType : String; override;
    function content : TBytes; override;
  end;

  { TFHIRBundle3 }

  TFHIRBundle3 = class (TFHIRBundleW)
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

  { TFHIROperationOutcomeIssue3 }

  TFHIROperationOutcomeIssue3 = class (TFHIROperationOutcomeIssueW)
  private
    function issue : TFHIROperationOutcomeIssue;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function display : String; override;
    function severity : TIssueSeverity; override;
    procedure addCode(systemUri, code : String); override;
    function getDiagnostics: String; override;
    procedure setDiagnostics(Value: String); override;
  end;

  { TFHIRSearchParamDefinition3 }

  TFHIRSearchParamDefinition3 = class (TFHIRSearchParamDefinitionW)
  private
    function param : TFhirCapabilityStatementRestResourceSearchParam;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function name : String; override;
    function documentation : String; override;
    function type_ : TFHIRSearchParamType; override;
  end;

  { TFhirCapabilityStatementRestResource3 }

  TFhirCapabilityStatementRestResource3 = class (TFhirCapabilityStatementRestResourceW)
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

  { TFHIRCapabilityStatement3 }

  TFHIRCapabilityStatement3 = class (TFHIRCapabilityStatementW)
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

  { TFhirElementDefinition3 }

  TFhirElementDefinition3 = class (TFhirElementDefinitionW)
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

  { TFHIRStructureDefinition3 }

  TFHIRStructureDefinition3 = class (TFhirStructureDefinitionW)
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

  { TFhirParametersParameter3 }

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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function name : String; override;
    function hasValue : boolean; override;
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

  { TFHIRParameters3 }

  TFHIRParameters3 = class (TFHIRParametersW)
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
    procedure addParam(name : String; value : TFHIRObject); override;
    procedure addParamCode(name : String; value : string); override;
    procedure addParamUri(name : String; value : string); override;
    procedure addParamCanonical(name : String; value : string); override;
    function addParam(name : String) : TFhirParametersParameterW; override;
    function bool(name : String) : boolean; override;
    function str(name : String) : String; override;
    function has(name : String) : boolean; override;
    function obj(name : String) : TFHIRObject; override;
    function getParameter(name: String): TFhirParametersParameterW;  override;
    function names : String; override;
  end;

  { TFHIRExpansionProfile3 }

  TFHIRExpansionProfile3 = class (TFHIRParametersW)
  private
    function profile : TFhirExpansionProfile;
  protected
    procedure populateList; override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamStr(name : String; value : string); override;
    procedure addParam(name : String; value : TFHIRObject); override;
    procedure addParamCode(name : String; value : string); override;
    procedure addParamUri(name : String; value : string); override;
    procedure addParamCanonical(name : String; value : string); override;
    function addParam(name : String) : TFhirParametersParameterW; override;
    function bool(name : String) : boolean; override;
    function str(name : String) : String; override;
    function has(name : String) : boolean; override;
    function obj(name : String) : TFHIRObject; override;
    function getParameter(name: String): TFhirParametersParameterW;  override;
    function names : String; override;
  end;

  { TFhirExpansionProfileFixedVersion3 }

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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function name : String; override;
    function hasValue : boolean; override;
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

  { TFhirValueSetExpansionContains3 }

  TFhirValueSetExpansionContains3 = class (TFhirValueSetExpansionContainsW)
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

  { TFhirValueSetExpansion3 }

  TFhirValueSetExpansion3 = class (TFhirValueSetExpansionW)
  private
    function exp : TFhirValueSetExpansion;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    procedure addParamStr(name, value : String); override;
    procedure addParamCode(name, value : String); override;
    procedure addParamUri(name, value : String); override;
    procedure addParamInt(name : String; value : integer); override;
    procedure addParamBool(name : String; value : boolean); override;
    function hasParam(name : string) : boolean; overload; override;
    function hasParam(name, value : string) : boolean; overload; override;
    procedure copyParams(source : TFhirValueSetExpansionW); override;
    procedure addContains(item : TFhirValueSetExpansionContainsW); override;
    function addContains : TFhirValueSetExpansionContainsW; override;
    function makeContains : TFhirValueSetExpansionContainsW; override;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; override;
    function getTotal : integer; override;
    function getOffset : integer; override;
    procedure setOffset(value : integer) ; override;
    procedure setTotal(value : integer) ; override;
    procedure defineProperty(focus : TFhirValueSetExpansionContainsW; url, code : String; value : TFHIRObject); override;
  end;

  { TFhirValueSetComposeIncludeFilter3 }

  TFhirValueSetComposeIncludeFilter3 = class (TFhirValueSetComposeIncludeFilterW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function getProp : String; override;
    function getOp : TFilterOperator; override;
    function getValue : String; override;
    procedure setOp(Value: TFilterOperator); override;
    procedure setProp(Value: String); override;
    procedure setValue(Value: String); override;
  end;

  { TFhirValueSetComposeIncludeConceptDesignation3 }

  TFhirValueSetComposeIncludeConceptDesignation3 = class (TFhirValueSetComposeIncludeConceptDesignationW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function language : String; override;
    function value : String; override;
    function use : TFHIRCodingW; override;
    function valueElement : TFHIRPrimitiveW; override;
  end;

  { TFhirValueSetComposeIncludeConcept3 }

  TFhirValueSetComposeIncludeConcept3 = class (TFhirValueSetComposeIncludeConceptW)
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

  { TFhirValueSetComposeInclude3 }

  TFhirValueSetComposeInclude3 = class (TFhirValueSetComposeIncludeW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function valueSets : TArray<String>; override;
    function getSystem : String; override;
    function getVersion : String; override;
    procedure setSystem(Value: String); override;
    procedure setVersion(Value: String); override;
    function hasConcepts : boolean; override;
    function concepts : TFslList<TFhirValueSetComposeIncludeConceptW>; override;
    function hasFilters : boolean; override;
    function hasValueSets : boolean; override;
    function filterCount : integer; override;
    function conceptCount : integer; override;
    function filters : TFslList<TFhirValueSetComposeIncludeFilterW>; override;
    function addConcept : TFhirValueSetComposeIncludeConceptW; override;
    function addFilter : TFhirValueSetComposeIncludeFilterW; override;
    procedure addValueSet(value : String); override;
  end;

  { TFHIRValueSet3 }

  TFHIRValueSet3 = class (TFHIRValueSetW)
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
    function getVersionAlgorithm : TFHIRVersionAlgorithm; override;
    procedure setVersion(value : String); override;
    function getDescription : String; override;
    procedure setDescription(value : String); override;
    function getStatus: TPublicationStatus; override;
    procedure setStatus(Value: TPublicationStatus); override;
    function getDate: TFslDateTime; override;
    procedure setDate(Value: TFslDateTime); override;
    function excludeInactives : boolean; override;
    function addInclude : TFhirValueSetComposeIncludeW; override;
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function getTitle : String; override;
    procedure setTitle(value : String); override;
    function source : String; override;
    function findContains(systemUri, version, code : String) : TFhirValueSetExpansionContainsW; override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFhirCodeSystemConceptProperty3 }

  TFhirCodeSystemConceptProperty3 = class (TFhirCodeSystemConceptPropertyW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : String; override;
    function value : TFHIRObject; override;
  end;

  { TFhirCodeSystemConceptDesignation3 }

  TFhirCodeSystemConceptDesignation3 = class (TFhirCodeSystemConceptDesignationW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function language : String; override;
    function useGen : String; override;
    function use : TFHIRCodingW; override;
    function hasUse : boolean; override;
    function value : String; override;
    function valueElement : TFHIRPrimitiveW; override;
  end;

  { TFhirCodeSystemConcept3 }

  TFhirCodeSystemConcept3 = class (TFhirCodeSystemConceptW)
  private
    function c : TFhirCodeSystemConcept;
  public
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

  { TFhirCodeSystemProperty3 }

  TFhirCodeSystemProperty3 = class (TFhirCodeSystemPropertyW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : String; override;
    function uri : String; override;
    function type_ : TFhirCodeSystemPropertyType; override;
  end;

  { TFhirCodeSystem3 }

  TFhirCodeSystem3 = class (TFhirCodeSystemW)
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
    function copyright : String; override;
    function language : String; override;
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
    function getDate: TFslDateTime; override;
    function getStatus: TPublicationStatus; override;
    function getContext: String; override;
    function getPublisher: String; override;
    procedure setPublisher(Value: String); override;
    function getTitle : String; override;
    procedure setTitle(value : String); override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;

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
    function buildImplicitValueSet : TFHIRValueSetW; override;
    function hasAnyDisplays(langs : THTTPLanguageList) : boolean; override;
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

  TFHIRLookupOpRespSubProperty3 = class (TFHIRLookupOpRespSubPropertyW)
  public
    function getDescription: string; override;
    procedure setDescription(Value: string); override;
    function getValue: String; override;
    procedure setValue(Value: String); override;
  end;

  { TFHIRLookupOpRespProperty3 }

  TFHIRLookupOpRespProperty3 = class (TFHIRLookupOpRespPropertyW)
  public
    function getDescription: string; override;
    procedure setDescription(Value: string); override;
    function getValue: TFHIRObject; override;
    procedure setValue(Value: TFHIRObject); override;
    function addSubProp(name : String) : TFHIRLookupOpRespSubPropertyW; override;
  end;

  TFHIRLookupOpRespDesignation3 = class (TFHIRLookupOpRespDesignationW)
  public
    function getUse: TFHIRObject; override;
    procedure setUse(Value: TFHIRObject); override;
  end;

  { TFHIRLookupOpResponse3 }

  TFHIRLookupOpResponse3 = class (TFHIRLookupOpResponseW)
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

  { TFhirConceptMapGroupElementDependsOn3 }

  TFhirConceptMapGroupElementDependsOn3 = class (TFhirConceptMapGroupElementDependsOnW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function property_ : String; override;
    function system_ : String; override;
    function value : String; override;
    function display : String; override;
  end;

  { TFhirConceptMapGroupElementTarget3 }

  TFhirConceptMapGroupElementTarget3 = class (TFhirConceptMapGroupElementTargetW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code: String; override;
    function equivalence : TFHIRConceptEquivalence; override;
    function comments : String; override;
    function products : TFslList<TFhirConceptMapGroupElementDependsOnW>; override;
  end;

  { TFhirConceptMapGroupElement3 }

  TFhirConceptMapGroupElement3 = class (TFhirConceptMapGroupElementW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code: String; override;
    function targets : TFslList<TFhirConceptMapGroupElementTargetW>; override;
    function targetCount : integer; override;
    function addTarget(code : String; eq : TFHIRConceptEquivalence) : TFhirConceptMapGroupElementTargetW; override;
  end;

  { TFhirConceptMapGroup3 }

  TFhirConceptMapGroup3 = class (TFhirConceptMapGroupW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function elements : TFslList<TFhirConceptMapGroupElementW>; override;
    function addElement(code : String) : TFhirConceptMapGroupElementW; override;
    function source : String; override;
    function target : String; override;
  end;

  { TFhirConceptMap3 }

  TFhirConceptMap3 = class (TFhirConceptMapW)
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
    function getTitle : String; override;
    procedure setTitle(value : String); override;
    function sourceDesc : String; override;
    function targetDesc : String; override;
    function getExperimental : boolean; override;
    procedure setExperimental(value : boolean); override;
  end;

  { TFHIRMeta3 }

  TFHIRMeta3 = class (TFHIRMetaW)
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

  { TFHIRAuditEvent3 }

  TFHIRAuditEvent3 = class (TFhirAuditEventW)
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

  { TFHIRSubscription3 }

  TFHIRSubscription3 = class (TFHIRSubscriptionW)
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

  { TFhirObservationComponent3 }

  TFhirObservationComponent3 = class (TFhirObservationComponentW)
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

  { TFhirObservation3 }

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

  { TFHIRQuantity3 }

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
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function asDuration : TDateTime; override;
    function renderText : String; override;
  end;

  { TFHIRPeriod3 }

  TFHIRPeriod3 = class (TFHIRPeriodW)
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

  { TFHIRAttachment3 }

  TFHIRAttachment3 = class (TFHIRAttachmentW)
  private
    function att : TFhirAttachment;
  protected
    function GetContentType: String; override;
    function GetData: TBytes; override;
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
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

  { TFHIRGroupCharacteristic3 }

  TFHIRGroupCharacteristic3 = class (TFHIRGroupCharacteristicW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function code : TFhirCodeableConceptW; override;
    function value : TFhirCodeableConceptW; override;
  end;

  { TFHIRGroup3 }

  TFHIRGroup3 = class (TFHIRGroupW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function name : String; override;
    function hasMembers : boolean; override;
    function hasCharacteristics : boolean; override;
    function characteristics : TFslList<TFHIRGroupCharacteristicW>; override;
  end;

  { TFhirPatient3 }

  TFhirPatient3 = class (TFhirPatientW)
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

  { TFhirEncounter3 }

  TFhirEncounter3 = class (TFhirEncounterW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
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

  { TFHIRNamingSystem3 }

  TFHIRNamingSystem3 = class (TFHIRNamingSystemW)
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

  { TFHIRStructureMap3 }

  TFHIRStructureMap3 = class (TFHIRStructureMapW)
  public
    function wrapExtension(extension : TFHIRObject) : TFHIRExtensionW; override;
    function GetLanguage: String; override;
    procedure SetLanguage(const Value: String); override;
    function url : String; override;
  end;

  { TFhirTerminologyCapabilities3 }

  TFhirTerminologyCapabilities3 = class (TFhirTerminologyCapabilitiesW)
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

  { TFHIRConsent3 }

  TFHIRConsent3 = class (TFHIRConsentW)
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

  { TFHIRTestScript3 }

  TFHIRTestScript3 = class (TFHIRTestScriptW)
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

  { TFhirProvenance3 }

  TFhirProvenance3 = class (TFhirProvenanceW)
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

  { TFhirImmunization3 }

  TFhirImmunization3 = class (TFhirImmunizationW)
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
  fhir3_utilities;


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


{ TFHIRPrimitive3 }

function TFHIRPrimitive3.GetAsString: String;
begin
  result := (FElement as TFHIRPrimitiveType).StringValue;
end;

procedure TFHIRPrimitive3.SetAsString(value: String);
begin
  (FElement as TFHIRPrimitiveType).StringValue := value;
end;

function TFHIRPrimitive3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

{ TFhirOperationOutcome3 }

procedure TFhirOperationOutcome3.addIssue(issue: TFhirOperationOutcomeIssueW; free : boolean);
begin
  (Fres as TFhirOperationOutcome).issueList.Add((issue.Element as TFhirOperationOutcomeIssue).link);
  if free then
    issue.free;
end;

procedure TFhirOperationOutcome3.addIssueNoId(level: TIssueSeverity; cause: TFHIRIssueType; path, message : String; code : TOpIssueCode; addIfDuplicate : boolean);
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
  iss.expressionList.Add(path);
end;

procedure TFhirOperationOutcome3.addIssue(level: TIssueSeverity;
  cause: TFhirIssueType; path, msgId, message: String; code: TOpIssueCode;
  addIfDuplicate: boolean);
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
  iss.expressionList.Add(path);
  iss.addExtension('http://hl7.org/fhir/StructureDefinition/operationoutcome-message-id', msgid);
end;

procedure TFhirOperationOutcome3.addDiagsIssue(message: string);
var
  iss : TFhirOperationOutcomeIssue;
begin
  iss := (Fres as TFhirOperationOutcome).issueList.Append;
  iss.code := IssueTypeInformational;
  iss.severity := IssueSeverityInformation;
  iss.diagnostics := message;
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

function TFhirOperationOutcome3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirOperationOutcome3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirOperationOutcome3.hasErrors: boolean;
begin
  result := (Fres as TFhirOperationOutcome).hasErrors;
end;

function TFhirOperationOutcome3.hasIssues: boolean;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  result := not op.issueList.IsEmpty;
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
  result := TFslList<TFhirOperationOutcomeIssueW>.Create;
  for iss in (resource as TFhirOperationOutcome).issueList do
    result.Add(TFHIROperationOutcomeIssue3.Create(iss.Link));
end;

function TFhirOperationOutcome3.rule(level: TIssueSeverity; source: String; typeCode: TFhirIssueType; path: string; test: boolean; msg: string): boolean;
begin
  result := (resource as TFhirOperationOutcome).rule(ISSUE_SEVERITY_MAP2[level], source, ExceptionTypeTranslations[typeCode], path, test, msg);
end;

procedure TFhirOperationOutcome3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
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
  result := TFhirBundleEntry3.Create(bundle.entryList.append.link);
end;

function TFHIRBundle3.bundle: TFhirBundle;
begin
  result := resource as TFHIRBundle;
end;

function TFHIRBundle3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

procedure TFHIRBundle3.clearLinks;
begin
  bundle.link_List.Clear;
end;

function TFHIRBundle3.count(rtype: String): Integer;
var
  be : TFhirBundleEntry;
begin
  result := 0;
  for be in bundle.entryList do
    if (be.resource <> nil) and ((rtype = '') or (rtype = be.resource.fhirType)) then
      inc(result);
end;

function TFHIRBundle3.entries: TFslList<TFhirBundleEntryW>;
var
  be : TFHIRBundleEntry;
begin
  result := TFslList<TFhirBundleEntryW>.Create;
  try
    for be in bundle.entryList do
      result.Add(TFhirBundleEntry3.Create(be.Link));
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRBundle3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRBundle3.getLastUpdated: TFslDateTime;
begin
  if bundle.meta <> nil then
    result := TFslDateTime.makeNull
  else
    result := bundle.meta.lastUpdated;
end;

function TFHIRBundle3.getLink(rel: String): String;
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

procedure TFHIRBundle3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRBundle3.setLastUpdated(Value: TFslDateTime);
begin
  if bundle.meta = nil then
    bundle.meta := TFHIRMeta.Create;
  bundle.meta.lastUpdated := value;
end;

procedure TFHIRBundle3.setLink(rel: String; const Value: String);
begin
  bundle.Links[rel] := value;
end;

procedure TFHIRBundle3.setTimestamp(Value: TFslDateTime);
begin
end;

procedure TFHIRBundle3.setTotal(Value: integer);
begin
  bundle.total := inttostr(value);
end;

function TFHIRBundle3.title: String;
begin
  result := BUNDLE_TYPE_TITLE[bundle.type_];
end;

function TFHIRBundle3.getTimestamp: TFslDateTime;
begin
  result := TFslDateTime.makeUTC;
end;

function TFHIRBundle3.getTotal: integer;
begin
  result := StrToIntDef(bundle.total, -1);
end;

function TFHIRBundle3.getType: TBundleType;
begin
  result := MAP_TFHIRBundleTypeR[bundle.type_];
end;

procedure TFHIRBundle3.setType(value: TBundleType);
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

function TFHIROperationOutcomeIssue3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFHIROperationOutcomeIssue3.severity: TIssueSeverity;
begin
  result := ISSUE_SEVERITY_MAP[issue.severity];
end;

procedure TFHIROperationOutcomeIssue3.addCode(systemUri, code: String);
begin
  issue.details.addCoding(systemUri, '', code, '');
end;

function TFHIROperationOutcomeIssue3.getDiagnostics: String;
begin
  result := issue.diagnostics;
end;

procedure TFHIROperationOutcomeIssue3.setDiagnostics(Value: String);
begin
  issue.diagnostics := value;
end;

{ TFHIRCapabilityStatement3 }

procedure TFHIRCapabilityStatement3.addInstantiates(url: String);
begin
  statement.instantiatesList.Append.value := url;
end;

procedure TFHIRCapabilityStatement3.addTxFeature(version: String);
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
  result := TFhirCapabilityStatementRestResource3.Create(statement.restList[0].resourceList.append.link);
  result.code := code;
end;

procedure TFHIRCapabilityStatement3.addSmartExtensions(authorize, token, register, manage: String; caps: array of String
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

procedure TFHIRCapabilityStatement3.defineFeatures(features: TFslList<TFHIRFeature>);
begin
end;

function TFHIRCapabilityStatement3.getURL: String;
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
          list.Add(TFHIRSearchParamDefinition3.Create(sp.Link))
      else
      begin
        for rr in r.resourceList do
        begin
          if CODES_TFHIRResourceTypesEnum[rr.type_] = name then
            for sp in rr.searchParamList do
              list.Add(TFHIRSearchParamDefinition3.Create(sp.Link))
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

procedure TFHIRCapabilityStatement3.setUrl(Value: String);
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

function TFHIRCapabilityStatement3.getTitle : String;
begin
  result := statement.Title;
end;

procedure TFHIRCapabilityStatement3.setTitle(value : String);
begin
  statement.Title := value;
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
  statement.software.releaseDate := TFslDateTime.fromHL7(release);
end;

function TFHIRCapabilityStatement3.getDescription : String;
begin
  result := statement.Description;
end;

function TFHIRCapabilityStatement3.getFhirVersion: string;
begin
  result := statement.fhirVersion;
end;

procedure TFHIRCapabilityStatement3.setDescription(value : String);
begin
  statement.Description := value;
end;

procedure TFHIRCapabilityStatement3.setFhirVersion(Value: string);
begin
  statement.fhirVersion := value;
end;

function TFHIRCapabilityStatement3.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[statement.Status];
end;

procedure TFHIRCapabilityStatement3.setStatus(Value: TPublicationStatus);
begin
  statement.Status := MAP_TPublicationStatus[value];
end;

procedure TFHIRCapabilityStatement3.fmt(mt: String);
begin
  statement.formatList.Append.value := mt;
end;

function TFHIRCapabilityStatement3.getDate: TFslDateTime;
begin
  result := statement.Date;
end;

procedure TFHIRCapabilityStatement3.setDate(Value: TFslDateTime);
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
  statement.text := TFhirNarrative.Create;
  statement.text.status := NarrativeStatusGenerated;
  if (ts <> '') then
    statement.instantiatesList.AddItem(TFHIRUri.Create(ts));
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

function TFHIRCapabilityStatement3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFHIRCapabilityStatement3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
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

procedure TFHIRCapabilityStatement3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
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
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = LCBooleanToString(value)) then
      exit;
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFhirParametersParameter3.addParamCode(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFhirParametersParameter3.addParamStr(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

procedure TFhirParametersParameter3.addParamUri(name, value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFhirParametersParameter3.addParamCanonical(name, value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.partList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCanonical.Create(value);
end;

function TFhirParametersParameter3.getParameterParameter(name: String): TFhirParametersParameterW;
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

function TFhirParametersParameter3.getResource: TFHIRResourceV;
begin
  result := parameter.resource;
end;

function TFhirParametersParameter3.getResourceParameter(name: String): TFHIRResourceV;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter3(t).parameter.resource);
end;

function TFhirParametersParameter3.getStringParameter(name: String): String;
var
  t : TFhirParametersParameterW;
begin
  if FList = nil then
    populateList;
  result := '';
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter3(t).parameter.value.primitiveValue);
end;

function TFhirParametersParameter3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirParametersParameter3.getValue: TFHIRObject;
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

procedure TFhirParametersParameter3.setResource(Value: TFHIRResourceV);
begin
  parameter.resource := value as TFhirResource;
end;

procedure TFhirParametersParameter3.setValue(Value: TFHIRObject);
begin
  parameter.value := value as TFHIRType;
end;

function TFhirParametersParameter3.valueString: String;
begin
  if (parameter.value = nil) or (not parameter.value.isPrimitive) then
    result := ''
  else
    result := parameter.value.primitiveValue;
end;

{ TFHIRParameters3 }

function TFHIRParameters3.addParam(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter3.Create(parameter.parameterList.Append.link);
  TFhirParametersParameter3(result).parameter.name := name;
  ParameterList.Add(result);
end;

procedure TFHIRParameters3.addParam(name: String; value: TFHIRObject);
begin
  parameter.AddParameter(name).value := value as TFHIRType;
end;

procedure TFHIRParameters3.addParamBool(name: String; value: boolean);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = LCBooleanToString(value)) then
      exit;
  parameter.AddParameter(name).value := TFHIRBoolean.Create(value);
end;

procedure TFHIRParameters3.addParamCode(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCode.Create(value);
end;

procedure TFHIRParameters3.addParamUri(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRUri.Create(value);
end;

procedure TFHIRParameters3.addParamCanonical(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRCanonical.Create(value);
end;

procedure TFHIRParameters3.addParamStr(name: String; value: string);
var
  p : TFhirParametersParameter;
begin
  for p in parameter.parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;
  parameter.AddParameter(name).value := TFHIRString.Create(value);
end;

function TFHIRParameters3.bool(name: String): boolean;
begin
  result := parameter.bool[name];
end;

function TFHIRParameters3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRParameters3.getParameter(name: String): TFhirParametersParameterW;
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

function TFHIRParameters3.names: String;
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

function TFHIRParameters3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

procedure TFHIRParameters3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRParameters3.str(name: String): String;
begin
  result := parameter.str[name];
end;

{ TFHIRExpansionProfile3 }

function TFHIRExpansionProfile3.addParam(name: String): TFhirParametersParameterW;
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFHIRExpansionProfile3.addParam(name: String; value: TFHIRObject);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFHIRExpansionProfile3.addParamBool(name: String; value: boolean);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFHIRExpansionProfile3.addParamCode(name: String; value: string);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFHIRExpansionProfile3.addParamStr(name: String; value: string);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFHIRExpansionProfile3.addParamUri(name: String; value: string);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;
                                   
procedure TFHIRExpansionProfile3.addParamCanonical(name: String; value: string);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
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

function TFHIRExpansionProfile3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
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

function TFHIRExpansionProfile3.getParameter(name: String): TFhirParametersParameterW;
begin
  raise EFHIRException.Create('Not supported for Expansion Profile');
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

function TFHIRExpansionProfile3.names: String;
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

procedure TFHIRExpansionProfile3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
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

function TFHIRExpansionProfile3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

{ TFhirExpansionProfileFixedVersion3 }

function TFhirExpansionProfileFixedVersion3.addParam(name: String): TFhirParametersParameterW;
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParam(name: String; value: TFHIRObject);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParamBool(name: String; value: boolean);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParamCode(name: String; value: string);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParamStr(name: String; value: string);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParamUri(name, value: string);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.addParamCanonical(name, value: string);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

function TFhirExpansionProfileFixedVersion3.getParameterParameter(name: String): TFhirParametersParameterW;
begin
  result := nil;
end;

function TFhirExpansionProfileFixedVersion3.getResource: TFHIRResourceV;
begin
  result := nil;
end;

function TFhirExpansionProfileFixedVersion3.getResourceParameter(name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TFhirExpansionProfileFixedVersion3.getStringParameter(name: String): String;
begin
  result := '';
end;

function TFhirExpansionProfileFixedVersion3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirExpansionProfileFixedVersion3.getValue: TFHIRObject;
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

procedure TFhirExpansionProfileFixedVersion3.setResource(Value: TFHIRResourceV);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
end;

procedure TFhirExpansionProfileFixedVersion3.setValue(Value: TFHIRObject);
begin
  raise EFHIRException.Create('Expansion Profile is read only');
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
  result := TFslList<TFHIRElementDefinitionW>.Create;
  try
    for ed in sd.snapshot.elementList do
      result.Add(TFhirElementDefinition3.Create(ed.Link));
    result.link;
  finally
    result.free;
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

function TFHIRStructureDefinition3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
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
    raise EFHIRException.Create('Unhandled value');
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

function TFHIRStructureDefinition3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

procedure TFHIRStructureDefinition3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
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

function TFHIRSearchParamDefinition3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFhirElementDefinition3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFHIRBundleEntry3.getResponseDate: TFslDateTime;
begin
  if entry.response = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.response.lastModified;
end;

function TFHIRBundleEntry3.getResponseETag: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.etag;
end;

function TFHIRBundleEntry3.getResponseLocation: string;
begin
  if entry.response = nil then
    result := ''
  else
    result := entry.response.Location;
end;

function TFHIRBundleEntry3.getResponseStatus: String;
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

function TFHIRBundleEntry3.getURL: String;
begin
  result := entry.fullUrl;
end;

procedure TFHIRBundleEntry3.setRequestMethod(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.Create;
  entry.request.method := TFhirHttpVerbEnum(ord(StringArrayIndexOfSensitive(CODES_TFhirHttpVerbEnum, value)));
end;

procedure TFHIRBundleEntry3.setRequestUrl(Value: String);
begin
  if entry.request = nil then
    entry.request := TFHIRBundleEntryRequest.Create;
  entry.request.url := value;
end;

procedure TFHIRBundleEntry3.setResource(Value: TFHIRResourceV);
begin
  entry.resource := value as TFHIRResource;
end;

procedure TFHIRBundleEntry3.setResponseDate(Value: TFslDateTime);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.lastModified := value;
end;

procedure TFHIRBundleEntry3.setResponseETag(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.ETag := value;
end;

procedure TFHIRBundleEntry3.setResponseLocation(Value: string);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.Location := value;
end;

procedure TFHIRBundleEntry3.setResponseStatus(Value: String);
begin
  if entry.response = nil then
    entry.response := TFHIRBundleEntryResponse.Create;
  entry.response.status := value;
end;

procedure TFHIRBundleEntry3.setSearchMode(Value: TFHIRBundleEntrySearchMode);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.mode := MAP_SEARCH_MODE2[value];
end;

procedure TFHIRBundleEntry3.setSearchMpiMatch(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.setExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match', value)
end;

procedure TFHIRBundleEntry3.setSearchScore(Value: String);
begin
  if entry.search = nil then
    entry.search := TFHIRBundleEntrySearch.Create;
  entry.search.score := value;
end;

procedure TFHIRBundleEntry3.setUrl(Value: String);
begin
  entry.fullUrl := value;
end;

function TFHIRBundleEntry3.getLink(rel: String): String;
begin
  result := entry.Links[rel];
end;

procedure TFHIRBundleEntry3.setLink(rel: String; const Value: String);
begin
  entry.Links[rel] := value;
end;

function TFHIRBundleEntry3.getrequestIfNoneExist: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.ifNoneExist;
end;

procedure TFHIRBundleEntry3.setrequestIfNoneExist(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.ifNoneExist := value;
end;

function TFHIRBundleEntry3.getrequestIfMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfMatch;
end;

procedure TFHIRBundleEntry3.setrequestIfMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.IfMatch := value;
end;

function TFHIRBundleEntry3.getrequestIfNoneMatch: String;
begin
  if entry.request = nil then
    result := ''
  else
    result := entry.request.IfNoneMatch;
end;

procedure TFHIRBundleEntry3.setrequestIfNoneMatch(Value: String);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.IfNoneMatch := value;
end;

function TFHIRBundleEntry3.getrequestIfModifiedSince: TFslDateTime;
begin
  if entry.request = nil then
    result := TFslDateTime.makeNull
  else
    result := entry.request.ifModifiedSince;
end;

procedure TFHIRBundleEntry3.setrequestIfModifiedSince(Value: TFslDateTime);
begin
  if entry.request = nil then
    entry.request := TFhirBundleEntryRequest.Create;
  entry.request.ifModifiedSince := value;
end;

function TFHIRBundleEntry3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFHIRValueSet3.getComposeExtensions: TFslList<TFHIRExtensionW>;
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
          result.add(TFHIRExtension3.Create(ext.link));
      finally
        list.free;
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValueSet3.checkExpansion(place, role: String): boolean;
begin
  result := vs.expansion <> nil;
  if result then
    vs.expansion.checkNoModifiers(place, role, nil);
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

procedure TFHIRValueSet3.clearDefinitionExtensions(exemptUrls: TStringArray);
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

destructor TFHIRValueSet3.Destroy;
begin
  FExp.free;
  inherited;
end;

function TFHIRValueSet3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFHIRValueSet3.excludes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  if vs.compose = nil then
    result := TFslList<TFhirValueSetComposeIncludeW>.create
  else
  begin
    result := TFslList<TFhirValueSetComposeIncludeW>.Create(vs.compose.excludeList.Count);

    for c in vs.compose.excludeList do
      result.Add(TFhirValueSetComposeInclude3.Create(c.Link));
  end;
end;

function TFHIRValueSet3.expansion: TFhirValueSetExpansionW;
begin
  if (FExp = nil) and (vs.expansion <> nil) then
    FExp := TFhirValueSetExpansion3.Create(vs.expansion.Link);
  result := FExp;
end;

function TFHIRValueSet3.forceExpansion: TFhirValueSetExpansionW;
begin
  if (vs.expansion = nil) then
    vs.expansion := TFhirValueSetExpansion.Create;
  vs.expansion.timestamp := TFslDateTime.makeUTC;
  vs.expansion.identifier := NewGuidURN;
  vs.expansion.parameterList.Clear;
  vs.expansion.containsList.Clear;
  result := expansion;
end;

function TFHIRValueSet3.getContext: String;
begin
  result := vs.context;
end;

function TFHIRValueSet3.getDescription: String;
begin
  result := vs.description;
end;

function TFHIRValueSet3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRValueSet3.hasExpansion: boolean;
begin
  result := vs.expansion <> nil;
end;

function TFHIRValueSet3.excludeInactives: boolean;
begin
  result := (vs.compose.inactiveElement <> nil) and not vs.compose.inactive;
end;

function TFHIRValueSet3.imports: TArray<String>;
begin
  SetLength(result, 0);
end;

function TFHIRValueSet3.includes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  if vs.compose = nil then
    result := TFslList<TFhirValueSetComposeIncludeW>.create
  else
  begin
    result := TFslList<TFhirValueSetComposeIncludeW>.Create(vs.compose.includeList.Count);
    for c in vs.compose.includeList do
      result.Add(TFhirValueSetComposeInclude3.Create(c.Link));
  end;
end;

function TFHIRValueSet3.getDate: TFslDateTime;
begin
  result := vs.date;
end;

procedure TFHIRValueSet3.setDate(Value: TFslDateTime);
begin
  vs.date := value;
end;

procedure TFHIRValueSet3.setDescription(value: String);
begin
  vs.description := value;
end;

procedure TFHIRValueSet3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRValueSet3.setName(value: String);
begin
  vs.name := value;
end;

procedure TFHIRValueSet3.setPublisher(Value: String);
begin
  vs.publisher := value;
end;

function TFHIRValueSet3.getTitle: String;
begin
  result := vs.title;
end;

procedure TFHIRValueSet3.setTitle(value: String);
begin
  vs.title := value;
end;

procedure TFHIRValueSet3.setStatus(Value: TPublicationStatus);
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

function TFHIRValueSet3.findContains(systemUri, version, code: String): TFhirValueSetExpansionContainsW;
var
  cc : TFhirValueSetExpansionContains;
begin
  cc := vs.findContains(systemuri, version, code);
  if (cc) = nil then
    result := nil
  else
    result := TFhirValueSetExpansionContains3.Create(cc.link);
end;

function TFHIRValueSet3.getExperimental: boolean;
begin
  result := vs.experimental;
end;

procedure TFHIRValueSet3.setExperimental(value: boolean);
begin
  vs.experimental := value;
end;

function TFHIRValueSet3.getName: String;
begin
  result := vs.name;
end;

function TFHIRValueSet3.getPublisher: String;
begin
  result := vs.publisher;
end;

function TFHIRValueSet3.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[vs.status];
end;

function TFHIRValueSet3.getURL: String;
begin
  result := vs.url;
end;

function TFHIRValueSet3.getVersion: String;
begin
  result := vs.version;
end;

function TFHIRValueSet3.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (vs.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(vs.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
end;

function TFHIRValueSet3.vs: TFhirValueSet;
begin
  result := Resource as TFHIRValueSet;
end;

{ TFhirValueSetComposeInclude3 }

function TFhirValueSetComposeInclude3.addConcept: TFhirValueSetComposeIncludeConceptW;
begin
  result := TFhirValueSetComposeIncludeConcept3.Create((Element as TFhirValueSetComposeInclude).conceptList.Append.link);
end;

function TFhirValueSetComposeInclude3.addFilter: TFhirValueSetComposeIncludeFilterW;
begin
  result := TFhirValueSetComposeIncludeFilter3.Create((Element as TFhirValueSetComposeInclude).filterList.Append.link);
end;

procedure TFhirValueSetComposeInclude3.addValueSet(value: String);
begin
  TFhirValueSetComposeInclude(element).valueSetList.AddItem(value);
end;

function TFhirValueSetComposeInclude3.concepts: TFslList<TFhirValueSetComposeIncludeConceptW>;
var
  i : TFhirValueSetComposeIncludeConcept;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptW>.Create((Element as TFhirValueSetComposeInclude).ConceptList.Count);
  for i in (Element as TFhirValueSetComposeInclude).ConceptList do
    result.Add(TFhirValueSetComposeIncludeConcept3.Create(i.Link));
end;

function TFhirValueSetComposeInclude3.filterCount: integer;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count;
end;

function TFhirValueSetComposeInclude3.conceptCount: integer;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count;
end;

function TFhirValueSetComposeInclude3.filters: TFslList<TFhirValueSetComposeIncludeFilterW>;
var
  i : TFhirValueSetComposeIncludeFilter;
begin
  result := TFslList<TFhirValueSetComposeIncludeFilterW>.Create((Element as TFhirValueSetComposeInclude).filterList.Count);
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

function TFhirValueSetComposeInclude3.hasValueSets: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).valueSetList.Count > 0;
end;

procedure TFhirValueSetComposeInclude3.setSystem(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).system := value;
end;

procedure TFhirValueSetComposeInclude3.setVersion(Value: String);
begin
  (Element as TFhirValueSetComposeInclude).version := value;
end;

function TFhirValueSetComposeInclude3.getSystem: String;
begin
  result := (Element as TFhirValueSetComposeInclude).system;
end;

function TFhirValueSetComposeInclude3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFhirValueSetComposeIncludeFilter3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirValueSetComposeIncludeFilter3.getProp: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).property_;
end;

function TFhirValueSetComposeIncludeFilter3.getValue: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).value;
end;

procedure TFhirValueSetComposeIncludeFilter3.setOp(Value: TFilterOperator);
begin
  (Element as TFhirValueSetComposeIncludeFilter).op := MAP_TFilterOperatorR[value];
end;

procedure TFhirValueSetComposeIncludeFilter3.setProp(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).property_ := value;
end;

procedure TFhirValueSetComposeIncludeFilter3.setValue(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeFilter).value := value;
end;

{ TFhirValueSetComposeIncludeConcept3 }

function TFhirValueSetComposeIncludeConcept3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirValueSetComposeIncludeConcept3.getCode: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).code;
end;

function TFhirValueSetComposeIncludeConcept3.designations: TFslList<TFhirValueSetComposeIncludeConceptDesignationW>;
var
  item : TFhirValueSetComposeIncludeConceptDesignation;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptDesignationW>.Create;
  for item in (Element as TFhirValueSetComposeIncludeConcept).designationList do
    result.Add(TFhirValueSetComposeIncludeConceptDesignation3.Create(item.Link));
end;

function TFhirValueSetComposeIncludeConcept3.getDisplay: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).display;
end;

procedure TFhirValueSetComposeIncludeConcept3.setCode(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).code := Value;
end;

procedure TFhirValueSetComposeIncludeConcept3.setDisplay(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).display := Value;
end;

function TFhirValueSetComposeIncludeConcept3.displayElement: TFHIRPrimitiveW;
begin
  if (Element as TFhirValueSetComposeIncludeConcept).displayELement = nil then
    result := nil
  else
    result := TFHIRPrimitive3.Create((Element as TFhirValueSetComposeIncludeConcept).displayELement.link);
end;

function TFhirValueSetComposeIncludeConcept3.GetItemWeight: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).getExtensionString(EXT_ORDINAL_VALUE);
  if result = '' then
    result := (Element as TFhirValueSetComposeIncludeConcept).getExtensionString(EXT_ITEM_WEIGHT);
end;

procedure TFhirValueSetComposeIncludeConcept3.SetItemWeight(Value: String);
begin
  (Element as TFhirValueSetComposeIncludeConcept).setExtensionString(EXT_ORDINAL_VALUE, value);
end;

{ TFHIRLookupOpResponse3 }

function TFHIRLookupOpResponse3.addDesignation(lang, system, code, display, value: string): TFHIRLookupOpRespDesignationW;
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
    result := TFHIRLookupOpRespDesignation3.Create(p.Link);
    list.add(result);
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse3.addDesignation(lang, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.Create;
  try
    p.language := lang;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation3.Create(p.Link);
    list.add(result);
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
  p := TFHIRLookupOpRespProperty_.Create;
  try
    p.code := name;
    (op as TFHIRLookupOpResponse).property_List.Add(p.link as TFHIRLookupOpRespProperty_);
    result := TFHIRLookupOpRespProperty3.Create(p.Link);
    List.add(result); // make sure it gets cleaned up
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse3.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationResponse).asParams;
end;

function TFHIRLookupOpResponse3.getDisplay: String;
begin
  result := (op as TFHIRLookupOpResponse).display;
end;

function TFHIRLookupOpResponse3.getName: String;
begin
  result := (op as TFHIRLookupOpResponse).name;
end;

function TFHIRLookupOpResponse3.getVersion: String;
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

procedure TFHIRLookupOpResponse3.setDisplay(Value: String);
begin
  (op as TFHIRLookupOpResponse).display := value;
end;

function TFHIRLookupOpResponse3.getIsAbstract: boolean;
begin
  result := (op as TFHIRLookupOpResponse).abstract;
end;

procedure TFHIRLookupOpResponse3.setIsAbstract(Value: boolean);
begin
   (op as TFHIRLookupOpResponse).abstract := value;
end;

procedure TFHIRLookupOpResponse3.setName(Value: String);
begin
  (op as TFHIRLookupOpResponse).name := value;
end;

function TFHIRLookupOpResponse3.getCode: String;
begin
  result := (op as TFHIRLookupOpResponse).code;
end;

procedure TFHIRLookupOpResponse3.setCode(Value: String);
begin
  (op as TFHIRLookupOpResponse).code := value;
end;

function TFHIRLookupOpResponse3.getSystem: String;
begin
  result := (op as TFHIRLookupOpResponse).systemUri;
end;

procedure TFHIRLookupOpResponse3.setSystem(Value: String);
begin
  (op as TFHIRLookupOpResponse).systemUri := value;
end;

procedure TFHIRLookupOpResponse3.setVersion(Value: String);
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

{ TFHIRLookupOpRespSubProperty3 }

function TFHIRLookupOpRespSubProperty3.GetDescription: string;
begin
  result := (obj as TFHIRLookupOpRespSubproperty).description;
end;

function TFHIRLookupOpRespSubProperty3.GetValue: String;
begin
  result := (obj as TFHIRLookupOpRespSubproperty).value;
end;

procedure TFHIRLookupOpRespSubProperty3.SetDescription(Value: string);
begin
  (obj as TFHIRLookupOpRespSubproperty).description := value;
end;

procedure TFHIRLookupOpRespSubProperty3.SetValue(Value: String);
begin
  (obj as TFHIRLookupOpRespSubproperty).value := value;
end;

{ TFHIRLookupOpRespProperty3 }

function TFHIRLookupOpRespProperty3.getDescription: string;
begin
  result := (obj as TFHIRLookupOpRespProperty_).description;
end;

function TFHIRLookupOpRespProperty3.getValue: TFHIRObject;
begin
  result := (obj as TFHIRLookupOpRespProperty_).value;
end;

procedure TFHIRLookupOpRespProperty3.setDescription(Value: string);
begin
  (obj as TFHIRLookupOpRespProperty_).description := value;
end;

procedure TFHIRLookupOpRespProperty3.setValue(Value: TFHIRObject);
begin
  (obj as TFHIRLookupOpRespProperty_).value := value as TFhirType;
end;

function TFHIRLookupOpRespProperty3.addSubProp(name: String): TFHIRLookupOpRespSubPropertyW;
var
  p : TFHIRLookupOpRespSubproperty;
begin
  p := TFHIRLookupOpRespSubproperty.Create;
  try
    p.code := name;
    (obj as TFHIRLookupOpRespProperty_).subpropertyList.Add(p.link as TFHIRLookupOpRespSubproperty);
    result := TFHIRLookupOpRespSubProperty3.Create(p.Link);
    List.add(result); // make sure it gets cleaned up
  finally
    p.free;
  end;
end;

{ TFHIRExtension3 }

function TFHIRExtension3.ext: TFHIRExtension;
begin
  result := (Element as TFHIRExtension);
end;

function TFHIRExtension3.renderText: String;
begin
  result := gen(ext.value);
end;

function TFHIRExtension3.valueAsCodeableConcept: TFhirCodeableConceptW;
begin
  if ext.value is TFHIRCodeableConcept then
    result := TFHIRCodeableConcept3.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension3.valueAsCoding: TFhirCodingW;
begin
  if ext.value is TFHIRCoding then
    result := TFHIRCoding3.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension3.valueAsPeriod: TFhirPeriodW;
begin
  if ext.value is TFHIRPeriod then
    result := TFHIRPeriod3.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension3.valueAsQuantity: TFhirQuantityW;
begin
  if ext.value is TFHIRQuantity then
    result := TFHIRQuantity3.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension3.valueAsIdentifier: TFhirIdentifierW;
begin
  if ext.value is TFHIRIdentifier then
    result := TFHIRIdentifier3.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension3.valueAsAttachment: TFhirAttachmentW;
begin
  if ext.value is TFHIRAttachment then
    result := TFHIRAttachment3.Create(ext.value.link)
  else
    result := nil;
end;

function TFHIRExtension3.valueAsString: string;
begin
  if ext.value is TFHIRPrimitiveType then
    result := ext.value.primitiveValue
  else
    result := '';
end;

function TFHIRExtension3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFHIRExtension3.url: String;
begin
  result := ext.url;
end;

function TFHIRExtension3.value: TFHIRObject;
begin
  result := ext.value;
end;

procedure TFHIRExtension3.setValueW(value: TFhirDataTypeW);
begin
  setValueV(value.Element);
end;

procedure TFHIRExtension3.setValueV(value: TFhirObject);
begin
  if not (value is TFHIRType) then
    raise EFHIRException.Create('Wrong type at TFHIRExtension3.setValueV: '+value.ClassName+' ('+Codes_TFHIRVersion[value.fhirObjectVersion]);
  ext.value := (value as TFHIRType).link;
end;

{ TFHIRCoding3 }

function TFHIRCoding3.getCode: String;
begin
  result := (element as TFHIRCoding).code;
end;

function TFHIRCoding3.getDisplay: String;
begin
  result := (element as TFHIRCoding).display;
end;

function TFHIRCoding3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFHIRCoding3.getSystem: String;
begin
  result := (element as TFHIRCoding).system;
end;

function TFHIRCoding3.getVersion: String;
begin
  result := (element as TFHIRCoding).version;
end;

function TFHIRCoding3.renderText: String;
begin
  result := gen(element as TFhirCoding);
end;

procedure TFHIRCoding3.setCode(Value: String);
begin
  (element as TFHIRCoding).code := value;
end;

procedure TFHIRCoding3.setDisplay(Value: String);
begin
  (element as TFHIRCoding).display := value;
end;

procedure TFHIRCoding3.setSystem(Value: String);
begin
  (element as TFHIRCoding).system := value;
end;

procedure TFHIRCoding3.setVersion(Value: String);
begin
  (element as TFHIRCoding).version := value;
end;

{ TFhirCodeSystemProperty3 }

function TFhirCodeSystemProperty3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirCodeSystemProperty3.code: String;
begin
  result := (Element as TFhirCodeSystemProperty).code;
end;

function TFhirCodeSystemProperty3.type_: TFhirCodeSystemPropertyType;
begin
  result := MAP_TFhirConceptPropertyTypeEnum[(Element as TFhirCodeSystemProperty).type_];
end;

function TFhirCodeSystemProperty3.uri: String;
begin
  result := (Element as TFhirCodeSystemProperty).uri;
end;

{ TFhirCodeSystemConceptProperty3 }

function TFhirCodeSystemConceptProperty3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirCodeSystemConceptProperty3.code: String;
begin
  result := (Element as TFhirCodeSystemConceptProperty).code;
end;

function TFhirCodeSystemConceptProperty3.value: TFHIRObject;
begin
  result := (Element as TFhirCodeSystemConceptProperty).value;
end;

{ TFhirCodeSystemConceptDesignation3 }

function TFhirCodeSystemConceptDesignation3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirCodeSystemConceptDesignation3.language: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).language;
end;

function TFhirCodeSystemConceptDesignation3.use: TFHIRCodingW;
begin
  if (Element as TFhirCodeSystemConceptDesignation).use = nil then
    result := nil
  else
    result := TFHIRCoding3.Create((Element as TFhirCodeSystemConceptDesignation).use.link);
end;

function TFhirCodeSystemConceptDesignation3.hasUse: boolean;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).use <> nil;
end;

function TFhirCodeSystemConceptDesignation3.useGen: String;
begin
  result := gen((Element as TFhirCodeSystemConceptDesignation).use)
end;

function TFhirCodeSystemConceptDesignation3.value: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).value;
end;

function TFhirCodeSystemConceptDesignation3.valueElement: TFHIRPrimitiveW;
begin
  if (Element as TFhirCodeSystemConceptDesignation).valueElement = nil then
    result := nil
  else
    result := TFHIRPrimitive3.Create((Element as TFhirCodeSystemConceptDesignation).valueElement.link);
end;

{ TFhirCodeSystemConcept3 }

function TFhirCodeSystemConcept3.c: TFhirCodeSystemConcept;
begin
  result := Element as TFhirCodeSystemConcept;
end;

function TFhirCodeSystemConcept3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirCodeSystemConcept3.code: String;
begin
  result := c.code;
end;

function TFhirCodeSystemConcept3.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept3.Create((element as TFhirCodeSystemConcept).conceptList[ndx].Link);
end;

function TFhirCodeSystemConcept3.conceptCount: integer;
begin
  result := c.conceptList.Count;
end;

function TFhirCodeSystemConcept3.hasConcepts: boolean;
begin
  result := c.hasConceptList;
end;

function TFhirCodeSystemConcept3.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.Create;
    for i in (element as TFhirCodeSystemConcept).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept3.Create(i.Link));
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
  result := TFslList<TFhirCodeSystemConceptDesignationW>.Create;
  for i in c.designationList do
    result.Add(TFhirCodeSystemConceptDesignation3.Create(i.Link));
end;

function TFhirCodeSystemConcept3.display: String;
begin
  result := c.display;
end;

function TFhirCodeSystemConcept3.displayElement: TFHIRPrimitiveW;
begin
  if c.displayElement = nil then
    result := nil
  else
    result := TFHIRPrimitive3.Create(c.displayElement.link);
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
  result := TFslList<TFhirCodeSystemConceptPropertyW>.Create;
  for i in c.property_List do
    result.Add(TFhirCodeSystemConceptProperty3.Create(i.Link));
end;

procedure TFhirCodeSystemConcept3.setDisplayTag(tag, value: String);
begin
  c.displayElement.Tags[tag] := value;
end;                  

function TFhirCodeSystemConcept3.itemWeight : String;
begin
  result := (Element as TFhirCodeSystemConcept).getExtensionString(EXT_ORDINAL_VALUE);
  if result = '' then
    result := (Element as TFhirCodeSystemConcept).getExtensionString(EXT_ITEM_WEIGHT);
end;

{ TFhirCodeSystem3 }

function TFhirCodeSystem3.buildImplicitValueSet: TFHIRValueSetW;
begin
  result := TFHIRValueSet3.Create(cs.buildImplicitValueSet);
end;

function TFhirCodeSystem3.hasLanguage(cc : TFhirCodeSystemConcept; langs: THTTPLanguageList): boolean;
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

function TFhirCodeSystem3.hasAnyDisplays(langs: THTTPLanguageList): boolean;
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

function TFhirCodeSystem3.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept3.Create(cs.conceptList[ndx].Link);
end;

function TFhirCodeSystem3.conceptCount: integer;
begin
  result := cs.conceptList.Count;
end;

function TFhirCodeSystem3.hasConcepts: boolean;
begin
  result := cs.hasConceptList;
end;

function TFhirCodeSystem3.conceptList: TFhirCodeSystemConceptListW;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFhirCodeSystemConceptListW.Create;
    for i in (resource as TFhirCodeSystem).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept3.Create(i.Link));
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

function TFhirCodeSystem3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirCodeSystem3.getCount: integer;
begin
  result := StrToInt(cs.count);
end;

function TFhirCodeSystem3.getDate: TFslDateTime;
begin
  result := cs.date;
end;

function TFhirCodeSystem3.getDescription: String;
begin
  result := cs.description;
end;

function TFhirCodeSystem3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirCodeSystem3.getChildren(c: TFhirCodeSystemConceptW): TFhirCodeSystemConceptListW;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getChildren(c.element as TFhirCodeSystemConcept);
  try
    result := TFhirCodeSystemConceptListW.Create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept3.Create(i.Link));
      result.link;
    finally
      result.free;
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
    result := TFhirCodeSystemConceptListW.Create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept3.Create(i.Link));
      result.link;
    finally
      result.free;
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

function TFhirCodeSystem3.isInactive(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isInactive(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem3.codeStatus(c: TFhirCodeSystemConceptW): String;
begin
  result := cs.codeStatus(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem3.isAbstract(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isAbstract(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem3.isDeprecated(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isDeprecated(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem3.language: String;
begin
  result := cs.language;
end;

function TFhirCodeSystem3.getName: String;
begin
  result := cs.name;
end;

function TFhirCodeSystem3.properties: TFslList<TFhirCodeSystemPropertyW>;
var
  i : TFhirCodeSystemProperty;
begin
  result := TFslList<TFhirCodeSystemPropertyW>.Create;
  for i in cs.property_List do
    result.Add(TFhirCodeSystemProperty3.Create(i.Link));
end;

procedure TFhirCodeSystem3.setContent(Value: TFhirCodeSystemContentMode);
begin
  cs.content := MAP_TFhirCodeSystemContentMode[Value];
end;

procedure TFhirCodeSystem3.setCount(Value: integer);
begin
  cs.count := inttostr(Value);
end;

procedure TFhirCodeSystem3.setDate(Value: TFslDateTime);
begin
  cs.date := Value;
end;

procedure TFhirCodeSystem3.setDescription(Value: String);
begin
  cs.description := Value;
end;

procedure TFhirCodeSystem3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirCodeSystem3.setName(Value: String);
begin
  cs.name := Value;
end;

procedure TFhirCodeSystem3.setPublisher(Value: String);
begin
  cs.publisher := value;
end;

function TFhirCodeSystem3.getTitle: String;
begin
  result := cs.title;
end;

procedure TFhirCodeSystem3.setTitle(value: String);
begin
  cs.title := value;
end;

function TFhirCodeSystem3.getExperimental: boolean;
begin
  result := cs.experimental;
end;

procedure TFhirCodeSystem3.setExperimental(value: boolean);
begin
  cs.experimental := value;
end;

procedure TFhirCodeSystem3.setStatus(Value: TPublicationStatus);
begin
  cs.status := MAP_TPublicationStatus[Value];
end;

procedure TFhirCodeSystem3.setUrl(Value: String);
begin
  cs.url := Value;
end;

procedure TFhirCodeSystem3.setVersion(Value: String);
begin
  cs.version := Value;
end;

function TFhirCodeSystem3.GetCaseSensitive: boolean;
begin
  result := cs.caseSensitive;
end;

function TFhirCodeSystem3.supplements: String;
begin
  result := '';
end;

function TFhirCodeSystem3.valueSet: String;
begin
  result := cs.valueSet;
end;

function TFhirCodeSystem3.getPublisher: String;
begin
  result := cs.publisher;
end;

function TFhirCodeSystem3.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[cs.status];
end;

function TFhirCodeSystem3.getURL: String;
begin
  result := cs.url;
end;

function TFhirCodeSystem3.getVersion: String;
begin
  result := cs.version;
end;

function TFhirCodeSystem3.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (cs.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(cs.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
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

procedure TFhirValueSetExpansion3.addParamBool(name: String; value: boolean);
begin
  exp.addParamBool(name, value);
end;

procedure TFhirValueSetExpansion3.addParamStr(name, value: String);
begin
  exp.addParamStr(name, value);
end;

procedure TFhirValueSetExpansion3.addParamCode(name, value: String);
begin
  exp.addParamCode(name, value);
end;

procedure TFhirValueSetExpansion3.addParamUri(name, value: String);
begin
  exp.addParamUri(name, value);
end;

procedure TFhirValueSetExpansion3.addParamInt(name: String; value: integer);
begin
  exp.AddParamInt(name, value);
end;

function TFhirValueSetExpansion3.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.Create;
  for item in (Element as TFhirValueSetExpansion).containsList do
    result.Add(TFhirValueSetExpansionContains3.Create(item.Link));
end;

function TFhirValueSetExpansion3.getTotal: integer;
begin
  result := StrToIntDef(exp.total, 0);
end;

procedure TFhirValueSetExpansion3.setTotal(value: integer);
begin
  exp.total := inttostr(value);
end;

function TFhirValueSetExpansion3.getOffset: integer;
begin
  result := StrToIntDef(exp.offset, 0);
end;

procedure TFhirValueSetExpansion3.setOffset(value: integer);
begin
  exp.offset := inttostr(value);
end;

procedure TFhirValueSetExpansion3.defineProperty(focus: TFhirValueSetExpansionContainsW; url, code: String; value: TFHIRObject);
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
    for ext in ((focus as TFhirValueSetExpansionContains3).element as TFhirValueSetExpansionContains).extensionList do
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
      ((focus as TFhirValueSetExpansionContains3).element as TFhirValueSetExpansionContains).extensionList.add(pd);
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

procedure TFhirValueSetExpansion3.copyParams(source: TFhirValueSetExpansionW);
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

function TFhirValueSetExpansion3.exp: TFhirValueSetExpansion;
begin
  result := element as TFhirValueSetExpansion;
end;

function TFhirValueSetExpansion3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFhirValueSetExpansionContains3.getCode: String;
begin
  result := (Element as TFhirValueSetExpansionContains).code;
end;

procedure TFhirValueSetExpansionContains3.addDesignation(lang, use, value: String);
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

procedure TFhirValueSetExpansionContains3.addDesignation(lang : TIETFLang; use: TFHIRCodingW; value: TFHIRPrimitiveW; extensions : TFslList<TFHIRExtensionW>);
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

procedure TFhirValueSetExpansionContains3.addProperty(code: String; value: TFHIRObject);
var
  p : TFhirExtension;
begin
  p := (Element as TFhirValueSetExpansionContains).extensionList.append;
  p.url := 'http://hl7.org/fhir/5.0/StructureDefinition/extension-ValueSet.expansion.contains.property';
  p.addExtension('code', code);
  p.addExtension('value', value.link as TFhirType);
end;

procedure TFhirValueSetExpansionContains3.addProperty(code: String; prop: TFhirCodeSystemConceptPropertyW);
var
  p : TFhirExtension;
begin
  p := (Element as TFhirValueSetExpansionContains).extensionList.append;
  p.url := 'http://hl7.org/fhir/5.0/StructureDefinition/extension-ValueSet.expansion.contains.property';
  p.addExtension('code', code);
  p.addExtension('value', prop.value.link as TFhirType);
  p.copyExtensions(prop.element, []);
end;

procedure TFhirValueSetExpansionContains3.addContains(contained: TFhirValueSetExpansionContainsW);
begin
  (Element as TFhirValueSetExpansionContains).containsList.add(contained.Element.link);
end;

procedure TFhirValueSetExpansionContains3.clearContains;
begin
  (Element as TFhirValueSetExpansionContains).containsList.clear;
end;

function TFhirValueSetExpansionContains3.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.Create;
  for item in (Element as TFhirValueSetExpansionContains).containsList do
    result.Add(TFhirValueSetExpansionContains3.Create(item.Link));
end;

function TFhirValueSetExpansionContains3.getVersion: String;
begin
  result := (Element as TFhirValueSetExpansionContains).version;
end;

procedure TFhirValueSetExpansionContains3.setVersion(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).version := value
end;


function TFhirValueSetExpansionContains3.getDisplay: String;
begin
  result := (Element as TFhirValueSetExpansionContains).display;
end;

function TFhirValueSetExpansionContains3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirValueSetExpansionContains3.getSystem: String;
begin
  result := (Element as TFhirValueSetExpansionContains).system;
end;

function TFhirValueSetExpansionContains3.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.Create;
end;

procedure TFhirValueSetExpansionContains3.setCode(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).code := value;
end;

procedure TFhirValueSetExpansionContains3.setDisplay(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).display := value;
end;

procedure TFhirValueSetExpansionContains3.setSystem(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).system := value;
end;        

function TFhirValueSetExpansionContains3.GetItemWeight: String;
begin
  result := (Element as TFhirValueSetExpansionContains).getExtensionString(EXT_ORDINAL_VALUE);
  if result = '' then
    result := (Element as TFhirValueSetExpansionContains).getExtensionString(EXT_ITEM_WEIGHT);
end;

procedure TFhirValueSetExpansionContains3.SetItemWeight(Value: String);
begin
  (Element as TFhirValueSetExpansionContains).setExtensionString(EXT_ORDINAL_VALUE, value);
end;

function TFhirValueSetExpansionContains3.GetAbstract: boolean;
begin
  result := (Element as TFhirValueSetExpansionContains).abstract;
end;

function TFhirValueSetExpansionContains3.GetInactive: boolean;
begin
  result := (Element as TFhirValueSetExpansionContains).inactive;
end;

procedure TFhirValueSetExpansionContains3.SetAbstract(Value: boolean);
begin
  (Element as TFhirValueSetExpansionContains).abstract := value;
end;

procedure TFhirValueSetExpansionContains3.SetInactive(Value: boolean);
begin
  (Element as TFhirValueSetExpansionContains).inactive := value;
end;

{ TFhirValueSetComposeIncludeConceptDesignation3 }

function TFhirValueSetComposeIncludeConceptDesignation3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirValueSetComposeIncludeConceptDesignation3.language: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).language;
end;

function TFhirValueSetComposeIncludeConceptDesignation3.value: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).value;
end;

function TFhirValueSetComposeIncludeConceptDesignation3.use: TFHIRCodingW;
begin
  if (element as TFhirValueSetComposeIncludeConceptDesignation).use = nil then
    result := nil
  else
    result := TFHIRCoding3.Create((element as TFhirValueSetComposeIncludeConceptDesignation).use.link);
end;

function TFhirValueSetComposeIncludeConceptDesignation3.valueElement: TFHIRPrimitiveW;
begin
  if (element as TFhirValueSetComposeIncludeConceptDesignation).valueElement = nil then
    result := nil
  else
    result := TFHIRPrimitive3.Create((element as TFhirValueSetComposeIncludeConceptDesignation).valueElement.link);
end;

{ TFhirConceptMap3 }

function TFhirConceptMap3.addGroup(source, target: String): TFhirConceptMapGroupW;
var
  g : TFhirConceptMapGroup;
begin
  g := cm.groupList.Append;
  g.source := source;
  g.target := target;
  result := TFhirConceptMapGroup3.Create(g.Link);
end;

function TFhirConceptMap3.cm: TFhirConceptMap;
begin
  result := Resource as TFhirConceptMap;
end;

function TFhirConceptMap3.getVersion: String;
begin
  result := cm.version;
end;

function TFhirConceptMap3.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (cm.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(cm.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
end;

function TFhirConceptMap3.groups: TFslList<TFhirConceptMapGroupW>;
var
  g : TFhirConceptMapGroup;
begin
  result := TFslList<TFhirConceptMapGroupW>.Create;
  for g in cm.groupList do
    result.Add(TFhirConceptMapGroup3.Create(g.Link))
end;

procedure TFhirConceptMap3.setVersion(Value: String);
begin
  cm.version := value;
end;

function TFhirConceptMap3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFhirConceptMap3.getExperimental: boolean;
begin
  result := cm.experimental;
end;

procedure TFhirConceptMap3.setExperimental(value: boolean);
begin
  cm.experimental := value;
end;

function TFhirConceptMap3.getURL: String;
begin
  result := cm.url;
end;

function TFhirConceptMap3.getDate: TFslDateTime;
begin
  result := cm.Date;
end;

function TFhirConceptMap3.getDescription: String;
begin
  result := cm.Description;
end;

function TFhirConceptMap3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirConceptMap3.getName: String;
begin
  result := cm.Name;
end;

function TFhirConceptMap3.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[cm.Status];
end;

procedure TFhirConceptMap3.setDate(Value: TFslDateTime);
begin
  cm.Date := value;
end;

procedure TFhirConceptMap3.setDescription(Value: String);
begin
  cm.Description := value;
end;

procedure TFhirConceptMap3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirConceptMap3.setName(Value: String);
begin
  cm.Name := value;
end;

procedure TFhirConceptMap3.setStatus(Value: TPublicationStatus);
begin
  cm.Status := MAP_TPublicationStatus[value];
end;

procedure TFhirConceptMap3.setUrl(Value: String);
begin
  cm.Url := value;
end;

procedure TFhirConceptMap3.setPublisher(Value: String);
begin
  cm.publisher := value;
end;

function TFhirConceptMap3.getTitle: String;
begin
  result := cm.title;
end;

procedure TFhirConceptMap3.setTitle(value: String);
begin
  cm.title := value;
end;

function TFhirConceptMap3.getContext: String;
begin
  result := cm.context;
end;

function TFhirConceptMap3.getPublisher: String;
begin
  result := cm.publisher;
end;

{ TFhirConceptMapGroupElementTarget3 }

function TFhirConceptMapGroupElementTarget3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

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
  result := TFslList<TFhirConceptMapGroupElementDependsOnW>.Create;
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

function TFhirConceptMapGroupElement3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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
  result := TFslList<TFhirConceptMapGroupElementTargetW>.Create;
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

function TFhirConceptMapGroup3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirConceptMapGroup3.elements: TFslList<TFhirConceptMapGroupElementW>;
var
  t : TFhirConceptMapGroupElement;
begin
  result := TFslList<TFhirConceptMapGroupElementW>.Create;
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

destructor TFHIRMeta3.Destroy;
begin
  FResource.free;
  inherited;
end;

function TFHIRMeta3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

procedure TFHIRMeta3.force;
begin
  if Element = nil then
  begin
    FElement := TFHIRMeta.Create;
    Resource.meta := m.Link;
  end;
end;

function TFHIRMeta3.getLastUpdated: TFslDateTime;
begin
  if Element = nil then
    result := TFslDateTime.makeNull
  else
    result := m.lastUpdated;
end;

function TFHIRMeta3.getVersionId: String;
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
  result := TFslList<TFHIRCodingW>.Create;
  if Element <> nil then
    for i in m.securityList do
      result.Add(TFHIRCoding3.Create(i.Link));
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

procedure TFHIRMeta3.setLastUpdated(Value: TFslDateTime);
begin
  force;
  m.lastUpdated := value;
end;

procedure TFHIRMeta3.setResource(value: TFHIRResource);
begin
  FResource.free;
  FResource := value;
end;

procedure TFHIRMeta3.setVersionId(Value: String);
begin
  force;
  m.versionId := value;
end;

function TFHIRMeta3.tags: TFslList<TFHIRCodingW>;
var
  i : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if Element <> nil then
    for i in m.tagList do
      result.Add(TFHIRCoding3.Create(i.Link));
end;

function TFHIRMeta3.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FResource.sizeInBytes(magic));
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

function TFhirBinary3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirBinary3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFhirBinary3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFHIRAuditEvent3 }

function TFHIRAuditEvent3.ae: TFHIRAuditEvent;
begin
  result := Resource as TFhirAuditEvent;
end;

function TFHIRAuditEvent3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFHIRAuditEvent3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
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

procedure TFHIRAuditEvent3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
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

function TFhirCapabilityStatementRestResource3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirCapabilityStatementRestResource3.getCode: String;
begin
  result := CODES_TFhirResourceTypesEnum[(Element as TFhirCapabilityStatementRestResource).type_];
end;

procedure TFhirCapabilityStatementRestResource3.setCode(Value: String);
begin
  (Element as TFhirCapabilityStatementRestResource).type_Element := TFhirEnum.Create('http://hl7.org/fhir/resource-types', value);
end;

function TFhirCapabilityStatementRestResource3.getProfile: String;
begin
  if (Element as TFhirCapabilityStatementRestResource).profile <> nil then
    result := (Element as TFhirCapabilityStatementRestResource).profile.reference;
end;

procedure TFhirCapabilityStatementRestResource3.setProfile(Value: String);
begin
  if value = '' then
    (Element as TFhirCapabilityStatementRestResource).profile := nil
  else
    (Element as TFhirCapabilityStatementRestResource).profile := TFhirReference.Create(value);
end;

procedure TFhirCapabilityStatementRestResource3.addInteraction(codeV, doco: String);
begin
  With (Element as TFhirCapabilityStatementRestResource).interactionList.Append do
  begin
    codeElement := TFhirEnum.Create('http://hl7.org/fhir/ValueSet/type-restful-interaction', codeV);
    documentation := doco;
  end;
end;

procedure TFhirCapabilityStatementRestResource3.addOperation(codeV, defn, doco: String);
begin
end;

function TFhirCapabilityStatementRestResource3.getReadHistory: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).readHistory;
end;

function TFhirCapabilityStatementRestResource3.hasInteraction: boolean;
begin
  result := (Element as TFhirCapabilityStatementRestResource).interactionList.Count > 0;
end;

procedure TFhirCapabilityStatementRestResource3.setReadHistory(Value: boolean);
begin
  (Element as TFhirCapabilityStatementRestResource).readHistory := Value;
end;

{ TFHIRSubscription3 }

function TFHIRSubscription3.getCriteria: String;
begin
  result := sub.criteria;
end;

function TFHIRSubscription3.getDirect: boolean;
begin
  result := sub.channel.endpointElement.hasExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct')
    and (sub.channel.endpointElement.getExtensionString('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct') = 'true');
end;

function TFHIRSubscription3.getEndpoint: String;
begin
  result := sub.channel.endpoint;
end;

function TFHIRSubscription3.getError: String;
begin
  result := sub.error;
end;

function TFHIRSubscription3.getHeaders: TArray<String>;
var
  i : integer;
begin
  setLength(result, sub.channel.headerList.Count);
  for i := 0 to sub.channel.headerList.Count - 1 do
    result[i] := sub.channel.headerList[i].value;
end;

function TFHIRSubscription3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRSubscription3.getMethod: TSubscriptionMethod;
begin
  result := MAP_TSubscriptionMethod[sub.channel.type_];
end;

function TFHIRSubscription3.getPayload: String;
begin
  result := sub.channel.payload;
end;

function TFHIRSubscription3.getStatus: TSubscriptionStatus;
begin
  result := MAP_TSubscriptionStatus[sub.status];
end;

function TFHIRSubscription3.getSummary: String;
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

procedure TFHIRSubscription3.setCriteria(Value: String);
begin
  sub.criteria := value;
end;

procedure TFHIRSubscription3.setDirect(Value: boolean);
begin
  if value then
    sub.channel.endpointElement.setExtensionBoolean('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct', 'true')
  else
    sub.channel.endpointElement.removeExtension('http://hl7.org/fhir/us/core/StructureDefinition/us-core-direct');
end;

procedure TFHIRSubscription3.setEndpoint(Value: String);
begin
  sub.channel.endpoint := value;
end;

procedure TFHIRSubscription3.setError(Value: String);
begin
  sub.error := value;
end;

procedure TFHIRSubscription3.setheaders(Value: TArray<String>);
var
  s : String;
begin
  sub.channel.headerList.Clear;
  for s in value do
    sub.channel.headerList.Append.value := s;
end;

procedure TFHIRSubscription3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRSubscription3.setMethod(Value: TSubscriptionMethod);
begin
  sub.channel.type_ := MAP_TSubscriptionMethod2[value];
end;

procedure TFHIRSubscription3.setPayload(Value: String);
begin
  sub.channel.payload := value;
end;

procedure TFHIRSubscription3.setStatus(Value: TSubscriptionStatus);
begin
  sub.status := MAP_TSubscriptionStatus2[value];
end;

function TFHIRSubscription3.sub: TFhirSubscription;
begin
  result := resource as TFhirSubscription;
end;

function TFHIRSubscription3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

{ TFhirObservationComponent3 }

function TFhirObservationComponent3.codings: TFslList<TFHIRCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFHIRCodingW>.Create;
  if comp.code <> nil then
    for c in comp.code.codingList do
      result.Add(TFHIRCoding3.Create(c.Link));
end;

function TFhirObservationComponent3.comp: TFhirObservationComponent;
begin
  result := (Element as TFhirObservationComponent);
end;

function TFhirObservationComponent3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirObservationComponent3.dataAbsentReason: TFhirCodeableConceptW;
begin
  if comp.dataAbsentReason = nil then
    result := nil
  else
    result := TFhirCodeableConcept3.Create(comp.dataAbsentReason.link);
end;

function TFhirObservationComponent3.getValue: TFHIRObject;
begin
  result := comp.value;
end;

procedure TFhirObservationComponent3.setValue(Value: TFHIRObject);
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

function TFhirObservation3.getStatus: TObservationStatus;
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
      obs.code := TFhirCodeableConcept.Create;
    obs.code.text := Value
  end
  else if (obs.code <> nil) and not obs.code.hasCoding then
    obs.code := nil;
end;

procedure TFhirObservation3.SetComment(const Value: String);
begin
  obs.comment := value;
end;

function TFhirObservation3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

procedure TFhirObservation3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirObservation3.setPeriod(start, finish: TDateTime);
begin
  obs.effective := TFhirPeriod.Create;
  TFhirPeriod(obs.effective).start := TFslDateTime.makeUTC(start);
  TFhirPeriod(obs.effective).end_ := TFslDateTime.makeUTC(finish);
end;

procedure TFhirObservation3.setStatus(Value: TObservationStatus);
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
  result := TFslList<TFHIRCodingW>.Create;
  if obs.code <> nil then
    for c in obs.code.codingList do
      result.Add(TFHIRCoding3.Create(c.Link));
end;

function TFhirObservation3.components: TFslList<TFhirObservationComponentW>;
var
  c : TFhirObservationComponent;
begin
  result := TFslList<TFhirObservationComponentW>.Create;
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
    comp := TFHIRObservationComponent3.Create(c.link);
end;

function TFhirObservation3.getComponent(system: String; var comp: TFhirObservationComponentW): boolean;
var
  c : TFHIRObservationComponent;
begin
  comp := nil;
  result := obs.getComponent(system, c);
  if result then
    comp := TFHIRObservationComponent3.Create(c.link);
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
    result := TFHIRPeriod3.Create(obs.effective.Link)
  else
    result := nil;
end;

function TFhirObservation3.GetIssued: TFslDateTime;
begin
  result := obs.issued;
end;

function TFhirObservation3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirObservation3.getValue: TFHIRObject;
begin
  result := obs.value;
end;

procedure TFhirObservation3.setValue(Value: TFHIRObject);
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
  result := TFslList<TFHIRCodingW>.Create;
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

function TFHIRQuantity3.getCode: String;
begin
  result := qty.code;
end;

function TFHIRQuantity3.getSystem: String;
begin
  result := qty.system;
end;

function TFHIRQuantity3.getUnit: String;
begin
  result := qty.unit_;
end;

function TFHIRQuantity3.getValue: String;
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

procedure TFHIRQuantity3.setCode(Value: String);
begin
  qty.code := Value;
end;

procedure TFHIRQuantity3.setSystem(Value: String);
begin
  qty.system := Value;
end;

procedure TFHIRQuantity3.setUnit(Value: String);
begin
  qty.unit_ := Value;
end;

procedure TFHIRQuantity3.setValue(Value: String);
begin
  qty.value := Value;
end;

function TFHIRQuantity3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

{ TFHIRLookupOpRequest3 }

function TFHIRLookupOpRequest3.asParams: TFHIRResourceV;
begin
  result := (op as TFHIROperationRequest).asParams;
end;

function TFHIRLookupOpRequest3.coding: TFHIRCodingW;
begin
  if (op as TFHIRLookupOpRequest).coding = nil then
    result := nil
  else
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
    raise EFSLException.Create('Unable to find a code to lookup (need coding or system/code)');
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
  result := (op as TFHIRSubsumesOpRequest).version;
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
  result := TFHIRCoding3.Create((Element as TFhirCodeableConcept).codingList.Append.link);
end;

procedure TFhirCodeableConcept3.removeCoding(systemUri, version, code: String);
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

function TFhirCodeableConcept3.codingCount: integer;
begin
  result := (Element as TFhirCodeableConcept).codingList.Count;
end;

function TFhirCodeableConcept3.codings: TFslList<TFhirCodingW>;
var
  c : TFHIRCoding;
begin
  result := TFslList<TFhirCodingW>.Create;
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

function TFhirCodeableConcept3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirCodeableConcept3.fromSystem(System: String; required: boolean): String;
begin
  result := (Element as TFhirCodeableConcept).fromSystem(System, required);
end;

function TFhirCodeableConcept3.summary: String;
begin
  result := summarise(Element as TFhirCodeableConcept);
end;

function TFhirCodeableConcept3.hasCode(systemUri, code: String): boolean;
begin
  result := (Element as TFhirCodeableConcept).hasCode(systemUri, code);
end;

function TFhirCodeableConcept3.hasCode(systemUri, version, code: String): boolean;
begin
  result := (Element as TFhirCodeableConcept).hasCode(systemUri, version, code);
end;

procedure TFhirCodeableConcept3.clearCodings;
begin
  (Element as TFhirCodeableConcept).codingList.Clear;
end;

procedure TFhirCodeableConcept3.addCoding(systemUri, version, code,
  display: String);
begin
  (Element as TFhirCodeableConcept).addCoding(systemUri, version, code, display);
end;

{ TFHIRGroup3 }

function TFHIRGroup3.characteristics: TFslList<TFHIRGroupCharacteristicW>;
var
  gc : TFHIRGroupCharacteristic;
begin
  result := TFslList<TFHIRGroupCharacteristicW>.Create;
  for gc in (Resource as TFHIRGroup).characteristicList do
    result.add(TFHIRGroupCharacteristic3.Create(gc.link));
end;

function TFHIRGroup3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFHIRGroup3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
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

procedure TFHIRGroup3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFHIRGroupCharacteristic3 }

function TFHIRGroupCharacteristic3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

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

function TFHIRNamingSystem3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
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

function TFHIRNamingSystem3.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (nm.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(nm.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
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

procedure TFHIRNamingSystem3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRNamingSystem3.setName(Value: String);
begin
  nm.name := value;
end;

procedure TFHIRNamingSystem3.setPublisher(Value: String);
begin
  nm.publisher := value;
end;

function TFHIRNamingSystem3.getTitle: String;
begin
  result := nm.title;
end;

procedure TFHIRNamingSystem3.setTitle(value: String);
begin
  nm.title := value;
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

function TFHIRNamingSystem3.getExperimental: boolean;
begin
  result := nm.experimental;
end;

procedure TFHIRNamingSystem3.setExperimental(value: boolean);
begin
  nm.experimental := value;
end;

function TFHIRNamingSystem3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

{ TFHIRStructureMap3 }

function TFHIRStructureMap3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFHIRStructureMap3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

procedure TFHIRStructureMap3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

function TFHIRStructureMap3.url: String;
begin
  result := (Resource as TFHIRStructureMap).url;
end;

{ TFhirPatient3 }

function TFhirPatient3.pat: TFHIRPatient;
begin
  result := resource as TFhirPatient;
end;

function TFhirPatient3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirPatient3.nameSummary: String;
begin
  result := HumanNamesAsText(pat.nameList);
end;

function TFhirPatient3.activeStr: String;
begin
  if pat.activeElement = nil then
    result := ''
  else if pat.active then
    result := 'true'
  else
    result := 'false';
end;

function TFhirPatient3.GetActive: boolean;
begin
  result := pat.active;
end;

procedure TFhirPatient3.SetActive(const Value: boolean);
begin
  pat.active := value;
end;


function TFhirPatient3.gender: String;
begin
  result := CODES_TFhirAdministrativeGenderEnum[pat.gender];
end;

function TFhirPatient3.genderPlus: String;
begin
  result := gender;
end;

function TFhirPatient3.GetFamily: String;
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

procedure TFhirPatient3.SetFamily(const Value: String);
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

function TFhirPatient3.GetDob: String;
begin
  result := pat.birthDate.toXML;
end;

procedure TFhirPatient3.SetDob(const Value: String);
begin
  pat.birthDate := TFslDateTime.fromXML(value);
end;

function TFhirPatient3.GetIdentifier(systemUri: String): String;
var
  id : TFhirIdentifier;
begin
  result := '';
  for id in pat.identifierList do
    if id.system = systemUri then
      exit(id.value);
end;

procedure TFhirPatient3.SetIdentifier(systemUri: String; const Value: String);
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

function TFhirPatient3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirPatient3.identifierSummary: String;
begin
  result := IdentifiersAsText(pat.identifierList);
end;

procedure TFhirPatient3.addGiven(name: String);
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

function TFhirPatient3.contactSummary: String;
begin
  result := ContactsAsText(pat.telecomList);
end;

procedure TFhirPatient3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
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

function TFhirTerminologyCapabilities3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
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
  raise EFslException.Create('Not done yet');
end;

procedure TFhirTerminologyCapabilities3.setDate(Value: TFslDateTime);
begin
  (FRes as TFhirParameters).AddParameter('date', TFhirDateTime.Create(Value));
end;

procedure TFhirTerminologyCapabilities3.setDescription(Value: String);
begin
end;

procedure TFhirTerminologyCapabilities3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFhirTerminologyCapabilities3.setName(Value: String);
begin
end;

procedure TFhirTerminologyCapabilities3.setPublisher(Value: String);
begin
end;

function TFhirTerminologyCapabilities3.getTitle: String;
begin
  result := '';
end;

procedure TFhirTerminologyCapabilities3.setTitle(value: String);
begin
  // nothing
end;

procedure TFhirTerminologyCapabilities3.setStatus(Value: TPublicationStatus);
begin
end;

procedure TFhirTerminologyCapabilities3.setUrl(Value: String);
begin
  (FRes as TFhirParameters).AddParameter('url', TFhirUri.Create(Value));
end;

procedure TFhirTerminologyCapabilities3.setVersion(Value: String);
begin
  (FRes as TFhirParameters).AddParameter('version', TFhirCode.Create(Value));
end;

function TFhirTerminologyCapabilities3.getKind: TCapabilityStatementKind;
begin
  result := cskNull;
end;

procedure TFhirTerminologyCapabilities3.setKind(Value: TCapabilityStatementKind);
begin
  // nothing
end;

function TFhirTerminologyCapabilities3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

procedure TFhirTerminologyCapabilities3.systemUri(url: String);
begin
  (FRes as TFhirParameters).AddParameter('system', TFhirUri.Create(url));
end;

procedure TFhirTerminologyCapabilities3.systemVersion(url: String);
var
  u, v : String;
begin
  StringSplit(url, '|', u, v);
  (FRes as TFhirParameters).AddParameter('system', TFhirUri.Create(u));
  (FRes as TFhirParameters).AddParameter('version', TFhirCode.Create(v))
end;

procedure TFhirTerminologyCapabilities3.addExpansionParameter(code, doco : String);
begin
  (FRes as TFhirParameters).AddParameter('expansion.parameter', TFhirCode.Create(code));
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

function TFHIRPeriod3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFHIRConsent3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFHIRConsent3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
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

procedure TFHIRConsent3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirEncounter3 }

function TFhirEncounter3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirEncounter3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirEncounter3.patientId: String;
begin
  result := (FRes as TFHIREncounter).subject.getId;
end;

procedure TFhirEncounter3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
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

function TFHIRTestScript3.getExperimental: boolean;
begin
  result := ts.experimental;
end;

procedure TFHIRTestScript3.setExperimental(value: boolean);
begin
  ts.experimental := value;
end;

function TFHIRTestScript3.getDate: TFslDateTime;
begin
  result := ts.date;
end;

function TFHIRTestScript3.getDescription: String;
begin
  result := ts.description;
end;

function TFHIRTestScript3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFHIRTestScript3.getName: String;
begin
  result := ts.name;
end;

function TFHIRTestScript3.getPublisher: String;
begin
  result := ts.publisher;
end;

function TFHIRTestScript3.getStatus: TPublicationStatus;
begin
  result := MAP_TPublicationStatusR[ts.Status];
end;

function TFHIRTestScript3.getURL: String;
begin
  result := ts.url;
end;

function TFHIRTestScript3.getVersion: String;
begin
  result := ts.version;
end;

function TFHIRTestScript3.getVersionAlgorithm: TFHIRVersionAlgorithm;
begin
  if (ts.hasExtension(EXT_VERSION_ALGORITHM)) then
    result := interpretVersionAlgorithm(ts.getExtensionValue(EXT_VERSION_ALGORITHM))
  else
    result := vaUnknown;
end;

procedure TFHIRTestScript3.setDate(Value: TFslDateTime);
begin
  ts.date := value;
end;

procedure TFHIRTestScript3.setDescription(Value: String);
begin
  ts.description := value;
end;

procedure TFHIRTestScript3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

procedure TFHIRTestScript3.setName(Value: String);
begin
  ts.name := value;
end;

procedure TFHIRTestScript3.setPublisher(Value: String);
begin
  ts.publisher := value;
end;

function TFHIRTestScript3.getTitle: String;
begin
  result := ts.title;
end;

procedure TFHIRTestScript3.setTitle(value: String);
begin
  ts.title := value;
end;

procedure TFHIRTestScript3.setStatus(Value: TPublicationStatus);
begin
  ts.Status := MAP_TPublicationStatus[Value];
end;

procedure TFHIRTestScript3.setUrl(Value: String);
begin
  ts.url := value;
end;

procedure TFHIRTestScript3.setVersion(Value: String);
begin
  ts.version := value;
end;

function TFHIRTestScript3.ts: TFHIRTestScript;
begin
  result := (Fres as TFhirTestScript);
end;

function TFHIRTestScript3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

function TFhirProvenance3.GetLanguage: String;
begin
  result := (resource as TFHIRResource).language;
end;

function TFhirProvenance3.p: TFhirProvenance;
begin
  result := (Fres as TFhirProvenance);
end;

function TFhirProvenance3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

procedure TFhirProvenance3.SetLanguage(const Value: String);
begin
  (resource as TFHIRResource).language := value;
end;

{ TFhirConceptMapGroupElementDependsOn3 }

function TFhirConceptMapGroupElementDependsOn3.display: String;
begin
  result := (Element as TFhirConceptMapGroupElementTargetDependsOn).display;
end;

function TFhirConceptMapGroupElementDependsOn3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
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

{ TFHIRAttachment3 }

function TFHIRAttachment3.att: TFhirAttachment;
begin
  result := Element as TFhirAttachment;
end;

function TFHIRAttachment3.GetContentType: String;
begin
  result := att.contentType;
end;

function TFHIRAttachment3.GetData: TBytes;
begin
  result := att.data;
end;

function TFHIRAttachment3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFHIRAttachment3.renderText: String;
begin
  result := '??';
end;

{ TFhirImmunization3 }

function TFhirImmunization3.code(systemUri: String): String;
var
  c : TFHIRCoding;
begin
  result := '';
  for c in imm.vaccineCode.codingList do
    if (c.system = systemUri) then
      exit(c.code);
end;

function TFhirImmunization3.GetDate: TFslDateTime;
begin
  result := imm.date;
end;

function TFhirImmunization3.GetLanguage: String;
begin
  result := imm.language;
end;

function TFhirImmunization3.GetLotNumber: String;
begin
  result := imm.lotNumber;
end;

function TFhirImmunization3.GetManufacturerIdSystem: String;
begin
  if (imm.manufacturer <> nil) and (imm.manufacturer.identifier <> nil) then
    result := imm.manufacturer.identifier.system
  else
    result := '';
end;

function TFhirImmunization3.GetManufacturerIdValue: String;
begin
  if (imm.manufacturer <> nil) and (imm.manufacturer.identifier <> nil) then
    result := imm.manufacturer.identifier.value
  else
    result := '';
end;

function TFhirImmunization3.GetPatient: String;
begin
  if imm.patient = nil then
    result := ''
  else
    result := imm.patient.reference;
end;

function TFhirImmunization3.GetPerformerDisplay: String;
begin
  result := '';
  if (imm.practitionerList.Count > 0) and (imm.practitionerList[0].actor <> nil) then
    result := imm.practitionerList[0].actor.display;
end;

function TFhirImmunization3.GetStatus: string;
begin
  result := imm.statusElement.value;
end;

function TFhirImmunization3.hasCode(systemUri, code: String): boolean;
var
  c : TFHIRCoding;
begin
  result := false;
  for c in imm.vaccineCode.codingList do
    if (c.system = systemUri) and (c.code = code) then
      exit(true);
end;

function TFhirImmunization3.imm: TFhirImmunization;
begin
  result := resource as TFhirImmunization;
end;

procedure TFhirImmunization3.setCodeBySystem(systemUri: String; code: String);
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

procedure TFhirImmunization3.SetDate(const Value: TFslDateTime);
begin
  imm.date := value;
end;

function TFhirImmunization3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

procedure TFhirImmunization3.SetLanguage(const Value: String);
begin
  imm.language := value;
end;

procedure TFhirImmunization3.SetLotNumber(const Value: String);
begin
  imm.lotNumber := value;
end;

procedure TFhirImmunization3.SetManufacturerIdSystem(const Value: String);
begin
  if imm.manufacturer = nil then
    imm.manufacturer := TFhirReference.Create;
  if imm.manufacturer.identifier = nil then
    imm.manufacturer.identifier := TFhirIdentifier.Create;
  imm.manufacturer.identifier.system := value;
end;

procedure TFhirImmunization3.SetManufacturerIdValue(const Value: String);
begin
  if imm.manufacturer = nil then
    imm.manufacturer := TFhirReference.Create;
  if imm.manufacturer.identifier = nil then
    imm.manufacturer.identifier := TFhirIdentifier.Create;
  imm.manufacturer.identifier.value := value;
end;

procedure TFhirImmunization3.SetPatient(const Value: String);
begin
  if (imm.patient = nil) then
    imm.patient := TFhirReference.Create;
  imm.patient.reference := value;
end;

procedure TFhirImmunization3.SetPerformerDisplay(const Value: String);
var
  p : TFhirImmunizationPractitioner;
begin
  if imm.practitionerList.Count > 0 then
    p := imm.practitionerList[0]
  else
    p := imm.practitionerList.Append;
  if p.actor = nil then
    p.actor := TFhirReference.Create;
  p.actor.display := value;
end;

procedure TFhirImmunization3.SetStatus(const Value: string);
begin
  imm.statusElement.value := value;
end;

{ TFhirIdentifier3 }

function TFhirIdentifier3.id: TFHIRIdentifier;
begin
  result := FElement as TFHIRIdentifier;
end;

function TFhirIdentifier3.wrapExtension(extension: TFHIRObject): TFHIRExtensionW;
begin
  result := TFHIRExtension3.Create(extension.link);
end;

function TFhirIdentifier3.renderText: String;
begin
  result := gen(element as TFhirIdentifier);
end;

function TFhirIdentifier3.GetSystem: String;
begin
  result := id.system;
end;

function TFhirIdentifier3.GetValue: String;
begin
  result := id.value;
end;

procedure TFhirIdentifier3.SetSystem(const Value: String);
begin
  id.system := value;
end;

procedure TFhirIdentifier3.SetValue(const Value: String);
begin
  id.value := value;
end;

const
  identifier_use_version_to_general : array [TFhirIdentifierUseEnum] of TIdentifierUse = (iuNull, iuUsual, iuOfficial, iuTemp, iuSecondary);
  identifier_use_general_to_version : array [TIdentifierUse] of TFhirIdentifierUseEnum = (IdentifierUseNull, IdentifierUseUsual, IdentifierUseOfficial, IdentifierUseTemp, IdentifierUseSecondary, IdentifierUseTemp);

function TFhirIdentifier3.GetUse: TIdentifierUse;
begin
  result := identifier_use_version_to_general[id.use];
end;

procedure TFhirIdentifier3.SetUse(const Value: TIdentifierUse);
begin
  id.use := identifier_use_general_to_version[value];
end;

function TFhirIdentifier3.GetTypeV: TFhirCodeableConceptW;
begin
  if id.type_ = nil then
    result := nil
  else
    result := TFhirCodeableConcept3.Create(id.type_.Link);
end;

procedure TFhirIdentifier3.SetTypeV(const Value: TFhirCodeableConceptW);
begin
  if value = nil then
    id.type_ := nil
  else
    id.type_ := ((Value as TFhirCodeableConcept3).FElement as TFhirCodeableConcept).link;
end;


end.

