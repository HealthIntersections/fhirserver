unit fhir4b_utilities;

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
{$I fhir4b.inc}

interface

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Generics.Collections, ZLib,

  fsl_base, fsl_utilities, fsl_http, fsl_stream, fsl_json, fsl_turtle, fsl_xml, fsl_crypto, fsl_html,
  fsl_fetcher, fsl_web_stream,

  fhir_parser, fhir_objects, fhir_xhtml, fhir_utilities, fhir_uris,
  fhir4b_context, fhir4b_enums, fhir4b_types, fhir4b_resources, fhir4b_constants, fhir4b_resources_base;

const

  MIN_DATE = DATETIME_MIN;
  MAX_DATE = DATETIME_MAX;
  CANONICAL_URL_RESOURCE_TYPES = [
    {$IFDEF FHIR_ACTIVITYDEFINITION}frtActivityDefinition, {$ENDIF}
    {$IFDEF FHIR_CAPABILITYSTATEMENT}frtCapabilityStatement, {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM}frtCodeSystem, {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION}frtCompartmentDefinition, {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP}frtConceptMap, {$ENDIF}
    {$IFDEF FHIR_EVENTDEFINITION}frtEventDefinition, {$ENDIF}
    {$IFDEF FHIR_GRAPHDEFINITION}frtGraphDefinition, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE}frtImplementationGuide, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDEINPUT}frtImplementationGuideInput, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDEOUTPUT}frtImplementationGuideOutput, {$ENDIF}
    {$IFDEF FHIR_LIBRARY}frtLibrary, {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION}frtMessageDefinition, {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM}frtNamingSystem, {$ENDIF}
    {$IFDEF FHIR_OBSERVATIONDEFINITION}frtObservationDefinition, {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION}frtOperationDefinition, {$ENDIF}
    {$IFDEF FHIR_PLANDEFINITION}frtPlanDefinition, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE}frtQuestionnaire, {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER}frtSearchParameter, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION}frtStructureDefinition, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREMAP}frtStructureMap, {$ENDIF}
    {$IFDEF FHIR_TERMINOLOGYCAPABILITIES}frtTerminologyCapabilities, {$ENDIF}
    frtValueSet];
  TERMINOLOGY_RESOURCES : Array of String = ['CodeSystem', 'ValueSet', 'ConceptMap'];

function HumanNamesAsText(names : TFhirHumanNameList):String;
function HumanNameAsText(name : TFhirHumanName):String;
function GetEmailAddress(contacts : TFhirContactPointList):String;
function ResourceTypeByName(name : String) : TFhirResourceType;
function isResourceName(name : String; canbeLower : boolean = false) : boolean;
function IdentifiersAsText(ids : TFhirIdentifierList):String;
function ContactsAsText(cps : TFhirContactPointList):String;

Function RecogniseFHIRResourceName(Const sName : String; out aType : TFhirResourceType): boolean;
Function RecogniseFHIRResourceManagerName(Const sName : String; out aType : TFhirResourceType): boolean;
function MakeParser(oWorker : TFHIRWorkerContext; langList : THTTPLanguageList; aFormat: TFHIRFormat; oContent: TStream; policy : TFHIRXhtmlParserPolicy): TFHIRParser; overload;
function MakeParser(oWorker : TFHIRWorkerContext; langList : THTTPLanguageList; aFormat: TFHIRFormat; content: TBytes; policy : TFHIRXhtmlParserPolicy): TFHIRParser; overload;
function MakeParser(oWorker : TFHIRWorkerContext; langList : THTTPLanguageList; mimetype : String; content: TBytes; policy : TFHIRXhtmlParserPolicy): TFHIRParser; overload;
function MakeComposer(style : TFHIROutputStyle; langList : THTTPLanguageList; mimetype : String; worker : TFHIRWorkerContext) : TFHIRComposer;
function geTFhirResourceNarrativeAsText(resource : TFhirDomainResource) : String;
function fullResourceUri(base: String; aType : TFhirResourceType; id : String) : String; overload;
function fullResourceUri(base: String; url : String) : String; overload;
function fullResourceUri(base: String; ref : TFhirReference) : String; overload;

procedure listReferences(resource : TFhirResource; list : TFhirReferenceList);
procedure listAttachments(resource : TFhirResource; list : TFhirAttachmentList);
function FindContainedResource(resource : TFhirDomainResource; ref : TFhirReference) : TFhirResource; overload;
function FindContainedResource(resource : TFhirDomainResource; ref : string) : TFhirResource; overload;
function LoadFromFormParam(worker : TFHIRWorkerContext; part : TMimePart; langList : THTTPLanguageList) : TFhirResource;
function LoadDTFromFormParam(worker : TFHIRWorkerContext; part : TMimePart; langList : THTTPLanguageList; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType;
function LoadDTFromParam(worker : TFHIRWorkerContext; value : String; langList : THTTPLanguageList; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType;

function BuildOperationOutcome(langList : THTTPLanguageList; e : exception; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;
Function BuildOperationOutcome(langList : THTTPLanguageList; message : String; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;
function BuildOperationOutcome4(langList : THTTPLanguageList; e : exception; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;
Function BuildOperationOutcome4(langList : THTTPLanguageList; message : String; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;

function getChildMap(profile : TFHIRStructureDefinition; name, path, nameReference : String) : TFHIRElementDefinitionList; overload;
function getChildMap(profile : TFHIRStructureDefinition; element : TFHIRElementDefinition) : TFHIRElementDefinitionList; overload;
function CreateResourceByName(name : String) : TFhirResource;
function CreateTypeByName(name : String) : TFhirElement;
function CreateBasicChildren(element : TFhirElement; exCoding : TFHIRCoding) : TFhirElement;

function asUTCMin(value : TFhirInstant) : TDateTime; overload;
function asUTCMax(value : TFhirInstant) : TDateTime; overload;
function asUTCMin(value : TFhirDate) : TDateTime; overload;
function asUTCMax(value : TFhirDate) : TDateTime; overload;
function asUTCMin(value : TFhirDateTime) : TDateTime; overload;
function asUTCMax(value : TFhirDateTime) : TDateTime; overload;
function asUTCMin(value : TFhirPeriod) : TDateTime; overload;
function asUTCMax(value : TFhirPeriod) : TDateTime; overload;
function asUTCMin(value : TFhirTiming) : TDateTime; overload;
function asUTCMax(value : TFhirTiming) : TDateTime; overload;

function asCode(obj : TFHIRObject) : TFHIRCode;
function asString(obj : TFHIRObject) : TFHIRString;
function asId(obj : TFHIRObject) : TFHIRId;
function asUri(obj : TFHIRObject) : TFHIRUri;
function asDateTime(obj : TFHIRObject) : TFHIRDateTime;
function asUnsignedInt(obj : TFHIRObject) : TFHIRUnsignedInt;
function asPositiveInt(obj : TFHIRObject) : TFHIRPositiveInt;
function asInstant(obj : TFHIRObject) : TFHIRInstant;
function asBoolean(obj : TFHIRObject) : TFHIRBoolean;
function asBase64Binary(obj : TFHIRObject) : TFHIRBase64Binary;
function asDate(obj : TFHIRObject) : TFHIRDate;
function asDecimal(obj : TFHIRObject) : TFHIRDecimal;
function asTime(obj : TFHIRObject) : TFHIRTime;
function asOid(obj : TFHIRObject) : TFHIROid;
function asInteger(obj : TFHIRObject) : TFHIRInteger;
function asResource(obj : TFHIRObject) : TFHIRResource;
function asExtension(obj : TFHIRObject) : TFHIRExtension;
function asMarkdown(obj : TFHIRObject) : TFHIRMarkdown;
function asXhtml(obj : TFHIRObject) : TFhirXHtmlNode;
function asXhtmlNode(obj : TFHIRObject) : TFhirXHtmlNode;

function asEnum(systems, values: array of String; obj : TFHIRObject) : TFHIREnum;

function HasExtension(element : TFhirElement; url : string):Boolean;
function GetExtension(element : TFhirElement; url : string) : TFhirExtension;

procedure BuildNarrative(op: TFhirOperationOutcome; opDesc : String); overload;
procedure BuildNarrative(vs : TFhirValueSet); overload;
procedure BuildNarrative(cs : TFhirCodeSystem); overload;
function ComposeJson(worker: TFHIRWorkerContext; r : TFhirResource) : String; overload;

function getConformanceResourceUrl(res : TFHIRResource) : string;

function fileToResource(name : String) : TFhirResource; overload;
function fileToResource(name : String; var format : TFHIRFormat) : TFhirResource; overload;
function streamToResource(stream : TStream; var format : TFHIRFormat) : TFhirResource;
function bytesToResource(bytes : TBytes) : TFhirResource; overload;
function bytesToResource(bytes : TBytes; var format : TFHIRFormat) : TFhirResource; overload;
function stringToResource(source : String) : TFhirResource; overload;
function stringToResource(source : String; var format : TFHIRFormat) : TFhirResource; overload;
procedure resourceToFile(res : TFhirResource; name : String; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal);
procedure resourceToStream(res : TFhirResource; stream : TStream; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal);
function resourceToString(res : TFhirResource; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal) : String;
function resourceToBytes(res : TFhirResource; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal) : TBytes;

function parseParamsFromForm(stream : TStream) : TFHIRParameters;
function makeMarkdownOrString : TFhirMarkdown;

const
  frtProcedureRequest = frtServiceRequest;

type
  TFhirProcedureRequest = TFhirServiceRequest;
  TFhirCapabilityStatementRestOperation = TFhirCapabilityStatementRestResourceOperation;

  TFHIRProfileStructureHolder = TFhirStructureDefinitionSnapshot;
  TFHIRProfileStructureElement = TFhirElementDefinition;
  TFhirProfileStructureElementList = TFhirElementDefinitionList;
  TFhirProfileStructureElementDefinitionBinding = TFhirElementDefinitionBinding;
  TFhirAuditEventParticipant = TFhirAuditEventAgent;
  TFhirAuditEventObject = TFhirAuditEventEntity;
  TFhirAuditEventEvent = TFhirAuditEvent;
  TFhirAuditEventParticipantNetwork  = TFhirAuditEventAgentNetwork;
  TFhirNamingSystemContact = TFHIRContactDetail;
  TFhirConformanceContact = TFHIRContactDetail;
  TFhirQuestionnaireItemOption = TFhirQuestionnaireItemAnswerOption;

  TFhirPeriodHelper = class helper for TFhirPeriod
  private
    function GetEditString: String;
    procedure SetEditString(const Value: String);
  public
    class function fromDateTimes(start, end_ : TDateTime) : TFhirPeriod; overload;
    class function fromDates(start, end_ : TDateTime) : TFhirPeriod; overload;
    class function fromEdit(s : String) : TFhirPeriod;
    property editString : String read GetEditString write SetEditString;
    function point : TDateTime;
  end;

  TFhirBinaryHelper = class helper for TFhirBinary
  private
    function GetContent: TBytes;
    procedure SetContent(const Value: TBytes);
  public
    property content : TBytes read GetContent write SetContent;
  end;

  TFhirIdentifierHelper = class helper for TFhirIdentifier
  private
    function GetEditString: String;
    procedure SetEditString(const Value: String);
  public
    constructor Create(system, value : String); overload;
    constructor Create(value : String); overload;
    class function fromEdit(s : String) : TFhirIdentifier;
    property editString : String read GetEditString write SetEditString;
    function isType(code : String) : boolean; overload;
    function isType(system, code : String) : boolean; overload;
  end;

  TFhirIdentifierListHelper = class helper for TFhirIdentifierList
  public
    function BySystem(uri : String) : TFhirIdentifier;
    function withCommas : String;
  end;

  TFhirPrimitiveTypeHelper = class helper for TFhirPrimitiveType
  public
    constructor Create(value : String); overload;
  end;

  TFhirAuditEventHelper = class helper for TFhirAuditEvent
  private
    function getevent: TFhirAuditEvent;
    procedure SetEvent(const Value: TFhirAuditEvent);
    function GetdateTime: TFslDateTime;
    procedure SetDateTime(const Value: TFslDateTime);
    function getParticipantList: TFhirAuditEventAgentList;
    function GetObjectList: TFhirAuditEventEntityList;
  public
    property event : TFhirAuditEvent read GetEvent write SetEvent;
    property dateTime : TFslDateTime read GetdateTime write SetDateTime;
    property participantList : TFhirAuditEventAgentList read getParticipantList;
    property object_List : TFhirAuditEventEntityList read GetObjectList;
  end;

  TFHIRDurationHelper = class helper for TFhirDuration
  private
  public
    function ToDateTime : TDateTime;
  end;

  TFhirExtensionListHelper = class helper for TFhirExtensionList
  public
    procedure addExtension(url : String; value : String); overload;
  end;

  TFHIRExpressionHelper = class helper for TFHIRExpression
  public
    function display : String;
  end;

  TFHIRElementHelper = class helper for TFHIRElement
  public
    function addExtension(ext : TFHIRExtension) : TFHIRExtension; overload;
    function forceExtension(url : String) : TFHIRExtension; overload;
    function addExtension(url : String) : TFHIRExtension; overload;
    procedure addExtension(url : String; t : TFhirDataType); overload;
    procedure addExtension(url : String; v : String); overload;
    procedure addExtensionUri(url : String; v : String); overload;
    function hasExtension(url : String) : boolean;
    function getExtension(url : String) : Integer;
    function getExtensionByUrl(url : String) : TFHIRExtension;
    function getExtensionCount(url : String) : Integer;
    function getExtensionString(url : String) : String; overload;
    function getExtensionDateAsString(url : String) : String; overload;
    function getExtensionBoolean(url : String) : boolean;
    function getExtensionString(url : String; index : integer) : String; overload;
    function listExtensions(url : String) : TFslList<TFhirExtension>; overload;
    procedure removeExtension(url : String);
    procedure setExtension(url : String; t : TFHIRDataType);
    procedure setExtensionString(url, value : String);
    procedure setExtensionMarkdown(url, value : String);
    procedure setExtensionCanonical(url, value : String);
    procedure setExtensionInteger(url, value : String);
    procedure setExtensionDecimal(url, value : String);
    procedure setExtensionURI(url, value : String);
    procedure setExtensionDate(url, value : String);
    procedure setExtensionDateTime(url, value : String);
    procedure setExtensionTime(url, value : String);
    procedure setExtensionCode(url, value : String);
    procedure setExtensionBoolean(url, value : String); overload;
    procedure setExtensionBoolean(url : String; value : boolean); overload;
  end;

  TFhirCanonicalHelper = class helper for TFhirCanonical
  private
    function GetReference: String;
    procedure SetReference(const sValue: String);
  public
    property reference : String read GetReference write SetReference;
  end;

  TFHIRBackboneElementHelper = class helper for TFHIRBackboneElement
  public
    procedure checkNoModifiers(place, role : String); overload;
    procedure checkNoModifiers(place, role : String; exempt : TArray<String>); overload;
  end;

  TFhirElementDefinitionHelper = class helper for TFhirElementDefinition
  public
    function hasType(t : String; out profile : String) : boolean;  overload;
    function hasType(t : String) : boolean; overload;
    function getType(t : String) : TFhirElementDefinitionType; overload;
    function minInt : Integer;
    function maxInt : Integer;
  end;

  TFhirElementDefinitionTypeHelper = class helper for TFhirElementDefinitionType
  private
    function GetProfile: String;
  public
    property profile : String read GetProfile;
    function hasTargetProfile(uri : String) : boolean;
    function targetProfileAsCSV : String;
  end;

  TFhirConceptMapGroupElementTargetDependsOnHelper = class helper for TFhirConceptMapGroupElementTargetDependsOn
  private
    function GetCode: String;
    procedure SetCode(const sValue: String);
  public
    property code : String read GetCode write SetCode;
  end;

  TFhirRangeHelper = class helper for TFhirRange
  public
    function either : TFhirQuantity;
  end;

  TFhirQuantityHelper = class helper for TFhirQuantity
  private
    function GetEditString: String;
    procedure SetEditString(vs: String);
  public
    class function fromUcum(value, code : String) : TFhirQuantity;
    function asDuration : TDateTime;
    class function fromDuration(v : TDateTime) : TFhirQuantity;
    class function fromPair(v : Double; units : String) : TFhirQuantity;
    class function fromEdit(s : String) : TFhirQuantity;
    property editString : String read GetEditString write SetEditString;
  end;

  TFHIRResourceHelper = class helper for TFHIRResource
  private
    function GetXmlId: String;
    procedure SetmlId(const Value: String);
  public
    property xmlId : String read GetXmlId write SetmlId;

    procedure checkNoImplicitRules(place, role : String);

    function textSummary : String;
  end;

  TFHIRDomainResourceHelper = class helper (TFHIRResourceHelper) for TFHIRDomainResource
  private
    function GetContained(id: String): TFhirResource;
  public
    property Contained[id : String] : TFhirResource read GetContained; default;
    procedure collapseAllContained;
    function addExtension(url : String; t : TFhirDataType) : TFhirExtension; overload;
    function addExtension(url : String) : TFhirExtension; overload;
    function addExtension(url : String; v : String) : TFhirExtension; overload;
    function hasExtension(url : String) : boolean;
    function getExtension(url : String) : Integer; overload;
    function getExtension(url : String; index : integer) : TFhirExtension; overload;
    function getExtensionValue(url : String) : TFHIRDataType;
    function listExtensions(url : String) : TFslList<TFhirExtension>; overload;
    function forceExtension(url : String) : TFHIRExtension;
    function getExtensionCount(url : String) : Integer;
    function getExtensionString(url : String) : String; overload;
    function getExtensionBool(url : String) : boolean; overload;
    function getExtensionString(url : String; index : integer) : String; overload;
    function getExtensionByUrl(url : String) : TFHIRExtension;
    procedure removeExtension(url : String); overload;
    procedure removeExtension(url : String; index : integer); overload;
    procedure setExtensionString(url, value : String);
    procedure setExtensionBool(url : String; value : boolean);
    procedure setExtensionCode(url, value : String);
    procedure setExtension(url : String; t : TFHIRDataType);
    procedure checkNoModifiers(place, role : String; allowed : TArray<String> = nil);
  end;

  TFhirCodeListHelper = class helper for TFhirCodeList
  public
    function hasCode(code : String) : boolean;
  end;

  TFhirUriListHelper = class helper for TFhirUriList
  public
    function hasUri(uri : String) : boolean;
    procedure removeUri(uri : String);
  end;

  TFhirCanonicalListHelper = class helper for TFhirCanonicalList
  public
    function hasUri(uri : String) : boolean;
    procedure removeUri(uri : String);
  end;

  TFhirProfileStructureSnapshotElementDefinitionTypeListHelper = class helper for TFhirElementDefinitionList
  public
    function summary : String;
  end;

  TFHIRCapabilityStatementHelper = class helper (TFHIRDomainResourceHelper) for TFHIRCapabilityStatement
  private
    function hasFormat(fmt: TFHIRFormat): boolean;
    procedure SetHasFormat(fmt: TFHIRFormat; const Value: boolean);
    function GetInstantiates(url: String): boolean;
    procedure SetInstantiates(url: String; const Value: boolean);
  public
    function rest(type_ : TFhirResourceType) : TFhirCapabilityStatementRestResource;
    procedure checkCompatible;

    property Format[fmt : TFHIRFormat] : boolean read HasFormat write SetHasFormat;
    property instantiates[url : String] : boolean read GetInstantiates write SetInstantiates;
    function supportsResource(name : String; commands : TFHIRCommandTypeSet) : boolean;
    function supportsOperation(rName, opName : string) : boolean;
  end;

  { TFHIRCodeableConceptHelper }

  TFHIRCodeableConceptHelper = class helper (TFHIRElementHelper) for TFHIRCodeableConcept
  public
    constructor Create(system, code : String); overload;
    function hasCode(System, Code : String) : boolean; overload;
    function hasCode(System, Version, Code : String) : boolean; overload;
    function hasCoding : boolean;
    function fromSystem(System : String; required : boolean = false) : String; overload;
    function fromSystem(Systems : TArray<String>; required : boolean = false) : String; overload;
    procedure addCoding(systemUri, version, code, display : String);
  end;

  TFHIRCodeableConceptListHelper = class helper for TFHIRCodeableConceptList
  private
    function GetHasCode(System, Code: String): boolean;
    procedure SetHasCode(System, Code: String; const Value: boolean);
  public
    property hasCode[System, Code : String] : boolean read GetHasCode write SetHasCode;
  end;

  TFhirConformanceRestResourceHelper = class helper (TFHIRElementHelper) for TFhirCapabilityStatementRestResource
  public
    function interaction(type_ : TFHIRTypeRestfulInteractionEnum) : TFhirCapabilityStatementRestResourceInteraction;
    procedure removeInteraction(type_ : TFHIRTypeRestfulInteractionEnum);
  end;

  TFhirConformanceRestHelper = class helper (TFHIRElementHelper) for TFhirCapabilityStatementRest
  public
    function interaction(type_ : TFhirSystemRestfulInteractionEnum) : TFhirCapabilityStatementRestInteraction;
  end;

  TFHIRContactPointListHelper = class helper for TFhirContactPointList
  public
    function system(type_ : TFHIRContactPointSystemEnum) : String;
    procedure setSystem(type_ : TFHIRContactPointSystemEnum; value : String);
  end;

  TFhirContactDetailListHelper = class helper for TFhirContactDetailList
  public
    function system(type_ : TFHIRContactPointSystemEnum) : String;
    procedure setSystem(type_ : TFHIRContactPointSystemEnum; value : String);
  end;

  TFhirTestScriptSetupActionOperationRequestHeaderListHelper = class helper for TFhirTestScriptSetupActionOperationRequestHeaderList
  public
    procedure add(name, value : String); overload;
  end;

  TFhirCodingHelper = class helper for TFhirCoding
  private
    function GetEditString: String;
    procedure SetEditString(const Value: String);
  public
    constructor Create(system, code : String); overload;
    constructor Create(system, code, display : String); overload;

    function hasCode(System, Code : String) : boolean;
    class function fromEdit(s : String) : TFhirCoding;
    property editString : String read GetEditString write SetEditString;
  end;

  TFhirHumanNameHelper = class helper for TFhirHumanName
  private
  public
    function given : string;
    class function fromEdit(n: String): TFhirHumanName; static;
  end;

  TFhirValueSetHelper = class helper for TFhirValueSet
  public
    function context : string;
    function source : string;
    function findContains(systemUri, version, code : String) : TFHIRValueSetExpansionContains;
  end;

  TFhirTerminologyCapabilitiesHelper = class helper for TFhirTerminologyCapabilities
  public
    function context : string;
  end;

  { TFhirValueSetExpansionHelper }

  TFhirValueSetExpansionHelper = class helper for TFhirValueSetExpansion
  public
    procedure AddParamStr(name, value : String); overload;
    procedure AddParamUri(name, value : String); overload;
    procedure AddParamCanonical(name, value : String); overload;
    procedure AddParamBool(name : String; value : boolean); overload;
    procedure addParamCode(name, value : String); overload;
    procedure addParamInt(name : String; value : integer); overload;
  end;

  TFHIROperationOutcomeHelper = class helper (TFHIRDomainResourceHelper) for TFhirOperationOutcome
  public
    function rule(level : TFhirIssueSeverityEnum; source : String; typeCode : TFhirIssueTypeEnum; path : string; test : boolean; msg : string) : boolean;
    function error(source : String; typeCode : TFhirIssueTypeEnum; path : string; test : boolean; msg : string) : boolean;
    function warning(source : String; typeCode : TFhirIssueTypeEnum; path : string; test : boolean; msg : string) : boolean;
    function hint(source : String; typeCode : TFhirIssueTypeEnum; path : string; test : boolean; msg : string) : boolean;

    function hasErrors : boolean;
    function asExceptionMessage : String;
  end;

  TFhirHealthcareServiceHelper = class helper (TFHIRResourceHelper) for TFhirHealthcareService
  public
    function category : TFhirCodeableConcept;
    function type_ : TFhirCodeableConcept;
    function summary : String;
  end;

  TFhirLocationHelper = class helper (TFHIRResourceHelper) for TFhirLocation
  public
    function type_ : TFhirCodeableConcept;
    function summary : String;
  end;

  TFHIRCompositionHelper = class helper (TFHIRResourceHelper) for TFHIRComposition
  public
    function summary : String;
  end;

  TFHIRCompositionSectionHelper = class helper (TFHIRElementHelper) for TFHIRCompositionSection
  public
    function display : String;
  end;

  TFhirConceptMapHelper = class helper (TFhirResourceHelper) for TFhirConceptMap
  public
//    function conceptList : TFhirConceptMapElementList;
    function context : string;
    function sourceDesc: String;
    function targetDesc: String;
  end;

  TSignatureType = (SignatureTypeAuthor, SignatureTypeCoAuthor, SignatureTypeParticipant, SignatureTypeTranscriptionist, SignatureTypeVerification,
                    SignatureTypeValidation, SignatureTypeConsent, SignatureTypeWitnessSignature, SignatureTypeWitnessEvent, SignatureTypeWitnessIdentity,
                    SignatureTypeWitnessConsent, SignatureTypeInterpreter, SignatureTypeReview, SignatureTypeSource, SignatureTypeAddendum,
                    SignatureTypeModification, SignatureTypeAdministrative, SignatureTypeTimestamp);

const
  FHIR_ENUM_VERSIONS : Array [TFHIRVersion] of TFhirFHIRVersionEnum = (FHIRVersionNull, FHIRVersion0082, FHIRVersion102, FHIRVersion301, FHIRVersion400, FHIRVersion430Snapshot1, FHIRVersion430Cibuild);
  CODES_TSignatureType : array [TSignatureType] of String = ('1.2.840.10065.1.12.1.1', '1.2.840.10065.1.12.1.2', '1.2.840.10065.1.12.1.3', '1.2.840.10065.1.12.1.4', '1.2.840.10065.1.12.1.5',
                '1.2.840.10065.1.12.1.6', '1.2.840.10065.1.12.1.7', '1.2.840.10065.1.12.1.8', '1.2.840.10065.1.12.1.9', '1.2.840.10065.1.12.1.10',
                '1.2.840.10065.1.12.1.11', '1.2.840.10065.1.12.1.12', '1.2.840.10065.1.12.1.13', '1.2.840.10065.1.12.1.14', '1.2.840.10065.1.12.1.15',
                '1.2.840.10065.1.12.1.16', '1.2.840.10065.1.12.1.17', '1.2.840.10065.1.12.1.18');

type
  TFHIRBundleEntryHelper = class helper (TFhirElementHelper) for TFHIRBundleEntry
  private
    function GetLinks(s: string): String;
    procedure SetLinks(s: string; const Value: String);
  public
    property Links[s : string] : String read GetLinks write SetLinks;
  end;

  TFhirBundleEntryListHelper = class helper for TFhirBundleEntryList
  public
    function append(url : String) : TFHIRBundleEntry; overload;
  end;

  TFHIRBundleHelper = class helper (TFhirResourceHelper) for TFHIRBundle
  private
    function GetLinks(s: string): String;
    procedure SetLinks(s: string; const Value: String);
  public
    property Links[s : string] : String read GetLinks write SetLinks;
    procedure deleteEntry(resource : TFHIRResource);
    class function Create(aType : TFhirBundleTypeEnum) : TFhirBundle; overload;
    class function wrap(aType : TFhirBundleTypeEnum; resource : TFhirResource) : TFhirBundle; overload;

    function findResource(ref : TFHIRReference) : TFhirResource;
    procedure signRef(code : TSignatureType; whoRef : String; format : TFHIRFormat; cert : String);
    function signRef2Provenance(code : TSignatureType; whoRef : String; format : TFHIRFormat; cert : String) : TFhirProvenance;
    function generatePresentation : String;
    function AsReference : TFHIRDocumentReference;
  end;

  TFHIRCodingListHelper = class helper for TFHIRCodingList
  public
    function AddCoding(system, code, display : String) : TFHIRCoding;
    procedure RemoveCoding(system, code : String);
  end;

  TFHIRStringListHelper = class helper for TFHIRStringList
  public
    function hasValue(value : String) : boolean;
    procedure add(s : String); overload;
    function summary : String;
  end;

  TFhirBundleLinkListHelper = class helper for TFhirBundleLinkList
  private
    function getMatch(rel: String): string;
    procedure SetMatch(rel: String; const Value: string);
  public
    procedure AddRelRef(rel, ref : String);
    function AsHeader : String;
    property Matches[rel : String] : string read getMatch write SetMatch;
  end;

  TFhirParametersHelper = class helper for TFhirParameters
  private
    function GetNamedParameter(name: String): TFHIRObject;
    function GetStringParameter(name: String): String;
    function GetBooleanParameter(name: String): boolean;
    function GetResourceParameter(name: String): TFHIRResource;
    function GetParameterParameter(name: String): TFhirParametersParameter;
  public
    function hasParameter(name : String):Boolean;
    Property NamedParameter[name : String] : TFHIRObject read GetNamedParameter; default;
    Property res[name : String] : TFHIRResource read GetResourceParameter;
    Property str[name : String] : String read GetStringParameter;
    Property param[name : String] : TFhirParametersParameter read GetParameterParameter;
    Property bool[name : String] : boolean read GetBooleanParameter;
    procedure AddParameter(name: String; value: TFHIRDataType); overload;
    procedure AddParameter(name: String; value: TFhirResource); overload;
    procedure AddParameter(name: String; value: boolean); overload;
    procedure AddParameter(name, value: string); overload;
    function AddParameter(name: String) : TFhirParametersParameter; overload;
    procedure AddParameter(p :  TFhirParametersParameter); overload;
  end;

  TFhirParametersParameterHelper = class helper for TFhirParametersParameter
  private
    function GetNamedParameter(name: String): TFHIRObject;
    function GetStringParameter(name: String): String;
    function GetParameterParameter(name: String): TFhirParametersParameter;
  public
    function hasParameter(name : String):Boolean;
    Property NamedParameter[name : String] : TFHIRObject read GetNamedParameter; default;
    Property str[name : String] : String read GetStringParameter;
    Property param[name : String] : TFhirParametersParameter read GetParameterParameter;
    procedure AddParameter(name: String; value: TFHIRDataType); overload;
    procedure AddParameter(name: String; value: TFhirResource); overload;
    procedure AddParameter(name: String; value: boolean); overload;
    procedure AddParameter(name: String; value: integer); overload;
    procedure AddParameter(name, value: string); overload;
    function AddParameter(name: String) : TFhirParametersParameter; overload;
    procedure AddParameter(p :  TFhirParametersParameter); overload;
  end;

  TFhirStrutureMapHelper = class helper for TFhirStructureMap
  private
  public
    function targetType : String;
  end;

  TFhirResourceMetaHelper = class helper for TFhirMeta
  private
    function GetTag(system, code: String): TFhirCoding;
  public
    function HasTag(system, code : String)  : boolean;
    function addTag(system, code, display : String) : TFhirCoding;
    function removeTag(system, code : String) : boolean;
    function removeLabel(system, code : String) : boolean;
    function hasProfile(url : String) : boolean;
    procedure addProfile(url : String);
    procedure dropProfile(url : String);
  end;

  TFhirOperationOutcomeIssueHelper = class helper for TFhirOperationOutcomeIssue
  public
    constructor Create(Severity : TFhirIssueSeverityEnum; Code : TFhirIssueTypeEnum; Diagnostics : string; location : String); overload;
    function summary : String;
  end;

  TFhirOperationOutcomeIssueListHelper = class helper for TFhirOperationOutcomeIssueList
  public
    function errorCount : integer;
  end;

  TFhirCodeSystem2 = TFhirCodeSystem;

  TFhirCodeSystemConceptHelper = class helper for TFhirCodeSystemConcept
  public
    function countDescendents : integer;
    function prop(code : String) : TFhirCodeSystemConceptProperty;
    function addProp(code : String) : TFhirCodeSystemConceptProperty;
    procedure deleteProp(code : String);
  end;

  { TFhirCodeSystemHelper }

  TFhirCodeSystemHelper = class helper for TFhirCodeSystem
  private
    function locate(parent: TFhirCodeSystemConcept; list: TFhirCodeSystemConceptList; code : String; var foundParent, foundConcept: TFhirCodeSystemConcept): boolean;
    procedure scanForSubsumes(parentList, conceptList : TFhirCodeSystemConceptList; code : String);
    function GetCodeSystem: TFhirCodeSystem;
    function GetSystem: String;

  public
    function getParents(concept : TFhirCodeSystemConcept) : TFhirCodeSystemConceptList;
    function getChildren(concept : TFhirCodeSystemConcept) : TFhirCodeSystemConceptList;
    property codeSystem : TFhirCodeSystem read GetCodeSystem;
    property system : String read GetSystem;
    function context : string;
    function isAbstract(concept :  TFhirCodeSystemConcept) : boolean;
    function isInactive(concept :  TFhirCodeSystemConcept) : boolean;
    function isDeprecated(concept :  TFhirCodeSystemConcept) : boolean;
    function codeStatus(concept :  TFhirCodeSystemConcept) : String;

    function buildImplicitValueSet : TFhirValueSet;
  end;

  TFhirQuestionnaireItemHelper = class helper for TFhirQuestionnaireItem
  private
    function getinitial: TFHIRDataType;
    procedure SetInitial(const Value: TFHIRDataType);
    function getOptions: String;

    procedure SetOptions(const Value: String);
    function GetOptionList: TFhirQuestionnaireItemAnswerOptionList;
  public
    function countDescendents : integer;

    property initial : TFHIRDataType read GetInitial write SetInitial;
    property options : String read getOptions write SetOptions;
    property optionList : TFhirQuestionnaireItemAnswerOptionList read GetOptionList;
  end;

  TFhirQuestionnaireHelper = class helper for TFhirQuestionnaire
  public
    function itemCount : integer;
  end;

  TFhirValueSetCodeSystem = TFhirCodeSystem;

  TFHIRNamingSystemHelper = class helper for TFHIRNamingSystem
  public
    function hasOid(oid : String) : boolean;
    function getUri : String;
  end;

  TFHIRObservationHelper = class helper for TFHIRObservation
  public
    function addComponent(system, code: String) : TFhirObservationComponent;
    function getComponent(system, code: String; var comp : TFhirObservationComponent) : boolean; overload;
    function getComponent(system : String; var comp : TFhirObservationComponent) : boolean; overload;
  end;

  TFHIRDocumentReferenceHelper = class helper for TFHIRDocumentReference
  public
    function asZip(var filename : String) : TStream;
  end;

  TFHIRAttachmentHelper = class helper for TFHIRAttachment
  public
    function asZipPart(i: integer) : TFslZipPart;
  end;

  TFHIRPractitionerHelper = class helper for TFHIRPractitioner
  public
    function display : string;
    function summary : string;
  end;

  TFhirReferenceHelper = class helper for TFhirReference
  private
    function GetEditString: String;
    procedure SetEditString(const Value: String);
  public
    constructor Create(ref : String); overload;
    function isRelative : boolean;
    function getType : String;
    function getId : String;
    property editString : String read GetEditString write SetEditString;
  end;

  TFhirCapabilityStatementRestResourceSearchParamHelper = class helper for TFhirCapabilityStatementRestResourceSearchParam
  public
    function summary : String;
  end;

  TFhirValueSetComposeIncludeHelper = class helper for TFhirValueSetComposeInclude
  public
    function summary : String;
  end;

  TFhirContactPointHelper = class helper for TFhirContactPoint
  public
    function isPhoneOrFax : boolean;
    function isEmail : boolean;
    class function fromEdit(n : String) : TFhirContactPoint;
  end;

  TFhirEncounterHelper = class helper for TFhirEncounter
  public
    function summary : String;
  end;

  TFhirEpisodeOfCareHelper = class helper for TFhirEpisodeOfCare
  public
    function summary : String;
  end;

  TFhirGroupHelper = class helper for TFhirGroup
  public
    function summary : String;
  end;

  TFhirMedicationHelper = class helper for TFhirMedication
  public
    function summary : String;
  end;

  TFhirOrganizationHelper = class helper for TFhirOrganization
  public
    function summary : String;
  end;

  { TFhirPatientHelper }

  TFhirPatientHelper = class helper for TFhirPatient
  public
    function summary : String;
    function genderPlus : String;
  end;

  TFhirPersonHelper = class helper for TFhirPerson
  public
    function summary : String;
  end;

  TFhirRelatedPersonHelper = class helper for TFhirRelatedPerson
  public
    function summary : String;
  end;

  TFhirSubstanceHelper = class helper for TFhirSubstance
  public
    function summary : String;
  end;

  TFHIRDeviceHelper = class helper for TFHIRDevice
  public
    function summary : String;
  end;

  TFhirEndpointHelper = class helper for TFhirEndpoint
  public
    function summary : String;
  end;

  TFHIRPractitionerRoleHelper = class helper for TFHIRPractitionerRole
  public
    function summary : String;
  end;

  { TFHIRHealthcareCardR4B }

  TFHIRHealthcareCardR4B = class (THealthcareCard)
  private
    function summarisePatient(pat : TFHIRPatient) : String;
    function summariseImmunization(imm : TFHIRImmunization) : String;
    function summariseObservation(obs : TFHIRObservation) : String;
    procedure htmlPatient(b : TFslHtmlBuilder; pat : TFHIRPatient);
    procedure htmlImmunization(b : TFslHtmlBuilder; imm : TFHIRImmunization; tx : TFHIRTerminologyService);
    procedure htmlObservation(b : TFslHtmlBuilder; obs : TFHIRObservation; tx : TFHIRTerminologyService);
  public
    procedure makeSummary; override;
    function htmlReport(tx : TFHIRTerminologyService) : String; override;
  end;

function summarise(coding : TFHIRCoding):String; overload;
function summarise(code : TFhirCodeableConcept):String; overload;

function gen(coding : TFHIRCoding):String; overload;
function gen(code : TFhirCodeableConcept):String; overload;
function gen(ref : TFhirReference) : String; overload;
function gen(id : TFhirIdentifier) : String; overload;
function gen(obj : TFhirAnnotation) : String; overload;
function gen(obj : TFhirAttachment) : String; overload;
function gen(obj : TFhirQuantity) : String; overload;
function gen(obj : TFhirRange) : String; overload;
function gen(obj : TFhirDate) : String; overload;
function gen(obj : TFhirPeriod) : String; overload;
function gen(obj : TFhirRatio) : String; overload;
function gen(obj : TFhirSampledData) : String; overload;
function gen(obj : TFhirSignature) : String; overload;
function gen(obj : TFhirAddress) : String; overload;
function gen(obj : TFhirContactPoint; hideType : boolean = false) : String; overload;
function gen(obj : TFhirTiming) : String; overload;
function gen(obj : TFhirUsageContext) : String; overload;
function gen(extension : TFHIRExtension) : String; overload;

function gen(t : TFHIRDataType):String; overload;

procedure genHtml(b : TFslHtmlBuilder; name : TFhirHumanName); overload;
procedure genHtml(b : TFslHtmlBuilder; id : TFhirIdentifier); overload;
procedure genHtml(b : TFslHtmlBuilder; p : TFhirPeriod); overload;
procedure genHtml(b : TFslHtmlBuilder; q : TFhirQuantity); overload;
procedure genHtml(b : TFslHtmlBuilder; c : TFhirCoding; tx : TFHIRTerminologyService); overload;
procedure genHtml(b : TFslHtmlBuilder; c : TFhirCodeableConcept; tx : TFHIRTerminologyService); overload;

function compareValues(e1, e2 : TFHIRObjectList; allowNull : boolean) : boolean; overload;
function compareValues(e1, e2 : TFHIRPrimitiveType; allowNull : boolean) : boolean; overload;
function compareValues(e1, e2 : TFHIRXhtmlNode; allowNull : boolean) : boolean; overload;
function hasProp(props : TList<String>; name : String; def : boolean) : boolean;

{$IFNDEF FPC}
type
  TResourceIteratorProcedure = reference to procedure (node : TFHIRObject);

procedure iterateResource(resource : TFHIRResource; proc : TResourceIteratorProcedure);
procedure iterateObject(obj : TFHIRObject; proc : TResourceIteratorProcedure);
{$ENDIF}

function describeResource(r: TFHIRResource): String;

implementation

uses
 {$IFDEF STACK_DUMPS}
  JclDebug,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Registry,
  {$ENDIF}
  fhir4b_elementmodel, fhir4b_parser;

{$IFDEF STACK_DUMPS}
function dumpStack : String;
var
  st : TJclStackInfoList;
  ts : TStringList;
begin
  if JclExceptionTrackingActive then
  begin
  st := JclCreateStackList(true, 3, nil);
  try
    ts := TStringList.Create;
    try
      st.AddToStrings(ts, true);
      result := ts.Text;
    finally
      ts.free;
    end;
  finally
    st.free;
  end;
  end
  else
    result := '';
end;
{$ENDIF}

function MakeParser(oWorker : TFHIRWorkerContext; langList : THTTPLanguageList; aFormat: TFHIRFormat; content: TBytes; policy : TFHIRXhtmlParserPolicy): TFHIRParser;
var
  mem : TBytesStream;
begin
  mem := TBytesStream.Create(content);
  try
    result := MakeParser(oWorker, langList, aformat, mem, policy);
  finally
    mem.free;
  end;
end;

function MakeParser(oWorker : TFHIRWorkerContext; langList : THTTPLanguageList; mimetype : String; content: TBytes; policy : TFHIRXhtmlParserPolicy): TFHIRParser; overload;
begin
  if mimeType.Contains('application/json') or mimeType.Contains('application/fhir+json') Then
    result := TFHIRParsers4B.parser(oWorker.Link, ffJson, langList.link)
  else if mimeType.Contains('text/plain') then
    result := TFHIRParsers4B.parser(oWorker.Link, ffText, langList.link)
  else if mimeType.Contains('application/xml') or mimeType.Contains('application/fhir+xml') or mimeType.Contains('text/xml')  then
    result := TFHIRParsers4B.parser(oWorker.Link, ffXml, langList.link)
  else
    result := TFHIRParsers4B.parser(oWorker.Link, DetectFormat(content), langList.link);
  try
    result.ParserPolicy := policy;
    result.Link;
  finally
    result.free;
  end;
end;
function MakeParser(oWorker : TFHIRWorkerContext; langList : THTTPLanguageList; aFormat: TFHIRFormat; oContent: TStream; policy : TFHIRXhtmlParserPolicy): TFHIRParser;
begin
  if aFormat in [ffUnspecified, ffXhtml] then
    result := TFHIRParsers4B.parser(oWorker.Link, DetectFormat(oContent), langList.link)
  else
    result := TFHIRParsers4B.parser(oWorker.Link, aFormat, langList.link);
  try
    result.source := oContent;
    result.ParserPolicy := policy;
    result.Parse;
    result.Link;
  finally
    result.free;
  end;
end;

function MakeComposer(Style : TFHIROutputStyle; langList : THTTPLanguageList; mimetype : String; worker : TFHIRWorkerContext) : TFHIRComposer;
begin
  if mimeType.StartsWith('text/xml') or mimeType.StartsWith('application/xml') or mimeType.StartsWith('application/fhir+xml') or (mimetype = 'xml') then
    result := TFHIRParsers4B.composer(worker.link, ffXml, langList.link, Style)
  else if mimeType.StartsWith('text/json') or mimeType.StartsWith('application/json') or mimeType.StartsWith('application/fhir+json') or (mimetype = 'json') then
    result := TFHIRParsers4B.composer(worker.link, ffJson, langList.link, Style)
//  else if mimeType.StartsWith('text/html') or mimeType.StartsWith('text/xhtml') or mimeType.StartsWith('application/fhir+xhtml') or (mimetype = 'xhtml') then
//    result := TFHIRXhtmlComposer.Create(worker.link, Style, langList)
  else
    raise EFHIRException.Create('Format '+mimetype+' not recognised');
end;

function ResourceTypeByName(name : String) : TFhirResourceType;
var
  index : Integer;
begin
  index := StringArrayIndexOfSensitive(CODES_TFhirResourceType, name);
  if index < 1 then
    raise EFHIRException.Create('Unknown resource name "'+name+'"');
  result := TFhirResourceType(index);
end;

function isResourceName(name : String; canbeLower : boolean = false) : boolean;
var
  index : Integer;
begin
  index := StringArrayIndexOfSensitive(CODES_TFhirResourceType, name);
  if (index <= 0) and canbeLower then
    index := StringArrayIndexOfSensitive(LOWERCASE_CODES_TFhirResourceType, name);
  result := index > 0;
end;

Function RecogniseFHIRResourceName(Const sName : String; out aType : TFhirResourceType): boolean;
var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOfSensitive(CODES_TFhirResourceType, sName);
  result := iIndex > -1;
  if result then
    aType := TFhirResourceType(iIndex);
End;

Function RecogniseFHIRResourceManagerName(Const sName : String; out aType : TFhirResourceType): boolean;
var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOfInsensitive(CODES_TFhirResourceType, sName);
  result := iIndex > -1;
  if result then
    aType := TFhirResourceType(iIndex);
End;

function geTFhirResourceNarrativeAsText(resource : TFhirDomainResource) : String;
begin
  result := resource.text.div_.Content;
end;

procedure iterateReferences(path : String; node : TFHIRObject; list : TFhirReferenceList);
var
  iter : TFHIRPropertyIterator;
  i : integer;
begin
  iter := node.createIterator(true, false);
  try
    while iter.More do
    begin
      if (iter.Current.Values <> nil)  then
      begin
        if StringStartsWith(iter.Current.Type_, 'Reference(') then
        begin
          for i := 0 to iter.Current.Values.count - 1 do
            if (iter.current.Values[i] <> nil) and (iter.current.Values[i].fhirType = 'Reference') and not StringStartsWith(TFhirReference(iter.current.Values[i]).reference, '#') then
              list.add(iter.Current.Values[i].Link)
        end
        else if (iter.Current.Type_ = 'Resource') then
        begin
          for i := 0 to iter.Current.Values.count - 1 do
            iterateReferences(path+'/'+iter.Current.Name, TFhirReference(iter.current.Values[i]), list)
        end
        else if not ((node is TFHIRPrimitiveType) and (iter.current.name = 'value')) then
          for i := 0 to iter.Current.Values.Count - 1 Do
            iterateReferences(path+'/'+iter.Current.Name, iter.Current.Values[i], list);
      end;
      iter.Next;
    end;
  finally
    iter.free;
  end;
end;

procedure listReferences(resource : TFhirResource; list : TFhirReferenceList);
begin
  iterateReferences(CODES_TFhirResourceType[resource.resourceType], resource, list);
end;

procedure iterateAttachments(path : String; node : TFHIRObject; list : TFhirAttachmentList);
var
  iter : TFHIRPropertyIterator;
  i : integer;
begin
  iter := node.createIterator(true, false);
  try
    while iter.More do
    begin
      if (iter.Current.Values <> nil)  then
        for i := 0 to iter.Current.Values.Count - 1 do
          if (iter.Current.Type_ = 'Attachment') then
            list.add(iter.Current.Values[i].Link)
          else if not ((node is TFHIRPrimitiveType) and (iter.current.name = 'value'))  then
            iterateAttachments(path+'/'+iter.Current.Name, iter.Current.Values[i], list);
      iter.Next;
    end;
  finally
    iter.free;
  end;
end;

procedure listAttachments(resource : TFhirResource; list : TFhirAttachmentList);
begin
  iterateAttachments(CODES_TFhirResourceType[resource.resourceType], resource, list);
end;

function asUTCMin(value : TFhirInstant) : TDateTime;
begin
  if (value = nil) or (value.value.null) then
    result := MIN_DATE
  else
    result := value.value.Min.UTC.DateTime;
end;

function asUTCMax(value : TFhirInstant) : TDateTime;
begin
  if (value = nil) or (value.value.null) then
    result := MAX_DATE
  else
    result := value.value.Max.UTC.DateTime;
end;

function asUTCMin(value : TFhirDateTime) : TDateTime;
begin
  if (value = nil) or (value.value.null) then
    result := MIN_DATE
  else
    result := value.value.Min.UTC.DateTime;
end;

function asUTCMax(value : TFhirDateTime) : TDateTime;
begin
  if (value = nil) or (value.value.null) then
    result := MAX_DATE
  else
    result := value.value.Max.UTC.DateTime;
end;

function asUTCMin(value : TFhirDate) : TDateTime;
begin
  if (value = nil) or (value.value.null) then
    result := MIN_DATE
  else
    result := value.value.Min.UTC.DateTime;
end;

function asUTCMax(value : TFhirDate) : TDateTime;
begin
  if (value = nil) or (value.value.null) then
    result := MAX_DATE
  else
    result := value.value.Max.UTC.DateTime;
end;

function asUTCMin(value : TFhirPeriod) : TDateTime;
begin
  if (value = nil) or (value.start.null) then
    result := MIN_DATE
  else
    result := value.start.Max.UTC.DateTime;
end;

function asUTCMax(value : TFhirPeriod) : TDateTime;
begin
  if (value = nil) or (value.end_.null) then
    result := MAX_DATE
  else
    result := value.end_.Max.UTC.DateTime;
end;

function asUTCMin(value : TFhirTiming) : TDateTime;
var
  i : integer;
begin
  if (value = nil) or (value.eventList.Count = 0) then
    result := MIN_DATE
  else
  begin
    result := MAX_DATE;
    for i := 0 to value.eventList.count - 1 do
      result := DateTimeMin(result, AsUTCMin(value.eventList[i]));
  end;
end;

function asUTCMax(value : TFhirTiming) : TDateTime;
var
  duration : TDateTime;
  i : integer;
begin
  if (value = nil) then
    result := MAX_DATE
  else if (value.repeat_ = nil) then
  begin
    if value.eventList.Count = 0 then
      result := MAX_DATE
    else
      result := MIN_DATE;
      for i := 0 to value.eventList.count - 1 do
        result := DateTimeMax(result, AsUTCMax(value.eventList[i]));
  end
  else if (value.repeat_.bounds <> nil) and (value.repeat_.bounds is TFhirPeriod) and (TFhirPeriod(value.repeat_.bounds).end_.notNull) then
    result := asUTCMax(TFhirPeriod(value.repeat_.bounds).end_Element)
  else if (value.repeat_.count <> '') and (value.eventList.Count > 0) and
    (value.repeat_.frequency <> '') and (value.repeat_.period <> '') and (value.repeat_.periodunit <> UnitsOfTimeNull) then
  begin
    result := MIN_DATE;
    for i := 0 to value.eventList.count - 1 do
      result := DateTimeMax(result, AsUTCMax(value.eventList[i]));
    if result = MIN_DATE then
      result := MAX_DATE
    else
    begin
      case value.repeat_.periodunit of
        UnitsOfTimeS : duration := DATETIME_SECOND_ONE;
        UnitsOfTimeMin : duration := DATETIME_MINUTE_ONE;
        UnitsOfTimeH : duration := DATETIME_HOUR_ONE;
        UnitsOfTimeD : duration := 1;
        UnitsOfTimeWk : duration := 7;
        UnitsOfTimeMo : duration := 30;
        UnitsOfTimeA : duration := 365 // todo - how to correct for leap years?;
      else
        raise EFHIRException.Create('unknown duration units "'+value.repeat_.periodunitElement.value+'"');
      end;
      result := result + (StrToInt(value.repeat_.count) * duration / StrToInt(value.repeat_.frequency));
    end;
  end
  else
    result := MAX_DATE;
end;

{
function GetResourceFromFeed(feed : TFHIRAtomFeed; ref : TFhirReference) : TFHIRResource;
var
  i : integer;
begin
  result := nil;
  for i := 0 to feed.entries.count - 1 do
  begin
    if feed.entries[i].id = ref.reference then
    begin
      result := feed.entries[i].resource;
      break;
    end;
  end;
end;
}

function FindContainedResource(resource : TFhirDomainResource; ref : TFhirReference) : TFhirResource;
var
  i : integer;
begin
  result := nil;
  for i := 0 to resource.containedList.Count - 1 do
    if ('#'+resource.containedList[i].Id = ref.reference) then
    begin
      result := resource.containedList[i];
      exit;
    end;
end;

function FindContainedResource(resource : TFhirDomainResource; ref : string) : TFhirResource;
var
  i : integer;
begin
  result := nil;
  for i := 0 to resource.containedList.Count - 1 do
    if ('#'+resource.containedList[i].Id = ref) then
    begin
      result := resource.containedList[i];
      exit;
    end;
end;

function BuildOperationOutcome4(langList : THTTPLanguageList; e : exception; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;
begin
  result := BuildOperationOutcome(langList, e.message, issueCode);
end;

Function BuildOperationOutcome4(langList : THTTPLanguageList; message : String; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;
begin
  result := BuildOperationOutcome(langList, message, issueCode);
end;

function BuildOperationOutcome(langList : THTTPLanguageList; e : exception; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome;
begin
  result := BuildOperationOutcome(langList, e.message, issueCode);
end;

Function BuildOperationOutcome(langList : THTTPLanguageList; message : String; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;
var
  outcome : TFhirOperationOutcome;
  report :  TFhirOperationOutcomeIssue;
begin
  outcome := TFhirOperationOutcome.Create;
  try
    outcome.text := TFhirNarrative.Create;
    outcome.text.status := NarrativeStatusGenerated;
    outcome.text.div_ := TFHIRXhtmlParser.Parse(langList, xppReject, [], '<div><p>'+FormatTextToHTML(message)+'</p></div>');
    report := outcome.issueList.Append;
    report.severity := issueSeverityError;
    report.code := issueCode;
    report.diagnostics := message;
    result := outcome.Link;
  finally
    outcome.free;
  end;
end;

function HasExtension(element : TFhirElement; url : string):Boolean;
begin
  result := GetExtension(element, url) <> nil;
end;

function GetExtension(element : TFhirElement; url : string) : TFhirExtension;
var
  i : integer;
  ex : TFhirExtension;
begin
  result := nil;
  for i := 0 to element.ExtensionList.count - 1 do
  begin
    ex := element.ExtensionList[i];
    if ex.url = url then
    begin
      result := ex;
      exit;
    end;
  end;
end;

function gen(coding : TFHIRCoding):String; overload;
var
  system : String;
begin
  if (coding = nil) then
    result := ''
  else
  begin
    system := csName(coding.system);
    result := system+'#'+coding.code;
    if (coding.display <> '') then
      result := result + ' "'+coding.display+'"';
    if (coding.version <> '') then
      result := result + ' (v='+coding.version+')';
  end;
end;

function gen(code : TFhirCodeableConcept):String; overload;
begin
  if (code = nil) then
    result := ''
  else if (code.text <> '') then
    result := code.text
  else if (code.codingList.Count > 0) then
    result := gen(code.codingList[0])
  else
    result := '';
end;

function summarise(coding : TFHIRCoding):String; overload;
begin
  if (coding = nil) then
     result := ''
  else
    result := coding.system+'#'+coding.Code;
end;

function summarise(code : TFhirCodeableConcept):String; overload;
var
  c : TFHIRCoding;
begin
  if (code = nil) then
    result := ''
  else
  begin
    result := '';
    for c in code.codingList do
      if result <> '' then
        result := result +', '+summarise(c)
      else
        result := summarise(c);
    if (code.text <> '') then
      if result <> '' then
        result := result +', ('+code.text+')'
      else
        result := '('+code.text+')';
  end;
end;

function gen(ref : TFhirReference) : String;
begin
  if (ref = nil) then
    result := ''
  else if ref.display <> '' then
    result := ref.display
  else
    result := ref.reference;
end;

function gen(id : TFhirIdentifier) : String;
begin
  if (id = nil) then
    result := ''
  else
    result := id.value;
end;

function gen(obj : TFhirAnnotation) : String;
begin
  if (obj = nil) then
    result := ''
  else
    result := obj.text;
end;

function gen(obj : TFhirAttachment) : String;
begin
  if (obj = nil) then
    result := ''
  else if (obj.url <> '') then
    result := obj.url
  else
    result := '(Attachment)';
end;

function gen(obj : TFhirQuantity) : String;
begin
  if (obj = nil) then
  begin
    result := '';
    exit;
  end;

  if obj.comparator = QuantityComparatorNull then
    result := obj.value
  else
    result := CODES_TFhirQuantityComparatorEnum[obj.comparator]+obj.value;
  if obj.unit_ <> '' then
    result := result + ' '+obj.unit_
  else
    result := result + ' '+obj.code;
end;

function gen(obj : TFhirRange) : String;
begin
  if (obj = nil) then
    result := ''
  else
    result := gen(obj.low) + ' -> '+gen(obj.high);
end;

function gen(obj : TFslDateTime) : String; overload;
begin
  if (obj.null) then
    result := ''
  else
    result := obj.ToString;
end;

function gen(obj : TFhirPeriod) : String;
begin
  if (obj = nil) then
    result := ''
  else
    result := gen(obj.start) + ' -> '+gen(obj.end_);
end;

function gen(obj : TFhirDate) : String;
begin
  if obj = nil then
    result := ''
  else
    result := obj.value.toString('c');
end;

function gen(obj : TFhirRatio) : String;
begin
  if (obj = nil) then
    result := ''
  else
    result := gen(obj.numerator) + ' / '+gen(obj.denominator);
end;

function gen(obj : TFhirSampledData) : String;
begin
  if (obj = nil) then
    result := ''
  else
    result := '(SampledData)';
end;

function gen(obj : TFhirSignature) : String;
begin
  if (obj = nil) then
    result := ''
  else
    result := '(Signature)';
end;

function gen(obj : TFhirAddress) : String;
var
  i : integer;
begin
  if obj = nil then
    result := ''
  else if obj.text <> '' then
    result := obj.text
  else
  begin
    result := ' ';
    for i := 0 to obj.lineList.Count - 1 do
      result := result + obj.lineList[i].value;
    result := result + ' '+obj.city;
    result := result + ' '+obj.district;
    result := result + ' '+obj.state;
    result := result + ' '+obj.postalCode;
    result := result + ' '+obj.country;
    result := result.Replace('  ', ' ').Trim;
  end;
end;

function gen(obj : TFhirContactPoint; hideType : boolean = false) : String;
begin
  if (obj = nil) then
    result := ''
  else if not hideType then
    result := obj.value
  else
    result := CODES_TFhirContactPointSystemEnum[obj.system]+': '+obj.value;
end;

function gen(obj : TFhirTiming) : String;
begin
  result := '';
end;

function gen(obj : TFhirUsageContext) : String; overload;
begin
  result := gen(obj.code)+'='+gen(obj.value);
end;

function gen(extension : TFHIRExtension):String; overload;
begin
  if extension = nil then
    result := ''
  else if (extension.Value is TFHIRCode) then
    result := TFHIRCode(extension.value).value
  else if (extension.value is TFHIRCoding) then
    result := gen(TFHIRCoding(extension.value))
  else
    raise EFHIRException.Create('Unhandled type '+extension.Value.ClassName);
end;

procedure BuildNarrative(op: TFhirOperationOutcome; opDesc : String);
var
  x, tbl, tr, td : TFhirXHtmlNode;
  hasSource, success, d : boolean;
  i, j : integer;
  issue : TFhirOperationOutcomeIssue;
  s : TFhirString;
begin
  x := TFhirXHtmlNode.Create;
  try
    x.NodeType := fhntElement;
    x.Name := 'div';
    x.AddTag('p').addTag('b').addText('Operation Outcome for :'+opDesc);

    hasSource := false;
    success := true;
    for i := 0 to op.issueList.count - 1 do
    begin
      issue := op.issueList[i];
      success := success and (issue.Severity = IssueSeverityInformation);
      hasSource := hasSource or (hasExtension(issue, 'http://hl7.org/fhir/tools#issue-source'));
    end;
    if (success) then
      x.AddChild('p').addText('All OK');
    if op.issueList.count > 0 then
    begin
      tbl := x.addTag('table');
      tbl.setAttribute('class', 'grid'); // on the basis that we'll most likely be rendered using the standard fhir css, but it doesn't really matter
      tr := tbl.addTag('tr');
      tr.addTag('td').addTag('b').addText('Severity');
      tr.addTag('td').addTag('b').addText('Location');
      tr.addTag('td').addTag('b').addText('Details');
      tr.addTag('td').addTag('b').addText('Diagnostics');
        tr.addTag('td').addTag('b').addText('Type');
      if (hasSource) then
        tr.addTag('td').addTag('b').addText('Source');
      for i := 0 to op.issueList.count - 1 do
      begin
        issue := op.issueList[i];
        tr := tbl.addTag('tr');
        tr.addTag('td').addText(CODES_TFhirIssueSeverityEnum[issue.severity]);
        td := tr.addTag('td');
        d := false;
        for j := 0 to issue.locationList.count -1 do
        begin
           s := issue.locationList[j];
           if (d) then
             td.addText(', ')
           else
             d := true;
           td.addText(s.Value);
        end;
        tr.addTag('td').addText(gen(issue.details));
        tr.addTag('td').addText(issue.diagnostics);
        tr.addTag('td').addText(CODES_TFhirIssueTypeEnum[issue.code]);
        if (hasSource) then
          tr.addTag('td').addText(gen(getExtension(issue, 'http://hl7.org/fhir/tools#issue-source')));
      end;
    end;
    if (op.Text = nil) then
      op.Text := TFhirNarrative.Create;
    op.Text.div_ := x.link;
    if hasSource then
      op.Text.status := NarrativeStatusExtensions
    else
      op.Text.status := NarrativeStatusGenerated;
  finally
    x.free;
  end;
end;

procedure addTableHeaderRowStandard(t : TFhirXHtmlNode);
var
  tr, td, b : TFhirXHtmlNode;
begin
  tr := t.addTag('tr');
  td := tr.addTag('td');
  b := td.addTag('b');
  b.addText('Code');
  td := tr.addTag('td');
  b := td.addTag('b');
  b.addText('Display');
  td := tr.addTag('td');
  b := td.addTag('b');
  b.addText('Definition');
end;

procedure addTableHeaderRowExpansion(t : TFhirXHtmlNode);
var
  tr, td, b : TFhirXHtmlNode;
begin
  tr := t.addTag('tr');
  td := tr.addTag('td');
  b := td.addTag('b');
  b.addText('Code');
  td := tr.addTag('td');
  b := td.addTag('b');
  b.addText('System');
  td := tr.addTag('td');
  b := td.addTag('b');
  b.addText('Display');
end;

procedure addDefineRowToTable(t : TFhirXHtmlNode; c : TFhirCodeSystemConcept; indent : integer);
var
  tr, td : TFhirXHtmlNode;
  s : string;
  i : integer;
begin
  tr := t.addTag('tr');
  td := tr.addTag('td');
  s := StringpadLeft('', '.', indent*2);
  td.addText(s+c.Code);
  td := tr.addTag('td');
  td.addText(c.Display);
  td := tr.addTag('td');
  td.addText(c.Definition);
  for i := 0 to c.ConceptList.count - 1 do
    addDefineRowToTable(t, c.conceptList[i], indent+1);
end;

procedure addContainsRowToTable(t : TFhirXHtmlNode; c : TFhirValueSetExpansionContains; indent : integer);
var
  tr, td : TFhirXHtmlNode;
  s : string;
  i : integer;
begin
  tr := t.addTag('tr');
  td := tr.addTag('td');
  s := StringpadLeft('', '.', indent*2);
  if c.code = '' then
    td.addText(s+'+')
  else
    td.addText(s+c.Code);
  td := tr.addTag('td');
  td.addText(c.System);
  td := tr.addTag('td');
  td.addText(c.Display);
  for i := 0 to c.containsList.count - 1 do
    addContainsRowToTable(t, c.containsList[i], indent+1);
end;

procedure generateExpansion(x : TFhirXHtmlNode; vs : TFhirValueSet);
var
  h, p, t : TFhirXHtmlNode;
  i : integer;
begin
  h := x.addTag('h2');
  h.addText('Expansion for '+vs.Name);
  p := x.addTag('p');
  p.addText(vs.Description);
  p := x.addTag('p');
  p.addText('This value set is an expansion, and includes the following terms in the expansion');
  t := x.addTag('table');
  addTableHeaderRowExpansion(t);
  for i := 0 to vs.expansion.containsList.Count - 1 do
    addContainsRowToTable(t, vs.expansion.containsList[i], 0);
end;

procedure generateComposition(x : TFhirXHtmlNode; vs : TFhirValueSet);
begin
   raise EFHIRException.Create('todo');
end;

procedure BuildNarrative(vs : TFhirValueSet);
var
  x, h, p : TFhirXHtmlNode;
begin
  x := TFhirXHtmlNode.Create;
  try
    x.NodeType := fhntElement;
    x.Name := 'div';

    if (vs.Expansion <> nil) then
      generateExpansion(x, vs)
    else
    begin
      h := x.addTag('h2');
      h.addText(vs.Name);
      p := x.addTag('p');
      p.addText(vs.Description);
      if (vs.Compose <> nil) then
        generateComposition(x, vs);
    end;

    if (vs.Text = nil) then
      vs.Text := TFhirNarrative.Create;
    vs.Text.div_ := x.link;
    vs.Text.status := NarrativeStatusGenerated;
  finally
    x.free;
  end;
end;

procedure BuildNarrative(cs : TFHIRCodeSystem);
var
  t, x, h, p : TFhirXHtmlNode;
  i : integer;
begin
  x := TFhirXHtmlNode.Create;
  try
    x.NodeType := fhntElement;
    x.Name := 'div';
      h := x.addTag('h2');
      h.addText(cs.Name);
      p := x.addTag('p');
      p.addText(cs.Description);

      p := x.addTag('p');
      p.addText('The code system '+cs.url+' defines the following codes: ');
      t := x.addTag('table');
      addTableHeaderRowStandard(t);
      for i := 0 to cs.ConceptList.Count - 1 do
        addDefineRowToTable(t, cs.ConceptList[i], 0);
      if (cs.Text = nil) then
        cs.Text := TFhirNarrative.Create;
      cs.Text.div_ := x.link;
      cs.Text.status := NarrativeStatusGenerated;
  finally
    x.free;
  end;
end;

function GetEmailAddress(contacts : TFhirContactPointList):String;
var
  i : integer;
begin
  result := '';
  if contacts <> nil then
    for i := 0 to contacts.Count - 1 do
      if contacts[i].system = ContactPointSystemEmail then
        result := contacts[i].value;
end;

function HumanNamesAsText(names : TFhirHumanNameList):String;
begin
  if (names = nil) or (names.Count = 0) then
    result := '??'
  else
    result := HumanNameAsText(names[0]);
end;

function HumanNameAsText(name : TFhirHumanName):String;
var
  i : integer;
begin
  if name = nil then
    result := ''
  else if name.text <> '' then
    result := name.text
  else
  begin
    result := '';
    for i := 0 to name.givenList.Count - 1 do
      result := result + name.givenList[i].value+' ';
    result := result + name.family+' ';
  end;
end;

function IdentifierAsText(id : TFhirIdentifier):String;
var
  i : integer;
begin
  if id = nil then
    result := ''
  else
    result := id.value;
end;

function IdentifiersAsText(ids : TFhirIdentifierList):String;
begin
  if (ids = nil) or (ids.Count = 0) then
    result := '??'
  else
    result := IdentifierAsText(ids[0]);
end;

function ContactAsText(cp : TFHIRContactPoint):String;
var
  i : integer;
begin
  if cp = nil then
    result := ''
  else
    result := cp.value;
end;

function ContactsAsText(cps : TFHIRContactPointList):String;
begin
  if (cps = nil) or (cps.Count = 0) then
    result := '??'
  else
    result := ContactAsText(cps[0]);
end;

function LoadDTFromFormParam(worker : TFHIRWorkerContext; part : TMimePart; langList : THTTPLanguageList; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType;
var
  ct : String;
  parser : TFHIRParser;
  mem : TFslMemoryStream;
  s : TVCLStream;
begin
  parser := nil;
  try
    // first, figure out the format
    ct := part.Headers.Values['Content-Type'];
    if ct <> '' then
    begin
      if StringStartsWithInsensitive(ct, 'application/json') or StringStartsWithInsensitive(ct, 'application/fhir+json') or StringStartsWithInsensitive(ct, 'application/json+fhir') or StringStartsWithInsensitive(ct, 'json') or StringStartsWithInsensitive(ct, 'text/json') Then
        parser := TFHIRParsers4B.parser(worker.link, ffJson, langList.link)
      else if StringStartsWithInsensitive(ct, 'text/xml') or StringStartsWithInsensitive(ct, 'application/xml') or
          StringStartsWithInsensitive(ct, 'application/fhir+xml') or StringStartsWithInsensitive(ct, 'application/xml+fhir') or StringStartsWithInsensitive(ct, 'xml') Then
        parser := TFHIRParsers4B.parser(worker.link, ffXml, langList.link);
    end;
    if parser = nil then
      parser := TFHIRParsers4B.parser(worker.link, DetectFormat(part.content), langList.link);
    mem := TFslMemoryStream.Create;
    try
      mem.Buffer := part.content.Link;
      s := TVCLStream.Create;
      try
        s.Stream := mem.Link;
        parser.source := s;
        result := parser.ParseDT(name, type_) as TFHIRDataType;
      finally
        s.free;
      end;
    finally
      mem.free;
    end;
  finally
    parser.free;
  end;
end;

function LoadDTFromParam(worker : TFHIRWorkerContext; value : String; langList : THTTPLanguageList; name : String; type_ : TFHIRDataTypeClass) : TFHIRDataType;
var
  parser : TFHIRParser;
  mem : TStringStream;
begin
  parser := TFHIRParsers4B.parser(worker.link, ffJson, langList.link);
  try
    // first, figure out the format
    mem := TStringStream.Create(value, TEncoding.UTF8);
    try
      parser.source := mem;
      result := parser.ParseDT(name, type_) as TFHIRDataType;
    finally
      mem.free;
    end;
  finally
    parser.free;
  end;
end;

function LoadFromFormParam(worker : TFHIRWorkerContext; part : TMimePart; langList : THTTPLanguageList) : TFhirResource;
var
  ct : String;
  parser : TFHIRParser;
  s : TVCLStream;
  mem : TFslMemoryStream;
begin
  parser := nil;
  try
    // first, figure out the format
    ct := part.Headers.Values['Content-Type'];
    if ct <> '' then
    begin
      if StringStartsWithInsensitive(ct, 'application/json') or StringStartsWithInsensitive(ct, 'application/fhir+json') or StringStartsWithInsensitive(ct, 'application/json+fhir') or StringStartsWithInsensitive(ct, 'json') or StringStartsWithInsensitive(ct, 'text/json') Then
        parser := TFHIRParsers4B.parser(worker.link, ffJson, langList.link)
      else if StringStartsWithInsensitive(ct, 'text/xml') or StringStartsWithInsensitive(ct, 'application/xml') or
          StringStartsWithInsensitive(ct, 'application/fhir+xml') or StringStartsWithInsensitive(ct, 'application/xml+fhir') or StringStartsWithInsensitive(ct, 'xml') Then
        parser := TFHIRParsers4B.parser(worker.link, ffXml, langList.link);
    end;
    if parser = nil then
      parser := TFHIRParsers4B.parser(worker.link, DetectFormat(part.content), langList.link);
    mem := TFslMemoryStream.Create;
    try
      mem.Buffer := part.content.Link;
      s := TVCLStream.Create;
      try
        s.Stream := mem.Link;
        parser.source := s;
        parser.Parse;
        result := parser.resource.Link as TFHIRResource;
      finally
        s.free;
      end;
    finally
      mem.free;
    end;
  finally
    parser.free;
  end;
end;

(*

  procedure generateComposition(x : TFHIRXHtmlNode; vs : TFHIRValueSet, Map<String, AtomEntry> codeSystems) throws Exception begin
    TFhirXHtmlNode h := x.addTag('h2');
    h.addText(vs.Name);
    TFhirXHtmlNode p := x.addTag('p');
    p.addText(vs.Description);
    p := x.addTag('p');
    p.addText('This value set includes terms defined in other code systems, using the following rules:');
    TFhirXHtmlNode ul := x.addTag('ul');
    TFhirXHtmlNode li;
    for (Uri imp : vs.Compose.Import) begin
      li := ul.addTag('li');
      li.addText('Import all the codes that are part of '+imp.Value);
    end;
    for (ConceptSetComponent inc : vs.Compose.Include) begin
      genInclude(ul, inc, 'Include', codeSystems);
    end;
    for (ConceptSetComponent exc : vs.Compose.Exclude) begin
      genInclude(ul, exc, 'Exclude', codeSystems);
    end;
  end;

  procedure genInclude(TFhirXHtmlNode ul, ConceptSetComponent inc, String type, Map<String, AtomEntry> codeSystems) throws Exception begin
    TFhirXHtmlNode li;
    li := ul.addTag('li');
    AtomEntry e := codeSystems.(inc.System.toString);

    if (inc.Code.size := := 0 && inc.Filter.size := := 0) begin then
      li.addText(type+' all codes defined in ');
      addCsRef(inc, li, e);
    end; else begin
      if (inc.Code.size > 0) begin then
        li.addText(type+' these codes as defined in ');
        addCsRef(inc, li, e);

        TFhirXHtmlNode t := li.addTag('table');
        addTableHeaderRowStandard(t);
        for (Code c : inc.Code) begin
          TFhirXHtmlNode tr := t.addTag('tr');
          TFhirXHtmlNode td := tr.addTag('td');
          td.addText(c.Value);
          ValueSetDefineConceptComponent cc := getConceptForCode(e, c.Value);
          if (cc <> nil) begin then
            td := tr.addTag('td');
            if (!Utilities.noString(cc.Display)) then
              td.addText(cc.Display);
            td := tr.addTag('td');
            if (!Utilities.noString(cc.Definition)) then
              td.addText(cc.Definition);
          end;
        end;
      end;
      for (ConceptSetFilterComponent f : inc.Filter) begin
        li.addText(type+' codes from ');
        addCsRef(inc, li, e);
        li.addText(' where '+f.PropertyST+' '+describe(f.Op)+' ');
        if (e <> nil && codeExistsInValueSet(e, f.Value)) begin then
          TFhirXHtmlNode a := li.addTag('a');
          a.addTag(f.Value);
          a.setAttribute('href', getCsRef(e)+'#'+f.Value);
        end; else
          li.addText(f.Value);
      end;
    end;
  end;

  private String describe(FilterOperator op) begin
    switch (op) begin
    case equal: return ' := ';
    case isA: return ' is-a ';
    case isNotA: return ' is-not-a ';
    case regex: return ' matches (by regex) ';

    end;
    return nil;
  end;

  private ValueSetDefineConceptComponent getConceptForCode(AtomEntry e, String code) begin
    if (e := := nil) then
      return nil;
    vs : TFHIRValueSet := (ValueSet) e.Resource;
    if (vs.CodeSystem := := nil) then
      return nil;
    for (ValueSetDefineConceptComponent c : vs.CodeSystem.Concept) begin
      ValueSetDefineConceptComponent v := getConceptForCode(c, code);
      if (v <> nil) then
        return v;
    end;
    return nil;
  end;

  private ValueSetDefineConceptComponent getConceptForCode(ValueSetDefineConceptComponent c, String code) begin
    if (code.equals(c.Code)) then
      return c;
    for (ValueSetDefineConceptComponent cc : c.Concept) begin
      ValueSetDefineConceptComponent v := getConceptForCode(cc, code);
      if (v <> nil) then
        return v;
    end;
    return nil;
  end;

  procedure addCsRef(ConceptSetComponent inc, TFhirXHtmlNode li, AtomEntry cs) begin
    if (cs <> nil && cs.Links.('self') <> nil) begin then
      TFhirXHtmlNode a := li.addTag('a');
      a.setAttribute('href', cs.Links.('self').replace('\\', '/'));
      a.addText(inc.System.toString);
    end; else
      li.addText(inc.System.toString);
  end;

  private String getCsRef(AtomEntry cs) begin
    return cs.Links.('self').replace('\\', '/');
  end;

  private boolean codeExistsInValueSet(AtomEntry cs, String code) begin
    vs : TFHIRValueSet := (ValueSet) cs.Resource;
    for (ValueSetDefineConceptComponent c : vs.CodeSystem.Concept) begin
      if (inConcept(code, c)) then
        return true;
    end;
    return false;
  end;

  private boolean inConcept(String code, ValueSetDefineConceptComponent c) begin
    if (c.Code <> nil && c.Code.equals(code)) then
      return true;
    for (ValueSetDefineConceptComponent g : c.Concept) begin
      if (inConcept(code, g)) then
        return true;
    end;
    return false;
  end;

*)

function getConformanceResourceUrl(res : TFHIRResource) : string;
begin
  case res.ResourceType of
    frtCodeSystem: result := TFHIRCodeSystem(res).url;
    frtConceptMap: result := TFHIRConceptMap(res).url;
    frtCapabilityStatement: result := TFHIRCapabilityStatement(res).url;
    frtImplementationGuide: result := TFHIRImplementationGuide(res).url;
    frtOperationDefinition: result := TFHIROperationDefinition(res).url;
    frtSearchParameter: result := TFHIRSearchParameter(res).url;
    frtStructureDefinition: result := TFHIRStructureDefinition(res).url;
    frtStructureMap: result := TFHIRStructureMap(res).url;
    frtTestScript: result := TFHIRTestScript(res).url;
    frtValueSet: result := TFHIRValueSet(res).url;
  else
    result := '';
  end;
end;

{ TFHIROperationOutcomeHelper }

function TFHIROperationOutcomeHelper.asExceptionMessage: String;
var
  b : TStringBuilder;
  issue : TFhirOperationOutcomeIssue;
begin
  b := TStringBuilder.Create;
  try
    for issue in issueList do
    begin
      if (b.Length > 0) then
        b.Append(', ');
      b.append(issue.summary);
    end;
    result := b.ToString;
  finally
    b.free;
  end;
end;

function TFHIROperationOutcomeHelper.error(source : String; typeCode : TFhirIssueTypeEnum; path: string; test: boolean; msg: string): boolean;
var
  issue : TFhirOperationOutcomeIssue;
  ex : TFhirExtension;
begin
  if not test then
  begin
    issue := TFhirOperationOutcomeIssue.Create;
    try
      issue.severity := IssueSeverityError;
      issue.code := typeCode;
      issue.details := TFHIRCodeableConcept.Create;
      issue.details.text := msg;
      {$IFDEF STACK_DUMPS}
      issue.diagnostics := dumpStack;
      {$ENDIF}
      if (path <> '') then
        issue.locationList.Append.value := path;
      ex := issue.ExtensionList.Append;
      ex.url := 'http://hl7.org/fhir/tools#issue-source';
      ex.value := TFhirCode.Create;
      TFhirCode(ex.value).value := source;
      self.issueList.add(issue.link);
      if self.text = nil then
      begin
        self.text := TFhirNarrative.Create;
        self.text.div_ := TFhirXHtmlNode.Create;
        self.text.div_.NodeType := fhntElement;
        self.text.div_.Name := 'div';
        self.text.div_.AddChild('div').SetAttribute('style', 'background: Salmon').AddText(msg);
      end;
    finally
      issue.free;
    end;
  end;
  result := test;
end;

function TFHIROperationOutcomeHelper.hasErrors: boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to issueList.Count - 1 do
    result := result or (issueList[i].severity in [IssueSeverityFatal, IssueSeverityError]);
end;

function TFHIROperationOutcomeHelper.hint(source : String; typeCode : TFhirIssueTypeEnum; path: string; test: boolean; msg: string): boolean;
var
  issue : TFhirOperationOutcomeIssue;
  ex : TFhirExtension;
begin
  if not test then
  begin
    issue := TFhirOperationOutcomeIssue.Create;
    try
      issue.severity := IssueSeverityInformation;
      issue.code := typeCode;
      issue.details := TFHIRCodeableConcept.Create;
      issue.details.text := msg;
      {$IFDEF STACK_DUMPS}
      issue.diagnostics := dumpStack;
      {$ENDIF}
      if (path <> '') then
        issue.locationList.Append.value := path;
      ex := issue.ExtensionList.Append;
      ex.url := 'http://hl7.org/fhir/tools#issue-source';
      ex.value := TFhirCode.Create;
      TFhirCode(ex.value).value := source;
      self.issueList.add(issue.link);
    finally
      issue.free;
    end;
  end;
  result := test;
end;

function TFHIROperationOutcomeHelper.rule(level: TFhirIssueSeverityEnum; source : String; typeCode : TFhirIssueTypeEnum; path: string; test: boolean; msg: string): boolean;
var
  issue : TFhirOperationOutcomeIssue;
  ex : TFhirExtension;
begin
  if not test then
  begin
    issue := TFhirOperationOutcomeIssue.Create;
    try
      issue.severity := level;
      issue.code := typeCode;
      issue.details := TFHIRCodeableConcept.Create;
      issue.details.text := msg;
      {$IFDEF STACK_DUMPS}
      issue.diagnostics := dumpStack;
      {$ENDIF}
      if (path <> '') then
        issue.locationList.Append.value := path;
      ex := issue.ExtensionList.Append;
      ex.url := 'http://hl7.org/fhir/tools#issue-source';
      ex.value := TFhirCode.Create;
      TFhirCode(ex.value).value := source;
      self.issueList.add(issue.link);
    finally
      issue.free;
    end;
  end;
  result := test;
end;

function TFHIROperationOutcomeHelper.warning(source : String; typeCode : TFhirIssueTypeEnum; path: string; test: boolean; msg: string): boolean;
var
  issue : TFhirOperationOutcomeIssue;
  ex : TFhirExtension;
begin
  if not test then
  begin
    issue := TFhirOperationOutcomeIssue.Create;
    try
      issue.severity := IssueSeverityWarning;
      issue.code := typeCode;
      issue.details := TFHIRCodeableConcept.Create;
      issue.details.text := msg;
      {$IFDEF STACK_DUMPS}
      issue.diagnostics := dumpStack;
      {$ENDIF}
      if (path <> '') then
        issue.locationList.Append.value := path;
      ex := issue.ExtensionList.Append;
      ex.url := 'http://hl7.org/fhir/tools#issue-source';
      ex.value := TFhirCode.Create;
      TFhirCode(ex.value).value := source;
      self.issueList.add(issue.link);
    finally
      issue.free;
    end;
  end;
  result := test;
end;

{ TFHIRElementHelper }

procedure TFHIRElementHelper.addExtension(url: String; t: TFHIRDataType);
var
  ex : TFhirExtension;
begin
  ex := self.ExtensionList.Append;
  ex.url := url;
  ex.value := t; // nolink here (done outside)
end;

procedure TFHIRElementHelper.addExtension(url, v: String);
begin
  addExtension(url, TFhirString.Create(v));
end;

procedure TFHIRElementHelper.addExtensionUri(url, v: String);
begin
  addExtension(url, TFhirUri.Create(v));
end;

function TFHIRElementHelper.forceExtension(url: String): TFHIRExtension;
begin
  result := getExtensionByUrl(url);
  if result = nil then
  begin
    result := self.ExtensionList.Append;
    result.url := url;
  end;
end;

function TFHIRElementHelper.addExtension(url: String): TFHIRExtension;
begin
  result := self.ExtensionList.Append;
  result.url := url;
end;

function TFHIRElementHelper.addExtension(ext: TFHIRExtension): TFHIRExtension;
begin
  self.extensionList.add(ext);
  result := ext;
end;

function TFHIRElementHelper.getExtension(url: String): Integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to self.ExtensionList.Count -1 do
    if self.ExtensionList[i].url = url then
      result := i;
end;

function TFHIRElementHelper.getExtensionBoolean(url: String): boolean;
var
  ndx : Integer;
begin
  ndx := getExtension(url);
  if (ndx = -1) then
    result := false
  else if (self.ExtensionList.Item(ndx).value is TFhirBoolean) then
    result := TFhirBoolean(self.ExtensionList.Item(ndx).value).value
  else if (self.ExtensionList.Item(ndx).value is TFhirString) then
    result := TFhirString(self.ExtensionList.Item(ndx).value).value <> ''
  else if (self.ExtensionList.Item(ndx).value is TFhirCode) then
    result := TFhirCode(self.ExtensionList.Item(ndx).value).value <> ''
  else if (self.ExtensionList.Item(ndx).value is TFhirUri) then
    result := TFhirUri(self.ExtensionList.Item(ndx).value).value <> ''
  else
    result := false;
end;

function TFHIRElementHelper.getExtensionByUrl(url: String): TFHIRExtension;
var
  i : integer;
begin
  result := nil;
  for i := 0 to self.ExtensionList.Count -1 do
    if self.ExtensionList[i].url = url then
      result := self.ExtensionList[i];
end;

function TFHIRElementHelper.getExtensionString(url: String): String;
var
  ndx : Integer;
begin
  ndx := getExtension(url);
  if (ndx = -1) then
    result := ''
  else if (self.ExtensionList.Item(ndx).value is TFhirString) then
    result := TFhirString(self.ExtensionList.Item(ndx).value).value
  else if (self.ExtensionList.Item(ndx).value is TFhirCode) then
    result := TFhirCode(self.ExtensionList.Item(ndx).value).value
  else if (self.ExtensionList.Item(ndx).value is TFhirUri) then
    result := TFhirUri(self.ExtensionList.Item(ndx).value).value
  else if (self.ExtensionList.Item(ndx).value is TFhirBoolean) then
    result := boolToStr(TFhirBoolean(self.ExtensionList.Item(ndx).value).value)
  else if (self.ExtensionList.Item(ndx).value is TFhirDateTime) then
    result := TFhirDateTime(self.ExtensionList.Item(ndx).value).value.ToXML
  else
    result := '';
end;

function TFHIRElementHelper.hasExtension(url: String): boolean;
begin
  result := getExtension(url) > -1;
end;

function TFHIRElementHelper.listExtensions(url: String): TFslList<TFhirExtension>;
var
  ext : TFHIRExtension;
begin
  result := TFslList<TFhirExtension>.Create;
  try
    for ext in extensionList do
      if ext.url = url then
        result.Add(ext.Link);
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRElementHelper.removeExtension(url: String);
var
  ndx : integer;
begin
  ndx := getExtension(url);
  while ndx > -1 do
  begin
    Self.ExtensionList.DeleteByIndex(ndx);
    ndx := getExtension(url);
  end;

end;

procedure TFHIRElementHelper.setExtension(url: String; t: TFHIRDataType);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (t <> nil) then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := t.link;
  end;
end;

procedure TFHIRElementHelper.setExtensionBoolean(url: String; value: boolean);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirBoolean.Create(value);
end;

procedure TFHIRElementHelper.setExtensionBoolean(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirBoolean.Create(value = 'true');
  end;
end;

procedure TFHIRElementHelper.setExtensionCode(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirCode.Create(value);
  end;
end;

procedure TFHIRElementHelper.setExtensionDate(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirDate.Create(TFslDateTime.fromXML(value));
  end;
end;

procedure TFHIRElementHelper.setExtensionDateTime(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirDateTime.Create(TFslDateTime.fromXML(value));
  end;
end;

procedure TFHIRElementHelper.setExtensionDecimal(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirDecimal.Create(value);
  end;
end;

procedure TFHIRElementHelper.setExtensionInteger(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirInteger.Create(value);
  end;
end;

procedure TFHIRElementHelper.setExtensionMarkdown(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirMarkdown.Create(value);
  end;
end;

procedure TFHIRElementHelper.setExtensionString(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirString.Create(value);
  end;
end;

procedure TFHIRElementHelper.setExtensionCanonical(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirCanonical.Create(value);
  end;
end;

procedure TFHIRElementHelper.setExtensionTime(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirTime.Create(value);
  end;
end;

procedure TFHIRElementHelper.setExtensionURI(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  if (value <> '') then
  begin
    ext := self.ExtensionList.Append;
    ext.url := url;
    ext.value := TFhirUri.Create(value);
  end;
end;

function TFHIRElementHelper.getExtensionCount(url: String): Integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to self.ExtensionList.Count - 1 do
    if self.ExtensionList[i].url = url then
      inc(result);
end;

function TFHIRElementHelper.getExtensionDateAsString(url: String): String;
var
  ndx : Integer;
begin
  ndx := getExtension(url);
  if (ndx = -1) then
    result := ''
  else if (self.ExtensionList.Item(ndx).value is TFhirDate) then
    result := TFhirDate(self.ExtensionList.Item(ndx).value).value.toString('yyyy-mm-dd')
  else if (self.ExtensionList.Item(ndx).value is TFhirDateTime) then
    result := TFhirDateTime(self.ExtensionList.Item(ndx).value).value.toString('yyyy-mm-dd')
  else if (self.ExtensionList.Item(ndx).value is TFhirInstant) then
    result := TFhirInstant(self.ExtensionList.Item(ndx).value).value.toString('yyyy-mm-dd')
  else
    result := '';
end;

{ TFHIRDomainResourceHelper }

function TFHIRDomainResourceHelper.addExtension(url: String; t: TFHIRDataType) : TFhirExtension;
begin
  result := self.ExtensionList.Append;
  result.url := url;
  result.value := t; // nolink here (done outside)
end;

function TFHIRDomainResourceHelper.addExtension(url, v: String) : TFhirExtension;
begin
  result := addExtension(url, TFhirString.Create(v));
end;

function TFHIRDomainResourceHelper.getExtension(url: String): Integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to self.ExtensionList.Count -1 do
    if self.ExtensionList[i].url = url then
      result := i;
end;

function TFHIRDomainResourceHelper.getExtension(url: String; index: integer): TFhirExtension;
var
  i, t : integer;
begin
  result := nil;
  t := 0;
  for i := 0 to self.ExtensionList.Count -1 do
    if self.ExtensionList[i].url = url then
    begin
      if t = index then
        exit(self.ExtensionList[i])
      else
        inc(t);
    end;
end;

function TFHIRDomainResourceHelper.getExtensionBool(url: String): boolean;
var
  ndx : Integer;
begin
  ndx := getExtension(url);
  if (ndx = -1) then
    result := false
  else if (self.ExtensionList.Item(ndx).value is TFHIRPrimitiveType) then
    result := (TFHIRPrimitiveType(self.ExtensionList.Item(ndx).value).primitiveValue = 'true')
      or (TFHIRPrimitiveType(self.ExtensionList.Item(ndx).value).primitiveValue = '1')
  else
    result := false;
end;

function TFHIRDomainResourceHelper.getExtensionByUrl( url: String): TFHIRExtension;
var
  i : integer;
begin
  result := nil;
  for i := 0 to self.ExtensionList.Count -1 do
    if self.ExtensionList[i].url = url then
      result := self.ExtensionList[i];
end;

function TFHIRDomainResourceHelper.getExtensionCount(url: String): Integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to self.ExtensionList.Count - 1 do
    if self.ExtensionList[i].url = url then
      inc(result);
end;

function TFHIRDomainResourceHelper.getExtensionString(url: String; index: integer): String;
var
  ndx : Integer;
begin
  result := '';
  for ndx := 0 to self.ExtensionList.Count - 1 do
  begin
    if self.ExtensionList[ndx].url = url then
    begin
      if index > 0 then
        dec(index)
      else
      begin
        if (self.ExtensionList.Item(ndx).value is TFhirString) then
          result := TFhirString(self.ExtensionList.Item(ndx).value).value
        else if (self.ExtensionList.Item(ndx).value is TFhirCode) then
          result := TFhirCode(self.ExtensionList.Item(ndx).value).value
        else if (self.ExtensionList.Item(ndx).value is TFhirUri) then
          result := TFhirUri(self.ExtensionList.Item(ndx).value).value
        else if (self.ExtensionList.Item(ndx).value is TFhirDecimal) then
          result := TFhirDecimal(self.ExtensionList.Item(ndx).value).value
        else
          result := '';
      end;
    end;
  end;
end;

function TFHIRDomainResourceHelper.getExtensionValue(url: String): TFHIRDataType;
var
  index : integer;
begin
  index := getExtension(url);
  if index = -1 then
    result := nil
  else
    result := extensionList[index].value;
end;

function TFHIRDomainResourceHelper.getExtensionString(url: String): String;
var
  ndx : Integer;
begin
  ndx := getExtension(url);
  if (ndx = -1) then
    result := ''
  else if (self.ExtensionList.Item(ndx).value is TFHIRPrimitiveType) then
    result := TFHIRPrimitiveType(self.ExtensionList.Item(ndx).value).primitiveValue
  else
    result := '';
end;

function TFHIRDomainResourceHelper.hasExtension(url: String): boolean;
begin
  result := getExtension(url) > -1;
end;

function TFHIRDomainResourceHelper.listExtensions(url: String): TFslList<TFhirExtension>;
var
  ext : TFHIRExtension;
begin
  result := TFslList<TFhirExtension>.Create;
  try
    for ext in extensionList do
      if ext.url = url then
        result.Add(ext.Link);
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRDomainResourceHelper.removeExtension(url: String; index: integer);
var
  i, t : integer;
begin
  t := 0;
  for i := 0 to extensionList.count - 1 do
  begin
    if extensionList[i].url = url then
    begin
      if t = index then
      begin
        extensionList.Remove(i);
        exit;
      end
      else
        inc(t);
    end;
  end;
end;

procedure TFHIRDomainResourceHelper.removeExtension(url: String);
var
  ndx : integer;
begin
  ndx := getExtension(url);
  while ndx > -1 do
  begin
    Self.ExtensionList.DeleteByIndex(ndx);
    ndx := getExtension(url);
  end;

end;

procedure TFHIRDomainResourceHelper.setExtension(url: String; t : TFHIRDataType);
var
  ext : TFHIRExtension;
begin
  ext := getExtensionByUrl(url);
  if ext = nil then
    addExtension(url, t)
  else
    ext.value := t;
end;

procedure TFHIRDomainResourceHelper.setExtensionBool(url: String; value : boolean);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirBoolean.Create(value);
end;

procedure TFHIRDomainResourceHelper.setExtensionCode(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirCode.Create(value);
end;

procedure TFHIRDomainResourceHelper.setExtensionString(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirString.Create(value);
end;

{ TFHIRCapabilityStatementHelper }

procedure TFHIRCapabilityStatementHelper.checkCompatible;
var
  res, code : String;
begin
  res := CODES_TFhirFHIRVersionEnum[fhirVersion];
  if res.Contains('-') then
    res := res.Substring(0, res.IndexOf('-'));
  res := res.Substring(0, res.LastIndexOf('.'));
  code := FHIR_GENERATED_VERSION.Substring(0, FHIR_GENERATED_VERSION.LastIndexOf('.'));
  if (code <> res) then
    raise EFHIRException.Create('Version Mismatch - this code is at version '+FHIR_GENERATED_VERSION+', but the server is version '+CODES_TFhirFHIRVersionEnum[fhirVersion]);
end;

function TFHIRCapabilityStatementHelper.hasFormat(fmt: TFHIRFormat): boolean;
var
  c : TFhirCode;
begin
  result := false;
  for c in formatList do
    if (c.value = CODES_TFHIRFormat[fmt]) or (c.value = MIMETYPES_TFHIRFormat[fmt]) then
      exit(true);
end;

function TFHIRCapabilityStatementHelper.GetInstantiates(url: String): boolean;
var
  u : TFhirUri;
begin
  result := false;
  for u in instantiatesList do
    if u.value = url then
      exit(true);
end;

function TFHIRCapabilityStatementHelper.rest(type_: TFhirResourceType): TFhirCapabilityStatementRestResource;
var
  i : integer;
  j : integer;
begin
  result := nil;
  for I := 0 to self.restlist.count - 1 do
    if self.restlist[i].mode = RestfulCapabilityModeServer then
      for j := 0 to self.restlist[i].resourceList.count - 1 do
        if CODES_TFhirResourceTypesEnum[self.restlist[i].resourceList[j].type_] = CODES_TFhirResourceType[type_] then
        begin
          result := self.restlist[i].resourceList[j];
          exit;
        end;
end;

procedure TFHIRCapabilityStatementHelper.SetHasFormat(fmt: TFHIRFormat; const Value: boolean);
var
  c : TFhirCode;
  i : integer;
begin
  for I := formatList.Count - 1 downto 0 do
  begin
    c := formatList[i];
    if (c.value = CODES_TFHIRFormat[fmt]) or (c.value = MIMETYPES_TFHIRFormat[fmt]) then
      formatList.DeleteByIndex(i);
  end;
  if value then
    formatList.Append.value := MIMETYPES_TFHIRFormat[fmt];
end;

procedure TFHIRCapabilityStatementHelper.SetInstantiates(url: String; const Value: boolean);
var
  u : TFhirUri;
  i : integer;
begin
  for I := instantiatesList.Count - 1 downto 0 do
  begin
    u := instantiatesList[i];
    if u.value = url then
      instantiatesList.DeleteByIndex(i);
  end;
  if value then
    instantiatesList.Append.value := url;
end;

function TFHIRCapabilityStatementHelper.supportsOperation(rName, opName: string): boolean;
var
  rest : TFHIRCapabilityStatementRest;
  op : TFHIRCapabilityStatementRestOperation;
begin
  result := false;
  for rest in self.restList do
  begin
    for op in rest.operationList do
      if op.name = opName then
        exit(true);
  end;
end;

function TFHIRCapabilityStatementHelper.supportsResource(name: String; commands: TFHIRCommandTypeSet): boolean;
var
  rest : TFHIRCapabilityStatementRest;
  res : TFHIRCapabilityStatementRestResource;
  cmd : TFHIRCapabilityStatementRestResourceInteraction;
  found : boolean;
  c : TFHIRCommandType;
begin
  result := false;
  for rest in self.restList do
  begin
    for res in rest.resourceList do
    begin
      if CODES_TFhirResourceTypesEnum[res.type_] = name then
      begin
        found := false;
        for c := low(TFHIRCommandType) to high(TFHIRCommandType) do
          if c in commands then
          begin
            for cmd in res.interactionList do
              if CODES_TFHIRTypeRestfulInteractionEnum[cmd.code] = CODES_TFHIRCommandType[c] then
                found := true;
          end;
        if found then
          exit(true);
      end;
    end;
  end;
end;

{ TFhirCapabilityStatementRestResourceHelper }

function TFhirConformanceRestResourceHelper.interaction(type_: TFHIRTypeRestfulInteractionEnum): TFhirCapabilityStatementRestResourceInteraction;
var
  i : integer;
begin
  result := nil;
  for i := 0 to self.interactionList.count - 1 do
    if (self.interactionList[i].code = type_) then
      result := self.interactionList[i];
end;

{ TFhirValueSetHelper }

{ TFHIRContactPointListHelper }

procedure TFHIRContactPointListHelper.setSystem(type_: TFHIRContactPointSystemEnum; value: String);
var
  i : integer;
  c : TFhirContactPoint;
begin
  for i := 0 to self.Count - 1 do
    if Item(i).system = type_ then
    begin
      Item(i).value := value;
      exit;
    end;
  c := self.Append;
  c.system := type_;
  c.value := value;
end;

function TFHIRContactPointListHelper.system(type_: TFHIRContactPointSystemEnum): String;
var
  i : integer;
begin
  result := '';
  for i := 0 to self.Count - 1 do
    if Item(i).system = type_ then
      result := Item(i).value;
end;

{ TFhirConceptMapHelper }

//function TFhirConceptMapHelper.conceptList: TFhirConceptMapElementList;
//begin
//  result := elementList;
//end;
//
function TFhirConceptMapHelper.context: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to useContextList.Count - 1 do
    result := result + gen(useContextList[i]);
end;

function TFhirConceptMapHelper.sourceDesc: String;
begin
  if source = nil then
    result := ''
  else if source is TFhirUri then
    result := TFhirUri(source).value
  else
    result := TFhirReference(source).reference
end;

function TFhirConceptMapHelper.targetDesc: String;
begin
  if target = nil then
    result := ''
  else if target is TFhirUri then
    result := TFhirUri(target).value
  else
    result := TFhirReference(target).reference
end;

{ TFHIRDomainResourceHelper }

function TFHIRDomainResourceHelper.addExtension(url: String): TFhirExtension;
begin
  result := self.ExtensionList.Append;
  result.url := url;
end;

procedure TFHIRDomainResourceHelper.checkNoModifiers(place, role: String; allowed : TArray<String> = nil);
begin
  if hasModifierExtensionList then
    raise EUnsafeOperation.Create('The element '+role+' has modifier exceptions that are unknown at '+place);
end;

procedure TFHIRDomainResourceHelper.collapseAllContained;
var
  i : integer;
begin
  i := 0;
  while (i < ContainedList.Count) do
  begin

    if containedList[i] is TFhirDomainResource then
    begin
      containedList.AddAll(TFhirDomainResource(containedList[i]).containedList);
      TFhirDomainResource(containedList[i]).containedList.Clear;
    end;
    inc(i);
  end;
end;

function TFHIRDomainResourceHelper.forceExtension(url: String): TFHIRExtension;
var
  i : integer;
begin
  for i := 0 to extensionList.Count - 1 do
    if extensionList[i].url = url then
      exit(extensionList[i]);
  result := addExtension(url);
end;

function TFHIRDomainResourceHelper.GetContained(id: String): TFhirResource;
var
  i : integer;
begin
  result := nil;
  for i := 0 to containedList.Count - 1 do
    if containedList[i].Id = id then
      result := containedList[i];
end;

{ TFhirProfileStructureSnapshotElementDefinitionTypeListHelper }

function TFhirProfileStructureSnapshotElementDefinitionTypeListHelper.summary: string;
var
  i : integer;
begin
  result := '';
  for i := 0 to Count - 1 do
    result := result + ','+Item(i).slicename;
  if result <> '' then
    result := result.Substring(1);
end;

{ TFHIRResourceHelper }

procedure TFHIRResourceHelper.checkNoImplicitRules(place, role: String);
begin
  if implicitRules <> '' then
    raise EUnsafeOperation.Create('The resource '+role+' has an unknown implicitRules tag at '+place);
end;

function TFHIRResourceHelper.GetXmlId: String;
begin
  result := id;
end;

procedure TFHIRResourceHelper.SetmlId(const Value: String);
begin
  id := value;
end;

function patSummary(pat : TFHIRPatient) : string;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    b.Append(HumanNamesAsText(pat.nameList));
    b.Append(' ');
    b.Append(CODES_TFhirAdministrativeGenderEnum[pat.gender]);
    b.Append(' ');
    b.Append(pat.birthDate.toString('c'));
    result := b.ToString;
  finally
    b.free;
  end;
end;

function groupSummary(grp : TFHIRGroup) : string;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    b.Append(grp.name);
    if (grp.code <> nil) then
    begin
      b.Append(' (');
      b.Append(gen(grp.code));
      b.Append(')');
    end;
    result := b.ToString;
  finally
    b.free;
  end;
end;

function TFHIRResourceHelper.textSummary: String;
begin
  case ResourceType of
    frtPatient: result := patSummary(self as TFHIRPatient);
    frtGroup: result := groupSummary(self as TFHIRGroup);
  else
    result := fhirType+'/'+id;
  end;
end;

{ TFHIRBundleHelper }

function TFHIRBundleHelper.AsReference: TFHIRDocumentReference;
var
  cmp : TFHIRComposition;
begin
  if type_ <> BundleTypeDocument then
    raise EFHIRException.Create('Cannot create a reference for something that is not a document');
  cmp := entryList[0].resource as TFhirComposition;
  result := TFHIRDocumentReference.Create;
  try
    result.identifierList.Add(identifier.Link);
    result.status := DocumentReferenceStatusCurrent;
    result.docStatus := cmp.status;
    result.identifierList.Add(cmp.identifier.Link);
    result.categoryList.AddAll(cmp.categoryList);
    result.type_ := cmp.type_.Link;
    result.subject := cmp.subject.Link;
    result.date := cmp.date;
    result.Link;
  finally
    result.free;
  end;
end;

class function TFHIRBundleHelper.Create(aType: TFhirBundleTypeEnum): TFhirBundle;
begin
  result := TFhirBundle.Create;
  result.type_ := aType;
end;

procedure TFHIRBundleHelper.deleteEntry(resource: TFHIRResource);
var
  i : integer;
begin
  for i := entryList.Count -1 downto 0 do
    if entryList[i].resource = resource then
      entrylist.DeleteByIndex(i);
end;

function TFHIRBundleHelper.findResource(ref: TFHIRReference): TFhirResource;
var
  be : TFhirBundleEntry;
  r : String;
begin
  r := ref.reference;
  result := nil;
  for be in entryList do
  begin
    if (be.resource <> nil) then
    begin
      if be.fullUrl = r then
        exit(be.resource);
      if be.resource.fhirType+'/'+be.resource.id = r then
        exit(be.resource);
    end;
  end;
end;

function TFHIRBundleHelper.generatePresentation: String;
var
  b : TStringBuilder;
  procedure addNarrative(br : boolean; n : TFhirNarrative);
  begin
    if br then
      b.Append('<br/>'+#13#10);
    b.Append(TFHIRXhtmlParser.compose(n.div_));
  end;
  procedure processSection(section : TFhirCompositionSection);
  var
    child : TFhirCompositionSection;
  begin
    b.Append('<br/>'+#13#10);
    if section.text <> nil then
      b.Append(TFHIRXhtmlParser.compose(section.text.div_));
    for child in section.sectionList do
      processSection(child);
  end;
var
  cmp : TFHIRComposition;
  sbj : TFhirDomainResource;
  section : TFhirCompositionSection;
begin
  if type_ = BundleTypeDocument then
  begin
    cmp := entryList[0].resource as TFhirComposition;
    b := TStringBuilder.Create;
    try
      // header
      b.append(
        '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
        '<head>'+#13#10+
        '  <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />'+#13#10+
        '  <title>'+cmp.title+'</title>'+#13#10+
        '</head>'+#13#10+
        '<body>'+#13#10);
      sbj := findResource(cmp.subject) as TFhirDomainResource;
      addNarrative(false, sbj.text);
      addNarrative(true, cmp.text);
      for section in cmp.sectionList do
        processSection(section);
      // foooter
      b.append(
        '</body>'+#13#10+
        '</html>'+#13#10);

      result := b.tostring;
    finally
      b.free;
    end;
  end
  else
    result := 'not a document';
end;

function TFHIRBundleHelper.GetLinks(s: string): String;
var
  i : integer;
begin
  result := '';
  for i := 0 to link_List.count -  1 do
    if link_List[i].relation = s then
    begin
      result := link_List[i].url;
      exit;
    end;
end;

procedure TFHIRBundleHelper.SetLinks(s: string; const Value: String);
var
  i : integer;
begin
  for i := 0 to link_List.count -  1 do
    if link_List[i].relation = s then
    begin
      link_List[i].url := value;
      exit;
    end;
  with link_List.Append do
  begin
    relation := s;
    url := value;
  end;
end;

procedure TFHIRBundleHelper.signRef(code: TSignatureType; whoRef: String; format: TFHIRFormat; cert : String);
var
  c : TFHIRCoding;
  dig : TDigitalSigner;
  src : TBytes;
begin
  // first. populate the signature
  signature := TFhirSignature.Create;
  c := signature.type_List.Append;
  c.system := 'urn:iso-astm:E1762-95:2013';
  c.code := CODES_TSignatureType[code];
  signature.when := TFslDateTime.makeUTC;
  signature.who := TFhirReference.Create(whoRef);
  signature.targetFormat := MIMETYPES_TFHIRFormat[format];
  case format of
    ffXml:
      begin
      signature.sigFormat := 'application/signature+xml';
      src := resourceToBytes(self, ffXml, OutputStyleCanonical);
      dig := TDigitalSigner.Create;
      try
        dig.PrivateKey := ansistring(cert);
        signature.data := dig.signDetached(src, '', sdXmlRSASha256, 'http://hl7.org/fhir/canonicalization/xml#bundle', true);
      finally
        dig.free;
      end;
      end;
    ffJson :
      begin
      signature.sigFormat := 'application/jose';
      src := resourceToBytes(self, ffJson, OutputStyleCanonical);
      signature.data := TJWTUtils.Sign_Hmac_RSA256(src, cert, '');
      end
  else
    raise EFHIRException.Create('The format '+CODES_TFHIRFormat[format]+' is not supported for digital signatures');
  end;
end;

function TFHIRBundleHelper.signRef2Provenance(code: TSignatureType; whoRef: String; format: TFHIRFormat; cert: String): TFhirProvenance;
var
  c : TFHIRCoding;
  dig : TDigitalSigner;
  src : TBytes;
  sig : TFHIRSignature;
  cmp : TFHIRComposition;
begin
  if type_ = BundleTypeDocument then
    cmp := entryList[0].resource as TFhirComposition
  else
    cmp := nil;

  result := TFhirProvenance.Create;
  try
    // first. populate the signature
    sig := result.signatureList.Append;
    c := sig.type_List.Append;
    c.system := 'urn:iso-astm:E1762-95:2013';
    c.code := CODES_TSignatureType[code];
    sig.when := TFslDateTime.makeUTC;
    sig.who := TFhirReference.Create(whoRef);
    case format of
      ffXml:
        begin
        sig.sigFormat := 'application/signature+xml';
        src := resourceToBytes(self, ffXml, OutputStyleCanonical);
        dig := TDigitalSigner.Create;
        try
          dig.PrivateKey := ansiString(cert);
          sig.data := dig.signDetached(src, '', sdXmlRSASha256, 'http://hl7.org/fhir/canonicalization/xml#bundle', true);
        finally
          dig.free;
        end;
        end;
      ffJson :
        begin
        sig.sigFormat := 'application/jose';
        src := resourceToBytes(self, ffJson, OutputStyleCanonical);
        sig.data := TJWTUtils.Sign_Hmac_RSA256(src, cert, '');
        end
    else
      raise EFHIRException.Create('The format '+CODES_TFHIRFormat[format]+' is not supported for digital signatures');
    end;
    // fill out other stuff on provenance
    result.occurred := TFhirPeriod.Create;
    if (cmp <> nil) then
    begin
      TFhirPeriod(result.occurred).start := cmp.date;
      TFhirPeriod(result.occurred).end_ := cmp.date;
    end;
    result.recorded := TFslDateTime.makeUTC;
    with result.agentList.Append do
    begin
      c := roleList.Append.codingList.Append;
      c.system := 'urn:iso-astm:E1762-95:2013';
      c.code := CODES_TSignatureType[code];
      who := TFhirReference.Create(whoRef);
    end;
    result.link;
  finally
    result.free;
  end;
end;

class function TFHIRBundleHelper.wrap(aType: TFhirBundleTypeEnum; resource: TFhirResource): TFhirBundle;
begin
  result := Create(atype);
  result.entryList.Append.resource := resource;
  result.id := NewGuidId;
end;

{ TFHIRCodingListHelper }

function TFHIRCodingListHelper.AddCoding(system, code, display: String) : TFHIRCoding;
var
  c : TFHIRCoding;
begin
  c := append;
  c.system := system;
  c.code := code;
  c.display := display;
  result := c;
end;

//function TFHIRCodingListHelper.AsHeader: String;
//begin
//  raise EFHIRException.Create('todo');
//end;
//
//procedure TFHIRCodingListHelper.CopyTags(meta: TFHIRMeta);
//begin
//  AddAll(meta.tagList);
//  AddAll(meta.securityList);
//  !
//end;
//
//function TFHIRCodingListHelper.getCoding(system, code: String): TFHIRCoding;
//begin
//  raise EFHIRException.Create('todo');
//end;
//
//function TFHIRCodingListHelper.hasCoding(system, code: String): boolean;
//begin
//  raise EFHIRException.Create('todo');
//end;
//
//procedure TFHIRCodingListHelper.CopyCodings(tags: TFHIRCodingList);
//begin
//  raise EFHIRException.Create('todo');
//end;
//
//function TFHIRCodingListHelper.json: TBytes;
//begin
//  SetLength(result, 0);
//end;
//
//procedure TFHIRCodingListHelper.WriteTags(meta: TFHIRMeta);
//begin
//  raise EFHIRException.Create('todo');
//end;
//
procedure TFHIRCodingListHelper.RemoveCoding(system, code: String);
var
  i : integer;
begin
 for i := Count - 1 downto 0 do
   if (Item(i).system = system) and (Item(i).code = code) then
     Remove(i);
end;

{ TFhirBundleLinkListHelper }

procedure TFhirBundleLinkListHelper.AddRelRef(rel, ref: String);
var
  link : TFhirBundleLink;
begin
  link := Append;
  link.relation := rel;
  link.url := ref;
end;

function TFhirBundleLinkListHelper.AsHeader: String;
var
  i : integer;
  bl : TFhirBundleLink;
begin
  result := '';
  for i := 0 to Count - 1 do
  begin
    bl := Item(i);
    if (result <> '') then
      result := result +', ';
    result := result + '<'+bl.url+'>;rel='+bl.relation;
  end;
end;

function TFhirBundleLinkListHelper.getMatch(rel: String): string;
var
  i : integer;
begin
  result := '';
  for i := 0 to count - 1 do
    if Item(i).relation = rel then
      result := Item(i).url;

end;

procedure TFhirBundleLinkListHelper.SetMatch(rel: String; const Value: string);
begin
  raise EFHIRException.Create('todo');
end;

function fullResourceUri(base: String; aType : TFhirResourceType; id : String) : String;
begin
  if (base = 'urn:oid:') then
  begin
    if isOid(id) then
      result := base+id
    else
      raise EFHIRException.Create('The resource id "'+'" has a base of "urn:oid:" but is not a valid OID');
  end
  else if (base = 'urn:uuid:') then
  begin
    if isGuid(id) then
      result := base+id
    else
      raise EFHIRException.Create('The resource id "'+id+'" has a base of "urn:uuid:" but is not a valid UUID');
  end
  else if not base.StartsWith('http://') and not base.StartsWith('https://')  then
    raise EFHIRException.Create('The resource base of "'+base+'" is not understood')
  else
    result := AppendForwardSlash(base)+CODES_TFhirResourceType[aType]+'/'+id;
end;

function fullResourceUri(base: String; ref : TFhirReference) : String; overload;
var
  url : String;
begin
  url := ref.reference;
  if url = '' then
    result := ''
  else if url.StartsWith('urn:oid:') or url.StartsWith('urn:uuid:') or url.StartsWith('http://') or url.StartsWith('https://') or url.StartsWith('resource:') then
    result := url
  else if not base.StartsWith('http://') and not base.StartsWith('https://')  then
    raise EFHIRException.Create('The resource base of "'+base+'" is not understood')
  else
    result := AppendForwardSlash(base)+url;
end;

function fullResourceUri(base: String; url : String) : String; overload;
begin
  if url = '' then
    result := ''
  else if url.StartsWith('urn:oid:') or url.StartsWith('urn:uuid:') or url.StartsWith('http://') or url.StartsWith('https://') then
    result := url
  else if not base.StartsWith('http://') and not base.StartsWith('https://')  then
    raise EFHIRException.Create('The resource base of "'+base+'" is not understood')
  else
    result := AppendForwardSlash(base)+url;
end;

{ TFhirParametersHelper }

procedure TFhirParametersHelper.AddParameter(name: String; value: TFHIRDataType);
var
  p : TFhirParametersParameter;
begin
  p := self.parameterList.Append;
  p.name := name;
  p.value := value;
end;

procedure TFhirParametersHelper.AddParameter(name: String; value: TFhirResource);
var
  p : TFhirParametersParameter;
begin
  p := self.parameterList.Append;
  p.name := name;
  p.resource := value;
end;

procedure TFhirParametersHelper.AddParameter(name: String; value: boolean);
var
  p : TFhirParametersParameter;
begin
  p := self.parameterList.Append;
  p.name := name;
  p.value := TFhirBoolean.Create(value);
end;

procedure TFhirParametersHelper.AddParameter(name, value: string);
var
  p : TFhirParametersParameter;
begin
  p := self.parameterList.Append;
  p.name := name;
  p.value := TFhirString.Create(value);
end;

function TFhirParametersHelper.AddParameter(name: String): TFhirParametersParameter;
begin
  result := self.parameterList.Append;
  result.name := name;
end;

function TFhirParametersHelper.GetBooleanParameter(name: String): boolean;
var
  v : TFHIRObject;
begin
  v := NamedParameter[name];
  if (v = nil) then
    result := false
  else if (v is TFhirBoolean) then
    result := (v as TFhirBoolean).value
  else if (v is TFHIRPrimitiveType) then
    result := StrToBoolDef(v.primitiveValue, false)
  else
  begin
    try
      raise EFHIRException.Create('Attempt to read "'+name+'" as a boolean, when it is a '+NamedParameter[name].FhirType);
    finally
      v.free;
    end;
  end;
end;

function TFhirParametersHelper.GetNamedParameter(name: String): TFHIRObject;
var
  i: Integer;
begin
  for i := 0 to parameterList.Count - 1 do
    if (parameterList[i].name = name) then
    begin
      if parameterList[i].valueElement <> nil then
        result := parameterList[i].valueElement
      else
        result := parameterList[i].resourceElement;
      exit;
    end;
  result := nil;
end;

function TFhirParametersHelper.GetParameterParameter(name: String): TFhirParametersParameter;
var
  i: Integer;
begin
  for i := 0 to parameterList.Count - 1 do
    if (parameterList[i].name = name) then
    begin
      result := parameterList[i];
      exit;
    end;
  result := nil;
end;

function TFhirParametersHelper.GetResourceParameter(name: String): TFHIRResource;
var
  i: Integer;
begin
  for i := 0 to parameterList.Count - 1 do
    if (parameterList[i].name = name) then
    begin
      if parameterList[i].resourceElement <> nil then
      begin
        result := parameterList[i].resourceElement;
        exit;
      end;
    end;
  result := nil;
end;

function TFhirParametersHelper.GetStringParameter(name: String): String;
var
  v : TFHIRObject;
begin
  v := NamedParameter[name];
  if (v = nil) then
    result := ''
  else if not (v is TFhirPrimitiveType) then
  begin
    try
      raise EFHIRException.Create('Attempt to read "'+name+'" as a string, when it is a '+NamedParameter[name].FhirType);
    finally
      v.free;
    end;
  end
  else
    result := (v as TFhirPrimitiveType).StringValue;
end;

function TFhirParametersHelper.hasParameter(name: String): Boolean;
var
  i: Integer;
begin
  for i := 0 to parameterList.Count - 1 do
    if (parameterList[i].name = name) then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

procedure TFhirParametersHelper.AddParameter(p: TFhirParametersParameter);
begin
  self.parameterList.Add(p);
end;

{ TFhirParametersParameterHelper }

procedure TFhirParametersParameterHelper.AddParameter(name: String; value: TFHIRDataType);
var
  p : TFhirParametersParameter;
begin
  p := self.partList.Append;
  p.name := name;
  p.value := value;
end;

procedure TFhirParametersParameterHelper.AddParameter(name: String; value: TFhirResource);
var
  p : TFhirParametersParameter;
begin
  p := self.partList.Append;
  p.name := name;
  p.resource := value;
end;

procedure TFhirParametersParameterHelper.AddParameter(name: String; value: boolean);
var
  p : TFhirParametersParameter;
begin
  p := self.partList.Append;
  p.name := name;
  p.value := TFhirBoolean.Create(value);
end;

procedure TFhirParametersParameterHelper.AddParameter(name, value: string);
var
  p : TFhirParametersParameter;
begin
  p := self.partList.Append;
  p.name := name;
  p.value := TFhirString.Create(value);
end;

function TFhirParametersParameterHelper.GetNamedParameter(name: String): TFHIRObject;
var
  i: Integer;
begin
  for i := 0 to partList.Count - 1 do
    if (partList[i].name = name) then
    begin
      if partList[i].valueElement <> nil then
        result := partList[i].valueElement
      else
        result := partList[i].resourceElement;
      exit;
    end;
  result := nil;
end;

function TFhirParametersParameterHelper.GetParameterParameter(name: String): TFhirParametersParameter;
var
  i: Integer;
begin
  for i := 0 to partList.Count - 1 do
    if (partList[i].name = name) then
    begin
      result := partList[i];
      exit;
    end;
  result := nil;
end;

function TFhirParametersParameterHelper.GetStringParameter(name: String): String;
begin
  result := (NamedParameter[name] as TFhirPrimitiveType).StringValue;
end;

function TFhirParametersParameterHelper.hasParameter(name: String): Boolean;
var
  i: Integer;
begin
  for i := 0 to partList.Count - 1 do
    if (partList[i].name = name) then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TFhirParametersParameterHelper.AddParameter(name: String): TFhirParametersParameter;
begin
  result := self.partList.Append;
  result.name := name;
end;

procedure TFhirParametersParameterHelper.AddParameter(p: TFhirParametersParameter);
begin
  self.partList.Add(p);
end;

procedure TFhirParametersParameterHelper.AddParameter(name: String; value: integer);
var
  p : TFhirParametersParameter;
begin
  p := self.partList.Append;
  p.name := name;
  p.value := TFhirInteger.Create(inttostr(value));
end;

{ TFHIRCodeableConceptHelper }

constructor TFHIRCodeableConceptHelper.Create(system, code: String);
begin
  Create;
  CodingList.Add(TFHIRCoding.Create(system, code));
end;

function TFHIRCodeableConceptHelper.fromSystem(System: String; required: boolean): String;
var
  c : TFHIRCoding;
begin
  result := '';
  for c in codingList do
  begin
    if c.system = system then
    begin
      result := c.code;
      break;
    end;
  end;
  if required and (result = '') then
    raise EFHIRException.Create('Unable to find code in '+system);
end;

function TFHIRCodeableConceptHelper.fromSystem(Systems: TArray<String>; required: boolean): String;
var
  c : TFHIRCoding;
begin
  result := '';
  for c in codingList do
  begin
    if StringArrayExistsSensitive(systems, c.system) then
    begin
      result := c.code;
      break;
    end;
  end;
  if required and (result = '') then
    raise EFHIRException.Create('Unable to find code in '+StringArrayToString(systems));
end;

procedure TFHIRCodeableConceptHelper.addCoding(systemUri, version, code, display: String);
var
  c : TFhirCoding;
begin
  c := TFHIRCoding.Create;
  try
    c.system := systemUri;
    c.version := version;
    c.code := code;
    c.display := display;
    codingList.add(c.link);
  finally
    c.free;
  end;
end;

function TFHIRCodeableConceptHelper.hasCode(System, Code: String): boolean;
var
  i : integer;
begin
  result :=  false;
  if self <> nil then
    for i := 0 to codingList.Count - 1 do
      if (codingList[i].system = system) and (codingList[i].code = code) then
      begin
        result := true;
        break;
      end;
end;

function TFHIRCodeableConceptHelper.hasCode(System, Version, Code: String): boolean;
var
  i : integer;
begin
  result :=  false;
  if self <> nil then
    for i := 0 to codingList.Count - 1 do
      if (codingList[i].system = system) and (codingList[i].version = version) and (codingList[i].code = code) then
      begin
        result := true;
        break;
      end;
end;

function TFHIRCodeableConceptHelper.hasCoding: boolean;
begin
  result := CodingList.Count > 0;
end;

{ TFhirResourceMetaHelper }

procedure TFhirResourceMetaHelper.addProfile(url: String);
begin
  if not hasProfile(url) then
    profileList.Add(TFHIRUri.Create(url));
end;

function TFhirResourceMetaHelper.addTag(system, code, display: String): TFhirCoding;
var
  c : TFhirCoding;
begin
  if not hasTag(system, code) then
  begin
    c := tagList.Append;
    c.system := system;
    c.code := code;
    c.display := display;
    result := c;
  end
  else
    result := getTag(system, code);
end;

procedure TFhirResourceMetaHelper.dropProfile(url: String);
var
  i : integer;
begin
  for I := ProfileList.Count - 1 downto 0 do
    if ProfileList[i].value = url then
      ProfileList.Remove(i);
end;

function TFhirResourceMetaHelper.HasTag(system, code: String): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to taglist.Count - 1 do
    result := result or (taglist[i].system = system) and (taglist[i].code = code);
end;

function TFhirResourceMetaHelper.GetTag(system, code: String): TFhirCoding;
var
  i : integer;
begin
  result := nil;
  for i := 0 to taglist.Count - 1 do
    if (taglist[i].system = system) and (taglist[i].code = code) then
    begin
      result := taglist[i];
      exit;
    end;
end;

function TFhirResourceMetaHelper.hasProfile(url: String): boolean;
var
  u : TFhirUri;
begin
  result := false;
  for u in profileList do
    if u.value = url then
      exit(true);
end;

function TFhirResourceMetaHelper.removeTag(system, code : String): boolean;
var
  i : integer;
  c : TFhirCoding;
begin
  result := false;
  for i := TagList.Count -1 downto 0 do
  begin
    c := TagList[i];
    if (c.system = system) and (c.code = code) then
    begin
      result := true;
      TagList.DeleteByIndex(i);
    end;
  end;
end;

function TFhirResourceMetaHelper.removeLabel(system, code : String): boolean;
var
  i : integer;
  c : TFhirCoding;
begin
  result := false;
  for i := securityList.Count -1 downto 0 do
  begin
    c := securityList[i];
    if (c.system = system) and (c.code = code) then
    begin
      result := true;
      securityList.DeleteByIndex(i);
    end;
  end;
end;

function TFHIRElementHelper.getExtensionString(url: String; index: integer): String;
var
  ndx : Integer;
begin
  result := '';
  for ndx := 0 to self.ExtensionList.Count - 1 do
  begin
    if self.ExtensionList[ndx].url = url then
    begin
      if index > 0 then
        dec(index)
      else
      begin
        if (self.ExtensionList.Item(ndx).value is TFhirString) then
          result := TFhirString(self.ExtensionList.Item(ndx).value).value
        else if (self.ExtensionList.Item(ndx).value is TFhirCode) then
          result := TFhirCode(self.ExtensionList.Item(ndx).value).value
        else if (self.ExtensionList.Item(ndx).value is TFhirUri) then
          result := TFhirUri(self.ExtensionList.Item(ndx).value).value
        else
          result := '';
      end;
    end;
  end;
end;

{ TFhirContactDetailListHelper }

procedure TFhirContactDetailListHelper.setSystem(type_: TFHIRContactPointSystemEnum; value: String);
var
  i : integer;
  c : TFhirContactPoint;
begin
  if Count = 0 then
    Append;
  for i := 0 to Item(0).telecomList.Count - 1 do
    if Item(0).telecomList[i].system = type_ then
    begin
      Item(0).telecomList[i].value := value;
      exit;
    end;
  c := Item(0).telecomList.Append;
  c.system := type_;
  c.value := value;
end;

function TFhirContactDetailListHelper.system(type_: TFHIRContactPointSystemEnum): String;
var
  i, j : integer;
begin
  result := '';
  for j := 0 to Count - 1 do
    for i := 0 to Item(j).telecomList.Count - 1 do
     if Item(j).telecomList[i].system = type_ then
       result := Item(j).telecomList[i].value;
end;

{ TFhirValueSetHelper }

function TFhirValueSetHelper.context: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to useContextList.Count - 1 do
    result := result + gen(useContextList[i]);
end;

function TFhirValueSetHelper.source: string;
var
  ts : TStringList;
  comp : TFhirValueSetComposeInclude;
begin
  ts := TStringList.Create;
  try
    ts.sorted := true;

    if (compose <> nil) then
      for comp in compose.includeList do
      begin
        if comp.system <> '' then
        begin
          if ts.IndexOf(comp.system) = -1 then
          begin
            ts.add(csName(comp.system));
          end;
        end;
      end;
    result := ts.commaText;
  finally
    ts.free;
  end;
end;

function findContainsInList(list : TFhirValueSetExpansionContainsList; systemUri, version, code: String): TFHIRValueSetExpansionContains;
var
  cc, t : TFhirValueSetExpansionContains;
begin
  for cc in list do
  begin
    if (systemUri = cc.system) and (code = cc.code) and ((version = '') or (version = cc.version)) then
      exit(cc);
    if (cc.hasContainsList) then
    begin
      t := findContainsInList(cc.containsList, systemUri, version, code);
      if (t <> nil) then
        exit(t);
    end;
  end;
  result := nil;
end;

function TFhirValueSetHelper.findContains(systemUri, version, code: String): TFHIRValueSetExpansionContains;
begin
  if Expansion = nil then
    result := nil
  else
    result := findContainsInList(Expansion.containsList, systemUri, version, code);

end;
function gen(t : TFHIRDataType):String;
begin
  if (t = nil) then
    result := ''
  else if t is TFhirCodeableConcept then
    result := gen(TFhirCodeableConcept(t))
  else if t is TFhirCoding then
    result := gen(TFhirCoding(t))
  else if t is TFhirString then
    result := TFhirString(t).value
  else if t is TFhirEnum then
    result := TFhirEnum(t).value
  else if t is TFhirHumanName then
    result := HumanNameAsText(TFhirHumanName(t))
  else if t is TFhirReference then
    result := gen(TFhirReference(t))
  else if t is TFhirIdentifier then
    result := gen(TFhirIdentifier(t))
  else if t is TFhirAnnotation then
    result := gen(TFhirAnnotation(t))
  else if t is TFhirAttachment then
    result := gen(TFhirAttachment(t))
  else if t is TFhirQuantity then
    result := gen(TFhirQuantity(t))
  else if t is TFhirRange then
    result := gen(TFhirRange(t))
  else if t is TFhirPeriod then
    result := gen(TFhirPeriod(t))
  else if t is TFhirRatio then
    result := gen(TFhirRatio(t))
  else if t is TFhirSampledData then
    result := gen(TFhirSampledData(t))
  else if t is TFhirSignature then
    result := gen(TFhirSignature(t))
  else if t is TFhirAddress then
    result := gen(TFhirAddress(t))
  else if t is TFhirContactPoint then
    result := gen(TFhirContactPoint(t))
  else if t is TFhirTiming then
    result := gen(TFhirTiming(t))
  else if t is TFhirDate then
    result := gen(TFhirDate(t))
  else if t is TFhirUsageContext then
    result := gen(TFhirUsageContext(t))
  else if t is TFhirBoolean then
    if TFhirBoolean(t).value then
      result := 'true'
    else
      result := 'false'
  else if t.isPrimitive then
    result := t.primitiveValue
  else
    raise EFHIRException.Create('Type '+t.className+' not handled yet');
end;

function getChildMap(profile : TFHIRStructureDefinition; element : TFHIRElementDefinition) : TFHIRElementDefinitionList; overload;
var
  e : TFHIRElementDefinition;
  index : integer;
begin
  if (element.contentReference <> '') then
  begin
    for e in profile.snapshot.elementList do
      if (element.ContentReference = '#'+e.id) then
        exit(getChildMap(profile, e));
      raise EDefinitionException.Create('Unable to resolve name reference '+element.contentReference+' at path '+element.path);
  end
  else
  begin
    result := TFHIRElementDefinitionList.Create;
    for index := profile.snapshot.elementList.indexOf(element) + 1 to profile.snapshot.elementList.count - 1 do
    begin
      e := profile.snapshot.elementList[index];
      if (e.path.startsWith(element.path + '.')) then
      begin
        // We only want direct children, not all descendants
        if (not e.path.substring(element.path.length+1).contains('.')) then
          result.add(e.link);
      end
      else
        break;
    end;
  end;
end;

{*
 * Given a Structure, navigate to the element given by the path and return the direct children of that element
 *
 * @param structure The structure to navigate into
 * @param path The path of the element within the structure to get the children for
 * @return A Map containing the name of the element child (not the path) and the child itself (an Element)
 * @throws Exception
 *}
function getChildMap(profile : TFHIRStructureDefinition; name, path, nameReference : String) : TFHIRElementDefinitionList;
var
   found : boolean;
   e : TFhirElementDefinition;
   p, tail : String;
   inScope : boolean;
begin
  result := TFHIRElementDefinitionList.Create();
  try
    // if we have a name reference, we have to find it, and iterate it's children
    if (nameReference <> '') then
    begin
      found := false;
      for e in profile.Snapshot.ElementList do
      begin
        if (nameReference = '#'+e.id) then
        begin
          found := true;
          path := e.Path;
        end;
      end;
      if (not found) then
        raise EFHIRException.Create('Unable to resolve name reference '+nameReference+' at path '+path);
    end;

    inScope := false;
    for e in profile.Snapshot.ElementList do
    begin
      p := e.Path;

      if (path <> '') and (e.ContentReference <> '') and (path.startsWith(p)) then
      begin
        {* The path we are navigating to is on or below this element, but the element defers its definition to another named part of the
         * structure.
         *}
        if (path.length > p.length) then
        begin
          // The path navigates further into the referenced element, so go ahead along the path over there
          result.free;
          result := getChildMap(profile, name, e.ContentReference+'.'+path.substring(p.length+1), '').link;
          exit;
        end
        else
        begin
          // The path we are looking for is actually this element, but since it defers it definition, go get the referenced element
          result.free;
          result := getChildMap(profile, name, e.ContentReference, '').Link;
          exit;
        end;
      end
      else
      begin
        if (p = path) then
        begin
          if (name = '') or (e.slicename = name) then
            inscope := true
          else
            inscope := false;
        end;

        if inScope and (p.startsWith(path+'.')) then
        begin
          // The path of the element is a child of the path we're looking for (i.e. the parent),
          // so add this element to the result.
          tail := p.substring(path.length+1);
          // Only add direct children, not any deeper paths
          if (not tail.contains('.')) then
            result.add(e.Link);
        end
        else if (p.Length < path.Length) then
          inScope := false;
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function compareValues(e1, e2 : TFHIRObjectList; allowNull : boolean) : boolean;
var
  i : integer;
begin
  if (e1 = nil) and (e2 = nil) and (allowNull) then
    result := true
  else if (e1 = nil) or (e2 = nil) then
    result := false
  else if (e1.count <> e2.count) then
    result := false
  else
  begin
    result := true;
    for i := 0 to e1.count - 1 do
      if (not compareValues(e1.get(i) as TFHIRPrimitiveType, e2.get(i) as TFHIRPrimitiveType, allowNull)) then
        result := false;
  end;
end;

function compareValues(e1, e2 : TFHIRPrimitiveType; allowNull : boolean) : boolean;
begin
  if (e1 = nil) and (e2 = nil) and (allowNull) then
    result := true
  else if (e1 = nil) or (e2 = nil) then
    result := false
  else
    result := e1.equals(e2);
end;

function compareValues(e1, e2 : TFHIRXhtmlNode; allowNull : boolean) : boolean; overload;
begin
  raise EFHIRTodo.Create('compareValues');
end;

{ TFHIRStringListHelper }

procedure TFHIRStringListHelper.add(s: String);
begin
  add(TFhirString.Create(s));
end;

function TFHIRStringListHelper.hasValue(value: String): boolean;
var
  v : TFhirString;
begin
  result := false;
  for v in Self do
    if (v.value = value) then
      result := true;
end;

function TFHIRStringListHelper.summary: String;
var
  b : TStringBuilder;
  f : boolean;
  v : TFHIRString;
begin
  f := true;
  b := TStringBuilder.Create;
  try
    for v in self do
    begin
      if (f) then
        f := false
      else
        b.Append(', ');
      b.Append(v.value);
    end;
    result := b.ToString;
  finally
    b.Free
  end;
end;

{ TFhirOperationOutcomeIssueHelper }

constructor TFhirOperationOutcomeIssueHelper.Create(Severity: TFhirIssueSeverityEnum; Code: TFhirIssueTypeEnum; Diagnostics, location: String);
begin
  Create;
  self.severity := Severity;
  self.code := code;
  self.diagnostics := Diagnostics;
  self.locationList.Add(location);
end;

function CreateResourceByName(name : String) : TFhirResource;
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TFhirResourceType, name);
  if i = -1 then
    raise EFHIRException.Create('Unknown resource type '+name);
  result := CLASSES_TFhirResourceType[TFhirResourceType(i)].Create;
end;

function CreateTypeByName(name : String) : TFhirElement;
begin
  if name = 'boolean' then
    result := TFHIRboolean.Create(false)
  else if name = 'integer' then
    result := TFHIRinteger.Create('1')
  else if name = 'decimal' then
    result := TFHIRdecimal.Create('1.0')
  else if name = 'base64Binary' then
    result := TFHIRbase64Binary.Create(AnsiStringAsBytes('%test content%'))
  else if name = 'instant' then
    result := TFHIRinstant.Create(TFslDateTime.makeLocal)
  else if name = 'string' then
    result := TFHIRstring.Create('%string%')
  else if name = 'uri' then
    result := TFHIRuri.Create('http://uri...')
  else if name = 'date' then
    result := TFHIRdate.Create(TFslDateTime.makeToday)
  else if name = 'dateTime' then
    result := TFHIRdateTime.Create(TFslDateTime.makeLocal)
  else if name = 'time' then
    result := TFHIRtime.Create('00:10:00')
  else if name = 'code' then
    result := TFHIRcode.Create('%code%')
  else if name = 'oid' then
    result := TFHIRoid.Create('urn:oid:0.1.2.3')
  else if name = 'id' then
    result := TFHIRid.Create('%id%')
  else if name = 'unsignedInt' then
    result := TFHIRunsignedInt.Create('0')
  else if name = 'positiveInt' then
    result := TFHIRpositiveInt.Create('1')
  else if name = 'markdown' then
    result := TFHIRmarkdown.Create('*markdown*')
  else if name = 'Annotation' then
    result := TFHIRAnnotation.create
  else if name = 'Attachment' then
    result := TFHIRAttachment.create
  else if name = 'Identifier' then
    result := TFHIRIdentifier.create
  else if name = 'CodeableConcept' then
    result := TFHIRCodeableConcept.create
  else if name = 'Coding' then
    result := TFHIRCoding.create
  else if name = 'Quantity' then
    result := TFHIRQuantity.create
  else if name = 'Range' then
    result := TFHIRRange.create
  else if name = 'Period' then
    result := TFHIRPeriod.create
  else if name = 'Ratio' then
    result := TFHIRRatio.create
  else if name = 'SampledData' then
    result := TFHIRSampledData.create
  else if name = 'Signature' then
    result := TFHIRSignature.create
  else if name = 'HumanName' then
    result := TFHIRHumanName.create
  else if name = 'Address' then
    result := TFHIRAddress.create
  else if name = 'ContactPoint' then
    result := TFHIRContactPoint.create
  else if name = 'Timing' then
    result := TFHIRTiming.create
  else if name = 'Reference' then
    result := TFHIRReference.create
  else if name = 'Narrative' then
    result := TFHIRNarrative.create
  else if name = 'Money' then
    result := TFHIRMoney.create
  else if name = 'Meta' then
    result := TFHIRMeta.create
  else if name = 'xhtml' then
    result := nil
  else
    raise EFHIRException.Create('Unknown type: '+name);
end;

function CreateBasicChildren(element : TFhirElement; exCoding : TFHIRCoding) : TFhirElement;
begin
  result := element;
  if element.FhirType = 'Annotation' then
  begin
    TFHIRAnnotation(element).author := CreateBasicChildren(TFhirReference.Create, nil) as TFhirReference;
    TFHIRAnnotation(element).time := TFslDateTime.makeLocal;
    TFHIRAnnotation(element).text := 'annotation text';
  end
  else if element.FhirType = 'Attachment' then
  begin
    TFHIRAttachment(element).contentType := 'text/plain';
    TFHIRAttachment(element).language := 'en-US';
    TFHIRAttachment(element).url := 'http://somewhere';
  end
  else if element.FhirType = 'Identifier' then
  begin
    TFHIRIdentifier(element).use := IdentifierUseUsual;
    TFHIRIdentifier(element).system := 'http://unique.namepace/details';
    TFHIRIdentifier(element).value := 'unique-id';
  end
  else if element.FhirType = 'CodeableConcept' then
  begin
    TFHIRCodeableConcept(element).text := 'text representation';
    if exCoding <> nil then
      TFHIRCodeableConcept(element).codingList.add(exCoding.Link)
    else
      CreateBasicChildren(TFHIRCodeableConcept(element).codingList.Append, nil);
  end
  else if element.FhirType = 'Coding' then
  begin
    if exCoding <> nil then
    begin
      TFHIRCoding(element).system := exCoding.system;
      TFHIRCoding(element).code := exCoding.code;
      TFHIRCoding(element).display := exCoding.display;
    end
    else
    begin
      TFHIRCoding(element).system := 'http://system.id';
      TFHIRCoding(element).code := 'code';
      TFHIRCoding(element).display := 'display';
    end;
  end
  else if element.FhirType = 'Quantity' then
  begin
    TFHIRQuantity(element).value := '5.4';
    if exCoding <> nil then
    begin
      TFHIRQuantity(element).system := exCoding.system;
      TFHIRQuantity(element).code := exCoding.code;
      TFHIRQuantity(element).unit_ := exCoding.display;
    end
    else
    begin
      TFHIRQuantity(element).unit_ := 'mg/mL';
      TFHIRQuantity(element).system := URI_UCUM;
      TFHIRQuantity(element).code := 'mg/mL';
    end;
  end
  else if element.FhirType = 'Range' then
  begin
    TFHIRRange(element).low := CreateBasicChildren(TFHIRQuantity.Create, exCoding) as TFhirQuantity;
    TFHIRRange(element).low.value := '4.8';
    TFHIRRange(element).high := CreateBasicChildren(TFHIRQuantity.Create, exCoding) as TFhirQuantity;
  end
  else if element.FhirType = 'Period' then
  begin
    TFHIRPeriod(element).start := TFslDateTime.makeLocal;
    TFHIRPeriod(element).end_ := TFslDateTime.makeLocal;
  end
  else if element.FhirType = 'Ratio' then
  begin
    TFHIRRatio(element).numerator := TFhirQuantity.Create;
    TFHIRRatio(element).numerator.value := '5.4';
    TFHIRRatio(element).numerator.unit_ := '$';
    TFHIRRatio(element).denominator := TFhirQuantity.Create;
    TFHIRRatio(element).denominator.value := '1';
    TFHIRRatio(element).denominator.unit_ := 'kg';
  end
  else if element.FhirType = 'Signature' then
  begin
    if exCoding <> nil then
      TFHIRSignature(element).type_List.Add(exCoding.Link)
    else
    begin
      TFHIRSignature(element).type_List.Append;
      TFHIRSignature(element).type_List[0].system := 'http://hl7.org/fhir/valueset-signature-type';
      TFHIRSignature(element).type_List[0].code := '1.2.840.10065.1.12.1.1';
      TFHIRSignature(element).type_List[0].display := 'AuthorID';
    end;
    TFHIRSignature(element).when := TFslDateTime.makeUTC;
    TFHIRSignature(element).who := CreateBasicChildren(TFhirReference.Create, nil) as TFhirReference;
    TFHIRSignature(element).sigFormat := 'application/signature+xml';
    TFHIRSignature(element).data := AnsiStringAsBytes('signature content');
  end
  else if element.FhirType = 'HumanName' then
  begin
    TFHIRHumanName(element).use := NameUseUsual;
    TFHIRHumanName(element).text := 'prefix given family';
    TFHIRHumanName(element).givenList.add('%given%');
    TFHIRHumanName(element).family := '%family%';
    TFHIRHumanName(element).prefixList.add('%prefix%');
  end
  else if element.FhirType = 'Address' then
  begin
    TFHIRAddress(element).use := AddressUseHome;
    TFHIRAddress(element).text := '13 Boring St, Erewhon, 5555 (New Zealand)';
    TFHIRAddress(element).lineList.add('13 Boring St');
    TFHIRAddress(element).city := 'Erewhon';
    TFHIRAddress(element).postalCode := '5555';
    TFHIRAddress(element).country := 'New Zealand';
  end
  else if element.FhirType = 'ContactPoint' then
  begin
    TFHIRContactPoint(element).system := ContactPointSystemPhone;
    TFHIRContactPoint(element).value := '%555-555-5555%';
    TFHIRContactPoint(element).use := ContactPointUseWork;
  end
  else if element.FhirType = 'Timing' then
  begin
    TFHIRTiming(element).eventList.Append.value := TFslDateTime.makeLocal;
    TFHIRTiming(element).repeat_ := TFhirTimingRepeat.Create;
    TFHIRTiming(element).repeat_.duration := '1';
// ggtodo    TFHIRTiming(element).repeat_.durationUnit := UnitsOfTimeH;
    TFHIRTiming(element).repeat_.frequency := '3';
    TFHIRTiming(element).repeat_.period := '21';
// ggtodo        TFHIRTiming(element).repeat_.periodUnit := UnitsOfTimeD;
  end
  else if element.FhirType = 'Reference' then
  begin
    TFHIRReference(element).display := '%test description%';
    TFHIRReference(element).reference := '%Type/id%';
  end
  else if element.FhirType = 'Narrative' then
  begin
    TFhirNarrative(element).status := NarrativeStatusAdditional;
    TFhirNarrative(element).div_ := TFHIRXhtmlParser.Parse(nil, xppAllow, [], '<div xmlns="http://www.w3.org/1999/xhtml"><p>%Some xhtml content%</p></div>');
  end
  else if element.FhirType = 'Meta' then
  begin
    TFHIRMeta(element).lastUpdated := TFslDateTime.makeUTC;
  end
  else if (exCoding <> nil) and (element is TFHIRCode) then
    TFHIRCode(element).value := exCoding.code
  else if (exCoding <> nil) and (element is TFHIREnum) then
    TFHIREnum(element).value := exCoding.code
end;

function asCode(obj : TFHIRObject) : TFHIRCode;
begin
  if obj is TFHIRCode then
    result := obj as TFHIRCode
  else if obj is TFHIREnum then
  begin
    result := TFHIRCode.Create(TFHIREnum(obj).value);
    obj.free;
  end
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRCode.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRCode\"')
  end;
end;

function asMarkdown(obj : TFHIRObject) : TFHIRMarkdown;
begin
  if obj is TFHIRMarkdown then
    result := obj as TFHIRMarkdown
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRMarkdown.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRMarkdown.Create(TFHIRObject(obj).primitiveValue);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRMarkdown\"')
  end;
end;

function asXhtml(obj : TFHIRObject) : TFhirXHtmlNode;
begin
  if obj is TFhirXHtmlNode then
    result := obj as TFhirXHtmlNode
  else if obj.isPrimitive then
  begin
    result := TFHIRXhtmlParser.parse(nil, xppDrop, [], obj.primitiveValue);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRMarkdown\"')
  end;
end;

function asXhtmlNode(obj : TFHIRObject) : TFhirXHtmlNode;
begin
  result := asXhtml(obj);
end;

function asString(obj : TFHIRObject) : TFHIRString;
begin
  if obj is TFHIRString then
    result := obj as TFHIRString
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRString.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRString.Create(TFHIRObject(obj).primitiveValue);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRString\"')
  end;
end;

function asId(obj : TFHIRObject) : TFHIRId;
begin
  if obj is TFHIRId then
    result := obj as TFHIRId
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRId.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRId\"')
  end;
end;

function asUri(obj : TFHIRObject) : TFHIRUri;
begin
  if obj is TFHIRUri then
    result := obj as TFHIRUri
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUri.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRUri.Create(TFHIRObject(obj).primitiveValue);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRUri\"')
  end;
end;

function asDateTime(obj : TFHIRObject) : TFHIRDateTime;
begin
  if obj is TFHIRDateTime then
    result := obj as TFHIRDateTime
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDateTime.Create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRDateTime\"')
  end;
end;

function asUnsignedInt(obj : TFHIRObject) : TFHIRUnsignedInt;
begin
  if obj is TFHIRUnsignedInt then
    result := obj as TFHIRUnsignedInt
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUnsignedInt.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRUnsignedInt\"')
  end;
end;

function asPositiveInt(obj : TFHIRObject) : TFHIRPositiveInt;
begin
  if obj is TFHIRPositiveInt then
    result := obj as TFHIRPositiveInt
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRPositiveInt.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRPositiveInt\"')
  end;
end;

function asInstant(obj : TFHIRObject) : TFHIRInstant;
begin
  if obj is TFHIRInstant then
    result := obj as TFHIRInstant
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRInstant.Create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRInstant\"')
  end;
end;

function asBoolean(obj : TFHIRObject) : TFHIRBoolean;
begin
  if obj is TFHIRBoolean then
    result := obj as TFHIRBoolean
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRBoolean.Create(TFHIRMMElement(obj).value = 'true');
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRBoolean\"')
  end;
end;

function asBase64Binary(obj : TFHIRObject) : TFHIRBase64Binary;
begin
  if obj is TFHIRBase64Binary then
    result := obj as TFHIRBase64Binary
//  else if obj is TFHIRMMElement then
//  begin
//    result := TFHIRBase64Binary.Create(TFHIRMMElement(obj).value);
//    obj.free;
//  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRBase64Binary\"')
  end;
end;

function asDate(obj : TFHIRObject) : TFHIRDate;
begin
  if obj is TFHIRDate then
    result := obj as TFHIRDate
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDate.Create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRDate\"')
  end;
end;

function asDecimal(obj : TFHIRObject) : TFHIRDecimal;
begin
  if obj is TFHIRDecimal then
    result := obj as TFHIRDecimal
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDecimal.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRDecimal\"')
  end;
end;

function asTime(obj : TFHIRObject) : TFHIRTime;
begin
  if obj is TFHIRTime then
    result := obj as TFHIRTime
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRTime.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRTime\"')
  end;
end;

function asOid(obj : TFHIRObject) : TFHIROid;
begin
  if obj is TFHIROid then
    result := obj as TFHIROid
  else if obj is TFHIRMMElement then
  begin
    result := TFHIROid.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIROid\"')
  end;
end;

function asInteger(obj : TFHIRObject) : TFHIRInteger;
begin
  if obj is TFHIRInteger then
    result := obj as TFHIRInteger
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRInteger.Create(TFHIRMMElement(obj).value);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRInteger\"')
  end;
end;

function asResource(obj : TFHIRObject) : TFHIRResource;
begin
  if obj is TFHIRResource then
    result := obj as TFHIRResource
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRResource\"')
  end;
end;

function asExtension(obj : TFHIRObject) : TFHIRExtension;
begin
  if obj is TFHIRExtension then
    result := obj as TFHIRExtension
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRResource\"')
  end;
end;

function asEnum(systems, values: array of String; obj : TFHIRObject) : TFHIREnum;
begin
  if obj is TFHIREnum then
    result := obj as TFHIREnum
  else if obj is TFHIRCode then
  begin
    result := TFHIREnum.Create(systems[StringArrayIndexOf(values, TFHIRCode(obj).value)], TFHIRCode(obj).value);
    obj.free;
  end
  else if obj is TFHIRString then
  begin
    result := TFHIREnum.Create(systems[StringArrayIndexOf(values, TFHIRString(obj).value)], TFHIRString(obj).value);
    obj.free;
  end
  else
  begin
    obj.free;
    raise EFHIRException.Create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRCode\"')
  end;
end;

function ComposeJson(worker: TFHIRWorkerContext; r : TFhirResource) : String;
var
  comp : TFHIRComposer;
begin
  comp := TFHIRParsers4B.composer(worker.link, ffJson, nil, OutputStyleNormal);
  try
    result := comp.Compose(r);
  finally
    comp.free;
  end;
end;

function TFhirOperationOutcomeIssueHelper.summary: String;
begin
  if details <> nil then
    result := CODES_TFhirIssueSeverityEnum[severity]+': '+details.text.Trim+' ('+CODES_TFhirIssueTypeEnum[code]+') @ '+locationList.summary
  else
    result := CODES_TFhirIssueSeverityEnum[severity]+': '+diagnostics.Trim+' ('+CODES_TFhirIssueTypeEnum[code]+') @ '+locationList.summary;
end;

{ TFhirOperationOutcomeIssueListHelper }

function TFhirOperationOutcomeIssueListHelper.errorCount: integer;
var
  issue : TFhirOperationOutcomeIssue;
begin
  result := 0;
  for issue in self do
    if (issue.severity in [IssueSeverityFatal, IssueSeverityError]) then
      inc(result);
end;

{ TFhirElementDefinitionHelper }

function TFhirElementDefinitionHelper.getType(t: String): TFhirElementDefinitionType;
var
  edt : TFhirElementDefinitionType;
begin
  result := nil;
  for edt in type_List do
    if edt.code = t then
      exit(edt);
end;

function TFhirElementDefinitionHelper.hasType(t: String): boolean;
var
  edt : TFhirElementDefinitionType;
begin
  result := false;
  for edt in type_List do
    if edt.code = t then
      exit(true);
end;

function TFhirElementDefinitionHelper.maxInt: Integer;
begin
  result := StrToIntDef(max, MAXINT);
end;

function TFhirElementDefinitionHelper.minInt: Integer;
begin
  result := StrToIntDef(min, 0);
end;

function TFhirElementDefinitionHelper.hasType(t: String; out profile: String): boolean;
var
  edt : TFhirElementDefinitionType;
begin
  result := false;
  profile := '';
  for edt in type_List do
    if SameText(edt.code, t) then
    begin
      if edt.profileList.Count > 0 then
        profile := edt.profileList[0].value;
      exit(true);
    end;
end;

{ TFHIRBackboneElementHelper }

procedure TFHIRBackboneElementHelper.checkNoModifiers(place, role: String; exempt : TArray<String>);
var
  ext : TFHIRExtension;
begin
  if hasModifierExtensionList then
    if length(exempt) > 0 then
    begin
      for ext in modifierExtensionList do
        if not StringArrayExistsInsensitive(exempt, ext.url) then
          raise EUnsafeOperation.Create('The element '+role+' has modifier exceptions that are unknown at '+place);
    end
    else
      raise EUnsafeOperation.Create('The element '+role+' has modifier exceptions that are unknown at '+place);
end;

procedure TFHIRBackboneElementHelper.checkNoModifiers(place, role: String);
begin
  if hasModifierExtensionList then
    raise EUnsafeOperation.Create('The element '+role+' has modifier exceptions that are unknown at '+place);
end;

{ TFhirCodeListHelper }

function TFhirCodeListHelper.hasCode(code: String): boolean;
var
   c : TFhirCode;
begin
  result := false;
  for c in self do
    if c.value = code then
      exit(true);
end;

{ TFhirUriListHelper }

function TFhirUriListHelper.hasUri(uri: String): boolean;
var
  i : integer;
begin
  result := false;
  for i := Count - 1 downto 0 do
    if (Item(i).value = uri) then
      Exit(true);
end;

procedure TFhirUriListHelper.removeUri(uri: String);
var
  i : integer;
begin
 for i := Count - 1 downto 0 do
   if (Item(i).value = uri) then
     Remove(i);
end;

{ TFhirCodeSystemHelper }

function TFhirCodeSystemHelper.locate(parent : TFhirCodeSystemConcept; list : TFhirCodeSystemConceptList; code : String; var foundParent, foundConcept : TFhirCodeSystemConcept) : boolean;
var
  c : TFhirCodeSystemConcept;
begin
  result := false;
  for c in list do
  begin
    if (c.code = code) then
    begin
      foundParent := parent;
      foundConcept := c;
      exit(true);
    end;
    result := Locate(c, c.conceptList, code, foundParent, foundConcept);
    if result then
      exit;
  end;
end;

procedure TFhirCodeSystemHelper.scanForSubsumes(parentList, conceptList: TFhirCodeSystemConceptList; code: String);
var
  ext : TFHIRExtension;
  c : TFhirCodeSystemConcept;
begin
  for c in conceptList do
  begin
    for ext in c.modifierExtensionList do
      if (ext.url = 'http://hl7.org/fhir/StructureDefinition/valueset-subsumes') then
        if (ext.value as TFHIRCode).value = code then
          parentList.Add(c.link);
    scanForSubsumes(parentList, c.conceptList, code);
  end;
end;

function TFhirCodeSystemHelper.buildImplicitValueSet: TFhirValueSet;
begin
  result := TFhirValueSet.Create;
  try
    result.url := valueSet;
    result.name := name;
    result.identifierList.Assign(identifierList);
    result.status := status;
    result.experimental := experimental;
    result.date := date;
    result.compose := TFhirValueSetCompose.Create;
    result.compose.includeList.Append.system := url;
    result.Link;
  finally
    result.free;
  end;
end;

function TFhirCodeSystemHelper.context: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to useContextList.Count - 1 do
    result := result + gen(useContextList[i]);
end;

function TFhirCodeSystemHelper.getChildren(concept: TFhirCodeSystemConcept): TFhirCodeSystemConceptList;
var
  ext : TFHIRExtension;
  p, c : TFhirCodeSystemConcept;
begin
  result := TFhirCodeSystemConceptList.Create;
  try
    result.AddAll(concept.conceptList);
    for ext in concept.modifierExtensionList do
      if (ext.url= 'http://hl7.org/fhir/StructureDefinition/valueset-subsumes') then
        if locate(nil, conceptList, (ext.value as TFHIRCode).value, p, c) then
          result.Add(c.link);
    result.Link;
  finally
    result.free;
  end;
end;

function TFhirCodeSystemHelper.GetCodeSystem: TFhirCodeSystem;
begin
  result := self;
end;

function TFhirCodeSystemHelper.getParents(concept: TFhirCodeSystemConcept): TFhirCodeSystemConceptList;
var
  p, c : TFhirCodeSystemConcept;
begin
  result := TFhirCodeSystemConceptList.Create;
  try
    if locate(nil, conceptList, concept.code, p, c) then
      if (p <> nil) then
        result.Add(p.Link);
    scanForSubsumes(result, conceptList, concept.code);
    result.Link;
  finally
    result.free;
  end;

end;

function TFhirCodeSystemHelper.GetSystem: String;
begin
  result := url;
end;

function TFhirCodeSystemHelper.isAbstract(concept: TFhirCodeSystemConcept): boolean;
var
  p : TFhirCodeSystemConceptProperty;
begin
  result := false;
  for p in concept.property_List do
    if (p.code = 'abstract') and (p.value is TFhirBoolean) and (TFHIRBoolean(p.value).value) then
      exit(true);    
  for p in concept.property_List do
      if (p.code = 'notSelectable') and (p.value is TFhirBoolean) and (TFHIRBoolean(p.value).value) then
        exit(true);
end;


function TFhirCodeSystemHelper.isInactive(concept: TFhirCodeSystemConcept): boolean;
var
  p : TFhirCodeSystemConceptProperty;
begin
  result := false;
  for p in concept.property_List do
  begin
    if (p.code = 'inactive') and (p.value is TFhirBoolean) and (TFHIRBoolean(p.value).value) then
      exit(true);
    if (p.code = 'inactive') and (p.value is TFhirCode) and (TFHIRCode(p.value).value = 'true') then
      exit(true);
    if (p.code = 'status') and ((p.value.ToString = 'inactive') or (p.value.ToString = 'retired')) then
      exit(true);
  end;
end;

function TFhirCodeSystemHelper.isDeprecated(concept: TFhirCodeSystemConcept): boolean;
var
  p : TFhirCodeSystemConceptProperty;
begin
  result := false;
  for p in concept.property_List do
  begin
    if (p.code = 'deprecated') and (p.value is TFhirBoolean) and (TFHIRBoolean(p.value).value) then
      exit(true);
    if (p.code = 'deprecationDate') and (p.value is TFhirDateTime) and (TFHIRDateTime(p.value).value.before(TFslDateTime.makeUTC, false)) then
      exit(true);
    if (p.code = 'status') and (p.value.ToString = 'deprecated') then
      exit(true);
  end;
end;

function TFhirCodeSystemHelper.codeStatus(concept: TFhirCodeSystemConcept): String;
var
  p : TFhirCodeSystemConceptProperty;
begin
  result := '';
  for p in concept.property_List do
    if (p.code = 'status') then
      exit(p.value.ToString);
  for p in concept.property_List do
  begin
    if (p.code = 'deprecated') and (p.value is TFhirBoolean) and (TFHIRBoolean(p.value).value) then
      exit('deprecated');
    if (p.code = 'deprecated') and (p.value is TFhirCode) and (TFHIRCode(p.value).value = 'true') then
      exit('deprecated');
    if (p.code = 'deprecationDate') and (p.value is TFhirDateTime) and (TFHIRDateTime(p.value).value.before(TFslDateTime.makeUTC, false)) then
      exit('deprecated');
    if (p.code = 'status') and (p.value.ToString = 'deprecated') then
      exit('deprecated');
    if (p.code = 'inactive') and (p.value is TFhirBoolean) and (TFHIRBoolean(p.value).value) then
      exit('inactive');
    if (p.code = 'inactive') and (p.value is TFhirCode) and (TFHIRCode(p.value).value = 'true') then
      exit('inactive');
    if (p.code = 'retired') and (p.value is TFhirBoolean) and (TFHIRBoolean(p.value).value) then
      exit('retired');
    if (p.code = 'retired') and (p.value is TFhirCode) and (TFHIRCode(p.value).value = 'true') then
      exit('retired');
  end;
end;

{ TFhirAuditEventHelper }

function TFhirAuditEventHelper.GetdateTime: TFslDateTime;
begin
  result := recorded;
end;

function TFhirAuditEventHelper.getevent: TFhirAuditEvent;
begin
  result := self;
end;

function TFhirAuditEventHelper.GetObjectList: TFhirAuditEventEntityList;
begin
  result := entityList;
end;

function TFhirAuditEventHelper.getParticipantList: TFhirAuditEventAgentList;
begin
  result := agentList;
end;

procedure TFhirAuditEventHelper.SetDateTime(const Value: TFslDateTime);
begin
  recorded := value;
end;

procedure TFhirAuditEventHelper.SetEvent(const Value: TFhirAuditEvent);
begin
  value.free;
end;

{ TFhirCodingHelper }

constructor TFhirCodingHelper.Create(system, code: String);
begin
  Create;
  self.system := system;
  self.code := code;
end;

constructor TFhirCodingHelper.Create(system, code, display: String);
begin
  Create;
  self.system := system;
  self.code := code;
  self.display := display;
end;

class function TFhirCodingHelper.fromEdit(s: String): TFhirCoding;
begin
  result := TFhirCoding.Create;
  try
    result.editString := s;
    result.Link;
  finally
    result.free;
  end;
end;

function TFhirCodingHelper.GetEditString: String;
begin
  if system = URI_SNOMED then
    result := 'sct:'+code
  else if system = URI_LOINC then
    result := 'loinc:'+code
  else if system = URI_LOINC then
    result := 'loinc:'+code

  else if system = URI_SNOMED then
    result := 'sct:'+code
  else if system = URI_RXNORM then
    result := 'rxnorm:'+code
  else if system = URI_LOINC then
    result := 'loinc:'+code
  else if system = URI_UCUM then
    result := 'ucum:'+code
  else if system = 'http://ncimeta.nci.nih.gov' then
    result := 'nci:'+code
  else if system = URI_CPT then
    result := 'cpt:'+code
  else if system = URI_NDFRT then
    result := 'ndfrt:'+code
  else if system = URI_UNII then
    result := 'unii:'+code
  else if system = URI_NDC then
    result := 'ndc:'+code
  else if system = URI_CVX then
    result := 'cvx:'+code
  else if system = URI_3166 then
    result := 'iso3166:'+code
  else if system = 'http://www.radlex.org' then
    result := 'radlex:'+code
  else if system = 'http://hl7.org/fhir/sid/icf-nl' then
    result := 'icf:'+code
  else if system = 'http://www.whocc.no/atc' then
    result := 'atcc:'+code
  else if system = URI_BCP47 then
    result := 'lang:'+code
  else if system = URI_11073 then
    result := 'mdc:'+code
  else if system = URI_DICOM then
    result := 'dicom:'+code
  else if system = 'http://hl7.org/fhir/sid/ca-hc-din' then
    result := 'ca-din:'+code
  else if system = 'http://nucc.org/provider-taxonomy' then
    result := 'nucc:'+code
  else if system = 'http://www.genenames.org' then
    result := 'hgnc:'+code
  else if system = 'http://www.ensembl.org' then
    result := 'ensembl:'+code
  else if system = 'http://www.ncbi.nlm.nih.gov/nuccore' then
    result := 'refseq:'+code
  else if system = 'http://www.ncbi.nlm.nih.gov/clinvar' then
    result := 'clinvar:'+code
  else if system = 'http://sequenceontology.org' then
    result := 'seqont:'+code
  else if system = 'http://www.hgvs.org/mutnomen' then
    result := 'hgvs:'+code
  else if system = 'http://www.ncbi.nlm.nih.gov/projects/SNP' then
    result := 'dbsnp:'+code
  else if system = 'http://cancer.sanger.ac.uk/cancergenome/projects/cosmic' then
    result := 'cosmic:'+code
  else if system = 'http://www.lrg-sequence.org' then
    result := 'lrg:'+code
  else if system = 'http://www.omim.org' then
    result := 'omim:'+code
  else if system = 'http://www.ncbi.nlm.nih.gov/pubmed' then
    result := 'pubmed:'+code
  else if system = 'http://www.pharmgkb.org' then
    result := 'pharmgkb:'+code
  else if system = 'http://clinicaltrials.gov' then
    result := 'clintrial:'+code
  else if system = 'http://www.ebi.ac.uk/ipd/imgt/hla/' then
    result := 'hla:'+code
  else if system.StartsWith('http://hl7.org/fhir/v2/') then
    result := 'v2-'+system.Substring(23)+':'+code
  else if system.StartsWith('http://hl7.org/fhir/v3/') then
    result := 'v3-'+system.Substring(23)+':'+code
  else
    result := system+'|'+code;
end;

function TFhirCodingHelper.hasCode(System, Code: String): boolean;
begin
  result := (self.system = system) and (self.code = code);
end;

procedure TFhirCodingHelper.SetEditString(const Value: String);
var
  s, c : String;
  function match(abbrev, sys : String) : boolean;
  begin
    result := s = abbrev+':';
    if result then
    begin
      system := sys;
      code := c;
    end;
  end;
begin
  StringSplit(value, ':', s, c);
  if not match('sct', URI_SNOMED) then
  if not match('rxnorm', URI_RXNORM) then
  if not match('loinc', URI_LOINC) then
  if not match('ucum', URI_UCUM) then
  if not match('nci', 'http://ncimeta.nci.nih.gov') then
  if not match('cpt', URI_CPT) then
  if not match('ndfrt', URI_NDFRT) then
  if not match('unii', URI_UNII) then
  if not match('ndc', URI_NDC) then
  if not match('cvx', URI_CVX) then
  if not match('iso3166', URI_3166) then
  if not match('radlex', 'http://www.radlex.org') then
  if not match('icf', 'http://hl7.org/fhir/sid/icf-nl') then
  if not match('atcc', 'http://www.whocc.no/atc') then
  if not match('lang', URI_BCP47) then
  if not match('mdc', URI_11073) then
  if not match('dicom', URI_DICOM) then
  if not match('ca', 'din http://hl7.org/fhir/sid/ca-hc-din') then
  if not match('nucc', 'http://nucc.org/provider-taxonomy') then
  if not match('hgnc', 'http://www.genenames.org') then
  if not match('ensembl', 'http://www.ensembl.org') then
  if not match('refseq', 'http://www.ncbi.nlm.nih.gov/nuccore') then
  if not match('clinvar', 'http://www.ncbi.nlm.nih.gov/clinvar') then
  if not match('seqont', 'http://sequenceontology.org') then
  if not match('hgvs', 'http://www.hgvs.org/mutnomen') then
  if not match('dbsnp', 'http://www.ncbi.nlm.nih.gov/projects/SNP') then
  if not match('cosmic', 'http://cancer.sanger.ac.uk/cancergenome/projects/cosmic') then
  if not match('lrg', 'http://www.lrg-sequence.org') then
  if not match('omim', 'http://www.omim.org') then
  if not match('pubmed', 'http://www.ncbi.nlm.nih.gov/pubmed') then
  if not match('pharmgkb', 'http://www.pharmgkb.org') then
  if not match('clintrial', 'http://clinicaltrials.gov') then
  if not match('hla', 'http://www.ebi.ac.uk/ipd/imgt/hla/') then

  if s.StartsWith('v2-') then
  begin
    system := 'http://hl7.org/fhir/v2/'+s.Substring(3);
    code := c;
  end
  else if s.StartsWith('v3-') then
  begin
    system := 'http://hl7.org/fhir/v3/'+s.Substring(3);
    code := c;
  end
  else
  begin
    StringSplit(value, '|', s, c);
    system := s;
    code := c;
  end;
end;

{ TFhirTestScriptSetupActionOperationRequestHeaderListHelper }

procedure TFhirTestScriptSetupActionOperationRequestHeaderListHelper.add(name, value: String);
var
  o : TFhirTestScriptSetupActionOperationRequestHeader;
begin
  if value <> '' then
  begin
    o := Append;
    o.field := name;
    o.value := value;
  end;
end;

{ TFHIRNamingSystemHelper }

function TFHIRNamingSystemHelper.getUri: String;
var
  id : TFhirNamingSystemUniqueId;
begin
  result := '';
  for id in uniqueIdList do
    if (id.type_ = NamingsystemIdentifierTypeUri) then
      exit(id.value);
end;

function TFHIRNamingSystemHelper.hasOid(oid: String): boolean;
var
  id : TFhirNamingSystemUniqueId;
begin
  result := false;
  for id in uniqueIdList do
    if (id.type_ = NamingSystemIdentifierTypeOID) and (id.value = oid) then
      exit(true);
end;

{ TFhirStrutureMapHelper }

function TFhirStrutureMapHelper.targetType: String;
var
  input : TFhirStructureMapGroupInput;
begin
  result := '';
  for input in groupList[0].inputList do
    if input.mode = StructureMapInputModeTarget then
      if result = '' then
        result := input.type_
      else
        raise EFHIRException.Create('Multiple input types not accepted');
end;

{ TFhirValueSetExpansionHelper }

procedure TFhirValueSetExpansionHelper.AddParamStr(name, value: String);
var
  p : TFhirValueSetExpansionParameter;
begin
  for p in parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;

  p := parameterList.Append;
  p.name := name;
  p.value := TFhirString.Create(value);
end;

procedure TFhirValueSetExpansionHelper.AddParamUri(name, value: String);
var
  p : TFhirValueSetExpansionParameter;
begin
  for p in parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;

  p := parameterList.Append;
  p.name := name;
  p.value := TFhirUri.Create(value);
end;

procedure TFhirValueSetExpansionHelper.AddParamCanonical(name, value: String);
var
  p : TFhirValueSetExpansionParameter;
begin
  for p in parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;

  p := parameterList.Append;
  p.name := name;
  p.value := TFhirCanonical.Create(value);
end;

procedure TFhirValueSetExpansionHelper.AddParamBool(name: String; value: boolean);
var
  p : TFhirValueSetExpansionParameter;
begin
  for p in parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = LCBooleanToString(value)) then
      exit;

  p := parameterList.Append;
  p.name := name;
  p.value := TFhirBoolean.Create(value);
end;

procedure TFhirValueSetExpansionHelper.addParamCode(name, value: String);
var
  p : TFhirValueSetExpansionParameter;
begin
  for p in parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = value) then
      exit;

  p := parameterList.Append;
  p.name := name;
  p.value := TFhirCode.Create(value);
end;

procedure TFhirValueSetExpansionHelper.addParamInt(name: String; value: integer);
var
  p : TFhirValueSetExpansionParameter;
begin
  for p in parameterList do
    if (p.name = name) and (p.value <> nil) and (p.value.primitiveValue = inttostr(value)) then
      exit;

  p := parameterList.Append;
  p.name := name;
  p.value := TFhirInteger.Create(inttostr(value));
end;

function hasProp(props : TList<String>; name : String; def : boolean) : boolean;
begin
  if (props = nil) or (props.Count = 0) then
    result := def
  else
    result := props.Contains(name);
end;

{ TFHIRObservationHelper }

function TFHIRObservationHelper.addComponent(system, code: String): TFhirObservationComponent;
var
  c : TFHIRCoding;
begin
  result := self.componentList.Append;
  result.code := TFhirCodeableConcept.Create;
  c := result.code.codingList.Append;
  c.system := system;
  c.code := code;
end;

function TFHIRObservationHelper.getComponent(system, code: String; var comp: TFhirObservationComponent): boolean;
var
  t : TFhirObservationComponent;
begin
  comp := nil;
  result := false;
  for t in self.componentList do
    if (t.code.codingList[0].system = system) and (t.code.codingList[0].code = code) then
    begin
      comp := t;
      exit(true);
    end;
end;

function TFHIRObservationHelper.getComponent(system: String; var comp: TFhirObservationComponent): boolean;
var
  t : TFhirObservationComponent;
begin
  comp := nil;
  result := false;
  for t in self.componentList do
    if (t.code.codingList[0].system = system) then
    begin
      comp := t;
      exit(true);
    end;
end;

{ TFhirQuantityHelper }

function TFhirQuantityHelper.asDuration: TDateTime;
var
  v : Double;
begin
  if system <> URI_UCUM then
    raise EFHIRException.Create('Unknown units system "'+system+'" trying to process quantity as a duration');
  if not IsNumericString(value) then
    raise EFHIRException.Create('invalid value "'+value+'" trying to process quantity as a duration');
  v := TFslDecimal.ValueOf(value).AsDouble;
  if (code = 'ps') then
    result := v * (DATETIME_MILLISECOND_ONE / 1000000000)
  else if (code = 'ns') then
    result := v * (DATETIME_MILLISECOND_ONE / 1000000)
  else if (code = 'us') then
    result := v * (DATETIME_MILLISECOND_ONE / 1000)
  else if (code = 'ms') then
    result := v * DATETIME_MILLISECOND_ONE
  else if (code = 's') then
    result := v * DATETIME_SECOND_ONE
  else if (code = 'min') then
    result := v * DATETIME_MINUTE_ONE
  else if (code = 'h') then
    result := v * DATETIME_HOUR_ONE
  else if (code = 'd') then
    result := v * 1
  else if (code = 'wk') then
    result := v * 7
  else if (code = 'mo') then
    result := v * 30
  else if (code = 'a') then
    result := v * 365.25
  else
    raise EFHIRException.Create('invalid UCUM unit "'+code+'" trying to process quantity as a duration');
end;

class function TFhirQuantityHelper.fromDuration(v : TDateTime): TFhirQuantity;
begin
  if v > 365 then
    result := fromPair(v/365.25, 'a')
  else if (v > 30) then
    result := fromPair(v/30, 'mo')
  else if (v > 7) then
    result := fromPair(v * 7, 'wk')
  else if (v > 1) then
    result := fromPair(v, 'd')
  else if (v > DATETIME_HOUR_ONE) then
    result := fromPair(v / DATETIME_HOUR_ONE, 'h')
  else if (v > DATETIME_MINUTE_ONE) then
    result := fromPair(v / DATETIME_MINUTE_ONE, 'min')
  else if (v > DATETIME_SECOND_ONE) then
    result := fromPair(v / DATETIME_SECOND_ONE, 's')
  else if (v > DATETIME_MILLISECOND_ONE) then
    result := fromPair(v / DATETIME_MILLISECOND_ONE, 'ms')
  else if (v > (DATETIME_MILLISECOND_ONE / 1000)) then
    result := fromPair(v / (DATETIME_MILLISECOND_ONE / 1000), 'us')
  else if (v > (DATETIME_MILLISECOND_ONE / 1000000)) then
    result := fromPair(v / (DATETIME_MILLISECOND_ONE / 1000000), 'ns')
  else
    result := fromPair(v / (DATETIME_MILLISECOND_ONE / 1000000000), 'ps');
end;

class function TFhirQuantityHelper.fromEdit(s: String): TFhirQuantity;
begin
  result := TFhirQuantity.Create;
  try
    result.editString := s;
    result.Link;
  finally
    result.free;
  end;
end;

class function TFhirQuantityHelper.fromPair(v: Double; units: String): TFhirQuantity;
begin
  result := TFhirQuantity.Create;
  try
    result.value := FloatToStr(v);
    result.system := 'http://unitsofmeasure.org/';
    result.code := units;
  finally
    result.free;
  end;
end;

class function TFhirQuantityHelper.fromUcum(value, code: String): TFhirQuantity;
begin
  result := TFHIRQuantity.Create;
  result.value := value;
  result.unit_ := code;
  result.system := URI_UCUM;
  result.code := code;
end;

function TFhirQuantityHelper.GetEditString: String;
begin
  result := CODES_TFhirQuantityComparatorEnum[comparator]+value+' '+unit_;
  if code <> '' then
  begin
    if system = URI_SNOMED then
      result := result+' [sct:'+code+']'
    else if system <> URI_UCUM then
      result := result+' ['+system+'|'+code+']'
    else if code <> unit_ then
      result := result +' ['+code+']';
  end;
end;

procedure TFhirQuantityHelper.SetEditString(vs: String);
var
  v, u : String;
  i : integer;
begin
  if StringArrayExistsSensitive(CODES_TFhirQuantityComparatorEnum, vs[1]) then
  begin
    comparator := TFhirQuantityComparatorEnum(StringArrayIndexOfSensitive(CODES_TFhirQuantityComparatorEnum, vs[1]));
    vs := vs.Substring(1);
  end;
  if vs.Contains('[') then
  begin
    StringSplit(vs, '[', vs, u);
    vs := vs.Trim;
  end;
  if vs.Contains(' ') then
    StringSplit(vs, ' ', v, vs)
  else
  begin
    i := StringFindEndOfNumber(vs, 1);
    if i = 1 then
      raise EFHIRException.Create('Unable to parse quantity '+vs);
    v := vs.Substring(0, i);
    vs := vs.Substring(i);
  end;
  value := v;
  unit_ := vs;
  if u <> '' then
  begin
    if u.EndsWith(']') then
      u := u.Substring(0, u.Length-1);
    if u.StartsWith('sct:') then
    begin
      system := URI_SNOMED;
      code := u.Substring(4);
    end
    else if u.Contains('|') then
    begin
      StringSplit(u, '|', u, v);
      system := u;
      code := v;
    end
    else
    begin
      system := URI_UCUM;
      code := u;
    end;
  end;
end;

{ TFhirIdentifierListHelper }

function TFhirIdentifierListHelper.BySystem(uri : String): TFhirIdentifier;
var
  id : TFhirIdentifier;
begin
  result := nil;
  for id in self do
    if id.system = uri then
      exit(id);
end;

function TFhirIdentifierListHelper.withCommas: String;
var
  id : TFhirIdentifier;
begin
  result := '';
  for id in self do
    result := result + id.value+' ';
  result := result.Trim;
end;

{ TFHIRDocumentReferenceHelper }

function makeFileName(s : String) : String;
var
  b : TStringBuilder;
  ws : boolean;
  ch : char;
begin
  b := TStringBuilder.Create;
  try
    ws := true;
    for ch in s do
    begin
      if not CharInSet(ch, ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']) then
        ws := true
      else if ws then
      begin
        b.Append(uppercase(ch));
        ws := false;
      end
      else
        b.Append(ch);
    end;
    result := b.ToString;
  finally
    b.free;
  end;
end;

function TFHIRDocumentReferenceHelper.asZip(var filename: String): TStream;
var
  vcl : TFslVCLStream;
  zip : TFslZipWriter;
  content : TFhirDocumentReferenceContent;
  i : integer;
begin
  result := TMemoryStream.Create;
  try
    vcl := TFslVCLStream.Create;
    try
      vcl.Stream := result;
      zip := TFslZipWriter.Create;
      try
        zip.Stream := vcl.Link;
        i := 0;
        for content in contentList do
        begin
          inc(i);
          zip.Parts.Add(content.attachment.asZipPart(i));
        end;
        zip.WriteZip;
        if (contentList.Count = 1) and (contentList[0].attachment.title <> '') then
          filename := makeFileName(contentList[0].attachment.title)+'.zip'
        else
          filename := makeFileName(description)+'.zip';
      finally
        zip.free;
      end;
    finally
      vcl.free;
    end;
    result.Position := 0;
  except
    result.free;
    raise;
  end;
end;

{ TFHIRAttachmentHelper }

Const
  KNOWN_MIME_TYPES : array[0..11] of String = (
    'text/plain',
    'text/html',
    'text/xml',
    'application/xml',
    'application/json',
    'text/css',
    'image/x-icon',
    'image/png',
    'image/gif',
    'image/jpeg',
    'video/mpeg',
    'text/javascript'
    );

  KNOWN_MIME_EXTENSIONS : array[0..11] of String = (
    '.txt',
    '.html',
    '.xml',
    '.xml',
    '.json',
    '.css',
    '.ico',
    '.png',
    '.gif',
    '.jpg',
    '.mpeg',
    '.js'
    );

Function GetExtForMimeType(mimeType: String): String;
{$IFDEF WINDOWS}
Var
  fReg: TRegistry;
  ts : TStringList;
  s : String;
  i : integer;
Begin
  mimeType := lowercase(mimeType);
  i := StringArrayIndexOfInsensitive(KNOWN_MIME_TYPES, mimeType);
  if i > -1 then
    exit(KNOWN_MIME_EXTENSIONS[i]);
  Try
    fReg := TRegistry.Create;
    Try
      fReg.RootKey := HKEY_LOCAL_MACHINE;
      fReg.OpenKeyReadOnly('Software\Classes');
      ts := TStringList.Create;
      try
        freg.GetKeyNames(ts);
        fReg.CloseKey;
        for s in ts do
        begin
          fReg.OpenKeyReadOnly('Software\Classes\'+s);
          if freg.ReadString('Content Type').ToLower = mimeType then
            exit('.'+s);
          fReg.CloseKey;
      end;
      finally
        ts.free;
      end;
    Finally
      freg.free;
    End;
  Except
  End;
  if mimeType.Contains('+xml') then
    result := '.xml';
  if mimeType.Contains('+json') then
    result := '.json';
  If Result = '' Then
    Result := '.bin';
End;
{$ELSE}
Var
  s : String;
  i : integer;
Begin
  mimeType := lowercase(mimeType);
  i := StringArrayIndexOfInsensitive(KNOWN_MIME_TYPES, mimeType);
  if i > -1 then
    exit(KNOWN_MIME_EXTENSIONS[i]);
  if mimeType.Contains('+xml') then
    result := '.xml';
  if mimeType.Contains('+json') then
    result := '.json';
  If Result = '' Then
    Result := '.bin';
end;
{$ENDIF}

function TFHIRAttachmentHelper.asZipPart(i: integer): TFslZipPart;
{$IFDEF MACOS}
begin
  raise EFHIRTodo.Create('TFHIRAttachmentHelper.asZipPart');
end;
{$ELSE}
var
  fetcher : TInternetFetcher;
begin
  result := TFslZipPart.Create;
  try
    if (url <> '') and (Length(data) = 0) then
    begin
      fetcher := TInternetFetcher.Create;
      try
        fetcher.URL := url;
        fetcher.Buffer := result.Link;
        fetcher.Fetch;
      finally
        fetcher.free;
      end;
    end
    else
    begin
      result.Size := Length(data);
      if length(data) > 0 then
        move(data[0], result.Data^, length(data));
    end;
    result.Name := title;
    result.Comment := contentType;
    if result.Name = '' then
      result.Name := 'file'+inttostr(i)+GetExtForMimeType(result.Comment)
    else if not result.name.contains('.') then
      result.Name := result.Name+GetExtForMimeType(result.Comment);
    result.Link;
  finally
    result.free;
  end;
end;
{$ENDIF}

{ TFhirReferenceHelper }

constructor TFhirReferenceHelper.Create(ref: String);
begin
  inherited Create;
  reference := ref;
end;

function TFhirReferenceHelper.GetEditString: String;
begin
  if reference <> '' then
    result := reference
  else
    result := '"'+display+'"';
end;

function TFhirReferenceHelper.getId: String;
var
  parts : TArray<String>;
begin
  parts := reference.Split(['/']);
  if (length(parts) < 2) then
    result := ''
  else if isResourceName(parts[length(parts) - 2]) then
    result := parts[length(parts) - 1]
  else if (length(parts) >= 4) and isResourceName(parts[length(parts) - 4]) then
    result := parts[length(parts) - 3]
  else
    result := '';
end;

function TFhirReferenceHelper.getType: String;
var
  parts : TArray<String>;
begin
  parts := reference.Split(['/']);
  if (length(parts) < 2) then
    result := ''
  else if isResourceName(parts[length(parts) - 2]) then
    result := parts[length(parts) - 2]
  else if (length(parts) >= 4) and isResourceName(parts[length(parts) - 4]) then
    result := parts[length(parts) - 4]
  else
    result := '';
end;

function TFhirReferenceHelper.isRelative: boolean;
begin
  result := not (reference.startsWith('http:') or reference.startsWith('https:') or reference.startsWith('urn:uuid:') or reference.startsWith('urn:oid:'));
end;

procedure TFhirReferenceHelper.SetEditString(const Value: String);
begin
  if not value.StartsWith('"') then
    reference := value
  else if value.EndsWith('"') then
    display := value.Substring(1, value.Length-2)
  else
    display := value.Substring(1, value.Length-1);
end;

function fileToResource(name : String; var format : TFHIRFormat) : TFhirResource;
var
  f : TFileStream;
begin
  f := TFileStream.Create(name, fmOpenRead + fmShareDenyWrite);
  try
    result := streamToResource(f, format);
  finally
    f.free;
  end;
end;

function fileToResource(name : String) : TFhirResource;
var
  format : TFHIRFormat;
begin
  format := ffUnspecified;
  result := fileToResource(name, format);
end;

function stringToResource(source : String) : TFhirResource;
var
  format : TFHIRFormat;
begin
  format := ffUnspecified;
  result := stringToResource(source, format);
end;

function stringToResource(source : String; var format : TFHIRFormat) : TFhirResource;
var
  b : TStringStream;
begin
  b := TStringStream.Create(source, TEncoding.UTF8);
  try
    result := streamToResource(b, format);
  finally
    b.free;
  end;
end;

function bytesToResource(bytes : TBytes) : TFhirResource;
var
  format : TFHIRFormat;
begin
  format := ffUnspecified;
  result := bytesToResource(bytes, format);
end;

function bytesToResource(bytes : TBytes; var format : TFHIRFormat) : TFhirResource;
var
  b : TBytesStream;
begin
  b := TBytesStream.Create(bytes);
  try
    result := streamToResource(b, format);
  finally
    b.free;
  end;
end;

function streamToResource(stream : TStream; var format : TFHIRFormat) : TFhirResource;
var
  p :  TFHIRParser;
begin
  if format = ffUnspecified then
    format := DetectFormat(stream);
  p := TFHIRParsers4B.parser(nil, format, nil);
  try
    p.source := stream;
    p.Parse;
    result := p.resource.Link as TFHIRResource;
  finally
    p.free;
  end;
end;

function resourceToString(res : TFhirResource; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal) : String;
var
  f : TStringStream;
begin
  f := TStringStream.Create('', TEncoding.UTF8);
  try
    resourceToStream(res, f, format, style);
    result := f.DataString;
  finally
    f.free;
  end;
end;

function resourceToBytes(res : TFhirResource; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal) : TBytes;
var
  f : TBytesStream;
begin
  f := TBytesStream.Create();
  try
    resourceToStream(res, f, format, style);
    result := f.Bytes;
    SetLength(result, f.size);
  finally
    f.free;
  end;
end;

procedure resourceToFile(res : TFhirResource; name : String; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal);
var
  f : TFileStream;
begin
  f := TFileStream.Create(name, fmCreate);
  try
    resourceToStream(res, f, format, style);
  finally
    f.free;
  end;
end;

procedure resourceToStream(res : TFhirResource; stream : TStream; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal);
var
  c : TFHIRComposer;
begin
  if format = ffXhtml then
    format := ffXml;
  c := TFHIRParsers4B.composer(nil, format, nil, style);
  try
    c.Compose(stream, res);
  finally
    c.free;
  end;
end;

{ TFHIRDurationHelper }

function TFHIRDurationHelper.ToDateTime: TDateTime;
var
  b : TDateTime;
begin
  if system <> URI_UCUM then
    raise EFHIRException.Create('Unknown system (must be UCUM)');
  if code = 'a' then
    b := 365.25
  else if (code = 'mo') then
    b := 29.53059
  else if (code = 'wk') then
    b := 7
  else if (code = 'd') then
    b := 1
  else if (code = 'h') then
    b := DATETIME_DAY_HOURS
  else if (code = 'min') then
    b := DATETIME_DAY_MINUTES
  else
    raise EFHIRException.Create('Unknown UCUM unit for time: '+code);
  result := b * TFslDecimal.ValueOf(value).AsDouble;
end;

function parseParamsFromForm(stream : TStream) : TFHIRParameters;
var
  pm : THTTPParameters;
  i, j : integer;
  n, v : String;
begin
  result := TFhirParameters.Create;
  try
    pm := THTTPParameters.Create(StreamToString(stream, TEncoding.ASCII));
    try
      for i := 0 to pm.Count - 1 do
      begin
        n := pm.Name[i];
        for j := 0 to pm.getValueCount(i) - 1 do
        begin
          pm.retrieveNumberedItem(i,j, v);
          result.AddParameter(n, TFHIRString.Create(v));
        end;
      end;
    finally
      pm.free;
    end;
    result.link;
  finally
    result.free;
  end;
end;

{ TFHIRCodeableConceptListHelper }

function TFHIRCodeableConceptListHelper.GetHasCode(System, Code: String): boolean;
var
  cc : TFHIRCodeableConcept;
begin
  result := false;
  for cc in self do
    if cc.hasCode(system, code) then
      exit(true);
end;

procedure TFHIRCodeableConceptListHelper.SetHasCode(System, Code: String; const Value: boolean);
var
  i : integer;
begin
  if value then
  begin
    if not getHasCode(system, code) then
      Add(TFhirCodeableConcept.Create(system, code));
  end
  else
    for i := Count - 1 downto 0 do
      if Item(i).hasCode(system, code) then
        DeleteByIndex(i);
end;

{ TFhirCapabilityStatementRestResourceSearchParamHelper }

function TFhirCapabilityStatementRestResourceSearchParamHelper.summary: String;
begin
  if (definition <> '') then
    result := name +' : '+CODES_TFhirSearchParamTypeEnum[type_]+' ('+definition+')'
  else
    result := name +' : '+CODES_TFhirSearchParamTypeEnum[type_];
end;

{ TFhirConformanceRestHelper }

function TFhirConformanceRestHelper.interaction(type_: TFhirSystemRestfulInteractionEnum): TFhirCapabilityStatementRestInteraction;
var
  i : integer;
begin
  result := nil;
  for i := 0 to self.interactionList.count - 1 do
    if (self.interactionList[i].code = type_) then
      result := self.interactionList[i];
end;

{ TFhirValueSetComposeIncludeHelper }

function TFhirValueSetComposeIncludeHelper.summary: String;
begin
  if valueSetList.Count > 0 then
    result := 'from valueset '+valueSetList[0].value
  else if conceptList.Count > 0 then
    result := 'enumerated concepts from '+system
  else if filterList.Count > 0 then
    result := 'select concepts from '+system
  else
    result := 'all concepts from '+system;
end;

{ TFhirHumanNameHelper }

class function TFhirHumanNameHelper.fromEdit(n: String): TFhirHumanName;
var
  s : String;
begin
  result := TFhirHumanName.Create;
  try
    // really, what we do should be drive by culture, but for now, we guess.
    if (n.contains(',')) then
    begin
      // anything before the , is family name
      result.family := n.substring(0, n.indexOf(','));
      for s in n.substring(n.indexOf(',')+1).split([' ']) do
        result.givenList.add(TFhirString.Create(s));
    end
    else
    begin
      // anything before the list space is given names
      result.family := n.substring(n.lastIndexOf(' '));
      for s in n.substring(0, n.lastIndexOf(',')+1).split([' ']) do
        result.givenList.add(TFhirString.Create(s));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFhirHumanNameHelper.given: string;
var
  g : TFHIRString;
begin
  result := '';
  for g in givenList do
    result := result + g.value+' ';
  result := result.Trim;
end;

{ TFhirCodeSystemConceptHelper }

function TFhirCodeSystemConceptHelper.addProp(code: String): TFhirCodeSystemConceptProperty;
begin
  result := property_List.Append;
  result.code := code;
end;

function TFhirCodeSystemConceptHelper.countDescendents: integer;
var
  c : TFhirCodeSystemConcept;
begin
  result := conceptList.Count;
  for c in conceptList do
    inc(result, c.countDescendents);
end;

procedure TFhirCodeSystemConceptHelper.deleteProp(code: String);
var
  i : integer;
begin
  for i := property_List.Count - 1 downto 0 do
    if property_List[i].code = code then
      property_List.Remove(i);
end;

function TFhirCodeSystemConceptHelper.prop(code: String): TFhirCodeSystemConceptProperty;
var
  t : TFhirCodeSystemConceptProperty;
begin
  result := nil;
  for t in property_List do
    if t.code = code then
      exit(t);
end;

{ TFhirQuestionnaireItemHelper }

function TFhirQuestionnaireItemHelper.countDescendents: integer;
var
  c : TFhirQuestionnaireItem;
begin
  result := itemList.Count;
  for c in itemList do
    inc(result, c.countDescendents);
end;

function TFhirQuestionnaireItemHelper.getinitial: TFHIRDataType;
begin
  if initialList.Count = 0 then
    result := nil
  else
    result := initialList[0].value;
end;

function TFhirQuestionnaireItemHelper.GetOptionList: TFhirQuestionnaireItemAnswerOptionList;
begin
 result := answerOptionList;
end;

function TFhirQuestionnaireItemHelper.getOptions: String;
begin
  result := answerValueSet;
end;

procedure TFhirQuestionnaireItemHelper.SetInitial(const Value: TFHIRDataType);
begin
  if initialList.Count = 0 then
    initialList.Append.value := value
  else
    initialList[0].value := value;
end;

procedure TFhirQuestionnaireItemHelper.SetOptions(const Value: String);
begin
  answerValueSet := value;
end;

{ TFhirQuestionnaireHelper }

function TFhirQuestionnaireHelper.itemCount: integer;
var
  c : TFhirQuestionnaireItem;
begin
  result := itemList.Count;
  for c in itemList do
    inc(result, c.countDescendents);
end;

{ TFhirExtensionListHelper }

procedure TFhirExtensionListHelper.addExtension(url, value: String);
var
  ext : TFhirExtension;
begin
  ext := TFhirExtension.Create;
  add(ext);
  ext.url := url;
  ext.value := TFhirString.Create(value);
end;

{ TFhirIdentifierHelper }

constructor TFhirIdentifierHelper.Create(system, value: String);
begin
  Create;
  FValue := TFhirString.Create(value);
  FSystem := TFhirUri.Create(system);
end;

constructor TFhirIdentifierHelper.Create(value: String);
begin
  Create;
  FValue := TFhirString.Create(value);
end;

class function TFhirIdentifierHelper.fromEdit(s: String): TFhirIdentifier;
begin
  result := TFhirIdentifier.Create;
  try
    result.editString := s;
    result.Link;
  finally
    result.free;
  end;
end;

function TFhirIdentifierHelper.GetEditString: String;
begin
  result := system+'|'+value;
end;

function TFhirIdentifierHelper.isType(code: String): boolean;
begin
  if StringArrayExistsSensitive(['DL', 'PPN', 'BRN', 'MR', 'MCN', 'EN', 'TAX', 'NIIP', 'PRN', 'MD', 'DR', 'ACSN'], code) then
    result := isType('http://hl7.org/fhir/v2/0203', code)
  else if StringArrayExistsSensitive(['UDI', 'SNO', 'SB', 'PLAC', 'FILL'], code) then
    result := isType('http://hl7.org/fhir/identifier-type', code)
  else
    result := false;
end;

function TFhirIdentifierHelper.isType(system, code: String): boolean;
var
  c : TFhirCoding;
begin
  result := false;
  if type_ <> nil then
    for c in type_.codingList do
      if (c.system = system) and (c.code = code) then
        exit(true);
end;

procedure TFhirIdentifierHelper.SetEditString(const Value: String);
var
  s, c : String;
begin
  StringSplit(value, '|', s, c);
  system := s;
  self.value := c;
end;

{ TFhirPeriodHelper }

class function TFhirPeriodHelper.fromDateTimes(start, end_: TDateTime) : TFHIRPeriod;
begin
  result := TFhirPeriod.Create;
  result.start := TFslDateTime.make(start, dttzUTC);
  result.end_ := TFslDateTime.make(end_, dttzUTC);
end;

class function TFhirPeriodHelper.fromDates(start, end_: TDateTime) : TFHIRPeriod;
begin
  result := TFhirPeriod.Create;
  result.start := TFslDateTime.make(start, dttzUTC, dtpDay);
  result.end_ := TFslDateTime.make(end_, dttzUTC, dtpDay);
end;

class function TFhirPeriodHelper.fromEdit(s: String): TFhirPeriod;
begin
  result := TFhirPeriod.Create;
  try
    result.editString := s;
    result.Link;
  finally
    result.free;
  end;
end;

function TFhirPeriodHelper.GetEditString: String;
begin
  if start.null then
    result := ''
  else
    result := start.toXML;
  result := result + ' -> ';
  if end_.null then
    result := result + ''
  else
    result := result + end_.toXML;

end;

function TFhirPeriodHelper.point: TDateTime;
begin
  if (startElement <> nil) and (end_Element <> nil) then
    result := (start.DateTime + end_.DateTime) / 2
  else if startElement <> nil then
    result := start.DateTime
  else if end_Element <> nil then
    result := end_.DateTime
  else
    result := 0;
end;

procedure TFhirPeriodHelper.SetEditString(const Value: String);
var
  s, c : String;
begin
  StringSplit(value, '->', s, c);
  if s.Trim <> '' then
    start := TFslDateTime.fromXML(s.Trim)
  else
    start := TFslDateTime.makeNull;
  if c.Trim <> '' then
    end_ := TFslDateTime.fromXML(c.Trim)
  else
    end_ := TFslDateTime.makeNull;
end;

{$IFNDEF FPC}
procedure iterateResource(resource : TFHIRResource; proc : TResourceIteratorProcedure);
begin
  iterateObject(resource, proc);
end;

procedure iterateObject(obj : TFHIRObject; proc : TResourceIteratorProcedure);
var
  child : TFHIRObject;
  pl : TFHIRPropertyList;
  p : TFHIRProperty;
begin
  proc(obj);
  if not obj.isPrimitive then
  begin
    pl := obj.createPropertyList(true);
    try
      for p in pl do
        for child in p.Values do
          iterateObject(child, proc);
    finally
      pl.free;
    end;
  end;
end;
{$ENDIF}

procedure TFhirConformanceRestResourceHelper.removeInteraction(type_: TFHIRTypeRestfulInteractionEnum);
var
  i : integer;
begin
  for i := self.interactionList.count - 1 downto 0 do
    if (self.interactionList[i].code = type_) then
      self.interactionList.DeleteByIndex(i);

end;

{ TFhirContactPointHelper }

class function TFhirContactPointHelper.fromEdit(n: String): TFhirContactPoint;
var
  ok : boolean;
  ch : char;
begin
  result := TFhirContactPoint.Create;
  try
    result.value := n;
    if n.contains('@') then
      result.system := ContactPointSystemEmail
    else
    begin
      ok := true;
      for ch in n do
        if not CharInSet(ch, [' ', '0'..'9', '+']) then
          ok := false;
      if ok then
        result.system := ContactPointSystemPhone;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFhirContactPointHelper.isEmail: boolean;
begin
  result := system in [ContactPointSystemEmail];
end;

function TFhirContactPointHelper.isPhoneOrFax: boolean;
begin
  result := system in [ContactPointSystemPhone, ContactPointSystemFax];
end;

{ TFHIRCompositionHelper }

function TFHIRCompositionHelper.summary: String;
begin
  result := title + '('+date.toString('c')+')';
end;

{ TFHIRPractitionerHelper }

function TFHIRPractitionerHelper.display: string;
begin
  if nameList.Count > 0 then
    result := gen(nameList[0])
  else
    result := '??'; // ??
end;

{ TFhirCanonicalListHelper }

function TFhirCanonicalListHelper.hasUri(uri: String): boolean;
var
  i : integer;
begin
  result := false;
  for i := Count - 1 downto 0 do
    if (Item(i).value = uri) then
      Exit(true);
end;

procedure TFhirCanonicalListHelper.removeUri(uri: String);
var
  i : integer;
begin
 for i := Count - 1 downto 0 do
   if (Item(i).value = uri) then
     Remove(i);
end;

{ TFhirElementDefinitionTypeHelper }

function TFhirElementDefinitionTypeHelper.GetProfile: String;
begin
  if Profilelist.count > 0 then
    result := Profilelist[0].value
  else
    result := '';
end;

function TFhirElementDefinitionTypeHelper.hasTargetProfile(uri: String): boolean;
var
  s : TFHIRCanonical;
begin
  result := false;
  for s in targetProfileList do
    if s.value = uri then
      exit(true);
end;

function TFhirElementDefinitionTypeHelper.targetProfileAsCSV: String;
var
  s : String;
  c : TFHIRCanonical;
begin
  s := '';
  for c in targetProfileList do
    s := s + ','+c.value;
  result := s.Substring(1);
end;

{ TFhirCanonicalHelper }

function TFhirCanonicalHelper.GetReference: String;
begin
  result := value;
end;

procedure TFhirCanonicalHelper.SetReference(const sValue: String);
begin
  Value := sValue;
end;

{ TFhirConceptMapGroupElementTargetDependsOnHelper }

function TFhirConceptMapGroupElementTargetDependsOnHelper.GetCode: String;
begin
  result := value;
end;

procedure TFhirConceptMapGroupElementTargetDependsOnHelper.SetCode(const sValue: String);
begin
  value := sValue;
end;

{ TFHIRCompositionSectionHelper }

function TFHIRCompositionSectionHelper.display: String;
begin
  result := title;
  if result = '' then
    result := gen(code);
  if result = '' then
    result := 'section';
end;

{ TFhirHealthcareServiceHelper }

function TFhirHealthcareServiceHelper.category: TFhirCodeableConcept;
begin
  if categoryList.Count = 0 then
    result := nil
  else
    result := categoryList[0];
end;

function TFhirHealthcareServiceHelper.type_: TFhirCodeableConcept;
begin
  if type_List.Count = 0 then
    result := nil
  else
    result := type_List[0];
end;

{ TFhirLocationHelper }

function TFhirLocationHelper.type_: TFhirCodeableConcept;
begin
  if type_List.Count = 0 then
    result := nil
  else
    result := type_List[0];
end;

{ TFhirPrimitiveTypeHelper }

constructor TFhirPrimitiveTypeHelper.Create(value: String);
begin
  Create;
  StringValue := value;
end;

{ TFhirRangeHelper }

function TFhirRangeHelper.either: TFhirQuantity;
begin
  if low <> nil then
    result := low
  else
    result := high;
end;

{ TFHIRBundleEntryHelper }

function TFHIRBundleEntryHelper.GetLinks(s: string): String;
var
  i : integer;
begin
  result := '';
  for i := 0 to link_List.count -  1 do
    if link_List[i].relation = s then
    begin
      result := link_List[i].url;
      exit;
    end;
end;

procedure TFHIRBundleEntryHelper.SetLinks(s: string; const Value: String);
var
  i : integer;
begin
  for i := 0 to link_List.count -  1 do
    if link_List[i].relation = s then
    begin
      link_List[i].url := value;
      exit;
    end;
  with link_List.Append do
  begin
    relation := s;
    url := value;
  end;
end;

function makeMarkdownOrString : TFhirMarkdown;
begin
  result := TFhirMarkdown.Create;
end;

{ TFhirBinaryHelper }

function TFhirBinaryHelper.GetContent: TBytes;
begin
  result := Data;
end;

procedure TFhirBinaryHelper.SetContent(const Value: TBytes);
begin
  Data := value;
end;

{ TFHIRExpressionHelper }

function TFHIRExpressionHelper.display: String;
begin
  if expression <> '' then
    result := expression
  else
    result := reference;
end;

{ TFhirTerminologyCapabilitiesHelper }

function TFhirTerminologyCapabilitiesHelper.context: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to useContextList.Count - 1 do
    result := result + gen(useContextList[i]);
end;

{ TFHIRHealthcareCardR4B }

function TFHIRHealthcareCardR4B.summarisePatient(pat: TFHIRPatient): String;
begin
  result := HumanNamesAsText(pat.nameList) +' ('+pat.birthDate.toString+')';
end;

function TFHIRHealthcareCardR4B.summariseImmunization(imm: TFHIRImmunization): String;
begin
  result := gen(imm.vaccineCode)+' ('+gen(imm.occurrence as TFhirDateTime)+')';
end;

function TFHIRHealthcareCardR4B.summariseObservation(obs: TFHIRObservation): String;
begin
  result := gen(obs.code)+' = '+gen(obs.value)+' ('+gen(obs.effective)+')';
end;

procedure TFHIRHealthcareCardR4B.htmlPatient(b : TFslHtmlBuilder; pat: TFHIRPatient);
var
  i : integer;
begin
  b.append('<table>');

  b.append('<tr><td>'+StringPlural('Name', pat.nameList.count)+'</td><td>');
  for i := 0 to pat.nameList.count - 1 do
  begin
    if (i > 0) then b.append('<br/>');
    genHtml(b, pat.nameList[i]);
  end;
  b.append('</td></tr>');

  b.append('<tr><td>Birth Date</td><td>');
  b.text(pat.birthDate.toString);
  b.append('</td></tr>');

  if pat.hasIdentifierList then
  begin
    b.append('<tr><td>'+StringPlural('Name', pat.nameList.count)+'</td><td>');
    for i := 0 to pat.IdentifierList.count - 1 do
    begin
      if (i > 0) then b.append('<br/>');
      genHtml(b, pat.IdentifierList[i]);
    end;
    b.append('</td></tr>');
  end;
  b.append('</table>');
end;

procedure TFHIRHealthcareCardR4B.htmlImmunization(b : TFslHtmlBuilder; imm: TFHIRImmunization; tx : TFHIRTerminologyService);
var
  i : integer;
begin
  b.append('<table>');

  b.append('<tr><td>'+StringPlural('Vaccine Code', imm.vaccineCode.codingList.count)+'</td><td>');
  for i := 0 to imm.vaccineCode.codingList.count - 1 do
  begin
    if (i > 0) then b.append('<br/>');
    genHtml(b, imm.vaccineCode.codingList[i], tx);
  end;
  b.append('</td></tr>');

  b.append('<tr><td>Date</td><td>'+(imm.occurrence as TFhirDateTime).toString+'</td></tr>');

  if (imm.manufacturer <> nil) and (imm.manufacturer.identifier <> nil) then
  begin
    b.append('<tr><td>Manufacturer</td><td>');
    genHtml(b, imm.manufacturer.identifier);
    b.append('</td></tr>');
  end;
  if (imm.lotNumber <> '') then
  begin
    b.append('<tr><td>Lot Number</td><td>');
    b.text(imm.lotNumber);
    b.append('</td></tr>');
  end;
  if (imm.hasPerformerList) and (imm.performerList[0].actor <> nil) and (imm.performerList[0].actor.display <> '') then
  begin
    b.append('<tr><td>Performer</td><td>');
    b.text(imm.performerList[0].actor.display);
    b.append('</td></tr>');
  end;
  if (imm.isSubpotentElement <> nil) then
    b.append('<tr><td>Sub Potent</td><td>'+BooleanToString(imm.isSubpotent)+'</td></tr>');
  b.append('</table>');
end;

procedure TFHIRHealthcareCardR4B.htmlObservation(b : TFslHtmlBuilder; obs: TFHIRObservation; tx : TFHIRTerminologyService);
var
  i : integer;
  vc : TFHIRCodeableConcept;
  vq : TFHIRQuantity;
begin
  b.append('<table>');

  b.append('<tr><td>'+StringPlural('Code', obs.code.codingList.count)+'</td><td>');
  for i := 0 to obs.code.codingList.count - 1 do
  begin
    if (i > 0) then b.append('<br/>');
    genHtml(b, obs.code.codingList[i], tx);
  end;
  b.append('</td></tr>');

  if (obs.value is TFHIRCodeableConcept) then
  begin
    vc := obs.value as TFHIRCodeableConcept;
    b.append('<tr><td>'+StringPlural('Value Code', vc.codingList.count)+'</td><td>');
    for i := 0 to vc.codingList.count - 1 do
    begin
      if (i > 0) then b.append('<br/>');
      genHtml(b, vc.codingList[i], tx);
    end;
    b.append('</td></tr>');
  end
  else  if (obs.value is TFHIRQuantity) then
  begin
    b.append('<tr><td>Value</td><td>');
    genHtml(b, obs.value as TFHIRQuantity);
    b.append('</td></tr>');
  end
  else
  begin
    b.append('<tr><td>Value</td><td>');
    b.text(obs.value.primitiveValue);
    b.append('</td></tr>');
  end;

  b.append('<tr><td>Status</td><td>');
  b.text(CODES_TFhirObservationStatusEnum[obs.status]);
  b.append('</td></tr>');

  b.append('<tr><td>Date</td><td>');
  b.text((obs.effective as TFhirDateTime).toString);
  b.append('</td></tr>');

  if (obs.hasReferenceRangeList) then
  begin
    b.append('<tr><td>'+StringPlural('Ref. Range', obs.ReferenceRangeList.count)+'</td><td>');
    for i := 0 to obs.ReferenceRangeList.count - 1 do
    begin
      if (i > 0) then b.append('<br/>');
      if obs.referenceRangeList[i].type_ <> nil then
      begin
        genHtml(b, obs.referenceRangeList[i].type_, tx);
        b.text(':');
      end;
      if (obs.referenceRangeList[i].low <> nil) or (obs.referenceRangeList[i].high <> nil) then
      begin
        genHtml(b, obs.referenceRangeList[i].low);
        b.text(' - ');
        genHtml(b, obs.referenceRangeList[i].high);
      end
      else if obs.referenceRangeList[i].text <> '' then
      begin
        b.text(obs.referenceRangeList[i].text);
      end;
    end;
    b.append('</td></tr>');
  end;
  b.append('</table>');
end;

procedure TFHIRHealthcareCardR4B.makeSummary;
var
  b : TFhirBundle;
  i : integer;
begin
  b := bundle as TFhirBundle;
  if (b.entryList.Count = 0) then
    FSummary := 'Error: Bundle is empty'
  else if not (b.entryList[0].resource is TFHIRPatient) then
    FSummary := 'Error: Bundle doesn''t start with Paitent'
  else
  begin
    FSummary := summarisePatient(b.entryList[0].resource as TFHIRPatient)+': ';
    for i := 1 to b.entryList.count - 1 do
    begin
      if i > 1 then
        FSummary := FSummary + '; ';
      if b.entryList[i].resource is TFHIRImmunization then
        FSummary := FSummary + summariseImmunization(b.entryList[i].resource as TFHIRImmunization)
      else if b.entryList[i].resource is TFHIRObservation then
       FSummary := FSummary + summariseObservation(b.entryList[i].resource as TFHIRObservation)
      else
       FSummary := FSummary + '?? Unknown';
    end;
  end;
end;

function TFHIRHealthcareCardR4B.htmlReport(tx : TFHIRTerminologyService): String;
var
  b : TFhirBundle;
  i : integer;
  bldr : TFslHtmlBuilder;
begin
  b := bundle as TFhirBundle;
  bldr := TFslHtmlBuilder.Create;
  try
    bldr.append('<html><body style="font-family: sans-serif">');
    if not isValid then
      bldr.append('<p style="font-color: maroon">This card is not valid: '+EncodeXML(validationMessage)+'</p>');
    for i := 0 to b.entryList.count - 1 do
    begin
      if b.entryList[i].resource = nil then
        bldr.append('<h2>Nil</h2><p>No Resource for this entry</p>')
      else
      begin
        bldr.append('<h2>'+b.entryList[i].resource.fhirType+'</h2>');
        if b.entryList[i].resource is TFHIRPatient then
          htmlPatient(bldr, b.entryList[i].resource as TFHIRPatient)
        else if b.entryList[i].resource is TFHIRImmunization then
          htmlImmunization(bldr, b.entryList[i].resource as TFHIRImmunization, tx)
        else if b.entryList[i].resource is TFHIRObservation then
          htmlObservation(bldr, b.entryList[i].resource as TFHIRObservation, tx)
        else
         bldr.append('<p>Not supported</p>');
      end;
    end;
    bldr.append('</body></html>');
    result := bldr.toString;
  finally
    bldr.free;
  end;
end;

procedure genHtml(b : TFslHtmlBuilder; name : TFhirHumanName);
var
  i : integer;
begin
  if (name = nil) then
    exit;
  for i := 0 to name.prefixList.count - 1 do
  begin
    b.text(name.prefixList[i].value);
    b.text(' ');
  end;
  b.start('b');
  for i := 0 to name.givenList.count - 1 do
  begin
    b.text(name.givenList[i].value);
    b.text(' ');
  end;
  b.Append(name.family);
  b.finish('b');
  for i := 0 to name.suffixList.count - 1 do
  begin
    b.text(' ');
    b.text(name.suffixList[i].value);
  end;
  if (name.use <> NameUseNull) or (name.period <> nil) then
  begin
    b.text(' ');
    b.text('(');
    if (name.use <> NameUseNull) then
    begin
      b.text('use = ');
      b.text(CODES_TFhirNameUseEnum[name.use]);
    end;
    if (name.period <> nil) then
    begin
      if (name.use <> NameUseNull) then
        b.text(', ');
      b.text('period = ');
      genHtml(b, name.period);
    end;
    b.Append(')');
  end;
end;

procedure genHtml(b : TFslHtmlBuilder; id : TFhirIdentifier);
begin
  if (id = nil) then
    exit;
  b.append(id.value);
  if (id.system <> '') then
    b.append(' in '+id.system);
end;

procedure genHtml(b : TFslHtmlBuilder; p : TFhirPeriod); overload;
begin
  if (p = nil) then
    exit;
  b.append(gen(p));
end;

procedure genHtml(b : TFslHtmlBuilder; c : TFhirCoding; tx : TFHIRTerminologyService); overload;
var
  d : String;
begin
  if (c = nil) then
    exit;
  if c.display = '' then
    d := tx.lookupCode(c.system, c.code)
  else
    d := c.display;

  if (d = '') then
  begin
    b.text(csName(c.system));
    b.text('#');
    b.text(c.code);
  end
  else
  begin
    b.text(d);
    b.text(' (');
    b.text(csName(c.system));
    b.text('#');
    b.text(c.code);
    b.text(')');
  end;
end;

procedure genHtml(b: TFslHtmlBuilder; c: TFhirCodeableConcept; tx: TFHIRTerminologyService);
begin
  if (c.text <> '') then
    b.text(c.text)
  else if c.hasCoding then
    genHtml(b, c.codingList[0], tx)
  else
    b.text('??');
end;

procedure genHtml(b : TFslHtmlBuilder; q : TFhirQuantity); overload;
begin
  if (q = nil) then
    exit;
  if (q.comparator <> QuantityComparatorNull) then
    b.text(CODES_TFhirQuantityComparatorEnum[q.comparator]);
  b.text(q.value+' '+q.unit_);
  if (q.code <> '') then
  begin
    b.append(' (');
    b.text(csName(q.system)+'#'+q.code);
    b.append(')');
  end;
end;

function describeResource(r: TFHIRResource): String;
begin
   if r is TFhirMetadataResource then
     result := (r as TFhirMetadataResource).url
   else
   begin
     case r.ResourceType of
    {$IFDEF FHIR_DEVICE}frtDevice : result := (r as TFHIRDevice).Summary; {$ENDIF}
    {$IFDEF FHIR_ENCOUNTER}frtEncounter : result := (r as TFHIREncounter).Summary; {$ENDIF}
    {$IFDEF FHIR_ENDPOINT}frtEndpoint : result := (r as TFHIREndPoint).Summary; {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE}frtEpisodeOfCare : result := (r as TFHIREpisodeOfCare).Summary; {$ENDIF}
    {$IFDEF FHIR_GROUP}frtGroup : result := (r as TFHIRGroup).Summary; {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE}frtHealthcareService : result := (r as TFHIRHealthcareService).Summary; {$ENDIF}
    {$IFDEF FHIR_LOCATION}frtLocation : result := (r as TFHIRLocation).Summary; {$ENDIF}
    {$IFDEF FHIR_MEDICATION}frtMedication : result := (r as TFHIRMedication).Summary; {$ENDIF}
    {$IFDEF FHIR_ORGANIZATION}frtOrganization : result := (r as TFHIROrganization).Summary; {$ENDIF}
    {$IFDEF FHIR_PATIENT}frtPatient : result := (r as TFHIRPatient).Summary; {$ENDIF}
    {$IFDEF FHIR_PERSON}frtPerson : result := (r as TFHIRPerson).Summary; {$ENDIF}
    {$IFDEF FHIR_PRACTITIONER}frtPractitioner : result := (r as TFHIRPractitioner).Summary; {$ENDIF}
    {$IFDEF FHIR_PRACTITIONERROLE}frtPractitionerRole : result := (r as TFHIRPractitionerRole).Summary; {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON}frtRelatedPerson : result := (r as TFHIRRelatedPerson).Summary; {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE}frtSubstance : result := (r as TFHIRSubstance).Summary; {$ENDIF}
     end;
   end;
end;

{ TFhirEncounterHelper }

function TFhirEncounterHelper.summary: String;
begin
  if hasIdentifierList then
    result := gen(identifierList[0]);
  if period <> nil then
    result := result +' ['+gen(period)+']';
  result := result.Trim;
end;

{ TFhirEpisodeOfCareHelper }

function TFhirEpisodeOfCareHelper.summary: String;
begin
  if hasIdentifierList then
    result := gen(identifierList[0]);
  if period <> nil then
    result := result +' ['+gen(period)+']';
  result := result.Trim;
end;

{ TFhirGroupHelper }

function TFhirGroupHelper.summary: String;
begin
  if name <> '' then
    result := name
  else
    result := CODES_TFhirGroupTypeEnum[type_] + ' Group';
end;

{ TFhirHealthcareServiceHelper }

function TFhirHealthcareServiceHelper.summary: String;
begin
  if name <> '' then
    result := name
  else if category <> nil then
    result := gen(category) + ' Service'
  else
    result := '??';
end;

{ TFhirLocationHelper }

function TFhirLocationHelper.summary: String;
begin
  if name <> '' then
    result := name
  else if description <> '' then
    result := description
  else
    result := '??';
end;

{ TFhirMedicationHelper }

function TFhirMedicationHelper.summary: String;
begin
  result := gen(code);
end;

{ TFhirOrganizationHelper }

function TFhirOrganizationHelper.summary: String;
begin
  if name <> '' then
    result := name
  else
    result := '??';
end;

{ TFhirPatientHelper }

function TFhirPatientHelper.summary: String;
begin
  if hasNameList then
    result := gen(nameList[0])
  else
    result := '??';
end;

function TFhirPatientHelper.genderPlus: String;
begin
  result := 'todo' + CODES_TFhirAdministrativeGenderEnum[gender];
end;

{ TFhirPersonHelper }

function TFhirPersonHelper.summary: String;
begin
  if hasNameList then
    result := gen(nameList[0])
  else
    result := '??';
end;

{ TFhirPractitionerHelper }

function TFhirPractitionerHelper.summary: String;
begin
  if hasNameList then
    result := gen(nameList[0])
  else
    result := '??';
end;

{ TFhirRelatedPersonHelper }

function TFhirRelatedPersonHelper.summary: String;
begin
  if hasNameList then
    result := gen(nameList[0])
  else
    result := '??';
end;

{ TFhirSubstanceHelper }

function TFhirSubstanceHelper.summary: String;
begin
  if code <> nil then
    result := gen(code)
  else
    result := '??';
end;

{ TFHIRDeviceHelper }

function TFHIRDeviceHelper.summary: String;
begin
  if hasDeviceNameList  then
    result := DeviceNameList[0].name
  else if url <> '' then
    result := url
  else
    result := '??';
end;

{ TFhirEndpointHelper }

function TFhirEndpointHelper.summary: String;
begin
  if name <> '' then
    result := name
  else if address <> '' then
    result := address
  else
    result := '??';
end;

{ TFHIRPractitionerRoleHelper }

function TFHIRPractitionerRoleHelper.summary: String;
begin
  result := '??';
end;

{ TFhirBundleEntryListHelper }

function TFhirBundleEntryListHelper.append(url: String): TFHIRBundleEntry;
begin
  result := append;
  result.fullUrl := url;
end;

end.

