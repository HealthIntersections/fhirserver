unit fhir2_utilities;

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

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
{$I fhir2.inc}

interface

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Generics.Collections, ZLib,

  fsl_base, fsl_utilities, fsl_http, fsl_stream, fsl_json, fsl_xml, 
  fsl_fetcher,

   fhir_parser, fhir_objects, fhir_xhtml, fhir_utilities,
  fhir2_types, fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_other, fhir2_resources_clinical, fhir2_resources, fhir2_constants, fhir2_context;


const
  ExceptionTypeTranslations : array [TFhirIssueType] of TFhirIssueTypeEnum = (IssueTypeNull, IssueTypeInvalid, IssueTypeStructure, IssueTypeRequired, IssueTypeValue,
    IssueTypeInvariant, IssueTypeSecurity, IssueTypeLogin, IssueTypeUnknown, IssueTypeExpired, IssueTypeForbidden, IssueTypeSuppressed, IssueTypeProcessing,
    IssueTypeNotSupported, IssueTypeDuplicate, IssueTypeNotFound, IssueTypeTooLong, IssueTypeCodeInvalid, IssueTypeExtension, IssueTypeTooCostly, IssueTypeBusinessRule,
    IssueTypeConflict, IssueTypeIncomplete, IssueTypeTransient, IssueTypeLockError, IssueTypeNoStore, IssueTypeException, IssueTypeTimeout, IssueTypeThrottled, IssueTypeInformational);

  CANONICAL_URL_RESOURCE_TYPES = [
    {$IFDEF FHIR_CONFORMANCE}frtConformance, {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP}frtConceptMap, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE}frtImplementationGuide, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDEINPUT}frtImplementationGuideInput, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDEOUTPUT}frtImplementationGuideOutput, {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM}frtNamingSystem, {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION}frtOperationDefinition, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE}frtQuestionnaire, {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER}frtSearchParameter, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION}frtStructureDefinition, {$ENDIF}
    frtValueSet];


function HumanNamesAsText(names : TFhirHumanNameList):String;
function HumanNameAsText(name : TFhirHumanName):String;
function GetEmailAddress(contacts : TFhirContactPointList):String;
function ResourceTypeByName(name : String) : TFhirResourceType;
function isResourceName(name : String; canbeLower : boolean = false) : boolean;

Function RecogniseFHIRResourceName(Const sName : String; out aType : TFhirResourceType): boolean;
Function RecogniseFHIRResourceManagerName(Const sName : String; out aType : TFhirResourceType): boolean;
function MakeParser(oWorker : TFHIRWorkerContext; const lang : THTTPLanguages; aFormat: TFHIRFormat; oContent: TStream; policy : TFHIRXhtmlParserPolicy): TFHIRParser; overload;
function MakeParser(oWorker : TFHIRWorkerContext; const lang : THTTPLanguages; aFormat: TFHIRFormat; content: TBytes; policy : TFHIRXhtmlParserPolicy): TFHIRParser; overload;
function MakeParser(oWorker : TFHIRWorkerContext; const lang : THTTPLanguages; mimetype : String; content: TBytes; policy : TFHIRXhtmlParserPolicy): TFHIRParser; overload;
function MakeComposer(style : TFHIROutputStyle; const lang : THTTPLanguages; mimetype : String; worker : TFHIRWorkerContext) : TFHIRComposer;
function geTFhirResourceNarrativeAsText(resource : TFhirDomainResource) : String;
function fullResourceUri(base: String; aType : TFhirResourceType; id : String) : String; overload;
function fullResourceUri(base: String; url : String) : String; overload;
function fullResourceUri(base: String; ref : TFhirReference) : String; overload;

procedure listReferences(resource : TFhirResource; list : TFhirReferenceList);
procedure listAttachments(resource : TFhirResource; list : TFhirAttachmentList);
function FindContainedResource(resource : TFhirDomainResource; ref : TFhirReference) : TFhirResource; overload;
function FindContainedResource(resource : TFhirDomainResource; ref : string) : TFhirResource; overload;
function LoadFromFormParam(worker : TFHIRWorkerContext; part : TMimePart; const lang : THTTPLanguages) : TFhirResource;
function LoadDTFromFormParam(worker : TFHIRWorkerContext; part : TMimePart; const lang : THTTPLanguages; name : String; type_ : TFHIRTypeClass) : TFhirType;
function LoadDTFromParam(worker : TFHIRWorkerContext; value : String; const lang : THTTPLanguages; name : String; type_ : TFHIRTypeClass) : TFhirType;

function BuildOperationOutcome(const lang : THTTPLanguages; e : exception; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;
Function BuildOperationOutcome(const lang : THTTPLanguages; message : String; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;

function getChildMap(profile : TFHIRStructureDefinition; name, path, nameReference : String) : TFHIRElementDefinitionList; overload;
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

procedure BuildNarrative(vs : TFhirValueSet); overload;
function ComposeJson(worker: TFHIRWorkerContext; r : TFhirResource) : String; overload;

function fileToResource(name : String) : TFhirResource; overload;
function fileToResource(name : String; var format : TFHIRFormat) : TFhirResource; overload;
function streamToResource(stream : TStream; var format : TFHIRFormat) : TFhirResource;
function bytesToResource(bytes : TBytes) : TFhirResource; overload;
function bytesToResource(bytes : TBytes; var format : TFHIRFormat) : TFhirResource; overload;
procedure resourceToFile(res : TFhirResource; name : String; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal);
procedure resourceToStream(res : TFhirResource; stream : TStream; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal);
function resourceToString(res : TFhirResource; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal) : String;
function resourceToBytes(res : TFhirResource; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal) : TBytes;

function parseParamsFromForm(stream : TStream) : TFHIRParameters;

type

  TFHIRProfileStructureHolder = TFhirStructureDefinitionSnapshot;
  TFHIRProfileStructureElement = TFhirElementDefinition;
  TFhirProfileStructureElementList = TFhirElementDefinitionList;
  TFhirProfileStructureElementDefinitionBinding = TFhirElementDefinitionBinding;
  TFhirConceptMapGroup = TFhirConceptMap;
  TFhirConceptMapGroupList = TFhirConceptMap;
  TFhirConceptMapGroupElement = TFhirConceptMapElement;
  TFhirConceptMapGroupElementTarget = TFhirConceptMapElementTarget;
  TFhirConceptMapGroupElementTargetList = TFhirConceptMapElementTargetList;

  TFhirCapabilityStatement = TFhirConformance;
  TFhirCapabilityStatementRest = TFhirConformanceRest;
  TFhirCapabilityStatementRestResourceSearchParamList = TFhirConformanceRestResourceSearchParamList;
  TFhirCapabilityStatementRestResourceSearchParam = TFhirConformanceRestResourceSearchParam;
  TFhirCapabilityStatementRestResource = TFhirConformanceRestResource;
  TFhirCapabilityStatementRestOperation = TFhirConformanceRestOperation;
  TFhirCapabilityStatementContact = TFhirConformanceContact;
  TFhirCapabilityStatementSoftware = TFhirConformanceSoftware;
  TFhirCapabilityStatementImplementation = TFhirConformanceImplementation;
  TFhirCapabilityStatementRestSecurity = TFhirConformanceRestSecurity;


 TFhirPeriodHelper = class helper for TFhirPeriod
  private
    function GetEditString: String;
    procedure SetEditString(const Value: String);
  public
    class function fromEdit(s : String) : TFhirPeriod;
    property editString : String read GetEditString write SetEditString;
    function point : TDateTime;
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

  TFHIRElementHelper = class helper for TFHIRElement
  public
    function addExtension(url : String) : TFHIRExtension; overload;
    procedure addExtension(url : String; t : TFhirType); overload;
    procedure addExtension(url : String; v : String); overload;
    procedure addExtensionUri(url : String; v : String); overload;
    function hasExtension(url : String) : boolean;
    function getExtension(url : String) : Integer;
    function getExtensionValue(url : String) : TFHIRType;
    function getExtensionCount(url : String) : Integer;
    function getExtensionString(url : String) : String; overload;
    function getExtensionString(url : String; index : integer) : String; overload;
    procedure removeExtension(url : String);
    procedure setExtension(url : String; t : TFHIRType);
    procedure setExtensionString(url, value : String);
    procedure setExtensionInteger(url, value : String);
    procedure setExtensionDecimal(url, value : String);
    procedure setExtensionURI(url, value : String);
    procedure setExtensionDate(url, value : String);
    procedure setExtensionDateTime(url, value : String);
    procedure setExtensionTime(url, value : String);
    procedure setExtensionCode(url, value : String);
  end;

  TFHIRBackboneElementHelper = class helper for TFHIRBackboneElement
  public
    procedure checkNoModifiers(place, role : String); overload;
    procedure checkNoModifiers(place, role : String; exempt : TArray<String>); overload;
  end;

  TFhirElementDefinitionTypeHelper = class helper for TFhirElementDefinitionType
  private
    function GetProfile: String;
  public
    property profile : String read GetProfile;
  end;

  TFhirElementDefinitionHelper = class helper for TFhirElementDefinition
  public
    function hasType(t : String; out profile : String) : boolean;  overload;
    function hasType(t : String) : boolean; overload;

    function ContentReference : String;
  end;

  TFhirStructureDefinitionHelper = class helper for TFhirStructureDefinition
  public
    function baseDefinition : String;
    function baseType : String;
    function type_ : String;
  end;

  TFhirQuantityHelper = class helper for TFhirQuantity
  private
    function GetEditString: String;
    procedure SetEditString(vs: String);
  public
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
  end;

  TFHIRDomainResourceHelper = class helper (TFHIRResourceHelper) for TFHIRDomainResource
  private
    function GetContained(id: String): TFhirResource;
  public
    property Contained[id : String] : TFhirResource read GetContained; default;
    procedure collapseAllContained;
    function addExtension(url : String; t : TFhirType) : TFhirExtension; overload;
    function addExtension(url : String) : TFhirExtension; overload;
    function addExtension(url : String; v : String) : TFhirExtension; overload;
    function hasExtension(url : String) : boolean;
    function getExtension(url : String) : Integer;
    function getExtensionValue(url : String) : TFHIRType;
    function getExtensionCount(url : String) : Integer;
    function getExtensionString(url : String) : String; overload;
    function getExtensionString(url : String; index : integer) : String; overload;
    procedure removeExtension(url : String);
    procedure setExtensionString(url, value : String);
    procedure checkNoModifiers(place, role : String);
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

  TFhirProfileStructureSnapshotElementDefinitionTypeListHelper = class helper for TFhirElementDefinitionList
  public
    function summary : String;
  end;

  TFHIRConformanceHelper = class helper (TFHIRDomainResourceHelper) for TFHIRConformance
  public
    function rest(type_ : TFhirResourceType) : TFhirConformanceRestResource;
    procedure checkCompatible;
  end;

  {$IFDEF FHIR_OPERATIONDEFINITION}
  TFHIROperationDefinitionHelper = class helper (TFHIRDomainResourceHelper) for TFHIROperationDefinition
  private
    function Gettype2_: boolean;
    procedure Settype2_(const Value: boolean);
  public
    function resourceList : TFhirEnumList;
    property type_ : boolean read Gettype2_ write Settype2_;
  end;
  {$ENDIF}

  TFHIRCodeableConceptHelper = class helper (TFHIRElementHelper) for TFHIRCodeableConcept
  public
    constructor Create(system, code : String); overload;
    function hasCode(System, Code : String) : boolean;
    function fromSystem(System : String; required : boolean = false) : String; overload;
    function fromSystem(Systems : TArray<String>; required : boolean = false) : String; overload;
  end;

  TFHIRCodeableConceptListHelper = class helper for TFHIRCodeableConceptList
  private
    function GetHasCode(System, Code: String): boolean;
    procedure SetHasCode(System, Code: String; const Value: boolean);
  public
    property hasCode[System, Code : String] : boolean read GetHasCode write SetHasCode;
  end;

  TFhirConformanceRestResourceHelper = class helper (TFHIRElementHelper) for TFhirConformanceRestResource
  public
    function interaction(type_ : TFhirTypeRestfulInteractionEnum) : TFhirConformanceRestResourceInteraction;
  end;

  TFHIRContactPointListHelper = class helper for TFhirContactPointList
  public
    function system(type_ : TFHIRContactPointSystemEnum) : String;
    procedure setSystem(type_ : TFHIRContactPointSystemEnum; value : String);
  end;

  TFhirValueSetContactListHelper = class helper for TFhirValueSetContactList
  public
    function system(type_ : TFHIRContactPointSystemEnum) : String;
    procedure setSystem(type_ : TFHIRContactPointSystemEnum; value : String);
  end;

  TFhirConceptMapGroupElementTargetHelper = class helper for TFhirConceptMapGroupElementTarget
  private
    function GetCommentElement: TFHIRString;
    procedure SetCommentElement(const Value: TFHIRString);
  public
    property commentElement : TFHIRString read GetCommentElement write SetCommentElement;
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

    class function fromEdit(s : String) : TFhirCoding;
    property editString : String read GetEditString write SetEditString;
  end;


  TFhirValueSetHelper = class helper for TFhirValueSet
  public
    function context : string;
    function source : string;
    function conceptList : TFhirValueSetCodeSystemConceptList;
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

  TFhirConceptMapElementHelper = class helper (TFhirElementHelper) for TFhirConceptMapElement
  private
    function getsystem: String;
    procedure SetSystem(const Value: String);
  public
    function systemElement : TFhirUri;
    property system : String read GetSystem write SetSystem;
  end;

  TFhirConceptMapElementTargetHelper = class helper (TFhirElementHelper) for TFhirConceptMapElementTarget
  private
    function getsystem: String;
    procedure SetSystem(const Value: String);
  public
    function systemElement : TFhirUri;
    property system : String read GetSystem write SetSystem;
  end;

  TFhirConceptMapHelper = class helper (TFhirResourceHelper) for TFhirConceptMap
  public
    function conceptList : TFhirConceptMapElementList;
    function context : string;
    function sourceDesc: String;
    function targetDesc: String;
    function groupList : TFhirConceptMap;
  end;

  TFHIRBundleEntryHelper = class helper (TFhirElementHelper) for TFHIRBundleEntry
  private
    function GetLinks(s: string): String;
    procedure SetLinks(s: string; const Value: String);
  public
    property Links[s : string] : String read GetLinks write SetLinks;
  end;

  TFHIRObservationHelper = class helper for TFHIRObservation
  public
    function addComponent(system, code: String) : TFhirObservationComponent;
    function getComponent(system, code: String; var comp : TFhirObservationComponent) : boolean; overload;
    function getComponent(system : String; var comp : TFhirObservationComponent) : boolean; overload;
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
    function generatePresentation : String;
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
    procedure AddParameter(name: String; value: TFhirType); overload;
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
    procedure AddParameter(name: String; value: TFhirType); overload;
    procedure AddParameter(name: String; value: TFhirResource); overload;
    procedure AddParameter(name: String; value: boolean); overload;
    procedure AddParameter(name, value: string); overload;
    function AddParameter(name: String) : TFhirParametersParameter; overload;
    procedure AddParameter(p :  TFhirParametersParameter); overload;
  end;

  TFhirResourceMetaHelper = class helper for TFhirMeta
  private
    function GetTag(system, code: String): TFhirCoding;
  public
    function HasTag(system, code : String)  : boolean;
    function addTag(system, code, display : String) : TFhirCoding;
    function removeTag(system, code : String) : boolean;
    function removeLabel(system, code : String) : boolean;
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

  TFhirValueSetCodeSystemHelper = class helper for TFhirValueSetCodeSystem
  private
    function locate(parent: TFhirValueSetCodeSystemConcept; list: TFhirValueSetCodeSystemConceptList; code : String; var foundParent, foundConcept: TFhirValueSetCodeSystemConcept): boolean;
    procedure scanForSubsumes(parentList, conceptList : TFhirValueSetCodeSystemConceptList; code : String);

  public
    function getParents(concept : TFhirValueSetCodeSystemConcept) : TFhirValueSetCodeSystemConceptList;
    function getChildren(concept : TFhirValueSetCodeSystemConcept) : TFhirValueSetCodeSystemConceptList;
  end;

  TFhirValueSetExpansionHelper = class helper for TFhirValueSetExpansion
  public
    procedure AddParam(name, value : String); overload;
    procedure AddParam(name : String; value : boolean); overload;
  end;

  TFhirSystemVersionProcessingModeEnum = (SystemVersionProcessingModeDefault, SystemVersionProcessingModeCheck, SystemVersionProcessingModeOverride);

  TFhirExpansionProfileFixedVersion = class (TFslObject)
  private
    FVersion: String;
    FSystem: String;
    FMode: TFhirSystemVersionProcessingModeEnum;
  protected
    function sizeInBytesV : cardinal; override;
  public
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property mode : TFhirSystemVersionProcessingModeEnum read FMode write FMode;
  end;

  TFhirExpansionProfile = class (TFslObject)
  private
    FincludeDefinition: boolean;
    FlimitedExpansion: boolean;
    FdisplayLanguage: String;
    FexcludeNested: boolean;
    FactiveOnly: boolean;
    FexcludeNotForUI: boolean;
    FexcludePostCoordinated: boolean;
    FincludeDesignations: boolean;
    FcodeSystemList : TFslList<TFhirExpansionProfileFixedVersion>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TFhirExpansionProfile; overload;
    function Clone : TFhirExpansionProfile; overload;
    function hash : string;

    property includeDefinition : boolean read FincludeDefinition write FincludeDefinition;
    property limitedExpansion : boolean read FlimitedExpansion write FlimitedExpansion;
    property displayLanguage : String read FdisplayLanguage write FdisplayLanguage;
    property includeDesignations : boolean read FincludeDesignations write FincludeDesignations;
    property activeOnly : boolean read FactiveOnly write FactiveOnly;
    property excludeNested : boolean read FexcludeNested write FexcludeNested;
    property excludeNotForUI : boolean read FexcludeNotForUI write FexcludeNotForUI;
    property excludePostCoordinated : boolean read FexcludePostCoordinated write FexcludePostCoordinated;

    property fixedVersionList : TFslList<TFhirExpansionProfileFixedVersion> read FcodeSystemList;
    function codeSystem : TFhirExpansionProfile;
    function include : TFhirExpansionProfile;
    class function defaultProfile : TFhirExpansionProfile;
  end;

  TFHIRCodeSystem = TFhirValueSet;
  TFhirCodeSystem2 = TFHIRValueSetCodeSystem;
  TFHIRCodeSystemList = TFHIRValueSetList;
  TFhirCodeSystemConcept = TFhirValueSetCodeSystemConcept;
  TFhirCodeSystemConceptList = TFhirValueSetCodeSystemConceptList;
  TFhirCodeSystemConceptDesignation = TFhirValueSetCodeSystemConceptDesignation;
  {$IFDEF FHIR_NAMINGSYSTEM}
  TFHIRNamingSystemHelper = class helper for TFHIRNamingSystem
  public
    function hasOid(oid : String) : boolean;
    function getUri : String;
  end;
  {$ENDIF}


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

const
  frtCodeSystem = frtValueSet;
  RestfulCapabilityModeServer = RestfulConformanceModeServer;
  PublicationStatusNull = ConformanceResourceStatusNull;
  PublicationStatusDraft = ConformanceResourceStatusDraft;
  PublicationStatusActive = ConformanceResourceStatusActive;
  PublicationStatusRetired = ConformanceResourceStatusRetired;


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

function gen(t : TFhirType):String; overload;

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

implementation

uses
  {$IFDEF STACK_DUMPS}
  JclDebug,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Registry,
  {$ENDIF}
  fhir2_elementmodel, fhir2_parser;

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
      ts.Free;
    end;
  finally
    st.Free;
  end;
  end
  else
    result := '';
end;
  {$ENDIF}


function MakeParser(oWorker : TFHIRWorkerContext; const lang : THTTPLanguages; aFormat: TFHIRFormat; content: TBytes; policy : TFHIRXhtmlParserPolicy): TFHIRParser;
var
  mem : TBytesStream;
begin
  mem := TBytesStream.Create(content);
  try
    result := MakeParser(oWorker, lang, aformat, mem, policy);
  finally
    mem.Free;
  end;
end;

function MakeParser(oWorker : TFHIRWorkerContext; const lang : THTTPLanguages; mimetype : String; content: TBytes; policy : TFHIRXhtmlParserPolicy): TFHIRParser; overload;
begin
  if mimeType.Contains('application/json') or mimeType.Contains('application/fhir+json') Then
    result := TFHIRParsers2.parser(oWorker.Link, ffJson, lang)
  else if mimeType.Contains('text/plain') then
    result := TFHIRParsers2.parser(oWorker.Link, ffText, lang)
  else if mimeType.Contains('application/xml') or mimeType.Contains('application/fhir+xml') or mimeType.Contains('text/xml')  then
    result := TFHIRParsers2.parser(oWorker.Link, ffXml, lang)
  else
    result := TFHIRParsers2.parser(oWorker.Link, DetectFormat(content), lang);
  try
    result.ParserPolicy := policy;
    result.Link;
  finally
    result.free;
  end;
end;
function MakeParser(oWorker : TFHIRWorkerContext; const lang : THTTPLanguages; aFormat: TFHIRFormat; oContent: TStream; policy : TFHIRXhtmlParserPolicy): TFHIRParser;
begin
  if aFormat in [ffUnspecified, ffXhtml] then
    result := TFHIRParsers2.parser(oWorker.Link, DetectFormat(oContent), lang)
  else
    result := TFHIRParsers2.parser(oWorker.Link, aFormat, lang);
  try
    result.source := oContent;
    result.ParserPolicy := policy;
    result.Parse;
    result.Link;
  finally
    result.free;
  end;
end;

function MakeComposer(Style : TFHIROutputStyle; const lang : THTTPLanguages; mimetype : String; worker : TFHIRWorkerContext) : TFHIRComposer;
begin
  if mimeType.StartsWith('text/xml') or mimeType.StartsWith('application/xml') or mimeType.StartsWith('application/fhir+xml') or (mimetype = 'xml') then
    result := TFHIRParsers2.composer(worker.link, ffXml, lang, Style)
  else if mimeType.StartsWith('text/json') or mimeType.StartsWith('application/json') or mimeType.StartsWith('application/fhir+json') or (mimetype = 'json') then
    result := TFHIRParsers2.composer(worker.link, ffJson, lang, Style)
//  else if mimeType.StartsWith('text/html') or mimeType.StartsWith('text/xhtml') or mimeType.StartsWith('application/fhir+xhtml') or (mimetype = 'xhtml') then
//    result := TFHIRXhtmlComposer.Create(worker.link, Style, lang)
  else
    raise EFHIRException.create('Format '+mimetype+' not recognised');
end;

function ResourceTypeByName(name : String) : TFhirResourceType;
var
  index : Integer;
begin
  index := StringArrayIndexOfSensitive(CODES_TFhirResourceType, name);
  if index < 1 then
    raise EFHIRException.create('Unknown resource name "'+name+'"');
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
    (value.repeat_.frequency <> '') and (value.repeat_.period <> '') and (value.repeat_.periodunits <> UnitsOfTimeNull) then
  begin
    result := MIN_DATE;
    for i := 0 to value.eventList.count - 1 do
      result := DateTimeMax(result, AsUTCMax(value.eventList[i]));
    if result = MIN_DATE then
      result := MAX_DATE
    else
    begin
      case value.repeat_.periodunits of
        UnitsOfTimeS : duration := DATETIME_SECOND_ONE;
        UnitsOfTimeMin : duration := DATETIME_MINUTE_ONE;
        UnitsOfTimeH : duration := DATETIME_HOUR_ONE;
        UnitsOfTimeD : duration := 1;
        UnitsOfTimeWk : duration := 7;
        UnitsOfTimeMo : duration := 30;
        UnitsOfTimeA : duration := 365 // todo - how to correct for leap years?;
      else
        raise EFHIRException.create('unknown duration units "'+value.repeat_.periodunitsElement.value+'"');
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

function BuildOperationOutcome(const lang : THTTPLanguages; e : exception; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome;
begin
  result := BuildOperationOutcome(lang, e.message, issueCode);
end;

Function BuildOperationOutcome(const lang : THTTPLanguages; message : String; issueCode : TFhirIssueTypeEnum = IssueTypeNull) : TFhirOperationOutcome; overload;
var
  outcome : TFhirOperationOutcome;
  report :  TFhirOperationOutcomeIssue;
begin
  outcome := TFhirOperationOutcome.create;
  try
    outcome.text := TFhirNarrative.create;
    outcome.text.status := NarrativeStatusGenerated;
    outcome.text.div_ := TFHIRXhtmlParser.Parse(lang, xppReject, [], '<div><p>'+FormatTextToHTML(message)+'</p></div>');
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
begin
  if (coding = nil) then
     result := ''
  else if (coding.DisplayElement <> nil) then
    result := coding.Display
  else if (coding.CodeElement <> nil) then
    result := coding.Code
  else
    result := '';
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


function gen(extension : TFHIRExtension):String; overload;
begin
  if extension = nil then
    result := ''
  else if (extension.Value is TFHIRCode) then
    result := TFHIRCode(extension.value).value
  else if (extension.value is TFHIRCoding) then
    result := gen(TFHIRCoding(extension.value))
  else
    raise EFHIRException.create('Unhandled type '+extension.Value.ClassName);
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


procedure addDefineRowToTable(t : TFhirXHtmlNode; c : TFhirValueSetCodeSystemConcept; indent : integer);
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

procedure generateDefinition(x : TFhirXHtmlNode; vs : TFHIRValueSet);
var
  p, t : TFhirXHtmlNode;
  i : integer;
begin
  p := x.addTag('p');
  p.addText('This value set defines it''s own terms in the system '+vs.CodeSystem.System);
  t := x.addTag('table');
  addTableHeaderRowStandard(t);
  for i := 0 to vs.CodeSystem.ConceptList.Count - 1 do
    addDefineRowToTable(t, vs.CodeSystem.ConceptList[i], 0);
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
   raise EFHIRException.create('todo');
end;

procedure BuildNarrative(vs : TFhirValueSet);
var
  x, h, p : TFhirXHtmlNode;
begin
  x := TFhirXHtmlNode.create;
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
      if (vs.CodeSystem <> nil) then
        generateDefinition(x, vs);
      if (vs.Compose <> nil) then
        generateComposition(x, vs);
    end;

    if (vs.Text = nil) then
      vs.Text := TFhirNarrative.create;
    vs.Text.div_ := x.link;
    vs.Text.status := NarrativeStatusGenerated;
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
    for i := 0 to name.familyList.Count - 1 do
      result := result + name.familyList[i].value+' ';
  end;
end;

function LoadDTFromFormParam(worker : TFHIRWorkerContext; part : TMimePart; const lang : THTTPLanguages; name : String; type_ : TFHIRTypeClass) : TFhirType;
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
        parser := TFHIRParsers2.parser(worker.link, ffJson, lang)
      else if StringStartsWithInsensitive(ct, 'text/xml') or StringStartsWithInsensitive(ct, 'application/xml') or
          StringStartsWithInsensitive(ct, 'application/fhir+xml') or StringStartsWithInsensitive(ct, 'application/xml+fhir') or StringStartsWithInsensitive(ct, 'xml') Then
        parser := TFHIRParsers2.parser(worker.link, ffXml, lang);
    end;
    if parser = nil then
      parser := TFHIRParsers2.parser(worker.link, DetectFormat(part.content), lang);
    mem := TFslMemoryStream.Create;
    try
      mem.Buffer := part.content.Link;
      s := TVCLStream.Create;
      try
        s.Stream := mem.Link;
        parser.source := s;
        result := parser.ParseDT(name, type_) as TFhirType;
      finally
        s.Free;
      end;
    finally
      mem.Free;

    end;
  finally
    parser.Free;
  end;
end;

function LoadDTFromParam(worker : TFHIRWorkerContext; value : String; const lang : THTTPLanguages; name : String; type_ : TFHIRTypeClass) : TFhirType;
var
  parser : TFHIRParser;
  mem : TStringStream;
begin
  parser := TFHIRParsers2.parser(worker.link, ffJson, lang);
  try
    // first, figure out the format
    mem := TStringStream.Create(value, TEncoding.UTF8);
    try
      parser.source := mem;
      result := parser.ParseDT(name, type_) as TFHIRType;
    finally
      mem.Free;
    end;
  finally
    parser.Free;
  end;
end;

function LoadFromFormParam(worker : TFHIRWorkerContext; part : TMimePart; const lang : THTTPLanguages) : TFhirResource;
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
        parser := TFHIRParsers2.parser(worker.link, ffJson, lang)
      else if StringStartsWithInsensitive(ct, 'text/xml') or StringStartsWithInsensitive(ct, 'application/xml') or
          StringStartsWithInsensitive(ct, 'application/fhir+xml') or StringStartsWithInsensitive(ct, 'application/xml+fhir') or StringStartsWithInsensitive(ct, 'xml') Then
        parser := TFHIRParsers2.parser(worker.link, ffXml, lang);
    end;
    if parser = nil then
      parser := TFHIRParsers2.parser(worker.link, DetectFormat(part.content), Lang);
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
        s.Free;
      end;
    finally
      mem.Free;
    end;
  finally
    parser.Free;
  end;
end;


(*



  procedure generateComposition(x : TFhirTFhirXHtmlNode; vs : TFHIRValueSet, Map<String, AtomEntry> codeSystems) throws Exception begin
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

    if (inc.Code.size :=:= 0 && inc.Filter.size :=:= 0) begin then
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
    if (e :=:= nil) then
      return nil;
    vs : TFHIRValueSet := (ValueSet) e.Resource;
    if (vs.CodeSystem :=:= nil) then
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
    {$IFDEF FHIR_CONCEPTMAP}
    frtConceptMap: result := TFHIRConceptMap(res).url;
    {$ENDIF}
    frtConformance: result := TFHIRConformance(res).url;
    {$IFDEF FHIR_CONCEPTMAP}
    frtDataElement: result := TFHIRDataElement(res).url;
    {$ENDIF}
    {$IFDEF FHIR_ImplementationGuide}
    frtImplementationGuide: result := TFHIRImplementationGuide(res).url;
    {$ENDIF}
    {$IFDEF FHIR_OperationDefinition}
    frtOperationDefinition: result := TFHIROperationDefinition(res).url;
    {$ENDIF}
    frtSearchParameter: result := TFHIRSearchParameter(res).url;
    frtStructureDefinition: result := TFHIRStructureDefinition(res).url;
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
    b.Free;
  end;
end;

function TFHIROperationOutcomeHelper.error(source : String; typeCode : TFhirIssueTypeEnum; path: string; test: boolean; msg: string): boolean;
var
  issue : TFhirOperationOutcomeIssue;
  ex : TFhirExtension;
begin
  if not test then
  begin
    issue := TFhirOperationOutcomeIssue.create;
    try
      issue.severity := IssueSeverityError;
      issue.code := typeCode;
      issue.details := TFHIRCodeableConcept.create;
      issue.details.text := msg;
      {$IFDEF STACK_DUMPS}
      issue.diagnostics := dumpStack;
      {$ENDIF}
      if (path <> '') then
        issue.locationList.Append.value := path;
      ex := issue.ExtensionList.Append;
      ex.url := 'http://hl7.org/fhir/tools#issue-source';
      ex.value := TFhirCode.create;
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
    issue := TFhirOperationOutcomeIssue.create;
    try
      issue.severity := IssueSeverityInformation;
      issue.code := typeCode;
      issue.details := TFHIRCodeableConcept.create;
      issue.details.text := msg;
      {$IFDEF STACK_DUMPS}
      issue.diagnostics := dumpStack;
      {$ENDIF}
      if (path <> '') then
        issue.locationList.Append.value := path;
      ex := issue.ExtensionList.Append;
      ex.url := 'http://hl7.org/fhir/tools#issue-source';
      ex.value := TFhirCode.create;
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
    issue := TFhirOperationOutcomeIssue.create;
    try
      issue.severity := level;
      issue.code := typeCode;
      issue.details := TFHIRCodeableConcept.create;
      issue.details.text := msg;
      {$IFDEF STACK_DUMPS}
      issue.diagnostics := dumpStack;
      {$ENDIF}
      if (path <> '') then
        issue.locationList.Append.value := path;
      ex := issue.ExtensionList.Append;
      ex.url := 'http://hl7.org/fhir/tools#issue-source';
      ex.value := TFhirCode.create;
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
    issue := TFhirOperationOutcomeIssue.create;
    try
      issue.severity := IssueSeverityWarning;
      issue.code := typeCode;
      issue.details := TFHIRCodeableConcept.create;
      issue.details.text := msg;
      {$IFDEF STACK_DUMPS}
      issue.diagnostics := dumpStack;
      {$ENDIF}
      if (path <> '') then
        issue.locationList.Append.value := path;
      ex := issue.ExtensionList.Append;
      ex.url := 'http://hl7.org/fhir/tools#issue-source';
      ex.value := TFhirCode.create;
      TFhirCode(ex.value).value := source;
      self.issueList.add(issue.link);
    finally
      issue.free;
    end;
  end;
  result := test;
end;

{ TFHIRElementHelper }

procedure TFHIRElementHelper.addExtension(url: String; t: TFhirType);
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

function TFHIRElementHelper.addExtension(url: String): TFHIRExtension;
begin
  result := self.ExtensionList.Append;
  result.url := url;
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

procedure TFHIRElementHelper.setExtension(url: String; t: TFHIRType);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := t.link;
end;

procedure TFHIRElementHelper.setExtensionCode(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirCode.Create(value);
end;

procedure TFHIRElementHelper.setExtensionDate(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirDate.Create(TFslDateTime.fromXML(value));
end;

procedure TFHIRElementHelper.setExtensionDateTime(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirDateTime.Create(TFslDateTime.fromXML(value));
end;

procedure TFHIRElementHelper.setExtensionDecimal(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirDecimal.Create(value);
end;

procedure TFHIRElementHelper.setExtensionInteger(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirInteger.Create(value);
end;

procedure TFHIRElementHelper.setExtensionString(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirString.Create(value);
end;

procedure TFHIRElementHelper.setExtensionTime(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirTime.Create(value);
end;

procedure TFHIRElementHelper.setExtensionURI(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirUri.Create(value);
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

{ TFHIRDomainResourceHelper }

function TFHIRDomainResourceHelper.addExtension(url: String; t: TFhirType) : TFhirExtension;
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
        else
          result := '';
      end;
    end;
  end;
end;

function TFHIRDomainResourceHelper.getExtensionValue(url: String): TFHIRType;
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
  else if (self.ExtensionList.Item(ndx).value is TFhirString) then
    result := TFhirString(self.ExtensionList.Item(ndx).value).value
  else if (self.ExtensionList.Item(ndx).value is TFhirCode) then
    result := TFhirCode(self.ExtensionList.Item(ndx).value).value
  else if (self.ExtensionList.Item(ndx).value is TFhirUri) then
    result := TFhirUri(self.ExtensionList.Item(ndx).value).value
  else if (self.ExtensionList.Item(ndx).value is TFhirDateTime) then
    result := TFhirDateTime(self.ExtensionList.Item(ndx).value).value.ToXML
  else
    result := '';
end;

function TFHIRDomainResourceHelper.hasExtension(url: String): boolean;
begin
  result := getExtension(url) > -1;
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

procedure TFHIRDomainResourceHelper.setExtensionString(url, value: String);
var
  ext : TFhirExtension;
begin
  removeExtension(url);
  ext := self.ExtensionList.Append;
  ext.url := url;
  ext.value := TFhirString.Create(value);
end;

{ TFHIRConformanceHelper }

procedure TFHIRConformanceHelper.checkCompatible;
var
  res, code : String;
begin
  res := fhirVersion;
  if res.Contains('-') then
    res := res.Substring(0, res.IndexOf('-'));
  res := res.Substring(0, res.LastIndexOf('.'));
  code := FHIR_GENERATED_VERSION.Substring(0, FHIR_GENERATED_VERSION.LastIndexOf('.'));
  if (code <> res) then
    raise EFHIRException.create('Version Mismatch - this code is at version '+FHIR_GENERATED_VERSION+', but the server is version '+fhirVersion);
end;

function TFHIRConformanceHelper.rest(type_: TFhirResourceType): TFhirConformanceRestResource;
var
  i : integer;
  j : integer;
begin
  result := nil;
  for I := 0 to self.restlist.count - 1 do
    if self.restlist[i].mode = RestfulConformanceModeServer then
      for j := 0 to self.restlist[i].resourceList.count - 1 do
        if CODES_TFhirResourceTypesEnum[self.restlist[i].resourceList[j].type_] = CODES_TFhirResourceType[type_] then
        begin
          result := self.restlist[i].resourceList[j];
          exit;
        end;
end;

{ TFhirConformanceRestResourceHelper }

function TFhirConformanceRestResourceHelper.interaction(type_: TFhirTypeRestfulInteractionEnum): TFhirConformanceRestResourceInteraction;
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


{ TFhirConceptMapElementHelper }

procedure TFhirConceptMapElementHelper.SetSystem(const Value: String);
begin
  codeSystem := value;
end;

function TFhirConceptMapElementHelper.systemElement: TFhirUri;
begin
  result := codeSystemElement;
end;

function TFhirConceptMapElementHelper.getsystem: String;
begin
  result := codeSystem;
end;

{ TFhirConceptMapHelper }

function TFhirConceptMapHelper.conceptList: TFhirConceptMapElementList;
begin
  result := elementList;
end;

function TFhirConceptMapHelper.context: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to useContextList.Count - 1 do
    result := result + gen(useContextList[i]);
end;

function TFhirConceptMapHelper.groupList: TFhirConceptMap;
begin
  result := self;
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

procedure TFHIRDomainResourceHelper.checkNoModifiers(place, role: String);
begin
  if modifierExtensionList.Count > 0 then
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
    result := result + ','+Item(i).name;
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

{ TFHIRBundleHelper }


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
//  raise EFHIRException.create('todo');
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
//  raise EFHIRException.create('todo');
//end;
//
//function TFHIRCodingListHelper.hasCoding(system, code: String): boolean;
//begin
//  raise EFHIRException.create('todo');
//end;
//
//procedure TFHIRCodingListHelper.CopyCodings(tags: TFHIRCodingList);
//begin
//  raise EFHIRException.create('todo');
//end;
//
//function TFHIRCodingListHelper.json: TBytes;
//begin
//  SetLength(result, 0);
//end;
//
//procedure TFHIRCodingListHelper.WriteTags(meta: TFHIRMeta);
//begin
//  raise EFHIRException.create('todo');
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
  raise EFHIRException.create('todo');
end;

function fullResourceUri(base: String; aType : TFhirResourceType; id : String) : String;
begin
  if (base = 'urn:oid:') then
  begin
    if isOid(id) then
      result := base+id
    else
      raise EFHIRException.create('The resource id "'+'" has a base of "urn:oid:" but is not a valid OID');
  end
  else if (base = 'urn:uuid:') then
  begin
    if isGuid(id) then
      result := base+id
    else
      raise EFHIRException.create('The resource id "'+id+'" has a base of "urn:uuid:" but is not a valid UUID');
  end
  else if not base.StartsWith('http://') and not base.StartsWith('https://')  then
    raise EFHIRException.create('The resource base of "'+base+'" is not understood')
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
  else if url.StartsWith('urn:oid:') or url.StartsWith('urn:uuid:') or url.StartsWith('http://') or url.StartsWith('https://') then
    result := url
  else if not base.StartsWith('http://') and not base.StartsWith('https://')  then
    raise EFHIRException.create('The resource base of "'+base+'" is not understood')
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
    raise EFHIRException.create('The resource base of "'+base+'" is not understood')
  else
    result := AppendForwardSlash(base)+url;
end;

{ TFhirParametersHelper }

procedure TFhirParametersHelper.AddParameter(name: String; value: TFhirType);
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
  else if not (v is TFhirBoolean) then
  begin
    try
      raise EFHIRException.create('Attempt to read "'+name+'" as a boolean, when it is a '+NamedParameter[name].FhirType);
    finally
      v.free;
    end;
  end
  else
    result := (v as TFhirBoolean).value;
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
      raise EFHIRException.create('Attempt to read "'+name+'" as a string, when it is a '+NamedParameter[name].FhirType);
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

procedure TFhirParametersParameterHelper.AddParameter(name: String; value: TFhirType);
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
        result := partList[i].valueElement.Link
      else
        result := partList[i].resourceElement.Link;
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

{ TFHIRCodeableConceptHelper }

constructor TFHIRCodeableConceptHelper.Create(system, code: String);
begin
  Create;
  CodingList.Add(TFHIRCoding.create(system, code));
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
    raise EFHIRException.create('Unable to find code in '+system);
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
    raise EFHIRException.create('Unable to find code in '+StringArrayToString(systems));
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


{ TFhirResourceMetaHelper }

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


function IsSlash(const S: string; Index: Integer): Boolean;
begin
  Result := (Index >= Low(string)) and (Index <= High(S)) and (S[Index] = '/') and (ByteType(S, Index) = mbSingleByte);
end;

function IncludeTrailingSlash(const S: string): string;
begin
  Result := S;
  if not IsSlash(Result, High(Result)) then
    Result := Result + PathDelim;
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

function TFHIRElementHelper.getExtensionValue(url: String): TFHIRType;
var
  index : integer;
begin
  index := getExtension(url);
  if index = -1 then
    result := nil
  else
    result := extensionList[index].value;
end;

{ TFhirValueSetContactListHelper }

procedure TFhirValueSetContactListHelper.setSystem(type_: TFHIRContactPointSystemEnum; value: String);
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

function TFhirValueSetContactListHelper.system(type_: TFHIRContactPointSystemEnum): String;
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

function TFhirValueSetHelper.conceptList: TFhirValueSetCodeSystemConceptList;
begin
  result := codeSystem.conceptList;
end;

function TFhirValueSetHelper.context: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to useContextList.Count - 1 do
    result := result + gen(useContextList[i]);
end;

function csName(url : string) : String;
begin
  if url.StartsWith('http://hl7.org/fhir/v2') then
    result := 'V2 '
  else if url.StartsWith('http://hl7.org/fhir/v3') then
    result := 'V3 '
  else if url.StartsWith('http://hl7.org/fhir') then
    result := 'FHIR '
  else if url = 'http://snomed.info/sct' then
    result := 'SCT '
  else if url = 'http://loinc.org' then
    result := 'LOINC '
  else
    result := 'Other';
end;

function TFhirValueSetHelper.source: string;
var
  b : TFslStringBuilder;
  comp : TFhirValueSetComposeInclude;
begin
  b := TFslStringBuilder.Create;
  try
    if (compose <> nil) then
      for comp in compose.includeList do
        if comp.system <> '' then
          b.Append(csName(comp.system));
    result := b.AsString;
  finally
    b.Free;
  end;
end;


function gen(t : TFhirType):String;
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
  else if t is TFhirBoolean then
    if TFhirBoolean(t).value then
      result := 'true'
    else
      result := 'false'
  else if t.isPrimitive then
    result := t.primitiveValue
  else
    raise EFHIRException.create('Type '+t.className+' not handled yet');
end;


{ TFhirElementDefinitionTypeHelper }

function TFhirElementDefinitionTypeHelper.GetProfile: String;
begin
  if profileList.Count = 1 then
    result := profileList[0].value
  else
    result := '';
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
  result := TFHIRElementDefinitionList.create();
  try
    // if we have a name reference, we have to find it, and iterate it's children
    if (nameReference <> '') then
    begin
      found := false;
      for e in profile.Snapshot.ElementList do
      begin
        if (nameReference = e.Name) then
        begin
          found := true;
          path := e.Path;
        end;
      end;
      if (not found) then
        raise EFHIRException.create('Unable to resolve name reference '+nameReference+' at path '+path);
    end;

    inScope := false;
    for e in profile.Snapshot.ElementList do
    begin
      p := e.Path;

      if (path <> '') and (e.NameReference <> '') and (path.startsWith(p)) then
      begin
        {* The path we are navigating to is on or below this element, but the element defers its definition to another named part of the
         * structure.
         *}
        if (path.length > p.length) then
        begin
          // The path navigates further into the referenced element, so go ahead along the path over there
          result.free;
          result := getChildMap(profile, name, e.NameReference+'.'+path.substring(p.length+1), '');
          exit;
        end
        else
        begin
          // The path we are looking for is actually this element, but since it defers it definition, go get the referenced element
          result.free;
          result := getChildMap(profile, name, e.NameReference, '');
          exit;
        end;
      end
      else
      begin
        if (p = path) then
        begin
          if (name = '') or (e.name = name) then
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
  raise EFHIRTodo.create('compareValues');
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

constructor TFhirOperationOutcomeIssueHelper.create(Severity: TFhirIssueSeverityEnum; Code: TFhirIssueTypeEnum; Diagnostics, location: String);
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
    raise EFHIRException.create('Unknown resource type '+name);
  result := CLASSES_TFhirResourceType[TFhirResourceType(i)].Create;
end;

function CreateTypeByName(name : String) : TFhirElement;
begin
  if name = 'boolean' then
    result := TFHIRboolean.create(false)
  else if name = 'integer' then
    result := TFHIRinteger.create('1')
  else if name = 'decimal' then
    result := TFHIRdecimal.create('1.0')
  else if name = 'base64Binary' then
    result := TFHIRbase64Binary.create(AnsiStringAsBytes('%test content%'))
  else if name = 'instant' then
    result := TFHIRinstant.create(TFslDateTime.makeLocal)
  else if name = 'string' then
    result := TFHIRstring.create('%string%')
  else if name = 'uri' then
    result := TFHIRuri.create('http://uri...')
  else if name = 'date' then
    result := TFHIRdate.create(TFslDateTime.makeToday)
  else if name = 'dateTime' then
    result := TFHIRdateTime.create(TFslDateTime.makeLocal)
  else if name = 'time' then
    result := TFHIRtime.create('00:10:00')
  else if name = 'code' then
    result := TFHIRcode.create('%code%')
  else if name = 'oid' then
    result := TFHIRoid.create('urn:oid:0.1.2.3')
  else if name = 'id' then
    result := TFHIRid.create('%id%')
  else if name = 'unsignedInt' then
    result := TFHIRunsignedInt.create('0')
  else if name = 'positiveInt' then
    result := TFHIRpositiveInt.create('1')
  else if name = 'markdown' then
    result := TFHIRmarkdown.create('*markdown*')
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
  else if name = 'Meta' then
    result := TFHIRMeta.create
  else if name = 'xhtml' then
    result := nil
  else
    raise EFHIRException.create('Unknown type: '+name);
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
      TFHIRQuantity(element).system := 'http://unitsofmeasure.org';
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
    TFHIRSignature(element).contentType := 'application/signature+xml';
    TFHIRSignature(element).blob := AnsiStringAsBytes('signature content');
  end
  else if element.FhirType = 'HumanName' then
  begin
    TFHIRHumanName(element).use := NameUseUsual;
    TFHIRHumanName(element).text := 'prefix given family';
    TFHIRHumanName(element).givenList.add('%given%');
    TFHIRHumanName(element).familyList.add('%family%');
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
    TFHIRTiming(element).repeat_ := TFhirTimingRepeat.create;
    TFHIRTiming(element).repeat_.duration := '1';
    TFHIRTiming(element).repeat_.durationUnits := UnitsOfTimeH;
    TFHIRTiming(element).repeat_.frequency := '3';
    TFHIRTiming(element).repeat_.period := '21';
    TFHIRTiming(element).repeat_.periodUnits := UnitsOfTimeD;
  end
  else if element.FhirType = 'Reference' then
  begin
    TFHIRReference(element).display := '%test description%';
    TFHIRReference(element).reference := '%Type/id%';
  end
  else if element.FhirType = 'Narrative' then
  begin
    TFhirNarrative(element).status := NarrativeStatusAdditional;
    TFhirNarrative(element).div_ := TFHIRXhtmlParser.Parse(THTTPLanguages.create('en'), xppAllow, [], '<div xmlns="http://www.w3.org/1999/xhtml"><p>%Some xhtml content%</p></div>');
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
    result := TFHIRCode.create(TFHIREnum(obj).value);
    obj.Free;
  end
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRCode.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRCode\"')
  end;
end;

function asMarkdown(obj : TFHIRObject) : TFHIRMarkdown;
begin
  if obj is TFHIRMarkdown then
    result := obj as TFHIRMarkdown
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRMarkdown.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRMarkdown.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRMarkdown\"')
  end;
end;

function asXhtml(obj : TFHIRObject) : TFhirXHtmlNode;
begin
  if obj is TFhirXHtmlNode then
    result := obj as TFhirXHtmlNode
  else if obj.isPrimitive then
  begin
    result := TFHIRXhtmlParser.parse(THTTPLanguages.create('en'), xppDrop, [], obj.primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRMarkdown\"')
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
    result := TFHIRString.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRString.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRString\"')
  end;
end;

function asId(obj : TFHIRObject) : TFHIRId;
begin
  if obj is TFHIRId then
    result := obj as TFHIRId
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRId.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRId\"')
  end;
end;

function asUri(obj : TFHIRObject) : TFHIRUri;
begin
  if obj is TFHIRUri then
    result := obj as TFHIRUri
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUri.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRUri.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRUri\"')
  end;
end;

function asDateTime(obj : TFHIRObject) : TFHIRDateTime;
begin
  if obj is TFHIRDateTime then
    result := obj as TFHIRDateTime
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDateTime.create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRDateTime\"')
  end;
end;

function asUnsignedInt(obj : TFHIRObject) : TFHIRUnsignedInt;
begin
  if obj is TFHIRUnsignedInt then
    result := obj as TFHIRUnsignedInt
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUnsignedInt.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRUnsignedInt\"')
  end;
end;

function asPositiveInt(obj : TFHIRObject) : TFHIRPositiveInt;
begin
  if obj is TFHIRPositiveInt then
    result := obj as TFHIRPositiveInt
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRPositiveInt.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRPositiveInt\"')
  end;
end;

function asInstant(obj : TFHIRObject) : TFHIRInstant;
begin
  if obj is TFHIRInstant then
    result := obj as TFHIRInstant
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRInstant.create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRInstant\"')
  end;
end;

function asBoolean(obj : TFHIRObject) : TFHIRBoolean;
begin
  if obj is TFHIRBoolean then
    result := obj as TFHIRBoolean
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRBoolean.create(TFHIRMMElement(obj).value = 'true');
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRBoolean\"')
  end;
end;

function asBase64Binary(obj : TFHIRObject) : TFHIRBase64Binary;
begin
  if obj is TFHIRBase64Binary then
    result := obj as TFHIRBase64Binary
//  else if obj is TFHIRMMElement then
//  begin
//    result := TFHIRBase64Binary.create(TFHIRMMElement(obj).value);
//    obj.Free;
//  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRBase64Binary\"')
  end;
end;

function asDate(obj : TFHIRObject) : TFHIRDate;
begin
  if obj is TFHIRDate then
    result := obj as TFHIRDate
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDate.create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRDate\"')
  end;
end;

function asDecimal(obj : TFHIRObject) : TFHIRDecimal;
begin
  if obj is TFHIRDecimal then
    result := obj as TFHIRDecimal
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDecimal.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRDecimal\"')
  end;
end;

function asTime(obj : TFHIRObject) : TFHIRTime;
begin
  if obj is TFHIRTime then
    result := obj as TFHIRTime
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRTime.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRTime\"')
  end;
end;

function asOid(obj : TFHIRObject) : TFHIROid;
begin
  if obj is TFHIROid then
    result := obj as TFHIROid
  else if obj is TFHIRMMElement then
  begin
    result := TFHIROid.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIROid\"')
  end;
end;

function asInteger(obj : TFHIRObject) : TFHIRInteger;
begin
  if obj is TFHIRInteger then
    result := obj as TFHIRInteger
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRInteger.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRInteger\"')
  end;
end;

function asResource(obj : TFHIRObject) : TFHIRResource;
begin
  if obj is TFHIRResource then
    result := obj as TFHIRResource
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRResource\"')
  end;
end;


function asExtension(obj : TFHIRObject) : TFHIRExtension;
begin
  if obj is TFHIRExtension then
    result := obj as TFHIRExtension
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRResource\"')
  end;
end;


function asEnum(systems, values: array of String; obj : TFHIRObject) : TFHIREnum;
begin
  if obj is TFHIREnum then
    result := obj as TFHIREnum
  else if obj is TFHIRCode then
  begin
    result := TFHIREnum.create(systems[StringArrayIndexOf(values, TFHIRCode(obj).value)], TFHIRCode(obj).value);
    obj.Free;
  end
  else if obj is TFHIRString then
  begin
    result := TFHIREnum.create(systems[StringArrayIndexOf(values, TFHIRString(obj).value)], TFHIRString(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFHIRException.create('Type mismatch: cannot convert from \"'+obj.className+'\" to \"TFHIRCode\"')
  end;
end;

function ComposeJson(worker: TFHIRWorkerContext; r : TFhirResource) : String;
var
  comp : TFHIRComposer;
begin
  comp := TFHIRParsers2.composer(worker.link, ffJson, THTTPLanguages.create('en'), OutputStyleNormal);
  try
    result := comp.Compose(r);
  finally
    comp.Free;
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

function TFhirElementDefinitionHelper.ContentReference: String;
begin
  result := nameReference;
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

function TFhirElementDefinitionHelper.hasType(t: String; out profile: String): boolean;
var
  edt : TFhirElementDefinitionType;
begin
  result := false;
  for edt in type_List do
    if SameText(edt.code, t) then
    begin
      if edt.profileList.Count > 0 then
        profile := edt.profileList[0].value
      else
        profile := '';
      exit(true);
    end;
end;

{ TFHIRBackboneElementHelper }

procedure TFHIRBackboneElementHelper.checkNoModifiers(place, role: String; exempt : TArray<String>);
var
  ext : TFHIRExtension;
begin
  if length(exempt) > 0 then
  begin
    for ext in modifierExtensionList do
      if not StringArrayExistsInsensitive(exempt, ext.url) then
        raise EUnsafeOperation.Create('The element '+role+' has modifier exceptions that are unknown at '+place);
  end
  else if modifierExtensionList.Count > 0 then
    raise EUnsafeOperation.Create('The element '+role+' has modifier exceptions that are unknown at '+place);
end;

procedure TFHIRBackboneElementHelper.checkNoModifiers(place, role: String);
begin
  if modifierExtensionList.Count > 0 then
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


{ TFhirValueSetCodeSystemHelper }

function TFhirValueSetCodeSystemHelper.locate(parent : TFhirValueSetCodeSystemConcept; list : TFhirValueSetCodeSystemConceptList; code : String; var foundParent, foundConcept : TFhirValueSetCodeSystemConcept) : boolean;
var
  c : TFhirValueSetCodeSystemConcept;
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


procedure TFhirValueSetCodeSystemHelper.scanForSubsumes(parentList, conceptList: TFhirValueSetCodeSystemConceptList; code: String);
var
  ext : TFHIRExtension;
  c : TFhirValueSetCodeSystemConcept;
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



function TFhirValueSetCodeSystemHelper.getChildren(concept: TFhirValueSetCodeSystemConcept): TFhirValueSetCodeSystemConceptList;
var
  ext : TFHIRExtension;
  p, c : TFhirValueSetCodeSystemConcept;
begin
  result := TFhirValueSetCodeSystemConceptList.Create;
  try
    result.AddAll(concept.conceptList);
    for ext in concept.modifierExtensionList do
      if (ext.url= 'http://hl7.org/fhir/StructureDefinition/valueset-subsumes') then
        if locate(nil, conceptList, (ext.value as TFHIRCode).value, p, c) then
          result.Add(c.link);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFhirValueSetCodeSystemHelper.getParents(concept: TFhirValueSetCodeSystemConcept): TFhirValueSetCodeSystemConceptList;
var
  p, c : TFhirValueSetCodeSystemConcept;
begin
  result := TFhirValueSetCodeSystemConceptList.Create;
  try
    if locate(nil, conceptList, concept.code, p, c) then
      if (p <> nil) then
        result.Add(p.Link);
    scanForSubsumes(result, conceptList, concept.code);
    result.Link;
  finally
    result.Free;
  end;

end;

{$IFDEF DSTU21}

{ TFhirExpansionProfileHelper }

function TFhirExpansionProfileHelper.hash: String;
begin
 result := BooleanToString(includeDefinition)+'|'+BooleanToString(limitedExpansion)+BooleanToString(includeDesignations)+BooleanToString(includeInactive)+
   BooleanToString(excludeNested)+BooleanToString(excludeNotForUI)+BooleanToString(excludePostCoordinated)+displayLanguage;
end;

{$ELSE}
{ TFhirExpansionProfile }

function TFhirExpansionProfile.Clone: TFhirExpansionProfile;
begin
  result := TFhirExpansionProfile(inherited Clone);
end;

function TFhirExpansionProfile.codeSystem: TFhirExpansionProfile;
begin
  result := self;
end;

constructor TFhirExpansionProfile.Create;
begin
  inherited;
  FcodeSystemList := TFslList<TFhirExpansionProfileFixedVersion>.create;
end;

function TFhirExpansionProfile.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FdisplayLanguage.length * sizeof(char)) + 12);
  inc(result, FcodeSystemList.sizeInBytes);
end;

class function TFhirExpansionProfile.defaultProfile: TFhirExpansionProfile;
begin
  result := TFhirExpansionProfile.create;
end;

destructor TFhirExpansionProfile.Destroy;
begin
  FcodeSystemList.Free;
  inherited;
end;

function TFhirExpansionProfile.hash: string;
begin
  result := BooleanToString(FincludeDefinition)+'|'+BooleanToString(FlimitedExpansion);
end;

function TFhirExpansionProfile.include: TFhirExpansionProfile;
begin
  result := self;
end;

function TFhirExpansionProfile.Link: TFhirExpansionProfile;
begin
  result := TFhirExpansionProfile(inherited Link);
end;

{$ENDIF}

{$IFDEF DSTU21}
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
  value.Free;
end;

{ TFHIRTFhirContractHelper }

function TFHIRTFhirContractHelper.getActorList: TFhirContractPartyList;
begin
  result := partyList;
end;


{$ENDIF}

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
    result.Free;
  end;
end;

function TFhirCodingHelper.GetEditString: String;
begin
  if system = 'http://snomed.info/sct' then
    result := 'sct:'+code
  else if system = 'http://loinc.org' then
    result := 'loinc:'+code
  else if system = 'http://loinc.org' then
    result := 'loinc:'+code

  else if system = 'http://snomed.info/sct' then
    result := 'sct:'+code
  else if system = 'http://www.nlm.nih.gov/research/umls/rxnorm' then
    result := 'rxnorm:'+code
  else if system = 'http://loinc.org' then
    result := 'loinc:'+code
  else if system = 'http://unitsofmeasure.org' then
    result := 'ucum:'+code
  else if system = 'http://ncimeta.nci.nih.gov' then
    result := 'nci:'+code
  else if system = 'http://www.ama-assn.org/go/cpt' then
    result := 'cpt:'+code
  else if system = 'http://hl7.org/fhir/ndfrt' then
    result := 'ndfrt:'+code
  else if system = 'http://fdasis.nlm.nih.gov' then
    result := 'unii:'+code
  else if system = 'http://hl7.org/fhir/sid/ndc' then
    result := 'ndc:'+code
  else if system = 'http://hl7.org/fhir/sid/cvx' then
    result := 'cvx:'+code
  else if system = 'urn:iso:std:iso:3166' then
    result := 'iso3166:'+code
  else if system = 'http://www.radlex.org' then
    result := 'radlex:'+code
  else if system = 'http://hl7.org/fhir/sid/icf-nl' then
    result := 'icf:'+code
  else if system = 'http://www.whocc.no/atc' then
    result := 'atcc:'+code
  else if system = 'urn:ietf:bcp:47' then
    result := 'lang:'+code
  else if system = 'urn:iso:std:iso:11073:10101' then
    result := 'mdc:'+code
  else if system = 'http://dicom.nema.org/resources/ontology/DCM' then
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
  if not match('sct', 'http://snomed.info/sct') then
  if not match('rxnorm', 'http://www.nlm.nih.gov/research/umls/rxnorm') then
  if not match('loinc', 'http://loinc.org') then
  if not match('ucum', 'http://unitsofmeasure.org') then
  if not match('nci', 'http://ncimeta.nci.nih.gov') then
  if not match('cpt', 'http://www.ama-assn.org/go/cpt') then
  if not match('ndfrt', 'http://hl7.org/fhir/ndfrt') then
  if not match('unii', 'http://fdasis.nlm.nih.gov') then
  if not match('ndc', 'http://hl7.org/fhir/sid/ndc') then
  if not match('cvx', 'http://hl7.org/fhir/sid/cvx') then
  if not match('iso3166', 'urn:iso:std:iso:3166') then
  if not match('radlex', 'http://www.radlex.org') then
  if not match('icf', 'http://hl7.org/fhir/sid/icf-nl') then
  if not match('atcc', 'http://www.whocc.no/atc') then
  if not match('lang', 'urn:ietf:bcp:47') then
  if not match('mdc', 'urn:iso:std:iso:11073:10101') then
  if not match('dicom', 'http://dicom.nema.org/resources/ontology/DCM') then
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

{ TFhirConceptMapElementTargetHelper }
function TFhirConceptMapElementTargetHelper.getsystem: String;
begin
  result := codeSystem;
end;

procedure TFhirConceptMapElementTargetHelper.SetSystem(const Value: String);
begin
  codeSystem := value;
end;

function TFhirConceptMapElementTargetHelper.systemElement: TFhirUri;
begin
  result := codeSystemElement;
end;

{ TFhirStructureDefinitionHelper }

function TFhirStructureDefinitionHelper.baseDefinition: String;
begin
  result := base;
end;

function TFhirStructureDefinitionHelper.baseType: String ;
begin
  result := constrainedType;
end;

function TFhirStructureDefinitionHelper.type_: String;
begin
  result := constrainedType;
end;

{ TFHIRNamingSystemHelper }
  {$IFDEF FHIR_NAMINGSYSTEM}

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
{$ENDIF}

{ TFhirValueSetExpansionHelper }

procedure TFhirValueSetExpansionHelper.AddParam(name, value: String);
var
  p : TFhirValueSetExpansionParameter;
begin
  p := parameterList.Append;
  p.name := name;
  p.value := TFhirString.Create(value);
end;

procedure TFhirValueSetExpansionHelper.AddParam(name: String; value: boolean);
var
  p : TFhirValueSetExpansionParameter;
begin
  p := parameterList.Append;
  p.name := name;
  p.value := TFhirBoolean.Create(value);
end;


function hasProp(props : TList<String>; name : String; def : boolean) : boolean;
begin
  if (props = nil) or (props.Count = 0) then
    result := def
  else
    result := props.Contains(name);
end;


{ TFhirQuantityHelper }

function TFhirQuantityHelper.asDuration: TDateTime;
var
  v : Double;
begin
  if system <> 'http://unitsofmeasure.org' then
    raise EFHIRException.create('Unknown units system "'+system+'" trying to process quantity as a duration');
  if not IsNumericString(value) then
    raise EFHIRException.create('invalid value "'+value+'" trying to process quantity as a duration');
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
    raise EFHIRException.create('invalid UCUM unit "'+code+'" trying to process quantity as a duration');
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
    result.Free;
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
    result.Free;
  end;
end;

function TFhirQuantityHelper.GetEditString: String;
begin
  result := CODES_TFhirQuantityComparatorEnum[comparator]+value+' '+unit_;
  if code <> '' then
  begin
    if system = 'http://snomed.info/sct' then
      result := result+' [sct:'+code+']'
    else if system <> 'http://unitsofmeasure.org' then
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
      raise EFHIRException.create('Unable to parse quantity '+vs);
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
      system := 'http://snomed.info/sct';
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
      system := 'http://unitsofmeasure.org';
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

{ TFHIROperationDefinitionHelper }
  {$IFDEF FHIR_OPERATIONDEFINITION}

function TFHIROperationDefinitionHelper.GetType2_: boolean;
begin
  result := false;
end;

function TFHIROperationDefinitionHelper.resourceList: TFhirEnumList;
begin
  result := type_list;
end;

procedure TFHIROperationDefinitionHelper.SetType2_(const Value: boolean);
begin
  raise EFHIRTodo.create('TFHIROperationDefinitionHelper.Settype_');
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

{ TFhirConceptMapGroupElementTargetHelper }

function TFhirConceptMapGroupElementTargetHelper.GetCommentElement: TFHIRString;
begin
  result := commentsElement;
end;

procedure TFhirConceptMapGroupElementTargetHelper.SetCommentElement(const Value: TFHIRString);
begin
  CommentsElement := value;
end;

function fileToResource(name : String; var format : TFHIRFormat) : TFhirResource;
var
  f : TFileStream;
begin
  f := TFileStream.Create(name, fmOpenRead + fmShareDenyWrite);
  try
    result := streamToResource(f, format);
  finally
    f.Free;
  end;
end;

function fileToResource(name : String) : TFhirResource;
var
  format : TFHIRFormat;
begin
  format := ffUnspecified;
  result := fileToResource(name, format);
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
    b.Free;
  end;
end;

function streamToResource(stream : TStream; var format : TFHIRFormat) : TFhirResource;
var
  p :  TFHIRParser;
begin
  if format = ffUnspecified then
    format := DetectFormat(stream);
  p := TFHIRParsers2.parser(nil, format, THTTPLanguages.create('en'));
  try
    p.source := stream;
    p.Parse;
    result := p.resource.Link as TFHIRResource;
  finally
    p.Free;
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
    f.Free;
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
    f.Free;
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
    f.Free;
  end;
end;

procedure resourceToStream(res : TFhirResource; stream : TStream; format : TFHIRFormat; style : TFHIROutputStyle = OutputStyleNormal);
var
  c : TFHIRComposer;
begin
  if format = ffXhtml then
    format := ffXml;
  c := TFHIRParsers2.composer(nil, format, THTTPLanguages.create('en'), style);
  try
    c.Compose(stream, res);
  finally
    c.Free;
  end;
end;


function parseParamsFromForm(stream : TStream) : TFHIRParameters;
var
  pm : THTTPParameters;
  i, j : integer;
  n, v : String;
begin
  result := TFhirParameters.Create;
  try
    pm := THTTPParameters.create(StreamToString(stream, TEncoding.ASCII));
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
    result.Free;
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

class function TFhirPeriodHelper.fromEdit(s: String): TFhirPeriod;
begin
  result := TFhirPeriod.Create;
  try
    result.editString := s;
    result.Link;
  finally
    result.Free;
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
      pl.Free;
    end;
  end;
end;
{$ENDIF}


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


function TFhirExpansionProfileFixedVersion.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FSystem.length * sizeof(char)) + 12);
end;

end.
