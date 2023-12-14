unit fhir_factory;

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
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_collections, fsl_json, fsl_xml, fsl_stream, fsl_http, fsl_npm_cache,
  fsl_ucum, fsl_npm, fsl_threads, fsl_web_stream,
  fhir_objects, fhir_parser, fhir_narrative, fhir_pathengine, fhir_common, fhir_xhtml,
  fhir_elementmodel, fhir_client, fhir_uris;

type
  TFhirReferenceValidationPolicy = (rvpIGNORE, rvpCHECK_VALID);

const
  TFhirReferenceValidationPolicyCheckExists = [rvpCHECK_VALID];
  TFhirReferenceValidationPolicyCheckType = [];
  TFhirReferenceValidationPolicyCheckValid = [];

type
  TFHIRWorkerContextWithFactory = class;

  TBestPracticeWarningLevel = (bpwlIgnore, bpwlHint, bpwlWarning, bpwlError);
  TCheckDisplayOption = (cdoIgnore, cdopCheck, cdoCheckCaseAndSpace, cdoCheckCase, cdoCheckSpace);
  TResourceIdStatus = (risOptional, risRequired, risProhibited);

  TFHIRValidatorContext = class (TFslObject)
  private
    FCheckDisplay: TCheckDisplayOption;
    FBPWarnings: TBestPracticeWarningLevel;
    FSuppressLoincSnomedMessages: boolean;
    FResourceIdRule: TResourceIdStatus;
    FIsAnyExtensionsAllowed: boolean;
    FIssues : TFslList<TFhirOperationOutcomeIssueW>;
    Fowned : TFslList<TFslObject>;
    FOperationDescription : String;
    procedure SetIssues(const Value: TFslList<TFhirOperationOutcomeIssueW>);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property CheckDisplay : TCheckDisplayOption read FCheckDisplay write FCheckDisplay;
    property BPWarnings: TBestPracticeWarningLevel read FBPWarnings write FBPWarnings;
    property SuppressLoincSnomedMessages: boolean read FSuppressLoincSnomedMessages write FSuppressLoincSnomedMessages;
    property ResourceIdRule: TResourceIdStatus read FResourceIdRule write FResourceIdRule;
    property IsAnyExtensionsAllowed: boolean read FIsAnyExtensionsAllowed write FIsAnyExtensionsAllowed;
    property OperationDescription : String read FOperationDescription write FOperationDescription;

    property owned : TFslList<TFslObject> read FOwned;
    property Issues : TFslList<TFhirOperationOutcomeIssueW> read FIssues write SetIssues;
  end;

  TValidatorProgressEvent = procedure (sender : TObject; message : String) of object;

  { TFHIRValidatorV }

  TFHIRValidatorV = class abstract(TFslObject)
  private
    FOnProgress : TValidatorProgressEvent;
    FContext : TFHIRWorkerContextWithFactory;
  protected
    procedure doProgress(path : String);
  public
    constructor Create(context: TFHIRWorkerContextWithFactory); virtual;
    destructor Destroy; override;
    procedure Unload; virtual;

    property Context : TFHIRWorkerContextWithFactory read FContext;

    property OnProgress : TValidatorProgressEvent read FOnProgress write FOnProgress;

    procedure validate(ctxt : TFHIRValidatorContext; obj: TJsonObject); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; obj: TJsonObject; profile: String); overload; virtual; abstract;

    procedure validate(ctxt : TFHIRValidatorContext; element: TMXmlElement); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; element: TMXmlElement; profile: String); overload; virtual; abstract;

    procedure validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument; profile: String); overload; virtual; abstract;

    procedure validate(ctxt : TFHIRValidatorContext; source : TFslBuffer; format : TFHIRFormat); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; source : TFslBuffer; format : TFHIRFormat; profile : String); overload; virtual; abstract;

    procedure validate(ctxt : TFHIRValidatorContext; resource : TFhirResourceV); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; resource : TFhirResourceV; profile : string); overload; virtual; abstract;

    function  describe(ctxt : TFHIRValidatorContext): TFHIROperationOutcomeW; virtual; abstract;
  end;

  TFHIRValidatorClass = class of TFHIRValidatorV;

  { TFHIRFactory }

  TFHIRFactory = class abstract (TFslObject)
  public
    function link : TFHIRFactory; overload;
    function version : TFHIRVersion; virtual;
    function versionString : String; virtual;
    function versionName : String; virtual; abstract;
    function corePackage : String; virtual; abstract;
    function specUrl : String; virtual; abstract;
    function description : String; virtual;
    function resourceNames : TArray<String>; virtual; abstract;
    function canonicalResources : TArray<String>; virtual; abstract;
    function isResourceName(name : String) : boolean; virtual;
    function resCategory(name: String) : TTokenCategory; virtual; abstract;
    function URLs : TCommonURLs; virtual; abstract;

    function makeParser(worker : TFHIRWorkerContextV; format : TFHIRFormat; langList : THTTPLanguageList) : TFHIRParser; virtual; abstract;
    function makeComposer(worker : TFHIRWorkerContextV; format : TFHIRFormat; langList : THTTPLanguageList; style: TFHIROutputStyle) : TFHIRComposer; virtual; abstract;
    function makeValidator(worker : TFHIRWorkerContextV) : TFHIRValidatorV; virtual; abstract;
    function makeGenerator(worker : TFHIRWorkerContextV) : TFHIRNarrativeGeneratorBase; virtual; abstract;
    function makePathEngine(worker : TFHIRWorkerContextV; ucum : TUcumServiceInterface) : TFHIRPathEngineV; virtual; abstract;
    function makeElementModelManager : TFHIRBaseMMManager; virtual; abstract;
    function createFromProfile(worker : TFHIRWorkerContextV; profile : TFhirStructureDefinitionW) : TFHIRResourceV; virtual; abstract;
    function createPropertyList(name : String; bPrimitiveValues : Boolean) : TFHIRPropertyList; Virtual;

    function makeClient(worker : TFHIRWorkerContextV; url : String; fmt : TFHIRFormat) : TFhirClientV; overload;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat) : TFhirClientV; overload;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal) : TFhirClientV; overload;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClientV; overload; virtual;  abstract;// because using indy is necessary if you're writing a server, or unixready code
    function makeClientThreaded(worker : TFHIRWorkerContextV; internal : TFhirClientV; event : TThreadManagementEvent) : TFhirClientV; overload; virtual; abstract;
    function makeClientInt(worker : TFHIRWorkerContextV; langList : THTTPLanguageList; comm : TFHIRClientCommunicator) : TFhirClientV; overload; virtual; abstract;

    function makeHealthcareCard : THealthcareCard; virtual; abstract;

    function getXhtml(res : TFHIRResourceV) : TFHIRXhtmlNode; virtual; abstract;
    procedure setXhtml(res : TFHIRResourceV; x : TFHIRXhtmlNode); virtual; abstract;
    function resetXhtml(r : TFHIRResourceV) : TFHIRXhtmlNode; virtual; abstract;
    procedure clearXhtml(res : TFHIRResourceV); virtual; abstract;
    function getContained(r : TFHIRResourceV) : TFslList<TFHIRResourceV>; virtual; abstract;
    function describe(r : TFHIRResourceV) : String; virtual; abstract;
    procedure markWithTag(r : TFHIRResourceV; systemUri, code, display : String); virtual; abstract;

    procedure checkNoModifiers(res : TFHIRObject; method, param : string; allowed : TArray<String> = nil); virtual; abstract;
    function buildOperationOutcome(langList : THTTPLanguageList; e : exception; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; virtual; abstract;
    Function buildOperationOutcome(langList : THTTPLanguageList; message : String; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; virtual; abstract;

    function makeByName(const name : String) : TFHIRObject; virtual; abstract;
    function makeResource(const name : String) : TFHIRResourceV;
    function makeBoolean(b : boolean): TFHIRObject; virtual; abstract;
    function makeCode(s : string) : TFHIRObject; virtual; abstract;
    function makeCoding(systemUri, code : String) : TFHIRObject; overload;
    function makeCoding(systemUri, code, display : String) : TFHIRObject; overload;
    function makeCoding(systemUri, version, code, display : String) : TFHIRObject; overload; virtual; abstract;
    function makeCodeableConcept(coding : TFHIRCodingW = nil) : TFHIRObject; virtual; abstract;
    function makeString(s : string) : TFHIRObject; virtual; abstract;
    function makeUri(s : string) : TFHIRObject; virtual; abstract;
    function makeInteger(s : string) : TFHIRObject; virtual; abstract;
    function makeDecimal(s : string) : TFHIRObject; virtual; abstract;
    function makeBase64Binary(s : string) : TFHIRObject; virtual;  abstract;// must DecodeBase64
    function makeBinary(content : TBytes; contentType : String) : TFHIRResourceV; virtual; abstract;
    function makeParamsFromForm(s : TStream) : TFHIRResourceV; virtual; abstract;
    function makeDateTime(dt : TFslDateTime) : TFHIRObject; virtual; abstract;
    function makeDtFromForm(part : TMimePart; langList : THTTPLanguageList; name : String; type_ : string) : TFHIRXVersionElementWrapper; virtual; abstract;
    function makeDuration(dt : TDateTime) : TFHIRObject; virtual; abstract;
    function makeBundle(list : TFslList<TFHIRResourceV>) : TFHIRBundleW; virtual; abstract;
    function wrapPrimitive(p : TFHIRObject) : TFHIRPrimitiveW; virtual; abstract;

    function makeParameters : TFHIRParametersW; virtual; abstract;
    function makeTerminologyCapablities : TFhirTerminologyCapabilitiesW; virtual; abstract;
    function makeIssue(level : TIssueSeverity; issue: TFhirIssueType; location, message: String) : TFhirOperationOutcomeIssueW; virtual; abstract;

    function makeProxy(pi : TNpmPackageResource; worker : TFHIRWorkerContextV; lock : TFslLock) : TFHIRResourceProxyV; overload; virtual; abstract;
    function makeProxy(presource : TFHIRResourceV) : TFHIRResourceProxyV; overload; virtual; abstract;

    function wrapResource(r : TFHIRResourceV) : TFHIRXVersionResourceWrapper; virtual;
    function wrapCapabilityStatement(r : TFHIRResourceV) : TFHIRCapabilityStatementW; virtual; abstract;
    function wrapStructureDefinition(r : TFHIRResourceV) : TFhirStructureDefinitionW; virtual; abstract;
    function wrapValueSet(r : TFHIRResourceV) : TFhirValueSetW; virtual; abstract;
    function wrapCodeSystem(r : TFHIRResourceV) : TFhirCodeSystemW; virtual; abstract;
    function wrapConceptMap(r : TFHIRResourceV) : TFhirConceptMapW; virtual; abstract;
    function wrapExtension(o : TFHIRObject) : TFhirExtensionW; virtual; abstract;
    function wrapCoding(o : TFHIRObject) : TFhirCodingW; virtual; abstract;
    function wrapCodeableConcept(o : TFHIRObject) : TFhirCodeableConceptW; virtual; abstract;
    function wrapIdentifier(o : TFHIRObject) : TFhirIdentifierW; virtual; abstract;
    function wrapOperationOutcome(r : TFHIRResourceV) : TFhirOperationOutcomeW; virtual; abstract;
    function wrapBundle(r : TFHIRResourceV) : TFhirBundleW; virtual; abstract;
    function wrapParams(r : TFHIRResourceV) : TFHIRParametersW; virtual; abstract;
    function wrapMeta(r : TFHIRResourceV) : TFhirMetaW; overload; virtual; abstract;  // speecial: does not take ownership
    function wrapMeta(r : TFHIRObject) : TFhirMetaW; overload; virtual; abstract;
    function wrapBinary(r : TFHIRResourceV) : TFhirBinaryW; virtual; abstract;
    function wrapAuditEvent(r : TFHIRResourceV) : TFhirAuditEventW; virtual; abstract;
    function wrapSubscription(r : TFHIRResourceV) : TFhirSubscriptionW; virtual; abstract;
    function wrapSubscriptionTopic(r : TFHIRResourceV) : TFhirSubscriptionTopicW; virtual; abstract;
    function wrapObservation(r : TFHIRResourceV) : TFhirObservationW; virtual; abstract;
    function wrapAttachment(r : TFHIRObject) : TFHIRAttachmentW; virtual; abstract;
    function wrapQuantity(r : TFHIRObject) : TFhirQuantityW; virtual; abstract;
    function wrapPeriod(r : TFHIRObject) : TFhirPeriodW; virtual; abstract;
    function wrapGroup(r : TFHIRResourceV) : TFhirGroupW; virtual; abstract;
    function wrapPatient(r : TFHIRResourceV) : TFhirPatientW; virtual; abstract;
    function wrapEncounter(r : TFHIRResourceV) : TFhirEncounterW; virtual; abstract;
    function wrapBundleEntry(o : TFHIRObject) : TFhirBundleEntryW; virtual; abstract;
    function wrapNamingSystem(o : TFHIRResourceV) : TFHIRNamingSystemW; virtual; abstract;
    function wrapStructureMap(o : TFHIRResourceV) : TFHIRStructureMapW; virtual; abstract;
    function wrapEventDefinition(o : TFHIRResourceV) : TFHIREventDefinitionW; virtual; abstract;
    function wrapConsent(o : TFHIRResourceV) : TFHIRConsentW; virtual; abstract;
    function wrapTestScript(o : TFHIRResourceV) : TFHIRTestScriptW; virtual; abstract;
    function wrapProvenance(o : TFHIRResourceV) : TFhirProvenanceW; virtual; abstract;
    function wrapImmunization(o : TFHIRResourceV) : TFhirImmunizationW; virtual; abstract;

    function makeOpReqLookup : TFHIRLookupOpRequestW; virtual; abstract;
    function makeOpRespLookup : TFHIRLookupOpResponseW; virtual; abstract;
    function makeOpReqSubsumes : TFHIRSubsumesOpRequestW; virtual; abstract;
    function makeOpRespSubsumes : TFHIRSubsumesOpResponseW; virtual; abstract;
    function makeValueSetContains : TFhirValueSetExpansionContainsW; virtual; abstract;

    function parseJson(worker : TFHIRWorkerContextV; bytes : TBytes) : TFHIRResourceV;
    function parseXml(worker : TFHIRWorkerContextV; bytes : TBytes) : TFHIRResourceV;
  end;

  TFHIRVersionFactories = class (TFslObject)
  private
    FVersionArray : array [TFHIRVersion] of TFHIRFactory;
    function getHasVersion(v: TFHIRVersion): boolean;
    function getVersion(v: TFHIRVersion): TFHIRFactory;
    procedure SetVersion(v: TFHIRVersion; const Value: TFHIRFactory);
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRVersionFactories; overload;

    property version[v : TFHIRVersion] : TFHIRFactory read getVersion write SetVersion; default;
    property hasVersion[v : TFHIRVersion] : boolean read getHasVersion;
  end;

  TExpansionOperationOption = (expOptLimited);
  TExpansionOperationOptionSet = set of TExpansionOperationOption;

  { TFHIRWorkerContextWithFactory }

  TFHIRWorkerContextWithFactory = class (TFHIRWorkerContextV)
  private
    FFactory : TFHIRFactory;
    FLoadInfo : TPackageLoadingInformation;
    FPcm : TFHIRPackageManager;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory; pcm : TFHIRPackageManager); overload; virtual;
    destructor Destroy; override;

    function link : TFHIRWorkerContextWithFactory;
    procedure Unload; override;

    property Factory : TFHIRFactory read FFactory;
    property pcm : TFHIRPackageManager read FPcm;
    property LoadInfo : TPackageLoadingInformation read FLoadInfo;

    procedure loadResourceJson(rType, id : String; json : TStream); override;
    procedure seeResource(res : TFHIRResourceV); overload; virtual; abstract;
    procedure seeResource(res : TFHIRResourceProxyV); overload; virtual; abstract;
    procedure dropResource(rtpe, id : String); overload; virtual; abstract;

    procedure setNonSecureTypes(names : Array of String); virtual; abstract;

    function getResourceNames : TFslStringSet; virtual; abstract;
    function fetchResource(rType : String; url, version : String) : TFhirResourceV; overload; virtual; abstract;
    function expand(vs : TFhirValueSetW; options : TExpansionOperationOptionSet = []) : TFHIRValueSetW; overload; virtual; abstract;
    function supportsSystem(systemUri, version : string) : boolean; overload; virtual; abstract;
    function validateCode(systemUri, version, code, display : String) : TValidationResult; overload; virtual; abstract;
    function validateCode(systemUri, version, code : String; vs : TFhirValueSetW) : TValidationResult; overload; virtual; abstract;
    function allResourceNames : TArray<String>; overload; virtual; abstract;
    function nonSecureResourceNames : TArray<String>; overload; virtual; abstract;
    procedure listStructures(list : TFslList<TFhirStructureDefinitionW>); overload; virtual; abstract;
    function getProfileLinks(non_resources : boolean) : TFslStringMatch; virtual; abstract;
    procedure LoadingFinished; virtual;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; virtual; abstract;
  end;

  { TFHIRFactoryX }
  TFHIRFactoryX = class (TFHIRFactory)
  public
    function versionName : String; override;
    function corePackage : String; override;
    function specUrl : String; override;
    function description : String; virtual;
    function resourceNames : TArray<String>; override;
    function canonicalResources : TArray<String>; override;
    function isResourceName(name : String) : boolean; virtual;
    function resCategory(name: String) : TTokenCategory; override;
    function URLs : TCommonURLs; override;
    function makeParser(worker : TFHIRWorkerContextV; format : TFHIRFormat; langList : THTTPLanguageList) : TFHIRParser; override;
    function makeComposer(worker : TFHIRWorkerContextV; format : TFHIRFormat; langList : THTTPLanguageList; style: TFHIROutputStyle) : TFHIRComposer; override;
    function makeValidator(worker : TFHIRWorkerContextV) : TFHIRValidatorV; override;
    function makeGenerator(worker : TFHIRWorkerContextV) : TFHIRNarrativeGeneratorBase; override;
    function makePathEngine(worker : TFHIRWorkerContextV; ucum : TUcumServiceInterface) : TFHIRPathEngineV; override;
    function makeElementModelManager : TFHIRBaseMMManager; override;
    function createFromProfile(worker : TFHIRWorkerContextV; profile : TFhirStructureDefinitionW) : TFHIRResourceV; override;
    function createPropertyList(name : String; bPrimitiveValues : Boolean) : TFHIRPropertyList; Virtual;
    function makeClient(worker : TFHIRWorkerContextV; url : String; fmt : TFHIRFormat) : TFhirClientV; overload;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat) : TFhirClientV; overload;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal) : TFhirClientV; overload;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClientV; overload; virtual;  abstract;// because using indy is necessary if you're writing a server, or unixready code
    function makeClientThreaded(worker : TFHIRWorkerContextV; internal : TFhirClientV; event : TThreadManagementEvent) : TFhirClientV; overload; override;
    function makeClientInt(worker : TFHIRWorkerContextV; langList : THTTPLanguageList; comm : TFHIRClientCommunicator) : TFhirClientV; overload; override;
    function makeHealthcareCard : THealthcareCard; override;
    function getXhtml(res : TFHIRResourceV) : TFHIRXhtmlNode; override;
    procedure setXhtml(res : TFHIRResourceV; x : TFHIRXhtmlNode); override;
    function resetXhtml(r : TFHIRResourceV) : TFHIRXhtmlNode; override;
    procedure clearXhtml(res : TFHIRResourceV); override;
    function getContained(r : TFHIRResourceV) : TFslList<TFHIRResourceV>; override;
    function describe(r : TFHIRResourceV) : String; override;
    procedure markWithTag(r : TFHIRResourceV; systemUri, code, display : String); override;
    procedure checkNoModifiers(res : TFHIRObject; method, param : string; allowed : TArray<String> = nil); override;
    function buildOperationOutcome(langList : THTTPLanguageList; e : exception; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; override;
    Function buildOperationOutcome(langList : THTTPLanguageList; message : String; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; override;
    function makeByName(const name : String) : TFHIRObject; override;
    function makeResource(const name : String) : TFHIRResourceV;
    function makeBoolean(b : boolean): TFHIRObject; override;
    function makeCode(s : string) : TFHIRObject; override;
    function makeCoding(systemUri, version, code, display : String) : TFHIRObject; overload; override;
    function makeCodeableConcept(coding : TFHIRCodingW = nil) : TFHIRObject; override;
    function makeString(s : string) : TFHIRObject; override;
    function makeUri(s : string) : TFHIRObject; override;
    function makeInteger(s : string) : TFHIRObject; override;
    function makeDecimal(s : string) : TFHIRObject; override;
    function makeBase64Binary(s : string) : TFHIRObject; override;
    function makeBinary(content : TBytes; contentType : String) : TFHIRResourceV; override;
    function makeParamsFromForm(s : TStream) : TFHIRResourceV; override;
    function makeDateTime(dt : TFslDateTime) : TFHIRObject; override;
    function makeDtFromForm(part : TMimePart; langList : THTTPLanguageList; name : String; type_ : string) : TFHIRXVersionElementWrapper; override;
    function makeDuration(dt : TDateTime) : TFHIRObject; override;
    function makeBundle(list : TFslList<TFHIRResourceV>) : TFHIRBundleW; override;
    function wrapPrimitive(p : TFHIRObject) : TFHIRPrimitiveW; override;
    function makeParameters : TFHIRParametersW; override;
    function makeTerminologyCapablities : TFhirTerminologyCapabilitiesW; override;
    function makeIssue(level : TIssueSeverity; issue: TFhirIssueType; location, message: String) : TFhirOperationOutcomeIssueW; override;
    function makeProxy(pi : TNpmPackageResource; worker : TFHIRWorkerContextV; lock : TFslLock) : TFHIRResourceProxyV; overload; override;
    function makeProxy(presource : TFHIRResourceV) : TFHIRResourceProxyV; overload; override;
    function wrapResource(r : TFHIRResourceV) : TFHIRXVersionResourceWrapper; virtual;
    function wrapCapabilityStatement(r : TFHIRResourceV) : TFHIRCapabilityStatementW; override;
    function wrapStructureDefinition(r : TFHIRResourceV) : TFhirStructureDefinitionW; override;
    function wrapValueSet(r : TFHIRResourceV) : TFhirValueSetW; override;
    function wrapCodeSystem(r : TFHIRResourceV) : TFhirCodeSystemW; override;
    function wrapConceptMap(r : TFHIRResourceV) : TFhirConceptMapW; override;
    function wrapExtension(o : TFHIRObject) : TFhirExtensionW; override;
    function wrapCoding(o : TFHIRObject) : TFhirCodingW; override;
    function wrapCodeableConcept(o : TFHIRObject) : TFhirCodeableConceptW; override;
    function wrapIdentifier(o : TFHIRObject) : TFhirIdentifierW; override;
    function wrapOperationOutcome(r : TFHIRResourceV) : TFhirOperationOutcomeW; override;
    function wrapBundle(r : TFHIRResourceV) : TFhirBundleW; override;
    function wrapParams(r : TFHIRResourceV) : TFHIRParametersW; override;
    function wrapMeta(r : TFHIRResourceV) : TFhirMetaW; overload; override;
    function wrapMeta(r : TFHIRObject) : TFhirMetaW; overload; override;
    function wrapBinary(r : TFHIRResourceV) : TFhirBinaryW; override;
    function wrapAuditEvent(r : TFHIRResourceV) : TFhirAuditEventW; override;
    function wrapSubscription(r : TFHIRResourceV) : TFhirSubscriptionW; override;
    function wrapSubscriptionTopic(r : TFHIRResourceV) : TFhirSubscriptionTopicW; override;
    function wrapObservation(r : TFHIRResourceV) : TFhirObservationW; override;
    function wrapAttachment(r : TFHIRObject) : TFHIRAttachmentW; override;
    function wrapQuantity(r : TFHIRObject) : TFhirQuantityW; override;
    function wrapPeriod(r : TFHIRObject) : TFhirPeriodW; override;
    function wrapGroup(r : TFHIRResourceV) : TFhirGroupW; override;
    function wrapPatient(r : TFHIRResourceV) : TFhirPatientW; override;
    function wrapEncounter(r : TFHIRResourceV) : TFhirEncounterW; override;
    function wrapBundleEntry(o : TFHIRObject) : TFhirBundleEntryW; override;
    function wrapNamingSystem(o : TFHIRResourceV) : TFHIRNamingSystemW; override;
    function wrapStructureMap(o : TFHIRResourceV) : TFHIRStructureMapW; override;
    function wrapEventDefinition(o : TFHIRResourceV) : TFHIREventDefinitionW; override;
    function wrapConsent(o : TFHIRResourceV) : TFHIRConsentW; override;
    function wrapTestScript(o : TFHIRResourceV) : TFHIRTestScriptW; override;
    function wrapProvenance(o : TFHIRResourceV) : TFhirProvenanceW; override;
    function wrapImmunization(o : TFHIRResourceV) : TFhirImmunizationW; override;
    function makeOpReqLookup : TFHIRLookupOpRequestW; override;
    function makeOpRespLookup : TFHIRLookupOpResponseW; override;
    function makeOpReqSubsumes : TFHIRSubsumesOpRequestW; override;
    function makeOpRespSubsumes : TFHIRSubsumesOpResponseW; override;
    function makeValueSetContains : TFhirValueSetExpansionContainsW; override;
  end;

implementation

{ TFHIRFactoryX }

function TFHIRFactoryX.versionName: String;
begin
  raise EFslException.Create('versionName is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.corePackage: String;
begin
  raise EFslException.Create('corePackage is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.specUrl: String;
begin
  raise EFslException.Create('specUrl is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.description: String;
begin
  raise EFslException.Create('description is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.resourceNames: TArray<String>;
begin
  raise EFslException.Create('resourceNames is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.canonicalResources: TArray<String>;
begin
  raise EFslException.Create('canonicalResources is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.isResourceName(name: String): boolean;
begin
  raise EFslException.Create('isResourceName is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.resCategory(name: String): TTokenCategory;
begin
  raise EFslException.Create('resCategory is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.URLs: TCommonURLs;
begin
  raise EFslException.Create('URLs is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; langList : THTTPLanguageList): TFHIRParser;
begin
  raise EFslException.Create('makeParser is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; langList : THTTPLanguageList; style: TFHIROutputStyle): TFHIRComposer;
begin
  raise EFslException.Create('makeComposer is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeValidator(worker: TFHIRWorkerContextV): TFHIRValidatorV;
begin
  raise EFslException.Create('makeValidator is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeGenerator(worker: TFHIRWorkerContextV): TFHIRNarrativeGeneratorBase;
begin
  raise EFslException.Create('makeGenerator is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makePathEngine(worker: TFHIRWorkerContextV; ucum: TUcumServiceInterface): TFHIRPathEngineV;
begin
  raise EFslException.Create('makePathEngine is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeElementModelManager: TFHIRBaseMMManager;
begin
  raise EFslException.Create('makeElementModelManager is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.createFromProfile(worker: TFHIRWorkerContextV; profile: TFhirStructureDefinitionW): TFHIRResourceV;
begin
  raise EFslException.Create('createFromProfile is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.createPropertyList(name: String; bPrimitiveValues: Boolean): TFHIRPropertyList;
begin
  raise EFslException.Create('createPropertyList is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeClient(worker: TFHIRWorkerContextV; url: String; fmt: TFHIRFormat): TFhirClientV;
begin
  raise EFslException.Create('makeClient is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeClient(worker: TFHIRWorkerContextV; url: String; kind: TFHIRClientType; fmt: TFHIRFormat): TFhirClientV;
begin
  raise EFslException.Create('makeClient is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeClient(worker: TFHIRWorkerContextV; url: String; kind: TFHIRClientType; fmt: TFHIRFormat; timeout: cardinal): TFhirClientV;
begin
  raise EFslException.Create('makeClient is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeClientThreaded(worker: TFHIRWorkerContextV;  internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
begin
  raise EFslException.Create('makeClientThreaded is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeClientInt(worker: TFHIRWorkerContextV; langList : THTTPLanguageList; comm: TFHIRClientCommunicator): TFhirClientV;
begin
  raise EFslException.Create('makeClientInt is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeHealthcareCard: THealthcareCard;
begin
  raise EFslException.Create('makeHealthcareCard is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.getXhtml(res: TFHIRResourceV): TFHIRXhtmlNode;
begin
  raise EFslException.Create('getXhtml is not implemented in the non-versioned FHIRFactory');
end;

procedure TFHIRFactoryX.setXhtml(res: TFHIRResourceV; x: TFHIRXhtmlNode);
begin
  raise EFslException.Create('. is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.resetXhtml(r: TFHIRResourceV): TFHIRXhtmlNode;
begin
  raise EFslException.Create('resetXhtml is not implemented in the non-versioned FHIRFactory');
end;

procedure TFHIRFactoryX.clearXhtml(res: TFHIRResourceV);
begin
  raise EFslException.Create('. is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.getContained(r: TFHIRResourceV): TFslList<TFHIRResourceV>;
begin
  raise EFslException.Create('getContained is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.describe(r: TFHIRResourceV): String;
begin
  raise EFslException.Create('describe is not implemented in the non-versioned FHIRFactory');
end;

procedure TFHIRFactoryX.markWithTag(r: TFHIRResourceV; systemUri, code, display: String);
begin
  raise EFslException.Create('. is not implemented in the non-versioned FHIRFactory');
end;

procedure TFHIRFactoryX.checkNoModifiers(res: TFHIRObject; method, param: string; allowed: TArray<String>);
begin
  raise EFslException.Create('. is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.buildOperationOutcome(langList : THTTPLanguageList; e: exception; issueCode: TFhirIssueType): TFhirResourceV;
begin
  raise EFslException.Create('buildOperationOutcome is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.buildOperationOutcome(langList : THTTPLanguageList; message: String; issueCode: TFhirIssueType): TFhirResourceV;
begin
  raise EFslException.Create('buildOperationOutcome is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeByName(const name: String): TFHIRObject;
begin
  raise EFslException.Create('makeByName is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeResource(const name: String): TFHIRResourceV;
begin
  raise EFslException.Create('makeResource is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeBoolean(b: boolean): TFHIRObject;
begin
  raise EFslException.Create('makeBoolean is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeCode(s: string): TFHIRObject;
begin
  raise EFslException.Create('makeCode is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeCodeableConcept(coding: TFHIRCodingW): TFHIRObject;
begin
  raise EFslException.Create('makeCodeableConcept is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeCoding(systemUri, version, code, display: String): TFHIRObject;
begin
  raise EFslException.Create('makeCoding is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeString(s: string): TFHIRObject;
begin
  result := TFHIRSystemString.Create(s);
end;

function TFHIRFactoryX.makeUri(s: string): TFHIRObject;
begin
  raise EFslException.Create('makeUri is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeInteger(s: string): TFHIRObject;
begin
  raise EFslException.Create('makeInteger is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeDecimal(s: string): TFHIRObject;
begin
  raise EFslException.Create('makeDecimal is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeBinary(content: TBytes; contentType: String): TFHIRResourceV;
begin
  raise EFslException.Create('makeBinary is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeParamsFromForm(s: TStream): TFHIRResourceV;
begin
  raise EFslException.Create('makeParamsFromForm is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeDateTime(dt: TFslDateTime): TFHIRObject;
begin
  raise EFslException.Create('makeDateTime is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeDtFromForm(part: TMimePart; langList : THTTPLanguageList; name: String; type_: string): TFHIRXVersionElementWrapper;
begin
  raise EFslException.Create('makeDtFromForm is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeDuration(dt: TDateTime): TFHIRObject;
begin
  raise EFslException.Create('makeDuration is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeBundle(list: TFslList<TFHIRResourceV>): TFHIRBundleW;
begin
  raise EFslException.Create('makeBundle is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapPrimitive(p: TFHIRObject): TFHIRPrimitiveW;
begin
  if (p = nil) then
    result := nil
  else
    result := TFHIRPrimitiveX.Create(p);
end;

function TFHIRFactoryX.makeParameters: TFHIRParametersW;
begin
  raise EFslException.Create('makeParameters is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeTerminologyCapablities: TFhirTerminologyCapabilitiesW;
begin
  raise EFslException.Create('makeTerminologyCapablities is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeIssue(level: TIssueSeverity; issue: TFhirIssueType; location, message: String): TFhirOperationOutcomeIssueW;
begin
  raise EFslException.Create('makeIssue is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeProxy(pi: TNpmPackageResource; worker: TFHIRWorkerContextV; lock: TFslLock): TFHIRResourceProxyV;
begin
  raise EFslException.Create('makeProxy is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeProxy(presource: TFHIRResourceV): TFHIRResourceProxyV;
begin
  raise EFslException.Create('makeProxy is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapResource(r: TFHIRResourceV): TFHIRXVersionResourceWrapper;
begin
  raise EFslException.Create('wrapResource is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapCapabilityStatement(r: TFHIRResourceV): TFHIRCapabilityStatementW;
begin
  raise EFslException.Create('wrapCapabilityStatement is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapStructureDefinition(r: TFHIRResourceV): TFhirStructureDefinitionW;
begin
  raise EFslException.Create('wrapStructureDefinition is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapValueSet(r: TFHIRResourceV): TFhirValueSetW;
begin
  raise EFslException.Create('wrapValueSet is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapCodeSystem(r: TFHIRResourceV): TFhirCodeSystemW;
begin
  raise EFslException.Create('wrapCodeSystem is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapConceptMap(r: TFHIRResourceV): TFhirConceptMapW;
begin
  raise EFslException.Create('wrapConceptMap is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapExtension(o: TFHIRObject): TFhirExtensionW;
begin
  raise EFslException.Create('wrapExtension is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapCoding(o: TFHIRObject): TFhirCodingW;
begin
  raise EFslException.Create('wrapCoding is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapCodeableConcept(o: TFHIRObject): TFhirCodeableConceptW;
begin
  raise EFslException.Create('wrapCodeableConcept is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapIdentifier(o: TFHIRObject): TFhirIdentifierW;
begin
  raise EFslException.Create('wrapIdentifier is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapOperationOutcome(r: TFHIRResourceV): TFhirOperationOutcomeW;
begin
  raise EFslException.Create('wrapOperationOutcome is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapBundle(r: TFHIRResourceV): TFhirBundleW;
begin
  raise EFslException.Create('wrapBundle is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapParams(r: TFHIRResourceV): TFHIRParametersW;
begin
  raise EFslException.Create('wrapParams is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapMeta(r: TFHIRResourceV): TFhirMetaW;
begin
  raise EFslException.Create('wrapMeta is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapMeta(r: TFHIRObject): TFhirMetaW;
begin
  raise EFslException.Create('wrapMeta is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapBinary(r: TFHIRResourceV): TFhirBinaryW;
begin
  raise EFslException.Create('wrapBinary is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapAuditEvent(r: TFHIRResourceV): TFhirAuditEventW;
begin
  raise EFslException.Create('wrapAuditEvent is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapSubscription(r: TFHIRResourceV): TFhirSubscriptionW;
begin
  raise EFslException.Create('wrapSubscription is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapSubscriptionTopic(r: TFHIRResourceV): TFhirSubscriptionTopicW;
begin
  raise EFslException.Create('wrapSubscriptionTopic is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapObservation(r: TFHIRResourceV): TFhirObservationW;
begin
  raise EFslException.Create('wrapObservation is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapAttachment(r: TFHIRObject): TFHIRAttachmentW;
begin
  raise EFslException.Create('wrapAttachment is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapQuantity(r: TFHIRObject): TFhirQuantityW;
begin
  raise EFslException.Create('wrapQuantity is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapPeriod(r: TFHIRObject): TFhirPeriodW;
begin
  raise EFslException.Create('wrapPeriod is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapGroup(r: TFHIRResourceV): TFhirGroupW;
begin
  raise EFslException.Create('wrapGroup is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapPatient(r: TFHIRResourceV): TFhirPatientW;
begin
  raise EFslException.Create('wrapPatient is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapEncounter(r: TFHIRResourceV): TFhirEncounterW;
begin
  raise EFslException.Create('wrapEncounter is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapBundleEntry(o: TFHIRObject): TFhirBundleEntryW;
begin
  raise EFslException.Create('wrapBundleEntry is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapNamingSystem(o: TFHIRResourceV): TFHIRNamingSystemW;
begin
  raise EFslException.Create('wrapNamingSystem is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapStructureMap(o: TFHIRResourceV): TFHIRStructureMapW;
begin
  raise EFslException.Create('wrapStructureMap is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapEventDefinition(o: TFHIRResourceV): TFHIREventDefinitionW;
begin
  raise EFslException.Create('wrapEventDefinition is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapConsent(o: TFHIRResourceV): TFHIRConsentW;
begin
  raise EFslException.Create('wrapConsent is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapTestScript(o: TFHIRResourceV): TFHIRTestScriptW;
begin
  raise EFslException.Create('wrapTestScript is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapProvenance(o: TFHIRResourceV): TFhirProvenanceW;
begin
  raise EFslException.Create('wrapProvenance is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.wrapImmunization(o: TFHIRResourceV): TFhirImmunizationW;
begin
  raise EFslException.Create('wrapImmunization is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeOpReqLookup: TFHIRLookupOpRequestW;
begin
  raise EFslException.Create('makeOpReqLookup is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeOpRespLookup: TFHIRLookupOpResponseW;
begin
  raise EFslException.Create('makeOpRespLookup is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeOpReqSubsumes: TFHIRSubsumesOpRequestW;
begin
  raise EFslException.Create('makeOpReqSubsumes is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeOpRespSubsumes: TFHIRSubsumesOpResponseW;
begin
  raise EFslException.Create('makeOpRespSubsumes is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeValueSetContains: TFhirValueSetExpansionContainsW;
begin
  raise EFslException.Create('makeValueSetContains is not implemented in the non-versioned FHIRFactory');
end;

function TFHIRFactoryX.makeBase64Binary(s : string) : TFHIRObject;
begin
  raise EFslException.Create('makeBase64Binary is not implemented in the non-versioned FHIRFactory');
end;

{ TFHIRFactory }

function TFHIRFactory.description: String;
begin
  result := 'Unknown version';
end;

function TFHIRFactory.isResourceName(name: String): boolean;
var
  s : String;
begin
  result := false;
  for s in ResourceNames do
    if s = name then
      exit(true);
end;

function TFHIRFactory.link: TFHIRFactory;
begin
  result := TFHIRFactory(inherited link);
end;

function TFHIRFactory.createPropertyList(name : String; bPrimitiveValues : Boolean) : TFHIRPropertyList;
var
  o : TFHIRObject;
begin
  o := makeByName(name);
  try
    result := o.createPropertyList(bPrimitiveValues);
  finally
    o.free;
  end;
end;

function TFHIRFactory.makeClient(worker : TFHIRWorkerContextV; url : String; fmt : TFHIRFormat) : TFhirClientV;
begin
  result := makeClient(worker, url, fctCrossPlatform, fmt, 0, '');
end;

function TFHIRFactory.makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat) : TFhirClientV;
begin
  result := makeClient(worker, url, kind, fmt, 0, '');
end;

function TFHIRFactory.makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal) : TFhirClientV;
begin
  result := makeClient(worker, url, kind, fmt, timeout, '');
end;

function TFHIRFactory.version: TFHIRVersion;
begin
  result := fhirVersionUnknown;
end;

function TFHIRFactory.versionString: String;
begin
  result := '??';
end;

function TFHIRFactory.makeResource(const name: String): TFHIRResourceV;
begin
  result := makeByName(name) as TFHIRResourceV;
end;

function TFHIRFactory.parseJson(worker: TFHIRWorkerContextV; bytes : TBytes): TFHIRResourceV;
var
  parser : TFHIRParser;
begin
  parser := makeParser(worker, ffJson, nil);
  try
    result := parser.parseResource(bytes);
  finally
    parser.free;
  end;
end;

function TFHIRFactory.parseXml(worker: TFHIRWorkerContextV; bytes : TBytes): TFHIRResourceV;
var
  parser : TFHIRParser;
begin
  parser := makeParser(worker, ffXml, nil);
  try
    result := parser.parseResource(bytes);
  finally
    parser.free;
  end;
end;

function TFHIRFactory.makeCoding(systemUri, code, display: String): TFHIRObject;
begin
  result := makeCoding(systemUri, '', code, display);
end;

function TFHIRFactory.wrapResource(r: TFHIRResourceV): TFHIRXVersionResourceWrapper;
begin
  if (r.fhirType = 'CapabilityStatement') then
    result := wrapCapabilityStatement(r)
  else if (r.fhirType = 'StructureDefinition') then
    result := wrapStructureDefinition(r)
  else if (r.fhirType = 'ValueSet') then
    result := wrapValueSet(r)
  else if (r.fhirType = 'CodeSystem') then
    result :=  wrapCodeSystem(r)
  else if (r.fhirType = 'ConceptMap') then
    result := wrapConceptMap(r)
  else if (r.fhirType = 'OperationOutcome') then
    result := wrapOperationOutcome(r)
  else if (r.fhirType = 'Bundle') then
    result := wrapBundle(r)
  else if (r.fhirType = 'Params') then
    result := wrapParams(r)
  else if (r.fhirType = 'Binary') then
    result := wrapBinary(r)
  else if (r.fhirType = 'AuditEvent') then
    result := wrapAuditEvent(r)
  else if (r.fhirType = 'Subscription') then
    result := wrapSubscription(r)
  else if (r.fhirType = 'SubscriptionTopic') then
    result := wrapSubscriptionTopic(r)
  else if (r.fhirType = 'Observation') then
    result := wrapObservation(r)
  else if (r.fhirType = 'Group') then
    result := wrapGroup(r)
  else if (r.fhirType = 'Patient') then
    result := wrapPatient(r)
  else if (r.fhirType = 'Encounter') then
    result := wrapEncounter(r)
  else if (r.fhirType = 'NamingSystem') then
    result := wrapNamingSystem(r)
  else if (r.fhirType = 'StructureMap') then
    result := wrapStructureMap(r)
  else if (r.fhirType = 'EventDefinition') then
    result := wrapEventDefinition(r)
  else if (r.fhirType = 'Consent') then
    result := wrapConsent(r)
  else if (r.fhirType = 'TestScript') then
    result := wrapTestScript(r)
  else if (r.fhirType = 'Provenance') then
    result := wrapProvenance(r)
  else if (r.fhirType = 'Immunization') then
    result := wrapImmunization(r)
  else
    result := nil;
end;

function TFHIRFactory.makeCoding(systemUri, code: String): TFHIRObject;
begin
  result := makeCoding(systemUri, '', code, '');
end;

{ TFHIRVersionFactories }

constructor TFHIRVersionFactories.Create;
var
  v : TFHIRVersion;
begin
  inherited;
  for v in FHIR_ALL_VERSIONS do
    FVersionArray[v] := nil;
end;

destructor TFHIRVersionFactories.Destroy;
var
  v : TFHIRVersion;
begin
  for v in FHIR_ALL_VERSIONS do
    FVersionArray[v].free;
  inherited;
end;

function TFHIRVersionFactories.getHasVersion(v: TFHIRVersion): boolean;
begin
  result := FVersionArray[v] <> nil;
end;

function TFHIRVersionFactories.getVersion(v: TFHIRVersion): TFHIRFactory;
begin
  result := FVersionArray[v];
end;

function TFHIRVersionFactories.link: TFHIRVersionFactories;
begin
  result := TFHIRVersionFactories(inherited link);
end;

procedure TFHIRVersionFactories.SetVersion(v: TFHIRVersion; const Value: TFHIRFactory);
begin
  FVersionArray[v].free;
  FVersionArray[v] := value;
end;



{ TFHIRWorkerContextWithFactory }

constructor TFHIRWorkerContextWithFactory.Create(factory: TFHIRFactory; pcm : TFHIRPackageManager);
begin
  inherited Create;
  FFactory := factory;
  FLoadInfo := TPackageLoadingInformation.Create(FFactory.versionString);
  FLoadInfo.OnLoadEvent := loadResourceJson;
  FPcm := pcm;
end;

destructor TFHIRWorkerContextWithFactory.Destroy;
begin
  FPcm.free;
  FLoadInfo.free;
  FFactory.free;
  inherited;
end;

function TFHIRWorkerContextWithFactory.link: TFHIRWorkerContextWithFactory;
begin
  result := TFHIRWorkerContextWithFactory(inherited link);
end;

procedure TFHIRWorkerContextWithFactory.Unload;
begin
  inherited Unload;
  FPcm.Unload;
end;

procedure TFHIRWorkerContextWithFactory.LoadingFinished;
begin
  // nothing here
end;

procedure TFHIRWorkerContextWithFactory.loadResourceJson(rType, id: String;
  json: TStream);
var
  p : TFHIRParser;
begin
  p := Factory.makeParser(self.link, ffJson, nil);
  try
    p.source := json;
    p.Parse;
    SeeResource(p.resource);
  finally
    p.free;
  end;
end;

function TFHIRWorkerContextWithFactory.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FFactory.sizeInBytes(magic));
  inc(result, FLoadInfo.sizeInBytes(magic));
end;

{ TFHIRValidatorContext }

constructor TFHIRValidatorContext.Create;
begin
  inherited;
  FOwned := TFslList<TFslObject>.Create;
  FIssues := TFslList<TFhirOperationOutcomeIssueW>.Create;
end;

destructor TFHIRValidatorContext.Destroy;
  begin
  FOwned.free;
  FIssues.free;
  inherited;
end;

procedure TFHIRValidatorContext.SetIssues(const Value: TFslList<TFhirOperationOutcomeIssueW>);
begin
  FIssues.free;
  FIssues := Value;
end;


function TFHIRValidatorContext.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FIssues.sizeInBytes(magic));
  inc(result, Fowned.sizeInBytes(magic));
  inc(result, (FOperationDescription.length * sizeof(char)) + 12);
end;

{ TFHIRValidatorV }

constructor TFHIRValidatorV.Create(context: TFHIRWorkerContextWithFactory);
begin
  inherited Create;
  FContext := context;
end;

destructor TFHIRValidatorV.Destroy;
begin
  FContext.free;
  inherited;
end;

procedure TFHIRValidatorV.Unload;
begin
  // nothing
end;

procedure TFHIRValidatorV.doProgress(path: String);
begin
  if assigned(FOnProgress) then
    FOnProgress(self, path);
end;

end.
