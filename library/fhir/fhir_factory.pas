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
  fsl_ucum, fhir_objects, fhir_parser, fhir_narrative, fhir_pathengine, fhir_common, fhir_xhtml, fhir_elementmodel, fhir_client;

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
    function sizeInBytesV : cardinal; override;
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

  TFHIRValidatorV = class abstract(TFslObject)
  private
    FOnProgress : TValidatorProgressEvent;
    FContext : TFHIRWorkerContextWithFactory;
  protected
    procedure doProgress(path : String);
  public
    constructor Create(context: TFHIRWorkerContextWithFactory); virtual;
    destructor Destroy; override;

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

  TFHIRFactory = class abstract (TFslObject)
  public
    function link : TFHIRFactory; overload;
    function version : TFHIRVersion; virtual;
    function versionString : String; virtual;
    function versionName : String; virtual; abstract;
    function corePackage : String; virtual; abstract;
    function txPackage : String; virtual; abstract;
    function txSupportPackage : String; virtual; abstract;
    function specUrl : String; virtual; abstract;
    function description : String; virtual;
    function resourceNames : TArray<String>; virtual; abstract;
    function canonicalResources : TArray<String>; virtual; abstract;
    function isResourceName(name : String) : boolean; virtual;
    function resCategory(name: String) : TTokenCategory; virtual; abstract;

    function makeParser(worker : TFHIRWorkerContextV; format : TFHIRFormat; const lang : THTTPLanguages) : TFHIRParser; virtual; abstract;
    function makeComposer(worker : TFHIRWorkerContextV; format : TFHIRFormat; const lang : THTTPLanguages; style: TFHIROutputStyle) : TFHIRComposer; virtual; abstract;
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
    function makeClientInt(worker : TFHIRWorkerContextV; const lang : THTTPLanguages; comm : TFHIRClientCommunicator) : TFhirClientV; overload; virtual; abstract;

    function getXhtml(res : TFHIRResourceV) : TFHIRXhtmlNode; virtual; abstract;
    procedure setXhtml(res : TFHIRResourceV; x : TFHIRXhtmlNode); virtual; abstract;
    function resetXhtml(r : TFHIRResourceV) : TFHIRXhtmlNode; virtual; abstract;
    function getContained(r : TFHIRResourceV) : TFslList<TFHIRResourceV>; virtual; abstract;
    procedure markWithTag(r : TFHIRResourceV; systemUri, code, display : String); virtual; abstract;

    procedure checkNoModifiers(res : TFHIRObject; method, param : string; allowed : TArray<String> = nil); virtual; abstract;
    function buildOperationOutcome(const lang : THTTPLanguages; e : exception; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; virtual; abstract;
    Function buildOperationOutcome(const lang : THTTPLanguages; message : String; issueCode : TFhirIssueType = itNull) : TFhirResourceV; overload; virtual; abstract;

    function makeByName(const name : String) : TFHIRObject; virtual; abstract;
    function makeResource(const name : String) : TFHIRResourceV;
    function makeBoolean(b : boolean): TFHIRObject; virtual; abstract;
    function makeCode(s : string) : TFHIRObject; virtual; abstract;
    function makeCoding(systemUri, code : String) : TFHIRObject; overload;
    function makeCoding(systemUri, code, display : String) : TFHIRObject; overload;
    function makeCoding(systemUri, version, code, display : String) : TFHIRObject; overload; virtual; abstract;
    function makeString(s : string) : TFHIRObject; virtual; abstract;
    function makeInteger(s : string) : TFHIRObject; virtual; abstract;
    function makeDecimal(s : string) : TFHIRObject; virtual; abstract;
    function makeBase64Binary(s : string) : TFHIRObject; virtual;  abstract;// must DecodeBase64
    function makeBinary(content : TBytes; contentType : String) : TFHIRResourceV; virtual; abstract;
    function makeParamsFromForm(s : TStream) : TFHIRResourceV; virtual; abstract;
    function makeDateTime(dt : TFslDateTime) : TFHIRObject; virtual; abstract;
    function makeDtFromForm(part : TMimePart; const lang : THTTPLanguages; name : String; type_ : string) : TFHIRXVersionElementWrapper; virtual; abstract;
    function makeDuration(dt : TDateTime) : TFHIRObject; virtual; abstract;
    function makeBundle(list : TFslList<TFHIRResourceV>) : TFHIRBundleW; virtual; abstract;

    function makeParameters : TFHIRParametersW; virtual; abstract;
    function makeTerminologyCapablities : TFhirTerminologyCapabilitiesW; virtual; abstract;
    function makeIssue(level : TIssueSeverity; issue: TFhirIssueType; location, message: String) : TFhirOperationOutcomeIssueW; virtual; abstract;

    function wrapCapabilityStatement(r : TFHIRResourceV) : TFHIRCapabilityStatementW; virtual; abstract;
    function wrapStructureDefinition(r : TFHIRResourceV) : TFhirStructureDefinitionW; virtual; abstract;
    function wrapValueSet(r : TFHIRResourceV) : TFhirValueSetW; virtual; abstract;
    function wrapCodeSystem(r : TFHIRResourceV) : TFhirCodeSystemW; virtual; abstract;
    function wrapConceptMap(r : TFHIRResourceV) : TFhirConceptMapW; virtual; abstract;
    function wrapExtension(o : TFHIRObject) : TFhirExtensionW; virtual; abstract;
    function wrapCoding(o : TFHIRObject) : TFhirCodingW; virtual; abstract;
    function wrapCodeableConcept(o : TFHIRObject) : TFhirCodeableConceptW; virtual; abstract;
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

  TFHIRWorkerContextWithFactory = class (TFHIRWorkerContextV)
  private
    FFactory : TFHIRFactory;
    FLoadInfo : TPackageLoadingInformation;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory); overload; virtual;
    destructor Destroy; override;

    function link : TFHIRWorkerContextWithFactory;

    property Factory : TFHIRFactory read FFactory;
    property LoadInfo : TPackageLoadingInformation read FLoadInfo;

    procedure loadResourceJson(rType, id : String; json : TStream); override;
    procedure seeResource(res : TFHIRResourceV); overload; virtual; abstract;
    procedure dropResource(rtpe, id : String); overload; virtual; abstract;

    procedure setNonSecureTypes(names : Array of String); virtual; abstract;

    function getResourceNames : TFslStringSet; virtual; abstract;
    function fetchResource(rType : String; url : String) : TFhirResourceV; overload; virtual; abstract;
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


implementation

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
    o.Free;
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
  parser := makeParser(worker, ffJson, THTTPlanguages.Create('en'));
  try
    result := parser.parseResource(bytes);
  finally
    parser.Free;
  end;
end;

function TFHIRFactory.parseXml(worker: TFHIRWorkerContextV; bytes : TBytes): TFHIRResourceV;
var
  parser : TFHIRParser;
begin
  parser := makeParser(worker, ffXml, THTTPlanguages.Create('en'));
  try
    result := parser.parseResource(bytes);
  finally
    parser.Free;
  end;
end;

function TFHIRFactory.makeCoding(systemUri, code, display: String): TFHIRObject;
begin
  result := makeCoding(systemUri, '', code, display);
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

constructor TFHIRWorkerContextWithFactory.Create(factory: TFHIRFactory);
begin
  inherited Create;
  FFactory := factory;
  FLoadInfo := TPackageLoadingInformation.Create(FFactory.versionString);
  FLoadInfo.OnLoadEvent := loadResourceJson;
end;

destructor TFHIRWorkerContextWithFactory.Destroy;
begin
  FLoadInfo.Free;
  FFactory.free;
  inherited;
end;

function TFHIRWorkerContextWithFactory.link: TFHIRWorkerContextWithFactory;
begin
  result := TFHIRWorkerContextWithFactory(inherited link);
end;

procedure TFHIRWorkerContextWithFactory.LoadingFinished;
begin
  // nothing here
end;

procedure TFHIRWorkerContextWithFactory.loadResourceJson(rtype, id: String; json: TStream);
var
  p : TFHIRParser;
begin
  p := Factory.makeParser(self.link, ffJson, THTTPLanguages.create('en'));
  try
    p.source := json;
    p.Parse;
    SeeResource(p.resource);
  finally
    p.Free;
  end;
end;

function TFHIRWorkerContextWithFactory.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFactory.sizeInBytes);
  inc(result, FLoadInfo.sizeInBytes);
end;

{ TFHIRValidatorContext }

constructor TFHIRValidatorContext.create;
begin
  inherited;
  FOwned := TFslList<TFslObject>.create;
  FIssues := TFslList<TFhirOperationOutcomeIssueW>.create;
end;

destructor TFHIRValidatorContext.destroy;
  begin
  FOwned.Free;
  FIssues.Free;
  inherited;
end;

procedure TFHIRValidatorContext.SetIssues(const Value: TFslList<TFhirOperationOutcomeIssueW>);
begin
  FIssues.Free;
  FIssues := Value;
end;


function TFHIRValidatorContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FIssues.sizeInBytes);
  inc(result, Fowned.sizeInBytes);
  inc(result, (FOperationDescription.length * sizeof(char)) + 12);
end;

{ TFHIRValidatorV }

constructor TFHIRValidatorV.Create(context: TFHIRWorkerContextWithFactory);
begin
  inherited create;
  FContext := context;
end;

destructor TFHIRValidatorV.Destroy;
begin
  FContext.Free;
  inherited;
end;

procedure TFHIRValidatorV.doProgress(path: String);
begin
  if assigned(FOnProgress) then
    FOnProgress(self, path);
end;

end.
