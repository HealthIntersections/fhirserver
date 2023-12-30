unit fhir_valuesets;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{
todo:
  include designations
  include Inactive

}
uses
  SysUtils, Classes,
  fsl_base, fsl_collections, fsl_utilities, fsl_http, fsl_lang, fsl_logging, fsl_i18n, fsl_versions,
  fhir_objects, fhir_common, ftx_service, fhir_factory, fhir_xhtml, fhir_extensions, fhir_uris, fhir_parser,
  fhir_codesystem_service;

const
  UPPER_LIMIT_NO_TEXT = 1000;
  UPPER_LIMIT_TEXT = 1000;// won't expand a value set bigger than this - just takes too long, and no one's going to do anything with it anyway

  FHIR_VERSION_CANONICAL_SPLIT_2 = '?version=';
  FHIR_VERSION_CANONICAL_SPLIT_3p = '|';

  EXPANSION_DEAD_TIME_SECS = 30;


Type
  TTrueFalseUnknown = (bTrue, bFalse, bUnknown);

  TFhirExpansionParamsVersionRuleMode = (fvmDefault, fvmCheck, fvmOverride);

  { TFhirExpansionParamsVersionRule }

  TFhirExpansionParamsVersionRule = class (TFslObject)
  private
    Fsystem : String;
    Fversion : String;
    FMode : TFhirExpansionParamsVersionRuleMode;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(system, version : String); overload;
    constructor Create(system, version : String; mode : TFhirExpansionParamsVersionRuleMode); overload;

    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property mode : TFhirExpansionParamsVersionRuleMode read FMode write FMode;

    function asString : String;
  end;

  { TFHIRExpansionParams }

  TFHIRExpansionParams = class (TFslObject)
  private
    FVersionRules : TFslList<TFhirExpansionParamsVersionRule>;
    FactiveOnly: boolean;
    FexcludeNested: boolean;
    FGenerateNarrative: boolean;
    FlimitedExpansion: boolean;
    FexcludeNotForUI: boolean;
    FexcludePostCoordinated: boolean;
    FincludeDesignations: boolean;
    FincludeDefinition: boolean;
    FUid: String;
    FMembershipOnly : boolean;
    FDefaultToLatestVersion : boolean;
    FIncompleteOK: boolean;
    FProperties : TStringList;
    FDisplayWarning : boolean;
    FLanguages : THTTPLanguageList;
    FDesignations : TStringList;

    FHasactiveOnly : boolean;
    FHasExcludeNested : boolean;
    FHasGenerateNarrative : boolean;
    FHasLimitedExpansion : boolean;

    FHesExcludeNotForUI : boolean;
    FHasExcludePostCoordinated : boolean;
    FHasIncludeDesignations : boolean;
    FHasIncludeDefinition : boolean;
    FHasDefaultToLatestVersion : boolean;
    FHasIncompleteOK : boolean;
    FHasexcludeNotForUI : boolean;
    FHasMembershipOnly : boolean;
    FHasDisplayWarning : boolean;
    FAltCodeRules : TAlternateCodeOptions;

    function GetHasDesignations: boolean;
    function GetHasLanguages: boolean;
    procedure SetLanguages(value : THTTPLanguageList);
    procedure SetActiveOnly(value : boolean);
    procedure SetExcludeNested(value : boolean);
    procedure SetGenerateNarrative(value : boolean);
    procedure SetLimitedExpansion(value : boolean);
    procedure SetExcludeNotForUI(value : boolean);
    procedure SetExcludePostCoordinated(value : boolean);
    procedure SetIncludeDesignations(value : boolean);
    procedure SetIncludeDefinition(value : boolean);
    procedure SetDefaultToLatestVersion(value : boolean);
    procedure SetIncompleteOK(value : boolean);
    procedure SetDisplayWarning(value : boolean);
    procedure SetMembershipOnly(value : boolean);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRExpansionParams;

    class function defaultProfile : TFHIRExpansionParams;

    procedure seeParameter(name : String; value : TFHIRObject; isValidation, overwrite : boolean);

    property versionRules : TFslList<TFhirExpansionParamsVersionRule> read FVersionRules;

    function getVersionForRule(systemURI : String; mode : TFhirExpansionParamsVersionRuleMode) : String;
    procedure seeVersionRule(url : String; mode : TFhirExpansionParamsVersionRuleMode);

    property activeOnly : boolean read FactiveOnly write SetActiveOnly;
    property languages : THTTPLanguageList read FLanguages write SetLanguages;
    function langSummary : String;
    property includeDefinition : boolean read FincludeDefinition write SetincludeDefinition;
    property generateNarrative : boolean read FGenerateNarrative write SetGenerateNarrative;
    property limitedExpansion : boolean read FlimitedExpansion write SetlimitedExpansion;   // deprecated
    property includeDesignations : boolean read FincludeDesignations write SetincludeDesignations;
    property excludeNested : boolean read FexcludeNested write SetexcludeNested;
    property excludeNotForUI : boolean read FexcludeNotForUI write SetexcludeNotForUI;
    property excludePostCoordinated : boolean read FexcludePostCoordinated write SetexcludePostCoordinated;
    property membershipOnly : boolean read FMembershipOnly write SetMembershipOnly;
    property uid : String read FUid write FUid;
    property defaultToLatestVersion : boolean read FDefaultToLatestVersion write SetDefaultToLatestVersion;
    property incompleteOK : boolean read FIncompleteOK write SetIncompleteOK;
    property displayWarning : boolean read FDisplayWarning write SetDisplayWarning;
    property properties : TStringList read FProperties;
    property altCodeRules : TAlternateCodeOptions read FAltCodeRules;
    property designations : TStringList read FDesignations;


    property hasActiveOnly : boolean read FHasactiveOnly;
    property hasIncludeDefinition : boolean read FHasincludeDefinition;
    property hasGenerateNarrative : boolean read FHasGenerateNarrative;
    property hasLimitedExpansion : boolean read FHaslimitedExpansion;
    property hasIncludeDesignations : boolean read FHasincludeDesignations;
    property hasExcludeNested : boolean read FHasexcludeNested;
    property hasExcludeNotForUI : boolean read FHasexcludeNotForUI;
    property hasExcludePostCoordinated : boolean read FHasexcludePostCoordinated;
    property hasMembershipOnly : boolean read FHasMembershipOnly;
    property hasDefaultToLatestVersion : boolean read FHasDefaultToLatestVersion;
    property hasIncompleteOK : boolean read FHasIncompleteOK;
    property hasDisplayWarning : boolean read FHasDisplayWarning;
    property hasLanguages : boolean read GetHasLanguages;
    property hasDesignations : boolean read GetHasDesignations;

    function hash : String;
  end;

  TSpecialProviderFilterContextNothing = class (TCodeSystemProviderFilterContext);
  TSpecialProviderFilterContextConcepts = class (TCodeSystemProviderFilterContext)
  private
    FList : TFslList<TCodeSystemProviderContext>;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure add(c : TCodeSystemProviderContext);
  end;

  { TFHIRImportedValueSet }

  TFHIRImportedValueSet = class (TFslObject)
  private
    FValueSet: TFHIRValueSetW;
    FMap : TStringList;
    procedure SetValueSet(const Value: TFHIRValueSetW);
    function key(system, code : String) : String;
    procedure addToMap(c : TFhirValueSetExpansionContainsW);
  public
    constructor Create(valueSet : TFHIRValueSetW);
    destructor Destroy; override;
    function link :TFHIRImportedValueSet; overload;

    procedure buildMap;
    function hasCode(system, code : String) : boolean;

    property valueSet : TFHIRValueSetW read FValueSet write SetValueSet;
  end;

  // this is denial of service protection. A terminology operation is not allowed to take too long, and
  // it's not allowed to recurse

  { TTerminologyOperationContext }

  TTerminologyOperationContext = class (TFslObject)
  private
    FDeadTime : UInt64;
    FContexts : TStringList;
    FLangList : THTTPLanguageList;
    FI18n : TI18nSupport;
  public
    constructor Create(i18n : TI18nSupport; langList : THTTPLanguageList);
    destructor Destroy; override;

    function copy : TTerminologyOperationContext;
    function deadCheck : boolean;
    procedure seeContext(vurl : String);
  end;

  TGetValueSetEvent = function (sender : TObject; url, version : String) : TFHIRValueSetW of object;
  TGetProviderEvent = function (sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider of object;
  TGetExpansionEvent = function (sender : TObject; opContext: TTerminologyOperationContext; url, version, filter : String; params : TFHIRExpansionParams; dependencies : TStringList; additionalResources : TFslMetadataResourceList; limit : integer) : TFHIRValueSetW of object;
  TGetSystemVersionsEvent = procedure (sender : TObject; url : String; list : TStringlist) of object;

  { TValueSetWorker }

  TValueSetWorker = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FOnGetValueSet : TGetValueSetEvent;
    FOnGetCSProvider : TGetProviderEvent;
    FOnListCodeSystemVersions : TGetSystemVersionsEvent;
    FOnGetExpansion : TGetExpansionEvent;
    FParams : TFHIRExpansionParams;
    FAdditionalResources : TFslMetadataResourceList;
    FLanguages : TIETFLanguageDefinitions;
    FRequiredSupplements : TStringList;
    FI18n : TI18nSupport;
    FValueSet : TFHIRValueSetW;
    FLangList : THTTPLanguageList;

    function findInAdditionalResources(url, version, resourceType : String; error : boolean) : TFHIRMetadataResourceW;
    function findValueSet(url, version : String) : TFHIRValueSetW;
    function findCodeSystem(url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
    function listVersions(url : String) : String;
    procedure loadSupplements(cse: TFHIRCodeSystemEntry; url: String);
    procedure checkSupplements(cs: TCodeSystemProvider; src: TFHIRXVersionElementWrapper);
  protected
    FAllAltCodes : TAlternateCodeOptions;
    FOpContext : TTerminologyOperationContext;

    procedure seeValueSet(vs : TFHIRValueSetW);

    function sizeInBytesV(magic : integer) : cardinal; override;
    procedure listDisplays(displays : TConceptDesignations; cs : TCodeSystemProvider; c: TCodeSystemProviderContext); overload;
    procedure listDisplays(displays : TConceptDesignations; c: TFhirCodeSystemConceptW); overload;
    procedure listDisplays(displays: TConceptDesignations; c: TFhirValueSetComposeIncludeConceptW; vs : TFHIRValueSetW); overload;
    procedure deadCheck(place : String);
    function isValidating : boolean; virtual; abstract;
  public
    constructor Create(factory : TFHIRFactory; opContext : TTerminologyOperationContext; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion : TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport); overload;
    destructor Destroy; override;
  end;

  { TValueSetChecker }
  TValidationCheckMode = (vcmCode, vcmCoding, vcmCodeableConcept);

  TValueSetChecker = class (TValueSetWorker)
  private
    FOthers : TFslStringObjectMatch; // checkers or code system providers
    FId: String;
    FLog : String;
    FAllValueSet : boolean;

    procedure checkCanonicalStatus(path : string; op : TFhirOperationOutcomeW; resource, source : TFHIRMetadataResourceW); overload;
    procedure checkCanonicalStatus(path : string; op : TFhirOperationOutcomeW; cs : TCodeSystemProvider; source : TFHIRMetadataResourceW); overload;
    procedure checkCanonicalStatus(path : string; op : TFhirOperationOutcomeW; rtype, vurl : String; status: TPublicationStatus; standardsStatus: String; experimental : boolean; source : TFHIRMetadataResourceW); overload;

    function dispWarning : TIssueSeverity;
    function determineSystemFromExpansion(code: String): String;
    function determineSystem(code : String) : String;
    function determineVersion(path, systemURI, versionVS, versionCoding : String; op : TFhirOperationOutcomeW; var message : String) : string;
    function check(path, system, version, code : String; abstractOk, inferSystem : boolean; displays : TConceptDesignations; unknownSystems : TStringList; var message, ver : String; var inactive : boolean; var vstatus : String; var cause : TFhirIssueType; op : TFhirOperationOutcomeW; vcc : TFHIRCodeableConceptW; params: TFHIRParametersW; var contentMode : TFhirCodeSystemContentMode; var impliedSystem : string; unkCodes, messages : TStringList) : TTrueFalseUnknown; overload;
    function findCode(cs : TFhirCodeSystemW; code: String; list : TFhirCodeSystemConceptListW; displays : TConceptDesignations; out isabstract : boolean): boolean;
    function checkConceptSet(path : String; cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code : String; abstractOk : boolean; displays : TConceptDesignations; vs : TFHIRValueSetW; var message : String; var inactive : boolean; var vstatus : String; op : TFHIROperationOutcomeW; vcc : TFHIRCodeableConceptW) : boolean;
    function checkExpansion(path : String; cs: TCodeSystemProvider; cset : TFhirValueSetExpansionContainsW; code : String; abstractOk : boolean; displays : TConceptDesignations; vs : TFHIRValueSetW; var message : String; var inactive : boolean; var vstatus : String; op : TFHIROperationOutcomeW) : boolean;
    function fixedSystemFromValueSet: String;
    procedure prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW);
    function getName: String;
    function valueSetDependsOnCodeSystem(url, version: String): boolean;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
    function isValidating : boolean; override;
  public
    constructor Create(factory : TFHIRFactory; opContext : TTerminologyOperationContext; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion : TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; id : String; i18n : TI18nSupport); overload;
    destructor Destroy; override;

    property id : String read FId;
    property name : String read getName;

    function prepare(vs : TFHIRValueSetW; params : TFHIRExpansionParams) : TFhirParametersW;

    function check(issuePath, system, version, code : String; abstractOk, inferSystem : boolean; op : TFhirOperationOutcomeW) : TTrueFalseUnknown; overload;
    function check(issuePath, system, version, code : String; inferSystem : boolean) : TFhirParametersW; overload;
    function check(issuePath : String; coding : TFhirCodingW; abstractOk, inferSystem : boolean): TFhirParametersW; overload;
    function check(issuePath : String; code: TFhirCodeableConceptW; abstractOk, inferSystem : boolean; mode : TValidationCheckMode) : TFhirParametersW; overload;

    property log : String read FLog;
  end;

  { TValueSetCounter }

  TValueSetCounter = class (TFslObject)
  private
    FCount : integer;
  public
    property count : Integer read FCount;
    procedure increment;
  end;

  { TFHIRValueSetExpander }

  TFHIRValueSetExpander = class (TValueSetWorker)
  private
    FHasCount : boolean;
    FCount : integer;
    FHasOffset : boolean;
    FOffset : integer;
    FLimitCount : integer;
    FCanBeHierarchy : boolean;
    FRootList : TFslList<TFhirValueSetExpansionContainsW>;
    FFullList : TFslList<TFhirValueSetExpansionContainsW>;
    FMap : TFslMap<TFhirValueSetExpansionContainsW>;
    FCSCounter : TFslMap<TValueSetCounter>;

    procedure checkCanonicalStatus(expansion : TFhirValueSetExpansionW; resource : TFHIRMetadataResourceW; source : TFHIRValueSetW); overload;
    procedure checkCanonicalStatus(expansion : TFhirValueSetExpansionW; cs : TCodeSystemProvider; source : TFHIRValueSetW); overload;
    procedure checkCanonicalStatus(expansion: TFhirValueSetExpansionW; vurl : String; status: TPublicationStatus; standardsStatus: String; experimental : boolean; source : TFHIRValueSetW); overload;
    procedure importValueSetItem(p, c: TFhirValueSetExpansionContainsW; imports: TFslList<TFHIRImportedValueSet>; offset: integer);
    function makeFilterForValueSet(cs : TCodeSystemProvider; vs : TFHIRValueSetW) : TCodeSystemProviderFilterContext;
    procedure processCodeAndDescendants(doDelete : boolean; cs : TCodeSystemProvider; context : TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; excludeInactive : boolean; srcUrl : String);

    procedure handleDefine(cs : TFhirCodeSystemW; source : TFhirValueSetCodeSystemW; defines : TFhirCodeSystemConceptListW; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; excludeInactive : boolean; srcURL : String);
    procedure importValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
    procedure excludeValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
    procedure processCodes(doDelete : boolean; cset : TFhirValueSetComposeIncludeW; vsSrc : TFHIRValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; excludeInactive : boolean; var notClosed : boolean);
    procedure handleCompose(source : TFhirValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);

    function passesImports(imports : TFslList<TFHIRImportedValueSet>; system, code : String; offset : integer) : boolean;
    function passesImport(import : TFHIRImportedValueSet; system, code : String) : boolean;

    function processCode(cs : TCodeSystemProvider; parent : TFhirValueSetExpansionContainsW; doDelete : boolean; system, version, code : String; isAbstract, isInactive, deprecated : boolean; displays : TConceptDesignations; definition, itemWeight: string; expansion : TFhirValueSetExpansionW;
        imports : TFslList<TFHIRImportedValueSet>; csExtList, vsExtList : TFslList<TFhirExtensionW>; csProps : TFslList<TFhirCodeSystemConceptPropertyW>; expProps : TFslList<TFhirValueSetExpansionContainsPropertyW>; excludeInactive : boolean; srcURL : string) : TFhirValueSetExpansionContainsW;
    procedure addDefinedCode(cs : TFhirCodeSystemW; system : string; c : TFhirCodeSystemConceptW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; excludeInactive : boolean; srcURL : String);
    function key(system, code : String): string; overload;
    function key(c : TFhirValueSetExpansionContainsW) : string;  overload;
    function expandValueSet(uri, version, filter: String; dependencies: TStringList; var notClosed: boolean): TFHIRValueSetW;
    function canonical(system, version: String): String;
    procedure checkSource(cset: TFhirValueSetComposeIncludeW; exp: TFHIRValueSetExpansionW; filter : TSearchFilterText; srcURL : String);
    procedure checkCanExpandValueset(uri, version: String);
    function useDesignation(cd: TConceptDesignation): boolean;
  protected
    function isValidating : boolean; override;
  public
    constructor Create(factory : TFHIRFactory; opContext : TTerminologyOperationContext; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion : TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport); overload;
    destructor Destroy; override;

    function expand(source : TFHIRValueSetW; params : TFHIRExpansionParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer) : TFHIRValueSetW;
  end;

const
   CODES_TFhirExpansionParamsVersionRuleMode : array [TFhirExpansionParamsVersionRuleMode] of String = ('Default', 'Check', 'Override');

implementation

{ TTerminologyOperationContext }

constructor TTerminologyOperationContext.Create(i18n: TI18nSupport; langList : THTTPLanguageList);
begin
  inherited create;
  FI18n := i18n;
  FLangList := langList;
  FContexts := TStringList.create;
  if (EXPANSION_DEAD_TIME_SECS = 0) or (UnderDebugger) then
    FDeadTime := 0
  else
    FDeadTime := GetTickCount64 + (EXPANSION_DEAD_TIME_SECS * 1000);
end;

destructor TTerminologyOperationContext.Destroy;
begin
  FLangList.free;
  FI18n.free;
  FContexts.free;
  inherited Destroy;
end;

function TTerminologyOperationContext.copy: TTerminologyOperationContext;
begin
  result := TTerminologyOperationContext.create(FI18n.link, FLangList.link);
  result.FContexts.assign(FContexts);
  result.FDeadTime := FDeadTime;
end;

function TTerminologyOperationContext.deadCheck: boolean;
begin
  result := (FDeadTime > 0) and (GetTickCount64 > FDeadTime);
end;

procedure TTerminologyOperationContext.seeContext(vurl: String);
var
  r, s : String;
begin
  if FContexts.IndexOf(vurl) > -1 then
  begin
    r := '';
    for s in FContexts do
      CommaAdd(r, s);
    raise ETerminologyError.create(FI18n.translate('VALUESET_CIRCULAR_REFERENCE', FLangList, [vurl, '['+r+']']), itBusinessRule);
  end
  else
    FContexts.add(vurl);
end;

{ TValueSetCounter }

procedure TValueSetCounter.increment;
begin
  inc(FCount);
end;

{ TSpecialProviderFilterContextConcepts }

constructor TSpecialProviderFilterContextConcepts.Create;
begin
  inherited;
  FList := TFslList<TCodeSystemProviderContext>.create;
end;

destructor TSpecialProviderFilterContextConcepts.Destroy;
begin
  FList.free;
  inherited;
end;

procedure TSpecialProviderFilterContextConcepts.add(c: TCodeSystemProviderContext);
begin
  FList.Add(c);
end;

{ TValueSetWorker }

constructor TValueSetWorker.Create(factory : TFHIRFactory; opContext : TTerminologyOperationContext; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion : TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  Create;
  FFactory := factory;
  FOpContext := opContext;
  FOnGetValueSet := getVS;
  FOnGetCSProvider := getCS;
  FOnListCodeSystemVersions := getVersions;
  FOnGetExpansion := getExpansion;
  FAdditionalResources := txResources;
  FLanguages := languages;
  FRequiredSupplements := TStringList.create;
  FI18n := i18n;
  FAllAltCodes := TAlternateCodeOptions.create;
  FAllAltCodes.all := true;
end;

destructor TValueSetWorker.Destroy;
begin
  FLangList.free;
  FValueSet.free;
  FAllAltCodes.free;
  FRequiredSupplements.free;
  FLanguages.free;
  FAdditionalResources.free;
  FFactory.free;
  FParams.free;
  FI18n.free;
  FOpContext.free;
  inherited;
end;

procedure TValueSetWorker.checkSupplements(cs : TCodeSystemProvider; src : TFHIRXVersionElementWrapper);
var
  ext : TFHIRExtensionW;
  i : integer;
begin
  for ext in src.getExtensionsW(EXT_VSSUPPLEMENT).forEnum do
    if not cs.hasSupplement(ext.valueAsString) then
      raise ETerminologyError.create('ValueSet depends on supplement '''+ext.valueAsString+''' on '+cs.systemUri(nil)+' that is not known', itBusinessRule);
  for i := FRequiredSupplements.count - 1 downto 0 do
    if cs.hasSupplement(FRequiredSupplements[i]) then
      FRequiredSupplements.delete(i);
end;

procedure TValueSetWorker.seeValueSet(vs: TFHIRValueSetW);
var
  ext : TFHIRExtensionW;
  n : String;
  v : TFHIRObject;
begin
  FOpContext.seeContext(vs.vurl);
  for ext in vs.getComposeExtensions.forEnum do
  begin
    if (ext.url = 'http://hl7.org/fhir/tools/StructureDefinion/valueset-expansion-param') then
    begin
      n := ext.getExtensionString('name');
      v := ext.getExtensionValue('value');
      FParams.seeParameter(n, v, isValidating, false);
    end;
  end;
  if not FParams.hasLanguages and (vs.language <> '') then
    FParams.languages := THTTPLanguageList.create(vs.language, not isValidating);
end;

procedure TValueSetWorker.loadSupplements(cse : TFHIRCodeSystemEntry; url : String);
var
  r : TFHIRMetadataResourceW;
  cs : TFhirCodeSystemW;
begin
  for r in FAdditionalResources do
  begin
    if r is TFHIRCodeSystemW then
    begin
      cs := r as TFHIRCodeSystemW;
      if (cs.supplements = url) then
        cse.Supplements.add(cs.link);
    end;
  end;
end;

function isLaterVersion(test, base : String) : boolean;
begin
  if TSemanticVersion.isValid(test) and TSemanticVersion.isValid(base) then
    result := TSemanticVersion.isMoreRecent(test, base)
  else
    result := StringCompare(test, base) > 0;
end;

function TValueSetWorker.findInAdditionalResources(url, version, resourceType : String; error : boolean) : TFHIRMetadataResourceW;
var
  r : TFHIRMetadataResourceW;
  matches : TFslMetadataResourceList;
  i, t : integer;
begin
  if FAdditionalResources = nil then
     exit(nil);

  matches := TFslMetadataResourceList.create;
  try
    for r in FAdditionalResources do
    begin
      if (url <> '') and ((r.url = url) or (r.vurl = url)) and ((version = '') or (version = r.version)) then
      begin
        if r.fhirType <> resourceType then
          if error then
            raise EFHIRException.Create('Attempt to reference '+url+' as a '+resourceType+' when it''s a '+r.fhirType)
          else
            exit(nil);
        matches.add(r.link);
        end;
      end;
    if matches.Count = 0 then
      exit(nil)
    else
    begin
      t := 0;
      for i := 1 to matches.count - 1 do
        if isLaterVersion(matches[i].version, matches[t].version) then
          t := i;
      exit(matches[t]);
    end;
  finally
    matches.free;
  end;
end;

function TValueSetWorker.findCodeSystem(url, version: String; params: TFHIRExpansionParams; nullOk: boolean): TCodeSystemProvider;
var
  r, r2 : TFHIRMetadataResourceW;
  cs, cs2 : TFhirCodeSystemW;
  ts : TStringlist;
  cse : TFHIRCodeSystemEntry;
begin
  if (url = '') then
    exit(nil);
  if (url = URI_NDC) then
  begin
    result := nil;
  end;

  cs := findInAdditionalResources(url, version, 'CodeSystem', not nullOk) as TFhirCodeSystemW;
  if (cs <> nil) and (cs.content = cscmComplete) then
  begin
    cse := TFHIRCodeSystemEntry.Create(cs.link);
    try
      loadSupplements(cse, url);
      exit(TFhirCodeSystemProvider.Create(FLanguages.link, FFactory.link, cse.link));
    finally
      cse.free;
    end;
  end;

  result := FOnGetCSProvider(self, url, version, FParams, true);

  if (result <> nil) then
    exit(result);

  if (cs <> nil) and (cs.content = cscmFragment) then
  begin
    cse := TFHIRCodeSystemEntry.Create(cs.link);
    try
      loadSupplements(cse, url);
      exit(TFhirCodeSystemProvider.Create(FLanguages.link, FFactory.link, cse.link));
    finally
      cse.free;
    end;
  end;

  if not nullok then
    if version = '' then
      raise ETerminologySetup.create('Unable to provide support for code system '+url)
    else
    begin
      ts := TStringList.Create;
      try
        FOnListCodeSystemVersions(self, url, ts);
        if (ts.Count = 0) then
          raise ETerminologySetup.create('Unable to provide support for code system '+url+' version '+version)
        else
          raise ETerminologySetup.create('Unable to provide support for code system '+url+' version '+version+' (known versions = '+ts.CommaText+')');
      finally
        ts.free;
      end;

    end;
end;

function TValueSetWorker.findValueSet(url, version: String): TFHIRValueSetW;
var
  r : TFHIRMetadataResourceW;
begin
  if (url = '') then
    exit(nil);

  r := findInAdditionalResources(url, '', 'ValueSet', false);
  if (r <> nil) then
    exit(r.link as TFHIRValueSetW);

  result := FOnGetValueSet(self, url, version);
end;

function TValueSetWorker.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FFactory.sizeInBytes(magic));
  inc(result, FParams.sizeInBytes(magic));
  inc(result, FAdditionalResources.sizeInBytes(magic));
end;

{ TValueSetChecker }

constructor TValueSetChecker.Create(factory: TFHIRFactory; opContext : TTerminologyOperationContext;
  getVS: TGetValueSetEvent; getCS: TGetProviderEvent;
  getVersions: TGetSystemVersionsEvent; getExpansion: TGetExpansionEvent;
  txResources: TFslMetadataResourceList; languages: TIETFLanguageDefinitions;
  id: String; i18n: TI18nSupport);
begin
  inherited Create(factory, opContext, getVs, getCs, getVersions, getExpansion, txResources, languages, i18n);
  FId := id;
  FOthers := TFslStringObjectMatch.create;
  FOthers.PreventDuplicates;
  FOthers.DefaultValue := nil;
  FOthers.Forced := true;
end;

destructor TValueSetChecker.Destroy;
begin
  FOthers.free;
  inherited;
end;

procedure TValueSetChecker.checkCanonicalStatus(path: string;
  op: TFhirOperationOutcomeW; resource, source: TFHIRMetadataResourceW);
begin
  checkCanonicalStatus(path, op, resource.fhirType, resource.vurl, resource.status, resource.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status'), resource.experimental, source);
end;

procedure TValueSetChecker.checkCanonicalStatus(path : string; op : TFhirOperationOutcomeW; cs: TCodeSystemProvider; source : TFHIRMetadataResourceW);
var
  status: TPublicationStatus;
  standardsStatus: String;
  experimental : boolean;
begin
  cs.getStatus(status, standardsStatus, experimental);
  if (cs.version(nil) <> '') then
    checkCanonicalStatus(path, op, 'CodeSystem', cs.systemUri(nil)+'|'+cs.version(nil), status, standardsStatus, experimental, source)
  else
    checkCanonicalStatus(path, op, 'CodeSystem', cs.systemUri(nil), status, standardsStatus, experimental, source);
end;

procedure TValueSetChecker.checkCanonicalStatus(path : string; op : TFhirOperationOutcomeW; rtype, vurl: String; status: TPublicationStatus; standardsStatus: String; experimental : boolean; source : TFHIRMetadataResourceW);
begin
  if op <> nil then
  begin
    if standardsStatus = 'deprecated' then
      op.addIssue(isInformation, itBusinessRule, '', FI18n.translate('MSG_DEPRECATED', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
    else if standardsStatus = 'withdrawn' then
      op.addIssue(isInformation, itBusinessRule, '', FI18n.translate('MSG_WITHDRAWN', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
    else if status = psRetired then
      op.addIssue(isInformation, itBusinessRule, '', FI18n.translate('MSG_RETIRED', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
    else if (source <> nil) then
    begin
      if experimental and not source.experimental then
        op.addIssue(isInformation, itBusinessRule, '', FI18n.translate('MSG_EXPERIMENTAL', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
      else if ((status = psDraft) or (standardsStatus = 'draft')) and
          not ((source.status = psDraft) or (source.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status') = 'draft')) then
        op.addIssue(isInformation, itBusinessRule, '', FI18n.translate('MSG_DRAFT', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
    end;
  end;
end;


function TValueSetChecker.dispWarning: TIssueSeverity;
begin
  if FParams.displayWarning then
    result := isWarning
  else
    result := isError;
end;

function TValueSetChecker.determineSystemFromExpansion(code: String): String;
var
  exp : TFHIRValueSetExpander;
  dep : TStringList;
  vse : TFHIRValueSetW;
  c : TFhirValueSetExpansionContainsW;
begin
  try
    exp := TFHIRValueSetExpander.create(FFactory.link, FOpContext.copy, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FOnGetExpansion,
      FAdditionalResources.link, FLanguages.link, FI18n.link);
    try
      dep := TStringList.Create;
      try
        vse := exp.expand(FValueSet, FParams, '', dep, 10000, 10000, 0);
        try
          result := '';
          for c in vse.expansion.contains.forEnum do
            if (c.code = code) then
              if result = '' then
                result := c.systemUri
              else
                exit('');
        finally
          vse.free;
        end;
      finally
        dep.free;
      end;
    finally
      exp.free;
    end;
  except
    on e : Exception do
      raise EFslException.create('Exception expanding value set in order to infer system: '+e.message);
  end;
end;

function TValueSetChecker.fixedSystemFromValueSet : String;
var
  c : TFhirValueSetComposeIncludeW;
begin
  if (FValueset = nil) then
    exit('');

  result := '';
  for c in FValueSet.includes.forEnum do
  begin
    if (c.hasValueSets or (c.systemUri = '')) then
      exit('');
    if (result = '') then
      result := c.systemUri
    else if (result <> c.systemURI) then
      exit('');
  end;
end;

function TValueSetChecker.determineSystem(code: String): String;
var
  vsi : TFhirValueSetComposeIncludeW;
  cs : TCodeSystemProvider;
  cc : TFhirValueSetComposeIncludeConceptW;
  match : boolean;
  s, msg : String;
  loc : TCodeSystemProviderContext;
  needDoExpansion : boolean;
begin
  result := '';
  needDoExpansion := false;

  s := fixedSystemFromValueSet();
  if (s > '') then
    exit(s);

  for vsi in FValueSet.excludes.forEnum do
    needDoExpansion := true;
  for vsi in FValueSet.includes do
  begin
    if (length(vsi.valueSets) > 0) or (vsi.systemUri = '') or vsi.hasFilters then
      needDoExpansion := true;
  end;

  if needDoExpansion then
  begin
    result := determineSystemFromExpansion(code);
  end
  else
  begin
    for vsi in FValueSet.includes do
    begin
      cs := findCodeSystem(vsi.systemUri, '', nil, true);
      if (cs = nil) then
        exit('');
      try
        if (vsi.hasConcepts) then
        begin
          for cc in vsi.concepts.forEnum do
          begin
            // if cs.casesensitive then
            match := cc.code = code;
            if (match) then
            begin
              if (result = '') then
                result := vsi.systemUri
              else if (result <> vsi.systemUri) then
                exit('');
            end;
          end;
        end
        else
        begin
          loc := cs.locate(code, nil, msg);
          if loc <> nil then
          begin
            loc.free;
            if (result = '') then
              result := vsi.systemUri
            else if (result <> vsi.systemUri) then
              exit('');
          end;
        end;
      finally
        cs.free;
      end;
    end;
  end;
end;

function addToPath(path, name : String) : String;
begin
  if path = '' then
    result := name
  else
    result := path+'.'+name;
end;

function TValueSetChecker.determineVersion(path, systemURI, versionVS, versionCoding: String; op : TFhirOperationOutcomeW; var message : String): string;
var
  v : string;
begin
  // version might come from multiple places
  v := FParams.getVersionForRule(systemURI, fvmOverride);
  if (v <> '') then
    exit(v);

  // we have two possible versions - the value set reference, and the coding reference
  // it's an error if they don't agree
  if (versionVS = '') and (versionCoding = '') then
    result := FParams.getVersionForRule(systemUri, fvmDefault)
  else if (versionVS = '') then
    result := versionCoding
  else if (versionCoding = '') or (versionCoding = versionVS) then
    result := versionVS
  else
  begin
    message := 'The code system "'+systemUri+'" version "'+versionVS+'" in the ValueSet include is different to the one in the value ("'+versionCoding+'")';
    op.addIssue(isError, itInvalid, addToPath(path, 'version'), message, oicVSProcessing);
    exit('');
  end;
  if result = '' then
    result := FParams.getVersionForRule(systemURI, fvmDefault);
end;

function TValueSetChecker.prepare(vs: TFHIRValueSetW; params : TFHIRExpansionParams) : TFhirParametersW;
var
  cc : TFhirValueSetComposeIncludeW;
  other : TFHIRValueSetW;
  checker : TValueSetChecker;
  ics : TFHIRValueSetCodeSystemW;
  s : String;
  cs : TFhirCodeSystemProvider;
  op : TFhirOperationOutcomeW;
  ext : TFHIRExtensionW;
begin
  result := nil;
  FParams := params.Link;
  if (vs = nil) then
    raise EFslException.Create('Error Error: vs = nil')
  else
  begin
    seeValueSet(vs);
    FRequiredSupplements.clear;
    for ext in vs.getExtensionsW(EXT_VSSUPPLEMENT).forEnum do
      FRequiredSupplements.add(ext.valueAsString);

    vs.checkNoImplicitRules('ValueSetChecker.prepare', 'ValueSet');
    FFactory.checkNoModifiers(vs, 'ValueSetChecker.prepare', 'ValueSet');

    FValueSet := vs.link;
    FAllValueSet := FValueSet.url = 'http://hl7.org/fhir/ValueSet/@all';

    // r2:
    ics := FValueSet.inlineCS;
    if ics <> nil then
    begin
      try
        FFactory.checkNoModifiers(ics, 'ValueSetChecker.prepare', 'CodeSystem');
        cs := TFhirCodeSystemProvider.create(FLanguages.link, ffactory.link, TFHIRCodeSystemEntry.Create(FFactory.wrapCodeSystem(FValueSet.Resource.Link)));
        FOthers.Add(ics.systemUri, cs);
        if (FValueSet.version <> '') then
          FOthers.Add(ics.systemUri+'|'+FValueSet.version, cs.link);
      finally
        ics.free;
      end;
    end;

    if (FValueSet.checkCompose('ValueSetChecker.prepare', 'ValueSet.compose')) then
    begin
      // not r2:
      for s in FValueSet.imports do
      begin
        other := findValueSet(s, '');
        try
          if other = nil then
            raise ETerminologyError.create('Unable to find value set '+s, itUnknown);
          checker := TValueSetChecker.create(FFactory.link, FopContext.copy, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FOnGetExpansion, FAdditionalResources.link, FLanguages.link, other.url, FI18n.link);
          try
            checker.prepare(other, params);
            FOthers.Add(s, checker.Link);
          finally
            checker.free;
          end;
        finally
          other.free;
        end;
      end;

      for cc in FValueSet.includes.forEnum do
        prepareConceptSet('include', cc);
      for cc in FValueSet.excludes.forEnum do
        prepareConceptSet('exclude', cc);
    end;
  end;
  if (FRequiredSupplements.count > 0) then
    raise ETerminologyError.create(FI18n.translatePlural(FRequiredSupplements.Count, 'VALUESET_SUPPLEMENT_MISSING', FParams.languages, [FRequiredSupplements.commaText]), itNotFound);
end;

procedure TValueSetChecker.prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW);
var
  other: TFhirValueSetW;
  checker: TValueSetChecker;
  s : string;
  ccf: TFhirValueSetComposeIncludeFilterW;
  cs: TCodeSystemProvider;
  i : integer;
begin
  FFactory.checkNoModifiers(cc, 'ValueSetChecker.prepare', desc);
  for s in cc.valueSets do
  begin
    if not FOthers.ExistsByKey(s) then
    begin
      other := findValueSet(s, '');
      try
        if other = nil then
          raise ETerminologyError.create('Unable to find value set ' + s, itUnknown);
        checker := TValueSetChecker.create(FFactory.link, FOpContext.copy, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FOnGetExpansion, FAdditionalResources.link, FLanguages.link, other.url, FI18n.link);
        try
          checker.prepare(other, FParams);
          FOthers.Add(s, checker.Link);
        finally
          checker.free;
        end;
      finally
        other.free;
      end;
    end;
  end;
  if not FOthers.ExistsByKey(cc.systemUri) then
    FOthers.Add(cc.systemUri, findCodeSystem(cc.systemUri, cc.version, FParams, true));
  if cc.version = '' then
    cs := FOthers.matches[cc.systemUri] as TCodeSystemProvider
  else
    cs := FOthers.matches[cc.systemUri+'|'+cc.version] as TCodeSystemProvider;
  if cs <> nil then
  begin
    for i := FRequiredSupplements.count - 1 downto 0 do
      if cs.hasSupplement(FRequiredSupplements[i]) then
        FRequiredSupplements.delete(i);
    for ccf in cc.filters.forEnum do
    begin
      FFactory.checkNoModifiers(ccf, 'ValueSetChecker.prepare', desc + '.filter');
      if not (('concept' = ccf.prop) and (ccf.Op in [foIsA, foDescendentOf])) then
        if not cs.doesFilter(ccf.prop, ccf.Op, ccf.value) then
          raise ETerminologyError.create('The filter "' + ccf.prop + ' ' + CODES_TFhirFilterOperator[ccf.Op] + ' ' + ccf.value + '"  from the value set '+FValueSet.url+' was not understood in the context of ' + cs.systemUri(nil), itNotSupported);
    end;
  end;
end;

function TValueSetChecker.findCode(cs : TFhirCodeSystemW; code: String; list : TFhirCodeSystemConceptListW; displays : TConceptDesignations; out isabstract : boolean): boolean;
var
  i : integer;
  ccl : TFhirCodeSystemConceptListW;
begin
  result := false;
  for i := 0 to list.count - 1 do
  begin
    if (code = list[i].code) then
    begin
      result := true;
      if cs = nil then
        isAbstract := false
      else
        isAbstract := cs.isAbstract(list[i]);
      displays.baseLang := FLanguages.parse(cs.language);
      displays.addDesignation(true, true, '', list[i].displayElement); {no .link}
      exit;
    end;
    ccl := list[i].conceptList;
    if findCode(cs, code, ccl, displays, isabstract) then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TValueSetChecker.getName: String;
begin
  if (FValueSet <> nil) then
    result := FValueSet.name
  else
    result := '??';
end;

function TValueSetChecker.check(issuePath, system, version, code: String; abstractOk, inferSystem : boolean; op : TFhirOperationOutcomeW): TTrueFalseUnknown;
var
  msg, ver, impliedSystem, vstatus : string;
  it : TFhirIssueType;
  contentMode : TFhirCodeSystemContentMode;
  unknownSystems, ts, msgs : TStringList;
  inactive : boolean;
begin
  unknownSystems := TStringList.create;
  ts := TStringList.create;
  msgs := TStringList.create;
  try
    unknownSystems.duplicates := dupIgnore;
    unknownSystems.sorted := true;
    result := check(issuePath, system, version, code, abstractOk, inferSystem, nil, unknownSystems, msg, ver, inactive, vstatus, it, op, nil, nil, contentMode, impliedSystem, ts, msgs);
  finally
    unknownSystems.free;
    ts.free;
    msgs.free;
  end;
end;

function vurl(system, version : String) : String;
begin
  if version = '' then
    result := system
  else
    result := system+'|'+version;
end;

function TValueSetChecker.check(path, system, version, code: String; abstractOk, inferSystem: boolean; displays: TConceptDesignations;
  unknownSystems : TStringList;
  var message, ver: String; var inactive : boolean; var vstatus : String; var cause: TFhirIssueType; op: TFhirOperationOutcomeW;
  vcc : TFHIRCodeableConceptW; params: TFHIRParametersW; var contentMode: TFhirCodeSystemContentMode; var impliedSystem: string; unkCodes, messages : TStringList): TTrueFalseUnknown;
var
  cs : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  cc : TFhirValueSetComposeIncludeW;
  excluded, ok, bAdd : boolean;
  isabstract : boolean;
  checker : TValueSetChecker;
  s, v, msg : String;
  ics : TFHIRValueSetCodeSystemW;
  ccl : TFhirCodeSystemConceptListW;
  ccc : TFhirValueSetExpansionContainsW;
  ts : TStringList;
  vss : TFHIRValueSetW;
begin
  if (system = '') and not inferSystem then
  begin
    msg := FI18n.translate('Coding_has_no_system__cannot_validate', FParams.languages, []);
    messages.add(msg);
    op.addIssue(isError, itInvalid, path, msg, oicInvalidData);
    exit(bFalse);
  end;

  ts := TStringList.create;
  try
    FLog := '';
    {special case:}
    contentMode := cscmNull;
    s := FValueSet.url;
    if (s = ANY_CODE_VS) then
    begin
      cs := findCodeSystem(system, version, FParams, true);
      try
        if cs = nil then
        begin
          result := bUnknown;
          cause := itNotFound;
          FLog := 'Unknown code system';
          vss := findValueSet(system, '');
          if (vss <> nil) then
          begin
            vss.free;
            msg := FI18n.translate('Terminology_TX_System_ValueSet2', FParams.languages, [system]);
            messages.add(msg);
            op.addIssue(isError, itInvalid, addToPath(path, 'system'), msg, oicInvalidData);
          end
          else if (version <> '') then
          begin
            msg := FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.languages, [system, version, '['+listVersions(system)+']']);
            messages.add(msg);
            if (unknownSystems.IndexOf(system+'|'+version) = -1) then
            begin
              op.addIssue(isError, itNotFound, addToPath(path, 'system'), msg, oicNotFound);
              unknownSystems.add(system+'|'+version);
            end;
          end
          else
          begin
            msg := FI18n.translate('UNKNOWN_CODESYSTEM', FParams.languages, [system]);
            messages.add(msg);
            op.addIssue(isError, itNotFound, addToPath(path, 'system'), msg, oicNotFound);
            unknownSystems.add(system);
          end;
        end
        else
        begin
          checkCanonicalStatus(path, op, cs, FValueSet);
          ver := cs.version(nil);
          contentMode := cs.contentMode;
          ctxt := cs.locate(code, nil, msg);
          if (ctxt = nil) then
          begin
            msg := '';
            unkCodes.add(cs.systemUri(nil)+'|'+cs.version(nil)+'#'+code);
            if cs.contentMode <> cscmComplete then
            begin
              result := bTrue; // we can't say it isn't valid. Need a third status?
              cause := itCodeInvalid;
              FLog := 'Not found in Incomplete Code System';
              msg := FI18n.translate('UNKNOWN_CODE_IN_FRAGMENT', FParams.languages, [code, cs.systemUri(nil), cs.version(nil)]);
              messages.add(msg);
              op.addIssue(isWarning, itCodeInvalid, addToPath(path, 'code'), msg, oicInvalidCode);
            end
            else
            begin
              result := bFalse;
              cause := itCodeInvalid;
              FLog := 'Unknown code';
              msg := FI18n.translate('Unknown_Code_in_Version', FParams.languages, [code, cs.systemUri(nil), cs.version(nil)]);
              messages.add(msg);
              op.addIssue(isError, itCodeInvalid, addToPath(path, 'code'), msg, oicInvalidCode);
            end;
          end
          else
          begin
            try
              if vcc <> nil then
                vcc.addCoding(cs.systemUri(ctxt), cs.version(ctxt), cs.code(ctxt), cs.display(ctxt, FParams.languages));
              cause := itNull;
              if not (abstractOk or not cs.IsAbstract(ctxt)) then
              begin
                result := bFalse;
                FLog := 'Abstract code when not allowed';
                cause := itBusinessRule;
                msg := FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [system, code]);
                messages.add(msg);
                op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), msg, oicCodeRule);
              end
              else if ((FParams <> nil) and FParams.activeOnly and cs.isInactive(ctxt)) then
              begin
                result := bFalse;
                FLog := 'Inactive code when not allowed';
                cause := itBusinessRule;
                msg := FI18n.translate('STATUS_CODE_WARNING_CODE', FParams.languages, ['not active', code]);
                messages.add(msg);
                op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), msg, oicCodeRule);
              end
              else
              begin
                FLog := 'found OK';
                result := bTrue;
                inactive := cs.IsInactive(ctxt);
                if (inactive) then
                  vstatus := cs.getCodeStatus(ctxt);
              end;
              if (displays <> nil) then
                listDisplays(displays, cs, ctxt);
            finally
              ctxt.free;
            end;
          end;
        end;
      finally
        cs.free;
      end;
    end
    else if (false) then
    begin
      // anyhow, we ignore the value set (at least for now)
      cs := findCodeSystem(system, version, FParams, true);
      try
        if cs = nil then
        begin
          result := bUnknown;
          cause := itNotFound;
          FLog := 'Unknown code system';
          if (version <> '') then
          begin
            msg := FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.languages, [system, version, '['+listVersions(system)+']']);
            messages.add(msg);
            if (unknownSystems.IndexOf(system+'|'+version) = -1) then
            begin
              op.addIssue(isError, itNotFound, addToPath(path, 'system'), msg, oicNotFound);
              unknownSystems.add(system+'|'+version);
            end;
          end
          else
          begin
            msg := FI18n.translate('UNKNOWN_CODESYSTEM', FParams.languages, [system]);
            messages.add(msg);
            op.addIssue(isError, itNotFound, addToPath(path, 'system'), msg, oicNotFound);
            unknownSystems.add(system);
          end;
        end
        else
        begin
          checkCanonicalStatus(path, op, cs, FValueSet);
          ver := cs.version(nil);
          contentMode := cs.contentMode;
          ctxt := cs.locate(code);
          if (ctxt = nil) then
          begin
            unkCodes.add(system+'|'+version+'#'+code);
            if cs.contentMode <> cscmComplete then
            begin
              result := bTrue; // we can't say it isn't valid. Need a third status?
              cause := itCodeInvalid;
              FLog := 'Not found in Incomplete Code System';
              msg := FI18n.translate('UNKNOWN_CODE_IN_FRAGMENT', FParams.languages, [code, system, version]);
              messages.add(msg);
              op.addIssue(isWarning, itCodeInvalid, addToPath(path, 'code'), msg, oicInvalidCode);
            end
            else
            begin
              result := bFalse;
              cause := itCodeInvalid;
              FLog := 'Unknown code';
              msg := FI18n.translate('Unknown_Code_in_Version', FParams.languages, [code, system, version]);
              messages.add(msg);
              op.addIssue(isWarning, itCodeInvalid, addToPath(path, 'code'), msg, oicInvalidCode);
            end;
          end
          else
          begin
            try
              cause := itNull;
              if not (abstractOk or not cs.IsAbstract(ctxt)) then
              begin
                result := bFalse;
                FLog := 'Abstract code when not allowed';
                cause := itBusinessRule;
                msg := FI18n.translate('STATUS_CODE_WARNING_CODE', FParams.languages, ['not active', code]);
                messages.add(msg);
                op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), msg, oicCodeRule);
              end
              else if ((FParams <> nil) and FParams.activeOnly and cs.isInactive(ctxt)) then
              begin
                result := bFalse;
                FLog := 'Inactive code when not allowed';
                cause := itBusinessRule;
                msg := FI18n.translate('STATUS_CODE_WARNING_CODE', FParams.languages, ['not active', code]);
                messages.add(msg);
                op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), msg, oicCodeRule);
              end
              else
              begin
                FLog := 'found';
                result := bTrue;
              end;
              listDisplays(displays, cs, ctxt);
            finally
              ctxt.free;
            end;
          end;
        end;
      finally
        cs.free;
      end;
    end
    else
    begin
      // todo: we can never get here?
      if (system = '') and inferSystem then
      begin
        system := determineSystem(code);
        if (system = '') then
        begin
          message := FI18n.translate('UNABLE_TO_INFER_CODESYSTEM', FParams.languages, [code, FValueSet.url]);
          messages.add(message);
          op.addIssue(isError, itNotFound, path, message, oicInferFailed);
          exit(bFalse);
        end
        else
          impliedSystem := system;
      end;

      ics := FValueSet.inlineCS; // r2
      if ics <> nil then
      begin
        try
          contentMode := cscmComplete;
          ver := FValueSet.version;
          if (system = ics.systemUri) or (system = SYSTEM_NOT_APPLICABLE) then
          begin
            ccl := ics.concepts;
            try
              ok := FindCode(nil, code, ccl, displays, isabstract);
              if ok and (abstractOk or not isabstract) then
                exit(bTrue)
              else
                exit(bFalse);
            finally
              ccl.free;
            end;
          end;
        finally
          ics.free;
        end;
      end;

      if (FRequiredSupplements.count > 0) then
        raise ETerminologyError.create('Required supplements not found: ['+FRequiredSupplements.commaText+']', itBusinessRule);

      if (FValueSet.checkCompose('ValueSetChecker.prepare', 'ValueSet.compose')) then
      begin
        result := bFalse;
        for s in FValueSet.imports do
        begin
          if result = bFalse then
          begin
            checker := TValueSetChecker(FOthers.matches[s]);
            if (checker = nil) then
              raise ETerminologyError.Create('No Match for '+s+' in '+FOthers.AsText, itUnknown);
            checkCanonicalStatus(path, op, checker.FValueSet, FValueSet);
            result := checker.check(path, system, version, code, abstractOk, inferSystem, displays, unknownSystems, message, ver, inactive, vstatus, cause, op, nil, params, contentMode, impliedSystem, unkCodes, messages);
          end;
        end;
        for cc in FValueSet.includes.forEnum do
        begin
          if cc.systemUri = '' then
            result := bTrue // why?
          else if (cc.systemUri = system) or (system = SYSTEM_NOT_APPLICABLE) then
          begin
            v := determineVersion(path, cc.systemUri, cc.version, version, op, message);
            if (v = '') then
              cs := TCodeSystemProvider(FOthers.matches[cc.systemUri]).link
            else
              cs := TCodeSystemProvider(FOthers.matches[cc.systemUri+'|'+v]).link;
            if (cs = nil) then
              cs := findCodeSystem(system, v, FParams, true);
            if (cs = nil) then
            begin
              if (not FParams.membershipOnly) then
              begin
                bAdd := true;
                if (v = '') then
                begin
                  message := FI18n.translate('UNKNOWN_CODESYSTEM', FParams.languages, [system]);
                  unknownSystems.add(system);
                end
                else
                begin
                  message := FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.languages, [system, v, '['+listVersions(system)+']']);
                  badd := unknownSystems.IndexOf(system+'|'+version) = -1;
                  if (bAdd) then
                    unknownSystems.add(system+'|'+v);
                end;
                messages.add(message);
                if (bAdd) then
                  op.addIssue(isError, itNotFound, addToPath(path, 'system'), message, oicNotFound);
                exit(bUnknown);
              end
              else
                exit(bFalse);
            end;
            try
              checkCanonicalStatus(path, op, cs, FValueSet);
              ver := cs.version(nil);
              checkSupplements(cs, cc);
              contentMode := cs.contentMode;

              if ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri(nil) = system)) and checkConceptSet(path, cs, cc, code, abstractOk, displays, FValueSet, message, inactive, vstatus, op, vcc) then
                result := bTrue
              else
                result := bFalse;
            finally
              cs.free;
            end;
          end
          else
            result := bFalse;
          for s in cc.valueSets do
          begin
            checker := TValueSetChecker(FOthers.matches[s]);
            if checker = nil then
              raise ETerminologyError.Create('No Match for '+s+' in '+FOthers.AsText, itUnknown);
            checkCanonicalStatus(path, op, checker.FValueSet, FValueSet);
            if (result = bTrue) then
              result := checker.check(path, system, version, code, abstractOk, inferSystem, displays, unknownSystems, message, ver, inactive, vstatus, cause, op, nil,  params, contentMode, impliedSystem, unkCodes, messages);
          end;
          if result = bTrue then
            break;
        end;
        if result = bTrue then
          for cc in FValueSet.excludes.forEnum do
          begin
            if cc.systemUri = '' then
              excluded := true
            else
            begin
              if (cc.version = '') then
                cs := TCodeSystemProvider(FOthers.matches[cc.systemUri])
              else
              begin
                cs := TCodeSystemProvider(FOthers.matches[cc.systemUri+'|'+cc.version]);
                if (cs = nil) then
                  cs := TCodeSystemProvider(FOthers.matches[cc.systemUri])
              end;
              if (cs = nil) then
                raise ETerminologyError.Create('No Match for '+cc.systemUri+'|'+cc.version+' in '+FOthers.AsText, itUnknown);
              checkCanonicalStatus(path, op, cs, FValueSet);
              checkSupplements(cs, cc);
              ver := cs.version(nil);
              contentMode := cs.contentMode;
              excluded := ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri(nil) = system)) and checkConceptSet(path, cs, cc, code, abstractOk, displays, FValueSet, message, inactive, vstatus, op, vcc);
            end;
            for s in cc.valueSets do
            begin
              checker := TValueSetChecker(FOthers.matches[s]);  
              if (cs = nil) then
                raise ETerminologyError.Create('No Match for '+cc.systemUri+'|'+cc.version+' in '+FOthers.AsText, itUnknown);
              checkCanonicalStatus(path, op, checker.FValueSet, FValueSet);
              excluded := excluded and (checker.check(path, system, version, code, abstractOk, inferSystem, displays, unknownSystems, message, ver, inactive, vstatus, cause, op, nil, params, contentMode, impliedSystem, unkCodes, messages) = bTrue);
            end;
            if excluded then
              exit(bFalse);
          end;
      end
      else if FValueSet.checkExpansion('ValueSetChecker.prepare', 'ValueSet.expansion') then
      begin
        ccc := FValueSet.findContains(system, version, code);
        try
          if (ccc = nil) then
            result := bFalse
          else
          begin
            if (ccc.version = '') and (version = '') then
              v := ''
            else if (ccc.version = '') then
              v := version
            else if (version = '') or (version = ccc.version) then
              v := ccc.version
            else
            begin
              message := 'The code system "'+ccc.systemUri+'" version "'+ccc.version+'" in the ValueSet expansion is different to the one in the value ("'+version+'")';
              messages.add(message);
              op.addIssue(isError, itNotFound, addToPath(path, 'version'), message, oicVSProcessing);
              exit(bFalse);
            end;
            if (v = '') then
              cs := TCodeSystemProvider(FOthers.matches[ccc.systemUri]).link
            else
              cs := TCodeSystemProvider(FOthers.matches[ccc.systemUri+'|'+v]).link;
            if (cs = nil) then
              cs := findCodeSystem(system, v, FParams, true);
            if (cs = nil) then
            begin
              if (not FParams.membershipOnly) then
              begin
                bAdd := true;
                if (v = '') then
                begin
                  message := FI18n.translate('UNKNOWN_CODESYSTEM', FParams.languages, [system]) ;
                  unknownSystems.add(system);
                end
                else
                begin
                  badd := unknownSystems.IndexOf(system+'|'+version) = -1;
                  if (bAdd) then
                  begin
                    message := FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.languages, [system, v, '['+listVersions(system)+']']);
                    unknownSystems.add(system+'|'+v);
                  end;
                end;
                messages.add(message);
                if bAdd then
                  op.addIssue(isError, itNotFound, addToPath(path, 'system'), message, oicNotFound);
                exit(bUnknown);
              end
              else
                exit(bfalse);
            end;
            try
              checkCanonicalStatus(path, op, cs, FValueSet);
              ver := cs.version(nil);
              contentMode := cs.contentMode;
              if ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri(nil) = system)) and checkExpansion(path, cs, ccc, code, abstractOk, displays, FValueSet, message, inactive, vstatus, op) then
                result := bTrue
              else
                result := bFalse;
            finally
              cs.free;
            end;
          end;
        finally
          ccc.free;
        end;
      end
      else
        result := bFalse;
    end;
  finally
    ts.free;
  end;
end;


function TValueSetChecker.check(issuePath : String; coding: TFhirCodingW; abstractOk, inferSystem : boolean) : TFhirParametersW;
var
  list : TConceptDesignations;
  message, ver, pd, impliedSystem, path, us, baseMsg : String;
  cause : TFhirIssueType;
  op : TFhirOperationOutcomeW;
  contentMode : TFhirCodeSystemContentMode;
  dc : integer;
  ok : TTrueFalseUnknown;
  unknownSystems, unkCodes, messages : TStringList;
  diff : TDisplayDifference;
  inactive : boolean;
  vstatus : String;
begin
  inactive := false;
  path := issuePath;
  unknownSystems := TStringList.create;
  unkCodes := TStringList.create;
  messages := TStringList.create;
  result := FFactory.makeParameters;
  try
    unknownSystems.sorted := true;
    unknownSystems.duplicates := dupIgnore;
    op := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
    try
      checkCanonicalStatus(path, op, FValueSet, FValueSet);
      list := TConceptDesignations.Create(FFactory.link, FLanguages.link);
      try
        ok := check(path, coding.systemUri, coding.version, coding.code, abstractOk, inferSystem, list, unknownSystems, message, ver, inactive, vstatus, cause, op, nil, result, contentMode, impliedSystem, unkCodes, messages);
        if ok = bTrue then
        begin
          result.AddParamBool('result', true);
          if ((cause = itNotFound) and (contentMode <> cscmComplete)) or (contentMode = cscmExample) then
             result.AddParamStr('message', 'The system "'+coding.systemUri+' was found but did not contain enough information to properly validate the code (mode = '+CODES_TFhirCodeSystemContentMode[contentMode]+')');
          if (coding.display <> '') and (not list.hasDisplay(FParams.languages, coding.display, dcsCaseInsensitive, diff)) then
          begin
             baseMsg := 'Display_Name_for__should_be_one_of__instead_of';
             dc := list.displayCount(FParams.languages, true);
             if dc > 0 then
             begin
               if (diff = ddNormalised) then
                 baseMsg := 'Display_Name_WS_for__should_be_one_of__instead_of';
               if dc = 1 then
                 result.AddParamStr('message', FI18n.translate(baseMsg+'_one', FParams.languages,
                  ['', coding.systemUri, coding.code, list.present(FParams.languages, true), coding.display, FParams.langSummary]))
               else
                 result.AddParamStr('message', FI18n.translate(baseMsg+'_other', FParams.languages, [inttostr(dc), coding.systemUri, coding.code, list.present(FParams.languages, true), coding.display, FParams.langSummary]));
             end;
          end;
          pd := list.preferredDisplay(FParams.languages);
          if (pd <> '') then
            result.AddParamStr('display', pd);
          result.addParamUri('system', coding.systemUri);
          if (ver <> '') then
            result.addParamStr('version', ver);
          if cause <> itNull then
            result.AddParamCode('cause', CODES_TFhirIssueType[cause]);
          if inactive then
          begin
            result.AddParamBool('inactive', inactive);
            if (vstatus <> '') and (vstatus <> 'inactive') then
              result.addParamStr('status', vstatus);
          end;
        end
        else if (ok = bUnknown) then
        begin
          result.AddParamBool('result', false);
          result.AddParamStr('message', 'The CodeSystem "'+coding.systemUri+'" is unknown, so the code "'+coding.code+'" is not known to be in the '+FValueSet.name);
          //result.AddParamCode('cause', CODES_TFhirIssueType[itNotFound]);
          for us in unknownSystems do
            result.addParamCanonical('x-caused-by-unknown-system', us);
        end
        else
        begin
          result.AddParamBool('result', false);
          if (ver <> '') then
            result.addParamStr('version', ver);
          result.AddParamStr('message', 'The system/code "'+coding.systemUri+'"/"'+coding.code+'" is not in the value set '+FValueSet.name);
          if (message <> '') then
            result.AddParamStr('message', message);
          if cause <> itNull then
            result.AddParamCode('cause', CODES_TFhirIssueType[cause]);
        end;
      finally
        list.free;
      end;
      if (op.hasIssues) then
        result.addParam('issues').resource := op.Resource.link;
    finally
      op.free;
    end;
    result.Link;
  finally
    result.free;
    unknownSystems.free;
    unkCodes.free;
    messages.free;
  end;
end;

function TValueSetChecker.valueSetDependsOnCodeSystem(url, version : String) : boolean;
var
  inc : TFHIRValueSetComposeIncludeW;
begin
  for inc in FValueSet.includes.forEnum do
  begin
    if (inc.systemUri = url) and ((version = '') or (version = inc.version) or (inc.version = '')) then
      exit(true)
  end;

  result := false;
end;

function hasMessage(params : TFhirParametersW; msg : String) : boolean;
var
  p : TFhirParametersParameterW;
begin
  result := false;
  for p in params.parameterList do
    if (p.name = 'message') and (p.value.primitiveValue = msg) then
      exit(true);
end;

function toText(st : TStringList; sep : String) : String;
var
  i : integer;
begin
  if (st = nil) or (st.count = 0) then
    result := ''
  else
  begin
    result := st[0];
    for i := 1 to st.count - 1 do
      result := result + sep + st[i];
  end;
end;

function TValueSetChecker.check(issuePath : String; code: TFhirCodeableConceptW; abstractOk, inferSystem : boolean; mode : TValidationCheckMode) : TFhirParametersW;
  function Summary(code: TFhirCodeableConceptW) : String;
  begin
    if (code.codingCount = 1) then
      result := 'The code provided ('+code.summary+') is not '
    else
      result := 'None of the codes provided ('+code.summary+') are ';
  end;
var
  list : TConceptDesignations;
  ok, v : TTrueFalseUnknown;
  first : boolean;
  contentMode : TFhirCodeSystemContentMode;
  cc, codelist, message, ver, pd, ws, impliedSystem, path, m, tsys, tcode, tver,vs, tdisp: String;
  prov, prov2 : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  c : TFhirCodingW;
  cause : TFhirIssueType;
  op : TFhirOperationOutcomeW;
  log : String;
  tl : TIETFLang;
  psys, pver, pdisp, pcode, us, baseMsg, p : String;
  dc, i : integer;
  a : TStringArray;
  unknownSystems : TStringList;
  vcc : TFHIRCodeableConceptW;
  severity : TIssueSeverity;
  diff : TDisplayDifference;    
  inactive, bAdd : boolean;
  vstatus : String;
  mt, ts : TStringList;
  vss : TFHIRValueSetW;
  procedure msg(s : String; clear : boolean = false);
  begin
    if (s = '') then
      exit();
    if (clear) then
      mt.clear;
    if mt.indexOf(s) = -1 then
      mt.add(s);
  end;
begin
  inactive := false;
  cause := itNull;
  if FValueSet = nil then
    raise ETerminologyError.create('Error: cannot validate a CodeableConcept without a nominated valueset', itInvalid);
  mt := TStringList.create;
  ts := TStringList.create;
  try
    tsys := '';
    tcode := '';
    tver := '';
    tdisp := '';
    vcc := FFactory.wrapCodeableConcept(FFactory.makeCodeableConcept);
    vcc.text := code.text;
    unknownSystems := TStringList.create;
    result := FFactory.makeParameters;
    try
      unknownSystems.sorted := true;
      unknownSystems.duplicates := dupIgnore;
      op := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
      try
        checkCanonicalStatus(issuePath, op, FValueSet, FValueSet);
        list := TConceptDesignations.Create(FFactory.link, FLanguages.link);
        try
          ok := bFalse;
          codelist := '';
          mt.clear;
          i := 0;
          for c in code.codings.forEnum do
          begin
            if (issuePath = 'CodeableConcept') then
              path := addToPath(issuePath, 'coding['+inttostr(i)+']')
            else
              path := issuePath;
            list.clear;
            v := check(path, c.systemUri, c.version, c.code, abstractOk, inferSystem, list, unknownSystems, message, ver, inactive, vstatus, cause, op, vcc, result, contentMode, impliedSystem, ts, mt);
            if (v <> bTrue) and (message <> '') then
              msg(message);
            if (v = bFalse) then
              cause := itCodeInvalid;
            if (impliedSystem <> '') then
              ws := impliedSystem
            else
              ws := c.systemUri;
            if (tcode = '') or (v = bTrue) then
            begin
              tsys := c.systemUri;
              tcode := c.code;
              tver := c.version;
              tdisp := c.display;
            end;
            if (c.version = '') then
              cc := ws+'#'+c.code
            else
              cc := ws+'|'+c.version+'#'+c.code;
            if (c.display <> '') then
              cc := cc + ' ('''+c.display+''')';
            CommaAdd(codelist, ''''+cc+'''');

            if (v = bFalse) and not FAllValueSet and (mode = vcmCodeableConcept) then
            begin
              m := FI18n.translate('None_of_the_provided_codes_are_in_the_value_set_one', FParams.languages, ['', FValueSet.vurl, ''''+cc+'''']);
              msg(m);
              p := issuePath + '.coding['+inttostr(i)+'].code';
              op.addIssue(isInformation, itCodeInvalid, p, m, oicThisNotInVS);
              if cause = itNull then
                cause := itUnknown;
            end;

            if (ok <> bTrue) and (v <> bFalse) then
              ok := v;
            message := '';

            if (v = bTrue) then
            begin
              if ((cause = itNotFound) and (contentMode <> cscmComplete)) or (contentMode = cscmExample) then
              begin
                m := 'The system '+c.systemUri+' was found but did not contain enough information to properly validate the code "'+c.code+'" ("'+c.display+'") (mode = '+CODES_TFhirCodeSystemContentMode[contentMode]+')';
                msg(m);
                op.addIssue(isWarning, itNotFound, path, m, oicVSProcessing);
              end
              else
              if (c.display <> '') and (not list.hasDisplay(FParams.languages, c.display, dcsCaseInsensitive, diff)) then
              begin
                if (diff = ddNormalised) then
                  baseMsg := 'Display_Name_WS_for__should_be_one_of__instead_of'
                else
                  baseMsg := 'Display_Name_for__should_be_one_of__instead_of';
                dc := list.displayCount(FParams.languages, true);
                severity := dispWarning;
                if dc = 0 then
                begin
                  severity := isWarning;
                  m := FI18n.translate(baseMsg+'_one', FParams.languages,
                    ['', c.systemUri, c.code, list.present(FParams.languages, true), c.display, FParams.langSummary])
                end
                else if dc = 1 then
                  m := FI18n.translate(baseMsg+'_one', FParams.languages,
                    ['', c.systemUri, c.code, list.present(FParams.languages, true), c.display, FParams.langSummary])
                else
                  m := FI18n.translate(baseMsg+'_other', FParams.languages,
                    [inttostr(dc), c.systemUri, c.code, list.present(FParams.languages, true), c.display, FParams.langSummary]);
                msg(m);
                op.addIssue(severity, itInvalid, addToPath(path, 'display'), m, oicDisplay);
              end;
              psys := c.systemUri;
              pcode := c.code;
              if (ver <> '') then
                pver := ver;
              pd := list.preferredDisplay(FParams.languages);
              if pd <> '' then
                pdisp := pd;
              if (pdisp = '') then
                pdisp := list.preferredDisplay;
            end
            else if (not FParams.membershipOnly and (ws <> '')) then
            begin
              if not isAbsoluteUrl(ws) then
              begin   
                m := FI18n.translate('Terminology_TX_System_Relative', FParams.languages, []);
                if mode = vcmCoding then
                  p := issuePath + '.system'
                else if mode = vcmCodeableConcept then
                  p := issuePath + '.coding['+inttostr(i)+'].system'
                else
                  p := issuePath;
                op.addIssue(isError, itInvalid, p, m, oicInvalidData);
              end;
              prov := findCodeSystem(ws, c.version, FParams, true);
              try
               if (prov = nil) then
               begin
                 vss := findValueSet(ws, '');
                 if (vss <> nil) then
                 begin
                   vss.free;
                   m := FI18n.translate('Terminology_TX_System_ValueSet2', FParams.languages, [ws]);
                   msg(m);
                   op.addIssue(isError, itInvalid, addToPath(path, 'system'), m, oicInvalidData);
                   cause := itNotFound;
                 end
                 else
                 begin
                   prov2 := findCodeSystem(ws, '', FParams, true);
                   try
                     bAdd := true;
                     if (prov2 = nil) then
                     begin
                       m := FI18n.translate('UNKNOWN_CODESYSTEM', FParams.languages, [ws]);
                       //if (valueSetDependsOnCodeSystem(ws, '')) then
                         unknownSystems.add(ws);
                     end
                     else
                     begin
                       m := FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.languages, [ws, c.version, '['+listVersions(c.systemUri)+']']);
                       badd := unknownSystems.IndexOf(ws+'|'+c.version) = -1;
                       if (bAdd) then
                         unknownSystems.add(ws+'|'+c.version);
                     end;
                     if (bAdd) then
                       op.addIssue(isError, itNotFound, addToPath(path, 'system'), m, oicNotFound);
                     if (valueSetDependsOnCodeSystem(ws, c.version)) then
                     begin
                       m := 'Unable to check whether the code is in the value set '+FValueSet.vurl+' because the code system '+ws+'|'+c.version+' was not found';
                       msg(m);
                       op.addIssue(isWarning, itNotFound, issuepath, m, oicVSProcessing);
                     end
                     else
                       msg(m);
                   finally
                     prov2.free;
                   end;
                   cause := itNotFound;
                 end;
               end
               else
               begin
                 checkCanonicalStatus(path, op, prov, FValueSet);
                 ctxt := prov.locate(c.code, FAllAltCodes, message);
                 try
                   if ctxt = nil then
                   begin
                     if (message <> '') then
                     begin
                       // msg(message); we just add this as an issue, but don't put it in the base message
                       if mode <> vcmCode then
                         p := path + '.code';
                       op.addIssue(isInformation, cause, p, message, oicInvalidCode);
                       message := '';
                     end;
                     vcc.removeCoding(prov.systemUri(nil), prov.version(nil), c.code);
                     vs := ws+'|'+prov.version(nil)+'#'+c.code;
                     if ts.indexOf(vs) = -1 then
                     begin
                       ts.add(vs);
                       m := FI18N.translate('Unknown_Code_in_Version', FParams.languages, [c.code, ws, prov.version(nil)]);
                       cause := itCodeInvalid;
                       msg(m);
                       op.addIssue(isError, itCodeInvalid, addToPath(path, 'code'), m, oicInvalidCode);
                     end;
                   end
                   else
                   begin
                     listDisplays(list, prov, ctxt);
                     pd := list.preferredDisplay(FParams.languages);
                     if pd <> '' then
                       pdisp := pd;
                     if (pdisp = '') then
                       pdisp := list.preferredDisplay;
                     severity := dispWarning();
                     if (c.display <> '') and (not list.hasDisplay(FParams.languages, c.display, dcsCaseInsensitive, diff)) then
                     begin
                       if (diff = ddNormalised) then
                         baseMsg := 'Display_Name_WS_for__should_be_one_of__instead_of'
                       else
                         baseMsg := 'Display_Name_for__should_be_one_of__instead_of';

                       dc := list.displayCount(FParams.languages, true);
                       if dc = 0 then
                       begin
                         severity := isWarning;
                         m := FI18n.translate(baseMsg+'_other', FParams.languages,
                           ['', prov.systemUri(ctxt), c.code, list.present(FParams.languages, true), c.display, FParams.langSummary])
                       end
                       else if dc = 1 then
                         m := FI18n.translate(baseMsg+'_one', FParams.languages,
                           ['', prov.systemUri(ctxt), c.code, list.present(FParams.languages, true), c.display, FParams.langSummary])
                       else
                         m := FI18n.translate(baseMsg+'_other', FParams.languages,
                          [inttostr(dc), prov.systemUri(ctxt), c.code, list.present(FParams.languages, true), c.display, FParams.langSummary]);
                       msg(m);
                       op.addIssue(severity, itInvalid, addToPath(path, 'display'), m, oicDisplay);
                     end;
                     if (prov.version(nil) <> '') then
                       result.addParamStr('version', prov.version(nil));
                   end;
                 finally
                   ctxt.free;
                 end;
               end;
              finally
                prov.free;
              end;
            end;
            inc(i);
          end;
          if (ok = bFalse) and not FAllValueSet then
          begin
            if mode = vcmCodeableConcept then
              m := FI18n.translate('TX_GENERAL_CC_ERROR_MESSAGE', FParams.languages, [FValueSet.vurl])
            else // true... if code.codingCount = 1 then
              m := FI18n.translate('None_of_the_provided_codes_are_in_the_value_set_one', FParams.languages, ['', FValueSet.vurl, codelist]);
            //else
            //  m := FI18n.translate('None_of_the_provided_codes_are_in_the_value_set_other', FParams.languages, ['', FValueSet.vurl, codelist]);
            msg(m);

            if mode = vcmCodeableConcept then
              p := ''
            else if (issuePath <> 'CodeableConcept') then
              p := issuePath + '.code'
            else if code.codingCount = 1 then
              p := issuePath + '.coding[0].code'
            else
              p := issuePath;

            op.addIssue(isError, itCodeInvalid, p, m, oicNotInVS);
            if cause = itNull then
              cause := itUnknown;
          end;
        finally
          list.free;
        end;

        result.AddParamBool('result', (ok = bTrue) and not op.hasErrors);
        if (psys <> '') then
          result.addParamUri('system', psys)
        else if (ok = bTrue) and (impliedSystem <> '') then
          result.addParamUri('system', impliedSystem)
        else if (tsys <> '') and (mode <> vcmCodeableConcept) then
          result.addParamUri('system', tsys);

        if (ok <> bTrue) and (unknownSystems.count > 0) then
          for us in unknownSystems do
            result.addParamCanonical('x-caused-by-unknown-system', us);
        if (pcode <>'') then
          result.addParamCode('code', pcode)
        else if (tcode <> '') and (mode <> vcmCodeableConcept) then
          result.addParamCode('code', tcode);
        if (pver <> '') then
          result.addParamStr('version', pver)
        else if (tver <> '') and (mode <> vcmCodeableConcept) then
          result.addParamStr('version', tver);

        if pdisp <> '' then
          result.AddParamStr('display', pdisp);
        //else if tdisp <> '' then
        //  result.AddParamStr('display', tdisp);

        if inactive then
        begin
          result.addParamBool('inactive',inactive);
          if (vstatus <> '') and (vstatus <> 'inactive') then
            result.addParamStr('status', vstatus);
        end;
        if mt.count > 0 then
        begin
          mt.sort;
          result.AddParamStr('message', toText(mt, '; '));
        end;
        if (mode = vcmCodeableConcept) then
        begin
          result.addParam('codeableConcept', code.Element.link);
        end;
        if (op.hasIssues) then
          result.addParam('issues').resource := op.Resource.link;
      finally
        op.free;
      end;
      result.Link;
    finally
      result.free;
      vcc.free;
      unknownSystems.free;
    end;
  finally
    mt.free;
    ts.free;
  end;
end;

function TValueSetChecker.check(issuePath, system, version, code: String; inferSystem : boolean): TFhirParametersW;
var
  list : TConceptDesignations;
  message, ver, pd, impliedSystem, us : String;
  cause : TFhirIssueType;
  op : TFhirOperationOutcomeW;
  contentMode : TFhirCodeSystemContentMode;
  ok : TTrueFalseUnknown;
  unknownSystems, unkCodes, messages : TStringList;
  inactive : boolean;
  vstatus : String;
begin
  unknownSystems := TStringList.create;
  unkCodes := TStringList.create;
  messages := TStringList.create;
  try
    unknownSystems.sorted := true;
    unknownSystems.duplicates := dupIgnore;
    result := FFactory.makeParameters;
    try                             
      op := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
      try
        checkCanonicalStatus(issuePath, op, FValueSet, FValueSet);
        list := TConceptDesignations.Create(FFactory.link, FLanguages.link);
        try
          ok := check(issuePath, system, version, code, true, inferSystem, list, unknownSystems, message, ver, inactive, vstatus, cause, op, nil, result, contentMode, impliedSystem, unkCodes, messages);
          if ok = bTrue then
          begin
            result.AddParamBool('result', true);
            pd := list.preferredDisplay(FParams.languages);
            if pd <> ''then
              result.AddParamStr('display', pd);
            result.addParamUri('system', system);
            if ((cause = itNotFound) and (contentMode <> cscmComplete)) or (contentMode = cscmExample) then
               result.AddParamStr('message', 'The system "'+system+' was found but did not contain enough information to properly validate the code (mode = '+CODES_TFhirCodeSystemContentMode[contentMode]+')');
            if cause <> itNull then
              result.AddParamCode('cause', CODES_TFhirIssueType[cause]);
            if (inactive) then
            begin
              result.addParamBool('inactive', inactive);
              if (vstatus <> '') and (vstatus <> 'inactive') then
                result.addParamStr('status', vstatus);
            end;
          end
          else if (ok = bUnknown) then
          begin
            result.AddParamBool('result', false);
            result.AddParamStr('message', 'The system "'+system+'" is unknown so the /"'+code+'" cannot be confirmed to be in the value set '+FValueSet.name);
            op.addIssue(isError, cause, 'code', 'The system "'+system+'" is unknown so the /"'+code+'" cannot be confirmed to be in the value set '+FValueSet.name, oicNotFound);
            //result.AddParamCode('cause', CODES_TFhirIssueType[itNotFound]);
            for us in unknownSystems do
              result.addParamCanonical('x-caused-by-unknown-system', us);
          end
          else
          begin
            result.AddParamBool('result', false);
            result.AddParamStr('message', 'The system/code "'+system+'"/"'+code+'" is not in the value set '+FValueSet.name);
            op.addIssue(isError, cause, 'code', 'The system/code "'+system+'"/"'+code+'" is not in the value set '+FValueSet.name, oicNotInVS);
            if (message <> '') then
              result.AddParamStr('message', message);
            if cause <> itNull then
              result.AddParamCode('cause', CODES_TFhirIssueType[cause]);
          end;
        finally
          list.free;
        end;
        if (op.hasIssues) then
          result.addParam('issues').resource := op.Resource.link;
      finally
        op.free;
      end;
      result.Link;
    finally
      result.free;
    end;
  finally
    unknownSystems.free;
    unkCodes.free;
    messages.free;
  end;
end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    ctxt.free;
end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderFilterContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    ctxt.free;
end;

function TValueSetChecker.checkConceptSet(path : String; cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code: String; abstractOk : boolean; displays : TConceptDesignations; vs : TFHIRValueSetW; var message : String; var inactive : boolean; var vstatus : String; op : TFHIROperationOutcomeW; vcc : TFHIRCodeableConceptW): boolean;
var
  i : integer;
  fc : TFhirValueSetComposeIncludeFilterW;
  ctxt : TCodeSystemProviderFilterContext;
  loc :  TCodeSystemProviderContext;
  prep : TCodeSystemProviderFilterPreparationContext;
  f : TCodeSystemProviderFilterContext;
  filters : Array of TCodeSystemProviderFilterContext;
  msg, c : String;
  cc : TFhirValueSetComposeIncludeConceptW;
  cfl : TFslList<TFhirValueSetComposeIncludeFilterW>;
begin
  result := false;
  if (not cset.hasConcepts) and (not cset.hasFilters) then
  begin
    loc := cs.locate(code, FParams.altCodeRules, message);
    try
      result := false;
      if loc = nil then
      begin
        if (not FParams.membershipOnly) then
          op.addIssue(isError, itCodeInvalid, addToPath(path, 'code'), FI18n.translate('Unknown_Code_in_Version', FParams.languages, [code, cs.systemUri(nil), cs.version(nil)]), oicInvalidCode)
      end
      else if not (abstractOk or not cs.IsAbstract(loc)) then
      begin
        if (not FParams.membershipOnly) then
          op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri(nil), code]), oicCodeRule)
      end
      else if FValueSet.excludeInactives and cs.IsInactive(loc) then
      begin
        op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('STATUS_CODE_WARNING_CODE', FParams.languages, ['not active', code]), oicCodeRule);
        result := false;
        if (not FParams.membershipOnly) then
        begin
          inactive := true;
          if (inactive) then
            vstatus := cs.getCodeStatus(loc);
        end;
      end
      else if FParams.activeOnly and cs.IsInactive(loc) then
      begin
        result := false;
        inactive := true;
        vstatus := cs.getCodeStatus(loc);
        op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('STATUS_CODE_WARNING_CODE', FParams.languages, ['not active', code]), oicCodeRule);
      end
      else
      begin
        result := true;
        listDisplays(displays, cs, loc);
        inactive := cs.IsInactive(loc);


        if (inactive) then
          vstatus := cs.getCodeStatus(loc);

        if vcc <> nil then
          vcc.addCoding(cs.systemUri(loc), cs.version(loc), cs.code(loc), displays.preferredDisplay(FParams.languages));
        exit;
      end;
    finally
      loc.free;
    end;
  end;

  for cc in cset.concepts.forEnum do
  begin
    c := cc.code;
    if (code = c) then
    begin
      loc := cs.locate(code, FAllAltCodes);
      try
        if Loc <> nil then
        begin
          listDisplays(displays, cs, loc);
          listDisplays(displays, cc, vs);
          if not (abstractOk or not cs.IsAbstract(loc)) then
          begin
            if (not FParams.membershipOnly) then
              op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri(nil), code]), oicCodeRule)
          end
          else if FValueSet.excludeInactives and cs.IsInactive(loc) then
          begin
            result := false;
            if (not FParams.membershipOnly) then
            begin
              inactive := true;
              vstatus := cs.getCodeStatus(loc);
            end
          end
          else
          begin
            if vcc <> nil then
              vcc.addCoding(cs.systemUri(loc), cs.version(loc), cs.code(loc), displays.preferredDisplay(FParams.languages));
            result := true;
            exit;
          end;
        end;
      finally
        loc.free;
      end;
    end;
  end;

  if cset.hasFilters then
  begin
    cfl := cset.filters;
    try
      SetLength(filters, cfl.count);
      prep := cs.getPrepContext;
      try
        i := 0;
        for fc in cfl do
        begin
          // gg - why? if ('concept' = fc.property_) and (fc.Op = FilterOperatorIsA) then
          f := cs.filter(false, fc.prop, fc.Op, fc.value, prep);
          if f = nil then
            raise ETerminologyError.create('The filter "'+fc.prop +' '+ CODES_TFhirFilterOperator[fc.Op]+ ' '+fc.value+'" from the value set '+vs.vurl+' was not understood in the context of '+cs.systemUri(nil), itNotSupported);
          filters[i] := f;
          inc(i);
        end;
        if cs.prepare(prep) then // all are together, just query the first filter
        begin
          ctxt := filters[0];
          loc := cs.filterLocate(ctxt, code);
          try
            if Loc <> nil then
            begin
              listDisplays(displays, cs, loc);
              if not (abstractOk or not cs.IsAbstract(loc)) then
              begin
                if (not FParams.membershipOnly) then
                  op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri(nil), code]), oicCodeRule)
              end
              else if FValueSet.excludeInactives and cs.IsInactive(loc) then
              begin
                result := false;
                if (not FParams.membershipOnly) then
                begin
                  inactive := true;
                  vstatus := cs.getCodeStatus(loc);
                end
              end
              else
              begin  
                if vcc <> nil then
                  vcc.addCoding(cs.systemUri(loc), cs.version(loc), cs.code(loc), displays.preferredDisplay(FParams.languages));
                result := true;
                exit;
              end;
            end;
          finally
            loc.free;
          end;
        end
        else
        begin
          result := true;
          i := 0;
          for fc in cfl do
          begin
            if ('concept' = fc.prop) and (fc.Op in [foIsA, foDescendentOf]) then
            begin
              loc := cs.locateIsA(code, fc.value, fc.Op = foDescendentOf);
              try
                if Loc <> nil then
                begin
                  listDisplays(displays, cs, loc);
                  if not (abstractOk or not cs.IsAbstract(loc)) then
                  begin
                    if (not FParams.membershipOnly) then
                      op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri(nil), code]), oicCodeRule)
                  end
                  else
                  begin    
                    if vcc <> nil then
                      vcc.addCoding(cs.systemUri(loc), cs.version(loc), cs.code(loc), displays.preferredDisplay(FParams.languages));
                    result := true;
                    exit;
                  end;
                end
                else
                  result := false;
              finally
                loc.free;
              end;
            end
            else if ('concept' = fc.prop) and (fc.Op = foIsNotA) then
            begin
              loc := cs.locateIsA(code, fc.value);
              try
                result := (loc = nil);
                if (result) then
                begin
                  loc := cs.locate(code, nil, msg);
                  if Loc <> nil then
                  begin
                    listDisplays(displays, cs, loc);
                    if not (abstractOk or not cs.IsAbstract(loc)) then
                    begin
                      if (not FParams.membershipOnly) then
                        op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri(nil), code]), oicCodeRule)
                    end
                    else if FValueSet.excludeInactives and cs.IsInactive(loc) then
                    begin
                      result := false;
                      if (not FParams.membershipOnly) then
                      begin
                        inactive := true;
                        vstatus := cs.getCodeStatus(loc);
                      end;
                    end
                    else
                    begin   
                      if vcc <> nil then
                        vcc.addCoding(cs.systemUri(loc), cs.version(loc), cs.code(loc), displays.preferredDisplay(FParams.languages));
                      result := true;
                      exit;
                    end;
                  end;
                end;
              finally
                loc.free;
              end;
            end
            else
            begin
              ctxt := filters[i];
              result := false;
              loc := cs.filterLocate(ctxt, code, msg);
              try
                if (loc = nil) and (message = '') then
                  message := msg;
                if Loc <> nil then
                begin
                  listDisplays(displays, cs, loc);
                  if not (abstractOk or not cs.IsAbstract(loc)) then
                  begin
                    if (not FParams.membershipOnly) then
                      op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri(nil), code]), oicCodeRule)
                  end
                  else if FValueSet.excludeInactives and cs.IsInactive(loc) then
                  begin
                    result := false;
                    if (not FParams.membershipOnly) then
                    begin
                      inactive := true;
                      vstatus := cs.getCodeStatus(loc);
                    end;
                  end
                  else
                  begin       
                    if vcc <> nil then
                      vcc.addCoding(cs.systemUri(loc), cs.version(loc), cs.code(loc), displays.preferredDisplay(FParams.languages));
                    result := true;
                    exit;
                  end;
                end;
              finally
                loc.free;
              end;
            end;
            if not result then
              break;
            inc(i);
          end;
        end;
      finally
        for i := 0 to cfl.count - 1 do
          filters[i].free;
        prep.free;
      end;
    finally
      cfl.free;
    end;
  end;
end;

function TValueSetChecker.checkExpansion(path: String; cs: TCodeSystemProvider; cset: TFhirValueSetExpansionContainsW; code: String; abstractOk: boolean;
  displays: TConceptDesignations; vs: TFHIRValueSetW; var message: String; var inactive : boolean; var vstatus : String; op: TFHIROperationOutcomeW): boolean;
var
  loc :  TCodeSystemProviderContext;
begin
  result := false;
  loc := cs.locate(code, nil, message);
  try
    result := false;
    if loc = nil then
    begin
      if (not FParams.membershipOnly) then
        op.addIssue(isError, itCodeInvalid, addToPath(path, 'code'), FI18n.translate('Unknown_Code_in_Version', FParams.languages, [code, cs.systemUri(nil), cs.version(nil)]), oicInvalidCode)
    end
    else if not (abstractOk or not cs.IsAbstract(loc)) then
    begin
      if (not FParams.membershipOnly) then
        op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri(nil), code]), oicCodeRule)
    end
    else
    begin
      result := true;
      inactive := cs.IsInactive(loc);
      if (inactive) then
        vstatus := cs.getCodeStatus(loc);
      listDisplays(displays, cs, loc);
      exit;
    end;
  finally
    loc.free;
  end;
end;

function TValueSetChecker.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FOthers.sizeInBytes(magic));
  inc(result, FValueSet.sizeInBytes(magic));
  inc(result, (FId.length * sizeof(char)) + 12);
end;

function TValueSetChecker.isValidating: boolean;
begin
  result := true;
end;

{ TFHIRValueSetExpander }


function TFHIRValueSetExpander.expand(source: TFHIRValueSetW;
  params: TFHIRExpansionParams; textFilter: String; dependencies: TStringList;
  limit, count, offset: integer): TFHIRValueSetW;
var
  i, t, o : integer;
  c : TFhirValueSetExpansionContainsW;
  //e : TFhirExtension;
  filter : TSearchFilterText;
  notClosed : boolean;
  div_, table : TFhirXHtmlNode;
  tr : TFhirXHtmlNode;
  exp: TFHIRValueSetExpansionW;
  ics : TFHIRValueSetCodeSystemW;
  cl : TFhirCodeSystemConceptListW;
  cs2 : TFhirCodeSystemW;
  lang : TIETFLang;
  list : TFslList<TFhirValueSetExpansionContainsW>;
  ext : TFHIRExtensionW;
  noTotal : boolean;
  s : String;
begin
  noTotal := false;
  source.checkNoImplicitRules('ValueSetExpander.Expand', 'ValueSet');
  FFactory.checkNoModifiers(source, 'ValueSetExpander.Expand', 'ValueSet');

  FParams := params.Link;
  seeValueSet(source);
  FValueSet := source.link;

  result := Ffactory.wrapValueSet(source.Resource.Clone as TFHIRResourceV);
  result.id := '';
  table := nil;
  div_ := nil;
  if not FParams.includeDefinition then
    result.clearDefinition;

  FRequiredSupplements.clear;
  for ext in source.getExtensionsW(EXT_VSSUPPLEMENT).forEnum do
    FRequiredSupplements.add(ext.valueAsString);

  if (result.hasExpansion) then
    exit; // just return the expansion

  if FParams.generateNarrative then
  begin
    div_ := FFactory.resetXhtml(result.Resource);
    table := div_.AddTag('table').setAttribute('class', 'grid');
  end
  else
  begin
    FFactory.clearXhtml(result.Resource);
  end;

  if (source.url <> '') then
    dependencies.Add(source.url);

//  if FParams.hasLanguages then
//    result.language := FParams.Languages.prefLang;

  filter := TSearchFilterText.create(textFilter);

  FMap := TFslMap<TFhirValueSetExpansionContainsW>.create('VS.Expander.map');
  FMap.defaultValue := nil;
  FRootList := TFslList<TFhirValueSetExpansionContainsW>.create;
  FFullList := TFslList<TFhirValueSetExpansionContainsW>.create;
  FCanBeHierarchy := not FParams.excludeNested;

  try
    if limit > 0 then
      FLimitCount := limit
    else if not filter.null then
      FLimitCount := UPPER_LIMIT_TEXT
    else
      FLimitCount := UPPER_LIMIT_NO_TEXT;

    FCount := count;
    FOffset := offset;
    if (FOffset > 0) or (FCount > 0) then
      FCanBeHierarchy := false;

    exp := result.forceExpansion;
    //if source.id <> '' then
    //  exp.addParamUri('expansion-source', 'ValueSet/'+source.id)
    //else if source.url <> '' then
    //  exp.addParamUri('expansion-source', source.url);

    if not filter.null then
      exp.addParamStr('filter', filter.filter);

    if FParams.hasLimitedExpansion then
      exp.addParamBool('limitedExpansion', FParams.limitedExpansion);
    if (FParams.hasLanguages) then
      exp.addParamCode('displayLanguage', FParams.languages.asString(false));
    if (FParams.hasDesignations) then
      for s in FParams.Designations do
        exp.addParamStr('designation', s);
    if FParams.hasExcludeNested then
      exp.addParamBool('excludeNested', FParams.excludeNested);
    if FParams.hasActiveOnly then
      exp.addParamBool('activeOnly', FParams.activeOnly);
    if FParams.hasIncludeDesignations then
      exp.addParamBool('includeDesignations', FParams.includeDesignations);
    if FParams.hasExcludeNotForUI then
      exp.addParamBool('excludeNotForUI', FParams.excludeNotForUI);
    if FParams.hasExcludePostCoordinated then
      exp.addParamBool('excludePostCoordinated', FParams.excludePostCoordinated);
    checkCanonicalStatus(exp, source, source);
    if FOffset > -1 then
    begin
      exp.addParamInt('offset', FOffset);
      exp.offset := FOffset;
    end;
    if FCount > -1 then
      exp.addParamInt('count', FCount);

    DeadCheck('expand');
    try
;      ics := source.inlineCS;
      try
        if (ics <> nil) then
        begin
          FFactory.checkNoModifiers(ics, 'ValueSetExpander.Expand', 'code system');
          cl := ics.concepts;
          try
            cs2 := FFactory.wrapCodeSystem(source.Resource.link);
            try
              handleDefine(cs2, ics, cl, filter, exp, nil, false, source.url);
            finally
              cs2.free;
            end;
          finally
            cl.free;
          end;
        end;
      finally
        ics.free;
      end;
      notClosed := false;
      if (source.checkCompose('ValueSetExpander.Expand', 'compose')) then
        handleCompose(source, filter, dependencies, exp, notClosed);

      if (FRequiredSupplements.count > 0) then
        raise ETerminologyError.create('Required supplements not found: ['+FRequiredSupplements.commaText+']', itBusinessRule);
    except
      on e : EFinished do
      begin
        // nothing - we're just trapping this
        noTotal := true;
      end;
      on e : ETooCostly do
      begin
        if FParams.limitedExpansion then
        begin
          exp.addExtensionV('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', FFactory.makeBoolean(true));
          if (table <> nil) then
            div_.addTag('p').setAttribute('style', 'color: Maroon').addText(e.message);
        end
        else
        begin
          recordStack(e);
          raise;
        end;
      end;
      on e : Exception do
      begin
        recordStack(e);
        raise;
      end;
    end;

    if notClosed then
    begin
      exp.addExtensionV('http://hl7.org/fhir/StructureDefinition/valueset-unclosed', FFactory.makeBoolean(true));
      list := FFullList;
      for c in FFullList do
        c.clearContains();
      if (table <> nil) then
        div_.addTag('p').setAttribute('style', 'color: Navy').addText('Because of the way that this value set is defined, not all the possible codes can be listed in advance');
    end
    else
    begin
      if not noTotal then
        exp.Total := FFullList.count;
      if (FCanBeHierarchy) then
        list := FRootList
      else
      begin
        list := FFullList;
        for c in FFullList do
          c.clearContains();
      end;
    end;

    t := 0;
    o := 0;
    for i := 0 to list.count - 1 do
    begin
      c := list[i];
      if FMap.containsKey(key(c)) then
      begin
        inc(o);
        if FCanBeHierarchy or (o > offset) and ((count <= 0) or (t < count)) then
        begin
          inc(t);
          exp.addContains(c);
          if (table <> nil) then
          begin
            tr := table.AddChild('tr');
            tr.AddChild('td').AddText(c.systemUri);
            tr.AddChild('td').AddText(c.code);
            tr.AddChild('td').AddText(c.display);
          end;
        end;
      end;
    end;

    result.link;
  finally
    FMap.free;
    FRootList.free;
    FFullList.free;
    result.free;
    filter.free;
  end;
end;

function TFHIRValueSetExpander.key(system, code : String): string;
begin
  result := '{'+system+'}'+code;
end;

function TFHIRValueSetExpander.key(c: TFhirValueSetExpansionContainsW): string;
begin
  result := key(c.systemUri, c.Code);
end;

procedure TFHIRValueSetExpander.checkCanonicalStatus(expansion: TFhirValueSetExpansionW; vurl : String; status : TPublicationStatus; standardsStatus : String; experimental : boolean; source : TFHIRValueSetW);
begin
  if standardsStatus = 'deprecated' then
    expansion.addParamUri('warning-deprecated', vurl)
  else if standardsStatus = 'withdrawn' then
    expansion.addParamUri('warning-withdrawn', vurl)
  else if status = psRetired then
    expansion.addParamUri('warning-retired', vurl)
  else if experimental and not source.experimental then
    expansion.addParamUri('warning-experimental', vurl)
  else if ((status = psDraft) or (standardsStatus = 'draft')) and
      not ((source.status = psDraft) or (source.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status') = 'draft')) then
    expansion.addParamUri('warning-draft', vurl)
end;


procedure TFHIRValueSetExpander.checkCanonicalStatus(expansion: TFhirValueSetExpansionW; resource: TFHIRMetadataResourceW; source : TFHIRValueSetW);
begin                              
  checkCanonicalStatus(expansion, resource.vurl, resource.status, resource.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status'), resource.experimental, source);
end;

procedure TFHIRValueSetExpander.checkCanonicalStatus(expansion: TFhirValueSetExpansionW; cs: TCodeSystemProvider; source : TFHIRValueSetW);
var
  status: TPublicationStatus;
  standardsStatus: String;
  experimental : boolean;
begin
  cs.getStatus(status, standardsStatus, experimental);
  if (cs.version(nil) <> '') then
    checkCanonicalStatus(expansion, cs.systemUri(nil)+'|'+cs.version(nil), status, standardsStatus, experimental, source)
  else
    checkCanonicalStatus(expansion, cs.systemUri(nil), status, standardsStatus, experimental, source);
end;

function TFHIRValueSetExpander.makeFilterForValueSet(cs: TCodeSystemProvider; vs: TFHIRValueSetW): TCodeSystemProviderFilterContext;
var
  inc : TFhirValueSetComposeIncludeW;
  cc : TFhirValueSetComposeIncludeConceptW;
  cf : TFhirValueSetComposeIncludeFilterW;
  message : String;
begin
  result := nil;
  for inc in vs.excludes.forEnum do // no short cuts if there's excludes
    exit;
  for inc in vs.includes.forEnum do
  begin
    if inc.systemUri = '' then
      exit; // no short cuts if there's further value set references
    if inc.systemUri = cs.systemUri(nil) then
    begin
      // ok we have a match. Check we can simplify it
      if inc.hasValueSets then
        exit;
      if inc.hasConcepts and inc.hasFilters then
        exit;
      if inc.filterCount > 1 then
        exit;
      if inc.hasFilters then
      begin
        for cf in inc.filters.forEnum do // will only cycle once
        begin
          exit(cs.filter(false, cf.prop, cf.op, cf.value, nil));
        end;
      end
      else
      begin
        result := TSpecialProviderFilterContextConcepts.create;
        for cc in inc.concepts.forEnum do
          TSpecialProviderFilterContextConcepts(result).add(cs.locate(cc.code, nil, message));
        exit;
      end;
    end;
  end;
  // if we get to here, there's nothing left
  result := TSpecialProviderFilterContextNothing.create;
end;

procedure TFHIRValueSetExpander.handleCompose(source: TFhirValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);
var
  vs : TFHIRValueSetW;
  s : String;
  c : TFhirValueSetComposeIncludeW;
begin
  for s in source.imports do
  begin
    vs := expandValueSet(s, '', filter.filter, dependencies, notClosed);
    try
      checkCanonicalStatus(expansion, vs, FValueSet);
      importValueSet(vs, expansion, nil, 0);
    finally
      vs.free;
    end;
  end;

  for c in source.includes.forEnum do
    checkSource(c, expansion, filter, source.url);
  for c in source.excludes.forEnum do
    checkSource(c, expansion, filter, source.url);

  for c in source.includes.forEnum do
    processCodes(false, c, source, filter, dependencies, expansion, source.excludeInactives, notClosed);
  for c in source.excludes.forEnum do
    processCodes(true, c, source, filter, dependencies, expansion, source.excludeInactives,notClosed);
end;

procedure TFHIRValueSetExpander.handleDefine(cs : TFhirCodeSystemW; source : TFhirValueSetCodeSystemW; defines : TFhirCodeSystemConceptListW; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; excludeInactive : boolean; srcURL : String);
var
  cm : TFhirCodeSystemConceptW;
  v : String;
begin
  deadCheck('handleDefine');
  if (defines.Count > 0) and (expansion <> nil) and (cs.version <> '') then
  begin
    if FFactory.version = fhirVersionRelease2 then
      v := source.systemUri+FHIR_VERSION_CANONICAL_SPLIT_2+cs.version
    else
      v := source.systemUri+FHIR_VERSION_CANONICAL_SPLIT_3p+cs.version;
    if not expansion.hasParam('used-codesystem', v) then
      expansion.addParamUri('used-codesystem', v);
    if not expansion.hasParam('version', v) then
      expansion.addParamUri('version', v);
  end;
  for cm in defines do
  begin
    FFactory.checkNoModifiers(cm, 'ValueSetExpander.handleDefine', 'concept');
    if filter.passes(cm.display) or filter.passes(cm.code) then
      addDefinedCode(cs, source.systemUri, cm, imports, nil, excludeInactive, srcUrl);
    handleDefine(cs, source, cm.conceptList, filter, nil, imports, excludeInactive, srcUrl);
  end;
end;

procedure TValueSetWorker.listDisplays(displays : TConceptDesignations; cs : TCodeSystemProvider; c: TCodeSystemProviderContext);
begin
  // list all known language displays
  cs.Designations(c, displays);
end;

procedure TValueSetWorker.listDisplays(displays : TConceptDesignations; c: TFhirCodeSystemConceptW); // todo: supplements
var
  ccd : TFhirCodeSystemConceptDesignationW;
begin
  // list all known provided displays
  for ccd in c.designations.forEnum do
    displays.addDesignation(ccd);
end;

procedure TValueSetWorker.listDisplays(displays : TConceptDesignations; c: TFhirValueSetComposeIncludeConceptW; vs : TFHIRValueSetW);
var
  cd : TFhirValueSetComposeIncludeConceptDesignationW;
  list : TFslList<TFHIRExtensionW>;
  i : integer;
begin
  if c.display <> '' then
  begin
    displays.Clear;
    displays.baseLang := FLanguages.parse(vs.language);
    displays.addDesignation(true, true, '', c.displayElement); {no .link}
  end;
  for cd in c.designations.forEnum do
    // see https://chat.fhir.org/#narrow/stream/179202-terminology/topic/ValueSet.20designations.20and.20languages
    displays.addDesignation(cd); { no .link}
end;

procedure TValueSetWorker.deadCheck(place: String);
var
  fn : String;
  comp : TFHIRComposer;
  i : integer;
  m : TFHIRMetadataResourceW;
begin
  if FOpContext.deadCheck then
  begin
    fn := FilePath(['[tmp]', 'vs-dead-'+inttostr(FOpContext.FDeadTime)]);
    comp := FFactory.makeComposer(nil, ffJson, FLangList, OutputStylePretty);
    try
      BytesToFile(comp.ComposeBytes(FValueSet.Resource), fn+'.json');
      i := 0;
      for m in FAdditionalResources do
      begin
        BytesToFile(comp.ComposeBytes(m.Resource), fn+'-'+inttostr(i)+'.json');
        inc(i);
      end;
    finally
      comp.free;
    end;
    logging.log('Expansion took too long @ '+place+': value set '+FValueSet.vurl+' stored at '+fn+'.json');
    raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.languages, [FValueSet.vurl, inttostr(EXPANSION_DEAD_TIME_SECS)]));
  end;
end;

function TValueSetWorker.listVersions(url: String): String;
var
  ts : TStringList;
  matches : TFslMetadataResourceList;
  r : TFHIRMetadataResourceW;
begin
  ts := TStringList.Create;
  try
    ts.Sorted := true;
    ts.Duplicates := Classes.dupIgnore;
    if FAdditionalResources <> nil then
    begin
      for r in FAdditionalResources do
      begin
        if (r.url = url) then
          ts.Add(r.version);
      end;
    end;
    FOnListCodeSystemVersions(self, url, ts);
    result := ts.CommaText;
  finally
    ts.free;
  end;
end;

constructor TFHIRValueSetExpander.Create(factory: TFHIRFactory; opContext: TTerminologyOperationContext; getVS: TGetValueSetEvent; getCS: TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion: TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  inherited create(factory, opContext, getVS, getCS, getVersions, getExpansion, txResources, languages, i18n);
  FCSCounter := TFslMap<TValueSetCounter>.create;
  FCSCounter.defaultValue := nil;
end;

destructor TFHIRValueSetExpander.Destroy;
begin
  FCSCounter.free;
  inherited Destroy;
end;

procedure TFHIRValueSetExpander.addDefinedCode(cs : TFhirCodeSystemW; system: string; c: TFhirCodeSystemConceptW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; excludeInactive : boolean; srcURL : String);
var
  i : integer;
  cds : TConceptDesignations;
  n : TFhirValueSetExpansionContainsW;
begin
  deadCheck('addDefinedCode');
  if not FParams.excludeNotForUI or not (cs.isAbstract(c)) then
  begin
    cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
    try
      listDisplays(cds, c);
      n := processCode(nil, parent, false, system, '', c.Code, cs.isAbstract(c), cs.isInactive(c), cs.isDeprecated(c),  cds, c.definition, c.itemWeight,
         nil, imports, c.getAllExtensionsW, nil, c.properties, nil, excludeInactive, srcUrl);
    finally
      cds.free;
    end;
  end;
  for i := 0 to c.conceptList.count - 1 do
    addDefinedCode(cs, system, c.conceptList[i], imports, n, excludeInactive, srcUrl);
end;

function TFHIRValueSetExpander.canonical(system, version : String) : String;
begin
  if FFactory.version = fhirVersionRelease2 then
    result := system + '?version='+version
  else if version = '' then
    result := system
  else
    result := system + '|'+version
end;

function TFHIRValueSetExpander.passesImport(import: TFHIRImportedValueSet; system, code: String): boolean;
begin
  import.buildMap;
  result := import.hasCode(system, code);
end;

function TFHIRValueSetExpander.passesImports(imports: TFslList<TFHIRImportedValueSet>; system, code: String; offset: integer): boolean;
var
  i : integer;
begin
  result := true;
  if imports <> nil then
  begin
    for i := offset to imports.Count - 1 do
      if not passesImport(imports[i], system, code) then
        exit(false);
  end;
end;

function hasExtension(list : TFslList<TFhirExtensionW>; url : String) : boolean;
var
  ext : TFhirExtensionW;
begin
  result := false;
  if (list <> nil) then
    for ext in list do
      if (ext.url = url) then
        exit(true);
end;

function getExtensionString(list : TFslList<TFhirExtensionW>; url : String) : String;
var
  ext : TFhirExtensionW;
begin
  result := '';
  if (list <> nil) then
    for ext in list do
      if (ext.url = url) then
        exit(ext.value.ToString);
end;

function TFHIRValueSetExpander.useDesignation(cd : TConceptDesignation) : boolean;
var
  s, l, r : String;
begin
  if not FParams.hasDesignations then
    exit(true);
  result := false;
  for s in FParams.designations do
  begin
    StringSplit(s, '|', l, r);
    if (cd.use <> nil) and (cd.use.systemUri = l) and (cd.use.code = r) then
      exit(true);
    if (cd.language <> nil) and (l = 'urn:ietf:bcp:47') and (r = cd.language.code) then
      exit(true);
  end;
end;

function TFHIRValueSetExpander.isValidating: boolean;
begin
  result := false;
end;

function TFHIRValueSetExpander.processCode(cs : TCodeSystemProvider; parent : TFhirValueSetExpansionContainsW; doDelete : boolean; system, version, code : String;
    isAbstract, isInactive, deprecated : boolean; displays : TConceptDesignations; definition, itemWeight: string; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>;
    csExtList, vsExtList : TFslList<TFhirExtensionW>; csProps : TFslList<TFhirCodeSystemConceptPropertyW>; expProps : TFslList<TFhirValueSetExpansionContainsPropertyW>; excludeInactive : boolean; srcURL : string) : TFhirValueSetExpansionContainsW;
var
  n : TFhirValueSetExpansionContainsW;
  s, pn, vs : String;
  pref, t : TConceptDesignation;
  ext : TFHIRExtensionW;
  cp : TFhirCodeSystemConceptPropertyW;
  ts : TStringList;
  cnt : TValueSetCounter;
  vstr : TFHIRObject;
begin
  deadCheck('processCode');
  try
    if not passesImports(imports, system, code, 0) then
      exit;
    if isInactive and excludeInactive then
      exit;

    cnt := FCSCounter[cs.systemUri(nil)];
    if (cnt = nil) then
    begin
      cnt := TValueSetCounter.create;
      FCSCounter.Add(cs.systemUri(nil), cnt);
    end;
    cnt.increment;
    if (cs.expandLimitation > 0) and (cnt.count > cs.expandLimitation) then
      exit;


    if (FLimitCount > 0) and (FFullList.Count >= FLimitCount) and not doDelete then
    begin
      if (FCount > -1) and (FOffset > -1) and (FCount + FOffset > 0) and (FFullList.count > FCount + FOffset) then
        raise EFinished.create('.')
      else
        raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.languages, [srcUrl, '>'+inttostr(FLimitCount)]));
    end;

    if (expansion <> nil) then
    begin
      s := canonical(system, version);
      if not expansion.hasParam('used-codesystem', s) then
        expansion.addParamUri('used-codesystem', s);   
      if not expansion.hasParam('version', s) then
        expansion.addParamUri('version', s);
      if (cs <> nil) then
      begin
        ts := TStringList.create;
        try
          cs.listSupplements(ts);
          for vs in ts do
          begin
            if not expansion.hasParam('used-supplement', vs) then
              expansion.addParamUri('used-supplement', vs);                  
            if not expansion.hasParam('version', vs) then
              expansion.addParamUri('version', vs);
          end;
        finally
          ts.free;
        end;
      end;
    end;

    s := key(system, code);

    if doDelete or not FMap.containsKey(s) then
    begin
      n := FFactory.makeValueSetContains;
      try
        n.systemUri := system;
        n.Code := code;
        if isAbstract then
          n.abstract_ := isAbstract;
        if isInactive then
          n.inactive := isInactive;

        if (deprecated) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#status', 'status', FFactory.makeCode('deprecated'));
        if (hasExtension(csExtList, 'http://hl7.org/fhir/StructureDefinition/codesystem-label')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#label', 'label', FFactory.makeString(getExtensionString(csExtList, 'http://hl7.org/fhir/StructureDefinition/codesystem-label')));
        if (hasExtension(vsExtList, 'http://hl7.org/fhir/StructureDefinition/valueset-label')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#label', 'label', FFactory.makeString(getExtensionString(vsExtList, 'http://hl7.org/fhir/StructureDefinition/valueset-label')));

        if (hasExtension(csExtList, 'http://hl7.org/fhir/StructureDefinition/codesystem-conceptOrder')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#order', 'order', FFactory.makeDecimal(getExtensionString(csExtList, 'http://hl7.org/fhir/StructureDefinition/codesystem-conceptOrder')));
        if (hasExtension(vsExtList, 'http://hl7.org/fhir/StructureDefinition/valueset-conceptOrder')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#order', 'order', FFactory.makeDecimal(getExtensionString(vsExtList, 'http://hl7.org/fhir/StructureDefinition/valueset-conceptOrder')));

        if (hasExtension(csExtList, 'http://hl7.org/fhir/StructureDefinition/itemWeight')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#itemWeight', 'weight', FFactory.makeDecimal(getExtensionString(csExtList, 'http://hl7.org/fhir/StructureDefinition/itemWeight')));
        if (hasExtension(vsExtList, 'http://hl7.org/fhir/StructureDefinition/itemWeight')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#itemWeight', 'weight', FFactory.makeDecimal(getExtensionString(vsExtList, 'http://hl7.org/fhir/StructureDefinition/itemWeight')));

        if (csExtList <> nil) then
          for ext in csExtList do
            if StringArrayExists(['http://hl7.org/fhir/StructureDefinition/coding-sctdescid', 'http://hl7.org/fhir/StructureDefinition/rendering-style',
                                  'http://hl7.org/fhir/StructureDefinition/rendering-xhtml', 'http://hl7.org/fhir/StructureDefinition/codesystem-alternate'], ext.url) then
              n.addExtensionV(ext.element.link);

        if (vsExtList <> nil) then
          for ext in vsExtList do
            if StringArrayExists([EXT_VSSUPPLEMENT, 'http://hl7.org/fhir/StructureDefinition/valueset-deprecated',
                                'http://hl7.org/fhir/StructureDefinition/valueset-concept-definition', 'http://hl7.org/fhir/StructureDefinition/coding-sctdescid',
                                'http://hl7.org/fhir/StructureDefinition/rendering-style', 'http://hl7.org/fhir/StructureDefinition/rendering-xhtml'], ext.url) then
              n.addExtensionV(ext.element.link);

        // display and designations
        pref := displays.preferredDesignation(FParams.languages);
        if (pref <> nil) then
          n.display := pref.value.AsString;

        if FParams.includeDesignations then
        begin
          for t in displays.designations do
            if (t <> pref) and useDesignation(t) and (t.value <> nil) then
              n.addDesignation(t.language, t.use, t.value, t.extensions);
        end;

        for pn in FParams.properties do
        begin
          if pn = 'definition' then
          begin
            if definition <> '' then
            begin
              vstr := FFactory.makeString(definition);
              try
                n.addProperty(pn, vstr);
              finally
                vstr.free;
              end;
            end;
          end
          else if csProps <> nil then
          begin
            for cp in csprops do
            begin
              if cp.code = pn then
                n.addProperty(pn, cp);
            end;
          end;
        end;


  //    for (ParametersParameterComponent p : expParams.getParameter()) {
  //      if ("property".equals(p.getName())) {
  //        if (csProps != null && p.hasValue()) {
  //          for (ConceptPropertyComponent cp : csProps) {
  //            if (p.getValue().primitiveValue().equals(cp.getCode())) {
  //              n.addProperty().setCode(cp.getCode()).setValue(cp.getValue());
  //            }
  //          }
  //        }
  //        if (expProps != null && p.hasValue()) {
  //          for (org.hl7.fhir.r5.model.ValueSet.ConceptPropertyComponent cp : expProps) {
  //            if (p.getValue().primitiveValue().equals(cp.getCode())) {
  //              n.addProperty(cp);
  //            }
  //          }
  //        }
  //      }
  //    }
  //
  //


//
//        n.Display := display.chooseDisplay(FLanguages, FParams.FdisplayLanguage);
//        if FParams.includeDesignations then
//        begin
//          for cd in display do
//          begin
//            if ((cd.language <> FLang) or (cd.value <> n.display)) and isValidLang(cd.language) then
//              n.addDesignation(cd.language, '', cd.value);
//          end;
//        end;

        if (dodelete) then
        begin
          FCanBeHierarchy := false;
          if FMap.ContainsKey(s) then
          begin
            FFullList.Remove(FMap[s]);
            FMap.Remove(s);
          end;
        end
        else if not FMap.ContainsKey(s) then
        begin
          FFullList.add(n.link);
          FMap.add(s, n.link);
          if (FCanBeHierarchy) then
          begin
            if (parent <> nil) then
              parent.addContains(n)
            else
              FRootList.add(n.link);
          end;
        end
        else
          FCanBeHierarchy := false;
        result := n;
      finally
        n.free;
      end;
    end;
  finally
    csExtList.free;
    vsExtList.free;
    csProps.free;
  end;
end;

procedure TFHIRValueSetExpander.checkCanExpandValueset(uri, version: String);
var
  vs : TFHIRValueSetW;
begin
  vs := findValueSet(uri, version);
  try
    if vs = nil then
      raise ETerminologyError.create('Unable to find value set "'+uri+'"', itUnknown);
  finally
    vs.free;
  end;
end;

function TFHIRValueSetExpander.expandValueSet(uri, version, filter: String;
  dependencies: TStringList; var notClosed: boolean): TFHIRValueSetW;
var
  dep : TStringList;
  exp : TFhirValueSetExpansionW;
begin
  dep := TStringList.Create;
  try
    result := FOnGetExpansion(self, FOpContext, uri, version, filter, FParams, dep, FAdditionalResources , -1);
    try
      dependencies.AddStrings(dep);
      if (result = nil) then
        raise ETerminologyError.create('unable to find value set '+uri, itUnknown);
      if result.expansion.hasextension('http://hl7.org/fhir/params/questionnaire-extensions#closed') then
        notClosed := true;
      result.Link;
    finally
      result.free;
    end;
  finally
    dep.free;
  end;
end;

procedure TFHIRValueSetExpander.importValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
var
  c : TFhirValueSetExpansionContainsW;
begin
  FCanBeHierarchy := false;
  expansion.copyParams(vs.expansion);
  checkCanonicalStatus(expansion, vs, FValueSet);

  for c in vs.expansion.contains.forEnum do
  begin
    deadCheck('importValueSet');
    importValueSetItem(nil, c, imports, offset);
  end;
end;

procedure TFHIRValueSetExpander.importValueSetItem(p, c : TFhirValueSetExpansionContainsW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
var
  s : String;              
  cc : TFhirValueSetExpansionContainsW;
begin
  deadCheck('importValueSetItem');
  s := key(c);
  if passesImports(imports, c.systemUri, c.code, offset) and not FMap.containsKey(s) then
  begin
    FFullList.add(c.link);
    if FCanBeHierarchy and (p <> nil) then
      p.addContains(c)
    else
      FRootList.add(c.link);
    FMap.add(s, c.link);
  end;
  for cc in c.contains.forEnum do
     importValueSetItem(c, cc, imports, offset);
end;

procedure TFHIRValueSetExpander.excludeValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
var
  c : TFhirValueSetExpansionContainsW;
  s : String;
begin
  for c in vs.expansion.contains.forEnum do
  begin
    deadCheck('excludeValueSet');
    s := key(c);
    if passesImports(imports, c.systemUri, c.code, offset) and FMap.containsKey(s) then
    begin
      FFullList.Remove(FMap[s]);
      FMap.Remove(s);
    end;
  end;
end;

procedure TFHIRValueSetExpander.checkSource(cset: TFhirValueSetComposeIncludeW; exp: TFHIRValueSetExpansionW; filter : TSearchFilterText; srcURL : String);
var
  cs : TCodeSystemProvider;
  s : string;
  imp : boolean;
begin
  deadCheck('checkSource');
  FFactory.checkNoModifiers(cset,'ValueSetExpander.processCodes', 'set');
  imp := false;
  for s in cset.valueSets do
  begin
    checkCanExpandValueset(s, '');
    imp := true;
  end;

  if cset.systemUri <> '' then
  begin
    cs := findCodeSystem(cset.systemUri, cset.version, FParams, false);
    try

      if cs.contentMode <> cscmComplete then
      begin
        if cs.contentMode = cscmNotPresent then
          raise ETerminologyError.create('The code system definition for '+cset.systemUri+' has no content, so this expansion cannot be performed', itInvalid)
        else if cs.contentMode = cscmNotPresent then
          raise ETerminologyError.create('The code system definition for '+cset.systemUri+' defines a supplement, so this expansion cannot be performed', itInvalid)
        else if FParams.incompleteOK then
          exp.addParamUri(CODES_TFhirCodeSystemContentMode[cs.contentMode], cs.systemUri(nil)+'|'+cs.version(nil))
        else
          raise ETerminologyError.create('The code system definition for '+cset.systemUri+' is a '+CODES_TFhirCodeSystemContentMode[cs.contentMode]+', so this expansion is not permitted unless the expansion parameter "incomplete-ok" has a value of "true"', itInvalid);
      end;

      if (not cset.hasConcepts) and (not cset.hasFilters) then
      begin
        if (cs.SpecialEnumeration <> '') and FParams.limitedExpansion then
        begin
          checkCanExpandValueSet(cs.SpecialEnumeration, '');
        end
        else if filter.Null then // special case - add all the code system
        begin
          if cs.isNotClosed(filter) then
            if cs.SpecialEnumeration <> '' then
              raise ETooCostly.create('The code System "'+cs.systemUri(nil)+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
            else  if (cs.systemUri(nil) = ALL_CODE_CS) then
              raise ETooCostly.create('Cannot filter across all code Systems known to the server')
            else
              raise ETooCostly.create('The code System "'+cs.systemUri(nil)+'" has a grammar, and cannot be enumerated directly');

          if not imp and (FLimitCount > 0) and (cs.TotalCount > FLimitCount) and not (FParams.limitedExpansion) and (FOffset+FCount <= 0) then
            raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.languages, [srcUrl, '>'+inttostr(FLimitCount)]));
        end
      end;

    finally
      cs.free;
    end;
  end;
end;

procedure TFHIRValueSetExpander.processCodes(doDelete : boolean; cset: TFhirValueSetComposeIncludeW; vsSrc : TFHIRValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; excludeInactive : boolean; var notClosed : boolean);
var
  cs : TCodeSystemProvider;
  i, count, offset : integer;
  fc : TFhirValueSetComposeIncludeFilterW;
  fcl : TFslList<TFhirValueSetComposeIncludeFilterW>;
  c : TCodeSystemProviderContext;
  filters : TFslList<TCodeSystemProviderFilterContext>;
  f : TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderFilterContext;
  ok : boolean;
  prep : TCodeSystemProviderFilterPreparationContext;
  inner : boolean;
  s, display, ov, code, vsId : String;
  valueSets : TFslList<TFHIRImportedValueSet>;
  base : TFHIRValueSetW;
  cc : TFhirValueSetComposeIncludeConceptW;
  cctxt : TCodeSystemProviderContext;
  cds : TConceptDesignations;
  iter : TCodeSystemIteratorContext;
  vs : TFHIRValueSetW;
  parent : TFhirValueSetExpansionContainsW;
  ivs : TFHIRImportedValueSet;
  function passesFilters(c : TCodeSystemProviderContext; offset : integer) : boolean;
  var
    j : integer;
    ok : boolean;
    t : TCodeSystemProviderContext;
  begin
    result := true;
    for j := offset to filters.Count - 1 do
    begin
      f := filters[j];
      if f is TSpecialProviderFilterContextNothing then
        result := false
      else if f is TSpecialProviderFilterContextConcepts then
      begin
        ok := false;
        for t in (f as TSpecialProviderFilterContextConcepts).FList do
          if cs.sameContext(t, c) then
            ok := true;
        result := result and ok;
      end
      else
        result := result and cs.InFilter(f, c);
    end;
  end;
begin
  vsId := FValueSet.vurl;
  if (vsId = '') then
    vsId := '<anon>';

  deadCheck('processCodes#1');
  valueSets := TFslList<TFHIRImportedValueSet>.create;
  try
    FFactory.checkNoModifiers(cset,'ValueSetExpander.processCodes', 'set');

    if (cset.hasValueSets or cset.hasConcepts or (cset.filterCount > 1)) then
      FCanBeHierarchy := false;

    if cset.systemUri = '' then
    begin
      for s in cset.valueSets do
      begin
        //Logging.log('Processing '+vsId+', import value set '+s);
        deadCheck('processCodes#2');
        ivs := TFHIRImportedValueSet.create(expandValueset(s, '', filter.filter, dependencies, notClosed));
        try
          checkCanonicalStatus(expansion, ivs.FValueSet, FValueSet);
          expansion.addParamUri('used-valueset', ivs.FValueSet.vurl);
          expansion.addParamUri('version', ivs.FValueSet.vurl);
          valueSets.add(ivs.link);
        finally
          ivs.free;
        end;
      end;
      if doDelete then
        excludeValueSet(valueSets[0].valueSet, expansion, valueSets, 1)
      else
        importValueSet(valueSets[0].valueSet, expansion, valueSets, 1);
    end
    else
    begin
      filters := TFslList<TCodeSystemProviderFilterContext>.create;
      try
        cs := findCodeSystem(cset.systemUri, cset.version, FParams, false);
        try
          //Logging.log('Processing '+vsId+',code system "'+cset.systemUri+'|'+cset.version+'", '+inttostr(cset.filterCount)+' filters, '+inttostr(cset.conceptCount)+' concepts');
          checkSupplements(cs, cset);
          checkCanonicalStatus(expansion, cs, FValueSet);
          for s in cset.valueSets do
          begin
            //Logging.log(' ...import value set '+s);
            deadCheck('processCodes#3');
            f := nil;
            // if we can, we can do a short cut evaluation that means we don't have to do a full expansion of the source value set.
            // this saves lots of overhead we don't need. But it does require simple cases (though they are common). So we have a look
            // at the value set, and see whether we can short cut it. If we can, it's just another filter (though we can't iterate on it)
            vs := FOnGetValueSet(self, s, '');
            try
              if (vs <> nil) then
                f := makeFilterForValueSet(cs, vs);
              if (f <> nil) then
                filters.add(f)
              else
                valueSets.add(TFHIRImportedValueSet.create(expandValueset(s, '', filter.filter, dependencies, notClosed)));
            finally
              vs.free;
            end;
          end;

          if (not cset.hasConcepts) and (not cset.hasFilters) then
          begin
            if (cs.SpecialEnumeration <> '') and FParams.limitedExpansion and filters.Empty then
            begin
              base := expandValueSet(cs.SpecialEnumeration, '', filter.filter, dependencies, notClosed);
              try
                expansion.addExtensionV('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', FFactory.makeBoolean(true));
                if doDelete then
                  excludeValueSet(base, expansion, valueSets, 0)
                else
                  importValueSet(base, expansion, valueSets, 0);
              finally
                base.free;
              end;
              notClosed := true;
            end
            else if filter.Null then // special case - add all the code system
            begin
              if cs.isNotClosed(filter) then
                if cs.SpecialEnumeration <> '' then
                  raise ETooCostly.create('The code System "'+cs.systemUri(nil)+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
                else  if (cs.systemUri(nil) = ALL_CODE_CS) then
                  raise ETooCostly.create('Cannot filter across all code Systems known to the server')
                else
                  raise ETooCostly.create('The code System "'+cs.systemUri(nil)+'" has a grammar, and cannot be enumerated directly');

              iter := cs.getIterator(nil);
              try
                if valueSets.Empty and (FLimitCount > 0) and (iter.count > FLimitCount) and not (FParams.limitedExpansion) and not doDelete and (FOffset + Fcount < 0) then
                  raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.languages, [vsSrc.url, '>'+inttostr(FLimitCount)]));
                while iter.more do
                begin                
                  deadCheck('processCodes#3a');
                  c := cs.getNextContext(iter);
                  try
                    if passesFilters(c, 0) then
                      processCodeAndDescendants(doDelete, cs, c, expansion, valueSets, nil, excludeInactive, vsSrc.url)
                  finally
                    c.free;
                  end;
                end;
              finally
                iter.free;
              end;
            end
            else
            begin
              if cs.isNotClosed(filter) then
                notClosed := true;
              prep := cs.getPrepContext;
              try
                ctxt := cs.searchFilter(filter, prep, false);
                try
                  cs.prepare(prep);
                  while cs.FilterMore(ctxt) do
                  begin
                    deadCheck('processCodes#4');
                    c := cs.FilterConcept(ctxt);
                    try
                      if passesFilters(c, 0) then
                      begin
                        cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
                        try
                          listDisplays(cds, cs, c); // cs.display(c, FParams.displayLanguage)
                          processCode(cs, nil, doDelete, cs.systemUri(c), cs.version(c), cs.code(c),  cs.isAbstract(c), cs.isInactive(c), cs.deprecated(c),
                          cds, cs.definition(c), cs.itemWeight(c), expansion, valueSets, cs.getExtensions(c), nil, cs.getProperties(c), nil, excludeInactive, vsSrc.url);
                        finally
                          cds.free;
                        end;
                      end;
                    finally
                      c.free;
                    end;
                  end;
                finally
                  ctxt.free;
                end;
              finally
                prep.free;
              end;
            end;
          end;

          cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
          try
            for cc in cset.concepts.forEnum do
            begin
              cds.Clear;
              FFactory.checkNoModifiers(cc, 'ValueSetExpander.processCodes', 'set concept reference');
              cctxt := cs.locate(cc.code, FAllAltCodes);
              try
                if (cctxt <> nil) and (not FParams.activeOnly or not cs.IsInactive(cctxt)) and passesFilters(cctxt, 0) then
                begin
                  listDisplays(cds, cs, cctxt);
                  listDisplays(cds, cc, vsSrc);
                  if filter.passes(cds) or filter.passes(cc.code) then
                  begin
                    ov := cc.itemWeight;
                    if ov = '' then
                      ov := cs.itemWeight(cctxt);
                    processCode(cs, nil, doDelete, cs.systemUri(nil), cs.version(nil), cc.code, cs.isAbstract(cctxt), cs.isInactive(cctxt), cs.deprecated(cctxt), cds,
                         cs.Definition(cctxt), ov, expansion, valueSets, cs.getExtensions(cctxt), cc.getAllExtensionsW, cs.getProperties(cctxt), nil, excludeInactive, vsSrc.url);
                  end;
                end;
              finally
                cctxt.free;
              end;
            end;
          finally
            cds.free;
          end;

          if cset.hasFilters then
          begin
            fcl := cset.filters;
            try
              prep := cs.getPrepContext;
              try
                offset := 0;
                if not filter.null then
                begin
                  filters.Insert(0, cs.searchFilter(filter, prep, true)); // this comes first, because it imposes order
                  inc(offset);
                end;

                if cs.specialEnumeration <> '' then
                begin
                  filters.Insert(offset, cs.specialFilter(prep, true));
                  expansion.addExtensionV('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', FFactory.makeBoolean(true));
                  notClosed := true;
                end;
                for i := 0 to fcl.count - 1 do
                begin
                  fc := fcl[i];
                  ffactory.checkNoModifiers(fc, 'ValueSetExpander.processCodes', 'filter');
                  f := cs.filter(i = 0, fc.prop, fc.Op, fc.value, prep);
                  if f = nil then
                    raise ETerminologyError.create('The filter "'+fc.prop +' '+ CODES_TFhirFilterOperator[fc.Op]+ ' '+fc.value+'" from the value set '+vsSrc.url+' was not understood in the context of '+cs.systemUri(nil), itNotSupported);
                  filters.Insert(offset, f);
                  if cs.isNotClosed(filter, f) then
                    notClosed := true;
                end;

                inner := cs.prepare(prep);
                count := 0;
                While cs.FilterMore(filters[0]) and (((FOffset <= 0) and (FCount <= 0)) or (count < FOffset + FCount)) do
                begin
                  deadCheck('processCodes#5');
                  c := cs.FilterConcept(filters[0]);
                  try
                    ok := (not FParams.activeOnly or not cs.IsInactive(c)) and (inner or passesFilters(c, 1));
                    if ok then
                    begin
                      inc(count);
                      if count > FOffset then
                      begin
                        cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
                        try
                          if passesImports(valueSets, cs.systemUri(nil), cs.code(c), 0) then
                          begin
                            listDisplays(cds, cs, c);
                            if cs.canParent then
                              parent := FMap[key(cs.systemUri(c), cs.parent(c))]
                            else
                            begin
                              FCanBeHierarchy := false;
                              parent := nil;
                            end;
                            for code in cs.listCodes(c, FParams.altCodeRules) do
                              processCode(cs, parent, doDelete, cs.systemUri(nil), cs.version(nil), code, cs.isAbstract(c), cs.IsInactive(c),
                                cs.deprecated(c), cds, cs.definition(c), cs.itemWeight(c), expansion, nil, cs.getExtensions(c), nil, cs.getProperties(c), nil, excludeInactive, vsSrc.url);
                          end;
                        finally
                          cds.free;
                        end;
                      end;
                    end;
                  finally
                    c.free;
                  end;
                end;
              finally
                prep.free;
              end;
            finally
              fcl.free;
            end;
          end;
        finally
          cs.free;
        end;
      finally
        filters.free;
      end;
    end;
  finally
    valueSets.free;
  end;
end;

procedure TFHIRValueSetExpander.processCodeAndDescendants(doDelete : boolean; cs: TCodeSystemProvider; context: TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; excludeInactive : boolean; srcUrl : String);
var
  i : integer;
  vs, code : String;
  cds : TConceptDesignations;
  iter : TCodeSystemIteratorContext;
  n, t : TFhirValueSetExpansionContainsW;
  ts : TStringList;
  c : TCodeSystemProviderContext;
begin
  deadCheck('processCodeAndDescendants');
  if (expansion <> nil) then
  begin
    vs := canonical(cs.systemUri(nil), cs.version(nil));
    if not expansion.hasParam('used-codesystem', vs) then
      expansion.addParamUri('used-codesystem', vs);
    if not expansion.hasParam('version', vs) then
      expansion.addParamUri('version', vs);
    ts := TStringList.create;
    try
      cs.listSupplements(ts);
      for vs in ts do
      begin
        if not expansion.hasParam('used-supplement', vs) then
          expansion.addParamUri('used-supplement', vs);
        if not expansion.hasParam('version', vs) then
          expansion.addParamUri('version', vs);
      end;
    finally
      ts.free;
    end;
  end;
  n := nil;
  if (not FParams.excludeNotForUI or not cs.IsAbstract(context)) and (not FParams.activeOnly or not cs.isInActive(context)) then
  begin
    cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
    try
      deadCheck('processCodeAndDescendants#2');
      listDisplays(cds, cs, context);
      for code in cs.listCodes(context, FParams.altCodeRules) do
      begin
        t := processCode(cs, parent, doDelete, cs.systemUri(context), cs.version(context), code, cs.isAbstract(context), cs.IsInactive(context), cs.deprecated(context), cds, cs.definition(context),
           cs.itemWeight(context), expansion, imports, cs.getExtensions(context), nil, cs.getProperties(context), nil, excludeInactive, srcUrl);
        if (n = nil) then
          n := t;
      end;
    finally
      cds.free;
    end;
  end
  else
    n := parent;
  iter := cs.getIterator(context);
  try
    while iter.more do
    begin
      deadCheck('processCodeAndDescendants#3');
      c := cs.getNextContext(iter);
      try
        processCodeAndDescendants(doDelete, cs, c, expansion, imports, n, excludeInactive, srcUrl);
      finally
        c.free;
      end;
    end;
  finally
    iter.free;
  end;
end;

{ TFHIRExpansionParams }

constructor TFHIRExpansionParams.Create;
begin
  inherited;
  FVersionRules := TFslList<TFhirExpansionParamsVersionRule>.create;
  FProperties := TStringList.create;
  FAltCodeRules := TAlternateCodeOptions.create;
  FDesignations := TStringlist.create;

  FGenerateNarrative := true;
end;

procedure TFHIRExpansionParams.SetLanguages(value: THTTPLanguageList);
begin
  FLanguages.free;
  FLanguages := value;
end;

function TFHIRExpansionParams.GetHasLanguages: boolean;
begin
  result := (FLanguages <> nil) and (FLanguages.source <> '');
end;

function TFHIRExpansionParams.GetHasDesignations: boolean;
begin
  result := designations.Count > 0;
end;

procedure TFHIRExpansionParams.SetActiveOnly(value : boolean);
begin
  FActiveOnly := value;
  FHasActiveOnly := true;
end;

procedure TFHIRExpansionParams.SetExcludeNested(value : boolean);
begin
  FExcludeNested := value;
  FHasExcludeNested:= true;
end;

procedure TFHIRExpansionParams.SetGenerateNarrative(value : boolean);
begin
  FGenerateNarrative := value;
  FHasGenerateNarrative := true;
end;

procedure TFHIRExpansionParams.SetLimitedExpansion(value : boolean);
begin
  FLimitedExpansion := value;
  FHasLimitedExpansion := true;
end;

procedure TFHIRExpansionParams.SetExcludeNotForUI(value : boolean);
begin
  FExcludeNotForUI := value;
  FHasExcludeNotForUI := true;
end;

procedure TFHIRExpansionParams.SetExcludePostCoordinated(value : boolean);
begin
  FExcludePostCoordinated := value;
  FHasExcludePostCoordinated := true;
end;

procedure TFHIRExpansionParams.SetIncludeDesignations(value : boolean);
begin
  FIncludeDesignations := value;
  FHasIncludeDesignations := true;
end;

procedure TFHIRExpansionParams.SetIncludeDefinition(value : boolean);
begin
  FIncludeDefinition := value;
  FHasIncludeDefinition := true;
end;

procedure TFHIRExpansionParams.SetDefaultToLatestVersion(value : boolean);
begin
  FDefaultToLatestVersion := value;
  FHasDefaultToLatestVersion := true;
end;

procedure TFHIRExpansionParams.SetIncompleteOK(value : boolean);
begin
  FIncompleteOK := value;
  FHasIncompleteOK := true;
end;

procedure TFHIRExpansionParams.SetDisplayWarning(value : boolean);
begin
  FDisplayWarning := value;
  FHasDisplayWarning := true;
end;

procedure TFHIRExpansionParams.SetMembershipOnly(value : boolean);
begin
  FMembershipOnly := value;
  FHasMembershipOnly := true;
end;

function TFHIRExpansionParams.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FVersionRules.sizeInBytes(magic));
  inc(result, FLanguages.sizeInBytes(magic));
  inc(result, (FUid.length * sizeof(char)) + 12);
end;

class function TFHIRExpansionParams.defaultProfile: TFHIRExpansionParams;
begin
  result := TFHIRExpansionParams.Create;
end;

procedure TFHIRExpansionParams.seeParameter(name: String; value: TFHIRObject; isValidation, overwrite: boolean);
begin
  if (value <> nil) then
  begin
    if (name = 'displayLanguage') and (not HasLanguages or overwrite) then
      languages := THTTPLanguageList.create(value.primitiveValue, not isValidation);

    if (name = 'includeAlternateCodes') then
      altCodeRules.seeParam(value.primitiveValue);
    if (name = 'designation') then
      designations.add(value.primitiveValue);
  end;
end;

function TFHIRExpansionParams.getVersionForRule(systemURI: String; mode: TFhirExpansionParamsVersionRuleMode): String;
var
  rule : TFhirExpansionParamsVersionRule;
begin
  for rule in FVersionRules do
    if (rule.system = systemUri) and (rule.mode = mode) then
      exit(rule.version);
  result := '';
end;

procedure TFHIRExpansionParams.seeVersionRule(url: String; mode: TFhirExpansionParamsVersionRuleMode);
var
  sl : TArray<String>;
begin
  sl := url.split(['|']);
  if (Length(sl) = 2) then
    versionRules.Add(TFhirExpansionParamsVersionRule.Create(sl[0], sl[1], mode))
  else
    raise ETerminologyError.Create('Unable to understand '+CODES_TFhirExpansionParamsVersionRuleMode[mode]+' system version "'+url+'"', itInvalid);
end;

function TFHIRExpansionParams.langSummary: String;
begin
  if (FLanguages = nil) or (FLanguages.source = '') then
    result := '--'
  else
    result := FLanguages.asString(false);
end;

destructor TFHIRExpansionParams.Destroy;
begin
  FAltCodeRules.free;
  FVersionRules.free;
  FLanguages.free;
  FProperties.free;
  FDesignations.free;
  inherited;
end;

function TFHIRExpansionParams.hash: String;
var
  s : String;
  l : TIETFLang;
  t : TFhirExpansionParamsVersionRule;
  function b(v : boolean):string;
  begin
    if v then
      result := '1|'
    else
      result := '0|';
  end;
begin
  s := FUid+'|'+ b(FMembershipOnly) + '|' + FProperties.CommaText+'|'+
    b(FactiveOnly)+b(FIncompleteOK)+b(FDisplayWarning)+b(FexcludeNested)+b(FGenerateNarrative)+b(FlimitedExpansion)+b(FexcludeNotForUI)+b(FexcludePostCoordinated)+
    b(FincludeDesignations)+b(FincludeDefinition)+b(FHasactiveOnly)+b(FHasExcludeNested)+b(FHasGenerateNarrative)+
    b(FHasLimitedExpansion)+b(FHesExcludeNotForUI)+b(FHasExcludePostCoordinated)+b(FHasIncludeDesignations)+
    b(FHasIncludeDefinition)+b(FHasDefaultToLatestVersion)+b(FHasIncompleteOK)+b(FHasDisplayWarning)+b(FHasexcludeNotForUI)+b(FHasMembershipOnly)+b(FDefaultToLatestVersion);

  if hasLanguages then
    s := s + FLanguages.AsString(true)+'|';
  if hasDesignations then
    s := s + FDesignations.commaText+'|';
  for t in FVersionRules do
  s := s + t.asString+'|';
  result := inttostr(HashStringToCode32(s));
end;

function TFHIRExpansionParams.link: TFHIRExpansionParams;
begin
  result := TFHIRExpansionParams(inherited Link);
end;


{ TFhirExpansionParamsVersionRule }

constructor TFhirExpansionParamsVersionRule.Create(system, version: String; mode: TFhirExpansionParamsVersionRuleMode);
begin
  inherited Create;
  FSystem := system;
  FVersion := version;
  FMode := mode;
end;

function TFhirExpansionParamsVersionRule.asString: String;
begin
  result := Fsystem+'#'+Fversion+'/'+inttostr(ord(FMode));
end;

constructor TFhirExpansionParamsVersionRule.Create(system, version: String);
begin
  inherited Create;
  FSystem := system;
  FVersion := version;
end;

function TFhirExpansionParamsVersionRule.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (Fsystem.length * sizeof(char)) + 12);
  inc(result, (Fversion.length * sizeof(char)) + 12);
end;

{ TFHIRImportedValueSet }

constructor TFHIRImportedValueSet.Create(valueSet: TFHIRValueSetW);
begin
  inherited Create;
  FValueSet := valueSet;
  FMap := nil;
end;

destructor TFHIRImportedValueSet.Destroy;
begin
  FMap.free;
  FValueSet.free;
  inherited;
end;

function TFHIRImportedValueSet.link: TFHIRImportedValueSet;
begin
  result := TFHIRImportedValueSet(inherited link);
end;

procedure TFHIRImportedValueSet.SetValueSet(const Value: TFHIRValueSetW);
begin
  FValueSet.free;
  FValueSet := Value;
end;

procedure TFHIRImportedValueSet.buildMap;
var
  cc : TFhirValueSetExpansionContainsW;
begin
  if FMap = nil then
  begin
    FMap := TStringList.Create;
    for cc in FValueSet.expansion.contains.forEnum do
      addToMap(cc);
    FMap.Sort;
  end;
end;

procedure TFHIRImportedValueSet.addToMap(c: TFhirValueSetExpansionContainsW);
var
  cc : TFhirValueSetExpansionContainsW;
begin
  if (c.systemUri <> '') and (c.code <> '') then
    FMap.Add(key(c.systemUri, c.code));
  for cc in c.contains.forEnum do
    addToMap(cc);
end;

function TFHIRImportedValueSet.hasCode(system, code: String): boolean;
begin
  result := FMap.IndexOf(key(system, code)) > -1;
end;

function TFHIRImportedValueSet.key(system, code: String): String;
begin
  result := system+#1+code;
end;

end.



