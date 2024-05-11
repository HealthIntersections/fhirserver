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
{.$.DEFINE DUMP_DEAD_VS}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_collections, fsl_utilities, fsl_http, fsl_lang, fsl_logging, fsl_i18n, fsl_versions, fsl_threads,
  fhir_objects, fhir_common, ftx_service, fhir_factory, fhir_xhtml, fhir_extensions, fhir_uris, fhir_parser,
  fhir_codesystem_service, fhir_tx;

const
  UPPER_LIMIT_NO_TEXT = 1000;
  UPPER_LIMIT_TEXT = 1000;// won't expand a value set bigger than this - just takes too long, and no one's going to do anything with it anyway
  INTERNAL_LIMIT = 10000; // won't make an internal buffer bigger than this. This might need to be expanded if there's a big exclusion?

  FHIR_VERSION_CANONICAL_SPLIT_2 = '?version=';
  FHIR_VERSION_CANONICAL_SPLIT_3p = '|';

  EXPANSION_DEAD_TIME_SECS = 30;
  VALIDATION_DEAD_TIME_SECS = 30;


Type

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


  TGetValueSetEvent = function (sender : TObject; url, version : String) : TFHIRValueSetW of object;
  TGetExpansionEvent = function (sender : TObject; opContext: TTerminologyOperationContext; url, version, filter : String; params : TFHIRTxOperationParams; dependencies : TStringList; additionalResources : TFslMetadataResourceList; limit : integer; noCacheThisOne : boolean) : TFHIRValueSetW of object;

  { TValueSetWorker }

  TValueSetWorker = class (TTerminologyWorker)
  private
    FOnGetValueSet : TGetValueSetEvent;
    FOnGetExpansion : TGetExpansionEvent;
    FValueSet : TFHIRValueSetW;

    function findValueSet(url, version : String) : TFHIRValueSetW;
  protected
    FAllAltCodes : TAlternateCodeOptions;

    procedure seeValueSet(vs : TFHIRValueSetW);

    function sizeInBytesV(magic : integer) : cardinal; override;
    procedure listDisplays(displays : TConceptDesignations; cs : TCodeSystemProvider; c: TCodeSystemProviderContext); overload;
    procedure listDisplays(displays : TConceptDesignations; c: TFhirCodeSystemConceptW); overload;
    procedure listDisplays(displays: TConceptDesignations; c: TFhirValueSetComposeIncludeConceptW; vs : TFHIRValueSetW); overload;
    function isValidating : boolean; virtual; abstract;
    procedure deadCheck(place : String); override;
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
    function check(path, system, version, code : String; abstractOk, inferSystem : boolean; displays : TConceptDesignations; unknownSystems : TStringList; var message, ver : String; var inactive : boolean; var normalForm : String; var vstatus : String; var cause : TFhirIssueType; op : TFhirOperationOutcomeW; vcc : TFHIRCodeableConceptW; params: TFHIRParametersW; var contentMode : TFhirCodeSystemContentMode; var impliedSystem : string; unkCodes, messages : TStringList) : TTrueFalseUnknown; overload;
    function findCode(cs : TFhirCodeSystemW; code: String; list : TFhirCodeSystemConceptListW; displays : TConceptDesignations; out isabstract : boolean): boolean;
    function checkConceptSet(path : String; cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code : String; abstractOk : boolean; displays : TConceptDesignations; vs : TFHIRValueSetW; var message : String; var inactive : boolean; var normalForm : String; var vstatus : String; op : TFHIROperationOutcomeW; vcc : TFHIRCodeableConceptW) : boolean;
    function checkExpansion(path : String; cs: TCodeSystemProvider; cset : TFhirValueSetExpansionContainsW; code : String; abstractOk : boolean; displays : TConceptDesignations; vs : TFHIRValueSetW; var message : String; var inactive : boolean; var vstatus : String; op : TFHIROperationOutcomeW) : boolean;
    function fixedSystemFromValueSet: String;
    procedure prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW; unknownValueSets : TStringList);
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

    procedure prepare(vs : TFHIRValueSetW; params : TFHIRTxOperationParams; unknownValueSets : TStringList);

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

  TTotalStatus = (tsUninitialised, tsSet, tsOff);

  { TFHIRValueSetExpander }

  TFHIRValueSetExpander = class (TValueSetWorker)
  private
    FOffset : integer;
    FCount : integer;
    FLimitCount : integer;
    FCanBeHierarchy : boolean;
    FHasExclusions : boolean;
    FExcluded : TFslStringSet;
    FRootList : TFslList<TFhirValueSetExpansionContainsW>;
    FFullList : TFslList<TFhirValueSetExpansionContainsW>;
    FMap : TFslMap<TFhirValueSetExpansionContainsW>;
    FCSCounter : TFslMap<TValueSetCounter>;
    FTotalStatus : TTotalStatus;
    FTotal : integer;

    function isExcluded(system, version, code : String) : boolean;
    procedure NoTotal;
    procedure AddToTotal(t : Integer);
    procedure checkCanonicalStatus(expansion : TFhirValueSetExpansionW; resource : TFHIRMetadataResourceW; source : TFHIRValueSetW); overload;
    procedure checkCanonicalStatus(expansion : TFhirValueSetExpansionW; cs : TCodeSystemProvider; source : TFHIRValueSetW); overload;
    procedure checkCanonicalStatus(expansion: TFhirValueSetExpansionW; vurl : String; status: TPublicationStatus; standardsStatus: String; experimental : boolean; source : TFHIRValueSetW); overload;
    procedure importValueSetItem(p, c: TFhirValueSetExpansionContainsW; imports: TFslList<TFHIRImportedValueSet>; offset: integer);
    function makeFilterForValueSet(cs : TCodeSystemProvider; vs : TFHIRValueSetW) : TCodeSystemProviderFilterContext;
    function includeCodeAndDescendants(cs : TCodeSystemProvider; context : TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; excludeInactive : boolean; srcUrl : String) : integer;
    procedure excludeCodeAndDescendants(cs : TCodeSystemProvider; context : TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; excludeInactive : boolean; srcUrl : String);

    procedure handleDefine(cs : TFhirCodeSystemW; source : TFhirValueSetCodeSystemW; defines : TFhirCodeSystemConceptListW; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; excludeInactive : boolean; srcURL : String);
    procedure importValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
    procedure excludeValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
    procedure includeCodes(cset : TFhirValueSetComposeIncludeW; vsSrc : TFHIRValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; excludeInactive : boolean; var notClosed : boolean);
    procedure excludeCodes(cset : TFhirValueSetComposeIncludeW; vsSrc : TFHIRValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; excludeInactive : boolean; var notClosed : boolean);
    procedure handleCompose(source : TFhirValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);

    function passesImports(imports : TFslList<TFHIRImportedValueSet>; system, code : String; offset : integer) : boolean;
    function passesImport(import : TFHIRImportedValueSet; system, code : String) : boolean;

    function includeCode(cs : TCodeSystemProvider; parent : TFhirValueSetExpansionContainsW; system, version, code : String; isAbstract, isInactive, deprecated : boolean; displays : TConceptDesignations; definition, itemWeight: string; expansion : TFhirValueSetExpansionW;
        imports : TFslList<TFHIRImportedValueSet>; csExtList, vsExtList : TFslList<TFhirExtensionW>; csProps : TFslList<TFhirCodeSystemConceptPropertyW>; expProps : TFslList<TFhirValueSetExpansionContainsPropertyW>; excludeInactive : boolean; srcURL : string) : TFhirValueSetExpansionContainsW;
    procedure excludeCode(cs : TCodeSystemProvider; system, version, code : String; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; srcURL : string);
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

    function expand(source : TFHIRValueSetW; params : TFHIRTxOperationParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer; noCacheThisOne : boolean) : TFHIRValueSetW;
  end;


  { TFHIRConceptMapTranslator }

  TFHIRConceptMapTranslator = class (TValueSetWorker)
  private
    function checkCode(op : TFhirOperationOutcomeW; langList : THTTPLanguageList; path : string; code : string; system, version : string; display : string) : boolean;
    function isOkTarget(cm: TFhirConceptMapW; vs: TFhirValueSetW): boolean;
    function isOkSource(cm: TFhirConceptMapW; vs: TFhirValueSetW; coding: TFHIRCodingW; out group : TFhirConceptMapGroupW; out match : TFhirConceptMapGroupElementW): boolean; overload;
    function isOkSource(cm: TFhirConceptMapW; coding: TFHIRCodingW; out group : TFhirConceptMapGroupW; out match : TFhirConceptMapGroupElementW): boolean; overload;
    function findConceptMap(var cm: TFhirConceptMapW; var msg : String): boolean;
  public
    constructor Create(factory : TFHIRFactory; opContext : TTerminologyOperationContext; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion : TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport); overload;
    destructor Destroy; override;

    function translate(langList : THTTPLanguageList; reqId : String; cml : TFslList<TFHIRConceptMapW>; coding: TFHIRCodingW; params : TFhirParametersW; profile : TFhirTxOperationParams) : TFhirParametersW;
    //function translate(langList : THTTPLanguageList; reqId : String; cm : TLoadedConceptMap; coding : TFHIRCodingW; params : TFhirParametersW; txResources : TFslMetadataResourceList; profile : TFhirTxOperationParams): TFhirParametersW; overload;
    //function translate(langList : THTTPLanguageList; source : TFhirValueSetW; coding : TFHIRCodingW; target : TFhirValueSetW; params : TFhirParametersW; txResources : TFslMetadataResourceList; profile : TFhirTxOperationParams) : TFhirParametersW; overload;
    //function translate(langList : THTTPLanguageList; source : TFhirValueSetW; coded : TFhirCodeableConceptW; target : TFhirValueSetW; params : TFhirParametersW; txResources : TFslMetadataResourceList; profile : TFhirTxOperationParams) : TFhirParametersW; overload;
  end;

implementation

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
  inherited Create(factory, opContext, getCS, getVersions, txResources, languages, i18n);
  FOnGetValueSet := getVS;
  FOnGetExpansion := getExpansion;
  FAllAltCodes := TAlternateCodeOptions.create;
  FAllAltCodes.all := true;
end;

destructor TValueSetWorker.Destroy;
begin
  FValueSet.free;
  FAllAltCodes.free;
  inherited;
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
  if (cs.version <> '') then
    checkCanonicalStatus(path, op, 'CodeSystem', cs.systemUri+'|'+cs.version, status, standardsStatus, experimental, source)
  else
    checkCanonicalStatus(path, op, 'CodeSystem', cs.systemUri, status, standardsStatus, experimental, source);
end;

procedure TValueSetChecker.checkCanonicalStatus(path : string; op : TFhirOperationOutcomeW; rtype, vurl: String; status: TPublicationStatus; standardsStatus: String; experimental : boolean; source : TFHIRMetadataResourceW);
begin
  if op <> nil then
  begin
    if standardsStatus = 'deprecated' then
      op.addIssue(isInformation, itBusinessRule, '', 'MSG_DEPRECATED', FI18n.translate('MSG_DEPRECATED', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
    else if standardsStatus = 'withdrawn' then
      op.addIssue(isInformation, itBusinessRule, '', 'MSG_WITHDRAWN', FI18n.translate('MSG_WITHDRAWN', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
    else if status = psRetired then
      op.addIssue(isInformation, itBusinessRule, '', 'MSG_RETIRED', FI18n.translate('MSG_RETIRED', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
    else if (source <> nil) then
    begin
      if experimental and not source.experimental then
        op.addIssue(isInformation, itBusinessRule, '', 'MSG_EXPERIMENTAL', FI18n.translate('MSG_EXPERIMENTAL', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
      else if ((status = psDraft) or (standardsStatus = 'draft')) and
          not ((source.status = psDraft) or (source.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status') = 'draft')) then
        op.addIssue(isInformation, itBusinessRule, '', 'MSG_DRAFT', FI18n.translate('MSG_DRAFT', FParams.languages, [vurl, '', rtype]), oicStatusCheck, false)
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
        vse := exp.expand(FValueSet, FParams, '', dep, 10000, 10000, 0, FNoCacheThisOne);
        try
          result := '';
          for c in vse.expansion.contains.forEnum do
          begin
            deadCheck('determineSystemFromExpansion');
            if (c.code = code) then
              if result = '' then
                result := c.systemUri
              else
                exit('');
          end;
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
    deadCheck('fixedSystemFromValueSet');
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
  msg : String;
  loc : TCodeSystemProviderContext;
  needDoExpansion : boolean;
begin
  result := '';
  needDoExpansion := false;

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
      deadCheck('determineSystem');
      cs := findCodeSystem(vsi.systemUri, '', nil, true);
      if (cs = nil) then
        exit('');
      try
        if (vsi.hasConcepts) then
        begin
          for cc in vsi.concepts.forEnum do
          begin
            deadCheck('determineSystem#2');
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

procedure TValueSetChecker.prepare(vs: TFHIRValueSetW; params : TFHIRTxOperationParams; unknownValueSets : TStringList);
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
  FParams := params.Link;
  if (vs = nil) then
    raise EFHIROperationException.Create(isError, itNotFound, oicNotFound, '', 'Error Error: vs = nil')
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
        cs := TFhirCodeSystemProvider.create(FLanguages.link, FI18n.link, ffactory.link, TFHIRCodeSystemEntry.Create(FFactory.wrapCodeSystem(FValueSet.Resource.Link)));
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
        deadCheck('prepare');
        other := findValueSet(s, '');
        try
          if other = nil then
            raise EFHIROperationException.CreateMsg(isError, itNotFound, oicNotFound, '', 'Unable_to_resolve_value_Set_', [s]);
          checker := TValueSetChecker.create(FFactory.link, FopContext.copy, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FOnGetExpansion, FAdditionalResources.link, FLanguages.link, other.url, FI18n.link);
          try
            checker.prepare(other, params, unknownValueSets);
            FOthers.Add(s, checker.Link);
          finally
            checker.free;
          end;
        finally
          other.free;
        end;
      end;

      for cc in FValueSet.includes.forEnum do
        prepareConceptSet('include', cc, unknownValueSets);
      for cc in FValueSet.excludes.forEnum do
        prepareConceptSet('exclude', cc, unknownValueSets);
    end;
  end;
  if (FRequiredSupplements.count > 0) then
    raise ETerminologyError.create(FI18n.translatePlural(FRequiredSupplements.Count, 'VALUESET_SUPPLEMENT_MISSING', FParams.languages, [FRequiredSupplements.commaText]), itNotFound);
end;

procedure TValueSetChecker.prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW; unknownValueSets : TStringList);
var
  other: TFhirValueSetW;
  checker: TValueSetChecker;
  s : string;
  ccf: TFhirValueSetComposeIncludeFilterW;
  cs: TCodeSystemProvider;
  i : integer;
  unknownSystems : TStringList;
begin
  deadCheck('prepareConceptSet');
  FFactory.checkNoModifiers(cc, 'ValueSetChecker.prepare', desc);
  for s in cc.valueSets do
  begin
    deadCheck('prepareConceptSet');
    if not FOthers.ExistsByKey(s) then
    begin
      other := findValueSet(s, '');
      try
        if other = nil then
        begin
          if unknownValueSets <> nil then
            unknownValueSets.add(s);
          raise EFHIROperationException.CreateMsg(isError, itNotFound, oicNotFound, '', 'Unable_to_resolve_value_Set_', [s]);
        end;
        checker := TValueSetChecker.create(FFactory.link, FOpContext.copy, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FOnGetExpansion, FAdditionalResources.link, FLanguages.link, other.url, FI18n.link);
        try
          checker.prepare(other, FParams, nil);
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
      deadCheck('prepareConceptSet#2');
      FFactory.checkNoModifiers(ccf, 'ValueSetChecker.prepare', desc + '.filter');
      if not (('concept' = ccf.prop) and (ccf.Op in [foIsA, foDescendentOf])) then
        if not cs.doesFilter(ccf.prop, ccf.Op, ccf.value) then
          raise ETerminologyError.create('The filter "' + ccf.prop + ' ' + CODES_TFhirFilterOperator[ccf.Op] + ' ' + ccf.value + '"  from the value set '+FValueSet.url+' was not understood in the context of ' + cs.systemUri, itNotSupported);
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
    deadCheck('findCode');
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
  normalForm : String;
begin
  FOpContext.clearContexts;
  unknownSystems := TStringList.create;
  ts := TStringList.create;
  msgs := TStringList.create;
  try
    unknownSystems.duplicates := dupIgnore;
    unknownSystems.sorted := true;
    result := check(issuePath, system, version, code, abstractOk, inferSystem, nil, unknownSystems, msg, ver, inactive, normalForm, vstatus, it, op, nil, nil, contentMode, impliedSystem, ts, msgs);
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
  var message, ver: String; var inactive : boolean; var normalForm : String; var vstatus : String; var cause: TFhirIssueType; op: TFhirOperationOutcomeW;
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
    op.addIssue(isWarning, itInvalid, path, msg, oicInvalidData);
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
      if system = '' then
      begin
        msg := FI18n.translate('Coding_has_no_system__cannot_validate_NO_INFER', FParams.languages, []);
        messages.add(msg);
        op.addIssue(isWarning, itInvalid, path, msg, oicInvalidData);
        exit(bFalse);
      end;
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
          ver := cs.version;
          contentMode := cs.contentMode;
          ctxt := cs.locate(code, nil, msg);
          if (ctxt = nil) then
          begin
            msg := '';
            unkCodes.add(cs.systemUri+'|'+cs.version+'#'+code);
            if cs.contentMode <> cscmComplete then
            begin
              result := bTrue; // we can't say it isn't valid. Need a third status?
              cause := itCodeInvalid;
              FLog := 'Not found in Incomplete Code System';
              msg := FI18n.translate('UNKNOWN_CODE_IN_FRAGMENT', FParams.languages, [code, cs.systemUri, cs.version]);
              messages.add(msg);
              op.addIssue(isWarning, itCodeInvalid, addToPath(path, 'code'), msg, oicInvalidCode);
            end
            else
            begin
              result := bFalse;
              cause := itCodeInvalid;
              FLog := 'Unknown code';
              msg := FI18n.translate('Unknown_Code_in_Version', FParams.languages, [code, cs.systemUri, cs.version]);
              messages.add(msg);
              op.addIssue(isError, itCodeInvalid, addToPath(path, 'code'), msg, oicInvalidCode);
            end;
          end
          else
          begin
            try
              if vcc <> nil then
                vcc.addCoding(cs.systemUri, cs.version, cs.code(ctxt), cs.display(ctxt, FParams.languages));
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
                if (cs.Code(ctxt) <> code) then
                begin
                  msg := FI18n.translate('CODE_CASE_DIFFERENCE', FParams.languages, [code, cs.Code(ctxt), cs.systemUri]);
                  messages.add(msg);
                  op.addIssue(isWarning, itBusinessRule, addToPath(path, 'code'), msg, oicCodeRule);
                end; 
                msg := cs.incompleteValidationMessage(ctxt, FParams.languages);
                if (msg <> '') then
                  op.addIssue(isInformation, itInformational, addToPath(path, 'code'), msg, oicProcessingNote);
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
          ver := cs.version;
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
          message := FI18n.translate('UNABLE_TO_INFER_CODESYSTEM', FParams.languages, [code, FValueSet.vurl]);
          messages.add(message);
          op.addIssue(isError, itNotFound, 'code', message, oicInferFailed);
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
          deadCheck('check#1');
          if result = bFalse then
          begin
            checker := TValueSetChecker(FOthers.matches[s]);
            if (checker = nil) then
              raise ETerminologyError.Create('No Match for '+s+' in '+FOthers.AsText, itUnknown);
            checkCanonicalStatus(path, op, checker.FValueSet, FValueSet);
            result := checker.check(path, system, version, code, abstractOk, inferSystem, displays, unknownSystems, message, ver, inactive, normalForm, vstatus, cause, op, nil, params, contentMode, impliedSystem, unkCodes, messages);
          end;
        end;
        for cc in FValueSet.includes.forEnum do
        begin
          deadCheck('check#2');
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
              ver := cs.version;
              checkSupplements(cs, cc);
              contentMode := cs.contentMode;

              if ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri = system)) and checkConceptSet(path, cs, cc, code, abstractOk, displays, FValueSet, message, inactive, normalForm, vstatus, op, vcc) then
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
            deadCheck('check#3');
            checker := TValueSetChecker(FOthers.matches[s]);
            if checker = nil then
              raise ETerminologyError.Create('No Match for '+s+' in '+FOthers.AsText, itUnknown);
            checkCanonicalStatus(path, op, checker.FValueSet, FValueSet);
            if (result = bTrue) then
              result := checker.check(path, system, version, code, abstractOk, inferSystem, displays, unknownSystems, message, ver, inactive, normalForm, vstatus, cause, op, nil,  params, contentMode, impliedSystem, unkCodes, messages);
          end;
          if result = bTrue then
            break;
        end;
        if result = bTrue then
          for cc in FValueSet.excludes.forEnum do
          begin
            deadCheck('check#4');
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
              ver := cs.version;
              contentMode := cs.contentMode;
              excluded := ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri = system)) and checkConceptSet(path, cs, cc, code, abstractOk, displays, FValueSet, message, inactive, normalForm, vstatus, op, vcc);
            end;
            for s in cc.valueSets do
            begin
              deadCheck('check#5');
              checker := TValueSetChecker(FOthers.matches[s]);
              if (cs = nil) then
                raise ETerminologyError.Create('No Match for '+cc.systemUri+'|'+cc.version+' in '+FOthers.AsText, itUnknown);
              checkCanonicalStatus(path, op, checker.FValueSet, FValueSet);
              excluded := excluded and (checker.check(path, system, version, code, abstractOk, inferSystem, displays, unknownSystems, message, ver, inactive, normalForm, vstatus, cause, op, nil, params, contentMode, impliedSystem, unkCodes, messages) = bTrue);
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
              ver := cs.version;
              contentMode := cs.contentMode;
              if ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri = system)) and checkExpansion(path, cs, ccc, code, abstractOk, displays, FValueSet, message, inactive, vstatus, op) then
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
  vstatus, normalForm : String;
begin
  FOpContext.clearContexts;
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
        ok := check(path, coding.systemUri, coding.version, coding.code, abstractOk, inferSystem, list, unknownSystems, message, ver, inactive, normalForm, vstatus, cause, op, nil, result, contentMode, impliedSystem, unkCodes, messages);
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
    deadCheck('valueSetDependsOnCodeSystem');
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
  psys, pver, pdisp, pcode, us, baseMsg, p, normalForm : String;
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
  FOpContext.clearContexts;
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
            deadCheck('check-b#1');
            if (issuePath = 'CodeableConcept') then
              path := addToPath(issuePath, 'coding['+inttostr(i)+']')
            else
              path := issuePath;
            list.clear;
            v := check(path, c.systemUri, c.version, c.code, abstractOk, inferSystem, list, unknownSystems, message, ver, inactive, normalForm, vstatus, cause, op, vcc, result, contentMode, impliedSystem, ts, mt);
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
              else if (c.display <> '') and (list.designations.count > 0) and (not list.hasDisplay(FParams.languages, c.display, dcsCaseInsensitive, diff)) then
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
                    ['', c.systemUri, c.code, list.present(FParams.languages, false), c.display, FParams.langSummary])
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
                     if (prov2 = nil) and (c.version = '') then
                     begin
                       m := FI18n.translate('UNKNOWN_CODESYSTEM', FParams.languages, [ws]);
                       badd := unknownSystems.IndexOf(ws) = -1;
                       if (bAdd) then
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
                       m := FI18n.translate('UNABLE_TO_CHECK_IF_THE_PROVIDED_CODES_ARE_IN_THE_VALUE_SET_CS', FParams.languages, [FValueSet.vurl, ws+'|'+c.version]);
                       msg(m);
                       op.addIssue(isWarning, itNotFound, '', m, oicVSProcessing);
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
                     vcc.removeCoding(prov.systemUri, prov.version, c.code);
                     vs := ws+'|'+prov.version+'#'+c.code;
                     if ts.indexOf(vs) = -1 then
                     begin
                       ts.add(vs);
                       m := FI18N.translate('Unknown_Code_in_Version', FParams.languages, [c.code, ws, prov.version]);
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
                     if (c.display <> '') and (list.designations.Count > 0) and (not list.hasDisplay(FParams.languages, c.display, dcsCaseInsensitive, diff)) then
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
                           ['', prov.systemUri, c.code, list.present(FParams.languages, true), c.display, FParams.langSummary])
                       end
                       else if dc = 1 then
                         m := FI18n.translate(baseMsg+'_one', FParams.languages,
                           ['', prov.systemUri, c.code, list.present(FParams.languages, true), c.display, FParams.langSummary])
                       else
                         m := FI18n.translate(baseMsg+'_other', FParams.languages,
                          [inttostr(dc), prov.systemUri, c.code, list.present(FParams.languages, true), c.display, FParams.langSummary]);
                       msg(m);
                       op.addIssue(severity, itInvalid, addToPath(path, 'display'), m, oicDisplay);
                     end;
                     if (prov.version <> '') then
                       result.addParamStr('version', prov.version);
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
            else if (issuePath = '') then
              p := 'code'
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
            if (ok = bFalse) then
              result.addParamCanonical('x-unknown-system', us)
            else
              result.addParamCanonical('x-caused-by-unknown-system', us);
        if normalForm <> '' then
          result.addParamCode('normalized-code', normalForm);

        if (pcode <>'') then
          result.addParamCode('code', pcode)
        else if (tcode <> '') and (mode <> vcmCodeableConcept) then
          result.addParamCode('code', tcode);
        if (pver <> '') then
          result.addParamStr('version', pver)
        else if (tver <> '') and (mode <> vcmCodeableConcept) then
          result.addParamStr('version', tver);

        if (pdisp <> '') and ((pcode <>'') or ((tcode <> '') and (mode <> vcmCodeableConcept))) then
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
  vstatus, normalForm : String;
begin
  FOpContext.clearContexts;
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
          ok := check(issuePath, system, version, code, true, inferSystem, list, unknownSystems, message, ver, inactive, normalForm, vstatus, cause, op, nil, result, contentMode, impliedSystem, unkCodes, messages);
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

function TValueSetChecker.checkConceptSet(path : String; cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code: String; abstractOk : boolean; displays : TConceptDesignations; vs : TFHIRValueSetW; var message : String; var inactive : boolean; var normalForm : String; var vstatus : String; op : TFHIROperationOutcomeW; vcc : TFHIRCodeableConceptW): boolean;
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
          op.addIssue(isError, itCodeInvalid, addToPath(path, 'code'), FI18n.translate('Unknown_Code_in_Version', FParams.languages, [code, cs.systemUri, cs.version]), oicInvalidCode)
      end
      else if not (abstractOk or not cs.IsAbstract(loc)) then
      begin
        if (not FParams.membershipOnly) then
          op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri, code]), oicCodeRule)
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
        if (cs.Code(loc) <> code) then
        begin
          if (cs.version <> '') then
            msg := FI18n.translate('CODE_CASE_DIFFERENCE', FParams.languages, [code, cs.Code(loc), cs.systemUri+'|'+cs.version])
          else
            msg := FI18n.translate('CODE_CASE_DIFFERENCE', FParams.languages, [code, cs.Code(loc), cs.systemUri]);
          op.addIssue(isInformation, itBusinessRule, addToPath(path, 'code'), msg, oicCodeRule);
          normalForm := cs.Code(loc);
        end;
        msg := cs.incompleteValidationMessage(loc, FParams.languages);
        if (msg <> '') then
          op.addIssue(isInformation, itInformational, addToPath(path, 'code'), msg, oicProcessingNote);
        listDisplays(displays, cs, loc);
        inactive := cs.IsInactive(loc);
        if (inactive) then
          vstatus := cs.getCodeStatus(loc);

        if vcc <> nil then
          vcc.addCoding(cs.systemUri, cs.version, cs.code(loc), displays.preferredDisplay(FParams.languages));
        exit;
      end;
    finally
      loc.free;
    end;
  end;

  for cc in cset.concepts.forEnum do
  begin
    deadCheck('checkConceptSet#1');
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
              op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri, code]), oicCodeRule)
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
              vcc.addCoding(cs.systemUri, cs.version, cs.code(loc), displays.preferredDisplay(FParams.languages));
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
          deadCheck('checkConceptSet#2');
          // gg - why? if ('concept' = fc.property_) and (fc.Op = FilterOperatorIsA) then
          f := cs.filter(false, fc.prop, fc.Op, fc.value, prep);
          if f = nil then
            raise ETerminologyError.create('The filter "'+fc.prop +' '+ CODES_TFhirFilterOperator[fc.Op]+ ' '+fc.value+'" from the value set '+vs.vurl+' was not understood in the context of '+cs.systemUri, itNotSupported);
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
                  op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri, code]), oicCodeRule)
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
                  vcc.addCoding(cs.systemUri, cs.version, cs.code(loc), displays.preferredDisplay(FParams.languages));
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
            deadCheck('checkConceptSet#3');
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
                      op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri, code]), oicCodeRule)
                  end
                  else
                  begin    
                    if vcc <> nil then
                      vcc.addCoding(cs.systemUri, cs.version, cs.code(loc), displays.preferredDisplay(FParams.languages));
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
                        op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri, code]), oicCodeRule)
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
                        vcc.addCoding(cs.systemUri, cs.version, cs.code(loc), displays.preferredDisplay(FParams.languages));
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
                      op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri, code]), oicCodeRule)
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
                      vcc.addCoding(cs.systemUri, cs.version, cs.code(loc), displays.preferredDisplay(FParams.languages));
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
        op.addIssue(isError, itCodeInvalid, addToPath(path, 'code'), FI18n.translate('Unknown_Code_in_Version', FParams.languages, [code, cs.systemUri, cs.version]), oicInvalidCode)
    end
    else if not (abstractOk or not cs.IsAbstract(loc)) then
    begin
      if (not FParams.membershipOnly) then
        op.addIssue(isError, itBusinessRule, addToPath(path, 'code'), FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.languages, [cs.systemUri, code]), oicCodeRule)
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
  params: TFHIRTxOperationParams; textFilter: String; dependencies: TStringList;
  limit, count, offset: integer; noCacheThisOne : boolean): TFHIRValueSetW;
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
  s : String;
begin
  FNoCacheThisOne := noCacheThisOne;

  FTotalStatus := tsUninitialised;
  FTotal := 0;
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
    FLimitCount := INTERNAL_LIMIT;
    if limit <= 0 then
    begin
      if not filter.null then
        limit := UPPER_LIMIT_TEXT
      else
        limit := UPPER_LIMIT_NO_TEXT;
    end;
    FOffset := offset;
    FCount := count;

    if (offset > 0) then
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
    if offset > -1 then
    begin
      exp.addParamInt('offset', offset);
      exp.offset := offset;
    end;
    if count > -1 then
    begin
      exp.addParamInt('count', count);
    end;
    if (count > 0) and (offset = -1) then
      offset := 0;

    DeadCheck('expand');
    try
      ics := source.inlineCS;
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
        raise ETerminologyError.create(FI18n.translatePlural(FRequiredSupplements.Count, 'VALUESET_SUPPLEMENT_MISSING', FParams.languages, [FRequiredSupplements.commaText]), itBusinessRule);

    except
      on e : EFinished do
      begin
        // nothing - we're just trapping this
        if FTotalStatus = tsUninitialised then
          FTotalStatus := tsOff; // ?
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
      if (FTotalStatus = tsOff) or (FTotal = -1) then
        FCanBeHierarchy := false
      else if (FTotal > 0) then
        exp.Total := FTotal
      else
        exp.Total := FFullList.count;
      if (FCanBeHierarchy) and ((count <= 0) or (count > FFullList.count)) then // no need to consider offset - it must be 0 if FCanBeHierarchy
        list := FRootList
      else
      begin
        list := FFullList;
        for c in FFullList do
          c.clearContains();
      end;
    end;

    if (offset + count < 0) and (FFullList.count > limit) then
    begin
      raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY_COUNT', FParams.languages, [source.vurl, '>'+inttostr(limit), inttostr(FFullList.count)]));
    end
    else
    begin
      t := 0;
      o := 0;
      for i := 0 to list.count - 1 do
      begin
        deadCheck('expand#1');
        c := list[i];
        if FMap.containsKey(key(c)) then
        begin
          inc(o);
          if (o > offset) and ((count <= 0) or (t < count)) then
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

function TFHIRValueSetExpander.isExcluded(system, version, code: String): boolean;
begin
  result := FExcluded.contains(system+'|'+version+'#'+code);
end;

procedure TFHIRValueSetExpander.NoTotal;
begin
  FTotal := -1;
  FTotalStatus := tsOff;
end;

procedure TFHIRValueSetExpander.AddToTotal(t: Integer);
begin
  if (FTotal > -1) and (FTotalStatus <> tsOff) then
  begin
    inc(FTotal, t);
    FTotalStatus := tsSet;
  end;
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
  if (cs.version <> '') then
    checkCanonicalStatus(expansion, cs.systemUri+'|'+cs.version, status, standardsStatus, experimental, source)
  else
    checkCanonicalStatus(expansion, cs.systemUri, status, standardsStatus, experimental, source);
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
    deadCheck('makeFilterForValueSet');
    if inc.systemUri = '' then
      exit; // no short cuts if there's further value set references
    if inc.systemUri = cs.systemUri then
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
        begin
          deadCheck('makeFilterForValueSet#2');
          TSpecialProviderFilterContextConcepts(result).add(cs.locate(cc.code, nil, message));
        end;
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
    deadCheck('handleCompose#1');
    vs := expandValueSet(s, '', filter.filter, dependencies, notClosed);
    try
      checkCanonicalStatus(expansion, vs, FValueSet);
      importValueSet(vs, expansion, nil, 0);
    finally
      vs.free;
    end;
  end;

  for c in source.includes.forEnum do
  begin
    deadCheck('handleCompose#2');
    checkSource(c, expansion, filter, source.url);
  end;
  for c in source.excludes.forEnum do
  begin
    deadCheck('handleCompose#3');
    FHasExclusions := true;
    checkSource(c, expansion, filter, source.url);
  end;

  for c in source.excludes.forEnum do
  begin
    deadCheck('handleCompose#4');
    excludeCodes(c, source, filter, dependencies, expansion, source.excludeInactives,notClosed);
  end;
  for c in source.includes.forEnum do
  begin
    deadCheck('handleCompose#5');
    includeCodes(c, source, filter, dependencies, expansion, source.excludeInactives, notClosed);
  end;
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
    deadCheck('handleDefine');
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
begin
  if (c.display <> '') then
  begin
    if (FFactory.version in [fhirVersionRelease2, fhirVersionRelease3]) then // this policy changed in R4
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
  id : String;
  fn : String;
  comp : TFHIRComposer;
  m : TFHIRMetadataResourceW;
  b : TFslBytesBuilder;
  time : integer;
begin
  SetThreadStatus(ClassName+'.'+place);
  if FOpContext.deadCheck(time) then
  begin
    {$IFDEF DUMP_DEAD_VS}
    id := FOpContext.FId;
    if (id = '') then
      id := 'internal';

    fn := FilePath(['[tmp]', 'vs-dead-'+id+'.json']);
    b := TFslbytesBuilder.create;
    try
      comp := FFactory.makeComposer(nil, ffJson, FLangList, OutputStylePretty);
      try
        b.append(comp.ComposeBytes(FValueSet.Resource));
        for m in FAdditionalResources do
          b.append(comp.ComposeBytes(m.Resource));
      finally
        comp.free;
      end;
      BytesToFile(b.AsBytes, fn);
    finally
      b.free;
    end;
    logging.log('Expansion took too long @ '+place+': value set '+FValueSet.vurl+' stored at '+fn+'.json');
    {$ELSE}
    logging.log('Expansion took too long');
    {$ENDIF}
    raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY_TIME', FParams.languages, [FValueSet.vurl, inttostr(time)]));
  end;
end;

constructor TFHIRValueSetExpander.Create(factory: TFHIRFactory; opContext: TTerminologyOperationContext; getVS: TGetValueSetEvent; getCS: TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion: TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  inherited create(factory, opContext, getVS, getCS, getVersions, getExpansion, txResources, languages, i18n);
  FCSCounter := TFslMap<TValueSetCounter>.create;
  FCSCounter.defaultValue := nil;
  FExcluded := TFslStringSet.create;
end;

destructor TFHIRValueSetExpander.Destroy;
begin
  FExcluded.free;
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
      n := includeCode(nil, parent, system, '', c.Code, cs.isAbstract(c), cs.isInactive(c), cs.isDeprecated(c),  cds, c.definition, c.itemWeight,
         nil, imports, c.getAllExtensionsW, nil, c.properties, nil, excludeInactive, srcUrl);
    finally
      cds.free;
    end;
  end;
  for i := 0 to c.conceptList.count - 1 do
  begin
    deadCheck('addDefinedCode');
    addDefinedCode(cs, system, c.conceptList[i], imports, n, excludeInactive, srcUrl);
  end;
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

function getPropUrl(cs : TCodeSystemProvider; pcode : String) : String;
var
  pl : TFslList<TFhirCodeSystemPropertyW>;
  p : TFhirCodeSystemPropertyW;
begin
  result := '';
  pl := cs.getPropertyDefinitions;
  try
    if pl <> nil then
      for p in pl do
        if p.code = pcode then
          exit(p.uri);
  finally
    pl.free;
  end;
end;

function TFHIRValueSetExpander.includeCode(cs : TCodeSystemProvider; parent : TFhirValueSetExpansionContainsW; system, version, code : String;
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
  result := nil;
  deadCheck('processCode');
  try
    if not passesImports(imports, system, code, 0) then
      exit;
    if isInactive and excludeInactive then
      exit;
    if isExcluded(system, version, code) then
      exit;


    if (cs <> nil) and (cs.expandLimitation > 0) then
    begin
      cnt := FCSCounter[cs.systemUri];
      if (cnt = nil) then
      begin
        cnt := TValueSetCounter.create;
        FCSCounter.Add(cs.systemUri, cnt);
      end;
      cnt.increment;
      if (cnt.count > cs.expandLimitation) then
        exit;
    end;

    if (FLimitCount > 0) and (FFullList.Count >= FLimitCount) and not FHasExclusions then
    begin
      if (FCount > -1) and (FOffset > -1) and (FCount + FOffset > 0) and (FFullList.count >= FCount + FOffset) then
        raise EFinished.create('.')
      else
      begin
        if (srcUrl = '') then
          srcUrl := '??';
        raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.languages, [srcUrl, '>'+inttostr(FLimitCount)]));
      end;
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

    if not FMap.containsKey(s) then
    begin
      n := FFactory.makeValueSetContains;
      try
        n.systemUri := system;
        n.Code := code;
        if isAbstract then
          n.abstract_ := isAbstract;
        if isInactive or deprecated then
          n.inactive := true;

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
        if (pref <> nil) and (pref.value <> nil) then
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
                expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#definition', pn, vstr.link);
              finally
                vstr.free;
              end;
            end;
          end
          else if (csProps <> nil) and (cs <> nil) then
          begin
            for cp in csprops do
            begin
              if cp.code = pn then
              begin
                expansion.defineProperty(n, getPropUrl(cs, pn), pn, cp.value.link);
                // n.addProperty(pn, cp);
              end;
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

        if not FMap.ContainsKey(s) then
        begin
          FFullList.add(n.link);
          FMap.add(s, n.link);
          if (parent <> nil) then
            parent.addContains(n)
          else
            FRootList.add(n.link);
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

procedure TFHIRValueSetExpander.excludeCode(cs : TCodeSystemProvider; system, version, code : String; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; srcURL : string);
var
  s, vs : String;
  ts : TStringList;
begin
  deadCheck('excludeCode');
  if not passesImports(imports, system, code, 0) then
    exit;

  //if (cs <> nil) and (cs.expandLimitation > 0) then
  //begin
  //  cnt := FCSCounter[cs.systemUri];
  //  if (cnt = nil) then
  //  begin
  //    cnt := TValueSetCounter.create;
  //    FCSCounter.Add(cs.systemUri, cnt);
  //  end;
  //  cnt.increment;
  //  if (cnt.count > cs.expandLimitation) then
  //    exit;
  //end;

  if (FLimitCount > 0) and (FFullList.Count >= FLimitCount) and not FHasExclusions then
  begin
    if (FCount > -1) and (FOffset > -1) and (FCount + FOffset > 0) and (FFullList.count >= FCount + FOffset) then
      raise EFinished.create('.')
    else
    begin
      if (srcUrl = '') then
        srcUrl := '??';
      raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.languages, [srcUrl, '>'+inttostr(FLimitCount)]));
    end;
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

  FExcluded.add(system+'|'+version+'#'+code);
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

function TFHIRValueSetExpander.expandValueSet(uri, version, filter: String; dependencies: TStringList; var notClosed: boolean): TFHIRValueSetW;
var
  dep : TStringList;
  exp : TFhirValueSetExpansionW;
begin
  dep := TStringList.Create;
  try
    result := FOnGetExpansion(self, FOpContext, uri, version, filter, FParams, dep, FAdditionalResources , -1, FNoCacheThisOne);
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
    if (p <> nil) then
      p.addContains(c)
    else
      FRootList.add(c.link);
    FMap.add(s, c.link);
  end;
  for cc in c.contains.forEnum do
  begin
    deadCheck('importValueSetItem');
    importValueSetItem(c, cc, imports, offset);
  end;
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
  FFactory.checkNoModifiers(cset,'ValueSetExpander.checkSource', 'set');
  imp := false;
  for s in cset.valueSets do
  begin
    deadCheck('checkSource');
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
          exp.addParamUri(CODES_TFhirCodeSystemContentMode[cs.contentMode], cs.systemUri+'|'+cs.version)
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
              raise ETooCostly.create('The code System "'+cs.systemUri+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
            else
              raise ETooCostly.create('The code System "'+cs.systemUri+'" has a grammar, and cannot be enumerated directly');

          if not imp and (FLimitCount > 0) and (cs.TotalCount > FLimitCount) and not (FParams.limitedExpansion) then
            raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.languages, [srcUrl, '>'+inttostr(FLimitCount)]));
        end
      end;

    finally
      cs.free;
    end;
  end;
end;

procedure TFHIRValueSetExpander.includeCodes(cset: TFhirValueSetComposeIncludeW; vsSrc : TFHIRValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; excludeInactive : boolean; var notClosed : boolean);
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
  s, display, ov, code, vsId, sv : String;
  valueSets : TFslList<TFHIRImportedValueSet>;
  base : TFHIRValueSetW;
  cc : TFhirValueSetComposeIncludeConceptW;
  cctxt : TCodeSystemProviderContext;
  cds : TConceptDesignations;
  iter : TCodeSystemIteratorContext;
  vs : TFHIRValueSetW;
  parent : TFhirValueSetExpansionContainsW;
  ivs : TFHIRImportedValueSet;
  tcount : integer;
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
          sv := canonical(cs.systemUri, cs.version);
          if not expansion.hasParam('used-codesystem', sv) then
            expansion.addParamUri('used-codesystem', sv);
          if not expansion.hasParam('version', sv) then
            expansion.addParamUri('version', sv);

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
                  raise ETooCostly.create('The code System "'+cs.systemUri+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
                else
                  raise ETooCostly.create('The code System "'+cs.systemUri+'" has a grammar, and cannot be enumerated directly');

              iter := cs.getIterator(nil);
              try
                if valueSets.Empty and (FLimitCount > 0) and (iter.count > FLimitCount) and not (FParams.limitedExpansion) then
                  raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.languages, [vsSrc.url, '>'+inttostr(FLimitCount)]));
                tcount := 0;
                while iter.more do
                begin                
                  deadCheck('processCodes#3a');
                  c := cs.getNextContext(iter);
                  try
                    if passesFilters(c, 0) then
                      inc(tcount, includeCodeAndDescendants(cs, c, expansion, valueSets, nil, excludeInactive, vsSrc.url));
                  finally
                    c.free;
                  end;
                end;
              finally
                iter.free;
              end;
              AddToTotal(tcount);
            end
            else
            begin
              NoTotal;
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
                          includeCode(cs, nil, cs.systemUri, cs.version, cs.code(c),  cs.isAbstract(c), cs.isInactive(c), cs.deprecated(c),
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

          if (cset.hasConcepts) then
          begin
            cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
            try
              tcount := 0;
              for cc in cset.concepts.forEnum do
              begin
                deadCheck('processCodes#3');
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
                      inc(tcount);
                      ov := cc.itemWeight;
                      if ov = '' then
                        ov := cs.itemWeight(cctxt);
                      includeCode(cs, nil, cs.systemUri, cs.version, cc.code, cs.isAbstract(cctxt), cs.isInactive(cctxt), cs.deprecated(cctxt), cds,
                           cs.Definition(cctxt), ov, expansion, valueSets, cs.getExtensions(cctxt), cc.getAllExtensionsW, cs.getProperties(cctxt), nil, excludeInactive, vsSrc.url);
                    end;
                  end;
                finally
                  cctxt.free;
                end;
              end;
              AddToTotal(tcount);
            finally
              cds.free;
            end;
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
                  deadCheck('processCodes#4a');
                  fc := fcl[i];
                  ffactory.checkNoModifiers(fc, 'ValueSetExpander.processCodes', 'filter');
                  f := cs.filter(i = 0, fc.prop, fc.Op, fc.value, prep);
                  if f = nil then
                    raise ETerminologyError.create('The filter "'+fc.prop +' '+ CODES_TFhirFilterOperator[fc.Op]+ ' '+fc.value+'" from the value set '+vsSrc.url+' was not understood in the context of '+cs.systemUri, itNotSupported);
                  filters.Insert(offset, f);
                  if cs.isNotClosed(filter, f) then
                    notClosed := true;
                  if (cset.filterCount = 1) and (not excludeInactive) and not FParams.activeOnly then
                    AddToTotal(cs.filterSize(f));
                  //else
                    //NoTotal;
                end;

                inner := cs.prepare(prep);
                count := 0;
                While cs.FilterMore(filters[0]) do
                begin
                  deadCheck('processCodes#5');
                  c := cs.FilterConcept(filters[0]);
                  try
                    ok := (not FParams.activeOnly or not cs.IsInactive(c)) and (inner or passesFilters(c, 1));
                    if ok then
                    begin
                      inc(count);
                      cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
                      try
                        if passesImports(valueSets, cs.systemUri, cs.code(c), 0) then
                        begin
                          listDisplays(cds, cs, c);
                          if cs.canParent then
                            parent := FMap[key(cs.systemUri, cs.parent(c))]
                          else
                          begin
                            FCanBeHierarchy := false;
                            parent := nil;
                          end;
                          for code in cs.listCodes(c, FParams.altCodeRules) do
                            includeCode(cs, parent, cs.systemUri, cs.version, code, cs.isAbstract(c), cs.IsInactive(c),
                              cs.deprecated(c), cds, cs.definition(c), cs.itemWeight(c), expansion, nil, cs.getExtensions(c), nil, cs.getProperties(c), nil, excludeInactive, vsSrc.url);
                        end;
                      finally
                        cds.free;
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

procedure TFHIRValueSetExpander.excludeCodes(cset: TFhirValueSetComposeIncludeW; vsSrc : TFHIRValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; excludeInactive : boolean; var notClosed : boolean);
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
  s, display, ov, code, vsId, sv : String;
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
      NoTotal;
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
      excludeValueSet(valueSets[0].valueSet, expansion, valueSets, 1);
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
          sv := canonical(cs.systemUri, cs.version);
          if not expansion.hasParam('used-codesystem', sv) then
            expansion.addParamUri('used-codesystem', sv);
          if not expansion.hasParam('version', sv) then
            expansion.addParamUri('version', sv);

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
                excludeValueSet(base, expansion, valueSets, 0);
              finally
                base.free;
              end;
              notClosed := true;
            end
            else if filter.Null then // special case - add all the code system
            begin
              if cs.isNotClosed(filter) then
                if cs.SpecialEnumeration <> '' then
                  raise ETooCostly.create('The code System "'+cs.systemUri+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
                else
                  raise ETooCostly.create('The code System "'+cs.systemUri+'" has a grammar, and cannot be enumerated directly');

              iter := cs.getIterator(nil);
              try
                if valueSets.Empty and (FLimitCount > 0) and (iter.count > FLimitCount) and not (FParams.limitedExpansion) then
                  raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.languages, [vsSrc.url, '>'+inttostr(FLimitCount)]));
                while iter.more do
                begin
                  deadCheck('processCodes#3a');
                  c := cs.getNextContext(iter);
                  try
                    if passesFilters(c, 0) then
                      excludeCodeAndDescendants(cs, c, expansion, valueSets, excludeInactive, vsSrc.url);
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
              NoTotal;
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
                          excludeCode(cs, cs.systemUri, cs.version, cs.code(c),  expansion, valueSets, vsSrc.url);
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

          if (cset.hasConcepts) then
          begin
            cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
            try
              for cc in cset.concepts.forEnum do
              begin
                deadCheck('processCodes#3');
                cds.Clear;
                FFactory.checkNoModifiers(cc, 'ValueSetExpander.processCodes', 'set concept reference');
                cctxt := cs.locate(cc.code, FAllAltCodes);
                try
                  if (cctxt <> nil) and (not FParams.activeOnly or not cs.IsInactive(cctxt)) and passesFilters(cctxt, 0) then
                  begin
                    if filter.passes(cds) or filter.passes(cc.code) then
                    begin
                      ov := cc.itemWeight;
                      if ov = '' then
                        ov := cs.itemWeight(cctxt);
                      excludeCode(cs, cs.systemUri, cs.version, cc.code, expansion, valueSets, vsSrc.url);
                    end;
                  end;
                finally
                  cctxt.free;
                end;
              end;
            finally
              cds.free;
            end;
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
                  deadCheck('processCodes#4a');
                  fc := fcl[i];
                  ffactory.checkNoModifiers(fc, 'ValueSetExpander.processCodes', 'filter');
                  f := cs.filter(i = 0, fc.prop, fc.Op, fc.value, prep);
                  if f = nil then
                    raise ETerminologyError.create('The filter "'+fc.prop +' '+ CODES_TFhirFilterOperator[fc.Op]+ ' '+fc.value+'" from the value set '+vsSrc.url+' was not understood in the context of '+cs.systemUri, itNotSupported);
                  filters.Insert(offset, f);
                  if cs.isNotClosed(filter, f) then
                    notClosed := true;
                end;

                inner := cs.prepare(prep);
                count := 0;
                While cs.FilterMore(filters[0]) do
                begin
                  deadCheck('processCodes#5');
                  c := cs.FilterConcept(filters[0]);
                  try
                    ok := (not FParams.activeOnly or not cs.IsInactive(c)) and (inner or passesFilters(c, 1));
                    if ok then
                    begin
                      inc(count);
                      cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
                      try
                        if passesImports(valueSets, cs.systemUri, cs.code(c), 0) then
                        begin
                          listDisplays(cds, cs, c);
                          if cs.canParent then
                            parent := FMap[key(cs.systemUri, cs.parent(c))]
                          else
                          begin
                            FCanBeHierarchy := false;
                            parent := nil;
                          end;
                          for code in cs.listCodes(c, FParams.altCodeRules) do
                            excludeCode(cs, cs.systemUri, cs.version, code, expansion, nil, vsSrc.url);
                        end;
                      finally
                        cds.free;
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

function TFHIRValueSetExpander.includeCodeAndDescendants(cs: TCodeSystemProvider; context: TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; excludeInactive : boolean; srcUrl : String) : integer;
var
  i : integer;
  vs, code : String;
  cds : TConceptDesignations;
  iter : TCodeSystemIteratorContext;
  n, t : TFhirValueSetExpansionContainsW;
  ts : TStringList;
  c : TCodeSystemProviderContext;
begin
  result := 0;
  deadCheck('processCodeAndDescendants');
  if (expansion <> nil) then
  begin
    vs := canonical(cs.systemUri, cs.version);
    if not expansion.hasParam('used-codesystem', vs) then
      expansion.addParamUri('used-codesystem', vs);
    if not expansion.hasParam('version', vs) then
      expansion.addParamUri('version', vs);
    ts := TStringList.create;
    try
      cs.listSupplements(ts);
      for vs in ts do
      begin
        deadCheck('processCodeAndDescendants');
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
      listDisplays(cds, cs, context);
      for code in cs.listCodes(context, FParams.altCodeRules) do
      begin
        deadCheck('processCodeAndDescendants#2');
        t := includeCode(cs, parent, cs.systemUri, cs.version, code, cs.isAbstract(context), cs.IsInactive(context), cs.deprecated(context), cds, cs.definition(context),
           cs.itemWeight(context), expansion, imports, cs.getExtensions(context), nil, cs.getProperties(context), nil, excludeInactive, srcUrl);
        if (t <> nil) then
          inc(result);
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
        inc(result, includeCodeAndDescendants(cs, c, expansion, imports, n, excludeInactive, srcUrl));
      finally
        c.free;
      end;
    end;
  finally
    iter.free;
  end;
end;

procedure TFHIRValueSetExpander.excludeCodeAndDescendants(cs: TCodeSystemProvider; context: TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; excludeInactive : boolean; srcUrl : String);
var
  i : integer;
  vs, code : String;
  cds : TConceptDesignations;
  iter : TCodeSystemIteratorContext;
  ts : TStringList;
  c : TCodeSystemProviderContext;
begin
  deadCheck('processCodeAndDescendants');
  if (expansion <> nil) then
  begin
    vs := canonical(cs.systemUri, cs.version);
    if not expansion.hasParam('used-codesystem', vs) then
      expansion.addParamUri('used-codesystem', vs);
    if not expansion.hasParam('version', vs) then
      expansion.addParamUri('version', vs);
    ts := TStringList.create;
    try
      cs.listSupplements(ts);
      for vs in ts do
      begin
        deadCheck('processCodeAndDescendants');
        if not expansion.hasParam('used-supplement', vs) then
          expansion.addParamUri('used-supplement', vs);
        if not expansion.hasParam('version', vs) then
          expansion.addParamUri('version', vs);
      end;
    finally
      ts.free;
    end;
  end;
  if (not FParams.excludeNotForUI or not cs.IsAbstract(context)) and (not FParams.activeOnly or not cs.isInActive(context)) then
  begin
    cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
    try
      listDisplays(cds, cs, context);
      for code in cs.listCodes(context, FParams.altCodeRules) do
      begin
        deadCheck('processCodeAndDescendants#2');
        excludeCode(cs, cs.systemUri, cs.version, code, expansion, imports, srcUrl);
      end;
    finally
      cds.free;
    end;
  end;
  iter := cs.getIterator(context);
  try
    while iter.more do
    begin
      deadCheck('processCodeAndDescendants#3');
      c := cs.getNextContext(iter);
      try
        excludeCodeAndDescendants(cs, c, expansion, imports, excludeInactive, srcUrl);
      finally
        c.free;
      end;
    end;
  finally
    iter.free;
  end;
end;

{ TFHIRConceptMapTranslator }

function TFHIRConceptMapTranslator.checkCode(op: TFhirOperationOutcomeW; langList: THTTPLanguageList; path: string; code: string; system, version: string; display: string): boolean;
var
  cs : TFhirCodeSystemW;
  cp : TCodeSystemProvider;
  lct : TCodeSystemProviderContext;
  def : TFhirCodeSystemConceptW;
  d : String;
begin
  result := false;
  cp := findCodeSystem(system, version, nil, true);
  if cp <> nil then
  begin
    try
      lct := cp.locate(code);
      try
        if (op.error('InstanceValidator', itInvalid, path, lct <> nil, 'Unknown Code ('+system+'#'+code+')')) then
          result := op.warning('InstanceValidator', itInvalid, path, (display = '') or (display = cp.Display(lct, THTTPLanguageList(nil))),
          'Display for '+system+' code "'+code+'" should be "'+cp.Display(lct, THTTPLanguageList(nil))+'"');
      finally
        lct.free;
      end;
    finally
      cp.free;
    end;
  end;
end;

constructor TFHIRConceptMapTranslator.Create(factory: TFHIRFactory; opContext: TTerminologyOperationContext; getVS: TGetValueSetEvent; getCS: TGetProviderEvent; getVersions: TGetSystemVersionsEvent; getExpansion: TGetExpansionEvent; txResources: TFslMetadataResourceList; languages: TIETFLanguageDefinitions; i18n: TI18nSupport);
begin
  inherited create(factory, opContext, getVS, getCS, getVersions, getExpansion, txResources, languages, i18n);
end;

destructor TFHIRConceptMapTranslator.Destroy;
begin
  inherited Destroy;
end;

function TFHIRConceptMapTranslator.translate(langList: THTTPLanguageList; reqId : String; cml : TFslList<TFHIRConceptMapW>; coding: TFHIRCodingW; params: TFhirParametersW; profile: TFhirTxOperationParams): TFhirParametersW;
var
  cm : TFHIRConceptMapW;
  g : TFhirConceptMapGroupW;
  em : TFhirConceptMapGroupElementW;
  map : TFhirConceptMapGroupElementTargetW;
  outcome : TFHIRCodingW;
  p, pp :  TFhirParametersParameterW;
  prod : TFhirConceptMapGroupElementDependsOnW;
  added : boolean;
  msg : String;
begin
  result := FFactory.wrapParams(FFactory.makeResource('Parameters'));
  try
    try     
      added := false;
      for cm in cml do
      begin
        //else if not checkCode(op, langList, '', coding.code, coding.systemUri, coding.version, coding.display) then
        //  raise ETerminologyError.Create('Code '+coding.code+' in system '+coding.systemUri+' not recognized', itUnknown);
        if isOkSource(cm, coding, g, em) then
        begin
          try
            for map in em.targets.forEnum do
            begin
              if (map.equivalence in [cmeNull, cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact]) then
              begin
                result.AddParamBool('result', true);
                added := true;
                outcome := FFactory.wrapCoding(FFactory.makeByName('Coding'));
                try
                  p := result.AddParam('match');
                  outcome.systemUri := g.target;
                  outcome.code := map.code;
                  p.AddParam('concept', outcome.Element.Link);
                  p.addParamCode('equivalence', CODES_TFHIRConceptEquivalence[map.equivalence]);
                  if (map.comments <> '') then
                    p.addParamStr('message', map.comments);
                  for prod in map.products.forEnum do
                  begin
                    pp := p.addParam('product');
                    pp.addParamStr('element', prod.property_);
                    pp.addParam('concept').value := FFactory.makeCoding(prod.system_, prod.value);
                  end;
                finally
                  outcome.free;
                end;
              end;
            end;
          finally
            em.free;
            g.free;
          end;
        end;
      end;
      if not added then
      begin
        result.AddParamBool('result', false);
        result.AddParamStr('message', 'No translations found');
      end;
    except
      on e : exception do
      begin
        result.AddParamBool('result', false);
        result.AddParamStr('message', e.message);
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRConceptMapTranslator.isOkTarget(cm: TFhirConceptMapW;
  vs: TFhirValueSetW): boolean;
begin
  //if cm.Target <> nil then
  //  result := cm.Target.url = vs.url
  //else
    result := false;
  // todo: or it might be ok to use this value set if it's a subset of the specified one?
end;

function TFHIRConceptMapTranslator.isOkSource(cm: TFhirConceptMapW;
  vs: TFhirValueSetW; coding: TFHIRCodingW; out group: TFhirConceptMapGroupW;
  out match: TFhirConceptMapGroupElementW): boolean;
var
  g : TFhirConceptMapGroupW;
  em : TFhirConceptMapGroupElementW;
begin
  result := false;
  if true {(vs = nil) or ((cm.source <> nil) and (cm.Source.url = vs.url))} then
  begin
    for g in cm.groups.forEnum do
      for em in g.elements.forEnum do
        if (g.source = coding.systemUri) and (em.code = coding.code) then
      begin
        result := true;
        match := em.link;
        group := g.link;
      end;
  end;
end;

  
function TFHIRConceptMapTranslator.isOkSource(cm: TFhirConceptMapW;
  coding: TFHIRCodingW; out group: TFhirConceptMapGroupW; out
  match: TFhirConceptMapGroupElementW): boolean;
var
  g : TFhirConceptMapGroupW;
  em : TFhirConceptMapGroupElementW;
begin
  result := false;
  for g in cm.groups.forEnum do
    if (g.source = coding.systemUri) then
    begin
      for em in g.elements.forEnum do
        if (em.code = coding.code) then
        begin
          result := true;
          match := em.link;
          group := g.link;
        end;
    end;
end;

function TFHIRConceptMapTranslator.findConceptMap(var cm: TFhirConceptMapW; var msg: String): boolean;
begin
  msg := '';
  if cm <> nil then
    result := true
  else
  begin

  end;
end;

//function TTerminologyServer.translate(langList : THTTPLanguageList; source : TFhirValueSetW; coding : TFHIRCodingW; target : TFhirValueSetW; params : TFhirParametersW; txResources : TFslMetadataResourceList; profile : TFhirTxOperationParams) : TFhirParametersW;
//var
//  op : TFhirOperationOutcomeW;
//  list : TLoadedConceptMapList;
//  i : integer;
//  summary : string;
//  cm : TLoadedConceptMap;
//  p : TFhirParametersW;
//  g : TFhirConceptMapGroupW;
//  em : TFhirConceptMapGroupElementW;
//  map : TFhirConceptMapGroupElementTargetW;
//  outcome : TFHIRCodingW;
//begin
//  op := Factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
//  try
//    try
//      if not checkCode(op, langList, '', coding.code, coding.systemUri, coding.version, coding.display) then
//        raise ETerminologyError.Create('Code '+coding.code+' in system '+coding.systemUri+' not recognized', itUnknown);
//
//      // check to see whether the coding is already in the target value set, and if so, just return it
//      p := validate('', target, coding, nil, false, false, nil, summary);
//      try
//        if p.bool('result') then
//        begin
//          result := Factory.wrapParams(factory.makeResource('Parameters'));
//          result.addParamBool('result', true);
//          result.addParam('outcome', coding.Link);
//          result.addParamCode('equivalence', 'equal');
//          exit;
//        end;
//      finally
//        p.free;
//      end;
//
//      result := Factory.wrapParams(factory.makeResource('Parameters'));
//      list := GetConceptMapList;
//      try
//        for i := 0 to list.Count - 1 do
//        begin
//          cm := list[i];
//          if isOkTarget(cm, target) and isOkSource(cm, source, coding, g, em) then
//          try
//            if em.targetCount = 0 then
//              raise ETerminologyError.Create('Concept Map has an element with no map for '+'Code '+coding.code+' in system '+coding.systemUri, itInvalid);
//            for map in em.targets.forEnum do
//            begin
//              if (map.equivalence in [cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact]) then
//              begin
//                result.addParamBool('result', true);
//                outcome := factory.wrapCoding(factory.makeByName('Coding'));
//                result.AddParam('outcome', outcome);
//                outcome.systemUri := g.target;
//                outcome.code := map.code;
//                result.addParamCode('equivalence', CODES_TFHIRConceptEquivalence[map.equivalence]);
//                if (map.comments <> '') then
//                  result.addParamStr('message', map.comments);
//                break;
//              end
//            end;
//            exit;
//          finally
//            em.free;
//            g.free;
//          end;
//        end;
//      finally
//        list.free;
//      end;
//
//      result.AddParamBool('result', false);
//      result.AddParamStr('message', 'no match found');
//    except
//      on e : exception do
//      begin
//        result := Factory.wrapParams(factory.makeResource('Parameters'));
//        result.AddParamBool('result', false);
//        result.AddParamStr('message', e.message);
//      end;
//    end;
//  finally
//    op.free;
//  end;
//end;
//
//function TTerminologyServer.translate(langList : THTTPLanguageList; source : TFhirValueSetW; coded : TFhirCodeableConceptW; target : TFhirValueSetW; params : TFhirParametersW; txResources : TFslMetadataResourceList; profile : TFhirTxOperationParams) : TFhirParametersW;
//var
//  c : TFhirCodingW;
//begin
//  for c in coded.codings.forEnum do
//    exit(translate(langList, source, c, target, params, txResources, profile));
//  raise ETerminologyTodo.Create('TTerminologyServer.translate');
//end;
//

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



