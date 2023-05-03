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
  fhir_objects, fhir_common, ftx_service, fhir_factory, fhir_xhtml, fhir_extensions,
  fhir_codesystem_service;

{  SysUtils, Classes, fsl_utilities, fsl_utilities,
  fsl_utilities,
  fsl_collections, fsl_base,
  fhir_objects,  fhir_common, fhir_factory,
  ftx_service;
  //, ftx_loinc_services, ftx_sct_services, ftx_ucum_services, FHIR.Tx.Server, FHIR.Tx.Manager;}

const
  UPPER_LIMIT_NO_TEXT = 1000;
  UPPER_LIMIT_TEXT = 1000;// won't expand a value set bigger than this - just takes too long, and no one's going to do anything with it anyway

  FHIR_VERSION_CANONICAL_SPLIT_2 = '?version=';
  FHIR_VERSION_CANONICAL_SPLIT_3p = '|';


Type

  TFhirExpansionParamsVersionRuleMode = (fvmDefault, fvmCheck, fvmOverride);
  TValueSetValidationMode = (vsvmAllChecks, vsvmMembershipOnly, vsvmNoMembership);

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
    FLanguage : TIETFLang;
    FDisplayLanguages: TFslList<TIETFLang>;
    FexcludeNested: boolean;
    FGenerateNarrative: boolean;
    FlimitedExpansion: boolean;
    FexcludeNotForUI: boolean;
    FexcludePostCoordinated: boolean;
    FincludeDesignations: boolean;
    FincludeDefinition: boolean;
    FUid: String;
    FValueSetMode: TValueSetValidationMode;
    FDefaultToLatestVersion : boolean;
    FIncompleteOK: boolean;
    FProperties : TStringList;

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
    FHasValueSetMode : boolean;

    procedure SetLanguage(value : TIETFLang);
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
    procedure SetValueSetMode(value : TValueSetValidationMode);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRExpansionParams;

    class function defaultProfile : TFHIRExpansionParams;
    procedure loadFromLangs(languages: TIETFLanguageDefinitions; langs : THTTPLanguages);

    property versionRules : TFslList<TFhirExpansionParamsVersionRule> read FVersionRules;

    function getVersionForRule(systemURI : String; mode : TFhirExpansionParamsVersionRuleMode) : String;
    procedure seeVersionRule(url : String; mode : TFhirExpansionParamsVersionRuleMode);

    property activeOnly : boolean read FactiveOnly write SetActiveOnly;
    property language : TIETFLang read FLanguage write SetLanguage;
    function langCode : string;
    function langSummary : String;
    property displayLanguages : TFslList<TIETFLang> read FDisplayLanguages;
    property includeDefinition : boolean read FincludeDefinition write SetincludeDefinition;
    property generateNarrative : boolean read FGenerateNarrative write SetGenerateNarrative;
    property limitedExpansion : boolean read FlimitedExpansion write SetlimitedExpansion;   // deprecated
    property includeDesignations : boolean read FincludeDesignations write SetincludeDesignations;
    property excludeNested : boolean read FexcludeNested write SetexcludeNested;
    property excludeNotForUI : boolean read FexcludeNotForUI write SetexcludeNotForUI;
    property excludePostCoordinated : boolean read FexcludePostCoordinated write SetexcludePostCoordinated;
    property valueSetMode : TValueSetValidationMode read FValueSetMode write SetValueSetMode;
    property uid : String read FUid write FUid;
    property defaultToLatestVersion : boolean read FDefaultToLatestVersion write SetDefaultToLatestVersion;
    property incompleteOK : boolean read FIncompleteOK write SetIncompleteOK;
    property properties : TStringList read FProperties;

    property hasActiveOnly : boolean read FHasactiveOnly;
    property hasIncludeDefinition : boolean read FHasincludeDefinition;
    property hasGenerateNarrative : boolean read FHasGenerateNarrative;
    property hasLimitedExpansion : boolean read FHaslimitedExpansion;
    property hasIncludeDesignations : boolean read FHasincludeDesignations;
    property hasExcludeNested : boolean read FHasexcludeNested;
    property hasExcludeNotForUI : boolean read FHasexcludeNotForUI;
    property hasExcludePostCoordinated : boolean read FHasexcludePostCoordinated;
    property hasValueSetMode : boolean read FHasValueSetMode;
    property hasDefaultToLatestVersion : boolean read FHasDefaultToLatestVersion;
    property hasIncompleteOK : boolean read FHasIncompleteOK;

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

    procedure buildMap;
    function hasCode(system, code : String) : boolean;

    property valueSet : TFHIRValueSetW read FValueSet write SetValueSet;
  end;

  TGetValueSetEvent = function (sender : TObject; url : String) : TFHIRValueSetW of object;
  TGetProviderEvent = function (sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider of object;
  TGetExpansionEvent = function (sender : TObject; url, filter : String; params : TFHIRExpansionParams; dependencies : TStringList; additionalResources : TFslMetadataResourceList; limit : integer) : TFHIRValueSetW of object;
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

    function findInAdditionalResources(url, version, resourceType : String) : TFHIRMetadataResourceW;
    function findValueSet(url : String) : TFHIRValueSetW;
    function findCodeSystem(url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
    function listVersions(url : String) : String;
    procedure loadSupplements(cse: TFHIRCodeSystemEntry; url: String);
    procedure checkSupplements(cs: TCodeSystemProvider; src: TFHIRXVersionElementWrapper);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
    procedure listDisplays(displays : TConceptDesignations; cs : TCodeSystemProvider; c: TCodeSystemProviderContext); overload;
    procedure listDisplays(displays : TConceptDesignations; c: TFhirCodeSystemConceptW); overload;
    procedure listDisplays(displays: TConceptDesignations; c: TFhirValueSetComposeIncludeConceptW; vs : TFHIRValueSetW); overload;
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion : TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport); overload;
    destructor Destroy; override;
  end;

  { TValueSetChecker }

  TValueSetChecker = class (TValueSetWorker)
  private
    FOthers : TFslStringObjectMatch; // checkers or code system providers
    FValueSet : TFHIRValueSetW;
    FId: String;
    FLog : String;

    function determineSystemFromExpansion(code: String): String;
    function determineSystem(code : String) : String;
    function determineVersion(path, systemURI, versionVS, versionCoding : String; op : TFhirOperationOutcomeW; var message : String) : string;
    function check(path, system, version, code : String; abstractOk, implySystem : boolean; displays : TConceptDesignations; var message, ver : String; var cause : TFhirIssueType; op : TFhirOperationOutcomeW; var contentMode : TFhirCodeSystemContentMode; var impliedSystem : string) : boolean; overload;
    function findCode(cs : TFhirCodeSystemW; code: String; list : TFhirCodeSystemConceptListW; displays : TConceptDesignations; out isabstract : boolean): boolean;
    function checkConceptSet(path : String; cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code : String; abstractOk : boolean; displays : TConceptDesignations; vs : TFHIRValueSetW; var message : String; op : TFHIROperationOutcomeW) : boolean;
    function checkExpansion(path : String; cs: TCodeSystemProvider; cset : TFhirValueSetExpansionContainsW; code : String; abstractOk : boolean; displays : TConceptDesignations; vs : TFHIRValueSetW; var message : String; op : TFHIROperationOutcomeW) : boolean;
    function fixedSystemFromValueSet: String;
    procedure prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW);
    function getName: String;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion : TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; id : String; i18n : TI18nSupport); overload;
    destructor Destroy; override;

    property id : String read FId;
    property name : String read getName;

    procedure prepare(vs : TFHIRValueSetW; params : TFHIRExpansionParams);

    function check(issuePath, system, version, code : String; abstractOk, implySystem : boolean; op : TFhirOperationOutcomeW) : boolean; overload;
    function check(issuePath, system, version, code : String; implySystem : boolean) : TFhirParametersW; overload;
    function check(issuePath : String; coding : TFhirCodingW; abstractOk, implySystem : boolean): TFhirParametersW; overload;
    function check(issuePath : String; code: TFhirCodeableConceptW; abstractOk, implySystem : boolean) : TFhirParametersW; overload;

    property log : String read FLog;
  end;

  { TFHIRValueSetExpander }

  TFHIRValueSetExpander = class (TValueSetWorker)
  private
    FCount : integer;
    FOffset : integer;
    FLang : String;
    FLimitCount : integer;
    FCanBeHierarchy : boolean;
    FRootList : TFslList<TFhirValueSetExpansionContainsW>;
    FFullList : TFslList<TFhirValueSetExpansionContainsW>;
    FMap : TFslMap<TFhirValueSetExpansionContainsW>;

    function makeFilterForValueSet(cs : TCodeSystemProvider; vs : TFHIRValueSetW) : TCodeSystemProviderFilterContext;
    procedure processCodeAndDescendants(doDelete : boolean; cs : TCodeSystemProvider; context : TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; srcUrl : String);

    procedure handleDefine(cs : TFhirCodeSystemW; source : TFhirValueSetCodeSystemW; defines : TFhirCodeSystemConceptListW; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; srcURL : String);
    procedure importValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
    procedure excludeValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
    procedure processCodes(doDelete : boolean; cset : TFhirValueSetComposeIncludeW; vsSrc : TFHIRValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);
    procedure handleCompose(source : TFhirValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);

    function passesImports(imports : TFslList<TFHIRImportedValueSet>; system, code : String; offset : integer) : boolean;
    function passesImport(import : TFHIRImportedValueSet; system, code : String) : boolean;

    function processCode(parent : TFhirValueSetExpansionContainsW; doDelete : boolean; system, version, code : String; isAbstract, isInactive, deprecated : boolean; displays : TConceptDesignations; definition, itemWeight: string; expansion : TFhirValueSetExpansionW;
        imports : TFslList<TFHIRImportedValueSet>; csExtList, vsExtList : TFslList<TFhirExtensionW>; csProps : TFslList<TFhirCodeSystemConceptPropertyW>; expProps : TFslList<TFhirValueSetExpansionContainsPropertyW>; srcURL : string) : TFhirValueSetExpansionContainsW;
    procedure addDefinedCode(cs : TFhirCodeSystemW; system : string; c : TFhirCodeSystemConceptW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; srcURL : String);
    function key(system, code : String): string; overload;
    function key(c : TFhirValueSetExpansionContainsW) : string;  overload;
    function expandValueSet(uri, filter: String; dependencies: TStringList; var notClosed: boolean): TFHIRValueSetW;
    function canonical(system, version: String): String;
    procedure checkSource(cset: TFhirValueSetComposeIncludeW; exp: TFHIRValueSetExpansionW; filter : TSearchFilterText; srcURL : String);
    procedure checkCanExpandValueset(uri: String);
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion : TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport); overload;

    function expand(source : TFHIRValueSetW; params : TFHIRExpansionParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer) : TFHIRValueSetW;
  end;

const
   CODES_TFhirExpansionParamsVersionRuleMode : array [TFhirExpansionParamsVersionRuleMode] of String = ('Default', 'Check', 'Override');

implementation

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

constructor TValueSetWorker.Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion : TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  Create;
  FFactory := factory;
  FOnGetValueSet := getVS;
  FOnGetCSProvider := getCS;
  FOnListCodeSystemVersions := getVersions;
  FOnGetExpansion := getExpansion;
  FAdditionalResources := txResources;
  FLanguages := languages;
  FRequiredSupplements := TStringList.create;
  FI18n := i18n;
end;

destructor TValueSetWorker.Destroy;
begin
  FRequiredSupplements.Free;
  FLanguages.Free;
  FAdditionalResources.Free;
  FFactory.Free;
  FParams.Free;
  FI18n.free;
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

function TValueSetWorker.findInAdditionalResources(url, version, resourceType : String) : TFHIRMetadataResourceW;
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
          raise EFHIRException.Create('Attempt to reference '+url+' as a '+resourceType+' when it''s a '+r.fhirType);
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

//  result := nil;

  cs := findInAdditionalResources(url, version, 'CodeSystem') as TFhirCodeSystemW;
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
          raise ETerminologySetup.create('Unable to provide support for code system '+url)
        else
          raise ETerminologySetup.create('Unable to provide support for code system '+url+' version '+version+' (known versions = '+ts.CommaText+')');
      finally
        ts.Free;
      end;

    end;
end;

function TValueSetWorker.findValueSet(url: String): TFHIRValueSetW;
var
  r : TFHIRMetadataResourceW;
begin
  if (url = '') then
    exit(nil);

  r := findInAdditionalResources(url, '', 'ValueSet');
  if (r <> nil) then
    exit(r.link as TFHIRValueSetW);

  result := FOnGetValueSet(self, url);
end;

function TValueSetWorker.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FFactory.sizeInBytes(magic));
  inc(result, FParams.sizeInBytes(magic));
  inc(result, FAdditionalResources.sizeInBytes(magic));
end;

{ TValueSetChecker }

constructor TValueSetChecker.Create(factory: TFHIRFactory;
  getVS: TGetValueSetEvent; getCS: TGetProviderEvent;
  getVersions: TGetSystemVersionsEvent; getExpansion: TGetExpansionEvent;
  txResources: TFslMetadataResourceList; languages: TIETFLanguageDefinitions;
  id: String; i18n: TI18nSupport);
begin
  inherited Create(factory, getVs, getCs, getVersions, getExpansion, txResources, languages, i18n);
  FId := id;
  FOthers := TFslStringObjectMatch.create;
  FOthers.PreventDuplicates;
  FOthers.DefaultValue := nil;
  FOthers.Forced := true;
end;

destructor TValueSetChecker.Destroy;
begin
  FValueSet.Free;
  FOthers.Free;
  inherited;
end;

function TValueSetChecker.determineSystemFromExpansion(code: String): String;
var
  exp : TFHIRValueSetExpander;
  dep : TStringList;
  vse : TFHIRValueSetW;
  c : TFhirValueSetExpansionContainsW;
begin
  try
    exp := TFHIRValueSetExpander.create(FFactory.link, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FOnGetExpansion,
      FAdditionalResources.link, FLanguages.link, FI18n.link);
    try
      dep := TStringList.Create;
      try
        vse := exp.expand(FValueSet, FParams, '', dep, 0, 10000, 0);
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
        dep.Free;
      end;
    finally
      exp.Free;
    end;
  except
    result := ''; // suppress?
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
          loc := cs.locate(code, msg);
          if loc <> nil then
          begin
            cs.Close(loc);
            if (result = '') then
              result := vsi.systemUri
            else if (result <> vsi.systemUri) then
              exit('');
          end;
        end;
      finally
        cs.Free;
      end;
    end;
  end;
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
    op.addIssue(isError, itNotFound, path+'.version', message);
    exit('');
  end;
  if result = '' then
    result := FParams.getVersionForRule(systemURI, fvmDefault);
end;

procedure TValueSetChecker.prepare(vs: TFHIRValueSetW; params : TFHIRExpansionParams);
var
  cc : TFhirValueSetComposeIncludeW;
  other : TFHIRValueSetW;
  checker : TValueSetChecker;
  ics : TFHIRValueSetCodeSystemW;
  s : String;
  cs : TFhirCodeSystemProvider;
begin
  FParams := params.Link;

  vs.checkNoImplicitRules('ValueSetChecker.prepare', 'ValueSet');
  FFactory.checkNoModifiers(vs, 'ValueSetChecker.prepare', 'ValueSet');
  if (vs = nil) then
    raise EFslException.Create('Error Error: vs = nil')
  else
  begin
    FValueSet := vs.link;

    // r2:
    ics := FValueSet.inlineCS;
    if ics <> nil then
    begin
      try
        FFactory.checkNoModifiers(ics, 'ValueSetChecker.prepare', 'CodeSystem');
        cs := TFhirCodeSystemProvider.create(FLanguages.link, ffactory.link, TFHIRCodeSystemEntry.Create(FFactory.wrapCodeSystem(FValueSet.Resource.Link)));
        FOthers.Add(ics.systemUri, cs);
        if (FValueSet.version <> '') then
          FOthers.Add(ics.systemUri+'|'+FValueSet.version, cs);
      finally
        ics.Free;
      end;
    end;

    if (FValueSet.checkCompose('ValueSetChecker.prepare', 'ValueSet.compose')) then
    begin
      // not r2:
      for s in FValueSet.imports do
      begin
        other := findValueSet(s);
        try
          if other = nil then
            raise ETerminologyError.create('Unable to find value set '+s, itUnknown);
          checker := TValueSetChecker.create(FFactory.link, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FOnGetExpansion, FAdditionalResources.link, FLanguages.link, other.url, FI18n.link);
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
end;

procedure TValueSetChecker.prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW);
var
  other: TFhirValueSetW;
  checker: TValueSetChecker;
  s : string;
  ccf: TFhirValueSetComposeIncludeFilterW;
  cs: TCodeSystemProvider;
begin
  FFactory.checkNoModifiers(cc, 'ValueSetChecker.prepare', desc);
  for s in cc.valueSets do
  begin
    if not FOthers.ExistsByKey(s) then
    begin
      other := findValueSet(s);
      try
        if other = nil then
          raise ETerminologyError.create('Unable to find value set ' + s, itUnknown);
        checker := TValueSetChecker.create(FFactory.link, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FOnGetExpansion, FAdditionalResources.link, FLanguages.link, other.url, FI18n.link);
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
      displays.display := list[i].displayElement; {no .link}
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

function TValueSetChecker.check(issuePath, system, version, code: String; abstractOk, implySystem : boolean; op : TFhirOperationOutcomeW): boolean;
var
  msg, ver, impliedSystem : string;
  it : TFhirIssueType;
  contentMode : TFhirCodeSystemContentMode;
begin
  result := check(issuePath, system, version, code, abstractOk, implySystem, nil, msg, ver, it, op, contentMode, impliedSystem);
end;

function TValueSetChecker.check(path, system, version, code: String;
  abstractOk, implySystem: boolean; displays: TConceptDesignations;
  var message, ver: String; var cause: TFhirIssueType;
  op: TFhirOperationOutcomeW; var contentMode: TFhirCodeSystemContentMode;
  var impliedSystem: string): boolean;
var
  cs : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  cc : TFhirValueSetComposeIncludeW;
  excluded : boolean;
  isabstract : boolean;
  checker : TValueSetChecker;
  s, v : String;
  ics : TFHIRValueSetCodeSystemW;
  ccl : TFhirCodeSystemConceptListW;
  ccc : TFhirValueSetExpansionContainsW;
begin
//  result := false;

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
        result := false;
        cause := itNotFound;
        FLog := 'Unknown code system';
        if (version <> '') then
          op.addIssue(isError, itNotFound, path+'.system', FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.langCode, [system, version]))
        else
          op.addIssue(isError, itNotFound, path+'.system', FI18n.translate('UNKNOWN_CODESYSTEM', FParams.langCode, [system]));
      end
      else
      begin
        ver := cs.version(nil);
        contentMode := cs.contentMode;
        ctxt := cs.locate(code, message);
        if (ctxt = nil) then
        begin
          if cs.contentMode <> cscmComplete then
          begin
            result := true; // we can't say it isn't valid. Need a third status?
            cause := itNotFound;
            FLog := 'Not found in Incomplete Code System';
            op.addIssue(isWarning, itNotFound, path+'.code', FI18n.translate('UNKNOWN_CODE__IN_FRAGMENT', FParams.langCode, [code, system]));
          end
          else
          begin
            result := false;
            cause := itCodeInvalid;
            FLog := 'Unknown code';
            op.addIssue(isWarning, itNotFound, path+'.code', FI18n.translate('Unknown_Code__in_', FParams.langCode, [code, system]));
          end;
        end
        else
        begin
          cause := itNull;
          try
            if not (abstractOk or not cs.IsAbstract(ctxt)) then
            begin
              result := false;
              FLog := 'Abstract code when not allowed';
              cause := itBusinessRule;
              op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.langCode, [system, code]));
            end
            else if ((FParams <> nil) and FParams.activeOnly and cs.isInactive(ctxt)) then
            begin
              result := false;
              FLog := 'Inactive code when not allowed';
              cause := itBusinessRule;
              op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('INACTIVE_CODE_NOT_ALLOWED', FParams.langCode, [system, code]));
            end
            else
            begin
              FLog := 'found OK';
              result := true;
            end;
            if (displays <> nil) then
              listDisplays(displays, cs, ctxt);
          finally
            cs.Close(ctxt);
          end;
        end;
      end;
    finally
      cs.Free;
    end;
  end
  else if (FParams.valueSetMode = vsvmNoMembership) then
  begin
    // anyhow, we ignore the value set (at least for now)
    cs := findCodeSystem(system, version, FParams, true);
    try
      if cs = nil then
      begin
        result := false;
        cause := itNotFound;
        FLog := 'Unknown code system';  
        if (version <> '') then
          op.addIssue(isError, itNotFound, path+'.system', FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.langCode, [system, version]))
        else
          op.addIssue(isError, itNotFound, path+'.system', FI18n.translate('UNKNOWN_CODESYSTEM', FParams.langCode, [system]));
      end
      else
      begin
        ver := cs.version(nil);
        contentMode := cs.contentMode;
        ctxt := cs.locate(code);
        if (ctxt = nil) then
        begin
          if cs.contentMode <> cscmComplete then
          begin
            result := true; // we can't say it isn't valid. Need a third status?
            cause := itNotFound;
            FLog := 'Not found in Incomplete Code System';
            op.addIssue(isWarning, itNotFound, path+'.code', FI18n.translate('UNKNOWN_CODE__IN_FRAGMENT', FParams.langCode, [code, system]));
          end
          else
          begin
            result := false;
            cause := itCodeInvalid;
            FLog := 'Unknown code';
            op.addIssue(isWarning, itNotFound, path+'.code', FI18n.translate('Unknown_Code__in_', FParams.langCode, [code, system]));
          end;
        end
        else
        begin
          cause := itNull;
          try
            if not (abstractOk or not cs.IsAbstract(ctxt)) then
            begin
              result := false;
              FLog := 'Abstract code when not allowed';
              cause := itBusinessRule;
              op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.langCode, [system, code]));
            end
            else if ((FParams <> nil) and FParams.activeOnly and cs.isInactive(ctxt)) then
            begin
              result := false;
              FLog := 'Inactive code when not allowed';
              cause := itBusinessRule;
              op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('INACTIVE_CODE_NOT_ALLOWED', FParams.langCode, [system, code]));
            end
            else
            begin
              FLog := 'found';
              result := true;
            end;
            listDisplays(displays, cs, ctxt);
          finally
            cs.Close(ctxt);
          end;
        end;
      end;
    finally
      cs.Free;
    end;
  end
  else
  begin
    if (system = '') and implySystem then
    begin
      system := determineSystem(code);
      if (system = '') then
      begin
        message := FI18n.translate('UNABLE_TO_INFER_CODESYSTEM', FParams.langCode, [code, FValueSet.url]);  
        op.addIssue(isError, itNotFound, path, message);
        exit(false);
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
            result := FindCode(nil, code, ccl, displays, isabstract);
            if result then
            begin
              result := abstractOk or not isabstract;
              exit;
            end;
          finally
            ccl.Free;
          end;
        end;
      finally
        ics.free;
      end;
    end;

    if (FValueSet.checkCompose('ValueSetChecker.prepare', 'ValueSet.compose')) then
    begin
      result := false;
      for s in FValueSet.imports do
      begin
        if not result then
        begin
          checker := TValueSetChecker(FOthers.matches[s]);
          result := checker.check(path, system, version, code, abstractOk, implySystem, displays, message, ver, cause, op, contentMode, impliedSystem);
        end;
      end;
      for cc in FValueSet.includes.forEnum do
      begin
        if cc.systemUri = '' then
          result := true
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
            if (FParams.valueSetMode <> vsvmMembershipOnly) then
            begin
              if (v = '') then
                message := FI18n.translate('UNKNOWN_CODESYSTEM', FParams.langCode, [system])
              else
                message := FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.langCode, [system, v, '['+listVersions(system)+']']);
              op.addIssue(isError, itNotFound, path+'.system', message);
            end;
            exit(false);
          end;
          try
            ver := cs.version(nil);
            checkSupplements(cs, cc);
            contentMode := cs.contentMode;

            result := ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri(nil) = system)) and checkConceptSet(path, cs, cc, code, abstractOk, displays, FValueSet, message, op);
          finally
            cs.free;
          end;
        end
        else
          result := false;
        for s in cc.valueSets do
        begin
          checker := TValueSetChecker(FOthers.matches[s]);
          if checker <> nil then
            result := result and checker.check(path, system, version, code, abstractOk, implySystem, displays, message, ver, cause, op, contentMode, impliedSystem)
          else
            raise ETerminologyError.Create('No Match for '+s, itUnknown);
        end;
        if result then
          break;
      end;
      if result then
        for cc in FValueSet.excludes.forEnum do
        begin
          if cc.systemUri = '' then
            excluded := true
          else
          begin
            if (cc.version = '') then
              cs := TCodeSystemProvider(FOthers.matches[cc.systemUri])
            else
              cs := TCodeSystemProvider(FOthers.matches[cc.systemUri+'|'+cc.version]);
            checkSupplements(cs, cc);
            ver := cs.version(nil);
            contentMode := cs.contentMode;
            excluded := ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri(nil) = system)) and checkConceptSet(path, cs, cc, code, abstractOk, displays, FValueSet, message, op);
          end;
          for s in cc.valueSets do
          begin
            checker := TValueSetChecker(FOthers.matches[s]);
            excluded := excluded and checker.check(path, system, version, code, abstractOk, implySystem, displays, message, ver, cause, op, contentMode, impliedSystem);
          end;
          if excluded then
            exit(false);
        end;
    end
    else if FValueSet.checkExpansion('ValueSetChecker.prepare', 'ValueSet.expansion') then
    begin
      ccc := FValueSet.findContains(system, version, code);
      try
        if (ccc = nil) then
          result := false
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
            op.addIssue(isError, itNotFound, path+'.version', message);
            exit(false);
          end;
          if (v = '') then
            cs := TCodeSystemProvider(FOthers.matches[ccc.systemUri]).link
          else
            cs := TCodeSystemProvider(FOthers.matches[ccc.systemUri+'|'+v]).link;
          if (cs = nil) then
            cs := findCodeSystem(system, v, FParams, true);
          if (cs = nil) then
          begin
            if (FParams.valueSetMode <> vsvmMembershipOnly) then
            begin
              if (v = '') then
                message := FI18n.translate('UNKNOWN_CODESYSTEM', FParams.langCode, [system])
              else
                message := FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.langCode, [system, v, '['+listVersions(system)+']']);
              op.addIssue(isError, itNotFound, path+'.system', message);
            end;
            exit(false);
          end;
          try
            ver := cs.version(nil);
            contentMode := cs.contentMode;
            result := ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri(nil) = system)) and checkExpansion(path, cs, ccc, code, abstractOk, displays, FValueSet, message, op);
          finally
            cs.free;
          end;
        end;
      finally
        ccc.free;
      end;
    end
    else
      result := false;
  end;
end;


function TValueSetChecker.check(issuePath : String; coding: TFhirCodingW; abstractOk, implySystem : boolean) : TFhirParametersW;
var
  list : TConceptDesignations;
  message, ver, pd, impliedSystem, path : String;
  cause : TFhirIssueType;
  op : TFhirOperationOutcomeW;
  contentMode : TFhirCodeSystemContentMode;
  dc : integer;
begin
  path := issuePath;
  result := FFactory.makeParameters;
  try
    op := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
    try
      list := TConceptDesignations.Create(FFactory.link, FLanguages.link);
      try
        if check(path, coding.systemUri, coding.version, coding.code, abstractOk,  implySystem, list, message, ver, cause, op, contentMode, impliedSystem) then
        begin
          result.AddParamBool('result', true);
          if ((cause = itNotFound) and (contentMode <> cscmComplete)) or (contentMode = cscmExample) then
             result.AddParamStr('message', 'The system "'+coding.systemUri+' was found but did not contain enough information to properly validate the code (mode = '+CODES_TFhirCodeSystemContentMode[contentMode]+')');
          if (coding.display <> '') and (not list.hasDisplay(FParams.displayLanguages, coding.display)) then
          begin
             dc := list.displayCount(FParams.displayLanguages);
             if dc = 0 then
             else if dc = 1 then
               result.AddParamStr('message', FI18n.translate('Display_Name_for__should_be_one_of__instead_of_one', FParams.language.language,
                ['', coding.systemUri, coding.code, list.present(FParams.displayLanguages), coding.display, FParams.langSummary]))
             else
               result.AddParamStr('message', FI18n.translate('Display_Name_for__should_be_one_of__instead_of_other', FParams.language.language,
                [inttostr(dc), coding.systemUri, coding.code, list.present(FParams.displayLanguages), coding.display, FParams.langSummary]));
          end;
          pd := list.preferredDisplay(FParams.displayLanguages);
          if (pd <> '') then
            result.AddParamStr('display', pd);
          result.addParamUri('system', coding.systemUri);
          if (ver <> '') then
            result.addParamStr('version', ver);
          if cause <> itNull then
            result.AddParamCode('cause', CODES_TFhirIssueType[cause]);
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
        list.Free;
      end;
      if (op.hasIssues) then
        result.addParam('issues').resource := op.Resource.link;
    finally
      op.Free;
    end;
    result.Link;
  finally
    result.free;
  end;
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

function TValueSetChecker.check(issuePath : String; code: TFhirCodeableConceptW; abstractOk, implySystem : boolean) : TFhirParametersW;
  function Summary(code: TFhirCodeableConceptW) : String;
  begin
    if (code.codingCount = 1) then
      result := 'The code provided ('+code.summary+') is not '
    else
      result := 'None of the codes provided ('+code.summary+') are ';
  end;
var
  list : TConceptDesignations;
  v : boolean;
  ok, first : boolean;
  contentMode : TFhirCodeSystemContentMode;
  cc, codelist, message, mt, ver, pd, ws, impliedSystem, path, m: String;
  prov, prov2 : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  c : TFhirCodingW;
  cause : TFhirIssueType;
  op : TFhirOperationOutcomeW;
  log : String;
  tl : TIETFLang;
  psys, pver, pdisp, pcode : String;
  dc, i : integer;
  a : TStringArray;
  procedure msg(s : String);
  begin
    if (s = '') then
      exit();
    if mt = '' then
      mt := s
    else if not mt.Contains(s) then
      mt := mt+'; '+s;
  end;
begin
  cause := itNull;
  if FValueSet = nil then
    raise ETerminologyError.create('Error: cannot validate a CodeableConcept without a nominated valueset', itInvalid);
  result := FFactory.makeParameters;
  try
    op := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
    try
      list := TConceptDesignations.Create(FFactory.link, FLanguages.link);
      try
        ok := false;
        codelist := '';
        mt := '';
        i := 0;
        for c in code.codings.forEnum do
        begin
          if (issuePath = 'CodeableConcept') then
            path := issuePath+'.coding['+inttostr(i)+']'
          else
            path := issuePath;;
          list.clear;
          v := check(path, c.systemUri, c.version, c.code, abstractOk, implySystem, list, message, ver, cause, op, contentMode, impliedSystem);
          if not v and (message <> '') then
            msg(message);
          if not v then
            cause := itInvalid;
          if (impliedSystem <> '') then
            ws := impliedSystem
          else
            ws := c.systemUri;
          if (c.version = '') then
            cc := ws+'#'+c.code
          else
            cc := ws+'|'+c.version+'#'+c.code;
          CommaAdd(codelist, cc);

          ok := ok or v;
          message := '';

          if (v) then
          begin
            if ((cause = itNotFound) and (contentMode <> cscmComplete)) or (contentMode = cscmExample) then
            begin
              m := 'The system "'+c.display+'" '+c.systemUri+' was found but did not contain enough information to properly validate the code (mode = '+CODES_TFhirCodeSystemContentMode[contentMode]+')';
              msg(m);
              op.addIssue(isWarning, itNotFound, path, m);
            end
            else
            if (c.display <> '') and (not list.hasDisplay(FParams.displayLanguages, c.display)) then
            begin
              dc := list.displayCount(FParams.displayLanguages);
              if dc = 0 then
                m := FI18n.translate('Display_Name_for__should_be_one_of__instead_of_one', FParams.langCode,
                  ['', c.systemUri, c.code, list.present(FParams.displayLanguages), c.display, FParams.langSummary])
              else if dc = 1 then
                m := FI18n.translate('Display_Name_for__should_be_one_of__instead_of_one', FParams.langCode,
                  ['', c.systemUri, c.code, list.present(FParams.displayLanguages), c.display, FParams.langSummary])
              else
                m := FI18n.translate('Display_Name_for__should_be_one_of__instead_of_other', FParams.langCode,
                  [inttostr(dc), c.systemUri, c.code, list.present(FParams.displayLanguages), c.display, FParams.langSummary]);
              msg(m);
              op.addIssue(isWarning, itInvalid, path+'.display', m);
            end;
            psys := c.systemUri;
            pcode := c.code;
            if (ver <> '') then
              pver := ver;
            pd := list.preferredDisplay(FParams.displayLanguages);
            if pd <> '' then
              pdisp := pd;
          end
          else if (FParams.valueSetMode <> vsvmMembershipOnly) then
          begin
            prov := findCodeSystem(ws, c.version, FParams, true);
            try
             if (prov = nil) then
             begin
               prov2 := findCodeSystem(ws, '', FParams, true);
               try
                 if (prov2 = nil) then
                   m := FI18n.translate('UNKNOWN_CODESYSTEM', FParams.langCode, [ws])
                 else
                   m := FI18n.translate('UNKNOWN_CODESYSTEM_VERSION', FParams.langCode, [ws, c.version, '['+listVersions(c.systemUri)+']']);
                 msg(m);
                 op.addIssue(isError, itNotFound, path+'.system', m);
               finally
                 prov2.free;
               end;
               cause := itNotFound;
             end
             else
             begin
               ctxt := prov.locate(c.code, message);
               try
                 if ctxt = nil then
                 begin
                   if (message <> '') then
                   begin
                     msg(message);
                     op.addIssue(isInformation, cause, path, message);
                   end;
                   m := FI18N.translate('Unknown_Code__in_', FParams.langCode, [c.code, ws]);
                   cause := itInvalid;
                   msg(m);
                   op.addIssue(isError, itInvalid, path+'.code', m);
                 end
                 else
                 begin
                   listDisplays(list, prov, ctxt);
                   if (c.display <> '') and (not list.hasDisplay(FParams.displayLanguages, c.display)) then
                   begin
                     dc := list.displayCount(FParams.displayLanguages);
                     if dc = 0 then
                       m := FI18n.translate('Display_Name_for__should_be_one_of__instead_of_other', FParams.langCode,
                         ['', prov.systemUri(ctxt), c.code, list.present(FParams.displayLanguages), c.display, FParams.langSummary])
                     else if dc = 1 then
                       m := FI18n.translate('Display_Name_for__should_be_one_of__instead_of_one', FParams.langCode,
                         ['', prov.systemUri(ctxt), c.code, list.present(FParams.displayLanguages), c.display, FParams.langSummary])
                     else
                       m := FI18n.translate('Display_Name_for__should_be_one_of__instead_of_other', FParams.langCode,
                        [inttostr(dc), prov.systemUri(ctxt), c.code, list.present(FParams.displayLanguages), c.display, FParams.langSummary]);
                     msg(m);
                     op.addIssue(isWarning, itInvalid, path+'.display', m);
                   end;
                   if (prov.version(nil) <> '') then
                     result.addParamStr('version', prov.version(nil));
                 end;
               finally
                 prov.Close(ctxt);
               end;
             end;
            finally
              prov.Free;
            end;
          end;
          inc(i);
        end;
        if (not ok) then
        begin
          if code.codingCount = 1 then
            m := FI18n.translate('None_of_the_provided_codes_are_in_the_value_set_one', FParams.langCode, ['', FValueSet.url, codelist])
          else
            m := FI18n.translate('None_of_the_provided_codes_are_in_the_value_set_other', FParams.langCode, ['', FValueSet.url, codelist]);
          msg(m);
          op.addIssue(isError, itInvalid, issuePath, m);
          if cause = itNull then
            cause := itUnknown;
        end;
      finally
        list.Free;
      end;

      result.AddParamBool('result', ok and not op.hasErrors);
      if (psys <> '') then
        result.addParamUri('system', psys)
      else if ok and (impliedSystem <> '') then
        result.addParamUri('system', impliedSystem);
      if (pcode <>'') then
        result.addParamCode('code', pcode);
      if (pver <> '') then
        result.addParamStr('version', pver);
      if pdisp <> '' then
        result.AddParamStr('display', pdisp);
      if mt <> '' then
        result.AddParamStr('message', mt);
      //if not (cause in [itNull]) then
      //  result.addParamCode('issue-type', CODES_TFhirIssueType[cause]);
      if (op.hasIssues) then
        result.addParam('issues').resource := op.Resource.link;
    finally
      op.free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function TValueSetChecker.check(issuePath, system, version, code: String; implySystem : boolean): TFhirParametersW;
var
  list : TConceptDesignations;
  message, ver, pd, impliedSystem : String;
  cause : TFhirIssueType;
  op : TFhirOperationOutcomeW;
  contentMode : TFhirCodeSystemContentMode;
begin
  result := FFactory.makeParameters;
  try
    op := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
    try
      list := TConceptDesignations.Create(FFactory.link, FLanguages.link);
      try
        if check(issuePath, system, version, code, true, implySystem, list, message, ver, cause, op, contentMode, impliedSystem) then
        begin
          result.AddParamBool('result', true);
          pd := list.preferredDisplay(FParams.displayLanguages);
          if pd <> ''then
            result.AddParamStr('display', pd);
          result.addParamUri('system', system);
          if ((cause = itNotFound) and (contentMode <> cscmComplete)) or (contentMode = cscmExample) then
             result.AddParamStr('message', 'The system "'+system+' was found but did not contain enough information to properly validate the code (mode = '+CODES_TFhirCodeSystemContentMode[contentMode]+')');
          if cause <> itNull then
            result.AddParamCode('cause', CODES_TFhirIssueType[cause]);
        end
        else
        begin
          result.AddParamBool('result', false);
          result.AddParamStr('message', 'The system/code "'+system+'"/"'+code+'" is not in the value set '+FValueSet.name);
          op.addIssue(isError, cause, 'code', 'The system/code "'+system+'"/"'+code+'" is not in the value set '+FValueSet.name);
          if (message <> '') then
            result.AddParamStr('message', message);
          if cause <> itNull then
            result.AddParamCode('cause', CODES_TFhirIssueType[cause]);
        end;
      finally
        list.Free;
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
end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    cs.Close(ctxt);
end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderFilterContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    cs.Close(ctxt);
end;

function TValueSetChecker.checkConceptSet(path : String; cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code: String; abstractOk : boolean; displays : TConceptDesignations; vs : TFHIRValueSetW; var message : String; op : TFHIROperationOutcomeW): boolean;
var
  i : integer;
  fc : TFhirValueSetComposeIncludeFilterW;
  ctxt : TCodeSystemProviderFilterContext;
  loc :  TCodeSystemProviderContext;
  prep : TCodeSystemProviderFilterPreparationContext;
  filters : Array of TCodeSystemProviderFilterContext;
  msg, c : String;
  cc : TFhirValueSetComposeIncludeConceptW;
  cfl : TFslList<TFhirValueSetComposeIncludeFilterW>;
begin
  result := false;
  if (not cset.hasConcepts) and (not cset.hasFilters) then
  begin
    loc := cs.locate(code, message);
    try
      result := false;
      if loc = nil then
      begin
        if (FParams.valueSetMode <> vsvmMembershipOnly) then
          op.addIssue(isError, itInvalid, path+'.code', FI18n.translate('Unknown_Code__in_', FParams.langCode, [code, cs.systemUri(nil)]))
      end
      else if not (abstractOk or not cs.IsAbstract(loc)) then
      begin
        if (FParams.valueSetMode <> vsvmMembershipOnly) then
          op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.langCode, [cs.systemUri(nil), code]))
      end
      else
      begin
        result := true;
        listDisplays(displays, cs, loc);
        exit;
      end;
    finally
      cs.Close(loc);
    end;
  end;

  for cc in cset.concepts.forEnum do
  begin
    c := cc.code;
    if (code = c) then
    begin
      loc := cs.locate(code);
      try
        if Loc <> nil then
        begin
          listDisplays(displays, cs, loc);
          listDisplays(displays, cc, vs);
          if not (abstractOk or not cs.IsAbstract(loc)) then
          begin
            if (FParams.valueSetMode <> vsvmMembershipOnly) then
              op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.langCode, [cs.systemUri(nil), code]))
          end
          else
          begin
            result := true;
            exit;
          end;
        end;
      finally
        cs.close(loc);
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
          filters[i] := cs.filter(false, fc.prop, fc.Op, fc.value, prep);
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
                if (FParams.valueSetMode <> vsvmMembershipOnly) then
                  op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.langCode, [cs.systemUri(nil), code]))
              end
              else
              begin
                result := true;
                exit;
              end;
            end;
          finally
            cs.Close(loc);
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
                    if (FParams.valueSetMode <> vsvmMembershipOnly) then
                      op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.langCode, [cs.systemUri(nil), code]))
                  end
                  else
                  begin
                    result := true;
                    exit;
                  end;
                end
                else
                  result := false;
              finally
                cs.Close(loc);
              end;
            end
            else if ('concept' = fc.prop) and (fc.Op = foIsNotA) then
            begin
              loc := cs.locateIsA(code, fc.value);
              try
                result := (loc = nil);
                if (result) then
                begin
                  loc := cs.locate(code, msg);
                  if Loc <> nil then
                  begin
                    listDisplays(displays, cs, loc);
                    if not (abstractOk or not cs.IsAbstract(loc)) then
                    begin
                      if (FParams.valueSetMode <> vsvmMembershipOnly) then
                        op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.langCode, [cs.systemUri(nil), code]))
                    end
                    else
                    begin
                      result := true;
                      exit;
                    end;
                  end;
                end;
              finally
                cs.Close(loc);
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
                    if (FParams.valueSetMode <> vsvmMembershipOnly) then
                      op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.langCode, [cs.systemUri(nil), code]))
                  end
                  else
                  begin
                    result := true;
                    exit;
                  end;
                end;
              finally
                cs.Close(loc);
              end;
            end;
            if not result then
              break;
            inc(i);
          end;
        end;
      finally
        for i := 0 to cfl.count - 1 do
          cs.Close(filters[i]);
        cs.Close(prep);
      end;
    finally
      cfl.free;
    end;
  end;
end;

function TValueSetChecker.checkExpansion(path: String; cs: TCodeSystemProvider; cset: TFhirValueSetExpansionContainsW; code: String; abstractOk: boolean;
  displays: TConceptDesignations; vs: TFHIRValueSetW; var message: String; op: TFHIROperationOutcomeW): boolean;
var
  loc :  TCodeSystemProviderContext;
begin
  result := false;
  loc := cs.locate(code, message);
  try
    result := false;
    if loc = nil then
    begin
      if (FParams.valueSetMode <> vsvmMembershipOnly) then
        op.addIssue(isError, itInvalid, path+'.code', FI18n.translate('Unknown_Code__in_', FParams.langCode, [code, cs.systemUri(nil)]))
    end
    else if not (abstractOk or not cs.IsAbstract(loc)) then
    begin
      if (FParams.valueSetMode <> vsvmMembershipOnly) then
        op.addIssue(isError, itBusinessRule, path+'.code', FI18n.translate('ABSTRACT_CODE_NOT_ALLOWED', FParams.langCode, [cs.systemUri(nil), code]))
    end
    else
    begin
      result := true;
      listDisplays(displays, cs, loc);
      exit;
    end;
  finally
    cs.Close(loc);
  end;
end;

function TValueSetChecker.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FOthers.sizeInBytes(magic));
  inc(result, FValueSet.sizeInBytes(magic));
  inc(result, (FId.length * sizeof(char)) + 12);
end;

{ TFHIRValueSetExpander }


function TFHIRValueSetExpander.Expand(source: TFHIRValueSetW; params : TFHIRExpansionParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer): TFHIRValueSetW;
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
begin
  noTotal := false;
  source.checkNoImplicitRules('ValueSetExpander.Expand', 'ValueSet');
  FFactory.checkNoModifiers(source, 'ValueSetExpander.Expand', 'ValueSet');

  FParams := params.Link;

  result := Ffactory.wrapValueSet(source.Resource.Clone as TFHIRResourceV);
  result.id := '';
  table := nil;
  div_ := nil;
  if not FParams.includeDefinition then
    result.clearDefinition
  else
    result.clearDefinitionExtensions([]);

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

  if FParams.Language <> nil then
    result.language := FParams.Language.code;
  FLang := result.language;

  filter := TSearchFilterText.create(textFilter);

  FMap := TFslMap<TFhirValueSetExpansionContainsW>.create('VS.Expander.map');
  FMap.defaultValue := nil;
  FRootList := TFslList<TFhirValueSetExpansionContainsW>.create;
  FFullList := TFslList<TFhirValueSetExpansionContainsW>.create;
  FCanBeHierarchy := not FParams.excludeNested;

  try
    if filter.null then
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

    if FParams.hasLimitedExpansion then
      exp.addParamBool('limitedExpansion', FParams.limitedExpansion);
    for lang in FParams.displayLanguages do
      if (FParams.Language = nil) or (not lang.matches(FParams.Language)) then
        exp.addParamCode('displayLanguage', lang.code);
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
    if FOffset + Fcount > 0 then
    begin
      exp.addParamInt('offset', FOffset);
      exp.addParamInt('count', FCount);
    end;

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
              handleDefine(cs2, ics, cl, filter, exp, nil, source.url);
            finally
              cs2.Free;
            end;
          finally
            cl.Free;
          end;
        end;
      finally
        ics.Free;
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
        if FCanBeHierarchy or (o > offset) and ((count = 0) or (t < count)) then
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
    filter.Free;
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
          TSpecialProviderFilterContextConcepts(result).add(cs.locate(cc.code, message));
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
    vs := expandValueSet(s, filter.filter, dependencies, notClosed);
    try
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
    processCodes(false, c, source, filter, dependencies, expansion, notClosed);
  for c in source.excludes.forEnum do
    processCodes(true, c, source, filter, dependencies, expansion, notClosed);
end;

procedure TFHIRValueSetExpander.handleDefine(cs : TFhirCodeSystemW; source : TFhirValueSetCodeSystemW; defines : TFhirCodeSystemConceptListW; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; srcURL : String);
var
  cm : TFhirCodeSystemConceptW;
  v : String;
begin
  if (defines.Count > 0) and (expansion <> nil) and (cs.version <> '') then
  begin
    if FFactory.version = fhirVersionRelease2 then
      v := source.systemUri+FHIR_VERSION_CANONICAL_SPLIT_2+cs.version
    else
      v := source.systemUri+FHIR_VERSION_CANONICAL_SPLIT_3p+cs.version;
    if not expansion.hasParam('version', v) then
      expansion.addParamUri('version', v);
  end;
  for cm in defines do
  begin
    FFactory.checkNoModifiers(cm, 'ValueSetExpander.handleDefine', 'concept');
    if filter.passes(cm.display) or filter.passes(cm.code) then
      addDefinedCode(cs, source.systemUri, cm, imports, nil, srcUrl);
    handleDefine(cs, source, cm.conceptList, filter, nil, imports, srcUrl);
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
    displays.addDesignation(ccd.language, ccd.valueElement); {no .link}
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
    displays.display := c.displayElement; {no .link}
  end;
  i := 0;
  for cd in c.designations.forEnum do
  begin
    // see https://chat.fhir.org/#narrow/stream/179202-terminology/topic/ValueSet.20designations.20and.20languages
    list := cd.getExtensionsW('http://hl7.org/fhir/StructureDefinition/coding-sctdescid');
    try
      displays.addDesignation(i, cd.language, cd.valueElement, list); { no .link}
      inc(i);
    finally
      list.free;
    end;
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
    ts.Free;
  end;
end;

constructor TFHIRValueSetExpander.Create(factory: TFHIRFactory; getVS: TGetValueSetEvent; getCS: TGetProviderEvent; getVersions : TGetSystemVersionsEvent; getExpansion: TGetExpansionEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  inherited create(factory, getVS, getCS, getVersions, getExpansion, txResources, languages, i18n);
end;

procedure TFHIRValueSetExpander.addDefinedCode(cs : TFhirCodeSystemW; system: string; c: TFhirCodeSystemConceptW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; srcURL : String);
var
  i : integer;
  cds : TConceptDesignations;
  n : TFhirValueSetExpansionContainsW;
begin
  if not FParams.excludeNotForUI or not (cs.isAbstract(c)) then
  begin
    cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
    try
      listDisplays(cds, c);
      n := processCode(parent, false, system, '', c.Code, cs.isAbstract(c), cs.isInactive(c), cs.isDeprecated(c),  cds, c.definition, c.itemWeight,
         nil, imports, c.getAllExtensionsW, nil, c.properties, nil, srcUrl);
    finally
      cds.Free;
    end;
  end;
  for i := 0 to c.conceptList.count - 1 do
    addDefinedCode(cs, system, c.conceptList[i], imports, n, srcUrl);
end;

function TFHIRValueSetExpander.canonical(system, version : String) : String;
begin
  if FFactory.version = fhirVersionRelease2 then
    result := system + '?version='+version
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

function TFHIRValueSetExpander.processCode(parent : TFhirValueSetExpansionContainsW; doDelete : boolean; system, version, code : String;
    isAbstract, isInactive, deprecated : boolean; displays : TConceptDesignations; definition, itemWeight: string; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>;
    csExtList, vsExtList : TFslList<TFhirExtensionW>; csProps : TFslList<TFhirCodeSystemConceptPropertyW>; expProps : TFslList<TFhirValueSetExpansionContainsPropertyW>; srcURL : string) : TFhirValueSetExpansionContainsW;
var
  n : TFhirValueSetExpansionContainsW;
  s, pn : String;
  srcLang, dstLang, tl : TIETFLang;
  usedDisplay : boolean;
  tu, t : TConceptDesignation;
  ext : TFHIRExtensionW;
  cp : TFhirCodeSystemConceptPropertyW;
begin
  try
    if not passesImports(imports, system, code, 0) then
      exit;

    if (FLimitCount > 0) and (FFullList.Count >= FLimitCount) and not doDelete then
    begin
      if (FCount + FOffset > 0) and (FFullList.count > FCount + FOffset) then
        raise EFinished.create('.')
      else
        raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.langCode, [srcUrl, '>'+inttostr(FLimitCount)]));
    end;

    if (expansion <> nil) and (version <> '') then
    begin
      s := canonical(system, version);
      if not expansion.hasParam('version', s) then
        expansion.addParamUri('version', s);
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

        if FParams.includeDefinition and (definition <> '') then
          n.addExtensionV('http://hl7.org/fhir/StructureDefinition/valueset-concept-definition', FFactory.makeString(definition));

        if (hasExtension(csExtList, 'http://hl7.org/fhir/StructureDefinition/codesystem-label')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#label', 'label', FFactory.makeString(getExtensionString(csExtList, 'http://hl7.org/fhir/StructureDefinition/codesystem-label')));
        if (hasExtension(vsExtList, 'http://hl7.org/fhir/StructureDefinition/valueset-label')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#label', 'label', FFactory.makeString(getExtensionString(vsExtList, 'http://hl7.org/fhir/StructureDefinition/valueset-label')));

        if (hasExtension(csExtList, 'http://hl7.org/fhir/StructureDefinition/codesystem-conceptOrder')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#order', 'order', FFactory.makeInteger(getExtensionString(csExtList, 'http://hl7.org/fhir/StructureDefinition/codesystem-conceptOrder')));
        if (hasExtension(vsExtList, 'http://hl7.org/fhir/StructureDefinition/valueset-conceptOrder')) then
          expansion.defineProperty(n, 'http://hl7.org/fhir/concept-properties#order', 'order', FFactory.makeInteger(getExtensionString(vsExtList, 'http://hl7.org/fhir/StructureDefinition/valueset-conceptOrder')));

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
        srcLang := displays.baseLang;
        dstLang := FParams.Language;

        usedDisplay := false;
        if (FParams.DisplayLanguages.Count > 0) then
          tu := displays.findDisplay(FParams.displayLanguages)
        else
          tu := nil;

        if (tu <> nil) then
          n.Display := tu.value.asString
        else if (displays.display <> nil) and ((srcLang = nil) or (dstLang = nil) or dstLang.matches(srcLang)) then
        begin
          n.Display := displays.display.asString;
          usedDisplay := true;
        end
        else
        begin
          // we don't have a usable display
        end;

        if FParams.includeDesignations then
        begin
          if not usedDisplay and (displays.display <> nil) then
            n.addDesignation(srcLang.code, '', displays.display.AsString);
          for t in displays.designations do
            if (t <> tu) and (((t.Language <> nil) or (t.use <> nil)) and (t.value <> nil)) then
              n.addDesignation(t.language, t.use, t.value, t.extensions);
        end;

        for pn in FParams.properties do
        begin
          if csProps <> nil then
            for cp in csprops do
            begin
              if cp.code = pn then
                n.addProperty(pn, cp.value);
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

procedure TFHIRValueSetExpander.checkCanExpandValueset(uri: String);
var
  vs : TFHIRValueSetW;
begin
  vs := findValueSet(uri);
  try
    if vs = nil then
      raise ETerminologyError.create('Unable to find value set "'+uri+'"', itUnknown);
  finally
    vs.Free;
  end;
end;

function TFHIRValueSetExpander.expandValueSet(uri: String; filter : String; dependencies : TStringList;  var notClosed : boolean) : TFHIRValueSetW;
var
  dep : TStringList;
  exp : TFhirValueSetExpansionW;
begin
  dep := TStringList.Create;
  try
    result := FOnGetExpansion(self, uri, filter, FParams, dep, FAdditionalResources , -1);
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
    dep.Free;
  end;
end;

procedure TFHIRValueSetExpander.importValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
var
  c : TFhirValueSetExpansionContainsW;
  s : String;
begin
  FCanBeHierarchy := false;
  expansion.copyParams(vs.expansion);

  for c in vs.expansion.contains.forEnum do
  begin
    s := key(c);
    if passesImports(imports, c.systemUri, c.code, offset) and not FMap.containsKey(s) then
    begin
      FFullList.add(c.link);
      FMap.add(s, c.link);
    end;
  end;
end;

procedure TFHIRValueSetExpander.excludeValueSet(vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
var
  c : TFhirValueSetExpansionContainsW;
  s : String;
begin
  for c in vs.expansion.contains.forEnum do
  begin
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
  FFactory.checkNoModifiers(cset,'ValueSetExpander.processCodes', 'set');
  imp := false;
  for s in cset.valueSets do
  begin
    checkCanExpandValueset(s);
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
          checkCanExpandValueSet(cs.SpecialEnumeration);
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

          if not imp and (FLimitCount > 0) and (cs.TotalCount > FLimitCount) and not (FParams.limitedExpansion) and (FOffset+FCount = 0) then
            raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.langCode, [srcUrl, '>'+inttostr(FLimitCount)]));
        end
      end;

    finally
      cs.free;
    end;
  end;
end;

procedure TFHIRValueSetExpander.processCodes(doDelete : boolean; cset: TFhirValueSetComposeIncludeW; vsSrc : TFHIRValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);
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
  s, display, ov : String;
  valueSets : TFslList<TFHIRImportedValueSet>;
  base : TFHIRValueSetW;
  cc : TFhirValueSetComposeIncludeConceptW;
  cctxt : TCodeSystemProviderContext;
  cds : TConceptDesignations;
  iter : TCodeSystemIteratorContext;
  vs : TFHIRValueSetW;
  parent : TFhirValueSetExpansionContainsW;
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
  valueSets := TFslList<TFHIRImportedValueSet>.create;
  try
    FFactory.checkNoModifiers(cset,'ValueSetExpander.processCodes', 'set');

    if (cset.hasValueSets or cset.hasConcepts or (cset.filterCount > 1)) then
      FCanBeHierarchy := false;

    if cset.systemUri = '' then
    begin
      for s in cset.valueSets do
        valueSets.add(TFHIRImportedValueSet.create(expandValueset(s, filter.filter, dependencies, notClosed)));
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
          checkSupplements(cs, cset);
          for s in cset.valueSets do
          begin
            f := nil;
            // if we can, we can do a short cut evaluation that means we don't have to do a full expansion of the source value set.
            // this saves lots of overhead we don't need. But it does require simple cases (though they are common). So we have a look
            // at the value set, and see whether we can short cut it. If we can, it's just another filter (though we can't iterate on it)
            vs := FOnGetValueSet(self, s);
            try
              if (vs <> nil) then
                f := makeFilterForValueSet(cs, vs);
              if (f <> nil) then
                filters.add(f)
              else
                valueSets.add(TFHIRImportedValueSet.create(expandValueset(s, filter.filter, dependencies, notClosed)));
            finally
              vs.Free;
            end;
          end;

          if (not cset.hasConcepts) and (not cset.hasFilters) then
          begin
            if (cs.SpecialEnumeration <> '') and FParams.limitedExpansion and filters.Empty then
            begin
              base := expandValueSet(cs.SpecialEnumeration, filter.filter, dependencies, notClosed);
              try
                expansion.addExtensionV('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', FFactory.makeBoolean(true));
                if doDelete then
                  excludeValueSet(base, expansion, valueSets, 0)
                else
                  importValueSet(base, expansion, valueSets, 0);
              finally
                base.Free;
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
                if valueSets.Empty and (FLimitCount > 0) and (iter.count > FLimitCount) and not (FParams.limitedExpansion) and not doDelete and (FOffset + Fcount = 0) then
                  raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY', FParams.langCode, [vsSrc.url, '>'+inttostr(FLimitCount)]));
                while iter.more do
                begin
                  c := cs.getNextContext(iter);
                  if passesFilters(c, 0) then
                    processCodeAndDescendants(doDelete, cs, c, expansion, valueSets, nil, vsSrc.url);
                end;
              finally
                iter.Free;
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
                    c := cs.FilterConcept(ctxt);
                    if passesFilters(c, 0) then
                    begin
                      cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
                      try
                        listDisplays(cds, cs, c); // cs.display(c, FParams.displayLanguage)
                        processCode(nil, doDelete, cs.systemUri(c), cs.version(c), cs.code(c),  cs.isAbstract(c), cs.isInactive(c), cs.deprecated(c),
                        cds, cs.definition(c), cs.itemWeight(c), expansion, valueSets, cs.getExtensions(c), nil, cs.getProperties(c), nil, vsSrc.url);
                      finally
                        cds.free;
                      end;
                    end;
                  end;
                finally
                  cs.Close(ctxt);
                end;
              finally
                cs.Close(prep);
              end;
            end;
          end;

          cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
          try
            for cc in cset.concepts.forEnum do
            begin
              cds.Clear;
              FFactory.checkNoModifiers(cc, 'ValueSetExpander.processCodes', 'set concept reference');
              cctxt := cs.locate(cc.code);
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
                    processCode(nil, doDelete, cs.systemUri(nil), cs.version(nil), cc.code, cs.isAbstract(cctxt), cs.isInactive(cctxt), cs.deprecated(cctxt), cds,
                         cs.Definition(cctxt), ov, expansion, valueSets, cs.getExtensions(cctxt), cc.getAllExtensionsW, cs.getProperties(cctxt), nil, vsSrc.url);
                  end;
                end;
              finally
                cs.Close(cctxt);
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

                  inner := not cs.prepare(prep);
                  count := 0;
                  While cs.FilterMore(filters[0]) and ((FOffset + FCount = 0) or (count < FOffset + FCount)) do
                  begin
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
                              processCode(parent, doDelete, cs.systemUri(nil), cs.version(nil), cs.code(c), cs.isAbstract(c), cs.IsInactive(c),
                                  cs.deprecated(c), cds, cs.definition(c), cs.itemWeight(c), expansion, nil, cs.getExtensions(c), nil, cs.getProperties(c), nil, vsSrc.url);
                            end;
                          finally
                            cds.free;
                          end;
                        end;
                      end;
                    finally
                      cs.close(c);
                    end;
                  end;
                finally
                  for f in filters do
                    cs.Close(f.Link);
                end;
              finally
                prep.free;
              end;
            finally
              fcl.Free;
            end;
          end;
        finally
          cs.free;
        end;
      finally
        filters.Free;
      end;
    end;
  finally
    valueSets.Free;
  end;
end;

procedure TFHIRValueSetExpander.processCodeAndDescendants(doDelete : boolean; cs: TCodeSystemProvider; context: TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; parent : TFhirValueSetExpansionContainsW; srcUrl : String);
var
  i : integer;
  vs : String;
  cds : TConceptDesignations;
  iter : TCodeSystemIteratorContext;
  n : TFhirValueSetExpansionContainsW;
begin
  try
    if (cs.version(nil) <> '') and (expansion <> nil) then
    begin
      vs := canonical(cs.systemUri(nil), cs.version(nil));
      if not expansion.hasParam('version', vs) then
        expansion.addParamUri('version', vs);
    end;
    if (not FParams.excludeNotForUI or not cs.IsAbstract(context)) and (not FParams.activeOnly or not cs.isInActive(context)) then
    begin
      cds := TConceptDesignations.Create(FFactory.link, FLanguages.link);
      try
        listDisplays(cds, cs, context);
        n := processCode(parent, doDelete, cs.systemUri(context), '', cs.Code(context), cs.isAbstract(context), cs.IsInactive(context), cs.deprecated(context), cds, cs.definition(context),
             cs.itemWeight(context), expansion, imports, cs.getExtensions(context), nil, cs.getProperties(context), nil, srcUrl);
      finally
        cds.Free;
      end;
    end
    else
      n := parent;
    iter := cs.getIterator(context);
    try
      while iter.more do
        processCodeAndDescendants(doDelete, cs, cs.getNextContext(iter), expansion, imports, n, srcUrl);
    finally
      iter.Free;
    end;
  finally
    cs.Close(context);
  end;
end;

{ TFHIRExpansionParams }

constructor TFHIRExpansionParams.Create;
begin
  inherited;
  FVersionRules := TFslList<TFhirExpansionParamsVersionRule>.create;
  FDisplayLanguages := TFslList<TIETFLang>.create;
  FProperties := TStringList.create;

  FGenerateNarrative := true;
end;

procedure TFHIRExpansionParams.SetLanguage(value: TIETFLang);
begin
  FLanguage.free;
  FLanguage := value;
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

procedure TFHIRExpansionParams.SetValueSetMode(value : TValueSetValidationMode);
begin
  FValueSetMode := value;
  FHasValueSetMode := true;
end;

function TFHIRExpansionParams.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FVersionRules.sizeInBytes(magic));
  inc(result, FDisplayLanguages.sizeInBytes(magic));
  inc(result, FLanguage.sizeInBytes(magic));
  inc(result, (FUid.length * sizeof(char)) + 12);
end;

class function TFHIRExpansionParams.defaultProfile: TFHIRExpansionParams;
begin
  result := TFHIRExpansionParams.Create;
end;

procedure TFHIRExpansionParams.loadFromLangs(languages: TIETFLanguageDefinitions; langs: THTTPLanguages);
var
  s : String;
begin
  FDisplayLanguages.clear;
  Language := nil;

  for s in langs.codes do
  begin
    if (s <> '') then
    begin
      if FLanguage = nil then
        FLanguage := languages.parse(s);
      FDisplayLanguages.add(languages.parse(s));
    end;
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

function TFHIRExpansionParams.langCode: string;
begin
  if language = nil then
    result := ''
  else
    result := language.language;
end;

function TFHIRExpansionParams.langSummary: String;
var
    i : integer;
begin
  if FDisplayLanguages.Count = 0 then
    result := '--'
  else
  begin
    result := FDisplayLanguages[0].language;
    for i := 1 to FDisplayLanguages.Count - 1 do
      result := result + '|' + FDisplayLanguages[i].language;
  end;
end;

destructor TFHIRExpansionParams.Destroy;
begin
  FVersionRules.Free;
  FDisplayLanguages.Free;
  FLanguage.Free;
  FProperties.free;
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
  s := FUid+'|'+ inttostr(ord(FValueSetMode)) + '|' + FProperties.CommaText+'|'+
    b(FactiveOnly)+b(FIncompleteOK)+b(FexcludeNested)+b(FGenerateNarrative)+b(FlimitedExpansion)+b(FexcludeNotForUI)+b(FexcludePostCoordinated)+
    b(FincludeDesignations)+b(FincludeDefinition)+b(FHasactiveOnly)+b(FHasExcludeNested)+b(FHasGenerateNarrative)+
    b(FHasLimitedExpansion)+b(FHesExcludeNotForUI)+b(FHasExcludePostCoordinated)+b(FHasIncludeDesignations)+
    b(FHasIncludeDefinition)+b(FHasDefaultToLatestVersion)+b(FHasIncompleteOK)+b(FHasexcludeNotForUI)+b(FHasValueSetMode)+b(FDefaultToLatestVersion);

  if FLanguage <> nil then
    s := s + FLanguage.Language+'|';
  for l in FDisplayLanguages do
    s := s + l.Language+'|';
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
  FMap.Free;
  FValueSet.free;
  inherited;
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



