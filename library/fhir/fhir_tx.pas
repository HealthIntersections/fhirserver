unit fhir_tx;

{$i fhir.inc}

interface


uses
  SysUtils, Classes,
  fsl_base, fsl_collections, fsl_utilities, fsl_http, fsl_lang, fsl_logging, fsl_i18n, fsl_versions, fsl_threads,
  fhir_objects, fhir_common, ftx_service, fhir_factory, fhir_xhtml, fhir_extensions, fhir_uris, fhir_parser, fhir_utilities,
  fhir_codesystem_service;

 type

  // this is denial of service protection. A terminology operation is not allowed to take too long, and
  // it's not allowed to recurse

  TGetCurrentRequestCountEvent = function : integer of Object;

  { TTerminologyOperationContext }

  TTerminologyOperationContext = class (TTxOperationContext)
  private
    FId : String;
    FStartTime : UInt64;
    FContexts : TStringList;
    FLangList : THTTPLanguageList;
    FI18n : TI18nSupport;
    FTimeLimit : Cardinal;
    FOnGetCurrentRequestCount: TGetCurrentRequestCountEvent;
    FTimeTracker: TFslTimeTracker;
    procedure SetTimeTracker(AValue: TFslTimeTracker);
  public
    constructor Create(i18n : TI18nSupport; id : String; langList : THTTPLanguageList; timeLimit : cardinal; getRequestCount : TGetCurrentRequestCountEvent; tt : TFslTimeTracker; version : TFHIRVersion);
    destructor Destroy; override;

    property reqId : String read FId;
    property TimeTracker : TFslTimeTracker read FTimeTracker write SetTimeTracker;
    function copy : TTerminologyOperationContext;
    function deadCheck(var time : integer) : boolean; override;
    procedure seeContext(vurl : String);
    procedure clearContexts;

    procedure log(note : String); override;
    procedure addNote(vs : TFHIRValueSetW; note : String);
    function diagnostics : String;
    property OnGetCurrentRequestCount : TGetCurrentRequestCountEvent read FOnGetCurrentRequestCount write FOnGetCurrentRequestCount;

    class function renderCoded(system : TCodeSystemProvider) : String; overload;
    class function renderCoded(system, version : String) : String; overload;
    class function renderCoded(system, version, code : String) : String; overload;
    class function renderCoded(system, version, code, display : String) : String; overload;
    class function renderCoded(code : TFhirCodingW) : String; overload;
    class function renderCoded(code : TFhirCodeableConceptW) : String; overload;
  end;

  { TFHIRCachedMetadataResource }

  TFHIRCachedMetadataResource = class (TFslObject)
  private
    FResource : TFHIRMetadataResourceW;
    FLoadedCS: TFHIRCodeSystemEntry;
    procedure SetLoadedCS(AValue: TFHIRCodeSystemEntry);
  public
    constructor create(resource : TFHIRMetadataResourceW);
    destructor Destroy; override;
    function link : TFHIRCachedMetadataResource; overload;

    property Resource : TFHIRMetadataResourceW read FResource;
    property LoadedCS : TFHIRCodeSystemEntry read FLoadedCS write SetLoadedCS;
  end;


  { TFslMetadataResourceByVersionSorter }

  { TFHIRCachedMetadataResourceByVersionSorter }

  TFHIRCachedMetadataResourceByVersionSorter = class (TFslComparer<TFHIRCachedMetadataResource>)
  private
    FReverse : boolean;
  protected
    function Compare(const l, r : TFHIRCachedMetadataResource) : integer; override;
  public
    constructor create(reverse : boolean);
  end;

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
    function link : TFhirExpansionParamsVersionRule; overload;

    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property mode : TFhirExpansionParamsVersionRuleMode read FMode write FMode;

    function asString : String;
    function asParam : String;
  end;

  { TFHIRTxOperationParams }

  TFHIRTxOperationParams = class (TFslObject)
  private
    FLanguages : TIETFLanguageDefinitions;
    FVersionRules : TFslList<TFhirExpansionParamsVersionRule>;
    FValueSetVersionRules : TStringList;
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
    FHTTPLanguages : THTTPLanguageList;
    FDisplayLanguages : THTTPLanguageList;
    FDesignations : TStringList;
    FDiagnostics : boolean;

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
    function GetHasHTTPLanguages: boolean;
    function GetHasDisplayLanguages: boolean;
    procedure SetHTTPLanguages(value : THTTPLanguageList);
    procedure SetDisplayLanguages(value : THTTPLanguageList);
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
    constructor Create(Languages : TIETFLanguageDefinitions);
    destructor Destroy; override;
    function link : TFHIRTxOperationParams; overload;

    function clone : TFHIRTxOperationParams; overload;
    procedure assign(other : TFslObject); override;
    class function defaultProfile(langDefs : TIETFLanguageDefinitions) : TFHIRTxOperationParams;

    procedure seeParameter(name : String; value : TFHIRObject; isValidation, overwrite : boolean);

    property versionRules : TFslList<TFhirExpansionParamsVersionRule> read FVersionRules;

    function getVersionForRule(systemURI : String; mode : TFhirExpansionParamsVersionRuleMode) : String;
    function rulesForSystem(systemURI : String) : TFslList<TFhirExpansionParamsVersionRule>;
    procedure seeVersionRule(url : String; mode : TFhirExpansionParamsVersionRuleMode);

    property activeOnly : boolean read FactiveOnly write SetActiveOnly;
    property HTTPLanguages : THTTPLanguageList read FHTTPLanguages write SetHTTPLanguages;
    property DisplayLanguages : THTTPLanguageList read FDisplayLanguages write SetDisplayLanguages;
    function workingLanguages : THTTPLanguageList;
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
    property diagnostics : boolean read FDiagnostics write FDiagnostics;


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
    property hasHTTPLanguages : boolean read GetHasHTTPLanguages;
    property hasDisplayLanguages : boolean read GetHasDisplayLanguages;
    property hasDesignations : boolean read GetHasDesignations;

    function summary : string;
    function verSummary : String;
    function hash : String;

    function hasValueSetVersionRules : boolean;
    function getValueSetVersionRules : TStringList;
  end;
          
  TGetProviderEvent = function (sender : TObject; url, version : String; params : TFHIRTxOperationParams; nullOk : boolean) : TCodeSystemProvider of object;
  TGetSystemVersionsEvent = procedure (sender : TObject; url : String; list : TStringlist) of object;

  { TTerminologyWorker }

  TTerminologyWorker = class (TFslObject)
  protected
    FOpContext : TTerminologyOperationContext;
    FFactory : TFHIRFactory;
    FOnGetCSProvider : TGetProviderEvent;
    FOnListCodeSystemVersions : TGetSystemVersionsEvent;
    FAdditionalResources : TFslList<TFHIRCachedMetadataResource>;
    FLanguages : TIETFLanguageDefinitions;
    FI18n : TI18nSupport;
    FLangList : THTTPLanguageList;
    FNoCacheThisOne : boolean;
    FParams : TFHIRTxOperationParams;
    FRequiredSupplements : TStringList;
    FFoundParameters : TStringList;

    function costDiags(e : ETooCostly) : ETooCostly;
    function opName : String; virtual;
    function sizeInBytesV(magic : integer) : cardinal; override;
    function vsHandle : TFHIRValueSetW; virtual; abstract;
    procedure deadCheck(place : String); virtual;
    function findInAdditionalResources(url, version, resourceType : String; error : boolean) : TFHIRCachedMetadataResource;
    function findCodeSystem(url, version : String; params : TFHIRTxOperationParams; kinds : TFhirCodeSystemContentModeSet; nullOk, checkVer, noVParams : boolean; op: TFhirOperationOutcomeW) : TCodeSystemProvider;
    function determineVersion(url, version : String; params : TFHIRTxOperationParams; va : TFHIRVersionAlgorithm):String;
    procedure checkVersion(url, version : String; params : TFHIRTxOperationParams; va : TFHIRVersionAlgorithm; op: TFhirOperationOutcomeW);
    function getVersionList(url : String) : TStringList;
    function listVersions(url : String) : String;
    function  loadSupplements(url, version : String) : TFslList<TFhirCodeSystemW>;
    procedure checkSupplements(cs: TCodeSystemProvider; src: TFHIRXVersionElementWrapper);
  public
    constructor Create(factory : TFHIRFactory; opContext : TTerminologyOperationContext; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslList<TFHIRCachedMetadataResource>; languages : TIETFLanguageDefinitions; i18n : TI18nSupport); overload;
    destructor Destroy; override;     
    property opContext : TTerminologyOperationContext read FOpContext;
  end;

  { TFHIRCodeSystemInformationProvider }

  TFHIRCodeSystemInformationProvider = class (TTerminologyWorker)
  public
    procedure lookupCode(opContext : TTxOperationContext; coding : TFHIRCodingW; profile : TFHIRTxOperationParams; props : TArray<String>; resp : TFHIRLookupOpResponseW);
  end;

const
   CODES_TFhirExpansionParamsVersionRuleMode : array [TFhirExpansionParamsVersionRuleMode] of String = ('Default', 'Check', 'Override');
   NAMES_TFhirExpansionParamsVersionRuleMode : array [TFhirExpansionParamsVersionRuleMode] of String = ('system-version', 'check-system-version', 'force-system-version');

   LOOKUP_DEAD_TIME_SECS = 30;

implementation

{ TTerminologyOperationContext }

procedure TTerminologyOperationContext.SetTimeTracker(AValue: TFslTimeTracker);
begin
  FTimeTracker.free;
  FTimeTracker:=AValue;
end;

constructor TTerminologyOperationContext.Create(i18n: TI18nSupport; id : String; langList : THTTPLanguageList; timeLimit : cardinal; getRequestCount : TGetCurrentRequestCountEvent; tt : TFslTimeTracker; version : TFHIRVersion);
begin
  inherited create(version);
  FI18n := i18n;
  FId := id;
  FLangList := langList;
  FContexts := TStringList.create;
  FStartTime := GetTickCount64;
  FOnGetCurrentRequestCount := getRequestCount;
  FTimeLimit := timeLimit;
  if (tt = nil) then
    FTimeTracker := TFslTimeTracker.create
  else
    FTimeTracker := tt;
  FTimeTracker.step('tx-op');
end;

destructor TTerminologyOperationContext.Destroy;
begin
  FLangList.free;
  FI18n.free;
  FContexts.free;
  FTimeTracker.free;
  inherited Destroy;
end;

function TTerminologyOperationContext.copy: TTerminologyOperationContext;
begin
  result := TTerminologyOperationContext.create(FI18n.link, FId, FLangList.link, FTimeLimit, OnGetCurrentRequestCount, FTimeTracker.link, Version);
  result.FContexts.assign(FContexts);
  result.FStartTime := FStartTime;
end;

function TTerminologyOperationContext.deadCheck(var time : integer): boolean;
var
  timeToDie : UInt64;
  rq : integer;
begin
  time := FTimeLimit;
  if UnderDebugger then
    exit(false);

  timeToDie := FStartTime + (time * 1000);
  if (GetTickCount64 > timeToDie) then
    exit(true)
  else
  begin
    if assigned(OnGetCurrentRequestCount) and (OnGetCurrentRequestCount < 10) then
    begin
      // once timelimit is hit, living on borrowed time until request counts build
      time := time + (time div 2);
      // but we only give it so much time
    end;
    timeToDie := FStartTime + (time * 1000);
    result := GetTickCount64 > timeToDie;
  end;
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
    raise ETerminologyError.create(FI18n.translate('VALUESET_CIRCULAR_REFERENCE', FLangList, [vurl, '['+r+']']), itProcessing);
  end
  else
    FContexts.add(vurl);
end;

procedure TTerminologyOperationContext.clearContexts;
begin
  FContexts.clear;
end;

procedure TTerminologyOperationContext.log(note: String);
var
  s : string;
begin
  s := inttostr(GetTickCount64 - FStartTime)+'ms '+note;
  //if UnderDebugger then
  //  Logging.log(s);
  FTimeTracker.step(s);
end;

procedure TTerminologyOperationContext.addNote(vs : TFHIRValueSetW; note : String);
var
  s : string;
begin
  s := inttostr(GetTickCount64 - FStartTime)+'ms '+vs.vurl+': '+note;
  //if UnderDebugger then
  //  Logging.log(s);
  FTimeTracker.step(s);
end;

function TTerminologyOperationContext.diagnostics: String;
begin
  result := FTimeTracker.log;
end;

class function TTerminologyOperationContext.renderCoded(system: TCodeSystemProvider): String;
begin
  result := system.systemUri+'|'+system.version;
  if (system.sourcePackage <> '') then
    result := result+' (from '+system.sourcePackage+')';
end;

class function TTerminologyOperationContext.renderCoded(system, version : String): String;
begin
  if (version = '') then
    result := system
  else
    result := system+'|'+version;
end;

class function TTerminologyOperationContext.renderCoded(system, version, code : String): String;
begin
  result := renderCoded(system, version)+'#'+code;
end;

class function TTerminologyOperationContext.renderCoded(system, version, code, display : String): String;
begin
  result := renderCoded(system, version, code)+' ("'+display+'")';
end;

class function TTerminologyOperationContext.renderCoded(code: TFhirCodingW): String;
begin
  result := renderCoded(code.systemUri, code.version, code.code, code.display);
end;

class function TTerminologyOperationContext.renderCoded(code: TFhirCodeableConceptW): String;
var
  c : TFHIRCodingW;
begin
  result := '';
  for c in code.codings.forEnum do
    CommaAdd(result, renderCoded(c));
  result := '['+result+']';
end;

{ TFHIRCachedMetadataResource }

procedure TFHIRCachedMetadataResource.SetLoadedCS(AValue: TFHIRCodeSystemEntry);
begin
  FLoadedCS.free;
  FLoadedCS:=AValue;
end;

constructor TFHIRCachedMetadataResource.create(resource: TFHIRMetadataResourceW);
begin
  inherited create;
  FResource := resource;
  if (resource = nil) then
    raise ETerminologyError.create('nil!');
end;

destructor TFHIRCachedMetadataResource.Destroy;
begin
  FResource.free;
  FLoadedCS.free;
  inherited Destroy;
end;

function TFHIRCachedMetadataResource.link: TFHIRCachedMetadataResource;
begin
  result := TFHIRCachedMetadataResource(inherited link);
end;

{ TFHIRCachedMetadataResourceByVersionSorter }
   
constructor TFHIRCachedMetadataResourceByVersionSorter.create(reverse: boolean);
begin
  inherited create;
  FReverse := reverse;
end;

function TFHIRCachedMetadataResourceByVersionSorter.Compare(const l, r: TFHIRCachedMetadataResource): integer;
begin 
  if l.resource.version = r.resource.version then
    result := 0
  else if not TSemanticVersion.isValid(l.resource.version) or not TSemanticVersion.isValid(r.resource.version) then
    result := StringCompare(l.resource.version, r.resource.version)
  else if TSemanticVersion.isMoreRecent(l.resource.version, r.resource.version) then
    result := 1
  else
    result := -1;

  if FReverse then
    result := - result;
end;

{ TTerminologyWorker }

constructor TTerminologyWorker.Create(factory : TFHIRFactory; opContext : TTerminologyOperationContext; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslList<TFHIRCachedMetadataResource>; languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  Create;
  FFactory := factory;
  FOpContext := opContext;
  FOnGetCSProvider := getCS;
  FOnListCodeSystemVersions := getVersions;
  FAdditionalResources := txResources;
  FLanguages := languages;
  FI18n := i18n;
  FRequiredSupplements := TStringList.create;
  FFoundParameters := TStringList.create;
end;

destructor TTerminologyWorker.Destroy;
begin
  FFoundParameters.free;
  FLangList.free;
  FLanguages.free;
  FAdditionalResources.free;
  FFactory.free;
  FParams.free;
  FI18n.free;
  FOpContext.free;
  FRequiredSupplements.free;
  inherited;
end;

function versionMatches(rrrr : TFHIRCachedMetadataResource; version : String) : boolean;
begin
  result := TFHIRVersions.versionMatches(rrrr.resource.versionAlgorithm, version, rrrr.Resource.version);
end;
                       
function isLaterVersion(r1, r2 : TFHIRCachedMetadataResource) : boolean;
begin
  result := TFHIRVersions.isLaterVersion(r1.resource.versionAlgorithm, r2.resource.versionAlgorithm, r1.Resource.version, r2.Resource.version);
end;

function TTerminologyWorker.findInAdditionalResources(url, version, resourceType : String; error : boolean) : TFHIRCachedMetadataResource;
var
  rrrr : TFHIRCachedMetadataResource;
  matches : TFslList<TFHIRCachedMetadataResource>;
  i, t : integer;
begin
  if FAdditionalResources = nil then
     exit(nil);

  matches := TFslList<TFHIRCachedMetadataResource>.create;
  try
    for rrrr in FAdditionalResources do
    begin
      deadCheck('findInAdditionalResources');
      if (url <> '') and ((rrrr.resource.url = url) or (rrrr.resource.vurl = url))
        and ((version = '') or (version = rrrr.resource.version) or versionMatches(rrrr, version)) then
      begin
        if rrrr.resource.fhirType <> resourceType then
          if error then
            raise EFHIRException.Create('Attempt to reference '+url+' as a '+resourceType+' when it''s a '+rrrr.resource.fhirType)
          else
            exit(nil);
        matches.add(rrrr.link);
      end;
    end;
    if matches.Count = 0 then
      exit(nil)
    else
    begin
      t := 0;
      for i := 1 to matches.count - 1 do
      begin
        if isLaterVersion(matches[t], matches[i]) then
          t := i;
      end;
      exit(matches[t]);
    end;
  finally
    matches.free;
  end;
end;

function TTerminologyWorker.findCodeSystem(url, version: String; params: TFHIRTxOperationParams; kinds : TFhirCodeSystemContentModeSet; nullOk, checkVer, noVParams : boolean; op: TFhirOperationOutcomeW): TCodeSystemProvider;
var
  r, r2 : TFHIRMetadataResourceW;
  csh : TFHIRCachedMetadataResource;
  cs : TFhirCodeSystemW;
  ts : TStringlist;
  supplements : TFslList<TFhirCodeSystemW>;
  prov : TCodeSystemProvider;
  msg, mid : String;
begin
  if (url = '') then
    exit(nil);

  if not noVParams then
    version := determineVersion(url, version, params, vaUnknown);

  result := nil;
  csh := nil;
  cs := nil;
  prov := nil;
  try
    csh := findInAdditionalResources(url, version, 'CodeSystem', not nullOk);
    if (csh <> nil) then
    begin
      if csh.LoadedCS = nil then
        csh.loadedCS := TFHIRCodeSystemEntry.Create(csh.Resource.link as TFHIRCodeSystemW);
      cs := csh.resource as TFhirCodeSystemW;
      if (cs.content = cscmComplete) then
      begin
        prov := TFhirCodeSystemProvider.Create(FLanguages.link, FI18n.link, FFactory.link, csh.loadedCS .link);
      end;
    end;

    if (prov = nil) then
      prov := FOnGetCSProvider(self, url, version, FParams, true);

    if (prov = nil) and (cs <> nil) and (cs.content in kinds) then
      prov := TFhirCodeSystemProvider.Create(FLanguages.link, FI18n.link, FFactory.link, csh.loadedCS .link);

    if (prov <> nil) then
    begin
      supplements := loadSupplements(url, version);
      try
        if supplements.Empty then
          result := prov.link
        else
          result := prov.cloneWithSupplements(supplements);
      finally
        supplements.free;
      end;
      if (checkVer) then
        checkVersion(url, prov.version, params, prov.versionAlgorithm, op);
    end
    else if not nullok then
    begin
      if version = '' then
      begin
        mid := 'UNKNOWN_CODESYSTEM_EXP';
        msg := FI18n.translate(mid, FParams.HTTPLanguages, [url]);
      end
      else
      begin
        ts := TStringList.Create;
        try
          FOnListCodeSystemVersions(self, url, ts);
          for csh in FAdditionalResources do
            if (csh.Resource.url = url) and (csh.Resource.version <> '') then
              ts.add(csh.Resource.version);
          if (ts.Count = 0) then
          begin
            mid := 'UNKNOWN_CODESYSTEM_VERSION_EXP_NONE';
            msg := FI18n.translate(mid, FParams.HTTPLanguages, [url, version]);
          end
          else
          begin
            ts.Sort;
            mid := 'UNKNOWN_CODESYSTEM_VERSION_EXP';
            msg := FI18n.translate(mid, FParams.HTTPLanguages, [url, version, ts.CommaText]);
          end
        finally
          ts.free;
        end;
      end;
      raise ETerminologyError.create(msg, itNotFound, oicNotFound, mid);
    end;
  finally
    prov.free;
  end;
end;

function TTerminologyWorker.determineVersion(url, version: String; params: TFHIRTxOperationParams; va : TFHIRVersionAlgorithm): String;
var
  list : TFslList<TFhirExpansionParamsVersionRule>;
  t : TFhirExpansionParamsVersionRule;
  b : boolean;
  msg : String;
begin
  if (params = nil) then
    exit(version);

  result := version;
  list := params.rulesForSystem(url);
  try
    b := false;
    for t in list do
      if t.FMode = fvmOverride then
        if not b then
        begin
          result := t.version;
          FFoundParameters.add(t.asParam);
        end
        else if result <> t.version then
          raise ETerminologyError.create(FI18n.translate('SYSTEM_VERSION_MULTIPLE_OVERRIDE', params.FHTTPLanguages, [url, result, t.version]),
            itException, oicVersionError, 'SYSTEM_VERSION_MULTIPLE_OVERRIDE');
    if (result = '') then
    begin
      b := false;
      for t in list do
        if t.FMode = fvmDefault then
          if not b then
          begin
            result := t.version;
          FFoundParameters.add(t.asParam);
          end
          else if version <> t.version then
            raise ETerminologyError.create(FI18n.translate('SYSTEM_VERSION_MULTIPLE_DEFAULT', params.FHTTPLanguages, [url, result, t.version]),
            itException, oicVersionError, 'SYSTEM_VERSION_MULTIPLE_DEFAULT');
    end;
      for t in list do
        if t.FMode = fvmCheck then
          if (result = '') then
          begin
            result := t.version;
            FFoundParameters.add(t.asParam);
          end
          // if we decide to allow check to guide the selection.
          // waiting for discussion
          //else if TFHIRVersions.isSubset(result, t.version) then
          //  result := t.version;
  finally
    list.free;
  end;
end;

procedure TTerminologyWorker.checkVersion(url, version: String; params: TFHIRTxOperationParams; va : TFHIRVersionAlgorithm; op: TFhirOperationOutcomeW);
var
  list : TFslList<TFhirExpansionParamsVersionRule>;
  t : TFhirExpansionParamsVersionRule;
  b : boolean;
  msg : String;
begin
  if (params <> nil) then
  begin
    list := params.rulesForSystem(url);
    try
      for t in list do
        if t.FMode = fvmCheck then
          if not TFHIRVersions.versionMatches(va, version, t.version) then
          begin
            msg := FI18n.translate('VALUESET_VERSION_CHECK', params.FHTTPLanguages, [url, version, t.version]);
            if op <> nil then
              op.addIssue(isError, itException, '', 'VALUESET_VERSION_CHECK', msg, oicVersionError)
            else
              raise ETerminologyError.create(msg, itException, oicVersionError, 'VALUESET_VERSION_CHECK');
          end;

    finally
      list.free;
    end;
  end;
end;

function TTerminologyWorker.costDiags(e: ETooCostly): ETooCostly;
begin
  e.diagnostics := FOpContext.diagnostics;
  result := e;
end;

function TTerminologyWorker.opName: String;
begin
  result := '??';
end;

function TTerminologyWorker.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FFactory.sizeInBytes(magic));
  inc(result, FParams.sizeInBytes(magic));
  inc(result, FAdditionalResources.sizeInBytes(magic));
end;

procedure TTerminologyWorker.deadCheck(place: String);
var
   time : integer;
begin
  SetThreadStatus(ClassName+'.'+place);
  if FOpContext.deadCheck(time) then
  begin
    FOpContext.addNote(vsHandle, 'Operation took too long @ '+place+' ('+className+')');
    Logging.log('Operation took too long @ '+place+' ('+className+')');
    raise costDiags(ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY_TIME', FParams.HTTPlanguages, ['??', inttostr(time), opName])));
  end;
end;

function TTerminologyWorker.getVersionList(url: String): TStringList;
var
  matches : TFslMetadataResourceList;
  r : TFHIRCachedMetadataResource;
begin
  result := TStringList.Create;
  result.Sorted := true;
  result.Duplicates := Classes.dupIgnore;
  if FAdditionalResources <> nil then
  begin
    for r in FAdditionalResources do
    begin
      if (r.resource.url = url) then
        result.Add(r.resource.version);
    end;
  end;
  FOnListCodeSystemVersions(self, url, result);
end;

function TTerminologyWorker.listVersions(url: String): String;
var
  ts : TStringList;
begin
  ts := getVersionList(url);
  try
    result := ts.CommaText;
  finally
    ts.free;
  end;
end;

function TTerminologyWorker.loadSupplements(url, version : String) : TFslList<TFhirCodeSystemW>;
var
  r : TFHIRCachedMetadataResource;
  cs : TFhirCodeSystemW;
begin
  result := TFslList<TFhirCodeSystemW>.create;
  try
    if (FAdditionalResources <> nil) then
    begin
      for r in FAdditionalResources do
      begin
        if r.resource is TFHIRCodeSystemW then
        begin
          cs := r.resource as TFHIRCodeSystemW;
          if (cs.supplements = url) or (cs.supplements.startsWith(url+'|')) then
            result.add(cs.link);
        end;
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

procedure TTerminologyWorker.checkSupplements(cs : TCodeSystemProvider; src : TFHIRXVersionElementWrapper);
var
  ext : TFHIRExtensionW;
  i : integer;
begin
  if (src <> nil) then
    for ext in src.getExtensionsW(EXT_VSSUPPLEMENT).forEnum do
      if not cs.hasSupplement(opContext, ext.valueAsString) then
        raise ETerminologyError.create('ValueSet depends on supplement '''+ext.valueAsString+''' on '+cs.systemUri+' that is not known', itBusinessRule);
  for i := FRequiredSupplements.count - 1 downto 0 do
    if cs.hasSupplement(opContext, FRequiredSupplements[i]) then
      FRequiredSupplements.delete(i);
end;

{ TFHIRCodeSystemInformationProvider }

procedure TFHIRCodeSystemInformationProvider.lookupCode(opContext : TTxOperationContext; coding: TFHIRCodingW; profile: TFHIRTxOperationParams; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  provider : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  s : String;
  p : TFHIRLookupOpRespPropertyW;
  params : TFHIRTxOperationParams;

  function hasProp(name : String; def : boolean) : boolean;
  begin
    if (props = nil) or (length(props) = 0) then
      result := def
    else
      result := StringArrayExistsInsensitive(props, name) or StringArrayExistsInsensitive(props, '*') ;
  end;
begin
  params := TFHIRTxOperationParams.Create(FLanguages.link);
  try
    params.defaultToLatestVersion := true;
    provider := findCodeSystem(coding.systemUri, coding.version, profile, [cscmComplete, cscmFragment], false, true, false, nil);
    try
      resp.name := provider.name(nil);
      resp.systemUri := provider.systemUri;
      s := provider.version;
      if (s <> '') then
        resp.version := s;
      ctxt := provider.locate(opContext, coding.code);
      try
        if ctxt = nil then
          raise ETerminologyError.Create('Unable to find code '+coding.code+' in '+coding.systemUri+' version '+s, itInvalid);

        if (hasProp('abstract', true) and provider.IsAbstract(opContext, ctxt)) then
        begin
          p := resp.addProp('abstract');
          p.value := FFactory.makeBoolean(true);
        end;
        if (hasProp('inactive', true)) then
        begin
          p := resp.addProp('inactive');
          p.value := FFactory.makeBoolean(provider.IsInactive(opContext, ctxt));
        end;
        if hasProp('definition', true) and (provider.Definition(opContext, ctxt) <> '') then
        begin
          p := resp.addProp('definition');
          p.value := FFactory.makeString(provider.Definition(opContext, ctxt));
        end;
        resp.code := coding.code;
        resp.display := provider.Display(opContext, ctxt, FlangList);
        provider.extendLookup(opContext, FFactory, ctxt, FlangList, props, resp);
      finally
        ctxt.free;
      end;
    finally
      provider.free;
    end;
  finally
    params.free;
  end;
end;

{ TFHIRTxOperationParams }

constructor TFHIRTxOperationParams.Create(Languages : TIETFLanguageDefinitions);
begin
  inherited Create;
  FVersionRules := TFslList<TFhirExpansionParamsVersionRule>.create;
  FProperties := TStringList.create;
  FAltCodeRules := TAlternateCodeOptions.create;
  FDesignations := TStringlist.create;
  FLanguages := languages;

  FGenerateNarrative := true;
end;

procedure TFHIRTxOperationParams.SetHTTPLanguages(value: THTTPLanguageList);
begin
  FHTTPLanguages.free;
  FHTTPLanguages := value;
end;

procedure TFHIRTxOperationParams.SetDisplayLanguages(value: THTTPLanguageList);
begin
  FDisplayLanguages.free;
  FDisplayLanguages := value;
end;

function TFHIRTxOperationParams.GetHasHTTPLanguages: boolean;
begin
  result := (FHTTPLanguages <> nil) and (FHTTPLanguages.source <> '');
end;

function TFHIRTxOperationParams.GetHasDisplayLanguages: boolean;
begin
  result := (FDisplayLanguages <> nil) and (FDisplayLanguages.source <> '');
end;

function TFHIRTxOperationParams.GetHasDesignations: boolean;
begin
  result := designations.Count > 0;
end;

procedure TFHIRTxOperationParams.SetActiveOnly(value : boolean);
begin
  FActiveOnly := value;
  FHasActiveOnly := true;
end;

procedure TFHIRTxOperationParams.SetExcludeNested(value : boolean);
begin
  FExcludeNested := value;
  FHasExcludeNested:= true;
end;

procedure TFHIRTxOperationParams.SetGenerateNarrative(value : boolean);
begin
  FGenerateNarrative := value;
  FHasGenerateNarrative := true;
end;

procedure TFHIRTxOperationParams.SetLimitedExpansion(value : boolean);
begin
  FLimitedExpansion := value;
  FHasLimitedExpansion := true;
end;

procedure TFHIRTxOperationParams.SetExcludeNotForUI(value : boolean);
begin
  FExcludeNotForUI := value;
  FHasExcludeNotForUI := true;
end;

procedure TFHIRTxOperationParams.SetExcludePostCoordinated(value : boolean);
begin
  FExcludePostCoordinated := value;
  FHasExcludePostCoordinated := true;
end;

procedure TFHIRTxOperationParams.SetIncludeDesignations(value : boolean);
begin
  FIncludeDesignations := value;
  FHasIncludeDesignations := true;
end;

procedure TFHIRTxOperationParams.SetIncludeDefinition(value : boolean);
begin
  FIncludeDefinition := value;
  FHasIncludeDefinition := true;
end;

procedure TFHIRTxOperationParams.SetDefaultToLatestVersion(value : boolean);
begin
  FDefaultToLatestVersion := value;
  FHasDefaultToLatestVersion := true;
end;

procedure TFHIRTxOperationParams.SetIncompleteOK(value : boolean);
begin
  FIncompleteOK := value;
  FHasIncompleteOK := true;
end;

procedure TFHIRTxOperationParams.SetDisplayWarning(value : boolean);
begin
  FDisplayWarning := value;
  FHasDisplayWarning := true;
end;

procedure TFHIRTxOperationParams.SetMembershipOnly(value : boolean);
begin
  FMembershipOnly := value;
  FHasMembershipOnly := true;
end;

function TFHIRTxOperationParams.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FVersionRules.sizeInBytes(magic));
  inc(result, FHTTPLanguages.sizeInBytes(magic));
  inc(result, FDisplayLanguages.sizeInBytes(magic));
  inc(result, (FUid.length * sizeof(char)) + 12);
end;

class function TFHIRTxOperationParams.defaultProfile(langDefs : TIETFLanguageDefinitions): TFHIRTxOperationParams;
begin
  result := TFHIRTxOperationParams.Create(langDefs);
end;

procedure TFHIRTxOperationParams.seeParameter(name: String; value: TFHIRObject; isValidation, overwrite: boolean);
begin
  if (value <> nil) then
  begin
    if (name = 'displayLanguage') and (not HasHTTPLanguages or overwrite) then
      DisplayLanguages := THTTPLanguageList.create(FLanguages.link, value.primitiveValue, not isValidation);

    if (name = 'includeAlternateCodes') then
      altCodeRules.seeParam(value.primitiveValue);
    if (name = 'designation') then
      designations.add(value.primitiveValue);
  end;
end;

function TFHIRTxOperationParams.getVersionForRule(systemURI: String; mode: TFhirExpansionParamsVersionRuleMode): String;
var
  rule : TFhirExpansionParamsVersionRule;
begin
  for rule in FVersionRules do
    if (rule.system = systemUri) and (rule.mode = mode) then
      exit(rule.version);
  result := '';
end;

function TFHIRTxOperationParams.rulesForSystem(systemURI: String): TFslList<TFhirExpansionParamsVersionRule>;
var
  t : TFhirExpansionParamsVersionRule;
begin
  result := TFslList<TFhirExpansionParamsVersionRule>.create;
  try
    for t in FVersionRules do
      if t.system = systemURI then
        result.add(t.link);
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRTxOperationParams.seeVersionRule(url: String; mode: TFhirExpansionParamsVersionRuleMode);
var
  sl : TArray<String>;
begin
  sl := url.split(['|']);
  if (Length(sl) = 2) then
    versionRules.Add(TFhirExpansionParamsVersionRule.Create(sl[0], sl[1], mode))
  else
    raise ETerminologyError.Create('Unable to understand '+CODES_TFhirExpansionParamsVersionRuleMode[mode]+' system version "'+url+'"', itInvalid);
end;

function TFHIRTxOperationParams.workingLanguages: THTTPLanguageList;
begin
  if FDisplayLanguages <> nil then
    result := FDisplayLanguages
  else
    result := FHTTPLanguages;
end;

function TFHIRTxOperationParams.langSummary: String;
begin
  if (FDisplayLanguages <> nil) and (FDisplayLanguages.source <> '') then
    result := FDisplayLanguages.asString(false)
  else if (FHTTPLanguages <> nil) and (FHTTPLanguages.source <> '') then
    result := FHTTPLanguages.asString(false)
  else
    result := '--'
end;

function TFHIRTxOperationParams.summary: string;
  procedure b(s : String; v : boolean);
  begin
    if v then
      CommaAdd(result, s);
  end;
  procedure s(s : String; v : String);
  begin
    if v <> '' then
      CommaAdd(result, s+'='+v);
  end;
begin
  result := '';
  s('uid', FUid);
  if (FProperties <> nil) then
    s('properties', FProperties.commaText);
  if (FHTTPLanguages <> nil) then
    s('http-lang' , FHTTPLanguages.asString(true));
  if (FDisplayLanguages <> nil) then
    s('disp-lang' , FDisplayLanguages.asString(true));
  if (FDesignations <> nil) then
    s('designations', FDesignations.commaText);
  b('active-only', FactiveOnly);
  b('exclude-nested', FexcludeNested);
  b('generate-narrative', FGenerateNarrative);
  b('limited-exansion', FlimitedExpansion);
  b('for-ui', FexcludeNotForUI);
  b('exclude-post-coordinated', FexcludePostCoordinated);
  b('include-designations', FincludeDesignations);
  b('include-definition', FincludeDefinition);
  b('membership-only', FMembershipOnly);
  b('default-to-latest', FDefaultToLatestVersion);
  b('incomplete-ok', FIncompleteOK);
  b('display-warning', FDisplayWarning);
end;

function TFHIRTxOperationParams.verSummary: String;
var
  p : TFhirExpansionParamsVersionRule;
begin
  result := '';
  for p in FVersionRules do
    CommaAdd(result, p.asString);
end;

destructor TFHIRTxOperationParams.Destroy;
begin
  FAltCodeRules.free;
  FVersionRules.free;
  FHTTPLanguages.free;
  FDisplayLanguages.free;
  FProperties.free;
  FDesignations.free;
  FValueSetVersionRules.free;
  FLanguages.free;
  inherited;
end;

function TFHIRTxOperationParams.hash: String;
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

  if hasHTTPLanguages then
    s := s + FHTTPLanguages.AsString(true)+'|';
  if hasDisplayLanguages then
    s := s + '*'+FDisplayLanguages.AsString(true)+'|';
  if hasDesignations then
    s := s + FDesignations.commaText+'|';
  for t in FVersionRules do
    s := s + t.asString+'|';
  result := inttostr(HashStringToCode32(s));
end;

function TFHIRTxOperationParams.hasValueSetVersionRules: boolean;
begin
  result := FValueSetVersionRules <> nil;
end;

function TFHIRTxOperationParams.getValueSetVersionRules: TStringList;
begin
  if FValueSetVersionRules = nil then
    FValueSetVersionRules := TStringList.create;
  result := FValueSetVersionRules;
end;

function TFHIRTxOperationParams.link: TFHIRTxOperationParams;
begin
  result := TFHIRTxOperationParams(inherited Link);
end;

function TFHIRTxOperationParams.clone: TFHIRTxOperationParams;
begin                                             
  result := TFHIRTxOperationParams(inherited clone);

end;

procedure TFHIRTxOperationParams.assign(other: TFslObject);
var
  o : TFHIRTxOperationParams;
begin
  inherited assign(other);
  o := other as TFHIRTxOperationParams;
  FLanguages := o.FLanguages.link;
  if (o.FVersionRules <> nil) then
  begin
    if (FVersionRules = nil) then
      FVersionRules := TFslList<TFhirExpansionParamsVersionRule>.create;
    FVersionRules.addAll(o.FVersionRules);
  end;
  if (o.FValueSetVersionRules <> nil) then
  begin
    FValueSetVersionRules := TStringList.create;
    FValueSetVersionRules.AddStrings(o.FValueSetVersionRules);
  end;
  FactiveOnly := o.FactiveOnly;
  FexcludeNested := o.FexcludeNested;
  FGenerateNarrative := o.FGenerateNarrative;
  FlimitedExpansion := o.FlimitedExpansion;
  FexcludeNotForUI := o.FexcludeNotForUI;
  FexcludePostCoordinated := o.FexcludePostCoordinated;
  FincludeDesignations := o.FincludeDesignations;
  FincludeDefinition := o.FincludeDefinition;
  FUid := o.FUid;
  FMembershipOnly := o.FMembershipOnly;
  FDefaultToLatestVersion := o.FDefaultToLatestVersion;
  FIncompleteOK := o.FIncompleteOK;
  FDisplayWarning := o.FDisplayWarning;
  FDiagnostics := o.FDiagnostics;
  FHasactiveOnly := o.FHasactiveOnly;
  FHasExcludeNested := o.FHasExcludeNested;
  FHasGenerateNarrative := o.FHasGenerateNarrative;
  FHasLimitedExpansion := o.FHasLimitedExpansion;

  FHesExcludeNotForUI := o.FHesExcludeNotForUI;
  FHasExcludePostCoordinated := o.FHasExcludePostCoordinated;
  FHasIncludeDesignations := o.FHasIncludeDesignations;
  FHasIncludeDefinition := o.FHasIncludeDefinition;
  FHasDefaultToLatestVersion := o.FHasDefaultToLatestVersion;
  FHasIncompleteOK := o.FHasIncompleteOK;
  FHasexcludeNotForUI := o.FHasIncompleteOK;
  FHasMembershipOnly := o.FHasMembershipOnly;
  FHasDisplayWarning := o.FHasDisplayWarning;
  if (o.FAltCodeRules <> nil) then
  begin
    FAltCodeRules := TAlternateCodeOptions.create;
    FAltCodeRules.all := o.FAltCodeRules.all;
    FAltCodeRules.useTypes.AddStrings(o.FAltCodeRules.useTypes);
  end;

  if (o.FProperties <> nil) then
  begin
    FProperties := TStringList.create;
    FProperties.AddStrings(o.FProperties);                
  end;

  if (o.FDesignations <> nil) then
  begin
    FDesignations := TStringList.create;
    FDesignations.AddStrings(o.FDesignations);
  end;

  if o.FHTTPLanguages <> nil then
    FHTTPLanguages := o.FHTTPLanguages.clone;
  if o.FDisplayLanguages <> nil then
    FDisplayLanguages := o.FHTTPLanguages.clone;
end;


{ TFhirExpansionParamsVersionRule }

constructor TFhirExpansionParamsVersionRule.Create(system, version: String; mode: TFhirExpansionParamsVersionRuleMode);
begin
  inherited Create;
  FSystem := system;
  FVersion := version;
  FMode := mode;
end;

function TFhirExpansionParamsVersionRule.link: TFhirExpansionParamsVersionRule;
begin
  result := TFhirExpansionParamsVersionRule(inherited link);
end;

function TFhirExpansionParamsVersionRule.asString: String;
begin
  result := Fsystem+'#'+Fversion+'/'+inttostr(ord(FMode));
end;

function TFhirExpansionParamsVersionRule.asParam: String;
begin
  result := NAMES_TFhirExpansionParamsVersionRuleMode[FMode]+'='+FSystem+'|'+FVersion;
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


end.

