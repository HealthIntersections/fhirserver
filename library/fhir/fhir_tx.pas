unit fhir_tx;

{$i fhir.inc}

interface


uses
  SysUtils, Classes,
  fsl_base, fsl_collections, fsl_utilities, fsl_http, fsl_lang, fsl_logging, fsl_i18n, fsl_versions, fsl_threads,
  fhir_objects, fhir_common, ftx_service, fhir_factory, fhir_xhtml, fhir_extensions, fhir_uris, fhir_parser,
  fhir_codesystem_service;

 type

  // this is denial of service protection. A terminology operation is not allowed to take too long, and
  // it's not allowed to recurse

  TGetCurrentRequestCountEvent = function : integer of Object;

  { TTerminologyOperationContext }

  TTerminologyOperationContext = class (TFslObject)
  private
    FId : String;
    FStartTime : UInt64;
    FContexts : TStringList;
    FLangList : THTTPLanguageList;
    FI18n : TI18nSupport;
    FDeadTime : Cardinal;
    FOnGetCurrentRequestCount: TGetCurrentRequestCountEvent;
  public
    constructor Create(i18n : TI18nSupport; id : String; langList : THTTPLanguageList; deadTime : cardinal; getRequestCount : TGetCurrentRequestCountEvent);
    destructor Destroy; override;

    property reqId : String read FId;
    function copy : TTerminologyOperationContext;
    function deadCheck(var time : integer) : boolean;
    procedure seeContext(vurl : String);
    procedure clearContexts;

    property OnGetCurrentRequestCount : TGetCurrentRequestCountEvent read FOnGetCurrentRequestCount write FOnGetCurrentRequestCount;
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

    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property mode : TFhirExpansionParamsVersionRuleMode read FMode write FMode;

    function asString : String;
  end;

  { TFHIRTxOperationParams }

  TFHIRTxOperationParams = class (TFslObject)
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
    function link : TFHIRTxOperationParams;

    class function defaultProfile : TFHIRTxOperationParams;

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
          
  TGetProviderEvent = function (sender : TObject; url, version : String; params : TFHIRTxOperationParams; nullOk : boolean) : TCodeSystemProvider of object;
  TGetSystemVersionsEvent = procedure (sender : TObject; url : String; list : TStringlist) of object;

  { TTerminologyWorker }

  TTerminologyWorker = class (TFslObject)
  protected
    FOpContext : TTerminologyOperationContext;
    FFactory : TFHIRFactory;
    FOnGetCSProvider : TGetProviderEvent;
    FOnListCodeSystemVersions : TGetSystemVersionsEvent;
    FAdditionalResources : TFslMetadataResourceList;
    FLanguages : TIETFLanguageDefinitions;
    FI18n : TI18nSupport;
    FLangList : THTTPLanguageList;
    FNoCacheThisOne : boolean;
    FParams : TFHIRTxOperationParams;
    FRequiredSupplements : TStringList;

    function sizeInBytesV(magic : integer) : cardinal; override;
    procedure deadCheck(place : String); virtual;
    function findInAdditionalResources(url, version, resourceType : String; error : boolean) : TFHIRMetadataResourceW;
    function findCodeSystem(url, version : String; params : TFHIRTxOperationParams; nullOk : boolean) : TCodeSystemProvider;
    function listVersions(url : String) : String;
    procedure loadSupplements(cse: TFHIRCodeSystemEntry; url: String);
    procedure checkSupplements(cs: TCodeSystemProvider; src: TFHIRXVersionElementWrapper);
  public
    constructor Create(factory : TFHIRFactory; opContext : TTerminologyOperationContext; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport); overload;
    destructor Destroy; override;
  end;

  { TFHIRCodeSystemInformationProvider }

  TFHIRCodeSystemInformationProvider = class (TTerminologyWorker)
  public
    procedure lookupCode(coding : TFHIRCodingW; profile : TFHIRTxOperationParams; props : TArray<String>; resp : TFHIRLookupOpResponseW);
  end;

const
   CODES_TFhirExpansionParamsVersionRuleMode : array [TFhirExpansionParamsVersionRuleMode] of String = ('Default', 'Check', 'Override');

   LOOKUP_DEAD_TIME_SECS = 30;

implementation

{ TTerminologyOperationContext }

constructor TTerminologyOperationContext.Create(i18n: TI18nSupport; id : String; langList : THTTPLanguageList; deadTime : cardinal; getRequestCount : TGetCurrentRequestCountEvent);
begin
  inherited create;
  FI18n := i18n;
  FId := id;
  FLangList := langList;
  FContexts := TStringList.create;
  FStartTime := GetTickCount64;
  FOnGetCurrentRequestCount := getRequestCount;
  FDeadTime := deadTime;
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
  result := TTerminologyOperationContext.create(FI18n.link, FId, FLangList.link, FDeadTime, OnGetCurrentRequestCount);
  result.FContexts.assign(FContexts);
  result.FStartTime := FStartTime;
end;

function TTerminologyOperationContext.deadCheck(var time : integer): boolean;
var
  dt : UInt64;
  rq : integer;
begin
  time := FDeadTime;
  if UnderDebugger then
    exit(false);

  // once timelimit is hit, living on borrowed time until request counts build
  if assigned(OnGetCurrentRequestCount) and (OnGetCurrentRequestCount > 10) then
    time := time * 5;

  dt := FStartTime + (time * 1000);
  result := GetTickCount64 > dt;
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

{ TTerminologyWorker }

constructor TTerminologyWorker.Create(factory : TFHIRFactory; opContext : TTerminologyOperationContext; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
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
end;

destructor TTerminologyWorker.Destroy;
begin
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


function isLaterVersion(test, base : String) : boolean;
begin
  if TSemanticVersion.isValid(test) and TSemanticVersion.isValid(base) then
    result := TSemanticVersion.isMoreRecent(test, base)
  else
    result := StringCompare(test, base) > 0;
end;

function TTerminologyWorker.findInAdditionalResources(url, version, resourceType : String; error : boolean) : TFHIRMetadataResourceW;
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
      deadCheck('findInAdditionalResources');
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

function TTerminologyWorker.findCodeSystem(url, version: String; params: TFHIRTxOperationParams; nullOk: boolean): TCodeSystemProvider;
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
      exit(TFhirCodeSystemProvider.Create(FLanguages.link, FI18n.link, FFactory.link, cse.link));
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
      exit(TFhirCodeSystemProvider.Create(FLanguages.link, FI18n.link, FFactory.link, cse.link));
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
    logging.log('Operation took too long ('+className+')');
    raise ETooCostly.create(FI18n.translate('VALUESET_TOO_COSTLY_TIME', FParams.languages, ['??', inttostr(time)]));
  end;
end;

function TTerminologyWorker.listVersions(url: String): String;
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

procedure TTerminologyWorker.loadSupplements(cse : TFHIRCodeSystemEntry; url : String);
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

procedure TTerminologyWorker.checkSupplements(cs : TCodeSystemProvider; src : TFHIRXVersionElementWrapper);
var
  ext : TFHIRExtensionW;
  i : integer;
begin
  for ext in src.getExtensionsW(EXT_VSSUPPLEMENT).forEnum do
    if not cs.hasSupplement(ext.valueAsString) then
      raise ETerminologyError.create('ValueSet depends on supplement '''+ext.valueAsString+''' on '+cs.systemUri+' that is not known', itBusinessRule);
  for i := FRequiredSupplements.count - 1 downto 0 do
    if cs.hasSupplement(FRequiredSupplements[i]) then
      FRequiredSupplements.delete(i);
end;

{ TFHIRCodeSystemInformationProvider }

procedure TFHIRCodeSystemInformationProvider.lookupCode(coding: TFHIRCodingW; profile: TFHIRTxOperationParams; props: TArray<String>; resp: TFHIRLookupOpResponseW);
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
  params := TFHIRTxOperationParams.Create;
  try
    params.defaultToLatestVersion := true;
    provider := findCodeSystem(coding.systemUri, coding.version, profile, false);
    try
      resp.name := provider.name(nil);
      resp.systemUri := provider.systemUri;
      s := provider.version;
      if (s <> '') then
        resp.version := s;
      ctxt := provider.locate(coding.code);
      try
        if ctxt = nil then
          raise ETerminologyError.Create('Unable to find code '+coding.code+' in '+coding.systemUri+' version '+s, itInvalid);

        if (hasProp('abstract', true) and provider.IsAbstract(ctxt)) then
        begin
          p := resp.addProp('abstract');
          p.value := FFactory.makeBoolean(true);
        end;
        if (hasProp('inactive', true)) then
        begin
          p := resp.addProp('inactive');
          p.value := FFactory.makeBoolean(provider.IsInactive(ctxt));
        end;
        if hasProp('definition', true) and (provider.Definition(ctxt) <> '') then
        begin
          p := resp.addProp('definition');
          p.value := FFactory.makeString(provider.Definition(ctxt));
        end;
        resp.code := coding.code;
        resp.display := provider.Display(ctxt, FlangList);
        provider.extendLookup(FFactory, ctxt, FlangList, props, resp);
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

constructor TFHIRTxOperationParams.Create;
begin
  inherited;
  FVersionRules := TFslList<TFhirExpansionParamsVersionRule>.create;
  FProperties := TStringList.create;
  FAltCodeRules := TAlternateCodeOptions.create;
  FDesignations := TStringlist.create;

  FGenerateNarrative := true;
end;

procedure TFHIRTxOperationParams.SetLanguages(value: THTTPLanguageList);
begin
  FLanguages.free;
  FLanguages := value;
end;

function TFHIRTxOperationParams.GetHasLanguages: boolean;
begin
  result := (FLanguages <> nil) and (FLanguages.source <> '');
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
  inc(result, FLanguages.sizeInBytes(magic));
  inc(result, (FUid.length * sizeof(char)) + 12);
end;

class function TFHIRTxOperationParams.defaultProfile: TFHIRTxOperationParams;
begin
  result := TFHIRTxOperationParams.Create;
end;

procedure TFHIRTxOperationParams.seeParameter(name: String; value: TFHIRObject; isValidation, overwrite: boolean);
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

function TFHIRTxOperationParams.getVersionForRule(systemURI: String; mode: TFhirExpansionParamsVersionRuleMode): String;
var
  rule : TFhirExpansionParamsVersionRule;
begin
  for rule in FVersionRules do
    if (rule.system = systemUri) and (rule.mode = mode) then
      exit(rule.version);
  result := '';
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

function TFHIRTxOperationParams.langSummary: String;
begin
  if (FLanguages = nil) or (FLanguages.source = '') then
    result := '--'
  else
    result := FLanguages.asString(false);
end;

destructor TFHIRTxOperationParams.Destroy;
begin
  FAltCodeRules.free;
  FVersionRules.free;
  FLanguages.free;
  FProperties.free;
  FDesignations.free;
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

  if hasLanguages then
    s := s + FLanguages.AsString(true)+'|';
  if hasDesignations then
    s := s + FDesignations.commaText+'|';
  for t in FVersionRules do
    s := s + t.asString+'|';
  result := inttostr(HashStringToCode32(s));
end;

function TFHIRTxOperationParams.link: TFHIRTxOperationParams;
begin
  result := TFHIRTxOperationParams(inherited Link);
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


end.

