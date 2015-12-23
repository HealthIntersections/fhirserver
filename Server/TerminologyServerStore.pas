unit TerminologyServerStore;

interface

uses
  SysUtils, Classes, kCritSct, System.Generics.Defaults,
  StringSupport,
  AdvObjects, AdvStringLists, AdvStringMatches, AdvObjectLists, AdvGenerics, AdvExceptions,
  KDBManager,
  FHIRTypes, FHIRResources, FHIRUtilities,
  TerminologyServices, LoincServices, UCUMServices, SnomedServices, RxNormServices, UniiServices, CvxServices, UriServices,
  USStateCodeServices, CountryCodeServices, AreaCodeServices, IETFLanguageCodeServices,
  YuStemmer;

Type

  ETerminologySetup = class (Exception);
  ETerminologyError = class (Exception);


  TLoadedConceptMap = class (TAdvObject)
  private
    FSource: TFhirValueSet;
    FResource: TFhirConceptMap;
    FTarget: TFhirValueSet;
    procedure SetResource(const Value: TFhirConceptMap);
    procedure SetSource(const Value: TFhirValueSet);
    procedure SetTarget(const Value: TFhirValueSet);

    function HasTranslation(list : TFhirConceptMapELementList; system, code : String; out maps : TFhirConceptMapElementTargetList) : boolean; overload;
  public
    Destructor Destroy; override;
    function Link : TLoadedConceptMap; overload;
    Property Source : TFhirValueSet read FSource write SetSource;
    Property Resource : TFhirConceptMap read FResource write SetResource;
    Property Target : TFhirValueSet read FTarget write SetTarget;

    function HasTranslation(system, code : String; out maps : TFhirConceptMapElementTargetList) : boolean; overload;
  end;

  TLoadedConceptMapList = class (TAdvObjectList)
  private
    function getMap(iIndex: integer): TLoadedConceptMap;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    Property map[iIndex : integer] : TLoadedConceptMap read getMap; default;

  end;
  TValueSetProviderContext = class (TCodeSystemProviderContext)
  private
    context : TFhirValueSetCodeSystemConcept;
  public
    constructor Create(context : TFhirValueSetCodeSystemConcept); overload;
    destructor Destroy; override;
  end;

  TFhirValueSetCodeSystemConceptMatch = class (TAdvObject)
  private
    FItem : TFhirValueSetCodeSystemConcept;
    FRating : double;
  public
    Constructor Create(item : TFhirValueSetCodeSystemConcept; rating : double);
    Destructor Destroy; override;
  end;

  TValueSetProviderFilterContext = class (TCodeSystemProviderFilterContext, IComparer<TFhirValueSetCodeSystemConceptMatch>)
  private
    ndx : integer;
    concepts : TAdvList<TFhirValueSetCodeSystemConceptMatch>;

    procedure Add(item : TFhirValueSetCodeSystemConcept; rating : double);
    function Compare(const Left, Right: TFhirValueSetCodeSystemConceptMatch): Integer;
    procedure sort;
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TValueSetProvider = class (TCodeSystemProvider)
  private
    FVs : TFhirValueSet;
    function doLocate(list : TFhirValueSetCodeSystemConceptList; code : String) : TValueSetProviderContext;
    procedure FilterCodes(dest : TValueSetProviderFilterContext; source : TFhirValueSetCodeSystemConceptList; filter : TSearchFilterText);
  public
    constructor Create(vs : TFHIRValueSet); overload;
    destructor Destroy; override;
    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; overload; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(code : String; list : TStringList); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList); override;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
  end;


  // the terminology server maintains a cache of terminology related resources
  // the rest server notifies terminology server whenever this list changes
  // (and at start up)
  TTerminologyServerStore = class (TAdvObject)
  private
    FLoading : boolean;
    FLoinc : TLOINCServices;
    FSnomed : TSnomedServices;
    FUcum : TUcumServices;
    FRxNorm : TRxNormServices;
    FNciMeta : TNciMetaServices;
    FUnii : TUniiServices;
    FCountryCode : TCountryCodeServices;
    FAreaCode : TAreaCodeServices;
    FCvx : TCvxServices;
    FStem : TYuStemmer_8;

    FLastConceptKey : integer;
    FLastClosureKey : integer;
    FLastClosureEntryKey : integer;
    FLastValueSetKey : integer;
    FLastValueSetMemberKey : integer;

    // value sets are indexed 3 ways:
    // by their local url
    // by their canonical url
    // if they're a value set, by their code system url
    FValueSetsById : TAdvMap<TFHIRValueSet>; // by local system's id
    FValueSetsByURL : TAdvMap<TFHIRValueSet>; // by canonical url
    FCodeSystems : TAdvMap<TFHIRValueSet>; // all current value sets that define systems, by their identifier
    FBaseValueSets : TAdvMap<TFHIRValueSet>; // value sets out of the specification - these can be overriden, but they never go away

    FBaseConceptMaps : TAdvMap<TLoadedConceptMap>; // value sets out of the specification - these can be overriden, but they never go away
    FConceptMapsById : TAdvMap<TLoadedConceptMap>;
    FConceptMapsByURL : TAdvMap<TLoadedConceptMap>;

    FProviderClasses : TAdvMap<TCodeSystemProvider>;

    procedure UpdateConceptMaps;
    procedure BuildStems(list : TFhirValueSetCodeSystemConceptList);

    procedure SetLoinc(const Value: TLOINCServices);
    procedure SetSnomed(const Value: TSnomedServices);
    procedure SetUcum(const Value: TUcumServices);
    procedure SetRxNorm(const Value: TRxNormServices);
    procedure SetNciMeta(const Value: TNciMetaServices);
    procedure SetUnii(const Value: TUniiServices);
    procedure SetCountryCode(const Value: TCountryCodeServices);
    procedure SetAreaCode(const Value: TAreaCodeServices);
    procedure SetCvx(const Value: TCvxServices);

    function TrackValueSet(id : String; bOnlyIfNew : boolean) : integer;
  protected
    FLock : TCriticalSection;  // it would be possible to use a read/write lock, but the complexity doesn't seem to be justified by the short amount of time in the lock anyway
    FDB : TKDBManager;
    procedure invalidateVS(id : String); virtual;
  public
    Constructor Create(db : TKDBManager); virtual;
    Destructor Destroy; Override;
    Function Link : TTerminologyServerStore; overload;

    Property Loinc : TLOINCServices read FLoinc write SetLoinc;
    Property Snomed : TSnomedServices read FSnomed write SetSnomed;
    Property Ucum : TUcumServices read FUcum write SetUcum;
    Property RxNorm : TRxNormServices read FRxNorm write SetRxNorm;
    Property NciMeta : TNciMetaServices read FNciMeta write SetNciMeta;
    Property Unii : TUniiServices read FUnii write SetUnii;
    Property CountryCode : TCountryCodeServices read FCountryCode write SetCountryCode;
    Property AreaCode : TAreaCodeServices read FAreaCode write SetAreaCode;
    Property Cvx : TCvxServices read FCvx write SetCvx;
    Property DB : TKDBManager read FDB;

    // maintenance procedures
    procedure SeeSpecificationResource(resource : TFHIRResource);
    procedure SeeTerminologyResource(resource : TFHIRResource);
    procedure DropTerminologyResource(aType : TFhirResourceType; id : String);

    // access procedures. All return values are owned, and must be freed
    Function getProvider(system : String; noException : boolean = false) : TCodeSystemProvider;
    function getValueSetByUrl(url : String) : TFHIRValueSet;
    function getValueSetById(id : String) : TFHIRValueSet;
    function getCodeSystem(url : String) : TFHIRValueSet;
    function hasCodeSystem(url : String) : Boolean;
    function getConceptMapById(id : String) : TLoadedConceptMap;

    // publishing access
    function GetCodeSystemList : TFHIRValueSetList;
    function GetValueSetList : TFHIRValueSetList;
    function GetConceptMapList : TLoadedConceptMapList;
    Property ProviderClasses : TAdvMap<TCodeSystemProvider> read FProviderClasses;

    // database maintenance
    Property Loading : boolean read FLoading write FLoading;
    procedure declareSystems(oConf : TFHIRConformance);
    function supportsSystem(s : String) : boolean;
    function subsumes(uri1, code1, uri2, code2 : String) : boolean;
    function NextClosureKey : integer;
    function NextClosureEntryKey : integer;
    function NextConceptKey : integer;
    function NextValueSetKey : integer;
    function NextValueSetMemberKey : integer;
  end;

implementation

Type
  TAllCodeSystemsProviderFilterPreparationContext = class (TCodeSystemProviderFilterPreparationContext)
  private
    rxnorm : TCodeSystemProviderFilterPreparationContext;
    ncimeta : TCodeSystemProviderFilterPreparationContext;
    snomed : TCodeSystemProviderFilterPreparationContext;
    loinc : TCodeSystemProviderFilterPreparationContext;
    actcode : TCodeSystemProviderFilterPreparationContext;
    unii : TCodeSystemProviderFilterPreparationContext;
  end;

  TAllCodeSystemsProviderFilter = class (TCodeSystemProviderFilterContext)
  private
    rxnormDone : boolean;
    ncimetaDone : boolean;
    uniiDone : boolean;
    snomedDone : boolean;
    loincDone : boolean;
    actcodeDone : boolean;

    rxnorm : TCodeSystemProviderFilterContext;
    ncimeta : TCodeSystemProviderFilterContext;
    unii : TCodeSystemProviderFilterContext;
    snomed : TCodeSystemProviderFilterContext;
    loinc : TCodeSystemProviderFilterContext;
    actcode : TCodeSystemProviderFilterContext;
  end;

  TAllCodeSystemsSource = (acssLoinc, acssSnomed, acssRxNorm, acssNciMeta, acssActCode, acssUnii);

  TAllCodeSystemsProviderContext = class (TCodeSystemProviderContext)
  private
    source : TAllCodeSystemsSource;
    context : TCodeSystemProviderContext;
  end;

  TAllCodeSystemsProvider = class (TCodeSystemProvider)
  private
    FStore: TTerminologyServerStore;
    FActCode : TCodeSystemProvider;
  public
    Constructor Create(store : TTerminologyServerStore);
    Destructor Destroy; override;
    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList); overload; override;
    procedure Displays(code : String; list : TStringList); overload; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); overload; override;
    procedure Close(ctxt : TCodeSystemProviderContext); overload; override;
    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
  end;

function TAllCodeSystemsProvider.TotalCount : integer;
begin
  result := FStore.Snomed.TotalCount + FStore.Loinc.TotalCount + FActCode.TotalCount + FStore.Unii.TotalCount;
  if FStore.RxNorm <> nil then
    result := result + FStore.RxNorm.TotalCount;
  if FStore.NciMeta <> nil then
    result := result + FStore.NciMeta.TotalCount;
end;

function TAllCodeSystemsProvider.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  if (context = nil) then
    result := TotalCount
  else
    raise Exception.Create('Not Created Yet');
end;

function TAllCodeSystemsProvider.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise Exception.Create('Not Created Yet');
end;

function TAllCodeSystemsProvider.system(context : TCodeSystemProviderContext) : String;
var
  c : TAllCodeSystemsProviderContext;
begin
  if Context = nil then
    result := ANY_CODE_VS
  else
  begin
    c := context as TAllCodeSystemsProviderContext;
    case c.source of
      acssLoinc : result := FStore.Loinc.System(c.context);
      acssSnomed : result := FStore.Snomed.System(c.context);
      acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.System(c.context) else result := '??';
      acssNciMeta : if FStore.NciMeta <> nil then result := FStore.NciMeta.System(c.context) else result := '??';
      acssUnii : result := FStore.Unii.System(c.context);
      acssActCode : result := FActCode.System(c.context);
    end;
  end;
end;

function TAllCodeSystemsProvider.getDisplay(code : String):String;
begin
  raise Exception.Create('Not Created Yet');
end;
function TAllCodeSystemsProvider.getPrepContext: TCodeSystemProviderFilterPreparationContext;
var
  ctxt : TAllCodeSystemsProviderFilterPreparationContext;
begin
  ctxt := TAllCodeSystemsProviderFilterPreparationContext.Create;
  try
    if FStore.RxNorm <> nil then
      ctxt.rxnorm := FStore.RxNorm.getPrepContext;
    if FStore.NciMeta <> nil then
      ctxt.NciMeta := FStore.NciMeta.getPrepContext;
    ctxt.unii := nil;
    ctxt.loinc := FStore.Loinc.getPrepContext;
    ctxt.snomed := FStore.Snomed.getPrepContext;
    ctxt.actcode := Factcode.getPrepContext;
    result := ctxt.link;
  finally
    ctxt.Free;
  end;
end;

function TAllCodeSystemsProvider.getDefinition(code : String):String;
begin
  raise Exception.Create('Not Created Yet');
end;
function TAllCodeSystemsProvider.locate(code : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('Not Created Yet');
end;
function TAllCodeSystemsProvider.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('Not Created Yet');
end;
function TAllCodeSystemsProvider.IsAbstract(context : TCodeSystemProviderContext) : boolean;
var
  c : TAllCodeSystemsProviderContext;
begin
  result := true;
  c := context as TAllCodeSystemsProviderContext;
  case c.source of
    acssLoinc : result := FStore.Loinc.IsAbstract(c.context);
    acssSnomed : result := FStore.Snomed.IsAbstract(c.context);
    acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.IsAbstract(c.context) else result := false;
    acssNciMeta : if FStore.NciMeta <> nil then result := FStore.NciMeta.IsAbstract(c.context) else result := false;
    acssUnii : result := FStore.Unii.IsAbstract(c.context);
    acssActCode : result := FActCode.IsAbstract(c.context);
  end;
end;

function TAllCodeSystemsProvider.Code(context : TCodeSystemProviderContext) : string;
var
  c : TAllCodeSystemsProviderContext;
begin
  c := context as TAllCodeSystemsProviderContext;
  case c.source of
    acssLoinc : result := FStore.Loinc.Code(c.context);
    acssSnomed : result := FStore.Snomed.Code(c.context);
    acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.Code(c.context) else result := '??';
    acssNciMeta : if FStore.NciMeta <> nil then result := FStore.NciMeta.Code(c.context) else result := '??';
    acssUnii : result := FStore.Unii.Code(c.context);
    acssActCode : result := FActCode.Code(c.context);
  end;
end;

constructor TAllCodeSystemsProvider.Create(store: TTerminologyServerStore);
begin
  inherited Create;
  FStore := store;
  FActCode := store.getProvider('http://hl7.org/fhir/v3/ActCode');
end;

function TAllCodeSystemsProvider.Display(context : TCodeSystemProviderContext) : string;
var
  c : TAllCodeSystemsProviderContext;
begin
  c := context as TAllCodeSystemsProviderContext;
  case c.source of
    acssLoinc : result := FStore.Loinc.Display(c.context)+' (LOINC: '+FStore.Loinc.Code(c.context)+')';
    acssSnomed : result := FStore.Snomed.Display(c.context)+' (S-CT: '+FStore.Snomed.Code(c.context)+')';
    acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.Display(c.context)+' (RxN: '+FStore.RxNorm.Code(c.context)+')' else result := '';
    acssNciMeta : if FStore.NciMeta <> nil then result := FStore.NciMeta.Display(c.context)+' (RxN: '+FStore.NciMeta.Code(c.context)+')' else result := '';
    acssUnii : result := FStore.Unii.Display(c.context)+' (Unii: '+FStore.Unii.Code(c.context)+')';
    acssActCode : result := FActCode.Display(c.context)+' (ActCode: '+FActCode.Code(c.context)+')';
  end;
end;

function TAllCodeSystemsProvider.Definition(context : TCodeSystemProviderContext) : string;
var
  c : TAllCodeSystemsProviderContext;
begin
  c := context as TAllCodeSystemsProviderContext;
  case c.source of
    acssLoinc : result := FStore.Loinc.Definition(c.context);
    acssSnomed : result := FStore.Snomed.Definition(c.context);
    acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.Definition(c.context) else result := '??';
    acssNciMeta : if FStore.NciMeta <> nil then result := FStore.NciMeta.Definition(c.context) else result := '??';
    acssUnii : result := FStore.Unii.Definition(c.context);
    acssActCode : result := FActCode.Definition(c.context);
  end;
end;
destructor TAllCodeSystemsProvider.Destroy;
begin
  FActCode.Free;
  FStore.Free;
  inherited;
end;

procedure TAllCodeSystemsProvider.Displays(context : TCodeSystemProviderContext; list : TStringList);
begin
  raise Exception.Create('Not Created Yet');
end;
procedure TAllCodeSystemsProvider.Displays(code : String; list : TStringList);
begin
  raise Exception.Create('Not Created Yet');
end;

function TAllCodeSystemsProvider.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
var
  ctxt : TAllCodeSystemsProviderFilter;
begin
  if filter.filter.trim.Length < 3 then
    result := nil
  else
  begin
    ctxt := TAllCodeSystemsProviderFilter.create;
    try
      if FStore.RxNorm <> nil then
        ctxt.rxnorm := FStore.RxNorm.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).rxnorm, sort);
      if FStore.NciMeta <> nil then
        ctxt.NciMeta := FStore.NciMeta.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).NciMeta, sort);
      ctxt.unii := nil; // FStore.Unii.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).unii, sort);
      ctxt.snomed := FStore.snomed.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).snomed, sort);
      ctxt.loinc := FStore.loinc.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).loinc, sort);
      ctxt.actcode := FActCode.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).actcode, sort);
      result := ctxt.Link;
    finally
      ctxt.free;
    end;
  end;
end;

function TAllCodeSystemsProvider.filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('Not Created Yet');
end;
function TAllCodeSystemsProvider.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
var
  ctxt : TAllCodeSystemsProviderFilterPreparationContext;
begin
  result := false;
  ctxt := prep as TAllCodeSystemsProviderFilterPreparationContext;
  if (ctxt <> nil) then
  begin
    FStore.Loinc.prepare(ctxt.loinc);
    FStore.Snomed.prepare(ctxt.snomed);
    if FStore.RxNorm <> nil then
      FStore.RxNorm.prepare(ctxt.rxnorm);
    if FStore.NciMeta <> nil then
      FStore.NciMeta.prepare(ctxt.NciMeta);
//    FStore.FUnii.prepare(ctxt.unii);
    FActCode.prepare(ctxt.actcode);
  end;
end;

function TAllCodeSystemsProvider.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('Not Created Yet');
end;

function TAllCodeSystemsProvider.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
var
  c : TAllCodeSystemsProviderFilter;
begin
  if ctxt = nil then
    result := false
  else
  begin
    c := TAllCodeSystemsProviderFilter(ctxt);
    result := true;
    if not c.snomedDone then
      c.snomedDone := not FStore.Snomed.FilterMore(c.snomed);
    if c.snomedDone then
    begin
      if not c.actcodeDone then
        c.actcodeDone := not FActCode.FilterMore(c.actcode);
      if c.actcodeDone then
      begin
        if not c.loincDone then
          c.loincDone := not FStore.Loinc.FilterMore(c.loinc);
        if c.loincDone then
        begin
          if not c.uniiDone then
            c.uniiDone := true; // not FStore.unii.FilterMore(c.unii);
          if c.uniiDone then
          begin
            if FStore.RxNorm = nil then
              result := false
            else
            begin
              if not c.rxNormDone then
                c.rxNormDone := not FStore.RxNorm.FilterMore(c.rxNorm);
              result := not c.rxnormDone;
            end;
            if not result and not c.ncimetaDone and (FStore.NciMeta <> nil) then
            begin
              c.ncimetaDone := not FStore.NciMeta.FilterMore(c.rxNorm);
              result := not c.ncimetaDone;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TAllCodeSystemsProvider.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  c : TAllCodeSystemsProviderContext;
  d : TAllCodeSystemsProviderFilter;
begin
  d := ctxt as TAllCodeSystemsProviderFilter;
  c := TAllCodeSystemsProviderContext.create;
  try
    if not d.snomedDone then
    begin
      c.source := acssSnomed;
      c.context := FStore.Snomed.FilterConcept(d.snomed);
    end
    else if not d.actCodeDone then
    begin
      c.source := acssActCode;
      c.context := FActCode.FilterConcept(d.actcode);
    end
    else if not d.loincDone then
    begin
      c.source := acssLoinc;
      c.context := FStore.Loinc.FilterConcept(d.loinc);
    end
    else if not d.uniiDone then
    begin
      c.source := acssunii;
      c.context := FStore.unii.FilterConcept(d.unii);
    end
    else if FStore.RxNorm = nil then
    begin
      // nothing
    end
    else
    begin
      c.source := acssRxNorm;
      c.context := FStore.RxNorm.FilterConcept(d.rxNorm);
    end;
    result := c.link;
  finally
    c.free;
  end;
end;

function TAllCodeSystemsProvider.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise Exception.Create('Not Created Yet');
end;
function TAllCodeSystemsProvider.isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean;
begin
  result := true;
end;

procedure TAllCodeSystemsProvider.Close(ctxt : TCodeSystemProviderFilterPreparationContext);
var
  c : TAllCodeSystemsProviderFilterPreparationContext;
begin
  c := ctxt as TAllCodeSystemsProviderFilterPreparationContext;
  if FStore.RxNorm <> nil then
    FStore.RxNorm.Close(c.rxnorm);
  if FStore.NciMeta <> nil then
    FStore.NciMeta.Close(c.NciMeta);
  FStore.unii.Close(c.unii);
  FStore.Loinc.Close(c.loinc);
  FStore.Snomed.Close(c.snomed);
  FActCode.Close(c.actcode);
  ctxt.free;
end;

procedure TAllCodeSystemsProvider.Close(ctxt : TCodeSystemProviderFilterContext);
var
  c : TAllCodeSystemsProviderFilter;
begin
  c := ctxt as TAllCodeSystemsProviderFilter;
  if (c <> nil) then
  begin
    if FStore.RxNorm <> nil then
      FStore.RxNorm.Close(c.rxnorm);
    if FStore.NciMeta <> nil then
      FStore.NciMeta.Close(c.NciMeta);
    FStore.Unii.Close(c.unii);
    FStore.Loinc.Close(c.loinc);
    FStore.Snomed.Close(c.snomed);
    FActCode.Close(c.actcode);
  end;
  ctxt.free;
end;

procedure TAllCodeSystemsProvider.Close(ctxt : TCodeSystemProviderContext);
var
  c : TAllCodeSystemsProviderContext;
begin
  c := ctxt as TAllCodeSystemsProviderContext;
  case c.source of
    acssLoinc : FStore.Loinc.Close(c.context);
    acssSnomed : FStore.Snomed.Close(c.context);
    acssRxNorm : if FStore.RxNorm <> nil then FStore.RxNorm.Close(c.context);
    acssNciMeta : if FStore.NciMeta <> nil then FStore.NciMeta.Close(c.context);
    acssUnii : FStore.Unii.Close(c.context);
    acssActCode : FActCode.Close(c.context);
  end;
  ctxt.free;
end;


{ TTerminologyServerStore }

procedure TTerminologyServerStore.BuildStems(list: TFhirValueSetCodeSystemConceptList);
var
  i : integer;
  ts : TAdvStringList;
  c : TFhirValueSetCodeSystemConcept;
  s, t : String;
begin
  for i := 0 to list.Count - 1 do
  begin
    c := list[i];
    ts := TAdvStringList.Create;
    try
      t := c.display;
      while (t <> '') Do
      begin
        StringSplit(t, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='], s, t);
        if (s <> '') Then
          ts.Add(lowercase(FStem.calc(s)));
      end;
      ts.SortAscending;
      c.Tag := ts.Link;
    finally
      ts.Free;
    end;
    BuildStems(c.conceptList);
  end;
end;

constructor TTerminologyServerStore.Create(db : TKDBManager);
var
  conn : TKDBConnection;
  p : TCodeSystemProvider;
begin
  inherited Create;
  FLock := TCriticalSection.Create('Terminology Server Store');
  FProviderClasses := TAdvMap<TCodeSystemProvider>.Create;

  FDB := db;

  FValueSetsById := TAdvMap<TFhirValueSet>.create;
  FValueSetsByURL := TAdvMap<TFhirValueSet>.create;
  FCodeSystems := TAdvMap<TFhirValueSet>.create;
  FBaseValueSets := TAdvMap<TFhirValueSet>.create;

  FBaseConceptMaps := TAdvMap<TLoadedConceptMap>.create;
  FConceptMapsById := TAdvMap<TLoadedConceptMap>.create;
  FConceptMapsByURL := TAdvMap<TLoadedConceptMap>.create;

  p := TIETFLanguageCodeServices.Create;
  FProviderClasses.Add(p.system(nil), p);
  p := TUriServices.Create();
  FProviderClasses.Add(p.system(nil), p);

  FStem := GetStemmer_8('english');

  conn := db.GetConnection('loadTerminologyKeys');
  try
    FLastConceptKey := conn.CountSQL('select Max(ConceptKey) from Concepts');
    FLastClosureKey := conn.CountSQL('select Max(ClosureKey) from Closures');
    FLastClosureEntryKey := conn.CountSQL('select Max(ClosureEntryKey) from ClosureEntries');
    FLastValueSetKey := conn.CountSQL('select Max(ValueSetKey) from ValueSets');
    FLastValueSetMemberKey := conn.CountSQL('select Max(ValueSetMemberKey) from ValueSetMembers');
    conn.Release;
  except
    on e:exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TTerminologyServerStore.declareSystems(oConf: TFHIRConformance);
var
  e : TFhirExtension;
  cp : TCodeSystemProvider;
  s : String;
begin
  for cp in FProviderClasses.Values do
  begin
    e := oConf.addExtension('http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system', nil);
    e.Tags['summary'] := 'true';
    s := cp.system(nil);
    e.addExtension('system', TFHIRUri.Create(s));
    s := cp.version(nil);
    if (s <> '') then
      e.addExtension('version', s);
    s := cp.name(nil);
    if (s <> '') then
      e.addExtension('name', s);
  end;
end;

destructor TTerminologyServerStore.Destroy;
begin
  FStem.Free;
  FValueSetsById.Free;
  FValueSetsByURL.Free;
  FCodeSystems.Free;
  FBaseValueSets.Free;

  FBaseConceptMaps.Free;
  FConceptMapsById.Free;
  FConceptMapsByURL.Free;

  FProviderClasses.Free;

  FLoinc.free;
  FSnomed.free;
  FUnii.Free;
  FCvx.Free;
  FUcum.free;
  FLock.Free;
  FRxNorm.Free;
  FNciMeta.Free;
  FCountryCode.Free;
  FAreaCode.Free;
  FDB.free;
  inherited;
end;

procedure TTerminologyServerStore.SetLoinc(const Value: TLOINCServices);
begin
  if FLoinc <> nil then
    FProviderClasses.Remove(FLoinc.system(nil));
  FLoinc.Free;
  FLoinc := Value;
  if FLoinc <> nil then
    FProviderClasses.add(FLoinc.system(nil), FLoinc.Link);
end;

procedure TTerminologyServerStore.SetRxNorm(const Value: TRxNormServices);
begin
  if FRxNorm <> nil then
    FProviderClasses.Remove(FRxNorm.system(nil));
  FRxNorm.Free;
  FRxNorm := Value;
  if FRxNorm <> nil then
    FProviderClasses.add(FRxNorm.system(nil), FRxNorm.Link);
end;

procedure TTerminologyServerStore.SetNciMeta(const Value: TNciMetaServices);
begin
  if FNciMeta <> nil then
    FProviderClasses.Remove(FNciMeta.system(nil));
  FNciMeta.Free;
  FNciMeta := Value;
  if FNciMeta <> nil then
    FProviderClasses.add(FNciMeta.system(nil), FNciMeta.Link);
end;

procedure TTerminologyServerStore.SetUnii(const Value: TUniiServices);
begin
  if FUnii <> nil then
    FProviderClasses.Remove(FUnii.system(nil));
  FUnii.Free;
  FUnii := Value;
  if FUnii <> nil then
    FProviderClasses.add(FUnii.system(nil), FUnii.Link);
end;

function TTerminologyServerStore.supportsSystem(s: String): boolean;
var
  p : TCodeSystemProvider;
begin
  p := getProvider(s, true);
  try
    result := p <> nil;
  finally
    p.Free;
  end;
end;

procedure TTerminologyServerStore.SetCountryCode(const Value: TCountryCodeServices);
begin
  if FCountryCode <> nil then
    FProviderClasses.Remove(FCountryCode.system(nil));
  FCountryCode.Free;
  FCountryCode := Value;
  if FCountryCode <> nil then
    FProviderClasses.add(FCountryCode.system(nil), FCountryCode.Link);
end;

procedure TTerminologyServerStore.SetAreaCode(const Value: TAreaCodeServices);
begin
  if FAreaCode <> nil then
    FProviderClasses.Remove(FAreaCode.system(nil));
  FAreaCode.Free;
  FAreaCode := Value;
  if FAreaCode <> nil then
    FProviderClasses.add(FAreaCode.system(nil), FAreaCode.Link);
end;

procedure TTerminologyServerStore.SetCvx(const Value: TCvxServices);
begin
  if FCvx <> nil then
    FProviderClasses.Remove(FCvx.system(nil));
  FCvx.Free;
  FCvx := Value;
  if FCvx <> nil then
    FProviderClasses.add(FCvx.system(nil), FCvx.Link);
end;

procedure TTerminologyServerStore.SetSnomed(const Value: TSnomedServices);
begin
  if FSnomed <> nil then
    FProviderClasses.Remove(FSnomed.system(nil));
  FSnomed.Free;
  FSnomed := Value;
  if FSnomed <> nil then
    FProviderClasses.add(FSnomed.system(nil), FSnomed.Link);
end;

procedure TTerminologyServerStore.SetUcum(const Value: TUcumServices);
begin
  if FUcum <> nil then
    FProviderClasses.Remove(FUcum.system(nil));
  FUcum.Free;
  FUcum := Value;
  if FUcum <> nil then
    FProviderClasses.add(FUcum.system(nil), FUcum.Link);
end;

function TTerminologyServerStore.TrackValueSet(id: String; bOnlyIfNew : boolean): integer;
var
  conn : TKDBConnection;
begin
  conn := FDB.GetConnection('TrackValueSet');
  try
    result := Conn.CountSQL('Select ValueSetKey from ValueSets where URL = '''+SQLWrapString(id)+'''');
    if result = 0 then
    begin
      result := NextValueSetKey;
      Conn.ExecSQL('Insert into ValueSets (ValueSetKey, URL, NeedsIndexing) values ('+inttostr(result)+', '''+SQLWrapString(id)+''', 1)');
    end
    else if not bOnlyIfNew and not Loading then
      Conn.ExecSQL('Update ValueSets set NeedsIndexing = 1 where ValueSetKey = '+inttostr(result));
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

// ----  maintenance procedures ------------------------------------------------

function urlTail(path : String) : String;
begin
  result := path.substring(path.lastIndexOf('/')+1);
end;

procedure TTerminologyServerStore.SeeSpecificationResource(resource : TFHIRResource);
var
  vs : TFhirValueSet;
  cm : TLoadedConceptMap;
begin
  FLock.Lock('SeeSpecificationResource');
  try
    if (resource.ResourceType = frtValueSet) then
    begin
      vs := TFhirValueSet(resource);
      vs.Tags['tracker'] := inttostr(TrackValueSet(vs.url, true));
      if (vs.url = 'http://hl7.org/fhir/ValueSet/ucum-common') then
        FUcum.SetCommonUnits(vs.Link);

      FBaseValueSets.AddOrSetValue(vs.url, vs.Link);
      FValueSetsById.AddOrSetValue(vs.id, vs.Link);
      FValueSetsByUrl.AddOrSetValue(vs.url, vs.Link);
      if (vs.codeSystem <> nil) then
      begin
        FCodeSystems.AddOrSetValue(vs.codeSystem.system, vs.Link);
        BuildStems(vs.codeSystem.conceptList);
      end;
      UpdateConceptMaps;
    end
    else if (resource.ResourceType = frtConceptMap) then
    begin
      cm := TLoadedConceptMap.Create;
      try
        cm.Resource := TFhirConceptMap(resource).Link;
        cm.Source := getValueSetByUrl(TFhirReference(cm.Resource.source).reference);
        cm.Target := getValueSetByUrl(TFhirReference(cm.Resource.target).reference);
        FConceptMapsById.AddOrSetValue(cm.Resource.id, cm.Link);
        FConceptMapsByURL.AddOrSetValue(cm.Resource.url, cm.Link);
        FBaseConceptMaps.AddOrSetValue(cm.Resource.url, cm.Link);
      finally
        cm.Free;
      end;
    end
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.SeeTerminologyResource(resource : TFHIRResource);
var
  vs : TFhirValueSet;
  cm : TLoadedConceptMap;
begin
  resource.checkNoImplicitRules('Repository.SeeResource', 'Resource');
  TFhirDomainResource(resource).checkNoModifiers('Repository.SeeResource', 'Resource');
  FLock.Lock('SeeTerminologyResource');
  try
    if (resource.ResourceType = frtValueSet) then
    begin
      vs := TFhirValueSet(resource);
      vs.Tags['tracker'] := inttostr(TrackValueSet(vs.url, false));
      FValueSetsById.AddOrSetValue(vs.id, vs.Link);
      FValueSetsByUrl.AddOrSetValue(vs.url, vs.Link);
      invalidateVS(vs.url);
      if (vs.codeSystem <> nil) then
      begin
        FCodeSystems.AddOrSetValue(vs.codeSystem.system, vs.Link);
        BuildStems(vs.codeSystem.conceptList);
      end;
      UpdateConceptMaps;
    end
    else if (resource.ResourceType = frtConceptMap) then
    begin
      cm := TLoadedConceptMap.Create;
      try
        cm.Resource := TFhirConceptMap(resource).Link;
        cm.Source := getValueSetByUrl(TFhirReference(cm.Resource.source).reference);
        cm.Target := getValueSetByUrl(TFhirReference(cm.Resource.target).reference);
        FConceptMapsById.AddOrSetValue(cm.Resource.id, cm.Link);
        FConceptMapsByURL.AddOrSetValue(cm.Resource.url, cm.Link);
      finally
        cm.Free;
      end;
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.DropTerminologyResource(aType : TFhirResourceType; id : String);
var
  vs, vs1 : TFhirValueSet;
  cm, cm1 : TLoadedConceptMap;
begin
  vs := nil;
  FLock.Lock('DropTerminologyResource');
  try
    if (aType = frtValueSet) then
    begin
      vs := FValueSetsById[id];
      if vs <> nil then
      begin
        vs1 := FBaseValueSets[vs.url];
        FValueSetsByURL.Remove(vs.url);
        if (vs.codeSystem <> nil) then
          FCodeSystems.Remove(vs.codeSystem.system);
        FValueSetsById.Remove(vs.id); // vs is no longer valid

        // add the base one back if we are dropping a value set that overrides it
        // current logical flaw: what if there's another one that overrides this? how do we prevent or deal with this?
        if vs1 <> nil then
        begin
          FValueSetsById.AddOrSetValue(vs.url, vs1.Link);
          if (vs1.codeSystem <> nil) then
            FCodeSystems.AddOrSetValue(vs1.codeSystem.system, vs1.Link);
        end;
        UpdateConceptMaps;
      end;
    end
    else if (aType = frtConceptMap) then
    begin
      cm := FConceptMapsById[id];
      if cm <> nil then
      begin
        cm1 := FBaseConceptMaps[cm.Resource.url];
        FConceptMapsByURL.Remove(cm.Resource.url);
        FConceptMapsByid.Remove(id); // cm is no longer valid

        // add the base one back if we are dropping a concept map that overrides it
        // current logical flaw: what if there's another one that overrides this? how do we prevent or deal with this?
        if cm1 <> nil then
        begin
          FConceptMapsById.AddOrSetValue(cm1.Resource.id, cm1.Link);
          FConceptMapsByURL.AddOrSetValue(cm1.Resource.url, cm1.Link);
        end;
      end;
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.UpdateConceptMaps;
var
  cm : TLoadedConceptMap;
begin
  assert(FLock.LockedToMe);
  for cm in FConceptMapsById.values do
  begin
    cm.Source := getValueSetByUrl(TFhirReference(cm.Resource.source).reference);
    if (cm.Source = nil) then
      cm.Source := getValueSetById(TFhirReference(cm.Resource.source).reference);
    cm.Target := getValueSetByUrl(TFhirReference(cm.Resource.target).reference);
    if (cm.Target = nil) then
      cm.Target := getValueSetById(TFhirReference(cm.Resource.target).reference);
  end;
end;

//---- access procedures. All return values are owned, and must be freed -------

function TTerminologyServerStore.getCodeSystem(url: String): TFHIRValueSet;
begin
  FLock.Lock('getValueSetByUrl');
  try
    if FCodeSystems.ContainsKey(url) then
      result := FCodeSystems[url].Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.GetCodeSystemList: TFHIRValueSetList;
var
  vs : TFhirValueSet;
begin
  result := TFHIRValueSetList.Create;
  try
    FLock.Lock('GetCodeSystemList');
    try
      for vs in FCodeSystems.values do
        result.add(vs.link);
    finally
      FLock.Unlock;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;


function TTerminologyServerStore.getConceptMapById( id: String): TLoadedConceptMap;
begin
  FLock.Lock('getValueSetByUrl');
  try
    if FConceptMapsById.ContainsKey(id) then
      result := FConceptMapsById[id].Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.GetConceptMapList: TLoadedConceptMapList;
var
  cm : TLoadedConceptMap;
begin
  result := TLoadedConceptMapList.Create;
  try
    FLock.Lock('GetConceptMapList');
    try
      for cm in FConceptMapsById.values do
        result.Add(TLoadedConceptMap(cm.Link));
    finally
      FLock.Unlock;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TTerminologyServerStore.GetValueSetList: TFHIRValueSetList;
var
  vs : TFhirValueSet;
begin
  result := TFHIRValueSetList.Create;
  try
    FLock.Lock('GetValueSetList');
    try
      for vs in FValueSetsById.values do
        result.Add(TFhirValueSet(vs.Link));
    finally
      FLock.Unlock;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;


Function TTerminologyServerStore.getProvider(system : String; noException : boolean = false) : TCodeSystemProvider;
begin
  result := nil;

  if FProviderClasses.ContainsKey(system) then
    result := FProviderClasses[system].Link
  else if system = ANY_CODE_VS then
    result := TAllCodeSystemsProvider.create(self.link)
  else
  begin
    FLock.Lock('getProvider');
    try
      if FCodeSystems.ContainsKey(system) then
        result := TValueSetProvider.create(FCodeSystems[system].link);
    finally
      FLock.Unlock;
    end;
  end;

  if (result = nil) and not noException then
    raise ETerminologySetup.create('unable to provide support for code system '+system);
end;


function TTerminologyServerStore.getValueSetById(id: String): TFHIRValueSet;
begin
  FLock.Lock('getValueSetByUrl');
  try
    if FValueSetsById.ContainsKey(id) then
      result := FValueSetsById[id].Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.getValueSetByUrl(url : String) : TFHIRValueSet;
begin
  FLock.Lock('getValueSetByUrl');
  try
    if FValueSetsByUrl.ContainsKey(url) then
      result := FValueSetsByUrl[url].Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.hasCodeSystem(url: String): Boolean;
begin
  FLock.Lock('getValueSetByUrl');
  try
    result := FCodeSystems.ContainsKey(url);
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.invalidateVS(id: String);
begin
end;

function TTerminologyServerStore.Link: TTerminologyServerStore;
begin
  result := TTerminologyServerStore(inherited Link);
end;

function TTerminologyServerStore.NextConceptKey: integer;
begin
  FLock.Lock;
  try
    inc(FLastConceptKey);
    result := FLastConceptKey;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.NextValueSetKey: integer;
begin
  FLock.Lock;
  try
    inc(FLastValueSetKey);
    result := FLastValueSetKey;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.NextValueSetMemberKey: integer;
begin
  FLock.Lock;
  try
    inc(FLastValueSetMemberKey);
    result := FLastValueSetMemberKey;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.NextClosureEntryKey: integer;
begin
  FLock.Lock;
  try
    inc(FLastClosureEntryKey);
    result := FLastClosureEntryKey;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.NextClosureKey: integer;
begin
  FLock.Lock;
  try
    inc(FLastClosureKey);
    result := FLastClosureKey;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.subsumes(uri1, code1, uri2, code2: String): boolean;
var
  prov : TCodeSystemProvider;
  loc :  TCodeSystemProviderContext;
begin
  result := false;
  if (uri1 <> uri2) then
    result := false // todo later - check that concept maps
  else if (snomed <> nil) and (uri1 = Snomed.system(nil)) then
    result := Snomed.Subsumes(code1, code2)
  else
  begin
    prov := getProvider(uri1, true);
    if prov <> nil then
    begin
      try
        loc := prov.locateIsA(code2, code1);
        result := Loc <> nil;
        prov.Close(loc);
      finally
        prov.Free;
      end;
    end;
  end;
end;


{ TValueSetProvider }

constructor TValueSetProvider.create(vs: TFHIRValueSet);
begin
  Create;
  FVs := vs
end;

function TValueSetProvider.Definition(context: TCodeSystemProviderContext): string;
begin
  result := TValueSetProviderContext(context).context.definition;
end;

destructor TValueSetProvider.destroy;
begin
  FVs.free;
  inherited;
end;

function TValueSetProvider.ChildCount(context: TCodeSystemProviderContext): integer;
begin
  if context = nil then
    result := FVs.codeSystem.conceptList.count
  else
    result := TValueSetProviderContext(context).context.conceptList.count;
end;

procedure TValueSetProvider.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.Free;
end;

function TValueSetProvider.Code(context: TCodeSystemProviderContext): string;
begin
  result := TValueSetProviderContext(context).context.code;
end;

function TValueSetProvider.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
begin
  if context = nil then
    result := TValueSetProviderContext.create(FVs.codeSystem.conceptList[ndx])
  else
    result := TValueSetProviderContext.create(TValueSetProviderContext(context).context.conceptList[ndx]);
end;

function TValueSetProvider.Display(context: TCodeSystemProviderContext): string;
begin
  result := TValueSetProviderContext(context).context.display;
end;

procedure TValueSetProvider.Displays(context: TCodeSystemProviderContext; list: TStringList);
begin
  list.Add(Display(context));
end;

procedure TValueSetProvider.Displays(code: String; list: TStringList);
begin
  list.Add(getDisplay(code));
end;

function TValueSetProvider.InFilter(ctxt: TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext): Boolean;
var
  cm : TFhirValueSetCodeSystemConceptMatch;
  c : TFhirValueSetCodeSystemConcept;
begin
  result := false;
  c := TValueSetProviderContext(concept).context;
  for cm in TValueSetProviderFilterContext(ctxt).concepts do
    if cm.FItem = c then
    begin
      result := true;
      exit;
    end;
end;

function TValueSetProvider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := (TValueSetProviderContext(context).context.abstractElement <> nil) and TValueSetProviderContext(context).context.abstract;
end;

function TValueSetProvider.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TValueSetProvider.getDefinition(code: String): String;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(code);
  try
    if (ctxt = nil) then
      raise ETerminologyError.create('Unable to find '+code+' in '+system(nil))
    else
      result := Definition(ctxt);
  finally
    Close(ctxt);
  end;
end;

function TValueSetProvider.getDisplay(code: String): String;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(code);
  try
    if (ctxt = nil) then
      raise Exception.create('Unable to find '+code+' in '+system(nil))
    else
      result := Display(ctxt);
  finally
    Close(ctxt);
  end;
end;

function TValueSetProvider.doLocate(list : TFhirValueSetCodeSystemConceptList; code : String) : TValueSetProviderContext;
var
  i : integer;
  c : TFhirValueSetCodeSystemConcept;
begin
  result := nil;
  for i := 0 to list.count - 1 do
  begin
    c := list[i];
    if (c.code = code) then
    begin
      result := TValueSetProviderContext.Create(c.Link);
      exit;
    end;
    result := doLocate(c.conceptList, code);
    if result <> nil then
      exit;
  end;
end;

function TValueSetProvider.locate(code: String): TCodeSystemProviderContext;
begin
  result := DoLocate(FVS.codeSystem.conceptList, code);
end;

function TValueSetProvider.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean): TCodeSystemProviderFilterContext;
var
  res : TValueSetProviderFilterContext;
begin
  res := TValueSetProviderFilterContext.Create;
  try
    FilterCodes(res, Fvs.codeSystem.conceptList, filter);
    res.sort;
    result := res.Link;
  finally
    res.Free;
  end;
end;

function TValueSetProvider.system(context : TCodeSystemProviderContext): String;
begin
  result := Fvs.codeSystem.system;
end;

function TValueSetProvider.TotalCount: integer;
function count(item : TFhirValueSetCodeSystemConcept) : integer;
var
  i : integer;
begin
  result := 1;
  for i := 0 to item.conceptList.count - 1 do
    inc(result, count(item.conceptList[i]));
end;
var
  i : integer;
begin
  result := 0;
  for i := 0 to FVs.codeSystem.conceptList.count - 1 do
    inc(result, count(FVs.codeSystem.conceptList[i]));
end;

procedure TValueSetProvider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  ctxt.Free;
end;

procedure iterateCodes(base : TFhirValueSetCodeSystemConcept; list : TValueSetProviderFilterContext);
var
  i : integer;
begin
  list.Add(base.Link, 0);
  for i := 0 to base.conceptList.count - 1 do
    iterateCodes(base.conceptList[i], list);
end;

function TValueSetProvider.filter(prop: String; op: TFhirFilterOperatorEnum; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  code : TValueSetProviderContext;
  ts : TStringList;
  i: Integer;
begin
  if (op = FilterOperatorIsA) and (prop = 'concept') then
  begin
    code := doLocate(FVs.codeSystem.conceptList, value);
    try
      if code = nil then
        raise Exception.Create('Unable to locate code '+value)
      else
      begin
        result := TValueSetProviderFilterContext.create;
        try
          iterateCodes(code.context, result as TValueSetProviderFilterContext);
          result.link;
        finally
          result.Free;
        end;
      end;
    finally
      Close(code)
    end;
  end
  else if (op = FilterOperatorIn) and (prop = 'concept') then
  begin
    result := TValueSetProviderFilterContext.Create;
    try
      ts := TStringList.Create;
      try
        ts.CommaText := value;
        for i := 0 to ts.Count - 1 do
        begin
          code := doLocate(FVs.codeSystem.conceptList, value);
          try
            if code = nil then
              raise Exception.Create('Unable to locate code '+value)
            else
              TValueSetProviderFilterContext(result).Add(code.context.Link, 0);
          finally
            Close(code)
          end;
        end;
      finally
        ts.Free;
      end;
      result.link;
    finally
      result.Free;
    end;
  end
  else
    result := nil;
end;

procedure TValueSetProvider.FilterCodes(dest : TValueSetProviderFilterContext; source: TFhirValueSetCodeSystemConceptList; filter : TSearchFilterText);
var
  i : integer;
  code : TFhirValueSetCodeSystemConcept;
  rating : double;
begin
  for i := 0 to source.Count - 1 do
  begin
    code := source[i];
    if filter.passes(code.tag as TAdvStringList, rating) then
      dest.Add(code.Link, rating);
    filterCodes(dest, code.conceptList, filter);
  end;
end;

function TValueSetProvider.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  inc(TValueSetProviderFilterContext(ctxt).ndx);
  result := TValueSetProviderFilterContext(ctxt).ndx < TValueSetProviderFilterContext(ctxt).concepts.Count;
end;

function TValueSetProvider.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  context : TValueSetProviderFilterContext;
begin
  context := TValueSetProviderFilterContext(ctxt);
  result := TValueSetProviderContext.Create(context.concepts[context.ndx].FItem.Link)
end;

function TValueSetProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
var
  context : TValueSetProviderFilterContext;
  i : integer;
begin
  result := nil;
  context := TValueSetProviderFilterContext(ctxt);
  for i := 0 to context.concepts.Count - 1 do
    if context.concepts[i].FItem.code = code  then
    begin
      result := TValueSetProviderContext.Create(context.concepts[i].FItem.Link);
      break;
    end;
end;


function TValueSetProvider.locateIsA(code, parent: String): TCodeSystemProviderContext;
var
  p : TValueSetProviderContext;
begin
  result := nil;
  p := Locate(parent) as TValueSetProviderContext;
  if (p <> nil) then
    try
      if (p.context.code = code) then
        result := p.Link
      else
        result := doLocate(p.context.conceptList, code);
    finally
      p.free;
    end;
end;

{ TLoadedConceptMap }

destructor TLoadedConceptMap.Destroy;
begin
  FResource.Free;
  FSource.Free;
  FTarget.Free;
  inherited;
end;

function TLoadedConceptMap.HasTranslation(system, code : String; out maps : TFhirConceptMapElementTargetList): boolean;
begin
  result := HasTranslation(Resource.conceptList, system, code, maps);
end;

function TLoadedConceptMap.Link: TLoadedConceptMap;
begin
  result := TLoadedConceptMap(inherited Link);
end;

function TLoadedConceptMap.HasTranslation(list : TFhirConceptMapELementList; system, code : String; out maps : TFhirConceptMapElementTargetList): boolean;
var
  i : integer;
  c : TFhirConceptMapElement;
begin
  result := false;
  for i := 0 to list.Count - 1 do
  begin
    c := list[i];
    if (c.codeSystem = system) and (c.code = code) then
    begin
      maps := c.targetList.Link;
      result := true;
      exit;
    end;
  end;
end;

procedure TLoadedConceptMap.SetResource(const Value: TFhirConceptMap);
begin
  FResource.Free;
  FResource := Value;
end;

procedure TLoadedConceptMap.SetSource(const Value: TFhirValueSet);
begin
  FSource.Free;
  FSource := Value;
end;

procedure TLoadedConceptMap.SetTarget(const Value: TFhirValueSet);
begin
  FTarget.Free;
  FTarget := Value;
end;

{ TValueSetProviderFilterContext }

procedure TValueSetProviderFilterContext.Add(item: TFhirValueSetCodeSystemConcept; rating : double);
begin
  concepts.Add(TFhirValueSetCodeSystemConceptMatch.Create(item, rating));
end;

function TValueSetProviderFilterContext.Compare(const Left, Right: TFhirValueSetCodeSystemConceptMatch): Integer;
begin
  if right.FRating > left.FRating then
    result := 1
  else if left.FRating > right.FRating then
    result := -1
  else
    result := 0;
end;

constructor TValueSetProviderFilterContext.Create;
begin
  inherited Create;
  self.concepts := TAdvList<TFhirValueSetCodeSystemConceptMatch>.create;
  ndx := -1;
end;

destructor TValueSetProviderFilterContext.Destroy;
begin
  concepts.Free;
  inherited;
end;

procedure TValueSetProviderFilterContext.sort;
var
  m : TFhirValueSetCodeSystemConceptMatch;
begin
  concepts.sort(self);
  for m in concepts do
    writeln(m.FItem.code+' ('+m.FItem.display+'): '+FloatToStr(m.FRating));
end;

{ TValueSetProviderContext }

constructor TValueSetProviderContext.Create(context: TFhirValueSetCodeSystemConcept);
begin
  inherited create;
  self.context := context;
end;

destructor TValueSetProviderContext.Destroy;
begin
  context.Free;
  inherited;
end;

{ TLoadedConceptMapList }

function TLoadedConceptMapList.getMap(iIndex: integer): TLoadedConceptMap;
begin
  result := TLoadedConceptMap(ObjectByIndex[iIndex]);
end;

function TLoadedConceptMapList.itemClass: TAdvObjectClass;
begin
  result := TLoadedConceptMap;
end;

{ TFhirValueSetCodeSystemConceptMatch }

constructor TFhirValueSetCodeSystemConceptMatch.Create(item: TFhirValueSetCodeSystemConcept; rating: double);
begin
  inherited Create;
  FItem := item;
  FRating := rating;
end;

destructor TFhirValueSetCodeSystemConceptMatch.Destroy;
begin
  FItem.Free;
  inherited;
end;

end.
