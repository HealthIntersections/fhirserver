unit tx_manager;

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
  SysUtils, Classes, fsl_threads, Generics.Defaults, Generics.Collections,
  fsl_utilities, fsl_stream, fsl_base, fsl_collections, fsl_http, fsl_lang, fsl_logging, fsl_i18n,
  fdb_manager,
  fhir_objects,  fhir_common, fhir_cdshooks, fhir_factory, fhir_features, fhir_uris,
  fhir_codesystem_service, fhir_valuesets,
  ftx_service, ftx_loinc_services, ftx_ucum_services, ftx_sct_services, tx_rxnorm, tx_unii, tx_acir,
  tx_uri, tx_areacode, tx_countrycode, tx_us_states, tx_iso_4217, tx_version,
  tx_mimetypes, ftx_lang, tx_ndc, tx_hgvs,
  utilities, server_config, kernel_thread;

const
  URI_VERSION_BREAK = '#';

Type

  { TCodeSystemProviderFactory }

  TCodeSystemProviderFactory = class (TFslObject)
  public
    function link : TCodeSystemProviderFactory; overload;
    function getProvider : TCodeSystemProvider; virtual; abstract;
    function systemUri : String; virtual; abstract;
    function version : String; virtual; abstract;
    function name : String; virtual; abstract;
    function TotalCount : integer; virtual; abstract;
    function versionDesc : String; virtual; abstract;
    function description : String; virtual; abstract;
  end;

  { TCodeSystemProviderGeneralFactory }

  TCodeSystemProviderGeneralFactory = class (TCodeSystemProviderFactory)
  private
    FProvider : TCodeSystemProvider;
  public
    Constructor Create(provider : TCodeSystemProvider);
    Destructor destroy; override;

    function getProvider : TCodeSystemProvider; override;
    function systemUri : String; override;
    function version : String; override;
    function name : String; override;
    function TotalCount : integer; override;
    function versionDesc : String; override;
    function description : String; override;
  end;


  { TSnomedProviderFactory }

  TSnomedProviderFactory = class (TCodeSystemProviderFactory)
  private
    FSnomed : TSnomedServices;
  public
    Constructor Create(snomed : TSnomedServices);
    Destructor destroy; override;

    function getProvider : TCodeSystemProvider; override;
    function systemUri : String; override;
    function version : String; override;
    function name : String; override;
    function TotalCount : integer; override;
    function versionDesc : String; override;
    function description : String; override;
  end;

  { TCommonTerminologies }

  TCommonTerminologies = class (TFslObject)
  private
    FSettings : TFHIRServerSettings;
    FLanguages : TIETFLanguageDefinitions;
    FLoinc : TLOINCServices;
    FSnomed : TFslList<TSnomedServices>;
    FDefSnomed : TSnomedServices;
    FUcum : TUcumServices;
    FRxNorm : TRxNormServices;
    FUnii : TUniiServices;
    FACIR : TACIRServices;
    FProviderClasses : TFslMap<TCodeSystemProviderFactory>;
    FNDFRT: TNDFRTServices;
    FNDC : TNDCServices;
    procedure SetLoinc(const Value: TLOINCServices);
    procedure SetDefSnomed(const Value: TSnomedServices);
    procedure SetUcum(const Value: TUcumServices);
    procedure SetRxNorm(const Value: TRxNormServices);
    procedure SetNDC(const Value: TNDCServices);
    procedure SetUnii(const Value: TUniiServices);
    procedure SetACIR(const Value: TACIRServices);

    procedure getSummary(b : TStringBuilder);

    procedure SetNDFRT(const Value: TNDFRTServices);
  public
    constructor Create(settings : TFHIRServerSettings);
    destructor Destroy; Override;
    function link: TCommonTerminologies; overload;
    procedure Unload;

    function add(p : TCodeSystemProvider) : TCodeSystemProvider; overload;
    procedure add(p : TCodeSystemProvider; defVer : boolean); overload;
    procedure add(p : TCodeSystemProviderFactory; defVer : boolean); overload;
    Property ProviderClasses : TFslMap<TCodeSystemProviderFactory> read FProviderClasses;
    property Settings : TFHIRServerSettings read FSettings;
    procedure sweepSnomed(callback : TFhirServerMaintenanceThreadTaskCallBack);
    procedure clearSnomed;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); virtual;
    procedure getCacheInfo(ci: TCacheInformation); virtual;

    // load external terminology resources (snomed, Loinc, etc)
    procedure load(txlist: TFHIRServerConfigSection; testing : boolean);
    procedure listVersions(url : String; list : TStringList);

    property Languages : TIETFLanguageDefinitions read FLanguages;
    Property Loinc : TLOINCServices read FLoinc write SetLoinc;
    Property Snomed : TFslList<TSnomedServices> read FSnomed;
    Property DefSnomed : TSnomedServices read FDefSnomed write SetDefSnomed;
    Property Ucum : TUcumServices read FUcum write SetUcum;
    Property RxNorm : TRxNormServices read FRxNorm write SetRxNorm;
    Property NDC : TNDCServices read FNDC write SetNDC;
    Property NDFRT : TNDFRTServices read FNDFRT write SetNDFRT;
    Property Unii : TUniiServices read FUnii write SetUnii;
    Property ACIR : TACIRServices read FACIR write SetACIR;
  end;

  TLoadedConceptMap = class (TFslObject)
  private
    FSource: TFhirValueSetW;
    FResource: TFhirConceptMapW;
    FTarget: TFhirValueSetW;
    procedure SetResource(const Value: TFhirConceptMapW);
    procedure SetSource(const Value: TFhirValueSetW);
    procedure SetTarget(const Value: TFhirValueSetW);

    function HasTranslation(list : TFslList<TFhirConceptMapGroupW>; system, code : String; out maps : TFslList<TFhirConceptMapGroupElementTargetW>) : boolean; overload;
  public
    destructor Destroy; override;
    function Link : TLoadedConceptMap; overload;
    Property Source : TFhirValueSetW read FSource write SetSource;
    Property Resource : TFhirConceptMapW read FResource write SetResource;
    Property Target : TFhirValueSetW read FTarget write SetTarget;

    function HasTranslation(system, code : String; out maps : TFslList<TFhirConceptMapGroupElementTargetW>) : boolean; overload;
  end;

  TLoadedConceptMapList = class (TFslObjectList)
  private
    function getMap(iIndex: integer): TLoadedConceptMap;
  protected
    function ItemClass : TFslObjectClass; override;
  public
    Property map[iIndex : integer] : TLoadedConceptMap read getMap; default;

  end;

  // the terminology server maintains a cache of terminology related resources
  // the rest server notifies terminology server whenever this list changes
  // (and at start up)

  { TTerminologyServerStore }

  TTerminologyServerStore = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FStem : TFslWordStemmer;
    FCommonTerminologies : TCommonTerminologies;
    FI18n : TI18nSupport;

    FLastConceptKey : integer;
    FLastClosureKey : integer;
    FLastClosureEntryKey : integer;
    FLastValueSetKey : integer;
    FLastValueSetMemberKey : integer;

    // value sets are indexed 3 ways:
    // by their local url
    // by their canonical url
    // if they're a value set, by their code system url
    FValueSets : TFHIRMetadataResourceManagerW<TFHIRValueSetW>; // by local system's id
    FCodeSystems : TFHIRCodeSystemManager; // all current value sets that define systems, by their url
    FCodeSystemsByVsUrl : TFslMap<TFHIRCodeSystemEntry>; // all current value sets that define systems, by their url
    FSupplementsById : TFslMap<TFHIRCodeSystemW>; // All supplements

    FBaseValueSets : TFslMap<TFHIRResourceProxyV>; // value sets out of the specification - these can be overriden, but they never go away
    FBaseCodeSystems : TFslMap<TFHIRCodeSystemEntry>; // value sets out of the specification - these can be overriden, but they never go away

    FBaseConceptMaps : TFslMap<TLoadedConceptMap>; // value sets out of the specification - these can be overriden, but they never go away
    FConceptMapsById : TFslMap<TLoadedConceptMap>;
    FConceptMapsByURL : TFslMap<TLoadedConceptMap>;

    FLoading : boolean;

    procedure SetLoading(AValue: boolean);
    procedure UpdateConceptMaps;
    procedure BuildStems(cs : TFhirCodeSystemW);

    procedure checkForDuplicates(codes: TStringList; list: TFhirCodeSystemConceptListW; url : String);
    function checkVersion(system, version: String; profile: TFHIRExpansionParams): String;
    procedure AddCodeSystemToCache(cs : TFhirCodeSystemW; base : boolean);
    procedure RemoveCodeSystemFromCache(id : String);
    function getProviderClasses: TFslMap<TCodeSystemProviderFactory>;
    function defToLatestForSystem(system : String) : boolean;
  protected
    FLock : TFslLock;  // it would be possible to use a read/write lock, but the complexity doesn't seem to be justified by the short amount of time in the lock anyway
    FDB : TFDBManager;
    procedure invalidateVS(id : String); virtual;
    procedure getSummary(b : TStringBuilder);
  public
    constructor Create(db : TFDBManager; factory : TFHIRFactory; common : TCommonTerminologies; i18n : TI18nSupport); virtual;
    destructor Destroy; Override;
    Function Link : TTerminologyServerStore; overload;

    Property Factory : TFHIRFactory read FFactory;
    Property DB : TFDBManager read FDB;
    property CommonTerminologies : TCommonTerminologies read FCommonTerminologies;
    property i18n : TI18nSupport read FI18n;

    // maintenance procedures
    procedure SeeSpecificationResource(resource : TFHIRResourceProxyV);
    procedure SeeTerminologyResource(resource : TFHIRResourceProxyV);
    procedure DropTerminologyResource(aType : String; id : String);

    // access procedures. All return values are owned, and must be freed
    Function getProvider(system : String; version : String; profile : TFHIRExpansionParams; noException : boolean = false) : TCodeSystemProvider; overload;
    Function getProvider(codesystem : TFHIRCodeSystemW; profile : TFHIRExpansionParams) : TCodeSystemProvider; overload;
    function getValueSetByUrl(url, version : String; txResources : TFslMetadataResourceList = nil) : TFHIRValueSetW;
    function getValueSetById(id : String) : TFHIRValueSetW;
    function getCodeSystemById(id : String) : TFHIRCodeSystemW;
    function getCodeSystemByValueSet(vs : String) : TFHIRCodeSystemW;
    function getCodeSystem(url : String; txResources : TFslMetadataResourceList = nil) : TFHIRCodeSystemW;
    function hasCodesystemUri(url : String; txResources : TFslMetadataResourceList = nil) : Boolean;
    function getConceptMapById(id : String) : TLoadedConceptMap;
    function getConceptMapBySrcTgt(src, tgt : String) : TLoadedConceptMap;
    procedure listVersions(url : String; list : TStringList);

    // publishing access
    procedure GetCodeSystemList(list : TFslList<TFHIRCodeSystemW>); overload;
    procedure GetCodeSystemList(list : TFslMetadataResourceList); overload;
    procedure GetValueSetList(list : TFslList<TFhirValueSetW>); overload;
    procedure GetValueSetList(list : TFslMetadataResourceList); overload;
    function GetConceptMapList : TLoadedConceptMapList;
    Property ProviderClasses : TFslMap<TCodeSystemProviderFactory> read getProviderClasses;
    function ValueSetCount : integer;
    function CodeSystemCount : integer;
    function listSystems : TArray<String>;

    procedure declareCodeSystems(list : TFslList<TFhirResourceV>);
    function supportsSystem(s : String; version : String) : boolean;
    function subsumes(uri1, code1, uri2, code2 : String) : boolean; overload;
    function subsumes(cs : TFhirCodeSystemW; codeA, codeB : TFHIRCodingW) : string; overload;
    function NextClosureKey : integer;
    function NextClosureEntryKey : integer;
    function NextConceptKey : integer;
    function NextValueSetKey : integer;
    function NextValueSetMemberKey : integer;

    function cacheSize(magic : integer) : UInt64; virtual;
    procedure Unload; virtual;

    procedure clearCache; virtual;
    procedure SetCacheStatus(status : boolean); virtual;
    procedure getCacheInfo(ci: TCacheInformation); virtual;
    procedure defineFeatures(features: TFslList<TFHIRFeature>); virtual;

    property Loading : boolean read FLoading write SetLoading;
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

  TAllCodeSystemsSource = (acssLoinc, acssSnomed, acssRxNorm, acssActCode, acssUnii);

  TAllCodeSystemsProviderContext = class (TCodeSystemProviderContext)
  private
    source : TAllCodeSystemsSource;
    context : TCodeSystemProviderContext;
  end;

  TAllCodeSystemsProvider = class (TCodeSystemProvider)
  private
    FStore: TCommonTerminologies;
    FSnomed : TSnomedProvider;
    FActCode : TCodeSystemProvider;
  public
    constructor Create(store : TCommonTerminologies; actCode : TCodeSystemProvider);
    destructor Destroy; override;
    function TotalCount : integer;  override;
    function description : String; override;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function sameContext(a, b : TCodeSystemProviderContext) : boolean; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    procedure Designations(context : TCodeSystemProviderContext; list : TConceptDesignations); overload; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); overload; override;
    procedure Close(ctxt : TCodeSystemProviderContext); overload; override;
    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
  end;

{ TCodeSystemProviderFactory }

function TCodeSystemProviderFactory.link: TCodeSystemProviderFactory;
begin
  result := TCodeSystemProviderFactory(inherited link);
end;


function TAllCodeSystemsProvider.TotalCount : integer;
begin
  result := FStore.DefSnomed.TotalCount + FStore.Loinc.TotalCount + FActCode.TotalCount + FStore.Unii.TotalCount;
  if FStore.RxNorm <> nil then
    result := result + FStore.RxNorm.TotalCount;
end;

function TAllCodeSystemsProvider.getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  if (context = nil) then
    result := TCodeSystemIteratorContext.Create(nil, TotalCount)
  else
    raise ETerminologyError.create('Not Created Yet', itNotSupported);
end;

function TAllCodeSystemsProvider.getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  result := nil;
  raise ETerminologyError.create('Not Created Yet', itNotSupported);
end;

function TAllCodeSystemsProvider.systemUri(context : TCodeSystemProviderContext) : String;
var
  c : TAllCodeSystemsProviderContext;
begin
  if Context = nil then
    result := ALL_CODE_CS
  else
  begin
    c := context as TAllCodeSystemsProviderContext;
    case c.source of
      acssLoinc : result := FStore.Loinc.systemUri(c.context);
      acssSnomed : result := FStore.DefSnomed.systemUri();
      acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.systemUri(c.context) else result := '??';
      acssUnii : result := FStore.Unii.systemUri(c.context);
      acssActCode : result := FActCode.systemUri(c.context);
    end;
  end;
end;

function TAllCodeSystemsProvider.getDisplay(code : String; const lang : THTTPLanguages):String;
begin
  result := '';
  raise ETerminologyError.create('Not Created Yet', itNotSupported);
end;

function TAllCodeSystemsProvider.getPrepContext: TCodeSystemProviderFilterPreparationContext;
var
  ctxt : TAllCodeSystemsProviderFilterPreparationContext;
begin
  ctxt := TAllCodeSystemsProviderFilterPreparationContext.Create;
  try
    if FStore.RxNorm <> nil then
      ctxt.rxnorm := FStore.RxNorm.getPrepContext;
    ctxt.unii := nil;
    ctxt.loinc := FStore.Loinc.getPrepContext;
    ctxt.snomed := FSnomed.getPrepContext;
    ctxt.actcode := Factcode.getPrepContext;
    result := ctxt.link;
  finally
    ctxt.Free;
  end;
end;

function TAllCodeSystemsProvider.getDefinition(code : String):String;
begin
  result := '';
  raise ETerminologyError.create('Not Created Yet', itNotSupported);
end;

function TAllCodeSystemsProvider.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := nil;
  raise ETerminologyError.create('Not Created Yet', itNotSupported);
end;


function TAllCodeSystemsProvider.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  result := nil;
  raise ETerminologyError.create('Not Created Yet', itNotSupported);
end;

function TAllCodeSystemsProvider.IsAbstract(context : TCodeSystemProviderContext) : boolean;
var
  c : TAllCodeSystemsProviderContext;
begin
  result := true;
  c := context as TAllCodeSystemsProviderContext;
  case c.source of
    acssLoinc : result := FStore.Loinc.IsAbstract(c.context);
    acssSnomed : result := FSnomed.IsAbstract(c.context);
    acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.IsAbstract(c.context) else result := false;
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
    acssSnomed : result := FSnomed.Code(c.context);
    acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.Code(c.context) else result := '??';
    acssUnii : result := FStore.Unii.Code(c.context);
    acssActCode : result := FActCode.Code(c.context);
  end;
end;

constructor TAllCodeSystemsProvider.Create(store: TCommonTerminologies; actCode : TCodeSystemProvider);
begin
  inherited Create(store.Languages.link);
  FStore := store;
  FSnomed := TSnomedProvider.create(FStore.defSnomed.link, nil);
  FActCode := actCode;
end;

procedure TAllCodeSystemsProvider.defineFeatures(features: TFslList<TFHIRFeature>);
begin

end;

function TAllCodeSystemsProvider.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
var
  c : TAllCodeSystemsProviderContext;
begin
  c := context as TAllCodeSystemsProviderContext;
  case c.source of
    acssLoinc : result := FStore.Loinc.Display(c.context, lang)+' (LOINC: '+FStore.Loinc.Code(c.context)+')';
    acssSnomed : result := FSnomed.Display(c.context, lang)+' (S-CT: '+FSnomed.Code(c.context)+')';
    acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.Display(c.context, lang)+' (RxN: '+FStore.RxNorm.Code(c.context)+')' else result := '';
    acssUnii : result := FStore.Unii.Display(c.context, lang)+' (Unii: '+FStore.Unii.Code(c.context)+')';
    acssActCode : result := FActCode.Display(c.context, lang)+' (ActCode: '+FActCode.Code(c.context)+')';
  end;
end;

function TAllCodeSystemsProvider.Definition(context : TCodeSystemProviderContext) : string;
var
  c : TAllCodeSystemsProviderContext;
begin
  c := context as TAllCodeSystemsProviderContext;
  case c.source of
    acssLoinc : result := FStore.Loinc.Definition(c.context);
    acssSnomed : result := FSnomed.Definition(c.context);
    acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.Definition(c.context) else result := '??';
    acssUnii : result := FStore.Unii.Definition(c.context);
    acssActCode : result := FActCode.Definition(c.context);
  end;
end;
function TAllCodeSystemsProvider.description: String;
begin
  result := 'All Code Systems combined';
end;

destructor TAllCodeSystemsProvider.Destroy;
begin
  FActCode.Free;
  FSnomed.Free;
  FStore.Free;
  inherited;
end;

procedure TAllCodeSystemsProvider.Designations(context : TCodeSystemProviderContext; list : TConceptDesignations);
begin
  raise ETerminologyError.create('Not Created Yet', itNotSupported);
end;

function TAllCodeSystemsProvider.sameContext(a, b: TCodeSystemProviderContext): boolean;
begin
  result := false;
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
      ctxt.unii := nil; // FStore.Unii.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).unii, sort);
      ctxt.snomed := FSnomed.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).snomed, sort);
      ctxt.loinc := FStore.loinc.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).loinc, sort);
      ctxt.actcode := FActCode.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).actcode, sort);
      result := ctxt.Link;
    finally
      ctxt.free;
    end;
  end;
end;

function TAllCodeSystemsProvider.filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  result := nil;
  raise ETerminologyError.create('Not Created Yet', itNotSupported);
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
    FSnomed.prepare(ctxt.snomed);
    if FStore.RxNorm <> nil then
      FStore.RxNorm.prepare(ctxt.rxnorm);
//    FStore.FUnii.prepare(ctxt.unii);
    FActCode.prepare(ctxt.actcode);
  end;
end;

function TAllCodeSystemsProvider.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := nil;
  raise ETerminologyError.create('Not Created Yet', itNotSupported);
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
      c.snomedDone := not FSnomed.FilterMore(c.snomed);
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
      c.context := FSnomed.FilterConcept(d.snomed);
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
  result := false;
  raise ETerminologyError.create('Not Created Yet', itNotSupported);
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
  FStore.unii.Close(c.unii);
  FStore.Loinc.Close(c.loinc);
  FSnomed.Close(c.snomed);
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
    FStore.Unii.Close(c.unii);
    FStore.Loinc.Close(c.loinc);
    FSnomed.Close(c.snomed);
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
    acssSnomed : FSnomed.Close(c.context);
    acssRxNorm : if FStore.RxNorm <> nil then FStore.RxNorm.Close(c.context);
    acssUnii : FStore.Unii.Close(c.context);
    acssActCode : FActCode.Close(c.context);
  end;
  ctxt.free;
end;


{ TTerminologyServerStore }

procedure TTerminologyServerStore.BuildStems(cs: TFhirCodeSystemW);
  function stems(c : TFhirCodeSystemConceptW) : TConceptAdornment;
  var
    s, t : String;
  begin
    result := TConceptAdornment.Create;
    c.Tag := result;
    t := c.display;
    while (t <> '') Do
    begin
      StringSplit(t, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='], s, t);
      if (s <> '') Then
        result.Add(lowercase(FStem.Stem(s)));
    end;
    result.SortAscending;
  end;
  procedure processConcepts(parent : TFhirCodeSystemConceptW; list : TFhirCodeSystemConceptListW; map : TFhirCodeSystemConceptMapW);
  var
    c : TFhirCodeSystemConceptW;
  begin
    for c in list do
    begin
      stems(c).parent := parent;
      if map.ContainsKey(c.code) then
        Logging.log('Duplicate code '+c.code+' in '+cs.url)
      else
        map.Add(c.code, c.Link);
      processConcepts(c, c.conceptList, map);
    end;
  end;
var
  map : TFhirCodeSystemConceptMapW;
begin
  map := TFhirCodeSystemConceptMapW.create('stems');
  try
    cs.Tag := TCodeSystemAdornment.create(map.link);
    processConcepts(nil, cs.conceptList, map);
  finally
    map.Free;
  end;
end;


function TTerminologyServerStore.cacheSize(magic : integer): UInt64;
begin
  result := 0;
end;

procedure TTerminologyServerStore.Unload;
begin
  clearCache;
  FCommonTerminologies.Unload;
  FValueSets.clear;
  FCodeSystems.clear;
  FCodeSystemsByVsUrl.clear;
  FSupplementsById.clear;
  FBaseValueSets.clear;
  FBaseCodeSystems.clear;
  FBaseConceptMaps.clear;
  FConceptMapsById.clear;
  FConceptMapsByURL.clear;
end;

procedure TTerminologyServerStore.checkForDuplicates(codes : TStringList; list : TFhirCodeSystemConceptListW; url : String);
var
  cc : TFhirCodeSystemConceptW;
  i : integer;
begin
  for cc in list do
  begin
    if cc.code <> '' then
    begin
      if codes.Find(cc.code, i) then
        raise ETerminologyError.create('Duplicate code: '+cc.code+' in value set '+url, itInvalid);
      codes.Add(cc.code);
    end;
    checkForDuplicates(codes, cc.conceptList, url);
  end;
end;

function exempt(vs : TFhirCodeSystemW) : boolean;
begin
  if (vs.fhirObjectVersion = fhirVersionRelease2) and vs.URL.startsWith('http://hl7.org/fhir/ValueSet/v2-')
    and StringArrayExists(['0003', '0207', '0277', '0278', '0279', '0281', '0294', '0396'],
        vs.URL.Substring(vs.url.Length-4)) then
      result := true
  else
    result := false;
end;

function TTerminologyServerStore.CodeSystemCount: integer;
begin
  FLock.Lock('CodeSystemCount');
  try
    result := FCodeSystems.Count;
  finally
    FLock.Unlock;
  end;
end;

constructor TTerminologyServerStore.Create(db : TFDBManager; factory : TFHIRFactory; common : TCommonTerminologies; i18n : TI18nSupport);
var
  conn : TFDBConnection;
begin
  inherited Create;
  FFactory := factory;
  FLock := TFslLock.Create('Terminology Server Store');
  FCommonTerminologies := common;
  FI18n := i18n;

  FDB := db;

  FValueSets := TFHIRMetadataResourceManagerW<TFhirValueSetW>.create;
  FCodeSystems := TFHIRCodeSystemManager.create;
  FCodeSystemsByVsUrl := TFslMap<TFhirCodeSystemEntry>.create('tx.cs.url');
  FBaseValueSets := TFslMap<TFhirResourceProxyV>.create('tx.vs.base');
  FBaseCodeSystems := TFslMap<TFHIRCodeSystemEntry>.create('tx.cs.base');
  FSupplementsById := TFslMap<TFhirCodeSystemW>.create('tx.cs.suppl');

  FBaseConceptMaps := TFslMap<TLoadedConceptMap>.create('tx.cm.base');
  FConceptMapsById := TFslMap<TLoadedConceptMap>.create('tx.cm.id');
  FConceptMapsByURL := TFslMap<TLoadedConceptMap>.create('tx.cm.url');

  FBaseValueSets.defaultValue := nil;
  FBaseCodeSystems.defaultValue := nil;
  FBaseConceptMaps.defaultValue := nil;

  FStem := TFslWordStemmer.create('english');

  if (Fdb <> nil) then
  begin
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
end;

procedure TTerminologyServerStore.declareCodeSystems(list : TFslList<TFhirResourceV>);
  procedure addCodesystemUri(name, id, uri, version : String; count : integer);
  var
    cs : TFhirCodeSystemW;
    r : TFHIRResourceV;
  begin
    r := FFactory.makeResource('CodeSystem');
    try
      cs := FFactory.wrapCodesystem(r.link);
      try
        cs.url := uri;
        cs.version := version;
        cs.name := name;
        cs.id := id;
        cs.status := psActive;
        cs.content := cscmNotPresent;
        if count <> 0 then
          cs.count := count;
      finally
        cs.Free;
      end;
      list.Add(r.link);
    finally
      r.Free;
    end;
  end;
  function tail(url : String) : String;
  begin
    result := url.Substring(url.LastIndexOf('/')+1);
  end;
var
  sn : TSnomedServices;
  sp : TSnomedProvider;
begin
  if FCommonTerminologies.FLoinc <> nil then
    addCodesystemUri('LOINC', 'loinc', FCommonTerminologies.FLoinc.systemUri(nil), FCommonTerminologies.FLoinc.version(nil), FCommonTerminologies.FLoinc.TotalCount);
  for sn in FCommonTerminologies.FSnomed do
  begin
    sp := TSnomedProvider.create(sn.link, nil);
    try
      addCodesystemUri('SNOMED CT', 'sct', sp.systemUri(nil), sp.version(nil), sp.TotalCount);
    finally
      sp.free;
    end;
  end;
  if FCommonTerminologies.FUcum <> nil then
    addCodesystemUri('Ucum', 'ucum', FCommonTerminologies.FUcum.systemUri(nil), FCommonTerminologies.FUcum.version(nil), FCommonTerminologies.FUcum.TotalCount);
  if FCommonTerminologies.FRxNorm <> nil then
    addCodesystemUri('RxNorm', 'rxnorm', FCommonTerminologies.FRxNorm.systemUri(nil), FCommonTerminologies.FRxNorm.version(nil), FCommonTerminologies.FRxNorm.TotalCount);
  if FCommonTerminologies.NDFRT <> nil then
    addCodesystemUri('NDFRT', 'ndfrt', FCommonTerminologies.NDFRT.systemUri(nil), FCommonTerminologies.NDFRT.version(nil), FCommonTerminologies.NDFRT.TotalCount);
  if FCommonTerminologies.FUnii <> nil then
    addCodesystemUri('Unii', 'unii', FCommonTerminologies.FUnii.systemUri(nil), FCommonTerminologies.FUnii.version(nil), FCommonTerminologies.FUnii.TotalCount);
  if FCommonTerminologies.FACIR <> nil then
    addCodesystemUri('ACIR', 'acir', FCommonTerminologies.FACIR.systemUri(nil), FCommonTerminologies.FACIR.version(nil), FCommonTerminologies.FACIR.TotalCount);
end;


procedure TTerminologyServerStore.defineFeatures(features: TFslList<TFHIRFeature>);
var
  cs : TFhirCodeSystemW;
  list : TFslList<TFHIRCodeSystemW>;
begin
  FCommonTerminologies.defineFeatures(features);
  list := TFslList<TFHIRCodeSystemW>.create;
  try
    FCodeSystems.listAll(list);
    for cs in list do
    begin

    end;
  finally
    list.Free;
  end;


end;

destructor TTerminologyServerStore.Destroy;
begin
  FCommonTerminologies.Free;
  FI18n.free;
  FStem.Free;
  FValueSets.Free;
  FCodeSystems.Free;
  FCodeSystemsByVsUrl.Free;
  FBaseValueSets.Free;
  FBaseCodeSystems.Free;
  FSupplementsByid.Free;


  FBaseConceptMaps.Free;
  FConceptMapsById.Free;
  FConceptMapsByURL.Free;

  FLock.Free;
  FDB.free;
  FFactory.Free;
  inherited;
end;

function TTerminologyServerStore.subsumes(cs: TFhirCodeSystemW; codeA, codeB: TFHIRCodingW): string;
var
  prov : TCodeSystemProvider;
begin
  // later, see if we can translate instead
  if (cs.url <> codeA.systemUri) then
    raise ETerminologyError.create('System uri / code uri mismatch - not supported at this time ('+cs.url+'/'+codeA.systemUri+')', itNotSupported);
  if (cs.url <> codeB.systemUri) then
    raise ETerminologyError.create('System uri / code uri mismatch - not supported at this time ('+cs.url+'/'+codeB.systemUri+')', itNotSupported);
  if (codeA.code = codeB.code) then
    exit('equivalent');

  prov := getProvider(cs, nil);
  try
    result := prov.subsumesTest(codeA.code, codeB.code);
  finally
    prov.Free;
  end;
end;

function TTerminologyServerStore.supportsSystem(s: String; version : String): boolean;
var
  p : TCodeSystemProvider;
begin
  p := getProvider(s, version, nil, true);
  try
    result := p <> nil;
  finally
    p.Free;
  end;
end;


// ----  maintenance procedures ------------------------------------------------

function urlTail(path : String) : String;
begin
  result := path.substring(path.lastIndexOf('/')+1);
end;


procedure TTerminologyServerStore.AddCodeSystemToCache(cs : TFhirCodeSystemW; base : boolean);
var
  cse, ct : TFHIRCodeSystemEntry;
  supp : TFhirCodeSystemW;
begin
  cse := TFHIRCodeSystemEntry.Create(cs.Link);
  try
    if base then
      FBaseCodeSystems.AddOrSetValue(cs.url, cse.Link);
    if (cs.supplements <> '') then
    begin
      FSupplementsById.AddOrSetValue(cs.id, cs.Link);
      if cs.supplements.StartsWith('CodeSystem/') then
      begin
        if FCodeSystems.has(cs.supplements.Substring(11), ct) then
          ct.Supplements.Add(cs.Link);
      end
      else if FCodeSystems.has(cs.supplements, ct) then
        ct.Supplements.Add(cs.Link);
    end
    else
    begin
      FCodeSystems.see(cse);
      if cs.valueSet <> '' then
        FCodeSystemsByVsUrl.AddOrSetValue(cs.valueSet, cse.Link);
      if (FDB <> nil) then // don't build stems in this case
        BuildStems(cs); // todo: move this out of the lock
      for supp in FSupplementsById.values do
        if (supp.supplements = cs.url) or (supp.supplements = 'CodeSystem/'+cs.id) then
          cse.Supplements.Add(cs.Link);
    end;
  finally
    cse.Free;
  end;
end;

procedure TTerminologyServerStore.RemoveCodeSystemFromCache(id : String);
var
  cse, cs1 : TFHIRCodeSystemEntry;
  cs : TFhirCodeSystemW;
begin
  if (FCodeSystems.has(id, cse)) then
  begin
    cs1 := FBaseCodeSystems[cse.CodeSystem.url].Link;
    try
      if cs1 <> nil then
        AddCodeSystemToCache(cs1.codeSystem, false);
    finally
      cs1.free;
    end;
    FCodeSystems.drop(id);
  end
  else if FSupplementsById.TryGetValue(id, cs) then
  begin
    cs1 := FBaseCodeSystems[cs.url].Link;
    try
      if cs.supplements.StartsWith('CodeSystem/') then
      begin
        if FCodeSystems.has(cs.supplements.Substring(11), cse) then
          cse.Supplements.Remove(cs);
      end
      else if FCodeSystems.has(cs.supplements, cse) then
        cse.Supplements.remove(cs);
      if cs1 <> nil then
        AddCodeSystemToCache(cs1.codeSystem, false);
    finally
      cs1.Free;
    end;
  end;
end;

procedure TTerminologyServerStore.SeeSpecificationResource(resource : TFHIRResourceProxyV);
var
  vs : TFhirValueSetW;
  cs : TFhirCodeSystemW;
  cm : TLoadedConceptMap;
begin
  FLock.Lock('SeeSpecificationResource');
  try
    if (resource.fhirType = 'ValueSet') then
    begin
      if (resource.url = 'http://hl7.org/fhir/ValueSet/ucum-common') and (FCommonTerminologies.FUcum <> nil) then
        FCommonTerminologies.FUcum.SetCommonUnits(resource.resourceW.Link as TFHIRValueSetW);

      FBaseValueSets.AddOrSetValue(resource.url, resource.Link);
      FValueSets.see(resource);
      if (resource.fhirObjectVersion = fhirVersionRelease2) then
      begin
        vs := resource.resourceW as TFHIRValueSetW;
        if vs.hasInlineCS then
        begin
          cs := FFactory.wrapCodesystem(vs.Resource.link);
          try
            AddCodeSystemToCache(cs, true);
          finally
            cs.free;
          end;
        end;
      end;
      if not loading then
        UpdateConceptMaps;
    end
    else if (resource.fhirType = 'CodeSystem') then
    begin
      cs := resource.resourceW as TFHIRCodeSystemW;
      AddCodeSystemToCache(cs, true);
    end
    else if (resource.fhirType = 'ConceptMap') then
    begin
      cm := TLoadedConceptMap.Create;
      try
        cm.Resource := resource.resourceW.Link as TFHIRConceptMapW;
        cm.Source := getValueSetByUrl(cm.Resource.source, '');
        cm.Target := getValueSetByUrl(cm.Resource.target, '');
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

procedure TTerminologyServerStore.SeeTerminologyResource(resource : TFHIRResourceProxyV);
var
  vs : TFhirValueSetW;
  cs : TFhirCodeSystemW;
  cm : TLoadedConceptMap;
begin
  FLock.Lock('SeeTerminologyResource');
  try
    if (resource.fhirType = 'ValueSet') then
    begin
      if (resource.url = 'http://hl7.org/fhir/ValueSet/ucum-common') and (FCommonTerminologies.FUcum <> nil) then
        FCommonTerminologies.FUcum.SetCommonUnits(resource.resourceW.link as TFHIRValueSetW);

      FValueSets.see(resource);
      invalidateVS(resource.url);
      if (resource.fhirObjectVersion = fhirVersionRelease2) then
      begin
        vs := resource.resourceW as TFHIRValueSetW;
        if vs.hasInlineCS then
        begin
          cs := FFactory.wrapCodesystem(vs.resource.Link);
          try
            AddCodeSystemToCache(cs, true);
          finally
            cs.free;
          end;
        end;
        UpdateConceptMaps;
      end;
    end
    else if (resource.fhirType = 'CodeSystem') then
    begin
      cs := resource.resourceW as TFHIRCodeSystemW;
      AddCodeSystemToCache(cs, false);
    end
    else if (resource.fhirType = 'ConceptMap') then
    begin
      cm := TLoadedConceptMap.Create;
      try
        cm.Resource := resource.resourceW.Link as TFHIRConceptMapW;
        cm.Source := getValueSetByUrl(cm.Resource.source, '');
        cm.Target := getValueSetByUrl(cm.Resource.target, '');
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

procedure TTerminologyServerStore.SetCacheStatus(status: boolean);
begin
  // nothing right now.
end;

procedure TTerminologyServerStore.DropTerminologyResource(aType : String; id : String);
var
  vs, vs1 : TFhirResourceProxyV;
  cm, cm1 : TLoadedConceptMap;
  cs : TFhirCodeSystemW;
  vsW : TFHIRValueSetW;
begin
  FLock.Lock('DropTerminologyResource');
  try
    if (aType = 'ValueSet') then
    begin
      vs := FValueSets.getP(id);
      if vs <> nil then
      begin
        vs1 := FBaseValueSets[vs.url];

        if (vs.fhirObjectVersion = fhirVersionRelease2) then
        begin
          //vs := resource.resourceW as TFHIRValueSetW;
          //if (vs.hasInlineCS) then
          //  removeCodeSystemFromCache(vs.id);
        end;
        FValueSets.drop(id);

        // add the base one back if we are dropping a value set that overrides it
        // current logical flaw: what if there's another one that overrides this? how do we prevent or deal with this?
        if vs1 <> nil then
        begin
          FValueSets.see(vs1.Link);
          if (vs1.fhirObjectVersion = fhirVersionRelease2) then
          begin
            //vs := vs1.resourceW as TFHIRValueSetW;
            //if (vs.hasInlineCS) then
            //begin
            //  cs := vs1.ResourceW.link as TFHIRValueSet;
            //  try
            //    AddCodeSystemToCache(cs, false);
            //  finally
            //    cs.Free;
            //  end;
            //end;
          end;
        end;
        UpdateConceptMaps;
      end;
    end
    else if (aType = 'CodeSystem') then
      removeCodeSystemFromCache(id)
    else if (aType = 'ConceptMap') then
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
    if cm.Resource.source = '' then
      cm.source := nil
    else
    begin
      cm.Source := getValueSetByUrl(cm.Resource.source, '');
      if (cm.Source = nil) then
        cm.Source := getValueSetById(cm.Resource.source);
    end;
    if cm.Resource.target = '' then
      cm.Target := nil
    else
    begin
      cm.Target := getValueSetByUrl(cm.Resource.target, '');
      if (cm.Target = nil) then
        cm.Target := getValueSetById(cm.Resource.target);
    end;
  end;
end;

procedure TTerminologyServerStore.SetLoading(AValue: boolean);
begin
  if FLoading then
  begin
    FLock.Lock;
    try
      UpdateConceptMaps;
    finally
      FLock.Unlock;
    end;
  end;
  FLoading := AValue;
end;

function TTerminologyServerStore.ValueSetCount: integer;
begin
  FLock.Lock('ValueSetCount');
  try
    result := FValueSets.Count;
  finally
    FLock.Unlock;
  end;
end;

//---- access procedures. All return values are owned, and must be freed -------

procedure TTerminologyServerStore.getCacheInfo(ci: TCacheInformation);
begin
  FCommonTerminologies.getCacheInfo(ci);
end;

function TTerminologyServerStore.getCodeSystem(url: String;
  txResources: TFslMetadataResourceList): TFHIRCodeSystemW;
var
  r : TFHIRMetadataResourceW;
begin
 if txResources <> nil then
  begin
    for r in txResources do
      if (url <> '') and ((r.url = url) or (r.vurl = url)) then
      begin
        if not (r is TFhirCodeSystemW) then
          raise EFHIRException.Create('Attempt to reference '+url+' as a CodeSystem when it''s a '+r.fhirType);
        exit(r.link as TFhirCodeSystemW);
      end;
  end;

  FLock.Lock('getValueSetByUrl');
  try
    if FCodeSystems.has(url) then
      result := FCodeSystems.get(url).CodeSystem.Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.getCodeSystemById(id: String
  ): TFHIRCodeSystemW;
begin
  FLock.Lock('getValueSetByUrl');
  try
    if FCodeSystems.has(id) then
      result := FCodeSystems.get(id).CodeSystem.Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.getCodeSystemByValueSet(vs: String
  ): TFHIRCodeSystemW;
var
  cse : TFHIRCodeSystemEntry;
begin
  FLock.Lock('getValueSetByUrl');
  try
    if FCodeSystemsByVsUrl.TryGetValue(vs, cse) then
      result := cse.CodeSystem.Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.GetCodeSystemList(list: TFslMetadataResourceList);
begin
  FLock.Lock('GetCodeSystemList');
  try
    FCodeSystems.listAllM(list);
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.GetCodeSystemList(list : TFslList<TFHIRCodeSystemW>);
begin
  FLock.Lock('GetCodeSystemList');
  try
    FCodeSystems.listAll(list);
  finally
    FLock.Unlock;
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

function TTerminologyServerStore.getConceptMapBySrcTgt(src, tgt: String): TLoadedConceptMap;
var
  lcm : TLoadedConceptMap;
begin
  result := nil;
  FLock.Lock('getValueSetByUrl');
  try
    for lcm in FConceptMapsById.Values do
      if (lcm.Resource.source = src) and
         (lcm.Resource.target = src) then
      begin
        result := lcm.Link;
        break;
      end
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

procedure TTerminologyServerStore.GetValueSetList(list : TFslList<TFhirValueSetW>);
begin
  FLock.Lock('GetValueSetList');
  try
    FValueSets.listAll(list);
  finally
    FLock.Unlock;
  end;
end;


function TTerminologyServerStore.checkVersion(system, version : String; profile : TFHIRExpansionParams) : String;
var
  t : TFhirExpansionParamsVersionRule;
begin
  if (profile = nil) then
    exit(version);

  for t in profile.versionRules do
    if (t.system = system) and (t.version <> '') then
    begin
      if (version = '') or (t.mode = fvmOverride) then
        version := t.version
      else if (version <> t.version) and (t.mode = fvmCheck) then
        raise ETerminologyError.Create('Expansion Parameters Error: the version "'+version+'" is inconsistent with the version "'+t.version+'" required by the profile', itInvalid);
    end;
  exit(version);
end;

procedure TTerminologyServerStore.clearCache;
begin
  FCommonTerminologies.clearSnomed;
end;

function TTerminologyServerStore.defToLatestForSystem(system : String) : boolean;
begin
  result := system <> URI_SNOMED;
end;

function TTerminologyServerStore.getProvider(system: String; version: String;
  profile: TFHIRExpansionParams; noException: boolean): TCodeSystemProvider;
var
  defToLatest : boolean;
  cs : TFHIRCodeSystemEntry;
begin
  result := nil;
  version := checkVersion(system, version, profile);
  if (profile = nil) then
    defToLatest := false
  else
    defToLatest := profile.defaultToLatestVersion;

  if ProviderClasses.ContainsKey(system) then
  begin
    if (version <> '') then
    begin
      if ProviderClasses.ContainsKey(system+URI_VERSION_BREAK+version) then
        result := ProviderClasses[system+URI_VERSION_BREAK+version].getProvider.Link
      else if defToLatest or defToLatestForSystem(system) then
      begin
        // special support for SNOMED Editions
        if (system = URI_SNOMED) and version.contains('/version/') and ProviderClasses.ContainsKey(system+URI_VERSION_BREAK+version.Substring(0, version.IndexOf('/version/'))) then
          result := ProviderClasses[system+URI_VERSION_BREAK+version.Substring(0, version.IndexOf('/version/'))].getProvider.Link
        else
          result := ProviderClasses[system].getProvider.Link;
        if (result = nil) or not (result.defToThisVersion(version)) then
        begin
          result.Free;
          result := nil;
          if not noException then
            raise ETerminologySetup.create('Unable to provide support for code system '+system+' version '+version);
        end;
      end;
    end
    else
      result := ProviderClasses[system].getProvider.Link;
  end
  else if system = ALL_CODE_CS then
    if FFactory.version in [fhirVersionRelease2, fhirVersionRelease3] then
      result := TAllCodeSystemsProvider.create(FCommonTerminologies.link, getProvider('http://hl7.org/fhir/v3/ActCode', '', nil, true))
    else
      result := TAllCodeSystemsProvider.create(FCommonTerminologies.link, getProvider('http://terminology.hl7.org/CodeSystem/v3-ActCode', '', nil, true))
  else
  begin
    FLock.Lock('getProvider');
    try
      if FCodeSystems.has(system)  then
      begin
        cs := FCodeSystems.get(system, version).link;
        if (cs = nil) and ((version = '') or defToLatest) then
          cs := FCodeSystems.get(system).link;
        try
          if cs <> nil then
            result := TFhirCodeSystemProvider.create(FCommonTerminologies.FLanguages.link, ffactory.link, cs.link);
        finally
          cs.Free;
        end;
      end;
    finally
      FLock.Unlock;
    end;
  end;

  if (result <> nil) then
  begin
    FLock.Lock('getProvider');
    try
      result.RecordUse;
      if (result is TSnomedProvider) then
        TSnomedProvider(result).Services.checkLoaded;
    finally
      FLock.Unlock;
    end;
  end;

  if (result = nil) and not noException then
    if version <> '' then
      raise ETerminologySetup.create('Unable to provide support for code system '+system+' v'+version)
    else
      raise ETerminologySetup.create('Unable to provide support for code system '+system);
end;


function TTerminologyServerStore.getProvider(codesystem: TFHIRCodeSystemW;
  profile: TFHIRExpansionParams): TCodeSystemProvider;
begin
  checkVersion(codeSystem.url, codeSystem.version, profile);
  result := TFhirCodeSystemProvider.create(FCommonTerminologies.FLanguages.link, FFactory.link, TFHIRCodeSystemEntry.Create(codesystem.link));
end;

function TTerminologyServerStore.getProviderClasses: TFslMap<TCodeSystemProviderFactory>;
begin
  result := FCommonTerminologies.ProviderClasses;
end;

procedure TTerminologyServerStore.getSummary(b: TStringBuilder);
begin
  FCommonTerminologies.getSummary(b);

  b.append('<li>ValueSets : '+inttostr(FValueSets.Count)+'</li>');
  b.append('<li>Code Systems : '+inttostr(FCodeSystems.Count)+'</li>');
  b.append('<li>Concept Maps : '+inttostr(FBaseConceptMaps.Count)+'</li>');
end;

function TTerminologyServerStore.getValueSetById(id: String): TFHIRValueSetW;
begin
  FLock.Lock('getValueSetByUrl');
  try
    if FValueSets.has(id) then
      result := FValueSets.get(id).Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.getValueSetByUrl(url, version : String; txResources : TFslMetadataResourceList = nil) : TFHIRValueSetW;
var
  p :  TArray<String>;
  r : TFHIRMetadataResourceW;
begin
 if txResources <> nil then
  begin
    for r in txResources do
      if (url <> '') and ((r.url = url) or (r.vurl = url)) and ((version = '') or (r.version = version)) then
      begin
        if not (r is TFHIRValueSetW) then
          raise EFHIRException.Create('Attempt to reference '+url+' as a ValueSet when it''s a '+r.fhirType);
        exit(r.link as TFHIRValueSetW);
      end;
  end;

  FLock.Lock('getValueSetByUrl');
  try
    if url.StartsWith('ValueSet/') then
    begin
      if FValueSets.has(url.Substring(9), result) then
        result.Link
      else
        result := nil;
    end
    else if (url.CountChar('|') = 1) then
    begin
      p := url.split(['|']);
      if FValueSets.has(p[0], p[1], result) then
        result.Link
      else
        result := nil;
    end
    else
    begin
      if (version <> '') and FValueSets.has(url, version, result) then
        result.Link
      else if FValueSets.has(url, result) then
        result.Link
      else
        result := nil;
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.GetValueSetList(list: TFslMetadataResourceList);
begin
  FLock.Lock('GetValueSetList');
  try
    FValueSets.listAllM(list);
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.hasCodesystemUri(url: String; txResources : TFslMetadataResourceList = nil): Boolean;
var
  r : TFHIRMetadataResourceW;
begin
 if txResources <> nil then
  begin
    for r in txResources do
      if (url <> '') and ((r.url = url) or (r.vurl = url)) then
      begin
        if not (r is TFhirCodeSystemW) then
          raise EFHIRException.Create('Attempt to reference '+url+' as a CodeSystem when it''s a '+r.fhirType);
        exit(true);
      end;
  end;

  FLock.Lock('getValueSetByUrl');
  try
    result := FCodeSystems.has(url);
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

function TTerminologyServerStore.listSystems: TArray<String>;
var
  ts : TStringList;
  p : TCodeSystemProviderFactory;
  i : integer;
  cs : TFHIRCodeSystemEntry;
begin
  ts := TStringList.Create;
  try
    ts.Sorted := true;
    ts.Duplicates := TDuplicates.dupIgnore;
    FLock.Lock('listSystems');
    try
      for p in ProviderClasses.Values do
        ts.Add(p.systemUri);
      for cs in FCodeSystems.list do
        ts.Add(cs.url);
    finally
      FLock.Unlock;
    end;
    SetLength(result, ts.Count);
    for i := 0 to ts.Count - 1 do
      result[i] := ts[i];
  finally
    ts.Free;
  end;
end;

procedure TTerminologyServerStore.listVersions(url: String; list: TStringList);
var
  cs : TFHIRCodeSystemEntry;
  v : String;
begin
  FCommonTerminologies.listVersions(url, list);
  FLock.Lock;
  try
    for cs in FCodeSystems.list do
    begin
      if (cs.url = url) then
      begin
        v := cs.version;
        if (v <> '') and (list.IndexOf(v) = -1) then
          list.Add(v);
      end;
    end;
  finally
    FLock.Unlock;
  end;

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
  else if (FCommonTerminologies.DefSnomed <> nil) and (uri1 = FCommonTerminologies.DefSnomed.systemUri()) then
    result := FCommonTerminologies.DefSnomed.Subsumes(code1, code2)
  else
  begin
    prov := getProvider(uri1, '', nil, true);
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

{ TLoadedConceptMap }

destructor TLoadedConceptMap.Destroy;
begin
  FResource.Free;
  FSource.Free;
  FTarget.Free;
  inherited;
end;

function TLoadedConceptMap.HasTranslation(system, code : String; out maps : TFslList<TFhirConceptMapGroupElementTargetW>) : boolean;
var
  gl : TFslList<TFhirConceptMapGroupW>;
begin
  gl := Resource.groups;
  try
    result := HasTranslation(gl, system, code, maps);
  finally
    gl.Free;
  end;
end;

function TLoadedConceptMap.Link: TLoadedConceptMap;
begin
  result := TLoadedConceptMap(inherited Link);
end;

function TLoadedConceptMap.HasTranslation(list : TFslList<TFhirConceptMapGroupW>; system, code : String; out maps : TFslList<TFhirConceptMapGroupElementTargetW>) : boolean;
var
  g : TFhirConceptMapGroupW;
  c : TFhirConceptMapGroupElementW;
begin
  result := false;
  for g in list do
    for c in g.elements.forEnum do
    begin
      if (g.source = system) and (c.code = code) then
      begin
        maps := c.targets;
        result := true;
        exit;
      end;
    end;
end;

procedure TLoadedConceptMap.SetResource(const Value: TFhirConceptMapW);
begin
  FResource.Free;
  FResource := Value;
end;

procedure TLoadedConceptMap.SetSource(const Value: TFhirValueSetW);
begin
  FSource.Free;
  FSource := Value;
end;

procedure TLoadedConceptMap.SetTarget(const Value: TFhirValueSetW);
begin
  FTarget.Free;
  FTarget := Value;
end;

{ TLoadedConceptMapList }

function TLoadedConceptMapList.getMap(iIndex: integer): TLoadedConceptMap;
begin
  result := TLoadedConceptMap(ObjectByIndex[iIndex]);
end;

function TLoadedConceptMapList.itemClass: TFslObjectClass;
begin
  result := TLoadedConceptMap;
end;


{ TCommonTerminologies }

function TCommonTerminologies.add(p: TCodeSystemProvider) : TCodeSystemProvider;
begin
  if p.version(nil) <> '' then
    FProviderClasses.Add(p.systemUri(nil)+URI_VERSION_BREAK+p.version(nil), TCodeSystemProviderGeneralFactory.create(p.link));
  if not FProviderClasses.ContainsKey(p.systemUri(nil)) then
    FProviderClasses.Add(p.systemUri(nil), TCodeSystemProviderGeneralFactory.create(p.link));
  result := p;
end;

procedure TCommonTerminologies.add(p: TCodeSystemProvider; defVer: boolean);
begin
  if p.version(nil) <> '' then
    FProviderClasses.Add(p.systemUri(nil)+URI_VERSION_BREAK+p.version(nil), TCodeSystemProviderGeneralFactory.create(p.link));
  if defVer and not FProviderClasses.ContainsKey(p.systemUri(nil)) then
    FProviderClasses.Add(p.systemUri(nil), TCodeSystemProviderGeneralFactory.create(p.link));
end;

procedure TCommonTerminologies.add(p: TCodeSystemProviderFactory; defVer: boolean);
begin
  if p.version <> '' then
    FProviderClasses.Add(p.systemUri+URI_VERSION_BREAK+p.version, p.link);
  if defVer and not FProviderClasses.ContainsKey(p.systemUri) then
    FProviderClasses.Add(p.systemUri, p.link);
end;

procedure TCommonTerminologies.clearSnomed;
var
  ss : TSnomedServices;
begin
  for ss in FSnomed do
    if ss <> FDefSnomed then
      ss.unloadMe;
end;

constructor TCommonTerminologies.Create(settings : TFHIRServerSettings);
begin
  inherited Create;
  FSettings := settings;
  FSnomed := TFslList<TSnomedServices>.create;
end;

procedure TCommonTerminologies.defineFeatures(features: TFslList<TFHIRFeature>);
var
  sp : TSnomedProvider;
begin
  if FLoinc <> nil then
    FLoinc.defineFeatures(features);
  if FDefSnomed <> nil then
  begin
    sp := TSnomedProvider.create(FDefSnomed.link, nil);
    try
      sp.defineFeatures(features);
    finally
      sp.free;
    end;
  end;
  if FUcum <> nil then
    FUcum.defineFeatures(features);
  if FRxNorm <> nil then
    FRxNorm.defineFeatures(features);
  if FUnii <> nil then
    FUnii.defineFeatures(features);
  if FACIR <> nil then
    FACIR.defineFeatures(features);
  if FNDFRT <> nil then
    FNDFRT.defineFeatures(features);
  if FNDC <> nil then
    FNDC.defineFeatures(features);
end;

destructor TCommonTerminologies.Destroy;
begin
  FProviderClasses.Free;
  FNDFRT.Free;
  FNDC.Free;
  FSettings.Free;
  FLoinc.free;
  FDefSnomed.Free;
  FSnomed.free;
  FUnii.Free;
  FACIR.Free;
  FUcum.free;
  FRxNorm.Free;
  FLanguages.Free;
  inherited;
end;

procedure TCommonTerminologies.getCacheInfo(ci: TCacheInformation);
var
  ss : TSnomedServices;
begin
  if FLanguages <> nil then
    ci.Add('Languages', FLanguages.sizeInBytes(ci.magic));
  if FLoinc <> nil then
    ci.Add('Loinc', FLoinc.sizeInBytes(ci.magic));
  for ss in FSnomed do
    ci.Add('SCT/'+ss.EditionName+'/'+ss.VersionDate, ss.sizeInBytes(ci.magic));
  if FUcum <> nil then
    ci.Add('Ucum', FUcum.sizeInBytes(ci.magic));
  if FACIR <> nil then
    ci.Add('ACIR', FACIR.sizeInBytes(ci.magic));
  if FProviderClasses <> nil then
    ci.Add('ProviderClasses', FProviderClasses.sizeInBytes(ci.magic));
  if FNDFRT <> nil then
    ci.Add('NDFRT', FNDFRT.sizeInBytes(ci.magic));
end;

procedure TCommonTerminologies.getSummary(b: TStringBuilder);
var
  sn : TSnomedServices;
  sp : TSnomedProvider;
begin
  if FLoinc = nil then
    b.append('<li>LOINC: not loaded</li>')
  else
    b.append('<li>LOINC: '+FLoinc.version(nil)+' ('+inttostr(FLoinc.UseCount)+' uses)');


  if FSnomed.Count = 0 then
    b.append('<li>Snomed: not loaded</li>')
  else for sn in FSnomed do
    b.append('<li>Snomed: '+sn.VersionUri+' ('+inttostr(sn.UseCount)+' uses)');

  if FUcum = nil then
    b.append('<li>Ucum: not loaded</li>')
  else
    b.append('<li>Ucum: '+FUcum.version(nil)+' ('+inttostr(FUcum.UseCount)+' uses)');

  if FRxNorm = nil then
    b.append('<li>RxNorm: not loaded</li>')
  else
    b.append('<li>RxNorm: '+FRxNorm.version(nil)+' ('+inttostr(FRxNorm.UseCount)+' uses)');

  if FNDFRT = nil then
    b.append('<li>NDFRT: not loaded</li>')
  else
    b.append('<li>NDFRT: '+NDFRT.version(nil)+' ('+inttostr(NDFRT.UseCount)+' uses)');

  if FUnii = nil then
    b.append('<li>Unii: not loaded</li>')
  else
    b.append('<li>Unii: '+FUnii.version(nil)+' ('+inttostr(FUnii.UseCount)+' uses)');

  if FACIR = nil then
    b.append('<li>ACIR: not loaded</li>')
  else
    b.append('<li>ACIR: '+FACIR.version(nil)+' ('+inttostr(FACIR.UseCount)+' uses)');
end;

function TCommonTerminologies.link: TCommonTerminologies;
begin
  result := TCommonTerminologies(inherited Link);
end;

procedure TCommonTerminologies.Unload;
begin
  FLanguages.clear;
  FProviderClasses.clear;
end;

procedure TCommonTerminologies.listVersions(url: String; list: TStringList);
var
  pc : TCodeSystemProviderFactory;
begin
  for pc in FProviderClasses.Values do
    if (pc.systemUri() = url) and (pc.version() <> '') then
      if list.IndexOf(pc.version()) = -1 then
        list.Add(pc.version());
end;

procedure TCommonTerminologies.load(txlist: TFHIRServerConfigSection; testing : boolean);
var
  tx : TFHIRServerConfigSection;
  s : string;
  sn: TSnomedServices;
  sp : TSnomedProviderFactory;
//  def : boolean;
  p : TUriServices;
  function fixFile(name, fn : String) : String;
  begin
    if FileExists(fn) then
      result := fn
    else if FileExists(FilePath([fn])) then
      result := FilePath([fn])
    else if FileExists(FilePath(['[exe]', fn])) then
      result := FilePath(['[exe]', fn])
    else if FileExists(FilePath(['[curr]', fn])) then
      result := FilePath(['[curr]', fn])
    else
      raise EFslException.Create('Unable to find the '+name+' file "'+fn+'"');
  end;

begin
  s := fixFile('lang', FSettings.LangFile);
  if not FileExists(s) then
    raise EFHIRException.Create('IETF language file "'+FSettings.LangFile+'" not found - necessary for server operation');
  FLanguages := TIETFLanguageDefinitions.Create(FileToString(s, TEncoding.ASCII));
  FProviderClasses := TFslMap<TCodeSystemProviderFactory>.Create('tc.common');

  p := TUriServices.Create(FLanguages.link);
  try
    FProviderClasses.Add(p.systemUri(nil), TCodeSystemProviderGeneralFactory.create(p.link));
    FProviderClasses.Add(p.systemUri(nil)+URI_VERSION_BREAK+p.version(nil), TCodeSystemProviderGeneralFactory.create(p.link));
  finally
    p.free;
  end;

  add(TIETFLanguageCodeServices.Create(FLanguages.link)).free;
  add(TACIRServices.Create(FLanguages.link)).free;
  add(TAreaCodeServices.Create(FLanguages.link)).free;
  add(TIso4217Services.Create(FLanguages.link)).free;
  add(TMimeTypeCodeServices.Create(FLanguages.link)).free;
  add(TCountryCodeServices.Create(FLanguages.link)).free;
  add(TUSStateServices.Create(FLanguages.link)).free;
  add(THGVSProvider.Create(FLanguages.link)).free;

  for tx in txList.sections do
  begin
    s := tx.Name;
    if tx['active'].valueBool and (not testing or (tx['when-testing'].readAsBool)) then
    begin
      if tx['type'].value = 'snomed' then
      begin
        Logging.log('load '+s+' from '+tx['source'].value);
        sn := TSnomedServices.Create(FLanguages.link);
        try
          sn.Load(fixFile('sct', tx['source'].value), tx['default'].value = 'true');
          sp := TSnomedProviderFactory.create(sn.link);
          try
            add(sp, tx['default'].readAsBool);
            if not FProviderClasses.ContainsKey(sn.systemUri()+URI_VERSION_BREAK+sn.EditionUri) then
              FProviderClasses.Add(sn.systemUri()+URI_VERSION_BREAK+sn.EditionUri, sp.link);
          finally
            sp.free;
          end;
          snomed.Add(sn.Link);
          if tx['default'].readAsBool then
            DefSnomed := sn.Link;
        finally
          sn.Free;
        end;
      end
      else if tx['type'].value = 'loinc' then
      begin
        Logging.log('load '+s+' from '+tx['source'].value);
        Loinc := TLoincServices.Create(FLanguages.link);
        try
          Loinc.Load(fixFile('loinc', tx['source'].value));
          add(Loinc.link);
        finally
          Loinc.free;
        end;
      end
      else if tx['type'].value = 'ucum' then
      begin
        Logging.log('load '+s+' from '+tx['source'].value);
        Ucum := TUcumServices.Create(FLanguages.link);
        Ucum.Import(fixFile('ucum', tx['source'].value));
      end
      else if tx['type'].value = 'rxnorm' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        RxNorm := TRxNormServices.create(FLanguages.link, connectToDatabase(tx))
      end
      else if tx['type'].value = 'ndc' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        NDC := TNDCServices.create(FLanguages.link, connectToDatabase(tx), tx['version'].value)
      end
      else if tx['type'].value = 'ndfrt' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        NDFRT := TNDFRTServices.create(FLanguages.link, connectToDatabase(tx))
      end
      else if tx['type'].value = 'unii' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        Unii := TUniiServices.Create(FLanguages.link, connectToDatabase(tx))
      end
      else
        raise EFslException.Create('Unknown type '+tx['type'].value);
    end;
  end;
end;

procedure TCommonTerminologies.SetLoinc(const Value: TLOINCServices);
begin
  FLoinc.Free;
  FLoinc := Value;
end;

procedure TCommonTerminologies.SetRxNorm(const Value: TRxNormServices);
begin
  if FRxNorm <> nil then
  begin
    FProviderClasses.Remove(FRxNorm.systemUri(nil));
    FProviderClasses.Remove(FRxNorm.systemUri(nil)+URI_VERSION_BREAK+FRxNorm.version(nil));
  end;
  FRxNorm.Free;
  FRxNorm := Value;
  if FRxNorm <> nil then
  begin
    FProviderClasses.add(FRxNorm.systemUri(nil), TCodeSystemProviderGeneralFactory.create(FRxNorm.Link));
    FProviderClasses.add(FRxNorm.systemUri(nil)+URI_VERSION_BREAK+FRxNorm.version(nil), TCodeSystemProviderGeneralFactory.create(FRxNorm.Link));
  end;
end;

procedure TCommonTerminologies.SetNDC(const Value: TNDCServices);
begin
  if FNDC <> nil then
  begin
    FProviderClasses.Remove(FNDC.systemUri(nil));
    FProviderClasses.Remove(FNDC.systemUri(nil)+URI_VERSION_BREAK+FNDC.version(nil));
  end;
  FNDC.Free;
  FNDC := Value;
  if FNDC <> nil then
  begin
    FProviderClasses.add(FNDC.systemUri(nil), TCodeSystemProviderGeneralFactory.create(FNDC.Link));
    FProviderClasses.add(FNDC.systemUri(nil)+URI_VERSION_BREAK+FNDC.version(nil), TCodeSystemProviderGeneralFactory.create(FNDC.Link));
  end;
end;

procedure TCommonTerminologies.SetNDFRT(const Value: TNDFRTServices);
begin
  if FNDFRT <> nil then
  begin
    FProviderClasses.Remove(FNDFRT.systemUri(nil));
    FProviderClasses.Remove(FNDFRT.systemUri(nil)+URI_VERSION_BREAK+FNDFRT.version(nil));
  end;
  FNDFRT.Free;
  FNDFRT := Value;
  if FNDFRT <> nil then
  begin
    FProviderClasses.add(FNDFRT.systemUri(nil), TCodeSystemProviderGeneralFactory.create(FNDFRT.Link));
    FProviderClasses.add(FNDFRT.systemUri(nil)+URI_VERSION_BREAK+FNDFRT.version(nil), TCodeSystemProviderGeneralFactory.create(FNDFRT.Link));
  end;
end;

procedure TCommonTerminologies.SetUnii(const Value: TUniiServices);
begin
  if FUnii <> nil then
  begin
    FProviderClasses.Remove(FUnii.systemUri(nil));
    FProviderClasses.Remove(FUnii.systemUri(nil)+URI_VERSION_BREAK+FUnii.version(nil));
  end;
  FUnii.Free;
  FUnii := Value;
  if FUnii <> nil then
  begin
    FProviderClasses.add(FUnii.systemUri(nil), TCodeSystemProviderGeneralFactory.create(FUnii.Link));
    FProviderClasses.add(FUnii.systemUri(nil)+URI_VERSION_BREAK+FUnii.version(nil), TCodeSystemProviderGeneralFactory.create(FUnii.Link));
  end;
end;

procedure TCommonTerminologies.sweepSnomed(callback: TFhirServerMaintenanceThreadTaskCallBack);
var
  ss : TSnomedServices;
begin
  callback(self, 'Sweeping Snomed', -1);
  for ss in FSnomed do
    if ss <> FDefSnomed then
      ss.checkUnloadMe;
end;

procedure TCommonTerminologies.SetACIR(const Value: TACIRServices);
begin
  if FACIR <> nil then
  begin
    FProviderClasses.Remove(FACIR.systemUri(nil));
    FProviderClasses.Remove(FACIR.systemUri(nil)+URI_VERSION_BREAK+FACIR.version(nil));
  end;
  FACIR.Free;
  FACIR := Value;
  if FACIR <> nil then
  begin
    FProviderClasses.add(FACIR.systemUri(nil), TCodeSystemProviderGeneralFactory.create(FACIR.Link));
    FProviderClasses.add(FACIR.systemUri(nil)+URI_VERSION_BREAK+FACIR.version(nil), TCodeSystemProviderGeneralFactory.create(FACIR.Link));
  end;
end;

procedure TCommonTerminologies.SetDefSnomed(const Value: TSnomedServices);
begin
  FDefSnomed.Free;
  FDefSnomed := Value;
end;

procedure TCommonTerminologies.SetUcum(const Value: TUcumServices);
begin
  if FUcum <> nil then
  begin
    FProviderClasses.Remove(FUcum.systemUri(nil));
    FProviderClasses.Remove(FUcum.systemUri(nil)+URI_VERSION_BREAK+FUcum.version(nil));
  end;
  FUcum.Free;
  FUcum := Value;
  if FUcum <> nil then
  begin
    FProviderClasses.add(FUcum.systemUri(nil), TCodeSystemProviderGeneralFactory.create(FUcum.Link));
    FProviderClasses.add(FUcum.systemUri(nil)+URI_VERSION_BREAK+FUcum.version(nil), TCodeSystemProviderGeneralFactory.create(FUcum.Link));
  end;
end;

{ TCodeSystemProviderGeneralFactory }

constructor TCodeSystemProviderGeneralFactory.Create(provider: TCodeSystemProvider);
begin
  inherited Create;
  FProvider := provider;
end;

destructor TCodeSystemProviderGeneralFactory.destroy;
begin
  FProvider.free;
  inherited destroy;
end;

function TCodeSystemProviderGeneralFactory.getProvider: TCodeSystemProvider;
begin
  result := FProvider.link;
end;

function TCodeSystemProviderGeneralFactory.systemUri: String;
begin
  result := FProvider.systemUri(nil);
end;

function TCodeSystemProviderGeneralFactory.version: String;
begin
  result := FProvider.version(nil);
end;

function TCodeSystemProviderGeneralFactory.name: String;
begin
  result := FProvider.Name(nil);
end;

function TCodeSystemProviderGeneralFactory.TotalCount: integer;
begin
  result := FProvider.TotalCount;
end;

function TCodeSystemProviderGeneralFactory.versionDesc: String;
begin
  result := FProvider.version(nil);
end;

function TCodeSystemProviderGeneralFactory.description: String;
begin
  result := FProvider.description;
end;

{ TSnomedProviderFactory }

constructor TSnomedProviderFactory.Create(snomed: TSnomedServices);
begin
  inherited Create;
  FSnomed := snomed;
end;

destructor TSnomedProviderFactory.destroy;
begin
  FSnomed.Free;
  inherited destroy;
end;

function TSnomedProviderFactory.getProvider: TCodeSystemProvider;
begin
  result := TSnomedProvider.create(FSnomed.Link, nil);
end;

function TSnomedProviderFactory.systemUri: String;
begin
  result := FSnomed.systemUri;
end;

function TSnomedProviderFactory.version: String;
begin
  result := FSnomed.VersionUri;
end;

function TSnomedProviderFactory.name: String;
begin
  result := FSnomed.EditionName;
end;

function TSnomedProviderFactory.TotalCount: integer;
begin
  result := FSnomed.TotalCount;
end;

function TSnomedProviderFactory.versionDesc: String;
begin
  result := FSnomed.EditionName+'/'+FSnomed.VersionDate;
end;

function TSnomedProviderFactory.description: String;
begin
  result := 'SNOMED CT '+FSnomed.EditionName;
end;

end.

