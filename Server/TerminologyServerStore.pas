unit TerminologyServerStore;

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


interface

uses
  SysUtils, Classes, kCritSct, Generics.Defaults, Generics.Collections,
  StringSupport, TextUtilities,
  AdvObjects, AdvStringLists, AdvStringMatches, AdvObjectLists, AdvGenerics, AdvExceptions,
  KDBManager,
  FHIRTypes, FHIRResources, FHIRUtilities, FHIROperations, CDSHooksUtilities,
  TerminologyServices, LoincServices, UCUMServices, SnomedServices, RxNormServices, UniiServices, ACIRServices, UriServices, ICD10Services,
  AreaCodeServices, IETFLanguageCodeServices, FHIRLog,
  YuStemmer;

const
  URI_VERSION_BREAK = '#';

Type
  TConceptAdornment = class (TAdvStringList)
  private
    FParent: TFHIRCodeSystemConcept;
  public
    property parent : TFHIRCodeSystemConcept read FParent write FParent; // not owned, can't be
  end;

  TCodeSystemAdornment = class (TAdvObject)
  private
    FMap : TAdvMap<TFhirCodeSystemConcept>;
  public
    constructor Create(map : TAdvMap<TFhirCodeSystemConcept>);
    destructor Destroy; override;
    property map : TAdvMap<TFhirCodeSystemConcept> read FMap;
  end;

  TLoadedConceptMap = class (TAdvObject)
  private
    FSource: TFhirValueSet;
    FResource: TFhirConceptMap;
    FTarget: TFhirValueSet;
    procedure SetResource(const Value: TFhirConceptMap);
    procedure SetSource(const Value: TFhirValueSet);
    procedure SetTarget(const Value: TFhirValueSet);

    function HasTranslation(list : TFhirConceptMapGroupList; system, code : String; out maps : TFhirConceptMapGroupElementTargetList) : boolean; overload;
  public
    Destructor Destroy; override;
    function Link : TLoadedConceptMap; overload;
    Property Source : TFhirValueSet read FSource write SetSource;
    Property Resource : TFhirConceptMap read FResource write SetResource;
    Property Target : TFhirValueSet read FTarget write SetTarget;

    function HasTranslation(system, code : String; out maps : TFhirConceptMapGroupElementTargetList) : boolean; overload;
  end;

  TLoadedConceptMapList = class (TAdvObjectList)
  private
    function getMap(iIndex: integer): TLoadedConceptMap;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    Property map[iIndex : integer] : TLoadedConceptMap read getMap; default;

  end;
  TFhirCodeSystemProviderContext = class (TCodeSystemProviderContext)
  private
    context : TFhirCodeSystemConcept;
  public
    constructor Create(context : TFhirCodeSystemConcept); overload;
    destructor Destroy; override;
  end;

  TFhirCodeSystemConceptMatch = class (TAdvObject)
  private
    FItem : TFhirCodeSystemConcept;
    FRating : double;
  public
    Constructor Create(item : TFhirCodeSystemConcept; rating : double);
    Destructor Destroy; override;
  end;

  TFhirCodeSystemProviderFilterContext = class (TCodeSystemProviderFilterContext, IComparer<TFhirCodeSystemConceptMatch>)
  private
    ndx : integer;
    concepts : TAdvList<TFhirCodeSystemConceptMatch>;

    procedure Add(item : TFhirCodeSystemConcept; rating : double);
    function Compare(const Left, Right: TFhirCodeSystemConceptMatch): Integer;
    procedure sort;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
  end;

  TFHIRCodeSystemEntry = class (TAdvObject)
  private
    FCodeSystem : TFHIRCodeSystem;
    FValueset : TFHIRValueSet;
    FSupplements : TAdvList<TFHIRCodeSystem>;
    function GetHasSupplements: boolean;
    function GetSupplements: TAdvList<TFHIRCodeSystem>;
    procedure SetCodeSystem(const Value: TFHIRCodeSystem);
  public
    Constructor Create({$IFNDEF FHIR2} cs : TFhirCodeSystem {$ELSE} cs : TFHIRValueSet {$ENDIF});
    destructor Destroy; override;

    function Link : TFHIRCodeSystemEntry; overload;

    property CodeSystem : TFHIRCodeSystem read FCodeSystem write SetCodeSystem;
    Property hasSupplements : boolean read GetHasSupplements;
    property Supplements : TAdvList<TFHIRCodeSystem> read GetSupplements;
  end;

  TFhirCodeSystemProvider = class (TCodeSystemProvider)
  private
    FCs : TFhirCodeSystemEntry;
    FMap : TAdvMap<TFhirCodeSystemConcept>;

    function LocateCode(code : String) : TFhirCodeSystemConcept;
    function doLocate(code : String) : TFhirCodeSystemProviderContext; overload;
    function doLocate(list : TFhirCodeSystemConceptList; code : String) : TFhirCodeSystemProviderContext; overload;
    function getParent(ctxt : TFhirCodeSystemConcept) : TFhirCodeSystemConcept;
    procedure FilterCodes(dest : TFhirCodeSystemProviderFilterContext; source : TFhirCodeSystemConceptList; filter : TSearchFilterText);
    procedure iterateCodes(base: TFhirCodeSystemConcept; list: TFhirCodeSystemProviderFilterContext);
    function locCode(list: TFhirCodeSystemConceptList; code: String): TFhirCodeSystemConcept;
    {$IFNDEF FHIR2}
    function getProperty(code : String) : TFhirCodeSystemProperty;
    {$ENDIF}
  public
    constructor Create(vs : TFhirCodeSystemEntry); overload;
    destructor Destroy; override;

    function name(context: TCodeSystemProviderContext): String; override;
    function version(context: TCodeSystemProviderContext): String; override;
    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; lang : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; overload; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; lang : String) : string; override;
    procedure Displays(code : String; list : TStringList; lang : String); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; lang : String); override;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(card : TCDSHookCard; slang, baseURL, code, display : String); override;
    procedure extendLookup(ctxt : TCodeSystemProviderContext; lang : String; props : TList<String>; resp : TFHIRLookupOpResponse); override;
    function subsumesTest(codeA, codeB : String) : String; override;
  end;

  // the terminology server maintains a cache of terminology related resources
  // the rest server notifies terminology server whenever this list changes
  // (and at start up)
  TTerminologyServerStore = class (TAdvObject)
  private
    FLoinc : TLOINCServices;
    FSnomed : TAdvList<TSnomedServices>;
    FIcd10 : TAdvList<TICD10Provider>;
    FDefSnomed : TSnomedServices;
    FUcum : TUcumServices;
    FRxNorm : TRxNormServices;
    FNciMeta : TNciMetaServices;
    FUnii : TUniiServices;
    FACIR : TACIRServices;
    FStem : TYuStemmer_8;
    FTagid : integer;

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
    FCodeSystemsById : TAdvMap<TFHIRCodeSystemEntry>; // all current value sets that define systems, by their url
    FCodeSystemsByUrl : TAdvMap<TFHIRCodeSystemEntry>; // all current value sets that define systems, by their url
    FCodeSystemsByVsUrl : TAdvMap<TFHIRCodeSystemEntry>; // all current value sets that define systems, by their url
    FSupplementsById : TAdvMap<TFHIRCodeSystem>; // All supplements

    FBaseValueSets : TAdvMap<TFHIRValueSet>; // value sets out of the specification - these can be overriden, but they never go away
    FBaseCodeSystems : TAdvMap<TFHIRCodeSystemEntry>; // value sets out of the specification - these can be overriden, but they never go away

    FBaseConceptMaps : TAdvMap<TLoadedConceptMap>; // value sets out of the specification - these can be overriden, but they never go away
    FConceptMapsById : TAdvMap<TLoadedConceptMap>;
    FConceptMapsByURL : TAdvMap<TLoadedConceptMap>;

    FProviderClasses : TAdvMap<TCodeSystemProvider>;
    FMaintenanceThreadStatus : String;
    FSubscriptionThreadStatus : String;
    FEmailThreadStatus : String;

    procedure UpdateConceptMaps;
    procedure BuildStems(cs : TFhirValueSetCodeSystem);

    procedure SetLoinc(const Value: TLOINCServices);
    procedure SetDefSnomed(const Value: TSnomedServices);
    procedure SetUcum(const Value: TUcumServices);
    procedure SetRxNorm(const Value: TRxNormServices);
    procedure SetNciMeta(const Value: TNciMetaServices);
    procedure SetUnii(const Value: TUniiServices);
    procedure SetACIR(const Value: TACIRServices);
    procedure checkCodeSystem(vs : TFHIRCodeSystem);

    function GetMaintenanceThreadStatus: String;
    procedure SetMaintenanceThreadStatus(const Value: String);
    function GetSubscriptionThreadStatus: String;
    procedure SetSubscriptionThreadStatus(const Value: String);
    function GetEmailThreadStatus: String;
    procedure SetEmailThreadStatus(const Value: String);
    procedure checkForDuplicates(codes: TStringList; list: TFhirCodeSystemConceptList; url : String);
    function checkVersion(system, version: String; profile: TFHIRExpansionProfile): String;
    procedure AddCodeSystemToCache({$IFNDEF FHIR2} cs : TFhirCodeSystem; {$ELSE} cs : TFHIRValueSet; {$ENDIF} base : boolean);
    procedure RemoveCodeSystemFromCache(id : String);
  protected
    FLock : TCriticalSection;  // it would be possible to use a read/write lock, but the complexity doesn't seem to be justified by the short amount of time in the lock anyway
    FDB : TKDBManager;
    procedure invalidateVS(id : String); virtual;
    procedure getSummary(b : TStringBuilder);
  public
    Constructor Create(db : TKDBManager); virtual;
    Destructor Destroy; Override;
    Function Link : TTerminologyServerStore; overload;

    Property Loinc : TLOINCServices read FLoinc write SetLoinc;
    Property Snomed : TAdvList<TSnomedServices> read FSnomed;
    Property Icd10 : TAdvList<TICD10Provider> read FIcd10;
    Property DefSnomed : TSnomedServices read FDefSnomed write SetDefSnomed;
    Property Ucum : TUcumServices read FUcum write SetUcum;
    Property RxNorm : TRxNormServices read FRxNorm write SetRxNorm;
    Property NciMeta : TNciMetaServices read FNciMeta write SetNciMeta;
    Property Unii : TUniiServices read FUnii write SetUnii;
    Property ACIR : TACIRServices read FACIR write SetACIR;
    Property DB : TKDBManager read FDB;

    // maintenance procedures
    procedure SeeSpecificationResource(resource : TFHIRResource);
    procedure SeeTerminologyResource(resource : TFHIRResource);
    procedure checkTerminologyResource(resource : TFHIRResource);
    procedure DropTerminologyResource(aType : TFhirResourceType; id : String);

    // access procedures. All return values are owned, and must be freed
    Function getProvider(system : String; version : String; profile : TFHIRExpansionProfile; noException : boolean = false) : TCodeSystemProvider; overload;
    Function getProvider(codesystem : TFHIRCodeSystem; profile : TFHIRExpansionProfile) : TCodeSystemProvider; overload;
    function getValueSetByUrl(url : String) : TFHIRValueSet;
    function getValueSetById(id : String) : TFHIRValueSet;
    function getCodeSystemById(id : String) : TFHIRCodeSystem;
    function getCodeSystemByValueSet(vs : String) : TFHIRCodeSystem;
    function getCodeSystem(url : String) : TFHIRCodeSystem;
    function hasCodeSystem(url : String) : Boolean;
    function getConceptMapById(id : String) : TLoadedConceptMap;
    function getConceptMapBySrcTgt(src, tgt : String) : TLoadedConceptMap;

    // publishing access
    function GetCodeSystemList : TFHIRCodeSystemList;
    function GetValueSetList : TFHIRValueSetList;
    function GetConceptMapList : TLoadedConceptMapList;
    Property ProviderClasses : TAdvMap<TCodeSystemProvider> read FProviderClasses;
    function ValueSetCount : integer;
    function CodeSystemCount : integer;

    {$IFDEF FHIR2}
    procedure declareSystems(oConf : TFhirCapabilityStatement);
    {$ENDIF}
    {$IFNDEF FHIR2}
    procedure declareCodeSystems(list : TFhirResourceList);
    {$ENDIF}
    function supportsSystem(s : String; version : String) : boolean;
    function subsumes(uri1, code1, uri2, code2 : String) : boolean; overload;
    function subsumes(cs : TFHIRCodeSystem; codeA, codeB : TFHIRCoding) : string; overload;
    function NextClosureKey : integer;
    function NextClosureEntryKey : integer;
    function NextConceptKey : integer;
    function NextValueSetKey : integer;
    function NextValueSetMemberKey : integer;
    Property MaintenanceThreadStatus : String read GetMaintenanceThreadStatus write SetMaintenanceThreadStatus;
    Property SubscriptionThreadStatus : String read GetSubscriptionThreadStatus write SetSubscriptionThreadStatus;
    Property EmailThreadStatus : String read GetEmailThreadStatus write SetEmailThreadStatus;
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
    function getDisplay(code : String; lang : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; lang : String) : string; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; lang : String); overload; override;
    procedure Displays(code : String; list : TStringList; lang : String); overload; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
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
  end;

function TAllCodeSystemsProvider.TotalCount : integer;
begin
  result := FStore.DefSnomed.TotalCount + FStore.Loinc.TotalCount + FActCode.TotalCount + FStore.Unii.TotalCount;
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
      acssSnomed : result := FStore.DefSnomed.System(c.context);
      acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.System(c.context) else result := '??';
      acssNciMeta : if FStore.NciMeta <> nil then result := FStore.NciMeta.System(c.context) else result := '??';
      acssUnii : result := FStore.Unii.System(c.context);
      acssActCode : result := FActCode.System(c.context);
    end;
  end;
end;

function TAllCodeSystemsProvider.getDisplay(code : String; lang : String):String;
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
    ctxt.snomed := FStore.DefSnomed.getPrepContext;
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
function TAllCodeSystemsProvider.locate(code : String; var message : String) : TCodeSystemProviderContext;
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
    acssSnomed : result := FStore.DefSnomed.IsAbstract(c.context);
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
    acssSnomed : result := FStore.DefSnomed.Code(c.context);
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
  FActCode := store.getProvider('http://hl7.org/fhir/v3/ActCode', '', nil);
end;

function TAllCodeSystemsProvider.Display(context : TCodeSystemProviderContext; lang : String) : string;
var
  c : TAllCodeSystemsProviderContext;
begin
  c := context as TAllCodeSystemsProviderContext;
  case c.source of
    acssLoinc : result := FStore.Loinc.Display(c.context, lang)+' (LOINC: '+FStore.Loinc.Code(c.context)+')';
    acssSnomed : result := FStore.DefSnomed.Display(c.context, lang)+' (S-CT: '+FStore.DefSnomed.Code(c.context)+')';
    acssRxNorm : if FStore.RxNorm <> nil then result := FStore.RxNorm.Display(c.context, lang)+' (RxN: '+FStore.RxNorm.Code(c.context)+')' else result := '';
    acssNciMeta : if FStore.NciMeta <> nil then result := FStore.NciMeta.Display(c.context, lang)+' (RxN: '+FStore.NciMeta.Code(c.context)+')' else result := '';
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
    acssSnomed : result := FStore.DefSnomed.Definition(c.context);
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

procedure TAllCodeSystemsProvider.Displays(context : TCodeSystemProviderContext; list : TStringList; lang : String);
begin
  raise Exception.Create('Not Created Yet');
end;
procedure TAllCodeSystemsProvider.Displays(code : String; list : TStringList; lang : String);
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
      ctxt.snomed := FStore.Defsnomed.searchFilter(filter, TAllCodeSystemsProviderFilterPreparationContext(prep).snomed, sort);
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
    FStore.DefSnomed.prepare(ctxt.snomed);
    if FStore.RxNorm <> nil then
      FStore.RxNorm.prepare(ctxt.rxnorm);
    if FStore.NciMeta <> nil then
      FStore.NciMeta.prepare(ctxt.NciMeta);
//    FStore.FUnii.prepare(ctxt.unii);
    FActCode.prepare(ctxt.actcode);
  end;
end;

function TAllCodeSystemsProvider.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
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
      c.snomedDone := not FStore.DefSnomed.FilterMore(c.snomed);
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
      c.context := FStore.DefSnomed.FilterConcept(d.snomed);
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
  FStore.DefSnomed.Close(c.snomed);
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
    FStore.DefSnomed.Close(c.snomed);
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
    acssSnomed : FStore.DefSnomed.Close(c.context);
    acssRxNorm : if FStore.RxNorm <> nil then FStore.RxNorm.Close(c.context);
    acssNciMeta : if FStore.NciMeta <> nil then FStore.NciMeta.Close(c.context);
    acssUnii : FStore.Unii.Close(c.context);
    acssActCode : FActCode.Close(c.context);
  end;
  ctxt.free;
end;


{ TTerminologyServerStore }

procedure TTerminologyServerStore.BuildStems(cs: TFhirValueSetCodeSystem);
  function stems(c : TFhirCodeSystemConcept) : TConceptAdornment;
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
        result.Add(lowercase(FStem.calc(s)));
    end;
    result.SortAscending;
  end;
  procedure processConcepts(parent : TFhirCodeSystemConcept; list : TFhirCodeSystemConceptList; map : TAdvMap<TFhirCodeSystemConcept>);
  var
    c : TFhirCodeSystemConcept;
  begin
    for c in list do
    begin
      stems(c).parent := parent;
      if map.ContainsKey(c.code) then
        logt('Duplicate code '+c.code+' in '+{$IFDEF FHIR2}'??'{$ELSE}cs.url{$ENDIF})
      else
        map.Add(c.code, c.Link);
      processConcepts(c, c.conceptList, map);
    end;
  end;
var
  map : TAdvMap<TFhirCodeSystemConcept>;
begin
  map := TAdvMap<TFhirCodeSystemConcept>.create;
  cs.Tag := TCodeSystemAdornment.create(map);
  processConcepts(nil, cs.conceptList, map);
end;


procedure TTerminologyServerStore.checkForDuplicates(codes : TStringList; list : TFhirCodeSystemConceptList; url : String);
var
  cc : TFhirCodeSystemConcept;
  i : integer;
begin
  for cc in list do
  begin
    if cc.code <> '' then
    begin
      if codes.Find(cc.code, i) then
        raise Exception.Create('Duplicate code: '+cc.code+' in value set '+url);
      codes.Add(cc.code);
    end;
    checkForDuplicates(codes, cc.conceptList, url);
  end;
end;

procedure TTerminologyServerStore.checkTerminologyResource(resource: TFHIRResource);
begin
  resource.checkNoImplicitRules('Repository.SeeResource', 'Resource');
  TFhirDomainResource(resource).checkNoModifiers('Repository.SeeResource', 'Resource');
  if (resource.ResourceType = frtCodeSystem) then
    checkCodeSystem(resource as TFHIRCodeSystem);
end;

function exempt(vs : TFHIRCodeSystem) : boolean;
begin
  {$IFDEF FHIR2}
  if vs.URL.startsWith('http://hl7.org/fhir/ValueSet/v2-')
    and StringArrayExists(['0003', '0207', '0277', '0278', '0279', '0281', '0294', '0396'],
        vs.URL.Substring(vs.url.Length-4)) then
      result := true
  else
  {$ENDIF}
    result := false;
end;

procedure TTerminologyServerStore.checkCodeSystem(vs: TFHIRCodeSystem);
{$IFDEF FHIR2}
var
  list : TStringList;
{$ENDIF}
begin
  if (exempt(vs)) then
    exit;

  {$IFDEF FHIR2}
  if (vs.codeSystem <> nil) then
  begin
    list := TStringList.Create;
    try
      list.Sorted := true;
      list.CaseSensitive := vs.codeSystem.caseSensitive;
      checkForDuplicates(list, vs.codeSystem.conceptList, vs.url);
    finally
      list.Free;
    end;
  end;
  {$ENDIF}
end;

function TTerminologyServerStore.CodeSystemCount: integer;
begin
  FLock.Lock('CodeSystemCount');
  try
    result := FCodeSystemsById.Count;
  finally
    FLock.Unlock;
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
  FCodeSystemsById := TAdvMap<TFhirCodeSystemEntry>.create;
  FCodeSystemsByUrl := TAdvMap<TFhirCodeSystemEntry>.create;
  FCodeSystemsByVsUrl := TAdvMap<TFhirCodeSystemEntry>.create;
  FBaseValueSets := TAdvMap<TFhirValueSet>.create;
  FBaseCodeSystems := TAdvMap<TFHIRCodeSystemEntry>.create;
  FSupplementsById := TAdvMap<TFHIRCodeSystem>.create;


  FBaseConceptMaps := TAdvMap<TLoadedConceptMap>.create;
  FConceptMapsById := TAdvMap<TLoadedConceptMap>.create;
  FConceptMapsByURL := TAdvMap<TLoadedConceptMap>.create;

  FSnomed := TAdvList<TSnomedServices>.create;
  FIcd10 := TAdvList<TICD10Provider>.create;
  p := TUriServices.Create();
  FProviderClasses.Add(p.system(nil), p);
  FProviderClasses.Add(p.system(nil)+URI_VERSION_BREAK+p.version(nil), p.link);

  FStem := GetStemmer_8('english');

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

{$IFNDEF FHIR2}
procedure TTerminologyServerStore.declareCodeSystems(list : TFhirResourceList);
  procedure addCodeSystem(name, id, uri, version : String; count : integer);
  var
    cs : TFhirCodeSystem;
  begin
    cs := TFhirCodeSystem.Create;
    list.Add(cs);
    cs.url := uri;
    cs.version := version;
    cs.name := name;
    cs.id := id;
    cs.status := PublicationStatusActive;
    cs.content := CodesystemContentModeNotPresent;
    if count <> 0 then
      cs.count := inttostr(count);
  end;
  function tail(url : String) : String;
  begin
    result := url.Substring(url.LastIndexOf('/')+1);
  end;
var
  sn : TSnomedServices;
  icd : TICD10Provider;
begin
  if FLoinc <> nil then
    addCodeSystem('LOINC', 'loinc', FLoinc.system(nil), FLoinc.version(nil), FLoinc.TotalCount);
  for sn in FSnomed do
    addCodeSystem('SNOMED CT', 'sct', sn.system(nil), sn.version(nil), sn.TotalCount);
  for icd in FIcd10 do
    addCodeSystem(icd.title, tail(icd.system(nil)), icd.system(nil), icd.version(nil), icd.TotalCount);
  if FUcum <> nil then
    addCodeSystem('Ucum', 'ucum', FUcum.system(nil), FUcum.version(nil), FUcum.TotalCount);
  if FRxNorm <> nil then
    addCodeSystem('RxNorm', 'rxnorm', FRxNorm.system(nil), FRxNorm.version(nil), FRxNorm.TotalCount);
  if FUnii <> nil then
    addCodeSystem('Unii', 'unii', FUnii.system(nil), FUnii.version(nil), FUnii.TotalCount);
  if FACIR <> nil then
    addCodeSystem('ACIR', 'acir', FACIR.system(nil), FACIR.version(nil), FACIR.TotalCount);
end;
{$ENDIF}

{$IFDEF FHIR2}
procedure TTerminologyServerStore.declareSystems(oConf: TFhirCapabilityStatement);
var
  e : TFhirExtension;
  c, s : String;
  cp : TCodeSystemProvider;
begin
  for c in FProviderClasses.Keys do
    if (c.Contains(URI_VERSION_BREAK)) then
    begin
      cp := FProviderClasses[c];
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
{$ENDIF}

destructor TTerminologyServerStore.Destroy;
begin
  FStem.Free;
  FValueSetsById.Free;
  FValueSetsByURL.Free;
  FCodeSystemsById.Free;
  FCodeSystemsByUrl.Free;
  FCodeSystemsByVsUrl.Free;
  FBaseValueSets.Free;
  FBaseCodeSystems.Free;
  FSupplementsByid.Free;


  FBaseConceptMaps.Free;
  FConceptMapsById.Free;
  FConceptMapsByURL.Free;

  FProviderClasses.Free;

  FLoinc.free;
  FDefSnomed.Free;
  FSnomed.free;
  FUnii.Free;
  FACIR.Free;
  FUcum.free;
  FLock.Free;
  FRxNorm.Free;
  FNciMeta.Free;
  FDB.free;
  inherited;
end;

procedure TTerminologyServerStore.SetLoinc(const Value: TLOINCServices);
begin
  if FLoinc <> nil then
  begin
    FProviderClasses.Remove(FLoinc.system(nil));
    FProviderClasses.Remove(FLoinc.system(nil)+URI_VERSION_BREAK+FLoinc.version(nil));
  end;
  FLoinc.Free;
  FLoinc := Value;
  if FLoinc <> nil then
  begin
    FProviderClasses.add(FLoinc.system(nil), FLoinc.Link);
    FProviderClasses.add(FLoinc.system(nil)+URI_VERSION_BREAK+FLoinc.version(nil), FLoinc.Link);
  end;
end;

procedure TTerminologyServerStore.SetRxNorm(const Value: TRxNormServices);
begin
  if FRxNorm <> nil then
  begin
    FProviderClasses.Remove(FRxNorm.system(nil));
    FProviderClasses.Remove(FRxNorm.system(nil)+URI_VERSION_BREAK+FRxNorm.version(nil));
  end;
  FRxNorm.Free;
  FRxNorm := Value;
  if FRxNorm <> nil then
  begin
    FProviderClasses.add(FRxNorm.system(nil), FRxNorm.Link);
    FProviderClasses.add(FRxNorm.system(nil)+URI_VERSION_BREAK+FRxNorm.version(nil), FRxNorm.Link);
  end;
end;

procedure TTerminologyServerStore.SetNciMeta(const Value: TNciMetaServices);
begin
  if FNciMeta <> nil then
  begin
    FProviderClasses.Remove(FNciMeta.system(nil));
    FProviderClasses.Remove(FNciMeta.system(nil)+URI_VERSION_BREAK+FNciMeta.version(nil));
  end;
  FNciMeta.Free;
  FNciMeta := Value;
  if FNciMeta <> nil then
  begin
    FProviderClasses.add(FNciMeta.system(nil), FNciMeta.Link);
    FProviderClasses.add(FNciMeta.system(nil)+URI_VERSION_BREAK+FNciMeta.version(nil), FNciMeta.Link);
  end;
end;

procedure TTerminologyServerStore.SetUnii(const Value: TUniiServices);
begin
  if FUnii <> nil then
  begin
    FProviderClasses.Remove(FUnii.system(nil));
    FProviderClasses.Remove(FUnii.system(nil)+URI_VERSION_BREAK+FUnii.version(nil));
  end;
  FUnii.Free;
  FUnii := Value;
  if FUnii <> nil then
  begin
    FProviderClasses.add(FUnii.system(nil), FUnii.Link);
    FProviderClasses.add(FUnii.system(nil)+URI_VERSION_BREAK+FUnii.version(nil), FUnii.Link);
  end;
end;

function TTerminologyServerStore.subsumes(cs: TFHIRCodeSystem; codeA, codeB: TFHIRCoding): string;
var
  prov : TCodeSystemProvider;
begin
  // later, see if we can translate instead
  if (cs.url <> codeA.system) then
    raise Exception.Create('System uri / code uri mismatch - not supported at this time ('+cs.url+'/'+codeA.system+')');
  if (cs.url <> codeB.system) then
    raise Exception.Create('System uri / code uri mismatch - not supported at this time ('+cs.url+'/'+codeB.system+')');
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

procedure TTerminologyServerStore.SetMaintenanceThreadStatus(const Value: String);
begin
  FLock.Lock;
  try
    FMaintenanceThreadStatus := Value;
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.SetSubscriptionThreadStatus(const Value: String);
begin
  FLock.Lock;
  try
    FSubscriptionThreadStatus := Value;
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.SetEmailThreadStatus(const Value: String);
begin
  FLock.Lock;
  try
    FEmailThreadStatus := Value;
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.SetACIR(const Value: TACIRServices);
begin
  if FACIR <> nil then
  begin
    FProviderClasses.Remove(FACIR.system(nil));
    FProviderClasses.Remove(FACIR.system(nil)+URI_VERSION_BREAK+FACIR.version(nil));
  end;
  FACIR.Free;
  FACIR := Value;
  if FACIR <> nil then
  begin
    FProviderClasses.add(FACIR.system(nil), FACIR.Link);
    FProviderClasses.add(FACIR.system(nil)+URI_VERSION_BREAK+FACIR.version(nil), FACIR.Link);
  end;
end;

procedure TTerminologyServerStore.SetDefSnomed(const Value: TSnomedServices);
begin
  if FDefSnomed <> nil then
    FProviderClasses.Remove(FDefSnomed.system(nil));
  FDefSnomed.Free;
  FDefSnomed := Value;
  if FDefSnomed <> nil then
    FProviderClasses.add(FDefSnomed.system(nil), FDefSnomed.Link);
end;

procedure TTerminologyServerStore.SetUcum(const Value: TUcumServices);
begin
  if FUcum <> nil then
  begin
    FProviderClasses.Remove(FUcum.system(nil));
    FProviderClasses.Remove(FUcum.system(nil)+URI_VERSION_BREAK+FUcum.version(nil));
  end;
  FUcum.Free;
  FUcum := Value;
  if FUcum <> nil then
  begin
    FProviderClasses.add(FUcum.system(nil), FUcum.Link);
    FProviderClasses.add(FUcum.system(nil)+URI_VERSION_BREAK+FUcum.version(nil), FUcum.Link);
  end;
end;

// ----  maintenance procedures ------------------------------------------------

function urlTail(path : String) : String;
begin
  result := path.substring(path.lastIndexOf('/')+1);
end;

function cmref(t : TFHIRType) : string;
begin
  if t is TFHIRUri then
    result := TFHIRUri(t).value
  else
    result := TFhirReference(t).reference;
end;

procedure TTerminologyServerStore.AddCodeSystemToCache({$IFNDEF FHIR2} cs : TFhirCodeSystem; {$ELSE} cs : TFHIRValueSet; {$ENDIF} base : boolean);
var
  cse, ct : TFHIRCodeSystemEntry;
  supp : TFHIRCodeSystem;
begin
  cse := TFHIRCodeSystemEntry.Create(cs.Link);
  try
    if base then
      FBaseCodeSystems.AddOrSetValue(cs.url, cse.Link);
    {$IFDEF FHIR4}
    if (cs.supplements <> nil) then
    begin
      FSupplementsById.AddOrSetValue(cs.id, cs.Link);
      if cs.supplements.reference.StartsWith('CodeSystem/') then
      begin
        if FCodeSystemsById.TryGetValue(cs.supplements.reference.Substring(11), ct) then
          ct.Supplements.Add(cs.Link);
      end
      else if FCodeSystemsByUrl.TryGetValue(cs.supplements.reference, ct) then
        ct.Supplements.Add(cs.Link);
    end
    else
    begin
   {$ENDIF}
      FCodeSystemsById.AddOrSetValue(cs.id, cse.Link);
      FCodeSystemsByUrl.AddOrSetValue(cs.url, cse.Link);
      {$IFNDEF FHIR2}
      if cs.valueSet <> '' then
        FCodeSystemsByVsUrl.AddOrSetValue(cs.valueSet, cse.Link);
      {$ENDIF}
      if (FDB <> nil) then // don't build stems in this case
        BuildStems({$IFDEF FHIR2}cs.codeSystem{$ELSE}cs{$ENDIF}); // todo: move this out of the lock
      {$IFDEF FHIR4}
      for supp in FSupplementsById.values do
        if (supp.supplements.reference = cs.url) or (supp.supplements.reference = 'CodeSystem/'+cs.id) then
          cse.Supplements.Add(cs.Link);
    end;
   {$ENDIF}
  finally
    cse.Free;
  end;
end;

procedure TTerminologyServerStore.RemoveCodeSystemFromCache(id : String);
var
  cse, cs1 : TFHIRCodeSystemEntry;
  cs : TFhirCodeSystem;
begin
  if (FCodeSystemsById.TryGetValue(id, cse)) then
  begin
    cs1 := FBaseCodeSystems[cse.CodeSystem.url].Link;
    try
      {$IFDEF FHIR2}
      FCodeSystemsByUrl.Remove(cse.FValueSet.codeSystem.system);
      FCodeSystemsByid.Remove(cse.FValueSet.id);
      if cs1 <> nil then
        AddCodeSystemToCache(cs1.FValueset, false);
      {$ELSE}
      FCodeSystemsByUrl.Remove(cse.codeSystem.system);
      FCodeSystemsByid.Remove(cse.codeSystem.id);
      if cs1 <> nil then
        AddCodeSystemToCache(cs1.codeSystem, false);
      {$ENDIF}
    finally
      cs1.free;
    end;
  end
  {$IFDEF FHIR4}
  else if FSupplementsById.TryGetValue(id, cs) then
  begin
    cs1 := FBaseCodeSystems[cs.url].Link;
    try
      if cs.supplements.reference.StartsWith('CodeSystem/') then
      begin
        if FCodeSystemsById.TryGetValue(cs.supplements.reference.Substring(11), cse) then
          cse.Supplements.Remove(cs);
      end
      else if FCodeSystemsByUrl.TryGetValue(cs.supplements.reference, cse) then
        cse.Supplements.remove(cs);
      if cs1 <> nil then
        AddCodeSystemToCache(cs1.codeSystem, false);
    finally
      cs1.Free;
    end;
  end;
  {$ENDIF}
end;

procedure TTerminologyServerStore.SeeSpecificationResource(resource : TFHIRResource);
var
  vs : TFhirValueSet;
  cs : TFhirCodeSystem;
  cm : TLoadedConceptMap;
  i : integer;
begin
  FLock.Lock('SeeSpecificationResource');
  try
    inc(FTagid);
    resource.TagInt := FTagId;
    if (resource.ResourceType = frtValueSet) then
    begin
      vs := TFhirValueSet(resource);
      if (vs.url = 'http://hl7.org/fhir/ValueSet/ucum-common') then
        FUcum.SetCommonUnits(vs.Link);

      FBaseValueSets.AddOrSetValue(vs.url, vs.Link);
      FValueSetsById.AddOrSetValue(vs.id, vs.Link);
      FValueSetsByUrl.AddOrSetValue(vs.url, vs.Link);
      {$IFDEF FHIR2}
      if (vs.codeSystem <> nil) then
        AddCodeSystemToCache(vs, true);
      {$ENDIF}
      UpdateConceptMaps;
    end
    {$IFNDEF FHIR2}
    else if (resource.ResourceType = frtCodeSystem) then
    begin
      cs := TFhirCodeSystem(resource);
      AddCodeSystemToCache(cs, true);
    end
    {$ENDIF}
    else if (resource.ResourceType = frtConceptMap) then
    begin
      cm := TLoadedConceptMap.Create;
      try
        cm.Resource := TFhirConceptMap(resource).Link;
        cm.Source := getValueSetByUrl(cmRef(cm.Resource.source));
        cm.Target := getValueSetByUrl(cmRef(cm.Resource.target));
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
  cs : TFHIRCodeSystem;
  cm : TLoadedConceptMap;
  cse : TFHIRCodeSystemEntry;
begin
  checkTerminologyResource(resource);

  FLock.Lock('SeeTerminologyResource');
  try
    inc(FTagid);
    resource.TagInt := FTagId;
    if (resource.ResourceType = frtValueSet) then
    begin
      vs := TFhirValueSet(resource);
      FValueSetsById.AddOrSetValue(vs.id, vs.Link);
      FValueSetsByUrl.AddOrSetValue(vs.url, vs.Link);
      invalidateVS(vs.url);
      {$IFDEF FHIR2}
      if (vs.codeSystem <> nil) then
        AddCodeSystemToCache(vs, false);
      {$ENDIF}
      UpdateConceptMaps;
    end
    {$IFNDEF FHIR2}
    else if (resource.ResourceType = frtCodeSystem) then
    begin
      cs := TFHIRCodeSystem(resource);
      AddCodeSystemToCache(cs, false);
    end
    {$ENDIF}
    else if (resource.ResourceType = frtConceptMap) then
    begin
      cm := TLoadedConceptMap.Create;
      try
        cm.Resource := TFhirConceptMap(resource).Link;
        cm.Source := getValueSetByUrl(cmRef(cm.Resource.source));
        cm.Target := getValueSetByUrl(cmRef(cm.Resource.target));
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
        {$IFDEF FHIR2}
        if (vs.codeSystem <> nil) then
          removeCodeSystemFromCache(vs.id);
        {$ENDIF}
        FValueSetsById.Remove(vs.id); // vs is no longer valid

        // add the base one back if we are dropping a value set that overrides it
        // current logical flaw: what if there's another one that overrides this? how do we prevent or deal with this?
        if vs1 <> nil then
        begin
          FValueSetsById.AddOrSetValue(vs.url, vs1.Link);
          {$IFDEF FHIR2}
          AddCodeSystemToCache(vs1, false);
          {$ENDIF}
        end;
        UpdateConceptMaps;
      end;
    end
    {$IFNDEF FHIR2}
    else if (aType = frtCodeSystem) then
    begin
      removeCodeSystemFromCache(id);
    end
    {$ENDIF}
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

function TTerminologyServerStore.GetMaintenanceThreadStatus: String;
begin
  FLock.Lock;
  try
    result := FMaintenanceThreadStatus;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.GetSubscriptionThreadStatus: String;
begin
  FLock.Lock;
  try
    result := FSubscriptionThreadStatus;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.GetEmailThreadStatus: String;
begin
  FLock.Lock;
  try
    result := FEmailThreadStatus;
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
    if cm.Resource.source = nil then
      cm.source := nil
    else
    begin
      cm.Source := getValueSetByUrl(cmRef(cm.Resource.source));
      if (cm.Source = nil) then
        cm.Source := getValueSetById(cmRef(cm.Resource.source));
    end;
    if cm.Resource.target = nil then
      cm.Target := nil
    else
    begin
      cm.Target := getValueSetByUrl(cmRef(cm.Resource.target));
      if (cm.Target = nil) then
        cm.Target := getValueSetById(cmRef(cm.Resource.target));
    end;
  end;
end;

function TTerminologyServerStore.ValueSetCount: integer;
begin
  FLock.Lock('ValueSetCount');
  try
    result := FValueSetsById.Count;
  finally
    FLock.Unlock;
  end;
end;

//---- access procedures. All return values are owned, and must be freed -------

function TTerminologyServerStore.getCodeSystem(url: String): TFHIRCodeSystem;
begin
  FLock.Lock('getValueSetByUrl');
  try
    if FCodeSystemsByUrl.ContainsKey(url) then
      result := FCodeSystemsByUrl[url].CodeSystem.Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.getCodeSystemById(id: String): TFHIRCodeSystem;
begin
  FLock.Lock('getValueSetByUrl');
  try
    if FCodeSystemsById.ContainsKey(id) then
      result := FCodeSystemsById[id].CodeSystem.Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.getCodeSystemByValueSet(vs: String): TFHIRCodeSystem;
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

function TTerminologyServerStore.GetCodeSystemList: TFHIRCodeSystemList;
var
  vs : TFHIRCodeSystemEntry;
begin
  result := TFHIRCodeSystemList.Create;
  try
    FLock.Lock('GetCodeSystemList');
    try
      for vs in FCodeSystemsByUrl.values do
        result.add(vs.CodeSystem.link);
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

function TTerminologyServerStore.getConceptMapBySrcTgt(src, tgt: String): TLoadedConceptMap;
var
  lcm : TLoadedConceptMap;
begin
  result := nil;
  FLock.Lock('getValueSetByUrl');
  try
    for lcm in FConceptMapsById.Values do
      if (cmRef(lcm.Resource.source) = src) and
         (cmRef(lcm.Resource.target) = src) then
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


function TTerminologyServerStore.checkVersion(system, version : String; profile : TFHIRExpansionProfile) : String;
var
  t : TFhirExpansionProfileFixedVersion;
begin
  if (profile = nil) then
    exit(version);

  for t in profile.fixedVersionList do
    if (t.system = system) and (t.version <> '') then
    begin
      if (version = '') or (t.mode = SystemVersionProcessingModeOverride) then
        version := t.version
      else if t.mode = SystemVersionProcessingModeCheck then
        raise ETerminologyError.Create('Expansion Profile Error: the version "'+version+'" is inconsistent with the version "'+t.version+'" required by the profile');
    end;
  exit(version);
end;

Function TTerminologyServerStore.getProvider(system : String; version : String; profile : TFHIRExpansionProfile; noException : boolean = false) : TCodeSystemProvider;
begin
  result := nil;
  version := checkVersion(system, version, profile);

  if FProviderClasses.ContainsKey(system) then
  begin
    if (version <> '') then
    begin
      if FProviderClasses.ContainsKey(system+URI_VERSION_BREAK+version) then
        result := FProviderClasses[system+URI_VERSION_BREAK+version].Link
      else
      begin
        // special support for SNOMED Editions
        if (system = 'http://snomed.info/sct') and version.contains('/version/') and FProviderClasses.ContainsKey(system+URI_VERSION_BREAK+version.Substring(0, version.IndexOf('/version/'))) then
          result := FProviderClasses[system+URI_VERSION_BREAK+version.Substring(0, version.IndexOf('/version/'))].Link
        else
          result := FProviderClasses[system].Link;
        if not result.defToThisVersion(version) then
        begin
          result.Free;
          raise ETerminologySetup.create('Unable to provide support for code system '+system+' version '+version);
        end;
      end;
    end
    else
      result := FProviderClasses[system].Link
  end else if system = ANY_CODE_VS then
    result := TAllCodeSystemsProvider.create(self.link)
  else
  begin
    FLock.Lock('getProvider');
    try
      // todo; version specific....
      if FCodeSystemsByUrl.ContainsKey(system) then
        result := TFhirCodeSystemProvider.create(FCodeSystemsByUrl[system].link);
    finally
      FLock.Unlock;
    end;
  end;

  if (result <> nil) then
  begin
    FLock.Lock('getProvider');
    try
      result.RecordUse;
    finally
      FLock.Unlock;
    end;
  end;

  if (result = nil) and not noException then
    raise ETerminologySetup.create('Unable to provide support for code system '+system);
end;


function TTerminologyServerStore.getProvider(codesystem: TFHIRCodeSystem; profile : TFHIRExpansionProfile): TCodeSystemProvider;
begin
  checkVersion(codeSystem.url, codeSystem.version, profile);
  result := TFhirCodeSystemProvider.create(TFHIRCodeSystemEntry.Create(codesystem.link));
end;

procedure TTerminologyServerStore.getSummary(b: TStringBuilder);
var
  sn : TSnomedServices;
begin
  if FLoinc = nil then
    b.append('<li>LOINC: not loaded</li>')
  else
    b.append('<li>LOINC: '+FLoinc.version(nil)+' ('+inttostr(FLoinc.UseCount)+' uses)');


  if FSnomed.Count = 0 then
    b.append('<li>Snomed: not loaded</li>')
  else for sn in FSnomed do
    b.append('<li>Snomed: '+sn.version(nil)+' ('+inttostr(sn.UseCount)+' uses)');

  if FUcum = nil then
    b.append('<li>Ucum: not loaded</li>')
  else
    b.append('<li>Ucum: '+FUcum.version(nil)+' ('+inttostr(FUcum.UseCount)+' uses)');

  if FRxNorm = nil then
    b.append('<li>RxNorm: not loaded</li>')
  else
    b.append('<li>RxNorm: '+FRxNorm.version(nil)+' ('+inttostr(FRxNorm.UseCount)+' uses)');

  if FNciMeta = nil then
    b.append('<li>NciMeta: not loaded</li>')
  else
    b.append('<li>NciMeta: '+FNciMeta.version(nil)+' ('+inttostr(FNciMeta.UseCount)+' uses)');

  if FUnii = nil then
    b.append('<li>Unii: not loaded</li>')
  else
    b.append('<li>Unii: '+FUnii.version(nil)+' ('+inttostr(FUnii.UseCount)+' uses)');

  if FACIR = nil then
    b.append('<li>ACIR: not loaded</li>')
  else
    b.append('<li>ACIR: '+FACIR.version(nil)+' ('+inttostr(FACIR.UseCount)+' uses)');

  b.append('<li>ValueSets : '+inttostr(FValueSetsById.Count)+'</li>');
  b.append('<li>Code Systems : '+inttostr(FCodeSystemsByUrl.Count)+'</li>');
  b.append('<li>Concept Maps : '+inttostr(FBaseConceptMaps.Count)+'</li>');
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
    if url.StartsWith('ValueSet/') then
    begin
      if FValueSetsById.TryGetValue(url.Substring(9), result) then
        result.Link
      else
        result := nil;
    end
    else if FValueSetsByUrl.TryGetValue(url, result) then
      result.Link
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
    result := FCodeSystemsByUrl.ContainsKey(url);
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
  else if (snomed <> nil) and (uri1 = DefSnomed.system(nil)) then
    result := DefSnomed.Subsumes(code1, code2)
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


{ TFhirCodeSystemProvider }

constructor TFhirCodeSystemProvider.create(vs: TFhirCodeSystemEntry);
begin
  Create;
  FCs := vs;
  if (FCs.CodeSystem.tag <> nil) then
    Fmap := TCodeSystemAdornment(FCs.CodeSystem.Tag).FMap;
end;

function TFhirCodeSystemProvider.Definition(context: TCodeSystemProviderContext): string;
begin
  result := TFhirCodeSystemProviderContext(context).context.definition;
end;

destructor TFhirCodeSystemProvider.destroy;
begin
  FCs.free;
  inherited;
end;

function TFhirCodeSystemProvider.ChildCount(context: TCodeSystemProviderContext): integer;
var
  ex : TFhirExtension;
begin
  if context = nil then
    result := FCs.CodeSystem.codeSystem.conceptList.count
  else
  begin
    result := TFhirCodeSystemProviderContext(context).context.conceptList.count;
    for ex in TFhirCodeSystemProviderContext(context).context.modifierExtensionList do
      if ex.url = 'http://hl7.org/fhir/StructureDefinition/codesystem-subsumes' then
        inc(result);
  end;
end;

procedure TFhirCodeSystemProvider.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.Free;
end;

function TFhirCodeSystemProvider.Code(context: TCodeSystemProviderContext): string;
begin
  result := TFhirCodeSystemProviderContext(context).context.code;
end;

function markdownNoPara(s : String) : String;
begin
  result := s.Replace('#13#10', '  '+#10);
end;

function spacer(s : String) : String;
begin
  if s = '' then
    result := s
  else
    result := s+' ';
end;

procedure TFhirCodeSystemProvider.getCDSInfo(card: TCDSHookCard; slang, baseURL, code, display: String);
var
  b : TStringBuilder;
  ctxt : TFhirCodeSystemProviderContext;
  concept, c : TFhirCodeSystemConcept;
  d : TFhirCodeSystemConceptDesignation;
  codes : TFhirCodeSystemConceptList;
begin
  b := TStringBuilder.Create;
  try
    ctxt := TFhirCodeSystemProviderContext(locate(code));
    if ctxt = nil Then
      b.Append('Error: Code '+code+' not known')
    else
    try
      card.addLink('Further Detail', baseURL+'/tx/valuesets/'+FCs.CodeSystem.id+'#'+code);
      concept := ctxt.context;

      b.Append(FCS.CodeSystem.name+' Code '+code+' : '+concept.display+#13#10#13#10);
      b.Append('* Definition: '+markdownNoPara(concept.definition)+#13#10);
      b.Append('* System Definition: '+markdownNoPara(FCs.CodeSystem.description)+#13#10);
      b.Append(#13#10);

      if concept.designationList.Count > 0 then
      begin
        b.Append('Designations: '+#13#10#13#10);
        for d in concept.designationList do
          b.Append('* '+spacer(d.language)+spacer(gen(d.use))+d.value+#13#10);
        b.Append(#13#10);
      end;

      codes := FCS.CodeSystem.codeSystem.getParents(concept);
      try
        if codes.count > 0 then
        begin
          b.Append('Parents: '+#13#10#13#10);
          for c in codes do
            b.Append('* '+c.code+': '+c.display+#13#10);
          b.Append(#13#10);
        end;
      finally
        codes.Free;
      end;

      codes := FCS.CodeSystem.codeSystem.getChildren(concept);
      try
        if codes.count > 0 then
        begin
          b.Append('Childrem: '+#13#10#13#10);
          for c in codes do
            b.Append('* '+c.code+': '+c.display+#13#10);
          b.Append(#13#10);
        end;
      finally
        codes.Free;
      end;

    finally
      close(ctxt);
    End;
    if FCs.CodeSystem.copyright <> '' then
      b.Append(FCs.CodeSystem.copyright+#13#10);
    card.detail := b.ToString;
  finally
    b.Free;
  end;end;

function TFhirCodeSystemProvider.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
var
  ex : TFhirExtension;
  code : String;
begin
  result := nil;
  if context = nil then
    result := TFhirCodeSystemProviderContext.create(FCs.CodeSystem.codeSystem.conceptList[ndx])
  else
  begin
    if (ndx < TFhirCodeSystemProviderContext(context).context.conceptList.Count) then
      result := TFhirCodeSystemProviderContext.create(TFhirCodeSystemProviderContext(context).context.conceptList[ndx])
    else
    begin
      ndx := ndx - TFhirCodeSystemProviderContext(context).context.conceptList.count;
      for ex in TFhirCodeSystemProviderContext(context).context.modifierExtensionList do
        if (ndx = 0) then
        begin
          code := TFHIRCode(ex.value).value;
          exit(doLocate(code));
        end
        else if ex.url = 'http://hl7.org/fhir/StructureDefinition/codesystem-subsumes' then
          dec(ndx);
    end;
  end;
end;

function TFhirCodeSystemProvider.Display(context: TCodeSystemProviderContext; lang : String): string;
var
  ccd : TFhirCodeSystemConceptDesignation;
  css : TFHIRCodeSystem;
  cc : TFhirCodeSystemConcept;
begin
  result := TFhirCodeSystemProviderContext(context).context.display;
  if (lang <> '') then
    for ccd in TFhirCodeSystemProviderContext(context).context.designationList do
      if languageMatches(lang, ccd.language) then
        result := ccd.value;
  for css in FCs.Supplements do
  begin
    cc := locCode(css.conceptList, TFhirCodeSystemProviderContext(context).context.code);
    if (cc <> nil) then
    begin
      if languageMatches(lang, css.language) then
        result := cc.display;
      for ccd in cc.designationList do
        if languageMatches(lang, ccd.language) then
          result := ccd.value;
    end;
  end;
end;

procedure TFhirCodeSystemProvider.Displays(context: TCodeSystemProviderContext; list: TStringList; lang : String);
begin
  list.Add(Display(context, lang));
end;

procedure TFhirCodeSystemProvider.Displays(code: String; list: TStringList; lang : String);
var
  ccd : TFhirCodeSystemConceptDesignation;
  cc : TFhirCodeSystemConcept;
  ctxt : TCodeSystemProviderContext;
  css : TFhirCodeSystem;
begin
  ctxt := locate(code);
  try
    list.Add(getDisplay(code, lang));
    for ccd in TFhirCodeSystemProviderContext(ctxt).context.designationList do
      if (lang = '') or languageMatches(lang, ccd.language) then
        list.add(ccd.value);
    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, code);
      if (cc <> nil) then
      begin
        if languageMatches(lang, css.language) then
          list.add(cc.display);
        for ccd in cc.designationList do
          if languageMatches(lang, ccd.language) then
            list.add(ccd.value);
      end;
    end;
  finally
    Close(ctxt);
  end;
end;

function TFhirCodeSystemProvider.InFilter(ctxt: TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext): Boolean;
var
  cm : TFhirCodeSystemConceptMatch;
  c : TFhirCodeSystemConcept;
begin
  result := false;
  c := TFhirCodeSystemProviderContext(concept).context;
  for cm in TFhirCodeSystemProviderFilterContext(ctxt).concepts do
    if cm.FItem = c then
    begin
      result := true;
      exit;
    end;
end;

function TFhirCodeSystemProvider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  {$IFNDEF FHIR2}
  result := FCs.CodeSystem.isAbstract(TFhirCodeSystemProviderContext(context).context);
  {$ELSE}
  result := (TFhirCodeSystemProviderContext(context).context.abstractElement <> nil) and TFhirCodeSystemProviderContext(context).context.abstract;
  {$ENDIF}
end;

function TFhirCodeSystemProvider.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TFhirCodeSystemProvider.getDefinition(code: String): String;
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

function TFhirCodeSystemProvider.getDisplay(code: String; lang : String): String;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(code);
  try
    if (ctxt = nil) then
      raise Exception.create('Unable to find '+code+' in '+system(nil))
    else
      result := Display(ctxt, lang);
  finally
    Close(ctxt);
  end;
end;

function TFhirCodeSystemProvider.getParent(ctxt: TFhirCodeSystemConcept): TFhirCodeSystemConcept;
  function getMyParent(list: TFhirCodeSystemConceptList): TFhirCodeSystemConcept;
  var
    c, t : TFhirCodeSystemConcept;
  begin
    for c in list do
    begin
      if c.conceptList.ExistsByReference(ctxt) then
        exit(c);
      t := getMyParent(c.conceptList);
      if (t <> nil) then
        exit(t);
    end;
    exit(nil);
  end;
begin
  if (FMap <> nil) then
    result := TConceptAdornment(ctxt.Tag).FParent
  else
    result := getMyParent(FCs.CodeSystem.conceptList)
end;

{$IFNDEF FHIR2}
function TFhirCodeSystemProvider.getProperty(code: String): TFhirCodeSystemProperty;
var
  p : TFhirCodeSystemProperty;
begin
  result := nil;
  for p in FCs.CodeSystem.property_List do
    if (p.code = code) then
      exit(p);
end;
{$ENDIF}

function TFhirCodeSystemProvider.locCode(list : TFhirCodeSystemConceptList; code : String) : TFhirCodeSystemConcept;
var
  c : TFhirCodeSystemConcept;
begin
  result := nil;
  for c in list do
  begin
    if (c.code = code) then
      exit(c);
    result := locCode(c.conceptList, code);
    if result <> nil then
      exit;
  end;
end;

function TFhirCodeSystemProvider.LocateCode(code : String) : TFhirCodeSystemConcept;
begin
  if (FMap = nil) then
    result := locCode(FCs.CodeSystem.conceptList, code)
  else if (FMap.ContainsKey(code)) then
    result := FMap[code]
  else
    result := locCode(FCs.CodeSystem.conceptList, code);
end;

function TFhirCodeSystemProvider.doLocate(list : TFhirCodeSystemConceptList; code : String) : TFhirCodeSystemProviderContext;
var
  c : TFhirCodeSystemConcept;
begin
  result := nil;
  for c in list do
  begin
    if (c.code = code) then
      exit(TFhirCodeSystemProviderContext.Create(c.Link));
    result := doLocate(c.conceptList, code);
    if result <> nil then
      exit;
  end;
end;

function TFhirCodeSystemProvider.doLocate(code : String) : TFhirCodeSystemProviderContext;
var
  c : TFhirCodeSystemConcept;
begin
  c := LocateCode(code);
  if (c = nil) then
    result := nil
  else
    result := TFhirCodeSystemProviderContext.Create(c.Link);
end;

procedure TFhirCodeSystemProvider.extendLookup(ctxt: TCodeSystemProviderContext; lang : String; props: TList<String>; resp: TFHIRLookupOpResponse);
{$IFNDEF FHIR2}
var
  concepts : TAdvList<TFhirCodeSystemConcept>;
  cc, context : TFhirCodeSystemConcept;
  parent, child : TFhirCodeSystemConcept;
  ccd : TFhirCodeSystemConceptDesignation;
  cp : TFhirCodeSystemConceptProperty;
  pp : TFhirCodeSystemProperty;
  d : TFHIRLookupOpRespDesignation;
  p : TFHIRLookupOpRespProperty_;
  css : TFHIRCodeSystem;
  {$ENDIF}
begin
  {$IFNDEF FHIR2}
  context := TFHIRCodeSystemProviderContext(ctxt).context;
  concepts := TAdvList<TFhirCodeSystemConcept>.create;
  try
    concepts.Add(context.Link);
    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, context.code);
      if (cc <> nil) then
      begin
        concepts.Add(cc.Link);
        if (css.language <> '') and (cc.displayElement <> nil) then
          cc.displayElement.Tags['lang'] := css.language;
      end;
    end;

    if hasProp(props, 'parent', true) then
    begin
      parent := getParent(context);
      if (parent <> nil) then
      begin
        p := TFHIRLookupOpRespProperty_.create;
        resp.property_List.Add(p);
        p.code := 'parent';
        p.value := parent.code;
        p.description := parent.display;
      end;
    end;

    if hasProp(props, 'child', true) then
    begin
      for child in context.conceptList do
      begin
        p := TFHIRLookupOpRespProperty_.create;
        resp.property_List.Add(p);
        p.code := 'child';
        p.value := child.code;
        p.description := child.display;
      end;
    end;

    if hasProp(props, 'designation', true) then
      for cc in concepts do
      begin
        if (cc.display <> '') and (cc.display <> context.display) and (cc.displayElement.Tags['lang'] <> '') then
        Begin
          d := TFHIRLookupOpRespDesignation.create;
          resp.designationList.Add(d);
          d.value := cc.display;
          d.language := cc.displayElement.Tags['lang']
        End;

        for ccd in cc.designationList do
        Begin
          d := TFHIRLookupOpRespDesignation.create;
          resp.designationList.Add(d);
          d.value := ccd.value;
          d.use := ccd.use.link;
          d.language := ccd.language
        End;
      end;


    for cc in concepts do
      for cp in cc.property_List do
      begin
        pp := getProperty(cp.code);
        if hasProp(props, cp.code, true) then
        begin
          p := TFHIRLookupOpRespProperty_.create;
          resp.property_List.Add(p);
          p.code := cp.code;
          p.value := cp.value.primitiveValue;
        end;
      end;
  finally
    concepts.Free;
  end;
  {$ENDIF}
end;

function TFhirCodeSystemProvider.locate(code: String; var message : String): TCodeSystemProviderContext;
begin
  result := DoLocate(code);
end;

function TFhirCodeSystemProvider.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean): TCodeSystemProviderFilterContext;
var
  res : TFhirCodeSystemProviderFilterContext;
begin
  res := TFhirCodeSystemProviderFilterContext.Create;
  try
    FilterCodes(res, FCs.CodeSystem.codeSystem.conceptList, filter);
    res.sort;
    result := res.Link;
  finally
    res.Free;
  end;
end;

function TFhirCodeSystemProvider.subsumesTest(codeA, codeB: String): String;
var
  t, cA, cB : TFhirCodeSystemConcept;
begin
  cA := LocateCode(codeA);
  if (cA = nil) then
    raise Exception.Create('Unknown Code "'+codeA+'"');
  cB := LocateCode(codeB);
  if (cB = nil) then
    raise Exception.Create('Unknown Code "'+codeB+'"');

  t := CB;
  while t <> nil do
  begin
    if (t = cA) then
      exit('subsumes');
    t := getParent(t);
  end;

  t := CA;
  while t <> nil do
  begin
    if (t = cB) then
      exit('subsumed-by');
    t := getParent(t);
  end;
  exit('not-subsumed');
end;

function TFhirCodeSystemProvider.system(context : TCodeSystemProviderContext): String;
begin
  result := FCs.CodeSystem.codeSystem.system;
end;

function TFhirCodeSystemProvider.TotalCount: integer;
function count(item : TFhirCodeSystemConcept) : integer;
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
  for i := 0 to FCs.CodeSystem.codeSystem.conceptList.count - 1 do
    inc(result, count(FCs.CodeSystem.codeSystem.conceptList[i]));
end;

function TFhirCodeSystemProvider.version(context: TCodeSystemProviderContext): String;
begin
   result := FCs.CodeSystem.version;
end;

procedure TFhirCodeSystemProvider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  ctxt.Free;
end;

procedure TFhirCodeSystemProvider.iterateCodes(base : TFhirCodeSystemConcept; list : TFhirCodeSystemProviderFilterContext);
var
  i : integer;
  ex: TFhirExtension;
  ctxt : TCodeSystemProviderContext;
begin
  base.checkNoModifiers('CodeSystemProvider.iterateCodes', 'code', ['http://hl7.org/fhir/StructureDefinition/codesystem-subsumes']);
  list.Add(base.Link, 0);
  for i := 0 to base.conceptList.count - 1 do
    iterateCodes(base.conceptList[i], list);
  for ex in base.modifierExtensionList do
    if ex.url = 'http://hl7.org/fhir/StructureDefinition/codesystem-subsumes' then
    begin
     ctxt := doLocate(TFHIRCode(ex.value).value);
     try
       iterateCodes(TFhirCodeSystemProviderContext(ctxt).context, list);
     finally
       Close(ctxt);
     end;
    end;
end;

function TFhirCodeSystemProvider.filter(prop: String; op: TFhirFilterOperatorEnum; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  code : TFhirCodeSystemProviderContext;
  ts : TStringList;
  i: Integer;
begin
  if (op = FilterOperatorIsA) and (prop = 'concept') then
  begin
    code := doLocate(value);
    try
      if code = nil then
        raise ETerminologyError.Create('Unable to locate code '+value)
      else
      begin
        result := TFhirCodeSystemProviderFilterContext.create;
        try
          iterateCodes(code.context, result as TFhirCodeSystemProviderFilterContext);
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
    result := TFhirCodeSystemProviderFilterContext.Create;
    try
      ts := TStringList.Create;
      try
        ts.CommaText := value;
        for i := 0 to ts.Count - 1 do
        begin
          code := doLocate(value);
          try
            if code = nil then
              raise ETerminologyError.Create('Unable to locate code '+value)
            else
              TFhirCodeSystemProviderFilterContext(result).Add(code.context.Link, 0);
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

procedure TFhirCodeSystemProvider.FilterCodes(dest : TFhirCodeSystemProviderFilterContext; source: TFhirCodeSystemConceptList; filter : TSearchFilterText);
var
  i : integer;
  code : TFhirCodeSystemConcept;
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

function TFhirCodeSystemProvider.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  inc(TFhirCodeSystemProviderFilterContext(ctxt).ndx);
  result := TFhirCodeSystemProviderFilterContext(ctxt).ndx < TFhirCodeSystemProviderFilterContext(ctxt).concepts.Count;
end;

function TFhirCodeSystemProvider.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  context : TFhirCodeSystemProviderFilterContext;
begin
  context := TFhirCodeSystemProviderFilterContext(ctxt);
  result := TFhirCodeSystemProviderContext.Create(context.concepts[context.ndx].FItem.Link)
end;

function TFhirCodeSystemProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message : String): TCodeSystemProviderContext;
var
  context : TFhirCodeSystemProviderFilterContext;
  i : integer;
begin
  result := nil;
  context := TFhirCodeSystemProviderFilterContext(ctxt);
  for i := 0 to context.concepts.Count - 1 do
    if context.concepts[i].FItem.code = code  then
    begin
      result := TFhirCodeSystemProviderContext.Create(context.concepts[i].FItem.Link);
      break;
    end;
end;


function TFhirCodeSystemProvider.locateIsA(code, parent: String): TCodeSystemProviderContext;
var
  p : TFhirCodeSystemProviderContext;
begin
  result := nil;
  p := Locate(parent) as TFhirCodeSystemProviderContext;
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

function TFhirCodeSystemProvider.name(context: TCodeSystemProviderContext): String;
begin
   result := FCs.CodeSystem.name;
end;

{ TLoadedConceptMap }

destructor TLoadedConceptMap.Destroy;
begin
  FResource.Free;
  FSource.Free;
  FTarget.Free;
  inherited;
end;

function TLoadedConceptMap.HasTranslation(system, code : String; out maps : TFhirConceptMapGroupElementTargetList): boolean;
begin
  result := HasTranslation(Resource.groupList, system, code, maps);
end;

function TLoadedConceptMap.Link: TLoadedConceptMap;
begin
  result := TLoadedConceptMap(inherited Link);
end;

function TLoadedConceptMap.HasTranslation(list : TFhirConceptMapGroupList; system, code : String; out maps : TFhirConceptMapGroupElementTargetList): boolean;
var
  g : TFhirConceptMapGroup;
  c : TFhirConceptMapGroupElement;
begin
  result := false;
  {$IFDEF FHIR2}
    for c in g.elementList do
    begin
      if (c.system = system) and (c.code = code) then
  {$ELSE}
  for g in list do
    for c in g.elementList do
    begin
      if (g.source = system) and (c.code = code) then
  {$ENDIF}
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

{ TFhirCodeSystemProviderFilterContext }

procedure TFhirCodeSystemProviderFilterContext.Add(item: TFhirCodeSystemConcept; rating : double);
begin
  concepts.Add(TFhirCodeSystemConceptMatch.Create(item, rating));
end;

function TFhirCodeSystemProviderFilterContext.Compare(const Left, Right: TFhirCodeSystemConceptMatch): Integer;
begin
  if right.FRating > left.FRating then
    result := 1
  else if left.FRating > right.FRating then
    result := -1
  else
    result := 0;
end;

constructor TFhirCodeSystemProviderFilterContext.Create;
begin
  inherited Create;
  self.concepts := TAdvList<TFhirCodeSystemConceptMatch>.create;
  ndx := -1;
end;

destructor TFhirCodeSystemProviderFilterContext.Destroy;
begin
  concepts.Free;
  inherited;
end;

procedure TFhirCodeSystemProviderFilterContext.sort;
var
  m : TFhirCodeSystemConceptMatch;
begin
  concepts.sort(self);
  for m in concepts do
    writeln(m.FItem.code+' ('+m.FItem.display+'): '+FloatToStr(m.FRating));
end;

{ TFhirCodeSystemProviderContext }

constructor TFhirCodeSystemProviderContext.Create(context: TFhirCodeSystemConcept);
begin
  inherited create;
  self.context := context;
end;

destructor TFhirCodeSystemProviderContext.Destroy;
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

{ TFhirCodeSystemConceptMatch }

constructor TFhirCodeSystemConceptMatch.Create(item: TFhirCodeSystemConcept; rating: double);
begin
  inherited Create;
  FItem := item;
  FRating := rating;
end;

destructor TFhirCodeSystemConceptMatch.Destroy;
begin
  FItem.Free;
  inherited;
end;

{ TCodeSystemAdornment }

constructor TCodeSystemAdornment.Create(map: TAdvMap<TFhirCodeSystemConcept>);
begin
  inherited Create;
  FMap := map;
end;

destructor TCodeSystemAdornment.destroy;
begin
  FMap.Free;
  inherited;
end;

{ TFHIRCodeSystemEntry }

constructor TFHIRCodeSystemEntry.Create({$IFNDEF FHIR2} cs : TFhirCodeSystem {$ELSE} cs : TFHIRValueSet {$ENDIF});
begin
  inherited Create;
  FCodeSystem := cs;
end;

destructor TFHIRCodeSystemEntry.Destroy;
begin
  FCodeSystem.Free;
  FSupplements.Free;
  {$IFDEF FHIR2};
  FValueset.Free;
  {$ENDIF}
  inherited;
end;

function TFHIRCodeSystemEntry.GetHasSupplements: boolean;
begin
  result := (FSupplements <> nil) and (FSupplements.Count > 0);
end;

function TFHIRCodeSystemEntry.GetSupplements: TAdvList<TFHIRCodeSystem>;
begin
  if FSupplements = nil then
    FSupplements := TAdvList<TFHIRCodeSystem>.create;
  result := FSupplements;
end;

function TFHIRCodeSystemEntry.Link: TFHIRCodeSystemEntry;
begin
  result := TFHIRCodeSystemEntry(inherited Link);
end;

procedure TFHIRCodeSystemEntry.SetCodeSystem(const Value: TFHIRCodeSystem);
begin
  FCodeSystem.Free;
  FCodeSystem := Value;
end;

end.
