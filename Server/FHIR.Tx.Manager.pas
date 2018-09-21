unit FHIR.Tx.Manager;

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
  SysUtils, Classes, FHIR.Support.Threads, Generics.Defaults, Generics.Collections,
  FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Base, FHIR.Support.Collections,
  FHIR.Database.Manager,
  FHIR.Base.Objects, FHIR.Base.Lang, FHIR.Base.Common, FHIR.CdsHooks.Utilities, FHIR.Base.Factory,
  FHIR.Tools.CodeSystemProvider, FHIR.Tools.ValueSets,
  FHIR.Tx.Service, FHIR.Loinc.Services, FHIR.Ucum.Services, FHIR.Snomed.Services, FHIR.Tx.RxNorm, FHIR.Tx.Unii, FHIR.Tx.ACIR,
  FHIR.Tx.Uri, FHIR.Tx.ICD10, FHIR.Tx.AreaCode, FHIR.Tx.CountryCode, FHIR.Tx.UsState, FHIR.Tx.Iso4217,
  FHIR.Tx.MimeTypes, FHIR.Tx.Lang, FHIR.Support.Logging,
  FHIR.Server.Utilities, FHIR.Server.Ini,
  YuStemmer;

const
  URI_VERSION_BREAK = '#';

Type
  TCommonTerminologies = class (TFslObject)
  private
    FSettings : TFHIRServerSettings;
    FLoinc : TLOINCServices;
    FSnomed : TFslList<TSnomedServices>;
    FIcd10 : TFslList<TICD10Provider>;
    FDefSnomed : TSnomedServices;
    FUcum : TUcumServices;
    FRxNorm : TRxNormServices;
    FNciMeta : TNciMetaServices;
    FUnii : TUniiServices;
    FACIR : TACIRServices;
    FProviderClasses : TFslMap<TCodeSystemProvider>;
    FNDFRT: TNDFRTServices;
    procedure SetLoinc(const Value: TLOINCServices);
    procedure SetDefSnomed(const Value: TSnomedServices);
    procedure SetUcum(const Value: TUcumServices);
    procedure SetRxNorm(const Value: TRxNormServices);
    procedure SetNciMeta(const Value: TNciMetaServices);
    procedure SetUnii(const Value: TUniiServices);
    procedure SetACIR(const Value: TACIRServices);

    procedure getSummary(b : TStringBuilder);

    procedure SetNDFRT(const Value: TNDFRTServices);
  public
    constructor Create(settings : TFHIRServerSettings);
    destructor Destroy; Override;
    function link: TCommonTerminologies; overload;

    procedure add(p : TCodeSystemProvider);
    Property ProviderClasses : TFslMap<TCodeSystemProvider> read FProviderClasses;
    property Settings : TFHIRServerSettings read FSettings;

    // load external terminology resources (snomed, Loinc, etc)
    procedure load(ini : TFHIRServerIniFile; databases : TFslMap<TKDBManager>; testing : boolean);

    Property Loinc : TLOINCServices read FLoinc write SetLoinc;
    Property Snomed : TFslList<TSnomedServices> read FSnomed;
    Property Icd10 : TFslList<TICD10Provider> read FIcd10;
    Property DefSnomed : TSnomedServices read FDefSnomed write SetDefSnomed;
    Property Ucum : TUcumServices read FUcum write SetUcum;
    Property RxNorm : TRxNormServices read FRxNorm write SetRxNorm;
    Property NDFRT : TNDFRTServices read FNDFRT write SetNDFRT;
    Property NciMeta : TNciMetaServices read FNciMeta write SetNciMeta;
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
  TTerminologyServerStore = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FStem : TYuStemmer_8;
    FTagid : integer;
    FCommonTerminologies : TCommonTerminologies;

    FLastConceptKey : integer;
    FLastClosureKey : integer;
    FLastClosureEntryKey : integer;
    FLastValueSetKey : integer;
    FLastValueSetMemberKey : integer;

    // value sets are indexed 3 ways:
    // by their local url
    // by their canonical url
    // if they're a value set, by their code system url
    FValueSetsById : TFslMap<TFHIRValueSetW>; // by local system's id
    FValueSetsByURL : TFslMap<TFHIRValueSetW>; // by canonical url
    FCodeSystemsById : TFslMap<TFHIRCodeSystemEntry>; // all current value sets that define systems, by their url
    FCodeSystemsByUrl : TFslMap<TFHIRCodeSystemEntry>; // all current value sets that define systems, by their url
    FCodeSystemsByVsUrl : TFslMap<TFHIRCodeSystemEntry>; // all current value sets that define systems, by their url
    FSupplementsById : TFslMap<TFHIRCodeSystemW>; // All supplements

    FBaseValueSets : TFslMap<TFHIRValueSetW>; // value sets out of the specification - these can be overriden, but they never go away
    FBaseCodeSystems : TFslMap<TFHIRCodeSystemEntry>; // value sets out of the specification - these can be overriden, but they never go away

    FBaseConceptMaps : TFslMap<TLoadedConceptMap>; // value sets out of the specification - these can be overriden, but they never go away
    FConceptMapsById : TFslMap<TLoadedConceptMap>;
    FConceptMapsByURL : TFslMap<TLoadedConceptMap>;

    procedure UpdateConceptMaps;
    procedure BuildStems(cs : TFhirCodeSystemW);


    procedure checkForDuplicates(codes: TStringList; list: TFslList<TFhirCodeSystemConceptW>; url : String);
    function checkVersion(system, version: String; profile: TFHIRExpansionParams): String;
    procedure AddCodeSystemToCache(cs : TFhirCodeSystemW; base : boolean);
    procedure RemoveCodeSystemFromCache(id : String);
    function getProviderClasses: TFslMap<TCodeSystemProvider>;
  protected
    FLock : TFslLock;  // it would be possible to use a read/write lock, but the complexity doesn't seem to be justified by the short amount of time in the lock anyway
    FDB : TKDBManager;
    procedure invalidateVS(id : String); virtual;
    procedure getSummary(b : TStringBuilder);
  public
    constructor Create(db : TKDBManager; factory : TFHIRFactory; common : TCommonTerminologies); virtual;
    destructor Destroy; Override;
    Function Link : TTerminologyServerStore; overload;

    Property Factory : TFHIRFactory read FFactory;
    Property DB : TKDBManager read FDB;
    property CommonTerminologies : TCommonTerminologies read FCommonTerminologies;

    // maintenance procedures
    procedure SeeSpecificationResource(resource : TFHIRResourceV);
    procedure SeeTerminologyResource(resource : TFHIRResourceV);
    procedure checkTerminologyResource(resource : TFHIRResourceV);
    procedure DropTerminologyResource(aType : String; id : String);

    // access procedures. All return values are owned, and must be freed
    Function getProvider(system : String; version : String; profile : TFHIRExpansionParams; noException : boolean = false) : TCodeSystemProvider; overload;
    Function getProvider(codesystem : TFHIRCodeSystemW; profile : TFHIRExpansionParams) : TCodeSystemProvider; overload;
    function getValueSetByUrl(url : String) : TFHIRValueSetW;
    function getValueSetById(id : String) : TFHIRValueSetW;
    function getCodeSystemById(id : String) : TFHIRCodeSystemW;
    function getCodeSystemByValueSet(vs : String) : TFHIRCodeSystemW;
    function getCodeSystem(url : String) : TFHIRCodeSystemW;
    function hasCodeSystem(url : String) : Boolean;
    function getConceptMapById(id : String) : TLoadedConceptMap;
    function getConceptMapBySrcTgt(src, tgt : String) : TLoadedConceptMap;

    // publishing access
    procedure GetCodeSystemList(list : TFslList<TFHIRCodeSystemW>); overload;
    procedure GetCodeSystemList(list : TFslList<TFHIRMetadataResourceW>); overload;
    procedure GetValueSetList(list : TFslList<TFhirValueSetW>); overload;
    procedure GetValueSetList(list : TFslList<TFHIRMetadataResourceW>); overload;
    function GetConceptMapList : TLoadedConceptMapList;
    Property ProviderClasses : TFslMap<TCodeSystemProvider> read getProviderClasses;
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
    FStore: TCommonTerminologies;
    FActCode : TCodeSystemProvider;
  public
    constructor Create(store : TCommonTerminologies; actCode : TCodeSystemProvider);
    destructor Destroy; override;
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
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
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
    raise ETerminologyError.create('Not Created Yet');
end;

function TAllCodeSystemsProvider.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('Not Created Yet');
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
  raise ETerminologyError.create('Not Created Yet');
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
  raise ETerminologyError.create('Not Created Yet');
end;
function TAllCodeSystemsProvider.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('Not Created Yet');
end;
function TAllCodeSystemsProvider.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('Not Created Yet');
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

constructor TAllCodeSystemsProvider.Create(store: TCommonTerminologies; actCode : TCodeSystemProvider);
begin
  inherited Create;
  FStore := store;
  FActCode := actCode;
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
  raise ETerminologyError.create('Not Created Yet');
end;
procedure TAllCodeSystemsProvider.Displays(code : String; list : TStringList; lang : String);
begin
  raise ETerminologyError.create('Not Created Yet');
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

function TAllCodeSystemsProvider.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('Not Created Yet');
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
  raise ETerminologyError.create('Not Created Yet');
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
  raise ETerminologyError.create('Not Created Yet');
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
        result.Add(lowercase(FStem.calc(s)));
    end;
    result.SortAscending;
  end;
  procedure processConcepts(parent : TFhirCodeSystemConceptW; list : TFslList<TFhirCodeSystemConceptW>; map : TFslMap<TFhirCodeSystemConceptW>);
  var
    c : TFhirCodeSystemConceptW;
  begin
    for c in list do
    begin
      stems(c).parent := parent;
      if map.ContainsKey(c.code) then
        logt('Duplicate code '+c.code+' in '+cs.url)
      else
        map.Add(c.code, c.Link);
      processConcepts(c, c.conceptList, map);
    end;
  end;
var
  map : TFslMap<TFhirCodeSystemConceptW>;
begin
  map := TFslMap<TFhirCodeSystemConceptW>.create;
  try
    cs.Tag := TCodeSystemAdornment.create(map.link);
    processConcepts(nil, cs.conceptList, map);
  finally
    map.Free;
  end;
end;


procedure TTerminologyServerStore.checkForDuplicates(codes : TStringList; list : TFslList<TFhirCodeSystemConceptW>; url : String);
var
  cc : TFhirCodeSystemConceptW;
  i : integer;
begin
  for cc in list do
  begin
    if cc.code <> '' then
    begin
      if codes.Find(cc.code, i) then
        raise ETerminologyError.create('Duplicate code: '+cc.code+' in value set '+url);
      codes.Add(cc.code);
    end;
    checkForDuplicates(codes, cc.conceptList, url);
  end;
end;

procedure TTerminologyServerStore.checkTerminologyResource(resource: TFHIRResourceV);
begin
  resource.checkNoImplicitRules('Repository.SeeResource', 'Resource');
  FFactory.checkNoModifiers(resource, 'Repository.SeeResource', 'Resource');
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
    result := FCodeSystemsById.Count;
  finally
    FLock.Unlock;
  end;
end;

constructor TTerminologyServerStore.Create(db : TKDBManager; factory : TFHIRFactory; common : TCommonTerminologies);
var
  conn : TKDBConnection;
begin
  inherited Create;
  FFactory := factory;
  FLock := TFslLock.Create('Terminology Server Store');
  FCommonTerminologies := common;

  FDB := db;

  FValueSetsById := TFslMap<TFhirValueSetW>.create;
  FValueSetsByURL := TFslMap<TFhirValueSetW>.create;
  FCodeSystemsById := TFslMap<TFhirCodeSystemEntry>.create;
  FCodeSystemsByUrl := TFslMap<TFhirCodeSystemEntry>.create;
  FCodeSystemsByVsUrl := TFslMap<TFhirCodeSystemEntry>.create;
  FBaseValueSets := TFslMap<TFhirValueSetW>.create;
  FBaseCodeSystems := TFslMap<TFHIRCodeSystemEntry>.create;
  FSupplementsById := TFslMap<TFhirCodeSystemW>.create;


  FBaseConceptMaps := TFslMap<TLoadedConceptMap>.create;
  FConceptMapsById := TFslMap<TLoadedConceptMap>.create;
  FConceptMapsByURL := TFslMap<TLoadedConceptMap>.create;

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

procedure TTerminologyServerStore.declareCodeSystems(list : TFslList<TFhirResourceV>);
  procedure addCodeSystem(name, id, uri, version : String; count : integer);
  var
    cs : TFhirCodeSystemW;
    r : TFHIRResourceV;
  begin
    r := FFactory.makeResource('CodeSystem');
    try
      cs := FFactory.wrapCodeSystem(r.link);
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
  icd : TICD10Provider;
begin
  if FCommonTerminologies.FLoinc <> nil then
    addCodeSystem('LOINC', 'loinc', FCommonTerminologies.FLoinc.system(nil), FCommonTerminologies.FLoinc.version(nil), FCommonTerminologies.FLoinc.TotalCount);
  for sn in FCommonTerminologies.FSnomed do
    addCodeSystem('SNOMED CT', 'sct', sn.system(nil), sn.version(nil), sn.TotalCount);
  for icd in FCommonTerminologies.FIcd10 do
    addCodeSystem(icd.title, tail(icd.system(nil)), icd.system(nil), icd.version(nil), icd.TotalCount);
  if FCommonTerminologies.FUcum <> nil then
    addCodeSystem('Ucum', 'ucum', FCommonTerminologies.FUcum.system(nil), FCommonTerminologies.FUcum.version(nil), FCommonTerminologies.FUcum.TotalCount);
  if FCommonTerminologies.FRxNorm <> nil then
    addCodeSystem('RxNorm', 'rxnorm', FCommonTerminologies.FRxNorm.system(nil), FCommonTerminologies.FRxNorm.version(nil), FCommonTerminologies.FRxNorm.TotalCount);
  if FCommonTerminologies.NDFRT <> nil then
    addCodeSystem('NDFRT', 'ndfrt', FCommonTerminologies.NDFRT.system(nil), FCommonTerminologies.NDFRT.version(nil), FCommonTerminologies.NDFRT.TotalCount);
  if FCommonTerminologies.FUnii <> nil then
    addCodeSystem('Unii', 'unii', FCommonTerminologies.FUnii.system(nil), FCommonTerminologies.FUnii.version(nil), FCommonTerminologies.FUnii.TotalCount);
  if FCommonTerminologies.FACIR <> nil then
    addCodeSystem('ACIR', 'acir', FCommonTerminologies.FACIR.system(nil), FCommonTerminologies.FACIR.version(nil), FCommonTerminologies.FACIR.TotalCount);
end;


destructor TTerminologyServerStore.Destroy;
begin
  FCommonTerminologies.Free;
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
  if (cs.url <> codeA.system) then
    raise ETerminologyError.create('System uri / code uri mismatch - not supported at this time ('+cs.url+'/'+codeA.system+')');
  if (cs.url <> codeB.system) then
    raise ETerminologyError.create('System uri / code uri mismatch - not supported at this time ('+cs.url+'/'+codeB.system+')');
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
        if FCodeSystemsById.TryGetValue(cs.supplements.Substring(11), ct) then
          ct.Supplements.Add(cs.Link);
      end
      else if FCodeSystemsByUrl.TryGetValue(cs.supplements, ct) then
        ct.Supplements.Add(cs.Link);
    end
    else
    begin
      FCodeSystemsById.AddOrSetValue(cs.id, cse.Link);
      FCodeSystemsByUrl.AddOrSetValue(cs.url, cse.Link);
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
  if (FCodeSystemsById.TryGetValue(id, cse)) then
  begin
    cs1 := FBaseCodeSystems[cse.CodeSystem.url].Link;
    try
      FCodeSystemsByUrl.Remove(cse.codeSystem.url);
      FCodeSystemsByid.Remove(cse.codeSystem.id);
      if cs1 <> nil then
        AddCodeSystemToCache(cs1.codeSystem, false);
    finally
      cs1.free;
    end;
  end
  else if FSupplementsById.TryGetValue(id, cs) then
  begin
    cs1 := FBaseCodeSystems[cs.url].Link;
    try
      if cs.supplements.StartsWith('CodeSystem/') then
      begin
        if FCodeSystemsById.TryGetValue(cs.supplements.Substring(11), cse) then
          cse.Supplements.Remove(cs);
      end
      else if FCodeSystemsByUrl.TryGetValue(cs.supplements, cse) then
        cse.Supplements.remove(cs);
      if cs1 <> nil then
        AddCodeSystemToCache(cs1.codeSystem, false);
    finally
      cs1.Free;
    end;
  end;
end;

procedure TTerminologyServerStore.SeeSpecificationResource(resource : TFHIRResourceV);
var
  vs : TFhirValueSetW;
  cs : TFhirCodeSystemW;
  cm : TLoadedConceptMap;
begin
  FLock.Lock('SeeSpecificationResource');
  try
    inc(FTagid);
    resource.TagInt := FTagId;
    if (resource.fhirType = 'ValueSet') then
    begin
      vs := FFactory.wrapValueSet(resource.link);
      try
        if (vs.url = 'http://hl7.org/fhir/ValueSet/ucum-common') then
          FCommonTerminologies.FUcum.SetCommonUnits(factory.wrapValueSet(resource.Link));

        FBaseValueSets.AddOrSetValue(vs.url, vs.Link);
        FValueSetsById.AddOrSetValue(vs.id, vs.Link);
        FValueSetsByUrl.AddOrSetValue(vs.url, vs.Link);
        if (vs.fhirObjectVersion = fhirVersionRelease2) then
        begin
          if vs.hasInlineCS then
          begin
            cs := FFactory.wrapCodeSystem(resource.Link);
            try
              AddCodeSystemToCache(cs, true);
            finally
              cs.free;
            end;
          end;
        end;
        UpdateConceptMaps;
      finally
        vs.Free;
      end;
    end
    else if (resource.fhirType = 'CodeSystem') then
    begin
      cs := FFactory.wrapCodeSystem(resource.link);
      try
        AddCodeSystemToCache(cs, true);
      finally
        cs.Free;
      end;
    end
    else if (resource.fhirType = 'ConceptMap') then
    begin
      cm := TLoadedConceptMap.Create;
      try
        cm.Resource := FFactory.wrapConceptMap(resource.Link);
        cm.Source := getValueSetByUrl(cm.Resource.source);
        cm.Target := getValueSetByUrl(cm.Resource.target);
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

procedure TTerminologyServerStore.SeeTerminologyResource(resource : TFHIRResourceV);
var
  vs : TFhirValueSetW;
  cs : TFhirCodeSystemW;
  cm : TLoadedConceptMap;
begin
  checkTerminologyResource(resource);

  FLock.Lock('SeeTerminologyResource');
  try
    inc(FTagid);
    resource.TagInt := FTagId;
    if (resource.fhirType = 'ValueSet') then
    begin
      vs := FFactory.wrapValueSet(resource.link);
      try
        if (vs.url = 'http://hl7.org/fhir/ValueSet/ucum-common') and (FCommonTerminologies.FUcum <> nil) then
          FCommonTerminologies.FUcum.SetCommonUnits(vs.link);

        FValueSetsById.AddOrSetValue(vs.id, vs.Link);
        FValueSetsByUrl.AddOrSetValue(vs.url, vs.Link);
        invalidateVS(vs.url);
        if vs.hasInlineCS then
        begin
          cs := FFactory.wrapCodeSystem(resource.Link);
          try
            AddCodeSystemToCache(cs, true);
          finally
            cs.free;
          end;
        end;
        UpdateConceptMaps;
      finally
        vs.free;
      end;
    end
    else if (resource.fhirType = 'CodeSystem') then
    begin
      cs := FFactory.wrapCodeSystem(resource.link);
      try
        AddCodeSystemToCache(cs, false);
      finally
        cs.Free;
      end;
    end
    else if (resource.fhirType = 'ConceptMap') then
    begin
      cm := TLoadedConceptMap.Create;
      try
        cm.Resource := FFactory.wrapConceptMap(resource.Link);
        cm.Source := getValueSetByUrl(cm.Resource.source);
        cm.Target := getValueSetByUrl(cm.Resource.target);
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

procedure TTerminologyServerStore.DropTerminologyResource(aType : String; id : String);
var
  vs, vs1 : TFhirValueSetW;
  cm, cm1 : TLoadedConceptMap;
  cs : TFhirCodeSystemW;
begin
  vs := nil;
  FLock.Lock('DropTerminologyResource');
  try
    if (aType = 'ValueSet') then
    begin
      vs := FValueSetsById[id];
      if vs <> nil then
      begin
        vs1 := FBaseValueSets[vs.url];
        FValueSetsByURL.Remove(vs.url);
        if (vs.hasInlineCS) then
          removeCodeSystemFromCache(vs.id);
        FValueSetsById.Remove(vs.id); // vs is no longer valid

        // add the base one back if we are dropping a value set that overrides it
        // current logical flaw: what if there's another one that overrides this? how do we prevent or deal with this?
        if vs1 <> nil then
        begin
          FValueSetsById.AddOrSetValue(vs.url, vs1.Link);
          cs := FFactory.wrapCodeSystem(vs.Resource.link);
          try
            AddCodeSystemToCache(cs, false);
          finally
            cs.Free;
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
      cm.Source := getValueSetByUrl(cm.Resource.source);
      if (cm.Source = nil) then
        cm.Source := getValueSetById(cm.Resource.source);
    end;
    if cm.Resource.target = '' then
      cm.Target := nil
    else
    begin
      cm.Target := getValueSetByUrl(cm.Resource.target);
      if (cm.Target = nil) then
        cm.Target := getValueSetById(cm.Resource.target);
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

function TTerminologyServerStore.getCodeSystem(url: String): TFhirCodeSystemW;
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

function TTerminologyServerStore.getCodeSystemById(id: String): TFhirCodeSystemW;
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

function TTerminologyServerStore.getCodeSystemByValueSet(vs: String): TFhirCodeSystemW;
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

procedure TTerminologyServerStore.GetCodeSystemList(list: TFslList<TFHIRMetadataResourceW>);
var
  vs : TFHIRCodeSystemEntry;
begin
  FLock.Lock('GetCodeSystemList');
  try
    for vs in FCodeSystemsByUrl.values do
      list.add(vs.CodeSystem.link);
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.GetCodeSystemList(list : TFslList<TFHIRCodeSystemW>);
var
  vs : TFHIRCodeSystemEntry;
begin
  FLock.Lock('GetCodeSystemList');
  try
    for vs in FCodeSystemsByUrl.values do
      list.add(vs.CodeSystem.link);
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
var
  vs : TFhirValueSetW;
begin
  FLock.Lock('GetValueSetList');
  try
    for vs in FValueSetsById.values do
      list.Add(vs.Link);
  finally
    FLock.Unlock;
  end;
end;


function TTerminologyServerStore.checkVersion(system, version : String; profile : TFHIRExpansionParams) : String;
var
  t : TFhirExpansionParamsFixedVersion;
begin
  if (profile = nil) then
    exit(version);

  for t in profile.fixedVersions do
    if (t.system = system) and (t.version <> '') then
    begin
      if (version = '') or (t.mode = fvmOverride) then
        version := t.version
      else if t.mode = fvmCheck then
        raise ETerminologyError.Create('Expansion Profile Error: the version "'+version+'" is inconsistent with the version "'+t.version+'" required by the profile');
    end;
  exit(version);
end;

Function TTerminologyServerStore.getProvider(system : String; version : String; profile : TFHIRExpansionParams; noException : boolean = false) : TCodeSystemProvider;
begin
  result := nil;
  version := checkVersion(system, version, profile);

  if ProviderClasses.ContainsKey(system) then
  begin
    if (version <> '') then
    begin
      if ProviderClasses.ContainsKey(system+URI_VERSION_BREAK+version) then
        result := ProviderClasses[system+URI_VERSION_BREAK+version].Link
      else
      begin
        // special support for SNOMED Editions
        if (system = 'http://snomed.info/sct') and version.contains('/version/') and ProviderClasses.ContainsKey(system+URI_VERSION_BREAK+version.Substring(0, version.IndexOf('/version/'))) then
          result := ProviderClasses[system+URI_VERSION_BREAK+version.Substring(0, version.IndexOf('/version/'))].Link
        else
          result := ProviderClasses[system].Link;
        if not result.defToThisVersion(version) then
        begin
          result.Free;
          raise ETerminologySetup.create('Unable to provide support for code system '+system+' version '+version);
        end;
      end;
    end
    else
      result := ProviderClasses[system].Link
  end
  else if system = ANY_CODE_VS then
    if FFactory.version in [fhirVersionRelease2, fhirVersionRelease3] then
      result := TAllCodeSystemsProvider.create(FCommonTerminologies.link, getProvider('http://hl7.org/fhir/v3/ActCode', '', nil))
    else
      result := TAllCodeSystemsProvider.create(FCommonTerminologies.link, getProvider('http://terminology.hl7.org/CodeSystem/v3-ActCode', '', nil))
  else
  begin
    FLock.Lock('getProvider');
    try
      // todo; version specific....
      if FCodeSystemsByUrl.ContainsKey(system) then
        result := TFhirCodeSystemProvider.create(ffactory.link, FCodeSystemsByUrl[system].link);
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


function TTerminologyServerStore.getProvider(codesystem: TFhirCodeSystemW; profile : TFHIRExpansionParams): TCodeSystemProvider;
begin
  checkVersion(codeSystem.url, codeSystem.version, profile);
  result := TFhirCodeSystemProvider.create(FFactory.link, TFHIRCodeSystemEntry.Create(codesystem.link));
end;

function TTerminologyServerStore.getProviderClasses: TFslMap<TCodeSystemProvider>;
begin
  result := FCommonTerminologies.ProviderClasses;
end;

procedure TTerminologyServerStore.getSummary(b: TStringBuilder);
begin
  FCommonTerminologies.getSummary(b);

  b.append('<li>ValueSets : '+inttostr(FValueSetsById.Count)+'</li>');
  b.append('<li>Code Systems : '+inttostr(FCodeSystemsByUrl.Count)+'</li>');
  b.append('<li>Concept Maps : '+inttostr(FBaseConceptMaps.Count)+'</li>');
end;

function TTerminologyServerStore.getValueSetById(id: String): TFHIRValueSetW;
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

function TTerminologyServerStore.getValueSetByUrl(url : String) : TFHIRValueSetW;
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

procedure TTerminologyServerStore.GetValueSetList(list: TFslList<TFHIRMetadataResourceW>);
var
  vs : TFhirValueSetW;
begin
  FLock.Lock('GetValueSetList');
  try
    for vs in FValueSetsById.values do
      list.Add(vs.Link);
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

function TTerminologyServerStore.listSystems: TArray<String>;
var
  ts : TStringList;
  p : TCodeSystemProvider;
  s : String;
  i : integer;
begin
  ts := TStringList.Create;
  try
    ts.Sorted := true;
    ts.Duplicates := TDuplicates.dupIgnore;
    FLock.Lock('CodeSystemCount');
    try
      for p in ProviderClasses.Values do
        ts.Add(p.system(nil));
      for s in FCodeSystemsByUrl.Keys do
        ts.Add(s);
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
  else if (FCommonTerminologies.DefSnomed <> nil) and (uri1 = FCommonTerminologies.DefSnomed.system(nil)) then
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

procedure TCommonTerminologies.add(p: TCodeSystemProvider);
begin
  if p.version(nil) <> '' then
    FProviderClasses.Add(p.system(nil)+URI_VERSION_BREAK+p.version(nil), p.link);
  if FProviderClasses.ContainsKey(p.system(nil)) then
    p.free
  else
    FProviderClasses.Add(p.system(nil), p);
end;

constructor TCommonTerminologies.Create(settings : TFHIRServerSettings);
var
  p : TCodeSystemProvider;
begin
  inherited Create;
  FSettings := settings;
  FSnomed := TFslList<TSnomedServices>.create;
  FIcd10 := TFslList<TICD10Provider>.create;
  p := TUriServices.Create();
  FProviderClasses := TFslMap<TCodeSystemProvider>.Create;
  FProviderClasses.Add(p.system(nil), p);
  FProviderClasses.Add(p.system(nil)+URI_VERSION_BREAK+p.version(nil), p.link);
end;

destructor TCommonTerminologies.Destroy;
begin
  FNDFRT.Free;
  FSettings.Free;
  FIcd10.Free;
  FLoinc.free;
  FDefSnomed.Free;
  FSnomed.free;
  FUnii.Free;
  FACIR.Free;
  FUcum.free;
  FRxNorm.Free;
  FNciMeta.Free;
  FProviderClasses.Free;
  inherited;
end;

procedure TCommonTerminologies.getSummary(b: TStringBuilder);
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

  if FNDFRT = nil then
    b.append('<li>NDFRT: not loaded</li>')
  else
    b.append('<li>NDFRT: '+NDFRT.version(nil)+' ('+inttostr(NDFRT.UseCount)+' uses)');

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
end;

function TCommonTerminologies.link: TCommonTerminologies;
begin
  result := TCommonTerminologies(inherited Link);
end;


procedure TCommonTerminologies.load(ini: TFHIRServerIniFile; databases: TFslMap<TKDBManager>; testing : boolean);
var
  details : TFHIRServerIniComplex;
  s : string;
  sn: TSnomedServices;
//  def : boolean;
  icdX: TICD10Provider;
begin
  logt('Load Common Terminologies');

  add(TACIRServices.Create);
  add(TAreaCodeServices.Create);
  add(TIso4217Services.Create);
  add(TMimeTypeCodeServices.Create);
  add(TCountryCodeServices.Create());
  add(TUSStateServices.Create);

  for s in ini.terminologies.Keys do
  begin
    details := ini.terminologies[s];
    if not testing or (details['when-testing'] = 'true') then
    begin
      if details['type'] = 'icd10' then
      begin
        logt('load '+s+' from '+details['source']);
        icdX := TICD10Provider.Create(true, details['source']);
        add(icdX);
        icd10.Add(icdX.Link);
      end
      else if details['type'] = 'snomed' then
      begin
        logt('load '+s+' from '+details['source']);
        sn := TSnomedServices.Create;
        try
          sn.Load(details['source']);
          add(sn.Link);
          snomed.Add(sn.Link);
          if details['default'] = 'true' then
            DefSnomed := sn.Link;
        finally
          sn.Free;
        end;
      end
      else if details['type'] = 'loinc' then
      begin
        logt('load '+s+' from '+details['source']);
        Loinc := TLoincServices.Create;
        add(Loinc.link);
        Loinc.Load(details['source']);
      end
      else if details['type'] = 'ucum' then
      begin
        logt('load '+s+' from '+details['source']);
        Ucum := TUcumServices.Create;
        Ucum.Import(details['source']);
      end
      else if details['type'] = 'rxnorm' then
      begin
        logt('load '+s+' from '+details['database']);
        RxNorm := TRxNormServices.create(databases[details['database']].link)
      end
      else if details['type'] = 'ndfrt' then
      begin
        logt('load '+s+' from '+details['database']);
        NDFRT := TNDFRTServices.create(databases[details['database']].link)
      end
      else if details['type'] = 'mcimeta' then
      begin
        logt('load '+s+' from '+details['database']);
        NciMeta := TNciMetaServices.Create(databases[details['database']].link)
      end
      else if details['type'] = 'unii' then
      begin
        logt('load '+s+' from '+details['database']);
        Unii := TUniiServices.Create(databases[details['database']].link)
      end
      else if details['type'] = 'lang' then
      begin
        logt('load '+s+' from '+details['source']);
        add(TIETFLanguageCodeServices.Create(details['source']))
      end
      else
        raise EFslException.Create('Unknown type '+details['type']);
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

procedure TCommonTerminologies.SetNciMeta(const Value: TNciMetaServices);
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

procedure TCommonTerminologies.SetNDFRT(const Value: TNDFRTServices);
begin
  if FNDFRT <> nil then
  begin
    FProviderClasses.Remove(FNDFRT.system(nil));
    FProviderClasses.Remove(FNDFRT.system(nil)+URI_VERSION_BREAK+FNDFRT.version(nil));
  end;
  FNDFRT.Free;
  FNDFRT := Value;
  if FNDFRT <> nil then
  begin
    FProviderClasses.add(FNDFRT.system(nil), FNDFRT.Link);
    FProviderClasses.add(FNDFRT.system(nil)+URI_VERSION_BREAK+FNDFRT.version(nil), FNDFRT.Link);
  end;
end;

procedure TCommonTerminologies.SetUnii(const Value: TUniiServices);
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

procedure TCommonTerminologies.SetACIR(const Value: TACIRServices);
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

procedure TCommonTerminologies.SetDefSnomed(const Value: TSnomedServices);
begin
  FDefSnomed.Free;
  FDefSnomed := Value;
end;

procedure TCommonTerminologies.SetUcum(const Value: TUcumServices);
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

end.
