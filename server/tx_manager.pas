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
  fhir_codesystem_service, fhir_tx, fhir_valuesets,
  ftx_service, ftx_loinc_services, ftx_ucum_services, ftx_sct_services, tx_rxnorm, tx_unii, tx_acir,
  tx_uri, tx_areacode, tx_countrycode, tx_us_states, tx_iso_4217, tx_version,
  tx_mimetypes, ftx_lang, tx_ndc, tx_hgvs, tx_cpt, tx_omop,
  utilities, server_config, kernel_thread, server_stats, fhir_utilities;

const
  URI_VERSION_BREAK = '#';

Type
  { TCodeSystemProviderGeneralFactory }

  TCodeSystemProviderGeneralFactory = class (TCodeSystemProviderFactory)
  private
    FProvider : TCodeSystemProvider;
  public
    Constructor Create(provider : TCodeSystemProvider);
    Destructor Destroy; override;

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
    FI18n : TI18nSupport;
  public
    Constructor Create(snomed : TSnomedServices; i18n : TI18nSupport);
    Destructor Destroy; override;

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
    FCPT: TCPTServices;
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
    FOMOP : TOMOPServices;
    FI18n : TI18nSupport;

    procedure SetCPT(AValue: TCPTServices);
    procedure SetI18n(AValue: TI18nSupport);
    procedure SetOMOP(AValue: TOMOPServices);
    procedure SetLoinc(const Value: TLOINCServices);
    procedure SetDefSnomed(const Value: TSnomedServices);
    procedure SetUcum(const Value: TUcumServices);
    procedure SetRxNorm(const Value: TRxNormServices);
    procedure SetNDC(const Value: TNDCServices);
    procedure SetUnii(const Value: TUniiServices);
    procedure SetACIR(const Value: TACIRServices);

    procedure getSummary(b : TFslStringBuilder);
    procedure loadLang;

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
    procedure defineFeatures(features : TFslList<TFHIRFeature>); virtual;
    procedure getCacheInfo(ci: TCacheInformation); virtual;

    // load external terminology resources (snomed, Loinc, etc)     
    procedure recordStats(rec : TStatusRecord);
    procedure load(txlist: TFHIRServerConfigSection; testing : boolean);
    procedure listVersions(url : String; list : TStringList);

    property i18n : TI18nSupport read FI18n write SetI18n;
    property Languages : TIETFLanguageDefinitions read FLanguages;
    Property Loinc : TLOINCServices read FLoinc write SetLoinc;
    Property Snomed : TFslList<TSnomedServices> read FSnomed;
    Property DefSnomed : TSnomedServices read FDefSnomed write SetDefSnomed;
    Property Ucum : TUcumServices read FUcum write SetUcum;
    Property RxNorm : TRxNormServices read FRxNorm write SetRxNorm;
    Property NDC : TNDCServices read FNDC write SetNDC;
    Property NDFRT : TNDFRTServices read FNDFRT write SetNDFRT;
    Property Unii : TUniiServices read FUnii write SetUnii;
    property CPT : TCPTServices read FCPT write SetCPT;
    property OMOP : TOMOPServices read FOMOP write SetOMOP;
    Property ACIR : TACIRServices read FACIR write SetACIR;
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
    FSupplementsById : TFslMap<TFHIRResourceProxyV>; // All supplements

    FBaseValueSets : TFslMap<TFHIRResourceProxyV>; // value sets out of the specification - these can be overriden, but they never go away
    FBaseCodeSystems : TFslMap<TFHIRCodeSystemEntry>; // value sets out of the specification - these can be overriden, but they never go away

    FBaseConceptMaps : TFslMap<TFHIRConceptMapW>; // value sets out of the specification - these can be overriden, but they never go away
    FConceptMapsById : TFslMap<TFHIRConceptMapW>;
    FConceptMapsByURL : TFslMap<TFHIRConceptMapW>;

    FLoading : boolean;

    procedure checkCSLoaded(codesystem: TFHIRCodeSystemEntry);
    procedure SetLoading(AValue: boolean);
    procedure UpdateConceptMaps;
    procedure BuildStems(cs : TFhirCodeSystemW);

    procedure checkForDuplicates(codes: TStringList; list: TFhirCodeSystemConceptListW; url : String);
    function checkVersion(system, version: String; profile: TFHIRTxOperationParams): String;
    procedure AddCodeSystemToCache(cs : TFHIRResourceProxyV; base : boolean); overload;
    procedure AddCodeSystemToCache(packageId : String; cs : TFHIRCodeSystemW; base : boolean); overload;
    procedure RemoveCodeSystemFromCache(id : String);
    function getProviderClasses: TFslMap<TCodeSystemProviderFactory>;
    function defToLatestForSystem(system : String) : boolean;
    procedure LoadImplicitConceptMaps;
  protected
    FLock : TFslLock;  // it would be possible to use a read/write lock, but the complexity doesn't seem to be justified by the short amount of time in the lock anyway
    FDB : TFDBManager;
    procedure invalidateVS(id : String); virtual;
    procedure getSummary(b : TFslStringBuilder);
  public
    constructor Create(db : TFDBManager; factory : TFHIRFactory; common : TCommonTerminologies; i18n : TI18nSupport); virtual;
    destructor Destroy; Override;
    Function Link : TTerminologyServerStore; overload;

    Property Factory : TFHIRFactory read FFactory;
    Property DB : TFDBManager read FDB;
    property CommonTerminologies : TCommonTerminologies read FCommonTerminologies;
    property i18n : TI18nSupport read FI18n;

    // if this id identifies a value set known to the external resources (if it does, construct it and return it)
    function isKnownValueSet(id : String; out vs : TFhirValueSetW): Boolean;
    function makeAnyValueSet: TFhirValueSetW;

    // maintenance procedures
    procedure SeeSpecificationResource(resource : TFHIRResourceProxyV);
    procedure SeeTerminologyResource(resource : TFHIRResourceProxyV);
    procedure DropTerminologyResource(aType : String; id : String);
    procedure loadCodeSystem(cs : TFHIRResourceProxyV); overload;
    procedure loadCodeSystem(cs : TFHIRCodeSystemW); overload;

    // access procedures. All return values are owned, and must be freed
    Function getProvider(system : String; version : String; profile : TFHIRTxOperationParams; noException : boolean = false) : TCodeSystemProvider; overload;
    Function getProvider(codesystem : TFHIRCodeSystemW; profile : TFHIRTxOperationParams) : TCodeSystemProvider; overload;
    function getValueSetByUrl(url, version : String; txResources : TFslList<TFHIRCachedMetadataResource> = nil) : TFHIRValueSetW;
    function getValueSetById(id : String) : TFHIRValueSetW;
    function getCodeSystemById(id : String) : TFHIRCodeSystemW;
    function getCodeSystemByValueSet(vs : String) : TFHIRCodeSystemW;
    function getCodeSystem(url : String; txResources : TFslList<TFHIRCachedMetadataResource> = nil) : TFHIRCodeSystemEntry;
    function hasCodesystemUri(url : String; txResources : TFslList<TFHIRCachedMetadataResource> = nil) : Boolean;
    function getConceptMapById(id : String) : TFHIRConceptMapW;
    function getConceptMapByUrl(url : String) : TFHIRConceptMapW;
    function getConceptMapBySrcTgt(src, tgt : String) : TFHIRConceptMapW;
    procedure listVersions(url : String; list : TStringList);

    // publishing access
    procedure GetCodeSystemList(list : TFslList<TFHIRCodeSystemW>); overload;
    procedure GetCodeSystemList(list : TFslMetadataResourceList); overload;
    procedure GetValueSetList(list : TFslList<TFhirValueSetW>); overload;
    procedure GetValueSetList(list : TFslMetadataResourceList); overload;
    function GetConceptMapList : TFslList<TFHIRConceptMapW>;
    Property ProviderClasses : TFslMap<TCodeSystemProviderFactory> read getProviderClasses;
    function ValueSetCount : integer;
    function CodeSystemCount : integer;
    function listSystems : TArray<String>;
    function listSystemVersions : TArray<String>;

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
    procedure recordStats(rec : TStatusRecord); virtual;

    procedure Unload; virtual;

    procedure clearCache; virtual;
    procedure SetCacheStatus(status : boolean); virtual;
    procedure getCacheInfo(ci: TCacheInformation); virtual;
    procedure defineFeatures(features: TFslList<TFHIRFeature>); virtual;

    property Loading : boolean read FLoading write SetLoading;
  end;

implementation

{ TTerminologyServerStore }

procedure TTerminologyServerStore.BuildStems(cs: TFhirCodeSystemW);
  //function stems(c : TFhirCodeSystemConceptW) : TConceptAdornment;
  //var
  //  s, t : String;
  //begin
  //  result := TConceptAdornment.Create;
  //  c.Tag := result;
  //  t := c.display;
  //  while (t <> '') Do
  //  begin
  //    StringSplit(t, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='], s, t);
  //    if (s <> '') Then
  //      result.Add(lowercase(FStem.Stem(s)));
  //  end;
  //  result.SortAscending;
  //end;
  //procedure processConcepts(parent : TFhirCodeSystemConceptW; list : TFhirCodeSystemConceptListW; map : TFhirCodeSystemConceptMapW);
  //var
  //  c : TFhirCodeSystemConceptW;
  //begin
  //  for c in list do
  //  begin
  //    stems(c).parent := parent;
  //    if map.ContainsKey(c.code) then
  //      Logging.log('Duplicate code '+c.code+' in '+cs.url)
  //    else
  //      map.Add(c.code, c.Link);
  //    processConcepts(c, c.conceptList, map);
  //  end;
  //end;
var
  map : TFhirCodeSystemConceptMapW;
begin
  raise EFslException.create('todo');
  //map := TFhirCodeSystemConceptMapW.Create('stems');
  //try
  //  !cs.Tag := TCodeSystemAdornment.Create(map.link);
  //  processConcepts(nil, cs.conceptList, map);
  //finally
  //  map.free;
  //end;
end;


function TTerminologyServerStore.cacheSize(magic : integer): UInt64;
begin
  result := 0;
end;

procedure TTerminologyServerStore.recordStats(rec: TStatusRecord);
begin
  // nothing?
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
        raise ETerminologyError.Create('Duplicate code: '+cc.code+' in value set '+url, itInvalid);
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

  FValueSets := TFHIRMetadataResourceManagerW<TFhirValueSetW>.Create;
  FCodeSystems := TFHIRCodeSystemManager.Create;
  FCodeSystemsByVsUrl := TFslMap<TFhirCodeSystemEntry>.Create('tx.cs.url');
  FBaseValueSets := TFslMap<TFhirResourceProxyV>.Create('tx.vs.base');
  FBaseCodeSystems := TFslMap<TFHIRCodeSystemEntry>.Create('tx.cs.base');
  FSupplementsById := TFslMap<TFhirResourceProxyV>.Create('tx.cs.suppl');

  FBaseConceptMaps := TFslMap<TFHIRConceptMapW>.Create('tx.cm.base');
  FConceptMapsById := TFslMap<TFHIRConceptMapW>.Create('tx.cm.id');
  FConceptMapsByURL := TFslMap<TFHIRConceptMapW>.Create('tx.cm.url');

  FBaseValueSets.defaultValue := nil;
  FBaseCodeSystems.defaultValue := nil;
  FBaseConceptMaps.defaultValue := nil;

  FStem := TFslWordStemmer.Create('english');

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
  LoadImplicitConceptMaps;
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
        cs.free;
      end;
      list.Add(r.link);
    finally
      r.free;
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
    addCodesystemUri('LOINC', 'loinc', FCommonTerminologies.FLoinc.systemUri, FCommonTerminologies.FLoinc.version, FCommonTerminologies.FLoinc.TotalCount);
  for sn in FCommonTerminologies.FSnomed do
  begin
    sp := TSnomedProvider.Create(sn.link, FI18n.link, nil);
    try
      addCodesystemUri('SNOMED CT', 'sct', sp.systemUri, sp.version, sp.TotalCount);
    finally
      sp.free;
    end;
  end;
  if FCommonTerminologies.FUcum <> nil then
    addCodesystemUri('Ucum', 'ucum', FCommonTerminologies.FUcum.systemUri, FCommonTerminologies.FUcum.version, FCommonTerminologies.FUcum.TotalCount);
  if FCommonTerminologies.FRxNorm <> nil then
    addCodesystemUri('RxNorm', 'rxnorm', FCommonTerminologies.FRxNorm.systemUri, FCommonTerminologies.FRxNorm.version, FCommonTerminologies.FRxNorm.TotalCount);
  if FCommonTerminologies.NDFRT <> nil then
    addCodesystemUri('NDFRT', 'ndfrt', FCommonTerminologies.NDFRT.systemUri, FCommonTerminologies.NDFRT.version, FCommonTerminologies.NDFRT.TotalCount);
  if FCommonTerminologies.FUnii <> nil then
    addCodesystemUri('Unii', 'unii', FCommonTerminologies.FUnii.systemUri, FCommonTerminologies.FUnii.version, FCommonTerminologies.FUnii.TotalCount);
  if FCommonTerminologies.FCPT <> nil then
    addCodesystemUri('CPT', 'cpt', FCommonTerminologies.FCPT.systemUri, FCommonTerminologies.FCPT.version, FCommonTerminologies.FCPT.TotalCount);
  if FCommonTerminologies.FOMOP <> nil then
    addCodesystemUri('OMOP', 'omop', FCommonTerminologies.FOMOP.systemUri, FCommonTerminologies.FOMOP.version, FCommonTerminologies.FOMOP.TotalCount);
  if FCommonTerminologies.FACIR <> nil then
    addCodesystemUri('ACIR', 'acir', FCommonTerminologies.FACIR.systemUri, FCommonTerminologies.FACIR.version, FCommonTerminologies.FACIR.TotalCount);
end;


procedure TTerminologyServerStore.defineFeatures(features: TFslList<TFHIRFeature>);
var
  cs : TFhirCodeSystemW;
  list : TFslList<TFHIRCodeSystemW>;
begin
  FCommonTerminologies.defineFeatures(features);
  list := TFslList<TFHIRCodeSystemW>.Create;
  try
    FCodeSystems.listAll(list);
    for cs in list do
    begin

    end;
  finally
    list.free;
  end;


end;

destructor TTerminologyServerStore.Destroy;
begin
  FCommonTerminologies.free;
  FI18n.free;
  FStem.free;
  FValueSets.free;
  FCodeSystems.free;
  FCodeSystemsByVsUrl.free;
  FBaseValueSets.free;
  FBaseCodeSystems.free;
  FSupplementsByid.free;


  FBaseConceptMaps.free;
  FConceptMapsById.free;
  FConceptMapsByURL.free;

  FLock.free;
  FDB.free;
  FFactory.free;
  inherited;
end;

function TTerminologyServerStore.subsumes(cs: TFhirCodeSystemW; codeA, codeB: TFHIRCodingW): string;
var
  prov : TCodeSystemProvider;
begin
  // later, see if we can translate instead
  if (cs.url <> codeA.systemUri) then
    raise ETerminologyError.Create('System uri / code uri mismatch - not supported at this time ('+cs.url+'/'+codeA.systemUri+')', itNotSupported);
  if (cs.url <> codeB.systemUri) then
    raise ETerminologyError.Create('System uri / code uri mismatch - not supported at this time ('+cs.url+'/'+codeB.systemUri+')', itNotSupported);
  if (codeA.code = codeB.code) then
    exit('equivalent');

  prov := getProvider(cs, nil);
  try
    result := prov.subsumesTest(nil, codeA.code, codeB.code);
  finally
    prov.free;
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
    p.free;
  end;
end;


// ----  maintenance procedures ------------------------------------------------

function urlTail(path : String) : String;
begin
  result := path.substring(path.lastIndexOf('/')+1);
end;


procedure TTerminologyServerStore.AddCodeSystemToCache(cs : TFHIRResourceProxyV; base : boolean);
var
  cse, ct : TFHIRCodeSystemEntry;
  supp : TFHIRResourceProxyV;
begin
  cse := TFHIRCodeSystemEntry.Create(cs.Link);
  try
    cs.TagObject := cse;
    if base then
      FBaseCodeSystems.AddOrSetValue(cs.url, cse.Link);
    if (cs.supplements <> '') then
    begin
      FSupplementsById.AddOrSetValue(cs.id, cs.Link);
      if cs.supplements.StartsWith('CodeSystem/') then
      begin
        if FCodeSystems.has(cs.supplements.Substring(11), ct) then
          ct.SupplementProxies.Add(cs.Link);
      end
      else if FCodeSystems.has(cs.supplements, ct) then
        ct.SupplementProxies.Add(cs.Link);
    end
    else
    begin
      FCodeSystems.see(cse);
      if cs.valueSet <> '' then
        FCodeSystemsByVsUrl.AddOrSetValue(cs.valueSet, cse.Link);
      //if (FDB <> nil) then // don't build stems in this case
      //  BuildStems(cs); // todo: bring it back and move this out of the lock
      for supp in FSupplementsById.values do
        if (supp.supplements = cs.url) or (supp.supplements = 'CodeSystem/'+cs.id) then
          cse.SupplementProxies.Add(cs.Link);
    end;
  finally
    cse.free;
  end;
end;

procedure TTerminologyServerStore.AddCodeSystemToCache(packageId : String; cs: TFHIRCodeSystemW; base: boolean);
var
  cse, ct : TFHIRCodeSystemEntry;
  supp : TFHIRResourceProxyV;
begin
  cse := TFHIRCodeSystemEntry.Create(cs.Link);
  try
    if base then
      FBaseCodeSystems.AddOrSetValue(cs.url, cse.Link);
    if (cs.supplements <> '') then
    begin
      FSupplementsById.AddOrSetValue(cs.id, TFHIRResourceProxyW.create(packageId, cs.Link, cs.url, cs.version));
      if cs.supplements.StartsWith('CodeSystem/') then
      begin
        if FCodeSystems.has(cs.supplements.Substring(11), ct) then
          ct.SupplementProxies.Add(TFHIRResourceProxyW.create(packageId, cs.Link, cs.vurl, cs.version));
      end
      else if FCodeSystems.has(cs.supplements, ct) then
        ct.SupplementProxies.Add(TFHIRResourceProxyW.create(packageId, cs.Link, cs.url, cs.version));
    end
    else
    begin
      FCodeSystems.see(cse);
      if cs.valueSet <> '' then
        FCodeSystemsByVsUrl.AddOrSetValue(cs.valueSet, cse.Link);
      //if (FDB <> nil) then // don't build stems in this case
      //  BuildStems(cs); // todo: bring it back and move this out of the lock
      for supp in FSupplementsById.values do
        if (supp.supplements = cs.url) or (supp.supplements = 'CodeSystem/'+cs.id) then
          cse.SupplementProxies.Add(TFHIRResourceProxyW.create(packageId, cs.Link, cs.url, cs.version));
    end;
  finally
    cse.free;
  end;
end;

procedure TTerminologyServerStore.RemoveCodeSystemFromCache(id : String);
var
  cse, cs1 : TFHIRCodeSystemEntry;
  cs : TFHIRResourceProxyV;
begin
  if (FCodeSystems.has(id, cse)) then
  begin
    cs1 := FBaseCodeSystems[cse.CodeSystem.url].Link;
    try
      if cs1 <> nil then
        AddCodeSystemToCache(cs1.CodeSystemProxy, false);
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
          cse.SupplementProxies.Remove(cs);
      end
      else if FCodeSystems.has(cs.supplements, cse) then
        cse.SupplementProxies.remove(cs);
      if cs1 <> nil then
        AddCodeSystemToCache(cs1.CodeSystemProxy, false);
    finally
      cs1.free;
    end;
  end;
end;

procedure TTerminologyServerStore.SeeSpecificationResource(resource : TFHIRResourceProxyV);
var
  vs : TFhirValueSetW;
  cs : TFhirCodeSystemW;
  cm : TFHIRConceptMapW;
begin
  FLock.Lock('SeeSpecificationResource');
  try
    if (resource.fhirType = 'ValueSet') then
    begin
      if (resource.url = 'http://hl7.org/fhir/ValueSet/ucum-common') and (FCommonTerminologies.FUcum <> nil) then
        FCommonTerminologies.FUcum.SetCommonUnits(resource.resourceW.Link as TFHIRValueSetW);

      FBaseValueSets.AddOrSetValue(resource.url, resource.Link);
      FValueSets.see(resource);
      if not loading then
        UpdateConceptMaps;
    end
    else if (resource.fhirType = 'CodeSystem') then
    begin
      AddCodeSystemToCache(resource, true);
    end
    else if (resource.fhirType = 'ConceptMap') then
    begin
      cm := resource.resourceW as TFHIRConceptMapW;
      FConceptMapsById.AddOrSetValue(cm.id, cm.Link);
      FConceptMapsByURL.AddOrSetValue(cm.url, cm.Link);
      FConceptMapsByURL.AddOrSetValue(cm.vurl, cm.Link);
      FBaseConceptMaps.AddOrSetValue(cm.url, cm.Link);
    end
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.SeeTerminologyResource(resource : TFHIRResourceProxyV);
var
  vs : TFhirValueSetW;
  cs : TFhirCodeSystemW;
  cm : TFHIRConceptMapW;
begin
  FLock.Lock('SeeTerminologyResource');
  try
    if (resource.fhirType = 'ValueSet') then
    begin
      if (resource.url = 'http://hl7.org/fhir/ValueSet/ucum-common') and (FCommonTerminologies.FUcum <> nil) then
        FCommonTerminologies.FUcum.SetCommonUnits(resource.resourceW.link as TFHIRValueSetW);

      FValueSets.see(resource);
      invalidateVS(resource.url);
    end
    else if (resource.fhirType = 'CodeSystem') then
    begin
      cs := resource.resourceW as TFHIRCodeSystemW;
      AddCodeSystemToCache(resource.packageId, cs, false);
    end
    else if (resource.fhirType = 'ConceptMap') then
    begin
      cm := resource.resourceW as TFHIRConceptMapW;
      FConceptMapsById.AddOrSetValue(cm.id, cm.Link);
      FConceptMapsByURL.AddOrSetValue(cm.url, cm.Link);
      FConceptMapsByURL.AddOrSetValue(cm.vurl, cm.Link);
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
  cm, cm1 : TFhirConceptMapW;
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
            //    cs.free;
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
        cm1 := FBaseConceptMaps[cm.url];
        FConceptMapsByURL.Remove(cm.url);
        FConceptMapsByid.Remove(id); // cm is no longer valid

        // add the base one back if we are dropping a concept map that overrides it
        // current logical flaw: what if there's another one that overrides this? how do we prevent or deal with this?
        if cm1 <> nil then
        begin
          FConceptMapsById.AddOrSetValue(cm1.id, cm1.Link);
          FConceptMapsByURL.AddOrSetValue(cm1.url, cm1.Link);
        end;
      end;
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TTerminologyServerStore.loadCodeSystem(cs: TFHIRResourceProxyV);
var
  cse : TFHIRCodeSystemEntry;
begin
  cse := cs.TagObject as TFHIRCodeSystemEntry;
  checkCSLoaded(cse);
end;

procedure TTerminologyServerStore.loadCodeSystem(cs: TFHIRCodeSystemW);
begin
  raise EFslException.create('loadCodeSystem(TFHIRCodeSystemW) not done yet');
end;

procedure TTerminologyServerStore.UpdateConceptMaps;
begin
end;

procedure TTerminologyServerStore.SetLoading(AValue: boolean);
begin
  if FLoading then
  begin
    FLock.Lock('SetLoading');
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

function TTerminologyServerStore.getCodeSystem(url: String; txResources: TFslList<TFHIRCachedMetadataResource>): TFHIRCodeSystemEntry;
var
  r : TFHIRCachedMetadataResource;
begin
 if txResources <> nil then
  begin
    for r in txResources do
      if (url <> '') and ((r.resource.url = url) or (r.resource.vurl = url)) then
      begin
        if not (r.resource is TFhirCodeSystemW) then
          raise EFHIRException.Create('Attempt to reference '+url+' as a CodeSystem when it''s a '+r.resource.fhirType);
        if r.LoadedCS = nil then
          r.LoadedCS := TFHIRCodeSystemEntry.create(r.resource.link as TFHIRCodeSystemW);
        exit(r.LoadedCS.link);
      end;
  end;

  FLock.Lock('getValueSetByUrl');
  try
    if FCodeSystems.has(url) then
      result := TFHIRCodeSystemEntry.create(FCodeSystems.get(url).CodeSystem.Link)
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.getCodeSystemById(id: String): TFHIRCodeSystemW;
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

function TTerminologyServerStore.getCodeSystemByValueSet(vs: String): TFHIRCodeSystemW;
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


function TTerminologyServerStore.getConceptMapById(id: String): TFHIRConceptMapW;
begin
  FLock.Lock('getConceptMapById');
  try
    if FConceptMapsById.ContainsKey(id) then
    begin
      result := FConceptMapsById[id].Link
    end
    else
    begin
      result := nil;
    end;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.getConceptMapByUrl(url: String): TFHIRConceptMapW;
begin
  FLock.Lock('getConceptMapByUrl');
  try
    if FConceptMapsByUrl.ContainsKey(url) then
    begin
      result := FConceptMapsByUrl[url].Link
    end
    else
    begin
      result := nil;
    end;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.getConceptMapBySrcTgt(src, tgt: String
  ): TFHIRConceptMapW;
var
  lcm : TFhirConceptMapW;
begin
  result := nil;
  FLock.Lock('getValueSetByUrl');
  try
    for lcm in FConceptMapsById.Values do
      if (lcm.source = src) and
         (lcm.target = src) then
      begin
        result := lcm.Link;
        break;
      end
  finally
    FLock.Unlock;
  end;
//  if result = nil then   
//    Logging.log('did not find map for "'+src+'" -> "'+tgt+'"')
//  else
//    Logging.log('Find map "'+lcm.url+'" for "'+src+'" -> "'+tgt+'"');
end;

function TTerminologyServerStore.GetConceptMapList: TFslList<TFHIRConceptMapW>;
var
  cm : TFhirConceptMapW;
begin
  result := TFslList<TFhirConceptMapW>.Create;
  try
    FLock.Lock('GetConceptMapList');
    try
      for cm in FConceptMapsById.values do
        result.Add(cm.Link);
    finally
      FLock.Unlock;
    end;
    result.Link;
  finally
    result.free;
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


function TTerminologyServerStore.checkVersion(system, version : String; profile : TFHIRTxOperationParams) : String;
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
end;

function TTerminologyServerStore.defToLatestForSystem(system : String) : boolean;
var
  p : TCodeSystemProvider;
begin
  if system = URI_SNOMED then
    result := false
  else if ProviderClasses.ContainsKey(system) then
  begin
    p := ProviderClasses[system].getProvider;
    try
      result := p.defaultToLatest;
    finally
      p.free;
    end;
  end
  else
    result := false;
end;

procedure TTerminologyServerStore.LoadImplicitConceptMaps;
var
  pvf : TCodeSystemProviderFactory;
  pv : TCodeSystemProvider;
  cml, l: TFslList<TFhirConceptMapW>;
  cm : TFhirConceptMapW;
begin
  cml := TFslList<TFhirConceptMapW>.create;
  try
    for pvf in ProviderClasses.Values do
    begin
      pv := pvf.getProvider;
      try
        l := TFslList<TFhirConceptMapW>.create;
        try
          pv.registerConceptMaps(l, Factory);
          for cm in l do
          begin
            cm.tag := pvf.link;
            cml.add(cm.link);
          end;
        finally
          l.free;
        end;
      finally
        pv.free;
      end;
    end;

    for cm in cml do
    begin
      FConceptMapsById.AddOrSetValue(cm.id, cm.Link);
      FConceptMapsByURL.AddOrSetValue(cm.url, cm.Link);
      FConceptMapsByURL.AddOrSetValue(cm.vurl, cm.Link);
      FBaseConceptMaps.AddOrSetValue(cm.url, cm.Link);
    end;
  finally
    cml.free;
  end;
end;

function TTerminologyServerStore.getProvider(system: String; version: String;
  profile: TFHIRTxOperationParams; noException: boolean): TCodeSystemProvider;
var
  defToLatest : boolean;
  cs : TFHIRCodeSystemEntry;
begin
  result := nil;
  if (profile = nil) then
    defToLatest := false
  else
    defToLatest := profile.defaultToLatestVersion;

  if ProviderClasses.ContainsKey(system) then
  begin
    if (version <> '') then
    begin
      if ProviderClasses.ContainsKey(system+URI_VERSION_BREAK+version) then
        result := ProviderClasses[system+URI_VERSION_BREAK+version].getProvider
      else if defToLatest or defToLatestForSystem(system) then
      begin
        // special support for SNOMED Editions
        if (system = URI_SNOMED) and version.contains('/version/') and ProviderClasses.ContainsKey(system+URI_VERSION_BREAK+version.Substring(0, version.IndexOf('/version/'))) then
          result := ProviderClasses[system+URI_VERSION_BREAK+version.Substring(0, version.IndexOf('/version/'))].getProvider
        else
          result := ProviderClasses[system].getProvider;
        if (result = nil) or not (result.defToThisVersion(version)) then
        begin
          result.free;
          result := nil;
          if not noException then
            raise ETerminologySetup.Create('Unable to provide support for code system '+system+' version '+version);
        end;
      end;
    end
    else
      result := ProviderClasses[system].getProvider;
  end
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
          begin
            checkCSLoaded(cs);
            result := TFhirCodeSystemProvider.Create(FCommonTerminologies.FLanguages.link, FI18n.link, ffactory.link, cs.link);
          end;
        finally
          cs.free;
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
      result.checkReady;
      result.RecordUse;
    finally
      FLock.Unlock;
    end;
  end;

  if (result = nil) and not noException then
    if version <> '' then
      raise ETerminologySetup.Create('Unable to provide support for code system '+system+' v'+version)
    else
      raise ETerminologySetup.Create('Unable to provide support for code system '+system);
end;

procedure TTerminologyServerStore.checkCSLoaded(codesystem: TFHIRCodeSystemEntry);
var
  p : TFHIRResourceProxyV;
  state : integer; // go = 0, load = 1, wait for loading = 2, exception = 3
  msg : String;
begin
  FLock.Lock('checkCSLoaded');
  try
    case codesystem.LoadingState of
      cseNotLoaded : // first encounter
        begin
        state := 1;
        codesystem.LoadingState := cseLoading;
        end;
      cseLoading : // some other thread is loading it
        begin
        state := 2;
        end;
      cseLoaded:
        begin
        state := 0;
        end;
      cseLoadingFailed:
        begin
          state := 3;
          msg := codesystem.LoadingFailMessage;
        end;
    end;
  finally
    FLock.Unlock;
  end;
  case state of
    0: ; // nothing
    1:
      begin
      try
        msg := '';
        try        
          codesystem.CodeSystem := codeSystem.CodeSystemProxy.resourceW.link as TFHIRCodeSystemW;
          for p in codeSystem.SupplementProxies do
            codeSystem.Supplements.add(p.resourceW.link as TFHIRCodeSystemW);
        except
          on e : Exception do
            msg := e.Message;
        end;

      finally
        FLock.Lock('checkCSLoaded2');
        try
          if msg = '' then
            codesystem.LoadingState := cseLoaded
          else
          begin
            codesystem.LoadingState := cseLoadingFailed;
            codesystem.LoadingFailMessage := msg;
          end;
        finally
          FLock.Unlock;
        end;
        if (msg <> '') then
          raise ETerminologyError.create(msg, itException);
      end;
      end;
    2:
      begin
        repeat
          sleep(100);
          FLock.Lock('checkCSLoaded3');
          try        
            case codesystem.LoadingState of
              cseNotLoaded: raise ETerminologyError.create('Impossible State NotLoaded', itException);
              cseLoading : state := 0;
              cseLoaded: state := 1;
              cseLoadingFailed:raise ETerminologyError.create(codesystem.LoadingFailMessage, itException);
            end;
          finally
            FLock.Unlock;
          end;
        until state = 1;
      end;
    3: raise ETerminologyError.create(msg, itException);
  end;
end;

function TTerminologyServerStore.getProvider(codesystem: TFHIRCodeSystemW; profile: TFHIRTxOperationParams): TCodeSystemProvider;
begin
  checkVersion(codeSystem.url, codeSystem.version, profile);
  result := TFhirCodeSystemProvider.Create(FCommonTerminologies.FLanguages.link, FI18n.link, FFactory.link, TFHIRCodeSystemEntry.Create(codesystem.link));
end;

function TTerminologyServerStore.getProviderClasses: TFslMap<TCodeSystemProviderFactory>;
begin
  result := FCommonTerminologies.ProviderClasses;
end;

procedure TTerminologyServerStore.getSummary(b: TFslStringBuilder);
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

function TTerminologyServerStore.getValueSetByUrl(url, version : String; txResources : TFslList<TFHIRCachedMetadataResource> = nil) : TFHIRValueSetW;
var
  p :  TArray<String>;
  r : TFHIRCachedMetadataResource;
begin
 if txResources <> nil then
  begin
    for r in txResources do
      if (url <> '') and ((r.resource.url = url) or (r.resource.vurl = url)) and ((version = '') or (r.resource.version = version)) then
      begin
        if not (r.resource is TFHIRValueSetW) then
          raise EFHIRException.Create('Attempt to reference '+url+' as a ValueSet when it''s a '+r.resource.fhirType);
        exit(r.resource.link as TFHIRValueSetW);
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
  if result = nil then
    isKnownValueSet(url, result);
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

function TTerminologyServerStore.hasCodesystemUri(url: String; txResources : TFslList<TFHIRCachedMetadataResource> = nil): Boolean;
var
  r : TFHIRCachedMetadataResource;
begin
 if txResources <> nil then
  begin
    for r in txResources do
      if (url <> '') and ((r.resource.url = url) or (r.resource.vurl = url)) then
      begin
        if not (r.resource is TFhirCodeSystemW) then
          raise EFHIRException.Create('Attempt to reference '+url+' as a CodeSystem when it''s a '+r.resource.fhirType);
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
    ts.free;
  end;
end;

function TTerminologyServerStore.listSystemVersions: TArray<String>;
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
        ts.Add(p.systemUri+'|'+p.version);
      for cs in FCodeSystems.list do
        ts.Add(cs.url+'|'+cs.version);
    finally
      FLock.Unlock;
    end;
    SetLength(result, ts.Count);
    for i := 0 to ts.Count - 1 do
      result[i] := ts[i];
  finally
    ts.free;
  end;
end;

procedure TTerminologyServerStore.listVersions(url: String; list: TStringList);
var
  cs : TFHIRCodeSystemEntry;
  v : String;
begin
  FCommonTerminologies.listVersions(url, list);
  FLock.Lock('listVersions');
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
  FLock.Lock('NextConceptKey');
  try
    inc(FLastConceptKey);
    result := FLastConceptKey;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.NextValueSetKey: integer;
begin
  FLock.Lock('NextValueSetKey');
  try
    inc(FLastValueSetKey);
    result := FLastValueSetKey;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.NextValueSetMemberKey: integer;
begin
  FLock.Lock('NextValueSetMemberKey');
  try
    inc(FLastValueSetMemberKey);
    result := FLastValueSetMemberKey;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.NextClosureEntryKey: integer;
begin
  FLock.Lock('NextClosureEntryKey');
  try
    inc(FLastClosureEntryKey);
    result := FLastClosureEntryKey;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServerStore.NextClosureKey: integer;
begin
  FLock.Lock('NextClosureKey');
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
        loc := prov.locateIsA(nil, code2, code1);
        result := Loc <> nil;
        loc.free;
      finally
        prov.free;
      end;
    end;
  end;
end;


function TTerminologyServerStore.isKnownValueSet(id: String; out vs: TFhirValueSetW): Boolean;
var
  cs : TFhirCodeSystemW;
  sn : TSnomedServices;
begin
  vs := nil;
  if id.Contains('|') then
    id := id.Substring(0, id.IndexOf('|'));

  if (CommonTerminologies.DefSnomed <> nil) and (id.StartsWith('http://snomed.info/')) then
  begin
    vs := CommonTerminologies.DefSnomed.buildValueSet(Factory, id);
    if (vs = nil) then
    begin
      for sn in CommonTerminologies.Snomed do
      begin
        vs := sn.buildValueSet(Factory, id);
        if (vs <> nil) then
          break;
      end;
    end;
  end
  else if (CommonTerminologies.Loinc <> nil) and (id.StartsWith('http://loinc.org/vs/LP') or id.StartsWith('http://loinc.org/vs/LL')) then
    vs := CommonTerminologies.Loinc.buildValueSet(Factory, id)
  else if id = 'http://loinc.org/vs' then
    vs := CommonTerminologies.Loinc.buildValueSet(Factory, '')
  else if id.startsWith('https://fhir-terminology.ohdsi.org/ValueSet') then
    vs := CommonTerminologies.OMOP.buildValueSet(Factory, id)
  else if id = ANY_CODE_VS then
    vs := makeAnyValueSet
  else
  begin
    cs := getCodeSystemByValueSet(id);
    if (cs <> nil) then
    begin
      try
        vs := cs.buildImplicitValueSet;
      finally
        cs.free;
      end;
    end;
  end;

  result := vs <> nil;
end;

function TTerminologyServerStore.makeAnyValueSet: TFhirValueSetW;
var
  inc : TFhirValueSetComposeIncludeW;
begin
  result := Factory.wrapValueSet(factory.makeResource('ValueSet'));
  try
    result.url := ANY_CODE_VS;
    result.name := 'All codes known to the system';
    result.description := 'All codes known to the system';
    result.status := psActive;
    inc := result.addInclude;
    try
      // inc.systemUri := ALL_CODE_CS;
    finally
      inc.free;
    end;
    result.link;
  finally
    result.free;
  end;
end;


{ TCommonTerminologies }

function TCommonTerminologies.add(p: TCodeSystemProvider) : TCodeSystemProvider;
begin
  if p.version <> '' then
    FProviderClasses.Add(p.systemUri+URI_VERSION_BREAK+p.version, TCodeSystemProviderGeneralFactory.Create(p.link));
  if not FProviderClasses.ContainsKey(p.systemUri) then
    FProviderClasses.Add(p.systemUri, TCodeSystemProviderGeneralFactory.Create(p.link));
  result := p;
end;

procedure TCommonTerminologies.add(p: TCodeSystemProvider; defVer: boolean);
begin
  if p.version <> '' then
    FProviderClasses.Add(p.systemUri+URI_VERSION_BREAK+p.version, TCodeSystemProviderGeneralFactory.Create(p.link));
  if defVer and not FProviderClasses.ContainsKey(p.systemUri) then
    FProviderClasses.Add(p.systemUri, TCodeSystemProviderGeneralFactory.Create(p.link));
end;

procedure TCommonTerminologies.add(p: TCodeSystemProviderFactory; defVer: boolean);
begin
  if p.version <> '' then
    FProviderClasses.Add(p.systemUri+URI_VERSION_BREAK+p.version, p.link);
  if defVer or not FProviderClasses.ContainsKey(p.systemUri) then
    FProviderClasses.AddOrSetValue(p.systemUri, p.link);
end;

constructor TCommonTerminologies.Create(settings: TFHIRServerSettings);
begin
  inherited Create;
  FSettings := settings;
  FSnomed := TFslList<TSnomedServices>.Create;                        
  FI18n := i18n;
  loadLang;
end;

procedure TCommonTerminologies.defineFeatures(features: TFslList<TFHIRFeature>);
var
  sp : TSnomedProvider;
begin
  if FLoinc <> nil then
    FLoinc.defineFeatures(nil, features);
  if FDefSnomed <> nil then
  begin
    sp := TSnomedProvider.Create(FDefSnomed.link, FI18n.link, nil);
    try
      sp.defineFeatures(nil, features);
    finally
      sp.free;
    end;
  end;
  if FUcum <> nil then
    FUcum.defineFeatures(nil, features);
  if FRxNorm <> nil then
    FRxNorm.defineFeatures(nil, features);
  if FUnii <> nil then
    FUnii.defineFeatures(nil, features);
  if FCPT <> nil then
    FCPT.defineFeatures(nil, features);
  if FOMOP <> nil then
    FOMOP.defineFeatures(nil, features);
  if FACIR <> nil then
    FACIR.defineFeatures(nil, features);
  if FNDFRT <> nil then
    FNDFRT.defineFeatures(nil, features);
  if FNDC <> nil then
    FNDC.defineFeatures(nil, features);
end;

destructor TCommonTerminologies.Destroy;
begin                   
  FI18n.free;
  FProviderClasses.free;
  FNDFRT.free;
  FNDC.free;
  FSettings.free;
  FLoinc.free;
  FDefSnomed.free;
  FSnomed.free;
  FUnii.free;
  FCPT.free;
  FOMOP.free;
  FACIR.free;
  FUcum.free;
  FRxNorm.free;
  FLanguages.free;
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

procedure TCommonTerminologies.recordStats(rec: TStatusRecord);
var
  ss : TSnomedServices;
begin
  for ss in FSnomed do
    rec.SnomedsLoaded := rec.SnomedsLoaded + 1;
end;

procedure TCommonTerminologies.getSummary(b: TFslStringBuilder);
var
  sn : TSnomedServices;
  sp : TSnomedProvider;
begin
  if FLoinc = nil then
    b.append('<li>LOINC: not loaded</li>')
  else
    b.append('<li>LOINC: '+FLoinc.version+' ('+inttostr(FLoinc.UseCount)+' uses)');


  if FSnomed.Count = 0 then
    b.append('<li>Snomed: not loaded</li>')
  else for sn in FSnomed do
    b.append('<li>Snomed: '+sn.VersionUri+' ('+inttostr(sn.UseCount)+' uses)');

  if FUcum = nil then
    b.append('<li>Ucum: not loaded</li>')
  else
    b.append('<li>Ucum: '+FUcum.version+' ('+inttostr(FUcum.UseCount)+' uses)');

  if FRxNorm = nil then
    b.append('<li>RxNorm: not loaded</li>')
  else
    b.append('<li>RxNorm: '+FRxNorm.version+' ('+inttostr(FRxNorm.UseCount)+' uses)');

  if FNDFRT = nil then
    b.append('<li>NDFRT: not loaded</li>')
  else
    b.append('<li>NDFRT: '+NDFRT.version+' ('+inttostr(NDFRT.UseCount)+' uses)');

  if FUnii = nil then
    b.append('<li>Unii: not loaded</li>')
  else
    b.append('<li>Unii: '+FUnii.version+' ('+inttostr(FUnii.UseCount)+' uses)');

  if FCPT = nil then
    b.append('<li>CPT: not loaded</li>')
  else
    b.append('<li>CPT: '+FCPT.version+' ('+inttostr(FCPT.UseCount)+' uses)');

  if FOMOP = nil then
    b.append('<li>OMOP: not loaded</li>')
  else
    b.append('<li>OMOP: '+FOMOP.version+' ('+inttostr(FOMOP.UseCount)+' uses)');

  if FACIR = nil then
    b.append('<li>ACIR: not loaded</li>')
  else
    b.append('<li>ACIR: '+FACIR.version+' ('+inttostr(FACIR.UseCount)+' uses)');
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

procedure TCommonTerminologies.loadLang;
var
  s : String;
begin
  s := fixFile('lang', FSettings.LangFile);
  if not FileExists(s) then
    raise EFHIRException.Create('IETF language file "'+FSettings.LangFile+'" not found - necessary for server operation');
  FLanguages := TIETFLanguageDefinitions.Create(FileToString(s, TEncoding.ASCII));
end;

procedure TCommonTerminologies.load(txlist: TFHIRServerConfigSection; testing : boolean);
var
  tx : TFHIRServerConfigSection;
  s : string;
  sn: TSnomedServices;
  sp : TSnomedProviderFactory;
  def : boolean;
  p : TUriServices;

begin
  FProviderClasses := TFslMap<TCodeSystemProviderFactory>.Create('tc.common');

  p := TUriServices.Create(FLanguages.link, FI18n.link);
  try
    FProviderClasses.Add(p.systemUri, TCodeSystemProviderGeneralFactory.Create(p.link));
    FProviderClasses.Add(p.systemUri+URI_VERSION_BREAK+p.version, TCodeSystemProviderGeneralFactory.Create(p.link));
  finally
    p.free;
  end;

  add(TIETFLanguageCodeServices.Create(FLanguages.link, FI18n.link)).free;
  add(TACIRServices.Create(FLanguages.link, FI18n.link)).free;
  add(TAreaCodeServices.Create(FLanguages.link, FI18n.link)).free;
  add(TIso4217Services.Create(FLanguages.link, FI18n.link)).free;
  add(TMimeTypeCodeServices.Create(FLanguages.link, FI18n.link)).free;
  add(TCountryCodeServices.Create(FLanguages.link, FI18n.link)).free;
  add(TUSStateServices.Create(FLanguages.link, FI18n.link)).free;
  add(THGVSProvider.Create(FLanguages.link, FI18n.link)).free;

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
          sn.Load(fixFile('sct', tx['source'].value));
          sp := TSnomedProviderFactory.Create(sn.link, FI18n.link);
          try
            def := tx['default'].readAsBool;
            add(sp, def);
            if not FProviderClasses.ContainsKey(sn.systemUri()+URI_VERSION_BREAK+sn.EditionUri) or def then
              FProviderClasses.AddOrSetValue(sn.systemUri()+URI_VERSION_BREAK+sn.EditionUri, sp.link);
          finally
            sp.free;
          end;
          snomed.Add(sn.Link);
          if tx['default'].readAsBool then
            DefSnomed := sn.Link;
        finally
          sn.free;
        end;
      end
      else if tx['type'].value = 'loinc' then
      begin
        Logging.log('load '+s+' from '+tx['source'].value);
        Loinc := TLoincServices.Create(FLanguages.link, FI18n.link);
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
        Ucum := TUcumServices.Create(FLanguages.link, FI18n.link);
        Ucum.Import(fixFile('ucum', tx['source'].value));
      end
      else if tx['type'].value = 'rxnorm' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        RxNorm := TRxNormServices.Create(FLanguages.link, FI18n.link, connectToDatabase(tx, true))
      end
      else if tx['type'].value = 'ndc' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        NDC := TNDCServices.Create(FLanguages.link, FI18n.link, connectToDatabase(tx, true), tx['version'].value)
      end
      else if tx['type'].value = 'ndfrt' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        NDFRT := TNDFRTServices.Create(FLanguages.link, FI18n.link, connectToDatabase(tx, true))
      end
      else if tx['type'].value = 'unii' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        Unii := TUniiServices.Create(FLanguages.link, FI18n.link, connectToDatabase(tx, true))
      end
      else if tx['type'].value = 'cpt' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        CPT := TCPTServices.Create(FLanguages.link, FI18n.link, connectToDatabase(tx, true))
      end    
      else if tx['type'].value = 'omop' then
      begin
        Logging.log('load '+s+' from '+describeDatabase(tx));
        OMOP := TOMOPServices.Create(FLanguages.link, FI18n.link, connectToDatabase(tx, true))
      end           
      else
        raise EFslException.Create('Unknown type '+tx['type'].value);
    end;
  end;
end;

procedure TCommonTerminologies.SetLoinc(const Value: TLOINCServices);
begin
  FLoinc.free;
  FLoinc := Value;
end;

procedure TCommonTerminologies.SetCPT(AValue: TCPTServices);
begin
  if FCPT <> nil then
  begin
    FProviderClasses.Remove(FCPT.systemUri);
    FProviderClasses.Remove(FCPT.systemUri+URI_VERSION_BREAK+FCPT.version);
  end;
  FCPT.free;
  FCPT := AValue;
  if FCPT <> nil then
  begin
    FProviderClasses.add(FCPT.systemUri, TCodeSystemProviderGeneralFactory.Create(FCPT.Link));
    FProviderClasses.add(FCPT.systemUri+URI_VERSION_BREAK+FCPT.version, TCodeSystemProviderGeneralFactory.Create(FCPT.Link));
  end;
end;

procedure TCommonTerminologies.SetI18n(AValue: TI18nSupport);
begin
  FI18n.free;
  FI18n := AValue;
end;

procedure TCommonTerminologies.SetOMOP(AValue: TOMOPServices);
begin
  if FOMOP <> nil then
  begin
    FProviderClasses.Remove(FOMOP.systemUri);
    FProviderClasses.Remove(FOMOP.systemUri+URI_VERSION_BREAK+FOMOP.version);
  end;
  FOMOP.free;
  FOMOP := AValue;
  if FOMOP <> nil then
  begin
    FProviderClasses.add(FOMOP.systemUri, TCodeSystemProviderGeneralFactory.Create(FOMOP.Link));
    FProviderClasses.add(FOMOP.systemUri+URI_VERSION_BREAK+FOMOP.version, TCodeSystemProviderGeneralFactory.Create(FOMOP.Link));
  end;
end;

procedure TCommonTerminologies.SetRxNorm(const Value: TRxNormServices);
begin
  if FRxNorm <> nil then
  begin
    FProviderClasses.Remove(FRxNorm.systemUri);
    FProviderClasses.Remove(FRxNorm.systemUri+URI_VERSION_BREAK+FRxNorm.version);
  end;
  FRxNorm.free;
  FRxNorm := Value;
  if FRxNorm <> nil then
  begin
    FProviderClasses.add(FRxNorm.systemUri, TCodeSystemProviderGeneralFactory.Create(FRxNorm.Link));
    FProviderClasses.add(FRxNorm.systemUri+URI_VERSION_BREAK+FRxNorm.version, TCodeSystemProviderGeneralFactory.Create(FRxNorm.Link));
  end;
end;

procedure TCommonTerminologies.SetNDC(const Value: TNDCServices);
begin
  if FNDC <> nil then
  begin
    FProviderClasses.Remove(FNDC.systemUri);
    FProviderClasses.Remove(FNDC.systemUri+URI_VERSION_BREAK+FNDC.version);
  end;
  FNDC.free;
  FNDC := Value;
  if FNDC <> nil then
  begin
    FProviderClasses.add(FNDC.systemUri, TCodeSystemProviderGeneralFactory.Create(FNDC.Link));
    FProviderClasses.add(FNDC.systemUri+URI_VERSION_BREAK+FNDC.version, TCodeSystemProviderGeneralFactory.Create(FNDC.Link));
  end;
end;

procedure TCommonTerminologies.SetNDFRT(const Value: TNDFRTServices);
begin
  if FNDFRT <> nil then
  begin
    FProviderClasses.Remove(FNDFRT.systemUri);
    FProviderClasses.Remove(FNDFRT.systemUri+URI_VERSION_BREAK+FNDFRT.version);
  end;
  FNDFRT.free;
  FNDFRT := Value;
  if FNDFRT <> nil then
  begin
    FProviderClasses.add(FNDFRT.systemUri, TCodeSystemProviderGeneralFactory.Create(FNDFRT.Link));
    FProviderClasses.add(FNDFRT.systemUri+URI_VERSION_BREAK+FNDFRT.version, TCodeSystemProviderGeneralFactory.Create(FNDFRT.Link));
  end;
end;

procedure TCommonTerminologies.SetUnii(const Value: TUniiServices);
begin
  if FUnii <> nil then
  begin
    FProviderClasses.Remove(FUnii.systemUri);
    FProviderClasses.Remove(FUnii.systemUri+URI_VERSION_BREAK+FUnii.version);
  end;
  FUnii.free;
  FUnii := Value;
  if FUnii <> nil then
  begin
    FProviderClasses.add(FUnii.systemUri, TCodeSystemProviderGeneralFactory.Create(FUnii.Link));
    FProviderClasses.add(FUnii.systemUri+URI_VERSION_BREAK+FUnii.version, TCodeSystemProviderGeneralFactory.Create(FUnii.Link));
  end;
end;

procedure TCommonTerminologies.SetACIR(const Value: TACIRServices);
begin
  if FACIR <> nil then
  begin
    FProviderClasses.Remove(FACIR.systemUri);
    FProviderClasses.Remove(FACIR.systemUri+URI_VERSION_BREAK+FACIR.version);
  end;
  FACIR.free;
  FACIR := Value;
  if FACIR <> nil then
  begin
    FProviderClasses.add(FACIR.systemUri, TCodeSystemProviderGeneralFactory.Create(FACIR.Link));
    FProviderClasses.add(FACIR.systemUri+URI_VERSION_BREAK+FACIR.version, TCodeSystemProviderGeneralFactory.Create(FACIR.Link));
  end;
end;

procedure TCommonTerminologies.SetDefSnomed(const Value: TSnomedServices);
begin
  FDefSnomed.free;
  FDefSnomed := Value;
end;

procedure TCommonTerminologies.SetUcum(const Value: TUcumServices);
begin
  if FUcum <> nil then
  begin
    FProviderClasses.Remove(FUcum.systemUri);
    FProviderClasses.Remove(FUcum.systemUri+URI_VERSION_BREAK+FUcum.version);
  end;
  FUcum.free;
  FUcum := Value;
  if FUcum <> nil then
  begin
    FProviderClasses.add(FUcum.systemUri, TCodeSystemProviderGeneralFactory.Create(FUcum.Link));
    FProviderClasses.add(FUcum.systemUri+URI_VERSION_BREAK+FUcum.version, TCodeSystemProviderGeneralFactory.Create(FUcum.Link));
  end;
end;

{ TCodeSystemProviderGeneralFactory }

constructor TCodeSystemProviderGeneralFactory.Create(provider: TCodeSystemProvider);
begin
  inherited Create;
  FProvider := provider;
end;

destructor TCodeSystemProviderGeneralFactory.Destroy;
begin
  FProvider.free;
  inherited Destroy;
end;

function TCodeSystemProviderGeneralFactory.getProvider: TCodeSystemProvider;
begin
  result := FProvider.link;
end;

function TCodeSystemProviderGeneralFactory.systemUri: String;
begin
  result := FProvider.systemUri;
end;

function TCodeSystemProviderGeneralFactory.version: String;
begin
  result := FProvider.version;
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
  result := FProvider.version;
end;

function TCodeSystemProviderGeneralFactory.description: String;
begin
  result := FProvider.description;
end;

{ TSnomedProviderFactory }

constructor TSnomedProviderFactory.Create(snomed: TSnomedServices; i18n : TI18nSupport);
begin
  inherited Create;
  FSnomed := snomed;
  FI18n := i18n;
end;

destructor TSnomedProviderFactory.Destroy;
begin
  FI18n.free;
  FSnomed.free;
  inherited Destroy;
end;

function TSnomedProviderFactory.getProvider: TCodeSystemProvider;
begin
  result := TSnomedProvider.Create(FSnomed.Link, FI18n.link, nil);
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

