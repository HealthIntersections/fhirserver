unit fhir_codesystem_service;

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
  SysUtils, Classes, Generics.Defaults, Generics.Collections,  
  fsl_base, fsl_utilities, fsl_collections, fsl_http, fsl_lang, fsl_versions, fsl_fpc, fsl_logging, fsl_regex, fsl_i18n, fsl_threads,
  fhir_objects, fhir_factory, fhir_common, fhir_cdshooks,  fhir_utilities, fhir_features, fhir_uris,
  ftx_service;

type
  TFhirCodeSystemProviderContext = class (TCodeSystemProviderContext)
  private
    concept : TFhirCodeSystemConceptW;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(concept : TFhirCodeSystemConceptW); overload;
    destructor Destroy; override;
  end;

  TFhirCodeSystemConceptMatch = class (TFslObject)
  private
    FItem : TFhirCodeSystemConceptW;
    FRating : double;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(item : TFhirCodeSystemConceptW; rating : double);
    destructor Destroy; override;
  end;

  { TFHIRCodeSystemCodeEntry }

  TFHIRCodeSystemCodeEntry = class (TFslObject)
  private
    FConcept: TFhirCodeSystemConceptW;
    FParents: TFslList<TFHIRCodeSystemCodeEntry>;
    FChildren: TFslList<TFHIRCodeSystemCodeEntry>;
    procedure addParent(p : TFHIRCodeSystemCodeEntry);
    procedure addChild(p : TFHIRCodeSystemCodeEntry);
    procedure clearRelations;
  public
    constructor Create(Concept : TFhirCodeSystemConceptW);
    destructor Destroy; override;
    function link: TFHIRCodeSystemCodeEntry; overload;

    property Concept : TFhirCodeSystemConceptW read FConcept; 
    property parents : TFslList<TFHIRCodeSystemCodeEntry> read FParents;
    function hasParents : boolean;
    property children : TFslList<TFHIRCodeSystemCodeEntry> read FChildren;
    function hasChildren : boolean;
  end;

  { TFHIRCodeSystemEntry }

  TFHIRCodeSystemEntryLoadingState = (cseNotLoaded, cseLoading, cseLoaded, cseLoadingFailed);

  TFHIRCodeSystemEntry = class (TFslObject)
  private
    FLoadingFailMessage: String;
    // we get the proxy, and maybe we load this into the loaded one when we need to
    // makes start up very very much faster
    FLoadingState : TFHIRCodeSystemEntryLoadingState;
    FCodeSystemProxy : TFHIRResourceProxyV;
    FSupplementProxies : TFslList<TFHIRResourceProxyV>;

    FCodeSystem : TFHIRCodeSystemW;
    FSupplements : TFslList<TFHIRCodeSystemW>;

    FCodeMap : TFslMap<TFHIRCodeSystemCodeEntry>;
    procedure loadCodeSystem;
    procedure clearCodeMap;
    function uc(code : String) : String;

    function GetHasSupplements: boolean;
    function GetSupplementProxies: TFslList<TFHIRResourceProxyV>;
    function GetSupplements: TFslList<TFHIRCodeSystemW>;
    procedure SetCodeSystem(const Value: TFHIRCodeSystemW);
    function GetUrl: String;

    function GetVersion: String;
    function GetId: String;
    procedure SetId(const Value: String);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(res : TFHIRResourceProxyV); overload;
    constructor Create(res : TFHIRCodeSystemW); overload;
    constructor Create(source : TFHIRCodeSystemEntry); overload;
    destructor Destroy; override;

    property LoadingState : TFHIRCodeSystemEntryLoadingState read FLoadingState write FLoadingState;
    property LoadingFailMessage : String read FLoadingFailMessage write FLoadingFailMessage;
    function Link : TFHIRCodeSystemEntry; overload;
    property id : String read GetId write SetId;
    property url : String read GetUrl;
    property version : String read GetVersion;

    function getCode(code : String) : TFhirCodeSystemConceptW;
    property CodeSystemProxy : TFHIRResourceProxyV read FCodeSystemProxy;
    property SupplementProxies : TFslList<TFHIRResourceProxyV> read GetSupplementProxies;

    property CodeSystem : TFHIRCodeSystemW read FCodeSystem write SetCodeSystem;
    Property hasSupplements : boolean read GetHasSupplements;
    property Supplements : TFslList<TFHIRCodeSystemW> read GetSupplements;
  end;

 TFHIRCodeSystemManager = class (TFslObject)
  private
    FMap : TFslMap<TFHIRCodeSystemEntry>;
    FList : TFslList<TFHIRCodeSystemEntry>;
    {$IFDEF FPC}
    function sort(sender : TObject; const L, R: TFHIRCodeSystemEntry): Integer;
    {$ENDIF}
    procedure updateList(url, version: String);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function link : TFHIRCodeSystemManager; overload;
    function clone : TFHIRCodeSystemManager; overload;
    procedure Assign(oSource : TFslObject); override;

    Property list : TFslList<TFHIRCodeSystemEntry> read FList;

    procedure see(r: TFHIRCodeSystemEntry);
    procedure drop(id : String);
    function get(url: String): TFHIRCodeSystemEntry; overload;
    function get(url, version: String): TFHIRCodeSystemEntry; overload;
    function has(url: String): boolean; overload;
    function has(url, version: String): boolean; overload;
    function has(url: String; var res : TFHIRCodeSystemEntry): boolean; overload;
    function has(url, version: String; var res : TFHIRCodeSystemEntry): boolean; overload;
    function count: integer;
    procedure clear;
    procedure listAll(list: TFslList<TFHIRCodeSystemW>);
    procedure listAllM(list: TFslMetadataResourceList);
  end;

  TFhirCodeSystemProviderFilterSorter = class (TFslComparer<TFhirCodeSystemConceptMatch>)
  public
    function Compare(const Left, Right: TFhirCodeSystemConceptMatch): Integer; override;
  end;

  TFhirCodeSystemProviderFilterContext = class (TCodeSystemProviderFilterContext)
  private
    ndx : integer;
    include : boolean;
    concepts : TFslList<TFhirCodeSystemConceptMatch>;

    procedure Add(item : TFhirCodeSystemConceptW; rating : double);
    procedure sort;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
  end;

  TCodeSystemCodeFilterProc = {$IFNDEF FPC}reference to {$ENDIF}function (context : pointer; concept : TFhirCodeSystemConceptW) : boolean;

function allCodes(context : pointer; concept : TFhirCodeSystemConceptW) : boolean;
function leafCodes(context : pointer; concept : TFhirCodeSystemConceptW) : boolean;
function nonLeafCodes(context : pointer; concept : TFhirCodeSystemConceptW) : boolean;

type

  { TFhirCodeSystemProvider }

  TFhirCodeSystemProvider = class (TCodeSystemProvider)
  private
    FCs : TFhirCodeSystemEntry;
    FFactory : TFHIRFactory;
    FHasHeirarchy : boolean;

    function LocateCode(code : String; altOpt : TAlternateCodeOptions) : TFhirCodeSystemConceptW;
    function doLocate(code : String; altOpt : TAlternateCodeOptions) : TFhirCodeSystemProviderContext; overload;
    function doLocate(list : TFhirCodeSystemConceptListW; code : String; altOpt : TAlternateCodeOptions) : TFhirCodeSystemProviderContext; overload;
    function getParent(ctxt : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptW;
    procedure FilterCodes(dest : TFhirCodeSystemProviderFilterContext; source : TFhirCodeSystemConceptListW; filter : TSearchFilterText);
    procedure iterateCodes(opContext : TTxOperationContext; op : String; base: TFhirCodeSystemConceptW; list: TFhirCodeSystemProviderFilterContext; filter : TCodeSystemCodeFilterProc; context : pointer; includeRoot : boolean; exception : TFhirCodeSystemConceptW = nil);
    function locateParent(ctxt: TFHIRCodeSystemConceptW; code: String): String;
    function locCode(list: TFhirCodeSystemConceptListW; code, synonym: String; altOpt : TAlternateCodeOptions): TFhirCodeSystemConceptW;
    function getProperty(code : String) : TFhirCodeSystemPropertyW;
    function hasPropForCode(code : String) : boolean;
    function conceptHasProperty(concept : TFhirCodeSystemConceptW; url : String; value : string) : boolean;
    procedure iterateConceptsByProperty(src : TFhirCodeSystemConceptListW; pp : TFhirCodeSystemPropertyW; values: TStringArray; list: TFhirCodeSystemProviderFilterContext; include : boolean);
    procedure iterateConceptsByPropertyRegex(src : TFhirCodeSystemConceptListW; pp : TFhirCodeSystemPropertyW; regex: TRegularExpression; list: TFhirCodeSystemProviderFilterContext; include : boolean);
    procedure iterateConceptsByKnownProperty(src : TFhirCodeSystemConceptListW; code : String; values: TStringArray; List: TFhirCodeSystemProviderFilterContext; include : boolean);
    procedure iterateConceptsByRegex(src : TFhirCodeSystemConceptListW; regex: TRegularExpression; list: TFhirCodeSystemProviderFilterContext);
    procedure iterateConceptsByEquality(positive : boolean; src : TFhirCodeSystemConceptListW; code: string; list: TFhirCodeSystemProviderFilterContext);

    procedure listChildrenByProperty(opContext : TTxOperationContext; op : String; code : String; list, children : TFhirCodeSystemConceptListW);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; factory : TFHIRFactory; cs : TFhirCodeSystemEntry); overload;
    destructor Destroy; override;
    function checkCodeSystem(langs : THTTPLanguageList; var msg : String) : boolean; override;

    function cloneWithSupplements(supplements : TFslList<TFhirCodeSystemW>) : TFhirCodeSystemProvider; override;

    function contentMode : TFhirCodeSystemContentMode; override;
    function sourcePackage : String; override;
    function description : String; override;
    function name(context: TCodeSystemProviderContext): String; override;
    function version(): String; override;
    function versionAlgorithm() : TFHIRVersionAlgorithm; override;
    function TotalCount : integer; override;
    function getPropertyDefinitions : TFslList<TFhirCodeSystemPropertyW>; override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri() : String; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; overload; override;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function IsInactive(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function getCodeStatus(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : String; override;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function itemWeight(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function deprecated(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function getExtensions(opContext : TTxOperationContext; context : TCodeSystemProviderContext)  : TFslList<TFHIRExtensionW>; override;
    function getProperties(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TFslList<TFhirCodeSystemConceptPropertyW>; override;
    function parent(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : String; override;
    function listCodes(opContext : TTxOperationContext; ctxt : TCodeSystemProviderContext; altOpt : TAlternateCodeOptions) : TStringArray; override;
    function canParent : boolean; override;
    function hasAnyDisplays(langs : THTTPLanguageList) : boolean; override;
    function versionIsMoreDetailed(v1, v2 : String): boolean; override;

    function hasSupplement(opContext : TTxOperationContext; url : String) : boolean; override;
    procedure listSupplements(opContext : TTxOperationContext; ts : TStringList); override;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowSelf : boolean = false) : TCodeSystemProviderContext; override;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(opContext : TTxOperationContext; card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); override;
    procedure extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    function subsumesTest(opContext : TTxOperationContext; codeA, codeB : String) : String; override;
    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
    procedure getStatus(out status: TPublicationStatus; out standardsStatus: String; out experimental : boolean); override;

  end;

implementation

{ TFhirCodeSystemProviderContext }

constructor TFhirCodeSystemProviderContext.Create(concept: TFhirCodeSystemConceptW);
begin
  inherited Create;
  self.concept := concept;
end;

destructor TFhirCodeSystemProviderContext.Destroy;
begin
  concept.free;
  inherited;
end;

function TFhirCodeSystemProviderContext.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, concept.sizeInBytes(magic));
end;

{ TFhirCodeSystemConceptMatch }

constructor TFhirCodeSystemConceptMatch.Create(item: TFhirCodeSystemConceptW; rating: double);
begin
  inherited Create;
  FItem := item;
  FRating := rating;
end;

destructor TFhirCodeSystemConceptMatch.Destroy;
begin
  FItem.free;
  inherited;
end;

function TFhirCodeSystemConceptMatch.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FItem.sizeInBytes(magic));
end;

{ TFHIRCodeSystemEntry }

constructor TFHIRCodeSystemEntry.Create(res : TFHIRResourceProxyV);
begin
  inherited Create;
  FCodeSystemProxy := res;
  FLoadingState := cseNotLoaded;
end;

constructor TFHIRCodeSystemEntry.Create(res: TFHIRCodeSystemW);
begin
  inherited Create;
  FCodeSystem := res;
  LoadCodeSystem;
  FLoadingState := cseLoaded;
end;

constructor TFHIRCodeSystemEntry.Create(source: TFHIRCodeSystemEntry);
begin              
  inherited Create;
  if (source.FLoadingState <> cseLoaded) then
    raise ETerminologyError.create('unable to clone Code System Entry '+source.GetUrl+'#'+source.GetVersion);
  FCodeSystem := source.FCodeSystem.link;
  FSupplements := TFslList<TFHIRCodeSystemW>.create;
  if (source.FSupplements <> nil) then
    FSupplements.AddAll(source.FSupplements);
  FCodeMap := source.FCodeMap.link;
end;

destructor TFHIRCodeSystemEntry.Destroy;
begin
  clearCodeMap;
  FCodeMap.Free;
  FCodeSystemProxy.Free;
  FSupplementProxies.Free;
  FCodeSystem.free;
  FSupplements.free;
  inherited;
end;

procedure TFHIRCodeSystemEntry.loadCodeSystem;
  procedure registerCodes(list : TFhirCodeSystemConceptListW; parent : TFHIRCodeSystemCodeEntry);
  var
    item : TFhirCodeSystemConceptW;
    entry : TFHIRCodeSystemCodeEntry;
  begin
    for item in list do
    begin
      if not FCodeMap.ContainsKey(uc(item.code)) then
      begin
        entry := TFHIRCodeSystemCodeEntry.create(item.link);
        try
          FCodeMap.AddOrSetValue(uc(item.code), entry.link);
          item.TagNoLink := entry;
          if (parent <> nil) then
          begin
            entry.addParent(parent.link);
            parent.addChild(entry.link);
          end;
          if (item.HasConcepts) then
            registerCodes(item.conceptList, entry);
        finally
          entry.free;
        end;
      end;
    end;
  end;

  procedure indexCodes(list : TFhirCodeSystemConceptListW; stemmer : TFslWordStemmer);
  var
    item : TFhirCodeSystemConceptW;
    dd : TFhirCodeSystemConceptDesignationW;
    stems : TFslStringList;
  begin
    for item in list do
    begin
      stems := TFslStringList.create;
      try
        item.tag := stems.link;
        stems.IgnoreDuplicates;
        stemmer.stems(item.display, stems, FCodeSystem.language, '');
        for dd in item.designations.forEnum do
          stemmer.stems(dd.value, stems, dd.language, FCodeSystem.language);
      finally
        stems.free;
      end;
      if (item.HasConcepts) then
        indexCodes(item.conceptList, stemmer);
    end;
  end;
var
  prop : TFHIRCodeSystemPropertyW;
  entry : TFHIRCodeSystemCodeEntry;
  p : TFhirCodeSystemConceptPropertyW;
  c : TFHIRCodeSystemCodeEntry;
  stemmer : TFslWordStemmer;
begin
  Assert(FCodeMap = nil);

  FCodeMap := TFslMap<TFHIRCodeSystemCodeEntry>.create;
  FCodeMap.defaultValue := nil;
  registerCodes(FCodeSystem.conceptList, nil);
  stemmer := TFslWordStemmer.create;
  try
    indexCodes(FCodeSystem.conceptList, stemmer);
  finally
    stemmer.free;
  end;
  for prop in FCodeSystem.properties.forEnum do
  begin
    if (prop.code = 'parent') or (prop.uri = 'http://hl7.org/fhir/concept-properties#parent') then
    begin
      for entry in FCodeMap.Values do
      begin
        for p in entry.Concept.properties.forEnum do
        begin
          if (p.code = prop.code) and (p.value <> nil) and (p.value.isPrimitive) then
          begin
            c := FCodeMap[p.value.primitiveValue];
            if (c <> nil) then
            begin
              entry.addParent(c.link);
              c.addChild(entry.link);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFHIRCodeSystemEntry.clearCodeMap;
var
  t : TFHIRCodeSystemCodeEntry;
begin
  for t in FCodeMap.Values do
    t.clearRelations;
end;

function TFHIRCodeSystemEntry.uc(code: String): String;
begin
  if FCodeSystem.caseSensitive then
    result := code
  else
    result := code.toUpper;
end;

function TFHIRCodeSystemEntry.GetHasSupplements: boolean;
begin
  result := ((FSupplements <> nil) and (FSupplements.Count > 0)) or ((FSupplementProxies <> nil) and (FSupplementProxies.Count > 0));
end;

function TFHIRCodeSystemEntry.GetId: String;
begin        
  if FCodeSystemProxy <> nil then
    result := FCodeSystemProxy.id
  else
    result := FCodeSystem.id;
end;

procedure TFHIRCodeSystemEntry.SetId(const Value: String);
begin
  if FCodeSystemProxy <> nil then
    FCodeSystemProxy.id := value;
  if FCodeSystem <> nil then
    FCodeSystem.id := value;
end;

function TFHIRCodeSystemEntry.GetSupplementProxies: TFslList<TFHIRResourceProxyV>;
begin
  if FSupplementProxies = nil then
    FSupplementProxies := TFslList<TFHIRResourceProxyV>.Create;
  result := FSupplementProxies;
end;

function TFHIRCodeSystemEntry.GetSupplements: TFslList<TFHIRCodeSystemW>;
begin
  if FSupplements = nil then
    FSupplements := TFslList<TFHIRCodeSystemW>.Create;
  result := FSupplements;
end;

procedure TFHIRCodeSystemEntry.SetCodeSystem(const Value: TFHIRCodeSystemW);
begin
  FCodeSystem.free;
  FCodeSystem := value;
  if FCodeSystem <> nil then
    LoadCodeSystem;
end;

function TFHIRCodeSystemEntry.GetUrl: String;
begin
  if FCodeSystemProxy <> nil then
    result := FCodeSystemProxy.url
  else
    result := FCodeSystem.url;
end;

function TFHIRCodeSystemEntry.GetVersion: String;
begin
  if FCodeSystemProxy <> nil then
    result := FCodeSystemProxy.version
  else
    result := FCodeSystem.version;
end;

function TFHIRCodeSystemEntry.Link: TFHIRCodeSystemEntry;
begin
  result := TFHIRCodeSystemEntry(inherited Link);
end;

function TFHIRCodeSystemEntry.getCode(code: String): TFhirCodeSystemConceptW;
var
  entry : TFHIRCodeSystemCodeEntry;
begin
  result := nil;
  entry := FCodeMap[code];
  if (entry <> nil) then
    result := entry.FConcept;

end;

function TFHIRCodeSystemEntry.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FCodeSystem.sizeInBytes(magic));
  inc(result, FSupplements.sizeInBytes(magic));
  inc(result, FCodeSystemProxy.sizeInBytes(magic));
  inc(result, FCodeMap.sizeInBytes(magic));
  inc(result, FSupplementProxies.sizeInBytes(magic));
end;

{ TFhirCodeSystemProviderFilterSorter }

function TFhirCodeSystemProviderFilterSorter.Compare(const Left, Right: TFhirCodeSystemConceptMatch): Integer;
begin
  if right.FRating > left.FRating then
    result := 1
  else if left.FRating > right.FRating then
    result := -1
  else
    result := 0;
end;

{ TFhirCodeSystemProviderFilterContext }

procedure TFhirCodeSystemProviderFilterContext.Add(item: TFhirCodeSystemConceptW; rating : double);
begin
  concepts.Add(TFhirCodeSystemConceptMatch.Create(item, rating));
end;

constructor TFhirCodeSystemProviderFilterContext.Create;
begin
  inherited Create;
  self.concepts := TFslList<TFhirCodeSystemConceptMatch>.Create;
  ndx := -1;
end;

destructor TFhirCodeSystemProviderFilterContext.Destroy;
begin
  concepts.free;
  inherited;
end;

procedure TFhirCodeSystemProviderFilterContext.sort;
begin
  concepts.sort(TFhirCodeSystemProviderFilterSorter.Create);
end;

function TFhirCodeSystemProviderFilterContext.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, concepts.sizeInBytes(magic));
end;

{ TFHIRCodeSystemCodeEntry }

procedure TFHIRCodeSystemCodeEntry.addParent(p: TFHIRCodeSystemCodeEntry);
begin
  if FParents = nil then
    FParents := TFslList<TFHIRCodeSystemCodeEntry>.create;
  FParents.add(p);
end;

procedure TFHIRCodeSystemCodeEntry.addChild(p: TFHIRCodeSystemCodeEntry);
begin
  if FChildren = nil then
    FChildren := TFslList<TFHIRCodeSystemCodeEntry>.create;
  FChildren.add(p);
end;

procedure TFHIRCodeSystemCodeEntry.clearRelations;
begin
  if FParents <> nil then
    FParents.clear;
  if FChildren <> nil then
    FChildren.clear;
end;

constructor TFHIRCodeSystemCodeEntry.Create(Concept: TFhirCodeSystemConceptW);
begin
  inherited Create;
  FConcept := Concept;
  FParents := nil;
  FChildren := nil;
end;

destructor TFHIRCodeSystemCodeEntry.Destroy;
begin
  FConcept.free;
  FParents.Free;
  FChildren.free;
  inherited Destroy;
end;

function TFHIRCodeSystemCodeEntry.link: TFHIRCodeSystemCodeEntry;
begin
  result := TFHIRCodeSystemCodeEntry(inherited Link);
end;

function TFHIRCodeSystemCodeEntry.hasParents: boolean;
begin
  result := FParents <> nil;
end;

function TFHIRCodeSystemCodeEntry.hasChildren: boolean;
begin
  result := FChildren <> nil;
end;

{ TFhirCodeSystemProvider }

constructor TFhirCodeSystemProvider.Create(languages: TIETFLanguageDefinitions; i18n : TI18nSupport; factory: TFHIRFactory; cs: TFhirCodeSystemEntry);
var
  cc : TFhirCodeSystemConceptW;
begin
  Create(languages, i18n);
  FCs := cs;
  FFactory := factory;
  if FCs.CodeSystem.language <> '' then
    setDefLang(FLanguages.parse(FCs.CodeSystem.language));
  for cc in FCs.CodeSystem.conceptList do
    if cc.hasConcepts then
    begin
      FHasHeirarchy := true;
      break;
    end;
end;

procedure TFhirCodeSystemProvider.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'concept:is-a'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'concept:is-not-a'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'concept:in'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'child:exists'));
end;

procedure TFhirCodeSystemProvider.getStatus(out status: TPublicationStatus; out standardsStatus: String; out experimental : boolean);
begin
  status := FCs.CodeSystem.status;
  standardsStatus := FCs.CodeSystem.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-standards-status');
  experimental := FCs.CodeSystem.experimental;
end;

function TFhirCodeSystemProvider.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := TFhirCodeSystemProviderContext(context).concept.definition;
end;

function TFhirCodeSystemProvider.itemWeight(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  Result := TFhirCodeSystemProviderContext(context).concept.itemWeight;
end;

function TFhirCodeSystemProvider.deprecated(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  Result := FCs.CodeSystem.isDeprecated(TFhirCodeSystemProviderContext(context).concept);
end;

function TFhirCodeSystemProvider.getExtensions(opContext : TTxOperationContext; context: TCodeSystemProviderContext): TFslList<TFHIRExtensionW>;
var
  ctxt : TFhirCodeSystemProviderContext;
  ext : TFHIRExtensionW;
  css : TFhirCodeSystemW;
  cc : TFhirCodeSystemConceptW;
begin
  ctxt := context as TFhirCodeSystemProviderContext;

  result := TFslList<TFHIRExtensionW>.Create;
  try
    for ext in ctxt.concept.getAllExtensionsW.forEnum do
      result.add(ext.link);

    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, ctxt.concept.code, css.propertyCode('http://hl7.org/fhir/concept-properties#alternateCode'), nil);
      if (cc <> nil) then
        for ext in cc.getAllExtensionsW.forEnum do
          result.add(ext.link);
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFhirCodeSystemProvider.getProperties(opContext : TTxOperationContext; context: TCodeSystemProviderContext): TFslList<TFhirCodeSystemConceptPropertyW>;
var
  ctxt : TFhirCodeSystemProviderContext;
  cp : TFhirCodeSystemConceptPropertyW;
  css : TFhirCodeSystemW;
  cc : TFhirCodeSystemConceptW;
begin
  ctxt := context as TFhirCodeSystemProviderContext;

  result := TFslList<TFhirCodeSystemConceptPropertyW>.Create;
  try
    for cp in ctxt.concept.properties.forEnum do
      result.add(cp.link);

    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, ctxt.concept.code, css.propertyCode('http://hl7.org/fhir/concept-properties#alternateCode'), nil);
      if (cc <> nil) then
        for cp in cc.properties.forEnum do
          result.add(cp.link);
    end;
    result.link;
  finally
    result.free;
  end;
end;


function TFhirCodeSystemProvider.locateParent(ctxt : TFHIRCodeSystemConceptW; code : String): String;
var
  c : TFHIRCodeSystemConceptW;
  pc : String;
begin
  result := '';
  for c in ctxt.conceptList do
  begin
    if c.code = code then
      exit(ctxt.code)
    else
    begin
      pc := locateParent(c, code);
      if (pc <> '') then
        exit(pc);
    end;
  end;
end;

function TFhirCodeSystemProvider.parent(opContext : TTxOperationContext; context: TCodeSystemProviderContext): String;
var
  ctxt : TFhirCodeSystemProviderContext;
  c : TFHIRCodeSystemConceptW;
  code, pc : String;
begin
  ctxt := context as TFhirCodeSystemProviderContext;
  code := ctxt.concept.code;
  result := '';
  for c in FCs.CodeSystem.conceptList do
  begin
    if c.code = code then
      exit('')
    else
    begin
      pc := locateParent(c, code);
      if (pc <> '') then
        exit(pc);
    end;
  end;
end;

function TFhirCodeSystemProvider.listCodes(opContext : TTxOperationContext; ctxt: TCodeSystemProviderContext; altOpt: TAlternateCodeOptions): TStringArray;
var
  c : TFHIRCodeSystemConceptW;
  p : TFhirCodeSystemConceptPropertyW;
  code : String;
begin
  c := (ctxt as TFhirCodeSystemProviderContext).concept;
  SetLength(result, 1);
  result[0] := c.code;

  if (altOpt <> nil) then
  begin
    for p in c.properties.forEnum do
    begin
      code := p.code;
      if (code = 'alternateCode') and altOpt.passes(p) then
      begin
        setLength(result, length(result)+1);
        result[length(result)-1] := p.value.primitiveValue
      end;
    end;
  end;
end;

function TFhirCodeSystemProvider.canParent: boolean;
begin
  Result := FHasHeirarchy;
end;

function TFhirCodeSystemProvider.hasAnyDisplays(langs: THTTPLanguageList): boolean;
begin
  result := fcs.CodeSystem.hasAnyDisplays(langs);
end;

function TFhirCodeSystemProvider.versionIsMoreDetailed(v1, v2: String): boolean;
begin
  result := TFHIRVersions.versionMatches(FCs.CodeSystem.versionAlgorithm, v1, v2);
end;

function TFhirCodeSystemProvider.description: String;
begin
  result := fcs.CodeSystem.name;
end;

destructor TFhirCodeSystemProvider.Destroy;
begin
  FCs.free;
  FFactory.free;
  inherited;
end;

function TFhirCodeSystemProvider.checkCodeSystem(langs: THTTPLanguageList; var msg : String) : boolean;
begin
  result := false;
  if (FCs.CodeSystem.supplements <> '') or (FCs.CodeSystem.content = cscmSupplement) then
    msg := FI18n.translate('CODESYSTEM_CS_NO_SUPPLEMENT', langs, [FCs.CodeSystem.vurl])
  else
    result := true;
end;

function TFhirCodeSystemProvider.cloneWithSupplements(supplements: TFslList<TFhirCodeSystemW>): TFhirCodeSystemProvider;
var
  cs : TFHIRCodeSystemEntry;
begin
  cs := TFHIRCodeSystemEntry.create(FCs);
  try
    cs.Supplements.AddAll(supplements);
    result := TFhirCodeSystemProvider.create(FLanguages.link, FI18n.link, FFactory.link, cs.link);
  finally
    cs.free;
  end;
end;

function {TFhirCodeSystemProvider.}allCodes(context : pointer; concept: TFhirCodeSystemConceptW): boolean;
begin
  result := true;
end;

function TFhirCodeSystemProvider.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  if context = nil then
    result := TCodeSystemIteratorContext.Create(nil, FCs.CodeSystem.conceptCount)
  else
    result := TCodeSystemIteratorContext.Create(context.Link, TFhirCodeSystemProviderContext(context).concept.conceptCount + TFhirCodeSystemProviderContext(context).concept.extensionCount('http://hl7.org/fhir/StructureDefinition/codesystem-subsumes'));
end;

function TFhirCodeSystemProvider.Code(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := TFhirCodeSystemProviderContext(context).concept.code;
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

procedure TFhirCodeSystemProvider.getCDSInfo(opContext : TTxOperationContext; card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
var
  b : TFslStringBuilder;
  ctxt : TFhirCodeSystemProviderContext;
  concept, c : TFhirCodeSystemConceptW;
  d : TFhirCodeSystemConceptDesignationW;
  codes : TFhirCodeSystemConceptListW;
begin
  b := TFslStringBuilder.Create;
  try
    ctxt := TFhirCodeSystemProviderContext(locate(opContext, code));
    if ctxt = nil Then
      b.Append('Error: Code '+code+' not known')
    else
    try
      card.addLink('Further Detail', baseURL+'/tx/valuesets/'+FCs.CodeSystem.id+'#'+code);
      concept := ctxt.concept;

      b.Append(FCS.CodeSystem.name+' Code '+code+' : '+concept.display+#13#10#13#10);
      b.Append('* Definition: '+markdownNoPara(concept.definition)+#13#10);
      b.Append('* System Definition: '+markdownNoPara(FCs.CodeSystem.description)+#13#10);
      b.Append(#13#10);

      if concept.designationCount > 0 then
      begin
        b.Append('Designations: '+#13#10#13#10);
        for d in concept.designations.forEnum do
          b.Append('* '+spacer(d.language)+spacer(d.useGen)+d.value+#13#10);
        b.Append(#13#10);
      end;

      codes := FCS.CodeSystem.getParents(concept);
      try
        if codes.count > 0 then
        begin
          b.Append('Parents: '+#13#10#13#10);
          for c in codes do
            b.Append('* '+c.code+': '+c.display+#13#10);
          b.Append(#13#10);
        end;
      finally
        codes.free;
      end;

      codes := FCS.CodeSystem.getChildren(concept);
      try
        if codes.count > 0 then
        begin
          b.Append('Childrem: '+#13#10#13#10);
          for c in codes do
            b.Append('* '+c.code+': '+c.display+#13#10);
          b.Append(#13#10);
        end;
      finally
        codes.free;
      end;

    finally
      ctxt.free;
    End;
    if FCs.CodeSystem.copyright <> '' then
      b.Append(FCs.CodeSystem.copyright+#13#10);
    card.detail := b.ToString;
  finally
    b.free;
  end;end;

function TFhirCodeSystemProvider.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
var
  ex : TFhirExtensionW;
  code : String;
  ndx : integer;
  ctxt : TFhirCodeSystemProviderContext;
begin
  result := nil;
  ctxt := context.context as TFhirCodeSystemProviderContext;
  if context.context = nil then
    result := TFhirCodeSystemProviderContext.create(FCs.CodeSystem.concept(context.current))
  else
  begin
    if (context.current < ctxt.concept.conceptCount) then
      result := TFhirCodeSystemProviderContext.create(ctxt.concept.concept(context.current))
    else
    begin
      raise EFslException.Create('Not supported right now');
//      ndx := context.current - TFhirCodeSystemProviderContext(context.context).context.conceptCount;
//      // what is this doing?
//      for ex in TFhirCodeSystemProviderContext(context).context.modifierExtensions.forEnum do
//      begin
//        if (ndx = 0) then
////        begin
////          code := TFHIRCode(ex.value).value;
////          exit(doLocate(code));
////        end
////        else if ex.url = 'http://hl7.org/fhir/StructureDefinition/codesystem-subsumes' then
////          dec(ndx);
//      end;
    end;
  end;
  context.next;
end;

function TFhirCodeSystemProvider.Display(opContext : TTxOperationContext; context: TCodeSystemProviderContext; langList : THTTPLanguageList): string;
var
  ccd : TFhirCodeSystemConceptDesignationW;
  css : TFHIRCodeSystemW;
  cc : TFhirCodeSystemConceptW;
  lm, um : Boolean;
begin
  result := TFhirCodeSystemProviderContext(context).concept.display;
  for ccd in TFhirCodeSystemProviderContext(context).concept.designations.forEnum do
  begin
    lm := (langList <> nil) and not (langList.matches(FCs.CodeSystem.language, false)) and langList.matches(ccd.language, false);
    um := not ccd.hasUse; // or isDisplayUsage(ccd.use);
    if (lm and um) then
      result := ccd.value.Trim;
  end;
  for css in FCs.Supplements do
  begin
    cc := locCode(css.conceptList, TFhirCodeSystemProviderContext(context).concept.code, css.propertyCode('http://hl7.org/fhir/concept-properties#alternateCode'), nil);
    if (cc <> nil) then
    begin
      if (langList <> nil) and langList.matches(css.language, false) then
        result := cc.display.Trim;
      for ccd in cc.designations.forEnum do
        if (langList <> nil) and langList.matches(ccd.language, false) then
          result := ccd.value.Trim;
    end;
  end;
end;

procedure TFhirCodeSystemProvider.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
var
  ctxt : TFhirCodeSystemProviderContext;
  ccd : TFhirCodeSystemConceptDesignationW;
  css : TFhirCodeSystemW;
  cc : TFhirCodeSystemConceptW;
begin
  ctxt := context as TFhirCodeSystemProviderContext;

  list.baseLang := FLanguages.parse(FCs.CodeSystem.language);
  if ctxt.concept.display <> '' then
    list.addDesignation(true, true, '', FCs.CodeSystem.language, ctxt.concept.displayElement);

  for ccd in ctxt.concept.designations.forEnum do
    list.addDesignation(ccd);
  for css in FCs.Supplements do
  begin
    cc := locCode(css.conceptList, ctxt.concept.code, css.propertyCode('http://hl7.org/fhir/concept-properties#alternateCode'), nil);
    if (cc <> nil) then
    begin
      if (cc.display <> '') then
      begin
        list.addDesignation(false, true, '', css.language, cc.displayElement); {no .link}
      end;
      for ccd in cc.designations.forEnum do
        list.addDesignation(ccd);
    end;
  end;
end;

function TFhirCodeSystemProvider.InFilter(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext): Boolean;
var
  cm : TFhirCodeSystemConceptMatch;
  c : TFhirCodeSystemConceptW;
begin
  result := false;
  c := TFhirCodeSystemProviderContext(concept).concept;
  for cm in TFhirCodeSystemProviderFilterContext(ctxt).concepts do
    if cm.FItem = c then
    begin
      result := true;
      exit;
    end;
end;

function TFhirCodeSystemProvider.IsAbstract(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  result := FCs.CodeSystem.isAbstract(TFhirCodeSystemProviderContext(context).concept);
end;

function TFhirCodeSystemProvider.IsInactive(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  Result := FCs.CodeSystem.isInactive(TFhirCodeSystemProviderContext(context).concept);
end;

function TFhirCodeSystemProvider.getCodeStatus(opContext : TTxOperationContext; context: TCodeSystemProviderContext): String;
begin
  Result := FCs.CodeSystem.codeStatus(TFhirCodeSystemProviderContext(context).concept);
end;


function TFhirCodeSystemProvider.conceptHasProperty(
  concept: TFhirCodeSystemConceptW; url: String; value: string): boolean;
var
  p : String;
  t : TFhirCodeSystemPropertyW;
  cs : TFhirCodeSystemW;
  v : TFhirCodeSystemConceptPropertyW;
begin
  result := false;
  p := '';
  for t in FCs.CodeSystem.properties.forEnum do
    if (t.uri = url) then
      p := t.code;
  for cs in FCs.Supplements do
    for t in cs.properties.forEnum do
      if (t.uri = url) then
        p := t.code;
  if (p <> '') then
    for v in concept.properties.forEnum do
      if (v.code = p) and (v.value <> nil) and (v.value.primitiveValue = value) then
        exit(true);
end;

function TFhirCodeSystemProvider.contentMode: TFhirCodeSystemContentMode;
begin
  result := FCs.CodeSystem.content;
end;

function TFhirCodeSystemProvider.sourcePackage: String;
begin
  if (FCs.CodeSystemProxy <> nil) and (FCs.CodeSystemProxy.packageId <> '') then
    result := FCs.CodeSystemProxy.packageId
  else
    result := '';
end;

function TFhirCodeSystemProvider.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TFhirCodeSystemProvider.getDefinition(opContext : TTxOperationContext; code: String): String;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(opContext, code);
  try
    if (ctxt = nil) then
      raise ETerminologyError.create('Unable to find '+code+' in '+systemUri, itUnknown)
    else
      result := Definition(opContext, ctxt);
  finally
    ctxt.free;
  end;
end;

function TFhirCodeSystemProvider.getDisplay(opContext : TTxOperationContext; code: String; langList : THTTPLanguageList): String;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(opContext, code);
  try
    if (ctxt = nil) then
      raise ETerminologyError.create('Unable to find '+code+' in '+systemUri, itUnknown)
    else
      result := Display(opContext, ctxt, langList);
  finally
    ctxt.free;
  end;
end;

function TFhirCodeSystemProvider.getParent(ctxt: TFhirCodeSystemConceptW): TFhirCodeSystemConceptW;
  //function getMyParent(list: TFhirCodeSystemConceptListW): TFhirCodeSystemConceptW;
  //var
  //  c, TFHIRCodeSystemEntry : TFhirCodeSystemConceptW;
  //begin
  //  for c in list do
  //  begin
  //    if c.hasConcept(ctxt) then
  //      exit(c);
  //    TFHIRCodeSystemEntry := getMyParent(c.conceptList);
  //    if (TFHIRCodeSystemEntry <> nil) then
  //      exit(TFHIRCodeSystemEntry);
  //  end;
  //  exit(nil);
  //end;
var
  entry : TFHIRCodeSystemCodeEntry;
begin
  entry := ctxt.TagNoLink as TFHIRCodeSystemCodeEntry;
  if (entry <> nil) and (entry.parents <> nil) and (entry.parents.Count > 0) then
    result := entry.parents[0].Concept
  else
    result := nil;
  //if (FCodeMap <> nil) then
  //  result := TConceptAdornment(ctxt.TagNoLink).FParent
  //else
  //  result := getMyParent(FCs.CodeSystem.conceptList)
end;

function TFhirCodeSystemProvider.getProperty(code: String): TFhirCodeSystemPropertyW;
var
  p : TFhirCodeSystemPropertyW;
  cs : TFhirCodeSystemW;
  uri : String;
begin
  result := nil;
  uri := csUriForProperty(code);
  if (uri <> '') then
  begin
    for p in FCs.CodeSystem.properties.forEnum do
      if (p.uri = uri) then
        exit(p.link);
    for cs in FCs.Supplements do
      for p in cs.properties.forEnum do
        if (p.uri = uri) then
          exit(p.link);
  end;

  for p in FCs.CodeSystem.properties.forEnum do
    if (p.code = code) and ((uri = '') or (p.uri = '')) then
      exit(p.link);

  for cs in FCs.Supplements do
    for p in cs.properties.forEnum do
      if (p.code = code) and ((uri = '') or (p.uri = '')) then
        exit(p.link);
end;

function TFhirCodeSystemProvider.hasPropForCode(code: String): boolean;
var
  p : TFhirCodeSystemPropertyW;
  cs : TFhirCodeSystemW;
  uri : String;
begin
  result := false;
  for p in FCs.CodeSystem.properties.forEnum do
    if (p.code = code) then
      exit(true);

  for cs in FCs.Supplements do
    for p in cs.properties.forEnum do
      if (p.code = code) then
        exit(true);
end;


function TFhirCodeSystemProvider.hasSupplement(opContext : TTxOperationContext; url: String): boolean;
var
  cs : TFHIRCodeSystemW;
begin
  result := false;
  for cs in FCs.Supplements do
    if (cs.vurl = url) or (cs.url = url) then
      exit(true);
end;

procedure TFhirCodeSystemProvider.listSupplements(opContext : TTxOperationContext; ts: TStringList);
var
  cs : TFHIRCodeSystemW;
begin
  for cs in FCs.Supplements do
    ts.add(cs.vurl);
end;

function TFhirCodeSystemProvider.locCode(list : TFhirCodeSystemConceptListW; code, synonym : String; altOpt : TAlternateCodeOptions) : TFhirCodeSystemConceptW;
var
  c : TFhirCodeSystemConceptW;
  p : TFhirCodeSystemConceptPropertyW;
  s : String;
  ext : TFHIRExtensionW;
begin
  if (synonym = '') then
    synonym := 'alternateCode';
  result := nil;

  for c in list do
  begin
    if (FCs.uc(c.code) = FCs.uc(code)) then
      exit(c);
    // legacy
    if (altOpt <> nil) then
    begin
      if (altOpt.all) then
        begin
        for ext in c.getExtensionsW('http://hl7.org/fhir/StructureDefinition/codesystem-alternate').forEnum do
        begin
          s := ext.getExtensionString('code');
          if (FCs.uc(s) = FCs.uc(code)) then
            exit(c);
        end;
      end;
      for p in c.properties.forEnum do
        if (p.code = synonym) and (p.value.primitiveValue = code) and altOpt.passes(p) then
          exit(c);

    end;
    result := locCode(c.conceptList, code, synonym, altOpt);
    if result <> nil then
      exit;
  end;
end;

function TFhirCodeSystemProvider.LocateCode(code : String; altOpt : TAlternateCodeOptions) : TFhirCodeSystemConceptW;
begin
  if (FCs.FCodeMap.ContainsKey(FCs.uc(code))) then
    result := FCs.FCodeMap[FCs.uc(code)].Concept
  else
    result := locCode(FCs.CodeSystem.conceptList, code, FCs.CodeSystem.propertyCode('http://hl7.org/fhir/concept-properties#alternateCode'), altOpt);
end;

function TFhirCodeSystemProvider.doLocate(list : TFhirCodeSystemConceptListW; code : String; altOpt : TAlternateCodeOptions) : TFhirCodeSystemProviderContext;
var
  c : TFhirCodeSystemConceptW;
begin
  result := nil;
  for c in list do
  begin
    if (c.code = code) then
      exit(TFhirCodeSystemProviderContext.Create(c.Link));
    result := doLocate(c.conceptList, code, altOpt);
    if result <> nil then
      exit;
  end;
end;

function TFhirCodeSystemProvider.doLocate(code : String; altOpt : TAlternateCodeOptions) : TFhirCodeSystemProviderContext;
var
  c : TFhirCodeSystemConceptW;
begin
  c := LocateCode(code, altOpt);
  if (c = nil) then
    result := nil
  else
    result := TFhirCodeSystemProviderContext.Create(c.Link);
end;

function hasProp(props : TArray<String>; name : String; def : boolean) : boolean;
begin
  if (props = nil) or (length(props) = 0) then
    result := def
  else
    result := StringArrayExistsSensitive(props, name) or StringArrayExistsSensitive(props, '*');
end;

procedure TFhirCodeSystemProvider.extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  concepts : TFhirCodeSystemConceptListW;
  cc, context : TFhirCodeSystemConceptW;
  parent, child : TFhirCodeSystemConceptW;
  ccd : TFhirCodeSystemConceptDesignationW;
  cp : TFhirCodeSystemConceptPropertyW;
  pp : TFhirCodeSystemPropertyW;
  d : TFHIRLookupOpRespDesignationW;
  p : TFHIRLookupOpRespPropertyW;
  css : TFHIRCodeSystemW;
  c : TFHIRCodingW;
begin
  context := TFHIRCodeSystemProviderContext(ctxt).concept;
  concepts := TFhirCodeSystemConceptListW.Create;
  try
    concepts.Add(context.Link);
    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, context.code, css.propertyCode('http://hl7.org/fhir/concept-properties#alternateCode'), nil);
      if (cc <> nil) then
      begin
        concepts.Add(cc.Link);
        if (css.language <> '') and (cc.display <> '') then
          cc.setDisplayTag('lang', css.language);
      end;
    end;

    if hasProp(props, 'parent', true) then
    begin
      parent := getParent(context);
      if (parent <> nil) then
      begin
        p := resp.addProp('parent');
        p.value := FFactory.makeCode(parent.code);
        p.description := parent.display;
      end;
    end;

    if hasProp(props, 'child', true) then
    begin
      for child in context.conceptList do
      begin
        p := resp.addProp('child');
        p.value := FFactory.makeCode(child.code);
        p.description := child.display;
      end;
    end;

    if hasProp(props, 'designation', true) then
      for cc in concepts do
      begin
        if (cc.display <> '') and (cc.display <> context.display) and (cc.displayTag('lang') <> '') then
        Begin
          resp.addDesignation(cc.displayTag('lang'), cc.display)
        End;

        for ccd in cc.designations.forEnum do
        Begin
          d := resp.addDesignation(ccd.language, ccd.value);
          c := ccd.use;
          try
            d.use := c.Element.link;
          finally
            c.free;
          end;
        End;
      end;


    for cc in concepts do
      for cp in cc.properties.forEnum do
      begin
        pp := getProperty(cp.code);
        try
          if (pp <> nil) and hasProp(props, cp.code, true) then
          begin
            p := resp.addprop(cp.code);
            p.value := cp.value.link; // todo: should we check this?
          end;
        finally
          pp.free;
        end;
      end;
  finally
    concepts.free;
  end;
end;

function {TFhirCodeSystemProvider.}leafCodes(context : pointer; concept: TFhirCodeSystemConceptW): boolean;
begin
  result := concept.conceptList.Count = 0;
end;

function {TFhirCodeSystemProvider.}nonLeafCodes(context : pointer; concept: TFhirCodeSystemConceptW): boolean;
begin
  result := concept.conceptList.Count > 0;
end;

procedure TFhirCodeSystemProvider.listChildrenByProperty(opContext : TTxOperationContext; op : String; code: String; list, children: TFhirCodeSystemConceptListW);
var
  item : TFhirCodeSystemConceptW;
begin
  for item in list do
  begin
    deadCheck(opContext, 'listChildrenByProperty', op);
    if conceptHasProperty(item, 'http://hl7.org/fhir/concept-properties#parent', code) then
      children.Add(item.link)
    else if item.HasConcepts then
      listChildrenByProperty(opContext, op, code, item.conceptList, children);
  end;
end;

function TFhirCodeSystemProvider.locate(opContext : TTxOperationContext; code: String; altOpt : TAlternateCodeOptions; var message : String): TCodeSystemProviderContext;
begin
  result := DoLocate(code, altOpt);
end;

function TFhirCodeSystemProvider.searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean): TCodeSystemProviderFilterContext;
var
  res : TFhirCodeSystemProviderFilterContext;
begin
  res := TFhirCodeSystemProviderFilterContext.Create;
  try
    FilterCodes(res, FCs.CodeSystem.conceptList, filter);
    res.sort;
    result := res.Link;
  finally
    res.free;
  end;
end;

function TFhirCodeSystemProvider.subsumesTest(opContext : TTxOperationContext; codeA, codeB: String): String;
var
  TFHIRCodeSystemEntry, cA, cB : TFhirCodeSystemConceptW;
begin
  cA := LocateCode(codeA, nil);
  if (cA = nil) then
    raise ETerminologyError.create('Unknown Code "'+codeA+'"', itUnknown);
  cB := LocateCode(codeB, nil);
  if (cB = nil) then
    raise ETerminologyError.create('Unknown Code "'+codeB+'"', itUnknown);

  TFHIRCodeSystemEntry := CB;
  while TFHIRCodeSystemEntry <> nil do
  begin
    if (TFHIRCodeSystemEntry = cA) then
      exit('subsumes');
    TFHIRCodeSystemEntry := getParent(TFHIRCodeSystemEntry);
  end;

  TFHIRCodeSystemEntry := CA;
  while TFHIRCodeSystemEntry <> nil do
  begin
    if (TFHIRCodeSystemEntry = cB) then
      exit('subsumed-by');
    TFHIRCodeSystemEntry := getParent(TFHIRCodeSystemEntry);
  end;
  exit('not-subsumed');
end;

function TFhirCodeSystemProvider.systemUri(): String;
begin
  result := FCs.CodeSystem.url;
end;

function TFhirCodeSystemProvider.TotalCount: integer;
function count(item : TFhirCodeSystemConceptW) : integer;
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
  for i := 0 to FCs.CodeSystem.conceptList.count - 1 do
    inc(result, count(FCs.CodeSystem.conceptList[i]));
end;

function TFhirCodeSystemProvider.getPropertyDefinitions: TFslList<TFhirCodeSystemPropertyW>;
begin
  Result := FCs.CodeSystem.properties;
end;

function TFhirCodeSystemProvider.version: String;
begin
   result := FCs.CodeSystem.version;
end;

function TFhirCodeSystemProvider.versionAlgorithm(): TFHIRVersionAlgorithm;
begin
  Result := FCs.CodeSystem.versionAlgorithm;
end;

procedure TFhirCodeSystemProvider.iterateCodes(opContext : TTxOperationContext; op : String; base : TFhirCodeSystemConceptW; list : TFhirCodeSystemProviderFilterContext; filter : TCodeSystemCodeFilterProc; context : pointer; includeRoot : boolean; exception : TFhirCodeSystemConceptW = nil);
var
  i : integer;
  el : TFslList<TFHIRObject>;
  e : TFHIRObject;
  ex: TFhirExtensionW;
  ctxt : TCodeSystemProviderContext;
  s : TArray<String>;
  cl : TFhirCodeSystemConceptListW;
  entry, child : TFHIRCodeSystemCodeEntry;
begin
  SetLength(s, 1);
  s[0] := 'http://hl7.org/fhir/StructureDefinition/codesystem-subsumes';
  FFactory.checkNoModifiers(base, 'CodeSystemProvider.iterateCodes', 'code', s);
  if (exception <> nil) and (exception.code = base.code) then
    exit;

  if includeRoot and filter(context, base) then
      list.Add(base.Link, 0);

  // 1. Add children in the heirarchy
  // for performance reasons, this is pregenerated
  entry := base.TagNoLink as TFHIRCodeSystemCodeEntry;
  if (entry.hasChildren) then
  begin
    for child in entry.children do
    begin
      deadCheck(opContext, 'iterate-codes-1', op);
      iterateCodes(opContext, op, child.Concept, list, filter, context, true);
    end;
  end;

  //// 2. find any codes that identify this as a parent in their properties
  //cl := TFhirCodeSystemConceptListW.Create;
  //try
  //  listChildrenByProperty(opContext, op, base.code, FCs.CodeSystem.conceptList, cl);
  //  for i := 0 to cl.count - 1 do
  //  begin
  //    deadCheck(opContext, 'iterate-codes-2', op);
  //    iterateCodes(opContext, op, cl[i], list, filter, context, true);
  //  end;
  //finally
  //  cl.free;
  //end;
  // 3. Look in http://hl7.org/fhir/StructureDefinition/codesystem-subsumes extension (deprecated now)
  //el := base.getExtensionsV('http://hl7.org/fhir/StructureDefinition/codesystem-subsumes');
  //try
  //  for e in el do
  //  begin
  //    ex := FFactory.wrapExtension(e.Link);
  //    try           
  //      deadCheck(opContext, 'iterate-codes-3', op);
  //      ctxt := doLocate(ex.value.primitiveValue, nil);
  //      try
  //        iterateCodes(opContext, op, TFhirCodeSystemProviderContext(ctxt).concept, list, filter, context, true);
  //      finally
  //        ctxt.free;
  //      end;
  //    finally
  //      ex.free;
  //    end;
  //  end;
  //finally
  //  el.free;
  //end;
end;


procedure TFhirCodeSystemProvider.iterateConceptsByProperty(src : TFhirCodeSystemConceptListW; pp: TFhirCodeSystemPropertyW; values: TStringArray; list: TFhirCodeSystemProviderFilterContext; include : boolean);
var
  c : TFhirCodeSystemConceptW;
  css : TFhirCodeSystemW;
  cp : TFhirCodeSystemConceptPropertyW;
  ok, val : boolean;
  coding : TFHIRCodingW;
begin
  for c in src do
  begin
    ok := not include;
    val := false;
    for cp in c.properties.forEnum do
    begin
      if (ok <> include) and (cp.code = pp.code) then
      begin
        val := true;
        case pp.type_ of
          cptCode, cptString, cptInteger, cptBoolean, cptDateTime, cptDecimal:
            begin
              ok := StringArrayExistsSensitive(values, cp.value.primitiveValue) = include;
            end;
          cptCoding:
            begin
              coding := FFactory.wrapCoding(cp.value.Link);
              try
                ok := StringArrayExistsSensitive(values, coding.code) = include;
              finally
                coding.free;
              end;
            end;
        end;
      end;
    end;
    if ok then
      list.Add(c.Link, 0); 
    if (c.hasConcepts) then
      iterateConceptsByProperty(c.conceptList, pp, values, list, include);
  end;
end;

procedure TFhirCodeSystemProvider.iterateConceptsByPropertyRegex(src: TFhirCodeSystemConceptListW; pp: TFhirCodeSystemPropertyW; regex: TRegularExpression; list: TFhirCodeSystemProviderFilterContext; include: boolean);
var
  c, cc : TFhirCodeSystemConceptW;
  css : TFhirCodeSystemW;
  cp : TFhirCodeSystemConceptPropertyW;
  ok, val : boolean;
  coding : TFHIRCodingW;
begin
  for c in src do
  begin
    ok := not include;
    val := false;
    for cp in c.properties.forEnum do
    begin
      if (ok <> include) and (cp.code = pp.code) then
      begin
        val := true;
        case pp.type_ of
          cptCode, cptString, cptInteger, cptBoolean, cptDateTime, cptDecimal:
            ok := regex.isMatch(cp.value.primitiveValue) = include;
          cptCoding:
            begin
            coding := FFactory.wrapCoding(cp.value.Link);
            try
              ok := regex.isMatch(coding.code) = include;
            finally
              coding.free;
            end;
            end;
        end;
      end;
    end;
    if ok then
      list.Add(c.Link, 0);  
    if (c.hasConcepts) then
      iterateConceptsByPropertyRegex(c.conceptList, pp, regex, list, include);
  end;
end;

procedure TFhirCodeSystemProvider.iterateConceptsByKnownProperty(
  src: TFhirCodeSystemConceptListW; code: String; values: TStringArray;
  List: TFhirCodeSystemProviderFilterContext; include: boolean);
var
  c, cc : TFhirCodeSystemConceptW;
  concepts : TFhirCodeSystemConceptListW;
  css : TFhirCodeSystemW;
  cp : TFhirCodeSystemConceptPropertyW;
  ok, val : boolean;
  coding : TFHIRCodingW;
  s1, s2 : String;
begin
  concepts := TFhirCodeSystemConceptListW.Create;
  try
    for c in src do
    begin
      concepts.Clear;
      concepts.Add(c.Link);
      for css in FCs.Supplements do
      begin
        cc := locCode(css.conceptList, c.code, css.propertyCode('http://hl7.org/fhir/concept-properties#alternateCode'), nil);
        if (cc <> nil) then
          concepts.Add(cc.Link);
      end;
      for cc in concepts do
      begin
        ok := not include;
        val := false;
        for cp in cc.properties.forEnum do
        begin
          if (ok <> include) and (cp.code = code) then
          begin
            val := true;
            if (cp.value.isPrimitive) then
            begin
              ok := StringArrayExistsSensitive(values, cp.value.primitiveValue) = include;
            end
            else // Coding:
            begin
              coding := FFactory.wrapCoding(cp.value.Link);
              try
                ok := StringArrayExistsSensitive(values, coding.code) = include;
              finally
                coding.free;
              end;
            end;
          end;
        end;
        if ok then
          list.Add(c.Link, 0);
      end;
      if (c.hasConcepts) then
        iterateConceptsByKnownProperty(c.conceptList, code, values, list, include);
    end;
  finally
    concepts.free;
  end;
end;

procedure TFhirCodeSystemProvider.iterateConceptsByRegex(src: TFhirCodeSystemConceptListW; regex: TRegularExpression; list: TFhirCodeSystemProviderFilterContext);
var
  c : TFhirCodeSystemConceptW;
  ok : boolean;
  rx: TRegularExpression;
begin
  for c in src do
  begin
    ok := regex.isMatch(c.code);
    if ok then
      list.Add(c.Link, 0);
    iterateConceptsByRegex(c.conceptList, regex, list);
  end;
end;

procedure TFhirCodeSystemProvider.iterateConceptsByEquality(positive: boolean; src: TFhirCodeSystemConceptListW; code: string;                        list: TFhirCodeSystemProviderFilterContext);
var
  c : TFhirCodeSystemConceptW;
  ok : boolean;
begin
  for c in src do
  begin
    ok := (c.code = code) = positive;
    if ok then
      list.Add(c.Link, 0);
    iterateConceptsByEquality(positive, c.conceptList, code, list);
  end;
end;

function toStringArray(value : String) : TStringArray; overload;
begin
  setLength(result, 1);
  result[0] := value;
end;

function toStringArray(value : String; ch : Char) : TStringArray; overload;
var
  i : integer;
begin
  result := value.split([',']);
  for i := 0 to length(result) - 1 do
    result[i] := result[i].trim();
end;

function TFhirCodeSystemProvider.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop: String; op: TFhirFilterOperator; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  code : TFhirCodeSystemProviderContext;
  ts : TStringList;
  i: Integer;
  pp : TFhirCodeSystemPropertyW;
  cc : TFhirCodeSystemConceptW;
  includeRoot : boolean;
  regex : TRegularExpression;
begin
  SetThreadStatus(ClassName+'.filter('+prop+CODES_TFhirFilterOperator[op]+value+')');
  if (op in [foIsA, foDescendentOf]) and ((prop = 'concept') or (prop = 'code')) then
  begin
    code := doLocate(value, nil);
    try
      if code = nil then
        raise ETerminologyError.Create('Unable to locate code '''+value+'''', itUnknown)
      else
      begin
        includeRoot := true;
        if op = foDescendentOf then
          includeRoot := false;
        result := TFhirCodeSystemProviderFilterContext.Create;
        try
          iterateCodes(opContext, 'filter('+prop+CODES_TFhirFilterOperator[op]+value+')', code.concept, result as TFhirCodeSystemProviderFilterContext, allCodes, nil, includeRoot);
          result.link;
        finally
          result.free;
        end;
      end;
    finally
      code.free;
    end;
  end
  else if (op in [foIsNotA]) and  ((prop = 'concept') or (prop = 'code')) then
  begin
    code := doLocate(value, nil);
    try
      if code = nil then
        raise ETerminologyError.Create('Unable to locate code '+value, itUnknown)
      else
      begin
        result := TFhirCodeSystemProviderFilterContext.Create;
        try
          for cc in FCs.CodeSystem.conceptList do
            iterateCodes(opContext, 'filter('+prop+CODES_TFhirFilterOperator[op]+value+')', cc, result as TFhirCodeSystemProviderFilterContext, allCodes, code.concept, true);
          result.link;
        finally
          result.free;
        end;
      end;
    finally
      code.free;
    end;
  end
  else if (op = foIn) and  ((prop = 'concept') or (prop = 'code')) then
  begin
    result := TFhirCodeSystemProviderFilterContext.Create;
    try
      ts := TStringList.Create;
      try
        ts.CommaText := value;
        for i := 0 to ts.Count - 1 do
        begin
          code := doLocate(value, nil);
          try
            deadCheck(opContext, 'filter-1', 'filter('+prop+CODES_TFhirFilterOperator[op]+value+')');
            if code = nil then
              raise ETerminologyError.Create('Unable to locate code '+value, itUnknown)
            else
              TFhirCodeSystemProviderFilterContext(result).Add(code.concept.Link, 0);
          finally
            code.free;
          end;
        end;
      finally
        ts.free;
      end;
      result.link;
    finally
      result.free;
    end;
  end
  else if (op = foExists) and (prop = 'child') then
  begin
    result := TFhirCodeSystemProviderFilterContext.Create;
    try
      for cc in FCs.CodeSystem.conceptList do
        if value = 'true' then
          iterateCodes(opContext, 'filter('+prop+CODES_TFhirFilterOperator[op]+value+')', cc, result as TFhirCodeSystemProviderFilterContext, nonLeafCodes, nil, true)
        else
          iterateCodes(opContext, 'filter('+prop+CODES_TFhirFilterOperator[op]+value+')', cc, result as TFhirCodeSystemProviderFilterContext, leafCodes, nil, true);
      result.link;
    finally
      result.free;
    end;
  end
  else if (op = foEqual) and (prop = 'code') then
  begin
    result := TFhirCodeSystemProviderFilterContext.Create;
    try
      iterateConceptsByEquality(true, FCs.CodeSystem.conceptList, value, result as TFhirCodeSystemProviderFilterContext);
      result.link;
    finally
      result.free;
    end;
  end
  else if (op = foRegex) and (prop = 'code') then
  begin
    result := TFhirCodeSystemProviderFilterContext.Create;
    regex := TRegularExpression.create('^'+value+'$');
    try
      iterateConceptsByRegex(FCs.CodeSystem.conceptList, regex, result as TFhirCodeSystemProviderFilterContext);
      result.link;
    finally
      regex.free;
      result.free;
    end;
  end
  else
  begin
    pp := getProperty(prop);
    try
      if (pp <> nil) and (op = foEqual) then
      begin
        result := TFhirCodeSystemProviderFilterContext.Create;
        try
          iterateConceptsByProperty(FCs.CodeSystem.conceptList, pp, toStringArray(value), result as TFhirCodeSystemProviderFilterContext, true);
          result.link;
        finally
          result.free;
        end;
      end
      else if (pp <> nil) and (op = foIn) then
      begin
        result := TFhirCodeSystemProviderFilterContext.Create;
        try
          iterateConceptsByProperty(FCs.CodeSystem.conceptList, pp, toStringArray(value, ','), result as TFhirCodeSystemProviderFilterContext, true);
          result.link;
        finally
          result.free;
        end;
      end
      else if (pp <> nil) and (op = foRegex) then
      begin
        result := TFhirCodeSystemProviderFilterContext.Create;     
        regex := TRegularExpression.create('^'+value+'$');
        try
          iterateConceptsByPropertyRegex(FCs.CodeSystem.conceptList, pp, regex, result as TFhirCodeSystemProviderFilterContext, true);
          result.link;
        finally        
          regex.free;
          result.free;
        end;
      end
      else if (pp <> nil) and (op = foNotIn) then
      begin
        result := TFhirCodeSystemProviderFilterContext.Create;
        try
          iterateConceptsByProperty(FCs.CodeSystem.conceptList, pp, toStringArray(value, ','), result as TFhirCodeSystemProviderFilterContext, false);
          result.link;
        finally
          result.free;
        end;
      end
      else if StringArrayExists(['notSelectable'], prop) and (op = foEqual) then // special known properties
      begin
        result := TFhirCodeSystemProviderFilterContext.Create;
        try
          iterateConceptsByKnownProperty(FCs.CodeSystem.conceptList, prop, toStringArray(value), result as TFhirCodeSystemProviderFilterContext, true);
          result.link;
        finally
          result.free;
        end;
      end
      else if StringArrayExists(['notSelectable'], prop) and (op = foIn) then // special known properties
      begin
        result := TFhirCodeSystemProviderFilterContext.Create;
        try
          iterateConceptsByKnownProperty(FCs.CodeSystem.conceptList, prop, toStringArray(value, ','), result as TFhirCodeSystemProviderFilterContext, true);
          result.link;
        finally
          result.free;
        end;
      end
      else if StringArrayExists(['notSelectable'], prop) and (op = foNotIn) then // special known properties
      begin
        result := TFhirCodeSystemProviderFilterContext.Create;
        try
          iterateConceptsByKnownProperty(FCs.CodeSystem.conceptList, prop, toStringArray(value, ','), result as TFhirCodeSystemProviderFilterContext, false);
          result.link;
        finally
          result.free;
        end;
      end
      else
        result := nil;
    finally
      pp.free;
    end;
  end
end;

procedure TFhirCodeSystemProvider.FilterCodes(dest : TFhirCodeSystemProviderFilterContext; source: TFhirCodeSystemConceptListW; filter : TSearchFilterText);
var
  i : integer;
  code : TFhirCodeSystemConceptW;
  rating : double;
  stems : TFslStringList;
begin
  for i := 0 to source.Count - 1 do
  begin
    code := source[i];
    stems := code.Tag as TFslStringList;
    rating := 0;
    if (code.code.toLower = filter.filter) or (code.display.toLower = filter.filter) then
      rating := 100
    else if code.code.toLower.startsWith(filter.filter) then
      rating := 90
    else if code.display.toLower.startsWith(filter.filter) then
      rating := 80+(10 * (length(filter.filter)/length(code.display)))
    else
      rating := filter.matches(stems);
    if rating > 0 then
      dest.add(code.link, rating);

    filterCodes(dest, code.conceptList, filter);
  end;
end;

function TFhirCodeSystemProvider.FilterMore(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  inc(TFhirCodeSystemProviderFilterContext(ctxt).ndx);
  result := TFhirCodeSystemProviderFilterContext(ctxt).ndx < TFhirCodeSystemProviderFilterContext(ctxt).concepts.Count;
end;

function TFhirCodeSystemProvider.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  result := TFhirCodeSystemProviderFilterContext(ctxt).concepts.Count;
end;

function TFhirCodeSystemProvider.FilterConcept(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  context : TFhirCodeSystemProviderFilterContext;
begin
  context := TFhirCodeSystemProviderFilterContext(ctxt);
  result := TFhirCodeSystemProviderContext.Create(context.concepts[context.ndx].FItem.Link)
end;

function TFhirCodeSystemProvider.filterLocate(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; code: String; var message : String): TCodeSystemProviderContext;
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

function hasParent(c, p : TFHIRCodeSystemCodeEntry) : boolean;
var
  e : TFHIRCodeSystemCodeEntry;
begin
  result := false;
  if (c.hasParents) then
    for e in c.parents do
      if (e = p) or hasParent(e, p) then
        exit(true);
end;

function TFhirCodeSystemProvider.locateIsA(opContext : TTxOperationContext; code, parent: String; disallowSelf: boolean = false): TCodeSystemProviderContext;
var
  c, p : TFHIRCodeSystemCodeEntry;
begin
  c := FCs.FCodeMap[FCs.uc(code)];
  p := FCs.FCodeMap[FCs.uc(parent)];
  if (c <> nil) and (p <> nil) and ((c <> p) or not disallowSelf) and hasParent(c, p) then
    result := TFhirCodeSystemProviderContext.create(c.Concept.link)
  else
    result := nil;
  //result := nil;
  //p := Locate(parent) as TFhirCodeSystemProviderContext;
  //if (p <> nil) then
  //  try
  //    if (p.concept.code <> code) then
  //      result := doLocate(p.concept.conceptList, code, nil)
  //    else if not disallowParent then
  //      result := p.Link
  //  finally
  //    p.free;
  //  end;
end;

function TFhirCodeSystemProvider.name(context: TCodeSystemProviderContext): String;
begin
   result := FCs.CodeSystem.name;
end;


function TFhirCodeSystemProvider.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FCs.sizeInBytes(magic));
  inc(result, FFactory.sizeInBytes(magic));
end;

{ TFHIRCodeSystemManager }

constructor TFHIRCodeSystemManager.Create;
begin
  inherited;
  FMap := TFslMap<TFHIRCodeSystemEntry>.create('CS.Manager');
  FMap.defaultValue := nil;
  FList := TFslList<TFHIRCodeSystemEntry>.Create;
end;

destructor TFHIRCodeSystemManager.Destroy;
begin
  FMap.free;
  FList.free;
  inherited;
end;

procedure TFHIRCodeSystemManager.Assign(oSource: TFslObject);
var
  src : TFHIRCodeSystemManager;
begin
  inherited;
  src := oSource as TFHIRCodeSystemManager;
  FMap.Clear;
  FList.Clear;
  FMap.addAll(src.FMap);
  Flist.addAll(src.FList);
end;

function TFHIRCodeSystemManager.clone: TFHIRCodeSystemManager;
begin
  result := TFHIRCodeSystemManager(inherited clone);
end;

function TFHIRCodeSystemManager.link: TFHIRCodeSystemManager;
begin
  result := TFHIRCodeSystemManager(inherited link);
end;

function escapeUrl(url : String) : String;
begin
  result := url.replace('|', '%7C');
end;

procedure TFHIRCodeSystemManager.see(r : TFHIRCodeSystemEntry);
var
  eurl : String;
begin
  if r.url = URI_CVX then
    r.id := r.id;

  if (r.id = '') then
    r.id := newGUIDId;
  if (FMap.containsKey(r.id)) then
    drop(r.id);

  FList.add(r.link);
  FMap.add(r.id, r.link); // we do this so we can drop by id

  eurl := escapeUrl(r.url);
  if (r.url <> '') then
  begin
    if (r.version <> '') then
    begin
      FMap.addorSetValue((eurl)+'|'+r.version, r.link);
    end;
    updateList(eurl, r.version);
  end;
end;

function isIntPart(s : String; start, length, min, max : integer; var value : integer) : boolean;
var
  v : String;
begin
  v := copy(s, start, length);
  value := StrToIntDef(v, -1);
  result := (value > -1) and (value >= min) and (value <= max);
end;

function isDate(s : String; out y, m, d : integer) : Boolean;
begin
  y := 0;
  m := 0;
  d := 0;

  if (length(s) = 6) then
    result := isIntPart(s, 1, 4, 0, 9999, y) and isIntPart(s, 5, 2, 1, 12, m)
  else if length(s) = 7 then
    result := isIntPart(s, 1, 4, 0, 9999, y) and isIntPart(s, 6, 2, 1, 12, m)
  else if length(s) = 8 then
    result := isIntPart(s, 1, 4, 0, 9999, y) and isIntPart(s, 5, 2, 1, 12, m) and isIntPart(s, 7, 2, 1, 31, d)
  else if length(s) = 10 then
    result := isIntPart(s, 1, 4, 0, 9999, y) and isIntPart(s, 6, 2, 1, 12, m) and isIntPart(s, 9, 2, 1, 31, d)
  else
    result := false
end;

{$IFDEF FPC}
function TFHIRCodeSystemManager.sort(sender : TObject; const L, R: TFHIRCodeSystemEntry): Integer;
var
  v1, v2, mm1, mm2 : string;
  y1, y2, m1, m2, d1, d2 : integer;
begin
  v1 := l.version;
  v2 := r.version;
  if (v1 = '') and (v2 = '') then
    result := FList.indexOf(l) - FList.indexOf(r)
  else if (v1 = '') then
    result := -1
  else if (v2 = '') then
    result := 1
  else if (TSemanticVersion.isValid(v1) and TSemanticVersion.isValid(v2)) then
  begin
    mm1 := TFHIRVersions.getMajMin(v1);
    mm2 := TFHIRVersions.getMajMin(v2);
    if (mm1 = '') or (mm2 = '') then
      result := v1.compareTo(v2)
    else
      result := CompareText(mm1, mm2);
  end
  else if isDate(v1, y1, m1, d1) and isDate(v2, y2, m2, d2) then
  begin
    if (y1 <> y2) then
      result := y1 - y2
    else if (m1 <> m2) then
      result := m1 - m2
    else
      result := d1 - d2;
  end
  else
    result := CompareText(v1, v2);
end;
{$ENDIF}

procedure TFHIRCodeSystemManager.updateList(url, version : String);
var
  rl : TFslList<TFHIRCodeSystemEntry>;
  tt, latest : TFHIRCodeSystemEntry;
  lv : String;
begin
  rl := TFslList<TFHIRCodeSystemEntry>.Create;
  try
    for tt in FList do
    begin
      if (url = tt.url) and not rl.contains(tt) then
        rl.add(tt.link);
    end;

    if (rl.count > 0) then
    begin
      // sort by version as much as we are able
      {$IFDEF FPC}
      rl.sortE(sort);
      {$ELSE}
      rl.sortF(function (const L, R: TFHIRCodeSystemEntry): Integer
        var v1, v2, mm1, mm2 : string;
        begin
          v1 := l.version;
          v2 := r.version;
          if (v1 = '') and (v2 = '') then
            result := FList.indexOf(l) - FList.indexOf(r)
          else if (v1 = '') then
            result := -1
          else if (v2 = '') then
            result := 1
          else
          begin
            mm1 := TFHIRVersions.getMajMin(v1, false);
            mm2 := TFHIRVersions.getMajMin(v2, false);
            if (mm1 = '') or (mm2 = '') then
              result := v1.compareTo(v2)
            else
              result := CompareText(mm1, mm2);
          end;
        end);
      {$ENDIF}

      // the current is the latest
      FMap.addOrSetValue(url, rl[rl.count-1].link);
      // now, also, the latest for major/minor
      if (version <> '') then
      begin
        latest := nil;
        for tt in rl do
        begin
          if (TFHIRVersions.matches(tt.version, version, semverMinor)) then
            latest := tt;
        end;
        if (latest <> nil) then // might be null if it's not using semver
        begin
          lv := TFHIRVersions.getMajMin(latest.version, false);
          if (lv <> '') and (lv <> version) then
            FMap.addOrSetValue(url+'|'+lv, rl[rl.count-1].link);
        end;
      end;
    end;
  finally
   rl.free;
  end;
end;

function TFHIRCodeSystemManager.get(url : String) : TFHIRCodeSystemEntry;
begin
  result := FMap[url];
end;

function TFHIRCodeSystemManager.get(url, version : string) : TFHIRCodeSystemEntry;
var
  mm : String;
begin
  if (FMap.containsKey(url+'|'+version)) then
    result := FMap[url+'|'+version]
  else
  begin
    mm := TFHIRVersions.getMajMin(version, false);
    if (mm <> '') then
      result := FMap[url+'|'+mm]
    else
      result := nil;
  end;
end;

function TFHIRCodeSystemManager.has(url : String) : boolean;
begin
  result := FMap.containsKey(url);
end;

function TFHIRCodeSystemManager.has(url, version : string) : boolean;
var
  mm : String;
begin
  if (FMap.containsKey(url+'|'+version)) then
    result := true
  else
  begin
    mm := TFHIRVersions.getMajMin(version);
    if (mm <> '') then
      result := FMap.containsKey(url+'|'+mm)
    else
     result := false;
  end;
end;

function TFHIRCodeSystemManager.has(url : String; var res : TFHIRCodeSystemEntry) : boolean;
begin
  result := FMap.TryGetValue(url, res);
  if res = nil then
    result := false;
end;

function TFHIRCodeSystemManager.has(url, version : string; var res : TFHIRCodeSystemEntry) : boolean;
var
  mm : String;
begin
  res := nil;
  if (FMap.TryGetValue(url+'|'+version, res)) then
    result := true
  else
  begin
    mm := TFHIRVersions.getMajMin(version);
    if (mm <> '') then
      result := FMap.TryGetValue(url+'|'+mm, res)
    else
     result := false;
  end;
  if res = nil then
    result := false;
end;

function TFHIRCodeSystemManager.count : integer;
begin
  result := FList.count;
end;

procedure TFHIRCodeSystemManager.drop(id : String);
var
  res : TFHIRCodeSystemEntry;
  mm : String;
begin
  res := FMap[id];
  if (res <> nil) then
  begin
    res.link;
    try
      FList.remove(res);
      FMap.remove(id);
      FMap.remove(res.url);
      if (res.version <> '') then
      begin
        FMap.remove(res.url+'|'+res.version);
        mm := TFHIRVersions.getMajMin(res.version);
        if (mm <> '') then
          FMap.remove(res.url+'|'+mm);
      end;
      updateList(escapeUrl(res.url), res.version);
    finally
      res.free;
    end;
  end;
end;

procedure TFHIRCodeSystemManager.listAll(list : TFslList<TFHIRCodeSystemW>);
var
  tt : TFHIRCodeSystemEntry;
begin
  for tt in FList do
    list.add(tt.CodeSystem.link);
end;

procedure TFHIRCodeSystemManager.listAllM(list : TFslMetadataResourceList);
var
  tt : TFHIRCodeSystemEntry;
begin
  for tt in FList do
    list.add(tt.CodeSystem.link);
end;

procedure TFHIRCodeSystemManager.clear();
begin
  FList.clear();
  FMap.clear();
end;


function TFHIRCodeSystemManager.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FMap.sizeInBytes(magic));
  inc(result, FList.sizeInBytes(magic));
end;

end.
