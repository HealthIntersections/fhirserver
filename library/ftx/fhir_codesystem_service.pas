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
  fsl_base, fsl_utilities, fsl_collections, fsl_http, fsl_lang, fsl_versions, fsl_fpc, fsl_logging, fsl_regex,
  fhir_objects, fhir_factory, fhir_common, fhir_cdshooks,  fhir_utilities, fhir_features, fhir_uris,
  ftx_service;

type
  TConceptAdornment = class (TFslStringList)
  private
    FParent: TFHIRCodeSystemConceptW;
  public
    property parent : TFHIRCodeSystemConceptW read FParent write FParent; // not owned, can't be
  end;

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

  TCodeSystemAdornment = class (TFslObject)
  private
    FCodeMap : TFhirCodeSystemConceptMapW;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(map : TFhirCodeSystemConceptMapW);
    destructor Destroy; override;
    property codeMap : TFhirCodeSystemConceptMapW read FCodeMap;
  end;

  TFHIRCodeSystemEntry = class (TFslObject)
  private
    FCodeSystem : TFHIRCodeSystemW;
    FSupplements : TFslList<TFHIRCodeSystemW>;
    function GetHasSupplements: boolean;
    function GetSupplements: TFslList<TFHIRCodeSystemW>;
    procedure SetCodeSystem(const Value: TFHIRCodeSystemW);
    function GetUrl: String;

    function GetVersion: String;
    function GetId: String;
    procedure SetId(const Value: String);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(cs : TFhirCodeSystemW);
    destructor Destroy; override;

    function Link : TFHIRCodeSystemEntry; overload;
    property id : String read GetId write SetId;
    property url : String read GetUrl;
    property version : String read GetVersion;

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
    FCodeMap : TFhirCodeSystemConceptMapW;
    FFactory : TFHIRFactory;

    function LocateCode(code : String) : TFhirCodeSystemConceptW;
    function doLocate(code : String) : TFhirCodeSystemProviderContext; overload;
    function doLocate(list : TFhirCodeSystemConceptListW; code : String) : TFhirCodeSystemProviderContext; overload;
    function getParent(ctxt : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptW;
    procedure FilterCodes(dest : TFhirCodeSystemProviderFilterContext; source : TFhirCodeSystemConceptListW; filter : TSearchFilterText);
    procedure iterateCodes(base: TFhirCodeSystemConceptW; list: TFhirCodeSystemProviderFilterContext; filter : TCodeSystemCodeFilterProc; context : pointer; includeRoot : boolean; exception : TFhirCodeSystemConceptW = nil);
    function locateParent(ctxt: TFHIRCodeSystemConceptW; code: String): String;
    function locCode(list: TFhirCodeSystemConceptListW; code, synonym: String): TFhirCodeSystemConceptW;
    function getProperty(code : String) : TFhirCodeSystemPropertyW;
    function conceptHasProperty(concept : TFhirCodeSystemConceptW; url : String; value : string) : boolean;
    procedure iterateConceptsByProperty(src : TFhirCodeSystemConceptListW; pp : TFhirCodeSystemPropertyW; value : String; list: TFhirCodeSystemProviderFilterContext);
    procedure iterateConceptsByRegex(src : TFhirCodeSystemConceptListW; regex: string; list: TFhirCodeSystemProviderFilterContext);
    procedure listChildrenByProperty(code : String; list, children : TFhirCodeSystemConceptListW);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(languages : TIETFLanguageDefinitions; factory : TFHIRFactory; vs : TFhirCodeSystemEntry); overload;
    destructor Destroy; override;

    function contentMode : TFhirCodeSystemContentMode; override;
    function description : String; override;
    function name(context: TCodeSystemProviderContext): String; override;
    function version(context: TCodeSystemProviderContext): String; override;
    function TotalCount : integer; override;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; overload; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function IsInactive(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    procedure Designations(context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    function itemWeight(context : TCodeSystemProviderContext) : string; override;
    function deprecated(context : TCodeSystemProviderContext) : boolean; override;
    function getExtensions(context : TCodeSystemProviderContext)  : TFslList<TFHIRExtensionW>; override;
    function getProperties(context : TCodeSystemProviderContext) : TFslList<TFhirCodeSystemConceptPropertyW>; override;
    function parent(context : TCodeSystemProviderContext) : String; override;
    function canParent : boolean; override;

    function hasSupplement(url : String) : boolean; override;
    function filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(card : TCDSHookCard; const slang : THTTPLanguages; baseURL, code, display : String); override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    function subsumesTest(codeA, codeB : String) : String; override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
  end;


implementation

{ TFhirCodeSystemProviderContext }

constructor TFhirCodeSystemProviderContext.Create(concept: TFhirCodeSystemConceptW);
begin
  inherited create;
  self.concept := concept;
end;

destructor TFhirCodeSystemProviderContext.Destroy;
begin
  concept.Free;
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
  FItem.Free;
  inherited;
end;

function TFhirCodeSystemConceptMatch.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FItem.sizeInBytes(magic));
end;

{ TFHIRCodeSystemEntry }

constructor TFHIRCodeSystemEntry.Create(cs : TFhirCodeSystemW);
begin
  inherited Create;
  FCodeSystem := cs;
end;

destructor TFHIRCodeSystemEntry.Destroy;
begin
  FCodeSystem.Free;
  FSupplements.Free;
  inherited;
end;

function TFHIRCodeSystemEntry.GetHasSupplements: boolean;
begin
  result := (FSupplements <> nil) and (FSupplements.Count > 0);
end;

function TFHIRCodeSystemEntry.GetId: String;
begin
  result := FCodeSystem.id;
end;

function TFHIRCodeSystemEntry.GetSupplements: TFslList<TFHIRCodeSystemW>;
begin
  if FSupplements = nil then
    FSupplements := TFslList<TFHIRCodeSystemW>.create;
  result := FSupplements;
end;

function TFHIRCodeSystemEntry.GetUrl: String;
begin
  result := FCodeSystem.url;
end;

function TFHIRCodeSystemEntry.GetVersion: String;
begin
  result := FCodeSystem.version;
end;

function TFHIRCodeSystemEntry.Link: TFHIRCodeSystemEntry;
begin
  result := TFHIRCodeSystemEntry(inherited Link);
end;

procedure TFHIRCodeSystemEntry.SetCodeSystem(const Value: TFHIRCodeSystemW);
begin
  FCodeSystem.Free;
  FCodeSystem := Value;
end;

procedure TFHIRCodeSystemEntry.SetId(const Value: String);
begin
  FCodeSystem.id := value;
end;

function TFHIRCodeSystemEntry.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FCodeSystem.sizeInBytes(magic));
  inc(result, FSupplements.sizeInBytes(magic));
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
  self.concepts := TFslList<TFhirCodeSystemConceptMatch>.create;
  ndx := -1;
end;

destructor TFhirCodeSystemProviderFilterContext.Destroy;
begin
  concepts.Free;
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

{ TCodeSystemAdornment }

constructor TCodeSystemAdornment.Create(map: TFhirCodeSystemConceptMapW);
begin
  inherited Create;
  FCodeMap := map;
end;

destructor TCodeSystemAdornment.destroy;
begin
  FCodeMap.Free;
  inherited;
end;

function TCodeSystemAdornment.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FCodeMap.sizeInBytes(magic));
end;

{ TFhirCodeSystemProvider }

constructor TFhirCodeSystemProvider.Create(languages: TIETFLanguageDefinitions;
  factory: TFHIRFactory; vs: TFhirCodeSystemEntry);
begin
  Create(languages);
  FCs := vs;
  FFactory := factory;
  if (FCs.CodeSystem.tag <> nil) then
    FCodeMap := TCodeSystemAdornment(FCs.CodeSystem.Tag).FCodeMap;
end;

procedure TFhirCodeSystemProvider.defineFeatures(features: TFslList<TFHIRFeature>);
begin
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri(nil)+'.filter', 'concept:is-a'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri(nil)+'.filter', 'concept:is-not-a'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri(nil)+'.filter', 'concept:in'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri(nil)+'.filter', 'child:exists'));
end;

function TFhirCodeSystemProvider.Definition(context: TCodeSystemProviderContext): string;
begin
  result := TFhirCodeSystemProviderContext(context).concept.definition;
end;

function TFhirCodeSystemProvider.itemWeight(context: TCodeSystemProviderContext): string;
begin
  Result := TFhirCodeSystemProviderContext(context).concept.itemWeight;
end;

function TFhirCodeSystemProvider.deprecated(context: TCodeSystemProviderContext): boolean;
begin
  Result := FCs.CodeSystem.isDeprecated(TFhirCodeSystemProviderContext(context).concept);
end;

function TFhirCodeSystemProvider.getExtensions(context: TCodeSystemProviderContext): TFslList<TFHIRExtensionW>;
var
  ctxt : TFhirCodeSystemProviderContext;
  ext : TFHIRExtensionW;
  css : TFhirCodeSystemW;
  cc : TFhirCodeSystemConceptW;
begin
  ctxt := context as TFhirCodeSystemProviderContext;

  result := TFslList<TFHIRExtensionW>.create;
  try
    for ext in ctxt.concept.getAllExtensionsW.forEnum do
      result.add(ext.link);

    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, ctxt.concept.code, css.propertyCode('http://hl7.org/fhir/concept-properties#synonym'));
      if (cc <> nil) then
        for ext in cc.getAllExtensionsW.forEnum do
          result.add(ext.link);
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFhirCodeSystemProvider.getProperties(context: TCodeSystemProviderContext): TFslList<TFhirCodeSystemConceptPropertyW>;
var
  ctxt : TFhirCodeSystemProviderContext;
  cp : TFhirCodeSystemConceptPropertyW;
  css : TFhirCodeSystemW;
  cc : TFhirCodeSystemConceptW;
begin
  ctxt := context as TFhirCodeSystemProviderContext;

  result := TFslList<TFhirCodeSystemConceptPropertyW>.create;
  try
    for cp in ctxt.concept.properties.forEnum do
      result.add(cp.link);

    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, ctxt.concept.code, css.propertyCode('http://hl7.org/fhir/concept-properties#synonym'));
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

function TFhirCodeSystemProvider.parent(context: TCodeSystemProviderContext): String;
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

function TFhirCodeSystemProvider.canParent: boolean;
begin
  Result := true;
end;

function TFhirCodeSystemProvider.description: String;
begin
  result := fcs.FCodeSystem.name;
end;

destructor TFhirCodeSystemProvider.Destroy;
begin
  FCs.free;
  FFactory.Free;
  inherited;
end;

function {TFhirCodeSystemProvider.}allCodes(context : pointer; concept: TFhirCodeSystemConceptW): boolean;
begin
  result := true;
end;

function TFhirCodeSystemProvider.getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  if context = nil then
    result := TCodeSystemIteratorContext.Create(nil, FCs.CodeSystem.conceptCount)
  else
    result := TCodeSystemIteratorContext.Create(context.Link, TFhirCodeSystemProviderContext(context).concept.conceptCount + TFhirCodeSystemProviderContext(context).concept.extensionCount('http://hl7.org/fhir/StructureDefinition/codesystem-subsumes'));
end;

procedure TFhirCodeSystemProvider.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.Free;
end;

function TFhirCodeSystemProvider.Code(context: TCodeSystemProviderContext): string;
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

procedure TFhirCodeSystemProvider.getCDSInfo(card: TCDSHookCard; const slang : THTTPLanguages; baseURL, code, display: String);
var
  b : TStringBuilder;
  ctxt : TFhirCodeSystemProviderContext;
  concept, c : TFhirCodeSystemConceptW;
  d : TFhirCodeSystemConceptDesignationW;
  codes : TFhirCodeSystemConceptListW;
begin
  b := TStringBuilder.Create;
  try
    ctxt := TFhirCodeSystemProviderContext(locate(code));
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
        codes.Free;
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

function TFhirCodeSystemProvider.getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
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

function TFhirCodeSystemProvider.Display(context: TCodeSystemProviderContext; const lang : THTTPLanguages): string;
var
  ccd : TFhirCodeSystemConceptDesignationW;
  css : TFHIRCodeSystemW;
  cc : TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemProviderContext(context).concept.display;
  if (lang.header <> '') then
    for ccd in TFhirCodeSystemProviderContext(context).concept.designations.forEnum do
      if lang.matches(ccd.language) then
        result := ccd.value.Trim;
  for css in FCs.Supplements do
  begin
    cc := locCode(css.conceptList, TFhirCodeSystemProviderContext(context).concept.code, css.propertyCode('http://hl7.org/fhir/concept-properties#synonym'));
    if (cc <> nil) then
    begin
      if lang.matches(css.language) then
        result := cc.display.Trim;
      for ccd in cc.designations.forEnum do
        if lang.matches(ccd.language) then
          result := ccd.value.Trim;
    end;
  end;
end;

procedure TFhirCodeSystemProvider.Designations(context: TCodeSystemProviderContext; list: TConceptDesignations);
var
  ctxt : TFhirCodeSystemProviderContext;
  ccd : TFhirCodeSystemConceptDesignationW;
  css : TFhirCodeSystemW;
  cc : TFhirCodeSystemConceptW;
begin
  ctxt := context as TFhirCodeSystemProviderContext;

  list.baseLang := FLanguages.parse(FCs.FCodeSystem.language);
  list.display := ctxt.concept.displayElement;

  for ccd in ctxt.concept.designations.forEnum do
    list.designations.Add(TConceptDesignation.build(FLanguages, ccd));
  for css in FCs.Supplements do
  begin
    cc := locCode(css.conceptList, ctxt.concept.code, css.propertyCode('http://hl7.org/fhir/concept-properties#synonym'));
    if (cc <> nil) then
    begin
      if (cc.display <> '') then
      begin
        list.addDesignation(css.language, cc.displayElement); {no .link}
      end;
      for ccd in cc.designations.forEnum do
        list.designations.Add(TConceptDesignation.build(FLanguages, ccd));
    end;
  end;
end;

function TFhirCodeSystemProvider.InFilter(ctxt: TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext): Boolean;
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

function TFhirCodeSystemProvider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := FCs.CodeSystem.isAbstract(TFhirCodeSystemProviderContext(context).concept);
end;

function TFhirCodeSystemProvider.IsInactive(context: TCodeSystemProviderContext): boolean;
begin
  Result := FCs.CodeSystem.isInactive(TFhirCodeSystemProviderContext(context).concept);
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
  result := FCs.FCodeSystem.content;
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
      raise ETerminologyError.create('Unable to find '+code+' in '+systemUri(nil), itUnknown)
    else
      result := Definition(ctxt);
  finally
    Close(ctxt);
  end;
end;

function TFhirCodeSystemProvider.getDisplay(code: String; const lang : THTTPLanguages): String;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(code);
  try
    if (ctxt = nil) then
      raise ETerminologyError.create('Unable to find '+code+' in '+systemUri(nil), itUnknown)
    else
      result := Display(ctxt, lang);
  finally
    Close(ctxt);
  end;
end;

function TFhirCodeSystemProvider.getParent(ctxt: TFhirCodeSystemConceptW): TFhirCodeSystemConceptW;
  function getMyParent(list: TFhirCodeSystemConceptListW): TFhirCodeSystemConceptW;
  var
    c, TFHIRCodeSystemEntry : TFhirCodeSystemConceptW;
  begin
    for c in list do
    begin
      if c.hasConcept(ctxt) then
        exit(c);
      TFHIRCodeSystemEntry := getMyParent(c.conceptList);
      if (TFHIRCodeSystemEntry <> nil) then
        exit(TFHIRCodeSystemEntry);
    end;
    exit(nil);
  end;
begin
  if (FCodeMap <> nil) then
    result := TConceptAdornment(ctxt.Tag).FParent
  else
    result := getMyParent(FCs.CodeSystem.conceptList)
end;

function TFhirCodeSystemProvider.getProperty(code: String): TFhirCodeSystemPropertyW;
var
  p : TFhirCodeSystemPropertyW;
  cs : TFhirCodeSystemW;
begin
  result := nil;
  for p in FCs.CodeSystem.properties.forEnum do
    if (p.code = code) then
      exit(p.link);
  for cs in FCs.Supplements do
    for p in cs.properties.forEnum do
      if (p.code = code) then
        exit(p.link);
end;


function TFhirCodeSystemProvider.hasSupplement(url: String): boolean;
var
  cs : TFHIRCodeSystemW;
begin
  result := false;
  for cs in FCs.Supplements do
    if cs.url = url then
      exit(true);
end;

function TFhirCodeSystemProvider.locCode(list : TFhirCodeSystemConceptListW; code, synonym : String) : TFhirCodeSystemConceptW;
var
  c : TFhirCodeSystemConceptW;
  p : TFhirCodeSystemConceptPropertyW;
begin
  result := nil;
  for c in list do
  begin
    if (c.code = code) then
      exit(c);
    if (synonym <> '') then
      for p in c.properties.forEnum do
        if (p.code = synonym) and (p.value.primitiveValue = code) then
          exit(c);
    result := locCode(c.conceptList, code, synonym);
    if result <> nil then
      exit;
  end;
end;

function TFhirCodeSystemProvider.LocateCode(code : String) : TFhirCodeSystemConceptW;
begin
  if (FCodeMap <> nil) and FCodeMap.ContainsKey(code) then
    result := FCodeMap[code]
  else
    result := locCode(FCs.CodeSystem.conceptList, code, FCs.CodeSystem.propertyCode('http://hl7.org/fhir/concept-properties#synonym'));
end;

function TFhirCodeSystemProvider.doLocate(list : TFhirCodeSystemConceptListW; code : String) : TFhirCodeSystemProviderContext;
var
  c : TFhirCodeSystemConceptW;
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
  c : TFhirCodeSystemConceptW;
begin
  c := LocateCode(code);
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
    result := StringArrayExistsSensitive(props, name);
end;

procedure TFhirCodeSystemProvider.extendLookup(factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; const lang : THTTPLanguages; props: TArray<String>; resp: TFHIRLookupOpResponseW);
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
begin
  context := TFHIRCodeSystemProviderContext(ctxt).concept;
  concepts := TFhirCodeSystemConceptListW.create;
  try
    concepts.Add(context.Link);
    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, context.code, css.propertyCode('http://hl7.org/fhir/concept-properties#synonym'));
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
          d.use := ccd.use;
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
          pp.Free;
        end;
      end;
  finally
    concepts.Free;
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

procedure TFhirCodeSystemProvider.listChildrenByProperty(code: String; list, children: TFhirCodeSystemConceptListW);
var
  item : TFhirCodeSystemConceptW;
begin
  for item in list do
  begin
    if conceptHasProperty(item, 'http://hl7.org/fhir/concept-properties#parent', code) then
      children.Add(item.link)
    else if item.conceptCount > 0 then
      listChildrenByProperty(code, item.conceptList, children);
  end;
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
    FilterCodes(res, FCs.CodeSystem.conceptList, filter);
    res.sort;
    result := res.Link;
  finally
    res.Free;
  end;
end;

function TFhirCodeSystemProvider.subsumesTest(codeA, codeB: String): String;
var
  TFHIRCodeSystemEntry, cA, cB : TFhirCodeSystemConceptW;
begin
  cA := LocateCode(codeA);
  if (cA = nil) then
    raise ETerminologyError.create('Unknown Code "'+codeA+'"', itUnknown);
  cB := LocateCode(codeB);
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

function TFhirCodeSystemProvider.systemUri(context : TCodeSystemProviderContext): String;
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

function TFhirCodeSystemProvider.version(context: TCodeSystemProviderContext): String;
begin
   result := FCs.CodeSystem.version;
end;

procedure TFhirCodeSystemProvider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  ctxt.Free;
end;

procedure TFhirCodeSystemProvider.iterateCodes(base : TFhirCodeSystemConceptW; list : TFhirCodeSystemProviderFilterContext; filter : TCodeSystemCodeFilterProc; context : pointer; includeRoot : boolean; exception : TFhirCodeSystemConceptW = nil);
var
  i : integer;
  el : TFslList<TFHIRObject>;
  e : TFHIRObject;
  ex: TFhirExtensionW;
  ctxt : TCodeSystemProviderContext;
  s : TArray<String>;
  cl : TFhirCodeSystemConceptListW;
begin
  SetLength(s, 1);
  s[0] := 'http://hl7.org/fhir/StructureDefinition/codesystem-subsumes';
  FFactory.checkNoModifiers(base, 'CodeSystemProvider.iterateCodes', 'code', s);
  if (exception <> nil) and (exception.code = base.code) then
    exit;

  if includeRoot and filter(context, base) then
      list.Add(base.Link, 0);

  // 1. Add children in the heirarchy
  for i := 0 to base.conceptList.count - 1 do
    iterateCodes(base.conceptList[i], list, filter, context, true);

  // 2. find any codes that identify this as a parent in their properties
  cl := TFhirCodeSystemConceptListW.create;
  try
    listChildrenByProperty(base.code, FCs.FCodeSystem.conceptList, cl);
    for i := 0 to cl.count - 1 do
      iterateCodes(cl[i], list, filter, context, true);
  finally
    cl.free;
  end;
  // 3. Look in http://hl7.org/fhir/StructureDefinition/codesystem-subsumes extension (deprecated now)
  el := base.getExtensionsV('http://hl7.org/fhir/StructureDefinition/codesystem-subsumes');
  try
    for e in el do
    begin
      ex := FFactory.wrapExtension(e.Link);
      try
        ctxt := doLocate(ex.value.primitiveValue);
        try
          iterateCodes(TFhirCodeSystemProviderContext(ctxt).concept, list, filter, context, true);
        finally
          Close(ctxt);
        end;
      finally
        ex.free;
      end;
    end;
  finally
    el.free;
  end;
end;


procedure TFhirCodeSystemProvider.iterateConceptsByProperty(src : TFhirCodeSystemConceptListW; pp: TFhirCodeSystemPropertyW; value: String; list: TFhirCodeSystemProviderFilterContext);
var
  c, cc : TFhirCodeSystemConceptW;
  concepts : TFhirCodeSystemConceptListW;
  css : TFhirCodeSystemW;
  cp : TFhirCodeSystemConceptPropertyW;
  ok, val : boolean;
  coding : TFHIRCodingW;
  s1, s2 : String;
begin
  concepts := TFhirCodeSystemConceptListW.create;
  try
    for c in src do
    begin
      concepts.Clear;
      concepts.Add(c.Link);
      for css in FCs.Supplements do
      begin
        cc := locCode(css.conceptList, c.code, css.propertyCode('http://hl7.org/fhir/concept-properties#synonym'));
        if (cc <> nil) then
          concepts.Add(cc.Link);
      end;
      for cc in concepts do
      begin
        ok := false;
        val := false;
        for cp in cc.properties.forEnum do
        begin
          if not ok and (cp.code = pp.code) then
          begin
            val := true;
            case pp.type_ of
              cptCode, cptString, cptInteger, cptBoolean, cptDateTime, cptDecimal:
                begin
                  s1 := cp.value.primitiveValue;
                  s2 := value;
                  ok := s1 = s2;
                end;
              cptCoding:
                begin
                  coding := FFactory.wrapCoding(cp.value.Link);
                  try
                    ok := coding.code = value;
                  finally
                    coding.free;
                  end;
                end;
            end;
          end;
        end;
        //if (not ok) and (not val and (pp.type_ = cptBoolean) and (value = 'false')) then
        //  ok := true;
        if ok then
          list.Add(c.Link, 0);
      end;
      iterateConceptsByProperty(c.conceptList, pp, value, list);
    end;
  finally
    concepts.Free;
  end;
end;

procedure TFhirCodeSystemProvider.iterateConceptsByRegex(src: TFhirCodeSystemConceptListW; regex: string; list: TFhirCodeSystemProviderFilterContext);
var
  c : TFhirCodeSystemConceptW;
  ok : boolean;
  rx: TRegularExpression;
begin
  for c in src do
  begin
    rx := TRegularExpression.create('^'+regex+'$');
    try
      ok := rx.isMatch(c.code);
    finally
      rx.free;
    end;
    //ok := c.code.length = 5;
    if ok then
      list.Add(c.Link, 0);
    iterateConceptsByRegex(c.conceptList, regex, list);
  end;
end;

function TFhirCodeSystemProvider.filter(forIteration : boolean; prop: String; op: TFhirFilterOperator; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  code : TFhirCodeSystemProviderContext;
  ts : TStringList;
  i: Integer;
  pp : TFhirCodeSystemPropertyW;
  cc : TFhirCodeSystemConceptW;
  includeRoot : boolean;
begin
  if (op in [foIsA, foDescendentOf]) and (prop = 'concept') then
  begin
    code := doLocate(value);
    try
      if code = nil then
        raise ETerminologyError.Create('Unable to locate code '+value, itUnknown)
      else
      begin
        includeRoot := true;
        if op = foDescendentOf then
          includeRoot := false;
        result := TFhirCodeSystemProviderFilterContext.create;
        try
          iterateCodes(code.concept, result as TFhirCodeSystemProviderFilterContext, allCodes, nil, includeRoot);
          result.link;
        finally
          result.Free;
        end;
      end;
    finally
      Close(code)
    end;
  end
  else if (op in [foIsNotA]) and (prop = 'concept') then
  begin
    code := doLocate(value);
    try
      if code = nil then
        raise ETerminologyError.Create('Unable to locate code '+value, itUnknown)
      else
      begin
        result := TFhirCodeSystemProviderFilterContext.create;
        try
          for cc in FCs.FCodeSystem.conceptList do
            iterateCodes(cc, result as TFhirCodeSystemProviderFilterContext, allCodes, code.concept, true);
          result.link;
        finally
          result.Free;
        end;
      end;
    finally
      Close(code)
    end;
  end
  else if (op = foIn) and (prop = 'concept') then
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
              raise ETerminologyError.Create('Unable to locate code '+value, itUnknown)
            else
              TFhirCodeSystemProviderFilterContext(result).Add(code.concept.Link, 0);
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
  else if (op = foExists) and (prop = 'child') then
  begin
    result := TFhirCodeSystemProviderFilterContext.create;
    try
      for cc in FCs.FCodeSystem.conceptList do
        if value = 'true' then
          iterateCodes(cc, result as TFhirCodeSystemProviderFilterContext, nonLeafCodes, nil, true)
        else
          iterateCodes(cc, result as TFhirCodeSystemProviderFilterContext, leafCodes, nil, true);
      result.link;
    finally
      result.Free;
    end;
  end
  else if (op = foRegex) and (prop = 'code') then
  begin
    result := TFhirCodeSystemProviderFilterContext.create;
    try
      iterateConceptsByRegex(FCs.CodeSystem.conceptList, value, result as TFhirCodeSystemProviderFilterContext);
      result.link;
    finally
      result.Free;
    end;

  end
  else
  begin
    pp := getProperty(prop);
    try
      if (pp <> nil) and (op = foEqual)  then
      begin
        result := TFhirCodeSystemProviderFilterContext.create;
        try
          iterateConceptsByProperty(FCs.CodeSystem.conceptList, pp, value, result as TFhirCodeSystemProviderFilterContext);
          result.link;
        finally
          result.Free;
        end;
      end
      else
        result := nil;
    finally
      pp.Free;
    end;
  end
end;

procedure TFhirCodeSystemProvider.FilterCodes(dest : TFhirCodeSystemProviderFilterContext; source: TFhirCodeSystemConceptListW; filter : TSearchFilterText);
var
  i : integer;
  code : TFhirCodeSystemConceptW;
  rating : double;
begin
  for i := 0 to source.Count - 1 do
  begin
    code := source[i];
    if filter.passes(code.tag as TFslStringList, rating) then
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


function TFhirCodeSystemProvider.locateIsA(code, parent: String; disallowParent : boolean = false): TCodeSystemProviderContext;
var
  p : TFhirCodeSystemProviderContext;
begin
  result := nil;
  p := Locate(parent) as TFhirCodeSystemProviderContext;
  if (p <> nil) then
    try
      if (p.concept.code <> code) then
        result := doLocate(p.concept.conceptList, code)
      else if not disallowParent then
        result := p.Link
    finally
      p.free;
    end;
end;

function TFhirCodeSystemProvider.name(context: TCodeSystemProviderContext): String;
begin
   result := FCs.CodeSystem.name;
end;


function TFhirCodeSystemProvider.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FCs.sizeInBytes(magic));
  inc(result, FCodeMap.sizeInBytes(magic));
  inc(result, FFactory.sizeInBytes(magic));
end;

{ TFHIRCodeSystemManager }

constructor TFHIRCodeSystemManager.Create;
begin
  inherited;
  FMap := TFslMap<TFHIRCodeSystemEntry>.create('CS.Manager');
  FMap.defaultValue := nil;
  FList := TFslList<TFHIRCodeSystemEntry>.create;
end;

destructor TFHIRCodeSystemManager.Destroy;
begin
  FMap.Free;
  FList.Free;
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

procedure TFHIRCodeSystemManager.see(r : TFHIRCodeSystemEntry);
begin
  if r.url = URI_CVX then
    r.id := r.id;

  if (r.id = '') then
    r.id := newGUIDId;
  if (FMap.containsKey(r.id)) then
    drop(r.id);

  FList.add(r.link);
  FMap.add(r.id, r.link); // we do this so we can drop by id

  if (r.url <> '') then
  begin
    if (r.version <> '') then
    begin
      FMap.addorSetValue(r.url+'|'+r.version, r.link);
    end;
    updateList(r.url, r.version);
  end;
end;

{$IFDEF FPC}
function TFHIRCodeSystemManager.sort(sender : TObject; const L, R: TFHIRCodeSystemEntry): Integer;
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
    mm1 := TFHIRVersions.getMajMin(v1);
    mm2 := TFHIRVersions.getMajMin(v2);
    if (mm1 = '') or (mm2 = '') then
      result := v1.compareTo(v2)
    else
      result := CompareText(mm1, mm2);
  end;
end;
{$ENDIF}

procedure TFHIRCodeSystemManager.updateList(url, version : String);
var
  rl : TFslList<TFHIRCodeSystemEntry>;
  tt, latest : TFHIRCodeSystemEntry;
  lv : String;
begin
  rl := TFslList<TFHIRCodeSystemEntry>.create;
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
      updateList(res.url, res.version);
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
