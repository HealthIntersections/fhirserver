unit FHIR.Tools.CodeSystemProvider;

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
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Collections,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.Common, FHIR.CdsHooks.Utilities, FHIR.Base.Lang,
  FHIR.Tx.Service;

type
  TConceptAdornment = class (TFslStringList)
  private
    FParent: TFHIRCodeSystemConceptW;
  public
    property parent : TFHIRCodeSystemConceptW read FParent write FParent; // not owned, can't be
  end;

  TFhirCodeSystemProviderContext = class (TCodeSystemProviderContext)
  private
    context : TFhirCodeSystemConceptW;
  public
    constructor Create(context : TFhirCodeSystemConceptW); overload;
    destructor Destroy; override;
  end;

  TFhirCodeSystemConceptMatch = class (TFslObject)
  private
    FItem : TFhirCodeSystemConceptW;
    FRating : double;
  public
    Constructor Create(item : TFhirCodeSystemConceptW; rating : double);
    Destructor Destroy; override;
  end;

  TCodeSystemAdornment = class (TFslObject)
  private
    FMap : TFslMap<TFhirCodeSystemConceptW>;
  public
    constructor Create(map : TFslMap<TFhirCodeSystemConceptW>);
    destructor Destroy; override;
    property map : TFslMap<TFhirCodeSystemConceptW> read FMap;
  end;

  TFHIRCodeSystemEntry = class (TFslObject)
  private
    FCodeSystem : TFHIRCodeSystemW;
    FValueset : TFHIRValueSetW;
    FSupplements : TFslList<TFHIRCodeSystemW>;
    function GetHasSupplements: boolean;
    function GetSupplements: TFslList<TFHIRCodeSystemW>;
    procedure SetCodeSystem(const Value: TFHIRCodeSystemW);
  public
    Constructor Create(cs : TFhirCodeSystemW);
    destructor Destroy; override;

    function Link : TFHIRCodeSystemEntry; overload;

    property CodeSystem : TFHIRCodeSystemW read FCodeSystem write SetCodeSystem;
    Property hasSupplements : boolean read GetHasSupplements;
    property Supplements : TFslList<TFHIRCodeSystemW> read GetSupplements;
  end;

  TFhirCodeSystemProviderFilterContext = class (TCodeSystemProviderFilterContext, IComparer<TFhirCodeSystemConceptMatch>)
  private
    ndx : integer;
    concepts : TFslList<TFhirCodeSystemConceptMatch>;

    procedure Add(item : TFhirCodeSystemConceptW; rating : double);
    function Compare(const Left, Right: TFhirCodeSystemConceptMatch): Integer;
    procedure sort;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
  end;

  TFhirCodeSystemProvider = class (TCodeSystemProvider)
  private
    FCs : TFhirCodeSystemEntry;
    FMap : TFslMap<TFhirCodeSystemConceptW>;
    FFactory : TFHIRFactory;

    function LocateCode(code : String) : TFhirCodeSystemConceptW;
    function doLocate(code : String) : TFhirCodeSystemProviderContext; overload;
    function doLocate(list : TFslList<TFhirCodeSystemConceptW>; code : String) : TFhirCodeSystemProviderContext; overload;
    function getParent(ctxt : TFhirCodeSystemConceptW) : TFhirCodeSystemConceptW;
    procedure FilterCodes(dest : TFhirCodeSystemProviderFilterContext; source : TFslList<TFhirCodeSystemConceptW>; filter : TSearchFilterText);
    procedure iterateCodes(base: TFhirCodeSystemConceptW; list: TFhirCodeSystemProviderFilterContext);
    function locCode(list: TFslList<TFhirCodeSystemConceptW>; code: String): TFhirCodeSystemConceptW;
    function getProperty(code : String) : TFhirCodeSystemPropertyW;
    procedure iterateConceptsByProperty(src : TFslList<TFhirCodeSystemConceptW>; pp : TFhirCodeSystemPropertyW; value : String; list: TFhirCodeSystemProviderFilterContext);
  public
    constructor Create(factory : TFHIRFactory; vs : TFhirCodeSystemEntry); overload;
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

    function hasSupplement(url : String) : boolean; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
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
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; lang : String; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    function subsumesTest(codeA, codeB : String) : String; override;
  end;


implementation

{ TFhirCodeSystemProviderContext }

constructor TFhirCodeSystemProviderContext.Create(context: TFhirCodeSystemConceptW);
begin
  inherited create;
  self.context := context;
end;

destructor TFhirCodeSystemProviderContext.Destroy;
begin
  context.Free;
  inherited;
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

function TFHIRCodeSystemEntry.GetSupplements: TFslList<TFHIRCodeSystemW>;
begin
  if FSupplements = nil then
    FSupplements := TFslList<TFHIRCodeSystemW>.create;
  result := FSupplements;
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

{ TFhirCodeSystemProviderFilterContext }

procedure TFhirCodeSystemProviderFilterContext.Add(item: TFhirCodeSystemConceptW; rating : double);
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
  self.concepts := TFslList<TFhirCodeSystemConceptMatch>.create;
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

{ TCodeSystemAdornment }

constructor TCodeSystemAdornment.Create(map: TFslMap<TFhirCodeSystemConceptW>);
begin
  inherited Create;
  FMap := map;
end;

destructor TCodeSystemAdornment.destroy;
begin
  FMap.Free;
  inherited;
end;

{ TFhirCodeSystemProvider }

constructor TFhirCodeSystemProvider.create(factory : TFHIRFactory; vs: TFhirCodeSystemEntry);
begin
  Create;
  FCs := vs;
  FFactory := factory;
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
  FFactory.Free;
  inherited;
end;

function TFhirCodeSystemProvider.ChildCount(context: TCodeSystemProviderContext): integer;
begin
  if context = nil then
    result := FCs.CodeSystem.conceptCount
  else
    result := TFhirCodeSystemProviderContext(context).context.conceptCount + TFhirCodeSystemProviderContext(context).context.extensionCount('http://hl7.org/fhir/StructureDefinition/codesystem-subsumes');
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
  concept, c : TFhirCodeSystemConceptW;
  d : TFhirCodeSystemConceptDesignationW;
  codes : TFslList<TFhirCodeSystemConceptW>;
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

function TFhirCodeSystemProvider.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
var
//  ex : TFhirExtension;
  code : String;
begin
  result := nil;
  if context = nil then
    result := TFhirCodeSystemProviderContext.create(FCs.CodeSystem.concept(ndx))
  else
  begin
    if (ndx < TFhirCodeSystemProviderContext(context).context.conceptCount) then
      result := TFhirCodeSystemProviderContext.create(TFhirCodeSystemProviderContext(context).context.concept(ndx))
    else
    begin
      ndx := ndx - TFhirCodeSystemProviderContext(context).context.conceptCount;
      // what is this doing?
//      for ex in TFhirCodeSystemProviderContext(context).context.modifierExtensionList do
//        if (ndx = 0) then
//        begin
//          code := TFHIRCode(ex.value).value;
//          exit(doLocate(code));
//        end
//        else if ex.url = 'http://hl7.org/fhir/StructureDefinition/codesystem-subsumes' then
//          dec(ndx);
    end;
  end;
end;

function TFhirCodeSystemProvider.Display(context: TCodeSystemProviderContext; lang : String): string;
var
  ccd : TFhirCodeSystemConceptDesignationW;
  css : TFHIRCodeSystemW;
  cc : TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemProviderContext(context).context.display;
  if (lang <> '') then
    for ccd in TFhirCodeSystemProviderContext(context).context.designations.forEnum do
      if languageMatches(lang, ccd.language) then
        result := ccd.value;
  for css in FCs.Supplements do
  begin
    cc := locCode(css.conceptList, TFhirCodeSystemProviderContext(context).context.code);
    if (cc <> nil) then
    begin
      if languageMatches(lang, css.language) then
        result := cc.display;
      for ccd in cc.designations.forEnum do
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
  ccd : TFhirCodeSystemConceptDesignationW;
  cc : TFhirCodeSystemConceptW;
  ctxt : TCodeSystemProviderContext;
  css : TFhirCodeSystemW;
begin
  ctxt := locate(code);
  try
    list.Add(getDisplay(code, lang));
    for ccd in TFhirCodeSystemProviderContext(ctxt).context.designations.forEnum do
      if (lang = '') or languageMatches(lang, ccd.language) then
        list.add(ccd.value);
    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, code);
      if (cc <> nil) then
      begin
        if languageMatches(lang, css.language) then
          list.add(cc.display);
        for ccd in cc.designations.forEnum do
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
  c : TFhirCodeSystemConceptW;
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
  result := FCs.CodeSystem.isAbstract(TFhirCodeSystemProviderContext(context).context);
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
      raise ETerminologyError.create('Unable to find '+code+' in '+system(nil))
    else
      result := Display(ctxt, lang);
  finally
    Close(ctxt);
  end;
end;

function TFhirCodeSystemProvider.getParent(ctxt: TFhirCodeSystemConceptW): TFhirCodeSystemConceptW;
  function getMyParent(list: TFslList<TFhirCodeSystemConceptW>): TFhirCodeSystemConceptW;
  var
    c, t : TFhirCodeSystemConceptW;
  begin
    for c in list do
    begin
      if c.hasConcept(ctxt) then
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

function TFhirCodeSystemProvider.getProperty(code: String): TFhirCodeSystemPropertyW;
var
  p : TFhirCodeSystemPropertyW;
  cs : TFhirCodeSystemW;
begin
  result := nil;
  for p in FCs.CodeSystem.properties.forEnum do
    if (p.code = code) then
      exit(p);
  for cs in FCs.Supplements do
    for p in cs.properties.forEnum do
      if (p.code = code) then
        exit(p);
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

function TFhirCodeSystemProvider.locCode(list : TFslList<TFhirCodeSystemConceptW>; code : String) : TFhirCodeSystemConceptW;
var
  c : TFhirCodeSystemConceptW;
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

function TFhirCodeSystemProvider.LocateCode(code : String) : TFhirCodeSystemConceptW;
begin
  if (FMap = nil) then
    result := locCode(FCs.CodeSystem.conceptList, code)
  else if (FMap.ContainsKey(code)) then
    result := FMap[code]
  else
    result := locCode(FCs.CodeSystem.conceptList, code);
end;

function TFhirCodeSystemProvider.doLocate(list : TFslList<TFhirCodeSystemConceptW>; code : String) : TFhirCodeSystemProviderContext;
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

procedure TFhirCodeSystemProvider.extendLookup(factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; lang : String; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  concepts : TFslList<TFhirCodeSystemConceptW>;
  cc, context : TFhirCodeSystemConceptW;
  parent, child : TFhirCodeSystemConceptW;
  ccd : TFhirCodeSystemConceptDesignationW;
  cp : TFhirCodeSystemConceptPropertyW;
  pp : TFhirCodeSystemPropertyW;
  d : TFHIRLookupOpRespDesignationW;
  p : TFHIRLookupOpRespPropertyW;
  css : TFHIRCodeSystemW;
begin
  context := TFHIRCodeSystemProviderContext(ctxt).context;
  concepts := TFslList<TFhirCodeSystemConceptW>.create;
  try
    concepts.Add(context.Link);
    for css in FCs.Supplements do
    begin
      cc := locCode(css.conceptList, context.code);
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
          d.use := ccd.use.link;
        End;
      end;


    for cc in concepts do
      for cp in cc.properties.forEnum do
      begin
        pp := getProperty(cp.code);
        if (pp <> nil) and hasProp(props, cp.code, true) then
        begin
          p := resp.addprop(cp.code);
          p.value := cp.value.link; // todo: should we check this?
        end;
      end;
  finally
    concepts.Free;
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
  t, cA, cB : TFhirCodeSystemConceptW;
begin
  cA := LocateCode(codeA);
  if (cA = nil) then
    raise ETerminologyError.create('Unknown Code "'+codeA+'"');
  cB := LocateCode(codeB);
  if (cB = nil) then
    raise ETerminologyError.create('Unknown Code "'+codeB+'"');

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

procedure TFhirCodeSystemProvider.iterateCodes(base : TFhirCodeSystemConceptW; list : TFhirCodeSystemProviderFilterContext);
var
  i : integer;
  el : TFslList<TFHIRObject>;
  e : TFHIRObject;
  ex: TFhirExtensionW;
  ctxt : TCodeSystemProviderContext;
begin
  FFactory.checkNoModifiers(base, 'CodeSystemProvider.iterateCodes', 'code', ['http://hl7.org/fhir/StructureDefinition/codesystem-subsumes']);
  list.Add(base.Link, 0);
  for i := 0 to base.conceptList.count - 1 do
    iterateCodes(base.conceptList[i], list);
  el := base.extensions('http://hl7.org/fhir/StructureDefinition/codesystem-subsumes');
  try
    for e in el do
    begin
      ex := FFactory.wrapExtension(e);
      try
        ctxt := doLocate(ex.value.primitiveValue);
        try
          iterateCodes(TFhirCodeSystemProviderContext(ctxt).context, list);
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


procedure TFhirCodeSystemProvider.iterateConceptsByProperty(src : TFslList<TFhirCodeSystemConceptW>; pp: TFhirCodeSystemPropertyW; value: String; list: TFhirCodeSystemProviderFilterContext);
var
  c, cc : TFhirCodeSystemConceptW;
  concepts : TFslList<TFhirCodeSystemConceptW>;
  css : TFhirCodeSystemW;
  cp : TFhirCodeSystemConceptPropertyW;
  ok : boolean;
  coding : TFHIRCodingW;
begin
  concepts := TFslList<TFhirCodeSystemConceptW>.create;
  try
    for c in src do
    begin
      concepts.Clear;
      concepts.Add(c.Link);
      for css in FCs.Supplements do
      begin
        cc := locCode(css.conceptList, c.code);
        if (cc <> nil) then
          concepts.Add(cc.Link);
      end;
      for cc in concepts do
      begin
        ok := false;
        for cp in cc.properties.forEnum do
          if not ok and (cp.code = pp.code) then
            case pp.type_ of
              cptCode, cptString, cptInteger, cptBoolean, cptDateTime, cptDecimal:
                ok := cp.value.primitiveValue = value;
              cptCoding:
                begin
                  coding := FFactory.wrapCoding(cp.value);
                  try
                    ok := coding.code = value;
                  finally
                    coding.free;
                  end;
                end;
            end;
        if ok then
          list.Add(c.Link, 0);
      end;
      iterateConceptsByProperty(c.conceptList, pp, value, list);
    end;
  finally
    concepts.Free;
  end;
end;

function TFhirCodeSystemProvider.filter(prop: String; op: TFhirFilterOperator; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  code : TFhirCodeSystemProviderContext;
  ts : TStringList;
  i: Integer;
  pp : TFhirCodeSystemPropertyW;
begin
  if (op = foIsA) and (prop = 'concept') then
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
  begin
    pp := getProperty(prop);
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
  end
end;

procedure TFhirCodeSystemProvider.FilterCodes(dest : TFhirCodeSystemProviderFilterContext; source: TFslList<TFhirCodeSystemConceptW>; filter : TSearchFilterText);
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


end.
