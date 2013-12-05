unit FHIRValueSetExpander;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
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
  SysUtils, StringSupport,

  AdvStringObjectMatches, AdvObjects, AdvObjectLists,

  FHIRBase, FHIRTypes, FHIRComponents, FHIRResources, FHIRUtilities, DateAndTime,

  LoincServices, SnomedServices;

const
  UPPER_LIMIT = 20000;

Type
  TCodeSystemProviderContext = TObject;
  TCodeSystemProviderFilterContext = TObject;

  TCodeSystemProvider = {abstract} class (TAdvObject)
  public
    function TotalCount : integer;  virtual; abstract;
    function ChildCount(context : TCodeSystemProviderContext) : integer; virtual; abstract;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; virtual; abstract;
    function system : String; virtual; abstract;
    function getDisplay(code : String):String; virtual; abstract;
    function locate(code : String) : TCodeSystemProviderContext; virtual; abstract;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; virtual; abstract;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; virtual; abstract;
    function Code(context : TCodeSystemProviderContext) : string; virtual; abstract;
    function Display(context : TCodeSystemProviderContext) : string; virtual; abstract;
    function doesFilter(prop : String; op : TFhirFilterOperator; value : String) : boolean;
    function filter(prop : String; op : TFhirFilterOperator; value : String) : TCodeSystemProviderFilterContext; virtual; abstract;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; virtual; abstract;
    function FilterCount(ctxt : TCodeSystemProviderFilterContext) : integer; virtual; abstract;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext; ndx : integer): TCodeSystemProviderContext; virtual; abstract;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); virtual; abstract;
  end;

  TLoincProvider = class (TCodeSystemProvider)
  private
    FLoinc: TLOINCServices;
  public
    constructor create; override;
    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system : String; override;
    function getDisplay(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String) : TCodeSystemProviderFilterContext; override;
    function FilterCount(ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext; ndx : integer): TCodeSystemProviderContext; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
  end;

  TSnomedProvider = class (TCodeSystemProvider)
  private
    FSnomed: TSnomedServices;
    function GetPN(iDescriptions: TCardinalArray): String;
    function GetFSN(iDescriptions: TCardinalArray): String;
  public
    constructor create; override;
    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system : String; override;
    function getDisplay(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String) : TCodeSystemProviderFilterContext; override;
    function FilterCount(ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext; ndx : integer): TCodeSystemProviderContext; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
  end;

  TUcumProvider = class (TCodeSystemProvider)
  public
    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system : String; override;
    function getDisplay(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String) : TCodeSystemProviderFilterContext; override;
    function FilterCount(ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext; ndx : integer): TCodeSystemProviderContext; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
  end;

  TValueSetProvider = class (TCodeSystemProvider)
  private
    FVs : TFhirValueSet;
    function doLocate(list : TFhirValueSetDefineConceptList; code : String) : TCodeSystemProviderContext;
  public
    constructor create(vs : TFHIRValueSet); overload;
    destructor destroy; override;
    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system : String; override;
    function getDisplay(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; overload; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String) : TCodeSystemProviderFilterContext; override;
    function FilterCount(ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext; ndx : integer): TCodeSystemProviderContext; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
  end;

  TValueSetWorker = class (TAdvObject)
  private
    FValueSets : TAdvStringObjectMatch; // TFHIRValueSet
    FCodeSystems : TAdvStringObjectMatch; // TFHIRValueSet
    procedure SetCodeSystems(const Value: TAdvStringObjectMatch);
    procedure SetValueSets(const Value: TAdvStringObjectMatch);
  protected
    function getCodeSystemProvider(cset : TFhirValueSetComposeInclude) : TCodeSystemProvider;
  public
    destructor destroy; override;
    Property ValueSets : TAdvStringObjectMatch read FValueSets write SetValueSets;
    Property CodeSystems : TAdvStringObjectMatch read FCodeSystems write SetCodeSystems;
  end;

  TFHIRValueSetExpander = class (TValueSetWorker)
  private
    FExpanded : TAdvStringObjectMatch; // TFHIRValueSet

    procedure addCodeAndDescendents(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cs : TCodeSystemProvider; context : TCodeSystemProviderContext);

    procedure handleDefine(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetDefine);
    procedure importValueSet(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; uri : TFhirUri);
    procedure includeCodes(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cset : TFhirValueSetComposeInclude);
    procedure excludeCodes(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cset : TFhirValueSetComposeInclude);
    procedure handleCompose(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetCompose);

    procedure addCode(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, code, display: string);
    procedure addDefinedCode(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; system : string; c : TFHIRValueSetDefineConcept);
    function key(system, code : String): string; overload;
    function key(c : TFhirValueSetExpansionContains) : string;  overload;
    function doExpand(source : TFHIRValueSet) : TFHIRValueSet;
  public
    constructor create; override;
    destructor destroy; override;

    function expand(source : TFHIRValueSet) : TFHIRValueSet;
  end;

  TValueSetChecker = class (TValueSetWorker)
  private
    FOthers : TAdvStringObjectMatch; // checkers or code system providers
    fvs : TFHIRValueSet;
    function findCode(code: String; list : TFhirValueSetDefineConceptList): boolean;
    function checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeInclude; code : String) : boolean;
  public
    constructor create; override;
    destructor destroy; override;

    procedure prepare(vs : TFHIRValueSet);

    function check(system, code : String) : boolean;
  end;

implementation

{ TValueSetWorker }

destructor TValueSetWorker.destroy;
begin
  FValueSets.free;
  FCodeSystems.free;
  inherited;
end;

function TValueSetWorker.getCodeSystemProvider(cset: TFhirValueSetComposeInclude): TCodeSystemProvider;
var
  system : String;
begin
  system := cset.systemST;
  if (system = 'http://loinc.org') then
    result := TLoincProvider.create
  else if system = 'http://snomed.info/sct' then
    result := TSnomedProvider.create
  else if system = 'http://unitsofmeasure.org' then
    result := TUcumProvider.create
  else if FCodeSystems.ExistsByKey(system) then
    result := TValueSetProvider.create((FCodeSystems.matches[system] as TFHIRValueSet).link)
  else
    raise Exception.create('unable to provide support for code system '+system);
end;


procedure TValueSetWorker.SetCodeSystems(const Value: TAdvStringObjectMatch);
begin
  FCodeSystems.Free;
  FCodeSystems := Value;
end;

procedure TValueSetWorker.SetValueSets(const Value: TAdvStringObjectMatch);
begin
  FValueSets.Free;
  FValueSets := Value;
end;

{ TFHIRValueSetExpander }

constructor TFHIRValueSetExpander.create;
begin
  inherited;
  FExpanded := TAdvStringObjectMatch.create;
end;

destructor TFHIRValueSetExpander.destroy;
begin
  FExpanded.free;
  inherited;
end;

function TFHIRValueSetExpander.doExpand(source: TFHIRValueSet): TFHIRValueSet;
var
  list : TFhirValueSetExpansionContainsList;
  map : TAdvStringObjectMatch;
  i : integer;
  c : TFhirValueSetExpansionContains;
  e : TFhirExtension;
begin
  result := source.Clone;
  map := TAdvStringObjectMatch.create;
  list := TFhirValueSetExpansionContainsList.create;
  try
    result.expansion := TFhirValueSetExpansion.create;
    result.expansion.timestampST := NowUTC;
    e := result.expansion.ExtensionList.Append;
    e.urlST := 'http://hl7.org/fhir/tools/extensions#version';
    e.value := TFhirString.create('20110731');

    if (source.define <> nil) then
      handleDefine(list, map, source.define);
    if (source.compose <> nil) then
      handleCompose(list, map, source.compose);

    if (map.Count > UPPER_LIMIT) then
      raise Exception.create('Value set size of '+inttostr(map.count)+' exceeds upper limit of '+inttostr(UPPER_LIMIT));

    for i := 0 to list.count - 1 do
    begin
      c := list[i];
      if map.ExistsByKey(key(c)) then
        result.Expansion.containsList.add(c.link);
    end;

    BuildNarrative(result);
    result.link;
  finally
    map.free;
    list.free;
    result.free
  end;
end;

function TFHIRValueSetExpander.expand(source: TFHIRValueSet): TFHIRValueSet;
var
  id : string;
begin
  // todo: locking
  id := source.identifierST;
  if FExpanded.ExistsByKey(id) then
    result := (FExpanded.matches[id] as TFhirValueSet).link
  else
  begin
    result := doExpand(source);
    FExpanded.Add(id, result.Link);
  end;
end;

function TFHIRValueSetExpander.key(system, code : String): string;
begin
  result:= '{'+system+'}'+code;
end;

function TFHIRValueSetExpander.key(c: TFhirValueSetExpansionContains): string;
begin
  result := key(c.SystemST, c.CodeST);
end;

procedure TFHIRValueSetExpander.handleCompose(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source: TFhirValueSetCompose);
var
  i : integer;
begin
  for i := 0 to source.importList.count - 1 do
    importValueSet(list, map, source.importList[i]);
  for i := 0 to source.includeList.count - 1 do
    includeCodes(list, map, source.includeList[i]);
  for i := 0 to source.importList.count - 1 do
    excludeCodes(list, map, source.excludeList[i]);
end;

procedure TFHIRValueSetExpander.handleDefine(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source: TFhirValueSetDefine);
var
  i : integer;
begin
  for i := 0 to source.conceptList.count - 1 do
    addDefinedCode(list, map, source.systemST, source.conceptList[i]);
end;

procedure TFHIRValueSetExpander.addDefinedCode(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system: string; c: TFHIRValueSetDefineConcept);
var
  i : integer;
begin
  if (c.abstract = nil) or not c.AbstractST then
    addCode(list, map, system, c.CodeST, c.DisplayST);
  for i := 0 to c.conceptList.count - 1 do
    addDefinedCode(list, map, system, c.conceptList[i]);
end;

procedure TFHIRValueSetExpander.addCode(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, code, display: string);
var
  n : TFHIRValueSetExpansionContains;
  s : String;
begin
  n := TFHIRValueSetExpansionContains.create;
  try
    n.SystemST := system;
    n.CodeST := code;
    n.DisplayST := display;
    s := key(n);
    if not map.ExistsByKey(s) then
    begin
      list.add(n.link);
      map.add(s, n.link);
    end;
  finally
    n.free;
  end;
end;


procedure TFHIRValueSetExpander.importValueSet(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; uri: TFhirUri);
var
  vs, vsSrc : TFHIRValueSet;
  i : integer;
  c : TFhirValueSetExpansionContains;
  s : String;
begin
  if (uri = nil) then
    raise Exception.create('unable to find value set with no identity');
  vsSrc := (FValuesets.matches[uri.value] as TFHIRValueSet).link;
  try
    // if vsSrc = nil then.... see if we can find it
    if (vsSrc = nil) then
      raise Exception.create('unable to find value set '+uri.value);
    vs := expand(vsSrc);
    try
      for i := 0 to vs.expansion.containsList.Count - 1 do
      begin
        c := vs.expansion.containsList[i];
        s := key(c);
        if not map.ExistsByKey(s) then
        begin
          list.add(c.link);
          map.add(s, c.link);
        end;
      end;
    finally
      vs.free;
    end;
  finally
    vsSrc.free;
  end;
end;

procedure TFHIRValueSetExpander.includeCodes(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cset: TFhirValueSetComposeInclude);
var
  cs : TCodeSystemProvider;
  i, j : integer;
  fc : TFhirValueSetComposeIncludeFilter;
  ctxt : TCodeSystemProviderFilterContext;
  c : TCodeSystemProviderContext;
begin
  cs := getCodeSystemProvider(cset);
  try
    if (cset.codeList.count = 0) and (cset.filterList.count = 0) then
    begin
      // special case - add all the code system
      if cs.TotalCount > UPPER_LIMIT then
        raise exception.create('code system '+cs.system+' too big to include as a whole');
      for i := 0 to cs.ChildCount(nil) - 1 do
        addCodeAndDescendents(list, map, cs, cs.getcontext(nil, i));
    end;
    for i := 0 to cset.codeList.count - 1 do
      addCode(list, map, cs.system, cset.codeList[i].value, cs.getDisplay(cset.codeList[i].value));

    for i := 0 to cset.filterList.count - 1 do
    begin
      fc := cset.filterList[i];
      if ('concept' = fc.property_ST) and (fc.OpST = FilterOperatorIsA) then
        addCodeAndDescendents(list, map, cs, cs.locate(fc.valueST))
      else
      begin
        ctxt := cs.filter(fc.property_ST, fc.OpST, fc.valueST);
        if ctxt = nil then
          raise Exception.create('The filter "'+fc.property_ST +' '+ CODES_TFhirFilterOperator[fc.OpST]+ ' '+fc.valueST+'" was not understood in the context of '+cs.system);
        try
          for j := 0 to cs.FilterCount(ctxt) - 1 do
          begin
            c := cs.FilterConcept(ctxt, j);
            addCode(list, map, cs.system, cs.code(c), cs.display(c));
          end;
        finally
          cs.close(ctxt);
        end;
      end;
    end;
  finally
    cs.free;
  end;
end;

procedure TFHIRValueSetExpander.excludeCodes(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cset: TFhirValueSetComposeInclude);
begin

end;

procedure TFHIRValueSetExpander.addCodeAndDescendents(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cs: TCodeSystemProvider; context: TCodeSystemProviderContext);
var
  i : integer;
begin
  if not cs.IsAbstract(context) then
    addCode(list, map, cs.system, cs.Code(context), cs.Display(context));
  for i := 0 to cs.ChildCount(context) - 1 do
    addCodeAndDescendents(list, map, cs, cs.getcontext(context, i));
end;

{ TLoincProvider }

function TLoincProvider.ChildCount(context: TCodeSystemProviderContext): integer;
begin
  // no children in loinc
  if context = nil then
    result := TotalCount
  else
    result := 0;
end;

function TLoincProvider.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
begin
  if (context = nil) then
    result := TCodeSystemProviderContext(ndx)
  else
    raise exception.create('shouldn''t be here');
end;

constructor TLoincProvider.create;
begin
  inherited;
  FLoinc := GLOINCs.DefaultService;
end;

function TLoincProvider.Code(context: TCodeSystemProviderContext): string;
var
  iDescription, iStems, iOtherNames : Cardinal;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
begin
  FLoinc.Code.GetInformation(integer(context), result, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
end;

function TLoincProvider.Display(context: TCodeSystemProviderContext): string;
var
  iDescription, iStems, iOtherNames : Cardinal;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
begin
  FLoinc.Code.GetInformation(integer(context), result, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
  result := FLoinc.Desc.GetEntry(iDescription)
end;

function TLoincProvider.getDisplay(code: String): String;
begin
  result := FLoinc.GetDisplayByName(code);
  if result = '' then
    raise Exception.create('unable to find '+code+' in '+system);
end;

function TLoincProvider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false; // loinc don't do abstract
end;

function TLoincProvider.locate(code: String): TCodeSystemProviderContext;
var
  i : Cardinal;
begin
  if FLoinc.Code.FindCode(code, i) then
    result := TCodeSystemProviderContext(i)
  else
    result := nil;//raise Exception.create('unable to find '+code+' in '+system);
end;

function TLoincProvider.system: String;
begin
  result := 'http://loinc.org';;
end;

function TLoincProvider.TotalCount: integer;
begin
  result := FLoinc.Code.Count;
end;

type
  THolder = class (TAdvObject)
  private
    Children : LoincServices.TCardinalArray;
    function HasChild(v : integer) : boolean;
  end;

function THolder.HasChild(v : integer) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to Length(Children) - 1 do
    if (Cardinal(v) = Children[i]) then
    begin
      result := true;
      exit;
    end;
end;

procedure TLoincProvider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  THolder(ctxt).free;
end;

function TLoincProvider.filter(prop: String; op: TFhirFilterOperator; value: String): TCodeSystemProviderFilterContext;
var
  ok : boolean;
  id : word;
  aChildren : LoincServices.TCardinalArray;
  iChildren : Cardinal;
  iCodes : Cardinal;
  iName : Cardinal;
begin
  SetLength(aChildren, 0);
  id := 0;
  if op = FilterOperatorEqual then
  begin
    ok := true;
    if prop = 'SCALE_TYP' then
      id := FLoinc.GetPropertyId(lptScales, value)
    else if prop = 'CLASSTYPE' then
      id := FLoinc.GetPropertyId(lptClasses, value)
    else
      ok := false;
  end
  else
    ok := false;

  if not ok then
    result := nil
  else
  begin
    aChildren := nil;
    if (id <> 0) then
    begin
      FLoinc.Concepts.GetConcept(id, iName, iChildren, iCodes);
      aChildren := FLoinc.Refs.GetCardinals(iCodes);
    end;
    result := THolder.create;
    THolder(result).Children := aChildren;
  end;
end;

function TLoincProvider.FilterConcept(ctxt: TCodeSystemProviderFilterContext; ndx: integer): TCodeSystemProviderContext;
begin
  result := TObject(THolder(ctxt).children[ndx]);
end;

function TLoincProvider.FilterCount(ctxt: TCodeSystemProviderFilterContext): integer;
begin
  result := length(THolder(ctxt).children);
end;

function TLoincProvider.locateIsA(code, parent: String): TCodeSystemProviderContext;
begin
  result := nil; // cause loinc don't do subsumption
end;

function TLoincProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
var
  i : Cardinal;
  holder : THolder;
begin
  holder := THolder(ctxt);
  if FLoinc.Code.FindCode(code, i) and holder.hasChild(i) then
    result := TCodeSystemProviderContext(i)
  else
    result := nil;
end;

{ TSnomedProvider }

function TSnomedProvider.ChildCount(context: TCodeSystemProviderContext): integer;
var
  i : integer;
  Identity : int64;
  Flags, Group : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  iWork, iWork2, iWork3 : Cardinal;
  Inbounds : TCardinalArray;
begin
  SetLength(inbounds, 0);
  if (context = nil) then
    result := 1
  else
  begin
    FSnomed.Concept.GetConcept(Cardinal(context), Identity, Flags, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex);
    Inbounds := FSnomed.Refs.GetReferences(InboundIndex);
    result := 0;
    For i := 0 to High(Inbounds) Do
    Begin
      FSnomed.Rel.GetRelationship(Inbounds[i], iWork, iWork2, iWork3, Flags, Group);
      if iWork3 = FSnomed.Is_a_Index Then
        inc(result);
    End;
  end;
end;

function TSnomedProvider.Code(context: TCodeSystemProviderContext): string;
var
  Identity : int64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  Inbounds : TCardinalArray;
begin
  SetLength(inbounds, 0);
  FSnomed.Concept.GetConcept(Cardinal(context), Identity, Flags, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex);
  result := inttostr(identity);
end;

function TSnomedProvider.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
var
  i, c : integer;
  Identity : int64;
  Flags, Group : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  iWork, iWork2, iWork3 : Cardinal;
  Inbounds : TCardinalArray;
begin
  result := nil;
  SetLength(inbounds, 0);
  if (context = nil) then
    result := TCodeSystemProviderContext(FSnomed.Is_a_Index)
  else
  begin
    FSnomed.Concept.GetConcept(Cardinal(context), Identity, Flags, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex);
    Inbounds := FSnomed.Refs.GetReferences(InboundIndex);
    c := -1;
    For i := 0 to High(Inbounds) Do
    Begin
      FSnomed.Rel.GetRelationship(Inbounds[i], iWork, iWork2, iWork3, Flags, Group);
      if iWork3 = FSnomed.Is_a_Index Then
      begin
        inc(c);
        if (c = ndx) then
        begin
          result := TCodeSystemProviderContext(iWork);
          exit;
        end;
      end;
    End;
  end;
end;

constructor TSnomedProvider.create;
begin
  inherited;
  FSnomed := GSnomeds.DefaultDefinition;
end;

Function TSnomedProvider.GetFSN(iDescriptions : TCardinalArray) : String;
var
  iLoop : Integer;
  iid : int64;
  iString, iDummy : Cardinal;
  iFlag : Byte;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    FSnomed.Desc.GetDescription(iDescriptions[iLoop], iString, iId, iDummy, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE = VAL_DESC_FullySpecifiedName shl 4) Then
      result := FSnomed.Strings.GetEntry(iString);
  End;
End;


Function TSnomedProvider.GetPN(iDescriptions : TCardinalArray) : String;
var
  iLoop : Integer;
  iid : int64;
  iString, iDummy : Cardinal;
  iFlag : Byte;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    FSnomed.Desc.GetDescription(iDescriptions[iLoop], iString, iId, iDummy, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE = VAL_DESC_Preferred shl 4) Then
      result := FSnomed.Strings.GetEntry(iString);
  End;
  if result = '' Then
    result := GetFSN(iDescriptions);
End;

function TSnomedProvider.Display(context: TCodeSystemProviderContext): string;
var
  Identity : int64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  Descriptions : TCardinalArray;
begin
  FSnomed.Concept.GetConcept(Cardinal(context), Identity, Flags, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex);
  Descriptions := FSnomed.Refs.GetReferences(DescriptionIndex);
  result := getPn(Descriptions);
end;

function TSnomedProvider.getDisplay(code: String): String;
var
  ctxt : TObject;
begin
  ctxt := locate(code);
  if (ctxt = nil) then
    raise Exception.create('Unable to find '+code+' in '+system)
  else
    result := Display(ctxt);
end;

function TSnomedProvider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false; // snomed don't do abstract?
end;

function TSnomedProvider.locate(code: String): TCodeSystemProviderContext;
var
  iId : Int64;
  index : cardinal;
begin
  iId := StrToInt64Def(code, -1);
  if FSnomed.Concept.FindConcept(iId, index) Then
    result := TCodeSystemProviderContext(index)
  else
    raise exception.create('unable to find code '+code+' in '+system);
end;

function TSnomedProvider.system: String;
begin
  result := 'http://snomed.info/sct';
end;

function TSnomedProvider.TotalCount: integer;
begin
  result := FSnomed.Concept.Count;
end;

procedure TSnomedProvider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  raise Exception.Create('to do');
end;

function TSnomedProvider.filter(prop: String; op: TFhirFilterOperator; value: String): TCodeSystemProviderFilterContext;
begin
  result := nil;
end;

function TSnomedProvider.FilterConcept(ctxt: TCodeSystemProviderFilterContext; ndx: integer): TCodeSystemProviderContext;
begin
  result := nil;
  raise Exception.Create('to do');
end;

function TSnomedProvider.FilterCount(ctxt: TCodeSystemProviderFilterContext): integer;
begin
  result := 0;
  raise Exception.Create('to do');
end;

function TSnomedProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
begin
  result := nil;
  raise Exception.Create('to do');
end;

function TSnomedProvider.locateIsA(code, parent: String): TCodeSystemProviderContext;
var
  ic, ip : Cardinal;
begin
  if FSnomed.Concept.FindConcept(FSnomed.StringToId(parent), ip) And
       FSnomed.Concept.FindConcept(FSnomed.StringToId(code), ic) And FSnomed.Subsumes(ip, ic) then
    result := TObject(ic)
  else
    result := nil;
end;

{ TUcumProvider }

function TUcumProvider.ChildCount(context: TCodeSystemProviderContext): integer;
begin
  result := 0;
  raise Exception.Create('to do');
end;

function TUcumProvider.Code(context: TCodeSystemProviderContext): string;
begin
  result := '';
  raise Exception.Create('to do');
end;

function TUcumProvider.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
begin
  result := nil;
  raise Exception.Create('to do');
end;

function TUcumProvider.Display(context: TCodeSystemProviderContext): string;
begin
  result := '';
  raise Exception.Create('to do');
end;

function TUcumProvider.getDisplay(code: String): String;
begin
  result := '';
  raise Exception.Create('to do');
end;

function TUcumProvider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
  raise Exception.Create('to do');
end;

function TUcumProvider.locate(code: String): TCodeSystemProviderContext;
begin
  result := nil;
  raise Exception.Create('to do');
end;

function TUcumProvider.system: String;
begin
  result := '';
  raise Exception.Create('to do');
end;

function TUcumProvider.TotalCount: integer;
begin
  result := 0;
  raise Exception.Create('to do');
end;

procedure TUcumProvider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  raise Exception.Create('to do');
end;

function TUcumProvider.filter(prop: String; op: TFhirFilterOperator; value: String): TCodeSystemProviderFilterContext;
begin
  result := nil;
  raise Exception.Create('to do');
end;

function TUcumProvider.FilterConcept(ctxt: TCodeSystemProviderFilterContext; ndx: integer): TCodeSystemProviderContext;
begin
  result := nil;
  raise Exception.Create('to do');
end;

function TUcumProvider.FilterCount(ctxt: TCodeSystemProviderFilterContext): integer;
begin
  result := 0;
  raise Exception.Create('to do');
end;

function TUcumProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
begin
  result := nil;
  raise Exception.Create('to do');
end;

function TUcumProvider.locateIsA(code, parent: String): TCodeSystemProviderContext;
begin
  result := nil;
  raise Exception.Create('to do');
end;

{ TValueSetProvider }

constructor TValueSetProvider.create(vs: TFHIRValueSet);
begin
  Create;
  FVs := vs
end;

destructor TValueSetProvider.destroy;
begin
  FVs.free;
  inherited;
end;

function TValueSetProvider.ChildCount(context: TCodeSystemProviderContext): integer;
begin
  if context = nil then
    result := FVs.define.conceptList.count
  else
    result := TFhirValueSetDefineConcept(context).conceptList.count;
end;

function TValueSetProvider.Code(context: TCodeSystemProviderContext): string;
begin
  result := TFhirValueSetDefineConcept(context).codeST;
end;

function TValueSetProvider.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
begin
  result := TFhirValueSetDefineConcept(context).conceptList[ndx];
end;

function TValueSetProvider.Display(context: TCodeSystemProviderContext): string;
begin
    result := TFhirValueSetDefineConcept(context).displayST;
end;

function TValueSetProvider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := (TFhirValueSetDefineConcept(context).abstract = nil) or not TFhirValueSetDefineConcept(context).abstractST;
end;

function TValueSetProvider.getDisplay(code: String): String;
var
  ctxt : TObject;
begin
  ctxt := locate(code);
  if (ctxt = nil) then
    raise Exception.create('Unable to find '+code+' in '+system)
  else
    result := Display(ctxt);
end;

function TValueSetProvider.doLocate(list : TFhirValueSetDefineConceptList; code : String) : TCodeSystemProviderContext;
var
  i : integer;
  c : TFhirValueSetDefineConcept;
begin
  result := nil;
  for i := 0 to list.count - 1 do
  begin
    c := list[i];
    if (c.codeST = code) then
    begin
      result := c;
      exit;
    end;
    result := doLocate(c.conceptList, code);
    if result <> nil then
      exit;
  end;
end;

function TValueSetProvider.locate(code: String): TCodeSystemProviderContext;
begin
  result := DoLocate(FVS.define.conceptList, code);
end;

function TValueSetProvider.system: String;
begin
  result := Fvs.define.systemST;
end;

function TValueSetProvider.TotalCount: integer;
function count(item : TFhirValueSetDefineConcept) : integer;
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
  for i := 0 to FVs.define.conceptList.count - 1 do
    inc(result, count(FVs.define.conceptList[i]));
end;

procedure TValueSetProvider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  // nothing to do
end;

function TValueSetProvider.filter(prop: String; op: TFhirFilterOperator; value: String): TCodeSystemProviderFilterContext;
begin
  result := nil;
  // no know uses at the moment
end;

function TValueSetProvider.FilterConcept(ctxt: TCodeSystemProviderFilterContext; ndx: integer): TCodeSystemProviderContext;
begin
  raise Exception.create('called in error');
end;

function TValueSetProvider.FilterCount(ctxt: TCodeSystemProviderFilterContext): integer;
begin
  raise Exception.create('called in error');
end;

function TValueSetProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
begin
  raise Exception.create('called in error');
end;

function TValueSetProvider.locateIsA(code, parent: String): TCodeSystemProviderContext;
var
  p : TFhirValueSetDefineConcept;
begin
  result := nil;
  p := Locate(parent) as TFhirValueSetDefineConcept;
  if (p <> nil) then
    if (p.codeST = code) then
      result := p
    else
      result := doLocate(p.conceptList, code);
end;

{ TValueSetChecker }

constructor TValueSetChecker.create;
begin
  inherited;
  FOthers := TAdvStringObjectMatch.create;
end;

destructor TValueSetChecker.destroy;
begin
  FVs.Free;
  FOthers.Free;
  inherited;
end;

procedure TValueSetChecker.prepare(vs: TFHIRValueSet);
var
  i, j : integer;
  checker : TValueSetChecker;
  cs : TCodeSystemProvider;
  other : TFHIRValueSet;
begin
  FVs := vs.link;
  if fvs.define <> nil then
    FOthers.Add(fvs.define.systemST, TValueSetProvider.create(FVs.Link));
  if (fvs.compose <> nil) then
  begin
    for i := 0 to fvs.compose.importList.Count - 1 do
    begin
      other := (FValuesets.matches[fvs.compose.importList[i].value] as TFHIRValueSet).link;
      if other = nil then
        raise exception.create('Unable to find value set '+fvs.compose.importList[i].value);
      checker := TValueSetChecker.create;
      try
        checker.prepare(other);
        FOthers.Add(fvs.compose.importList[i].value, checker.Link);
      finally
        checker.free;
      end;
    end;
    for i := 0 to fvs.compose.includeList.Count - 1 do
    begin
      if not FOthers.ExistsByKey(fvs.compose.includeList[i].systemST) then
        FOthers.Add(fvs.compose.includeList[i].systemST, getCodeSystemProvider(fvs.compose.includeList[i]));
      cs := TCodeSystemProvider(FOthers.matches[fvs.compose.includeList[i].systemST]);
      for j := 0 to fvs.compose.includeList[i].filterList.count - 1 do
        if not (('concept' = fvs.compose.includeList[i].filterList[j].property_ST) and (fvs.compose.includeList[i].filterList[j].OpST = FilterOperatorIsA)) then
          if not cs.doesFilter(fvs.compose.includeList[i].filterList[j].property_ST, fvs.compose.includeList[i].filterList[j].OpST, fvs.compose.includeList[i].filterList[j].valueST) then
            raise Exception.create('The filter "'+fvs.compose.includeList[i].filterList[j].property_ST +' '+ CODES_TFhirFilterOperator[fvs.compose.includeList[i].filterList[j].OpST]+ ' '+fvs.compose.includeList[i].filterList[j].valueST+'" was not understood in the context of '+cs.system);
    end;
    for i := 0 to fvs.compose.excludeList.Count - 1 do
    begin
      if not FOthers.ExistsByKey(fvs.compose.excludeList[i].systemST) then
        FOthers.Add(fvs.compose.excludeList[i].systemST, getCodeSystemProvider(fvs.compose.excludeList[i]));
      cs := TCodeSystemProvider(FOthers.matches[fvs.compose.excludeList[i].systemST]);
      for j := 0 to fvs.compose.excludeList[i].filterList.count - 1 do
        if not (('concept' = fvs.compose.excludeList[i].filterList[j].property_ST) and (fvs.compose.excludeList[i].filterList[j].OpST = FilterOperatorIsA)) then
          if not cs.doesFilter(fvs.compose.excludeList[i].filterList[j].property_ST, fvs.compose.excludeList[i].filterList[j].OpST, fvs.compose.excludeList[i].filterList[j].valueST) then
            raise Exception.create('The filter "'+fvs.compose.excludeList[i].filterList[j].property_ST +' '+ CODES_TFhirFilterOperator[fvs.compose.excludeList[i].filterList[j].OpST]+ ' '+fvs.compose.excludeList[i].filterList[j].valueST+'" was not understood in the context of '+cs.system);
    end;
  end;
end;

function TValueSetChecker.findCode(code: String; list : TFhirValueSetDefineConceptList): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to list.count - 1 do
  begin
    if (code = list[i].codeST) or (findCode(code, list[i].conceptList)) then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TValueSetChecker.check(system, code: String): boolean;
var
  checker : TValueSetChecker;
  cs : TCodeSystemProvider;
  i : integer;
begin
  result := false;
  if (fvs.define <> nil) and (system = fvs.define.systemST) then
  begin
    result := FindCode(code, fvs.define.conceptList);
    if result then
      exit;
  end;
  if (fvs.compose <> nil) then
  begin
    for i := 0 to fvs.compose.importList.Count - 1 do
    begin
      if not result then
      begin
        checker := TValueSetChecker(FOthers.matches[fvs.compose.importList[i].value]);
        result := checker.check(system, code);
      end;
    end;
    for i := 0 to fvs.compose.includeList.Count - 1 do
    begin
      if not result then
      begin
        cs := TCodeSystemProvider(FOthers.matches[fvs.compose.includeList[i].systemST]);
        result := (cs.system = system) and checkConceptSet(cs, fvs.compose.includeList[i], code);
      end;
    end;
    for i := 0 to fvs.compose.excludeList.Count - 1 do
    begin
      if result then
      begin
        cs := TCodeSystemProvider(FOthers.matches[fvs.compose.excludeList[i].systemST]);
        result := not ((cs.system = system) and checkConceptSet(cs, fvs.compose.excludeList[i], code));
      end;
    end;
  end;
end;

function TValueSetChecker.checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeInclude; code: String): boolean;
var
  i : integer;
  fc : TFhirValueSetComposeIncludeFilter;
  ctxt : TCodeSystemProviderFilterContext;
begin
  result := false;
  if (cset.codeList.count = 0) and (cset.filterList.count = 0) then
    result := cs.locate(code) <> nil;

  for i := 0 to cset.codeList.count - 1 do
    if (code = cset.codeList[i].value) then
    begin
      result := true;
      exit;
    end;

  for i := 0 to cset.filterList.count - 1 do
  begin
    fc := cset.filterList[i];
    if ('concept' = fc.property_ST) and (fc.OpST = FilterOperatorIsA) then
      result := cs.locateIsA(code, fc.valueST) <> nil
    else
    begin
      ctxt := cs.filter(fc.property_ST, fc.OpST, fc.valueST);
      try
        result := cs.filterLocate(ctxt, code) <> nil;
      finally
        cs.close(ctxt);
      end;
    end;
    if result then
      exit;
  end;
end;

{ TCodeSystemProvider }

function TCodeSystemProvider.doesFilter(prop: String; op: TFhirFilterOperator; value: String): boolean;
var
  ctxt : TCodeSystemProviderFilterContext;
begin
  ctxt := filter(prop, op, value);
  result := ctxt <> nil;
  if result then
    Close(ctxt);
end;

end.
