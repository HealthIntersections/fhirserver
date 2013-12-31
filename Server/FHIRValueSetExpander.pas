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
  SysUtils, Classes, StringSupport,
  AdvStringObjectMatches, AdvObjects, AdvObjectLists,
  FHIRBase, FHIRTypes, FHIRComponents, FHIRResources, FHIRUtilities, DateAndTime,
  TerminologyServices, LoincServices, SnomedServices, UcumServices,
  TerminologyServer;

const
  UPPER_LIMIT = 10000;
  // won't expand a value set bigger than this - just takes too long, and no one's going to do anything with it anyway

Type
  TFHIRValueSetExpander = class (TAdvObject)
  private
    FStore : TTerminologyServer;
    procedure addCodeAndDescendents(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cs : TCodeSystemProvider; context : TCodeSystemProviderContext);

    procedure handleDefine(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetDefine; defines : TFhirValueSetDefineConceptList; filter : TSearchFilterText);
    procedure importValueSet(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; uri : TFhirUri; filter : TSearchFilterText; dependencies : TStringList);
    procedure includeCodes(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cset : TFhirValueSetComposeInclude; filter : TSearchFilterText);
    procedure excludeCodes(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cset : TFhirValueSetComposeInclude);
    procedure handleCompose(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetCompose; filter : TSearchFilterText; dependencies : TStringList);

    procedure addCode(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, code, display: string);
    procedure addDefinedCode(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; system : string; c : TFHIRValueSetDefineConcept);
    function key(system, code : String): string; overload;
    function key(c : TFhirValueSetExpansionContains) : string;  overload;
  public
    constructor create(store : TTerminologyServer); overload;
    destructor destroy; override;

    function expand(source : TFHIRValueSet; textFilter : String; dependencies : TStringList) : TFHIRValueSet;
  end;


implementation

{ TFHIRValueSetExpander }

constructor TFHIRValueSetExpander.create(store : TTerminologyServer);
begin
  Create;
  FStore := store;
end;

destructor TFHIRValueSetExpander.destroy;
begin
  FStore.free;
  inherited;
end;

function TFHIRValueSetExpander.Expand(source: TFHIRValueSet; textFilter : String; dependencies : TStringList): TFHIRValueSet;
var
  list : TFhirValueSetExpansionContainsList;
  map : TAdvStringObjectMatch;
  i : integer;
  c : TFhirValueSetExpansionContains;
  e : TFhirExtension;
  filter : TSearchFilterText;
begin
  result := source.Clone;
  dependencies.Add(source.identifierST);

  filter := TSearchFilterText.create(textFilter);
  map := TAdvStringObjectMatch.create;
  map.PreventDuplicates;
  list := TFhirValueSetExpansionContainsList.create;
  try
    result.expansion := TFhirValueSetExpansion.create;
    result.expansion.timestampST := NowUTC;
    e := result.expansion.ExtensionList.Append;

    if (source.define <> nil) then
      handleDefine(list, map, source.define, source.define.conceptList, filter);
    if (source.compose <> nil) then
      handleCompose(list, map, source.compose, filter, dependencies);


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
    result.free;
    filter.Free;
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

procedure TFHIRValueSetExpander.handleCompose(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source: TFhirValueSetCompose; filter : TSearchFilterText; dependencies : TStringList);
var
  i : integer;
begin
  for i := 0 to source.importList.count - 1 do
    importValueSet(list, map, source.importList[i], filter, dependencies);
  for i := 0 to source.includeList.count - 1 do
    includeCodes(list, map, source.includeList[i], filter);
  for i := 0 to source.importList.count - 1 do
    excludeCodes(list, map, source.excludeList[i]);
end;

procedure TFHIRValueSetExpander.handleDefine(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source : TFhirValueSetDefine; defines : TFhirValueSetDefineConceptList; filter : TSearchFilterText);
var
  i : integer;
  cm : TFhirValueSetDefineConcept;
begin
  for i := 0 to defines.count - 1 do
  begin
    cm := defines[i];
    if filter.passes(cm.displayST) then
      addDefinedCode(list, map, source.systemST, cm);
    handleDefine(list, map, source, cm.conceptList, filter);
  end;
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
    if (map.Count > UPPER_LIMIT) then
      raise Exception.create('Value set size of '+inttostr(map.count)+' exceeds upper limit of '+inttostr(UPPER_LIMIT));

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


procedure TFHIRValueSetExpander.importValueSet(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; uri: TFhirUri; filter : TSearchFilterText; dependencies : TStringList);
var
  vs : TFHIRValueSet;
  i : integer;
  c : TFhirValueSetExpansionContains;
  s : String;
  dep : TStringList;
begin
  if (uri = nil) then
    raise Exception.create('unable to find value set with no identity');
  dep := TStringList.Create;
  try
    vs := FStore.expandVS(uri.value, filter.filter, dep);
    if (vs = nil) then
      raise Exception.create('unable to find value set '+uri.value);
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
      dependencies.AddStrings(dep);
    finally
      vs.free;
    end;
  finally
    dep.free;
  end;
end;

procedure TFHIRValueSetExpander.includeCodes(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cset: TFhirValueSetComposeInclude; filter : TSearchFilterText);
var
  cs : TCodeSystemProvider;
  i, j, offset : integer;
  fc : TFhirValueSetComposeIncludeFilter;
  c : TCodeSystemProviderContext;
  filters : Array of TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderFilterContext;
  ok : boolean;
begin
  cs := FStore.getProvider(cset.systemST);
  try
    if (cset.codeList.count = 0) and (cset.filterList.count = 0) then
    begin
      // special case - add all the code system
      if cs.TotalCount > UPPER_LIMIT then
        raise exception.create('code system '+cs.system+' too big to include as a whole');
      if filter.Null then
        for i := 0 to cs.ChildCount(nil) - 1 do
          addCodeAndDescendents(list, map, cs, cs.getcontext(nil, i))
      else
      begin
        ctxt := cs.searchFilter(filter);
        try
          for i := 0 to cs.FilterCount(ctxt) - 1 do
          begin
            c := cs.FilterConcept(ctxt, j);
            addCode(list, map, cs.system, cs.code(c), cs.display(c));
          end;
        finally
          cs.Close(ctxt);
        end;
      end;
    end;

    for i := 0 to cset.codeList.count - 1 do
      if filter.passes(cs.getDisplay(cset.codeList[i].value)) then
        addCode(list, map, cs.system, cset.codeList[i].value, cs.getDisplay(cset.codeList[i].value));

    if cset.filterList.Count > 0 then
    begin
      if filter.null then
      begin
        SetLength(filters, cset.filterList.count);
        offset := 0;
      end
      else
      begin
        SetLength(filters, cset.filterList.count+1);
        offset := 1;
        filters[0] := cs.searchFilter(filter); // this comes first, because it imposes order
      end;

      for i := 0 to cset.filterList.count - 1 do
      begin
        fc := cset.filterList[i];
        filters[i+offset] := cs.filter(fc.property_ST, fc.OpST, fc.valueST);
        if filters[i+offset] = nil then
          raise Exception.create('The filter "'+fc.property_ST +' '+ CODES_TFhirFilterOperator[fc.OpST]+ ' '+fc.valueST+'" was not understood in the context of '+cs.system);
      end;

      for j := 0 to cs.FilterCount(filters[0]) - 1 do
      begin
        c := cs.FilterConcept(filters[0], j);
        ok := true;
        for i := 1 to length(filters) - 1 do
          ok := ok and cs.InFilter(filters[i], c);
        if ok then
          addCode(list, map, cs.system, cs.code(c), cs.display(c));
      end;
      for i := 0 to length(filters) - 1 do
        cs.Close(filters[i]);
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


{ TUcumProvider }


end.
