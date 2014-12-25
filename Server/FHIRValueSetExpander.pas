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
  SysUtils, Classes, StringSupport, GuidSupport,
  AdvStringObjectMatches, AdvObjects, AdvObjectLists,
  FHIRBase, FHIRTypes, FHIRComponents, FHIRResources, FHIRUtilities, DateAndTime,
  TerminologyServices, LoincServices, SnomedServices, UcumServices,
  TerminologyServer;

const
  UPPER_LIMIT_NO_TEXT = 10000;
  UPPER_LIMIT_TEXT = 1000;

  // won't expand a value set bigger than this - just takes too long, and no one's going to do anything with it anyway

Type
  TFHIRValueSetExpander = class (TAdvObject)
  private
    FLimit : integer;
    FStore : TTerminologyServer;
    procedure addCodeAndDescendents(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cs : TCodeSystemProvider; context : TCodeSystemProviderContext);

    procedure handleDefine(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetDefine; defines : TFhirValueSetDefineConceptList; filter : TSearchFilterText);
    procedure importValueSet(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; uri : TFhirUri; filter : TSearchFilterText; dependencies : TStringList; allowIncomplete : boolean; var notClosed : boolean);
    procedure includeCodes(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cset : TFhirValueSetComposeInclude; filter : TSearchFilterText; allowIncomplete : boolean; var notClosed : boolean);
    procedure excludeCodes(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cset : TFhirValueSetComposeInclude; var notClosed : boolean);
    procedure handleCompose(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetCompose; filter : TSearchFilterText; dependencies : TStringList; allowIncomplete : boolean; var notClosed : boolean);

    procedure addCode(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, code, display, definition: string);
    procedure addDefinedCode(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; system : string; c : TFHIRValueSetDefineConcept);
    function key(system, code : String): string; overload;
    function key(c : TFhirValueSetExpansionContains) : string;  overload;
  public
    constructor Create(store : TTerminologyServer); overload;
    destructor Destroy; override;

    function expand(source : TFHIRValueSet; textFilter : String; dependencies : TStringList; limit : integer; allowIncomplete : boolean) : TFHIRValueSet;
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

function TFHIRValueSetExpander.Expand(source: TFHIRValueSet; textFilter : String; dependencies : TStringList; limit : integer; allowIncomplete : boolean): TFHIRValueSet;
var
  list : TFhirValueSetExpansionContainsList;
  map : TAdvStringObjectMatch;
  i : integer;
  c : TFhirValueSetExpansionContains;
  //e : TFhirExtension;
  filter : TSearchFilterText;
  notClosed : boolean;
begin
  result := source.Clone;
  result.xmlId := '';
  {$IFNDEF FHIR-DSTU}
  result.id := NewGuidURN;
  {$ENDIF}
  if (source.identifier <> '') then
    dependencies.Add(source.identifier);

  filter := TSearchFilterText.create(textFilter);
  map := TAdvStringObjectMatch.create;
  map.PreventDuplicates;
  list := TFhirValueSetExpansionContainsList.create;
  try
    if filter.null then
      FLimit := UPPER_LIMIT_NO_TEXT
    else
      FLimit := UPPER_LIMIT_TEXT;

    if (limit > 0) and (limit < FLimit) then
      FLimit := limit;

    result.expansion := TFhirValueSetExpansion.create;
    result.expansion.timestamp := NowUTC;
    //e := result.expansion.ExtensionList.Append;

    try
      if (source.define <> nil) then
        handleDefine(list, map, source.define, source.define.conceptList, filter);
      notClosed := false;
      if (source.compose <> nil) then
        handleCompose(list, map, source.compose, filter, dependencies, allowIncomplete, notClosed);
    except
      on e : ETooCostly do
      begin
        if allowIncomplete then
          result.expansion.addExtension('http://hl7.org/fhir/Profile/questionnaire-extensions#toocostly', TFhirBoolean.Create(true))
        else
          raise
      end;
      on e : Exception do
        raise;
    end;
    if notClosed then
      result.expansion.addExtension('http://hl7.org/fhir/Profile/questionnaire-extensions#closed', TFhirBoolean.Create(true));

    for i := 0 to list.count - 1 do
    begin
      c := list[i];
      if map.ExistsByKey(key(c)) then
        result.Expansion.containsList.add(c.link);
    end;

// make it faster    BuildNarrative(result);
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
  result := key(c.System, c.Code);
end;

procedure TFHIRValueSetExpander.handleCompose(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source: TFhirValueSetCompose; filter : TSearchFilterText; dependencies : TStringList; allowIncomplete : boolean; var notClosed : boolean);
var
  i : integer;
begin
  for i := 0 to source.importList.count - 1 do
    importValueSet(list, map, source.importList[i], filter, dependencies, allowIncomplete, notClosed);
  for i := 0 to source.includeList.count - 1 do
    includeCodes(list, map, source.includeList[i], filter, allowIncomplete, notClosed);
  for i := 0 to source.excludeList.count - 1 do
    excludeCodes(list, map, source.excludeList[i], notClosed);
end;

procedure TFHIRValueSetExpander.handleDefine(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source : TFhirValueSetDefine; defines : TFhirValueSetDefineConceptList; filter : TSearchFilterText);
var
  i : integer;
  cm : TFhirValueSetDefineConcept;
begin
  for i := 0 to defines.count - 1 do
  begin
    cm := defines[i];
    if filter.passes(cm.display) or filter.passes(cm.code) then
      addDefinedCode(list, map, source.system, cm);
    handleDefine(list, map, source, cm.conceptList, filter);
  end;
end;

procedure TFHIRValueSetExpander.addDefinedCode(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system: string; c: TFHIRValueSetDefineConcept);
var
  i : integer;
begin
  if (c.abstractElement = nil) or not c.Abstract then
    addCode(list, map, system, c.Code, c.Display, c.definition);
  for i := 0 to c.conceptList.count - 1 do
    addDefinedCode(list, map, system, c.conceptList[i]);
end;

procedure TFHIRValueSetExpander.addCode(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, code, display, definition: string);
var
  n : TFHIRValueSetExpansionContains;
  s : String;
begin
    if (map.Count > FLimit) then
      raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');

  n := TFHIRValueSetExpansionContains.create;
  try
    n.System := system;
    n.Code := code;
    if (display <> '') then
      n.Display := display
    else
      n.Display := code;
    s := key(n);
    if not map.ExistsByKey(s) then
    begin
      list.add(n.link);
      map.add(s, n.link);
    end;
    if definition <> '' then
      n.setExtensionString('http://hl7.org/fhir/Profile/tools-extensions#definition', definition);
  finally
    n.free;
  end;
end;


procedure TFHIRValueSetExpander.importValueSet(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; uri: TFhirUri; filter : TSearchFilterText; dependencies : TStringList; allowIncomplete : boolean; var notClosed : boolean);
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
    vs := FStore.expandVS(uri.value, filter.filter, dep, FLimit, allowIncomplete);
    if (vs = nil) then
      raise Exception.create('unable to find value set '+uri.value);
    try
      if vs.expansion.hasextension('http://hl7.org/fhir/Profile/questionnaire-extensions#closed') then
        notClosed := true;

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

procedure TFHIRValueSetExpander.includeCodes(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cset: TFhirValueSetComposeInclude; filter : TSearchFilterText; allowIncomplete : boolean; var notClosed : boolean);
var
  cs : TCodeSystemProvider;
  i, offset : integer;
  fc : TFhirValueSetComposeIncludeFilter;
  c : TCodeSystemProviderContext;
  filters : Array of TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderFilterContext;
  ok : boolean;
  prep : TCodeSystemProviderFilterPreparationContext;
  inner : boolean;
begin
  cs := FStore.getProvider(cset.system);
  try
    if (cset.conceptList.count = 0) and (cset.filterList.count = 0) then
    begin
      // special case - add all the code system
      if filter.Null then
      begin
        if (cs.TotalCount > FLimit) and not (allowIncomplete) then
          raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');
        for i := 0 to cs.ChildCount(nil) - 1 do
          addCodeAndDescendents(list, map, cs, cs.getcontext(nil, i))
      end
      else
      begin
        if cs.isNotClosed(filter) then
          notClosed := true;
        prep := cs.getPrepContext;
        try
          ctxt := cs.searchFilter(filter, prep);
          try
            cs.prepare(prep);
            i := 0;
            while cs.FilterMore(ctxt) do
            begin
              inc(i);
              if i > FLimit then
                raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');
              c := cs.FilterConcept(ctxt);
              addCode(list, map, cs.system(c), cs.code(c), cs.display(c), cs.definition(c));
            end;
          finally
            cs.Close(ctxt);
          end;
        finally
          cs.Close(prep);
        end;
      end;
    end;

    for i := 0 to cset.conceptList.count - 1 do
      if filter.passes(cs.getDisplay(cset.conceptList[i].code)) then
        addCode(list, map, cs.system(nil), cset.conceptList[i].code, cs.getDisplay(cset.conceptList[i].code), cs.getDefinition(cset.conceptList[i].code));

    if cset.filterList.Count > 0 then
    begin
      prep := cs.getPrepContext;
      try
        if filter.null then
        begin
          SetLength(filters, cset.filterList.count);
          offset := 0;
        end
        else
        begin
          SetLength(filters, cset.filterList.count+1);
          offset := 1;
          filters[0] := cs.searchFilter(filter, prep); // this comes first, because it imposes order
        end;

        for i := 0 to cset.filterList.count - 1 do
        begin
          fc := cset.filterList[i];
          filters[i+offset] := cs.filter(fc.property_, fc.Op, fc.value, prep);
          if filters[i+offset] = nil then
            raise Exception.create('The filter "'+fc.property_ +' '+ CODES_TFhirFilterOperator[fc.Op]+ ' '+fc.value+'" was not understood in the context of '+cs.system(nil));
          if cs.isNotClosed(filter, filters[i+offset]) then
            notClosed := true;
        end;

        inner := not cs.prepare(prep);
        While cs.FilterMore(filters[0]) do
        begin
          c := cs.FilterConcept(filters[0]);
          ok := true;
          if inner then
            for i := 1 to length(filters) - 1 do
              ok := ok and cs.InFilter(filters[i], c);
          if ok then
            addCode(list, map, cs.system(nil), cs.code(c), cs.display(c), cs.definition(c));
        end;
        for i := 0 to length(filters) - 1 do
          cs.Close(filters[i]);
      finally
        prep.free;
      end;
    end;
  finally
    cs.free;
  end;
end;

procedure TFHIRValueSetExpander.excludeCodes(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cset: TFhirValueSetComposeInclude; var notClosed : boolean);
begin
  raise exception.Create('Not done yet');
end;

procedure TFHIRValueSetExpander.addCodeAndDescendents(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cs: TCodeSystemProvider; context: TCodeSystemProviderContext);
var
  i : integer;
begin
  if not cs.IsAbstract(context) then
    addCode(list, map, cs.system(context), cs.Code(context), cs.Display(context), cs.definition(context));
  for i := 0 to cs.ChildCount(context) - 1 do
    addCodeAndDescendents(list, map, cs, cs.getcontext(context, i));
end;


{ TUcumProvider }


end.
