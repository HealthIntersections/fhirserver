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
  AdvStringObjectMatches, AdvObjects, AdvObjectLists, AdvExceptions,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, DateAndTime,
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
    procedure processCodeAndDescendents(doDelete : boolean; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cs : TCodeSystemProvider; context : TCodeSystemProviderContext; params : TFhirValueSetExpansionParameterList);

    procedure handleDefine(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetCodeSystem; defines : TFhirValueSetCodeSystemConceptList; filter : TSearchFilterText; params : TFhirValueSetExpansionParameterList);
    procedure importValueSet(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; uri : TFhirUri; profile : String; filter : TSearchFilterText; dependencies : TStringList; allowIncomplete : boolean; params : TFhirValueSetExpansionParameterList; var notClosed : boolean);
    procedure processCodes(doDelete : boolean; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cset : TFhirValueSetComposeInclude; profile : String; filter : TSearchFilterText; allowIncomplete : boolean; params : TFhirValueSetExpansionParameterList; var notClosed : boolean);
    procedure handleCompose(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetCompose; profile : String; filter : TSearchFilterText; dependencies : TStringList; allowIncomplete : boolean; params : TFhirValueSetExpansionParameterList; var notClosed : boolean);

    procedure processCode(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, version, code, display, definition: string; params : TFhirValueSetExpansionParameterList);
    procedure addDefinedCode(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; system : string; c : TFhirValueSetCodeSystemConcept);
    function key(system, code : String): string; overload;
    function key(c : TFhirValueSetExpansionContains) : string;  overload;
  public
    constructor Create(store : TTerminologyServer); overload;
    destructor Destroy; override;

    function expand(source : TFHIRValueSet; profile : String; textFilter : String; dependencies : TStringList; limit : integer; allowIncomplete : boolean) : TFHIRValueSet;
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

function TFHIRValueSetExpander.Expand(source: TFHIRValueSet; profile : String; textFilter : String; dependencies : TStringList; limit : integer; allowIncomplete : boolean): TFHIRValueSet;
var
  list : TFhirValueSetExpansionContainsList;
  map : TAdvStringObjectMatch;
  i : integer;
  c : TFhirValueSetExpansionContains;
  //e : TFhirExtension;
  filter : TSearchFilterText;
  notClosed : boolean;
  ref : TFhirReference;
  table : TFhirXHtmlNode;
  tr : TFhirXHtmlNode;
  param : TFhirValueSetExpansionParameter;
begin
  result := source.Clone;
  if profile.startsWith('http://www.healthintersections.com.au/fhir/expansion/no-details') then
  begin
    result.codeSystem := nil;
    result.compose := nil;
    result.description := '';
    result.contactList.Clear;
    result.requirements := '';
    result.copyright := '';
    result.publisher := '';
    result.extensionList.Clear;
    result.text := nil;
    table := nil;
  end;

  if (profile <> 'http://www.healthintersections.com.au/fhir/expansion/no-details') then
  begin
    if result.text = nil then
      result.text := TFhirNarrative.Create;
    result.text.status := NarrativeStatusGenerated;
    result.text.div_ := TFhirXHtmlNode.Create('div');
    table := result.text.div_.AddTag('table').setAttribute('class', 'grid');
  end;

  if (result.expansion <> nil) then
    exit; // just return the expansion

  result.xmlId := '';
  if (source.url <> '') then
    dependencies.Add(source.url);

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
    result.expansion.identifier := NewGuidURN;

    if source.id <> '' then
    begin
      param := result.expansion.parameterList.Append;
      param.name := 'expansion-source';
      param.value := TFHIRString.Create('ValueSet/'+source.id);
    end;

    try
      if (source.codeSystem <> nil) then
        handleDefine(list, map, source.codeSystem, source.codeSystem.conceptList, filter, result.expansion.parameterList);
      notClosed := false;
      if (source.compose <> nil) then
        handleCompose(list, map, source.compose, profile, filter, dependencies, allowIncomplete, result.expansion.parameterList, notClosed);
    except
      on e : ETooCostly do
      begin
        if allowIncomplete then
        begin
          result.expansion.addExtension('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', TFhirBoolean.Create(true));
          if (table <> nil) then
            result.text.div_.addTag('p').setAttribute('style', 'color: Maroon').addText(e.message);
        end
        else
        begin
          recordStack(e);
          raise;
        end;
      end;
      on e : Exception do
      begin
        recordStack(e);
        raise;
      end;
    end;
    if notClosed then
    begin
      result.expansion.addExtension('http://hl7.org/fhir/StructureDefinition/valueset-unclosed', TFhirBoolean.Create(true));
      if (table <> nil) then
        result.text.div_.addTag('p').setAttribute('style', 'color: Navy').addText('Because of the way that this value set is defined, not all the possible codes can be listed in advance');
    end;

    for i := 0 to list.count - 1 do
    begin
      c := list[i];
      if map.ExistsByKey(key(c)) then
        result.Expansion.containsList.add(c.link);
      if (table <> nil) then
      begin
        tr := table.AddChild('tr');
        tr.AddChild('td').AddText(c.system);
        tr.AddChild('td').AddText(c.code);
        tr.AddChild('td').AddText(c.display);
      end;
    end;

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

procedure TFHIRValueSetExpander.handleCompose(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source: TFhirValueSetCompose; profile : String; filter : TSearchFilterText; dependencies : TStringList; allowIncomplete : boolean; params : TFhirValueSetExpansionParameterList; var notClosed : boolean);
var
  i : integer;
begin
  for i := 0 to source.importList.count - 1 do
    importValueSet(list, map, source.importList[i], profile, filter, dependencies, allowIncomplete, params, notClosed);
  for i := 0 to source.includeList.count - 1 do
    processCodes(false, list, map, source.includeList[i], profile, filter, allowIncomplete, params, notClosed);
  for i := 0 to source.excludeList.count - 1 do
    processCodes(true, list, map, source.excludeList[i], profile, filter, allowIncomplete, params, notClosed);
end;

procedure TFHIRValueSetExpander.handleDefine(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source : TFhirValueSetCodeSystem; defines : TFhirValueSetCodeSystemConceptList; filter : TSearchFilterText; params : TFhirValueSetExpansionParameterList);
var
  i : integer;
  cm : TFhirValueSetCodeSystemConcept;
  param : TFhirValueSetExpansionParameter;
begin
  if (defines.Count > 0) and (params <> nil) and (source.version <> '') then
  begin
    param := params.Append;
    param.name := 'version';
    param.value := TFHIRUri.Create(source.system+'?version='+source.version);
  end;
  for i := 0 to defines.count - 1 do
  begin
    cm := defines[i];
    if filter.passes(cm.display) or filter.passes(cm.code) then
      addDefinedCode(list, map, source.system, cm);
    handleDefine(list, map, source, cm.conceptList, filter, nil);
  end;
end;

procedure TFHIRValueSetExpander.addDefinedCode(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system: string; c: TFhirValueSetCodeSystemConcept);
var
  i : integer;
begin
  if (c.abstractElement = nil) or not c.Abstract then
    processCode(false, list, map, system, '', c.Code, c.Display, c.definition, nil);
  for i := 0 to c.conceptList.count - 1 do
    addDefinedCode(list, map, system, c.conceptList[i]);
end;

procedure TFHIRValueSetExpander.processCode(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, version, code, display, definition: string; params : TFhirValueSetExpansionParameterList);
var
  n : TFHIRValueSetExpansionContains;
  s : String;
  f : boolean;
  param : TFhirValueSetExpansionParameter;
begin
  if (map.Count >= FLimit) and not doDelete then
    raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');

  if (params <> nil) and (version <> '') then
  begin
    f := false;
    s := system+'?version='+version;
    for param in params do
      if (param.name = 'version') and (param.value.primitiveValue = s) then
        f := true;
    if not f then
    begin
      param := params.Append;
      param.name := 'version';
      param.value := TFHIRUri.Create(s);
    end;
  end;

  n := TFHIRValueSetExpansionContains.create;
  try
    n.System := system;
    n.Code := code;
    if (display <> '') then
      n.Display := display
    else
      n.Display := code;
    s := key(n);
    if (dodelete) then
    begin
      if map.ExistsByKey(s) then
      begin
        list.DeleteByReference(map.Matches[s]);
        map.DeleteByKey(s);
      end;
    end
    else if not map.ExistsByKey(s) then
    begin
      list.add(n.link);
      map.add(s, n.link);
    end;
    if definition <> '' then
      n.setExtensionString('http://hl7.org/fhir/StructureDefinition/valueset-definition', definition);
  finally
    n.free;
  end;
end;


procedure TFHIRValueSetExpander.importValueSet(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; uri: TFhirUri; profile : String; filter : TSearchFilterText; dependencies : TStringList; allowIncomplete : boolean; params : TFhirValueSetExpansionParameterList; var notClosed : boolean);
var
  vs : TFHIRValueSet;
  i : integer;
  c : TFhirValueSetExpansionContains;
  s : String;
  dep : TStringList;
  param : TFhirValueSetExpansionParameter;
begin
  if (uri = nil) then
    raise Exception.create('unable to find value set with no identity');
  dep := TStringList.Create;
  try
    vs := FStore.expandVS(uri.value, profile, filter.filter, dep, FLimit, allowIncomplete);
    if (vs = nil) then
      raise Exception.create('unable to find value set '+uri.value);
    try
      if vs.expansion.hasextension('http://hl7.org/fhir/Profile/questionnaire-extensions#closed') then
        notClosed := true;

      for param in vs.expansion.parameterList do
        params.Add(param.Link);

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

procedure TFHIRValueSetExpander.processCodes(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cset: TFhirValueSetComposeInclude; profile : String; filter : TSearchFilterText; allowIncomplete : boolean; params : TFhirValueSetExpansionParameterList; var notClosed : boolean);
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
  display : String;
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
          processCodeAndDescendents(doDelete, list, map, cs, cs.getcontext(nil, i), params)
      end
      else
      begin
        if cs.isNotClosed(filter) then
          notClosed := true;
        prep := cs.getPrepContext;
        try
          ctxt := cs.searchFilter(filter, prep, false);
          try
            cs.prepare(prep);
            i := 0;
            while cs.FilterMore(ctxt) do
            begin
              inc(i);
              if i > FLimit then
                raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');
              c := cs.FilterConcept(ctxt);
              processCode(doDelete, list, map, cs.system(c), cs.version(c), cs.code(c), cs.display(c), cs.definition(c), params);
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
    begin
      display := cset.conceptList[i].display;
      if display = '' then
        display := cs.getDisplay(cset.conceptList[i].code);
      if filter.passes(display) or filter.passes(cset.conceptList[i].code) then
        processCode(doDelete, list, map, cs.system(nil), cs.version(nil), cset.conceptList[i].code, display, cs.getDefinition(cset.conceptList[i].code), params);
    end;

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
          filters[0] := cs.searchFilter(filter, prep, true); // this comes first, because it imposes order
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
            processCode(doDelete, list, map, cs.system(nil), cs.version(nil), cs.code(c), cs.display(c), cs.definition(c), params);
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

procedure TFHIRValueSetExpander.processCodeAndDescendents(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cs: TCodeSystemProvider; context: TCodeSystemProviderContext; params : TFhirValueSetExpansionParameterList);
var
  i : integer;
  param : TFhirValueSetExpansionParameter;
begin
  if (cs.version(nil) <> '') and (params <> nil) then
  begin
    param := params.Append;
    param.name := 'version';
    param.value := TFHIRUri.Create(cs.system(nil)+'?version='+cs.version(nil));
  end;

  if not cs.IsAbstract(context) then
    processCode(doDelete, list, map, cs.system(context), '', cs.Code(context), cs.Display(context), cs.definition(context), nil);
  for i := 0 to cs.ChildCount(context) - 1 do
    processCodeAndDescendents(doDelete, list, map, cs, cs.getcontext(context, i), nil);
end;


end.
