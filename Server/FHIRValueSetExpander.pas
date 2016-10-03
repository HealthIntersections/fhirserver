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

{
todo:
  include designations
  include Inactive

}
uses
  SysUtils, Classes, StringSupport, GuidSupport,
  AdvStringObjectMatches, AdvObjects, AdvObjectLists, AdvExceptions,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, DateAndTime,
  TerminologyServices, LoincServices, SnomedServices, UcumServices,
  TerminologyServer, TerminologyServerStore;

const
  UPPER_LIMIT_NO_TEXT = 10000;
  UPPER_LIMIT_TEXT = 1000;

  // won't expand a value set bigger than this - just takes too long, and no one's going to do anything with it anyway

Type
  TFHIRValueSetExpander = class (TAdvObject)
  private
    FLimit : integer;
    FCount : integer;
    FOffset : integer;
    FProfile : TFhirExpansionProfile;

    FStore : TTerminologyServer;
    procedure processCodeAndDescendants(doDelete : boolean; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cs : TCodeSystemProvider; context : TCodeSystemProviderContext; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile);

    procedure handleDefine(cs : TFhirCodeSystem; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirCodeSystem2; defines : TFhirCodeSystemConceptList; filter : TSearchFilterText; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile);
    procedure importValueSet(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; uri : String; filter : TSearchFilterText; dependencies : TStringList; params : TFhirValueSetExpansionParameterList; var notClosed : boolean);
    procedure processCodes(doDelete : boolean; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cset : TFhirValueSetComposeInclude; filter : TSearchFilterText; dependencies : TStringList; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile; var notClosed : boolean);
    procedure handleCompose(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetCompose; filter : TSearchFilterText; dependencies : TStringList; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile; var notClosed : boolean);

    procedure processCode(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, version, code, display, definition: string; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile);
    procedure addDefinedCode(cs : TFhirCodeSystem; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; system : string; c : TFhirCodeSystemConcept; profile : TFhirExpansionProfile);
    function key(system, code : String): string; overload;
    function key(c : TFhirValueSetExpansionContains) : string;  overload;
    function chooseDisplay(c: TFhirCodeSystemConcept;  profile: TFHIRExpansionProfile): String; overload;
    function chooseDisplay(c: TFhirValueSetComposeIncludeConcept;  profile: TFHIRExpansionProfile): String; overload;
  public
    constructor Create(store : TTerminologyServer); overload;
    destructor Destroy; override;

    function expand(source : TFHIRValueSet; profile : TFhirExpansionProfile; textFilter : String; dependencies : TStringList; limit, count, offset : integer) : TFHIRValueSet;
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
  FProfile.Free;
  FStore.free;
  inherited;
end;

function TFHIRValueSetExpander.Expand(source: TFHIRValueSet; profile : TFhirExpansionProfile; textFilter : String; dependencies : TStringList; limit, count, offset : integer): TFHIRValueSet;
var
  list : TFhirValueSetExpansionContainsList;
  map : TAdvStringObjectMatch;
  i, t, o : integer;
  c : TFhirValueSetExpansionContains;
  //e : TFhirExtension;
  filter : TSearchFilterText;
  notClosed : boolean;
  table : TFhirXHtmlNode;
  tr : TFhirXHtmlNode;
  param : TFhirValueSetExpansionParameter;
begin
  source.checkNoImplicitRules('ValueSetExpander.Expand', 'ValueSet');
  source.checkNoModifiers('ValueSetExpander.Expand', 'ValueSet');
  {$IFDEF FHIR3}
  profile.checkNoImplicitRules('ValueSetExpander.Expand', 'ExpansionProfile');
  profile.checkNoModifiers('ValueSetExpander.Expand', 'ExpansionProfile');
  {$ENDIF}

  FProfile := profile.Link;

  result := source.Clone;
  if not profile.includeDefinition then
  begin
    {$IFDEF FHIR2}
    result.codeSystem := nil;
    {$ENDIF}
    result.compose := nil;
    result.description := '';
    result.contactList.Clear;
    result.requirements := '';
    result.copyright := '';
    result.publisher := '';
    result.extensionList.Clear;
    result.text := nil;
    table := nil;
  end
  else
  begin
    if result.text = nil then
      result.text := TFhirNarrative.Create;
    result.text.status := NarrativeStatusGenerated;
    result.text.div_ := TFhirXHtmlNode.Create('div');
    table := result.text.div_.AddTag('table').setAttribute('class', 'grid');
  end;

  result.status := source.status;
  if result.status = ConformanceResourceStatusNull then
    result.status := ConformanceResourceStatusDraft;

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
    FCount := count;
    FOffset := offset;

    result.expansion := TFhirValueSetExpansion.create;
    result.expansion.timestamp := NowUTC;
    result.expansion.identifier := NewGuidURN;

    if source.id <> '' then
      result.expansion.addParam('expansion-source', 'ValueSet/'+source.id)
    else if source.url <> '' then
      result.expansion.addParam('expansion-source', source.url);
    {$IFDEF FHIR3}
    if profile.url <> '' then
      result.expansion.addParam('expansion-profile', profile.url);
    {$ENDIF}
    if profile.limitedExpansion then
      result.expansion.addParam('limitedExpansion', profile.limitedExpansion);
    if profile.displayLanguage <> '' then
      result.expansion.addParam('displayLanguage', profile.displayLanguage);
    if profile.includeDesignations then
      result.expansion.addParam('includeDesignations', profile.includeDesignations);
    if profile.includeDefinition then
      result.expansion.addParam('includeDefinition', profile.includeDefinition);
    if profile.includeInactive then
      result.expansion.addParam('includeInactive', profile.includeInactive);
    if profile.excludeNested then
      result.expansion.addParam('excludeNested', profile.excludeNested);
    if profile.excludeNotForUI then
      result.expansion.addParam('excludeNotForUI', profile.excludeNotForUI);
    if profile.excludePostCoordinated then
      result.expansion.addParam('excludePostCoordinated', profile.excludePostCoordinated);


    try
      {$IFDEF FHIR2}
      if (source.codeSystem <> nil) then
      begin
        source.codeSystem.checkNoModifiers('ValueSetExpander.Expand', 'code system');
        handleDefine(source, list, map, source.codeSystem, source.codeSystem.conceptList, filter, result.expansion.parameterList, profile);
      end;
      {$ENDIF}
      notClosed := false;
      if (source.compose <> nil) then
      begin
        source.compose.checkNoModifiers('ValueSetExpander.Expand', 'compose');
        handleCompose(list, map, source.compose, filter, dependencies, result.expansion.parameterList, profile, notClosed);
      end;
    except
      on e : ETooCostly do
      begin
        if FProfile.limitedExpansion then
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

    t := 0;
    o := 0;
    for i := 0 to list.count - 1 do
    begin
      c := list[i];
      if map.ExistsByKey(key(c)) then
      begin
        inc(o);
        if (o >= offset) and ((count = 0) or (t < count)) then
        begin
          inc(t);
          result.Expansion.containsList.add(c.link);
          if (table <> nil) then
          begin
            tr := table.AddChild('tr');
            tr.AddChild('td').AddText(c.system);
            tr.AddChild('td').AddText(c.code);
            tr.AddChild('td').AddText(c.display);
          end;
        end;
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

procedure TFHIRValueSetExpander.handleCompose(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source: TFhirValueSetCompose; filter : TSearchFilterText; dependencies : TStringList; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile; var notClosed : boolean);
var
  i : integer;
begin
  for i := 0 to source.importList.count - 1 do
    importValueSet(list, map, source.importList[i].value, filter, dependencies, params, notClosed);
  for i := 0 to source.includeList.count - 1 do
    processCodes(false, list, map, source.includeList[i], filter, dependencies, params, profile, notClosed);
  for i := 0 to source.excludeList.count - 1 do
    processCodes(true, list, map, source.excludeList[i], filter, dependencies, params, profile, notClosed);
end;

procedure TFHIRValueSetExpander.handleDefine(cs : TFhirCodeSystem; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source : TFhirCodeSystem2; defines : TFhirCodeSystemConceptList; filter : TSearchFilterText; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile);
var
  i : integer;
  cm : TFhirCodeSystemConcept;
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
    cm.checkNoModifiers('ValueSetExpander.handleDefine', 'concept');
    if filter.passes(cm.display) or filter.passes(cm.code) then
      addDefinedCode(cs, list, map, source.system, cm, profile);
    handleDefine(cs, list, map, source, cm.conceptList, filter, nil, profile);
  end;
end;

function TFHIRValueSetExpander.chooseDisplay(c: TFhirCodeSystemConcept; profile : TFHIRExpansionProfile) : String;
var
  ccd : TFhirCodeSystemConceptDesignation;
begin
  result := c.display;
  for ccd in c.designationList do
    if (profile.displayLanguage = '') or languageMatches(profile.displayLanguage, ccd.language) then
      result := ccd.value;
end;

function TFHIRValueSetExpander.chooseDisplay(c: TFhirValueSetComposeIncludeConcept; profile : TFHIRExpansionProfile) : String;
{$IFDEF FHIR3}
var
  ccd : TFhirValueSetComposeIncludeConceptDesignation;
{$ENDIF}
begin
  result := c.display;
{$IFDEF FHIR3}
  for ccd in c.designationList do
    if (profile.displayLanguage = '') or languageMatches(profile.displayLanguage, ccd.language) then
      result := ccd.value;
{$ENDIF}
end;

procedure TFHIRValueSetExpander.addDefinedCode(cs : TFhirCodeSystem; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system: string; c: TFhirCodeSystemConcept; profile : TFhirExpansionProfile);
var
  i : integer;
begin
  {$IFDEF FHIR3}
  if not profile.excludeNotForUI or not (cs.isAbstract(c)) then
  {$ELSE}
  if not profile.excludeNotForUI or (c.abstractElement = nil) or not c.Abstract then
  {$ENDIF}
    processCode(false, list, map, system, '', c.Code, chooseDisplay(c, profile), c.definition, nil, profile);
  for i := 0 to c.conceptList.count - 1 do
    addDefinedCode(cs, list, map, system, c.conceptList[i], profile);
end;

procedure TFHIRValueSetExpander.processCode(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, version, code, display, definition: string; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile);
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


procedure TFHIRValueSetExpander.importValueSet(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; uri: String; filter : TSearchFilterText; dependencies : TStringList; params : TFhirValueSetExpansionParameterList; var notClosed : boolean);
var
  vs : TFHIRValueSet;
  i : integer;
  c : TFhirValueSetExpansionContains;
  s : String;
  dep : TStringList;
  param : TFhirValueSetExpansionParameter;
begin
  if (uri = '') then
    raise ETerminologyError.create('unable to find value set with no identity');

  dep := TStringList.Create;
  try
    vs := FStore.expandVS(uri, Fprofile, filter.filter, dep, FLimit, 0, 0);
    if (vs = nil) then
      raise Exception.create('unable to find value set '+uri);
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

procedure TFHIRValueSetExpander.processCodes(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cset: TFhirValueSetComposeInclude; filter : TSearchFilterText; dependencies : TStringList; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile; var notClosed : boolean);
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
  cset.checkNoModifiers('ValueSetExpander.processCodes', 'set');
  cs := FStore.getProvider(cset.system, cset.version);
  try
    if (cset.conceptList.count = 0) and (cset.filterList.count = 0) then
    begin
      if (cs.SpecialEnumeration <> '') and Fprofile.limitedExpansion then
      begin
        importValueSet(list, map, cs.SpecialEnumeration, filter, dependencies, params, notClosed);
        notClosed := true;
      end
      else if filter.Null then // special case - add all the code system
      begin
        if cs.isNotClosed(filter) then
          if cs.SpecialEnumeration <> '' then
            raise ETooCostly.create('The code System "'+cs.system(nil)+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
          else
            raise ETooCostly.create('The code System "'+cs.system(nil)+'" has a grammar, and cannot be enumerated directly');

        if (cs.TotalCount > FLimit) and not (Fprofile.limitedExpansion) then
          raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');
        for i := 0 to cs.ChildCount(nil) - 1 do
          processCodeAndDescendants(doDelete, list, map, cs, cs.getcontext(nil, i), params, profile)
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
              processCode(doDelete, list, map, cs.system(c), cs.version(c), cs.code(c), cs.display(c, profile.displayLanguage), cs.definition(c), params, profile);
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
      cset.conceptList[i].checkNoModifiers('ValueSetExpander.processCodes', 'set concept reference');
      display := chooseDisplay(cset.conceptList[i], profile);
      if (display = '') then
        display := cs.getDisplay(cset.conceptList[i].code, profile.displayLanguage);
      if filter.passes(display) or filter.passes(cset.conceptList[i].code) then
        processCode(doDelete, list, map, cs.system(nil), cs.version(nil), cset.conceptList[i].code, display, cs.getDefinition(cset.conceptList[i].code), params, profile);
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
          fc.checkNoModifiers('ValueSetExpander.processCodes', 'filter');
          filters[i+offset] := cs.filter(fc.property_, fc.Op, fc.value, prep);
          if filters[i+offset] = nil then
            raise Exception.create('The filter "'+fc.property_ +' '+ CODES_TFhirFilterOperatorEnum[fc.Op]+ ' '+fc.value+'" was not understood in the context of '+cs.system(nil));
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
            processCode(doDelete, list, map, cs.system(nil), cs.version(nil), cs.code(c), cs.display(c, profile.displayLanguage), cs.definition(c), params, profile);
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

procedure TFHIRValueSetExpander.processCodeAndDescendants(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cs: TCodeSystemProvider; context: TCodeSystemProviderContext; params : TFhirValueSetExpansionParameterList; profile : TFhirExpansionProfile);
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

  if not profile.excludeNotForUI or not cs.IsAbstract(context) then
    processCode(doDelete, list, map, cs.system(context), '', cs.Code(context), cs.Display(context, profile.displayLanguage), cs.definition(context), nil, profile);
  for i := 0 to cs.ChildCount(context) - 1 do
    processCodeAndDescendants(doDelete, list, map, cs, cs.getcontext(context, i), nil, profile);
end;


end.
