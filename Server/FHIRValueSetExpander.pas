unit FHIRValueSetExpander;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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
  AdvStringObjectMatches, AdvObjects, AdvObjectLists, AdvExceptions, AdvGenerics,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, DateSupport,
  TerminologyServices, LoincServices, SnomedServices, UcumServices,
  TerminologyServer, TerminologyServerStore;

const
  UPPER_LIMIT_NO_TEXT = 10000;
  UPPER_LIMIT_TEXT = 1000;// won't expand a value set bigger than this - just takes too long, and no one's going to do anything with it anyway

  FHIR_VERSION_CANONICAL_SPLIT = {$IFDEF FHIR2} '?version=' {$ELSE} '|' {$ENDIF};


Type
  TFHIRValueSetExpander = class (TAdvObject)
  private
    FLimit : integer;
    FCount : integer;
    FOffset : integer;
    FProfile : TFhirExpansionProfile;

    FStore : TTerminologyServer;
    procedure processCodeAndDescendants(doDelete : boolean; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cs : TCodeSystemProvider; context : TCodeSystemProviderContext; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; importHash : TStringList);

    procedure handleDefine(cs : TFhirCodeSystem; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirCodeSystem2; defines : TFhirCodeSystemConceptList; filter : TSearchFilterText; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; importHash : TStringList);
    procedure importValueSet(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; vs : TFHIRValueSet; expansion : TFhirValueSetExpansion; importHash : TStringList);
    procedure processCodes(doDelete : boolean; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; cset : TFhirValueSetComposeInclude; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; var notClosed : boolean);
    procedure handleCompose(list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; source : TFhirValueSetCompose; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; var notClosed : boolean);

    procedure hashImport(hash : TStringList; cc : TFhirValueSetExpansionContains);
    function makeImportHash(imports : TAdvList<TFHIRValueSet>; start : integer) : TStringList;
    function passesImportFilter(importHash : TStringList; system, code : string) : boolean;

    procedure processCode(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, version, code, display, definition: string; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; importHash : TStringList);
    procedure addDefinedCode(cs : TFhirCodeSystem; list : TFhirValueSetExpansionContainsList; map : TAdvStringObjectMatch; system : string; c : TFhirCodeSystemConcept; profile : TFhirExpansionProfile; importHash : TStringList);
    function key(system, code, display : String): string; overload;
    function key(c : TFhirValueSetExpansionContains) : string;  overload;
    function chooseDisplay(c: TFhirCodeSystemConcept;  profile: TFHIRExpansionProfile): String; overload;
    function chooseDisplay(c: TFhirValueSetComposeIncludeConcept;  profile: TFHIRExpansionProfile): String; overload;
    function expandValueSet(uri, filter: String; dependencies: TStringList; var notClosed: boolean): TFHIRValueSet;
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
begin
  source.checkNoImplicitRules('ValueSetExpander.Expand', 'ValueSet');
  source.checkNoModifiers('ValueSetExpander.Expand', 'ValueSet');
  {$IFNDEF FHIR2}
  profile.checkNoImplicitRules('ValueSetExpander.Expand', 'ExpansionProfile');
  profile.checkNoModifiers('ValueSetExpander.Expand', 'ExpansionProfile');
  {$ENDIF}

  FProfile := profile.Link;

  result := source.Clone;
  if not profile.includeDefinition then
  begin
    {$IFDEF FHIR2}
    result.codeSystem := nil;
    result.requirements := '';
    {$ELSE}
    result.purpose := '';
    {$ENDIF}
    result.compose := nil;
    result.description := '';
    result.contactList.Clear;
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
  if result.status = PublicationStatusNull then
    result.status := PublicationStatusDraft;

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
    result.expansion.timestamp := TDateTimeEx.makeUTC;
    result.expansion.identifier := NewGuidURN;

    if source.id <> '' then
      result.expansion.addParam('expansion-source', 'ValueSet/'+source.id)
    else if source.url <> '' then
      result.expansion.addParam('expansion-source', source.url);
    {$IFNDEF FHIR2}
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
    if profile.activeOnly then
      result.expansion.addParam('activeOnly', profile.activeOnly);
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
        handleDefine(source, list, map, source.codeSystem, source.codeSystem.conceptList, filter, result.expansion, profile, nil);
      end;
      {$ENDIF}
      notClosed := false;
      if (source.compose <> nil) then
      begin
        source.compose.checkNoModifiers('ValueSetExpander.Expand', 'compose');
        handleCompose(list, map, source.compose, filter, dependencies, result.expansion, profile, notClosed);
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

function TFHIRValueSetExpander.key(system, code, display : String): string;
begin
  result:= '{'+system+'}'+code+'#1'+display;
end;

function TFHIRValueSetExpander.key(c: TFhirValueSetExpansionContains): string;
begin
  result := key(c.System, c.Code, c.display);
end;

procedure TFHIRValueSetExpander.hashImport(hash: TStringList; cc: TFhirValueSetExpansionContains);
var
  ccc : TFhirValueSetExpansionContains;
begin
  if cc.code <> '' then
    hash.Add(cc.system+#1+cc.code);
  for ccc in cc.containsList do
    hashImport(hash, ccc);
end;

function TFHIRValueSetExpander.makeImportHash(imports: TAdvList<TFHIRValueSet>; start: integer): TStringList;
var
  i : integer;
  vs : TFHIRValueSet;
  cc : TFhirValueSetExpansionContains;
begin
  if imports.Count <= start then
    exit(nil);

  result := TStringList.Create;
  try
    for i := start to imports.Count - 1 do
    begin
      vs := imports[i];
      for cc in vs.expansion.containsList do
        hashImport(result, cc);
    end;
    result.Sorted := true;
  except
    result.Free;
    raise;
  end;
end;

procedure TFHIRValueSetExpander.handleCompose(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source: TFhirValueSetCompose; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; var notClosed : boolean);
var
  i : integer;
  {$IFDEF FHIR2}
  vs : TFHIRValueSet;
  {$ENDIF}
begin
  {$IFDEF FHIR2}
  for i := 0 to source.importList.count - 1 do
  begin
    vs := expandValueSet(source.importList[i].value, filter.filter, dependencies, notClosed);
    try
      importValueSet(list, map, vs, expansion, nil);
    finally
      vs.free;
    end;
  end;
  {$ENDIF}
  for i := 0 to source.includeList.count - 1 do
    processCodes(false, list, map, source.includeList[i], filter, dependencies, expansion, profile, notClosed);
  for i := 0 to source.excludeList.count - 1 do
    processCodes(true, list, map, source.excludeList[i], filter, dependencies, expansion, profile, notClosed);
end;

procedure TFHIRValueSetExpander.handleDefine(cs : TFhirCodeSystem; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; source : TFhirCodeSystem2; defines : TFhirCodeSystemConceptList; filter : TSearchFilterText; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; importHash : TStringList);
var
  i : integer;
  cm : TFhirCodeSystemConcept;
  param : TFhirValueSetExpansionParameter;
begin
  if (defines.Count > 0) and (expansion <> nil) and (source.version <> '') then
  begin
    param := expansion.parameterList.Append;
    param.name := 'version';
    param.value := TFHIRUri.Create(source.system+FHIR_VERSION_CANONICAL_SPLIT+source.version);
  end;
  for i := 0 to defines.count - 1 do
  begin
    cm := defines[i];
    cm.checkNoModifiers('ValueSetExpander.handleDefine', 'concept');
    if filter.passes(cm.display) or filter.passes(cm.code) then
      addDefinedCode(cs, list, map, source.system, cm, profile, importHash);
    handleDefine(cs, list, map, source, cm.conceptList, filter, nil, profile, importHash);
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
{$IFNDEF FHIR2}
var
  ccd : TFhirValueSetComposeIncludeConceptDesignation;
{$ENDIF}
begin
  result := c.display;
{$IFNDEF FHIR2}
  for ccd in c.designationList do
    if (profile.displayLanguage = '') or languageMatches(profile.displayLanguage, ccd.language) then
      result := ccd.value;
{$ENDIF}
end;

procedure TFHIRValueSetExpander.addDefinedCode(cs : TFhirCodeSystem; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system: string; c: TFhirCodeSystemConcept; profile : TFhirExpansionProfile; importHash : TStringList);
var
  i : integer;
begin
  {$IFNDEF FHIR2}
  if not profile.excludeNotForUI or not (cs.isAbstract(c)) then
  {$ELSE}
  if not profile.excludeNotForUI or (c.abstractElement = nil) or not c.Abstract then
  {$ENDIF}
    processCode(false, list, map, system, '', c.Code, chooseDisplay(c, profile), c.definition, nil, profile, importHash);
  for i := 0 to c.conceptList.count - 1 do
    addDefinedCode(cs, list, map, system, c.conceptList[i], profile, importHash);
end;

function TFHIRValueSetExpander.passesImportFilter(importHash: TStringList; system, code: string): boolean;
var
  i : integer;
begin
  if importHash = nil then
    result := true
  else
    result := importHash.find(system+#1+code, i);
end;

procedure TFHIRValueSetExpander.processCode(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; system, version, code, display, definition: string; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; importHash : TStringList);
var
  n : TFHIRValueSetExpansionContains;
  s : String;
  f : boolean;
  param : TFhirValueSetExpansionParameter;
begin
  if not passesImportFilter(importHash, system, code) then
    exit;

  if (map.Count >= FLimit) and not doDelete then
    raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');

  if (expansion <> nil) and (version <> '') then
  begin
    f := false;
    s := system+'?version='+version;
    for param in expansion.parameterList do
      if (param.name = 'version') and (param.value.primitiveValue = s) then
        f := true;
    if not f then
    begin
      param := expansion.parameterList.Append;
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

function TFHIRValueSetExpander.expandValueSet(uri: String; filter : String; dependencies : TStringList; var notClosed : boolean) : TFHIRValueSet;
var
  dep : TStringList;
begin
  dep := TStringList.Create;
  try
    result := FStore.expandVS(uri, Fprofile, filter, dep, FLimit, 0, 0);
    try
      dependencies.AddStrings(dep);
      if (result = nil) then
        raise Exception.create('unable to find value set '+uri);
      if result.expansion.hasextension('http://hl7.org/fhir/Profile/questionnaire-extensions#closed') then
        notClosed := true;
      result.Link;
    finally
      result.free;
    end;
  finally
    dep.Free;
  end;
end;

procedure TFHIRValueSetExpander.importValueSet(list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; vs : TFHIRValueSet; expansion : TFhirValueSetExpansion; importHash : TStringList);
var
  i : integer;
  c : TFhirValueSetExpansionContains;
  s : String;
  param : TFhirValueSetExpansionParameter;
begin
  for param in vs.expansion.parameterList do
    expansion.parameterList.Add(param.Link);

  for i := 0 to vs.expansion.containsList.Count - 1 do
  begin
    c := vs.expansion.containsList[i];
    s := key(c);
    if passesImportFilter(importHash, c.system, c.code) and not map.ExistsByKey(s) then
    begin
      list.add(c.link);
      map.add(s, c.link);
    end;
  end;
end;

procedure TFHIRValueSetExpander.processCodes(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cset: TFhirValueSetComposeInclude; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; var notClosed : boolean);
var
  cs : TCodeSystemProvider;
  i, offset, count : integer;
  fc : TFhirValueSetComposeIncludeFilter;
  c : TCodeSystemProviderContext;
  filters : Array of TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderFilterContext;
  ok : boolean;
  prep : TCodeSystemProviderFilterPreparationContext;
  inner : boolean;
  display : String;
  imports : TAdvList<TFHIRValueSet>;
  hash : TStringList;
  uri : TFhirUri;
  base : TFHIRValueSet;
  ex : TFHIRExtension;
begin
  imports := TAdvList<TFHIRValueSet>.create;
  try
    cset.checkNoModifiers('ValueSetExpander.processCodes', 'set');
    {$IFNDEF FHIR2}
    for uri in cset.valueSetList do
      imports.add(expandValueset(uri.value, filter.filter, dependencies, notClosed));
    {$ENDIF}

    if cset.system = '' then
    begin
      base := imports[0];
      hash := makeImportHash(imports, 1);
      try
        importValueSet(list, map, base, expansion, hash);
      finally
        hash.Free;
      end;
    end
    else
    begin
      hash := makeImportHash(imports, 0);
      try
        cs := FStore.getProvider(cset.system, cset.version, FProfile);
        try
          if (cset.conceptList.count = 0) and (cset.filterList.count = 0) then
          begin
            if (cs.SpecialEnumeration <> '') and Fprofile.limitedExpansion then
            begin
              base := expandValueSet(cs.SpecialEnumeration, filter.filter, dependencies, notClosed);
              try
                ex := expansion.extensionList.Append;
                ex.url := 'http://hl7.org/fhir/StructureDefinition/valueset-toocostly';
                ex.value := TFHIRBoolean.Create(true);
                importValueSet(list, map, base, expansion, hash);
              finally
                base.Free;
              end;
              notClosed := true;
            end
            else if filter.Null then // special case - add all the code system
            begin
              if cs.isNotClosed(filter) then
                if cs.SpecialEnumeration <> '' then
                  raise ETooCostly.create('The code System "'+cs.system(nil)+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
                else
                  raise ETooCostly.create('The code System "'+cs.system(nil)+'" has a grammar, and cannot be enumerated directly');

              if imports.Empty and (cs.TotalCount > FLimit) and not (Fprofile.limitedExpansion) then
                raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');
              for i := 0 to cs.ChildCount(nil) - 1 do
                processCodeAndDescendants(doDelete, list, map, cs, cs.getcontext(nil, i), expansion, profile, hash)
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
                  while cs.FilterMore(ctxt) do
                  begin
                    c := cs.FilterConcept(ctxt);
                    processCode(doDelete, list, map, cs.system(c), cs.version(c), cs.code(c), cs.display(c, profile.displayLanguage), cs.definition(c), expansion, profile, hash);
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
              processCode(doDelete, list, map, cs.system(nil), cs.version(nil), cset.conceptList[i].code, display, cs.getDefinition(cset.conceptList[i].code), expansion, profile, hash);
          end;

          if cset.filterList.Count > 0 then
          begin
            prep := cs.getPrepContext;
            try
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

                if cs.specialEnumeration <> '' then
                begin
                  SetLength(filters, length(filters)+1);
                  filters[offset] := cs.specialFilter(prep, true);
                  offset := offset + 1;
                  ex := expansion.extensionList.Append;
                  ex.url := 'http://hl7.org/fhir/StructureDefinition/valueset-toocostly';
                  ex.value := TFHIRBoolean.Create(true);
                  notClosed := true;
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
                count := 0;
                While cs.FilterMore(filters[0]) and ((FOffset + FCount = 0) or (count < FOffset + FCount)) do
                begin
                  c := cs.FilterConcept(filters[0]);
                  ok := true;
                  if inner then
                    for i := 1 to length(filters) - 1 do
                      ok := ok and cs.InFilter(filters[i], c);
                  if ok then
                  begin
                    inc(count);
                    if count > FOffset then
                      processCode(doDelete, list, map, cs.system(nil), cs.version(nil), cs.code(c), cs.display(c, profile.displayLanguage), cs.definition(c), expansion, profile, hash);
                  end;
                end;
              finally
                for i := 0 to length(filters) - 1 do
                  if filters[i] <> nil then
                    cs.Close(filters[i]);
              end;
            finally
              prep.free;
            end;
          end;
        finally
          cs.free;
        end;
      finally
        hash.Free;
      end;
    end;
  finally
    imports.Free;
  end;
end;

procedure TFHIRValueSetExpander.processCodeAndDescendants(doDelete : boolean; list: TFhirValueSetExpansionContainsList; map: TAdvStringObjectMatch; cs: TCodeSystemProvider; context: TCodeSystemProviderContext; expansion : TFhirValueSetExpansion; profile : TFhirExpansionProfile; importHash : TStringList);
var
  i : integer;
  param : TFhirValueSetExpansionParameter;
begin
  if (cs.version(nil) <> '') and (expansion <> nil) then
  begin
    param := expansion.parameterList.Append;
    param.name := 'version';
    param.value := TFHIRUri.Create(cs.system(nil)+'?version='+cs.version(nil));
  end;

  if not profile.excludeNotForUI or not cs.IsAbstract(context) then
    processCode(doDelete, list, map, cs.system(context), '', cs.Code(context), cs.Display(context, profile.displayLanguage), cs.definition(context), nil, profile, importHash);
  for i := 0 to cs.ChildCount(context) - 1 do
    processCodeAndDescendants(doDelete, list, map, cs, cs.getcontext(context, i), nil, profile, importHash);
end;


end.
