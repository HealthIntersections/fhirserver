unit TerminologyServer;

interface

uses
  SysUtils, Classes, IniFiles,
  AdvObjects, AdvStringObjectMatches, AdvStringLists,
  FHIRTypes, FHIRComponents, FHIRResources, FHIRUtilities,
  TerminologyServices, SnomedServices, LoincServices,
  FHIRValueSetChecker,
  TerminologyServerStore;

Type
  TTerminologyServer = class (TTerminologyServerStore)
  private
    FExpansions : TAdvStringObjectMatch;
    FDependencies : TAdvStringObjectMatch; // object is TAdvStringList of identity

    procedure AddDependency(name, value : String);
    function getCodeDefinition(c : TFHIRValueSetDefineConcept; code : string) : TFHIRValueSetDefineConcept; overload;
    function getCodeDefinition(vs : TFHIRValueSet; code : string) : TFHIRValueSetDefineConcept; overload;
  protected
    procedure invalidateVS(id : String); override;
  public
    constructor Create;
    Destructor Destroy;
    function Link: TTerminologyServer; overload;

    // load external terminology resources (snomed, Loinc)
    procedure load(ini : TIniFile);


    // functional services

    // if this id identifies a value set known to the external resources (if it does, construct it and return it)
    function isKnownValueSet(id : String; out vs : TFHIRValueSet): Boolean;

    // given a value set, expand it
    function expandVS(vs : TFHIRValueSet; cacheId : String; textFilter : String) : TFHIRValueSet; overload;
    function expandVS(uri : String; textFilter : String) : TFHIRValueSet; overload;

    // these are internal services - not for use outside the terminology server
    function expandVS(uri: String; textFilter : String; dependencies : TStringList) : TFHIRValueSet; overload;
    function expandVS(vs: TFHIRValueSet; cacheId : String; textFilter : String; dependencies : TStringList): TFHIRValueSet; overload;

    function validate(vs : TFHIRValueSet; coding : TFhirCoding) : TFhirOperationOutcome; overload;
    function validate(vs : TFHIRValueSet; coded : TFhirCodeableConcept) : TFhirOperationOutcome; overload;
    function translate(vs : TFHIRValueSet; coding : TFhirCoding; dest : String) : TFhirOperationOutcome; overload;
    Function MakeChecker(uri : string) : TValueSetChecker;
    function getDisplayForCode(system, code : String): String;
    function checkCode(op : TFhirOperationOutcome; path : string; code : string; system : string; display : string; context : TFHIRProfileStructureElement) : boolean;
  end;

implementation

uses
  FHIRValueSetExpander;


{ TTerminologyServer }

constructor TTerminologyServer.Create;
begin
  inherited;
  FExpansions := TAdvStringObjectMatch.create;
  FExpansions.PreventDuplicates;
  FDependencies := TAdvStringObjectMatch.create;
  FDependencies.PreventDuplicates;
end;

destructor TTerminologyServer.Destroy;
begin
  FDependencies.Free;
  FExpansions.free;
  inherited;
end;

procedure TTerminologyServer.load(ini : TIniFile);
var
  fn : string;
begin
  fn := ini.ReadString('snomed', 'cache', '');
  if fn <> '' then
  begin
    write('Load Snomed');
    Snomed := TSnomedServices.Create;
    Snomed.Load(fn);
    writeln(' - done');
  end;
  fn := ini.ReadString('loinc', 'cache', '');
  if fn <> '' then
  begin
    write('Load Loinc');
    Loinc := TLoincServices.Create;
    Loinc.Load(fn);
    writeln(' - done');
  end;
end;

function TTerminologyServer.Link: TTerminologyServer;
begin
  result := TTerminologyServer(inherited Link);
end;

procedure TTerminologyServer.AddDependency(name, value : String);
var
  ts : TAdvStringList;
begin
  // must be in lock
  if FDependencies.ExistsByKey(name) then
    ts := FDependencies.GetValueByKey(name) as TAdvStringList
  else
  begin
    ts := TAdvStringList.Create;
    FDependencies.Add(name, ts);
  end;
  if not ts.ExistsByValue(value) then
    ts.Add(value);
end;

procedure TTerminologyServer.invalidateVS(id: String);
var
  ts : TAdvStringList;
  i : integer;
begin
  // must be in lock
  if FDependencies.ExistsByKey(id) then
  begin
    ts := FDependencies.GetValueByKey(id) as TAdvStringList;
    for i := 0 to ts.Count - 1 do
      if FExpansions.ExistsByKey(ts[i]) then
        FExpansions.DeleteByKey(ts[i]);
    FDependencies.DeleteByKey(id);
  end;
end;

function TTerminologyServer.expandVS(vs: TFHIRValueSet; cacheId : String; textFilter : String): TFHIRValueSet;
var
  ts : TStringList;
begin
  ts := TStringList.create;
  try
    result := expandVS(vs, cacheId, textFilter, ts);
  finally
    ts.free;
  end;
end;

function TTerminologyServer.expandVS(vs: TFHIRValueSet; cacheId : String; textFilter : String; dependencies : TStringList): TFHIRValueSet;
var
  s : String;
  exp : TFHIRValueSetExpander;
begin
  result := nil;
  if cacheId <> '' then
  begin
    FLock.Lock('expandVS.1');
    try
      if FExpansions.ExistsByKey(cacheId+#1+textFilter) then
        result := (FExpansions.matches[cacheId+#1+textFilter] as TFhirValueSet).link;
    finally
      FLock.Unlock;
    end;
  end;
  if result = nil then
  begin
    exp := TFHIRValueSetExpander.create(self.Link);
    try
      result := exp.expand(vs, textFilter, dependencies);
      if (dependencies.Count > 0) then
      begin
        FLock.Lock('expandVS.2');
        try
          FExpansions.Add(cacheId+#1+textFilter, result.Link);
          // in addition, we trace the dependencies so we can expire the cache
          for s in dependencies do
            AddDependency(s, cacheId);
        finally
          FLock.Unlock;
        end;
      end;
    finally
      exp.Free;
    end;
  end;
end;



function TTerminologyServer.expandVS(uri: String; textFilter : String): TFHIRValueSet;
var
  vs : TFHIRValueSet;
  ts : TStringList;
begin
  ts := TStringList.create;
  try
    vs := getValueSetByUrl(uri);
    try
      result := expandVS(vs, uri, textFilter, ts);
    finally
      vs.Free;
    end;
  finally
    ts.Free;
  end;
end;

function TTerminologyServer.expandVS(uri: String; textFilter : String; dependencies: TStringList): TFHIRValueSet;
var
  vs : TFHIRValueSet;
begin
  vs := getValueSetByUrl(uri);
  try
    if vs = nil then
      raise Exception.Create('Unable to find value set "'+uri+'"');
    result := expandVS(vs, uri, textFilter);
  finally
    vs.Free;
  end;
end;

function TTerminologyServer.isKnownValueSet(id: String; out vs: TFHIRValueSet): Boolean;
begin
  vs := nil;
  if id.StartsWith('http://snomed.info/') then
    vs := Snomed.buildValueSet(id);
  result := vs <> nil;
end;

function TTerminologyServer.MakeChecker(uri: string): TValueSetChecker;
var
  vs : TFhirValueSet;
begin
  result := TValueSetChecker.create(self.Link, uri);
  try
    vs := getValueSetByUrl(uri);
    try
      result.prepare(vs);
    finally
      vs.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TTerminologyServer.validate(vs : TFHIRValueSet; coding : TFhirCoding) : TFhirOperationOutcome;
var
  check : TValueSetChecker;
begin
  check := TValueSetChecker.create(self.Link, vs.identifierST);
  try
    check.prepare(vs);
    result := check.check(coding);
  finally
    check.Free;
  end;
end;


function TTerminologyServer.validate(vs : TFHIRValueSet; coded : TFhirCodeableConcept) : TFhirOperationOutcome;
var
  check : TValueSetChecker;
begin
  check := TValueSetChecker.create(self.Link, vs.identifierST);
  try
    check.prepare(vs);
    result := check.check(coded);
  finally
    check.Free;
 end;
end;

function TTerminologyServer.checkCode(op : TFhirOperationOutcome; path : string; code : string; system : string; display : string; context : TFHIRProfileStructureElement) : boolean;
var
  vs : TFhirValueSet;
  def : TFhirValueSetDefineConcept;
  d : String;
begin
  result := false;
  if system.StartsWith('http://hl7.org/fhir') then
  begin
    if (system = 'http://hl7.org/fhir/sid/icd-10') then
      result := true// nothing for now....
    else
    begin
      vs := getCodeSystem(system);
      if op.warning('InstanceValidator', 'code-unknown', path, vs <> nil, 'Unknown Code System '+system) then
      begin
        def := getCodeDefinition(vs, code);
        if (op.warning('InstanceValidator', 'code-unknown', path, def <> nil, 'Unknown Code ('+system+'#'+code+')')) then
            result := op.warning('InstanceValidator', 'code-unknown', path, (display = '') or (display = def.DisplayST), 'Display for '+system+' code "'+code+'" should be "'+def.DisplayST+'"');
      end;
    end;
  end
  else if system.StartsWith('http://snomed.info/sct') and (Snomed <> nil) then
  begin
    if op.warning('InstanceValidator', 'code-unknown', path, Snomed.IsValidConcept(code), 'The SNOMED-CT term "'+code+'" is unknown') then
    begin
      d := Snomed.GetDisplayName(code, '');
      result := op.warning('InstanceValidator', 'code-unknown', path, (display = '') or (display = d), 'Display for SNOMED-CT term "'+code+'" should be "'+d+'"');
    end;
  end
  else if system.StartsWith('http://loinc.org') and (Loinc <> nil) then
  begin
    d := Loinc.GetDisplayByName(code);
    if op.warning('InstanceValidator', 'code-unknown', path, d <> '', 'The LOINC code "'+code+'" is unknown') then
      result := op.warning('InstanceValidator', 'code-unknown', path, (display = '') or (display = d), 'Display for Loinc Code "'+code+'" should be "'+d+'"');
  end
  else if system.StartsWith('http://unitsofmeasure.org') and (Ucum <> nil) then
  begin
    d := Ucum.validate(code);
    result := op.warning('InstanceValidator', 'code-unknown', path, d = '', 'The UCUM code "'+code+'" is not valid: '+d);
    // we don't make rules about display for UCUM.
  end
  else
    result := true;
end;



function TTerminologyServer.getCodeDefinition(c : TFHIRValueSetDefineConcept; code : string) : TFHIRValueSetDefineConcept;
var
  i : integer;
  g : TFHIRValueSetDefineConcept;
  r : TFHIRValueSetDefineConcept;
begin
  result := nil;
  if (code = c.CodeST) then
    result := c;
  for i := 0 to c.conceptList.Count - 1 do
  begin
    g := c.conceptList[i];
    r := getCodeDefinition(g, code);
    if (r <> nil) then
    begin
      result := r;
      exit;
    end;
  end;
end;

function TTerminologyServer.getCodeDefinition(vs : TFHIRValueSet; code : string) : TFHIRValueSetDefineConcept;
var
  i : integer;
  c : TFHIRValueSetDefineConcept;
  r : TFHIRValueSetDefineConcept;
begin
  result := nil;
  for i := 0 to vs.define.conceptList.Count - 1 do
  begin
    c := vs.define.conceptList[i];
    r := getCodeDefinition(c, code);
    if (r <> nil) then
    begin
      result := r;
      exit;
    end;
  end;
end;


function TTerminologyServer.getDisplayForCode(system, code: String): String;
var
  provider : TCodeSystemProvider;
begin
  provider := getProvider(system, true);
  if provider <> nil then
  try
    result := provider.getDisplay(code);
  finally
    provider.Free;
  end;
end;

function TTerminologyServer.translate(vs: TFHIRValueSet; coding: TFhirCoding; dest : String): TFhirOperationOutcome;
var
  isCs, ok: boolean;
  i, j : integer;
  cm : TLoadedConceptMap;
  cc : TFhirCodeableConcept;
  c : TFHIRCoding;
  maps : TFhirConceptMapConceptMapList;
  map : TFhirConceptMapConceptMap;
begin
  result := TFhirOperationOutcome.Create;
  try
    if checkCode(result, '', coding.codeST, coding.systemST, coding.displayST, nil) then
    begin
      cc := TFhirCodeableConcept.Create;
      try
        cc.codingList.Add(coding.Link);

        isCs := HasCodeSystem(dest);
        ok := false;

        // iterate the concept maps
        // a concept map is a match if there is a translation from source to dest
        FLock.Lock('translate');
        try
          for i := 0 to FConceptMaps.count - 1 do
          begin
            cm := FConceptMaps.values[i] as TLoadedConceptMap;
            if (cm.source <> nil) and (cm.source.identifierST = vs.identifierST) and
              cm.hasTranslation(coding.systemST, coding.codeST, maps) then
            try
              for j := 0 to maps.Count - 1 do
              begin
                map := maps[j];
                if (map.equivalenceST in [ConceptEquivalenceEqual, ConceptEquivalenceEquivalent, ConceptEquivalenceWider, ConceptEquivalenceInexact]) and
                  (not isCs {if we'rea value set mapping, we'll just run with all the maps) } or (map.systemST = dest)) then
                begin
                  ok := true;
                  c := cc.codingList.Append;
                  c.system := map.system.Clone;
                  c.code := map.code.Clone;
                  c.displayST := getDisplayForCode(map.systemST, map.codeST);
                  if map.commentsST <> '' then
                    result.hint('terminology-server', 'mapping', '', false, 'Mapping from "'+coding.systemST+'"/"'+coding.codeST+'" to "'+c.systemST+'"/"'+c.codeST+'": '+map.commentsST)
                end;
              end;
            finally
              maps.free;
            end;
          end;
          result.error('terminology-server', 'mapping', '', ok, 'No Mapping found');
        finally
          FLock.Unlock;
        end;
        if ok then
          result.AddExtension('http://hl7.org/fhir/Profile/general-extensions#translation', cc.Link);
      finally
        cc.free;
      end;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

(*
function TTerminologyServer.checkCode(op: TFhirOperationOutcome; path, code, system, display: string; context: TFHIRProfileStructureElement): boolean;
begin
{
    function getValueSet(system : string) : TFHIRValueSet;
}

end;

procedure TTerminologyServer.DropTerminologyResource(key: Integer; aType : TFhirResourceType);
begin

end;

function TTerminologyServer.expandVS(vs: TFHIRValueSet): TFHIRValueSet;
begin

end;

function TTerminologyServer.isKnownValueSet(id: String;
  out vs: TFHIRValueSet): Boolean;
begin

end;

procedure TTerminologyServer.load(ini: TIniFile);
begin

end;

procedure TTerminologyServer.SeeSpecificationResource(resource: TFHIRResource);
begin

end;

procedure TTerminologyServer.SeeTerminologyResource(key: Integer;
  resource: TFHIRResource);
begin

end;

function TTerminologyServer.translate(vs: TFHIRValueSet;
  coding: TFhirCoding): TFhirOperationOutcome;
begin

end;

function TTerminologyServer.validate(vs: TFHIRValueSet;
  coding: TFhirCoding): TFhirOperationOutcome;
begin

end;

function TTerminologyServer.validate(vs: TFHIRValueSet;
  coded: TFhirCodeableConcept): TFhirOperationOutcome;
begin

end;

function TTerminologyServer.MakeChecker(uri: string): TValueSetChecker;
begin
{

function TTerminologyServer.findValueSet(uri: string): TFhirValueSet;
begin
//    result := FValuesets.matches[TFHIRUri(ref).value] as TFHIRValueSet

end;


  if Fchecks.ExistsByKey(vs.identifierST) then
    result := FChecks.matches[vs.identifierST] as TValueSetChecker
  else
  begin
  end;}
end;


end.


function TFHIRDataStore.translate(vs: TFHIRValueSet; coding: TFhirCoding): TFhirOperationOutcome;
var
  cm : TFhirConceptMap;
  s : String;
begin
  s.Substring()
  result := TFhirOperationOutcome.Create;
  try
    cm := FindConceptMap(vs, coding.systemST);
    try
      if cm = nil then
        rule(


    finally
      cm.Free;
    end;
    result.link;
  finally
    result.Free;
  end;
  cm := T
  // is there a valueset?
    // is there a concept map for the value set
  // else
    // is there a concept map for the code system

end;

function TFHIRDataStore.validate(vs: TFHIRValueSet; coding: TFhirCoding): TFhirOperationOutcome;
var
  check : TValueSetChecker;
begin
  check := TValueSetChecker.create;
  try
    FLock.Lock('validate');
    try
      check.ValueSets := FValuesetExpander.ValueSets.Link;
      check.CodeSystems := FValuesetExpander.CodeSystems.Link;
      check.prepare(vs);
      result := check.check(coding);
    finally
      FLock.Unlock;
    end;
  finally
    check.Free;
  end;
end;

function TFHIRDataStore.validate(vs: TFHIRValueSet; coded: TFhirCodeableConcept): TFhirOperationOutcome;
var
  check : TValueSetChecker;
begin
  check := TValueSetChecker.create;
  try
    FLock.Lock('validate');
    try
      check.ValueSets := FValuesetExpander.ValueSets.Link;
      check.CodeSystems := FValuesetExpander.CodeSystems.Link;
      check.prepare(vs);
      result := check.check(coded);
    finally
      FLock.Unlock;
    end;
  finally
    result.Free;
  end;
end;


procedure TFHIRDataStore.DelistValueSet(key: Integer);
var
  vs : TFHIRValueSet;
begin
  FLock.Lock('DelistValueSet');
  try
    if FValueSetTracker.ExistsByKey(inttostr(key)) then
    begin
      vs := FValueSetTracker.Matches[inttostr(key)] as TFhirValueSet;
      if (vs.define <> nil) then
        FValuesetExpander.CodeSystems.DeleteByKey(vs.define.systemST);
      if (vs.identifierST <> '') then
      FValuesetExpander.ValueSets.DeleteByKey(vs.identifierST);
      FValueSetTracker.DeleteByKey(inttostr(key));
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRDataStore.DelistConceptMap(key: Integer);
var
  vs : TFHIRConceptMap;
begin
  FLock.Lock('DelistConceptMap');
  try
    if FConceptMapTracker.ExistsByKey(inttostr(key)) then
    begin
      vs := FConceptMapTracker.Matches[inttostr(key)] as TFhirConceptMap;
      FConceptMapTracker.DeleteByKey(inttostr(key));
    end;
  finally
    FLock.Unlock;
  end;
end;

    begin
      vs := resource as TFHIRValueSet;
      FValueSetTracker.add(inttostr(key), vs.link);
      if (vs.define <> nil) then
        FValuesetExpander.CodeSystems.add(vs.define.systemST, vs.link);
      if (vs.identifierST <> '') then
        FValuesetExpander.ValueSets.add(vs.identifierST, vs.link);
    end
    else if resource.ResourceType = frtConceptMap then
    begin
      cm := resource as TFhirConceptMap;
      FConceptMapTracker.add(inttostr(key), cm.link);
    end

        if (aType = frtValueSet) and (FValueSetTracker.ExistsByKey(inttostr(key)))  then
    begin
      vs := FValueSetTracker.Matches[inttostr(key)] as TFHIRValueSet;
      if (vs.define <> nil) then
        FValuesetExpander.CodeSystems.DeleteByKey(vs.define.systemST);
      if (vs.identifierST <> '') then
        FValuesetExpander.ValueSets.DeleteByKey(vs.identifierST);
      FValueSetTracker.DeleteByKey(inttostr(key));
    end
    else if (aType = frtConceptMap) and (FValueSetTracker.ExistsByKey(inttostr(key)))  then
    begin
      cm := FConceptMapTracker.Matches[inttostr(key)] as TFHIRConceptMap;
      FValueSetTracker.DeleteByKey(inttostr(key));
    end



    procedure DelistValueSet(key : Integer);
    procedure DelistConceptMap(key : Integer);
        if request.ResourceType = frtValueSet then
          FRepository.DelistValueSet(ResourceKey);
        if request.ResourceType = frtConceptMap then
          FRepository.DelistConceptMap(ResourceKey);

function TFHIRValidator.getValueSet(system : string) : TFHIRValueSet;
begin
  if FCodeSystems.ExistsByKey(system) then
    result := FCodeSystems.matches[system] as TFHIRValueSet
  else
    result := nil;
end;






    begin
      vs := r as TFhirValueSet;
      FValuesets.add(vs.IdentifierST, vs.link);
      if (vs.Define <> nil) then
        FCodesystems.add(vs.Define.SystemST, vs.link);
    end;




*)

end.
