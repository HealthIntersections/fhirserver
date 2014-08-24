unit FHIRValueSetChecker;

interface

uses
  SysUtils, Classes,
  AdvObjects, AdvStringObjectMatches,
  FHIRTypes, FHIRComponents, FHIRResources, FHIRUtilities,
  TerminologyServices, TerminologyServerStore;

Type
  TValueSetChecker = class (TAdvObject)
  private
    FStore : TTerminologyServerStore;
    FOthers : TAdvStringObjectMatch; // checkers or code system providers
    fvs : TFHIRValueSet;
    FId: String;
    function check(system, code : String; displays : TStringList) : boolean; overload;
    function findCode(code: String; list : TFhirValueSetDefineConceptList; displays : TStringList): boolean;
    function checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeInclude; code : String; displays : TStringList) : boolean;
    function rule(op : TFhirOperationOutcome; severity : TFhirIssueSeverity; test : boolean; code, msg : string):boolean;
    procedure check(coding: TFhirCoding; op : TFhirOperationOutcome); overload;
    procedure check(code: TFhirCodeableConcept; op : TFhirOperationOutcome); overload;
  public
    constructor create(store : TTerminologyServerStore; id : String); overload;
    destructor destroy; override;

    property id : String read FId;

    procedure prepare(vs : TFHIRValueSet);

    function check(system, code : String) : boolean; overload;
    function check(coding : TFhirCoding) : TFhirOperationOutcome; overload;
    function check(coded : TFhirCodeableConcept) : TFhirOperationOutcome; overload;
  end;

implementation

{ TValueSetChecker }

constructor TValueSetChecker.create(store : TTerminologyServerStore; id : string);
begin
  Create;
  FStore := store;
  FId := id;
  FOthers := TAdvStringObjectMatch.create;
  FOthers.PreventDuplicates;
end;

destructor TValueSetChecker.destroy;
begin
  FVs.Free;
  FOthers.Free;
  FStore.Free;
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
      other := FStore.getValueSetByIdentifier(fvs.compose.importList[i].value);
      try
        if other = nil then
          raise exception.create('Unable to find value set '+fvs.compose.importList[i].value);
        checker := TValueSetChecker.create;
        try
          checker.prepare(other);
          FOthers.Add(fvs.compose.importList[i].value, checker.Link);
        finally
          checker.free;
        end;
      finally
        other.free;
      end;
    end;
    for i := 0 to fvs.compose.includeList.Count - 1 do
    begin
      if not FOthers.ExistsByKey(fvs.compose.includeList[i].systemST) then
        FOthers.Add(fvs.compose.includeList[i].systemST, FStore.getProvider(fvs.compose.includeList[i].systemST));
      cs := TCodeSystemProvider(FOthers.matches[fvs.compose.includeList[i].systemST]);
      for j := 0 to fvs.compose.includeList[i].filterList.count - 1 do
        if not (('concept' = fvs.compose.includeList[i].filterList[j].property_ST) and (fvs.compose.includeList[i].filterList[j].OpST = FilterOperatorIsA)) then
          if not cs.doesFilter(fvs.compose.includeList[i].filterList[j].property_ST, fvs.compose.includeList[i].filterList[j].OpST, fvs.compose.includeList[i].filterList[j].valueST) then
            raise Exception.create('The filter "'+fvs.compose.includeList[i].filterList[j].property_ST +' '+ CODES_TFhirFilterOperator[fvs.compose.includeList[i].filterList[j].OpST]+ ' '+fvs.compose.includeList[i].filterList[j].valueST+'" was not understood in the context of '+cs.system);
    end;
    for i := 0 to fvs.compose.excludeList.Count - 1 do
    begin
      if not FOthers.ExistsByKey(fvs.compose.excludeList[i].systemST) then
        FOthers.Add(fvs.compose.excludeList[i].systemST, FStore.getProvider(fvs.compose.excludeList[i].systemST));
      cs := TCodeSystemProvider(FOthers.matches[fvs.compose.excludeList[i].systemST]);
      for j := 0 to fvs.compose.excludeList[i].filterList.count - 1 do
        if not (('concept' = fvs.compose.excludeList[i].filterList[j].property_ST) and (fvs.compose.excludeList[i].filterList[j].OpST = FilterOperatorIsA)) then
          if not cs.doesFilter(fvs.compose.excludeList[i].filterList[j].property_ST, fvs.compose.excludeList[i].filterList[j].OpST, fvs.compose.excludeList[i].filterList[j].valueST) then
            raise Exception.create('The filter "'+fvs.compose.excludeList[i].filterList[j].property_ST +' '+ CODES_TFhirFilterOperator[fvs.compose.excludeList[i].filterList[j].OpST]+ ' '+fvs.compose.excludeList[i].filterList[j].valueST+'" was not understood in the context of '+cs.system);
    end;
  end;
end;

function TValueSetChecker.rule(op: TFhirOperationOutcome; severity: TFhirIssueSeverity; test: boolean; code, msg: string): boolean;
var
  issue : TFhirOperationOutcomeIssue;
begin
  result := test;
  if not test then
  begin
    issue := op.issueList.Append;
    issue.severityST := severity;
    issue.type_ := TFhirCoding.Create;
    issue.type_.systemST := 'http://hl7.org/fhir/issue-type';
    issue.type_.codeST := code;
    issue.detailsST := msg;
  end;
end;


function TValueSetChecker.findCode(code: String; list : TFhirValueSetDefineConceptList; displays : TStringList): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to list.count - 1 do
  begin
    if (code = list[i].codeST) and not list[i].abstractST then
    begin
      result := true;
      displays.Add(list[i].displayST);
      exit;
    end;
    if findCode(code, list[i].conceptList, displays) then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TValueSetChecker.check(system, code: String): boolean;
var
  list : TStringList;
begin
  list := TStringList.Create;
  try
    result := check(system, code, list);
  finally
    list.Free;
  end;
end;

function TValueSetChecker.check(system, code : String; displays : TStringList) : boolean;
var
  checker : TValueSetChecker;
  cs : TCodeSystemProvider;
  i : integer;
begin
  result := false;
  if (fvs.define <> nil) and (system = fvs.define.systemST) then
  begin
    result := FindCode(code, fvs.define.conceptList, displays);
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
        result := checker.check(system, code, displays);
      end;
    end;
    for i := 0 to fvs.compose.includeList.Count - 1 do
    begin
      if not result then
      begin
        cs := TCodeSystemProvider(FOthers.matches[fvs.compose.includeList[i].systemST]);
        result := (cs.system = system) and checkConceptSet(cs, fvs.compose.includeList[i], code, displays);
      end;
    end;
    for i := 0 to fvs.compose.excludeList.Count - 1 do
    begin
      if result then
      begin
        cs := TCodeSystemProvider(FOthers.matches[fvs.compose.excludeList[i].systemST]);
        result := not ((cs.system = system) and checkConceptSet(cs, fvs.compose.excludeList[i], code, displays));
      end;
    end;
  end;
end;

procedure TValueSetChecker.check(coding: TFhirCoding; op : TFhirOperationOutcome);
var
  list : TStringList;
begin
  list := TStringList.Create;
  try
    if rule(op, IssueSeverityError, check(coding.systemST, coding.codeST, list), 'code-unknown', 'The system/code "'+coding.systemST+'"/"'+coding.codeST+'" is not in the value set') then
      rule(op, IssueSeverityWarning, (coding.displayST = '') or (list.IndexOf(coding.displayST) >= 0), 'value', 'The display "'+coding.displayST+'" is not a valid display for the code');
  finally
    list.Free;
  end;
end;

function TValueSetChecker.check(coding: TFhirCoding): TFhirOperationOutcome;
begin
  result := TFhirOperationOutcome.Create;
  try
    check(coding, result);
    BuildNarrative(result, 'Code Validation');
    result.Link;
  finally
    result.free;
  end;
end;

function TValueSetChecker.check(coded: TFhirCodeableConcept): TFhirOperationOutcome;
begin
  result := TFhirOperationOutcome.Create;
  try
    check(coded, result);
    BuildNarrative(result, 'Code Validation');
    result.Link;
  finally
    result.free;
  end;
end;

procedure TValueSetChecker.check(code: TFhirCodeableConcept; op: TFhirOperationOutcome);
var
  list : TStringList;
  i : integer;
  ok : boolean;
  codelist : String;
begin
  list := TStringList.Create;
  try
    ok := false;
    codelist := '';
    for i := 0 to code.codingList.Count - 1 do
    begin
      codelist := codelist + '{'+code.codingList[i].systemST+'"/"'+code.codingList[i].codeST+'}';
      ok := ok or check(code.codingList[i].systemST, code.codingList[i].codeST, list);
    end;

  finally
    list.Free;
  end;

end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    cs.Close(ctxt);
end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderFilterContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    cs.Close(ctxt);
end;

function TValueSetChecker.checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeInclude; code: String; displays : TStringList): boolean;
var
  i : integer;
  fc : TFhirValueSetComposeIncludeFilter;
  ctxt : TCodeSystemProviderFilterContext;
  loc :  TCodeSystemProviderContext;
  prep : TCodeSystemProviderFilterPreparationContext;
  filters : Array of TCodeSystemProviderFilterContext;
begin
  result := false;
  if (cset.codeList.count = 0) and (cset.filterList.count = 0) then
  begin
    loc := cs.locate(code);
    try
      result := loc <> nil;
      if result then
      begin
        cs.displays(loc, displays);
        exit;
      end;
    finally
      cs.Close(loc);
    end;
  end;

  for i := 0 to cset.codeList.count - 1 do
    if (code = cset.codeList[i].value) and (cs.locate(code) <> nil) then
    begin
      cs.displays(code, displays);
      result := true;
      exit;
    end;

  if cset.filterList.count > 0 then
  begin
    SetLength(filters, cset.filterList.count);
    prep := cs.getPrepContext;
    try
      for i := 0 to cset.filterList.count - 1 do
      begin
        fc := cset.filterList[i];
        if ('concept' = fc.property_ST) and (fc.OpST = FilterOperatorIsA) then
          filters[i] := cs.filter(fc.property_ST, fc.OpST, fc.valueST, prep);
      end;
      if cs.prepare(prep) then // all are together, just query the first filter
      begin
        ctxt := filters[0];
        loc := cs.filterLocate(ctxt, code);
        try
          result := loc <> nil;
          if result then
            cs.displays(loc, displays);
        finally
          cs.Close(loc);
        end;
      end
      else
      begin
        for i := 0 to cset.filterList.count - 1 do
        begin
          fc := cset.filterList[i];
          if ('concept' = fc.property_ST) and (fc.OpST = FilterOperatorIsA) then
          begin
            loc := cs.locateIsA(code, fc.valueST);
            try
              result := loc <> nil;
              if result then
                cs.displays(loc, displays);
            finally
              cs.Close(loc);
            end;
          end
          else
          begin
            ctxt := filters[i];
            loc := cs.filterLocate(ctxt, code);
            try
              result := loc <> nil;
              if result then
                cs.displays(loc, displays);
            finally
              cs.Close(loc);
            end;
          end;
          if result then
            break;
        end;
      end;
    finally
      for i := 0 to cset.filterList.count - 1 do
        cs.Close(filters[i]);
      cs.Close(prep);
    end;
  end;
end;

end.
