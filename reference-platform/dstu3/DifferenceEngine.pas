unit DifferenceEngine;

interface

uses
  SysUtils, Classes, MsXmlParser,
  AdvObjects, AdvGenerics, StringSupport, TextUtilities,
  FHIRBase, FHIRTypes, FHIRResources, FHIRParser,
  MsXml;

type
  TDifferenceOperation = (diffAdd, diffDelete, diffReplace);

const
  CODES_DIFF_OP : Array [TDifferenceOperation] of String = ('add', 'delete', 'replace');

type
  TDifference = class (TAdvObject)
  private
    FPath : String;
    FOp : TDIfferenceOperation;
    FName : String;
    FValue : TFHIRBase;
  public
    Constructor CreateReplace(path : String; value : TFHIRBase);
    Constructor CreateAdd(path, name : String; value : TFHIRBase);
    Constructor CreateDelete(path : String); overload;
    Destructor Destroy; override;

    property Path : String read FPath;
    property Op : TDIfferenceOperation read FOp;
    property Name : String read FName;
    property Value : TFHIRBase read FValue;
  end;

  TDifferenceMatch = class (TAdvObject)
  private
    FSourceIndex : integer;
    FTargetIndex : integer;
  public
    Constructor Create(sourceIndex, targetIndex : integer);
  end;

  TDifferenceEngine = class (TAdvObject)
  private
    function matchRating(obj1, obj2 : TFHIRBase) : Double;
    function listUnchanged(matches : TAdvList<TDifferenceMatch>; l1, l2 : integer) : boolean;
    function matchedTarget(matches : TAdvList<TDifferenceMatch>; ti : integer) : boolean;
    function matchedSource(matches : TAdvList<TDifferenceMatch>; si : integer) : boolean;
    procedure findCertainMatches(matches : TAdvList<TDifferenceMatch>; bl, ml : TFHIRObjectList);
    procedure findPossibleMatches(matches : TAdvList<TDifferenceMatch>; bl, ml : TFHIRObjectList);
    procedure generate(path : String; base, modified : TFHIRBase; differences : TAdvList<TDifference>);
    procedure encodeValue(part : TFhirParametersParameter; value : TFHIRBase);
    function asParams(differences : TAdvList<TDifference>) : TFHIRParameters;
  public
    function generateDifference(base, modified : TFHIRBase) : TFHIRParameters;
    function applyDifference(base : TFHIRBase; delta : TFHIRParameters) : TFHIRBase;
  end;

  TDifferenceEngineTests = class (TAdvObject)
  private
    FDom : IXMLDOMDocument2;
    function parseResource(elem : IXMLDOMElement) : TFhirResource;
    function AsXml(res : TFHIRResource) : String;
    procedure CompareXml(name : String; expected, obtained : TFHIRResource);
    procedure execCase(name : String; mode : String; input : TFhirResource; diff : TFhirParameters; output : TFhirResource);
  public
    constructor Create(path : String); virtual;
    destructor Destroy; override;

    procedure execute;
  end;

implementation

{ TDifferenceEngine }

function TDifferenceEngine.applyDifference(base: TFHIRBase; delta: TFHIRParameters): TFHIRBase;
begin
  raise Exception.Create('Not done yet');
end;

function TDifferenceEngine.generateDifference(base, modified: TFHIRBase): TFHIRParameters;
var
  list : TAdvList<TDifference>;
begin
  list := TAdvList<TDifference>.create;
  try
    generate(base.fhirType, base, modified, list);
    result := asParams(list);
  finally
    list.free;
  end;
end;

function TDifferenceEngine.listUnchanged(matches: TAdvList<TDifferenceMatch>; l1, l2 : integer): boolean;
var
  i : integer;
begin
  result := true;
  if (l1 <> l2) or (l1 <> matches.Count) then
    exit(false);
  for I := 0 to matches.Count - 1 do
    if (matches[i].FSourceIndex <> i) or (matches[i].FTargetIndex <> i)  then
      exit(false);
end;

function TDifferenceEngine.matchedTarget(matches: TAdvList<TDifferenceMatch>; ti: integer): boolean;
var
  dm : TDifferenceMatch;
begin
  result := false;
  for dm in matches do
    if dm.FTargetIndex = ti then
      exit(true);
end;

function TDifferenceEngine.matchRating(obj1, obj2: TFHIRBase): Double;
var
  ol1, ol2 : TFHIRPropertyList;
  o1, o2 : TFHIRProperty;
  ov1, ov2 : TFHIRBase;
  t, c, i1, i2 : integer;
  m : boolean;
begin
  ol1 := obj1.createPropertyList(true);
  ol2 := obj2.createPropertyList(true);
  try
    t := 0;
    c := 0;
    for o1 in ol1 do
    begin
      o2 := ol2.ByName[o1.Name];
      o1.forceValues;
      o2.forceValues;
      if (o1.Values.Count > 0) or (o2.Values.Count > 0) then
      begin
        if o1.IsList then
        begin
          for i1 := 0 to o1.Values.Count - 1 do
          begin
            ov1 := o1.Values[i1] as TFHIRBase;
            inc(t);
            m := false;
            for i2 := 0 to o2.Values.Count - 1 do
            begin
              ov2 := o2.Values[0] as TFHIRBase;
              if (ov1.fhirType = ov2.fhirType) and ((ov1.getid = ov2.getid) or compareDeep(ov1, ov2, false)) then
                m := true;
            end;
            if (m) then
              inc(c);
          end;
        end
        else
        begin
          inc(t);
          if (o1.Values.Count > 0) and (o2.Values.Count > 0) then
          begin
            ov1 := o1.Values[0] as TFHIRBase;
            ov2 := o2.Values[0] as TFHIRBase;
            if (ov1.fhirType = ov2.fhirType) and ((ov1.getid = ov2.getid) or compareDeep(ov1, ov2, false)) then
              inc(c);
          end;
        end;
      end;
    end;
  finally
    ov1.Free;
    ov2.Free;
  end;
  if (t = 0) then
    result := 0
  else
    result := c / t;
end;

function TDifferenceEngine.matchedSource(matches: TAdvList<TDifferenceMatch>; si: integer): boolean;
var
  dm : TDifferenceMatch;
begin
  result := false;
  for dm in matches do
    if dm.FSourceIndex = si then
      exit(true);
end;

function TDifferenceEngine.asParams(differences: TAdvList<TDifference>): TFHIRParameters;
var
  diff : TDifference;
  p, pp : TFhirParametersParameter;
begin
  result := TFhirParameters.Create;
  try
    for diff in differences do
    begin
      if diff.FOp = diffDelete then
      begin
        p := result.parameterList.Append;
        p.name := 'operation';
        pp := p.partList.Append;
        pp.name := 'type';
        pp.value := TFhirCode.Create(CODES_DIFF_OP[diffDelete]);
        pp := p.partList.Append;
        pp.name := 'path';
        pp.value := TFhirString.Create(diff.Path);
      end
      else
      begin
        p := result.parameterList.Append;
        p.name := 'operation';
        pp := p.partList.Append;
        pp.name := 'type';
        pp.value := TFhirCode.Create(CODES_DIFF_OP[diff.FOp]);
        pp := p.partList.Append;
        pp.name := 'path';
        pp.value := TFhirString.Create(diff.Path);
        if diff.Name <> '' then
        begin
          pp := p.partList.Append;
          pp.name := 'name';
          pp.value := TFhirString.Create(diff.Name);
        end;
        pp := p.partList.Append;
        pp.name := 'value';
        encodeValue(pp, diff.Value);
      end;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TDifferenceEngine.encodeValue(part: TFhirParametersParameter; value: TFHIRBase);
var
  pl : TFHIRPropertyList;
  p : TFHIRProperty;
  b : TFHIRObject;
  pp : TFhirParametersParameter;
begin
  if value is TFHIREnum then
    pp.value := TFHIRCode.create(TFHIREnum(value).value)
  else if (value is TFHIRType) and (Value.isPrimitive or StringArrayExistsSensitive(['Annotation', 'Attachment', 'Identifier', 'CodeableConcept', 'Coding', 'Quantity', 'Range', 'Period', 'Ratio', 'SampledData', 'Signature', 'HumanName', 'Address', 'ContactPoint', 'Timing', 'Reference', 'Meta'], Value.fhirType)) then
    pp.value := Value.Link as TFHIRType
  else
  begin
    // an anonymous type. So what we do here is create the value, but the value
    // part has no actual value. instead, it contains parts for the properties
    pl := value.createPropertyList(true);
    try
      for p in pl do
      begin
        p.forceValues;
        for b in p.Values do
        begin
          pp := part.partList.Append;
          pp.name := p.Name;
          encodeValue(pp, b as TFhirBase);
        end;
      end;
    finally
      pl.free;
    end;

  end;
end;

procedure TDifferenceEngine.findCertainMatches(matches: TAdvList<TDifferenceMatch>; bl, ml: TFHIRObjectList);
var
  bi, mi : integer;
  bv, mv : TFHIRBase;
begin
  for bi := 0 to bl.Count - 1 do
  begin
    for mi := 0 to ml.Count - 1 do
    begin
      if not matchedTarget(matches, mi) then
      begin
        mv := ml[mi] as TFHIRBase;
        bv := bl[bi] as TFHIRBase;
        if (bv.fhirType = mv.fhirType) then
        begin
          if (bv.getId <> '') and (bv.getid = mv.getid) then
             matches.Add(TDifferenceMatch.Create(bi, mi))
          else if compareDeep(bv, mv, false) then
             matches.Add(TDifferenceMatch.Create(bi, mi))
        end;
      end;
    end;
  end;
end;

procedure TDifferenceEngine.findPossibleMatches(matches: TAdvList<TDifferenceMatch>; bl, ml: TFHIRObjectList);
var
  bi, mi : integer;
  bv, mv : TFHIRBase;
begin
  for bi := 0 to bl.Count - 1 do
  begin
    if not matchedSource(matches, bi) then
    begin
      for mi := 0 to ml.Count - 1 do
      begin
        if not matchedTarget(matches, mi) then
        begin
          mv := ml[mi] as TFHIRBase;
          bv := bl[bi] as TFHIRBase;
          if (bv.fhirType = mv.fhirType) then
          begin
            if matchRating(mv, bv) > 0.5 then
              matches.Add(TDifferenceMatch.Create(bi, mi))
          end;
        end;
      end;
    end;
  end;
end;

procedure TDifferenceEngine.generate(path : String; base, modified : TFHIRBase; differences : TAdvList<TDifference>);
var
  bl, ml : TFHIRPropertyList;
  b, m : TFHIRProperty;
  bv, mv : TFHIRBase;
  i : integer;
  matches : TAdvList<TDifferenceMatch>;
  n : String;
begin
  if base.fhirType <> modified.fhirType then
    raise Exception.Create('Unable to generate difference for different types ('+base.fhirType+'/'+modified.fhirType+')');
  bl := base.createPropertyList(true);
  ml := modified.createPropertyList(true);
  try
    for b in bl do
    begin
      m := ml.ByName[b.Name];
      b.forceValues;
      m.forceValues;
      if b.IsList and ((b.Values.Count > 1) or (m.Values.Count > 1)) then
      begin
        matches := TAdvList<TDifferenceMatch>.create;
        try
          // map the certain matches between lists
          findCertainMatches(matches, b.Values, m.Values);
          // map the probabilistic matches between the lists
          findPossibleMatches(matches, b.Values, m.Values);
          if listUnchanged(matches, b.Values.Count, m.Values.Count) then
            for i := 0 to b.Values.Count - 1 do
              generate(path+'.'+b.Name+'['+inttostr(i)+']', b.Values[i] as TFHIRBase, m.Values[i] as TFHIRBase, differences)
          else
            raise Exception.Create('Error Message');
          // work through the source list, updating, moving, or deleting
          // work through the target list, inserting anything new
        finally
          matches.Free;
        end;
      end
      else
      begin
        if b.IsList then
          n := b.Name+'[0]'
        else
          n := b.Name;
        if b.Values.Count = 0 then
        begin
          if m.Values.Count <> 0 then
            differences.Add(TDifference.CreateAdd(path, b.Name, m.Values[0].link as TFHIRBase));
        end
        else
        begin
          if m.Values.Count = 0 then
            differences.Add(TDifference.CreateDelete(path+'.'+n))
          else
          begin
            bv := b.Values[0] as TFHIRBase;
            mv := m.Values[0] as TFHIRBase;
            // If their type is different, replace
            // if their id is different, replace
            if (bv.fhirType <> mv.fhirType) or (bv.getid <> mv.getid) then
              differences.Add(TDifference.CreateReplace(path+'.'+n, mv.link))
            // if it's a primitive, replace
            else if bv.isPrimitive then
            begin
              if not compareDeep(bv, mv, false) then
                differences.Add(TDifference.CreateReplace(path+'.'+n, mv.Link))
            end
            // otherwise, generate for the type
            else
              generate(path+'.'+n, bv, mv, differences);
          end;
        end;
      end;
    end;
  finally
    bl.Free;
    ml.Free;
  end;
end;

{ TDifference }

constructor TDifference.CreateReplace(path: String; value: TFHIRBase);
begin
  inherited create;
  FPath := path;
  FOp := diffReplace;
  FValue := value;
end;

constructor TDifference.CreateAdd(path, name: String; value: TFHIRBase);
begin
  inherited create;
  FPath := path;
  FOp := diffAdd;
  FName := name;
  FValue := value;
end;

constructor TDifference.CreateDelete(path: String);
begin
  inherited create;
  FPath := path;
  FOp := diffDelete;
end;

destructor TDifference.Destroy;
begin
  FValue.Free;
  inherited;
end;

{ TDifferenceEngineTests }

function TDifferenceEngineTests.AsXml(res: TFHIRResource): String;
var
  p : TFHIRXmlComposer;
  s : TStringStream;
begin
  p := TFHIRXmlComposer.Create(nil, 'en');
  try
    s := TStringStream.Create;
    try
      p.Compose(s, res, true);
      result := s.DataString;
    finally
      s.Free;
    end;
  finally
    p.Free;
  end;

end;

procedure TDifferenceEngineTests.CompareXml(name : String; expected, obtained: TFHIRResource);
var
  e, o : String;
begin
  e := asXml(expected);
  o := asXml(obtained);
  StringToFile(e, 'c:\temp\expected.xml', TEncoding.UTF8);
  StringToFile(o, 'c:\temp\obtained.xml', TEncoding.UTF8);
  if e <> o then
  begin
    raise Exception.Create('Difference does not match for '+name);
  end;
end;

constructor TDifferenceEngineTests.Create(path: String);
begin
  inherited Create;
  FDom := TMsXmlParser.Parse(path);
end;

destructor TDifferenceEngineTests.Destroy;
begin
  inherited;
end;

procedure TDifferenceEngineTests.execCase(name: String; mode : String; input: TFhirResource; diff: TFhirParameters; output: TFhirResource);
var
  engine : TDifferenceEngine;
  delta : TFhirParameters;
begin
  if (mode = 'both') or (mode = 'reverse') then
  begin
    engine := TDifferenceEngine.Create;
    try
      delta := engine.generateDifference(input, output);
      try
        compareXml(name, diff, delta);
      finally
        delta.Free;
      end;
    finally
      engine.free;
    end;
  end;
end;

procedure TDifferenceEngineTests.execute;
var
  child : IXMLDOMElement;
  input, output : TFhirResource;
  diff : TFhirParameters;
begin
  child := TMsXmlParser.FirstChild(FDom.documentElement);
  while child <> nil do
  begin
    if child.nodeName = 'case' then
    begin
      input := parseResource(TMsXmlParser.NamedChild(child, 'input'));
      try
        output := parseResource(TMsXmlParser.NamedChild(child, 'output'));
        try
          diff := parseResource(TMsXmlParser.NamedChild(child, 'diff')) as TFHIRParameters;
          try
            execCase(child.getAttribute('name'), child.getAttribute('mode'), input, diff, output);
          finally
            diff.Free;
          end;
        finally
          output.free;
        end;
      finally
        input.Free;
      end;
    end;
    child := TMsXmlParser.NextSibling(child);
  end;
end;

function TDifferenceEngineTests.parseResource(elem: IXMLDOMElement): TFhirResource;
var
  p : TFHIRXmlParser;
begin
  p := TFHIRXmlParser.Create(nil, 'en');
  try
    p.Element := TMsXmlParser.FirstChild(elem);
    p.Parse;
    result := p.resource.Link;
  finally
    p.Free;
  end;

end;

{ TDifferenceMatch }

constructor TDifferenceMatch.Create(sourceIndex, targetIndex: integer);
begin
  inherited Create;
  FSourceIndex := sourceIndex;
  FTargetIndex := targetIndex;
end;

end.
