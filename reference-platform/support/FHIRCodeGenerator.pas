unit FHIRCodeGenerator;

interface

uses
  SysUtils, Classes, Character,
  StringSupport, DateSupport,
  AdvObjects,
  FHIRBase, FHIRTypes, FHIRResources, FHIRContext, FHIRXhtml;

type
  TFHIRCodeGenerator = class (TAdvObject)
  private
    FResource: TFHIRResource;
    FContext: TWorkerContext;
    procedure SetResource(const Value: TFHIRResource);
    procedure SetContext(const Value: TWorkerContext);
  protected
    function getElementDefinition(sd : TFhirStructureDefinition; path : String) : TFhirElementDefinition;
    function enumify(code : String) : String;
  public
    destructor Destroy; override;
    property Resource : TFHIRResource read FResource write SetResource;
    property Context : TWorkerContext read FContext write SetContext;

    function generate : String; virtual;
    function languageName : String; virtual;
  end;

  TFHIRCodeGeneratorJavaRI = class (TFHIRCodeGenerator)
  public
    function languageName : String; override;
  end;

  TFHIRCodeGeneratorJavaHapi = class (TFHIRCodeGenerator)
  public
    function languageName : String; override;
  end;

  TFHIRCodeGeneratorPascal = class (TFHIRCodeGenerator)
  private
    units : TStringList;
    vars : TStringList;
    lines : TStringList;

    procedure line(indent : integer; s : String);
    function varName(fhirType: String; defn : TFhirElementDefinition; inScope: TArray<String>): string;
    function addVar(inScope : TArray<String>; varName : String) : TArray<String>;
    procedure processResource;
    procedure processObject(indent: integer; name, path : String; sd : TFhirStructureDefinition; obj: TFHIRObject; inScope : TArray<String>);
    procedure processAssignment(indent : integer; variableName: String; varIsSelf : boolean; sd : TFhirStructureDefinition; path : String; prop : TFHIRProperty; value : TFHIRObject; inScope : TArray<String>; defn : TFhirElementDefinition);
    procedure processXhtml(indent : integer; variableName: String; value : TFHIRObject);
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    function languageName : String; override;
    function generate : String; override;

    procedure test;
  end;

  TFHIRCodeGeneratorDotNet = class (TFHIRCodeGenerator)
  public
    function languageName : String; override;
  end;


implementation

{ TFHIRCodeGenerator }

destructor TFHIRCodeGenerator.Destroy;
begin
  FResource.free;
  FContext.Free;
  inherited;
end;

function delphiIse(s : String) : String;
begin
  if (StringArrayExistsInsensitive(['and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const', 'constructor', 'create', 'destructor', 'dispinterface', 'div', 'do', 'downto', 'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally', 'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited', 'initialization', 'inline', 'interface', 'is', 'label', 'library', 'link', 'mod', 'nil', 'not', 'object', 'of', 'or', 'out',
    'packed', 'procedure', 'program', 'property', 'raise', 'record', 'repeat', 'resourcestring', 'set', 'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until', 'uses', 'var', 'while', 'with', 'xor'
    ], s)) then
    result := s + '_'
  else
    result := s;
end;


function TFHIRCodeGenerator.enumify(code: String): String;
var
  b : TStringBuilder;
  ch : char;
  ws : boolean;
begin
  if (code = '-') then
    result := 'Minus'
  else if (code = '+') then
    result := 'Plus'
  else
  begin
    b := TStringBuilder.Create;
    try
      result := code.replace('-', ' ').replace('+', ' ');
      ws := true;
      for ch in result do
        if ch.IsWhiteSpace then
          ws := true
        else
        begin
          if ws then
          begin
            b.Append(ch.ToUpper);
            ws := false;
          end
          else
            b.Append(ch);
        end;
      result := b.ToString;
    finally
      b.Free;
    end;
    result := result.replace('>=', 'greaterOrEquals').replace('<=', 'lessOrEquals').replace('<', 'lessThan').replace('>', 'greaterThan').replace('=', 'equal');
  end;
  result := delphiIse(result);
end;

function TFHIRCodeGenerator.generate: String;
begin
  result := 'Code Generation for '+languageName+' not done yet';
end;

function TFHIRCodeGenerator.getElementDefinition(sd: TFhirStructureDefinition; path: String): TFhirElementDefinition;
var
  t : TFhirElementDefinition;
begin
  result := nil;
  for t in sd.snapshot.elementList do
    if t.path = path then
      exit(t);
end;

function TFHIRCodeGenerator.languageName: String;
begin
  result := 'Unknown';
end;

procedure TFHIRCodeGenerator.SetContext(const Value: TWorkerContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TFHIRCodeGenerator.SetResource(const Value: TFHIRResource);
begin
  FResource.free;
  FResource := Value;
end;

{ TFHIRCodeGeneratorJavaRI }

function TFHIRCodeGeneratorJavaRI.languageName: String;
begin
  result := 'Java (for reference implementation)';
end;

{ TFHIRCodeGeneratorJavaHapi }

function TFHIRCodeGeneratorJavaHapi.languageName: String;
begin
  result := 'Java (for HAPI interface)';
end;

{ TFHIRCodeGeneratorPascal }

function TFHIRCodeGeneratorPascal.addVar(inScope: TArray<String>; varName: String): TArray<String>;
var
  i : integer;
begin
  setLength(result, length(inscope)+1);
  for I := 0 to length(inScope) - 1 do
    result[i] := inscope[i];
  result[length(inscope)] := varName;
end;

constructor TFHIRCodeGeneratorPascal.Create;
begin
  inherited;
  units := TStringList.Create;
  units.duplicates := dupIgnore;
  vars := TStringList.Create;
  vars.NameValueSeparator := ':';
  lines := TStringList.create;
end;

destructor TFHIRCodeGeneratorPascal.Destroy;
begin
  vars.Free;
  units.Free;
  lines.Free;
  inherited;
end;

function TFHIRCodeGeneratorPascal.generate: String;
var
  b : TStringBuilder;
  first : boolean;
  s, l, r : String;
begin
  processResource;
  b := TStringBuilder.Create;
  try
    b.Append('// uses ');
    first := true;
    for s in units do
    begin
      if first then first := false else b.Append(', ');
        b.Append(s);
    end;
    b.AppendLine;
    b.AppendLine;
    if (vars.Count > 0) then
    begin
      b.Append('var');
      b.AppendLine;
      for s in vars do
      begin
        StringSplit(s, ':', l, r);
        b.Append('  ');
        b.Append(l);
        b.Append(' : ');
        b.Append(r);
        b.Append(';');
        b.AppendLine;
      end;
    end;
    b.Append('begin');
    b.AppendLine;
    for s in lines do
    begin
      b.Append(s);
      b.AppendLine;
    end;
    b.Append('end;');
    b.AppendLine;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFHIRCodeGeneratorPascal.languageName: String;
begin
  result := 'Pascal';
end;

procedure TFHIRCodeGeneratorPascal.line(indent : integer; s: String);
begin
  lines.Add(StringPadLeft('', ' ', indent)+ s);
end;

function capitalize(s : String): String;
begin
  if s = '' then
    result := s
  else
    result := Uppercase(s[1]) + s.Substring(1);
end;

procedure TFHIRCodeGeneratorPascal.processAssignment(indent: integer; variableName : String; varIsSelf : boolean; sd : TFhirStructureDefinition; path : String; prop: TFHIRProperty; value : TFHIRObject; inScope : TArray<String>; defn : TFhirElementDefinition);
var
  t : String;
  vs : TFhirValueSet;
  tsd : TFhirStructureDefinition;
  vn, an : string;
begin
  t := prop.Type_;
  if varIsSelf then
    an := variableName+'.value'
  else
    an := variableName+'.'+delphiIse(prop.Name.Replace('[x]', ''));

  if StringArrayExistsSensitive(['string', 'id', 'markdown', 'uri', 'base64Binary', 'oid', 'id'], t) then
    line(indent, an+' := '''+value.primitiveValue+''';')
  else if t = 'code' then
  begin
    vs := nil;
    if (defn <> nil) and (defn.binding <> nil) and (defn.binding.strength = BindingStrengthRequired) and (defn.binding.ValueSet <> nil) then
      if (defn.binding.valueSet is TFhirUri) then
        vs := FContext.fetchResource(frtValueSet, (defn.binding.valueSet as TFhirUri).value) as TFHIRValueSet
      else
        vs := FContext.fetchResource(frtValueSet, (defn.binding.valueSet as TFhirReference).reference) as TFHIRValueSet;
    try
      if (vs <> nil) then
        line(indent, an+' := '+vs.name+capitalize(enumify(value.primitiveValue))+';')
      else
        line(indent, an+' := '''+value.primitiveValue+''';')
    finally
      vs.Free;
    end;
  end
  else if StringArrayExistsSensitive(['integer', 'decimal', 'unsignedInt', 'positiveInt', 'boolean'], t) then
    line(indent, an+' := '''+value.primitiveValue+''';')
  else if StringArrayExistsSensitive(['date', 'dateTime', 'instant'], t) then
  begin
    units.add('DateSupport');
    line(indent, an+' := TDateTimeEx.fromXML('''+value.primitiveValue+''');')
  end
  // we assume this is an object
  else
  begin
    tsd := FContext.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/'+value.fhirType) as TFhirStructureDefinition;
    if (tsd <> nil) then
    begin
      try
        if varIsSelf then
          processObject(indent+2, variableName, tsd.type_, tsd, value, inScope)
        else
        begin
          vn := varName(value.fhirType, defn, inScope);
          line(indent, vn +' := '+vars.values[vn]+'.create;');
          line(indent, 'try');
          processObject(indent+2, vn, tsd.type_, tsd, value, addVar(inScope, vn));
          line(indent, '  '+variableName+'.'+prop.Name.Replace('[x]', '')+' := '+vn+'.Link;');
          line(indent, 'finally');
          line(indent, '  '+vn+'.Free');
          line(indent, 'end;');
        end;
      finally
        tsd.Free;
      end;
    end
    else if varIsSelf then
      processObject(indent+2, variableName, path+'.'+prop.Name, sd, value, inScope)
    else
    begin
      vn := varName(path+'.'+prop.Name, defn, inScope);
      line(indent, vn +' := '+vars.values[vn]+'.create;');
      line(indent, 'try');
      processObject(indent+2, vn, tsd.type_, tsd, value, addVar(inScope, vn));
      line(indent, '  '+variableName+'.'+prop.Name.Replace('[x]', '')+' := '+vn+'.Link;');
      line(indent, 'finally');
      line(indent, '  '+vn+'.Free');
      line(indent, 'end;');
    end;
  end;
end;

function isXhtml(defn : TFhirElementDefinition) : boolean;
begin
  result := (defn.type_List.Count = 1) and (defn.type_List[0].code = 'xhtml');
end;

procedure TFHIRCodeGeneratorPascal.processObject(indent: integer; name, path : String; sd : TFhirStructureDefinition; obj: TFHIRObject; inScope : TArray<String>);
var
  props : TFHIRPropertyList;
  prop : TFHIRProperty;
  value : TFHIRObject;
  vn : String;
  defn : TFhirElementDefinition;
begin
  props := obj.createPropertyList(true);
  try
    for prop in props do
      if prop.hasValue then
      begin
        defn := getElementDefinition(sd, path+'.'+prop.Name);
        if prop.IsList then
        begin
          for value in prop.Values do
          begin
            if (defn <> nil) and (defn.type_List.IsEmpty) then
              vn := varName(path+'.'+prop.Name, defn, inScope)
            else
              vn := varName(value.fhirType, defn, inScope);
            line(indent, vn +' := '+name+'.'+prop.Name+'List.append;');
            processAssignment(indent, vn, true, sd, path, prop, value, addVar(inScope, vn), defn);
          end;
        end
        else if isXhtml(defn) then
          processXhtml(indent, name, prop.values[0])
        else
          processAssignment(indent, name, false, sd, path, prop, prop.Values[0], inScope, defn);
      end;
  finally
    props.Free;
  end;
end;

procedure TFHIRCodeGeneratorPascal.processResource;
var
  sd : TFHIRStructureDefinition;
begin
  units.Add('FHIRTypes');
  units.Add('FHIRResources');
  units.Add('FHIRUtilities');
  vars.Values['res'] := 'TFHIR'+resource.fhirType;
  line(2, 'res := TFHIR'+resource.fhirType+'.create;');
  line(2, 'try');
  sd := FContext.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/'+resource.fhirType) as TFhirStructureDefinition;
  try
    processObject(4, 'res', resource.fhirType, sd, resource, addVar([], 'res'));
  finally
    sd.free;
  end;
  line(2, 'finally');
  line(2, '  res.free;');
  line(2, 'end;');
end;

function delphiStringWrap(indent : integer; s : String) : String;
var
  i : integer;
begin
  result := s;
  i := 244-indent;
  while i < length(result) do
  begin
    insert('''+'+#13#10+StringPadLeft('', ' ', indent+2)+'''', result, i);
    inc(i, 250+indent+2);
  end;
  result := ''''+result+'''';
end;

procedure TFHIRCodeGeneratorPascal.processXhtml(indent: integer; variableName: String; value: TFHIRObject);
begin
  units.add('FHIRXhtml');
  line(indent, variableName+'.div_ := TFHIRXhtmlParser.parse(''en'', xppReject, [], '+delphiStringWrap(indent, TFHIRXhtmlParser.compose(value as TFhirXHtmlNode))+'); // (lang, policy, options, html)');
end;

const
  typeNames : array of String = ['boolean', 'integer', 'string', 'decimal', 'uri', 'base64Binary', 'instant', 'date', 'dateTime', 'time', 'code', 'oid', 'id', 'markdown', 'unsignedInt', 'positiveInt'];
  varNames :  array of String = ['b', 'i', 's', 'd', 'u', 'b64', 'inst', 'd', 'dt', 't', 'c', 's', 's', 's', 'i', 'i'];

function isAbstractType(code : string) : boolean;
begin
  result := StringArrayExistsSensitive(['Element', 'BackboneElement', 'Resource', 'DomainResource'], code);
end;

function TFHIRCodeGeneratorPascal.varName(fhirType: String; defn : TFhirElementDefinition; inScope: TArray<String>): string;
var
  ch : Char;
  i : integer;
  last : boolean;
  s : String;
begin
  i := StringArrayIndexOfSensitive(typenames, fhirType);
  if (i > -1) then
    result := varNames[i]
  else
  begin
    result := '';
    last := true;
    for ch in fhirType do
      if ch.IsUpper or last then
      begin
        result := result + ch.ToLower;
        last := ch = '.';
      end;
  end;
  if StringArrayIndexOfInsensitive(inScope, result) > -1 then
  begin
    i := 1;
    while StringArrayIndexOfInsensitive(inScope, result+inttostr(i)) > -1 do
      inc(i);
    result := result + inttostr(i);
  end;
  if vars.Values[result] = '' then
    if (defn.type_List.IsEmpty) or ((defn.type_List.count = 1) and isAbstractType(defn.type_List[0].code)) then
    begin
      s := defn.path;
      for i := 2 to length(s) do
        if (s[i-1] = '.') then
          s[i] := s[i].ToUpper;
      s := s.Replace('.', '');
      vars.Values[result] := 'TFhir'+s;
    end
    else
      vars.Values[result] := 'TFhir'+capitalize(fhirType);
end;

procedure TFHIRCodeGeneratorPascal.test;
// uses FHIRTypes, FHIRResources, FHIRUtilities, FHIRXhtml, DateSupport

var
  res : TFHIRObservation;
  n : TFhirNarrative;
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
  r : TFhirReference;
  dt : TFhirDateTime;
  q : TFhirQuantity;
begin
  res := TFHIRObservation.create;
  try
    res.id := 'example';
    n := TFhirNarrative.create;
    try
      n.status := NarrativeStatusGenerated;
      n.div_ := TFHIRXhtmlParser.parse('en', xppReject, [], '<div xmlns="http://www.w3.org/1999/xhtml"><p><b>Generated Narrative with Details</b></p><p><b>id</b>: example</p><p><b>status</b>: final</p><p><b>category</b>: Vital Signs <span>(Details : {http://hl7.org/fhir/observation-category code &'+
        '#39;vital-signs&#39; = &#39;Vital Signs&#39;, given as &#39;Vital Signs&#39;})</span></p><p><b>code</b>: Body Weight <span>(Details : {LOINC code &#39;29463-7&#39; = &#39;Body weight&#39;, given as &#39;Body Weight&#39;}; {LOINC code &#39;3141-9'+
        '&#39; = &#39;Body weight Measured&#39;, given as &#39;Body weight Measured&#39;}; {SNOMED CT code &#39;27113001&#39; = &#39;Body weight&#39;, given as &#39;Body weight&#39;}; {http://acme.org/devices/clinical-codes code &#39;body-weight&#39; = &'+
        '#39;body-weight&#39;, given as &#39;Body Weight&#39;})</span></p><p><b>subject</b>: <a>Patient/example</a></p><p><b>context</b>: <a>Encounter/example</a></p><p><b>effective</b>: 28/03/2016</p><p><b>value</b>: 185 lbs<span> (Details: UCUM code [l'+
        'b_av] = &#39;lb_av&#39;)</span></p></div>'); // (lang, policy, options, html)
      res.text := n.Link;
    finally
      n.Free
    end;
    res.status := ObservationStatusFinal;
    cc := res.categoryList.append;
      c := cc.codingList.append;
        c.system := 'http://hl7.org/fhir/observation-category';
        c.code := 'vital-signs';
        c.display := 'Vital Signs';
    cc := TFhirCodeableConcept.create;
    try
      c := cc.codingList.append;
        c.system := 'http://loinc.org';
        c.code := '29463-7';
        c.display := 'Body Weight';
      c := cc.codingList.append;
        c.system := 'http://loinc.org';
        c.code := '3141-9';
        c.display := 'Body weight Measured';
      c := cc.codingList.append;
        c.system := 'http://snomed.info/sct';
        c.code := '27113001';
        c.display := 'Body weight';
      c := cc.codingList.append;
        c.system := 'http://acme.org/devices/clinical-codes';
        c.code := 'body-weight';
        c.display := 'Body Weight';
      res.code := cc.Link;
    finally
      cc.Free
    end;
    r := TFhirReference.create;
    try
      r.reference := 'Patient/example';
      res.subject := r.Link;
    finally
      r.Free
    end;
    r := TFhirReference.create;
    try
      r.reference := 'Encounter/example';
      res.context := r.Link;
    finally
      r.Free
    end;
    dt := TFhirDateTime.create;
    try
      dt.value := TDateTimeEx.fromXML('28/03/2016');
      res.effective := dt.Link;
    finally
      dt.Free
    end;
    q := TFhirQuantity.create;
    try
      q.value := '185';
      q.unit_ := 'lbs';
      q.system := 'http://unitsofmeasure.org';
      q.code := '[lb_av]';
      res.value := q.Link;
    finally
      q.Free
    end;
  finally
    res.free;
  end;
end;

{ TFHIRCodeGeneratorDotNet }

function TFHIRCodeGeneratorDotNet.languageName: String;
begin
  result := 'DotNet';
end;

end.
