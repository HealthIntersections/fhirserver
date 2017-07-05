unit FHIRCodeGenerator;

interface

uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects,
  FHIRBase, FHIRTypes, FHIRResources, FHIRContext;

type
  TFHIRCodeGenerator = class (TAdvObject)
  private
    FResource: TFHIRResource;
    FContext: TWorkerContext;
    procedure SetResource(const Value: TFHIRResource);
    procedure SetContext(const Value: TWorkerContext);
  protected
    function getElementDefinition(sd : TFhirStructureDefinition; path : String) : TFhirElementDefinition;
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
    procedure processResource;
    procedure processObject(indent: integer; name, path : String; sd : TFhirStructureDefinition; obj: TFHIRObject);
    procedure processAssignment(indent : integer; name : String; sd : TFhirStructureDefinition; path : String; prop : TFHIRProperty);
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    function languageName : String; override;
    function generate : String; override;
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

constructor TFHIRCodeGeneratorPascal.Create;
begin
  inherited;
  units := TStringList.Create;
  vars := TStringList.Create;
  vars.Delimiter := ':';
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
  s : String;
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
        b.Append('  ');
        b.Append(s);
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
    b.Append('end');
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

procedure TFHIRCodeGeneratorPascal.processAssignment(indent: integer; name: String; sd : TFhirStructureDefinition; path : String; prop: TFHIRProperty);
var
  t : String;
  defn : TFhirElementDefinition;
  vs : TFhirValueSet;
begin
  defn := getElementDefinition(sd, path+'.'+prop.Name);
  t := prop.Type_;
  if StringArrayExistsSensitive(['string', 'id', 'markdown', 'uri', 'base64Binary', 'oid', 'id'], t) then
    line(indent, name+'.'+prop.Name+' := '''+prop.Values[0].primitiveValue+''';')
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
        line(indent, name+'.'+prop.Name+' := '+vs.name+capitalize(prop.Values[0].primitiveValue)+';')
      else
        line(indent, name+'.'+prop.Name+' := '''+prop.Values[0].primitiveValue+''';')
    finally
      vs.Free;
    end;
  end
  else if StringArrayExistsSensitive(['integer', 'decimal', 'unsignedInt', 'positiveInt'], t) then
    line(indent, name+'.'+prop.Name+' := '''+prop.Values[0].primitiveValue+''';')
  else if StringArrayExistsSensitive(['date', 'dateTime', 'instant'], t) then
    line(indent, name+'.'+prop.Name+' := TDateTimeEx.fromXML('''+prop.Values[0].primitiveValue+''');')
  else
    line(indent, name+'.'+prop.Name+' := ??;');
end;

procedure TFHIRCodeGeneratorPascal.processObject(indent: integer; name, path : String; sd : TFhirStructureDefinition; obj: TFHIRObject);
var
  props : TFHIRPropertyList;
  prop : TFHIRProperty;

begin
  props := obj.createPropertyList(true);
  try
    for prop in props do
      if prop.hasValue then
        if prop.IsList then
          line(indent, name+'.add'+capitalize(prop.Name)+'(something);')
        else
          processAssignment(indent, name, sd, path, prop);
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
    processObject(4, 'res', resource.fhirType, sd, resource);
  finally
    sd.free;
  end;
  line(2, 'finally');
  line(2, '  res.free;');
  line(2, 'end;');
end;

{ TFHIRCodeGeneratorDotNet }

function TFHIRCodeGeneratorDotNet.languageName: String;
begin
  result := 'DotNet';
end;

end.
