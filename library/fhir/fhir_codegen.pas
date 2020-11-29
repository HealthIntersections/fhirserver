unit fhir_codegen;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Character,
  fsl_base, fsl_utilities, fsl_fpc, fsl_http,
  fhir_objects, fhir_xhtml, fhir_common, fhir_factory;

type
  TFHIRCodeGenerator = class (TFslObject)
  private
    FResource: TFHIRResourceV;
    FContext: TFHIRWorkerContextWithFactory;

    lines : TStringList;
    vars : TStringList;

    procedure line(indent : integer; s : String);
    function addVar(inScope : TArray<String>; varName : String) : TArray<String>;

    procedure SetResource(const Value: TFHIRResourceV);
    procedure SetContext(const Value: TFHIRWorkerContextWithFactory);
  protected
    function getElementDefinition(sd : TFhirStructureDefinitionW; path : String) : TFhirElementDefinitionW;
    function enumify(code : String) : String;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    property Resource : TFHIRResourceV read FResource write SetResource;
    property Context : TFHIRWorkerContextWithFactory read FContext write SetContext;

    function generate : String; virtual;
    function languageName : String; virtual;
  end;

  TFHIRCodeGeneratorJavaRI = class (TFHIRCodeGenerator)
  private
    imports : TStringList;
    function verPack : String;
    function varName(fhirType: String; sd : TFhirStructureDefinitionW; defn : TFhirElementDefinitionW; inScope: TArray<String>; var vt : String; var def : String): string;
    procedure processXhtml(indent : integer; variableName: String; value : TFHIRObject);
    procedure processAssignment(indent : integer; variableName: String; varIsSelf : boolean; sd : TFhirStructureDefinitionW; path : String; prop : TFHIRProperty; value : TFHIRObject; inScope : TArray<String>; defn : TFhirElementDefinitionW);
    procedure processObject(indent: integer; name, path : String; sd : TFhirStructureDefinitionW; obj: TFHIRObject; inScope : TArray<String>);
    procedure processResource;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function languageName : String; override;
    function generate : String; override;
  end;

  TFHIRCodeGeneratorJavaHapi = class (TFHIRCodeGenerator)
  public
    function languageName : String; override;
  end;

  TFHIRCodeGeneratorPascal = class (TFHIRCodeGenerator)
  private
    units : TStringList;

    function varName(fhirType: String; defn : TFhirElementDefinitionW; inScope: TArray<String>): string;
    procedure processXhtml(indent : integer; variableName: String; value : TFHIRObject);
    procedure processAssignment(indent : integer; variableName: String; varIsSelf : boolean; sd : TFhirStructureDefinitionW; path : String; prop : TFHIRProperty; value : TFHIRObject; inScope : TArray<String>; defn : TFhirElementDefinitionW);
    procedure processObject(indent: integer; name, path : String; sd : TFhirStructureDefinitionW; obj: TFHIRObject; inScope : TArray<String>);
    procedure processResource;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function languageName : String; override;
    function generate : String; override;

    procedure test;
  end;

  TFHIRCodeGeneratorDotNet = class (TFHIRCodeGenerator)
  public
    function languageName : String; override;
  end;

function makeCodeGenerator(lang : String) : TFHIRCodeGenerator;

implementation

function makeCodeGenerator(lang : String) : TFHIRCodeGenerator;
begin
  if (StringArrayExistsSensitive(['java', 'java-ri'], lang)) then
    result := TFHIRCodeGeneratorJavaRI.Create
  else if (StringArrayExistsSensitive(['pascal', 'delphi'], lang)) then
    result := TFHIRCodeGeneratorPascal.Create
  else
    raise EFHIRException.create('Unknown code generation language '+lang);
end;

const
  typeNames : array of String = ['boolean', 'integer', 'string', 'decimal', 'uri', 'base64Binary', 'instant', 'date', 'dateTime', 'time', 'code', 'oid', 'id', 'markdown', 'unsignedInt', 'positiveInt'];
  varNames :  array of String = ['b', 'i', 's', 'd', 'u', 'b64', 'inst', 'd', 'dt', 't', 'c', 's', 's', 's', 'i', 'i'];

function capitalize(s : String): String;
begin
  if s = '' then
    result := s
  else
    result := Uppercase(s[1]) + s.Substring(1);
end;

function isXhtml(defn : TFhirElementDefinitionW) : boolean;
begin
  result := defn.types = 'xhtml';
end;

function delphiIse(s : String) : String;
begin
  if (StringArrayExistsInsensitive(['and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const', 'constructor', 'create', 'destructor', 'dispinterface', 'div', 'do', 'downto', 'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally', 'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited', 'initialization', 'inline', 'interface', 'is', 'label', 'library', 'link', 'mod', 'nil', 'not', 'object', 'of', 'or', 'out',
    'packed', 'procedure', 'program', 'property', 'raise', 'record', 'repeat', 'resourcestring', 'set', 'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until', 'uses', 'var', 'while', 'with', 'xor'], s)) then
    result := s + '_'
  else
    result := s;
end;

function javaise(s : String) : String;
begin
  if (StringArrayExistsInsensitive(['abstract', 'assert', 'boolean', 'break', 'byte', 'case', 'catch', 'char', 'class', 'const', 'continue', 'default', 'double', 'do', 'else', 'enum', 'extends', 'false', 'final', 'finally', 'float', 'for', 'goto', 'if', 'implements', 'import', 'instanceof', 'int', 'interface', 'long', 'native', 'new', 'null', 'package', 'private', 'protected', 'public', 'return', 'short', 'static', 'strictfp', 'super',
    'switch', 'synchronized', 'this', 'throw', 'throws', 'transient', 'true', 'try', 'void', 'volatile', 'while', 'Exception'], s)) then
    result := s + '_'
  else
    result := s;
end;

function isAbstractType(code : string) : boolean;
begin
  result := StringArrayExistsSensitive(['Element', 'BackboneElement', 'Resource', 'DomainResource'], code);
end;


{ TFHIRCodeGenerator }

constructor TFHIRCodeGenerator.Create;
begin
  inherited;
  lines := TStringList.create;
  vars := TStringList.Create;
  vars.NameValueSeparator := ':';
end;

destructor TFHIRCodeGenerator.Destroy;
begin
  FResource.free;
  FContext.Free;
  lines.Free;
  vars.Free;
  inherited;
end;

function TFHIRCodeGenerator.addVar(inScope: TArray<String>; varName: String): TArray<String>;
var
  i : integer;
begin
  setLength(result, length(inscope)+1);
  for I := 0 to length(inScope) - 1 do
    result[i] := inscope[i];
  result[length(inscope)] := varName;
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

function TFHIRCodeGenerator.getElementDefinition(sd: TFhirStructureDefinitionW; path: String): TFhirElementDefinitionW;
var
  l : TFslList<TFhirElementDefinitionW>;
  t : TFhirElementDefinitionW;
begin
  result := nil;
  l := sd.elements;
  try
    for t in l do
      if t.path = path then
        exit(t.link);
  finally
    l.free;
  end;
end;

function TFHIRCodeGenerator.languageName: String;
begin
  result := 'Unknown';
end;

procedure TFHIRCodeGenerator.SetContext(const Value: TFHIRWorkerContextWithFactory);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TFHIRCodeGenerator.SetResource(const Value: TFHIRResourceV);
begin
  FResource.free;
  FResource := Value;
end;

procedure TFHIRCodeGenerator.line(indent : integer; s: String);
begin
  lines.Add(StringPadLeft('', ' ', indent)+ s);
end;

function TFHIRCodeGenerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FResource.sizeInBytes);
  inc(result, FContext.sizeInBytes);
  inc(result, lines.sizeInBytes);
  inc(result, vars.sizeInBytes);
end;

{ TFHIRCodeGeneratorJavaRI }

constructor TFHIRCodeGeneratorJavaRI.Create;
begin
  inherited;
  imports := TStringList.create;
  imports.Duplicates := dupIgnore;
end;

destructor TFHIRCodeGeneratorJavaRI.Destroy;
begin
  imports.free;
  inherited;
end;

function TFHIRCodeGeneratorJavaRI.generate: String;
var
  b : TStringBuilder;
  s : String;
begin
  processResource;
  b := TStringBuilder.Create;
  try
    imports.Sort;
    for s in imports do
    begin
      b.Append('//import ');
      b.Append(s);
      b.Append(';');
      b.AppendLine;
    end;
    b.AppendLine;
    b.AppendLine;
    for s in lines do
    begin
      b.Append(s);
      b.AppendLine;
    end;
    b.AppendLine;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFHIRCodeGeneratorJavaRI.languageName: String;
begin
  result := 'Java (for reference implementation)';
end;

procedure TFHIRCodeGeneratorJavaRI.processResource;
var
  sd : TFhirStructureDefinitionW;
begin
  imports.Add('org.hl7.fhir.'+verPack+'.model.Enumerations.*');
  imports.Add('org.hl7.fhir.'+verPack+'.model.'+resource.fhirType);
  imports.Add('org.hl7.fhir.'+verPack+'.model.'+resource.fhirType+'.*');
  vars.Values['res'] := resource.fhirType;
  line(4, resource.fhirType + ' res = new '+resource.fhirType+'();');
  sd := FContext.Factory.wrapStructureDefinition(FContext.fetchResource('StructureDefinition', 'http://hl7.org/fhir/StructureDefinition/'+resource.fhirType));
  try
    processObject(4, 'res', resource.fhirType, sd, resource, addVar([], ' res'));
  finally
    sd.free;
  end;
end;

procedure TFHIRCodeGeneratorJavaRI.processObject(indent: integer; name, path : String; sd : TFhirStructureDefinitionW; obj: TFHIRObject; inScope : TArray<String>);
var
  props : TFHIRPropertyList;
  prop : TFHIRProperty;
  value : TFHIRObject;
  vn, vt, vdef : String;
  defn : TFhirElementDefinitionW;
  first : boolean;
begin
  props := obj.createPropertyList(true);
  try
    for prop in props do
      if prop.hasValue then
      begin
        defn := getElementDefinition(sd, path+'.'+prop.Name);
        try
          if prop.IsList then
          begin
            if (defn <> nil) and (length(defn.typeList) = 0) then
              vn := varName(path+'.'+prop.Name, sd, defn, inScope, vt, vdef)
            else
              vn := varName(prop.Values[0].fhirType, sd, defn, inScope, vt, vdef);
            first := true;
            for value in prop.Values do
            begin
              if first then
                line(indent, vdef+vn +' = '+name+'.add'+capitalize(prop.Name)+'();')
              else
                line(indent, vn +' = '+name+'.add'+capitalize(prop.Name)+'();');
              first := false;
              processAssignment(indent, vn, true, sd, path, prop, value, addVar(inScope, vn), defn);
            end;
          end
          else if isXhtml(defn) then
            processXhtml(indent, name, prop.values[0])
          else
            processAssignment(indent, name, false, sd, path, prop, prop.Values[0], inScope, defn);
        finally
          defn.free;
        end;
      end;
  finally
    props.Free;
  end;
end;

procedure TFHIRCodeGeneratorJavaRI.processAssignment(indent: integer; variableName : String; varIsSelf : boolean; sd : TFhirStructureDefinitionW; path : String; prop: TFHIRProperty; value : TFHIRObject; inScope : TArray<String>; defn : TFhirElementDefinitionW);
var
  t : String;
  vs : TFhirValueSetW;
  tsd : TFhirStructureDefinitionW;
  vn, an, vt, vdef : string;
begin
  t := value.fhirType; // prop.Type_;
  if varIsSelf then
    an := variableName+'.setValue'
  else
    an := variableName+'.set'+capitalize(javaise(prop.Name.Replace('[x]', ''))+'');

  if StringArrayExistsSensitive(['string', 'id', 'markdown', 'uri', 'base64Binary', 'oid', 'id'], t) then
    line(indent, an+'("'+value.primitiveValue+'");')
  else if t = 'code' then
  begin
    vs := nil;
    if (defn <> nil) and (defn.binding = edbRequired) and (defn.valueSet <> '') then
      vs := FContext.Factory.wrapValueSet(FContext.fetchResource('ValueSet', defn.valueSet));
    try
      if (vs <> nil) then
        line(indent, an+'('+vs.name+'.'+enumify(value.primitiveValue).ToUpper+');')
      else
        line(indent, an+'("'+value.primitiveValue+'");')
    finally
      vs.Free;
    end;
  end
  else if StringArrayExistsSensitive(['integer', 'decimal', 'unsignedInt', 'positiveInt', 'boolean'], t) then
    line(indent, an+'('+value.primitiveValue+');')
  else if StringArrayExistsSensitive(['date', 'dateTime', 'instant'], t) then
  begin
    imports.add('org.hl7.fhir.'+verPack+'.model.DateTimeType');
    if (prop.Type_.Contains('|')) then
      line(indent, an+'(new DateTimeType("'+value.primitiveValue+'"));')
    else
      line(indent, an+'Element(new DateTimeType("'+value.primitiveValue+'"));')
  end
  // we assume this is an object
  else
  begin
    tsd := FContext.Factory.wrapStructureDefinition(FContext.fetchResource('StructureDefinition', 'http://hl7.org/fhir/StructureDefinition/'+value.fhirType));
    if (tsd <> nil) then
    begin
      try
        if varIsSelf then
          processObject(indent+2, variableName, tsd.type_, tsd, value, inScope)
        else
        begin
          vn := varName(value.fhirType, sd, defn, inScope, vt, vdef);
          line(indent, vdef+vn +' = new '+vt+'();');
          line(indent, '  '+variableName+'.set'+capitalize(prop.Name.Replace('[x]', ''))+'('+vn+');');
          processObject(indent+2, vn, tsd.type_, tsd, value, addVar(inScope, vn));
        end;
      finally
        tsd.Free;
      end;
    end
    else if varIsSelf then
      processObject(indent+2, variableName, path+'.'+prop.Name, sd, value, inScope)
    else
    begin
      vn := varName(path+'.'+prop.Name, sd, defn, inScope, vt, vdef);
      line(indent, vdef+vn +' = new '+vt+'();');
      line(indent, '  '+variableName+'.set'+capitalize(prop.Name.Replace('[x]', ''))+'('+vn+');');
      processObject(indent+2, vn, path+'.'+prop.Name, sd, value, addVar(inScope, vn));
    end;
  end;
end;

procedure TFHIRCodeGeneratorJavaRI.processXhtml(indent: integer; variableName: String; value: TFHIRObject);
var
  s : String;
begin
  imports.add('org.hl7.fhir.utilities.xhtml.XhtmlParser');
  s := TFHIRXhtmlParser.compose(value as TFhirXHtmlNode);
  s := '"'+s.Replace('"', '\"')+'"';
  line(indent, variableName+'.setDiv(new XhtmlParser().parse('+s+', "div"));');
end;

function TFHIRCodeGeneratorJavaRI.varName(fhirType: String; sd : TFhirStructureDefinitionW; defn : TFhirElementDefinitionW; inScope: TArray<String>; var vt : String; var def : String): string;
  function isOkTouse(name : string) : boolean;
  begin
    if StringArrayIndexOfSensitive(inScope, name) > -1 then
      result := false
    else if vars.Values[name] = '' then
      result := true
    else
      result := vars.Values[name] = vt;
  end;
var
  ch : Char;
  i : integer;
  last : boolean;
  pfx : String;
  et : String;
begin
  // figure out the type
  pfx := '';
  if (length(defn.typeList) = 0) or ((length(defn.typeList) = 1) and isAbstractType(defn.typeList[0])) then
  begin
    pfx := sd.type_+'.';
    et := defn.explicitTypeName;
    if (et <> '') then
      vt := et
    else
      vt := capitalize(defn.path.Substring(defn.path.LastIndexOf('.')+1));
    vt := vt+'Component';
  end
  else if (StringArrayExistsSensitive(['date', 'dateTime', 'instant'], fhirType)) then
    vt := 'DateTimeType'
  else
    vt := capitalize(fhirType);

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
  if not isOkToUse(result) then
  begin
    i := 1;
    while not isOkToUse(result+inttostr(i))do
      inc(i);
    result := result + inttostr(i);
  end;
  imports.Add('org.hl7.fhir.'+verPack+'.model.'+pfx+vt);
  imports.Add('org.hl7.fhir.'+verPack+'.model.'+pfx+vt+'.*');
  if vars.Values[result] = '' then
  begin
    def := vt+' ';
    vars.Values[result] := vt;
  end
  else
    def := '';
end;


function TFHIRCodeGeneratorJavaRI.verPack: String;
begin
  case FContext.Factory.version of
    fhirVersionRelease1: result := 'dstu1';
    fhirVersionRelease2: result := 'dstu2';
    fhirVersionRelease3: result := 'dstu3';
    fhirVersionRelease4: result := 'r4';
  end;
end;

function TFHIRCodeGeneratorJavaRI.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, imports.sizeInBytes);
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
  units.duplicates := dupIgnore;
end;

destructor TFHIRCodeGeneratorPascal.Destroy;
begin
  units.Free;
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

procedure TFHIRCodeGeneratorPascal.processAssignment(indent: integer; variableName : String; varIsSelf : boolean; sd : TFhirStructureDefinitionW; path : String; prop: TFHIRProperty; value : TFHIRObject; inScope : TArray<String>; defn : TFhirElementDefinitionW);
var
  t : String;
  vs : TFhirValueSetW;
  tsd : TFhirStructureDefinitionW;
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
    if (defn <> nil) and (defn.binding = edbRequired) and (defn.valueSet <> '') then
      vs :=  FContext.Factory.wrapValueSet(FContext.fetchResource('ValueSet', defn.valueSet));
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
    units.add('fsl_utilities');
    line(indent, an+' := TFslDateTime.fromXML('''+value.primitiveValue+''');')
  end
  // we assume this is an object
  else
  begin
    tsd := FContext.Factory.wrapStructureDefinition(FContext.fetchResource('StructureDefinition', 'http://hl7.org/fhir/StructureDefinition/'+value.fhirType));
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

procedure TFHIRCodeGeneratorPascal.processObject(indent: integer; name, path : String; sd : TFhirStructureDefinitionW; obj: TFHIRObject; inScope : TArray<String>);
var
  props : TFHIRPropertyList;
  prop : TFHIRProperty;
  value : TFHIRObject;
  vn : String;
  defn : TFhirElementDefinitionW;
begin
  props := obj.createPropertyList(true);
  try
    for prop in props do
      if prop.hasValue then
      begin
        defn := getElementDefinition(sd, path+'.'+prop.Name);
        try
          if prop.IsList then
          begin
            for value in prop.Values do
            begin
              if (defn <> nil) and (length(defn.typeList) = 0) then
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
        finally
          defn.free;
        end;
      end;
  finally
    props.Free;
  end;
end;

procedure TFHIRCodeGeneratorPascal.processResource;
var
  sd : TFhirStructureDefinitionW;
begin
  units.Add('FHIR.Version.Types');
  units.Add('FHIR.Version.Resources');
  units.Add('FHIR.Version.Utilities');
  vars.Values['res'] := 'TFHIR'+resource.fhirType;
  line(2, 'res := TFHIR'+resource.fhirType+'.create;');
  line(2, 'try');
  sd := FContext.Factory.wrapStructureDefinition(FContext.fetchResource('StructureDefinition', 'http://hl7.org/fhir/StructureDefinition/'+resource.fhirType));
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
  units.add('fhir_xhtml');
  line(indent, variableName+'.div_ := TFHIRXhtmlParser.parse(THTTPLanguages.create(''en''), xppReject, [], '+delphiStringWrap(indent, TFHIRXhtmlParser.compose(value as TFhirXHtmlNode))+'); // (lang, policy, options, html)');
end;

function TFHIRCodeGeneratorPascal.varName(fhirType: String; defn : TFhirElementDefinitionW; inScope: TArray<String>): string;
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
    if (length(defn.typeList) = 0) or ((length(defn.typeList) = 1) and isAbstractType(defn.typeList[0])) then
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
begin
end;

function TFHIRCodeGeneratorPascal.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, units.sizeInBytes);
end;

{ TFHIRCodeGeneratorDotNet }

function TFHIRCodeGeneratorDotNet.languageName: String;
begin
  result := 'DotNet';
end;

end.
