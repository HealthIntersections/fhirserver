unit FHIR.Tools.ExpressionComposer;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Binary,
  FHIR.Support.Objects, FHIR.Support.Generics, FHIR.Support.Json, FHIR.Support.Stream, FHIR.Support.Xml,
  FHIR.Base.Objects, FHIR.Base.Parser,
  FHIR.Tools.PathNode;

type
  TFHIRExpressionNodeComposer = class (TAdvObject)
  private
    FStyle : TFHIROutputStyle;
    FWorker : TFHIRWorkerContextV;
    FLang : String;
    procedure ComposeXml(stream : TStream; expr : TFHIRPathExpressionNode; items : TFHIRObjectList; types : TAdvStringSet);
    procedure composeXmlExpression(xml: TXmlBuilder; expr: TFHIRPathExpressionNode);
    procedure ComposeJson(stream : TStream; expr : TFHIRPathExpressionNode; items : TFHIRObjectList; types : TAdvStringSet);
    procedure ComposeJsonExpression(json: TJSONWriter; expr : TFHIRPathExpressionNode); reintroduce; overload; virtual;
  public
    Constructor Create(worker : TFHIRWorkerContextV; style : TFHIROutputStyle; lang : String); Virtual;

    procedure ComposeExpression(stream : TStream; expr : TFHIRPathExpressionNode; fmt : TFHIRFormat; items : TFHIRObjectList; types : TAdvStringSet); Virtual;
    function Compose(expr : TFHIRPathExpressionNode; fmt : TFHIRFormat; items : TFHIRObjectList; types : TAdvStringSet): String; Overload;
  end;

implementation

uses
  FHIR.Tools.Parser;

constructor TFHIRExpressionNodeComposer.Create(worker: TFHIRWorkerContextV; style: TFHIROutputStyle; lang: String);
begin
  inherited Create;
  FWorker := worker;
  FLang := lang;
  FStyle := Style;
end;

function TFHIRExpressionNodeComposer.Compose(expr : TFHIRPathExpressionNode; fmt : TFHIRFormat; items: TFHIRObjectList; types : TAdvStringSet): String;
var
  stream : TBytesStream;
begin
  stream := TBytesStream.create;
  try
    composeExpression(stream, expr, fmt, items, types);
    result := TEncoding.UTF8.GetString(copy(stream.Bytes, 0, stream.position));
  finally
    stream.Free;
  end;
end;

procedure TFHIRExpressionNodeComposer.ComposeExpression(stream: TStream; expr: TFHIRPathExpressionNode; fmt: TFHIRFormat; items: TFHIRObjectList; types: TAdvStringSet);
begin
  case fmt of
    ffXml : ComposeXml(stream, expr, items, types);
    ffJson: ComposeJson(stream, expr, items, types);
  else
    raise Exception.Create('ComposeExpression is Not supported for '+CODES_TFHIRFormat[fmt]);
  end;
end;

procedure TFHIRExpressionNodeComposer.composeXml(stream: TStream; expr : TFHIRPathExpressionNode; items: TFHIRObjectList; types : TAdvStringSet);
var
  xml : TXmlBuilder;
  base : TFHIRObject;
  x : TFHIRXmlComposer;
begin
  x := TFHIRXmlComposer.Create(FWorker, FStyle, FLang);
  try
    xml := TAdvXmlBuilder.Create;
    try
      xml.IsPretty := FStyle = OutputStylePretty;
      xml.NoHeader := true;
      xml.CurrentNamespaces.DefaultNS := FHIR_NS;
      xml.Start;
      xml.Open('Expression');
      if items <> nil then
        xml.addattribute('count', inttostr(items.count))
      else
        xml.addattribute('count', 'nil');
      xml.Open('outcome');
      if items <> nil then
        for base in items do
          if (base = nil) then
            xml.tag('Null')
          else
            x.ComposeBase(xml, base.FhirType, base);
      xml.Close('outcome');
      xml.TagText('canonical', expr.Canonical);
      xml.Open('tree');
      composeXmlExpression(xml, expr);
      xml.Close('tree');
      if (types <> nil) then
      begin
        xml.AddAttribute('value', types.ToString);
        xml.Tag('types');
      end;
      xml.Close('Expression');
      xml.Finish;
      xml.Build(stream);
    finally
      xml.Free;
    end;
  finally
    x.Free;
  end;
end;

procedure TFHIRExpressionNodeComposer.composeXmlExpression(xml: TXmlBuilder; expr: TFHIRPathExpressionNode);
var
  p : TFHIRPathExpressionNode;
begin
  if expr.Proximal then
  begin
    xml.AddAttribute('value', 'true');
    xml.Tag('proximal');
  end;

  case expr.kind of
    enkName :
      begin
        xml.AddAttribute('value', expr.name);
        xml.Tag('name');
      end;
    enkFunction :
      begin
        xml.AddAttribute('value', CODES_TFHIRPathFunctions[expr.FunctionId]);
        xml.Tag('function');
        for p in expr.Parameters do
        begin
          xml.open('parameter');
          composeXmlExpression(xml, p);
          xml.close('parameter');
        end;
      end;
    enkConstant :
      begin
        xml.AddAttribute('value', expr.presentConstant);
        xml.Tag('constant');
      end;
    enkGroup :
      begin
        xml.Open('group');
        composeXmlExpression(xml, expr.Group);
        xml.Close('group');
      end;
  end;
  if expr.Types <> nil then
  begin
    xml.AddAttribute('value', expr.types.ToString);
    xml.Tag('types');
  end;
  if expr.Inner <> nil then
  begin
    xml.open('inner');
    composeXmlExpression(xml, expr.Inner);
    xml.close('inner');
  end;
  if expr.Operation <> popNull then
  begin
    xml.AddAttribute('kind', CODES_TFHIRPathOperation[expr.Operation]);
    xml.open('operation');
    composeXmlExpression(xml, expr.OpNext);
    xml.close('operation');
  end;
  if expr.OpTypes <> nil then
  begin
    xml.AddAttribute('value', expr.optypes.ToString);
    xml.Tag('op-types');
  end;
end;

procedure TFHIRExpressionNodeComposer.ComposeJson(stream: TStream; expr : TFHIRPathExpressionNode; items: TFHIRObjectList; types : TAdvStringSet);
var
  oStream : TAdvVCLStream;
  json : TJSONWriter;
  base : TFHIRObject;
  j : TFHIRJsonComposer;
begin
  j := TFHIRJsonComposer.Create(FWorker, FStyle, FLang);
  try
    json := TJsonWriterDirect.create;
    try
      oStream := TAdvVCLStream.Create;
      json.Stream := oStream;
      oStream.Stream := stream;
      json.HasWhitespace := FStyle = OutputStylePretty;
      json.Start;
      json.ValueArray('outcome');
      for base in items do
        j.ComposeBase(json, '', base);
      json.FinishArray;
      json.Value('canonical', expr.Canonical);
      json.ValueObject('tree');
      composeJsonExpression(json, expr);
      json.FinishObject;
      if (types <> nil) then
        json.Value('types', types.ToString);
      json.Finish;
    finally
      json.free;
    end;
  finally
    j.Free;
  end;
end;

procedure TFHIRExpressionNodeComposer.ComposeJsonExpression(json: TJSONWriter; expr: TFHIRPathExpressionNode);
var
  p : TFHIRPathExpressionNode;
begin
  if expr.Proximal then
    json.value('proximal', true);

  case expr.kind of
    enkName: json.value('name', expr.name);
    enkFunction:
      begin
        json.value('function', CODES_TFHIRPathFunctions[expr.FunctionId]);
        json.ValueArray('parameters');
        for p in expr.Parameters do
        begin
          json.ValueObject('');
          ComposeJsonExpression(json, p);
          json.FinishObject;
        end;
        json.FinishArray();
      end;
    enkConstant: json.value('constant', expr.presentConstant);
    enkGroup:
      begin
      json.valueObject('group');
      ComposeJsonExpression(json, expr.Group);
      json.FinishObject;
      end;
  end;
  if expr.Types <> nil then
    json.value('types', expr.types.ToString);
  if expr.Inner <> nil then
  begin
    json.ValueObject('inner');
    ComposeJsonExpression(json, expr.Inner);
    json.FinishObject;
  end;
  if expr.Operation <> popNull then
  begin
    json.ValueObject('operation');
    json.value('kind', CODES_TFHIRPathOperation[expr.Operation]);
    ComposeJsonExpression(json, expr.OpNext);
    json.FinishObject;
  end;
  if expr.OpTypes <> nil then
    json.value('op-types', expr.optypes.ToString);
end;


end.

