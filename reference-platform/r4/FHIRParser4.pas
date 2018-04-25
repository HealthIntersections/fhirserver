unit FHIRParser4;

interface

uses
  SysUtils, Classes,
  FHIRBase, FHIRParserBase, FHIRXhtml, FHIRXhtmlComposer,
  FHIRTypes4, FHIRResources4, FHIRParserXml4, FHIRParserJson4, FHIRParserTurtle4, FHIRContext4;

type
  TFHIRParsers4 = class
  public
    class function parser(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String) : TFHIRParser;
    class function composer(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String; style: TFHIROutputStyle) : TFHIRComposer;
    class function ParseFile(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String; filename : String) : TFHIRResource; overload;
    class procedure composeFile(worker : TFHIRWorkerContext; format : TFHIRFormat; r : TFHIRResourceV; lang : String; filename : String; style : TFHIROutputStyle); overload;
  end;

implementation

uses
  FHIRParser;

{ TFHIRParsers4 }

class function TFHIRParsers4.composer(worker: TFHIRWorkerContext; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  case format of
    ffXml : result := FHIRParserXml4.TFHIRXmlComposer.Create(worker, style, lang);
    ffJson : result := FHIRParserJson4.TFHIRJsonComposer.Create(worker, style, lang);
    ffTurtle : result := FHIRParserTurtle4.TFHIRTurtleComposer.Create(worker, style, lang);
    ffText : result := TFHIRTextComposer.Create(worker, style, lang);
    ffNDJson : result := TFHIRNDJsonComposer.Create(worker, style, lang);
    ffXhtml  : result := TFHIRXhtmlComposer.Create(worker, style, lang);
  else
    raise Exception.Create('Unspecified/unsupported format');
  end;
end;

class function TFHIRParsers4.parser(worker: TFHIRWorkerContext; format: TFHIRFormat; lang: String): TFHIRParser;
begin
  case format of
    ffXml: result := FHIRParserXml4.TFHIRXmlParser.Create(worker, lang);
    ffJson: result := FHIRParserJson4.TFHIRJsonParser.Create(worker, lang);
    ffTurtle: result := FHIRParserTurtle4.TFHIRTurtleParser.Create(worker, lang);
  else
    raise Exception.Create('Unspecified/unsupported format');
  end;
end;

class procedure TFHIRParsers4.composeFile(worker: TFHIRWorkerContext; format: TFHIRFormat; r: TFHIRResourceV; lang, filename: String; style: TFHIROutputStyle);
var
  c : TFHIRComposer;
  f : TFileStream;
begin
  c := composer(worker, format, lang, style);
  try
    f := TFileStream.Create(filename, fmCreate);
    try
      c.Compose(f, r);
    finally
      f.Free;
    end;
  finally
    c.Free;
  end;
end;

class function TFHIRParsers4.ParseFile(worker: TFHIRWorkerContext; format: TFHIRFormat; lang, filename: String): TFHIRResource;
var
  p : TFHIRParser;
begin
  p := parser(worker, format, lang);
  try
    p.ParseFile(filename);
    result := p.resource.Link as TFhirResource;
  finally
    p.Free;
  end;
end;

end.
