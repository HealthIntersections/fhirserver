unit FHIRParser3;

interface

uses
  SysUtils, Classes,
  FHIRBase, FHIRParserBase, FHIRXhtml, FHIRXhtmlComposer,
  FHIRTypes3, FHIRResources3, FHIRParserXml3, FHIRParserJson3, FHIRParserTurtle3, FHIRContext3;

type
  TFHIRParsers3 = class
  public
    class function parser(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String) : TFHIRParser;
    class function composer(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String; style: TFHIROutputStyle) : TFHIRComposer;
    class function ParseFile(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String; filename : String) : TFHIRResource; overload;
    class procedure composeFile(worker : TFHIRWorkerContext; format : TFHIRFormat; r : TFHIRResourceV; lang : String; filename : String; style : TFHIROutputStyle); overload;
  end;

implementation

uses
  FHIRParser;

{ TFHIRParsers3 }

class function TFHIRParsers3.composer(worker: TFHIRWorkerContext; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  case format of
    ffXml : result := FHIRParserXml3.TFHIRXmlComposer.Create(worker, style, lang);
    ffJson : result := FHIRParserJson3.TFHIRJsonComposer.Create(worker, style, lang);
    ffTurtle : result := FHIRParserTurtle3.TFHIRTurtleComposer.Create(worker, style, lang);
    ffText : result := TFHIRTextComposer.Create(worker, style, lang);
    ffNDJson : result := TFHIRNDJsonComposer.Create(worker, style, lang);
    ffXhtml  : result := TFHIRXhtmlComposer.Create(worker, style, lang);
  else
    raise Exception.Create('Unspecified/unsupported format');
  end;
end;

class function TFHIRParsers3.parser(worker: TFHIRWorkerContext; format: TFHIRFormat; lang: String): TFHIRParser;
begin
  case format of
    ffXml: result := FHIRParserXml3.TFHIRXmlParser.Create(worker, lang);
    ffJson: result := FHIRParserJson3.TFHIRJsonParser.Create(worker, lang);
    ffTurtle: result := FHIRParserTurtle3.TFHIRTurtleParser.Create(worker, lang);
  else
    raise Exception.Create('Unspecified/unsupported format');
  end;
end;

class procedure TFHIRParsers3.composeFile(worker: TFHIRWorkerContext; format: TFHIRFormat; r: TFHIRResourceV; lang, filename: String; style: TFHIROutputStyle);
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

class function TFHIRParsers3.ParseFile(worker: TFHIRWorkerContext; format: TFHIRFormat; lang, filename: String): TFHIRResource;
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
