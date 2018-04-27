unit FHIR.R4.Parser;

interface

uses
  SysUtils, Classes,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.Base.Xhtml, FHIR.Tools.XhtmlComp,
  FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.Xml, FHIR.R4.Json, FHIR.R4.Turtle, FHIR.R4.Context;

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
  FHIR.Tools.Parser;

{ TFHIRParsers4 }

class function TFHIRParsers4.composer(worker: TFHIRWorkerContext; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  case format of
    ffXml : result := FHIR.R4.Xml.TFHIRXmlComposer.Create(worker, style, lang);
    ffJson : result := FHIR.R4.Json.TFHIRJsonComposer.Create(worker, style, lang);
    ffTurtle : result := FHIR.R4.Turtle.TFHIRTurtleComposer.Create(worker, style, lang);
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
    ffXml: result := FHIR.R4.Xml.TFHIRXmlParser.Create(worker, lang);
    ffJson: result := FHIR.R4.Json.TFHIRJsonParser.Create(worker, lang);
    ffTurtle: result := FHIR.R4.Turtle.TFHIRTurtleParser.Create(worker, lang);
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
