unit FHIR.R3.Parser;

interface

uses
  SysUtils, Classes,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.Base.Xhtml,
  FHIR.R3.Types, FHIR.R3.Resources, FHIR.R3.Xml, FHIR.R3.Json, FHIR.R3.Turtle, FHIR.R3.Context;

type
  TFHIRParsers3 = class
  public
    class function parser(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String) : TFHIRParser;
    class function composer(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String; style: TFHIROutputStyle) : TFHIRComposer;
    class function ParseFile(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String; filename : String) : TFHIRResource; overload;
    class procedure composeFile(worker : TFHIRWorkerContext; format : TFHIRFormat; r : TFHIRResourceV; lang : String; filename : String; style : TFHIROutputStyle); overload;
  end;

implementation

{ TFHIRParsers3 }

class function TFHIRParsers3.composer(worker: TFHIRWorkerContext; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  case format of
    ffXml : result := FHIR.R3.Xml.TFHIRXmlComposer.Create(worker, style, lang);
    ffJson : result := FHIR.R3.Json.TFHIRJsonComposer.Create(worker, style, lang);
    ffTurtle : result := FHIR.R3.Turtle.TFHIRTurtleComposer.Create(worker, style, lang);
    ffText : result := TFHIRTextComposer.Create(worker, style, lang);
//    ffNDJson : result := TFHIRNDJsonComposer.Create(worker, style, lang);
//    ffXhtml  : result := TFHIRXhtmlComposer.Create(worker, style, lang);
  else
    raise Exception.Create('Unspecified/unsupported format');
  end;
end;

class function TFHIRParsers3.parser(worker: TFHIRWorkerContext; format: TFHIRFormat; lang: String): TFHIRParser;
begin
  case format of
    ffXml: result := FHIR.R3.Xml.TFHIRXmlParser.Create(worker, lang);
    ffJson: result := FHIR.R3.Json.TFHIRJsonParser.Create(worker, lang);
    ffTurtle: result := FHIR.R3.Turtle.TFHIRTurtleParser.Create(worker, lang);
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
