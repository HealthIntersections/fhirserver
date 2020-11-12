unit fhir3_parser;

{
  Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
  SysUtils, Classes,
  fhir_objects, fhir_parser, fhir_xhtml,  fsl_http,
  fhir3_types, fhir3_resources, fhir3_xml, fhir3_json, fhir3_turtle, fhir3_context;

type
  TFHIRParsers3 = class
  public
    class function parser(worker : TFHIRWorkerContext; format : TFHIRFormat; const lang : THTTPLanguages) : TFHIRParser;
    class function composer(worker : TFHIRWorkerContext; format : TFHIRFormat; const lang : THTTPLanguages; style: TFHIROutputStyle) : TFHIRComposer;
    class function ParseFile(worker : TFHIRWorkerContext; format : TFHIRFormat; const lang : THTTPLanguages; filename : String) : TFHIRResource; overload;
    class procedure composeFile(worker : TFHIRWorkerContext; format : TFHIRFormat; r : TFHIRResourceV; const lang : THTTPLanguages; filename : String; style : TFHIROutputStyle); overload;
  end;

implementation

{ TFHIRParsers3 }

class function TFHIRParsers3.composer(worker: TFHIRWorkerContext; format: TFHIRFormat; const lang : THTTPLanguages; style: TFHIROutputStyle): TFHIRComposer;
begin
  case format of
    ffXml : result := fhir3_xml.TFHIRXmlComposer.Create(worker, style, lang);
    ffJson : result := fhir3_json.TFHIRJsonComposer.Create(worker, style, lang);
    ffTurtle : result := fhir3_turtle.TFHIRTurtleComposer.Create(worker, style, lang);
    ffText : result := TFHIRTextComposer.Create(worker, style, lang);
//    ffNDJson : result := TFHIRNDJsonComposer.Create(worker, style, lang);
//    ffXhtml  : result := TFHIRXhtmlComposer.Create(worker, style, lang);
  else
    raise EFHIRException.create('Unspecified/unsupported format');
  end;
end;

class function TFHIRParsers3.parser(worker: TFHIRWorkerContext; format: TFHIRFormat; const lang : THTTPLanguages): TFHIRParser;
begin
  case format of
    ffXml: result := fhir3_xml.TFHIRXmlParser.Create(worker, lang);
    ffJson: result := fhir3_json.TFHIRJsonParser.Create(worker, lang);
    ffTurtle: result := fhir3_turtle.TFHIRTurtleParser.Create(worker, lang);
  else
    raise EFHIRException.create('Unspecified/unsupported format '+CODES_TFHIRFormat[format]);
  end;
end;

class procedure TFHIRParsers3.composeFile(worker: TFHIRWorkerContext; format: TFHIRFormat; r: TFHIRResourceV; const lang : THTTPLanguages; filename: String; style: TFHIROutputStyle);
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

class function TFHIRParsers3.ParseFile(worker: TFHIRWorkerContext; format: TFHIRFormat; const lang : THTTPLanguages; filename: String): TFHIRResource;
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
