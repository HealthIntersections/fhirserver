unit fhir4b_parser;

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
{$I fhir4b.inc}

interface

uses
  SysUtils, Classes,
  fsl_http,
  fhir_objects, fhir_parser, fhir_xhtml, 
  fhir4b_types, fhir4b_resources, fhir4b_xml,fhir4b_json, fhir4b_turtle, fhir4b_context;

type
  TFHIRParsers4B = class
  public
    class function parser(worker : TFHIRWorkerContext; format : TFHIRFormat; langList : THTTPLanguageList) : TFHIRParser;
    class function composer(worker : TFHIRWorkerContext; format : TFHIRFormat; langList : THTTPLanguageList; style: TFHIROutputStyle) : TFHIRComposer;
    class function ParseFile(worker : TFHIRWorkerContext; format : TFHIRFormat; langList : THTTPLanguageList; filename : String) : TFHIRResource; overload;
    class procedure composeFile(worker : TFHIRWorkerContext; format : TFHIRFormat; r : TFHIRResourceV; langList : THTTPLanguageList; filename : String; style : TFHIROutputStyle); overload;
  end;

implementation

{ TFHIRParsers4B }

class function TFHIRParsers4B.composer(worker: TFHIRWorkerContext; format: TFHIRFormat; langList : THTTPLanguageList; style: TFHIROutputStyle): TFHIRComposer;
begin
  case format of
    ffXml : result := fhir4b_xml.TFHIRXmlComposer.Create(worker, style, langList);
    ffJson : result := fhir4b_json.TFHIRJsonComposer.Create(worker, style, langList);
    ffTurtle : result := fhir4b_turtle.TFHIRTurtleComposer.Create(worker, style, langList);
    ffText : result := TFHIRTextComposer.Create(worker, style, langList);
  else
    raise EFHIRException.Create('Unspecified/unsupported format');
  end;
end;

class function TFHIRParsers4B.parser(worker: TFHIRWorkerContext; format: TFHIRFormat; langList : THTTPLanguageList): TFHIRParser;
begin
  case format of
    ffXml: result := fhir4b_xml.TFHIRXmlParser.Create(worker, langList);
    ffJson: result := fhir4b_json.TFHIRJsonParser.Create(worker, langList);
    ffTurtle: result := fhir4b_turtle.TFHIRTurtleParser.Create(worker, langList);
  else
    raise EFHIRException.Create('Unspecified/unsupported format');
  end;
end;

class procedure TFHIRParsers4B.composeFile(worker: TFHIRWorkerContext; format: TFHIRFormat; r: TFHIRResourceV; langList : THTTPLanguageList; filename: String; style: TFHIROutputStyle);
var
  c : TFHIRComposer;
  f : TFileStream;
begin
  c := composer(worker, format, langList.link, style);
  try
    f := TFileStream.Create(filename, fmCreate);
    try
      c.Compose(f, r);
    finally
      f.free;
    end;
  finally
    c.free;
  end;
end;

class function TFHIRParsers4B.ParseFile(worker: TFHIRWorkerContext; format: TFHIRFormat; langList : THTTPLanguageList; filename: String): TFHIRResource;
var
  p : TFHIRParser;
begin
  p := parser(worker, format, langList.link);
  try
    p.ParseFile(filename);
    result := p.resource.Link as TFhirResource;
  finally
    p.free;
  end;
end;

end.

