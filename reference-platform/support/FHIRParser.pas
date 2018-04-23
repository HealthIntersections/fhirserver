unit FHIRParser;

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


interface

// FHIR v3.1.0 generated 2017-07-09T00:15:31+10:00

uses
  SysUtils, Classes,
  AdvObjects, AdvVclStreams, AdvBuffers,
  FHIRBase, FHIRParserBase,

  {$IFDEF FHIR2}
  FHIRTypes2, FHIRResources2, FHIRParserXml2, FHIRParserJson2, FHIRContext2;
  {$ENDIF}
  {$IFDEF FHIR3}
  FHIRTypes3, FHIRResources3, FHIRParserXml3, FHIRParserJson3, FHIRParserTurtle3, FHIRContext3;
  {$ENDIF}
  {$IFDEF FHIR4}
  FHIRTypes4, FHIRResources4, FHIRParserXml4, FHIRParserJson4, FHIRParserTurtle4, FHIRContext4;
  {$ENDIF}

Type
  TFHIRParser = FHIRParserBase.TFHIRParser;
  TFHIRComposer = FHIRParserBase.TFHIRComposer;

  {$IFDEF FHIR2}
  TFHIRXmlParser = FHIRParserXml2.TFHIRXmlParser;
  TFHIRXmlComposer = FHIRParserXml2.TFHIRXmlComposer;
  TFHIRJsonParser = FHIRParserJson2.TFHIRJsonParser;
  TFHIRJsonComposer = FHIRParserJson2.TFHIRJsonComposer;
  {$ENDIF}
  {$IFDEF FHIR3}
  TFHIRXmlParser = FHIRParserXml3.TFHIRXmlParser;
  TFHIRXmlComposer = FHIRParserXml3.TFHIRXmlComposer;
  TFHIRJsonParser = FHIRParserJson3.TFHIRJsonParser;
  TFHIRJsonComposer = FHIRParserJson3.TFHIRJsonComposer;
  TFHIRTurtleComposer = FHIRParserTurtle3.TFHIRTurtleComposer;
  TFHIRTurtleParser = FHIRParserTurtle3.TFHIRTurtleParser;
  {$ENDIF}
  {$IFDEF FHIR4}
  TFHIRXmlParser = FHIRParserXml4.TFHIRXmlParser;
  TFHIRXmlComposer = FHIRParserXml4.TFHIRXmlComposer;
  TFHIRJsonParser = FHIRParserJson4.TFHIRJsonParser;
  TFHIRJsonComposer = FHIRParserJson4.TFHIRJsonComposer;
  TFHIRTurtleComposer = FHIRParserTurtle4.TFHIRTurtleComposer;
  TFHIRTurtleParser = FHIRParserTurtle4.TFHIRTurtleParser;
  {$ENDIF}

  TFHIRParsers = class
  public
    class function parser(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String) : TFHIRParser;
    class function composer(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String; style: TFHIROutputStyle) : TFHIRComposer;
    class function ParseFile(worker : TFHIRWorkerContext; format : TFHIRFormat; lang : String; filename : String) : TFHIRResource; overload;
    class procedure composeFile(worker : TFHIRWorkerContext; format : TFHIRFormat; r : TFHIRResourceV; lang : String; filename : String; style : TFHIROutputStyle); overload;
  end;

  TFHIRNDJsonComposer = class (TFHIRComposer)
  public
    Procedure Compose(stream : TStream; oResource : TFhirResourceV); Override;
  end;


implementation

uses
  FHIRXhtmlComposer;

{ TFHIRNDJsonComposer }

procedure TFHIRNDJsonComposer.Compose(stream: TStream; oResource: TFhirResourceV);
var
  oStream : TAdvVCLStream;
  json : TFHIRJsonComposer;
  be : TFhirBundleEntry;
  first : boolean;
  ch : char;
  res : TFHIRResource;
begin
  res := oResource as TFHIRResource;
  ch := #10;
  if res.ResourceType = frtBundle then
  begin
    first := true;
    for be in TFHIRBundle(oResource).entryList do
    begin
      if first then
        first := false
      else
        stream.Write(ch, 1);
      if be.resource <> nil then
      begin
        json := TFHIRJsonComposer.Create(FWorker.link, OutputStyleNormal, lang);
        try
          json.Compose(stream, be.resource);
        finally
          json.Free;
        end;
      end
      else if be.tag is TAdvBuffer then
      begin
        TAdvBuffer(be.tag).SaveToStream(stream);
      end;
    end;
  end
  else
  begin
    json := TFHIRJsonComposer.Create(FWorker.link, OutputStyleNormal, lang);
    try
      json.Compose(stream, oResource);
    finally
      json.Free;
    end;
  end;
end;


{ TFHIRParsers }

class function TFHIRParsers.composer(worker: TFHIRWorkerContext; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  case format of
    ffXml : result := TFHIRXmlComposer.Create(worker, style, lang);
    ffJson : result := TFHIRJsonComposer.Create(worker, style, lang);
    {$IFNDEF FHIR2}
    ffTurtle : result := TFHIRTurtleComposer.Create(worker, style, lang);
    {$ENDIF}
    ffText : result := TFHIRTextComposer.Create(worker, style, lang);
    ffNDJson : result := TFHIRNDJsonComposer.Create(worker, style, lang);
    ffXhtml  : result := TFHIRXhtmlComposer.Create(worker, style, lang);
  else
    raise Exception.Create('Unspecified/unsupported format');
  end;
end;

class function TFHIRParsers.parser(worker: TFHIRWorkerContext; format: TFHIRFormat; lang: String): TFHIRParser;
begin
  case format of
    ffXml: result := TFHIRXmlParser.Create(worker, lang);
    ffJson: result := TFHIRJsonParser.Create(worker, lang);
    {$IFNDEF FHIR2}
    ffTurtle: result := TFHIRTurtleParser.Create(worker, lang);
    {$ENDIF}
  else
    raise Exception.Create('Unspecified/unsupported format');
  end;
end;

class procedure TFHIRParsers.composeFile(worker: TFHIRWorkerContext; format: TFHIRFormat; r: TFHIRResourceV; lang, filename: String; style: TFHIROutputStyle);
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

class function TFHIRParsers.ParseFile(worker: TFHIRWorkerContext; format: TFHIRFormat; lang, filename: String): TFHIRResource;
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

