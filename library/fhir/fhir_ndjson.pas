unit fhir_ndjson;

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


interface

uses
  SysUtils, Classes,
  fsl_base, fsl_xml, fsl_stream,
  fhir_objects,  fhir_parser, fhir_factory, fhir_common;

type
  TFHIRNDJsonComposer = class (TFHIRComposer)
  private
    FFactory : TFHIRFactory;
    procedure SetFactory(const Value: TFHIRFactory);
  protected
    function GetFormat: TFHIRFormat; override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;

    property factory : TFHIRFactory read FFactory write SetFactory;
    Procedure Compose(stream : TStream; oResource : TFhirResourceV); Override;
    Procedure ComposeResourceV(xml : TXmlBuilder; oResource : TFhirResourceV); overload; override;
  end;


implementation

{ TFHIRNDJsonComposer }

procedure TFHIRNDJsonComposer.Compose(stream: TStream; oResource: TFhirResourceV);
var
  json : TFHIRComposer;
  b : TFhirBundleW;
  be : TFhirBundleEntryW;
  first : boolean;
  ch : char;
begin
  ch := #10;
  if oResource.fhirType = 'Bundle' then
  begin
    first := true;
    b := FFactory.wrapBundle(oResource.link);
    try
      for be in b.entries.forEnum do
      begin
        if first then
          first := false
        else
          stream.Write(ch, 1);
        if be.resource <> nil then
        begin
          json := factory.makeComposer(FWorker.link, ffJson, lang, OutputStyleNormal);
          try
            json.Compose(stream, be.resource);
          finally
            json.Free;
          end;
        end
        else if be.tag is TFslBuffer then
        begin
          TFslBuffer(be.tag).SaveToStream(stream);
        end;
      end;
    finally
      b.Free;
    end;
  end
  else
  begin
    json := factory.makeComposer(FWorker.link, ffJson, lang, OutputStyleNormal);
    try
      json.Compose(stream, oResource);
    finally
      json.Free;
    end;
  end;
end;

procedure TFHIRNDJsonComposer.ComposeResourceV(xml: TXmlBuilder; oResource: TFhirResourceV);
begin
  raise EFHIRTodo.create('TFHIRNDJsonComposer.ComposeResourceV');
end;

destructor TFHIRNDJsonComposer.Destroy;
begin
  FFactory.Free;
  inherited;
end;

function TFHIRNDJsonComposer.GetFormat: TFHIRFormat;
begin
  result := ffNDJson;
end;


procedure TFHIRNDJsonComposer.SetFactory(const Value: TFHIRFactory);
begin
  FFactory.Free;
  FFactory := Value;
end;

function TFHIRNDJsonComposer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFactory.sizeInBytes);
end;

end.
