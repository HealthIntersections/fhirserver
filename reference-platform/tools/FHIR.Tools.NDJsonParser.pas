unit FHIR.Tools.NDJsonParser;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Xml, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Lang, FHIR.Base.Parser, FHIR.Base.Factory, FHIR.Base.Common;

type
  TFHIRNDJsonComposer = class (TFHIRComposer)
  private
    FFactory : TFHIRFactory;
    procedure SetFactory(const Value: TFHIRFactory);
  protected
    function GetFormat: TFHIRFormat; override;
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
  raise EFHIRException.create('Not done yet');
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

end.
