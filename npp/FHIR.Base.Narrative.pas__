unit FHIR.Base.Narrative;

interface

uses
  SysUtils,
  FHIR.Support.Objects,
  FHIR.Base.Objects;

type
  TFHIRNarrativeGeneratorBase = class (TFslObject)
  public
    constructor create(context : TFHIRWorkerContextV); virtual;
    procedure generate(res : TFHIRResourceV); virtual;
  end;

implementation

{ TFHIRNarrativeGeneratorBase }

constructor TFHIRNarrativeGeneratorBase.create(context: TFHIRWorkerContextV);
begin
  inherited Create;
end;

procedure TFHIRNarrativeGeneratorBase.generate(res: TFHIRResourceV);
begin
  raise Exception.Create('The method TFHIRNarrativeGeneratorBase.generate should never be called');
end;

end.
