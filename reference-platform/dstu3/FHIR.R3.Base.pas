unit FHIR.R3.Base;

interface

uses
  FHIR.Base.Objects;

type
  TFHIRObject3 = class (TFHIRObject)
  protected
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function GetFhirObjectVersion: TFHIRVersion; override;
  end;

  TFHIRObjectX = TFHIRObject3;

  TFHIRResource3 = class (TFHIRResourceV)
  protected
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function GetFhirObjectVersion: TFHIRVersion; override;
  end;

  TFHIRResourceX = TFHIRResource3;

implementation

uses
  FHIR.R3.Types;


{ TFHIRObject3 }

function TFHIRObject3.GetFhirObjectVersion: TFHIRVersion;
begin
  result := fhirVersionRelease3;
end;

function TFHIRObject3.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFhirCode.Create(v);
end;

function TFHIRObject3.makeStringValue(v: String): TFHIRObject;
begin
  result := TFhirString.Create(v);
end;

{ TFHIRResource3 }

function TFHIRResource3.GetFhirObjectVersion: TFHIRVersion;
begin
  result := fhirVersionRelease3;
end;

function TFHIRResource3.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFhirCode.Create(v);
end;

function TFHIRResource3.makeStringValue(v: String): TFHIRObject;
begin
  result := TFhirString.Create(v);
end;

end.
