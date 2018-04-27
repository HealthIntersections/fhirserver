unit FHIR.R4.Base;

interface

uses
  FHIR.Base.Objects;

type
  TFHIRObject4 = class (TFHIRObject)
  protected
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function GetVersion: TFHIRVersion; override;
  end;

  TFHIRObjectX = TFHIRObject4;

  TFHIRResource4 = class (TFHIRResourceV)
  protected
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function GetVersion: TFHIRVersion; override;
  end;

  TFHIRResourceX = TFHIRResource4;

implementation

uses
  FHIR.R4.Types;


{ TFHIRObject4 }

function TFHIRObject4.GetVersion: TFHIRVersion;
begin
  result := fhirVersionRelease4;
end;

function TFHIRObject4.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFhirCode.Create(v);
end;

function TFHIRObject4.makeStringValue(v: String): TFHIRObject;
begin
  result := TFhirString.Create(v);
end;

{ TFHIRResource4 }

function TFHIRResource4.GetVersion: TFHIRVersion;
begin
  result := fhirVersionRelease4;
end;

function TFHIRResource4.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFhirCode.Create(v);
end;

function TFHIRResource4.makeStringValue(v: String): TFHIRObject;
begin
  result := TFhirString.Create(v);
end;

end.
