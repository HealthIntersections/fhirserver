unit FHIRBase3;

interface

uses
  FHIRBase;

type
  TFHIRObject3 = class (TFHIRObject)
  protected
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function GetVersion: TFHIRVersion; override;
  end;

  TFHIRObjectX = TFHIRObject3;

  TFHIRResource3 = class (TFHIRResourceV)
  protected
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function GetVersion: TFHIRVersion; override;
  end;

  TFHIRResourceX = TFHIRResource3;

implementation

uses
  FHIRTypes3;


{ TFHIRObject3 }

function TFHIRObject3.GetVersion: TFHIRVersion;
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

function TFHIRResource3.GetVersion: TFHIRVersion;
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
