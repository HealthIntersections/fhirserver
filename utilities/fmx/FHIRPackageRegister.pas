unit FHIRPackageRegister;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FHIRStringEdit{, FMX.Controls,
  FMX.Controls.Presentation, FMX.Edit};

procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('FHIR', [TFHIRStringEdit]);
end;


end.
