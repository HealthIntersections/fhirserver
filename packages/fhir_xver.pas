{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_xver;

{$warn 5023 off : no warning about unused units}
interface

uses
  fxver_conversion_engine, fxver_convertor_30_40, fxver_convertor_base, 
  fxver_convertors, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_xver', @Register);
end.
