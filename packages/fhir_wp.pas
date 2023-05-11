{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_wp;

{$warn 5023 off : no warning about unused units}
interface

uses
  wp_types, wp_graphics, wp_printing_base, wp_document, wp_working, wp_text, 
  wp_format, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_wp', @Register);
end.
