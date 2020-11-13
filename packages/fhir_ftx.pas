{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_ftx;

{$warn 5023 off : no warning about unused units}
interface

uses
  fhir_codesystem_service, fhir_valuesets, ftx_loinc_importer, 
  ftx_loinc_publisher, ftx_loinc_services, ftx_ndc, ftx_sct_analysis, 
  ftx_sct_combiner, ftx_sct_expressions, ftx_sct_importer, ftx_sct_publisher, 
  ftx_sct_services, ftx_service, ftx_ucum_base, ftx_ucum_expressions, 
  ftx_ucum_handlers, ftx_ucum_search, ftx_ucum_services, ftx_ucum_validators, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_ftx', @Register);
end.
