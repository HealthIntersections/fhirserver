{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_fui;

{$warn 5023 off : no warning about unused units}
interface

uses
  fui_lcl_cache, fui_lcl_managers, fui_lcl_progress, fui_lcl_registry, 
  fui_lcl_widgets, fui_syn_hl7, fui_lcl_utilities, fui_fake_console, 
  fui_fake_console_settings, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_fui', @Register);
end.
