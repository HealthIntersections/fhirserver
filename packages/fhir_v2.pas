{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_v2;

{$warn 5023 off : no warning about unused units}
interface

uses
  v2_base, v2_conformance, v2_dictionary, v2_dictionary_Compiled, 
  v2_dictionary_Database, v2_dictionary_v21, v2_dictionary_v22, 
  v2_dictionary_v23, v2_dictionary_v24, v2_dictionary_v25, v2_dictionary_v26, 
  v2_dictionary_v27, v2_dictionary_v231, v2_dictionary_v251, 
  v2_dictionary_Versions, v2_javascript, v2_message, v2_objects, v2_protocol, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_v2', @Register);
end.
