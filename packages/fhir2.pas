{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir2;

{$warn 5023 off : no warning about unused units}
interface

uses
  fhir2_authmap, fhir2_base, fhir2_client, fhir2_common, fhir2_constants, 
  fhir2_context, fhir2_elementmodel, fhir2_factory, fhir2_indexinfo, 
  fhir2_javascript, fhir2_json, fhir2_opbase, fhir2_operations, fhir2_parser, 
  fhir2_parserBase, fhir2_patch, fhir2_pathengine, fhir2_pathnode, 
  fhir2_profiles, fhir2_resources, fhir2_resources_admin, 
  fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_clinical, 
  fhir2_resources_other, fhir2_tags, fhir2_types, fhir2_utilities, fhir2_xml, 
  fhir2_questionnaire, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir2', @Register);
end.
