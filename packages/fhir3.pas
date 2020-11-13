{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir3;

{$warn 5023 off : no warning about unused units}
interface

uses
  fhir3_authmap, fhir3_base, fhir3_client, fhir3_common, fhir3_constants, 
  fhir3_context, fhir3_elementmodel, fhir3_factory, fhir3_indexinfo, 
  fhir3_javascript, fhir3_json, fhir3_liquid, fhir3_maputils, fhir3_opbase, 
  fhir3_operations, fhir3_organiser, fhir3_parser, fhir3_parserBase, 
  fhir3_patch, fhir3_pathengine, fhir3_pathnode, fhir3_profiles, 
  fhir3_resources, fhir3_resources_admin, fhir3_resources_base, 
  fhir3_resources_canonical, fhir3_resources_clinical, fhir3_resources_other, 
  fhir3_tags, fhir3_turtle, fhir3_types, fhir3_utilities, fhir3_xml, 
  fhir3_questionnaire, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir3', @Register);
end.
