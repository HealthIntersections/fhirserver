{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir4;

{$warn 5023 off : no warning about unused units}
interface

uses
  fhir4_adaptor, fhir4_authmap, fhir4_base, fhir4_client, fhir4_common, 
  fhir4_constants, fhir4_context, fhir4_elementmodel, fhir4_factory, 
  fhir4_graphdefinition, fhir4_indexinfo, fhir4_javascript, fhir4_json, 
  fhir4_liquid, fhir4_maputils, fhir4_opbase, fhir4_operations, 
  fhir4_organiser, fhir4_parser, fhir4_parserBase, fhir4_patch, 
  fhir4_pathengine, fhir4_pathnode, fhir4_profiles, fhir4_resources, 
  fhir4_resources_admin, fhir4_resources_base, fhir4_resources_canonical, 
  fhir4_resources_clinical, fhir4_resources_financial, 
  fhir4_resources_medications, fhir4_resources_other, fhir4_tags, 
  fhir4_turtle, fhir4_types, fhir4_utilities, fhir4_xml, fhir4_questionnaire, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir4', @Register);
end.
