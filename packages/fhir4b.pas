{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir4b;

{$warn 5023 off : no warning about unused units}
interface

uses
  fhir4b_adaptor, fhir4b_authmap, fhir4b_base, fhir4b_client, fhir4b_common, 
  fhir4b_constants, fhir4b_context, fhir4b_elementmodel, fhir4b_factory, 
  fhir4b_graphdefinition, fhir4b_indexinfo, fhir4b_json, fhir4b_liquid, 
  fhir4b_maputils, fhir4b_opbase, fhir4b_operations, fhir4b_organiser, 
  fhir4b_parser, fhir4b_parserBase, fhir4b_patch, fhir4b_pathengine, 
  fhir4b_pathnode, fhir4b_profiles, fhir4b_resources, fhir4b_resources_admin, 
  fhir4b_resources_base, fhir4b_resources_canonical, fhir4b_resources_clinical, 
  fhir4b_resources_financial, fhir4b_resources_medications, 
  fhir4b_resources_other, fhir4b_tags, fhir4b_turtle, fhir4b_types, 
  fhir4b_utilities, fhir4b_xml, fhir4b_questionnaire, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir4b', @Register);
end.
