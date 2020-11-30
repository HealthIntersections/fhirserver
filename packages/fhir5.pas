{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir5;

{$warn 5023 off : no warning about unused units}
interface

uses
  fhir5_adaptor, fhir5_authmap, fhir5_base, fhir5_client, fhir5_common, 
  fhir5_constants, fhir5_context, fhir5_elementmodel, fhir5_factory, 
  fhir5_graphdefinition, fhir5_indexinfo, fhir5_javascript, fhir5_json, 
  fhir5_liquid, fhir5_maputils, fhir5_opbase, fhir5_operations, 
  fhir5_organiser, fhir5_parser, fhir5_parserBase, fhir5_patch, 
  fhir5_pathengine, fhir5_pathnode, fhir5_profiles, fhir5_resources, 
  fhir5_resources_admin, fhir5_resources_base, fhir5_resources_canonical, 
  fhir5_resources_clinical, fhir5_resources_financial, 
  fhir5_resources_medications, fhir5_resources_other, fhir5_tags, 
  fhir5_turtle, fhir5_types, fhir5_utilities, fhir5_xml, fhir5_questionnaire, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir5', @Register);
end.
