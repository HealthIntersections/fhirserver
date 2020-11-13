{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir;

{$warn 5023 off : no warning about unused units}
interface

uses
  fhir_cdshooks, fhir_client, fhir_client_async, fhir_client_debugger, 
  fhir_client_http, fhir_client_registry, fhir_client_threaded, fhir_codegen, 
  fhir_common, fhir_consentengine, fhir_diff, fhir_elementmodel, fhir_factory, 
  fhir_graphdefinition, fhir_graphql, fhir_htmlgen, fhir_indexing, 
  fhir_javascript, fhir_js_client, fhir_narrative, fhir_ndjson, fhir_oauth, 
  fhir_objects, fhir_oids, fhir_parser, fhir_pathengine, fhir_ucum, 
  fhir_utilities, fhir_validator, fhir_xhtml, cda_base, cda_documents, 
  cda_javascript, cda_narrative, cda_objects, cda_parser, cda_types, 
  cda_writer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir', @Register);
end.
