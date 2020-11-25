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
  fhir_graphdefinition, fhir_graphql, fhir_indexing, fhir_javascript, 
  fhir_js_client, fhir_narrative, fhir_ndjson, fhir_oauth, fhir_objects, 
  fhir_oids, fhir_parser, fhir_pathengine, fhir_utilities, fhir_validator, 
  fhir_xhtml, cda_base, cda_documents, cda_javascript, cda_narrative, 
  cda_objects, cda_parser, cda_types, cda_writer, fdb_dialects, fdb_logging, 
  fdb_manager, fdb_odbc, fdb_odbc_fpc, fdb_odbc_objects, fdb_settings, 
  fdb_sqlite3, fdb_sqlite3_objects, fdb_sqlite3_utilities, 
  fdb_sqlite3_wrapper, fhir_codesystem_service, fhir_valuesets, 
  ftx_loinc_importer, ftx_loinc_publisher, ftx_loinc_services, ftx_ndc, 
  ftx_sct_analysis, ftx_sct_combiner, ftx_sct_expressions, ftx_sct_importer, 
  ftx_sct_publisher, ftx_sct_services, ftx_service, ftx_ucum_base, 
  ftx_ucum_expressions, ftx_ucum_handlers, ftx_ucum_search, ftx_ucum_services, 
  ftx_ucum_validators, v2_base, v2_conformance, v2_dictionary, 
  v2_dictionary_Compiled, v2_dictionary_database, v2_dictionary_v21, 
  v2_dictionary_v22, v2_dictionary_v23, v2_dictionary_v24, v2_dictionary_v25, 
  v2_dictionary_v26, v2_dictionary_v27, v2_dictionary_v231, 
  v2_dictionary_v251, v2_dictionary_Versions, v2_javascript, v2_message, 
  v2_objects, v2_protocol, dicom_dictionary, dicom_jpegls, dicom_objects, 
  dicom_parser, dicom_Writer, ftx_lang, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir', @Register);
end.
