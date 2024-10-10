{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_fsl;

{$warn 5023 off : no warning about unused units}
interface

uses
  fsl_base, fsl_collections, fsl_comparisons, fsl_fpc, fsl_graphql, fsl_html, 
  fsl_http, fsl_json, fsl_lang, fsl_logging, fsl_npm, fsl_rdf, fsl_scim, 
  fsl_scrypt, fsl_service, fsl_service_win, fsl_shell, fsl_stream, 
  fsl_threads, fsl_turtle, fsl_utilities, fsl_xml, fsl_ucum, fsl_htmlgen, 
  fsl_diff, fsl_unicode, fsl_versions, fsl_i18n, fsl_fpc_memory, fsl_regex, 
  fsl_gzip, fsl_cpu, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_fsl', @Register);
end.
