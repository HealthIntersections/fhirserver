{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_fsl;

{$warn 5023 off : no warning about unused units}
interface

uses
  fsl_base, fsl_collections, fsl_comparisons, fsl_crypto, fsl_fetcher, 
  fsl_fpc, fsl_graphql, fsl_html, fsl_http, fsl_javascript, fsl_json, 
  fsl_lang, fsl_logging, fsl_npm, fsl_npm_cache, fsl_npm_client, fsl_oauth, 
  fsl_openssl, fsl_rdf, fsl_scim, fsl_scrypt, fsl_service, fsl_stream, 
  fsl_threads, fsl_turtle, fsl_twilio, fsl_utilities, fsl_websocket, fsl_xml, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_fsl', @Register);
end.
