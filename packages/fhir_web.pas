{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_web;

{$warn 5023 off : no warning about unused units}
interface

uses
  fsl_crypto, fsl_web_stream, fsl_fetcher, fsl_web_init, fsl_npm_cache, 
  fsl_npm_client, fsl_oauth, fsl_openssl, fsl_twilio, fsl_websocket, 
  fsl_wininet, fsl_zulip, fsl_msxml, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_web', @Register);
end.
