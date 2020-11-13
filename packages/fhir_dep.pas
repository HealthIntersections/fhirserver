{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_dep;

{$warn 5023 off : no warning about unused units}
interface

uses
  ChakraCommon, ChakraCore, ChakraCoreClasses, ChakraCoreUtils, 
  ChakraCoreVersion, ChakraDebug, Compat, IdHeaderList, IdStreamVCL, IdCTypes, 
  IdHMAC, IdHashSHA, IdHMACSHA1, IdOpenSSLHeaders_ossl_typ, 
  IdOpenSSLHeaders_aes, IdOpenSSLHeaders_asn1, IdOpenSSLHeaders_asn1_mac, 
  IdOpenSSLHeaders_asn1err, IdOpenSSLHeaders_asn1t, IdOpenSSLHeaders_async, 
  IdOpenSSLHeaders_asyncerr, IdOpenSSLHeaders_bio, IdOpenSSLHeaders_bioerr, 
  IdOpenSSLHeaders_blowfish, IdOpenSSLHeaders_bn, IdOpenSSLHeaders_bnerr, 
  IdOpenSSLHeaders_buffer, IdOpenSSLHeaders_buffererr, 
  IdOpenSSLHeaders_camellia, IdOpenSSLHeaders_cast, IdOpenSSLHeaders_cmac, 
  IdOpenSSLHeaders_cms, IdOpenSSLHeaders_cmserr, IdOpenSSLHeaders_comp, 
  IdOpenSSLHeaders_comperr, IdOpenSSLHeaders_conf, IdOpenSSLHeaders_conf_api, 
  IdOpenSSLHeaders_conferr, IdOpenSSLHeaders_crypto, 
  IdOpenSSLHeaders_cryptoerr, IdOpenSSLHeaders_cterr, IdOpenSSLHeaders_dh, 
  IdOpenSSLHeaders_dherr, IdOpenSSLHeaders_dsa, IdOpenSSLHeaders_dsaerr, 
  IdOpenSSLHeaders_ebcdic, IdOpenSSLHeaders_ec, IdOpenSSLHeaders_ecerr, 
  IdOpenSSLHeaders_engine, IdOpenSSLHeaders_engineerr, IdOpenSSLHeaders_err, 
  IdOpenSSLHeaders_evp, IdOpenSSLHeaders_evperr, IdOpenSSLHeaders_hmac, 
  IdOpenSSLHeaders_idea, IdOpenSSLHeaders_kdferr, IdOpenSSLHeaders_obj_mac, 
  IdOpenSSLHeaders_objects, IdOpenSSLHeaders_objectserr, 
  IdOpenSSLHeaders_ocsperr, IdOpenSSLHeaders_pem, IdOpenSSLHeaders_pemerr, 
  IdOpenSSLHeaders_pkcs7, IdOpenSSLHeaders_pkcs7err, IdOpenSSLHeaders_rand, 
  IdOpenSSLHeaders_randerr, IdOpenSSLHeaders_rsa, IdOpenSSLHeaders_rsaerr, 
  IdOpenSSLHeaders_sha, IdOpenSSLHeaders_srtp, IdOpenSSLHeaders_ssl, 
  IdOpenSSLHeaders_ssl3, IdOpenSSLHeaders_sslerr, IdOpenSSLHeaders_storeerr, 
  IdOpenSSLHeaders_tls1, IdOpenSSLHeaders_ts, IdOpenSSLHeaders_tserr, 
  IdOpenSSLHeaders_txt_db, IdOpenSSLHeaders_ui, IdOpenSSLHeaders_uierr, 
  IdOpenSSLHeaders_whrlpool, IdOpenSSLHeaders_x509, IdOpenSSLHeaders_x509_vfy, 
  IdOpenSSLHeaders_x509err, IdOpenSSLHeaders_x509v3, IdOpenSSLConsts, 
  IdOpenSSLContext, IdOpenSSLContextClient, IdOpenSSLContextServer, 
  IdOpenSSLExceptionResourcestrings, IdOpenSSLExceptions, 
  IdOpenSSLIOHandlerClient, IdOpenSSLIOHandlerClientBase, 
  IdOpenSSLIOHandlerClientServer, IdOpenSSLIOHandlerServer, IdOpenSSLLoader, 
  IdOpenSSLOptions, IdOpenSSLOptionsClient, IdOpenSSLOptionsServer, 
  IdOpenSSLPersistent, IdOpenSSLSocket, IdOpenSSLSocketClient, 
  IdOpenSSLSocketServer, IdOpenSSLTypes, IdOpenSSLUtils, IdOpenSSLVersion, 
  IdOpenSSLX509, IdFTP, IdHTTP, IdCustomHTTPServer, IdHTTPServer, IdTelnet, 
  IdStream, IdSMTP, IdCompressorZLib, IdScheduler, IdSchedulerOfThread, 
  IdSchedulerOfThreadPool, IdPOP3, IdMessage, IdMessageParts, 
  IdExplicitTLSClientServerBase, IdTelnetServer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_dep', @Register);
end.
