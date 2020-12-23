{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fhir_fsl;

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
  IdExplicitTLSClientServerBase, IdTelnetServer, fsl_base, fsl_collections, 
  fsl_comparisons, fsl_crypto, fsl_fetcher, fsl_fpc, fsl_graphql, fsl_html, 
  fsl_http, fsl_java_jni, fsl_java_runtime, fsl_java_strings, 
  fsl_java_utilities, fsl_java_wrapper, fsl_javascript, fsl_json, fsl_lang, 
  fsl_logging, fsl_msxml, fsl_npm, fsl_npm_cache, fsl_npm_client, fsl_oauth, 
  fsl_openssl, fsl_rdf, fsl_scim, fsl_scrypt, fsl_service, fsl_service_win, 
  fsl_shell, fsl_stream, fsl_threads, fsl_turtle, fsl_twilio, fsl_utilities, 
  fsl_websocket, fsl_wininet, fsl_xml, fsl_ucum, IdLogDebug, 
  IdServerInterceptLogFile, fsl_htmlgen, fsl_diff, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fhir_fsl', @Register);
end.
