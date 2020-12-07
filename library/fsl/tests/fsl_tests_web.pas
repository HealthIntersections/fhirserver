unit fsl_tests_web;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$I fhir.inc}
{.$.DEFINE SSL_100_TESTS} // adds tests on older SSL version. deprecated.

interface

uses
  Sysutils,
  fsl_testing,
  IdGlobal, IdUri, IdSMTP, IdMessage, IdExplicitTLSClientServerBase, IdHTTPServer, IdSchedulerOfThreadPool, IdContext, IdCustomHTTPServer, IdSSLOpenSSL, IdHTTP, IdTcpClient,
  IdLogDebug, IdServerInterceptLogFile,
  IdOpenSSLVersion, IdOpenSSLIOHandlerClient, IdOpenSSLIOHandlerServer,
  fsl_json, fsl_utilities,
  fsl_oauth, fsl_http, fsl_fetcher, fsl_crypto;

const
  MASTER_URL = 'https://raw.githubusercontent.com/FHIR/ig-registry/master/fhir-ig-list.json';

type
  TIdUriParserTests = Class (TFslTestCase)
  private
    procedure ok(uri : String);
  published
    Procedure TestOK;
    Procedure TestFail;
    Procedure TestUnicode1;
    Procedure TestUnicode2;
  end;

  TLangParserTests = Class (TFslTestCase)
  Published
    Procedure testBase;
  End;

  TJWTTests = Class (TFslTestCase)
  public
    Procedure SetUp; override;
  Published
    procedure TestPacking;
    procedure TestUnpacking;
    procedure TestCert;
  End;

  TOpenSSLTests = Class (TFslTestCase)
  private
    FServer : TIdHTTPServer;
    FIOHandler : TIdOpenSSLIOHandlerServer;
    {$IFDEF SSL_100_TESTS}
    FIOHandlerOld : TIdServerIOHandlerSSLOpenSSL;
    procedure startServer100;
    procedure SSLPassword100(var Password: string);
    {$ENDIF}
    Procedure DoServe(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    procedure startServer110;
    procedure stopServer;
    procedure SSLPassword(Sender: TObject; var Password: string; const IsWrite: Boolean);
    procedure DoQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
  published
    procedure testWebFetcher;
    procedure testSendEmail;
    {$IFDEF SSL_100_TESTS}
    procedure testWebServer_100;  // openssl 1.0.2
    {$ENDIF}
    procedure testWebServer_110;  // openssl 1.1.0
  End;

procedure registerTests;

implementation

{ TIdUriParserTests }

procedure TIdUriParserTests.ok(uri: String);
var
  o : TIdUri;
begin
  o := TIdUri.create(uri);
  try
    assertTrue(o <> nil);
  finally
    o.free;
  end;
end;

procedure TIdUriParserTests.TestFail;
begin
  ok('http://foo@127.0.0.1 @google.com/');
end;

procedure TIdUriParserTests.TestOK;
begin
  ok('http://test.fhir.org/r3');
end;

procedure TIdUriParserTests.TestUnicode1;
begin
  ok('http://orange.tw/sandbox/o<.o<./passwd');
end;

procedure TIdUriParserTests.TestUnicode2;
begin
  ok('http://orange.tw/sandbox/%EF%BC%AE%EF%BC%AE/passwd');
end;

{ TLangParserTests }

Procedure TLangParserTests.testBase;
var
  lang : THTTPLanguages;
begin
  lang := THTTPLanguages.create('en');
  assertTrue(lang.header = 'en');
  assertTrue(length(lang.Codes) = 1);
  assertTrue(lang.Codes[0] = 'en');
  assertTrue(lang.prefLang = 'en');
  assertTrue(lang.matches('en'));
  assertTrue(lang.matches('en-AU'));
  assertTrue(not lang.matches('eng'));
end;


{ TJWTTests }

procedure TJWTTests.Setup;
begin
//  IdSSLOpenSSLHeaders.Load;
//  LoadEAYExtensions(true);
end;

procedure TJWTTests.TestCert;
var
  jwk : TJWK;
  s: String;
begin
  jwk := TJWTUtils.loadKeyFromRSACert(AnsiString(TestSettings.serverTestFile(['testcases', 'certs', 'jwt-test.key.crt'])));
  try
    s := TJSONWriter.writeObjectStr(jwk.obj, true);
    assertTrue(true);
  finally
    jwk.Free;
  end;
end;

var
  gs : String;

procedure TJWTTests.TestPacking;
var
  jwk : TJWK;
  s : String;
  jwt : TJWT;
begin
  jwk := TJWK.create(TJSONParser.Parse('{"kty": "oct", "k": "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow"}'));
  try
    // this test is from the spec
    s := TJWTUtils.pack(
      '{"typ":"JWT",'+#13#10+' "alg":"HS256"}',
      '{"iss":"joe",'+#13#10+' "exp":1300819380,'+#13#10+' "http://example.com/is_root":true}',
      jwt_hmac_sha256, jwk);
    assertTrue(s = 'eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk',
      'packing failed. expected '+#13#10+'eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk, but got '+s);
  finally
    jwk.Free;
  end;

  jwk := TJWK.create(TJSONParser.Parse(
     '{"kty":"RSA", '+#13#10+
     '  "kid": "http://tools.ietf.org/html/draft-ietf-jose-json-web-signature-26#appendix-A.2.1", '+#13#10+
     '  "n":"ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddxHmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMsD1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSHSXndS5z5rexMdb'+'BYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdVMTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ", '+#13#10+
     '  "e":"AQAB", '+#13#10+
     '  "d":"Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88vLNO97IjlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0BkTGoVEn2bKVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn439X0M_V51gfpR'+'LI9JYanrC4D4qAdGcopV_0ZHHzQlBjudU2QvXt4ehNYTCBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLhBOneufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ" '+#13#10+
     ' } '+#13#10
   ));
  try
    gs := TJWTUtils.pack(
      '{"alg":"RS256"}',
      '{"iss":"joe",'+#13#10+' "exp":1300819380,'+#13#10+' "http://example.com/is_root":true}',
      jwt_hmac_rsa256, jwk);
//    assertTrue(gs = 'eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.LteI-Jtns1KTLm0-lnDU_gI8_QHDnnIfZCEB2dI-ix4YxLQjaOTVQolkaa-Y4Cie-mEd8c34vSWeeNRgVcXuJsZ_iVYywDWqUDpXY6KwdMx6kXZQ0-'+'mihsowKzrFbmhUWun2aGOx44w3wAxHpU5cqE55B0wx2v_f98zUojMp6mkje_pFRdgPmCIYTbym54npXz7goROYyVl8MEhi1HgKmkOVsihaVLfaf5rt3OMbK70Lup3RrkxFbneKslTQ3bwdMdl_Zk1vmjRklvjhmVXyFlEHZVAe4_4n_FYk6oq6UFFJDkEjrWo25B0lKC7XucZZ5b8NDr04xujyV4XaR11ZuQ');
  finally
    jwk.Free;
  end;

  jwt := TJWT.create;
  try
    jwt.id := GUIDToString(CreateGUID);
    s := TJWTUtils.rsa_pack(jwt, jwt_hmac_rsa256, TestSettings.serverTestFile(['testcases', 'certs', 'jwt-test.key.key']), 'fhirserver');
    assertTrue(true);
  finally
    jwt.Free;
  end;
end;

var
  jwk : TJWKList;

procedure TJWTTests.TestUnpacking;
var
  jwt : TJWT;
begin
  // HS256 test from the spec
  jwk := TJWKList.create(TJSONParser.Parse('{"kty": "oct", "k": "AyM1SysPpbyDfgZld3umj1qzKObwVMkoqQ-EstJQLr_T-1qS0gZH75aKtMN3Yj0iPS4hcgUuTwjAzZr1Z9CAow"}'));
  try
    jwt := TJWTUtils.unpack('eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk', true, jwk);
    try
      // inspect
      assertTrue(true);
    finally
      jwt.Free;
    end;
  finally
    jwk.Free;
  end;
   (*
  // from google
  jwk := TJWKList.create(TJSONParser.Parse(
    // as downloaded from google at the same time as the JWT below
    '{'+#13#10+
    ' "keys": ['+#13#10+
    '  {'+#13#10+
    '   "kty": "RSA",'+#13#10+
    '   "alg": "RS256",'+#13#10+
    '   "use": "sig",'+#13#10+
    '   "kid": "024806d09e6067ca21bc6e25219d15dd981ddf9d",'+#13#10+
    '   "n": "AKGBohjSehyKnx7t5HZGzLtNaFpbNBiCf9O6G/qUeOy8l7XBflg/79G+t23eP77dJ+iCPEoLU1R/3NKPAk6Y6hKbSIvuzLY+B877ozutOn/6H/DNWumVZKnkSpDa7A5nsCNSm63b7uJ4XO5W0NtueiXj855h8j+WLi9vP8UwXhmL",'+#13#10+
    '   "e": "AQAB"'+#13#10+
    '  },'+#13#10+
    '  {'+#13#10+
    '   "kty": "RSA",'+#13#10+
    '   "alg": "RS256",'+#13#10+
    '   "use": "sig",'+#13#10+
    '   "kid": "8140c5f1c9d0c738c1b6328528f7ab1f672f5ba0",'+#13#10+
    '   "n": "AMAxJozHjwYxXqcimf93scqnDKZrKm1O4+TSH4eTJyjM1NU1DnhRJ8xL8fJd/rZwBWgPCUNi34pYlLWwfzR/17diqPgGSMt+mBVKXo5HD7+9SfQPjH3Fw810BQpxslBuAPsSGaNcLvHPpUSJDB/NH2rTxw6YtQ/R3neo7Amcfn/d",'+#13#10+
    '   "e": "AQAB"'+#13#10+
    '  }'+#13#10+
    ' ]'+#13#10+
    '}'+#13#10
  ));
  try
    jwt := TJWTUtils.unpack('eyJhbGciOiJSUzI1NiIsImtpZCI6IjAyNDgwNmQwOWU2MDY3Y2EyMWJjNmUyNTIxOWQxNWRkOTgxZGRmOWQifQ.eyJpc3MiOiJhY2NvdW50cy5nb29nbGUuY29tIiwic3ViIjoiMTExOTA0NjIwMDUzMzY0MzkyMjg2Ii'+'wiYXpwIjoiOTQwMDA2MzEwMTM4LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwiZW1haWwiOiJncmFoYW1lZ0BnbWFpbC5jb20iLCJhdF9oYXNoIjoidDg0MGJMS3FsRU'+'ZqUmQwLWlJS2dZUSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJhdWQiOiI5NDAwMDYzMTAxMzguYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJpYXQiOjE0MDIxODUxMjksImV'+'4cCI6MTQwMjE4OTAyOX0.Jybn06gURs7lcpCYaXBuszC7vacnWxwSwH_ffIDDu7bxOPo9fiVnRDCidKSLy4m0sAL1xxDHA5gXSZ9C6nj7abGqQ_LOrcPdTncuvYUPhF7mUq7fr3EPW-34PVkBSiOrjYdO6SOYyeP443WzPQRkhVJkRP4oQF-k0zXuwCkWlfc', true, jwk);
    try
      // inspect
      assertTrue(true);
    finally
      jwt.Free;
    end;
  finally
    jwk.free;
  end;

  // RS256 test from the spec (except the value is from above, because the sig doesn't match)
  jwk := TJWKList.create(TJSONParser.Parse(
     '{"kty":"RSA", '+#13#10+
     '  "kid": "http://tools.ietf.org/html/draft-ietf-jose-json-web-signature-26#appendix-A.2.1", '+#13#10+
     '  "n":"ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddxHmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMsD1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSHSXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg66'+'5xsmtdVMTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ", '+#13#10+
     '  "e":"AQAB", '+#13#10+
     '  "d":"Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88vLNO97IjlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0BkTGoVEn2bKVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn439X0M_V51gfpRLI9JYanrC4D4qAdGcopV_0ZHHzQlBjudU2QvX'+'t4ehNYTCBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLhBOneufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ" '+#13#10+
     ' } '+#13#10
   ));
  try
    jwt := TJWTUtils.unpack(gs {'eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.LteI-Jtns1KTLm0-lnDU_gI8_QHDnnIfZCEB2dI-ix4YxLQjaOTVQolkaa-Y4Cie-mEd8c34vSWeeNRgVcXuJsZ_iVYywDWqUDpXY6KwdMx6kXZQ0-'+'mihsowKzrFbmhUWun2aGOx44w3wAxHpU5cqE55B0wx2v_f98zUojMp6mkje_pFRdgPmCIYTbym54npXz7goROYyVl8MEhi1HgKmkOVsihaVLfaf5rt3OMbK70Lup3RrkxFbneKslTQ3bwdMdl_Zk1vmjRklvjhmVXyFlEHZVAe4_4n_FYk6oq6UFFJDkEjrWo25B0lKC7XucZZ5b8NDr04xujyV4XaR11ZuQ'}, true, jwk);
    try
      // inspect
      assertTrue(true);
    finally
      jwt.Free;
    end;
  finally
    jwk.Free;
  end;
    *)
end;


procedure RegisterTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Web.IdUri', TIdUriParserTests.Suite);
  RegisterTest('Web.Language Parser Tests', TLangParserTests.Suite);
  RegisterTest('Web.OpenSSL', TOpenSSLTests.Suite);
  RegisterTest('Web.JWT Tests', TJWTTests.Suite);
end;

{ TOpenSSLTests }

procedure TOpenSSLTests.testSendEmail;
var
  sender : TIdSMTP;
  msg : TIdMessage;
  ssl : TIdOpenSSLIOHandlerClient;
begin
  assertTrue(TestSettings.SMTPUsername <> '', 'Must provide username(/source email) for SMTP test in '+TestSettings.filename+' ([email] sender=)');
  assertTrue(TestSettings.SMTPPassword <> '', 'Must provide password for SMTP test in '+TestSettings.filename+' ([email] password=)');
  assertTrue(TestSettings.SMTPDestination <> '', 'Must provide destinatino for SMTP test in '+TestSettings.filename+' ([email] destination=)');

  sender := TIdSMTP.Create(Nil);
  try
    sender.Host := 'smtp.gmail.com';
    sender.port := 587;
    sender.Username := TestSettings.SMTPUsername;
    sender.Password := TestSettings.SMTPPassword;
    ssl := TIdOpenSSLIOHandlerClient.create;
    sender.IOHandler := ssl;
    sender.UseTLS := utUseExplicitTLS;
    ssl.Destination := 'smtp.gmail.com:587';
    ssl.Host := 'smtp.gmail.com';
    ssl.MaxLineAction := maException;
    ssl.Port := 587;
    ssl.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_3;
    ssl.Options.VerifyServerCertificate := false;
    sender.Connect;
    msg := TIdMessage.Create(Nil);
    try
      msg.Subject := 'Test Email';
      msg.Recipients.Add.Address := TestSettings.SMTPDestination;
      msg.From.Text := TestSettings.SMTPUsername;
      msg.Body.Text := 'Test Email from FHIRServer Unit tests';
      sender.Send(msg);
    Finally
      msg.Free;
    End;
    sender.Disconnect;
  Finally
    sender.IOHandler.free;
    sender.Free;
  End;
end;

procedure TOpenSSLTests.testWebFetcher;
var
  json : TJsonObject;
begin
  json := TInternetFetcher.fetchJson(MASTER_URL);
  try
    assertTrue(json <> nil)
  finally
    json.Free;
  end;
end;

procedure TOpenSSLTests.DoQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
begin
  VUseSSL := true;
end;

procedure TOpenSSLTests.SSLPassword(Sender: TObject; var Password: string; const IsWrite: Boolean);
begin
  Password := TestSettings.SSLPassword;
end;

procedure TOpenSSLTests.startServer110;
begin
  FServer := TIdHTTPServer.Create(Nil);
  FServer.Scheduler := TIdSchedulerOfThreadPool.Create(nil);
  TIdSchedulerOfThreadPool(FServer.Scheduler).PoolSize := 20;
  FServer.DefaultPort := 17423;
  FServer.KeepAlive := false;
  FIOHandler := TIdOpenSSLIOHandlerServer.Create(Nil);
  FServer.IOHandler := FIOHandler;

  FIOHandler.Options.CertFile := TestSettings.SSLCertFile;
  FIOHandler.Options.OnGetPassword := SSLPassword;
  FIOHandler.Options.CertKey := TestSettings.SSLKeyFile;
  FIOHandler.Options.VerifyCertificate := TestSettings.SSLCAFile;

  FIOHandler.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_3;
  FIOHandler.Options.TLSVersionMaximum := TIdOpenSSLVersion.TLSv1_3;
  FIOHandler.Options.UseServerCipherPreferences := true;
  FIOHandler.Options.AllowUnsafeLegacyRenegotiation := true;
  FIOHandler.Options.UseLegacyServerConnect := true;

  FServer.OnCommandGet := DoServe;
  FServer.OnQuerySSLPort := DoQuerySSLPort;
  FServer.active := true;
end;

procedure TOpenSSLTests.DoServe(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
begin
  response.ContentText := 'Response';
end;

{$IFDEF SSL_100_TESTS}
procedure TOpenSSLTests.SSLPassword100(var Password: string);
begin
  Password := TestSettings.SSLPassword;
end;

procedure TOpenSSLTests.startServer100;
begin
  FServer := TIdHTTPServer.Create(Nil);
  FServer.Scheduler := TIdSchedulerOfThreadPool.Create(nil);
  TIdSchedulerOfThreadPool(FServer.Scheduler).PoolSize := 20;
  FServer.ServerSoftware := 'Health Intersections FHIR Server';
  FServer.ParseParams := false;
  FServer.DefaultPort := 17423;
  FServer.KeepAlive := false;
  FIOHandlerOld := TIdServerIOHandlerSSLOpenSSL.Create(Nil);
  FServer.IOHandler := FIOHandlerOld;
  FIOHandlerOld.SSLOptions.Method := sslvTLSv1_2;
  FIOHandlerOld.SSLOptions.Mode := sslmServer;
  FIOHandlerOld.SSLOptions.SSLVersions := [sslvTLSv1_2];
  FIOHandlerOld.SSLOptions.CipherList := {$IFDEF NCTS}'ALL:!SSLv2:!DES:!RC4:!MD5:!SHA-1'{$ELSE}'ALL:!SSLv2:!DES'{$ENDIF};
  FIOHandlerOld.SSLOptions.CertFile := TestSettings.SSLCertFile;
  FIOHandlerOld.SSLOptions.KeyFile := TestSettings.SSLKeyFile;
  FIOHandlerOld.SSLOptions.RootCertFile := TestSettings.SSLCAFile;
  FIOHandlerOld.SSLOptions.VerifyMode := [];
  FIOHandlerOld.OnGetPassword := SSLPassword100;
  FServer.OnCommandGet := DoServe;
  FServer.OnQuerySSLPort := DoQuerySSLPort;
  FServer.active := true;
end;
procedure TOpenSSLTests.testWebServer_100;
var
  cnt : TBytes;
begin
  assertTrue(TestSettings.SSLCertFile <> '', 'Must provide public key file for SSL test in '+TestSettings.filename+' ([ssl] cert=)');
  assertTrue(TestSettings.SSLKeyFile <> '', 'Must provide private key file for SSL test in '+TestSettings.filename+' ([ssl] key=)');
  assertTrue(TestSettings.SSLPassword <> '', 'Must provide password for private key for SSL test in '+TestSettings.filename+' ([ssl] password=)');
  assertTrue(TestSettings.SSLCAFile <> '', 'Must provide ca cert file for SSL test in '+TestSettings.filename+' ([ssl] cacert=)');

  startServer100;
  try
    cnt := TInternetFetcher.fetchUrl('https://localhost:17423/test');
    assertTrue(length(cnt) = 8);
  finally
    try
      stopServer;
    except
      // nothing
    end;
  end;
end;

{$ENDIF}

procedure TOpenSSLTests.stopServer;
begin
  try
    FServer.active := false;
  except
    // nothing
  end;
  FServer.IOHandler := nil;
  FIOHandler.Free;
  {$IFDEF SSL_100_TESTS}
  FIOHandlerOld.Free;
  {$ENDIF}
  FServer.Scheduler.Free;
  FServer.Free;
end;

procedure TOpenSSLTests.testWebServer_110;
begin
  assertTrue(TestSettings.SSLCertFile <> '', 'Must provide public key file for SSL test in '+TestSettings.filename+' ([ssl] cert=)');
  assertTrue(TestSettings.SSLKeyFile <> '', 'Must provide private key file for SSL test in '+TestSettings.filename+' ([ssl] key=)');
  assertTrue(TestSettings.SSLPassword <> '', 'Must provide password for private key for SSL test in '+TestSettings.filename+' ([ssl] password=)');
  assertTrue(TestSettings.SSLCAFile <> '', 'Must provide ca cert file for SSL test in '+TestSettings.filename+' ([ssl] cacert=)');

  startServer110;
  try
    assertTrue(length(TInternetFetcher.fetchUrl('https://localhost:17423/test')) = 8);
  finally
    try
      stopServer;
    except
      // nothing
    end;
  end;
end;



end.
