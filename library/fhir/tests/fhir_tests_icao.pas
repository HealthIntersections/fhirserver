unit fhir_tests_icao;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_testing, fsl_stream, fsl_crypto,
  fhir_objects, fhir_icao,
  fhir4_factory;

type

  { TFHIRICAOTests }

  TFHIRICAOTests = Class (TFslTestCase)
  public
  published
    {$IFDEF WINDOWS}
    // this is labelled as windows only in order to prevent the ci-build failing because the certificate - a real one - isn't in git (todo: sort this out)
    Procedure TestIcaoCertAu;
    {$ENDIF}
    Procedure TestIcaoCertAuBroken;
    Procedure TestIcaoCertNoStore;
  end;

procedure registerTests;

implementation

procedure registerTests;
begin
  RegisterTest('R4', TFHIRICAOTests.Suite);
end;


{ TFHIRICAOTests }

{$IFDEF WINDOWS}
procedure TFHIRICAOTests.TestIcaoCertAu;
var
  imp : TICAOCardImporter;
  card : THealthcareCard;
begin
  imp := TICAOCardImporter.Create;
  try
    imp.factory := TFHIRFactoryR4.Create;
    imp.issuer := 'http://test.fhir.org';
    imp.jwk := TJWK.loadFromFile(TestSettings.serverTestFile(['testcases' ,'jwk', 'test.jwk']));
    imp.mustVerify := true;
    imp.Store := TX509CertificateStore.create;
    imp.Store.addFolder(TestSettings.serverTestFile(['testcases' ,'jwk', 'store']));

    card := imp.import(FileToString(TestSettings.serverTestFile(['testcases' ,'icao', 'fhir-test-icao.json']), TEncoding.UTF8));
    try
      assertTrue(card <> nil);
    finally
      card.Free;
    end;
  finally
    imp.Free;
  end;
end;
{$ENDIF}

procedure TFHIRICAOTests.TestIcaoCertAuBroken;
var
  imp : TICAOCardImporter;
  card : THealthcareCard;
begin
  imp := TICAOCardImporter.Create;
  try
    imp.factory := TFHIRFactoryR4.Create;
    imp.issuer := 'http://test.fhir.org';
    imp.jwk := TJWK.loadFromFile(TestSettings.serverTestFile(['testcases' ,'jwk', 'test.jwk']));
    imp.Store := TX509CertificateStore.create;
    imp.Store.addFolder(TestSettings.serverTestFile(['testcases' ,'jwk', 'store']));

    try
      card := imp.import(FileToString(TestSettings.serverTestFile(['testcases' ,'icao', 'fhir-test-icao-broken.json']), TEncoding.UTF8));
      assertFail('Should have blown up');
    except
      on e : Exception do
        assertEqual('The Covid Passport Signature is not valid', e.message, 'Exception Message is wrong for signature validation fail');
    end;
  finally
    imp.Free;
  end;
end;

procedure TFHIRICAOTests.TestIcaoCertNoStore;
var
  imp : TICAOCardImporter;
  card : THealthcareCard;
begin
  imp := TICAOCardImporter.Create;
  try
    imp.factory := TFHIRFactoryR4.Create;
    imp.issuer := 'http://test.fhir.org';
    imp.jwk := TJWK.loadFromFile(TestSettings.serverTestFile(['testcases' ,'jwk', 'test.jwk']));
    imp.mustVerify := true;
    imp.Store := TX509CertificateStore.create;

    try
      card := imp.import(FileToString(TestSettings.serverTestFile(['testcases' ,'icao', 'fhir-test-icao.json']), TEncoding.UTF8));
      assertFail('Should have blown up');
    except
      on e : Exception do
        assertEqual('Cannot verify certificate - no match for key "3617C1E7F56795712E3775708E55833186E9380E"', e.message, 'Exception Message is wrong for certificate verification fail');
    end;
  finally
    imp.Free;
  end;
end;

end.
