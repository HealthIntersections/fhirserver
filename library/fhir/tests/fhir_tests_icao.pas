unit fhir_tests_icao;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_testing, fsl_utilities, fsl_stream, fsl_crypto,
  fhir_objects, fhir_icao,
  fhir4_factory;

type

  { TFHIRICAOTests }

  TFHIRICAOTests = Class (TFslTestCase)
  public
    {$IFDEF WINDOWS}
    // this is labelled as windows only in order to prevent the ci-build failing because the certificate - a real one - isn't in git (todo: sort this out)
    Procedure TestIcaoCertAu;
    {$ENDIF}
  published
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
    try
      imp.factory := TFHIRFactoryR4.Create;
      imp.issuer := 'http://test.fhir.org';
      imp.jwk := TJWK.loadFromFile(TestSettings.serverTestFile(['testcases' ,'jwk', 'test.jwk']));
      imp.mustVerify := true;
      imp.Store := TX509CertificateStore.Create;
      imp.Store.addFolder(TestSettings.serverTestFile(['testcases' ,'jwk', 'store']));

      card := imp.import(FileToString(TestSettings.serverTestFile(['testcases' ,'icao', 'fhir-test-icao.json']), TEncoding.UTF8));
      try
        assertTrue(card <> nil);
      finally
        card.free;
      end;
    finally
      StringToFile(imp.htmlReport, FilePath(['[tmp]', 'icao-verify.log']), TEncoding.UTF8);
    end;
  finally
    imp.free;
  end;
end;
{$ENDIF}

procedure TFHIRICAOTests.TestIcaoCertAuBroken;
{$IFNDEF LINUX}
var
  imp : TICAOCardImporter;
  card : THealthcareCard;
{$ENDIF}
begin
  {$IFNDEF LINUX}
  imp := TICAOCardImporter.Create;
  try
    imp.factory := TFHIRFactoryR4.Create;
    imp.issuer := 'http://test.fhir.org';
    imp.jwk := TJWK.loadFromFile(TestSettings.serverTestFile(['testcases' ,'jwk', 'test.jwk']));
    imp.Store := TX509CertificateStore.Create;
    imp.Store.addFolder(TestSettings.serverTestFile(['testcases' ,'jwk', 'store']));

    try
      card := imp.import(FileToString(TestSettings.serverTestFile(['testcases' ,'icao', 'fhir-test-icao-broken.json']), TEncoding.UTF8));
      assertFail('Should have blown up');
    except
      on e : Exception do
        assertEqual('The Covid Passport Signature is not valid', e.message, 'Exception Message is wrong for signature validation fail');
    end;
  finally
    imp.free;
  end;
  {$ENDIF}
end;

procedure TFHIRICAOTests.TestIcaoCertNoStore;
{$IFNDEF LINUX}
var
  imp : TICAOCardImporter;
  card : THealthcareCard;
{$ENDIF}
begin
  {$IFNDEF LINUX}
  imp := TICAOCardImporter.Create;
  try
    imp.factory := TFHIRFactoryR4.Create;
    imp.issuer := 'http://test.fhir.org';
    imp.jwk := TJWK.loadFromFile(TestSettings.serverTestFile(['testcases' ,'jwk', 'test.jwk']));
    imp.mustVerify := true;
    imp.Store := TX509CertificateStore.Create;
    try
      card := imp.import(FileToString(TestSettings.serverTestFile(['testcases' ,'icao', 'fhir-test-icao-broken.json']), TEncoding.UTF8));
      assertFail('Should have blown up');
    except
      on e : Exception do
        assertEqual('Cannot verify certificate - no match for key "3617C1E7F56795712E3775708E55833186E9380E"', e.message, 'Exception Message is wrong for certificate verification fail');
    end;
  finally
    imp.free;
  end;
  {$ENDIF}
end;

end.
