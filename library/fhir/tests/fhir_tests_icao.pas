unit fhir_tests_icao;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_testing, fsl_stream,
  fhir_objects, fhir_icao,
  fhir4_factory;

type
  TFHIRICAOTests = Class (TFslTestCase)
  public
  published
    // Procedure TestIcaoCertAu;
  end;

procedure registerTests;

implementation

procedure registerTests;
begin
  RegisterTest('R4', TFHIRICAOTests.Suite);
end;


//{ TFHIRICAOTests }
//
//procedure TFHIRICAOTests.TestIcaoCertAu;
//var
//  imp : TICAOCardImporter;
//  card : THealthcareCard;
//begin
//  imp := TICAOCardImporter.Create;
//  try
//    imp.factory := TFHIRFactoryR4.Create;
//    imp.issuer := 'http://test.fhir.org';
//
//    card := imp.import(FileToString('C:\work\fhirserver\testcases\icao\fhir-test-icao.json', TEncoding.UTF8));
//    try
//      assertTrue(card <> nil);
//    finally
//      card.Free;
//    end;
//  finally
//    imp.Free;
//  end;
//end;

end.
