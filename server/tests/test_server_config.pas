unit test_server_config;

interface

uses
  fsl_testing,
  server_config;

type
  TFHIRServerConfigTestCases = class (TFslTestCase)
  published
    procedure testConfig;
  end;

procedure registerTests;

implementation

{ TFHIRServerConfigTestCases }

procedure TFHIRServerConfigTestCases.testConfig;
var
  f : TFHIRServerConfigFile;
begin
  f := TFHIRServerConfigFile.create('C:\work\fhirserver\testcases\config\example.cfg.txt');
  try
    assertTrue(f.web['host'].value = 'local.fhir.org');
    f.web['host'].value := 'local.fhir.org';
    f.save;
    assertTrue(f.web['host'].value = 'local.fhir.org');
  finally
    f.Free;
  end;
end;

procedure registerTests;
begin
  RegisterTest('Server.Ini', TFHIRServerConfigTestCases.Suite);
end;

end.
