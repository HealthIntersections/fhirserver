unit test_server_ini;

interface

uses
  fsl_testing,
  server_ini;

type
  TFHIRServerIniTestCases = class (TFslTestCase)
  published
    procedure testConfig;
  end;

procedure registerTests;

implementation

{ TFHIRServerIniTestCases }

procedure TFHIRServerIniTestCases.testConfig;
var
  f : TFHIRServerIniFile;
begin
  f := TFHIRServerIniFile.create('C:\work\fhirserver\testcases\config\example.cfg.txt');
  try
    assertTrue(f.web['host'] = 'local.fhir.org');
    f.web['host'] := 'local.fhir.org';
    f.save;
    assertTrue(f.web['host'] = 'local.fhir.org');
  finally
    f.Free;
  end;
end;

procedure registerTests;
begin
  RegisterTest('Server.Ini', TFHIRServerIniTestCases.Suite);
end;

end.
