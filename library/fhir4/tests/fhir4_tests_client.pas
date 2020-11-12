unit fhir4_tests_Client;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

interface

uses
  SysUtils, Classes,
  fsl_testing, fsl_utilities,
  fsl_http,
  fhir_objects, fhir_client_http,
  fhir4_json, fhir4_client, fhir4_types, fhir4_resources_base, fhir4_resources, fhir4_constants, fhir4_context, fhir4_pathengine, fhir4_tests_worker;

Type
  TFhirHTTPClientTests4 = class (TFslTestCase)
  private
    FWorker : TFHIRWorkerContext;
    function loadResource(filename : String) : TFHIRResource;
    procedure testClient(client: TFhirClient4);
  public
    Procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testIndyJson;
    procedure testIndyXml;
    {$IFNDEF FPC}
    procedure testIEJson;
    procedure testIEXml;
    {$ENDIF}
  end;

procedure registerTests;

implementation

{ TFhirHTTPClientTests4 }

procedure TFhirHTTPClientTests4.setup;
begin
  FWorker := TTestingWorkerContext4.Use;
end;

procedure TFhirHTTPClientTests4.TearDown;
begin
  FWorker.Free;
end;

function TFhirHTTPClientTests4.LoadResource(filename: String): TFHIRResource;
var
  f : TFileStream;
  prsr : TFHIRJsonParser;
begin
  f := TFileStream.Create(TestSettings.fhirTestFile(['R4', 'examples', filename]), fmOpenRead + fmShareDenyWrite);
  try
    prsr := TFHIRJsonParser.Create(nil, THTTPLanguages.create('en'));
    try
      prsr.source := f;
      prsr.parse;
      result := prsr.resource.Link as TFhirResource;
    finally
      prsr.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TFhirHTTPClientTests4.testClient(client: TFhirClient4);
var
  patient : TFhirPatient;
  id : string;
  ok : boolean;
begin
  client.conformance(true).Free;
  client.conformance(false).Free;
  patient := LoadResource('patient-example.json') as TFHIRPatient;
  try
    client.createResource(patient, id);
  finally
    patient.free
  end;
  patient := client.readResource(frtPatient, id) as TFHIRPatient;
  try
    patient.deceased := TFHIRDate.Create(TFslDateTime.makeUTC);
    client.updateResource(patient);
  finally
    patient.free;
  end;
  ok := false;
  client.deleteResource(frtPatient, id);
  try
    client.readResource(frtPatient, id).Free;
  except
    ok := true;
  end;
  assertTrue(ok);
end;

{$IFNDEF FPC}
procedure TFhirHTTPClientTests4.testIEJson;
var
  http: TFHIRHTTPCommunicator;
  client : TFhirClient4;
begin
  http := TFHIRHTTPCommunicator.Create('http://test.fhir.org/r4');
  try
    http.UseIndy := false;
    client := TFhirClient4.Create(FWorker.link, THTTPLanguages.Create('en'), http.link);
    try
      client.format := ffJson;
      testClient(client);
    finally
      client.Free;
    end;
  finally
    http.Free;
  end;
end;

procedure TFhirHTTPClientTests4.testIEXml;
var
  http: TFHIRHTTPCommunicator;
  client : TFhirClient4;
begin
  http := TFHIRHTTPCommunicator.Create('http://test.fhir.org/r4');
  try
    http.UseIndy := false;
    client := TFhirClient4.Create(FWorker.link, THTTPLanguages.Create('en'), http.link);
    try
      client.format := ffXml;
      testClient(client);
    finally
      client.Free;
    end;
  finally
    http.Free;
  end;
end;
{$ENDIF}

procedure TFhirHTTPClientTests4.testIndyJson;
var
  http: TFHIRHTTPCommunicator;
  client : TFhirClient4;
begin
  http := TFHIRHTTPCommunicator.Create('http://test.fhir.org/r4');
  try
    http.UseIndy := true;
    client := TFhirClient4.Create(FWorker.link, THTTPLanguages.Create('en'), http.link);
    try
      client.format := ffJson;
      testClient(client);
    finally
      client.Free;
    end;
  finally
    http.Free;
  end;
end;

procedure TFhirHTTPClientTests4.testIndyXml;
var
  http: TFHIRHTTPCommunicator;
  client : TFhirClient4;
begin
  http := TFHIRHTTPCommunicator.Create('http://test.fhir.org/r4');
  try
    http.UseIndy := true;
    client := TFhirClient4.Create(FWorker.link, THTTPLanguages.Create('en'), http.link);
    try
      client.format := ffXml;
      testClient(client);
    finally
      client.Free;
    end;
  finally
    http.Free;
  end;
end;

procedure registerTests;
begin
  RegisterTest('R4', TFhirHTTPClientTests4.Suite);
end;

end.

