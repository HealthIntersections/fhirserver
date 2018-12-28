unit FHIR.R3.Tests.Client;

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
  FHIR.Support.Utilities,
  FHIR.Base.Objects, FHIR.Version.Parser,
  FHIR.R3.Types, FHIR.R3.Resources, FHIR.R3.Constants, FHIR.R3.Context, FHIR.R3.PathEngine, FHIR.R3.Tests.Worker,
  DUnitX.TestFramework;


Type
  [TextFixture]
  TFhirHTTPClientTests = class (TObject)
  private
    FWorker : TFHIRWorkerContext;

  public
    [SetupFixture] procedure setup;
    [TearDownFixture] procedure teardown;

  end;

implementation




{ TFhirHTTPClientTests }
(*
class function TFhirHTTPClientTests.LoadResource(filename: String): TFHIRResource;
var
  f : TFileStream;
  prsr : TFHIRJsonParser;
begin
  f := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    prsr := TFHIRJsonParser.Create(nil, 'en');
    try
      prsr.source := f;
      prsr.parse;
      result := prsr.resource.Link;
    finally
      prsr.Free;
    end;
  finally
    f.Free;
  end;
end;

class procedure TFhirHTTPClientTests.testClient(client: TFhirHTTPClient);
var
  conf : TFHIRConformance;
  patient : TFhirPatient;
  id : string;
  ok : boolean;
begin
  client.conformance(true).Free;
  client.conformance(false).Free;
  patient := LoadResource('C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu2\build\publish\patient-example.json') as TFHIRPatient;
  try
    client.createResource(patient, id);
  finally
    patient.free
  end;
  patient := client.readResource(frtPatient, id) as TFHIRPatient;
  try
    patient.deceased := TFHIRDate.Create(NowUTC);
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
  if not ok then
    raise ETestCase.create('test failed');
end;

class procedure TFhirHTTPClientTests.tests(url: String);
var
  client : TFhirHTTPClient;
begin
  client := TFhirHTTPClient.Create(nil, url, true);
  try
    client.UseIndy := true;
    testClient(client);
  finally
    client.free;
  end;
  client := TFhirHTTPClient.Create(nil, url, false);
  try
    client.UseIndy := true;
    testClient(client);
  finally
    client.free;
  end;
  client := TFhirHTTPClient.Create(nil, url, true);
  try
    client.UseIndy := false;
    testClient(client);
  finally
    client.free;
  end;
  client := TFhirHTTPClient.Create(nil, url, false);
  try
    client.UseIndy := false;
    testClient(client);
  finally
    client.free;
  end;
end;

 *)

{ TFhirHTTPClientTests }

procedure TFhirHTTPClientTests.setup;
begin
  FWorker := TTestingWorkerContext.Use;
end;

procedure TFhirHTTPClientTests.teardown;
begin
  FWorker.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TFhirHTTPClientTests);
end.

