unit fhir4_tests_Objects;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$I fhir.inc}

interface

uses
  SysUtils, Classes,
  IdSSLOpenSSLHeaders,
  {$IFDEF FPC} FPCUnit, TestRegistry, {$ELSE} DUnitX.TestFramework, {$ENDIF}
  FHIR.Support.Certs, fsl_stream, fsl_tests, fsl_http,
  fhir_objects, FHIR.Version.Parser,
  fhir4_types, fhir4_resources, fhir4_json;

{$IFNDEF FPC}
type
  [TextFixture]
  TFHIRObjectTests4 = Class (TObject)
  private
    function json(o : TFHIRResource) : String;
  public
    [TestCase] Procedure TestDropEmptySimple;
    [TestCase] Procedure TestDropEmptyComplex;
  end;
{$ENDIF}

implementation

{$IFNDEF FPC}

{ TFHIRObjectTests4 }

function TFHIRObjectTests4.json(o: TFHIRResource): String;
var
  c : TFHIRJsonComposer;
begin
  c := TFHIRJsonComposer.Create(nil, OutputStyleCanonical, THTTPLanguages.create('en'));
  try
    result := c.Compose(o);
  finally
    c.Free;
  end;
end;

procedure TFHIRObjectTests4.TestDropEmptySimple;
var
  o : TFHIRPatient;
begin
  o := TFHIRPatient.Create;
  try
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.idElement = nil);
    o.id := 'test';
    Assert.IsTrue(json(o) = '{"id":"test","resourceType":"Patient"}');
    Assert.IsTrue(o.idElement <> nil);
    o.id := '';
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.idElement <> nil);
    o.dropEmpty;
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.idElement = nil);
  finally
    o.Free;
  end;
  Assert.IsTrue(true);
end;

procedure TFHIRObjectTests4.TestDropEmptyComplex;
var
  o : TFHIRPatient;
begin
  o := TFHIRPatient.Create;
  try
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.identifierList.Count = 0);
    o.identifierList.Append.value := 'test';
    Assert.IsTrue(json(o) = '{"identifier":[{"value":"test"}],"resourceType":"Patient"}');
    Assert.IsTrue(o.identifierList.Count = 1);
    o.identifierList[0].value := '';
    Assert.IsTrue(json(o) = '{"identifier":[{}],"resourceType":"Patient"}');
    Assert.IsTrue(o.identifierList.Count = 1);
    o.dropEmpty;
    Assert.IsTrue(json(o) = '{"resourceType":"Patient"}');
    Assert.IsTrue(o.identifierList.Count = 0);
  finally
    o.Free;
  end;
  Assert.IsTrue(true);
end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRObjectTests4);
{$ENDIF}
end.
