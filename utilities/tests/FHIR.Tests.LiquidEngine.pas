unit FHIR.Tests.LiquidEngine;

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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  FHIR.Support.Base,
  FHIR.R4.Resources, FHIR.R4.Parser, FHIR.R4.Liquid, FHIR.R4.PathEngine, FHIR.R4.Xml,
  DUnitX.TestFramework, FHIR.Support.Tests, FHIR.R4.Tests.Worker;

type
  [TextFixture]
  TLiquidEngineTests = Class (TObject)
  private
    patient : TFHIRPatient;
    engine : TFHIRLiquidEngine;
    pages : TDictionary<String, String>;
    procedure checkProcess(source, expected : String);
    function FetchInclude(sender : TFHIRLiquidEngine; name : String; var content : String) : boolean;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [TestCase] Procedure testConstant;
    [TestCase] Procedure testSimple;
    [TestCase] Procedure testSimple1;
    [TestCase] Procedure testSimple2;
    [TestCase] Procedure testMix1;
    [TestCase] Procedure testIf;
    [TestCase] Procedure testLoop;
    [TestCase] procedure testInclude;
  End;

implementation

function TLiquidEngineTests.FetchInclude(sender : TFHIRLiquidEngine; name: String; var content: String): boolean;
begin
  result := pages.TryGetValue(name, content);
end;

procedure TLiquidEngineTests.Setup;
var
  p : TFHIRXmlParser;
  f : TFileStream;
begin
  pages := TDictionary<String, String>.create;
  engine := TFHIRLiquidEngine.Create(TFHIRPathEngine.Create(TTestingWorkerContext.Use, nil));
  engine.OnFetchInclude := FetchInclude;
  p := TFHIRXmlParser.create(TTestingWorkerContext.Use, THTTPLanguages.create('en'));
  try
    f := TFileStream.Create(FHIR_PUB_FILE('patient-example.xml'), fmOpenRead);
    try
      p.source := f;
      p.parse;
      patient := p.resource.Link as TFHIRPatient;
    finally
      f.Free;
    end;
  finally
    p.Free;
  end;
end;

procedure TLiquidEngineTests.TearDown;
begin
  patient.free;
  engine.Free;
  pages.free;
end;

procedure TLiquidEngineTests.testConstant();
begin
  checkProcess('test', 'test');
end;

procedure TLiquidEngineTests.testSimple();
begin
  checkProcess('{{ Patient.id}}', 'example');
end;

procedure TLiquidEngineTests.testSimple1();
begin
  checkProcess('{{ Patient.id }}', 'example');
end;

procedure TLiquidEngineTests.testSimple2();
begin
  checkProcess('{{Patient.id}}', 'example');
end;

procedure TLiquidEngineTests.testMix1();
begin
  checkProcess('t{{Patient.id}}t', 'texamplet');
end;

procedure TLiquidEngineTests.testIf();
begin
  checkProcess('{% if Patient.id = ''example''%} yes {%else%} no {%endif%}', ' yes ');
end;

procedure TLiquidEngineTests.testLoop();
begin
  checkProcess('{%loop name in Patient.name%}{{name.family}}{%endloop%}', 'ChalmersWindsor');
end;

procedure TLiquidEngineTests.testInclude();
begin
  pages.AddOrSetValue('humanname.html', '{{include.name.family}}');
  checkProcess('{%loop name in Patient.name%}{%include humanname.html name=name pat=''patient'' %}{%endloop%}', 'ChalmersWindsor');
end;

procedure TLiquidEngineTests.checkProcess(source, expected : String);
var
  doc : TFHIRLiquidDocument;
  output : String;
begin
  doc := engine.parse(source, 'test-script');
  try
    output := engine.evaluate(doc, patient, nil);
    Assert.IsTrue(expected = output);
  finally
    doc.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLiquidEngineTests);
end.
