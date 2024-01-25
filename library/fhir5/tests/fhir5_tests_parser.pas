unit fhir5_tests_parser;

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
  SysUtils, Classes, {$IFDEF DELPHI} IOUtils, {$ENDIF}
  fsl_testing,
  IdGlobalProtocols,
  fsl_utilities, fsl_stream, fsl_tests, fsl_comparisons, fsl_http, fsl_fpc,
  fhir_objects, fhir5_parser, fhir_parser,
  fhir5_resources, fhir5_elementmodel, fhir5_context, fhir5_tests_worker;

type
  TFHIRR5ParserTestCase = class (TFslTestSuiteCase)
  public
    procedure TestCase(name : String); override;
  end;

  TFHIRParser5SpecialTests = class (TFslTestCase)
  published
    procedure DecimalTest;
  end;

procedure registerTests;

implementation


{ TFHIRR5ParserTestCase }

procedure TFHIRR5ParserTestCase.TestCase(name: String);
var
  r : TFHIRResource;
  fn, j1, j2, x1, x2 : String;
  b : boolean;
  msg : String;
  re : TFHIRMMElement;
  ctxt : TFHIRWorkerContext;
begin
//  AllocConsole;
  r := TFHIRParsers5.parseFile(nil, ffXml, nil, filename);
  try
    assertTrue(r <> nil, 'Resource could not be loaded');
    fn := MakeTempFilename();
    try
      TFHIRParsers5.composeFile(nil, ffXml, r, nil, fn, OutputStylePretty);
      b := CheckXMLIsSame(filename, fn, msg);
      assertTrue(b, msg);
    finally
      DeleteFile(fn);
    end;
    j1 := MakeTempFilename();
    TFHIRParsers5.composeFile(nil, ffJson, r, nil, j1, OutputStylePretty);
  finally
    r.free;
  end;

  ctxt := TTestingWorkerContext5.Use;
  try
    re := TFHIRMMManager.parseFile(ctxt, filename, ffXml);
    try
      assertTrue(re <> nil, 'Resource could not be loaded');
      fn := MakeTempFilename();
      try
        TFHIRMMManager.composeFile(ctxt, re, fn, ffXml, OutputStylePretty);
        b := CheckXMLIsSame(filename, fn, msg);
        assertTrue(b, msg);
      finally
        DeleteFile(fn);
      end;
      j2 := MakeTempFilename();
      TFHIRMMManager.composeFile(ctxt, re, j2, ffJson, OutputStylePretty);
    finally
      re.free;
    end;
  finally
    ctxt.free;
  end;

  b := CheckJsonIsSame(j1, j2, msg);
  assertTrue(b, msg);

  // ok, we've produced equivalent JSON by both methods.
  // now, we're going to reverse the process
  r := TFHIRParsers5.parseFile(nil, ffJson, nil, j2); // crossover too
  try
    assertTrue(r <> nil, 'Resource could not be loaded');
    fn := MakeTempFilename();
    try
      TFHIRParsers5.composeFile(nil, ffJson, r, nil, fn, OutputStyleNormal);
      b := CheckJsonIsSame(j2, fn, msg);
      assertTrue(b, msg);
    finally
      DeleteFile(fn);
    end;
    x1 := MakeTempFilename();
    TFHIRParsers5.composeFile(nil, ffXml, r, nil, x1, OutputStyleNormal);
  finally
    r.free;
  end;

  ctxt := TTestingWorkerContext5.Use;
  try
    re := TFHIRMMManager.parseFile(ctxt, j1, ffJson);
    try
      assertTrue(re <> nil, 'Resource could not be loaded');
      fn := MakeTempFilename();
      try
        TFHIRMMManager.composeFile(ctxt, re, fn, ffJson, OutputStylePretty);
        b := CheckJsonIsSame(j1, fn, msg);
        assertTrue(b, msg);
      finally
        DeleteFile(fn);
      end;
      x2 := MakeTempFilename();
      TFHIRMMManager.composeFile(ctxt, re, x2, ffXml, OutputStylePretty);
    finally
      re.free;
    end;
  finally
    ctxt.free;
  end;

  b := CheckXMLIsSame(x1, x2, msg);
  assertTrue(b, msg);

  b := CheckXMLIsSame(filename, x1, msg);
  assertTrue(b, msg);

(*  r := TFHIRParsers.parseFile(nil, ffTurtle, nil, filename);
  try
    Assert.IsNotNull(r, 'Resource could not be loaded');
    fn := MakeTempFilename();
    try
      TFHIRParsers.composeFile(nil, ffTurtle, r, nil, fn, OutputStylePretty);
      b := CheckTurtleIsSame(filename, fn, msg);
      assertTrue(b, msg);
    finally
      DeleteFile(fn);
    end;
    x1 := MakeTempFilename();
    TFHIRParsers.composeFile(nil, ffXml, r, nil, x1, OutputStyleNormal);
  finally
    r.free;
  end;

  b := CheckXMLIsSame(x1, x2, msg);
  assertTrue(b, msg);*)
end;


{ TFHIRParser5SpecialTests }

procedure TFHIRParser5SpecialTests.DecimalTest;
var
  src, xml, json, json2 : String;
  obs : TFhirObservation;
begin
  src := FilePath(['[tmp]', 'obs.xml']);
  xml := FilePath(['[tmp]', 'xml.xml']);
  json := FilePath(['[tmp]', 'json.json']);
  json2 := FilePath(['[tmp]', 'json2.json']);
  if FileExists(src) then
    TFile.Delete(src);
  TFile.Copy(TestSettings.fhirTestFile(['r5', 'examples', 'observation-decimal.xml']), src, false);
  obs := TFHIRParsers5.ParseFile(nil, ffXml, nil, src) as TFhirObservation;
  try
    TFHIRParsers5.composeFile(nil, ffJson, obs, nil, json, OutputStylePretty);
  finally
    obs.free;
  end;
  obs := TFHIRParsers5.ParseFile(nil, ffJson, nil, json) as TFhirObservation;
  try
    TFHIRParsers5.composeFile(nil, ffJson, obs, nil, json2, OutputStylePretty);
    TFHIRParsers5.composeFile(nil, ffXml, obs, nil, xml, OutputStylePretty);
  finally
    obs.free;
  end;
  assertTrue(true);
end;

procedure registerTests;
begin
  registerTest('R5.Parser', TFslTestSuite.Create(TestSettings.fhirTestFile(['r5', 'examples']), '.xml', 20, TFHIRR5ParserTestCase));
  registerTest('R5.Parser', TFHIRParser5SpecialTests.Suite);
end;

end.
