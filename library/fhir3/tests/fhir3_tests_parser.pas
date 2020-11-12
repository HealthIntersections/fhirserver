unit FHIR.R3.Tests.Parser;


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


interface

uses
  Windows, SysUtils, Classes, System.IOUtils,
  DUnitX.TestFramework,
  fsl_stream,
  fhir_objects, fhir3_parser, fhir_parser,
  fhir3_resources, fhir3_elementmodel, fhir3_context, FHIR.R3.Tests.Worker,
  fsl_tests, fsl_comparisons;

type
  FHIRParserTestCaseAttribute = class (FHIRFolderBasedTestCaseAttribute)
  public
    constructor Create;
  end;

  [TextFixture]
  TFHIRParserTests = class (TObject)
  private
  public
    [FHIRParserTestCase]
    procedure RoundTripTest(Filename: String);
  end;

implementation

uses
  IdGlobalProtocols;

{ TFHIRParserTests }

procedure TFHIRParserTests.RoundTripTest(filename: String);
var
  r : TFHIRResource;
  fn, j1, j2, x1, x2 : String;
  b : boolean;
  msg : String;
  re : TFHIRMMElement;
  ctxt : TFHIRWorkerContext;
begin
//  AllocConsole;
  r := TFHIRParsers3.parseFile(nil, ffXml, THTTPLanguages.create('en'), filename);
  try
    Assert.IsNotNull(r, 'Resource could not be loaded');
    fn := MakeTempFilename();
    try
      TFHIRParsers3.composeFile(nil, ffXml, r, THTTPLanguages.create('en'), fn, OutputStylePretty);
      b := CheckXMLIsSame(filename, fn, msg);
      assert.IsTrue(b, msg);
    finally
      DeleteFile(fn);
    end;
    j1 := MakeTempFilename();
    TFHIRParsers3.composeFile(nil, ffJson, r, THTTPLanguages.create('en'), j1, OutputStylePretty);
  finally
    r.Free;
  end;

  ctxt := TTestingWorkerContext.Use;
  try
    re := TFHIRMMManager.parseFile(ctxt, filename, ffXml);
    try
      Assert.IsNotNull(re, 'Resource could not be loaded');
      fn := MakeTempFilename();
      try
        TFHIRMMManager.composeFile(ctxt, re, fn, ffXml, false);
        b := CheckXMLIsSame(filename, fn, msg);
        assert.IsTrue(b, msg);
      finally
        DeleteFile(fn);
      end;
      j2 := MakeTempFilename();
      TFHIRMMManager.composeFile(ctxt, re, j2, ffJson, false);
    finally
      re.Free;
    end;
  finally
    ctxt.free;
  end;

  b := CheckJsonIsSame(j1, j2, msg);
  assert.IsTrue(b, msg);

  // ok, we've produced equivalent JSON by both methods.
  // now, we're going to reverse the process
  r := TFHIRParsers3.parseFile(nil, ffJson, THTTPLanguages.create('en'), j2); // crossover too
  try
    Assert.IsNotNull(r, 'Resource could not be loaded');
    fn := MakeTempFilename();
    try
      TFHIRParsers3.composeFile(nil, ffJson, r, THTTPLanguages.create('en'), fn, OutputStyleNormal);
      b := CheckJsonIsSame(j2, fn, msg);
      assert.IsTrue(b, msg);
    finally
      DeleteFile(fn);
    end;
    x1 := MakeTempFilename();
    TFHIRParsers3.composeFile(nil, ffXml, r, THTTPLanguages.create('en'), x1, OutputStyleNormal);
  finally
    r.Free;
  end;

  ctxt := TTestingWorkerContext.Use;
  try
    re := TFHIRMMManager.parseFile(ctxt, j1, ffJson);
    try
      Assert.IsNotNull(re, 'Resource could not be loaded');
      fn := MakeTempFilename();
      try
        TFHIRMMManager.composeFile(ctxt, re, fn, ffJson, false);
        b := CheckJsonIsSame(j1, fn, msg);
        assert.IsTrue(b, msg);
      finally
        DeleteFile(fn);
      end;
      x2 := MakeTempFilename();
      TFHIRMMManager.composeFile(ctxt, re, x2, ffXml, false);
    finally
      re.Free;
    end;
  finally
    ctxt.free;
  end;

  b := CheckXMLIsSame(x1, x2, msg);
  assert.IsTrue(b, msg);

  b := CheckXMLIsSame(filename, x1, msg);
  assert.IsTrue(b, msg);
end;

{ FHIRParserTestCaseAttribute }

constructor FHIRParserTestCaseAttribute.Create;
begin
  inherited Create(IncludeTrailingPathDelimiter(PUB_HOME) + 'build\publish\examples', '.xml', 20);
end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRParserTests);
end.
