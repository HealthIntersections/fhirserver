unit fhir4_tests_diff;

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
  SysUtils, Classes,
  fsl_base, fsl_testing, fsl_utilities, fsl_stream, fsl_tests,
  fsl_http,
  fhir_objects, fhir_parser, fhir_factory, fhir_common,
  fhir4_tests_worker, fhir4_resources, fhir4_types, fhir4_factory, fhir4_common, fhir4_parser, fhir4_xml,
  fhir_diff,
  fsl_xml;

type
  TFHIRDifferenceTestContext = class (TFslObject)
  private
    engine : TDifferenceEngine;
    delta : TFhirParametersW;
    input: TFhirResource;
    diff: TFhirParameters;
    output : TFhirResource;
  end;

  TFHIRDifferenceTest4 = Class (TFslTestSuiteCase)
  Private
    FTest : TMXmlElement;

    function parseResource(elem : TMXmlElement) : TFhirResource;
    function AsXml(res : TFHIRResource) : String;
    procedure CompareXml(name, mode : String; expected, obtained : TFHIRResource);
    procedure execCase(mode : String; input : TFhirResource; diff : TFhirParameters; output : TFhirResource);
    procedure execCaseError(mode : String; input : TFhirResource; diff : TFhirParameters; msg : string);
    procedure execGenDiffError(context : TObject);
    procedure execApplyDiffError(context : TObject);
  Public
    constructor Create(test : TMXmlElement);
    destructor Destroy; override;
  Published
    procedure TestCase(Name : String); override;
  End;


  TFHIRDifferenceTests4 = class (TFslTestSuite)
  private
  public
    constructor Create; override;
  end;

procedure registerTests;

implementation

procedure registerTests;
begin
  RegisterTest('R4', TFHIRDifferenceTests4.create);
end;

{ TFHIRDifferenceTests4 }

constructor TFHIRDifferenceTests4.Create;
var
  test : TMXmlElement;
  tests : TMXmlElement;
begin
  inherited;

  tests := TMXmlParser.ParseFile(TestSettings.fhirTestFile(['r4', 'patch', 'fhir-path-tests.xml']), [xpResolveNamespaces]);
  try
    test := tests.document.firstElement;
    while test <> nil do
    begin
      AddTest(TFHIRDifferenceTest4.create(test.Link));
      test := test.nextElement;
    end;
  finally
    tests.free;
  end;
end;

{ TFHIRDifferenceTest4 }

constructor TFHIRDifferenceTest4.Create(test: TMXmlElement);
begin
  inherited Create(test.attribute['name']);
  FTest := test;
end;

function TFHIRDifferenceTest4.AsXml(res: TFHIRResource): String;
var
  p : TFHIRXmlComposer;
  s : TStringStream;
begin
  p := TFHIRXmlComposer.Create(nil, OutputStylePretty, THTTPLanguages.create('en'));
  try
    s := TStringStream.Create;
    try
      p.Compose(s, res);
      result := s.DataString;
    finally
      s.Free;
    end;
  finally
    p.Free;
  end;
end;

procedure TFHIRDifferenceTest4.CompareXml(name, mode: String; expected, obtained: TFHIRResource);
var
  e, o : String;
begin
  e := asXml(expected);
  o := asXml(obtained);
  StringToFile(e, filePath(['[tmp]', 'expected.xml']), TEncoding.UTF8);
  StringToFile(o, filePath(['[tmp]', 'obtained.xml']), TEncoding.UTF8);
  assertTrue(e = o, mode+' does not match for '+name);
end;

destructor TFHIRDifferenceTest4.Destroy;
begin
  FTest.free;
  inherited;
end;

procedure TFHIRDifferenceTest4.execApplyDiffError(context: TObject);
var
  ctxt : TFHIRDifferenceTestContext;
  w : TFhirParametersW;
begin
  ctxt := context as TFHIRDifferenceTestContext;

  w := TFhirParameters4.create(ctxt.diff.link);
  try
    ctxt.engine.applyDifference(ctxt.input, w);
  finally
    w.free;
  end;
end;

procedure TFHIRDifferenceTest4.execCase(mode: String; input: TFhirResource; diff: TFhirParameters; output: TFhirResource);
var
  engine : TDifferenceEngine;
  delta : TFhirParametersW;
  outcome : TFhirResource;
  html : String;
  w : TFhirParametersW;
begin
  if (mode = 'both') or (mode = 'reverse') then
  begin
    engine := TDifferenceEngine.Create(TTestingWorkerContext4.Use, TFHIRFactoryR4.create);
    try
      delta := engine.generateDifference(input, output, html);
      try
        compareXml(TestName, 'Difference', diff, delta.Resource as TFHIRParameters);
      finally
        delta.Free;
      end;
    finally
      engine.free;
    end;
  end;

  if (mode = 'both') or (mode = 'forwards') then
  begin
    engine := TDifferenceEngine.Create(TTestingWorkerContext4.Use, TFHIRFactoryR4.create);
    try
      w := TFhirParameters4.create(diff.link);
      try
        outcome := engine.applyDifference(input, w) as TFhirResource;
        try
          compareXml(TestName, 'Output', output, outcome);
        finally
          outcome.Free;
        end;
      finally
        w.free;
      end;
    finally
      engine.free;
    end;
  end;
end;

procedure TFHIRDifferenceTest4.execCaseError(mode: String; input: TFhirResource; diff: TFhirParameters; msg: string);
var
  context : TFHIRDifferenceTestContext;
  html : String;
  w : TFhirParametersW;
begin
  if (mode = 'both') or (mode = 'reverse') then
  begin
    context := TFHIRDifferenceTestContext.Create;
    try
      context.input := input;
      context.engine := TDifferenceEngine.Create(TTestingWorkerContext4.Use, TFHIRFactoryR4.create);
      try
        assertWillRaise(execGenDiffError, context, EFHIRException, msg);
      finally
        context.engine.free;
      end;
    finally
      context.Free;
    end;
  end;

  if (mode = 'both') or (mode = 'forwards') then
  begin
    context := TFHIRDifferenceTestContext.Create;
    try
      context.input := input;
      context.diff := diff;
      context.engine := TDifferenceEngine.Create(TTestingWorkerContext4.Use, TFHIRFactoryR4.create);
      try
        context.engine := TDifferenceEngine.Create(TTestingWorkerContext4.Use, TFHIRFactoryR4.create);
        assertWillRaise(execApplyDiffError, context, EFHIRException, msg);
      finally
        context.engine.free;
      end;
    finally
      context.Free;
    end;
  end;
end;

procedure TFHIRDifferenceTest4.execGenDiffError(context: TObject);
var
  ctxt : TFHIRDifferenceTestContext;
  w : TFhirParametersW;
  html : String;
begin
  ctxt := context as TFHIRDifferenceTestContext;
  ctxt.engine.generateDifference(ctxt.input, ctxt.output, html);
end;

function TFHIRDifferenceTest4.parseResource(elem: TMXmlElement): TFhirResource;
var
  p : TFHIRXmlParser;
begin
  p := TFHIRXmlParser.Create(nil, THTTPLanguages.create('en'));
  try
    p.Element := elem.firstElement.Link;
    p.Parse;
    result := p.resource.Link as TFHIRResource;
  finally
    p.Free;
  end;
end;

procedure TFHIRDifferenceTest4.TestCase(Name: String);
var
  input, output : TFhirResource;
  diff : TFhirParameters;
begin
  input := parseResource(FTest.element('input'));
  try
    diff := parseResource(FTest.element('diff')) as TFHIRParameters;
    try
      if FTest.element('error') <> nil then
      begin
        execCaseError(FTest.attribute['mode'], input, diff, FTest.element('error').attribute['msg']);
      end
      else
      begin
        output := parseResource(FTest.element('output'));
        try
          execCase(FTest.attribute['mode'], input, diff, output);
        finally
          output.free;
        end;
      end;
    finally
      diff.Free;
    end;
  finally
    input.Free;
  end;
end;

end.
