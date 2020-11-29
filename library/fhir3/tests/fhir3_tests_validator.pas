unit FHIR.R3.Tests.Validator;


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
  fsl_utilities, fsl_base, fsl_stream, fsl_tests,
  fhir_objects, FHIR.Server.Session, FHIR.Version.Parser, fhir_factory, fhir_common,
  fhir3_context, FHIR.R3.Tests.Worker, fhir3_validator,
  DUnitX.TestFramework;

type
  [TextFixture]
  TFHIRValidatorTests = class (TObject)
  private
    FServices : TFHIRWorkerContext;

    procedure validate(path : String; errorCount : integer; fmt : TFHIRFormat);
//    procedure validateResource(path : String; errorCount : integer; fmt : TFHIRFormat);
//    procedure testBuildPatientExampleB;
  protected
    function sizeInBytesV : cardinal; override;
  public

    [SetupFixture] Procedure SetUp;
    [TearDownFixture] procedure TearDown;

    [TestCase] procedure testXmlListMinimal;
    [TestCase] procedure testXmlListWrongOrder;
    procedure testXmlListWrongCode; // need terminology server for this one
    [TestCase] procedure testXmlListWrongNS;
    [TestCase] procedure testXmlListWrongNS1;
    [TestCase] procedure testXmlListWrongNS2;
    [TestCase] procedure testXmlListEmpty1;
    [TestCase] procedure testXmlListEmpty2;
    [TestCase] procedure testXmlListUnknownAttr;
    [TestCase] procedure testXmlListUnknownElement;
    [TestCase] procedure testXmlListText;
    [TestCase] procedure testXmlListExtension;
    [TestCase] procedure testXmlListXhtml1;
    [TestCase] procedure testXmlListXhtml2;
    [TestCase] procedure testXmlListXhtmlWrongNs1;
    [TestCase] procedure testXmlListXhtmlWrongNs2;
    [TestCase] procedure testXmlListXhtmlWrongNs3;
    [TestCase] procedure testXmlListXhtmlBadElement;
    [TestCase] procedure testXmlListXhtmlBadAttribute;
    [TestCase] procedure testXmlbadSyntax;
    [TestCase] procedure testXmlContained;
    [TestCase] procedure testXmlContainedBad;
    [TestCase] procedure testXmlBundle;
    [TestCase] procedure testXmlGroupOk;
    [TestCase] procedure testXmlGroupGood;
    [TestCase] procedure testXmlGroupBad1;
    [TestCase] procedure testXmlGroupBad2;
    [TestCase] procedure testXmlGroupBad3;
    [TestCase] procedure testXmlGroupEmpty;
    [TestCase] procedure testJsonListMinimal;
    [TestCase] procedure testJsonListWrongOrder;
    procedure testJsonListWrongCode;
    [TestCase] procedure testJsonListEmpty1;
    [TestCase] procedure testJsonListEmpty2;
    [TestCase] procedure testJsonListUnknownProp;
    [TestCase] procedure testJsonListExtension1;
    [TestCase] procedure testJsonListExtension2;
    [TestCase] procedure testJsonListXhtmlCorrect1;
    [TestCase] procedure testJsonListXhtmlCorrect2;
    [TestCase] procedure testJsonListXhtmlBadSyntax;
    [TestCase] procedure testJsonListXhtmlWrongNS1;
    [TestCase] procedure testJsonListXhtmlWrongNS2;
    [TestCase] procedure testJsonListXhtmlBadElement;
    [TestCase] procedure testJsonListXhtmlBadAttribute;
    [TestCase] procedure testJsonbadSyntax;
    [TestCase] procedure testJsonContained;
    [TestCase] procedure testJsonContainedBad;
    [TestCase] procedure testJsonBundle;
    [TestCase] procedure testJsonGroupOk;
    [TestCase] procedure testJsonGroupTiny;
    [TestCase] procedure testJsonGroupGood;
    [TestCase] procedure testJsonGroupBad1;
    [TestCase] procedure testJsonGroupBad2;
    [TestCase] procedure testJsonGroupBad3;
    [TestCase] procedure testJsonGroupEmpty;
    [TestCase] procedure testJsonListXhtmlXXE;
    [TestCase] procedure testParametersReference;
    procedure testXmlListXXE;
    [TestCase] procedure testXmlListXXE2;

  end;

implementation

uses
  SysUtils, Classes,
  fhir_parser, FHIR.Version.Types, FHIR.Version.Resources;

{ TFHIRValidatorTests }

procedure TFHIRValidatorTests.setup;
begin
  FServices := TTestingWorkerContext.Use;
end;

procedure TFHIRValidatorTests.TearDown;
begin
  FServices.Free;
end;

procedure TFHIRValidatorTests.validate(path: String; errorCount: integer; fmt : TFHIRFormat);
var
  src : TFslBuffer;
//  val : TFHIRValidator;
  ctxt : TFHIRValidatorContext;
  ec : integer;
  msg : TFhirOperationOutcomeIssueW;
begin
  src := TFslBuffer.Create;
  try
    src.LoadFromFileName(FHIR_PUB_FILE(path));
    ctxt := TFHIRValidatorContext.Create;
    try
      ctxt.ResourceIdRule := risOptional;
//      val := TFHIRValidator.Create(FServices.link);
//      try
//        val.validate(ctxt, src, fmt);
//      finally
//        val.Free;
//      end;
      ec := 0;
      for msg in ctxt.Issues do
      begin
        if msg.severity in [isFatal, isError] then
        begin
//        if msg.locationList.count = 1 then
//          System.writeln('Error @ '+ msg.locationList[0].value+': '+msg.details.text)
//        else
//          System.writeln('Error @ unknown: '+msg.details.text);
        inc(ec);
        end;
      end;
      Assert.areEqual(errorCount, ec, StringFormat('Expected %d errors, but found %d', [errorCount, ec]));
    finally
      ctxt.Free;
    end;
  finally
    src.Free;
  end;
end;


//procedure TFHIRValidatorTests.validateResource(path: String; errorCount: integer; fmt: TFHIRFormat);
//var
//  p : TFHIRParser;
//  f : TFilestream;
////  val : TFHIRValidator;
//  ctxt : TFHIRValidatorContext;
//  ec : integer;
//  msg : TFhirOperationOutcomeIssueW;
//  s : string;
//begin
//  if (fmt = ffXml) then
//    p := TFHIRXmlParser.Create(nil, THTTPLanguages.create('en'))
//  else
//    p := TFHIRJsonParser.Create(nil, THTTPLanguages.create('en'));
//  try
//    f := TFilestream.create(fsl_utilities.path([PUB_HOME, path]), fmOpenRead + fmShareDenywrite);
//    try
//      p.source := f;
//      p.Parse;
//
//      ctxt := TFHIRValidatorContext.Create;
//      try
//        ctxt.ResourceIdRule := risOptional;
////        val := TFHIRValidator.Create(FServices.link);
////        try
////          val.validate(ctxt, p.resource);
////        finally
////          val.Free;
////        end;
//        ec := 0;
//        s := '';
//        for msg in ctxt.Issues do
//          if msg.severity in [isFatal, isError] then
//          begin
//            inc(ec);
//            s := s + msg.display;
//          end;
//        Assert.areEqual(errorCount, ec, StringFormat('Expected %d errors, but found %d: %s', [errorCount, ec, s]));
//      finally
//        ctxt.Free;
//      end;
//
//    finally
//      f.free;
//    end;
//  finally
//    p.Free;
//  end;
//end;
//
procedure TFHIRValidatorTests.testXmlListMinimal;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-minimal.xml']), 0, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListWrongOrder;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-wrong-order.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListWrongCode;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-wrong-code.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListWrongNS;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-wrong-ns.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListWrongNS1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-wrong-ns1.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListWrongNS2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-wrong-ns2.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListEmpty1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-empty1.xml']), 3, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListEmpty2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-empty2.xml']), 3, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListUnknownAttr;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-unknown-attr.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListUnknownElement;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-unknown-element.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListText;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-text.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListExtension;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-extension.xml']), 0, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListXhtml1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-correct1.xml']), 0, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListXhtml2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-correct2.xml']), 0, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListXXE;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-xxe1.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListXXE2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-xxe2.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListXhtmlWrongNs1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-wrongns1.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListXhtmlWrongNs2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-wrongns2.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListXhtmlWrongNs3;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-wrongns3.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListXhtmlBadElement;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-element.xml']), 2, ffXml);
end;

procedure TFHIRValidatorTests.testXmlListXhtmlBadAttribute;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-attribute.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlbadSyntax;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-bad-syntax.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlContained;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-contained.xml']), 0, ffXml);
end;

procedure TFHIRValidatorTests.testXmlContainedBad;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-contained-bad.xml']), 2, ffXml);
end;

procedure TFHIRValidatorTests.testXmlBundle;
begin
  validate(path(['build', 'tests', 'validation-examples', 'bundle-good.xml']), 0, ffXml);
end;

procedure TFHIRValidatorTests.testXmlGroupOk;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-minimal.xml']), 0, ffXml);
end;

procedure TFHIRValidatorTests.testXmlGroupGood;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-good.xml']), 0, ffXml);
end;

procedure TFHIRValidatorTests.testXmlGroupBad1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-bad1.xml']), 2, ffXml);
end;

procedure TFHIRValidatorTests.testXmlGroupBad2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-bad2.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlGroupBad3;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-bad3.xml']), 1, ffXml);
end;

procedure TFHIRValidatorTests.testXmlGroupEmpty;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-empty.xml']), 2, ffXml);
end;

procedure TFHIRValidatorTests.testParametersReference;
begin
  validate(path(['build', 'tests', 'validation-examples', 'params-reference.xml']), 0, ffXml);
end;


// --- json --------------------------------------------------------------------------

procedure TFHIRValidatorTests.testJsonListMinimal;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-minimal.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListWrongOrder;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-wrong-order.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListWrongCode;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-wrong-code.json']), 1, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListEmpty1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-empty1.json']), 3, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListEmpty2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-empty2.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListUnknownProp;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-unknown-prop.json']), 1, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListExtension1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-extension1.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListExtension2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-extension2.json']), 1, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListXhtmlCorrect1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-correct1.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListXhtmlCorrect2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-correct2.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListXhtmlXXE;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-xxe.json']), 1, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListXhtmlBadSyntax;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-syntax.json']), 1, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListXhtmlWrongNS1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-wrongns1.json']), 1, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListXhtmlWrongNS2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-wrongns2.json']), 1, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListXhtmlBadElement;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-element.json']), 2, ffJson);
end;

procedure TFHIRValidatorTests.testJsonListXhtmlBadAttribute;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-xhtml-attribute.json']), 1, ffJson);
end;

procedure TFHIRValidatorTests.testJsonbadSyntax;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-bad-syntax.json']), 1, ffJson);
end;

procedure TFHIRValidatorTests.testJsonContained;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-contained.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonContainedBad;
begin
  validate(path(['build', 'tests', 'validation-examples', 'list-contained-bad.json']), 2, ffJson);
end;

procedure TFHIRValidatorTests.testJsonBundle;
begin
  validate(path(['build', 'tests', 'validation-examples', 'bundle-good.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonGroupOk;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-minimal.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonGroupTiny;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-minimal-tiny.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonGroupGood;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-good.json']), 0, ffJson);
end;

procedure TFHIRValidatorTests.testJsonGroupBad1;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-bad1.json']), 2, ffJson);
end;

procedure TFHIRValidatorTests.testJsonGroupBad2;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-bad2.json']), 2, ffJson);
end;

procedure TFHIRValidatorTests.testJsonGroupBad3;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-bad3.json']), 2, ffJson);
end;

procedure TFHIRValidatorTests.testJsonGroupEmpty;
begin
  validate(path(['build', 'tests', 'validation-examples', 'group-choice-empty.json']), 1, ffJson);
end;

//procedure TFHIRValidatorTests.testBuildPatientExampleB;
//begin
//  validate(path(['publish', 'patient-example-b.xml']), 0, ffJson);
//end;
//
initialization
  if FindCmdLineSwitch('dev') then
  TDUnitX.RegisterTestFixture(TFHIRValidatorTests);
function TFHIRValidatorTests.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FServices.sizeInBytes);
end;

end.
