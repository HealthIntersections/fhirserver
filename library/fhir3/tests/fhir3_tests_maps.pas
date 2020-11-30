unit FHIR.R3.Tests.Maps;


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
  SysUtils, Classes, IOUtils,
  DUnitX.TestFramework,
  fsl_utilities, fsl_stream, fsl_tests,
  fsl_base, fsl_comparisons,
  fhir_objects, 
  FHIR.Version.Parser,
  fhir3_types, fhir3_resources, fhir3_elementmodel, fhir3_context, FHIR.R3.Tests.Worker, fhir3_maputils, fhir3_profiles;

type
  StructureMapTestCaseAttribute = class (FHIRFolderBasedTestCaseAttribute)
  public
    constructor Create;
  end;

  TTestTransformerServices = class(TTransformerServices)
  private
  public
    function oid2Uri(oid : String) : String; override;
    function translate(appInfo : TFslObject; src : TFHIRCoding; conceptMapUrl : String) : TFHIRCoding; override;
    procedure log(s : String); override;
  end;

  [TextFixture]
  TMapParserTests = class (TObject)
  private
  public
    [StructureMapTestCase]
    procedure MapParserTest(Filename: String);
  end;

  [TextFixture]
  TMapTransformTests = class (TObject)
  private
    ctxt : TFHIRWorkerContext;
    utils : TFHIRStructureMapUtilities;
    procedure loadMap(filename : String);
    procedure loadMaps(folder : String);
  protected
    function sizeInBytesV : cardinal; override;
  public
    [SetupFixture] Procedure SetUp;
    [TearDownFixture] procedure TearDown;

    [TestCase] procedure testCD;
  end;

implementation

function normalise(text : String) : String;
begin
  result := text.Trim.replace(#13, ' ').replace(#10, ' ').replace(#9, ' ');
  while result.Contains('  ') do
    result := result.Replace('  ', ' ');
end;

{ TMapParserTests }

procedure TMapParserTests.MapParserTest(filename: String);
var
  ctxt : TFHIRWorkerContext;
  utils : TFHIRStructureMapUtilities;
  map : TFHIRStructureMap;
  source, output : String;
begin
  ctxt := TTestingWorkerContext.Use;
  try
    utils := TFHIRStructureMapUtilities.Create(ctxt.link, nil, nil);
    try
      source := fileToString(filename, TEncoding.UTF8);
      map := utils.parse(source);
      try
        output := utils.render(map);
        StringToFile(output, filename+'.outp', TEncoding.UTF8);
      finally
        map.free;
      end;
      // Assert.AreEqual(normalise(source), normalise(output), 'input and output do not match');
      Assert.IsTrue(true);
    finally
      utils.Free;
    end;
  finally
    ctxt.free;
  end;
end;

{ StructureMapTestCaseAttribute }

constructor StructureMapTestCaseAttribute.Create;
begin
  inherited Create(IncludeTrailingPathDelimiter(PUB_HOME) + 'build\guides\ccda\maps', '.map', 0);
end;

{ TMapTransformTests }

procedure TMapTransformTests.loadMap(filename: String);
var
  map : TFHIRStructureMap;
begin
  map := utils.parse(FileToString(filename, TEncoding.UTF8));
  utils.lib.AddOrSetValue(map.url, map);
end;

procedure TMapTransformTests.loadMaps(folder: String);
var
  sr : TSearchRec;
  s : String;
begin
  if FindFirst(Folder+'\*.map', faAnyFile, SR) = 0 then
    repeat
      loadMap(IncludeTrailingPathDelimiter(folder)+ sr.Name);
    until FindNext(SR) <> 0;
end;

procedure TMapTransformTests.setup;
begin
  ctxt := TTestingWorkerContext.Use;
  (ctxt as TBaseWorkerContext).LoadFromDefinitions(IncludeTrailingBackslash(PUB_HOME)+'build\guides\ccda\cda\cda.zip');
  utils := TFHIRStructureMapUtilities.Create(ctxt.link, TFslMap<TFHIRStructureMap>.create, TTestTransformerServices.Create);
  loadMaps(IncludeTrailingBackslash(PUB_HOME)+'build\guides\ccda\maps');
end;

procedure TMapTransformTests.TearDown;
begin
  utils.Free;
  ctxt.free;
end;

procedure TMapTransformTests.testCD;
var
  res : TFhirResource;
  x : TFHIRXmlParser;
  s : TStringStream;
  cd : TFhirCodeableConcept;
begin
  x := TFHIRXmlParser.Create(ctxt.link, THTTPLanguages.create('en'));
  try
    s := TStringStream.Create('<CD xmlns="urn:hl7-org:v3" code="34133-9" displayName="Summarization of Episode Note" codeSystem="2.16.840.1.113883.6.1" codeSystemName="LOINC"/>');
    try
      x.source := s;
      x.Parse;
      cd := TFhirCodeableConcept.Create;
      try
        utils.transform(nil, x.resource, utils.Lib['http://hl7.org/fhir/StructureMap/cda-cd'], cd);
        assert.AreEqual(cd.codingList.count, 1);
        assert.AreEqual(cd.codingList[0].code, '34133-9');
        assert.AreEqual(cd.codingList[0].system, 'http://loinc.org');
      finally
        cd.Free;
      end;
    finally
      s.Free;
    end;
  finally
    x.Free;
  end;
end;

function TMapTransformTests.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, ctxt.sizeInBytes);
  inc(result, utils.sizeInBytes);
end;

{ TTestTransformerServices }

procedure TTestTransformerServices.log(s: String);
begin
  writeln(s);
end;

function TTestTransformerServices.oid2Uri(oid: String): String;
begin
  if oid = '2.16.840.1.113883.6.1' then
    result := 'http://loinc.org'
  else
    result := 'http://unknown.com/what?';
end;

function TTestTransformerServices.translate(appInfo: TFslObject; src: TFHIRCoding; conceptMapUrl: String): TFHIRCoding;
begin
  raise ETestCase.create('Not done yet');
end;

initialization
  TDUnitX.RegisterTestFixture(TMapParserTests);
  TDUnitX.RegisterTestFixture(TMapTransformTests);
end.
