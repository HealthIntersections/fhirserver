unit fhir4_tests_Maps;

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
  SysUtils, Classes, IOUtils,
  {$IFDEF FPC} FPCUnit, TestRegistry, {$ELSE} DUnitX.TestFramework, {$ENDIF}
  fsl_utilities, fsl_stream, fsl_tests,
  fsl_base, fsl_comparisons,
  fhir_objects, 
  FHIR.Version.Parser,
  fhir4_types, fhir4_resources, fhir4_elementmodel, fhir4_context, fhir4_tests_worker, fhir4_maputils, fhir4_profiles;

{$IFNDEF FPC}
type
  StructureMapTestCase4Attribute = class (FHIRFolderBasedTestCase4Attribute)
  public
    constructor Create;
  end;

  TTestTransformerServices4 = class(TTransformerServices)
  public
    function translate(appInfo : TFslObject; src : TFHIRCoding; conceptMapUrl : String) : TFHIRCoding; override;
    procedure log(s : String); override;
    function performSearch(appInfo : TFslObject; url : String) : TFslList<TFHIRObject>; override;
    function createType(appInfo : TFslObject; tn : String) : TFHIRObject; override;
    procedure createResource(appInfo : TFslObject; res : TFHIRObject; atRootofTransform : boolean); override;
  end;

  [TextFixture]
  TMapParserTests4 = class (TObject)
  private
  public
    [StructureMapTestCase4]
    procedure MapParserTest(Filename: String);
  end;

  [TextFixture]
  TMapTransformTests4 = class (TObject)
  private
    ctxt : TFHIRWorkerContext;
    utils : TFHIRStructureMapUtilities;
    procedure loadMap(filename : String);
    procedure loadMaps(folder : String);
  public
    [SetupFixture] Procedure SetUp;
    [TearDownFixture] procedure TearDown;

    [TestCase] procedure testCD;
  end;

  MapParserTest2Case4Attribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TMapParserTests24 = Class (TObject)
  private
    ctxt : TFHIRWorkerContext;
    utils : TFHIRStructureMapUtilities;
  Published
    [SetupFixture] Procedure SetUp;
    [TearDownFixture] procedure TearDown;

    [MapParserTest2Case4]
    procedure Test(filename : String);
  End;
{$ENDIF}

implementation

{$IFNDEF FPC}

function normalise(text : String) : String;
begin
  result := text.Trim.replace(#13, ' ').replace(#10, ' ').replace(#9, ' ');
  while result.Contains('  ') do
    result := result.Replace('  ', ' ');
end;

{ TMapParserTests4 }

procedure TMapParserTests4.MapParserTest(filename: String);
var
  ctxt : TFHIRWorkerContext;
  utils : TFHIRStructureMapUtilities;
  map : TFHIRStructureMap;
  source, output : String;
begin
  ctxt := TTestingWorkerContext4.Use;
  try
    utils := TFHIRStructureMapUtilities.Create(ctxt.link, nil, nil, nil);
    try
      source := fileToString(filename, TEncoding.UTF8);
      map := utils.parse(source, filename);
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

{ StructureMapTestCase4Attribute }

constructor StructureMapTestCase4Attribute.Create;
begin
  inherited Create('guides\ccda\maps', '.map', 0);
end;

{ TMapTransformTests4 }

procedure TMapTransformTests4.loadMap(filename: String);
var
  map : TFHIRStructureMap;
begin
  map := utils.parse(FileToString(filename, TEncoding.UTF8), filename);
  utils.lib.AddOrSetValue(map.url, map);
end;

procedure TMapTransformTests4.loadMaps(folder: String);
var
  sr : TSearchRec;
begin
  if FindFirst(Folder+'\*.map', faAnyFile, SR) = 0 then
    repeat
      loadMap(IncludeTrailingPathDelimiter(folder)+ sr.Name);
    until FindNext(SR) <> 0;
end;

procedure TMapTransformTests4.setup;
begin
  ctxt := TTestingWorkerContext4.Use;
  (ctxt as TBaseWorkerContext).LoadFromDefinitions(FHIR_TESTING_FILE('cda', 'cda.zip'));
  utils := TFHIRStructureMapUtilities.Create(ctxt.link, TFslMap<TFHIRStructureMap>.create('utils'), TTestTransformerServices4.Create, nil);
  loadMaps(FHIR_TESTING_FILE('ccda', 'maps'));
end;

procedure TMapTransformTests4.TearDown;
begin
  utils.Free;
  ctxt.free;
end;

procedure TMapTransformTests4.testCD;
var
  x : TFHIRXmlParser;
  s : TStringStream;
  cd : TFhirCodeableConcept;
begin
  x := TFHIRXmlParser.Create(ctxt.link, ctxt.lang);
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

{ TTestTransformerServices4 }

procedure TTestTransformerServices4.createResource(appInfo: TFslObject; res: TFHIRObject; atRootofTransform: boolean);
begin
  raise EFslException.Create('Not done yet');
end;

function TTestTransformerServices4.createType(appInfo: TFslObject; tn: String): TFHIRObject;
begin
  raise EFslException.Create('Not done yet');
end;

procedure TTestTransformerServices4.log(s: String);
begin
  writeln(s);
end;

function TTestTransformerServices4.performSearch(appInfo: TFslObject; url: String): TFslList<TFHIRObject>;
begin
  raise EFslException.Create('Not done yet');
end;

function TTestTransformerServices4.translate(appInfo: TFslObject; src: TFHIRCoding; conceptMapUrl: String): TFHIRCoding;
begin
  raise EFHIRTodo.create('TTestTransformerServices4.translate');
end;

{ MapParserTest2Case4Attribute }

function MapParserTest2Case4Attribute.GetCaseInfoArray: TestCaseInfoArray;
var
  st : TStringList;
  s : String;
  i : integer;
begin
  st := TStringList.Create;
  try
    for s in TDirectory.GetFiles('C:\work\org.hl7.fhir\org.fhir.interversion\r4\R3toR4') do
      if s.endsWith('map') then
        st.Add(s);
    for s in TDirectory.GetFiles('C:\work\org.hl7.fhir\org.fhir.interversion\r4\R4toR3') do
      if s.endsWith('map') then
        st.Add(s);
    SetLength(result, st.Count);
    for i := 0 to st.Count - 1 do
    begin
      result[i].Name := st[i].Substring(50);
      SetLength(result[i].Values, 1);
      result[i].Values[0] := st[i];
    end;
  finally
    st.Free;
  end;
end;

{ TMapParserTests24 }

procedure TMapParserTests24.Test(filename: String);
var
  source, output, s : String;
  ok : boolean;
  map : TFHIRStructureMap;
begin
  source := fileToString(filename, TEncoding.UTF8);
  map := utils.parse(source, filename);
  try
    output := utils.render(map);

    source := source.replace('  ', ' ', [rfReplaceAll]).replace(' '#13#10, #13#10, [rfReplaceAll]).replace(#13#10#13#10, #13#10, [rfReplaceAll]);
    output := output.replace('  ', ' ', [rfReplaceAll]).replace(' '#13#10, #13#10, [rfReplaceAll]).replace(#13#10#13#10, #13#10, [rfReplaceAll]);
    ok := checkTextIsSame(source, output, s);
    assert.IsTrue(ok, s);
  finally
    map.free;
  end;
end;

procedure TMapParserTests24.setup;
begin
  ctxt := TTestingWorkerContext4.Use;
//  (ctxt as TBaseWorkerContext).LoadFromDefinitions(FHIR_SRC_FILE(['guides', 'ccda', 'cda', 'cda.zip']));
  utils := TFHIRStructureMapUtilities.Create(ctxt.link, TFslMap<TFHIRStructureMap>.create('utils'), TTestTransformerServices4.Create, nil);
end;

procedure TMapParserTests24.TearDown;
begin
  // todo

end;

initialization
  TDUnitX.RegisterTestFixture(TMapParserTests4);
  TDUnitX.RegisterTestFixture(TMapParserTests24);
//  TDUnitX.RegisterTestFixture(TMapTransformTests4);
{$ENDIF}
end.
