unit tests;

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
  SysUtils, Classes, fsl_shell, IniFiles,
  fsl_base, GuidSupport, AdvXmlBuilders,
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Parser,
  tx_server, FHIRDataStore, FHIR.Version.Client, FHIR.Version.PathEngine;

procedure ExecuteFhirServerTests(all: boolean);
//Type
//  TFhirServerTests = class (TFslObject)
//  private
//    FIni: TIniFile;
//    FTerminologyServer: TTerminologyServer;
//    procedure TestSnomedExpressions;
//    procedure SetTerminologyServer(const Value: TTerminologyServer);
//  public
//    destructor Destroy; override;
//    property  ini : TIniFile read FIni write FIni;
//    property tx_server : TTerminologyServer read FTerminologyServer write SetTerminologyServer;
//    procedure executeLibrary; // library functionality to test
//    procedure executeBefore; // before server is started
//    procedure executeRound1;  // initial state - all loaded, but empty
//    procedure executeRound2;  // 2nd cycle: after everything is loaded
//    procedure executeAfter;
//  end;
//
//  TFHIRQuestionnaireBuilderTests = class (TFslObject)
//  private
//    FDataStore: TFHIRNativeStorageService;
//    function LoadJsonResource(filename : String) : TFhirResource;
//    procedure SaveResource(resource : TFhirResource; filename : String);
//    procedure RoundTrip(filename : String; name : String);
//    procedure RunTests(srcDir : String); overload;
//  public
//    class procedure RunTests(ini : TIniFile; dataStore: TFHIRNativeStorageService); overload;
//  end;


implementation

uses
  ftx_sct_services, ftx_sct_expressions, scim_search, search_syntax,
  ftx_ucum_tests, TwilioClient, FHIR.Support.Signatures, FHIR.Version.Questionnaire,
  MarkdownDaringFireballTests, fsl_json;

const
  !{$IFDEF FHIR2}
  PUB_PATH = 'C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu2\build\publish';
  {$ELSE}
  PUB_PATH = 'C:\work\org.hl7.fhir.old\org.hl7.fhir.2016May\build\publish';
  {$ENDIF}


procedure ExecuteFhirServerTests(all: boolean);
var
  context : TTestingWorkerContext;
begin
//  Writeln('FHIR Server Tests');
//  Writeln('=================');
//  try
//    writeln('Decimal Tests....');
//    if all then TDecimalTests.runTests;
//    writeln('... done');
//
//    writeln('SCIM Search Tests....');
//    if all then TSCIMSearchParser.runTests;
//    writeln('... done');
//
//    writeln('FHIR Search Syntax Tests....');
//    if all then TFSFilterParser.runTests;
//    writeln('... done');
//
//    writeln('JWT Tests....');
//    if all then TJWTTests.runTests;
//    writeln('... done');
//
//    writeln('Markdown Tests....');
//    if all then TMarkdownDaringFireballTests.tests('C:\work\markdown\resources\df');
//    writeln('... done');
//
//    writeln('JSON Patch Tests....');
//    if all then TJsonPatchEngine.tests('C:\work\fhirserver\utilities\tests\json-patch-tests.json');
//    writeln('... done');
//
//    writeln('Twilio Tests....');
//    if all then TTwilioClient.RunTests;
//    writeln('... done');
//
//// failing.. fix them up
////    writeln('UCUM Tests....');
//    if all then TUcumTests.runTests('C:\work\fhirserver\Exec');
////    writeln('... done');
//
//    writeln('Loading validation pack...');
//    context := TTestingWorkerContext.create;
//    try
//      context.LoadFromDefinitions(PUB_PATH+'\validation-min.xml.zip');
//
//      writeln('FHIR Path Tests....');
////      if all then
//      TFHIRPathTests.runTests(context);
//      writeln('... done');
//
//      writeln('FHIR Validator Tests....');
//      if all then TFHIRValidatorTests.runTests(context, PUB_PATH);
//      writeln('... done');
//
//      writeln('FHIR Parser Tests....');
//      if all then TFHIRParserTests.runTests(PUB_PATH);
//      writeln('... done');
//    finally
//      context.free;
//    end;
//
//// others to add:
////  // ;
//// // TDigitalSignatureTests.test;
//////  !{$IFDEF FHIR2}
//////  TFHIRParserTests.runTests('C:\work\org.hl7.fhir.dstu2\build\publish\examples');
//////  {$ELSE}
//////  TFHIRParserTests.runTests(PUB_HOME+'\examples');
//////  {$ENDIF}
//////  TFslXmlBuilderCanonicalizationTests.test;
//////  !{$IFDEF FHIR2}
//////  TFhirHTTPClientTests.tests('http://local.healthintersections.com.au:960/open');
//////  {$ELSE}
//////  TFhirHTTPClientTests.tests('http://fhir21.healthintersections.com.au');
//
//    WriteLn('All Library tests Passed');
//  except
//    on e : exception do
//      writeln('exception: '+e.message);
//  end;
//  writeln('press enter to close');
//  readln;
end;

//procedure TFhirServerTests.executeAfter;
//begin
//    // import rf1
//    // import rf2
//end;
//
//procedure TFhirServerTests.TestSnomedExpressions;
//begin
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '297186008 | motorcycle accident |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '297186008').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '217724009 | accident caused by blizzard | +297186008 | motorcycle accident |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '217724009 +297186008 | motorcycle accident |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '217724009'#13#10' + 297186008 | motorcycle accident |'#13#10'').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '217724009 + 297186008 '#13#10'| motorcycle accident |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '217724009 | accident caused by blizzard |:116680003 | is a | =297186008 | motorcycle accident |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '297186008 | motorcycle accident |:116680003 | is a | =217724009 | accident caused by blizzard |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '83152002 | oophorectomy |: 260686004 | method |=257820006| laser excision - action |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '313056006 | epiphysis of ulna |:272741003 | laterality | =7771000 | left |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '119189000 | ulna part | + 312845000 | epiphysis of upper limb |:272741003 | laterality | =7771000 | left |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '83152002 | oophorectomy |:260686004 | method |=257820006| laser excision - action |,363704007 | procedure site | =20837000 | structure of right ovary |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '120053002 | Salpingectomy |:260686004 | method | =261519002 | diathermy excision - action |,363704007 | procedure site | =113293009 | structure of left fallopian tube |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '116028008 | salpingo-oophorectomy |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '71388002 | procedure |:{260686004 | method | =129304002 | excision - action |,405813007 | procedure site - Direct | =15497006 | ovarian '+'structure |}{260686004 | method | =129304002 | excision - action |,405813007 | procedure site - Direct | =31435000 | fallopian tube structure |}').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '116028008 | salpingo-oophorectomy |: {260686004 | method |=257820006| laser excision - action |,363704007 | procedure site | =20837000 | structure of right ovary |}{260686004 | '+'method | =261519002 | diathermy excision - action |,363704007 | procedure site | =113293009 | structure of left fallopian tube |}').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '71620000 | fracture of femur |: 42752001 | due to | = (217724009 | accident caused by blizzard | +297186008 | motorcycle accident |)').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '24136001 | hip joint structure |: 272741003 | laterality | =7771000 | left |').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '397956004 | prosthetic arthroplasty of the hip |:363704007 | procedure site | = (24136001 | hip joint structure | :272741003 | laterality | =7771000 | left |)').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '397956004 | prosthetic arthroplasty of the hip |:363704007 | procedure site | = (24136001 | hip joint structure| :272741003 | laterality | =7771000 | left |) {363699004 |'+' direct device | =304120007 | total hip replacement prosthesis |,260686004 | method | =257867005 | insertion - action |}').Free;
//  TSnomedExpressionParser.Parse(tx_server.Snomed, '243796009 | situation with explicit context |: {363589002 | associated procedure | = (397956004 | prosthetic arthroplasty of the hip |:363704007 | procedure site | = (24136001 | '+'hip joint structure | :272741003 | laterality | =7771000 | left |) {363699004 | direct device | =304120007 | total hip replacement prosthesis |, '+'260686004 | method | =257867005 | insertion - action |}), 408730004 | procedure context | =385658003 | done |, 408731000 | temporal context | =410512000 | current or specified |, 408732007 | subject relationship context |=410604004 | subject of record | }').Free;
//end;
//
//
//{ TFHIRQuestionnaireBuilderTests }
//
//function TFHIRQuestionnaireBuilderTests.LoadJsonResource(filename: String): TFhirResource;
//var
//  stream : TFileStream;
//  json : TFHIRJsonParser;
//begin
//  stream := TFileStream.Create(filename, fmOpenRead);
//  try
//    json := TFHIRJsonParser.Create(THTTPLanguages.create('en'));
//    try
//      json.source := stream;
//      json.Parse;
//      result := json.resource.Link;
//    finally
//      json.free;
//    end;
//  finally
//    stream.Free
//  end;
//end;
//
//class procedure TFHIRQuestionnaireBuilderTests.RunTests(ini : TInifile; dataStore: TFHIRNativeStorageService);
//var
//  this : TFHIRQuestionnaireBuilderTests;
//  srcDir : string;
//begin
//  this := TFHIRQuestionnaireBuilderTests.Create;
//  try
//    this.FDataStore := dataStore;
//    srcDir := ini.ReadString('fhir', 'source', '');
//    this.RunTests(srcDir);
//  finally
//    this.free;
//  end;
//end;
//
//procedure TFHIRQuestionnaireBuilderTests.SaveResource(resource: TFhirResource; filename: String);
//var
//  stream : TFileStream;
//  json : TFHIRJsonComposer;
//begin
//  stream := TFileStream.Create(filename, fmCreate);
//  try
//    json := TFHIRJsonComposer.Create(THTTPLanguages.create('en'));
//    try
//      json.Compose(stream, resource, true, nil);
//    finally
//      json.free;
//    end;
//  finally
//    stream.Free
//  end;
//end;
//
//procedure TFHIRQuestionnaireBuilderTests.RunTests(srcDir: String);
//begin
//  RoundTrip(IncludeTrailingPathDelimiter(srcDir)+'location-example.json', 'Location');
////  RoundTrip(IncludeTrailingPathDelimiter(srcDir)+'medication-example.json', 'Medication');
////  RoundTrip(IncludeTrailingPathDelimiter(srcDir)+'adversereaction-example.json', 'AdverseReaction');
////  RoundTrip(IncludeTrailingPathDelimiter(srcDir)+'observation-example.json', 'Observation');
//end;
//
//procedure TFHIRQuestionnaireBuilderTests.RoundTrip(filename : String; name : String);
//var
//  thisOut : TQuestionnaireBuilder;
//  thisIn : TQuestionnaireBuilder;
//begin
//  thisOut := TQuestionnaireBuilder.Create;
//  thisIn := TQuestionnaireBuilder.Create;
//  try
//    thisOut.Profiles := FDataStore.ValidatorContext.profiles.Link;
//    thisOut.OnExpand := FDataStore.ExpandVS;
//    thisIn.Profiles := FDataStore.ValidatorContext.profiles.Link;
//    thisIn.OnExpand := FDataStore.ExpandVS;
//    thisOut.QuestionnaireId := NewGuidURN;
//
//    thisOut.Resource := LoadJsonResource(filename) as TFhirDomainResource;
//    thisOut.Resource.text := nil;
//    thisOut.Profile := FDataStore.ValidatorContext.profiles['http://hl7.org/fhir/Profile/'+name].Link;
//    thisOut.Build;
//    saveResource(thisOut.Resource, 'c:\temp\start.json');
//    saveResource(thisOut.Answers, 'c:\temp\qa.json');
//
//    thisIn.answers := thisOut.Answers.Link;
//    thisIn.Profile := FDataStore.ValidatorContext.profiles['http://hl7.org/fhir/Profile/'+name].Link;
//    thisIn.UnBuild;
//    saveResource(thisIn.Resource, 'c:\temp\end.json');
//
//  finally
//    thisOut.Free;
//    thisIn.Free;
//  end;
//  ExecuteOpen('c:\Program Files (x86)\WinMerge\WinMergeU.exe', 'c:\temp\start.json c:\temp\end.json');
//end;
//

{
var
  tests : TFhirServerTests;
begin
  try
    TestMode := true;
    tests := TFhirServerTests.Create;
    try
      tests.ini := FIni;
      tests.executeLibrary;
      exit;
      if FDb = nil then
        ConnectToDatabase;
      if dbExists then
        UnInstallDatabase;
      InstallDatabase;
      LoadTerminologies;
      tests.tx_server := FTerminologyServer.Link;
      tests.executeBefore;

      CanStart;
      TFHIRQuestionnaireBuilderTests.runTests(FIni, FWebServer.DataStore);
      tests.executeRound1;
      DoStop;

      CanStart;
      tests.executeRound2;
      DoStop;

      UnloadTerminologies;
      UnInstallDatabase;

      tests.executeAfter; // final tests - these go on for a very long time,
    finally
      tests.Free;
    end;
    ExitCode := 0;
  except
    on e: Exception do
    begin
      Logging.log(e.Message);
      ExitCode := 1;
    end;
  end;
end;

}
end.


