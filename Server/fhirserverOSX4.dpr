{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
program fhirserverOSX4;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  AdvObjects in '..\reference-platform\support\AdvObjects.pas',
  AdvExceptions in '..\reference-platform\support\AdvExceptions.pas',
  StringSupport in '..\reference-platform\support\StringSupport.pas',
  MathSupport in '..\reference-platform\support\MathSupport.pas',
  BytesSupport in '..\reference-platform\support\BytesSupport.pas',
  OSXUtils in '..\reference-platform\support\OSXUtils.pas',
  AdvStringBuilders in '..\reference-platform\support\AdvStringBuilders.pas',
  AdvStreams in '..\reference-platform\support\AdvStreams.pas',
  AdvObjectLists in '..\reference-platform\support\AdvObjectLists.pas',
  MemorySupport in '..\reference-platform\support\MemorySupport.pas',
  AdvItems in '..\reference-platform\support\AdvItems.pas',
  AdvFilers in '..\reference-platform\support\AdvFilers.pas',
  DateSupport in '..\reference-platform\support\DateSupport.pas',
  ErrorSupport in '..\reference-platform\support\ErrorSupport.pas',
  SystemSupport in '..\reference-platform\support\SystemSupport.pas',
  ThreadSupport in '..\reference-platform\support\ThreadSupport.pas',
  ColourSupport in '..\reference-platform\support\ColourSupport.pas',
  CurrencySupport in '..\reference-platform\support\CurrencySupport.pas',
  DecimalSupport in '..\reference-platform\support\DecimalSupport.pas',
  AdvCollections in '..\reference-platform\support\AdvCollections.pas',
  AdvPersistents in '..\reference-platform\support\AdvPersistents.pas',
  AdvIterators in '..\reference-platform\support\AdvIterators.pas',
  HL7V2DateSupport in '..\reference-platform\support\HL7V2DateSupport.pas',
  HashSupport in '..\reference-platform\support\HashSupport.pas',
  ParserSupport in '..\reference-platform\support\ParserSupport.pas',
  OIDSupport in '..\reference-platform\support\OIDSupport.pas',
  TextUtilities in '..\reference-platform\support\TextUtilities.pas',
  EncodeSupport in '..\reference-platform\support\EncodeSupport.pas',
  AdvGenerics in '..\reference-platform\support\AdvGenerics.pas',
  AdvFiles in '..\reference-platform\support\AdvFiles.pas',
  ParseMap in '..\reference-platform\support\ParseMap.pas',
  RDFUtilities in '..\reference-platform\support\RDFUtilities.pas',
  GUIDSupport in '..\reference-platform\support\GUIDSupport.pas',
  FileSupport in '..\reference-platform\support\FileSupport.pas',
  kCritSct in '..\reference-platform\support\kCritSct.pas',
  MimeMessage in '..\reference-platform\support\MimeMessage.pas',
  OSXTests in '..\reference-platform\support\Tests\OSXTests.pas',
  AdvMemories in '..\reference-platform\support\AdvMemories.pas',
  AdvBuffers in '..\reference-platform\support\AdvBuffers.pas',
  MXML in '..\reference-platform\support\MXML.pas',
  AdvJson in '..\reference-platform\support\AdvJson.pas',
  AdvPersistentLists in '..\reference-platform\support\AdvPersistentLists.pas',
  AdvStreamReaders in '..\reference-platform\support\AdvStreamReaders.pas',
  AdvVCLStreams in '..\reference-platform\support\AdvVCLStreams.pas',
  AdvTextFormatters in '..\reference-platform\support\AdvTextFormatters.pas',
  AdvFormatters in '..\reference-platform\support\AdvFormatters.pas',
  AdvTextExtractors in '..\reference-platform\support\AdvTextExtractors.pas',
  AdvExtractors in '..\reference-platform\support\AdvExtractors.pas',
  AdvCharacterSets in '..\reference-platform\support\AdvCharacterSets.pas',
  AdvOrdinalSets in '..\reference-platform\support\AdvOrdinalSets.pas',
  AdvStringLists in '..\reference-platform\support\AdvStringLists.pas',
  AdvCSVFormatters in '..\reference-platform\support\AdvCSVFormatters.pas',
  AdvCSVExtractors in '..\reference-platform\support\AdvCSVExtractors.pas',
  MXmlBuilder in '..\reference-platform\support\MXmlBuilder.pas',
  AdvStringStreams in '..\reference-platform\support\AdvStringStreams.pas',
  XMLBuilder in '..\reference-platform\support\XMLBuilder.pas',
  libeay32 in '..\reference-platform\support\libeay32.pas',
  HMAC in '..\reference-platform\support\HMAC.pas',
  TurtleParser in '..\reference-platform\support\TurtleParser.pas',
  AdvStringMatches in '..\reference-platform\support\AdvStringMatches.pas',
  JWT in '..\reference-platform\support\JWT.pas' {,
  FHIRParser in '..\reference-platform\support\FHIRParser.pas',
  FHIRParserXml in '..\reference-platform\r4\FHIRParserXml.pas',
  FHIRParserJson in '..\reference-platform\r4\FHIRParserJson.pas',
  FHIRParserTurtle in '..\reference-platform\r4\FHIRParserTurtle.pas',
  FHIRResources in '..\reference-platform\r4\FHIRResources.pas',
  FHIRTypes in '..\reference-platform\r4\FHIRTypes.pas',
  FHIRParserBase in '..\reference-platform\support\FHIRParserBase.pas',
  AdvXmlBuilders in '..\reference-platform\support\AdvXmlBuilders.pas',
  AdvXMLFormatters in '..\reference-platform\support\AdvXMLFormatters.pas',
  AdvXMLEntities in '..\reference-platform\support\AdvXMLEntities.pas',
  FHIRBase in '..\reference-platform\support\FHIRBase.pas',
  AdvNames in '..\reference-platform\support\AdvNames.pas',
  FHIRUtilities in '..\reference-platform\r4\FHIRUtilities.pas',
  AdvZipWriters in '..\reference-platform\support\AdvZipWriters.pas',
  AdvNameBuffers in '..\reference-platform\support\AdvNameBuffers.pas',
  AdvObjectMatches in '..\reference-platform\support\AdvObjectMatches.pas',
  AdvZipDeclarations in '..\reference-platform\support\AdvZipDeclarations.pas',
  AdvZipParts in '..\reference-platform\support\AdvZipParts.pas',
  AdvZipUtilities in '..\reference-platform\support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\reference-platform\support\AdvZipWorkers.pas',
  InternetFetcher in '..\reference-platform\support\InternetFetcher.pas'{,
  FHIRContext in '..\reference-platform\r4\FHIRContext.pas',
  FHIRSupport in '..\reference-platform\support\FHIRSupport.pas',
  SCIMObjects in '..\reference-platform\support\SCIMObjects.pas',
  GraphQL in '..\reference-platform\support\GraphQL.pas',
  FHIRConstants in '..\reference-platform\r4\FHIRConstants.pas',
  FHIRSecurity in '..\reference-platform\support\FHIRSecurity.pas',
  FHIRTags in '..\reference-platform\r4\FHIRTags.pas',
  FHIRLang in '..\reference-platform\support\FHIRLang.pas',
  FHIRXhtml in '..\reference-platform\support\FHIRXhtml.pas',
  FHIRMetaModel in '..\reference-platform\r4\FHIRMetaModel.pas',
  FHIRProfileUtilities in '..\reference-platform\r4\FHIRProfileUtilities.pas',
  AdvZipReaders in '..\reference-platform\support\AdvZipReaders.pas',
  FhirPath in '..\reference-platform\r4\FhirPath.pas'},
  KDBTests in '..\Libraries\db\KDBTests.pas',
  KDBDialects in '..\reference-platform\support\KDBDialects.pas',
  KDBManager in '..\Libraries\db\KDBManager.pas',
  KDBLogging in '..\Libraries\db\KDBLogging.pas',
  KSettings in '..\Libraries\db\KSettings.pas',
  KDBOdbc in '..\Libraries\db\KDBOdbc.pas',
  ODBCObjects in '..\Libraries\db\ODBCObjects.pas',
  OdbcHeaders in '..\Libraries\db\OdbcHeaders.pas',
  KDBSQLite in '..\Libraries\db\KDBSQLite.pas',
  SQLite3 in '..\Libraries\db\SQLite3.pas',
  SQLite3Wrap in '..\Libraries\db\SQLite3Wrap.pas',
  SQLite3Utils in '..\Libraries\db\SQLite3Utils.pas',
  FHIRTestWorker in '..\reference-platform\r4\tests\FHIRTestWorker.pas',
  FHIRBase in '..\reference-platform\support\FHIRBase.pas',
  AdvNames in '..\reference-platform\support\AdvNames.pas',
  FHIRUtilities in '..\reference-platform\r4\FHIRUtilities.pas',
  AdvZipWriters in '..\reference-platform\support\AdvZipWriters.pas',
  AdvNameBuffers in '..\reference-platform\support\AdvNameBuffers.pas',
  AdvObjectMatches in '..\reference-platform\support\AdvObjectMatches.pas',
  AdvZipDeclarations in '..\reference-platform\support\AdvZipDeclarations.pas',
  AdvZipParts in '..\reference-platform\support\AdvZipParts.pas',
  AdvZipUtilities in '..\reference-platform\support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\reference-platform\support\AdvZipWorkers.pas',
  FHIRContext in '..\reference-platform\r4\FHIRContext.pas',
  FHIRTypes in '..\reference-platform\r4\FHIRTypes.pas',
  FHIRResources in '..\reference-platform\r4\FHIRResources.pas',
  FHIRSupport in '..\reference-platform\support\FHIRSupport.pas',
  SCIMObjects in '..\reference-platform\support\SCIMObjects.pas',
  GraphQL in '..\reference-platform\support\GraphQL.pas',
  FHIRConstants in '..\reference-platform\r4\FHIRConstants.pas',
  FHIRSecurity in '..\reference-platform\support\FHIRSecurity.pas',
  FHIRTags in '..\reference-platform\r4\FHIRTags.pas',
  FHIRLang in '..\reference-platform\support\FHIRLang.pas',
  FHIRXhtml in '..\reference-platform\support\FHIRXhtml.pas',
  FHIRParser in '..\reference-platform\support\FHIRParser.pas',
  FHIRParserBase in '..\reference-platform\support\FHIRParserBase.pas',
  AdvXmlBuilders in '..\reference-platform\support\AdvXmlBuilders.pas',
  AdvXMLFormatters in '..\reference-platform\support\AdvXMLFormatters.pas',
  AdvXMLEntities in '..\reference-platform\support\AdvXMLEntities.pas',
  FHIRParserXml in '..\reference-platform\r4\FHIRParserXml.pas',
  FHIRParserJson in '..\reference-platform\r4\FHIRParserJson.pas',
  FHIRParserTurtle in '..\reference-platform\r4\FHIRParserTurtle.pas',
  FHIRMetaModel in '..\reference-platform\r4\FHIRMetaModel.pas',
  FHIRProfileUtilities in '..\reference-platform\r4\FHIRProfileUtilities.pas',
  AdvZipReaders in '..\reference-platform\support\AdvZipReaders.pas',
  FHIRLog in '..\reference-platform\support\FHIRLog.pas',
  FhirPath in '..\reference-platform\r4\FhirPath.pas',
  InternetFetcher in '..\reference-platform\support\InternetFetcher.pas',
  FHIRIndexBase in '..\reference-platform\support\FHIRIndexBase.pas',
  DigitalSignatures in '..\reference-platform\support\DigitalSignatures.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
//  GBasePath  := paramstr(1);
//  if GBasePath = '' then
//    GBasePath := 'C:\work\org.hl7.fhir';

{$IFDEF TESTINSIGHT}
TestInsight not for OSX
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.


  FhirPath in '..\reference-platform\r4\FhirPath.pas'},
  KDBTests in '..\Libraries\db\KDBTests.pas',
  KDBDialects in '..\reference-platform\support\KDBDialects.pas',
  KDBManager in '..\Libraries\db\KDBManager.pas',
  KSettings in '..\Libraries\db\KSettings.pas',
  KDBLogging in '..\Libraries\db\KDBLogging.pas',
  KDBOdbc in '..\Libraries\db\KDBOdbc.pas',
  ODBCObjects in '..\Libraries\db\ODBCObjects.pas',
  OdbcHeaders in '..\Libraries\db\OdbcHeaders.pas';

