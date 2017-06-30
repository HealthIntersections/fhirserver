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
  JWT in '..\reference-platform\support\JWT.pas',
  AdvStringMatches in '..\reference-platform\support\AdvStringMatches.pas',
  FHIRParser in '..\reference-platform\r4\FHIRParser.pas',
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
  InternetFetcher in '..\reference-platform\support\InternetFetcher.pas',
  FHIRContext in '..\reference-platform\r4\FHIRContext.pas',
  FHIRSupport in '..\reference-platform\r4\FHIRSupport.pas',
  SCIMObjects in '..\reference-platform\support\SCIMObjects.pas',
  GraphQL in '..\reference-platform\support\GraphQL.pas',
  FHIRConstants in '..\reference-platform\r4\FHIRConstants.pas',
  FHIRSecurity in '..\reference-platform\r4\FHIRSecurity.pas',
  FHIRTags in '..\reference-platform\r4\FHIRTags.pas',
  FHIRLang in '..\reference-platform\support\FHIRLang.pas',
  FHIRXhtml in '..\reference-platform\support\FHIRXhtml.pas',
  FHIRMetaModel in '..\reference-platform\r4\FHIRMetaModel.pas',
  FHIRProfileUtilities in '..\reference-platform\r4\FHIRProfileUtilities.pas',
  AdvZipReaders in '..\reference-platform\support\AdvZipReaders.pas',
  FhirPath in '..\reference-platform\r4\FhirPath.pas';

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
