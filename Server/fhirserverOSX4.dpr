program fhirserverOSX4;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
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
  OSXTests in '..\reference-platform\support\Tests\OSXTests.pas';

{
  ShellSupport in '..\support\ShellSupport.pas',

  kCritSct in '..\reference-platform\support\kCritSct.pas',
  MimeMessage in '..\reference-platform\support\MimeMessage.pas',
  MXML in '..\reference-platform\support\MXML.pas',

  HMAC in '..\support\HMAC.pas',
  JWT in '..\support\JWT.pas',
  libeay32 in '..\support\libeay32.pas',
  MXmlBuilder in '..\support\MXmlBuilder.pas',

}
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
  TestInsight.DUnitX.RunRegisteredTests;
  !
  exit;
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
