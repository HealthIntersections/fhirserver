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
program FHIRServerTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  FastMM4 in '..\Libraries\FMM\FastMM4.pas',
  FastMM4Messages in '..\Libraries\FMM\FastMM4Messages.pas',
  Windows,
  SysUtils,
  Classes,
  IdSSLOpenSSLHeaders,
  JclDebug,
  FHIRRestServer in '..\server\FHIRRestServer.pas',
  EncodeSupport in '..\reference-platform\Support\EncodeSupport.pas',
  StringSupport in '..\reference-platform\Support\StringSupport.pas',
  MathSupport in '..\reference-platform\Support\MathSupport.pas',
  SystemSupport in '..\reference-platform\Support\SystemSupport.pas',
  DateSupport in '..\reference-platform\Support\DateSupport.pas',
  MemorySupport in '..\reference-platform\Support\MemorySupport.pas',
  ErrorSupport in '..\reference-platform\Support\ErrorSupport.pas',
  ThreadSupport in '..\reference-platform\Support\ThreadSupport.pas',
  BytesSupport in '..\reference-platform\Support\BytesSupport.pas',
  AdvStringBuilders in '..\reference-platform\Support\AdvStringBuilders.pas',
  AdvObjects in '..\reference-platform\Support\AdvObjects.pas',
  AdvExceptions in '..\reference-platform\Support\AdvExceptions.pas',
  AdvFactories in '..\reference-platform\Support\AdvFactories.pas',
  FileSupport in '..\reference-platform\Support\FileSupport.pas',
  AdvControllers in '..\reference-platform\Support\AdvControllers.pas',
  AdvPersistents in '..\reference-platform\Support\AdvPersistents.pas',
  AdvFilers in '..\reference-platform\Support\AdvFilers.pas',
  ColourSupport in '..\reference-platform\Support\ColourSupport.pas',
  AdvPersistentLists in '..\reference-platform\Support\AdvPersistentLists.pas',
  AdvObjectLists in '..\reference-platform\Support\AdvObjectLists.pas',
  AdvItems in '..\reference-platform\Support\AdvItems.pas',
  AdvCollections in '..\reference-platform\Support\AdvCollections.pas',
  AdvIterators in '..\reference-platform\Support\AdvIterators.pas',
  AdvClassHashes in '..\reference-platform\Support\AdvClassHashes.pas',
  AdvHashes in '..\reference-platform\Support\AdvHashes.pas',
  HashSupport in '..\reference-platform\Support\HashSupport.pas',
  AdvStringHashes in '..\reference-platform\Support\AdvStringHashes.pas',
  AdvStringIntegerMatches in '..\reference-platform\Support\AdvStringIntegerMatches.pas',
  AdvStreams in '..\reference-platform\Support\AdvStreams.pas',
  AdvParameters in '..\reference-platform\Support\AdvParameters.pas',
  AdvFiles in '..\reference-platform\Support\AdvFiles.pas',
  AdvMemories in '..\reference-platform\Support\AdvMemories.pas',
  AdvBuffers in '..\reference-platform\Support\AdvBuffers.pas',
  AdvStreamFilers in '..\reference-platform\Support\AdvStreamFilers.pas',
  AdvExclusiveCriticalSections in '..\reference-platform\Support\AdvExclusiveCriticalSections.pas',
  AdvThreads in '..\reference-platform\Support\AdvThreads.pas',
  AdvSignals in '..\reference-platform\Support\AdvSignals.pas',
  AdvInt64Matches in '..\reference-platform\Support\AdvInt64Matches.pas',
  AdvLargeIntegerMatches in '..\reference-platform\Support\AdvLargeIntegerMatches.pas',
  AdvStringLargeIntegerMatches in '..\reference-platform\Support\AdvStringLargeIntegerMatches.pas',
  AdvStringLists in '..\reference-platform\Support\AdvStringLists.pas',
  AdvCSVFormatters in '..\reference-platform\Support\AdvCSVFormatters.pas',
  AdvTextFormatters in '..\reference-platform\Support\AdvTextFormatters.pas',
  AdvFormatters in '..\reference-platform\Support\AdvFormatters.pas',
  AdvCSVExtractors in '..\reference-platform\Support\AdvCSVExtractors.pas',
  AdvTextExtractors in '..\reference-platform\Support\AdvTextExtractors.pas',
  AdvExtractors in '..\reference-platform\Support\AdvExtractors.pas',
  AdvCharacterSets in '..\reference-platform\Support\AdvCharacterSets.pas',
  AdvOrdinalSets in '..\reference-platform\Support\AdvOrdinalSets.pas',
  AdvStreamReaders in '..\reference-platform\Support\AdvStreamReaders.pas',
  AdvStringStreams in '..\reference-platform\Support\AdvStringStreams.pas',
  AdvStringMatches in '..\reference-platform\Support\AdvStringMatches.pas',
  AdvXMLEntities in '..\reference-platform\Support\AdvXMLEntities.pas',
  AdvXMLFormatters in '..\reference-platform\Support\AdvXMLFormatters.pas',
  ParseMap in '..\reference-platform\Support\ParseMap.pas',
  AdvZipParts in '..\reference-platform\Support\AdvZipParts.pas',
  AdvNameBuffers in '..\reference-platform\Support\AdvNameBuffers.pas',
  AdvZipReaders in '..\reference-platform\Support\AdvZipReaders.pas',
  AdvVCLStreams in '..\reference-platform\Support\AdvVCLStreams.pas',
  AdvZipDeclarations in '..\reference-platform\Support\AdvZipDeclarations.pas',
  AdvZipUtilities in '..\reference-platform\Support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\reference-platform\Support\AdvZipWorkers.pas',
  GUIDSupport in '..\reference-platform\Support\GUIDSupport.pas',
  DecimalSupport in '..\reference-platform\Support\DecimalSupport.pas',
  HL7V2DateSupport in '..\reference-platform\Support\HL7V2DateSupport.pas',
  MsXmlParser in '..\reference-platform\Support\MsXmlParser.pas',
  XMLBuilder in '..\reference-platform\Support\XMLBuilder.pas',
  AdvWinInetClients in '..\reference-platform\Support\AdvWinInetClients.pas',
  MXmlBuilder in '..\reference-platform\support\MXmlBuilder.pas',
  AdvXmlBuilders in '..\reference-platform\Support\AdvXmlBuilders.pas',
  AdvJSON in '..\reference-platform\Support\AdvJSON.pas',
  AfsResourceVolumes in '..\reference-platform\Support\AfsResourceVolumes.pas',
  AfsVolumes in '..\reference-platform\Support\AfsVolumes.pas',
  AfsStreamManagers in '..\reference-platform\Support\AfsStreamManagers.pas',
  AdvObjectMatches in '..\reference-platform\Support\AdvObjectMatches.pas',
  TextUtilities in '..\reference-platform\Support\TextUtilities.pas',
  AdvIntegerObjectMatches in '..\reference-platform\Support\AdvIntegerObjectMatches.pas',
  AdvStringObjectMatches in '..\reference-platform\Support\AdvStringObjectMatches.pas',
  FHIRIndexManagers in '..\server\FHIRIndexManagers.pas',
  AdvNames in '..\reference-platform\Support\AdvNames.pas',
  UcumServices in '..\Libraries\ucum\UcumServices.pas',
  AdvClassLists in '..\reference-platform\Support\AdvClassLists.pas',
  AdvPointers in '..\reference-platform\Support\AdvPointers.pas',
  UcumHandlers in '..\Libraries\ucum\UcumHandlers.pas',
  Ucum in '..\Libraries\ucum\Ucum.pas',
  UcumValidators in '..\Libraries\ucum\UcumValidators.pas',
  UcumExpressions in '..\Libraries\ucum\UcumExpressions.pas',
  UcumSearch in '..\Libraries\ucum\UcumSearch.pas',
  FHIRValueSetExpander in '..\server\FHIRValueSetExpander.pas',
  YuStemmer in '..\Libraries\Stem\YuStemmer.pas',
  LoincServices in '..\Libraries\loinc\LoincServices.pas',
  DISystemCompat in '..\Libraries\Stem\DISystemCompat.pas',
  SnomedServices in '..\Libraries\Snomed\SnomedServices.pas',
  InternetFetcher in '..\reference-platform\Support\InternetFetcher.pas',
  FacebookSupport in '..\reference-platform\Support\FacebookSupport.pas',
  SystemService in '..\reference-platform\Support\SystemService.pas',
  ServiceController in '..\reference-platform\Support\ServiceController.pas',
  AdvIntegerLists in '..\reference-platform\Support\AdvIntegerLists.pas',
  AdvDispatchers in '..\reference-platform\Support\AdvDispatchers.pas',
  AdvEvents in '..\reference-platform\Support\AdvEvents.pas',
  AdvMethods in '..\reference-platform\Support\AdvMethods.pas',
  DBInstaller in '..\server\DBInstaller.pas',
  KDBDialects in '..\reference-platform\Support\KDBDialects.pas',
  KDBLogging in '..\Libraries\db\KDBLogging.pas',
  KDBManager in '..\Libraries\db\KDBManager.pas',
  KDBUtils in '..\Libraries\db\KDBUtils.pas',
  KSettings in '..\Libraries\db\KSettings.pas',
  CurrencySupport in '..\reference-platform\Support\CurrencySupport.pas',
  FHIRNativeStorage in '..\Server\FHIRNativeStorage.pas',
  SnomedImporter in '..\Libraries\snomed\SnomedImporter.pas',
  AdvProfilers in '..\reference-platform\Support\AdvProfilers.pas',
  AnsiStringBuilder in '..\reference-platform\Support\AnsiStringBuilder.pas',
  AdvIntegerMatches in '..\reference-platform\Support\AdvIntegerMatches.pas',
  SnomedPublisher in '..\Libraries\snomed\SnomedPublisher.pas',
  SnomedExpressions in '..\Libraries\snomed\SnomedExpressions.pas',
  HTMLPublisher in '..\reference-platform\Support\HTMLPublisher.pas',
  LoincImporter in '..\Libraries\loinc\LoincImporter.pas',
  LoincPublisher in '..\Libraries\loinc\LoincPublisher.pas',
  TerminologyServerStore in '..\server\TerminologyServerStore.pas',
  TerminologyServices in '..\Libraries\TerminologyServices.pas',
  FHIRValueSetChecker in '..\server\FHIRValueSetChecker.pas',
  TerminologyWebServer in '..\server\TerminologyWebServer.pas',
  FHIRServerConstants in '..\server\FHIRServerConstants.pas',
  FHIRServerUtilities in '..\server\FHIRServerUtilities.pas',
  SearchProcessor in '..\server\SearchProcessor.pas',
  AuthServer in '..\server\AuthServer.pas',
  libeay32 in '..\reference-platform\Support\libeay32.pas',
  HMAC in '..\reference-platform\Support\HMAC.pas',
  JWT in '..\reference-platform\Support\JWT.pas',
  SCIMServer in '..\server\SCIMServer.pas',
  SCIMSearch in '..\server\SCIMSearch.pas',
  TwilioClient in '..\Libraries\security\TwilioClient.pas',
  FHIRSearchSyntax in '..\server\FHIRSearchSyntax.pas',
  ShellSupport in '..\reference-platform\Support\ShellSupport.pas',
  RectSupport in '..\server\RectSupport.pas',
  CoordinateSupport in '..\server\CoordinateSupport.pas',
  AdvGenerics in '..\reference-platform\Support\AdvGenerics.pas',
  XMLSupport in '..\reference-platform\Support\XMLSupport.pas',
  DigitalSignatures in '..\reference-platform\Support\DigitalSignatures.pas',
  UriServices in '..\server\UriServices.pas',
  UniiServices in '..\server\UniiServices.pas',
  RxNormServices in '..\server\RxNormServices.pas',
  OIDSupport in '..\reference-platform\Support\OIDSupport.pas',
  SnomedAnalysis in '..\Libraries\snomed\SnomedAnalysis.pas',
  AreaCodeServices in '..\server\AreaCodeServices.pas',
  FHIRSubscriptionManager in '..\server\FHIRSubscriptionManager.pas',
  ServerValidator in '..\server\ServerValidator.pas',
  IdWebSocket in '..\reference-platform\Support\IdWebSocket.pas',
  MimeMessage in '..\reference-platform\Support\MimeMessage.pas',
  kCritSct in '..\reference-platform\Support\kCritSct.pas',
  QuestionnaireBuilder in '..\reference-platform\r4\QuestionnaireBuilder.pas',
  SCIMObjects in '..\reference-platform\support\SCIMObjects.pas',
  NarrativeGenerator in '..\reference-platform\r4\NarrativeGenerator.pas',
  FHIRSecurity in '..\reference-platform\support\FHIRSecurity.pas',
  FHIRNarrativeGenerator in '..\reference-platform\r4\FHIRNarrativeGenerator.pas',
  SmartOnFhirUtilities in '..\reference-platform\client\SmartOnFhirUtilities.pas',
  FhirPath in '..\reference-platform\r4\FhirPath.pas',
  FHIRTags in '..\reference-platform\r4\FHIRTags.pas',
  FHIRProfileUtilities in '..\reference-platform\r4\FHIRProfileUtilities.pas',
  FHIRBase in '..\reference-platform\support\FHIRBase.pas',
  FHIRTypes in '..\reference-platform\r4\FHIRTypes.pas',
  FHIRResources in '..\reference-platform\r4\FHIRResources.pas',
  FHIRParser in '..\reference-platform\support\FHIRParser.pas',
  FHIRParserBase in '..\reference-platform\support\FHIRParserBase.pas',
  FHIRConstants in '..\reference-platform\r4\FHIRConstants.pas',
  FHIRSupport in '..\reference-platform\support\FHIRSupport.pas',
  FHIRLang in '..\reference-platform\support\FHIRLang.pas',
  FHIRUtilities in '..\reference-platform\r4\FHIRUtilities.pas',
  FHIRClient in '..\reference-platform\client\FHIRClient.pas',
  FHIRValidator in '..\reference-platform\r4\FHIRValidator.pas',
  ClosureManager in '..\server\ClosureManager.pas',
  CDSHooksUtilities in '..\reference-platform\support\CDSHooksUtilities.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  AccessControlEngine in '..\server\AccessControlEngine.pas',
  MPISearch in '..\server\MPISearch.pas',
  RDFUtilities in '..\reference-platform\support\RDFUtilities.pas',
  FHIROperations in '..\reference-platform\r4\FHIROperations.pas',
  FhirOpBase in '..\reference-platform\r4\FhirOpBase.pas',
  FHIRIndexInformation in '..\reference-platform\r4\FHIRIndexInformation.pas',
  FHIRMetaModel in '..\reference-platform\r4\FHIRMetaModel.pas',
  FHIRXhtml in '..\reference-platform\support\FHIRXhtml.pas',
  FHIRStructureMapUtilities in '..\reference-platform\r4\FHIRStructureMapUtilities.pas',
  FHIRContext in '..\reference-platform\r4\FHIRContext.pas',
  XmlPatch in '..\reference-platform\support\XmlPatch.pas',
  FHIRLog in '..\reference-platform\support\FHIRLog.pas',
  FHIRAuthMap in '..\reference-platform\r4\FHIRAuthMap.pas',
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  ServerUtilities in '..\Server\ServerUtilities.pas',
  Logging in '..\Server\Logging.pas',
  ServerAdaptations in '..\Server\ServerAdaptations.pas',
  AdvZipWriters in '..\reference-platform\support\AdvZipWriters.pas',
  ObservationStatsEvaluator in '..\Server\ObservationStatsEvaluator.pas',
  OpenMHealthServer in '..\Server\OpenMHealthServer.pas',
  DifferenceEngine in '..\reference-platform\support\DifferenceEngine.pas',
  ACIRServices in '..\Server\ACIRServices.pas',
  IETFLanguageCodeServices in '..\Server\IETFLanguageCodeServices.pas',
  ReverseClient in '..\Server\ReverseClient.pas',
  FHIRUserProvider in '..\Server\FHIRUserProvider.pas',
  FHIRServerContext in '..\Server\FHIRServerContext.pas',
  FHIRTagManager in '..\Server\FHIRTagManager.pas',
  FHIRSessionManager in '..\Server\FHIRSessionManager.pas',
  FHIRStorageService in '..\Server\FHIRStorageService.pas',
  FHIRGraphQL in '..\reference-platform\support\FHIRGraphQL.pas',
  GraphQL in '..\reference-platform\support\GraphQL.pas',
  FHIRTestWorker in '..\reference-platform\r4\tests\FHIRTestWorker.pas',
  JsonTests in '..\reference-platform\support\Tests\JsonTests.pas',
  XmlTests in '..\reference-platform\support\Tests\XmlTests.pas',
  GraphQLTests in 'GraphQLTests.pas',
  SnomedTests in 'SnomedTests.pas',
  DecimalTests in '..\reference-platform\support\tests\DecimalTests.pas',
  IETFLangTests in 'IETFLangTests.pas',
  JWTTests in '..\reference-platform\support\Tests\JWTTests.pas',
  DifferenceEngineTests in '..\reference-platform\support\tests\DifferenceEngineTests.pas',
  FHIRClientTests in '..\reference-platform\r4\tests\FHIRClientTests.pas',
  FHIRParserTests in '..\reference-platform\r4\tests\FHIRParserTests.pas',
  FHIRPathTests in '..\reference-platform\r4\tests\FHIRPathTests.pas',
  FHIRValidatorTests in '..\reference-platform\r4\tests\FHIRValidatorTests.pas',
  StructureMapTests in '..\reference-platform\r4\tests\StructureMapTests.pas',
  FHIRUtilitiesTests in '..\reference-platform\r4\tests\FHIRUtilitiesTests.pas',
  MXML in '..\reference-platform\support\MXML.pas',
  ParserSupport in '..\reference-platform\support\ParserSupport.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  OSXTests in '..\reference-platform\support\Tests\OSXTests.pas',
  OSXUtils in '..\reference-platform\support\OSXUtils.pas',
  FHIRCodeGenerator in '..\reference-platform\support\FHIRCodeGenerator.pas',
  CDSHooksServices in '..\Server\CDSHooksServices.pas',
  CDSHooksServer in '..\Server\CDSHooksServer.pas',
  TurtleParser in '..\reference-platform\support\TurtleParser.pas',
  TurtleTests in '..\reference-platform\support\Tests\TurtleTests.pas',
  FHIRParserXml in '..\reference-platform\r4\FHIRParserXml.pas',
  FHIRParserJson in '..\reference-platform\r4\FHIRParserJson.pas',
  FHIRParserTurtle in '..\reference-platform\r4\FHIRParserTurtle.pas',
  CqlModel in '..\Libraries\cql\CqlModel.pas',
  CqlParser in '..\Libraries\cql\CqlParser.pas',
  CqlTests in '..\Libraries\cql\CqlTests.pas',
  RestFulServerTests in 'RestFulServerTests.pas',
  SmartOnFhirTestingLogin in 'SmartOnFhirTestingLogin.pas',
  CertificateSupport in '..\reference-platform\support\CertificateSupport.pas',
  GraphDefinitionEngine in '..\Server\GraphDefinitionEngine.pas',
  GraphDefinitionTests in 'GraphDefinitionTests.pas',
  UcumTests in '..\Libraries\ucum\UcumTests.pas',
  ClientApplicationVerifier in '..\Libraries\security\ClientApplicationVerifier.pas',
  JWTService in '..\Server\JWTService.pas',
  CDSHooksClientManager in '..\reference-platform\support\CDSHooksClientManager.pas',
  HackingHealthLogic in '..\Server\Modules\HackingHealthLogic.pas',
  SCrypt in '..\Libraries\security\SCrypt.pas',
  ApplicationCache in '..\Server\ApplicationCache.pas',
  TerminologyOperations in '..\Server\TerminologyOperations.pas',
  WebSourceProvider in '..\Server\WebSourceProvider.pas',
  FHIRIndexBase in '..\reference-platform\support\FHIRIndexBase.pas',
  KDBTests in '..\Libraries\db\KDBTests.pas',
  KDBOdbc in '..\Libraries\db\KDBOdbc.pas',
  ODBCObjects in '..\Libraries\db\ODBCObjects.pas',
  OdbcHeaders in '..\Libraries\db\OdbcHeaders.pas',
  SQLite3 in '..\Libraries\db\SQLite3.pas',
  SQLite3Utils in '..\Libraries\db\SQLite3Utils.pas',
  SQLite3Wrap in '..\Libraries\db\SQLite3Wrap.pas',
  KDBSQLite in '..\Libraries\db\KDBSQLite.pas',
  ServerPostHandlers in '..\Server\ServerPostHandlers.pas',
  TerminologyServer in '..\Server\TerminologyServer.pas',
  ICD10Services in '..\Server\ICD10Services.pas',
  JavascriptTests in '..\Libraries\js\JavascriptTests.pas',
  ChakraCommon in '..\Libraries\js\ChakraCommon.pas',
  Javascript in '..\Libraries\js\Javascript.pas',
  DigitalSignatureTests in '..\reference-platform\support\Tests\DigitalSignatureTests.pas',
  AdvJavascript in '..\Libraries\js\AdvJavascript.pas',
  FHIRJavascriptReg in '..\reference-platform\r4\FHIRJavascriptReg.pas',
  FHIRJavascript in '..\Libraries\js\FHIRJavascript.pas',
  FHIRJavascriptTests in '..\Libraries\js\FHIRJavascriptTests.pas',
  FHIRClientJs in '..\Libraries\js\FHIRClientJs.pas',
  ServerEventJs in '..\Server\ServerEventJs.pas',
  ServerJavascriptHost in '..\Server\ServerJavascriptHost.pas',
  JNIWrapper in '..\Libraries\java\JNIWrapper.pas',
  JNI in '..\Libraries\java\JNI.pas',
  JUtils in '..\Libraries\java\JUtils.pas',
  JavaRuntime in '..\Libraries\java\JavaRuntime.pas',
  JavaBridge in '..\Server\JavaBridge.pas',
  JavaBridgeTests in 'JavaBridgeTests.pas',
  myUTF8Strings in '..\Libraries\java\myUTF8Strings.pas',
  FHIRFactory in '..\reference-platform\support\FHIRFactory.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  GBasePath  := paramstr(1);
  if GBasePath = '' then
    GBasePath := 'C:\work\org.hl7.fhir';

{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
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
