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
  FHIR.Ucum.Services in '..\Libraries\Ucum\FHIR.Ucum.Services.pas',
  AdvClassLists in '..\reference-platform\Support\AdvClassLists.pas',
  AdvPointers in '..\reference-platform\Support\AdvPointers.pas',
  FHIR.Ucum.Handlers in '..\Libraries\Ucum\FHIR.Ucum.Handlers.pas',
  FHIR.Ucum.Base in '..\Libraries\Ucum\FHIR.Ucum.Base.pas',
  FHIR.Ucum.Validators in '..\Libraries\Ucum\FHIR.Ucum.Validators.pas',
  FHIR.Ucum.Expressions in '..\Libraries\Ucum\FHIR.Ucum.Expressions.pas',
  FHIR.Ucum.Search in '..\Libraries\Ucum\FHIR.Ucum.Search.pas',
  FHIRValueSetExpander in '..\server\FHIRValueSetExpander.pas',
  YuStemmer in '..\Libraries\Stem\YuStemmer.pas',
  FHIR.Loinc.Services in '..\Libraries\loinc\FHIR.Loinc.Services.pas',
  DISystemCompat in '..\Libraries\Stem\DISystemCompat.pas',
  FHIR.Snomed.Services in '..\Libraries\Snomed\FHIR.Snomed.Services.pas',
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
  FHIR.Database.Logging in '..\Libraries\db\FHIR.Database.Logging.pas',
  FHIR.Database.Manager in '..\Libraries\db\FHIR.Database.Manager.pas',
  FHIR.Database.Utilities in '..\Libraries\db\FHIR.Database.Utilities.pas',
  FHIR.Database.Settings in '..\Libraries\db\FHIR.Database.Settings.pas',
  CurrencySupport in '..\reference-platform\Support\CurrencySupport.pas',
  FHIRNativeStorage in '..\Server\FHIRNativeStorage.pas',
  FHIR.Snomed.Importer in '..\Libraries\snomed\FHIR.Snomed.Importer.pas',
  AdvProfilers in '..\reference-platform\Support\AdvProfilers.pas',
  AnsiStringBuilder in '..\reference-platform\Support\AnsiStringBuilder.pas',
  AdvIntegerMatches in '..\reference-platform\Support\AdvIntegerMatches.pas',
  FHIR.Snomed.Publisher in '..\Libraries\snomed\FHIR.Snomed.Publisher.pas',
  FHIR.Snomed.Expressions in '..\Libraries\snomed\FHIR.Snomed.Expressions.pas',
  HTMLPublisher in '..\reference-platform\Support\HTMLPublisher.pas',
  FHIR.Loinc.Importer in '..\Libraries\loinc\FHIR.Loinc.Importer.pas',
  FHIR.Loinc.Publisher in '..\Libraries\loinc\FHIR.Loinc.Publisher.pas',
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
  FHIR.Misc.Twilio in '..\Libraries\security\FHIR.Misc.Twilio.pas',
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
  FHIR.Snomed.Analysis in '..\Libraries\snomed\FHIR.Snomed.Analysis.pas',
  AreaCodeServices in '..\server\AreaCodeServices.pas',
  FHIRSubscriptionManager in '..\server\FHIRSubscriptionManager.pas',
  ServerValidator in '..\server\ServerValidator.pas',
  IdWebSocket in '..\reference-platform\Support\IdWebSocket.pas',
  MimeMessage in '..\reference-platform\Support\MimeMessage.pas',
  kCritSct in '..\reference-platform\Support\kCritSct.pas',
  QuestionnaireBuilder4 in '..\reference-platform\r4\QuestionnaireBuilder4.pas',
  SCIMObjects in '..\reference-platform\support\SCIMObjects.pas',
  NarrativeGenerator4 in '..\reference-platform\r4\NarrativeGenerator4.pas',
  FHIRSecurity in '..\reference-platform\support\FHIRSecurity.pas',
  FHIRNarrativeGenerator4 in '..\reference-platform\r4\FHIRNarrativeGenerator4.pas',
  SmartOnFhirUtilities in '..\reference-platform\client\SmartOnFhirUtilities.pas',
  FhirPath4 in '..\reference-platform\r4\FhirPath4.pas',
  FHIRTags4 in '..\reference-platform\r4\FHIRTags4.pas',
  FHIRProfileUtilities4 in '..\reference-platform\r4\FHIRProfileUtilities4.pas',
  FHIRBase in '..\reference-platform\support\FHIRBase.pas',
  FHIRTypes4 in '..\reference-platform\r4\FHIRTypes4.pas',
  FHIRResources4 in '..\reference-platform\r4\FHIRResources4.pas',
  FHIRParser in '..\reference-platform\support\FHIRParser.pas',
  FHIRParserBase in '..\reference-platform\support\FHIRParserBase.pas',
  FHIRConstants4 in '..\reference-platform\r4\FHIRConstants4.pas',
  FHIRSupport in '..\reference-platform\support\FHIRSupport.pas',
  FHIRLang in '..\reference-platform\support\FHIRLang.pas',
  FHIRUtilities4 in '..\reference-platform\r4\FHIRUtilities4.pas',
  FHIRClient in '..\reference-platform\client\FHIRClient.pas',
  FHIRValidator4 in '..\reference-platform\r4\FHIRValidator4.pas',
  ClosureManager in '..\server\ClosureManager.pas',
  CDSHooksUtilities in '..\reference-platform\support\CDSHooksUtilities.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  AccessControlEngine in '..\server\AccessControlEngine.pas',
  MPISearch in '..\server\MPISearch.pas',
  RDFUtilities in '..\reference-platform\support\RDFUtilities.pas',
  FHIROperations4 in '..\reference-platform\r4\FHIROperations4.pas',
  FhirOpBase4 in '..\reference-platform\r4\FhirOpBase4.pas',
  FHIRIndexInformation4 in '..\reference-platform\r4\FHIRIndexInformation4.pas',
  FHIRMetaModel4 in '..\reference-platform\r4\FHIRMetaModel4.pas',
  FHIRXhtml in '..\reference-platform\support\FHIRXhtml.pas',
  FHIRStructureMapUtilities4 in '..\reference-platform\r4\FHIRStructureMapUtilities4.pas',
  FHIRContext4 in '..\reference-platform\r4\FHIRContext4.pas',
  XmlPatch in '..\reference-platform\support\XmlPatch.pas',
  FHIRLog in '..\reference-platform\support\FHIRLog.pas',
  FHIRAuthMap4 in '..\reference-platform\r4\FHIRAuthMap4.pas',
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
  FHIRTestWorker4 in '..\reference-platform\r4\tests\FHIRTestWorker4.pas',
  JsonTests in '..\reference-platform\support\Tests\JsonTests.pas',
  XmlTests in '..\reference-platform\support\Tests\XmlTests.pas',
  GraphQLTests in 'GraphQLTests.pas',
  SnomedTests in 'SnomedTests.pas',
  DecimalTests in '..\reference-platform\support\tests\DecimalTests.pas',
  IETFLangTests in 'IETFLangTests.pas',
  JWTTests in '..\reference-platform\support\Tests\JWTTests.pas',
  DifferenceEngineTests in '..\reference-platform\support\tests\DifferenceEngineTests.pas',
  FHIRClientTests4 in '..\reference-platform\r4\tests\FHIRClientTests4.pas',
  FHIRParserTests4 in '..\reference-platform\r4\tests\FHIRParserTests4.pas',
  FHIRPathTests4 in '..\reference-platform\r4\tests\FHIRPathTests4.pas',
  FHIRValidatorTests4 in '..\reference-platform\r4\tests\FHIRValidatorTests4.pas',
  StructureMapTests4 in '..\reference-platform\r4\tests\StructureMapTests4.pas',
  FHIRUtilitiesTests4 in '..\reference-platform\r4\tests\FHIRUtilitiesTests4.pas',
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
  FHIRParserXml4 in '..\reference-platform\r4\FHIRParserXml4.pas',
  FHIRParserJson4 in '..\reference-platform\r4\FHIRParserJson4.pas',
  FHIRParserTurtle4 in '..\reference-platform\r4\FHIRParserTurtle4.pas',
  FHIR.Cql.Model in '..\Libraries\cql\FHIR.Cql.Model.pas',
  FHIR.Cql.Parser in '..\Libraries\cql\FHIR.Cql.Parser.pas',
  FHIR.Cql.Tests in '..\Libraries\cql\FHIR.Cql.Tests.pas',
  RestFulServerTests in 'RestFulServerTests.pas',
  SmartOnFhirTestingLogin in 'SmartOnFhirTestingLogin.pas',
  CertificateSupport in '..\reference-platform\support\CertificateSupport.pas',
  GraphDefinitionEngine in '..\Server\GraphDefinitionEngine.pas',
  GraphDefinitionTests in 'GraphDefinitionTests.pas',
  FHIR.Ucum.Tests in '..\Libraries\Ucum\FHIR.Ucum.Tests.pas',
  FHIR.Misc.ApplicationVerifier in '..\Libraries\security\FHIR.Misc.ApplicationVerifier.pas',
  JWTService in '..\Server\JWTService.pas',
  CDSHooksClientManager in '..\reference-platform\support\CDSHooksClientManager.pas',
  HackingHealthLogic in '..\Server\Modules\HackingHealthLogic.pas',
  FHIR.Utilities.SCrypt in '..\Libraries\security\FHIR.Utilities.SCrypt.pas',
  ApplicationCache in '..\Server\ApplicationCache.pas',
  TerminologyOperations in '..\Server\TerminologyOperations.pas',
  WebSourceProvider in '..\Server\WebSourceProvider.pas',
  FHIRIndexBase in '..\reference-platform\support\FHIRIndexBase.pas',
  FHIR.Database.Tests in '..\Libraries\db\FHIR.Database.Tests.pas',
  FHIR.Database.ODBC in '..\Libraries\db\FHIR.Database.ODBC.pas',
  FHIR.Database.ODBC.Objects in '..\Libraries\db\FHIR.Database.ODBC.Objects.pas',
  FHIR.Database.ODBC.Headers in '..\Libraries\db\FHIR.Database.ODBC.Headers.pas',
  FHIR.Database.SQLite3.Objects in '..\Libraries\db\FHIR.Database.SQLite3.Objects.pas',
  FHIR.Database.SQLite3.Utilities in '..\Libraries\db\FHIR.Database.SQLite3.Utilities.pas',
  FHIR.Database.SQLite3.Wrapper in '..\Libraries\db\FHIR.Database.SQLite3.Wrapper.pas',
  FHIR.Database.SQLite in '..\Libraries\db\FHIR.Database.SQLite.pas',
  ServerPostHandlers in '..\Server\ServerPostHandlers.pas',
  TerminologyServer in '..\Server\TerminologyServer.pas',
  ICD10Services in '..\Server\ICD10Services.pas',
  FHIR.Javascript.Tests in '..\Libraries\js\FHIR.Javascript.Tests.pas',
  FHIR.Javascript.Chakra in '..\Libraries\js\FHIR.Javascript.Chakra.pas',
  FHIR.Javascript in '..\Libraries\js\FHIR.Javascript.pas',
  DigitalSignatureTests in '..\reference-platform\support\Tests\DigitalSignatureTests.pas',
  FHIR.Support.Javascript in '..\Libraries\js\FHIR.Support.Javascript.pas',
  FHIRJavascriptReg4 in '..\reference-platform\r4\FHIRJavascriptReg4.pas',
  FHIR.Javascript.Base in '..\Libraries\js\FHIR.Javascript.Base.pas',
  FHIR.Javascript.ObjectsTests in '..\Libraries\js\FHIR.Javascript.ObjectsTests.pas',
  FHIR.Client.Javascript in '..\Libraries\js\FHIR.Client.Javascript.pas',
  ServerEventJs in '..\Server\ServerEventJs.pas',
  ServerJavascriptHost in '..\Server\ServerJavascriptHost.pas',
  FHIRFactory in '..\reference-platform\support\FHIRFactory.pas',
  IdUriParserTests in 'IdUriParserTests.pas',
  FHIR.Cql.Engine in '..\Libraries\cql\FHIR.Cql.Engine.pas',
  CountryCodeServices in '..\Server\CountryCodeServices.pas',
  USStatesServices in '..\Server\USStatesServices.pas',
  GoogleAnalyticsProvider in '..\Server\GoogleAnalyticsProvider.pas',
  FHIRPathNode4 in '..\reference-platform\r4\FHIRPathNode4.pas',
  UcumServiceInterface in '..\reference-platform\support\UcumServiceInterface.pas',
  FHIRBase4 in '..\reference-platform\r4\FHIRBase4.pas',
  FHIRXhtmlComposer in '..\reference-platform\Support\FHIRXhtmlComposer.pas',
  FHIRParserBase4 in '..\reference-platform\r4\FHIRParserBase4.pas',
  VersionConvertor_30_40 in '..\reference-platform\xversion\VersionConvertor_30_40.pas',
  FHIRResources3 in '..\reference-platform\dstu3\FHIRResources3.pas',
  FHIRBase3 in '..\reference-platform\dstu3\FHIRBase3.pas',
  FHIRTypes3 in '..\reference-platform\dstu3\FHIRTypes3.pas',
  FHIRMetaModel3 in '..\reference-platform\dstu3\FHIRMetaModel3.pas',
  FHIRContext3 in '..\reference-platform\dstu3\FHIRContext3.pas',
  FHIRUtilities3 in '..\reference-platform\dstu3\FHIRUtilities3.pas',
  FHIRConstants3 in '..\reference-platform\dstu3\FHIRConstants3.pas',
  FHIRPathNode3 in '..\reference-platform\dstu3\FHIRPathNode3.pas',
  FHIRProfileUtilities3 in '..\reference-platform\dstu3\FHIRProfileUtilities3.pas',
  VersionConvertorTests in '..\reference-platform\xversion\VersionConvertorTests.pas',
  FHIRParser3 in '..\reference-platform\dstu3\FHIRParser3.pas',
  FHIRParser4 in '..\reference-platform\r4\FHIRParser4.pas',
  FHIRParserXml3 in '..\reference-platform\dstu3\FHIRParserXml3.pas',
  FHIRParserBase3 in '..\reference-platform\dstu3\FHIRParserBase3.pas',
  FHIRParserJson3 in '..\reference-platform\dstu3\FHIRParserJson3.pas',
  FHIRParserTurtle3 in '..\reference-platform\dstu3\FHIRParserTurtle3.pas',
  FhirVersionConvertors in '..\reference-platform\xversion\FhirVersionConvertors.pas',
  FHIRClientBase in '..\reference-platform\client\FHIRClientBase.pas',
  FHIRClientHTTP in '..\reference-platform\client\FHIRClientHTTP.pas',
  FHIRClientThreaded in '..\reference-platform\client\FHIRClientThreaded.pas',
  FHIRClient4 in '..\reference-platform\r4\FHIRClient4.pas',
  FHIR.Java.JNI in '..\Libraries\java\FHIR.Java.JNI.pas',
  FHIR.Java.Runtime in '..\Libraries\java\FHIR.Java.Runtime.pas',
  FHIR.Java.Strings in '..\Libraries\java\FHIR.Java.Strings.pas',
  FHIR.Java.Utilities in '..\Libraries\java\FHIR.Java.Utilities.pas',
  FHIR.Java.Wrapper in '..\Libraries\java\FHIR.Java.Wrapper.pas';

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
