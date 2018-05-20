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
  FHIR.Support.Strings in '..\reference-platform\Support\FHIR.Support.Strings.pas',
  FHIR.Support.Math in '..\reference-platform\Support\FHIR.Support.Math.pas',
  FHIR.Support.DateTime in '..\reference-platform\Support\FHIR.Support.DateTime.pas',
  FHIR.Support.Binary in '..\reference-platform\Support\FHIR.Support.Binary.pas',
  FHIR.Support.Objects in '..\reference-platform\Support\FHIR.Support.Objects.pas',
  FHIR.Support.Exceptions in '..\reference-platform\Support\FHIR.Support.Exceptions.pas',
  FHIR.Support.System in '..\reference-platform\Support\FHIR.Support.System.pas',
  FHIR.Support.Collections in '..\reference-platform\Support\FHIR.Support.Collections.pas',
  FHIR.Support.Stream in '..\reference-platform\Support\FHIR.Support.Stream.pas',
  FHIR.Web.Parsers in '..\reference-platform\Support\FHIR.Web.Parsers.pas',
  FHIR.Support.Zip in '..\reference-platform\Support\FHIR.Support.Zip.pas',
  FHIR.Support.Decimal in '..\reference-platform\Support\FHIR.Support.Decimal.pas',
  FHIR.Support.WInInet in '..\reference-platform\Support\FHIR.Support.WInInet.pas',
  FHIR.Support.JSON in '..\reference-platform\Support\FHIR.Support.JSON.pas',
  FHIRIndexManagers in '..\server\FHIRIndexManagers.pas',
  FHIR.Ucum.Services in '..\Libraries\Ucum\FHIR.Ucum.Services.pas',
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
  FHIR.Web.Fetcher in '..\reference-platform\Support\FHIR.Web.Fetcher.pas',
  FHIR.Misc.Facebook in '..\reference-platform\Support\FHIR.Misc.Facebook.pas',
  FHIR.Support.Service in '..\reference-platform\Support\FHIR.Support.Service.pas',
  DBInstaller in '..\server\DBInstaller.pas',
  FHIR.Database.Dialects in '..\reference-platform\Support\FHIR.Database.Dialects.pas',
  FHIR.Database.Logging in '..\Libraries\db\FHIR.Database.Logging.pas',
  FHIR.Database.Manager in '..\Libraries\db\FHIR.Database.Manager.pas',
  FHIR.Database.Utilities in '..\Libraries\db\FHIR.Database.Utilities.pas',
  FHIR.Database.Settings in '..\Libraries\db\FHIR.Database.Settings.pas',
  FHIRNativeStorage in '..\Server\FHIRNativeStorage.pas',
  FHIR.Snomed.Importer in '..\Libraries\snomed\FHIR.Snomed.Importer.pas',
  FHIR.Snomed.Publisher in '..\Libraries\snomed\FHIR.Snomed.Publisher.pas',
  FHIR.Snomed.Expressions in '..\Libraries\snomed\FHIR.Snomed.Expressions.pas',
  FHIR.Web.HtmlGen in '..\reference-platform\Support\FHIR.Web.HtmlGen.pas',
  FHIR.Loinc.Importer in '..\Libraries\loinc\FHIR.Loinc.Importer.pas',
  FHIR.Loinc.Publisher in '..\Libraries\loinc\FHIR.Loinc.Publisher.pas',
  TerminologyServerStore in '..\server\TerminologyServerStore.pas',
  FHIR.Tx.Service in '..\Libraries\FHIR.Tx.Service.pas',
  FHIRValueSetChecker in '..\server\FHIRValueSetChecker.pas',
  TerminologyWebServer in '..\server\TerminologyWebServer.pas',
  FHIR.Support.Factory in '..\reference-platform\support\FHIR.Support.Factory.pas',
  FHIR.Support.Text in '..\reference-platform\support\FHIR.Support.Text.pas',
  FHIRServerConstants in '..\server\FHIRServerConstants.pas',
  FHIRServerUtilities in '..\server\FHIRServerUtilities.pas',
  SearchProcessor in '..\server\SearchProcessor.pas',
  AuthServer in '..\server\AuthServer.pas',
  SCIMServer in '..\server\SCIMServer.pas',
  SCIMSearch in '..\server\SCIMSearch.pas',
  FHIR.Misc.Twilio in '..\Libraries\security\FHIR.Misc.Twilio.pas',
  FHIRSearchSyntax in '..\server\FHIRSearchSyntax.pas',
  FHIR.Support.Shell in '..\reference-platform\Support\FHIR.Support.Shell.pas',
  RectSupport in '..\server\RectSupport.pas',
  CoordinateSupport in '..\server\CoordinateSupport.pas',
  FHIR.Support.Generics in '..\reference-platform\Support\FHIR.Support.Generics.pas',
  FHIR.Support.Signatures in '..\reference-platform\Support\FHIR.Support.Signatures.pas',
  UriServices in '..\server\UriServices.pas',
  UniiServices in '..\server\UniiServices.pas',
  RxNormServices in '..\server\RxNormServices.pas',
  FHIR.Snomed.Analysis in '..\Libraries\snomed\FHIR.Snomed.Analysis.pas',
  AreaCodeServices in '..\server\AreaCodeServices.pas',
  FHIRSubscriptionManager in '..\server\FHIRSubscriptionManager.pas',
  ServerValidator in '..\server\ServerValidator.pas',
  FHIR.Web.Socket in '..\reference-platform\Support\FHIR.Web.Socket.pas',
  FHIR.Support.Mime in '..\reference-platform\Support\FHIR.Support.Mime.pas',
  FHIR.Support.Lock in '..\reference-platform\Support\FHIR.Support.Lock.pas',
  FHIR.R4.Questionnaire in '..\reference-platform\r4\FHIR.R4.Questionnaire.pas',
  FHIR.Base.Scim in '..\reference-platform\base\FHIR.Base.Scim.pas',
  FHIR.R4.Narrative2 in '..\reference-platform\r4\FHIR.R4.Narrative2.pas',
  FHIR.Tools.Security in '..\reference-platform\tools\FHIR.Tools.Security.pas',
  FHIR.R4.Narrative in '..\reference-platform\r4\FHIR.R4.Narrative.pas',
  FHIR.Client.SmartUtilities in '..\reference-platform\client\FHIR.Client.SmartUtilities.pas',
  FHIR.R4.PathEngine in '..\reference-platform\r4\FHIR.R4.PathEngine.pas',
  FHIR.R4.Tags in '..\reference-platform\r4\FHIR.R4.Tags.pas',
  FHIR.R4.Profiles in '..\reference-platform\r4\FHIR.R4.Profiles.pas',
  FHIR.Base.Objects in '..\reference-platform\base\FHIR.Base.Objects.pas',
  FHIR.R4.Types in '..\reference-platform\r4\FHIR.R4.Types.pas',
  FHIR.R4.Resources in '..\reference-platform\r4\FHIR.R4.Resources.pas',
  FHIR.Tools.Parser in '..\reference-platform\tools\FHIR.Tools.Parser.pas',
  FHIR.Base.Parser in '..\reference-platform\base\FHIR.Base.Parser.pas',
  FHIR.R4.Constants in '..\reference-platform\r4\FHIR.R4.Constants.pas',
  FHIR.Tools.Session in '..\reference-platform\tools\FHIR.Tools.Session.pas',
  FHIR.Base.Lang in '..\reference-platform\base\FHIR.Base.Lang.pas',
  FHIR.R4.Utilities in '..\reference-platform\r4\FHIR.R4.Utilities.pas',
  FHIR.Tools.Client in '..\reference-platform\client\FHIR.Tools.Client.pas',
  FHIR.R4.Validator in '..\reference-platform\r4\FHIR.R4.Validator.pas',
  ClosureManager in '..\server\ClosureManager.pas',
  FHIR.CdsHooks.Utilities in '..\reference-platform\support\FHIR.CdsHooks.Utilities.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  AccessControlEngine in '..\server\AccessControlEngine.pas',
  MPISearch in '..\server\MPISearch.pas',
  FHIR.Web.Rdf in '..\reference-platform\support\FHIR.Web.Rdf.pas',
  FHIR.R4.Operations in '..\reference-platform\r4\FHIR.R4.Operations.pas',
  FHIR.R4.OpBase in '..\reference-platform\r4\FHIR.R4.OpBase.pas',
  FHIR.R4.IndexInfo in '..\reference-platform\r4\FHIR.R4.IndexInfo.pas',
  FHIR.R4.ElementModel in '..\reference-platform\r4\FHIR.R4.ElementModel.pas',
  FHIR.Base.Xhtml in '..\reference-platform\base\FHIR.Base.Xhtml.pas',
  FHIR.R4.MapUtilities in '..\reference-platform\r4\FHIR.R4.MapUtilities.pas',
  FHIR.R4.Context in '..\reference-platform\r4\FHIR.R4.Context.pas',
  FHIR.Debug.Logging in '..\reference-platform\support\FHIR.Debug.Logging.pas',
  FHIR.R4.AuthMap in '..\reference-platform\r4\FHIR.R4.AuthMap.pas',
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  ServerUtilities in '..\Server\ServerUtilities.pas',
  ServerAdaptations in '..\Server\ServerAdaptations.pas',
  ObservationStatsEvaluator in '..\Server\ObservationStatsEvaluator.pas',
  OpenMHealthServer in '..\Server\OpenMHealthServer.pas',
  FHIR.Tools.DiffEngine in '..\reference-platform\tools\FHIR.Tools.DiffEngine.pas',
  ACIRServices in '..\Server\ACIRServices.pas',
  IETFLanguageCodeServices in '..\Server\IETFLanguageCodeServices.pas',
  ReverseClient in '..\Server\ReverseClient.pas',
  FHIRUserProvider in '..\Server\FHIRUserProvider.pas',
  FHIRServerContext in '..\Server\FHIRServerContext.pas',
  FHIRTagManager in '..\Server\FHIRTagManager.pas',
  FHIRSessionManager in '..\Server\FHIRSessionManager.pas',
  FHIRStorageService in '..\Server\FHIRStorageService.pas',
  FHIR.Tools.GraphQL in '..\reference-platform\tools\FHIR.Tools.GraphQL.pas',
  JsonTests in '..\reference-platform\support\Tests\JsonTests.pas',
  XmlTests in '..\reference-platform\support\Tests\XmlTests.pas',
  GraphQLTests in 'GraphQLTests.pas',
  SnomedTests in 'SnomedTests.pas',
  DecimalTests in '..\reference-platform\support\tests\DecimalTests.pas',
  IETFLangTests in 'IETFLangTests.pas',
  JWTTests in '..\reference-platform\support\Tests\JWTTests.pas',
  DifferenceEngineTests in '..\reference-platform\support\tests\DifferenceEngineTests.pas',
  FHIR.R4.Tests.Client in '..\reference-platform\r4\tests\FHIR.R4.Tests.Client.pas',
  FHIR.R4.Tests.Parser in '..\reference-platform\r4\tests\FHIR.R4.Tests.Parser.pas',
  FHIR.R4.Tests.PathEngine in '..\reference-platform\r4\tests\FHIR.R4.Tests.PathEngine.pas',
  FHIR.R4.Tests.Validator in '..\reference-platform\r4\tests\FHIR.R4.Tests.Validator.pas',
  FHIR.R4.Tests.Maps in '..\reference-platform\r4\tests\FHIR.R4.Tests.Maps.pas',
  FHIR.R4.Tests.Utilities in '..\reference-platform\r4\tests\FHIR.R4.Tests.Utilities.pas',
  FHIR.Support.MXml in '..\reference-platform\support\FHIR.Support.MXml.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  OSXTests in '..\reference-platform\support\Tests\OSXTests.pas',
  FHIR.Support.Osx in '..\reference-platform\support\FHIR.Support.Osx.pas',
  FHIR.Tools.CodeGen in '..\reference-platform\tools\FHIR.Tools.CodeGen.pas',
  CDSHooksServices in '..\Server\CDSHooksServices.pas',
  CDSHooksServer in '..\Server\CDSHooksServer.pas',
  FHIR.Support.Turtle in '..\reference-platform\support\FHIR.Support.Turtle.pas',
  TurtleTests in '..\reference-platform\support\Tests\TurtleTests.pas',
  FHIR.R4.Xml in '..\reference-platform\r4\FHIR.R4.Xml.pas',
  FHIR.R4.Json in '..\reference-platform\r4\FHIR.R4.Json.pas',
  FHIR.R4.Turtle in '..\reference-platform\r4\FHIR.R4.Turtle.pas',
  FHIR.Cql.Model in '..\Libraries\cql\FHIR.Cql.Model.pas',
  FHIR.Cql.Parser in '..\Libraries\cql\FHIR.Cql.Parser.pas',
  FHIR.Cql.Tests in '..\Libraries\cql\FHIR.Cql.Tests.pas',
  RestFulServerTests in 'RestFulServerTests.pas',
  SmartOnFhirTestingLogin in 'SmartOnFhirTestingLogin.pas',
  FHIR.Support.Certs in '..\reference-platform\support\FHIR.Support.Certs.pas',
  GraphDefinitionEngine in '..\Server\GraphDefinitionEngine.pas',
  GraphDefinitionTests in 'GraphDefinitionTests.pas',
  FHIR.Ucum.Tests in '..\Libraries\Ucum\FHIR.Ucum.Tests.pas',
  FHIR.Misc.ApplicationVerifier in '..\Libraries\security\FHIR.Misc.ApplicationVerifier.pas',
  JWTService in '..\Server\JWTService.pas',
  FHIR.CdsHooks.Client in '..\reference-platform\support\FHIR.CdsHooks.Client.pas',
  HackingHealthLogic in '..\Server\Modules\HackingHealthLogic.pas',
  FHIR.Utilities.SCrypt in '..\Libraries\security\FHIR.Utilities.SCrypt.pas',
  ApplicationCache in '..\Server\ApplicationCache.pas',
  TerminologyOperations in '..\Server\TerminologyOperations.pas',
  WebSourceProvider in '..\Server\WebSourceProvider.pas',
  FHIR.Tools.Indexing in '..\reference-platform\tools\FHIR.Tools.Indexing.pas',
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
  FHIR.R4.Javascript in '..\reference-platform\r4\FHIR.R4.Javascript.pas',
  FHIR.Javascript.Base in '..\Libraries\js\FHIR.Javascript.Base.pas',
  FHIR.Javascript.ObjectsTests in '..\Libraries\js\FHIR.Javascript.ObjectsTests.pas',
  FHIR.Client.Javascript in '..\Libraries\js\FHIR.Client.Javascript.pas',
  ServerEventJs in '..\Server\ServerEventJs.pas',
  ServerJavascriptHost in '..\Server\ServerJavascriptHost.pas',
  IdUriParserTests in 'IdUriParserTests.pas',
  FHIR.Cql.Engine in '..\Libraries\cql\FHIR.Cql.Engine.pas',
  CountryCodeServices in '..\Server\CountryCodeServices.pas',
  USStatesServices in '..\Server\USStatesServices.pas',
  GoogleAnalyticsProvider in '..\Server\GoogleAnalyticsProvider.pas',
  FHIR.R4.PathNode in '..\reference-platform\r4\FHIR.R4.PathNode.pas',
  FHIR.Ucum.IFace in '..\reference-platform\support\FHIR.Ucum.IFace.pas',
  FHIR.R4.Base in '..\reference-platform\r4\FHIR.R4.Base.pas',
  FHIR.Tools.XhtmlComp in '..\reference-platform\tools\FHIR.Tools.XhtmlComp.pas',
  FHIR.R4.ParserBase in '..\reference-platform\r4\FHIR.R4.ParserBase.pas',
  FHIR.XVersion.Conv_30_40 in '..\reference-platform\xversion\FHIR.XVersion.Conv_30_40.pas',
  FHIR.R3.Resources in '..\reference-platform\dstu3\FHIR.R3.Resources.pas',
  FHIR.R3.Base in '..\reference-platform\dstu3\FHIR.R3.Base.pas',
  FHIR.R3.Types in '..\reference-platform\dstu3\FHIR.R3.Types.pas',
  FHIR.R3.ElementModel in '..\reference-platform\dstu3\FHIR.R3.ElementModel.pas',
  FHIR.R3.Context in '..\reference-platform\dstu3\FHIR.R3.Context.pas',
  FHIR.R3.Utilities in '..\reference-platform\dstu3\FHIR.R3.Utilities.pas',
  FHIR.R3.Constants in '..\reference-platform\dstu3\FHIR.R3.Constants.pas',
  FHIR.R3.PathNode in '..\reference-platform\dstu3\FHIR.R3.PathNode.pas',
  FHIR.R3.Profiles in '..\reference-platform\dstu3\FHIR.R3.Profiles.pas',
  FHIR.XVersion.Tests in '..\reference-platform\xversion\FHIR.XVersion.Tests.pas',
  FHIR.R3.Parser in '..\reference-platform\dstu3\FHIR.R3.Parser.pas',
  FHIR.R4.Parser in '..\reference-platform\r4\FHIR.R4.Parser.pas',
  FHIR.R3.Xml in '..\reference-platform\dstu3\FHIR.R3.Xml.pas',
  FHIR.R3.ParserBase in '..\reference-platform\dstu3\FHIR.R3.ParserBase.pas',
  FHIR.R3.Json in '..\reference-platform\dstu3\FHIR.R3.Json.pas',
  FHIR.R3.Turtle in '..\reference-platform\dstu3\FHIR.R3.Turtle.pas',
  FHIR.XVersion.Convertors in '..\reference-platform\xversion\FHIR.XVersion.Convertors.pas',
  FHIR.Client.Base in '..\reference-platform\client\FHIR.Client.Base.pas',
  FHIR.Client.HTTP in '..\reference-platform\client\FHIR.Client.HTTP.pas',
  FHIR.Client.Threaded in '..\reference-platform\client\FHIR.Client.Threaded.pas',
  FHIR.R4.Client in '..\reference-platform\r4\FHIR.R4.Client.pas',
  FHIR.Java.JNI in '..\Libraries\java\FHIR.Java.JNI.pas',
  FHIR.Java.Runtime in '..\Libraries\java\FHIR.Java.Runtime.pas',
  FHIR.Java.Strings in '..\Libraries\java\FHIR.Java.Strings.pas',
  FHIR.Java.Utilities in '..\Libraries\java\FHIR.Java.Utilities.pas',
  FHIR.Java.Wrapper in '..\Libraries\java\FHIR.Java.Wrapper.pas',
  FHIR.Support.Xml in '..\reference-platform\support\FHIR.Support.Xml.pas',
  FHIR.Support.Controllers in '..\reference-platform\support\FHIR.Support.Controllers.pas',
  FHIR.Misc.GraphQL in '..\reference-platform\support\FHIR.Misc.GraphQL.pas',
  FHIR.Support.MsXml in '..\reference-platform\support\FHIR.Support.MsXml.pas',
  FHIR.Base.Factory in '..\reference-platform\base\FHIR.Base.Factory.pas',
  FHIR.Base.Validator in '..\reference-platform\base\FHIR.Base.Validator.pas',
  FHIR.XVersion.Resources in '..\reference-platform\xversion\FHIR.XVersion.Resources.pas',
  FHIR.Base.Narrative in '..\reference-platform\base\FHIR.Base.Narrative.pas',
  FHIR.Base.PathEngine in '..\reference-platform\base\FHIR.Base.PathEngine.pas',
  FHIR.R4.Common in '..\reference-platform\r4\FHIR.R4.Common.pas',
  FHIR.R3.Common in '..\reference-platform\dstu3\FHIR.R3.Common.pas',
  FHIR.R4.Tests.Worker in '..\reference-platform\r4\tests\FHIR.R4.Tests.Worker.pas',
  FHIR.R4.Factory in '..\reference-platform\r4\FHIR.R4.Factory.pas',
  FHIR.Cache.PackageManager in '..\reference-platform\cache\FHIR.Cache.PackageManager.pas',
  FHIR.Support.Tarball in '..\reference-platform\support\FHIR.Support.Tarball.pas',
  FHIR.Base.Utilities in '..\reference-platform\base\FHIR.Base.Utilities.pas';

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
