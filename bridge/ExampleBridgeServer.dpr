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
program ExampleBridgeServer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4 in '..\Libraries\FMM\FastMM4.pas',
  System.SysUtils,
  ExampleBridge in 'ExampleBridge.pas',
  FHIR.Support.Collections in '..\reference-platform\support\FHIR.Support.Collections.pas',
  AdvControllers in '..\reference-platform\support\AdvControllers.pas',
  AdvDispatchers in '..\reference-platform\support\AdvDispatchers.pas',
  AdvEvents in '..\reference-platform\support\AdvEvents.pas',
  FHIR.Support.Exceptions in '..\reference-platform\support\FHIR.Support.Exceptions.pas',
  AdvExclusiveCriticalSections in '..\reference-platform\support\AdvExclusiveCriticalSections.pas',
  FHIR.Support.Factory in '..\reference-platform\support\FHIR.Support.Factory.pas',
  FHIR.Support.Filers in '..\reference-platform\support\FHIR.Support.Filers.pas',
  FHIR.Support.Generics in '..\reference-platform\support\FHIR.Support.Generics.pas',
  FHIR.Support.Json in '..\reference-platform\support\FHIR.Support.Json.pas',
  AdvMethods in '..\reference-platform\support\AdvMethods.pas',
  AdvNames in '..\reference-platform\support\AdvNames.pas',
  FHIR.Support.Objects in '..\reference-platform\support\FHIR.Support.Objects.pas',
  AdvParameters in '..\reference-platform\support\AdvParameters.pas',
  AdvProfilers in '..\reference-platform\support\AdvProfilers.pas',
  AdvSignals in '..\reference-platform\support\AdvSignals.pas',
  FHIR.Support.Stream in '..\reference-platform\support\FHIR.Support.Stream.pas',
  AdvThreads in '..\reference-platform\support\AdvThreads.pas',
  FHIR.Support.WInInet in '..\reference-platform\support\FHIR.Support.WInInet.pas',
  FHIR.Support.Binary in '..\reference-platform\support\FHIR.Support.Binary.pas',
  ColourSupport in '..\reference-platform\support\ColourSupport.pas',
  CurrencySupport in '..\reference-platform\support\CurrencySupport.pas',
  FHIR.Support.DateTime in '..\reference-platform\support\FHIR.Support.DateTime.pas',
  FHIR.Support.Decimal in '..\reference-platform\support\FHIR.Support.Decimal.pas',
  ErrorSupport in '..\reference-platform\support\ErrorSupport.pas',
  FHIR.Base.Objects in '..\reference-platform\support\FHIR.Base.Objects.pas',
  FHIR.Misc.Facebook in '..\reference-platform\support\FHIR.Misc.Facebook.pas',
  FHIR.Support.System in '..\reference-platform\support\FHIR.Support.System.pas',
  GUIDSupport in '..\reference-platform\support\GUIDSupport.pas',
  HL7V2DateSupport in '..\reference-platform\support\HL7V2DateSupport.pas',
  HMAC in '..\reference-platform\support\HMAC.pas',
  FHIR.Web.HtmlGen in '..\reference-platform\support\FHIR.Web.HtmlGen.pas',
  HashSupport in '..\reference-platform\support\HashSupport.pas',
  FHIR.Web.Socket in '..\reference-platform\support\FHIR.Web.Socket.pas',
  FHIR.Web.Fetcher in '..\reference-platform\support\FHIR.Web.Fetcher.pas',
  JWT in '..\reference-platform\support\JWT.pas',
  FHIR.Database.Dialects in '..\reference-platform\support\FHIR.Database.Dialects.pas',
  FHIR.Support.Math in '..\reference-platform\support\FHIR.Support.Math.pas',
  MemorySupport in '..\reference-platform\support\MemorySupport.pas',
  FHIR.Support.Mime in '..\reference-platform\support\FHIR.Support.Mime.pas',
  MXmlBuilder in '..\reference-platform\support\MXmlBuilder.pas',
  MsXmlParser in '..\reference-platform\support\MsXmlParser.pas',
  FHIR.Web.ParseMap in '..\reference-platform\support\FHIR.Web.ParseMap.pas',
  FHIR.Web.Rdf in '..\reference-platform\support\FHIR.Web.Rdf.pas',
  ServiceController in '..\reference-platform\support\ServiceController.pas',
  FHIR.Support.Shell in '..\reference-platform\support\FHIR.Support.Shell.pas',
  FHIR.Support.Strings in '..\reference-platform\support\FHIR.Support.Strings.pas',
  FHIR.Support.Service in '..\reference-platform\support\FHIR.Support.Service.pas',
  SystemSupport in '..\reference-platform\support\SystemSupport.pas',
  TextUtilities in '..\reference-platform\support\TextUtilities.pas',
  ThreadSupport in '..\reference-platform\support\ThreadSupport.pas',
  FHIR.Support.Lock in '..\reference-platform\support\FHIR.Support.Lock.pas',
  libeay32 in '..\reference-platform\support\libeay32.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  FHIR.CdsHooks.Utilities in '..\reference-platform\support\FHIR.CdsHooks.Utilities.pas',
  FHIR.R3.AuthMap in '..\reference-platform\dstu3\FHIR.R3.AuthMap.pas',
  FHIR.Tools.Client in '..\reference-platform\client\FHIR.Tools.Client.pas',
  FHIR.R3.Constants in '..\reference-platform\dstu3\FHIR.R3.Constants.pas',
  FHIR.R3.Context in '..\reference-platform\dstu3\FHIR.R3.Context.pas',
  FHIR.R3.IndexInfo in '..\reference-platform\dstu3\FHIR.R3.IndexInfo.pas',
  FHIR.Base.Lang in '..\reference-platform\support\FHIR.Base.Lang.pas',
  FHIR.R3.ElementModel in '..\reference-platform\dstu3\FHIR.R3.ElementModel.pas',
  FHIR.R3.Operations in '..\reference-platform\dstu3\FHIR.R3.Operations.pas',
  FHIR.Tools.Parser in '..\reference-platform\support\FHIR.Tools.Parser.pas',
  FHIR.R3.Xml in '..\reference-platform\dstu3\FHIR.R3.Xml.pas',
  FHIR.R3.Json in '..\reference-platform\dstu3\FHIR.R3.Json.pas',
  FHIR.R3.Turtle in '..\reference-platform\dstu3\FHIR.R3.Turtle.pas',
  FHIR.Base.Parser in '..\reference-platform\support\FHIR.Base.Parser.pas',
  FHIR.R3.Profiles in '..\reference-platform\dstu3\FHIR.R3.Profiles.pas',
  FHIR.R3.Resources in '..\reference-platform\dstu3\FHIR.R3.Resources.pas',
  FHIR.Tools.Security in '..\reference-platform\support\FHIR.Tools.Security.pas',
  FHIR.Tools.Session in '..\reference-platform\support\FHIR.Tools.Session.pas',
  FHIR.R3.Tags in '..\reference-platform\dstu3\FHIR.R3.Tags.pas',
  FHIR.R3.Types in '..\reference-platform\dstu3\FHIR.R3.Types.pas',
  FHIR.R3.Utilities in '..\reference-platform\dstu3\FHIR.R3.Utilities.pas',
  FHIR.R3.Validator in '..\reference-platform\dstu3\FHIR.R3.Validator.pas',
  FHIR.Base.Xhtml in '..\reference-platform\support\FHIR.Base.Xhtml.pas',
  FHIR.R3.OpBase in '..\reference-platform\dstu3\FHIR.R3.OpBase.pas',
  FHIR.R3.PathEngine in '..\reference-platform\dstu3\FHIR.R3.PathEngine.pas',
  FHIR.R3.Narrative2 in '..\reference-platform\dstu3\FHIR.R3.Narrative2.pas',
  FHIR.R3.Questionnaire in '..\reference-platform\dstu3\FHIR.R3.Questionnaire.pas',
  FHIR.Base.Scim in '..\reference-platform\support\FHIR.Base.Scim.pas',
  FHIR.Client.SmartUtilities in '..\reference-platform\client\FHIR.Client.SmartUtilities.pas',
  TerminologyServices in '..\Libraries\TerminologyServices.pas',
  FHIR.Database.Logging in '..\Libraries\db\FHIR.Database.Logging.pas',
  FHIR.Database.Manager in '..\Libraries\db\FHIR.Database.Manager.pas',
  FHIR.Database.Settings in '..\Libraries\db\FHIR.Database.Settings.pas',
  FHIR.Loinc.Publisher in '..\Libraries\loinc\FHIR.Loinc.Publisher.pas',
  FHIR.Loinc.Services in '..\Libraries\loinc\FHIR.Loinc.Services.pas',
  FHIR.Misc.Twilio in '..\Libraries\security\FHIR.Misc.Twilio.pas',
  FHIR.Snomed.Analysis in '..\Libraries\snomed\FHIR.Snomed.Analysis.pas',
  FHIR.Snomed.Expressions in '..\Libraries\snomed\FHIR.Snomed.Expressions.pas',
  FHIR.Snomed.Publisher in '..\Libraries\snomed\FHIR.Snomed.Publisher.pas',
  FHIR.Snomed.Services in '..\Libraries\snomed\FHIR.Snomed.Services.pas',
  DISystemCompat in '..\Libraries\stem\DISystemCompat.pas',
  YuStemmer in '..\Libraries\stem\YuStemmer.pas',
  FHIR.Ucum.Base in '..\Libraries\Ucum\FHIR.Ucum.Base.pas',
  FHIR.Ucum.Expressions in '..\Libraries\Ucum\FHIR.Ucum.Expressions.pas',
  FHIR.Ucum.Handlers in '..\Libraries\Ucum\FHIR.Ucum.Handlers.pas',
  FHIR.Ucum.Search in '..\Libraries\Ucum\FHIR.Ucum.Search.pas',
  FHIR.Ucum.Services in '..\Libraries\Ucum\FHIR.Ucum.Services.pas',
  FHIR.Ucum.Validators in '..\Libraries\Ucum\FHIR.Ucum.Validators.pas',
  ACIRServices in '..\Server\ACIRServices.pas',
  AreaCodeServices in '..\Server\AreaCodeServices.pas',
  AuthServer in '..\Server\AuthServer.pas',
  ClosureManager in '..\Server\ClosureManager.pas',
  FHIRIndexManagers in '..\Server\FHIRIndexManagers.pas',
  FHIR.Debug.Logging in '..\reference-platform\support\FHIR.Debug.Logging.pas',
  FHIRRestServer in '..\Server\FHIRRestServer.pas',
  FHIRServerConstants in '..\Server\FHIRServerConstants.pas',
  FHIRServerContext in '..\Server\FHIRServerContext.pas',
  FHIRServerUtilities in '..\Server\FHIRServerUtilities.pas',
  FHIRSessionManager in '..\Server\FHIRSessionManager.pas',
  FHIRStorageService in '..\Server\FHIRStorageService.pas',
  FHIRSubscriptionManager in '..\Server\FHIRSubscriptionManager.pas',
  FHIRTagManager in '..\Server\FHIRTagManager.pas',
  FHIRValueSetChecker in '..\Server\FHIRValueSetChecker.pas',
  FHIRValueSetExpander in '..\Server\FHIRValueSetExpander.pas',
  IETFLanguageCodeServices in '..\Server\IETFLanguageCodeServices.pas',
  OpenMHealthServer in '..\Server\OpenMHealthServer.pas',
  ReverseClient in '..\Server\ReverseClient.pas',
  RxNormServices in '..\Server\RxNormServices.pas',
  SCIMSearch in '..\Server\SCIMSearch.pas',
  SCIMServer in '..\Server\SCIMServer.pas',
  ServerAdaptations in '..\Server\ServerAdaptations.pas',
  ServerUtilities in '..\Server\ServerUtilities.pas',
  ServerValidator in '..\Server\ServerValidator.pas',
  TerminologyServer in '..\Server\TerminologyServer.pas',
  TerminologyServerStore in '..\Server\TerminologyServerStore.pas',
  TerminologyWebServer in '..\Server\TerminologyWebServer.pas',
  UniiServices in '..\Server\UniiServices.pas',
  UriServices in '..\Server\UriServices.pas',
  logging in '..\Server\logging.pas',
  FHIRUserProvider in '..\Server\FHIRUserProvider.pas',
  FastMM4Messages in '..\Libraries\FMM\FastMM4Messages.pas',
  GraphQL in '..\reference-platform\support\GraphQL.pas',
  FHIR.Tools.GraphQL in '..\reference-platform\support\FHIR.Tools.GraphQL.pas',
  ParserSupport in '..\reference-platform\support\ParserSupport.pas',
  FHIR.Support.MXml in '..\reference-platform\support\FHIR.Support.MXml.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  CDSHooksServer in '..\Server\CDSHooksServer.pas',
  FHIR.Support.Turtle in '..\reference-platform\support\FHIR.Support.Turtle.pas',
  CertificateSupport in '..\reference-platform\support\CertificateSupport.pas',
  FHIR.Misc.ApplicationVerifier in '..\Libraries\security\FHIR.Misc.ApplicationVerifier.pas',
  JWTService in '..\Server\JWTService.pas',
  FHIR.Utilities.SCrypt in '..\Libraries\security\FHIR.Utilities.SCrypt.pas',
  ApplicationCache in '..\Server\ApplicationCache.pas',
  FHIR.CdsHooks.Client in '..\reference-platform\support\FHIR.CdsHooks.Client.pas',
  WebSourceProvider in '..\Server\WebSourceProvider.pas',
  FHIR.Tools.Indexing in '..\reference-platform\support\FHIR.Tools.Indexing.pas',
  ICD10Services in '..\Server\ICD10Services.pas',
  ServerPostHandlers in '..\Server\ServerPostHandlers.pas',
  ServerJavascriptHost in '..\Server\ServerJavascriptHost.pas',
  FHIR.R3.Javascript in '..\reference-platform\dstu3\FHIR.R3.Javascript.pas',
  FHIR.Javascript.Base in '..\Libraries\js\FHIR.Javascript.Base.pas',
  FHIR.Client.Javascript in '..\Libraries\js\FHIR.Client.Javascript.pas',
  ServerEventJs in '..\Server\ServerEventJs.pas',
  FHIR.Javascript in '..\Libraries\js\FHIR.Javascript.pas',
  FHIR.Javascript.Chakra in '..\Libraries\js\FHIR.Javascript.Chakra.pas',
  FHIR.Support.Javascript in '..\Libraries\js\FHIR.Support.Javascript.pas',
  FHIR.Support.Signatures in '..\reference-platform\support\FHIR.Support.Signatures.pas',
  FHIR.Tools.Factory in '..\reference-platform\support\FHIR.Tools.Factory.pas',
  CountryCodeServices in '..\Server\CountryCodeServices.pas',
  USStatesServices in '..\Server\USStatesServices.pas',
  FHIR.R3.PathNode in '..\reference-platform\dstu3\FHIR.R3.PathNode.pas',
  GoogleAnalyticsProvider in '..\Server\GoogleAnalyticsProvider.pas',
  FHIR.Ucum.IFace in '..\reference-platform\support\FHIR.Ucum.IFace.pas',
  FHIR.Tools.XhtmlComp in '..\reference-platform\Support\FHIR.Tools.XhtmlComp.pas',
  FHIR.R3.ParserBase in '..\reference-platform\dstu3\FHIR.R3.ParserBase.pas',
  FHIR.R3.Base in '..\reference-platform\dstu3\FHIR.R3.Base.pas',
  FHIR.R3.Parser in '..\reference-platform\dstu3\FHIR.R3.Parser.pas',
  FHIR.Client.Base in '..\reference-platform\client\FHIR.Client.Base.pas',
  FHIR.Client.HTTP in '..\reference-platform\client\FHIR.Client.HTTP.pas',
  FHIR.Client.Threaded in '..\reference-platform\client\FHIR.Client.Threaded.pas',
  FHIR.R3.Client in '..\reference-platform\dstu3\FHIR.R3.Client.pas',
  FHIR.Support.Text in '..\reference-platform\support\FHIR.Support.Text.pas',
  FHIR.Support.Xml in '..\reference-platform\support\FHIR.Support.Xml.pas',
  FHIR.Support.Zip in '..\reference-platform\support\FHIR.Support.Zip.pas';

var
  gsrv : TExampleFhirServer;
begin
  try
    gsrv := TExampleFhirServer.create;
    try
      gsrv.Port := 971;
      gsrv.SystemName := 'Example Bridge Server';
      gsrv.SystemUID := 'urn:uuid:f374f5fa-5ba2-4bbf-8c44-ce026b9f9772';
      gsrv.Start;
      try
        writeln('FHIR Server running on port 971');
        writeln('Press enter to stop');
        readln;
      finally
        gsrv.stop;
      end;
    finally
      gsrv.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
