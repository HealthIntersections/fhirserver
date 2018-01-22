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
  AdvBuffers in '..\reference-platform\support\AdvBuffers.pas',
  AdvCSVExtractors in '..\reference-platform\support\AdvCSVExtractors.pas',
  AdvCSVFormatters in '..\reference-platform\support\AdvCSVFormatters.pas',
  AdvCharacterSets in '..\reference-platform\support\AdvCharacterSets.pas',
  AdvClassHashes in '..\reference-platform\support\AdvClassHashes.pas',
  AdvClassLists in '..\reference-platform\support\AdvClassLists.pas',
  AdvCollections in '..\reference-platform\support\AdvCollections.pas',
  AdvControllers in '..\reference-platform\support\AdvControllers.pas',
  AdvDispatchers in '..\reference-platform\support\AdvDispatchers.pas',
  AdvEvents in '..\reference-platform\support\AdvEvents.pas',
  AdvExceptions in '..\reference-platform\support\AdvExceptions.pas',
  AdvExclusiveCriticalSections in '..\reference-platform\support\AdvExclusiveCriticalSections.pas',
  AdvExtractors in '..\reference-platform\support\AdvExtractors.pas',
  AdvFactories in '..\reference-platform\support\AdvFactories.pas',
  AdvFilers in '..\reference-platform\support\AdvFilers.pas',
  AdvFiles in '..\reference-platform\support\AdvFiles.pas',
  AdvFormatters in '..\reference-platform\support\AdvFormatters.pas',
  AdvGenerics in '..\reference-platform\support\AdvGenerics.pas',
  AdvHashes in '..\reference-platform\support\AdvHashes.pas',
  AdvIntegerLists in '..\reference-platform\support\AdvIntegerLists.pas',
  AdvIntegerMatches in '..\reference-platform\support\AdvIntegerMatches.pas',
  AdvIntegerObjectMatches in '..\reference-platform\support\AdvIntegerObjectMatches.pas',
  AdvItems in '..\reference-platform\support\AdvItems.pas',
  AdvIterators in '..\reference-platform\support\AdvIterators.pas',
  AdvJSON in '..\reference-platform\support\AdvJSON.pas',
  AdvLargeIntegerMatches in '..\reference-platform\support\AdvLargeIntegerMatches.pas',
  AdvMemories in '..\reference-platform\support\AdvMemories.pas',
  AdvMethods in '..\reference-platform\support\AdvMethods.pas',
  AdvNameBuffers in '..\reference-platform\support\AdvNameBuffers.pas',
  AdvNames in '..\reference-platform\support\AdvNames.pas',
  AdvObjectLists in '..\reference-platform\support\AdvObjectLists.pas',
  AdvObjectMatches in '..\reference-platform\support\AdvObjectMatches.pas',
  AdvObjects in '..\reference-platform\support\AdvObjects.pas',
  AdvOrdinalSets in '..\reference-platform\support\AdvOrdinalSets.pas',
  AdvParameters in '..\reference-platform\support\AdvParameters.pas',
  AdvPersistentLists in '..\reference-platform\support\AdvPersistentLists.pas',
  AdvPersistents in '..\reference-platform\support\AdvPersistents.pas',
  AdvPointers in '..\reference-platform\support\AdvPointers.pas',
  AdvProfilers in '..\reference-platform\support\AdvProfilers.pas',
  AdvSignals in '..\reference-platform\support\AdvSignals.pas',
  AdvStreamFilers in '..\reference-platform\support\AdvStreamFilers.pas',
  AdvStreamReaders in '..\reference-platform\support\AdvStreamReaders.pas',
  AdvStreams in '..\reference-platform\support\AdvStreams.pas',
  AdvStringBuilders in '..\reference-platform\support\AdvStringBuilders.pas',
  AdvStringHashes in '..\reference-platform\support\AdvStringHashes.pas',
  AdvStringIntegerMatches in '..\reference-platform\support\AdvStringIntegerMatches.pas',
  AdvStringLargeIntegerMatches in '..\reference-platform\support\AdvStringLargeIntegerMatches.pas',
  AdvStringLists in '..\reference-platform\support\AdvStringLists.pas',
  AdvStringMatches in '..\reference-platform\support\AdvStringMatches.pas',
  AdvStringObjectMatches in '..\reference-platform\support\AdvStringObjectMatches.pas',
  AdvStringStreams in '..\reference-platform\support\AdvStringStreams.pas',
  AdvTextExtractors in '..\reference-platform\support\AdvTextExtractors.pas',
  AdvTextFormatters in '..\reference-platform\support\AdvTextFormatters.pas',
  AdvThreads in '..\reference-platform\support\AdvThreads.pas',
  AdvVCLStreams in '..\reference-platform\support\AdvVCLStreams.pas',
  AdvWinInetClients in '..\reference-platform\support\AdvWinInetClients.pas',
  AdvXMLEntities in '..\reference-platform\support\AdvXMLEntities.pas',
  AdvXMLFormatters in '..\reference-platform\support\AdvXMLFormatters.pas',
  AdvXmlBuilders in '..\reference-platform\support\AdvXmlBuilders.pas',
  AdvZipDeclarations in '..\reference-platform\support\AdvZipDeclarations.pas',
  AdvZipParts in '..\reference-platform\support\AdvZipParts.pas',
  AdvZipReaders in '..\reference-platform\support\AdvZipReaders.pas',
  AdvZipUtilities in '..\reference-platform\support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\reference-platform\support\AdvZipWorkers.pas',
  AdvZipWriters in '..\reference-platform\support\AdvZipWriters.pas',
  AfsResourceVolumes in '..\reference-platform\support\AfsResourceVolumes.pas',
  AfsStreamManagers in '..\reference-platform\support\AfsStreamManagers.pas',
  AfsVolumes in '..\reference-platform\support\AfsVolumes.pas',
  BytesSupport in '..\reference-platform\support\BytesSupport.pas',
  ColourSupport in '..\reference-platform\support\ColourSupport.pas',
  CurrencySupport in '..\reference-platform\support\CurrencySupport.pas',
  DateSupport in '..\reference-platform\support\DateSupport.pas',
  DecimalSupport in '..\reference-platform\support\DecimalSupport.pas',
  EncodeSupport in '..\reference-platform\support\EncodeSupport.pas',
  ErrorSupport in '..\reference-platform\support\ErrorSupport.pas',
  FHIRBase in '..\reference-platform\support\FHIRBase.pas',
  FacebookSupport in '..\reference-platform\support\FacebookSupport.pas',
  FileSupport in '..\reference-platform\support\FileSupport.pas',
  GUIDSupport in '..\reference-platform\support\GUIDSupport.pas',
  HL7V2DateSupport in '..\reference-platform\support\HL7V2DateSupport.pas',
  HMAC in '..\reference-platform\support\HMAC.pas',
  HTMLPublisher in '..\reference-platform\support\HTMLPublisher.pas',
  HashSupport in '..\reference-platform\support\HashSupport.pas',
  IdWebSocket in '..\reference-platform\support\IdWebSocket.pas',
  InternetFetcher in '..\reference-platform\support\InternetFetcher.pas',
  JWT in '..\reference-platform\support\JWT.pas',
  KDBDialects in '..\reference-platform\support\KDBDialects.pas',
  MathSupport in '..\reference-platform\support\MathSupport.pas',
  MemorySupport in '..\reference-platform\support\MemorySupport.pas',
  MimeMessage in '..\reference-platform\support\MimeMessage.pas',
  MXmlBuilder in '..\reference-platform\support\MXmlBuilder.pas',
  MsXmlParser in '..\reference-platform\support\MsXmlParser.pas',
  OIDSupport in '..\reference-platform\support\OIDSupport.pas',
  ParseMap in '..\reference-platform\support\ParseMap.pas',
  RDFUtilities in '..\reference-platform\support\RDFUtilities.pas',
  ServiceController in '..\reference-platform\support\ServiceController.pas',
  ShellSupport in '..\reference-platform\support\ShellSupport.pas',
  StringSupport in '..\reference-platform\support\StringSupport.pas',
  SystemService in '..\reference-platform\support\SystemService.pas',
  SystemSupport in '..\reference-platform\support\SystemSupport.pas',
  TextUtilities in '..\reference-platform\support\TextUtilities.pas',
  ThreadSupport in '..\reference-platform\support\ThreadSupport.pas',
  XMLBuilder in '..\reference-platform\support\XMLBuilder.pas',
  kCritSct in '..\reference-platform\support\kCritSct.pas',
  libeay32 in '..\reference-platform\support\libeay32.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  CDSHooksUtilities in '..\reference-platform\support\CDSHooksUtilities.pas',
  FHIRAuthMap in '..\reference-platform\dstu3\FHIRAuthMap.pas',
  FHIRClient in '..\reference-platform\client\FHIRClient.pas',
  FHIRConstants in '..\reference-platform\dstu3\FHIRConstants.pas',
  FHIRContext in '..\reference-platform\dstu3\FHIRContext.pas',
  FHIRIndexInformation in '..\reference-platform\dstu3\FHIRIndexInformation.pas',
  FHIRLang in '..\reference-platform\support\FHIRLang.pas',
  FHIRMetaModel in '..\reference-platform\dstu3\FHIRMetaModel.pas',
  FHIROperations in '..\reference-platform\dstu3\FHIROperations.pas',
  FHIRParser in '..\reference-platform\support\FHIRParser.pas',
  FHIRParserXml in '..\reference-platform\dstu3\FHIRParserXml.pas',
  FHIRParserJson in '..\reference-platform\dstu3\FHIRParserJson.pas',
  FHIRParserTurtle in '..\reference-platform\dstu3\FHIRParserTurtle.pas',
  FHIRParserBase in '..\reference-platform\support\FHIRParserBase.pas',
  FHIRProfileUtilities in '..\reference-platform\dstu3\FHIRProfileUtilities.pas',
  FHIRResources in '..\reference-platform\dstu3\FHIRResources.pas',
  FHIRSecurity in '..\reference-platform\support\FHIRSecurity.pas',
  FHIRSupport in '..\reference-platform\support\FHIRSupport.pas',
  FHIRTags in '..\reference-platform\dstu3\FHIRTags.pas',
  FHIRTypes in '..\reference-platform\dstu3\FHIRTypes.pas',
  FHIRUtilities in '..\reference-platform\dstu3\FHIRUtilities.pas',
  FHIRValidator in '..\reference-platform\dstu3\FHIRValidator.pas',
  FHIRXhtml in '..\reference-platform\support\FHIRXhtml.pas',
  FhirOpBase in '..\reference-platform\dstu3\FhirOpBase.pas',
  FhirPath in '..\reference-platform\dstu3\FhirPath.pas',
  NarrativeGenerator in '..\reference-platform\dstu3\NarrativeGenerator.pas',
  QuestionnaireBuilder in '..\reference-platform\dstu3\QuestionnaireBuilder.pas',
  SCIMObjects in '..\reference-platform\support\SCIMObjects.pas',
  SmartOnFhirUtilities in '..\reference-platform\client\SmartOnFhirUtilities.pas',
  TerminologyServices in '..\Libraries\TerminologyServices.pas',
  KDBLogging in '..\Libraries\db\KDBLogging.pas',
  KDBManager in '..\Libraries\db\KDBManager.pas',
  KSettings in '..\Libraries\db\KSettings.pas',
  LoincPublisher in '..\Libraries\loinc\LoincPublisher.pas',
  LoincServices in '..\Libraries\loinc\LoincServices.pas',
  TwilioClient in '..\Libraries\security\TwilioClient.pas',
  SnomedAnalysis in '..\Libraries\snomed\SnomedAnalysis.pas',
  SnomedExpressions in '..\Libraries\snomed\SnomedExpressions.pas',
  SnomedPublisher in '..\Libraries\snomed\SnomedPublisher.pas',
  SnomedServices in '..\Libraries\snomed\SnomedServices.pas',
  DISystemCompat in '..\Libraries\stem\DISystemCompat.pas',
  YuStemmer in '..\Libraries\stem\YuStemmer.pas',
  Ucum in '..\Libraries\ucum\Ucum.pas',
  UcumExpressions in '..\Libraries\ucum\UcumExpressions.pas',
  UcumHandlers in '..\Libraries\ucum\UcumHandlers.pas',
  UcumSearch in '..\Libraries\ucum\UcumSearch.pas',
  UcumServices in '..\Libraries\ucum\UcumServices.pas',
  UcumValidators in '..\Libraries\ucum\UcumValidators.pas',
  ACIRServices in '..\Server\ACIRServices.pas',
  AreaCodeServices in '..\Server\AreaCodeServices.pas',
  AuthServer in '..\Server\AuthServer.pas',
  ClosureManager in '..\Server\ClosureManager.pas',
  FHIRIndexManagers in '..\Server\FHIRIndexManagers.pas',
  FHIRLog in '..\reference-platform\support\FHIRLog.pas',
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
  FHIRGraphQL in '..\reference-platform\support\FHIRGraphQL.pas',
  ParserSupport in '..\reference-platform\support\ParserSupport.pas',
  MXML in '..\reference-platform\support\MXML.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  CDSHooksServer in '..\Server\CDSHooksServer.pas',
  TurtleParser in '..\reference-platform\support\TurtleParser.pas',
  CertificateSupport in '..\reference-platform\support\CertificateSupport.pas',
  ClientApplicationVerifier in '..\Libraries\security\ClientApplicationVerifier.pas',
  JWTService in '..\Server\JWTService.pas',
  SCrypt in '..\Libraries\security\SCrypt.pas',
  ApplicationCache in '..\Server\ApplicationCache.pas',
  CDSHooksClientManager in '..\reference-platform\support\CDSHooksClientManager.pas',
  WebSourceProvider in '..\Server\WebSourceProvider.pas',
  FHIRIndexBase in '..\reference-platform\support\FHIRIndexBase.pas',
  ICD10Services in '..\Server\ICD10Services.pas',
  ServerPostHandlers in '..\Server\ServerPostHandlers.pas',
  ServerJavascriptHost in '..\Server\ServerJavascriptHost.pas',
  FHIRJavascriptReg in '..\reference-platform\dstu3\FHIRJavascriptReg.pas',
  FHIRJavascript in '..\Libraries\js\FHIRJavascript.pas',
  FHIRClientJs in '..\Libraries\js\FHIRClientJs.pas',
  ServerEventJs in '..\Server\ServerEventJs.pas',
  Javascript in '..\Libraries\js\Javascript.pas',
  ChakraCommon in '..\Libraries\js\ChakraCommon.pas',
  AdvJavascript in '..\Libraries\js\AdvJavascript.pas',
  DigitalSignatures in '..\reference-platform\support\DigitalSignatures.pas',
  FHIRFactory in '..\reference-platform\support\FHIRFactory.pas',
  JavaBridge in '..\Server\JavaBridge.pas',
  myUTF8Strings in '..\Libraries\java\myUTF8Strings.pas',
  JUtils in '..\Libraries\java\JUtils.pas',
  JNIWrapper in '..\Libraries\java\JNIWrapper.pas',
  JNI in '..\Libraries\java\JNI.pas',
  JavaRuntime in '..\Libraries\java\JavaRuntime.pas';

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
