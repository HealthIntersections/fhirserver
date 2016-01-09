// JCL_DEBUG_EXPERT_INSERTJDBG ON
program FHIRServer21;
{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$APPTYPE CONSOLE}
{$I FHIR.INC}

{$R *.res}


{

bug list:

Hi Grahame,
[12:05:43 PM] David Hay: any reason why this query doesn’t return any expansion:
[12:05:51 PM] David Hay: http://fhir-dev.healthintersections.com.au/openValueSet/valueset-medication-codes/$expand?filter=amox
[12:05:59 PM] David Hay: This one works just fine:
[12:06:07 PM] David Hay: http://fhir-dev.healthintersections.com.au/openValueSet/valueset-condition-code/$expand?filter=asth
[12:12:43 PM] David Hay: also: valueset-medication-form-codes
[12:12:50 PM] David Hay: are actually route codes…



 * multiple duplicate tags
 * Try dereferencing http://hl7.org/fhir/questionnaire-extensions#answerFormat -- 404
    Our URLs for extensions are broken
    They should all be http://hl7.org/fhir/Profile/someid#extension

[2:25:55 PM] Brian Postlethwaite: http://fhir.healthintersections.com.au/open/MedicationStatement/_search?_count=100&patient:Patient._id=163
 is there a way to issue a query and include _since>= on it?


[1:33:14 AM] Ewout Kramer: paging on your server seems to reverse url and relation:
[1:33:14 AM] Ewout Kramer: <relation value="http://fhir-dev.healthintersections.com.au/open/DiagnosticReport/_search?_format=text/xml+fhir&amp;search-id=e9a68b91-6777-4fbf-a4e2-de31e44829&amp;&amp;search-sort=_id" xmlns="http://hl7.org/fhir" />
[1:33:18 AM] Ewout Kramer: <url value="self" xmlns="http://hl7.org/fhir" />


build validator jar not in validation pack


Change record:\
  3-May 2015
    * fix string searching - case and accent insensitive

  2-May 2014
    * fix search / last
    * rebuild history to work like search (modify search tables too)

  19-April 2014
    * add Questionnaire web interface (Lloyd's transform)
    * fix bug indexing patient compartment
  18-April 2014
    * pick up tags on PUT/POST and handle them properly
    * fix tag functionality in Web UI
    * reject unknown attributes
    * fix problem where you couldn't vread an old version of a resource that is currently deleted
    * fix compartment searches looking for plural name instead of singular name e.g.http://hl7connect.healthintersections.com.au/open/Patient/1053/Observation[s]
    * fix mime type on content element in atom feed
}

uses
  FastMM4 in '..\Libraries\FMM\FastMM4.pas',
  FastMM4Messages in '..\Libraries\FMM\FastMM4Messages.pas',
  Windows,
  System.SysUtils,
  Classes,
  IdSSLOpenSSLHeaders,
  JclDebug,
  FHIRServerApplicationCore in 'FHIRServerApplicationCore.pas',
  FHIRRestServer in 'FHIRRestServer.pas',
  EncodeSupport in '..\Libraries\Support\EncodeSupport.pas',
  StringSupport in '..\Libraries\Support\StringSupport.pas',
  MathSupport in '..\Libraries\Support\MathSupport.pas',
  SystemSupport in '..\Libraries\Support\SystemSupport.pas',
  DateSupport in '..\Libraries\Support\DateSupport.pas',
  MemorySupport in '..\Libraries\Support\MemorySupport.pas',
  ErrorSupport in '..\Libraries\Support\ErrorSupport.pas',
  ThreadSupport in '..\Libraries\Support\ThreadSupport.pas',
  BytesSupport in '..\Libraries\Support\BytesSupport.pas',
  AdvStringBuilders in '..\Libraries\Support\AdvStringBuilders.pas',
  AdvObjects in '..\Libraries\Support\AdvObjects.pas',
  AdvExceptions in '..\Libraries\Support\AdvExceptions.pas',
  AdvFactories in '..\Libraries\Support\AdvFactories.pas',
  FileSupport in '..\Libraries\Support\FileSupport.pas',
  AdvControllers in '..\Libraries\Support\AdvControllers.pas',
  AdvPersistents in '..\Libraries\Support\AdvPersistents.pas',
  AdvFilers in '..\Libraries\Support\AdvFilers.pas',
  ColourSupport in '..\Libraries\Support\ColourSupport.pas',
  AdvPersistentLists in '..\Libraries\Support\AdvPersistentLists.pas',
  AdvObjectLists in '..\Libraries\Support\AdvObjectLists.pas',
  AdvItems in '..\Libraries\Support\AdvItems.pas',
  AdvCollections in '..\Libraries\Support\AdvCollections.pas',
  AdvIterators in '..\Libraries\Support\AdvIterators.pas',
  AdvClassHashes in '..\Libraries\Support\AdvClassHashes.pas',
  AdvHashes in '..\Libraries\Support\AdvHashes.pas',
  HashSupport in '..\Libraries\Support\HashSupport.pas',
  AdvStringHashes in '..\Libraries\Support\AdvStringHashes.pas',
  AdvStringIntegerMatches in '..\Libraries\Support\AdvStringIntegerMatches.pas',
  AdvStreams in '..\Libraries\Support\AdvStreams.pas',
  AdvParameters in '..\Libraries\Support\AdvParameters.pas',
  AdvFiles in '..\Libraries\Support\AdvFiles.pas',
  AdvMemories in '..\Libraries\Support\AdvMemories.pas',
  AdvBuffers in '..\Libraries\Support\AdvBuffers.pas',
  AdvStreamFilers in '..\Libraries\Support\AdvStreamFilers.pas',
  AdvExclusiveCriticalSections in '..\Libraries\Support\AdvExclusiveCriticalSections.pas',
  AdvThreads in '..\Libraries\Support\AdvThreads.pas',
  AdvSignals in '..\Libraries\Support\AdvSignals.pas',
  AdvSynchronizationRegistries in '..\Libraries\Support\AdvSynchronizationRegistries.pas',
  AdvTimeControllers in '..\Libraries\Support\AdvTimeControllers.pas',
  AdvInt64Matches in '..\Libraries\support\AdvInt64Matches.pas',
  AdvLargeIntegerMatches in '..\Libraries\Support\AdvLargeIntegerMatches.pas',
  AdvStringLargeIntegerMatches in '..\Libraries\Support\AdvStringLargeIntegerMatches.pas',
  AdvStringLists in '..\Libraries\Support\AdvStringLists.pas',
  AdvCSVFormatters in '..\Libraries\Support\AdvCSVFormatters.pas',
  AdvTextFormatters in '..\Libraries\Support\AdvTextFormatters.pas',
  AdvFormatters in '..\Libraries\Support\AdvFormatters.pas',
  AdvCSVExtractors in '..\Libraries\Support\AdvCSVExtractors.pas',
  AdvTextExtractors in '..\Libraries\Support\AdvTextExtractors.pas',
  AdvExtractors in '..\Libraries\Support\AdvExtractors.pas',
  AdvCharacterSets in '..\Libraries\Support\AdvCharacterSets.pas',
  AdvOrdinalSets in '..\Libraries\Support\AdvOrdinalSets.pas',
  AdvStreamReaders in '..\Libraries\Support\AdvStreamReaders.pas',
  AdvStringStreams in '..\Libraries\Support\AdvStringStreams.pas',
  AdvStringMatches in '..\Libraries\Support\AdvStringMatches.pas',
  AdvXMLEntities in '..\Libraries\Support\AdvXMLEntities.pas',
  AdvXMLFormatters in '..\Libraries\Support\AdvXMLFormatters.pas',
  ParseMap in '..\Libraries\Support\ParseMap.pas',
  AdvZipParts in '..\Libraries\Support\AdvZipParts.pas',
  AdvNameBuffers in '..\Libraries\Support\AdvNameBuffers.pas',
  AdvZipReaders in '..\Libraries\Support\AdvZipReaders.pas',
  AdvVCLStreams in '..\Libraries\Support\AdvVCLStreams.pas',
  AdvZipDeclarations in '..\Libraries\Support\AdvZipDeclarations.pas',
  AdvZipUtilities in '..\Libraries\Support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\Libraries\Support\AdvZipWorkers.pas',
  GUIDSupport in '..\Libraries\Support\GUIDSupport.pas',
  DecimalSupport in '..\Libraries\Support\DecimalSupport.pas',
  DateAndTime in '..\Libraries\Support\DateAndTime.pas',
  KDate in '..\Libraries\Support\KDate.pas',
  HL7V2DateSupport in '..\Libraries\Support\HL7V2DateSupport.pas',
  MsXmlParser in '..\Libraries\Support\MsXmlParser.pas',
  XMLBuilder in '..\Libraries\Support\XMLBuilder.pas',
  AdvWinInetClients in '..\Libraries\Support\AdvWinInetClients.pas',
  MsXmlBuilder in '..\Libraries\Support\MsXmlBuilder.pas',
  AdvXmlBuilders in '..\Libraries\Support\AdvXmlBuilders.pas',
  AdvJSON in '..\Libraries\Support\AdvJSON.pas',
  AfsResourceVolumes in '..\Libraries\Support\AfsResourceVolumes.pas',
  AfsVolumes in '..\Libraries\Support\AfsVolumes.pas',
  AfsStreamManagers in '..\Libraries\Support\AfsStreamManagers.pas',
  AdvObjectMatches in '..\Libraries\Support\AdvObjectMatches.pas',
  RegExpr in '..\Libraries\Support\RegExpr.pas',
  TextUtilities in '..\Libraries\Support\TextUtilities.pas',
  FHIROperation in 'FHIROperation.pas',
  AdvIntegerObjectMatches in '..\Libraries\Support\AdvIntegerObjectMatches.pas',
  AdvStringObjectMatches in '..\Libraries\Support\AdvStringObjectMatches.pas',
  FHIRIndexManagers in 'FHIRIndexManagers.pas',
  AdvNames in '..\Libraries\Support\AdvNames.pas',
  UcumServices in '..\Libraries\ucum\UcumServices.pas',
  AdvBinaryFilers in '..\Libraries\Support\AdvBinaryFilers.pas',
  AdvClassLists in '..\Libraries\Support\AdvClassLists.pas',
  AdvPointers in '..\Libraries\Support\AdvPointers.pas',
  UcumHandlers in '..\Libraries\ucum\UcumHandlers.pas',
  Ucum in '..\Libraries\ucum\Ucum.pas',
  UcumValidators in '..\Libraries\ucum\UcumValidators.pas',
  UcumExpressions in '..\Libraries\ucum\UcumExpressions.pas',
  UcumSearch in '..\Libraries\ucum\UcumSearch.pas',
  FHIRValueSetExpander in 'FHIRValueSetExpander.pas',
  YuStemmer in '..\Libraries\Stem\YuStemmer.pas',
  LoincServices in '..\Libraries\loinc\LoincServices.pas',
  DISystemCompat in '..\Libraries\Stem\DISystemCompat.pas',
  SnomedServices in '..\Libraries\Snomed\SnomedServices.pas',
  InternetFetcher in '..\Libraries\Support\InternetFetcher.pas',
  FacebookSupport in '..\Libraries\Support\FacebookSupport.pas',
  DCPsha256 in '..\Libraries\DCP\DCPsha256.pas',
  DCPcrypt2 in '..\Libraries\DCP\DCPcrypt2.pas',
  DCPconst in '..\Libraries\DCP\DCPconst.pas',
  DCPbase64 in '..\Libraries\DCP\DCPbase64.pas',
  SystemService in '..\Libraries\Support\SystemService.pas',
  ServiceController in '..\Libraries\Support\ServiceController.pas',
  AdvIntegerLists in '..\Libraries\Support\AdvIntegerLists.pas',
  AdvDispatchers in '..\Libraries\Support\AdvDispatchers.pas',
  AdvEvents in '..\Libraries\Support\AdvEvents.pas',
  AdvMethods in '..\Libraries\Support\AdvMethods.pas',
  DBInstaller in 'DBInstaller.pas',
  KDBDialects in '..\Libraries\Support\KDBDialects.pas',
  KDBLogging in '..\Libraries\db\KDBLogging.pas',
  KDBManager in '..\Libraries\db\KDBManager.pas',
  KDBOdbcExpress in '..\Libraries\db\KDBOdbcExpress.pas',
  KDBUtils in '..\Libraries\db\KDBUtils.pas',
  KSettings in '..\Libraries\db\KSettings.pas',
  OdbcCore in '..\Libraries\db\OdbcCore.pas',
  OdbcExtras in '..\Libraries\db\OdbcExtras.pas',
  OdbcHeaders in '..\Libraries\db\OdbcHeaders.pas',
  OdbcImplementation in '..\Libraries\db\OdbcImplementation.pas',
  CurrencySupport in '..\Libraries\Support\CurrencySupport.pas',
  FHIRDataStore in 'FHIRDataStore.pas',
  SnomedImporter in '..\Libraries\snomed\SnomedImporter.pas',
  AdvProfilers in '..\Libraries\Support\AdvProfilers.pas',
  AnsiStringBuilder in '..\Libraries\support\AnsiStringBuilder.pas',
  AdvIntegerMatches in '..\Libraries\support\AdvIntegerMatches.pas',
  SnomedPublisher in '..\Libraries\snomed\SnomedPublisher.pas',
  SnomedExpressions in '..\Libraries\snomed\SnomedExpressions.pas',
  FhirServerTests in 'FhirServerTests.pas',
  HTMLPublisher in '..\Libraries\support\HTMLPublisher.pas',
  LoincImporter in '..\Libraries\loinc\LoincImporter.pas',
  LoincPublisher in '..\Libraries\loinc\LoincPublisher.pas',
  TerminologyServer in 'TerminologyServer.pas',
  TerminologyServerStore in 'TerminologyServerStore.pas',
  TerminologyServices in '..\Libraries\TerminologyServices.pas',
  FHIRValueSetChecker in 'FHIRValueSetChecker.pas',
  TerminologyWebServer in 'TerminologyWebServer.pas',
  FHIRServerConstants in 'FHIRServerConstants.pas',
  DecimalTests in '..\Libraries\tests\DecimalTests.pas',
  UcumTests in '..\Libraries\tests\UcumTests.pas',
  FHIRServerUtilities in 'FHIRServerUtilities.pas',
  SearchProcessor in 'SearchProcessor.pas',
  AuthServer in 'AuthServer.pas',
  libeay32 in '..\Libraries\support\libeay32.pas',
  JWTTests in '..\Libraries\support\JWTTests.pas',
  HMAC in '..\Libraries\support\HMAC.pas',
  JWT in '..\Libraries\support\JWT.pas',
  SCIMServer in 'SCIMServer.pas',
  SCIMSearch in 'SCIMSearch.pas',
  TwilioClient in '..\Libraries\security\TwilioClient.pas',
  FHIRSearchSyntax in 'FHIRSearchSyntax.pas',
  ShellSupport in '..\Libraries\support\ShellSupport.pas',
  RectSupport in 'RectSupport.pas',
  CoordinateSupport in 'CoordinateSupport.pas',
  AdvGenerics in '..\Libraries\support\AdvGenerics.pas',
  XMLSupport in '..\Libraries\support\XMLSupport.pas',
  DigitalSignatures in '..\Libraries\support\DigitalSignatures.pas',
  UriServices in 'UriServices.pas',
  CvxServices in 'CvxServices.pas',
  USStateCodeServices in 'USStateCodeServices.pas',
  UniiServices in 'UniiServices.pas',
  RxNormServices in 'RxNormServices.pas',
  OIDSupport in '..\Libraries\support\OIDSupport.pas',
  IETFLanguageCodeServices in 'IETFLanguageCodeServices.pas',
  SnomedAnalysis in '..\Libraries\snomed\SnomedAnalysis.pas',
  AreaCodeServices in 'AreaCodeServices.pas',
  FHIRSubscriptionManager in 'FHIRSubscriptionManager.pas',
  ServerValidator in 'ServerValidator.pas',
  IdWebSocket in '..\Libraries\support\IdWebSocket.pas',
  MsXML in '..\Libraries\support\MsXML.pas',
  MimeMessage in '..\Libraries\support\MimeMessage.pas',
  kCritSct in '..\Libraries\Support\kCritSct.pas',
  QuestionnaireBuilder in '..\Libraries\refplat21\QuestionnaireBuilder.pas',
  SCIMObjects in '..\Libraries\refplat21\SCIMObjects.pas',
  NarrativeGenerator in '..\Libraries\refplat21\NarrativeGenerator.pas',
  FHIRSecurity in '..\Libraries\refplat21\FHIRSecurity.pas',
  FHIRParserTests in '..\Libraries\refplat21\FHIRParserTests.pas',
  FHIRNarrativeGenerator in '..\Libraries\refplat21\FHIRNarrativeGenerator.pas',
  SmartOnFhirUtilities in '..\Libraries\refplat21\SmartOnFhirUtilities.pas',
  FhirPath in '..\Libraries\refplat21\FhirPath.pas',
  FHIRTags in '..\Libraries\refplat21\FHIRTags.pas',
  FHIRProfileUtilities in '..\Libraries\refplat21\FHIRProfileUtilities.pas',
  FHIRBase in '..\Libraries\refplat21\FHIRBase.pas',
  FHIRTypes in '..\Libraries\refplat21\FHIRTypes.pas',
  FHIRResources in '..\Libraries\refplat21\FHIRResources.pas',
  FHIRParser in '..\Libraries\refplat21\FHIRParser.pas',
  FHIRParserBase in '..\Libraries\refplat21\FHIRParserBase.pas',
  FHIRConstants in '..\Libraries\refplat21\FHIRConstants.pas',
  FHIRSupport in '..\Libraries\refplat21\FHIRSupport.pas',
  FHIRLang in '..\Libraries\refplat21\FHIRLang.pas',
  FHIRUtilities in '..\Libraries\refplat21\FHIRUtilities.pas',
  FHIRClient in '..\Libraries\refplat21\FHIRClient.pas',
  FHIRValidator in '..\Libraries\refplat21\FHIRValidator.pas',
  ClosureManager in 'ClosureManager.pas',
  CDSHooksUtilities in '..\Libraries\refplat21\CDSHooksUtilities.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownDaringFireballTests in '..\..\markdown\source\MarkdownDaringFireballTests.pas',
  AccessControlEngine in 'AccessControlEngine.pas',
  MPISearch in 'MPISearch.pas';

begin
  JclStartExceptionTracking;
  IdOpenSSLSetLibPath(ExtractFilePath(Paramstr(0)));
  try
    SetConsoleTitle('FHIR Server DSTU2');
    ExecuteFhirServer;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
end.


