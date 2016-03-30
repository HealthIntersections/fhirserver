// JCL_DEBUG_EXPERT_INSERTJDBG ON
program FHIRServer2;
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
 * Try dereferencing http://hl7.org/fhir/quest'ionnaire-extensions#answerFormat -- 404
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
  AdvSynchronizationRegistries in '..\reference-platform\Support\AdvSynchronizationRegistries.pas',
  AdvTimeControllers in '..\reference-platform\Support\AdvTimeControllers.pas',
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
  DateAndTime in '..\reference-platform\Support\DateAndTime.pas',
  KDate in '..\reference-platform\Support\KDate.pas',
  HL7V2DateSupport in '..\reference-platform\Support\HL7V2DateSupport.pas',
  MsXmlParser in '..\reference-platform\Support\MsXmlParser.pas',
  XMLBuilder in '..\reference-platform\Support\XMLBuilder.pas',
  AdvWinInetClients in '..\reference-platform\Support\AdvWinInetClients.pas',
  MsXmlBuilder in '..\reference-platform\Support\MsXmlBuilder.pas',
  AdvXmlBuilders in '..\reference-platform\Support\AdvXmlBuilders.pas',
  AdvJSON in '..\reference-platform\Support\AdvJSON.pas',
  AfsResourceVolumes in '..\reference-platform\Support\AfsResourceVolumes.pas',
  AfsVolumes in '..\reference-platform\Support\AfsVolumes.pas',
  AfsStreamManagers in '..\reference-platform\Support\AfsStreamManagers.pas',
  AdvObjectMatches in '..\reference-platform\Support\AdvObjectMatches.pas',
  RegExpr in '..\reference-platform\Support\RegExpr.pas',
  TextUtilities in '..\reference-platform\Support\TextUtilities.pas',
  FHIROperation in 'FHIROperation.pas',
  AdvIntegerObjectMatches in '..\reference-platform\Support\AdvIntegerObjectMatches.pas',
  AdvStringObjectMatches in '..\reference-platform\Support\AdvStringObjectMatches.pas',
  FHIRIndexManagers in 'FHIRIndexManagers.pas',
  AdvNames in '..\reference-platform\Support\AdvNames.pas',
  UcumServices in '..\Libraries\ucum\UcumServices.pas',
  AdvBinaryFilers in '..\reference-platform\Support\AdvBinaryFilers.pas',
  AdvClassLists in '..\reference-platform\Support\AdvClassLists.pas',
  AdvPointers in '..\reference-platform\Support\AdvPointers.pas',
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
  InternetFetcher in '..\reference-platform\Support\InternetFetcher.pas',
  FacebookSupport in '..\reference-platform\Support\FacebookSupport.pas',
  DCPsha256 in '..\Libraries\DCP\DCPsha256.pas',
  DCPcrypt2 in '..\Libraries\DCP\DCPcrypt2.pas',
  DCPconst in '..\Libraries\DCP\DCPconst.pas',
  DCPbase64 in '..\Libraries\DCP\DCPbase64.pas',
  SystemService in '..\reference-platform\Support\SystemService.pas',
  ServiceController in '..\reference-platform\Support\ServiceController.pas',
  AdvIntegerLists in '..\reference-platform\Support\AdvIntegerLists.pas',
  AdvDispatchers in '..\reference-platform\Support\AdvDispatchers.pas',
  AdvEvents in '..\reference-platform\Support\AdvEvents.pas',
  AdvMethods in '..\reference-platform\Support\AdvMethods.pas',
  DBInstaller in 'DBInstaller.pas',
  KDBDialects in '..\reference-platform\Support\KDBDialects.pas',
  KDBLogging in '..\Libraries\db\KDBLogging.pas',
  KDBManager in '..\Libraries\db\KDBManager.pas',
  KDBOdbcExpress in '..\Libraries\db\KDBOdbcExpress.pas',
  KDBUtils in '..\Libraries\db\KDBUtils.pas',
  KSettings in '..\Libraries\db\KSettings.pas',
  OdbcCore in '..\Libraries\db\OdbcCore.pas',
  OdbcExtras in '..\Libraries\db\OdbcExtras.pas',
  OdbcHeaders in '..\Libraries\db\OdbcHeaders.pas',
  OdbcImplementation in '..\Libraries\db\OdbcImplementation.pas',
  CurrencySupport in '..\reference-platform\Support\CurrencySupport.pas',
  FHIRDataStore in 'FHIRDataStore.pas',
  SnomedImporter in '..\Libraries\snomed\SnomedImporter.pas',
  AdvProfilers in '..\reference-platform\Support\AdvProfilers.pas',
  AnsiStringBuilder in '..\reference-platform\Support\AnsiStringBuilder.pas',
  AdvIntegerMatches in '..\reference-platform\Support\AdvIntegerMatches.pas',
  SnomedPublisher in '..\Libraries\snomed\SnomedPublisher.pas',
  SnomedExpressions in '..\Libraries\snomed\SnomedExpressions.pas',
  FhirServerTests in 'FhirServerTests.pas',
  HTMLPublisher in '..\reference-platform\Support\HTMLPublisher.pas',
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
  libeay32 in '..\reference-platform\Support\libeay32.pas',
  JWTTests in '..\reference-platform\Support\JWTTests.pas',
  HMAC in '..\reference-platform\Support\HMAC.pas',
  JWT in '..\reference-platform\Support\JWT.pas',
  SCIMServer in 'SCIMServer.pas',
  SCIMSearch in 'SCIMSearch.pas',
  TwilioClient in '..\Libraries\security\TwilioClient.pas',
  FHIRSearchSyntax in 'FHIRSearchSyntax.pas',
  ShellSupport in '..\reference-platform\Support\ShellSupport.pas',
  RectSupport in 'RectSupport.pas',
  CoordinateSupport in 'CoordinateSupport.pas',
  AdvGenerics in '..\reference-platform\Support\AdvGenerics.pas',
  XMLSupport in '..\reference-platform\Support\XMLSupport.pas',
  DigitalSignatures in '..\reference-platform\Support\DigitalSignatures.pas',
  UriServices in 'UriServices.pas',
  CvxServices in 'CvxServices.pas',
  USStateCodeServices in 'USStateCodeServices.pas',
  UniiServices in 'UniiServices.pas',
  RxNormServices in 'RxNormServices.pas',
  OIDSupport in '..\reference-platform\Support\OIDSupport.pas',
  IETFLanguageCodeServices in 'IETFLanguageCodeServices.pas',
  SnomedAnalysis in '..\Libraries\snomed\SnomedAnalysis.pas',
  AreaCodeServices in 'AreaCodeServices.pas',
  FHIRSubscriptionManager in 'FHIRSubscriptionManager.pas',
  ServerValidator in 'ServerValidator.pas',
  IdWebSocket in '..\reference-platform\Support\IdWebSocket.pas',
  MsXML in '..\reference-platform\Support\MsXML.pas',
  MimeMessage in '..\reference-platform\Support\MimeMessage.pas',
  kCritSct in '..\reference-platform\Support\kCritSct.pas',
  QuestionnaireBuilder in '..\reference-platform\dstu2\QuestionnaireBuilder.pas',
  SCIMObjects in '..\reference-platform\dstu2\SCIMObjects.pas',
  NarrativeGenerator in '..\reference-platform\dstu2\NarrativeGenerator.pas',
  FHIRSecurity in '..\reference-platform\dstu2\FHIRSecurity.pas',
  FHIRParserTests in '..\tests\FHIRParserTests.pas',
  FHIRNarrativeGenerator in '..\reference-platform\dstu2\FHIRNarrativeGenerator.pas',
  SmartOnFhirUtilities in '..\reference-platform\dstu2\SmartOnFhirUtilities.pas',
  FHIRPath in '..\reference-platform\dstu2\FHIRPath.pas',
  FHIRTags in '..\reference-platform\dstu2\FHIRTags.pas',
  FHIRProfileUtilities in '..\reference-platform\dstu2\FHIRProfileUtilities.pas',
  FHIRBase in '..\reference-platform\dstu2\FHIRBase.pas',
  FHIRTypes in '..\reference-platform\dstu2\FHIRTypes.pas',
  FHIRResources in '..\reference-platform\dstu2\FHIRResources.pas',
  FHIRParser in '..\reference-platform\dstu2\FHIRParser.pas',
  FHIRParserBase in '..\reference-platform\dstu2\FHIRParserBase.pas',
  FHIRConstants in '..\reference-platform\dstu2\FHIRConstants.pas',
  FHIRSupport in '..\reference-platform\dstu2\FHIRSupport.pas',
  FHIRLang in '..\reference-platform\dstu2\FHIRLang.pas',
  FHIRUtilities in '..\reference-platform\dstu2\FHIRUtilities.pas',
  FHIRClient in '..\reference-platform\dstu2\FHIRClient.pas',
  FHIRValidator in '..\reference-platform\dstu2\FHIRValidator.pas',
  ClosureManager in 'ClosureManager.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownDaringFireballTests in '..\..\markdown\source\MarkdownDaringFireballTests.pas',
  CDSHooksUtilities in '..\reference-platform\dstu2\CDSHooksUtilities.pas',
  FHIRPatch in '..\reference-platform\dstu2\FHIRPatch.pas',
  MPISearch in 'MPISearch.pas',
  RDFUtilities in '..\reference-platform\support\RDFUtilities.pas',
  FHIROperations in '..\reference-platform\dstu2\FHIROperations.pas',
  FHIRIndexConstants in '..\reference-platform\dstu2\FHIRIndexConstants.pas',
  FhirOpBase in '..\reference-platform\dstu3\FhirOpBase.pas',
  FHIRMetaModel in '..\reference-platform\dstu2\FHIRMetaModel.pas';

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


