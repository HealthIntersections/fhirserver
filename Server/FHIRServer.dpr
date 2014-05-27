program FHIRServer;
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
todo for connectathon:
* questionnaire in web interface
* questionnaire conversion based on concept map
* review searching conformance

bug list:

http://fhir.healthintersections.com.au/open/Patient/C82E147E-6E80-4AF6-97CD-94C4294A3A75/_history
[10:30:12 AM] Lloyd McKenzie: Atom feed shows <totalResluts>0
[10:30:57 AM] Lloyd McKenzie: Also, the contained elements declare a <content type="text/xml"/> instead of ""


build:
validator jar not in validation pack


Change record:
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
  System.SysUtils,
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
  FHIRBase in '..\Libraries\refplat\FHIRBase.pas',
  DecimalSupport in '..\Libraries\Support\DecimalSupport.pas',
  DateAndTime in '..\Libraries\Support\DateAndTime.pas',
  KDate in '..\Libraries\Support\KDate.pas',
  HL7V2DateSupport in '..\Libraries\Support\HL7V2DateSupport.pas',
  FHIRTypes in '..\Libraries\refplat\FHIRTypes.pas',
  FHIRResources in '..\Libraries\refplat\FHIRResources.pas',
  FHIRComponents in '..\Libraries\refplat\FHIRComponents.pas',
  FHIRParser in '..\Libraries\refplat\FHIRParser.pas',
  FHIRParserBase in '..\Libraries\refplat\FHIRParserBase.pas',
  FHIRConstants in '..\Libraries\refplat\FHIRConstants.pas',
  FHIRSupport in '..\Libraries\refplat\FHIRSupport.pas',
  FHIRAtomFeed in '..\Libraries\refplat\FHIRAtomFeed.pas',
  MsXmlParser in '..\Libraries\Support\MsXmlParser.pas',
  XMLBuilder in '..\Libraries\Support\XMLBuilder.pas',
  AdvWinInetClients in '..\Libraries\Support\AdvWinInetClients.pas',
  MsXmlBuilder in '..\Libraries\Support\MsXmlBuilder.pas',
  AdvXmlBuilders in '..\Libraries\Support\AdvXmlBuilders.pas',
  JSON in '..\Libraries\Support\JSON.pas',
  FHIRLang in '..\Libraries\refplat\FHIRLang.pas',
  AfsResourceVolumes in '..\Libraries\Support\AfsResourceVolumes.pas',
  AfsVolumes in '..\Libraries\Support\AfsVolumes.pas',
  AfsStreamManagers in '..\Libraries\Support\AfsStreamManagers.pas',
  AdvObjectMatches in '..\Libraries\Support\AdvObjectMatches.pas',
  RegExpr in '..\Libraries\Support\RegExpr.pas',
  FHIRUtilities in '..\Libraries\refplat\FHIRUtilities.pas',
  TextUtilities in '..\Libraries\Support\TextUtilities.pas',
  FHIRTags in 'FHIRTags.pas',
  FHIRClient in '..\Libraries\refplat\FHIRClient.pas',
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
  FHIRValidator in 'FHIRValidator.pas',
  AltovaXMLLib_TLB in '..\Libraries\Support\AltovaXMLLib_TLB.pas',
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
  kCritSct in '..\Libraries\Support\kCritSct.pas',
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
  IdSoapConsts in 'C:\HL7Connect\indysoap\source\IdSoapConsts.pas',
  IdSoapClasses in 'C:\HL7Connect\indysoap\source\IdSoapClasses.pas',
  IdSoapDebug in 'C:\HL7Connect\indysoap\source\IdSoapDebug.pas',
  IdSoapResourceStrings in 'C:\HL7Connect\indysoap\source\IdSoapResourceStrings.pas',
  IdSoapTracker in 'C:\HL7Connect\indysoap\source\IdSoapTracker.pas',
  IdSoapUtilities in 'C:\HL7Connect\indysoap\source\IdSoapUtilities.pas',
  IdSoapExceptions in 'C:\HL7Connect\indysoap\source\IdSoapExceptions.pas',
  IdSoapMsXml in 'C:\HL7Connect\indysoap\source\IdSoapMsXml.pas',
  IdSoapXML in 'C:\HL7Connect\indysoap\source\IdSoapXML.pas',
  IdSoapComponent in 'C:\HL7Connect\indysoap\source\IdSoapComponent.pas',
  IdSoapNamespaces in 'C:\HL7Connect\indysoap\source\IdSoapNamespaces.pas',
  IdSoapMime in 'C:\HL7Connect\indysoap\source\IdSoapMime.pas',
  IdSoapBase64 in 'C:\HL7Connect\indysoap\source\IdSoapBase64.pas',
  FHIRServerConstants in 'FHIRServerConstants.pas',
  DecimalTests in '..\Libraries\tests\DecimalTests.pas',
  UcumTests in '..\Libraries\tests\UcumTests.pas';

begin
  try
    ExecuteFhirServer;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.


