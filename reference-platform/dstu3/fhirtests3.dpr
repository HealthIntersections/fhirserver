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
program fhirtests3;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\..\Libraries\FMM\FastMM4.pas',
  FastMM4Messages in '..\..\Libraries\FMM\FastMM4Messages.pas',
  TestInsight.DUnitX,
  ActiveX,
  SysUtils,
  IdSSLOpenSSLHeaders,
  DecimalTests in '..\support\tests\DecimalTests.pas',
  JWTTests in '..\Support\tests\JWTTests.pas',
  FHIRValidatorTests in 'tests\FHIRValidatorTests.pas',
  AdvFactories in '..\support\AdvFactories.pas',
  AdvControllers in '..\support\AdvControllers.pas',
  AdvPersistents in '..\support\AdvPersistents.pas',
  AdvObjects in '..\support\AdvObjects.pas',
  AdvExceptions in '..\support\AdvExceptions.pas',
  AdvFilers in '..\support\AdvFilers.pas',
  StringSupport in '..\support\StringSupport.pas',
  MathSupport in '..\support\MathSupport.pas',
  DecimalSupport in '..\support\DecimalSupport.pas',
  GUIDSupport in '..\support\GUIDSupport.pas',
  FileSupport in '..\support\FileSupport.pas',
  MemorySupport in '..\support\MemorySupport.pas',
  DateSupport in '..\support\DateSupport.pas',
  ErrorSupport in '..\support\ErrorSupport.pas',
  SystemSupport in '..\support\SystemSupport.pas',
  ThreadSupport in '..\support\ThreadSupport.pas',
  EncodeSupport in '..\support\EncodeSupport.pas',
  ColourSupport in '..\support\ColourSupport.pas',
  CurrencySupport in '..\support\CurrencySupport.pas',
  AdvPersistentLists in '..\support\AdvPersistentLists.pas',
  AdvObjectLists in '..\support\AdvObjectLists.pas',
  AdvItems in '..\support\AdvItems.pas',
  AdvCollections in '..\support\AdvCollections.pas',
  AdvIterators in '..\support\AdvIterators.pas',
  AdvClassHashes in '..\support\AdvClassHashes.pas',
  AdvHashes in '..\support\AdvHashes.pas',
  HashSupport in '..\support\HashSupport.pas',
  AdvStringHashes in '..\support\AdvStringHashes.pas',
  AdvProfilers in '..\support\AdvProfilers.pas',
  AdvStringIntegerMatches in '..\support\AdvStringIntegerMatches.pas',
  AdvStreams in '..\support\AdvStreams.pas',
  AdvParameters in '..\support\AdvParameters.pas',
  AdvExclusiveCriticalSections in '..\support\AdvExclusiveCriticalSections.pas',
  AdvThreads in '..\support\AdvThreads.pas',
  AdvSignals in '..\support\AdvSignals.pas',
  AdvIntegerMatches in '..\support\AdvIntegerMatches.pas',
  AdvBuffers in '..\support\AdvBuffers.pas',
  BytesSupport in '..\support\BytesSupport.pas',
  AdvStringBuilders in '..\support\AdvStringBuilders.pas',
  AdvFiles in '..\support\AdvFiles.pas',
  AdvLargeIntegerMatches in '..\support\AdvLargeIntegerMatches.pas',
  AdvStringLargeIntegerMatches in '..\support\AdvStringLargeIntegerMatches.pas',
  AdvStringLists in '..\support\AdvStringLists.pas',
  AdvCSVFormatters in '..\support\AdvCSVFormatters.pas',
  AdvTextFormatters in '..\support\AdvTextFormatters.pas',
  AdvFormatters in '..\support\AdvFormatters.pas',
  AdvCSVExtractors in '..\support\AdvCSVExtractors.pas',
  AdvTextExtractors in '..\support\AdvTextExtractors.pas',
  AdvExtractors in '..\support\AdvExtractors.pas',
  AdvCharacterSets in '..\support\AdvCharacterSets.pas',
  AdvOrdinalSets in '..\support\AdvOrdinalSets.pas',
  AdvStreamReaders in '..\support\AdvStreamReaders.pas',
  AdvStringStreams in '..\support\AdvStringStreams.pas',
  AdvStringMatches in '..\support\AdvStringMatches.pas',
  AdvMemories in '..\support\AdvMemories.pas',
  AdvVCLStreams in '..\support\AdvVCLStreams.pas',
  AdvXmlBuilders in '..\support\AdvXmlBuilders.pas',
  AdvXMLFormatters in '..\support\AdvXMLFormatters.pas',
  AdvXMLEntities in '..\support\AdvXMLEntities.pas',
  AdvJSON in '..\support\AdvJSON.pas',
  AdvGenerics in '..\support\AdvGenerics.pas',
  AdvWinInetClients in '..\support\AdvWinInetClients.pas',
  AdvStringObjectMatches in '..\support\AdvStringObjectMatches.pas',
  AdvNames in '..\support\AdvNames.pas',
  AdvObjectMatches in '..\support\AdvObjectMatches.pas',
  AdvNameBuffers in '..\support\AdvNameBuffers.pas',
  AdvZipReaders in '..\support\AdvZipReaders.pas',
  AdvZipWorkers in '..\support\AdvZipWorkers.pas',
  AdvZipParts in '..\support\AdvZipParts.pas',
  AdvZipUtilities in '..\support\AdvZipUtilities.pas',
  AdvZipDeclarations in '..\support\AdvZipDeclarations.pas',
  MsXmlParser in '..\support\MsXmlParser.pas',
  XMLBuilder in '..\support\XMLBuilder.pas',
  MXmlBuilder in '..\support\MXmlBuilder.pas',
  XmlPatch in '..\support\XmlPatch.pas',
  RDFUtilities in '..\support\RDFUtilities.pas',
  TextUtilities in '..\support\TextUtilities.pas',
  HL7V2DateSupport in '..\support\HL7V2DateSupport.pas',
  ParseMap in '..\support\ParseMap.pas',
  AfsResourceVolumes in '..\support\AfsResourceVolumes.pas',
  AfsVolumes in '..\support\AfsVolumes.pas',
  AfsStreamManagers in '..\support\AfsStreamManagers.pas',
  OIDSupport in '..\support\OIDSupport.pas',
  MimeMessage in '..\support\MimeMessage.pas',
  kCritSct in '..\support\kCritSct.pas',
  InternetFetcher in '..\support\InternetFetcher.pas',
  JWT in '..\support\JWT.pas',
  HMAC in '..\support\HMAC.pas',
  libeay32 in '..\support\libeay32.pas',
  FHIRBase in '..\support\FHIRBase.pas',
  FHIRXhtml in '..\support\FHIRXhtml.pas',
  FHIRResources in 'FHIRResources.pas',
  FHIRConstants,
  FHIRParser in '..\support\FHIRParser.pas',
  FHIRParserXml,
  FHIRParserJson,
  FHIRParserTurtle,
  FHIRParserBase in '..\support\FHIRParserBase.pas',
  FHIRSupport in '..\support\FHIRSupport.pas',
  FHIRLang in '..\support\FHIRLang.pas',
  FHIRUtilities in 'FHIRUtilities.pas',
  FHIRPath in 'FHIRPath.pas',
  FHIRTestWorker in 'tests\FHIRTestWorker.pas',
  ShellSupport in '..\support\ShellSupport.pas',
  DifferenceEngine in '..\support\DifferenceEngine.pas',
  DifferenceEngineTests in '..\support\tests\DifferenceEngineTests.pas',
  FHIRPathTests in 'tests\FHIRPathTests.pas',
  GraphQL in '..\support\GraphQL.pas',
  AdvZipWriters in '..\support\AdvZipWriters.pas',
  ParserSupport in '..\support\ParserSupport.pas',
  MXML in '..\support\MXML.pas',
  FHIRParserTests in 'tests\FHIRParserTests.pas',
  XmlTests in '..\support\Tests\XmlTests.pas',
  JsonTests in '..\support\Tests\JsonTests.pas',
  SCIMObjects in '..\support\SCIMObjects.pas',
  FHIRSecurity in '..\support\FHIRSecurity.pas',
  TurtleParser in '..\support\TurtleParser.pas',
  FHIRIndexBase in '..\support\FHIRIndexBase.pas',
  DigitalSignatures in '..\support\DigitalSignatures.pas';

var
  s: String;

begin
  CoInitialize(nil);
  s := ExtractFilePath(Paramstr(0));
  IdOpenSSLSetLibPath(s);
  GBasePath := 'C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu3';
  RunRegisteredTests;
  TTestingWorkerContext.closeUp;

end.
