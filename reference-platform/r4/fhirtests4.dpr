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
  ActiveX,
  AdvBuffers in '..\support\AdvBuffers.pas',
  AdvCSVExtractors in '..\support\AdvCSVExtractors.pas',
  AdvCSVFormatters in '..\support\AdvCSVFormatters.pas',
  AdvCharacterSets in '..\support\AdvCharacterSets.pas',
  AdvClassHashes in '..\support\AdvClassHashes.pas',
  AdvCollections in '..\support\AdvCollections.pas',
  AdvControllers in '..\support\AdvControllers.pas',
  AdvExceptions in '..\support\AdvExceptions.pas',
  AdvExtractors in '..\support\AdvExtractors.pas',
  AdvFilers in '..\support\AdvFilers.pas',
  AdvFiles in '..\support\AdvFiles.pas',
  AdvFormatters in '..\support\AdvFormatters.pas',
  AdvGenerics in '..\support\AdvGenerics.pas',
  AdvHashes in '..\support\AdvHashes.pas',
  AdvItems in '..\support\AdvItems.pas',
  AdvIterators in '..\support\AdvIterators.pas',
  AdvJSON in '..\support\AdvJSON.pas',
  AdvMemories in '..\support\AdvMemories.pas',
  AdvNameBuffers in '..\support\AdvNameBuffers.pas',
  AdvNames in '..\support\AdvNames.pas',
  AdvObjectLists in '..\support\AdvObjectLists.pas',
  AdvObjectMatches in '..\support\AdvObjectMatches.pas',
  AdvObjects in '..\Support\AdvObjects.pas',
  AdvOrdinalSets in '..\support\AdvOrdinalSets.pas',
  AdvPersistentLists in '..\support\AdvPersistentLists.pas',
  AdvPersistents in '..\support\AdvPersistents.pas',
  AdvStreamReaders in '..\support\AdvStreamReaders.pas',
  AdvStreams in '..\support\AdvStreams.pas',
  AdvStringBuilders in '..\support\AdvStringBuilders.pas',
  AdvStringHashes in '..\support\AdvStringHashes.pas',
  AdvStringLists in '..\support\AdvStringLists.pas',
  AdvStringMatches in '..\support\AdvStringMatches.pas',
  AdvStringStreams in '..\support\AdvStringStreams.pas',
  AdvTextExtractors in '..\support\AdvTextExtractors.pas',
  AdvTextFormatters in '..\support\AdvTextFormatters.pas',
  AdvVCLStreams in '..\support\AdvVCLStreams.pas',
  AdvWinInetClients in '..\support\AdvWinInetClients.pas',
  AdvXMLEntities in '..\support\AdvXMLEntities.pas',
  AdvXMLFormatters in '..\support\AdvXMLFormatters.pas',
  AdvXmlBuilders in '..\support\AdvXmlBuilders.pas',
  AdvZipDeclarations in '..\support\AdvZipDeclarations.pas',
  AdvZipParts in '..\support\AdvZipParts.pas',
  AdvZipReaders in '..\support\AdvZipReaders.pas',
  AdvZipUtilities in '..\support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\support\AdvZipWorkers.pas',
  AdvZipWriters in '..\support\AdvZipWriters.pas',
  AfsResourceVolumes in '..\support\AfsResourceVolumes.pas',
  AfsStreamManagers in '..\support\AfsStreamManagers.pas',
  AfsVolumes in '..\support\AfsVolumes.pas',
  BytesSupport in '..\support\BytesSupport.pas',
  ColourSupport in '..\support\ColourSupport.pas',
  CurrencySupport in '..\support\CurrencySupport.pas',
  DateSupport in '..\support\DateSupport.pas',
  DecimalSupport in '..\support\DecimalSupport.pas',
  DecimalTests in '..\support\Tests\DecimalTests.pas',
  DifferenceEngine in '..\support\DifferenceEngine.pas',
  DifferenceEngineTests in '..\support\Tests\DifferenceEngineTests.pas',
  EncodeSupport in '..\support\EncodeSupport.pas',
  ErrorSupport in '..\support\ErrorSupport.pas',
  FHIRBase in '..\support\FHIRBase.pas',
  FHIRConstants,
  FHIRLang in '..\support\FHIRLang.pas',
  FHIRParser in '..\support\FHIRParser.pas',
  FHIRParserBase in '..\support\FHIRParserBase.pas',
  FHIRPath in 'FHIRPath.pas',
  FHIRPathTests in 'tests\FHIRPathTests.pas',
  FHIRResources in 'FHIRResources.pas',
  FHIRSupport in '..\support\FHIRSupport.pas',
  FHIRTestWorker in 'tests\FHIRTestWorker.pas',
  FHIRUtilities in 'FHIRUtilities.pas',
  FHIRValidatorTests in 'tests\FHIRValidatorTests.pas',
  FHIRXhtml in '..\support\FHIRXhtml.pas',
  FileSupport in '..\support\FileSupport.pas',
  GUIDSupport in '..\Support\GUIDSupport.pas',
  GraphQL in '..\support\GraphQL.pas',
  HL7V2DateSupport in '..\support\HL7V2DateSupport.pas',
  HMAC in '..\support\HMAC.pas',
  HashSupport in '..\support\HashSupport.pas',
  IdSSLOpenSSLHeaders,
  InternetFetcher in '..\support\InternetFetcher.pas',
  JWT in '..\support\JWT.pas',
  JWTTests in '..\support\Tests\JWTTests.pas',
  JsonTests in '..\support\Tests\JsonTests.pas',
  kCritSct in '..\support\kCritSct.pas',
  libeay32 in '..\support\libeay32.pas',
  MXML in '..\support\MXML.pas',
  MXmlBuilder in '..\support\MXmlBuilder.pas',
  MathSupport in '..\support\MathSupport.pas',
  MemorySupport in '..\support\MemorySupport.pas',
  MimeMessage in '..\support\MimeMessage.pas',
  MsXmlParser in '..\support\MsXmlParser.pas',
  OIDSupport in '..\support\OIDSupport.pas',
  ParseMap in '..\support\ParseMap.pas',
  ParserSupport in '..\support\ParserSupport.pas',
  RDFUtilities in '..\support\RDFUtilities.pas',
  ShellSupport in '..\support\ShellSupport.pas',
  StringSupport in '..\support\StringSupport.pas',
  SysUtils,
  SystemSupport in '..\support\SystemSupport.pas',
  TestInsight.DUnitX,
  TextUtilities in '..\support\TextUtilities.pas',
  ThreadSupport in '..\support\ThreadSupport.pas',
  XMLBuilder in '..\support\XMLBuilder.pas',
  XmlTests in '..\support\Tests\XmlTests.pas',
  XmlPatch in '..\support\XmlPatch.pas',
  SCIMObjects in '..\support\SCIMObjects.pas',
  FHIRSecurity in '..\support\FHIRSecurity.pas',
  FHIRParserXml in 'FHIRParserXml.pas',
  FHIRParserJson in 'FHIRParserJson.pas',
  TurtleParser in '..\support\TurtleParser.pas',
  FHIRParserTurtle in 'FHIRParserTurtle.pas',
  FHIRLog in '..\support\FHIRLog.pas',
  Logging in '..\..\Server\Logging.pas',
  FHIRIndexBase in '..\support\FHIRIndexBase.pas',
  Javascript in '..\..\Libraries\js\Javascript.pas',
  ChakraCommon in '..\..\Libraries\js\ChakraCommon.pas',
  JavascriptTests in '..\..\Libraries\js\JavascriptTests.pas',
  DigitalSignatures in '..\support\DigitalSignatures.pas';

var
  s: String;

begin
  CoInitialize(nil);
  s := ExtractFilePath(Paramstr(0));
  IdOpenSSLSetLibPath(s);
  GBasePath := 'C:\work\org.hl7.fhir';
  RunRegisteredTests;
  TTestingWorkerContext.closeUp;

end.
