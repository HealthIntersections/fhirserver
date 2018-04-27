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
  FHIRValidatorTests3 in 'tests\FHIRValidatorTests3.pas',
  FHIR.Support.Factory in '..\support\FHIR.Support.Factory.pas',
  AdvControllers in '..\support\AdvControllers.pas',
  AdvPersistents in '..\support\AdvPersistents.pas',
  FHIR.Support.Objects in '..\support\FHIR.Support.Objects.pas',
  FHIR.Support.Exceptions in '..\support\FHIR.Support.Exceptions.pas',
  FHIR.Support.Filers in '..\support\FHIR.Support.Filers.pas',
  FHIR.Support.Strings in '..\support\FHIR.Support.Strings.pas',
  FHIR.Support.Math in '..\support\FHIR.Support.Math.pas',
  FHIR.Support.Decimal in '..\support\FHIR.Support.Decimal.pas',
  GUIDSupport in '..\support\GUIDSupport.pas',
  FHIR.Support.System in '..\support\FHIR.Support.System.pas',
  MemorySupport in '..\support\MemorySupport.pas',
  FHIR.Support.DateTime in '..\support\FHIR.Support.DateTime.pas',
  ErrorSupport in '..\support\ErrorSupport.pas',
  SystemSupport in '..\support\SystemSupport.pas',
  ThreadSupport in '..\support\ThreadSupport.pas',
  EncodeSupport in '..\support\EncodeSupport.pas',
  ColourSupport in '..\support\ColourSupport.pas',
  CurrencySupport in '..\support\CurrencySupport.pas',
  AdvPersistentLists in '..\support\AdvPersistentLists.pas',
  AdvObjectLists in '..\support\AdvObjectLists.pas',
  AdvItems in '..\support\AdvItems.pas',
  FHIR.Support.Collections in '..\support\FHIR.Support.Collections.pas',
  AdvIterators in '..\support\AdvIterators.pas',
  AdvClassHashes in '..\support\AdvClassHashes.pas',
  AdvHashes in '..\support\AdvHashes.pas',
  HashSupport in '..\support\HashSupport.pas',
  AdvStringHashes in '..\support\AdvStringHashes.pas',
  AdvProfilers in '..\support\AdvProfilers.pas',
  AdvStringIntegerMatches in '..\support\AdvStringIntegerMatches.pas',
  FHIR.Support.Stream in '..\support\FHIR.Support.Stream.pas',
  AdvParameters in '..\support\AdvParameters.pas',
  AdvExclusiveCriticalSections in '..\support\AdvExclusiveCriticalSections.pas',
  AdvThreads in '..\support\AdvThreads.pas',
  AdvSignals in '..\support\AdvSignals.pas',
  AdvIntegerMatches in '..\support\AdvIntegerMatches.pas',
  AdvBuffers in '..\support\AdvBuffers.pas',
  FHIR.Support.Binary in '..\support\FHIR.Support.Binary.pas',
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
  FHIR.Support.Json in '..\support\FHIR.Support.Json.pas',
  FHIR.Support.Generics in '..\support\FHIR.Support.Generics.pas',
  FHIR.Support.WInInet in '..\support\FHIR.Support.WInInet.pas',
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
  FHIR.Xml.Builder in '..\support\FHIR.Xml.Builder.pas',
  MXmlBuilder in '..\support\MXmlBuilder.pas',
  FHIR.Xml.Patch in '..\support\FHIR.Xml.Patch.pas',
  FHIR.Web.Rdf in '..\support\FHIR.Web.Rdf.pas',
  TextUtilities in '..\support\TextUtilities.pas',
  HL7V2DateSupport in '..\support\HL7V2DateSupport.pas',
  FHIR.Web.ParseMap in '..\support\FHIR.Web.ParseMap.pas',
  AfsResourceVolumes in '..\support\AfsResourceVolumes.pas',
  AfsVolumes in '..\support\AfsVolumes.pas',
  AfsStreamManagers in '..\support\AfsStreamManagers.pas',
  OIDSupport in '..\support\OIDSupport.pas',
  FHIR.Support.Mime in '..\support\FHIR.Support.Mime.pas',
  FHIR.Support.Lock in '..\support\FHIR.Support.Lock.pas',
  FHIR.Web.Fetcher in '..\support\FHIR.Web.Fetcher.pas',
  JWT in '..\support\JWT.pas',
  HMAC in '..\support\HMAC.pas',
  libeay32 in '..\support\libeay32.pas',
  FHIR.Base.Objects in '..\support\FHIR.Base.Objects.pas',
  FHIR.Base.Xhtml in '..\support\FHIR.Base.Xhtml.pas',
  FHIR.R3.Resources in 'FHIR.R3.Resources.pas',
  FHIR.R3.Constants,
  FHIR.Tools.Parser in '..\support\FHIR.Tools.Parser.pas',
  FHIR.R3.Xml,
  FHIR.R3.Json,
  FHIR.R3.Turtle,
  FHIR.Base.Parser in '..\support\FHIR.Base.Parser.pas',
  FHIR.Tools.Session in '..\support\FHIR.Tools.Session.pas',
  FHIR.Base.Lang in '..\support\FHIR.Base.Lang.pas',
  FHIR.R3.Utilities in 'FHIR.R3.Utilities.pas',
  FHIR.R3.PathEngine in 'FHIR.R3.PathEngine.pas',
  FHIRTestWorker3 in 'tests\FHIRTestWorker3.pas',
  FHIR.Support.Shell in '..\support\FHIR.Support.Shell.pas',
  FHIR.Tools.DiffEngine in '..\support\FHIR.Tools.DiffEngine.pas',
  DifferenceEngineTests in '..\support\tests\DifferenceEngineTests.pas',
  FHIRPathTests3 in 'tests\FHIRPathTests3.pas',
  GraphQL in '..\support\GraphQL.pas',
  AdvZipWriters in '..\support\AdvZipWriters.pas',
  ParserSupport in '..\support\ParserSupport.pas',
  FHIR.Support.MXml in '..\support\FHIR.Support.MXml.pas',
  FHIRParserTests3 in 'tests\FHIRParserTests3.pas',
  XmlTests in '..\support\Tests\XmlTests.pas',
  JsonTests in '..\support\Tests\JsonTests.pas',
  FHIR.Base.Scim in '..\support\FHIR.Base.Scim.pas',
  FHIR.Tools.Security in '..\support\FHIR.Tools.Security.pas',
  FHIR.Support.Turtle in '..\support\FHIR.Support.Turtle.pas',
  FHIR.Tools.Indexing in '..\support\FHIR.Tools.Indexing.pas',
  FHIR.Support.Signatures in '..\support\FHIR.Support.Signatures.pas',
  FHIR.Ucum.IFace in '..\support\FHIR.Ucum.IFace.pas',
  FHIR.Tools.XhtmlComp in '..\support\FHIR.Tools.XhtmlComp.pas';

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
