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
  FHIR.Support.Collections in '..\support\FHIR.Support.Collections.pas',
  AdvControllers in '..\support\AdvControllers.pas',
  FHIR.Support.Exceptions in '..\support\FHIR.Support.Exceptions.pas',
  AdvExtractors in '..\support\AdvExtractors.pas',
  FHIR.Support.Filers in '..\support\FHIR.Support.Filers.pas',
  AdvFiles in '..\support\AdvFiles.pas',
  AdvFormatters in '..\support\AdvFormatters.pas',
  FHIR.Support.Generics in '..\support\FHIR.Support.Generics.pas',
  AdvHashes in '..\support\AdvHashes.pas',
  AdvItems in '..\support\AdvItems.pas',
  AdvIterators in '..\support\AdvIterators.pas',
  FHIR.Support.Json in '..\support\FHIR.Support.Json.pas',
  AdvMemories in '..\support\AdvMemories.pas',
  AdvNameBuffers in '..\support\AdvNameBuffers.pas',
  AdvNames in '..\support\AdvNames.pas',
  AdvObjectLists in '..\support\AdvObjectLists.pas',
  AdvObjectMatches in '..\support\AdvObjectMatches.pas',
  FHIR.Support.Objects in '..\Support\FHIR.Support.Objects.pas',
  AdvOrdinalSets in '..\support\AdvOrdinalSets.pas',
  AdvPersistentLists in '..\support\AdvPersistentLists.pas',
  AdvPersistents in '..\support\AdvPersistents.pas',
  AdvStreamReaders in '..\support\AdvStreamReaders.pas',
  FHIR.Support.Stream in '..\support\FHIR.Support.Stream.pas',
  AdvStringBuilders in '..\support\AdvStringBuilders.pas',
  AdvStringHashes in '..\support\AdvStringHashes.pas',
  AdvStringLists in '..\support\AdvStringLists.pas',
  AdvStringMatches in '..\support\AdvStringMatches.pas',
  AdvStringStreams in '..\support\AdvStringStreams.pas',
  AdvTextExtractors in '..\support\AdvTextExtractors.pas',
  AdvTextFormatters in '..\support\AdvTextFormatters.pas',
  AdvVCLStreams in '..\support\AdvVCLStreams.pas',
  FHIR.Support.WInInet in '..\support\FHIR.Support.WInInet.pas',
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
  FHIR.Support.Binary in '..\support\FHIR.Support.Binary.pas',
  ColourSupport in '..\support\ColourSupport.pas',
  CurrencySupport in '..\support\CurrencySupport.pas',
  FHIR.Support.DateTime in '..\support\FHIR.Support.DateTime.pas',
  FHIR.Support.Decimal in '..\support\FHIR.Support.Decimal.pas',
  DecimalTests in '..\support\Tests\DecimalTests.pas',
  FHIR.Tools.DiffEngine in '..\support\FHIR.Tools.DiffEngine.pas',
  DifferenceEngineTests in '..\support\Tests\DifferenceEngineTests.pas',
  EncodeSupport in '..\support\EncodeSupport.pas',
  ErrorSupport in '..\support\ErrorSupport.pas',
  FHIR.Base.Objects in '..\support\FHIR.Base.Objects.pas',
  FHIR.R4.Constants,
  FHIR.Base.Lang in '..\support\FHIR.Base.Lang.pas',
  FHIR.Tools.Parser in '..\support\FHIR.Tools.Parser.pas',
  FHIR.Base.Parser in '..\support\FHIR.Base.Parser.pas',
  FHIR.R4.PathEngine in 'FHIR.R4.PathEngine.pas',
  FHIRPathTests4 in 'tests\FHIRPathTests4.pas',
  FHIR.R4.Resources in 'FHIR.R4.Resources.pas',
  FHIR.Tools.Session in '..\support\FHIR.Tools.Session.pas',
  FHIRTestWorker4 in 'tests\FHIRTestWorker4.pas',
  FHIR.R4.Utilities in 'FHIR.R4.Utilities.pas',
  FHIR.Base.Xhtml in '..\support\FHIR.Base.Xhtml.pas',
  FHIR.Support.System in '..\support\FHIR.Support.System.pas',
  GUIDSupport in '..\Support\GUIDSupport.pas',
  GraphQL in '..\support\GraphQL.pas',
  HL7V2DateSupport in '..\support\HL7V2DateSupport.pas',
  HMAC in '..\support\HMAC.pas',
  HashSupport in '..\support\HashSupport.pas',
  IdSSLOpenSSLHeaders,
  FHIR.Web.Fetcher in '..\support\FHIR.Web.Fetcher.pas',
  JWT in '..\support\JWT.pas',
  JWTTests in '..\support\Tests\JWTTests.pas',
  JsonTests in '..\support\Tests\JsonTests.pas',
  FHIR.Support.Lock in '..\support\FHIR.Support.Lock.pas',
  libeay32 in '..\support\libeay32.pas',
  FHIR.Support.MXml in '..\support\FHIR.Support.MXml.pas',
  MXmlBuilder in '..\support\MXmlBuilder.pas',
  FHIR.Support.Math in '..\support\FHIR.Support.Math.pas',
  MemorySupport in '..\support\MemorySupport.pas',
  FHIR.Support.Mime in '..\support\FHIR.Support.Mime.pas',
  MsXmlParser in '..\support\MsXmlParser.pas',
  OIDSupport in '..\support\OIDSupport.pas',
  FHIR.Web.ParseMap in '..\support\FHIR.Web.ParseMap.pas',
  ParserSupport in '..\support\ParserSupport.pas',
  FHIR.Web.Rdf in '..\support\FHIR.Web.Rdf.pas',
  FHIR.Support.Shell in '..\support\FHIR.Support.Shell.pas',
  FHIR.Support.Strings in '..\support\FHIR.Support.Strings.pas',
  SysUtils,
  SystemSupport in '..\support\SystemSupport.pas',
  TestInsight.DUnitX,
  TextUtilities in '..\support\TextUtilities.pas',
  ThreadSupport in '..\support\ThreadSupport.pas',
  FHIR.Xml.Builder in '..\support\FHIR.Xml.Builder.pas',
  XmlTests in '..\support\Tests\XmlTests.pas',
  FHIR.Xml.Patch in '..\support\FHIR.Xml.Patch.pas',
  FHIR.Base.Scim in '..\support\FHIR.Base.Scim.pas',
  FHIR.Tools.Security in '..\support\FHIR.Tools.Security.pas',
  FHIR.R4.Xml in 'FHIR.R4.Xml.pas',
  FHIR.R4.Json in 'FHIR.R4.Json.pas',
  FHIR.Support.Turtle in '..\support\FHIR.Support.Turtle.pas',
  FHIR.R4.Turtle in 'FHIR.R4.Turtle.pas',
  FHIR.Debug.Logging in '..\support\FHIR.Debug.Logging.pas',
  Logging in '..\..\Server\Logging.pas',
  FHIR.Tools.Indexing in '..\support\FHIR.Tools.Indexing.pas',
  FHIR.Javascript in '..\..\Libraries\js\FHIR.Javascript.pas',
  FHIR.Javascript.Chakra in '..\..\Libraries\js\FHIR.Javascript.Chakra.pas',
  JavascriptTests in '..\..\Libraries\js\JavascriptTests.pas',
  FHIR.Support.Signatures in '..\support\FHIR.Support.Signatures.pas',
  FHIR.Ucum.IFace in '..\support\FHIR.Ucum.IFace.pas',
  FHIR.Tools.XhtmlComp in '..\support\FHIR.Tools.XhtmlComp.pas';

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
