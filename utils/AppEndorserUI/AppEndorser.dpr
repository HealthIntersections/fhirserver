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
program AppEndorser;

uses
  FastMM4 in '..\..\Libraries\FMM\FastMM4.pas',
  System.StartUpCopy,
  FMX.Forms,
  AppEndorserForm in 'AppEndorserForm.pas' {AppEndorsementForm},
  FHIRClient in '..\..\reference-platform\client\FHIRClient.pas',
  StringSupport in '..\..\reference-platform\support\StringSupport.pas',
  MathSupport in '..\..\reference-platform\support\MathSupport.pas',
  EncodeSupport in '..\..\reference-platform\support\EncodeSupport.pas',
  GUIDSupport in '..\..\reference-platform\support\GUIDSupport.pas',
  OSXUtils in '..\..\reference-platform\support\OSXUtils.pas',
  DecimalSupport in '..\..\reference-platform\support\DecimalSupport.pas',
  DateSupport in '..\..\reference-platform\support\DateSupport.pas',
  MimeMessage in '..\..\reference-platform\support\MimeMessage.pas',
  AdvObjects in '..\..\reference-platform\support\AdvObjects.pas',
  AdvExceptions in '..\..\reference-platform\support\AdvExceptions.pas',
  AdvGenerics in '..\..\reference-platform\support\AdvGenerics.pas',
  AdvStreams in '..\..\reference-platform\support\AdvStreams.pas',
  AdvObjectLists in '..\..\reference-platform\support\AdvObjectLists.pas',
  MemorySupport in '..\..\reference-platform\support\MemorySupport.pas',
  AdvItems in '..\..\reference-platform\support\AdvItems.pas',
  AdvFilers in '..\..\reference-platform\support\AdvFilers.pas',
  ColourSupport in '..\..\reference-platform\support\ColourSupport.pas',
  CurrencySupport in '..\..\reference-platform\support\CurrencySupport.pas',
  AdvCollections in '..\..\reference-platform\support\AdvCollections.pas',
  AdvPersistents in '..\..\reference-platform\support\AdvPersistents.pas',
  AdvIterators in '..\..\reference-platform\support\AdvIterators.pas',
  AdvMemories in '..\..\reference-platform\support\AdvMemories.pas',
  AdvBuffers in '..\..\reference-platform\support\AdvBuffers.pas',
  BytesSupport in '..\..\reference-platform\support\BytesSupport.pas',
  AdvStringBuilders in '..\..\reference-platform\support\AdvStringBuilders.pas',
  FileSupport in '..\..\reference-platform\support\FileSupport.pas',
  AdvPersistentLists in '..\..\reference-platform\support\AdvPersistentLists.pas',
  AdvFiles in '..\..\reference-platform\support\AdvFiles.pas',
  ErrorSupport in '..\..\reference-platform\support\ErrorSupport.pas',
  AdvStringMatches in '..\..\reference-platform\support\AdvStringMatches.pas',
  AdvJSON in '..\..\reference-platform\support\AdvJSON.pas',
  AdvVCLStreams in '..\..\reference-platform\support\AdvVCLStreams.pas',
  AdvTextFormatters in '..\..\reference-platform\support\AdvTextFormatters.pas',
  AdvFormatters in '..\..\reference-platform\support\AdvFormatters.pas',
  AdvTextExtractors in '..\..\reference-platform\support\AdvTextExtractors.pas',
  AdvExtractors in '..\..\reference-platform\support\AdvExtractors.pas',
  AdvCharacterSets in '..\..\reference-platform\support\AdvCharacterSets.pas',
  AdvOrdinalSets in '..\..\reference-platform\support\AdvOrdinalSets.pas',
  AdvStringLists in '..\..\reference-platform\support\AdvStringLists.pas',
  AdvCSVFormatters in '..\..\reference-platform\support\AdvCSVFormatters.pas',
  AdvCSVExtractors in '..\..\reference-platform\support\AdvCSVExtractors.pas',
  AdvStreamReaders in '..\..\reference-platform\support\AdvStreamReaders.pas',
  AdvStringStreams in '..\..\reference-platform\support\AdvStringStreams.pas',
  TextUtilities in '..\..\reference-platform\support\TextUtilities.pas',
  ParserSupport in '..\..\reference-platform\support\ParserSupport.pas',
  FHIRParser in '..\..\reference-platform\support\FHIRParser.pas',
  FHIRParserXml in '..\..\reference-platform\dstu3\FHIRParserXml.pas',
  FHIRParserBase in '..\..\reference-platform\support\FHIRParserBase.pas',
  MXML in '..\..\reference-platform\support\MXML.pas',
  XMLBuilder in '..\..\reference-platform\support\XMLBuilder.pas',
  MXmlBuilder in '..\..\reference-platform\support\MXmlBuilder.pas',
  AdvXmlBuilders in '..\..\reference-platform\support\AdvXmlBuilders.pas',
  AdvXMLFormatters in '..\..\reference-platform\support\AdvXMLFormatters.pas',
  AdvXMLEntities in '..\..\reference-platform\support\AdvXMLEntities.pas',
  TurtleParser in '..\..\reference-platform\support\TurtleParser.pas',
  FHIRBase in '..\..\reference-platform\support\FHIRBase.pas',
  AdvNames in '..\..\reference-platform\support\AdvNames.pas',
  FHIRUtilities in '..\..\reference-platform\dstu3\FHIRUtilities.pas',
  OIDSupport in '..\..\reference-platform\support\OIDSupport.pas',
  ParseMap in '..\..\reference-platform\support\ParseMap.pas',
  AdvZipWriters in '..\..\reference-platform\support\AdvZipWriters.pas',
  AdvZipDeclarations in '..\..\reference-platform\support\AdvZipDeclarations.pas',
  AdvZipParts in '..\..\reference-platform\support\AdvZipParts.pas',
  AdvZipUtilities in '..\..\reference-platform\support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\..\reference-platform\support\AdvZipWorkers.pas',
  AdvNameBuffers in '..\..\reference-platform\support\AdvNameBuffers.pas',
  AdvObjectMatches in '..\..\reference-platform\support\AdvObjectMatches.pas',
  InternetFetcher in '..\..\reference-platform\support\InternetFetcher.pas',
  FHIRContext in '..\..\reference-platform\dstu3\FHIRContext.pas',
  FHIRTypes in '..\..\reference-platform\dstu3\FHIRTypes.pas',
  FHIRResources in '..\..\reference-platform\dstu3\FHIRResources.pas',
  FHIRSupport in '..\..\reference-platform\dstu3\FHIRSupport.pas',
  JWT in '..\..\reference-platform\support\JWT.pas',
  HMAC in '..\..\reference-platform\support\HMAC.pas',
  libeay32 in '..\..\reference-platform\support\libeay32.pas',
  SCIMObjects in '..\..\reference-platform\support\SCIMObjects.pas',
  GraphQL in '..\..\reference-platform\support\GraphQL.pas',
  FHIRConstants in '..\..\reference-platform\dstu3\FHIRConstants.pas',
  FHIRSecurity in '..\..\reference-platform\support\FHIRSecurity.pas',
  FHIRTags in '..\..\reference-platform\dstu3\FHIRTags.pas',
  FHIRLang in '..\..\reference-platform\support\FHIRLang.pas',
  FHIRXhtml in '..\..\reference-platform\support\FHIRXhtml.pas',
  FHIRParserJson in '..\..\reference-platform\dstu3\FHIRParserJson.pas',
  FHIRParserTurtle in '..\..\reference-platform\dstu3\FHIRParserTurtle.pas',
  FHIRMetaModel in '..\..\reference-platform\dstu3\FHIRMetaModel.pas',
  FHIRProfileUtilities in '..\..\reference-platform\dstu3\FHIRProfileUtilities.pas',
  kCritSct in '..\..\reference-platform\support\kCritSct.pas',
  AdvZipReaders in '..\..\reference-platform\support\AdvZipReaders.pas',
  FhirPath in '..\..\reference-platform\dstu3\FhirPath.pas',
  SystemSupport in '..\..\reference-platform\support\SystemSupport.pas',
  ThreadSupport in '..\..\reference-platform\support\ThreadSupport.pas',
  SmartOnFhirUtilities in '..\..\reference-platform\client\SmartOnFhirUtilities.pas',
  MarkdownProcessor in '..\..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownCommonMark in '..\..\..\markdown\source\MarkdownCommonMark.pas',
  HashSupport in '..\..\reference-platform\support\HashSupport.pas',
  CDSHooksUtilities in '..\..\reference-platform\support\CDSHooksUtilities.pas',
  {$IFNDEF OSX}
  AfsStreamManagers in '..\..\reference-platform\support\AfsStreamManagers.pas',
  AfsVolumes in '..\..\reference-platform\support\AfsVolumes.pas',
  AfsResourceVolumes in '..\..\reference-platform\support\AfsResourceVolumes.pas',
  AdvWinInetClients in '..\..\reference-platform\support\AdvWinInetClients.pas',
  {$ENDIF }
  AdvStringHashes in '..\..\reference-platform\support\AdvStringHashes.pas',
  AdvHashes in '..\..\reference-platform\support\AdvHashes.pas',
  FastMM4Messages in '..\..\Libraries\FMM\FastMM4Messages.pas',
  OrganizationChooser in 'OrganizationChooser.pas' {OrganizationSelectionForm},
  MasterForm in 'MasterForm.pas' {MasterToolsForm},
  ServerForm in 'ServerForm.pas' {ServerFrameForm: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMasterToolsForm, MasterToolsForm);
  Application.CreateForm(TAppEndorsementForm, AppEndorsementForm);
  Application.CreateForm(TOrganizationSelectionForm, OrganizationSelectionForm);
  Application.Run;
end.
