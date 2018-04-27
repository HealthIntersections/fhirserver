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
  FHIR.Tools.Client in '..\..\reference-platform\client\FHIR.Tools.Client.pas',
  FHIR.Support.Strings in '..\..\reference-platform\support\FHIR.Support.Strings.pas',
  FHIR.Support.Math in '..\..\reference-platform\support\FHIR.Support.Math.pas',
  EncodeSupport in '..\..\reference-platform\support\EncodeSupport.pas',
  GUIDSupport in '..\..\reference-platform\support\GUIDSupport.pas',
  FHIR.Support.Osx in '..\..\reference-platform\support\FHIR.Support.Osx.pas',
  FHIR.Support.Decimal in '..\..\reference-platform\support\FHIR.Support.Decimal.pas',
  FHIR.Support.DateTime in '..\..\reference-platform\support\FHIR.Support.DateTime.pas',
  FHIR.Support.Mime in '..\..\reference-platform\support\FHIR.Support.Mime.pas',
  FHIR.Support.Objects in '..\..\reference-platform\support\FHIR.Support.Objects.pas',
  FHIR.Support.Exceptions in '..\..\reference-platform\support\FHIR.Support.Exceptions.pas',
  FHIR.Support.Generics in '..\..\reference-platform\support\FHIR.Support.Generics.pas',
  FHIR.Support.Stream in '..\..\reference-platform\support\FHIR.Support.Stream.pas',
  AdvObjectLists in '..\..\reference-platform\support\AdvObjectLists.pas',
  MemorySupport in '..\..\reference-platform\support\MemorySupport.pas',
  AdvItems in '..\..\reference-platform\support\AdvItems.pas',
  FHIR.Support.Filers in '..\..\reference-platform\support\FHIR.Support.Filers.pas',
  ColourSupport in '..\..\reference-platform\support\ColourSupport.pas',
  CurrencySupport in '..\..\reference-platform\support\CurrencySupport.pas',
  FHIR.Support.Collections in '..\..\reference-platform\support\FHIR.Support.Collections.pas',
  AdvPersistents in '..\..\reference-platform\support\AdvPersistents.pas',
  AdvIterators in '..\..\reference-platform\support\AdvIterators.pas',
  AdvMemories in '..\..\reference-platform\support\AdvMemories.pas',
  AdvBuffers in '..\..\reference-platform\support\AdvBuffers.pas',
  FHIR.Support.Binary in '..\..\reference-platform\support\FHIR.Support.Binary.pas',
  AdvStringBuilders in '..\..\reference-platform\support\AdvStringBuilders.pas',
  FHIR.Support.System in '..\..\reference-platform\support\FHIR.Support.System.pas',
  AdvPersistentLists in '..\..\reference-platform\support\AdvPersistentLists.pas',
  AdvFiles in '..\..\reference-platform\support\AdvFiles.pas',
  ErrorSupport in '..\..\reference-platform\support\ErrorSupport.pas',
  AdvStringMatches in '..\..\reference-platform\support\AdvStringMatches.pas',
  FHIR.Support.Json in '..\..\reference-platform\support\FHIR.Support.Json.pas',
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
  FHIR.Tools.Parser in '..\..\reference-platform\tools\FHIR.Tools.Parser.pas',
  FHIR.Tools.Xml in '..\..\reference-platform\dstu3\FHIR.Tools.Xml.pas',
  FHIR.Base.Parser in '..\..\reference-platform\base\FHIR.Base.Parser.pas',
  FHIR.Support.MXml in '..\..\reference-platform\support\FHIR.Support.MXml.pas',
  FHIR.Xml.Builder in '..\..\reference-platform\support\FHIR.Xml.Builder.pas',
  MXmlBuilder in '..\..\reference-platform\support\MXmlBuilder.pas',
  AdvXmlBuilders in '..\..\reference-platform\support\AdvXmlBuilders.pas',
  AdvXMLFormatters in '..\..\reference-platform\support\AdvXMLFormatters.pas',
  AdvXMLEntities in '..\..\reference-platform\support\AdvXMLEntities.pas',
  FHIR.Support.Turtle in '..\..\reference-platform\support\FHIR.Support.Turtle.pas',
  FHIR.Base.Objects in '..\..\reference-platform\base\FHIR.Base.Objects.pas',
  AdvNames in '..\..\reference-platform\support\AdvNames.pas',
  FHIR.Tools.Utilities in '..\..\reference-platform\dstu3\FHIR.Tools.Utilities.pas',
  OIDSupport in '..\..\reference-platform\support\OIDSupport.pas',
  FHIR.Web.ParseMap in '..\..\reference-platform\support\FHIR.Web.ParseMap.pas',
  AdvZipWriters in '..\..\reference-platform\support\AdvZipWriters.pas',
  AdvZipDeclarations in '..\..\reference-platform\support\AdvZipDeclarations.pas',
  AdvZipParts in '..\..\reference-platform\support\AdvZipParts.pas',
  AdvZipUtilities in '..\..\reference-platform\support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\..\reference-platform\support\AdvZipWorkers.pas',
  AdvNameBuffers in '..\..\reference-platform\support\AdvNameBuffers.pas',
  AdvObjectMatches in '..\..\reference-platform\support\AdvObjectMatches.pas',
  FHIR.Web.Fetcher in '..\..\reference-platform\support\FHIR.Web.Fetcher.pas',
  FHIR.Tools.Context in '..\..\reference-platform\dstu3\FHIR.Tools.Context.pas',
  FHIR.Tools.Types in '..\..\reference-platform\dstu3\FHIR.Tools.Types.pas',
  FHIR.Tools.Resources in '..\..\reference-platform\dstu3\FHIR.Tools.Resources.pas',
  FHIR.Tools.Session in '..\..\reference-platform\dstu3\FHIR.Tools.Session.pas',
  JWT in '..\..\reference-platform\support\JWT.pas',
  HMAC in '..\..\reference-platform\support\HMAC.pas',
  libeay32 in '..\..\reference-platform\support\libeay32.pas',
  FHIR.Base.Scim in '..\..\reference-platform\base\FHIR.Base.Scim.pas',
  GraphQL in '..\..\reference-platform\support\GraphQL.pas',
  FHIR.Tools.Constants in '..\..\reference-platform\dstu3\FHIR.Tools.Constants.pas',
  FHIR.Tools.Security in '..\..\reference-platform\tools\FHIR.Tools.Security.pas',
  FHIR.Tools.Tags in '..\..\reference-platform\dstu3\FHIR.Tools.Tags.pas',
  FHIR.Base.Lang in '..\..\reference-platform\base\FHIR.Base.Lang.pas',
  FHIR.Base.Xhtml in '..\..\reference-platform\base\FHIR.Base.Xhtml.pas',
  FHIR.Tools.Json in '..\..\reference-platform\dstu3\FHIR.Tools.Json.pas',
  FHIR.Tools.Turtle in '..\..\reference-platform\dstu3\FHIR.Tools.Turtle.pas',
  FHIR.Tools.ElementModel in '..\..\reference-platform\dstu3\FHIR.Tools.ElementModel.pas',
  FHIR.Tools.Profiles in '..\..\reference-platform\dstu3\FHIR.Tools.Profiles.pas',
  FHIR.Support.Lock in '..\..\reference-platform\support\FHIR.Support.Lock.pas',
  AdvZipReaders in '..\..\reference-platform\support\AdvZipReaders.pas',
  FHIR.Tools.PathEngine in '..\..\reference-platform\dstu3\FHIR.Tools.PathEngine.pas',
  SystemSupport in '..\..\reference-platform\support\SystemSupport.pas',
  ThreadSupport in '..\..\reference-platform\support\ThreadSupport.pas',
  FHIR.Client.SmartUtilities in '..\..\reference-platform\client\FHIR.Client.SmartUtilities.pas',
  MarkdownProcessor in '..\..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownCommonMark in '..\..\..\markdown\source\MarkdownCommonMark.pas',
  HashSupport in '..\..\reference-platform\support\HashSupport.pas',
  FHIR.CdsHooks.Utilities in '..\..\reference-platform\support\FHIR.CdsHooks.Utilities.pas',
  {$IFNDEF OSX}
  AfsStreamManagers in '..\..\reference-platform\support\AfsStreamManagers.pas',
  AfsVolumes in '..\..\reference-platform\support\AfsVolumes.pas',
  AfsResourceVolumes in '..\..\reference-platform\support\AfsResourceVolumes.pas',
  FHIR.Support.WInInet in '..\..\reference-platform\support\FHIR.Support.WInInet.pas',
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
