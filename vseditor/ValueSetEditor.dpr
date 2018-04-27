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
program ValueSetEditor;

{
todo:
   [10:12:17 AM] Lloyd McKenzie: "New rule" button is clunky.  Why can't I right-cllick on imports or includes, etc. to add something?
    General impression is: clunky and not intuitive.  You'll need to fix both of those things if you want to market it for $$
    [10:16:53 AM] Lloyd McKenzie: stuff that helps manage post-coordination a bit would be helpful
    [10:17:55 AM] Lloyd McKenzie: need a way to look up systems

    I can see people wanting to be able to look at stuff on the HL7 server, the Infoway server and perhaps some jurisdiction server in parallel

}
uses
  FastMM4 in '..\Libraries\FMM\FastMM4.pas',
  ValueSetEditorVCLForm in 'ValueSetEditorVCLForm.pas' {ValueSetEditorForm},
  ValueSetEditorCore in 'ValueSetEditorCore.pas',
  VirtualStringTreeComboBox in 'VirtualStringTreeComboBox.pas',
  ServerChooser in 'ServerChooser.pas' {ServerChooserForm},
  Unit1 in 'Unit1.pas' {Form1},
  ValueSetEditorAbout in 'ValueSetEditorAbout.pas' {ValueSetEditorAboutForm},
  ValueSetEditorWelcome in 'ValueSetEditorWelcome.pas' {ValueSetEditorWelcomeForm},
  ServerOperationForm in 'ServerOperationForm.pas' {Form2},
  FastMM4Messages in '..\Libraries\FMM\FastMM4Messages.pas',
  VirtualTrees.Actions in '..\Libraries\treeview\Source\VirtualTrees.Actions.pas',
  VirtualTrees.Classes in '..\Libraries\treeview\Source\VirtualTrees.Classes.pas',
  VirtualTrees.ClipBoard in '..\Libraries\treeview\Source\VirtualTrees.ClipBoard.pas',
  VirtualTrees.Export in '..\Libraries\treeview\Source\VirtualTrees.Export.pas',
  VirtualTrees in '..\Libraries\treeview\Source\VirtualTrees.pas',
  VirtualTrees.StyleHooks in '..\Libraries\treeview\Source\VirtualTrees.StyleHooks.pas',
  VirtualTrees.Utils in '..\Libraries\treeview\Source\VirtualTrees.Utils.pas',
  VirtualTrees.WorkerThread in '..\Libraries\treeview\Source\VirtualTrees.WorkerThread.pas',
  VTAccessibility in '..\Libraries\treeview\Source\VTAccessibility.pas',
  VTAccessibilityFactory in '..\Libraries\treeview\Source\VTAccessibilityFactory.pas',
  VTHeaderPopup in '..\Libraries\treeview\Source\VTHeaderPopup.pas',
  FHIR.Base.Objects in '..\reference-platform\dstu2\FHIR.Base.Objects.pas',
  FHIR.Tools.Client in '..\reference-platform\client\FHIR.Tools.Client.pas',
  FHIR.Tools.Constants in '..\reference-platform\dstu2\FHIR.Tools.Constants.pas',
  FHIR.Tools.Signatures in '..\reference-platform\dstu2\FHIR.Tools.Signatures.pas',
  FHIR.Base.Lang in '..\reference-platform\base\FHIR.Base.Lang.pas',
  FHIR.Tools.Parser in '..\reference-platform\dstu2\FHIR.Tools.Parser.pas',
  FHIR.Base.Parser in '..\reference-platform\dstu2\FHIR.Base.Parser.pas',
  FHIR.Tools.Resources in '..\reference-platform\dstu2\FHIR.Tools.Resources.pas',
  FHIR.Tools.Session in '..\reference-platform\dstu2\FHIR.Tools.Session.pas',
  FHIR.Tools.Tags in '..\reference-platform\dstu2\FHIR.Tools.Tags.pas',
  FHIR.Tools.Types in '..\reference-platform\dstu2\FHIR.Tools.Types.pas',
  FHIR.Tools.Utilities in '..\reference-platform\dstu2\FHIR.Tools.Utilities.pas',
  FHIRWorkerContext in '..\reference-platform\dstu2\FHIRWorkerContext.pas',
  FHIR.Tools.Narrative2 in '..\reference-platform\dstu2\FHIR.Tools.Narrative2.pas',
  FHIR.Base.Scim in '..\reference-platform\base\FHIR.Base.Scim.pas',
  FHIR.Support.Decimal in '..\reference-platform\Support\FHIR.Support.Decimal.pas',
  GUIDSupport in '..\reference-platform\Support\GUIDSupport.pas',
  FHIR.Support.Strings in '..\reference-platform\Support\FHIR.Support.Strings.pas',
  FHIR.Support.Math in '..\reference-platform\Support\FHIR.Support.Math.pas',
  FHIR.Support.Factory in '..\reference-platform\Support\FHIR.Support.Factory.pas',
  FHIR.Support.System in '..\reference-platform\Support\FHIR.Support.System.pas',
  MemorySupport in '..\reference-platform\Support\MemorySupport.pas',
  FHIR.Support.DateTime in '..\reference-platform\Support\FHIR.Support.DateTime.pas',
  ErrorSupport in '..\reference-platform\Support\ErrorSupport.pas',
  SystemSupport in '..\reference-platform\Support\SystemSupport.pas',
  ThreadSupport in '..\reference-platform\Support\ThreadSupport.pas',
  EncodeSupport in '..\reference-platform\Support\EncodeSupport.pas',
  AdvControllers in '..\reference-platform\Support\AdvControllers.pas',
  AdvPersistents in '..\reference-platform\Support\AdvPersistents.pas',
  FHIR.Support.Objects in '..\reference-platform\Support\FHIR.Support.Objects.pas',
  FHIR.Support.Exceptions in '..\reference-platform\Support\FHIR.Support.Exceptions.pas',
  FHIR.Support.Filers in '..\reference-platform\Support\FHIR.Support.Filers.pas',
  ColourSupport in '..\reference-platform\Support\ColourSupport.pas',
  CurrencySupport in '..\reference-platform\Support\CurrencySupport.pas',
  AdvPersistentLists in '..\reference-platform\Support\AdvPersistentLists.pas',
  AdvObjectLists in '..\reference-platform\Support\AdvObjectLists.pas',
  AdvItems in '..\reference-platform\Support\AdvItems.pas',
  FHIR.Support.Collections in '..\reference-platform\Support\FHIR.Support.Collections.pas',
  AdvIterators in '..\reference-platform\Support\AdvIterators.pas',
  AdvClassHashes in '..\reference-platform\Support\AdvClassHashes.pas',
  AdvHashes in '..\reference-platform\Support\AdvHashes.pas',
  HashSupport in '..\reference-platform\Support\HashSupport.pas',
  AdvStringHashes in '..\reference-platform\Support\AdvStringHashes.pas',
  AdvProfilers in '..\reference-platform\Support\AdvProfilers.pas',
  AdvStringIntegerMatches in '..\reference-platform\Support\AdvStringIntegerMatches.pas',
  FHIR.Support.Stream in '..\reference-platform\Support\FHIR.Support.Stream.pas',
  AdvParameters in '..\reference-platform\Support\AdvParameters.pas',
  AdvExclusiveCriticalSections in '..\reference-platform\Support\AdvExclusiveCriticalSections.pas',
  AdvThreads in '..\reference-platform\Support\AdvThreads.pas',
  AdvSignals in '..\reference-platform\Support\AdvSignals.pas',
  AdvSynchronizationRegistries in '..\reference-platform\Support\AdvSynchronizationRegistries.pas',
  AdvTimeControllers in '..\reference-platform\Support\AdvTimeControllers.pas',
  AdvIntegerMatches in '..\reference-platform\Support\AdvIntegerMatches.pas',
  AdvBuffers in '..\reference-platform\Support\AdvBuffers.pas',
  FHIR.Support.Binary in '..\reference-platform\Support\FHIR.Support.Binary.pas',
  AdvStringBuilders in '..\reference-platform\Support\AdvStringBuilders.pas',
  AdvFiles in '..\reference-platform\Support\AdvFiles.pas',
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
  DateAndTime in '..\reference-platform\Support\DateAndTime.pas',
  KDate in '..\reference-platform\Support\KDate.pas',
  HL7V2DateSupport in '..\reference-platform\Support\HL7V2DateSupport.pas',
  AdvNames in '..\reference-platform\Support\AdvNames.pas',
  AdvStringMatches in '..\reference-platform\Support\AdvStringMatches.pas',
  OIDSupport in '..\reference-platform\Support\OIDSupport.pas',
  TextUtilities in '..\reference-platform\Support\TextUtilities.pas',
  FHIR.Web.ParseMap in '..\reference-platform\Support\FHIR.Web.ParseMap.pas',
  JWT in '..\reference-platform\Support\JWT.pas',
  FHIR.Support.Json in '..\reference-platform\Support\FHIR.Support.Json.pas',
  AdvVCLStreams in '..\reference-platform\Support\AdvVCLStreams.pas',
  AdvStringObjectMatches in '..\reference-platform\Support\AdvStringObjectMatches.pas',
  HMAC in '..\reference-platform\Support\HMAC.pas',
  libeay32 in '..\reference-platform\Support\libeay32.pas',
  FHIR.Tools.Security in '..\reference-platform\tools\FHIR.Tools.Security.pas',
  MsXmlParser in '..\reference-platform\Support\MsXmlParser.pas',
  AdvMemories in '..\reference-platform\Support\AdvMemories.pas',
  FHIR.Xml.Builder in '..\reference-platform\Support\FHIR.Xml.Builder.pas',
  FHIR.Support.WInInet in '..\reference-platform\Support\FHIR.Support.WInInet.pas',
  MsXmlBuilder in '..\reference-platform\Support\MsXmlBuilder.pas',
  AdvXmlBuilders in '..\reference-platform\Support\AdvXmlBuilders.pas',
  AdvXMLFormatters in '..\reference-platform\Support\AdvXMLFormatters.pas',
  AdvXMLEntities in '..\reference-platform\Support\AdvXMLEntities.pas',
  FHIR.Support.Generics in '..\reference-platform\Support\FHIR.Support.Generics.pas',
  AfsResourceVolumes in '..\reference-platform\Support\AfsResourceVolumes.pas',
  AfsVolumes in '..\reference-platform\Support\AfsVolumes.pas',
  AfsStreamManagers in '..\reference-platform\Support\AfsStreamManagers.pas',
  AdvObjectMatches in '..\reference-platform\Support\AdvObjectMatches.pas',
  FHIR.Support.Signatures in '..\reference-platform\Support\FHIR.Support.Signatures.pas',
  FHIR.Xml.Base in '..\reference-platform\Support\FHIR.Xml.Base.pas',
  FHIR.Web.Fetcher in '..\reference-platform\Support\FHIR.Web.Fetcher.pas',
  FHIR.Support.Lock in '..\reference-platform\Support\FHIR.Support.Lock.pas',
  AdvZipReaders in '..\reference-platform\Support\AdvZipReaders.pas',
  AdvNameBuffers in '..\reference-platform\Support\AdvNameBuffers.pas',
  AdvZipDeclarations in '..\reference-platform\Support\AdvZipDeclarations.pas',
  AdvZipParts in '..\reference-platform\Support\AdvZipParts.pas',
  AdvZipUtilities in '..\reference-platform\Support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\reference-platform\Support\AdvZipWorkers.pas',
  AltovaXMLLib_TLB in '..\reference-platform\Support\AltovaXMLLib_TLB.pas',
  FHIR.Support.Shell in '..\reference-platform\Support\FHIR.Support.Shell.pas',
  Vcl.Forms,
  FHIR.Ui.Lookahead in '..\Libraries\ui\FHIR.Ui.Lookahead.pas',
  FHIR.Ui.Progress in '..\Libraries\ui\FHIR.Ui.Progress.pas',
  ValueSetEditorExceptionForm in 'ValueSetEditorExceptionForm.pas' {ExceptionDialog},
  ValueSetEditorRegisterServerForm in 'ValueSetEditorRegisterServerForm.pas' {frmRegisterServer},
  FHIR.Support.Mime in '..\reference-platform\Support\FHIR.Support.Mime.pas',
  FHIR.Tools.PathEngine in '..\reference-platform\dstu2\FHIR.Tools.PathEngine.pas',
  FHIR.Tools.Profiles in '..\reference-platform\dstu2\FHIR.Tools.Profiles.pas',
  FHIR.Client.SmartUtilities in '..\reference-platform\client\FHIR.Client.SmartUtilities.pas',
  ConceptLookupFrm in 'ConceptLookupFrm.pas' {ConceptLookupForm},
  NewServerForm in 'NewServerForm.pas' {frmNewServer},
  FHIR.Tools.Questionnaire in '..\reference-platform\dstu2\FHIR.Tools.Questionnaire.pas',
  FHIR.Web.Rdf in '..\reference-platform\support\FHIR.Web.Rdf.pas',
  FHIR.Base.Xhtml in '..\reference-platform\base\FHIR.Base.Xhtml.pas',
  FHIR.Tools.Context in '..\reference-platform\dstu2\FHIR.Tools.Context.pas',
  FHIR.Tools.ElementModel in '..\reference-platform\dstu2\FHIR.Tools.ElementModel.pas',
  JclSysUtils in '..\Libraries\jcl\JclSysUtils.pas',
  JclBase in '..\Libraries\jcl\JclBase.pas',
  JclResources in '..\Libraries\jcl\JclResources.pas',
  JclSynch in '..\Libraries\jcl\JclSynch.pas',
  JclWin32 in '..\Libraries\jcl\JclWin32.pas',
  JclLogic in '..\Libraries\jcl\JclLogic.pas',
  JclRegistry in '..\Libraries\jcl\JclRegistry.pas',
  JclStrings in '..\Libraries\jcl\JclStrings.pas',
  JclAnsiStrings in '..\Libraries\jcl\JclAnsiStrings.pas',
  JclStreams in '..\Libraries\jcl\JclStreams.pas',
  JclStringConversions in '..\Libraries\jcl\JclStringConversions.pas',
  JclCharsets in '..\Libraries\jcl\JclCharsets.pas',
  JclMath in '..\Libraries\jcl\JclMath.pas',
  Jcl8087 in '..\Libraries\jcl\Jcl8087.pas',
  JclConsole in '..\Libraries\jcl\JclConsole.pas',
  JclFileUtils in '..\Libraries\jcl\JclFileUtils.pas',
  JclShell in '..\Libraries\jcl\JclShell.pas',
  JclWideStrings in '..\Libraries\jcl\JclWideStrings.pas',
  JclUnicode in '..\Libraries\jcl\JclUnicode.pas',
  JclSysInfo in '..\Libraries\jcl\JclSysInfo.pas',
  Snmp in '..\Libraries\jcl\Snmp.pas',
  JclIniFiles in '..\Libraries\jcl\JclIniFiles.pas',
  JclSecurity in '..\Libraries\jcl\JclSecurity.pas',
  JclDateTime in '..\Libraries\jcl\JclDateTime.pas',
  JclMapi in '..\Libraries\jcl\JclMapi.pas',
  JclPeImage in '..\Libraries\jcl\JclPeImage.pas',
  JclUnitVersioning in '..\Libraries\jcl\JclUnitVersioning.pas',
  JclUnitVersioningProviders in '..\Libraries\jcl\JclUnitVersioningProviders.pas',
  JclDebug in '..\Libraries\jcl\JclDebug.pas',
  JclTD32 in '..\Libraries\jcl\JclTD32.pas',
  JclHookExcept in '..\Libraries\jcl\JclHookExcept.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TValueSetEditorForm, ValueSetEditorForm);
  Application.CreateForm(TServerChooserForm, ServerChooserForm);
  Application.CreateForm(TfrmRegisterServer, frmRegisterServer);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TValueSetEditorAboutForm, ValueSetEditorAboutForm);
  Application.CreateForm(TValueSetEditorWelcomeForm, ValueSetEditorWelcomeForm);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TConceptLookupForm, ConceptLookupForm);
  Application.CreateForm(TfrmNewServer, frmNewServer);
  Application.Run;
end.
