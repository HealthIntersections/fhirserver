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
  FHIRBase in '..\reference-platform\dstu2\FHIRBase.pas',
  FHIRClient in '..\reference-platform\client\FHIRClient.pas',
  FHIRConstants in '..\reference-platform\dstu2\FHIRConstants.pas',
  FHIRDigitalSignatures in '..\reference-platform\dstu2\FHIRDigitalSignatures.pas',
  FHIRLang in '..\reference-platform\support\FHIRLang.pas',
  FHIRParser in '..\reference-platform\dstu2\FHIRParser.pas',
  FHIRParserBase in '..\reference-platform\dstu2\FHIRParserBase.pas',
  FHIRResources in '..\reference-platform\dstu2\FHIRResources.pas',
  FHIRSupport in '..\reference-platform\dstu2\FHIRSupport.pas',
  FHIRTags in '..\reference-platform\dstu2\FHIRTags.pas',
  FHIRTypes in '..\reference-platform\dstu2\FHIRTypes.pas',
  FHIRUtilities in '..\reference-platform\dstu2\FHIRUtilities.pas',
  FHIRWorkerContext in '..\reference-platform\dstu2\FHIRWorkerContext.pas',
  NarrativeGenerator in '..\reference-platform\dstu2\NarrativeGenerator.pas',
  SCIMObjects in '..\reference-platform\support\SCIMObjects.pas',
  DecimalSupport in '..\reference-platform\Support\DecimalSupport.pas',
  GUIDSupport in '..\reference-platform\Support\GUIDSupport.pas',
  StringSupport in '..\reference-platform\Support\StringSupport.pas',
  MathSupport in '..\reference-platform\Support\MathSupport.pas',
  AdvFactories in '..\reference-platform\Support\AdvFactories.pas',
  FileSupport in '..\reference-platform\Support\FileSupport.pas',
  MemorySupport in '..\reference-platform\Support\MemorySupport.pas',
  DateSupport in '..\reference-platform\Support\DateSupport.pas',
  ErrorSupport in '..\reference-platform\Support\ErrorSupport.pas',
  SystemSupport in '..\reference-platform\Support\SystemSupport.pas',
  ThreadSupport in '..\reference-platform\Support\ThreadSupport.pas',
  EncodeSupport in '..\reference-platform\Support\EncodeSupport.pas',
  AdvControllers in '..\reference-platform\Support\AdvControllers.pas',
  AdvPersistents in '..\reference-platform\Support\AdvPersistents.pas',
  AdvObjects in '..\reference-platform\Support\AdvObjects.pas',
  AdvExceptions in '..\reference-platform\Support\AdvExceptions.pas',
  AdvFilers in '..\reference-platform\Support\AdvFilers.pas',
  ColourSupport in '..\reference-platform\Support\ColourSupport.pas',
  CurrencySupport in '..\reference-platform\Support\CurrencySupport.pas',
  AdvPersistentLists in '..\reference-platform\Support\AdvPersistentLists.pas',
  AdvObjectLists in '..\reference-platform\Support\AdvObjectLists.pas',
  AdvItems in '..\reference-platform\Support\AdvItems.pas',
  AdvCollections in '..\reference-platform\Support\AdvCollections.pas',
  AdvIterators in '..\reference-platform\Support\AdvIterators.pas',
  AdvClassHashes in '..\reference-platform\Support\AdvClassHashes.pas',
  AdvHashes in '..\reference-platform\Support\AdvHashes.pas',
  HashSupport in '..\reference-platform\Support\HashSupport.pas',
  AdvStringHashes in '..\reference-platform\Support\AdvStringHashes.pas',
  AdvProfilers in '..\reference-platform\Support\AdvProfilers.pas',
  AdvStringIntegerMatches in '..\reference-platform\Support\AdvStringIntegerMatches.pas',
  AdvStreams in '..\reference-platform\Support\AdvStreams.pas',
  AdvParameters in '..\reference-platform\Support\AdvParameters.pas',
  AdvExclusiveCriticalSections in '..\reference-platform\Support\AdvExclusiveCriticalSections.pas',
  AdvThreads in '..\reference-platform\Support\AdvThreads.pas',
  AdvSignals in '..\reference-platform\Support\AdvSignals.pas',
  AdvSynchronizationRegistries in '..\reference-platform\Support\AdvSynchronizationRegistries.pas',
  AdvTimeControllers in '..\reference-platform\Support\AdvTimeControllers.pas',
  AdvIntegerMatches in '..\reference-platform\Support\AdvIntegerMatches.pas',
  AdvBuffers in '..\reference-platform\Support\AdvBuffers.pas',
  BytesSupport in '..\reference-platform\Support\BytesSupport.pas',
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
  ParseMap in '..\reference-platform\Support\ParseMap.pas',
  JWT in '..\reference-platform\Support\JWT.pas',
  AdvJSON in '..\reference-platform\Support\AdvJSON.pas',
  AdvVCLStreams in '..\reference-platform\Support\AdvVCLStreams.pas',
  AdvStringObjectMatches in '..\reference-platform\Support\AdvStringObjectMatches.pas',
  HMAC in '..\reference-platform\Support\HMAC.pas',
  libeay32 in '..\reference-platform\Support\libeay32.pas',
  FHIRSecurity in '..\reference-platform\support\FHIRSecurity.pas',
  MsXmlParser in '..\reference-platform\Support\MsXmlParser.pas',
  AdvMemories in '..\reference-platform\Support\AdvMemories.pas',
  XMLBuilder in '..\reference-platform\Support\XMLBuilder.pas',
  AdvWinInetClients in '..\reference-platform\Support\AdvWinInetClients.pas',
  MsXmlBuilder in '..\reference-platform\Support\MsXmlBuilder.pas',
  AdvXmlBuilders in '..\reference-platform\Support\AdvXmlBuilders.pas',
  AdvXMLFormatters in '..\reference-platform\Support\AdvXMLFormatters.pas',
  AdvXMLEntities in '..\reference-platform\Support\AdvXMLEntities.pas',
  AdvGenerics in '..\reference-platform\Support\AdvGenerics.pas',
  AfsResourceVolumes in '..\reference-platform\Support\AfsResourceVolumes.pas',
  AfsVolumes in '..\reference-platform\Support\AfsVolumes.pas',
  AfsStreamManagers in '..\reference-platform\Support\AfsStreamManagers.pas',
  AdvObjectMatches in '..\reference-platform\Support\AdvObjectMatches.pas',
  DigitalSignatures in '..\reference-platform\Support\DigitalSignatures.pas',
  XMLSupport in '..\reference-platform\Support\XMLSupport.pas',
  InternetFetcher in '..\reference-platform\Support\InternetFetcher.pas',
  kCritSct in '..\reference-platform\Support\kCritSct.pas',
  AdvZipReaders in '..\reference-platform\Support\AdvZipReaders.pas',
  AdvNameBuffers in '..\reference-platform\Support\AdvNameBuffers.pas',
  AdvZipDeclarations in '..\reference-platform\Support\AdvZipDeclarations.pas',
  AdvZipParts in '..\reference-platform\Support\AdvZipParts.pas',
  AdvZipUtilities in '..\reference-platform\Support\AdvZipUtilities.pas',
  AdvZipWorkers in '..\reference-platform\Support\AdvZipWorkers.pas',
  AltovaXMLLib_TLB in '..\reference-platform\Support\AltovaXMLLib_TLB.pas',
  ShellSupport in '..\reference-platform\Support\ShellSupport.pas',
  Vcl.Forms,
  LookAheadUnit in '..\Libraries\ui\LookAheadUnit.pas',
  DProgress in '..\Libraries\ui\DProgress.pas',
  ValueSetEditorExceptionForm in 'ValueSetEditorExceptionForm.pas' {ExceptionDialog},
  ValueSetEditorRegisterServerForm in 'ValueSetEditorRegisterServerForm.pas' {frmRegisterServer},
  MimeMessage in '..\reference-platform\Support\MimeMessage.pas',
  FHIRPath in '..\reference-platform\dstu2\FHIRPath.pas',
  FHIRProfileUtilities in '..\reference-platform\dstu2\FHIRProfileUtilities.pas',
  SmartOnFhirUtilities in '..\reference-platform\client\SmartOnFhirUtilities.pas',
  ConceptLookupFrm in 'ConceptLookupFrm.pas' {ConceptLookupForm},
  NewServerForm in 'NewServerForm.pas' {frmNewServer},
  QuestionnaireBuilder in '..\reference-platform\dstu2\QuestionnaireBuilder.pas',
  RDFUtilities in '..\reference-platform\support\RDFUtilities.pas',
  FHIRXhtml in '..\reference-platform\support\FHIRXhtml.pas',
  FHIRContext in '..\reference-platform\dstu2\FHIRContext.pas',
  FHIRMetaModel in '..\reference-platform\dstu2\FHIRMetaModel.pas',
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
