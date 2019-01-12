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

program FHIRToolkitR3;

uses
  FastMM4 in '..\dependencies\FMM\FastMM4.pas',
  FastMM4Messages in '..\dependencies\FMM\FastMM4Messages.pas',
  SysUtils,
  IdSSLOpenSSLHeaders,
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  FHIR.Support.Osx in '..\library\support\FHIR.Support.Osx.pas',
  FHIR.Support.Base in '..\library\support\FHIR.Support.Base.pas',
  FHIR.Support.Stream in '..\library\support\FHIR.Support.Stream.pas',
  FHIR.Support.Collections in '..\library\support\FHIR.Support.Collections.pas',
  FHIR.Support.Utilities in '..\library\support\FHIR.Support.Utilities.pas',
  FHIR.Support.MXml in '..\library\support\FHIR.Support.MXml.pas',
  FHIR.Support.Turtle in '..\library\support\FHIR.Support.Turtle.pas',
  FHIR.Support.Json in '..\library\support\FHIR.Support.Json.pas',
  FHIR.Support.Signatures in '..\library\support\FHIR.Support.Signatures.pas',
  FHIR.Support.Logging in '..\library\support\FHIR.Support.Logging.pas',
  FHIR.Support.Xml in '..\library\support\FHIR.Support.Xml.pas',
  FHIR.Support.Certs in '..\library\support\FHIR.Support.Certs.pas',
  fhir.support.fpc in '..\library\support\fhir.support.fpc.pas',
  FHIR.Support.Threads in '..\library\support\FHIR.Support.Threads.pas',
  FHIR.Web.Parsers in '..\library\web\FHIR.Web.Parsers.pas',
  FHIR.Web.Fetcher in '..\library\web\FHIR.Web.Fetcher.pas',
  FHIR.Web.WinInet in '..\library\web\FHIR.Web.WinInet.pas',
  FHIR.Web.HtmlGen in '..\library\web\FHIR.Web.HtmlGen.pas',
  FHIR.Cache.PackageManager in '..\library\cache\FHIR.Cache.PackageManager.pas',
  FHIR.Base.Parser in '..\library\base\FHIR.Base.Parser.pas',
  FHIR.Base.Objects in '..\library\base\FHIR.Base.Objects.pas',
  FHIR.Base.Scim in '..\library\base\FHIR.Base.Scim.pas',
  FHIR.Base.Lang in '..\library\base\FHIR.Base.Lang.pas',
  FHIR.Base.Xhtml in '..\library\base\FHIR.Base.Xhtml.pas',
  FHIR.Base.Factory in '..\library\base\FHIR.Base.Factory.pas',
  FHIR.Base.Validator in '..\library\base\FHIR.Base.Validator.pas',
  FHIR.Base.Common in '..\library\base\FHIR.Base.Common.pas',
  FHIR.Base.PathEngine in '..\library\base\FHIR.Base.PathEngine.pas',
  FHIR.Base.Narrative in '..\library\base\FHIR.Base.Narrative.pas',
  FHIR.Base.Utilities in '..\library\base\FHIR.Base.Utilities.pas',
  FHIR.Client.Base in '..\library\client\FHIR.Client.Base.pas',
  FHIR.Ucum.IFace in '..\library\ucum\FHIR.Ucum.IFace.pas',
  FHIR.R3.Xml in '..\library\r3\FHIR.R3.Xml.pas',
  FHIR.R3.Utilities in '..\library\r3\FHIR.R3.Utilities.pas',
  FHIR.R3.Context in '..\library\r3\FHIR.R3.Context.pas',
  FHIR.R3.Types in '..\library\r3\FHIR.R3.Types.pas',
  FHIR.R3.Resources in '..\library\r3\FHIR.R3.Resources.pas',
  FHIR.R3.Constants in '..\library\r3\FHIR.R3.Constants.pas',
  FHIR.R3.Tags in '..\library\r3\FHIR.R3.Tags.pas',
  FHIR.R3.Json in '..\library\r3\FHIR.R3.Json.pas',
  FHIR.R3.Turtle in '..\library\r3\FHIR.R3.Turtle.pas',
  FHIR.R3.ElementModel in '..\library\r3\FHIR.R3.ElementModel.pas',
  FHIR.R3.Profiles in '..\library\r3\FHIR.R3.Profiles.pas',
  FHIR.R3.PathEngine in '..\library\r3\FHIR.R3.PathEngine.pas',
  FHIR.R3.IndexInfo in '..\library\r3\FHIR.R3.IndexInfo.pas',
  FHIR.R3.PathNode in '..\library\r3\FHIR.R3.PathNode.pas',
  FHIR.R3.Questionnaire2 in '..\library\r3\FHIR.R3.Questionnaire2.pas',
  FHIR.R3.Base in '..\library\r3\FHIR.R3.Base.pas',
  FHIR.R3.ParserBase in '..\library\r3\FHIR.R3.ParserBase.pas',
  FHIR.R3.Parser in '..\library\r3\FHIR.R3.Parser.pas',
  FHIR.R3.Client in '..\library\r3\FHIR.R3.Client.pas',
  FHIR.R3.Common in '..\library\r3\FHIR.R3.Common.pas',
  FHIR.R3.Factory in '..\library\r3\FHIR.R3.Factory.pas',
  FHIR.R3.Narrative in '..\library\r3\FHIR.R3.Narrative.pas',
  FHIR.R3.Validator in '..\library\r3\FHIR.R3.Validator.pas',
  FHIR.R3.Operations in '..\library\r3\FHIR.R3.Operations.pas',
  FHIR.R3.OpBase in '..\library\r3\FHIR.R3.OpBase.pas',
  FHIR.R3.AuthMap in '..\library\r3\FHIR.R3.AuthMap.pas',
  FHIR.Client.Threaded in '..\library\client\FHIR.Client.Threaded.pas',
  FHIR.Client.HTTP in '..\library\client\FHIR.Client.HTTP.pas',
  FHIR.Smart.Utilities in '..\library\smart\FHIR.Smart.Utilities.pas',
  FHIR.Smart.Login in '..\library\smart\FHIR.Smart.Login.pas',
  FHIR.Tools.Indexing in '..\library\tools\FHIR.Tools.Indexing.pas',
  FHIR.Tools.DiffEngine in '..\library\tools\FHIR.Tools.DiffEngine.pas',
  FHIR.Tools.ValidationWrapper in '..\library\tools\FHIR.Tools.ValidationWrapper.pas',
  FHIR.Version.Client in '..\library\version\FHIR.Version.Client.pas',
  FHIR.Version.Parser in '..\library\version\FHIR.Version.Parser.pas',
  FHIR.Client.Registry in '..\library\client\FHIR.Client.Registry.pas',
  FHIR.Client.ServerDialogFMX in '..\library\client\FHIR.Client.ServerDialogFMX.pas' {EditRegisteredServerForm},
  FHIR.Client.ClientDialogFMX in '..\library\client\FHIR.Client.ClientDialogFMX.pas' {RegisterClientForm},
  FHIR.CdsHooks.Utilities in '..\library\cdshooks\FHIR.CdsHooks.Utilities.pas',
  FHIR.Ui.OSX in '..\library\ui\FHIR.Ui.OSX.pas',
  FHIR.Ui.ComboFMX in '..\library\ui\FHIR.Ui.ComboFMX.pas',
  FHIR.Web.GraphQL in '..\library\web\FHIR.Web.GraphQL.pas',
  FHIR.Client.Async in '..\library\client\FHIR.Client.Async.pas',
  BaseFrame in 'BaseFrame.pas',
  BaseResourceFrame in 'BaseResourceFrame.pas',
  BaseServerFrame in 'BaseServerFrame.pas',
  FMX.Forms,
  ResourceEditingSupport in 'ResourceEditingSupport.pas',
  HelpContexts in 'HelpContexts.pas',
  ToolkitSettings in 'ToolkitSettings.pas',
  ToolKitUtilities in 'ToolKitUtilities.pas',
  QuestionnairePanel in 'QuestionnairePanel.pas',
  OrganizationChooser in 'OrganizationChooser.pas' {OrganizationSelectionForm},
  ProcessForm in 'ProcessForm.pas' {ProcessingForm},
  ListSelector in 'ListSelector.pas' {ListSelectorForm},
  SettingsDialog in 'SettingsDialog.pas' {FHIR.Npp.Settings},
  AboutDialog in 'AboutDialog.pas' {AboutForm},
  UpgradeNeededDialog in 'UpgradeNeededDialog.pas' {UpgradeNeededForm},
  MemoEditorDialog in 'MemoEditorDialog.pas' {MemoEditorForm},
  ResourceLanguageDialog in 'ResourceLanguageDialog.pas' {ResourceLanguageForm},
  CodeSystemConceptDialog in 'CodeSystemConceptDialog.pas' {CodeSystemConceptForm},
  SearchParameterEditor in 'SearchParameterEditor.pas' {SearchParameterEditorForm},
  AddRestResourceDialog in 'AddRestResourceDialog.pas' {AddRestResourceForm},
  AddRestOperationDialog in 'AddRestOperationDialog.pas' {AddRestOperationForm},
  TranslationsEditorDialog in 'TranslationsEditorDialog.pas' {TranslationsEditorForm},
  SourceViewer in 'SourceViewer.pas' {SourceViewerForm},
  QuestionnaireItemPanel in 'QuestionnaireItemPanel.pas',
  QuestionnaireItemDialog in 'QuestionnaireItemDialog.pas' {QuestionnaireItemForm},
  ValuesetSelectDialog in 'ValuesetSelectDialog.pas' {ValuesetSelectForm},
  PackageManagerFrame in 'PackageManagerFrame.pas' {PackageManagerFrame: TFrame},
  ValidationFrame in 'ValidationFrame.pas' {ValidationEngineFrame: TFrame},
  TransformationFrame in 'TransformationFrame.pas' {TransformationEngineFrame: TFrame},
  PackageBrowser in 'PackageBrowser.pas' {PackageFinderForm},
  ServerForm in 'ServerForm.pas' {ServerFrameForm: TFrame},
  FHIRToolkitForm in 'FHIRToolkitForm.pas' {MasterToolsForm},
  ValueSetEditor in 'ValueSetEditor.pas' {ValueSetEditorFrame: TFrame},
  CapabilityStatementEditor in 'CapabilityStatementEditor.pas' {CapabilityStatementEditorFrame: TFrame},
  ValuesetExpansion in 'ValuesetExpansion.pas' {ValuesetExpansionForm},
  CodeSystemEditor in 'CodeSystemEditor.pas' {CodeSystemEditorFrame: TFrame},
  QuestionnaireEditor in 'QuestionnaireEditor.pas' {QuestionnaireEditorFrame: TFrame},
  ProviderDirectoryForm in 'ProviderDirectoryForm.pas' {ProviderDirectoryFrame},
  VitalSignsGeneratorDialog in 'VitalSignsGeneratorDialog.pas' {VitalSignsGeneratorForm},
  RegistryForm in 'RegistryForm.pas' {RegistryFrame: TFrame},
  PatientHomeForm in 'PatientHomeForm.pas' {PatientHomeFrame: TFrame},
  DocumentGenerationForm in 'DocumentGenerationForm.pas' {DocumentGeneratorForm},
  LibraryEditor in 'LibraryEditor.pas' {LibraryEditorFrame: TFrame},
  FHIR.Ui.Graph in '..\library\ui\FHIR.Ui.Graph.pas',
  FHIR.Tools.ObsGraph in '..\library\tools\FHIR.Tools.ObsGraph.pas',
  FHIR.Base.OIDs in '..\library\base\FHIR.Base.OIDs.pas',
  FHIR.Base.ElementModel in '..\library\base\FHIR.Base.ElementModel.pas',
  MarkdownHTMLEntities in '..\..\markdown\source\MarkdownHTMLEntities.pas';

{$R *.res}

begin
  {$IFDEF WINDOWS}
  IdOpenSSLSetLibPath(extractFilePath(paramstr(0)));
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMasterToolsForm, MasterToolsForm);
  Application.Run;
end.
