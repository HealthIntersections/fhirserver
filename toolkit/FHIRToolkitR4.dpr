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
program FHIRToolkitR4;

uses
  FastMM4 in '..\dependencies\FMM\FastMM4.pas',
  SysUtils,
  IdSSLOpenSSLHeaders,
  FMX.Forms,
  FHIR.Version.Client in '..\library\version\FHIR.Version.Client.pas',
  FHIR.Support.Osx in '..\library\support\FHIR.Support.Osx.pas',
  FHIR.Support.Base in '..\library\support\FHIR.Support.Base.pas',
  FHIR.Support.Stream in '..\library\support\FHIR.Support.Stream.pas',
  FHIR.Support.Collections in '..\library\support\FHIR.Support.Collections.pas',
  FHIR.Support.Utilities in '..\library\support\FHIR.Support.Utilities.pas',
  FHIR.Support.Json in '..\library\support\FHIR.Support.Json.pas',
  FHIR.Version.Parser in '..\library\version\FHIR.Version.Parser.pas',
  FHIR.R4.Xml in '..\library\r4\FHIR.R4.Xml.pas',
  FHIR.Base.Parser in '..\library\base\FHIR.Base.Parser.pas',
  FHIR.Support.MXml in '..\library\support\FHIR.Support.MXml.pas',
  FHIR.Support.Turtle in '..\library\support\FHIR.Support.Turtle.pas',
  FHIR.Base.Objects in '..\library\base\FHIR.Base.Objects.pas',
  FHIR.R4.Utilities in '..\library\r4\FHIR.R4.Utilities.pas',
  FHIR.Web.Parsers in '..\library\web\FHIR.Web.Parsers.pas',
  FHIR.Web.Fetcher in '..\library\web\FHIR.Web.Fetcher.pas',
  FHIR.R4.Context in '..\library\r4\FHIR.R4.Context.pas',
  FHIR.R4.Types in '..\library\r4\FHIR.R4.Types.pas',
  FHIR.R4.Resources in '..\library\r4\FHIR.R4.Resources.pas',
  FHIR.Base.Scim in '..\library\base\FHIR.Base.Scim.pas',
  FHIR.R4.Constants in '..\library\r4\FHIR.R4.Constants.pas',
  FHIR.R4.Tags in '..\library\r4\FHIR.R4.Tags.pas',
  FHIR.Base.Lang in '..\library\base\FHIR.Base.Lang.pas',
  FHIR.Base.Xhtml in '..\library\base\FHIR.Base.Xhtml.pas',
  FHIR.R4.Json in '..\library\r4\FHIR.R4.Json.pas',
  FHIR.R4.Turtle in '..\library\r4\FHIR.R4.Turtle.pas',
  FHIR.R4.ElementModel in '..\library\r4\FHIR.R4.ElementModel.pas',
  FHIR.R4.Profiles in '..\library\r4\FHIR.R4.Profiles.pas',
  FHIR.R4.PathEngine in '..\library\r4\FHIR.R4.PathEngine.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  FHIR.CdsHooks.Utilities in '..\library\cdshooks\FHIR.CdsHooks.Utilities.pas',
  FastMM4Messages in '..\dependencies\FMM\FastMM4Messages.pas',
  OrganizationChooser in 'OrganizationChooser.pas' {OrganizationSelectionForm},
  FHIRToolkitForm in 'FHIRToolkitForm.pas' {MasterToolsForm},
  ServerForm in 'ServerForm.pas' {ServerFrameForm: TFrame},
  BaseFrame in 'BaseFrame.pas',
  ValueSetEditor in 'ValueSetEditor.pas' {ValueSetEditorFrame: TFrame},
  SearchParameterCombinationEditor in 'SearchParameterCombinationEditor.pas' {SearchParameterCombinationEditorForm},
  ListSelector in 'ListSelector.pas' {ListSelectorForm},
  FHIR.R4.IndexInfo in '..\library\r4\FHIR.R4.IndexInfo.pas',
  FHIR.Tools.Indexing in '..\library\tools\FHIR.Tools.Indexing.pas',
  AddRestResourceDialog in 'AddRestResourceDialog.pas' {AddRestResourceForm},
  SourceViewer in 'SourceViewer.pas' {SourceViewerForm},
  BaseResourceFrame in 'BaseResourceFrame.pas',
  BaseServerFrame in 'BaseServerFrame.pas',
  CapabilityStatementEditor in 'CapabilityStatementEditor.pas' {CapabilityStatementEditorFrame: TFrame},
  ValuesetExpansion in 'ValuesetExpansion.pas' {ValuesetExpansionForm},
  HelpContexts in 'HelpContexts.pas',
  ProcessForm in 'ProcessForm.pas' {ProcessingForm},
  SettingsDialog in 'SettingsDialog.pas' {FHIR.Npp.Settings},
  AboutDialog in 'AboutDialog.pas' {AboutForm},
  FHIR.Ui.OSX in '..\library\ui\FHIR.Ui.OSX.pas',
  ValuesetSelectDialog in 'ValuesetSelectDialog.pas' {ValuesetSelectForm},
  MemoEditorDialog in 'MemoEditorDialog.pas' {MemoEditorForm},
  FHIR.Client.Registry in '..\library\client\FHIR.Client.Registry.pas',
  FHIR.Client.ServerDialogFMX in '..\library\client\FHIR.Client.ServerDialogFMX.pas' {EditRegisteredServerForm},
  ToolkitSettings in 'ToolkitSettings.pas',
  CodeSystemEditor in 'CodeSystemEditor.pas' {CodeSystemEditorFrame: TFrame},
  CodeSystemConceptDialog in 'CodeSystemConceptDialog.pas' {CodeSystemConceptForm},
  ResourceEditingSupport in 'ResourceEditingSupport.pas',
  ToolKitUtilities in 'ToolKitUtilities.pas',
  UpgradeNeededDialog in 'UpgradeNeededDialog.pas' {UpgradeNeededForm},
  QuestionnaireEditor in 'QuestionnaireEditor.pas' {QuestionnaireEditorFrame: TFrame},
  FHIR.Web.HtmlGen in '..\library\web\FHIR.Web.HtmlGen.pas',
  QuestionnaireItemDialog in 'QuestionnaireItemDialog.pas' {QuestionnaireItemForm},
  ProviderDirectoryForm in 'ProviderDirectoryForm.pas' {ProviderDirectoryFrame},
  FHIR.Client.ClientDialogFMX in '..\library\client\FHIR.Client.ClientDialogFMX.pas' {RegisterClientForm},
  QuestionnaireItemPanel in 'QuestionnaireItemPanel.pas',
  QuestionnairePanel in 'QuestionnairePanel.pas',
  VitalSignsGeneratorDialog in 'VitalSignsGeneratorDialog.pas' {VitalSignsGeneratorForm},
  TranslationsEditorDialog in 'TranslationsEditorDialog.pas' {TranslationsEditorForm},
  ResourceLanguageDialog in 'ResourceLanguageDialog.pas' {ResourceLanguageForm},
  AddRestOperationDialog in 'AddRestOperationDialog.pas' {AddRestOperationForm},
  RegistryForm in 'RegistryForm.pas' {RegistryFrame: TFrame},
  FHIR.Ui.ComboFMX in '..\library\ui\FHIR.Ui.ComboFMX.pas',
  PatientHomeForm in 'PatientHomeForm.pas' {PatientHomeFrame: TFrame},
  FHIR.Support.Signatures in '..\library\support\FHIR.Support.Signatures.pas',
  DocumentGenerationForm in 'DocumentGenerationForm.pas' {DocumentGeneratorForm},
  LibraryEditor in 'LibraryEditor.pas' {LibraryEditorFrame: TFrame},
  FHIR.Ucum.IFace in '..\library\ucum\FHIR.Ucum.IFace.pas',
  FHIR.R4.PathNode in '..\library\r4\FHIR.R4.PathNode.pas',
  FHIR.Support.Logging in '..\library\support\FHIR.Support.Logging.pas',
  FHIR.R4.Questionnaire2 in '..\library\r4\FHIR.R4.Questionnaire2.pas',
  FHIR.R4.Base in '..\library\r4\FHIR.R4.Base.pas',
  FHIR.R4.ParserBase in '..\library\r4\FHIR.R4.ParserBase.pas',
  FHIR.R4.Parser in '..\library\r4\FHIR.R4.Parser.pas',
  FHIR.Client.Base in '..\library\client\FHIR.Client.Base.pas',
  FHIR.Client.HTTP in '..\library\client\FHIR.Client.HTTP.pas',
  FHIR.Client.Threaded in '..\library\client\FHIR.Client.Threaded.pas',
  FHIR.R4.Client in '..\library\r4\FHIR.R4.Client.pas',
  FHIR.Support.Xml in '..\library\support\FHIR.Support.Xml.pas',
  FHIR.Web.WinInet in '..\library\web\FHIR.Web.WinInet.pas',
  FHIR.Support.Threads in '..\library\support\FHIR.Support.Threads.pas',
  FHIR.Support.Certs in '..\library\support\FHIR.Support.Certs.pas',
  FHIR.Web.GraphQL in '..\library\web\FHIR.Web.GraphQL.pas',
  BulkDataForm in 'BulkDataForm.pas' {BulkDataDialog},
  UsageContextForm in 'UsageContextForm.pas' {UsageContextDialog},
  FHIR.Base.Factory in '..\library\base\FHIR.Base.Factory.pas',
  FHIR.Base.Validator in '..\library\base\FHIR.Base.Validator.pas',
  FHIR.Base.Common in '..\library\base\FHIR.Base.Common.pas',
  FHIR.Base.Narrative in '..\library\base\FHIR.Base.Narrative.pas',
  FHIR.Base.PathEngine in '..\library\base\FHIR.Base.PathEngine.pas',
  FHIR.R4.Common in '..\library\r4\FHIR.R4.Common.pas',
  FHIR.R4.Factory in '..\library\r4\FHIR.R4.Factory.pas',
  FHIR.R4.Narrative in '..\library\r4\FHIR.R4.Narrative.pas',
  FHIR.R4.Validator in '..\library\r4\FHIR.R4.Validator.pas',
  FHIR.Client.Async in '..\library\client\FHIR.Client.Async.pas',
  FHIR.Cache.PackageManager in '..\library\cache\FHIR.Cache.PackageManager.pas',
  TransformationFrame in 'TransformationFrame.pas' {TransformationEngineFrame: TFrame},
  ValidationFrame in 'ValidationFrame.pas' {ValidationEngineFrame: TFrame},
  PackageManagerFrame in 'PackageManagerFrame.pas' {PackageManagerFrame: TFrame},
  FHIR.Tools.ValidationWrapper in '..\library\tools\FHIR.Tools.ValidationWrapper.pas',
  PackageBrowser in 'PackageBrowser.pas' {PackageFinderForm},
  fhir.support.fpc in '..\library\support\fhir.support.fpc.pas',
  FHIR.Base.Utilities in '..\library\base\FHIR.Base.Utilities.pas',
  FHIR.Smart.Utilities in '..\library\smart\FHIR.Smart.Utilities.pas',
  FHIR.Smart.Login in '..\library\smart\FHIR.Smart.Login.pas',
  FHIR.R4.Operations in '..\library\r4\FHIR.R4.Operations.pas',
  FHIR.R4.OpBase in '..\library\r4\FHIR.R4.OpBase.pas',
  OsxPopupmenuWorkaround in 'OsxPopupmenuWorkaround.pas' {PopupMenuWorkaroundForm},
  FHIR.R4.AuthMap in '..\library\r4\FHIR.R4.AuthMap.pas',
  IGPublisher in 'IGPublisher.pas' {IGPublishForm},
  FDownloadForm in 'FDownloadForm.pas' {IGSettingsForm},
  ImplementationGuideEditor in 'ImplementationGuideEditor.pas' {ImplementationGuideEditorFrame: TFrame},
  FHIR.Support.Lang in '..\library\support\FHIR.Support.Lang.pas',
  QuestionnaireContextDialog in 'QuestionnaireContextDialog.pas' {QuestionnaireContextForm},
  FHIR.Ui.Graph in '..\library\ui\FHIR.Ui.Graph.pas',
  FHIR.Tools.ObsGraph in '..\library\tools\FHIR.Tools.ObsGraph.pas',
  DiffEngineFrame in 'DiffEngineFrame.pas' {DiffEngineEngineFrame: TFrame},
  FHIR.Tools.DiffEngine in '..\library\tools\FHIR.Tools.DiffEngine.pas',
  Import2html in 'Import2html.pas' {ContentImport},
  ExampleScenarioEditor in 'ExampleScenarioEditor.pas' {exampleScenarioEditorFrame: TFrame},
  ScenarioRendering in 'ScenarioRendering.pas' {ESPublishForm},
  FHIR.Cda.Narrative in '..\library\cda\FHIR.Cda.Narrative.pas',
  FHIR.Cda.Types in '..\library\cda\FHIR.Cda.Types.pas',
  FHIR.Cda.Base in '..\library\cda\FHIR.Cda.Base.pas',
  FHIR.Base.OIDs in '..\library\base\FHIR.Base.OIDs.pas',
  FHIR.Base.ElementModel in '..\library\base\FHIR.Base.ElementModel.pas',
  ResourceHistoryDialog in 'ResourceHistoryDialog.pas' {ResourceHistoryForm},
  MarkdownHTMLEntities in '..\..\markdown\source\MarkdownHTMLEntities.pas',
  FHIR.Ui.Fmx in '..\library\ui\FHIR.Ui.Fmx.pas',
  ProjectFilesDialog in 'ProjectFilesDialog.pas' {ProjectDialog},
  UTGMgmtFrame in 'UTGMgmtFrame.pas' {UTGManagementFrame: TFrame},
  uGitForDelphi in '..\dependencies\git\uGitForDelphi.pas',
  FHIR.Web.Git in '..\library\web\FHIR.Web.Git.pas',
  FHIR.R4.Organiser in '..\library\r4\FHIR.R4.Organiser.pas',
  PublisherHome in '..\utilities\publisher\PublisherHome.pas' {PublisherForm},
  FHIR.Support.Shell in '..\library\support\FHIR.Support.Shell.pas';

{$R *.res}

begin
  {$IFDEF WINDOWS}
  IdOpenSSLSetLibPath(extractFilePath(paramstr(0)));
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMasterToolsForm, MasterToolsForm);
  Application.CreateForm(TResourceHistoryForm, ResourceHistoryForm);
  Application.CreateForm(TProjectDialog, ProjectDialog);
  Application.Run;
end.
