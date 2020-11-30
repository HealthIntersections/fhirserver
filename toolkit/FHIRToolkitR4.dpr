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
  {$IFNDEF MACOS}
  FastMM4 in '..\dependencies\FMM\FastMM4.pas',
  {$ENDIF }
  SysUtils,
  IdSSLOpenSSLHeaders,
  FMX.Forms,
  FHIR.Version.Client in '..\library\version\FHIR.Version.Client.pas',
  FHIR.Support.Osx in '..\library\support\FHIR.Support.Osx.pas',
  fsl_base in '..\library\support\fsl_base.pas',
  fsl_stream in '..\library\support\fsl_stream.pas',
  fsl_collections in '..\library\support\fsl_collections.pas',
  fsl_utilities in '..\library\support\fsl_utilities.pas',
  fsl_json in '..\library\support\fsl_json.pas',
  FHIR.Version.Parser in '..\library\version\FHIR.Version.Parser.pas',
  fhir4_xml in '..\library\r4\fhir4_xml.pas',
  fhir_parser in '..\library\base\fhir_parser.pas',
  fsl_xml in '..\library\support\fsl_xml.pas',
  fsl_turtle in '..\library\support\fsl_turtle.pas',
  fhir_objects in '..\library\base\fhir_objects.pas',
  fhir4_utilities in '..\library\r4\fhir4_utilities.pas',
  fsl_http in '..\library\web\fsl_http.pas',
  fsl_fetcher in '..\library\web\fsl_fetcher.pas',
  fhir4_context in '..\library\r4\fhir4_context.pas',
  fhir4_types in '..\library\r4\fhir4_types.pas',
  fhir4_resources in '..\library\r4\fhir4_resources.pas',
  fsl_scim in '..\library\base\fsl_scim.pas',
  fhir4_constants in '..\library\r4\fhir4_constants.pas',
  fhir4_tags in '..\library\r4\fhir4_tags.pas',
  FHIR.Base.Lang in '..\library\base\FHIR.Base.Lang.pas',
  fhir_xhtml in '..\library\base\fhir_xhtml.pas',
  fhir4_json in '..\library\r4\fhir4_json.pas',
  fhir4_turtle in '..\library\r4\fhir4_turtle.pas',
  fhir4_elementmodel in '..\library\r4\fhir4_elementmodel.pas',
  fhir4_profiles in '..\library\r4\fhir4_profiles.pas',
  fhir4_pathengine in '..\library\r4\fhir4_pathengine.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  fhir_cdshooks in '..\library\cdshooks\fhir_cdshooks.pas',
  FastMM4Messages in '..\dependencies\FMM\FastMM4Messages.pas',
  OrganizationChooser in 'OrganizationChooser.pas' {OrganizationSelectionForm},
  FHIRToolkitForm in 'FHIRToolkitForm.pas' {MasterToolsForm},
  ServerForm in 'ServerForm.pas' {ServerFrameForm: TFrame},
  BaseFrame in 'BaseFrame.pas',
  ValueSetEditor in 'ValueSetEditor.pas' {ValueSetEditorFrame: TFrame},
  SearchParameterCombinationEditor in 'SearchParameterCombinationEditor.pas' {SearchParameterCombinationEditorForm},
  ListSelector in 'ListSelector.pas' {ListSelectorForm},
  fhir4_indexinfo in '..\library\r4\fhir4_indexinfo.pas',
  fhir_indexing in '..\library\tools\fhir_indexing.pas',
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
  fhir_client_registry in '..\library\client\fhir_client_registry.pas',
  FHIR.Client.ServerDialogFMX in '..\library\client\FHIR.Client.ServerDialogFMX.pas' {EditRegisteredServerForm},
  ToolkitSettings in 'ToolkitSettings.pas',
  CodeSystemEditor in 'CodeSystemEditor.pas' {CodeSystemEditorFrame: TFrame},
  CodeSystemConceptDialog in 'CodeSystemConceptDialog.pas' {CodeSystemConceptForm},
  ResourceEditingSupport in 'ResourceEditingSupport.pas',
  ToolKitUtilities in 'ToolKitUtilities.pas',
  UpgradeNeededDialog in 'UpgradeNeededDialog.pas' {UpgradeNeededForm},
  QuestionnaireEditor in 'QuestionnaireEditor.pas' {QuestionnaireEditorFrame: TFrame},
  fhir_htmlgen in '..\library\web\fhir_htmlgen.pas',
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
  fsl_ucum in '..\library\ucum\fsl_ucum.pas',
  fhir4_pathnode in '..\library\r4\fhir4_pathnode.pas',
  fsl_logging in '..\library\support\fsl_logging.pas',
  fhir4_questionnaire2 in '..\library\r4\fhir4_questionnaire2.pas',
  fhir4_base in '..\library\r4\fhir4_base.pas',
  fhir4_parserBase in '..\library\r4\fhir4_parserBase.pas',
  fhir4_parser in '..\library\r4\fhir4_parser.pas',
  fhir_client in '..\library\client\fhir_client.pas',
  fhir_client_http in '..\library\client\fhir_client_http.pas',
  fhir_client_threaded in '..\library\client\fhir_client_threaded.pas',
  fhir4_client in '..\library\r4\fhir4_client.pas',
  fsl_xml in '..\library\support\fsl_xml.pas',
  fsl_wininet in '..\library\web\fsl_wininet.pas',
  fsl_threads in '..\library\support\fsl_threads.pas',
  FHIR.Support.Certs in '..\library\support\FHIR.Support.Certs.pas',
  fsl_graphql in '..\library\web\fsl_graphql.pas',
  BulkDataForm in 'BulkDataForm.pas' {BulkDataDialog},
  UsageContextForm in 'UsageContextForm.pas' {UsageContextDialog},
  fhir_factory in '..\library\base\fhir_factory.pas',
  fhir_validator in '..\library\base\fhir_validator.pas',
  fhir_common in '..\library\base\fhir_common.pas',
  fhir_narrative in '..\library\base\fhir_narrative.pas',
  fhir_pathengine in '..\library\base\fhir_pathengine.pas',
  fhir4_common in '..\library\r4\fhir4_common.pas',
  fhir4_factory in '..\library\r4\fhir4_factory.pas',
  fhir4_narrative in '..\library\r4\fhir4_narrative.pas',
  fhir4_validator in '..\library\r4\fhir4_validator.pas',
  fhir_client_async in '..\library\client\fhir_client_async.pas',
  fsl_npm_cache in '..\library\npm\fsl_npm_cache.pas',
  TransformationFrame in 'TransformationFrame.pas' {TransformationEngineFrame: TFrame},
  ValidationFrame in 'ValidationFrame.pas' {ValidationEngineFrame: TFrame},
  PackageManagerFrame in 'PackageManagerFrame.pas' {PackageManagerFrame: TFrame},
  FHIR.Tools.ValidationWrapper in '..\library\tools\FHIR.Tools.ValidationWrapper.pas',
  PackageBrowser in 'PackageBrowser.pas' {PackageFinderForm},
  fsl_fpc in '..\library\support\fsl_fpc.pas',
  fhir_utilities in '..\library\base\fhir_utilities.pas',
  fhir_oauth in '..\library\smart\fhir_oauth.pas',
  FHIR.Smart.Login in '..\library\smart\FHIR.Smart.Login.pas',
  fhir4_operations in '..\library\r4\fhir4_operations.pas',
  fhir4_opbase in '..\library\r4\fhir4_opbase.pas',
  OsxPopupmenuWorkaround in 'OsxPopupmenuWorkaround.pas' {PopupMenuWorkaroundForm},
  fhir4_authmap in '..\library\r4\fhir4_authmap.pas',
  {$IFNDEF OSX}
  IGPublisher in 'IGPublisher.pas' {IGPublishForm},
  {$ENDIF }
  FDownloadForm in 'FDownloadForm.pas' {IGSettingsForm},
  ImplementationGuideEditor in 'ImplementationGuideEditor.pas' {ImplementationGuideEditorFrame: TFrame},
  FHIR.Support.Lang in '..\library\support\FHIR.Support.Lang.pas',
  QuestionnaireContextDialog in 'QuestionnaireContextDialog.pas' {QuestionnaireContextForm},
  FHIR.Ui.Graph in '..\library\ui\FHIR.Ui.Graph.pas',
  FHIR.Tools.ObsGraph in '..\library\tools\FHIR.Tools.ObsGraph.pas',
  DiffEngineFrame in 'DiffEngineFrame.pas' {DiffEngineEngineFrame: TFrame},
  fhir_diff in '..\library\tools\fhir_diff.pas',
  Import2html in 'Import2html.pas' {ContentImport},
  ExampleScenarioEditor in 'ExampleScenarioEditor.pas' {exampleScenarioEditorFrame: TFrame},
  ScenarioRendering in 'ScenarioRendering.pas' {ESPublishForm},
  cda_narrative in '..\library\cda\cda_narrative.pas',
  cda_types in '..\library\cda\cda_types.pas',
  cda_base in '..\library\cda\cda_base.pas',
  fhir_oids in '..\library\base\fhir_oids.pas',
  fhir_elementmodel in '..\library\base\fhir_elementmodel.pas',
  ResourceHistoryDialog in 'ResourceHistoryDialog.pas' {ResourceHistoryForm},
  MarkdownHTMLEntities in '..\..\markdown\source\MarkdownHTMLEntities.pas',
  FHIR.Ui.Fmx in '..\library\ui\FHIR.Ui.Fmx.pas',
  ProjectFilesDialog in 'ProjectFilesDialog.pas' {ProjectDialog},
  UTGMgmtFrame in 'UTGMgmtFrame.pas' {UTGManagementFrame: TFrame},
  {$IFNDEF OSX}
  uGitForDelphi in '..\dependencies\git\uGitForDelphi.pas',
  FHIR.Web.Git in '..\library\web\FHIR.Web.Git.pas',
  fsl_shell in '..\library\support\fsl_shell.pas',
  {$ENDIF }
  fhir4_organiser in '..\library\r4\fhir4_organiser.pas',
  BaseFileFrame in 'BaseFileFrame.pas',
  PackageEditorFrame in 'PackageEditorFrame.pas' {PackageEditorFrame: TFrame},
  fsl_npm in '..\library\npm\fsl_npm.pas',
  NamingSystemEditor in 'NamingSystemEditor.pas' {NamingSystemEditorFrame: TFrame},
  FHIR.Npm.Client in '..\library\npm\FHIR.Npm.Client.pas',
  fhir4_resources_canonical in '..\library\r4\fhir4_resources_canonical.pas',
  fhir4_resources_base in '..\library\r4\fhir4_resources_base.pas',
  fhir4_resources_admin in '..\library\r4\fhir4_resources_admin.pas',
  fhir4_resources_other in '..\library\r4\fhir4_resources_other.pas',
  fhir4_resources_medications in '..\library\r4\fhir4_resources_medications.pas',
  fhir4_resources_financial in '..\library\r4\fhir4_resources_financial.pas',
  fhir4_resources_clinical in '..\library\r4\fhir4_resources_clinical.pas',
  MarkdownUnicodeUtils in '..\..\markdown\source\MarkdownUnicodeUtils.pas';

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
