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
  {$IFNDEF MACOS}
  FastMM4 in '..\dependencies\FMM\FastMM4.pas',
  FastMM4Messages in '..\dependencies\FMM\FastMM4Messages.pas',
  {$ENDIF }
  SysUtils,
  IdSSLOpenSSLHeaders,
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  fsl_base in '..\library\support\fsl_base.pas',
  fsl_stream in '..\library\support\fsl_stream.pas',
  fsl_collections in '..\library\support\fsl_collections.pas',
  fsl_utilities in '..\library\support\fsl_utilities.pas',
  fsl_xml in '..\library\support\fsl_xml.pas',
  fsl_turtle in '..\library\support\fsl_turtle.pas',
  fsl_json in '..\library\support\fsl_json.pas',
  fsl_crypto in '..\library\web\fsl_crypto.pas',
  fsl_logging in '..\library\support\fsl_logging.pas',
  fsl_xml in '..\library\support\fsl_xml.pas',
  FHIR.Support.Certs in '..\library\support\FHIR.Support.Certs.pas',
  fsl_fpc in '..\library\support\fsl_fpc.pas',
  fsl_threads in '..\library\support\fsl_threads.pas',
  fsl_http in '..\library\web\fsl_http.pas',
  fsl_fetcher in '..\library\web\fsl_fetcher.pas',
  fsl_wininet in '..\library\web\fsl_wininet.pas',
  fhir_htmlgen in '..\library\web\fhir_htmlgen.pas',
  fsl_npm_cache in '..\library\npm\fsl_npm_cache.pas',
  fhir_parser in '..\library\base\fhir_parser.pas',
  fhir_objects in '..\library\base\fhir_objects.pas',
  fsl_scim in '..\library\base\fsl_scim.pas',
  FHIR.Base.Lang in '..\library\base\FHIR.Base.Lang.pas',
  fhir_xhtml in '..\library\base\fhir_xhtml.pas',
  fhir_factory in '..\library\base\fhir_factory.pas',
  fhir_validator in '..\library\base\fhir_validator.pas',
  fhir_common in '..\library\base\fhir_common.pas',
  fhir_pathengine in '..\library\base\fhir_pathengine.pas',
  fhir_narrative in '..\library\base\fhir_narrative.pas',
  fhir_utilities in '..\library\base\fhir_utilities.pas',
  fhir_client in '..\library\client\fhir_client.pas',
  fhir_ucum in '..\library\ucum\fhir_ucum.pas',
  fhir3_xml in '..\library\r3\fhir3_xml.pas',
  fhir3_utilities in '..\library\r3\fhir3_utilities.pas',
  fhir3_context in '..\library\r3\fhir3_context.pas',
  fhir3_types in '..\library\r3\fhir3_types.pas',
  fhir3_resources in '..\library\r3\fhir3_resources.pas',
  fhir3_constants in '..\library\r3\fhir3_constants.pas',
  fhir3_tags in '..\library\r3\fhir3_tags.pas',
  fhir3_json in '..\library\r3\fhir3_json.pas',
  fhir3_turtle in '..\library\r3\fhir3_turtle.pas',
  fhir3_elementmodel in '..\library\r3\fhir3_elementmodel.pas',
  fhir3_profiles in '..\library\r3\fhir3_profiles.pas',
  fhir3_pathengine in '..\library\r3\fhir3_pathengine.pas',
  fhir3_indexinfo in '..\library\r3\fhir3_indexinfo.pas',
  fhir3_pathnode in '..\library\r3\fhir3_pathnode.pas',
  fhir3_questionnaire2 in '..\library\r3\fhir3_questionnaire2.pas',
  fhir3_base in '..\library\r3\fhir3_base.pas',
  fhir3_parserBase in '..\library\r3\fhir3_parserBase.pas',
  fhir3_parser in '..\library\r3\fhir3_parser.pas',
  fhir3_client in '..\library\r3\fhir3_client.pas',
  fhir3_common in '..\library\r3\fhir3_common.pas',
  fhir3_factory in '..\library\r3\fhir3_factory.pas',
  fhir3_narrative in '..\library\r3\fhir3_narrative.pas',
  fhir3_validator in '..\library\r3\fhir3_validator.pas',
  fhir3_operations in '..\library\r3\fhir3_operations.pas',
  fhir3_opbase in '..\library\r3\fhir3_opbase.pas',
  fhir3_authmap in '..\library\r3\fhir3_authmap.pas',
  fhir_client_threaded in '..\library\client\fhir_client_threaded.pas',
  fhir_client_http in '..\library\client\fhir_client_http.pas',
  fhir_oauth in '..\library\smart\fhir_oauth.pas',
  FHIR.Smart.Login in '..\library\smart\FHIR.Smart.Login.pas',
  FHIR.Tools.Indexing in '..\library\tools\FHIR.Tools.Indexing.pas',
  FHIR.Tools.DiffEngine in '..\library\tools\FHIR.Tools.DiffEngine.pas',
  FHIR.Tools.ValidationWrapper in '..\library\tools\FHIR.Tools.ValidationWrapper.pas',
  FHIR.Version.Client in '..\library\version\FHIR.Version.Client.pas',
  FHIR.Version.Parser in '..\library\version\FHIR.Version.Parser.pas',
  fhir_client_registry in '..\library\client\fhir_client_registry.pas',
  FHIR.Client.ServerDialogFMX in '..\library\client\FHIR.Client.ServerDialogFMX.pas' {EditRegisteredServerForm},
  FHIR.Client.ClientDialogFMX in '..\library\client\FHIR.Client.ClientDialogFMX.pas' {RegisterClientForm},
  fhir_cdshooks in '..\library\cdshooks\fhir_cdshooks.pas',
  FHIR.Ui.OSX in '..\library\ui\FHIR.Ui.OSX.pas',
  FHIR.Ui.ComboFMX in '..\library\ui\FHIR.Ui.ComboFMX.pas',
  fsl_graphql in '..\library\web\fsl_graphql.pas',
  fhir_client_async in '..\library\client\fhir_client_async.pas',
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
  SearchParameterCombinationEditor in 'SearchParameterCombinationEditor.pas' {SearchParameterCombinationEditorForm},
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
  fhir_oids in '..\library\base\fhir_oids.pas',
  fhir_elementmodel in '..\library\base\fhir_elementmodel.pas',
  MarkdownHTMLEntities in '..\..\markdown\source\MarkdownHTMLEntities.pas',
  FHIR.Ui.Fmx in '..\library\ui\FHIR.Ui.Fmx.pas',
  fhir3_organiser in '..\library\r3\fhir3_organiser.pas',
  fsl_npm in '..\library\npm\fsl_npm.pas',
  FHIR.Npm.Client in '..\library\npm\FHIR.Npm.Client.pas',
  fhir3_resources_admin in '..\library\r3\fhir3_resources_admin.pas',
  fhir3_resources_other in '..\library\r3\fhir3_resources_other.pas',
  fhir3_resources_clinical in '..\library\r3\fhir3_resources_clinical.pas',
  fhir3_resources_canonical in '..\library\r3\fhir3_resources_canonical.pas',
  fhir3_resources_base in '..\library\r3\fhir3_resources_base.pas',
  MarkdownUnicodeUtils in '..\..\markdown\source\MarkdownUnicodeUtils.pas';

{$R *.res}

begin
  {$IFDEF WINDOWS}
  IdOpenSSLSetLibPath(extractFilePath(paramstr(0)));
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMasterToolsForm, MasterToolsForm);
  Application.Run;
end.
