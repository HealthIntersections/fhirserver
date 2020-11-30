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
  fsl_base in '..\library\fsl\fsl_base.pas',
  fsl_stream in '..\library\fsl\fsl_stream.pas',
  fsl_collections in '..\library\fsl\fsl_collections.pas',
  fsl_utilities in '..\library\fsl\fsl_utilities.pas',
  fsl_xml in '..\library\fsl\fsl_xml.pas',
  fsl_turtle in '..\library\fsl\fsl_turtle.pas',
  fsl_json in '..\library\fsl\fsl_json.pas',
  fsl_crypto in '..\library\fsl\fsl_crypto.pas',
  fsl_logging in '..\library\fsl\fsl_logging.pas',
  fsl_fpc in '..\library\fsl\fsl_fpc.pas',
  fsl_threads in '..\library\fsl\fsl_threads.pas',
  fsl_http in '..\library\fsl\fsl_http.pas',
  fsl_fetcher in '..\library\fsl\fsl_fetcher.pas',
  fsl_wininet in '..\library\fsl\fsl_wininet.pas',
  fsl_npm_cache in '..\library\fsl\fsl_npm_cache.pas',
  fhir_parser in '..\library\fhir\fhir_parser.pas',
  fhir_objects in '..\library\fhir\fhir_objects.pas',
  fsl_htmlgen in '..\library\fsl\fsl_htmlgen.pas',
  fsl_scim in '..\library\fsl\fsl_scim.pas',
  fhir_xhtml in '..\library\fhir\fhir_xhtml.pas',
  fhir_factory in '..\library\fhir\fhir_factory.pas',
  fhir_validator in '..\library\fhir\fhir_validator.pas',
  fhir_common in '..\library\fhir\fhir_common.pas',
  fhir_pathengine in '..\library\fhir\fhir_pathengine.pas',
  fhir_narrative in '..\library\fhir\fhir_narrative.pas',
  fhir_utilities in '..\library\fhir\fhir_utilities.pas',
  fhir_client in '..\library\fhir\fhir_client.pas',
  fsl_ucum in '..\library\fsl\fsl_ucum.pas',
  fhir3_xml in '..\library\fhir3\fhir3_xml.pas',
  fhir3_utilities in '..\library\fhir3\fhir3_utilities.pas',
  fhir3_context in '..\library\fhir3\fhir3_context.pas',
  fhir3_types in '..\library\fhir3\fhir3_types.pas',
  fhir3_resources in '..\library\fhir3\fhir3_resources.pas',
  fhir3_constants in '..\library\fhir3\fhir3_constants.pas',
  fhir3_tags in '..\library\fhir3\fhir3_tags.pas',
  fhir3_json in '..\library\fhir3\fhir3_json.pas',
  fhir3_turtle in '..\library\fhir3\fhir3_turtle.pas',
  fhir3_elementmodel in '..\library\fhir3\fhir3_elementmodel.pas',
  fhir3_profiles in '..\library\fhir3\fhir3_profiles.pas',
  fhir3_pathengine in '..\library\fhir3\fhir3_pathengine.pas',
  fhir3_indexinfo in '..\library\fhir3\fhir3_indexinfo.pas',
  fhir3_pathnode in '..\library\fhir3\fhir3_pathnode.pas',
  fhir3_questionnaire2 in '..\library\fhir3\fhir3_questionnaire2.pas',
  fhir3_base in '..\library\fhir3\fhir3_base.pas',
  fhir3_parserBase in '..\library\fhir3\fhir3_parserBase.pas',
  fhir3_parser in '..\library\fhir3\fhir3_parser.pas',
  fhir3_client in '..\library\fhir3\fhir3_client.pas',
  fhir3_common in '..\library\fhir3\fhir3_common.pas',
  fhir3_factory in '..\library\fhir3\fhir3_factory.pas',
  fhir3_narrative in '..\library\fhir3\fhir3_narrative.pas',
  fhir3_validator in '..\library\fhir3\fhir3_validator.pas',
  fhir3_operations in '..\library\fhir3\fhir3_operations.pas',
  fhir3_opbase in '..\library\fhir3\fhir3_opbase.pas',
  fhir3_authmap in '..\library\fhir3\fhir3_authmap.pas',
  fhir_client_threaded in '..\library\fhir\fhir_client_threaded.pas',
  fhir_client_http in '..\library\fhir\fhir_client_http.pas',
  fhir_oauth in '..\library\fhir\fhir_oauth.pas',
  fhir_indexing in '..\library\fhir\fhir_indexing.pas',
  fhir_diff in '..\library\tools\fhir_diff.pas',
  FHIR.Tools.ValidationWrapper in '..\library\tools\FHIR.Tools.ValidationWrapper.pas',
  FHIR.Version.Client in '..\library\version\FHIR.Version.Client.pas',
  FHIR.Version.Parser in '..\library\version\FHIR.Version.Parser.pas',
  fhir_client_registry in '..\library\fhir\fhir_client_registry.pas',
  FHIR.Client.ServerDialogFMX in '..\library\fhir\FHIR.Client.ServerDialogFMX.pas' {EditRegisteredServerForm},
  FHIR.Client.ClientDialogFMX in '..\library\fhir\FHIR.Client.ClientDialogFMX.pas' {RegisterClientForm},
  fhir_cdshooks in '..\library\cdshooks\fhir_cdshooks.pas',
  FHIR.Ui.OSX in '..\library\ui\FHIR.Ui.OSX.pas',
  FHIR.Ui.ComboFMX in '..\library\ui\FHIR.Ui.ComboFMX.pas',
  fsl_graphql in '..\library\fsl\fsl_graphql.pas',
  fhir_client_async in '..\library\fhir\fhir_client_async.pas',
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
  fhir_oids in '..\library\fhir\fhir_oids.pas',
  fhir_elementmodel in '..\library\fhir\fhir_elementmodel.pas',
  MarkdownHTMLEntities in '..\..\markdown\source\MarkdownHTMLEntities.pas',
  FHIR.Ui.Fmx in '..\library\ui\FHIR.Ui.Fmx.pas',
  fhir3_organiser in '..\library\fhir3\fhir3_organiser.pas',
  fsl_npm in '..\library\fsl\fsl_npm.pas',
  FHIR.Npm.Client in '..\library\fsl\FHIR.Npm.Client.pas',
  fhir3_resources_admin in '..\library\fhir3\fhir3_resources_admin.pas',
  fhir3_resources_other in '..\library\fhir3\fhir3_resources_other.pas',
  fhir3_resources_clinical in '..\library\fhir3\fhir3_resources_clinical.pas',
  fhir3_resources_canonical in '..\library\fhir3\fhir3_resources_canonical.pas',
  fhir3_resources_base in '..\library\fhir3\fhir3_resources_base.pas',
  MarkdownUnicodeUtils in '..\..\markdown\source\MarkdownUnicodeUtils.pas',
  fsl_openssl in '..\library\fsl\fsl_openssl.pas',
  fsl_npm_client in '..\library\fsl\fsl_npm_client.pas';

{$R *.res}

begin
  {$IFDEF WINDOWS}
  IdOpenSSLSetLibPath(extractFilePath(paramstr(0)));
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMasterToolsForm, MasterToolsForm);
  Application.Run;
end.
