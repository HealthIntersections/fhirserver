unit ValueSetEditor;

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
interface

uses
  System.SysUtils, System.Rtti, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.ListBox, FMX.Edit, FMX.DialogService,
  FMX.Grid.Style, FMX.Grid, FMX.Menus,
  fsl_utilities, fsl_stream, FHIR.Ui.Fmx,
  fhir_objects, 
  FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities, fhir_indexing, FHIR.Version.IndexInfo,
  BaseResourceFrame,
  SearchParameterEditor, ListSelector, AddRestResourceDialog, ValuesetExpansion, ValuesetSelectDialog, MemoEditorDialog, TranslationsEditorDialog,
  ResourceEditingSupport, System.ImageList, FMX.ImgList, ResourceHistoryDialog;

type
  TFrame = TBaseResourceFrame; // re-aliasing the Frame to work around a designer bug

  TValueSetEditorFrame = class(TFrame)
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    tvStructure: TTreeView;
    tbStructure: TTabControl;
    tbMetadata: TTabItem;
    tbRest: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cbExperimental: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    edtURL: TEdit;
    edtName: TEdit;
    edtTitle: TEdit;
    cbxStatus: TComboBox;
    dedDate: TDateEdit;
    edtPublisher: TEdit;
    edtDescription: TEdit;
    edtPurpose: TEdit;
    edtCopyright: TEdit;
    cbxJurisdiction: TComboBox;
    edtVersion: TEdit;
    tvMetadata: TTreeViewItem;
    tbResource: TTabItem;
    btnFlipCompose: TButton;
    btnFlipExpansion: TButton;
    Panel1: TPanel;
    VertScrollBox1: TVertScrollBox;
    cbImmutable: TCheckBox;
    cbExtensible: TCheckBox;
    btnIdentifierAdd: TButton;
    btnIdentifierDelete: TButton;
    gridIdentifiers: TGrid;
    Label12: TLabel;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    cbInactive: TCheckBox;
    dedLockedDate: TDateEdit;
    Label14: TLabel;
    Label15: TLabel;
    edtSystem: TEdit;
    Label16: TLabel;
    edtSystemVersion: TEdit;
    lbImports: TListBox;
    btnAddImport: TButton;
    btnDeleteImport: TButton;
    Label18: TLabel;
    gridFilters: TGrid;
    btnAddFilter: TButton;
    btnDeleteFilter: TButton;
    gridConcepts: TGrid;
    btnAddConcept: TButton;
    btnDeleteConcept: TButton;
    gridDesignations: TGrid;
    btnAddDesignation: TButton;
    btnDeleteDesignation: TButton;
    Label20: TLabel;
    StringColumn3: TStringColumn;
    PopupColumn1: TPopupColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringColumn8: TStringColumn;
    PopupColumn2: TPopupColumn;
    PopupColumn3: TPopupColumn;
    StringColumn9: TStringColumn;
    btnAddInclude: TButton;
    btnAddExclude: TButton;
    btnDeleteRules: TButton;
    tabSelect: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    rbList: TRadioButton;
    Concepts: TLabel;
    rbFilter: TRadioButton;
    tabExpansion: TTabItem;
    Label17: TLabel;
    edtIdentifier: TEdit;
    Label19: TLabel;
    Label21: TLabel;
    edtTotal: TEdit;
    edtOffset: TEdit;
    Label22: TLabel;
    gridContains: TGrid;
    btnAddContains: TButton;
    btnDeleteContains: TButton;
    gridContainsDesignations: TGrid;
    PopupColumn4: TPopupColumn;
    PopupColumn5: TPopupColumn;
    StringColumn7: TStringColumn;
    Label23: TLabel;
    btnAddContainsDesignation: TButton;
    btnDeleteContainsDesignation: TButton;
    StringColumn10: TStringColumn;
    CheckColumn1: TCheckColumn;
    CheckColumn2: TCheckColumn;
    StringColumn11: TStringColumn;
    StringColumn12: TStringColumn;
    StringColumn13: TStringColumn;
    dedTimestamp: TDateEdit;
    tmTimestamp: TTimeEdit;
    Label24: TLabel;
    tabParameter: TTabItem;
    lblError: TLabel;
    btnExpand: TButton;
    gridParameters: TGrid;
    StringColumn14: TStringColumn;
    StringColumn15: TStringColumn;
    PopupColumn6: TPopupColumn;
    btnAddParameter: TButton;
    btnDeleteParameter: TButton;
    btnSelectConcepts: TButton;
    btnMemoForDesc: TButton;
    btnMemoPurpose: TButton;
    btnMemoCopyright: TButton;
    VertScrollBox2: TVertScrollBox;
    btnExport: TButton;
    dlgExport: TSaveDialog;
    rbAll: TRadioButton;
    TabItem3: TTabItem;
    ToolbarImages: TImageList;
    Label25: TLabel;
    edtVSVersion: TEdit;
    btnNameTranslations: TButton;
    Label26: TLabel;
    Label27: TLabel;
    edtSteward: TEdit;
    TreeViewItem1: TTreeViewItem;
    tbHl7: TTabItem;
    VertScrollBox3: TVertScrollBox;
    Label29: TLabel;
    memOpenIssues: TMemo;
    Label30: TLabel;
    Label31: TLabel;
    edtLegalese: TEdit;
    Label32: TLabel;
    edtVDeprecated: TEdit;
    edtVersionPolicy: TEdit;
    Label33: TLabel;
    procedure tvStructureClick(Sender: TObject);
    procedure inputChanged(Sender: TObject);
    procedure btnFlipComposeClick(Sender: TObject);
    procedure gridIdentifiersGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridIdentifiersSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnIdentifierAddClick(Sender: TObject);
    procedure btnIdentifierDeleteClick(Sender: TObject);
    procedure btnAddImportClick(Sender: TObject);
    procedure btnDeleteImportClick(Sender: TObject);
    procedure lbImportsClick(Sender: TObject);
    procedure gridFiltersGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridFiltersSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnAddFilterClick(Sender: TObject);
    procedure btnDeleteFilterClick(Sender: TObject);
    procedure gridIdentifiersSelChanged(Sender: TObject);
    procedure gridFiltersSelChanged(Sender: TObject);
    procedure gridConceptsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridConceptsSelChanged(Sender: TObject);
    procedure gridConceptsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnAddConceptClick(Sender: TObject);
    procedure btnDeleteConceptClick(Sender: TObject);
    procedure gridDesignationsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridDesignationsSelChanged(Sender: TObject);
    procedure gridDesignationsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnAddDesignationClick(Sender: TObject);
    procedure btnDeleteDesignationClick(Sender: TObject);
    procedure btnAddIncludeClick(Sender: TObject);
    procedure btnAddExcludeClick(Sender: TObject);
    procedure btnDeleteRulesClick(Sender: TObject);
    procedure btnFlipExpansionClick(Sender: TObject);
    procedure gridContainsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridContainsSelChanged(Sender: TObject);
    procedure gridContainsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnAddContainsClick(Sender: TObject);
    procedure btnDeleteContainsClick(Sender: TObject);
    procedure gridContainsDesignationsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridContainsDesignationsSelChanged(Sender: TObject);
    procedure gridContainsDesignationsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnAddContainsDesignationClick(Sender: TObject);
    procedure btnDeleteContainsDesignationClick(Sender: TObject);
    procedure btnAddParameterClick(Sender: TObject);
    procedure btnExpandClick(Sender: TObject);
    procedure gridParametersGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridParametersSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure gridParametersSelChanged(Sender: TObject);
    procedure btnDeleteParameterClick(Sender: TObject);
    procedure rbListClick(Sender: TObject);
    procedure rbFilterClick(Sender: TObject);
    procedure btnSelectConceptsClick(Sender: TObject);
    procedure btnMemoForDescClick(Sender: TObject);
    procedure btnMemoPurposeClick(Sender: TObject);
    procedure btnMemoCopyrightClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure rbAllClick(Sender: TObject);
    procedure btnNameTranslationsClick(Sender: TObject);
  private
    tvCompose, tvExpansion : TTreeViewItem;
    function GetValueSet: TFHIRValueSet;
    function readJurisdiction : Integer;
    function getJurisdiction(i : integer) : TFHIRCodeableConcept;

    procedure loadMetadata;
    procedure loadHL7Process;
    procedure loadCompose(compose: TFhirValueSetCompose);
    procedure loadRule(rule: TFhirValueSetComposeInclude);
    procedure loadExpansion(expansion: TFhirValueSetExpansion);
    procedure loadParameters();

    procedure commitMetadata;
    procedure commitHL7Process;
    procedure commitCompose(compose: TFhirValueSetCompose);
    procedure commitRule(rule: TFhirValueSetComposeInclude);
    procedure commitExpansion(expansion: TFhirValueSetExpansion);

  public
    destructor Destroy; override;

    property ValueSet : TFHIRValueSet read GetValueSet;
    procedure load; override;

    procedure commit; override;
    procedure cancel; override;

  end;

implementation

{$R *.fmx}

{ TValueSetEditorFrame }

procedure TValueSetEditorFrame.btnFlipComposeClick(Sender: TObject);
begin
  if ValueSet.compose = nil then
  begin
    ValueSet.compose := TFhirValueSetCompose.Create;
    tvCompose := TTreeViewItem.Create(tvStructure);
    tvCompose.text := 'Content Definition';
    tvStructure.AddObject(tvCompose);
    tvCompose.TagObject := ValueSet.compose;
    ValueSet.compose.TagObject := tvCompose;
    btnFlipCompose.text := 'Remove Content Definition';
    tvStructure.Selected := tvCompose;
    tvStructureClick(nil);
    ResourceIsDirty := true;
  end
  else
  begin
    ValueSet.compose := nil;
    tvCompose := TTreeViewItem.Create(tvStructure);
    tvStructure.RemoveObject(tvCompose);
    tvCompose.Free;
    tvCompose := nil;
    btnFlipCompose.text := 'Add Content Definition';
    tvStructure.Selected := tvMetadata;
    tvStructureClick(nil);
    ResourceIsDirty := true;
  end;
  btnExpand.Enabled := ValueSet.compose <> nil;
end;

procedure TValueSetEditorFrame.btnAddConceptClick(Sender: TObject);
var
  rule : TFHIRValueSetComposeInclude;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  rule.conceptList.Append.code := '*';
  gridConcepts.RowCount := 0;
  gridConcepts.RowCount := rule.conceptList.count;
  ResourceIsDirty := true;
  btnDeleteConcept.Enabled := gridConcepts.Row > -1;
  rbList.Enabled := false;
  rbFilter.Enabled := false;
  rbAll.Enabled := false;
  rbFilter.isChecked := true;
end;

procedure TValueSetEditorFrame.btnAddContainsClick(Sender: TObject);
begin
  ValueSet.expansion.containsList.Append.code := '*';
  gridContains.RowCount := 0;
  gridContains.RowCount := ValueSet.expansion.containsList.count;
  ResourceIsDirty := true;
  btnDeleteContains.Enabled := gridContains.Row > -1;
end;

procedure TValueSetEditorFrame.btnAddContainsDesignationClick(Sender: TObject);
var
  contains : TFhirValueSetExpansionContains;
begin
  contains := ValueSet.expansion.containsList[gridContains.Row];
  contains.designationList.Append.language := 'en';
  gridContainsDesignations.RowCount := 0;
  gridContainsDesignations.RowCount := contains.designationList.count;
  ResourceIsDirty := true;
  btnDeleteContainsDesignation.Enabled := gridContainsDesignations.Row > -1;
end;

procedure TValueSetEditorFrame.btnAddDesignationClick(Sender: TObject);
var
  rule : TFHIRValueSetComposeInclude;
  concept : TFhirValueSetComposeIncludeConcept;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  concept := rule.conceptList[gridConcepts.Row];
  concept.designationList.Append.language := 'en';
  gridDesignations.RowCount := 0;
  gridDesignations.RowCount := concept.designationList.count;
  ResourceIsDirty := true;
  btnDeleteDesignation.Enabled := gridDesignations.Row > -1;
end;

procedure TValueSetEditorFrame.btnAddExcludeClick(Sender: TObject);
var
  inc : TFhirValueSetComposeInclude;
  tiInc : TTreeViewItem;
begin
  inc := ValueSet.compose.excludeList.Append;
  tiInc := TTreeViewItem.Create(tvCompose);
  tiInc.text := 'Exclude Rule';
  tvCompose.AddObject(tiInc);
  tiInc.TagObject := inc;
  inc.TagObject := tiInc;
  tvStructure.Selected := tiInc;
  tvStructureClick(nil);
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.btnFlipExpansionClick(Sender: TObject);
var
  tiSubItem : TTreeViewItem;
begin
  if ValueSet.expansion = nil then
  begin
    ValueSet.expansion := TFhirValueSetExpansion.Create;
    ValueSet.expansion.timestamp := TFslDateTime.makeLocal;
    tvExpansion := TTreeViewItem.Create(tvStructure);
    tvExpansion.text := 'Expansion';
    tvStructure.AddObject(tvExpansion);
    tvExpansion.TagObject := ValueSet.expansion;
    tiSubItem := TTreeViewItem.Create(tvExpansion);
    tiSubItem.text := 'Parameters';
    tvExpansion.AddObject(tiSubItem);
    tiSubItem.TagObject := ValueSet.expansion.parameterList;
    ValueSet.compose.TagObject := tvExpansion;
    btnFlipExpansion.text := 'Remove Fixed Expansion';
    tvStructure.Selected := tvExpansion;
    tvStructureClick(nil);
    ResourceIsDirty := true;
  end
  else
  begin
    ValueSet.expansion := nil;
    tvStructure.RemoveObject(tvExpansion);
    tvExpansion.Free;
    tvExpansion := nil;
    btnFlipExpansion.text := 'Add Fixed Expansion';
    tvStructure.Selected := tvMetadata;
    tvStructureClick(nil);
    ResourceIsDirty := true;
  end;
end;

procedure TValueSetEditorFrame.btnAddFilterClick(Sender: TObject);
var
  rule : TFHIRValueSetComposeInclude;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  rule.filterList.Append.op := FilterOperatorEqual;
  gridFilters.RowCount := 0;
  gridFilters.RowCount := rule.filterList.count;
  ResourceIsDirty := true;
  btnDeleteFilter.Enabled := gridIdentifiers.Row > -1;
  rbList.Enabled := false;
  rbFilter.Enabled := false;
  rbAll.Enabled := false;
  rbFilter.Enabled := true;
end;

procedure TValueSetEditorFrame.btnAddImportClick(Sender: TObject);
var
  rule : TFHIRValueSetComposeInclude;
begin
  // todo: replace this with a real dialog that looks up value sets from somewhere...
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  TDialogService.InputQuery('Import ValueSet', ['URL'], [''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if (AResult = mrOK) and (aValues[0] <> '') then
      begin
        rule.valueSetList.Append.value := aValues[0];
        lbImports.Items.add(aValues[0]);
        ResourceIsDirty := true;
        btnDeleteImport.Enabled := lbImports.Count > 0;
        if lbImports.Count > 0 then
          lbImports.ItemIndex := lbImports.Count-1;
      end;
    end);
end;

procedure TValueSetEditorFrame.btnAddIncludeClick(Sender: TObject);
var
  inc : TFhirValueSetComposeInclude;
  tiInc : TTreeViewItem;
begin
  inc := ValueSet.compose.includeList.Append;
  tiInc := TTreeViewItem.Create(tvCompose);
  tiInc.text := 'Include Rule';
  tvCompose.AddObject(tiInc);
  tiInc.TagObject := inc;
  inc.TagObject := tiInc;
  tvStructure.Selected := tiInc;
  tvStructureClick(nil);
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.btnAddParameterClick(Sender: TObject);
begin
  ValueSet.expansion.parameterList.Append.name := '*';
  gridParameters.RowCount := 0;
  gridParameters.RowCount := ValueSet.expansion.parameterList.count;
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.btnDeleteContainsDesignationClick(Sender: TObject);
var
  contains : TFhirValueSetExpansionContains;
begin
  contains := ValueSet.expansion.containsList[gridContains.Row];
  contains.designationList.Remove(gridContainsDesignations.Row);
  gridContainsDesignations.RowCount := 0;
  gridContainsDesignations.RowCount := contains.designationList.count;
  if gridContainsDesignations.Row >= contains.designationList.count then
    gridContainsDesignations.Row := gridContainsDesignations.Row - 1;
  ResourceIsDirty := true;
  btnDeleteContainsDesignation.Enabled := gridContainsDesignations.Row > -1;
end;

procedure TValueSetEditorFrame.btnDeleteConceptClick(Sender: TObject);
var
  rule : TFHIRValueSetComposeInclude;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  rule.conceptList.Remove(gridConcepts.Row);
  gridConcepts.RowCount := 0;
  gridConcepts.RowCount := rule.conceptList.count;
  ResourceIsDirty := true;
  if gridConcepts.Row >= rule.conceptList.count then
    gridConcepts.Row := gridConcepts.Row - 1;
  btnDeleteConcept.Enabled := gridConcepts.Row > -1;
  rbList.Enabled := rule.conceptList.Count = 0;
  rbFilter.Enabled := rule.conceptList.Count = 0;
  rbAll.Enabled := rule.conceptList.Count = 0;
end;

procedure TValueSetEditorFrame.btnDeleteContainsClick(Sender: TObject);
begin
  ValueSet.expansion.containsList.remove(gridContains.Row);
  gridContains.RowCount := 0;
  gridContains.RowCount := ValueSet.expansion.containsList.count;
  if gridContains.Row >= ValueSet.expansion.containsList.count then
    gridContains.Row := gridContains.Row - 1;
  ResourceIsDirty := true;
  btnDeleteContains.Enabled := gridContains.Row > -1;
end;

procedure TValueSetEditorFrame.btnDeleteDesignationClick(Sender: TObject);
var
  rule : TFHIRValueSetComposeInclude;
  concept : TFhirValueSetComposeIncludeConcept;
  designation : TFhirValueSetComposeIncludeConceptDesignation;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  concept := rule.conceptList[gridConcepts.Row];
  concept.designationList.Remove(gridDesignations.Row);
  gridDesignations.RowCount := 0;
  gridDesignations.RowCount := concept.designationList.count;
  if gridDesignations.Row >= concept.designationList.count then
    gridDesignations.Row := gridDesignations.Row - 1;
  ResourceIsDirty := true;
  btnDeleteDesignation.Enabled := gridDesignations.Row > -1;
end;

procedure TValueSetEditorFrame.btnDeleteFilterClick(Sender: TObject);
var
  rule : TFHIRValueSetComposeInclude;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  rule.filterList.Remove(gridFilters.Row);
  gridFilters.RowCount := 0;
  gridFilters.RowCount := rule.filterList.count;
  if gridFilters.Row >= rule.filterList.count then
    gridFilters.Row := gridFilters.Row - 1;
  ResourceIsDirty := true;
  btnDeleteFilter.Enabled := gridFilters.Row > -1;
  rbList.Enabled := rule.filterList.Count = 0;
  rbFilter.Enabled := rule.filterList.Count = 0;
  rbAll.Enabled := rule.conceptList.Count = 0;
end;

procedure TValueSetEditorFrame.btnDeleteImportClick(Sender: TObject);
var
  rule : TFHIRValueSetComposeInclude;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  rule.valueSetList.Remove(lbImports.ItemIndex);
  lbImports.Items.Delete(lbImports.ItemIndex);
  ResourceIsDirty := true;
  btnDeleteImport.Enabled := lbImports.Count > 0;
  if (lbImports.Count > 0) and (lbImports.ItemIndex = -1) then
    lbImports.ItemIndex := 0;
end;

procedure TValueSetEditorFrame.btnDeleteParameterClick(Sender: TObject);
begin
  ValueSet.expansion.parameterList.remove(gridParameters.Row);
  gridParameters.RowCount := 0;
  gridParameters.RowCount := ValueSet.expansion.parameterList.count;
  ResourceIsDirty := true;
  if gridParameters.Row >= ValueSet.expansion.parameterList.count then
    gridParameters.Row := gridParameters.Row - 1;
  btnDeleteParameter.Enabled := gridParameters.Row > -1;
end;

procedure TValueSetEditorFrame.btnDeleteRulesClick(Sender: TObject);
var
  form : TListSelectorForm;
  i, ind : integer;
  rule : TFhirValueSetComposeInclude;
begin
  form := TListSelectorForm.Create(self);
  try
    form.Caption := 'Select Rules to delete';
    for rule in ValueSet.compose.includeList do
      form.ListBox1.Items.AddObject('Include '+rule.summary, rule);
    for rule in ValueSet.compose.excludeList do
      form.ListBox1.Items.AddObject('Exclude '+rule.summary, rule);
    if ShowModalHack(form) = mrOk then
    begin
      for i := 0 to form.ListBox1.Items.Count - 1 do
        if form.ListBox1.ListItems[i].IsChecked then
        begin
          rule := form.ListBox1.Items.Objects[i] as TFhirValueSetComposeInclude;
          ind := ValueSet.compose.includeList.IndexOf(rule);
          if ind > -1 then
          begin
            tvCompose.RemoveObject(rule.TagObject as TFmxObject);
            ValueSet.compose.includeList.Remove(ind);
          end
          else
          begin
            ind := ValueSet.compose.excludeList.IndexOf(form.ListBox1.Items.Objects[i] as TFhirValueSetComposeInclude);
            tvCompose.RemoveObject(rule.TagObject as TFmxObject);
            ValueSet.compose.excludeList.Remove(ind);
          end;
        end;
      ResourceIsDirty := true;
    end;
  finally
    form.Free;
  end;
end;

procedure TValueSetEditorFrame.btnIdentifierAddClick(Sender: TObject);
begin
  ValueSet.identifierList.Append;
  gridIdentifiers.RowCount := 0;
  gridIdentifiers.RowCount := ValueSet.identifierList.count;
end;

procedure TValueSetEditorFrame.btnIdentifierDeleteClick(Sender: TObject);
begin
  ValueSet.identifierList.Remove(gridIdentifiers.Selected);
  gridIdentifiers.RowCount := 0;
  gridIdentifiers.RowCount := ValueSet.identifierList.count;
end;

procedure TValueSetEditorFrame.btnMemoCopyrightClick(Sender: TObject);
begin
  if ValueSet.copyrightElement = nil then
    ValueSet.copyrightElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'ValueSet Copyright', btnMemoCopyright, edtCopyright, ValueSet, ValueSet.copyrightElement);
end;

procedure TValueSetEditorFrame.btnMemoForDescClick(Sender: TObject);
begin
  if ValueSet.descriptionElement = nil then
    ValueSet.descriptionElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'ValueSet Description', btnMemoForDesc, edtDescription, ValueSet, ValueSet.descriptionElement);
end;

procedure TValueSetEditorFrame.btnSelectConceptsClick(Sender: TObject);
var
  form : TValuesetSelectForm;
  rule : TFHIRValueSetComposeInclude;
  contains : TFhirValueSetExpansionContains;
  concept : TFhirValueSetComposeIncludeConcept;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  form := TValuesetSelectForm.Create(self);
  try
    form.Settings := Settings.link;
    form.system := edtSystem.Text;
    form.version :=  edtSystemVersion.Text;
    form.hasConcepts := rule.conceptList.Count > 0;
    if ShowModalHack(form) = mrOk then
    begin
      if form.replace then
        rule.conceptList.Clear;
      for contains in form.expansion.expansion.containsList do
        if contains.TagInt > 0 then
        begin
          concept := rule.conceptList.Append;
          concept.code := contains.code;
          if form.cbUseDisplays.IsChecked then
            concept.display := contains.display;
        end;
      gridConcepts.RowCount := 0;
      gridConcepts.RowCount := rule.conceptList.count;
      ResourceIsDirty := true;
      btnDeleteConcept.Enabled := gridConcepts.Row > -1;
      rbList.Enabled := false;
      rbFilter.Enabled := false;
    end;
  finally
    form.Free;
  end;
end;

procedure TValueSetEditorFrame.btnMemoPurposeClick(Sender: TObject);
begin
  if ValueSet.purposeElement = nil then
    ValueSet.purposeElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'ValueSet Purpose', btnMemoPurpose, edtPurpose, ValueSet, ValueSet.purposeElement);
end;

procedure TValueSetEditorFrame.btnNameTranslationsClick(Sender: TObject);
begin
  if ValueSet.nameElement = nil then
    ValueSet.nameElement := TFhirString.Create;
  editStringDialog(self, 'ValueSet Name', btnNameTranslations, edtVSVersion, ValueSet, ValueSet.nameElement);
end;

procedure TValueSetEditorFrame.btnExpandClick(Sender: TObject);
var
  form : TValuesetExpansionForm;
  tiSubItem : TTreeViewItem;
  parameter : TFhirValueSetExpansionParameter;
begin
  form := TValuesetExpansionForm.Create(self);
  try
    form.Settings := Settings.link;
    form.ValueSet := ValueSet.Link;
    if ShowModalHack(form) = mrOk then
    begin
      ValueSet.expansion := form.Expansion.expansion.Link;
      if (tvExpansion = nil) then
      begin
        tvExpansion := TTreeViewItem.Create(tvStructure);
        tvExpansion.text := 'Expansion';
        tvStructure.AddObject(tvExpansion);
      end
      else
        tvExpansion.DeleteChildren;

      tvExpansion.TagObject := ValueSet.expansion;
      ValueSet.expansion.TagObject := tvExpansion;
      tiSubItem := TTreeViewItem.Create(tvExpansion);
      tiSubItem.text := 'Parameters';
      tvExpansion.AddObject(tiSubItem);
      tiSubItem.TagObject := ValueSet.expansion.parameterList;
      btnFlipExpansion.text := 'Remove Fixed Expansion';
      tvStructure.Selected := tvMetadata;
      tvStructure.ExpandAll;
      tvStructureClick(nil);
      ResourceIsDirty := true;
    end;
  finally
    form.Free;
  end;
end;

procedure TValueSetEditorFrame.btnExportClick(Sender: TObject);
var
  obj : TObject;
  s : String;
begin
  if dlgExport.Execute then
  begin
    s := ExtractFileExt(dlgExport.FileName);
    if s = '.csv' then
    begin
    obj := tvStructure.Selected.TagObject;
    if obj is TFhirValueSetExpansion then
      produceCsv(dlgExport.FileName,
        ['system', 'abstract', 'inactive', 'version', 'code', 'display'],
        procedure (csv : TCSVWriter)
        var
          c : TFhirValueSetExpansionContains;
        begin
          for c in (obj as TFhirValueSetExpansion).containsList do
          begin
            csv.cell(c.system);
            csv.cell(c.abstract);
            csv.cell(c.inactive);
            csv.cell(c.version);
            csv.cell(c.code);
            csv.cell(c.display);
            csv.line;
          end;
        end
      )
    else // obj is TFhirValueSetComposeInclude
    begin
      produceCsv(dlgExport.FileName,
        ['system', 'version', 'code', 'display'],
        procedure (csv : TCSVWriter)
        var
          c : TFhirValueSetComposeIncludeConcept;
        begin
          for c in (obj as TFhirValueSetComposeInclude).conceptList do
          begin
            csv.cell((obj as TFhirValueSetComposeInclude).system);
            csv.cell((obj as TFhirValueSetComposeInclude).version);
            csv.cell(c.code);
            csv.cell(c.display);
            csv.line;
          end;
        end
      )
    end
    end
    else
      raise EFHIRException.create('Unknown format');
  end;
end;

procedure TValueSetEditorFrame.cancel;
begin
end;

procedure TValueSetEditorFrame.commit;
var
  obj : TObject;
begin
  obj := tvStructure.Selected.TagObject;
  if obj is TFhirValueSet then
    CommitMetadata
  else if obj = nil then
    commitHL7Process
  else if obj is TFhirValueSetCompose then
    commitCompose(obj as TFhirValueSetCompose)
  else if obj is TFhirValueSetComposeInclude then
    commitRule(obj as TFhirValueSetComposeInclude)
  else if obj is TFhirValueSetExpansion then
    commitExpansion(obj as TFhirValueSetExpansion);
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.commitMetadata;
var
  s : String;
  cc : TFHIRCodeableConcept;
begin
  ValueSet.experimental := cbExperimental.IsChecked;

  ValueSet.url := edtURL.Text;
  ValueSet.name := edtName.Text;
  ValueSet.title := edtTitle.Text;
  ValueSet.version := edtVSVersion.Text;
  ValueSet.publisher := edtPublisher.text;
  ValueSet.description := edtDescription.Text;
  ValueSet.purpose := edtPurpose.Text;
  ValueSet.copyright := edtCopyright.Text;
  ValueSet.status := TFhirPublicationStatusEnum(cbxStatus.ItemIndex);
  ValueSet.date := TFslDateTime.make(dedDate.DateTime, dttzLocal);
  ValueSet.immutable := cbImmutable.IsChecked;
  ValueSet.jurisdictionList.Clear;
  {$IFDEF FHIR3}
  ValueSet.extensible := cbExtensible.IsChecked;
  {$ELSE}
  if cbExtensible.IsChecked then
    ValueSet.forceExtension('http://hl7.org/fhir/StructureDefinition/valueset-extensible').value := TFHIRBoolean.Create(true)
  else
    ValueSet.removeExtension('http://hl7.org/fhir/StructureDefinition/valueset-extensible');
  {$ENDIF}
  cc := getJurisdiction(cbxJurisdiction.ItemIndex);
  if (cc <> nil) then
    ValueSet.jurisdictionList.add(cc);
end;


procedure TValueSetEditorFrame.commitRule(rule: TFhirValueSetComposeInclude);
begin
  rule.system := edtSystem.Text;
  rule.version := edtSystemVersion.Text;
end;

destructor TValueSetEditorFrame.Destroy;
begin
  inherited;
end;

procedure TValueSetEditorFrame.commitCompose(compose: TFhirValueSetCompose);
begin
  if dedLockedDate.Text = '' then
    compose.lockedDateElement := nil
  else
    compose.lockedDate := TFslDateTime.make(dedLockedDate.DateTime, dttzLocal);
  compose.inactive := cbInactive.IsChecked;
end;

procedure TValueSetEditorFrame.commitExpansion(expansion: TFhirValueSetExpansion);
begin
  expansion.timestampElement := storeDateTime(dedTimestamp, tmTimestamp);
  expansion.identifier := edtIdentifier.text ;
  expansion.total := edtTotal.Text;
  expansion.offset := edtOffset.text;
end;

procedure TValueSetEditorFrame.commitHL7Process;
var
  cp : TFhirContactDetail;
  s : String;
begin
  if edtSteward.Text = '' then
    ValueSet.removeExtension('http://hl7.org/fhir/StructureDefinition/resource-steward')
  else
  begin
    if ValueSet.hasExtension('http://hl7.org/fhir/StructureDefinition/resource-steward') then
     cp := ValueSet.getExtensionValue('http://hl7.org/fhir/StructureDefinition/resource-steward') as TFhirContactDetail
    else
    begin
      cp := TFhirContactDetail.Create;
      ValueSet.addExtension('http://hl7.org/fhir/StructureDefinition/resource-steward', cp);
    end;
    cp.name := edtSteward.Text
  end;
  if edtLegalese.Text = '' then
    ValueSet.removeExtension('http://hl7.org/fhir/StructureDefinition/codesystem-MIFNotation')
  else
    ValueSet.setExtensionString('http://hl7.org/fhir/StructureDefinition/codesystem-MIFNotation', edtLegalese.Text);
  if edtVDeprecated.Text = '' then
    ValueSet.removeExtension('http://hl7.org/fhir/StructureDefinition/resource-versionDeprecated')
  else
    ValueSet.setExtensionString('http://hl7.org/fhir/StructureDefinition/resource-versionDeprecated', edtVDeprecated.Text);
  if edtVersionPolicy.Text = '' then
    ValueSet.removeExtension('http://hl7.org/fhir/StructureDefinition/resource-versioningPolicy')
  else
    ValueSet.setExtensionString('http://hl7.org/fhir/StructureDefinition/resource-versioningPolicy', edtVersionPolicy.Text);
  ValueSet.removeExtension('http://hl7.org/fhir/StructureDefinition/resource-openIssue');
  for s in memOpenIssues.Lines do
    if s <> '' then
      ValueSet.addExtension('http://hl7.org/fhir/StructureDefinition/resource-openIssue', s);
end;

function TValueSetEditorFrame.GetValueSet: TFHIRValueSet;
begin
  result := TFHIRValueSet(Resource);
end;

procedure TValueSetEditorFrame.gridConceptsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  rule : TFHIRValueSetComposeInclude;
  concept : TFhirValueSetComposeIncludeConcept;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  concept := rule.conceptList[aRow];
  case aCol of
    0: value := concept.code;
    1: value := concept.display;
//    2: value := concept.extension....;
  end;
end;

procedure TValueSetEditorFrame.gridConceptsSelChanged(Sender: TObject);
var
  rule : TFHIRValueSetComposeInclude;
  concept : TFhirValueSetComposeIncludeConcept;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  btnDeleteConcept.Enabled := gridConcepts.Row > -1;
  if gridConcepts.Row > -1 then
  begin
    concept := rule.conceptList[gridConcepts.Row];
    btnAddDesignation.Enabled := true;
    btnDeleteDesignation.Enabled := false;
    gridDesignations.Enabled := true;
    gridDesignations.RowCount := concept.designationList.Count;
  end
  else
  begin
    btnAddDesignation.Enabled := false;
    btnDeleteDesignation.Enabled := false;
    gridDesignations.Enabled := false;
    gridDesignations.RowCount := 0;
  end;
end;

procedure TValueSetEditorFrame.gridConceptsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  rule : TFHIRValueSetComposeInclude;
  concept : TFhirValueSetComposeIncludeConcept;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  concept := rule.conceptList[aRow];
  case aCol of
    0: concept.code := value.AsString;
    1: concept.display := value.AsString;
//    2: concept.extension....;
  end;
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.gridContainsDesignationsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  contains : TFhirValueSetExpansionContains;
  designation : TFhirValueSetComposeIncludeConceptDesignation;
begin
  contains := ValueSet.expansion.containsList[gridContains.Row];
  designation := contains.designationList[aRow];
  case aCol of
    0: value := displayLang(designation.language);
    1: value := displayUse(designation.use);
    2: value := designation.value
  end;
end;

procedure TValueSetEditorFrame.gridContainsDesignationsSelChanged(Sender: TObject);
begin
  btnDeleteContainsDesignation.Enabled := gridContainsDesignations.Row > 1;
end;

procedure TValueSetEditorFrame.gridContainsDesignationsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  contains : TFhirValueSetExpansionContains;
  designation : TFhirValueSetComposeIncludeConceptDesignation;
  s : String;
begin
  contains := ValueSet.expansion.containsList[gridContains.Row];
  designation := contains.designationList[aRow];
  s := value.AsString;
  case aCol of
    0: if s = '' then designation.language := '' else designation.language := s.Substring(0, 2);
    1: if s = '' then designation.use := nil else designation.use := TFHIRCoding.Create('http://snomed.info/sct', codeForUse(s));
    2: designation.value := s;
  end;
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.gridContainsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  contains : TFhirValueSetExpansionContains;
begin
  contains := ValueSet.expansion.containsList[ARow];
  case ACol of
    0: value := contains.code;
    1: value := contains.abstract;
    2: value := contains.inactive;
    3: value := contains.display;
    4: value := contains.system;
    5: value := contains.version;
  end;
end;

procedure TValueSetEditorFrame.gridContainsSelChanged(Sender: TObject);
var
  contains : TFhirValueSetExpansionContains;
begin
  btnDeleteContains.Enabled := gridContains.Row > -1;

  if gridContains.Row > -1 then
  begin
    contains := ValueSet.expansion.containsList[gridContains.Row];
    btnAddContainsDesignation.Enabled := true;
    btnDeleteContainsDesignation.Enabled := false;
    gridContainsDesignations.Enabled := true;
    gridContainsDesignations.RowCount := contains.designationList.Count;
  end
  else
  begin
    btnAddContainsDesignation.Enabled := false;
    btnDeleteContainsDesignation.Enabled := false;
    gridContainsDesignations.Enabled := false;
    gridContainsDesignations.RowCount := 0;
  end;
end;

procedure TValueSetEditorFrame.gridContainsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  contains : TFhirValueSetExpansionContains;
begin
  contains := ValueSet.expansion.containsList[ARow];
  case ACol of
    0: contains.code := Value.AsString;
    1: contains.abstract := Value.AsBoolean;
    2: contains.inactive := Value.AsBoolean;
    3: contains.display := Value.AsString;
    4: contains.system := Value.AsString;
    5: contains.version := Value.AsString;
  end;
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.gridDesignationsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  rule : TFHIRValueSetComposeInclude;
  concept : TFhirValueSetComposeIncludeConcept;
  designation : TFhirValueSetComposeIncludeConceptDesignation;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  concept := rule.conceptList[gridConcepts.Row];
  designation := concept.designationList[aRow];
  case aCol of
    0: value := displayLang(designation.language);
    1: value := displayUse(designation.use);
    2: value := designation.value
  end;
end;

procedure TValueSetEditorFrame.gridDesignationsSelChanged(Sender: TObject);
begin
  btnDeleteDesignation.Enabled := gridDesignations.Row > 1;
end;

procedure TValueSetEditorFrame.gridDesignationsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  rule : TFHIRValueSetComposeInclude;
  concept : TFhirValueSetComposeIncludeConcept;
  designation : TFhirValueSetComposeIncludeConceptDesignation;
  s : string;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  concept := rule.conceptList[gridConcepts.Row];
  designation := concept.designationList[aRow];
  s := value.AsString;
  case aCol of
    0: if s = '' then designation.language := '' else designation.language := s.Substring(0, 2);
    1: if s = '' then designation.use := nil else designation.use := TFHIRCoding.Create('http://snomed.info/sct', codeForUse(s));
    2: designation.value := s;
  end;
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.gridFiltersGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  rule : TFHIRValueSetComposeInclude;
  filter : TFhirValueSetComposeIncludeFilter;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  filter := rule.filterList[aRow];
  case aCol of
    0: value := filter.property_;
    1: value := CODES_TFhirFilterOperatorEnum[filter.op];
    2: value := filter.value;
  end;
end;

procedure TValueSetEditorFrame.gridFiltersSelChanged(Sender: TObject);
begin
  btnDeleteFilter.Enabled := gridFilters.Row > -1;
end;

procedure TValueSetEditorFrame.gridFiltersSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  rule : TFHIRValueSetComposeInclude;
  filter : TFhirValueSetComposeIncludeFilter;
begin
  rule := tbResource.TagObject as TFHIRValueSetComposeInclude;
  filter := rule.filterList[aRow];
  case aCol of
    0: filter.property_ := value.AsString;
    1: filter.op := TFhirFilterOperatorEnum(StringArrayIndexOfSensitive(CODES_TFhirFilterOperatorEnum, value.AsString));
    2: filter.value  := value.AsString;
  end;
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.gridIdentifiersGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  id : TFhirIdentifier;
begin
  id := ValueSet.identifierList[ARow];
  Value := '';
  case aCol of
    0: Value := id.system;
    1: Value := id.value;
  end;
end;

procedure TValueSetEditorFrame.gridIdentifiersSelChanged(Sender: TObject);
begin
  btnIdentifierDelete.Enabled := gridIdentifiers.Row > -1;
end;

procedure TValueSetEditorFrame.gridIdentifiersSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  id : TFhirIdentifier;
begin
  id := ValueSet.identifierList[ARow];
  case aCol of
    0: id.system := Value.AsString;
    1: id.value := Value.AsString;
  end;
  ResourceIsDirty := true;
end;

procedure TValueSetEditorFrame.gridParametersGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  param : TFhirValueSetExpansionParameter;
begin
  param := ValueSet.expansion.parameterList[ARow];
  case ACol of
    0: Value := param.name;
    1: if param.value = nil then Value := '' else Value := param.value.primitiveValue;
    2: if param.value = nil then Value := 'string' else Value := param.value.fhirType;
  end;
end;

procedure TValueSetEditorFrame.gridParametersSelChanged(Sender: TObject);
begin
  btnDeleteParameter.Enabled := (gridParameters.Row >= 0) and (gridParameters.Row < ValueSet.expansion.parameterList.Count);
end;

procedure TValueSetEditorFrame.gridParametersSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  param : TFhirValueSetExpansionParameter;
  v : TFHIRType;
  s : String;
begin
  param := ValueSet.expansion.parameterList[ARow];
  try
    case ACol of
      0: param.name := Value.AsString;
      1: if param.value = nil then
           param.value := TFHIRString.Create(Value.AsString)
         else if param.value is TFhirString then
           TFHIRString(param.value).value := value.AsString
         else if param.value is TFhirBoolean then
           TFhirBoolean(param.value).value := StrToBool(value.AsString)
         else if param.value is TFhirInteger then
           TFhirInteger(param.value).value := inttostr(StrToInt(value.AsString))
         else if param.value is TFhirDecimal then
           TFhirDecimal(param.value).value := TFslDecimal.ValueOf(value.AsString).AsString
         else if param.value is TFhirUri then
           TFhirUri(param.value).value := value.AsString
         else if param.value is TFhirCode then
           TFhirCode(param.value).value := value.AsString;
      2:
       begin
         v := param.value.Link;
         try
           try
             if v = nil then
               s := ''
             else
               s := v.primitiveValue;
             if Value.AsString = 'string' then
               param.value := TFhirString.Create(s)
             else if Value.AsString = 'boolean' then
               param.value := TFhirBoolean.Create(StrToBool(s))
             else if Value.AsString = 'integer' then
               param.value := TFhirInteger.Create(inttostr(StrToInt((s))))
             else if Value.AsString = 'decimal' then
               param.value := TFhirDecimal.Create(TFslDecimal.ValueOf(s).AsString)
             else if Value.AsString = 'uri' then
               param.value := TFhirUri.Create(s)
             else if Value.AsString = 'code' then
               param.value := TFhirCode.Create(s)
           except
             param.value := v.Link;
             raise;
           end;
         finally
           v.Free;
         end;
       end;
    end;
    lblError.Visible := false;
    ResourceIsDirty := true;
  except
    on e: Exception do
    begin
      lblError.Visible := true;
      lblError.Text := 'Error: '+e.Message;
    end;
  end;
end;

function TValueSetEditorFrame.getJurisdiction(i: integer): TFHIRCodeableConcept;
begin
  case i of
    1:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'AT');
    2:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'AU');
    3:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'BR');
    4:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CA');
    5:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CH');
    6:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CL');
    7:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CN');
    8:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'DE');
    9:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'DK');
    10:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'EE');
    11:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'ES');
    12:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'FI');
    13:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'FR');
    14:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'GB');
    15:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'NL');
    16:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'NO');
    17:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'NZ');
    18:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'RU');
    19:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'US');
    21:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'VN');
    22:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '001');
    23:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '002');
    24:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '019');
    25:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '142');
    26:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '150');
    27:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '053');
    else
      result := nil;
  end;
end;

procedure TValueSetEditorFrame.inputChanged(Sender: TObject);
begin
  if not Loading then
    commit;
end;

procedure TValueSetEditorFrame.lbImportsClick(Sender: TObject);
begin
  btnDeleteImport.Enabled := (lbImports.ItemIndex > -1);
end;

procedure TValueSetEditorFrame.load;
var
  tiSubItem : TTreeViewItem;
  item : TFhirValueSetComposeInclude;
begin
  inherited;
  tvMetadata.TagObject := ValueSet;
  if (tvCompose <> nil) then
  begin
    tvStructure.RemoveObject(tvCompose);
    FreeAndNil(tvCompose);
  end;
  if (tvExpansion <> nil) then
  begin
    tvStructure.RemoveObject(tvExpansion);
    FreeAndNil(tvExpansion);
  end;

  if ValueSet.compose <> nil then
  begin
    tvCompose := TTreeViewItem.Create(tvStructure);
    tvCompose.text := 'Content Definition';
    tvStructure.AddObject(tvCompose);
    tvCompose.TagObject := ValueSet.compose;
    ValueSet.compose.TagObject := tvCompose;
    for item in ValueSet.compose.includeList do
    begin
      tiSubItem := TTreeViewItem.Create(tvCompose);
      tiSubItem.text := 'Include Rule';
      tvCompose.AddObject(tiSubItem);
      tiSubItem.TagObject := item;
      item.TagObject := tiSubItem;
    end;
    for item in ValueSet.compose.excludeList do
    begin
      tiSubItem := TTreeViewItem.Create(tvCompose);
      tiSubItem.text := 'Exclude Rule';
      tvCompose.AddObject(tiSubItem);
      tiSubItem.TagObject := item;
      item.TagObject := tiSubItem;
    end;
  end;
  if ValueSet.expansion <> nil then
  begin
    tvExpansion := TTreeViewItem.Create(tvStructure);
    tvExpansion.text := 'Expansion';
    tvStructure.AddObject(tvExpansion);
    tvExpansion.TagObject := ValueSet.expansion;
    ValueSet.expansion.TagObject := tvExpansion;
    tiSubItem := TTreeViewItem.Create(tvExpansion);
    tiSubItem.text := 'Parameters';
    tvExpansion.AddObject(tiSubItem);
    tiSubItem.TagObject := ValueSet.expansion.parameterList;
  end;
  if ValueSet.compose = nil then
    btnFlipCompose.text := 'Add Content Definition'
  else
    btnFlipCompose.text := 'Remove Content Definition';
  if ValueSet.expansion = nil then
    btnFlipExpansion.text := 'Add Fixed Expansion'
  else
    btnFlipExpansion.text := 'Remove Fixed Expansion';
  btnExpand.Enabled := ValueSet.compose <> nil;

  tvStructure.Selected := tvMetadata;
  tvStructure.ExpandAll;
  tvStructureClick(nil);
end;

procedure TValueSetEditorFrame.loadMetadata;
var
  url : TFHIRUri;
begin
  cbExperimental.IsChecked := ValueSet.experimental;

  edtURL.Text := ValueSet.url;
  edtName.Text := ValueSet.name;
  edtTitle.Text := ValueSet.title;
  edtVSVersion.Text := ValueSet.version;
  edtPublisher.text := ValueSet.publisher;
  edtDescription.Text := ValueSet.description;
  edtPurpose.Text := ValueSet.purpose;
  edtCopyright.Text := ValueSet.copyright;
  cbxStatus.ItemIndex := ord(ValueSet.status);
  if ValueSet.dateElement = nil then
    dedDate.Text := ''
  else
    dedDate.DateTime := ValueSet.date.DateTime;
  cbxJurisdiction.ItemIndex := readJurisdiction;
  cbImmutable.IsChecked := ValueSet.immutable;
  {$IFDEF FHIR3}
  cbExtensible.IsChecked := ValueSet.extensible;
  {$ELSE}
  cbExtensible.IsChecked := ValueSet.hasExtension('http://hl7.org/fhir/StructureDefinition/valueset-extensible') and
    (ValueSet.getExtensionString('http://hl7.org/fhir/StructureDefinition/valueset-extensible') = 'true');

  {$ENDIF}
  gridIdentifiers.RowCount := ValueSet.identifierList.Count;
  btnIdentifierDelete.Enabled := false;
end;

procedure TValueSetEditorFrame.loadParameters();
begin
  lblError.Visible := false;
  gridParameters.RowCount := ValueSet.expansion.parameterList.Count;
end;

procedure TValueSetEditorFrame.loadRule(rule: TFhirValueSetComposeInclude);
var
  uri : TFhirUri;
begin
  edtSystem.Text := rule.system;
  edtSystemVersion.Text := rule.version;
  lbImports.Clear;
  for uri in rule.valueSetList do
    lbImports.Items.Add(uri.value);
  btnDeleteImport.Enabled := lbImports.Count > 0;
  if lbImports.Count > 0 then
    lbImports.ItemIndex := 0;

  if rule.conceptList.Count > 0 then
  begin
    rbAll.IsChecked := false;
    rbAll.Enabled := false;
    rbList.IsChecked := true;
    tabSelect.TabIndex := 1;
    rbList.Enabled := false;
    rbFilter.Enabled := false;
  end
  else if rule.filterList.Count > 0 then
  begin
    rbAll.IsChecked := false;
    rbAll.Enabled := false;
    rbFilter.IsChecked := true;
    tabSelect.TabIndex := 0;
    rbList.Enabled := false;
    rbFilter.Enabled := false;
  end
  else
  begin
    rbAll.IsChecked := true;
    rbAll.Enabled := true;
    tabSelect.TabIndex := 2;
    rbList.Enabled := true;
    rbFilter.Enabled := true;
  end;

  gridFilters.RowCount := 0;
  gridFilters.RowCount := rule.filterList.Count;
  btnDeleteFilter.Enabled := false;
  gridConcepts.RowCount := 0;
  gridConcepts.RowCount := rule.conceptList.Count;
  btnDeleteConcept.Enabled := false;
  gridDesignations.RowCount := 0;
end;

procedure TValueSetEditorFrame.loadCompose(compose: TFhirValueSetCompose);
begin
  if compose.lockedDateElement = nil then
    dedLockedDate.Text := ''
  else
    dedLockedDate.DateTime := compose.lockedDate.DateTime;
  cbInactive.IsChecked := compose.inactive;
end;

procedure TValueSetEditorFrame.loadExpansion(expansion: TFhirValueSetExpansion);
begin
  presentDateTime(expansion.timestampElement, dedTimestamp, tmTimestamp);
  edtIdentifier.text := expansion.identifier;
  edtTotal.Text := expansion.total;
  edtOffset.text := expansion.offset;
  gridContains.RowCount := 0;
  gridContains.RowCount := expansion.containsList.Count;
  gridContainsDesignations.RowCount := 0;
  btnAddContainsDesignation.Enabled := false;
  btnDeleteContainsDesignation.Enabled := false;
end;

procedure TValueSetEditorFrame.loadHL7Process;
var
  cp : TFhirContactDetail;
  ext : TFhirExtension;
begin
  if ValueSet.hasExtension('http://hl7.org/fhir/StructureDefinition/valueset-steward') then
    ValueSet.getExtensionByUrl('http://hl7.org/fhir/StructureDefinition/valueset-steward').url := 'http://hl7.org/fhir/StructureDefinition/resource-steward';

  if ValueSet.hasExtension('http://hl7.org/fhir/StructureDefinition/resource-steward') then
    cp := ValueSet.getExtensionValue('http://hl7.org/fhir/StructureDefinition/resource-steward') as TFhirContactDetail
  else
    cp := nil;
  if cp = nil then
    edtSteward.Text := ''
  else
    edtSteward.Text := cp.name;

  edtLegalese.Text := ValueSet.getExtensionString('http://hl7.org/fhir/StructureDefinition/codesystem-MIFNotation');
  edtVDeprecated.Text := ValueSet.getExtensionString('http://hl7.org/fhir/StructureDefinition/resource-versionDeprecated');
  edtVersionPolicy.Text := ValueSet.getExtensionString('http://hl7.org/fhir/StructureDefinition/resource-versioningPolicy');

  memOpenIssues.Text := '';
  for ext in ValueSet.extensionList do
  begin
    if ext.url = 'http://hl7.org/fhir/StructureDefinition/resource-openIssue' then
      memOpenIssues.lines.Add(ext.value.primitiveValue);
  end;
end;

procedure TValueSetEditorFrame.rbListClick(Sender: TObject);
begin
  tabSelect.TabIndex := 1;
end;

procedure TValueSetEditorFrame.rbAllClick(Sender: TObject);
begin
  tabSelect.TabIndex := 2;
end;

procedure TValueSetEditorFrame.rbFilterClick(Sender: TObject);
begin
  tabSelect.TabIndex := 0;
end;


function TValueSetEditorFrame.readJurisdiction: Integer;
var
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
begin
  result := -1;
  for cc in ValueSet.jurisdictionList do
    for c in cc.codingList do
    begin
      if c.system = 'urn:iso:std:iso:3166' then
      begin
        if c.code = 'AT' then exit(1);
        if c.code = 'AU' then exit(2);
        if c.code = 'BR' then exit(3);
        if c.code = 'CA' then exit(4);
        if c.code = 'CH' then exit(5);
        if c.code = 'CL' then exit(6);
        if c.code = 'CN' then exit(7);
        if c.code = 'DE' then exit(8);
        if c.code = 'DK' then exit(9);
        if c.code = 'EE' then exit(10);
        if c.code = 'ES' then exit(11);
        if c.code = 'FI' then exit(12);
        if c.code = 'FR' then exit(13);
        if c.code = 'GB' then exit(14);
        if c.code = 'NL' then exit(15);
        if c.code = 'NO' then exit(16);
        if c.code = 'NZ' then exit(17);
        if c.code = 'RU' then exit(18);
        if c.code = 'US' then exit(19);
        if c.code = 'VN' then exit(20);
      end
      else if c.system = 'http://unstats.un.org/unsd/methods/m49/m49.htm' then
      begin
        if c.code = '001' { World } then exit(22);
        if c.code = '002' { Africa } then exit(23);
        if c.code = '019' { Americas } then exit(24);
        if c.code = '142' { Asia } then exit(25);
        if c.code = '150' { Europe } then exit(26);
        if c.code = '053' { Australia and New Zealand } then exit(27);
      end
    end;
end;


procedure TValueSetEditorFrame.tvStructureClick(Sender: TObject);
var
  obj : TObject;
begin
  Loading := true;
  try
    obj := tvStructure.Selected.TagObject;
    if obj is TFhirValueSet then
    begin
      tbMetadata.TagObject := obj;
      tbStructure.ActiveTab := tbMetadata;
      loadMetadata;
      btnExport.Enabled := false;
    end
    else if obj = nil then
    begin
      tbMetadata.TagObject := obj;
      tbStructure.ActiveTab := tbHl7;
      loadHL7Process;
      btnExport.Enabled := false;
    end
    else if obj is TFhirValueSetCompose then
    begin
      tbRest.TagObject := obj;
      tbStructure.ActiveTab := tbRest;
      loadCompose(obj as TFhirValueSetCompose);
      btnExport.Enabled := false;
    end
    else if obj is TFhirValueSetComposeInclude then
    begin
      tbResource.TagObject := obj;
      tbStructure.ActiveTab := tbResource;
      loadRule(obj as TFhirValueSetComposeInclude);
      btnExport.Enabled := true;
    end
    else if obj is TFhirValueSetExpansion then
    begin
      tbRest.TagObject := obj;
      tbStructure.ActiveTab := tabExpansion;
      loadExpansion(obj as TFhirValueSetExpansion);
      btnExport.Enabled := true;
    end
    else if obj is TFhirValueSetExpansionParameterList then
    begin
      tabParameter.TagObject := obj;
      tbStructure.ActiveTab := tabParameter;
      loadParameters;
      btnExport.Enabled := false;
    end
  finally
    Loading := false;
  end;
end;

end.


