unit CapabilityStatementEditor;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation, System.ImageList,
  FMX.ImgList, FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.ListBox, FMX.Edit,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.Menus,
  BaseResourceFrame,
  fsl_base, fsl_utilities, FHIR.Ui.Fmx,
  fhir_objects, FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Utilities, fhir_indexing, FHIR.Version.IndexInfo, FHIR.Version.Factory, FHIR.Version.Common,
  SearchParameterEditor, SearchParameterCombinationEditor, ListSelector, AddRestResourceDialog, AddRestOperationDialog, TranslationsEditorDialog, MemoEditorDialog,
  FMX.Memo.Types;

type
  TFrame = TBaseResourceFrame; // re-aliasing the Frame to work around a designer bug

  TCapabilityStatementEditorFrame = class(TFrame)
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
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    cbXml: TCheckBox;
    cbJson: TCheckBox;
    cbTurtle: TCheckBox;
    cbTerminologyService: TCheckBox;
    Label15: TLabel;
    edtURL: TEdit;
    edtName: TEdit;
    edtTitle: TEdit;
    cbxStatus: TComboBox;
    dedDate: TDateEdit;
    edtPublisher: TEdit;
    edtDescription: TEdit;
    edtPurpose: TEdit;
    edtCopyright: TEdit;
    cbxKind: TComboBox;
    cbxJurisdiction: TComboBox;
    mImplementationGuides: TMemo;
    edtVersion: TEdit;
    tvMetadata: TTreeViewItem;
    Label16: TLabel;
    mSecurity: TMemo;
    cbCORS: TCheckBox;
    cbClientCerts: TCheckBox;
    cbOAuth: TCheckBox;
    cbSmart: TCheckBox;
    Label17: TLabel;
    mDoco: TMemo;
    tbResource: TTabItem;
    Label18: TLabel;
    mDocoRes: TMemo;
    Label19: TLabel;
    Label20: TLabel;
    cbRead: TCheckBox;
    edtDocoRead: TEdit;
    cbVRead: TCheckBox;
    edtDocoVRead: TEdit;
    edtDocoSearch: TEdit;
    cbUpdate: TCheckBox;
    edtDocoCreate: TEdit;
    cbPatch: TCheckBox;
    cbDelete: TCheckBox;
    edtDocoUpdate: TEdit;
    cbHistoryInstance: TCheckBox;
    edtDocoPatch: TEdit;
    cbHistoryType: TCheckBox;
    edtDocoDelete: TEdit;
    cbCreate: TCheckBox;
    edtDocoHistoryInstance: TEdit;
    edtDocoHistoryType: TEdit;
    cbSearch: TCheckBox;
    cbUpdateCreate: TCheckBox;
    Label21: TLabel;
    cbCondCreate: TCheckBox;
    cbCondUpdate: TCheckBox;
    cbCondDelete: TCheckBox;
    Label22: TLabel;
    cbxReadCondition: TComboBox;
    Label23: TLabel;
    cbRefLiteral: TCheckBox;
    cbRefLogical: TCheckBox;
    cbRefResolve: TCheckBox;
    cbRefEnforced: TCheckBox;
    cbRefLocal: TCheckBox;
    edtProfile: TEdit;
    Label24: TLabel;
    cbxVersioning: TComboBox;
    Label25: TLabel;
    btnParamAdd: TButton;
    btnParamAddStd: TButton;
    btnParamEdit: TButton;
    btnParamDelete: TButton;
    Label26: TLabel;
    cbTransaction: TCheckBox;
    edtTransaction: TEdit;
    cbBatch: TCheckBox;
    edtBatch: TEdit;
    cbSystemSearch: TCheckBox;
    edtSystemSearch: TEdit;
    cbSystemHistory: TCheckBox;
    edtSystemHistory: TEdit;
    btnAddClient: TButton;
    btnAddServer: TButton;
    Panel1: TPanel;
    btnAddResources: TButton;
    VertScrollBox1: TVertScrollBox;
    btnDeleteResources: TButton;
    gridSearch: TGrid;
    Column1: TColumn;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    PopupColumn1: TPopupColumn;
    VertScrollBox2: TVertScrollBox;
    VertScrollBox3: TVertScrollBox;
    btnAddOperations: TButton;
    btnRemoveOperations: TButton;
    ToolbarImages: TImageList;
    btnDoco: TButton;
    btnSecurity: TButton;
    btnPublisher: TButton;
    btnDescription: TButton;
    btnPurpose: TButton;
    btnCopyright: TButton;
    btnDocoRes: TButton;
    btnDocoRead: TButton;
    btnDocoVRead: TButton;
    btnDocoSearch: TButton;
    btnDocoCreate: TButton;
    btnDocoUpdate: TButton;
    btnDocoPatch: TButton;
    btnDocoDelete: TButton;
    btnDocoHistoryInstance: TButton;
    btnDocoHistoryType: TButton;
    btnTitle: TButton;
    btnTransaction: TButton;
    btnBatch: TButton;
    btnSystemSearch: TButton;
    btnSystemHistory: TButton;
    cbxFHIRVersion: TComboBox;
    Label27: TLabel;
    cbxResourceConformance: TComboBox;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Label28: TLabel;
    gridSearchCombinations: TGrid;
    PopupColumn2: TPopupColumn;
    StringColumn3: TStringColumn;
    btnAddSearchParamCombination: TButton;
    btnEditSearchParamCombination: TButton;
    btnDeleteSearchCombination: TButton;
    PopupColumn3: TPopupColumn;
    procedure tvStructureClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure inputChanged(Sender: TObject);
    procedure btnParamEditClick(Sender: TObject);
    procedure lbSearchClick(Sender: TObject);
    procedure btnParamDeleteClick(Sender: TObject);
    procedure btnParamAddClick(Sender: TObject);
    procedure btnParamAddStdClick(Sender: TObject);
    procedure btnAddClientClick(Sender: TObject);
    procedure btnAddServerClick(Sender: TObject);
    procedure btnAddResourcesClick(Sender: TObject);
    procedure btnDeleteResourcesClick(Sender: TObject);
    procedure gridSearchGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridSearchSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnAddOperationsClick(Sender: TObject);
    procedure btnRemoveOperationsClick(Sender: TObject);
    procedure btnPublisherClick(Sender: TObject);
    procedure btnTitleClick(Sender: TObject);
    procedure btnDescriptionClick(Sender: TObject);
    procedure btnPurposeClick(Sender: TObject);
    procedure btnCopyrightClick(Sender: TObject);
    procedure btnDocoClick(Sender: TObject);
    procedure btnSecurityClick(Sender: TObject);
    procedure btnTransactionClick(Sender: TObject);
    procedure btnBatchClick(Sender: TObject);
    procedure btnSystemSearchClick(Sender: TObject);
    procedure btnSystemHistoryClick(Sender: TObject);
    procedure btnDocoResClick(Sender: TObject);
    procedure btnDocoReadClick(Sender: TObject);
    procedure btnDocoVReadClick(Sender: TObject);
    procedure btnDocoSearchClick(Sender: TObject);
    procedure btnDocoCreateClick(Sender: TObject);
    procedure btnDocoUpdateClick(Sender: TObject);
    procedure btnDocoPatchClick(Sender: TObject);
    procedure btnDocoDeleteClick(Sender: TObject);
    procedure btnDocoHistoryInstanceClick(Sender: TObject);
    procedure btnDocoHistoryTypeClick(Sender: TObject);
    procedure gridSearchCellDblClick(const Column: TColumn; const Row: Integer);
    procedure gridSearchCombinationsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridSearchCombinationsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnDeleteSearchCombinationClick(Sender: TObject);
    procedure btnAddSearchParamCombinationClick(Sender: TObject);
    procedure btnEditSearchParamCombinationClick(Sender: TObject);
  private
    function GetCapabilityStatement: TFHIRCapabilityStatement;
    function readJurisdiction : Integer;
    function getJurisdiction(i : integer) : TFHIRCodeableConcept;

    procedure loadMetadata;
    procedure loadRest(rest : TFhirCapabilityStatementRest);
    procedure loadResource(res : TFhirCapabilityStatementRestResource);

    procedure commitMetadata;
    procedure commitRest(rest : TFhirCapabilityStatementRest);
    procedure commitResource(res : TFhirCapabilityStatementRestResource);

  public
    { Public declarations }
    property CapabilityStatement : TFHIRCapabilityStatement read GetCapabilityStatement;
    procedure load; override;

    procedure commit; override;
    procedure cancel; override;

  end;

implementation

{$R *.fmx}

function editStringorMarkdownDialog(owner : TComponent; title : String; button : TButton; edit : TEdit; resource : TFHIRResource; element : {$IFDEF FHIR3}TFHIRString{$ELSE}TFHIRMarkdown{$ENDIF}) : boolean;
begin
  {$IFDEF FHIR3}
  result := editStringDialog(owner, title, button, edit, resource, element);
  {$ELSE}
  result := editMarkdownDialog(owner, title, button, edit, resource, element);
  {$ENDIF}
end;

{ TCapabilityStatementEditorFrame }

procedure TCapabilityStatementEditorFrame.btnAddClientClick(Sender: TObject);
var
  rest : TFhirCapabilityStatementRest;
  tiRest : TTreeViewItem;
begin
  rest := CapabilityStatement.restList.Append;
  rest.mode := RestfulCapabilityModeClient;
  tiRest := TTreeViewItem.Create(tvMetadata);
  tiRest.text := 'Client';
  tvMetadata.AddObject(tiRest);
  tiRest.TagObject := rest;
  rest.TagObject := tiRest;
  btnAddClient.Enabled := false;
  tvStructure.Selected := tiRest;
  tvStructureClick(nil);
  ResourceIsDirty := true;
end;

procedure TCapabilityStatementEditorFrame.btnAddOperationsClick(Sender: TObject);
var
  form : TAddRestOperationForm;
  res : TFhirCapabilityStatementRestResource;
  rest : TFhirCapabilityStatementRest;
begin
  rest := tvStructure.Selected.TagObject as TFhirCapabilityStatementRest;

  form := TAddRestOperationForm.create(self);
  try
    form.cbRead.enabled := false;
    form.cbVRead.enabled := false;
    form.cbSearch.enabled := false;
    form.cbCreate.enabled := false;
    form.cbUpdate.enabled := false;
    form.cbDelete.enabled := false;
    form.cbHistoryInstance.enabled := false;
    form.cbHistoryType.enabled := false;
    form.cbPatch.enabled := false;

    for res in rest.resourceList do
    begin
      if res.interaction(TypeRestfulInteractionRead) = nil then form.cbRead.Enabled := true;
      if res.interaction(TypeRestfulInteractionVRead) = nil then form.cbVRead.Enabled := true;
      if res.interaction(TypeRestfulInteractionSearchType) = nil then form.cbSearch.Enabled := true;
      if res.interaction(TypeRestfulInteractionCreate) = nil then form.cbCreate.Enabled := true;
      if res.interaction(TypeRestfulInteractionUpdate) = nil then form.cbUpdate.Enabled := true;
      if res.interaction(TypeRestfulInteractionDelete) = nil then form.cbDelete.Enabled := true;
      if res.interaction(TypeRestfulInteractionHistoryInstance) = nil then form.cbHistoryInstance.Enabled := true;
      if res.interaction(TypeRestfulInteractionHistoryType) = nil then form.cbHistoryType.Enabled := true;
      if res.interaction(TypeRestfulInteractionPatch) = nil then form.cbPatch.Enabled := true;
    end;
    if showmodalHack(form) = mrOk then
    begin
      for res in rest.resourceList do
      begin
        if form.cbRead.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionRead;
        if form.cbVRead.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionVRead;
        if form.cbSearch.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionSearchType;
        if form.cbCreate.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionCreate;
        if form.cbUpdate.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionUpdate;
        if form.cbDelete.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionDelete;
        if form.cbHistoryInstance.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionHistoryInstance;
        if form.cbHistoryType.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionHistoryType;
        if form.cbPatch.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionPatch;
        if form.cbUpdateCreate.IsChecked then res.updateCreate := true;
        if form.cbCondCreate.IsChecked then res.conditionalCreate := true;
        if form.cbCondUpdate.IsChecked then res.conditionalUpdate := true;
        if form.cbCondDelete.IsChecked then res.conditionalDelete := ConditionalDeleteStatusSingle;
      end;
      ResourceIsDirty := true;
    end;
  finally
    form.free;
  end;
end;

procedure TCapabilityStatementEditorFrame.btnAddResourcesClick(Sender: TObject);
var
  form : TAddRestResourceForm;
  res : TFhirCapabilityStatementRestResource;
  rs : TFhirResourceTypeSet;
  r : TFhirResourceType;
  rest : TFhirCapabilityStatementRest;
  i : integer;
  indexes : TFhirIndexList;
  compartments : TFHIRCompartmentList;
  builder : TFHIRIndexBuilder;
  list : TFslList<TFhirIndex>;
  index : TFhirIndex;
  p : TFhirCapabilityStatementRestResourceSearchParam;
  tiRes : TTreeViewItem;
begin
  rest := tvStructure.Selected.TagObject as TFhirCapabilityStatementRest;
  rs := ALL_RESOURCE_TYPES;
  for res in rest.resourceList do
  begin
    r := ResourceTypeByName(CODES_TFhirResourceTypesEnum[res.type_]);
    rs := rs - [r];
  end;

  form := TAddRestResourceForm.create(self);
  try
    for r in rs do
      if r <> frtCustom then
        form.ListBox1.Items.addObject(CODES_TFhirResourceType[r], TObject(r));
    if showmodalHack(form) = mrOk then
    begin
      for i := 0 to form.ListBox1.Items.Count - 1 do
        if form.ListBox1.ListItems[i].isChecked then
        begin
          r := TFhirResourceType(form.ListBox1.Items.Objects[i]);
          res := rest.resourceList.Append;
          res.type_ := TFhirResourceTypesEnum(StringArrayIndexOfSensitive(CODES_TFhirResourceTypesEnum, CODES_TFhirResourceType[r]));
          if form.cbRead.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionRead;
          if form.cbVRead.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionVRead;
          if form.cbSearch.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionSearchType;
          if form.cbCreate.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionCreate;
          if form.cbUpdate.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionUpdate;
          if form.cbDelete.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionDelete;
          if form.cbHistoryInstance.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionHistoryInstance;
          if form.cbHistoryType.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionHistoryType;
          if form.cbPatch.IsChecked then res.interactionList.Append.code := TypeRestfulInteractionPatch;
          if form.cbUpdateCreate.IsChecked then res.updateCreate := true;
          if form.cbCondCreate.IsChecked then res.conditionalCreate := true;
          if form.cbCondUpdate.IsChecked then res.conditionalUpdate := true;
          if form.cbCondDelete.IsChecked then res.conditionalDelete := ConditionalDeleteStatusSingle;
          if form.cbxVersioning.ItemIndex > -1 then
            res.versioning := TFhirVersioningPolicyEnum(form.cbxVersioning.ItemIndex);
          if form.cbxReadCondition.ItemIndex > -1 then
            res.conditionalRead := TFhirConditionalReadStatusEnum(form.cbxReadCondition.ItemIndex);
          if form.cbRefLocal.IsChecked then res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyLocal];
          if form.cbRefEnforced.IsChecked then res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyEnforced];
          if form.cbRefLogical.IsChecked then res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyLogical];
          if form.cbRefResolve.IsChecked then res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyResolves];
          if form.cbRefLiteral.IsChecked then res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyLiteral];
          if form.cbStandardSearch.isChecked then
          begin
            compartments := TFHIRCompartmentList.create;
            indexes := TFhirIndexList.Create(TFHIRFactoryX.create);
            try
              builder := TFHIRIndexBuilderX.Create;
              try
                builder.registerIndexes(indexes, compartments);
              finally
                builder.Free;
              end;
              list := indexes.listByType(CODES_TFhirResourceTypesEnum[res.type_]);
              try
                for index in list do
                begin
                  p := TFhirCapabilityStatementRestResourceSearchParam.Create;
                  try
                    p.name := index.Name;
                    p.type_ := MAP_TFHIRSearchParamType2[index.SearchType];
                    p.definition := index.URI;
                    p.documentation := index.Description;
                    res.searchParamList.Add(p.Link);
                  finally
                    p.Free;
                  end;
                end;
              finally
                list.free;
              end;
            finally
              indexes.Free;
              compartments.Free;
            end;

          end;
          tiRes := TTreeViewItem.Create(tvStructure.Selected);
          tiRes.text := CODES_TFhirResourceTypesEnum[res.type_];
          tvStructure.Selected.AddObject(tiRes);
          tiRes.TagObject := res;
          res.TagObject := tiRes;
        end;
      ResourceIsDirty := true;
    end;
    btnAddOperations.Enabled := rest.resourceList.Count > 0;
    btnRemoveOperations.Enabled := rest.resourceList.Count > 0;
  finally
    form.free;
  end;
end;

procedure TCapabilityStatementEditorFrame.btnAddServerClick(Sender: TObject);
var
  rest : TFhirCapabilityStatementRest;
  tiRest : TTreeViewItem;
begin
  rest := CapabilityStatement.restList.Append;
  rest.mode := RestfulCapabilityModeServer;
  tiRest := TTreeViewItem.Create(tvMetadata);
  tiRest.text := 'Server';
  tvMetadata.AddObject(tiRest);
  tiRest.TagObject := rest;
  rest.TagObject := tiRest;
  btnAddServer.Enabled := false;
  tvStructure.Selected := tiRest;
  tvStructureClick(nil);
  ResourceIsDirty := true;
end;

procedure TCapabilityStatementEditorFrame.btnBatchClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestInteraction;
begin
  ri := edtTransaction.TagObject as TFhirCapabilityStatementRestInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Batch Documentation', btnBatch, edtBatch, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnCancelClick(Sender: TObject);
begin
  cancel;
end;

procedure TCapabilityStatementEditorFrame.btnCopyrightClick(Sender: TObject);
begin
  if CapabilityStatement.copyrightElement = nil then
    CapabilityStatement.copyrightElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Capability Statement copyright', btncopyright, edtcopyright, CapabilityStatement, CapabilityStatement.copyrightElement);
end;

procedure TCapabilityStatementEditorFrame.btnDeleteResourcesClick(Sender: TObject);
var
  form : TListSelectorForm;
  r : TFhirCapabilityStatementRestResource;
  rest : TFhirCapabilityStatementRest;
  i : integer;
begin
  rest := tvStructure.Selected.TagObject as TFhirCapabilityStatementRest;
  form := TListSelectorForm.create(self);
  try
    form.Caption := 'Choose Resources to Delete';
    for r in rest.resourceList do
      form.ListBox1.items.AddObject(CODES_TFHIRResourceTypesEnum[r.type_], r);

    if form.ListBox1.Items.Count = 0 then
      ShowMessage('No resources to delete')
    else if ShowModalHack(form) = mrOk then
    begin
      for i := 0 to form.ListBox1.Items.Count - 1 do
        if form.ListBox1.ListItems[i].IsChecked then
        begin
          r :=  form.ListBox1.Items.Objects[i] as TFhirCapabilityStatementRestResource;
          tvStructure.Selected.RemoveObject(r.TagObject as TFmxObject);
          rest.resourceList.DeleteByReference(r);
        end;
      ResourceIsDirty := true;
    end;
  finally
    form.free;
  end;
end;

procedure TCapabilityStatementEditorFrame.btnParamAddClick(Sender: TObject);
var
  p : TFhirCapabilityStatementRestResourceSearchParam;
  form : TSearchParameterEditorForm;
  res : TFhirCapabilityStatementRestResource;
begin
  res := tvStructure.Selected.TagObject as TFhirCapabilityStatementRestResource;
  p := TFhirCapabilityStatementRestResourceSearchParam.Create;
  try
    form := TSearchParameterEditorForm.create(self);
    try
      form.param := p.link;
      if showModalHack(form) = mrOk then
      begin
        res.searchParamList.Add(p.link);
        gridSearch.RowCount := 0;
        gridSearch.RowCount := res.searchParamList.Count;
        ResourceIsDirty := true;
      end;
    finally
      form.free;
    end;
    lbSearchClick(nil);
  finally
    p.Free;
  end;
end;

procedure TCapabilityStatementEditorFrame.btnParamAddStdClick(Sender: TObject);
var
  res : TFhirCapabilityStatementRestResource;
  form : TListSelectorForm;
  indexes : TFhirIndexList;
  compartments : TFHIRCompartmentList;
  builder : TFHIRIndexBuilder;
  list : TFslList<TFhirIndex>;
  index : TFhirIndex;
  found : boolean;
  p : TFhirCapabilityStatementRestResourceSearchParam;
  i : integer;
begin
  res := tvStructure.Selected.TagObject as TFhirCapabilityStatementRestResource;
  form := TListSelectorForm.create(self);
  try
    form.Caption := 'Choose Standard Parameters';
    compartments := TFHIRCompartmentList.create;
    indexes := TFhirIndexList.Create(TFHIRFactoryX.Create);
    try
      builder := TFHIRIndexBuilderX.Create;
      try
        builder.registerIndexes(indexes, compartments);
      finally
        builder.Free;
      end;
      list := indexes.listByType(CODES_TFhirResourceTypesEnum[res.type_]);
      try
        for index in list do
        begin
          found := false;
          for p in res.searchParamList do
            if p.name = index.Name then
              found := true;
          if not found then
            form.ListBox1.items.AddObject(index.summary, index);
        end;
        if form.ListBox1.Items.Count = 0 then
          ShowMessage('No Standard Search Parameters Left to add')
        else if ShowModalHack(form) = mrOk then
        begin
          for i := 0 to form.ListBox1.Items.Count - 1 do
            if form.ListBox1.ListItems[i].IsChecked then
            begin
              index := form.ListBox1.Items.Objects[i] as TFhirIndex;
              p := TFhirCapabilityStatementRestResourceSearchParam.Create;
              try
                p.name := index.Name;
                p.type_ := MAP_TFHIRSearchParamType2[index.SearchType];
                p.definition := index.URI;
                p.documentation := index.Description;
                res.searchParamList.Add(p.Link);
              finally
                p.Free;
              end;
            end;
          gridSearch.RowCount := res.searchParamList.Count;
          lbSearchClick(nil);
        end;
      finally
        list.free;
      end;
    finally
      indexes.Free;
      compartments.Free;
    end;
  finally
    form.free;
  end;
end;

procedure TCapabilityStatementEditorFrame.btnParamDeleteClick(Sender: TObject);
var
  sp : TFhirCapabilityStatementRestResourceSearchParamList;
  p : TFhirCapabilityStatementRestResourceSearchParam;
  form : TSearchParameterEditorForm;
begin
  if gridSearch.Selected = -1 then
    exit;
  sp := gridSearch.TagObject as TFhirCapabilityStatementRestResourceSearchParamList;
  p := sp[gridSearch.Selected];
  sp.DeleteByReference(p);
  gridSearch.RowCount := 0;
  gridSearch.RowCount := sp.Count;
  ResourceIsDirty := true;
end;

procedure TCapabilityStatementEditorFrame.btnParamEditClick(Sender: TObject);
var
  sp : TFhirCapabilityStatementRestResourceSearchParamList;
  p : TFhirCapabilityStatementRestResourceSearchParam;
  form : TSearchParameterEditorForm;
begin
  if gridSearch.Selected = -1 then
    exit;

  sp := gridSearch.TagObject as TFhirCapabilityStatementRestResourceSearchParamList;
  p := sp[gridSearch.Selected];
  form := TSearchParameterEditorForm.create(self);
  try
    form.param := p.link;
    if showModalHack(form) = mrOk then
    begin
      gridSearch.RowCount := 0;
      gridSearch.RowCount := sp.Count;
      ResourceIsDirty := true;
    end;
  finally
    form.free;
  end;
  lbSearchClick(nil);
end;

procedure TCapabilityStatementEditorFrame.btnRemoveOperationsClick(Sender: TObject);
var
  form : TAddRestOperationForm;
  res : TFhirCapabilityStatementRestResource;
  rest : TFhirCapabilityStatementRest;
begin
  rest := tvStructure.Selected.TagObject as TFhirCapabilityStatementRest;

  form := TAddRestOperationForm.create(self);
  try
    form.cbRead.enabled := false;
    form.cbVRead.enabled := false;
    form.cbSearch.enabled := false;
    form.cbCreate.enabled := false;
    form.cbUpdate.enabled := false;
    form.cbDelete.enabled := false;
    form.cbHistoryInstance.enabled := false;
    form.cbHistoryType.enabled := false;
    form.cbPatch.enabled := false;

    for res in rest.resourceList do
    begin
      if res.interaction(TypeRestfulInteractionRead) <> nil then form.cbRead.Enabled := true;
      if res.interaction(TypeRestfulInteractionVRead) <> nil then form.cbVRead.Enabled := true;
      if res.interaction(TypeRestfulInteractionSearchType) <> nil then form.cbSearch.Enabled := true;
      if res.interaction(TypeRestfulInteractionCreate) <> nil then form.cbCreate.Enabled := true;
      if res.interaction(TypeRestfulInteractionUpdate) <> nil then form.cbUpdate.Enabled := true;
      if res.interaction(TypeRestfulInteractionDelete) <> nil then form.cbDelete.Enabled := true;
      if res.interaction(TypeRestfulInteractionHistoryInstance) <> nil then form.cbHistoryInstance.Enabled := true;
      if res.interaction(TypeRestfulInteractionHistoryType) <> nil then form.cbHistoryType.Enabled := true;
      if res.interaction(TypeRestfulInteractionPatch) <> nil then form.cbPatch.Enabled := true;
    end;
    if showmodalHack(form) = mrOk then
    begin
      for res in rest.resourceList do
      begin
        if form.cbRead.IsChecked then res.removeInteraction(TypeRestfulInteractionRead);
        if form.cbVRead.IsChecked then res.removeInteraction(TypeRestfulInteractionVRead);
        if form.cbSearch.IsChecked then res.removeInteraction(TypeRestfulInteractionSearchType);
        if form.cbCreate.IsChecked then res.removeInteraction(TypeRestfulInteractionCreate);
        if form.cbUpdate.IsChecked then res.removeInteraction(TypeRestfulInteractionUpdate);
        if form.cbDelete.IsChecked then res.removeInteraction(TypeRestfulInteractionDelete);
        if form.cbHistoryInstance.IsChecked then res.removeInteraction(TypeRestfulInteractionHistoryInstance);
        if form.cbHistoryType.IsChecked then res.removeInteraction(TypeRestfulInteractionHistoryType);
        if form.cbPatch.IsChecked then res.removeInteraction(TypeRestfulInteractionPatch);
        if form.cbUpdateCreate.IsChecked then res.updateCreate := false;
        if form.cbCondCreate.IsChecked then res.conditionalCreate := false;
        if form.cbCondUpdate.IsChecked then res.conditionalUpdate := false;
        if form.cbCondDelete.IsChecked then res.conditionalDelete := ConditionalDeleteStatusNotSupported;
      end;
      ResourceIsDirty := true;
    end;
  finally
    form.free;
  end;
end;

procedure TCapabilityStatementEditorFrame.btnSecurityClick(Sender: TObject);
var
  rest: TFhirCapabilityStatementRest;
begin
  rest := mDoco.TagObject as TFhirCapabilityStatementRest;
  if rest.security.descriptionElement = nil then
    rest.security.descriptionElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Security Documentation', btnSecurity, mSecurity, CapabilityStatement, rest.security.descriptionElement);
end;

procedure TCapabilityStatementEditorFrame.btnSystemHistoryClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestInteraction;
begin
  ri := edtTransaction.TagObject as TFhirCapabilityStatementRestInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'System History Documentation', btnSystemHistory, edtSystemHistory, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnSystemSearchClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestInteraction;
begin
  ri := edtTransaction.TagObject as TFhirCapabilityStatementRestInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'System Search Documentation', btnSystemSearch, edtSystemSearch, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnTitleClick(Sender: TObject);
begin
  if CapabilityStatement.titleElement = nil then
    CapabilityStatement.titleElement := TFhirString.Create;
  editStringDialog(self, 'Capability Statement Title', btnTitle, edtTitle, CapabilityStatement, CapabilityStatement.titleElement);
end;

procedure TCapabilityStatementEditorFrame.btnTransactionClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestInteraction;
begin
  ri := edtTransaction.TagObject as TFhirCapabilityStatementRestInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Transaction Documentation', btnTransaction, edtTransaction, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnEditSearchParamCombinationClick(Sender: TObject);
var
  p : TFhirCapabilityStatementRestResource;
  exl : TFsLList<TFHIRExtension>;
  ex : TFHIRExtension;
  form : TSearchParameterCombinationEditorForm;
begin
  if gridSearchCombinations.Selected = -1 then
    exit;

  p := gridSearchCombinations.TagObject as TFhirCapabilityStatementRestResource;
  exl := p.listExtensions('http://hl7.org/fhir/StructureDefinition/capabilitystatement-search-parameter-combination');
  try
    ex := exl[gridSearch.Selected];
    form := TSearchParameterCombinationEditorForm.create(self);
    try
      form.Parameters := p.searchParamList.Link;
      form.extension := ex.link;
      if showModalHack(form) = mrOk then
      begin
        gridSearchCombinations.RowCount := 0;
        gridSearchCombinations.RowCount := exl.Count;
        ResourceIsDirty := true;
      end;
    finally
      form.free;
    end;
  finally
    exl.Free;
  end;
  lbSearchClick(nil);
end;

procedure TCapabilityStatementEditorFrame.btnAddSearchParamCombinationClick(Sender: TObject);
var
  form : TSearchParameterCombinationEditorForm;
  res : TFhirCapabilityStatementRestResource;
  ex : TFHIRExtension;
begin
  res := tvStructure.Selected.TagObject as TFhirCapabilityStatementRestResource;
  ex := TFhirExtension.Create;
  try
    ex.url := 'http://hl7.org/fhir/StructureDefinition/capabilitystatement-search-parameter-combination';
    form := TSearchParameterCombinationEditorForm.create(self);
    try
      form.extension := ex.link;
      form.Parameters := res.searchParamList.Link;
      if showModalHack(form) = mrOk then
      begin
        res.extensionList.Add(ex.link);
        gridSearchCombinations.RowCount := 0;
        gridSearchCombinations.RowCount := res.getExtensionCount('http://hl7.org/fhir/StructureDefinition/capabilitystatement-search-parameter-combination');
        ResourceIsDirty := true;
      end;
    finally
      form.free;
    end;
    lbSearchClick(nil);
  finally
    ex.Free;
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDeleteSearchCombinationClick(Sender: TObject);
var
  sp : TFhirCapabilityStatementRestResource;
  ex : TFHIRExtension;
  form : TSearchParameterEditorForm;
  exl : TFslList<TFHIRExtension>;
begin
  if gridSearchCombinations.Selected = -1 then
    exit;
  sp := gridSearchCombinations.TagObject as TFhirCapabilityStatementRestResource;
  exl := sp.listExtensions('http://hl7.org/fhir/StructureDefinition/capabilitystatement-search-parameter-combination');
  try
    ex := exl[gridSearchCombinations.Selected];
    sp.extensionList.DeleteByReference(ex);
    gridSearchCombinations.RowCount := 0;
    gridSearchCombinations.RowCount := exl.Count-1;
    ResourceIsDirty := true;
  finally
    exl.Free;
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDocoPatchClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestResourceInteraction;
begin
  ri := edtDocoPatch.TagObject as TFhirCapabilityStatementRestResourceInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Patch Documentation', btnDocoPatch, edtDocoPatch, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDocoReadClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestResourceInteraction;
begin
  ri := edtDocoRead.TagObject as TFhirCapabilityStatementRestResourceInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Read Documentation', btnDocoRead, edtDocoRead, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDocoResClick(Sender: TObject);
var
  res: TFhirCapabilityStatementRestResource;
begin
  res := mDocoRes.TagObject as TFhirCapabilityStatementRestResource;
  if res.documentationElement = nil then
    res.documentationElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Resource Use Description', btnDocoRes, mDocoRes, CapabilityStatement, res.documentationElement);
end;

procedure TCapabilityStatementEditorFrame.btnDocoSearchClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestResourceInteraction;
begin
  ri := edtDocoSearch.TagObject as TFhirCapabilityStatementRestResourceInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Search Documentation', btnDocoSearch, edtDocoSearch, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDocoUpdateClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestResourceInteraction;
begin
  ri := edtDocoUpdate.TagObject as TFhirCapabilityStatementRestResourceInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Update Documentation', btnDocoUpdate, edtDocoUpdate, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDocoVReadClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestResourceInteraction;
begin
  ri := edtDocoVRead.TagObject as TFhirCapabilityStatementRestResourceInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Version Read Documentation', btnDocoVRead, edtDocoVRead, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDescriptionClick(Sender: TObject);
begin
  if CapabilityStatement.descriptionElement = nil then
    CapabilityStatement.descriptionElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'ValueSet Description', btnDescription, edtDescription, CapabilityStatement, CapabilityStatement.descriptionElement);
end;

procedure TCapabilityStatementEditorFrame.btnDocoClick(Sender: TObject);
var
  rest: TFhirCapabilityStatementRest;
begin
  rest := mDoco.TagObject as TFhirCapabilityStatementRest;
  if rest.documentationElement = nil then
    rest.documentationElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Capability Statement Documentation', btnDoco, mDoco, CapabilityStatement, rest.documentationElement);
end;

procedure TCapabilityStatementEditorFrame.btnDocoCreateClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestResourceInteraction;
begin
  ri := edtDocoCreate.TagObject as TFhirCapabilityStatementRestResourceInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Create Documentation', btnDocoCreate, edtDocoCreate, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDocoDeleteClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestResourceInteraction;
begin
  ri := edtDocoRead.TagObject as TFhirCapabilityStatementRestResourceInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Delete Documentation', btnDocoDelete, edtDocoDelete, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDocoHistoryInstanceClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestResourceInteraction;
begin
  ri := edtDocoHistoryInstance.TagObject as TFhirCapabilityStatementRestResourceInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Instance History Documentation', btnDocoHistoryInstance, edtDocoHistoryInstance, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnDocoHistoryTypeClick(Sender: TObject);
var
  ri : TFhirCapabilityStatementRestResourceInteraction;
begin
  ri := edtDocoHistoryType.TagObject as TFhirCapabilityStatementRestResourceInteraction;
  if ri <> nil then
  begin
    if ri.documentationElement = nil then
      ri.documentationElement := makeMarkdownOrString;
    editStringOrMarkdownDialog(self, 'Instance History Documentation', btnDocoHistoryType, edtDocoHistoryType, CapabilityStatement, ri.documentationElement);
  end;
end;

procedure TCapabilityStatementEditorFrame.btnPublisherClick(Sender: TObject);
begin
  if CapabilityStatement.publisherElement = nil then
    CapabilityStatement.publisherElement := TFhirString.Create;
  editStringDialog(self, 'Capability Statement Title', btnPublisher, edtPublisher, CapabilityStatement, CapabilityStatement.publisherElement);
end;

procedure TCapabilityStatementEditorFrame.btnPurposeClick(Sender: TObject);
begin
  if CapabilityStatement.purposeElement = nil then
    CapabilityStatement.purposeElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Capability Statement purpose', btnpurpose, edtpurpose, CapabilityStatement, CapabilityStatement.purposeElement);
end;

procedure TCapabilityStatementEditorFrame.cancel;
begin
end;

procedure TCapabilityStatementEditorFrame.commit;
var
  obj : TObject;
begin
  obj := tvStructure.Selected.TagObject;
  if obj is TFhirCapabilityStatement then
    CommitMetadata
  else if obj is TFhirCapabilityStatementRest then
    commitRest(obj as TFhirCapabilityStatementRest)
  else if obj is TFhirCapabilityStatementRestResource then
    commitResource(obj as TFhirCapabilityStatementRestResource);
  ResourceIsDirty := true;
end;

procedure TCapabilityStatementEditorFrame.commitMetadata;
var
  s : String;
  cc : TFHIRCodeableConcept;
begin
  CapabilityStatement.experimental := cbExperimental.IsChecked;
  CapabilityStatement.Format[ffXml] := cbXml.IsChecked;
  CapabilityStatement.Format[ffJson] := cbJson.IsChecked;
  CapabilityStatement.Format[ffTurtle] := cbTurtle.IsChecked;
  CapabilityStatement.instantiates['http://hl7.org/fhir/CapabilityStatement/terminology-server'] := cbTerminologyService.IsChecked;

  CapabilityStatement.url := edtURL.Text;
  CapabilityStatement.name := edtName.Text;
  CapabilityStatement.title := edtTitle.Text;
  CapabilityStatement.fhirVersion := {$IFDEF FHIR3}cbxFHIRVersion.items[cbxFHIRVersion.itemIndex] {$ELSE} TFhirFHIRVersionEnum(cbxFHIRVersion.itemIndex) {$ENDIF};
  CapabilityStatement.version := edtVersion.Text;
  CapabilityStatement.publisher := edtPublisher.text;
  CapabilityStatement.description := edtDescription.Text;
  CapabilityStatement.purpose := edtPurpose.Text;
  CapabilityStatement.copyright := edtCopyright.Text;
  CapabilityStatement.status := TFhirPublicationStatusEnum(cbxStatus.ItemIndex);
  CapabilityStatement.date := TFslDateTime.make(dedDate.DateTime, dttzLocal);
  CapabilityStatement.kind := TFhirCapabilityStatementKindEnum(cbxKind.ItemIndex);
  CapabilityStatement.jurisdictionList.Clear;
  cc := getJurisdiction(cbxJurisdiction.ItemIndex);
  if (cc <> nil) then
    CapabilityStatement.jurisdictionList.add(cc);

  CapabilityStatement.implementationGuideList.Clear;
  for s in mImplementationGuides.Lines do
    CapabilityStatement.implementationGuideList.Append.value := s;
end;

procedure TCapabilityStatementEditorFrame.commitResource(res: TFhirCapabilityStatementRestResource);
  procedure interaction(code : TFhirTypeRestfulInteractionEnum; cb : TCheckBox; edt : TEdit);
  var
    ri : TFhirCapabilityStatementRestResourceInteraction;
  begin
    ri := res.interaction(code);
    if cb.IsChecked then
    begin
      if (ri = nil) then
      begin
        ri := res.interactionList.Append;
        ri.code := code;
      end;
      ri.documentation := edt.Text;
      edt.TagObject := ri;
    end
    else if (ri <> nil) then
      res.interactionList.DeleteByReference(ri);
  end;
begin
  res.documentation := mDocoRes.Text;
  {$IFDEF FHIR4}
  res.profile := edtProfile.Text;
  {$ELSE}
  if edtProfile.Text = '' then
    res.profile := nil
  else
  begin
    if res.profile = nil then
      res.profile := TFhirReference.Create();
    res.profile.reference := edtProfile.Text;
  end;
  {$ENDIF}

  case cbxResourceConformance.ItemIndex of
    1: res.setExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', TFHIRCode.create('SHALL'));
    2: res.setExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', TFHIRCode.create('SHOULD'));
    3: res.setExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', TFHIRCode.create('MAY'));
    4: res.setExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', TFHIRCode.create('SHALL NOT'));
  else
    res.removeExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation')
  end;

  interaction(TypeRestfulInteractionread, cbRead, edtDocoRead);
  interaction(TypeRestfulInteractionVread, cbVRead, edtDocoVRead);
  interaction(TypeRestfulInteractionUpdate, cbUpdate, edtDocoUpdate);
  interaction(TypeRestfulInteractionPatch, cbPatch, edtDocoPatch);
  interaction(TypeRestfulInteractionDelete, cbDelete, edtDocoDelete);
  interaction(TypeRestfulInteractionHistoryInstance, cbHistoryInstance, edtDocoHistoryInstance);
  interaction(TypeRestfulInteractionHistoryType, cbHistoryType, edtDocoHistoryType);
  interaction(TypeRestfulInteractioncreate, cbCreate, edtDocoCreate);
  interaction(TypeRestfulInteractionSearchType, cbSearch, edtDocoSearch);

  res.versioning := TFhirVersioningPolicyEnum(cbxVersioning.ItemIndex);
  res.updateCreate := cbUpdateCreate.IsChecked;
  res.conditionalCreate := cbCondCreate.IsChecked;
  res.conditionalUpdate := cbCondUpdate.IsChecked;
  if cbCondDelete.IsChecked then
    res.conditionalDelete := ConditionalDeleteStatusSingle
  else
    res.conditionalDelete := ConditionalDeleteStatusNotSupported;
  res.conditionalRead := TFhirConditionalReadStatusEnum(cbxReadCondition.ItemIndex);

  if cbRefLiteral.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyLiteral]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyLiteral];
  if cbRefLogical.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyLogical]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyLogical];
  if cbRefResolve.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyResolves]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyResolves];
  if cbRefEnforced.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyEnforced]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyEnforced];
  if cbRefLocal.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyLocal]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyLocal];
end;

procedure TCapabilityStatementEditorFrame.commitRest(rest: TFhirCapabilityStatementRest);
  procedure interaction(code : TFhirSystemRestfulInteractionEnum; cb : TCheckBox; edt : TEdit);
  var
    ri : TFhirCapabilityStatementRestInteraction;
  begin
    ri := rest.interaction(code);
    if cb.IsChecked then
    begin
      if (ri = nil) then
      begin
        ri := rest.interactionList.Append;
        ri.code := code;
      end;
      ri.documentation := edt.Text;
    end
    else if (ri <> nil) then
      rest.interactionList.DeleteByReference(ri);
  end;
begin
  rest.documentation := mDoco.Text;
  if (mSecurity.Text = '') and not (cbCORS.IsChecked or cbOAuth.IsChecked or cbClientCerts.IsChecked or cbSmart.IsChecked) then
    rest.security := nil
  else
  begin
    if rest.security = nil then
      rest.security := TFhirCapabilityStatementRestSecurity.Create;
    rest.security.description := mSecurity.Text;
    rest.security.cors := cbCORS.IsChecked;
    rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'OAuth'] := cbOAuth.IsChecked;
    rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'Certificates'] := cbClientCerts.IsChecked;
    rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'SMART-on-FHIR'] := cbSmart.IsChecked;
  end;
  interaction(SystemRestfulInteractionTransaction, cbTransaction, edtTransaction);
  interaction(SystemRestfulInteractionBatch, cbBatch, edtBatch);
  interaction(SystemRestfulInteractionSearchSystem, cbSystemSearch, edtSystemSearch);
  interaction(SystemRestfulInteractionHistorySystem, cbSystemHistory, edtSystemHistory);
end;

function TCapabilityStatementEditorFrame.GetCapabilityStatement: TFHIRCapabilityStatement;
begin
  result := TFHIRCapabilityStatement(Resource);
end;

function TCapabilityStatementEditorFrame.getJurisdiction(i: integer): TFHIRCodeableConcept;
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

procedure TCapabilityStatementEditorFrame.gridSearchCellDblClick(const Column: TColumn; const Row: Integer);
begin
  btnParamEditClick(self);
end;

procedure TCapabilityStatementEditorFrame.gridSearchCombinationsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  p : TFhirCapabilityStatementRestResource;
  ex : TFHIRExtension;
  exl : TFslList<TFHIRExtension>;
  ts : TStringList;
  i : integer;
begin
  p := gridSearchCombinations.TagObject as TFhirCapabilityStatementRestResource;
  exl := p.listExtensions('http://hl7.org/fhir/StructureDefinition/capabilitystatement-search-parameter-combination');
  try
    ex := exl[ARow];
    case aCol of
      0: value := ex.getExtensionString('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation');
      1: begin
         ts := TStringList.Create;
         try
           for i := 0 to ex.extensionList.Count -1  do
             if ex.extensionList[i].url = 'required' then
               ts.Add(ex.extensionList[i].value.primitiveValue);
           value := ts.CommaText;
         finally
           ts.Free;
         end;
      end;
    end;
  finally
    exl.Free;
  end;
end;

procedure TCapabilityStatementEditorFrame.gridSearchCombinationsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  sp : TFhirCapabilityStatementRestResource;
  ex : TFhirExtension;
  exl : TFslList<TFHIRExtension>;
  ts : TStringList;
  i : integer;
begin
  sp := gridSearchCombinations.TagObject as TFhirCapabilityStatementRestResource;
  exl := sp.listExtensions('http://hl7.org/fhir/StructureDefinition/capabilitystatement-search-parameter-combination');
  try
    ex := exl[ARow];
    case aCol of
      0: ex.setExtensionString('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', value.AsString);
      1: begin
         ts := TStringList.Create;
         try
           ts.CommaText := value.AsString;
           ex.removeExtension('required');
           for i := 0 to ts.Count - 1 do
             ex.addExtension('required', ts[i]);
         finally
           ts.Free;
         end;
      end;
    end;
  finally
    exl.Free;
  end;
  ResourceIsDirty := true;
end;

procedure TCapabilityStatementEditorFrame.gridSearchGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  sp : TFhirCapabilityStatementRestResourceSearchParamList;
  p : TFhirCapabilityStatementRestResourceSearchParam;
begin
  sp := gridSearch.TagObject as TFhirCapabilityStatementRestResourceSearchParamList;
  p := sp[ARow];
  case aCol of
    0: value := p.name;
    1: value := CODES_TFhirSearchParamTypeEnum[p.type_];
    2: value := p.getExtensionString('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation');
    3: value := p.definition;
    4: value := p.documentation;
  end;
end;

procedure TCapabilityStatementEditorFrame.gridSearchSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  sp : TFhirCapabilityStatementRestResourceSearchParamList;
  p : TFhirCapabilityStatementRestResourceSearchParam;
begin
  sp := gridSearch.TagObject as TFhirCapabilityStatementRestResourceSearchParamList;
  p := sp[ARow];
  case aCol of
    0: p.name := value.AsString;
    1: p.type_ := TFhirSearchParamTypeEnum(StringArrayIndexOfSensitive(CODES_TFhirSearchParamTypeEnum, value.AsString));
    2: if value.AsString = '' then
         p.removeExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation')
       else
         p.setExtensionString('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', value.AsString);
    3: p.definition := value.AsString;
    4: p.documentation := value.AsString;
  end;
  ResourceIsDirty := true;
end;

procedure TCapabilityStatementEditorFrame.inputChanged(Sender: TObject);
begin
  if not Loading then
    commit;
end;

procedure TCapabilityStatementEditorFrame.lbSearchClick(Sender: TObject);
begin
//  btnParamEdit.enabled := lbSearch.ItemIndex > -1;
//  btnParamDelete.enabled := lbSearch.ItemIndex > -1;
end;

procedure TCapabilityStatementEditorFrame.load;
var
  rest : TFhirCapabilityStatementRest;
  tiRest, tiRes : TTreeViewItem;
  res : TFhirCapabilityStatementRestResource;
  bClient, bServer : boolean;
begin
  inherited;

  tvMetadata.TagObject := CapabilityStatement;
  bClient := false;
  bServer := false;

  for rest in CapabilityStatement.restList do
  begin
    tiRest := TTreeViewItem.Create(tvMetadata);
    if rest.mode = RestfulCapabilityModeClient then
    begin
      tiRest.text := 'Client';
      bClient := true;
    end
    else
    begin
      tiRest.text := 'Server';
      bServer := true;
    end;
    tvMetadata.AddObject(tiRest);
    tiRest.TagObject := rest;
    rest.TagObject := tiRest;
    for res in rest.resourceList do
    begin
      tiRes := TTreeViewItem.Create(tiRest);
      tiRes.text := CODES_TFhirResourceTypesEnum[res.type_];
      tiRest.AddObject(tiRes);
      tiRes.TagObject := res;
      res.TagObject := tiRes;
    end;

  end;
  btnAddClient.Enabled := not bClient;
  btnAddServer.Enabled := not bServer;

  tvStructure.Selected := tvMetadata;
  tvStructure.ExpandAll;
  tvStructureClick(nil);
end;

procedure TCapabilityStatementEditorFrame.loadMetadata;
var
  url : TFHIRUri;
begin
  cbExperimental.IsChecked := CapabilityStatement.experimental;
  cbXml.IsChecked := CapabilityStatement.Format[ffXml];
  cbJson.IsChecked := CapabilityStatement.Format[ffJson];
  cbTurtle.IsChecked := CapabilityStatement.Format[ffTurtle];
  cbTerminologyService.IsChecked := CapabilityStatement.instantiates['http://hl7.org/fhir/CapabilityStatement/terminology-server'];

  edtURL.Text := CapabilityStatement.url;
  edtName.Text := CapabilityStatement.name;
  edtTitle.Text := CapabilityStatement.title;
  {$IFDEF FHIR3}
  cbxFHIRVersion.itemIndex := cbxFHIRVersion.items.indexOf(CapabilityStatement.fhirVersion);
  {$ELSE}
  cbxFHIRVersion.itemIndex := Ord(CapabilityStatement.fhirVersion);
  {$ENDIF}
  edtVersion.Text := CapabilityStatement.version;
  edtPublisher.text := CapabilityStatement.publisher;
  edtDescription.Text := CapabilityStatement.description;
  edtPurpose.Text := CapabilityStatement.purpose;
  edtCopyright.Text := CapabilityStatement.copyright;
  cbxStatus.ItemIndex := ord(CapabilityStatement.status);
  if CapabilityStatement.dateElement = nil then
    dedDate.Text := ''
  else
    dedDate.DateTime := CapabilityStatement.date.DateTime;
  cbxKind.ItemIndex := ord(CapabilityStatement.kind);
  cbxJurisdiction.ItemIndex := readJurisdiction;

  mImplementationGuides.Text := '';
  for url in CapabilityStatement.implementationGuideList do
    mImplementationGuides.Text := mImplementationGuides.Text + url.value+#13#10;
end;

procedure TCapabilityStatementEditorFrame.loadResource(res: TFhirCapabilityStatementRestResource);
  procedure interaction(code : TFhirTypeRestfulInteractionEnum; cb : TCheckBox; edt : TEdit);
  var
    ri : TFhirCapabilityStatementRestResourceInteraction;
  begin
    ri := res.interaction(code);
    cb.IsChecked := ri <> nil;
    if cb.IsChecked then
    begin
      edt.Text := ri.documentation;
      edt.TagObject := ri;
    end
    else
      edt.Text := '';
  end;
var
  search : TFhirCapabilityStatementRestResourceSearchParam;
  s : String;
begin
  mDocoRes.Text := res.documentation;
  mDocoRes.TagObject := res;
  {$IFDEF FHIR4}
  edtProfile.Text := res.profile;
  {$ELSE}
  if res.profile <> nil then
    edtProfile.Text := res.profile.reference
  else
    edtProfile.Text := '';
  {$ENDIF}

  interaction(TypeRestfulInteractionread, cbRead, edtDocoRead);
  interaction(TypeRestfulInteractionVread, cbVRead, edtDocoVRead);
  interaction(TypeRestfulInteractionUpdate, cbUpdate, edtDocoUpdate);
  interaction(TypeRestfulInteractionPatch, cbPatch, edtDocoPatch);
  interaction(TypeRestfulInteractionDelete, cbDelete, edtDocoDelete);
  interaction(TypeRestfulInteractionHistoryInstance, cbHistoryInstance, edtDocoHistoryInstance);
  interaction(TypeRestfulInteractionHistoryType, cbHistoryType, edtDocoHistoryType);
  interaction(TypeRestfulInteractioncreate, cbCreate, edtDocoCreate);
  interaction(TypeRestfulInteractionSearchType, cbSearch, edtDocoSearch);

  cbxResourceConformance.ItemIndex := 0;
  if res.hasExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation') then
  begin
    s := res.getExtensionString('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation');
    if s = 'SHALL' then
      cbxResourceConformance.ItemIndex := 1;
    if s = 'SHOULD' then
      cbxResourceConformance.ItemIndex := 2;
    if s = 'MAY' then
      cbxResourceConformance.ItemIndex := 3;
    if s = 'SHALL NOT' then
      cbxResourceConformance.ItemIndex := 4;
  end;

  cbxVersioning.ItemIndex := ord(res.versioning);
  cbUpdateCreate.IsChecked := res.updateCreate;
  cbCondCreate.IsChecked := res.conditionalCreate;
  cbCondUpdate.IsChecked := res.conditionalUpdate;
  cbCondDelete.IsChecked := ord(res.conditionalDelete) > 1;
  cbxReadCondition.ItemIndex := ord(res.conditionalRead);
  cbRefLiteral.IsChecked := ReferenceHandlingPolicyLiteral in res.referencePolicy;
  cbRefLogical.IsChecked := ReferenceHandlingPolicyLogical in res.referencePolicy;
  cbRefResolve.IsChecked := ReferenceHandlingPolicyResolves in res.referencePolicy;
  cbRefEnforced.IsChecked := ReferenceHandlingPolicyEnforced in res.referencePolicy;
  cbRefLocal.IsChecked := ReferenceHandlingPolicyLocal in res.referencePolicy;

  gridSearch.tagObject := res.searchParamList;
  gridSearch.RowCount := 0;
  gridSearch.RowCount := res.searchParamList.Count;
  gridSearch.RealignContent;
  gridSearchCombinations.tagObject := res;
  gridSearchCombinations.RowCount := res.getExtensionCount('http://hl7.org/fhir/StructureDefinition/capabilitystatement-search-parameter-combination');
  gridSearchCombinations.RealignContent;
  lbSearchClick(nil);
end;


procedure TCapabilityStatementEditorFrame.loadRest(rest: TFhirCapabilityStatementRest);
  procedure interaction(code : TFhirSystemRestfulInteractionEnum; cb : TCheckBox; edt : TEdit);
  var
    ri : TFhirCapabilityStatementRestInteraction;
  begin
    ri := rest.interaction(code);
    cb.IsChecked := ri <> nil;
    if cb.IsChecked then
    begin
      edt.TagObject := ri;
      edt.Text := ri.documentation;
    end;
  end;
var
  res : TFhirCapabilityStatementRestResource;
  rs : TFhirResourceTypeSet;
  r : TFhirResourceType;
begin
  mDoco.Text := rest.documentation;
  mDoco.TagObject := rest;
  if rest.security <> nil then
  begin
    mSecurity.Text := rest.security.description;
    cbCORS.IsChecked := rest.security.cors;
    cbOAuth.IsChecked := rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'OAuth'];
    cbClientCerts.IsChecked := rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'Certificates'];
    cbSmart.IsChecked := rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'SMART-on-FHIR'];
  end;
  interaction(SystemRestfulInteractionTransaction, cbTransaction, edtTransaction);
  interaction(SystemRestfulInteractionBatch, cbBatch, edtBatch);
  interaction(SystemRestfulInteractionSearchSystem, cbSystemSearch, edtSystemSearch);
  interaction(SystemRestfulInteractionHistorySystem, cbSystemHistory, edtSystemHistory);
  rs := ALL_RESOURCE_TYPES;
  for res in rest.resourceList do
  begin
    r := ResourceTypeByName(CODES_TFhirResourceTypesEnum[res.type_]);
    rs := rs - [r];
  end;
  btnAddResources.Enabled := rs <> [frtCustom];
  btnAddOperations.Enabled := rest.resourceList.Count > 0;
  btnRemoveOperations.Enabled := rest.resourceList.Count > 0;
end;

function TCapabilityStatementEditorFrame.readJurisdiction: Integer;
var
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
begin
  result := -1;
  for cc in CapabilityStatement.jurisdictionList do
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


procedure TCapabilityStatementEditorFrame.tvStructureClick(Sender: TObject);
var
  obj : TObject;
begin
  Loading := true;
  try
    obj := tvStructure.Selected.TagObject;
    if obj is TFhirCapabilityStatement then
    begin
      tbMetadata.TagObject := obj;
      tbStructure.ActiveTab := tbMetadata;
      loadMetadata;
    end
    else if obj is TFhirCapabilityStatementRest then
    begin
      tbRest.TagObject := obj;
      tbStructure.ActiveTab := tbRest;
      loadRest(obj as TFhirCapabilityStatementRest);
    end
    else if obj is TFhirCapabilityStatementRestResource then
    begin
      tbResource.TagObject := obj;
      tbStructure.ActiveTab := tbResource;
      loadResource(obj as TFhirCapabilityStatementRestResource);
    end
  finally
    Loading := false;
  end;
end;

end.
