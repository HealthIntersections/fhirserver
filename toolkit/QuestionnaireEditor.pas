unit QuestionnaireEditor;

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
  FMX.Grid.Style, FMX.Grid, FMX.Menus,FMX.WebBrowser,
  System.ImageList, FMX.ImgList, FMX.Effects, FMX.Filter.Effects,
  fsl_base, fsl_utilities, fsl_stream, FHIR.Ui.Fmx,
  fhir_objects, FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Utilities, fhir_indexing, FHIR.Version.IndexInfo,
  FHIR.Version.Questionnaire2,
  BaseResourceFrame,
  ToolkitUtilities, QuestionnaireItemDialog, MemoEditorDialog, QuestionnairePanel, TranslationsEditorDialog, UsageContextForm, QuestionnaireContextDialog;

type
  TFrame = TBaseResourceFrame; // re-aliasing the Frame to work around a designer bug

  TQuestionnaireEditorFrame = class(TFrame)
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    tvStructure: TTreeView;
    Label1: TLabel;
    tvMetadata: TTreeViewItem;
    tbStructure: TTabControl;
    tbMetadata: TTabItem;
    ScrollBox1: TScrollBox;
    dedDate: TDateEdit;
    edtCopyright: TEdit;
    edtDescription: TEdit;
    edtName: TEdit;
    edtPublisher: TEdit;
    edtPurpose: TEdit;
    edtTitle: TEdit;
    edtURL: TEdit;
    edtVersion: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    btnCopyright: TButton;
    btnDescription: TButton;
    btnPurpose: TButton;
    cbExperimental: TCheckBox;
    cbxJurisdiction: TComboBox;
    cbxStatus: TComboBox;
    tvGrid: TTreeViewItem;
    tbGrid: TTabItem;
    Panel6: TPanel;
    tvContext: TTreeViewItem;
    tbContext: TTabItem;
    Label14: TLabel;
    dedApproval: TDateEdit;
    Label15: TLabel;
    dedReview: TDateEdit;
    dedBegin: TDateEdit;
    Label16: TLabel;
    dedEnd: TDateEdit;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    lbResources: TListBox;
    grdUseContext: TGrid;
    grdItems: TGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    PopupColumn1: TPopupColumn;
    CheckColumn1: TCheckColumn;
    CheckColumn2: TCheckColumn;
    CheckColumn3: TCheckColumn;
    btnAddItem: TButton;
    btnAddChildItem: TButton;
    btnEditItem: TButton;
    btnItemUp: TButton;
    btnItemDown: TButton;
    btnItemIn: TButton;
    btnItemOut: TButton;
    btnDeleteItem: TButton;
    Panel8: TPanel;
    Label20: TLabel;
    edtSearch: TEdit;
    btnSearchStart: TButton;
    btnSearchNext: TButton;
    btnSearchPrev: TButton;
    btnSearchEnd: TButton;
    CheckBox1: TCheckBox;
    tvForm: TTreeViewItem;
    tbForm: TTabItem;
    ImageList1: TImageList;
    ToolbarImages: TImageList;
    btnPublisher: TButton;
    btnTitle: TButton;
    btnName: TButton;
    Panel1: TPanel;
    btnAddQuestionItem: TButton;
    Label12: TLabel;
    ComboBox1: TComboBox;
    cbSDC: TCheckBox;
    Label22: TLabel;
    edtSDCEndPoint: TEdit;
    cbProvenanceSignatureRequired: TCheckBox;
    cbStyleSensitive: TCheckBox;
    Label23: TLabel;
    Label24: TLabel;
    edtSDCsourceStructureMap: TEdit;
    Label25: TLabel;
    edtSDCtargetStructureMap: TEdit;
    Label26: TLabel;
    Label27: TLabel;
    edtSDCIdSystem: TEdit;
    Label28: TLabel;
    edtSDCIdValue: TEdit;
    tvSDC: TTreeViewItem;
    tbSDC: TTabItem;
    ScrollBox2: TScrollBox;
    Label21: TLabel;
    Label29: TLabel;
    edtSDCTitleStyle: TEdit;
    edtSDCTitleXtml: TEdit;
    Label30: TLabel;
    btnAddCode: TButton;
    btnEditCode: TButton;
    btnRemoveCode: TButton;
    gcUsageCode: TStringColumn;
    gcUsageValue: TStringColumn;
    Label31: TLabel;
    gridContext: TGrid;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    StringColumn9: TStringColumn;
    btnClone: TButton;
    procedure tvStructureClick(Sender: TObject);
    procedure inputChanged(Sender: TObject);
    procedure btnDescriptionClick(Sender: TObject);
    procedure btnPurposeClick(Sender: TObject);
    procedure btnCopyrightClick(Sender: TObject);
    procedure grdItemsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure btnAddItemClick(Sender: TObject);
    procedure btnEditItemClick(Sender: TObject);
    procedure grdItemsCellDblClick(const Column: TColumn; const Row: Integer);
    procedure grdItemsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnAddChildItemClick(Sender: TObject);
    procedure btnItemUpClick(Sender: TObject);
    procedure btnItemDownClick(Sender: TObject);
    procedure btnItemInClick(Sender: TObject);
    procedure btnItemOutClick(Sender: TObject);
    procedure grdItemsSelChanged(Sender: TObject);
    procedure btnDeleteItemClick(Sender: TObject);
    procedure lbResourcesChangeCheck(Sender: TObject);
    procedure btnPublisherClick(Sender: TObject);
    procedure btnTitleClick(Sender: TObject);
    procedure btnNameClick(Sender: TObject);
    procedure btnAddQuestionItemClick(Sender: TObject);
    procedure grdUseContextGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure btnAddCodeClick(Sender: TObject);
    procedure btnEditCodeClick(Sender: TObject);
    procedure btnRemoveCodeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure gridContextGetValue(Sender: TObject; const ACol, ARow: Integer;
      var Value: TValue);
    procedure btnCloneClick(Sender: TObject);
  private
    flatItems : TFslList<TFhirQuestionnaireItem>;
    FLoading : boolean;
    FSelected : TFhirQuestionnaireItem;
    FPanel : TQuestionnairePanel;
    function GetQuestionnaire: TFHIRQuestionnaire;
    function readJurisdiction : Integer;
    function getJurisdiction(i : integer) : TFHIRCodeableConcept;
    procedure addToItems(level : integer; list : TFhirQuestionnaireItemList);
    function renderItem(item : TFhirQuestionnaireItem) : String;

    function findItem(sel : TFhirQuestionnaireItem; var parent : TFhirQuestionnaireItem; var list : TFhirQuestionnaireItemList; var index : integer) : boolean;
    procedure updateStatus(sel : TFhirQuestionnaireItem);

    procedure loadMetadata;
    procedure loadSDC;
    procedure loadContext;
    procedure loadGrid;
    procedure loadForm;

    procedure commitMetadata;
    procedure commitSDC;
//    procedure commitGrid;
//    procedure commitItems;
  public
    constructor Create(owner : TComponent); override;
    destructor Destroy; override;

    property Questionnaire : TFHIRQuestionnaire read GetQuestionnaire;
    procedure load; override;

    procedure commit; override;
    procedure cancel; override;
  end;

implementation

{$R *.fmx}

{ TValueSetEditorFrame }

procedure TQuestionnaireEditorFrame.addToItems(level : integer; list: TFhirQuestionnaireItemList);
var
  item : TFhirQuestionnaireItem;
begin
  for item in list do
  begin
    item.TagInt := level;
    flatItems.Add(item.Link);
    addToItems(level + 1, item.itemList);
  end;
end;

procedure TQuestionnaireEditorFrame.btnAddChildItemClick(Sender: TObject);
var
  p, c : TFhirQuestionnaireItem;
begin
  if grdItems.Row = -1 then
  begin
    btnAddItemClick(self);
    exit;
  end;

  grdItems.BeginUpdate;
  p := flatItems[grdItems.Row];
  c := p.itemList.Append;
  c.linkId := 'i'+inttostr(flatItems.Count + 1);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  grdItems.EndUpdate;
  grdItems.RowCount := flatItems.Count;
  ResourceIsDirty := true;
  grdItems.SelectCell(1, flatItems.IndexOf(c));
  btnEditItemClick(nil);
end;

procedure TQuestionnaireEditorFrame.btnAddCodeClick(Sender: TObject);
var
  dlg : TUsageContextDialog;
begin
  dlg := TUsageContextDialog.create(nil);
  try
    dlg.UsageContext := TFhirUsageContext.Create;
    dlg.Settings := Settings.link;
    if ShowModalHack(dlg) = mrOk then
    begin
      Questionnaire.useContextList.Add(dlg.UsageContext.Link);
      grdUseContext.RowCount := Questionnaire.useContextList.Count;
    end;
  finally
    dlg.free;
  end;
end;

procedure TQuestionnaireEditorFrame.btnAddItemClick(Sender: TObject);
var
  c : TFhirQuestionnaireItem;
begin
  grdItems.BeginUpdate;
  c := Questionnaire.itemList.Append;
  c.linkId := 'i'+inttostr(flatItems.Count + 1);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  grdItems.EndUpdate;
  grdItems.RowCount := flatItems.Count;
  grdItems.SelectCell(1, flatItems.IndexOf(c));
  ResourceIsDirty := true;
  btnEditItemClick(nil);
end;

procedure TQuestionnaireEditorFrame.btnDeleteItemClick(Sender: TObject);
var
  sel, nf, parent : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  index, count : integer;
  s : String;
begin
  if grdItems.Row = -1 then
    exit;
  sel := flatItems[grdItems.Row];
  if not findItem(sel, parent, list, index) then
    exit;

  count := sel.countDescendents;
  if count = 0 then
    s := 'Delete Item '+sel.linkId+'?'
  else if count = 1 then
    s := 'Delete Item '+sel.linkId+' and 1 child?'
  else
    s := 'Delete Item '+sel.linkId+' and it''s '+inttostr(count)+' children?';

  TDialogService.MessageDialog(s, TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult)
    begin
      if AResult = mrYes then
      begin
        if index > 0 then
          nf := list[index - 1]
        else if list.Count > 1 then
          nf := list[index + 1]
        else
          nf := nil;
        grdItems.BeginUpdate;
        list.Remove(index);
        flatItems.Clear;
        addToItems(0, Questionnaire.itemList);
        grdItems.RowCount := flatItems.Count;
        grdItems.EndUpdate;
        if nf <> nil then
          grdItems.SelectCell(1, flatItems.IndexOf(nf));
        ResourceIsDirty := true;
      end;
    end
  );
end;

procedure TQuestionnaireEditorFrame.btnItemDownClick(Sender: TObject);
var
  sel : TFhirQuestionnaireItem;
  parent : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  index : integer;
begin
  if grdItems.Row = -1 then
    exit;
  sel := flatItems[grdItems.Row];
  if not findItem(sel, parent, list, index) then
    exit;

  grdItems.BeginUpdate;
  list.Exchange(index, index+1);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  grdItems.EndUpdate;
  grdItems.SelectCell(1, flatItems.IndexOf(sel));
  ResourceIsDirty := true;
end;

procedure TQuestionnaireEditorFrame.btnItemInClick(Sender: TObject);
var
  sel : TFhirQuestionnaireItem;
  parent, grandparent : TFhirQuestionnaireItem;
  list, gList : TFhirQuestionnaireItemList;
  index, gIndex : integer;
begin
  if grdItems.Row = -1 then
    exit;
  sel := flatItems[grdItems.Row];
  if not findItem(sel, parent, list, index) then
    exit;
  if parent = nil then
    exit;
  if not findItem(parent, grandparent, glist, gindex) then
    exit;

  grdItems.BeginUpdate;
  glist.InsertItem(gindex+1, sel.Link);
  list.DeleteByIndex(index);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  grdItems.EndUpdate;
  grdItems.SelectCell(1, flatItems.IndexOf(sel));
  ResourceIsDirty := true;
end;

procedure TQuestionnaireEditorFrame.btnItemOutClick(Sender: TObject);
var
  sel : TFhirQuestionnaireItem;
  parent : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  index : integer;
begin
  if grdItems.Row = -1 then
    exit;
  sel := flatItems[grdItems.Row];
  if not findItem(sel, parent, list, index) then
    exit;
  if index = 0 then
    exit;

  grdItems.BeginUpdate;
  parent := list[index - 1];
  parent.itemList.add(sel.Link);
  list.DeleteByIndex(index);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  grdItems.EndUpdate;
  grdItems.SelectCell(1, flatItems.IndexOf(sel));
  ResourceIsDirty := true;
end;

procedure TQuestionnaireEditorFrame.btnItemUpClick(Sender: TObject);
var
  sel : TFhirQuestionnaireItem;
  parent : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  index : integer;
begin
  if grdItems.Row = -1 then
    exit;
  sel := flatItems[grdItems.Row];
  if not findItem(sel, parent, list, index) then
    exit;

  grdItems.BeginUpdate;
  list.Exchange(index, index-1);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  grdItems.EndUpdate;
  grdItems.SelectCell(1, flatItems.IndexOf(sel));
  ResourceIsDirty := true;
end;

procedure TQuestionnaireEditorFrame.btnNameClick(Sender: TObject);
begin
  if Questionnaire.nameElement = nil then
    Questionnaire.nameElement := TFhirString.Create;
  editStringDialog(self, 'Questionnaire Name', btnName, edtName, Questionnaire, Questionnaire.nameElement);
end;

procedure TQuestionnaireEditorFrame.btnEditCodeClick(Sender: TObject);
var
  dlg : TUsageContextDialog;
  i : integer;
begin
  i := grdUseContext.Row;
  if i < 0 then
    i := 0;

  dlg := TUsageContextDialog.create(nil);
  try
    dlg.UsageContext := Questionnaire.useContextList[i].Clone;
    dlg.Settings := Settings.link;
    if ShowModalHack(dlg) = mrOk then
    begin
      Questionnaire.useContextList[i].assign(dlg.UsageContext);
      grdUseContext.RowCount := 0;
      grdUseContext.RowCount := Questionnaire.useContextList.Count;
      grdUseContext.Row := i;
    end;
  finally
    dlg.free;
  end;
end;

procedure TQuestionnaireEditorFrame.btnEditItemClick(Sender: TObject);
var
  form : TQuestionnaireItemForm;
  item : TFHIRQuestionnaireItem;
begin
  form := TQuestionnaireItemForm.Create(self);
  try
    item := flatItems[grdItems.Row];
    form.item := item.clone;
    form.questionnaire := questionnaire.Link;
    form.Settings := Settings.link;
    form.OnWork := OnWork;
    if ShowModalHack(form) = mrOk then
    begin
      grdItems.BeginUpdate;
      item.required := form.item.required;
      item.repeats := form.item.repeats;
      item.readOnly := form.item.readOnly;
      item.linkId := form.item.linkId;
      item.definition := form.item.definition;
      item.prefix := form.item.prefix;
      item.text := form.item.text;
      item.type_ := form.item.type_;
      item.maxLength := form.item.maxLength;
      item.options := form.item.options{$IFNDEF FHIR4}.Link{$ENDIF};
      item.extensionList.Assign(form.item.extensionList);
      item.enableWhenList.Assign(form.item.enableWhenList);
      grdItems.EndUpdate;
      ResourceIsDirty := true;
    end;
  finally
    form.Free;
  end;
end;

procedure TQuestionnaireEditorFrame.btnCloneClick(Sender: TObject);
var
  c : TFhirQuestionnaireItem;
begin
  if grdItems.Row = -1 then
    Abort;

  grdItems.BeginUpdate;
  c := Questionnaire.itemList[grdItems.Row].Clone;
  Questionnaire.itemList.Add(c);
  c.linkId := 'i'+inttostr(flatItems.Count + 1);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  grdItems.EndUpdate;
  grdItems.RowCount := flatItems.Count;
  ResourceIsDirty := true;
  grdItems.SelectCell(1, flatItems.IndexOf(c));
  btnEditItemClick(nil);
end;

procedure TQuestionnaireEditorFrame.btnCopyrightClick(Sender: TObject);
begin
  if Questionnaire.copyrightElement = nil then
    Questionnaire.copyrightElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Questionnaire Copyright', btnCopyright, edtCopyright, Questionnaire, Questionnaire.copyrightElement);
end;

procedure TQuestionnaireEditorFrame.btnDescriptionClick(Sender: TObject);
begin
  if Questionnaire.descriptionElement = nil then
    Questionnaire.descriptionElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Questionnaire Description', btnDescription, edtDescription, Questionnaire, Questionnaire.descriptionElement);
end;

procedure TQuestionnaireEditorFrame.btnPublisherClick(Sender: TObject);
begin
  if Questionnaire.publisherElement = nil then
    Questionnaire.publisherElement := TFhirMarkdown.Create;
  editStringDialog(self, 'Questionnaire Publisher', btnPublisher, edtPublisher, Questionnaire, Questionnaire.publisherElement);
end;

procedure TQuestionnaireEditorFrame.btnPurposeClick(Sender: TObject);
begin
  if Questionnaire.purposeElement = nil then
    Questionnaire.purposeElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Questionnaire Purpose', btnPurpose, edtPurpose, Questionnaire, Questionnaire.purposeElement);
end;

procedure TQuestionnaireEditorFrame.btnRemoveCodeClick(Sender: TObject);
var
  i : integer;
begin
  i := grdUseContext.Row;
  if i >= 0 then
  begin
    if FMX.Dialogs.MessageDlg('Delete '+gen(Questionnaire.useContextList[i].code)+' : '+gen(Questionnaire.useContextList[i].value),
       TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      Questionnaire.useContextList.Remove(i);
      grdUseContext.RowCount := 0;
      grdUseContext.RowCount := Questionnaire.useContextList.Count;
      grdUseContext.Row := i-1;
    end;
  end;
end;

procedure TQuestionnaireEditorFrame.btnTitleClick(Sender: TObject);
begin
  if Questionnaire.titleElement = nil then
    Questionnaire.titleElement := TFhirString.Create;
  editStringDialog(self, 'Questionnaire Title', btnTitle, edtTitle, Questionnaire, Questionnaire.titleElement);
end;

procedure TQuestionnaireEditorFrame.Button1Click(Sender: TObject);
var
  dlg : TQuestionnaireContextForm;
begin
  dlg := TQuestionnaireContextForm.create(nil);
  try
    dlg.Context := TFHIRExtension.Create;
    dlg.Context.url := 'http://hl7.org/fhir/StructureDefinition/questionnaire-context';
    if ShowModalHack(dlg) = mrOk then
    begin
      Questionnaire.extensionList.Add(dlg.Context.Link);
      grdUseContext.RowCount := Questionnaire.GetExtensionCount('http://hl7.org/fhir/StructureDefinition/questionnaire-context');
    end;
  finally
    dlg.free;
  end;
end;

procedure TQuestionnaireEditorFrame.Button2Click(Sender: TObject);
var
  dlg : TQuestionnaireContextForm;
  i : integer;
  ext : TFhirExtension;
begin
  i := gridContext.Row;
  if i < 0 then
    i := 0;

  ext := Questionnaire.getExtension('http://hl7.org/fhir/StructureDefinition/questionnaire-context', i);
  dlg := TQuestionnaireContextForm.create(nil);
  try
    dlg.Context := ext.Clone;
    if ShowModalHack(dlg) = mrOk then
    begin
      ext.Assign(dlg.Context);
      grdUseContext.RowCount := 0;
      grdUseContext.RowCount := Questionnaire.getExtensionCount('http://hl7.org/fhir/StructureDefinition/questionnaire-context');
      grdUseContext.Row := i;
    end;
  finally
    dlg.free;
  end;
end;

function genContext (ext : TFHIRExtension) : String;
begin
  result := ext.getExtensionByUrl('name').value.primitiveValue;
end;

procedure TQuestionnaireEditorFrame.Button3Click(Sender: TObject);
var
  i : integer;
  ext : TFhirExtension;
begin
  i := gridContext.Row;
  ext := Questionnaire.getExtension('http://hl7.org/fhir/StructureDefinition/questionnaire-context', i);
  if i >= 0 then
  begin
    if FMX.Dialogs.MessageDlg('Delete '+genContext(ext)+'?', TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      Questionnaire.RemoveExtension('http://hl7.org/fhir/StructureDefinition/questionnaire-context', i);
      grdUseContext.RowCount := 0;
      grdUseContext.RowCount := Questionnaire.getExtensionCount('http://hl7.org/fhir/StructureDefinition/questionnaire-context');
      grdUseContext.Row := i-1;
    end;
  end;
end;

procedure TQuestionnaireEditorFrame.btnAddQuestionItemClick(Sender: TObject);
begin
  ResourceIsDirty := true;
  FPanel.build;
  FPanel.AddItem(nil);
end;

procedure TQuestionnaireEditorFrame.cancel;
begin
end;

procedure TQuestionnaireEditorFrame.commit;
begin
  if tvStructure.Selected = tvMetadata then
    CommitMetadata;
  if tvStructure.Selected = tvSDC then
    CommitSDC;
  ResourceIsDirty := true;
end;

procedure TQuestionnaireEditorFrame.commitMetadata;
var
  s : String;
  cc : TFHIRCodeableConcept;
  function makeDT(ded : TDateEdit) : TFslDateTime;
  begin
    if ded.Text = '' then
      result := TFslDateTime.makeNull
    else
      result := TFslDateTime.makeLocal(ded.Date);
  end;
begin
  if Loading then
    exit;
  Questionnaire.experimental := cbExperimental.IsChecked;

  Questionnaire.url := edtURL.Text;
  Questionnaire.name := edtName.Text;
  Questionnaire.title := edtTitle.Text;
  Questionnaire.version := edtVersion.Text;
  Questionnaire.publisher := edtPublisher.text;
  Questionnaire.description := edtDescription.Text;
  Questionnaire.purpose := edtPurpose.Text;
  Questionnaire.copyright := edtCopyright.Text;
  Questionnaire.status := TFhirPublicationStatusEnum(cbxStatus.ItemIndex);
  Questionnaire.date := makeDT(dedDate);
  Questionnaire.approvalDate := makeDT(dedApproval);
  Questionnaire.lastReviewDate := makeDT(dedReview);
  if (dedBegin.Text = '') and (dedEnd.Text = '') then
    Questionnaire.effectivePeriod := nil
  else
  begin
    if Questionnaire.effectivePeriod = nil then
      Questionnaire.effectivePeriod := TFhirPeriod.Create;
    Questionnaire.effectivePeriod.start := makeDT(dedBegin);
    Questionnaire.effectivePeriod.end_ := makeDT(dedEnd);
  end;
  Questionnaire.jurisdictionList.Clear;

  cc := getJurisdiction(cbxJurisdiction.ItemIndex);
  if (cc <> nil) then
    Questionnaire.jurisdictionList.add(cc);
end;

procedure TQuestionnaireEditorFrame.commitSDC;
begin
  if cbSDC.IsChecked then
  begin
    if Questionnaire.meta = nil then
      Questionnaire.meta := TFhirMeta.Create;
    Questionnaire.meta.addProfile('http://hl7.org/fhir/us/sdc/StructureDefinition/sdc-questionnaire');
  end
  else if Questionnaire.meta <> nil then
    Questionnaire.meta.dropProfile('http://hl7.org/fhir/us/sdc/StructureDefinition/sdc-questionnaire');
  writeExtension(Questionnaire, 'http://hl7.org/fhir/us/sdc/StructureDefinition/sdc-questionnaire-endpoint', edtSDCEndPoint.Text, TFhirUri);
  if cbProvenanceSignatureRequired.IsChecked then
    writeExtension(Questionnaire, 'http://hl7.org/fhir/us/sdc/StructureDefinition/sdc-questionnaire-provenanceSignatureRequired', 'true', TFhirBoolean)
  else
    writeExtension(Questionnaire, 'http://hl7.org/fhir/us/sdc/StructureDefinition/sdc-questionnaire-provenanceSignatureRequired', '', TFhirBoolean);
  if cbStyleSensitive.IsChecked then
    writeExtension(Questionnaire, 'http://hl7.org/fhir/StructureDefinition/rendering-styleSensitive', 'true', TFhirBoolean)
  else
    writeExtension(Questionnaire, 'http://hl7.org/fhir/StructureDefinition/rendering-styleSensitive', '', TFhirBoolean);
  writeExtension(Questionnaire, 'http://hl7.org/fhir/StructureDefinition/questionnaire-sourceStructureMap', edtSDCsourceStructureMap.Text, TFhirCanonical);
  writeExtension(Questionnaire, 'http://hl7.org/fhir/StructureDefinition/questionnaire-targetStructureMap', edtSDCtargetStructureMap.Text, TFhirCanonical);

  if (edtSDCIdSystem.Text <> '') or (edtSDCIdValue.Text <> '') then
    writeExtension(Questionnaire, 'http://hl7.org/fhir/StructureDefinition/questionnaire-studyProtocolIdentifier',
      TFhirIdentifier.Create(edtSDCIdSystem.Text, edtSDCIdValue.Text))
  else
    writeExtension(Questionnaire, 'http://hl7.org/fhir/StructureDefinition/questionnaire-studyProtocolIdentifier', nil);

  if (Questionnaire.titleElement = nil) then
    Questionnaire.titleElement := TFhirString.Create;
  writeExtension(Questionnaire.titleElement, 'http://hl7.org/fhir/StructureDefinition/rendering-style', edtSDCTitleStyle.Text, TFHIRString);
  writeExtension(Questionnaire.titleElement, 'http://hl7.org/fhir/StructureDefinition/rendering-xhtml', edtSDCTitleXtml.Text, TFHIRString);
end;

constructor TQuestionnaireEditorFrame.Create(owner: TComponent);
begin
  inherited;
  flatItems := TFslList<TFhirQuestionnaireItem>.create;
end;

destructor TQuestionnaireEditorFrame.Destroy;
begin
  flatItems.Free;
  inherited;
end;

function TQuestionnaireEditorFrame.findItem(sel: TFhirQuestionnaireItem; var parent : TFhirQuestionnaireItem; var list: TFhirQuestionnaireItemList; var index: integer): boolean;
var
  i : integer;
begin
  parent := nil;
  list := nil;
  index := -1;
  if (sel = nil) then
    exit(false);

  if sel.TagInt = 0 then
    list := Questionnaire.itemList
  else
  begin
    i := flatItems.IndexOf(sel);
    while flatItems[i].TagInt >= sel.TagInt do
      dec(i);
    parent := flatItems[i];
    list := flatItems[i].itemList;
  end;
  index := list.IndexOf(sel);
  result := index > -1;
end;

function TQuestionnaireEditorFrame.GetQuestionnaire: TFHIRQuestionnaire;
begin
  result := TFHIRQuestionnaire(Resource);
end;

procedure TQuestionnaireEditorFrame.grdItemsCellDblClick(const Column: TColumn; const Row: Integer);
begin
  btnEditItemClick(nil);
end;

procedure TQuestionnaireEditorFrame.grdItemsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  item : TFhirQuestionnaireItem;
begin
  item := flatItems[aRow];
  case aCol of
    0: value := item.TagInt;
    1: value := item.linkId;
    2: value := item.definition;
    3: value := ''; //todo: codes
    4: value := item.prefix;
    5: value := item.text;
    6: value := CODES_TFhirItemTypeEnum[item.type_];
    7: value := item.required;
    8: value := item.repeats;
    9: value := item.readOnly;
  end;
end;

procedure TQuestionnaireEditorFrame.grdItemsSelChanged(Sender: TObject);
begin
  if grdItems.Row < 0 then
  begin
    updateStatus(nil);
    FSelected := nil;
  end
  else
  begin
    FSelected := flatItems[grdItems.Row];
    updateStatus(FSelected);
  end;
end;

procedure TQuestionnaireEditorFrame.grdItemsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  item : TFhirQuestionnaireItem;
begin
  item := flatItems[aRow];
  case aCol of
    1: item.linkId := value.AsString;
    2: item.definition := value.AsString;
    3: ; // value := ''; //todo: codes
    4: item.prefix := value.AsString;
    5: item.text := value.AsString;
    6: item.type_ := TFhirItemTypeEnum(StringArrayIndexOfSensitive(CODES_TFhirItemTypeEnum, value.AsString));
    7: item.required := value.AsBoolean;
    8: item.repeats := value.AsBoolean;
    9: item.readOnly := value.AsBoolean;
  end;
  ResourceIsDirty := true;
end;

procedure TQuestionnaireEditorFrame.grdUseContextGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  uc : TFhirUsageContext;
begin
  uc := Questionnaire.useContextList[aRow];
  case ACol of
    0 :
      if uc.code.hasCode('http://hl7.org/fhir/usage-context-type', 'gender') then
        value := 'Gender'
      else if uc.code.hasCode('http://hl7.org/fhir/usage-context-type', 'age') then
        value := 'Age'
      else if uc.code.hasCode('http://hl7.org/fhir/usage-context-type', 'focus') then
        value := 'Focus'
      else if uc.code.hasCode('http://hl7.org/fhir/usage-context-type', 'user') then
        value := 'User Type'
      else if uc.code.hasCode('http://hl7.org/fhir/usage-context-type', 'workflow') then
        value := 'Workflow Context'
      else if uc.code.hasCode('http://hl7.org/fhir/usage-context-type', 'task') then
        value := 'Task Type'
      else if uc.code.hasCode('http://hl7.org/fhir/usage-context-type', 'venue') then
        value := 'Venue'
      else if uc.code.hasCode('http://hl7.org/fhir/usage-context-type', 'species') then
        value := 'Species'
      else
        value := gen(uc.code);
    1 : value := gen(uc.value);
  end;
end;

procedure TQuestionnaireEditorFrame.gridContextGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  ext : TFhirExtension;
begin
  ext := Questionnaire.getExtension('http://hl7.org/fhir/StructureDefinition/questionnaire-context', aRow);
  case ACol of
    0 : if ext.hasExtension('name') then
          value := ext.getExtensionByUrl('name')
        else
          value := '';
    1 : if ext.hasExtension('type') then
          value := ext.getExtensionByUrl('type')
        else
          value := '';
    2 : if ext.hasExtension('description') then
          value := ext.getExtensionByUrl('description')
        else
          value := '';
  end;
end;

function displayLang(lang : String) : string;
begin
  if lang = '' then
    result := ''
  else if lang.StartsWith('bn') then result := 'bn (Bengali)'
  else if lang.startsWith('cs') then result := 'cs (Czech)'
  else if lang.startsWith('da') then result := 'da (Danish)'
  else if lang.startsWith('de') then result := 'de (German)'
  else if lang.startsWith('el') then result := 'el (Greek)'
  else if lang.startsWith('en') then result := 'en (English)'
  else if lang.startsWith('es') then result := 'es (Spanish)'
  else if lang.startsWith('fi') then result := 'fi (Finnish)'
  else if lang.startsWith('fr') then result := 'fr (French)'
  else if lang.startsWith('fy') then result := 'fy (Frysian)'
  else if lang.startsWith('hi') then result := 'hi (Hindi)'
  else if lang.startsWith('hr') then result := 'hr (Croatian)'
  else if lang.startsWith('it') then result := 'it (Italian)'
  else if lang.startsWith('ja') then result := 'ja (Japanese)'
  else if lang.startsWith('ko') then result := 'ko (Korean)'
  else if lang.startsWith('nl') then result := 'nl (Dutch)'
  else if lang.startsWith('no') then result := 'no (Norwegian)'
  else if lang.startsWith('pa') then result := 'pa (Punjabi)'
  else if lang.startsWith('pt') then result := 'pt (Portuguese)'
  else if lang.startsWith('ru') then result := 'ru (Russian)'
  else if lang.startsWith('sr') then result := 'sr (Serbian)'
  else if lang.startsWith('sv') then result := 'sv (Swedish)'
  else if lang.startsWith('te') then result := 'te (Telegu)'
  else if lang.startsWith('zh') then result := 'zh (Chinese))'
  else
    result := lang;
end;

function TQuestionnaireEditorFrame.getJurisdiction(i: integer): TFHIRCodeableConcept;
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

procedure TQuestionnaireEditorFrame.inputChanged(Sender: TObject);
begin
  if not Loading then
    commit;
end;

procedure TQuestionnaireEditorFrame.lbResourcesChangeCheck(Sender: TObject);
var
  s : String;
  c, d : TFhirEnum;
  found : boolean;
begin
  s := lbResources.Items[lbResources.ItemIndex];
  found := false;
  d := nil;
  for c in Questionnaire.subjectTypeList do
  begin
    found := found or (c.value = s);
    if found then
      d := c;
  end;
  if not lbResources.ListItems[lbResources.ItemIndex].IsChecked then {not - it's about to change }
  begin
    if found then
      Questionnaire.subjectTypeList.DeleteByReference(d);
  end
  else if not found then
    Questionnaire.subjectTypeList.Append.value := s;
  ResourceIsDirty := true;
end;

procedure TQuestionnaireEditorFrame.load;
begin
  tvStructure.Selected := tvMetadata;
  tvStructure.ExpandAll;
  tvStructureClick(nil);
end;

procedure TQuestionnaireEditorFrame.loadContext;
var
  rt : TFhirResourceType;
  c : TFhirEnum;
  ok : boolean;
begin
  grdUseContext.RowCount := Questionnaire.useContextList.Count;

  lbResources.Items.clear;
  lbResources.BeginUpdate;
  try
    for rt := low(TFhirResourceType) to High(TFhirResourceType) do
      if rt <> frtNull then
      begin
        ok := false;
        for c in Questionnaire.subjectTypeList do
          ok := ok or (c.value = CODES_TFhirResourceType[rt]);
        lbResources.Items.Add(CODES_TFhirResourceType[rt]);
        lbResources.ListItems[lbResources.Items.Count - 1].IsChecked := ok;
      end;
  finally
    lbResources.EndUpdate;
  end;
end;

procedure TQuestionnaireEditorFrame.loadForm;
begin
  if FPanel = nil then
  begin
    FPanel := TQuestionnairePanel.Create(self);
    FPanel.Parent := tbForm;
    FPanel.Margins.Left := 6;
    FPanel.Margins.Top := 6;
    FPanel.Margins.Right := 6;
    FPanel.Margins.Bottom := 6;
    FPanel.Width := tbForm.Width;
    FPanel.Align := Align.alClient;
    FPanel.imageList := ImageList1;
    FPanel.Settings := Settings.link;
    FPanel.OnWork := OnWork;
  end;
  FPanel.Questionnaire := Questionnaire.Link;
  FPanel.build;
end;

procedure TQuestionnaireEditorFrame.loadGrid;
begin
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  grdItems.RowCount := flatItems.Count;
  if FSelected <> nil then
    grdItems.Row := flatItems.IndexOf(FSelected);
end;

procedure TQuestionnaireEditorFrame.loadMetadata;
var
  url : TFHIRUri;
begin
  Loading := true;
  try
    cbExperimental.IsChecked := Questionnaire.experimental;

    edtURL.Text := Questionnaire.url;
    edtName.Text := Questionnaire.name;
    btnName.ImageIndex := translationsImageIndex(Questionnaire.nameElement);
    edtTitle.Text := Questionnaire.title;
    btnTitle.ImageIndex := translationsImageIndex(Questionnaire.titleElement);
    edtVersion.Text := Questionnaire.version;
    edtPublisher.text := Questionnaire.publisher;
    btnPublisher.ImageIndex := translationsImageIndex(Questionnaire.publisherElement);
    edtDescription.Text := Questionnaire.description;
    btnDescription.ImageIndex := translationsImageIndex(Questionnaire.descriptionElement);
    edtPurpose.Text := Questionnaire.purpose;
    btnPurpose.ImageIndex := translationsImageIndex(Questionnaire.purposeElement);
    edtCopyright.Text := Questionnaire.copyright;
    btnCopyright.ImageIndex := translationsImageIndex(Questionnaire.copyrightElement);
    cbxStatus.ItemIndex := ord(Questionnaire.status);
    if Questionnaire.dateElement = nil then
      dedDate.Text := ''
    else
      dedDate.DateTime := Questionnaire.date.DateTime;
    cbxJurisdiction.ItemIndex := readJurisdiction;

    if Questionnaire.approvalDateElement = nil then
      dedApproval.Text := ''
    else
      dedApproval.DateTime := Questionnaire.approvalDate.DateTime;
    if Questionnaire.lastReviewDateElement = nil then
      dedReview.Text := ''
    else
      dedReview.DateTime := Questionnaire.lastReviewDate.DateTime;

    if Questionnaire.effectivePeriod = nil then
    begin
      dedBegin.Text := '';
      dedEnd.Text := '';
    end
    else
    begin
      if Questionnaire.effectivePeriod.startElement = nil then
        dedBegin.Text := ''
      else
        dedBegin.DateTime := Questionnaire.effectivePeriod.start.DateTime;
      if Questionnaire.effectivePeriod.end_Element = nil then
        dedEnd.Text := ''
      else
        dedEnd.DateTime := Questionnaire.effectivePeriod.end_.DateTime;
    end;
  finally
    loading := false;
  end;
end;

procedure TQuestionnaireEditorFrame.loadSDC;
var
  id : TFhirIdentifier;
  exl : TFhirExtensionList;
begin
  Loading := true;
  try
    cbSDC.IsChecked := (Questionnaire.meta <> nil) and (Questionnaire.meta.hasProfile('http://hl7.org/fhir/us/sdc/StructureDefinition/sdc-questionnaire'));
    edtSDCEndPoint.Text := readExtension(Questionnaire, 'http://hl7.org/fhir/us/sdc/StructureDefinition/sdc-questionnaire-endpoint');
    cbProvenanceSignatureRequired.IsChecked := readExtension(Questionnaire, 'http://hl7.org/fhir/us/sdc/StructureDefinition/sdc-questionnaire-provenanceSignatureRequired') = 'true';
    cbStyleSensitive.IsChecked := readExtension(Questionnaire, 'http://hl7.org/fhir/StructureDefinition/rendering-styleSensitive') = 'true';
    edtSDCsourceStructureMap.Text := readExtension(Questionnaire, 'http://hl7.org/fhir/StructureDefinition/questionnaire-sourceStructureMap');
    edtSDCtargetStructureMap.Text := readExtension(Questionnaire, 'http://hl7.org/fhir/StructureDefinition/questionnaire-targetStructureMap');
    id := Questionnaire.getExtensionValue('http://hl7.org/fhir/StructureDefinition/questionnaire-studyProtocolIdentifier') as TFhirIdentifier;
    if (id <> nil) then
    begin
      edtSDCIdSystem.Text := id.system;
      edtSDCIdValue.Text := id.value;
    end
    else
    begin
      edtSDCIdSystem.Text := '';
      edtSDCIdValue.Text := '';
    end;
    if (Questionnaire.titleElement <> nil) then
    begin
      edtSDCTitleStyle.Text := readExtension(Questionnaire.titleElement, 'http://hl7.org/fhir/StructureDefinition/rendering-style');
      edtSDCTitleXtml.Text := readExtension(Questionnaire.titleElement, 'http://hl7.org/fhir/StructureDefinition/rendering-xhtml');
    end
    else
    begin
      edtSDCTitleStyle.Text := '';
      edtSDCTitleXtml.Text := '';
    end;
    gridContext.RowCount := Questionnaire.getExtensionCount('http://hl7.org/fhir/StructureDefinition/questionnaire-context');
  finally
    loading := false;
  end;
end;

function TQuestionnaireEditorFrame.readJurisdiction: Integer;
var
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
begin
  result := -1;
  for cc in Questionnaire.jurisdictionList do
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


function TQuestionnaireEditorFrame.renderItem(item: TFhirQuestionnaireItem): String;
begin
  if item.prefix <> '' then
    result := item.prefix +' '+item.text+' : '+CODES_TFhirItemTypeEnum[item.type_]+ ' ('+item.linkId+')'
  else
    result := item.text+' : '+CODES_TFhirItemTypeEnum[item.type_]+ ' ('+item.linkId+')';
end;

procedure TQuestionnaireEditorFrame.tvStructureClick(Sender: TObject);
begin
  Loading := true;
  try
    if tvStructure.Selected = tvMetadata then
    begin
      tbStructure.ActiveTab := tbMetadata;
      loadMetadata;
    end
    else if tvStructure.Selected = tvSDC then
    begin
      tbStructure.ActiveTab := tbSDC;
      loadSDC;
    end
    else if tvStructure.Selected = tvContext then
    begin
      tbStructure.ActiveTab := tbContext;
      loadContext;
    end
    else if tvStructure.Selected = tvGrid then
    begin
      tbStructure.ActiveTab := tbGrid;
      loadGrid;
    end
    else if tvStructure.Selected = tvForm then
    begin
      tbStructure.ActiveTab := tbForm;
      loadForm;
    end
  finally
    Loading := false;
  end;
end;

function template(fragment : String) : String;
begin
result :=
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
'<head>'+#13#10+
'  <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />'+#13#10+
' <style>'+#13#10+
'  body { background-color: rgb(255, 254, 245);'+#13#10+
'  margin: 0px;'+#13#10+
'  padding: 0px;'+#13#10+
'  height: 100%;'+#13#10+
'  font-size: 12px;'+#13#10+
'  font-family: verdana;'+#13#10+
'}'+#13#10+
' </style>'+#13#10+
'</head>'+#13#10+
fragment+#13#10+
'<body>'+#13#10+
''+#13#10+
'</body>'+#13#10+
'</html>'+#13#10;
end;


procedure TQuestionnaireEditorFrame.updateStatus(sel: TFhirQuestionnaireItem);
var
  list : TFhirQuestionnaireItemList;
  parent : TFhirQuestionnaireItem;
  i : integer;
begin
  if not findItem(sel, parent, list, i) then
  begin
    btnItemUp.Enabled := false;
    btnItemDown.Enabled := false;
    btnItemIn.Enabled := false;
    btnItemOut.Enabled := false;
    btnEditItem.Enabled := false;
    btnDeleteItem.Enabled := false;
  end
  else
  begin
    btnItemUp.Enabled := i > 0;
    btnItemDown.Enabled := i < list.Count - 1;
    btnItemIn.Enabled := sel.TagInt > 0;
    btnItemOut.Enabled := i > 0;
    btnEditItem.Enabled := true;
    btnDeleteItem.Enabled := true;
  end;
end;

end.

