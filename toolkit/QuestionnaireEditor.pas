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
  DateSupport, StringSupport, DecimalSupport,
  AdvGenerics, CSVSupport,
  FHIRBase, FHIRConstants, FHIRTypes, FHIRResources, FHIRUtilities, FHIRIndexBase, FHIRIndexInformation, FHIRSupport,
  QuestionnaireRenderer,
  BaseResourceFrame, { QuestionnaireFrame, }
  QuestionnaireItemDialog, MemoEditorDialog;

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
    btnMemoCopyright: TButton;
    btnMemoForDesc: TButton;
    btnMemoPurpose: TButton;
    cbExperimental: TCheckBox;
    cbxJurisdiction: TComboBox;
    cbxStatus: TComboBox;
    tvGrid: TTreeViewItem;
    tvTree: TTreeViewItem;
    Panel1: TPanel;
    Panel5: TPanel;
    webPreview: TWebBrowser;
    Splitter1: TSplitter;
    Label12: TLabel;
    tbGrid: TTabItem;
    tbTree: TTabItem;
    Panel6: TPanel;
    Panel7: TPanel;
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
    Grid1: TGrid;
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
    btnAddItemTV: TButton;
    btnAddChildItemTV: TButton;
    btnEditItemTV: TButton;
    btnItemUpTV: TButton;
    btnItemDownTV: TButton;
    btnItemInTV: TButton;
    btnItemOutTV: TButton;
    btnDeleteItemTV: TButton;
    tvItemTree: TTreeView;
    Panel8: TPanel;
    Label20: TLabel;
    edtSearch: TEdit;
    btnSearchStart: TButton;
    btnSearchNext: TButton;
    btnSearchPrev: TButton;
    btnSearchEnd: TButton;
    CheckBox1: TCheckBox;
    Panel9: TPanel;
    Label21: TLabel;
    Edit1: TEdit;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    CheckBox2: TCheckBox;
    tvForm: TTreeViewItem;
    tbForm: TTabItem;
    procedure tvStructureClick(Sender: TObject);
    procedure inputChanged(Sender: TObject);
    procedure btnMemoForDescClick(Sender: TObject);
    procedure btnMemoPurposeClick(Sender: TObject);
    procedure btnMemoCopyrightClick(Sender: TObject);
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
    procedure tvItemTreeClick(Sender: TObject);
    procedure btnAddItemTVClick(Sender: TObject);
    procedure btnAddChildItemTVClick(Sender: TObject);
    procedure btnEditItemTVClick(Sender: TObject);
    procedure btnItemUpTVClick(Sender: TObject);
    procedure btnItemDownTVClick(Sender: TObject);
    procedure btnItemInTVClick(Sender: TObject);
    procedure btnItemOutTVClick(Sender: TObject);
    procedure btnDeleteItemTVClick(Sender: TObject);
    procedure lbResourcesChangeCheck(Sender: TObject);
  private
    flatItems : TAdvList<TFhirQuestionnaireItem>;
    loading : boolean;
    FSelected : TFhirQuestionnaireItem;
//    FFrame : TQuestionnaireFrame;
    function GetQuestionnaire: TFHIRQuestionnaire;
    function readJurisdiction : Integer;
    function getJurisdiction(i : integer) : TFHIRCodeableConcept;
    procedure addToItems(level : integer; list : TFhirQuestionnaireItemList);
    procedure updateRendering;
    function renderItem(item : TFhirQuestionnaireItem) : String;

    function addItemToTree(parent, item : TFhirQuestionnaireItem) : TTreeViewItem;
    function findItem(sel : TFhirQuestionnaireItem; var parent : TFhirQuestionnaireItem; var list : TFhirQuestionnaireItemList; var index : integer) : boolean;
    procedure updateStatusGrid(sel : TFhirQuestionnaireItem);
    procedure updateStatusTree(sel : TFhirQuestionnaireItem);
//    procedure buildFlatGrid(list : TFhirQuestionnaireConceptList);

    procedure loadMetadata;
    procedure loadContext;
    procedure loadGrid;
    procedure loadTree;
    procedure loadForm;

    procedure commitMetadata;
//    procedure commitGrid;
//    procedure commitItems;
  public
    Constructor Create(owner : TComponent); override;
    Destructor Destroy; override;

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
  grdItems.BeginUpdate;
  p := flatItems[grdItems.Row];
  c := p.itemList.Append;
  c.linkId := 'i'+inttostr(flatItems.Count + 1);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  grdItems.EndUpdate;
  grdItems.RowCount := flatItems.Count;
  ResourceIsDirty := true;
  updateRendering;
  grdItems.SelectCell(1, flatItems.IndexOf(c));
  btnEditItemClick(nil);
end;

procedure TQuestionnaireEditorFrame.btnAddChildItemTVClick(Sender: TObject);
var
  p, c : TFhirQuestionnaireItem;
  tvp, tv : TTreeViewItem;
begin
  p := tvItemTree.Selected.TagObject as TFhirQuestionnaireItem;
  c := p.itemList.Append;
  c.linkId := 'i'+inttostr(Questionnaire.itemCount + 1);
  tvp := TTreeViewItem(p.TagObject);
  tv := TTreeViewItem.Create(self);
  tvp.AddObject(tv);
  tv.Text := renderItem(c);
  tv.TagObject := c;
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  c.TagObject := tv;
  ResourceIsDirty := true;
  updateRendering;
  tvItemTree.Selected := tv;
  btnEditItemClick(nil);
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
  updateRendering;
  btnEditItemClick(nil);
end;

procedure TQuestionnaireEditorFrame.btnAddItemTVClick(Sender: TObject);
var
  c : TFhirQuestionnaireItem;
  tv : TTreeViewItem;
begin
  c := Questionnaire.itemList.Append;
  c.linkId := 'i'+inttostr(Questionnaire.itemCount + 1);
  tv := TTreeViewItem.Create(self);
  tvItemTree.AddObject(tv);
  tv.Text := renderItem(c);
  tv.TagObject := c;
  c.TagObject := tv;
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  ResourceIsDirty := true;
  updateRendering;
  tvItemTree.Selected := tv;
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
        updateRendering;
      end;
    end
  );
end;

procedure TQuestionnaireEditorFrame.btnDeleteItemTVClick(Sender: TObject);
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
    var p : TFhirQuestionnaireItem;
    begin
      if AResult = mrYes then
      begin
        if index > 0 then
          nf := list[index - 1]
        else if list.Count > 1 then
          nf := list[index + 1]
        else
          nf := nil;
        tvItemTree.BeginUpdate;
        list.Remove(index);
        tvItemTree.Clear;
        for p in Questionnaire.itemList do
          addItemToTree(nil, p);
        flatItems.Clear;
        addToItems(0, Questionnaire.itemList);
        tvItemTree.EndUpdate;
        if nf <> nil then
          tvItemTree.Selected := nf.TagObject as TTreeViewItem;
        ResourceIsDirty := true;
        updateRendering;
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
  updateRendering;
end;

procedure TQuestionnaireEditorFrame.btnItemDownTVClick(Sender: TObject);
var
  sel, p : TFhirQuestionnaireItem;
  parent : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  index : integer;
begin
  sel := tvItemTree.Selected.TagObject as TFhirQuestionnaireItem;
  if not findItem(sel, parent, list, index) then
    exit;

  tvItemTree.BeginUpdate;
  list.Exchange(index, index+1);
  tvItemTree.Clear;
  for p in Questionnaire.itemList do
    addItemToTree(nil, p);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  tvItemTree.EndUpdate;
  tvItemTree.Selected := sel.TagObject as TTreeViewItem;
  ResourceIsDirty := true;
  updateRendering;
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
  updateRendering;
end;

procedure TQuestionnaireEditorFrame.btnItemInTVClick(Sender: TObject);
var
  sel, p : TFhirQuestionnaireItem;
  parent, grandparent : TFhirQuestionnaireItem;
  list, gList : TFhirQuestionnaireItemList;
  index, gIndex : integer;
begin
  sel := tvItemTree.Selected.TagObject as TFhirQuestionnaireItem;
  if not findItem(sel, parent, list, index) then
    exit;
  if parent = nil then
    exit;
  if not findItem(parent, grandparent, glist, gindex) then
    exit;

  tvItemTree.BeginUpdate;
  glist.InsertItem(gindex+1, sel.Link);
  list.DeleteByIndex(index);
  tvItemTree.Clear;
  for p in Questionnaire.itemList do
    addItemToTree(nil, p);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  tvItemTree.EndUpdate;
  tvItemTree.Selected := sel.TagObject as TTreeViewItem;
  ResourceIsDirty := true;
  updateRendering;
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
  updateRendering;
end;

procedure TQuestionnaireEditorFrame.btnItemOutTVClick(Sender: TObject);
var
  sel, p : TFhirQuestionnaireItem;
  parent : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  index : integer;
begin
  sel := tvItemTree.Selected.TagObject as TFhirQuestionnaireItem;
  if not findItem(sel, parent, list, index) then
    exit;
  if index = 0 then
    exit;

  tvItemTree.BeginUpdate;
  parent := list[index - 1];
  parent.itemList.add(sel.Link);
  list.DeleteByIndex(index);
  tvItemTree.Clear;
  for p in Questionnaire.itemList do
    addItemToTree(nil, p);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  tvItemTree.EndUpdate;
  tvItemTree.Selected := sel.TagObject as TTreeViewItem;
  ResourceIsDirty := true;
  updateRendering;
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
  updateRendering;
end;

procedure TQuestionnaireEditorFrame.btnItemUpTVClick(Sender: TObject);
var
  sel, p : TFhirQuestionnaireItem;
  parent : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  index : integer;
begin
  sel := tvItemTree.Selected.TagObject as TFhirQuestionnaireItem;
  if grdItems.Row = -1 then
    exit;
  sel := flatItems[grdItems.Row];
  if not findItem(sel, parent, list, index) then
    exit;

  tvItemTree.BeginUpdate;
  list.Exchange(index, index-1);
  tvItemTree.Clear;
  for p in Questionnaire.itemList do
    addItemToTree(nil, p);
  flatItems.Clear;
  addToItems(0, Questionnaire.itemList);
  tvItemTree.EndUpdate;
  tvItemTree.Selected := sel.TagObject as TTreeViewItem;
  ResourceIsDirty := true;
  updateRendering;
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
    form.Settings := Settings.link;
    form.OnWork := OnWork;
    if form.ShowModal = mrOk then
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
      item.options := form.item.options.Link;
      grdItems.EndUpdate;
      ResourceIsDirty := true;
      updateRendering;
    end;
  finally
    form.Free;
  end;
end;

procedure TQuestionnaireEditorFrame.btnEditItemTVClick(Sender: TObject);
var
  form : TQuestionnaireItemForm;
  item : TFHIRQuestionnaireItem;
begin
  form := TQuestionnaireItemForm.Create(self);
  try
    item := tvItemTree.Selected.TagObject as TFhirQuestionnaireItem;
    form.item := item.clone;
    form.Settings := Settings.link;
    form.OnWork := OnWork;
    if form.ShowModal = mrOk then
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
      item.options := form.item.options.Link;
      grdItems.EndUpdate;
      ResourceIsDirty := true;
      updateRendering;
    end;
  finally
    form.Free;
  end;
end;

procedure TQuestionnaireEditorFrame.btnMemoCopyrightClick(Sender: TObject);
begin
  editMemo(self, 'ValueSet Copyright', edtCopyright);
end;

procedure TQuestionnaireEditorFrame.btnMemoForDescClick(Sender: TObject);
begin
  editMemo(self, 'ValueSet Description', edtDescription);
end;

procedure TQuestionnaireEditorFrame.btnMemoPurposeClick(Sender: TObject);
begin
  editMemo(self, 'ValueSet Purpose', edtPurpose);
end;

procedure TQuestionnaireEditorFrame.cancel;
begin
end;

procedure TQuestionnaireEditorFrame.commit;
begin
  if tvStructure.Selected = tvMetadata then
    CommitMetadata;
  updateRendering;
  ResourceIsDirty := true;
end;

procedure TQuestionnaireEditorFrame.commitMetadata;
var
  s : String;
  cc : TFHIRCodeableConcept;
  function makeDT(ded : TDateEdit) : TDateTimeEx;
  begin
    if ded.Text = '' then
      result := TDateTimeEx.makeNull
    else
      result := TDateTimeEx.makeLocal(ded.Date);
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

constructor TQuestionnaireEditorFrame.Create(owner: TComponent);
begin
  inherited;
  flatItems := TAdvList<TFhirQuestionnaireItem>.create;
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
    updateStatusGrid(nil);
    FSelected := nil;
  end
  else
  begin
    FSelected := flatItems[grdItems.Row];
    updateStatusGrid(FSelected);
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
  updateRendering;
  ResourceIsDirty := true;
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
  updateRendering;
end;

procedure TQuestionnaireEditorFrame.loadContext;
var
  rt : TFhirResourceType;
  c : TFhirEnum;
  ok : boolean;
begin
  for rt := low(TFhirResourceType) to High(TFhirResourceType) do
    if rt <> frtNull then
    begin
      ok := false;
      for c in Questionnaire.subjectTypeList do
        ok := ok or (c.value = CODES_TFhirResourceType[rt]);
      lbResources.Items.Add(CODES_TFhirResourceType[rt]);
      lbResources.ListItems[lbResources.Items.Count - 1].IsChecked := ok;
    end;
end;

procedure TQuestionnaireEditorFrame.loadForm;
begin
{  if FFrame = nil then
  begin
    FFrame := TQuestionnaireFrame.Create(self);
    FFrame.Parent := tbForm;
    FFrame.Margins.Left := 6;
    FFrame.Margins.Top := 6;
    FFrame.Margins.Right := 6;
    FFrame.Margins.Bottom := 6;
  end;
  FFrame.Questionnaire := Questionnaire.Link;
  FFrame.build;}
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
    edtTitle.Text := Questionnaire.title;
    edtVersion.Text := Questionnaire.version;
    edtPublisher.text := Questionnaire.publisher;
    edtDescription.Text := Questionnaire.description;
    edtPurpose.Text := Questionnaire.purpose;
    edtCopyright.Text := Questionnaire.copyright;
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

procedure TQuestionnaireEditorFrame.loadTree;
var
  item : TFhirQuestionnaireItem;
begin
  Loading := true;
  try
    tvItemTree.Clear;
    for item in Questionnaire.itemList do
      addItemToTree(nil, item);
    flatItems.Clear;
    addToItems(0, Questionnaire.itemList);
    tvItemTree.ExpandAll;
    if FSelected <> nil then
      tvItemTree.Selected := FSelected.TagObject as TTreeViewItem;
  finally
    Loading := false;
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

procedure TQuestionnaireEditorFrame.tvItemTreeClick(Sender: TObject);
begin
  if Loading then
    exit;
  FSelected := tvItemTree.Selected.TagObject as TFhirQuestionnaireItem;
  updateStatusTree(FSelected);
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
    else if tvStructure.Selected = tvTree then
    begin
      tbStructure.ActiveTab := tbTree;
      loadTree;
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
'	margin: 0px;'+#13#10+
'	padding: 0px;'+#13#10+
'	height: 100%;'+#13#10+
'	font-size: 12px;'+#13#10+
'	font-family: verdana;'+#13#10+
'}'+#13#10+
' </style>'+#13#10+
'</head>'+#13#10+
fragment+#13#10+
'<body>'+#13#10+
''+#13#10+
'</body>'+#13#10+
'</html>'+#13#10;
end;


procedure TQuestionnaireEditorFrame.updateRendering;
var
  r : TQuestionnaireRenderer;
begin
  r := TQuestionnaireRenderer.Create;
  try
    r.questionnaire := Questionnaire.Link;
    webPreview.LoadFromStrings(template(r.render), 'my.html');
  finally
    r.free;
  end;
end;

procedure TQuestionnaireEditorFrame.updateStatusGrid(sel: TFhirQuestionnaireItem);
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

procedure TQuestionnaireEditorFrame.updateStatusTree(sel: TFhirQuestionnaireItem);
var
  list : TFhirQuestionnaireItemList;
  parent : TFhirQuestionnaireItem;
  i : integer;
begin
  if not findItem(sel, parent, list, i) then
  begin
    btnItemUpTV.Enabled := false;
    btnItemDownTV.Enabled := false;
    btnItemInTV.Enabled := false;
    btnItemOutTV.Enabled := false;
    btnEditItemTV.Enabled := false;
    btnDeleteItemTV.Enabled := false;
  end
  else
  begin
    btnItemUpTV.Enabled := i > 0;
    btnItemDownTV.Enabled := i < list.Count - 1;
    btnItemInTV.Enabled := sel.TagInt > 0;
    btnItemOutTV.Enabled := i > 0;
    btnEditItemTV.Enabled := true;
    btnDeleteItemTV.Enabled := true;
  end;
end;

function TQuestionnaireEditorFrame.addItemToTree(parent, item: TFhirQuestionnaireItem) : TTreeViewItem;
var
  tvp, tv : TTreeViewItem;
  c : TFhirQuestionnaireItem;
begin
  tv := TTreeViewItem.Create(self);
  tv.Text := renderItem(item);
  tv.TagObject := Item;
  Item.TagObject := tv;
  if parent = nil then
  begin
    tvItemTree.AddObject(tv);
    Item.TagInt := 0;
  end
  else
  begin
    tvp := TTreeViewItem(parent.TagObject);
    tvp.AddObject(tv);
    Item.TagInt := parent.TagInt + 1;
  end;
  for c in Item.ItemList do
    addItemToTree(Item, c);
  result := tv;
end;

end.

