unit CodeSystemEditor;

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
  FMX.Grid.Style, FMX.Grid, FMX.Menus, FMX.ImgList,
  fsl_base, fsl_utilities, fsl_stream, FHIR.Ui.Fmx,
  fhir_objects, 
  FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities, fhir_indexing, FHIR.Version.IndexInfo,
  BaseResourceFrame, ToolKitUtilities,
  SearchParameterEditor, ListSelector, AddRestResourceDialog, ValuesetExpansion, ValuesetSelectDialog, MemoEditorDialog,
  CodeSystemConceptDialog, FMX.Platform, System.ImageList, TranslationsEditorDialog, ResourceHistoryDialog, ResourceContributorDialog;

type
  TFrame = TBaseResourceFrame; // re-aliasing the Frame to work around a designer bug

  TCodeSystemEditorFrame = class(TFrame)
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    tvStructure: TTreeView;
    tbStructure: TTabControl;
    tbMetadata: TTabItem;
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
    btnMemoForDesc: TButton;
    btnMemoPurpose: TButton;
    btnMemoCopyright: TButton;
    Label12: TLabel;
    edtIdSystem: TEdit;
    Label25: TLabel;
    edtIdValue: TEdit;
    Label26: TLabel;
    Label27: TLabel;
    edtValueSet: TEdit;
    Label28: TLabel;
    cbxHeirarchy: TComboBox;
    Label29: TLabel;
    cbxContent: TComboBox;
    Label30: TLabel;
    edtConceptCount: TEdit;
    Button1: TButton;
    cbCaseSensitive: TCheckBox;
    cbCompositional: TCheckBox;
    cbNeedsVersion: TCheckBox;
    ScrollBox1: TScrollBox;
    tbProperties: TTabItem;
    tbFilters: TTabItem;
    tbConcepts: TTabItem;
    tvProperties: TTreeViewItem;
    tvFilters: TTreeViewItem;
    tvConcepts: TTreeViewItem;
    pnlPropertyActions: TPanel;
    grdProperties: TGrid;
    btnAddProperty: TButton;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    PopupColumn1: TPopupColumn;
    StringColumn3: TStringColumn;
    btnDeleteProperty: TButton;
    Panel1: TPanel;
    btnAddFIlter: TButton;
    btnDeleteFilter: TButton;
    grdFilters: TGrid;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringColumn7: TStringColumn;
    Panel5: TPanel;
    btnAddConcept: TButton;
    btnAddChildConcept: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    tvConceptTree: TTreeView;
    grdConcepts: TGrid;
    IntegerColumn1: TIntegerColumn;
    StringColumn8: TStringColumn;
    StringColumn9: TStringColumn;
    StringColumn10: TStringColumn;
    btnEditConcept: TButton;
    btnConceptUp: TButton;
    btnConceptDown: TButton;
    btnConceptIn: TButton;
    btnConceptOut: TButton;
    btnDeleteConcept: TButton;
    btnExport: TButton;
    dlgExport: TSaveDialog;
    Panel6: TPanel;
    Label14: TLabel;
    edtSearch: TEdit;
    btnSearchStart: TButton;
    btnSearchNext: TButton;
    btnSearchPrev: TButton;
    btnSearchEnd: TButton;
    CheckBox1: TCheckBox;
    btnName: TButton;
    ToolbarImages: TImageList;
    btnTitle: TButton;
    btnPublisher: TButton;
    Label15: TLabel;
    edtSupplements: TEdit;
    tvHl7: TTreeViewItem;
    tbHl7: TTabItem;
    VertScrollBox3: TVertScrollBox;
    edtSteward: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    memOpenIssues: TMemo;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    edtLegalese: TEdit;
    edtVDeprecated: TEdit;
    edtVersionPolicy: TEdit;
    CheckColumn1: TCheckColumn;
    StringColumn13: TStringColumn;
    StringColumn15: TStringColumn;
    PopupColumn2: TPopupColumn;
    procedure tvStructureClick(Sender: TObject);
    procedure inputChanged(Sender: TObject);
    procedure btnMemoForDescClick(Sender: TObject);
    procedure btnMemoPurposeClick(Sender: TObject);
    procedure btnMemoCopyrightClick(Sender: TObject);
    procedure grdPropertiesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure grdPropertiesSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure grdPropertiesSelChanged(Sender: TObject);
    procedure btnAddPropertyClick(Sender: TObject);
    procedure btnDeletePropertyClick(Sender: TObject);
    procedure btnAddFIlterClick(Sender: TObject);
    procedure grdFiltersSelChanged(Sender: TObject);
    procedure grdFiltersGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure grdFiltersSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnDeleteFilterClick(Sender: TObject);
    procedure btnAddConceptClick(Sender: TObject);
    procedure grdConceptsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure grdConceptsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure tvConceptTreeChange(Sender: TObject);
    procedure grdConceptsSelChanged(Sender: TObject);
    procedure btnAddChildConceptClick(Sender: TObject);
    procedure btnConceptUpClick(Sender: TObject);
    procedure btnConceptDownClick(Sender: TObject);
    procedure btnConceptInClick(Sender: TObject);
    procedure btnConceptOutClick(Sender: TObject);
    procedure btnDeleteConceptClick(Sender: TObject);
    procedure btnEditConceptClick(Sender: TObject);
    procedure grdConceptsCellDblClick(const Column: TColumn; const Row: Integer);
    procedure btnExportClick(Sender: TObject);
    procedure btnSearchStartClick(Sender: TObject);
    procedure btnSearchNextClick(Sender: TObject);
    procedure btnSearchPrevClick(Sender: TObject);
    procedure btnSearchEndClick(Sender: TObject);
    procedure btnNameClick(Sender: TObject);
    procedure btnTitleClick(Sender: TObject);
    procedure btnPublisherClick(Sender: TObject);
    procedure grdPropertiesCellClick(const Column: TColumn; const Row: Integer);
    procedure grdFiltersCellClick(const Column: TColumn; const Row: Integer);
    procedure grdPropertiesDrawColumnBackground(Sender: TObject;
      const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
      const Row: Integer; const Value: TValue; const State: TGridDrawStates);
    procedure Button1Click(Sender: TObject);
  private
    flatConcepts : TFslList<TFhirCodeSystemConcept>;
    selchanging : boolean;

    function GetCodeSystem: TFHIRCodeSystem;
    function readJurisdiction : Integer;
    function getJurisdiction(i : integer) : TFHIRCodeableConcept;
    function addConceptToTree(parent, concept : TFhirCodeSystemConcept) : TTreeViewItem;
    function findConcept(sel : TFhirCodeSystemConcept; var parent : TFhirCodeSystemConcept; var list : TFhirCodeSystemConceptList; var index : integer) : boolean;
    procedure updateStatus(sel : TFhirCodeSystemConcept);
    procedure buildFlatGrid(list : TFhirCodeSystemConceptList);
    function matchesSearch(concept : TFhirCodeSystemConcept) : boolean;
    
    procedure loadMetadata;
    procedure loadHL7Process;
    procedure loadProperties;
    procedure loadFilters;
    procedure loadConcepts;

    procedure commitMetadata;
    procedure commitHL7Process;
    procedure commitProperties;
    procedure commitFilters;
    procedure commitConcepts;
  public
    constructor Create(owner : TComponent); override;
    destructor Destroy; override;

    property CodeSystem : TFHIRCodeSystem read GetCodeSystem;
    procedure load; override;

    procedure commit; override;
    procedure cancel; override;

  end;

implementation

{$R *.fmx}

function polish(s : String): String;
begin
  result := s.trim.replace(#13, ' ').replace(#10, ' ').replace('  ', ' ');
end;



{ TValueSetEditorFrame }

function TCodeSystemEditorFrame.addConceptToTree(parent, concept: TFhirCodeSystemConcept) : TTreeViewItem;
var
  tvp, tv : TTreeViewItem;
  c : TFhirCodeSystemConcept;
begin
  tv := TTreeViewItem.Create(self);
  tv.Text := concept.Code +' "'+concept.display+'": '+polish(concept.definition);
  tv.TagObject := concept;
  concept.TagObject := tv;
  if parent = nil then
  begin
    tvConceptTree.AddObject(tv);
    concept.TagInt := 0;
  end
  else
  begin
    tvp := TTreeViewItem(parent.TagObject);
    tvp.AddObject(tv);
    concept.TagInt := parent.TagInt + 1;
  end;
  for c in concept.conceptList do
    addConceptToTree(concept, c);
  result := tv;
end;

procedure TCodeSystemEditorFrame.btnAddChildConceptClick(Sender: TObject);
var
  p, c : TFhirCodeSystemConcept;
begin
  if tvConceptTree.Selected = nil then
    exit;

  p := TFhirCodeSystemConcept(tvConceptTree.Selected.TagObject);
  c := p.conceptList.Append;
  c.code := 'new-code';
  flatConcepts.Clear;
  buildFlatGrid(CodeSystem.conceptList);
  grdConcepts.RowCount := 0;
  grdConcepts.RowCount := flatConcepts.Count;
  tvConceptTree.Selected := addConceptToTree(p, c);
  grdConcepts.SelectCell(1, flatConcepts.IndexOf(c));
  ResourceIsDirty := true;
  btnEditConceptClick(nil);
end;

procedure TCodeSystemEditorFrame.btnAddConceptClick(Sender: TObject);
var
  c : TFhirCodeSystemConcept;
begin
  c := CodeSystem.conceptList.Append;
  c.code := 'new-code';
  tvConceptTree.Selected := addConceptToTree(nil, c);
  flatConcepts.Clear;
  buildFlatGrid(CodeSystem.conceptList);
  grdConcepts.RowCount := 0;
  grdConcepts.RowCount := flatConcepts.Count;
  grdConcepts.SelectCell(1, flatConcepts.IndexOf(c));
  ResourceIsDirty := true;
  btnEditConceptClick(nil);
end;

procedure TCodeSystemEditorFrame.btnAddFIlterClick(Sender: TObject);
begin
  CodeSystem.filterList.Append;
  ResourceIsDirty := true;
  loadFilters;
end;

procedure TCodeSystemEditorFrame.btnAddPropertyClick(Sender: TObject);
begin
  CodeSystem.property_List.Append;
  loadProperties;
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.btnConceptDownClick(Sender: TObject);
var
  sel : TFhirCodeSystemConcept;
  parent : TFhirCodeSystemConcept;
  list : TFhirCodeSystemConceptList;
  index : integer;
begin
  if tvConceptTree.Selected = nil then
    exit;
  sel := TFhirCodeSystemConcept(tvConceptTree.Selected.TagObject);
  if not findConcept(sel, parent, list, index) then
    exit;
  // 3 things to update: list, tree, and grid
  list.Exchange(index, index+1);
  loadConcepts;
  tvConceptTree.Selected := TTreeViewItem(sel.TagObject);
  grdConcepts.SelectCell(1, flatConcepts.IndexOf(sel));
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.btnConceptInClick(Sender: TObject);
var
  sel : TFhirCodeSystemConcept;
  parent, grandparent : TFhirCodeSystemConcept;
  list, gList : TFhirCodeSystemConceptList;
  index, gIndex : integer;
begin
  if tvConceptTree.Selected = nil then
    exit;
  sel := TFhirCodeSystemConcept(tvConceptTree.Selected.TagObject);
  if not findConcept(sel, parent, list, index) then
    exit;
  if parent = nil then
    exit;
  if not findConcept(parent, grandparent, glist, gindex) then
    exit;

  // 3 things to update: list, tree, and grid
  glist.InsertItem(gindex+1, sel.Link);
  list.DeleteByIndex(index);
  loadConcepts;
  tvConceptTree.Selected := TTreeViewItem(sel.TagObject);
  grdConcepts.SelectCell(1, flatConcepts.IndexOf(sel));
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.btnConceptOutClick(Sender: TObject);
var
  sel : TFhirCodeSystemConcept;
  parent : TFhirCodeSystemConcept;
  list : TFhirCodeSystemConceptList;
  index : integer;
begin
  if tvConceptTree.Selected = nil then
    exit;
  sel := TFhirCodeSystemConcept(tvConceptTree.Selected.TagObject);
  if not findConcept(sel, parent, list, index) then
    exit;
  if index = 0 then
    exit;

  // 3 things to update: list, tree, and grid
  parent := list[index - 1];
  parent.conceptList.add(sel.Link);
  list.DeleteByIndex(index);
  loadConcepts;
  tvConceptTree.Selected := TTreeViewItem(sel.TagObject);
  grdConcepts.SelectCell(1, flatConcepts.IndexOf(sel));
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.btnConceptUpClick(Sender: TObject);
var
  sel : TFhirCodeSystemConcept;
  parent : TFhirCodeSystemConcept;
  list : TFhirCodeSystemConceptList;
  index : integer;
begin
  if tvConceptTree.Selected = nil then
    exit;
  sel := TFhirCodeSystemConcept(tvConceptTree.Selected.TagObject);
  if not findConcept(sel, parent, list, index) then
    exit;
  // 3 things to update: list, tree, and grid
  list.Exchange(index, index-1);
  loadConcepts;
  tvConceptTree.Selected := TTreeViewItem(sel.TagObject);
  grdConcepts.SelectCell(1, flatConcepts.IndexOf(sel));
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.btnDeleteConceptClick(Sender: TObject);
var
  sel, nf, parent : TFhirCodeSystemConcept;
  list : TFhirCodeSystemConceptList;
  index, count : integer;
  s : String;
begin
  if tvConceptTree.Selected = nil then
    exit;
  sel := TFhirCodeSystemConcept(tvConceptTree.Selected.TagObject);
  if not findConcept(sel, parent, list, index) then
    exit;

  count := sel.countDescendents;
  if count = 0 then
    s := 'Delete Concept '+sel.code+'?'
  else if count = 1 then
    s := 'Delete Concept '+sel.code+' and 1 child?'
  else
    s := 'Delete Concept '+sel.code+' and it''s '+inttostr(count)+' children?';

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
        list.Remove(index);
        loadConcepts;
        if nf <> nil then
        begin
          tvConceptTree.Selected := TTreeViewItem(nf.TagObject);
          grdConcepts.SelectCell(1, flatConcepts.IndexOf(nf));
        end;
        ResourceIsDirty := true;
      end;
    end
  );
end;

procedure TCodeSystemEditorFrame.btnDeleteFilterClick(Sender: TObject);
begin
  CodeSystem.filterList.DeleteByIndex(grdFilters.Row);
  loadFilters;
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.btnDeletePropertyClick(Sender: TObject);
begin
  CodeSystem.property_List.DeleteByIndex(grdProperties.Row);
  ResourceIsDirty := true;
  loadProperties;
end;

procedure TCodeSystemEditorFrame.btnMemoCopyrightClick(Sender: TObject);
begin
  if CodeSystem.copyrightElement = nil then
    CodeSystem.copyrightElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Code System Copyright', btnMemoCopyright, edtCopyright, CodeSystem, CodeSystem.copyrightElement);
end;

procedure TCodeSystemEditorFrame.btnMemoForDescClick(Sender: TObject);
begin
  if CodeSystem.descriptionElement = nil then
    CodeSystem.descriptionElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Code System Description', btnMemoForDesc, edtDescription, CodeSystem, CodeSystem.descriptionElement);
end;

procedure TCodeSystemEditorFrame.btnMemoPurposeClick(Sender: TObject);
begin
  if CodeSystem.purposeElement = nil then
    CodeSystem.purposeElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Code System Purpose', btnMemoPurpose, edtPurpose, CodeSystem, CodeSystem.purposeElement);
end;

procedure TCodeSystemEditorFrame.buildFlatGrid(list: TFhirCodeSystemConceptList);
var
  c : TFhirCodeSystemConcept;
begin
  for c in list do
  begin
    flatConcepts.Add(c.link);
    buildFlatGrid(c.conceptList);
  end;
end;

function countConcepts(list : TFhirCodeSystemConceptList) : integer;
var
  c : TFhirCodeSystemConcept;
begin
  result := 0;
  for c in list do
    result := result + 1 + countConcepts(c.conceptList);
end;

procedure TCodeSystemEditorFrame.Button1Click(Sender: TObject);
begin
  edtConceptCount.Text := IntToStr(countConcepts(CodeSystem.conceptList));
end;

procedure TCodeSystemEditorFrame.btnNameClick(Sender: TObject);
begin
  if CodeSystem.nameElement = nil then
    CodeSystem.nameElement := TFhirString.Create;
  editStringDialog(self, 'Code System Name', btnName, edtName, CodeSystem, CodeSystem.nameElement);
end;

procedure TCodeSystemEditorFrame.btnPublisherClick(Sender: TObject);
begin
  if CodeSystem.publisherElement = nil then
    CodeSystem.publisherElement := TFhirString.Create;
  editStringDialog(self, 'Code System Publisher', btnPublisher, edtPublisher, CodeSystem, CodeSystem.publisherElement);
end;

procedure TCodeSystemEditorFrame.btnTitleClick(Sender: TObject);
begin
  if CodeSystem.titleElement = nil then
    CodeSystem.titleElement := TFhirString.Create;
  editStringDialog(self, 'Code System Title', btnTitle, edtTitle, CodeSystem, CodeSystem.titleElement);
end;

procedure TCodeSystemEditorFrame.btnSearchEndClick(Sender: TObject);
var
  i : integer;
  fcs : IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    i := grdConcepts.RowCount - 1;
    while i >= 0 do
    begin
      if matchesSearch(flatConcepts[i]) then
      begin
        grdConcepts.SelectCell(0, i);
        grdConcepts.ScrollToSelectedCell;
        exit;
      end;
      dec(i);
    end;
    beep;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TCodeSystemEditorFrame.btnSearchNextClick(Sender: TObject);
var
  i : integer;
  fcs : IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    i := grdConcepts.Row + 1;
    while i < flatConcepts.Count do
    begin
      if matchesSearch(flatConcepts[i]) then
      begin
        grdConcepts.SelectCell(0, i);
        grdConcepts.ScrollToSelectedCell;
        exit;
      end;
      inc(i);
    end;
    beep;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TCodeSystemEditorFrame.btnSearchPrevClick(Sender: TObject);
var
  i : integer;
  fcs : IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    i := grdConcepts.Row - 1;
    while i >= 0 do
    begin
      if matchesSearch(flatConcepts[i]) then
      begin
        grdConcepts.SelectCell(0, i);
        grdConcepts.ScrollToSelectedCell;
        exit;
      end;
      dec(i);
    end;
    beep;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TCodeSystemEditorFrame.btnSearchStartClick(Sender: TObject);
var
  i : integer;
  fcs : IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    i := 0;
    while i < flatConcepts.Count do
    begin
      if matchesSearch(flatConcepts[i]) then
      begin
        grdConcepts.SelectCell(0, i);
        grdConcepts.ScrollToSelectedCell;
        exit;
      end;
      inc(i);
    end;
    beep;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TCodeSystemEditorFrame.btnEditConceptClick(Sender: TObject);
var
  form : TCodeSystemConceptForm;
  sel : TFhirCodeSystemConcept;
begin
  if tvConceptTree.Selected = nil then
    exit;
  sel := TFhirCodeSystemConcept(tvConceptTree.Selected.TagObject);
  form := TCodeSystemConceptForm.create(self);
  try
    form.CodeSystem := CodeSystem.Link;
    form.Concept := sel.Clone;
    if ShowModalHack(form) = mrOk then
    begin
      grdConcepts.BeginUpdate;
      sel.code := form.Concept.code;
      sel.display := form.Concept.display;
      sel.definition := form.Concept.definition;
      sel.designationList.Assign(form.Concept.designationList);
      sel.extensionList.Assign(form.Concept.extensionList);
      sel.Property_List.Assign(form.Concept.Property_List);
      TTreeViewItem(sel.TagObject).Text := sel.Code +' "'+sel.display+'": '+polish(sel.definition);
      grdConcepts.EndUpdate;
    end;
  finally
    form.free;
  end;
end;

procedure TCodeSystemEditorFrame.btnExportClick(Sender: TObject);
var
  s : String;
  arr : Array of String;
  i : integer;
begin
  if dlgExport.Execute then
  begin
    s := ExtractFileExt(dlgExport.FileName);
    if s = '.csv' then
    begin
      SetLength(arr, 4+CodeSystem.property_List.Count);
      arr[0] := 'Level';
      arr[1] := 'Code';
      arr[2] := 'Display';
      arr[3] := 'Definition';
      for i := 0 to CodeSystem.property_List.Count - 1 do
        arr[i+4] := CodeSystem.property_List[i].code;
      produceCsv(dlgExport.FileName,
        arr,
        procedure (csv : TCSVWriter)
        var
          c : TFhirCodeSystemConcept;
          p : TFhirCodeSystemProperty;
          v : TFhirCodeSystemConceptProperty;
        begin
          for c in flatConcepts do
          begin
            csv.cell(c.TagInt);
            csv.cell(c.code);
            csv.cell(c.display);
            csv.cell(polish(c.definition));
            for p in CodeSystem.property_List do
            begin
              v := c.prop(p.code);
              if v = nil then
                csv.cell('')

              else case p.type_ of
                ConceptPropertyTypeCode, ConceptPropertyTypeString, ConceptPropertyTypeInteger:
                  csv.cell(v.value.primitiveValue);
                ConceptPropertyTypeDateTime:
                  csv.cell(TFHIRDateTime(v.value).value.toString('x'));
                ConceptPropertyTypeBoolean:
                  csv.cell(TFHIRBoolean(v.value).value);
//        ConceptPropertyTypeCoding: ;
                else
                  csv.cell('')
              end;
            end;
            csv.line;
          end;
        end
      )
    end
    else
      raise EFHIRException.create('Unknown format for file "'+dlgExport.FileName+'"');
  end;

end;

procedure TCodeSystemEditorFrame.cancel;
begin
end;


procedure TCodeSystemEditorFrame.commit;
begin
  if tvStructure.Selected = tvMetadata then
    CommitMetadata
  else if tvStructure.Selected = tvHl7 then
    commitHL7Process
  else if tvStructure.Selected = tvProperties then
    CommitProperties
  else if tvStructure.Selected = tvFilters then
    CommitFilters
  else if tvStructure.Selected = tvConcepts then
    CommitConcepts;
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.commitConcepts;
begin

end;

procedure TCodeSystemEditorFrame.commitFilters;
begin

end;

procedure TCodeSystemEditorFrame.commitHL7Process;
var
  s : String;
begin
  if edtSteward.Text = '' then
    CodeSystem.removeExtension('http://hl7.org/fhir/StructureDefinition/structuredefinition-wg')
  else
    CodeSystem.setExtensionCode('http://hl7.org/fhir/StructureDefinition/structuredefinition-wg', edtSteward.Text);

  if edtLegalese.Text = '' then
    CodeSystem.removeExtension('http://hl7.org/fhir/StructureDefinition/codesystem-legalese')
  else
    CodeSystem.setExtensionString('http://hl7.org/fhir/StructureDefinition/codesystem-legalese', edtLegalese.Text);
  if edtVDeprecated.Text = '' then
    CodeSystem.removeExtension('http://hl7.org/fhir/StructureDefinition/resource-versionDeprecated')
  else
    CodeSystem.setExtensionString('http://hl7.org/fhir/StructureDefinition/resource-versionDeprecated', edtVDeprecated.Text);
  if edtVersionPolicy.Text = '' then
    CodeSystem.removeExtension('http://hl7.org/fhir/StructureDefinition/resource-versioningPolicy')
  else
    CodeSystem.setExtensionString('http://hl7.org/fhir/StructureDefinition/resource-versioningPolicy', edtVersionPolicy.Text);

  CodeSystem.removeExtension('http://hl7.org/fhir/StructureDefinition/codesystem-openIssue');
  for s in memOpenIssues.Lines do
    if s <> '' then
      CodeSystem.addExtension('http://hl7.org/fhir/StructureDefinition/codesystem-openIssue', s);
end;

procedure TCodeSystemEditorFrame.commitMetadata;
var
  s : String;
  cc : TFHIRCodeableConcept;
  id : TFhirIdentifier;
begin
   CodeSystem.experimental := cbExperimental.IsChecked;

  CodeSystem.url := edtURL.Text;
  CodeSystem.name := edtName.Text;
  CodeSystem.title := edtTitle.Text;
  CodeSystem.version := edtVersion.Text;
  CodeSystem.publisher := edtPublisher.text;
  CodeSystem.description := edtDescription.Text;
  CodeSystem.purpose := edtPurpose.Text;
  CodeSystem.copyright := edtCopyright.Text;
  CodeSystem.status := TFhirPublicationStatusEnum(cbxStatus.ItemIndex);
  CodeSystem.date := TFslDateTime.make(dedDate.DateTime, dttzLocal);
  CodeSystem.jurisdictionList.Clear;
  cc := getJurisdiction(cbxJurisdiction.ItemIndex);
  if (cc <> nil) then
    CodeSystem.jurisdictionList.add(cc);

  if (edtIdSystem.Text <> '') or (edtIdValue.Text <> '') then
  begin
    {$IFDEF FHIR3}
    if CodeSystem.identifier = nil then
      CodeSystem.identifier := TFhirIdentifier.Create;
    id := CodeSystem.identifier;
    {$ELSE}
    if CodeSystem.identifierList.Count = 0 then
      id := CodeSystem.identifierList.Append
    else
      id := CodeSystem.identifierList[0];
    {$ENDIF}
    id.system := edtIdSystem.Text;
    id.value := edtIdValue.Text;
  end
  else
    {$IFDEF FHIR3}
    CodeSystem.identifier := nil;
    {$ELSE}
    CodeSystem.identifierList.Clear;
    {$ENDIF}

  CodeSystem.valueSet := edtValueSet.Text;
  CodeSystem.hierarchyMeaning := TFhirCodesystemHierarchyMeaningEnum(cbxHeirarchy.ItemIndex);
  {$IFDEF FHIR4}
  CodeSystem.supplements := edtSupplements.Text;
  {$ENDIF}
  CodeSystem.content := TFhirCodesystemContentModeEnum(cbxContent.ItemIndex);
  if (edtConceptCount.Text = '') or StringIsInteger32(edtConceptCount.Text) then
    CodeSystem.count := edtConceptCount.Text
  else
    raise EFHIRException.create('Integer required');
  CodeSystem.caseSensitive := cbCaseSensitive.IsChecked;
  CodeSystem.compositional := cbCompositional.IsChecked;
  CodeSystem.versionNeeded := cbNeedsVersion.IsChecked;
end;

procedure TCodeSystemEditorFrame.commitProperties;
begin

end;

constructor TCodeSystemEditorFrame.Create(owner: TComponent);
begin
  inherited;
  flatConcepts := TFslList<TFhirCodeSystemConcept>.create;
end;

destructor TCodeSystemEditorFrame.Destroy;
begin
  flatConcepts.Free;
  inherited;
end;

function TCodeSystemEditorFrame.findConcept(sel: TFhirCodeSystemConcept; var parent : TFhirCodeSystemConcept; var list: TFhirCodeSystemConceptList; var index: integer): boolean;
var
  i : integer;
begin
  parent := nil;
  list := nil;
  index := -1;
  if (sel = nil) then
    exit(false);

  if sel.TagInt = 0 then
    list := CodeSystem.conceptList
  else
  begin
    i := flatConcepts.IndexOf(sel);
    while flatConcepts[i].TagInt >= sel.TagInt do
      dec(i);
    parent := flatConcepts[i];
    list := flatConcepts[i].conceptList;
  end;
  index := list.IndexOf(sel);
  result := index > -1;
end;

function TCodeSystemEditorFrame.GetCodeSystem: TFHIRCodeSystem;
begin
  result := TFHIRCodeSystem(Resource);
end;

function displayLang(lang : string) : string;
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

function TCodeSystemEditorFrame.getJurisdiction(i: integer): TFHIRCodeableConcept;
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

procedure TCodeSystemEditorFrame.grdConceptsCellDblClick(const Column: TColumn; const Row: Integer);
begin
  btnEditConceptClick(nil);
end;

procedure TCodeSystemEditorFrame.grdConceptsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  c : TFhirCodeSystemConcept;
  p : TFhirCodeSystemProperty;
  v : TFhirCodeSystemConceptProperty;
begin
  c := flatConcepts[ARow];
  case ACol of
    0: value := c.TagInt;
    1: value := c.code;
    2: value := c.display;
    3: value := polish(c.definition);
  else
  begin
    p := CodeSystem.property_List[aCol - 4];
    v := c.prop(p.code);
    if (v <> nil) and (v.value <> nil) then
      case p.type_ of
        ConceptPropertyTypeCode, ConceptPropertyTypeString, ConceptPropertyTypeInteger:
          value := v.value.primitiveValue;
        ConceptPropertyTypeDateTime:
          if (v.value is TFHIRDateTime) then
            value := TFHIRDateTime(v.value).value.toString('x')
          else
            value := 0;
        ConceptPropertyTypeBoolean:
          if v.value is TFHIRBoolean then
            value := TFHIRBoolean(v.value).value
          else
            value := false;
//        ConceptPropertyTypeCoding: ;
      end;
  end;
  end;
end;

procedure TCodeSystemEditorFrame.grdConceptsSelChanged(Sender: TObject);
var
  sel : TFhirCodeSystemConcept;
begin
  if selchanging then
    exit;
  if grdConcepts.Row < 0 then
    updateStatus(nil)
  else
  begin
    selChanging := true;
    try
      sel := flatConcepts[grdConcepts.Row];
      tvConceptTree.selected := TTreeViewItem(sel.TagObject);
      updateStatus(sel);
    finally
      selChanging := false;
    end;
  end;
end;

procedure TCodeSystemEditorFrame.grdConceptsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  c : TFhirCodeSystemConcept;
  p : TFhirCodeSystemProperty;
  v : TFhirCodeSystemConceptProperty;
begin
  c := flatConcepts[ARow];
  case ACol of
    1: c.code := value.AsString;
    2: c.display := value.AsString;
    3: c.definition := value.AsString;
  else
  begin
    p := CodeSystem.property_List[aCol - 4];
    v := c.prop(p.code);
    if (p.type_ <> ConceptPropertyTypeBoolean) and (value.AsString = '') then
      c.deleteProp(p.code)
    else
    begin
      if v = nil then
        v := c.addProp(p.code);
      case p.type_ of
        ConceptPropertyTypeCode: v.value := TFhirCode.Create(value.AsString);
        ConceptPropertyTypeString: v.value := TFhirString.Create(value.AsString);
        ConceptPropertyTypeInteger: v.value := TFhirInteger.Create(value.AsString);
        ConceptPropertyTypeDateTime: v.value := TFhirDateTime.Create(TFslDateTime.fromFormat('x', value.AsString));
        ConceptPropertyTypeBoolean: v.value := TFhirBoolean.Create(value.AsBoolean);
//        ConceptPropertyTypeCoding: ;
      end;
    end;
  end;
  end;
  TTreeViewItem(c.TagObject).Text := c.Code +' "'+c.display+'": '+polish(c.definition);
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.grdFiltersCellClick(const Column: TColumn; const Row: Integer);
//var
//  f : TFhirCodeSystemFilter;
begin
//  if Column = grdFilters.Columns[7] then
//  begin
//    f := CodeSystem.filterList[grdFilters.Row];
//    if f.descriptionElement = nil then
//      f.descriptionElement := TFhirString.Create;
//    if editStringDialog(self, 'Code System Filter', nil, nil, CodeSystem, f.descriptionElement) then
//    begin
//      grdFilters.BeginUpdate;
//      grdFilters.EndUpdate;
//    end;
//  end;
end;

procedure TCodeSystemEditorFrame.grdFiltersGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  f : TFhirCodeSystemFilter;
  e : TFhirFilterOperatorEnum;
  s : String;
  size : TSizeF;
  ext : TFhirExtension;
begin
  size.cx := 16;
  size.cy := 16;
  f := CodeSystem.filterList[ARow];
  case aCol of
    0: value := f.code;
    1:
      begin
        s := '';
        for e := low(TFhirFilterOperatorEnum) to high(TFhirFilterOperatorEnum) do
          if e in f.&operator then
            s := s +' '+CODES_TFhirFilterOperatorEnum[e];
        value := s.trim;
      end;
    2: value := f.value;
    3: value := f.description;
    4: value := TValue.From<TBitmap>(ToolbarImages.Bitmap(size, translationsImageIndex(f.descriptionElement)));
  end;
end;

procedure TCodeSystemEditorFrame.grdFiltersSelChanged(Sender: TObject);
begin
  btnDeleteFilter.Enabled := grdFilters.Row > -1;
end;

procedure TCodeSystemEditorFrame.grdFiltersSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  f : TFhirCodeSystemFilter;
  a : TArray<String>;
  s, l : String;
  e : TFhirFilterOperatorEnumList;
  i : TFhirFilterOperatorEnum;
begin
  f := CodeSystem.filterList[ARow];
  case aCol of
    0: f.code := value.AsString;
    1:
      begin
      e := [];
      a := value.AsString.Split([' ']);
      for s in a do
        if StringArrayExistsSensitive(CODES_TFhirFilterOperatorEnum, s) then
          e := e + [TFhirFilterOperatorEnum(StringArrayIndexOfSensitive(CODES_TFhirFilterOperatorEnum, s))]
        else
        begin
          l := CODES_TFhirFilterOperatorEnum[FilterOperatorEqual];
          for i := FilterOperatorIsA to high(TFhirFilterOperatorEnum) do
            l := l +', '+ CODES_TFhirFilterOperatorEnum[i];
          raise EFHIRException.create('Unknown Operator '+s+': use a space separated set from '+l);
        end;
      f.operator := e;
      end;
    2: f.value := value.AsString;
    3: f.description := value.AsString;
  end;
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.grdPropertiesCellClick(const Column: TColumn; const Row: Integer);
var
  p : TFhirCodeSystemProperty;
begin
  if Column = grdProperties.Columns[7] then
  begin
    p := CodeSystem.property_List[grdProperties.Row];
    if p.descriptionElement = nil then
      p.descriptionElement := TFhirString.Create;
    if editStringDialog(self, 'Code System Property', nil, nil, CodeSystem, p.descriptionElement) then
    begin
      grdProperties.BeginUpdate;
      grdProperties.EndUpdate;
    end;
  end;
end;

procedure TCodeSystemEditorFrame.grdPropertiesDrawColumnBackground(
  Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
  const Bounds: TRectF; const Row: Integer; const Value: TValue;
  const State: TGridDrawStates);
begin
//  Canvas.
end;

procedure TCodeSystemEditorFrame.grdPropertiesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  p : TFhirCodeSystemProperty;
  size : TSizeF;
  ext : TFhirExtension;
begin
  p := CodeSystem.property_List[ARow];
  size.cx := 16;
  size.cy := 16;
  case aCol of
    0: value := p.code;
    1: value := p.uri;
    2: value := CODES_TFhirConceptPropertyTypeEnum[p.type_];
    3: value := p.description;
    4: begin
         ext := p.getExtensionByUrl('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties');
         if (ext <> nil) then
           value := ext.getExtensionBoolean('isMandatory')
         else
          value := false;
       end;
    5: begin
         ext := p.getExtensionByUrl('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties');
         if (ext <> nil) then
           value := ext.getExtensionString('defaultValue')
         else
          value := '';
       end;
    6: value := p.getExtensionString('http://hl7.org/fhir/StructureDefinition/codesystem-relationshipKind');
// what is this meant do? whatever it is.... it doesn't do it
//    7: value := TValue.From<TBitmap>(ToolbarImages.Bitmap(size, translationsImageIndex(p.descriptionElement)));
  end;
end;

procedure TCodeSystemEditorFrame.grdPropertiesSelChanged(Sender: TObject);
begin
  btnDeleteProperty.Enabled := grdProperties.Row > -1;
end;

procedure TCodeSystemEditorFrame.grdPropertiesSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  p : TFhirCodeSystemProperty;
begin
  p := CodeSystem.property_List[ARow];
  case aCol of
    0: p.code := value.AsString;
    1: p.uri := value.AsString;
    2: p.type_ := TFhirConceptPropertyTypeEnum(StringArrayIndexOfSensitive(CODES_TFhirConceptPropertyTypeEnum, value.AsString));
    3: p.description := value.AsString;
    4: begin
         if value.AsBoolean then
           p.forceExtension('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties').setExtensionBoolean('isMandatory', true)
         else if p.hasExtension('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties') then
           if not p.getExtensionByUrl('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties').hasExtension('defaultValue') then
             p.removeExtension('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties')
           else
             p.getExtensionByUrl('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties').removeExtension('isMandatory');
       end;
    5: begin
         if value.AsString <> '' then
           p.forceExtension('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties').setExtensionString('defaultValue', value.AsString)
         else if p.hasExtension('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties') then
           if not p.getExtensionByUrl('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties').hasExtension('isMandatory') then
             p.removeExtension('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties')
           else
             p.getExtensionByUrl('http://hl7.org/fhir/StructureDefinition/codesystem-mif-extended-properties').removeExtension('defaultValue');
       end;
    6: if value.AsString = '' then
         p.removeExtension('http://hl7.org/fhir/StructureDefinition/codesystem-relationshipKind')
       else
         p.setExtension('http://hl7.org/fhir/StructureDefinition/codesystem-relationshipKind', TFHIRCode.Create(value.AsString));
  end;
  ResourceIsDirty := true;
end;

procedure TCodeSystemEditorFrame.inputChanged(Sender: TObject);
begin
  if not Loading then
    commit;
end;

procedure TCodeSystemEditorFrame.load;
begin
  tvStructure.Selected := tvMetadata;
  tvStructure.ExpandAll;
  tvStructureClick(nil);
end;

procedure TCodeSystemEditorFrame.loadConcepts;
var
  c : TFhirCodeSystemConcept;
  prop : TFhirCodeSystemProperty;
  col : TColumn;
  i : integer;
begin
  flatConcepts.Clear;
  tvConceptTree.Clear;
  for c in CodeSystem.conceptList do
    addConceptToTree(nil, c);
  buildFlatGrid(CodeSystem.conceptList);
  grdConcepts.RowCount := 0;
  grdConcepts.RowCount := flatConcepts.Count;
  tvConceptTree.ExpandAll;

  for i := grdConcepts.ColumnCount - 1 downto 4 do
  begin
    col := grdConcepts.Columns[i];
    grdConcepts.RemoveObject(col);
  end;

  for prop in CodeSystem.property_List do
    case prop.type_ of
      ConceptPropertyTypeBoolean :
        begin
          col := TCheckColumn.Create(self);
          col.Header := prop.code;
          grdConcepts.AddObject(col);
        end;
      else // ConceptPropertyTypeCode, ConceptPropertyTypeString,  ConceptPropertyTypeInteger, ConceptPropertyTypeDateTime:
        begin
          col := TStringColumn.Create(self);
          col.Header := prop.code;
          grdConcepts.AddObject(col);
        end;
//      ConceptPropertyTypeCoding, {@enum.value ConceptPropertyTypeCoding  }
    end;
end;

procedure TCodeSystemEditorFrame.loadFilters;
begin
  grdFilters.RowCount := 0;
  grdFilters.RowCount := CodeSystem.filterList.Count;
  grdFiltersSelChanged(self);
  adjustLastColWidth(grdFilters, 60);
end;

procedure TCodeSystemEditorFrame.loadHL7Process;
var
  ext : TFhirExtension;
begin
  if CodeSystem.hasExtension('http://hl7.org/fhir/StructureDefinition/structuredefinition-wg') then
    edtSteward.Text := prepEdit(CodeSystem.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-wg'))
  else
    edtSteward.Text := '';

  edtLegalese.Text := prepEdit(CodeSystem.getExtensionString('http://hl7.org/fhir/StructureDefinition/codesystem-legalese'));
  edtVDeprecated.Text := prepEdit(CodeSystem.getExtensionString('http://hl7.org/fhir/StructureDefinition/resource-versionDeprecated'));
  edtVersionPolicy.Text := prepEdit(CodeSystem.getExtensionString('http://hl7.org/fhir/StructureDefinition/resource-versioningPolicy'));

  memOpenIssues.Text := '';
  for ext in CodeSystem.extensionList do
  begin
    if ext.url = 'http://hl7.org/fhir/StructureDefinition/codesystem-openIssue' then
      memOpenIssues.lines.Add(ext.value.primitiveValue);
  end;
end;

procedure TCodeSystemEditorFrame.loadMetadata;
var
  url : TFHIRUri;
  id : TFhirIdentifier;
begin
  cbExperimental.IsChecked := CodeSystem.experimental;

  edtURL.Text := CodeSystem.url;
  edtName.Text := CodeSystem.name;
  btnName.ImageIndex := translationsImageIndex(CodeSystem.nameElement);
  edtTitle.Text := prepEdit(CodeSystem.title);
  btnTitle.ImageIndex := translationsImageIndex(CodeSystem.titleElement);
  edtVersion.Text := CodeSystem.version;
  edtPublisher.text := CodeSystem.publisher;
  btnPublisher.ImageIndex := translationsImageIndex(CodeSystem.publisherElement);
  edtDescription.Text := prepEdit(CodeSystem.description);
  btnMemoForDesc.ImageIndex := translationsImageIndex(CodeSystem.descriptionElement);
  edtPurpose.Text := prepEdit(CodeSystem.purpose);
  btnMemoPurpose.ImageIndex := translationsImageIndex(CodeSystem.purposeElement);
  edtCopyright.Text := prepEdit(CodeSystem.copyright);
  btnMemoCopyright.ImageIndex := translationsImageIndex(CodeSystem.copyrightElement);
  cbxStatus.ItemIndex := ord(CodeSystem.status);
  if CodeSystem.dateElement = nil then
    dedDate.Text := ''
  else
    dedDate.DateTime := CodeSystem.date.DateTime;
  cbxJurisdiction.ItemIndex := readJurisdiction;

  {$IFDEF FHIR3}
  id := CodeSystem.identifier;
  {$ELSE}
  id := nil;
  if not CodeSystem.identifierList.IsEmpty then
    id := CodeSystem.identifierList[0];
  {$ENDIF}

  if id <> nil then
  begin
    edtIdSystem.Text := id.system;
    edtIdValue.Text := id.value;
  end
  else
  begin
    edtIdSystem.Text := '';
    edtIdValue.Text := '';
  end;
  edtValueSet.Text := CodeSystem.valueSet;
  {$IFDEF FHIR4}
  edtSupplements.Text := CodeSystem.supplements;
  if cbxContent.Items.IndexOf('Supplement') = -1 then
    cbxContent.Items.Add('Supplement');
  {$ELSE}
  edtSupplements.Enabled := false;
  {$ENDIF}
  cbxHeirarchy.ItemIndex := ord(CodeSystem.hierarchyMeaning);
  cbxContent.ItemIndex := ord(CodeSystem.content);
  edtConceptCount.Text := CodeSystem.count;
  cbCaseSensitive.IsChecked := CodeSystem.caseSensitive;
  cbCompositional.IsChecked := CodeSystem.compositional;
  cbNeedsVersion.IsChecked := CodeSystem.versionNeeded;
end;


procedure TCodeSystemEditorFrame.loadProperties;
begin
  grdProperties.RowCount := 0;
  grdProperties.RowCount := CodeSystem.property_List.Count;
  adjustLastColWidth(grdProperties, 60);
  grdPropertiesSelChanged(self);
end;

function TCodeSystemEditorFrame.matchesSearch(concept: TFhirCodeSystemConcept): boolean;
begin
  result := concept.code.Contains(edtSearch.Text) or concept.display.Contains(edtSearch.Text) or concept.definition.Contains(edtSearch.Text);
end;

function TCodeSystemEditorFrame.readJurisdiction: Integer;
var
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
begin
  result := -1;
  for cc in CodeSystem.jurisdictionList do
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


procedure TCodeSystemEditorFrame.tvConceptTreeChange(Sender: TObject);
var
  sel : TFhirCodeSystemConcept;
begin
  if selchanging then
    exit;
  if tvConceptTree.Selected = nil then
    updateStatus(nil)
  else
  begin
    selChanging := true;
    try
      sel := TFhirCodeSystemConcept(tvConceptTree.Selected.TagObject);
      grdConcepts.Row := flatConcepts.IndexOf(sel);
      grdConcepts.Col := 1;
      grdConcepts.SelectCell(grdConcepts.Col, grdConcepts.Row);
      updateStatus(sel);
    finally
      selChanging := false;
    end;
  end;
end;

procedure TCodeSystemEditorFrame.tvStructureClick(Sender: TObject);
begin
  Loading := true;
  try
    if tvStructure.Selected = tvMetadata then
    begin
      tbStructure.ActiveTab := tbMetadata;
      loadMetadata;
    end
    else if tvStructure.Selected = tvHl7 then
    begin
      tbStructure.ActiveTab := tbHl7;
      loadHL7Process;
    end
    else if tvStructure.Selected = tvProperties then
    begin
      tbStructure.ActiveTab := tbProperties;
      loadProperties;
    end
    else if tvStructure.Selected = tvFilters then
    begin
      tbStructure.ActiveTab := tbFilters;
      loadFilters;
    end
    else if tvStructure.Selected = tvConcepts then
    begin
      tbStructure.ActiveTab := tbConcepts;
      loadConcepts;
    end
  finally
    Loading := false;
  end;
end;

procedure TCodeSystemEditorFrame.updateStatus(sel: TFhirCodeSystemConcept);
var
  list : TFhirCodeSystemConceptList;
  parent : TFhirCodeSystemConcept;
  i : integer;
begin
  if not findConcept(sel, parent, list, i) then
  begin
    btnConceptUp.Enabled := false;
    btnConceptDown.Enabled := false;
    btnConceptIn.Enabled := false;
    btnConceptOut.Enabled := false;
    btnEditConcept.Enabled := false;
    btnDeleteConcept.Enabled := false;
  end
  else
  begin
    btnConceptUp.Enabled := i > 0;
    btnConceptDown.Enabled := i < list.Count - 1;
    btnConceptIn.Enabled := sel.TagInt > 0;
    btnConceptOut.Enabled := i > 0;
    btnEditConcept.Enabled := true;
    btnDeleteConcept.Enabled := true;
  end;
end;

end.


