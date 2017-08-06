unit FetchResourceForm;


{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

// todo:
//   look ahead on search parameters on past values by name
//   better date entry

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.ExtCtrls, NppForms, FHIRClient,
  FHIRResources, FHIRTypes, FHIRUtilities, FHIRBase, AdvObjects, AdvGenerics, Math, StringSupport,
  EncodeSupport, clipbrd, parsemap, Generics.Collections,  FHIRProfileUtilities,
  Vcl.Samples.Spin, fhirpath;

const
  MIN_COL_WIDTH = 260;
  SEARCH_PANEL_HEIGHT = 26;

type
  TFetchResourceFrm = class;

  TSearchEntryPanel = class (TAdvObject)
  private
    form : TFetchResourceFrm;
    definition : TFhirCapabilityStatementRestResourceSearchParam;
    panel : TPanel;
    lbl : TLabel;
    edit, edit2 : TEdit;
    prefix, modList : TComboBox;

    procedure build;
    procedure buildEdit(typeNote : String);
    procedure buildNumberEdit(typeNote : String; dateQualifiers : boolean);
    procedure buildQuantityEdit;
    procedure readParamValue(s : String);
  public
    constructor create(form : TFetchResourceFrm; definition : TFhirCapabilityStatementRestResourceSearchParam);
    destructor Destroy; override;

    procedure AdjustPosition(top, left, width : integer);
    procedure addToUrl(b : TStringBuilder);
    procedure readFromURL(pm : TParseMap);
  end;

  TFetchResourceFrm = class(TNppForm)
    Panel1: TPanel;
    Panel2: TPanel;
    pnlCriteria: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Label1: TLabel;
    cbxType: TComboBox;
    btnCancel: TButton;
    btnOpen: TButton;
    vtMatches: TVirtualStringTree;
    ScrollBox1: TScrollBox;
    Panel3: TPanel;
    btnSearchAll: TButton;
    btnCopy: TButton;
    btnPaste: TButton;
    btnFirst: TButton;
    btnPrev: TButton;
    btnNext: TButton;
    btnLast: TButton;
    btnSearchPaged: TButton;
    Label2: TLabel;
    cbxSort: TComboBox;
    Label3: TLabel;
    edtPageCount: TSpinEdit;
    cbSearchOrder: TCheckBox;
    rbXml: TRadioButton;
    rbJson: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnSearchAllClick(Sender: TObject);
    procedure btnSearchPagedClick(Sender: TObject);
    procedure vtMatchesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtMatchesAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtMatchesRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtMatchesDblClick(Sender: TObject);
  private
    { Private declarations }
    FClient : TFhirHTTPClient;
    FCapabilityStatement : TFHIRCapabilityStatement;
    FProfiles : TProfileManager;

    FType : TFhirResourceType;
    FSearchItems : TAdvList<TSearchEntryPanel>;
    FColumns : TDictionary<String, String>;
    FMatches : TAdvList<TFHIRResource>;
    FLinks : TDictionary<String, String>;
    FSelectedId: String;
    FSelectedType: TFhirResourceType;

    procedure SetClient(const Value: TFhirHTTPClient);
    procedure SetCapabilityStatement(const Value: TFHIRCapabilityStatement);

    // choosing type
    procedure loadTypeCombo;
    procedure changeSelectedType;

    // search parameters
    procedure ClearSearchItems;
    procedure AddSearchAll;
    procedure AddSearch(rr : TFhirCapabilityStatementRestResource);
    procedure loadSortCombo;
    function createSearchParam(name, definition : String; type_ : TFhirSearchParamTypeEnum) : TFhirCapabilityStatementRestResourceSearchParam;
    procedure SetProfiles(const Value: TProfileManager);
    procedure layoutSearchParameters;
    function asURL : String;
    procedure readURL(pm : TParseMap);
    procedure fetchUrl(s : String);

    // columns
    procedure clearColumns;
    function ignoreColumn(st : TFhirStructureDefinition; ed : TFhirElementDefinition) : boolean;
    procedure AddColumn(name, path : String);

    // search
    procedure clearSearch;
    procedure dosearch(url : String; all : boolean);
    function GetCell(res : TFhirResource; path : String ) : String;
  public
    { Public declarations }
    property Client : TFhirHTTPClient read FClient write SetClient;
    property Conformance : TFHIRCapabilityStatement read FCapabilityStatement write SetCapabilityStatement;
    property Profiles : TProfileManager read FProfiles write SetProfiles;

    property SelectedType : TFhirResourceType read FSelectedType write FSelectedType;
    property SelectedId : String read FSelectedId write FSelectedId;
  end;

var
  FetchResourceFrm: TFetchResourceFrm;

implementation

{$R *.dfm}

function tail(s : String) : String;
begin
  result := s.Substring(s.LastIndexOf('.')+1);
end;

function TFetchResourceFrm.ignoreColumn(st : TFhirStructureDefinition; ed : TFhirElementDefinition) : boolean;
var
  edx : TFhirElementDefinition;
begin
  if (ed.path.EndsWith('.meta')) then
    result := true
  else
  begin
    result := false;
    for edx in st.snapshot.elementList do
      if edx.path.StartsWith(ed.path+'.') then
        result := false;
  end;
end;

procedure TFetchResourceFrm.AddColumn(name, path: String);
var
  col : TVirtualTreeColumn;
begin
  if not FColumns.ContainsKey(name) then
  begin
    FColumns.Add(name, path);
    col := vtMatches.Header.Columns.Add;
    col.Text := name;
  end;
end;

procedure TFetchResourceFrm.AddSearch(rr: TFhirCapabilityStatementRestResource);
  function ignore(name : String) : boolean;
  begin
    result := StringArrayExistsInsensitive(['_sort', '_filter', '_count'],  name);
  end;
var
  n : boolean;
  sp : TFhirCapabilityStatementRestResourceSearchParam;
  st : TFhirStructureDefinition;
  ed : TFhirElementDefinition;
begin
  n := false;
  for sp in rr.searchParamList do
    n := n or (sp.name = '_id');
  if not n then
    FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_id', 'Resource id', SearchParamTypeToken)));
  for sp in rr.searchParamList do
    if not ignore(sp.name) then
      FSearchItems.Add(TSearchEntryPanel.create(self, sp.Link));
  loadSortCombo;
  FormResize(nil);
  ClearColumns;
  AddColumn('id', 'id');
  AddColumn('version', 'meta.versionId');
  AddColumn('date', 'meta.lastUpdated');
  AddColumn('profile', 'meta.profile');
  AddColumn('security', 'meta.security');
  if not Profiles.getProfileStructure(nil, 'http://hl7.org/fhir/StructureDefinition/'+CODES_TFHIRResourceTypesEnum[rr.type_], st) then
    AddColumn('text', 'text.div')
  else
    try
      for ed in st.snapshot.elementList do
        if ed.path.Contains('.') and ed.isSummary and not ignoreColumn(st, ed) then
          AddColumn(tail(ed.path), ed.path);
    finally
      st.Free;
    end;
end;

procedure TFetchResourceFrm.AddSearchAll;
begin
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_id', 'Resource id', SearchParamTypeToken)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_text', 'Search by text content (narrative)', SearchParamTypeString)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_content', 'Search by text content (all elements)', SearchParamTypeString)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_lastUpdated', 'Search by date of last update', SearchParamTypeDate)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_profile', 'Search by Profile', SearchParamTypeToken)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_security', 'Search by security label', SearchParamTypeToken)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_tag', 'Search by workflow tag', SearchParamTypeToken)));
  FormResize(nil);
  loadSortCombo;

  ClearColumns;
  AddColumn('type', '@type');
  AddColumn('id', 'id');
  AddColumn('version', 'meta.versionId');
  AddColumn('date', 'meta.lastUpdated');
  AddColumn('profile', 'meta.profile');
  AddColumn('security', 'meta.security');
  AddColumn('tag', 'meta.tag');
  AddColumn('text', 'text.div');
end;

function TFetchResourceFrm.asURL: String;
var
  b : TStringBuilder;
  se : TSearchEntryPanel;
begin
  b := TStringBuilder.Create;
  try
    for se in FSearchItems do
      se.addToUrl(b);
    if b.Length > 0 then
      b.Append('&');
    b.Append('_count=');
    b.Append(edtPageCount.Value);
    if cbxSort.Text <> '' then
    begin
      if cbSearchOrder.Checked then
        b.Append('_sort:desc=')
      else
        b.Append('_sort=');
      b.Append(cbxSort.Text);
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

procedure TFetchResourceFrm.btnCopyClick(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := '?'+asURL;
  Clipboard.Close;
end;

procedure TFetchResourceFrm.btnPasteClick(Sender: TObject);
var
  pm : TParseMap;
  s : String;
begin
  Clipboard.Open;
  s := Clipboard.AsText;
  Clipboard.Close;
  if (s.StartsWith(FClient.url)) and not s.Contains('?') then
    fetchUrl(s.Substring(FClient.url.Length+1))
  else
  begin
    if s.Contains('?') then
      s := s.Substring(s.IndexOf('?')+1);
    pm := TParseMap.create(s);
    try
      readURL(pm);
    finally
      pm.Free;
    end;
  end;
end;

procedure TFetchResourceFrm.btnSearchAllClick(Sender: TObject);
begin
  doSearch(asURL, true);
end;

procedure TFetchResourceFrm.btnSearchPagedClick(Sender: TObject);
begin
  doSearch(asURL, false);
end;

procedure TFetchResourceFrm.cbxTypeChange(Sender: TObject);
begin
  changeSelectedType;
end;

procedure TFetchResourceFrm.changeSelectedType;
var
  obj : TFHIRObject;
begin
  ClearSearchItems;
  clearColumns;
  clearSearch;
  obj := cbxType.Items.Objects[cbxType.ItemIndex] as TFHIRObject;
  if obj is TFhirCapabilityStatement then
  begin
    FType := frtNull;
    AddSearchAll
  end
  else
  begin
    FType := ResourceTypeByName(CODES_TFHIRResourceTypesEnum[(obj as TFhirCapabilityStatementRestResource).type_]);
    AddSearch(obj as TFhirCapabilityStatementRestResource);
  end;
  btnCopy.Enabled := true;
  btnPaste.Enabled := true;
end;

procedure TFetchResourceFrm.clearColumns;
begin
  FColumns.Clear;
  vtMatches.Header.Columns.Clear;
end;

procedure TFetchResourceFrm.clearSearch;
begin
  FMatches.Clear;
  vtMatches.RootNodeCount := 0;
  vtMatches.Invalidate;
  btnFirst.Enabled := false;
  btnNext.Enabled := false;
  btnPrev.Enabled := false;
  btnLast.Enabled := false;
end;

procedure TFetchResourceFrm.ClearSearchItems;
begin
  FSearchItems.Clear;
end;

function TFetchResourceFrm.createSearchParam(name, definition: String; type_: TFhirSearchParamTypeEnum): TFhirCapabilityStatementRestResourceSearchParam;
begin
  result := TFhirCapabilityStatementRestResourceSearchParam.Create;
  try
    result.name := name;
    result.definition := definition;
    result.type_ := type_;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFetchResourceFrm.dosearch(url: String; all : boolean);
var
  bundle : TFHIRBundle;
  entry : TFhirBundleEntry;
  link : TFhirBundleLink;
begin
  clearSearch;
  bundle := FClient.search(FType, all, url+'&_summary=true');
  try
    for entry in bundle.entryList do
      if ((entry.search = nil) or (entry.search.mode = SearchEntryModeMatch)) and (entry.resource <> nil) and ((entry.resource.ResourceType = FType) or (FType = frtNull)) then
        FMatches.Add(entry.resource.Link);
    for link in bundle.link_List do
      if link.relation = 'first' then
      begin
        FLinks.Add('first', link.url);
        btnFirst.Enabled := true;
      end
      else if link.relation = 'next' then
      begin
        FLinks.Add('next', link.url);
        btnNext.Enabled := true;
      end
      else if link.relation = 'previous' then
      begin
        FLinks.Add('prev', link.url);
        btnPrev.Enabled := true;
      end
      else if link.relation = 'last' then
      begin
        FLinks.Add('last', link.url);
        btnLast.Enabled := true;
      end;
    vtMatches.RootNodeCount := FMatches.Count;
    vtMatches.Invalidate;
  finally
    bundle.Free;
  end;
end;

procedure TFetchResourceFrm.fetchUrl(s: String);
var
  pm : TParseMap;
  a : TArray<String>;
  bundle : TFHIRBundle;
begin
  pm := TParseMap.create('');
  try
    readURL(pm);
  finally
    pm.free;
  end;
  clearSearch;
  clearColumns;
  a := s.Split(['/']);
  if (Length(a) >= 1) then
  begin
    cbxType.ItemIndex := cbxType.Items.IndexOf(a[0]);
    changeSelectedType;
  end;
  if (Length(a) >= 2) and (cbxType.ItemIndex > -1) then
  begin
    FMatches.Add(FClient.readResource(ResourceTypeByName(cbxType.Text), a[1]));
    btnFirst.Enabled := false;
    btnNext.Enabled := false;
    btnPrev.Enabled := false;
    btnLast.Enabled := false;
    vtMatches.RootNodeCount := FMatches.Count;
    vtMatches.Invalidate;
  end;
end;

procedure TFetchResourceFrm.FormCreate(Sender: TObject);
begin
  FSearchItems := TAdvList<TSearchEntryPanel>.create;
  FColumns := TDictionary<String, String>.create;
  FMatches := TAdvList<TFHIRResource>.create;
  FLinks := TDictionary<String, String>.create;
end;

procedure TFetchResourceFrm.FormDestroy(Sender: TObject);
begin
  FClient.free;
  FCapabilityStatement.free;
  FSearchItems.Free;
  FProfiles.Free;
  FColumns.Free;
  FMatches.Free;
  FLinks.Free;
end;

function enough(v, i: Integer) : integer;
begin
  if v mod i = 0 then
    result := v div i
  else
    result := v div i + 1;
end;
procedure TFetchResourceFrm.FormResize(Sender: TObject);
begin
  layoutSearchParameters;
end;

procedure TFetchResourceFrm.FormShow(Sender: TObject);
begin
  loadTypeCombo;
end;

function TFetchResourceFrm.GetCell(res: TFhirResource; path: String): String;
var
  query : TFHIRPathEngine;
begin
  query := TFHIRPathEngine.create(nil);
  try
    result := query.evaluateToString(nil, res, path);
  finally
    query.free;
  end;
end;

procedure TFetchResourceFrm.layoutSearchParameters;
var
  maxCols, naturalRows, naturalCols, actualCols, actualRows, width, left, top, i, c : integer;
begin
  if FSearchItems.Count > 0 then
  begin
    maxCols := (ScrollBox1.ClientWidth  - 20 {vertical scrollbar}) div MIN_COL_WIDTH;
    naturalRows := ScrollBox1.ClientHeight div SEARCH_PANEL_HEIGHT;
    naturalCols := enough(FSearchItems.Count, naturalRows);
    actualCols := min(maxCols, naturalCols);
    actualRows := enough(FSearchItems.Count, actualCols);
    width := (ScrollBox1.ClientWidth  - 20 {vertical scrollbar}) div actualCols;
    top := 0;
    left := 0;
    c := 0;
    for i := 0 to FSearchItems.Count - 1 do
    begin
      FSearchItems[i].AdjustPosition(top, left, width);
      inc(c);
      inc(top, SEARCH_PANEL_HEIGHT);
      if c = actualRows then
      begin
        c := 0;
        left := left + width;
        top := 0;
      end;
    end;
  end;
end;

procedure TFetchResourceFrm.loadSortCombo;
var
  se : TSearchEntryPanel;
begin
  cbxSort.Items.Clear;
  cbxSort.Items.Add('');
  for se in FSearchItems do
    cbxSort.Items.Add(se.definition.name);
end;

procedure TFetchResourceFrm.loadTypeCombo;
var
  r : TFhirCapabilityStatementRest;
  rr : TFhirCapabilityStatementRestResource;
  int : TFhirCapabilityStatementRestResourceInteraction;
  it : TFhirCapabilityStatementRestInteraction;
  any, ok : boolean;
  fmt : TFHIRCode;
begin
  cbxType.Items.Clear;
  any := false;
  for r in FCapabilityStatement.restList do
    if r.mode = RestfulCapabilityModeServer then
    begin
      for it in r.interactionList do
        if it.code = SystemRestfulInteractionSearchSystem then
          cbxType.Items.AddObject('All Types', FCapabilityStatement);
      for rr in r.resourceList do
      begin
        ok := false;
        for int in rr.interactionList do
          if (int.code in [TypeRestfulInteractionRead, TypeRestfulInteractionSearchType]) then
            ok := true;
        if ok then
          cbxType.Items.AddObject(CODES_TFHIRResourceTypesEnum[rr.type_], rr);
      end;
    end;
  for fmt in FCapabilityStatement.formatList do
    if fmt.value.ToLower.Contains('xml') then
      rbXml.Enabled := true
    else if fmt.value.ToLower.Contains('json') then
      rbJson.Enabled := true;
  if not rbJson.Enabled then
    rbXml.Checked := true
  else
    rbJson.Checked := true;
end;

procedure TFetchResourceFrm.readURL(pm: TParseMap);
var
  se : TSearchEntryPanel;
begin
  for se in FSearchItems do
    se.readFromURL(pm);
  if pm.varExists('_sort') then
  begin
    cbxSort.itemindex := cbxSort.Items.IndexOf(pm.GetVar('_sort'));
    cbSearchOrder.Checked := false;
  end else if pm.varExists('_sort:asc') then
  begin
    cbxSort.itemindex := cbxSort.Items.IndexOf(pm.GetVar('_sort:asc'));
    cbSearchOrder.Checked := false;
  end else if pm.varExists('_sort:desc') then
  begin
    cbxSort.itemindex := cbxSort.Items.IndexOf(pm.GetVar('_sort:desc'));
    cbSearchOrder.Checked := true;
  end;
  if (pm.VarExists('_count')) then
    edtPageCount.Value := StrToIntDef(pm.GetVar('_count'), 20);
end;

procedure TFetchResourceFrm.SetClient(const Value: TFhirHTTPClient);
begin
  FClient.Free;
  FClient := Value;
end;

procedure TFetchResourceFrm.SetCapabilityStatement(const Value: TFhirCapabilityStatement);
begin
  FCapabilityStatement.Free;
  FCapabilityStatement := Value;
end;

procedure TFetchResourceFrm.SetProfiles(const Value: TProfileManager);
begin
  FProfiles.Free;
  FProfiles := Value;
end;

procedure TFetchResourceFrm.vtMatchesAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  res : TFhirResource;
begin
  res := FMatches[node.Index];
  SelectedType := res.ResourceType;
  SelectedId := res.id;
  btnOpen.Enabled := true;
end;

procedure TFetchResourceFrm.vtMatchesDblClick(Sender: TObject);
begin
  if btnOpen.Enabled then
    ModalResult := mrOK;
end;

procedure TFetchResourceFrm.vtMatchesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  res : TFhirResource;
  path : String;
begin
  res := FMatches[node.Index];
  path := FColumns[vtMatches.Header.Columns[Column].Text];
  CellText := getCell(res, path);
end;

procedure TFetchResourceFrm.vtMatchesRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnOpen.Enabled := false;
end;

{ TSearchEntryPanel }

constructor TSearchEntryPanel.create(form: TFetchResourceFrm; definition: TFhirCapabilityStatementRestResourceSearchParam);
begin
  inherited create;
  self.form := form;
  self.definition := definition;
  build;
end;

destructor TSearchEntryPanel.destroy;
begin
  lbl.Free;
  modList.Free;
  panel.Free;
  definition.Free;
  inherited;
end;

procedure TSearchEntryPanel.readFromURL(pm: TParseMap);
var
  ok : boolean;
  s : String;
begin
  if pm.VarExists(definition.name) then
    readParamValue(pm.GetVar(definition.name))
  else
  begin
    ok := false;
    if (modList <> nil) then
      for s in modList.Items do
        if pm.VarExists(definition.name+':'+s) then
        begin
          ok := true;
          modList.ItemIndex := modlist.Items.IndexOf(s);
          readParamValue(pm.GetVar(definition.name+':'+s));
        end;
    if not ok then
      readParamValue('');
  end;
end;

procedure TSearchEntryPanel.readParamValue(s: String);
var
  i, c : integer;
  a : TArray<String>;
begin
  case definition.type_ of
    SearchParamTypeString, SearchParamTypeToken, SearchParamTypeComposite, SearchParamTypeUri, SearchParamTypeReference:
      edit.text := s;
    SearchParamTypeNumber, SearchParamTypeDate, SearchParamTypeQuantity:
      begin
        c := -1;
        for i := 0 to prefix.Items.Count - 1 do
          if s.StartsWith(prefix.Items[i].Substring(0, 2)) then
            c := i;
        if (c = -1) then
          edit.text := s
        else
        begin
          prefix.ItemIndex := c;
          edit.text := s.Substring(2);
        end;
        if (definition.type_ = SearchParamTypeQuantity) then
        begin
          s := edit.text;
          if (s.Contains('|')) then
          begin
            a := s.Split(['|']);
            edit.Text := a[0];
            if (length(a) >= 2) then
              edit2.Text := a[2];
          end;
        end;
      end;
  end;
end;

procedure TSearchEntryPanel.build;
var
  hasMod : boolean;
  modc : TFhirSearchModifierCodeEnum;
begin
  panel := TPanel.Create(form);
  panel.Parent := form.ScrollBox1;
  // the correct values will be assigned elsewhere, and pushed into adjustposition
  panel.Top := 0;
  panel.Left := 0;
  panel.Width := 200;
  panel.Height := SEARCH_PANEL_HEIGHT;
  panel.BevelOuter := bvNone;
  panel.BevelInner := bvNone;

  lbl := TLabel.Create(form);
  lbl.Parent := panel;
  lbl.Caption := definition.name+':';
  lbl.ParentShowHint := false;
  lbl.ShowHint := true;
  lbl.Hint := definition.documentation;
  lbl.Top := 3;
  lbl.Left := 5;
  lbl.Width := form.Canvas.TextWidth(lbl.Caption);

//  todo: fetch the search parameter definitions?
//  hasMod := false;
//  for modc := low(TFhirSearchModifierCodeEnum) to high(TFhirSearchModifierCodeEnum) do
//    if modc in definition.modifier then
      hasMod := true;

  case definition.type_ of
    SearchParamTypeString: buildEdit('Text to search by');
    SearchParamTypeToken: buildEdit('Code or text to search by');
    SearchParamTypeComposite: buildEdit('Composite value to search by');
    SearchParamTypeUri: buildEdit('URI to search by');
    SearchParamTypeReference: buildEdit('ID to search by');
    SearchParamTypeNumber: buildNumberEdit('Number to search by', false);
    SearchParamTypeDate: buildNumberEdit('Date to search by (variable precision, in the format yyyy-mm-ddThh:mm:ss[TS], where TX is Z or +/-hh:mm)', true);
    SearchParamTypeQuantity: buildQuantityEdit;
  end;
  if (hasMod) then
  begin
    modList := TComboBox.Create(form);
    modList.parent := panel;
    modList.Left := panel.Width - 55;
    modList.Top := edit.Top;
    modlist.Width := 50;
    modlist.Style := csDropDownList;
    modlist.Items.Add('');
    for modc := low(TFhirSearchModifierCodeEnum) to high(TFhirSearchModifierCodeEnum) do
// todo - use search parameter definition      if modc in definition.modifier then
        modlist.Items.Add(':'+CODES_TFhirSearchModifierCodeEnum[modc]);
  end;
end;


procedure TSearchEntryPanel.buildEdit(typeNote : String);
begin
  edit := TEdit.Create(form);
  edit.Parent := panel;
  edit.Left := lbl.left + lbl.Width + 5;
  edit.ParentShowHint := false;
  edit.ShowHint := true;
  edit.Hint := typeNote;
  edit.Width := 100;
  edit.Text := '';
end;

procedure TSearchEntryPanel.buildNumberEdit(typeNote: String; dateQualifiers : boolean);
begin
  prefix := TComboBox.Create(form);
  prefix.Parent := panel;
  prefix.Left := lbl.left + lbl.Width + 5;
  prefix.ParentShowHint := false;
  prefix.ShowHint := true;
  prefix.Hint := 'Criteria for this value';
  prefix.Width := 40;
  prefix.Style := csDropDownList;
  SendMessage(prefix.Handle, CB_SETDROPPEDWIDTH, 200, 0);
  prefix.items.Add('eq - equals');
  prefix.items.Add('ne - not equals');
  prefix.items.Add('gt - greater than');
  prefix.items.Add('lt - less than');
  prefix.items.Add('ge - greater or equal');
  prefix.items.Add('le - less or equal');
  if (dateQualifiers) then
  begin
    prefix.items.Add('sa - starts before');
    prefix.items.Add('eb - ends after');
  end;
  prefix.items.Add('ap - Approximately');
  prefix.ItemIndex := 0;
  prefix.DropDownCount := prefix.Items.Count;

  edit := TEdit.Create(form);
  edit.Parent := panel;
  edit.Left := prefix.left + prefix.Width + 5;
  edit.ParentShowHint := false;
  edit.ShowHint := true;
  edit.Hint := typeNote;
  edit.Width := 100;
  edit.Text := '';
end;

procedure TSearchEntryPanel.buildQuantityEdit;
begin
  prefix := TComboBox.Create(form);
  prefix.Parent := panel;
  prefix.Left := lbl.left + lbl.Width + 5;
  prefix.ParentShowHint := false;
  prefix.ShowHint := true;
  prefix.Hint := 'Criteria for this value';
  prefix.Width := 40;
  prefix.Style := csDropDownList;
  SendMessage(prefix.Handle, CB_SETDROPPEDWIDTH, 200, 0);
  prefix.items.Add('eq - equals');
  prefix.items.Add('ne - not equals');
  prefix.items.Add('gt - greater than');
  prefix.items.Add('lt - less than');
  prefix.items.Add('ge - greater or equal');
  prefix.items.Add('le - less or equal');
  prefix.items.Add('sa - starts before');
  prefix.items.Add('eb - ends after');
  prefix.items.Add('ap - Approximately');
  prefix.ItemIndex := 0;
  prefix.DropDownCount := prefix.Items.Count;

  edit := TEdit.Create(form);
  edit.Parent := panel;
  edit.Left := prefix.left + prefix.Width + 5;
  edit.ParentShowHint := false;
  edit.ShowHint := true;
  edit.Hint := 'Value of the quantity';
  edit.Width := 500;
  edit.Text := '';

  edit2 := TEdit.Create(form);
  edit2.Parent := panel;
  edit2.Left := edit.left + edit.Width + 5;
  edit2.ParentShowHint := false;
  edit2.ShowHint := true;
  edit2.Hint := 'Unit of the quantity';
  edit2.Width := 500;
  edit2.Text := '';
end;

procedure TSearchEntryPanel.addToUrl(b: TStringBuilder);
begin
  if edit.text <> '' then
  begin
    if b.Length > 0 then
      b.Append('&');
    b.Append(definition.name);
    if (modList <> nil) and (modList.ItemIndex > 0) then
      b.Append(':'+modList.Text);
    b.Append('=');
    if (prefix <> nil) and (prefix.itemIndex > 0) then
      b.append(copy(prefix.text, 1, 2));
    b.Append(EncodeMIME(edit.Text));
    if (edit2 <> nil) and (edit2.Text <> '') then
      b.Append('||'+EncodeMIME(edit2.Text));
  end;
end;

procedure TSearchEntryPanel.AdjustPosition(top, left, width: integer);
var
  rw, w : integer;
begin
  panel.Top := top;
  panel.Left := left;
  panel.Width := width;
  if modList <> nil then
  begin
    modList.Left := panel.Width - 55;
    rw := modList.Left;
  end
  else
    rw := panel.Width;
  case definition.type_ of
    SearchParamTypeString, SearchParamTypeToken, SearchParamTypeComposite, SearchParamTypeUri, SearchParamTypeReference: edit.Width := rw-5-edit.Left;
    SearchParamTypeNumber, SearchParamTypeDate: edit.Width := rw-5-edit.Left;
    SearchParamTypeQuantity:
      begin
      w := (rw - edit.Left - 5) div 2;
      edit.Width := w;
      edit2.left := edit.Left + edit.Width + 5;
      edit2.Width := rw - edit2.left;
      end;
  end;
end;

end.
