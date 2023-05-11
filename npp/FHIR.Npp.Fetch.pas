unit FHIR.Npp.Fetch;


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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Generics.Collections, Math, clipbrd,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin,
  VirtualTrees,
  FHIR.Npp.Form,
  fsl_base, fsl_utilities, fsl_http,
  fhir_objects, fhir_common, fhir_client, fhir_pathengine, fhir_utilities,
  fhir_oauth,
  FHIR.Npp.Context, FHIR.Npp.Settings;

const
  MIN_COL_WIDTH = 260;
  SEARCH_PANEL_HEIGHT = 26;

type
  TFhirSearchModifier = (smNull, smMissing, smExact, smContains, smNot, smText, smIn, smNotIn, smBelow, smAbove, smType);

const
  ALL_MODIFIERS = [smNull..smType];
  CODES_MODIFIERS : array [TFhirSearchModifier] of String = ('', 'missing', 'exact', 'contains', 'not', 'text', 'in', 'not-in', 'below', 'above', 'type');

type
  TFetchResourceFrm = class;

  TSearchEntryPanel = class (TFslObject)
  private
    form : TFetchResourceFrm;
    definition : TFHIRSearchParamDefinitionW;
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
    constructor Create(form : TFetchResourceFrm; definition : TFHIRSearchParamDefinitionW);
    destructor Destroy; override;

    procedure AdjustPosition(top, left, width : integer);
    procedure addToUrl(b : TStringBuilder);
    procedure readFromURL(pm : THTTPParameters);
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
    Label4: TLabel;
    cbxServers: TComboBox;
    btnSerevrs: TButton;
    RadioButton1: TRadioButton;
    edtFilter: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnSearchAllClick(Sender: TObject);
    procedure btnSearchPagedClick(Sender: TObject);
    procedure vtMatchesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtMatchesAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMatchesRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMatchesDblClick(Sender: TObject);
    procedure btnSerevrsClick(Sender: TObject);
    procedure cbxServersChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    FContext : TFHIRNppContext;
    FServers : TFslList<TRegisteredFHIRServer>;
    FSelection : integer;

    FActivated : boolean;
    FConnection : TFHIRNppServerConnection;
    FType : String;
    FSearchItems : TFslList<TSearchEntryPanel>;
    FColumns : TFslStringDictionary;
    FLinks : TFslStringDictionary;
    FMatches : TFslList<TFHIRResourceV>;
    FQuery : TFHIRPathEngineV;

    procedure SetContext(value : TFHIRNppContext);
    procedure loadServers;

    procedure addSearchItem(sp : TFHIRSearchParamDefinitionW);
    procedure layoutSearchParameters;
    procedure loadSortCombo;
    function asURL : String;
    procedure readURL(pm : THTTPParameters);

    Procedure SetUpColumns;
    procedure AddColumn(name, path : String);
    function ignoreColumn(st : TFhirStructureDefinitionW; ed : TFhirElementDefinitionW) : boolean;
    procedure clearColumns;

    procedure dosearch(url : String; all : boolean);
    procedure clearSearch;
    function GetCell(res : TFhirResourceV; path : String ) : String;

(*
{    FType : TFhirResourceType;}
    FSelectedId: String;
  //  FSelectedType: TFhirResourceType;

    // choosing type
    procedure loadTypeCombo;
    procedure changeSelectedType;

    // search parameters
    procedure AddSearchAll;
//    procedure AddSearch(rr : TFhirCapabilityStatementRestResource);
//    function createSearchParam(name, definition : String; type_ : TFhirSearchParamTypeEnum) : TFhirCapabilityStatementRestResourceSearchParam;
//    procedure SetProfiles(const Value: TProfileManager);
    procedure fetchUrl(s : String);

    // columns

    // search
*)
  public
    property Context : TFHIRNppContext read FContext write SetContext;
  end;

var
  FetchResourceFrm: TFetchResourceFrm;

implementation

{$R *.dfm}

uses
  FHIR.Npp.Plugin, Types;

{ TSearchEntryPanel }

constructor TSearchEntryPanel.create(form: TFetchResourceFrm; definition : TFHIRSearchParamDefinitionW);
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

procedure TSearchEntryPanel.readFromURL(pm: THTTPParameters);
var
  ok : boolean;
  s : String;
begin
  if pm.has(definition.name) then
    readParamValue(pm[definition.name])
  else
  begin
    ok := false;
    if (modList <> nil) then
      for s in modList.Items do
        if pm.has(definition.name+':'+s) then
        begin
          ok := true;
          modList.ItemIndex := modlist.Items.IndexOf(s);
          readParamValue(pm[definition.name+':'+s]);
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
    sptString, sptToken, sptComposite, sptUri, sptReference:
      edit.text := s;
    sptNumber, sptDate, sptQuantity:
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
        if (definition.type_ = sptQuantity) then
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
  modc : TFhirSearchModifier;
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
    sptString: buildEdit('Text to search by');
    sptToken: buildEdit('Code or text to search by');
    sptComposite: buildEdit('Composite value to search by');
    sptUri: buildEdit('URI to search by');
    sptReference: buildEdit('ID to search by');
    sptNumber: buildNumberEdit('Number to search by', false);
    sptDate: buildNumberEdit('Date to search by (variable precision, in the format yyyy-mm-ddThh:mm:ss[TS], where TX is Z or +/-hh:mm)', true);
    sptQuantity: buildQuantityEdit;
  end;
  if (hasMod) then
  begin
    modList := TComboBox.Create(form);
    modList.parent := panel;
    modList.Left := panel.Width - 75;
    modList.Top := edit.Top;
    modlist.Width := 70;
    modlist.Style := csDropDownList;
    modlist.Items.Add('');
    for modc in ALL_MODIFIERS do
// todo - use search parameter definition      if modc in definition.modifier then
        modlist.Items.Add(':'+CODES_MODIFIERS[modc]);
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
  prefix.Width := 60;
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
    sptString, sptToken, sptComposite, sptUri, sptReference: edit.Width := rw-5-edit.Left;
    sptNumber, sptDate: edit.Width := rw-5-edit.Left;
    sptQuantity:
      begin
      w := (rw - edit.Left - 5) div 2;
      edit.Width := w;
      edit2.left := edit.Left + edit.Width + 5;
      edit2.Width := rw - edit2.left;
      end;
  end;
end;

{ TFetchResourceFrm }

procedure TFetchResourceFrm.FormActivate(Sender: TObject);
begin
  if not (FActivated) then
    loadServers;
  FActivated := true;
end;

procedure TFetchResourceFrm.FormCreate(Sender: TObject);
begin
  FServers := TFslList<TRegisteredFHIRServer>.create;
  FSearchItems := TFslList<TSearchEntryPanel>.create;
  FColumns := TFslStringDictionary.create;
  FMatches := TFslList<TFHIRResourceV>.create;
  FLinks := TFslStringDictionary.create;
end;

procedure TFetchResourceFrm.FormDestroy(Sender: TObject);
begin
  FServers.Free;
  FContext.Free;
  FSearchItems.Free;
  FColumns.Free;
  FMatches.Free;
  FLinks.Free;
end;

procedure TFetchResourceFrm.FormResize(Sender: TObject);
begin
  layoutSearchParameters;
end;

procedure TFetchResourceFrm.SetContext(value: TFHIRNppContext);
begin
  FContext.Free;
  FContext := value;
end;

procedure TFetchResourceFrm.btnSerevrsClick(Sender: TObject);
begin
  FNpp.FuncSettings(true);
  loadServers;
end;

procedure TFetchResourceFrm.loadServers;
var
  rs : TRegisteredFHIRServer;
begin
  FServers.Clear;
  cbxServers.Items.Clear;
  Settings.ListServers('', FServers);
  for rs in FServers do
    cbxServers.Items.addObject(rs.name + ': '+rs.fhirEndpoint, rs);
  cbxServers.ItemIndex := 0;
  cbxServersChange(nil);
end;

procedure TFetchResourceFrm.cbxServersChange(Sender: TObject);
var
  rs : TRegisteredFHIRServer;
  vf : TFHIRNppVersionFactory;
begin
  if cbxServers.ItemIndex = -1 then
  begin
    cbxType.Items.Clear
  end
  else
  begin
    rs := cbxServers.Items.objects[cbxServers.ItemIndex] as TRegisteredFHIRServer;
    vf := FContext.Version[rs.version];
    if not FContext.connections.TryGetValue(rs.fhirEndpoint, FConnection) then
    begin
      FConnection := TFHIRNppServerConnection.Create;
      try
        if rs.format = ffUnspecified then
          FConnection.client := vf.Factory.makeClient(vf.Worker.link, rs.fhirEndpoint, fctWinInet, ffJson, 5000)
        else
          FConnection.client := vf.Factory.makeClient(vf.Worker.link, rs.fhirEndpoint, fctWinInet, rs.format, 5000);
        FConnection.statement := vf.Factory.wrapCapabilityStatement(FConnection.client.conformanceV(false));
        FContext.connections.Add(rs.fhirEndpoint, FConnection.Link);
      finally
        FConnection.free;
      end;
    end;
    FQuery.Free;
    FQuery := vf.Factory.makePathEngine(vf.Worker.link, nil);
  end;
  cbxType.Items.Clear;
  if FConnection <> nil then
    FConnection.statement.listTypes([fiSearch], cbxType.Items);
  cbxType.ItemIndex := 0;
  cbxTypeChange(nil);
end;

procedure TFetchResourceFrm.cbxTypeChange(Sender: TObject);
var
  obj : TFHIRObject;
  searchParams : TFslList<TFHIRSearchParamDefinitionW>;
  sp : TFHIRSearchParamDefinitionW;
begin
  FSearchItems.Clear;
  clearColumns;
//  clearSearch;
  FType := '';
  btnCopy.Enabled := false;
  btnPaste.Enabled := false;
  if cbxType.ItemIndex > -1 then
  begin
    FType := cbxType.Items[cbxType.Itemindex];
    searchParams := TFslList<TFHIRSearchParamDefinitionW>.create;
    try
      FConnection.statement.listSearchParams(FType, searchParams);
      for sp in searchParams do
        if not StringArrayExistsSensitive(['_summary', '_count', '_sort'], sp.name) then

        addSearchitem(sp);
      loadSortCombo;
      layoutSearchParameters;
      setupColumns;
    finally
      searchParams.Free;
    end;
    btnCopy.Enabled := true;
    btnPaste.Enabled := true;
  end;
end;

procedure TFetchResourceFrm.clearColumns;
begin
  FColumns.Clear;
  vtMatches.Header.Columns.Clear;
end;

function tail(s : String) : String;
begin
  result := s.Substring(s.LastIndexOf('.')+1);
end;

procedure TFetchResourceFrm.setUpColumns;
var
  rs : TRegisteredFHIRServer;
  vf : TFHIRNppVersionFactory;
  r : TFHIRResourceV;
  sd : TFhirStructureDefinitionW;
  edl : TFslList<TFHIRElementDefinitionW>;
  ed : TFHIRElementDefinitionW;
begin
  ClearColumns;
  AddColumn('id', 'id');
  AddColumn('version', 'meta.versionId');
  AddColumn('date', 'meta.lastUpdated');
  AddColumn('profile', 'meta.profile');
  AddColumn('security', 'meta.security');
  rs := cbxServers.Items.objects[cbxServers.ItemIndex] as TRegisteredFHIRServer;
  vf := FContext.Version[rs.version];
  r := vf.Worker.fetchResource('StructureDefinition', 'http://hl7.org/fhir/StructureDefinition/'+FType);
  if r <> nil then
  begin
    sd := vf.Factory.wrapStructureDefinition(r); // take ownership
    try
      edl := sd.elements;
      try
        for ed in edl do
          if ed.path.Contains('.') and ed.isSummary and not ignoreColumn(sd, ed) then
            AddColumn(tail(ed.path), ed.path);
      finally
        edl.Free;
      end;
    finally
      sd.Free;
    end;
  end
  else
    AddColumn('text', 'text.div')
end;

function TFetchResourceFrm.ignoreColumn(st : TFhirStructureDefinitionW; ed : TFhirElementDefinitionW) : boolean;
begin
  if (ed.path.EndsWith('.meta')) then
    result := true
  else
    result := ed.types = 'BackboneElement';
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


procedure TFetchResourceFrm.loadSortCombo;
var
  se : TSearchEntryPanel;
begin
  cbxSort.Items.Clear;
  cbxSort.Items.Add('');
  for se in FSearchItems do
    cbxSort.Items.Add(se.definition.name);
end;

procedure TFetchResourceFrm.addSearchItem(sp : TFHIRSearchParamDefinitionW);
begin
  FSearchItems.Add(TSearchEntryPanel.create(self, sp.Link));
end;

function enough(v, i: Integer) : integer;
begin
  if v mod i = 0 then
    result := v div i
  else
    result := v div i + 1;
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

procedure TFetchResourceFrm.btnCopyClick(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := '?'+asURL;
  Clipboard.Close;
end;

procedure TFetchResourceFrm.btnOpenClick(Sender: TObject);
var
  rs : TRegisteredFHIRServer;
  vf : TFHIRNppVersionFactory;
  r, f : TFHIRResourceV;
  url : String;
  fmt : TFHIRFormat;
begin
  rs := cbxServers.Items.objects[cbxServers.ItemIndex] as TRegisteredFHIRServer;
  vf := FContext.Version[rs.version];
  r := FMatches[FSelection];
  url := Urlpath([rs.fhirEndpoint, r.fhirType, r.id]);
  if rbXml.Checked then
    fmt := ffXml
  else if rbJson.Checked then
    fmt := ffJson
  else
    fmt := rs.format;
  if fmt = ffUnspecified then
    fmt := ffXml;

  f := FConnection.client.readResourceV(r.fhirType, r.id);
  try
    FNpp.newResource(f, rs.version, fmt, url);
  finally
    f.Free;
  end;
  modalResult := mrClose;
end;

procedure TFetchResourceFrm.btnPasteClick(Sender: TObject);
var
  pm : THTTPParameters;
  s : String;
begin
  Clipboard.Open;
  s := Clipboard.AsText;
  Clipboard.Close;
  if s.Contains('?') then
    s := s.Substring(s.IndexOf('?')+1);
  pm := THTTPParameters.create(s);
  try
    readURL(pm);
  finally
    pm.Free;
  end;
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


procedure TFetchResourceFrm.readURL(pm: THTTPParameters);
var
  se : TSearchEntryPanel;
begin
  for se in FSearchItems do
    se.readFromURL(pm);
  if pm.has('_sort') then
  begin
    cbxSort.itemindex := cbxSort.Items.IndexOf(pm['_sort']);
    cbSearchOrder.Checked := false;
  end else if pm.has('_sort:asc') then
  begin
    cbxSort.itemindex := cbxSort.Items.IndexOf(pm['_sort:asc']);
    cbSearchOrder.Checked := false;
  end else if pm.has('_sort:desc') then
  begin
    cbxSort.itemindex := cbxSort.Items.IndexOf(pm['_sort:desc']);
    cbSearchOrder.Checked := true;
  end;
  if (pm.has('_count')) then
    edtPageCount.Value := StrToIntDef(pm['_count'], 20);
end;

procedure TFetchResourceFrm.btnSearchAllClick(Sender: TObject);
begin
  doSearch(asURL, true);
end;

procedure TFetchResourceFrm.btnSearchPagedClick(Sender: TObject);
begin
  doSearch(asURL, false);
end;

procedure TFetchResourceFrm.dosearch(url: String; all : boolean);
var
  bundle : TFHIRBundleW;
  be : TFhirBundleEntryW;
  bel : TFslList<TFhirBundleEntryW>;
  links : TFslStringDictionary;
  link : String;
begin
  clearSearch;
  bundle := FConnection.client.BundleFactory.Create(FConnection.client.searchV(FType, all, url+'&_summary=true'));
  try
    bel := bundle.entries;
    try
      for be in bel do
        if (be.searchMode = smMatch) and (be.resource <> nil) and ((be.resource.fhirType = FType) or (FType = '')) then
          FMatches.Add(be.resource.Link);
    finally
      bel.free;
    end;
    bundle.listLinks(FLinks);
    btnFirst.Enabled := FLinks.ContainsKey('first');
    btnNext.Enabled := FLinks.ContainsKey('next');
    btnPrev.Enabled := FLinks.ContainsKey('previous');
    btnLast.Enabled := FLinks.ContainsKey('last');
    vtMatches.RootNodeCount := FMatches.Count;
    vtMatches.Invalidate;
  finally
    bundle.Free;
  end;
end;

procedure TFetchResourceFrm.vtMatchesAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelection := node.Index;
  btnOpen.Enabled := true;
end;

procedure TFetchResourceFrm.vtMatchesDblClick(Sender: TObject);
begin
  if btnOpen.Enabled then
    btnOpenClick(nil);
end;

procedure TFetchResourceFrm.vtMatchesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  res : TFhirResourceV;
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


procedure TFetchResourceFrm.clearSearch;
begin

end;

function TFetchResourceFrm.GetCell(res: TFhirResourceV; path: String): String;
var
  sl : TFHIRSelectionList;
begin
  sl := FQuery.evaluate(nil, res, path);
  try
    result := FQuery.convertToString(sl);
  finally
    sl.free;
  end;
end;


(*
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
    FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_id', 'Resource id', sptToken)));
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
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_id', 'Resource id', sptToken)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_text', 'Search by text content (narrative)', sptString)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_content', 'Search by text content (all elements)', sptString)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_lastUpdated', 'Search by date of last update', sptDate)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_profile', 'Search by Profile', sptToken)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_security', 'Search by security label', sptToken)));
  FSearchItems.Add(TSearchEntryPanel.create(self, createSearchParam('_tag', 'Search by workflow tag', sptToken)));
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
end;

function TFetchResourceFrm.createSearchParam(name, definition: String; type_: TFhirsptEnum): TFhirCapabilityStatementRestResourceSearchParam;
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

procedure TFetchResourceFrm.SetClient(const Value: TFhirClient);
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


(*
  begin
    res := FClient.readResource(FetchResourceFrm.SelectedType, FetchResourceFrm.SelectedId);
    try
      if FetchResourceFrm.rbJson.Checked then
        comp := FContext.Version[FCurrentFileInfo.workingVersion].makeComposer(ffJson)
      else
        comp := FContext.Version[FCurrentFileInfo.workingVersion].makeComposer(ffXml);
      try
        s := TStringStream.Create;
        try
          comp.Compose(s, res);
          NewFile(s.DataString);
          if FetchResourceFrm.rbJson.Checked then
            saveFileAs(IncludeTrailingPathDelimiter(SystemTemp)+CODES_TFhirResourceType[res.ResourceType]+'-'+res.id+'.json')
          else
            saveFileAs(IncludeTrailingPathDelimiter(SystemTemp)+CODES_TFhirResourceType[res.ResourceType]+'-'+res.id+'.xml');
        finally
          s.Free;
        end;
      finally
        comp.Free;
      end;
    finally
      res.Free;
    end;
  end;
  *)
end.

