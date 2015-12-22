unit ConceptLookupFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ValueSetEditorCore, VirtualTrees,
  Vcl.StdCtrls, Vcl.ExtCtrls, FHIRResources, FHIRTypes, FHIRUtilities, ServerChooser;

type
  TConceptLookupForm = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    cbxValueSets: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    edtFilter: TEdit;
    Label4: TLabel;
    tvExpansion: TVirtualStringTree;
    lblServerError: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure cbxValueSetsChange(Sender: TObject);
    procedure tvExpansionInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvExpansionGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure tvExpansionInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure tvExpansionCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure tvExpansionColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure tvExpansionAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvExpansionDblClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    FContext : TValueSetEditorContext;
    FExpansion : TFhirValueSet;
    FSelected: TFHIRCoding;
    procedure SetContext(const Value: TValueSetEditorContext);
    procedure LoadMatches;
    procedure SetExpansion(const Value: TFHIRValueSet);
    procedure SetSelected(const Value: TFHIRCoding);
  public
    { Public declarations }
    property Context : TValueSetEditorContext read FContext write SetContext;
    property Expansion : TFHIRValueSet read FExpansion write SetExpansion;
    property Selected : TFHIRCoding read FSelected write SetSelected;
  end;

var
  ConceptLookupForm: TConceptLookupForm;

implementation

{$R *.dfm}

{ TForm3 }

procedure TConceptLookupForm.btnOkClick(Sender: TObject);
var
  node : PVirtualNode;
  p : PTreeDataPointer;
  c : TFhirValueSetExpansionContains;
  cc : TFHIRCoding;
begin
  node := tvExpansion.GetFirstSelected(false);
  if node <> nil then
  begin
    p := tvExpansion.GetNodeData(Node);
    c := TFhirValueSetExpansionContains(p.obj);
    if (c <> nil) and not c.abstract then
    begin
      cc := TFhirCoding.Create;
      try
        cc.system := c.system;
        cc.code := c.code;
        cc.display := c.display;
        Selected := cc.Link;
        ModalResult := mrOK;
      finally
        cc.free;
      end;
    end;
  end;
end;

procedure TConceptLookupForm.cbxValueSetsChange(Sender: TObject);
begin
  LoadMatches;
end;

procedure TConceptLookupForm.edtFilterChange(Sender: TObject);
begin
  loadMatches;
end;

procedure TConceptLookupForm.FormDestroy(Sender: TObject);
begin
  FExpansion.Free;
  FContext.Free;
  FSelected.Free;
end;

procedure TConceptLookupForm.FormShow(Sender: TObject);
var
  vs : TFHIRValueSet;
begin
  cbxValueSets.Clear;
  cbxValueSets.Items.Add('(any)');
  for vs in FContext.WorkingServer.List do
    cbxValueSets.Items.Add(vs.name+' ('+vs.url+')');
  cbxValueSets.ItemIndex := 0;
end;

procedure TConceptLookupForm.LoadMatches;
var
  url : String;
  vs : TFHIRValueset;
begin
  if (edtFilter.Text <> '') or (cbxValueSets.ItemIndex > 0) then
  begin
    if cbxValueSets.ItemIndex = 0 then
      url := ANY_CODE_VS
    else
      url := FContext.WorkingServer.List[cbxValueSets.ItemIndex-1].url;
    SetExpansion(nil);
    try
      SetExpansion(Context.WorkingServer.expand(url, edtFilter.Text, 100, true));
    except
      on e : Exception do
        lblServerError.Caption := 'Server Error: '+e.Message;
    end;
  end;
end;

procedure TConceptLookupForm.SetContext(const Value: TValueSetEditorContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TConceptLookupForm.SetExpansion(const Value: TFHIRValueSet);
begin
  FExpansion.Free;
  FExpansion := Value;
  btnOk.Enabled := false;
  if FExpansion = nil then
    tvExpansion.RootNodeCount := 0
  else
    tvExpansion.RootNodeCount := FExpansion.expansion.containsList.Count;
end;

procedure TConceptLookupForm.SetSelected(const Value: TFHIRCoding);
begin
  FSelected.Free;
  FSelected := Value;
end;

procedure TConceptLookupForm.tvExpansionAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnOk.Enabled := true;
end;

procedure TConceptLookupForm.tvExpansionColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  case column of
    0 : Context.Settings.setColumnWidth('lookup', 'system', tvExpansion.Header.Columns[0].width);
    1 : Context.Settings.setColumnWidth('lookup', 'code', tvExpansion.Header.Columns[1].width);
    2 : Context.Settings.setColumnWidth('lookup', 'display', tvExpansion.Header.Columns[2].width);
  end;
end;

procedure TConceptLookupForm.tvExpansionCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  c1, c2 : TFhirValueSetExpansionContains;
begin
  c1 := TFhirValueSetExpansionContains(PTreeDataPointer(tvExpansion.GetNodeData(node1)).obj);
  c2 := TFhirValueSetExpansionContains(PTreeDataPointer(tvExpansion.GetNodeData(node2)).obj);
  case column of
    0 : result := CompareText(c1.system, c2.system);
    1 : result := CompareText(c1.code, c2.code);
    2 : result := CompareText(c1.display, c2.display);
  end;
end;

procedure TConceptLookupForm.tvExpansionDblClick(Sender: TObject);
begin
  btnOKClick(nil);
end;

procedure TConceptLookupForm.tvExpansionGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
  c : TFhirValueSetExpansionContains;
begin
  p := tvExpansion.GetNodeData(Node);
  c := TFhirValueSetExpansionContains(p.obj);
  if (c <> nil) then
    case Column of
      0: CellText := c.system;
      1: CellText := c.code;
      2: CellText := c.display;
    end
  else if Column = 0 then
    CellText := '(no entries)'
  else
    CellText := '';
end;

procedure TConceptLookupForm.tvExpansionInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  p : PTreeDataPointer;
  c : TFhirValueSetExpansionContains;
begin
  p := tvExpansion.GetNodeData(node);
  c := TFhirValueSetExpansionContains(p.obj);
  if (c <> nil) then
    ChildCount := c.containsList.Count;
end;

procedure TConceptLookupForm.tvExpansionInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  list : TFhirValueSetExpansionContainsList;
  pp, p : PTreeDataPointer;
begin
  if parentNode = nil then
    list := FExpansion.Expansion.containsList
  else
  begin
    pp := tvExpansion.GetNodeData(parentNode);
    list := TFhirValueSetExpansionContains(pp.obj).containsList;
  end;
  p := tvExpansion.GetNodeData(Node);
  if Node.Index < list.Count then
  begin
    p.obj := list[Node.Index];
    if list[node.Index].containsList.Count > 0 then
       InitialStates := [ivsHasChildren];
  end;
end;

end.
