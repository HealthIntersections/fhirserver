unit ServerChooser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.ExtCtrls,
  FHIRTypes, FHIRComponents, FHIRResources, FHIRUtilities,
  ValueSetEditorCore, ValueSetEditorRegisterServerForm;

type
  TTreeDataPointer = record
    obj : TObject;
  end;
  PTreeDataPointer = ^TTreeDataPointer;

  TServerChooserForm = class(TForm)
    tvValuesets: TVirtualStringTree;
    Panel1: TPanel;
    Button1: TButton;
    btnOpen: TButton;
    lblId: TLabel;
    procedure tvValuesetsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvValuesetsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure tvValuesetsColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure tvValuesetsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure tvValuesetsDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvValuesetsClick(Sender: TObject);
  private
    FContext: TValueSetEditorContext;
    FCurrentServer : String;
    procedure SetContext(const Value: TValueSetEditorContext);
    { Private declarations }
  public
    { Public declarations }
    property Context : TValueSetEditorContext read FContext write SetContext;
  end;

var
  ServerChooserForm: TServerChooserForm;

implementation

{$R *.dfm}

procedure TServerChooserForm.FormDestroy(Sender: TObject);
begin
  Context := nil;
end;

procedure TServerChooserForm.FormShow(Sender: TObject);
begin
  tvValuesets.Header.Columns[0].Width := Context.Settings.columnWidth('choice', 'name', 150);
  tvValuesets.Header.Columns[1].Width := Context.Settings.columnWidth('choice', 'status', 50);
  tvValuesets.Header.Columns[2].Width := Context.Settings.columnWidth('choice', 'identifier', 200);
  tvValuesets.Header.Columns[3].Width := Context.Settings.columnWidth('choice', 'publisher', 150);
  tvValuesets.Header.Columns[4].Width := Context.Settings.columnWidth('choice', 'description', 200);
end;

procedure TServerChooserForm.SetContext(const Value: TValueSetEditorContext);
begin
  FContext.Free;
  FContext := Value;
  if Context <> nil then
  begin
    Caption := 'Value Sets on '+Context.Server.URL;
    tvValuesets.RootNodeCount := Context.Server.List.entryList.Count;
    tvValuesets.Invalidate;
  end;
end;

procedure TServerChooserForm.tvValuesetsClick(Sender: TObject);
var
  ae : TFHIRBundleEntry;
begin
  if tvValuesets.FocusedNode <> nil then
  begin
    ae := TFHIRBundleEntry(PTreeDataPointer(tvValuesets.GetNodeData(tvValuesets.FocusedNode)).obj);
    lblId.caption := ae.id;
  end
  else
    lblId.caption := '';
  btnOpen.enabled := tvValuesets.FocusedNode <> nil;
end;

procedure TServerChooserForm.tvValuesetsColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  case column of
    0 : Context.Settings.setColumnWidth('choice', 'name', tvValuesets.Header.Columns[0].width);
    1 : Context.Settings.setColumnWidth('choice', 'status', tvValuesets.Header.Columns[1].width);
    2 : Context.Settings.setColumnWidth('choice', 'identifier', tvValuesets.Header.Columns[2].width);
    3 : Context.Settings.setColumnWidth('choice', 'publisher', tvValuesets.Header.Columns[3].width);
    4 : Context.Settings.setColumnWidth('choice', 'description', tvValuesets.Header.Columns[4].width);
  end;
end;

procedure TServerChooserForm.tvValuesetsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p : PTreeDataPointer;
begin
  Node.ChildCount := 0; // no children here
  p := tvValuesets.GetNodeData(node);
  p.obj := Context.Server.List.entryList[Node.Index];
end;

procedure TServerChooserForm.tvValuesetsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
  vs : TFhirValueSet;
begin
  p := tvValuesets.GetNodeData(node);
  vs := TFhirValueSet(TFHIRBundleEntry(p.obj).resource);
  case Column of
    0: CellText := vs.name;
    1: CellText := CODES_TFhirConformanceResourceStatus[vs.status];
    2: CellText := vs.url;
    3: CellText := vs.publisher;
    4: CellText := vs.description;
  end;
end;

procedure TServerChooserForm.tvValuesetsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  vs1, vs2 : TFhirValueSet;
begin
  vs1 := TFhirValueSet(TFHIRBundleEntry(PTreeDataPointer(tvValuesets.GetNodeData(node1)).obj).resource);
  vs2 := TFhirValueSet(TFHIRBundleEntry(PTreeDataPointer(tvValuesets.GetNodeData(node2)).obj).resource);
  case column of
    0 : result := CompareText(vs1.name, vs2.name);
    1 : result := CompareText(CODES_TFhirConformanceResourceStatus[vs1.status], CODES_TFhirConformanceResourceStatus[vs2.status]);
    2 : result := CompareText(vs1.url, vs2.url);
    3 : result := CompareText(vs1.publisher, vs2.publisher);
    4 : result := CompareText(vs1.description, vs2.description);
  end;
end;


procedure TServerChooserForm.tvValuesetsDblClick(Sender: TObject);
var
  ae : TFHIRBundleEntry;
begin
  ae := TFHIRBundleEntry(PTreeDataPointer(tvValuesets.GetNodeData(tvValuesets.FocusedNode)).obj);
  Context.openFromServer(nil, ae.id);
  ModalResult := mrOk;
end;

end.
