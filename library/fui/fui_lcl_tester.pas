unit fui_lcl_tester;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, ActnList, Menus, IniFiles,
  fpcunit, testregistry, testdecorator,
  fsl_base, fsl_utilities;

type
  TTestDetails = class (TFslObject)
  private
    path : String;
    node : TTreeNode;
    test : TTest;
  end;

  { TTesterForm }
  TTesterForm = class(TForm)
    actionTestSelectAll: TAction;
    actionTestUnselectAll: TAction;
    actionTestReset: TAction;
    actionTestCopy: TAction;
    actionTestStop: TAction;
    actionTestRunFailed: TAction;
    actTestRunAll1: TAction;
    actTestRunSelected: TAction;
    ActionList1: TActionList;
    ilNodeStates: TImageList;
    ilMain: TImageList;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    pmTests: TPopupMenu;
    ProgressBar1: TProgressBar;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tvTests: TTreeView;
    procedure actionTestSelectAllExecute(Sender: TObject);
    procedure actionTestUnselectAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FIni : TIniFile;
    FTestDetails : TFslList<TTestDetails>;
    Function registerTestNode(list : TTreeNodes; parent : TTreeNode; test : TTest; name, path : String) : TTreeNode;
    procedure LoadTree;
    procedure BuildTree(rootNode: TTreeNode; aSuite: TTestSuite; path : String);
    procedure setTestState(node: TTreeNode; state : integer);
    procedure checkStateOfChildren(node: TTreeNode);
  public

  end;

var
  TesterForm: TTesterForm;

implementation

{$R *.lfm}

{ TTesterForm }

procedure TTesterForm.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.create(path([getAppConfigDir(false), 'fhir-tests-settings.ini']));
  FTestDetails := TFslList<TTestDetails>.create;
end;

procedure TTesterForm.FormShow(Sender: TObject);
begin
  LoadTree;
end;

procedure TTesterForm.FormDestroy(Sender: TObject);
begin
  FIni.Free;
  FTestDetails.Free;
end;

Function TTesterForm.registerTestNode(list : TTreeNodes; parent : TTreeNode; test : TTest; name, path : String) : TTreeNode;
var
  details : TTestDetails;
begin
  if parent <> nil then
    result := list.AddChildObject(parent, name, test)
  else
    result := list.AddObject(nil, name, test);
  details := TTestDetails.create;
  FTestDetails.add(details);
  details.path := path;
  details.node := result;
  details.test := test;
  result.Data := details;
  result.StateIndex := FIni.ReadInteger('Tests', path, 0);
end;

procedure TTesterForm.LoadTree;
var
  test : TTestSuite;
  node: TTreeNode;
begin
  tvTests.Items.Clear;
  test := GetTestRegistry;
  node := registerTestNode(tvTests.Items, nil, test, 'FHIR Tests', 'tests');
  BuildTree(node, test, 'tests');
end;

procedure TTesterForm.BuildTree(rootNode: TTreeNode; aSuite: TTestSuite; path : String);
var
  node: TTreeNode;
  n : String;
  i: integer;
begin
  for i := 0 to ASuite.ChildTestCount - 1 do
  begin
    if ASuite.Test[i].TestName = '' then
    begin
      node := rootNode;
      n := path;
    end
    else
    begin
      n := path+'.'+ASuite.Test[i].ClassName+inttostr(i);
      node := registerTestNode(tvTests.Items, rootNode, ASuite.Test[i], ASuite.Test[i].TestName, n);
    end;

    if ASuite.Test[i] is TTestSuite then
      BuildTree(Node, TTestSuite(ASuite.Test[i]), n)
    else if TObject(ASuite.Test[i]).InheritsFrom(TTestDecorator) then
      BuildTree(Node, TTestSuite(TTestDecorator(ASuite.Test[i]).Test), n);
    //node.ImageIndex := imgGrayBall;
    //node.SelectedIndex := imgGrayBall;
    //node.StateIndex := ord(tsChecked);
  end;
  //ResetNodeColors;
  //
  //
end;

procedure TTesterForm.setTestState(node: TTreeNode; state: integer);
var
  i : integer;
  details : TTestDetails;
  child : TTreeNode;
begin
  node.StateIndex := state;
  details := TTestDetails(node.data);
  FIni.WriteInteger('Tests', details.path, state);
  child := node.GetFirstChild;
  while (child <> nil) do
  begin
    setTestState(child, state);
    child := child.GetNext;
  end;
end;

procedure TTesterForm.checkStateOfChildren(node: TTreeNode);
var
  i : integer;
  details : TTestDetails;
  child : TTreeNode;
begin
  child := node.GetFirstChild; // will never be nil
  i := child.StateIndex;
  while (child <> nil) do
  begin
    if i <> child.StateIndex then
      i := 2;
    child := child.GetNext;
  end;
  node.StateIndex := i;
  details := TTestDetails(node.data);
  FIni.WriteInteger('Tests', details.path, i);
  if (node.Parent <> nil) then
    checkStateOfChildren(node.Parent);
end;

procedure TTesterForm.actionTestSelectAllExecute(Sender: TObject);
var
  node : TTreeNode;
begin
  node := tvTests.Selected;
  if (node <> nil) then
  begin
    tvTests.BeginUpdate;
    setTestState(node, 1);
    checkStateOfChildren(node.Parent);
    tvTests.EndUpdate;
  end;
end;

procedure TTesterForm.actionTestUnselectAllExecute(Sender: TObject);
var
  node : TTreeNode;
begin
  node := tvTests.Selected;
  if (node <> nil) then
  begin
    tvTests.BeginUpdate;
    setTestState(node, 0);
    checkStateOfChildren(node.Parent);
    tvTests.EndUpdate;
  end;
end;

end.

