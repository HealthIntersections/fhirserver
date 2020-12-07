unit fui_lcl_tester;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, ActnList, Menus, IniFiles, LCLIntf,
  fpcunit, testregistry, testdecorator,
  fsl_base, fsl_utilities;

type
  TTestCheckState = (tcsUnchecked, tcsChecked, tcsMixed);
  TTestOutcome = (toNotRun, toRunning, toChildRunning, toPass, toFail, toError);

  TTestDetails = class (TFslObject)
  private
    path : String;
    node : TTreeNode;
    test : TTest;
    checked : TTestCheckState;
    outcome : TTestOutcome;

    doInRun : boolean;
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
    procedure actionTestCopyExecute(Sender: TObject);
    procedure actionTestResetExecute(Sender: TObject);
    procedure actionTestRunFailedExecute(Sender: TObject);
    procedure actionTestSelectAllExecute(Sender: TObject);
    procedure actionTestStopExecute(Sender: TObject);
    procedure actionTestUnselectAllExecute(Sender: TObject);
    procedure actTestRunAll1Execute(Sender: TObject);
    procedure actTestRunSelectedExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvTestsClick(Sender: TObject);
  private
    FIni : TIniFile;
    FTestDetails : TFslList<TTestDetails>;
    FTestsToRun : cardinal;
    FWantStop : boolean;
    function getTestDetails(test : TTest) : TTestDetails;
    Function registerTestNode(list : TTreeNodes; parent : TTreeNode; test : TTest; name, path : String) : TTreeNode;
    procedure LoadTree;
    procedure BuildTree(rootNode: TTreeNode; aSuite: TTestSuite; path : String);
    procedure setDoExecute(node: TTreeNode);
    procedure setDoExecuteParent(node: TTreeNode);
    procedure setTestState(node: TTreeNode; state : integer);
    procedure checkStateOfChildren(node: TTreeNode);
    procedure loadState;
    procedure saveState;
    procedure ExecuteTests;
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
  LoadState;
end;

procedure TTesterForm.tvTestsClick(Sender: TObject);
var
  p : TPoint;
  node : TTreeNode;
  ht : THitTests;
begin
  GetCursorPos(p);
  p := tvTests.ScreenToClient(p);
  node := tvTests.GetNodeAt(p.x, p.y);
  if (node <> nil) then
  begin
    ht := tvTests.GetHitTestInfoAt(p.x, p.y);
    if (htOnStateIcon in ht) then
    begin
      if node.StateIndex = 1 then
        setTestState(node, 0)
      else
        setTestState(node, 1)
    end;
  end;
end;

function TTesterForm.getTestDetails(test: TTest): TTestDetails;
var
  td : TTestDetails;
begin
  result := nil;
  for td in FTestDetails do
    if td.test = test then
      exit(td);
end;

procedure TTesterForm.FormDestroy(Sender: TObject);
begin
  FIni.Free;
  FTestDetails.Free;
end;

function TTesterForm.registerTestNode(list: TTreeNodes; parent: TTreeNode;
  test: TTest; name, path: String): TTreeNode;
var
  td : TTestDetails;
begin
  if parent <> nil then
    result := list.AddChildObject(parent, name, test)
  else
    result := list.AddObject(nil, name, test);
  td := TTestDetails.create;
  FTestDetails.add(td);
  td.path := path;
  td.node := result;
  td.test := test;
  result.Data := td;
  td.checked := tcsUnchecked;
  result.StateIndex := 2;
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
  end;
end;

procedure TTesterForm.setTestState(node: TTreeNode; state: integer);
var
  i : integer;
  td : TTestDetails;
  child : TTreeNode;
begin
  node.StateIndex := state;
  td := TTestDetails(node.data);
  if (state = 1) then
    td.Checked := tcsChecked
  else
    td.Checked := tcsUnchecked;
  child := node.GetFirstChild;
  while (child <> nil) do
  begin
    setTestState(child, state);
    child := child.GetNextSibling;
  end;
end;

procedure TTesterForm.checkStateOfChildren(node: TTreeNode);
var
  i : integer;
  td : TTestDetails;
  child : TTreeNode;
begin
  if node <> nil then
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
    td := TTestDetails(node.data);
    td.checked := tcsMixed;
    if (node.Parent <> nil) then
      checkStateOfChildren(node.Parent);
  end;
end;

procedure TTesterForm.loadState;
var
  ss, so : String;
  i : integer;
  td : TTestDetails;
begin
  ss := FIni.ReadString('Tests', 'States', '');
  so := FIni.ReadString('Tests', 'Outcomes', '');
  for i := 0 to FTestDetails.count - 1 do
  begin
    td := FTestDetails[i];
    if i < length(ss) then
      td.checked := TTestCheckState(StrToInt(ss[i+1]))
    else
      td.checked := tcsUnchecked;
    td.node.StateIndex := ord(td.checked);

    if i < length(so) then
      td.outcome := TTestOutcome(StrToInt(so[i+1]))
    else
      td.outcome := toNotRun;
    td.node.ImageIndex := ord(td.checked);
  end;
end;

procedure TTesterForm.saveState;
var
  bs, bo : TStringBuilder;
  td : TTestDetails;
begin
  bs := TStringBuilder.create;
  bo := TStringBuilder.create;
  try
    for td in FTestDetails do
    begin
      bs.append(inttostr(ord(td.checked)));
      bo.append(inttostr(ord(td.outcome)));
    end;
    FIni.WriteString('Tests', 'States', bs.toString);
    FIni.WriteString('Tests', 'Outcomes', bo.toString);
  finally
    bo.Free;
    bs.Free;
  end;
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
    saveState;
  end;
end;

procedure TTesterForm.actionTestStopExecute(Sender: TObject);
begin
  FWantStop := true;
end;

procedure TTesterForm.actionTestResetExecute(Sender: TObject);
var
  td : TTestDetails;
begin
  tvTests.BeginUpdate;
  for td in FTestDetails do
  begin
    if td.outcome <> toNotRun then
    begin
      td.outcome := toNotRun;
      td.node.ImageIndex := 0;
    end;
  end;
  tvTests.EndUpdate;
  saveState;
end;

procedure TTesterForm.actionTestCopyExecute(Sender: TObject);
begin

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
    saveState;
  end;
end;

procedure TTesterForm.actTestRunAll1Execute(Sender: TObject);
var
  td : TTestDetails;
begin
  for td in FTestDetails do
    td.doInRun := true;
  FTestsToRun := FTestDetails.Count;
  ExecuteTests;
end;

procedure TTesterForm.actTestRunSelectedExecute(Sender: TObject);
var
  td : TTestDetails;
begin
  for td in FTestDetails do
    td.doInRun := false;
  if tvTests.Selected <> nil then
  begin
    setDoExecute(tvTests.Selected);
    setDoExecuteParent(tvTests.Selected.Parent);

    FTestsToRun := 0;
    for td in FTestDetails do
      if td.doInRun then
        inc(FTestsToRun);
    ExecuteTests;
  end;
end;

procedure TTesterForm.actionTestRunFailedExecute(Sender: TObject);
var
  td : TTestDetails;
begin
  for td in FTestDetails do
  begin
    td.doInRun := td.outcome in (toFail, toError);
    if td.doInRun then
      setDoExecuteParent(td.node.parent);
  end;

  FTestsToRun := 0;
  for td in FTestDetails do
    if td.doInRun then
      inc(FTestsToRun);

  if FTestsToRun > 0 then
    ExecuteTests
  else
    ShowMessage('No Failed Tests');
end;


procedure TTesterForm.setDoExecute(node : TTreeNode);
var
  child : TTreeNode;
begin
  TTestDetails(node.data).doInRun := true;

  child := node.GetFirstChild;
  while (child <> nil) do
  begin
    setDoExecute(child);
    child := child.GetNextSibling;
  end;
end;

procedure TTesterForm.setDoExecuteParent(node: TTreeNode);
begin
  if node <> nil then
  begin
    TTestDetails(node.data).doInRun := true;
    setDoExecuteParent(node.parent);
  end;
end;

procedure TTesterForm.ExecuteTests;
begin
  FWantStop := false;
  ProgressBar1.Max := FTestsToRun;
  ProgressBar1.Position := 0;
  try
    RunTests;
  finally
    saveState;
  end;
end;


end.

