unit fui_lcl_tester;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, ActnList, Menus, IniFiles, LCLIntf, laz.VirtualTrees, ClipBrd,
  Generics.Collections,
  fpcunit, testregistry, testdecorator,
  fsl_utilities, fsl_threads;

type
  TTestCheckState = (tcsUnchecked, tcsChecked, tcsMixed);
  TTestOutcome = (toUnknown, toNotRun, toRunning, toChildRunning, toPass, toFail, toError, toSomePass);

  TTestNodeList = class;

  { TTestNode }

  TTestNode = class (TObject)
  private
    FDuration: Int64;
    FExceptionClassName: string;
    FExceptionMessage: string;
    FLineNumber: longint;
    FParent : TTestNode;
    FChildren : TTestNodeList;
    FSourceUnitName: string;
    FTest : TTest;
    FOutcome : TTestOutcome;
    FStartTime : UInt64;
    FNode : PVirtualNode;

    FExecute : boolean;
    function GetCheckState: TTestCheckState;
    procedure SetCheckState(AValue: TTestCheckState);
    procedure SetDuration(AValue: Int64);
    procedure SetOutcome(AValue: TTestOutcome);

    function testCount : cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    property test : TTest read FTest;
    property node : PVirtualNode read FNode;
    property checkState : TTestCheckState read GetCheckState write SetCheckState;
    property outcome : TTestOutcome read FOutcome write SetOutcome;
    property execute : Boolean read FExecute write FExecute;
    property parent : TTestNode read FParent;
    property duration : Int64 read FDuration write SetDuration;

    property ExceptionMessage: string read FExceptionMessage write FExceptionMessage;
    property ExceptionClassName: string read FExceptionClassName write FExceptionClassName;
    property SourceUnitName: string read FSourceUnitName write FSourceUnitName;
    property LineNumber: longint read FLineNumber write FLineNumber;

    function description : String;
    function details(indent : String) : String;
    procedure start;
    procedure finish;
  end;

  { TTestNodeList }

  TTestNodeList = class (TObjectList<TTestNode>)
  public
    function forTest(test : TTest) : TTestNode;
  end;

  TTestNodeData = record
    node : TTestNode;
  end;
  PTestNodeData = ^TTestNodeData;

  TTestEventKind = (tekStartSuite, tekFinishSuite, tekStart, tekEnd, tekFail, tekError, tekEndRun);

  TTestEvent = class (TObject)
  private
    node : TTestNode;
    event : TTestEventKind;
    duration : Cardinal;
    excMessage : String;
    excClass : String;
  end;
  TTestEventQueue = class (TObjectList<TTestEvent>);

  { TTestThread }

  TTestThread = class (TThread)
  protected
    procedure execute; override;
  end;

  { TTesterForm }
  TTesterForm = class(TForm, ITestListener)
    actionTestSelectAll: TAction;
    actionTestUnselectAll: TAction;
    actionTestReset: TAction;
    actionTestCopy: TAction;
    actionTestStop: TAction;
    actionTestRunFailed: TAction;
    actTestRunAll1: TAction;
    actTestRunSelected: TAction;
    ActionList1: TActionList;
    ilMain: TImageList;
    ilOutcomes: TImageList;
    lblStatus: TLabel;
    MenuItem4: TMenuItem;
    Panel2: TPanel;
    Timer1: TTimer;
    tvTests: TLazVirtualStringTree;
    MenuItem3: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pbBar: TPaintBox;
    Panel1: TPanel;
    pmTests: TPopupMenu;
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
    procedure actionTestCopyExecute(Sender: TObject);
    procedure actionTestResetExecute(Sender: TObject);
    procedure actionTestRunFailedExecute(Sender: TObject);
    procedure actionTestSelectAllExecute(Sender: TObject);
    procedure actionTestStopExecute(Sender: TObject);
    procedure actionTestUnselectAllExecute(Sender: TObject);
    procedure actTestRunAll1Execute(Sender: TObject);
    procedure actTestRunSelectedExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbBarPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure tvTestsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvTestsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure tvTestsRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FIni : TIniFile;
    FTestInfo : TTestNodeList;
    FRunningTest : TTestNode;
    FTestsTotal, FTestsCount, FFailCount, FErrorCount : cardinal;
    FWantStop : boolean;
    FSelectedNode : TTestNode;
    FTestResult : TTestResult;
    FLock : TFslLock;
    FIncoming : TTestEventQueue;
    FShuttingDown : boolean;

    procedure LoadTree;
    procedure BuildTree(rootTest: TTestNode; aSuite: TTestSuite);
    Function registerTestNode(parent : TTestNode; test : TTest; name : String) : TTestNode;

    procedure loadState;
    procedure saveState;

    procedure setDoExecute(node: TTestNode);
    procedure setDoExecuteParent(node: TTestNode);
    procedure setTestState(node: TTestNode; state : TTestCheckState);
    procedure checkStateOfChildren(node: TTestNode);
    procedure setActionStatus(running : boolean);
    procedure StartTestRun;
    procedure DoExecuteTests; // in alternative thread
    procedure FinishTestRun;
    function tn(p: PVirtualNode): TTestNode;
    procedure UpdateTotals;
    procedure queueEvent(test: TTestNode; event : TTestEventKind; msg, clssName : String);
  public
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite); overload;
    procedure EndTestSuite(ATest: TTestNode); overload;
    procedure EndAll(ATest: TTestNode);
  end;

var
  TesterForm: TTesterForm;

implementation

{$R *.lfm}

resourcestring
  rsAllTests = 'All Tests';
  //rsRun = 'Run ';
  rsRuns = 'Tests: %s/%s';
  rsErrors = '%s    Errors: %s';
  rsFailures = '%s     Failures: %s';

  //rsMessage = 'Message: %s';
  //rsException = 'Exception: %s';
  //rsExceptionMes = 'Exception message: %s';
  //rsExceptionCla = 'Exception class: %s';
  //rsUnitName = 'Unit name: %s';
  //rsMethodName = 'Method name: %s';
  //rsLineNumber = 'Line number: %s';
  //rsRunning = 'Running %s';
  //rsNumberOfExec = 'Number of executed tests: %s  Time elapsed: %s';
  //// Visual components captions
  //sfrmGUITest = 'FPCUnit - run unit test';
  //sbtnRun = 'Run';
  //sbtnRunH = 'Run highlighted test';
  //sbtnClose = 'Close';
  //stshTree = 'Testcase tree';
  //stshResults = 'Results XML';
  //sactRunAction = '&Run all';
  //sactRunActionH = 'Run all checked tests';
  //sactCloseForm = 'Quit';
  //sactCloseFormH = 'Quit testing';
  //sactCheckCurrentSuite = 'Select current suite';
  //sactUncheckCurrentSuite = 'Deselect current suite';
  //sactCheckAll = 'Select all tests';
  //sactUncheckAll = 'Deselect all tests';
  //sactRunHighlightedTest = 'Run selected';
  //sactRunHighlightedTestH = 'Run selected test';
  //smiActions = 'Actions';
  //smiTestTree = 'Test tree';
  //smiEdit = 'Edit';
  //sactCopyAllToClipboard = 'Copy text to clipboard';
  //sactCopyAllToClipboardH = 'Copy the entire text to clipboard';
  //sactSaveResults = 'Save results';
  //sactSaveResultsH = 'Save XML results to file';

{ TTestThread }

procedure TTestThread.execute;
begin
  try
    TesterForm.DoExecuteTests;
  except
  end;
end;

{ TTestNode }

procedure TTestNode.SetOutcome(AValue: TTestOutcome);
begin
  FOutcome := AValue;
  TesterForm.tvTests.InvalidateNode(FNode);
end;

function TTestNode.testCount: cardinal;
var
  child : TTestNode;
begin
  result := 0;
  for child in FChildren do
    if (child.test is TTestCase) then
      inc(result)
    else
      inc(result, child.testCount);
end;

constructor TTestNode.Create;
begin
  inherited Create;
  FChildren := TTestNodeList.create;
  FChildren.OwnsObjects := false;
  FDuration := -1;
end;

destructor TTestNode.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

function TTestNode.description: String;
begin
  if FParent = nil then
    result := rsAllTests
  else if FTest.TestName <> '' then
    result := FTest.TestName
  else
    result := FTest.ClassName;

  if (FDuration > -1) or (FChildren.Count > 0) then
  begin
    result := result + ' (';
    if (FChildren.Count > 0) then
      result := result + inttostr(testCount)+' tests';
    if (FDuration > -1) and (FChildren.Count > 0) then
      result := result + ', ';
    if FDuration > -1 then
      result := result + inttostr(FDuration)+'ms';
    result := result + ')';
  end;
  if ExceptionMessage <> '' then
    result := result +': '+ExceptionMessage;
end;

function TTestNode.details(indent : String): String;
var
  b : TStringBuilder;
  child : TTestNode;
  tn : String;
begin
  if FChildren.Count = 0 then
  begin
    result := indent+FTest.TestName;
    if (FDuration > -1) or (FChildren.Count > 0) then
    begin
      result := result + ' (';
      if (FChildren.Count > 0) then
        result := result + inttostr(testCount)+' tests';
      if (FDuration > -1) and (FChildren.Count > 0) then
        result := result + ', ';
      if FDuration > -1 then
        result := result + inttostr(FDuration)+'ms';
      result := result + ')';
    end;
    if FExceptionMessage <> '' then
      if FSourceUnitName <> '' then
        result := result + '. '+FExceptionClassName+': '+FExceptionMessage+' (@'+FSourceUnitName+'#'+inttostr(FLineNumber)
      else
        result := result + '. '+FExceptionClassName+': '+FExceptionMessage;
    result := result + #13#10;
  end
  else
  begin
    b := TStringBuilder.create;
    try
      if FParent = nil then
        tn := rsAllTests
      else
        tn := FTest.TestName;
      b.append(indent+'-- '+tn+' Starts ---------------------------------'+#13#10);
      for child in FChildren do
        b.append(child.details(indent+'  '));
      b.append(indent+'-- '+tn+' Ends -----------------------------------'+#13#10);
      result := b.toString;
    finally
      b.free;
    end;
  end;
end;

procedure TTestNode.start;
begin
  FStartTime := GetTickCount64;
end;

procedure TTestNode.finish;
begin
  if FStartTime >0 then
  begin
    Duration := GetTickCount64 - FStartTime;
    FStartTime := 0;
  end;
end;

procedure TTestNode.SetCheckState(AValue: TTestCheckState);
begin
  case AValue of
    tcsUnchecked : FNode.CheckState := csUncheckedNormal;
    tcsChecked : FNode.CheckState := csCheckedNormal;
    tcsMixed : FNode.CheckState := csMixedNormal;
  end;
  TesterForm.tvTests.InvalidateNode(FNode);
end;

function TTestNode.GetCheckState: TTestCheckState;
begin
  case FNode.CheckState of
    csUncheckedNormal, csUncheckedPressed : result := tcsUnchecked;
    csCheckedNormal, csCheckedPressed : result := tcsChecked;
    csMixedNormal, csMixedPressed : result := tcsMixed;
  end;
end;

procedure TTestNode.SetDuration(AValue: Int64);
begin
  FDuration := AValue;
  // don't do this - we're in the test thread, and update will be called later... TesterForm.tvTests.InvalidateNode(FNode);
end;

{ TTestNodeList }

function TTestNodeList.forTest(test: TTest): TTestNode;
var
  ti : TTestNode;
begin
  result := nil;
  for ti in self do
    if ti.test = test then
      exit(ti);
end;

{ TTesterForm }

procedure TTesterForm.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.create(IncludeTrailingPathDelimiter(getAppConfigDir(true))+'fhir-tests-settings.ini');
  FTestInfo := TTestNodeList.create;
  FTestInfo.OwnsObjects := true;
  FLock := TFslLock.create;
  FIncoming := TTestEventQueue.create;
  FIncoming.OwnsObjects := false;

  pbBarPaint(pbBar);
  setActionStatus(false);
end;

procedure TTesterForm.FormDestroy(Sender: TObject);
begin
  FShuttingDown := true;
  saveState;
  FIni.Free;
  FTestInfo.Free;
  FIncoming.Free;
  FLock.Free;
end;

procedure TTesterForm.FormShow(Sender: TObject);
begin
  LoadTree;
  LoadState;
  UpdateTotals;
end;

procedure TTesterForm.setActionStatus(running: boolean);
begin
  actionTestCopy.Enabled := not running;
  actionTestSelectAll.Enabled := not running;
  actionTestUnselectAll.Enabled := not running;
  actionTestReset.Enabled := not running;
  actionTestStop.Enabled := running;
  actionTestRunFailed.Enabled := not running;
  actTestRunAll1.Enabled := not running;
  actTestRunSelected.Enabled := not running;
end;

// -- Tree Management ----------------------------------------------------------

function TTesterForm.tn(p : PVirtualNode) : TTestNode;
var
  pd : PTestNodeData;
begin
  pd := tvTests.GetNodeData(p);
  result := pd.node;
end;

procedure TTesterForm.UpdateTotals;
var
  tc, pc, ec, fc, nr, cc : cardinal;
  tn : TTestNode;
  s : String;
begin
  if ((FRunningTest <> nil) or FShuttingDown) then
    exit;

  tc := 0;
  ec := 0;
  fc := 0;
  nr := 0;
  cc := 0;
  pc := 0;
  for tn in FTestInfo do
  begin
    if tn.test is TTestCase then
    begin
      inc(tc);
      if tn.checkState = tcsChecked then
        inc(cc);
      case tn.outcome of
        toPass : inc(pc);
        toFail : inc(ec);
        toError : inc(fc);
      else //  toNotRun
        inc(nr);
      end;
    end;
  end;

  s := inttostr(tc)+ ' Tests: ';
  s := s + inttostr(FTestInfo.count)+ ' checked';
  s := s + ', '+inttostr(pc)+' passed';
  if (fc + ec > 0) then
    s := s + ', '+inttostr(fc+ec)+' failed';
  if (ec > 0) then
    s := s + ' ('+inttostr(ec)+' errors)';
  if (nr > 0) then
    s := s + ', '+inttostr(nr)+' not run';
  if (ec + fc + nr = 0) then
    s := s + ' - All OK✓';
  lblStatus.caption := s;
end;

procedure TTesterForm.tvTestsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := ord(tn(node).outcome);
end;

procedure TTesterForm.tvTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  CellText := tn(node).description;
end;

procedure TTesterForm.LoadTree;
var
  test : TTestSuite;
  node: TTestNode;
begin
  tvTests.NodeDataSize := sizeof(pointer);
  test := GetTestRegistry;
  node := registerTestNode(nil, test, rsAllTests);
  BuildTree(node, test);
  tvTests.Selected[FTestInfo[0].node] := true;
  tvTests.Expanded[FTestInfo[0].node] := true;
end;

procedure TTesterForm.BuildTree(rootTest: TTestNode; aSuite: TTestSuite);
var
  test: TTestNode;
  i: integer;
begin
  for i := 0 to ASuite.ChildTestCount - 1 do
  begin
    if (ASuite.Test[i].TestName = '') and (ASuite.ChildTestCount = 1) then
      test := rootTest
    else
      test := registerTestNode(rootTest, ASuite.Test[i], ASuite.Test[i].TestName);

    if ASuite.Test[i] is TTestSuite then
      BuildTree(test, TTestSuite(ASuite.Test[i]))
    else if TObject(ASuite.Test[i]).InheritsFrom(TTestDecorator) then
      BuildTree(test, TTestSuite(TTestDecorator(ASuite.Test[i]).Test));
  end;
end;

function TTesterForm.registerTestNode(parent: TTestNode; test: TTest; name: String): TTestNode;
var
  p : PTestNodeData;
begin
  result := TTestNode.create;
  FTestInfo.add(result);

  if (parent <> nil) then
  begin
    result.FParent := parent;
    parent.FChildren.add(result);
    result.FNode := tvTests.AddChild(parent.Node);
  end
  else
    result.FNode := tvTests.AddChild(nil);
  p := tvTests.GetNodeData(result.FNode);
  p.node := result;
  result.FTest := test;
  result.checkState := tcsUnchecked;
  result.outcome := toNotRun;
  result.FNode.CheckType := ctTriStateCheckBox;
  result.FNode.CheckState := csUncheckedNormal;
end;


// --- Test Selection Management -----------------------------------------------

procedure TTesterForm.tvTestsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelectedNode := tn(node);
  if FSelectedNode.FChildren.Count = 0 then
  begin
    actTestRunSelected.caption := 'Run this test';
    actionTestSelectAll.caption := 'Check this test';
    actionTestUnselectAll.caption := 'Uncheck this test';
    actionTestSelectAll.hint := 'Check selected test';
    actionTestUnselectAll.hint := 'Uncheck selected test';
    actionTestCopy.hint := 'Copy results to clipboard for selected test';
    actTestRunSelected.caption := 'Run Selected Test';
  end
  else
  begin
    actTestRunSelected.caption := 'Run these tests';
    actionTestSelectAll.caption := 'Check these tests';
    actionTestUnselectAll.caption := 'Uncheck these tests';
    actionTestSelectAll.hint := 'Check selected tests + children';
    actionTestUnselectAll.hint := 'Uncheck selected tests + children';
    actionTestCopy.hint := 'Copy results to clipboard for selected tests';
    actTestRunSelected.caption := 'Run Selected Test + children';
  end;
  UpdateTotals;
end;

procedure TTesterForm.tvTestsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
end;

procedure TTesterForm.tvTestsRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelectedNode := nil;
  UpdateTotals;
end;


procedure TTesterForm.loadState;
var
  ss, so : String;
  i : integer;
  ti : TTestNode;
begin
  tvTests.BeginUpdate;

  if (FIni.ReadInteger('Tests', 'Count', 0) = FTestInfo.Count) then
  begin
    ss := FIni.ReadString('Tests', 'States', '');
    so := FIni.ReadString('Tests', 'Outcomes', '');
    for i := 0 to FTestInfo.count - 1 do
    begin
      ti := FTestInfo[i];
      if i < length(ss) then
        ti.checkState := TTestCheckState(StrToInt(ss[i+1]))
      else
        ti.checkState := tcsUnchecked;

      if i < length(so) then
      begin
        ti.outcome := TTestOutcome(StrToInt(so[i+1]));
        if not (ti.outcome in [toSomePass, toPass, toFail, toError]) then
          ti.outcome := toNotRun;
      end
      else
        ti.outcome := toNotRun;
    end;
  end;

  tvTests.EndUpdate;
end;

procedure TTesterForm.saveState;
var
  bs, bo : TStringBuilder;
  ti : TTestNode;
begin
  bs := TStringBuilder.create;
  bo := TStringBuilder.create;
  try
    for ti in FTestInfo do
    begin
      bs.append(inttostr(ord(ti.checkState)));
      bo.append(inttostr(ord(ti.outcome)));
    end;
    FIni.WriteInteger('Tests', 'Count', FTestInfo.Count);
    FIni.WriteString('Tests', 'States', bs.toString);
    FIni.WriteString('Tests', 'Outcomes', bo.toString);
  finally
    bo.Free;
    bs.Free;
  end;
end;

procedure TTesterForm.setTestState(node: TTestNode; state: TTestCheckState);
var
  ti : TTestNode;
begin
  node.checkState := state;
  for ti in node.FChildren do
    setTestState(ti, state);
end;

procedure TTesterForm.checkStateOfChildren(node: TTestNode);
var
  state : TTestCheckState;
  ti : TTestNode;
begin
  if node <> nil then
  begin
    ti := node.FChildren[0]; // will never be empty
    state := ti.checkState;
    for ti in node.FChildren do;
      if state <> ti.checkState then
        state := tcsMixed;
    node.checkState := state;
    checkStateOfChildren(node.Parent);
  end;
end;

procedure TTesterForm.actionTestSelectAllExecute(Sender: TObject);
var
  node : TTestNode;
begin
  node := FSelectedNode;
  if (node <> nil) then
  begin
    setTestState(node, tcsChecked);
    checkStateOfChildren(node.Parent);
  end;
  UpdateTotals;
end;

procedure TTesterForm.actionTestUnselectAllExecute(Sender: TObject);
var
  node : TTestNode;
begin
  node := FSelectedNode;
  if (node <> nil) then
  begin
    setTestState(node, tcsUnchecked);
    checkStateOfChildren(node.Parent);
  end;
  UpdateTotals;
end;

// ---- Test Execution Control -------------------------------------------------

procedure TTesterForm.actTestRunAll1Execute(Sender: TObject);
var
  ti : TTestNode;
begin
  FTestsTotal := 0;
  for ti in FTestInfo do
  begin
    ti.execute := ti.CheckState <> tcsUnchecked;
    if (ti.execute) and (ti.test is TTestCase) then
      inc(FTestsTotal);
  end;

  if FTestsTotal > 0 then
  begin
    FRunningTest := FTestInfo[0];
    StartTestRun;
    TTestThread.create(false).FreeOnTerminate := true;
  end
  else
    ShowMessage('No Tests Checked');
end;

procedure TTesterForm.actTestRunSelectedExecute(Sender: TObject);
var
  node, ti : TTestNode;
begin
  for ti in FTestInfo do
    ti.execute := false;
  Node := FSelectedNode;
  if node <> nil then
  begin
    setDoExecute(node);
    setDoExecuteParent(node.Parent);

    FTestsTotal := 0;
    for ti in FTestInfo do
      if ti.execute and (ti.test is TTestCase) then
        inc(FTestsTotal);
    FRunningTest := node;
    StartTestRun;
    TTestThread.create(false).FreeOnTerminate := true;
  end;
end;

procedure TTesterForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FRunningTest = nil;
end;

procedure TTesterForm.actionTestRunFailedExecute(Sender: TObject);
var
  ti : TTestNode;
begin
  for ti in FTestInfo do
  begin
    ti.execute := (ti.outcome in [toFail, toError]) and (ti.CheckState <> tcsUnchecked);
    if ti.execute then
      setDoExecuteParent(ti.parent);
  end;

  FTestsTotal := 0;
  for ti in FTestInfo do
    if ti.execute and (ti.test is TTestCase) then
      inc(FTestsTotal);

  if FTestsTotal > 0 then
  begin
    FRunningTest := FTestInfo[0];
    StartTestRun;
    TTestThread.create(false).FreeOnTerminate := true;
  end
  else
    ShowMessage('No Failed Tests');
end;

procedure TTesterForm.actionTestResetExecute(Sender: TObject);
var
  ti : TTestNode;
begin
  for ti in FTestInfo do
  begin
    if ti.outcome <> toNotRun then
      ti.outcome := toNotRun;
  end;
  UpdateTotals;
end;

procedure TTesterForm.actionTestStopExecute(Sender: TObject);
begin
  FWantStop := true;
end;

procedure TTesterForm.setDoExecute(node : TTestNode);
var
  child : TTestNode;
begin
  node.execute := true;

  for child in node.FChildren do
    setDoExecute(child);
end;

procedure TTesterForm.setDoExecuteParent(node: TTestNode);
begin
  if node <> nil then
  begin
    node.execute := true;
    setDoExecuteParent(node.parent);
  end;
end;

// -- Actually executing the tests ---------------------------------------------

procedure TTesterForm.pbBarPaint(Sender: TObject);
var
  msg: string;
  OldStyle: TBrushStyle;
begin
  with (Sender as TPaintBox) do
  begin
    Canvas.Lock;
    Canvas.Brush.Color := clSilver;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Font.Color := clWhite;
    if FTestsTotal > 0 then
    begin
      if FErrorCount > 0 then
        Canvas.Brush.Color := clRed
      else if FFailCount > 0 then
        Canvas.Brush.Color := clMaroon
      else
        Canvas.Brush.Color := clGreen;
      Canvas.Rectangle(0, 0, round(FTestsCount / (FTestsTotal) * Width), Height);
      msg := Format(rsRuns, [IntToStr(FTestsCount), IntToStr(FTestsTotal)]);
      msg := Format(rsErrors, [msg, IntToStr(FErrorCount)]);
      msg := Format(rsFailures, [msg, IntToStr(FFailCount)]);
      OldStyle := Canvas.Brush.Style;
      Canvas.Brush.Style := bsClear;
      Canvas.Textout(10, 4,  msg);
      Canvas.Brush.Style := OldStyle;
    end;
    Canvas.UnLock;
  end;
end;

procedure TTesterForm.Timer1Timer(Sender: TObject);
var
  list : TTestEventQueue;
  ev : TTestEvent;
  ti : TTestNode;
begin
  list := TTestEventQueue.create;
  try
    list.OwnsObjects := true;
    FLock.Lock;
    try
      for ev in FIncoming do
        list.add(ev);
      FIncoming.Clear;
    finally
      FLock.UnLock;
    end;
    for ev in list do
    begin
      case ev.event of
        tekStartSuite:
          begin
            ev.node.outcome := toRunning;
            ti := ev.node.parent;
            while (ti <> nil) do
            begin
              ti.outcome := toChildRunning;
              ti := ti.parent;
            end;
          end;
        tekFinishSuite:
          begin
            EndTestSuite(ev.node);
          end;
        tekStart:
          begin
            ev.node.outcome := toRunning;
            ev.node.parent.outcome := toChildRunning;
            lblStatus.caption := 'Running Test '+ev.node.test.testName;
          end;
        tekEnd:
          begin
            if (ev.node.outcome = toRunning) then
              ev.node.outcome := toPass;
            Inc(FTestsCount);
            lblStatus.caption := '';
          end;
        tekFail:
          begin
            ev.node.ExceptionMessage := ev.excMessage;
            ev.node.ExceptionClassName := ev.excClass;
            ev.node.outcome := toFail;
            Inc(FFailCount);
            lblStatus.caption := '';
          end;
        tekError:
          begin
            ev.node.ExceptionMessage := ev.excMessage;
            ev.node.ExceptionClassName := ev.excClass;
            ev.node.outcome := toError;
            Inc(FErrorCount);
            lblStatus.caption := '';
          end;
        tekEndRun:
          begin
            ti := ev.node;
            if (ti <> nil) then
            begin
              ti := ti.parent;
              while (ti <> nil) do
              begin
                EndTestSuite(ti);
                ti := ti.parent;
              end;
            end;
            FinishTestRun;
          end;
      end;
    end;
    pbbar.Refresh;
    Application.ProcessMessages;
  finally
    list.free;
  end;
end;

procedure TTesterForm.EndTestSuite(ATest: TTestNode);
var
  outcome : TTestOutcome;
  child : TTestNode;
begin
  outcome := toUnknown;
  for child in ATest.FChildren do
  begin
    case child.outcome of
      toSomePass : if outcome = toUnknown then outcome := toSomePass else if outcome in [toPass, toNotRun] then outcome := toSomePass;
      toNotRun : if outcome = toUnknown then outcome := toNotRun else if outcome = toPass then outcome := toSomePass;
      toPass : if outcome = toUnknown then outcome := toPass else if outcome = toUnknown then outcome := toSomePass;
      toFail: if outcome <> toError then outcome := toFail;
      toError: outcome := toError;
    end;
  end;
  ATest.outcome := outcome;
  if ATest.FStartTime > 0 then
    ATest.finish;
end;

procedure TTesterForm.StartTestRun;
var
  ti : TTestNode;
begin
  saveState;
  FWantStop := false;
  FTestsCount := 0;
  FFailCount := 0;
  FErrorCount := 0;
  pbBar.Invalidate;
  setActionStatus(true);
  timer1.Enabled := true;
  FTestResult := TTestResult.Create;
  for ti in TesterForm.FTestInfo do
    if not ti.execute and (ti.test is TTestCase) then
      FTestResult.AddToSkipList(ti.test as TTestCase);
end;

procedure TTesterForm.FinishTestRun;
begin
  FRunningTest := nil;
  FreeAndNil(FTestResult);
  Timer1.Enabled := false;
  setActionStatus(false);
  saveState;
  UpdateTotals;
end;

procedure TTesterForm.actionTestCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := FSelectedNode.details('');
end;

// -- test thread  - no UI access ----------------------------------------------

procedure TTesterForm.DoExecuteTests;
begin
  try
    FTestResult.AddListener(self);
    if (FRunningTest.Test is TTestSuite) then
      StartTestSuite(FRunningTest.Test as TTestSuite);
    try
      FRunningTest.Test.Run(FTestResult);
    finally
      if (FRunningTest.Test is TTestSuite) then
        EndTestSuite(FRunningTest.test as TTestSuite);
    end;
  finally
    EndAll(FRunningTest);
  end;
end;

procedure TTesterForm.queueEvent(test: TTestNode; event : TTestEventKind; msg, clssName : String);
var
  ev : TTestEvent;
begin
  ev := TTestEvent.create;
  ev.node := test;
  ev.event := event;
  ev.excMessage := msg;
  ev.excClass := clssName;
  FLock.Lock;
  try
    FIncoming.add(ev);
  finally
    FLock.UnLock;
  end;
  if FWantStop then
    Abort;
end;

procedure TTesterForm.StartTest(ATest: TTest);
var
  ti : TTestNode;
begin
  ti := FTestInfo.forTest(aTest);
  ti.start;
  queueEvent(ti, tekStart, '', '');
end;

procedure TTesterForm.EndTest(ATest: TTest);
var
  ti : TTestNode;
begin
  ti := FTestInfo.forTest(aTest);
  ti.finish;
  queueEvent(ti, tekEnd, '', '');
end;

procedure TTesterForm.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  ti : TTestNode;
begin
  ti := FTestInfo.forTest(aTest);
  queueEvent(ti, tekFail, AFailure.ExceptionMessage, AFailure.ExceptionClassName);
end;

procedure TTesterForm.AddError(ATest: TTest; AError: TTestFailure);
var
  ti : TTestNode;
begin
  ti := FTestInfo.forTest(aTest);
  queueEvent(ti, tekError, AError.ExceptionMessage, AError.ExceptionClassName);
end;

procedure TTesterForm.StartTestSuite(ATestSuite: TTestSuite);
var
  ti : TTestNode;
begin
  ti := FTestInfo.forTest(ATestSuite);
  if (ti <> nil) then
  begin
    ti.start;
    queueEvent(ti, tekStartSuite, '', '');
  end;
end;

procedure TTesterForm.EndTestSuite(ATestSuite: TTestSuite);
var
  ti : TTestNode;
begin
  ti := FTestInfo.forTest(ATestSuite);
  if (ti <> nil) then
  begin
    ti.finish;
    queueEvent(ti, tekFinishSuite, '', '');
  end;
end;

procedure TTesterForm.EndAll(ATest: TTestNode);
begin
  queueEvent(ATest, tekEndRun, '', '');
end;


end.

