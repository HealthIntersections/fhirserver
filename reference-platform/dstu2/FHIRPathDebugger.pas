unit FHIRPathDebugger;

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

{$IFNDEF FHIR2}
This is the dstu2 version of the FHIR code
{$ENDIF}


// see SmartOnFhirUtilities for doco

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, {$IFDEF NPPUNICODE} NppForms,{$ENDIF} Vcl.OleCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.ComCtrls, VirtualTrees, FHIRResources, FHIRBase, FHIRTypes, nppplugin, FHIRPath, FHIRProfileUtilities,
  FHIRParserBase, FHIRParser, System.ImageList, Vcl.ImgList, AdvObjectLists, AdvGenerics, pluginutilities, FHIRContext;

const
  UMSG = WM_USER + 1;

type
  TExecutionMode = (emWaiting, emNext, emBreak, emAbort, emFinish);

  TFHIRPathDebuggerForm = class({$IFDEF NPPUNICODE}TNppForm{$ELSE} TForm {$ENDIF})
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    vtExpressions: TVirtualStringTree;
    Splitter2: TSplitter;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    mSource: TMemo;
    mResource: TMemo;
    mContext: TMemo;
    mInput: TMemo;
    mInput2: TMemo;
    mOutcome: TMemo;
    btnStop: TBitBtn;
    btnNext: TBitBtn;
    btnSkip: TBitBtn;
    btnRunToMark: TBitBtn;
    btnClearMarks: TBitBtn;
    Panel5: TPanel;
    TabSheet6: TTabSheet;
    mConsole: TMemo;
    btnFinish: TBitBtn;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vtExpressionsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtExpressionsInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vtExpressionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtExpressionsChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnClearMarksClick(Sender: TObject);
    procedure btnFinishClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure btnRunToMarkClick(Sender: TObject);
    procedure vtExpressionsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FResource : TFHIRResource;
    FContext : TFHIRBase;
    FFormat : TFHIRFormat;
    FExpression : TFHIRPathExpressionNode;
    FEngine : TFHIRPathEngine;
    FServices : TFHIRWorkerContext;
    FLog : String;
    FLayoutInProgress : boolean;
    FMode : TExecutionMode;
    FSkip : TStringList;
    FDone : TStringList;
    FCurrent : TFHIRPathExpressionNode;
    FCurrentIsOp : boolean;
    FFreq : Int64;
    FStartLast : Int64;
    FTypes : TFHIRTypeDetails;
    FOutcome : TFHIRBaseList;

    procedure ResetNode(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);

    procedure Compose(memo : TMemo; obj : TFHIRBase; name : String); overload;
    procedure Compose(memo : TMemo; obj : TFHIRBaseList; name, def : String); overload;
    procedure ApplyMarks;
    procedure SaveMarks;
    procedure Log(s : String);

    procedure Layout;
    procedure SetUp;
    procedure Go;
    procedure Init(var Msg: TMessage); message UMSG;
    function WantStop(package : TFHIRPathDebugPackage) : boolean;
    procedure DoDebug(source : TFHIRPathEngine; package : TFHIRPathDebugPackage);

  public
    { Public declarations }
  end;

var
  FHIRPathDebuggerForm: TFHIRPathDebuggerForm;


function RunPathDebugger(owner : {$IFDEF NPPUNICODE}TNppPlugin{$ELSE} TComponent {$ENDIF};
    services : TFHIRWorkerContext;
    resource : TFHIRResource; context : TFHIRBase; path : String; fmt : TFHIRFormat;
    out types : TFHIRTypeDetails; out items : TFHIRBaseList) : boolean;

implementation

{$R *.dfm}

uses
  FHIRPluginSettings, textUtilities;


function getId(expr : TFHIRPathExpressionNode; op : boolean) : String;
begin
  if (op) then
    result := inttostr(expr.uniqueId)+'.op'
  else
    result := inttostr(expr.uniqueId);
end;

procedure TFHIRPathDebuggerForm.FormCreate(Sender: TObject);
begin
  inherited;
  mSource.Font.Name := Settings.FontName;
  mSource.Font.Size := Settings.FontSize;
  mResource.Font.Name := Settings.FontName;
  mResource.Font.Size := Settings.FontSize;
  mContext.Font.Name := Settings.FontName;
  mContext.Font.Size := Settings.FontSize;
  mInput.Font.Name := Settings.FontName;
  mInput.Font.Size := Settings.FontSize;
  mInput2.Font.Name := Settings.FontName;
  mInput2.Font.Size := Settings.FontSize;
  mOutcome.Font.Name := Settings.FontName;
  mOutcome.Font.Size := Settings.FontSize;
  mConsole.Font.Name := Settings.FontName;
  mConsole.Font.Size := Settings.FontSize;
  vtExpressions.NodeDataSize := sizeof(TTreeDataPointer);
  FSkip := TStringList.Create;
  FDone := TStringList.Create;
  QueryPerformanceFrequency(FFreq);
end;

function RunPathDebugger(owner : {$IFDEF NPPUNICODE}TNppPlugin{$ELSE} TComponent {$ENDIF};
    services : TFHIRWorkerContext;
    resource : TFHIRResource; context : TFHIRBase; path : String; fmt : TFHIRFormat;
    out types : TFHIRTypeDetails; out items : TFHIRBaseList) : boolean;
begin
  FHIRPathDebuggerForm := TFHIRPathDebuggerForm.Create(owner);
  try
    FHIRPathDebuggerForm.FResource := resource.link;
    FHIRPathDebuggerForm.FContext := context.link;
    FHIRPathDebuggerForm.FFormat := fmt;
    FHIRPathDebuggerForm.FServices := services.link;
    FHIRPathDebuggerForm.mSource.Text := path;
    result := FHIRPathDebuggerForm.ShowModal = mrOk;
    types := FHIRPathDebuggerForm.FTypes.Link;
    items := FHIRPathDebuggerForm.FOutcome.Link;
  finally
    FreeAndNil(FHIRPathDebuggerForm);
  end;
end;

procedure TFHIRPathDebuggerForm.FormDestroy(Sender: TObject);
begin
  FTypes.Free;
  FOutcome.Free;
  FResource.Free;
  FContext.Free;
  FExpression.Free;
  FEngine.Free;
  FServices.Free;
  FSkip.Free;
  FDone.Free;
  inherited;
end;

procedure TFHIRPathDebuggerForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) then
    case Key of
      VK_F2 : btnStopClick(nil);
      VK_F5 : btnRunToMarkClick(nil);
      VK_F7 : btnNextClick(nil);
      VK_F8 : btnSkipClick(nil);
      VK_F9 : btnFinishClick(nil);
    end;
end;

procedure TFHIRPathDebuggerForm.FormResize(Sender: TObject);
begin
  if not FLayoutInProgress then
  begin
    Settings.DebuggerHeight := Height;
    Settings.DebuggerWidth := Width;
  end;

end;

procedure TFHIRPathDebuggerForm.FormShow(Sender: TObject);
begin
  Layout;
  SetUp;
  Go;
end;

procedure TFHIRPathDebuggerForm.Go;
begin
  PostMessage(handle, UMSG, 0, 0);
end;

procedure TFHIRPathDebuggerForm.Init(var Msg: TMessage);
begin
  QueryPerformanceCounter(FStartLast);
  try
    FOutcome := FEngine.evaluate(nil, FResource, FContext, FExpression);
    ModalResult := mrok;
  except
    on E : EAbort do
      ModalResult := mrCancel;
    on e : Exception do
    begin
      Log(e.message);
      MessageDlg('Error executing expression: '+e.Message, mtError, [mbOk], 0);
      ModalResult := mrCancel;
    end;
  end;
end;

procedure TFHIRPathDebuggerForm.Layout;
begin
  FLayoutInProgress := true;
  Height := Settings.DebuggerHeight;
  Width := Settings.DebuggerWidth;
  Panel2.Height := Settings.DebuggerSourceHeight;
  Panel4.Width := Settings.DebuggerBreaksWidth;
  PageControl1.ActivePageIndex := Settings.DebuggerActivePage;
  FLayoutInProgress := false;
end;

procedure TFHIRPathDebuggerForm.Log(s: String);
begin
//  FLog := FLog + s + #13#10;
//  StringToFile(FLog, 'c:\temp\log.txt', TEncoding.UTF8);
  mConsole.Lines.Add(s);
end;

procedure TFHIRPathDebuggerForm.PageControl1Change(Sender: TObject);
begin
  if not FLayoutInProgress then
    Settings.DebuggerActivePage := PageControl1.ActivePageIndex;
end;

procedure TFHIRPathDebuggerForm.SetUp;
begin
  compose(mResource, FResource, 'Resource');
  compose(mContext, FContext, 'Context');
  mInput.Text := '';
  mInput2.Text := '';
  mOutcome.Text := '';
  mConsole.Text := '';
  FEngine := TFHIRPathEngine.create(FServices.link);
  FEngine.OnDebug := DoDebug;
  try
    FExpression := FEngine.parse(mSource.Text);
  except
    on e : Exception do
    begin
      Log('Error parsing Expression: ' + e.Message);
      MessageDlg('Error parsing Expression: ' + e.Message, mtError, [mbok], 0);
      ModalResult := mrCancel;
      exit;
    end;
  end;
  ApplyMarks;
  try
    Ftypes := FEngine.check(nil, FResource.FhirType, FContext.FhirType, mSource.Text, FExpression, false);
    Log('Expression Parsed OK');
  except
    on e : Exception do
    begin
      Log('Error checking Expression: ' + e.Message);
      Log('Proceeding anyway');
    end;
  end;
  vtExpressions.RootNodeCount := 1;
  vtExpressions.FullExpand();
  FMode := emNext;
  FSkip.Clear;
end;

procedure TFHIRPathDebuggerForm.Splitter1Moved(Sender: TObject);
begin
  if not FLayoutInProgress then
    Settings.DebuggerSourceHeight := Panel2.Height;
end;

procedure TFHIRPathDebuggerForm.Splitter2Moved(Sender: TObject);
begin
  if not FLayoutInProgress then
    Settings.DebuggerBreaksWidth := Panel4.Width;
end;

procedure UpdateMarks(expr : TFHIRPathExpressionNode; marks : TStringList);
var
  c : TFHIRPathExpressionNode;
begin
  if expr = nil then
    exit;
  if marks.IndexOf(inttostr(expr.uniqueId)) > -1 then
    expr.tag := 1
  else
    expr.tag := 0;

  if expr.ParameterCount > 0 then
    for c in expr.Parameters do
      UpdateMarks(c, marks);
  UpdateMarks(expr.Inner, marks);
  UpdateMarks(expr.Group, marks);
  UpdateMarks(expr.OpNext, marks);
end;

procedure GetMarks(expr : TFHIRPathExpressionNode; marks : TStringList);
var
  c : TFHIRPathExpressionNode;
begin
  if expr = nil then
    exit;
  if expr.tag = 1 then
    marks.Add(inttostr(expr.uniqueId));
  if expr.ParameterCount > 0 then
    for c in expr.Parameters do
      GetMarks(c, marks);
  GetMarks(expr.Inner, marks);
  GetMarks(expr.Group, marks);
  GetMarks(expr.OpNext, marks);
end;

procedure TFHIRPathDebuggerForm.ApplyMarks;
var
  ts : TStringList;
begin
  ts := TStringList.Create;
  try
    ts.CommaText := Settings.DebuggerBreaks;
    UpdateMarks(FExpression, ts);
  finally
    ts.free;
  end;
end;

procedure TFHIRPathDebuggerForm.SaveMarks;
var
  ts : TStringList;
begin
  ts := TStringList.Create;
  try
    GetMarks(FExpression, ts);
    Settings.DebuggerBreaks := ts.CommaText;
  finally
    ts.free;
  end;
end;

procedure TFHIRPathDebuggerForm.btnStopClick(Sender: TObject);
begin
  FMode := emAbort;
end;

procedure TFHIRPathDebuggerForm.btnNextClick(Sender: TObject);
begin
  FMode := emNext;
end;

procedure TFHIRPathDebuggerForm.btnSkipClick(Sender: TObject);
begin
  FSkip.Add(inttostr(FCurrent.uniqueId));
  FMode := emNext;
end;

procedure TFHIRPathDebuggerForm.btnRunToMarkClick(Sender: TObject);
begin
  FMode := emBreak;
end;

procedure TFHIRPathDebuggerForm.btnFinishClick(Sender: TObject);
begin
  FMode := emFinish;
end;

procedure TFHIRPathDebuggerForm.btnClearMarksClick(Sender: TObject);
begin
  vtExpressions.IterateSubtree(vtExpressions.RootNode.FirstChild, ResetNode, nil);
  Settings.DebuggerBreaks := '';
  vtExpressions.InvalidateChildren(vtExpressions.RootNode.FirstChild, true);
end;

procedure TFHIRPathDebuggerForm.Compose(memo: TMemo; obj: TFHIRBaseList; name, def: String);
var
  comp : TFHIRComposer;
begin
  if (obj = nil) then
    memo.text := def
  else
  begin
    if (FFormat = ffJson) then
      comp := TFHIRJsonComposer.Create(FServices.link, 'en')
    else
      comp := TFHIRXmlComposer.Create(FServices.link, 'en');
    try
      memo.Text := comp.Compose(name, obj, true)
    finally
      comp.Free;
    end;
  end;
end;

procedure TFHIRPathDebuggerForm.Compose(memo: TMemo; obj: TFHIRBase; name : String);
var
  comp : TFHIRComposer;
begin
  if (FFormat = ffJson) then
    comp := TFHIRJsonComposer.Create(FServices.link, 'en')
  else
    comp := TFHIRXmlComposer.Create(FServices.link, 'en');
  try
    if obj is TFHIRResource then
      memo.Text := comp.Compose(TFHIRResource(obj), true)
    else
      memo.Text := comp.Compose(name, obj, true)
  finally
    comp.Free;
  end;
end;

procedure TFHIRPathDebuggerForm.DoDebug(source : TFHIRPathEngine; package : TFHIRPathDebugPackage);
var
  id : string;
  tc : Int64;
  l : integer;
begin
  QueryPerformanceCounter(tc);
  if WantStop(package) then
  begin
    Compose(mInput, package.input1, 'Input1', 'error?');
    if (package.IsOperation) then
      Compose(mInput2, package.input2, 'Input2', 'not evaluated (short circuit)')
    else
      Compose(mInput2, package.input2, 'Input2', 'n/a');
    Compose(mOutcome, package.outcome, 'Outcome', 'error?');
    mSource.SelStart := SendMessage(mSource.Handle, EM_LINEINDEX, package.SourceStart.line-1, 0) + package.SourceStart.col-1;
    mSource.SelLength := (SendMessage(mSource.Handle, EM_LINEINDEX, package.SourceEnd.line-1, 0) + package.SourceEnd.col-1) - mSource.SelStart;
    FCurrent := package.Expression;
    FCurrentIsOp := package.IsOperation;
    id := getId(package.Expression, FCurrentIsOp);
    if FDone.IndexOf(id) = -1 then
      FDone.Add(id);
    vtExpressions.Invalidate;
    l := trunc(((tc-FStartLast)/FFreq)*1000000);
    if FCurrentIsOp then
      Log(CODES_TFHIRPathOperation[FCurrent.Operation]+' ('+inttostr(l)+'μs)')
    else
      Log(FCurrent.summary()+' ('+inttostr(l)+'μs)');

    FMode := emWaiting;
    while FMode = emWaiting do
      Application.ProcessMessages;
    if FMode = emAbort then
      abort;
     QueryPerformanceCounter(FStartLast);
  end;
end;

procedure TFHIRPathDebuggerForm.ResetNode(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  p : PTreeDataPointer;
begin
  p := vtExpressions.GetNodeData(node);
  node.CheckState := csUncheckedNormal;
  p.expr.tag := 0;
  vtExpressions.InvalidateNode(vtExpressions.RootNode.FirstChild);
end;

procedure TFHIRPathDebuggerForm.vtExpressionsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  p : PTreeDataPointer;
begin
  p := vtExpressions.GetNodeData(node);
  if (node.CheckState = csCheckedNormal) then
    p.expr.Tag := 1
  else
    p.expr.Tag := 0;
  SaveMarks;
end;

procedure TFHIRPathDebuggerForm.vtExpressionsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  p : PTreeDataPointer;
begin
  p := vtExpressions.GetNodeData(node);
  if (p.expr = FCurrent) and (p.op = FCurrentIsOp) then
    ImageIndex := 2
  else if FDone.indexof(getid(p.expr, p.op)) > -1 then
    ImageIndex := 1
  else
    ImageIndex := 0;
end;

procedure TFHIRPathDebuggerForm.vtExpressionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
begin
  p := vtExpressions.GetNodeData(node);
  if p.expr = nil then
    CellText := '??'
  else if (p.op) then
    CellText := CODES_TFHIRPathOperation[p.expr.Operation]
  else case p.expr.kind of
    enkName : CellText := p.expr.name;
    enkFunction : CellText := CODES_TFHIRPathFunctions[p.expr.FunctionId]+'()';
    enkConstant : CellText := '"'+p.expr.constant+'"';
    enkGroup : CellText := '(Group)';
  end;
end;

procedure TFHIRPathDebuggerForm.vtExpressionsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  p : PTreeDataPointer;
begin
  p := vtExpressions.GetNodeData(node);
  if p.op then
    ChildCount := 0
  else
  begin
    ChildCount := p.expr.ParameterCount;
    if (p.expr.Inner <> nil) then
      inc(ChildCount);
    if (p.expr.Group <> nil) then
      inc(ChildCount);
    if (p.expr.Operation <> popNull) then
      inc(ChildCount, 2);
  end;
end;

procedure TFHIRPathDebuggerForm.vtExpressionsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p, pp : PTreeDataPointer;
  pe : TFHIRPathExpressionNode;
  i : integer;
begin
  p := vtExpressions.GetNodeData(Node);
  if ParentNode = nil then
  begin
    p.expr := FExpression;
    Node.CheckType := ctNone;
  end
  else
  begin
    Node.CheckType := ctCheckBox;
    pp := vtExpressions.GetNodeData(parentNode);
    pe := pp.expr;

    // possible nodes:
    case pe.kind of
      enkName: i := 0; // no child nodes
      enkFunction:
        begin
          i := pe.Parameters.Count;
          if node.Index < i then
            p.expr := pe.Parameters[node.Index];
        end;
      enkConstant: i := 0; // no children
      enkGroup:
        begin
        i := 1;
        if node.Index = 0 then
          p.expr := pe.Group;
        end;
    end;
    if (pe.Inner <> nil) then
    begin
      if node.Index = i then
        p.expr := pe.Inner;
      inc(i);
    end;
    case node.Index - i of
      0: begin
         p.expr := pe;
         p.op := true;
         end;
      1: p.expr := pe.OpNext;
    else if node.index - i > 0 then
      raise Exception.Create('not done yet');
    end;
  end;
  if p.expr.tag = 1 then
    node.CheckState := csCheckedNormal;
  if not p.op and ((p.expr.Inner <> nil) or (p.expr.Group <> nil) or (p.expr.ParameterCount > 0) or (p.expr.OpNext <> nil)) then
     InitialStates := [ivsHasChildren];
end;

function TFHIRPathDebuggerForm.WantStop(package: TFHIRPathDebugPackage): boolean;
begin
  case FMode of
    emWaiting : result := true; // though this should not happen
    emNext : result := FSkip.IndexOf(inttostr(package.Expression.uniqueId)) = -1;
    emBreak : result := package.Expression.tag = 1;
    emAbort : result := true; // though this should not happen
    emFinish : result := false;
  end;
end;

end.


