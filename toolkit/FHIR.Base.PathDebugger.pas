unit FHIR.Base.PathDebugger;

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

// see FHIR.Client.SmartUtilities for doco

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.ComCtrls, System.ImageList, Vcl.ImgList,
  VirtualTrees,
  {$IFDEF NPPUNICODE} FHIR.Npp.Form, FHIR.Npp.Base, {$ENDIF}

  fsl_collections,
//  v2_message,
  fhir_objects, fhir_parser, fhir_factory, fhir_pathengine,  fhir_elementmodel;

const
  UMSG = WM_USER + 1;

type
  TExecutionMode = (emWaiting, emNext, emBreak, emAbort, emFinish);

  TFHIRPathDebuggerFormSetting = (settingDebuggerHeight, settingDebuggerWidth, settingDebuggerSourceHeight, settingDebuggerBreaksWidth,
    settingDebuggerActivePage, settingDebuggerBreaks, settingDebuggerFontName, settingDebuggerFontSize);

  TFHIRPathDebuggerFormGetSetting = function (name : TFHIRPathDebuggerFormSetting) : Integer of object;
  TFHIRPathDebuggerFormSetSetting = procedure (name : TFHIRPathDebuggerFormSetting; value : Integer) of object;
  TFHIRPathDebuggerFormGetSettingStr = function (name : TFHIRPathDebuggerFormSetting) : String of object;
  TFHIRPathDebuggerFormSetSettingStr = procedure (name : TFHIRPathDebuggerFormSetting; value : String) of object;

  TFHIRPathDebuggerForm = class({$IFNDEF NPPUNICODE}TForm{$ELSE} TNppForm{$ENDIF})
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
    Panel6: TPanel;
    rbXml: TRadioButton;
    rbJson: TRadioButton;
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vtExpressionsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure rbJsonClick(Sender: TObject);
    procedure rbXmlClick(Sender: TObject);
  private
    { Private declarations }
    FResource : TFHIRResourceV;
    FContext : TFHIRObject;
    FFormat : TFHIRFormat;
    FExpression : TFHIRPathExpressionNodeV;
    FEngine : TFHIRPathEngineV;
    FServices : TFHIRWorkerContextWithFactory;
    FFactory : TFHIRFactory;
    FLog : String;
    FLayoutInProgress : boolean;
    FMode : TExecutionMode;
    FSkip : TStringList;
    FDone : TStringList;
    FCurrent : TFHIRPathExpressionNodeV;
    FCurrentIsOp : boolean;
    FFreq : Int64;
    FStartLast : Int64;
    FTypes : TFHIRTypeDetailsV;
    FOutcome : TFHIRSelectionList;
    FPackage : TFHIRPathDebugPackage;

    FOnGetSetting: TFHIRPathDebuggerFormGetSetting;
    FOnSetSetting: TFHIRPathDebuggerFormSetSetting;
    FOnGetSettingStr: TFHIRPathDebuggerFormGetSettingStr;
    FOnSetSettingStr: TFHIRPathDebuggerFormSetSettingStr;
    procedure ResetNode(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);

    procedure Compose(memo : TMemo; obj : TFHIRObject; name : String); overload;
    procedure Compose(memo : TMemo; obj : TFHIRObjectList; name, def : String); overload;
    procedure Compose(memo : TMemo; obj : TFHIRSelectionList; name, def : String); overload;
    procedure ApplyMarks;
    procedure SaveMarks;
    procedure Log(s : String);

    procedure Layout;
    Procedure SetUp;
    procedure Go;
    procedure Init(var Msg: TMessage); message UMSG;
    function WantStop(package : TFHIRPathDebugPackage) : boolean;
    procedure DoDebug(source : TFHIRPathEngineV; package : TFHIRPathDebugPackage);
    procedure SetEngine(const Value: TFHIRPathEngineV);
    procedure ShowPackage;

    property OnGetSetting : TFHIRPathDebuggerFormGetSetting read FOnGetSetting write FOnGetSetting;
    property OnSetSetting : TFHIRPathDebuggerFormSetSetting read FOnSetSetting write FOnSetSetting;
    property OnGetSettingStr : TFHIRPathDebuggerFormGetSettingStr read FOnGetSettingStr write FOnGetSettingStr;
    property OnSetSettingStr : TFHIRPathDebuggerFormSetSettingStr read FOnSetSettingStr write FOnSetSettingStr;
  public
    property Engine : TFHIRPathEngineV read FEngine write SetEngine;
  end;

var
  FHIRPathDebuggerForm: TFHIRPathDebuggerForm;

const
  CODES_TFHIRPathDebuggerFormSetting : Array [TFHIRPathDebuggerFormSetting] of String = ('FPDebuggerHeight', 'FPDebuggerWidth', 'FPDebuggerSourceHeight', 'FPDebuggerBreaksWidth',
    'FPDebuggerActivePage', 'FPDebuggerBreaks', 'FPDebuggerFontName', 'FPDebuggerFontSize');
  DEF_INTS_TFHIRPathDebuggerFormSetting : Array [TFHIRPathDebuggerFormSetting] of Integer = (772, 495, 67, 211, 0, 0, 0, 10);
  DEF_STR_TFHIRPathDebuggerFormSetting : Array [TFHIRPathDebuggerFormSetting] of String = ('', '', '', '', '', '', 'Courier New', '');

function RunPathDebugger(owner : {$IFDEF NPPUNICODE}TNppPlugin{$ELSE} TComponent {$ENDIF};
    services : TFHIRWorkerContextWithFactory; engine : TFHIRPathEngineV;
    GetSetting : TFHIRPathDebuggerFormGetSetting; SetSetting : TFHIRPathDebuggerFormSetSetting; GetSettingStr : TFHIRPathDebuggerFormGetSettingStr; SetSettingStr : TFHIRPathDebuggerFormSetSettingStr;
    factory : TFHIRFactory;
    resource : TFHIRResourceV; context : TFHIRObject; path : String; fmt : TFHIRFormat;
    out types : TFHIRTypeDetailsV; out items : TFHIRSelectionList) : boolean;

implementation

{$R *.dfm}

uses
  fsl_stream;

function getId(expr : TFHIRPathExpressionNodeV; op : boolean) : String;
begin
  if (op) then
    result := inttostr(expr.uniqueId)+'.op'
  else
    result := inttostr(expr.uniqueId);
end;

procedure TFHIRPathDebuggerForm.FormCreate(Sender: TObject);
begin
  inherited;
  vtExpressions.NodeDataSize := sizeof(TTreeDataPointer);
  FSkip := TStringList.Create;
  FDone := TStringList.Create;
  QueryPerformanceFrequency(FFreq);
end;

function RunPathDebugger(owner : {$IFDEF NPPUNICODE}TNppPlugin{$ELSE} TComponent {$ENDIF};
    services : TFHIRWorkerContextWithFactory; engine : TFHIRPathEngineV;
    GetSetting : TFHIRPathDebuggerFormGetSetting; SetSetting : TFHIRPathDebuggerFormSetSetting; GetSettingStr : TFHIRPathDebuggerFormGetSettingStr; SetSettingStr : TFHIRPathDebuggerFormSetSettingStr;
    factory : TFHIRFactory;
    resource : TFHIRResourceV; context : TFHIRObject; path : String; fmt : TFHIRFormat;
    out types : TFHIRTypeDetailsV; out items : TFHIRSelectionList) : boolean;
begin
  FHIRPathDebuggerForm := TFHIRPathDebuggerForm.Create(owner);
  try
    FHIRPathDebuggerForm.FResource := resource.link;
    FHIRPathDebuggerForm.FContext := context.link;
    if fmt = ffXml then
    begin
      FHIRPathDebuggerForm.rbXml.checked := true;
      FHIRPathDebuggerForm.FFormat := fmt;
    end
    else
    begin
      FHIRPathDebuggerForm.rbJson.checked := true;
      FHIRPathDebuggerForm.FFormat := ffJson;
    end;

    FHIRPathDebuggerForm.Engine := engine.link;
    FHIRPathDebuggerForm.FServices := services.link;
    FHIRPathDebuggerForm.FFactory := factory.Link;
    FHIRPathDebuggerForm.mSource.Text := path;
    FHIRPathDebuggerForm.OnGetSetting := GetSetting;
    FHIRPathDebuggerForm.OnSetSetting := SetSetting;
    FHIRPathDebuggerForm.OnGetSettingStr := GetSettingStr;
    FHIRPathDebuggerForm.OnSetSettingStr := SetSettingStr;

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
  FFactory.Free;
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
      VK_F6 : btnFinishClick(nil);
    end;
end;

procedure TFHIRPathDebuggerForm.FormResize(Sender: TObject);
begin
  if not FLayoutInProgress then
  begin
    FOnSetSetting(settingDebuggerHeight, Height);
    FOnSetSetting(settingDebuggerWidth, Width);
  end;

end;

procedure TFHIRPathDebuggerForm.FormShow(Sender: TObject);
begin
  mSource.Font.Name := OnGetSettingStr(settingDebuggerFontName);
  mSource.Font.Size := OnGetSetting(settingDebuggerFontSize);
  mResource.Font.Name := mSource.Font.Name;
  mResource.Font.Size := mSource.Font.Size;
  mContext.Font.Name := mSource.Font.Name;
  mContext.Font.Size := mSource.Font.Size;
  mInput.Font.Name := mSource.Font.Name;
  mInput.Font.Size := mSource.Font.Size;
  mInput2.Font.Name := mSource.Font.Name;
  mInput2.Font.Size := mSource.Font.Size;
  mOutcome.Font.Name := mSource.Font.Name;
  mOutcome.Font.Size := mSource.Font.Size;
  mConsole.Font.Name := mSource.Font.Name;
  mConsole.Font.Size := mSource.Font.Size;
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
  Height := FOnGetSetting(settingDebuggerHeight);
  Width := FOnGetSetting(settingDebuggerWidth);
  Panel2.Height := FOnGetSetting(settingDebuggerSourceHeight);
  Panel4.Width := FOnGetSetting(settingDebuggerBreaksWidth);
  PageControl1.ActivePageIndex := FOnGetSetting(settingDebuggerActivePage);
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
    FonSetSetting(settingDebuggerActivePage, PageControl1.ActivePageIndex);
end;

procedure TFHIRPathDebuggerForm.rbJsonClick(Sender: TObject);
begin
  FFormat := ffJson;
  if FPackage <> nil then
    ShowPackage;
end;

procedure TFHIRPathDebuggerForm.rbXmlClick(Sender: TObject);
begin
  FFormat := ffXml;
  if FPackage <> nil then
    ShowPackage;
end;

procedure TFHIRPathDebuggerForm.SetEngine(const Value: TFHIRPathEngineV);
begin
  FEngine.Free;
  FEngine := Value;
end;

procedure TFHIRPathDebuggerForm.SetUp;
begin
  compose(mResource, FResource, 'Resource');
  compose(mContext, FContext, 'Context');
  mInput.Text := '';
  mInput2.Text := '';
  mOutcome.Text := '';
  mConsole.Text := '';
  FEngine.OnDebug := DoDebug;
  try
    FExpression := FEngine.parseV(mSource.Text);
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
    if FResource = nil then
      Ftypes := FEngine.check(nil, '', FContext.FhirType, mSource.Text, FExpression, false)
    else
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
    FOnSetSetting(settingDebuggerSourceHeight, Panel2.Height);
end;

procedure TFHIRPathDebuggerForm.Splitter2Moved(Sender: TObject);
begin
  if not FLayoutInProgress then
    FOnSetSetting(settingDebuggerBreaksWidth, Panel4.Width);
end;

procedure UpdateMarks(expr : TFHIRPathExpressionNodeV; marks : TStringList);
begin
  if expr = nil then
    exit;
  expr.visitAll(nil, procedure (context : pointer; item : TFHIRPathExpressionNodeV)
    begin
      if marks.IndexOf(inttostr(item.uniqueId)) > -1 then
        item.tag := 1
      else
        item.tag := 0;
    end);

end;

procedure GetMarks(expr : TFHIRPathExpressionNodeV; marks : TStringList);
begin
  if expr = nil then
    exit;
  expr.visitAll(nil, procedure (context : pointer; item : TFHIRPathExpressionNodeV)
    begin
      if item.tag = 1 then
        marks.Add(inttostr(item.uniqueId));
    end);
end;

procedure TFHIRPathDebuggerForm.ApplyMarks;
var
  ts : TStringList;
  i : integer;
begin
  ts := TStringList.Create;
  try
    ts.CommaText := FOnGetSettingStr(settingDebuggerBreaks);
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
    FOnSetSettingStr(settingDebuggerBreaks, ts.CommaText);
  finally
    ts.free;
  end;
end;

procedure TFHIRPathDebuggerForm.btnStopClick(Sender: TObject);
begin
  FMode := emAbort;
end;

procedure TFHIRPathDebuggerForm.Compose(memo: TMemo; obj: TFHIRSelectionList; name, def: String);
var
  ol : TFHIRObjectList;
begin
  if obj = nil then
    memo.text := def
  else
  begin
    ol := obj.asValues;
    try
      compose(memo, ol, name, def);
    finally
      ol.Free;
    end;
  end;
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
  OnSetSettingStr(settingDebuggerBreaks, '');
  vtExpressions.InvalidateChildren(vtExpressions.RootNode.FirstChild, true);
end;

procedure TFHIRPathDebuggerForm.Compose(memo: TMemo; obj: TFHIRObjectList; name, def: String);
var
  comp : TFHIRComposer;
begin
  if (obj = nil) then
    memo.text := def
  else
  begin
    comp := FServices.Factory.makeComposer(FServices.link, FFormat, FServices.lang, OutputStylePretty);
    try
      memo.Text := comp.Compose(name, obj)
    finally
      comp.Free;
    end;
  end;
end;

procedure TFHIRPathDebuggerForm.Compose(memo: TMemo; obj: TFHIRObject; name : String);
var
  comp : TFHIRComposer;
  mgr : TFHIRBaseMMManager;
begin
  if obj = nil then
    memo.text := ''
  else if obj.isMetaDataBased then
  begin
    mgr := FFactory.makeElementModelManager();
    try
      memo.Text := mgr.composeV(FServices, obj, FFormat, OutputStylePretty);
    finally
      mgr.Free;
    end;
  end
  else
  begin
    comp := FFactory.makeComposer(FServices.Link, FFormat, FServices.lang, OutputStylePretty);
    try
      if obj is TFHIRResourceV then
        memo.Text := comp.Compose(TFHIRResourceV(obj))
      else
        memo.Text := comp.Compose(name, obj)
    finally
      comp.Free;
    end;
  end;
end;

procedure TFHIRPathDebuggerForm.ShowPackage;
begin
  compose(mResource, FResource, 'Resource');
  compose(mContext, FContext, 'Context');
  if (Fpackage.IsOperation) then
  begin
    TabSheet3.Caption := 'Left';
    TabSheet4.Caption := 'Right';
    Compose(mInput, Fpackage.input1, 'Left', 'error?');
    Compose(mInput2, Fpackage.input2, 'Right', 'not evaluated (short circuit)')
  end
  else
  begin
    TabSheet3.Caption := 'Focus';
    TabSheet4.Caption := 'N/A';
    Compose(mInput, Fpackage.input1, 'Focus', 'error?');
    Compose(mInput2, Fpackage.input2, 'Input2', 'N/A');
  end;
  Compose(mOutcome, Fpackage.outcome, 'Outcome', 'error?');
end;

procedure TFHIRPathDebuggerForm.DoDebug(source : TFHIRPathEngineV; package : TFHIRPathDebugPackage);
var
  id : string;
  tc : Int64;
  l : integer;
  s, e : integer;
begin
  QueryPerformanceCounter(tc);
  if WantStop(package) then
  begin
    FPackage := package;
    showPackage;
    s := SendMessage(mSource.Handle, EM_LINEINDEX, package.SourceStart.line-1, 0);
    s := s + package.SourceStart.col-1;
    e := SendMessage(mSource.Handle, EM_LINEINDEX, package.SourceEnd.line-1, 0);
    e := e + package.SourceEnd.col-1;
    mSource.SelStart := s;
    mSource.SelLength := e - mSource.SelStart;
    FCurrent := package.Expression;
    FCurrentIsOp := package.IsOperation;
    id := getId(package.Expression, FCurrentIsOp);
    if FDone.IndexOf(id) = -1 then
      FDone.Add(id);
    vtExpressions.Invalidate;
    l := trunc(((tc-FStartLast)/FFreq)*1000000);
    if FCurrentIsOp then
      Log(FCurrent.nodeOpName+' ('+inttostr(l)+'N<s)')
    else
      Log(FCurrent.summary()+' ('+inttostr(l)+'N<s)');

    FMode := emWaiting;
    while FMode = emWaiting do
      Application.ProcessMessages;
    FPackage := nil;
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

procedure TFHIRPathDebuggerForm.vtExpressionsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  p : PTreeDataPointer;
begin
  p := vtExpressions.GetNodeData(node);
  if (p.expr = FCurrent) and (p.isOp = FCurrentIsOp) then
    ImageIndex := 2
  else if FDone.indexof(getid(p.expr, p.isOp)) > -1 then
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
  else if (p.isOp) then
    CellText := p.expr.nodeOpName
  else
    CellText := p.expr.NodeName;
end;

procedure TFHIRPathDebuggerForm.vtExpressionsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  p : PTreeDataPointer;
begin
  p := vtExpressions.GetNodeData(node);
  if p.isOp then
    ChildCount := 0
  else
    ChildCount := p.expr.nodeChildCount;
end;

procedure TFHIRPathDebuggerForm.vtExpressionsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p, pp : PTreeDataPointer;
  pe : TFHIRPathExpressionNodeV;
  offset : integer;
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
    p.expr := pe.nodeGetChild(node.Index, offset);
    case node.Index - offset of
      0: begin
         p.expr := pe;
         p.isOp := true;
         end;
      1: p.expr := pe.nodeOpNext;
    else if node.index - offset > 0 then
      raise EFHIRTodo.create('TFHIRPathDebuggerForm.vtExpressionsInitNode');
    end;
  end;
  if p.expr.tag = 1 then
    node.CheckState := csCheckedNormal;
  if not p.isOp and (p.expr.nodeChildCount > 0) then
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
  else
    result := false;
  end;
end;

end.

