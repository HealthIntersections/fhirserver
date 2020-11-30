unit FHIR.Transformer.IDE;

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

{$I fhir.inc}

{
standard function keys
f2: stop
F3: find
F4: close
F5: run
f6: run no debug
F7: step in
F8: step over
F9: step ot
f10: validate
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, IniFiles, ClipBrd, IOUtils, ActiveX,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.ImageList,
  Vcl.ImgList, VirtualTrees, Vcl.Buttons, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ToolWin,
  JclDebug,
  ScintEdit, ScintInt, ScintFormats,
  MarkdownCommonMark,
  fsl_base, fsl_utilities, fsl_stream, fsl_comparisons, fsl_xml, fsl_shell, fsl_threads,
  FHIR.Ui.ListSelector,
  FHIR.Javascript,
  FHIR.Npm.Manager,
  fhir_objects, fhir_parser, fhir_pathengine, FHIR.Base.PathDebugger,
  v2_message,
  fhir4_context, fhir4_resources, fhir4_maputils, fhir4_elementmodel, fhir4_json, FHIR.R4.XML, fhir4_factory, fhir4_pathengine, fhir4_utilities,
  FHIR.Transformer.Workspace, FHIR.Transformer.Utilities, FHIR.Transformer.Engine, FHIR.Transformer.Context, FHIR.Transformer.Editor,
  FHIR.Ui.WorkerTask, FHIR.Transformer.FileChangedDlg, FHIR.Transformer.ExceptionHandlerDlg,
  Vcl.OleCtrls, SHDocVw;

const
  TEMPLATE_V2 = 'MSH|^~\&|GHH LAB|ELAB-3|GHH OE|BLDG4|200202150930||ORU^R01|CNTRL-3456|P|2.4'+#13#10+
                'PID|||555-44-4444||EVERYWOMAN^EVE^E^^^^L|JONES|19620320|F|||153 FERNWOOD DR.^^STATESVILLE^OH^35292||(206)3345232|(206)752-121||||AC555444444||67-A4335^OH^20030520'+#13#10+
                'OBR|1|845439^GHH OE|1045813^GHH LAB|15545^GLUCOSE|||200202150730|||||||||555-55-5555^PRIMARY^PATRICIA P^^^^MD^^|||||||||F||||||444-44-4444^HIPPOCRATES^HOWARD H^^^^MD'+#13#10+
                'OBX|1|SN|1554-5^GLUCOSE^POST 12H CFST:MCNC:PT:SER/PLAS:QN||^182|mg/dl|70_105|H|||F'+#13#10;
  TEMPLATE_MAP = 'map "url" = "title"'+#13#10+
                 ''+#13#10+
                 'uses "definitionURL" alias Name1 as source'+#13#10+
                 'uses "definitionURL" alias Name2 as target'+#13#10+
                 ''+#13#10+
                 'group Name(source src : Name1, target tgt : Name2) {'+#13#10+
                 '  src.x -> tgt.x;'+#13#10+
                 '}'+#13#10;

  TEMPLATE_JSON = '{'+#13#10+
                  '  "resourceType" : "Patient"'+#13#10+
                  '}'+#13#10;
  TEMPLATE_XML = '<Patient xmlns="http://hl7.org/fhir">'+#13#10+
                 '</Patient>'+#13#10;
  TEMPLATE_JS =  'function name(params) {'+#13#10+
                 '  // statement;' +#13#10+
                 '}'+#13#10;
  TEMPLATE_LIQUID = '{% comment %} See http://wiki.hl7.org/index.php?title=FHIR_Liquid_Profile for doco {% endcomment %}';

  TEMPLATE_CDA = 'todo';
  TEMPLATE_MARKDOWN = 'Markdown with Liquid support';

  spCaretPos = 0;
  spPath = 1;
  spMode = 2;
  spStatus = 3;

const
  DBG_STOPPED = 0;
  DBG_EXECUTE = 1;
  DBG_STEP_OVER = 2;
  DBG_STEP_OUT = 3;
  DBG_STEP_INTO = 4;
  // values 10 or above will abort debugging run
  DBG_STOP = 10;
  DBG_CLOSING = 11;

type
  TTransformerForm = class(TForm)
    Panel1: TPanel;
    pnlWorkspace: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    pnlDebug: TPanel;
    pgTabs: TPageControl;
    Splitter2: TSplitter;
    TabSheet1: TTabSheet;
    Panel6: TPanel;
    edtWorkspace: TEdit;
    btnAddContent: TBitBtn;
    Panel7: TPanel;
    vtWorkspace: TVirtualStringTree;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    mnuNewMessage: TMenuItem;
    mnuNewResourceJson: TMenuItem;
    mnuNewResourceXml: TMenuItem;
    mnuNewScript: TMenuItem;
    mnuNewMap: TMenuItem;
    mnuNewTemplate: TMenuItem;
    mnuNewDocument: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuDuplicate: TMenuItem;
    mnuRename: TMenuItem;
    Print1: TMenuItem;
    N1: TMenuItem;
    mnuDrop: TMenuItem;
    mnuWorkspace: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    mnuEdit: TMenuItem;
    mnuRedo: TMenuItem;
    mnuUndo: TMenuItem;
    N3: TMenuItem;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    N4: TMenuItem;
    mnuSearch: TMenuItem;
    pmAddAsset: TPopupMenu;
    pmAddExisting: TMenuItem;
    NweV2Message1: TMenuItem;
    NewCDADocument1: TMenuItem;
    NewResourceJSON1: TMenuItem;
    newResourceXML1: TMenuItem;
    NewJavascript1: TMenuItem;
    NewMap1: TMenuItem;
    NewTemplate1: TMenuItem;
    pmWorkspace: TPopupMenu;
    pmNewItem: TMenuItem;
    pmDuplicate: TMenuItem;
    pmRename: TMenuItem;
    pmDrop: TMenuItem;
    sdNew: TSaveDialog;
    odImport: TOpenDialog;
    fd: TFileOpenDialog;
    Settings1: TMenuItem;
    mnuFont: TMenuItem;
    fontDlg: TFontDialog;
    sdText: TSaveDialog;
    ToolBar1: TToolBar;
    tbHome: TToolButton;
    pmTabs: TPopupMenu;
    pmCloseThis: TMenuItem;
    pmCloseOthers: TMenuItem;
    pmEditor: TPopupMenu;
    pmnuCut: TMenuItem;
    pmnuCopy: TMenuItem;
    pmnuPaste: TMenuItem;
    N5: TMenuItem;
    Search2: TMenuItem;
    Replace1: TMenuItem;
    N6: TMenuItem;
    mnuReplace: TMenuItem;
    ReplaceDialog: TReplaceDialog;
    FindDialog: TFindDialog;
    mnuFindNext: TMenuItem;
    N7: TMenuItem;
    mnuGoto: TMenuItem;
    FindNext2: TMenuItem;
    mnuClose: TMenuItem;
    Execute1: TMenuItem;
    mnuExecute: TMenuItem;
    btnExecute: TBitBtn;
    mnuPackageManager: TMenuItem;
    pgDebug: TPageControl;
    tbConsole: TTabSheet;
    Panel5: TPanel;
    btnConsoleClear: TButton;
    btnConsoleSave: TButton;
    btnConsoleCopy: TButton;
    mConsole: TMemo;
    tbExecute: TToolButton;
    ools1: TMenuItem;
    mnuCompare: TMenuItem;
    ToolButton3: TToolButton;
    tbCompare: TToolButton;
    ToolButton5: TToolButton;
    tbOpen: TToolButton;
    tbNew: TToolButton;
    tbSave: TToolButton;
    tbPrint: TToolButton;
    tbUndo: TToolButton;
    tbCut: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    tbFind: TToolButton;
    Timer1: TTimer;
    ToolButton1: TToolButton;
    tbPackageManager: TToolButton;
    N8: TMenuItem;
    mnuPretty: TMenuItem;
    mnuDense: TMenuItem;
    mnuEOL: TMenuItem;
    mnuRemoveEmptyLines: TMenuItem;
    mnuUnixEOL: TMenuItem;
    mnuWindowsEOL: TMenuItem;
    ToolButton2: TToolButton;
    mnuCompile: TMenuItem;
    tbCompile: TToolButton;
    pnlStatus: TStatusBar;
    mnuSaveAll: TMenuItem;
    tbSaveAll: TToolButton;
    mnuChooseWorkspace: TMenuItem;
    btnMaximise: TBitBtn;
    btnNormalSize: TBitBtn;
    tbBreakpoints: TTabSheet;
    tbVariables: TTabSheet;
    TabSheet3: TTabSheet;
    Panel2: TPanel;
    Panel4: TPanel;
    Label4: TLabel;
    edtFHIRPath: TEdit;
    btnPathGo: TBitBtn;
    btnPathDebug: TBitBtn;
    Label5: TLabel;
    ListBox1: TListBox;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Splitter3: TSplitter;
    vtVars: TVirtualStringTree;
    Panel11: TPanel;
    Panel14: TPanel;
    vtVarDetails: TVirtualStringTree;
    lbFHIRPathOutcomes: TListBox;
    mnuOptions: TMenuItem;
    N9: TMenuItem;
    mnuStepInto: TMenuItem;
    mnuStepOver: TMenuItem;
    mnuStepOut: TMenuItem;
    mnuEndDebugging: TMenuItem;
    mnuRunNoDebug: TMenuItem;
    N10: TMenuItem;
    tbStepInto: TToolButton;
    tbStepOver: TToolButton;
    tbStepOut: TToolButton;
    tbStop: TToolButton;
    btnRunNoDebug: TBitBtn;
    vtCallStack: TVirtualStringTree;
    Splitter4: TSplitter;
    Panel15: TPanel;
    Help1: TMenuItem;
    N11: TMenuItem;
    About1: TMenuItem;
    estException1: TMenuItem;
    lblExecutionError: TLabel;
    N12: TMenuItem;
    mnuRecompileAll: TMenuItem;
    mnuNewMarkdown: TMenuItem;
    NewMarkdown1: TMenuItem;
    N13: TMenuItem;
    mnuClipboard: TMenuItem;
    mnuCopyFileName: TMenuItem;
    mnuCopyDirectory: TMenuItem;
    mnuCopyContents: TMenuItem;
    Panel17: TPanel;
    vtConfig: TVirtualStringTree;
    Label1: TLabel;
    btnAddConfig: TBitBtn;
    btnEditConfig: TBitBtn;
    btnDeleteConfig: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddContentClick(Sender: TObject);
    procedure mnuNewMessageClick(Sender: TObject);
    procedure mnuNewDocumentClick(Sender: TObject);
    procedure mnuNewResourceJsonClick(Sender: TObject);
    procedure mnuNewResourceXmlClick(Sender: TObject);
    procedure mnuNewScriptClick(Sender: TObject);
    procedure mnuNewMapClick(Sender: TObject);
    procedure mnuNewTemplateClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure vtWorkspaceGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtWorkspaceInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtWorkspaceInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vtWorkspaceAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtWorkspaceRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtWorkspaceGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
    procedure edtWorkspaceChange(Sender: TObject);
    procedure pmWorkspacePopup(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure pmNewItemClick(Sender: TObject);
    procedure mnuDropClick(Sender: TObject);
    procedure mnuDuplicateClick(Sender: TObject);
    procedure mnuRenameClick(Sender: TObject);
    procedure vtWorkspaceDblClick(Sender: TObject);
    procedure mnuFontClick(Sender: TObject);
    procedure btnConsoleCopyClick(Sender: TObject);
    procedure btnConsoleSaveClick(Sender: TObject);
    procedure btnConsoleClearClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tbHomeClick(Sender: TObject);
    procedure pmCloseThisClick(Sender: TObject);
    procedure pmCloseOthersClick(Sender: TObject);
    procedure pmTabsPopup(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuSearchClick(Sender: TObject);
    procedure mnuReplaceClick(Sender: TObject);
    procedure mnuFindNextClick(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure mnuGotoClick(Sender: TObject);
    procedure mnuPackageManagerClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure Rewrite1Click(Sender: TObject);
    procedure mnuCompareClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure pgTabsChange(Sender: TObject);
    procedure mnuPrettyClick(Sender: TObject);
    procedure mnuDenseClick(Sender: TObject);
    procedure mnuWindowsEOLClick(Sender: TObject);
    procedure mnuUnixEOLClick(Sender: TObject);
    procedure mnuRemoveEmptyLinesClick(Sender: TObject);
    procedure mnuCompileClick(Sender: TObject);
    procedure mnuSaveAllClick(Sender: TObject);
    procedure mnuChooseWorkspaceClick(Sender: TObject);
    procedure btnMaximiseClick(Sender: TObject);
    procedure btnNormalSizeClick(Sender: TObject);
    procedure btnPathGoClick(Sender: TObject);
    procedure mnuOptionsClick(Sender: TObject);
    procedure lbFHIRPathOutcomesClick(Sender: TObject);
    procedure btnPathDebugClick(Sender: TObject);
    procedure mnuRunNoDebugClick(Sender: TObject);
    procedure tbStepIntoClick(Sender: TObject);
    procedure tbStepOverClick(Sender: TObject);
    procedure tbStepOutClick(Sender: TObject);
    procedure tbStopClick(Sender: TObject);
    procedure vtCallStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtCallStackInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtCallStackClick(Sender: TObject);
    procedure vtCallStackAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtCallStackRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtCallStackDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vtVarsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtVarsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vtVarsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtVarsRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtVarsClick(Sender: TObject);
    procedure vtVarDetailsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtVarDetailsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vtVarDetailsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtVarDetailsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtVarDetailsColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure pmEditorPopup(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure estException1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuRecompileAllClick(Sender: TObject);
    procedure NewMarkdown1Click(Sender: TObject);
    procedure mnuCopyFileNameClick(Sender: TObject);
    procedure mnuCopyDirectoryClick(Sender: TObject);
    procedure mnuCopyContentsClick(Sender: TObject);
    procedure btnAddConfigClick(Sender: TObject);
    procedure vtConfigGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtConfigInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtConfigAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtConfigRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnEditConfigClick(Sender: TObject);
    procedure btnDeleteConfigClick(Sender: TObject);
    procedure PackageManager1Click(Sender: TObject);
  private
    FIni : TIniFile;
    FWorkspace : TWorkspace;
    FCache : TResourceMemoryCache;
    FEngine : TTransformEngine;

    FEditor : TEditorInformation;
    FLoading : boolean;
    FInitialised : boolean;
    FProgress : TWorkingForm;

    FWorkspaceWidth : integer;
    FDebugHeight : integer;

    FLastFindOptions: TFindOptions;
    FLastFindText: String;
    FLastReplaceText: String;

    FSelected : PVirtualNode;
    FCallStackSelected : PVirtualNode;
    FVarsSelected : PVirtualNode;
    FConfigSelected : PVirtualNode;

    FPathSelection : TFslList<TPathSelection>;
    FRunningState : boolean;
    FWantClose : boolean;
    FDebugInfo : TTransformEngineDebugContext;
    FStepEditor : TEditorInformation;
    FStepOutcome : integer;
    FVariables : TFslList<TTransformEngineExecutionVariable>;
    FVariable : TTransformEngineExecutionVariable;

    function DoSave(cmdCode, cmdTitle : String) : boolean;
    function nodeCaption(i : integer) : String;
    procedure LoadWorkspace(proj: TWorkspace);
    procedure makeNewFile(title, ext, template : String; fmt : TTransformerFormat; category : TFslList<TWorkspaceFile>);

    function findWorkspaceFile(f : TWorkspaceFile) : TEditorInformation;
    function openWorkspaceFile(f : TWorkspaceFile) : TEditorInformation;
    procedure saveWorkspaceFile(editor : TEditorInformation);
    procedure closeWorkspaceFile(editor : TEditorInformation; checkSave : boolean);
    procedure renameWorkspaceFile(editor : TEditorInformation; fn : String);
    procedure updateWorkspaceFile(editor : TEditorInformation; src : String);
    function editorForTab(tab : TTabSheet) : TEditorInformation;
    function editorForFile(f : TWorkspaceFile) : TEditorInformation;
    function describeEditorPath : String;
    function anyFilesDirty : boolean;

    procedure FindNext(editor : TScintEdit);
    procedure InitializeFindText(Dlg: TFindDialog; editor : TScintEdit);

    procedure mnuPastWorkspaceClick(Sender: TObject);
    procedure memoChange(Sender: TObject; const Info: TScintEditChangeInfo);
    procedure memoStatusChange(Sender: TObject);
    procedure MemoMarginClick(Sender: TObject; MarginNumber: Integer; Line: Integer);

    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomReset;

    procedure status(color : TColor; msg : String);
    procedure eventLog(sender : TTransformEngine; message : String);
    procedure cacheLog(sender : TObject; pct : integer; done : boolean; desc : String);
    procedure setDebugStatus(enabled : boolean);
    procedure checkExecutionState;

    procedure SetStepLine(editor : TEditorInformation; ALine: Integer);
    procedure MemoLinesDeleted(FirstLine, Count, FirstAffectedLine: Integer);
    procedure MemoLinesInserted(FirstLine, Count: integer);
    procedure ToggleBreakPoint(Line: Integer);

    function canExecute : boolean;
    procedure runFHIRPath(debug: boolean);
    function GetFPDebuggerSetting(name : TFHIRPathDebuggerFormSetting) : Integer;
    procedure SetFPDebuggerSetting(name : TFHIRPathDebuggerFormSetting; value : Integer);
    function GetFPDebuggerSettingStr(name : TFHIRPathDebuggerFormSetting) : String;
    procedure SetFPDebuggerSettingStr(name : TFHIRPathDebuggerFormSetting; value : String);

    procedure startRunning;
    procedure stopRunning;
    procedure ShowCallStack;
    procedure ClearCallStack;
    procedure ShowVars(vars : TFslList<TTransformEngineExecutionVariable>);
    procedure ClearVars;
    procedure ShowVariable(variable : TTransformEngineExecutionVariable);
    procedure ClearVariable;
    procedure DebugTransform(sender : TTransformEngine; info : TTransformEngineDebugContext);

    procedure HandleException(Sender: TObject; E: Exception);
    Procedure InitExceptions;
    procedure CloseExceptions;

    procedure InitialiseExec;
    procedure ExecutorUpdateFile(sender : TTransformEngine; f : TWorkspaceFile);
    procedure ExecutorStateUpdate(sender : TTransformEngine);
    procedure ExecutorStatusMessage(sender : TTransformEngine; color : TColor; msg: String; beep : UInt);
    function ExecutorOpenFile(sender : TTransformEngine; f : TWorkspaceFile) : TEditorInformation;
    procedure SaveInputs;
    function loadEvent: TWorkspaceExecConfig;
  public
  end;

var
  TransformerForm: TTransformerForm;

implementation

{$R *.dfm}

uses FHIR.Transformer.SettingsDialog, FHIR.Transformer.MarkdownPreview,
  FHIR.Transformer.ExecConfig;


function makeXmlDense(src : String) : String;
var
  xml : TMXmlDocument;
begin
  xml := TMXmlParser.parse(src, [xpDropWhitespace]);
  try
    result := xml.ToXml(false, true);
  finally
    xml.free;
  end;
end;

function makeXmlPretty(src : String) : String;
var
  xml : TMXmlDocument;
begin
  xml := TMXmlParser.parse(src, [xpDropWhitespace]);
  try
    result := xml.ToXml(true, true);
  finally
    xml.free;
  end;
end;

{ TTransformerForm }

function TTransformerForm.anyFilesDirty: boolean;
var
  i : integer;
begin
  result := false;
  for i := 1 to pgTabs.PageCount - 1 do
    if editorForTab(pgTabs.Pages[i]).IsDirty then
      exit(true);
end;

procedure TTransformerForm.btnAddContentClick(Sender: TObject);
var
  pnt: TPoint;
begin
  pmAddExisting.Visible := true;
  if GetCursorPos(pnt) then
    pmAddAsset.Popup(pnt.X, pnt.Y);
end;

procedure TTransformerForm.btnConsoleClearClick(Sender: TObject);
begin
  mConsole.text := '';
end;

procedure TTransformerForm.btnConsoleCopyClick(Sender: TObject);
begin
  Clipboard.AsText := mConsole.Text;
end;

procedure TTransformerForm.btnConsoleSaveClick(Sender: TObject);
begin
  if sdText.execute then
    mConsole.lines.SaveToFile(sdText.FileName);
end;

procedure TTransformerForm.btnDeleteConfigClick(Sender: TObject);
var
  p : PTreeDataPointer;
  ec : TWorkspaceExecConfig;
begin
  if FEngine.Running then
    exit;

  if FConfigSelected <> nil then
  begin
    p := vtConfig.GetNodeData(FConfigSelected);
    ec := p.obj as TWorkspaceExecConfig;
    if MessageDlg('Delete Configuration '+ec.summary+'?', mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      FWorkspace.configurations.Remove(ec);
      FWorkspace.save;
      vtConfig.RootNodeCount := 0;
      vtConfig.RootNodeCount := FWorkspace.configurations.Count;
      checkExecutionState;
    end;
  end;
end;

procedure TTransformerForm.btnEditConfigClick(Sender: TObject);
var
  p : PTreeDataPointer;
  ec : TWorkspaceExecConfig;
begin
  if FEngine.Running then
    exit;

  ec := nil;
  if FConfigSelected <> nil then
  begin
    p := vtConfig.GetNodeData(FConfigSelected);
    ec := p.obj as TWorkspaceExecConfig;
  end
  else if FWorkspace.configurations.Count = 0 then
    btnAddConfigClick(self)
  else
    ec := FWorkspace.configurations[0];
  if ec <> nil then
  begin
    TransformerExecConfigForm := TTransformerExecConfigForm.create(self);
    try
      TransformerExecConfigForm.Workspace := FWorkspace.link;
      TransformerExecConfigForm.Config := ec.Link;
      if TransformerExecConfigForm.ShowModal = mrOk then
      begin
        FWorkspace.save;
        if FConfigSelected <> nil then
          vtConfig.InvalidateNode(FConfigSelected)
        else
        begin
          vtConfig.RootNodeCount := 0;
          vtConfig.RootNodeCount := FWorkspace.configurations.Count;
        end;
        checkExecutionState;
      end;
    finally
      TransformerExecConfigForm.Free;
    end;
  end;
end;

procedure TTransformerForm.btnExecuteClick(Sender: TObject);
var
  ev : TWorkspaceExecConfig;
begin
  if FDebugInfo <> nil then
    FStepOutcome := DBG_EXECUTE
  else
  begin
    ev := loadEvent;
    try
      FEngine.debug(ev);
    finally
      ev.Free;
    end;
  end;
end;

procedure TTransformerForm.btnMaximiseClick(Sender: TObject);
begin
  FWorkspaceWidth := pnlWorkspace.Width;
  FDebugHeight := pnlDebug.Height;
  pnlWorkspace.Width := 0;
  pnlDebug.Height := 0;
  btnMaximise.Visible := false;
  btnNormalSize.Visible := true;
end;

procedure TTransformerForm.btnNormalSizeClick(Sender: TObject);
begin
  pnlWorkspace.Width := FWorkspaceWidth;
  pnlDebug.Height := FDebugHeight;
  btnMaximise.Visible := true;
  btnNormalSize.Visible := false;
end;

function tail(s : String) : String;
begin
  if s.Contains('/') then
    result := s.Substring(s.LastIndexOf('/')+1)
  else
    result := s;
end;

procedure TTransformerForm.btnPathDebugClick(Sender: TObject);
begin
  DoSave('debug-fp', 'Debug');
  runFHIRPath(true);
end;

procedure TTransformerForm.btnPathGoClick(Sender: TObject);
begin
  DoSave('execute-fp', 'Execute');
  runFHIRPath(false);
end;

procedure TTransformerForm.btnAddConfigClick(Sender: TObject);
var
  ec : TWorkspaceExecConfig;
begin
  if FEngine.Running then
    exit;

  ec := TWorkspaceExecConfig.create;
  try
    TransformerExecConfigForm := TTransformerExecConfigForm.create(self);
    try
      TransformerExecConfigForm.Workspace := FWorkspace.link;
      TransformerExecConfigForm.Config := ec.Link;
      if TransformerExecConfigForm.ShowModal = mrOk then
      begin
        FWorkspace.configurations.Add(ec.link);
        if FWorkspace.scripts.Count > 0 then
          ec.script := FWorkspace.scripts[0].filename;
        if FWorkspace.documents.Count > 0 then
          ec.focus := FWorkspace.documents[0].filename
        else if FWorkspace.messages.Count > 0 then
          ec.focus := FWorkspace.messages[0].filename;
        FWorkspace.save;
        vtConfig.RootNodeCount := FWorkspace.configurations.Count;
        vtConfig.Refresh;
        checkExecutionState;
      end;
    finally
      TransformerExecConfigForm.Free;
    end;
  finally
    ec.Free;
  end;
end;

procedure TTransformerForm.runFHIRPath(debug : boolean);
var
  context : TFHIRTransformerContext;
  engine : TFHIRPathEngine;
  b : TFHIRObject;
  ol : TFHIRSelectionList;
  o : TFHIRSelection;
  s : String;
  ps : TPathSelection;
  types : TFHIRTypeDetailsV;
  fmt : TFHIRFormat;
begin
  context := TFHIRTransformerContext.Create(TFHIRFactoryR4.create);
  try
    context.loadFromCache(FCache);
    engine := TFHIRPathEngine.Create(context.Link, nil); // todo: do we need UCUM?
    try
      engine.registerExtension(TV2FHIRPathExtensions.create);
      b := FEditor.Parse(context);
      try
        lbFHIRPathOutcomes.Items.Clear;
        FPathSelection.Clear;
        try
          if debug then
          begin
            if isXml(FEditor.memo.RawText) then
              fmt := ffXml
            else
              fmt := ffJson;
            RunPathDebugger(self, context, engine, GetFPDebuggerSetting, setFPDebuggerSetting, getFPDebuggerSettingStr, setFPDebuggerSettingStr,
              context.Factory, nil, b, edtFHIRPath.text, fmt, types, ol)
          end
          else
            ol := engine.evaluate(nil, b, edtFHIRPath.text);
          try
            if assigned(ol) then
            begin
              for o in ol do
              begin

                s := tail(o.value.fhirType);
                if o.name <> '' then
                  s := s + ' ('+o.name +')';
                if o.value = nil then
                begin
                  s := s+' = null';
                  ps := TPathSelection.create(s, -1,-1,-1,-1);
                end
                else
                begin
                  if o.value.isPrimitive then
                    s := s +' = '+o.value.primitiveValue;
                  ps := TPathSelection.create(s, o.value.LocationStart.line-1, o.value.LocationStart.col-1, o.value.LocationEnd.line-1, o.value.LocationEnd.col-1);
                end;
                FPathSelection.Add(ps);
                lbFHIRPathOutcomes.Items.AddObject(s, ps);
              end;
            end;
          finally
            ol.free;
          end;
        except
          on e : Exception do
          begin
            MessageDlg(e.Message, mtError, [mbok], 0);
            exit;
          end;
        end;
      finally
        b.free;
      end;
    finally
      engine.Free;
    end;
  finally
    context.free;
  end;
end;

procedure TTransformerForm.mnuRunNoDebugClick(Sender: TObject);
var
  ev : TWorkspaceExecConfig;
begin
  ev := loadEvent;
  try
    FEngine.run(ev);
  finally
    ev.Free;
  end;
end;

procedure TTransformerForm.cacheLog(sender : TObject; pct : integer; done : boolean; desc : String);
begin
  if FProgress = nil then
  begin
    FProgress := TWorkingForm.Create(nil);
    FProgress.Show;
  end;
  FProgress.lblStatus.caption := desc;
  FProgress.lblStatus.Update;
  FProgress.pbPercent.Position := pct;
  FProgress.pbPercent.Update;
  if done then
  begin
    FProgress.Free;
    FProgress := nil;
  end;
  Application.ProcessMessages;
end;

function TTransformerForm.canExecute: boolean;
var
  ev : TWorkspaceExecConfig;
  msg : String;
begin
  ev := loadEvent;
  try
    result := false;
    lblExecutionError.Caption := '';
    if ev = nil then
      lblExecutionError.Caption := 'Cannot Execute: No Execution Configuration Defined'
    else if FIni.ReadString('Workspace', 'TerminologyServer', '') = '' then
      lblExecutionError.Caption := 'Cannot Execute: No Terminolgy Server Defined (Tools...Options)'
    else if FEngine.canRun(ev, msg) then
      result := true
    else
      lblExecutionError.Caption := 'Cannot Execute: '+msg;
  finally
    ev.Free;
  end;
end;

procedure TTransformerForm.checkExecutionState;
begin
  btnExecute.enabled := not FEngine.Running and canExecute;
  mnuExecute.Enabled := btnExecute.enabled;
  btnRunNoDebug.enabled := not FEngine.Running and canExecute;
  mnuRunNoDebug.enabled := btnRunNoDebug.enabled;
  btnAddConfig.Enabled := not FEngine.running;
  btnEditConfig.Enabled := not FEngine.running;
  btnDeleteConfig.Enabled := not FEngine.running;
end;

procedure TTransformerForm.pgTabsChange(Sender: TObject);
var
  e : TEditorInformation;
begin
  if pgTabs.ActivePageIndex = 0 then
  begin
    FEditor := nil;
    mnuUndo.Enabled := false;
    tbUndo.Enabled := false;
    mnuRedo.Enabled := false;
    mnuCut.Enabled := false;
    tbCut.Enabled := false;
    pmnuCut.Enabled := false;
    mnuCopy.Enabled := false;
    tbCopy.Enabled := false;
    pmnuCopy.Enabled := false;
    mnuSave.Enabled := false;
    tbSave.Enabled := false;
    mnuCompare.Enabled := false;
    tbCompare.Enabled := false;
    mnuSearch.Enabled := false;
    mnuFindNext.Enabled := false;
    mnuReplace.Enabled := false;
    mnuGoto.Enabled := false;
    mnuClipboard.Enabled := false;
    mnuPretty.Enabled := false;
    mnuDense.Enabled := false;
    mnuEOL.Enabled := false;
    mnuRemoveEmptyLines.Enabled := false;
    mnuCompile.Enabled := false;
    tbCompile.Enabled := false;

    Caption := 'FHIR Transformer IDE';
    pnlStatus.Panels[spMode].Text := '';
    pnlStatus.Panels[spCaretPos].Text := '';
    pnlStatus.Panels[spPath].Text := '';
  end
  else
  begin
    e := editorForTab(pgTabs.ActivePage);
    if e <> FEditor then
    begin
      FEditor := e;
      FEngine.setActiveEditor(FEditor);
      Caption := pgTabs.ActivePage.Caption+'- FHIR Transformer';
    end;
    mnuSearch.Enabled := true;
    mnuFindNext.Enabled := true;
    mnuReplace.Enabled := not FEditor.memo.ReadOnly;
    mnuGoto.Enabled := true;
    mnuClipboard.Enabled := true;
    mnuCompare.Enabled := FEditor.id.format in [fmtV2, fmtCDA, fmtResource, fmtMap];
    tbCompare.Enabled := mnuCompare.Enabled;
    mnuPretty.Enabled := not FEngine.Running and (FEditor.id.format in [fmtCDA, fmtResource]);
    mnuDense.Enabled := not FEngine.Running and (FEditor.id.format in [fmtCDA, fmtResource]);
    mnuEOL.Enabled := not FEngine.Running;
    mnuWindowsEOL.Checked := false;
    mnuUnixEOL.Checked := false;
    mnuCompile.Enabled := true;
    tbCompile.Enabled := true;
    case FEditor.memo.LineEndings of
      sleCRLF: mnuWindowsEOL.Checked := true;
      sleLF: mnuUnixEOL.Checked := true;
    end;
    mnuRemoveEmptyLines.Enabled := not FEngine.Running;
    memoStatusChange(nil);
  end;
  tbSaveAll.Enabled := anyFilesDirty;
  mnuSaveAll.Enabled := tbSaveAll.Enabled;
end;

procedure TTransformerForm.pmCloseOthersClick(Sender: TObject);
var
  dirty : boolean;
  form : TListSelectorForm;
  i : integer;
  f : TEditorInformation;
begin
  dirty := false;
  FWorkspace.save;
  if FIni.ReadBool('Workspace', 'AutoSave', false) then
  begin
    for i := pgTabs.PageCount - 1 downto 1 do
    begin
      f := editorForTab(pgTabs.Pages[i]);
      if (f <> FEditor) then
      begin
        saveWorkspaceFile(f);
        closeWorkspaceFile(f, false);
      end;
    end;
    exit;
  end;

  form := TListSelectorForm.Create(self);
  try
    form.Caption := 'Unsaved Content found. Which files do you want to save?';
    form.okWithNoneSelected := true;
    form.Verb := 'Close';
    for i := 1 to pgTabs.PageCount - 1 do
    begin
      f := editorForTab(pgTabs.Pages[i]);
      if (f <> FEditor) then
      begin
        if (f.isDirty) then
        begin
          dirty := true;
          form.ListBox1.Items.AddObject(f.id.title, f)
        end;
      end;
    end;
    if dirty then
    begin
      if form.ShowModal = mrOk then
      begin
        for i := 0 to form.ListBox1.Items.Count - 1 do
        begin
          if form.ListBox1.Checked[i] then
            saveWorkspaceFile(TEditorInformation(form.ListBox1.items.objects[i]));
        end;
      end;
    end;

    for i := pgTabs.PageCount - 1 downto 1 do
    begin
      f := editorForTab(pgTabs.Pages[i]);
      if (f <> FEditor) then
        closeWorkspaceFile(f, false);
    end;
    if form.cbDontAsk.Checked then
      FIni.WriteBool('Workspace', 'AutoSave', true);
  finally
    form.Free;
  end;
  FWorkspace.save;
end;

procedure TTransformerForm.pmCloseThisClick(Sender: TObject);
var
  e : TEditorInformation;
begin
  FWorkspace.save;
  closeWorkspaceFile(FEditor, true);
  FWorkspace.save;
end;

procedure TTransformerForm.pmEditorPopup(Sender: TObject);
begin
  pmnuCut.Enabled := not FEditor.memo.ReadOnly and (FEditor.memo.SelText <> '');
  pmnuPaste.Enabled := not FEditor.memo.ReadOnly and (Clipboard.HasFormat(CF_TEXT));
end;

procedure TTransformerForm.DebugTransform(sender: TTransformEngine; info: TTransformEngineDebugContext);
begin
  // set up viewing the info
  FDebugInfo := info.Link;
  try
    ShowCallStack;
    ShowVars(info.variables);
    setDebugStatus(true);
    try
      FStepOutcome := DBG_STOPPED;
      while FStepOutcome = DBG_STOPPED do
        Application.ProcessMessages;
      case FStepOutcome of
        DBG_EXECUTE: FDebugInfo.status := tdsRunToBreakpoint;
        DBG_STEP_OVER: FDebugInfo.status := tdsStepOver;
        DBG_STEP_OUT: FDebugInfo.status := tdsStepOut;
        DBG_STEP_INTO: FDebugInfo.status := tdsStepIn;
        DBG_STOP: FDebugInfo.status := tdsRunToBreakpoint;
        DBG_CLOSING: FDebugInfo.status := tdsRunToBreakpoint;
      end;
    finally
      setDebugStatus(false);
      ClearCallStack;
      ClearVars;
      vtVars.RootNodeCount := 0;
      vtVarDetails.RootNodeCount := 0;
      SetStepLine(nil, -1);
    end;
  finally
    FDebugInfo.Free;
    FDebugInfo := nil;
  end;
  if FStepOutcome >= DBG_STOP then
    abort;
end;

function hasChar(s : String; ch : Char; terminators : TSysCharSet) : boolean;
var
  i : integer;
begin
  result := false;
  i := 1;
  while i <= length(s) do
  begin
    if s[i] = ch then
      exit(true);
    if CharInSet(s[i], terminators) then
      exit(false);
    inc(i);
  end;
end;

function describeV2Path(s : String; indent : integer) : String;
var
  f, r, c, sc, i : integer;
begin
  result := copy(s, 1, 3);
  f := 0;
  r := 0;
  c := 0;
  sc := 0;
  i := 1;
  while i <= indent do
  begin
    case s[i] of
      '|' : begin
          inc(f);
          r := 0;
          c := 1;
          sc := 1;
        end;
      '~' : begin
          inc(r);
          c := 1;
          sc := 1;
        end;
      '^' : begin
          inc(c);
          sc := 1;
        end;
      '&' : inc(sc);
    end;
    inc(i);
  end;
  if f > 0 then
  begin
    result := result + '-'+inttostr(f);
    if r > 0 then
      result := result + '['+inttostr(r)+']';
    if (c > 1) or (hasChar(s.Substring(i), '^', ['|', '~'])) or (hasChar(s.Substring(i), '&', ['|', '~', '^'])) then
    begin
      result := result + '-'+inttostr(c);
      if (sc > 1) or (hasChar(s.Substring(i), '&', ['|', '~', '^'])) then
        result := result + '-'+inttostr(sc);
    end;
  end;
end;

function TTransformerForm.describeEditorPath: String;
begin
  case FEditor.id.format of
    fmtV2: result := describeV2Path(FEditor.memo.Lines[FEditor.memo.CaretLine], FEditor.memo.CaretColumn);
    fmtCDA: result := '';
    fmtResource: result := '';
    fmtJS: result := '';
    fmtMap: result := '';
    fmtTemplate: result := '';
    fmtMarkdown: result := '';
  end;
end;

procedure TTransformerForm.SaveInputs;
begin
  FIni.WriteString('debug', 'FHIRPath', edtFHIRPath.Text);
end;

function TTransformerForm.DoSave(cmdCode, cmdTitle : String) : boolean;
var
  dirty : boolean;
  form : TListSelectorForm;
  i : integer;
  e : TEditorInformation;
begin
  dirty := false;
  result := false;
  saveInputs;
  if FIni.ReadBool('Workspace', 'AutoSave-'+cmdCode, false) then
  begin
    mnuSaveAllClick(nil);
    for i := 1 to pgTabs.PageCount - 1 do
    begin
      e := editorForTab(pgTabs.Pages[i]);
      FWorkspace.OpenFile(e.id);
    end;
    exit(true);
  end;

  form := TListSelectorForm.Create(self);
  try
    form.Caption := 'Unsaved Content found. Which files do you want to save?';
    form.okWithNoneSelected := true;
    form.Verb := cmdTitle;
    for i := 1 to pgTabs.PageCount - 1 do
    begin
      e := editorForTab(pgTabs.Pages[i]);
      FWorkspace.OpenFile(e.id);
      if (e.isDirty) then
      begin
        dirty := true;
        form.ListBox1.Items.AddObject(e.id.title, e)
      end;
    end;
    if dirty then
    begin
      if form.ShowModal = mrOk then
      begin
        for i := 0 to form.ListBox1.Items.Count - 1 do
        begin
          if form.ListBox1.Checked[i] then
            saveWorkspaceFile(TEditorInformation(form.ListBox1.items.objects[i]));
        end;
        result := true;
      end;
    end
    else
      result := true;
    if form.cbDontAsk.Checked then
      FIni.WriteBool('Workspace', 'AutoSave-'+cmdCode, true);
  finally
    form.Free;
  end;
  FWorkspace.save;
end;

function TTransformerForm.editorForFile(f: TWorkspaceFile): TEditorInformation;
var
  i : integer;
  e : TEditorInformation;
begin
  for i := 1 to pgTabs.PageCount - 1 do
  begin
    e := editorForTab(pgTabs.Pages[i]);
    if e.id = f then
      exit(e);
  end;
  result := nil;

end;

function TTransformerForm.editorForTab(tab: TTabSheet): TEditorInformation;
begin
 result := (tab.Controls[0] as TScintEdit).context as TEditorInformation;
end;

procedure TTransformerForm.edtWorkspaceChange(Sender: TObject);
begin
  if not FLoading then
  begin
    FWorkspace.name := edtWorkspace.Text;
    FWorkspace.save;
  end;
end;

procedure TTransformerForm.eventLog(sender: TTransformEngine; message: String);
begin
  mConsole.Lines.add(message);
  mConsole.perform(EM_LINESCROLL, 0, mConsole.Lines.Count);
end;

procedure TTransformerForm.estException1Click(Sender: TObject);
begin
  raise Exception.Create('Test Exception');
end;

function TTransformerForm.ExecutorOpenFile(sender: TTransformEngine;f: TWorkspaceFile): TEditorInformation;
begin
  raise Exception.Create('Not implemented yet');
end;

procedure TTransformerForm.ExecutorStateUpdate(sender: TTransformEngine);
begin
  if sender.Running <> FRunningState then
    startRunning
  else
    stopRunning;
end;

procedure TTransformerForm.ExecutorStatusMessage(sender: TTransformEngine; color: TColor; msg: String; beep: UInt);
begin
  status(color, msg);
  if (beep <> 0) then
    MessageBeep(beep);
end;

procedure TTransformerForm.ExecutorUpdateFile(sender: TTransformEngine; f: TWorkspaceFile);
begin
  vtWorkspace.InvalidateNode(PVirtualNode(f.TagObject));
  pgTabsChange(nil);
  checkExecutionState;
end;

procedure TTransformerForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TTransformerForm.File1Click(Sender: TObject);
var
  node : PVirtualNode;
  p : PTreeDataPointer;
  e : TEditorInformation;
begin
  if FSelected = nil then
  begin
    mnuDuplicate.Enabled := false;
    mnuRename.Enabled := false;
    mnuDrop.Enabled := false;
  end
  else
  begin
    node := FSelected;
    p := vtWorkspace.GetNodeData(node);
    if (p.obj is TWorkspaceFile) then
    begin
      mnuDuplicate.Enabled := true;
      mnuRename.Enabled := not FEngine.Running;
      mnuDrop.Enabled := not FEngine.Running;
      node := node.Parent;
    end
    else
    begin
      mnuDuplicate.Enabled := false;
      mnuRename.Enabled := false;
      mnuDrop.Enabled := false;
    end;
  end;
  if assigned(FEditor) then
  begin
    mnuSave.Enabled := FEditor.isDirty;
    mnuClose.Enabled := true;
  end
  else
  begin
    mnuSave.Enabled := false;
    mnuClose.Enabled := false;
  end;
end;

procedure TTransformerForm.FindDialogFind(Sender: TObject);
begin
 { this event handler is shared between FindDialog & ReplaceDialog }
  with Sender as TFindDialog do begin
    { Save a copy of the current text so that InitializeFindText doesn't
      mess up the operation of Edit | Find Next }
    FLastFindOptions := Options;
    FLastFindText := FindText;
  end;
  FindNext(pgTabs.ActivePage.Controls[0] as TScintEdit);
end;

function FindOptionsToSearchOptions(const FindOptions: TFindOptions): TScintFindOptions;
begin
  Result := [];
  if frMatchCase in FindOptions then
    Include(Result, sfoMatchCase);
  if frWholeWord in FindOptions then
    Include(Result, sfoWholeWord);
end;


procedure TTransformerForm.FindNext(editor : TScintEdit);
var
  StartPos, EndPos: Integer;
  Range: TScintRange;
begin
  if frDown in FLastFindOptions then
  begin
    StartPos := editor.Selection.EndPos;
    EndPos := editor.RawTextLength;
  end
  else
  begin
    StartPos := editor.Selection.StartPos;
    EndPos := 0;
  end;
  if editor.FindText(StartPos, EndPos, FLastFindText, FindOptionsToSearchOptions(FLastFindOptions), Range) then
    editor.Selection := Range
  else
    MsgBoxFmt('Cannot find "%s"', [FLastFindText], 'Find', mbInformation, MB_OK);
end;

procedure TTransformerForm.mnuFindNextClick(Sender: TObject);
begin
  if FLastFindText = '' then
    mnuSearchClick(Sender)
  else
    FindNext(pgTabs.ActivePage.Controls[0] as TScintEdit);
end;

procedure TTransformerForm.FormActivate(Sender: TObject);
var
  i : integer;
  e : TEditorInformation;
  s, ts : string;
begin
  for i := 1 to pgTabs.PageCount - 1 do
  begin
    e := editorForTab(pgTabs.Pages[i]);
    s := e.id.actualName;
    if e.FileTime <> TFile.GetLastWriteTimeUtc(s) then
    begin
      FileChangedForm := TFileChangedForm.Create(self);
      try
        FileChangedForm.lblinfo.Caption := 'File '+e.id.title+' has changed on disk - what do you want to do? (if comparing, save the merge to disk, then come back and reload)';
        case FileChangedForm.ShowModal of
          mrOk : e.memo.Lines.LoadFromFile(s);
          mrRetry :
            begin
              ts := Path([SystemTemp, 'loaded-'+ExtractFileName(s)]);
              StringToFile(e.memo.rawText, ts, TEncoding.UTF8);
              ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar('"'+s+'" "'+ts+'"'), true);
            end;
          mrIgnore : ;// nothing
        end;
        e.FileTime := TFile.GetLastWriteTimeUtc(s);
      finally
        FileChangedForm.Free;
      end;
    end;
  end;
end;

procedure TTransformerForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  dirty : boolean;
  form : TListSelectorForm;
  i : integer;
  e : TEditorInformation;
begin
  if FDebugInfo <> nil then
  begin
    if MessageDlg('Stop debugging and Close?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    begin
      CanClose := false;
      exit;
    end;
  end;

  dirty := false;
  FWorkspace.save;
  FWorkspace.ClearOpenFiles;
  if FIni.ReadBool('Workspace', 'AutoSave', false) then
  begin
    mnuSaveAllClick(nil);
    for i := 1 to pgTabs.PageCount - 1 do
    begin
      e := editorForTab(pgTabs.Pages[i]);
      FWorkspace.OpenFile(e.id);
    end;
    if FDebugInfo <> nil then
    begin
      FStepOutcome := DBG_CLOSING;
      FWantClose := true;
    end;
    CanClose := true;
    exit;
  end;

  form := TListSelectorForm.Create(self);
  try
    form.Caption := 'Unsaved Content found. Which files do you want to save?';
    form.okWithNoneSelected := true;
    form.Verb  := 'Close';
    for i := 1 to pgTabs.PageCount - 1 do
    begin
      e := editorForTab(pgTabs.Pages[i]);
      FWorkspace.OpenFile(e.id);
      if (e.isDirty) then
      begin
        dirty := true;
        form.ListBox1.Items.AddObject(e.id.title, e)
      end;
    end;
    if not dirty then
      CanClose := true
    else
      CanClose := form.ShowModal = mrOk;
      for i := 0 to form.ListBox1.Items.Count - 1 do
        if form.ListBox1.Checked[i] then
          saveWorkspaceFile(TEditorInformation(form.ListBox1.items.objects[i]));
    if form.cbDontAsk.Checked then
      FIni.WriteBool('Workspace', 'AutoSave', true);
  finally
    form.Free;
  end;
  if (FDebugInfo <> nil) and CanClose then
  begin
    FStepOutcome := DBG_CLOSING;
    FWantClose := true;
  end;
end;

procedure TTransformerForm.FormCreate(Sender: TObject);
var
  s : String;
  i : integer;
begin
  InitExceptions;
  Application.OnActivate := FormActivate;
  FPathSelection := TFslList<TPathSelection>.create;

  FCache := TResourceMemoryCache.create;
  FCache.Packages := ['hl7.fhir.r4.core#4.0.1', 'hl7.fhir.cda#0.0.1'];
  FCache.ResourceTypes := [{'CodeSystem', 'ValueSet', }'ConceptMap', 'StructureMap', 'StructureDefinition', 'NamingSystem'];
  FCache.OnLog := cacheLog;

  FIni := TIniFile.create(Path([SystemTemp, 'FHIRTransformer.ini']));
  s := FIni.ReadString('Workspace', 'folder', '');
  if FolderExists(s) then
    LoadWorkspace(TWorkspace.Create(s))
  else
  begin
     s := Path([UserFolder, 'FHIR', 'transformer']);
     if (not FolderExists(s)) then
       ForceFolder(s);
     LoadWorkspace(TWorkspace.Create(s));
  end;
  edtFHIRPath.Text := FIni.ReadString('debug', 'FHIRPath', '');
  vtVarDetails.Header.Columns[0].Width := FIni.ReadInteger('debug', 'var-details-col-1', vtVarDetails.Header.Columns[0].Width);
  vtVarDetails.Header.Columns[1].Width := FIni.ReadInteger('debug', 'var-details-col-2', vtVarDetails.Header.Columns[1].Width);
  vtVarDetails.Header.Columns[2].Width := FIni.ReadInteger('debug', 'var-details-col-3', vtVarDetails.Header.Columns[2].Width);
end;

procedure TTransformerForm.FormDestroy(Sender: TObject);
var
  i : integer;
begin
  for i := mnuWorkspace.Count - 1 downto 1 do
    mnuWorkspace.Items[I].Free;
  FIni.Free;
  FCache.Free;
  FEngine.Free;
  FWorkspace.Free;
  FPathSelection.Free;
  CloseExceptions;
end;

procedure TTransformerForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 189) and (Shift = [ssCtrl]) then
    ZoomOut;
  if (key = 187) and (Shift = [ssCtrl]) then
    ZoomIn;
  if (key = 48) and (Shift = [ssCtrl]) then
    ZoomReset;
  if (key = ord('Z')) and (Shift = [ssCtrl]) then
    mnuUndoClick(nil);
end;

procedure TTransformerForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

function TTransformerForm.GetFPDebuggerSetting(name: TFHIRPathDebuggerFormSetting): Integer;
begin
  result := FIni.ReadInteger('Debugger', CODES_TFHIRPathDebuggerFormSetting[name], DEF_INTS_TFHIRPathDebuggerFormSetting[name]);
end;

function TTransformerForm.GetFPDebuggerSettingStr(name: TFHIRPathDebuggerFormSetting): String;
begin
  result := FIni.ReadString('Debugger', CODES_TFHIRPathDebuggerFormSetting[name], DEF_STR_TFHIRPathDebuggerFormSetting[name]);
end;

procedure TTransformerForm.mnuGotoClick(Sender: TObject);
var
  S: String;
  L: Integer;
  editor : TScintEdit;
begin
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  S := IntToStr(editor.CaretLine + 1);
  if InputQuery('Go to Line', 'Line number:', S) then begin
    L := StrToIntDef(S, Low(L));
    if L <> Low(L) then
      editor.CaretLine := L - 1;
  end;
end;

procedure TTransformerForm.InitialiseExec;
begin
  FInitialised := true;
  FEngine.initialise(FCache);
  FEngine.CompileAll;
end;

procedure TTransformerForm.InitializeFindText(Dlg: TFindDialog; editor : TScintEdit);
var
  S: String;
begin
  S := editor.SelText;
  if (S <> '') and (Pos(#13, S) = 0) and (Pos(#10, S) = 0) then
    Dlg.FindText := S
  else
    Dlg.FindText := FLastFindText;
end;

procedure TTransformerForm.lbFHIRPathOutcomesClick(Sender: TObject);
var
  ps : TPathSelection;
  sel : TScintRange;
begin
  ps := TPathSelection(lbFHIRPathOutcomes.Items.Objects[lbFHIRPathOutcomes.ItemIndex]);
  if (ps.LineStart >= 0) then
  begin
    sel.StartPos := FEditor.memo.GetPositionFromLineColumn(ps.lineStart, ps.colStart);
    sel.EndPos := FEditor.memo.GetPositionFromLineColumn(ps.lineEnd, ps.colEnd);
    FEditor.memo.Selection := sel;
    FEditor.memo.ScrollCaretIntoView;
  end;
end;

function TTransformerForm.loadEvent: TWorkspaceExecConfig;
var
  p : PTreeDataPointer;
begin
  if FConfigSelected <> nil then
  begin
    p := vtConfig.GetNodeData(FConfigSelected);
    result := (p.obj as TWorkspaceExecConfig).link;
  end
  else if FWorkspace.configurations.Count > 0 then
    result := FWorkspace.configurations[0].link
  else
    result := nil;
end;

procedure TTransformerForm.LoadWorkspace(proj: TWorkspace);
var
  f : TWorkspaceFile;
  files : TFslList<TWorkspaceFile>;
  close : boolean;
  key : integer;
  st : TStringList;
  s : String;
  mnu : TMenuItem;
  i : integer;
begin
  if FWorkspace <> nil then
  begin
    FormCloseQuery(nil, close);
    if not close then
      exit;
    for i := pgTabs.PageCount - 1 downto 1 do
      closeWorkspaceFile(editorForTab(pgTabs.Pages[i]), false);
  end;
  FEngine.Free;
  FWorkspace.Free;
  FWorkspace := proj;
  FEngine := TTransformEngine.Create(FWorkspace.link);
  FEngine.OnFileUpdate := ExecutorUpdateFile;
  FEngine.OnStateUpdate := ExecutorStateUpdate;
  FEngine.OnStatusMessage := ExecutorStatusMessage;
  FEngine.OnDebug := DebugTransform;
  FEngine.OnOpenFile := ExecutorOpenFile;
  FEngine.OnLog := eventLog;
  FEngine.terminologyServer := FIni.ReadString('Workspace', 'TerminologyServer', '');
  FIni.DeleteKey('Workspaces', proj.folder);
  key := FIni.ReadInteger('Workspace', 'last', 0) + 1;
  FIni.WriteInteger('Workspace', 'last', key);
  FIni.WriteInteger('Workspaces', proj.folder, key);
  FLoading := true;
  try
    FIni.WriteString('Workspace', 'folder', FWorkspace.folder);
    edtWorkspace.Text := proj.name;
    edtWorkspace.Hint := proj.folder;
    vtWorkspace.RootNodeCount := 0;
    vtWorkspace.RootNodeCount := 7;
//    cbxOutcome.ItemIndex := ord(FWorkspace.Outcome);
//    if cbxOutcome.ItemIndex = -1 then
//      cbxOutcome.ItemIndex := 0;
//    cbxOutcomeChange(nil);
//    cbxEventType.ItemIndex := FWorkspace.EventType;
//    cbxEventTypeChange(nil);
    files := FWorkspace.listOpenFiles;
    try
      for f in files do
        openWorkspaceFile(f);
    finally
      files.Free;
    end;
    pgTabsChange(self);
    vtConfig.RootNodeCount := 0;
    vtConfig.RootNodeCount := FWorkspace.configurations.Count;
    vtConfig.Refresh;
  finally
    FLoading := false;
  end;
  for i := mnuWorkspace.Count - 1 downto 1 do
    mnuWorkspace.Items[I].Free;
  st := TStringList.Create;
  try
    FIni.ReadSection('Workspaces', st);
    for i := st.Count -1 downto 0 do // assuming order is maintained in the INi file...
      if st[i] <> FWorkspace.folder then
      begin
        mnu := TMenuItem.Create(mnuWorkspace);
        mnu.Caption := st[i];
        mnu.Tag := FIni.ReadInteger('Workspaces', st[i], 0);
        mnu.OnClick := mnuPastWorkspaceClick;
        mnuWorkspace.add(mnu);
      end;
  finally
    st.Free;
  end;
  if FInitialised then
  begin
    FEngine.initialise(FCache);
    FEngine.CompileAll;
  end;
end;


procedure TTransformerForm.makeNewFile(title, ext, template: String; fmt : TTransformerFormat; category: TFslList<TWorkspaceFile>);
var
  s : String;
  f : TWorkspaceFile;
begin
  sdNew.InitialDir := FWorkspace.folder;
  sdNew.DefaultExt := ext;
  sdNew.FileName := '';
  sdNew.Title := 'New '+title;
  if sdNew.execute then
  begin
    if FWorkspace.includesFile(sdNew.FileName) then
      raise EFslException.Create('The file "'+sdNew.FileName+'" is already in the '+UI_NAME);
    s := makeRelativePath(sdNew.FileName, FWorkspace.folder);
    StringToFile(template, sdNew.FileName, TEncoding.UTF8);
    f := TWorkspaceFile.Create(FWorkspace, s, fmt);
    category.Add(f);
    FWorkspace.allFiles.Add(f.link);
    FWorkspace.save;
    vtWorkspace.RootNodeCount := 0;
    vtWorkspace.RootNodeCount := 7;
    openWorkspaceFile(f);
  end;
end;

procedure TTransformerForm.memoChange(Sender: TObject; const Info: TScintEditChangeInfo);
 procedure LinesInsertedOrDeleted;
  var
    FirstAffectedLine, Line, LinePos: Integer;
  begin
    Line := FEditor.Memo.GetLineFromPosition(Info.StartPos);
    LinePos := FEditor.Memo.GetPositionFromLine(Line);
    FirstAffectedLine := Line;
    { If the deletion/insertion does not start on the first character of Line,
      then we consider the first deleted/inserted line to be the following
      line (Line+1). This way, if you press Del at the end of line 1, the dot
      on line 2 is removed, while line 1's dot stays intact. }
    if Info.StartPos > LinePos then
      Inc(Line);
    if Info.LinesDelta > 0 then
      MemoLinesInserted(Line, Info.LinesDelta)
    else
      MemoLinesDeleted(Line, -Info.LinesDelta, FirstAffectedLine);
  end;
begin
  FEditor.isDirty := true;
  FEditor.id.compileStatus := csWorking;
  ExecutorUpdateFile(FEngine, FEditor.id);
  tbSave.Enabled := FEditor.isDirty;
  mnuSave.Enabled := tbSave.Enabled;
  tbSaveAll.Enabled := anyFilesDirty;
  mnuSaveAll.Enabled := tbSaveAll.Enabled;
  if Info.LinesDelta <> 0 then
    LinesInsertedOrDeleted;
end;

procedure TTransformerForm.memoStatusChange(Sender: TObject);
begin
  if FEditor = nil then
    exit;

  FEditor.id.Row := FEditor.memo.CaretLine;
  if (FEditor.ErrorLine < 0) then
    FEditor.HideError;
  if FEditor.memo.ReadOnly then
    pnlStatus.Panels[spMode].Text := 'LOCK'
  else if FEditor.memo.InsertMode then
    pnlStatus.Panels[spMode].Text := 'INS'
  else
    pnlStatus.Panels[spMode].Text := 'OVR';
  pnlStatus.Panels[spCaretPos].Text := Format('%4d:%4d', [FEditor.memo.CaretLine + 1, FEditor.memo.CaretColumnExpanded + 1]);
  pnlStatus.Panels[spPath].Text := describeEditorPath;

  // undo/redo
  mnuUndo.Enabled := not FEditor.memo.ReadOnly and FEditor.memo.CanUndo;
  tbUndo.Enabled := not FEditor.memo.ReadOnly and FEditor.memo.CanUndo;
  mnuRedo.Enabled := not FEditor.memo.ReadOnly and FEditor.memo.CanUndo;
  // clipbrd
  mnuCut.Enabled := not FEditor.memo.ReadOnly and (FEditor.memo.SelText <> '');
  tbCut.Enabled := mnuCut.Enabled;
  pmnuCut.Enabled := mnuCut.Enabled;
  mnuCopy.Enabled := mnuCut.Enabled;
  tbCopy.Enabled := mnuCut.Enabled;
  pmnuCopy.Enabled := mnuCut.Enabled;

  // misc
  tbSave.Enabled := FEditor.isDirty;
  mnuSave.Enabled := tbSave.Enabled;
  tbSaveAll.Enabled := anyFilesDirty;
  mnuSaveAll.Enabled := tbSaveAll.Enabled;
end;

procedure TTransformerForm.mnuCopyClick(Sender: TObject);
var
  editor : TScintEdit;
begin
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  editor.CopyToClipboard;
end;

procedure TTransformerForm.mnuCopyContentsClick(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := FEditor.memo.RawText;
  Clipboard.Close;
end;

procedure TTransformerForm.mnuCopyDirectoryClick(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := ExtractFileDir(FEditor.id.actualName);
  Clipboard.Close;
end;

procedure TTransformerForm.mnuCopyFileNameClick(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := FEditor.id.actualName;
  Clipboard.Close;
end;

procedure TTransformerForm.mnuCutClick(Sender: TObject);
var
  editor : TScintEdit;
begin
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  editor.CutToClipboard;
end;

procedure TTransformerForm.mnuDenseClick(Sender: TObject);
begin
  case FEditor.id.format of
    fmtV2: raise Exception.Create('Not Supported Yet');
    fmtCDA: FEditor.memo.RawText := makeXmlDense(FEditor.memo.RawText);
    fmtResource: raise Exception.create('Not Supported Yet');
    fmtJS: raise Exception.create('Not Supported Yet');
    fmtMap: raise Exception.create('Not Supported');
    fmtTemplate: raise Exception.create('Not Supported Yet');
    fmtMarkdown: raise Exception.create('Not Supported For Markdown');
  end;
end;

procedure TTransformerForm.mnuDropClick(Sender: TObject);
var
  node : PVirtualNode;
  p : PTreeDataPointer;
  f : TWorkspaceFile;
  list : TFslList<TWorkspaceFile>;
begin
  p := vtWorkspace.GetNodeData(FSelected);
  if (p.obj is TWorkspaceFile) then
  begin
    f := (p.obj as TWorkspaceFile);
    node := FSelected.Parent;
    list := PTreeDataPointer(vtWorkspace.GetNodeData(FSelected.Parent)).obj as TFslList<TWorkspaceFile>;
    case MessageDlg('Drop '+f.filename+'. Do you want to delete the actual file?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], 0) of
      mrCancel : abort;
      mrYes : DeleteFile(f.actualName);
      mrNo : saveWorkspaceFile(editorForFile(f));
    end;
    list.Remove(f);
    closeWorkspaceFile(editorForFile(f), false);
    FWorkspace.save;
    vtWorkspace.RootNodeCount := 0;
    vtWorkspace.RootNodeCount := 7;
  end;
end;

procedure TTransformerForm.mnuDuplicateClick(Sender: TObject);
var
  node : PVirtualNode;
  p : PTreeDataPointer;
  f : TWorkspaceFile;
  list : TFslList<TWorkspaceFile>;
begin
  p := vtWorkspace.GetNodeData(FSelected);
  if (p.obj is TWorkspaceFile) then
  begin
    f := (p.obj as TWorkspaceFile);
    node := FSelected.Parent;
    list := PTreeDataPointer(vtWorkspace.GetNodeData(FSelected.Parent)).obj as TFslList<TWorkspaceFile>;
    makeNewFile(nodeCaption(node.Index), ExtractFileExt(f.filename), f.source, f.format, list);
  end;
end;

procedure TTransformerForm.mnuFontClick(Sender: TObject);
var
  i : integer;
begin
  fontDlg.Font.Assign(mConsole.Font);
  if fontDlg.Execute then
  begin
    mConsole.Font.Assign(fontDlg.Font);
    for i := 1 to pgTabs.PageCount - 1 do
      TScintEdit(pgTabs.Pages[i].Controls[0]).font.Assign(fontDlg.Font);
  end;
end;

procedure TTransformerForm.mnuNewDocumentClick(Sender: TObject);
begin
  makeNewFile('CDA Document', 'json', TEMPLATE_CDA, fmtCDA, FWorkspace.documents);
end;

procedure TTransformerForm.mnuNewMapClick(Sender: TObject);
begin
  makeNewFile('Map', 'json', TEMPLATE_MAP, fmtMap, FWorkspace.maps);
end;

procedure TTransformerForm.mnuNewMessageClick(Sender: TObject);
begin
  makeNewFile('V2 Message', 'json', TEMPLATE_V2, fmtV2, FWorkspace.messages);
end;

procedure TTransformerForm.mnuNewResourceJsonClick(Sender: TObject);
begin
  makeNewFile('Resource', 'json', TEMPLATE_JSON, fmtResource, FWorkspace.resources);
end;

procedure TTransformerForm.mnuNewResourceXmlClick(Sender: TObject);
begin
  makeNewFile('Resource', 'json', TEMPLATE_XML, fmtResource, FWorkspace.resources);
end;

procedure TTransformerForm.mnuNewScriptClick(Sender: TObject);
begin
  makeNewFile('Script', 'json', TEMPLATE_JS, fmtJS, FWorkspace.scripts);
end;

procedure TTransformerForm.mnuNewTemplateClick(Sender: TObject);
begin
  makeNewFile('Template', 'json', TEMPLATE_LIQUID, fmtTemplate, FWorkspace.templates);
end;

procedure TTransformerForm.mnuOpenClick(Sender: TObject);
var
  s : String;
  f : TWorkspaceFile;
  fmt : TTransformerFormat;
  category : TFslList<TWorkspaceFile>;
begin
  odImport.InitialDir := FWorkspace.folder;
  if odImport.Execute then
  begin
    if FWorkspace.includesFile(odImport.FileName) then
      raise EFslException.Create('The file "'+odImport.FileName+'" is already in the '+UI_NAME);

    fmt := detectFormat(odImport.FileName);
    s := makeRelativePath(odImport.FileName, FWorkspace.folder);
    f := TWorkspaceFile.Create(FWorkspace, s, fmt);
    case fmt of
      fmtV2: category := FWorkspace.messages;
      fmtResource: category := FWorkspace.resources;
      fmtJS: category := FWorkspace.scripts;
      fmtMap: category := FWorkspace.maps;
      fmtTemplate: category := FWorkspace.templates;
      fmtMarkdown: category := FWorkspace.markdowns;
      fmtCDA: category := FWorkspace.documents;
    end;
    category.Add(f);
    FWorkspace.allFiles.Add(f.link);
    FWorkspace.save;
    vtWorkspace.RootNodeCount := 0;
    vtWorkspace.RootNodeCount := 7;
    openWorkspaceFile(f);
  end;
end;

procedure TTransformerForm.mnuOptionsClick(Sender: TObject);
begin
  TransformerOptionsForm := TTransformerOptionsForm.create(self);
  try
    TransformerOptionsForm.ini := FIni;
    if TransformerOptionsForm.ShowModal = mrOk then
    begin
      FEngine.terminologyServer := FIni.ReadString('Workspace', 'TerminologyServer', '');
      checkExecutionState;
    end;
  finally
    TransformerOptionsForm.free;
  end;

end;

procedure TTransformerForm.mnuPackageManagerClick(Sender: TObject);
begin
  PackageCacheForm := TPackageCacheForm.Create(self);
  try
    PackageCacheForm.ShowModal;
  finally
    PackageCacheForm.Free;
  end;
end;

procedure TTransformerForm.mnuPasteClick(Sender: TObject);
var
  editor : TScintEdit;
begin
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  editor.PasteFromClipboard;
end;

procedure TTransformerForm.mnuPastWorkspaceClick(Sender: TObject);
var
  st : TStringList;
  s, f : String;
begin
  if FEngine.Running then
    MessageDlg('Cannot change workspace while debugging', mtError, [mbok], 0)
  else
  begin
    f := '';
    st := TStringList.create;
    try
      FIni.ReadSection('Workspaces', st);
      for s in st do
        if FIni.ReadInteger('Workspaces', s, 0) = (Sender as TMenuItem).Tag then
          f := s;
    finally
      st.free;
    end;
    if (folderExists(f)) then
      LoadWorkspace(TWorkspace.Create(f))
    else
      MessageDlg('Unable to find '+f, mtError, [mbok], 0);
  end;
end;

procedure TTransformerForm.mnuPrettyClick(Sender: TObject);
begin
  case FEditor.id.format of
    fmtV2: raise Exception.Create('Not Supported Yet');
    fmtCDA: FEditor.memo.RawText := makeXmlPretty(FEditor.memo.RawText);
    fmtResource: raise Exception.create('Not Supported Yet');
    fmtJS: raise Exception.create('Not Supported Yet');
    fmtMap: raise Exception.create('Not Supported');
    fmtTemplate: raise Exception.create('Not Supported Yet');
    fmtMarkdown: raise Exception.create('Not Supported for Markdown');
  end;
end;

procedure TTransformerForm.mnuRecompileAllClick(Sender: TObject);
begin
  FEngine.clear;
  FEngine.compileAll;
end;

procedure TTransformerForm.mnuRedoClick(Sender: TObject);
var
  editor : TScintEdit;
begin
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  editor.Redo;
end;

procedure TTransformerForm.mnuRemoveEmptyLinesClick(Sender: TObject);
var
  editor : TScintEdit;
  i : integer;
begin
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  editor.BeginUndoAction;
  try
    for i := editor.Lines.Count - 1 downto 0 do
      if editor.Lines[i] = '' then
        editor.Lines.Delete(i);
  finally
    editor.EndUndoAction;
  end;
end;

procedure TTransformerForm.mnuRenameClick(Sender: TObject);
var
  node : PVirtualNode;
  p : PTreeDataPointer;
  f : TWorkspaceFile;
  list : TFslList<TWorkspaceFile>;
  s : String;
begin
  p := vtWorkspace.GetNodeData(FSelected);
  if (p.obj is TWorkspaceFile) then
  begin
    f := (p.obj as TWorkspaceFile);
    node := FSelected.Parent;
    list := PTreeDataPointer(vtWorkspace.GetNodeData(FSelected.Parent)).obj as TFslList<TWorkspaceFile>;
    sdNew.InitialDir := FWorkspace.folder;
    sdNew.DefaultExt := ExtractFileExt(f.filename);
    sdNew.FileName := '';
    sdNew.Title := 'Rename '+nodeCaption(node.Index);
    if sdNew.execute then
    begin
      if FWorkspace.includesFile(sdNew.FileName) then
        raise EFslException.Create('The file "'+sdNew.FileName+'" is already in the '+UI_NAME);
      RenameFile(f.actualName, sdNew.FileName);
      s := makeRelativePath(sdNew.FileName, FWorkspace.folder);
      f.filename := s;
      FWorkspace.save;
      vtWorkspace.RootNodeCount := 0;
      vtWorkspace.RootNodeCount := 7;
      renameWorkspaceFile(editorForFile(f), s);
    end;
  end;
end;

procedure TTransformerForm.mnuReplaceClick(Sender: TObject);
begin
  FindDialog.CloseDialog;
  if ReplaceDialog.Handle = 0 then begin
    InitializeFindText(ReplaceDialog, pgTabs.ActivePage.Controls[0] as TScintEdit);
    ReplaceDialog.ReplaceText := FLastReplaceText;
  end;
  ReplaceDialog.Execute;
end;

procedure TTransformerForm.mnuSaveAllClick(Sender: TObject);
var
  i : integer;
  e : TEditorInformation;
begin
  for i := 1 to pgTabs.PageCount - 1 do
  begin
    e := editorForTab(pgTabs.Pages[i]);
    if (e.isDirty) then
      saveWorkspaceFile(e);
  end;
end;

procedure TTransformerForm.mnuSaveClick(Sender: TObject);
begin
  saveWorkspaceFile(FEditor);
  FWorkspace.save;
  tbSave.Enabled := FEditor.isDirty;
  mnuSave.Enabled := tbSave.Enabled;
  tbSaveAll.Enabled := anyFilesDirty;
  mnuSaveAll.Enabled := tbSaveAll.Enabled;
end;

procedure TTransformerForm.mnuSearchClick(Sender: TObject);
begin
  ReplaceDialog.CloseDialog;
  if FindDialog.Handle = 0 then
    InitializeFindText(FindDialog, pgTabs.ActivePage.Controls[0] as TScintEdit);
  FindDialog.Execute;
end;

procedure TTransformerForm.mnuUndoClick(Sender: TObject);
var
  editor : TScintEdit;
begin
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  editor.Undo;
end;

procedure TTransformerForm.mnuUnixEOLClick(Sender: TObject);
var
  editor : TScintEdit;
begin
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  editor.LineEndings := sleLF;
  mnuWindowsEOL.Checked := false;
  mnuUnixEOL.Checked := false;
  case editor.LineEndings of
    sleCRLF: mnuWindowsEOL.Checked := true;
    sleLF: mnuUnixEOL.Checked := true;
  end;
end;

procedure TTransformerForm.mnuWindowsEOLClick(Sender: TObject);
var
  editor : TScintEdit;
begin
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  editor.LineEndings := sleCRLF;
  mnuWindowsEOL.Checked := false;
  mnuUnixEOL.Checked := false;
  case editor.LineEndings of
    sleCRLF: mnuWindowsEOL.Checked := true;
    sleLF: mnuUnixEOL.Checked := true;
  end;
end;

procedure TTransformerForm.NewMarkdown1Click(Sender: TObject);
begin
  makeNewFile('Template', 'md', TEMPLATE_MARKDOWN, fmtMarkdown, FWorkspace.markdowns);
end;

function TTransformerForm.nodeCaption(i: integer): String;
begin
  case i of
    0: result := 'Message';
    1: result := 'Document';
    2: result := 'Resource';
    3: result := 'Script';
    4: result := 'Map';
    5: result := 'Template';
    6: result := 'Markdown';
  end
end;

procedure TTransformerForm.pmNewItemClick(Sender: TObject);
var
  node : PVirtualNode;
  p : PTreeDataPointer;
begin
  node := FSelected;
  p := vtWorkspace.GetNodeData(node);
  if (p.obj is TWorkspaceFile) then
    node := FSelected.Parent;
  case node.Index of
    0: mnuNewMessageClick(self);
    1: mnuNewDocumentClick(self);
    2: mnuNewResourceJsonClick(self);
    3: mnuNewScriptClick(self);
    4: mnuNewMapClick(self);
    5: mnuNewTemplateClick(self);
    6: NewMarkdown1Click(self);
  end;
end;

procedure TTransformerForm.pmTabsPopup(Sender: TObject);
begin
  pmCloseThis.Enabled := pgTabs.TabIndex <> 0;
end;

procedure TTransformerForm.pmWorkspacePopup(Sender: TObject);
var
  node : PVirtualNode;
  p : PTreeDataPointer;
begin
  if FSelected = nil then
  begin
    pmNewItem.Caption := '&New Item';
    pmNewItem.Enabled := false;
    pmDuplicate.Enabled := false;
    pmRename.Enabled := false;
    pmDrop.Enabled := false;
  end
  else
  begin
    node := FSelected;
    p := vtWorkspace.GetNodeData(node);
    if (p.obj is TWorkspaceFile) then
    begin
      pmDuplicate.Enabled := true;
      pmRename.Enabled := not FEngine.Running;
      pmDrop.Enabled := not FEngine.Running;
      node := node.Parent;
    end
    else
    begin
      pmDuplicate.Enabled := false;
      pmRename.Enabled := false;
      pmDrop.Enabled := false;
    end;
    pmNewItem.Enabled := true;
    pmNewItem.Caption := '&New '+nodeCaption(node.Index);
  end;
end;

procedure TTransformerForm.Print1Click(Sender: TObject);
begin
  raise Exception.Create('Not done yet');
end;

procedure TTransformerForm.vtCallStackAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FCallStackSelected := Node;
end;

procedure TTransformerForm.vtCallStackClick(Sender: TObject);
var
  p : PTreeDataPointer;
  dbg : TTransformEngineExecutionPoint;
  f :  TWorkspaceFile;
  sel : TScintRange;
begin
  p := vtCallStack.GetNodeData(FCallStackSelected);
  dbg := p.obj as TTransformEngineExecutionPoint;
  if (dbg = nil) then
    exit;
  if (dbg.Start.line <> -1) then
  begin
    if dbg.file_ <> nil then
    begin
      openWorkspaceFile(dbg.file_);
      sel.StartPos := FEditor.memo.GetPositionFromLineColumn(dbg.Start.line-1, dbg.Start.col-1);
      sel.EndPos := FEditor.memo.GetPositionFromLineColumn(dbg.Stop.line-1, dbg.Stop.col-1);
      FEditor.memo.Selection := sel;
      FEditor.memo.ScrollCaretIntoView;
    end;
  end;
  if dbg.variables <> nil then
    ShowVars(dbg.variables);
end;

procedure TTransformerForm.vtCallStackDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  p : PTreeDataPointer;
  dbg : TTransformEngineExecutionPoint;
begin
  p := vtWorkspace.GetNodeData(Node);
  dbg := p.obj as TTransformEngineExecutionPoint;
  case dbg.kind of
    epJavscript: TargetCanvas.Font.Color := clPurple;
    epMap: TargetCanvas.Font.Color := clYellow;
    epMapGroup: TargetCanvas.Font.Color := clNavy;
    epMapRule: TargetCanvas.Font.Color := clBlack;
    epMapTarget: TargetCanvas.Font.Color := clMaroon;
    epLiquidStatement: TargetCanvas.Font.Color := clOlive;
    epLiquidTag: TargetCanvas.Font.Color := clGreen;
  end;
end;

procedure TTransformerForm.vtCallStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
  dbg : TTransformEngineExecutionPoint;
begin
  p := vtWorkspace.GetNodeData(Node);
  dbg := p.obj as TTransformEngineExecutionPoint;
  CellText := dbg.desc;
end;

procedure TTransformerForm.vtCallStackInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p : PTreeDataPointer;
begin
  p := vtWorkspace.GetNodeData(Node);
  p.obj := FDebugInfo.callStack[Node.Index];
end;

procedure TTransformerForm.vtCallStackRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FCallStackSelected := Nil;
end;

procedure TTransformerForm.vtConfigAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FConfigSelected := Node;
  checkExecutionState;
end;

procedure TTransformerForm.vtConfigGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  ec : TWorkspaceExecConfig;
  p : PTreeDataPointer;
begin
  p := vtWorkspace.GetNodeData(Node);
  ec := p.obj as TWorkspaceExecConfig;
  case Column of
    0: CellText := ec.script;
    1: CellText := ec.focus;
    2: CellText := ''; // for now
  end;
end;

procedure TTransformerForm.vtConfigInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p : PTreeDataPointer;
begin
  p := vtWorkspace.GetNodeData(Node);
//  ParentNode = nil
  p.obj := FWorkspace.configurations[Node.Index];
end;

procedure TTransformerForm.vtConfigRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FConfigSelected := Nil;
  checkExecutionState;
end;

procedure TTransformerForm.vtVarDetailsColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  FIni.WriteInteger('debug', 'var-details-col-1', vtVarDetails.Header.Columns[0].Width);
  FIni.WriteInteger('debug', 'var-details-col-2', vtVarDetails.Header.Columns[1].Width);
  FIni.WriteInteger('debug', 'var-details-col-3', vtVarDetails.Header.Columns[2].Width);
end;

procedure TTransformerForm.vtVarDetailsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  p : PTreeDataPointer;
begin
  p := vtWorkspace.GetNodeData(Node);
  if (p.obj is TFHIRPRoperty) then
    p.obj.free;
end;

procedure TTransformerForm.vtVarDetailsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
  b : TFHIRObject;
  n : String;
begin
  p := vtWorkspace.GetNodeData(Node);
  b := p.obj2 as TFHIRObject;
  if (p.obj is TTransformEngineExecutionVariable) then
    n := (p.obj as TTransformEngineExecutionVariable).name
  else
    n := (p.obj as TFHIRProperty).name;
  case Column of
    0: CellText := n;
    1: CellText := tail(b.fhirType);
    2: CellText := b.primitiveValue;
  end;
end;

procedure TTransformerForm.vtVarDetailsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  p : PTreeDataPointer;
  pl : TFHIRPropertyList;
  pr : TFHIRProperty;
  b, v : TFHIRObject;
begin
  p := vtWorkspace.GetNodeData(Node);
  b := p.obj2 as TFHIRObject;
  pl := b.createPropertyList(false);
  try
    ChildCount := 0;
    for pr in pl do
    begin
      for v in pr.Values do
        inc(ChildCOunt);
    end;
  finally
    pl.free;
  end;
end;

procedure TTransformerForm.vtVarDetailsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p, pp : PTreeDataPointer;
  b, v, t : TFHIRObject;
  pl : TFHIRPropertyList;
  i : Integer;
  pr : TFHIRProperty;
  c : Boolean;
begin
  p := vtWorkspace.GetNodeData(Node);
  if ParentNode = nil then
  begin
    p.obj := FVariable;
    p.obj2 := FVariable.obj;
    c := not FVariable.obj.isPrimitive or FVariable.obj.hasExtensions;
    if c then
      InitialStates := [ivsHasChildren, ivsExpanded]
    else
      InitialStates := [];
  end
  else
  begin
    pp := vtWorkspace.GetNodeData(ParentNode);
    b := pp.obj2 as TFHIRObject;
    pl := b.createPropertyList(false);
    try
      i := 0;
      for pr in pl do
      begin
        for v in pr.Values do
        begin
          if i = Node.Index then
          begin
            p.obj := pr.link;
            p.obj2 := v;
            c := not v.isPrimitive or v.hasExtensions;
            if c then
              InitialStates := [ivsHasChildren]
            else
              InitialStates := [];
            exit;
          end;
          inc(i);
        end;
      end;
    finally
      pl.free;
    end;
  end;
end;

procedure TTransformerForm.vtVarsAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FVarsSelected := Node;
end;

procedure TTransformerForm.vtVarsClick(Sender: TObject);
var
  p : PTreeDataPointer;
  v : TTransformEngineExecutionVariable;
begin
  p := vtVars.GetNodeData(FVarsSelected);
  v := p.obj as TTransformEngineExecutionVariable;
  ShowVariable(v);
end;

procedure TTransformerForm.vtVarsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  p : PTreeDataPointer;
  v : TTransformEngineExecutionVariable;
begin
  p := vtWorkspace.GetNodeData(Node);
  v := p.obj as TTransformEngineExecutionVariable;
  case v.mode of
    tvmNone: TargetCanvas.Font.Color := clBlack;
    tvmInput: TargetCanvas.Font.Color := clNavy;
    tvmOutput: TargetCanvas.Font.Color := clMaroon;
  end;
end;

procedure TTransformerForm.vtVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
  v : TTransformEngineExecutionVariable;
begin
  p := vtWorkspace.GetNodeData(Node);
  v := p.obj as TTransformEngineExecutionVariable;
  if v.value <> '' then
    CellText := v.name+' = "'+v.value+'"'
  else
    CellText := v.name+': '+v.typeName;
  if v.mode <> tvmNone then
    CellText := CellText + ' ['+CODES_TTransformEngineExecutionVariableMode[v.mode]+']';
end;

procedure TTransformerForm.vtVarsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p : PTreeDataPointer;
begin
  p := vtWorkspace.GetNodeData(Node);
  p.obj := FVariables[Node.Index];
end;

procedure TTransformerForm.vtVarsRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FVarsSelected := nil;
end;

procedure TTransformerForm.vtWorkspaceAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelected := Node;
end;

procedure TTransformerForm.vtWorkspaceDblClick(Sender: TObject);
var
  p : PTreeDataPointer;
  f : TWorkspaceFile;
begin
  p := vtWorkspace.GetNodeData(FSelected);
  if (p.obj is TWorkspaceFile) then
  begin
    f := (p.obj as TWorkspaceFile);
    openWorkspaceFile(f);
  end;
end;

procedure TTransformerForm.vtWorkspaceGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
var
  p : PTreeDataPointer;
begin
  Ghosted := false;
  ImageList := ImageList1;
  p := vtWorkspace.GetNodeData(Node);
  if (p.obj is TFslList<TWorkspaceFile>) then
    case Node.Index of
      0: ImageIndex := 9;
      1: ImageIndex := 10;
      2: ImageIndex := 11;
      3: ImageIndex := 12;
      4: ImageIndex := 13;
      5: ImageIndex := 14;
      6: ImageIndex := 44;
    end
  else
    case (p.obj as TWorkspaceFile).compileStatus of
      csNotCompiled: ImageIndex := 41;
      csOk: ImageIndex := 15;
      csError: ImageIndex := 42;
      csWorking: ImageIndex := 43;
    end;
end;

procedure TTransformerForm.vtWorkspaceGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
begin
  p := vtWorkspace.GetNodeData(Node);
  if (p.obj is TFslList<TWorkspaceFile>) then
    CellText := nodeCaption(node.Index)+'s'
  else
    CellText := (p.obj as TWorkspaceFile).title;
end;

procedure TTransformerForm.vtWorkspaceInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  p : PTreeDataPointer;
begin
  p := vtWorkspace.GetNodeData(Node);
  if (p.obj is TFslList<TWorkspaceFile>) then
    ChildCount := (p.obj as TFslList<TWorkspaceFile>).Count
  else
    ChildCount := 0;
end;

procedure TTransformerForm.vtWorkspaceInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p, pp : PTreeDataPointer;
begin
  p := vtWorkspace.GetNodeData(Node);
  if ParentNode = nil then
  begin
    case Node.Index of
      0: p.obj := FWorkspace.messages;
      1: p.obj := FWorkspace.documents;
      2: p.obj := FWorkspace.resources;
      3: p.obj := FWorkspace.scripts;
      4: p.obj := FWorkspace.maps;
      5: p.obj := FWorkspace.templates;
      6: p.obj := FWorkspace.markdowns;
    end;
    (p.obj as TFslList<TWorkspaceFile>).TagObject := TObject(node);
    InitialStates := [ivsHasChildren, ivsExpanded];
  end
  else
  begin
    pp := vtWorkspace.GetNodeData(parentNode);
    p.obj := (pp.obj as TFslList<TWorkspaceFile>)[Node.Index];
    (p.obj as TWorkspaceFile).TagObject := TObject(node);
    InitialStates := [];
  end;
end;

procedure TTransformerForm.vtWorkspaceRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelected := Nil;
end;

procedure TTransformerForm.zoomIn;
var
  i : integer;
begin
  mConsole.Font.Size := trunc(mConsole.Font.Size * 1.25);
  for i := 1 to pgTabs.PageCount - 1 do
    TScintEdit(pgTabs.Pages[i].Controls[0]).font.Assign(mConsole.Font);
end;

procedure TTransformerForm.ZoomOut;
var
  i : integer;
begin
  mConsole.Font.Size := trunc(mConsole.Font.Size / 1.25);
  for i := 1 to pgTabs.PageCount - 1 do
    TScintEdit(pgTabs.Pages[i].Controls[0]).font.Assign(mConsole.Font);
end;

procedure TTransformerForm.ZoomReset;
var
  i : integer;
begin
  mConsole.Font.Size := 12;
  for i := 1 to pgTabs.PageCount - 1 do
    TScintEdit(pgTabs.Pages[i].Controls[0]).font.Assign(mConsole.Font);
end;

function TTransformerForm.findWorkspaceFile(f : TWorkspaceFile) : TEditorInformation;
var
  i : integer;
  e : TEditorInformation;
begin
  for i := 1 to pgTabs.PageCount - 1 do
  begin
    e := editorForTab(pgTabs.Pages[i]);
    if e.id = f then
      exit(e);
  end;
  result := nil;
end;

function TTransformerForm.openWorkspaceFile(f : TWorkspaceFile) : TEditorInformation;
var
  tab : TTabSheet;
begin
  result := findWorkspaceFile(f);
  if result <> nil then
  begin
    pgTabs.ActivePage := result.tab;
  end
  else
  begin
    tab := TTabSheet.Create(pgTabs);
    tab.Caption := f.title;
    tab.PageControl := pgTabs;
    case f.format of
      fmtV2: tab.ImageIndex := 9;
      fmtCDA: tab.ImageIndex := 10;
      fmtResource: tab.ImageIndex := 11;
      fmtJS: tab.ImageIndex := 12;
      fmtMap: tab.ImageIndex := 13;
      fmtTemplate: tab.ImageIndex := 14;
      fmtMarkdown: tab.ImageIndex := 44;
    end;
    result := TEditorInformation.create(f.link);
    FEngine.editors.Add(result);
    result.tab := tab;
    result.init();
    result.memo.Font.Assign(mConsole.Font);
    result.memo.OnChange := memoChange;
    result.memo.OnUpdateUI := memoStatusChange;
    result.memo.OnKeyDown := FormKeyDown;
    result.memo.OnMarginClick := MemoMarginClick;
    result.memo.PopupMenu := pmEditor;
    FEditor := result;
    FEngine.setActiveEditor(FEditor);
    pgTabs.ActivePage := tab;
    pgTabsChange(pgTabs);
  end;
  if result.id.compileStatus = csError then
    status(clMaroon, result.id.errorMsg);
end;

procedure TTransformerForm.PackageManager1Click(Sender: TObject);
var
  form : TPackageCacheForm;
begin
  form := TPackageCacheForm.create(self);
  try
    form.ShowModal;
  finally
    form.Free;
  end;
end;

procedure TTransformerForm.saveWorkspaceFile(editor : TEditorInformation);
begin
  editor.save;
  // memoChange(editor);
end;

procedure TTransformerForm.setDebugStatus(enabled: boolean);
begin
  tbStepInto.Enabled := enabled;
  tbStepOver.Enabled := enabled;
  tbStepOut.Enabled := enabled;
  tbStop.Enabled := enabled;
  mnuStepInto.Enabled := enabled;
  mnuStepOver.Enabled := enabled;
  mnuStepOut.Enabled := enabled;
  mnuEndDebugging.Enabled := enabled;
end;

procedure TTransformerForm.startRunning;
var
  i : integer;
begin
  FRunningState := true;
  for i := 1 to pgTabs.PageCount - 1 do
    editorForTab(pgTabs.Pages[i]).readOnly := true;
  checkExecutionState;
  pgTabsChange(nil);
end;

procedure TTransformerForm.stopRunning;
var
  i : integer;
begin
  FRunningState := false;
  for i := 1 to pgTabs.PageCount - 1 do
    editorForTab(pgTabs.Pages[i]).readOnly := false;
  checkExecutionState;
  pgTabsChange(nil);
end;

procedure TTransformerForm.status(color: TColor; msg: String);
begin
  pnlStatus.Color := color;
  pnlStatus.Panels[spStatus].text := '   '+msg;
  pnlStatus.Update;
  Application.ProcessMessages;
end;


procedure TTransformerForm.tbHomeClick(Sender: TObject);
begin
  pgTabs.TabIndex := 0;
end;

procedure TTransformerForm.tbNewClick(Sender: TObject);
var
  pnt: TPoint;
begin
  pmAddExisting.Visible := false;
  if GetCursorPos(pnt) then
    pmAddAsset.Popup(pnt.X, pnt.Y);
end;

procedure TTransformerForm.tbStepIntoClick(Sender: TObject);
begin
  FStepOutcome := DBG_STEP_INTO;
end;

procedure TTransformerForm.tbStepOutClick(Sender: TObject);
begin
  FStepOutcome := DBG_STEP_OUT;
end;

procedure TTransformerForm.tbStepOverClick(Sender: TObject);
begin
  FStepOutcome := DBG_STEP_OVER;
end;

procedure TTransformerForm.tbStopClick(Sender: TObject);
begin
  FStepOutcome := DBG_STOP;
end;

procedure TTransformerForm.Timer1Timer(Sender: TObject);
begin
  if not FInitialised then
    InitialiseExec;
  if (pgTabs.ActivePageIndex = 0) or (Feditor = nil) then
  begin
    mnuPaste.Enabled := false;
    tbPaste.Enabled := false;
    pmnuPaste.Enabled := false;
  end
  else
  begin
    mnuPaste.Enabled := not FEditor.memo.ReadOnly and Clipboard.HasFormat(CF_TEXT);
    tbPaste.Enabled := mnuPaste.Enabled;
    pmnuPaste.Enabled := mnuPaste.Enabled;
  end;
end;

procedure TTransformerForm.ToggleBreakPoint(Line: Integer);
var
  bpi : TBreakPointInfo;
begin
  if FEditor.id.hasBreakPoint(Line, bpi) then
    FEditor.id.BreakPoints.Remove(bpi)
  else
    FEditor.id.BreakPoints.Add(TBreakPointInfo.Create(line));
  FEditor.UpdateLineMarkers(Line);
end;

procedure TTransformerForm.closeWorkspaceFile(editor : TEditorInformation; checkSave : boolean);
begin
  if (editor = nil) then
    exit;

  if editor.isDirty and checkSave then
  begin
    case MessageDlg('Do you want to save '+editor.id.filename+'?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], 0) of
      mrCancel : abort;
      mrYes : saveWorkspaceFile(editor);
      mrNo : saveWorkspaceFile(editor);
    end;
  end;
  if pgTabs.ActivePage = editor.tab then
    pgTabs.TabIndex := pgTabs.TabIndex - 1;
  editor.tab.Free;
  FEngine.editors.Remove(editor);
end;


procedure TTransformerForm.renameWorkspaceFile(editor : TEditorInformation; fn : String);
begin
  if editor = nil then
    exit;
  editor.tab.Caption := editor.id.title;
  editor.FileTime := TFile.GetLastWriteTimeUtc(fn);
end;

procedure TTransformerForm.ReplaceDialogReplace(Sender: TObject);
var
  ReplaceCount, Pos: Integer;
  Range, NewRange: TScintRange;
  editor : TScintEdit;
begin
  FLastFindOptions := ReplaceDialog.Options;
  FLastFindText := ReplaceDialog.FindText;
  FLastReplaceText := ReplaceDialog.ReplaceText;
  editor := pgTabs.ActivePage.Controls[0] as TScintEdit;
  if frReplaceAll in FLastFindOptions then
  begin
    ReplaceCount := 0;
    editor.BeginUndoAction;
    try
      Pos := 0;
      while editor.FindText(Pos, editor.RawTextLength, FLastFindText,
         FindOptionsToSearchOptions(FLastFindOptions), Range) do
      begin
        NewRange := editor.ReplaceTextRange(Range.StartPos, Range.EndPos, FLastReplaceText);
        Pos := NewRange.EndPos;
        Inc(ReplaceCount);
      end;
    finally
      editor.EndUndoAction;
    end;
    if ReplaceCount = 0 then
      MsgBoxFmt('Cannot find "%s"', [FLastFindText], 'Find', mbInformation, MB_OK)
    else
      MsgBoxFmt('%d occurrence(s) replaced.', [ReplaceCount], 'Find', mbInformation, MB_OK);
  end
  else
  begin
    if editor.SelTextEquals(FLastFindText, frMatchCase in FLastFindOptions) then
      editor.SelText := FLastReplaceText;
    FindNext(editor);
  end;
end;

procedure TTransformerForm.mnuChooseWorkspaceClick(Sender: TObject);
begin
  if FEngine.Running then
    MessageDlg('Cannot change workspace while debugging', mtError, [mbok], 0)
  else
  begin
    fd.DefaultFolder := FWorkspace.folder;
    if fd.Execute then
      LoadWorkspace(TWorkspace.Create(fd.FileName));
  end;
end;

procedure TTransformerForm.mnuCompareClick(Sender: TObject);
var
  fmt : integer;
  src, output, msg : String;
  fnin, fnout: String;
begin
  src := FEditor.memo.RawText;
  fmt := 1;
  case FEditor.id.format of
    fmtV2: output := FEngine.canonical(FEditor.id);
    fmtCDA:
      begin
      fmt := 2;
      output := FEngine.canonical(FEditor.id);
      end;
    fmtResource:
      begin
      if isXml(src) then
        fmt := 2
      else
        fmt := 3;
      output := FEngine.canonical(FEditor.id);
      end;
    fmtJS: raise Exception.create('Not Supported Yet');
    fmtMap: output := FEngine.canonical(FEditor.id);
    fmtTemplate: output := FEngine.canonical(FEditor.id);
    fmtMarkdown: output := FEngine.canonical(FEditor.id);
  end;
  showdiff := true;
  if fmt = 1 then
  begin
    if CheckTextIsSame(src, output, msg) then
      ShowMessage('ok');
  end
  else if fmt = 2 then
  begin
    fnin := path(['c:\temp', 'source.xml']);
    fnout := path(['c:\temp', 'output.xml']);
    StringToFile(src, fnin, TEncoding.UTF8);
    StringToFile(output, fnout, TEncoding.UTF8);
    if CheckXMLIsSame(fnin, fnout, msg) then
      ShowMessage('ok');
  end
  else if fmt = 3 then
  begin
    fnin := path(['c:\temp', 'source.json']);
    fnout := path(['c:\temp', 'output.json']);
    StringToFile(src, fnin, TEncoding.UTF8);
    StringToFile(output, fnout, TEncoding.UTF8);
    if CheckJsonIsSame(fnin, fnout, msg) then
      ShowMessage('ok');
  end
end;

procedure TTransformerForm.mnuCompileClick(Sender: TObject);
begin
  if Feditor.id.format = fmtMarkdown then
  begin
    MarkdownPreviewForm := TMarkdownPreviewForm.create(self);
    try
      MarkdownPreviewForm.HTML := TCommonMarkEngine.process(FEditor.memo.RawText, true).replace(#10, #13#10).replace(#13#10#13#10, #13#10#13#10#13#10);
      MarkdownPreviewForm.Percent := FIni.ReadFloat('View', 'Markdown-Preview-Width', 0.5);
      if FIni.ReadBool('View', 'Markdown-Preview-Maximised', false) then
        MarkdownPreviewForm.WindowState := wsMaximized;
      MarkdownPreviewForm.ShowModal;
      FIni.writeFloat('View', 'Markdown-Preview-Width', MarkdownPreviewForm.Percent);
      FIni.WriteBool('View', 'Markdown-Preview-Maximised', MarkdownPreviewForm.WindowState = wsMaximized);
    finally
      MarkdownPreviewForm.free;
    end;
  end
  else
    FEngine.compile(FEditor.id, true);
end;

procedure TTransformerForm.Rewrite1Click(Sender: TObject);
begin
  case FEditor.id.format of
    fmtV2: FEditor.memo.RawText := FEngine.canonical(FEditor.id);
    fmtCDA: FEditor.memo.RawText := FEngine.canonical(FEditor.id);
    fmtResource: FEditor.memo.RawText := FEngine.canonical(FEditor.id);
    fmtJS: raise Exception.create('Not Supported Yet');
    fmtMap: FEditor.memo.RawText := FEngine.canonical(FEditor.id);
    fmtTemplate: FEditor.memo.RawText := FEngine.canonical(FEditor.id);
    fmtMarkdown: FEditor.memo.RawText := FEngine.canonical(FEditor.id);
  end;
end;

procedure TTransformerForm.ClearCallStack;
begin
  vtCallStack.RootNodeCount := 0;
  FCallStackSelected := nil;
end;

procedure TTransformerForm.ClearVariable;
begin
  vtVarDetails.RootNodeCount := 0;
  FVariable := nil;
end;

procedure TTransformerForm.ClearVars;
begin
  vtVars.RootNodeCount := 0;
  FVariables := nil;
end;


procedure TTransformerForm.SetFPDebuggerSetting(name: TFHIRPathDebuggerFormSetting; value: Integer);
begin
  FIni.WriteInteger('Debugger', CODES_TFHIRPathDebuggerFormSetting[name], value);
end;

procedure TTransformerForm.SetFPDebuggerSettingStr(name: TFHIRPathDebuggerFormSetting; value: String);
begin
  FIni.WriteString('Debugger', CODES_TFHIRPathDebuggerFormSetting[name], value);
end;

procedure TTransformerForm.SetStepLine(editor: TEditorInformation; ALine: Integer);
var
  OldLine: Integer;
begin
  if (editor <> FStepEditor) or (editor.StepLine <> ALine) then
  begin
    if FStepEditor <> nil then
    begin
      OldLine := FStepEditor.StepLine;
      FStepEditor.StepLine := -1;
      if OldLine >= 0 then
        FStepEditor.UpdateLineMarkers(OldLine);
    end;
    FStepEditor := editor;
    if FStepEditor <> nil then
    begin
      FStepEditor.StepLine := ALine;
      if FStepEditor.StepLine >= 0 then
      begin
        FStepEditor.memo.CaretLine := FStepEditor.StepLine;
        FStepEditor.memo.ScrollCaretIntoView;
        FStepEditor.UpdateLineMarkers(FStepEditor.StepLine);
      end;
    end;
  end;
end;

procedure TTransformerForm.ShowCallStack;
begin
  FCallStackSelected := nil;
  vtCallStack.RootNodeCount := FDebugInfo.callStack.Count;
  if FDebugInfo.callStack.Count > 0 then
  begin
    openWorkspaceFile(FDebugInfo.callStack[0].file_);
    SetStepLine(FEditor, FDebugInfo.callStack[0].Start.line-1);
  end;
end;

procedure TTransformerForm.ShowVariable(variable: TTransformEngineExecutionVariable);
begin
  FVariable := variable;
  vtVarDetails.RootNodeCount := 0;
  vtVarDetails.RootNodeCount := 1;
end;

procedure TTransformerForm.ShowVars(vars : TFslList<TTransformEngineExecutionVariable>);
begin
  FVariables := vars;
  vtVars.RootNodeCount := 0;
  vtVars.RootNodeCount := FVariables.Count;
  FVarsSelected := nil;
  ClearVariable;
end;

procedure TTransformerForm.MemoLinesInserted(FirstLine, Count: integer);
var
  I, Line: Integer;
begin
  if FEditor.ErrorLine >= FirstLine then
    FEditor.ErrorLine := FEditor.ErrorLine + Count;
end;

procedure TTransformerForm.MemoMarginClick(Sender: TObject; MarginNumber, Line: Integer);
begin
  if (MarginNumber = 1) and (FEditor.id.format in [fmtJS, fmtMap, fmtTemplate, fmtMarkdown]) then
    ToggleBreakPoint(Line);
end;

procedure TTransformerForm.MemoLinesDeleted(FirstLine, Count, FirstAffectedLine: Integer);
var
  I, Line: Integer;
begin
  if FEditor.ErrorLine >= FirstLine then
  begin
    if FEditor.ErrorLine < FirstLine + Count then
      FEditor.ErrorLine := -1
    else
      FEditor.ErrorLine := FEditor.ErrorLine - Count;
  end;

  { When lines are deleted, Scintilla insists on moving all of the deleted
    lines' markers to the line on which the deletion started
    (FirstAffectedLine). This is bad for us as e.g. it can result in the line
    having two conflicting markers (or two of the same marker). There's no
    way to stop it from doing that, or to easily tell which markers came from
    which lines, so we simply delete and re-create all markers on the line. }
  FEditor.UpdateLineMarkers(FirstAffectedLine);
end;

procedure TTransformerForm.updateWorkspaceFile(editor: TEditorInformation; src: String);
begin
  editor.memo.RawText := src;
end;

procedure TTransformerForm.InitExceptions;
begin
  JclStartExceptionTracking;
  Application.OnException := HandleException;
end;

procedure TTransformerForm.CloseExceptions;
Begin
End;

procedure TTransformerForm.HandleException(Sender: TObject; E: Exception);
begin
  ExceptionHandlerDialog := TExceptionHandlerDialog.Create(self);
  Try
    ExceptionHandlerDialog.eMessage.Caption := e.Message+#13#10#13#10+e.StackTrace;
    ExceptionHandlerDialog.ShowModal;
  Finally
    ExceptionHandlerDialog.Free;
  End;
end;


end.


