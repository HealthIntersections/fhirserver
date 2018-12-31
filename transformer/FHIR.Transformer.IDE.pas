unit FHIR.Transformer.IDE;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, IniFiles, ClipBrd, IOUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.ImageList,
  Vcl.ImgList, VirtualTrees, Vcl.Buttons, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ToolWin,
  JclDebug,
  ScintEdit, ScintInt, ScintFormats,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Comparisons, FHIR.Support.MXml, FHIR.Support.Shell,
  FHIR.Ui.ListSelector, FHIR.Cda.Scint, FHIR.V2.Scint,
  FHIR.Javascript,
  FHIR.Cache.PackageManagerDialog,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.Base.PathEngine, FHIR.Base.PathDebugger,
  FHIR.R4.Context, FHIR.R4.Resources, FHIR.R4.MapUtilities, FHIR.R4.Scint, FHIR.R4.ElementModel, FHIR.R4.Json, FHIR.R4.XML, FHIR.R4.Factory, FHIR.R4.PathEngine, FHIR.R4.Utilities,
  FHIR.Transformer.Workspace, FHIR.Transformer.Utilities, FHIR.Transformer.Engine, FHIR.Transformer.Context, FHIR.Transformer.Editor, FHIR.Transformer.Debugger,
  FHIR.Transformer.WorkingDialog, FHIR.Transformer.FileChangedDlg, FHIR.Transformer.ExceptionHandlerDlg;

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

  spCaretPos = 0;
  spMode = 1;
  spStatus = 2;


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
    Edit1: TMenuItem;
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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbxEventType: TComboBox;
    cbxSource: TComboBox;
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
    cbxScript: TComboBox;
    lblScript: TLabel;
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
    btnOpenScript: TBitBtn;
    btnOpenSource: TBitBtn;
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
    Panel10: TPanel;
    ListBox3: TListBox;
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
    Label6: TLabel;
    cbxOutcome: TComboBox;
    cbxTarget: TComboBox;
    btnOpenTarget: TBitBtn;
    Help1: TMenuItem;
    N11: TMenuItem;
    About1: TMenuItem;
    estException1: TMenuItem;
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
    procedure cbxEventTypeChange(Sender: TObject);
    procedure cbxSourceChange(Sender: TObject);
    procedure mnuPackageManagerClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure cbxScriptChange(Sender: TObject);
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
    procedure btnOpenScriptClick(Sender: TObject);
    procedure btnOpenSourceClick(Sender: TObject);
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
    procedure cbxOutcomeChange(Sender: TObject);
    procedure btnOpenTargetClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure estException1Click(Sender: TObject);
  private
    FIni : TIniFile;
    FWorkspace : TWorkspace;
    FCache : TResourceMemoryCache;

    FEditor : TEditorInformation;
    FLoading : boolean;
    FProgress : TWorkingForm;

    FWorkspaceWidth : integer;
    FDebugHeight : integer;

    FLastFindOptions: TFindOptions;
    FLastFindText: String;
    FLastReplaceText: String;

    FSelected : PVirtualNode;
    FCallStackSelected : PVirtualNode;
    FVarsSelected : PVirtualNode;

    FPathSelection : TFslList<TPathSelection>;
    FRunning : boolean;
    FWantClose : boolean;
    FDebugInfo : TFHIRStructureMapDebugContext;
    FStepEditor : TEditorInformation;
    FStepOutcome : integer;
    FStack : TFslList<TFHIRStructureMapDebugContext>;
    FVariables : TVariables;
    FVariable : TVariable;

    function DoSave(command : String) : boolean;
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
    function engineGetSource(sender : TConversionEngine; f : TWorkspaceFile) : TStream;
    procedure engineStatus(sender : TConversionEngine; message : String);
    procedure engineLog(sender : TConversionEngine; message : String);
    procedure cacheLog(sender : TObject; pct : integer; done : boolean; desc : String);
    procedure setDebugStatus(enabled : boolean);

    function rewriteV2(src, title: String): String;
    function rewriteCDA(src, title: String): String;

    procedure checkV2(src, title : String);
    procedure checkCDA(src, title : String);
    procedure checkResource(src, title : String);
    procedure checkJS(src, title : String);
    procedure checkMap(src, title : String; f : TWorkspaceFile);
    procedure checkTemplate(src, title : String);

    procedure SetErrorLine(ALine: Integer);
    procedure SetStepLine(editor : TEditorInformation; ALine: Integer);
    procedure HideError();
    procedure UpdateLineMarkers(editor : TEditorInformation; const Line: Integer);
    procedure MemoLinesDeleted(FirstLine, Count, FirstAffectedLine: Integer);
    procedure MemoLinesInserted(FirstLine, Count: integer);
    procedure UpdateAllLineMarkers();
    procedure ToggleBreakPoint(Line: Integer);

    function parseCDA(context : TFHIRWorkerContext) : TFHIRObject;
    function parseResource(context : TFHIRWorkerContext) : TFHIRObject;
    procedure runFHIRPath(debug: boolean);
    function GetFPDebuggerSetting(name : TFHIRPathDebuggerFormSetting) : Integer;
    procedure SetFPDebuggerSetting(name : TFHIRPathDebuggerFormSetting; value : Integer);
    function GetFPDebuggerSettingStr(name : TFHIRPathDebuggerFormSetting) : String;
    procedure SetFPDebuggerSettingStr(name : TFHIRPathDebuggerFormSetting; value : String);

    procedure startRunning;
    procedure stopRunning;
    procedure executeTransform(debug: boolean);
    procedure ShowCallStack(f : TWorkspaceFile);
    procedure ClearCallStack;
    procedure ShowVars(vars : TVariables);
    procedure ClearVars;
    procedure ShowVariable(variable : TVariable);
    procedure ClearVariable;
    procedure DebugTransform(sender : TObject; info : TFHIRStructureMapDebugContext);
    procedure DoCompiled(sender : TConversionEngine; f : TWorkspaceFile; checkBreakpointProc : TCheckBreakpointEvent);

    procedure HandleException(Sender: TObject; E: Exception);
    Procedure InitExceptions;
    procedure CloseExceptions;
  public
    { Public declarations }
  end;

var
  TransformerForm: TTransformerForm;

implementation

{$R *.dfm}

uses FHIR.Transformer.SettingsDialog;


function isXml(s : String) : boolean;
begin
  if not s.Contains('<') then
    result := false
  else if not s.Contains('{') then
    result := true
  else
    result := s.IndexOf('<') < s.IndexOf('{');
end;

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

procedure TTransformerForm.btnExecuteClick(Sender: TObject);
begin
  if FDebugInfo <> nil then
    FStepOutcome := DBG_EXECUTE
  else
    executeTransform(true);
end;

procedure TTransformerForm.executeTransform(debug : boolean);
var
  engine : TCDAConversionEngine;
  f : TWorkspaceFile;
  editor :TEditorInformation;
  s, fns, fnt : String;
begin
  try
    startRunning;
    engine := TCDAConversionEngine.create;
    try
      engine.source := (cbxSource.Items.Objects[cbxSource.ItemIndex] as TWorkspaceFile).link;
      engine.map := (cbxScript.Items.Objects[cbxScript.ItemIndex] as TWorkspaceFile).link;
      engine.cache := FCache.Link;
      engine.workspace := FWorkspace.link;
      engine.OnWantSource := engineGetSource;
      engine.OnStatus := engineStatus;
      engine.OnLog := engineLog;
      if debug then
        engine.OnTransformDebug := DebugTransform;
      engine.load;
      engine.execute;
      if engine.Outcomes.Count = 0 then
        MessageDlg('No output from Transform', mtInformation, [mbok], 0)
      else if engine.Outcomes.Count > 1 then
        MessageDlg('Multiple outputs from Transform - not handled yet', mtError, [mbok], 0)
      else
        case FWorkspace.Outcome of
          tomIgnore : MessageDlg('Transform Complete with no errors', mtInformation, [mbok], 0);
          tomSaveTo :
            begin
            f := cbxTarget.Items.Objects[cbxTarget.ItemIndex] as TWorkspaceFile;
            editor := editorForFile(f);
            if (editor = nil) then
              editor := openWorkspaceFile(f);
            if isXml(editor.memo.RawText) then
              s := resourceToString(engine.Outcomes[0] as TFhirResource, ffXml, OutputStylePretty)
            else
              s := resourceToString(engine.Outcomes[0] as TFhirResource, ffJson, OutputStylePretty);
            updateWorkspaceFile(editor, s);
            end;
          tomCompare :
            begin
            f := cbxTarget.Items.Objects[cbxTarget.ItemIndex] as TWorkspaceFile;
            fns := makeAbsolutePath(f.filename, FWorkspace.folder);
            fnt := Path([SystemTemp, 'generated-'+ExtractFileName(fns)]);
            if isXml(FileToString(fns, TEncoding.UTF8)) then
              StringToFile(resourceToString(engine.Outcomes[0] as TFhirResource, ffXml, OutputStylePretty), fnt, TEncoding.UTF8)
            else
              StringToFile(resourceToString(engine.Outcomes[0] as TFhirResource, ffJson, OutputStylePretty), fnt, TEncoding.UTF8);
            ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar('"'+fns+'" "'+fnt+'"'), true);
            end;
        end;
    finally
      engine.free;
    end;
  finally
    stopRunning;
    if FWantClose then
      Close;
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

procedure TTransformerForm.btnOpenScriptClick(Sender: TObject);
begin
  if cbxScript.ItemIndex > -1 then
    openWorkspaceFile(cbxScript.Items.Objects[cbxScript.ItemIndex] as TWorkspaceFile);
end;

procedure TTransformerForm.btnOpenSourceClick(Sender: TObject);
begin
  if cbxSource.ItemIndex > -1 then
    openWorkspaceFile(cbxSource.Items.Objects[cbxScript.ItemIndex] as TWorkspaceFile);
end;

procedure TTransformerForm.btnOpenTargetClick(Sender: TObject);
begin
  if cbxTarget.ItemIndex > -1 then
    openWorkspaceFile(cbxTarget.Items.Objects[cbxTarget.ItemIndex] as TWorkspaceFile);
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
  DoSave('Debug FHIRPath');
  runFHIRPath(true);
end;

procedure TTransformerForm.btnPathGoClick(Sender: TObject);
begin
  DoSave('Execute FHIRPath');
  runFHIRPath(false);
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
begin
  context := TFHIRTransformerContext.Create(TFHIRFactoryR4.create);
  try
    context.loadFromCache(FCache);
    engine := TFHIRPathEngine.Create(context.Link, nil); // todo: do we need UCUM?
    try
      case FEditor.id.format of
        fmtV2: raise EFslException.Create('Not done yet');
        fmtCDA: b := parseCDA(context);
        fmtResource: b := parseResource(context);
        fmtJS: raise EFslException.Create('Not supported - you cannot use FHIRPath with this type');
        fmtMap: raise EFslException.Create('Not done yet');
        fmtTemplate: raise EFslException.Create('Not supported - you cannot use FHIRPath with this type');
      end;
      try
        lbFHIRPathOutcomes.Items.Clear;
        FPathSelection.Clear;
        try
          if debug then
            RunPathDebugger(self, context, GetFPDebuggerSetting, setFPDebuggerSetting, getFPDebuggerSettingStr, setFPDebuggerSettingStr,
            context.Factory, nil, b, edtFHIRPath.text, ffXml, types, ol)
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
begin
  executeTransform(false);
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
end;

procedure TTransformerForm.cbxEventTypeChange(Sender: TObject);
  procedure loadSource(srcList, scrList, dstList : TFslList<TWorkspaceFile>; caption : String);
  var
    f : TWorkspaceFile;
  begin
    lblScript.Caption := caption;
    cbxScript.Items.Clear;
    for f in scrlist do
      cbxScript.Items.AddObject(f.title, f);
    cbxScript.ItemIndex := cbxScript.Items.IndexOf(FWorkspace.Script);
    if (cbxScript.ItemIndex = -1) and (cbxScript.Items.Count > 0) then
      cbxScript.ItemIndex := 0;

    cbxSource.Items.Clear;
    for f in srclist do
      cbxSource.Items.AddObject(f.title, f);
    cbxSource.ItemIndex := cbxSource.Items.IndexOf(FWorkspace.Source);
    if (cbxSource.ItemIndex = -1) and (cbxSource.Items.Count > 0) then
      cbxSource.ItemIndex := 0;

    cbxTarget.Items.Clear;
    for f in dstlist do
      cbxTarget.Items.AddObject(f.title, f);
    cbxTarget.ItemIndex := cbxTarget.Items.IndexOf(FWorkspace.Target);
    if (cbxTarget.ItemIndex = -1) and (cbxTarget.Items.Count > 0) then
      cbxTarget.ItemIndex := 0;

    btnExecute.enabled := not FRunning and (cbxScript.ItemIndex > -1) and (cbxSource.ItemIndex > -1) and ((cbxOutcome.ItemIndex = 0) or (cbxTarget.ItemIndex > -1));
    mnuExecute.Enabled := btnExecute.enabled;
    btnRunNoDebug.enabled := not FRunning and (cbxScript.ItemIndex > -1) and (cbxSource.ItemIndex > -1) and ((cbxOutcome.ItemIndex = 0) or (cbxTarget.ItemIndex > -1));
    mnuRunNoDebug.enabled := btnRunNoDebug.enabled;
  end;
begin
   case cbxEventType.ItemIndex of
    0: LoadSource(FWorkspace.messages, FWorkspace.scripts, FWorkspace.resources, 'Scripts');
    1: LoadSource(FWorkspace.documents, FWorkspace.maps, FWorkspace.resources, 'Maps');
  end;
  if not Floading then
  begin
    FWorkspace.EventType := cbxEventType.ItemIndex;
    FWorkspace.Source := cbxSource.Text;
    FWorkspace.Script := cbxScript.Text;
    FWorkspace.Target := cbxTarget.Text;
    FWorkspace.Outcome := TTrasnformOutcomeMode(cbxOutcome.ItemIndex);
    FWorkspace.Save;
  end;
end;

procedure TTransformerForm.cbxOutcomeChange(Sender: TObject);
begin
  cbxTarget.Enabled := cbxOutcome.ItemIndex > 0;
  FWorkspace.Outcome := TTrasnformOutcomeMode(cbxOutcome.ItemIndex);
end;

procedure TTransformerForm.cbxScriptChange(Sender: TObject);
begin
  if not Floading then
  begin
    FWorkspace.Script := cbxScript.Text;
    FWorkspace.Save;
  end;
  btnExecute.enabled := not FRunning and (cbxScript.ItemIndex > -1) and (cbxSource.ItemIndex > -1);
  btnRunNoDebug.enabled := not FRunning and (cbxScript.ItemIndex > -1) and (cbxSource.ItemIndex > -1);
end;

procedure TTransformerForm.cbxSourceChange(Sender: TObject);
begin
  if not Floading then
  begin
    FWorkspace.Source := cbxSource.Text;
    FWorkspace.Save;
  end;
  btnExecute.enabled := not FRunning and (cbxScript.ItemIndex > -1) and (cbxSource.ItemIndex > -1);
  btnRunNoDebug.enabled := not FRunning and (cbxScript.ItemIndex > -1) and (cbxSource.ItemIndex > -1);
end;

function TTransformerForm.parseCDA(context: TFHIRWorkerContext): TFHIRObject;
var
  ss : TStringStream;
begin
  ss := TStringStream.Create(FEditor.memo.RawText, TEncoding.UTF8);
  try
    result := TFHIRMMManager.parse(context, ss, ffXml);
  finally
    ss.Free;
  end;
end;

function TTransformerForm.parseResource(context: TFHIRWorkerContext): TFHIRObject;
var
  p : TFHIRParser;
begin
  if isXml(FEditor.memo.RawText) then
    p := TFHIRXmlParser.Create(context.link, 'en')
  else
    p := TFHIRJsonParser.Create(context.link, 'en');
  try
    result := p.parseResource(FEditor.memo.RawText);
  finally
    p.Free;
  end;
end;

procedure TTransformerForm.pgTabsChange(Sender: TObject);
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
    mnuPretty.Enabled := false;
    mnuDense.Enabled := false;
    mnuEOL.Enabled := false;
    mnuRemoveEmptyLines.Enabled := false;
    mnuCompile.Enabled := false;
    tbCompile.Enabled := false;

    Caption := 'FHIR Transformer IDE';
    pnlStatus.Panels[spMode].Text := '';
    pnlStatus.Panels[spCaretPos].Text := '';
  end
  else
  begin
    FEditor := editorForTab(pgTabs.ActivePage); // (pgTabs.ActivePage.Controls[0] as TScintEdit).context as TEditorInformation;
    Caption := pgTabs.ActivePage.Caption+'- FHIR Transformer';
    mnuSearch.Enabled := true;
    mnuFindNext.Enabled := true;
    mnuReplace.Enabled := not FEditor.memo.ReadOnly;
    mnuGoto.Enabled := true;
    mnuCompare.Enabled := FEditor.id.format in [fmtV2, fmtCDA, fmtResource, fmtMap];
    tbCompare.Enabled := mnuCompare.Enabled;
    mnuPretty.Enabled := not FRunning and (FEditor.id.format in [fmtCDA, fmtResource]);
    mnuDense.Enabled := not FRunning and (FEditor.id.format in [fmtCDA, fmtResource]);
    mnuEOL.Enabled := not FRunning;
    mnuWindowsEOL.Checked := false;
    mnuUnixEOL.Checked := false;
    mnuCompile.Enabled := true;
    tbCompile.Enabled := true;
    case FEditor.memo.LineEndings of
      sleCRLF: mnuWindowsEOL.Checked := true;
      sleLF: mnuUnixEOL.Checked := true;
    end;
    mnuRemoveEmptyLines.Enabled := not FRunning;
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

procedure TTransformerForm.DebugTransform(sender: TObject; info: TFHIRStructureMapDebugContext);
var
  bpi : TBreakPointInfo;
  f : TWorkspaceFile;
begin
  if (info.group = nil) then
    exit; // don't break for the root map
  f := FWorkspace.findFileByParsedObject(info.map);
  if (info.status = dsRunToBreakpoint) then
  begin
    if (f = nil) or not f.hasBreakPoint(info.line-1, bpi) or (info.target <> nil) then
      exit;
  end;

  // set up viewing the info
  FDebugInfo := info.Link;
  try
    ShowCallStack(f);
    ShowVars(FDebugInfo.variables);
    setDebugStatus(true);
    try
      FStepOutcome := DBG_STOPPED;
      while FStepOutcome = DBG_STOPPED do
        Application.ProcessMessages;
      case FStepOutcome of
        DBG_EXECUTE: FDebugInfo.status := dsRunToBreakpoint;
        DBG_STEP_OVER: FDebugInfo.status := dsStepOver;
        DBG_STEP_OUT: FDebugInfo.status := dsStepOut;
        DBG_STEP_INTO: FDebugInfo.status := dsStepIn;
        DBG_STOP: FDebugInfo.status := dsRunToBreakpoint;
        DBG_CLOSING: FDebugInfo.status := dsRunToBreakpoint;
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

procedure TTransformerForm.DoCompiled(sender: TConversionEngine; f: TWorkspaceFile; checkBreakpointProc: TCheckBreakpointEvent);
var
  bpi : TBreakPointInfo;
  valid : boolean;
  editor : TEditorInformation;
begin
  editor := editorForFile(f);
  for bpi in f.BreakPoints do
  begin
    bpi.invalid := not checkBreakpointProc(bpi.line);
    UpdateLineMarkers(editor, bpi.line);
  end;
end;

function TTransformerForm.DoSave(command : String) : boolean;
var
  dirty : boolean;
  form : TListSelectorForm;
  i : integer;
  e : TEditorInformation;
begin
  dirty := false;
  result := false;
  FIni.WriteString('debug', 'FHIRPath', edtFHIRPath.Text);
  if FIni.ReadBool('Workspace', 'AutoSave', false) then
  begin
    mnuSaveAllClick(nil);
    for i := 1 to pgTabs.PageCount - 1 do
    begin
      e := editorForTab(pgTabs.Pages[i]);
      FWorkspace.OpenFile(e.id);
    end;
    exit;
  end;

  form := TListSelectorForm.Create(self);
  try
    form.Caption := 'Unsaved Content found. Which files do you want to save?';
    form.okWithNoneSelected := true;
    form.Verb := command;
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
      FIni.WriteBool('Workspace', 'AutoSave', true);
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

function TTransformerForm.engineGetSource(sender: TConversionEngine; f: TWorkspaceFile): TStream;
var
  editor : TEditorInformation;
begin
  editor := findWorkspaceFile(f);
  if editor = nil then
    result := nil
  else
    result := TStringStream.Create(editor.memo.RawText);
end;

procedure TTransformerForm.engineLog(sender: TConversionEngine; message: String);
begin
  mConsole.Lines.add(message);
  mConsole.perform(EM_LINESCROLL, 0, mConsole.Lines.Count);
end;

procedure TTransformerForm.engineStatus(sender: TConversionEngine; message: String);
begin
  raise Exception.Create('Not Done yet');
end;

procedure TTransformerForm.estException1Click(Sender: TObject);
begin
  raise Exception.Create('Test Exception');
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
      mnuRename.Enabled := not FRunning;
      mnuDrop.Enabled := not FRunning;
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
    s := makeAbsolutePath(e.id.filename, FWorkspace.folder);
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
  FStack := TFslList<TFHIRStructureMapDebugContext>.create;

  FCache := TResourceMemoryCache.create;
  FCache.Packages := ['hl7.fhir.core#4.0.0', 'hl7.fhir.cda#0.0.1'];
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
  for i := 1 to pgTabs.PageCount - 1 do
    editorForTab(pgTabs.Pages[i]).Free;
  for i := mnuWorkspace.Count - 1 downto 1 do
    mnuWorkspace.Items[I].Free;
  FIni.Free;
  FCache.Free;
  FWorkspace.Free;
  FPathSelection.Free;
  FStack.Free;
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
  if (ps.LineStart <> -1) then
  begin
    sel.StartPos := FEditor.memo.GetPositionFromLineColumn(ps.lineStart, ps.colStart);
    sel.EndPos := FEditor.memo.GetPositionFromLineColumn(ps.lineEnd, ps.colEnd);
    FEditor.memo.Selection := sel;
    FEditor.memo.ScrollCaretIntoView;
  end;
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
  FWorkspace.Free;
  FWorkspace := proj;
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
    vtWorkspace.RootNodeCount := 6;
    cbxOutcome.ItemIndex := ord(FWorkspace.Outcome);
    if cbxOutcome.ItemIndex = -1 then
      cbxOutcome.ItemIndex := 0;
    cbxOutcomeChange(nil);
    cbxEventType.ItemIndex := FWorkspace.EventType;
    cbxEventTypeChange(nil);
    files := FWorkspace.listOpenFiles;
    try
      for f in files do
        openWorkspaceFile(f);
    finally
      files.Free;
    end;
    pgTabsChange(self);
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
    f := TWorkspaceFile.Create(s, fmt);
    category.Add(f);
    FWorkspace.save;
    vtWorkspace.RootNodeCount := 0;
    vtWorkspace.RootNodeCount := 6;
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
    HideError;
  if FEditor.memo.ReadOnly then
    pnlStatus.Panels[spMode].Text := 'LOCK'
  else if FEditor.memo.InsertMode then
    pnlStatus.Panels[spMode].Text := 'INS'
  else
    pnlStatus.Panels[spMode].Text := 'OVR';
  pnlStatus.Panels[spCaretPos].Text := Format('%4d:%4d', [FEditor.memo.CaretLine + 1, FEditor.memo.CaretColumnExpanded + 1]);

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
      mrYes : DeleteFile(makeAbsolutePath(f.filename, FWorkspace.folder));
      mrNo : saveWorkspaceFile(editorForFile(f));
    end;
    list.Remove(f);
    closeWorkspaceFile(editorForFile(f), false);
    FWorkspace.save;
    vtWorkspace.RootNodeCount := 0;
    vtWorkspace.RootNodeCount := 6;
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
    makeNewFile(nodeCaption(node.Index), ExtractFileExt(f.filename), FileToString(makeAbsolutePath(f.filename, FWorkspace.folder), TEncoding.UTF8), f.format, list);
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
    f := TWorkspaceFile.Create(s, fmt);
    case fmt of
      fmtV2: category := FWorkspace.messages;
      fmtResource: category := FWorkspace.resources;
      fmtJS: category := FWorkspace.scripts;
      fmtMap: category := FWorkspace.maps;
      fmtTemplate: category := FWorkspace.templates;
      fmtCDA: category := FWorkspace.documents;
    end;
    category.Add(f);
    FWorkspace.save;
    vtWorkspace.RootNodeCount := 0;
    vtWorkspace.RootNodeCount := 6;
    openWorkspaceFile(f);
  end;
end;

procedure TTransformerForm.mnuOptionsClick(Sender: TObject);
begin
  TransformerOptionsForm := TTransformerOptionsForm.create(self);
  try
    TransformerOptionsForm.ini := FIni;
    TransformerOptionsForm.ShowModal;
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

procedure TTransformerForm.mnuPrettyClick(Sender: TObject);
begin
  case FEditor.id.format of
    fmtV2: raise Exception.Create('Not Supported Yet');
    fmtCDA: FEditor.memo.RawText := makeXmlPretty(FEditor.memo.RawText);
    fmtResource: raise Exception.create('Not Supported Yet');
    fmtJS: raise Exception.create('Not Supported Yet');
    fmtMap: raise Exception.create('Not Supported');
    fmtTemplate: raise Exception.create('Not Supported Yet');
  end;
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
      RenameFile(makeAbsolutePath(f.filename, FWorkspace.folder), sdNew.FileName);
      s := makeRelativePath(sdNew.FileName, FWorkspace.folder);
      f.filename := s;
      FWorkspace.save;
      vtWorkspace.RootNodeCount := 0;
      vtWorkspace.RootNodeCount := 6;
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

function TTransformerForm.nodeCaption(i: integer): String;
begin
  case i of
    0: result := 'Message';
    1: result := 'Document';
    2: result := 'Resource';
    3: result := 'Script';
    4: result := 'Map';
    5: result := 'Template';
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
      pmRename.Enabled := not FRunning;
      pmDrop.Enabled := not FRunning;
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
  dbg : TFHIRStructureMapDebugContext;
  f :  TWorkspaceFile;
  sel : TScintRange;
begin
  p := vtCallStack.GetNodeData(FCallStackSelected);
  dbg := p.obj as TFHIRStructureMapDebugContext;
  if (dbg = nil) then
    exit;
  if (dbg.focus.LocationStart.line <> -1) then
  begin
    f := FWorkspace.findFileByParsedObject(dbg.map);
    if f <> nil then
    begin
      openWorkspaceFile(f);
      sel.StartPos := FEditor.memo.GetPositionFromLineColumn(dbg.focus.LocationStart.line-1, dbg.focus.LocationStart.col-1);
      sel.EndPos := FEditor.memo.GetPositionFromLineColumn(dbg.focus.LocationEnd.line-1, dbg.focus.LocationEnd.col-1);
      FEditor.memo.Selection := sel;
      FEditor.memo.ScrollCaretIntoView;
    end;
  end;
  ShowVars(dbg.variables);
end;

procedure TTransformerForm.vtCallStackDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  p : PTreeDataPointer;
  dbg : TFHIRStructureMapDebugContext;
begin
  p := vtWorkspace.GetNodeData(Node);
  dbg := p.obj as TFHIRStructureMapDebugContext;
  if dbg.target <> nil then
    TargetCanvas.Font.Color := clMaroon
  else if dbg.rule <> nil then
    TargetCanvas.Font.Color := clBlack
  else if dbg.group <> nil then
    TargetCanvas.Font.Color := clNavy
  else
    TargetCanvas.Font.Color := clGreen;
end;

procedure TTransformerForm.vtCallStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
  dbg : TFHIRStructureMapDebugContext;
begin
  p := vtWorkspace.GetNodeData(Node);
  dbg := p.obj as TFHIRStructureMapDebugContext;
  CellText := dbg.Name;
end;

procedure TTransformerForm.vtCallStackInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p : PTreeDataPointer;
begin
  p := vtWorkspace.GetNodeData(Node);
  p.obj := FStack[Node.Index];
end;

procedure TTransformerForm.vtCallStackRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FCallStackSelected := Nil;
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
  if (p.obj is TVariable) then
    n := (p.obj as TVariable).name
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
  v : TVariable;
begin
  p := vtVars.GetNodeData(FVarsSelected);
  v := p.obj as TVariable;
  ShowVariable(v);
end;

procedure TTransformerForm.vtVarsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  p : PTreeDataPointer;
  v : TVariable;
begin
  p := vtWorkspace.GetNodeData(Node);
  v := p.obj as TVariable;
  if v.mode = vmINPUT then
    TargetCanvas.Font.Color := clNavy
  else
    TargetCanvas.Font.Color := clMaroon;
end;

procedure TTransformerForm.vtVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
  v : TVariable;
begin
  p := vtWorkspace.GetNodeData(Node);
  v := p.obj as TVariable;
  CellText := v.summary(true);
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
    end
  else
    ImageIndex := 15;
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
    end;
    (p.obj as TFslList<TWorkspaceFile>).TagObject := TObject(node);
    InitialStates := [ivsHasChildren, ivsExpanded];
  end
  else
  begin
    pp := vtWorkspace.GetNodeData(parentNode);
    p.obj := (pp.obj as TFslList<TWorkspaceFile>)[Node.Index];
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
  editor : TScintEdit;
  s : String;
  bpi : TBreakPointInfo;
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
    end;
    editor := TIDEScintEdit.create(tab);
    editor.Parent := tab;
    editor.Font.Assign(mConsole.Font);
    editor.OnChange := memoChange;
    editor.OnUpdateUI := memoStatusChange;
    editor.OnKeyDown := FormKeyDown;
    editor.OnMarginClick := MemoMarginClick;
    editor.LineNumbers := true;
    editor.PopupMenu := pmEditor;
    editor.Align := alClient;
    case f.format of
      fmtV2 :  editor.Styler := TV2Styler.Create(self);
      fmtCDA  : editor.Styler := TCDAStyler.Create(self);
      fmtResource:
        if isXml(editor.rawText) then
          editor.Styler := TXmlStyler.Create(self)
        else
          editor.Styler := TJsonStyler.Create(self);
      fmtJS : editor.Styler := TJSStyler.Create(self);
      fmtMap : editor.Styler := TFHIRMapStyler.Create(self);
      fmtTemplate : editor.Styler := TLiquidStyler.Create(self);
    end;

    result := TEditorInformation.create;
    editor.context := result;
    result.memo := editor;
    result.id := f.link;
    result.tab := tab;
    FEditor := result;

    s := makeAbsolutePath(f.filename, FWorkspace.folder);
    editor.ReadOnly := FileIsReadOnly(s);
    result.FileTime := TFile.GetLastWriteTimeUtc(s);
    result.fileIsReadOnly := editor.ReadOnly;
    editor.Lines.LoadFromFile(s);
    editor.ClearUndo;
    editor.CaretLine := f.row;
    result.isDirty := false;
    result.ErrorLine := -1;
    result.StepLine := -1;
    for bpi in f.BreakPoints do
      UpdateLineMarkers(result, bpi.line);
    pgTabs.ActivePage := tab;
    pgTabsChange(pgTabs);
  end;
end;

procedure TTransformerForm.saveWorkspaceFile(editor : TEditorInformation);
var
  s, v : String;
begin
  if editor = nil then
    exit;
  s := makeAbsolutePath(editor.id.filename, FWorkspace.folder);
  editor.memo.Lines.SaveToFile(s);
  editor.FileTime := TFile.GetLastWriteTimeUtc(s);
  editor.isDirty := false;
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
  FRunning := true;
  for i := 1 to pgTabs.PageCount - 1 do
    editorForTab(pgTabs.Pages[i]).readOnly := true;
  cbxEventType.Enabled := false;
  cbxScript.Enabled := false;
  cbxSource.Enabled := false;
  cbxEventTypeChange(nil);
  cbxScriptChange(nil);
  cbxSourceChange(nil);
  pgTabsChange(nil);
end;

procedure TTransformerForm.stopRunning;
var
  i : integer;
begin
  FRunning := false;
  for i := 1 to pgTabs.PageCount - 1 do
    editorForTab(pgTabs.Pages[i]).readOnly := false;
  cbxEventType.Enabled := true;
  cbxScript.Enabled := true;
  cbxSource.Enabled := true;
  cbxEventTypeChange(nil);
  cbxScriptChange(nil);
  cbxSourceChange(nil);
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
  UpdateLineMarkers(FEditor, Line);
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
  editor.Free;
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

function TTransformerForm.rewriteCDA(src, title : String) : String;
var
  stream, outStream : TStringStream;
  elem : TFHIRMMElement;
  engine : TCDAConversionEngine;
begin
  engine := TCDAConversionEngine.create;
  try
    engine.cache := FCache.Link;
    engine.workspace := FWorkspace.link;
    engine.OnWantSource := engineGetSource;
    engine.OnStatus := engineStatus;
    engine.OnLog := engineLog;
    engine.load;
    stream := TStringStream.Create(src, TEncoding.UTF8);
    try
      elem := TFHIRMMManager.parse(engine.Context, stream, ffXml);
      try
        outStream := TStringStream.Create;
        try
          TFHIRMMManager.compose(engine.Context, elem, outStream, ffXml, OutputStylePretty);
          result := outStream.DataString;
        finally
          outStream.Free;
        end;
      finally
        elem.free;
      end;
    finally
      stream.free;
    end;
  finally
    engine.free;
  end;
end;

function TTransformerForm.rewriteV2(src, title : String) : String;
var
  utils : TFHIRStructureMapUtilities;
  map : TFHIRStructureMap;
begin
   utils := TFHIRStructureMapUtilities.Create(nil, nil, nil, nil);
   try
     map := utils.parse(src, title);
     try
       result := utils.render(map);
     finally
       map.Free;
     end;
   finally
     utils.Free;
   end;
end;

procedure TTransformerForm.mnuChooseWorkspaceClick(Sender: TObject);
begin
  fd.DefaultFolder := FWorkspace.folder;
  if fd.Execute then
    LoadWorkspace(TWorkspace.Create(fd.FileName));
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
    fmtV2: raise Exception.Create('Not Supported Yet');
    fmtCDA:
      begin
      fmt := 2;
      output := rewriteCDA(src, FEditor.id.title);
      end;
    fmtResource: raise Exception.create('Not Supported Yet');
    fmtJS: raise Exception.create('Not Supported Yet');
    fmtMap: output := rewriteV2(src, FEditor.id.title);
    fmtTemplate: raise Exception.create('Not Supported Yet');
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
end;

procedure TTransformerForm.mnuCompileClick(Sender: TObject);
begin
  SetErrorLine(-1);
  status(clNavy, 'Checking '+FEditor.id.title);
  try
    case FEditor.id.format of
      fmtV2: checkV2(FEditor.memo.RawText, FEditor.id.title);
      fmtCDA: checkCDA(FEditor.memo.RawText, FEditor.id.title);
      fmtResource: checkResource(FEditor.memo.RawText, FEditor.id.title);
      fmtJS: checkJS(FEditor.memo.RawText, FEditor.id.title);
      fmtMap: checkMap(FEditor.memo.RawText, FEditor.id.title, FEditor.id);
      fmtTemplate: checkTemplate(FEditor.memo.RawText, FEditor.id.title);
    end;
    status(clGreen, FEditor.id.title+' is syntactically valid at '+FormatDateTime('c', now));
    MessageBeep(SOUND_SYSTEM_INFORMATION);
  except
    on e : EParserException do
    begin
      SetErrorLine(e.Line-1);
      status(clMaroon, 'Error Compiling: '+e.message);
      MessageBeep(SOUND_SYSTEM_ERROR)
    end;
    on e : Exception do
    begin
      status(clMaroon, 'Error Compiling: '+e.message);
      MessageBeep(SOUND_SYSTEM_ERROR);
    end;
  end;
end;

procedure TTransformerForm.Rewrite1Click(Sender: TObject);
begin
  case FEditor.id.format of
    fmtV2: raise Exception.Create('Not Supported Yet');
    fmtCDA: FEditor.memo.RawText := rewriteCDA(FEditor.memo.RawText, FEditor.id.title);
    fmtResource: raise Exception.create('Not Supported Yet');
    fmtJS: raise Exception.create('Not Supported Yet');
    fmtMap: FEditor.memo.RawText := rewriteV2(FEditor.memo.RawText, FEditor.id.title);
    fmtTemplate: raise Exception.create('Not Supported Yet');
  end;
end;

procedure TTransformerForm.checkV2(src, title : String);
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TTransformerForm.ClearCallStack;
begin
  FStack.Clear;
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

procedure TTransformerForm.checkCDA(src, title : String);
begin
  rewriteCDA(src, title); // just ignore output
end;

procedure TTransformerForm.checkResource(src, title : String);
var
  p : TFHIRParser;
begin
  if isXml(src) then
    p := TFHIRXmlParser.Create(nil, 'en')
  else
    p := TFHIRJsonParser.Create(nil, 'en');
  try
    p.parseResource(src).Free;
  finally
    p.free;
  end;
end;

procedure TTransformerForm.checkJS(src, title : String);
var
  js : TJavascript;
begin
  js := TJavascript.Create(ExtractFilePath(ParamStr(0)));
  try
    js.compile(src, title);
  finally
    js.Free;
  end;
end;

procedure TTransformerForm.checkMap(src, title : String; f : TWorkspaceFile);
var
  utils : TFHIRStructureMapUtilities;
  map : TFHIRStructureMap;
  mbpr : TMapbreakpointResolver;
begin
  utils := TFHIRStructureMapUtilities.Create(nil, nil, nil, nil);
  try
    map := utils.parse(src, title);
    try
      mbpr := TMapbreakpointResolver.create(map.Link);
      try
        DoCompiled(nil, f, mbpr.checkBreakPoint);
      finally
        mbpr.free;
      end;
    finally
      map.free;
    end;
  finally
    utils.Free;
  end;
end;

procedure TTransformerForm.checkTemplate(src, title : String);
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TTransformerForm.SetErrorLine(ALine: Integer);
var
  OldLine: Integer;
begin
  if FEditor.ErrorLine <> ALine then
  begin
    OldLine := FEditor.ErrorLine;
    FEditor.ErrorLine := ALine;
    if OldLine >= 0 then
      UpdateLineMarkers(FEditor, OldLine);
    if FEditor.ErrorLine >= 0 then
    begin
      FEditor.memo.CaretLine := FEditor.ErrorLine;
      FEditor.memo.ScrollCaretIntoView;
      UpdateLineMarkers(FEditor, FEditor.ErrorLine);
    end;
  end;
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
        UpdateLineMarkers(FStepEditor, OldLine);
    end;
    FStepEditor := editor;
    if FStepEditor <> nil then
    begin
      FStepEditor.StepLine := ALine;
      if FStepEditor.StepLine >= 0 then
      begin
        FStepEditor.memo.CaretLine := FStepEditor.StepLine;
        FStepEditor.memo.ScrollCaretIntoView;
        UpdateLineMarkers(FStepEditor, FStepEditor.StepLine);
      end;
    end;
  end;
end;

procedure TTransformerForm.ShowCallStack(f : TWorkspaceFile);
var
  dbg : TFHIRStructureMapDebugContext;
begin
  Fstack.Clear;
  FCallStackSelected := nil;
  dbg := FDebugInfo;
  while dbg <> nil do
  begin
    FStack.add(dbg.link);
    dbg := dbg.Parent;
  end;
  vtCallStack.RootNodeCount := FStack.Count;
  if Fstack.Count > 0 then
  begin
    openWorkspaceFile(f);
    SetStepLine(FEditor, FDebugInfo.focus.LocationStart.line-1);
  end;
end;

procedure TTransformerForm.ShowVariable(variable: TVariable);
begin
  FVariable := variable;
  vtVarDetails.RootNodeCount := 0;
  vtVarDetails.RootNodeCount := 1;
end;

procedure TTransformerForm.ShowVars(vars : TVariables);
begin
  FVariables := vars;
  vtVars.RootNodeCount := 0;
  vtVars.RootNodeCount := FVariables.Count;
  FVarsSelected := nil;
  ClearVariable;
end;

procedure TTransformerForm.HideError;
begin
  SetErrorLine(-1);
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
  if (MarginNumber = 1) and (FEditor.id.format in [fmtJS, fmtMap, fmtTemplate]) then
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
  UpdateLineMarkers(FEditor, FirstAffectedLine);
end;

procedure TTransformerForm.UpdateLineMarkers(editor : TEditorInformation; const Line: Integer);
var
  bpi : TBreakPointInfo;
begin
  if Line >= Editor.Memo.Lines.Count then
    Exit;

  { Delete all markers on the line. To flush out any possible duplicates,
    even the markers we'll be adding next are deleted. }
  if Editor.Memo.GetMarkers(Line) <> [] then
    Editor.Memo.DeleteAllMarkersOnLine(Line);

  if editor.StepLine = Line then
    Editor.Memo.AddMarker(Line, mmLineStep)
  else if Editor.ErrorLine = Line then
    Editor.Memo.AddMarker(Line, mmLineError)
  else if editor.id.hasBreakPoint(line, bpi) then
    if (bpi.invalid) then
      Editor.Memo.AddMarker(Line, mmLineBreakpointBad)
    else
      Editor.Memo.AddMarker(Line, mmLineBreakpoint);
end;

procedure TTransformerForm.updateWorkspaceFile(editor: TEditorInformation; src: String);
begin
  editor.memo.RawText := src;
end;

procedure TTransformerForm.UpdateAllLineMarkers;
var
  Line: Integer;
begin
  for Line := 0 to FEditor.Memo.Lines.Count-1 do
    UpdateLineMarkers(FEditor, Line);
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


