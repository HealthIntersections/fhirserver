unit frm_main;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, Math,
  ComCtrls, ActnList, StdActns, IniFiles, Clipbrd, Buttons, StdCtrls, SynEdit,
  lclintf, ValEdit, LCLType, FPImage,

  IdOpenSSLLoader,

  fsl_base, fsl_utilities, fsl_stream, fsl_threads, fsl_fpc, fsl_logging, fsl_http, fsl_openssl, fsl_lang, fsl_json, fsl_fetcher,

  fhir_objects, fhir_client, fhir_factory, fhir_oauth, fhir_parser, fhir_context,
  fui_lcl_managers,

  ftk_context, ftk_store_temp, ftk_utilities, ftk_terminology_service, ftk_fhir_context, ftk_constants, ftk_version,
  ftk_store, ftk_store_files, ftk_store_internal, ftk_store_http, ftk_store_server,
  ftk_factory, ftk_search, ftk_serverlist, ftk_project_tree, ftk_worker_server,

  fui_lcl_cache, frm_file_format, frm_settings, frm_about, dlg_edit_changes, frm_server_settings, frm_oauth,
  frm_format_chooser, frm_clip_chooser, frm_file_deleted, frm_file_changed, frm_project_editor, frm_view_manager, Types,
  dlg_new_resource, dlg_open_url, dlg_scanner, dlg_upgrade, dlg_clipboard_process;

type
  {$IFDEF WINDOWS}
   { TPageControl }
   TPageControl = class(ComCtrls.TPageControl)
   private
     const btnSize = 10;
   protected
     procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
     procedure PaintWindow(DC: HDC); override;
   end;
   {$ENDIF}

{ TMainToolkitForm }

  TMainToolkitForm = class(TForm)
    actExecuteDebug: TAction;
    actExecuteFinish: TAction;
    actExecuteParameters: TAction;
    actExecuteRun: TAction;
    actExecuteStep: TAction;
    actExecuteStepInto: TAction;
    actExecuteStepOut: TAction;
    actExecuteStop: TAction;
    actConnectToServer: TAction;
    actionEditPasteProcessed: TAction;
    actionFileOpenQRCode: TAction;
    actionHelpWelcomePage: TAction;
    actionViewManager: TAction;
    actionViewsCopyLog: TAction;
    actionViewsOpenLog: TAction;
    actionCreateNewProject: TAction;
    actionEditPasteEscaped: TAction;
    actionNewEditorJWT: TAction;
    actionNewEditorDicom: TAction;
    actionNewEditorHTML: TAction;
    actionNewEditorJS: TAction;
    actionNewEditorMD: TAction;
    actionNewEditorText: TAction;
    actionNewEditorIni: TAction;
    actionNewEditorLiquid: TAction;
    actionNewEditorJSON: TAction;
    actionNewEditorXML: TAction;
    actionNewEditorCDA: TAction;
    actionNewEditorV2: TAction;
    actionNewEditorFHIR: TAction;
    actionEditPasteNewFile: TAction;
    actionEditReview: TAction;
    actionEditFindNext: TAction;
    actionEditFindPrev: TAction;
    actionEditFind: TAction;
    actionToolsSideBySideMode: TAction;
    actionViewFormDesigner: TAction;
    actionViewsClearLog: TAction;
    actionPagesMoveRight: TAction;
    actionPagesMoveLeft: TAction;
    actionPagesMoveFarleft: TAction;
    actionPagesMoveFarRIght: TAction;
    actionPagesCloseRight: TAction;
    actionPagesCloseAll: TAction;
    actionPagesCloseLeft: TAction;
    actionToolsPackageManager: TAction;
    actionHelpCheckUpgrade: TAction;
    actionhelpAbout: TAction;
    actionToolsOptions: TAction;
    actionViewTasks: TAction;
    actionViewExpressionEditor: TAction;
    actionViewVariables: TAction;
    actionViewInspector: TAction;
    actionViewPackages: TAction;
    actionZoomIn: TAction;
    actionZoomOut: TAction;
    actionViewEditor: TAction;
    actionViewProjectManager: TAction;
    actionViewServers: TAction;
    actionViewSearch: TAction;
    actionViewMessages: TAction;
    actionViewLog: TAction;
    actionViewStack: TAction;
    actionEditPasteFormat: TAction;
    actionEditCopyFilename: TAction;
    actionCopyFileTitle: TAction;
    actionCopyFilePath: TAction;
    actionEditBeginEnd: TAction;
    actionEditRedo: TAction;
    actionFilePrint: TAction;
    actionFileClose: TAction;
    actionFileSaveAll: TAction;
    actionFileSave: TAction;
    actionFileManageFolder: TAction;
    actionFileOpenUrl: TAction;
    actionFileManageRename: TAction;
    actionFileManageCopy: TAction;
    actionFileManageDelete: TAction;
    actionFileManageReload: TAction;
    actList: TActionList;
    actionEditCopy: TEditCopy;
    actionEditCut: TEditCut;
    actionEditDelete: TEditDelete;
    actionEditPaste: TEditPaste;
    actionEditSelectAll: TEditSelectAll;
    actionEditUndo: TEditUndo;
    actionFileExit: TFileExit;
    actionFileNew: TAction;
    actionFileOpen: TAction;
    actionFileSaveAs1: TAction;
    actionHelpContent: THelpContents;
    btnOpenLog: TSpeedButton;
    btnSearch: TBitBtn;
    chkCase: TCheckBox;
    chkWholeWord: TCheckBox;
    chkhideHintsAndWarnings: TCheckBox;
    chkTaskEngines: TCheckBox;
    chkCurrrentFileOnly: TCheckBox;
    cbxSearch: TComboBox;
    cbxSearchType: TComboBox;
    cbxSearchScope: TComboBox;
    imgMain: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lvServers: TListView;
    lvSearch: TListView;
    lvTasks: TListView;
    lvMessages: TListView;
    MainMenu1: TMainMenu;
    mConsole: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem100: TMenuItem;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem103: TMenuItem;
    MenuItem111: TMenuItem;
    MenuItem112: TMenuItem;
    MenuItem113: TMenuItem;
    MenuItem114: TMenuItem;
    MenuItem115: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem118: TMenuItem;
    MenuItem119: TMenuItem;
    MenuItem120: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    N15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem40: TMenuItem;
    N14: TMenuItem;
    MenuItem116: TMenuItem;
    N13: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem105: TMenuItem;
    MenuItem106: TMenuItem;
    MenuItem107: TMenuItem;
    MenuItem108: TMenuItem;
    MenuItem109: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem110: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    mnuFileExit: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    mnuEdit: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    MenuItem99: TMenuItem;
    mnuApple: TMenuItem;
    MenuItem95: TMenuItem;
    N12: TMenuItem;
    MenuItem94: TMenuItem;
    N10: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    mnuSearchGo: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem81: TMenuItem;
    mnuLVGo: TMenuItem;
    mnuLVCopy: TMenuItem;
    mnuLVCopyAll: TMenuItem;
    mnuContent: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    mnuPagesClose: TMenuItem;
    mnuPagesCloseLeft: TMenuItem;
    mnuPagesCloseRight: TMenuItem;
    mnuPagesCloseOthers: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem80: TMenuItem;
    mnuRecent: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    N11: TMenuItem;
    N9: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pnlLeftSpace: TPanel;
    pnlRightSpace: TPanel;
    pnlStack: TPanel;
    pnlPackages: TPanel;
    pnlFHIRPath: TPanel;
    pnlTasks: TPanel;
    pnlBreakpoints: TPanel;
    pnlSearch: TPanel;
    pnlLog: TPanel;
    pnlMessages: TPanel;
    pnlVariables: TPanel;
    pnlInspector: TPanel;
    pnlServers: TPanel;
    pnlProjects: TPanel;
    pnlClient: TPanel;
    pgEditors: TPageControl;
    Panel1: TPanel;
    Panel3: TPanel;
    pgLeft: TPageControl;
    pgBottom: TPageControl;
    pgRight: TPageControl;
    pnlBottom: TPanel;
    pnlMain: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pmNew: TPopupMenu;
    pmPages: TPopupMenu;
    pmMessageView: TPopupMenu;
    pmSearch: TPopupMenu;
    dlgFolder: TSelectDirectoryDialog;
    pmTestServers: TPopupMenu;
    btnClearLog: TSpeedButton;
    btnSearchFolder: TSpeedButton;
    btnCopyLog: TSpeedButton;
    btnStopTask: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    pnlStatus: TStatusBar;
    SynEdit1: TSynEdit;
    tbMessages: TTabSheet;
    tbStack: TTabSheet;
    tbExpression: TTabSheet;
    tbPackages: TTabSheet;
    tbProjects: TTabSheet;
    tbServers: TTabSheet;
    tbInspector: TTabSheet;
    tbVariables: TTabSheet;
    tbLog: TTabSheet;
    tbSearch: TTabSheet;
    tbBreakpoints: TTabSheet;
    tbTasks: TTabSheet;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    tbView1: TToolButton;
    tbView2: TToolButton;
    tbView3: TToolButton;
    tbView4: TToolButton;
    ToolButton2: TToolButton;
    tbView5: TToolButton;
    tbView6: TToolButton;
    tbView7: TToolButton;
    tbView8: TToolButton;
    tbView9: TToolButton;
    tbView10: TToolButton;
    tbView11: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton3: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton35: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tvProjects: TTreeView;
    vlInspector: TValueListEditor;
    procedure actConnectToServerExecute(Sender: TObject);
    procedure actionCopyFilePathExecute(Sender: TObject);
    procedure actionCopyFileTitleExecute(Sender: TObject);
    procedure actionCreateNewProjectExecute(Sender: TObject);
    procedure actionEditBeginEndExecute(Sender: TObject);
    procedure actionEditCopyExecute(Sender: TObject);
    procedure actionEditCopyFilenameExecute(Sender: TObject);
    procedure actionEditFindExecute(Sender: TObject);
    procedure actionEditFindNextExecute(Sender: TObject);
    procedure actionEditFindPrevExecute(Sender: TObject);
    procedure actionEditPasteEscapedExecute(Sender: TObject);
    procedure actionEditPasteFormatExecute(Sender: TObject);
    procedure actionEditPasteNewFileExecute(Sender: TObject);
    procedure actionEditPasteProcessedExecute(Sender: TObject);
    procedure actionEditRedoExecute(Sender: TObject);
    procedure actionEditReviewExecute(Sender: TObject);
    procedure actionFileCloseExecute(Sender: TObject);
    procedure actionFileManageCopyExecute(Sender: TObject);
    procedure actionFileManageDeleteExecute(Sender: TObject);
    procedure actionFileManageFolderExecute(Sender: TObject);
    procedure actionFileManageReloadExecute(Sender: TObject);
    procedure actionFileManageRenameExecute(Sender: TObject);
    procedure actionFileNewExecute(Sender: TObject);
    procedure actionFileOpenExecute(Sender: TObject);
    procedure actionFileOpenUrlExecute(Sender: TObject);
    procedure actionFileOpenQRCodeExecute(Sender: TObject);
    procedure actionFileSaveAllExecute(Sender: TObject);
    procedure actionFileSaveAs1Execute(Sender: TObject);
    procedure actionFileSaveExecute(Sender: TObject);
    procedure actionhelpAboutExecute(Sender: TObject);
    procedure actionHelpCheckUpgradeExecute(Sender: TObject);
    procedure actionHelpContentExecute(Sender: TObject);
    procedure actionHelpWelcomePageExecute(Sender: TObject);
    procedure actionNewEditorExecute(Sender: TObject);
    procedure actionPagesCloseAllExecute(Sender: TObject);
    procedure actionPagesCloseLeftExecute(Sender: TObject);
    procedure actionPagesCloseRightExecute(Sender: TObject);
    procedure actionPagesMoveFarleftExecute(Sender: TObject);
    procedure actionPagesMoveFarRIghtExecute(Sender: TObject);
    procedure actionPagesMoveLeftExecute(Sender: TObject);
    procedure actionPagesMoveRightExecute(Sender: TObject);
    procedure actionToolsOptionsExecute(Sender: TObject);
    procedure actionToolsPackageManagerExecute(Sender: TObject);
    procedure actionToolsSideBySideModeExecute(Sender: TObject);
    procedure actionViewEditorExecute(Sender: TObject);
    procedure actionViewExpressionEditorExecute(Sender: TObject);
    procedure actionViewFormDesignerExecute(Sender: TObject);
    procedure actionViewInspectorExecute(Sender: TObject);
    procedure actionViewLogExecute(Sender: TObject);
    procedure actionViewManagerExecute(Sender: TObject);
    procedure actionViewMessagesExecute(Sender: TObject);
    procedure actionViewPackagesExecute(Sender: TObject);
    procedure actionViewProjectManagerExecute(Sender: TObject);
    procedure actionViewResetExecute(Sender: TObject);
    procedure actionViewsClearLogExecute(Sender: TObject);
    procedure actionViewsCopyLogExecute(Sender: TObject);
    procedure actionViewSearchExecute(Sender: TObject);
    procedure actionViewServersExecute(Sender: TObject);
    procedure actionViewsOpenLogExecute(Sender: TObject);
    procedure actionViewStackExecute(Sender: TObject);
    procedure actionViewTasksExecute(Sender: TObject);
    procedure actionViewVariablesExecute(Sender: TObject);
    procedure actionZoomInExecute(Sender: TObject);
    procedure actionZoomOutExecute(Sender: TObject);
    procedure btnOpenLogClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSearchFolderClick(Sender: TObject);
    procedure cbxSearchEditingDone(Sender: TObject);
    procedure cbxSearchTypeChange(Sender: TObject);
    procedure chkCurrrentFileOnlyChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvMessagesDblClick(Sender: TObject);
    procedure lvSearchClick(Sender: TObject);
    procedure lvSearchDblClick(Sender: TObject);
    procedure MenuItem117Click(Sender: TObject);
    procedure MenuItem118Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem40Click(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure MenuItem60Click(Sender: TObject);
    procedure MenuItem98Click(Sender: TObject);
    procedure mnuLVCopyAllClick(Sender: TObject);
    procedure mnuLVCopyClick(Sender: TObject);
    procedure mnuLVGoClick(Sender: TObject);
    procedure mnuRecentClick(Sender: TObject);
    procedure mnuSearchGoClick(Sender: TObject);
    procedure NewFromFormatClick(Sender: TObject);
    procedure MenuItem79Click(Sender: TObject);
    procedure pgBottomChange(Sender: TObject);
    procedure pgEditorsChange(Sender: TObject);
    procedure pgEditorsCloseTabClicked(Sender: TObject);
    procedure pgEditorsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pgLeftChange(Sender: TObject);
    procedure pgRightChange(Sender: TObject);
    procedure pmMessageViewPopup(Sender: TObject);
    procedure pnlRightClick(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure Splitter3Moved(Sender: TObject);
    procedure tbLogShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure tvProjectsChange(Sender: TObject; Node: TTreeNode);
    procedure tvProjectsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure tvProjectsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvProjectsEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure tvProjectsEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure updateMessages(sender : TObject);
  private
    FIni : TIniFile;
    FTempStore : TFHIRToolkitTemporaryStorage;
    FFileSystem : TStorageService;
    FInternalStorage : TStorageService;
    FCheckingCurrency : boolean;
    FContext : TToolkitContext;
    FFactory : TToolkitFactory;
    FViewManager : TViewManager;
    FServerView : TFHIRServersView;
    FProjectsView : TFHIRProjectsView;
    FFinishedLoading : boolean;
    FScale : integer;
    FSearchTask : integer;
    FLastTaskUpdate : UInt64;
    FSearch : TFslList<TToolkitSearchMatch>;
    FShuttingDown : boolean;
    FDoingLayout : boolean;
    FViews : Array [TViewManagerPanelId] of TWinControl;
    FLeftStack : TPanelStack;
    FRightStack : TPanelStack;

    function checkDoSave(editor : TToolkitEditor): boolean;
    procedure copyFonts;
    procedure hideTabs(pg: TPageControl);
    procedure loadFont(font: TFont; sname: String);
    procedure placeView(panel : TPanel; id: TViewManagerPanelId);
    procedure SaveFile(editor: TToolkitEditor; address : String; updateStatus : boolean);
    procedure saveFont(font: TFont; sname : String);
    procedure saveFonts;
    procedure loadFonts;
    procedure loadSearch;
    procedure saveSearch;
    procedure maximiseSource;
    procedure showView(viewId : TViewManagerPanelId);
    procedure unmaximiseSource;
    procedure updateActionStatus(Sender : TObject);
    procedure DoAppActivate(Sender : TObject);
    procedure DoAppException(Sender : TObject; E : Exception);
    procedure updateUI;
    procedure updateTasks;
    procedure updateConsole;
    procedure updateActions;
    procedure checkActiveTabCurrency;
    procedure checkLastUpdated;
    procedure updateStatusBar;
    procedure openMRUItem(sender: TObject);
    procedure onChangeFocus(sender : TObject);
    procedure updateInspector(sender : TObject);
    procedure closeFile(tab : TTabSheet; store : boolean);
    procedure locateOnTab(sender : TObject; x, y: integer; var point : TPoint);
    function ChooseFileName(editor : TToolkitEditor; out address : String): boolean;
    procedure setScale(pct : integer);
    procedure doSearch;
    procedure doProcessSearchResults(id : integer; response : TBackgroundTaskResponsePackage);
    procedure processSearchResults(results : TFslList<TToolkitSearchMatch>; goFirst : boolean);
    procedure openServer(sender : TObject; server : TFHIRServerEntry);
    procedure editServer(sender : TObject; server : TFHIRServerEntry);
    procedure doOpenResourceUrl(sender : TObject; url : String);
    procedure doOpenResourceObj(sender : TObject; obj : TFHIRResourceV);
    procedure doOpenResourceSrc(sender : TObject; src : TBytes; format : TFHIRFormat; version : TFHIRVersion);
    procedure doOpenSource(sender : TObject; src : TBytes; kind : TSourceEditorKind);
    procedure DoConnectToServer(sender : TObject; server : TFHIRServerEntry);
    function DoSmartLogin(server : TFHIRServerEntry) : boolean;
    procedure AddServer(name, url : String);
    procedure clearContentMenu;
    procedure ApplyViewLayout;
    procedure startLoadingContexts;
    procedure doContextLoaded(id : integer; response : TBackgroundTaskResponsePackage);
    procedure storeOpenFileList;
    procedure checkWelcomePage;
  public
    property FileSystem : TStorageService read FFileSystem;
    property Context : TToolkitContext read FContext;

    // used by the project manager
    function createNewFile(kind : TSourceEditorKind; bytes : TBytes = []) : TToolkitEditor; overload;
    function createNewFile(kinds : TSourceEditorKindSet; bytes : TBytes = []) : TToolkitEditor; overload;
    function createNewFile(kind : TSourceEditorKind; filename, path : String; bytes : TBytes = []) : TToolkitEditor; overload;
    function determineClipboardFormat(var cnt : TBytes) : TSourceEditorKind;
    function openFile(address : String) : TToolkitEditor;
    procedure renameProjectFile(op, np : string); // if the file is open, update it's session and tab caption and update it's timestamp
    procedure renameFolder(op, np : string); // if the file is open, update it's session and tab caption and update it's timestamp
    function closeFiles(path : String) : boolean;
    function openFromURL(url : String) : boolean;
  end;

var
  MainToolkitForm: TMainToolkitForm;

implementation

{$R *.lfm}

{$IFDEF WINDOWS}
procedure TPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  r : TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (self = MainToolkitForm.pgEditors) and (Button = mbLeft) then
  begin
    R := TabRect(ActivePageIndex);
    if PtInRect(Classes.Rect(R.Right - btnSize - 4, R.Top + 2, R.Right - 4, R.Top + btnSize + 2), Classes.Point(X, Y)) then
      MainToolkitForm.pgEditorsCloseTabClicked(nil);
  end;
end;

procedure TPageControl.PaintWindow(DC: HDC);
var
  i : integer;
  r : TRect;
  bm : TBitmap;
begin
  inherited PaintWindow(DC);

  if (self = MainToolkitForm.pgEditors) then
  begin
    bm := TBitmap.Create;
    try
      bm.SetSize(16, 16);
      MainToolkitForm.imgMain.GetBitmap(ICON_CLOSE, bm);
      for i := 0 to PageCount - 1 do
      begin
        R := TabRect(i);
        if i = ActivePageIndex then
          StretchBlt(DC, R.Right - btnSize - 4, R.Top + 4, btnSize, btnSize, bm.Canvas.Handle, 0, 0, 16, 16, cmSrcCopy)
        else
          StretchBlt(DC, R.Right - btnSize - 4, R.Top + 6, btnSize, btnSize, bm.Canvas.Handle, 0, 0, 16, 16, cmSrcCopy);
      end;
    finally
      bm.Free;
    end;
  end;
end;

{$ENDIF}

{ TMainToolkitForm }

procedure TMainToolkitForm.FormCreate(Sender: TObject);
var
  ss : TServerStorageService;
begin
  Application.OnActivate := DoAppActivate;
  Application.OnException := DoAppException;
  {$IFDEF OSX}
  mnuApple.caption := #$EF#$A3#$BF;
  actionToolsOptions.caption := 'Preferences...';
  mnuFileExit.visible := false;
  {$ELSE}
  mnuApple.Visible := false;
  {$ENDIF}
  {$IFNDEF FPC}
  !
  GetOpenSSLLoader.OpenSSLPath := ExtractFilePath(Paramstr(0));
  try
    InitOpenSSL;
  except
    on e : Exception do
      MessageDlg('Error loading openSSL', e.message, mtError, [mbok], 0);
  end;
  {$ENDIF}
  //{$IFDEF OSX}
  //GetOpenSSLLoader.OpenSSLPath := ExtractFilePath(Paramstr(0)); // '/opt/homebrew/Cellar/openssl@1.1/1.1.1l/lib/';
  //{$ENDIF}
  GBackgroundTasks.start;
  FSearchTask := GBackgroundTasks.registerTaskEngine(TToolkitSearchTaskEngine.create);
  FIni := TIniFile.create(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'fhir-toolkit.ini');
  FScale := 100;
  FLeftStack := TPanelStack.create(pnlLeftSpace);
  FRightStack := TPanelStack.create(pnlRightSpace);
  FViewManager := TViewManager.create;
  FViewManager.IniFile := FIni;
  if FIni.readBool('main-form', 'maximised', false) then
    WindowState := wsMaximized
  else
  begin
    Top := FIni.readInteger('main-form', 'top', Top);
    Left := FIni.readInteger('main-form', 'left', Left);
    Height := FIni.readInteger('main-form', 'height', Height);
    Width := FIni.readInteger('main-form', 'width', Width);
  end;
  loadFonts;
  loadSearch;


  FTempStore := TFHIRToolkitTemporaryStorage.create;

  FContext := TToolkitContext.create(imgMain, actList);
  FContext.OnUpdateActions := updateActionStatus;
  FContext.OnLocate := locateOnTab;
  FContext.OnChangeFocus := onChangeFocus;
  FContext.OnOpenResourceUrl := doOpenResourceUrl;
  FContext.OnOpenResourceObj := doOpenResourceObj;
  FContext.OnOpenResourceSrc := doOpenResourceSrc;
  FContext.OnOpenSource := doOpenSource;
  FContext.MessageView.OnChange := updateMessages;
  FContext.Inspector.OnChange := updateInspector;
  FContext.Font := SynEdit1.Font;
  FContext.OnConnectToServer := DoConnectToServer;
  FContext.Settings := FIni;
  FContext.TerminologyService := TToolkitTerminologyService.create(
      FIni.ReadString('tx', 'server', 'http://tx.fhir.org/r4'),
      FIni.ReadString('tx', 'log', ''),
      IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'fhir-tx.cache');
  FServerView := TFHIRServersView.create(FIni);
  FServerView.ControlFile := FilePath([ExtractFileDir(FIni.FileName), 'servers.json']);
  FServerView.Images := imgMain;
  FServerView.List := lvServers;
  FServerView.OnOpenServer := OpenServer;
  FServerView.OnEditServer := EditServer;
  FServerView.load;
  FContext.OnFetchServer := FServerView.FetchServer;

  FProjectsView := TFHIRProjectsView.create(FIni);
  FProjectsView.ControlFile := FilePath([ExtractFileDir(FIni.FileName), 'projects.json']);
  FProjectsView.Images := imgMain;
  FProjectsView.Tree := tvProjects;
  FProjectsView.Context := FContext.link;
  FProjectsView.load;

  FContext.SideBySide := FIni.readBool('Settings', 'SideBySide', false);
  actionToolsSideBySideMode.Checked := FContext.SideBySide;

  FFileSystem := TFileStorageService.create(self, FIni);
  FContext.storages.add(FFileSystem.link);
  FInternalStorage := TInternalStorageService.create(self, FIni);
  FContext.storages.add(FInternalStorage.link);
  FContext.storages.add(THTTPStorageService.create); // must be before the next line
  ss := TServerStorageService.create(FServerView.ServerList.link);
  FContext.storages.add(ss);
  ss.OnConnectToServer := DoConnectToServer;
  FSearch := TFslList<TToolkitSearchMatch>.create;
  FFactory := TToolkitFactory.create(FContext.link, self);
  actionViewsClearLog.enabled := false;
  actionViewsOpenLog.enabled := false;
  actionViewsCopyLog.enabled := false;
  updateActionStatus(nil);
  FContext.Languages := TIETFLanguageDefinitions.create(FileToString(partnerFile('lang.dat'), TEncoding.UTF8));
  startLoadingContexts;
  Logging.Log('FHIR Toolkit Started: '+TFslDateTime.makeLocal.toString);
end;

procedure TMainToolkitForm.FormDestroy(Sender: TObject);
var
  editor : TToolkitEditor;
begin
  FLeftStack.free;
  FRightStack.free;
  for editor in FContext.Editors do
    editor.saveStatus;
  FServerView.saveStatus;
  FProjectsView.saveStatus;
  FShuttingDown := true;
  Timer1.Enabled := false;
  FServerView.Free;
  FProjectsView.Free;
  FSearch.Free;
  FFactory.free;
  FFileSystem.Free;
  FInternalStorage.Free;
  FTempStore.Free;
  FContext.Free;
  FViewManager.Free;

  FIni.WriteBool('main-form', 'maximised', WindowState = wsMaximized);
  if WindowState <> wsMaximized then
  begin
    FIni.WriteInteger('main-form', 'top', Top);
    FIni.WriteInteger('main-form', 'left', Left);
    FIni.WriteInteger('main-form', 'height', Height);
    FIni.WriteInteger('main-form', 'width', Width);
  end;
  FIni.Free;
end;

procedure TMainToolkitForm.FormShow(Sender: TObject);
var
  sessions : TFslList<TToolkitEditSession>;
  session : TToolkitEditSession;
  editor : TToolkitEditor;
  tab : TTabSheet;
  ns : boolean;
  i : integer;
  empty : boolean;
begin
  ApplyViewLayout;

  empty := true;
  sessions := TFslList<TToolkitEditSession>.create;
  try
    FTempStore.fetchOpenList(sessions);
    for session in sessions do
    begin
      empty := false;
      ns := session.NeedsSaving;
      editor := FFactory.makeEditor(session.link, FTempStore);
      FContext.addEditor(editor);
      tab := pgEditors.AddTabSheet;
      editor.bindToTab(tab);
      editor.LoadBytes(FTempStore.fetchContent(session.Guid));
      editor.session.NeedsSaving := ns;
      editor.lastChangeChecked := true;
      editor.lastMoveChecked := true;
      editor.editPause;
    end;
  finally
    sessions.free;
  end;
  if (empty) then
    createNewFile(sekHome).Session.Info.Values['auto'] := 'true';

  for i := 1 to ParamCount do
    if (FileExists(ParamStr(i))) then
      openFile('file:'+ParamStr(i))
    else
      openFile(ParamStr(i));

  clearContentMenu;
  if pgEditors.PageCount > 0 then
  begin
    tab := pgEditors.Pages[0];
    pgEditors.ActivePage := tab;
    FContext.Focus := FContext.EditorForTab(tab);
    FContext.focus.getFocus(mnuContent);
  end;
  updateActionStatus(editor);
  FContext.ToolBarHeight := ToolBar1.Height;
  FServerView.refresh;
  FProjectsView.refresh;
end;

procedure TMainToolkitForm.lvMessagesDblClick(Sender: TObject);
begin
  if lvMessages.Selected <> nil then
    mnuLVGoClick(sender);
end;

procedure TMainToolkitForm.lvSearchClick(Sender: TObject);
begin
end;

procedure TMainToolkitForm.lvSearchDblClick(Sender: TObject);
begin
  if lvSearch.Selected <> nil then
    mnuSearchGoClick(sender);
end;

procedure TMainToolkitForm.MenuItem117Click(Sender: TObject);
begin
  AddServer('tx.fhir.org', 'http://tx.fhir.org/r4');
end;

procedure TMainToolkitForm.MenuItem118Click(Sender: TObject);
begin
  AddServer('registry.fhir.org', 'http://registry.fhir.org');
end;

procedure TMainToolkitForm.MenuItem2Click(Sender: TObject);
begin
  AddServer('test.fhir.org', 'http://test.fhir.org/r4');
end;

procedure TMainToolkitForm.MenuItem40Click(Sender: TObject);
begin
  AddServer('hapi.fhir.org', 'http://hapi.fhir.org/baseR4');
end;

procedure TMainToolkitForm.mnuEditClick(Sender: TObject);
begin
end;

procedure TMainToolkitForm.loadFonts;
begin
  loadFont(SynEdit1.font, 'font-editor');
  loadFont(lvMessages.font, 'font-view');
  loadFont(mConsole.font, 'font-log');
  copyFonts;
end;

procedure TMainToolkitForm.loadSearch;
var
  i : integer;
begin
  cbxSearch.text := FIni.readString('Search', 'current', '');
  for i := 0 to Math.min(20, FIni.readInteger('Search', 'count', 0)) - 1 do
    cbxSearch.items.add(FIni.readString('Search', 'item'+inttostr(i), ''));
  chkCase.checked := FIni.readBool('Search', 'case', false);
  chkWholeWord.checked := FIni.readBool('Search', 'whole-word', false);
  cbxSearchType.itemIndex := FIni.readInteger('Search', 'type', 0);
  cbxSearchTypeChange(self);
end;

procedure TMainToolkitForm.saveSearch;
var
  i : integer;
begin
  FIni.writeString('Search', 'current', cbxSearch.text);
  FIni.writeInteger('Search', 'count', cbxSearch.items.count);
  for i := 0 to cbxSearch.items.count - 1 do
    FIni.writeString('Search', 'item'+inttostr(i), cbxSearch.items[i]);
  FIni.writeBool('Search', 'case', chkCase.checked);
  FIni.writeBool('Search', 'whole-word', chkWholeWord.checked);
  FIni.writeInteger('Search', 'type', cbxSearchType.itemIndex);

  if (cbxSearchType.itemIndex in [3..4]) then
  begin
    FIni.writeInteger('Search', 'folder-count', cbxSearchScope.items.count);
    for i := 0 to cbxSearchScope.items.count - 1 do
      FIni.writeString('Search', 'folder-item'+inttostr(i), cbxSearchScope.items[i]);
  end;
end;


procedure TMainToolkitForm.copyFonts;
begin
  vlInspector.Font.assign(lvMessages.Font);
  lvTasks.Font.assign(lvMessages.Font);
  lvSearch.Font.assign(lvMessages.Font);
end;

procedure TMainToolkitForm.loadFont(font : TFont; sname : String);
begin
  font.Name := FIni.ReadString(sname, 'name', font.Name);
  font.Size := FIni.ReadInteger(sname, 'size', font.Size);
  font.Color := FIni.ReadInteger(sname, 'color', font.Color);
  if FIni.readBool(sname, 'bold', false) then
    font.Style := font.Style + [fsBold];
  if FIni.readBool(sname, 'italic', false) then
    font.Style := font.Style + [fsItalic];
  if FIni.readBool(sname, 'underline', false) then
    font.Style := font.Style + [fsUnderline];
end;

procedure TMainToolkitForm.saveFont(font : TFont; sname : String);
begin
  FIni.WriteString(sname, 'name', font.Name);
  FIni.WriteInteger(sname, 'size', font.Size);
  FIni.WriteInteger(sname, 'color', font.Color);
  if fsBold in font.Style then
    FIni.WriteBool(sname, 'bold', true);
  if fsItalic in font.Style then
    FIni.WriteBool(sname, 'italic', true);
  if fsUnderline in font.Style then
    FIni.WriteBool(sname, 'underline', true);
end;

procedure TMainToolkitForm.saveFonts;
begin
  saveFont(SynEdit1.font, 'font-editor');
  saveFont(lvMessages.font, 'font-view');
  saveFont(mConsole.font, 'font-log');
end;

procedure TMainToolkitForm.tbLogShow(Sender: TObject);
begin
  tbLog.ImageIndex := 45;
end;

procedure TMainToolkitForm.Timer1Timer(Sender: TObject);
begin
  if FContext = nil then
    exit;

  FFinishedLoading := true;
  updateUI;
  try
    GBackgroundTasks.primaryThreadCheck;
  except
  end;
  checkLastUpdated;
end;

procedure TMainToolkitForm.ToolButton1Click(Sender: TObject);
var
  pt, pt2: TPoint;
begin
  pt.x := ToolBar1.left+ToolButton1.Left;
  pt.y := ToolBar1.top+ToolButton1.Top + ToolButton1.Height;
  pt2 := ClientToScreen(pt);
  pmNew.PopUp(pt2.x, pt2.y);
end;

procedure TMainToolkitForm.ToolButton2Click(Sender: TObject);
begin

end;

procedure TMainToolkitForm.tvProjectsChange(Sender: TObject; Node: TTreeNode);
begin
end;

procedure TMainToolkitForm.tvProjectsChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
end;

procedure TMainToolkitForm.tvProjectsEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  showMessage('edit from '+TFHIRProjectNode(node.data).name+' to '+node.Text+''''+s+'''');
  s := 'what?';
end;

procedure TMainToolkitForm.tvProjectsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
end;

procedure TMainToolkitForm.tvProjectsEditingEnd(Sender: TObject;
  Node: TTreeNode; Cancel: Boolean);
begin

end;

procedure TMainToolkitForm.maximiseSource;
begin
  pnlLeft.visible := false;
  pnlRight.visible := false;
  pnlBottom.visible := false;
  Splitter1.enabled := false;
  Splitter2.enabled := false;
  Splitter3.enabled := false;
  FViewManager.srcIsMaximised := true;
end;

procedure TMainToolkitForm.unmaximiseSource;
begin
  pnlLeft.visible := true;
  pnlRight.visible := true;
  pnlBottom.visible := true;
  Splitter1.enabled := true;
  Splitter2.enabled := true;
  Splitter3.enabled := true;
  FViewManager.srcIsMaximised := false;
end;

procedure TMainToolkitForm.updateUI;
begin
  updateStatusBar;
  updateConsole;
  updateActions;
  updateTasks;
end;

procedure TMainToolkitForm.updateTasks;
  procedure setSubItem(entry : TListItem; index : integer; value : String);
  begin
    if (entry.SubItems.count <= index) then
      entry.subitems.add(value)
    else
      entry.subitems[index] := value;
  end;
var
  list : TFslList<TBackgroundTaskStatusInfo>;
  entry : TListItem;
  item : TBackgroundTaskStatusInfo;
  id, index, count, i : integer;

begin
  if FLastTaskUpdate + 500 > GetTickCount64 then
    exit;
  FLastTaskUpdate := GetTickCount64;
  list := TFslList<TBackgroundTaskStatusInfo>.create;
  try
    if chkTaskEngines.checked then
      GBackgroundTasks.report(list, tvtEngines)
    else
      GBackgroundTasks.report(list, tvtTasks);
    if lvTasks.ItemIndex > -1 then
      id := integer(lvTasks.items[lvTasks.ItemIndex].data)
    else
      id := 0;
    index := -1;
    count := 0;

    for item in list do
    begin
      if count = lvTasks.Items.count then
        entry := lvTasks.items.add
      else
        entry := lvTasks.items[count];
      if item.info <> '' then
        entry.Caption := item.name+': '+item.info
      else
        entry.Caption := item.name;
      if (item.UniqueID = id) then
        index := count;
      entry.data := TObject(item.UniqueID);
      setSubItem(entry, 0, item.StatusDisplay);
      setSubItem(entry, 1, item.PctDisplay);
      setSubItem(entry, 2, item.timeDisplay);
      setSubItem(entry, 3, item.Message);
      inc(count);
    end;
    for i := lvTasks.Items.count - 1 downto count do
      lvTasks.Items.Delete(i);
    lvTasks.ItemIndex := index;
    lvTasks.Refresh;
    btnStopTask.enabled := (lvTasks.ItemIndex > -1) and (list[lvTasks.ItemIndex].status in [btsWaiting, btsProcessing]) and (list[lvTasks.ItemIndex].canCancel) ;
  finally
    list.free;
  end;
  if GBackgroundTasks.TasksAreWorking then
    tbTasks.ImageIndex := 84
  else
    tbTasks.ImageIndex := 46;
end;

procedure TMainToolkitForm.updateConsole;
var
  ts : TStringList;
  s : String;
begin
  ts := TStringList.create;
  try
    Context.Console.GetIncoming(ts);
    if (ts.count > 0) then
    begin
      mConsole.Lines.BeginUpdate;
      try
        for s in ts do
          mConsole.lines.add(s);
        while mConsole.lines.count > 1000 do
          mConsole.lines.delete(0);
        mConsole.SelStart := Length(mConsole.Text) - mConsole.lines[mConsole.lines.count - 1].length;
      finally
        mConsole.Lines.EndUpdate;
      end;
      actionViewsClearLog.enabled := true;
      actionViewsOpenLog.enabled := true;
      actionViewsCopyLog.enabled := true;
      if not mConsole.IsVisible then
        tbLog.ImageIndex := 85;
    end;
  finally
    ts.free;
  end;
end;

procedure TMainToolkitForm.updateActions;
begin
end;

procedure TMainToolkitForm.checkActiveTabCurrency;
var
  loaded : TLoadedBytes;
begin
  if not FCheckingCurrency and FFinishedLoading and not FShuttingDown and Context.hasFocus and Context.focus.hasStore and Context.focus.Store.CheckTimes and
    (Context.focus.session.nextCurrencyCheck < now) and not Context.focus.session.NoCheckCurrency then
  begin
    FCheckingCurrency := true;
    try
      Context.focus.session.nextCurrencyCheck := now + (Context.focus.Store.CurrencyCheckFrequency * DATETIME_SECOND_ONE);
      loaded := Context.focus.Store.load(Context.focus.session.address, false);
      if loaded.timestamp = 0 then
        exit; // problem accessing content at all... todo: show this status on the status panel

      if loaded.timestamp < 0 then
      begin
        case checkDeletedFileAction(self, Context.focus.session.presentedAddress) of
          dfaSave :
            begin
            Context.focus.session.nextCurrencyCheck := now;
            Context.focus.Store.forceLocation(Context.focus.session.address);
            actionFileSaveExecute(self);
            end;
          dfaIgnore :
            begin
            Context.focus.session.Timestamp := 1; // this ensures that if it comes back, user will be notified
            Context.focus.session.KnownToBeDeleted := true;
            end;
          dfaSaveAs :
            begin
            Context.focus.session.nextCurrencyCheck := now;
            actionFileSaveAs1Execute(self);
            end;
          dfaDiscard : closeFile(pgEditors.ActivePage, false);
          dfaNoCheck : Context.focus.session.NoCheckCurrency := true;
        end;
      end
      else if abs(loaded.timestamp - Context.focus.session.Timestamp) > DATETIME_SECOND_ONE then
      begin
        Context.focus.session.KnownToBeDeleted := false;
        case checkModifiedFileAction(self, Context.focus.session.presentedAddress, Context.focus.session.Timestamp, loaded.timestamp) of
          dmaSave :
            begin
            actionFileSaveExecute(self);
            Context.focus.session.nextCurrencyCheck := now;
            end;
          dmaDiff : actionEditReviewExecute(self);
          dmaReload :
            begin
            actionFileManageReloadExecute(self);
            Context.focus.session.nextCurrencyCheck := now;
            end;
          dmaIgnore : Context.focus.session.Timestamp := loaded.timestamp;
          dmaNoCheck : Context.focus.session.NoCheckCurrency := true;
        end;
      end;
    finally
      FCheckingCurrency := false;
    end;
  end;
end;

procedure TMainToolkitForm.checkLastUpdated;
var
  editor : TToolkitEditor;
begin
  for editor in FContext.Editors do
  begin
    if not editor.lastChangeChecked and (editor.lastChange + editor.Pause < GetTickCount64) then
    begin
      FTempStore.storeContent(editor.session.guid, false, editor.GetBytes);
      editor.editPause;
    end
    else if not editor.lastMoveChecked and (editor.lastMove + editor.Pause < GetTickCount64) then
    begin
      editor.movePause;
    end
  end;
end;

procedure TMainToolkitForm.updateStatusBar;
var
  focus : TToolkitEditor;
begin
  if FContext = nil then
    exit;

  focus := FContext.focus;
  if focus = nil then
  begin
    pnlStatus.Panels[0].Text := '';
    pnlStatus.Panels[1].Text := '';
  end
  else
  begin
    pnlStatus.Panels[0].Text := focus.location;
    if focus.session.NeedsSaving then
      pnlStatus.Panels[1].Text := 'Needs Saving'
    else if focus.session.Address = '' then
      pnlStatus.Panels[1].Text := 'New File'
    else
      pnlStatus.Panels[1].Text := 'Saved';
  end;
end;

procedure TMainToolkitForm.openMRUItem(sender: TObject);
begin
  openFile(FTempStore.getMRU((sender as TMenuItem).tag));
end;

function TMainToolkitForm.createNewFile(kind : TSourceEditorKind; bytes : TBytes = []) : TToolkitEditor;
begin
  result := createNewFile(kind, '', '', bytes);
end;

function TMainToolkitForm.createNewFile(kinds: TSourceEditorKindSet; bytes: TBytes): TToolkitEditor;
begin
  if (onlySourceKind(kinds) = sekNull) then
  begin
    FileFormatChooser := TFileFormatChooser.create(self);
    try
      FileFormatChooser.filter := kinds;
      if FileFormatChooser.ShowModal = mrOK then
        result := createNewFile(FileFormatChooser.kind, bytes)
      else
        abort;
    finally
      FileFormatChooser.free;
    end;
  end;
end;

function TMainToolkitForm.createNewFile(kind : TSourceEditorKind; filename, path : String; bytes : TBytes = []) : TToolkitEditor;
var
  session : TToolkitEditSession;
  editor : TToolkitEditor;
  tab : TTabSheet;
  info : TStringList;
begin
  info := TStringList.create;
  try
    if (kind = sekNull) then
    begin
      FileFormatChooser := TFileFormatChooser.create(self);
      try
        FileFormatChooser.ImageList := imgMain;
        if FileFormatChooser.ShowModal = mrOK then
          kind := FileFormatChooser.kind
        else
          abort;
      finally
        FileFormatChooser.free;
      end;
    end;
    session := FFactory.makeNewSession(kind);
    try
      if (kind = sekFHIR) and (length(bytes) = 0) then
      begin
        NewResourceDialog := TNewResourceDialog.create(self);
        try
          NewResourceDialog.IniFile := FIni;
          NewResourceDialog.Context := FContext.link;
          if NewResourceDialog.ShowModal <> mrOk then
            abort;
          session.Info.AddPair('fhir-version', NewResourceDialog.version);
          session.Info.AddPair('fhir-format', NewResourceDialog.format);
          bytes := NewResourceDialog.generate;
        finally
          NewResourceDialog.Free;
        end;
      end;

      if path <> '' then
        session.Address := 'file:'+path;
      if (filename <> '') then
        session.caption := filename;
      editor := FFactory.makeEditor(session.link, FTempStore);
      FContext.addEditor(editor);
      tab := pgEditors.AddTabSheet;
      editor.bindToTab(tab);
      if path <> '' then
      begin
        BytesToFile(bytes, path);
        session.Timestamp := FileGetModified(path);
      end;
      if (length(bytes) = 0) then
        editor.newContent
      else
        editor.loadBytes(bytes);
      editor.session.NeedsSaving := false;
      editor.lastChangeChecked := true;
      pgEditors.ActivePage := tab;
      storeOpenFileList;
      FTempStore.storeContent(editor.session.Guid, true, editor.getBytes);
      FContext.Focus := editor;
      clearContentMenu;
      FContext.Focus.getFocus(mnuContent);
      FProjectsView.refresh;
      updateActionStatus(editor);
      checkWelcomePage;
      result := editor;
    finally
      session.free;
    end;
  finally
    info.free;
  end;
end;

function TMainToolkitForm.openFile(address: String) : TToolkitEditor;
var
  loaded : TLoadedBytes;
  editor : TToolkitEditor;
  tab : TTabSheet;
  store : TStorageService;
  session : TToolkitEditSession;
begin
  editor := Context.EditorForAddress(address);
  if (editor <> nil) then
    pgEditors.ActivePage := editor.tab
  else
  begin
    store := Context.StorageForAddress(address);
    loaded := store.load(address, true);
    session := FFactory.examineFile(address, loaded.mimeType, loaded.content, false);
    if session <> nil then
    begin
      session.address := address;
      session.Timestamp := loaded.timestamp;
      editor := FFactory.makeEditor(session, FTempStore);
      FContext.addEditor(editor);
      tab := pgEditors.AddTabSheet;
      editor.bindToTab(tab);
      editor.LoadBytes(loaded.content);
      editor.session.NeedsSaving := false;
      pgEditors.ActivePage := tab;
      storeOpenFileList;
      FTempStore.storeContent(editor.session.Guid, true, editor.getBytes);
      FTempStore.removeFromMRU(editor.session.address);
      editor.lastChangeChecked := true;
      FContext.Focus := editor;
      clearContentMenu;
      FContext.Focus.getFocus(mnuContent);
      editor.lastChangeChecked := true;
      editor.lastMoveChecked := true;
      editor.editPause;
      FProjectsView.refresh;
      updateActionStatus(editor);
      checkWelcomePage;
      result := editor;
    end;
  end;
end;

procedure TMainToolkitForm.renameProjectFile(op, np: string);
var
  //loaded : TLoadedBytes;
  editor : TToolkitEditor;
  //tab : TTabSheet;
  //store : TStorageService;
  //session : TToolkitEditSession;
begin
  editor := Context.EditorForAddress('file:'+op);
  if (editor <> nil) then
  begin
    editor.Session.Address := 'file:'+np;
    editor.Session.Caption := Context.StorageForAddress(editor.Session.Address).CaptionForAddress(editor.Session.Address);
    editor.Tab.Caption := editor.Session.Caption;
    FTempStore.removeFromMRU('file:'+op);
    storeOpenFileList;
    updateMessages(self);
  end;
end;

procedure TMainToolkitForm.renameFolder(op, np: string);
var
  editor : TToolkitEditor;
begin
  op := 'file:/'+op;
  np := 'file:/'+np;
  for editor in FContext.Editors do
    if editor.Session.Address.StartsWith(op) then
      editor.Session.Address := np+editor.Session.Address.Substring(op.Length);
end;

function TMainToolkitForm.closeFiles(path: String) : boolean;
var
  i : integer;
begin
  result := true;
  for i := FContext.Editors.count - 1 downto 0 do
    if FContext.Editors[i].session.Address.startsWith('file:'+path) then
      closeFile(FContext.Editors[i].tab, false);
end;

function TMainToolkitForm.openFromURL(url: String): boolean;
  function showError(msg : String) : boolean;
  begin
    result := MessageDlg('Error', msg, mtError, [mbRetry, mbOK], 0) = mrOK;
  end;
var
  store : TStorageService;
  loaded : TLoadedBytes;
  editor : TToolkitEditor;
  tab : TTabSheet;
  session : TToolkitEditSession;
begin
  editor := Context.EditorForAddress(url);
  if (editor <> nil) then
    pgEditors.ActivePage := editor.tab
  else
  begin
    // todo: if URL is a canonical, present a list of versions
    store := Context.StorageForAddress(url);
    if (store = nil) then
      exit(showError('Unable to fetch '+url+': not a supported protocol'));
    try
      loaded := store.load(url, true);
      // well, we can open whatever it is
      session := FFactory.examineFile(url, loaded.mimeType, loaded.content, true);
      session.address := url;
      session.Timestamp := loaded.timestamp;
      editor := FFactory.makeEditor(session, FTempStore);
      FContext.addEditor(editor);
      tab := pgEditors.AddTabSheet;
      editor.bindToTab(tab);
      editor.LoadBytes(loaded.content);
      editor.session.NeedsSaving := false;
      pgEditors.ActivePage := tab;
      storeOpenFileList;
      FTempStore.storeContent(editor.session.Guid, true, editor.getBytes);
      FTempStore.removeFromMRU(editor.session.address);
      editor.lastChangeChecked := true;
      FContext.Focus := editor;
      clearContentMenu;
      FContext.Focus.getFocus(mnuContent);
      editor.lastChangeChecked := true;
      editor.lastMoveChecked := true;
      editor.editPause;
      FProjectsView.refresh;
      updateActionStatus(editor);
      checkWelcomePage;
      result := true;
    except
      on e : Exception do
        exit(showError('Error fetching '+url+': '+e.message));
    end;
  end;
end;

procedure TMainToolkitForm.onChangeFocus(sender: TObject);
begin
  if chkCurrrentFileOnly.Checked then
    updateMessages(sender);
end;

procedure TMainToolkitForm.updateInspector(sender: TObject);
begin
  if Context.Inspector.active then
  begin
    //vlInspector.Enabled := true;
    vlInspector.Color := clWhite;
    vlInspector.FixedColor := clBtnFace;
    vlInspector.FixedCols := 1;
    vlInspector.Strings.Assign(Context.Inspector.Content);
  end
  else
  begin
    //vlInspector.Enabled := false;
    if Context.hasFocus and (context.focus.Pause > 500) then
      vlInspector.Color := $fcf3f5;
    vlInspector.FixedColor := clBtnFace;
    vlInspector.FixedCols := 1;
  end;
end;

function TMainToolkitForm.checkDoSave(editor : TToolkitEditor) : boolean;
var
  address : String;
begin
  if editor.Session.NeedsSaving then
  begin
    case MessageDlg(editor.session.caption, editor.describe+' has changed. Save it?', mtConfirmation, mbYesNoCancel, 0) of
      mrYes :
        if editor.hasAddress then
          SaveFile(editor, editor.Session.Address, true)
        else if ChooseFileName(editor, address) then
        begin
          editor.Session.Address := address;
          SaveFile(editor, editor.Session.Address, true);
        end
        else
          abort;
      mrNo : {ignore};
      mrCancel: abort;
    end;
  end;
end;

procedure TMainToolkitForm.closeFile(tab: TTabSheet; store : boolean);
var
  editor : TToolkitEditor;
  home : boolean;
begin
  editor := Context.EditorForTab(tab);
  home := editor.Session.kind = sekHome;
  if store and (editor.CanBeSaved) then
    checkDoSave(editor);
  editor.saveStatus; // internal save, and then unhook
  if store and (editor.session.Address <> '') then
    FTempStore.addToMru(editor.session.Address, editor.session.caption);
  Context.removeEditor(editor);
  tab.free;
  storeOpenFileList;
  pgEditorsChange(self);
  FProjectsView.refresh;
  if (not home) and (pgEditors.PageCount = 0) then
    createNewFile(sekHome).Session.Info.Values['auto'] := 'true';
end;

procedure TMainToolkitForm.locateOnTab(sender: TObject; x, y: integer; var point: TPoint);
var
  p : TPoint;
  h : integer;
begin
  p.X := pnlMain.left+pgEditors.left + x;
  h := pgEditors.ActivePage.Height;
  p.Y := pnlMain.top+pgEditors.top + y + (pgEditors.height - h);
  point := ClientToScreen(p);
end;

procedure TMainToolkitForm.updateMessages(sender: TObject);
var
  msg : TToolkitMessage;
  entry : TListItem;
begin
  lvMessages.Items.clear;
  for msg in Context.MessageView.messages do
    if (not chkCurrrentFileOnly.checked or (Context.focus = msg.Editor)) and
       (not chkhideHintsAndWarnings.checked or (msg.level = msgError)) then
    begin
      entry := lvMessages.items.add;
      entry.Data := msg;
      entry.caption := msg.editor.Session.Caption;
      entry.SubItems.add(CODES_TToolkitMessageLevel[msg.level]);
      entry.SubItems.add('Line '+inttostr(msg.Location.lineForHuman));
      entry.SubItems.add(msg.Content);
    end;
end;

function TMainToolkitForm.ChooseFileName(editor: TToolkitEditor; out address : String): boolean;
begin
  if editor.Store <> nil then
    result := editor.Store.saveDlg(editor.session.Address, editor.FileExtension, address)
  else
    result := FFileSystem.saveDlg(editor.session.Address, editor.FileExtension, address);
end;

procedure TMainToolkitForm.setScale(pct: integer);
begin
  pnlMain.ScaleBy(100, FScale);
  Splitter1.ScaleBy(100, FScale);
  pnlBottom.ScaleBy(100, FScale);
  FScale := pct;
  pnlMain.ScaleBy(FScale, 100);
  Splitter1.ScaleBy(FScale, 100);
  pnlBottom.ScaleBy(FScale, 100);
  if not FDoingLayout then
    FViewManager.scale := FScale;
end;

procedure TMainToolkitForm.doSearch;
var
  i : integer;
  spec : TToolkitSearchSpecification;
  engine : TToolkitSearchEngine;
  req : TToolkitSearchTaskRequest;
  resp : TToolkitSearchTaskResponse;
  editor : TToolkitEditor;
begin
  if cbxSearch.text = '' then
    MessageDlg('Search', 'Please enter some text to search for', mtError, [mbok], 0)
  else
  begin
    i := cbxSearch.items.indexof(cbxSearch.text);
    if i > -1 then
      cbxSearch.items.Delete(i);
    cbxSearch.items.insert(0, cbxSearch.text);
    if cbxSearchScope.text <> '' then
    begin
      i := cbxSearchScope.items.indexof(cbxSearchScope.text);
      if i > -1 then
        cbxSearchScope.items.Delete(i);
      cbxSearchScope.items.insert(0, cbxSearchScope.text);
    end;
    saveSearch;
    spec := TToolkitSearchSpecification.create;
    try
      spec.text := cbxSearch.text;
      spec.caseSensitive := chkCase.checked;
      spec.wholeWords := chkWholeWord.checked;
      spec.kind := TToolkitSearchKind(cbxSearchType.itemIndex);
      spec.scope := cbxSearchScope.text;
      case spec.kind of
        tskCurrent : if not Context.hasFocus then abort;
        tskAllOpen : if not Context.hasFocus then abort;
        tskProject : abort;
        tskFolder : if not FolderExists(spec.scope) then raise EFslException.Create('Folder "'+spec.scope+'" not found');
        tskFolderTree : if not FolderExists(spec.scope) then raise EFslException.Create('Folder "'+spec.scope+'" not found');
      end;

      FSearch.Clear;
      lvSearch.items.clear;
      if spec.kind = tskCurrent then // we do this one in thread
      begin
        spec.sources.add(TToolkitSearchSource.create( Context.Focus.Session.Caption, context.focus.Session.Guid, Context.Focus.getSource));
        engine := TToolkitSearchEngine.create;
        try
          engine.context := context.link;
          engine.spec := spec.link;
          engine.results := TFslList<TToolkitSearchMatch>.create;
          engine.go;
          processSearchResults(engine.results, true);
        finally
          engine.free;
        end;
      end
      else
      begin
        if spec.kind = tskAllOpen then
          for editor in Context.Editors do
            spec.sources.add(TToolkitSearchSource.create(editor.Session.Caption, editor.Session.Guid, editor.getSource));
        GBackgroundTasks.queueTask(FSearchTask, TToolkitSearchTaskRequest.create(spec.link, context.link), TToolkitSearchTaskResponse.create, doProcessSearchResults);
      end;
    finally
      spec.Free;
    end;
  end;
end;

procedure TMainToolkitForm.doProcessSearchResults(id : integer; response : TBackgroundTaskResponsePackage);
begin
  if response.Exception <> '' then
    Logging.log('Search Failed: '+response.Exception);

  processSearchResults((response as TToolkitSearchTaskResponse).results, false);
end;

procedure TMainToolkitForm.processSearchResults(results: TFslList<TToolkitSearchMatch>; goFirst: boolean);
var
  m : TToolkitSearchMatch;
  entry : TListItem;
begin
  FSearch.Clear;
  FSearch.AddAll(results);
  lvSearch.BeginUpdate;
  try
    lvSearch.Items.Clear;
    for m in FSearch do
    begin
      entry := lvSearch.Items.add;
      entry.caption := m.name;
      entry.SubItems.add(inttostr(m.location.lineForHuman));
      entry.SubItems.add(m.fragment);
      entry.Data := m;
    end;
  finally
    lvSearch.EndUpdate;
  end;
  lvSearch.ItemIndex := -1;
  if (goFirst and (lvSearch.items.count > 0)) then
    actionEditFindNextExecute(self);
end;

procedure TMainToolkitForm.openServer(sender: TObject; server: TFHIRServerEntry);
var
  worker : TServerWorker;
  session : TToolkitEditSession;
  tab : TTabSheet;
begin
  if server.client = nil then
    DoConnectToServer(self, server);
  worker := server.workerObject as TServerWorker;
  if (worker <> nil) then
    pgEditors.ActivePage := worker.tab
  else
  begin
    session := FFactory.makeNewSession(sekServer);
    session.caption := 'Server '+server.Name;
    session.address := 'internal:server.'+server.name;
    worker := FFactory.makeEditor(session, FTempStore) as TServerWorker;
    FContext.addEditor(worker);
    tab := pgEditors.AddTabSheet;
    worker.bindToTab(tab);
    worker.newContent;
    worker.session.NeedsSaving := false;
    worker.lastChangeChecked := true;
    pgEditors.ActivePage := tab;
    storeOpenFileList;
    FTempStore.storeContent(worker.session.Guid, true, worker.getBytes);
    FContext.Focus := worker;
    clearContentMenu;
    FContext.Focus.getFocus(mnuContent);
    FServerView.refresh;
    updateActionStatus(worker);
    checkWelcomePage;
  end;
end;

procedure TMainToolkitForm.editServer(sender: TObject; server: TFHIRServerEntry);
var
  srvr : TFHIRServerEntry;
begin
  srvr := server.clone;
  try
    ServerSettingsForm := TServerSettingsForm.create(self);
    try
      ServerSettingsForm.Server := srvr.link;
      ServerSettingsForm.ServerList := FServerView.ServerList.link;
      if ServerSettingsForm.ShowModal = mrOk then
      begin
        FServerView.updateServer(server, srvr);
      end;
    finally
      FreeAndNil(ServerSettingsForm);
    end;
  finally
    srvr.free;
  end;
end;

procedure TMainToolkitForm.doOpenResourceUrl(sender: TObject; url: String);
var
  loaded : TLoadedBytes;
  editor : TToolkitEditor;
  tab : TTabSheet;
  store : TStorageService;
  session : TToolkitEditSession;
begin
  editor := Context.EditorForAddress(url);
  if (editor <> nil) then
    pgEditors.ActivePage := editor.tab
  else
  begin
    store := Context.StorageForAddress(url);
    loaded := store.load(url, true);
    session := FFactory.examineFile(url, loaded.mimeType, loaded.content, false);
    if session <> nil then
    begin
      session.address := url;
      session.Timestamp := loaded.timestamp;
      editor := FFactory.makeEditor(session, FTempStore);
      FContext.addEditor(editor);
      tab := pgEditors.AddTabSheet;
      editor.bindToTab(tab);
      editor.LoadBytes(loaded.content);
      editor.session.NeedsSaving := false;
      pgEditors.ActivePage := tab;
      storeOpenFileList;
      FTempStore.storeContent(editor.session.Guid, true, editor.getBytes);
      FTempStore.removeFromMRU(editor.session.address);
      editor.lastChangeChecked := true;
      FContext.Focus := editor;
      clearContentMenu;
      FContext.Focus.getFocus(mnuContent);
      editor.lastChangeChecked := true;
      editor.lastMoveChecked := true;
      editor.editPause;
      updateActionStatus(editor);
      checkWelcomePage;
    end;
  end;
end;

procedure TMainToolkitForm.doOpenResourceSrc(sender: TObject; src : TBytes; format : TFHIRFormat; version : TFHIRVersion);
var
  editor : TToolkitEditor;
  tab : TTabSheet;
  store : TStorageService;
  session : TToolkitEditSession;
begin
  session := FFactory.makeNewSession(sekFHIR);
  session.info.Values['Format'] := CODES_TFHIRFormat[format];
  editor := FFactory.makeEditor(session, FTempStore);
  FContext.addEditor(editor);
  tab := pgEditors.AddTabSheet;
  editor.bindToTab(tab);
  editor.loadBytes(src);
  editor.session.NeedsSaving := false;
  editor.lastChangeChecked := true;
  pgEditors.ActivePage := tab;
  storeOpenFileList;
  FTempStore.storeContent(editor.session.Guid, true, editor.getBytes);
  FContext.Focus := editor;
  clearContentMenu;
  FContext.Focus.getFocus(mnuContent);
  updateActionStatus(editor);
  checkWelcomePage;
end;

procedure TMainToolkitForm.doOpenSource(sender : TObject; src : TBytes; kind : TSourceEditorKind);
var
  editor : TToolkitEditor;
  tab : TTabSheet;
  store : TStorageService;
  session : TToolkitEditSession;
begin
  session := FFactory.makeNewSession(kind);
  editor := FFactory.makeEditor(session, FTempStore);
  FContext.addEditor(editor);
  tab := pgEditors.AddTabSheet;
  editor.bindToTab(tab);
  editor.loadBytes(src);
  editor.session.NeedsSaving := false;
  editor.lastChangeChecked := true;
  pgEditors.ActivePage := tab;
  storeOpenFileList;
  FTempStore.storeContent(editor.session.Guid, true, editor.getBytes);
  FContext.Focus := editor;
  clearContentMenu;
  FContext.Focus.getFocus(mnuContent);
  updateActionStatus(editor);
  checkWelcomePage;
end;

procedure TMainToolkitForm.doOpenResourceObj(sender: TObject; obj : TFHIRResourceV);
var
  fact : TFHIRFactory;
  json : TFHIRComposer;
begin
  fact := FContext.factory(obj.fhirObjectVersion);
  try
    json := fact.makeComposer(nil, ffJson, defLang, OutputStylePretty);
    try
      doOpenResourceSrc(sender, json.ComposeBytes(obj), ffJson, obj.fhirObjectVersion);
    finally
      json.free;
    end;
  finally
    fact.free;
  end;
end;

procedure TMainToolkitForm.DoConnectToServer(sender: TObject; server: TFHIRServerEntry);
var
  factory : TFHIRFactory;
begin
  case server.smartMode of
    salmNone : {nothing} ;
    salmOAuthClient : if not DoSmartLogin(server) then
      Abort;
    salmBackendClient : raise ETodo.create('ConnectToServer/salmBackendClient');
  end;
  factory := Context.factory(server.version);
  try
    server.client := factory.makeClient(nil, server.URL, fctCrossPlatform, server.format);
    server.client.smartToken := server.token.link;
    if server.logFileName <> '' then
      server.client.Logger := TTextFileLogger.create(server.logFileName);
  finally
    factory.free;
  end;
end;

function TMainToolkitForm.DoSmartLogin(server: TFHIRServerEntry): boolean;
var
  form : TOAuthForm;
begin
  form := TOAuthForm.create(self);
  try
    form.server := server.makeOAuthDetails;
    form.scopes := server.scopes.split([' ']);
    result := form.ShowModal = mrOk;
    if result then
      server.Token := form.token.link;
  finally
    form.free;
  end;
end;


procedure TMainToolkitForm.updateActionStatus(Sender: TObject);
begin
  if context.hasFocus then
    Caption := 'FHIR Toolkit - '+context.focus.session.caption
  else
    Caption := 'FHIR Toolkit';

  // always enabled
  actionToolsPackageManager.enabled := true;
  actionToolsOptions.enabled := true;
  actionHelpContent.enabled := true;
  actionHelpCheckUpgrade.enabled := true;
  actionhelpAbout.enabled := true;
  actionViewTasks.enabled := true;
  actionViewExpressionEditor.enabled := true;
  actionViewVariables.enabled := true;
  actionViewInspector.enabled := true;
  actionViewPackages.enabled := true;
  actionViewEditor.enabled := true;
  actionViewProjectManager.enabled := true;
  actionViewServers.enabled := true;
  actionViewSearch.enabled := true;
  actionViewMessages.enabled := true;
  actionViewLog.enabled := true;
  actionViewStack.enabled := true;
  actionFileNew.enabled := true;
  actionFileOpen.enabled := true;
  actionFileOpenUrl.enabled := true;
  actionFileExit.enabled := true;

  actionFileSaveAll.enabled := context.anyDirtyEditors;

  // enabled if there's an open file:
  actionCopyFileTitle.enabled := context.hasFocus and context.Focus.isFile;
  actionEditCopyFilename.enabled := context.hasFocus and context.Focus.hasAddress;
  actionFileManageFolder.enabled := context.hasFocus and context.Focus.isFile;
  actionCopyFilePath.enabled := context.hasFocus and context.Focus.hasAddress;
  actionCopyFileTitle.enabled := context.hasFocus and context.Focus.hasAddress;
  actionFileClose.enabled := context.hasFocus;
  actionFileSave.enabled := context.hasFocus and context.Focus.CanBeSaved;
  actionFileManageRename.enabled := context.hasFocus and context.Focus.isFile;
  actionFileManageCopy.enabled := context.hasFocus and context.Focus.isFile;
  actionFileManageDelete.enabled := context.hasFocus and context.Focus.isFile;
  actionFileManageReload.enabled := context.hasFocus and context.Focus.hasAddress;
  actionFileSaveAs1.enabled := context.hasFocus and context.Focus.CanBeSaved;
  actionEditReview.enabled := context.hasFocus and (Context.Focus.Store <> nil);

  actionPagesCloseAll.enabled := context.hasFocus and (context.Editors.count > 0);
  actionPagesCloseLeft.enabled := context.hasFocus and (pgEditors.ActivePage.PageIndex > 0);
  actionPagesCloseRight.enabled := context.hasFocus and (pgEditors.ActivePage.PageIndex < pgEditors.PageCount - 1);
  actionPagesMoveFarleft.enabled := context.hasFocus and (pgEditors.ActivePage.PageIndex > 1);
  actionPagesMoveLeft.enabled := context.hasFocus and (pgEditors.ActivePage.PageIndex > 0);
  actionPagesMoveRight.enabled := context.hasFocus and (pgEditors.ActivePage.PageIndex < pgEditors.PageCount - 1);
  actionPagesMoveFarRIght.enabled := context.hasFocus and (pgEditors.ActivePage.PageIndex < pgEditors.PageCount - 2);

  actionViewFormDesigner.enabled := context.hasFocus and context.Focus.hasDesigner;
  if context.hasFocus and context.focus.IsShowingDesigner then
  begin
    actionViewFormDesigner.Caption := 'Text E&ditor';
    actionViewFormDesigner.ImageIndex := 77;
  end
  else
  begin
    actionViewFormDesigner.Caption := '&Designer';
    actionViewFormDesigner.ImageIndex := 76;
  end;

  // enabled if there's a text selectable
  actionEditCopy.enabled := context.hasFocus and context.Focus.hasText;
  actionEditSelectAll.enabled := context.hasFocus and context.Focus.hasText;

  // enabled if the text is writable
  actionEditUndo.enabled := context.hasFocus and context.Focus.canUndo;
  actionEditRedo.enabled := context.hasFocus and context.Focus.canRedo;
  actionEditCut.enabled := context.hasFocus and context.Focus.canCut;
  actionEditDelete.enabled := context.hasFocus and context.Focus.canCut;
  actionEditPaste.enabled := context.hasFocus and context.Focus.canPaste;
  actionEditPasteFormat.enabled := actionEditPaste.enabled;
  actionEditPasteEscaped.enabled := actionEditPaste.enabled and context.Focus.canEscape;

  // enabled if we're in source mode
  actionEditBeginEnd.enabled := context.hasFocus and not context.Focus.IsShowingDesigner;
  actionZoomIn.enabled := true;
  actionZoomOut.enabled := true;
  actionFilePrint.enabled := context.hasFocus and not context.Focus.IsShowingDesigner;

  FServerView.refresh;
end;

procedure TMainToolkitForm.DoAppActivate(Sender: TObject);
begin
  checkActiveTabCurrency;
end;

procedure TMainToolkitForm.DoAppException(Sender: TObject; E: Exception);
begin
  showmessage('Exception: '+e.message);
end;

procedure TMainToolkitForm.actionViewEditorExecute(Sender: TObject);
begin
  if FViewManager.srcIsMaximised then
    unmaximiseSource
  else
    maximiseSource;
end;

procedure TMainToolkitForm.showView(viewId : TViewManagerPanelId);
var
  pg : ComCtrls.TPageControl;
begin
  if FViewManager.srcIsMaximised then
    unmaximiseSource;
  case FViewManager.location(viewId) of
    tvlLeft : if pnlLeft.Width < 170 then pnlLeft.Width := 170;
    tvlRight : if pnlRight.Width < 230 then pnlRight.Width := 230;
    tvlBottom : if pnlBottom.Height < 142 then pnlBottom.Height := 142;
  end;
  if FViews[viewId] is TTabSheet then
  begin
    pg := (FViews[viewId] as TTabSheet).PageControl;
    pg.ActivePage := FViews[viewId] as TTabSheet;
  end;
end;

procedure TMainToolkitForm.actionViewExpressionEditorExecute(Sender: TObject);
begin
  showView(tviFHIRPath);
end;

procedure TMainToolkitForm.actionViewFormDesignerExecute(Sender: TObject);
begin
  if Context.Focus.IsShowingDesigner then
    Context.Focus.ShowTextTab
  else
    Context.Focus.ShowDesigner;
  updateActionStatus(self);
end;

procedure TMainToolkitForm.actionViewInspectorExecute(Sender: TObject);
begin
  showView(tviInspector);
end;

procedure TMainToolkitForm.actionViewLogExecute(Sender: TObject);
begin
  showView(tviLog);
end;

procedure TMainToolkitForm.actionViewManagerExecute(Sender: TObject);
begin
  ViewManagerForm := TViewManagerForm.create(self);
  try
    ViewManagerForm.Manager := FViewManager.link;
    if ViewManagerForm.showModal = mrOk then
      ApplyViewLayout;
  finally
    ViewManagerForm.Free;
  end;
end;

procedure TMainToolkitForm.actionViewMessagesExecute(Sender: TObject);
begin
  showView(tviMessages);
end;

procedure TMainToolkitForm.actionViewPackagesExecute(Sender: TObject);
begin
  showView(tviPackages);
end;

procedure TMainToolkitForm.actionViewProjectManagerExecute(Sender: TObject);
begin
  showView(tviProjects);
end;

procedure TMainToolkitForm.actionViewResetExecute(Sender: TObject);
begin

end;

procedure TMainToolkitForm.actionViewsClearLogExecute(Sender: TObject);
begin
  mConsole.Clear;
  actionViewsClearLog.enabled := false;
  actionViewsOpenLog.enabled := false;
  actionViewsCopyLog.enabled := false;
end;

procedure TMainToolkitForm.actionViewsCopyLogExecute(Sender: TObject);
begin
  mConsole.CopyToClipboard;
end;

procedure TMainToolkitForm.actionViewSearchExecute(Sender: TObject);
begin
  showView(tviSearch);
  cbxSearch.SetFocus;
end;

procedure TMainToolkitForm.actionViewServersExecute(Sender: TObject);
begin
  showView(tviServers);
end;

procedure TMainToolkitForm.actionViewsOpenLogExecute(Sender: TObject);
begin
  createNewFile(sekText, TEncoding.UTF8.GetBytes(mConsole.Text));
end;

procedure TMainToolkitForm.actionViewStackExecute(Sender: TObject);
begin
  showView(tviStack);
end;

procedure TMainToolkitForm.actionViewTasksExecute(Sender: TObject);
begin
  showView(tviTasks);
end;

procedure TMainToolkitForm.actionViewVariablesExecute(Sender: TObject);
begin
  showView(tviVariables);
end;

procedure TMainToolkitForm.actionZoomInExecute(Sender: TObject);
begin
  BeginFormUpdate;
  try
    setScale(trunc(FScale * 1.2));
  finally
    EndFormUpdate;
  end;
end;

procedure TMainToolkitForm.actionZoomOutExecute(Sender: TObject);
begin
  BeginFormUpdate;
  try
    setScale(trunc(FScale / 1.2));
  finally
    EndFormUpdate;
  end;
end;

procedure TMainToolkitForm.btnOpenLogClick(Sender: TObject);
begin

end;


procedure TMainToolkitForm.btnSearchClick(Sender: TObject);
begin
  doSearch;
end;

procedure TMainToolkitForm.btnSearchFolderClick(Sender: TObject);
begin
  if dlgFolder.execute then
    cbxSearchScope.text := dlgFolder.FileName;
end;

procedure TMainToolkitForm.cbxSearchEditingDone(Sender: TObject);
begin
  doSearch;
end;

procedure TMainToolkitForm.cbxSearchTypeChange(Sender: TObject);
var
  i : integer;
begin
  cbxSearchScope.Text := '';
  cbxSearchScope.Enabled := false;
  btnSearchFolder.Enabled := false;
  if cbxSearchType.ItemIndex in [3..4] then
  begin
    cbxSearchScope.Text := FIni.ReadString('Search', 'folder-text', '');
    for i := 0 to Math.min(20, FIni.readInteger('Search', 'folder-count', 0)) - 1 do
      cbxSearchScope.items.add(FIni.readString('Search', 'folder-item'+inttostr(i), ''));
    cbxSearchScope.Enabled := true;
    btnSearchFolder.Enabled := true;
  end;
end;

procedure TMainToolkitForm.chkCurrrentFileOnlyChange(Sender: TObject);
begin

end;

procedure TMainToolkitForm.FormActivate(Sender: TObject);
begin
  checkActiveTabCurrency;
end;

procedure TMainToolkitForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  editor : TToolkitEditor;
begin
  GBackgroundTasks.stopAll;
  storeOpenFileList;
  for editor in FContext.Editors do
  begin
    if not editor.lastChangeChecked then
      FTempStore.storeContent(editor.session.guid, false, editor.GetBytes);
  end;
  CanClose := true;
end;

procedure TMainToolkitForm.actionFileManageRenameExecute(Sender: TObject);
var
  address, old : String;
begin
  if ChooseFileName(Context.Focus, address) then
  begin
    old := Context.Focus.Session.Address;
    Context.Focus.Session.Address := address;
    SaveFile(Context.Focus, Context.Focus.Session.Address, true);
    Context.Focus.Session.Caption := Context.StorageForAddress(address).CaptionForAddress(address);
    Context.Focus.Tab.Caption := Context.Focus.Session.Caption;
    FTempStore.removeFromMRU(address);
    storeOpenFileList;
    Context.StorageForAddress(old).delete(old);
    updateMessages(self);
    FProjectsView.renameFile(old.Substring(5), address.substring(5));
  end;
end;

procedure TMainToolkitForm.actionEditRedoExecute(Sender: TObject);
begin
  Context.focus.redo;
end;

procedure TMainToolkitForm.actionEditReviewExecute(Sender: TObject);
var
  frm : TEditChangeReviewForm;
begin
  frm := TEditChangeReviewForm.create(self);
  try
    frm.editor := Context.Focus.link;
    frm.DiffTool := FIni.ReadString('Tools', 'Diff', '');
    if frm.ShowModal = mrOK then
      Context.Focus.loadBytes(TEncoding.UTF8.getBytes(frm.mEditorSource.Lines.Text));
  finally
    frm.Free;
  end;
end;

procedure TMainToolkitForm.actionEditBeginEndExecute(Sender: TObject);
begin
  Context.Focus.BeginEndSelect;
end;

procedure TMainToolkitForm.actConnectToServerExecute(Sender: TObject);
begin
  AddServer('', '');
end;

procedure TMainToolkitForm.actionCopyFilePathExecute(Sender: TObject);
begin
  if context.hasFocus then
  begin
    Clipboard.Open;
    Clipboard.AsText := context.focus.store.getName(context.focus.Session.Address, nameModeFolder);
    Clipboard.Close;
  end;
end;

procedure TMainToolkitForm.actionCopyFileTitleExecute(Sender: TObject);
begin
  if context.hasFocus then
  begin
    Clipboard.Open;
    Clipboard.AsText := context.focus.store.getName(context.focus.Session.Address, nameModeName);
    Clipboard.Close;
  end;
end;

procedure TMainToolkitForm.actionCreateNewProjectExecute(Sender: TObject);
var
  proj : TFHIRProjectNode;
begin
  proj := TFHIRProjectNode.create;
  try
    proj.id := NewGuidId;
    ProjectSettingsForm := TProjectSettingsForm.create(self);
    try
      ProjectSettingsForm.Project := proj.link;
      ProjectSettingsForm.Projects := FProjectsView.Projects.link;
      if ProjectSettingsForm.ShowModal = mrOk then
      begin
        proj.loadFromAddress;
        FProjectsView.addProject(proj);
      end;
    finally
      ProjectSettingsForm.Free;
    end;
  finally
    proj.free;
  end;
end;

procedure TMainToolkitForm.actionEditCopyExecute(Sender: TObject);
begin

end;

procedure TMainToolkitForm.actionEditCopyFilenameExecute(Sender: TObject);
begin
  if context.hasFocus then
  begin
    Clipboard.Open;
    Clipboard.AsText := context.focus.store.getName(context.focus.Session.Address, nameModeFullPath);
    Clipboard.Close;
  end;
end;

procedure TMainToolkitForm.actionEditFindExecute(Sender: TObject);
begin
  showView(tviSearch);
  cbxSearch.SetFocus;
end;

procedure TMainToolkitForm.actionEditFindNextExecute(Sender: TObject);
begin
  if lvSearch.itemIndex < lvSearch.Items.count - 1 then
  begin
    lvSearch.itemIndex := lvSearch.itemIndex + 1;
    mnuSearchGoClick(self);
  end;
end;

procedure TMainToolkitForm.actionEditFindPrevExecute(Sender: TObject);
begin
  if lvSearch.itemIndex > 0 then
  begin
    lvSearch.itemIndex := lvSearch.itemIndex - 1;
    mnuSearchGoClick(self);
  end;
end;

procedure TMainToolkitForm.actionEditPasteEscapedExecute(Sender: TObject);
var
  clip : TClipboard;
begin
  if Context.HasFocus and Context.focus.canEscape then
  begin
    clip := TClipboard.create;
    try
      Context.focus.insertText(clip.AsText, true);
    finally
      clip.free;
    end;
  end;
end;

procedure TMainToolkitForm.actionEditPasteFormatExecute(Sender: TObject);
var
  src : String;
begin
  src := chooseClipboardContent(self);
  if (src <> '') then
    context.focus.insertText(src, false);
end;

function TMainToolkitForm.determineClipboardFormat(var cnt : TBytes) : TSourceEditorKind;
var
  clip : TClipboard;
  i : integer;
  f : TClipboardFormat;
  s : TBytesStream;
  fmts : TSourceEditorKindSet;
begin
  clip := TClipboard.create;
  try
    if clip.AsText <> '' then
    begin
      fmts := TToolkitFactory.determineFormatFromText(clip.asText, cnt);
      if fmts = [] then
      begin
        MessageDlg('Paste New File', 'Unable to determine file format', mtError, [mbok], 0);
        exit(sekNull)
      end
      else
      begin
        result := onlySourceKind(fmts);
        if result <> sekNull then
          exit
        else
          exit(ChooseFormat(self, fmts, TEncoding.UTF8.GetString(cnt)));
      end;
    end;
    for i := 0 to clip.FormatCount - 1 do
    begin
      s := TBytesStream.create;
      try
        f := clip.Formats[i];
        clip.GetFormat(f, s);
        if TToolkitFactory.determineFormatFromFmt(f, s.Bytes, result, cnt) then
          exit;
      finally
        s.free;
      end;
    end;
  finally
    clip.free;
  end;
  MessageDlg('Paste New File', 'Unable to determine file format', mtError, [mbok], 0);
  result := sekNull;
end;

procedure TMainToolkitForm.AddServer(name, url: String);
var
  server : TFHIRServerEntry;
begin
  server := TFHIRServerEntry.create;
  try
    ServerSettingsForm := TServerSettingsForm.create(self);
    try
      ServerSettingsForm.Server := server.link;
      ServerSettingsForm.edtName.text := name;
      ServerSettingsForm.edtUrl.text := url;
      ServerSettingsForm.ServerList := FServerView.ServerList.link;
      if ServerSettingsForm.ShowModal = mrOk then
      begin
        FServerView.addServer(server);
      end;
    finally
      FreeAndNil(ServerSettingsForm);
    end;
  finally
    server.free;
  end;
end;

procedure TMainToolkitForm.clearContentMenu;
begin
  mnuContent.OnClick := nil;
  mnuContent.Clear;
end;

procedure TMainToolkitForm.actionEditPasteNewFileExecute(Sender: TObject);
var
  kind : TSourceEditorKind;
  cnt : TBytes;
begin
  kind := determineClipboardFormat(cnt);
  if kind <> sekNull then
    createNewFile(kind, cnt);
end;

procedure TMainToolkitForm.actionEditPasteProcessedExecute(Sender: TObject);
var
  form : TTextPasteProcessorForm;
begin
  if Context.HasFocus then
  begin
    form := TTextPasteProcessorForm.create(self);
    try
      if form.showModal = mrOk then
        Context.focus.insertText(form.Text, false);
    finally
      form.free;
    end;
  end;
end;

procedure TMainToolkitForm.actionFileCloseExecute(Sender: TObject);
begin
  closeFile(pgEditors.ActivePage, true);
end;

procedure TMainToolkitForm.actionFileManageCopyExecute(Sender: TObject);
var
  address : String;
begin
  if ChooseFileName(Context.Focus, address) then
    SaveFile(Context.Focus, address, true);
end;

procedure TMainToolkitForm.actionFileManageDeleteExecute(Sender: TObject);
begin
  if MessageDlg(context.Focus.session.caption, context.Focus.describe+' will be deleted. Really delete it?', mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    context.Focus.Session.NeedsSaving := false;
    context.Focus.Store.delete(Context.focus.Session.Address);
    actionFileCloseExecute(self);
  end;
end;

procedure TMainToolkitForm.actionFileManageFolderExecute(Sender: TObject);
var
  editor : TToolkitEditor;
  path : String;
begin
  editor := Context.Focus;
  path := ExtractFilePath(editor.session.address.substring(5));
  OpenDocument(path);
end;

procedure TMainToolkitForm.actionFileManageReloadExecute(Sender: TObject);
var
  loaded : TLoadedBytes;
begin
  loaded := Context.Focus.Store.load(Context.Focus.Session.Address, true);
  Context.Focus.LoadBytes(loaded.content);
  Context.Focus.session.NeedsSaving := false;
  Context.Focus.session.Timestamp := loaded.timestamp;
  storeOpenFileList;
  FTempStore.storeContent(Context.Focus.session.Guid, true, Context.Focus.getBytes);
  Context.Focus.lastChangeChecked := true;
end;

procedure TMainToolkitForm.actionFileNewExecute(Sender: TObject);
begin
  createNewFile(sekNull);
end;

procedure TMainToolkitForm.actionFileOpenExecute(Sender: TObject);
var
  address : String;
begin
  if FFileSystem.openDlg(address) then
    OpenFile(address);
end;

procedure TMainToolkitForm.actionFileOpenUrlExecute(Sender: TObject);
var
  mr : TModalResult;
begin
  OpenURLForm := TOpenURLForm.create(self);
  try
    FTempStore.getURLList(OpenURLForm.cbxURL.items);
    OpenURLForm.cbxURL.Text := '';
    if isAbsoluteUrl(Clipboard.AsText) then
      OpenURLForm.cbxURL.Text := Clipboard.AsText;

    repeat
      mr := OpenURLForm.ShowModal;
      case mr of
        mrOK: if openFromURL(OpenURLForm.cbxURL.Text) then
            FTempStore.addURL(OpenURLForm.cbxURL.Text)
          else
            mr := mrNone;
        mrRetry : AddServer('', OpenURLForm.cbxURL.Text);
      end;
    until mr <> mrNone;
  finally
    OpenURLForm.free;
  end;
end;

procedure TMainToolkitForm.actionFileOpenQRCodeExecute(Sender: TObject);
var
  s : String;
  cnt : TBytes;
  fmt : TSourceEditorKindSet;
begin
  QRCodeScannerForm := TQRCodeScannerForm.create(nil);
  try
    if QRCodeScannerForm.ShowModal = mrOk then
    begin
      s := QRCodeScannerForm.Memo1.Text;
      if (s.StartsWith('shc:/')) then
        createNewFile(sekJWT, TEncoding.UTF8.getBytes(s))
      else
      begin
        cnt := TEncoding.UTF8.getBytes(s);
        fmt := FFactory.determineFormatFromText(s, cnt);
        createNewFile(fmt, cnt);
      end;
    end;
  finally
    QRCodeScannerForm.free;
  end;
end;

procedure TMainToolkitForm.actionFileSaveAllExecute(Sender: TObject);
var
  editor : TToolkitEditor;
begin
  for editor in Context.editors do
    if editor.session.NeedsSaving then
      if editor.session.hasAddress then
         actionFileSaveExecute(sender);

  for editor in Context.editors do
    if editor.session.NeedsSaving then
      if not editor.session.hasAddress then
         actionFileSaveAs1Execute(sender);
end;

procedure TMainToolkitForm.actionFileSaveAs1Execute(Sender: TObject);
var
  address : String;
begin
  if (Context.Focus <> nil) then
    if ChooseFileName(Context.Focus, address) then
    begin
      Context.Focus.Session.Address := address;
      Context.Focus.Store := Context.StorageForAddress(address).link;
      SaveFile(Context.Focus, Context.Focus.Session.Address, true);
      Context.Focus.Session.Caption := Context.Focus.Store.CaptionForAddress(address);
      Context.Focus.Tab.Caption := Context.Focus.Session.Caption;
      FTempStore.removeFromMRU(address);
      storeOpenFileList;
      updateMessages(self);
      updateActionStatus(self);
    end
    else
      abort;
end;

procedure TMainToolkitForm.actionFileSaveExecute(Sender: TObject);
begin
  if (Context.Focus <> nil) then
    if Context.Focus.hasAddress and Context.Focus.Store.canSave then
      SaveFile(Context.Focus, Context.Focus.Session.Address, true)
    else
      actionFileSaveAs1Execute(sender);
end;

procedure TMainToolkitForm.actionhelpAboutExecute(Sender: TObject);
var
  frm : TToolkitAboutForm;
begin
  frm := TToolkitAboutForm.create(self);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainToolkitForm.SaveFile(editor : TToolkitEditor; address : String; updateStatus : boolean);
var
  store : TStorageService;
  dt : TDateTime;
begin
  store := Context.StorageForAddress(address);
  if (store = nil) then
  begin
    pgEditors.ActivePage := editor.tab;
    raise EFslException.Create('Unable to save to '+address);
  end;
  dt := store.Save(address, editor.getBytes);
  if updateStatus then
  begin
    editor.Session.Timestamp := dt;
    editor.Session.NeedsSaving := false;
  end;
end;

procedure TMainToolkitForm.actionHelpCheckUpgradeExecute(Sender: TObject);
var
  json : TJsonArray;
  v : string;
  md : String;
  i : integer;
  function ver(i : integer) : String;
  begin
    result := json.Obj[i].str['tag_name'];
    delete(result, 1, 1);
  end;
begin
  try
    json := TInternetFetcher.fetchJsonArray('https://api.github.com/repos/HealthIntersections/fhirserver/releases');
    try
      v := ver(0);
      if v = TOOLKIT_VERSION then
        MessageDlg('Toolkit Version', 'This is the current version ('+TOOLKIT_VERSION+')', mtInformation, [mbok], 0)
      else
      begin
        md := '';
        i := 0;
        while (i < json.Count) and TSemVer.isMoreRecent(ver(i), TOOLKIT_VERSION) do
        begin
          md := md + '## '+json.Obj[i].str['tag_name']+#13#10#13#10+json.Obj[i].str['body'].replace('\r', #13).replace('\n', #10)+#13#10#13#10;
          inc(i);
        end;
        if showUpgradeInformation(self, 'https://github.com/HealthIntersections/fhirserver/releases/tag/v'+ver(0),
              'https://github.com/HealthIntersections/fhirserver/releases/download/v2.0.0/fhirtoolkit-win64-'+ver(0)+'.exe',
            md) = mrOK then
            actionFileExit.Execute;
      end;
    finally
      json.free;
    end;
  except
    on e : exception do
      MessageDlg('Error checking for releases', e.message, mtError, [mbok], 0);
  end;
end;

procedure TMainToolkitForm.actionHelpContentExecute(Sender: TObject);
begin
  ShowMessage('Not implemented yet');
end;

procedure TMainToolkitForm.actionHelpWelcomePageExecute(Sender: TObject);
var
  editor : TToolkitEditor;
begin
  for editor in FContext.editors do
  begin
    if editor.Session.Kind = sekHome then
    begin
      pgEditors.ActivePage := editor.tab;
      exit;
    end;
  end;

  createNewFile(sekHome);
end;

procedure TMainToolkitForm.actionNewEditorExecute(Sender: TObject);
begin
  createNewFile(TSourceEditorKind((sender as TComponent).tag));
end;

procedure TMainToolkitForm.actionPagesCloseAllExecute(Sender: TObject);
var
  i, c : integer;
begin
  c := pgEditors.ActivePageIndex;
  for i := pgEditors.PageCount - 1 downto 0 do
    if i <> c then
      closeFile(pgEditors.Pages[i], false);
  storeOpenFileList;
  updateActionStatus(self);
end;

procedure TMainToolkitForm.actionPagesCloseLeftExecute(Sender: TObject);
var
  i, c : integer;
begin
  c := pgEditors.ActivePageIndex;
  for i := c - 1 downto 0 do
    closeFile(pgEditors.Pages[i], false);
  storeOpenFileList;
  updateActionStatus(self);
end;

procedure TMainToolkitForm.actionPagesCloseRightExecute(Sender: TObject);
var
  i, c : integer;
begin
  c := pgEditors.ActivePageIndex;
  for i := pgEditors.PageCount - 1 downto c+1 do
    closeFile(pgEditors.Pages[i], false);
  storeOpenFileList;
  updateActionStatus(self);
end;

procedure TMainToolkitForm.actionPagesMoveFarleftExecute(Sender: TObject);
begin
  pgEditors.ActivePage.PageIndex := 0;
  updateActionStatus(self);
  storeOpenFileList;
end;

procedure TMainToolkitForm.actionPagesMoveFarRIghtExecute(Sender: TObject);
begin
  pgEditors.ActivePage.PageIndex := pgEditors.PageCount - 1;
  updateActionStatus(self);
  storeOpenFileList;
end;

procedure TMainToolkitForm.actionPagesMoveLeftExecute(Sender: TObject);
begin
  pgEditors.ActivePage.PageIndex := pgEditors.ActivePage.PageIndex - 1;
  updateActionStatus(self);
  storeOpenFileList;
end;

procedure TMainToolkitForm.actionPagesMoveRightExecute(Sender: TObject);
begin
  pgEditors.ActivePage.PageIndex := pgEditors.ActivePage.PageIndex + 1;
  updateActionStatus(self);
  storeOpenFileList;
end;

procedure TMainToolkitForm.actionToolsOptionsExecute(Sender: TObject);
begin
  ToolkitSettingsForm := TToolkitSettingsForm.create(self);
  try
    ToolkitSettingsForm.lblEditorFont.Font.assign(SynEdit1.font);
    ToolkitSettingsForm.lblLogFont.Font.assign(mConsole.font);
    ToolkitSettingsForm.lblViewFont.Font.assign(lvMessages.Font);
    ToolkitSettingsForm.chkSideBySide.Checked := actionToolsSideBySideMode.Checked;
    ToolkitSettingsForm.DiffTool := FIni.ReadString('Tools', 'Diff', '');
    ToolkitSettingsForm.edtTxServer.Text := FIni.ReadString('tx', 'server', 'http://tx.fhir.org/r4');
    ToolkitSettingsForm.edtTxLog.Text := FIni.ReadString('tx', 'log', '');
    ToolkitSettingsForm.edtCache.Text := IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'fhir-tx.cache';
    if ToolkitSettingsForm.ShowModal = mrOk then
    begin
      SynEdit1.font.assign(ToolkitSettingsForm.lblEditorFont.Font);
      Context.updateFont;
      mConsole.font.assign(ToolkitSettingsForm.lblLogFont.Font);
      lvMessages.Font.assign(ToolkitSettingsForm.lblViewFont.Font);
      actionToolsSideBySideMode.Checked := ToolkitSettingsForm.chkSideBySide.Checked;
      Context.SideBySide := actionToolsSideBySideMode.Checked;
      FIni.WriteBool('Settings', 'SideBySide', Context.SideBySide);
      FIni.WriteString('Tools', 'Diff', ToolkitSettingsForm.DiffTool);
      FIni.WriteString('tx', 'server', ToolkitSettingsForm.edtTxServer.Text);
      FIni.WriteString('tx', 'log', ToolkitSettingsForm.edtTxLog.Text);
      copyFonts;
      saveFonts;
    end;
  finally
    ToolkitSettingsForm.Free;
  end;
end;

procedure TMainToolkitForm.actionToolsPackageManagerExecute(Sender: TObject);
begin
  PackageCacheForm := TPackageCacheForm.create(self);
  try
    PackageCacheForm.Ini := FIni;
    PackageCacheForm.showModal;
  finally
    PackageCacheForm.free;
  end;
end;

procedure TMainToolkitForm.actionToolsSideBySideModeExecute(Sender: TObject);
begin
  actionToolsSideBySideMode.Checked := not actionToolsSideBySideMode.Checked;
  Context.SideBySide := actionToolsSideBySideMode.Checked;
  FIni.WriteBool('Settings', 'SideBySide', Context.SideBySide);
end;

procedure TMainToolkitForm.MenuItem34Click(Sender: TObject);
begin

end;

procedure TMainToolkitForm.MenuItem60Click(Sender: TObject);
begin
  openFile('file:'+FIni.FileName);
end;

procedure TMainToolkitForm.MenuItem98Click(Sender: TObject);
begin

end;

procedure TMainToolkitForm.mnuLVCopyAllClick(Sender: TObject);
var
  i : integer;
  msg : TToolkitMessage;
  b : TStringBuilder;
begin
  b := TStringBuilder.create;
  try
    for i := 0 to lvMessages.items.count - 1 do
    begin
      msg := TToolkitMessage(lvMessages.items[i].Data);
      b.append(msg.Summary);
      b.append(EOLN);
    end;
  finally
    b.Free;
  end;
end;

procedure TMainToolkitForm.mnuLVCopyClick(Sender: TObject);
var
  msg : TToolkitMessage;
begin
  msg := TToolkitMessage(lvMessages.Selected.Data);
  Clipboard.AsText := msg.Summary;
end;

procedure TMainToolkitForm.mnuLVGoClick(Sender: TObject);
var
  msg : TToolkitMessage;
begin
  msg := TToolkitMessage(lvMessages.Selected.Data);
  pgEditors.ActivePage := msg.editor.tab;
  msg.editor.locate(msg.Location);
end;

procedure TMainToolkitForm.mnuRecentClick(Sender: TObject);
var
  ts : TStringList;
  i : integer;
  item : TMenuItem;
begin
  ts := TStringList.create;
  try
    FTempStore.getMRUList(ts);
    mnuRecent.Clear;
    for i := 0 to ts.count - 1 do
    begin
      item := TMenuItem.create(nil);
      item.Caption := ts[i];
      item.Tag := i;
      item.OnClick := openMRUItem;
      mnuRecent.Add(item);
    end;
    if (ts.count = 0) then
    begin
      item := TMenuItem.create(nil);
      item.Caption := '(none)';
      item.Enabled := false;
      mnuRecent.Add(item);
    end;
  finally
    ts.free;
  end;
end;

procedure TMainToolkitForm.mnuSearchGoClick(Sender: TObject);
var
  msg : TToolkitSearchMatch;
  editor : TToolkitEditor;
begin
  msg := TToolkitSearchMatch(lvSearch.Selected.Data);
  editor := FContext.EditorForAddress(msg.address);
  if (editor = nil) then
    editor := openFile(msg.address);
  pgEditors.ActivePage := editor.tab;
  editor.locate(msg.Location);
end;

procedure TMainToolkitForm.NewFromFormatClick(Sender: TObject);
begin
  createNewFile(TSourceEditorKind((Sender as TMenuItem).tag));
end;

procedure TMainToolkitForm.MenuItem79Click(Sender: TObject);
begin

end;

procedure TMainToolkitForm.pgEditorsChange(Sender: TObject);
begin
  if FContext.Focus <> nil then
    FContext.Focus.loseFocus();
  FContext.Focus := FContext.EditorForTab(pgEditors.ActivePage);
  clearContentMenu;
  if FContext.Focus <> nil then
  begin
    FContext.Focus.getFocus(mnuContent);
    FContext.Focus.editPause;
  end;
  checkActiveTabCurrency;
  updateActionStatus(self);
  updateStatusBar;
end;

procedure TMainToolkitForm.pgEditorsCloseTabClicked(Sender: TObject);
begin
  closeFile(pgEditors.ActivePage, true);
end;

procedure TMainToolkitForm.pgEditorsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  index: integer;
  tab : TTabsheet;
  editor : TToolkitEditor;
  hint : String;
begin
  hint := '';
  if (y > 0) then
    exit;
  index := pgEditors.IndexOfTabAt(X, Y);
  if (index >= 0)  then
  begin
    tab := pgEditors.Pages[index];
    editor := Context.EditorForTab(tab);
    if (editor <> nil) then
      hint := editor.Hint;
  end;
  if (pgEditors.Hint <> hint) then
  begin
    if (hint <> '') then
    begin
      Application.CancelHint;
      pgEditors.Hint := Hint;
      pgEditors.ShowHint := true;
    end
    else
    begin
      pgEditors.Hint := '';
      pgEditors.ShowHint := false;
    end;
  end;
end;

procedure TMainToolkitForm.pmMessageViewPopup(Sender: TObject);
begin
  mnuLVGo.enabled := lvMessages.Selected <> nil;
  mnuLVCopy.enabled := lvMessages.Selected <> nil;
  mnuLVCopyAll.enabled := lvMessages.Items.count > 0;
end;

procedure TMainToolkitForm.pnlRightClick(Sender: TObject);
begin

end;

procedure TMainToolkitForm.placeView(panel : TPanel; id : TViewManagerPanelId);
var
  loc : TViewManagerLocation;
  pg : TPageControl;
  ndx : integer;
  tab : TTabSheet;
  stack : TPanelStack;
begin
  loc := FViewManager.location(id);
  ndx := FViewManager.index(id);
  if FViewManager.tabbed[loc] then
  begin
    case loc of
      tvlLeft : pg := pgLeft;
      tvlRight : pg := pgRight;
      tvlBottom : pg := pgBottom;
    end;
    if ndx < pg.PageCount then
      tab := pg.Pages[ndx]
    else
    begin
      tab := pg.AddTabSheet;
    end;
    tab.Caption := TITLES_TViewManagerPanelId[id];
    tab.ImageIndex := ICONS_TViewManagerPanelId[id];
    tab.TabVisible := true;
    FViews[id] := tab;
    panel.Parent := tab;
  end
  else
  begin
    if loc = tvlLeft then
      stack := FLeftStack
    else
      stack := FRightStack;
    stack.setCaption(ndx, TITLES_TViewManagerPanelId[id], ICONS_TViewManagerPanelId[id]);
    FViews[id] := stack.panel[ndx];
    panel.Parent := stack.panel[ndx];
//    stack.setSize(ndx,
  end;
end;

procedure TMainToolkitForm.hideTabs(pg : TPageControl);
var
  i : integer;
begin
  for i := 0 to pg.PageCount - 1 do
    pg.Pages[i].TabVisible := false;
end;

procedure TMainToolkitForm.ApplyViewLayout;
begin
  BeginFormUpdate;
  FDoingLayout := true;
  try
    if FViewManager.scale <> FScale then
      setScale(FViewManager.scale);

    pnlLeft.Width := FViewManager.size(tvlLeft, pnlLeft.Width);
    pnlRight.Width := FViewManager.size(tvlRight, pnlRight.Width);
    pnlBottom.Height := FViewManager.size(tvlBottom, pnlBottom.Height);

    if FViewManager.srcIsMaximised then
      maximiseSource
    else
      unmaximiseSource;

    // hide all tabs
    hideTabs(pgLeft);
    hideTabs(pgRight);
    hideTabs(pgBottom);
    if (FViewManager.tabbed[tvlLeft]) then
    begin
      pgLeft.Visible := true;
      pnlLeftSpace.visible := false;
    end
    else
    begin
      pnlLeftSpace.visible := true;
      FLeftStack.reset(FViewManager.count(tvlLeft));
      pgLeft.Visible := false;
    end;
    if (FViewManager.tabbed[tvlRight]) then
    begin
      pgRight.Visible := true;
      pnlRightSpace.visible := false;
    end
    else
    begin
      FRightStack.reset(FViewManager.count(tvlRight));
      pnlRightSpace.visible := true;
      pgRight.Visible := false;
    end;

    // fill out views
    placeView(pnlProjects, tviProjects);
    placeView(pnlServers, tviServers);
    placeView(pnlInspector, tviInspector);
    placeView(pnlVariables, tviVariables);
    placeView(pnlStack, tviStack);
    placeView(pnlMessages, tviMessages);
    placeView(pnlLog, tviLog);
    placeView(pnlSearch, tviSearch);
    placeView(pnlBreakpoints, tviBreakpoints);
    placeView(pnlTasks, tviTasks);
    placeView(pnlFHIRPath, tviFHIRPath);
    placeView(pnlPackages, tviPackages);

    if (not FViewManager.tabbed[tvlLeft]) then
      FLeftStack.resize;
    if (not FViewManager.tabbed[tvlRight]) then
      FRightStack.resize;

    pgLeft.ActivePageIndex := FViewManager.active[tvlLeft];
    pgRight.ActivePageIndex := FViewManager.active[tvlRight];
    pgBottom.ActivePageIndex := FViewManager.active[tvlBottom];

    tbView1.Visible := FViewManager.showToolbarButtons;
    tbView2.Visible := FViewManager.showToolbarButtons;
    tbView3.Visible := FViewManager.showToolbarButtons;
    tbView4.Visible := FViewManager.showToolbarButtons;
    tbView5.Visible := FViewManager.showToolbarButtons;
    tbView6.Visible := FViewManager.showToolbarButtons;
    tbView7.Visible := FViewManager.showToolbarButtons;
    tbView8.Visible := FViewManager.showToolbarButtons;
    tbView9.Visible := FViewManager.showToolbarButtons;
    tbView10.Visible := FViewManager.showToolbarButtons;
    tbView11.Visible := FViewManager.showToolbarButtons;
  finally
    FDoingLayout := false;
    EndFormUpdate;
  end;
end;

procedure TMainToolkitForm.startLoadingContexts;
  procedure setLoadContext(context : TFHIRWorkerContextWithFactory; pid : String);
  var
    req : TFHIRLoadContextTaskRequest;
    resp : TFHIRLoadContextTaskResponse;
  begin
    req := TFHIRLoadContextTaskRequest.create;
    try
      req.context := context;
      req.packages.add(pid);
      req.userMode := true;
      resp := TFHIRLoadContextTaskResponse.create;
      try
        resp.context := context.link;
        GBackgroundTasks.queueTask(GContextLoaderTaskId, req.link, resp.link, doContextLoaded);
      finally
        resp.free;
      end;
    finally
      req.free;
    end;
  end;
begin
  //SetLoadContext(TToolkitValidatorContext.create(FContext.Languages.link, makeFactory(fhirVersionRelease3), FIni.ReadString('tx', 'server', 'http://tx.fhir.org/r3'), FContext.pcm), 'hl7.fhir.r3.core');
  //SetLoadContext(TToolkitValidatorContext.create(FContext.Languages.link, makeFactory(fhirVersionRelease4), FIni.ReadString('tx', 'server', 'http://tx.fhir.org/r4'), FContext.pcm), 'hl7.fhir.r4.core');
end;

procedure TMainToolkitForm.doContextLoaded(id: integer; response: TBackgroundTaskResponsePackage);
begin
  FContext.context[(response as TFHIRLoadContextTaskResponse).context.Factory.version] := (response as TFHIRLoadContextTaskResponse).context.link;
end;

procedure TMainToolkitForm.storeOpenFileList;
var
  list : TJsonArray;
  i : integer;
  session : TToolkitEditSession;
begin
  list := FTempStore.startOpenFileList;
  for i := 0 to pgEditors.PageCount - 1 do
  begin
    session := FContext.EditorForTab(pgEditors.Pages[i]).Session;
    FTempStore.storeOpenFile(list, session);
  end;
  FTempStore.finishOpenFileList(list);
end;

procedure TMainToolkitForm.checkWelcomePage;
var
  editor : TToolkitEditor;
begin
  for editor in FContext.Editors do
    if (editor.Session.kind = sekHome) and (editor.Session.Info.Values['auto'] = 'true') then
      closeFile(editor.Tab, false);
end;

procedure TMainToolkitForm.Splitter1Moved(Sender: TObject);
begin
  if FDoingLayout then exit;
  FViewManager.setsize(tvlBottom, pnlBottom.Height);
end;

procedure TMainToolkitForm.Splitter3Moved(Sender: TObject);
begin
  if FDoingLayout then exit;
  FViewManager.setsize(tvlLeft, pnlLeft.Width);
end;

procedure TMainToolkitForm.Splitter2Moved(Sender: TObject);
begin
  if FDoingLayout then exit;
  FViewManager.setsize(tvlRight, pnlRight.Width);
end;

procedure TMainToolkitForm.pgBottomChange(Sender: TObject);
begin
  if FDoingLayout then exit;
  FViewManager.active[tvlBottom] := pgBottom.ActivePageIndex;
end;

procedure TMainToolkitForm.pgLeftChange(Sender: TObject);
begin
  if FDoingLayout then exit;
  FViewManager.active[tvlLeft] := pgLeft.ActivePageIndex;
end;

procedure TMainToolkitForm.pgRightChange(Sender: TObject);
begin
  if FDoingLayout then exit;
  FViewManager.active[tvlRight] := pgRight.ActivePageIndex;
end;


end.



