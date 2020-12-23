unit frm_main;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, Math,
  ComCtrls, ActnList, StdActns, IniFiles, Clipbrd, Buttons, StdCtrls, SynEdit,
  lclintf, ValEdit,

  fsl_base, fsl_utilities, fsl_stream, fsl_threads, fsl_fpc, fsl_logging,
  ftk_context, ftk_store_temp,
  ftk_store, ftk_store_files,
  ftk_factory, ftk_search,

  fui_lcl_cache, frm_file_format, frm_settings, frm_about, frm_edit_changes;

type
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
    actionEditReview: TAction;
    actionEditFindNext: TAction;
    actionEditFindPrev: TAction;
    actionEditFind: TAction;
    actionViewReset: TAction;
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
    actionEditPasteSpecial: TAction;
    actionEditCopyFilename: TAction;
    actionCopyFileTitle: TAction;
    actionCopyFilePath: TAction;
    actionCopyFile: TAction;
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
    btnSearch: TBitBtn;
    chkCase: TCheckBox;
    chkWholeWord: TCheckBox;
    chkhideHintsAndWarnings: TCheckBox;
    chkTaskInactive: TCheckBox;
    chkCurrrentFileOnly: TCheckBox;
    cbxSearch: TComboBox;
    cbxSearchType: TComboBox;
    cbxSearchScope: TComboBox;
    imgMain: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lvSearch: TListView;
    lvTasks: TListView;
    lvMessages: TListView;
    MainMenu1: TMainMenu;
    mConsole: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
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
    MenuItem40: TMenuItem;
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
    MenuItem54: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
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
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    btnSearchFolder: TSpeedButton;
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
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton3: TToolButton;
    ToolButton30: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    vlInspector: TValueListEditor;
    procedure actionEditBeginEndExecute(Sender: TObject);
    procedure actionEditCopyExecute(Sender: TObject);
    procedure actionEditFindExecute(Sender: TObject);
    procedure actionEditFindNextExecute(Sender: TObject);
    procedure actionEditFindPrevExecute(Sender: TObject);
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
    procedure actionFileSaveAllExecute(Sender: TObject);
    procedure actionFileSaveAs1Execute(Sender: TObject);
    procedure actionFileSaveExecute(Sender: TObject);
    procedure actionhelpAboutExecute(Sender: TObject);
    procedure actionHelpCheckUpgradeExecute(Sender: TObject);
    procedure actionHelpContentExecute(Sender: TObject);
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
    procedure actionViewMessagesExecute(Sender: TObject);
    procedure actionViewPackagesExecute(Sender: TObject);
    procedure actionViewProjectManagerExecute(Sender: TObject);
    procedure actionViewResetExecute(Sender: TObject);
    procedure actionViewsClearLogExecute(Sender: TObject);
    procedure actionViewSearchExecute(Sender: TObject);
    procedure actionViewServersExecute(Sender: TObject);
    procedure actionViewStackExecute(Sender: TObject);
    procedure actionViewTasksExecute(Sender: TObject);
    procedure actionViewVariablesExecute(Sender: TObject);
    procedure actionZoomInExecute(Sender: TObject);
    procedure actionZoomOutExecute(Sender: TObject);
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
    procedure MenuItem34Click(Sender: TObject);
    procedure MenuItem60Click(Sender: TObject);
    procedure mnuLVCopyAllClick(Sender: TObject);
    procedure mnuLVCopyClick(Sender: TObject);
    procedure mnuLVGoClick(Sender: TObject);
    procedure mnuRecentClick(Sender: TObject);
    procedure mnuSearchGoClick(Sender: TObject);
    procedure NewFromFormatClick(Sender: TObject);
    procedure MenuItem79Click(Sender: TObject);
    procedure pgEditorsChange(Sender: TObject);
    procedure pgEditorsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pmMessageViewPopup(Sender: TObject);
    procedure pmPagesPopup(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure Splitter3Moved(Sender: TObject);
    procedure tbLogShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure updateMessages(sender : TObject);
  private
    FIni : TIniFile;
    FTempStore : TFHIRToolkitTemporaryStorage;
    FFileSystem : TStorageService;
    FSourceMaximised : boolean;
    FContext : TToolkitContext;
    FFactory : TToolkitFactory;
    FFinishedLoading : boolean;
    FScale : integer;
    FSearchTask : integer;
    FSearch : TFslList<TToolkitSearchMatch>;
    FShuttingDown : boolean;
    function checkDoSave(editor : TToolkitEditor): boolean;
    procedure copyFonts;
    procedure loadFont(font: TFont; sname: String);
    procedure SaveFile(editor: TToolkitEditor; address : String; updateStatus : boolean);
    procedure saveFont(font: TFont; sname : String);
    procedure saveLayout;
    procedure loadLayout;
    procedure loadSearch;
    procedure saveSearch;
    procedure maximiseSource;
    procedure showView(pnl: TPanel; pg: TPageControl; tab: TTabSheet);
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
    procedure createNewFile(kind : TSourceEditorKind);
    function openFile(address : String) : TToolkitEditor;
    procedure onChangeFocus(sender : TObject);
    procedure updateInspector(sender : TObject);
    procedure closeFile(tab : TTabSheet; store : boolean);
    procedure locateOnTab(sender : TObject; x, y: integer; var point : TPoint);
    function ChooseFileName(editor : TToolkitEditor; out address : String): boolean;
    procedure setScale(pct : integer);
    procedure doSearch;
    procedure doProcessSearchResults(id : integer; response : TBackgroundTaskResponsePackage);
    procedure processSearchResults(results : TFslList<TToolkitSearchMatch>; goFirst : boolean);
  public
    property Context : TToolkitContext read FContext;
  end;

var
  MainToolkitForm: TMainToolkitForm;

implementation

{$R *.lfm}

{ TMainToolkitForm }

procedure TMainToolkitForm.FormCreate(Sender: TObject);
begin
  Application.OnActivate := DoAppActivate;
  Application.OnException := DoAppException;
  initialiseTZData(partnerFile('tzdata.tar.gz'));

  GBackgroundTasks.start;
  FSearchTask := GBackgroundTasks.registerTaskEngine(TToolkitSearchTaskEngine.create);
  FIni := TIniFile.create(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'fhir-toolkit.ini');
  FTempStore := TFHIRToolkitTemporaryStorage.create;
  loadLayout;
  loadSearch;

  FContext := TToolkitContext.create(imgMain, actList);
  FContext.OnUpdateActions := updateActionStatus;
  FContext.OnLocate := locateOnTab;
  FContext.OnChangeFocus := onChangeFocus;
  FContext.MessageView.OnChange := updateMessages;
  FContext.Inspector.OnChange := updateInspector;
  FContext.Font := SynEdit1.Font;

  FContext.SideBySide := FIni.readBool('Settings', 'SideBySide', false);
  actionToolsSideBySideMode.Checked := FContext.SideBySide;

  FFileSystem := TFileStorageService.create(self, FIni);
  FContext.storages.add(FFileSystem.link);
  FSearch := TFslList<TToolkitSearchMatch>.create;
  FFactory := TToolkitFactory.create(FContext.link, self);
  updateActionStatus(nil);
end;

procedure TMainToolkitForm.FormDestroy(Sender: TObject);
begin
  FShuttingDown := true;
  Timer1.Enabled := false;
  FSearch.Free;
  FFactory.free;
  FFileSystem.Free;
  FTempStore.Free;
  FContext.Free;
  saveLayout;
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
begin
  sessions := TFslList<TToolkitEditSession>.create;
  try
    FTempStore.fetchOpenList(sessions);
    for session in sessions do
    begin
      ns := session.NeedsSaving;
      editor := FFactory.makeEditor(session.link);
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

  for i := 1 to ParamCount do
    if (FileExists(ParamStr(i))) then
      openFile('file:'+ParamStr(i))
    else
      openFile(ParamStr(i));

  if pgEditors.PageCount > 0 then
  begin
    tab := pgEditors.Pages[0];
    pgEditors.ActivePage := tab;
    FContext.Focus := FContext.EditorForTab(tab);
    FContext.focus.getFocus(mnuContent);
    updateActionStatus(editor);
  end;
  FContext.ToolBarHeight := ToolBar1.Height;
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

procedure TMainToolkitForm.loadLayout;
begin
  FScale := FIni.ReadInteger('main-form', 'scale', 100);
  if FIni.readBool('main-form', 'maximised', false) then
    WindowState := wsMaximized
  else
  begin
    Top := FIni.readInteger('main-form', 'top', Top);
    Left := FIni.readInteger('main-form', 'left', Left);
    Height := FIni.readInteger('main-form', 'height', Height);
    Width := FIni.readInteger('main-form', 'width', Width);
  end;
  pnlLeft.Width := FIni.readInteger('main-form', 'left.width', pnlLeft.Width);
  pnlRight.Width := FIni.readInteger('main-form', 'right.width', pnlRight.Width);
  pnlBottom.Height := FIni.ReadInteger('main-form', 'bottom.height', pnlBottom.Height);
  if FIni.readBool('main-form', 'source.maximised', false) then
    maximiseSource;

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

procedure TMainToolkitForm.saveLayout;
begin
  FIni.WriteInteger('main-form', 'scale', FScale);
  FIni.WriteBool('main-form', 'maximised', WindowState = wsMaximized);
  if WindowState <> wsMaximized then
  begin
    FIni.WriteInteger('main-form', 'top', Top);
    FIni.WriteInteger('main-form', 'left', Left);
    FIni.WriteInteger('main-form', 'height', Height);
    FIni.WriteInteger('main-form', 'width', Width);
  end;
  FIni.WriteInteger('main-form', 'panel.left.width', pnlLeft.Width);
  FIni.WriteInteger('main-form', 'panel.right.width', pnlRight.Width);
  FIni.WriteInteger('main-form', 'panel.bottom.height', pnlBottom.Height);
  FIni.WriteBool('main-form', 'source.maximised', FSourceMaximised);

  saveFont(SynEdit1.font, 'font-editor');
  saveFont(lvMessages.font, 'font-view');
  saveFont(mConsole.font, 'font-log');
end;

procedure TMainToolkitForm.Splitter3Moved(Sender: TObject);
begin
  saveLayout;
end;

procedure TMainToolkitForm.tbLogShow(Sender: TObject);
begin
  tbLog.ImageIndex := 45;
end;

procedure TMainToolkitForm.Timer1Timer(Sender: TObject);
begin
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

procedure TMainToolkitForm.Splitter1Moved(Sender: TObject);
begin
  saveLayout;
end;

procedure TMainToolkitForm.Splitter2Moved(Sender: TObject);
begin
  saveLayout;
end;

procedure TMainToolkitForm.maximiseSource;
begin
  pnlLeft.visible := false;
  pnlRight.visible := false;
  pnlBottom.visible := false;
  Splitter1.enabled := false;
  Splitter2.enabled := false;
  Splitter3.enabled := false;
  FSourceMaximised := true;
end;

procedure TMainToolkitForm.unmaximiseSource;
begin
  pnlLeft.visible := true;
  pnlRight.visible := true;
  pnlBottom.visible := true;
  Splitter1.enabled := true;
  Splitter2.enabled := true;
  Splitter3.enabled := true;
  FSourceMaximised := false;
end;

procedure TMainToolkitForm.updateUI;
begin
  updateStatusBar;
  updateConsole;
  updateActions;
  updateTasks;
end;

procedure TMainToolkitForm.updateTasks;
var
  list : TFslList<TBackgroundTaskStatusInfo>;
  entry : TListItem;
  item : TBackgroundTaskStatusInfo;
begin
  list := TFslList<TBackgroundTaskStatusInfo>.create;
  try
    GBackgroundTasks.report(list);
    lvTasks.BeginUpdate;
    try
      lvTasks.items.clear;
      for item in list do
        if chkTaskInactive.Checked or not (item.status in [btsWaiting, btsClosed]) then
        begin
          entry := lvTasks.items.add;
          entry.Caption := item.name;
          entry.subitems.add(item.StatusDisplay);
          entry.subitems.add(item.PctDisplay);
          entry.subitems.add(item.Message);
        end;
    finally
      lvTasks.EndUpdate;
    end;
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
  if FFinishedLoading and not FShuttingDown and Context.hasFocus and Context.focus.hasStore and Context.focus.Store.CheckTimes then
  begin
    loaded := Context.focus.Store.load(Context.focus.session.address);
    if loaded.timestamp <> Context.focus.session.Timestamp then
    begin
      if (MessageDlg(Context.focus.session.caption, Context.focus.describe+' has been changed since you loaded it. Reload?', mtConfirmation, mbYesNo, 0) = mrYes) then
        actionFileManageReloadExecute(self);
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

procedure TMainToolkitForm.createNewFile(kind : TSourceEditorKind);
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
        if FileFormatChooser.ShowModal = mrOK then
          kind := TSourceEditorKind(FileFormatChooser.ListBox1.ItemIndex + 1)
        else
          abort;
      finally
        FileFormatChooser.free;
      end;
    end
    else if kind = sekFHIR then
    begin
      FileFormatChooser := TFileFormatChooser.create(self);
      try
        FileFormatChooser.setFHIRResource;
        if FileFormatChooser.ShowModal = mrOK then
          kind := TSourceEditorKind(FileFormatChooser.ListBox1.ItemIndex + 1)
        else
          abort;
      finally
        FileFormatChooser.free;
      end;
    end;

    session := FFactory.makeNewSession(kind);
    editor := FFactory.makeEditor(session);
    FContext.addEditor(editor);
    tab := pgEditors.AddTabSheet;
    editor.bindToTab(tab);
    editor.newContent;
    editor.session.NeedsSaving := false;
    editor.lastChangeChecked := true;
    pgEditors.ActivePage := tab;
    FTempStore.storeOpenFileList(FContext.EditorSessions);
    FTempStore.storeContent(editor.session.Guid, true, editor.getBytes);
    FContext.Focus := editor;
    FContext.Focus.getFocus(mnuContent);
    updateActionStatus(editor);
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
    loaded := store.load(address);
    session := FFactory.examineFile(address, loaded.content);
    if session <> nil then
    begin
      session.address := address;
      session.Timestamp := loaded.timestamp;
      editor := FFactory.makeEditor(session);
      FContext.addEditor(editor);
      tab := pgEditors.AddTabSheet;
      editor.bindToTab(tab);
      editor.LoadBytes(loaded.content);
      editor.session.NeedsSaving := false;
      pgEditors.ActivePage := tab;
      FTempStore.storeOpenFileList(FContext.EditorSessions);
      FTempStore.storeContent(editor.session.Guid, true, editor.getBytes);
      FTempStore.removeFromMRU(editor.session.address);
      editor.lastChangeChecked := true;
      FContext.Focus := editor;
      FContext.Focus.getFocus(mnuContent);
      editor.lastChangeChecked := true;
      editor.lastMoveChecked := true;
      editor.editPause;
      updateActionStatus(editor);
      result := editor;
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
    vlInspector.Enabled := true;
    vlInspector.Color := clWhite;
    vlInspector.FixedColor := clBtnFace;
    vlInspector.FixedCols := 1;
    vlInspector.Strings.Assign(Context.Inspector.Content);
  end
  else
  begin
    vlInspector.Enabled := false;
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
begin
  editor := Context.EditorForTab(tab);
  if (editor.CanBeSaved) then
    checkDoSave(editor);
  if editor.session.Address <> '' then
    FTempStore.addToMru(editor.session.Address, editor.session.caption);
  Context.removeEditor(editor);
  tab.free;
  if (store) then
    FTempStore.storeOpenFileList(FContext.EditorSessions);
  pgEditorsChange(self);
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
        tskFolder : if not FolderExists(spec.scope) then raise Exception.create('Folder "'+spec.scope+'" not found');
        tskFolderTree : if not FolderExists(spec.scope) then raise Exception.create('Folder "'+spec.scope+'" not found');
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
  actionCopyFile.enabled := context.hasFocus and context.Focus.isFile;
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
  actionEditPasteSpecial.enabled := context.hasFocus and context.Focus.canPaste;
  actionEditDelete.enabled := context.hasFocus and context.Focus.canCut;
  actionEditPaste.enabled := context.hasFocus and context.Focus.canPaste;

  // enabled if we're in source mode
  actionEditBeginEnd.enabled := context.hasFocus and not context.Focus.IsShowingDesigner;
  actionZoomIn.enabled := true;
  actionZoomOut.enabled := true;
  actionFilePrint.enabled := context.hasFocus and not context.Focus.IsShowingDesigner;
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
  if FSourceMaximised then
    unmaximiseSource
  else
    maximiseSource;
end;

procedure TMainToolkitForm.showView(pnl : TPanel; pg : TPageControl; tab : TTabSheet);
begin
  if FSourceMaximised then
    unmaximiseSource;
  pg.ActivePage := tab;
end;

procedure TMainToolkitForm.actionViewExpressionEditorExecute(Sender: TObject);
begin
  showView(pnlLeft, pgLeft, tbExpression);
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
  showView(pnlRight, pgRight, tbInspector);
end;

procedure TMainToolkitForm.actionViewLogExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbLog);
end;

procedure TMainToolkitForm.actionViewMessagesExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbMessages);
end;

procedure TMainToolkitForm.actionViewPackagesExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbPackages);
end;

procedure TMainToolkitForm.actionViewProjectManagerExecute(Sender: TObject);
begin
  showView(pnlLeft, pgLeft, tbProjects);
end;

procedure TMainToolkitForm.actionViewResetExecute(Sender: TObject);
begin
  BeginFormUpdate;
  try
    setScale(100);
    pnlLeft.Width := 170;
    pgLeft.ActivePage := tbProjects;
    pnlRight.Width := 230;
    pgRight.ActivePage := tbInspector;
    pnlBottom.Height := 130;
    pgBottom.ActivePage := tbMessages;
    if FSourceMaximised then
      unmaximiseSource
  finally
    EndFormUpdate;
  end;
end;

procedure TMainToolkitForm.actionViewsClearLogExecute(Sender: TObject);
begin
  mConsole.Clear;
  actionViewsClearLog.enabled := false;
end;

procedure TMainToolkitForm.actionViewSearchExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbSearch);
  cbxSearch.SetFocus;
end;

procedure TMainToolkitForm.actionViewServersExecute(Sender: TObject);
begin
  showView(pnlLeft, pgLeft, tbServers);
end;

procedure TMainToolkitForm.actionViewStackExecute(Sender: TObject);
begin
  showView(pnlRight, pgRight, tbStack);
end;

procedure TMainToolkitForm.actionViewTasksExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbTasks);
end;

procedure TMainToolkitForm.actionViewVariablesExecute(Sender: TObject);
begin
  showView(pnlRight, pgRight, tbVariables);
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
  FTempStore.storeOpenFileList(Context.editorSessions);
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
    FTempStore.storeOpenFileList(Context.editorSessions);
    Context.StorageForAddress(old).delete(old);
    updateMessages(self);
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
      Context.Focus.loadBytes(TEncoding.UTF8.getBytes(frm.mSource.Lines.Text));
  finally
    frm.Free;
  end;
end;

procedure TMainToolkitForm.actionEditBeginEndExecute(Sender: TObject);
begin
  Context.Focus.BeginEndSelect;
end;

procedure TMainToolkitForm.actionEditCopyExecute(Sender: TObject);
begin

end;

procedure TMainToolkitForm.actionEditFindExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbSearch);
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
  loaded := Context.Focus.Store.load(Context.Focus.Session.Address);
  Context.Focus.LoadBytes(loaded.content);
  Context.Focus.session.NeedsSaving := false;
  Context.Focus.session.Timestamp := loaded.timestamp;
  FTempStore.storeOpenFileList(FContext.EditorSessions);
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
begin

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
      FTempStore.storeOpenFileList(Context.editorSessions);
      updateMessages(self);
      updateActionStatus(self);
    end
    else
      abort;
end;

procedure TMainToolkitForm.actionFileSaveExecute(Sender: TObject);
begin
  if (Context.Focus <> nil) then
    if Context.Focus.hasAddress then
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
    raise Exception.Create('Unable to save to '+address);
  end;
  dt := store.Save(address, editor.getBytes);
  if updateStatus then
  begin
    editor.Session.Timestamp := dt;
    editor.Session.NeedsSaving := false;
  end;
end;

procedure TMainToolkitForm.actionHelpCheckUpgradeExecute(Sender: TObject);
begin
  ShowMessage('Not implemented until there''s a release');
end;

procedure TMainToolkitForm.actionHelpContentExecute(Sender: TObject);
begin
  ShowMessage('Not implemented yet');
end;

procedure TMainToolkitForm.actionPagesCloseAllExecute(Sender: TObject);
var
  i, c : integer;
begin
  c := pgEditors.ActivePageIndex;
  for i := pgEditors.PageCount - 1 downto 0 do
    if i <> c then
      closeFile(pgEditors.Pages[i], false);
  FTempStore.storeOpenFileList(FContext.EditorSessions);
  updateActionStatus(self);
end;

procedure TMainToolkitForm.actionPagesCloseLeftExecute(Sender: TObject);
var
  i, c : integer;
begin
  c := pgEditors.ActivePageIndex;
  for i := c - 1 downto 0 do
    closeFile(pgEditors.Pages[i], false);
  FTempStore.storeOpenFileList(FContext.EditorSessions);
  updateActionStatus(self);
end;

procedure TMainToolkitForm.actionPagesCloseRightExecute(Sender: TObject);
var
  i, c : integer;
begin
  c := pgEditors.ActivePageIndex;
  for i := pgEditors.PageCount - 1 downto c+1 do
    closeFile(pgEditors.Pages[i], false);
  FTempStore.storeOpenFileList(FContext.EditorSessions);
  updateActionStatus(self);
end;

procedure TMainToolkitForm.actionPagesMoveFarleftExecute(Sender: TObject);
begin
  pgEditors.ActivePage.PageIndex := 0;
  updateActionStatus(self);
  FTempStore.storeOpenFileList(FContext.EditorSessions);
end;

procedure TMainToolkitForm.actionPagesMoveFarRIghtExecute(Sender: TObject);
begin
  pgEditors.ActivePage.PageIndex := pgEditors.PageCount - 1;
  updateActionStatus(self);
  FTempStore.storeOpenFileList(FContext.EditorSessions);
end;

procedure TMainToolkitForm.actionPagesMoveLeftExecute(Sender: TObject);
begin
  pgEditors.ActivePage.PageIndex := pgEditors.ActivePage.PageIndex - 1;
  updateActionStatus(self);
  FTempStore.storeOpenFileList(FContext.EditorSessions);
end;

procedure TMainToolkitForm.actionPagesMoveRightExecute(Sender: TObject);
begin
  pgEditors.ActivePage.PageIndex := pgEditors.ActivePage.PageIndex + 1;
  updateActionStatus(self);
  FTempStore.storeOpenFileList(FContext.EditorSessions);
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
      copyFonts;
      saveLayout;
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
  if FContext.Focus <> nil then
  begin
    FContext.Focus.getFocus(mnuContent);
    FContext.Focus.editPause;
  end;
  checkActiveTabCurrency;
  updateActionStatus(self);
  updateStatusBar;
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

procedure TMainToolkitForm.pmPagesPopup(Sender: TObject);
begin

end;

end.

