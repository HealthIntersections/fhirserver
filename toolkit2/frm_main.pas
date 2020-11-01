unit frm_main;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, ActnList, StdActns, IniFiles, Clipbrd, Buttons, StdCtrls, SynEdit,

  FHIR.Support.Base, FHIR.Support.Stream, FHIR.Support.Threads, FHIR.Toolkit.Context, FHIR.Toolkit.TempStorage,
  FHIR.Toolkit.FileStore, FHIR.Toolkit.Factory,

  frm_npm_manager, frm_file_format;

const
  PAUSE_SECONDS = 2; // move to settings?

type

  { TMainToolkitForm }
  TMainToolkitForm = class(TForm)
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
    ActionList1: TActionList;
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
    chkTaskInactive: TCheckBox;
    chkCurrrentFileOnly: TCheckBox;
    imgMain: TImageList;
    imgContext: TImageList;
    lvTasks: TListView;
    lvTasks1: TListView;
    MainMenu1: TMainMenu;
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
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
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
    dlgOpen: TOpenDialog;
    Panel4: TPanel;
    pgEditors: TPageControl;
    Panel1: TPanel;
    Panel3: TPanel;
    pgLeft: TPageControl;
    pgBottom: TPageControl;
    pgRight: TPageControl;
    pnlBottom: TPanel;
    Panel2: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pmNew: TPopupMenu;
    pmPages: TPopupMenu;
    dlgSave: TSaveDialog;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    pnlStatus: TStatusBar;
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
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure actionEditRedoExecute(Sender: TObject);
    procedure actionFileManageRenameExecute(Sender: TObject);
    procedure actionFileNewExecute(Sender: TObject);
    procedure actionFileOpenExecute(Sender: TObject);
    procedure actionFileSaveAllExecute(Sender: TObject);
    procedure actionFileSaveAs1Execute(Sender: TObject);
    procedure actionFileSaveExecute(Sender: TObject);
    procedure actionHelpCheckUpgradeExecute(Sender: TObject);
    procedure actionHelpContentExecute(Sender: TObject);
    procedure actionToolsOptionsExecute(Sender: TObject);
    procedure actionToolsPackageManagerExecute(Sender: TObject);
    procedure actionViewEditorExecute(Sender: TObject);
    procedure actionViewExpressionEditorExecute(Sender: TObject);
    procedure actionViewInspectorExecute(Sender: TObject);
    procedure actionViewLogExecute(Sender: TObject);
    procedure actionViewMessagesExecute(Sender: TObject);
    procedure actionViewPackagesExecute(Sender: TObject);
    procedure actionViewProjectManagerExecute(Sender: TObject);
    procedure actionViewSearchExecute(Sender: TObject);
    procedure actionViewServersExecute(Sender: TObject);
    procedure actionViewStackExecute(Sender: TObject);
    procedure actionViewTasksExecute(Sender: TObject);
    procedure actionViewVariablesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure mnuRecentClick(Sender: TObject);
    procedure NewFromFormatClick(Sender: TObject);
    procedure MenuItem79Click(Sender: TObject);
    procedure pgEditorsChange(Sender: TObject);
    procedure pgEditorsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure Splitter3Moved(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    FIni : TIniFile;
    FTempStore : TFHIRToolkitTemporaryStorage;
    FSourceMaximised : boolean;
    FContext : TToolkitContext;
    FFactory : TToolkitFactory;
    procedure SaveFile(editor: TToolkitEditor; address : String; updateStatus : boolean);
    procedure saveLayout;
    procedure loadLayout;
    procedure maximiseSource;
    procedure showView(pnl: TPanel; pg: TPageControl; tab: TTabSheet);
    procedure unmaximiseSource;
    procedure updateActionStatus(Sender : TObject);
    procedure updateUI;
    procedure updateTasks;
    procedure updateActions;
    procedure checkLastUpdated;
    procedure updateStatusBar;
    procedure createNewFile(kind : TSourceEditorKind);
    procedure openFile(filename : String);
    function ChooseFileName(editor : TToolkitEditor; out address : String): boolean;
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
  FIni := TIniFile.create(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'fhir-toolkit.ini');
  FTempStore := TFHIRToolkitTemporaryStorage.create;
  loadLayout;
  FContext := TToolkitContext.create;
  FContext.OnUpdateActions := updateActionStatus;
  FContext.storages.add(TFileStorageService.create);
  FFactory := TToolkitFactory.create(FContext.link, self);
  updateActionStatus(nil);
end;

procedure TMainToolkitForm.FormDestroy(Sender: TObject);
begin
  FFactory.free;
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
  //    var
  //address : String;
  //bytes : TBytes;
  //info : TStringList;
  //clss : TToolkitEditorClass;
  //;
  //store : TStorageService;
begin
  sessions := TFslList<TToolkitEditSession>.create;
  try
    FTempStore.fetchOpenList(sessions);
    for session in sessions do
    begin
      editor := FFactory.makeEditor(session.link);
      FContext.addEditor(editor);
      tab := pgEditors.AddTabSheet;
      editor.bindToTab(tab);
      editor.LoadBytes(FTempStore.fetchContent(session.Guid));
      editor.session.NeedsSaving := false;
      editor.lastChangeChecked := true;
  end;
  finally
    sessions.free;
  end;

  if pgEditors.PageCount > 0 then
  begin
    tab := pgEditors.Pages[0];
    pgEditors.ActivePage := tab;
    FContext.Focus := FContext.EditorForTab(tab);
    updateActionStatus(editor);
  end;
end;

procedure TMainToolkitForm.loadLayout;
begin
  if FIni.readBool('window', 'maximised', false) then
    WindowState := wsMaximized;
  Top := FIni.readInteger('window', 'top', Top);
  Left := FIni.readInteger('window', 'left', Left);
  Height := FIni.readInteger('window', 'height', Height);
  Width := FIni.readInteger('window', 'width', Width);
  pnlLeft.Width := FIni.readInteger('window', 'panel.left.width', pnlLeft.Width);
  pnlRight.Width := FIni.readInteger('window', 'panel.right.width', pnlRight.Width);
  pnlBottom.Width := FIni.ReadInteger('window', 'panel.bottom.width', pnlBottom.Width);
  if FIni.readBool('window', 'source.maximised', false) then
    maximiseSource;
end;

procedure TMainToolkitForm.saveLayout;
begin
  FIni.WriteBool('window', 'maximised', WindowState = wsMaximized);
  if WindowState = wsMaximized then
  begin
    FIni.WriteInteger('window', 'top', Top);
    FIni.WriteInteger('window', 'left', Left);
    FIni.WriteInteger('window', 'height', Height);
    FIni.WriteInteger('window', 'width', Width);
  end;
  FIni.WriteInteger('window', 'panel.left.width', pnlLeft.Width);
  FIni.WriteInteger('window', 'panel.right.width', pnlRight.Width);
  FIni.WriteInteger('window', 'panel.bottom.width', pnlBottom.Width);
  FIni.WriteBool('window', 'maximised', WindowState = wsMaximized);
  FIni.WriteBool('window', 'source.maximised', FSourceMaximised);
end;

procedure TMainToolkitForm.Splitter3Moved(Sender: TObject);
begin
  saveLayout;
end;

procedure TMainToolkitForm.Timer1Timer(Sender: TObject);
begin
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
end;

procedure TMainToolkitForm.updateActions;
begin
end;

procedure TMainToolkitForm.checkLastUpdated;
var
  editor : TToolkitEditor;
begin
  for editor in FContext.Editors do
  begin
    if not editor.lastChangeChecked and (editor.lastChange + (PAUSE_SECONDS*1000) < GetTickCount64) then
    begin
      FTempStore.storeContent(editor.session.guid, false, editor.GetBytes);
      editor.editPause;
    end;
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

procedure TMainToolkitForm.createNewFile(kind : TSourceEditorKind);
var
  session : TToolkitEditSession;
  editor : TToolkitEditor;
  tab : TTabSheet;
begin
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
  updateActionStatus(editor);
end;

procedure TMainToolkitForm.openFile(filename: String);
var
  address : String;
  bytes : TBytes;
  info : TStringList;
  editor : TToolkitEditor;
  tab : TTabSheet;
  store : TStorageService;
  session : TToolkitEditSession;
begin
  address := 'file:'+filename;
  editor := Context.EditorForAddress(address);
  if (editor <> nil) then
    pgEditors.ActivePage := editor.tab
  else
  begin
    bytes := FileToBytes(filename);
    session := FFactory.examineFile(filename, bytes);
    if session <> nil then
    begin
      session.address := address;
      store := Context.StorageForAddress(address);
      session.caption := store.CaptionForAddress(session.address);
      editor := FFactory.makeEditor(session);
      FContext.addEditor(editor);
      tab := pgEditors.AddTabSheet;
      editor.bindToTab(tab);
      editor.LoadBytes(bytes);
      editor.session.NeedsSaving := false;
      pgEditors.ActivePage := tab;
      FTempStore.storeOpenFileList(FContext.EditorSessions);
      FTempStore.storeContent(editor.session.Guid, true, editor.getBytes);
      editor.lastChangeChecked := true;
      FContext.Focus := editor;
      updateActionStatus(editor);
    end;
  end;
end;

function TMainToolkitForm.ChooseFileName(editor: TToolkitEditor; out address : String): boolean;
var
  fn : String;
begin
  // for now, only a local file:
  if editor.hasAddress and editor.session.address.startsWith('file:') then
  begin
    fn := editor.session.address.Substring(5);
    dlgSave.InitialDir := ExtractFilePath(fn);
    dlgSave.FileName := ChangeFileExt(ExtractFileName(fn), '.'+editor.FileExtension);
  end
  else
  begin
    dlgSave.InitialDir := FIni.ReadString('folders', 'last-save', '');
    dlgSave.FileName := 'filename.'+editor.FileExtension;
  end;
  result := dlgSave.Execute;
  if result then
  begin
    address := 'file:'+dlgSave.filename;
    FIni.WriteString('folders', 'last-save', ExtractFilePath(dlgSave.fileName));
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
  actionCopyFileTitle.enabled := context.hasFocus and context.Focus.hasAddress;
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
  actionEditBeginEnd.enabled := context.hasFocus and context.Focus.inSource;
  actionZoomIn.enabled := context.hasFocus and context.Focus.inSource;
  actionZoomOut.enabled := context.hasFocus and context.Focus.inSource;
  actionFilePrint.enabled := context.hasFocus and context.Focus.inSource;
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

procedure TMainToolkitForm.actionViewSearchExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbSearch);
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

procedure TMainToolkitForm.actionFileManageRenameExecute(Sender: TObject);
begin

end;

procedure TMainToolkitForm.actionEditRedoExecute(Sender: TObject);
begin
  Context.focus.redo;
end;

procedure TMainToolkitForm.actionFileNewExecute(Sender: TObject);
begin
  createNewFile(sekNull);
end;

procedure TMainToolkitForm.actionFileOpenExecute(Sender: TObject);
begin
  if (dlgOpen.execute) then
    OpenFile(dlgOpen.filename);
end;

procedure TMainToolkitForm.actionFileSaveAllExecute(Sender: TObject);
var
  editor : TToolkitEditor;
begin
  for editor in Context.editors do
    if editor.session.NeedsSaving then
       actionFileSaveExecute(sender);
end;

procedure TMainToolkitForm.actionFileSaveAs1Execute(Sender: TObject);
var
  address : String;
begin
  if (Context.Focus <> nil) then
    if ChooseFileName(Context.Focus, address) then
    begin
      Context.Focus.Session.Address := address;
      SaveFile(Context.Focus, Context.Focus.Session.Address, true);
      Context.Focus.Session.Caption := Context.StorageForAddress(address).CaptionForAddress(address);
      Context.Focus.Tab.Caption := Context.Focus.Session.Caption;
      Context.Focus.Tab.Hint := address;
      FTempStore.removeFromMRU(address);
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

procedure TMainToolkitForm.SaveFile(editor : TToolkitEditor; address : String; updateStatus : boolean);
var
  store : TStorageService;
begin
  store := Context.StorageForAddress(address);
  if (store = nil) then
  begin
    pgEditors.ActivePage := editor.tab;
    raise Exception.Create('Unable to save to '+address);
  end;
  store.Save(address, editor.getBytes);
  if updateStatus then
    editor.Session.NeedsSaving := false;
end;

procedure TMainToolkitForm.actionHelpCheckUpgradeExecute(Sender: TObject);
begin

end;

procedure TMainToolkitForm.actionHelpContentExecute(Sender: TObject);
begin

end;

procedure TMainToolkitForm.actionToolsOptionsExecute(Sender: TObject);
begin

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

procedure TMainToolkitForm.MenuItem34Click(Sender: TObject);
begin

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

procedure TMainToolkitForm.NewFromFormatClick(Sender: TObject);
begin
  createNewFile(TSourceEditorKind((Sender as TMenuItem).tag));
end;

procedure TMainToolkitForm.MenuItem79Click(Sender: TObject);
begin

end;

procedure TMainToolkitForm.pgEditorsChange(Sender: TObject);
begin
  FContext.Focus := FContext.EditorForTab(pgEditors.ActivePage);
end;

procedure TMainToolkitForm.pgEditorsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  tabindex: integer;
begin
  tabindex := pgEditors.IndexOfTabAt(X, Y);
  if (tabindex >= 0)  then
  begin
    if (pgEditors.Hint <> pgEditors.Pages[tabindex].Hint) then
    begin
      Application.CancelHint;
      pgEditors.Hint := pgEditors.Pages[tabindex].Hint;
      pgEditors.ShowHint := true;
    end;
  end
  else
  begin
    pgEditors.Hint := '';
    pgEditors.ShowHint := false;
  end;
end;

end.

