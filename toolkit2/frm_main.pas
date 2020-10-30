unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, ActnList, StdActns, IniFiles, Clipbrd,
  FHIR.Toolkit.Context, frm_fhir_manager;

type

  { TForm1 }

  TForm1 = class(TForm)
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
    actionFileOpen: TFileOpen;
    actionFileSaveAs1: TFileSaveAs;
    actionHelpContent: THelpContents;
    ImageList1: TImageList;
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
    PageControl1: TPageControl;
    pgLeft: TPageControl;
    pgBottom: TPageControl;
    pgRight: TPageControl;
    pnlBottom: TPanel;
    Panel2: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
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
    procedure actionFileManageRenameExecute(Sender: TObject);
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
    procedure MenuItem34Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure Splitter3Moved(Sender: TObject);
  private
    FIni : TIniFile;
    FSourceMaximised : boolean;
    FContext : TToolkitContext;
    procedure saveLayout;
    procedure loadLayout;
    procedure maximiseSource;
    procedure showView(pnl: TPanel; pg: TPageControl; tab: TTabSheet);
    procedure unmaximiseSource;
    procedure updateActionStatus;
  public
    property Context : TToolkitContext read FContext;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.create(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'fhir-toolkit.ini');
  loadLayout;
  FContext := TToolkitContext.create;
  updateActionStatus;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FContext.Free;
  saveLayout;
  FIni.Free;
end;

procedure TForm1.loadLayout;
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

procedure TForm1.saveLayout;
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

procedure TForm1.Splitter3Moved(Sender: TObject);
begin
  saveLayout;
end;

procedure TForm1.Splitter1Moved(Sender: TObject);
begin
  saveLayout;
end;

procedure TForm1.Splitter2Moved(Sender: TObject);
begin
  saveLayout;
end;

procedure TForm1.maximiseSource;
begin
  pnlLeft.visible := false;
  pnlRight.visible := false;
  pnlBottom.visible := false;
  Splitter1.enabled := false;
  Splitter2.enabled := false;
  Splitter3.enabled := false;
  FSourceMaximised := true;
end;

procedure TForm1.unmaximiseSource;
begin
  pnlLeft.visible := true;
  pnlRight.visible := true;
  pnlBottom.visible := true;
  Splitter1.enabled := true;
  Splitter2.enabled := true;
  Splitter3.enabled := true;
  FSourceMaximised := false;
end;

procedure TForm1.updateActionStatus;
begin
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

  // enabled if there's an open file:
  actionCopyFileTitle.enabled := context.hasFocus;
  actionEditCopyFilename.enabled := context.hasFocus;
  actionFileManageFolder.enabled := context.hasFocus;
  actionCopyFilePath.enabled := context.hasFocus;
  actionCopyFile.enabled := context.hasFocus;
  actionFileClose.enabled := context.hasFocus;
  actionFileSave.enabled := context.hasFocus;
  actionFileSaveAll.enabled := context.hasFocus;
  actionFileManageRename.enabled := context.hasFocus;
  actionFileManageCopy.enabled := context.hasFocus;
  actionFileManageDelete.enabled := context.hasFocus;
  actionFileManageReload.enabled := context.hasFocus;
  actionFileSaveAs1.enabled := context.hasFocus;

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

procedure TForm1.actionViewEditorExecute(Sender: TObject);
begin
  if FSourceMaximised then
    unmaximiseSource
  else
    maximiseSource;
end;

procedure TForm1.showView(pnl : TPanel; pg : TPageControl; tab : TTabSheet);
begin
  if FSourceMaximised then
    unmaximiseSource;
  pg.ActivePage := tab;
end;

procedure TForm1.actionViewExpressionEditorExecute(Sender: TObject);
begin
  showView(pnlLeft, pgLeft, tbExpression);
end;

procedure TForm1.actionViewInspectorExecute(Sender: TObject);
begin
  showView(pnlRight, pgRight, tbInspector);
end;

procedure TForm1.actionViewLogExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbLog);
end;

procedure TForm1.actionViewMessagesExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbMessages);
end;

procedure TForm1.actionViewPackagesExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbPackages);
end;

procedure TForm1.actionViewProjectManagerExecute(Sender: TObject);
begin
  showView(pnlLeft, pgLeft, tbProjects);
end;

procedure TForm1.actionViewSearchExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbSearch);
end;

procedure TForm1.actionViewServersExecute(Sender: TObject);
begin
  showView(pnlLeft, pgLeft, tbServers);
end;

procedure TForm1.actionViewStackExecute(Sender: TObject);
begin
  showView(pnlRight, pgRight, tbStack);
end;

procedure TForm1.actionViewTasksExecute(Sender: TObject);
begin
  showView(pnlBottom, pgBottom, tbTasks);
end;

procedure TForm1.actionViewVariablesExecute(Sender: TObject);
begin
  showView(pnlRight, pgRight, tbVariables);
end;

procedure TForm1.actionFileManageRenameExecute(Sender: TObject);
begin

end;

procedure TForm1.actionHelpCheckUpgradeExecute(Sender: TObject);
begin

end;

procedure TForm1.actionHelpContentExecute(Sender: TObject);
begin

end;

procedure TForm1.actionToolsOptionsExecute(Sender: TObject);
begin

end;

procedure TForm1.actionToolsPackageManagerExecute(Sender: TObject);
begin
  PackageCacheForm := TPackageCacheForm.create(self);
  try
    PackageCacheForm.showModal;
  finally
    PackageCacheForm.free;
  end;
end;

procedure TForm1.MenuItem34Click(Sender: TObject);
begin

end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin

end;

end.

