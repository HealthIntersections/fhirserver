unit FHIR.Transformer.IDE;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, IniFiles, ClipBrd,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.ImageList,
  Vcl.ImgList, VirtualTrees, Vcl.Buttons, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ToolWin,
  ScintEdit, ScintInt, ScintFormats,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Comparisons, FHIR.Support.MXml,
  FHIR.Ui.ListSelector, FHIR.Cda.Scint, FHIR.V2.Scint,
  FHIR.Cache.PackageManagerDialog,
  FHIR.Base.Objects,
  FHIR.R4.Context, FHIR.R4.Resources, FHIR.R4.MapUtilities, FHIR.R4.Scint, FHIR.R4.ElementModel,
  FHIR.Transformer.Workspace, FHIR.Transformer.Utilities, FHIR.Transformer.Engine;

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
  spStatus = 2;

  { Memo marker numbers }
  mmIconHasEntry = 0;        { grey dot }
  mmIconEntryProcessed = 1;  { green dot }
  mmIconBreakpoint = 2;      { stop sign }
  mmIconBreakpointGood = 3;  { stop sign + check }
  mmIconBreakpointBad = 4;   { stop sign + X }
  mmLineError = 10;          { red line highlight }
  mmLineBreakpoint = 11;     { red line highlight }
  mmLineBreakpointBad = 12;  { ugly olive line highlight }
  mmLineStep = 13;           { blue line highlight }
  inSquiggly = 0;
  inPendingSquiggly = 1;

type
  TTreeDataPointer = record
    obj : TFslObject;
  end;
  PTreeDataPointer = ^TTreeDataPointer;

  TEditorInformation = class (TFslObject)
  private
    FErrorLine: Integer;
    FIsDirty: boolean;
    FInfo: TWorkspaceFile;
    FMemo: TScintEdit;
    FTab: TTabSheet;
    procedure SetInfo(const Value: TWorkspaceFile);

  public
    destructor destroy; override;
    property id : TWorkspaceFile read FInfo write SetInfo;
    property tab : TTabSheet read FTab write FTab;
    property memo : TScintEdit read FMemo write FMemo;
    property isDirty : boolean read FIsDirty write FIsDirty;
    property ErrorLine : Integer read FErrorLine write FErrorLine;
  end;

  TIDEScintEdit = class(TScintEdit)
  protected
    procedure CreateWnd; override;
  end;

  TTransformerForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
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
    Go1: TMenuItem;
    btnExecute: TBitBtn;
    mnuPackageManager: TMenuItem;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
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
  private
    FIni : TIniFile;
    FWorkspace : TWorkspace;
    FSelected : PVirtualNode;
    FLastFindOptions: TFindOptions;
    FLastFindText: String;
    FLastReplaceText: String;
    FLoading : boolean;
    FCache : TResourceMemoryCache;
    FEditor : TEditorInformation;

    function DoSave(command : String) : boolean;
    function nodeCaption(i : integer) : String;
    procedure LoadWorkspace(proj: TWorkspace);
    procedure makeNewFile(title, ext, template : String; fmt : TTransformerFormat; category : TFslList<TWorkspaceFile>);

    function findWorkspaceFile(f : TWorkspaceFile) : TEditorInformation;
    procedure openWorkspaceFile(f : TWorkspaceFile);
    procedure saveWorkspaceFile(editor : TEditorInformation);
    procedure closeWorkspaceFile(editor : TEditorInformation; checkSave : boolean);
    procedure renameWorkspaceFile(editor : TEditorInformation);
    function editorForTab(tab : TTabSheet) : TEditorInformation;
    function editorForFile(f : TWorkspaceFile) : TEditorInformation;
    function anyFilesDirty : boolean;

    procedure FindNext(editor : TScintEdit);
    procedure InitializeFindText(Dlg: TFindDialog; editor : TScintEdit);

    procedure mnuPastWorkspaceClick(Sender: TObject);
    procedure memoChange(Sender: TObject; const Info: TScintEditChangeInfo);
    procedure memoExit(Sender: TObject);
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomReset;

    procedure status(color : TColor; msg : String);
    function engineGetSource(sender : TConversionEngine; f : TWorkspaceFile) : TStream;
    procedure engineStatus(sender : TConversionEngine; message : String);
    procedure engineLog(sender : TConversionEngine; message : String);
    function rewriteV2(src, title: String): String;
    function rewriteCDA(src, title: String): String;

    procedure checkV2(src, title : String);
    procedure checkCDA(src, title : String);
    procedure checkResource(src, title : String);
    procedure checkJS(src, title : String);
    procedure checkMap(src, title : String);
    procedure checkTemplate(src, title : String);

    procedure SetErrorLine(ALine: Integer);
    procedure HideError();
    procedure UpdateLineMarkers(const Line: Integer);
    procedure MemoLinesDeleted(FirstLine, Count, FirstAffectedLine: Integer);
    procedure MemoLinesInserted(FirstLine, Count: integer);
    procedure UpdateAllLineMarkers();
  public
    { Public declarations }
  end;

var
  TransformerForm: TTransformerForm;

implementation

{$R *.dfm}


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
var
  engine : TCDAConversionEngine;
begin
  engine := TCDAConversionEngine.create;
  try
    engine.source := (cbxSource.Items.Objects[cbxSource.ItemIndex] as TWorkspaceFile).link;
    engine.cache := FCache.Link;
    engine.workspace := FWorkspace.link;
    engine.OnWantSource := engineGetSource;
    engine.OnStatus := engineStatus;
    engine.OnLog := engineLog;
//      engine.OnDebug := EngineDebug;
    engine.load;
    engine.execute;
  finally
    engine.free;
  end;
end;

procedure TTransformerForm.cbxEventTypeChange(Sender: TObject);
  procedure loadSource(srcList, scrList : TFslList<TWorkspaceFile>; caption : String);
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
    btnExecute.enabled := (cbxScript.ItemIndex > -1) and (cbxSource.ItemIndex > -1);
  end;
begin
   case cbxEventType.ItemIndex of
    0: LoadSource(FWorkspace.messages, FWorkspace.scripts, 'Scripts');
    1: LoadSource(FWorkspace.documents, FWorkspace.maps, 'Maps');
  end;
  if not Floading then
  begin
    FWorkspace.EventType := cbxEventType.ItemIndex;
    FWorkspace.Source := cbxSource.Text;
    FWorkspace.Script := cbxScript.Text;
    FWorkspace.Save;
  end;
end;

procedure TTransformerForm.cbxScriptChange(Sender: TObject);
begin
  if not Floading then
  begin
    FWorkspace.Script := cbxScript.Text;
    FWorkspace.Save;
  end;
  btnExecute.enabled := (cbxScript.ItemIndex > -1) and (cbxSource.ItemIndex > -1);
end;

procedure TTransformerForm.cbxSourceChange(Sender: TObject);
begin
  if not Floading then
  begin
    FWorkspace.Source := cbxSource.Text;
    FWorkspace.Save;
  end;
  btnExecute.enabled := (cbxScript.ItemIndex > -1) and (cbxSource.ItemIndex > -1);
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
  end
  else
  begin
    FEditor := editorForTab(pgTabs.ActivePage); // (pgTabs.ActivePage.Controls[0] as TScintEdit).context as TEditorInformation;
    Caption := pgTabs.ActivePage.Caption+'- FHIR Transformer';
    mnuSearch.Enabled := true;
    mnuFindNext.Enabled := true;
    mnuReplace.Enabled := true;
    mnuGoto.Enabled := true;
    mnuCompare.Enabled := FEditor.id.format in [fmtV2, fmtCDA, fmtResource, fmtMap];
    tbCompare.Enabled := mnuCompare.Enabled;
    mnuPretty.Enabled := FEditor.id.format in [fmtCDA, fmtResource];
    mnuDense.Enabled := FEditor.id.format in [fmtCDA, fmtResource];
    mnuEOL.Enabled := true;
    mnuWindowsEOL.Checked := false;
    mnuUnixEOL.Checked := false;
    mnuCompile.Enabled := true;
    tbCompile.Enabled := true;
    case FEditor.memo.LineEndings of
      sleCRLF: mnuWindowsEOL.Checked := true;
      sleLF: mnuUnixEOL.Checked := true;
    end;
    mnuRemoveEmptyLines.Enabled := true;
    memoExit(nil);
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

function TTransformerForm.DoSave(command : String) : boolean;
var
  dirty : boolean;
  form : TListSelectorForm;
  i : integer;
  e : TEditorInformation;
begin
  dirty := false;
  result := false;

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
  finally
    form.Free;
  end;
end;

function TTransformerForm.editorForFile(f: TWorkspaceFile): TEditorInformation;
var
  i : integer;
  e : TEditorInformation;
begin
  for i := 1 to pgTabs.PageCount do
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
  FWorkspace.name := edtWorkspace.Text;
  FWorkspace.save;
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
      mnuRename.Enabled := true;
      mnuDrop.Enabled := true;
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

procedure TTransformerForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  dirty : boolean;
  form : TListSelectorForm;
  i : integer;
  e : TEditorInformation;
begin
  dirty := false;
  FWorkspace.save;
  FWorkspace.ClearOpenFiles;
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
  finally
    form.Free;
  end;
end;

procedure TTransformerForm.FormCreate(Sender: TObject);
var
  s : String;
  i : integer;
begin
  FCache := TResourceMemoryCache.create;
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

procedure TTransformerForm.memoExit(Sender: TObject);
begin
  FEditor.id.Row := FEditor.memo.CaretLine;
  if (FEditor.ErrorLine < 0) then
    HideError;
  pnlStatus.Panels[spCaretPos].Text := Format('%4d:%4d', [FEditor.memo.CaretLine + 1, FEditor.memo.CaretColumnExpanded + 1]);

  // undo/redo
  mnuUndo.Enabled := FEditor.memo.CanUndo;
  tbUndo.Enabled := FEditor.memo.CanUndo;
  mnuRedo.Enabled := FEditor.memo.CanUndo;
  // clipbrd
  mnuCut.Enabled := FEditor.memo.SelText <> '';
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
      renameWorkspaceFile(editorForFile(f));
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
  for i := 1 to pgTabs.PageCount do
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
      pmRename.Enabled := true;
      pmDrop.Enabled := true;
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

function isXml(s : String) : boolean;
begin
  if not s.Contains('<') then
    result := false
  else if not s.Contains('{') then
    result := true
  else
    result := s.IndexOf('<') < s.IndexOf('{');
end;

procedure TTransformerForm.openWorkspaceFile(f : TWorkspaceFile);
var
  tab : TTabSheet;
  editor : TScintEdit;
  s : String;
  info : TEditorInformation;
begin
  info := findWorkspaceFile(f);
  if info <> nil then
    pgTabs.ActivePage := info.tab
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
    editor.OnUpdateUI := memoExit;
    editor.OnKeyDown := FormKeyDown;
    editor.LineNumbers := true;
    editor.PopupMenu := pmEditor;
    editor.Align := alClient;
    case f.format of
      fmtV2 :  editor.Styler := TV2Styler.Create(self);
      fmtCDA  : editor.Styler := TCDAStyler.Create(self);
      fmtMap : editor.Styler := TFHIRMapStyler.Create(self);
      fmtResource:
        if isXml(editor.rawText) then
          editor.Styler := TXmlStyler.Create(self)
        else
          editor.Styler := TJsonStyler.Create(self);
      fmtTemplate : editor.Styler := TLiquidStyler.Create(self);
    end;

    info := TEditorInformation.create;
    editor.context := info;
    info.memo := editor;
    info.id := f.link;
    info.tab := tab;
    FEditor := info;

    s := makeAbsolutePath(f.filename, FWorkspace.folder);
    editor.Lines.LoadFromFile(s);
    editor.ClearUndo;
    editor.CaretLine := f.row;
    info.isDirty := false;
    info.ErrorLine := -1;
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
  editor.isDirty := false;
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

procedure TTransformerForm.Timer1Timer(Sender: TObject);
begin
  if pgTabs.ActivePageIndex = 0 then
  begin
    mnuPaste.Enabled := false;
    tbPaste.Enabled := false;
    pmnuPaste.Enabled := false;
  end
  else
  begin
    mnuPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
    tbPaste.Enabled := mnuPaste.Enabled;
    pmnuPaste.Enabled := mnuPaste.Enabled;
  end;
end;

procedure TTransformerForm.closeWorkspaceFile(editor : TEditorInformation; checkSave : boolean);
var
  tab : TTabSheet;
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
  if pgTabs.ActivePage = tab then
    pgTabs.TabIndex := pgTabs.TabIndex - 1;
  tab.Free;
  editor.Free;
end;

procedure TTransformerForm.renameWorkspaceFile(editor : TEditorInformation);
begin
  if editor = nil then
    exit;
  editor.tab.Caption := editor.id.title;
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
   utils := TFHIRStructureMapUtilities.Create(nil, nil, nil);
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
      fmtMap: checkMap(FEditor.memo.RawText, FEditor.id.title);
      fmtTemplate: checkTemplate(FEditor.memo.RawText, FEditor.id.title);
    end;
    status(clGreen, 'All Ok')
  except
    on e : EParserException do
    begin
      SetErrorLine(e.Line-1);
      status(clMaroon, 'Error Compiling: '+e.message);
    end;
    on e : Exception do
    begin
      status(clMaroon, 'Error Compiling: '+e.message)
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

procedure TTransformerForm.checkCDA(src, title : String);
begin
  rewriteCDA(src, title); // just ignore output
end;

procedure TTransformerForm.checkResource(src, title : String);
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TTransformerForm.checkJS(src, title : String);
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TTransformerForm.checkMap(src, title : String);
var
  utils : TFHIRStructureMapUtilities;
begin
  utils := TFHIRStructureMapUtilities.Create(nil, nil, nil);
  try
    utils.parse(src, title).free;
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
      UpdateLineMarkers(OldLine);
    if FEditor.ErrorLine >= 0 then
    begin
      FEditor.memo.CaretLine := FEditor.ErrorLine;
      FEditor.memo.ScrollCaretIntoView;
      UpdateLineMarkers(FEditor.ErrorLine);
    end;
  end;
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
  UpdateLineMarkers(FirstAffectedLine);
end;

procedure TTransformerForm.UpdateLineMarkers(const Line: Integer);
var
  NewMarker: Integer;
begin
  if Line >= FEditor.Memo.Lines.Count then
    Exit;

  { Delete all markers on the line. To flush out any possible duplicates,
    even the markers we'll be adding next are deleted. }
  if FEditor.Memo.GetMarkers(Line) <> [] then
    FEditor.Memo.DeleteAllMarkersOnLine(Line);

//  if FStepLine = Line then
//    Memo.AddMarker(Line, mmLineStep)
//  else
  if FEditor.ErrorLine = Line then
    FEditor.Memo.AddMarker(Line, mmLineError)
//  else if NewMarker in [mmIconBreakpoint, mmIconBreakpointGood] then
//    Memo.AddMarker(Line, mmLineBreakpoint)
//  else if NewMarker = mmIconBreakpointBad then
//    Memo.AddMarker(Line, mmLineBreakpointBad);
end;

procedure TTransformerForm.UpdateAllLineMarkers;
var
  Line: Integer;
begin
  for Line := 0 to FEditor.Memo.Lines.Count-1 do
    UpdateLineMarkers(Line);
end;

{ TEditorInformation }

destructor TEditorInformation.destroy;
begin
  FInfo.free;
  inherited;
end;

procedure TEditorInformation.SetInfo(const Value: TWorkspaceFile);
begin
  FInfo.free;
  FInfo := Value;
end;

{ TIDEScintEdit }

procedure TIDEScintEdit.CreateWnd;
const
  PixmapHasEntry: array[0..8] of PAnsiChar = (
    '5 5 2 1',
    'o c #808080',
    '. c #c0c0c0',
    'ooooo',
    'o...o',
    'o...o',
    'o...o',
    'ooooo',
    nil);
  PixmapEntryProcessed: array[0..8] of PAnsiChar = (
    '5 5 2 1',
    'o c #008000',
    '. c #00ff00',
    'ooooo',
    'o...o',
    'o...o',
    'o...o',
    'ooooo',
    nil);
  PixmapBreakpoint: array[0..14] of PAnsiChar = (
    '9 10 3 1',
    '= c none',
    'o c #000000',
    '. c #ff0000',
    '=========',
    '==ooooo==',
    '=o.....o=',
    'o.......o',
    'o.......o',
    'o.......o',
    'o.......o',
    'o.......o',
    '=o.....o=',
    '==ooooo==',
    nil);
  PixmapBreakpointGood: array[0..15] of PAnsiChar = (
    '9 10 4 1',
    '= c none',
    'o c #000000',
    '. c #ff0000',
    '* c #00ff00',
    '======oo=',
    '==oooo**o',
    '=o....*o=',
    'o....**.o',
    'o....*..o',
    'o...**..o',
    'o**.*...o',
    'o.***...o',
    '=o.*...o=',
    '==ooooo==',
    nil);
  PixmapBreakpointBad: array[0..15] of PAnsiChar = (
    '9 10 4 1',
    '= c none',
    'o c #000000',
    '. c #ff0000',
    '* c #ffff00',
    '=========',
    '==ooooo==',
    '=o.....o=',
    'o.*...*.o',
    'o.**.**.o',
    'o..***..o',
    'o.**.**.o',
    'o.*...*.o',
    '=o.....o=',
    '==ooooo==',
    nil);
const
  SC_MARK_BACKFORE = 3030;  { new marker type added in my Scintilla build }
begin
  inherited;

  Call(SCI_SETCARETWIDTH, 2, 0);
  Call(SCI_AUTOCSETAUTOHIDE, 0, 0);
  Call(SCI_AUTOCSETCANCELATSTART, 0, 0);
  Call(SCI_AUTOCSETDROPRESTOFWORD, 1, 0);
  Call(SCI_AUTOCSETIGNORECASE, 1, 0);
  Call(SCI_AUTOCSETMAXHEIGHT, 7, 0);

  Call(SCI_ASSIGNCMDKEY, Ord('Z') or ((SCMOD_SHIFT or SCMOD_CTRL) shl 16), SCI_REDO);

  Call(SCI_SETSCROLLWIDTH, 1024 * CallStr(SCI_TEXTWIDTH, 0, 'X'), 0);

  Call(SCI_INDICSETSTYLE, inSquiggly, INDIC_SQUIGGLE);
  Call(SCI_INDICSETFORE, inSquiggly, clRed);
  Call(SCI_INDICSETSTYLE, inPendingSquiggly, INDIC_HIDDEN);

  Call(SCI_SETMARGINTYPEN, 1, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINWIDTHN, 1, 21);
  Call(SCI_SETMARGINSENSITIVEN, 1, 1);
  Call(SCI_SETMARGINCURSORN, 1, SC_CURSORARROW);
  Call(SCI_SETMARGINTYPEN, 2, SC_MARGIN_BACK);
  Call(SCI_SETMARGINMASKN, 2, 0);
  Call(SCI_SETMARGINWIDTHN, 2, 1);
  Call(SCI_SETMARGINTYPEN, 3, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINMASKN, 3, 0);
  Call(SCI_SETMARGINWIDTHN, 3, 1);
  Call(SCI_SETMARGINLEFT, 0, 2);

  Call(SCI_MARKERDEFINEPIXMAP, mmIconHasEntry, LPARAM(@PixmapHasEntry));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconEntryProcessed, LPARAM(@PixmapEntryProcessed));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpoint, LPARAM(@PixmapBreakpoint));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpointGood, LPARAM(@PixmapBreakpointGood));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpointBad, LPARAM(@PixmapBreakpointBad));
  Call(SCI_MARKERDEFINE, mmLineError, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineError, clWhite);
  Call(SCI_MARKERSETBACK, mmLineError, clRed);
  Call(SCI_MARKERDEFINE, mmLineBreakpoint, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineBreakpoint, clWhite);
  Call(SCI_MARKERSETBACK, mmLineBreakpoint, clRed);
  Call(SCI_MARKERDEFINE, mmLineBreakpointBad, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineBreakpointBad, clLime);
  Call(SCI_MARKERSETBACK, mmLineBreakpointBad, clOlive);
  Call(SCI_MARKERDEFINE, mmLineStep, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineStep, clWhite);
  Call(SCI_MARKERSETBACK, mmLineStep, clBlue);
end;

end.


