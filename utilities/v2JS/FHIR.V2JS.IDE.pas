unit FHIR.V2JS.IDE;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Menus, FMX.Controls.Presentation, FMX.StdCtrls,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Javascript,
  FHIR.v2.Protocol, FHIR.v2.Message, FHIR.v2.Javascript, FMX.TabControl,
  FMX.TreeView, FMX.Layouts, System.ImageList, FMX.ImgList, FMX.Edit, FMX.Memo.Types,
  FHIR.Ui.TabsFMX,
  FHIR.V2JS.Workspace, FHIR.V2JS.Utilities;

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

type
  TEditorForm = class(TForm)
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuNew: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuExit: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    tvWorkspace: TTreeView;
    tviMessages: TTreeViewItem;
    tviResources: TTreeViewItem;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Panel6: TPanel;
    Splitter2: TSplitter;
    Label2: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    tviScripts: TTreeViewItem;
    tviMaps: TTreeViewItem;
    tviTemplates: TTreeViewItem;
    MenuItem1: TMenuItem;
    mnuDuplicateItem: TMenuItem;
    mnuRenameItem: TMenuItem;
    mnuDropItem: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel3: TPanel;
    Button10: TButton;
    Button11: TButton;
    ImageList1: TImageList;
    edtWorkspace: TEdit;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    mnuWorkspace: TMenuItem;
    btnAdd: TButton;
    pmAddAsset: TPopupMenu;
    pmAddExisting: TMenuItem;
    pmNewMessage: TMenuItem;
    pmNewResource: TMenuItem;
    pmNewScript: TMenuItem;
    pmNewMap: TMenuItem;
    pmNewTemplate: TMenuItem;
    mnuNewMessage: TMenuItem;
    mnuNewResource: TMenuItem;
    mnuNewScript: TMenuItem;
    mnuNewMap: TMenuItem;
    mnuNewTemplate: TMenuItem;
    sdNew: TSaveDialog;
    pmWorkspace: TPopupMenu;
    mnuWorkspaceAddItem: TMenuItem;
    mnuWorkspaceDropItem: TMenuItem;
    pmNewResourceXml: TMenuItem;
    mnuNewResourceXML: TMenuItem;
    odImport: TOpenDialog;
    mnuWorkspaceDuplicateItem: TMenuItem;
    mnuWorkspaceRenameItem: TMenuItem;
    tabs: TTabControl;
    TabItem1: TTabItem;
    btnClose: TButton;
    Button3: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure editorChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure edtWorkspaceChangeTracking(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure mnuWorkspaceClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure pmNewMessageClick(Sender: TObject);
    procedure pmWorkspacePopup(Sender: TObject);
    procedure mnuWorkspaceAddItemClick(Sender: TObject);
    procedure pmNewResourceClick(Sender: TObject);
    procedure pmNewScriptClick(Sender: TObject);
    procedure pmNewMapClick(Sender: TObject);
    procedure pmNewTemplateClick(Sender: TObject);
    procedure pmNewResourceXmlClick(Sender: TObject);
    procedure mnuWorkspaceDropItemClick(Sender: TObject);
    procedure pmAddExistingClick(Sender: TObject);
    procedure mnuFileClick(Sender: TObject);
    procedure mnuWorkspaceDuplicateItemClick(Sender: TObject);
    procedure mnuWorkspaceRenameItemClick(Sender: TObject);
    procedure tabsChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
  private
    FIni : TIniFile;
    FFilename: String;
    FDirty : Boolean;
    FJs : TJavascript;
    FProject : TV2JSWorkspace;
    procedure SetFileName(const Value: String);
    procedure JSLog(sender : TJavascript; message : String);
    procedure DoSave;
    procedure LoadProject(proj : TV2JSWorkspace);
    procedure LoadList(list : TFslList<TV2JSWorkspaceFile>);
    procedure makeNewFile(title, ext, template : String; category : TFslList<TV2JSWorkspaceFile>);
    procedure InitProject;

    procedure openWorkspaceItem(Sender: TObject);
    procedure memoChangeTracking(Sender: TObject);
    procedure memoExit(Sender: TObject);

    function findWorkspaceFile(f : TV2JSWorkspaceFile) : TTabItem;
    procedure openWorkspaceFile(f : TV2JSWorkspaceFile);
    procedure saveWorkspaceFile(f : TV2JSWorkspaceFile);
    procedure closeWorkspaceFile(f : TV2JSWorkspaceFile; checkSave : boolean);
    procedure renameWorkspaceFile(f : TV2JSWorkspaceFile);
  public
    Property Filename : String read FFilename write SetFileName;
  end;

var
  EditorForm: TEditorForm;

implementation

{$R *.fmx}

procedure TEditorForm.btnAddClick(Sender: TObject);
var
  pt : TPointF;
begin
  pt.X := 0;
  pt.Y := btnAdd.Height;
  pt := btnAdd.LocalToAbsolute(pt);
  pt := ClientToScreen(pt);
  pmAddAsset.Popup(pt.X, pt.Y);
end;

procedure TEditorForm.btnCloseClick(Sender: TObject);
var
  m : TMemo;
begin
  m := tabs.ActiveTab.TagObject as TMemo;
  if (m <> nil) then
    closeWorkspaceFile(m.TagObject as TV2JSWorkspaceFile, true);
end;

procedure TEditorForm.Button1Click(Sender: TObject);
var
  dt : TDateTime;
begin
  DoSave;
  JSLog(Fjs, 'Starting');
  dt := now;
//  Fjs.execute(editor.Text, '', 'test', []);
  JSLog(Fjs, 'Stopped ('+DescribePeriod(now - dt)+')');
end;

procedure TEditorForm.DoSave;
var
  s : String;
begin
  FIni.writeString('Editor', 'filename', FileName);
//  s := EncodeBase64(TEncoding.UTF8.GetBytes(editor.text));
  s := s.Replace(#13#10, '');
  FIni.writeString('Editor', 'source', s);
end;

procedure TEditorForm.editorChange(Sender: TObject);
begin
  FDirty := true;
end;

procedure TEditorForm.edtWorkspaceChangeTracking(Sender: TObject);
begin
  FProject.name := edtWorkspace.Text;
  FProject.save;
end;

function TEditorForm.findWorkspaceFile(f: TV2JSWorkspaceFile): TTabItem;
var
  i : integer;
  m : TMemo;
begin
  for i := 0 to tabs.TabCount - 1 do
  begin
    m := tabs.Tabs[i].TagObject as TMemo;
    if (m <> nil) and (m.TagObject = f) then
      exit(tabs.Tabs[i]);
  end;
  result := nil;
end;

procedure TEditorForm.InitProject;
var
  s : String;
begin
  if FProject = nil then
  begin
    s := FIni.ReadString('Project', 'filename', '');
    if FileExists(s) then
      LoadProject(TV2JSWorkspace.Create(s))
    else
    begin
       s := Path([UserFolder, 'FHIR', 'v2js']);
       if (not FolderExists(s)) then
         ForceFolder(s);
       s := Path([s, 'v2js-workspace.ini']);
       LoadProject(TV2JSWorkspace.Create(s));
    end;
  end;
end;

procedure TEditorForm.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.create(Path([SystemTemp, 'v2JS.ini']));
  FJs := TJavascript.Create('ChakraCore.dll');
  FJs.OnLog := JSLog;
  TV2JavascriptHelper.registerv2Objects(FJs);
  tviMessages.TagString := 'Message';
  tviMessages.Tag := 1;
  tviResources.TagString := 'Resource';
  tviResources.Tag := 2;
  tviScripts.TagString := 'Script';
  tviScripts.Tag := 3;
  tviMaps.TagString := 'Map';
  tviMaps.Tag := 4;
  tviTemplates.TagString := 'Template';
  tviTemplates.Tag := 5;
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
begin
  DoSave;
  FJs.Free;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
  InitProject;
//  FileName := FIni.readString('Editor', 'filename', '');
//  editor.text := TEncoding.UTF8.GetString(DecodeBase64(FIni.readString('Editor', 'source', '')));
end;

procedure TEditorForm.JSLog(sender: TJavascript; message: String);
var
  i : integer;
begin
//  i := log.Text.Length;
//  log.Text := log.Text+message+#13#10;
//  log.SelStart := i;
end;

procedure TEditorForm.LoadList(list: TFslList<TV2JSWorkspaceFile>);
var
  tvi, child : TTreeViewItem;
  item : TV2JSWorkspaceFile;
  i : integer;
begin
  tvi := list.TagObject as TTreeViewItem;
  for i := tvi.Count - 1 downto 0 do
    tvi.RemoveObject(tvi.Items[i]);
  for item in list do
  begin
    child := TTreeViewItem.Create(self);
    child.TagObject := item;
    child.Text := ExtractFileName(item.filename);
    child.Hint := item.filename;
    child.ShowHint := true;
    child.Parent := tvi;
    child.OnDblClick := openWorkspaceItem;
    tvi.AddObject(child);
  end;
  tvi.ExpandAll;
end;

procedure TEditorForm.LoadProject(proj: TV2JSWorkspace);
begin
  FProject.Free;
  FProject := proj;
  FIni.WriteString('Project', 'filename', FProject.fileName);
  edtWorkspace.Text := proj.name;
  edtWorkspace.Hint := proj.fileName;
  FProject.messages.TagObject := tviMessages;
  tviMessages.TagObject := FProject.messages;
  FProject.resources.TagObject := tviResources;
  tviResources.TagObject := FProject.resources;
  FProject.scripts.TagObject := tviScripts;
  tviScripts.TagObject := FProject.scripts;
  FProject.maps.TagObject := tviMaps;
  tviMaps.TagObject := FProject.maps;
  FProject.templates.TagObject := tviTemplates;
  tviTemplates.TagObject := FProject.templates;
  loadList(FProject.messages);
  loadList(FProject.resources);
  loadList(FProject.scripts);
  loadList(FProject.maps);
  loadList(FProject.templates);
end;

procedure TEditorForm.makeNewFile(title, ext, template: String; category: TFslList<TV2JSWorkspaceFile>);
var
  s : String;
  f : TV2JSWorkspaceFile;
begin
  sdNew.InitialDir := FProject.folder;
  sdNew.DefaultExt := ext;
  sdNew.FileName := '';
  sdNew.Title := 'New '+title;
  if sdNew.execute then
  begin
    if FProject.includesFile(sdNew.FileName) then
      raise EFslException.Create('The file "'+sdNew.FileName+'" is already in the '+UI_NAME);
    s := makeRelativePath(sdNew.FileName, FProject.folder);
    StringToFile(template, sdNew.FileName, TEncoding.UTF8);
    f := TV2JSWorkspaceFile.Create(s);
    category.Add(f);
    FProject.save;
    loadList(category);
    openWorkspaceFile(f);
  end;
end;

procedure TEditorForm.memoChangeTracking(Sender: TObject);
begin
  ((Sender as TMemo).TagObject as TV2JSWorkspaceFile).isDirty := true;
end;

procedure TEditorForm.memoExit(Sender: TObject);
var
  m : TMemo;
begin
  m := (Sender as TMemo);
  (m.TagObject as TV2JSWorkspaceFile).Row := m.TextPosToPos(m.SelStart).Line;
end;

procedure TEditorForm.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TEditorForm.mnuFileClick(Sender: TObject);
var
  tvi : TTreeViewItem;
begin
  tvi := tvWorkspace.Selected;
  if tvi = nil then
  begin
    mnuDuplicateItem.enabled := false;
    mnuDropItem.enabled := false;
    mnuRenameItem.enabled := false;
  end
  else
  begin
    if (tvi.Tag = 0) then
      mnuDropItem.enabled := true
    else
      mnuDropItem.enabled := false;
    mnuDuplicateItem.enabled := mnuDropItem.enabled;
    mnuDropItem.enabled := mnuDropItem.enabled;
  end;
end;

procedure TEditorForm.mnuSaveClick(Sender: TObject);
var
  m : TMemo;
begin
  m := tabs.ActiveTab.TagObject as TMemo;
  if (m <> nil) then
    saveWorkspaceFile(m.TagObject as TV2JSWorkspaceFile);
end;

procedure TEditorForm.mnuWorkspaceAddItemClick(Sender: TObject);
var
  tvi : TTreeViewItem;
begin
  tvi := tvWorkspace.Selected;
  if tvi <> nil then
  begin
    if (tvi.Tag = 0) then
      tvi := tvi.ParentItem;
    case tvi.Tag of
      1: pmNewMessageClick(self);
      2: pmNewResourceClick(self);
      3: pmNewScriptClick(self);
      4: pmNewMapClick(self);
      5: pmNewTemplateClick(self);
    end;
  end;
end;

procedure TEditorForm.mnuWorkspaceClick(Sender: TObject);
var
  dir : String;
begin
  if SelectDirectory('Choose Workspace', '', dir) then
    LoadProject(TV2JSWorkspace.Create(Path([dir, 'v2js-workspace.ini'])));
end;

procedure TEditorForm.mnuWorkspaceDropItemClick(Sender: TObject);
var
  tvi : TTreeViewItem;
  f : TV2JSWorkspaceFile;
  list : TFslList<TV2JSWorkspaceFile>;
begin
  tvi := tvWorkspace.Selected;
  if tvi <> nil then
  begin
    if (tvi.Tag = 0) then
    begin
      f := tvi.TagObject as TV2JSWorkspaceFile;
      tvi := tvi.ParentItem;
      list := tvi.TagObject as TFslList<TV2JSWorkspaceFile>;
      case MessageDlg('Drop '+f.filename+'. Do you want to delete the actual file?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], 0) of
        mrCancel : abort;
        mrYes : DeleteFile(makeAbsolutePath(f.filename, FProject.folder));
        mrNo : saveWorkspaceFile(f);
      end;
      list.Remove(f);
      closeWorkspaceFile(f, false);
      FProject.save;
      LoadList(list);
    end;
  end;
end;

procedure TEditorForm.mnuWorkspaceDuplicateItemClick(Sender: TObject);
var
  tvi : TTreeViewItem;
  f : TV2JSWorkspaceFile;
  list : TFslList<TV2JSWorkspaceFile>;
begin
  tvi := tvWorkspace.Selected;
  if tvi <> nil then
  begin
    if (tvi.Tag = 0) then
    begin
      f := tvi.TagObject as TV2JSWorkspaceFile;
      tvi := tvi.ParentItem;
      list := tvi.TagObject as TFslList<TV2JSWorkspaceFile>;
      makeNewFile(tvi.TagString, ExtractFileExt(f.filename), FileToString(makeAbsolutePath(f.filename, FProject.folder), TEncoding.UTF8), list);
    end;
  end;
end;

procedure TEditorForm.mnuWorkspaceRenameItemClick(Sender: TObject);
var
  s : String;
  f : TV2JSWorkspaceFile;
  tvi : TTreeViewItem;
  list : TFslList<TV2JSWorkspaceFile>;
begin
  tvi := tvWorkspace.Selected;
  if tvi <> nil then
  begin
    if (tvi.Tag = 0) then
    begin
      f := tvi.TagObject as TV2JSWorkspaceFile;
      tvi := tvi.ParentItem;
      list := tvi.TagObject as TFslList<TV2JSWorkspaceFile>;
      sdNew.InitialDir := FProject.folder;
      sdNew.DefaultExt := ExtractFileExt(f.filename);
      sdNew.FileName := '';
      sdNew.Title := 'Rename '+tvi.TagString;
      if sdNew.execute then
      begin
        if FProject.includesFile(sdNew.FileName) then
          raise EFslException.Create('The file "'+sdNew.FileName+'" is already in the '+UI_NAME);
        RenameFile(makeAbsolutePath(f.filename, FProject.folder), sdNew.FileName);
        s := makeRelativePath(sdNew.FileName, FProject.folder);
        f.filename := s;
        FProject.save;
        loadList(list);
        renameWorkspaceFile(f);
      end;
    end;
  end;
end;

procedure TEditorForm.pmAddExistingClick(Sender: TObject);
var
  s : String;
  f : TV2JSWorkspaceFile;
  fmt : integer;
  category : TFslList<TV2JSWorkspaceFile>;
begin
  odImport.InitialDir := FProject.folder;
  if odImport.Execute then
  begin
    if FProject.includesFile(odImport.FileName) then
      raise EFslException.Create('The file "'+odImport.FileName+'" is already in the '+UI_NAME);

    fmt := detectFormat(odImport.FileName);
    if (fmt = 0) then
      raise EFslException.Create('The file "'+odImport.FileName+'" was not understood');

    s := makeRelativePath(odImport.FileName, FProject.folder);
    f := TV2JSWorkspaceFile.Create(s);
    case fmt of
      1: category := FProject.messages;
      2: category := FProject.resources;
      3: category := FProject.scripts;
      4: category := FProject.maps;
      5: category := FProject.templates;
    end;
    category.Add(f);
    FProject.save;
    loadList(category);
    openWorkspaceFile(f);
  end;
end;

procedure TEditorForm.pmNewMapClick(Sender: TObject);
begin
  makeNewFile('Map', 'map', TEMPLATE_MAP, FProject.maps);
end;

procedure TEditorForm.pmNewMessageClick(Sender: TObject);
begin
  makeNewFile('Message', 'msg', TEMPLATE_V2, FProject.messages);
end;

procedure TEditorForm.pmNewResourceClick(Sender: TObject);
begin
  makeNewFile('Resource', 'json', TEMPLATE_JSON, FProject.resources);
end;

procedure TEditorForm.pmNewResourceXmlClick(Sender: TObject);
begin
  makeNewFile('Resource', 'xml', TEMPLATE_XML, FProject.resources);
end;

procedure TEditorForm.pmNewScriptClick(Sender: TObject);
begin
  makeNewFile('Script', 'js', TEMPLATE_JS, FProject.scripts);
end;

procedure TEditorForm.pmNewTemplateClick(Sender: TObject);
begin
  makeNewFile('Template', 'liquid', TEMPLATE_LIQUID, FProject.templates);
end;

procedure TEditorForm.pmWorkspacePopup(Sender: TObject);
var
  tvi : TTreeViewItem;
begin
  tvi := tvWorkspace.Selected;
  if tvi = nil then
  begin
    mnuWorkspaceDropItem.enabled := false;
    mnuWorkspaceDuplicateItem.enabled := false;
    mnuWorkspaceRenameItem.enabled := false;
    mnuWorkspaceAddItem.enabled := false;
    mnuWorkspaceAddItem.Text := '&Add Item';
  end
  else
  begin
    if (tvi.Tag = 0) then
    begin
      mnuWorkspaceDropItem.enabled := true;
      tvi := tvi.ParentItem;
    end
    else
      mnuWorkspaceDropItem.enabled := false;
    mnuWorkspaceDuplicateItem.enabled := mnuWorkspaceDropItem.enabled;
    mnuWorkspaceRenameItem.enabled := mnuWorkspaceDropItem.enabled;
    mnuWorkspaceAddItem.Text := '&Add '+tvi.TagString;
    mnuWorkspaceAddItem.enabled := true;
  end;
end;

procedure TEditorForm.SetFileName(const Value: String);
begin
  FFilename := Value;
  if FFilename <> '' then
    caption := FFIlename+' - v2 Javascript Editor';
end;

procedure TEditorForm.tabsChange(Sender: TObject);
begin
  btnClose.enabled := tabs.ActiveTab <> tabs.Tabs[0];
end;

procedure TEditorForm.openWorkspaceItem(Sender: TObject);
var
  tvi : TTreeViewItem;
begin
  tvi := Sender as TTreeViewItem;
  openWorkspaceFile(tvi.TagObject as TV2JSWorkspaceFile);
end;

procedure TEditorForm.openWorkspaceFile(f: TV2JSWorkspaceFile);
var
  tab : TTabItem;
  editor : TMemo;
  s : String;
  pos : TCaretPosition;
begin
  tab := findWorkspaceFile(f);
  if tab <> nil then
    tabs.ActiveTab := tab
  else
  begin
    tab := tabs.Add(TTabItem);
    tab.Text := f.filename;
    tabs.ActiveTab := tab;
    editor := TMemo.create(tab);
    editor.Parent := tab;
    editor.OnChangeTracking := memoChangeTracking;
    editor.OnExit := memoExit;
    tab.TagObject := editor;
    editor.TagObject := f;
    editor.Align := TAlignLayout.Client;
    s := makeAbsolutePath(f.filename, FProject.folder);
    editor.Lines.LoadFromFile(s);
    pos.line := f.row;
    pos.Pos := 0;
    editor.SelStart := editor.PosToTextPos(pos);
    f.isDirty := false;
  end;
end;

procedure TEditorForm.renameWorkspaceFile(f: TV2JSWorkspaceFile);
var
  tab : TTabItem;
begin
  tab := findWorkspaceFile(f);
  if tab <> nil then
    tab.Text := f.filename;
end;

procedure TEditorForm.saveWorkspaceFile(f: TV2JSWorkspaceFile);
var
  tab : TTabItem;
  s : String;
  m : TMemo;
begin
  tab := findWorkspaceFile(f);
  if tab <> nil then
  begin
    s := makeAbsolutePath(f.filename, FProject.folder);
    m := tab.TagObject as TMemo;
    m.Lines.SaveToFile(s);
    f.isDirty := false;
  end;
end;

procedure TEditorForm.closeWorkspaceFile(f: TV2JSWorkspaceFile; checkSave : boolean);
var
  tab : TTabItem;
begin
  tab := findWorkspaceFile(f);
  if tab <> nil then
  begin
    if f.isDirty and checkSave then
    begin
      case MessageDlg('Do you want to save '+f.filename+'?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], 0) of
        mrCancel : abort;
        mrYes : saveWorkspaceFile(f);
        mrNo : saveWorkspaceFile(f);
      end;
    end;
    if tabs.ActiveTab = tab then
      tabs.TabIndex := tabs.TabIndex - 1;
    tab.Free;
  end;
end;

end.
