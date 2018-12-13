unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Menus, FMX.Controls.Presentation, FMX.StdCtrls,
  FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Javascript,
  FHIR.v2.Protocol, FHIR.v2.Message, FHIR.v2.Javascript, FMX.TabControl,
  FMX.TreeView, FMX.Layouts;

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
    Label1: TLabel;
    TreeView1: TTreeView;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TabControl1: TTabControl;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Panel6: TPanel;
    Splitter2: TSplitter;
    Label2: TLabel;
    Memo1: TMemo;
    TabItem1: TTabItem;
    Button1: TButton;
    Button2: TButton;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure editorChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FIni : TIniFile;
    FFilename: String;
    FDirty : Boolean;
    FJs : TJavascript;
    procedure SetFileName(const Value: String);
    procedure JSLog(sender : TJavascript; message : String);
    procedure DoSave;
  public
    Property Filename : String read FFilename write SetFileName;
  end;

var
  EditorForm: TEditorForm;

implementation

{$R *.fmx}

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

procedure TEditorForm.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.create(Path([SystemTemp, 'v2JS.ini']));
  FJs := TJavascript.Create('ChakraCore.dll');
  FJs.OnLog := JSLog;
  TV2JavascriptHelper.registerv2Objects(FJs);
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
begin
  DoSave;
  FJs.Free;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
  FileName := FIni.readString('Editor', 'filename', '');
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

procedure TEditorForm.SetFileName(const Value: String);
begin
  FFilename := Value;
  if FFilename <> '' then
    caption := FFIlename+' - v2 Javascript Editor';
end;

end.
