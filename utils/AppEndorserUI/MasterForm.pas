unit MasterForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.TabControl, FMX.Controls.Presentation, FMX.DialogService,
  IniFiles,
  SystemSupport,
  FHIRClient,
  ServerForm;

type
  TMasterToolsForm = class(TForm)
    tbMain: TTabControl;
    Label2: TLabel;
    TabItem1: TTabItem;
    pnlToolbar: TPanel;
    Panel1: TPanel;
    lbServers: TListBox;
    btnConnect: TButton;
    btnAddServer: TButton;
    btnRemoveServer: TButton;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Label1: TLabel;
    lbFiles: TListBox;
    btnReopen: TButton;
    btnRemoveFile: TButton;
    btnOpen: TButton;
    btnNew: TButton;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbServersClick(Sender: TObject);
    procedure lbFilesClick(Sender: TObject);
    procedure btnRemoveServerClick(Sender: TObject);
    procedure btnAddServerClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    { Private declarations }
    FIni : TIniFile;
    procedure saveServers;
    procedure saveFiles;
  public
    { Public declarations }
  end;

var
  MasterToolsForm: TMasterToolsForm;

implementation

{$R *.fmx}

procedure TMasterToolsForm.btnAddServerClick(Sender: TObject);
begin
  TDialogService.InputQuery('Server Address', ['URL'], [''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if (AResult = mrOK) and (aValues[0] <> '') then
      begin
        lbServers.Items.Insert(0, aValues[0]);
      end;
    end);
end;

procedure TMasterToolsForm.btnConnectClick(Sender: TObject);
var
  client : TFhirHTTPClient;
  tab : TTabItem;
  serverForm : TServerFrameForm;
begin
  client := TFhirHTTPClient.Create(nil, lbServers.Items[lbServers.ItemIndex], false);
  try
    tab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := tab;
    tab.Text := lbServers.Items[lbServers.ItemIndex];
    serverForm := TServerFrameForm.create(tab);
    serverForm.Parent := tab;
    serverForm.Align := TAlignLayout.Client;
    serverForm.Client := client.link;
  finally
    client.Free;
  end;
end;

procedure TMasterToolsForm.btnRemoveServerClick(Sender: TObject);
var
  i : integer;
begin
  i := lbServers.ItemIndex;
  lbServers.items.Delete(i);
  if i = lbServers.items.Count then
    dec(i);
  lbServers.ItemIndex := i;
  saveServers;
  lbServersClick(nil);
end;

procedure TMasterToolsForm.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.Create(IncludeTrailingPathDelimiter(SystemTemp) + 'settings.ini');
  FIni.ReadSection('Servers', lbServers.Items);
  if lbServers.Items.count = 0 then
    lbServers.Items.add('http://test.fhir.org');
  lbServers.ItemIndex := 0;
  lbServersClick(self);
  FIni.ReadSection('Files', lbFiles.Items);
  if lbFiles.Items.count > 0 then
    lbFiles.ItemIndex := 0;
  lbFilesClick(self);
end;

procedure TMasterToolsForm.FormDestroy(Sender: TObject);
var
  s : String;
begin
  saveServers;
  saveFiles;
  FIni.Free;
end;

procedure TMasterToolsForm.lbFilesClick(Sender: TObject);
begin
  btnReopen.Enabled := lbFiles.ItemIndex >= 0;
  btnRemoveFile.Enabled := lbFiles.ItemIndex >= 0;
end;

procedure TMasterToolsForm.lbServersClick(Sender: TObject);
begin
  btnConnect.Enabled := lbServers.ItemIndex >= 0;
  btnRemoveServer.Enabled := lbServers.ItemIndex >= 0;
end;

procedure TMasterToolsForm.saveFiles;
var
  s : String;
begin
  try
    FIni.EraseSection('Files');
    for s in lbFiles.Items do
      FIni.WriteString('Files', s, '');
  except
    // nothing we can do
  end;
end;

procedure TMasterToolsForm.saveServers;
var
  s : String;
begin
  try
    FIni.EraseSection('Servers');
    for s in lbServers.Items do
      FIni.WriteString('Servers', s, '');
  except
    // nothing we can do
  end;
end;

end.


