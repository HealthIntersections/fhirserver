unit FHIRToolkitForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Platform,
  FMX.Layouts, FMX.ListBox, FMX.TabControl, FMX.Controls.Presentation, FMX.DialogService,
  IniFiles,
  SystemSupport,
  FHIRBase, FHIRTypes, FHIRResources, FHIRClient, FHIRUtilities,
  ServerForm, CapabilityStatementEditor, BaseResourceFrame;

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
    odFile: TOpenDialog;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbServersClick(Sender: TObject);
    procedure lbFilesClick(Sender: TObject);
    procedure btnRemoveServerClick(Sender: TObject);
    procedure btnAddServerClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnReopenClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    { Private declarations }
    FIni : TIniFile;

    procedure saveServers;
    procedure saveFiles;
    procedure openResourceFromFile(filename : String; res : TFHIRResource; frameClass : TBaseResourceFrameClass);
    procedure addFileToList(filename : String);
  public
    { Public declarations }
  end;

var
  MasterToolsForm: TMasterToolsForm;

implementation

{$R *.fmx}

procedure TMasterToolsForm.addFileToList(filename: String);
var
  i : integer;
begin
  for i := lbFiles.Count - 1 downto 0 do
    if lbFiles.items[i] = filename then
      lbFiles.Items.Delete(i);
  lbFiles.Items.Insert(0, filename);
  saveFiles;
  lbFilesClick(nil);
end;

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
  serverForm : TServerFrame;
  cs : TFhirCapabilityStatement;
  fcs : IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    client := TFhirHTTPClient.Create(nil, lbServers.Items[lbServers.ItemIndex], false);
    try
      cs := client.conformance(false);
      try
        tab := tbMain.Add(TTabItem);
        tbMain.ActiveTab := tab;
        tab.Text := lbServers.Items[lbServers.ItemIndex];
        serverForm := TServerFrame.create(tab);
        serverForm.Parent := tab;
        serverForm.tabs := tbMain;
        serverForm.Tab := tab;
        serverForm.Align := TAlignLayout.Client;
        serverForm.Client := client.link;
        serverForm.CapabilityStatement := cs.link;
        serverForm.load;
      finally
        cs.free;
      end;
    finally
      client.Free;
    end;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.btnOpenClick(Sender: TObject);
var
  res : TFhirResource;
begin
  if odFile.Execute then
  begin
    try
      res := fileToResource(odFile.Filename, ffUnspecified);
      try
        if res is TFhirCapabilityStatement then
          openResourceFromFile(odFile.Filename, res, TCapabilityStatementEditorFrame)
        else
          MessageDlg('Unsupported Resource Type: '+res.fhirType, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      finally
        res.free;
      end;
    except
      on e : Exception do
        MessageDlg('Error reading Resource: '+e.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end;
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

procedure TMasterToolsForm.btnReopenClick(Sender: TObject);
var
  res : TFhirResource;
  fn : String;
begin
  fn := lbFiles.Items[lbFiles.ItemIndex];
  try
    res := fileToResource(fn, ffUnspecified);
    try
      if res is TFhirCapabilityStatement then
        openResourceFromFile(fn, res, TCapabilityStatementEditorFrame)
      else
        MessageDlg('Unsupported Resource Type: '+res.fhirType, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    finally
      res.free;
    end;
  except
    on e : Exception do
      MessageDlg('Error reading Resource: '+e.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TMasterToolsForm.CloseButtonClick(Sender: TObject);
begin
  showMessage('duh');
end;

procedure TMasterToolsForm.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.Create(IncludeTrailingPathDelimiter(SystemTemp) + 'settings.ini');
  FIni.ReadSection('Servers', lbServers.Items);
  if lbServers.Items.count = 0 then
    lbServers.Items.add('http://test.fhir.org/r3');
  lbServers.ItemIndex := 0;
  lbServersClick(self);
  FIni.ReadSection('Files', lbFiles.Items);
  if lbFiles.Items.count > 0 then
    lbFiles.ItemIndex := 0;
  lbFilesClick(self);
end;

procedure TMasterToolsForm.FormDestroy(Sender: TObject);
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

procedure TMasterToolsForm.openResourceFromFile(filename: String; res: TFHIRResource; frameClass: TBaseResourceFrameClass);
var
  tab : TTabItem;
  frame : TFrame;
  fcs : IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    tab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := tab;
    tab.Text := ExtractFileName(filename);
    tab.Hint := filename;
    tab.ShowHint := true;
    frame := frameClass.create(tab);
    frame.Parent := tab;
    frame.tabs := tbMain;
    frame.Tab := tab;
    frame.Align := TAlignLayout.Client;
    frame.Filename := filename;
    frame.resource := res.link;
    frame.load;
    addFileToList(filename);
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
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


