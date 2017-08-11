unit TxServerFormUnit;

interface

uses
  {$IFNDEF OSX} WIndows, {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus, FMX.Platform,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Edit, IOUtils,
  OSXUIUtils,
  FileSupport, SystemSupport, kCritSct, StringSupport, DateSupport,
  FHIRResources, FHIRParser,
  FHIRServerContext, FHIRUserProvider, FHIRStorageService, FHIRRestServer, ServerUtilities, FHIRLog,
  VocabPocServerCore, TerminologyServer, FMX.WebBrowser;

const
  MIN_WIDTH = 400;
  MIN_HEIGHT = 200;

type
  TServerStatus = (Resting, Starting, Running, Stopping);

  TServerControl = class (TThread)
  private
    FStatus : TServerStatus;
    FSource : String;
    FWantStart : boolean;
    FWantStop : boolean;
    FLock : TCriticalSection;
    FMsgs : TStringList;
    FIni : TFHIRServerIniFile;
    FWebServer : TFhirWebServer;
    FStart : TDateTime;

    procedure loadVocab(server : TTerminologyServer; folder : String);
    procedure loadPoC(server : TTerminologyServer);
    procedure DoStart;
    procedure DoStop;
    procedure log(s : String);
    procedure init;
    procedure close;
  protected
    procedure Execute; override;
  public
    procedure start;
    procedure stop;
  end;

  TTxServerForm = class(TForm)
    pnlSettings: TPanel;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuHelp: TMenuItem;
    mnuServer: TMenuItem;
    mnuExit: TMenuItem;
    mnuAbout: TMenuItem;
    mnuStatus: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtFolder: TEdit;
    edtPort: TEdit;
    edtSnomed: TEdit;
    pnlStatus: TPanel;
    StyleBook1: TStyleBook;
    btnStatus: TButton;
    lblStatus: TLabel;
    mLog: TMemo;
    btnPoCFolder: TButton;
    btnSnomed: TButton;
    odSnomed: TOpenDialog;
    tmrStatus: TTimer;
    web: TWebBrowser;
    Splitter1: TSplitter;
    mnuView: TMenuItem;
    mnuViewSettings: TMenuItem;
    mnuViewLog: TMenuItem;
    mnuViewBrowser: TMenuItem;
    procedure mnuExitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure btnPoCFolderClick(Sender: TObject);
    procedure btnSnomedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrStatusTimer(Sender: TObject);
    procedure btnStatusClick(Sender: TObject);
    procedure mnuViewSettingsClick(Sender: TObject);
    procedure mnuViewBrowserClick(Sender: TObject);
    procedure mnuViewLogClick(Sender: TObject);
  private
    { Private declarations }
    FIni : TIniFile;
    FStatus : TServerStatus;
    FServer : TServerControl;
    FWantClose : boolean;

  public
    { Public declarations }
  end;

var
  TxServerForm: TTxServerForm;

implementation

{$R *.fmx}

procedure TTxServerForm.btnPoCFolderClick(Sender: TObject);
var
  s : String;
begin
  if SelectDirectory(Handle, 'Select Vocab PoC', edtFolder.text, s) then
    edtFolder.Text := s;
end;

procedure TTxServerForm.btnSnomedClick(Sender: TObject);
begin
  odSnomed.InitialDir := PathFolder(edtSnomed.Text);
  if odSnomed.Execute then
    edtSnomed.Text := odSnomed.FileName;
end;

procedure TTxServerForm.btnStatusClick(Sender: TObject);
begin
  if btnStatus.Text = 'Start' then
  begin
    if not StringIsInteger16(edtPort.Text) then
      raise Exception.Create('Port must be a Positive Integer between 0 and 65535');

    FServer.FSource := edtFolder.Text;
    FServer.FIni.WriteString('web', 'http', edtPort.Text);
    FServer.FIni.WriteString('web', 'host', 'localhost');
    FServer.FIni.WriteString('web', 'base', '/');
    FServer.FIni.WriteString('web', 'clients', IncludeTrailingPathDelimiter(SystemTemp) + 'auth.ini');
    FServer.FIni.WriteString('admin', 'email', 'grahame@hl7.org');
    if FileExists(path([ExtractFilePath(paramstr(0)), 'ucum-essence.xml'])) then
      FServer.FIni.WriteString('ucum', 'source', path([ExtractFilePath(paramstr(0)), 'ucum-essence.xml']))
    else
      FServer.FIni.WriteString('ucum', 'source', 'C:\work\fhirserver\Exec\ucum-essence.xml');
    if FileExists(path([ExtractFilePath(paramstr(0)), 'loinc.cache'])) then
      FServer.FIni.WriteString('loinc', 'cache', path([ExtractFilePath(paramstr(0)), 'loinc.cache']))
    else
      FServer.FIni.WriteString('loinc', 'cache', 'C:\ProgramData\fhirserver\loinc_261.cache');
    FServer.FIni.WriteString('snomed', 'cache', edtSnomed.text);
    FServer.start;
  end
  else
    FServer.stop;
end;

procedure TTxServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caNone;
  case FStatus of
    Resting :
      Action := TCloseAction.caFree;
    Starting :
      beep;
    Running:
      begin
      FWantClose := true;
      FServer.Stop;
      end;
    Stopping:
      FWantClose := true;
  end;
end;

procedure TTxServerForm.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.Create(IncludeTrailingPathDelimiter(SystemTemp) + 'vocab-server.ini');
  edtFolder.Text := Fini.ReadString('server', 'folder', '');
  edtPort.Text := Fini.ReadString('server', 'port', '');
  edtSnomed.Text := Fini.ReadString('server', 'snomed', '');
  FServer := TServerControl.Create;
  FStatus := Stopping;
  mnuViewSettings.IsChecked := true;
  mnuViewLog.IsChecked := true;
  mnuViewBrowser.IsChecked := true;
  tmrStatusTimer(nil);
end;

procedure TTxServerForm.FormDestroy(Sender: TObject);
begin
  FServer.Free;
  Fini.WriteString('server', 'folder', edtFolder.Text);
  Fini.WriteString('server', 'port', edtPort.Text);
  Fini.WriteString('snomed', 'snomed', edtSnomed.Text);
  FIni.Free;
end;

procedure TTxServerForm.FormResize(Sender: TObject);
begin
  if ClientWidth < MIN_WIDTH then
    ClientWidth := MIN_WIDTH
  else if ClientHeight < MIN_HEIGHT then
    ClientHeight := MIN_HEIGHT
  else
    exit;
  {$IFNDEF OSX}
  Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
  {$ENDIF}
end;

procedure TTxServerForm.mnuExitClick(Sender: TObject);
begin
  Close;
end;


procedure TTxServerForm.mnuViewBrowserClick(Sender: TObject);
begin
  if not mnuViewBrowser.IsChecked then
  begin
    web.Visible := true;
    Splitter1.Visible := true;
    mnuViewBrowser.IsChecked := true;
  end
  else if not mnuViewLog.IsChecked then
    Beep
  else
  begin
    web.Visible := false;
    Splitter1.Visible := false;
    mnuViewBrowser.IsChecked := false;
  end;
end;

procedure TTxServerForm.mnuViewLogClick(Sender: TObject);
begin
  if not mnuViewLog.IsChecked then
  begin
    mLog.Visible := true;
    Splitter1.Visible := true;
    web.Align := TAlignLayout.MostBottom;
    if web.Size.Height > ClientHeight - (pnlStatus.Size.Height + 80 + pnlSettings.Size.Height) then
      web.Size.Height := ClientHeight - (pnlStatus.Size.Height + 80 + pnlSettings.Size.Height);
    mnuViewLog.IsChecked := true;
  end
  else if not mnuViewBrowser.IsChecked then
    Beep
  else
  begin
    mLog.Visible := false;
    Splitter1.Visible := false;
    web.Align := TAlignLayout.Client;
    mnuViewLog.IsChecked := false;
  end;

end;

procedure TTxServerForm.mnuViewSettingsClick(Sender: TObject);
begin
  if mnuViewSettings.IsChecked then
  begin
    pnlSettings.size.height := 0;
    mnuViewSettings.IsChecked := false;
  end
  else
  begin
    pnlSettings.size.height := 105;
    mnuViewSettings.IsChecked := true;
  end;
end;

procedure TTxServerForm.tmrStatusTimer(Sender: TObject);
var
  b : boolean;
begin
  if FServer.FStatus <> FStatus then
  begin
    FStatus := FServer.FStatus;
    case FStatus of
      Resting:
         begin
         if FWantClose then
           Close;
         btnStatus.Text := 'Start';
         btnStatus.enabled := true;
         mnuStatus.Text := 'Start';
         mnuStatus.enabled := true;
         edtFolder.enabled := true;
         edtPort.enabled := true;
         edtSnomed.enabled := true;
         btnPoCFolder.enabled := true;
         btnSnomed.enabled := true;
         lblStatus.Text := 'The server is not running';
         web.Enabled := false;
         end;
      Starting:
         begin
         btnStatus.Text := 'Starting';
         btnStatus.enabled := false;
         mnuStatus.Text := 'Starting';
         mnuStatus.enabled := false;
         edtFolder.enabled := false;
         edtPort.enabled := false;
         edtSnomed.enabled := false;
         btnPoCFolder.enabled := false;
         btnSnomed.enabled := false;
         lblStatus.Text := 'The server is starting';
         web.Enabled := false;
         end;
      Running:
         begin
         btnStatus.Text := 'Stop';
         btnStatus.enabled := true;
         mnuStatus.Text := 'Stop';
         mnuStatus.enabled := true;
         edtFolder.enabled := false;
         edtPort.enabled := false;
         edtSnomed.enabled := false;
         btnPoCFolder.enabled := false;
         btnSnomed.enabled := false;
         lblStatus.Text := 'The server is running';
         web.Enabled := true;
         if web.Visible then
           web.Navigate(FServer.FWebServer.ServerContext.FormalURLPlain);
         end;
      Stopping:
         begin
         btnStatus.Text := 'Stopping';
         btnStatus.enabled := false;
         mnuStatus.Text := 'Stopping';
         mnuStatus.enabled := false;
         edtFolder.enabled := false;
         edtPort.enabled := false;
         edtSnomed.enabled := false;
         btnPoCFolder.enabled := false;
         btnSnomed.enabled := false;
         lblStatus.Text := 'The server is stopping';
         web.Enabled := false;
         end;
    end;
  end;
  FServer.FLock.Enter;
  try
    b := FServer.FMsgs.Count > 0;
    if b then
    begin
      mLog.Lines.AddStrings(FServer.FMsgs);
      FServer.FMsgs.Clear;
    end;
  finally
    FServer.FLock.Leave;
  end;
  if b then
   mLog.GoToTextEnd;
end;

{ TServerControl }

procedure TServerControl.close;
begin
  FLock.Free;
  FMsgs.Free;
  FIni.Free;
end;

procedure TServerControl.DoStart;
var
  ctxt : TFHIRServerContext;
  store : TTerminologyServerStorage;
begin
  FStart := now;
  FStatus := Starting;
  Log('Starting Server');
  Try
    store := TTerminologyServerStorage.create();
    try
      ctxt := TFHIRServerContext.Create(store.Link);
      try
        ctxt.TerminologyServer := TTerminologyServer.create(nil);
        ctxt.TerminologyServer.load(FIni);
        loadPoC(ctxt.TerminologyServer);
        ctxt.ownername := 'Vocab PoC Server';
        FWebServer := TFhirWebServer.create(FIni.Link, ctxt.ownername, nil {FTerminologyServer}, ctxt.link);
        ctxt.UserProvider := TTerminologyServerUserProvider.Create;
        ctxt.userProvider.OnProcessFile := FWebServer.ReturnProcessedFile;
        FWebServer.AuthServer.UserProvider := ctxt.userProvider.Link;
        FWebServer.Start(true);
      finally
        ctxt.free;
      end;
    finally
      store.free;
    end;

    Log('Server is Started and ready for business at '+FWebServer.ServerContext.FormalURLPlain);
    FStatus := Running;
  except
    on e : Exception do
    begin
      Log('Server failed to Start: '+e.message);
      FStatus := Resting;
      if FWebServer <> nil then
      try
        FWebServer.Free;
      except
      end;
    end;
  End;
end;

procedure TServerControl.DoStop;
begin
  FStatus := Stopping;
  Log('Stopping');
  FWebServer.Stop;
  FWebServer.Free;
  Log('Stopped');
  FStatus := Resting;
end;

procedure TServerControl.Execute;
begin
  init;
  try
    try
      repeat
        if FWantStart then
        begin
          FWantStart := false;
          DoStart;
        end;
        if FWantStop then
        begin
          FWantStop := false;
          DoStop;
        end;
        sleep(50);
      until Terminated;
    except
      on e:exception do
        Log('internal thread exiting: '+e.Message);
    end;
  finally
    close;
  end;
end;

procedure TServerControl.init;
begin
  FMsgs := TStringList.create;
  FLock := TCriticalSection.Create('Control Thread');
  FIni := TFHIRServerIniFile.Create('');
  logevent := log;
end;

procedure TServerControl.loadPoC(server: TTerminologyServer);
begin
  log('Loading Vocab from '+FSource);
  loadVocab(server, 'fhir');
  loadVocab(server, 'v2');
  loadVocab(server, 'v3');
  loadVocab(server, 'cda');
  loadVocab(server, 'cimi');
end;

procedure TServerControl.loadVocab(server: TTerminologyServer; folder: String);
var
  filename : String;
  count : integer;
  procedure load(fn : String);
  var
    res : TFHIRResource;
  begin
    inc(count);
    res := TFHIRXmlParser.ParseFile(nil, 'en', fn);
    try
      server.SeeTerminologyResource(res);
    finally
      res.Free;
    end;
  end;
begin
  count := 0;
  log(folder+'...');
  for filename in TDirectory.GetFiles(path([FSource, folder]), '*.xml', TSearchOption.soTopDirectoryOnly) do
    load(filename);
  if  FolderExists(path([FSource, folder, 'codeSystems'])) then
    for filename in TDirectory.GetFiles(path([FSource, folder, 'codeSystems']), '*.xml', TSearchOption.soTopDirectoryOnly) do
      load(filename);
  if  FolderExists(path([FSource, folder, 'valueSets'])) then
    for filename in TDirectory.GetFiles(path([FSource, folder, 'valueSets']), '*.xml', TSearchOption.soTopDirectoryOnly) do
      load(filename);
  log(inttostr(count)+' resources loaded');
end;

procedure TServerControl.log(s: String);
begin
  s := DescribePeriod(now - FStart)+' '+s;
  FLock.Enter;
  try
    FMsgs.Add(s);
  finally
    FLock.Leave;
  end;
end;

procedure TServerControl.start;
begin
  FWantStart := true;
end;

procedure TServerControl.stop;
begin
  FWantStop := true;
end;

end.
