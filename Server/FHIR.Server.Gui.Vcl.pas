unit FHIR.Server.Gui.Vcl;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.FileCtrl, Vcl.Menus, Vcl.Buttons,
  Vcl.StdCtrls, Vcl.ExtCtrls,
  FHIR.Support.Utilities, FHIR.Support.Shell,
  FHIR.Server.Ini, FHIR.Server.Gui.Controller;

type
  TServerGUI = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    mLog: TMemo;
    btnStatus: TButton;
    btnBrowser: TButton;
    lblStatus: TLabel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    edtFolder: TEdit;
    edtPort: TEdit;
    BitBtn1: TBitBtn;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    About1: TMenuItem;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnStatusClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtFolderChange(Sender: TObject);
    procedure edtPortChange(Sender: TObject);
    procedure btnBrowserClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FIni : TFHIRServerIniFile;
    FServer : TFHIRServerController;
    procedure serverStatusChange(Sender: TObject);
    procedure log(msg : String);
  public
    { Public declarations }
  end;

var
  ServerGUI: TServerGUI;

implementation

{$R *.dfm}

procedure TServerGUI.FormCreate(Sender: TObject);
begin
  Fini := TFHIRServerIniFile.Create(Path([ExtractFilePath(paramstr(0)), 'fhir-server-gui.ini']));
  FServer := TFHIRServerController.create(FIni.link);
  FServer.OnStatusChange := serverStatusChange;
  FServer.OnLog := log;
  FServer.Initialise;
  edtFolder.Text := FIni.kernel['utg-folder'];
  edtPort.Text := Fini.web['http'];
end;

procedure TServerGUI.FormDestroy(Sender: TObject);
begin
  FServer.Free;
  FIni.Free;
end;

procedure TServerGUI.log(msg: String);
var
   s : String;
begin
  mLog.lines.add(msg);
  s := mlog.text;
  mLog.selStart := s.Length - msg.length;
end;

procedure TServerGUI.BitBtn1Click(Sender: TObject);
var
  s : String;
begin
  s := FIni.kernel['utg-folder'];
  if SelectDirectory('Select UTG Folder', '', s, [sdNewFolder, sdShowEdit, sdNewUI], self) then
  begin
    FIni.kernel['utg-folder'] := s;
    edtFolder.Text := s;
  end;
end;

procedure TServerGUI.edtFolderChange(Sender: TObject);
begin
  FIni.kernel['utg-folder'] := edtFolder.Text;
end;

procedure TServerGUI.edtPortChange(Sender: TObject);
begin
  Fini.web['http'] := edtPort.Text;
end;

procedure TServerGUI.btnBrowserClick(Sender: TObject);
begin
  ExecuteURL(FServer.Address);
end;

procedure TServerGUI.btnStatusClick(Sender: TObject);
begin
  if FServer.Status = ssNotRunning then
  begin
    FServer.checkOk;
    FServer.Start;
    btnStatus.Enabled := false;
  end
  else
  begin
    FServer.Stop;
  end;
end;

procedure TServerGUI.serverStatusChange(Sender: TObject);
begin
  btnBrowser.Enabled := false;
  case FServer.Status of
    ssStarting :
      begin
      btnStatus.Enabled := false;
      btnStatus.Caption := 'Starting...';
      end;
    ssRunning :
      begin
      btnStatus.Enabled := true;
      btnStatus.Caption := 'Stop';
      btnBrowser.Enabled := true;
      end;
    ssStopping :
      begin
      btnStatus.Enabled := false;
      btnStatus.Caption := 'Stopping...';

      end;
    ssNotRunning :
      begin
      btnStatus.Enabled := true;
      btnStatus.Caption := 'Start';
      end;
  end;
end;

procedure TServerGUI.Timer1Timer(Sender: TObject);
begin
  FServer.Ping;
end;

end.



