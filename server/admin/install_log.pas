unit install_log;

{$i fhir.inc}

interface

uses
  Process,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ComCtrls,
  ExtCtrls, StdCtrls,
  fsl_base, fsl_threads;

type
  TInstallProgressForm = class;

  TInstallerThread = class (TFslThread)
  private
    FForm : TInstallProgressForm;
    process : TProcess;
  protected
    procedure execute; override;
  end;

  { TInstallProgressForm }

  TInstallProgressForm = class(TForm)
    BitBtn1: TBitBtn;
    btnDBTest3: TBitBtn;
    lblStatus: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnDBTest3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCommand: String;
    FIncoming : TStringList;
    FThread : TInstallerThread;
    FLock : TFslLock;

    procedure processOutput(s : String);
    procedure log(s : String);

  public
    property command : String read FCommand write FCommand;
  end;

var
  InstallProgressForm: TInstallProgressForm;

implementation

{$R *.lfm}

const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks

{ TInstallProgressForm }

procedure TInstallProgressForm.FormCreate(Sender: TObject);
begin
  FThread := TInstallerThread.create;
  FThread.FForm := self;
  FLock := TFslLock.create;
  FIncoming := TStringList.create;
  Timer1.Enabled := true;
end;

procedure TInstallProgressForm.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := false;
  FThread.StopAndWait(2000);
  FThread.Free;
  FIncoming.Free;
  FLock.Free;
end;

procedure TInstallProgressForm.Button1Click(Sender: TObject);
begin
  FThread.Start;
end;

procedure TInstallProgressForm.BitBtn1Click(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TInstallProgressForm.btnDBTest3Click(Sender: TObject);
begin
  if FThread.process <> nil then
    FThread.process.Terminate(0);
  ModalResult := mrCancel;
end;

procedure TInstallProgressForm.processOutput(s: String);
begin
  FLock.Lock;
  try
    FIncoming.add(s);
  finally
    FLock.unlock;
  end;
end;

procedure TInstallProgressForm.Timer1Timer(Sender: TObject);
var
  s : String;
begin
  FLock.Lock;
  try
    for s in FIncoming do
      log(s);
    FIncoming.clear;
  finally
    FLock.unlock;
  end;
end;

procedure TInstallProgressForm.log(s: String);
begin
  Memo1.lines.Add(s);
end;

{ TInstallerThread }

procedure TInstallerThread.execute;
var
  BytesRead    : longint;
  Buffer       : TBytes;
  s : String;
begin
  fform.processOutput('Running Server to do the install...');
  process := TProcess.create(nil);
  try
    process.Executable := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)))+'fhirserver.exe';
    for s in FForm.command.split([' ']) do
      process.Parameters.add(s);
    process.Options := [poUsePipes];
    process.ShowWindow := swoHIDE;
    process.Execute;
    repeat
      SetLength(Buffer, BUF_SIZE);
      BytesRead := process.Output.Read(Buffer, BUF_SIZE);
      fform.processOutput(TEncoding.UTF8.GetString(Buffer, 0, BytesRead));
    until BytesRead = 0;
  finally
    process.free;
  end;
  process := nil;
  fform.processOutput('Server Process Terminated');
end;


end.

