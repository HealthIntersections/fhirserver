unit install_log;

{$i fhir.inc}

interface

uses
  Process,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ComCtrls,
  ExtCtrls, StdCtrls,
  fsl_base;

type
  { TInstallProgressForm }

  TInstallProgressForm = class(TForm)
    BitBtn1: TBitBtn;
    btnDBTest3: TBitBtn;
    Button1: TButton;
    lblStatus: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
  private
    FCommand: String;
    FProcess : TProcess;

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

procedure TInstallProgressForm.Button1Click(Sender: TObject);
var
  BytesRead    : longint;
  Buffer       : TBytes;
begin
  FProcess := TProcess.create(nil);
  try
    FProcess.Executable := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)))+'fhirserver.exe';
    FProcess.Parameters.add(command);
    FProcess.Options := [poUsePipes];
    FProcess.Execute;
    repeat
      SetLength(Buffer, BUF_SIZE);
      BytesRead := FProcess.Output.Read(Buffer, BUF_SIZE);
      processOutput(TEncoding.UTF8.GetString(Buffer, 0, BytesRead));
    until BytesRead = 0;

  finally
    FProcess.free;
  end;
end;

procedure TInstallProgressForm.processOutput(s: String);
begin
  log(s);
end;

procedure TInstallProgressForm.log(s: String);
begin
  Memo1.lines.Add(s);
end;

end.

