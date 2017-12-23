unit ProgressDialog;

{
Simple Dialog that shows while an action is occuring, and
gives the user to cancel the action
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TProgressWindow = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    lbCounter: TLabel;
    Timer: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FStopped : Boolean;
    FMessage: string;
    procedure SetMessage(const Value: string);
  public
    Property Message : string read FMessage write SetMessage;
    property Stopped : boolean read FStopped;
  end;

var
  ProgressWindow: TProgressWindow;

implementation

{$R *.dfm}


procedure TProgressWindow.FormShow(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TProgressWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer.Enabled := False;
  lbCounter.Caption := '0';
end;

procedure TProgressWindow.Button1Click(Sender: TObject);
begin
  FStopped := true;
  Close;
end;

procedure TProgressWindow.SetMessage(const Value: string);
begin
  FMessage := Value;
  Label1.Caption := Value;
  Label1.Update;
end;

procedure TProgressWindow.TimerTimer(Sender: TObject);
var
  cnt: Integer;
begin
  cnt := StrToInt(lbCounter.Caption);
  inc(cnt);
  lbCounter.Caption := IntToStr(cnt);
end;

end.
