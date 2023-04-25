unit codeScanForm;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TLogEvent = procedure (msg : String; ack : boolean) of object;

  { TCodeScannerForm }

  TCodeScannerForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
  private
    FOnExecute: TNotifyEvent;

  public
    property OnExecute : TNotifyEvent read FOnExecute write FOnExecute;
    procedure log(msg : String; ack : boolean);
  end;

var
  CodeScannerForm: TCodeScannerForm;

implementation

{$R *.lfm}

{ TCodeScannerForm }

procedure TCodeScannerForm.log(msg: String; ack: boolean);
begin
  if (ack) then
    showMessage(msg)
  else
    memo1.lines.add(msg);
end;

end.

