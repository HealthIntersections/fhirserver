unit FHIR.Transformer.ExceptionHandlerDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, clipbrd;

type
  TExceptionHandlerDialog = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    eMessage: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExceptionHandlerDialog: TExceptionHandlerDialog;

implementation

{$R *.DFM}

procedure TExceptionHandlerDialog.Button2Click(Sender: TObject);
begin
  Clipboard.AsText := eMessage.Caption;
end;

end.
