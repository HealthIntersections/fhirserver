unit frmServerConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TServerConnectionForm }

  TServerConnectionForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtServer: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private

  public

  end;

var
  ServerConnectionForm: TServerConnectionForm;

implementation

{$R *.lfm}

{ TServerConnectionForm }

procedure TServerConnectionForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not (SameText(edtServer.text, 'Localhost') or SameText(edtServer.text, '127.0.0.1')) then
    CanClose := edtPassword.text <> '';
end;

end.

