unit NewServerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, ValueSetEditorCore;

type
  TfrmNewServer = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    edtName: TEdit;
    edtAddress: TEdit;
    Label3: TLabel;
    edtUsername: TEdit;
    Label4: TLabel;
    edtPassword: TEdit;
    CheckBox1: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    FContext : TValueSetEditorContext;
    FDoesSearch : boolean;
    procedure SetContext(const Value: TValueSetEditorContext);
  public
    property Context : TValueSetEditorContext read FContext write SetContext;
    Property DoesSearch : boolean read FDoesSearch write FDoesSearch;
  end;

var
  frmNewServer: TfrmNewServer;

implementation

{$R *.dfm}

procedure TfrmNewServer.Button1Click(Sender: TObject);
var
  msg : String;
begin
  screen.Cursor := crHourGlass;
  Panel1.Caption := '   Checking Server';
  try
    Panel1.Update;
    if Context.CheckServer(frmNewServer.edtAddress.Text, msg, FDoesSearch) then
      ModalResult := mrOk
  finally
    screen.Cursor := crDefault;
    Panel1.Caption := '';
  end;
  if ModalResult <> mrOk then
    ShowMessage(msg);
end;

procedure TfrmNewServer.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    edtPassword.PasswordChar := #0
  else
    edtPassword.PasswordChar := '*';
end;

procedure TfrmNewServer.FormDestroy(Sender: TObject);
begin
  FContext.Free;
end;

procedure TfrmNewServer.SetContext(const Value: TValueSetEditorContext);
begin
  FContext.Free;
  FContext := value;
end;

end.
