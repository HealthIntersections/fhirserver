unit NewServerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmNewServer: TfrmNewServer;

implementation

{$R *.dfm}

end.
