unit ValueSetEditorRegisterServerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmRegisterServer = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    edtName: TEdit;
    Panel2: TPanel;
    Label2: TLabel;
    edtAddress: TEdit;
    btnOpenFile: TButton;
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRegisterServer1: TfrmRegisterServer;

implementation

{$R *.dfm}

end.
