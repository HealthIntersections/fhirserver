unit SettingsDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.EditBox,
  FMX.SpinBox, FMX.Edit, FMX.StdCtrls, FMX.TabControl, FMX.Controls.Presentation,
  IniFiles;

type
  TSettingsForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    edtProxy: TEdit;
    edtTimeout: TSpinBox;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FIni : TIniFile;
  public
    { Public declarations }
    Property Ini : TIniFile read FIni write FIni;
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.fmx}

procedure TSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FIni.WriteString('HTTP', 'proxy', edtProxy.Text);
  FIni.WriteInteger('HTTP', 'timeout', trunc(edtTimeout.Value));
end;

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := true;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  edtProxy.Text := FIni.ReadString('HTTP', 'proxy', '');
  edtTimeout.Value := FIni.ReadInteger('HTTP', 'timeout', 5);
end;

end.
