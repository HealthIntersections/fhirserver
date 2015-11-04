unit SettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Buttons,
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TSettingForm = class(TNppForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtServer: TEdit;
    Label2: TLabel;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    edtFile: TEdit;
    SpeedButton1: TSpeedButton;
    od: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SettingForm: TSettingForm;

implementation

{$R *.dfm}

uses
  FHIRPluginSettings,
  FHIRConstants,
  FHIRPlugin;

procedure TSettingForm.Button1Click(Sender: TObject);
begin
  Settings.TerminologyServer := edtServer.Text;
  Settings.DefinitionsSource := edtFile.Text;
  _FuncClearClient;
end;

procedure TSettingForm.FormShow(Sender: TObject);
begin
  edtServer.Text := Settings.TerminologyServer;
  edtFile.Text := Settings.DefinitionsSource;
end;

procedure TSettingForm.SpeedButton1Click(Sender: TObject);
begin
  if od.Execute then
    edtFile.Text := od.FileName;
end;

end.
