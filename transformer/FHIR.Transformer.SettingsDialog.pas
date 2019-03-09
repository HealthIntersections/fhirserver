unit FHIR.Transformer.SettingsDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, IniFiles;

type
  TTransformerOptionsForm = class(TForm)
    Panel1: TPanel;
    btnok: TButton;
    btnCancel: TButton;
    cbAutosave: TCheckBox;
    Label1: TLabel;
    cbxTerminologyServer: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure btnokClick(Sender: TObject);
  private
    FIni : TIniFile;

  public
    Property Ini : TIniFile read FIni write FIni;
  end;

var
  TransformerOptionsForm: TTransformerOptionsForm;

implementation

{$R *.dfm}

procedure TTransformerOptionsForm.btnokClick(Sender: TObject);
begin
  FIni.WriteBool('Workspace', 'AutoSave', cbAutosave.Checked);
  FIni.WriteString('Workspace', 'TerminologyServer', cbxTerminologyServer.Text);
end;

procedure TTransformerOptionsForm.FormShow(Sender: TObject);
begin
  cbAutosave.Checked := FIni.ReadBool('Workspace', 'AutoSave', false);
  FIni.ReadSection('TerminologyServers', cbxTerminologyServer.items);
  if cbxTerminologyServer.items.Count = 0 then
    cbxTerminologyServer.items.Add('http://tx.fhir.org/r4');
  cbxTerminologyServer.Text := FIni.ReadString('Workspace', 'TerminologyServer', '');
end;

end.
