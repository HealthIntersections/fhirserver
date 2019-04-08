unit FHIR.Transformer.ExecConfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FHIR.Transformer.Workspace;

type
  TTransformerExecConfigForm = class(TForm)
    Panel1: TPanel;
    btnok: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    cbxScripts: TComboBox;
    Label2: TLabel;
    cbxFocus: TComboBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnokClick(Sender: TObject);
  private
    FConfig : TWorkspaceExecConfig;
    FWorkspace : TWorkspace;
    procedure SetConfig(const Value: TWorkspaceExecConfig);
    procedure SetWorkspace(const Value: TWorkspace);
  public
    property Config : TWorkspaceExecConfig read FConfig write SetConfig;
    property Workspace : TWorkspace read FWorkspace write SetWorkspace;
  end;

var
  TransformerExecConfigForm: TTransformerExecConfigForm;

implementation

{$R *.dfm}

procedure TTransformerExecConfigForm.btnokClick(Sender: TObject);
begin
  FConfig.script := cbxScripts.Text;
  FConfig.focus := cbxFocus.Text;
end;

procedure TTransformerExecConfigForm.FormDestroy(Sender: TObject);
begin
  FConfig.Free;
  FWorkspace.Free;
end;

procedure TTransformerExecConfigForm.FormShow(Sender: TObject);
var
  f : TWorkspaceFile;
begin
  cbxScripts.Items.Clear;
  for f in FWorkspace.scripts do
    cbxScripts.Items.AddObject(f.filename, f);
  cbxScripts.ItemIndex := cbxScripts.Items.IndexOf(FConfig.script);

  cbxFocus.Items.Clear;
  for f in FWorkspace.messages do
    cbxFocus.Items.AddObject(f.filename, f);
  for f in FWorkspace.documents do
    cbxFocus.Items.AddObject(f.filename, f);
  cbxFocus.ItemIndex := cbxFocus.Items.IndexOf(FConfig.focus);
end;

procedure TTransformerExecConfigForm.SetConfig(const Value: TWorkspaceExecConfig);
begin
  FConfig.Free;
  FConfig := Value;
end;

procedure TTransformerExecConfigForm.SetWorkspace(const Value: TWorkspace);
begin
  FWorkspace.Free;
  FWorkspace := Value;
end;

end.
