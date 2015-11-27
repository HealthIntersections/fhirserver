unit UpgradePrompt;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.ExtCtrls, StringSupport,
  Vcl.ComCtrls, Vcl.CheckLst, Vcl.Imaging.pngimage, nppplugin, shellapi;

type
  TUpgradePromptForm = class(TNppForm)
    Panel1: TPanel;
    btnOk: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    FLink : String;
  public
    { Public declarations }
  end;

var
  UpgradePromptForm: TUpgradePromptForm;

procedure ShowUpgradePrompt(owner : TNppPlugin; link, notes : String);

implementation

{$R *.dfm}

uses
  FHIRPluginSettings, FHIRClient, FHIRResources, SmartOnFhirUtilities, SettingsForm;

procedure ShowUpgradePrompt(owner : TNppPlugin; link, notes : String);
begin
  UpgradePromptForm := TUpgradePromptForm.create(owner);
  try
    UpgradePromptForm.Memo1.text := notes;
    UpgradePromptForm.FLink := link;
    UpgradePromptForm.ShowModal;
  finally
    FreeAndNil(UpgradePromptForm);
  end;
end;

procedure TUpgradePromptForm.btnOkClick(Sender: TObject);
begin
  Settings.BuildPrompt := FLink;
end;

procedure TUpgradePromptForm.Button1Click(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(FLink), '', '', SW_SHOWNORMAL);
end;

end.
