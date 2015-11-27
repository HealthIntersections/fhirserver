unit WelcomeScreen;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.ExtCtrls, StringSupport,
  Vcl.ComCtrls, Vcl.CheckLst, Vcl.Imaging.pngimage, nppplugin;

type
  TWelcomeScreenForm = class(TNppForm)
    Panel1: TPanel;
    btnOk: TButton;
    chkWelcomeScreen: TCheckBox;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Image1: TImage;
    Image2: TImage;
    Label4: TLabel;
    Label5: TLabel;
    chkToolbox: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    chkVisualizer: TCheckBox;
    Image3: TImage;
    Label3: TLabel;
    Label8: TLabel;
    chkValidation: TCheckBox;
    Button1: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WelcomeScreenForm: TWelcomeScreenForm;

procedure ShowWelcomeScreen(owner : TNppPlugin);

implementation

{$R *.dfm}

uses
  FHIRPluginSettings, FHIRClient, FHIRResources, SmartOnFhirUtilities, SettingsForm;

procedure ShowWelcomeScreen(owner : TNppPlugin);
begin
  WelcomeScreenForm := TWelcomeScreenForm.Create(owner);
  try
    WelcomeScreenForm.ShowModal;
  finally
    FreeAndNil(WelcomeScreenForm);
  end;
end;

procedure TWelcomeScreenForm.btnOkClick(Sender: TObject);
begin
  Settings.ToolboxVisible := chkToolbox.Checked;
  Settings.NoWelcomeScreen := chkWelcomeScreen.Checked;
  Settings.VisualiserVisible := chkVisualizer.Checked;
  Settings.BackgroundValidation := chkValidation.Checked;
end;

procedure TWelcomeScreenForm.Button1Click(Sender: TObject);
begin
  SettingForm := TSettingForm.Create(self);
  try
    SettingForm.ShowModal;
  finally
    FreeAndNil(SettingForm);
  end;
end;

end.
