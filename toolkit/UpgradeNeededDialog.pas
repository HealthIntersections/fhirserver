unit UpgradeNeededDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, OSXUiUtils, ToolkitSettings;

type
  TUpgradeNeededForm = class(TForm)
    lblVersion: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    procedure Label2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSettings: TFHIRToolkitSettings;
    procedure SetSettings(const Value: TFHIRToolkitSettings);
  public
    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
  end;

var
  UpgradeNeededForm: TUpgradeNeededForm;

implementation

{$R *.fmx}

procedure TUpgradeNeededForm.FormDestroy(Sender: TObject);
begin
  Settings.CheckForUpgradesOnStart := CheckBox1.IsChecked;
  FSettings.Free;
end;

procedure TUpgradeNeededForm.FormShow(Sender: TObject);
begin
  CheckBox1.IsChecked := Settings.CheckForUpgradesOnStart;
end;

procedure TUpgradeNeededForm.Label2Click(Sender: TObject);
begin
  OpenURL('http://www.healthintersections.com.au/FhirServer/toolkit.htm');
end;

procedure TUpgradeNeededForm.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

end.
