unit AboutDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.Controls.Presentation,
  OSXUIUtils,
  FHIRConstants,
  toolkitversion;

type
  TAboutForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblVersion: TLabel;
    Label4: TLabel;
    lblDoco: TLabel;
    lblIssue: TLabel;
    lblSpec: TLabel;
    lblUpdates: TLabel;
    procedure lblUpdatesClick(Sender: TObject);
    procedure lblSpecClick(Sender: TObject);
    procedure lblDocoClick(Sender: TObject);
    procedure lblIssueClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.fmx}

procedure TAboutForm.FormShow(Sender: TObject);
begin
  lblVersion.Text := 'Toolkit Version 0.0.'+inttostr(BuildCount)+', FHIR Version '+FHIR_GENERATED_VERSION;
end;

procedure TAboutForm.lblDocoClick(Sender: TObject);
begin
  OpenURL('http://wiki.hl7.org/index.php?title=FHIR_Toolkit_Documentation');
end;

procedure TAboutForm.lblIssueClick(Sender: TObject);
begin
  OpenURL('https://github.com/grahamegrieve/fhirserver/issues');
end;

procedure TAboutForm.lblSpecClick(Sender: TObject);
begin
  OpenURL('http://www.hl7.org/fhir');
end;

procedure TAboutForm.lblUpdatesClick(Sender: TObject);
begin
  OpenURL('http://www.healthintersections.com.au/FhirServer/toolkit.htm');
end;

end.
