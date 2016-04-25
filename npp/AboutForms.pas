unit AboutForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls, ShellApi,
  FHIRProfileUtilities;

type
  TAboutForm = class(TNppForm)
    Button1: TButton;
    Panel1: TPanel;
    Image1: TImage;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblVersion: TLabel;
    Label5: TLabel;
    GroupBox1: TGroupBox;
    lnkDoco: TLabel;
    lnkIssue: TLabel;
    lnkSpec: TLabel;
    lnkUpdates: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lnkDocoClick(Sender: TObject);
    procedure lnkIssueClick(Sender: TObject);
    procedure lnkSpecClick(Sender: TObject);
    procedure lnkUpdatesClick(Sender: TObject);
  private
    { Private declarations }
    FServices : TWorkerContext;
    procedure SetServices(const Value: TWorkerContext);
  public
    { Public declarations }
    property Services : TWorkerContext read FServices write SetServices;
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

uses
  FHIRConstants,
  FHIRPlugin,
  FHIRPath,
  nppbuildcount;

procedure TAboutForm.FormDestroy(Sender: TObject);
begin
  inherited;
  FServices.free;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  inherited;
  lblVersion.Caption := 'Plugin Version 1.0.'+inttostr(buildcount)+', FHIR Version '+FHIR_GENERATED_VERSION
end;

procedure TAboutForm.lnkDocoClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', 'http://wiki.hl7.org/index.php?title=FHIR_Notepad%2B%2B_Plugin_Documentation', '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.lnkIssueClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', 'https://github.com/grahamegrieve/fhirserver/issues', '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.lnkSpecClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', 'http://hl7.org/fhir/index.html', '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.lnkUpdatesClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', 'http://www.healthintersections.com.au/FhirServer/fhirnpp.htm', '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.SetServices(const Value: TWorkerContext);
begin
  FServices.Free;
  FServices := Value;
end;

end.
