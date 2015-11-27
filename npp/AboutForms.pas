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
    Button2: TButton;
    Button3: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FServices : TValidatorServiceProvider;
    procedure SetServices(const Value: TValidatorServiceProvider);
  public
    { Public declarations }
    property Services : TValidatorServiceProvider read FServices write SetServices;
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

uses
  FHIRConstants,
  FHIRPlugin,
  FHIRPath;

procedure TAboutForm.Button2Click(Sender: TObject);
begin
  FNpp.DoOpen(TFHIRPathTests.runTests(Services));
  MessageDlg('all tests passed', mtInformation, [mbOk], 0);
end;

procedure TAboutForm.Button3Click(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', 'http://wiki.hl7.org/index.php?title=FHIR_Notepad%2B%2B_Plugin_Documentation', '', '', SW_SHOWNORMAL);
  inherited;

end;

procedure TAboutForm.FormDestroy(Sender: TObject);
begin
  inherited;
  FServices.free;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  inherited;
  lblVersion.Caption := 'FHIR Version '+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION;
end;

procedure TAboutForm.SetServices(const Value: TValidatorServiceProvider);
begin
  FServices.Free;
  FServices := Value;
end;

end.
