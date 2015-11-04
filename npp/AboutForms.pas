unit AboutForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

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
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

uses
  FHIRConstants;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  inherited;
  lblVersion.Caption := 'FHIR Version '+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION;
end;

end.
