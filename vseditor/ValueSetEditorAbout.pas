unit ValueSetEditorAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls;

type
  TValueSetEditorAboutForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblFHIRVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lblFHIRVersionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ValueSetEditorAboutForm: TValueSetEditorAboutForm;

implementation

{$R *.dfm}

uses
  FHIRConstants;

procedure TValueSetEditorAboutForm.FormCreate(Sender: TObject);
begin
  lblFHIRVersion.Caption := 'FHIR Version: '+ FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION;
end;

procedure TValueSetEditorAboutForm.lblFHIRVersionClick(Sender: TObject);
begin
  close;
end;

end.
