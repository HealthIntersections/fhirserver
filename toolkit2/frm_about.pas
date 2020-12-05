unit frm_about;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fsl_utilities,
  ftk_version;

type

  { TToolkitAboutForm }

  TToolkitAboutForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    lblVersion: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblAge: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  ToolkitAboutForm: TToolkitAboutForm;

implementation

{$R *.lfm}

{ TToolkitAboutForm }

procedure TToolkitAboutForm.Image1Click(Sender: TObject);
begin

end;

procedure TToolkitAboutForm.FormShow(Sender: TObject);
begin
  lblVersion.Caption := 'Version '+TOOLKIT_VERSION;
  if TOOLKIT_RELEASE_DATE = '' then
    lblAge.Caption := 'Development Version'
  else
    lblAge.Caption := 'Released '+DescribePeriod(now - TFslDateTime.fromHL7(TOOLKIT_RELEASE_DATE).DateTime)+'Ago';
end;

end.

