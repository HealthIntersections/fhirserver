unit ResourceContributorDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.ImageList, FMX.ImgList, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  FMX.DateTimeCtrls, FMX.StdCtrls, FMX.Controls.Presentation,
  FHIR.Support.Utilities,
  FHIR.Version.Types, FHIR.Version.Utilities;

type
  TResourceContributorForm = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    Button2: TButton;
    btnAsChild: TButton;
    lblId: TLabel;
    LblAuthor: TLabel;
    lblNotes: TLabel;
    edtName: TEdit;
    edtRole: TEdit;
    memNotes: TMemo;
    procedure FormShow(Sender: TObject);
    procedure edtDateChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    FExtension : TFhirExtension;
    procedure SetExtension(const Value: TFhirExtension);
  public
    destructor Destroy; override;
    property Extension : TFhirExtension read FExtension write SetExtension;
  end;

var
  ResourceContributorForm: TResourceContributorForm;

implementation

{$R *.fmx}

{ TForm1 }


procedure TResourceContributorForm.btnOkClick(Sender: TObject);
begin
  FExtension.setExtensionString('name', edtName.Text);
  FExtension.setExtensionString('role', edtRole.Text);
  FExtension.setExtensionString('notes', memNotes.Text);
end;

destructor TResourceContributorForm.Destroy;
begin
  FExtension.Free;
  inherited;
end;

procedure TResourceContributorForm.edtDateChange(Sender: TObject);
begin
  btnOk.Enabled := (edtName.Text <> '') and (edtRole.Text <> '');
end;

procedure TResourceContributorForm.FormShow(Sender: TObject);
begin
  edtName.Text := FExtension.getExtensionString('name');
  edtRole.Text := FExtension.getExtensionString('role');
  memNotes.Text := FExtension.getExtensionString('notes');
end;

procedure TResourceContributorForm.SetExtension(const Value: TFhirExtension);
begin
  FExtension.Free;
  FExtension := Value;
end;

end.
