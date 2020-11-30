unit console_id_edit;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  server_config;

type
  { TEditIDForm }

  TEditIDForm = class(TForm)
    btnDBTest1: TBitBtn;
    btnDBTest3: TBitBtn;
    edtIdentity: TEdit;
    edtAppSecret: TEdit;
    edtAppid: TEdit;
    edtAPIKey: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    FId: TFHIRServerConfigSection;
    procedure SetId(AValue: TFHIRServerConfigSection);
  public
    property Id : TFHIRServerConfigSection read FId write SetId;
    procedure update;
  end;

var
  EditIDForm: TEditIDForm;

implementation

{$R *.lfm}

{ TEditIDForm }

procedure TEditIDForm.FormDestroy(Sender: TObject);
begin
  FId.Free;
end;

procedure TEditIDForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    update;
    CanClose := true;
  end;
end;

procedure TEditIDForm.SetId(AValue: TFHIRServerConfigSection);
begin
  FId.Free;
  FId := AValue;

  if FId <> nil then
  begin
    edtIdentity.text := Id.name;
    edtAppid.text := Id['app-id'].value;
    edtAppSecret.text := Id['app-secret'].value;
    edtAPIKey.text := Id['api-key'].value;
  end;
end;

procedure TEditIDForm.update;
begin
  Id.name := edtIdentity.text;
  Id['app-id'].value := edtAppid.Text;
  Id['app-secret'].value := edtAppSecret.Text;
  Id['api-key'].value := edtAPIKey.Text;
end;

end.

