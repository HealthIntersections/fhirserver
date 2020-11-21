unit console_id_edit;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  server_ini;

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
    FId: TFHIRServerIniComplex;
    procedure SetId(AValue: TFHIRServerIniComplex);
  public
    property Id : TFHIRServerIniComplex read FId write SetId;
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

procedure TEditIDForm.SetId(AValue: TFHIRServerIniComplex);
begin
  FId.Free;
  FId := AValue;

  if FId <> nil then
  begin
    edtIdentity.text := Id['id'];
    edtAppid.text := Id['app-id'];
    edtAppSecret.text := Id['app-secret'];
    edtAPIKey.text := Id['api-key'];
  end;
end;

procedure TEditIDForm.update;
begin
  Id['id'] := edtIdentity.text;
  Id['app-id'] := edtAppid.Text;
  Id['app-secret'] := edtAppSecret.Text;
  Id['api-key'] := edtAPIKey.Text;
end;

end.

