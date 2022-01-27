unit dlg_server_upload;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  fhir_factory, fhir_client, fhir_package_upload;

type

  { TServerPackageUploadForm }

  TServerPackageUploadForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    CheckGroup1: TCheckGroup;
    CheckGroup2: TCheckGroup;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
  private
    FClient: TFHIRClientV;
    FFactory: TFHIRFactory;
    procedure SetClient(AValue: TFHIRClientV);
    procedure SetFactory(AValue: TFHIRFactory);

  public
    destructor Destroy; override;

    property client : TFHIRClientV read FClient write SetClient;
    property factory : TFHIRFactory read FFactory write SetFactory;
  end;

var
  ServerPackageUploadForm: TServerPackageUploadForm;

implementation

{$R *.lfm}

{ TServerPackageUploadForm }

procedure TServerPackageUploadForm.SetClient(AValue: TFHIRClientV);
begin
  FClient.free;
  FClient := AValue;
end;

procedure TServerPackageUploadForm.SetFactory(AValue: TFHIRFactory);
begin
  FFactory.free;
  FFactory := AValue;
end;

destructor TServerPackageUploadForm.Destroy;
begin
  FClient.free;
  FFactory.free;
  inherited Destroy;
end;

end.

