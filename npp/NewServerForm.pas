unit NewServerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.ExtCtrls, StringSupport;

type
  TRegisterServerForm = class(TNppForm)
    btnOk: TButton;
    Button2: TButton;
    Label1: TLabel;
    edtServer: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtName: TEdit;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    edtClientId: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    edtClientSecret: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    edtRedirect: TEdit;
    Label9: TLabel;
    Bevel1: TBevel;
    Label10: TLabel;
    Label11: TLabel;
    edtAuthorize: TEdit;
    Label12: TLabel;
    edtToken: TEdit;
    btnFetch: TButton;
    Label13: TLabel;
    procedure edtNameChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnFetchClick(Sender: TObject);
  private
    { Private declarations }
    FIndex : integer;
  public
    { Public declarations }
    procedure LoadFrom(i : integer);
  end;

var
  RegisterServerForm: TRegisterServerForm;

implementation

{$R *.dfm}

uses
  FHIRPluginSettings, FHIRClient, FHIRResources, SmartOnFhirUtilities;

procedure TRegisterServerForm.btnFetchClick(Sender: TObject);
var
  client : TFhirClient;
  conf : TFhirConformance;
  authorize, token : String;
begin
  client := TFhirClient.Create(edtServer.text, true);
  try
    conf := client.conformance(false);
    try
      if usesSmartOnFHIR(conf, authorize, token) then
      begin
        edtAuthorize.Text := authorize;
        edtToken.Text := token;
      end
      else
        ShowMessage('This end point doesn''t support SMART on FHIR');
    finally
      conf.Free;
    end;
  finally
    client.Free;
  end;
end;

procedure TRegisterServerForm.btnOkClick(Sender: TObject);
var
  server : TRegisteredServer;
begin
  server.name := edtName.Text;
  server.SmartOnFHIR := edtAuthorize.Text <> '';
  server.fhirEndpoint := edtServer.Text;
  server.tokenEndpoint := edtToken.Text;
  server.authorizeEndpoint := edtAuthorize.Text;
  server.clientid := edtClientId.Text;
  server.clientsecret := edtClientSecret.Text;
  server.redirectport := StrToIntDef(edtRedirect.Text, 0);
  Settings.registerServer(server);
end;

procedure TRegisterServerForm.edtNameChange(Sender: TObject);
begin
  if (edtAuthorize.Text <> '') then
    btnOk.Enabled := (edtName.text <> '') and (edtServer.text <> '') and (edtAuthorize.Text <> '') and (edtToken.Text <> '') and (edtClientId.Text <> '') and StringIsInteger16(edtRedirect.Text)
  else
    btnOk.Enabled := (edtName.text <> '') and (edtServer.text <> '') and (edtAuthorize.Text = '') and (edtToken.Text = '') and (edtClientId.Text = '') and (edtRedirect.Text = '');
  btnOk.Enabled := edtServer.text <> '';
end;

procedure TRegisterServerForm.LoadFrom(i: integer);
var
  server : TRegisteredServer;
begin
  Caption := 'Edit Server';
  FIndex := i;
  server := settings.serverInfo(FIndex);
  edtName.Text := server.name;
  edtServer.Text := server.fhirEndpoint;
  if server.SmartOnFHIR then
  begin
    edtToken.Text := server.tokenEndpoint;
    edtAuthorize.Text := server.authorizeEndpoint;
    edtClientId.Text := server.clientid;
    edtClientSecret.Text := server.clientsecret;
    edtRedirect.Text := IntToStr(server.redirectport);
  end
  else
  begin
    edtToken.Text := '';
    edtAuthorize.Text := '';
    edtClientId.Text := '';
    edtClientSecret.Text := '';
    edtRedirect.Text := '';
  end;
end;

end.
