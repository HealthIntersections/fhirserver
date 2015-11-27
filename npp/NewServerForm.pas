unit NewServerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.ExtCtrls, StringSupport,
  Vcl.ComCtrls, Vcl.CheckLst;

type
  TRegisterServerForm = class(TNppForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Panel1: TPanel;
    btnOk: TButton;
    Button2: TButton;
    Panel2: TPanel;
    btnFetch: TButton;
    Label13: TLabel;
    edtToken: TEdit;
    edtAuthorize: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label10: TLabel;
    Bevel1: TBevel;
    Label9: TLabel;
    edtRedirect: TEdit;
    Label8: TLabel;
    Label7: TLabel;
    edtClientSecret: TEdit;
    Label6: TLabel;
    Label4: TLabel;
    edtClientId: TEdit;
    Label5: TLabel;
    Panel3: TPanel;
    edtName: TEdit;
    Label3: TLabel;
    Label1: TLabel;
    edtServer: TEdit;
    Label2: TLabel;
    Panel4: TPanel;
    CheckBox1: TCheckBox;
    Label14: TLabel;
    CheckListBox1: TCheckListBox;
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
