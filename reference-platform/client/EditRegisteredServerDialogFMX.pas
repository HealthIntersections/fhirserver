unit EditRegisteredServerDialogFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.ListBox, FMX.Edit, FMX.StdCtrls, FMX.TabControl,
  FMX.Controls.Presentation,
  StringSupport,
  FHIRBase, FHIRResources, FHIRClient, FHIRUtilities,
  SmartOnFhirUtilities;

type
  TEditRegisteredServerForm = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtName: TEdit;
    edtURL: TEdit;
    cbxFormat: TComboBox;
    Label5: TLabel;
    edtClientId: TEdit;
    Label6: TLabel;
    edtClientSecret: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Line1: TLine;
    Label11: TLabel;
    Label12: TLabel;
    edtAuthorize: TEdit;
    edtToken: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    btnFetchEndpoints: TButton;
    Label9: TLabel;
    edtRedirectPort: TEdit;
    btnCheckFormat: TButton;
    Label15: TLabel;
    Label16: TLabel;
    edtUsername: TEdit;
    edtPassword: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCheckFormatClick(Sender: TObject);
    procedure inputChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnFetchEndpointsClick(Sender: TObject);
  private
    FServer: TRegisteredFHIRServer;
    FCapabilityStatement: TFhirCapabilityStatement;
    procedure SetServer(const Value: TRegisteredFHIRServer);
    procedure SetCapabilityStatement(const Value: TFhirCapabilityStatement);
    procedure loadCapabilityStatement;
  public
    property Server : TRegisteredFHIRServer read FServer write SetServer;
    Property CapabilityStatement : TFhirCapabilityStatement read FCapabilityStatement write SetCapabilityStatement;
  end;

var
  EditRegisteredServerForm: TEditRegisteredServerForm;

implementation

{$R *.fmx}

procedure TEditRegisteredServerForm.btnCheckFormatClick(Sender: TObject);
begin
  if FCapabilityStatement = nil then
    loadCapabilityStatement;
  if FCapabilityStatement.formatList.hasCode('application/json+fhir') then
    cbxFormat.ItemIndex := 2
  else if FCapabilityStatement.formatList.hasCode('application/xml+fhir') then
    cbxFormat.ItemIndex := 1
  else if FCapabilityStatement.formatList.hasCode('application/fhir+json') then
    cbxFormat.ItemIndex := 2
  else if FCapabilityStatement.formatList.hasCode('application/fhir+xml') then
    cbxFormat.ItemIndex := 1
  else if FCapabilityStatement.formatList.hasCode('text/turtle') then
    cbxFormat.ItemIndex := 3
  else if FCapabilityStatement.formatList.hasCode('json') then
    cbxFormat.ItemIndex := 2
  else if FCapabilityStatement.formatList.hasCode('xml') then
    cbxFormat.ItemIndex := 1
  else if FCapabilityStatement.formatList.hasCode('turtle') then
    cbxFormat.ItemIndex := 3
  else
    ShowMessage('This end point doens''t have any compatible formats in it''s conformance statement');
end;

procedure TEditRegisteredServerForm.btnFetchEndpointsClick(Sender: TObject);
var
  authorize, token : String;
begin
  if FCapabilityStatement = nil then
    loadCapabilityStatement;
  if usesSmartOnFHIR(FCapabilityStatement, authorize, token) then
  begin
    edtAuthorize.Text := authorize;
    edtToken.Text := token;
  end
  else
    ShowMessage('This end point doesn''t support SMART on FHIR');
end;

procedure TEditRegisteredServerForm.btnOkClick(Sender: TObject);
begin
  server.name := edtName.Text;
  server.fhirEndpoint := edtURL.Text;
  server.format := TFHIRFormat(cbxFormat.ItemIndex);
  server.SmartOnFHIR := (edtClientId.Text <> '') or (edtClientSecret.Text <> '') or ((edtRedirectPort.Text <> '') and (edtRedirectPort.Text <> '0')) or (edtAuthorize.Text <> '') or (edtToken.Text <> '');
  server.username := edtUsername.Text;
  server.password := edtPassword.Text;
  if server.SmartOnFHIR then
  begin
    server.clientid := edtClientId.Text;
    if server.clientid = '' then
      raise Exception.Create('Client id is required');
    server.clientsecret := edtClientSecret.Text;
    if not StringIsCardinal16(edtRedirectPort.Text) then
      raise Exception.Create('Redirectport must be a valid port number');
    server.redirectport := StrToInt(edtRedirectPort.Text);
    server.authorizeEndpoint := edtAuthorize.Text;
    if server.authorizeEndpoint = '' then
      raise Exception.Create('Authorize end-point is required');
    server.tokenEndpoint := edtToken.Text;
    if server.tokenEndpoint = '' then
      raise Exception.Create('Token end-point is required');
  end;
  ModalResult := mrok;
end;

procedure TEditRegisteredServerForm.FormDestroy(Sender: TObject);
begin
  FServer.Free;
  FCapabilityStatement.Free;
end;

procedure TEditRegisteredServerForm.FormShow(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem1;
  edtName.Text := FServer.name;
  edtURL.Text := FServer.fhirEndpoint;
  cbxFormat.ItemIndex := ord(FServer.format);
  edtClientId.Text := FServer.clientid;
  edtClientSecret.Text := FServer.clientsecret;
  edtAuthorize.Text := FServer.authorizeEndpoint;
  edtToken.Text := FServer.tokenEndpoint;
  edtRedirectPort.Text := inttostr(FServer.redirectport);
  edtUsername.Text := server.username;
  edtPassword.Text := server.password;
end;

procedure TEditRegisteredServerForm.inputChange(Sender: TObject);
begin
  btnOk.Enabled := (edtName.Text <> '') and isAbsoluteUrl(edtURL.Text);
end;

procedure TEditRegisteredServerForm.loadCapabilityStatement;
var
  client : TFhirHTTPClient;
begin
  if not isAbsoluteUrl(edtUrl.Text) then
    raise Exception.Create('Plase supply a valid URL for the server');

  try
    client := TFhirHTTPClient.Create(nil, edtUrl.text, true);
    try
      client.timeout := 5000;
      client.allowR2 := true;
      FCapabilityStatement := client.conformance(false);
    finally
      client.Free;
    end;
  except
    client := TFhirHTTPClient.Create(nil, edtUrl.text, false);
    try
      client.timeout := 5000;
      client.allowR2 := true;
      FCapabilityStatement := client.conformance(false);
    finally
      client.Free;
    end;
  end;
end;

procedure TEditRegisteredServerForm.SetCapabilityStatement(const Value: TFhirCapabilityStatement);
begin
  FCapabilityStatement.Free;
  FCapabilityStatement := Value;
end;

procedure TEditRegisteredServerForm.SetServer(const Value: TRegisteredFHIRServer);
begin
  FServer.Free;
  FServer := Value;
end;

end.
