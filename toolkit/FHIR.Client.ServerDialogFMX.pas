unit FHIR.Client.ServerDialogFMX;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.ListBox, FMX.Edit, FMX.StdCtrls, FMX.TabControl,
  FMX.Controls.Presentation,
  fsl_utilities, FHIR.Ui.Fmx,
  fhir_objects, fhir_factory, fhir_common, 
  FHIR.Version.Constants,
  fhir_client, fhir_oauth, FHIR.Client.ClientDialogFMX;

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
    Line1: TLine;
    Label11: TLabel;
    Label12: TLabel;
    edtAuthorize: TEdit;
    edtToken: TEdit;
    Label13: TLabel;
    btnFetchEndpoints: TButton;
    btnCheckFormat: TButton;
    Label15: TLabel;
    Label16: TLabel;
    edtUsername: TEdit;
    edtPassword: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    TabControl2: TTabControl;
    TabItem4: TTabItem;
    Label5: TLabel;
    edtClientId: TEdit;
    Label6: TLabel;
    edtClientSecret: TEdit;
    Label7: TLabel;
    Label9: TLabel;
    edtRedirectPort: TEdit;
    Label10: TLabel;
    Label8: TLabel;
    Label14: TLabel;
    cbxSmartType: TComboBox;
    TabItem5: TTabItem;
    TabItem6: TTabItem;
    Label19: TLabel;
    edtIssuerURL: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    edtPrivateKey: TEdit;
    Button1: TButton;
    edtPassphrase: TEdit;
    Label22: TLabel;
    Label23: TLabel;
    edtClientId1: TEdit;
    btnRegister: TButton;
    odPrivateKey: TOpenDialog;
    Label24: TLabel;
    Label25: TLabel;
    edtSSLPublicCert: TEdit;
    Label26: TLabel;
    edtSSLPrivateKey: TEdit;
    Label27: TLabel;
    Label28: TLabel;
    edtSSLPassphrase: TEdit;
    Button2: TButton;
    Button3: TButton;
    odPublicCert: TOpenDialog;
    Label29: TLabel;
    cbxVersion: TComboBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCheckFormatClick(Sender: TObject);
    procedure inputChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnFetchEndpointsClick(Sender: TObject);
    procedure cbxSmartTypeChange(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FServer: TRegisteredFHIRServer;
    FCapabilityStatement: TFhirCapabilityStatementW;
    FRegister : String;
    FSoftwareVersion: String;
    FSoftwareId: String;
    FVersions: TFHIRVersionFactories;
    FDefaultVersion: TFHIRVersion;
    procedure SetServer(const Value: TRegisteredFHIRServer);
    procedure SetCapabilityStatement(const Value: TFhirCapabilityStatementW);
    procedure loadCapabilityStatement;
    procedure SetVersions(const Value: TFHIRVersionFactories);
    function readVersion : TFHIRVersion;
  public
    property Server : TRegisteredFHIRServer read FServer write SetServer;
    Property CapabilityStatement : TFhirCapabilityStatementW read FCapabilityStatement write SetCapabilityStatement;
    property SoftwareId : String read FSoftwareId write FSoftwareId;
    property SoftwareVersion : String read FSoftwareVersion write FSoftwareVersion;
    property Versions : TFHIRVersionFactories read FVersions write SetVersions;
    property DefaultVersion : TFHIRVersion read FDefaultVersion write FDefaultVersion;
  end;

var
  EditRegisteredServerForm: TEditRegisteredServerForm;

implementation

{$R *.fmx}

procedure TEditRegisteredServerForm.btnCheckFormatClick(Sender: TObject);
begin
  if FCapabilityStatement = nil then
    loadCapabilityStatement;
  if FCapabilityStatement.hasFormat('application/json+fhir') then
    cbxFormat.ItemIndex := 2
  else if FCapabilityStatement.hasFormat('application/xml+fhir') then
    cbxFormat.ItemIndex := 1
  else if FCapabilityStatement.hasFormat('application/fhir+json') then
    cbxFormat.ItemIndex := 2
  else if FCapabilityStatement.hasFormat('application/fhir+xml') then
    cbxFormat.ItemIndex := 1
  else if FCapabilityStatement.hasFormat('text/turtle') then
    cbxFormat.ItemIndex := 3
  else if FCapabilityStatement.hasFormat('json') then
    cbxFormat.ItemIndex := 2
  else if FCapabilityStatement.hasFormat('xml') then
    cbxFormat.ItemIndex := 1
  else if FCapabilityStatement.hasFormat('turtle') then
    cbxFormat.ItemIndex := 3
  else
    ShowMessage('This end point doens''t have any compatible formats in it''s conformance statement');
end;

procedure TEditRegisteredServerForm.btnFetchEndpointsClick(Sender: TObject);
var
  authorize, token : String;
begin
  btnRegister.Enabled := false;
  if FCapabilityStatement = nil then
    loadCapabilityStatement;
  if usesSmartOnFHIR(FCapabilityStatement, authorize, token, FRegister) then
  begin
    edtAuthorize.Text := authorize;
    edtToken.Text := token;
    btnRegister.Enabled := FRegister <> '';
  end
  else
    ShowMessage('This end point doesn''t support SMART on FHIR');
end;

procedure TEditRegisteredServerForm.btnOkClick(Sender: TObject);
begin
  server.name := edtName.Text;
  server.fhirEndpoint := edtURL.Text;
  server.format := TFHIRFormat(cbxFormat.ItemIndex);
  if cbxVersion.ItemIndex = -1 then
    server.version := currentFHIRVersionRelease
  else if cbxVersion.ItemIndex = 0 then
    server.version := DefaultVersion
  else
    server.version := (cbxVersion.ListItems[cbxVersion.ItemIndex].Data as TFHIRFactory).version;

  server.SmartAppLaunchMode := TSmartAppLaunchMode(cbxSmartType.ItemIndex);
  server.username := edtUsername.Text;
  server.password := edtPassword.Text;
  server.authorizeEndpoint := edtAuthorize.Text;
  server.tokenEndpoint := edtToken.Text;
  server.clientsecret := edtClientSecret.Text;
  server.redirectport := StrToInt(edtRedirectPort.Text);
  server.SSLPublicCert := edtSSLPublicCert.Text;
  server.SSLPrivateKey := edtSSLPrivateKey.Text;
  server.SSLPassphrase := edtSSLPassphrase.Text;

  server.issuerUrl := edtIssuerURL.Text;
  server.privatekey := edtPrivateKey.Text;
  server.passphrase := edtPassphrase.Text;
  if server.SmartAppLaunchMode = salmOAuthClient then
  begin
    server.clientid := edtClientId.Text;
    if server.clientid = '' then
      raise EFHIRException.create('Client id is required');
    if not StringIsCardinal16(edtRedirectPort.Text) then
      raise EFHIRException.create('Redirectport must be a valid port number');
    if server.authorizeEndpoint = '' then
      raise EFHIRException.create('Authorize end-point is required');
    if server.tokenEndpoint = '' then
      raise EFHIRException.create('Token end-point is required');
  end
  else if server.SmartAppLaunchMode = salmBackendClient then
  begin
    server.clientid := edtClientId1.Text;
    if server.issuerUrl = '' then
      raise EFHIRException.create('Issuer URL is required');
    if not FileExists(server.privatekey) then
      raise EFHIRException.create('Private Key is required (file not found)');
    if server.authorizeEndpoint = '' then
      raise EFHIRException.create('Authorize end-point is required');
    if server.tokenEndpoint = '' then
      raise EFHIRException.create('Token end-point is required');
  end;
  ModalResult := mrok;
end;

procedure TEditRegisteredServerForm.btnRegisterClick(Sender: TObject);
var
  form : TRegisterClientForm;
begin
  form := TRegisterClientForm.Create(self);
  try
    form.Mode := TSmartAppLaunchMode(cbxSmartType.ItemIndex);
    form.Port := StrToIntDef(edtRedirectPort.Text, 0);
    form.Server := FRegister;
    form.SoftwareId := SoftwareId;
    form.SoftwareVersion := SoftwareVersion;
    form.edtIssuer.Text := edtIssuerURL.Text;
    if ShowModalHack(form) = mrOk then
    begin
      if form.Mode = salmOAuthClient then
      begin
        edtClientId.Text := form.ClientId;
        edtClientSecret.Text := form.ClientSecret;
      end
      else if form.Mode = salmBackendClient then
      begin
        edtClientId1.Text := form.ClientId;
        edtIssuerURL.Text := form.edtIssuer.Text;
      end;
    end;
  finally
    form.Free;
  end;
end;

procedure TEditRegisteredServerForm.Button1Click(Sender: TObject);
begin
  if odPrivateKey.Execute then
    edtPrivateKey.Text := odPrivateKey.FileName;
end;

procedure TEditRegisteredServerForm.Button2Click(Sender: TObject);
begin
  if odPublicCert.Execute then
    edtSSLPublicCert.Text := odPublicCert.FileName;
end;

procedure TEditRegisteredServerForm.Button3Click(Sender: TObject);
begin
  if odPrivateKey.Execute then
    edtSSLPrivateKey.Text := odPrivateKey.FileName;
end;

procedure TEditRegisteredServerForm.cbxSmartTypeChange(Sender: TObject);
begin
  TabControl2.TabIndex := cbxSmartType.ItemIndex;
end;

procedure TEditRegisteredServerForm.FormDestroy(Sender: TObject);
begin
  FServer.Free;
  FVersions.Free;
  FCapabilityStatement.Free;
end;

procedure TEditRegisteredServerForm.FormShow(Sender: TObject);
var
  v : TFHIRVersion;
  i :  integer;
begin
  cbxVersion.Items.Clear;
  i := -1;
  for v := Low(TFHIRVersion) to High(TFHIRVersion) do
    if FVersions.hasVersion[v] then
    begin
      cbxVersion.Items.Add(FVersions[v].description);
      cbxVersion.ListItems[cbxVersion.Items.Count - 1].Data := FVersions[v];
      if FVersions[v].version = server.version then
        i := cbxVersion.Items.Count - 1;
    end;
  if i = -1 then // not supported in this version
    i := cbxVersion.Items.Count - 1;
  cbxVersion.ItemIndex := i;

  TabControl1.ActiveTab := TabItem1;
  edtName.Text := FServer.name;
  edtURL.Text := FServer.fhirEndpoint;
  cbxFormat.ItemIndex := ord(FServer.format);
  cbxSmartType.ItemIndex := ord(FServer.SmartAppLaunchMode);
  edtClientId.Text := FServer.clientid;
  edtClientId1.Text := FServer.clientid;
  edtClientSecret.Text := FServer.clientsecret;
  edtRedirectPort.Text := inttostr(FServer.redirectport);
  edtIssuerURL.Text := FServer.issuerUrl;
  edtPrivateKey.Text := FServer.privatekey;
  edtPassphrase.Text := FServer.passphrase;
  edtAuthorize.Text := FServer.authorizeEndpoint;
  edtToken.Text := FServer.tokenEndpoint;
  edtUsername.Text := server.username;
  edtPassword.Text := server.password;
  edtSSLPublicCert.Text := server.SSLPublicCert;
  edtSSLPrivateKey.Text := server.SSLPrivateKey;
  edtSSLPassphrase.Text := server.SSLPassphrase;
end;

procedure TEditRegisteredServerForm.inputChange(Sender: TObject);
begin
  btnOk.Enabled := (edtName.Text <> '') and isAbsoluteUrl(edtURL.Text);
end;

procedure TEditRegisteredServerForm.loadCapabilityStatement;
var
  client : TFhirClientV;
begin
  if not isAbsoluteUrl(edtUrl.Text) then
    raise EFHIRException.create('Plase supply a valid URL for the server');

  try
    client := FVersions[readVersion].makeClient(nil, edtUrl.text, fctCrossPlatform, ffJson, 5000);
    try
      FCapabilityStatement := FVersions[readVersion].wrapCapabilityStatement(client.conformanceV(false));
    finally
      client.Free;
    end;
  except
    client := FVersions[readVersion].makeClient(nil, edtUrl.text, fctCrossPlatform, ffXml, 5000);
    try
      FCapabilityStatement := FVersions[readVersion].wrapCapabilityStatement(client.conformanceV(false));
    finally
      client.Free;
    end;
  end;
end;

function TEditRegisteredServerForm.readVersion: TFHIRVersion;
begin
  if cbxVersion.ItemIndex = -1 then
    result := fhirVersionRelease4
  else
    result := TFHIRFactory(cbxVersion.ListItems[cbxVersion.ItemIndex].Data).version;
end;

procedure TEditRegisteredServerForm.SetCapabilityStatement(const Value: TFhirCapabilityStatementW);
begin
  FCapabilityStatement.Free;
  FCapabilityStatement := Value;
end;

procedure TEditRegisteredServerForm.SetServer(const Value: TRegisteredFHIRServer);
begin
  FServer.Free;
  FServer := Value;
end;

procedure TEditRegisteredServerForm.SetVersions(const Value: TFHIRVersionFactories);
begin
  FVersions.Free;
  FVersions := Value;
end;

end.
