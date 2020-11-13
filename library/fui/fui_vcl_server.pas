unit FHIR.Client.ServerDialog;


{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.CheckLst,
  FHIR.Npp.Form,
  fsl_utilities, fsl_base,
  fhir_objects, fhir_factory, fhir_common, 
  fhir_cdshooks, fhir_oauth;

type
  TEditRegisteredServerForm = class(TNppForm)
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
    Panel3: TPanel;
    edtName: TEdit;
    Label3: TLabel;
    Label1: TLabel;
    edtServer: TEdit;
    Label2: TLabel;
    Panel4: TPanel;
    CheckBox1: TCheckBox;
    Label14: TLabel;
    clHooks: TCheckListBox;
    Button1: TButton;
    Formt: TLabel;
    cbxFormat: TComboBox;
    Button3: TButton;
    Notebook1: TNotebook;
    Label15: TLabel;
    cbxSmartMode: TComboBox;
    Notebook2: TNotebook;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edtClientId: TEdit;
    edtClientSecret: TEdit;
    edtRedirect: TEdit;
    Label9: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    edtIssuerURL: TEdit;
    edtPrivateKey: TEdit;
    edtPassphrase: TEdit;
    Label19: TLabel;
    edtClientId1: TEdit;
    Label20: TLabel;
    cbxVersion: TComboBox;
    Button4: TButton;
    procedure edtNameChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnFetchClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure cbxSmartModeChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FIndex : integer;
    FCapabilityStatement : TFhirCapabilityStatementW;
    FServer: TRegisteredFHIRServer;
    FVersions: TFHIRVersionFactories;
    FAdding : boolean;
    FSoftwareVersion: String;
    FSoftwareId: String;
    FRegister : String;

    procedure loadCapabilityStatement;
    function readServerVersion : TFHIRVersion;
//    function hookIndex(c : TFslObject) : integer;
    procedure listHooks(list : TFslList<TRegisteredCDSHook>);
    procedure loadHooks;
//    procedure readExtension(ext: TFHIRExtensionW; preFetch: TStringList; var name: String; var c: String);
    procedure SetServer(const Value: TRegisteredFHIRServer);
//    procedure readExtension(ext: TFHIRExtension; preFetch: TStringList;
//      var name, c: String);
    procedure SetVersions(const Value: TFHIRVersionFactories);
  public
    procedure Load;

    property versions : TFHIRVersionFactories read FVersions write SetVersions;
    property server : TRegisteredFHIRServer read FServer write SetServer;
    property SoftwareId : String read FSoftwareId write FSoftwareId;
    property SoftwareVersion : String read FSoftwareVersion write FSoftwareVersion;

  end;

var
  EditRegisteredServerForm: TEditRegisteredServerForm;

implementation

{$R *.dfm}

uses
  FHIR.Npp.Settings, fhir_client, FHIR.Client.ClientDialog;

procedure TEditRegisteredServerForm.btnFetchClick(Sender: TObject);
var
  authorize, token, register : String;
begin
  if FCapabilityStatement = nil then
    loadCapabilityStatement;
  if usesSmartOnFHIR(FCapabilityStatement, authorize, token, register) then
  begin
    edtAuthorize.Text := authorize;
    edtToken.Text := token;
    FRegister := register;
  end
  else
    ShowMessage('This end point doesn''t support SMART on FHIR');
end;

procedure TEditRegisteredServerForm.listHooks(list : TFslList<TRegisteredCDSHook>);
var
  i : integer;
  cds : TRegisteredCDSHook;
  name : String;
  c: String;
begin
  list.Clear;
  for i := 0 to clHooks.Items.Count - 1 do
    if clHooks.Checked[i] and (clHooks.items.Objects[i] <> nil) then
    begin
      cds := TRegisteredCDSHook.Create;
      try
//        readExtension(TFHIRExtension(clHooks.items.Objects[i]), cds.preFetch, name, c);
        cds.name := name;
        cds.hook := c;
        list.Add(cds.link);
      finally
        cds.Free;
      end;
    end;
end;

procedure TEditRegisteredServerForm.loadCapabilityStatement;
var
  client : TFhirClientV;
  msg : String;
begin
  try
    try
      clHooks.items.Clear;
      client := FVersions[readServerVersion].makeClient(nil, edtServer.text, fctWinInet, ffJson, 5000);
      try
        FCapabilityStatement := FVersions[readServerVersion].wrapCapabilityStatement(client.conformanceV(false));
      finally
        client.Free;
      end;
      // loadHooks;
    except
      on e : exception do
      begin
        msg := e.Message;
        client := FVersions[readServerVersion].makeClient(nil, edtServer.text, fctWinInet, ffXml, 5000);
        try
          FCapabilityStatement := FVersions[readServerVersion].wrapCapabilityStatement(client.conformanceV(false));
        finally
          client.Free;
        end;
      end;
    end;
  except
    on e : Exception do
      raise EFHIRException.create('Error reading Server conformance statement: "'+msg+'" and "'+e.message+'" - tried both xml and json for '+FVersions[readServerVersion].description);
  end;
end;

procedure TEditRegisteredServerForm.btnOkClick(Sender: TObject);
var
  server : TRegisteredFHIRServer;
begin
  server := TRegisteredFHIRServer.Create;
  try
    server.name := edtName.Text;
    server.SmartAppLaunchMode := TSmartAppLaunchMode(cbxSmartMode.ItemIndex);
    server.fhirEndpoint := edtServer.Text;
    server.format := TFHIRFormat(cbxFormat.ItemIndex);
    server.version := readServerVersion;
    server.tokenEndpoint := edtToken.Text;
    server.authorizeEndpoint := edtAuthorize.Text;
    if cbxSmartMode.itemIndex = 2 then
      server.clientid := edtClientId1.Text
    else
      server.clientid := edtClientId.Text;
    server.clientsecret := edtClientSecret.Text;
    server.redirectport := StrToIntDef(edtRedirect.Text, 0);
    server.issuerUrl := edtIssuerURL.Text;
    server.privatekey := edtPrivateKey.Text;
    server.passphrase := edtPassphrase.Text;
    listHooks(server.cdshooks);
    if FAdding then
      Settings.registerServer('', server)
    else
      Settings.updateServerInfo('', server);
  finally
    server.Free;
  end;
end;

procedure TEditRegisteredServerForm.Button1Click(Sender: TObject);
//var
//  ext : TFHIRExtension;
//  i : integer;
begin
  raise EFHIRTodo.create('TEditRegisteredServerForm.Button1Click');
//  if FCapabilityStatement = nil then
//    loadCapabilityStatement;
//  for i := 0 to clHooks.Items.Count - 1 do
//    clHooks.Checked[i] := false;
//  for ext in FCapabilityStatement.extensionList do
//    if ext.url = 'http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity' then
//    begin
//      i := hookIndex(ext.value as TFHIRCoding);
//      if i > -1 then
//        clHooks.Checked[i] := true;
//    end;
end;

procedure TEditRegisteredServerForm.Button3Click(Sender: TObject);
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
  else if FCapabilityStatement.hasFormat('json') then
    cbxFormat.ItemIndex := 2
  else if FCapabilityStatement.hasFormat('xml') then
    cbxFormat.ItemIndex := 1
  else
    ShowMessage('This end point doens''t have any compatible formats in it''s conformance statement');
end;

procedure TEditRegisteredServerForm.Button4Click(Sender: TObject);
var
  form : TRegisterClientForm;
begin
  if FRegister = '' then
    btnFetchClick(nil);
  form := TRegisterClientForm.Create(self);
  try
    form.Mode := TSmartAppLaunchMode(cbxSmartMode.ItemIndex);
    form.Port := StrToIntDef(edtRedirect.Text, 0);
    form.Server := FRegister;
    form.SoftwareId := SoftwareId;
    form.SoftwareVersion := SoftwareVersion;
//    form.edtIssuer.Text := edtIssuerURL.Text;
    if form.ShowModal = mrOk then
    begin
      if form.Mode = salmOAuthClient then
      begin
        edtClientId.Text := form.ClientId;
        edtClientSecret.Text := form.ClientSecret;
      end
      else if form.Mode = salmBackendClient then
      begin
        edtClientId1.Text := form.ClientId;
//        edtIssuerURL.Text := form.edtIssuer.Text;
      end;
    end;
  finally
    form.Free;
  end;
end;

procedure TEditRegisteredServerForm.cbxSmartModeChange(Sender: TObject);
begin
  Notebook1.PageIndex := cbxSmartMode.ItemIndex;
end;

procedure TEditRegisteredServerForm.edtNameChange(Sender: TObject);
begin
//  case cbxSmartMode.itemIndex of
//    0: btnOk.Enabled := (edtName.text <> '') and (edtServer.text <> '');
//    1: btnOk.Enabled := (edtName.text <> '') and (edtServer.text <> '') and (edtAuthorize.Text <> '') and (edtToken.Text <> '') and (edtClientId.Text <> '') and StringIsInteger16(edtRedirect.Text);
//    2: btnOk.Enabled := (edtName.text <> '') and (edtServer.text <> '') and (edtAuthorize.Text <> '') and (edtToken.Text <> '') and (edtClientId1.Text <> '') and (edtIssuerURL.Text <> '');
//  end;
//  if (edtAuthorize.Text <> '') then
//    btnOk.Enabled :=
//  else

  btnOk.Enabled := edtServer.text <> '';
end;

procedure TEditRegisteredServerForm.FormCreate(Sender: TObject);
begin
  FIndex := -1;
  inherited;
end;

procedure TEditRegisteredServerForm.FormDestroy(Sender: TObject);
begin
  FVersions.Free;
  FCapabilityStatement.Free;
  FServer.Free;
  inherited;
end;

procedure TEditRegisteredServerForm.FormShow(Sender: TObject);
begin
  if FIndex = -1 then
    loadHooks;
end;


(*
procedure TEditRegisteredServerForm.readExtension(ext : TFHIRExtension; preFetch : TStringList; var name : String; var c : String);
var
  iext : TFhirExtension;
begin
  for iext in ext.extensionList do
    if iext.url = 'name' then
      name := (iext.value as TFhirPrimitiveType).primitiveValue
   else if iext.url = 'activity' then
      c := (iext.value as TFhirString).StringValue
   else if iext.url = 'preFetchMandatory' then
     if preFetch <> nil then
       preFetch.add(TFHIRPrimitiveType(iext.value).primitiveValue);
end;
*)

procedure TEditRegisteredServerForm.SetServer(const Value: TRegisteredFHIRServer);
begin
  FServer := Value;
  load;
end;

procedure TEditRegisteredServerForm.SetVersions(const Value: TFHIRVersionFactories);
begin
  FVersions.Free;
  FVersions := Value;
end;

procedure TEditRegisteredServerForm.loadHooks;
(*var
  ext, iext : TFhirExtension;
  rest : TFhirCapabilityStatementRest;
  name : String;
  c : String;
  err : String; *)
begin
 // raise EFHIRTodo.create();
  (*
  clHooks.items.Clear;

  if FCapabilityStatement = nil then
    exit;

  for rest in FCapabilityStatement.restList do
    for ext in rest.extensionList do
      if ext.url = 'http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity' then
      begin
        err := '';
        c := '';
        for iext in ext.extensionList do
          if iext.url = 'name' then
            name := (iext.value as TFhirPrimitiveType).primitiveValue
          else if iext.url = 'activity' then
          begin
            if c <> '' then
              err := 'multiple activities found'
            else
            begin
              c := (iext.value as TFhirString).StringValue;
              if not TCDSHooks.isKnownHook(c) then
                err := 'Not a known hook type';
            end;

          end
          else if iext.url = 'preFetchMandatory' then
            err := 'Prefetch requirements cannot be met';
        if c = '' then
          err := 'Activity code not found';
        if err = '' then
          clHooks.Items.AddObject(name, ext)
        else
          clHooks.Items.Add(name+' (cannot be used because '+err+')');
  end;
  *)
end;

function TEditRegisteredServerForm.readServerVersion: TFHIRVersion;
begin
  result := TFHIRVersion(cbxVersion.Items.Objects[cbxVersion.ItemIndex]);
end;

(*function TEditRegisteredServerForm.hookIndex(c: TFHIRCoding): integer;
var
  i : integer;
  h : TFhirCoding;
begin
  if c = nil then
    exit(-1);
  for i := 0 to clHooks.Items.Count - 1 do
  begin
    h := clHooks.Items.Objects[i] as TFhirCoding;
    if (c.system = h.system) and (c.code = h.code) then
      exit(i);
  end;
  exit(-1);
end;
*)

procedure TEditRegisteredServerForm.Load;
var
  c : TRegisteredCDSHook;
  a : string;
  name : string;
  ndx : integer;
  b : boolean;
  i : integer;
  v : TFHIRVersion;
begin
  cbxVersion.Items.Clear;
  for v in SUPPORTED_VERSIONS do
    if FVersions.hasVersion[v] then
     cbxVersion.Items.AddObject(FVersions[v].description, TObject(FVersions[v].version));

  FAdding := server = nil;
  if server <> nil then
  begin
    Caption := 'Edit Server';
    edtName.Text := server.name;
    edtServer.Text := server.fhirEndpoint;
    cbxFormat.ItemIndex := ord(server.format);
    b := false;
    for ndx := 0 to cbxVersion.Items.Count - 1 do
      if TFHIRVersion(cbxVersion.Items.Objects[ndx]) = server.version then
      begin
        b := true;
        cbxVersion.ItemIndex := ndx;
      end;
    if not b then
    begin
      // the server has a stated version that isn't compatible in this context?
      // we're going to... guess... at the latest version?
      cbxVersion.ItemIndex := cbxVersion.Items.Count - 1 ;
    end;

    for i := 0 to clHooks.Items.Count - 1 do
      clHooks.checked[i] := false;
    for c in server.cdshooks do
      for i := 0 to clHooks.Items.Count - 1 do
      begin
        if clHooks.Items.Objects[i] <> nil then
        begin
  //          readExtension(clHooks.Items.Objects[i] as TFHIRExtension, nil, name, a);
          if (a <> '') and (c.hook = a) then
            clHooks.checked[i] := true;
        end;
      end;

    edtToken.Text := server.tokenEndpoint;
    edtAuthorize.Text := server.authorizeEndpoint;
    edtClientId.Text := server.clientid;
    edtClientId1.Text := server.clientid;
    edtClientSecret.Text := server.clientsecret;
    edtRedirect.Text := IntToStr(server.redirectport);
    edtIssuerURL.Text := server.issuerUrl;
    edtPrivateKey.Text := server.privatekey;
    edtPassphrase.Text := server.passphrase;
    cbxSmartMode.ItemIndex := ord(server.SmartAppLaunchMode);
  end;
end;

end.
