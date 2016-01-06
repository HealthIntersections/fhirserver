unit NewServerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.ExtCtrls, StringSupport,
  Vcl.ComCtrls, Vcl.CheckLst, FHIRBase, FHIRResources, FHIRTypes, AdvGenerics, FHIRUtilities,
  CDSHooksUtilities, SmartOnFHIRUtilities;

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
    clHooks: TCheckListBox;
    Button1: TButton;
    Formt: TLabel;
    cbxFormat: TComboBox;
    Button3: TButton;
    procedure edtNameChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnFetchClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FIndex : integer;
    FConformance : TFhirConformance;
    procedure loadConformance;
    function hookIndex(c : TFHIRCoding) : integer;
    procedure listHooks(list : TAdvList<TRegisteredCDSHook>);
    procedure loadHooks;
    procedure readExtension(ext: TFHIRExtension; preFetch: TStringList; var name: String; var c: TFHIRCoding);
  public
    { Public declarations }
    procedure LoadFrom(i : integer);
  end;

var
  RegisterServerForm: TRegisterServerForm;

implementation

{$R *.dfm}

uses
  FHIRPluginSettings, FHIRClient;

procedure TRegisterServerForm.btnFetchClick(Sender: TObject);
var
  authorize, token : String;
begin
  if FConformance = nil then
    loadConformance;
  if usesSmartOnFHIR(FConformance, authorize, token) then
  begin
    edtAuthorize.Text := authorize;
    edtToken.Text := token;
  end
  else
    ShowMessage('This end point doesn''t support SMART on FHIR');
end;

procedure TRegisterServerForm.listHooks(list : TAdvList<TRegisteredCDSHook>);
var
  i : integer;
  cds : TRegisteredCDSHook;
  name : String;
  c: TFHIRCoding;
begin
  list.Clear;
  for i := 0 to clHooks.Items.Count - 1 do
    if clHooks.Checked[i] and (clHooks.items.Objects[i] <> nil) then
    begin
      cds := TRegisteredCDSHook.Create;
      try
        readExtension(TFHIRExtension(clHooks.items.Objects[i]), cds.preFetch, name, c);
        cds.name := name;
        cds.activity := c.Link;
        list.Add(cds.link);
      finally
        cds.Free;
      end;
    end;
end;

procedure TRegisterServerForm.loadConformance;
var
  client : TFhirClient;
begin
  try
    clHooks.items.Clear;
    client := TFhirClient.Create(edtServer.text, true);
    try
      client.timeout := 5000;
      FConformance := client.conformance(false);
    finally
      client.Free;
    end;
    loadHooks;
  except
    client := TFhirClient.Create(edtServer.text, false);
    try
      client.timeout := 5000;
      FConformance := client.conformance(false);
    finally
      client.Free;
    end;
  end;
end;

procedure TRegisterServerForm.btnOkClick(Sender: TObject);
var
  server : TRegisteredFHIRServer;
begin
  server := TRegisteredFHIRServer.Create;
  try
    server.name := edtName.Text;
    server.SmartOnFHIR := edtAuthorize.Text <> '';
    server.fhirEndpoint := edtServer.Text;
    server.format := TFHIRFormat(cbxFormat.ItemIndex);
    server.tokenEndpoint := edtToken.Text;
    server.authorizeEndpoint := edtAuthorize.Text;
    server.clientid := edtClientId.Text;
    server.clientsecret := edtClientSecret.Text;
    server.redirectport := StrToIntDef(edtRedirect.Text, 0);
    listHooks(server.cdshooks);
    if FIndex = -1 then
      Settings.registerServer(server)
    else
      Settings.updateServerInfo(FIndex, server);
  finally
    server.Free;
  end;
end;

procedure TRegisterServerForm.Button1Click(Sender: TObject);
var
  ext : TFHIRExtension;
  i : integer;
begin
  if FConformance = nil then
    loadConformance;
  for i := 0 to clHooks.Items.Count - 1 do
    clHooks.Checked[i] := false;
  for ext in FConformance.extensionList do
    if ext.url = 'http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity' then
    begin
      i := hookIndex(ext.value as TFHIRCoding);
      if i > -1 then
        clHooks.Checked[i] := true;
    end;
end;

procedure TRegisterServerForm.Button3Click(Sender: TObject);
begin
  if FConformance = nil then
    loadConformance;
  if FConformance.formatList.hasCode('application/json+fhir') then
    cbxFormat.ItemIndex := 2
  else if FConformance.formatList.hasCode('application/xml+fhir') then
    cbxFormat.ItemIndex := 1
  else
    ShowMessage('This end point doens''t have any compatible formats in it''s conformance statement');
end;

procedure TRegisterServerForm.edtNameChange(Sender: TObject);
begin
  if (edtAuthorize.Text <> '') then
    btnOk.Enabled := (edtName.text <> '') and (edtServer.text <> '') and (edtAuthorize.Text <> '') and (edtToken.Text <> '') and (edtClientId.Text <> '') and StringIsInteger16(edtRedirect.Text)
  else
    btnOk.Enabled := (edtName.text <> '') and (edtServer.text <> '') and (edtAuthorize.Text = '') and (edtToken.Text = '') and (edtClientId.Text = '') and (edtRedirect.Text = '');
  btnOk.Enabled := edtServer.text <> '';
end;

procedure TRegisterServerForm.FormCreate(Sender: TObject);
begin
  FIndex := -1;
  inherited;
end;

procedure TRegisterServerForm.FormDestroy(Sender: TObject);
begin
  FConformance.Free;
  inherited;
end;

procedure TRegisterServerForm.FormShow(Sender: TObject);
begin
  if FIndex = -1 then
    loadHooks;
end;

procedure TRegisterServerForm.readExtension(ext : TFHIRExtension; preFetch : TStringList; var name : String; var c : TFHIRCoding);
var
  iext : TFhirExtension;
begin
  for iext in ext.extensionList do
    if iext.url = 'name' then
      name := (iext.value as TFhirPrimitiveType).primitiveValue
   else if iext.url = 'activity' then
      c := iext.value as TFhirCoding
   else if iext.url = 'preFetchMandatory' then
     if preFetch <> nil then
       preFetch.add(TFHIRPrimitiveType(iext.value).primitiveValue);
end;

procedure TRegisterServerForm.loadHooks;
var
  ext, iext : TFhirExtension;
  rest : TFhirConformanceRest;
  name : String;
  c : TFHIRCoding;
  err : String;
begin
  clHooks.items.Clear;

  if FConformance = nil then
    exit;

  for rest in FConformance.restList do
    for ext in rest.extensionList do
      if ext.url = 'http://fhir-registry.smarthealthit.org/StructureDefinition/cds-activity' then
      begin
        err := '';
        c := nil;
        for iext in ext.extensionList do
          if iext.url = 'name' then
            name := (iext.value as TFhirPrimitiveType).primitiveValue
          else if iext.url = 'activity' then
          begin
            if c <> nil then
              err := 'multiple activities found'
            else
            begin
              c := iext.value as TFhirCoding;
              if not TCDSHooks.isKnownHook(c) then
                err := 'Not a known hook type';
            end;

          end
          else if iext.url = 'preFetchMandatory' then
            err := 'Prefetch requirements cannot be met';
        if c = nil then
          err := 'Activity code not found';
        if err = '' then
          clHooks.Items.AddObject(name, ext)
        else
          clHooks.Items.Add(name+' (cannot be used because '+err+')');
  end;
end;

function TRegisterServerForm.hookIndex(c: TFHIRCoding): integer;
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

procedure TRegisterServerForm.LoadFrom(i: integer);
var
  server : TRegisteredFHIRServer;
  c : TRegisteredCDSHook;
  a : TFHIRCoding;
  name : string;
begin
  Caption := 'Edit Server';
  FIndex := i;
  server := settings.serverInfo(FIndex);
  try
    edtName.Text := server.name;
    edtServer.Text := server.fhirEndpoint;
    cbxFormat.ItemIndex := ord(server.format);

    loadConformance;
    for i := 0 to clHooks.Items.Count - 1 do
      clHooks.checked[i] := false;
    for c in server.cdshooks do
      for i := 0 to clHooks.Items.Count - 1 do
      begin
        if clHooks.Items.Objects[i] <> nil then
        begin
          readExtension(clHooks.Items.Objects[i] as TFHIRExtension, nil, name, a);
          if (a <> nil) and (c.activity.system = a.system) and (c.activity.code = a.code) then
            clHooks.checked[i] := true;
        end;
      end;

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
  finally
    server.Free;
  end;
end;

end.
