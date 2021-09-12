unit frm_server_settings;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, ComCtrls,
  ExtCtrls, StdCtrls, Buttons,
  fsl_base, fsl_utilities,
  fhir_objects, fhir_utilities, fhir_client, fhir_oauth, fhir_common, fhir_factory,
  ftk_utilities;

type

  { TServerSettingsForm }
  TServerSettingsForm = class(TForm)
    btnCert: TSpeedButton;
    btnOk: TButton;
    Button2: TButton;
    Button3: TButton;
    cbxFormat: TComboBox;
    cbxSmartMode: TComboBox;
    cbxVersion: TComboBox;
    dlgLog: TOpenDialog;
    edtClientId: TEdit;
    edtClientSecret: TEdit;
    edtName: TEdit;
    edtRedirect: TEdit;
    edtUrl: TEdit;
    edtLogFile: TEdit;
    Formt: TLabel;
    Label1: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    mInfo: TMemo;
    Notebook2: TNotebook;
    pgBackend: TPage;
    pgNoSmart: TPage;
    pgSmart: TPage;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnCertClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure edtUrlChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServer : TFHIRServerEntry;
    FServerList: TFslList<TFHIRServerEntry>;
    FLastUrl : String;
    procedure SetServer(AValue: TFHIRServerEntry);
    procedure checkInput(control : TWinControl; test : boolean; message : String);
    procedure SetServerList(AValue: TFslList<TFHIRServerEntry>);
    function nameOk(s : String) : boolean;
  public
    property Server : TFHIRServerEntry read FServer write SetServer;
    property ServerList : TFslList<TFHIRServerEntry> read FServerList write SetServerList;
  end;

var
  ServerSettingsForm: TServerSettingsForm;

implementation

{$R *.lfm}

{ TServerSettingsForm }

procedure TServerSettingsForm.FormDestroy(Sender: TObject);
begin
  FServerList.Free;
  FServer.Free;
end;

//factory : TFHIRFactory;
//client : TFHIRClientV;
//csv : TFHIRResourceV;
//cs : TFhirCapabilityStatementW;
    //factory := makeFactory(TFHIRVersions.readVersion(cbxVersion.text));
    //try
    //  client := factory.makeClient(nil, url, fctCrossPlatform, ffJson);
    //  try
    //    csv := client.conformanceV(true);
    //    try
    //      cs := factory.wrapCapabilityStatement(csv);
    //      try
    //        cbxFormat.Items.Clear;
    //        if cs.hasFormat('json') then
    //          cbxFormat.Items.add('json');
    //        if cs.hasFormat('xml') then
    //          cbxFormat.Items.add('xml');
    //        Beep;
    //      finally
    //        cs.free;
    //      end;
    //    finally
    //      csv.free;
    //    end;
    //  finally
    //    client.free;
    //  end;
    //finally
    //  factory.free;
    //end;

procedure TServerSettingsForm.Button3Click(Sender: TObject);
var
  msg1, msg2 : String;
begin
  checkInput(edtName, edtName.Text <> '', 'A name is required');
  checkInput(edtName, nameOK(edtName.Text), 'The name "'+edtName.Text+'" already is in use');
  FLastUrl := edtUrl.Text;
  checkInput(edtUrl, isAbsoluteUrl(FLastUrl) and (FLastUrl.startsWith('http://') or FLastUrl.startsWith('https://')), 'A valid web URL is required');
  try
    mInfo.text := '';
    Cursor := crHourGlass;
    try
      if checkWellKnown(FLastUrl, FServer, msg1) and checkMetadata(FLastUrl, FServer, msg2) then
      begin
        case FServer.version of
          fhirVersionRelease2 : cbxVersion.ItemIndex := 0;
          fhirVersionRelease3 : cbxVersion.ItemIndex := 1;
          fhirVersionRelease4 : cbxVersion.ItemIndex := 2;
        else
          cbxVersion.ItemIndex := -1;
        end;
        cbxVersion.enabled := false;

        cbxFormat.Enabled := false;
        if FServer.xml and FServer.Json then
        begin
          cbxFormat.Enabled := true;
          cbxFormat.ItemIndex := 0;
        end
        else if FServer.xml then
          cbxFormat.ItemIndex := 1
        else if FServer.json then
          cbxFormat.ItemIndex := 0
        else
          cbxFormat.ItemIndex := -1;
        btnOk.Enabled := (cbxFormat.ItemIndex > -1) and (cbxVersion.ItemIndex > -1);
      end
      else
      begin
        btnOk.Enabled := false;
        cbxVersion.Enabled := false;
        cbxFormat.Enabled := false;
      end;
      mInfo.lines.Clear;
      if msg1 <> '' then
        mInfo.lines.Add(msg1);
      if msg2 <> '' then
        mInfo.lines.Add(msg2);
      FServer.getInformation(mInfo.lines);
    finally
      cursor := crDefault;
    end;
  except
    on e : Exception do
      MessageDlg('Error Connecting to '+edtName.text, e.message, mtError, [mbok], 0);
  end;
end;

procedure TServerSettingsForm.edtUrlChange(Sender: TObject);
begin
  if FLastUrl <> edtUrl.Text then
  begin
    btnOk.enabled := false;
  end;
end;

procedure TServerSettingsForm.btnOkClick(Sender: TObject);
var
  url : String;
begin
  checkInput(edtName, edtName.Text <> '', 'A name is required');
  checkInput(edtName, nameOK(edtName.Text), 'The name "'+edtName.Text+'" already is in use');
  url := edtUrl.Text;
  checkInput(edtUrl, isAbsoluteUrl(url) and (url.startsWith('http://') or url.startsWith('https://')), 'A valid web URL is required');
  FServer.name := edtName.Text;
  FServer.URL := edtUrl.text;
  FServer.logFileName := edtLogFile.text;
  if cbxFormat.ItemIndex = 0 then
    FServer.format := ffJson
  else
    FServer.format := ffXml;
  FServer.version := TFHIRVersions.readVersion(cbxVersion.text);
end;

procedure TServerSettingsForm.btnCertClick(Sender: TObject);
begin
  dlgLog.FileName := edtLogFile.text;
  if dlgLog.Execute then
    edtLogFile.text := dlgLog.FileName;
end;

procedure TServerSettingsForm.SetServer(AValue: TFHIRServerEntry);
begin
  FServer.Free;
  FServer := AValue;
  if FServer = nil then
  begin
    edtName.text := '';
    edtUrl.text := '';
    edtName.enabled := false;
    edtUrl.enabled := false;
    edtLogFile.text := '';
  end
  else
  begin
    edtName.text := FServer.Name;
    edtUrl.text := FServer.URL;
    edtName.enabled := true;
    edtUrl.enabled := true;
    edtLogFile.text := FServer.logFileName;
    if FServer.id <> '' then
    begin
      cbxFormat.Enabled := FServer.xml and FServer.json;
      if FServer.format = ffXml then
        cbxFormat.ItemIndex := 1
      else
        cbxFormat.ItemIndex := 0;
      case FServer.version of
        fhirVersionRelease2 : cbxVersion.ItemIndex := 0;
        fhirVersionRelease3 : cbxVersion.ItemIndex := 1;
        fhirVersionRelease4 : cbxVersion.ItemIndex := 2;
      else
        cbxVersion.ItemIndex := -1;
      end;
      cbxVersion.enabled := false;
    end
    else
    begin
      cbxFormat.Enabled := false;
      cbxFormat.itemIndex := -1;
      cbxVersion.Enabled := false;
      cbxVersion.itemIndex := -1;
    end;
  end;
end;

procedure TServerSettingsForm.checkInput(control: TWinControl; test: boolean; message: String);
begin
  if not test then
  begin
    FocusControl(control);
    MessageDlg('Input Error', message, mtError, [mbok], 0);
    abort;
  end;
end;

procedure TServerSettingsForm.SetServerList(AValue: TFslList<TFHIRServerEntry>);
begin
  FServerList.Free;
  FServerList := AValue;
end;

function TServerSettingsForm.nameOk(s: String): boolean;
var
  sd : TFHIRServerEntry;
begin
  result := true;
  for sd in ServerList do
    if (s = sd.name) and (sd.id <> FServer.id) then
      exit(false);
end;

end.

