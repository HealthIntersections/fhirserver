unit frm_server_settings;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
    btnAddAll1: TButton;
    btnCert: TSpeedButton;
    btnOk: TButton;
    btnAddAll: TButton;
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
    edtScopes: TEdit;
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
    Label9: TLabel;
    mInfo: TMemo;
    nbSmartDetails: TNotebook;
    pgBackend: TPage;
    pgNoSmart: TPage;
    pgSmart: TPage;
    pgSettings: TPageControl;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnAddAll1Click(Sender: TObject);
    procedure btnAddAllClick(Sender: TObject);
    procedure btnCertClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure cbxSmartModeChange(Sender: TObject);
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
      if checkMetadata(FLastUrl, FServer, msg2) then
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
      checkWellKnown(FLastUrl, FServer, msg1);
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

procedure TServerSettingsForm.cbxSmartModeChange(Sender: TObject);
begin
  nbSmartDetails.PageIndex := cbxSmartMode.ItemIndex;
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
  FServer.smartMode := TSmartAppLaunchMode(cbxSmartMode.ItemIndex);
  FServer.ClientId := edtClientId.text;
  FServer.ClientSecret := edtClientSecret.text;
  FServer.Redirect := edtRedirect.text;
  FServer.scopes := edtScopes.text;
end;

procedure TServerSettingsForm.btnCertClick(Sender: TObject);
begin
  dlgLog.FileName := edtLogFile.text;
  if dlgLog.Execute then
    edtLogFile.text := dlgLog.FileName;
end;

procedure TServerSettingsForm.btnAddAllClick(Sender: TObject);
var
  s, t : String;
  i : integer;
begin
  s := '';
  for i := 0 to FServer.smartConfig.forceArr['scopes_supported'].Count - 1 do
  begin
    t := FServer.smartConfig.forceArr['scopes_supported'].Value[i];
    if (t.StartsWith('patient/')) then
      s := s + ' ' +t;
  end;
  edtScopes.text := s.trim;
end;

procedure TServerSettingsForm.btnAddAll1Click(Sender: TObject);
var
  s, t : String;
  i : integer;
begin
  s := '';
  for i := 0 to FServer.smartConfig.forceArr['scopes_supported'].Count - 1 do
  begin
    t := FServer.smartConfig.forceArr['scopes_supported'].Value[i];
    if (t.StartsWith('user/')) then
      s := s + ' ' +t;
  end;
  edtScopes.text := s.trim;
end;

procedure TServerSettingsForm.SetServer(AValue: TFHIRServerEntry);
begin
  pgSettings.PageIndex := 0;
  FServer.Free;
  FServer := AValue;
  if FServer = nil then
  begin
    edtName.text := '';
    edtUrl.text := '';
    edtName.enabled := false;
    edtUrl.enabled := false;
    cbxVersion.ItemIndex := -1;
    cbxVersion.enabled := false;
    cbxFormat.ItemIndex := -1;
    cbxFormat.enabled := false;
    edtLogFile.text := '';
    cbxSmartMode.ItemIndex := -1;
    cbxSmartMode.enabled := false;
    edtClientId.text := '';
    edtClientId.enabled := false;
    edtClientSecret.text := '';
    edtClientSecret.enabled := false;
    edtRedirect.text := '';
    edtRedirect.enabled := false;
    edtScopes.text := '';
    edtScopes.enabled := false;
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
    if FServer.smartConfig <> nil then
      FServer.getInformation(mInfo.lines);
    cbxSmartMode.ItemIndex := Ord(server.smartMode);
    cbxSmartModeChange(self);
    edtClientId.text := FServer.ClientId;
    edtClientSecret.text := FServer.ClientSecret;
    edtRedirect.text := FServer.Redirect;
    edtScopes.text := FServer.scopes;
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

