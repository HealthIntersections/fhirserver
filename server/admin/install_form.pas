unit install_form;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, ComCtrls, IniFiles,
  fsl_base, fsl_utilities, fsl_npm_client,
  fdb_manager,
  server_config, utilities, database_installer,
  install_log;

type

  { TEndpointInstallForm }

  TEndpointInstallForm = class(TForm)
    btnInstall: TBitBtn;
    btnDBTest3: TBitBtn;
    cbxSecurity: TComboBox;
    edtFilter: TEdit;
    edtUserName: TEdit;
    edtPassword: TEdit;
    edtAnonymousRights: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblCurrentStatus: TLabel;
    lblMode: TLabel;
    lvPackages: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure btnInstallClick(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure edtUserNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvPackagesItemChecked(Sender: TObject; Item: TListItem);
  private
    FConnection: TFDBConnection;
    FEndPoint: String;
    FFilename: String;
    FPackages : TFslList<TFHIRPackageInfo>;
    FSelectedPackages : TStringList;
    FType: String;
    FVersion: String;
    function isAutomatic(pi: TFHIRPackageInfo): boolean;
    function matchesVersion(pi, piv: String): boolean;
    procedure SetConnection(AValue: TFDBConnection);
    procedure SetPackages(AValue: TFslList<TFHIRPackageInfo>);
    procedure loadPackages;
    function passesFilter(id : String) : boolean;
    function command : String;
    procedure SetVersion(AValue: String);
  public
    property Packages : TFslList<TFHIRPackageInfo> read FPackages write SetPackages;
    property Connection : TFDBConnection read FConnection write SetConnection;
    property Filename : String read FFilename write FFilename;
    property endpoint : String read FEndPoint write FEndPoint;
    property version : String read FVersion write SetVersion;
    property type_ : String read FType write FType;
  end;

var
  EndpointInstallForm: TEndpointInstallForm;

function InstallEndPoint(owner : TComponent; cfg : TFHIRServerConfigFile; epInfo : TFHIRServerConfigSection) : boolean;

implementation

{$R *.lfm}

uses
  console_form;

function InstallEndPoint(owner : TComponent; cfg : TFHIRServerConfigFile; epInfo : TFHIRServerConfigSection) : boolean;
var
  t : String;
  db : TFDBManager;
  conn : TFDBConnection;
  dbi : TFHIRDatabaseInstaller;
  form : TEndpointInstallForm;
  cursor : TCursor;
begin
  cursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    db := connectToDatabase(epInfo, false);
    try
      conn := db.GetConnection('install');
      try
        form := TEndpointInstallForm.Create(owner);
        try
          form.Packages := MainConsoleForm.Packages.link;
          form.Connection := conn.link;
          form.Filename := cfg.filename;
          form.endpoint := epInfo.name;
          form.type_ := epInfo['type'].value;
          form.version := epInfo['version'].value;
          form.lblCurrentStatus.Caption := 'Current Database Status: '+checkDatabaseInstall(epInfo);
          Screen.Cursor := crDefault;
          result := form.ShowModal = mrOK;
        finally
          form.free;
        end;
      finally
        conn.release;
      end;
    finally
      db.free;
    end;
  finally
    Screen.Cursor := cursor;
  end;
end;


{ TEndpointInstallForm }

procedure TEndpointInstallForm.FormDestroy(Sender: TObject);
begin
  FSelectedPackages.free;
  FPackages.free;
  FConnection.free;
end;

procedure TEndpointInstallForm.edtUserNameChange(Sender: TObject);
begin
  btnInstall.enabled := 
   (not edtUserName.enabled or ((edtUserName.text <> '') and IsValidIdent(edtUserName.text))) and
   (not edtPassword.enabled or ((edtPassword.text <> '') and (pos(' ', edtPassword.text) = 0))) and
   (not edtAnonymousRights.Enabled or (edtAnonymousRights.text <> ''));
end;

procedure TEndpointInstallForm.btnInstallClick(Sender: TObject);
var
  form : TInstallProgressForm;
begin
  form := TInstallProgressForm.Create(self);
  try
    form.command := command;
    if form.ShowModal = mrOk then
      ModalResult := mrOk;
  finally
    form.free;
  end;
end;

procedure TEndpointInstallForm.edtFilterChange(Sender: TObject);
begin
  loadPackages;
end;

function TEndpointInstallForm.matchesVersion(pi, piv : String):boolean;
begin
  if version = 'r2' then
    result := SameText(pi, 'DSTU2') or SameText(pi, 'STU2') or pi.StartsWith('1.0') or piv.StartsWith('1.0')
  else if version = 'r3' then
    result := SameText(pi, 'STU3') or pi.StartsWith('3.0') or piv.StartsWith('3.0')
  else if version = 'r4' then
    result := SameText(pi, 'R4') or pi.StartsWith('4.0') or piv.StartsWith('4.0')
  else if version = 'r4b' then
    result := SameText(pi, 'R4B') or pi.StartsWith('4.3') or piv.StartsWith('4.3')
  else if version = 'r5' then
    result := SameText(pi, 'R5') or pi.StartsWith('5.0') or piv.StartsWith('5.0')
  else
    result := false;
end;

function TEndpointInstallForm.isAutomatic(pi : TFHIRPackageInfo):boolean;
begin
  if pi.id = 'hl7.fhir.core' then
    result := true
  else if pi.id = 'hl7.fhir.'+version+'.core' then
    result := true
  else if pi.id = 'hl7.terminology' then
    result := true
  else
    result := false;
end;

procedure TEndpointInstallForm.FormShow(Sender: TObject);
begin
  FSelectedPackages := TStringList.Create;
  FSelectedPackages.Duplicates := dupIgnore;
  FSelectedPackages.sorted := true;
  lblMode.caption := 'Install '+type_+' for version '+version;
  if type_ = 'terminology' then
  begin
    edtUserName.text := '';
    edtUserName.Enabled := true;
    edtPassword.text := '';
    edtPassword.Enabled := true;
    cbxSecurity.itemIndex := 0;
    cbxSecurity.Enabled := false;
    edtAnonymousRights.text := 'User/*.read';
    edtAnonymousRights.Enabled := false;
    lvPackages.items.clear;
    lvPackages.Enabled := false;
    loadPackages;
  end
  else if type_ = 'full' then
  begin
    edtUserName.text := '';
    edtUserName.Enabled := true;
    edtPassword.text := '';
    edtPassword.Enabled := true;
    cbxSecurity.itemIndex := 0;
    edtPassword.Enabled := true;
    edtAnonymousRights.text := 'User/*.*';
    edtAnonymousRights.Enabled := true;
    lvPackages.items.clear;
    lvPackages.Enabled := true;
    loadPackages;
  end
  else
  begin
    edtUserName.text := 'n/a';
    edtUserName.Enabled := false;
    edtPassword.text := 'n/a';
    edtPassword.Enabled := false;
    cbxSecurity.itemIndex := -1;
    edtPassword.Enabled := false;
    edtAnonymousRights.text := 'n/a';
    edtAnonymousRights.Enabled := false;
    lvPackages.items.clear;
    lvPackages.Enabled := false;
  end;
end;

procedure TEndpointInstallForm.lvPackagesItemChecked(Sender: TObject; Item: TListItem);
begin
  if item.Checked then
    FSelectedPackages.add(item.caption)
  else if (FSelectedPackages.IndexOf(item.caption) > -1) then
    FSelectedPackages.delete(FSelectedPackages.IndexOf(item.caption));
end;

procedure TEndpointInstallForm.SetPackages(AValue: TFslList<TFHIRPackageInfo>);
begin
  FPackages.free;
  FPackages := AValue;
end;

procedure TEndpointInstallForm.loadPackages;
var
  pi : TFHIRPackageInfo;
  li : TListItem;
begin
  lvPackages.Items.clear;
  for pi in FPackages do
  begin
    if matchesversion(pi.fhirVersion, pi.version) and not isAutomatic(pi) and passesFilter(pi.id) then
    begin
      li := lvPackages.items.Add;
      li.Caption := pi.id+'#'+pi.version;
      li.Checked := FSelectedPackages.IndexOf(li.caption) > -1;
    end;
  end;
  lvPackages.ViewStyle := vsIcon;
  lvPackages.ViewStyle := vsList;
end;

function TEndpointInstallForm.passesFilter(id: String): boolean;
begin
  result := (edtFilter.Text = '') or id.contains(edtFilter.text);
end;

function TEndpointInstallForm.command: String;
var
  f, s : String;
  i : TListItem;
  ini : TIniFile;
begin
  f := FilePath(['[tmp]', 'fhir-server-install.ini']);
  Ini := TIniFile.create(f);
  try
    ini.writeString('config', 'cfgFile', filename);
  finally
    Ini.Free;
  end;

  // default-rights
  result := '-cmd installdb -installer -cfg "'+f+'" -endpoint '+FEndPoint+' -username '+edtUserName.text+' -password '+edtPassword.text;
  case cbxSecurity.ItemIndex of
    0: result := result + ' -security open';
    1: result := result + ' -security oauth?';
    2: result := result + ' -security oauth';
    3: result := result + ' -security cert';
  end;
  s := '';
  for i in lvPackages.items do
    if i.Checked then
      if s = '' then
        s := i.Caption
      else
        s := s+','+i.Caption;
  if s <> '' then
    result := result + ' -packages '+s;
  if edtAnonymousRights.text <> '' then
    result := result + ' -default-rights '+edtAnonymousRights.text;
end;

procedure TEndpointInstallForm.SetVersion(AValue: String);
begin
  FVersion := AValue.toLower;
end;

procedure TEndpointInstallForm.SetConnection(AValue: TFDBConnection);
begin
  FConnection.free;
  FConnection := AValue;
end;

end.

