unit console_ep_edit;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  fsl_base,
  fdb_dialects, fdb_manager, fdb_odbc, fdb_odbc_objects,
  server_config, utilities,
  install_form;

type
  { TEditEPForm }

  TEditEPForm = class(TForm)
    Bevel1: TBevel;
    btnDBTest: TBitBtn;
    btnDBTest1: TBitBtn;
    btnDBTest3: TBitBtn;
    btnEPInstall: TBitBtn;
    cbxDriver: TComboBox;
    cbxType: TComboBox;
    cbxVersion: TComboBox;
    cbAutocreate: TCheckBox;
    chkActive: TCheckBox;
    edtDBName: TEdit;
    edtIdentity: TEdit;
    edtPassword: TEdit;
    edtSQLiteFile: TEdit;
    edtPath: TEdit;
    edtServer: TEdit;
    edtFolder: TEdit;
    edtUsername: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    rbMSSQL: TRadioButton;
    rbMySQL: TRadioButton;
    rbSQLite: TRadioButton;
    procedure btnDBTestClick(Sender: TObject);
    procedure btnEPInstallClick(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbMSSQLChange(Sender: TObject);
  private
    FEP: TFHIRServerConfigSection;
    FCfg : TFHIRServerConfigFile;
    procedure SetCfg(AValue: TFHIRServerConfigFile);
    procedure SetEP(AValue: TFHIRServerConfigSection);
  public
    property EP : TFHIRServerConfigSection read FEP write SetEP;
    property Cfg : TFHIRServerConfigFile read FCfg write SetCfg;
    procedure update;
  end;

var
  EditEPForm: TEditEPForm;

function hasVersion(typ : String) : boolean;
function hasFixedVersion(typ : String) : boolean;
function hasVersions(typ : String) : boolean;
function hasDatabase(typ : String) : boolean;
function hasSrcFolder(typ : String) : boolean;

implementation

{$R *.lfm}

function hasVersion(typ : String) : boolean;
begin
  result := (typ = 'full') or (typ = 'terminology') or (typ = 'bridge');
end;

function hasVersions(typ : String) : boolean;
begin
  result := (typ = 'full') or (typ = 'terminology');
end;

function hasFixedVersion(typ : String) : boolean;
begin
  result := (typ = 'bridge');
end;

function hasDatabase(typ : String) : boolean;
begin
  result := not ((typ = 'snomed') or (typ = 'loinc') or (typ = 'folder') or (typ = 'icao'));
end;

function hasSrcFolder(typ : String) : boolean;
begin
  result := (typ = 'folder');
end;

{ TEditEPForm }

procedure TEditEPForm.FormDestroy(Sender: TObject);
begin
  FEP.Free;
  FCfg.Free;
end;

procedure TEditEPForm.FormResize(Sender: TObject);
begin
  rbMySQL.left := edtIdentity.Left + ((edtIdentity.Width div 3) * 1);
  rbSQLite.left := edtIdentity.Left + ((edtIdentity.Width div 3) * 2);
end;

procedure TEditEPForm.FormShow(Sender: TObject);
var
 env : TOdbcEnv;
 adm : TOdbcAdministrator;
 s : String;
begin
  env := TOdbcEnv.create;
  try
    adm := TOdbcAdministrator.create(env);
    try
      s := cbxDriver.Text;
      cbxDriver.items.assign(adm.Drivers);
      cbxDriver.Text := s;
    finally
      adm.Free;
    end;
  finally
    env.free;
  end;
end;

procedure TEditEPForm.rbMSSQLChange(Sender: TObject);
var
  dialect : TFDBPlatform;
  i : integer;
begin
  cbxDriver.Enabled := not rbSQLite.Checked;
  edtServer.Enabled := not rbSQLite.Checked;
  edtDBName.Enabled := not rbSQLite.Checked;
  edtUsername.Enabled := not rbSQLite.Checked;
  edtPassword.Enabled := not rbSQLite.Checked;
  edtSQLiteFile.Enabled := rbSQLite.Checked;
  cbAutocreate.enabled := rbSQLite.Checked;

  if not rbSQLite.Checked then
  begin
    if rbMySQL.Checked then
      dialect := kdbMySQL
    else
      dialect := kdbSQLServer;
    if RecogniseDriver(cbxDriver.Text) <> dialect then
    begin
      i := cbxDriver.items.IndexOf(StandardODBCDriverName(dialect));
      if i > -1 then
        cbxDriver.text := StandardODBCDriverName(dialect)
      else
        cbxDriver.text := '';
    end;
  end;
end;

procedure TEditEPForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    update;
    CanClose := true;
  end;
end;

procedure TEditEPForm.SetEP(AValue: TFHIRServerConfigSection);
begin
  FEp.Free;
  FEp := AValue;

  if FEp <> nil then
  begin
    edtIdentity.text := EP.name;
    cbxType.itemIndex := cbxType.Items.IndexOf(EP['type'].value);
    cbxVersion.itemIndex := cbxVersion.Items.IndexOf(EP['version'].value);
    edtPath.text := EP['path'].value;
    chkActive.Checked := EP['active'].readAsBool;

    edtFolder.Text := EP['folder'].value;
    rbMSSQL.Checked := EP['db-type'].value = 'mssql';
    rbMySQL.Checked := EP['db-type'].value = 'mysql';
    rbSQLite.Checked := EP['db-type'].value = 'sqlite';
    cbxDriver.Text := EP['db-driver'].value;
    edtServer.Text := EP['db-server'].value;
    edtDBName.Text := EP['db-database'].value;
    edtUsername.Text := EP['db-username'].value;
    edtPassword.Text := EP['db-password'].value;
    edtSQLiteFile.Text := EP['db-file'].value;
    cbAutocreate.checked := EP['db-auto-create'].valueBool;
    rbMSSQLChange(self);
    cbxTypeChange(self);
  end;
end;

procedure TEditEPForm.SetCfg(AValue: TFHIRServerConfigFile);
begin
  FCfg.Free;
  FCfg := AValue;
end;


procedure TEditEPForm.cbxTypeChange(Sender: TObject);
begin
  // 1. sorting out version
  if (cbxType.ItemIndex = -1) or (hasVersions(cbxType.items[cbxType.ItemIndex])) then
  begin
    cbxVersion.Enabled := true;
    cbxVersion.itemIndex := cbxVersion.Items.IndexOf(EP['version'].value);
  end
  else if hasFixedVersion(cbxType.items[cbxType.ItemIndex]) then
  begin
    cbxVersion.Enabled := false;
    cbxVersion.itemIndex := cbxVersion.Items.IndexOf('r3');
  end
  else
  begin
    cbxVersion.Enabled := false;
    cbxVersion.itemIndex := -1;
  end;

  // sorting out source
  if (cbxType.ItemIndex = -1) or (hasDatabase(cbxType.items[cbxType.ItemIndex])) then
  begin
    edtFolder.Enabled := cbxType.ItemIndex = -1;
    rbMSSQL.enabled := true;
    rbMySQL.enabled := true;
    rbSQLite.enabled := true;
    cbxDriver.enabled := not rbSQLite.Checked;
    edtServer.enabled := not rbSQLite.Checked;
    edtDBName.enabled := not rbSQLite.Checked;
    edtUsername.enabled := not rbSQLite.Checked;
    edtPassword.enabled := not rbSQLite.Checked;
    edtSQLiteFile.enabled := rbSQLite.Checked;
    cbAutocreate.enabled := rbSQLite.Checked;
  end
  else
  begin
    rbMSSQL.enabled := false;
    rbMySQL.enabled := false;
    rbSQLite.enabled := false;
    cbxDriver.enabled := false;
    edtServer.enabled := false;
    edtDBName.enabled := false;
    edtUsername.enabled := false;
    edtPassword.enabled := false;
    edtSQLiteFile.enabled := false;
    cbAutocreate.enabled := false;
    edtFolder.Enabled := hasSrcFolder(cbxType.items[cbxType.ItemIndex]);
  end;
  btnDBTest.enabled := (cbxType.ItemIndex > -1) and hasDatabase(cbxType.items[cbxType.ItemIndex]);
  btnEPInstall.enabled := btnDBTest.enabled;
end;


procedure TEditEPForm.btnDBTestClick(Sender: TObject);
var
  db : TFDBManager;
begin
  update;
  try
    db := connectToDatabase(EP);
    try
      db.checkConnection;
    finally
      db.free;
    end;
    MessageDlg('Database Connection Succeeded', mtInformation, [mbok], 0);
  except
    on e: Exception do
      MessageDlg('Database Connection Failed: '+e.message, mtError, [mbok], 0);
  end
end;

procedure TEditEPForm.btnEPInstallClick(Sender: TObject);
begin
  InstallEndPoint(self, FCfg, EP);
end;

procedure TEditEPForm.update;
begin
  EP.name := edtIdentity.text;
  if cbxType.ItemIndex > -1 then
    EP['type'].value := cbxType.items[cbxType.ItemIndex]
  else
    EP['type'].value := '';

  if not hasVersion(EP['type'].value) then
    EP['version'].value := ''
  else if cbxVersion.itemIndex = -1 then
    raise EFslException.Create('A Version is required')
  else
    EP['version'].value := cbxVersion.Items[cbxVersion.itemIndex];
  EP['active'].ValueBool := chkActive.Checked;

  EP['path'].value := edtPath.text;
  EP['folder'].value := edtFolder.Text;
  EP['db-driver'].value := cbxDriver.Text;
  if rbMySQL.Checked then
    EP['db-type'].value := 'mysql'
  else if rbSQLite.Checked then
    EP['db-type'].value := 'sqlite'
  else
    EP['db-type'].value := 'mssql';
  EP['db-server'].value := edtServer.Text;
  EP['db-database'].value := edtDBName.Text;
  EP['db-username'].value := edtUsername.Text;
  EP['db-password'].value := edtPassword.Text;
  EP['db-file'].value := edtSQLiteFile.Text;
  EP['db-auto-create'].valueBool := cbAutocreate.checked;
end;

end.

