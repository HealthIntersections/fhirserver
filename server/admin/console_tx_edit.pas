unit console_tx_edit;

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
  fdb_dialects, fdb_manager, fdb_odbc, fdb_odbc_objects,
  server_config, utilities;

type
  { TEditTxForm }

  TEditTxForm = class(TForm)
    Bevel1: TBevel;
    btnDBTest: TBitBtn;
    btnSource: TBitBtn;
    btnDBTest1: TBitBtn;
    btnDBTest3: TBitBtn;
    btnSource1: TBitBtn;
    cbAutocreate: TCheckBox;
    cbxDriver: TComboBox;
    cbxType: TComboBox;
    chkActive: TCheckBox;
    chkDefault: TCheckBox;
    edtDBName: TEdit;
    edtIdentity: TEdit;
    edtPassword: TEdit;
    edtSQLiteFile: TEdit;
    edtServer: TEdit;
    edtFile: TEdit;
    edtUsername: TEdit;
    edtVersion: TEdit;
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
    dlgOpen: TOpenDialog;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    rbMSSQL: TRadioButton;
    rbMySQL: TRadioButton;
    rbSQLite: TRadioButton;
    procedure btnDBTestClick(Sender: TObject);
    procedure btnSource1Click(Sender: TObject);
    procedure btnSourceClick(Sender: TObject);
    procedure btnTxImportClick(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbMSSQLClick(Sender: TObject);
  private
    FTx: TFHIRServerConfigSection;
    function isDatabase(type_: String): boolean;
    procedure SetTx(AValue: TFHIRServerConfigSection);
  public
    property Tx : TFHIRServerConfigSection read FTx write SetTx;
    procedure update;
  end;

var
  EditTxForm: TEditTxForm;

implementation

{$R *.lfm}

{ TEditTxForm }

procedure TEditTxForm.FormDestroy(Sender: TObject);
begin
  FTx.Free;
end;

procedure TEditTxForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    update;
    CanClose := true;
  end;
end;

function TEditTxForm.isDatabase(type_ : String) : boolean;
begin
  result := (type_ = 'rxnorm') or (type_ = 'ndc') or (type_ = 'unii') or (type_ = 'ndfrt') or (type_ = 'nci');
end;

procedure TEditTxForm.cbxTypeChange(Sender: TObject);
begin
  if cbxType.itemIndex > -1 then
  begin
    Tx['type'].value := cbxType.items[cbxType.itemIndex];
    if isDatabase(Tx['type'].value) then
    begin
      edtFile.enabled := false;
      btnSource.enabled := false;
      rbMSSQL.Enabled := true;
      rbMySQL.Enabled := true;
      cbxDriver.Enabled := true;
      edtServer.Enabled := true;
      edtDBName.Enabled := true;
      edtUsername.Enabled := true;
      edtPassword.Enabled := true;
      btnDBTest.enabled := true;
    end
    else
    begin
      edtFile.enabled := true;
      btnSource.enabled := true;
      rbMSSQL.Enabled := false;
      rbMySQL.Enabled := false;
      cbxDriver.Enabled := false;
      edtServer.Enabled := false;
      edtDBName.Enabled := false;
      edtUsername.Enabled := false;
      edtPassword.Enabled := false;
      btnDBTest.enabled := false;
    end;
    edtVersion.enabled := Tx['type'].value = 'ndc';
    chkDefault.Enabled := Tx['type'].value = 'snomed';
  end;
end;

procedure TEditTxForm.btnSourceClick(Sender: TObject);
begin
  dlgOpen.fileName := edtFile.text;
  if dlgOpen.Execute then
    edtFile.text := dlgOpen.fileName;
end;

procedure TEditTxForm.btnTxImportClick(Sender: TObject);
begin

end;

procedure TEditTxForm.btnDBTestClick(Sender: TObject);
var
  db : TFDBManager;
begin
  update;
  try
    db := connectToDatabase(tx);
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

procedure TEditTxForm.btnSource1Click(Sender: TObject);
begin
  dlgOpen.fileName := edtSQLiteFile.text;
  if dlgOpen.Execute then
    edtSQLiteFile.text := dlgOpen.fileName;
end;

procedure TEditTxForm.FormResize(Sender: TObject);
begin
  rbMySQL.left := edtIdentity.Left + ((edtIdentity.Width div 3) * 1);
  rbSQLite.left := edtIdentity.Left + ((edtIdentity.Width div 3) * 2);
end;

procedure TEditTxForm.FormShow(Sender: TObject);
var
 env : TOdbcEnv;
 adm : TOdbcAdministrator;
begin
  env := TOdbcEnv.create;
  try
    adm := TOdbcAdministrator.create(env);
    try
      cbxDriver.items.assign(adm.Drivers);
    finally
      adm.Free;
    end;
  finally
    env.free;
  end;
end;

procedure TEditTxForm.rbMSSQLClick(Sender: TObject);
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

procedure TEditTxForm.SetTx(AValue: TFHIRServerConfigSection);
begin
  FTx.Free;
  FTx := AValue;

  if FTx <> nil then
  begin
    edtIdentity.text := Tx.name;
    cbxType.itemIndex := cbxType.Items.IndexOf(Tx['type'].value);
    cbxTypeChange(self);

    edtFile.Text := Tx['source'].value;
    chkActive.Checked := tx['active'].readAsBool;

    rbMSSQL.Checked := tx['db-type'].value = 'mssql';
    rbMySQL.Checked := tx['db-type'].value = 'mysql';
    rbSQLite.Checked := tx['db-type'].value = 'sqlite';
    rbMSSQLClick(self);
    if tx['db-type'].value <> '' then
      cbxDriver.Text := tx['db-type'].value;
    edtServer.Text := tx['db-server'].value;
    edtDBName.Text := tx['db-database'].value;
    edtUsername.Text := tx['db-username'].value;
    edtPassword.Text := tx['db-password'].value;
    edtSQLiteFile.Text := tx['db-file'].value;
    edtVersion.text := Tx['version'].value;
    chkDefault.Checked := Tx['default'].readAsBool;
    cbAutocreate.Checked := Tx['db-auto-create'].value = 'true';
  end;
end;

procedure TEditTxForm.update;
begin
  Tx.name := edtIdentity.text;
  Tx['type'].value := cbxType.items[cbxType.ItemIndex];
  Tx['source'].value := edtFile.Text;
  Tx['active'].ValueBool := chkActive.Checked;

  tx['db-type'].value := cbxDriver.Text;
  if rbMySQL.Checked then
    tx['db-type'].value := 'mysql'
  else if rbSQLite.checked then
    tx['db-type'].value := 'sqlite'
  else
    tx['db-type'].value := 'mssql';
  tx['db-server'].value := edtServer.Text;
  tx['db-database'].value := edtDBName.Text;
  tx['db-username'].value := edtUsername.Text;
  tx['db-password'].value := edtPassword.Text;
  tx['db-file'].value := edtSQLiteFile.Text;
  Tx['version'].value := edtVersion.text;
  if not chkDefault.Enabled then
    Tx['default'].value := ''
  else if chkDefault.Checked then
    Tx['default'].value := 'true'
  else
    Tx['default'].value := 'false';
  if cbAutocreate.Checked then
    Tx['db-auto-create'].value := 'true'
  else
    Tx['db-auto-create'].value := 'false';
end;

end.

