unit console_tx_edit;

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
    btnTxImport: TBitBtn;
    cbxDriver: TComboBox;
    cbxType: TComboBox;
    chkActive: TCheckBox;
    chkDefault: TCheckBox;
    edtDBName: TEdit;
    edtIdentity: TEdit;
    edtPassword: TEdit;
    edtServer: TEdit;
    edtFile: TEdit;
    edtUsername: TEdit;
    edtVersion: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
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
    procedure btnDBTestClick(Sender: TObject);
    procedure btnSourceClick(Sender: TObject);
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

procedure TEditTxForm.FormResize(Sender: TObject);
begin
  rbMySQL.left := edtIdentity.Left + edtIdentity.Width div 2;
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

procedure TEditTxForm.SetTx(AValue: TFHIRServerConfigSection);
begin
  FTx.Free;
  FTx:=AValue;

  if FTx <> nil then
  begin
    edtIdentity.text := Tx.name;
    cbxType.itemIndex := cbxType.Items.IndexOf(Tx['type'].value);
    cbxTypeChange(self);

    edtFile.Text := Tx['source'].value;
    chkActive.Checked := tx['active'].readAsBool;
    rbMSSQL.Checked := tx['db-type'].value = 'mssql';
    rbMySQL.Checked := tx['db-type'].value = 'mysql';
    rbMSSQLClick(self);
    if tx['db-type'].value <> '' then
      cbxDriver.Text := tx['db-type'].value;
    edtServer.Text := tx['db-server'].value;
    edtDBName.Text := tx['db-database'].value;
    edtUsername.Text := tx['db-username'].value;
    edtPassword.Text := tx['db-password'].value;
    edtVersion.text := Tx['version'].value;
    chkDefault.Checked := Tx['default'].readAsBool;
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
  else
    tx['db-type'].value := 'mssql';
  tx['db-server'].value := edtServer.Text;
  tx['db-database'].value := edtDBName.Text;
  tx['db-username'].value := edtUsername.Text;
  tx['db-password'].value := edtPassword.Text;
  Tx['version'].value := edtVersion.text;
  if not chkDefault.Enabled then
    Tx['default'].value := ''
  else if chkDefault.Checked then
    Tx['default'].value := 'true'
  else
    Tx['default'].value := 'false'
end;

end.

