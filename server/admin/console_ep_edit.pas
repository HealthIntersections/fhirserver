unit console_ep_edit;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  fdb_dialects, fdb_manager, fdb_odbc, fdb_odbc_objects,
  server_config, utilities;

type
  { TEditEPForm }

  TEditEPForm = class(TForm)
    btnDBTest: TBitBtn;
    btnDBTest1: TBitBtn;
    btnDBTest3: TBitBtn;
    btnEPInstall: TBitBtn;
    cbxDriver: TComboBox;
    cbxType: TComboBox;
    cbxMode: TComboBox;
    edtDBName: TEdit;
    edtIdentity: TEdit;
    edtPassword: TEdit;
    edtPath: TEdit;
    edtServer: TEdit;
    edtUsername: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    rbMSSQL: TRadioButton;
    rbMySQL: TRadioButton;
    procedure btnDBTestClick(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbMSSQLChange(Sender: TObject);
  private
    FEP: TFHIRServerConfigSection;
    procedure SetEP(AValue: TFHIRServerConfigSection);
  public
    property EP : TFHIRServerConfigSection read FEP write SetEP;
    procedure update;
  end;

var
  EditEPForm: TEditEPForm;

implementation

{$R *.lfm}

{ TEditEPForm }

procedure TEditEPForm.FormDestroy(Sender: TObject);
begin
  FEP.Free;
end;

procedure TEditEPForm.FormResize(Sender: TObject);
begin
  rbMySQL.left := edtIdentity.Left + edtIdentity.Width div 2;
end;

procedure TEditEPForm.FormShow(Sender: TObject);
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

procedure TEditEPForm.rbMSSQLChange(Sender: TObject);
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
    cbxMode.itemIndex := cbxMode.Items.IndexOf(EP['mode'].value);
    edtPath.text := EP['path'].value;
    rbMSSQL.Checked := EP['db-type'].value = 'mssql';
    rbMySQL.Checked := EP['db-type'].value = 'mysql';
    rbMSSQLChange(self);
    if EP['db-type'].value <> '' then
      cbxDriver.Text := EP['db-type'].value;
    edtServer.Text := EP['db-server'].value;
    edtDBName.Text := EP['db-database'].value;
    edtUsername.Text := EP['db-username'].value;
    edtPassword.Text := EP['db-password'].value;
  end;
end;


procedure TEditEPForm.cbxTypeChange(Sender: TObject);
begin
  if cbxType.ItemIndex = -1 then
  begin
    cbxMode.Enabled := false;
  end
  else
  if cbxType.items[cbxType.ItemIndex] = 'package' then
  begin
    cbxMode.Enabled := false;
    cbxMode.itemIndex := -1;
  end
  else
  begin
    cbxMode.Enabled := true;
    if cbxMode.itemIndex = -1 then
      cbxMode.itemIndex := cbxMode.Items.IndexOf(EP['mode'].value);
  end;
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

procedure TEditEPForm.update;
begin
  EP.name := edtIdentity.text;
  if cbxType.ItemIndex > -1 then
    EP['type'].value := cbxType.items[cbxType.ItemIndex]
  else
    EP['type'].value := '';
  if cbxMode.itemIndex > -1 then
    EP['mode'].value := cbxMode.Items[cbxMode.itemIndex]
  else
    EP['mode'].value := '';
  EP['path'].value := edtPath.text;

  EP['db-type'].value := cbxDriver.Text;
  if rbMySQL.Checked then
    EP['db-type'].value := 'mysql'
  else
    EP['db-type'].value := 'mssql';
  EP['db-server'].value := edtServer.Text;
  EP['db-database'].value := edtDBName.Text;
  EP['db-username'].value := edtUsername.Text;
  EP['db-password'].value := edtPassword.Text;
end;

end.

