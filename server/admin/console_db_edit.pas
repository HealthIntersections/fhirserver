unit console_db_edit;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  fdb_odbc_objects, fdb_dialects, fdb_odbc,
  server_ini;

type
  { TEditDBForm }

  TEditDBForm = class(TForm)
    btnDBTest: TBitBtn;
    btnDBTest1: TBitBtn;
    btnDBTest3: TBitBtn;
    cbxDriver: TComboBox;
    edtIdentity: TEdit;
    edtServer: TEdit;
    edtDBName: TEdit;
    edtUsername: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    rbMSSQL: TRadioButton;
    rbMySQL: TRadioButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbMSSQLClick(Sender: TObject);
  private
    FDB: TFHIRServerIniComplex;
    procedure SetDB(AValue: TFHIRServerIniComplex);
  public
    property DB : TFHIRServerIniComplex read FDB write SetDB;
    procedure update;
  end;

var
  EditDBForm: TEditDBForm;

implementation

{$R *.lfm}

{ TEditDBForm }

procedure TEditDBForm.FormDestroy(Sender: TObject);
begin
  FDB.Free;
end;

procedure TEditDBForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    update;
    CanClose := true;
  end;
end;

procedure TEditDBForm.FormResize(Sender: TObject);
begin
  rbMySQL.left := edtIdentity.Left + edtIdentity.Width div 2;
end;

procedure TEditDBForm.FormShow(Sender: TObject);
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

procedure TEditDBForm.rbMSSQLClick(Sender: TObject);
var
  dialect : TFslDBPlatform;
  i : integer;
begin
  if rbMySQL.Checked then
  begin
    DB['type'] := 'mysql';
    dialect := kdbMySQL;
  end
  else
  begin
    DB['type'] := 'mssql';
    dialect := kdbSQLServer;
  end;
  if RecogniseDriver(cbxDriver.Text) <> dialect then
  begin
    i := cbxDriver.items.IndexOf(StandardODBCDriverName(dialect));
    if i > -1 then
      cbxDriver.text := StandardODBCDriverName(dialect)
    else
      cbxDriver.text := '';
  end;
end;

procedure TEditDBForm.SetDB(AValue: TFHIRServerIniComplex);
begin
  FDB.Free;
  FDB:=AValue;

  if FDb <> nil then
  begin
    edtIdentity.text := DB['id'];
    if DB['type'] = 'mysql' then
      rbMySQL.Checked := true
    else
      rbMSSQL.Checked := true;
    cbxDriver.text := DB['driver'];
    rbMSSQLClick(self);
    edtServer.text := DB['server'];
    edtDBName.text := DB['database'];
    edtUsername.text := DB['username'];
    edtPassword.text := DB['password'];
  end;
end;

procedure TEditDBForm.update;
begin
  DB['id'] := edtIdentity.text;
  DB['driver'] := cbxDriver.text;
  DB['server'] := edtServer.text;
  DB['database'] := edtDBName.text;
  DB['username'] := edtUsername.text;
  DB['password'] := edtPassword.text;
end;

end.

