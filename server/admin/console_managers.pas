unit console_managers;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, UITypes,
  Dialogs,
  fsl_base, fsl_threads, fsl_utilities,
  fdb_manager, fdb_odbc, fdb_dialects, fdb_sqlite3,
  fui_lcl_managers,
  server_ini, database_installer,
  console_db_edit, console_tx_edit, console_ep_edit, console_id_edit, install_form;

type

  { TAdminManager }

  TAdminManager = class abstract (TListManager<TFHIRServerIniComplex>)
  private
    FIni: TFHIRServerIniFile;
    procedure SetIni(AValue: TFHIRServerIniFile);
  public
    property ini : TFHIRServerIniFile read FIni write SetIni;
    destructor Destroy; override;

    function makeDB(dbInfo : TFHIRServerIniComplex) :  TFDBManager;
  end;

  { TAdminThread }

  TAdminThread = class (TFslThread)
  private
    FItem : TFHIRServerIniComplex;
    FManager : TAdminManager;
  public
    constructor create(manager : TAdminManager; item : TFHIRServerIniComplex);
  end;

  { TDatabaseCheck }

  TDatabaseCheck = class (TAdminThread)
  public
    procedure execute; override;
  end;

  { TDBManager }

  TDBManager = class (TAdminManager)
  private
    function status(item : TFHIRServerIniComplex) : String;
  public
    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerIniComplex) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRServerIniComplex; col : integer) : String; override;
    function getSummaryText(item : TFHIRServerIniComplex) : String; override;
    function compareItem(left, right : TFHIRServerIniComplex; col : integer) : integer; override;

    procedure Timer; override;

    function EditItem(item : TFHIRServerIniComplex; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerIniComplex; override;
    procedure DeleteItem(item : TFHIRServerIniComplex); override;
  end;

  { TTXManager }

  TTXManager = class (TAdminManager)
  private
    function source(item: TFHIRServerIniComplex): String;
    function status(item: TFHIRServerIniComplex): String;
  public
    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerIniComplex) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRServerIniComplex; col : integer) : String; override;
    function getSummaryText(item : TFHIRServerIniComplex) : String; override;
    function compareItem(left, right : TFHIRServerIniComplex; col : integer) : integer; override;

    function EditItem(item : TFHIRServerIniComplex; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerIniComplex; override;
    procedure DeleteItem(item : TFHIRServerIniComplex); override;
  end;

  { TEndPointCheck }

  TEndPointCheck = class (TAdminThread)
  public
    procedure execute; override;
  end;

  { TEndPointManager }

  TEndPointManager = class (TAdminManager)
  private
    function status(item: TFHIRServerIniComplex): String;
  public
    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerIniComplex) : TNodeOperationSet; override;
    function loadList : boolean; override;

    procedure Timer; override;

    function getCellText(item : TFHIRServerIniComplex; col : integer) : String; override;
    function getSummaryText(item : TFHIRServerIniComplex) : String; override;
    function compareItem(left, right : TFHIRServerIniComplex; col : integer) : integer; override;

    function EditItem(item : TFHIRServerIniComplex; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerIniComplex; override;
    procedure DeleteItem(item : TFHIRServerIniComplex); override;
    procedure ExecuteItem(item : TFHIRServerIniComplex; mode : String); override;
  end;

  { TIdentityProviderManager }

  TIdentityProviderManager = class (TAdminManager)
  public
    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerIniComplex) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRServerIniComplex; col : integer) : String; override;
    function getSummaryText(item : TFHIRServerIniComplex) : String; override;
    function compareItem(left, right : TFHIRServerIniComplex; col : integer) : integer; override;

    function EditItem(item : TFHIRServerIniComplex; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerIniComplex; override;
    procedure DeleteItem(item : TFHIRServerIniComplex); override;
  end;


implementation

uses
  console_form;

{ TAdminThread }

constructor TAdminThread.create(manager: TAdminManager; item: TFHIRServerIniComplex);
begin
  inherited create;
  FManager := manager;
  FItem := item;
end;

{ TAdminManager }

destructor TAdminManager.Destroy;
begin
  FIni.Free;
  inherited Destroy;
end;

function TAdminManager.makeDB(dbInfo: TFHIRServerIniComplex): TFDBManager;
begin
  if (dbInfo['type'] = 'mssql') then
    result := TFDBOdbcManager.create(dbInfo.name, kdbSQLServer, 2, 0, dbInfo['driver'], dbInfo['server'], dbInfo['database'], dbInfo['username'], dbInfo['password'])
  else if (dbInfo['type'] = 'sqlite3') then
    result := TFDBSQLiteManager.create(dbInfo.name, dbInfo['database'], true)
  else
    result := TFDBOdbcManager.create(dbInfo.name, kdbMySQL, 2, 0, dbInfo['driver'], dbInfo['server'], dbInfo['database'], dbInfo['username'], dbInfo['password']);
end;

procedure TAdminManager.SetIni(AValue: TFHIRServerIniFile);
begin
  FIni.Free;
  FIni := aValue;
  Enabled := FIni <> nil;
end;

{ TIdentityProviderManager }

function TIdentityProviderManager.canSort: boolean;
begin
  Result := true;
end;

function TIdentityProviderManager.allowedOperations(item: TFHIRServerIniComplex): TNodeOperationSet;
begin
  if (item = nil) then
    result := [opAdd]
  else
    result := [opAdd, opEdit, opDelete, opExecute];
end;

function TIdentityProviderManager.loadList: boolean;
var
  s : String;
begin
  if (ini <> nil) then
    for s in ini.identityProviders.SortedKeys do
      Data.Add(ini.identityProviders[s].link);
end;

function TIdentityProviderManager.getCellText(item: TFHIRServerIniComplex; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item.value['app-id'];
    2: result := item.value['app-secret'];
    3: result := item.value['api-key'];
  end;
end;

function TIdentityProviderManager.getSummaryText(item: TFHIRServerIniComplex): String;
begin
  Result := item.name;
end;

function TIdentityProviderManager.compareItem(left, right: TFHIRServerIniComplex; col: integer): integer;
begin
  case col of
    0: result := CompareStr(left.name, right.name);
    1: result := CompareStr(left.value['app-id'], right.value['app-id']);
    2: result := CompareStr(left.value['app-secret'], right.value['app-secret']);
    3: result := CompareStr(left.value['api-key'], right.value['api-key']);
  else
    result := inherited compareItem(left, right, col);
  end;
end;

function TIdentityProviderManager.EditItem(item: TFHIRServerIniComplex; mode: String): boolean;
var
  frm : TEditIdForm;
begin
  frm := TEditIdForm.create(List.Owner);
  try
    frm.ID := item.clone;
    result := frm.ShowModal = mrOK;
    if result then
    begin
      item.assign(frm.Id);
      item.status := '';
      if (item.name <> item['id']) then
      begin
        ini.identityProviders.add(item['id'], item.link);
        ini.identityProviders.Remove(item.Name);
        item.Name := item['id'];
      end;
    end;
    ini.save;
  finally
    frm.free;
  end;
end;

function TIdentityProviderManager.AddItem(mode: String): TFHIRServerIniComplex;
var
  frm : TEditIdForm;
begin
  Result := nil;
  frm := TEditIdForm.create(List.Owner);
  try
    frm.Id := TFHIRServerIniComplex.create('id'+inttostr(ini.databases.Count), '');
    if frm.ShowModal = mrOK then
    begin
      result := frm.Id.link;
      ini.identityProviders.add(result['id'], result.link);
      ini.save;
    end;
  finally
    frm.free;
  end;
end;

procedure TIdentityProviderManager.DeleteItem(item: TFHIRServerIniComplex);
begin
  Fini.identityProviders.Remove(item.name);
  FIni.Save;
end;


{ TEndPointCheck }

procedure TEndPointCheck.execute;
var
  db : TFDBManager;
  conn : TFDBConnection;
  meta : TFDBMetaData;
  t, m, s : String;
begin
  try
    db := FManager.makeDB(FManager.FIni.databases[FItem['database']]);
    try
      conn := db.GetConnection('check');
      try
        meta := conn.FetchMetaData;
        try
          if not meta.HasTable('Config') then
            FItem.threadStatus := 'Not Installed'
          else
          begin
            s := conn.Lookup('Config', 'ConfigKey', '100', 'Value', '');
            if (s = '') then
              FItem.threadStatus := 'Needs Reinstalling'
            else
            begin
              StringSplit(s, '|', t, s);
              StringSplit(s, '|', m, s);
              if (t <> FItem['type']) then
                FItem.threadStatus := 'Type Mismatch - Database is for '+t+': reinstall'
              else if (m <> FItem['mode']) then
                FItem.threadStatus := 'Mode Mismatch - Database is for '+m+': reinstall'
              else
                FItem.threadStatus := 'OK ('+s+')';
            end;
          end;
        finally
          meta.free;
        end;
        conn.release;
      except
        on e : Exception do
        begin
          conn.Error(e);
          raise;
        end;
      end;
    finally
      db.free;
    end;
  except
    on e: Exception do
      FItem.threadStatus := 'Error: '+e.message;
  end;
  Stop;
end;


{ TEndPointManager }

function TEndPointManager.status(item: TFHIRServerIniComplex): String;
begin
  if (item.status = '') then
  begin
    if not Fini.databases.ContainsKey(item['database']) then
      item.status := 'Database not found'
    else
    begin
      item.status := 'Checking...';
      item.Data := TEndPointCheck.create(self, item);
      TEndPointCheck(item.Data).Open;
    end;
  end;
  result := item.status;
end;

function TEndPointManager.canSort: boolean;
begin
  Result := true;
end;

function TEndPointManager.allowedOperations(item: TFHIRServerIniComplex): TNodeOperationSet;
begin
  if (item = nil) then
    result := [opAdd]
  else
    result := [opAdd, opEdit, opDelete, opExecute];
end;

function TEndPointManager.loadList: boolean;
var
  s : String;
begin
  if (ini <> nil) then
    for s in ini.endpoints.SortedKeys do
      Data.Add(ini.endpoints[s].link);
end;

procedure TEndPointManager.Timer;
var
  item : TFHIRServerIniComplex;
  wantLoad : boolean;
begin
  if FIni = nil then
    exit;

  wantLoad := false;
  for item in FIni.endpoints.values do
  begin
    if item.threadStatus <> '' then
    begin
      item.data.free;
      item.status := item.threadStatus;
      item.threadStatus := '';
      wantLoad := true;
    end;
  end;
  if wantLoad then
    doLoad;
end;

function TEndPointManager.getCellText(item: TFHIRServerIniComplex; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item.value['type'];
    2: result := item.value['mode'];
    3: result := item.value['path'];
    4: result := item.value['database'];
    5: result := status(item);
  end;
end;

function TEndPointManager.getSummaryText(item: TFHIRServerIniComplex): String;
begin
  Result := item.name;
end;

function TEndPointManager.compareItem(left, right: TFHIRServerIniComplex; col: integer): integer;
begin
  case col of
    0: result := CompareStr(left.name, right.name);
    1: result := CompareStr(left.value['type'], right.value['type']);
    2: result := CompareStr(left.value['mode'], right.value['mode']);
    3: result := CompareStr(left.value['path'], right.value['path']);
    4: result := CompareStr(left.value['database'], right.value['database']);
    5: result := CompareStr(status(left), status(right));
  else
    result := inherited compareItem(left, right, col);
  end;
end;

function TEndPointManager.EditItem(item: TFHIRServerIniComplex; mode: String): boolean;
var
  frm : TEditEPForm;
begin
  frm := TEditEPForm.create(List.Owner);
  try
    frm.Ini := ini.link;
    frm.EP := item.clone;
    result := frm.ShowModal = mrOK;
    if result then
    begin
      item.assign(frm.EP);
      item.status := '';
      if (item.name <> item['id']) then
      begin
        ini.endpoints.add(item['id'], item.link);
        ini.endpoints.Remove(item.Name);
        item.Name := item['id'];
      end;
    end;
    ini.save;
  finally
    frm.free;
  end;
end;

function TEndPointManager.AddItem(mode: String): TFHIRServerIniComplex;
var
  frm : TEditEPForm;
begin
  Result := nil;
  frm := TEditEPForm.create(List.Owner);
  try
    frm.Ini := ini.link;
    frm.EP := TFHIRServerIniComplex.create('EP'+inttostr(ini.databases.Count), '');
    frm.EP['path'] := '/path';
    if frm.ShowModal = mrOK then
    begin
      result := frm.EP.link;
      ini.endpoints.add(result['id'], result.link);
      ini.save;
    end;
  finally
    frm.free;
  end;
end;

procedure TEndPointManager.DeleteItem(item: TFHIRServerIniComplex);
begin
  Fini.endpoints.Remove(item.name);
  FIni.Save;
end;

procedure TEndPointManager.ExecuteItem(item: TFHIRServerIniComplex; mode : String);
var
  t : String;
  db : TFDBManager;
  conn : TFDBConnection;
  dbi : TFHIRDatabaseInstaller;
  form : TEndpointInstallForm;
begin
  db := makeDB(FIni.databases[item['database']]);
  try
    conn := db.GetConnection('install');
    try
      t := item['type'];
      if (t = 'package') then
      begin
        if MessageDlg('Install Package Server', 'This operation will wipe any existing installation in the database. Proceed?', mtConfirmation, mbYesNo, 0) = mryes then
        begin
            dbi := TFHIRDatabaseInstaller.create(conn, nil, nil);
            try
              dbi.uninstall;
              dbi.installPackageServer;
            finally
              dbi.free;
            end;
        end;
      end
      else
      begin
        form := TEndpointInstallForm.create(MainConsoleForm);
        try
          form.Packages := MainConsoleForm.Packages.link;
          form.Connection := conn.link;
          form.version := item['type'];
          form.mode := item['mode'];
          form.ShowModal;
        finally
          form.free;
        end;
      end;
    finally
      conn.release;
    end;
  finally
    db.free;
  end;
  item.status := '';
  doLoad;
end;

{ TTXManager }

function TTXManager.canSort: boolean;
begin
  Result := true;
end;

function TTXManager.allowedOperations(item: TFHIRServerIniComplex): TNodeOperationSet;
begin
  if (item = nil) then
    result := [opAdd]
  else
    result := [opAdd, opEdit, opDelete];
end;

function TTXManager.loadList: boolean;
var
  s : String;
begin
  if (ini <> nil) then
    for s in ini.terminologies.SortedKeys do
      Data.Add(ini.terminologies[s].link);
end;

function TTXManager.source(item: TFHIRServerIniComplex) : String;
begin
  if (item.value['source'] <> '') then
    result := item.value['source']
  else
    result := item.value['database'];
end;

function TTXManager.status(item: TFHIRServerIniComplex) : String;
begin
  if (item.status = '') then
  begin
    if (item.value['source'] <> '') then
    begin
      if FileExists(item.value['source']) then
        item.status := '-'
      else
        item.status := 'File Not Found';
    end
    else
    begin
      if ini.databases.ContainsKey(item.value['database']) then
        item.status := '-'
      else
        item.status := 'Database Not Found';
    end;
  end;
  if item.status = '-' then
    result := ''
  else
    result := item.status;
end;

function TTXManager.getCellText(item: TFHIRServerIniComplex; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item.value['type'];
    2: result := source(item);
    3: result := item.value['version'];
    4: result := item.value['default'];
    5: result := status(item);
  end;
end;

function TTXManager.getSummaryText(item: TFHIRServerIniComplex): String;
begin
  Result := item.name;
end;

function TTXManager.compareItem(left, right: TFHIRServerIniComplex; col: integer): integer;
begin
  case col of
    0: result := CompareStr(left.name, right.name);
    1: result := CompareStr(left.value['type'], right.value['type']);
    2: result := CompareStr(source(left), source(right));
    3: result := CompareStr(left.value['version'], right.value['version']);
    4: result := CompareStr(left.value['default'], right.value['default']);
    5: result := CompareStr(status(left), status(right));
  else
    result := inherited compareItem(left, right, col);
  end;
end;

function TTXManager.EditItem(item: TFHIRServerIniComplex; mode: String): boolean;
var
  frm : TEditTxForm;
begin
  frm := TEditTxForm.create(List.Owner);
  try
    frm.Ini := ini.link;
    frm.Tx := item.clone;
    result := frm.ShowModal = mrOK;
    if result then
    begin
      item.assign(frm.Tx);
      item.status := '';
      if (item.name <> item['id']) then
      begin
        ini.terminologies.add(item['id'], item.link);
        ini.terminologies.Remove(item.Name);
        item.Name := item['id'];
      end;
    end;
    ini.save;
  finally
    frm.free;
  end;
end;

function TTXManager.AddItem(mode: String): TFHIRServerIniComplex;
var
  frm : TEditTxForm;
begin
  Result := nil;
  frm := TEditTxForm.create(List.Owner);
  try
    frm.Ini := ini.link;
    frm.Tx := TFHIRServerIniComplex.create('Tx'+inttostr(ini.databases.Count), '');
    if frm.ShowModal = mrOK then
    begin
      result := frm.Tx.link;
      ini.terminologies.add(result['id'], result.link);
      ini.save;
    end;
  finally
    frm.free;
  end;
end;

procedure TTXManager.DeleteItem(item: TFHIRServerIniComplex);
begin
  Fini.terminologies.Remove(item.name);
  FIni.Save;
end;

{ TDatabaseCheck }

procedure TDatabaseCheck.execute;
var
  db : TFDBManager;
begin
  try
    db := FManager.makeDB(FItem);
    try
      db.checkConnection;
    finally
      db.free;
    end;
    FItem.threadStatus := 'Connection OK';
  except
    on e: Exception do
      FItem.threadStatus := 'Error: '+e.message;
  end;
  Stop;
end;

{ TDBManager }

function TDBManager.status(item : TFHIRServerIniComplex) : String;
begin
  if (item.status = '') then
  begin
    item.status := 'Checking ...';
    item.Data := TDatabaseCheck.create(self, item);
    TDatabaseCheck(item.Data).Open;
  end;
  result := item.status;
end;

function TDBManager.canSort: boolean;
begin
  Result := true;
end;

function TDBManager.allowedOperations(item: TFHIRServerIniComplex): TNodeOperationSet;
begin
  if (item = nil) then
    result := [opAdd]
  else
    result := [opAdd, opEdit, opDelete];
end;

function TDBManager.loadList: boolean;
var
  s : String;
begin
  if (ini <> nil) then
    for s in ini.databases.SortedKeys do
      Data.Add(ini.databases[s].link);
end;

function TDBManager.getCellText(item: TFHIRServerIniComplex; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item.value['type'];
    2: result := item.value['server'];
    3: result := item.value['database'];
    4: result := item.value['username'];
    5: result := item.value['password'];
    6: result := item.value['driver'];
    7: result := status(item);
  end;
end;

function TDBManager.getSummaryText(item: TFHIRServerIniComplex): String;
begin
  Result := item.name;
end;

function TDBManager.compareItem(left, right: TFHIRServerIniComplex; col: integer): integer;
begin
  case col of
    0: result := CompareStr(left.name, right.name);
    1: result := CompareStr(left.value['type'], right.value['type']);
    2: result := CompareStr(left.value['server'], right.value['server']);
    3: result := CompareStr(left.value['database'], right.value['database']);
    4: result := CompareStr(left.value['username'], right.value['username']);
    5: result := CompareStr(left.value['password'], right.value['password']);
    6: result := CompareStr(left.value['driver'], right.value['driver']);
    7: result := CompareStr(status(left), status(right));
  else
    result := inherited compareItem(left, right, col);
  end;
end;

procedure TDBManager.Timer;
var
  item : TFHIRServerIniComplex;
  wantLoad : boolean;
begin
  if FIni = nil then
    exit;
  wantLoad := false;
  for item in FIni.databases.values do
  begin
    if item.threadStatus <> '' then
    begin
      item.data.free;
      item.status := item.threadStatus;
      item.threadStatus := '';
      wantLoad := true;
    end;
  end;
  if wantLoad then
    doLoad;
end;

function TDBManager.EditItem(item: TFHIRServerIniComplex; mode: String): boolean;
var
  frm : TEditDBForm;
begin
  frm := TEditDBForm.create(List.Owner);
  try
    frm.DB := item.clone;
    result := frm.ShowModal = mrOK;
    if result then
    begin
      item.assign(frm.DB);
      item.status := '';
      if (item.name <> item['id']) then
      begin
        ini.databases.add(item['id'], item.link);
        ini.databases.Remove(item.Name);
        item.Name := item['id'];
      end;
    end;
    ini.save;
  finally
    frm.free;
  end;
end;

function TDBManager.AddItem(mode: String): TFHIRServerIniComplex;
var
  frm : TEditDBForm;
begin
  Result := nil;
  frm := TEditDBForm.create(List.Owner);
  try
    frm.DB := TFHIRServerIniComplex.create('DB'+inttostr(ini.databases.Count), '');
    if frm.ShowModal = mrOK then
    begin
      result := frm.DB.link;
      ini.databases.add(result['id'], result.link);
      ini.save;
    end;
  finally
    frm.free;
  end;
end;

procedure TDBManager.DeleteItem(item: TFHIRServerIniComplex);
begin
  Fini.databases.Remove(item.name);
  FIni.Save;
end;

end.

