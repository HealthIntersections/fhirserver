unit console_managers;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, UITypes,
  Dialogs,
  fsl_base,
  fdb_manager, fdb_odbc, fdb_dialects,
  fui_lcl_managers,
  server_ini,
  console_db_edit, console_tx_edit, console_ep_edit, console_id_edit;

type

  { TDBManager }

  TDBManager = class (TListManager<TFHIRServerIniComplex>)
  private
    FIni: TFHIRServerIniFile;
    procedure SetIni(AValue: TFHIRServerIniFile);
    procedure DoTest(sender : TObject);
  public
    destructor Destroy; override;
    property ini : TFHIRServerIniFile read FIni write SetIni;

    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerIniComplex) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRServerIniComplex; col : integer) : String; override;
    function getSummaryText(item : TFHIRServerIniComplex) : String; override;
    function compareItem(left, right : TFHIRServerIniComplex; col : integer) : integer; override;

    function EditItem(item : TFHIRServerIniComplex; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerIniComplex; override;
    procedure DeleteItem(item : TFHIRServerIniComplex); override;
    procedure ExecuteItem(item : TFHIRServerIniComplex; mode : String); override;
  end;

  { TTXManager }

  TTXManager = class (TListManager<TFHIRServerIniComplex>)
  private
    FIni: TFHIRServerIniFile;
    procedure SetIni(AValue: TFHIRServerIniFile);
    function source(item: TFHIRServerIniComplex): String;
    function status(item: TFHIRServerIniComplex): String;
  public
    destructor Destroy; override;
    property ini : TFHIRServerIniFile read FIni write SetIni;

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

  { TEndPointManager }

  TEndPointManager = class (TListManager<TFHIRServerIniComplex>)
  private
    FIni: TFHIRServerIniFile;
    procedure SetIni(AValue: TFHIRServerIniFile);
    function status(item: TFHIRServerIniComplex): String;
  public
    destructor Destroy; override;
    property ini : TFHIRServerIniFile read FIni write SetIni;

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

  { TIdentityProviderManager }

  TIdentityProviderManager = class (TListManager<TFHIRServerIniComplex>)
  private
    FIni: TFHIRServerIniFile;
    procedure SetIni(AValue: TFHIRServerIniFile);
  public
    destructor Destroy; override;
    property ini : TFHIRServerIniFile read FIni write SetIni;

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

{ TIdentityProviderManager }

destructor TIdentityProviderManager.Destroy;
begin
  FIni.Free;
  inherited Destroy;
end;

procedure TIdentityProviderManager.SetIni(AValue: TFHIRServerIniFile);
begin
  FIni.Free;
  FIni := aValue;
  Enabled := FIni <> nil;
end;

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
      item['#status'] := '';
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

{ TEndPointManager }

destructor TEndPointManager.Destroy;
begin
  Fini.Free;
  inherited Destroy;
end;

procedure TEndPointManager.SetIni(AValue: TFHIRServerIniFile);
begin
  Fini.Free;
  FIni := AValue;
  Enabled := FIni <> nil;
end;

function TEndPointManager.status(item: TFHIRServerIniComplex): String;
begin
  if (item.value['#status'] = '') then
  begin
    item.value['#status'] := 'todo';

  end;
  if item.value['#status'] = '-' then
    result := ''
  else
    result := item.value['#status'];

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

function TEndPointManager.getCellText(item: TFHIRServerIniComplex; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item.value['type'];
    2: result := item.value['mode'];
    3: result := item.value['path'];
    4: result := item.value['database'];
    5: result := item.value['security'];
    6: result := item.value['validate'];
    7: result := status(item);
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
    5: result := CompareStr(left.value['security'], right.value['security']);
    6: result := CompareStr(left.value['validate'], right.value['validate']);
    7: result := CompareStr(status(left), status(right));
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
      item['#status'] := '';
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

{ TTXManager }

destructor TTXManager.Destroy;
begin
  Fini.Free;
  inherited Destroy;
end;

procedure TTXManager.SetIni(AValue: TFHIRServerIniFile);
begin
  Fini.Free;
  FIni := AValue;
  Enabled := FIni <> nil;
end;

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
  if (item.value['#status'] = '') then
  begin
    if (item.value['source'] <> '') then
    begin
      if FileExists(item.value['source']) then
        item.value['#status'] := '-'
      else
        item.value['#status'] := 'File Not Found';
    end
    else
    begin
      if ini.databases.ContainsKey(item.value['database']) then
        item.value['#status'] := '-'
      else
        item.value['#status'] := 'Database Not Found';
    end;
  end;
  if item.value['#status'] = '-' then
    result := ''
  else
    result := item.value['#status'];
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
      item['#status'] := '';
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

{ TDBManager }

destructor TDBManager.Destroy;
begin
  Fini.Free;
  inherited Destroy;
end;

procedure TDBManager.SetIni(AValue: TFHIRServerIniFile);
begin
  Fini.Free;
  FIni := AValue;
  Enabled := FIni <> nil;
end;

procedure TDBManager.DoTest(sender: TObject);
var
  frm : TEditDBForm;
begin
  frm := TComponent(sender).Owner as TEditDBForm;
  frm.update;
  ExecuteItem(frm.DB, '');
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
    result := [opAdd, opEdit, opDelete, opExecute];
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
  else
    result := inherited compareItem(left, right, col);
  end;
end;

function TDBManager.EditItem(item: TFHIRServerIniComplex; mode: String): boolean;
var
  frm : TEditDBForm;
begin
  frm := TEditDBForm.create(List.Owner);
  try
    frm.DB := item.clone;
    frm.btnDBTest.OnClick := DoTest;
    result := frm.ShowModal = mrOK;
    if result then
    begin
      item.assign(frm.DB);
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
    frm.btnDBTest.OnClick := DoTest;
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

procedure TDBManager.ExecuteItem(item: TFHIRServerIniComplex; mode: String);
var
  db : TFslDBManager;
begin
  try
    if (item['type'] = 'mssql') then
      db := TFslDBOdbcManager.create(item.name, kdbSQLServer, 2, 0, item['driver'], item['server'], item['database'], item['username'], item['password'])
    else
      db := TFslDBOdbcManager.create(item.name, kdbMySQL, 2, 0, item['driver'], item['server'], item['database'], item['username'], item['password']);
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

end.

