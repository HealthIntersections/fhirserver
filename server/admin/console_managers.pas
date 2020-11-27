unit console_managers;

{$i fhir.inc}

interface

uses
  SysUtils, Classes, Graphics, UITypes,
  Dialogs,
  fsl_base, fsl_threads, fsl_utilities,
  fdb_manager,
  ftx_sct_services, ftx_loinc_services, ftx_ucum_services, ftx_lang,
  fui_lcl_managers, fui_lcl_progress,
  tx_icd10, tx_ndc, tx_rxnorm, tx_unii,
  server_config, database_installer, utilities, installer,
  console_tx_edit, console_ep_edit, console_id_edit, install_form;

type

  { TAdminManager }

  TAdminManager = class abstract (TListManager<TFHIRServerConfigSection>)
  private
    FFile : TFHIRServerConfigFile;
    procedure SetFile(value : TFHIRServerConfigFile);
  public
    destructor Destroy; override;
    property ConfigFile : TFHIRServerConfigFile read FFile write SetFile;
  end;

  { TAdminThread }

  TAdminThread = class (TFslThread)
  private
    FItem : TFHIRServerConfigSection;
    FManager : TAdminManager;
  public
    constructor create(manager : TAdminManager; item : TFHIRServerConfigSection);
  end;

  { TTxCheck }

  TTxCheck = class (TAdminThread)
  public
    procedure execute; override;
  end;

  { TTXManager }

  TTXManager = class (TAdminManager)
  private
    function source(item: TFHIRServerConfigSection): String;
    function status(item: TFHIRServerConfigSection): String;

    procedure importNDC(item: TFHIRServerConfigSection);
    procedure importRxNorm(item: TFHIRServerConfigSection);
  public
    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerConfigSection) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRServerConfigSection; col : integer) : String; override;
    function getCellColors(item : TFHIRServerConfigSection; col : integer; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TFHIRServerConfigSection) : String; override;
    function compareItem(left, right : TFHIRServerConfigSection; col : integer) : integer; override;

    procedure Timer; override;

    function EditItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerConfigSection; override;
    procedure DeleteItem(item : TFHIRServerConfigSection); override;
    procedure ExecuteItem(item : TFHIRServerConfigSection; mode : String); override;
  end;

  { TEndPointCheck }

  TEndPointCheck = class (TAdminThread)
  public
    procedure execute; override;
  end;

  { TEndPointManager }

  TEndPointManager = class (TAdminManager)
  private
    function status(item: TFHIRServerConfigSection): String;
  public
    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerConfigSection) : TNodeOperationSet; override;
    function loadList : boolean; override;

    procedure Timer; override;

    function getCellText(item : TFHIRServerConfigSection; col : integer) : String; override;
    function getCellColors(item : TFHIRServerConfigSection; col : integer; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TFHIRServerConfigSection) : String; override;
    function compareItem(left, right : TFHIRServerConfigSection; col : integer) : integer; override;

    function EditItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerConfigSection; override;
    procedure DeleteItem(item : TFHIRServerConfigSection); override;
    procedure ExecuteItem(item : TFHIRServerConfigSection; mode : String); override;
  end;

  { TIdentityProviderManager }

  TIdentityProviderManager = class (TAdminManager)
  public
    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerConfigSection) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRServerConfigSection; col : integer) : String; override;
    function getSummaryText(item : TFHIRServerConfigSection) : String; override;
    function compareItem(left, right : TFHIRServerConfigSection; col : integer) : integer; override;

    function EditItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerConfigSection; override;
    procedure DeleteItem(item : TFHIRServerConfigSection); override;
  end;


implementation

uses
  console_form;

{ TTxCheck }

procedure TTxCheck.execute;
var
  db : TFDBManager;
  conn : TFDBConnection;
begin
  if (FItem['source'].value <> '') then
  begin
    if not FileExists(FItem['source'].value) then
      FItem.threadStatus := 'File Not Found'
    else if FItem['type'].value = 'snomed' then
      FItem.threadStatus := TSnomedServices.checkFile(FItem['source'].value)
    else if FItem['type'].value = 'loinc' then
      FItem.threadStatus := TLoincServices.checkFile(FItem['source'].value)
    else if FItem['type'].value = 'ucum' then
      FItem.threadStatus := TUcumServices.checkFile(FItem['source'].value)
    else if FItem['type'].value = 'lang' then
      FItem.threadStatus := TIETFLanguageCodeServices.checkFile(FItem['source'].value)
    else if FItem['type'].value = 'icd10' then
      FItem.threadStatus := TICD10Provider.checkFile(FItem['source'].value)
    else
      FItem.threadStatus := 'to do';
  end
  else
  begin
    try
      db := connectToDatabase(FItem);
      try
        conn := db.GetConnection('check');
        try
          if FItem['type'].value = 'rxnorm' then
            FItem.threadStatus := TRxNormServices.checkDB(conn)
          else if FItem['type'].value = 'ndc' then
            FItem.threadStatus := TNDCServices.checkDB(conn)
          else if FItem['type'].value = 'unii' then
            FItem.threadStatus := TUNIIServices.checkDB(conn)
          else
            FItem.threadStatus := 'to do';
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
  end;
  Stop;
end;

{ TAdminThread }

constructor TAdminThread.create(manager: TAdminManager; item: TFHIRServerConfigSection);
begin
  inherited create;
  FManager := manager;
  FItem := item;
  AutoFree := true;
end;

{ TAdminManager }

procedure TAdminManager.SetFile(value: TFHIRServerConfigFile);
begin
  FFile.Free;
  FFile := value;
  Enabled := FFile <> nil
end;

destructor TAdminManager.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

{ TIdentityProviderManager }

function TIdentityProviderManager.canSort: boolean;
begin
  Result := true;
end;

function TIdentityProviderManager.allowedOperations(item: TFHIRServerConfigSection): TNodeOperationSet;
begin
  if (item = nil) then
    result := [opAdd]
  else
    result := [opAdd, opEdit, opDelete, opExecute];
end;

function TIdentityProviderManager.loadList: boolean;
var
  sect : TFHIRServerConfigSection;
begin
  if (ConfigFile <> nil) then
    for sect in ConfigFile['identity-providers'].sections do
      Data.Add(sect.link);
end;

function TIdentityProviderManager.getCellText(item: TFHIRServerConfigSection; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item['app-id'].value;
    2: result := item['app-secret'].value;
    3: result := item['api-key'].value;
  end;
end;

function TIdentityProviderManager.getSummaryText(item: TFHIRServerConfigSection): String;
begin
  Result := item.name;
end;

function TIdentityProviderManager.compareItem(left, right: TFHIRServerConfigSection; col: integer): integer;
begin
  case col of
    0: result := CompareStr(left.name, right.name);
    1: result := CompareStr(left['app-id'].value, right['app-id'].value);
    2: result := CompareStr(left['app-secret'].value, right['app-secret'].value);
    3: result := CompareStr(left['api-key'].value, right['api-key'].value);
  else
    result := inherited compareItem(left, right, col);
  end;
end;

function TIdentityProviderManager.EditItem(item: TFHIRServerConfigSection; mode: String): boolean;
var
  frm : TEditIdForm;
begin
  frm := TEditIdForm.create(List.Owner);
  try
    frm.ID := item.clone;
    result := frm.ShowModal = mrOK;
    if result then
      item.assign(frm.Id);
    FFile.save;
  finally
    frm.free;
  end;
end;

function TIdentityProviderManager.AddItem(mode: String): TFHIRServerConfigSection;
var
  frm : TEditIdForm;
begin
  Result := nil;
  frm := TEditIdForm.create(List.Owner);
  try
    frm.Id := TFHIRServerConfigSection.create('id'+inttostr(FFile['identity-providers'].sections.Count));
    if frm.ShowModal = mrOK then
    begin
      result := FFile['identity-providers'].section[frm.Id.Name].link;
      result.assign(frm.Id);
      FFile.save;
    end;
  finally
    frm.free;
  end;
end;

procedure TIdentityProviderManager.DeleteItem(item: TFHIRServerConfigSection);
begin
  FFile['identity-providers'].remove(item.name);
  FFile.Save;
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
    db := connectToDatabase(FItem);
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
              if (t <> FItem['type'].value) then
                FItem.threadStatus := 'Type Mismatch - Database is for '+t+': reinstall'
              else if (m <> FItem['mode'].value) then
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

function TEndPointManager.status(item: TFHIRServerConfigSection): String;
begin
  if not hasDatabase(item['type'].value) then
    item.status := '-'
  else if (item.status = '') then
  begin
    item.status := 'Checking...';
    TEndPointCheck.create(self, item.link).Start;
  end;
  result := item.status;
end;

function TEndPointManager.canSort: boolean;
begin
  Result := true;
end;

function TEndPointManager.allowedOperations(item: TFHIRServerConfigSection): TNodeOperationSet;
begin
  if (item = nil) then
    result := [opAdd]
  else
    result := [opAdd, opEdit, opDelete, opExecute];
end;

function TEndPointManager.loadList: boolean;
var
  sect : TFHIRServerConfigSection;
begin
  result := true;
  if (FFile <> nil) then
    for sect in FFile['endpoints'].sections do
      Data.Add(sect.link);
end;

procedure TEndPointManager.Timer;
var
  item : TFHIRServerConfigSection;
  wantLoad : boolean;
begin
  if FFile = nil then
    exit;

  wantLoad := false;
  for item in FFile['endpoints'].sections do
  begin
    if item.threadStatus <> '' then
    begin
      item.status := item.threadStatus;
      item.threadStatus := '';
      wantLoad := true;
    end;
  end;
  if wantLoad then
    doLoad;
end;

function TEndPointManager.getCellText(item: TFHIRServerConfigSection; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item['type'].value;
    2: result := item['version'].value;
    3: result := BoolToStr(item['active'].valueBool, 'yes', 'no');
    4: result := item['path'].value;
    5: result := item['database'].value;
    6: result := status(item);
  end;
end;

function TEndPointManager.getCellColors(item: TFHIRServerConfigSection; col: integer; var fore, back: TColor): boolean;
begin
  Result := false;
  if col = 3 then
  begin
    result := true;
    if item['active'].valueBool then
      back := ColourCompose(120, 236, 120, 0)
    else
      back := ColourCompose(255, 180, 180, 0);
  end;
end;

function TEndPointManager.getSummaryText(item: TFHIRServerConfigSection): String;
begin
  Result := item.name;
end;

function TEndPointManager.compareItem(left, right: TFHIRServerConfigSection; col: integer): integer;
begin
  case col of
    0: result := CompareStr(left.name, right.name);
    1: result := CompareStr(left['type'].value, right['type'].value);
    2: result := CompareStr(left['version'].value, right['version'].value);
    3: result := CompareStr(BoolToStr(left['active'].valueBool, 'yes', 'no'), BoolToStr(right['active'].valueBool, 'yes', 'no'));
    4: result := CompareStr(left['path'].value, right['path'].value);
    5: result := CompareStr(left['database'].value, right['database'].value);
    6: result := CompareStr(status(left), status(right));
  else
    result := inherited compareItem(left, right, col);
  end;
end;

function TEndPointManager.EditItem(item: TFHIRServerConfigSection; mode: String): boolean;
var
  frm : TEditEPForm;
begin
  frm := TEditEPForm.create(List.Owner);
  try
    frm.EP := item.clone;
    result := frm.ShowModal = mrOK;
    if result then
      item.assign(frm.EP);
    FFile.save;
  finally
    frm.free;
  end;
end;

function TEndPointManager.AddItem(mode: String): TFHIRServerConfigSection;
var
  frm : TEditEPForm;
begin
  Result := nil;
  frm := TEditEPForm.create(List.Owner);
  try
    frm.EP := TFHIRServerConfigSection.create('EP'+inttostr(FFile['endpoints'].sections.Count));
    frm.EP['path'].value := '/path';
    if frm.ShowModal = mrOK then
    begin
      result := FFile['endpoints'].section[frm.EP.Name].link;
      result.assign(frm.EP);
      FFile.save;
    end;
  finally
    frm.free;
  end;
end;

procedure TEndPointManager.DeleteItem(item: TFHIRServerConfigSection);
begin
  FFile['endpoints'].remove(item.name);
  FFile.Save;
end;

procedure TEndPointManager.ExecuteItem(item: TFHIRServerConfigSection; mode : String);
begin
  InstallEndPoint(list.Owner, FFile, item);
  item.status := '';
  doLoad;
end;

{ TTXManager }

function TTXManager.canSort: boolean;
begin
  Result := true;
end;

function TTXManager.allowedOperations(item: TFHIRServerConfigSection): TNodeOperationSet;
begin
  if (item = nil) then
    result := [opAdd]
  else if (item['type'].value = 'ndc') or (item['type'].value = 'rxnorm') then
    result := [opAdd, opEdit, opDelete, opExecute]
  else
    result := [opAdd, opEdit, opDelete];
end;

function TTXManager.loadList: boolean;
var
  sect : TFHIRServerConfigSection;
begin
  result := true;
  if (FFile <> nil) then
    for sect in FFile['terminologies'].sections do
      Data.Add(sect.link);
end;

function TTXManager.source(item: TFHIRServerConfigSection) : String;
begin
  if (item['source'].value <> '') then
    result := 'file:'+item['source'].value
  else
    result := describeDatabase(item);
end;

function TTXManager.status(item: TFHIRServerConfigSection) : String;
begin
  if (item.status = '') then
  begin
    item.status := 'Checking ...';
    TTxCheck.create(self, item).Start;
  end;
  result := item.status;
end;

procedure TTXManager.importNDC(item: TFHIRServerConfigSection);
var
  db : TFDBManager;
  conn : TFDBConnection;
  dlg : TSelectDirectoryDialog;
  ndc : TNdcImporter;
begin
  dlg := TSelectDirectoryDialog.create(List.Owner);
  try
    dlg.Filename := Settings.readString('ndc', 'source', '');
    if dlg.execute then
    begin
      Settings.writeString('ndc', 'source', dlg.Filename);
      db := connectToDatabase(item);
      try
        conn := db.GetConnection('check');
        try
          ndc := TNdcImporter.create(dlg.FileName, conn.link);
          try
            DoForegroundTask(List.Owner, ndc.Doinstall);
          finally
            ndc.free;
          end;
          item.status := '';
          doLoad;
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
    end;
  finally
    dlg.Free;
  end;
end;

procedure TTXManager.importRxNorm(item: TFHIRServerConfigSection);
var
  db : TFDBManager;
  conn : TFDBConnection;
  dlg : TSelectDirectoryDialog;
  umls : TUMLSImporter;
begin
  dlg := TSelectDirectoryDialog.create(List.Owner);
  try
    dlg.Filename := Settings.readString('rxnorm', 'source', '');
    if dlg.execute then
    begin
      Settings.writeString('rxnorm', 'source', dlg.Filename);
      db := connectToDatabase(item);
      try
        conn := db.GetConnection('check');
        try
          umls := TUMLSImporter.create(dlg.FileName, conn.link);
          try
            DoForegroundTask(List.Owner, umls.Doinstall);
          finally
            umls.free;
          end;
          item.status := '';
          doLoad;
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
    end;
  finally
    dlg.Free;
  end;
end;

function TTXManager.getCellText(item: TFHIRServerConfigSection; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item['type'].value;
    2: result := BoolToStr(item['active'].valueBool, 'yes', 'no');
    3: result := source(item);
    4: result := item['version'].value;
    5: result := item['default'].value;
    6: result := status(item);
  end;
end;

function TTXManager.getCellColors(item: TFHIRServerConfigSection; col: integer; var fore, back: TColor): boolean;
begin
  Result := false;
  if col = 2 then
  begin
    result := true;
    if item['active'].valueBool then
      back := ColourCompose(120, 236, 120, 0)
    else
      back := ColourCompose(255, 180, 180, 0);
  end;
end;

function TTXManager.getSummaryText(item: TFHIRServerConfigSection): String;
begin
  Result := item.name;
end;

function TTXManager.compareItem(left, right: TFHIRServerConfigSection; col: integer): integer;
begin
  case col of
    0: result := CompareStr(left.name, right.name);
    1: result := CompareStr(left['type'].value, right['type'].value);
    2: result := CompareStr(BoolToStr(left['active'].valueBool, 'yes', 'no'), BoolToStr(right['active'].valueBool, 'yes', 'no'));
    3: result := CompareStr(source(left), source(right));
    4: result := CompareStr(left['version'].value, right['version'].value);
    5: result := CompareStr(left['default'].value, right['default'].value);
    6: result := CompareStr(status(left), status(right));
  else
    result := inherited compareItem(left, right, col);
  end;
end;

procedure TTXManager.Timer;
var
  item : TFHIRServerConfigSection;
  wantLoad : boolean;
begin
  if FFile = nil then
    exit;
  wantLoad := false;
  for item in FFile['terminologies'].sections do
  begin
    if item.threadStatus <> '' then
    begin
      item.status := item.threadStatus;
      item.threadStatus := '';
      wantLoad := true;
    end;
  end;
  if wantLoad then
    doLoad;
end;

function TTXManager.EditItem(item: TFHIRServerConfigSection; mode: String): boolean;
var
  frm : TEditTxForm;
begin
  frm := TEditTxForm.create(List.Owner);
  try
    frm.Tx := item.clone;
    result := frm.ShowModal = mrOK;
    if result then
      item.assign(frm.Tx);
    FFile.save;
  finally
    frm.free;
  end;
end;

function TTXManager.AddItem(mode: String): TFHIRServerConfigSection;
var
  frm : TEditTxForm;
begin
  Result := nil;
  frm := TEditTxForm.create(List.Owner);
  try
    frm.Tx := TFHIRServerConfigSection.create('tx'+inttostr(FFile['terminologies'].sections.Count));
    if frm.ShowModal = mrOK then
    begin
      result := FFile['terminologies'].section[frm.Tx.Name].link;
      result.assign(frm.Tx);
      FFile.save;
    end;
  finally
    frm.free;
  end;
end;


procedure TTXManager.DeleteItem(item: TFHIRServerConfigSection);
begin
  FFile['terminologies'].remove(item.name);
  FFile.Save;
end;

procedure TTXManager.ExecuteItem(item: TFHIRServerConfigSection; mode: String);
begin
  if (item['type'].value = 'ndc') then
    importNDC(item)
  else if (item['type'].value = 'rxnorm') then
    importRxNorm(item)
  else
    raise Exception.create('Not done yet');
end;


end.

