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
  server_config, database_installer, utilities,
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

  { TTXStatusCheckRequest }

  TTXStatusCheckRequest = class (TBackgroundTaskRequestPackage)
  private
    Fitem : TFHIRServerConfigSection;
  public
    constructor Create(item : TFHIRServerConfigSection);
    destructor Destroy; override;
  end;

  { TTXStatusCheckResponse }

  TTXStatusCheckResponse = class (TBackgroundTaskResponsePackage)
  private
    FItem : TFHIRServerConfigSection;
    FStatus : String;
  public
    constructor Create(item : TFHIRServerConfigSection);
    destructor Destroy; override;
    Property Status : String read FStatus write FStatus;
  end;

  { TTXStatusChecker }

  TTXStatusChecker = class (TBackgroundTaskEngine)
  public
    procedure execute(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage); override;
    function name : String; override;
  end;

  { TTXManager }

  TTXManager = class (TAdminManager)
  private
    FStatusTask : integer;
    function source(item: TFHIRServerConfigSection): String;
    function status(item: TFHIRServerConfigSection): String;
    procedure doStatusCallback(id : integer; response : TBackgroundTaskResponsePackage);

    procedure importNDC(item: TFHIRServerConfigSection);
    procedure importRxNorm(item: TFHIRServerConfigSection);
  public
    constructor Create; override;

    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerConfigSection) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRServerConfigSection; col : integer) : String; override;
    function getCellColors(item : TFHIRServerConfigSection; col : integer; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TFHIRServerConfigSection) : String; override;
    function compareItem(left, right : TFHIRServerConfigSection; col : integer) : integer; override;

    procedure buildMenu; override;

    function EditItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerConfigSection; override;
    procedure DeleteItem(item : TFHIRServerConfigSection); override;
    function ExecuteItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
  end;

  { TEPStatusCheckRequest }

  TEPStatusCheckRequest = class (TBackgroundTaskRequestPackage)
  private
    Fitem : TFHIRServerConfigSection;
  public
    constructor Create(item : TFHIRServerConfigSection);
    destructor Destroy; override;
  end;

  { TEPStatusCheckResponse }

  TEPStatusCheckResponse = class (TBackgroundTaskResponsePackage)
  private
  private
    FItem : TFHIRServerConfigSection;
    FStatus : String;
  public
    constructor Create(item : TFHIRServerConfigSection);
    destructor Destroy; override;
    Property Status : String read FStatus write FStatus;
  end;

  { TEPStatusChecker }

  TEPStatusChecker = class (TBackgroundTaskEngine)
  public
    procedure execute(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage); override;
    function name : String; override;
  end;

  { TEndPointManager }

  TEndPointManager = class (TAdminManager)
  private
    FStatusTask : integer;
    function status(item: TFHIRServerConfigSection): String;
    procedure doStatusCallback(id : integer; response : TBackgroundTaskResponsePackage);
  public
    constructor Create; override;

    function canSort : boolean; override;
    function allowedOperations(item : TFHIRServerConfigSection) : TNodeOperationSet; override;
    function loadList : boolean; override;

    procedure buildMenu; override;

    function getCellText(item : TFHIRServerConfigSection; col : integer) : String; override;
    function getCellColors(item : TFHIRServerConfigSection; col : integer; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TFHIRServerConfigSection) : String; override;
    function compareItem(left, right : TFHIRServerConfigSection; col : integer) : integer; override;

    function EditItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
    function AddItem(mode : String) : TFHIRServerConfigSection; override;
    procedure DeleteItem(item : TFHIRServerConfigSection); override;
    function ExecuteItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
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

{ TEPStatusChecker }

procedure TEPStatusChecker.execute(request: TBackgroundTaskRequestPackage; response: TBackgroundTaskResponsePackage);
begin
  (response as TEPStatusCheckResponse).status := checkDatabaseInstall((request as TEPStatusCheckRequest).Fitem);
end;

function TEPStatusChecker.name: String;
begin
  result := 'End-point Status Checker';
end;

{ TEPStatusCheckResponse }

constructor TEPStatusCheckResponse.Create(item: TFHIRServerConfigSection);
begin
  inherited Create;
  FItem := item;
end;

destructor TEPStatusCheckResponse.Destroy;
begin
  FItem.Free;
  inherited Destroy;
end;

{ TTXStatusChecker }

procedure TTXStatusChecker.execute(request: TBackgroundTaskRequestPackage; response: TBackgroundTaskResponsePackage);
var
  item : TFHIRServerConfigSection;
  s : String;
  db : TFDBManager;
  conn : TFDBConnection;
begin
  item := (request as TTXStatusCheckRequest).Fitem;
  if (item['source'].value <> '') then
  begin
    if not FileExists(item['source'].value) then
      s := 'File Not Found'
    else if item['type'].value = 'snomed' then
      s := TSnomedServices.checkFile(item['source'].value)
    else if item['type'].value = 'loinc' then
      s := TLoincServices.checkFile(item['source'].value)
    else if item['type'].value = 'ucum' then
      s := TUcumServices.checkFile(item['source'].value)
    else if item['type'].value = 'lang' then
      s := TIETFLanguageCodeServices.checkFile(item['source'].value)
    else if item['type'].value = 'icd10' then
      s := TICD10Provider.checkFile(item['source'].value)
    else
      s := 'to do';
  end
  else
  begin
    try
      db := connectToDatabase(item);
      try
        conn := db.GetConnection('check');
        try
          if item['type'].value = 'rxnorm' then
            s := TRxNormServices.checkDB(conn)
          else if item['type'].value = 'ndc' then
            s := TNDCServices.checkDB(conn)
          else if item['type'].value = 'unii' then
            s := TUNIIServices.checkDB(conn)
          else
            s := 'to do';
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
        s := 'Error: '+e.message;
    end;
  end;
  (response as TTXStatusCheckResponse).Status := s;
end;

function TTXStatusChecker.name: String;
begin
  result := 'Terminology Status Checker';
end;

{ TTXStatusCheckResponse }

constructor TTXStatusCheckResponse.Create(item: TFHIRServerConfigSection);
begin
  Inherited Create;
  FItem := item;
end;

destructor TTXStatusCheckResponse.Destroy;
begin
  FItem.Free;
  inherited Destroy;
end;

{ TTXStatusCheckRequest }

constructor TTXStatusCheckRequest.Create(item: TFHIRServerConfigSection);
begin
  inherited Create;
  FItem := item;
end;

destructor TTXStatusCheckRequest.Destroy;
begin
  FItem.Free;
  inherited Destroy;
end;

{ TEPStatusCheckRequest }

constructor TEPStatusCheckRequest.Create(item: TFHIRServerConfigSection);
begin
  inherited Create;
  Fitem := item;
end;

destructor TEPStatusCheckRequest.Destroy;
begin
  FItem.Free;
  inherited;
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

{ TEndPointManager }

function TEndPointManager.status(item: TFHIRServerConfigSection): String;
begin
  if not hasDatabase(item['type'].value) then
    item.status := '-'
  else if (item.status = '') then
  begin
    item.status := 'Checking...';
    GBackgroundTasks.queueTask(FStatusTask, TEPStatusCheckRequest.create(item.clone), TEPStatusCheckResponse.create(item.link), doStatusCallback);
  end;
  result := item.status;
end;

procedure TEndPointManager.doStatusCallback(id: integer; response: TBackgroundTaskResponsePackage);
var
  resp : TEPStatusCheckResponse;
begin
  resp := response as TEPStatusCheckResponse;
  resp.FItem.status := resp.Status; // we're in the right thread for this now
  refresh(resp.FItem);
end;

constructor TEndPointManager.Create;
begin
  inherited Create;
  FStatusTask := GBackgroundTasks.registerTaskEngine(TEPStatusChecker.create);
end;

function TEndPointManager.canSort: boolean;
begin
  Result := true;
end;

function TEndPointManager.allowedOperations(item: TFHIRServerConfigSection): TNodeOperationSet;
begin
  if (item = nil) then
    result := [opAdd]
  else if hasDatabase(item['type'].value) then
    result := [opAdd, opEdit, opDelete, opExecute]
  else
    result := [opAdd, opEdit, opDelete];
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

procedure TEndPointManager.buildMenu;
begin
  registerMenuEntry('Add', 11, copAdd);
  registerMenuEntry('Delete', 12, copDelete);
  registerMenuEntry('Edit', 11, copEdit);
  registerMenuEntry('Install', 15, copExecute);
end;

function TEndPointManager.getCellText(item: TFHIRServerConfigSection; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item['type'].value;
    2: result := item['version'].value;
    3: result := BoolToStr(item['active'].valueBool, 'yes', 'no');
    4: result := item['path'].value;
    5: if hasDatabase(item['type'].value) then
         result := describeDatabase(item)
       else
         result := '';
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
    5: result := CompareStr(describeDatabase(left), describeDatabase(right));
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
    frm.Cfg := FFile.link;
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

function TEndPointManager.ExecuteItem(item: TFHIRServerConfigSection; mode : String) : boolean;
begin
  result := InstallEndPoint(list.Owner, FFile, item);
  if result then
    item.status := '';
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
    GBackgroundTasks.queueTask(FStatusTask, TTXStatusCheckRequest.create(item.clone), TTXStatusCheckResponse.create(item.link), doStatusCallback);
  end;
  result := item.status;
end;

procedure TTXManager.doStatusCallback(id: integer; response: TBackgroundTaskResponsePackage);
var
  resp : TTXStatusCheckResponse;
begin
  resp := response as TTXStatusCheckResponse;
  resp.FItem.status := resp.Status; // we're in the right thread for this now
  doLoad;
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

constructor TTXManager.Create;
begin
  inherited Create;
  FStatusTask := GBackgroundTasks.registerTaskEngine(TTXStatusChecker.create);
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

procedure TTXManager.buildMenu;
begin
  registerMenuEntry('Add', 11, copAdd);
  registerMenuEntry('Delete', 12, copDelete);
  registerMenuEntry('Edit', 11, copEdit);
  registerMenuEntry('Import', 13, copExecute);
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

function TTXManager.ExecuteItem(item: TFHIRServerConfigSection; mode: String) : boolean;
begin
  result := true;
  if (item['type'].value = 'ndc') then
    importNDC(item)
  else if (item['type'].value = 'rxnorm') then
    importRxNorm(item)
  else
    raise Exception.create('Not done yet');
end;


end.

