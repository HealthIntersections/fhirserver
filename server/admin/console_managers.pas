unit console_managers;

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
  SysUtils, Classes, Graphics, System.UITypes,
  Dialogs,
  fsl_base, fsl_threads, fsl_utilities,
  fdb_manager,
  fhir_colour_utils,
  ftx_sct_services, ftx_loinc_services, ftx_ucum_services, ftx_lang,
  fui_lcl_managers, fui_lcl_progress,
  tx_ndc, tx_rxnorm, tx_unii,
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
    property configFile : TFHIRServerConfigFile read FFile write SetFile;
  end;

  { TTXStatusCheckRequest }

  TTXStatusCheckRequest = class (TBackgroundTaskRequestPackage)
  private
    Fitem : TFHIRServerConfigSection;
  public
    constructor Create(item : TFHIRServerConfigSection);
    destructor Destroy; override;
    function description : String; override;
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
  protected
    function canCancel : boolean; override;
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

    function editItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
    function addItem(mode : String) : TFHIRServerConfigSection; override;
    function deleteItem(item : TFHIRServerConfigSection) : boolean; override;
    function executeItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
  end;

  { TEPStatusCheckRequest }

  TEPStatusCheckRequest = class (TBackgroundTaskRequestPackage)
  private
    Fitem : TFHIRServerConfigSection;
  public
    constructor Create(item : TFHIRServerConfigSection);
    destructor Destroy; override;
    function description : String; override;
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
  protected
    function canCancel : boolean; override;
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

    function editItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
    function addItem(mode : String) : TFHIRServerConfigSection; override;
    function deleteItem(item : TFHIRServerConfigSection) : boolean; override;
    function executeItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
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

    function editItem(item : TFHIRServerConfigSection; mode : String) : boolean; override;
    function addItem(mode : String) : TFHIRServerConfigSection; override;
    function deleteItem(item : TFHIRServerConfigSection) : boolean; override;
  end;


implementation

uses
  console_form;

{ TEPStatusChecker }

function TEPStatusChecker.canCancel: boolean;
begin
  result := false;
end;

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
  FItem.free;
  inherited Destroy;
end;

{ TTXStatusChecker }

function TTXStatusChecker.canCancel: boolean;
begin
  result := false;
end;

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
    else
      s := 'to do';
  end
  else
  begin
    try
      db := connectToDatabase(item, true);
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
  FItem.free;
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
  FItem.free;
  inherited Destroy;
end;

function TTXStatusCheckRequest.description: String;
begin
  result := '';
end;

{ TEPStatusCheckRequest }

constructor TEPStatusCheckRequest.Create(item: TFHIRServerConfigSection);
begin
  inherited Create;
  Fitem := item;
end;

destructor TEPStatusCheckRequest.Destroy;
begin
  FItem.free;
  inherited;
end;

function TEPStatusCheckRequest.description: String;
begin
  result := '';
end;

{ TAdminManager }

procedure TAdminManager.SetFile(value: TFHIRServerConfigFile);
begin
  FFile.free;
  FFile := value;
  Enabled := FFile <> nil
end;

destructor TAdminManager.Destroy;
begin
  FFile.free;
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

function TIdentityProviderManager.editItem(item: TFHIRServerConfigSection; mode: String): boolean;
var
  frm : TEditIdForm;
begin
  frm := TEditIdForm.Create(List.Owner);
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

function TIdentityProviderManager.addItem(mode: String): TFHIRServerConfigSection;
var
  frm : TEditIdForm;
begin
  Result := nil;
  frm := TEditIdForm.Create(List.Owner);
  try
    frm.Id := TFHIRServerConfigSection.Create('id'+inttostr(FFile['identity-providers'].sections.Count));
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

function TIdentityProviderManager.deleteItem(item: TFHIRServerConfigSection) : boolean;
begin
  FFile['identity-providers'].remove(item.name);
  FFile.Save;
  result := false;
end;

{ TEndPointManager }

function TEndPointManager.status(item: TFHIRServerConfigSection): String;
begin
  if not hasDatabase(item['type'].value) then
    item.status := '-'
  else if (item.status = '') then
  begin
    item.status := 'Checking...';
    GBackgroundTasks.queueTask(FStatusTask, TEPStatusCheckRequest.Create(item.clone), TEPStatusCheckResponse.Create(item.link), doStatusCallback);
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
       else if hasSrcFolder(item['type'].value) then
         result := item['folder'].value;
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

function TEndPointManager.editItem(item: TFHIRServerConfigSection; mode: String): boolean;
var
  frm : TEditEPForm;
begin
  frm := TEditEPForm.Create(List.Owner);
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

function TEndPointManager.addItem(mode: String): TFHIRServerConfigSection;
var
  frm : TEditEPForm;
begin
  Result := nil;
  frm := TEditEPForm.Create(List.Owner);
  try
    frm.EP := TFHIRServerConfigSection.Create('EP'+inttostr(FFile['endpoints'].sections.Count));
    frm.EP['path'].value := '/path';
    frm.Cfg := FFile.link;
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

function TEndPointManager.deleteItem(item: TFHIRServerConfigSection) : boolean;
begin
  FFile['endpoints'].remove(item.name);
  FFile.Save;
  result := true;
end;

function TEndPointManager.executeItem(item: TFHIRServerConfigSection; mode : String) : boolean;
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
    GBackgroundTasks.queueTask(FStatusTask, TTXStatusCheckRequest.Create(item.clone), TTXStatusCheckResponse.Create(item.link), doStatusCallback);
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
  dlg := TSelectDirectoryDialog.Create(List.Owner);
  try
    dlg.Filename := Settings.readString('ndc', 'source', '');
    if dlg.execute then
    begin
      Settings.writeString('ndc', 'source', dlg.Filename);
      db := connectToDatabase(item, false);
      try
        conn := db.GetConnection('check');
        try
          ndc := TNdcImporter.Create(dlg.FileName, conn.link);
          try
            DoForegroundTask(List.Owner, nil, ndc.Doinstall);
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
    dlg.free;
  end;
end;

procedure TTXManager.importRxNorm(item: TFHIRServerConfigSection);
var
  db : TFDBManager;
  conn : TFDBConnection;
  dlg : TSelectDirectoryDialog;
  umls : TUMLSImporter;
begin
  dlg := TSelectDirectoryDialog.Create(List.Owner);
  try
    dlg.Filename := Settings.readString('rxnorm', 'source', '');
    if dlg.execute then
    begin
      Settings.writeString('rxnorm', 'source', dlg.Filename);
      db := connectToDatabase(item, false);
      try
        conn := db.GetConnection('check');
        try
          umls := TUMLSImporter.Create(dlg.FileName, conn.link);
          try
            DoForegroundTask(List.Owner, nil, umls.Doinstall);
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
    dlg.free;
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

function TTXManager.editItem(item: TFHIRServerConfigSection; mode: String): boolean;
var
  frm : TEditTxForm;
begin
  frm := TEditTxForm.Create(List.Owner);
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

function TTXManager.addItem(mode: String): TFHIRServerConfigSection;
var
  frm : TEditTxForm;
begin
  Result := nil;
  frm := TEditTxForm.Create(List.Owner);
  try
    frm.Tx := TFHIRServerConfigSection.Create('tx'+inttostr(FFile['terminologies'].sections.Count));
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

function TTXManager.deleteItem(item: TFHIRServerConfigSection) : boolean;
begin
  FFile['terminologies'].remove(item.name);
  FFile.Save;
  result := true;
end;

function TTXManager.executeItem(item: TFHIRServerConfigSection; mode: String) : boolean;
begin
  result := true;
  if (item['type'].value = 'ndc') then
    importNDC(item)
  else if (item['type'].value = 'rxnorm') then
    importRxNorm(item)
  else
    raise EFslException.Create('Not done yet');
end;


end.

