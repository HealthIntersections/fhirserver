unit ftk_frame_igpub;

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
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs, Graphics,
  LclIntf, Buttons,
  fsl_base, fsl_utilities, fsl_json, fsl_fetcher, fsl_threads,
  fui_lcl_managers, fsl_fpc,
  ftk_context, ftk_store_temp, ftk_worker_base, ftk_engine_igpub, dlg_igpub_config;

type
  TFrame = TBaseWorkerFrame;
  TIgPubPageFrame = class;
  TIGPublicationManager = class;

  TIGPublicationFolderStatus = (fsNotRun, fsRunning, fsSuccess, fsError);

  { TIGPublicationFolder }

  TIGPublicationFolder = class (TFslObject)
  private
    FManager : TIGPublicationManager;
    FEngine: TIgPublisherBuildEngine;
    FFolder: String;
    FLines: TStringList;
    Fname: String;
    FStatus: TIGPublicationFolderStatus;
    procedure SetEngine(AValue: TIgPublisherBuildEngine);

    procedure emitLine(line : String);
  public
    constructor Create(manager : TIGPublicationManager); overload;
    constructor Create(manager : TIGPublicationManager; name, folder : String); overload;
    destructor Destroy; override;
    function link : TIGPublicationFolder; overload;

    property status : TIGPublicationFolderStatus read FStatus write FStatus;

    procedure terminate;

    property name : String read Fname write FName;
    property folder : String read FFolder write FFolder;
    property lines : TStringList read FLines;
    property engine : TIgPublisherBuildEngine read FEngine write SetEngine;
  end;

  { TIGPublicationManager }

  TIGPublicationManager = class (TListManager<TIGPublicationFolder>)
  private
    FLock : TFslLock;
    FCurrent : TIGPublicationFolder;
    FCurrentLine : integer;

    FFrame : TIgPubPageFrame;
    FFolderList : TJsonArray; // initial list, will be used for reloading if necessary

    procedure updateMemo;
    procedure updateStatuses;
  public
    constructor Create; override;
    destructor Destroy; override;

    function doubleClickEdit : boolean; override;
    function AskOnDelete(item : TIGPublicationFolder) : boolean; override;
    function canSort : boolean; override;

    function allowedOperations(item : TIGPublicationFolder) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getImageIndex(item : TIGPublicationFolder) : integer; override;
    function getCellText(item : TIGPublicationFolder; col : integer) : String; override;
    function getCellColors(item : TIGPublicationFolder; col : integer; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TIGPublicationFolder) : String; override;

    procedure focusItemChange(item : TIGPublicationFolder); override;
    function addItem(mode : String) : TIGPublicationFolder; override;
    function addItems(mode : String) : TFslList<TIGPublicationFolder>; override;
    function deleteItem(item : TIGPublicationFolder) : boolean; override;
    function executeItem(item : TIGPublicationFolder; mode : String) : boolean; override;
    function stopItem(item : TIGPublicationFolder; mode : String) : boolean; override;
    function refreshItem(item : TIGPublicationFolder) : boolean; override;
  end;

  { TIgPubPageFrame }

  TIgPubPageFrame = class(TFrame)
    cbxTxServer: TComboBox;
    cbxVersions: TComboBox;
    ImageList1: TImageList;
    lvFolders: TListView;
    mStatus: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    fd: TSelectDirectoryDialog;
    Panel6: TPanel;
    Splitter1: TSplitter;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    tbAdd: TToolButton;
    tbDelete: TToolButton;
    tbCopy: TToolButton;
    tbAddSet: TToolButton;
    ToolButton2: TToolButton;
    tbBuild: TToolButton;
    tbUp: TToolButton;
    ToolButton4: TToolButton;
    tbStop: TToolButton;
    tbOpen: TToolButton;
    tbQA: TToolButton;
    tbDown: TToolButton;
    ToolButton8: TToolButton;
    tbConfig: TToolButton;
    procedure tbConfigClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
    procedure tbOpenClick(Sender: TObject);
    procedure tbQAClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FTempStore: TFHIRToolkitTemporaryStorage;
    FIgUrls : TStringList;
    FTxServers : TStringList;
    FJavaCmd : String;
    FDevParams : String;
    FCounter : integer;

    FManager : TIGPublicationManager;

    procedure SetTempStore(AValue: TFHIRToolkitTemporaryStorage);

    procedure listVersions(json : TJsonObject);
    procedure loadServers(json : TJsonObject);
    procedure loadFolders(json : TJsonObject);
    procedure updateButtons;
    procedure doReloadVersions(sender : TObject);
  protected
    procedure save(json : TJsonObject); override;
  public
    destructor Destroy; override;
    procedure init(json : TJsonObject); override;
    procedure saveStatus; override;
    procedure getFocus; override;

    property TempStore : TFHIRToolkitTemporaryStorage read FTempStore write SetTempStore;
  end;

implementation

{$R *.lfm}

uses
  frm_main;

const
  HOME_PAGE =
    '## Welcome to the FHIR Toolkit'+#13#10+#13#10+
    'The FHIR Toolkit allows you to edit and validate all sorts of files that are useful when working with FHIR content.'+#13#10+
    'In addition, you can connect to servers and explore them.</p>'+#13#10+
    ''+#13#10+
    '## User Help'+#13#10+
    'The best place to task for help is on the [tooling stream on chat.fhir.org](https://chat.fhir.org/#narrow/stream/179239-tooling).'+#13#10+
    'Note that you might be ''encouraged'' to contribute to the documentation '#$F609'. '+#13#10+
    ''+#13#10;

{ TIGPublicationManager }

procedure TIGPublicationManager.updateMemo;
var
  ts : TStringList;
  i : integer;
  b : boolean;
begin
  if FCurrent <> nil then
  begin
    ts := TStringlist.create;
    try
      FLock.Lock;
      try
        for i := FCurrentLine to FCurrent.lines.count - 1 do
          ts.add(FCurrent.lines[i]);
        FCurrentLine := FCurrent.lines.count;
      finally
        FLock.Unlock;
      end;
      if ts.count > 0 then
      begin
        FFrame.mStatus.lines.BeginUpdate;
        try
          for i := 0 to ts.count - 1 do
            FFrame.mStatus.lines.add(ts[i]);
        finally
          FFrame.mStatus.lines.EndUpdate;
        end;
        if ts.count > 0 then
          FFrame.mStatus.SelStart := length(FFrame.mStatus.Text);
      end;
    finally
      ts.free;
    end;
  end
  else if FFrame.mStatus.lines.count <> 0 then
    FFrame.mStatus.clear;
end;

procedure TIGPublicationManager.updateStatuses;
var
  o : TIGPublicationFolder;
begin
  for o in Data do
  begin
    if o.status = fsRunning then
    begin
      if not o.Engine.Running then
      begin
        if (o.engine.success) then
          o.status := fsSuccess
        else
          o.status := fsError;
        o.Engine.free;
        o.Engine := nil;
        refresh(o);
      end;
    end;
  end;
end;

constructor TIGPublicationManager.Create;
begin
  inherited Create;
  FLock := TFslLock.create('IG publisher');
end;

destructor TIGPublicationManager.Destroy;
begin
  FLock.Free;
  FFolderList.free;
  inherited Destroy;
end;

function TIGPublicationManager.doubleClickEdit: boolean;
begin
  Result := false;
end;

function TIGPublicationManager.AskOnDelete(item : TIGPublicationFolder): boolean;
begin
  Result := item.status = fsRunning;
end;

function TIGPublicationManager.canSort: boolean;
begin
  Result := false;
end;

function TIGPublicationManager.allowedOperations(item: TIGPublicationFolder): TNodeOperationSet;
begin
  if item = nil then
    result := [opAdd]
  else if item.status = fsRunning then
    result := [opAdd, opOrder, opDelete, opStop]
  else
    result := [opAdd, opOrder, opDelete, opExecute];
end;

function TIGPublicationManager.loadList: boolean;
var
  o : TJsonObject;
begin
  for o in FFolderList.asObjects.forEnum do
    Data.Add(TIGPublicationFolder.create(self, o['name'], o['folder']));
  result := true;
end;

function TIGPublicationManager.getImageIndex(item: TIGPublicationFolder): integer;
begin
  case item.status of
    fsNotRun : result := 0;
    fsRunning : result := 1;
    fsSuccess : result := 2;
    fsError : result := 3;
  else
    result := -1;
  end;
end;

function TIGPublicationManager.getCellText(item: TIGPublicationFolder; col: integer): String;
begin
  result := item.name;
end;

function TIGPublicationManager.getCellColors(item: TIGPublicationFolder; col: integer; var fore, back: TColor): boolean;
begin
  if not FolderExists(item.folder) then
  begin
    fore := clBlack;
    back := clRed;
  end
  else
    Result := inherited getCellColors(item, col, fore, back);
end;

function TIGPublicationManager.getSummaryText(item: TIGPublicationFolder): String;
begin
  Result := item.name+' ('+item.folder+')';
end;

procedure TIGPublicationManager.focusItemChange(item: TIGPublicationFolder);
begin
  FFrame.mStatus.Clear;
  FCurrent := item;
  FCurrentLine := 0;
end;

function TIGPublicationManager.addItem(mode: String): TIGPublicationFolder;
var
  name, folder : String;
begin
  result := nil;
  if FFrame.fd.execute then
  begin
    folder := FFrame.fd.filename;
    if not FileNameCaseSensitive then
      folder := folder.ToLower;
    name := ExtractFileName(folder);
    result := TIGPublicationFolder.create(self, name, folder);
    FFrame.FWorker.lastChange := GetTickCount64;
    FFrame.FWorker.lastChangeChecked := false;
    FFrame.FWorker.session.NeedsSaving := true;
  end;
end;

function TIGPublicationManager.addItems(mode: String): TFslList<TIGPublicationFolder>;
var
  name, s, folder : String;
begin
  result := nil;
  if FFrame.fd.execute then
  begin
    result := TFslList<TIGPublicationFolder>.create;
    for s in TDirectory.getDirectories(FFrame.fd.filename) do
    begin
      folder := s;
      if not FileNameCaseSensitive then
        folder := folder.ToLower;
      name := ExtractFileName(folder);
      result.add(TIGPublicationFolder.create(self, name, folder));
    end;
    FFrame.FWorker.lastChange := GetTickCount64;
    FFrame.FWorker.lastChangeChecked := false;
    FFrame.FWorker.session.NeedsSaving := true;
  end;
end;

function TIGPublicationManager.deleteItem(item: TIGPublicationFolder): boolean;
begin
  result := true;
  if item.status = fsRunning then
    item.engine.Terminate;

  FFrame.FWorker.lastChange := GetTickCount64;
  FFrame.FWorker.lastChangeChecked := false;
  FFrame.FWorker.session.NeedsSaving := true;
end;

function TIGPublicationManager.executeItem(item: TIGPublicationFolder; mode: String): boolean;
begin
  result := false;
  if (item.status = fsRunning ) then
    MessageDlg('Build IG', 'The IG '+item.name+' is already being built', mtError, [mbok], 0)
  else
  begin
    item.lines.clear;
    item.lines.add('Building '+item.name+'. Starting at '+TFslDateTime.makeLocal.toString('c'));
    item.engine := TIgPublisherBuildEngine.create;
    item.engine.folder := item.folder;
    item.engine.OnEmitLine := item.emitLine;
    item.engine.javaCmd := FFrame.FJavaCmd;
    item.engine.devParams := FFrame.FDevParams;
    item.engine.txSrvr := FFrame.FTxServers[FFrame.cbxTxServer.ItemIndex];
    item.engine.version := FFrame.cbxVersions.text;
    item.engine.url := FFrame.FIgUrls[FFrame.cbxVersions.ItemIndex];
    item.engine.Start;
    item.status := fsRunning;
    result := true;
  end;
end;

function TIGPublicationManager.stopItem(item: TIGPublicationFolder; mode: String): boolean;
begin
  result := false;
  if (item.status = fsRunning ) then
  begin
    item.engine.Terminate;
    item.status := fsError;
    result := true;
  end;
end;

function TIGPublicationManager.refreshItem(item: TIGPublicationFolder): boolean;
begin
  result := false; // nothing yet
end;

{ TIGPublicationFolder }

constructor TIGPublicationFolder.Create(manager : TIGPublicationManager);
begin
  inherited Create;
  FManager := manager; // no own
  FLines := TStringList.create;
end;

constructor TIGPublicationFolder.Create(manager : TIGPublicationManager; name, folder: String);
begin
  Create;
  FManager := manager; // no own
  self.name := name;
  self.folder := folder;
  FLines := TStringList.create;
end;

destructor TIGPublicationFolder.Destroy;
begin
  FLines.Free;
  FEngine.Free;
  inherited Destroy;
end;

function TIGPublicationFolder.link: TIGPublicationFolder;
begin
  result := TIGPublicationFolder(inherited Link);
end;

procedure TIGPublicationFolder.terminate;
begin
  // nothing yet
end;

procedure TIGPublicationFolder.SetEngine(AValue: TIgPublisherBuildEngine);
begin
  FEngine.Free;
  FEngine := AValue;
end;

procedure TIGPublicationFolder.emitLine(line: String);
begin
  FManager.FLock.Lock;
  try
    FLines.add(line);
  finally
    FManager.Flock.Unlock;
  end;
end;

{ TIgPubPageFrame }

destructor TIgPubPageFrame.Destroy;
begin
  FManager.Free;
  FTempStore.free;
  FIgUrls.Free;
  FTxServers.Free;
  inherited;
end;

procedure TIgPubPageFrame.saveStatus;
begin
  inherited;
end;

procedure TIgPubPageFrame.getFocus;
begin
  inherited GetFocus;
end;

procedure TIgPubPageFrame.init(json: TJsonObject);
begin
  FIgUrls := TStringList.create;
  FTxServers := TStringList.create;

  FManager := TIGPublicationManager.create;
  FManager.FFrame := self;

  FManager.registerControl(tbAdd, copAdd);
  FManager.registerControl(tbAddSet, copAddSet, 'set');
  FManager.registerControl(tbDelete, copDelete);
  FManager.registerControl(tbUp, copUp);
  FManager.registerControl(tbDown, copDown);
  FManager.registerControl(tbBuild, copExecute);
  FManager.registerControl(tbStop, copStop);

  FManager.registerMenuEntry('Add', 4, copAdd);
  FManager.registerMenuEntry('Delete', 12, copDelete);
  FManager.registerMenuEntry('Up', 11, copUp);
  FManager.registerMenuEntry('Down', 10, copDown);
  FManager.registerMenuEntry('-', -1, copNone);
  FManager.registerMenuEntry('Build', 1, copExecute);
  FManager.registerMenuEntry('Stop', 5, copStop);

  FManager.Images := ImageList1;
  FManager.list := lvFolders;

  cbxTxServer.items.clear;
  FJavaCmd := json.str['java-cmd'];
  if FJavaCmd = '' then
    FJavaCmd := 'java';
  FDevParams := json.str['dev-params'];
  listVersions(json);
  loadServers(json);
  loadFolders(json);
end;

procedure TIgPubPageFrame.tbConfigClick(Sender: TObject);
begin
  IGPublisherConfigForm := TIGPublisherConfigForm.create(self);
  try
    IGPublisherConfigForm.edtDevParams.Text := FDevParams;
    IGPublisherConfigForm.edtJavaCmd.Text := FJavaCmd;
    IGPublisherConfigForm.OnReloadVersionList := doReloadVersions;
    if IGPublisherConfigForm.ShowModal = mrOk then
    begin
      FJavaCmd := IGPublisherConfigForm.edtJavaCmd.Text;
      if FDevParams = '' then
      begin
        if IGPublisherConfigForm.edtDevParams.Text <> '' then
        begin
          FDevParams := IGPublisherConfigForm.edtDevParams.Text;
          cbxVersions.items.Insert(0, 'Dev');
          FIgUrls.Insert(0, '#dev');
        end;
      end
      else if IGPublisherConfigForm.edtDevParams.Text <> '' then
      begin
        // we had a dev cmd, and we still have one
        FDevParams := IGPublisherConfigForm.edtDevParams.Text;
      end
      else
      begin
        // no dev command now
        cbxVersions.items.Delete(0);
        FIgUrls.Delete(0);
      end;
      FWorker.lastChange := GetTickCount64;
      FWorker.lastChangeChecked := false;
      FWorker.session.NeedsSaving := true;
    end;
  finally
    IGPublisherConfigForm.free;
  end;
end;

procedure TIgPubPageFrame.tbCopyClick(Sender: TObject);
begin
  mStatus.SelectAll;
  mStatus.CopyToClipboard;
  mStatus.SelStart := length(mStatus.Text);
end;

procedure TIgPubPageFrame.tbOpenClick(Sender: TObject);
begin
  if FManager.FCurrent <> nil then
    OpenDocument(FilePath([FManager.FCurrent.FFolder, 'output', 'index.html']));
end;

procedure TIgPubPageFrame.tbQAClick(Sender: TObject);
begin
  if FManager.FCurrent <> nil then
    OpenDocument(FilePath([FManager.FCurrent.FFolder, 'output', 'qa.html']));
end;

procedure TIgPubPageFrame.Timer1Timer(Sender: TObject);
begin
  FManager.updateStatuses;
  FManager.updateMemo;
  inc(FCounter);
  if (FCounter mod 20 = 0) then
  begin
    FCounter := 0;
    updateButtons;
  end;
end;

procedure TIgPubPageFrame.SetTempStore(AValue: TFHIRToolkitTemporaryStorage);
begin
  FTempStore.Free;
  FTempStore := AValue;
end;

procedure TIgPubPageFrame.listVersions(json : TJsonObject);
var
  arr : TJsonArray;
  o : TJsonObject;
begin
  cbxVersions.items.clear;
  if FDevParams <> '' then
  begin
    cbxVersions.items.add('Dev');
    FIgUrls.add('#dev');
  end;

  if (json <> nil) and json.has('ig-pub-versions') then
  begin
    arr := json.arr['ig-pub-versions'];
    for o in arr.asObjects.forEnum do
    begin
      cbxVersions.items.add(o.str['version']);
      FIgUrls.add(o.str['url']);
    end;
    cbxVersions.itemIndex := json.int['ig-pub-version'];
  end
  else
  begin
    try
      arr := TInternetFetcher.fetchJsonArray('https://api.github.com/repos/HL7/fhir-ig-publisher/releases');
      try
        for o in arr.asObjects.forEnum do
        begin
          cbxVersions.items.add(o.str['tag_name']);
          FIgUrls.add(o.forceArr['assets'].Obj[0].str['browser_download_url']);
        end;
        FWorker.lastChange := GetTickCount64;
        FWorker.lastChangeChecked := false;
        FWorker.session.NeedsSaving := true;
        cbxVersions.itemIndex := 0;
      finally
        arr.free;
      end;
    except
      on e : Exception do
        raise EFslException.create('Unable to fetch IG publisher versions from https://api.github.com/repos/HL7/fhir-ig-publisher/releases: '+e.message);
    end;
  end;
end;

procedure TIgPubPageFrame.loadServers(json: TJsonObject);
var
  arr : TJsonArray;
  o : TJsonObject;
begin
  if json.has('tx-servers') then
  begin
    for o in json.arr['tx-servers'].asObjects.forEnum do
    begin
      cbxTxServer.items.add(o['name']);
      FTxServers.add(o['address']);
    end;
  end
  else
  begin
    cbxTxServer.items.add('tx.fhir.org');
    FTxServers.add('http://tx.fhir.org');
    cbxTxServer.items.add('localhost');
    FTxServers.add('http://local.fhir.org:960');
  end;
  cbxTxServer.itemIndex := json.int['tx-server'];
end;

procedure TIgPubPageFrame.loadFolders(json: TJsonObject);
var
  arr : TJsonArray;
  i : integer;
begin
  arr := json.forceArr['igs'];
  FManager.FFolderList := arr.link;
  FManager.doLoad;
end;

procedure TIgPubPageFrame.updateButtons;
begin
  if FManager.FCurrent = nil then
  begin
    tbOpen.enabled := false;
    tbQA.enabled := false;
  end
  else
  begin
    tbOpen.enabled := FileExists(FilePath([FManager.FCurrent.FFolder, 'output', 'index.html']));
    tbQA.enabled := FileExists(FilePath([FManager.FCurrent.FFolder, 'output', 'qa.html']));
  end;
end;

procedure TIgPubPageFrame.doReloadVersions(sender: TObject);
begin
  listVersions(nil);
end;

procedure TIgPubPageFrame.save(json: TJsonObject);
var
  i : integer;
  v : TJsonObject;
  arr : TJsonArray;
begin
  json.str['igpub-page'] := 'true';
  json.str['java-cmd'] := FJavaCmd;
  json.str['dev-params'] := FDevParams;
  arr := json.forceArr['ig-pub-versions'];
  arr.clear;
  for i := 0 to cbxVersions.items.count - 1 do
  begin
    if FIgUrls[i] <> '#dev' then
    begin
      v := TJsonObject.create;
      arr.add(v);
      v.str['version'] := cbxVersions.items[i];
      v.str['url'] := FIgUrls[i];
    end;
  end;
  json.int['ig-pub-version'] := cbxVersions.itemIndex;

  arr := json.forceArr['tx-servers'];
  arr.clear;
  for i := 0 to cbxTxServer.items.count - 1 do
  begin
    v := TJsonObject.create;
    arr.add(v);
    v.str['name'] := cbxTxServer.items[i];
    v.str['address'] := FTxServers[0];
  end;
  json.int['tx-server'] := cbxTxServer.itemIndex;

  arr := json.forceArr['igs'];
  arr.clear;
  for i := 0 to FManager.Data.count - 1 do
  begin
    v := TJsonObject.create;
    arr.add(v);
    v.str['name'] := FManager.Data[i].name;
    v.str['folder'] := FManager.Data[i].folder;
  end;
end;


end.

