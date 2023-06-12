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
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs, Graphics, Process,
  LclIntf, Buttons, Menus,
  fsl_base, fsl_utilities, fsl_json, fsl_fetcher, fsl_threads, fsl_fpc, fsl_versions, fsl_stream,
  fui_lcl_managers, fui_lcl_progress, fui_lcl_utilities,
  ftk_context, ftk_store_temp, ftk_worker_base, ftk_engine_igpub, dlg_igpub_config, dlg_igpub_github;

const
  {$IFDEF WINDOWS}
  TEXT_CMDPROMPT_NAME = 'Command Prompt here';
  {$ELSE}
  TEXT_CMDPROMPT_NAME = 'Terminal here';
  {$ENDIF}

type
  TFrame = TBaseWorkerFrame;
  TIgPubPageFrame = class;
  TIGPublicationManager = class;

  { TIgPublisherVersion }

  TIgPublisherVersion = class (TFslObject)
  private
    FUrl: String;
    FVersion: String;
  public
    constructor Create(version, url : String);
    function link : TIgPublisherVersion; overload;
    property version : String read FVersion write FVersion;
    property url : String read FUrl write FUrl;
  end;

  TIGPublicationFolderStatus = (fsNotRun, fsRunning, fsSuccess, fsError);

  { TIGPublicationFolder }

  TIGPublicationFolder = class (TFslObject)
  private
    FLineCount: integer;
    FManager : TIGPublicationManager;
    FEngine: TIgPublisherBuildBaseEngine;
    FFolder: String;
    FLines: TStringList;
    Fname: String;
    FRunLength: Int64;
    FStartRun: Int64;
    FStatus: TIGPublicationFolderStatus;
    procedure SetEngine(AValue: TIgPublisherBuildBaseEngine);

    procedure emitLine(line : String; repl : boolean);
    function log : String;
    function ghURL : String;
    procedure inspect(ts : TStringList);
  public
    constructor Create(manager : TIGPublicationManager); overload;
    constructor Create(manager : TIGPublicationManager; name, folder : String; RunLength, LineCount : integer); overload;
    destructor Destroy; override;
    function link : TIGPublicationFolder; overload;

    property status : TIGPublicationFolderStatus read FStatus write FStatus;

    procedure terminate;

    property name : String read Fname write FName;
    property folder : String read FFolder write FFolder;
    property lines : TStringList read FLines;
    property engine : TIgPublisherBuildBaseEngine read FEngine write SetEngine;
    property RunLength : Int64 read FRunLength write FRunLength;
    property LineCount : integer read FLineCount write FLineCount;
    property StartRun : Int64 read FStartRun write FStartRun;
  end;

  { TDeleteTaskContext }

  TDeleteTaskContext = class (TFslObject)
  private
    FDeleteDisk: boolean;
    FFolder: TIGPublicationFolder;
    FKillFirst: boolean;
    procedure SetFolder(AValue: TIGPublicationFolder);
  public
    destructor Destroy; override;

    property folder : TIGPublicationFolder read FFolder write SetFolder;
    property killFirst : boolean read FKillFirst write FKillFirst;
    property deleteDisk : boolean read FDeleteDisk write FDeleteDisk;
  end;

  { TIGPublicationManager }

  TIGPublicationManager = class (TListManager<TIGPublicationFolder>)
  private
    FLock : TFslLock;
    FCurrent : TIGPublicationFolder;
    FCurrentLine : integer;

    FFrame : TIgPubPageFrame;
    FFolderList : TJsonArray; // initial list, will be used for reloading if necessary

    procedure deleteFiles(folder: String; i: integer; progress: TWorkProgressEvent);
    procedure ScanForFolders(ts: TStringList; folder: String);
    procedure updateMemo;
    procedure updateStatuses;
    procedure doDeleteItem(sender : TObject; context : TObject; progress : TWorkProgressEvent);
  public
    constructor Create; override;
    destructor Destroy; override;

    function doubleClickEdit : boolean; override;
    function AskOnDelete(item : TIGPublicationFolder) : boolean; override;
    function canSort : boolean; override;
    procedure getCopyModes(modes : TStringList); override;

    function allowedOperations(item : TIGPublicationFolder) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getImageIndex(item : TIGPublicationFolder) : integer; override;
    function getCellText(item : TIGPublicationFolder; col : integer) : String; override;
    function getCellColors(item : TIGPublicationFolder; col : integer; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TIGPublicationFolder) : String; override;
    function getCanCopy(item : TIGPublicationFolder; mode : String) : boolean; override;
    function getCopyValue(item : TIGPublicationFolder; mode : String) : String; override;

    procedure focusItemChange(item : TIGPublicationFolder); override;
    function addItem(mode : String) : TIGPublicationFolder; override;
    function addItems(mode : String) : TFslList<TIGPublicationFolder>; override;
    function deleteItem(item : TIGPublicationFolder) : boolean; override;
    function executeItem(item : TIGPublicationFolder; mode : String) : boolean; override;
    function stopItem(item : TIGPublicationFolder; mode : String) : boolean; override;
    function refreshItem(item : TIGPublicationFolder) : boolean; override;
    function updateItem(item : TIGPublicationFolder; mode : String) : TIGPublicationFolder; override;
    procedure itemInfo(item : TIGPublicationFolder; mode : String); override;
  end;

  { TIgPubPageFrame }

  TIgPubPageFrame = class(TFrame)
    cbxTxServer: TComboBox;
    cbxVersions: TComboBox;
    ImageList1: TImageList;
    lvFolders: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuAddFolder: TMenuItem;
    mnuAddGit: TMenuItem;
    mnuAddFolders: TMenuItem;
    mnuClean: TMenuItem;
    mnuClearTx: TMenuItem;
    mnuJekyll: TMenuItem;
    mnuCommand: TMenuItem;
    mnuFolder: TMenuItem;
    mStatus: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    fd: TSelectDirectoryDialog;
    Panel6: TPanel;
    Panel7: TPanel;
    pmDev: TPopupMenu;
    pmAdd: TPopupMenu;
    ProgressBar1: TProgressBar;
    Splitter1: TSplitter;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    tbAdd: TToolButton;
    tbDelete: TToolButton;
    tbDevTools: TToolButton;
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
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure mnuCleanClick(Sender: TObject);
    procedure mnuClearTxClick(Sender: TObject);
    procedure mnuCommandClick(Sender: TObject);
    procedure mnuFolderClick(Sender: TObject);
    procedure mnuJekyllClick(Sender: TObject);
    procedure tbConfigClick(Sender: TObject);
    procedure tbOpenClick(Sender: TObject);
    procedure tbQAClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FTempStore: TFHIRToolkitTemporaryStorage;
    FIgPublisherVersions : TFslList<TIgPublisherVersion>;
    FJavaCmd : String;
    FDevParams : String;
    FCounter : integer;
    FDefaultRootFolder : String;

    FManager : TIGPublicationManager;

    procedure SetTempStore(AValue: TFHIRToolkitTemporaryStorage);

    procedure listVersions(json : TJsonObject);
    procedure loadFolders(json : TJsonObject);
    procedure loadServers;
    procedure updateButtons;
    procedure doReloadVersions(sender : TObject);
    procedure ClearDirectory(dir : String);
  protected
    procedure save(json : TJsonObject); override;
    procedure updateSettings;  override;
    procedure inspect; override;
  public
    destructor Destroy; override;
    procedure init(json : TJsonObject); override;
    procedure saveStatus; override;
    procedure getFocus; override;
    procedure changeToolbarButtons; override;

    property TempStore : TFHIRToolkitTemporaryStorage read FTempStore write SetTempStore;
  end;

implementation

uses
  frm_main;

{$R *.lfm}

function jekyllCommand(s : String; out cmd, folder : String) : boolean;
var
  i : integer;
  l, r : String;
begin
  cmd := '';
  folder := '';
  i := s.indexOf('Run jekyll:');
  if i = 0 then
    result := false
  else
  begin
    result := true;
    s := s.substring(i+12);
    i := s.indexOf(#13#10);
    s := s.substring(0, i);
    StringSplit(s, '(in folder', l, r);
    i := r.indexOf(')');
    r := r.subString(0, i);
    cmd := l.trim;
    folder := r.trim;
  end;
end;

{ TIgPublisherVersion }

constructor TIgPublisherVersion.Create(version, url: String);
begin
  inherited Create;
  self.version := version;
  self.url := url;
end;

function TIgPublisherVersion.link: TIgPublisherVersion;
begin
  result := TIgPublisherVersion(inherited link);
end;

{ TDeleteTaskContext }

procedure TDeleteTaskContext.SetFolder(AValue: TIGPublicationFolder);
begin
  FFolder.Free;
  FFolder := AValue;
end;

destructor TDeleteTaskContext.Destroy;
begin
  FFolder.Free;
  inherited Destroy;
end;


{ TIGPublicationManager }

procedure TIGPublicationManager.updateMemo;
var
  ts : TStringList;
  i : integer;
  b : boolean;
  pctTime, pctLines, pct : integer;
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
      if (FCurrent.status <> fsRunning) or not FCurrent.engine.trackBuildTime then
      begin
        FFrame.ProgressBar1.Enabled := false;
        FFrame.ProgressBar1.Position := 0;
      end
      else
      begin
        pctTime := 0;
        pctLines := 0;
        if FCurrent.RunLength > 0 then
          pctTime := trunc(((GetTickCount64 - FCurrent.StartRun) / FCurrent.RunLength) * 100);
        if FCurrent.LineCount > 0 then
          pctLines := trunc((FCurrent.lines.count / FCurrent.LineCount) * 100);
        FFrame.ProgressBar1.Enabled := true;
        FFrame.ProgressBar1.Position := (pctTime + pctLines) div 2;
      end;
      if ts.count > 0 then
      begin
        FFrame.mStatus.lines.BeginUpdate;
        try
          for i := 0 to ts.count - 1 do
            if ts[i].StartsWith('t') and (FFrame.mStatus.lines.count > 0) then
              FFrame.mStatus.lines[FFrame.mStatus.lines.count - 1] := ts[i].Substring(1)
            else
              FFrame.mStatus.lines.add(ts[i].Substring(1));
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
        begin
          o.status := fsSuccess;
          if o.engine.trackBuildTime then
            o.RunLength := GetTickCount64 - o.StartRun;
          o.lineCount := o.lines.count;
          FFrame.FWorker.lastChange := GetTickCount64;
          FFrame.FWorker.lastChangeChecked := false;
          FFrame.FWorker.session.NeedsSaving := true;
        end
        else
          o.status := fsError;
        o.Engine := nil;
        refresh(o);
      end;
    end;
  end;
end;

procedure TIGPublicationManager.ScanForFolders(ts : TStringList; folder : String);
var
  s : String;
begin
  ts.add(folder);
  for s in TDirectory.getDirectories(folder) do
    ScanForFolders(ts, s);
end;

procedure TIGPublicationManager.deleteFiles(folder : String; i : integer; progress: TWorkProgressEvent);
var
  s : String;
begin
  for s in TDirectory.GetFiles(folder) do
  begin
    progress(self, i, false, 'Delete Files  '+folder);
    {$IFDEF WINDOWS}
    FileSetReadOnly(s, false);
    {$ENDIF}
    deleteFile(s);
  end;
end;

procedure TIGPublicationManager.doDeleteItem(sender: TObject; context : TObject; progress: TWorkProgressEvent);
var
  deleteTask : TDeleteTaskContext;
  ts : TStringList;
  i : integer;
begin
  deleteTask := context as TDeleteTaskContext;
  if (deleteTask.killFirst) then
  begin
    deleteTask.folder.engine.Terminate;
    while (deleteTask.folder.engine <> nil) and (deleteTask.folder.engine.Running) do
      progress(self, 0, false, 'Terminating build before deleting');
  end;
  if (deleteTask.deleteDisk) then
  begin
    progress(self, 0, false, 'Scanning Folder '+deleteTask.folder.folder);
    ts := TStringList.Create;
    try
      ScanForFolders(ts, deleteTask.folder.folder);
      for i := 0 to ts.count - 1 do
      begin
        progress(self, trunc(i/ts.count * 100), false, 'Delete Files in '+ts[(ts.count - 1)- i]);
        deleteFiles(ts[(ts.count - 1)- i], trunc(i/ts.count * 100), progress);
        FolderDelete(ts[(ts.count - 1)- i]);
      end;
      progress(self, 100, false, 'Delete Folder');
      FolderDelete(deleteTask.folder.folder);
      progress(self, 100, true, 'Delete Files in '+deleteTask.folder.folder);
    finally
      ts.Free;
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
  Result := false;
end;

function TIGPublicationManager.canSort: boolean;
begin
  Result := false;
end;

procedure TIGPublicationManager.getCopyModes(modes: TStringList);
begin
  inherited getCopyModes(modes);
  modes.AddPair('path', 'Path');
  modes.AddPair('log', 'Log');
  modes.AddPair('gh', 'GitHub URL');
  modes.AddPair('jekyll', 'Jekyll Command');
end;

function TIGPublicationManager.getCanCopy(item: TIGPublicationFolder; mode: String): boolean;
var
  s, c, f : String;
begin
  if item = nil then
    Result := false
  else if mode = 'gh' then
    Result := FolderExists(FilePath([item.FFolder, '.git']))
  else
  begin
    s := item.log;
    if mode = 'log' then
      Result := s <> ''
    else if mode = 'jekyll' then
      Result := jekyllCommand(s, c, f)
    else
      Result := true;
  end;
end;

function TIGPublicationManager.allowedOperations(item: TIGPublicationFolder): TNodeOperationSet;
begin
  if item = nil then
    result := [opAdd]
  else if item.status = fsRunning then
    result := [opAdd, opOrder, opInfo, opStop]
  else
  begin
    result := [opAdd, opOrder, opDelete, opExecute, opInfo];
    if FolderExists(FilePath([item.folder, '.git'])) then
      result := result + [opUpdate];
  end;
end;

function TIGPublicationManager.loadList: boolean;
var
  o : TJsonObject;
begin
  for o in FFolderList.asObjects.forEnum do
    Data.Add(TIGPublicationFolder.create(self, o['name'], o['folder'], o.int['run-length'], o.int['line-count']));
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

function TIGPublicationManager.getCopyValue(item: TIGPublicationFolder; mode: String): String;
var
  c, f : String;
begin
  if (mode = 'path') then
    result := item.folder
  else if (mode = 'log') then
    result := item.log
  else if (mode = 'jekyll') then
  begin
    if jekyllCommand(item.log, c, f) then
      result := c+' in '+f
    else
      result := ''
  end
  else if (mode = 'gh') then
    result := item.ghURL
  else
    result := inherited getCopyValue(item, mode);
end;

procedure TIGPublicationManager.focusItemChange(item: TIGPublicationFolder);
var
  ts : TStringList;
begin
  FFrame.mStatus.Clear;
  FCurrent := item;
  FCurrentLine := 0;

  ts := TStringList.create;
  try
    if (item <> nil) then
      item.inspect(ts);
    FFrame.Context.Inspector.Populate(ts);
  finally
    ts.Free;
  end;
end;

function TIGPublicationManager.addItem(mode: String): TIGPublicationFolder;
var
  name, folder : String;
begin
  result := nil;
  if (mode = 'git') then
  begin
    IgGitHubDialog := TIgGitHubDialog.create(FFrame);
    try
      IgGitHubDialog.edtFolder.Text := FFrame.FDefaultRootFolder;
      if IgGitHubDialog.ShowModal = mrOk then
      begin
        FFrame.FDefaultRootFolder := IgGitHubDialog.edtFolder.Text;
        name := IgGitHubDialog.edtLocalFolder.Text;
        folder := FilePath([FFrame.FDefaultRootFolder, name]);
        result := TIGPublicationFolder.create(self, name, folder, 0, 0);
        FFrame.FWorker.lastChange := GetTickCount64;
        FFrame.FWorker.lastChangeChecked := false;
        FFrame.FWorker.session.NeedsSaving := true;
      end;
    finally
      IgGitHubDialog.free;
    end;
  end
  else
  begin
    if FFrame.fd.execute then
    begin
      folder := FFrame.fd.filename;
      if not FileNameCaseSensitive then
        folder := folder.ToLower;
      name := ExtractFileName(folder);
      result := TIGPublicationFolder.create(self, name, folder, 0, 0);
      FFrame.FWorker.lastChange := GetTickCount64;
      FFrame.FWorker.lastChangeChecked := false;
      FFrame.FWorker.session.NeedsSaving := true;
    end;
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
      result.add(TIGPublicationFolder.create(self, name, folder, 0, 0));
    end;
    FFrame.FWorker.lastChange := GetTickCount64;
    FFrame.FWorker.lastChangeChecked := false;
    FFrame.FWorker.session.NeedsSaving := true;
  end;
end;

function TIGPublicationManager.deleteItem(item: TIGPublicationFolder): boolean;
var
  res : TModalResult;
  ctxt : TDeleteTaskContext;
begin
  if item.status = fsRunning then
    res := MessageDlg('Delete IG', 'Cancel build and Delete '+item.name+'. Delete underlying source from disk?', mtConfirmation, mbYesNoCancel, 0)
  else
    res := MessageDlg('Delete IG', 'Delete '+item.name+'. Delete underlying source from disk?', mtConfirmation, mbYesNoCancel, 0);
  result := res <> mrCancel;
  if result then
  begin
    ctxt := TDeleteTaskContext.create;
    try
      ctxt.folder := item.link;
      ctxt.killFirst := item.status = fsRunning;
      ctxt.deleteDisk := res = mrYes;
      if ctxt.killFirst or ctxt.deleteDisk then
        DoForegroundTask(FFrame, ctxt, doDeleteItem);
      if ctxt.deleteDisk and (FolderExists(item.folder)) then
        MessageDlg('Delete IG', item.name+' could not be deleted from the disk entirely (usually due to a file being in use)', mtInformation, [mbOK], 0);
    finally
      ctxt.free;
    end;
    FFrame.FWorker.lastChange := GetTickCount64;
    FFrame.FWorker.lastChangeChecked := false;
    FFrame.FWorker.session.NeedsSaving := true;
  end;
end;

function TIGPublicationManager.executeItem(item: TIGPublicationFolder; mode: String): boolean;
var
  engine : TIgPublisherBuildEngine;
begin
  result := false;
  if (item.status = fsRunning ) then
    MessageDlg('Build IG', 'The IG '+item.name+' is being built or updated', mtError, [mbok], 0)
  else
  begin
    item.lines.clear;
    item.lines.add('fBuilding '+item.name+'. Starting at '+TFslDateTime.makeLocal.toString('c'));
    engine := TIgPublisherBuildEngine.create;
    item.engine := engine;
    engine.folder := item.folder;
    engine.OnEmitLine := item.emitLine;
    engine.javaCmd := FFrame.FJavaCmd;
    engine.devParams := FFrame.FDevParams;
    engine.txSrvr := (FFrame.cbxTxServer.items.objects[FFrame.cbxTxServer.ItemIndex] as TToolkitContextTerminologyServer).address;
    engine.version := FFrame.cbxVersions.text;
    engine.url := (FFrame.cbxVersions.items.Objects[FFrame.cbxVersions.ItemIndex] as TIgPublisherVersion).FUrl;
    engine.Start;
    item.status := fsRunning;
    item.StartRun := GetTickCount64;
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

function TIGPublicationManager.updateItem(item: TIGPublicationFolder; mode : String): TIGPublicationFolder;
begin
  result := nil;
  if (item.status = fsRunning ) then
    MessageDlg('Update IG', 'The IG '+item.name+' is being built or updated', mtError, [mbok], 0)
  else
  begin
    item.lines.clear;
    item.engine := TIgPublisherUpdateEngine.create;
    item.engine.folder := item.folder;
    item.engine.OnEmitLine := item.emitLine;
    item.engine.Start;
    item.StartRun := 0;
    item.status := fsRunning;
    result := item;
  end;
end;

procedure TIGPublicationManager.itemInfo(item: TIGPublicationFolder; mode: String);
begin
  if mode = 'terminal' then
    FFrame.mnuCommandClick(self)
  else if mode = 'project' then
    FFrame.MenuItem2Click(self)
  else if mode = 'clean' then
    FFrame.mnuCleanClick(self)
  else if mode = 'clear' then
    FFrame.mnuClearTxClick(self)
  else if mode = 'jekyll' then
    FFrame.mnuJekyllClick(self)
  else if mode = 'folder' then
    FFrame.mnuFolderClick(self)
  else if mode = 'file' then
    FFrame.MenuItem1Click(self)
  else if mode <> 'group' then
    inherited itemInfo(item, mode);
end;

{ TIGPublicationFolder }

constructor TIGPublicationFolder.Create(manager : TIGPublicationManager);
begin
  inherited Create;
  FManager := manager; // no own
  FLines := TStringList.create;
end;

constructor TIGPublicationFolder.Create(manager : TIGPublicationManager; name, folder: String; RunLength, LineCount : integer);
begin
  Create;
  FManager := manager; // no own
  self.name := name;
  self.folder := folder;
  self.RunLength := RunLength;
  self.LineCount := LineCount;
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

procedure TIGPublicationFolder.SetEngine(AValue: TIgPublisherBuildBaseEngine);
begin
  FEngine.Free;
  FEngine := AValue;
end;

procedure TIGPublicationFolder.emitLine(line: String; repl : boolean);
begin
  FManager.FLock.Lock;
  try
    if repl then
      FLines.add('t'+line)
    else
      FLines.add('f'+line);
  finally
    FManager.Flock.Unlock;
  end;
end;

function TIGPublicationFolder.log: String;
var
  b : TStringBuilder;
  s : String;
begin
  b := TStringBuilder.create;
  try
    FManager.FLock.Lock;
    try
      for s in FLines do
      begin
        b.append(s.substring(1));
        b.append(#13#10);
      end;
    finally
      FManager.Flock.Unlock;
    end;
    result := b.toString;
  finally
    b.free;
  end;
end;

function TIGPublicationFolder.ghURL: String;
var
  ts : TStringList;
  i : integer;
  s : String;
begin
  result := 'GitHub Repo URL unknown';
  try
    ts := TStringList.create;
    try
      ts.LoadFromFile(FilePath([folder, '.git', 'config']));
      for i := 0 to ts.count - 1 do
      begin
        s := ts[i].trim.replace(' ', '');
        if s.StartsWith('url=') then
           exit(s.Substring(4));
      end;
    finally
      ts.free;
    end;
  except
    on e : Exception do
      result := e.message;
  end;
end;

procedure TIGPublicationFolder.inspect(ts: TStringList);
begin
  ts.Values['Path'] := FFolder;
  case FStatus of
    fsNotRun : ts.Values['Status'] := 'Not run this session';
    fsRunning : ts.Values['Status'] := 'Building Now';
    fsSuccess : ts.Values['Status'] := 'Built Succesfully';
    fsError : ts.Values['Status'] := 'Failed';
  end;
  if FStatus <> fsNotRun then
    ts.Values['Run Time'] := DescribePeriod(FRunLength * DATETIME_MILLISECOND_ONE);
end;

{ TIgPubPageFrame }

destructor TIgPubPageFrame.Destroy;
begin
  FManager.Free;
  FTempStore.free;
  FIgPublisherVersions.Free;
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

procedure TIgPubPageFrame.changeToolbarButtons;
begin
  setToolbarForCaptions(Toolbar1, Context.ToolbarCaptions, false);
end;

procedure TIgPubPageFrame.init(json: TJsonObject);
var
  grp : TControlEntry;
  mDev : TMenuItem;
begin
  FIgPublisherVersions := TFslList<TIgPublisherVersion>.create;

  FManager := TIGPublicationManager.create;
  FManager.FFrame := self;

  grp := FManager.registerControlForMenu(tbAdd, pmAdd);
  FManager.registerMenuEntry(grp, 'Folder', 18, copAdd);
  FManager.registerMenuEntry(grp, 'From GitHub', 15, copAdd, 'git');
  FManager.registerMenuEntry(grp, 'Sub-Folders', 14, copAddSet);
  FManager.registerControl(tbDelete, copDelete);
  FManager.registerControl(tbUp, copUp);
  FManager.registerControl(tbDown, copDown);
  FManager.registerControl(tbBuild, copExecute);
  FManager.registerControl(tbStop, copStop);

  FManager.registerMenuEntry('Add', 4, copAdd);
  FManager.registerMenuEntry('Up', 11, copUp);
  FManager.registerMenuEntry('Down', 10, copDown);
  FManager.registerMenuEntry('Delete', 12, copDelete);
  FManager.registerMenuEntry('-', -1, copNone);
  FManager.registerMenuEntry('Copy', 13, copCopy);
  FManager.registerMenuEntry('-', -1, copNone);
  FManager.registerMenuEntry('Build', 1, copExecute);
  FManager.registerMenuEntry('Stop', 5, copStop);
  mDev := FManager.registerMenuEntry('Dev Tools', 1, copInfo, 'group');

  FManager.registerSubMenuEntry(mDev, TEXT_CMDPROMPT_NAME, 21, copInfo, 'terminal');
  FManager.registerSubMenuEntry(mDev, 'Open As Project', 24, copInfo, 'project');
  FManager.registerSubMenuEntry(mDev, 'Clean Generated Files', 17, copInfo, 'clean');
  FManager.registerSubMenuEntry(mDev, 'Clear Tx Cache', 19, copInfo, 'clear');
  FManager.registerSubMenuEntry(mDev, 'Run Jekyll', 20, copInfo, 'jekyll');
  FManager.registerSubMenuEntry(mDev, 'Open Folder', 18, copInfo, 'folder');
  FManager.registerSubMenuEntry(mDev, 'Open File', 23, copInfo, 'file');
  FManager.registerMenuEntry('-', -1, copNone);
  FManager.registerMenuEntry('Update', 16, copUpdate);
  FManager.registerMenuEntry('Update w. Stash', 25, copUpdate, 'stash');

  FManager.Images := ImageList1;
  FManager.list := lvFolders;

  cbxTxServer.items.clear;
  FJavaCmd := json.str['java-cmd'];
  FDefaultRootFolder := json.str['default-root'];
  if (FDefaultRootFolder = '') then
    FDefaultRootFolder := FilePath(['[tmp]', 'ig-pub']);
  if FJavaCmd = '' then
    FJavaCmd := 'java';
  FDevParams := json.str['dev-params'];
  listVersions(json);
  loadFolders(json);
  loadServers;
  changeToolbarButtons;
end;

procedure TIgPubPageFrame.tbConfigClick(Sender: TObject);
var
  igp : TIgPublisherVersion;
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
          igp := TIgPublisherVersion.create('Dev', '#dev');
          try
            FIgPublisherVersions.add(igp.link);
            cbxVersions.items.InsertObject(0, 'Dev', igp);
          finally
            igp.free;
          end;
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
        FIgPublisherVersions.Delete(0);
      end;
      FWorker.lastChange := GetTickCount64;
      FWorker.lastChangeChecked := false;
      FWorker.session.NeedsSaving := true;
    end;
  finally
    IGPublisherConfigForm.free;
  end;
end;

{$IFDEF WINDOWS}
function makeCleanBat : String;
begin
  result :=
    '@echo off'#13#10+
    'echo Delete Output'#13#10+
    'del /s /q output\*.* >nul'#13#10+
    'echo Delete Temp Pages'#13#10+
    'del /s /q temp\pages\*.* >nul'#13#10+
    'echo Delete QA Files'#13#10+
    'del /s /q temp\qa\*.* >nul'#13#10+
    'echo Delete Backup Files'#13#10+
    'del /s /q *.bak >nul'#13#10
end;
{$ELSE}
function makeCleanSh : String;
begin
  result :=
    '@echo off'#13#10+
    'echo To Do'#13#10
end;
{$ENDIF}

procedure TIgPubPageFrame.mnuCleanClick(Sender: TObject);
var
  fn : String;
begin
  if FManager.FCurrent <> nil then
  begin
    if (FManager.FCurrent.status = fsRunning) then
      MessageDlg('Clean IG', 'The IG '+FManager.FCurrent.name+' is being built or updated', mtError, [mbok], 0)
    else
    begin
      {$IFDEF WINDOWS}
      fn := FilePath([FManager.FCurrent.FFolder, 'clean.bat']);
      if not FileExists(fn) then
        StringToFile(makeCleanBat, fn, TEncoding.ASCII);
      {$ELSE}
      fn := FilePath([FManager.FCurrent.FFolder, 'clean.sh']);
      if not FileExists(fn) then
        StringToFile(makeCleanSh, fn, TEncoding.ASCII);
      {$ENDIF}
      FManager.FCurrent.lines.clear;
      FManager.FCurrent.engine := TIgPublisherCleanEngine.create;
      FManager.FCurrent.engine.folder := FManager.FCurrent.folder;
      FManager.FCurrent.engine.OnEmitLine := FManager.FCurrent.emitLine;
      FManager.FCurrent.engine.Start;
      FManager.FCurrent.StartRun := 0;
      FManager.FCurrent.status := fsRunning;
    end;
  end;
end;

procedure TIgPubPageFrame.MenuItem1Click(Sender: TObject);
begin
  raise EFslException.create('todo');
end;

procedure TIgPubPageFrame.MenuItem2Click(Sender: TObject);
begin
  if FManager.FCurrent <> nil then
    MainToolkitForm.newProject(FManager.FCurrent.name, FManager.FCurrent.folder);
end;

procedure TIgPubPageFrame.ClearDirectory(dir : String);
var
  s : String;
begin
  for s in TDirectory.getDirectories(dir) do
    ClearDirectory(s);
  FManager.FCurrent.FLines.add(' Delete files in '+dir);
  Application.ProcessMessages;
  for s in TDirectory.GetFiles(dir) do
    DeleteFile(s);
end;

procedure TIgPubPageFrame.mnuClearTxClick(Sender: TObject);
var
  s : string;
begin
  if FManager.FCurrent <> nil then
  begin
    FManager.FCurrent.FLines.Clear;
    ClearDirectory(FilePath([FManager.FCurrent.FFolder, 'input-cache', 'txCache']));
    FManager.FCurrent.FLines.Add(' Done');
  end;
end;

procedure TIgPubPageFrame.mnuCommandClick(Sender: TObject);
var
  proc : TProcess;
  s : String;
  i : integer;
begin
  if FManager.FCurrent <> nil then
  begin
    {$IFDEF WINDOWS}
    proc := TProcess.create(nil);
    try
      proc.CurrentDirectory := FilePath([FManager.FCurrent.FFolder]);
      proc.CommandLine := 'cmd';
      for i := 1 to GetEnvironmentVariableCount do
        begin
        s := GetEnvironmentString(i);
        proc.Environment.Add(s+'=' + GetEnvironmentVariable(s));
      end;
      proc.Execute;
    finally
      proc.free;
    end;
    {$ELSE}
    proc := TProcess.create(nil);
    try
      proc.CurrentDirectory := FilePath([FManager.FCurrent.FFolder]);
      proc.Executable := 'open';
      proc.Parameters.Add('-a');
      proc.Parameters.Add('Terminal');
      proc.Parameters.Add(proc.CurrentDirectory);
      proc.showWindow := swoShow;
      proc.Execute;
    finally
      proc.free;
    end;
    {$ENDIF}
  end;
end;

procedure TIgPubPageFrame.mnuFolderClick(Sender: TObject);
begin
  if FManager.FCurrent <> nil then
    OpenDocument(FilePath([FManager.FCurrent.FFolder]));
end;

procedure TIgPubPageFrame.mnuJekyllClick(Sender: TObject);
var
  proc : TProcess;
  s, c, f, fn, td : String;
  i : integer;
  j : TIgPublisherJekyllEngine;
begin
  if FManager.FCurrent <> nil then
  begin
    if (FManager.FCurrent.status = fsRunning) then
      MessageDlg('Run Jekyll', 'The IG '+FManager.FCurrent.name+' is being built or updated', mtError, [mbok], 0)
    else
    begin
      if jekyllCommand(FManager.FCurrent.log, c, f) then
      begin
        FManager.FCurrent.lines.clear;
        j := TIgPublisherJekyllEngine.create;
        FManager.FCurrent.engine := j;
        FManager.FCurrent.engine.folder := f;
        j.command := c;
        FManager.FCurrent.engine.OnEmitLine := FManager.FCurrent.emitLine;
        FManager.FCurrent.engine.Start;
        FManager.FCurrent.StartRun := 0;
        FManager.FCurrent.status := fsRunning;
      end;
    end;
  end;
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
  procedure addIGP(version, url : String);
  var
    igp : TIgPublisherVersion;
  begin
    igp := TIgPublisherVersion.create(version, url);
    try
      FIgPublisherVersions.add(igp.link);
      cbxVersions.items.addObject(version, igp);
    finally
      igp.free;
    end;
  end;
begin
  cbxVersions.items.clear;
  if FDevParams <> '' then
    AddIgp('Dev', '#dev');

  if (json <> nil) and json.has('ig-pub-versions') then
  begin
    arr := json.arr['ig-pub-versions'];
    for o in arr.asObjects.forEnum do
      AddIgp(o.str['version'], o.str['url']);
    cbxVersions.itemIndex := json.int['ig-pub-version'];
  end
  else
  begin
    try
      arr := TInternetFetcher.fetchJsonArray('https://api.github.com/repos/HL7/fhir-ig-publisher/releases');
      try
        for o in arr.asObjects.forEnum do
          AddIgp(o.str['tag_name'], o.forceArr['assets'].Obj[0].str['browser_download_url']);
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

procedure TIgPubPageFrame.loadFolders(json: TJsonObject);
var
  arr : TJsonArray;
  i : integer;
begin
  arr := json.forceArr['igs'];
  FManager.FFolderList := arr.link;
  FManager.doLoad;
end;

procedure TIgPubPageFrame.loadServers;
var
  t : TToolkitContextTerminologyServer;
begin
  cbxTxServer.items.clear;
  for t in Context.txServers.list do
    cbxTxServer.items.addObject(t.name, t);
end;

procedure TIgPubPageFrame.updateButtons;
var
  c, f : string;
begin
  if FManager.FCurrent = nil then
  begin
    tbOpen.enabled := false;
    tbQA.enabled := false;
    tbDevTools.enabled := false;
  end
  else
  begin
    tbOpen.enabled := FileExists(FilePath([FManager.FCurrent.FFolder, 'output', 'index.html']));
    tbQA.enabled := FileExists(FilePath([FManager.FCurrent.FFolder, 'output', 'qa.html']));
    mnuCommand.caption := TEXT_CMDPROMPT_NAME;
    tbDevTools.enabled := true;
    mnuClean.enabled := true;
    mnuCommand.enabled := true;
    mnuFolder.enabled := true;
    mnuClearTx.enabled := FolderExists(FilePath([FManager.FCurrent.FFolder, 'input-cache', 'txCache']));
    mnuJekyll.enabled := JekyllCommand(FManager.FCurrent.log, c, f);
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
  igp : TIgPublisherVersion;
begin
  json.str['igpub-page'] := 'true';
  json.str['default-root'] := FDefaultRootFolder;
  json.str['java-cmd'] := FJavaCmd;
  json.str['dev-params'] := FDevParams;
  arr := json.forceArr['ig-pub-versions'];
  arr.clear;
  for igp in FIgPublisherVersions do
  begin
    if igp.url <> '#dev' then
    begin
      v := TJsonObject.create;
      arr.add(v);
      v.str['version'] := igp.version;
      v.str['url'] := igp.url;
    end;
  end;
  json.int['ig-pub-version'] := cbxVersions.itemIndex;
  arr := json.forceArr['igs'];
  arr.clear;
  for i := 0 to FManager.Data.count - 1 do
  begin
    v := TJsonObject.create;
    arr.add(v);
    v.str['name'] := FManager.Data[i].name;
    v.str['folder'] := FManager.Data[i].folder;
    v.int['run-length'] := FManager.Data[i].RunLength;
    v.int['line-count'] := FManager.Data[i].LineCount;
  end;
end;

procedure TIgPubPageFrame.updateSettings;
begin
  loadServers;
  mStatus.Font.assign(Context.LogFont);
  lvFolders.Font.assign(Context.ViewFont);
end;

procedure TIgPubPageFrame.inspect;
var
  ts : TStringList;
begin
  ts := TStringList.create;
  try
    if FManager.Focus <> nil then
      FManager.Focus.inspect(ts);
    Context.Inspector.Populate(ts);
  finally
    ts.Free;
  end;
end;


end.

