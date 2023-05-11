unit ftk_project_tree;

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
  Classes, SysUtils, Graphics, IniFiles,
  Controls, ComCtrls, Dialogs, UITypes, Menus,
  fsl_base, fsl_utilities, fsl_json, fsl_fpc, fsl_stream,
  fui_lcl_managers,
  fhir_client,
  ftk_utilities, ftk_constants, ftk_context;

type
  TFHIRProjectNodeKind = (pnkProject, pnkFolder, pnkFile);

const
  CODES_TFHIRProjectNodeKind : array [TFHIRProjectNodeKind] of String = ('project', 'folder', 'file');

type
  { TProjectIgnorer }

  TProjectIgnorer = class (TFslObject)
  private
    FFolder : String;
    FIgnoreFile : String;
    FLines : TStringList;
    FLoaded : TDateTime;
    function matchesLine(line, path : String) : boolean;
    procedure load;

  public
    constructor Create(folder, ignoreFile: String);
    destructor Destroy; override;
    function Link : TProjectIgnorer; overload;

    procedure checkTime;
    function ignore(path : String) : boolean;
    procedure addPath(path : String);
  end;

  { TFHIRProjectNode }

  TFHIRProjectNode = class (TFslTreeNode)
  private
    FId : String;
    FName : String;
    FAddress : String;
    FIgnoreFile : String;
    FIgnorer : TProjectIgnorer;
    FKind : TFHIRProjectNodeKind;
    FFormat : TSourceEditorKind;
    FChildren : TFslList<TFHIRProjectNode>;
    function nameExists(name : String) : boolean;
    function contains(node : TFHIRProjectNode) : boolean;
    function sortChildren(sender : TObject; const o1, o2 : TFHIRProjectNode) : Integer;
    procedure loadIgnorer;
  protected
    function getChildCount : integer; override;
    function getChild(index : integer) : TFslTreeNode; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TFHIRProjectNode; overload;

    function toJson : TJsonObject;
    class function fromJson(json : TJsonObject) : TFHIRProjectNode;

    procedure loadFromAddress;
    function getNewName(ext : String) : String;
    function getNameForFile(fn : String) : String;
    procedure renameChildren(op, np : String);

    property id : String read FId write FId;
    property kind : TFHIRProjectNodeKind read FKind write FKind;
    property name : String read FName write FName;
    property address : String read FAddress write FAddress;
    property format : TSourceEditorKind read FFormat write FFormat;
    property ignoreFile : String read FIgnoreFile write FIgnoreFile;
    property children : TFslList<TFHIRProjectNode> read FChildren;
  end;

  TFHIRProjectsView = class;

  { TProjectTreeManager }

  TProjectTreeManager = class (TTreeManager<TFHIRProjectNode>)
  private
    FView : TFHIRProjectsView;
    FContext : TToolkitContext;
    function getKindByMode(mode : String) : TSourceEditorKind;
    function getProjectForNode(node : TFHIRProjectNode) : TFHIRProjectNode;
    procedure SetContext(AValue: TToolkitContext);
  public
    destructor Destroy; override;

    property Context : TToolkitContext read FContext write SetContext;

    function doubleClickEdit : boolean; override;
    function AskOnDelete(item : TFHIRProjectNode) : boolean; override;
    function readOnly : boolean; override;

    function allowedOperations(item : TFHIRProjectNode) : TNodeOperationSet; override;
    procedure getCopyModes(modes : TStringList); override;

    function loadData : boolean; override;

    procedure buildMenu; override;
    function getImageIndex(item : TFHIRProjectNode) : integer; override;
    function getCellText(item : TFHIRProjectNode) : String; override;
//    function getCellColors(item : TFHIRProjectNode; col : integer; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TFHIRProjectNode) : String; override;

    procedure changed; override;

    function addItem(parent : TFHIRProjectNode; mode : String) : TFHIRProjectNode; override;
    function editItem(item : TFHIRProjectNode; mode : String) : boolean; override;
    function editItemText(parent, item : TFHIRProjectNode; var text : String) : boolean; override;
    function deleteItem(parent, item : TFHIRProjectNode) : boolean; override;
    function executeItem(item : TFHIRProjectNode; mode : String) : boolean; override;
    function refreshItem(item : TFHIRProjectNode) : boolean; override;
    function updateItem(item : TFHIRProjectNode; mode : String) : TFHIRProjectNode; override;
    function getCanCopy(item : TFHIRProjectNode; mode : String) : boolean; override;
    function getCopyValue(item : TFHIRProjectNode; mode : String) : String; override;
  end;


  { TFHIRProjectsView }

  TFHIRProjectsView = class(TFslObject)
  private
    FControlFile: String;
    FManager : TProjectTreeManager;
    function GetContext: TToolkitContext;
    function GetImages: TImageList;
    function GetProjects: TFslList<TFHIRProjectNode>;
    function GetTree: TTreeView;
    procedure SetContext(AValue: TToolkitContext);
    procedure SetImages(AValue: TImageList);
    //function GetServerList: TFslList<TFHIRServerEntry>;
    procedure SetTree(AValue: TTreeView);

    function findNodeForFile(list : TFslList<TFHIRProjectNode>; address: String): TFHIRProjectNode; overload;
  public
    constructor Create(settings : TIniFile);
    destructor Destroy; override;

    property Context : TToolkitContext read GetContext write SetContext;
    property ControlFile : String read FControlFile write FControlFile;
    property Tree : TTreeView read GetTree write SetTree;
    property Projects : TFslList<TFHIRProjectNode> read GetProjects;
    property Images : TImageList read GetImages write SetImages;
    //property OnOpenServer : TOpenServerEvent read FOnOpenServer write FOnOpenServer;
    //property OnEditServer : TOpenServerEvent read FOnEditServer write FOnEditServer;

    procedure load;
    procedure save;
    procedure refresh;
    procedure saveStatus;

    //function FetchServer(sender : TObject; name : String) : TFHIRServerEntry;
    //
    procedure addProject(project : TFHIRProjectNode);
    function deleteProjectItem(parent, item : TFHIRProjectNode; files : boolean) : boolean;
    procedure deleteProject(project : TFHIRProjectNode; files : boolean);
    procedure renameFile(old, new : String);
    //procedure updateServer(server, newDetails : TFHIRServerEntry);
  end;

implementation

uses
  ftk_worker_server,
  frm_main;

{ TFHIRProjectNode }

constructor TFHIRProjectNode.Create;
begin
  inherited Create;
  FChildren := TFslList<TFHIRProjectNode>.create;
end;

destructor TFHIRProjectNode.Destroy;
begin
  FChildren.Free;
  FIgnorer.Free;
  inherited Destroy;
end;

function TFHIRProjectNode.nameExists(name: String): boolean;
var
  c : TFHIRProjectNode;
  s : String;
begin
  result := false;
  for c in FChildren do
    if SameText(c.name, name) then
      exit(true);
  if (address <> '') then
  begin
    for s in TDirectory.GetFiles(address) do
      if SameText(ExtractFileName(s), name) then
        exit(true);
    for s in TDirectory.getDirectories(address) do
      if SameText(ExtractFileName(s), name) then
        exit(true);
  end;
end;

function TFHIRProjectNode.contains(node: TFHIRProjectNode): boolean;
var
  n : TFHIRProjectNode;
begin
  if node = self then
    result := true
  else
  begin
    result := false;
    if FChildren <> nil then
      for n in FChildren do
        if n.contains(node) then
          exit(true);
  end;
end;

function TFHIRProjectNode.sortChildren(sender: TObject; const o1, o2: TFHIRProjectNode): Integer;
begin
  result := ord(o1.kind) - ord(o2.kind);
  if result = 0 then
    result := compareStr(o1.name, o2.name);
end;

procedure TFHIRProjectNode.loadIgnorer;
begin
  if (FIgnorer = nil) then
    FIgnorer := TProjectIgnorer.create(address, FIgnoreFile);
end;

function TFHIRProjectNode.getChildCount: integer;
begin
  result := FChildren.count;
end;

function TFHIRProjectNode.getChild(index: integer): TFslTreeNode;
begin
  result := FChildren[index];

end;

function TFHIRProjectNode.link: TFHIRProjectNode;
begin
  result := TFHIRProjectNode(inherited Link);
end;

function TFHIRProjectNode.toJson: TJsonObject;
var
  i : integer;
begin
  result := TJsonObject.create;
  try
    result['id'] := FId;
    result['kind'] := CODES_TFHIRProjectNodeKind[FKind];
    result['name'] := FName;
    result['address'] := FAddress;
    result['ignoreFile'] := FIgnoreFile;
    result.int['format'] := integer(FFormat);
    if FChildren.Count > 0 then
      for i := 0 to FChildren.count - 1 do
         result.forceArr['children'].add(FChildren[i].toJson);
    result.link;
  finally
    result.free;
  end;
end;

class function TFHIRProjectNode.fromJson(json: TJsonObject): TFHIRProjectNode;
var
  i : integer;
begin
  result := TFHIRProjectNode.create;
  try
    result.FId := json['id'];
    result.FKind := TFHIRProjectNodeKind(StringArrayIndexOf(CODES_TFHIRProjectNodeKind, json['kind']));
    result.FName := json['name'];
    result.FAddress := json['address'];
    result.FIgnoreFile := json['ignoreFile'];
    result.FFormat := TSourceEditorKind(json.int['format']);
    for i := 0 to json.forceArr['children'].count - 1 do
       result.FChildren.add(TFHIRProjectNode.fromJson(json.forceArr['children'].Obj[i]));
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRProjectNode.loadFromAddress;
var
  s, ext : String;
  n : TFHIRProjectNode;
  i : integer;
begin
  FChildren.clear;
  loadIgnorer;

  if address <> '' then
  begin
    for s in TDirectory.getDirectories(address) do
    begin
      if (FileGetAttr(s) and faHidden = 0) and not FIgnorer.ignore(s+'/') then
      begin
        n := TFHIRProjectNode.create;
        FChildren.add(n);
        n.kind := pnkFolder;
        n.name := ExtractFileName(s);
        n.address := s;
        n.FIgnorer := FIgnorer.Link;
        n.loadFromAddress;
      end;
    end;

    for s in TDirectory.GetFiles(address) do
    begin
      if (FileGetAttr(s) and faHidden = 0) and not FIgnorer.ignore(s) then
      begin
        n := TFHIRProjectNode.create;
        FChildren.add(n);
        n.kind := pnkFile;
        n.name := ExtractFileName(s);
        n.address := s;
        ext := lowercase(ExtractFileExt(s));
        i := StringArrayIndexOf(EXTENSIONS_TSourceEditorKind, ext);
        if (i > -1) then
          n.format := TSourceEditorKind(i);
      end;
    end;
  end;
  FChildren.SortE(sortChildren);
end;

function TFHIRProjectNode.getNewName(ext: String): String;
var
  base : String;
  i : integer;
begin
  if ext = '' then
    base := 'new_folder_'
  else
    base := 'new_file_';
  result := base+ext;
  i := 0;
  while nameExists(result) do
  begin
    inc(i);
    result := base+inttostr(i)+ext;
  end;
end;

function TFHIRProjectNode.getNameForFile(fn: String): String;
var
  base, ext : String;
  i : integer;
begin
  ext := ExtractFileExt(fn);
  base := fn.replace(ext, '');
  result := base+ext;
  i := 0;
  while nameExists(result) do
  begin
    inc(i);
    result := base+inttostr(i)+ext;
  end;
end;

procedure TFHIRProjectNode.renameChildren(op, np: String);
var
  i : TFHIRProjectNode;
begin
  for i in FChildren do
  begin
    if i.address.startsWith(op) then
    begin
      i.address := np+i.address.Substring(op.length);
      if (i.kind = pnkFolder) then
       i.renameChildren(op, np);
    end;
  end;
end;

{ TProjectTreeManager }

destructor TProjectTreeManager.Destroy;
begin
  FContext.Free;
  inherited Destroy;
end;

function TProjectTreeManager.getKindByMode(mode: String): TSourceEditorKind;
begin
  if StringArrayExists(CODES_TSourceEditorKind, mode) then
    result := TSourceEditorKind(StringArrayIndexOf(CODES_TSourceEditorKind, mode))
  else
    result := sekNull;
end;

function TProjectTreeManager.getProjectForNode(node : TFHIRProjectNode): TFHIRProjectNode;
var
  n : TFHIRProjectNode;
begin
  result := nil;
  for n in Data do
    if (n.contains(node)) then
      exit(n);
end;

procedure TProjectTreeManager.SetContext(AValue: TToolkitContext);
begin
  FContext.Free;
  FContext := AValue;
end;
function TProjectTreeManager.doubleClickEdit: boolean;
begin
  Result := false;
end;

function TProjectTreeManager.AskOnDelete(item : TFHIRProjectNode): boolean;
begin
  Result := false;
end;

function TProjectTreeManager.readOnly: boolean;
begin
  result := false;
end;

function TProjectTreeManager.allowedOperations(item: TFHIRProjectNode): TNodeOperationSet;
begin
  result := [];
  if item <> nil then
    case item.kind of
      pnkProject:
        if item.address <> '' then
          result := [opAdd, opDelete, opRefresh, opEdit, opInfo]
        else
          result := [opAdd, opDelete, opEdit];
      pnkFolder:
        if item.address <> '' then
          result := [opAdd, opDelete, opRefresh, opEdit, opInfo, opUpdate]
        else
          result := [opAdd, opDelete, opEdit];
      pnkFile: result := [opDelete, opRefresh, opEdit, opExecute, opUpdate];
    end;
end;

procedure TProjectTreeManager.getCopyModes(modes: TStringList);
begin
  modes.addPair('name', 'Name');
  modes.addPair('path', 'Path');
  modes.addPair('content', 'Contents');
end;

function TProjectTreeManager.loadData : boolean;
var
  json, o : TJsonObject;
  arr : TJsonArray;
  i : integer;
begin
  result := false;
  if (FileExists(FView.FControlFile)) then
  begin
    result := true;
    json := TJSONParser.ParseFile(FView.FControlFile);
    try
      arr := json.forceArr['projects'];
      for i := 0 to arr.count - 1 do
        Data.Add(TFHIRProjectNode.fromJson(arr.Obj[i]));
    finally
      json.free;
    end;
  end;
end;

procedure TProjectTreeManager.buildMenu;
var
  mnu, t : TMenuItem;
begin
  inherited buildMenu;
  mnu := registerMenuEntry('Add', ICON_ADD, copNone, 'file');
  registerSubMenuEntry(mnu, 'Folder', 127, copAdd, 'folder');
  registerSubMenuEntry(mnu, 'Existing File', 3, copAdd, 'file');
  registerSubMenuEntry(mnu, '-', -1, copNone);
  for t in MainToolkitForm.pmNew.Items do
    if t.visible then
      if t.tag <> 0 then
        registerSubMenuEntry(mnu, t.Caption, t.ImageIndex, copAdd, CODES_TSourceEditorKind[TSourceEditorKind(t.tag)])
      else
        registerSubMenuEntry(mnu, t.Caption, t.ImageIndex, copAdd, 'paste');
  registerMenuEntry('Open', ICON_EXECUTE, copExecute);
  mnu := registerMenuEntry('Open As', ICON_EXECUTE, copExecute, 'group');
  for t in MainToolkitForm.pmNew.Items do
    if t.visible then
      if t.tag > 0 then
        registerSubMenuEntry(mnu, t.Caption, t.ImageIndex, copExecute, CODES_TSourceEditorKind[TSourceEditorKind(t.tag)]);
  registerMenuEntry('Copy', ICON_COPY, copCopy);
  registerMenuEntry('Refresh', ICON_REFRESH, copRefresh);
  registerMenuEntry('Delete', ICON_DELETE, copDelete);
  registerMenuEntry('Hide', ICON_HIDE, copUpdate, 'hide');
end;

function TProjectTreeManager.getImageIndex(item: TFHIRProjectNode): integer;
var
  fmt : TSourceEditorKind;
begin
  case item.kind of
    pnkProject :
      if (item.address <> '') then
        result := ICON_PACKAGE_LINK
      else
        result := ICON_PACKAGE;
    pnkFolder : result := ICON_FOLDER;
    pnkFile :
      if (item.address <> '') and (Context.EditorForAddress('file:'+item.address) <> nil) then
        result := ICON_OPEN_FILE
      else
      begin
        result := ICONS_TSourceEditorKind[item.format];
        if (result = -1) then
          result := ICON_FILE;
      end
  else
    result := -1;
  end;
end;

function TProjectTreeManager.getCellText(item: TFHIRProjectNode): String;
begin
  result := item.name;
end;

//function TProjectTreeManager.getCellColors(item: TFHIRProjectNode; col: integer; var fore, back: TColor): boolean;
//begin
//  Result := inherited getCellColors(item, col, fore, back);
//end;
//
function TProjectTreeManager.getSummaryText(item: TFHIRProjectNode): String;
begin
  if (item = nil) then
    result := 'Project List'
  else if (item.address <> '') then
    Result := item.name+' ('+item.address+')'
  else
    Result := item.name;
end;

procedure TProjectTreeManager.changed;
begin
  FView.save;
end;

function TProjectTreeManager.addItem(parent : TFHIRProjectNode; mode: String): TFHIRProjectNode;
var
  k : TSourceEditorKind;
  n, e, p : String;
  cnt : TBytes;
begin
  result := nil;
  if (parent.kind = pnkFile) then
    exit;

  if (mode = 'folder') then
  begin
    n := parent.getNewName('');
    result := TFHIRProjectNode.create;
    try
      result.kind := pnkFolder;
      result.name := n;
      if parent.address <> '' then
      begin
        result.address := FilePath([parent.address, n]);
        CreateDir(result.address);
      end;
      parent.FChildren.add(result.link);
    finally
      result.free;
    end;
  end
  else if (mode = 'file') then
  begin
    if MainToolkitForm.FileSystem.openDlg(p) then
    begin
      p := p.substring(5); // remove the file:
      n := ExtractFileName(p);
      n := parent.getNameForFile(n);
      result := TFHIRProjectNode.create;
      try
        result.kind := pnkFile;
        result.name := n;
        if parent.address <> '' then
        begin
          result.address := FilePath([parent.address, n]);
          BytesToFile(FileToBytes(p), result.address);
        end
        else
          result.address := p;
        MainToolkitForm.openFile('file:'+result.address);
        parent.FChildren.add(result.link);
      finally
        result.free;
      end;
    end;
  end
  else
  begin
    if mode = 'paste' then
      k := MainToolkitForm.determineClipboardFormat(cnt)
    else
      k := getKindByMode(mode);
    if k <> sekNull then
    begin
      e := EXTENSIONS_TSourceEditorKind[k];
      n := parent.getNewName(e);
      result := TFHIRProjectNode.create;
      try
        result.kind := pnkFile;
        result.name := n;
        if parent.address <> '' then
          result.address := FilePath([parent.address, n]);
        MainToolkitForm.createNewFile(k, result.name, result.address, cnt);
        result.link;
      finally
        result.free;
      end;
    end;
  end;
end;

function TProjectTreeManager.editItem(item: TFHIRProjectNode; mode: String): boolean;
begin
  // FView.OnEditServer(FView, item);
end;

function TProjectTreeManager.editItemText(parent, item: TFHIRProjectNode; var text: String): boolean;
var
  t : TFHIRProjectNode;
  op, np : String;
begin
  result := true;
  for t in parent.children do
  begin
    if (t <> item) and (t.name = text) then
    begin
      MessageDlg('The name '+text+' is already in use', mtInformation, [mbOK], 0);
      exit(false);
    end;
  end;
  if parent.address <> '' then
  begin
    op := item.address;
    np := FilePath([parent.address, text]);
    if FileExists(np) then
    begin
      MessageDlg('The name '+text+' is already in use', mtInformation, [mbOK], 0);
      exit(false);
    end;
    if not RenameFile(op, np) then
    begin
      MessageDlg('Unable to rename '+item.name+' to '+text+' in '+parent.address, mtInformation, [mbOK], 0);
      exit(false);
    end;
    if item.kind = pnkFolder then
      item.renameChildren(op, np);
    item.name := text;
    if item.kind = pnkFolder then
      MainToolkitForm.renameFolder(op, np)
    else
      MainToolkitForm.renameProjectFile(op, np);
  end
  else
  begin
    // in this case, we only rename the entry in the project; the file itself doesn't change
    item.name := text;
  end;
end;

function TProjectTreeManager.deleteItem(parent, item: TFHIRProjectNode) : boolean;
var
  res : TModalResult;
begin
  result := true;
  case item.kind of
    pnkProject :
      if item.address <> '' then
        res := MessageDlg('Delete Project '+item.Name+'. Truly delete all the files in the project too? This operation cannot be undone', mtConfirmation, [mbYes, mbNo, mbCancel], 0)
      else
        res := MessageDlg('Delete Project '+item.Name+' and all it''s entries? This operation cannot be undone', mtConfirmation, [mbYes, mbCancel], 0);
    pnkFolder:
      if item.address <> '' then
        res := MessageDlg('Delete Folder '+item.Name+' and all it''s contents? This operation cannot be undone', mtConfirmation, [mbYes, mbCancel], 0)
      else
        res := MessageDlg('Delete Folder '+item.Name+' and all it''s entries? This operation cannot be undone', mtConfirmation, [mbYes, mbCancel], 0);
    pnkFile :
      if parent.address <> '' then
        res := MessageDlg('Delete the file '+item.Name+'. This operation cannot be undone', mtConfirmation, [mbYes, mbNo, mbCancel], 0)
      else
        res := MessageDlg('Delete the link to file '+item.Name+'? This operation cannot be undone', mtConfirmation, [mbYes, mbCancel], 0);
  end;
  case res of
    mrYes: result := FView.deleteProjectItem(parent, item, true);
    mrNo: result := FView.deleteProjectItem(parent, item, false);
    mrCancel: result := false;
  end;
  if result then
  begin
    if parent = nil then
      Data.remove(item)
    else
      parent.children.remove(item);
  end;
end;

function TProjectTreeManager.executeItem(item: TFHIRProjectNode; mode: String): boolean;
var
  kind : TSourceEditorKind;
begin
  if (item.kind = pnkFile) and (mode <> 'group') then
  begin
    if mode <> '' then
    begin
      kind := TSourceEditorKind(StringArrayIndexOf(CODES_TSourceEditorKind, mode));
      if kind <= sekNull then
        MainToolkitForm.openFile('file:'+item.address)
      else
        MainToolkitForm.openFile('file:'+item.address, kind);
    end
    else
      MainToolkitForm.openFile('file:'+item.address);
    result := true;
  end
  else
    Result := false;
end;

function TProjectTreeManager.refreshItem(item: TFHIRProjectNode) : boolean;
begin
  if (item.address <> '') then
  begin
    item.loadFromAddress;
    FView.save;
    doLoad;
    result := true;
  end
  else
    result := false;
end;

function TProjectTreeManager.updateItem(item: TFHIRProjectNode; mode : String): TFHIRProjectNode;
var
  n : TFHIRProjectNode;
begin
  n := getProjectForNode(item);
  n.loadIgnorer;
  n.FIgnorer.addPath(item.address.substring(n.address.length+1));
  result := n;
end;

function TProjectTreeManager.getCanCopy(item: TFHIRProjectNode; mode: String): boolean;
begin
  if (mode = 'content') then
    result := item.kind = pnkFile
  else if (mode = 'path') then
    result := item.address <> ''
  else if (mode = 'name') then
    result := true
  else if (mode = '') then
    result := true
  else
    result := false;
end;

function TProjectTreeManager.getCopyValue(item: TFHIRProjectNode; mode: String): String;
begin
  if (mode = 'name') then
    result := item.name
  else if (mode = 'path') then
    result := item.address
  else if (mode = 'content') then
    result := FileToString(item.address, TEncoding.UTF8)
  else
    result := '';
end;

{ TFHIRProjectsView }

constructor TFHIRProjectsView.Create(settings : TIniFile);
begin
  inherited create;
  FManager := TProjectTreeManager.create;
  FManager.Settings := Settings;
  FManager.FView := self;
end;

destructor TFHIRProjectsView.Destroy;
begin
  FManager.Free;
  inherited Destroy;
end;

function TFHIRProjectsView.GetTree: TTreeView;
begin
  result := FManager.Tree;
end;

procedure TFHIRProjectsView.SetContext(AValue: TToolkitContext);
begin
  FManager.Context := AValue;
end;

procedure TFHIRProjectsView.SetImages(AValue: TImageList);
begin
  FManager.Images := AValue;
end;

function TFHIRProjectsView.GetProjects: TFslList<TFHIRProjectNode>;
begin
  result := FManager.Data;
end;

function TFHIRProjectsView.GetImages: TImageList;
begin
  result := FManager.Images;
end;

function TFHIRProjectsView.GetContext: TToolkitContext;
begin
  result := FManager.Context;
end;

//function TFHIRProjectsView.GetServerList: TFslList<TFHIRServerEntry>;
//begin
//  result := FManager.Data;
//end;

procedure TFHIRProjectsView.SetTree(AValue: TTreeView);
begin
  FManager.Tree := AValue;
end;

function TFHIRProjectsView.findNodeForFile(list : TFslList<TFHIRProjectNode>; address: String): TFHIRProjectNode;
var
  t : TFHIRProjectNode;
begin
  result := nil;
  for t in list do
  begin
    if (t.address = address) then
      exit(t);
    result := findNodeForFile(t.children, address);
    if result <> nil then
      exit;
  end;
end;

procedure TFHIRProjectsView.load;
begin
  try
    FManager.doLoad();
  except
  end;
end;

procedure TFHIRProjectsView.save;
var
  json : TJsonObject;
  p : TFHIRProjectNode;
begin
  json := TJsonObject.create;
  try
    json.str['edit'] := 'Don''t edit this file manually';
    for p in FManager.Data do
      json.forceArr['projects'].add(p.toJson);
    BytesToFile(TJSONWriter.writeObject(json, true), FControlFile);
  finally
    json.free;
  end;
end;

procedure TFHIRProjectsView.refresh;
begin
  FManager.refresh();
end;

procedure TFHIRProjectsView.saveStatus;
begin
  FManager.saveStatus;
end;


procedure TFHIRProjectsView.addProject(project: TFHIRProjectNode);
var
  json : TJsonObject;
begin
  if FileExists(FControlFile) then
    json := TJsonParser.ParseFile(FControlFile)
  else
    json := TJsonObject.create;
  try
    json.str['edit'] := 'Don''t edit this file manually';
    json.forceArr['projects'].add(project.toJson);

    BytesToFile(TJSONWriter.writeObject(json, true), FControlFile);
  finally
    json.free;
  end;
  FManager.doLoad;
end;

function TFHIRProjectsView.deleteProjectItem(parent, item: TFHIRProjectNode; files: boolean) : boolean;
begin
  result := true;
  case item.kind of
    pnkProject, pnkFolder :
      begin
        if (item.address <> '') then
          if not MainToolkitForm.closeFiles(item.address) then
            exit(false);
        if files and (item.address <> '') then
          FolderDelete(item.address);
      end;
    pnkFile :
      begin
        if (item.address <> '') then
          if not MainToolkitForm.closeFiles(item.address) then
            exit(false);
        if files and (item.address <> '') then
          FileDelete(item.address);
      end;
  end;
end;

procedure TFHIRProjectsView.deleteProject(project: TFHIRProjectNode; files: boolean);
var
  json : TJsonObject;
  i : integer;
begin
  if files and (project.address <> '') then
    FolderDelete(project.address);
  if FileExists(FControlFile) then
    json := TJsonParser.ParseFile(FControlFile)
  else
    json := TJsonObject.create;

  try
    json.str['edit'] := 'Don''t edit this file manually';
    for i := json.forceArr['projects'].count - 1 downto 0 do
      if json.arr['projects'][i]['id'] = project.id then
        json.arr['projects'].remove(i);
    BytesToFile(TJSONWriter.writeObject(json, true), FControlFile);
  finally
    json.free;
  end;
end;

procedure TFHIRProjectsView.renameFile(old, new: String);
var
  p : TFHIRProjectNode;
begin
  p := findNodeForFile(FManager.Data, old);
  if (p <> nil) then
  begin
    p.address := new;
    p.name := ExtractFileName(new);
    save;
    FManager.refresh(p);
  end;
end;

{ TProjectIgnorer }

function TProjectIgnorer.matchesLine(line, path: String): boolean;
var
  n : String;
begin
  path := path.trim;
  if path.startsWith('#') then
    exit(false);

  result := false;
  n := path.substring(FFolder.length);
  if (n = line) then
    exit(true);
  if (path.endsWith('/')) then
    path := path.subString(0, path.Length-1);
  n := extractFileName(path);
  if (n = line) then
    exit(true);
end;

procedure TProjectIgnorer.load;
begin
  FLoaded := FileGetModified(FilePath([FFolder, FIgnoreFile]));
  if FIgnoreFile <> '' then
  begin
    FLines.LoadFromFile(FilePath([FFolder, FIgnoreFile]));
  end;
end;

constructor TProjectIgnorer.Create(folder, ignoreFile : String);
begin
  inherited Create;
  FLines := TStringList.create;
  FFolder := folder;
  FIgnoreFile := ignoreFile;
  load();
end;

destructor TProjectIgnorer.Destroy;
begin
  FLines.free;
  inherited Destroy;
end;

function TProjectIgnorer.Link : TProjectIgnorer;
begin
  result := TProjectIgnorer(inherited Link);
end;

procedure TProjectIgnorer.checkTime;
begin
  if FLoaded <> FileGetModified(FilePath([FFolder, FIgnoreFile])) then
    load;
end;

function TProjectIgnorer.ignore(path : String) : boolean;
var
  s : String;
begin
  result := false;
  for s in FLines do
    if matchesLine(s, path) then
      exit(true);
end;

procedure TProjectIgnorer.addPath(path: String);
var
  ts : TStringList;
begin
  ts := TStringlist.create;
  try
    ts.LoadFromFile(FilePath([FFolder, FIgnoreFile]));
    ts.add(path);
    ts.SaveToFile(FilePath([FFolder, FIgnoreFile]));
  finally
    ts.free;
  end;
end;

end.
