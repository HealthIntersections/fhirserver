unit ftk_project_tree;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Graphics, IniFiles,
  ComCtrls,
  fsl_base, fsl_utilities, fsl_json,
  fui_lcl_managers,
  fhir_client,
  ftk_utilities, ftk_constants;

type
  TFHIRProjectNodeKind = (pnkProject, pnkFolder, pnkFile);

const
  CODES_TFHIRProjectNodeKind : array [TFHIRProjectNodeKind] of String = ('project', 'folder', 'file');

type
  { TFHIRProjectNode }

  TFHIRProjectNode = class (TFslTreeNode)
  private
    FId : String;
    FName : String;
    FAddress : String;
    FKind : TFHIRProjectNodeKind;
    FChildren : TFslList<TFHIRProjectNode>;
  protected
    function getChildCount : integer; override;
    function getChild(index : integer) : TFslTreeNode; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TFHIRProjectNode; overload;

    function toJson : TJsonObject;
    class function fromJson(json : TJsonObject) : TFHIRProjectNode;

    property id : String read FId write FId;
    property kind : TFHIRProjectNodeKind read FKind write FKind;
    property name : String read FName write FName;
    property address : String read FAddress write FAddress;
    property children : TFslList<TFHIRProjectNode> read FChildren;
  end;

  TFHIRProjectsView = class;

  { TProjectTreeManager }

  TProjectTreeManager = class (TTreeManager<TFHIRProjectNode>)
  private
    FView : TFHIRProjectsView;
  public
    function doubleClickEdit : boolean; override;
    function allowedOperations(item : TFHIRProjectNode) : TNodeOperationSet; override;
    function loadData : boolean; override;

    procedure buildMenu; override;
    function getImageIndex(item : TFHIRProjectNode) : integer; override;
    function getCellText(item : TFHIRProjectNode) : String; override;
//    function getCellColors(item : TFHIRProjectNode; col : integer; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TFHIRProjectNode) : String; override;

    function addItem(mode : String) : TFHIRProjectNode; override;
    function editItem(item : TFHIRProjectNode; mode : String) : boolean; override;
    procedure deleteItem(item : TFHIRProjectNode); override;
    function executeItem(item : TFHIRProjectNode; mode : String) : boolean; override;
  end;


  { TFHIRProjectsView }

  TFHIRProjectsView = class(TFslObject)
  private
    FControlFile: String;
    FManager : TProjectTreeManager;
    function GetProjects: TFslList<TFHIRProjectNode>;
    function GetTree: TTreeView;
    //function GetServerList: TFslList<TFHIRServerEntry>;
    procedure SetTree(AValue: TTreeView);
  public
    constructor Create(settings : TIniFile);
    destructor Destroy; override;

    property ControlFile : String read FControlFile write FControlFile;
    property Tree : TTreeView read GetTree write SetTree;
    property Projects : TFslList<TFHIRProjectNode> read GetProjects;
    //property OnOpenServer : TOpenServerEvent read FOnOpenServer write FOnOpenServer;
    //property OnEditServer : TOpenServerEvent read FOnEditServer write FOnEditServer;

    procedure load;
    procedure refresh;
    procedure saveStatus;

    //function FetchServer(sender : TObject; name : String) : TFHIRServerEntry;
    //
    procedure addProject(project : TFHIRProjectNode);
    //procedure updateServer(server, newDetails : TFHIRServerEntry);
  end;

implementation

uses
  ftk_worker_server;

{ TFHIRProjectNode }

function TFHIRProjectNode.getChildCount: integer;
begin
  result := FChildren.count;
end;

function TFHIRProjectNode.getChild(index: integer): TFslTreeNode;
begin
  result := FChildren[index];

end;

constructor TFHIRProjectNode.Create;
begin
  inherited Create;
  FChildren := TFslList<TFHIRProjectNode>.create;

end;

destructor TFHIRProjectNode.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
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
    for i := 0 to json.forceArr['children'].count - 1 do
       result.FChildren.add(TFHIRProjectNode.fromJson(json.forceArr['children'].Obj[i]));
    result.link;
  finally
    result.free;
  end;
end;

{ TProjectTreeManager }

function TProjectTreeManager.doubleClickEdit: boolean;
begin
  Result := false;
end;

function TProjectTreeManager.allowedOperations(item: TFHIRProjectNode): TNodeOperationSet;
begin
  if item <> nil then
    result := [opDelete, opEdit, opExecute]
  else
    result := [];
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
begin
  inherited buildMenu;
  registerMenuEntry('Open', ICON_EXECUTE, copExecute);
  registerMenuEntry('Delete', ICON_DELETE, copDelete);
end;

function TProjectTreeManager.getImageIndex(item: TFHIRProjectNode): integer;
begin
  case item.kind of
    pnkProject : result := ICON_PACKAGE;
    pnkFolder : result := ICON_FOLDER;
    pnkFile : result := ICON_FILE;
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
  if (item.address <> '') then
    Result := item.name+' ('+item.address+')'
  else
    Result := item.name;
end;

function TProjectTreeManager.addItem(mode: String): TFHIRProjectNode;
begin
  Result := inherited AddItem(mode);
end;

function TProjectTreeManager.editItem(item: TFHIRProjectNode; mode: String): boolean;
begin
  // FView.OnEditServer(FView, item);
end;

procedure TProjectTreeManager.deleteItem(item: TFHIRProjectNode);
begin
  inherited DeleteItem(item);
end;

function TProjectTreeManager.executeItem(item: TFHIRProjectNode; mode: String): boolean;
begin
  // FView.OnOpenServer(Fview, item);
  Result := false;
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

function TFHIRProjectsView.GetProjects: TFslList<TFHIRProjectNode>;
begin
  result := FManager.Data;
end;

//function TFHIRProjectsView.GetServerList: TFslList<TFHIRServerEntry>;
//begin
//  result := FManager.Data;
//end;

procedure TFHIRProjectsView.SetTree(AValue: TTreeView);
begin
  FManager.Tree := AValue;
end;

procedure TFHIRProjectsView.load;
begin
  try
    FManager.doLoad();
  except
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
    json.forceArr['projects'].add(project.toJson);

    BytesToFile(TJSONWriter.writeObject(json, true), FControlFile);
  finally
    json.free;
  end;
  FManager.doLoad;
end;

//function TFHIRProjectsView.FetchServer(sender: TObject; name: String): TFHIRServerEntry;
//var
//  t : TFHIRServerEntry;
//begin
//  result := nil;
//  for t in Fmanager.Data do
//    if (t.Name = name) or (t.URL = name) then
//      exit(t);
//end;
//
//procedure TFHIRProjectsView.addServer(server: TFHIRServerEntry);
//begin
//  server.id := NewGuidId;
//  FManager.Data.Insert(0, server.link);
//  save;
//  FManager.doLoad();
//end;
//
//procedure TFHIRProjectsView.updateServer(server, newDetails: TFHIRServerEntry);
//begin
//  server.assign(newDetails);
//  save;
//  FManager.doLoad();
//  if (server.workerObject <> nil) then
//    (server.workerObject as TServerWorker).serverChanged;
//end;

end.
