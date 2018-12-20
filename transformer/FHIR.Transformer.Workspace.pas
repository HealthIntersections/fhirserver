unit FHIR.Transformer.Workspace;

interface

uses
  SysUtils, Classes, IniFiles,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.MXML;

Const
  UI_NAME = 'Workspace';

type
  TTransformerFormat = (fmtV2, fmtCDA, fmtResource, fmtJS, fmtMap, fmtTemplate);

function detectFormat(fn : String) : TTransformerFormat;

Type
  TWorkspaceFile = class (TFSLObject)
  private
    FFilename: String;
    FRow: integer;
    FIsDirty: boolean;
    FKey: integer;
    FFormat: TTransformerFormat;
    function GetTitle: string;
  public
    constructor Create(filename : String; format : TTransformerFormat); overload;
    constructor Create(filename : String; format : TTransformerFormat; row : integer); overload;
    function link : TWorkspaceFile; overload;
    property format : TTransformerFormat read FFormat;
    property filename : String read FFilename write FFilename;
    property title : string read GetTitle; // derived from filename

    property key : integer read FKey write FKey; // this is not persisted; just used locally

    property row : integer read FRow write FRow;  // persisted in UI settings, not the worksapce (version control)
    property isDirty : boolean read FIsDirty write FIsDirty; // status
  end;

  TWorkspace = class (TFSLObject)
  private
    FFolder: String;
    FName: String;
    FMessages: TFslList<TWorkspaceFile>;
    FDocuments: TFslList<TWorkspaceFile>;
    FMaps: TFslList<TWorkspaceFile>;
    FResources: TFslList<TWorkspaceFile>;
    FScripts: TFslList<TWorkspaceFile>;
    FTemplates: TFslList<TWorkspaceFile>;
    FEventType: integer;
    FSource: String;

    function hasFile(fn : String; list : TFslList<TWorkspaceFile>) : boolean;
    function findFile(fn : String; list : TFslList<TWorkspaceFile>) : TWorkspaceFile;
    function GetFileByKey(key: integer): TWorkspaceFile;
  public
    constructor Create(folder : String);

    destructor Destroy; override;
    function link : TWorkspace; overload;

    property name : String read FName write FName;
    property folder : String read FFolder write FFolder;

    property EventType : integer read FEventType write FEventType;
    property Source : String read FSource write FSource;

    property messages : TFslList<TWorkspaceFile> read FMessages;
    property documents : TFslList<TWorkspaceFile> read FDocuments;
    property resources : TFslList<TWorkspaceFile> read FResources;
    property scripts : TFslList<TWorkspaceFile> read FScripts;
    property maps : TFslList<TWorkspaceFile> read FMaps;
    property templates : TFslList<TWorkspaceFile> read FTemplates;

    function includesFile(fn : String) : boolean;
    function findFileByName(fn : String) : TWorkspaceFile;
    property FileByKey[key : integer] : TWorkspaceFile read GetFileByKey;

    procedure reload;
    procedure save;
    procedure ClearOpenFiles;
    procedure OpenFile(f : TWorkspaceFile);
    function listOpenFiles : TFslList<TWorkspaceFile>;
  end;

implementation

var
  GLastKey : integer = 0;

function detectFormat(fn : String) : TTransformerFormat;
var
  s : String;
  x : TMXmlDocument;
begin
  s := FileToString(fn, TEncoding.UTF8);
  s := s.Trim;
  if s.StartsWith('MSH|') then
    exit(fmtV2);
  if s.StartsWith('map ') then
    exit(fmtMap);
  if s.StartsWith('{') and not s.Contains('{%') then
    exit(fmtResource);
  if s.StartsWith('<') then
  begin
    try
      x := TMXmlParser.parse(s, [xpResolveNamespaces, xpDropWhitespace, xpDropComments]);
      try
        if x.docElement.NamespaceURI = 'http://hl7.org/fhir' then
          exit(fmtResource);
        if x.docElement.NamespaceURI = 'urn:hl7-org:v3' then
          exit(fmtCDA);
      finally
        x.Free;
      end;
    except
      // nothing
    end;
  end;
  if s.StartsWith('//') or s.StartsWith('function') then
    exit(fmtJs);
  if s.Contains('{%') or s.Contains('{{') then
    exit(fmtTemplate);
  raise Exception.create('unknown format');
end;

{ TWorkspace }

procedure TWorkspace.ClearOpenFiles;
var
  iniT : TIniFile;
begin
  iniT := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.status']));
  try
    iniT.EraseSection('open-files');
  finally
    iniT.Free;
  end;
end;

constructor TWorkspace.Create(folder: String);
begin
  inherited Create;
  FMessages := TFslList<TWorkspaceFile>.create;
  FDocuments := TFslList<TWorkspaceFile>.create;
  FMaps := TFslList<TWorkspaceFile>.create;
  FResources := TFslList<TWorkspaceFile>.create;
  FScripts := TFslList<TWorkspaceFile>.create;
  FTemplates := TFslList<TWorkspaceFile>.create;
  FFolder := folder;
  reload;
end;

destructor TWorkspace.Destroy;
begin
  FMessages.Free;
  FDocuments.Free;
  FMaps.Free;
  FResources.Free;
  FScripts.Free;
  FTemplates.Free;
  inherited;
end;

function TWorkspace.findFile(fn: String; list: TFslList<TWorkspaceFile>): TWorkspaceFile;
var
  s : String;
  item : TWorkspaceFile;
begin
  s := makeRelativePath(fn, folder);
  for item in list do
    if s = item.filename then
      exit(item);
  result := nil;
end;

function TWorkspace.findFileByName(fn: String): TWorkspaceFile;
var
  o : TWorkspaceFile;
begin
  o := findFile(fn, messages);
  if (o <> nil) then
    exit(o);
  o := findFile(fn, documents);
  if (o <> nil) then
    exit(o);
  o := findFile(fn, resources);
  if (o <> nil) then
    exit(o);
  o := findFile(fn, scripts);
  if (o <> nil) then
    exit(o);
  o := findFile(fn, maps);
  if (o <> nil) then
    exit(o);
  o := findFile(fn, templates);
  if (o <> nil) then
    exit(o);
  result := nil;
end;

function TWorkspace.GetFileByKey(key: integer): TWorkspaceFile;
var
  item : TWorkspaceFile;
begin
  result := nil;
  for item in messages do
    if item.key = key then
      exit(item);
  for item in documents do
    if item.key = key then
      exit(item);
  for item in resources do
    if item.key = key then
      exit(item);
  for item in scripts do
    if item.key = key then
      exit(item);
  for item in maps do
    if item.key = key then
      exit(item);
  for item in templates do
    if item.key = key then
      exit(item);
end;


function TWorkspace.hasFile(fn: String; list: TFslList<TWorkspaceFile>): boolean;
var
  s : String;
  item : TWorkspaceFile;
begin
  s := makeRelativePath(fn, folder);
  for item in list do
    if s = item.filename then
      exit(true);
  result := false;
end;

function TWorkspace.includesFile(fn: String): boolean;
begin
  if hasFile(fn, messages) then
    exit(true);
  if hasFile(fn, documents) then
    exit(true);
  if hasFile(fn, resources) then
    exit(true);
  if hasFile(fn, scripts) then
    exit(true);
  if hasFile(fn, maps) then
    exit(true);
  if hasFile(fn, templates) then
    exit(true);
  result := false;
end;

function TWorkspace.link: TWorkspace;
begin
  result := TWorkspace(inherited link);
end;

function TWorkspace.listOpenFiles: TFslList<TWorkspaceFile>;
var
  iniT : TIniFile;
  st : TStringList;
  s : String;
  f : TWorkspaceFile;
begin
  st := TStringList.Create;
  iniT := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.status']));
  try
    iniT.ReadSection('open-files', st);
    result := TFslList<TWorkspaceFile>.create;
    try
      for s in st do
      begin
        f := findFileByName(s);
        if (f <> nil) then
          result.Add(f.link);
      end;
      result.link;
    finally
      result.Free;
    end;
  finally
    st.Free;
    iniT.Free;
  end;
end;

procedure TWorkspace.OpenFile(f: TWorkspaceFile);
var
  iniT : TIniFile;
begin
  iniT := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.status']));
  try
    iniT.WriteString('open-files', f.filename, '1');
  finally
    iniT.Free;
  end;
end;

procedure TWorkspace.reload;
var
  iniVC, iniT : TIniFile;
  st : TStringList;
  s : String;
  fmt : String;
  row : integer;
begin
  st := TStringList.create;
  iniVC := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.control']));
  iniT := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.status']));
  try
    FName := iniVC.ReadString('Workspace', 'Name', '');
    if FName = '' then
    begin
      FName := 'New '+UI_NAME;
      iniVC.WriteString('Workspace', 'Name', FName);
      iniVC.WriteDateTime('Workspace', 'Created', now);
    end;
    FEventType := iniT.ReadInteger('Status', 'EventType', -1);
    FSource := iniT.ReadString('Status', 'Source', '');
    st.clear;
    iniVC.ReadSection('Files', st);
    for s in st do
    begin
      fmt := iniVC.ReadString('Files', s, '');
      row := iniT.ReadInteger('Files', s, 0);
      if fmt = 'v2' then
        FMessages.Add(TWorkspaceFile.Create(s, fmtV2, row))
      else if fmt = 'cda' then
        FDocuments.Add(TWorkspaceFile.Create(s, fmtCDA, row))
      else if fmt = 'fhir' then
        FResources.Add(TWorkspaceFile.Create(s, fmtResource, row))
      else if fmt = 'js' then
        FScripts.Add(TWorkspaceFile.Create(s, fmtJS, row))
      else if fmt = 'map' then
        FMaps.Add(TWorkspaceFile.Create(s, fmtMap, row))
      else if fmt = 'liquid' then
        FTemplates.Add(TWorkspaceFile.Create(s, fmtTemplate, row));
    end;
    if FEventType = -1 then
      if FMessages.Count > 0 then
        FEventType := 0
      else if FDocuments.Count > 0 then
        FEventType := 1;
  finally
    st.free;
    iniVC.Free;
    iniT.Free;
  end;
end;


procedure TWorkspace.save;
var
  iniVC, iniT : TIniFile;
  f : TWorkspaceFile;
begin
  if FEventType = -1 then
    if FMessages.Count > 0 then
      FEventType := 0
    else if FDocuments.Count > 0 then
      FEventType := 1;

  iniVC := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.control']));
  iniT := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.status']));
  try
    iniVC.WriteString('Workspace', 'Name', FName);
    iniVC.WriteDateTime('Workspace', 'Updated', now);
    iniT.WriteInteger('Status', 'EventType', FEventType);
    iniT.WriteString('Status', 'Source', FSource);

    iniVC.EraseSection('Files');
    for f in FMessages do
    begin
      iniVC.WriteString('Files', f.filename, 'v2');
      iniT.WriteInteger('Files', f.filename, f.row);
    end;
    iniVC.EraseSection('Documents');
    for f in FDocuments do
    begin
      iniVC.WriteString('Files', f.filename, 'cda');
      iniT.WriteInteger('Files', f.filename, f.row);
    end;
    iniVC.EraseSection('Resources');
    for f in FResources do
    begin
      iniVC.WriteString('Files', f.filename, 'fhir');
      iniT.WriteInteger('Files', f.filename, f.row);
    end;
    iniVC.EraseSection('Scripts');
    for f in FScripts do
    begin
      iniVC.WriteString('Files', f.filename, 'js');
      iniT.WriteInteger('Files', f.filename, f.row);
    end;
    iniVC.EraseSection('Maps');
    for f in FMaps do
    begin
      iniVC.WriteString('Files', f.filename, 'map');
      iniT.WriteInteger('Files', f.filename, f.row);
    end;
    iniVC.EraseSection('Templates');
    for f in FTemplates do
    begin
      iniVC.WriteString('Files', f.filename, 'liquid');
      iniT.WriteInteger('Files', f.filename, f.row);
    end;
  finally
    iniVC.Free;
    iniT.Free;
  end;
end;

{ TWorkspaceFile }

constructor TWorkspaceFile.Create(filename: String; format : TTransformerFormat);
begin
  Create;
  Inc(GLastKey);
  FKey := GLastKey;
  FFilename := filename;
  FFormat := format;
  FRow := 0;
end;

constructor TWorkspaceFile.Create(filename : string; format : TTransformerFormat; row : integer);
var
  l, r : String;
begin
  Create;
  Inc(GLastKey);
  FKey := GLastKey;
  FFilename := filename;
  FFormat := format;
  FRow := row;
end;

function TWorkspaceFile.GetTitle: string;
begin
  result := FFilename;
end;

function TWorkspaceFile.link: TWorkspaceFile;
begin
  result := TWorkspaceFile(inherited link);
end;

end.
