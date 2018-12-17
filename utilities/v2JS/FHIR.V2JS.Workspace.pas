unit FHIR.V2JS.Workspace;

interface

uses
  SysUtils, Classes, IniFiles,
  FHIR.Support.Base, FHIR.Support.Utilities;

Const
  UI_NAME = 'Workspace';

Type
  TV2JSWorkspaceFile = class (TFSLObject)
  private
    FFilename: String;
    FOpen: boolean;
    FRow: integer;
    FIsDirty: boolean;
    function status : String;
  public
    constructor Create(filename : String); overload;
    constructor Create(filename : String; status : string); overload; // internal use only
    function link : TV2JSWorkspaceFile; overload;
    property filename : String read FFilename write FFilename;
    property open : boolean read FOpen write FOpen;
    property row : integer read FRow write FRow;

    property isDirty : boolean read FIsDirty write FIsDirty;
  end;

  TV2JSWorkspace = class (TFSLObject)
  private
    FFilename: String;
    FName: String;
    FMessages: TFslList<TV2JSWorkspaceFile>;
    FMaps: TFslList<TV2JSWorkspaceFile>;
    FResources: TFslList<TV2JSWorkspaceFile>;
    FScripts: TFslList<TV2JSWorkspaceFile>;
    FTemplates: TFslList<TV2JSWorkspaceFile>;

    function hasFile(fn : String; list : TFslList<TV2JSWorkspaceFile>) : boolean;
    function GetFolder: String;
  public
    constructor Create; overload; override;
    constructor Create(filename : String); overload;

    destructor Destroy; override;
    function link : TV2JSWorkspace; overload;

    property name : String read FName write FName;
    property fileName : String read FFilename write FFilename;
    property folder : String read GetFolder;

    property messages : TFslList<TV2JSWorkspaceFile> read FMessages;
    property resources : TFslList<TV2JSWorkspaceFile> read FResources;
    property scripts : TFslList<TV2JSWorkspaceFile> read FScripts;
    property maps : TFslList<TV2JSWorkspaceFile> read FMaps;
    property templates : TFslList<TV2JSWorkspaceFile> read FTemplates;

    function includesFile(fn : String) : boolean;

    procedure reload;
    procedure save;
  end;

implementation

{ TV2JSWorkspace }

constructor TV2JSWorkspace.Create;
begin
  inherited;
  FMessages := TFslList<TV2JSWorkspaceFile>.create;
  FMaps := TFslList<TV2JSWorkspaceFile>.create;
  FResources := TFslList<TV2JSWorkspaceFile>.create;
  FScripts := TFslList<TV2JSWorkspaceFile>.create;
  FTemplates := TFslList<TV2JSWorkspaceFile>.create;
end;

constructor TV2JSWorkspace.Create(filename: String);
begin
  Create;
  FFilename := filename;
  reload;
end;

destructor TV2JSWorkspace.Destroy;
begin
  FMessages.Free;
  FMaps.Free;
  FResources.Free;
  FScripts.Free;
  FTemplates.Free;
  inherited;
end;

function TV2JSWorkspace.GetFolder: String;
begin
  result := ExtractFilePath(FFilename);
end;

function TV2JSWorkspace.hasFile(fn: String; list: TFslList<TV2JSWorkspaceFile>): boolean;
var
  s : String;
  item : TV2JSWorkspaceFile;
begin
  s := makeRelativePath(fn, ExtractFilePath(filename));
  for item in list do
    if s = item.filename then
      exit(true);
  result := false;
end;

function TV2JSWorkspace.includesFile(fn: String): boolean;
begin
  if hasFile(fn, messages) then
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

function TV2JSWorkspace.link: TV2JSWorkspace;
begin
  result := TV2JSWorkspace(inherited link);
end;

procedure TV2JSWorkspace.reload;
var
  ini : TIniFile;
  st : TStringList;
  s : String;
begin
  st := TStringList.create;
  ini := TIniFile.Create(FFilename);
  try
    FName := ini.ReadString('Workspace', 'Name', '');
    if FName = '' then
    begin
      FName := 'New '+UI_NAME;
      ini.WriteString('Workspace', 'Name', FName);
      ini.WriteDateTime('Workspace', 'Created', now);
    end;
    st.clear;
    ini.ReadSection('Messages', st);
    for s in st do
      FMessages.Add(TV2JSWorkspaceFile.Create(s, ini.ReadString('Messages', s, '')));
    st.clear;
    ini.ReadSection('Resources', st);
    for s in st do
      FResources.Add(TV2JSWorkspaceFile.Create(s, ini.ReadString('Resources', s, '')));
    st.clear;
    ini.ReadSection('Scripts', st);
    for s in st do
      FScripts.Add(TV2JSWorkspaceFile.Create(s, ini.ReadString('Scripts', s, '')));
    st.clear;
    ini.ReadSection('Maps', st);
    for s in st do
      FMaps.Add(TV2JSWorkspaceFile.Create(s, ini.ReadString('Maps', s, '')));
    st.clear;
    ini.ReadSection('Templates', st);
    for s in st do
      FTemplates.Add(TV2JSWorkspaceFile.Create(s, ini.ReadString('Templates', s, '')));
  finally
    st.free;
    ini.Free;
  end;
end;


procedure TV2JSWorkspace.save;
var
  ini : TIniFile;
  f : TV2JSWorkspaceFile;
begin
  ini := TIniFile.Create(FFilename);
  try
    ini.WriteString('Workspace', 'Name', FName);
    ini.WriteDateTime('Workspace', 'Updated', now);

    ini.EraseSection('Messages');
    for f in FMessages do
      ini.WriteString('Messages', f.filename, f.status);
    ini.EraseSection('Resources');
    for f in FResources do
      ini.WriteString('Resources', f.filename, f.status);
    ini.EraseSection('Scripts');
    for f in FScripts do
      ini.WriteString('Scripts', f.filename, f.status);
    ini.EraseSection('Maps');
    for f in FMaps do
      ini.WriteString('Maps', f.filename, f.status);
    ini.EraseSection('Templates');
    for f in FTemplates do
      ini.WriteString('Templates', f.filename, f.status);
  finally
    ini.Free;
  end;
end;

{ TV2JSWorkspaceFile }

constructor TV2JSWorkspaceFile.Create(filename: String);
begin
  Create;
  FFilename := filename;
  FOpen := false;
  FRow := 0;
end;

constructor TV2JSWorkspaceFile.Create(filename, status: string);
var
  l, r : String;
begin
  Create;
  FFilename := filename;
  StringSplit(status, ':', l, r);
  FOpen := l = 'open';
  FRow := StrToIntDef(r, 0);
end;

function TV2JSWorkspaceFile.link: TV2JSWorkspaceFile;
begin
  result := TV2JSWorkspaceFile(inherited link);
end;

function TV2JSWorkspaceFile.status: String;
begin
  if FOpen then
    result := 'open:'+inttostr(FRow)
  else
    result := 'closed:'+inttostr(FRow);
end;

end.
