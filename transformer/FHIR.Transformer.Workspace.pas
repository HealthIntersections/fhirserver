unit FHIR.Transformer.Workspace;

interface

uses
  SysUtils, Classes, IniFiles, Generics.Collections,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.MXML;

Const
  UI_NAME = 'Workspace';

type
  TTransformerFormat = (fmtV2, fmtCDA, fmtResource, fmtJS, fmtMap, fmtTemplate);
  TTrasnformOutcomeMode = (tomIgnore, tomSaveTo, tomCompare);

function detectFormat(fn : String) : TTransformerFormat;

Type
  TBreakPointInfo = class (TFSLObject)
  private
    Fline: integer;
    Finvalid: boolean;
  public
    constructor create(line : integer);
    property line : integer read Fline write Fline;
    property invalid : boolean read Finvalid write Finvalid;
  end;

  TWorkspaceFile = class (TFSLObject)
  private
    FFilename: String;
    FRow: integer;
    FIsDirty: boolean;
    FFormat: TTransformerFormat;
    FErrorLine : Integer;
    FParsed: TObject;
    FBreakPoints: TFslList<TBreakPointInfo>;
    function GetTitle: string;
    function breakpointSummary : String;
  public
    constructor Create(filename : String; format : TTransformerFormat); overload;
    constructor Create(filename : String; format : TTransformerFormat; row : integer; bpl : String); overload;
    destructor Destroy; override;
    function link : TWorkspaceFile; overload;
    property format : TTransformerFormat read FFormat;
    property filename : String read FFilename write FFilename;
    property title : string read GetTitle; // derived from filename
    property row : integer read FRow write FRow;  // persisted in UI settings, not the worksapce (version control)
    property parsed : TObject read FParsed write FParsed;

    property BreakPoints: TFslList<TBreakPointInfo> read FBreakPoints;
    function hasBreakPoint(line : integer; var bpi : TBreakPointInfo): boolean;
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
    FScript: String;
    FTarget: String;
    FOutcome: TTrasnformOutcomeMode;

    function hasFile(fn : String; list : TFslList<TWorkspaceFile>) : boolean;
    function findFile(fn : String; list : TFslList<TWorkspaceFile>) : TWorkspaceFile;
    function findObject(obj: TObject; list: TFslList<TWorkspaceFile>): TWorkspaceFile;
  public
    constructor Create(folder : String);

    destructor Destroy; override;
    function link : TWorkspace; overload;

    property name : String read FName write FName;
    property folder : String read FFolder write FFolder;

    property EventType : integer read FEventType write FEventType;
    property Source : String read FSource write FSource;
    property Script : String read FScript write FScript;
    property Target : String read FTarget write FTarget;
    property Outcome : TTrasnformOutcomeMode read FOutcome write FOutcome;

    property messages : TFslList<TWorkspaceFile> read FMessages;
    property documents : TFslList<TWorkspaceFile> read FDocuments;
    property resources : TFslList<TWorkspaceFile> read FResources;
    property scripts : TFslList<TWorkspaceFile> read FScripts;
    property maps : TFslList<TWorkspaceFile> read FMaps;
    property templates : TFslList<TWorkspaceFile> read FTemplates;

    function includesFile(fn : String) : boolean;
    function findFileByName(fn : String) : TWorkspaceFile;
    function findFileByParsedObject(Obj : TObject) : TWorkspaceFile;

    procedure reload;
    procedure save;
    procedure ClearOpenFiles;
    procedure OpenFile(f : TWorkspaceFile);
    function listOpenFiles : TFslList<TWorkspaceFile>;
    procedure ClearParsedObjects;
  end;

implementation

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
      on e : Exception do
      begin
        // nothing
      end;
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

procedure TWorkspace.ClearParsedObjects;
var
  f : TWorkspaceFile;
begin
  for f in FMessages do
   f.parsed := nil;
  for f in FDocuments do
   f.parsed := nil;
  for f in FMaps do
   f.parsed := nil;
  for f in FScripts do
   f.parsed := nil;
  for f in FTemplates do
   f.parsed := nil;
  for f in FResources do
   f.parsed := nil;
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

function TWorkspace.findObject(obj : TObject; list: TFslList<TWorkspaceFile>): TWorkspaceFile;
var
  item : TWorkspaceFile;
begin
  for item in list do
    if obj = item.parsed then
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


function TWorkspace.findFileByParsedObject(Obj: TObject): TWorkspaceFile;
var
  o : TWorkspaceFile;
begin
  o := findObject(Obj, messages);
  if (o <> nil) then
    exit(o);
  o := findObject(Obj, documents);
  if (o <> nil) then
    exit(o);
  o := findObject(Obj, resources);
  if (o <> nil) then
    exit(o);
  o := findObject(Obj, scripts);
  if (o <> nil) then
    exit(o);
  o := findObject(Obj, maps);
  if (o <> nil) then
    exit(o);
  o := findObject(Obj, templates);
  if (o <> nil) then
    exit(o);
  result := nil;
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
  bpl : String;
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
    FScript := iniT.ReadString('Status', 'Script', '');
    FTarget := iniT.ReadString('Status', 'Target', '');
    FOutcome := TTrasnformOutcomeMode(iniT.ReadInteger('Status', 'Outcome', 0));
    st.clear;
    iniVC.ReadSection('Files', st);
    for s in st do
    begin
      fmt := iniVC.ReadString('Files', s, '');
      row := iniT.ReadInteger(s, 'Row', 0);
      bpl := iniT.ReadString(s, 'BreakPoints', '');
      if fmt = 'v2' then
        FMessages.Add(TWorkspaceFile.Create(s, fmtV2, row, bpl))
      else if fmt = 'cda' then
        FDocuments.Add(TWorkspaceFile.Create(s, fmtCDA, row, bpl))
      else if fmt = 'fhir' then
        FResources.Add(TWorkspaceFile.Create(s, fmtResource, row, bpl))
      else if fmt = 'js' then
        FScripts.Add(TWorkspaceFile.Create(s, fmtJS, row, bpl))
      else if fmt = 'map' then
        FMaps.Add(TWorkspaceFile.Create(s, fmtMap, row, bpl))
      else if fmt = 'liquid' then
        FTemplates.Add(TWorkspaceFile.Create(s, fmtTemplate, row, bpl));
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
    iniT.WriteString('Status', 'Script', FScript);
    iniT.WriteString('Status', 'Target', FTarget);
    iniT.WriteInteger('Status', 'Outcome', Ord(FOutcome));

    iniVC.EraseSection('Files');
    for f in FMessages do
    begin
      iniVC.WriteString('Files', f.filename, 'v2');
      iniT.WriteInteger(f.filename, 'Row', f.row);
    end;
    iniVC.EraseSection('Documents');
    for f in FDocuments do
    begin
      iniVC.WriteString('Files', f.filename, 'cda');
      iniT.WriteInteger(f.filename, 'Row', f.row);
    end;
    iniVC.EraseSection('Resources');
    for f in FResources do
    begin
      iniVC.WriteString('Files', f.filename, 'fhir');
      iniT.WriteInteger(f.filename, 'Row', f.row);
    end;
    iniVC.EraseSection('Scripts');
    for f in FScripts do
    begin
      iniVC.WriteString('Files', f.filename, 'js');
      iniT.WriteInteger(f.filename, 'Row', f.row);
      iniT.WriteString(f.filename, 'BreakPoints', f.breakpointSummary);
    end;
    iniVC.EraseSection('Maps');
    for f in FMaps do
    begin
      iniVC.WriteString('Files', f.filename, 'map');
      iniT.WriteInteger(f.filename, 'Row', f.row);
      iniT.WriteString(f.filename, 'BreakPoints', f.breakpointSummary);
    end;
    iniVC.EraseSection('Templates');
    for f in FTemplates do
    begin
      iniVC.WriteString('Files', f.filename, 'liquid');
      iniT.WriteInteger(f.filename, 'Row', f.row);
      iniT.WriteString(f.filename, 'BreakPoints', f.breakpointSummary);
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
  FFilename := filename;
  FFormat := format;
  FRow := 0;
  FBreakPoints := TFslList<TBreakPointInfo>.create;
end;

function TWorkspaceFile.breakpointSummary: String;
var
  b : TFslStringBuilder;
  bpi : TBreakPointInfo;
begin
  b := TFslStringBuilder.Create;
  try
    for bpi in FBreakPoints do
      b.CommaAdd(inttostr(bpi.line));
    result := b.toString;
  finally
    b.Free;
  end;
end;

constructor TWorkspaceFile.Create(filename : string; format : TTransformerFormat; row : integer; bpl : String);
var
  l, r : String;
  s : String;
begin
  Create;
  FFilename := filename;
  FFormat := format;
  FRow := row;
  FBreakPoints := TFslList<TBreakPointInfo>.create;
  for s in bpl.Split([',']) do
    if StringIsInteger32(s) then
      FBreakPoints.Add(TBreakPointInfo.create(StrToInt(s)));
end;

destructor TWorkspaceFile.Destroy;
begin
  FBreakpoints.free;
  inherited;
end;

function TWorkspaceFile.GetTitle: string;
begin
  result := FFilename;
end;

function TWorkspaceFile.hasBreakPoint(line: integer; var bpi : TBreakPointInfo): boolean;
var
  t : TBreakPointInfo;
begin
  result := false;
  for t in FBreakPoints do
  begin
    if t.line = line then
    begin
      bpi := t;
      exit(true);
    end;
  end;
end;

function TWorkspaceFile.link: TWorkspaceFile;
begin
  result := TWorkspaceFile(inherited link);
end;

{ TBreakPointInfo }

constructor TBreakPointInfo.create(line: integer);
begin
  Inherited Create;
  FLine := line;
end;

end.
