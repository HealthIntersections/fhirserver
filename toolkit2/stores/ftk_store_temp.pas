unit ftk_store_temp;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, IniFiles,
  fsl_base, fsl_utilities, fsl_stream, fsl_json, fsl_fpc,
  ftk_context;

type

  { TFHIRToolkitTemporaryStorage }

  TFHIRToolkitTemporaryStorage = class (TFslObject)
  private
    FFolder : String;
    FIni : TIniFile;
    function sessionToJson(session : TToolkitEditSession) : TJsonObject;
    function jsonToSession(json : TJsonObject) : TToolkitEditSession;
    function hasGuid(guid : String; sessions : TFslList<TToolkitEditSession>) : boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    // this is called immediately after opening a file, to save the current list of open files
    // it aims to be reliable storage based on a normal file system
    procedure storeOpenFileList(sessions : TFslList<TToolkitEditSession>);

    // this is called regularly on any open file
    procedure storeContent(guid : String; original : boolean; bytes : TBytes);

    // this is called at start up to reload what was open
    procedure fetchOpenList(sessions : TFslList<TToolkitEditSession>);
    function fetchContent(guid : String) : TBytes;

    procedure getMRUList(list : TStrings);
    function getMRU(index : integer) : String;
    procedure addToMru(address, description : String);
    procedure removeFromMRU(address : String);

  end;

implementation

{ TFHIRToolkitTemporaryStorage }

constructor TFHIRToolkitTemporaryStorage.Create;
begin
  inherited Create;
  FFolder := GetAppConfigDir(false);
end;

destructor TFHIRToolkitTemporaryStorage.Destroy;
begin
  inherited Destroy;
end;

procedure TFHIRToolkitTemporaryStorage.storeOpenFileList(sessions: TFslList<TToolkitEditSession>);
var
  arr : TJsonArray;
  session : TToolkitEditSession;
  cnt : TBytes;
  fn, fnn : String;
begin
  arr := TJsonArray.create;
  try
    for session in sessions do
      arr.add(sessionTOJson(session));
    cnt := TJSONWriter.writeArray(arr, true);
  finally
    arr.free;
  end;

  fn := FFolder+'editor-sessions.json';
  fnn := FFolder+'editor-sessions-new.json';
  if (FileExists(fnn)) then
    DeleteFile(fnn);
  if (FileExists(fn)) then
  begin
    BytesToFile(cnt, fnn);
    DeleteFile(fn);
    RenameFile(fnn, fn);
  end
  else
    BytesToFile(cnt, fn);
end;

procedure TFHIRToolkitTemporaryStorage.fetchOpenList(sessions: TFslList<TToolkitEditSession>);
var
  arr : TJsonArray;
  n : TJsonNode;
  fn, fnn, cnt, s, g : String;
  session : TToolkitEditSession;
begin
  fn := FFolder+'editor-sessions.json';
  fnn := FFolder+'editor-sessions-new.json';
  if (FileExists(fnn)) then
    cnt := FiletoString(fnn, TEncoding.UTF8)
  else if (FileExists(fn)) then
    cnt := FiletoString(fn, TEncoding.UTF8)
  else
    cnt := '';
  if (cnt <> '') then
  begin
    arr := TJSONParser.ParseNode(cnt) as TJsonArray;
    try
      for n in arr do
        sessions.add(jsonToSession(n as TJsonObject));
    finally
      arr.free;
    end;
  end;
  try
    for s in TDirectory.GetFiles(FFolder, '*.cnt') do
    begin
      g := ExtractFileName(s);
      g := g.subString(0, g.IndexOf('.'));
      if (not hasGuid(g, sessions)) then
        deleteFile(s);
    end;

    if (FileExists(fnn)) then
    begin
      if (FileExists(fn)) then
        deleteFile(fn);
      RenameFile(fnn, fn);
    end;
  except
    // nothing here
  end;
end;

procedure TFHIRToolkitTemporaryStorage.storeContent(guid: String; original : boolean; bytes: TBytes);
begin
  if original then
    BytesToFile(bytes, FFolder+guid+'.original.cnt');
  BytesToFile(bytes, FFolder+guid+'.cnt');
end;


function TFHIRToolkitTemporaryStorage.fetchContent(guid: String): TBytes;
begin
  result := FileToBytes(FFolder+guid+'.cnt');
end;


function TFHIRToolkitTemporaryStorage.sessionToJson(session: TToolkitEditSession): TJsonObject;
var
  params : TJsonObject;
  i : integer;
begin
  result := TJsonObject.create;
  try
    result.str['guid'] := session.guid;
    if session.Address <> '' then
      result.str['address'] := session.Address;
    result.str['caption'] := session.Caption;
    if session.NeedsSaving then
      result.bool['needs-saving'] := session.NeedsSaving;
    result.str['type'] := CODES_TSourceEditorKind[session.kind];
    result.str['encoding'] := CODES_TSourceEncoding[session.Encoding];
    result.str['eoln'] := CODES_TSourceLineMarker[session.EndOfLines];
    if session.Timestamp <> 0 then
      result.str['timestamp'] := TFslDateTime.make(session.Timestamp, dttzLocal).toHL7;
    if session.HasBOM then
      result.bool['bom'] := session.HasBOM;
    if (session.Info.Count > 0) then
    begin
      params := result.forceObj['params'];
      for i := 0 to session.Info.count - 1 do
        params.str[session.info.Names[i]] := session.info.ValueFromIndex[i];
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRToolkitTemporaryStorage.jsonToSession(json: TJsonObject): TToolkitEditSession;
var
  n : String;
begin
  result := TToolkitEditSession.create;
  try
    result.guid := json.str['guid'];
    result.Address := json.str['address'];
    result.Caption := json.str['caption'];
    result.NeedsSaving := json.bool['needs-saving'];
    result.kind := TSourceEditorKind(StringArrayIndexOfInsensitive(CODES_TSourceEditorKind, json.str['type']));
    result.Encoding := TSourceEncoding(StringArrayIndexOfInsensitive(CODES_TSourceEncoding, json.str['encoding']));
    result.EndOfLines := TSourceLineMarker(StringArrayIndexOfInsensitive(CODES_TSourceLineMarker, json.str['eoln']));
    result.HasBOM := json.bool['bom'];
    if (json.has('timestamp')) then
      result.Timestamp := TFslDateTime.fromHL7(json.str['timestamp']).DateTime;
    if json.has('params') then
    begin
      for n in json.obj['params'].properties.Keys do
        result.info.AddPair(n, json.obj['params'].str[n]);
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRToolkitTemporaryStorage.hasGuid(guid: String; sessions: TFslList<TToolkitEditSession>): boolean;
var
  session : TToolkitEditSession;
begin
  result := false;
  for session in sessions do
    if session.guid = guid then
      exit(true);
end;


procedure TFHIRToolkitTemporaryStorage.getMRUList(list: TStrings);
var
  ts : TStringList;
  s : String;
begin
  ts := TStringList.create;
  try
    if FileExists(FFolder+'mrulist.cfg') then
      ts.LoadFromFile(FFolder+'mrulist.cfg');
    for s in ts do
      list.add(s.Substring(s.IndexOf('|')+1));
  finally
    ts.free;
  end;
end;

function TFHIRToolkitTemporaryStorage.getMRU(index: integer): String;
var
  ts : TStringList;
  s : String;
begin
  ts := TStringList.create;
  try
    if FileExists(FFolder+'mrulist.cfg') then
      ts.LoadFromFile(FFolder+'mrulist.cfg');
    s := ts[index];
    result := s.Substring(0, s.IndexOf('|'));
  finally
    ts.free;
  end;
end;

procedure TFHIRToolkitTemporaryStorage.addToMru(address, description: String);
var
  ts : TStringList;
  i : integer;
begin
  ts := TStringList.create;
  try
    if FileExists(FFolder+'mrulist.cfg') then
      ts.LoadFromFile(FFolder+'mrulist.cfg');
    for i := ts.count - 1 downto 0 do
      if ts[i].startsWith(address+'|') then
        ts.delete(i);
    ts.insert(0, address+'|'+description);
    ts.SaveTOFile(FFolder+'mrulist.cfg');
  finally
    ts.free;
  end;
end;

procedure TFHIRToolkitTemporaryStorage.removeFromMRU(address: String);
var
  ts : TStringList;
  i : integer;
begin
  ts := TStringList.create;
  try
    if FileExists(FFolder+'mrulist.cfg') then
      ts.LoadFromFile(FFolder+'mrulist.cfg');
    for i := ts.count - 1 downto 0 do
      if ts[i].startsWith(address+'|') then
        ts.delete(i);
    ts.SaveTOFile(FFolder+'mrulist.cfg');
  finally
    ts.free;
  end;
end;

end.

