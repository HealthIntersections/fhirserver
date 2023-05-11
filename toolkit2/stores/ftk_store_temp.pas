unit ftk_store_temp;

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
    function link : TFHIRToolkitTemporaryStorage; overload;

    // this is called immediately after opening a file, to save the current list of open files
    // it aims to be reliable storage based on a normal file system
    function startOpenFileList() : TJsonArray;
    procedure storeOpenFile(list : TJsonArray; session : TToolkitEditSession);
    procedure finishOpenFileList(list : TJsonArray);

    // this is called regularly on any open file
    procedure storeContent(guid : String; original : boolean; bytes : TBytes);

    // this is called at start up to reload what was open
    procedure fetchOpenList(sessions : TFslList<TToolkitEditSession>);
    function fetchContent(guid : String) : TBytes;

    procedure getMRUList(list : TStrings);
    procedure getMRUListRaw(list : TStrings);
    function getMRU(index : integer) : String;
    procedure addToMru(address, description : String);
    procedure removeFromMRU(address : String);

    procedure getURLList(list : TStrings);
    procedure addURL(url : String);
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

function TFHIRToolkitTemporaryStorage.link: TFHIRToolkitTemporaryStorage;
begin
  result := TFHIRToolkitTemporaryStorage(inherited link);
end;

function TFHIRToolkitTemporaryStorage.startOpenFileList() : TJsonArray;
begin
  result := TJsonArray.create;
end;

procedure TFHIRToolkitTemporaryStorage.storeOpenFile(list : TJsonArray; session : TToolkitEditSession);
begin
  list.add(sessionTOJson(session));
end;

procedure TFHIRToolkitTemporaryStorage.finishOpenFileList(list : TJsonArray);
var
  cnt : TBytes;
  fn, fnn : String;
begin
  cnt := TJSONWriter.writeArray(list, true);
  list.free;

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
    if session.KnownToBeDeleted then
      result.bool['is-deleted'] := session.KnownToBeDeleted;
    if session.NoCheckCurrency then
      result.bool['no-check'] := session.NoCheckCurrency;
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
    result.KnownToBeDeleted := json.bool['is-deleted'];
    result.NoCheckCurrency := json.bool['no-check'];
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

procedure TFHIRToolkitTemporaryStorage.getMRUListRaw(list: TStrings);
begin
  list.clear;
  if FileExists(FFolder+'mrulist.cfg') then
    list.LoadFromFile(FFolder+'mrulist.cfg');
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

procedure TFHIRToolkitTemporaryStorage.getURLList(list: TStrings);
begin
  if FileExists(FFolder+'urllist.cfg') then
    list.LoadFromFile(FFolder+'urllist.cfg');
end;

procedure TFHIRToolkitTemporaryStorage.addURL(url : String);
var
  ts : TStringList;
  s : String;
  i : integer;
begin
  ts := TStringList.create;
  try
    if FileExists(FFolder+'urllist.cfg') then
      ts.LoadFromFile(FFolder+'urllist.cfg');
    i := ts.IndexOf(url);
    if (i > -1) then
      ts.Delete(i);
    ts.Insert(0, url);
    ts.SaveToFile(FFolder+'urllist.cfg');
  finally
    ts.free;
  end;
end;

end.

