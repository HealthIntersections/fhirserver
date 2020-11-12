unit FHIR.Transformer.Workspace;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, IniFiles, Generics.Collections,
  fsl_base, fsl_utilities, fsl_stream, fsl_xml;

Const
  UI_NAME = 'Workspace';

type
  TTransformerFormat = (fmtV2, fmtCDA, fmtResource, fmtJS, fmtMap, fmtTemplate, fmtMarkdown);
  TTransformOutcomeMode = (tomIgnore, tomSaveTo, tomCompare);

function detectFormat(fn : String) : TTransformerFormat;

Type
  TBreakPointInfo = class (TFslObject)
  private
    Fline: integer;
    Finvalid: boolean;
  public
    constructor create(line : integer);
    property line : integer read Fline write Fline;
    property invalid : boolean read Finvalid write Finvalid;
  end;

  TCompilerStatus = (csNotCompiled, csOk, csError, csWorking);

  TWorkspace = class;

  TWorkspaceFile = class (TFslObject)
  private
    FFilename: String;
    FRow: integer;
    FIsDirty: boolean;
    FFormat: TTransformerFormat;
    FErrorLine : Integer;
    FBreakPoints: TFslList<TBreakPointInfo>;
    FcompileStatus: TCompilerStatus;
    FWorkspace : TWorkspace; // no link
    FCompiled : TFslObject;
    FFmtInfo : String;
    FErrorMsg: String;
    function GetTitle: string;
    function breakpointSummary : String;
    procedure SetCompiled(const Value: TFslObject);
  public
    constructor Create(workspace : TWorkspace; filename : String; format : TTransformerFormat); overload;
    constructor Create(workspace : TWorkspace; filename : String; format : TTransformerFormat; row : integer; bpl : String); overload;
    destructor Destroy; override;
    function link : TWorkspaceFile; overload;
    property format : TTransformerFormat read FFormat;
    property filename : String read FFilename write FFilename;
    property title : string read GetTitle; // derived from filename
    property row : integer read FRow write FRow;  // persisted in UI settings, not the worksapce (version control)

    property compileStatus : TCompilerStatus read FcompileStatus write FcompileStatus;
    property BreakPoints: TFslList<TBreakPointInfo> read FBreakPoints;
    function hasBreakPoint(line : integer; var bpi : TBreakPointInfo): boolean;

    function stream : TStream;
    function actualName : String;
    function source : String;
    property compiled : TFslObject read FCompiled write SetCompiled;
    property errorLine : integer read FErrorLine write FErrorLine;
    property errorMsg : String read FErrorMsg write FErrorMsg;
    property fmtInfo : String read FFmtInfo write FFmtInfo;
  end;

  TWorkspaceExecConfig = class (TFslObject)
  private
    FFocus: String;
    FScript: String;
    function read : String;
  public
    constructor Create; overload; override;
    constructor Create(src : String); overload;

    function link : TWorkspaceExecConfig; overload;

    function summary : String;

    property script : String read FScript write FScript;
    property focus : String read FFocus write FFocus;
  end;

  TWorkspace = class (TFslObject)
  private
    FFolder: String;
    FName: String;
    FMessages: TFslList<TWorkspaceFile>;
    FDocuments: TFslList<TWorkspaceFile>;
    FMaps: TFslList<TWorkspaceFile>;
    FResources: TFslList<TWorkspaceFile>;
    FScripts: TFslList<TWorkspaceFile>;
    FTemplates: TFslList<TWorkspaceFile>;
    FMarkdowns: TFslList<TWorkspaceFile>;
    FAllFiles: TFslList<TWorkspaceFile>;
    FConfigurations: TFslList<TWorkspaceExecConfig>;

    function hasFile(fn : String; list : TFslList<TWorkspaceFile>) : boolean;
    function findFile(fn : String; list : TFslList<TWorkspaceFile>) : TWorkspaceFile;
    function findObject(obj: TObject; list: TFslList<TWorkspaceFile>): TWorkspaceFile;
  public
    constructor Create(folder : String);

    destructor Destroy; override;
    function link : TWorkspace; overload;

    property name : String read FName write FName;
    property folder : String read FFolder write FFolder;

    property allFiles : TFslList<TWorkspaceFile> read FAllFiles;
    property messages : TFslList<TWorkspaceFile> read FMessages;
    property documents : TFslList<TWorkspaceFile> read FDocuments;
    property resources : TFslList<TWorkspaceFile> read FResources;
    property scripts : TFslList<TWorkspaceFile> read FScripts;
    property maps : TFslList<TWorkspaceFile> read FMaps;
    property templates : TFslList<TWorkspaceFile> read FTemplates;
    property markdowns : TFslList<TWorkspaceFile> read FMarkdowns;
    property configurations : TFslList<TWorkspaceExecConfig> read FConfigurations;

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
  if s.StartsWith('# ') then
    exit(fmtMarkdown);
  if fn.endsWith('.md') then
    exit(fmtMarkdown);

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
   f.compiled := nil;
  for f in FDocuments do
   f.compiled := nil;
  for f in FMaps do
   f.compiled := nil;
  for f in FScripts do
   f.compiled := nil;
  for f in FTemplates do
   f.compiled := nil;
  for f in FMarkdowns do
   f.compiled := nil;
  for f in FResources do
   f.compiled := nil;
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
  FMarkdowns := TFslList<TWorkspaceFile>.create;
  FAllFiles := TFslList<TWorkspaceFile>.create;
  FConfigurations := TFslList<TWorkspaceExecConfig>.create;
  FFolder := folder;
  reload;
end;

destructor TWorkspace.Destroy;
begin
  FAllFiles.Free;
  FMessages.Free;
  FDocuments.Free;
  FMaps.Free;
  FResources.Free;
  FScripts.Free;
  FTemplates.Free;
  FMarkdowns.Free;
  FConfigurations.Free;
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
    if obj = item.compiled then
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
  o := findFile(fn, markdowns);
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
  o := findObject(Obj, markdowns);
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
  if hasFile(fn, markdowns) then
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
  iniVC := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.control'])); // persisted in version control
  iniT := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.status'])); // local only
  try
    FName := iniVC.ReadString('Workspace', 'Name', '');
    if FName = '' then
    begin
      FName := 'New '+UI_NAME;
      iniVC.WriteString('Workspace', 'Name', FName);
      iniVC.WriteDateTime('Workspace', 'Created', now);
    end;
    st.clear;
    iniVC.ReadSection('Files', st);
    for s in st do
    begin
      fmt := iniVC.ReadString('Files', s, '');
      row := iniT.ReadInteger(s, 'Row', 0);
      bpl := iniT.ReadString(s, 'BreakPoints', '');
      if fmt = 'v2' then
        FMessages.Add(TWorkspaceFile.Create(Self, s, fmtV2, row, bpl))
      else if fmt = 'cda' then
        FDocuments.Add(TWorkspaceFile.Create(Self, s, fmtCDA, row, bpl))
      else if fmt = 'fhir' then
        FResources.Add(TWorkspaceFile.Create(Self, s, fmtResource, row, bpl))
      else if fmt = 'js' then
        FScripts.Add(TWorkspaceFile.Create(Self, s, fmtJS, row, bpl))
      else if fmt = 'map' then
        FMaps.Add(TWorkspaceFile.Create(Self, s, fmtMap, row, bpl))
      else if fmt = 'liquid' then
        FTemplates.Add(TWorkspaceFile.Create(Self, s, fmtTemplate, row, bpl))
      else if fmt = 'markdown' then
        FMarkdowns.Add(TWorkspaceFile.Create(Self, s, fmtMarkdown, row, bpl));
    end;
    FAllFiles.AddAll(FMessages);
    FAllFiles.AddAll(FDocuments);
    FAllFiles.AddAll(FResources);
    FAllFiles.AddAll(FScripts);
    FAllFiles.AddAll(FMaps);
    FAllFiles.AddAll(FTemplates);
    FAllFiles.AddAll(FMarkdowns);
    st.clear;
    iniVC.ReadSection('Configurations', st);
    for s in st do
      FConfigurations.Add(TWorkspaceExecConfig.Create(iniVC.ReadString('Configurations', s, '')));
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
  ec : TWorkspaceExecConfig;
  i : integer;
begin
  iniVC := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.control']));
  iniT := TIniFile.Create(path([folder, 'fhir-transfomer-workspace.status']));
  try
    iniVC.WriteString('Workspace', 'Name', FName);
    iniVC.WriteDateTime('Workspace', 'Updated', now);
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
    iniVC.EraseSection('Markdowns');
    for f in FMarkdowns do
    begin
      iniVC.WriteString('Files', f.filename, 'markdown');
      iniT.WriteInteger(f.filename, 'Row', f.row);
      iniT.WriteString(f.filename, 'BreakPoints', f.breakpointSummary);
    end;
    iniVC.EraseSection('Configurations');
    i := 0;
    for ec in FConfigurations do
    begin
      inc(i);
      iniVC.WriteString('Configurations', 'ec'+inttostr(i), ec.read);
    end;
  finally
    iniVC.Free;
    iniT.Free;
  end;
end;

{ TWorkspaceFile }

constructor TWorkspaceFile.Create(workspace : TWorkspace; filename: String; format : TTransformerFormat);
begin
  Create;
  FWorkspace := workspace;
  FFilename := filename;
  FFormat := format;
  FRow := 0;
  FBreakPoints := TFslList<TBreakPointInfo>.create;
end;

function TWorkspaceFile.actualName: String;
begin
  result := makeAbsolutePath(filename, FWorkspace.folder)
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

constructor TWorkspaceFile.Create(workspace : TWorkspace; filename : string; format : TTransformerFormat; row : integer; bpl : String);
var
  l, r : String;
  s : String;
begin
  Create;
  FWorkspace := workspace;
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
  FCompiled.Free;
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

procedure TWorkspaceFile.SetCompiled(const Value: TFslObject);
begin
  FCompiled.Free;
  FCompiled := Value;
end;

function TWorkspaceFile.source: String;
begin
  result := FileToString(actualName, TEncoding.UTF8);
end;

function TWorkspaceFile.stream: TStream;
begin
  result := TFileStream.Create(actualName, fmOpenRead + fmShareDenyWrite);
end;

{ TBreakPointInfo }

constructor TBreakPointInfo.create(line: integer);
begin
  Inherited Create;
  FLine := line;
end;

{ TWorkspaceExecConfig }

constructor TWorkspaceExecConfig.Create;
begin
  inherited create;
end;

constructor TWorkspaceExecConfig.Create(src: String);
begin
  inherited create;
  StringSplit(src, ';', FScript, FFocus);
end;

function TWorkspaceExecConfig.link: TWorkspaceExecConfig;
begin
  result := TWorkspaceExecConfig(inherited link);
end;

function TWorkspaceExecConfig.read: String;
begin
  result := FScript+';'+FFocus;
end;

function TWorkspaceExecConfig.summary: String;
begin
  result := 'Script '+script+' applied to '+focus;
end;

end.
