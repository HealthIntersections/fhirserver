unit fsl_npm;

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
  SysUtils, Classes, Types, {$IFDEF DELPHI}IOUtils, {$ENDIF} zlib,
  fsl_base, fsl_utilities, fsl_stream, fsl_json, fsl_fpc, fsl_threads;

Type
  TFHIRPackageKind = (fpkNull, fpkCore, fpkIG, fpkIGTemplate, fpkTool, fpkToolGen, fpkGroup, fpkExamples);
  TFHIRPackageKindSet = set of TFHIRPackageKind;

 TNpmPackageIndexBuilder = class (TFslObject)
  private
    index : TJsonObject;
    files : TJsonArray;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure seeFile(name : String; bytes : TBytes);
    function build : String;
  end;

  TNpmPackageObject = class (TFslObject)
  public
  end;

  TNpmPackageResource = class (TNpmPackageObject)
  private
    FName : String;
    FType : String;
    FId : String;
    FURL : String;
    FVersion : String;
    FSize: Integer;
    FKind: String;
    FResourceType: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    function Link : TNpmPackageResource; overload;

    property Name : String read FName write FName;
    property ResourceType : String read FResourceType write FResourceType;
    property Id : String read FId write FId;
    property TypeV : String read FType write FType;
    property Kind : String read FKind write FKind;
    property URL : String read FURL write FURL;
    property Version : String read FVersion write FVersion;
    property size : Integer read FSize write FSize;

    function matches(text : String) : boolean;
  end;

  TNpmPackageFolder = class (TNpmPackageObject)
  private
    FName : String;
    FFolder : String;
    FContent : TFslMap<TFslBuffer>;
    FResources : TFslList<TNpmPackageResource>;
    procedure readIndex(index : TJsonObject);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(); overload; override;
    constructor Create(name : String); overload; virtual;
    destructor Destroy; override;

    property name : String read FName;
    property content : TFslMap<TFslBuffer> read FContent;
    property resources : TFslList<TNpmPackageResource> read FResources;
    function listFiles : TArray<String>;
    function fetchFile(name : String) : TBytes;
    function hasFile(name : String) : boolean;
  end;

  TNpmPackage = class (TNpmPackageObject)
  private
    FPath : String;
    FNpm : TJsonObject;
    FFolders : TFslMap<TNpmPackageFolder>;
    FInstalled: TDateTime;
    FSize: integer;
    procedure loadFiles(path : String; exemptions : TArray<String>);
    procedure loadSubFolders(path, dir : String);
    procedure readStream(tgz : TStream; desc : String; progress : TWorkProgressEvent);
    procedure loadFile(n : String; data : TBytes);
    procedure checkIndexed(desc : String);
    function GetCanonical: String;
    function GetDate: String;
    function GetDependencies: TArray<String>;
    function GetDependencySummary: String;
    function GetDescription: String;
    function GetFhirVersion: String;
    function GetFhirVersionList: String;
    function GetHomepage: String;
    function GetKind: TFHIRPackageKind;
    function GetLicense: String;
    function GetName: String;
    function GetSummary: String;
    function GetTitle: String;
    function GetToolsVersion: String;
    function GetUrl: String;
    function GetVersion: String;
    function GetWebLocation: String;
    function findFolder(name : String) : TNpmPackageFolder;
    function GetAuthor: String;
    function GetNotForPublication: Boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TNpmPackage; overload;

    class function fromFolder(path : String) : TNpmPackage; overload;
    class function fromFolderQuick(path : String) : TNpmPackage; overload;
    class function fromPackage(tgz : TStream) : TNpmPackage; overload;
    class function fromPackage(tgz : TStream; desc : String) : TNpmPackage; overload;
    class function fromPackage(tgz : TStream; desc : String; progress : TWorkProgressEvent) : TNpmPackage; overload;
    class function fromPackage(tgz : TBytes; desc : String; progress : TWorkProgressEvent) : TNpmPackage; overload;
    class function fromSource(source: String): TNpmPackage; static;
//    procedure save(stream : TStream);

    function list(folder : String) : TArray<String>;
    function listResources(types : TArray<String>) : TArray<String>;
    function load(name : String) : TStream; overload;
    function load(folder, name : String) : TStream; overload;
    function loadBytes(name : String) : TBytes; overload;
    function loadBytes(folder, name : String) : TBytes; overload;
    function loadResource(resType, id : String) : TStream;
    function loadExampleResource(resType, id : String) : TStream;
    function hasFile(name : String) : boolean; overload;
    function hasFile(folder, name : String) : boolean; overload;
    property info : TJsonObject read FNpm;

    property folders : TFslMap<TNpmPackageFolder> read FFolders;
    property loadPath : String read FPath;

    property name : String read GetName;
    property date : String read GetDate;
    property canonical : String read GetCanonical;
    property notForPublication : Boolean read GetNotForPublication;
    property version : String read GetVersion;
    property fhirVersion : String read GetFhirVersion;
    property fhirVersionList : String read GetFhirVersionList;
    property summary : String read GetSummary;
    property description : String read GetDescription;
    property kind : TFHIRPackageKind read GetKind;
    property dependencies : TArray<String> read GetDependencies;
    property dependencySummary : String read GetDependencySummary;
    property homepage : String read GetHomepage;
    property url : String read GetUrl;
    property title : String read GetTitle;
    property toolsVersion : String read GetToolsVersion;
    property license : String read GetLicense;
    property webLocation : String read GetWebLocation;
    property author : String read GetAuthor;

    property installed : TDateTime read FInstalled write FInstalled;
    property size : integer read FSize write FSize;
    function isCore : boolean;

    procedure report(b : TStringBuilder);
    function presentation : String;
  end;

const
  All_Package_Kinds = [fpkNull..fpkToolGen];
  CODES_TFHIRPackageKind : Array [TFHIRPackageKind] of String = ('', 'Core', 'IG', 'IG-Template', 'Tool', 'GenPack', 'Group', 'Examples');
  NAMES_TFHIRPackageKind : Array [TFHIRPackageKind] of String = ('', 'Core Specification', 'Implementation Guides', 'IG Templates', 'Tools', 'Generation Package', 'Group', 'Examples');

function isValidPackageId(id : String) : boolean;
function isMoreRecentVersion(test, base : String) : boolean;

implementation

function isValidPackagePart(part : String) : boolean;
var
  ch : char;
begin
  if (part = '') or not CharInSet(part[1], ['a'..'z', 'A'..'Z']) then
    result := false
  else
  begin
    result := true;
    for ch in part do
      if not CharInSet(ch, ['a'..'z', 'A'..'Z', '0'..'9', '-']) then
        result := false;
  end;
end;

function isValidPackageId(id : String) : boolean;
var
  parts : TArray<String>;
  p : String;
begin
  if id = '' then
    exit(false);
  if (not id.Contains('.')) then
    exit(false);
  parts := id.Split(['.']);
  for p in parts do
    if not isValidPackagePart(p) then
      exit(false);
  result := true;
end;

{ TNpmPackageIndexBuilder }

constructor TNpmPackageIndexBuilder.Create;
begin
  Inherited Create;
  index := TJsonObject.Create;
  files := index.forceArr['files'];
end;

destructor TNpmPackageIndexBuilder.Destroy;
begin
  index := nil;
  files := nil;
  inherited Destroy;
end;

procedure TNpmPackageIndexBuilder.seeFile(name: String; bytes: TBytes);
var
  json, fi : TJsonObject;
begin
  if (name.endsWith('.json') and not name.EndsWith('package.json') and not name.EndsWith('.index.json') and not name.EndsWith('ig-r4.json')) then
  begin
    try
      json := TJSONParser.Parse(bytes);
      try
        if (json.has('resourceType')) then
        begin
          fi := files.addObject;
          fi.str['filename'] := name;
          fi.str['resourceType'] := json.str['resourceType'];
          if (json.has('id') and (json.node['id'].kind = jnkString)) then
            fi.str['id'] := json.str['id'];
          if (json.has('url') and (json.node['url'].kind = jnkString)) then
            fi.str['url'] := json.str['url'];
          if (json.has('version') and (json.node['version'].kind = jnkString)) then
            fi.str['version'] := json.str['version'];
          if (json.has('kind') and (json.node['kind'].kind = jnkString)) then
            fi.str['kind'] := json.str['kind'];
          if (json.has('type') and (json.node['type'].kind = jnkString)) then
            fi.str['type'] := json.str['type'];
        end;
      finally
        json.free;
      end;
    except
    end;
  end;
end;

function TNpmPackageIndexBuilder.build: String;
begin
  result := TJsonWriterDirect.writeObjectStr(index, true);
  index.Free;
  index := nil;
  files := nil;
end;

function TNpmPackageIndexBuilder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, index.sizeInBytes);
  inc(result, files.sizeInBytes);
end;

{ TNpmPackageFolder }

constructor TNpmPackageFolder.Create;
begin
  inherited create;
  FContent := TFslMap<TFslBuffer>.create('Npm Packages');
end;

constructor TNpmPackageFolder.Create(name: String);
begin
  Create;
  FName := name;
end;

destructor TNpmPackageFolder.Destroy;
begin
  FContent.Free;
  FResources.Free;
  inherited;
end;

function TNpmPackageFolder.fetchFile(name: String): TBytes;
var
  f : String;
  b : TFslBuffer;
begin
  result := [];
  if (FFolder <> '') then
  begin
    f := path([FFolder, name]);
    if (FileExists(f)) then
      result := FileToBytes(f)
    else
  end
  else
  begin
    if FContent.TryGetValue(name, b) then
      result := b.asBytes;
  end;
end;

function TNpmPackageFolder.hasFile(name: String): boolean;
begin
  if (FFolder <> '') then
  begin
    result := FileExists(path([FFolder, name]));
  end
  else
  begin
    result := FContent.containsKey(name);
  end;
end;

function TNpmPackageFolder.listFiles: TArray<String>;
var
  sl : TStringList;
  f, fl : String;
begin
  sl := TStringList.Create;
  try
    if (FFolder <> '') then
    begin
      for f in TDirectory.GetFiles(FFolder) do
      begin
        fl := ExtractFileName(f);
        if not folderExists(f) and not StringArrayExists(['package.json', '.index.json'], fl) then
          sl.add(fl);
      end;
    end
    else
    begin
      for f in FContent.keys do
      begin
        if not StringArrayExists(['package.json', '.index.json'], f) then
          sl.add(f);
      end;
    end;
    sl.sort;
    result := sl.ToStringArray;
  finally
    sl.free;
  end;
end;

procedure TNpmPackageFolder.readIndex(index: TJsonObject);
var
  e : TJsonNode;
  f : TJsonObject;
  r : TNpmPackageResource;
begin
  if FResources <> nil then
    FResources.Clear
  else
    FResources := TFslList<TNpmPackageResource>.create;
  for e in index.arr['files'] do
  begin
    f := e as TJsonObject;
    r := TNpmPackageResource.Create;
    FResources.Add(r);
    r.Name := f.str['filename'];
    r.ResourceType := f.str['resourceType'];
    r.Id := f.str['id'];
    r.TypeV := f.str['type'];
    r.Kind := f.str['kind'];
    r.URL := f.str['url'];
    r.Version := f.str['version'];
    if FFolder <> '' then
      r.size := FileSize(FilePath([FFolder, r.name]));
  end;
end;

function TNpmPackageFolder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FFolder.length * sizeof(char)) + 12);
  inc(result, FContent.sizeInBytes);
  inc(result, FResources.sizeInBytes);
end;

{ TNpmPackage }

constructor TNpmPackage.Create;
begin
  inherited;
  FFolders := TFslMap<TNpmPackageFolder>.create('Npm Folders');
  FFolders.defaultValue := nil;
end;

destructor TNpmPackage.Destroy;
begin
  FNpm.Free;
  FFolders.Free;
  inherited;
end;

procedure TNpmPackage.checkIndexed(desc: String);
var
  folder : TNpmPackageFolder;
  indexer : TNpmPackageIndexBuilder;
  n : string;
  json : TJsonObject;
begin
  for folder in FFolders.values do
  begin
    if (folder.FResources = nil) then
    begin
      indexer := TNpmPackageIndexBuilder.create;
      try
        for n in folder.listFiles() do 
        begin
          indexer.seeFile(n, folder.fetchFile(n));
        end;
        try 
          json := TJsonParser.parse(indexer.build());
          try
            folder.readIndex(json);
          finally
            json.free;
          end;
        except
          on e : Exception do
            raise EFslException.create('Error parsing '+desc+'#'+'package/'+folder.name+'/.index.json: '+e.message);
        end;
      finally
        indexer.free;  
      end;
    end;
  end;
end;

function TNpmPackage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPath.length * sizeof(char)) + 12);
  inc(result, FNpm.sizeInBytes);
  inc(result, FFolders.sizeInBytes);
end;

class function TNpmPackage.fromFolder(path: String): TNpmPackage;
var
  this : TNpmPackage;
begin
  this := TNpmPackage.create;
  try
    this.loadFiles(path, []);
    this.checkIndexed(path);
    result := this.link;
  finally
    this.free;
  end;
end;

class function TNpmPackage.fromFolderQuick(path: String): TNpmPackage;
var
  this : TNpmPackage;
begin
  this := TNpmPackage.create;
  try
    this.FNpm := TJsonParser.parseFile(filePath([path, 'package', 'package.json']));
    result := this.link;
  finally
    this.free;
  end;
end;

class function TNpmPackage.fromPackage(tgz: TStream): TNpmPackage;
begin
  result := fromPackage(tgz, '', nil);
end;

class function TNpmPackage.fromPackage(tgz: TStream; desc: String): TNpmPackage;
begin
  result := fromPackage(tgz, desc, nil);
end;

class function TNpmPackage.fromPackage(tgz: TStream; desc: String; progress: TWorkProgressEvent): TNpmPackage;
var
  this : TNpmPackage;
begin
  this := TNpmPackage.create;
  try
    this.readStream(tgz, desc, progress);
    result := this.link;
  finally
    this.free;
  end;
end;

function TNpmPackage.GetAuthor: String;
begin
  result := info.str['author'];
end;

function TNpmPackage.GetCanonical: String;
begin
  result := info.str['canonical'];
end;

function TNpmPackage.GetDate: String;
begin
  result := info.str['date'];
end;

function TNpmPackage.GetDependencies: TArray<String>;
var
  sl : TStringList;
  n : String;
begin
  sl := TStringList.create;
  try
    if (info.has('dependencies')) then
    begin
      for n in info.obj['dependencies'].properties.keys do
        sl.add(n+'#'+info.obj['dependencies'].str[n]);
    end;
    result := sl.toStringArray;
  finally
    sl.free;
  end;
end;

function TNpmPackage.GetDependencySummary: String;
var
  sl : TStringList;
  n : String;
begin
  sl := TStringList.create;
  try
    if (info.has('dependencies')) then
    begin
      for n in info.obj['dependencies'].properties.keys do
        sl.add(n+'#'+info.obj['dependencies'].str[n]);
    end;
    result := sl.commaText;
  finally
    sl.free;
  end;
end;

function TNpmPackage.GetDescription: String;
begin
  result := info.str['description'];
end;

function TNpmPackage.GetFhirVersion: String;
var
  dep : TJsonObject;
  n : string;
begin
  if ('hl7.fhir.core' = info['name']) then
    result := info['version']
  else if (info['name'].startsWith('hl7.fhir.r2.') or info['name'].startsWith('hl7.fhir.r2b.') or info['name'].startsWith('hl7.fhir.r3.')or info['name'].startsWith('hl7.fhir.r4.') or info['name'].startsWith('hl7.fhir.r5.')) then
    result := info['version']
  else
  begin
    dep := info.obj['dependencies'];
    if (dep <> nil) then
    begin
      for n in dep.properties.keys do 
      begin
        if (StringArrayExists(['hl7.fhir.r2.core', 'hl7.fhir.r2b.core', 'hl7.fhir.r3.core', 'hl7.fhir.r4.core'], n)) then
          exit(dep[n]);
        if (n = 'hl7.fhir.core') then // while all packages are updated
          exit(dep[n]);
      end;
    end;
    if (info.has('fhirVersions')) then
        exit(info.arr['fhirVersions'].Value[0]);
    if (dep <> nil) then
    begin
      // legacy simplifier support:
      if (dep.has('simplifier.core.r4')) then
        exit('4.0');
      if (dep.has('simplifier.core.r3')) then
        exit('3.0');
      if (dep.has('simplifier.core.r2')) then
        exit('2.0');
    end;
    result := 'n/a';
  end;
end;

function TNpmPackage.GetFhirVersionList: String;
var
  sl : TStringList;
  i : integer;
begin
  sl := TStringList.create;
  try
    if (info.has('fhirVersions')) then
    begin
      for i := 0 to info.arr['fhirVersions'].count - 1 do
        sl.add(info.arr['fhirVersions'].value[i]);
    end;
    result := sl.commaText;
  finally
    sl.free;
  end;
end;

function TNpmPackage.GetHomepage: String;
begin
  result := info.str['homepage'];
end;

function TNpmPackage.GetKind: TFHIRPackageKind;
var
  s : String;
begin
  s := info['type'];
  if (s = 'fhir.core') then
    result := fpkCore
  else if (s = 'fhir.ig') then
    result := fpkIG
  else if (s = 'fhir.tool') then
    result := fpkTool
  else if (s = 'fhir.template') then
    result := fpkIGTemplate
  else if (s = 'fhir.core.gen') then
    result := fpkToolGen
  else if (s = 'fhir.tool') then
    result := fpkTool
  else if (s = 'fhir.examples') then
    result := fpkExamples
  else if (s = 'fhir.group') or (name = 'hl7.fhir.core') then
    result := fpkGroup
  else if StringArrayExistsSensitive(['hl7.fhir.r2.core', 'hl7.fhir.r3.core', 'hl7.fhir.r4.core'], name) then
    result := fpkCore
  else
    result := fpkNull;
end;

function TNpmPackage.GetLicense: String;
begin
  result := info.str['license'];
end;

function TNpmPackage.GetName: String;
begin
  result := info.str['name'];
end;

function TNpmPackage.GetNotForPublication: Boolean;
begin
  result := info.bool['notForPublication'];
end;

function TNpmPackage.GetSummary: String;
begin
  if (FPath <> '') then
    result := FPath
  else
    result := '(memory)';
end;

function TNpmPackage.GetTitle: String;
begin
  result := info.str['title'];
end;

function TNpmPackage.GetToolsVersion: String;
begin
  result := info.str['tools-version'];
end;

function TNpmPackage.GetUrl: String;
begin
  result := info.str['url'];
end;

function TNpmPackage.GetVersion: String;
begin
  result := info.str['version'];
end;

function TNpmPackage.GetWebLocation: String;
begin
  if (info.has('url')) then
    result := info.str['url']
  else
    result := info.str['canonical'];
end;

function TNpmPackage.hasFile(folder, name: String): boolean;
var
  f : TNpmPackageFolder;
begin
  f := findFolder(folder);
  result := (f <> nil) and f.hasFile(name);
end;

function TNpmPackage.hasFile(name: String): boolean;
var
  f : TNpmPackageFolder;
begin
  f := findFolder('package');
  result := (f <> nil) and f.hasFile(name);
end;

function TNpmPackage.isCore: boolean;
begin
  result := StringArrayExistsInsensitive(['hl7.fhir.core', 'hl7.fhir.r2.core','hl7.fhir.r2b.core','hl7.fhir.r3.core',
    'hl7.fhir.r4.core','hl7.fhir.r5.core','hl7.fhir.r6.core'], name);
end;

function TNpmPackage.Link: TNpmPackage;
begin
  result := TNpmPackage(inherited Link);
end;

function TNpmPackage.findFolder(name: String): TNpmPackageFolder;
var
  s : string;
begin
  if (folders.containsKey(name)) then
    result := folders[name]
  else 
  begin
    s := path(['package', name]);
    if (folders.containsKey(s)) then
      result := folders[s]
    else
      result := nil;
  end;
end;

function TNpmPackage.list(folder: String): TArray<String>;
var
  f : TNpmPackageFolder;
begin
  f := findFolder(folder);
  if (f <> nil) then
    result := f.listFiles()
  else 
    result := [];
end;

function TNpmPackage.listResources(types: TArray<String>): TArray<String>;
var
  sl : TStringList;
  t : String;
  f : TNpmPackageFolder;
  r : TNpmPackageResource;
begin
  sl := TStringList.create;
  try
    f := findFolder('package');
    for t in types do
      for r in f.FResources do
        if r.ResourceType = t then
          sl.add(r.Name);
    sl.sort;
    result := sl.toStringArray;
  finally
    sl.free;
  end;
end;

function TNpmPackage.load(folder, name: String): TStream;
var
  f : TNpmPackageFolder;
begin
  f := findFolder(folder);
  result := nil;
  if (f <> nil) and f.hasFile(name) then
    result := TBytesStream.create(f.fetchFile(name));
end;

function TNpmPackage.load(name: String): TStream;
var
  f : TNpmPackageFolder;
begin
  f := findFolder('package');
  result := nil;
  if (f <> nil) and f.hasFile(name) then
    result := TBytesStream.create(f.fetchFile(name));
end;

function TNpmPackage.loadBytes(folder, name: String): TBytes;
var
  f : TNpmPackageFolder;
begin
  f := findFolder(folder);
  result := nil;
  if (f <> nil) and f.hasFile(name) then
    result := f.fetchFile(name);
end;

function TNpmPackage.loadBytes(name: String): TBytes;
var
  f : TNpmPackageFolder;
begin
  f := findFolder('package');
  result := nil;
  if (f <> nil) and f.hasFile(name) then
    result := f.fetchFile(name);
end;

function TNpmPackage.loadExampleResource(resType, id: String): TStream;
var
  f : TNpmPackageFolder;
  r : TNpmPackageResource;
begin
  f := findFolder('example');
  if (f <> nil) then
  begin
    for r in f.FResources do
      if (r.resourceType = resType) and (r.id = id) then
        exit(load('example', r.name));
  end;
  result := nil;
end;

procedure TNpmPackage.loadFile(n: String; data: TBytes);
var
  dir : String;
  folder : TNpmPackageFolder;
begin
  if (n.contains('/')) then
     dir := n.substring(0, n.lastIndexOf('/'))
  else
    dir := '$root';
  if (dir.startsWith('package/')) then
    dir := dir.substring(8);
  n := n.substring(n.lastIndexOf('/')+1);
  folder := folders[dir];
  if (folder = nil) then
  begin
    folder := TNpmPackageFolder.create(dir);
    folders.add(dir, folder);
  end;
  folder.FContent.add(n, TFslBuffer.create(data));
end;

procedure TNpmPackage.loadFiles(path: String; exemptions: TArray<String>);
var
  f, d, ij : String;
  folder : TNpmPackageFolder;
  json : TJsonObject;
begin
  FNpm := TJsonParser.parseFile(filePath([path, 'package', 'package.json']));
  for f in TDirectory.getDirectories(path) do
  begin
    d := extractFileName(f);
    if (d <> 'package') then
      d := FilePath(['package', d]);
    folder := TNpmPackageFolder.create(d);
    folders.add(d, folder);
    folder.FFolder := f;
    ij := FilePath([f, '.index.json']);
    if FileExists(ij) then
    begin
      try
        json := TJsonParser.parseFile(ij);
        try
          folder.readIndex(json);
        finally
          json.free;
        end;
      except 
        on e : Exception do
          raise EFslException.create('Error parsing '+ij+': '+e.Message);
      end;
    end;
    loadSubFolders(path, f);
  end;
end;

function TNpmPackage.loadResource(resType, id: String): TStream;
var
  f : TNpmPackageFolder;
  r : TNpmPackageResource;
begin
  f := findFolder('package');
  if (f <> nil) then
  begin
    for r in f.FResources do
      if (r.resourceType = resType) and (r.id = id) then
        exit(load('package', r.name));
  end;
  result := nil;
end;

procedure TNpmPackage.loadSubFolders(path, dir: String);
var
  f, d, ij : String;
  folder : TNpmPackageFolder;
  json : TJsonObject;
begin
  for f in TDirectory.getDirectories(dir) do
  begin
    d := f.substring(length(path)+1);
    if (not d.startsWith('package')) then
      d := FilePath(['package', d]);
    folder := TNpmPackageFolder.create(d);
    folders.add(d, folder);
    folder.FFolder := f;
    ij := FilePath([f, '.index.json']);
    if FileExists(ij) then
    begin
      try
        json := TJsonParser.parseFile(ij);
        try
          folder.readIndex(json);
        finally
          json.free;
        end;
      except
        on e : Exception do
          raise EFslException.create('Error parsing '+ij+': '+e.Message);
      end;
    end;
    loadSubFolders(path, f);
  end;
end;

function TNpmPackage.presentation: String;
var
  ts : TStringList;
  n : TNpmPackageResource;
  i : integer;
begin
  result :=
    'id: '+name+'#'+Version+#13#10+
    'canonical: '+Canonical+#13#10+
    'size: '+DescribeBytes(FSize)+#13#10+
    'installed: '+FormatDateTime('c', FInstalled)+#13#10+
    'dependencies: '+dependencySummary+#13#10+
    'fhirVersion: '+fhirVersionList+#13#10+
    'source: '+summary+#13#10+
    ''+#13#10;
  ts := TStringList.Create;
  try
    for n in folders['package'].resources do
    begin
      i := ts.IndexOf(n.FType);
      if (i = -1) then
        i := ts.Add(n.FType);
      ts.Objects[i] := TObject(integer(ts.Objects[i])+1);
    end;
    ts.Sort;
    for i := 0 to ts.Count - 1 do
      result := result + ts[i]+': '+inttostr(Integer(ts.Objects[i]))+#13#10;
  finally
    ts.Free;
  end;
end;

procedure TNpmPackage.readStream(tgz: TStream; desc: String; progress: TWorkProgressEvent);
var
  z : TZDecompressionStream;
  tar : TTarArchive;
  entry : TTarDirRec;
  n : String;
  b : TBytes;
  bi : TBytesStream;
begin
  z := TZDecompressionStream.Create(tgz, 15+16);
  try
    tar := TTarArchive.Create(z);
    try
      tar.Reset;
      while tar.FindNext(entry) do
      begin
        n := String(entry.Name);
        bi := TBytesStream.Create;
        try
          tar.ReadFile(bi);
          b := copy(bi.Bytes, 0, bi.size);
        finally
          bi.free;
        end;
        loadFile(n, b);
        if assigned(progress) then
          progress(self, -1, false, 'Loading '+n);
      end;
    finally
      tar.free;
    end;
  finally
    z.free;
  end;
  try 
    FNpm := TJsonParser.parse(folders['package'].fetchFile('package.json'));
  except 
    on e : Exception do
      raise EFslException.create('Error parsing '+desc+'#'+'package/package.json: '+e.Message);
  end;    
  checkIndexed(desc);
end;

procedure TNpmPackage.report(b : TStringBuilder);
begin
  b.AppendLine('  '+Version+' on '+FhirVersion+' from '+url+' in '+FPath);
  b.AppendLine('    dependencies: '+dependencySummary);
end;

class function TNpmPackage.fromSource(source: String): TNpmPackage;
begin
  if FolderExists(source) then
    result := fromFolder(source)
  else
    result := fromPackage(TFileStream.create(source, fmOpenRead + fmShareDenyWrite));
end;

class function TNpmPackage.fromPackage(tgz: TBytes; desc: String; progress: TWorkProgressEvent): TNpmPackage;
var
  s : TBytesStream;
begin
  s := TBytesStream.Create(tgz);
  try
    result := fromPackage(s, desc, progress);
  finally
    s.Free;
  end;
end;

{ TNpmPackageResource }

function TNpmPackageResource.Link: TNpmPackageResource;
begin
  result := TNpmPackageResource(inherited Link);
end;

function TNpmPackageResource.matches(text: String): boolean;
begin
  text := text.ToLower;
  result := FName.ToLower.Contains(text) or
    FType.ToLower.Contains(text) or
    FId.ToLower.Contains(text) or
    FURL.ToLower.Contains(text) or
    FVersion.ToLower.Contains(text) or
    FKind.ToLower.Contains(text) or
    FResourceType.ToLower.Contains(text);
end;

function isMoreRecentVersion(test, base : String) : boolean;
begin
  if isValidSemVer(test) and isValidSemVer(base) then
    result := TSemVer.isMoreRecent(test, base)
  else
    result := test > base;
end;

function TNpmPackageResource.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FType.length * sizeof(char)) + 12);
  inc(result, (FId.length * sizeof(char)) + 12);
  inc(result, (FURL.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FKind.length * sizeof(char)) + 12);
  inc(result, (FResourceType.length * sizeof(char)) + 12);
end;

end.
