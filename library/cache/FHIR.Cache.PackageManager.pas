unit FHIR.Cache.PackageManager;

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

interface

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, IniFiles, zlib, Generics.Collections, System.Types,
  FHIR.Support.Base, FHIR.Base.Lang, FHIR.Support.Utilities, FHIR.Support.Json,
  FHIR.Support.Stream, FHIR.Web.Fetcher,
  FHIR.Base.Utilities;

type
  TFHIRPackageKind = (fpkNull, fpkCore, fpkIG, fpkIGTemplate, fpkTool, fpkToolGen, fpkGroup, fpkExamples);
  TFHIRPackageKindSet = set of TFHIRPackageKind;

type
  TNpmFileInfo = class (TFslObject)
  private
    FName : String;
    FType : String;
    FId : String;
    FURL : String;
    FVersion : String;
    FSize: Integer;
    FKind: String;
    FResourceType: String;
  public
    function Link : TNpmFileInfo;

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

  TNpmPackageInfo = class (TFslObject)
  private
    FFiles : TFslList<TNpmFileInfo>;
    FPackage: TJsonObject;
    FIndex: TJsonObject;
    class function loadArchive(content: TBytes): TDictionary<String, TBytes>;
    function GetAuthor: string;
    function GetCanonical: string;
    function GetDependencies: string;
    function GetDescription: string;
    function GetFhirVersions: string;
    function GetHomePage: string;
    function GetLicense: string;
    function GetPackageId: string;
    function GetTitle: string;
    function GetType: string;
    function GetUrl: string;
    function GetVersion: string;
    function GetFHIRVersion: String;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Link : TNpmPackageInfo;

    property Files : TFslList<TNpmFileInfo> read FFiles;
    property package : TJsonObject read FPackage write FPackage;

    property packageId : string read GetPackageId;
    property fhirVersions : string read GetFhirVersions;
    property fhirVersion : String read GetFHIRVersion;
    property canonical : string read GetCanonical;
    property homePage : string read GetHomePage;
    property title : string read GetTitle;
    property version : string read GetVersion;
    property typeV : string read GetType;
    property license : string read GetLicense;
    property url : string read GetUrl;
    property author : string read GetAuthor;
    property description : string read GetDescription;
    property dependencies : string read GetDependencies;

    class function fromFolder(folder : String; size : boolean) : TNpmPackageInfo;
    class function fromPackage(bytes : TBytes) : TNpmPackageInfo;
    class function fromSource(source : String; size : boolean) : TNpmPackageInfo;
  end;

  TPackageDefinition = class (TFslObject)
  private
    FId : String;
    FVersion : String;
    FCanonical : String;
    FDate : TDateTime;
    FDescription : String;
    FFHIRVersion : String;
    FUrl: String;
  public
    function link : TPackageDefinition; overload;

    property Id : String read FId write FId;
    property Version : String read FVersion write FVersion;
    property Canonical : String read FCanonical write FCanonical;
    property Date : TDateTime read FDate write FDate;
    property Description : String read FDescription write FDescription;
    property FHIRVersion : String read FFHIRVersion write FFHIRVersion;
    property Url : String read FUrl write FUrl;

    class procedure addStandardPackages(list : TFslList<TPackageDefinition>);
    class procedure addPackagesFromBuild(list : TFslList<TPackageDefinition>);
    class procedure AddCustomPackages(list : TFslList<TPackageDefinition>);
  end;

  TFHIRPackageObject = class abstract (TFslObject)
  public
    function summary : string; virtual;
    function presentation : String; virtual;
    function childCount : integer; virtual;
  end;

  TFHIRPackageDependencyStatus = (stUnknown, stOK, stMoreRecentVersion, stNotResolved);

  TFHIRPackageDependencyInfo = class (TFHIRPackageObject)
  private
    Fversion: String;
    Fid: String;
    Furi: String;
    FStatus: TFHIRPackageDependencyStatus;
  public
    function Link : TFHIRPackageDependencyInfo; overload;
    function summary : String; override;

    property id : String read Fid write FId;
    property version : String read Fversion write FVersion;
    property uri : String read Furi write Furi;
    property status : TFHIRPackageDependencyStatus read FStatus write FStatus;
  end;

  TFHIRPackageVersionInfo = class (TFHIRPackageObject)
  private
    FFolder: String;
    FId: String;
    FVersion : String;
    FOriginalId : String;
    FOriginalVersion : String;
    FFhirVersion: string;
    FCanonical: String;
    FUrl : String;
    FDependencies : TFslList<TFHIRPackageDependencyInfo>;
    FInstalled: TDateTime;
    FSize: integer;
    FProfiles: TFslStringDictionary;
    FCanonicals : TFslStringDictionary;
    FKind: TFHIRPackageKind;
    FPackage : TNpmPackageInfo;
    function listDependencies : String;
    procedure report (b : TStringBuilder);
    function GetFhirVersion: string;
  public
    constructor Create(package : TNpmPackageInfo);
    destructor Destroy; override;
    function Link : TFHIRPackageVersionInfo; overload;
    function summary : String; override;
    function presentation : String; override;
    function childCount : integer; override;

    property id : String read FId write FId;
    property version : String read FVersion write FVersion;
    property originalId : String read FOriginalId write FOriginalId;
    property originalVersion : String read FOriginalVersion write FOriginalVersion;
    property fhirVersion : string read GetFhirVersion;
    property Canonical : String read FCanonical write FCanonical;
    property url : String read FUrl write FUrl;
    property size : integer read FSize write FSize;
    property installed : TDateTime read FInstalled write FInstalled;
    property dependencies : TFslList<TFHIRPackageDependencyInfo> read FDependencies;
    property profiles : TFslStringDictionary read FProfiles;
    property canonicals : TFslStringDictionary read FCanonicals;
    property folder : String read FFolder write FFolder;
    property kind : TFHIRPackageKind read FKind write FKind;

    function isCore : boolean;
    function depString : String;
  end;

  TFHIRPackageInfo = class (TFHIRPackageObject)
  private
    FId : String;
    FCanonical : String;
    FVersions : TFslList<TFHIRPackageVersionInfo>;
    FKind: TFHIRPackageKind;
    procedure report (b : TStringBuilder);
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TFHIRPackageInfo; overload;
    function summary : String; override;
    function childCount : integer; override;

    property id : String read FId write FId;
    property Canonical : String read FCanonical write FCanonical;
    property versions : TFslList<TFHIRPackageVersionInfo> read FVersions;
    property kind : TFHIRPackageKind read FKind write FKind;
  end;

  TCheckEvent = function(sender : TObject; msg : String):boolean of object;
  TPackageLoadingEvent = procedure (rType, id : String; stream : TStream) of object;
  TWorkProgressEvent = procedure (sender : TObject; pct : integer; done : boolean; desc : String) of object;

  TNpmPackageIndexBuilder = class (TFslObject)
  private
    index : TJsonObject;
    files : TJsonArray;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure seeFile(name : String; bytes : TBytes);
    function build : String;
  end;

  TPackageLoadingInformation = class (TFslObject)
  private
    FVersion: String;
    FLoaded: TStringList;
    FOnLoadEvent: TPackageLoadingEvent;
  public
    constructor Create(ver : string);
    destructor Destroy; override;

    property OnLoadEvent : TPackageLoadingEvent read FOnLoadEvent write FOnLoadEvent;

    procedure checkVersion(ver : String);
    function isLoaded(id, ver: String) : boolean;
    procedure load(id, ver: String);
  end;

  TFHIRPackageManager = class (TFslObject)
  private
    FUser : boolean;
    FFolder : String;
    FIni : TIniFile;
    FOnWork : TWorkProgressEvent;
    FOnCheck : TCheckEvent;
    FCache : TFslMap<TNpmPackageInfo>;
    FTaskDesc : String;
    function resolve(list : TFslList<TFHIRPackageInfo>; dep : TFHIRPackageDependencyInfo) : TFHIRPackageDependencyStatus;
    function loadArchive(content : TBytes) : TDictionary<String, TBytes>;
    procedure clearCache;
    function readNpmKind(kind, id: String): TFHIRPackageKind;
    function checkPackageSize(dir : String) : integer;
    procedure work(pct : integer; done : boolean; desc : String);
    procedure progress(sender : TObject; pct : integer);
    function check(desc : String) : boolean;
    function loadPackageFromCache(folder : String) : TNpmPackageInfo;
    procedure buildPackageIndex(folder : String);
    function latestPackageVersion(id: String): String;
    function isIgnored(s : String): boolean;
  public
    constructor Create(user : boolean);
    destructor Destroy; override;
    function Link : TFHIRPackageManager;

    property OnWork : TWorkProgressEvent read FOnWork write FOnWork;
    property OnCheck : TCheckEvent read FOnCheck write FOnCheck;

    property Folder : String read FFolder;
    property UserMode : boolean read FUser;
    function description : String;

    procedure ListPackages(list : TStrings); overload;
    procedure ListPackages(kinds : TFHIRPackageKindSet; list : TFslList<TFHIRPackageInfo>); overload;
    procedure ListPackageIds(list : TStrings);
    procedure ListPackageVersions(id : String; list : TStrings); overload;
    procedure ListPackageVersions(list : TFslList<TFHIRPackageVersionInfo>); overload;
    procedure ListPackagesForFhirVersion(kinds : TFHIRPackageKindSet; core : boolean; version : String; list : TStrings);

    function packageExists(id, ver : String) : boolean; overload;
    function autoInstallPackage(id, ver : String) : boolean; overload;

    procedure loadPackage(id, ver : String; resources : Array of String; loadInfo : TPackageLoadingInformation); overload;
    procedure loadPackage(idver : String; resources : Array of String; loadInfo : TPackageLoadingInformation); overload;
    procedure loadPackage(id, ver : String; resources : TFslStringSet; loadInfo : TPackageLoadingInformation); overload;

    procedure import(content : TBytes); overload;
    function install(url : String) : boolean;

    procedure remove(id : String); overload;
    procedure remove(id, ver: String); overload;

    function getId(url : String) : String;
    function getUrl(id : String) : String;

    function report : String;
  end;

const
  All_Package_Kinds = [fpkCore..fpkToolGen];
  CODES_TFHIRPackageKind : Array [TFHIRPackageKind] of String = ('', 'Core', 'IG', 'IG-Template', 'Tool', 'GenPack', 'Group', 'Examples');
  NAMES_TFHIRPackageKind : Array [TFHIRPackageKind] of String = ('', 'Core Specification', 'Implementation Guides', 'IG Templates', 'Tools', 'Generation Package', 'Group', 'Examples');
  NAMES_TFHIRPackageDependencyStatus : Array [TFHIRPackageDependencyStatus] of String = ('?', 'ok', 'ver?', 'bad');
  ANALYSIS_VERSION = 2;
  CACHE_VERSION = 3;



implementation

uses
  IOUtils;

function getVersFromDeps(json : TJsonObject) : String;
var
  deps : TJsonObject;
begin
  deps := json.obj['dependencies'];
  if (deps <> nil) then
  begin
    if (deps.has('hl7.fhir.r2.core')) then
      exit('1.0.2')
    else if (deps.has('hl7.fhir.r2b.core')) then
      exit('1.4.0')
    else if (deps.has('hl7.fhir.r3.core')) then
      exit('3.0.2')
    else if (deps.has('hl7.fhir.r4.core')) then
      exit('4.0.1')
    else if (deps.has('hl7.fhir.core')) then
      exit(deps.str['hl7.fhir.core'])
  end;
  if json.has('fhirVersions') then
    result := json.arr['fhirVersions'].Value[0]
  else
    result := '';
end;

function isCoreName(s : String) : boolean;
begin
  result := StringArrayExistsInsensitive(['hl7.fhir.core', 'hl7.fhir.r2.core','hl7.fhir.r2b.core','hl7.fhir.r3.core',
    'hl7.fhir.r4.core','hl7.fhir.r5.core','hl7.fhir.r6.core'], s);
end;

{ TFHIRPackageManager }

constructor TFHIRPackageManager.Create(user : boolean);
begin
  inherited Create;
  FCache := TFslMap<TNpmPackageInfo>.create;
  FUser := user;
  if user then
    FFolder := path([UserFolder, '.fhir', 'packages'])
  else
    FFolder := path([ProgData, '.fhir', 'packages']);
  ForceFolder(FFolder);
  FIni := TIniFile.create(Path([FFolder, 'packages.ini']));
  if FIni.ReadInteger('cache', 'version', 0) <> CACHE_VERSION then
  begin
    clearCache;
    FIni.WriteInteger('cache', 'version', CACHE_VERSION);
  end;
end;

destructor TFHIRPackageManager.Destroy;
begin
  FCache.Free;
  FIni.Free;
  Inherited;
end;

function TFHIRPackageManager.Link: TFHIRPackageManager;
begin
  result := TFHIRPackageManager(Inherited Link);
end;

procedure TFHIRPackageManager.clearCache;
var
  s : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
    if s.Contains('#') then
    begin
      FolderDelete(s);
      FIni.DeleteKey('packages', s);
    end;
  FCache.Clear;
end;

function TFHIRPackageManager.check(desc: String): boolean;
begin
  if (assigned(FOnCheck)) then
    result := FOnCheck(self, desc)
  else
    result := true;
end;

function TFHIRPackageManager.checkPackageSize(dir: String): integer;
var
  s : String;
begin
  result := 0;
  for s in TDirectory.GetDirectories(dir) do
    inc(result, checkPackageSize(s));
  for s in TDirectory.GetFiles(dir) do
    inc(result, FileSize(s));
end;

function TFHIRPackageManager.description: String;
begin
  if FUser then
    result := 'User Cache'
  else
    result := 'System Cache';
end;

function TFHIRPackageManager.getId(url: String): String;
var
  list : TStringList;
  s : String;
begin
  list := TStringList.Create;
  try
    FIni.ReadSection('urls', list);
    for s in list do
      if url = FIni.ReadString('urls', s, '') then
        exit(s);
  finally
    list.Free;
  end;
end;

function TFHIRPackageManager.getUrl(id: String): String;
begin
  result := FIni.ReadString('urls', id, '');
end;


procedure TFHIRPackageManager.import(content : TBytes);
var
  npm : TJsonObject;
  id, ver, dir, fn, fver, s : String;
  files : TDictionary<String, TBytes>;
  size,c : integer;
  indexer : TNpmPackageIndexBuilder;
begin
  files := loadArchive(content);
  try
    npm := TJSONParser.Parse(files['package\package.json']);
    try
      id := npm.str['name'];
      ver := npm.str['version'];
      fver := getVersFromDeps(npm);
      if npm.has('dependencies') then
        for s in npm.obj['dependencies'].properties.Keys do
          autoInstallPackage(s, npm.obj['dependencies'].str[s]);
    finally
      npm.Free;
    end;
    if packageExists(id, ver) then
      if not check('Replace existing copy of '+id+' version '+ver+'?') then
        exit;
    work(0, false, 'Installing');
    dir := path([FFolder, id+'#'+ver]);
    if FolderExists(dir) then
      if not FolderDelete(dir) then
        raise EIOException.create('Unable to delete existing package');
    ForceFolder(dir);
    size := 0;
    c := 0;
    indexer := TNpmPackageIndexBuilder.create;
    try
      for s in files.Keys do
      begin
        inc(c);
        work(trunc((c / files.Count) * 100), false, 'Installing');
        if length(files[s]) > 0 then
        begin
          fn := path([dir, s]);
          ForceFolder(PathFolder(fn));
          if FileExists(fn) then
          begin
            if not DeleteFile(fn) then
            begin
              Sleep(100);
              if not DeleteFile(fn) then
                raise EIOException.create('Unable to delete existing file '+fn);
            end;
          end;
          BytesToFile(files[s], fn);
          inc(size, length(files[s]));
          if (s.StartsWith('package\')) then
            indexer.seeFile(s.Substring(8), files[s]);
        end;
      end;
      work(100, false, 'Installing');
      StringToFile(indexer.build, path([FFolder, '.index.json']), TEncoding.UTF8);
      Fini.WriteInteger('package-sizes', id+'#'+ver, size);
      Fini.WriteString('packages', id+'#'+ver, FormatDateTime('yyyymmddhhnnss', now));
      work(100, true, 'Installing');
    finally
      indexer.free;
    end;
  finally
    files.Free;
  end;
end;

function TFHIRPackageManager.install(url: String) : boolean;
var
  fetch : TInternetFetcher;
  aborted : boolean;
  s : String;
begin
  fetch := TInternetFetcher.Create;
  try
    fetch.onProgress := progress;
    FTaskDesc := 'Downloading';
    fetch.Buffer := TFslBuffer.create;
    aborted := false;
    s := '';
    result := false;
    try
      fetch.URL := URLPath([url, 'package.tgz']);
      fetch.Fetch;
      result := (fetch.ContentType = 'application/x-compressed') or (fetch.ContentType = 'application/octet-stream') or (fetch.ContentType = 'application/x-tar');
    except
      on e : exception do
      begin
        s := e.Message;
        aborted := e is EAbort;
      end;
    end;
    if not result and not aborted then
    begin
      try
        fetch.URL := url;
        fetch.Fetch;
        result := (fetch.ContentType = 'application/x-compressed') or (fetch.ContentType = 'application/octet-stream') or (fetch.ContentType = 'application/x-tar');
      except
        on e : exception do
        begin
          s := e.Message;
          aborted := e is EAbort;
        end;
      end;
    end;
    if not result and not aborted then
      raise EIOException.create('Unable to find package for '+url+': '+s);
    if result then
    begin
      Import(fetch.Buffer.AsBytes);
    end;
  finally
    fetch.Free;
  end;
end;

function TFHIRPackageManager.isIgnored(s: String): boolean;
begin
  result := StringArrayExists(['ig-r4.json'], s);
end;

procedure TFHIRPackageManager.ListPackageIds(list: TStrings);
var
  s, id : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
  begin
    id := s.subString(s.lastIndexOf('\')+1);
    if id.Contains('#') then
    begin
      id := id.Substring(0, id.IndexOf('#'));
      if list.IndexOf(id) = -1 then
        list.Add(id);
    end;
  end;
end;

function TFHIRPackageManager.readNpmKind(kind, id : String) : TFHIRPackageKind;
begin
  if kind = 'fhir.core' then
    result := fpkCore
  else if kind = 'fhir.ig' then
    result := fpkIG
  else if kind = 'fhir.tool' then
    result := fpkTool
  else if kind = 'fhir.core.gen' then
    result := fpkToolGen
  else if kind = 'fhir.template' then
    result := fpkIGTemplate
  else if kind = 'fhir.examples' then
    result := fpkExamples
  else if (kind = 'fhir.group') or (id= 'hl7.fhir.core') then
    result := fpkGroup
  else if kind <> '' then
    raise ELibraryException.create('Unknown Package Kind: '+kind)
  else if StringArrayExistsSensitive(['hl7.fhir.r2.core', 'hl7.fhir.r3.core', 'hl7.fhir.r4.core'], id) then
    result := fpkCore
  else
    result := fpkIG;
end;

procedure TFHIRPackageManager.ListPackages(kinds : TFHIRPackageKindSet; list: TFslList<TFHIRPackageInfo>);
var
  s : String;
  id, n, ver : String;
  npm : TNpmPackageInfo;
  dep : TJsonObject;
  t, pck : TFHIRPackageInfo;
  v : TFHIRPackageVersionInfo;
  d : TFHIRPackageDependencyInfo;
  ts : TStringList;
  kind : TFHIRPackageKind;
begin
  ts := TStringList.Create;
  try
    for s in TDirectory.GetDirectories(FFolder) do
      ts.Add(s);
    ts.Sort;
    for s in ts do
    begin
      if s.Contains('#') and FileExists(Path([s, 'package', 'package.json'])) then
      begin
        npm := loadPackageFromCache(s);
        try
          n := ExtractFileName(s);
          id := n.Substring(0, n.IndexOf('#'));
          ver := n.Substring(n.LastIndexOf('#')+1);
          if (id <> npm.package.str['name']) or (ver <> npm.package.str['version']) then
          begin
            if (id <> npm.package.str['name']) then
            begin
              npm.package.str['original-name'] := npm.package.str['name'];
              npm.package.str['name'] := id;
            end;
            if (ver <> npm.package.str['version']) then
            begin
              npm.package.str['original-version'] := npm.package.str['version'];
              npm.package.str['version'] := ver;
            end;
            StringToFile(TJsonWriterDirect.writeObjectStr(npm.package, true), Path([s, 'package', 'package.json']), TEncoding.UTF8);
          end;
          kind := readNpmKind(npm.package.str['type'], id);
          if (kind in kinds) then
          begin
            pck := nil;
            try
              for t in list do
                if t.id = id then
                  pck := t.Link;
              if pck = nil then
              begin
                pck := TFHIRPackageInfo.create;
                pck.id := id;
                pck.kind := kind;
                if (id = 'hl7.fhir.core') then
                  list.Insert(0, pck.link)
                else
                  list.add(pck.link);
              end
              else if (pck.kind <> kind) then
                raise ELibraryException.create('Package kind mismatch beteen versions for Package '+id+': '+CODES_TFHIRPackageKind[kind]+'/'+CODES_TFHIRPackageKind[pck.kind]);

              if pck.Canonical = '' then
                pck.Canonical := npm.package.str['canonical'];
              if pck.Canonical = '' then
                pck.Canonical := getUrl(pck.id);
              v := TFHIRPackageVersionInfo.Create(npm.Link);
              try
                v.folder := s;
                v.id := npm.package.str['name'];
                v.version := npm.package.str['version'];
                v.originalId := npm.package.str['original-name'];
                v.originalVersion := npm.package.str['original-version'];
                v.url := npm.package.str['homepage'];
                if (v.url = '') then
                  v.url := npm.package.str['url'];

                if Fini.ReadString('packages', ExtractFileName(s), '') <> '' then
                  v.installed := TFslDateTime.fromFormat('yyyymmddhhnnss', Fini.ReadString('packages', ExtractFileName(s), '')).DateTime
                else
                  v.installed := 0;
                v.size := Fini.ReadInteger('package-sizes', ExtractFileName(s), 0);
                if (v.size = 0) then
                begin
                  v.size := checkPackageSize(s);
                  Fini.WriteInteger('package-sizes', ExtractFileName(s), v.size);
                end;

//                if npm.str['name'] = 'hl7.fhir.core' then
//                  v.fhirVersion := npm.str['version']
//                else
//                begin
                  dep := npm.package.obj['dependencies'];
                  if dep <> nil then
                  begin
                    for n in dep.properties.Keys do
                    begin
                      if not isCoreName(n) then
                      begin
                        d := TFHIRPackageDependencyInfo.Create;
                        try
                          d.id := n;
                          d.version := dep.str[n];
                          v.dependencies.add(d.Link);
                        finally
                          d.Free;
                        end;
                      end
                    end;
                  end;
//                end;
                pck.versions.Add(v.link);
              finally
                v.Free;
              end;
            finally
              pck.Free;
            end;
          end;
        finally
          npm.free;
        end;
      end;
    for pck in list do
      for v in pck.versions do
        for d in v.dependencies do
          d.status := resolve(list, d);
    end;
  finally
    ts.Free;
  end;
end;

procedure TFHIRPackageManager.ListPackagesForFhirVersion(kinds : TFHIRPackageKindSet; core : boolean; version: String; list: TStrings);
var
  l : TFslList<TFHIRPackageInfo>;
  p : TFHIRPackageInfo;
  v : TFHIRPackageVersionInfo;
begin
  list.Clear;
  l := TFslList<TFHIRPackageInfo>.create;
  try
    ListPackages(kinds, l);
    for p in l do
      if core or not StringArrayExistsSensitive(['hl7.fhir.core', 'hl7.fhir.r2.core', 'hl7.fhir.r3.core', 'hl7.fhir.r4.core'], p.id) then
        for v in p.versions do
          if v.fhirVersion = version then
            list.Add(p.id+'#'+v.Version);
  finally
    l.Free;
  end;
end;

procedure TFHIRPackageManager.ListPackageVersions(list: TFslList<TFHIRPackageVersionInfo>);
var
  s : String;
  n, id, ver : String;
  npm : TNpmPackageInfo;
  dep : TJsonObject;
  v : TFHIRPackageVersionInfo;
  d : TFHIRPackageDependencyInfo;
  ts : TStringList;
begin
  ts := TStringList.Create;
  try
    for s in TDirectory.GetDirectories(FFolder) do
      ts.Add(s);
    ts.Sort;
    for s in ts do
    begin
      if s.Contains('#') and FileExists(Path([s, 'package', 'package.json'])) then
      begin
        n := ExtractFileName(s);
        id := n.Substring(0, n.IndexOf('#'));
        ver := n.Substring(n.LastIndexOf('#')+1);

        npm := loadPackageFromCache(s);
        try
          if (id <> npm.package.str['name']) or (ver <> npm.package.str['version']) then
          begin
            if (id <> npm.package.str['name']) then
            begin
              npm.package.str['original-name'] := npm.package.str['name'];
              npm.package.str['name'] := id;
            end;
            if (ver <> npm.package.str['version']) then
            begin
              npm.package.str['original-version'] := npm.package.str['version'];
              npm.package.str['version'] := ver;
            end;
            StringToFile(TJsonWriterDirect.writeObjectStr(npm.package, true), Path([s, 'package', 'package.json']), TEncoding.UTF8);
          end;

          v := TFHIRPackageVersionInfo.Create(npm.Link);
          try
            v.id := npm.package.str['name'];
            v.version := npm.package.str['version'];
            v.originalId := npm.package.str['original-name'];
            v.originalVersion := npm.package.str['original-version'];
            v.kind := readNpmKind(npm.package.str['type'], v.id);
            v.Canonical := npm.package.str['canonical'];
            if v.Canonical = '' then
              v.Canonical := getUrl(v.id);
            v.folder := s;
            v.url := npm.package.str['homepage'];
            if (v.url = '') then
              v.url := npm.package.str['url'];

            if Fini.ReadString('packages', ExtractFileName(s), '') <> '' then
              v.installed := TFslDateTime.fromFormat('yyyymmddhhnnss', Fini.ReadString('packages', ExtractFileName(s), '')).DateTime
            else
              v.installed := 0;
            v.size := Fini.ReadInteger('package-sizes', ExtractFileName(s), 0);
            if (v.size = 0) then
            begin
              v.size := checkPackageSize(s);
              Fini.WriteInteger('package-sizes', ExtractFileName(s), v.size);
            end;

            dep := npm.package.obj['dependencies'];
            if dep <> nil then
            begin
              for n in dep.properties.Keys do
              begin
                d := TFHIRPackageDependencyInfo.Create;
                try
                  d.id := n;
                  d.version := dep.str[n];
                  v.dependencies.add(d.Link);
                finally
                  d.Free;
                end;
              end
            end;

            list.Add(v.link);
          finally
            v.Free;
          end;
        finally
          npm.free;
        end;
      end;
    end;
  finally
    ts.Free;
  end;
end;

procedure TFHIRPackageManager.ListPackages(list: TStrings);
var
  s : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
    if s.Contains('#') then
      list.Add(s);
end;

procedure TFHIRPackageManager.ListPackageVersions(id: String; list: TStrings);
var
  s, f : String;
  ts : TStringList;
begin
  ts := TStringList.Create;
  try
    for s in TDirectory.GetDirectories(FFolder) do
      ts.Add(s);
    ts.Sort;
    for s in TDirectory.GetDirectories(FFolder) do
    begin
      f := s.subString(FFolder.length+1);
      if (f.StartsWith(id+'#')) then
        list.Add(f.Substring(id.Length+1));
    end;
  finally
    ts.free;
  end;
end;

function TFHIRPackageManager.loadArchive(content: TBytes): TDictionary<String, TBytes>;
var
  bo, bi : TBytesStream;
  tar : TTarArchive;
  DirRec : TTarDirRec;
  z : TZDecompressionStream;
  fn : String;
  b : TBytes;
begin
  work(0, false, 'Loading Package');
  try
    result := TDictionary<String, TBytes>.create;
    bo := TBytesStream.Create(content);
    try
      z := TZDecompressionStream.Create(bo, 15+16);
      try
        work(trunc(bo.Position / bo.Size * 100), false, 'Loading Package');
        tar := TTarArchive.Create(z);
        try
          tar.Reset;
          while tar.FindNext(DirRec) do
          begin
            fn := String(DirRec.Name);
            fn := fn.replace('/', '\');
            if not fn.contains('@') then
            begin
              bi := TBytesStream.Create;
              try
                tar.ReadFile(bi);
                b := bi.Bytes;
                if result.ContainsKey(fn) then
                  raise EFSLException.Create('Duplicate Entry: '+fn);
                result.Add(fn, copy(b, 0, bi.Size));
              finally
                bi.free;
              end;
            end;
          end;
        finally
          tar.Free;
        end;
      finally
        z.Free;
      end;
    finally
      bo.free;
    end;
  finally
    work(100, true, '');
  end;
end;

procedure TFHIRPackageManager.loadPackage(id, ver: String; resources: Array of String; loadInfo : TPackageLoadingInformation);
var
  fsl : TFslStringSet;
begin
  fsl := TFslStringSet.Create(resources);
  try
    loadPackage(id, ver, fsl, loadInfo);
  finally
    fsl.Free;
  end;
end;

procedure TFHIRPackageManager.loadPackage(id, ver: String; resources : TFslStringSet; loadInfo : TPackageLoadingInformation);
var
  c : integer;
  s : String;
  f : TFileStream;
  npm : TNpmPackageInfo;
  fi : TNpmFileInfo;
begin
  if loadInfo.isLoaded(id, ver) then
    exit;

  loadInfo.load(id, ver);
  autoInstallPackage(id, ver);
  if not packageExists(id, ver) then
    raise EIOException.create('Unable to load package '+id+'#'+ver+' as it doesn''t exist');
  if (ver = '') then
    ver := latestPackageVersion(id);

  work(0, false, 'Scanning Package');

  npm := loadPackageFromCache(Path([FFolder, id+'#'+ver]));
  try
    if npm.package.has('dependencies') then
    begin
      for s in npm.package.obj['dependencies'].properties.Keys do
      begin
        loadPackage(s, npm.package.obj['dependencies'].str[s], resources, loadInfo);
      end;
    end;
    loadInfo.checkVersion(npm.fhirVersion);

    c := 0;
    for fi in npm.Files do
    begin
      if not isIgnored(ExtractFileName(fi.Name)) and ((resources = nil) or resources.contains(fi.ResourceType)) then
      begin
        f := TFileStream.Create(path([FFolder, id+'#'+ver, 'package', fi.Name]), fmOpenRead + fmShareDenyWrite);
        try
          try
            loadInfo.OnLoadEvent(fi.ResourceType, fi.id, f);
          except
            on e : Exception do
            begin
              raise EFHIRException.Create('Error Parsing '+fi.Name+' in package '+id+'#'+ver+': '+e.message);
            end;

          end;
        finally
          f.Free;
        end;
      end;
      work(trunc((c / npm.files.count) * 100), false, 'Package '+id+'#'+ver+': Load '+fi.Name);
      inc(c);
    end;
    work(100, true, 'Complete');
  finally
    npm.Free;
  end;
end;


function TFHIRPackageManager.loadPackageFromCache(folder: String): TNpmPackageInfo;
begin
  if FCache.TryGetValue(folder, result) then
    result.Link
  else
  begin
    if not FileExists(Path([folder, 'package', '.index.json'])) then
      buildPackageIndex(folder);
    result := TNpmPackageInfo.fromFolder(Path([folder]), false);
    FCache.add(folder, result.Link);
  end;
end;

function TFHIRPackageManager.autoInstallPackage(id, ver: String): boolean;
var
  list : TFslList<TPackageDefinition>;
  pd : TPackageDefinition;
begin
  result := packageExists(id, ver);
  if (not result) then
  begin
    list := TFslList<TPackageDefinition>.create;
    try
      TPackageDefinition.addStandardPackages(list);
      TPackageDefinition.addPackagesFromBuild(list);
      TPackageDefinition.AddCustomPackages(list);
      for pd in list do
        if (pd.Id = id) and ((ver = '') or (pd.Version = ver)) then
          self.install(pd.Url);
    finally
      list.Free;
    end;
    result := packageExists(id, ver);
  end;
end;

procedure TFHIRPackageManager.loadPackage(idver: String; resources: array of String; loadInfo : TPackageLoadingInformation);
var
  id, ver : String;
begin
  StringSplit(idver, '#', id, ver);
  loadPackage(id, ver, resources, loadInfo);
end;

procedure TFHIRPackageManager.buildPackageIndex(folder : String);
var
  s{, bd} : String;
  l : TStringDynArray;
  i : integer;
  indexer : TNpmPackageIndexBuilder;
begin
  indexer := TNpmPackageIndexBuilder.Create;
  try
    l := TDirectory.GetFiles(path([folder, 'package']));
    try
      work(0, false, 'Analysing '+ExtractFileName(folder));
      try
        i := 0;
        for s in l do
        begin
          inc(i);
          work(trunc(i / length(l) * 100), false, 'Analysing '+Path([ExtractFileName(folder),'package',ExtractFileName(s)]));
          indexer.seeFile(ExtractFileName(s), FileToBytes(s));
        end;
      finally
        work(100, true, 'Analysing '+ExtractFileName(folder));
      end;
      StringToFile(indexer.build, path([folder, 'package', '.index.json']), TEncoding.UTF8);
    except
      on e : EAbort do
      begin
        exit; // just swallow this
      end;
      on e : Exception do
      begin
        if not check('Exception analysing package '+folder+'. Continue?') then
          raise;
      end;
    end;
  finally
    indexer.free;
  end;
end;

function TFHIRPackageManager.packageExists(id, ver: String): boolean;
var
  s : String;
begin
  if ver = '' then
  begin
    result := false;
    for s in TDirectory.GetDirectories(FFolder) do
      if ExtractFileName(s).StartsWith(id+'#') then
        exit(true);
  end
  else
    result := FolderExists(Path([FFolder, id+'#'+ver])) and FileExists(Path([FFolder, id+'#'+ver, 'package', 'package.json']));
end;

procedure TFHIRPackageManager.progress(sender: TObject; pct: integer);
begin
  work(pct, false, FTaskDesc);
end;

function TFHIRPackageManager.latestPackageVersion(id: String): String;
var
  s, n : String;
begin
  result := '';
  for s in TDirectory.GetDirectories(FFolder) do
  begin
    n := ExtractFileName(s);
    if n.StartsWith(id+'#') then
      exit(n.Substring(n.IndexOf('#')+1));
  end;
end;

procedure TFHIRPackageManager.remove(id : String);
var
  s, n : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
  begin
    n := s.Substring(s.LastIndexOf('\')+1);
    if (n.StartsWith(id+'#')) then
      if not FolderDelete(s) then
        raise EIOException.create('Unable to delete package '+n+'. Perhaps it is in use?');
  end;
end;

procedure TFHIRPackageManager.remove(id, ver : String);
var
  s, n : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
  begin
    n := s.Substring(s.LastIndexOf('\')+1);
    if (n = id+'#'+ver) then
      if not FolderDelete(s) then
        raise EIOException.create('Unable to delete package '+n+'. Perhaps it is in use?');
  end;
end;

function TFHIRPackageManager.report: String;
var
  b : TStringBuilder;
  list : TFslList<TFHIRPackageInfo>;
  p : TFHIRPackageInfo;
  ts : TStringList;
  s : String;
begin
  b := TStringBuilder.Create;
  try
    b.AppendLine('Packages in '+Folder);
    b.AppendLine;

    list := TFslList<TFHIRPackageInfo>.create;
    try
      ListPackages(All_Package_Kinds, list);
      for p in list do
        p.report(b);
    finally
      list.Free;
    end;

    b.AppendLine;
    b.AppendLine('URL Map:');
    ts := TStringList.Create;
    try
      FIni.ReadSection('urls', ts);
      for s in ts do
        b.AppendLine('  '+s+'='+FIni.ReadString('urls', s, ''));
    finally
      ts.Free;
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFHIRPackageManager.resolve(list: TFslList<TFHIRPackageInfo>; dep: TFHIRPackageDependencyInfo): TFHIRPackageDependencyStatus;
var
  t : TFHIRPackageInfo;
  v : TFHIRPackageVersionInfo;
begin
  result := stNotResolved;
  for t in list do
    if t.id = dep.id then
    begin
      for v in t.versions do
        if v.version = dep.version then
          exit(stOK);
      for v in t.versions do
        if v.version > dep.version then
          exit(stMoreRecentVersion);
    end;
end;

procedure TFHIRPackageManager.work(pct: integer; done: boolean; desc: String);
begin
  if assigned(FOnWork) then
    FOnWork(self, pct, done, desc);
end;

{ TFHIRPackageInfo }

function TFHIRPackageInfo.childCount: integer;
begin
  result := FVersions.Count;
end;

constructor TFHIRPackageInfo.Create;
begin
  inherited;
  FVersions := TFslList<TFHIRPackageVersionInfo>.Create;
end;

destructor TFHIRPackageInfo.Destroy;
begin
  FVersions.Free;
  inherited;
end;

function TFHIRPackageInfo.Link: TFHIRPackageInfo;
begin
  result := TFHIRPackageInfo(inherited Link);
end;

procedure TFHIRPackageInfo.report(b: TStringBuilder);
var
  v : TFHIRPackageVersionInfo;
begin
  b.AppendLine(id+' : '+CODES_TFHIRPackageKind[kind]+' = '+Canonical);
  for v in FVersions do
    v.report(b);
end;

function TFHIRPackageInfo.summary: String;
begin
  if FCanonical <> '' then
    result := FId
  else
    result := FId + '('+FCanonical+')';
end;

{ TFHIRPackageDependencyInfo }

{ TFHIRPackageVersionInfo }

function TFHIRPackageVersionInfo.childCount: integer;
begin
  result := FDependencies.Count;
end;

constructor TFHIRPackageVersionInfo.Create;
begin
  inherited Create;
  FPackage := package;
  FDependencies := TFslList<TFHIRPackageDependencyInfo>.create;
  FProfiles := TFslStringDictionary.create;
  FCanonicals := TFslStringDictionary.create;
end;

function TFHIRPackageVersionInfo.depString: String;
var
  vd : TFHIRPackageDependencyInfo;
begin
  result := '';
  for vd in FDependencies do
  begin
    if not isCoreName(vd.id) then
    begin
      if result <> '' then
        result := result+', ';
      result := result + vd.id+'#'+vd.version;
    end;
  end;
end;

destructor TFHIRPackageVersionInfo.Destroy;
begin
  FPackage.Free;
  FProfiles.Free;
  FCanonicals.Free;
  FDependencies.Free;
  inherited;
end;

function TFHIRPackageVersionInfo.GetFhirVersion: string;
var
  n : String;
begin
  result := '';
  if isCoreName(id) then
    result := FVersion
  else if FPackage.FPackage.has('fhirVersions') then
    result := FPackage.FPackage.arr['fhirVersions'].Value[0]
  else if (FPackage.FPackage.has('dependencies')) then
    for n in FPackage.FPackage.obj['dependencies'].properties.Keys do
      if isCoreName(n) then
        result := FPackage.FPackage.obj['dependencies'].str[n];
end;

function TFHIRPackageVersionInfo.isCore: boolean;
begin
  result := isCoreName(id);
end;

function TFHIRPackageVersionInfo.Link: TFHIRPackageVersionInfo;
begin
  result := TFHIRPackageVersionInfo(inherited link);
end;

function TFHIRPackageVersionInfo.listDependencies: String;
var
  d : TFHIRPackageDependencyInfo;
begin
  result := '';
  for d in dependencies do
    result := result + ', '+d.id+'#'+d.version+' ('+NAMES_TFHIRPackageDependencyStatus[d.status]+')';
  if result <> '' then
    result := result.Substring(2);
end;

function TFHIRPackageVersionInfo.presentation: String;
var
  ts : TStringList;
  n : TNpmFileInfo;
  i : integer;
begin
  result :=
    'id: '+FId+'#'+FVersion+#13#10+
    'canonical: '+FCanonical+#13#10+
    'size: '+DescribeBytes(FSize)+#13#10+
    'installed: '+FormatDateTime('c', FInstalled)+#13#10+
    'dependencies: '+depString+#13#10+
    'fhirVersion: '+FFhirVersion+#13#10+
    'source: '+FFolder+#13#10+
    ''+#13#10;
  ts := TStringList.Create;
  try
    for n in FPackage.Files do
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

procedure TFHIRPackageVersionInfo.report(b: TStringBuilder);
begin
  b.AppendLine('  '+Version+' on '+FhirVersion+' from '+url+' in '+folder);
  b.AppendLine('    dependencies: '+listDependencies);
  if (originalId+originalVersion <> '') then
    b.AppendLine('    original: '+originalId+'#'+originalVersion);
  if installed = 0 then
    b.AppendLine('    install: '+DescribeBytes(size)+' on (n/a)')
  else
    b.AppendLine('    install: '+DescribeBytes(size)+' on '+TFslDateTime.make(installed, dttzLocal).toXML);
  b.AppendLine('    analysis: '+IntToStr(profiles.count)+' '+StringPlural('profile', profiles.Count)+', '+IntToStr(canonicals.count)+' '+StringPlural('canonical', canonicals.Count));
end;

function TFHIRPackageVersionInfo.summary: String;
begin
  result := 'v'+FVersion+' [FHIR v'+FFhirVersion+']';
  if installed > 0 then
    result := result +' ('+DescribeBytes(size)+', '+DescribePeriod(now - installed)+')'
  else
    result := result +' ('+DescribeBytes(size)+')';
end;

{ TFHIRPackageDependencyInfo }

function TFHIRPackageDependencyInfo.Link: TFHIRPackageDependencyInfo;
begin
  result := TFHIRPackageDependencyInfo(inherited link);
end;

function TFHIRPackageDependencyInfo.summary: String;
begin
  result := 'Depends on '+Fid+'#'+Fversion;
  case status of
    stUnknown: ;
    stOK: result := result + ' (found)';
    stMoreRecentVersion: result := result + ' (found more recent version)';
    stNotResolved: result := result + ' (not found)';
  end;
end;

{ TFHIRPackageObject }

function TFHIRPackageObject.childCount: integer;
begin
  result := 0;
end;

function TFHIRPackageObject.presentation: String;
begin
  result := '?';
end;

function TFHIRPackageObject.summary: string;
begin
  result := '?';
end;

{ TPackageDefinition }

function TPackageDefinition.link: TPackageDefinition;
begin
  result := TPackageDefinition(inherited link);
end;

class procedure TPackageDefinition.addPackagesFromBuild(list: TFslList<TPackageDefinition>);
var
  j : TJsonObject;
  a : TJsonArray;
  i : TJsonNode;
  p : TPackageDefinition;
begin
  a := TInternetFetcher.fetchJsonArr('http://build.fhir.org/ig/qas.json');
  try
    for i in a do
    begin
      j := i as TJsonObject;
      if (j.str['package-id'].Contains('.')) then
      begin
        p := TPackageDefinition.Create;
        try
          p.Id := j.str['package-id'];
          p.Version := j.str['ig-ver'];
          p.Canonical := j.str['url'];
          p.Date := TFslDateTime.fromFormat('DDD, dd mmm, yyyy hh:nn:ss Z', j.str['date']).DateTime;
          p.Description := j.str['name'];
          p.FHIRVersion := j.str['version'];
          p.Url := 'http://build.fhir.org/ig/'+j.str['repo'];
          list.Add(p.Link);
        finally
          p.Free;
        end;
      end;
    end;
  finally
    a.Free;
  end;
end;

class procedure TPackageDefinition.AddStandardPackages;
  procedure addPackage(id, version, canonical, fhirversion, url : String; date : TDateTime; desc : String);
  var
    p : TPackageDefinition;
  begin
    p := TPackageDefinition.Create;
    try
      p.Id := id;
      p.Version := version;
      p.Canonical := canonical;
      p.Date := Now;
      p.Description := desc;
      p.FHIRVersion := fhirversion;
      p.Url := url;
      list.Add(p.Link);
    finally
      p.Free;
    end;
  end;
begin
  addPackage('hl7.fhir.r4.core', '4.0.1', 'http://hl7.org/fhir', '4.0.0', 'http://hl7.org/fhir/R4/hl7.fhir.r4.core.tgz', EncodeDate(2018, 12, 18), 'FHIR R4');
  addPackage('hl7.fhir.r3.core', '3.0.2', 'http://hl7.org/fhir', '3.0.2', 'http://hl7.org/fhir/STU3/hl7.fhir.r3.core.tgz', EncodeDate(2017, 4, 19), 'FHIR R3');
  addPackage('hl7.fhir.r2.core', '1.0.2', 'http://hl7.org/fhir', '3.0.2', 'http://hl7.org/fhir/DSTU2/hl7.fhir.r2.core.tgz', EncodeDate(2016, 5, 15), 'FHIR R2');
  addPackage('hl7.fhir.r4.examples', '4.0.1', 'http://hl7.org/fhir', '4.0.0', 'http://hl7.org/fhir/R4/hl7.fhir.r4.examples.tgz', EncodeDate(2018, 12, 18), 'FHIR R4');
  addPackage('hl7.fhir.r3.examples', '3.0.2', 'http://hl7.org/fhir', '3.0.2', 'http://hl7.org/fhir/STU3/hl7.fhir.r3.examples.tgz', EncodeDate(2017, 4, 19), 'FHIR R3');
  addPackage('hl7.fhir.r2.examples', '1.0.2', 'http://hl7.org/fhir', '3.0.2', 'http://hl7.org/fhir/DSTU2/hl7.fhir.r2.examples.tgz', EncodeDate(2016, 5, 15), 'FHIR R2');
end;

class procedure TPackageDefinition.AddCustomPackages;
  procedure add(id, version, canonical, desc, fver, uri : String);
  var
    p : TPackageDefinition;
  begin
    p := TPackageDefinition.Create;
    try
      p.Id := id;
      p.Version := version;
      p.Canonical := canonical;
      p.Date := Now;
      p.Description := desc;
      p.FHIRVersion := fver;
      p.Url := uri;
      list.Add(p.Link);
    finally
      p.Free;
    end;
  end;
begin
  add('fhir.tx.support', '4.0.0', 'http://fhir.org/test', 'tx.fhir.org definitions', '4.0.1', 'http://fhir.org/packages/fhir.tx.support/4.0.0');
  add('fhir.tx.support', '3.0.1', 'http://fhir.org/test', 'tx.fhir.org definitions', '3.0.2', 'http://fhir.org/packages/fhir.tx.support/3.0.1');
  add('fhir.tx.support', '1.0.2', 'http://fhir.org/test', 'tx.fhir.org definitions', '1.0.2', 'http://fhir.org/packages/fhir.tx.support/1.0.2');
  add('fhir.argonaut.ehr', '1.0.0', 'http://fhir.org/guides/argonaut', 'Argonaut EHR Query', '1.0.2', 'http://www.fhir.org/guides/argonaut/r2');
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
      // ignore....
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


{ TNpmPackageInfo }

constructor TNpmPackageInfo.Create;
begin
  inherited create;
  FFiles := TFslList<TNpmFileInfo>.create;
  FPackage := nil;
end;

destructor TNpmPackageInfo.Destroy;
begin
  FFiles.Free;
  FPackage.Free;
  FIndex.Free;
  inherited;
end;

class function TNpmPackageInfo.fromPackage(bytes : TBytes): TNpmPackageInfo;
var
  files : TDictionary<String, TBytes>;
  indexer : TNpmPackageIndexBuilder;
  s : String;
  fi : TJsonObject;
  i : TJsonNode;
  nfi : TNpmFileInfo;
begin
  result := TNpmPackageInfo.Create;
  try
    files := loadArchive(bytes);
    try
      result.FPackage := TJSONParser.Parse(files['package\package.json']);
      if not files.ContainsKey('package\.index.json') then
      begin
        indexer := TNpmPackageIndexBuilder.create;
        try
          for s in files.Keys do
          begin
            if (length(files[s]) > 0) and (s.StartsWith('package\')) then
              indexer.seeFile(s.Substring(8), files[s]);
          end;
          result.FIndex := TJSONParser.Parse(indexer.build);
        finally
          indexer.free;
        end;
      end
      else
       result.FIndex := TJSONParser.Parse(files['package\.index.json']);
    for i in result.FIndex.forceArr['files'] do
    begin
      fi := i as TJsonObject;
      nfi := TNpmFileInfo.Create;
      try
        nfi.Name := fi.str['filename'];
        nfi.ResourceType := fi.str['resourceType'];
        nfi.Id := fi.str['id'];
        nfi.URL := fi.str['url'];
        nfi.Version := fi.str['version'];
        nfi.Kind := fi.str['kind'];
        nfi.TypeV := fi.str['type'];
        nfi.size := length(files['package\'+nfi.Name]);

        result.FFiles.Add(nfi.Link);
      finally
        nfi.Free;
      end;
    end;
    finally
      files.Free;
    end;
    result.link;
  finally
    result.free;
  end;
end;

class function TNpmPackageInfo.fromSource(source: String; size : boolean): TNpmPackageInfo;
begin
  if FolderExists(source) then
    result := fromFolder(source, size)
  else
    result := fromPackage(FileToBytes(source));
end;

function TNpmPackageInfo.GetAuthor: string;
begin
  result := FPackage.vStr['author'];
end;

function TNpmPackageInfo.GetCanonical: string;
begin
  result := FPackage.vStr['canonical'];
end;

function TNpmPackageInfo.GetDependencies: string;
var
  dep : TJsonObject;
  s : String;
begin
  result := '';
  dep := FPackage.obj['dependencies'];
  if dep <> nil then
  begin
    for s in dep.properties.Keys do
    begin
      result := result + ', '+s+'#'+dep.str[s];
    end;
    result := result.Substring(2);
  end;
end;

function TNpmPackageInfo.GetDescription: string;
begin
  result := FPackage.vStr['description'];
end;

function TNpmPackageInfo.GetFHIRVersion: String;
var
  arr : TJsonArray;
begin
  arr := FPackage.arr['fhir-version-list'];
  if arr = nil then
    raise Exception.Create('Unable to process package '+packageId+'#'+version+': size(fhir-version-list) == '+inttostr(arr.Count))
  else if arr.Count <> 1 then
    raise Exception.Create('Unable to process package '+packageId+'#'+version+': size(fhir-version-list) == '+inttostr(arr.Count));
  result := arr.Value[0];
end;

function TNpmPackageInfo.GetFhirVersions: string;
var
  arr : TJsonArray;
  i : integer;
begin
  result := '';
  arr := FPackage.arr['fhir-version-list'];
  if arr <> nil then
  begin
    for i := 0 to arr.Count - 1 do
    begin
      result := result + ', '+arr.Value[i];
    end;
    result := result.Substring(2);
  end;
end;

function TNpmPackageInfo.GetHomePage: string;
begin
  result := FPackage.vStr['homePage'];
end;

function TNpmPackageInfo.GetLicense: string;
begin
  result := FPackage.vStr['license'];
end;

function TNpmPackageInfo.GetPackageId: string;
begin
  result := FPackage.vStr['name'];
end;

function TNpmPackageInfo.GetTitle: string;
begin
  result := FPackage.vStr['title'];
end;

function TNpmPackageInfo.GetType: string;
begin
  result := FPackage.vStr['type'];
end;

function TNpmPackageInfo.GetUrl: string;
begin
  result := FPackage.vStr['url'];
end;

function TNpmPackageInfo.GetVersion: string;
begin
  result := FPackage.vStr['version'];
end;

function TNpmPackageInfo.Link: TNpmPackageInfo;
begin
  result := TNpmPackageInfo(inherited link);
end;

class function TNpmPackageInfo.loadArchive(content: TBytes): TDictionary<String, TBytes>;
var
  bo, bi : TBytesStream;
  tar : TTarArchive;
  DirRec : TTarDirRec;
  z : TZDecompressionStream;
  fn : String;
  b : TBytes;
begin
  result := TDictionary<String, TBytes>.create;
  try
    bo := TBytesStream.Create(content);
    try
      z := TZDecompressionStream.Create(bo, 15+16);
      try
        tar := TTarArchive.Create(z);
        try
          tar.Reset;
          while tar.FindNext(DirRec) do
          begin
            fn := String(DirRec.Name);
            fn := fn.replace('/', '\');
            if not fn.contains('@') then
            begin
              bi := TBytesStream.Create;
              try
                tar.ReadFile(bi);
                b := bi.Bytes;
                if result.ContainsKey(fn) then
                  raise EFSLException.Create('Duplicate Entry: '+fn);
                result.Add(fn, copy(b, 0, bi.Size));
              finally
                bi.free;
              end;
            end;
          end;
        finally
          tar.Free;
        end;
      finally
        z.Free;
      end;
    finally
      bo.free;
    end;
  except
    result.Free;
    raise;
  end;
end;

class function TNpmPackageInfo.fromFolder(folder : String; size : boolean) : TNpmPackageInfo;
var
  fi : TJsonObject;
  i : TJsonNode;
  nfi : TNpmFileInfo;
begin
  result := TNpmPackageInfo.Create;
  try
    result.FPackage := TJSONParser.ParseFile(Path([folder, 'package', 'package.json']));
    result.FIndex := TJSONParser.ParseFile(Path([folder, 'package', '.index.json']));
    for i in result.FIndex.forceArr['files'] do
    begin
      fi := i as TJsonObject;
      nfi := TNpmFileInfo.Create;
      try
        nfi.Name := fi.str['filename'];
        nfi.ResourceType := fi.str['resourceType'];
        nfi.Id := fi.str['id'];
        nfi.URL := fi.str['url'];
        nfi.Version := fi.str['version'];
        nfi.Kind := fi.str['kind'];
        nfi.TypeV := fi.str['type'];
        if (size) then
          nfi.size := FileSize(path([folder, 'package', nfi.Name]));

        result.FFiles.Add(nfi.Link);
      finally
        nfi.Free;
      end;
    end;
    result.link;
  finally
    result.Free;
  end;
end;

{ TNpmFileInfo }

function TNpmFileInfo.Link: TNpmFileInfo;
begin
  result := TNpmFileInfo(inherited link);
end;

function TNpmFileInfo.matches(text: String): boolean;
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

{ TPackageLoadingInformation }

constructor TPackageLoadingInformation.Create;
begin
  inherited Create;
  FLoaded := TStringList.Create;
  FVersion := TFHIRVersions.getMajMin(ver);
end;

destructor TPackageLoadingInformation.Destroy;
begin
  FLoaded.Free;
  inherited;
end;

procedure TPackageLoadingInformation.checkVersion(ver: String);
begin
  ver := TFHIRVersions.getMajMin(ver);
  if FVersion = '' then
    FVersion := ver
  else if FVersion <> ver then
    raise Exception.Create('FHIR Loading Version mismatch: loading '+ver+' but already loaded '+FVersion);
end;

function TPackageLoadingInformation.isLoaded(id, ver: String): boolean;
begin
  result := FLoaded.IndexOf(id+'#'+ver) > -1;
end;

procedure TPackageLoadingInformation.load(id, ver: String);
begin
  FLoaded.Add(id+'#'+ver);
end;

end.

