unit fsl_npm_cache;

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
  {$IFDEF WINDOWS} Windows, {$ELSE} LazFileUtils, {$ENDIF}
  SysUtils, Classes, IniFiles, Generics.Collections, Types, {$IFDEF DELPHI} IOUtils, {$ENDIF}
  fsl_base,  fsl_utilities, fsl_json, fsl_fpc, fsl_threads, fsl_logging, fsl_stream, fsl_fetcher, fsl_versions,
  fsl_npm, fsl_npm_client, fsl_gzip;

type
  TCheckEvent = function(sender : TObject; msg : String):boolean of object;
  TPackageLoadingEvent = procedure (packageId : String; rType, id : String; stream : TStream) of object;

  TPackageDefinition = class (TFslObject)
  private
    FId : String;
    FVersion : String;
    FCanonical : String;
    FDate : TDateTime;
    FDescription : String;
    FFHIRVersion : String;
    FUrl: String;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    function link : TPackageDefinition; overload;

    property Id : String read FId write FId;
    property Version : String read FVersion write FVersion;
    property Canonical : String read FCanonical write FCanonical;
    property Date : TDateTime read FDate write FDate;
    property Description : String read FDescription write FDescription;
    property FHIRVersion : String read FFHIRVersion write FFHIRVersion;
    property Url : String read FUrl write FUrl;
  end;

  TPackageLoadingInformation = class (TFslObject)
  private
    FVersion: TSemanticVersion;
    FLoaded: TStringList;
    FOnLoadEvent: TPackageLoadingEvent;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(ver : string);
    destructor Destroy; override;

    property OnLoadEvent : TPackageLoadingEvent read FOnLoadEvent write FOnLoadEvent;

    procedure checkVersion(ver : String);
    function isLoaded(id, ver: String) : boolean;
    procedure load(id, ver: String);
  end;

  TFHIRPackageManagerMode = (npmModeNone, npmModeUser, npmModeSystem, npmModeTesting);

  { TFHIRPackageManager }

  TFHIRPackageManager = class (TFslObject)
  private
    FMode : TFHIRPackageManagerMode;
    FFolder : String;
    FIni : TIniFile;
    FOnWork : TWorkProgressEvent;
    FOnCheck : TCheckEvent;
    FLock : TFslLock;
    FCache : TFslMap<TNpmPackage>;
    FTaskDesc : String;
    FCaching : boolean;

    function loadArchive(content : TBytes; description : String) : TDictionary<String, TBytes>;
    procedure clearCache;
    function checkPackageSize(dir : String) : integer;
    procedure work(pct : integer; done : boolean; desc : String);
    procedure progress(sender : TObject; pct : integer);
    function check(desc : String) : boolean;
    procedure buildPackageIndex(folder : String);
    function latestPackageVersion(id: String): String;
    function isIgnored(s : String): boolean;
    procedure init;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(mode : TFHIRPackageManagerMode); overload;
    constructor Create(dir : String); overload;
    destructor Destroy; override;
    function Link : TFHIRPackageManager;

    property OnWork : TWorkProgressEvent read FOnWork write FOnWork;
    property OnCheck : TCheckEvent read FOnCheck write FOnCheck;

    property Folder : String read FFolder;
    property Mode : TFHIRPackageManagerMode read FMode;
    function description : String;
    property Caching : boolean read FCaching write FCaching;

    procedure listAllKnownPackages(list: TFslList<TFHIRPackageInfo>; ver : String);
    procedure ListPackageIds(list : TStrings);
    procedure ListPackages(list : TStrings); overload;
    procedure ListPackages(kinds : TFHIRPackageKindSet; list : TFslList<TNpmPackage>); overload;
    procedure ListAllPackages(list : TFslList<TNpmPackage>); overload;

    function packageExists(id, ver : String;  allowed : TSemanticVersionLevel = semverMinor) : boolean; overload;
    function PathForPackage(id, ver : String; allowed : TSemanticVersionLevel = semverMinor) : String;
    function autoInstallPackage(id, ver : String) : boolean; overload;
    function latestPublishedVersion(id : String) : String;

    function loadPackageFromCache(folder : String) : TNpmPackage;
    function loadPackage(id : String) : TNpmPackage; overload;
    function loadPackage(id, ver : String) : TNpmPackage; overload;
    procedure loadPackage(id, ver : String; resources : Array of String; loadInfo : TPackageLoadingInformation); overload;
    procedure loadPackage(idver : String; resources : Array of String; loadInfo : TPackageLoadingInformation); overload;
    procedure loadPackage(id, ver : String; resources : TFslStringSet; loadInfo : TPackageLoadingInformation); overload;

    procedure clear;
    procedure UnLoad;

    function import(content : TBytes; description : String) : TNpmPackage; overload;
    function install(url : String) : boolean;

    procedure remove(id : String); overload;
    procedure remove(id, ver: String); overload;

    function getId(url : String) : String;
    function getUrl(id : String) : String;

    function report : String;
  end;

  { TFHIRLoadPackagesTaskRequest }

  TFHIRLoadPackagesTaskRequest = class (TBackgroundTaskRequestPackage)
  private
    FManager: TFHIRPackageManager;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(manager : TFHIRPackageManager);
    destructor Destroy; override;

    function description : String; override;

    property Manager : TFHIRPackageManager read FManager;
  end;

  { TFHIRLoadPackagesTaskResponse }

  TFHIRLoadPackagesTaskResponse = class (TBackgroundTaskResponsePackage)
  private
    FPackages : TFslList<TNpmPackage>;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Packages :  TFslList<TNpmPackage> read FPackages;
  end;

  { TFHIRLoadPackagesTaskEngine }

  TFHIRLoadPackagesTaskEngine  = class (TBackgroundTaskEngine)
  private
    procedure doWork(sender : TObject; pct : integer; done : boolean; desc : String);
  protected
    function canCancel : boolean; override;
  public
    function name : String; override;
    procedure execute(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage); override;
  end;

const
//  NAMES_TFHIRPackageDependencyStatus : Array [TFHIRPackageDependencyStatus] of String = ('?', 'ok', 'ver?', 'bad');
  ANALYSIS_VERSION = 2;
  CACHE_VERSION = 3;


var
  ForcedNpmCacheMode : TFHIRPackageManagerMode = npmModeNone;
  GPackageLoaderTaskId : integer;


implementation

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

{ TFHIRLoadPackagesTaskEngine }

function TFHIRLoadPackagesTaskEngine.canCancel: boolean;
begin
  result := false;
end;

procedure TFHIRLoadPackagesTaskEngine.doWork(sender: TObject; pct: integer; done: boolean; desc: String);
begin
  progress(desc, pct);
end;

function TFHIRLoadPackagesTaskEngine.name: String;
begin
  result := 'Load Packages';
end;

procedure TFHIRLoadPackagesTaskEngine.execute(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage);
var
  req : TFHIRLoadPackagesTaskRequest;
  resp : TFHIRLoadPackagesTaskResponse;
begin
  req := request as TFHIRLoadPackagesTaskRequest;
  resp := response as TFHIRLoadPackagesTaskResponse;
  req.Manager.OnWork := doWork;
  req.Manager.ListAllPackages(resp.Packages);
end;


{ TFHIRLoadPackagesTaskResponse }

constructor TFHIRLoadPackagesTaskResponse.Create;
begin
  inherited Create;
  FPackages := TFslList<TNpmPackage>.Create;
end;

destructor TFHIRLoadPackagesTaskResponse.Destroy;
begin
  FPackages.free;
  inherited Destroy;
end;

function TFHIRLoadPackagesTaskResponse.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FPackages.sizeInBytes(magic));
end;

{ TFHIRLoadPackagesTaskRequest }

constructor TFHIRLoadPackagesTaskRequest.Create(manager: TFHIRPackageManager);
begin
  inherited Create;
  FManager := manager;
end;

destructor TFHIRLoadPackagesTaskRequest.Destroy;
begin
  FManager.free;
  inherited Destroy;
end;

function TFHIRLoadPackagesTaskRequest.description: String;
begin
  result := '';
end;

function TFHIRLoadPackagesTaskRequest.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FManager.sizeInBytes(magic));
end;

{ TFHIRPackageManager }

constructor TFHIRPackageManager.Create(mode : TFHIRPackageManagerMode);
begin
  inherited Create;
  if ForcedNpmCacheMode <> npmModeNone then
    FMode := ForcedNpmCacheMode
  else
    FMode := mode;
  {$IFDEF WINDOWS}
  case FMode of
    npmModeNone: raise EFslException.create('Error- must provide a NPM Cache mode');
    npmModeUser: FFolder := FilePath([UserFolder, '.fhir', 'packages']);
    npmModeSystem: FFolder := FilePath([ProgData, '.fhir', 'packages']);
    npmModeTesting: FFolder := FilePath(['[tmp]', '.fhir', 'packages']);
  end;
  {$ELSE}
  case FMode of
    npmModeNone: raise EFslException.create('Error- must provide a NPM Cache mode');
    npmModeUser: FFolder := ExpandFileNameUTF8('~/.fhir/packages');
    npmModeSystem: FFolder := '/var/lib/.fhir/packages';
    npmModeTesting: FFolder := FilePath(['[tmp]', '.fhir', 'packages']);
  end;
  {$ENDIF}
  init;
end;

constructor TFHIRPackageManager.Create(dir: String);
begin
  inherited Create;
  if (ForcedNpmCacheMode <> npmModeNone) then
    raise EFslException.Create('Unable to create PackageManager for a specific directory');
  FFolder := dir;
  init;
end;

destructor TFHIRPackageManager.Destroy;
begin
  FCache.free;
  FIni.free;
  FLock.free;
  Inherited;
end;

procedure TFHIRPackageManager.init;
begin
  FLock := TFslLock.Create('PackageManager');
  FCache := TFslMap<TNpmPackage>.create('Npm Package manager');
  ForceFolder(FFolder);
  FIni := TIniFile.create(FilePath([FFolder, 'packages.ini']));
  if FIni.ReadInteger('cache', 'version', 0) <> CACHE_VERSION then
  begin
    clearCache;
    FIni.WriteInteger('cache', 'version', CACHE_VERSION);
  end;
end;

function TFHIRPackageManager.Link: TFHIRPackageManager;
begin
  result := TFHIRPackageManager(Inherited Link);
end;

procedure TFHIRPackageManager.listAllKnownPackages(list: TFslList<TFHIRPackageInfo>; ver: String);
var
  s : String;
  npm : TNpmPackage;
  t : TFHIRPackageInfo;
  found : boolean;
begin
  for s in TDirectory.GetDirectories(FFolder) do
  begin
    npm := TNpmPackage.fromFolderQuick(s);
    try
      if TSemanticVersion.matches(npm.fhirVersion, ver, semverMinor) then
      begin
        found := false;
        for t in list do
          if npm.name = t.id then
            found := true;
        if not found then
          list.Add(TFHIRPackageInfo.Create(npm.name, npm.version, npm.fhirVersion, npm.description, npm.canonical, npm.url));
      end;
    finally
      npm.free;
    end;
  end;
  TFHIRPackageClient.loadPackagesForVersion(list, PACKAGE_SERVER_BACKUP, ver);
  TFHIRPackageClient.loadPackagesForVersion(list, PACKAGE_SERVER_PRIMARY, ver);
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
  FLock.Lock('clearCache');
  try
    FCache.Clear;
  finally
    FLock.Unlock;
  end;
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
  case FMode of
    npmModeNone:result := 'Unknown Mode Cache'; // can't happen
    npmModeUser:result := 'User Cache';
    npmModeSystem:result := 'System Cache';
    npmModeTesting:result := 'Testing Cache';
  end;
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
    list.free;
  end;
end;

function TFHIRPackageManager.getUrl(id: String): String;
begin
  result := FIni.ReadString('urls', id, '');
end;


function TFHIRPackageManager.import(content : TBytes; description : String) : TNpmPackage;
var
  npm : TJsonObject;
  id, ver, dir, fn, fver, s : String;
  files : TDictionary<String, TBytes>;
  size,c : integer;
  indexer : TNpmPackageIndexBuilder;
begin
  files := loadArchive(content, description);
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
      npm.free;
    end;
    if packageExists(id, ver) then
      if not check('Replace existing copy of '+id+' version '+ver+'?') then
        exit(nil);
    work(0, false, 'Installing');
    dir := FilePath([FFolder, id+'#'+ver]);
    if FolderExists(dir) then
      if not FolderDelete(dir) then
        raise EIOException.create('Unable to delete existing package');
    ForceFolder(dir);
    size := 0;
    c := 0;
    indexer := TNpmPackageIndexBuilder.Create;
    try
      for s in files.Keys do
      begin
        inc(c);
        work(trunc((c / files.Count) * 100), false, 'Installing');
        if length(files[s]) > 0 then
        begin
          fn := FilePath([dir, s]);
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
      StringToFile(indexer.build, FilePath([FFolder, id+'#'+ver, 'package', '.index.json']), TEncoding.UTF8);
      Fini.WriteInteger('package-sizes', id+'#'+ver, size);
      Fini.WriteString('packages', id+'#'+ver, FormatDateTime('yyyymmddhhnnss', now));
      work(100, true, 'Installing');
      result := TNpmPackage.fromFolderQuick(dir);
      result.size := size;
      result.installed := now;
    finally
      indexer.free;
    end;
  finally
    files.free;
  end;
end;

function TFHIRPackageManager.install(url: String) : boolean;
var
  fetch : TInternetFetcher;
  aborted : boolean;
  s : String;
begin
  Logging.log('Installing Package from '+url+' to '+FFolder);
  fetch := TInternetFetcher.Create;
  try
    fetch.onProgress := progress;
    FTaskDesc := 'Downloading from '+url;
    fetch.Buffer := TFslBuffer.Create;
    aborted := false;
    s := '';
    result := false;
    try
      fetch.URL := url;
      fetch.Fetch;
      result := StringArrayExists(['application/x-compressed', 'application/octet-stream', 'application/x-tar', 'application/tar+gzip'], fetch.ContentType);
    except
      on e : exception do
      begin
        s := e.Message;
        aborted := e is EAbort;
      end;
    end;
    if not result and not aborted then
      raise EIOException.create('Unable to find package for '+url+': '+s);
    if result then
    begin
      Import(fetch.Buffer.AsBytes, url).free;
    end;
  finally
    fetch.free;
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

procedure TFHIRPackageManager.ListPackages(kinds : TFHIRPackageKindSet; list: TFslList<TNpmPackage>);
var
  ts : TStringList;
  s : String;
  id, n, ver : String;
  npm : TNpmPackage;
begin
  ts := TStringList.Create;
  try
    for s in TDirectory.GetDirectories(FFolder) do
      ts.Add(s);
    ts.Sort;
    for s in ts do
    begin
      work(trunc(1/ts.count * 100), false, 'Package '+ExtractFileName(s));
      if s.Contains('#') and FileExists(FilePath([s, 'package', 'package.json'])) then
      begin
        npm := loadPackageFromCache(s);
        try
          n := ExtractFileName(s);
          id := n.Substring(0, n.IndexOf('#'));
          ver := n.Substring(n.LastIndexOf('#')+1);
          if (npm.kind in kinds) then
          begin
            if (id = 'hl7.fhir.core') then
              list.Insert(0, npm.link)
             else
               list.add(npm.link);
            if Fini.ReadString('packages', ExtractFileName(s), '') <> '' then
              npm.installed := TFslDateTime.fromFormat('yyyymmddhhnnss', Fini.ReadString('packages', ExtractFileName(s), '')).DateTime
            else
              npm.installed := 0;
            npm.size := Fini.ReadInteger('package-sizes', ExtractFileName(s), 0);
            if (npm.size = 0) then
            begin
              npm.size := checkPackageSize(s);
              Fini.WriteInteger('package-sizes', ExtractFileName(s), npm.size);
            end;
          end;
        finally
          npm.free;
        end;
      end;
    end;
    work(100, true, '');
  finally
    ts.free;
  end;
end;

procedure TFHIRPackageManager.ListAllPackages(list: TFslList<TNpmPackage>);
var
  ts : TStringList;
  s : String;
  id, n, ver : String;
  npm : TNpmPackage;
begin
  ts := TStringList.Create;
  try
    for s in TDirectory.GetDirectories(FFolder) do
      ts.Add(s);
    ts.Sort;
    for s in ts do
    begin
      work(trunc(1/ts.count * 100), false, 'Package '+ExtractFileName(s));
      if s.Contains('#') and FileExists(FilePath([s, 'package', 'package.json'])) then
      begin
        npm := TNpmPackage.fromFolderQuick(s);
        try
          n := ExtractFileName(s);
          id := n.Substring(0, n.IndexOf('#'));
          ver := n.Substring(n.LastIndexOf('#')+1);
          list.add(npm.link);
          if Fini.ReadString('packages', ExtractFileName(s), '') <> '' then
            npm.installed := TFslDateTime.fromFormat('yyyymmddhhnnss', Fini.ReadString('packages', ExtractFileName(s), '')).DateTime
          else
            npm.installed := 0;
          npm.size := Fini.ReadInteger('package-sizes', ExtractFileName(s), 0);
          if (npm.size = 0) then
          begin
            npm.size := checkPackageSize(s);
            Fini.WriteInteger('package-sizes', ExtractFileName(s), npm.size);
          end;
        finally
          npm.free;
        end;
      end;
    end;
    work(100, true, '');
  finally
    ts.free;
  end;
end;

procedure TFHIRPackageManager.clear;
var
  s : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
    if s.Contains('#') then
      FolderDelete(s);
end;

procedure TFHIRPackageManager.UnLoad;
begin
  FCache.Clear;
end;

procedure TFHIRPackageManager.ListPackages(list: TStrings);
var
  s : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
    if s.Contains('#') then
      list.Add(s);
end;

function TFHIRPackageManager.loadArchive(content: TBytes; description : String): TDictionary<String, TBytes>;
var
  bo, bi : TBytesStream;
  tar : TTarArchive;
  DirRec : TTarDirRec;
  fn : String;
  b : TBytes;
begin
  Logging.log('Loading Package ('+DescribeBytes(length(content))+')');
  work(0, false, 'Loading Package ('+DescribeBytes(length(content))+')');
  try
    result := TDictionary<String, TBytes>.Create;
    bo := TBytesStream.create(ungzip(content, description));
    try
      work(trunc(bo.Position / bo.Size * 100), false, 'Loading Package');
      tar := TTarArchive.Create(bo);
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
              if not result.ContainsKey(fn) then
                result.Add(fn, copy(b, 0, bi.Size));
//                else
//                  raise EFSLException.Create('Duplicate Entry: '+fn);
            finally
              bi.free;
            end;
          end;
        end;
      finally
        tar.free;
      end;
    finally
      bo.free;
    end;
  finally
    work(100, true, '');
  end;                   
  Logging.log('Loaded Package ('+inttostr(result.Count)+' files)');
end;

procedure TFHIRPackageManager.loadPackage(id, ver: String;
  resources: array of String; loadInfo: TPackageLoadingInformation);
var
  fsl : TFslStringSet;
begin
  fsl := TFslStringSet.Create(resources);
  try
    loadPackage(id, ver, fsl, loadInfo);
  finally
    fsl.free;
  end;
end;


function TFHIRPackageManager.loadPackage(id : String) : TNpmPackage;
var
  ver : String;
begin
  if id.contains('#') then
  begin
    StringSplit(id, '#', id, ver);
    result := loadPackage(id, ver);
  end
  else
  begin
    autoInstallPackage(id, '');
    if not packageExists(id, '') then
      raise EIOException.create('Unable to load package '+id+' as it couldn''t be found');
    ver := latestPackageVersion(id);
    result := loadPackageFromCache(PathForPackage(id, ver));
  end;
end;

function TFHIRPackageManager.loadPackage(id, ver : String) : TNpmPackage;
begin
  if ver = '' then
    result := loadPackage(id)
  else
  begin
    autoInstallPackage(id, ver);
    if not packageExists(id, ver) then
      raise EIOException.create('Unable to load package '+id+'#'+ver+' as it couldn''t be found');
    result := loadPackageFromCache(PathForPackage(id, ver));
  end;
end;

procedure TFHIRPackageManager.loadPackage(id, ver: String; resources : TFslStringSet; loadInfo : TPackageLoadingInformation);
var
  c : integer;
  p, s : String;
  f : TFileStream;
  npm : TNpmPackage;
  fi : TNpmPackageResource;
begin
  if loadInfo.isLoaded(id, ver) then
    exit;

  loadInfo.load(id, ver);
  autoInstallPackage(id, ver);
  if not packageExists(id, ver) then
    raise EIOException.create('Unable to load package '+id+'#'+ver+' as it doesn''t exist');
  p := PathForPackage(id, ver);
  if (ver = '') then
    ver := latestPackageVersion(id);

  work(0, false, 'Scanning Package');

  npm := loadPackageFromCache(p);
  try
    if npm.info.has('dependencies') then
    begin
      for s in npm.info.obj['dependencies'].properties.Keys do
      begin
        loadPackage(s, npm.info.obj['dependencies'].str[s], resources, loadInfo);
      end;
    end;
    loadInfo.checkVersion(npm.fhirVersion);

    c := 0;
    for fi in npm.Folders['package'].resources do
    begin
      if not isIgnored(ExtractFileName(fi.Name)) and ((resources = nil) or resources.contains(fi.ResourceType)) then
      begin
        f := TFileStream.Create(FilePath([p, 'package', fi.Name]), fmOpenRead + fmShareDenyWrite);
        try
          try
            loadInfo.OnLoadEvent(npm.name+'#'+npm.version, fi.ResourceType, fi.id, f);
          except
            on e : Exception do
            begin
              raise EFslException.Create('Error Parsing '+fi.Name+' in package '+id+'#'+ver+': '+e.message);
            end;

          end;
        finally
          f.free;
        end;
      end;
      work(trunc((c / npm.Folders['package'].resources.count) * 100), false, 'Package '+id+'#'+ver+': Load '+fi.Name);
      inc(c);
    end;
    work(100, true, 'Complete');
  finally
    npm.free;
  end;
end;


function TFHIRPackageManager.loadPackageFromCache(folder: String): TNpmPackage;
var
  found : boolean;
begin
  FLock.Lock('loadPackageFromCache');
  try
    found := FCache.TryGetValue(folder, result);
    if found then
      result.Link;
  finally
    FLock.Unlock;
  end;

  if not found then
  begin
    if not FileExists(FilePath([folder, 'package', '.index.json'])) then
      buildPackageIndex(folder);
    result := TNpmPackage.fromFolder(folder);
    if FCaching then
    begin
      FLock.Lock('loadPackageFromCache2');
      try
        FCache.add(folder, result.Link);
      finally
        FLock.Unlock;
      end;
    end;
  end;
end;

function TFHIRPackageManager.autoInstallPackage(id, ver: String): boolean;
var
  list : TFslList<TFHIRPackageInfo>;
  t, pd : TFHIRPackageInfo;
begin
  if (ver = '') then
    ver := latestPublishedVersion(id);
  result := packageExists(id, ver, semverLabel);
  if (not result) then
  begin
    list := TFslList<TFHIRPackageInfo>.Create;
    try
      if (ver = 'current') then
        TFHIRPackageClient.LoadPackages(list, PACKAGE_SERVER_CIBUILD, id)
      else
      begin
        TFHIRPackageClient.LoadPackages(list, PACKAGE_SERVER_PRIMARY, id);
        TFHIRPackageClient.LoadPackages(list, PACKAGE_SERVER_BACKUP, id);
      end;

      pd := nil;
      for t in list do
        if (t.Id = id) and ((ver = '') or TSemanticVersion.matches(t.Version, ver, semverPatch)) and ((pd = nil) or isMoreRecentVersion(t.version, pd.version)) then
          pd := t;
      if (pd <> nil) then
        self.install(pd.Url);
    finally
      list.free;
    end;
    result := packageExists(id, ver);
  end
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
    l := TDirectory.GetFiles(FilePath([folder, 'package']));
    try
      work(0, false, 'Analysing '+ExtractFileName(folder));
      try
        i := 0;
        for s in l do
        begin
          inc(i);
          work(trunc(i / length(l) * 100), false, 'Analysing '+FilePath([ExtractFileName(folder),'package',ExtractFileName(s)]));
          indexer.seeFile(ExtractFileName(s), FileToBytes(s));
        end;
      finally
        work(100, true, 'Analysing '+ExtractFileName(folder));
      end;
      StringToFile(indexer.build, FilePath([folder, 'package', '.index.json']), TEncoding.UTF8);
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

function TFHIRPackageManager.packageExists(id, ver: String;  allowed : TSemanticVersionLevel = semverMinor): boolean;
begin
  result := PathForPackage(id, ver) <> '';
end;

function TFHIRPackageManager.PathForPackage(id, ver: String; allowed : TSemanticVersionLevel = semverMinor): String;
var
  ts : TStringlist;
  s, n, t : String;
  a : TSemanticVersionLevel;
begin
  result := '';
  ts := TStringList.Create;
  try
    for s in TDirectory.GetDirectories(FFolder) do
      ts.Add(s);
    for a := semverLabel downto allowed do
    begin
      for s in ts do
      begin
        n := ExtractFileName(s);
        t := n.Substring(n.IndexOf('#')+1);
        n := n.Substring(0, n.IndexOf('#'));

        if (n = id) then
          if ((ver = '') or TSemanticVersion.matches(ver, t, a)) then
            if FileExists(FilePath([s, 'package', 'package.json'])) then
              exit(s);
      end;
    end;
  finally
    ts.free;
  end;
end;

procedure TFHIRPackageManager.progress(sender: TObject; pct: integer);
begin
  work(pct, false, FTaskDesc);
end;

function TFHIRPackageManager.latestPackageVersion(id: String): String;
var
  ts : TStringList;
  s, n : String;
begin
  result := '';
  ts := TStringList.create;
  try
    for s in TDirectory.GetDirectories(FFolder) do
      ts.add(s);
    ts.sort;
    for s in ts do
    begin
      n := ExtractFileName(s);
      if n.StartsWith(id+'#') then
        result := n.Substring(n.IndexOf('#')+1);
    end;
  finally
    ts.free;
  end;
end;

function TFHIRPackageManager.latestPublishedVersion(id: String): String;
var
  pck, t : TFHIRPackageInfo;
  list : TFslList<TFHIRPackageInfo>;
begin
  result := '';
  list := TFslList<TFHIRPackageInfo>.Create;
  try
    try
      TFHIRPackageClient.LoadPackages(list, PACKAGE_SERVER_PRIMARY, id);
    except
    end;
    try
      if list.Empty or StringArrayExists(['us.nlm.vsac'], id) then
        TFHIRPackageClient.LoadPackages(list, PACKAGE_SERVER_BACKUP, id);
    except
    end;
    pck := nil;
    for t in list do
      if (t.Id = id) and ((pck = nil) or isMoreRecentVersion(t.version, pck.version)) then
        pck := t;
    if pck <> nil then
      result := pck.version;
  finally
    list.free;
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
  b : TFslStringBuilder;
  list : TFslList<TNpmPackage>;
  p : TNpmPackage;
  ts : TStringList;
  s : String;
begin
  b := TFslStringBuilder.Create;
  try
    b.AppendLine('Packages in '+Folder);
    b.AppendLine;

    list := TFslList<TNpmPackage>.Create;
    try
      ListPackages(All_Package_Kinds, list);
      for p in list do
        p.report(b);
    finally
      list.free;
    end;

    b.AppendLine;
    b.AppendLine('URL Map:');
    ts := TStringList.Create;
    try
      FIni.ReadSection('urls', ts);
      for s in ts do
        b.AppendLine('  '+s+'='+FIni.ReadString('urls', s, ''));
    finally
      ts.free;
    end;
    result := b.ToString;
  finally
    b.free;
  end;
end;

procedure TFHIRPackageManager.work(pct: integer; done: boolean; desc: String);
begin
  if assigned(FOnWork) then
    FOnWork(self, pct, done, desc);
end;


function TFHIRPackageManager.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FFolder.length * sizeof(char)) + 12);
  inc(result, FCache.sizeInBytes(magic));
  inc(result, (FTaskDesc.length * sizeof(char)) + 12);
end;

{ TPackageLoadingInformation }

constructor TPackageLoadingInformation.Create(ver : string);
begin
  inherited Create;
  FLoaded := TStringList.Create;
  FVersion := TSemanticVersion.fromString(ver);
end;

destructor TPackageLoadingInformation.Destroy;
begin
  FLoaded.free;
  FVersion.free;
  inherited;
end;

procedure TPackageLoadingInformation.checkVersion(ver: String);
var
  sv : TSemanticVersion;
begin
  sv := TSemanticVersion.fromString(ver);
  try
    if not FVersion.matches(sv, semverMinor) then
      raise EFslException.Create('FHIR Loading Version mismatch: loading '+ver+' but already loaded '+FVersion.ToString);
  finally
    sv.free;
  end;
end;

function TPackageLoadingInformation.isLoaded(id, ver: String): boolean;
begin
  result := FLoaded.IndexOf(id+'#'+ver) > -1;
end;

procedure TPackageLoadingInformation.load(id, ver: String);
begin
  FLoaded.Add(id+'#'+ver);
end;

function TPackageLoadingInformation.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FVersion.sizeInBytes(magic));
  inc(result, FLoaded.sizeInBytes(magic));
end;

{ TPackageDefinition }

function TPackageDefinition.link: TPackageDefinition;
begin
  result := TPackageDefinition(inherited link);
end;

function TPackageDefinition.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FId.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FCanonical.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, (FFHIRVersion.length * sizeof(char)) + 12);
  inc(result, (FUrl.length * sizeof(char)) + 12);
end;

initialization
  GPackageLoaderTaskId := GBackgroundTasks.registerTaskEngine(TFHIRLoadPackagesTaskEngine.create);
end.

