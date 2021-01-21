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
  SysUtils, Classes, IniFiles, zlib, Generics.Collections, Types, {$IFDEF DELPHI} IOUtils, {$ENDIF}
  fsl_base,  fsl_utilities, fsl_json, fsl_fpc, fsl_threads,
  fsl_stream, fsl_fetcher,
  fsl_npm, fsl_npm_client;

type
  TCheckEvent = function(sender : TObject; msg : String):boolean of object;
  TPackageLoadingEvent = procedure (rType, id : String; stream : TStream) of object;

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
    function sizeInBytesV : cardinal; override;
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
    FVersion: String;
    FLoaded: TStringList;
    FOnLoadEvent: TPackageLoadingEvent;
  protected
    function sizeInBytesV : cardinal; override;
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
    FCache : TFslMap<TNpmPackage>;
    FTaskDesc : String;
    function loadArchive(content : TBytes) : TDictionary<String, TBytes>;
    procedure clearCache;
    function checkPackageSize(dir : String) : integer;
    procedure work(pct : integer; done : boolean; desc : String);
    procedure progress(sender : TObject; pct : integer);
    function check(desc : String) : boolean;
    function loadPackageFromCache(folder : String) : TNpmPackage;
    procedure buildPackageIndex(folder : String);
    function latestPackageVersion(id: String): String;
    function isIgnored(s : String): boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(user : boolean);
    destructor Destroy; override;
    function Link : TFHIRPackageManager;

    property OnWork : TWorkProgressEvent read FOnWork write FOnWork;
    property OnCheck : TCheckEvent read FOnCheck write FOnCheck;

    property Folder : String read FFolder;
    property UserMode : boolean read FUser;
    function description : String;

    procedure listAllKnownPackages(list: TFslList<TFHIRPackageInfo>; ver : String);
    procedure ListPackageIds(list : TStrings);
    procedure ListPackages(list : TStrings); overload;
    procedure ListPackages(kinds : TFHIRPackageKindSet; list : TFslList<TNpmPackage>); overload;
    procedure ListAllPackages(list : TFslList<TNpmPackage>); overload;

    function packageExists(id, ver : String) : boolean; overload;
    function autoInstallPackage(id, ver : String) : boolean; overload;

    function loadPackage(id : String) : TNpmPackage; overload;
    function loadPackage(id, ver : String) : TNpmPackage; overload;
    procedure loadPackage(id, ver : String; resources : Array of String; loadInfo : TPackageLoadingInformation); overload;
    procedure loadPackage(idver : String; resources : Array of String; loadInfo : TPackageLoadingInformation); overload;
    procedure loadPackage(id, ver : String; resources : TFslStringSet; loadInfo : TPackageLoadingInformation); overload;

    procedure clear;

    function import(content : TBytes) : TNpmPackage; overload;
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
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(manager : TFHIRPackageManager);
    destructor Destroy; override;

    property Manager : TFHIRPackageManager read FManager;
  end;

  { TFHIRLoadPackagesTaskResponse }

  TFHIRLoadPackagesTaskResponse = class (TBackgroundTaskResponsePackage)
  private
    FPackages : TFslList<TNpmPackage>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Packages :  TFslList<TNpmPackage> read FPackages;
  end;

  { TFHIRLoadPackagesTaskEngine }

  TFHIRLoadPackagesTaskEngine  = class abstract (TBackgroundTaskEngine)
  private
    procedure doWork(sender : TObject; pct : integer; done : boolean; desc : String);
  public
    function name : String; override;
    procedure execute(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage); override;
  end;

const
//  NAMES_TFHIRPackageDependencyStatus : Array [TFHIRPackageDependencyStatus] of String = ('?', 'ok', 'ver?', 'bad');
  ANALYSIS_VERSION = 2;
  CACHE_VERSION = 3;


var
  MustBeUserMode : boolean = false;
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
  FPackages := TFslList<TNpmPackage>.create;
end;

destructor TFHIRLoadPackagesTaskResponse.Destroy;
begin
  FPackages.Free;
  inherited Destroy;
end;

function TFHIRLoadPackagesTaskResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPackages.sizeInBytes);
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

function TFHIRLoadPackagesTaskRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FManager.sizeInBytes);
end;

{ TFHIRPackageManager }

constructor TFHIRPackageManager.Create(user : boolean);
begin
  inherited Create;
  FCache := TFslMap<TNpmPackage>.create('Npm Package manager');
  FUser := user or MustBeUserMode;
  {$IFDEF WINDOWS}
  if FUser then
    FFolder := path([UserFolder, '.fhir', 'packages'])
  else
    FFolder := path([ProgData, '.fhir', 'packages']);
  {$ELSE}
  if FUser then
    FFolder := ExpandFileNameUTF8('~/.fhir/packages')
  else
    FFolder := '/var/lib/.fhir/packages';
  {$ENDIF}
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
      if TSemVer.matches(npm.fhirVersion, ver) then
      begin
        found := false;
        for t in list do
          if npm.name = t.id then
            found := true;
        if not found then
          list.Add(TFHIRPackageInfo.Create(npm.name, npm.version, npm.fhirVersion, npm.description, npm.canonical, npm.url));
      end;
    finally
      npm.Free;
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


function TFHIRPackageManager.import(content : TBytes) : TNpmPackage;
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
        exit(nil);
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
      result := TNpmPackage.fromFolderQuick(dir);
      result.size := size;
      result.installed := now;
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
      if s.Contains('#') and FileExists(Path([s, 'package', 'package.json'])) then
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
    ts.Free;
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
      if s.Contains('#') and FileExists(Path([s, 'package', 'package.json'])) then
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
    ts.Free;
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

procedure TFHIRPackageManager.ListPackages(list: TStrings);
var
  s : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
    if s.Contains('#') then
      list.Add(s);
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
    result := loadPackageFromCache(Path([FFolder, id+'#'+ver]));
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
    result := loadPackageFromCache(Path([FFolder, id+'#'+ver]));
  end;
end;

procedure TFHIRPackageManager.loadPackage(id, ver: String; resources : TFslStringSet; loadInfo : TPackageLoadingInformation);
var
  c : integer;
  s : String;
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
  if (ver = '') then
    ver := latestPackageVersion(id);

  work(0, false, 'Scanning Package');

  npm := loadPackageFromCache(Path([FFolder, id+'#'+ver]));
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
        f := TFileStream.Create(path([FFolder, id+'#'+ver, 'package', fi.Name]), fmOpenRead + fmShareDenyWrite);
        try
          try
            loadInfo.OnLoadEvent(fi.ResourceType, fi.id, f);
          except
            on e : Exception do
            begin
              raise EFslException.Create('Error Parsing '+fi.Name+' in package '+id+'#'+ver+': '+e.message);
            end;

          end;
        finally
          f.Free;
        end;
      end;
      work(trunc((c / npm.Folders['package'].resources.count) * 100), false, 'Package '+id+'#'+ver+': Load '+fi.Name);
      inc(c);
    end;
    work(100, true, 'Complete');
  finally
    npm.Free;
  end;
end;


function TFHIRPackageManager.loadPackageFromCache(folder: String): TNpmPackage;
begin
  if FCache.TryGetValue(folder, result) then
    result.Link
  else
  begin
    if not FileExists(Path([folder, 'package', '.index.json'])) then
      buildPackageIndex(folder);
    result := TNpmPackage.fromFolder(folder);
    FCache.add(folder, result.Link);
  end;
end;

function TFHIRPackageManager.autoInstallPackage(id, ver: String): boolean;
var
  list : TFslList<TFHIRPackageInfo>;
  t, pd : TFHIRPackageInfo;
begin
  result := packageExists(id, ver);
  if (not result) then
  begin
    list := TFslList<TFHIRPackageInfo>.create;
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
        if (t.Id = id) and ((ver = '') or (t.Version = ver)) and ((pd = nil) or isMoreRecentVersion(t.version, pd.version)) then
          pd := t;
      if (pd <> nil) then
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
  s, t : String;
begin
  if ver = '' then
  begin
    result := false;
    for s in TDirectory.GetDirectories(FFolder) do
    begin
        t := ExtractFileName(s);
        if t.StartsWith(id+'#') then
          exit(true);
    end;
  end
  else if ver.CountChar('.') = 1 then
  begin
    result := false;
    for s in TDirectory.GetDirectories(FFolder) do
    begin
      t := ExtractFileName(s);
      if t.StartsWith(id+'#'+ver) then
        exit(true);
    end;
  end
  else
  begin
    result := FolderExists(Path([FFolder, id+'#'+ver]));
    if result then
      result := FileExists(Path([FFolder, id+'#'+ver, 'package', 'package.json']));
  end;
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
      result := n.Substring(n.IndexOf('#')+1);
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
  list : TFslList<TNpmPackage>;
  p : TNpmPackage;
  ts : TStringList;
  s : String;
begin
  b := TStringBuilder.Create;
  try
    b.AppendLine('Packages in '+Folder);
    b.AppendLine;

    list := TFslList<TNpmPackage>.create;
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

procedure TFHIRPackageManager.work(pct: integer; done: boolean; desc: String);
begin
  if assigned(FOnWork) then
    FOnWork(self, pct, done, desc);
end;


function TFHIRPackageManager.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFolder.length * sizeof(char)) + 12);
  inc(result, FCache.sizeInBytes);
  inc(result, (FTaskDesc.length * sizeof(char)) + 12);
end;

{ TPackageLoadingInformation }

constructor TPackageLoadingInformation.Create(ver : string);
begin
  inherited Create;
  FLoaded := TStringList.Create;
  FVersion := TSemVer.getMajMin(ver);
end;

destructor TPackageLoadingInformation.Destroy;
begin
  FLoaded.Free;
  inherited;
end;

procedure TPackageLoadingInformation.checkVersion(ver: String);
begin
  ver := TSemVer.getMajMin(ver);
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

function TPackageLoadingInformation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, FLoaded.sizeInBytes);
end;

{ TPackageDefinition }

function TPackageDefinition.link: TPackageDefinition;
begin
  result := TPackageDefinition(inherited link);
end;

function TPackageDefinition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
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

