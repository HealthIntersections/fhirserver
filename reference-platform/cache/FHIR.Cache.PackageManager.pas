unit FHIR.Cache.PackageManager;

interface

uses
  SysUtils, Classes, IniFiles, zlib, Generics.Collections,
  FHIR.Support.Objects, FHIR.Support.Generics, FHIR.Support.System, FHIR.Support.Json, FHIR.Support.Strings,
  FHIR.Support.Text, FHIR.Support.Tarball;

type
  TFHIRPackageObject = {abstract} class (TFslObject)
  public
    function summary : string; virtual;
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
    FUrl : String;
    FVersion : String;
    FDependencies : TFslList<TFHIRPackageDependencyInfo>;
    FFhirVersion: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TFHIRPackageVersionInfo; overload;
    function summary : String; override;
    function childCount : integer; override;

    property version : String read FVersion write FVersion;
    property fhirVersion : string read FFhirVersion write FFhirVersion;
    property url : String read FUrl write FUrl;
    property dependencies : TFslList<TFHIRPackageDependencyInfo> read FDependencies;
  end;

  TFHIRPackageInfo = class (TFHIRPackageObject)
  private
    FId : String;
    FCanonical : String;
    FVersions : TFslList<TFHIRPackageVersionInfo>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TFHIRPackageInfo; overload;
    function summary : String; override;
    function childCount : integer; override;

    property id : String read FId write FId;
    property Canonical : String read FCanonical write FCanonical;
    property versions : TFslList<TFHIRPackageVersionInfo> read FVersions;
  end;

  TCheckFunction = reference to function(msg : String):boolean;
  TPackageLoadingEvent = procedure (rType, id : String; stream : TStream) of object;

  TFHIRPackageManager = class (TFslObject)
  private
    FUser : boolean;
    FFolder : String;
    FIni : TIniFile;
    function resolve(list : TFslList<TFHIRPackageInfo>; dep : TFHIRPackageDependencyInfo) : TFHIRPackageDependencyStatus;
    function loadArchive(content : TBytes) : TDictionary<String, TBytes>;
    procedure clearCache;
  public
    constructor Create(user : boolean);
    destructor Destroy; override;

    property Folder : String read FFolder;
    function description : String;

    procedure ListPackages(list : TStringList); overload;
    procedure ListPackages(list : TFslList<TFHIRPackageInfo>); overload;
    procedure ListPackageIds(list : TStringList);
    procedure ListPackageVersions(id : String; list : TStringList);

    function packageExists(id, ver : String) : boolean;
    procedure loadPackage(id, ver : String; resources : Array of String; loadEvent : TPackageLoadingEvent); overload;
    procedure loadPackage(id, ver : String; resources : TFslStringSet; loadEvent : TPackageLoadingEvent); overload;

    procedure import(content : TBytes; callback : TCheckFunction); overload;

    procedure remove(id : String); overload;
    procedure remove(id, ver: String); overload;

    function getId(url : String) : String;
    function getUrl(id : String) : String;
  end;

implementation

uses
  IOUtils;

{ TFHIRPackageManager }

procedure TFHIRPackageManager.clearCache;
var
  s : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
    if s.Contains('-') then
      FolderDelete(s);
end;

constructor TFHIRPackageManager.Create(user : boolean);
begin
  inherited Create;
  FUser := user;
  if user then
    FFolder := path([UserFolder, '.fhir', 'packages'])
  else
    FFolder := path([ProgData, '.fhir', 'packages']);
  ForceFolder(FFolder);
  FIni := TIniFile.create(Path([FFolder, 'packages.ini']));
  if FIni.ReadString('cache', 'version', '0') <> '1' then
  begin
    clearCache;
    FIni.WriteString('cache', 'version', '1');
  end;
end;

function TFHIRPackageManager.description: String;
begin
  if FUser then
    result := 'User Cache'
  else
    result := 'System Cache';
end;

destructor TFHIRPackageManager.Destroy;
begin
  FIni.Free;
  Inherited;
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


procedure TFHIRPackageManager.import(content : TBytes; callback : TCheckFunction);
var
  npm : TJsonObject;
  id, ver, dir, fn : String;
  files : TDictionary<String, TBytes>;
  s: string;
begin
  files := loadArchive(content);
  try
    npm := TJSONParser.Parse(files['package\package.json']);
    try
      id := npm.str['name'];
      ver := npm.str['version'];
    finally
      npm.Free;
    end;
    if packageExists(id, ver) then
      if not callback('Replace existing copy of '+id+' version '+ver+'?') then
        exit;
    dir := path([FFolder, id+'-'+ver]);
    if FolderExists(dir) then
      if not FolderDelete(dir) then
        raise Exception.Create('Unable to delete existing package');
    ForceFolder(dir);
    for s in files.Keys do
    begin
      fn := path([dir, s]);
      ForceFolder(PathFolder(fn));
      BytesToFile(files[s], fn);
    end;
  finally
    files.Free;
  end;
end;
(*
  // we need to unzip all the files before we can read the package that tells us everything we need to know
  // we're going to do this to a temporary directory
    forceFolder(fn);


//  t, fn, d : String;

    archiveclass := GetArchiveFormats.FindDecompressFormat(FileName);
    if not Assigned(archiveclass) then
      Raise exception.Create('Not supported by 7z.dll');
    Myarchive := archiveclass.Create(FileName);
    try
      if (Myarchive is TJclSevenZipDecompressArchive) then
      Begin
        Myarchive.ListFiles; { Fails without doing this first }
       { ExtractAll (AutocreateSubDir) must be set true if arc has directories or it will crash }
        Myarchive.ExtractAll(fn, True);
      End;
    Finally
      MyArchive.Free;
    End;
    // now we've unhzipped the file, and just have a tarball.
    FileName := Path([t, 'gunzip', PathTitle(fileName)]);

    FileDelete(filename);


  finally
    FolderDelete(t);
  end;
end;
*)

procedure TFHIRPackageManager.ListPackageIds(list: TStringList);
var
  s, id : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
  begin
    id := s.subString(s.lastIndexOf('\')+1);
    if id.Contains('-') then
    begin
      id := id.Substring(0, id.IndexOf('-'));
      if list.IndexOf(id) = -1 then
        list.Add(id);
    end;
  end;
end;

procedure TFHIRPackageManager.ListPackages(list: TFslList<TFHIRPackageInfo>);
var
  s : String;
  id, n : String;
  npm, dep : TJsonObject;
  t, pck : TFHIRPackageInfo;
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
      if s.Contains('-') and FileExists(Path([s, 'package', 'package.json'])) then
      begin
        npm := TJSONParser.ParseFile(Path([s, 'package', 'package.json']));
        try
          id := npm.str['name'];
          pck := nil;
          try
            for t in list do
              if t.id = id then
                pck := t.Link;
            if pck = nil then
            begin
              pck := TFHIRPackageInfo.create;
              pck.id := id;
              if (id = 'hl7.fhir.core') then
                list.Insert(0, pck.link)
              else
                list.add(pck.link);
            end;
            if pck.Canonical = '' then
              pck.Canonical := npm.str['canonical'];
            if pck.Canonical = '' then
              pck.Canonical := getUrl(pck.id);
            v := TFHIRPackageVersionInfo.Create;
            try
              v.Version := npm.str['version'];
              v.url := npm.str['url'];
              dep := npm.obj['dependencies'];
              for n in dep.properties.Keys do
              begin
                if n = 'hl7.fhir.core' then
                  v.fhirVersion := dep.str[n]
                else
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
              pck.versions.Add(v.link);
            finally
              v.Free;
            end;
          finally
            pck.Free;
          end;
        finally
          npm.free;
        end;
      end;
    for pck in list do
      for v in pck.versions do
        for d in v.dependencies do
          d.status := resolve(list, d);
  finally
    ts.Free;
  end;
end;

procedure TFHIRPackageManager.ListPackages(list: TStringList);
var
  s : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
    if s.Contains('-') then
      list.Add(s);
end;

procedure TFHIRPackageManager.ListPackageVersions(id: String; list: TStringList);
var
  s : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
    if (s.StartsWith(id+'-')) then
      list.Add(s.Substring(id.Length+1));
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
  result := TDictionary<String, TBytes>.create;
  bo := TBytesStream.Create(content);
  try
    z := TZDecompressionStream.Create(bo, 15+16);
    try
      tar := TTarArchive.Create(z);
      try
        tar.Reset;
        while tar.FindNext(DirRec) do
        begin
          fn := DirRec.Name;
          fn := fn.replace('/', '\');
          bi := TBytesStream.Create;
          try
            tar.ReadFile(bi);
            b := bi.Bytes;
            result.Add(fn, copy(b, 0, bi.Size));
          finally
            bi.free;
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
end;

procedure TFHIRPackageManager.loadPackage(id, ver: String; resources: Array of String; loadEvent: TPackageLoadingEvent);
var
  fsl : TFslStringSet;
begin
  fsl := TFslStringSet.Create(resources);
  try
    loadPackage(id, ver, fsl, loadEvent);
  finally
    fsl.Free;
  end;
end;

procedure TFHIRPackageManager.loadPackage(id, ver: String; resources : TFslStringSet; loadEvent: TPackageLoadingEvent);
var
  s, t, i : String;
  f : TFileStream;
begin
  if not packageExists(id, ver) then
    raise Exception.Create('Unable to load package '+id+' v '+ver+' as it doesn''t exist');
  for s in TDirectory.GetFiles(Path([FFolder, id+'-'+ver, 'package'])) do
    if not s.endsWith('package.json') then
    begin
      t := PathTitle(s);
      i := t.substring(t.IndexOf('-')+1);
      t := t.substring(0, t.IndexOf('-'));
      if resources.contains(t) then
      begin
        f := TFileStream.Create(s, fmOpenRead + fmShareDenyWrite);
        try
          loadEvent(t, i, f);
        finally
          f.Free;
        end;
      end;
    end;
end;

function TFHIRPackageManager.packageExists(id, ver: String): boolean;
begin
  result := FolderExists(Path([FFolder, id+'-'+ver])) and FileExists(Path([FFolder, id+'-'+ver, 'package', 'package.json']));
end;

procedure TFHIRPackageManager.remove(id : String);
var
  s, n : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
  begin
    n := s.Substring(s.LastIndexOf('\')+1);
    if (n.StartsWith(id+'-')) then
      if not FolderDelete(s) then
        raise Exception.Create('Unable to delete package '+n+'. Perhaps it is in use?');
  end;
end;

procedure TFHIRPackageManager.remove(id, ver : String);
var
  s, n : String;
begin
  for s in TDirectory.GetDirectories(FFolder) do
  begin
    n := s.Substring(s.LastIndexOf('\')+1);
    if (n = id+'-'+ver) then
      if not FolderDelete(s) then
        raise Exception.Create('Unable to delete package '+n+'. Perhaps it is in use?');
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
  inherited;
  FDependencies := TFslList<TFHIRPackageDependencyInfo>.create;
end;

destructor TFHIRPackageVersionInfo.Destroy;
begin
  FDependencies.Free;
  inherited;
end;

function TFHIRPackageVersionInfo.Link: TFHIRPackageVersionInfo;
begin
  result := TFHIRPackageVersionInfo(inherited link);
end;


function TFHIRPackageVersionInfo.summary: String;
begin
  if FVersion = 'current' then
    result := '(current) [FHIR v'+FFhirVersion+']'
  else if FVersion = 'dev' then
    result := '(dev) [FHIR v'+FFhirVersion+']'
  else
    result := 'v'+FVersion+' [FHIR v'+FFhirVersion+']';
end;

{ TFHIRPackageDependencyInfo }

function TFHIRPackageDependencyInfo.Link: TFHIRPackageDependencyInfo;
begin
  result := TFHIRPackageDependencyInfo(inherited link);
end;

function TFHIRPackageDependencyInfo.summary: String;
begin
  result := 'Depends on '+Fid+'-'+Fversion;
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

function TFHIRPackageObject.summary: string;
begin
  result := '?';
end;

end.
