unit ftk_search;

{$i fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_threads, fsl_stream, fsl_fpc,
  ftk_context;

const
  FRAGMENT_UNDERHANG = 15;
  FRAGMENT_UNDERHANG_MIN = 7;

  FRAGMENT_OVERHANG = 15;
  FRAGMENT_OVERHANG_MIN = 7;

  KNOWN_EXTENSIONS : Array of String = ['.xml', '.json', '.cda', '.txt', '.js', '.map', '.md', '.html', '.hl7', '.msg', '.htm', '.ini', '.liquid', '.template'];

type
   TToolkitSearchKind = (tskCurrent, tskAllOpen, tskProject, tskFolder, tskFolderTree);

   { TToolkitSearchSource }

   TToolkitSearchSource  = class (TFslObject)
   private
     FAddress: String;
     FContent: String;
     FName: String;
     FLoaded : boolean;
   public
     constructor Create(name, address, content : String); overload;
     constructor Create(name, address : String); overload;
     function link : TToolkitSearchSource; overload;

     property loaded : boolean read FLoaded write FLoaded;
     property name : String read FName;
     property address : String read FAddress;
     property content : String read FContent;
   end;

   { TToolkitSearchSpecification }

   TToolkitSearchSpecification = class (TFslObject)
   private
     FCaseSensitive: boolean;
     FKind: TToolkitSearchKind;
     FScope: String;
     FText: String;
     FWholeWords: boolean;
     FSources : TFslList<TToolkitSearchSource>;
   public
     constructor Create; overload;
     destructor Destroy; overload;
     function link : TToolkitSearchSpecification; overload;

     property text : String read FText write FText;
     property caseSensitive : boolean read FCaseSensitive write FCaseSensitive;
     property wholeWords : boolean read FWholeWords write FWholeWords;
     property kind : TToolkitSearchKind read FKind write FKind;
     property scope : String read FScope write FScope;
     property sources : TFslList<TToolkitSearchSource> read FSources;
   end;

   { TToolkitSearchMatch }

   TToolkitSearchMatch = class (TFslObject)
   private
     FAddress: String;
     FFragment: String;
     FLocation: TSourceLocation;
     FName: String;
   public
     constructor Create(name, address : String; location : TSourceLocation; fragment : String);
     function link : TToolkitSearchMatch; overload;

     property name : String read FName write FName;
     property address : String read FAddress write FAddress;
     property location : TSourceLocation read FLocation write FLocation;
     property fragment : String read FFragment write FFragment;
   end;

   { TToolkitSearchEngine }

   TToolkitSearchEngine = class (TFslObject)
   private
     FContext: TToolkitContext;
     FOnWork: TWorkProgressEvent;
     FResults: TFslList<TToolkitSearchMatch>;
     FSpec: TToolkitSearchSpecification;
     procedure SetContext(AValue: TToolkitContext);
     procedure SetResults(AValue: TFslList<TToolkitSearchMatch>);
     procedure SetSpec(AValue: TToolkitSearchSpecification);

     function isWord(line : String; start, stop : integer) : boolean;
     function fragment(line : String; start, stop : integer) : String;

     procedure searchLine(src : TToolkitSearchSource; linenum : integer; line : String);
     procedure searchSource(src : TToolkitSearchSource);
     function loadFile(src : String) : String;
     procedure listFiles(root, path : String; recurse : boolean);

     procedure goSources;
     procedure goProject;
     procedure goFolder(contained : boolean);
   public
     constructor Create; override;
     destructor Destroy; override;

     property spec : TToolkitSearchSpecification read FSpec write SetSpec;
     property results : TFslList<TToolkitSearchMatch> read FResults write SetResults;
     property context : TToolkitContext read FContext write SetContext;

     procedure go;
     property OnWork : TWorkProgressEvent read FOnWork write FOnWork;
   end;

   { TToolkitSearchTaskRequest }

   TToolkitSearchTaskRequest = class (TBackgroundTaskRequestPackage)
   private
     FContext: TToolkitContext;
     FSpec: TToolkitSearchSpecification;
   public
     constructor Create(spec : TToolkitSearchSpecification; context : TToolkitContext);
     destructor Destroy; override;

     property spec : TToolkitSearchSpecification read FSpec;
     property context : TToolkitContext read FContext;
   end;


   { TToolkitSearchTaskResponse }

   TToolkitSearchTaskResponse = class (TBackgroundTaskResponsePackage)
   private
     FResults : TFslList<TToolkitSearchMatch>;
   public
     constructor Create;
     destructor Destroy; override;

     property results :  TFslList<TToolkitSearchMatch> read FResults;
   end;

   { TToolkitSearchTaskEngine }

   TToolkitSearchTaskEngine  = class abstract (TBackgroundTaskEngine)
   private
     procedure doWork(sender : TObject; pct : integer; done : boolean; desc : String);
   public
     function name : String; override;
     procedure execute(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage); override;
   end;

implementation

{ TToolkitSearchSource }

constructor TToolkitSearchSource.Create(name, address, content: String);
begin
  inherited create;
  FName := name;
  FAddress := address;
  FContent := content;
  FLoaded := true;
end;

constructor TToolkitSearchSource.Create(name, address: String);
begin
  inherited create;
  FName := name;
  FAddress := address;
  FLoaded := false;
end;

function TToolkitSearchSource.link: TToolkitSearchSource;
begin
  result := TToolkitSearchSource(inherited link);
end;

{ TToolkitSearchMatch }

constructor TToolkitSearchMatch.Create(name, address : String; location: TSourceLocation; fragment: String);
begin
  inherited Create;
  FName := name;
  FAddress := address;
  FLocation := location;
  FFragment := fragment;
end;

function TToolkitSearchMatch.link: TToolkitSearchMatch;
begin
  result := TToolkitSearchMatch(inherited link);
end;

{ TToolkitSearchSpecification }

constructor TToolkitSearchSpecification.Create;
begin
  inherited Create;
  FSources := TFslList<TToolkitSearchSource>.create;
end;

destructor TToolkitSearchSpecification.Destroy;
begin
  FSources.free;
  inherited;
end;

function TToolkitSearchSpecification.link: TToolkitSearchSpecification;
begin
  result := TToolkitSearchSpecification(inherited link);
end;

{ TToolkitSearchTaskEngine }

procedure TToolkitSearchTaskEngine.doWork(sender: TObject; pct: integer; done: boolean; desc: String);
begin
  progress(desc, pct);
end;

function TToolkitSearchTaskEngine.name: String;
begin
  result := 'Search';
end;

procedure TToolkitSearchTaskEngine.execute(request: TBackgroundTaskRequestPackage; response: TBackgroundTaskResponsePackage);
var
  engine : TToolkitSearchEngine;
begin
  engine := TToolkitSearchEngine.create;
  try
    engine.spec := (request as TToolkitSearchTaskRequest).spec.link;
    engine.context := (request as TToolkitSearchTaskRequest).context.link;
    engine.results := (response as TToolkitSearchTaskResponse).results.link;
    engine.OnWork := doWork;
    engine.go;
  finally
    engine.free;
  end;
end;

{ TToolkitSearchTaskRequest }

constructor TToolkitSearchTaskRequest.Create(spec: TToolkitSearchSpecification; context: TToolkitContext);
begin
  Inherited Create;
  FSpec := spec;
  FContext := context;
end;

destructor TToolkitSearchTaskRequest.Destroy;
begin
  FSpec.Free;
  FContext.free;
  inherited Destroy;
end;

{ TToolkitSearchTaskResponse }

constructor TToolkitSearchTaskResponse.Create;
begin
  inherited Create;
  FResults := TFslList<TToolkitSearchMatch>.create;
end;

destructor TToolkitSearchTaskResponse.Destroy;
begin
  FResults.Free;
  inherited Destroy;
end;


{ TToolkitSearchEngine }

constructor TToolkitSearchEngine.Create;
begin
  inherited Create;
end;

destructor TToolkitSearchEngine.Destroy;
begin
  FResults.Free;
  FContext.free;
  inherited Destroy;
end;

procedure TToolkitSearchEngine.SetContext(AValue: TToolkitContext);
begin
  FContext.free;
  FContext := AValue;
end;

procedure TToolkitSearchEngine.SetResults(AValue: TFslList<TToolkitSearchMatch>);
begin
  FResults.Free;
  FResults := AValue;
end;

procedure TToolkitSearchEngine.SetSpec(AValue: TToolkitSearchSpecification);
begin
  FSpec.Free;
  FSpec := AValue;
end;

var
  delims : set of char = [' ', ',', '.', ';', ':', '(', ')', '<', '>', #9];

function TToolkitSearchEngine.isWord(line: String; start, stop: integer): boolean;
begin
  result := (((start >= 1)    and (line[start] in delims)) or (start < 1)) and
     (((stop <= length(line)) and (line[stop] in delims))  or (stop > length(line)));
end;

function TToolkitSearchEngine.fragment(line: String; start, stop: integer): String;
var
  b, e : integer;
begin
  if start <= FRAGMENT_UNDERHANG then
    b := 1
  else
  begin
    b := start - FRAGMENT_UNDERHANG;
    while b < start - FRAGMENT_UNDERHANG_MIN do
      if line[b] in delims then
        break
      else
        inc(b);
  end;

  if length(line)-stop  <= FRAGMENT_OVERHANG then
    e := length(line)
  else
  begin
    e := stop + FRAGMENT_OVERHANG;
    while e > stop + FRAGMENT_OVERHANG_MIN do
      if line[e] in delims then
        break
      else
        dec(e);
  end;
  result := copy(line, b, e-b);
end;

procedure TToolkitSearchEngine.searchLine(src : TToolkitSearchSource; linenum : integer; line : String);
var
  cursor, match : integer;
begin
  if not spec.caseSensitive then
    line := line.toLower;
  cursor := 1;
  repeat
    match := Pos(spec.text, line, cursor);
    if match > 0 then
    begin
      if not spec.wholeWords or isWord(line, match-1, match+spec.text.length) then
        results.add(TToolkitSearchMatch.create(src.name, src.address, TSourceLocation.Create(linenum, match), fragment(line, match, spec.text.length)));
    end;
    cursor := match + length(spec.text);
  until match = 0;
end;

procedure TToolkitSearchEngine.searchSource(src : TToolkitSearchSource);
var
  ts : TStringList;
  line : String;
  i : integer;
begin
  ts := TStringList.create;
  try
    if src.loaded then
      ts.text := src.content
    else
      ts.text := loadFile(src.address);
    i := 0;
    for line in ts do
    begin
      searchLine(src, i, line);
      inc(i);
    end;
  finally
    ts.free;
  end;
end;

function TToolkitSearchEngine.loadFile(src: String): String;
var
  bytes : TBytes;
begin
  try
    bytes := FileToBytes(src.Substring(5));
  except
    exit('');
  end;

  try
    result := TEncoding.UTF8.GetString(bytes);
    if pos(#0, result) = 0 then
      exit;
  except
  end;
  try
    result := TEncoding.Unicode.GetString(bytes);
    if pos(#0, result) = 0 then
      exit;
  except
  end;
  try
    result := TEncoding.BigEndianUnicode.GetString(bytes);
    if pos(#0, result) = 0 then
      exit;
  except
  end;
  result := TEncoding.ASCII.GetString(bytes);
end;

procedure TToolkitSearchEngine.listFiles(root, path: String; recurse : boolean);
var
  f, e : String;
begin
  for f in TDirectory.GetFiles(Path) do
  begin
    e := ExtractFileExt(f);
    if StringArrayExistsInsensitive(KNOWN_EXTENSIONS, e) then
      spec.sources.Add(TToolkitSearchSource.create(f.Substring(root.Length), 'file:'+f));
  end;

  for f in TDirectory.getDirectories(path) do
    listFiles(root, f, true);
end;

procedure TToolkitSearchEngine.goSources;
var
  src : TToolkitSearchSource;
  i : integer;
begin
  i := 0;
  for src in spec.sources do
  begin
    if Assigned(OnWork) then
      FOnWork(self, trunc(i * 100 / spec.sources.count), false, src.name);
    inc(i);
    searchSource(src);
  end;
end;

procedure TToolkitSearchEngine.goProject;
begin
  raise Exception.create('not supported yet');
end;

procedure TToolkitSearchEngine.goFolder(contained: boolean);
begin
  listFiles(IncludeTrailingPathDelimiter(Spec.scope), Spec.scope, contained);
  goSources;
end;

procedure TToolkitSearchEngine.go;
begin
  if not spec.caseSensitive then
    spec.text := spec.text.ToLower;

  case spec.kind of
    tskCurrent : goSources;
    tskAllOpen : goSources;
    tskProject : goProject;
    tskFolder : goFolder(false);
    tskFolderTree : goFolder(true);
  end;
end;

end.
