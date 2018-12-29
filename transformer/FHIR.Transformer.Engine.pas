unit FHIR.Transformer.Engine;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Lang,
  FHIR.R4.Context, FHIR.R4.Factory, FHIR.R4.MapUtilities, FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.ElementModel, FHIR.R4.Profiles,
  FHIR.Transformer.Workspace, FHIR.Transformer.Context;

type
  TConversionEngine = class;

  TCheckBreakpointEvent = function (line : integer) : boolean of Object;
  TConversionEngineFetchSourceEvent = function (sender : TConversionEngine; f : TWorkspaceFile) : TStream of object;
  TConversionEngineStatusEvent = procedure (sender : TConversionEngine; message : String) of object;
  TConversionEngineLogEvent = procedure (sender : TConversionEngine; message : String) of object;
  TConversionEngineCompiledEvent = procedure (sender : TConversionEngine; f : TWorkspaceFile; checkBreakpointProc : TCheckBreakpointEvent) of object;

  TLocalTransformerServices = class (TTransformerServices)
  private
    FOutcomes: TFslList<TFhirResource>;
    FOnLog: TConversionEngineLogEvent;
    FContext : TFHIRTransformerContext;
    FFactory : TFHIRFactoryR4;
  public
    constructor Create(log : TConversionEngineLogEvent; context : TFHIRTransformerContext; factory : TFHIRFactoryR4);
    destructor Destroy; override;
    function link : TLocalTransformerServices; overload;
    property outcomes : TFslList<TFhirResource> read FOutcomes;

    function translate(appInfo : TFslObject; src : TFHIRCoding; conceptMapUrl : String) : TFHIRCoding; override;
    procedure log(s : String); override;
    function performSearch(appInfo : TFslObject; url : String) : TFslList<TFHIRObject>; override;
    function createType(appInfo : TFslObject; tn : String) : TFHIRObject; override;
    procedure createResource(appInfo : TFslObject; res : TFHIRObject; atRootofTransform : boolean); override;
  end;

  TMapBreakpointResolver = class (TFslObject)
  private
    FMap : TFhirStructureMap;
    function checkRule(rule : TFhirStructureMapGroupRule; line : integer) : boolean;
  public
    constructor Create(map : TFhirStructureMap);
    destructor Destroy; override;
    function checkBreakpoint(line : integer) : boolean;
  end;

  TConversionEngine = class abstract (TFslObject)
  private
    FSource: TWorkspaceFile;
    FOnWantSource: TConversionEngineFetchSourceEvent;
    FOnStatus: TConversionEngineStatusEvent;
    FOnLog: TConversionEngineLogEvent;
    FOnCompiled: TConversionEngineCompiledEvent;
    FWorkspace: TWorkspace;
    FMap: TWorkspaceFile;
    FCache: TResourceMemoryCache;
    FOnTransformDebug: TFHIRStructureMapDebugEvent;
    FOutcomes: TFslList<TFhirObject>;
    procedure SetSource(const Value: TWorkspaceFile);
    procedure SetWorkspace(const Value: TWorkspace);
    procedure SetMap(const Value: TWorkspaceFile);
    procedure SetCache(const Value: TResourceMemoryCache);
    procedure relog(sender : TConversionEngine; message : String);
  protected
    FMapUtils : TFHIRStructureMapUtilities;
    FContext : TFHIRTransformerContext;
    FFactory : TFHIRFactoryR4;
    procedure log(msg : String);
    function fetchSource(f : TWorkspaceFile) : TStream;
    function parseMap(f : TWorkspaceFile) : TFhirStructureMap;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TConversionEngine; overload;

    property workspace : TWorkspace read FWorkspace write SetWorkspace;
    property source : TWorkspaceFile read FSource write SetSource;
    property map : TWorkspaceFile read FMap write SetMap;
    property cache : TResourceMemoryCache read FCache write SetCache;
    property OnWantSource : TConversionEngineFetchSourceEvent read FOnWantSource write FOnWantSource;
    property OnStatus : TConversionEngineStatusEvent read FOnStatus write FOnStatus;
    property OnLog : TConversionEngineLogEvent read FOnLog write FOnLog;
    property OnTransformDebug : TFHIRStructureMapDebugEvent read FOnTransformDebug write FOnTransformDebug;
    property OnCompiled: TConversionEngineCompiledEvent read FOnCompiled write FOnCompiled;

    property Context : TFHIRTransformerContext read FContext;
    property Outcomes: TFslList<TFhirObject> read FOutcomes;

    procedure load; virtual; abstract;
    procedure execute; virtual; abstract;
  end;

  TCDAConversionEngine = class (TConversionEngine)
  private
  public
    function link : TCDAConversionEngine; overload;

    procedure load; override;
    procedure execute; override;
  end;

implementation

{ TConversionEngine }

constructor TConversionEngine.Create;
begin
  inherited;
  FOutcomes := TFslList<TFhirObject>.create;
end;

destructor TConversionEngine.Destroy;
begin
  FOutcomes.Free;
  FMapUtils.Free;
  FCache.Free;
  FWorkspace.Free;
  FMap.Free;
  FSource.Free;
  FContext.Free;
  FFactory.Free;
  inherited;
end;

function TConversionEngine.fetchSource(f: TWorkspaceFile): TStream;
begin
  if Assigned(FOnWantSource) then
    result := FOnWantSource(self, f);
  if result = nil then
    result := TFileStream.Create(makeAbsolutePath(f.filename, FWorkspace.folder), fmOpenRead + fmShareDenyWrite);
end;

function TConversionEngine.link: TConversionEngine;
begin
  result := TConversionEngine(inherited Link);
end;

procedure TConversionEngine.log(msg: String);
begin
  if assigned(OnLog) then
    OnLog(self, msg);
end;

function TConversionEngine.parseMap(f: TWorkspaceFile): TFhirStructureMap;
var
  stream : TStream;
begin
  stream := fetchSource(f);
  try
    result := FMapUtils.parse(StreamToString(stream, TEncoding.UTF8), f.title);
  finally
    stream.free;
  end;
end;

procedure TConversionEngine.relog(sender: TConversionEngine; message: String);
begin
  if assigned(FOnLog) then
    FOnLog(self, message);
end;

procedure TConversionEngine.SetCache(const Value: TResourceMemoryCache);
begin
  FCache.Free;
  FCache := Value;
end;

procedure TConversionEngine.SetMap(const Value: TWorkspaceFile);
begin
  FMap.Free;
  FMap := Value;
end;

procedure TConversionEngine.SetSource(const Value: TWorkspaceFile);
begin
  FSource.Free;
  FSource := Value;
end;

procedure TConversionEngine.SetWorkspace(const Value: TWorkspace);
begin
  FWorkspace.Free;
  FWorkspace := Value;
end;

{ TCDAConversionEngine }

procedure TCDAConversionEngine.execute;
var
  f : TWorkspaceFile;
  stream : TStream;
  map : TFHIRStructureMap;
  elem : TFHIRMMElement;
  services : TLocalTransformerServices;
  mbpr : TMapBreakpointResolver;
  res : TFhirResource;
begin
  log('Parse the Workspace Maps');
  for f in FWorkspace.maps do
  begin
    map := parseMap(f);
    try
      f.parsed := map;
      FMapUtils.Lib.Add(map.url, map.Link);
      if assigned(FOnCompiled) then
      begin
        mbpr := TMapbreakpointResolver.create(map.Link);
        try
          FOnCompiled(self, f, mbpr.checkBreakPoint);
        finally
          mbpr.free;
        end;
      end;
    finally
      map.free;
    end;
  end;
  try
    log('Load the CDA Source');
    stream := fetchSource(FSource);
    try
      elem := TFHIRMMManager.parse(FContext, stream, ffXml);
      try
        log('Parse the Map');
        if self.map.parsed <> nil then
          map := (self.map.parsed as TFhirStructureMap).link
        else
          map := parseMap(self.map);
        try
          log('Execute the Conversion [url]');
          services := TLocalTransformerServices.Create(relog, FContext, FFactory.link);
          try
            FMapUtils.Services := Services.Link;
            FMapUtils.OnDebug := FOnTransformDebug;
            FMapUtils.transform(nil, elem, map, nil);
            assert(services.outcomes.Count = 1);
            for res in services.outcomes do
              FOutcomes.Add(res.Link);
          finally
            services.Free;
          end;
        finally
          map.Free;
        end;
      finally
        elem.Free;
      end;
    finally
      stream.free;
    end;
  finally
    FWorkspace.ClearParsedObjects;
  end;
end;

function TCDAConversionEngine.link: TCDAConversionEngine;
begin
  result := TCDAConversionEngine(inherited Link);
end;

procedure TCDAConversionEngine.load;
var
  r : TFhirResource;
begin
  FFactory := TFHIRFactoryR4.Create;
  FContext := TFHIRTransformerContext.Create(FFactory.link);
  FContext.loadFromCache(FCache);
  FMapUtils := TFHIRStructureMapUtilities.Create(FContext.Link, TFslMap<TFHIRStructureMap>.create, nil, FFactory.link);
end;

{ TLocalTransformerServices }

constructor TLocalTransformerServices.Create;
begin
  inherited Create;
  FOutcomes := TFslList<TFhirResource>.create;
  FFactory := factory;
  FOnLog := log;
  FContext := context;
end;

procedure TLocalTransformerServices.createResource(appInfo: TFslObject; res: TFHIRObject; atRootofTransform: boolean);
begin
  if (atRootofTransform) then
    FOutcomes.add(TFHIRResource(res).Link);
end;

function TLocalTransformerServices.createType(appInfo: TFslObject; tn: String): TFHIRObject;
var
  sd : TFHIRStructureDefinition;
begin
  sd := FContext.fetchResource(frtStructureDefinition, sdNs(tn)) as TFHIRStructureDefinition;
  try
    if (sd <> nil) and (sd.kind = StructureDefinitionKindLogical) then
    begin
      // result := Manager.build(context, sd);
      raise Exception.Create('Not Done yet');
    end
    else
    begin
      if (tn.startsWith('http://hl7.org/fhir/StructureDefinition/')) then
        tn := tn.substring('http://hl7.org/fhir/StructureDefinition/'.length);
      result := FFactory.makeByName(tn);
    end;
  finally
    sd.free;
  end;
end;

destructor TLocalTransformerServices.Destroy;
begin
  FOutcomes.Free;
  FFactory.Free;
  inherited;
end;

function TLocalTransformerServices.link: TLocalTransformerServices;
begin
  result := TLocalTransformerServices(inherited link);
end;

procedure TLocalTransformerServices.log(s: String);
begin
  if assigned(FOnLog) then
    FOnLog(nil, s);
end;

function TLocalTransformerServices.performSearch(appInfo: TFslObject; url: String): TFslList<TFHIRObject>;
begin
  raise EFHIRException.Create('Not implemented: performSearch');
end;

function TLocalTransformerServices.translate(appInfo: TFslObject; src: TFHIRCoding; conceptMapUrl: String): TFHIRCoding;
begin
  raise Exception.Create('Not done yet');
end;

{ TMapBreakpointResolver }

function TMapBreakpointResolver.checkRule(rule: TFhirStructureMapGroupRule; line: integer): boolean;
var
  tgt : TFhirStructureMapGroupRuleTarget;
  r : TFhirStructureMapGroupRule;
begin
  result := false;
  if rule.LocationStart.line = line then
    exit(true);
  for tgt in rule.targetList do
    if tgt.LocationStart.line = line then
      exit(true);
  for r in rule.ruleList do
    if checkRule(r, line) then
      exit(true);
end;

constructor TMapBreakpointResolver.Create(map: TFhirStructureMap);
begin
  Inherited Create;
  FMap := map;
end;

destructor TMapBreakpointResolver.Destroy;
begin
  FMap.Free;
  inherited;
end;

function TMapBreakpointResolver.checkBreakpoint(line: integer): boolean;
var
  grp : TFhirStructureMapGroup;
  rule : TFhirStructureMapGroupRule;
begin
  line := line + 1;
  result := false;
  for grp in FMap.groupList do
  begin
    if grp.LocationStart.line = line then
      exit(true);
    for rule in grp.ruleList do
    begin
      if checkRule(rule, line) then
        exit(true);
    end;
  end;
end;

end.
