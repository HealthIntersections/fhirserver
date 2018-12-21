unit FHIR.Transformer.Engine;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Cache.PackageManager,
  FHIR.Base.Objects,
  FHIR.R4.Context, FHIR.R4.Factory, FHIR.R4.MapUtilities, FHIR.R4.Resources, FHIR.R4.ElementModel,
  FHIR.Transformer.Workspace, FHIR.Transformer.Context;

type
  TConversionEngine = class;

  TConversionEngineFetchSourceEvent = function (sender : TConversionEngine; f : TWorkspaceFile) : TStream of object;
  TConversionEngineStatusEvent = procedure (sender : TConversionEngine; message : String) of object;
  TConversionEngineLogEvent = procedure (sender : TConversionEngine; message : String) of object;

  TLocalTransformerServices = class (TTransformerServices)
  private
  public
  end;

  TConversionEngine = class abstract (TFslObject)
  private
    FSource: TWorkspaceFile;
    FOnWantSource: TConversionEngineFetchSourceEvent;
    FOnStatus: TConversionEngineStatusEvent;
    FOnLog: TConversionEngineLogEvent;
    FWorkspace: TWorkspace;
    FMap: TWorkspaceFile;
    FCache: TResourceMemoryCache;
    procedure SetSource(const Value: TWorkspaceFile);
    procedure SetWorkspace(const Value: TWorkspace);
    procedure SetMap(const Value: TWorkspaceFile);
    procedure SetCache(const Value: TResourceMemoryCache);
  protected
    FMapUtils : TFHIRStructureMapUtilities;
    procedure log(msg : String);
    function fetchSource(f : TWorkspaceFile) : TStream;
    function parseMap(f : TWorkspaceFile) : TFhirStructureMap;
  public
    destructor Destroy; override;
    function link : TConversionEngine; overload;

    property workspace : TWorkspace read FWorkspace write SetWorkspace;
    property source : TWorkspaceFile read FSource write SetSource;
    property map : TWorkspaceFile read FMap write SetMap;
    property cache : TResourceMemoryCache read FCache write SetCache;
    property OnWantSource : TConversionEngineFetchSourceEvent read FOnWantSource write FOnWantSource;
    property OnStatus : TConversionEngineStatusEvent read FOnStatus write FOnStatus;
    property OnLog : TConversionEngineLogEvent read FOnLog write FOnLog;

    procedure execute; virtual; abstract;
  end;

  TCDAConversionEngine = class (TConversionEngine)
  private
  public
    function link : TCDAConversionEngine; overload;

    procedure execute; override;
  end;

implementation

{ TConversionEngine }

destructor TConversionEngine.Destroy;
begin
  FCache.Free;
  FWorkspace.Free;
  FMap.Free;
  FSource.Free;
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
  context : TFHIRTransformerContext;
  cache : TFHIRPackageManager;
  f : TWorkspaceFile;
  stream : TStream;
  map : TFHIRStructureMap;
  elem : TFHIRMMElement;
  r : TFhirResource;
begin
  log('Preparing');
  context := TFHIRTransformerContext.Create(TFHIRFactoryR4.create);
  try
    if FCache.List.Empty then
    begin
      cache := TFHIRPackageManager.Create(true);
      try
        log('Loading the FHIR Package');
        cache.loadPackage('hl7.fhir.core', '4.0.0', ['CodeSystem', 'ValueSet', 'ConceptMap', 'StructureMap', 'StructureDefinition'], FCache.load);
        log('Loading the CDA Package');
        cache.loadPackage('hl7.fhir.cda', '0.0.1', ['CodeSystem', 'ValueSet', 'ConceptMap', 'StructureMap', 'StructureDefinition'], FCache.load);
      finally
        cache.Free;
      end;
    end;
    for r in FCache.List do
      context.SeeResource(r);
    log('Parse the Workspace Maps');
    FMapUtils := TFHIRStructureMapUtilities.Create(context.Link, TFslMap<TFHIRStructureMap>.create, TLocalTransformerServices.create());
    try
      for f in FWorkspace.maps do
      begin
        map := parseMap(f);
        FMapUtils.Lib.Add(map.url, map.Link);
      end;
      log('Load the CDA Source');
      stream := fetchSource(FSource);
      try
        elem := TFHIRMMManager.parse(context, stream, ffXml);
        try
          log('Parse the Map');
          map := parseMap(self.map);
          try
            log('Execute the Conversion [url]');
            // actually do it...
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
      FMapUtils.Free;
    end;
  finally
    context.Free;
  end;
end;

function TCDAConversionEngine.link: TCDAConversionEngine;
begin
  result := TCDAConversionEngine(inherited Link);
end;

end.
