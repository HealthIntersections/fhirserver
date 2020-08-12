unit FHIR.Server.ClientCacheManager;

interface

uses
  SysUtils,
  FHIR.Support.Base, FHIR.Support.Threads,
  FHIR.Base.Common;

Const
  DWELL_TIME = 30 / (24*60) {min};

type
  TClientCacheManagerEntry = class (TFslObject)
  private
    FCacheId : String;
    FLastTouched : TDateTime;
    FList : TFslList<TFHIRMetadataResourceW>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TClientCacheManagerEntry; overload;
    procedure update(list : TFslList<TFHIRMetadataResourceW>);
  end;

  TClientCacheManager = class (TFslObject)
  private
    FList : TFslList<TClientCacheManagerEntry>;
    FLock : TFslLock;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure sweep;
    function processResources(cacheId : String; list : TFslList<TFHIRMetadataResourceW>) : TFslList<TFHIRMetadataResourceW>;
  end;

implementation

{ TClientCacheManagerEntry }

constructor TClientCacheManagerEntry.Create;
begin
  inherited;
  FLastTouched := now;
  FList := TFslList<TFHIRMetadataResourceW>.create;
end;

destructor TClientCacheManagerEntry.Destroy;
begin
  FList.free;
  inherited;
end;

function TClientCacheManagerEntry.link: TClientCacheManagerEntry;
begin
  result := TClientCacheManagerEntry(inherited link);
end;

procedure TClientCacheManagerEntry.update(list: TFslList<TFHIRMetadataResourceW>);
var
  i, j : TFHIRMetadataResourceW;
  remove : TFslList<TFHIRMetadataResourceW>;
begin
  remove := TFslList<TFHIRMetadataResourceW>.create;
  try
    for i in list do
    begin
      for j in FList do
        if (i.url = j.url) and (i.version = j.version) then
          remove.Add(j.link);
      FList.Add(i.link);
    end;
    FList.RemoveAll(remove);
  finally
    remove.Free;
  end;
end;

{ TClientCacheManager }

constructor TClientCacheManager.Create;
begin
  inherited;
  FLock := TFslLock.Create('ClientCacheManager');
  FList := TFslList<TClientCacheManagerEntry>.create;
end;

destructor TClientCacheManager.Destroy;
begin
  FList.Free;
  FLock.Free;
  inherited;
end;

procedure TClientCacheManager.sweep;
var
  n : TDateTime;
  list : TFslList<TClientCacheManagerEntry>;
  i : TClientCacheManagerEntry;
begin
  n := now;
  list := TFslList<TClientCacheManagerEntry>.create;
  try
    FLock.Lock('sweep');
    try
      for i in FList do
      begin
        if i.FLastTouched + DWELL_TIME < n then
          list.Add(i.Link);
      end;
      FList.RemoveAll(list);
    finally
      FLock.Unlock;
    end;
  finally
    list.Free;
  end;
end;

function TClientCacheManager.processResources(cacheId: String; list: TFslList<TFHIRMetadataResourceW>): TFslList<TFHIRMetadataResourceW>;
var
  i, f : TClientCacheManagerEntry;
  o : TFHIRMetadataResourceW;
begin
  result := TFslList<TFHIRMetadataResourceW>.create;
  try
    FLock.Lock('cache='+cacheId);
    try
      f := nil;
      for i in FList do
      begin
        if i.FCacheId = cacheId then
        begin
          f := i;
          break;
        end;
      end;
      if (f = nil) then
      begin
        f := TClientCacheManagerEntry.Create;
        FList.Add(f);
        f.FCacheId := cacheId;
      end;
      f.FLastTouched := now;
      f.update(list);
      for o in f.FList do
        result.Add(o.link);
    finally
      FLock.Unlock;
    end;
    result.link;
  finally
    result.Free;
  end;
end;

end.
