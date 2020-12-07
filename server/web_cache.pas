unit web_cache;

{$i fhir.inc}

interface

Uses
  SysUtils, Classes,
  IdCustomHTTPServer, IdContext,
  fsl_base, fsl_threads, fsl_stream, fsl_utilities;

type
  TCachedHTTPResponse = class (TFslBuffer)
  private
    FContentType: String;
    FSummary : String;
    FHitCount : Cardinal;
  public
    function Link : TCachedHTTPResponse; overload;
    property ContentType : String read FContentType write FContentType;
    property Summary : String read FSummary write FSummary;
    property HitCount : Cardinal read FHitCount write FHitCount;
  end;

  THTTPCacheManager = class (TFslObject)
  private
    FLock : TFslLock;
    FSize : Cardinal;
    FCache : TFslMap<TCachedHTTPResponse>;
    FMaxSize: Cardinal;
    function generateKey(ep : String; req : TIdHTTPRequestInfo) : String;
  public
    constructor Create; override;
    destructor Destroy; override;
    function respond(ep : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; var summary : String) : boolean;
    procedure recordResponse(ep : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; summary : String);
    procedure Clear;
    procedure Trim;
    property MaxSize : Cardinal read FMaxSize write FMaxSize;
  end;


implementation

{ TCachedHTTPResponse }

function TCachedHTTPResponse.Link: TCachedHTTPResponse;
begin
  result := TCachedHTTPResponse(inherited Link);
end;

{ THTTPCacheManager }

constructor THTTPCacheManager.Create;
begin
  inherited Create;
  FLock := TFslLock.Create('HTTP.Cache');
  FCache := TFslMap<TCachedHTTPResponse>.create('HTTP.Cache');
  FSize := 0;
  FMaxSize := 1024 * 1024 * 1024; // 1 GB
end;

destructor THTTPCacheManager.Destroy;
begin
  FCache.Free;
  FLock.Free;
  inherited;
end;

function THTTPCacheManager.generateKey(ep : String; req: TIdHTTPRequestInfo): String;
var
  pos : Integer;
begin
  result :=  ep+'|'+req.RawHTTPCommand+'|'+req.Accept+'|'+req.AuthUsername;
  if req.PostStream <> nil then
  begin
    pos := req.PostStream.Position;
    result := result+'|'+intToStr(GetCRC(StreamToBytes(req.PostStream)));
    req.PostStream.Position := pos;
  end;
end;

procedure THTTPCacheManager.recordResponse(ep : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; summary : String);
var
  key : String;
  pos : integer;
  co : TCachedHTTPResponse;
begin
  pos := response.ContentStream.Position;
  key := generateKey(ep, request);
  co := TCachedHTTPResponse.Create;
  try
    co.ContentType := response.ContentType;
    co.LoadFromStream(response.ContentStream);
    co.Summary := summary;
    FLock.Lock;
    try
      FCache.AddOrSetValue(key, co.Link);
      inc(FSize, co.Size);
    finally
      FLock.Unlock;
    end;
  finally
    co.Free;
  end;
  response.ContentStream.Position := pos;
end;

function THTTPCacheManager.respond(ep : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; var summary : String): boolean;
var
  co : TCachedHTTPResponse;
begin
  FLock.Lock;
  try
    result := FCache.TryGetValue(generateKey(ep, request), co);
    if result then
    begin
      co.Link;
      inc(co.FHitCount);
    end;
  finally
    FLock.Unlock;
  end;
  if result then
  begin
    try
      response.ContentStream := TMemoryStream.create;
      co.SaveToStream(response.ContentStream);
      response.ContentStream.Position := 0;
      response.ContentType := co.ContentType;
      summary := co.Summary + ' (from cache)';
    finally
      co.Free;
    end;
  end;
end;


procedure THTTPCacheManager.Trim;
var
  i : cardinal;
  s : String;
  list : TStringList;
  v : TCachedHTTPResponse;
begin
  list := TStringList.create;
  try
    FLock.Lock;
    try
      i := 1;
      while FSize > FMaxSize do
      begin
        for s in FCache.Keys do
        begin
          v := FCache[s];
          if v.HitCount <= i then
          begin
            list.Add(s);
            FSize := FSize - v.Size;
          end;
          if (FSize < FMaxSize) then
            break;
        end;
        FCache.RemoveKeys(list);
        list.clear;
        inc(i);
      end;
    finally
      FLock.Unlock;
    end;
  finally
    list.Free;
  end;
end;

procedure THTTPCacheManager.Clear;
begin
  FLock.Lock;
  try
    FCache.Clear;
    FSize := 0;
  finally
    FLock.Unlock;
  end;
end;


end.
