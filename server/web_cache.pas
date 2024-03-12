unit web_cache;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$i fhir.inc}

interface

Uses
  SysUtils, Classes,
  IdCustomHTTPServer, IdContext,
  fsl_base, fsl_threads, fsl_stream, fsl_utilities,
  kernel_thread, server_stats, server_constants;

type
  TCachedHTTPResponse = class (TFslBuffer)
  private
    FContentType: String;
    FSummary : String;
    FHitCount : Cardinal;   
    FLastTouched : TDateTime;
  public
    function Link : TCachedHTTPResponse; overload;
    property ContentType : String read FContentType write FContentType;
    property Summary : String read FSummary write FSummary;
    property HitCount : Cardinal read FHitCount write FHitCount;
  end;

  { THTTPCacheManager }

  THTTPCacheManager = class (TFslObject)
  private
    FCacheDwellTime: TDateTime;
    FLock : TFslLock;
    FSize : Cardinal;
    FCache : TFslMap<TCachedHTTPResponse>;
    FMaxSize: Cardinal;
    FCaching : boolean;
    FMinTat : Cardinal;
    function generateKey(ep : String; req : TIdHTTPRequestInfo) : String;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(minTat : cardinal);
    destructor Destroy; override;
    function respond(ep : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; var summary : String) : boolean;
    procedure recordResponse(ep : String; request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; tat : UInt64; summary : String);
    procedure Clear;
    procedure Trim(callback : TFhirServerMaintenanceThreadTaskCallBack);
    property Caching : boolean read FCaching write FCaching;
    property MaxSize : Cardinal read FMaxSize write FMaxSize;    
    property cacheDwellTime : TDateTime read FCacheDwellTime write FCacheDwellTime;
    procedure recordStats(rec : TStatusRecord);
  end;


implementation

{ TCachedHTTPResponse }

function TCachedHTTPResponse.Link: TCachedHTTPResponse;
begin
  result := TCachedHTTPResponse(inherited Link);
end;

{ THTTPCacheManager }

constructor THTTPCacheManager.Create(minTat : cardinal);
begin
  inherited Create;
  FLock := TFslLock.Create('HTTP.Cache');
  FCache := TFslMap<TCachedHTTPResponse>.Create('HTTP.Cache');
  FSize := 0;
  FMaxSize := 1024 * 1024 * 1024; // 1 GB
  FMinTat := minTat;
  FCacheDwellTime := DEFAULT_DWELL_TIME;
end;

destructor THTTPCacheManager.Destroy;
begin
  FCache.free;
  FLock.free;
  inherited;
end;

function THTTPCacheManager.generateKey(ep : String; req: TIdHTTPRequestInfo): String;
var
  pos : Integer;
begin
  result :=  ep+'|'+req.RawHTTPCommand+'|'+req.Accept+'|'+req.AuthUsername+'|'+req.AcceptLanguage+'|'+req.AcceptCharSet+'|'+req.AcceptEncoding;
  if req.PostStream <> nil then
  begin
    pos := req.PostStream.Position;
    result := result+'|'+intToStr(GetCRC(StreamToBytes(req.PostStream)));
    req.PostStream.Position := pos;
  end;
end;

procedure THTTPCacheManager.recordResponse(ep : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; tat : UInt64; summary : String);
var
  key : String;
  pos : integer;
  co : TCachedHTTPResponse;
begin
  if Caching and (tat >= FMinTat) then
  begin
    pos := response.ContentStream.Position;
    key := generateKey(ep, request);
    co := TCachedHTTPResponse.Create;
    try
      co.ContentType := response.ContentType;
      co.LoadFromStream(response.ContentStream);
      co.Summary := summary;
      co.FLastTouched := now;
      FLock.Lock;
      try
        FCache.AddOrSetValue(key, co.Link);
        inc(FSize, co.Size);
      finally
        FLock.Unlock;
      end;
    finally
      co.free;
    end;
    response.ContentStream.Position := pos;
  end;
end;

function THTTPCacheManager.respond(ep : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; var summary : String): boolean;
var
  co : TCachedHTTPResponse;
begin
  if not FCaching or UnderDebugger or request.QueryParams.contains('no-cache=please') then
    exit(false);

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
      co.FLastTouched := now;
      response.ContentStream := TMemoryStream.Create;
      co.SaveToStream(response.ContentStream);
      response.ContentStream.Position := 0;
      response.ContentType := co.ContentType;
      summary := co.Summary + ' (from cache)';
    finally
      co.free;
    end;
  end;
end;


function THTTPCacheManager.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic) + SizeOf(FLock) + SizeOf(FSize) + FCache.sizeInBytes(magic)+SizeOf(FMaxSize)+SizeOf(FCaching);
end;

procedure THTTPCacheManager.Trim(callback : TFhirServerMaintenanceThreadTaskCallBack);
var
  i : cardinal;
  s : String;
  list : TStringList;
  v : TCachedHTTPResponse;
  dt : TDateTime;
begin
  callback(self, 'Trimming Cache', -1);
  list := TStringList.Create;
  try
    FLock.Lock;
    try
      dt := now - FCacheDwellTime;
      for s in FCache.Keys do
      begin
        v := FCache[s];
        if (v.FLastTouched < dt) then
        begin
          list.add(s);
          FSize := FSize - v.Size;
        end;
      end;
      FCache.RemoveKeys(list);

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
    list.free;
  end;
end;

procedure THTTPCacheManager.recordStats(rec: TStatusRecord);
begin
  rec.HTTPCacheCount := FCache.Count;
  rec.HTTPCacheSize := FSize;
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
