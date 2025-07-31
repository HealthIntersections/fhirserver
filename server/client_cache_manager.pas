unit client_cache_manager;

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
  SysUtils,
  fsl_base, fsl_threads, fsl_logging,
  fhir_common, fhir_tx,
  server_constants, server_stats;

type
  TClientCacheManagerEntry = class (TFslObject)
  private
    FCacheId : String;
    FLastTouched : TDateTime;
    FList : TFslList<TFHIRCachedMetadataResource>;
    FSize : UInt64;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TClientCacheManagerEntry; overload;
    procedure update(list : TFslList<TFHIRCachedMetadataResource>);
  end;

  { TClientCacheManager }

  TClientCacheManager = class (TFslObject)
  private
    FCacheDwellTime: TDateTime;
    FList : TFslList<TClientCacheManagerEntry>;
    FLock : TFslLock;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function cacheSize(magic : integer) : UInt64;
    procedure clearCache;
    procedure sweep;
    function processResources(cacheId : String; list : TFslList<TFHIRCachedMetadataResource>) : TFslList<TFHIRCachedMetadataResource>;
    procedure recordStats(rec : TStatusRecord);
    property cacheDwellTime : TDateTime read FCacheDwellTime write FCacheDwellTime;
  end;

implementation

{ TClientCacheManagerEntry }

constructor TClientCacheManagerEntry.Create;
begin
  inherited;
  FLastTouched := now;
  FList := TFslList<TFHIRCachedMetadataResource>.Create;
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

procedure TClientCacheManagerEntry.update(list: TFslList<TFHIRCachedMetadataResource>);
var
  i, j : TFHIRCachedMetadataResource;
  remove : TFslList<TFHIRCachedMetadataResource>;
  c : cardinal;
  magic : integer;
begin
  magic := nextMagic;
  remove := TFslList<TFHIRCachedMetadataResource>.Create;
  try
    for i in list do
    begin
      for j in FList do
        if (i.resource.url = j.resource.url) and (i.resource.version = j.resource.version) then
        begin
          c := j.resource.sizeInBytes(magic);
          if (c > FSize) then
            FSize := 0
          else
            FSize := FSize - c;
          //Logging.log('Cache '+FCacheId+': remove '+j.vurl);
          remove.Add(j.link);
        end;
    end;
    FList.RemoveAll(remove);
    for i in list do
    begin
      if (i.resource.url <> '') then
      begin
        //Logging.log('Cache '+FCacheId+': add '+i.vurl);
        FSize := FSize + i.resource.sizeInBytes(magic);
        FList.Add(i.link);
      end;
    end;
  finally
    remove.free;
  end;
end;

{ TClientCacheManager }

function TClientCacheManager.cacheSize(magic : integer): UInt64;
var
  item : TClientCacheManagerEntry;
begin
  FLock.Lock('cacheSize');
  try
    result := 0;
    for item in FList do
      result := result + item.FSize;
  finally
    FLock.Unlock;
  end;
end;

procedure TClientCacheManager.clearCache;
begin
  FLock.Lock('clearCache');
  try
    FList.Clear;
  finally
    FLock.Unlock;
  end;
end;

constructor TClientCacheManager.Create;
begin
  inherited;
  FLock := TFslLock.Create('ClientCacheManager');
  FList := TFslList<TClientCacheManagerEntry>.Create;
  FCacheDwellTime := DEFAULT_DWELL_TIME;
end;

destructor TClientCacheManager.Destroy;
begin
  FList.free;
  FLock.free;
  inherited;
end;

function TClientCacheManager.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic) + SizeOf(FLock) + FList.sizeInBytes(magic);
end;

procedure TClientCacheManager.sweep;
var
  n : TDateTime;
  list : TFslList<TClientCacheManagerEntry>;
  i : TClientCacheManagerEntry;
begin
  n := now;
  list := TFslList<TClientCacheManagerEntry>.Create;
  try
    FLock.Lock('sweep');
    try
      for i in FList do
      begin
        if i.FLastTouched + FCacheDwellTime < n then
          list.Add(i.Link);
      end;
      if list.count > 0 then
        FList.RemoveAll(list);
    finally
      FLock.Unlock;
    end;
  finally
    list.free;
  end;
end;

function TClientCacheManager.processResources(cacheId: String; list: TFslList<TFHIRCachedMetadataResource>): TFslList<TFHIRCachedMetadataResource>;
var
  i, f : TClientCacheManagerEntry;
  o : TFHIRCachedMetadataResource;
begin
  result := TFslList<TFHIRCachedMetadataResource>.Create;
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
    result.free;
  end;
end;

procedure TClientCacheManager.recordStats(rec: TStatusRecord);
var
  e : TClientCacheManagerEntry;
begin
  FLock.Lock('recordStats');
  try
    rec.ClientCacheCount := rec.ClientCacheCount + FList.count;
    for e in FList do
    begin
      rec.ClientCacheObjectCount := rec.ClientCacheObjectCount + e.FList.Count;
      rec.ClientCacheSize := rec.ClientCacheSize + e.FList.sizeInBytes(rec.magic);
    end;
  finally
    FLock.Unlock;
  end;
end;

end.
