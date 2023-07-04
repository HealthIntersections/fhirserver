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
  fsl_base, fsl_threads,
  fhir_common,
  server_constants, server_stats;

type
  TClientCacheManagerEntry = class (TFslObject)
  private
    FCacheId : String;
    FLastTouched : TDateTime;
    FList : TFslMetadataResourceList;
    FSize : UInt64;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TClientCacheManagerEntry; overload;
    procedure update(list : TFslMetadataResourceList);
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
    function processResources(cacheId : String; list : TFslMetadataResourceList) : TFslMetadataResourceList;
    procedure recordStats(rec : TStatusRecord);
    property cacheDwellTime : TDateTime read FCacheDwellTime write FCacheDwellTime;
  end;

implementation

{ TClientCacheManagerEntry }

constructor TClientCacheManagerEntry.Create;
begin
  inherited;
  FLastTouched := now;
  FList := TFslMetadataResourceList.create;
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

procedure TClientCacheManagerEntry.update(list: TFslMetadataResourceList);
var
  i, j : TFHIRMetadataResourceW;
  remove : TFslMetadataResourceList;
  c : cardinal;
  magic : integer;
begin
  magic := nextMagic;
  remove := TFslMetadataResourceList.create;
  try
    for i in list do
    begin
      for j in FList do
        if (i.url = j.url) and (i.version = j.version) then
        begin
          c := j.sizeInBytes(magic);
          if (c > FSize) then
            FSize := 0
          else
            FSize := FSize - c;
          remove.Add(j.link);
        end;
    end;
    FList.RemoveAll(remove);
    for i in list do
    begin
      if (i.url <> '') then
      begin
        FSize := FSize + i.sizeInBytes(magic);
        FList.Add(i.link);
      end;
    end;
  finally
    remove.Free;
  end;
end;

{ TClientCacheManager }

function TClientCacheManager.cacheSize(magic : integer): UInt64;
var
  item : TClientCacheManagerEntry;
begin
  FLock.Lock;
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
  FLock.Lock;
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
  FList := TFslList<TClientCacheManagerEntry>.create;
  FCacheDwellTime := DEFAULT_DWELL_TIME;
end;

destructor TClientCacheManager.Destroy;
begin
  FList.Free;
  FLock.Free;
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
  list := TFslList<TClientCacheManagerEntry>.create;
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
    list.Free;
  end;
end;

function TClientCacheManager.processResources(cacheId: String; list: TFslMetadataResourceList): TFslMetadataResourceList;
var
  i, f : TClientCacheManagerEntry;
  o : TFHIRMetadataResourceW;
begin
  result := TFslMetadataResourceList.create;
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

procedure TClientCacheManager.recordStats(rec: TStatusRecord);
var
  e : TClientCacheManagerEntry;
begin
  FLock.Lock();
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
