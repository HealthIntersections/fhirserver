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
  fhir_common;

Const
  DWELL_TIME = 30 / (24*60) {min};

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

  TClientCacheManager = class (TFslObject)
  private
    FList : TFslList<TClientCacheManagerEntry>;
    FLock : TFslLock;
  public
    constructor Create; override;
    destructor Destroy; override;

    function cacheSize : UInt64;
    procedure clearCache;
    procedure sweep;
    function processResources(cacheId : String; list : TFslMetadataResourceList) : TFslMetadataResourceList;
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
begin
  remove := TFslMetadataResourceList.create;
  try
    for i in list do
    begin
      for j in FList do
        if (i.url = j.url) and (i.version = j.version) then
        begin
          FSize := FSize - j.sizeInBytes;
          remove.Add(j.link);
        end;
    end;
    FList.RemoveAll(remove);
    for i in list do
    begin
      FSize := FSize + i.sizeInBytes;
      FList.Add(i.link);
    end;
  finally
    remove.Free;
  end;
end;

{ TClientCacheManager }

function TClientCacheManager.cacheSize: UInt64;
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

end.
