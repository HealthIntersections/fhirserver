unit FHIRTagManager;

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


interface

uses
  SysUtils, Classes, kCritSct,
  AdvObjects, AdvGenerics,
  FHIRTags;

type
  TFHIRTagManager = class (TAdvObject)
  private
    FLock: TCriticalSection;
    FTags: TFHIRTagList;
    FTagsByKey: TAdvMap<TFHIRTag>;
    FLastTagVersionKey: integer;
    FLastTagKey: integer;
  public
    constructor Create(); override;
    destructor Destroy; override;

    property LastTagVersionKey: integer read FLastTagVersionKey write FLastTagVersionKey;
    property LastTagKey: integer read FLastTagKey write FLastTagKey;

    procedure crossLink;
    function add(key : integer; category : TFHIRTagCategory; uri, code, display : String): TFHIRTag;
    function GetTagByKey(key: integer): TFHIRTag;
    function KeyForTag(category : TFHIRTagCategory; system, code: String): integer;
    function findTag(category : TFHIRTagCategory; system, code : String) : TFHIRTag;
    procedure registerTag(tag : TFHIRTag);
    function NextTagVersionKey: integer;
    function NextTagKey: integer;
  end;

implementation

{ TFHIRTagManager }

procedure TFHIRTagManager.registerTag(tag: TFHIRTag);
begin
  FTags.add(tag.Link);
  FTagsByKey.add(inttostr(FLastTagKey), tag.Link);
end;

constructor TFHIRTagManager.Create;
begin
  inherited create;
  FLock := TCriticalSection.Create('session-manager');
  FTags := TFHIRTagList.Create;
  FTagsByKey := TAdvMap<TFHIRTag>.Create;
end;

procedure TFHIRTagManager.crossLink;
var
  i : integer;
begin
  FLock.Lock;
  try
    for i := 0 to FTags.Count - 1 do
      FTagsByKey.add(inttostr(FTags[i].key), FTags[i].Link);
  finally
    FLock.Unlock;
  end;
end;

destructor TFHIRTagManager.Destroy;
begin
  FTagsByKey.free;
  FTags.free;
  FLock.Free;
  inherited;
end;

function TFHIRTagManager.findTag(category: TFHIRTagCategory; system, code: String): TFHIRTag;
begin
  FLock.Lock('findTag');
  try
    result := FTags.findTag(category, system, code);
  finally
    FLock.Unlock;
  end;
end;

function TFHIRTagManager.add(key: integer; category: TFHIRTagCategory; uri, code, display: String): TFHIRTag;
begin
  FLock.Lock;
  try
    result := FTags.addTag(key, category, uri, code, display);
  finally
    FLock.Unlock;
  end;
end;

function TFHIRTagManager.GetTagByKey(key: integer): TFHIRTag;
begin
  FLock.Lock('GetTagByKey');
  try
    if FTagsByKey.TryGetValue(inttostr(key), result) then
      result := result.Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRTagManager.KeyForTag(category : TFHIRTagCategory; system, code: String): integer;
var
  p: TFHIRTag;
begin
  FLock.Lock('KeyForTag');
  try
    p := FTags.findTag(category, system, code);
    if (p = nil) then
      result := 0
    else
      result := p.key;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRTagManager.NextTagVersionKey: integer;
begin
  FLock.Lock('NextTagVersionKey');
  try
    inc(FLastTagVersionKey);
    result := FLastTagVersionKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRTagManager.NextTagKey: integer;
begin
  FLock.Lock('NextTagKey');
  try
    inc(FLastTagKey);
    result := FLastTagKey;
  finally
    FLock.Unlock;
  end;
end;


end.
