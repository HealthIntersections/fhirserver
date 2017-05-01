unit FHIRTagManager;

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
    constructor Create();
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
