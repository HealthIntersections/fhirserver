unit IdSoapTracker;

interface

{$I IdSoapDefines.inc}

{$IFNDEF OBJECT_TRACKING}
  // This unit should only be included if OBJECT_TRACKING is defined
{$ENDIF}


uses
  Classes,
  SyncObjs;

const
  HASH_MASK = $03FF;
  HASH_SIZE = 1024;
  GROW_SIZE = 128;
  MAX_TYPE_LEN = 35;

type
  TIdDebugObjectList = class;

  {$IFDEF CLR}
  TIdSoapObjRec = class
    FPtr : integer;
    FBreakpoint : boolean;
  end;
  pIdSoapObjRec = TIdSoapObjRec;
  TObjArray = array of TIdSoapObjRec;
  pObjArray = TObjArray;
  {$ELSE}
  TIdSoapObjRec = packed record
    FPtr : integer;
    FBreakpoint : boolean;
  end;
  pIdSoapObjRec = ^TIdSoapObjRec;

  // this used to be 0..0 but range checking doesn't like that
  TObjArray = array [0.. MAXINT div (SizeOf(integer) * 2)] of TIdSoapObjRec;
  pObjArray = ^TObjArray;
  {$ENDIF}

  TIdDebugObjectSubList = class(TObject)
  Private
    FVal: Cardinal;
    FCount, FAllocated: Cardinal;
    FItems: pObjArray;
    procedure AddItem(AObj: Integer);
    procedure DeleteItem(AObj: Integer);
    function FindItem(AObj: Integer; var VIndex: Cardinal): Boolean;
    procedure Grow;
    function GetRec(AObj : Integer):pIdSoapObjRec;
  Public
    constructor Create;
    destructor Destroy; Override;
  end;

  TListArray = array  of TIdDebugObjectSubList;

  TIdDebugObjectList = class(TObject)
  Private
    FLock: TCriticalSection;
    FLastSerialNo : Integer;
    FTotalCount : Integer;
    FClassCount : TStringList;
    FHashTable : TListArray;
    function GetExists(AObj: TObject): Boolean;
    function GetBreakpoint(AObj: TObject): Boolean;
    procedure SetBreakPoint(AObj: TObject; const AValue: Boolean);
  Public
    constructor Create;
    destructor Destroy; Override;

    property TotalCount : integer read FTotalCount;

    property Exists[AObj: TObject]: Boolean Read GetExists;
    property BreakPoint[AObj : TObject] : Boolean read GetBreakpoint write SetBreakPoint;

    function Construct(AObject:TObject):Integer; // returns serial number
    procedure Destruct(AObject:TObject);

    function ClassCount(AClassName : String) : Integer;
    procedure GetClassCounts(AList : TStrings);

    function DescribeLiveObjects : String;
  end;

function PadString(const AStr: String; AWidth: Integer): String;

{$IFNDEF VCL5ORABOVE}
procedure FreeAndNil(var Obj);
{$ENDIF}

implementation

uses
  {$IFDEF CLR}
  System.Diagnostics,
  {$ENDIF}
  SysUtils;

const
  ASSERT_UNIT = 'IdSoapTracker';

//break point into the debugger if there is one;
procedure IdBreakpoint;
begin
  {$IFDEF CLR}
  Debugger.Break;
  {$ELSE}
  {$IFDEF WIN32}
  try
    asm
    int $03
    end;
  except
    // on some poorly configured Windows systems int $03 can cause unhandled
    // exceptions with improperly installed Dr Watsons etc....
  end;
  {$ENDIF}
  {$ENDIF}
end;

{$IFNDEF VCL5ORABOVE}
procedure FreeAndNil(var Obj);
begin
  TObject(Obj).Free;
  TObject(Obj) := nil;
end;
{$ENDIF}


{ TIdDebugObjectSubList }

constructor TIdDebugObjectSubList.Create;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.Create';
{$IFDEF CLR}
var
  i : integer;
{$ENDIF}
begin
  inherited Create;
  FCount := 0;
  FAllocated := GROW_SIZE;
  {$IFDEF CLR}
  SetLength(FItems, FAllocated);
  for i := 0 to FAllocated - 1 do
    begin
    FItems[i] := TIdSoapObjRec.create;
    end;
  {$ELSE}
  GetMem(FItems, FAllocated * sizeof(TIdSoapObjRec));
  {$ENDIF}
end;

destructor TIdDebugObjectSubList.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.Destroy';
{$IFDEF CLR}
var
  i : integer;
{$ENDIF}
begin
  assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  {$IFDEF CLR}
  for i := Low(FItems) to HIgh(FItems) do
    begin
    FItems[i].free;
    end;
  SetLength(FItems, 0);
  {$ELSE}
  FreeMem(FItems);
  {$ENDIF}
  inherited Destroy;
end;

procedure TIdDebugObjectSubList.AddItem(AObj: Integer);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.AddItem';
var
  i: Cardinal;
  {$IFDEF CLR}
  j : integer;
  {$ENDIF}
begin
  assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  // no check on AObj
  if FindItem(AObj, i) then
    begin
    assert(False, ASSERT_LOCATION+': Attempt to re-register an object')
    end
  else
    begin
    if FCount = FAllocated then
      Grow;
    assert(i < FAllocated, ASSERT_LOCATION+': i >= allocated');
    {$IFDEF CLR}
    if I < FCount then
      begin
      for j := FCount - 1 downto i do
        begin
        FItems[j+1] := FItems[j];
        end;
      end;
    FItems[i] := TIdSoapObjRec.create;
    FItems[i].FPtr := AObj;
    FItems[i].FBreakpoint := false;
    {$ELSE}
    if I < FCount then
      System.Move(FItems^[i], FItems^[i + 1], (FCount - I) * SizeOf(TIdSoapObjRec));
    FItems^[i].FPtr := AObj;
    FItems^[i].FBreakpoint := false;
    {$ENDIF}
    Inc(FCount);
    end;
end;

function TIdDebugObjectSubList.GetRec(AObj: Integer): pIdSoapObjRec;
var
  LIndex : Cardinal;
begin
  if FindItem(AObj, LIndex) then
    begin
    {$IFDEF CLR}
    result := FItems[LIndex];
    {$ELSE}
    result := @FItems[LIndex];
    {$ENDIF}
    end
  else
    begin
    result := nil;
    end;
end;

function TIdDebugObjectSubList.FindItem(AObj: Integer; var VIndex: Cardinal): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.FindItem';
var
  L, H, I, C: Integer;
begin
  assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  // no check on AObj
  Result := False;
  L := 0;
  H := integer(FCount) - 1; // workaround a bad pascal range checking bug
  while L <= H do
    begin
    I := (L + H) shr 1;
    {$IFDEF CLR}
    C := FItems[I].FPtr - AObj;
    {$ELSE}
    C := FItems^[I].FPtr - AObj;
    {$ENDIF}
    if C < 0 then
      begin
      L := I + 1
      end
    else
      begin
      H := I - 1;
      if C = 0 then
        begin
        Result := True;
        L := I;
        end;
      end;
    end;
  VIndex := L;
end;

procedure TIdDebugObjectSubList.Grow;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.Grow';
{$IFDEF CLR}
var
  i : integer;
{$ENDIF}
begin
  assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  inc(FAllocated, GROW_SIZE);
  {$IFDEF CLR}
  SetLength(FItems, FAllocated);
  for i := FAllocated - GROW_SIZE to FAllocated - 1 do
    begin
    FItems[i] := nil;
    end;
  {$ELSE}
  ReallocMem(FItems, FAllocated * SizeOf(TIdSoapObjRec));
  Assert(FItems <> NIL, ASSERT_LOCATION+': Grow failed to reallocate it''s memory');
  {$ENDIF}
end;

procedure TIdDebugObjectSubList.DeleteItem(AObj: Integer);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.DeleteItem';
var
  i: Cardinal;
  {$IFDEF CLR}
  j : integer;
  {$ENDIF}
begin
  assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  // no check on AObj
  if FindItem(AObj, i) then
    begin
    Dec(FCount);
    {$IFDEF CLR}
    FItems[i].Free;
    for j := i to FCount - 1 do
      begin
      FItems[j] := FItems[j+1];
      end;
    FItems[FCount] := nil;
    {$ELSE}
    if I < FCount then
      System.Move(FItems^[I + 1], FItems^[I], (FCount - I) * SizeOf(TIdSoapObjRec));
    {$ENDIF}
    end
  else
    begin
    assert(False, ASSERT_LOCATION+': Attempt to de-register an object that doesn''t exist ("'+TObject(AObj).ClassName+'")');
    end;
end;

{ TIdDebugObjectList }

constructor TIdDebugObjectList.Create;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectList.Create';
var
  i: Longint;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FLastSerialNo := 0;
  FTotalCount := 0;
  FClassCount := TStringList.create;
  FClassCount.Sorted := true;
  FClassCount.Duplicates := dupError;
  SetLength(FHashTable, HASH_SIZE);
  for i := 0 to HASH_SIZE - 1 do
    begin
    FHashtable[i] := TIdDebugObjectSubList.Create;
    FHashtable[i].FVal := i;
    end;
end;

destructor TIdDebugObjectList.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectList.Destroy';
var
  i: Longint;
begin
  assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  for i := 0 to HASH_SIZE - 1 do
    begin
    FreeAndNil(FHashtable[i]);
    end;
  SetLength(FHashTable, 0);
  FreeAndNil(FLock);
  FreeAndNil(FClassCount);
  inherited Destroy;
end;

function TIdDebugObjectList.GetExists(AObj: TObject): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectList.GetExists';
var
  LDummy: Cardinal;
begin
  assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  // no check on AObj

  Result := FHashTable[(Cardinal(AObj) shr 3) and HASH_MASK].FindItem(Integer(AObj), LDummy);
end;

function TIdDebugObjectList.ClassCount(AClassName : String): Integer;
var
  LIndex : integer;
begin
  result := 0;
  if FClassCount.Find(AClassName, LIndex) then
    begin
    result := integer(FClassCount.Objects[LIndex])
    end
end;

function TIdDebugObjectList.Construct(AObject: TObject): Integer;
var
  i : integer;
begin
  assert(Assigned(AObject));
  FLock.Enter;
  try
    assert(not Exists[AObject]);

    inc(FTotalCount);
    if not FClassCount.find(AObject.ClassName, i) then
      begin
      i := FClassCount.AddObject(AObject.ClassName, NIL);
      end;
    FClassCount.objects[i] := TObject(Integer(FClassCount.objects[i]) + 1);

    inc(FLastSerialNo);
    Result := FLastSerialNo;

    FHashTable[(Cardinal(AObject) shr 3) and HASH_MASK].AddItem(Integer(AObject))
  finally
    FLock.Leave;
  end;
end;

procedure TIdDebugObjectList.Destruct(AObject: TObject);
var
  LRec : pIdSoapObjRec;
  i : Integer;
begin
  assert(Assigned(AObject));
  FLock.Enter;
  try
    assert(Exists[AObject]);
    LRec := FHashTable[(Cardinal(AObject) shr 3) and HASH_MASK].GetRec(Integer(AObject));
    assert(Assigned(LRec));
    if LRec{$IFNDEF CLR}^{$ENDIF}.FBreakPoint then
      begin
      IdBreakPoint;
      end;
    If (FTotalCount = 0) or not FClassCount.find(AObject.ClassName, i) then
      begin
      // likely cause for being here : a missing inherited create in some descendent class
      IdBreakpoint;
      end
    else
      begin
      dec(FTotalCount);
      FClassCount.objects[i] := TObject(Integer(FClassCount.objects[i]) - 1);
      FHashTable[(Cardinal(AObject) shr 3) and HASH_MASK].DeleteItem(Integer(AObject))
      end;
  finally
    FLock.Leave;
  end;
end;

function TIdDebugObjectList.GetBreakpoint(AObj: TObject): Boolean;
var
  LRec : pIdSoapObjRec;
begin
  assert(Exists[AObj]);
  LRec := FHashTable[(Cardinal(AObj) shr 3) and HASH_MASK].GetRec(Integer(AObj));
  result := Assigned(LRec) and LRec{$IFNDEF CLR}^{$ENDIF}.FBreakPoint;
end;

procedure TIdDebugObjectList.SetBreakPoint(AObj: TObject; const AValue: Boolean);
var
  LRec : pIdSoapObjRec;
begin
  assert(Exists[AObj]);
  LRec := FHashTable[(Cardinal(AObj) shr 3) and HASH_MASK].GetRec(Integer(AObj));
  assert(Assigned(LRec));
  LRec{$IFNDEF CLR}^{$ENDIF}.FBreakPoint := AValue;
end;

function TIdDebugObjectList.DescribeLiveObjects: String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectList.DescribeLiveObjects';
var
  i: Integer;
begin
  FLock.Enter;
  try
    Result := '';
    for I := 0 to FClassCount.Count - 1 do    // Iterate
      if Integer(FClassCount.Objects[i]) <> 0 then
        begin
        Result := Result + FClassCount[i] + ' ' + IntToStr(Integer(FClassCount.Objects[i])) + #13#10;
        end;
  finally
    FLock.Leave;
    end;
end;

function PadString(const AStr: String; AWidth: Integer): String;
begin
  if Length(AStr) >= AWidth then
    begin
    Result := AStr
    end
  else
    begin
    {$IFDEF CLR}
    result := AStr;
    while length(result) < AWidth do
      begin
      result := result + ' ';
      end;
    {$ELSE}
    SetLength(Result, AWidth);
    FillChar(Result[1], AWidth, ' ');
    if AStr <> '' then
      begin
      Move(AStr[1], Result[1], Length(AStr))
      end;
    {$ENDIF}
    end;
end;

procedure TIdDebugObjectList.GetClassCounts(AList: TStrings);
var
  i : integer;
begin
  FLock.Enter;
  try
    for i := 0 to FClassCount.Count - 1 do
      if integer(FClassCount.Objects[i]) <> 0 then
        begin
        AList.Add(PadString(FClassCount[i], MAX_TYPE_LEN)+' ' +inttostr(integer(FClassCount.Objects[i])));
        end;
  finally
    FLock.Leave;
  end;
end;

end.



