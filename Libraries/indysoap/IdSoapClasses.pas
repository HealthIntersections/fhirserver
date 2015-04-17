{
IndySOAP: Support Classes
}

unit IdSoapClasses;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  Contnrs,
  IdSoapDebug,
  SysUtils,
  SyncObjs,
  windows;

type
  TIdStringList = class(TStringList)
  Private
    FSerialNo : cardinal;
    FOwnsObjects: Boolean;
  Public
    constructor Create(AOwnsObjects: Boolean = False);
    destructor Destroy; Override;
    procedure Clear; Override;
    procedure Delete(AIndex: Integer); Override;
    property OwnsObjects: Boolean Read FOwnsObjects Write FOwnsObjects;
    function TestValid(AClassType: TClass = NIL): Boolean;
  end;

  { there is potential leak in the use of TMemoryStreams on the client.
    TIdMemoryStream connects to the IndySoap object tracking to help catch
    leaks }
  {$M+}
  TIdMemoryStream = class(TMemoryStream)
  Private
    FSerialNo: Cardinal;
    function GetDataString: AnsiString;
    procedure SetDataString(const aValue: AnsiString);
    {$IFDEF UNICODE}
    function GetDataBytes: TBytes;
    procedure SetDataBytes(const aValue: TBytes);
    {$ENDIF}
  Public
    constructor Create;
    destructor Destroy; Override;
    function TestValid(AClassType: TClass = NIL): Boolean;
    class function Clone(AStream : TStream):TIdMemoryStream;
  published
    Constructor CreateString(ADataString : AnsiString);
    property SerialNumber : Cardinal read FSerialNo;
    Property DataString : AnsiString Read GetDataString Write SetDataString;
    {$IFDEF UNICODE}
    Property DataBytes : TBytes Read GetDataBytes Write SetDataBytes;
    {$ENDIF}
  end;
  {$M-}

  TIdCriticalSection = class(TSynchroObject)
  Protected
    FSerialNo : Cardinal;
    FSection: TRTLCriticalSection;
    FOwnerThread: Cardinal;
    FOwnerThreadCount: Cardinal;
  Public
    constructor Create;
    destructor Destroy; Override;
    procedure Acquire; Override;
    procedure Release; Override;
    procedure Enter;
    procedure Leave;
    function LockedToMe: Boolean;
  end;

  TIdObjectList = class(TObjectList)
  Private
    FSerialNo : cardinal;
  Public
    constructor Create(AOwnsObjects: Boolean);
    destructor Destroy; Override;
    function TestValid(AClassType: TClass = NIL): Boolean;
  end;

const
  ID_DEFAULT_HASH_SIZE = 100;
  ID_DEFAULT_ALLOCATION_SIZE = 16;

type
  TOnDisposeEvent = procedure(AData: TObject) of object;

  TIdKeyList = class;

  TIdKeyPair = {$IFDEF CLR} class {$ELSE} record {$ENDIF}
    FKey: Integer;
    FValue: TObject;
  end;

  TIdKeyPairArray = array of TIdKeyPair;

  TIdKeyListSection = class(TIdBaseObject)
  Private
    FCount,
    FAllocated: Integer;
    FItems: TIdKeyPairArray;
    FList: TIdKeyList;
    procedure Grow;
    function FindItem(AKey: Integer; var VIndex: Integer): Boolean;
    procedure AddItem(AKey: Integer; AValue : TObject);
    function GetItem(AKey : Integer; ADefaultValue: TObject): TObject;
    procedure Delete(AKey: Integer);
  Public
    constructor Create(AOwner: TIdKeyList);
    destructor Destroy; Override;
  end;

  TIdSectionList = array of TIdKeyListSection;

  TIdKeyProgressRec = record
    Step1, Step2: Integer;
  end;

  TIdKeyList = class(TIdBaseObject)
  Private
    FHashSize: Integer;
    FHashTable: TIdSectionList;

    FMemoryUsage: Integer;
    FOnDispose: TOnDisposeEvent;
    FCount: Integer;

    {$IFNDEF CLR}
    function GetAsPointer(AKey: Integer): pointer;
    procedure SetAsPointer(AKey: Integer; const AValue: pointer);
    {$ENDIF}
    function GetAsInt(AKey: Integer): Integer;
    function GetAsObj(AKey: Integer): TObject;
    procedure SetAsInt(AKey: Integer; const AValue: Integer);
    procedure SetAsObj(AKey: Integer; const AValue: TObject);
    procedure init(AHashSize: Integer);
    function GetExists(AKey: Integer): Boolean;
  Public
    constructor Create(AHashSize: Integer = ID_DEFAULT_HASH_SIZE);
    destructor Destroy; Override;
    property HashSize: Integer Read FHashSize;
    property MemoryUsage: Integer Read FMemoryUsage;
    {$IFNDEF CLR}
    property AsPtr[AKey: Integer]: pointer Read GetAsPointer Write SetAsPointer;
    {$ENDIF}
    property AsInt[AKey: Integer]: Integer Read GetAsInt Write SetAsInt;
    property AsObj[AKey: Integer]: TObject Read GetAsObj Write SetAsObj; default;
    property Exists[AKey: Integer]: Boolean Read GetExists;
    property Count: Integer Read FCount;
    property OnDispose: TOnDisposeEvent Read FOnDispose Write FOnDispose;
    procedure Delete(AKey: Integer);
    procedure Clear;
    procedure DisposeObject(AObj : TObject);
    function GetFirstKey(var VProgressRec: TIdKeyProgressRec; var VKey: Integer): Boolean;
    function GetNextKey(var VProgressRec: TIdKeyProgressRec; var VKey: Integer): Boolean;
  end;

  // template:
  //
  // var
  //  LIter : TKeyListIterator;
  //
  //  LIter := TKeyListIterator.create(AKeyList);
  //  try
  //    while LIter.More do
  //      begin
  //      DoSomething(AKeyList[LIter.Key]);
  //      LIter.Next
  //      end;
  //  finally
  //    LIter.Free;
  //  end;
  //
  TIdKeyListIterator = class(TIdBaseObject)
  Private
    FList: TIdKeyList;
    FRec: TIdKeyProgressRec;
    FMore: Boolean;
    FKey: Integer;
    FCount : integer;
  Public
    constructor Create(AKeyList: TIdKeyList);
    procedure Next;
    property More: Boolean Read FMore;
    property Key: Integer Read FKey;
    property Count : integer read FCount;
  end;


implementation


const
  ASSERT_UNIT = 'IdSoapClasses';

{ TIdStringList }

constructor TIdStringList.Create;
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
  FSerialNo := IdObjectRegister(self);
end;

destructor TIdStringList.Destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdStringList.Destroy';
begin
  assert(assigned(self), ASSERT_LOCATION + ': self is nil');
  Clear;
  IdObjectDeregister(self, FSerialNo);
  inherited Destroy;
end;

procedure TIdStringList.Clear;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdStringList.Clear';
var
  i: Integer;
  LObj: TObject;
begin
  assert(assigned(self), ASSERT_LOCATION + ': self is nil');
  if FOwnsObjects then
    begin
    for i := Count - 1 downto 0 do
      begin
      LObj := objects[i];
      objects[i] := NIL;
      Delete(i);
      FreeAndNil(LObj);
      end;
    end;
  inherited Clear;
end;

procedure TIdStringList.Delete(AIndex: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdStringList.Delete';
begin
  assert(assigned(self), ASSERT_LOCATION + ': self is nil');
  // AIndex checked in ancestor
  if FOwnsObjects then
    begin
    Objects[AIndex].Free;    // can't use FreeAndNil
    Objects[AIndex] := NIL;
    end;
  inherited Delete(AIndex);
end;

function TIdStringList.TestValid(AClassType: TClass): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdStringList.TestValid';
begin
  Result := IdObjectTestValid(self);
  if Result and assigned(AClassType) then
    begin
    Result := self is AClassType;
    end;
end;

{ TIdCriticalSection }

constructor TIdCriticalSection.Create;
begin
  inherited Create;
  FSerialNo := IdObjectRegister(self);
  InitializeCriticalSection(FSection);
  FOwnerThread := 0;
  FOwnerThreadCount := 0;
end;

destructor TIdCriticalSection.Destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdCriticalSection.Destroy';
begin
  Assert(Assigned(Self), ASSERT_LOCATION + ': Self is nil');
  IdObjectDeregister(self, FSerialNo);
  DeleteCriticalSection(FSection);
  inherited Destroy;
end;

procedure TIdCriticalSection.Acquire;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdCriticalSection.Acquire';
begin
  Assert(Assigned(Self), ASSERT_LOCATION + ': Self is nil');
  EnterCriticalSection(FSection);
  assert((FOwnerThread = 0) or (FOwnerThread = GetCurrentThreadid));
  if FOwnerThread = GetCurrentThreadid then
    begin
    inc(FOwnerThreadCount);
    end
  else
    begin
    FOwnerThread := GetCurrentThreadId;
    FOwnerThreadCount := 1;
    end;
end;

procedure TIdCriticalSection.Release;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdCriticalSection.Release';
begin
  Assert(Assigned(Self), ASSERT_LOCATION + ': Self is nil');
  Assert(FOwnerThread = GetCurrentThreadID);
  dec(FOwnerThreadCount);
  if FOwnerThreadCount = 0 then
    FOwnerThread := 0;
  LeaveCriticalSection(FSection);
end;

procedure TIdCriticalSection.Enter;
begin
  Acquire;
end;

procedure TIdCriticalSection.Leave;
begin
  Release;
end;

function TIdCriticalSection.LockedToMe: Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdCriticalSection.LockedToMe';
begin
  Assert(Assigned(Self), ASSERT_LOCATION + ': Self is nil');
  Result := FOwnerThread = GetCurrentThreadId;
end;

{ TIdMemoryStream }

class function TIdMemoryStream.Clone(AStream: TStream): TIdMemoryStream;
var
  LPos : Integer;
begin
  if not assigned(AStream) then
    result := nil
  else
    begin
    LPos := AStream.Position;
    result := TIdMemoryStream.create;
    try
      result.CopyFrom(AStream, AStream.Size - AStream.Position);
      result.Position := 0;
      AStream.Position := LPos;
    except
      result.free;
      raise;
    end;
    end;
end;

constructor TIdMemoryStream.Create;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdMemoryStream.create';
begin
  inherited Create;
  FSerialNo := IdObjectRegister(self);
end;

constructor TIdMemoryStream.CreateString(ADataString: AnsiString);
begin
  Create;
  DataString := ADataString;
end;

destructor TIdMemoryStream.Destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdMemoryStream.destroy';
begin
  IdObjectDeregister(self, FSerialNo);
  inherited;
end;

{$IFDEF UNICODE}
function TIdMemoryStream.GetDataBytes: TBytes;
begin
  SetLength(Result, Size);
  if Size > 0 Then
    Move(Memory^, Result[1], Size);
end;
{$ENDIF}

function TIdMemoryStream.GetDataString: AnsiString;
begin
  SetLength(Result, Size);
  if Size > 0 Then
    MoveMemory(@Result[1], Memory, Size);
end;

{$IFDEF UNICODE}
procedure TIdMemoryStream.SetDataBytes(const aValue: TBytes);
var
  pData : Pointer;
begin
  GetMem(pData, Length(aValue));
  if Length(aValue) > 0 then
    Move(AValue[0], pData^, length(aValue));
  SetPointer(pData, Length(aValue));
end;
{$ENDIF}

procedure TIdMemoryStream.SetDataString(const aValue: AnsiString);
var
  pData : Pointer;
begin
  GetMem(pData, Length(aValue));
  if (aValue <> '') then
    Move(AValue[1], pData^, length(aValue));
  SetPointer(pData, Length(aValue));
end;

function TIdMemoryStream.TestValid(AClassType: TClass): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdMemoryStream.TestValid';
begin
  Result := IdObjectTestValid(self);
  if Result and assigned(AClassType) then
    begin
    Result := self is AClassType;
    end;
end;

{ TIdObjectList }

constructor TIdObjectList.Create(AOwnsObjects: Boolean);
begin
  inherited create(AOwnsObjects);
  FSerialNo := IdObjectRegister(self);
end;

destructor TIdObjectList.Destroy;
begin
  IdObjectDeregister(self, FSerialNo);
  inherited;
end;

function TIdObjectList.TestValid(AClassType: TClass): Boolean;
begin
  Result := IdObjectTestValid(self);
  if Result and assigned(AClassType) then
    begin
    Result := self is AClassType;
    end;
end;

{ TKeyListSection }

constructor TIdKeyListSection.Create(AOwner: TIdKeyList);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyListSection.Create';
begin
  inherited Create;
  Assert(Assigned(AOwner), ASSERT_LOCATION+': Owner not assigned in Create');
  FList := AOwner;
  FCount := 0;
  FAllocated := ID_DEFAULT_ALLOCATION_SIZE;
  SetLength(FItems, FAllocated);
  inc(FList.FMemoryUsage, FAllocated * sizeof(TIdKeyPair));
end;

destructor TIdKeyListSection.Destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyListSection.Destroy';
var
  i: Integer;
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Assert(FCount >= 0, ASSERT_LOCATION+': Count cannot be negative');
  Assert(FAllocated >= ID_DEFAULT_ALLOCATION_SIZE, ASSERT_LOCATION+': Illegal size of FAllocated variable ' + IntToStr(FAllocated));
  if assigned(FList.FOnDispose) then
    begin
    for i := 0 to FCount - 1 do
      FList.FOnDispose(FItems[i].FValue);
    end;
  SetLength(FItems, 0);
  Fitems := NIL;
  inherited Destroy;
end;

procedure TIdKeyListSection.AddItem(AKey: Integer; AValue : TObject);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyListSection.AddItem';
var
  i: Integer;
  j: Integer;
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Assert(FCount >= 0, ASSERT_LOCATION+': Count cannot be negative');
  Assert(FAllocated >= ID_DEFAULT_ALLOCATION_SIZE, ASSERT_LOCATION+': Illegal size of FAllocated variable ' + IntToStr(FAllocated));

  if FindItem(Akey, i) then
    begin
    if assigned(FList.FOnDispose) then
      Flist.FOnDispose(Fitems[i].FValue);
    Fitems[i].FValue := AValue;
    end
  else
    begin
    if FCount = FAllocated then
      Grow;
    if I < FCount then
      begin
      for j := FCount - 1 downto i do
        FItems[j + 1] := FItems[j];
      {$IFDEF CLR}
      FItems[i] := TIdKeyPair.Create;
      {$ENDIF}
      end;
    FItems[i].FKey := AKey;
    FItems[i].FValue := AValue;
    Inc(FCount);
    inc(FList.FCount);
    end;
end;

function TIdKeyListSection.FindItem(AKey: Integer; var VIndex: Integer): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyListSection.FindItem';
var
  L, H, I, C: Integer;
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Assert(FCount >= 0, ASSERT_LOCATION+': Count cannot be negative');
  Assert(FAllocated >= ID_DEFAULT_ALLOCATION_SIZE, ASSERT_LOCATION+': Illegal size of FAllocated variable ' + IntToStr(FAllocated));
  VIndex := 0;   // Initialise. Will be set in this function

  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
    begin
    I := (L + H) shr 1;
    C := FItems[I].FKey - AKey;
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

function TIdKeyListSection.GetItem(AKey: Integer; ADefaultValue : TObject): TObject;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyListSection.GetItem';
var
  i: Integer;
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Assert(FCount >= 0, ASSERT_LOCATION+': Count cannot be negative');
  Assert(FAllocated >= ID_DEFAULT_ALLOCATION_SIZE, ASSERT_LOCATION+': Illegal size of FAllocated variable ' + IntToStr(FAllocated));
  // ADefault - values can be any Integer

  if FindItem(AKey, i) then
    begin
    Result := FItems[i].FValue;
    end
  else
    begin
    Result := ADefaultValue;
    end;
end;

procedure TIdKeyListSection.Grow;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyListSection.Grow';
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Assert(FCount >= 0, ASSERT_LOCATION+': Count cannot be negative');
  Assert(FAllocated >= ID_DEFAULT_ALLOCATION_SIZE, ASSERT_LOCATION+': Illegal size of FAllocated variable ' + IntToStr(FAllocated));
  inc(FAllocated, ID_DEFAULT_ALLOCATION_SIZE);
  inc(FList.FMemoryUsage, ID_DEFAULT_ALLOCATION_SIZE * sizeof(TIdKeyPair));
  SetLength(FItems, FAllocated);
end;

procedure TIdKeyListSection.Delete(Akey: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyListSection.Delete';
var
  i: Integer;
  j: Integer;
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Assert(FCount >= 0, ASSERT_LOCATION+': Count cannot be negative');
  Assert(FAllocated >= ID_DEFAULT_ALLOCATION_SIZE, ASSERT_LOCATION+': Illegal size of FAllocated variable ' + IntToStr(FAllocated));

  if FindItem(Akey, i) then
    begin
    if assigned(Flist.FOnDispose) then
      Flist.FOnDispose(Fitems[i].FValue);
    Dec(FCount);
    Dec(Flist.FCount);
    for j := i to FCount - 1 do
      FItems[j] := FItems[j + 1];
    end;
end;

{ TIdKeyList }

constructor TIdKeyList.Create;
begin
  inherited Create;
  init(AHashSize);
  FCount := 0;
end;

procedure TIdKeyList.init(AHashSize: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.init';
var
  i: Integer;
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Assert(AHashSize > 10, ASSERT_LOCATION+': Hash size must be at least 10');

  FHashSize := AHashSize;
  SetLength(FHashTable, FHashSize);
  FMemoryUsage := FHashSize * sizeof(TObject);
  for i := 0 to FHashSize - 1 do
    FHashtable[i] := TIdKeyListSection.Create(self);
end;

destructor TIdKeyList.Destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.Destroy';
var
  i: Integer;
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');

  for i := 0 to FHashSize - 1 do
    FHashtable[i].Free;
  SetLength(FHashTable, 0);
  inherited Destroy;
end;

function TIdKeyList.GetAsInt(AKey: Integer): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.GetAsInt';
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Result := integer(FHashTable[abs(AKey) mod FHashSize].GetItem(AKey, TObject($FFFFFFFF)));
end;

{$IFNDEF CLR}
function TIdKeyList.GetAsPointer(AKey: Integer): pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.GetAsPointer';
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Result := pointer(FHashTable[abs(AKey) mod FHashSize].GetItem(AKey, nil));
end;
{$ENDIF}

function TIdKeyList.GetAsObj(AKey: Integer): TObject;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKeyList.GetAsObj';
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Result := FHashTable[abs(AKey) mod FHashSize].GetItem(AKey, nil);
end;

procedure TIdKeyList.SetAsInt(AKey: Integer; const AValue: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.SetAsInt';
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  FHashTable[abs(AKey) mod FHashSize].AddItem(AKey, TObject(AValue));
end;

{$IFNDEF CLR}
procedure TIdKeyList.SetAsPointer(AKey: Integer; const AValue: pointer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.SetAsPointer';
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  FHashTable[abs(Akey) mod FHashSize].AddItem(Akey, TObject(Avalue));
end;
{$ENDIF}

procedure TIdKeyList.SetAsObj(AKey: Integer; const AValue: TObject);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.SetAsObj';
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  FHashTable[abs(AKey) mod FHashSize].AddItem(AKey, AValue);
end;

function TIdKeyList.GetExists(AKey: Integer): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.GetExists';
var
  LDummy: Integer;
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  Result := FHashTable[abs(AKey) mod FHashSize].FindItem(AKey, LDummy);
end;

procedure TIdKeyList.Delete(AKey: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.Delete';
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  FHashTable[abs(AKey) mod FHashSize].Delete(AKey);
end;

procedure TIdKeyList.Clear;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.Clear';
var
  LP: TIdKeyProgressRec;
  LK: Integer;
begin
  Assert(Assigned(Self), ASSERT_LOCATION+': Try to use uninstantiated instance');
  if GetFirstKey(LP, LK) then
    begin
    Delete(LK);
    while GetNextKey(LP, LK) do
      begin
      Delete(LK);
      end;
    end;
end;

procedure TIdKeyList.DisposeObject(AObj : TObject);
begin
  AObj.Free;
end;

function TIdKeyList.GetFirstKey(var VProgressRec: TIdKeyProgressRec; var VKey: Integer): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.GetFirstKey';
begin
  Assert(Assigned(Self), 'Try to use uninstantiated instance');

  // VProgressRec just a return only. No checking required

  // Leave VKey as is. It could have a value in it that the user will use
  // if this routine returns FALSE. Of course this is not a good thing for
  // the programmer to do. The return value should only be relied on
  // when the function returns TRUE

  VProgressRec.Step1 := -1;
  VProgressRec.Step2 := 0;
  Result := GetNextKey(VProgressRec, VKey);
end;

function TIdKeyList.GetNextKey(var VProgressRec: TIdKeyProgressRec; var VKey: Integer): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdKeyList.GetNextKey';
begin
  Assert(Assigned(Self), 'Try to use uninstantiated instance');

  // VProgressRec just a return only. No checking required

  // Leave VKey as is. It could have a value in it that the user will use
  // if this routine returns FALSE. Of course this is not a good thing for
  // the programmer to do. The return value should only be relied on
  // when the function returns TRUE

  if VProgressRec.Step1 = -1 then
    inc(VProgressRec.Step1)
  else
    inc(VProgressRec.Step2);
  while (VProgressRec.Step1 < FHashSize) and (FHashTable[VProgressRec.Step1].FCount <= VProgressRec.Step2) do
    begin
    inc(VProgressRec.Step1);
    VProgressRec.Step2 := 0;
    end;
  Result := (VProgressRec.Step1 < FHashSize) and (FHashTable[VProgressRec.Step1].FCount > VProgressRec.Step2);
  if Result then
    VKey := FHashTable[VProgressRec.Step1].Fitems[VProgressRec.Step2].FKey;
end;


{ TIdKeyListIterator }

constructor TIdKeyListIterator.Create(AKeyList: TIdKeyList);
begin
  inherited Create;
  FList := AKeyList;
  FCount := 1;
  FMore := FList.GetFirstKey(FRec, FKey);
end;

procedure TIdKeyListIterator.Next;
begin
  inc(FCount);
  FMore := FList.GetNextKey(FRec, FKey);
end;


end.
