unit FHIR.Support.Generics;

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

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes, Types, RTLConsts, {$IFDEF MACOS} FHIR.Support.Osx, {$ELSE} Windows, {$ENDIF} SysUtils, Generics.Collections, Generics.Defaults,
  FHIR.Support.Objects;

Type
  TFslEnumerable<T : class> = class (TFslObject)
  private
  {$HINTS OFF}
    function ToArrayImpl(Count: Integer): TArray<T>; // used by descendants
  {$HINTS ON}
  protected
    function DoGetEnumerator: TEnumerator<T>; virtual; abstract;
  public
    function GetEnumerator: TEnumerator<T>;
    function ToArray: TArray<T>; virtual;
  end;

  TArrayManager<T> = class abstract
  public
    procedure Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer); overload; virtual; abstract;
    procedure Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer); overload; virtual; abstract;
    procedure Finalize(var AArray: array of T; Index, Count: Integer); virtual; abstract;
  end;

  TMoveArrayManager<T> = class(TArrayManager<T>)
  public
    procedure Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Finalize(var AArray: array of T; Index, Count: Integer); override;
  end;

{$IF Defined(WEAKREF)}
  TManualArrayManager<T> = class(TArrayManager<T>)
  public
    procedure Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Finalize(var AArray: array of T; Index, Count: Integer); override;
  end;
{$ENDIF}


  // Actually, T must be TFslObject, but this doesn't work because of forwards class definitions
  TFslList<T : class> = class (TFslEnumerable<T>)
  private
    function GetEmpty: boolean;
  type
    arrayofT = array of T;
    {$IFNDEF FPC}
    TFslListRemoveFunction =  reference to function(item : T):boolean;
    {$ENDIF}
  var
    FItems: arrayofT;
    FCount: Integer;
    FComparer: IComparer<T>;
    FOnNotify: TCollectionNotifyEvent<T>;
    FArrayManager: TArrayManager<T>;
    FEnumFree : boolean;

    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    procedure Grow(ACount: Integer);
    procedure GrowCheck(ACount: Integer); inline;
    procedure DoDelete(Index: Integer; Notification: TCollectionNotification);
    procedure MySort(var Values: array of T; const Comparer: IComparer<T>; Index, Count: Integer);
    procedure QuickSort(var Values: array of T; const Comparer: IComparer<T>; L, R: Integer);
    procedure DoQuickSort(var Values: array of T; const Comparer: IComparer<T>; L, R: Integer);
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure Notify(const Item: T; Action: TCollectionNotification); virtual;
  public
  type
    {$IFNDEF FPC}
    TDirection = System.Types.TDirection;
    TEmptyFunc = reference to function (const L, R: T): Boolean;
    TListCompareFunc = reference to function (const L, R: T): Integer;
    {$ENDIF}

    constructor Create; Overload; Override;
    constructor Create(capacity : integer); Overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(const Collection: TEnumerable<T>); overload;
    destructor Destroy; override;

    function link : TFslList<t>; overload;
    function forEnum : TFslList<t>; // auto frees a collection once an enumerator is finished with it - a commmon pattern

    class procedure Error(const Msg: string; Data: NativeInt); overload; virtual;
{$IFNDEF NEXTGEN}
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
{$ENDIF  NEXTGEN}

    function Add(const Value: T): Integer;

    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload;
    procedure AddRange(const Collection: TEnumerable<T>); overload;

    procedure Insert(Index: Integer; const Value: T);

    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;

    procedure AddAll(list : TFslList<T>);

    procedure Pack; overload;

    function Remove(const Value: T): Integer;
    procedure RemoveAll(list : TFslList<T>); overload;
    {$IFNDEF FPC}
    procedure RemoveAll(filter : TFslListRemoveFunction); overload;
    {$ENDIF}
    function RemoveItem(const Value: T; Direction: TDirection): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function Extract(const Value: T): T;
    function ExtractItem(const Value: T; Direction: TDirection): T;

    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Replace(old, new : T);

    function First: T;
    function Last: T;

    procedure Clear;

    function Expand: TFslList<T>;

    function Contains(const Value: T): Boolean;
    function IndexOf(const Value: T): Integer;
    function IndexOfItem(const Value: T; Direction: TDirection): Integer;
    function LastIndexOf(const Value: T): Integer;

    procedure Reverse;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(const Item: T; out Index: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload;
    function matches(other : TFslList<T>; ordered : boolean; criteria : IComparer<T>) : boolean;

    procedure TrimExcess;

    function ToArray: TArray<T>; override; final;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Empty : boolean read GetEmpty;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property List: arrayofT read FItems;

    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;

    type
      TFslEnumerator = class(TEnumerator<T>)
      private
        FList: TFslList<T>;
        FIndex: Integer;
        function GetCurrent: T;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AList: TFslList<T>);
        destructor Destroy; override;
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;

    function GetEnumerator: TFslEnumerator; reintroduce;
  end;

  TFslPair<T : TFslObject> = record
    Key: String;
    Value: T;
    constructor Create(const AKey: String; const AValue: T);
  end;

  TFslMap<T : TFslObject> = class(TEnumerable<TFslPair<T>>)
  private
    type
      TItem = record
        HashCode: Integer;
        Key: String;
        Value: T;
      end;
      TItemArray = array of TItem;
  private
    FFslObjectReferenceCount : TFslReferenceCount;
    FItems: TItemArray;
    FCount: Integer;
    FGrowThreshold: Integer;
    FSortedKeys : TStringList;

    procedure SetCapacity(ACapacity: Integer);
    procedure Rehash(NewCapPow2: Integer);
    procedure Grow;
    function GetBucketIndex(const Key: String; HashCode: Integer): Integer;
    function Hash(const Key: String): Integer;
    function GetItem(const Key: String): T;
    procedure SetItem(const Key: String; const Value: T);
    procedure RehashAdd(HashCode: Integer; const Key: String; const Value: T);
    procedure DoAdd(HashCode, Index: Integer; const Key: String; const Value: T);
    procedure DoSetValue(Index: Integer; const Value: T);
    function DoRemove(const Key: String; HashCode: Integer; Notification: TCollectionNotification): T;
    function InCircularRange(Bottom, Item, TopInc: Integer): Boolean;
  private
    function GetEmpty: Boolean;
  protected
    function DoGetEnumerator: TEnumerator<TFslPair<T>>; override;
    procedure KeyNotify(const Key: String; Action: TCollectionNotification); virtual;
    procedure ValueNotify(const Value: T; Action: TCollectionNotification); virtual;
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(const Collection: TEnumerable<TFslPair<T>>); overload;
    destructor Destroy; override;
    function Link : TFslMap<T>; overload;
    Procedure Free; Overload;

    procedure Add(const Key: String; const Value: T);
    procedure Remove(const Key: String);
    procedure Clear;
    procedure TrimExcess;
    function TryGetValue(const Key: String; out Value: T): Boolean;
    procedure AddOrSetValue(const Key: String; const Value: T);
    function ContainsKey(const Key: String): Boolean;
    function ContainsValue(const Value: T): Boolean;
    function ToArray: TArray<TFslPair<T>>; override; final;

    procedure addAll(other : TFslMap<T>);
    property Items[const Key: String]: T read GetItem write SetItem; default;
    property Count: Integer read FCount;
    property IsEmpty : Boolean read GetEmpty;

    type
      TFslPairEnumerator = class(TEnumerator<TFslPair<T>>)
      private
        FMap: TFslMap<T>;
        FIndex: Integer;
        function GetCurrent: TFslPair<T>;
      protected
        function DoGetCurrent: TFslPair<T>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMap: TFslMap<T>);
        property Current: TFslPair<T> read GetCurrent;
        function MoveNext: Boolean;
      end;

      TKeyEnumerator = class(TEnumerator<String>)
      private
        FMap: TFslMap<T>;
        FIndex: Integer;
        function GetCurrent: String;
      protected
        function DoGetCurrent: String; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMap: TFslMap<T>);
        property Current: String read GetCurrent;
        function MoveNext: Boolean;
      end;

      TValueEnumerator = class(TEnumerator<T>)
      private
        FMap: TFslMap<T>;
        FIndex: Integer;
        function GetCurrent: T;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMap: TFslMap<T>);
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;

      TValueCollection = class(TEnumerable<T>)
      private
        FMap: TFslMap<T>;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<T>; override;
      public
        constructor Create(const AMap: TFslMap<T>);
        function GetEnumerator: TValueEnumerator; reintroduce;
        function ToArray: TArray<T>; override; final;
        property Count: Integer read GetCount;
      end;

      TKeyCollection = class(TEnumerable<String>)
      private
        FMap: TFslMap<T>;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<String>; override;
      public
        constructor Create(const AMap: TFslMap<T>);
        function GetEnumerator: TKeyEnumerator; reintroduce;
        function ToArray: TArray<String>; override; final;
        property Count: Integer read GetCount;
      end;

  private
    FOnKeyNotify: TCollectionNotifyEvent<String>;
    FOnValueNotify: TCollectionNotifyEvent<T>;
    FKeyCollection: TKeyCollection;
    FValueCollection: TValueCollection;
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
    function GetSortedKeys: TStringList;
  public
    function GetEnumerator: TFslPairEnumerator; reintroduce;
    property Keys: TKeyCollection read GetKeys;
    property SortedKeys : TStringList read GetSortedKeys;
    property Values: TValueCollection read GetValues;
    property OnKeyNotify: TCollectionNotifyEvent<String> read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<T> read FOnValueNotify write FOnValueNotify;
  end;

  TFslStringDictionary = class (TDictionary<String, String>)
  private
    FFslObjectReferenceCount : TFslReferenceCount;
  public
    // Cannot be virtual as they are allowed to be called from Nil or invalid objects (but will assert).
    Procedure Free; Overload;
    Function Link : TFslStringDictionary; Overload;
  end;

  TFslStringSet = class (TFslObject)
  private
    // not sorted - this doesn't get long enough to make it worth sorting
    FItems : TArray<String>;
  public
    Constructor Create(initial : String); overload;
    Constructor Create(initial : array of String); overload;
    Constructor Create(c1, c2 : TFslStringSet); overload;
    Destructor Destroy; override;
    function Link : TFslStringSet; overload;

    procedure addAll(collection : TFslStringSet);
    procedure add(value : String);

    function contains(s : String) : boolean;
    procedure remove(s : String);
    function isEmpty : boolean;
    function ToString : String; override;
    function AsString(sep : String = ', ') : String;

    type
      TFslStringSetEnumerator = class(TEnumerator<string>)
      private
        FSet: TFslStringSet;
        FIndex: Integer;
        function GetCurrent: string;
      protected
        function DoGetCurrent: string; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const aSet: TFslStringSet);
        property Current: String read GetCurrent;
        function MoveNext: Boolean;
      end;

    function GetEnumerator: TFslStringSetEnumerator;
  end;

  TFslStringMap = class (TFslObject)
  private
    FDict : TDictionary<String, String>;
    function GetItem(const Key: String): String;
    procedure SetItem(const Key, Value: String);
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TFslStringMap;
    property Items[const Key: String]: String read GetItem write SetItem; default;

  end;

implementation

{ TFslEnumerable<T> }

function TFslEnumerable<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := DoGetEnumerator;
end;

function TFslEnumerable<T>.ToArray: TArray<T>;
var
  buf: TFslList<T>;
  x: T;
begin
  buf := TFslList<T>.Create;
  try
    for x in Self do
      buf.Add(TFslObject(x).Link);
    Result := buf.ToArray; // relies on TList<T>.ToArray override
  finally
    buf.Free;
  end;
end;

function TFslEnumerable<T>.ToArrayImpl(Count: Integer): TArray<T>;
var
  x: T;
begin
  // We assume our caller has passed correct Count
  SetLength(Result, Count);
  Count := 0;
  for x in Self do
  begin
    TObject(Result[Count]) := TFslObject(x).Link;
    Inc(Count);
  end;
end;

{ TFslList<T> }

function TFslList<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

procedure TFslList<T>.SetCapacity(Value: Integer);
begin
  if Value < Count then
    Count := Value;
  SetLength(FItems, Value);
end;

procedure TFslList<T>.SetCount(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > Capacity then
    SetCapacity(Value);
  if Value < Count then
    DeleteRange(Value, Count - Value);
  FCount := Value;
end;

function TFslList<T>.GetItem(Index: Integer): T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := FItems[Index];
end;

procedure TFslList<T>.SetItem(Index: Integer; const Value: T);
var
  oldItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  oldItem := FItems[Index];
  FItems[Index] := Value;
  try
    Notify(oldItem, cnRemoved);
    Notify(Value, cnAdded);
  finally
    TFslObject(oldItem).free;
  end;
end;

procedure TFslList<T>.Grow(ACount: Integer);
var
  newCount: Integer;
begin
  newCount := Length(FItems);
  if newCount = 0 then
    newCount := ACount
  else
    repeat
      newCount := newCount * 2;
      if newCount < 0 then
        OutOfMemoryError;
    until newCount >= ACount;
  Capacity := newCount;
end;

procedure TFslList<T>.GrowCheck(ACount: Integer);
begin
  if ACount > Length(FItems) then
    Grow(ACount)
  else if ACount < 0 then
    OutOfMemoryError;
end;

procedure TFslList<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

procedure TFslList<T>.Pack;
var
  PackedCount : Integer;
  StartIndex : Integer;
  EndIndex : Integer;
begin
  if FCount = 0 then
    Exit;

  PackedCount := 0;
  StartIndex := 0;
  repeat
    // Locate the first/next non-nil element in the list
//    while (StartIndex < FCount) and (FComparer.Compare(FItems[StartIndex], Default(T)) = 0) do
    while (StartIndex < FCount) and (FItems[StartIndex] = nil) do
      Inc(StartIndex);

    if StartIndex < FCount then // There is nothing more to do
    begin
      // Locate the next nil pointer
      EndIndex := StartIndex;
//      while (EndIndex < FCount) and (FComparer.Compare(FItems[EndIndex], Default(T)) <> 0) do
      while (EndIndex < FCount) and (FItems[EndIndex] <> nil) do
        Inc(EndIndex);
      Dec(EndIndex);

      // Move this block of non-null items to the index recorded in PackedToCount:
      // If this is a contiguous non-nil block at the start of the list then
      // StartIndex and PackedToCount will be equal (and 0) so don't bother with the move.
      if StartIndex > PackedCount then
        FArrayManager.Move(FItems, StartIndex, PackedCount, EndIndex - StartIndex + 1);

      // Set the PackedToCount to reflect the number of items in the list
      // that have now been packed.
      Inc(PackedCount, EndIndex - StartIndex + 1);

      // Reset StartIndex to the element following EndIndex
      StartIndex := EndIndex + 1;
    end;
  until StartIndex >= FCount;

  // Set Count so that the 'free' item
  FCount := PackedCount;
end;

constructor TFslList<T>.Create;
begin
  Create(TComparer<T>.Default);
end;

constructor TFslList<T>.Create(const AComparer: IComparer<T>);
begin
  inherited Create;
  FArrayManager := TMoveArrayManager<T>.Create;
  FComparer := AComparer;
  if FComparer = nil then
    FComparer := TComparer<T>.Default;
end;

constructor TFslList<T>.Create(const Collection: TEnumerable<T>);
begin
  inherited Create;
  FArrayManager := TMoveArrayManager<T>.Create;
  FComparer := TComparer<T>.Default;
  InsertRange(0, Collection);
end;

constructor TFslList<T>.Create(capacity: integer);
begin
  Create(TComparer<T>.Default);
  self.Capacity := capacity;
end;

destructor TFslList<T>.Destroy;
begin
// Clear method only sets lenght to 0, does not destroy any objects, does it?
// Is the sequence below ok?
  Clear;
  FArrayManager.Free;
  inherited;
end;

class procedure TFslList<T>.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddress;
end;

class procedure TFslList<T>.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) at ReturnAddress;
end;

function TFslList<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TFslList<T>.Add(const Value: T): Integer;
begin
  GrowCheck(Count + 1);
  Result := Count;
  FItems[Count] := Value; // .link - no link here - the link has to be external, because the consumer of the list has to decide that the list owns the object
  Inc(FCount);
  Notify(Value, cnAdded);
end;

procedure TFslList<T>.AddRange(const Values: array of T);
begin
  InsertRange(Count, Values);
end;

procedure TFslList<T>.AddRange(const Collection: IEnumerable<T>);
begin
  InsertRange(Count, Collection);
end;

procedure TFslList<T>.AddAll(list: TFslList<T>);
var
  item: T;
begin
  for item in list do
    Add(TFslObject(item).link); // yes we link here too
end;

procedure TFslList<T>.AddRange(const Collection: TEnumerable<T>);
begin
  InsertRange(Count, Collection);
end;

function TFslList<T>.BinarySearch(const Item: T; out Index: Integer): Boolean;
begin
  Result := TArray.BinarySearch<T>(FItems, Item, Index, FComparer, 0, Count);
end;

function TFslList<T>.BinarySearch(const Item: T; out Index: Integer;
  const AComparer: IComparer<T>): Boolean;
begin
  Result := TArray.BinarySearch<T>(FItems, Item, Index, AComparer, 0, Count);
end;

procedure TFslList<T>.Insert(Index: Integer; const Value: T);
begin
  if (Index < 0) or (Index > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  GrowCheck(Count + 1);
  if Index <> Count then
  begin
    FArrayManager.Move(FItems, Index, Index + 1, Count - Index);
    FArrayManager.Finalize(FItems, Index, 1);
  end;
  FItems[Index] := Value; // .link - no, see above
  Inc(FCount);
  Notify(Value, cnAdded);
end;

procedure TFslList<T>.InsertRange(Index: Integer; const Values: array of T);
var
  I: Integer;
begin
  if (Index < 0) or (Index > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  GrowCheck(Count + Length(Values));
  if Index <> Count then
  begin
    FArrayManager.Move(FItems, Index, Index + Length(Values), Count - Index);
    FArrayManager.Finalize(FItems, Index, Length(Values));
  end;

  for I := 0 to Length(Values) - 1 do
    TObject(FItems[Index + I]) := TFslObject(Values[I]).Link; // yes, here we link. This means that the user cannot construct an array of objects and link them assuming this will respect that

  Inc(FCount, Length(Values));

  for I := 0 to Length(Values) - 1 do
    Notify(Values[I], cnAdded);
end;

procedure TFslList<T>.InsertRange(Index: Integer; const Collection: IEnumerable<T>);
var
  item: T;
begin
  for item in Collection do
  begin
    Insert(Index, TFslObject(item).link); // yes we link here too
    Inc(Index);
  end;
end;

procedure TFslList<T>.InsertRange(Index: Integer; const Collection: TEnumerable<T>);
var
  item: T;
begin
  for item in Collection do
  begin
    Insert(Index, TFslObject(item).Link);
    Inc(Index);
  end;
end;

procedure TFslList<T>.Exchange(Index1, Index2: Integer);
var
  temp: T;
begin
  temp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := temp;
end;

function TFslList<T>.Extract(const Value: T): T;
begin
  Result := ExtractItem(Value, TDirection.FromBeginning);
end;

function TFslList<T>.ExtractItem(const Value: T; Direction: TDirection): T;
var
  index: Integer;
begin
  index := IndexOfItem(Value, Direction);
  if index < 0 then
    Result := nil
  else
  begin
    Result := FItems[index];
    DoDelete(index, cnExtracted);
  end;
end;

function TFslList<T>.First: T;
begin
  Result := Items[0];
end;

function TFslList<T>.forEnum: TFslList<t>;
begin
  FEnumFree := true;
  result := self;
end;

function TFslList<T>.Remove(const Value: T): Integer;
begin
  Result := IndexOf(Value);
  if Result >= 0 then
    Delete(Result);
end;

procedure TFslList<T>.RemoveAll(filter: TFslListRemoveFunction);
var
  i : integer;
begin
  for i := Count - 1 downto 0 do
    if (filter(Items[i])) then
      Delete(i);
end;

procedure TFslList<T>.RemoveAll(list: TFslList<T>);
var
  item: T;
begin
  for item in list do
    Remove(TFslObject(item));
end;

function TFslList<T>.RemoveItem(const Value: T; Direction: TDirection): Integer;
begin
  Result := IndexOfItem(Value, Direction);
  if Result >= 0 then
    Delete(Result);
end;

procedure TFslList<T>.Replace(old, new: T);
var
  i : integer;
begin
  i := IndexOf(old);
  if i = -1 then
    raise Exception.Create('Item not found to delete');
  Insert(i, new);
  Delete(i+1);
end;

procedure TFslList<T>.DoDelete(Index: Integer; Notification: TCollectionNotification);
var
  oldItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  oldItem := FItems[Index];
  FItems[Index] := Default(T);
  Dec(FCount);
  if Index <> Count then
  begin
    FArrayManager.Move(FItems, Index + 1, Index, Count - Index);
    FArrayManager.Finalize(FItems, Count, 1);
  end;
  try
    Notify(oldItem, Notification);
  finally
    TFslObject(oldItem).free;
  end;
end;

procedure TFslList<T>.Delete(Index: Integer);
begin
  DoDelete(Index, cnRemoved);
end;

procedure TFslList<T>.DeleteRange(AIndex, ACount: Integer);
var
  oldItems: array of T;
  tailCount, I: Integer;
begin
  if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > Count)
    or (AIndex + ACount < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if ACount = 0 then
    Exit;

  SetLength(oldItems, ACount);
  FArrayManager.Move(FItems, oldItems, AIndex, 0, ACount);
  try
    tailCount := Count - (AIndex + ACount);
    if tailCount > 0 then
    begin
      FArrayManager.Move(FItems, AIndex + ACount, AIndex, tailCount);
      FArrayManager.Finalize(FItems, Count - ACount, ACount);
    end else
      FArrayManager.Finalize(FItems, AIndex, ACount);

    Dec(FCount, ACount);

    for I := 0 to Length(oldItems) - 1 do
      Notify(oldItems[I], cnRemoved);
  finally
    for I := 0 to Length(oldItems) - 1 do
      TFslObject(oldItems[I]).free;
  end;
end;

procedure TFslList<T>.Clear;
begin
  Count := 0;
  Capacity := 0;
end;

function TFslList<T>.Expand: TFslList<T>;
begin
  if FCount = Length(FItems) then
    GrowCheck(FCount + 1);
  Result := Self;
end;

function TFslList<T>.Contains(const Value: T): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

function TFslList<T>.IndexOf(const Value: T): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if FComparer.Compare(FItems[i], Value) = 0 then
      Exit(i);
  Result := -1;
end;

function TFslList<T>.IndexOfItem(const Value: T; Direction: TDirection): Integer;
var
  P: T;
  i: Integer;
begin
  if Direction = TDirection.FromBeginning then
    Result := IndexOf(Value)
  else
  begin
    if Count > 0 then
    begin
      for i := Count - 1 downto 0 do
        if FComparer.Compare(FItems[i], Value) = 0 then
          Exit(i);
    end;
    Result := -1;
  end;
end;

function TFslList<T>.Last: T;
begin
  Result := Items[Count - 1];
end;

function TFslList<T>.LastIndexOf(const Value: T): Integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if FComparer.Compare(FItems[i], Value) = 0 then
      Exit(i);
  Result := -1;
end;

function TFslList<T>.link: TFslList<t>;
begin
  result := TFslList<T>(inherited Link);
end;

function TFslList<T>.matches(other: TFslList<T>; ordered: boolean; criteria: IComparer<T>): boolean;
var
  i, j : integer;
  ok : boolean;
begin
  if other = nil then
    exit(false);
  if count <> other.Count then
    exit(false);
  result := true;
  if ordered then
  begin
    for i := 0 to Count - 1 do
      if criteria.Compare(Items[i], other[i]) <> 0 then
        exit(false);
  end
  else
  begin
    for i := 0 to Count - 1 do
    begin
      ok := false;
      for j := 0 to Count - 1 do
        if criteria.Compare(Items[i], other[j]) = 0 then
        begin
          ok := true;
          break;
        end;
      if not ok then
        exit(false);
    end;
  end;
end;

procedure TFslList<T>.Move(CurIndex, NewIndex: Integer);
var
  temp: T;
begin
  if CurIndex = NewIndex then
    Exit;
  if (NewIndex < 0) or (NewIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  temp := FItems[CurIndex];
  FItems[CurIndex] := Default(T);
  if CurIndex < NewIndex then
    FArrayManager.Move(FItems, CurIndex + 1, CurIndex, NewIndex - CurIndex)
  else
    FArrayManager.Move(FItems, NewIndex, NewIndex + 1, CurIndex - NewIndex);

  FArrayManager.Finalize(FItems, NewIndex, 1);
  FItems[NewIndex] := temp;
end;

procedure TFslList<T>.DoQuickSort(var Values: array of T; const Comparer: IComparer<T>; L, R: Integer);
Var
  I, J, K : Integer;
  temp : T;
Begin
  Repeat
    I := L;
    J := R;
    K := (L + R) Shr 1;

    Repeat
      While Comparer.Compare(Values[I], Values[K]) > 0 Do
        Inc(I);

      While Comparer.Compare(Values[J], Values[K]) < 0 Do
        Dec(J);

      If I <= J Then
      Begin
        temp := Values[i];
        Values[i] := Values[j];
        Values[j] := temp;

        // Keep K as the index of the original middle element as it might get exchanged.
        If I = K Then
          K := J
        Else If J = K Then
          K := I;

        Inc(I);
        Dec(J);
      End;
    Until I > J;

    If L < J Then
      DoQuickSort(Values, Comparer, L, J);

    L := I;
  Until I >= R;
End;

procedure TFslList<T>.QuickSort(var Values: array of T; const Comparer: IComparer<T>; L, R: Integer);
Begin
  If R-L > 1 Then
    DoQuickSort(Values, Comparer, l, R);
end;


procedure TFslList<T>.MySort(var Values: array of T; const Comparer: IComparer<T>; Index, Count: Integer);
begin
  if Count <= 1 then
    Exit;
  QuickSort(Values, Comparer, Index, Index + Count - 1);
end;

procedure TFslList<T>.Reverse;
var
  tmp: T;
  b, e: Integer;
begin
  b := 0;
  e := Count - 1;
  while b < e do
  begin
    tmp := FItems[b];
    FItems[b] := FItems[e];
    FItems[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TFslList<T>.Sort;
begin
  MySort(FItems, FComparer, 0, Count);
end;

procedure TFslList<T>.Sort(const AComparer: IComparer<T>);
begin
  MySort(FItems, AComparer, 0, Count);
end;

// no ownership on the array - it cannot be kept alive after the list is freed
function TFslList<T>.ToArray: TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Items[i];
end;

procedure TFslList<T>.TrimExcess;
begin
  Capacity := Count;
end;

function TFslList<T>.GetEmpty: boolean;
begin
  result := Count = 0;
end;

function TFslList<T>.GetEnumerator: TFslEnumerator;
begin
  Result := TFslEnumerator.Create(Self);
end;

{ TFslList<T>.TFslEnumerator }

constructor TFslList<T>.TFslEnumerator.Create(const AList: TFslList<T>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

destructor TFslList<T>.TFslEnumerator.Destroy;
begin
  if FList.FEnumFree then
    FList.Free;
  inherited;
end;

function TFslList<T>.TFslEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TFslList<T>.TFslEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TFslList<T>.TFslEnumerator.GetCurrent: T;
begin
  Result := FList[FIndex];
end;

function TFslList<T>.TFslEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TFslPair<T> }

constructor TFslPair<T>.Create(const AKey: String; const AValue: T);
begin
  Key := AKey;
  Value := AValue;
end;

{ TFslMap<T> }
const
  EMPTY_HASH = -1;

procedure TFslMap<T>.Rehash(NewCapPow2: Integer);
var
  oldItems, newItems: TItemArray;
  i: Integer;
begin
  if NewCapPow2 = Length(FItems) then
    Exit
  else if NewCapPow2 < 0 then
    OutOfMemoryError;

  oldItems := FItems;
  SetLength(newItems, NewCapPow2);
  for i := 0 to Length(newItems) - 1 do
    newItems[i].HashCode := EMPTY_HASH;
  FItems := newItems;
  FGrowThreshold := NewCapPow2 shr 1 + NewCapPow2 shr 2; // 75%

  for i := 0 to Length(oldItems) - 1 do
    if oldItems[i].HashCode <> EMPTY_HASH then
      RehashAdd(oldItems[i].HashCode, oldItems[i].Key, oldItems[i].Value);
end;

procedure TFslMap<T>.SetCapacity(ACapacity: Integer);
var
  newCap: Integer;
begin
  if ACapacity < Count then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if ACapacity = 0 then
    Rehash(0)
  else
  begin
    newCap := 4;
    while newCap < ACapacity do
      newCap := newCap shl 1;
    Rehash(newCap);
  end
end;

procedure TFslMap<T>.Grow;
var
  newCap: Integer;
begin
  newCap := Length(FItems) * 2;
  if newCap = 0 then
    newCap := 4;
  Rehash(newCap);
end;

function TFslMap<T>.GetBucketIndex(const Key: String; HashCode: Integer): Integer;
var
  start, hc: Integer;
begin
  if Length(FItems) = 0 then
    Exit(not High(Integer));

  start := HashCode and (Length(FItems) - 1);
  Result := start;
  while True do
  begin
    hc := FItems[Result].HashCode;

    // Not found: return complement of insertion point.
    if hc = EMPTY_HASH then
      Exit(not Result);

    // Found: return location.
    if (hc = HashCode) and (FItems[Result].Key = Key) then
      Exit(Result);

    Inc(Result);
    if Result >= Length(FItems) then
      Result := 0;
  end;
end;

function TFslMap<T>.GetItem(const Key: String): T;
var
  index: Integer;
begin
  index := GetBucketIndex(Key, Hash(Key));
  if index < 0 then
    raise EListError.CreateRes(@SGenericItemNotFound);
  Result := FItems[index].Value;
end;

procedure TFslMap<T>.SetItem(const Key: String; const Value: T);
var
  index: Integer;
  oldValue: T;
begin
  index := GetBucketIndex(Key, Hash(Key));
  if index < 0 then
    raise EListError.CreateRes(@SGenericItemNotFound);

  oldValue := FItems[index].Value;
  FItems[index].Value := Value;
  try
    ValueNotify(oldValue, cnRemoved);
    ValueNotify(Value, cnAdded);
  finally
    TFslObject(oldValue).free;
  end;
end;

procedure TFslMap<T>.RehashAdd(HashCode: Integer; const Key: String; const Value: T);
var
  index: Integer;
begin
  index := not GetBucketIndex(Key, HashCode);
  FItems[index].HashCode := HashCode;
  FItems[index].Key := Key;
  FItems[index].Value := Value;
end;

procedure TFslMap<T>.KeyNotify(const Key: String; Action: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, Key, Action);
end;

function TFslMap<T>.Link: TFslMap<T>;
begin
  Result := Self;

  If Assigned(Self) Then
    InterlockedIncrement(FFslObjectReferenceCount);
end;

procedure TFslMap<T>.ValueNotify(const Value: T; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, Value, Action);
end;

constructor TFslMap<T>.Create(ACapacity: Integer = 0);
begin
  inherited Create;
  if ACapacity < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  SetCapacity(ACapacity);
end;

constructor TFslMap<T>.Create(const Collection: TEnumerable<TFslPair<T>>);
var
  item: TFslPair<T>;
begin
  Create(0);
  for item in Collection do
    AddOrSetValue(item.Key, item.Value);
end;

destructor TFslMap<T>.Destroy;
begin
  Clear;
  FKeyCollection.Free;
  FValueCollection.Free;
  FSortedKeys.Free;
  inherited;
end;

procedure TFslMap<T>.Add(const Key: String; const Value: T);
var
  index, hc: Integer;
begin
  if Count >= FGrowThreshold then
    Grow;

  hc := Hash(Key);
  index := GetBucketIndex(Key, hc);
  if index >= 0 then
    raise EListError.CreateRes(@SGenericDuplicateItem);

  DoAdd(hc, not index, Key, Value);
end;

function TFslMap<T>.InCircularRange(Bottom, Item, TopInc: Integer): Boolean;
begin
  Result := (Bottom < Item) and (Item <= TopInc) // normal
    or (TopInc < Bottom) and (Item > Bottom) // top wrapped
    or (TopInc < Bottom) and (Item <= TopInc) // top and item wrapped
end;

function TFslMap<T>.DoRemove(const Key: String; HashCode: Integer; Notification: TCollectionNotification): T;
var
  gap, index, hc, bucket: Integer;
begin
  index := GetBucketIndex(Key, HashCode);
  if index < 0 then
    Exit(Default(T));

  // Removing item from linear probe hash table is moderately
  // tricky. We need to fill in gaps, which will involve moving items
  // which may not even hash to the same location.
  // Knuth covers it well enough in Vol III. 6.4.; but beware, Algorithm R
  // (2nd ed) has a bug: step R4 should go to step R1, not R2 (already errata'd).
  // My version does linear probing forward, not backward, however.

  // gap refers to the hole that needs filling-in by shifting items down.
  // index searches for items that have been probed out of their slot,
  // but being careful not to move items if their bucket is between
  // our gap and our index (so that they'd be moved before their bucket).
  // We move the item at index into the gap, whereupon the new gap is
  // at the index. If the index hits a hole, then we're done.

  // If our load factor was exactly 1, we'll need to hit this hole
  // in order to terminate. Shouldn't normally be necessary, though.
  FItems[index].HashCode := EMPTY_HASH;
  Result := FItems[index].Value;

  gap := index;
  while True do
  begin
    Inc(index);
    if index = Length(FItems) then
      index := 0;

    hc := FItems[index].HashCode;
    if hc = EMPTY_HASH then
      Break;

    bucket := hc and (Length(FItems) - 1);
    if not InCircularRange(gap, bucket, index) then
    begin
      FItems[gap] := FItems[index];
      gap := index;
      // The gap moved, but we still need to find it to terminate.
      FItems[gap].HashCode := EMPTY_HASH;
    end;
  end;

  FItems[gap].HashCode := EMPTY_HASH;
  FItems[gap].Key := Default(String);
  FItems[gap].Value := Default(T);
  Dec(FCount);

  FreeAndNil(FSortedKeys);
  KeyNotify(Key, Notification);
  ValueNotify(Result, Notification);
end;

procedure TFslMap<T>.Remove(const Key: String);
begin
  TFslObject(DoRemove(Key, Hash(Key), cnRemoved)).Free;
end;

procedure TFslMap<T>.Clear;
var
  i: Integer;
  oldItems: TItemArray;
begin
  oldItems := FItems;
  FCount := 0;
  SetLength(FItems, 0);
  SetCapacity(0);
  FGrowThreshold := 0;

  for i := 0 to Length(oldItems) - 1 do
  begin
    if oldItems[i].HashCode = EMPTY_HASH then
      Continue;
    KeyNotify(oldItems[i].Key, cnRemoved);
    ValueNotify(oldItems[i].Value, cnRemoved);
    TFslObject(oldItems[i].Value).free;
  end;
end;

function TFslMap<T>.ToArray: TArray<TFslPair<T>>;
begin
  raise Exception.Create('unimplemented');
//  result := ToArrayImpl(Count);
end;

procedure TFslMap<T>.TrimExcess;
begin
  // Ensure at least one empty slot for GetBucketIndex to terminate.
  SetCapacity(Count + 1);
end;

function TFslMap<T>.TryGetValue(const Key: String; out Value: T): Boolean;
var
  index: Integer;
begin
  index := GetBucketIndex(Key, Hash(Key));
  Result := index >= 0;
  if Result then
    Value := FItems[index].Value
  else
    Value := nil;
end;

procedure TFslMap<T>.DoAdd(HashCode, Index: Integer; const Key: String; const Value: T);
begin
  FItems[Index].HashCode := HashCode;
  FItems[Index].Key := Key;
  FItems[Index].Value := Value;
  Inc(FCount);

  FreeAndNil(FSortedKeys);
  KeyNotify(Key, cnAdded);
  ValueNotify(Value, cnAdded);
end;

function TFslMap<T>.DoGetEnumerator: TEnumerator<TFslPair<T>>;
begin
  Result := GetEnumerator;
end;

procedure TFslMap<T>.DoSetValue(Index: Integer; const Value: T);
var
  oldValue: T;
begin
  oldValue := FItems[Index].Value;
  FItems[Index].Value := Value;

  ValueNotify(oldValue, cnRemoved);
  ValueNotify(Value, cnAdded);
  TFslObject(oldValue).Free;
end;

procedure TFslMap<T>.Free;
begin
  If Assigned(Self) and (InterlockedDecrement(FFslObjectReferenceCount) < 0) Then
    Destroy;
end;


procedure TFslMap<T>.addAll(other: TFslMap<T>);
var
  s : String;
begin
  for s in other.Keys do
    AddOrSetValue(s, other[s].link);
end;

procedure TFslMap<T>.AddOrSetValue(const Key: String; const Value: T);
var
  hc: Integer;
  index: Integer;
begin
  if Count >= FGrowThreshold then
    Grow;
  hc := Hash(Key);
  index := GetBucketIndex(Key, hc);
  if index >= 0 then
    DoSetValue(index, Value)
  else
    DoAdd(hc, not index, Key, Value);
end;


function TFslMap<T>.ContainsKey(const Key: String): Boolean;
begin
  Result := GetBucketIndex(Key, Hash(Key)) >= 0;
end;

function TFslMap<T>.ContainsValue(const Value: T): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(FItems) - 1 do
    if (FItems[i].HashCode <> EMPTY_HASH) and (FItems[i].Value = Value) then
      Exit(True);
  Result := False;
end;

function TFslMap<T>.GetEmpty: Boolean;
begin
  result := Count = 0;
end;

function TFslMap<T>.GetEnumerator: TFslPairEnumerator;
begin
  Result := TFslPairEnumerator.Create(Self);
end;

function TFslMap<T>.GetKeys: TKeyCollection;
begin
  if FKeyCollection = nil then
    FKeyCollection := TKeyCollection.Create(Self);
  Result := FKeyCollection;
end;

function TFslMap<T>.GetSortedKeys: TStringList;
var
  p : TFslPair<T>;
begin
  if FSortedKeys = nil then
  begin
    FSortedKeys := TStringList.Create;
    for p in self do
      FSortedKeys.AddObject(p.Key, p.Value);
    FSortedKeys.Sort;
  end;
  result := FSortedKeys;
end;

function TFslMap<T>.GetValues: TValueCollection;
begin
  if FValueCollection = nil then
    FValueCollection := TValueCollection.Create(Self);
  Result := FValueCollection;
end;

// Pairs

constructor TFslMap<T>.TFslPairEnumerator.Create(const AMap: TFslMap<T>);
begin
  inherited Create;
  FIndex := -1;
  FMap := AMap;
end;

function TFslMap<T>.TFslPairEnumerator.DoGetCurrent: TFslPair<T>;
begin
  Result := GetCurrent;
end;

function TFslMap<T>.TFslPairEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TFslMap<T>.TFslPairEnumerator.GetCurrent: TFslPair<T>;
begin
  Result.Key := FMap.FItems[FIndex].Key;
  Result.Value := FMap.FItems[FIndex].Value;
end;

function TFslMap<T>.TFslPairEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FMap.FItems) - 1 do
  begin
    Inc(FIndex);
    if FMap.FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

// Keys

constructor TFslMap<T>.TKeyEnumerator.Create(const AMap : TFslMap<T>);
begin
  inherited Create;
  FIndex := -1;
  FMap := AMap;
end;

function TFslMap<T>.TKeyEnumerator.DoGetCurrent: String;
begin
  Result := GetCurrent;
end;

function TFslMap<T>.TKeyEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TFslMap<T>.TKeyEnumerator.GetCurrent: String;
begin
  Result := FMap.FItems[FIndex].Key;
end;

function TFslMap<T>.TKeyEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FMap.FItems) - 1 do
  begin
    Inc(FIndex);
    if FMap.FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

// Values

constructor TFslMap<T>.TValueEnumerator.Create(const AMap : TFslMap<T>);
begin
  inherited Create;
  FIndex := -1;
  FMap := AMap;
end;

function TFslMap<T>.TValueEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TFslMap<T>.TValueEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TFslMap<T>.TValueEnumerator.GetCurrent: T;
begin
  Result := FMap.FItems[FIndex].Value;
end;

function TFslMap<T>.TValueEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FMap.FItems) - 1 do
  begin
    Inc(FIndex);
    if FMap.FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

{ TFslMap<T>.TValueCollection }

constructor TFslMap<T>.TValueCollection.Create(const AMap : TFslMap<T>);
begin
  inherited Create;
  FMap := AMap;
end;

function TFslMap<T>.TValueCollection.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TFslMap<T>.TValueCollection.GetCount: Integer;
begin
  Result := FMap.Count;
end;

function TFslMap<T>.TValueCollection.GetEnumerator: TValueEnumerator;
begin
  Result := TValueEnumerator.Create(FMap);
end;

function TFslMap<T>.TValueCollection.ToArray: TArray<T>;
begin
  raise Exception.Create('unimplemented');
//  Result := ToArrayImpl(FMap.Count);
end;

{ TFslMap<T>.TKeyCollection }

constructor TFslMap<T>.TKeyCollection.Create(const AMap : TFslMap<T>);
begin
  inherited Create;
  FMap := AMap;
end;

function TFslMap<T>.TKeyCollection.DoGetEnumerator: TEnumerator<String>;
begin
  Result := GetEnumerator;
end;

function TFslMap<T>.TKeyCollection.GetCount: Integer;
begin
  Result := FMap.Count;
end;

function TFslMap<T>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(FMap);
end;

function TFslMap<T>.TKeyCollection.ToArray: TArray<String>;
begin
  raise Exception.Create('unimplemented');
//  Result := ToArrayImpl(FMap.Count);
end;


{ TFslStringDictionary }

procedure TFslStringDictionary.Free;
begin
  If Assigned(Self) and (InterlockedDecrement(FFslObjectReferenceCount) < 0) Then
    Destroy;
end;

function TFslStringDictionary.Link: TFslStringDictionary;
begin
  Result := Self;
  If Assigned(Self) Then
    InterlockedIncrement(FFslObjectReferenceCount);
end;

{ TArrayMoveManager<T> }

procedure TMoveArrayManager<T>.Finalize(var AArray: array of T; Index, Count: Integer);
begin
  System.FillChar(AArray[Index], Count * SizeOf(T), 0);
end;

procedure TMoveArrayManager<T>.Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer);
begin
  System.Move(AArray[FromIndex], AArray[ToIndex], Count * SizeOf(T));
end;

procedure TMoveArrayManager<T>.Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer);
begin
  System.Move(FromArray[FromIndex], ToArray[ToIndex], Count * SizeOf(T));
end;

{$IF Defined(WEAKREF)}
procedure TManualArrayManager<T>.Finalize(var AArray: array of T; Index, Count: Integer);
begin
  System.Finalize(AArray[Index], Count);
  System.FillChar(AArray[Index], Count * SizeOf(T), 0);
end;

procedure TManualArrayManager<T>.Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer);
var
  I: Integer;
begin
  if Count > 0 then
    if FromIndex < ToIndex then
      for I := Count - 1 downto 0 do
        AArray[ToIndex + I] := AArray[FromIndex + I]
    else if FromIndex > ToIndex then
      for I := 0 to Count - 1 do
        AArray[ToIndex + I] := AArray[FromIndex + I];
end;

procedure TManualArrayManager<T>.Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer);
var
  I: Integer;
begin
  if Count > 0 then
    if FromIndex < ToIndex then
      for I := Count - 1 downto 0 do
        ToArray[ToIndex + I] := FromArray[FromIndex + I]
    else if FromIndex > ToIndex then
      for I := 0 to Count - 1 do
        ToArray[ToIndex + I] := FromArray[FromIndex + I];
end;
{$ENDIF}


{$R-}
function TFslMap<T>.Hash(const Key: String): Integer;
var
  LResult: UInt32;
  I: Integer;
begin
  LResult := 0;
  for I := 0 to Key.Length - 1 do
  begin
    LResult := (LResult shl 5) or (LResult shr 27); //ROL Result, 5
    LResult := LResult xor UInt32(Key[I]);
  end;
  Result := LResult
end;

{ TFslStringSet }

constructor TFslStringSet.Create(c1, c2: TFslStringSet);
begin
  create;
  addAll(c1);
  addAll(c2);
end;

constructor TFslStringSet.Create(initial: array of String);
var
  s : String;
begin
  create;
  for s in initial do
    add(s);
end;

destructor TFslStringSet.Destroy;
begin
  inherited;
end;

function TFslStringSet.GetEnumerator: TFslStringSetEnumerator;
begin
  Result := TFslStringSetEnumerator.Create(Self);
end;

function TFslStringSet.isEmpty: boolean;
begin
  result := length(FItems) = 0;
end;

constructor TFslStringSet.Create(initial: String);
begin
  create;
  add(initial);
end;

function TFslStringSet.Link: TFslStringSet;
begin
  result := TFslStringSet(inherited Link);
end;

procedure TFslStringSet.remove(s: String);
begin
  if contains(s) then
    raise Exception.Create('Not done yet');
end;

function TFslStringSet.ToString: String;
var
  b : TStringBuilder;
  f : boolean;
  s : string;
begin
  f := true;
  b := TStringBuilder.Create;
  try
    for s in FItems do
    begin
      if f then
        f := false
      else
        b.Append(', ');
      b.Append(s)
    end;
    result := b.toString;
  finally
    b.Free;
  end;
end;

procedure TFslStringSet.add(value: String);
begin
  if not contains(value) then
  begin
    SetLength(FItems, length(FItems)+1);
    FItems[length(FItems)-1] := value;
  end;
end;

procedure TFslStringSet.addAll(collection: TFslStringSet);
var
  s : String;
begin
  for s in collection.FItems do
    add(s);
end;

function TFslStringSet.AsString(sep: String): String;
var
  b : TStringBuilder;
  f : boolean;
  s : string;
begin
  f := true;
  b := TStringBuilder.Create;
  try
    for s in FItems do
    begin
      if f then
        f := false
      else
        b.Append(sep);
      b.Append(s)
    end;
    result := b.toString;
  finally
    b.Free;
  end;
end;

function TFslStringSet.contains(s: String): boolean;
var
  i : String;
begin
  result := true;
  for i in FItems do
    if i = s then
      exit;
  result := false;
end;

{ TFslStringSet.TFslStringSetEnumerator }

constructor TFslStringSet.TFslStringSetEnumerator.Create(const aSet: TFslStringSet);
begin
  inherited Create;
  FSet := aSet;
  FIndex := -1;
end;

function TFslStringSet.TFslStringSetEnumerator.DoGetCurrent: String;
begin
  Result := GetCurrent;
end;

function TFslStringSet.TFslStringSetEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TFslStringSet.TFslStringSetEnumerator.GetCurrent: String;
begin
  Result := FSet.FItems[FIndex];
end;

function TFslStringSet.TFslStringSetEnumerator.MoveNext: Boolean;
begin
  if FIndex >= Length(FSet.FItems) then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < Length(FSet.FItems);
end;

{ TFslStringMap }

constructor TFslStringMap.Create;
begin
  inherited;
  FDict := TDictionary<String, String>.create;
end;

destructor TFslStringMap.Destroy;
begin
  FDict.Free;
  inherited;
end;

function TFslStringMap.GetItem(const Key: String): String;
begin
  if not FDict.TryGetValue(Key, result) then
    result := '';
end;

function TFslStringMap.link: TFslStringMap;
begin
  result := TFslStringMap(inherited Link);
end;

procedure TFslStringMap.SetItem(const Key, Value: String);
begin
  FDict[key] := value;
end;

end.

