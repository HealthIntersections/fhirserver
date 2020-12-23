Unit fsl_base;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

{$I fhir.inc}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ELSE}
  LCLType, Dialogs,
  {$ENDIF}
  SysUtils, Classes, Types, RTLConsts, Generics.Defaults, Generics.Collections, fsl_fpc;

const
  {$IFDEF FPC}
  SArgumentOutOfRange : AnsiString = 'index out of range';
  {$ENDIF}
  EMPTY_HASH = -1;

threadvar
  gExceptionStack : String;
  gException : Exception;

var
  DebugConsoleMessages : boolean = false;

procedure recordStack(e : Exception);
function ExceptionStack(e : Exception) : String;

Type
  Exception = SysUtils.Exception;
  ExceptionClass = Class Of Exception;

  EFslException = Class(Exception)
    Private
      FSender : String;
      FMethod : String;
      FReason : String;
      FStackTrace : String;

    Public
      constructor Create(Const sSender, sMethod, sReason : String); Overload; Virtual;
      constructor Create(oSender : TObject; Const sMethod, sReason : String); Overload;

      Function Description : String;

      Property Sender : String Read FSender;
      Property Method : String Read FMethod;
      Property Reason : String Read FReason;
      Property StackTrace : String Read FStackTrace Write FStackTrace;
  End;

  EFslExceptionClass = Class Of EFslException;

  // particular subclasses
  EFslAbstract = Class(EFslException);
  EFslAssertion = Class(EFslException);
  ETodo = Class(EFslException)
  public
    Constructor Create(place : String);
  End;
  ELibraryException = Class(EFslException); // general library functionality
  EIOException = Class(EFslException); // problems reading/writing files
  EWebException = Class(EFslException); // error in web stack (client or server)
  EER7Exception = class (EFslException); // error reading or writing Vertical Bar
  EXmlException = class (EFslException); // error reading or writing Xml
  EXmlTodo = Class(EXmlException)
  public
    Constructor Create(place : String);
  End;
  ERdfException = Class(EFslException); // error reading or writing RDF
  EDBException = Class(EFslException); // error accessing / working with database (including sql and dialect errors
  EDBTodo = Class(EDBException)
  public
    Constructor Create(place : String);
  End;
  ETerminologySetup = class (EFslException); // problem in the terminology configuration or loaded terminologies
  ETerminologyError = class (EFslException); // problem in terminology operation
  ETerminologyTodo = Class(ETerminologyError)
  public
    Constructor Create(place : String);
  End;
  ETestCase = class (EFslException); // Failing test case
  EJavascriptException = class (EFslException); // exception thrown in javscript library
  EJavascriptScript = class (EJavascriptException); // error thrown by script
  EJavascriptSource = class (EJavascriptException); // error compiling
  EJavascriptHost = class (EJavascriptException);   // error from hosting infrastructure
  EJavascriptApplication = class (EJavascriptException);    // error running application functionality
  ETestExceptionNotDone = class (EFslException);
  EJsonException = class (EFslException); // error reading or writing Json
  EJsonTodo = Class(EJsonException)
  public
    Constructor Create(place : String);
  End;

Function ExceptObject : Exception;
Function HasExceptObject : Boolean;

Type
  TGetThreadNameStatus = function : String;

var
  GetThreadNameStatusDelegate : TGetThreadNameStatus = nil;
  ShowObjectLeaks : boolean;

Type
  TFslObjectClass = Class Of TFslObject;
  TFslClass = TFslObjectClass;

  TFslReferenceCount = Integer;

  {$M+}
  TFslObject = Class(TObject)
  Private
    // Reference counted using Interlocked* Windows API functions.
    FFslObjectReferenceCount : TFslReferenceCount;
    FTagObject : TObject;
    {$IFOPT D+}
    // This is a workaround for the delphi debugger not showing the actual class of an object that is polymorphic
    // It's sole purpose is to be visible in the debugger. No other functionality should depend on it
    FNamedClass : String;
    {$ENDIF}
    {$IFDEF OBJECT_TRACKING}
    FTracked : boolean;
    FSerial : integer;
    FNext, FPrev : TFslObject; // same class type
    FThreadName : String;
    {$ENDIF}

  Protected
    // Declared here for ease of implementing interfaces.
    Function _AddRef : Integer; Stdcall;
    Function _Release : Integer; Stdcall;
    Function QueryInterface(const IID : TGUID; Out Obj): HResult; Virtual; Stdcall;
    // May be called from Nil or invalid references (so can't be virtual).
    Function Invariant(Const sMethod, sMessage : String) : Boolean; Overload;
    Function Invariants(Const sLocation : String; oObject : TObject; aClass : TClass; Const sObject : String) : Boolean; Overload;
    Function Invariants(Const sLocation : String; oObject : TFslObject; aClass : TClass; Const sObject : String) : Boolean; Overload;
    Function Invariants(Const sLocation : String; aReference, aClass : TClass; Const sReference : String) : Boolean; Overload;

    Function CheckCondition(bCorrect : Boolean; aException : EFslExceptionClass; Const sMethod, sMessage : String) : Boolean; Overload;
    Function CheckCondition(bCorrect : Boolean; Const sMethod, sMessage : String) : Boolean; Overload;

    // Override to introduce additional or alternate behaviour.
    Function Assignable(Const sLocation : String; oObject : TFslObject; Const sObject : String) : Boolean; Overload; Virtual;
    Function Alterable(Const sMethod : String) : Boolean; Overload; Virtual;
    Procedure RaiseError(aException : EFslExceptionClass; Const sMethod, sMessage : String); Overload; Virtual;
    Procedure RaiseError(Const sMethod, sMessage : String); Overload; Virtual;

    Class Procedure ClassError(Const sMethod, sMessage : String); Overload;

    Function ErrorClass : EFslExceptionClass; Overload; Virtual;

    function sizeInBytesV : cardinal; virtual;
  Public
    constructor Create; Overload; Virtual;
    destructor Destroy; Override;

    Procedure AfterConstruction; Override;
    Procedure BeforeDestruction; Override;

    // Cannot be virtual as they are allowed to be called from Nil or invalid objects (but will assert).
    Procedure Free; Overload;
    Function Link : TFslObject; Overload;
    Function Unlink : TFslObject; Overload;
    Function Clone : TFslObject; Overload;
    Function ClassType : TFslObjectClass; Overload;

    // Assignment.
    Function Assignable : Boolean; Overload; Virtual;
    Function Duplicate : TFslObject; Overload; Virtual;
    Procedure Assign(oObject : TFslObject); Overload; Virtual;
    function sizeInBytes : cardinal;

    // Determine if self is a valid reference of the specified class.
    Function Invariants(Const sLocation : String; aClass : TClass) : Boolean; Overload;

    Property FslObjectReferenceCount : TFslReferenceCount Read FFslObjectReferenceCount;
    property TagObject : TObject read FTagObject write FTagObject; // no ownership....
    {$IFOPT D+}
    property NamedClass : String read FNamedClass;
    {$ENDIF}
    {$IFDEF OBJECT_TRACKING}
    property SerialNumber : integer read FSerial;
    {$ENDIF}

    class function getReport(sep : String; full : boolean) : String;
  End;
  {$M-}

  PFslObject = ^TFslObject;

  EFslInvariant = Class(EFslException)
    Public
      constructor Create(Const sSender, sMethod, sReason : String); Overload; Override;
  End;

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

  TFslComparer<T : class> = class abstract (TFslObject)
  protected
    function Compare(const l, r : T) : integer; virtual; abstract;
  public
    function link : TFslComparer<T>; overload;
  end;

  // Actually, T must be TFslObject, but this doesn't work because of forwards class definitions

  { TFslList }
  TFslList<T : class> = class (TFslEnumerable<T>)
  public
  type
    TListCompareEvent = function (sender : TObject; const L, R: T): Integer of object;
    TEmptyEvent = function (sender : TObject; const L, R: T): Boolean of object;
    TListMatchEvent = function (sender : TObject; const i : T): boolean of object;
    TFslListRemoveEvent = function (sender : TObject; item : T):boolean of object;
    TFslCollectionNotifyEvent = procedure(ASender: TObject; const AItem: T; AAction: TCollectionNotification) of object;
    {$IFNDEF FPC}
    TEmptyFunc = reference to function (const L, R: T): Boolean;
    TListCompareFunc = reference to function (const L, R: T): Integer;
    TListMatchFunc = reference to function (const i : T): boolean;
    TFslListRemoveFunc =  reference to function(item : T):boolean;
    {$ENDIF}
  private
    FJsHandle: pointer;
    FJsInstance: cardinal;
    function GetEmpty: boolean;
  type
    arrayofT = array of T;
    TDefaultComparer = class (TFslComparer<T>)
    protected
      function Compare(const l, r : T) : integer; override;
    end;

  var
    FItems: arrayofT;
    FCount: Integer;
    FComparer: TFslComparer<T>;
    FOnNotify: TFslCollectionNotifyEvent;
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
    {$IFNDEF FPC}
    procedure QuickSort(L, R: Integer; compare: TListCompareFunc); overload;
    {$ENDIF}
    procedure QuickSort(L, R: Integer; compare: TListCompareEvent); overload;
    procedure QuickSort(L, R: Integer; comparer: TFslComparer<T>); overload;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure NotifyChange(const Item: T; Action: TCollectionNotification);
    function sizeInBytesV : cardinal; override;
  public

    constructor Create; Overload; Override;
    constructor Create(capacity : integer); Overload;
    constructor Create(const AComparer: TFslComparer<T>); overload;
    constructor Create(const Collection: TEnumerable<T>); overload;
    destructor Destroy; override;

    function link : TFslList<T>; overload;
    function forEnum : TFslList<T>; // auto frees a collection once an enumerator is finished with it - a commmon pattern

    {$IFNDEF FPC}
    // something about this makes FPC blow up in a different unit
    // if B is a subclass of A, TFslList<B> is not a subclass of TFslList<A>. These 2 routines help deal with this
    function asBase : TFslList<TFslObject>;
    procedure copyList(list : TFslList<TFslObject>);
    {$ENDIF}

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

    procedure AddAll(list : TFslList<T>); overload;

    function Remove(const Value: T): Integer;
    procedure RemoveAll(list : TFslList<T>); overload;
    {$IFNDEF FPC}
    procedure RemoveAll(filter : TFslListRemoveFunc); overload;
    {$ENDIF}
    procedure RemoveAll(filter : TFslListRemoveEvent); overload;
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

    function Contains(const Value: T): Boolean; overload;
    {$IFNDEF FPC}
    function Contains(match: TListMatchFunc): Boolean; overload;
    {$ENDIF}
    function Contains(match: TListMatchEvent): Boolean; overload;
    function IndexOf(const Value: T): Integer;
    function IndexOfItem(const Value: T; Direction: TDirection): Integer;
    function LastIndexOf(const Value: T): Integer;

    procedure Reverse;

    procedure Sort; overload;

    {$IFNDEF FPC}
    procedure SortF(compare: TListCompareFunc); overload;
    {$ENDIF}
    procedure SortE(compare: TListCompareEvent); overload;
    procedure Sort(comparer: TFslComparer<T>); overload;


    {$IFNDEF FPC}
    function matches(other : TFslList<T>; ordered : boolean; criteria : TListCompareFunc) : boolean; overload;
    {$ENDIF}
    function matches(other : TFslList<T>; ordered : boolean; criteria : TFslComparer<T>) : boolean; overload;
    function matches(other : TFslList<T>; ordered : boolean; criteria : TListCompareEvent) : boolean; overload;

    procedure TrimExcess;

    function ToArray: TArray<T>; override; final;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Empty : boolean read GetEmpty;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property List: arrayofT read FItems;

    property OnNotify: TFslCollectionNotifyEvent read FOnNotify write FOnNotify;

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

    // javascript caching for Chakra performance
    property jsInstance : cardinal read FJsInstance write FJsInstance;
    property jsHandle : pointer read FJsHandle write FJsHandle;
  end;

  TFslPair<T : TFslObject> = record
    Key: String;
    Value: T;
    constructor Create(const AKey: String; const AValue: T);
  end;

  { TFslMap }

  TFslMap<T : TFslObject> = class(TEnumerable<TFslPair<T>>)
  public
  type
    TFslCollectionKeyNotifyEvent = procedure(ASender: TObject; const AItem: string; AAction: TCollectionNotification) of object;
    TFslCollectionValueNotifyEvent = procedure(ASender: TObject; const AItem: T; AAction: TCollectionNotification) of object;
    TFslCollectionKeyMissingEvent = procedure(ASender: TFslMap<T>; const key : String; var item : T) of object;
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
    FAsAddedKeys : TStringList;
    FDefault : T;
    FHasDefault : boolean;
    FName : String;

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
  private
    procedure SetDefault(const Value: T);
    procedure SetHasDefault(const Value: Boolean);
  protected
    function DoGetEnumerator: TEnumerator<TFslPair<T>>; override;
    {$IFDEF FPC}
    function GetPtrEnumerator: TEnumerator<PT>; override;
    {$ENDIF}
    procedure KeyNotify(const Key: String; Action: TCollectionNotification); virtual;
    procedure ValueNotify(const Value: T; Action: TCollectionNotification); virtual;
  public
    constructor Create(name : String = ''; ACapacity: Integer = 0);
    constructor CreateCollection(name : String; const Collection: TEnumerable<TFslPair<T>>); overload;
    destructor Destroy; override;
    function Link : TFslMap<T>; overload;
    Procedure Free; Overload;
    procedure trackOrder;

    procedure Add(const Key: String; const Value: T);
    procedure Remove(const Key: String);
    procedure RemoveKeys(const keyList : TStringList);
    procedure Clear;
    procedure TrimExcess;
    function TryGetValue(const Key: String; out Value: T): Boolean;
    procedure AddOrSetValue(const Key: String; const Value: T);
    function ContainsKey(const Key: String): Boolean;
    function ContainsValue(const Value: T): Boolean;
    function ToArray: TArray<TFslPair<T>>; override; final;

    procedure addAll(other : TFslMap<T>);
    procedure listAll(other : TFslList<T>);
    property Items[const Key: String]: T read GetItem write SetItem; default;
    property Count: Integer read FCount;
    property IsEmpty : Boolean read GetEmpty;
    property defaultValue : T read FDefault write SetDefault;
    property hasDefault : Boolean read FHasDefault write SetHasDefault;
    function sizeInBytes : cardinal;

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

      { TValueCollection }

      TValueCollection = class(TEnumerable<T>)
      private
        FMap: TFslMap<T>;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<T>; override;
        {$IFDEF FPC}
        function GetPtrEnumerator: TEnumerator<PT>; override;
        {$ENDIF}
      public
        constructor Create(const AMap: TFslMap<T>);
        function GetEnumerator: TValueEnumerator; reintroduce;
        function ToArray: TArray<T>; override; final;
        property Count: Integer read GetCount;
      end;

      { TKeyCollection }

      TKeyCollection = class(TEnumerable<String>)
      private
        FMap: TFslMap<T>;
        function GetCount: Integer;
      protected
        {$IFDEF FPC}
        function GetPtrEnumerator: TEnumerator<PT>; override;
        {$ENDIF}
        function DoGetEnumerator: TEnumerator<String>; override;
      public
        constructor Create(const AMap: TFslMap<T>);
        function GetEnumerator: TKeyEnumerator; reintroduce;
        function ToArray: TArray<String>; override; final;
        property Count: Integer read GetCount;
      end;

  private
    FOnKeyNotify: TFslCollectionKeyNotifyEvent;
    FOnValueNotify: TFslCollectionValueNotifyEvent;
    FOnNoMatch: TFslCollectionKeyMissingEvent;
    FKeyCollection: TKeyCollection;
    FValueCollection: TValueCollection;
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
    function GetSortedKeys: TStringList;
    function GetAsAddedKeys: TStringList;
  public
    function GetEnumerator: TFslPairEnumerator; reintroduce;
    property Keys: TKeyCollection read GetKeys;
    property SortedKeys : TStringList read GetSortedKeys;
    property AsAddedKeys : TStringList read GetAsAddedKeys;
    property Values: TValueCollection read GetValues;
    property OnKeyNotify: TFslCollectionKeyNotifyEvent read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TFslCollectionValueNotifyEvent read FOnValueNotify write FOnValueNotify;
    property OnNoMatch: TFslCollectionKeyMissingEvent read FOnNoMatch write FOnNoMatch;
  end;

  TFslStringDictionary = class (TDictionary<String, String>)
  private
    FFslObjectReferenceCount : TFslReferenceCount;
    function GetValue(const Key: String): String;
    procedure SetValue(const Key, Value: String);
  public
    // Cannot be virtual as they are allowed to be called from Nil or invalid objects (but will assert).
    Procedure Free; Overload;
    Function Link : TFslStringDictionary; Overload;
    procedure assign(source : TFslStringDictionary);
    property Values[const Key: String]: String read GetValue write SetValue; default;
    function sizeInBytes : cardinal;
  end;

  TFslStringSet = class (TFslObject)
  private
    // not sorted - this doesn't get long enough to make it worth sorting
    FItems : TArray<String>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(initial : String); overload;
    constructor Create(initial : array of String); overload;
    constructor Create(c1, c2 : TFslStringSet); overload;
    destructor Destroy; override;
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
    FDict : TFslStringDictionary;
    function GetItem(const Key: String): String;
    procedure SetItem(const Key, Value: String);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFslStringMap;

    procedure clear;
    property Items[const Key: String]: String read GetItem write SetItem; default;
  end;

implementation

type
  TClassTrackingType = class (TObject)
  private
    first, last : TFslObject;
    count : integer;
    dcount : integer;
    serial : integer;
  end;

var
  GInited : boolean;
  GLock : TRTLCriticalSection;
  GClassTracker : TDictionary<String, TClassTrackingType>;

procedure initUnit;
begin
  if not GInited then
  begin
    InitializeCriticalSection(GLock);
    GClassTracker := TDictionary<String, TClassTrackingType>.create;
    GInited := true;
    ShowObjectLeaks := true;
  end;
end;

procedure endUnit;
var
  t : TClassTrackingType;
  n, s : String;
  i : integer;
begin
  if ShowObjectLeaks then
  begin
    s := '';
    i := 0;
    for n in GClassTracker.Keys do
    begin
      t := GClassTracker[n];
      i := i + t.count;
      if t.count > 0 then
        s := s + n+': '+inttostr(t.count)+#13#10;
      t.Free;
    end;
    if i > 0 then
    begin
      {$IFDEF WINDOWS}
      messagebox(0, pchar(s), 'Object Leaks', MB_OK);
      {$ELSE}
      DefaultMessageBox('Object Leaks', pchar(s), MB_OK);
      {$ENDIF}
    end;
  end;
  GClassTracker.Free;
  DeleteCriticalSection(GLock);
  GInited := false;
end;


Function ExceptObject : Exception;
Begin
{$IFDEF FPC}
  Result := Exception(sysutils.ExceptObject);
{$ELSE}
  Result := Exception(System.ExceptObject);
{$ENDIF}
End;

Function HasExceptObject : Boolean;
Begin
  Result := ExceptObject <> Nil;
End;

Procedure AbstractHandler(oObject : TObject);
  {$IFDEF WIN32}
Var
  pAddress : ^Integer;
  {$ENDIF}
Begin
  {$IFDEF WIN32}
  // pAddress will point at the location of the method in memory.  The Delphi action
  // Search | Find Error can be used to locate the line that causes the abstract error
  // when the application is running.

  ASM
    mov pAddress, ebp
  End;

  Inc(pAddress, 2);

  If Assigned(oObject) Then
    Raise EFslAbstract.Create('fsl_base', 'AbstractHandler', String.Format('Attempted call onto an abstract method $%x in class ''%s''.', [pAddress^, oObject.ClassName]))
  Else
    Raise EFslAbstract.Create('fsl_base', 'AbstractHandler', String.Format('Attempted call onto an abstract method $%x in object $%x.', [pAddress^, Integer(oObject)]));
  {$ELSE}
  {$IFDEF FPC}
  Raise EFslAbstract.Create('fsl_base', 'AbstractHandler', String.Format('Attempted call onto an abstract method $?? in object $%x.', [Pointer(oObject)]));
  {$ELSE}
  Raise EFslAbstract.Create('fsl_base', 'AbstractHandler', String.Format('Attempted call onto an abstract method $?? in object $%x.', [Integer(oObject)]));
  {$ENDIF}
  {$ENDIF}
End;

//Procedure AssertionHandler(Const sMessage, sFilename : AnsiString; iLineNumber : Integer);
//Begin
//  Raise EFslAssertion.Create('fsl_base', 'AssertionHandler', String.Format('%s (%s:%d)', [sMessage, sFilename, iLineNumber]));
//End;

procedure recordStack(e : Exception);
begin
  if (e <> gException) then
  begin
    {$IFNDEF FPC}
    gExceptionStack := e.StackTrace;
    {$ENDIF}
    gException := e;
  end;
end;

function ExceptionStack(e : Exception) : String;
begin
  if (e = gException) then
    result := gExceptionStack
  else
  begin
    {$IFNDEF FPC}
    result := e.StackTrace;
    {$ENDIF}
  end;
  gException := nil;
end;

{ EFslException }

Constructor EFslException.Create(Const sSender, sMethod, sReason : String);
Begin
  FSender := sSender;
  FMethod := sMethod;
  FReason := sReason;

  Message := FReason;
End;

Constructor EFslException.Create(oSender : TObject; Const sMethod, sReason : String);
Var
  sSender : String;
Begin
  If Assigned(oSender) Then
  Begin
    sSender := oSender.ClassName;
  End
  Else
  Begin
    sSender := '<Nil>';
  End;

  Create(sSender, sMethod, sReason);
End;

Function EFslException.Description : String;
Begin
  If (FSender <> '') Or (FMethod <> '') Then
    Result := String.Format('(%s.%s): %s', [FSender, FMethod, FReason])
  Else
    Result := FReason;
End;

{ EFslInvariant }

Constructor EFslInvariant.Create(Const sSender, sMethod, sReason : String);
Begin
  Inherited;

  Message := Description;
End;

{ TFslObject }

Constructor TFslObject.Create;
var
  t : TClassTrackingType;
Begin
  Inherited;
  {$IFOPT D+}
  FNamedClass := ClassName;
  {$ENDIF}
  {$IFDEF OBJECT_TRACKING}
  if not GInited then
    initUnit;
  if Assigned(GetThreadNameStatusDelegate) then
    FThreadName := GetThreadNameStatusDelegate;
  EnterCriticalSection(GLock);
  try
    if not GClassTracker.TryGetValue(FNamedClass, t) then
    begin
      t := TClassTrackingType.Create;
      GClassTracker.Add(FNamedClass, t);
    end;
    FTracked := true;
    inc(t.count);
    inc(t.dcount);
    inc(t.serial);
    FSerial := t.serial;
    if t.first = nil then
    begin
      t.first := self;
      t.last := self;
    end
    else
    begin
      t.last.FNext := self;
      FPrev := t.last;
      t.last := self;
    end;
  finally
    LeaveCriticalSection(GLock);
  end;
  {$ENDIF}
End;

Destructor TFslObject.Destroy;
var
  t : TClassTrackingType;
Begin
  {$IFDEF OBJECT_TRACKING}
  if GInited then
  begin
    EnterCriticalSection(GLock);
    try
      if GClassTracker.TryGetValue(FNamedClass, t) then // this will succeed
      begin
        dec(t.Count);
        dec(t.dCount);
        if FTracked then
        begin
          if FPrev = nil then
          begin
            t.first := self.FNext;
            if self.FNext <> nil then
              self.FNext.FPrev := nil;
          end
          else
          begin
            if self.FNext <> nil then
              self.FNext.FPrev := self.FPrev;
            self.FPrev.FNext := self.FNext;
          end;

          if FNext = nil then
          begin
            t.last := self.FPrev;
            if self.FPrev <> nil then
              self.FPrev.FNext := nil;
          end
          else
          begin
            if self.FPrev <> nil then
              self.FPrev.FNext := self.FNext;
            self.FNext.FPrev := self.FPrev;
          end;
        end;
      end;
    finally
      LeaveCriticalSection(GLock);
    end;
  end;
  {$ENDIF}
  Inherited;
End;

Procedure TFslObject.AfterConstruction;
Begin
  Inherited;
End;

Procedure TFslObject.BeforeDestruction;
Begin
  // TODO: really should always be -1, but SysUtils.FreeAndNil may bypass the correct Free method.
  Assert(CheckCondition(FFslObjectReferenceCount <= 0, 'BeforeDestruction', 'Attempted to destroy object before all references are released (possibly freed while cast as a TObject).'));

  Inherited;
End;

Procedure TFslObject.Free;
Begin
  If Assigned(Self) Then
  Begin
    Assert(Invariants('Free', TFslObject));

    if FFslObjectReferenceCount = -1 then
      raise EFslException.Create('Attempt to free a class a second time (of type '+className+'?)');
    If (InterlockedDecrement(FFslObjectReferenceCount) < 0) Then
      Destroy;
  End;
End;

class function TFslObject.getReport(sep : String; full : boolean): String;
var
  cn : String;
  t : TClassTrackingType;
  ts : TStringList;
  o : TFslObject;
  i : integer;
begin
  {$IFDEF OBJECT_TRACKING}
  result := '';
  EnterCriticalSection(GLock);
  try
    for cn in GClassTracker.Keys do
    begin
      t := GClassTracker[cn];
      if full then
      begin
        ts := TStringList.Create;
        try
          if t.dcount <> 0 then
          begin
            o := t.first;
            while o <> nil do
            begin
              i := ts.IndexOf(o.FThreadName);
              if i = -1 then
                i := ts.Add(o.FThreadName);
              ts.Objects[i] := TObject(integer(ts.Objects[i])+1);
              o.FTracked := false;
              o.FThreadName := '';
              o := o.FNext;
            end;
            t.first := nil;
            t.last := nil;
            result := result + cn + ': '+inttostr(t.count)+' of '+inttostr(t.serial)+'. Delta = '+inttostr(t.dcount);
            if ts.Count <> 0 then
            begin
              result := result + ': ';
              for i := 0 to ts.Count - 1 do
                result := result + ts[i]+': '+inttostr(integer(ts.objects[i]))+',';
            end;
            t.dcount := 0;
            result := result + sep;
          end;
        finally
          ts.Free;
        end;
      end
      else
        result := result + cn + ': '+inttostr(t.count)+' of '+inttostr(t.serial)+sep;
    end;
  finally
    LeaveCriticalSection(GLock);
  end;
  if result = '' then
    result := 'Nothing to report';
  {$ELSE}
  result := 'Object Tracking is not enable';
  {$ENDIF}
end;

Function TFslObject.ClassType : TFslObjectClass;
Begin
  Result := TFslObjectClass(Inherited ClassType);
End;

Function TFslObject.Unlink : TFslObject;
Begin
  Result := Self;

  If Assigned(Self) Then
  Begin
    Assert(Invariants('Unlink', TFslObject));

    If (InterlockedDecrement(FFslObjectReferenceCount) < 0) Then
    Begin
      Destroy;
      Result := Nil;
    End;
  End;
End;

Function TFslObject.Link : TFslObject;
Begin
  Result := Self;

  If Assigned(Self) Then
  Begin
    Assert(Invariants('Link', TFslObject));

    InterlockedIncrement(FFslObjectReferenceCount);
  End;
End;

Function TFslObject.Duplicate : TFslObject;
Begin
  Result := ClassType.Create;
End;

Function TFslObject.Clone : TFslObject;
Begin
  If Assigned(Self) Then
  Begin
    Assert(Invariants('Clone', TFslObject));

    Result := Duplicate;
    Result.Assign(Self);

    Assert(Invariants('Clone', Result, ClassType, 'Result'));
  End
  Else
  Begin
    Result := Nil;
  End;
End;

Function TFslObject._AddRef : Integer; stdcall;
Begin
  If Assigned(Self) Then
  Begin
    Assert(Invariants('_AddRef', TFslObject));

    Result := InterlockedIncrement(FFslObjectReferenceCount);
  End
  Else
  Begin
    Result := 0;
  End;
End;

Function TFslObject._Release: Integer; stdcall;
Begin
  If Assigned(Self) Then
  Begin
    Assert(Invariants('_Release', TFslObject));

    Result := InterlockedDecrement(FFslObjectReferenceCount);

    If Result < 0 Then
      Destroy;
  End
  Else
  Begin
    Result := 0;
  End;
End;

Function TFslObject.QueryInterface(const IID: TGUID; Out Obj): HResult; stdcall;
//Const
//  // Extra typecast to longint prevents a warning about subrange bounds
//  SUPPORT_INTERFACE : Array[Boolean] Of HResult = (Longint($80004002), 0);
Begin
//  Result := SUPPORT_INTERFACE[GetInterface(IID, Obj)];
  If GetInterface(IID, Obj) Then
    Result := S_OK
  Else
    Result := E_NOINTERFACE;
End;

Function TFslObject.Assignable : Boolean;
Begin
  Result := True;
End;

Function TFslObject.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslException;
End;

Procedure TFslObject.RaiseError(aException : EFslExceptionClass; Const sMethod, sMessage : String);
Begin
  Raise aException.Create(Self, sMethod, sMessage);
End;

Procedure TFslObject.RaiseError(Const sMethod, sMessage : String);
Begin
  RaiseError(ErrorClass, sMethod, sMessage);
End;

Function TFslObject.Assignable(Const sLocation : String; oObject : TFslObject; Const sObject : String) : Boolean;
Begin
  Invariants(sLocation, oObject, ClassType, sObject);

  If (Self = oObject) Then
    Invariant(sLocation, 'Cannot assign an object to itself.');

  Result := Alterable(sLocation);
End;

Procedure TFslObject.Assign(oObject : TFslObject);
Begin
  Assert(CheckCondition(Assignable, 'Assign', 'Object is not marked as assignable.'));
  Assert(Assignable('Assign', oObject, 'oObject'));

  // Override and inherit to assign the properties of your class.
End;

Function TFslObject.Invariants(Const sLocation: String; aReference, aClass: TClass; Const sReference : String): Boolean;
Begin
  // Ensure class is assigned.
  If Not Assigned(aReference) Then
    Invariant(sLocation, sReference + ' was not assigned and was expected to have been of class type ' + aClass.ClassName);

  // Ensure class is of the expected class.
  If Not aReference.InheritsFrom(aClass) Then
    Invariant(sLocation, sReference + ' was of class type ' + aReference.ClassName + ' and should have been of class type ' + aClass.ClassName);

  Result := True;
End;

Function TFslObject.Invariants(Const sLocation : String; oObject : TObject; aClass: TClass; Const sObject : String) : Boolean;
Begin
  If Not Assigned(aClass) Then
    Invariant('Invariants', 'aClass was not assigned.');

  // Ensure object is assigned.                            se
  If Not Assigned(oObject) Then
    Invariant(sLocation, sObject + ' was not assigned and was expected to have been of class ' + aClass.ClassName);

  if assigned(oObject) and not (oObject is aClass) then
    Invariant(sLocation, sObject + ' was expected to have been of class ' + aClass.ClassName+', but is '+oObject.ClassName);

  Result := True;
End;

Function TFslObject.Invariants(Const sLocation : String; oObject: TFslObject; aClass: TClass; Const sObject : String) : Boolean;
Begin
  Invariants(sLocation, TObject(oObject), aClass, sObject);

  Result := True;
End;

Function TFslObject.Invariants(Const sLocation: String; aClass: TClass) : Boolean;
Begin
  Invariants(sLocation, TObject(Self), aClass, 'Self');

  Result := True;
End;

Function TFslObject.CheckCondition(bCorrect: Boolean; Const sMethod, sMessage: String): Boolean;
Begin
  // Call this method as you would the Assert procedure to raise an exception if bCorrect is False.

  If Not bCorrect Then
    Invariant(sMethod, sMessage);

  Result := True;
End;

Function TFslObject.CheckCondition(bCorrect : Boolean; aException : EFslExceptionClass; Const sMethod, sMessage : String) : Boolean;
Begin
  // Call this method as you would the Assert procedure to raise an exception if bCorrect is False.

  If Not bCorrect Then
    RaiseError(aException, sMethod, sMessage);

  Result := True;
End;

Function TFslObject.Invariant(Const sMethod, sMessage: String): Boolean;
Begin
  // Call this method as you would the Error method to raise an exception.
  // Use this when you are not sure if self is valid as it is a non-virtual method.

  Raise EFslInvariant.Create(Self, sMethod, sMessage); // Can't use Error method here as it is virtual.

  Result := True;
End;

Function TFslObject.Alterable(Const sMethod: String): Boolean;
Begin
  Result := True;
End;

Class Procedure TFslObject.ClassError(Const sMethod, sMessage: String);
Begin
  Raise EFslException.Create(Nil, sMethod, sMessage);
End;

function TFslObject.sizeInBytes : cardinal;
begin
  if self = nil then
    result := 0
  else
    result := sizeInBytesV;
end;

function TFslObject.sizeInBytesV : cardinal;
begin
  result := sizeof(self);
  {$IFOPT D+}
  inc(result, (FNamedClass.length*2)+12);
  {$ENDIF}
  {$IFDEF OBJECT_TRACKING}
  inc(result, length(FThreadName)+12);
  {$ENDIF}
end;

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
      buf.Add(T(TFslObject(x).Link));
    Result := buf.ToArray; // relies on TList<T>.ToArray override
  finally
    buf.Free;
  end;
end;

function TFslEnumerable<T>.ToArrayImpl(Count: Integer): TArray<T>;
var
  x: T;
begin
  result := nil;
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
    NotifyChange(oldItem, cnRemoved);
    NotifyChange(Value, cnAdded);
  finally
    TFslObject(oldItem).free;
  end;
end;

procedure TFslList<T>.QuickSort(L, R: Integer; comparer: TFslComparer<T>);
Var
  I, J, K : Integer;
  v : T;
Begin
  // QuickSort routine (Recursive)
  // * Items is the default indexed property that returns a pointer, subclasses
  //   specify these return values as their default type.
  // * The Compare routine used must be aware of what this pointer actually means.

  Repeat
    I := L;
    J := R;
    K := (L + R) Shr 1;

    Repeat
      // Keep pK pointed at the middle element as it might be moved.
      While (comparer.compare(Items[I], Items[K]) < 0) Do
        Inc(I);

      While (comparer.compare(Items[J], Items[K]) > 0) Do
        Dec(J);

      If I <= J Then
      Begin
        v := FItems[i];
        Fitems[i] := Fitems[j];
        FItems[j] := v;

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
      QuickSort(L, J, comparer);

    L := I;
  Until I >= R;
End;

{$IFNDEF FPC}
Procedure TFslList<T>.QuickSort(L, R: Integer; compare: TListCompareFunc);
Var
  I, J, K : Integer;
  v : T;
Begin
  // QuickSort routine (Recursive)
  // * Items is the default indexed property that returns a pointer, subclasses
  //   specify these return values as their default type.
  // * The Compare routine used must be aware of what this pointer actually means.

  Repeat
    I := L;
    J := R;
    K := (L + R) Shr 1;

    Repeat
      // Keep pK pointed at the middle element as it might be moved.
      While (compare(Items[I], Items[K]) < 0) Do
        Inc(I);

      While (compare(Items[J], Items[K]) > 0) Do
        Dec(J);

      If I <= J Then
      Begin
        v := FItems[i];
        Fitems[i] := Fitems[j];
        FItems[j] := v;

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
      QuickSort(L, J, compare);

    L := I;
  Until I >= R;
End;

procedure TFslList<T>.SortF(compare: TListCompareFunc);
begin
  If (FCount > 1) Then
    QuickSort(0, FCount - 1, compare);              // call the quicksort routine
end;
{$ENDIF}

procedure TFslList<T>.QuickSort(L, R: Integer; compare: TListCompareEvent);
Var
  I, J, K : Integer;
  v : T;
Begin
  // QuickSort routine (Recursive)
  // * Items is the default indexed property that returns a pointer, subclasses
  //   specify these return values as their default type.
  // * The Compare routine used must be aware of what this pointer actually means.

  Repeat
    I := L;
    J := R;
    K := (L + R) Shr 1;

    Repeat
      // Keep pK pointed at the middle element as it might be moved.
      While (compare(self, Items[I], Items[K]) < 0) Do
        Inc(I);

      While (compare(self, Items[J], Items[K]) > 0) Do
        Dec(J);

      If I <= J Then
      Begin
        v := FItems[i];
        Fitems[i] := Fitems[j];
        FItems[j] := v;

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
      QuickSort(L, J, compare);

    L := I;
  Until I >= R;
End;

procedure TFslList<T>.SortE(compare: TListCompareEvent);
begin
  If (FCount > 1) Then
    QuickSort(0, FCount - 1, compare);              // call the quicksort routine
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

procedure TFslList<T>.NotifyChange(const Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

constructor TFslList<T>.Create;
begin
  inherited Create;
  FComparer := TDefaultComparer.create;
  FArrayManager := TMoveArrayManager<T>.Create;
end;

constructor TFslList<T>.Create(const AComparer: TFslComparer<T>);
begin
  inherited Create;
  FArrayManager := TMoveArrayManager<T>.Create;
  FComparer := AComparer;
end;

constructor TFslList<T>.Create(const Collection: TEnumerable<T>);
begin
  inherited Create;
  FComparer := TDefaultComparer.create;
  FArrayManager := TMoveArrayManager<T>.Create;
  InsertRange(0, Collection);
end;

constructor TFslList<T>.Create(capacity: integer);
begin
  inherited Create;
  FComparer := TDefaultComparer.create;
  FArrayManager := TMoveArrayManager<T>.Create;
  self.Capacity := capacity;
end;

destructor TFslList<T>.Destroy;
begin
// Clear method only sets lenght to 0, does not destroy any objects, does it?
// Is the sequence below ok?
  Clear;
  FArrayManager.Free;
  FComparer.Free;
  inherited;
end;

class procedure TFslList<T>.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]) {$IFNDEF FPC}at ReturnAddress{$ENDIF};
end;

class procedure TFslList<T>.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) {$IFNDEF FPC}at ReturnAddress{$ENDIF};
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
  NotifyChange(Value, cnAdded);
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
    Add(T(TFslObject(item).link)); // yes we link here too
end;

procedure TFslList<T>.AddRange(const Collection: TEnumerable<T>);
begin
  InsertRange(Count, Collection);
end;

{$IFNDEF FPC}
function TFslList<T>.asBase: TFslList<TFslObject>;
var
  item : T;
begin
  result := TFslList<TFslObject>.create;
  try
    for item in self do
      result.add(TFslObject(item).link);
  finally
    result.free;
  end;
end;
{$ENDIF}

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
  NotifyChange(Value, cnAdded);
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
    NotifyChange(Values[I], cnAdded);
end;

procedure TFslList<T>.InsertRange(Index: Integer; const Collection: IEnumerable<T>);
var
  item: T;
begin
  for item in Collection do
  begin
    Insert(Index, T(TFslObject(item).link)); // yes we link here too
    Inc(Index);
  end;
end;

procedure TFslList<T>.InsertRange(Index: Integer; const Collection: TEnumerable<T>);
var
  item: T;
begin
  for item in Collection do
  begin
    Insert(Index, T(TFslObject(item).Link));
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
    Result := T(nil)
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

{$IFNDEF FPC}
procedure TFslList<T>.RemoveAll(filter: TFslListRemoveFunc);
var
  i : integer;
begin
  for i := Count - 1 downto 0 do
    if (filter(Items[i])) then
      Delete(i);
end;
{$ENDIF}

procedure TFslList<T>.RemoveAll(filter: TFslListRemoveEvent);
var
  i : integer;
begin
  for i := Count - 1 downto 0 do
    if (filter(self, Items[i])) then
      Delete(i);
end;

procedure TFslList<T>.RemoveAll(list: TFslList<T>);
var
  item: T;
begin
  for item in list do
    Remove(T(item));
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
    raise ELibraryException.create('Item not found to delete');
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
    NotifyChange(oldItem, Notification);
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

  oldItems := nil;
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
      NotifyChange(oldItems[I], cnRemoved);
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

{$IFNDEF FPC}
function TFslList<T>.Contains(match: TListMatchFunc): Boolean;
var
  i : T;
begin
  result := false;
  for i in self do
    if (match(i)) then
      exit(true);
end;
{$ENDIF}

function TFslList<T>.Contains(match: TListMatchEvent): Boolean;
var
  i : T;
begin
  result := false;
  for i in self do
    if (match(self, i)) then
      exit(true);
end;

{$IFNDEF FPC}
procedure TFslList<T>.copyList(list: TFslList<TFslObject>);
var
  item : TFslObject;
begin
  for item in list do
    add(item.link as T);
end;
{$ENDIF}

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

function TFslList<T>.link: TFslList<T>;
begin
  result := TFslList<T>(inherited Link);
end;

function TFslList<T>.matches(other: TFslList<T>; ordered: boolean; criteria: TFslComparer<T>): boolean;
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

{$IFNDEF FPC}
function TFslList<T>.matches(other: TFslList<T>; ordered: boolean; criteria: TListCompareFunc): boolean;
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
      if criteria(Items[i], other[i]) <> 0 then
        exit(false);
  end
  else
  begin
    for i := 0 to Count - 1 do
    begin
      ok := false;
      for j := 0 to Count - 1 do
        if criteria(Items[i], other[j]) = 0 then
        begin
          ok := true;
          break;
        end;
      if not ok then
        exit(false);
    end;
  end;
end;
{$ENDIF}

function TFslList<T>.matches(other: TFslList<T>; ordered: boolean; criteria: TListCompareEvent): boolean;
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
      if criteria(self, Items[i], other[i]) <> 0 then
        exit(false);
  end
  else
  begin
    for i := 0 to Count - 1 do
    begin
      ok := false;
      for j := 0 to Count - 1 do
        if criteria(self, Items[i], other[j]) = 0 then
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
  Sort(FComparer.Link);
end;

procedure TFslList<T>.Sort(comparer: TFslComparer<T>);
begin
  try
    If (FCount > 1) Then
      QuickSort(0, FCount - 1, comparer); // call the quicksort routine
  finally
    comparer.Free;
  end;
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

function TFslList<T>.sizeInBytesV : cardinal;
var
  i : T;
begin
  result := sizeof(self);
  inc(result, length(FItems) * sizeof(Pointer));
  inc(result, FComparer.sizeInBytes);
  inc(result, sizeof(FArrayManager));
  for i in FItems do
    inc(result, TFslObject(t).sizeInBytes);
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
    raise EArgumentOutOfRangeException.Create('Attempt to set capacity to less than count for map '+FName);

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

procedure TFslMap<T>.SetDefault(const Value: T);
begin
  FDefault.free;
  FDefault := Value;
  FHasDefault := true;
end;

procedure TFslMap<T>.SetHasDefault(const Value: Boolean);
begin
  FHasDefault := Value;
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

function TFslMap<T>.GetAsAddedKeys: TStringList;
begin
  if FAsAddedKeys <> nil then
    result := FAsAddedKeys
  else
    raise EFSLException.Create('This map "'+Fname+'" is not tracking order of addition');
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
  if index >= 0 then
    Result := FItems[index].Value
  else if hasDefault then
    result := FDefault
  else if assigned(OnNoMatch) then
    OnNoMatch(self, Key, result)
  else
    raise EListError.Create('Attempt to access unknown value "'+key+'" from map ' + Fname);
end;

procedure TFslMap<T>.SetItem(const Key: String; const Value: T);
var
  index: Integer;
  oldValue: T;
begin
  index := GetBucketIndex(Key, Hash(Key));
  if index < 0 then
    raise EListError.Create('Attempt to set unknown value "'+key+'" in map ' + Fname);

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

procedure TFslMap<T>.listAll(other: TFslList<T>);
var
  item: T;
begin
  for item in Values do
    other.Add(T(TFslObject(item).link));
end;

procedure TFslMap<T>.ValueNotify(const Value: T; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, Value, Action);
end;

constructor TFslMap<T>.Create(name : String = ''; ACapacity: Integer = 0);
begin
  inherited Create;
  FName := name;
  if ACapacity < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  SetCapacity(ACapacity);
end;

constructor TFslMap<T>.CreateCollection(name : String; const Collection: TEnumerable<TFslPair<T>>);
var
  item: TFslPair<T>;
begin
  Create(name, 0);
  for item in Collection do
    AddOrSetValue(item.Key, item.Value);
end;

destructor TFslMap<T>.Destroy;
begin
  Clear;
  FDefault.free;
  FKeyCollection.Free;
  FValueCollection.Free;
  FSortedKeys.Free;
  FAsAddedKeys.Free;
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
    raise EListError.Create('Attempt to add duplicate value "'+key+'" to map ' + FName);

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
  if FAsAddedKeys <> nil then
    FAsAddedKeys.delete(FAsAddedKeys.indexOf(Key));
  KeyNotify(Key, Notification);
  ValueNotify(Result, Notification);
end;

procedure TFslMap<T>.Remove(const Key: String);
begin
  TFslObject(DoRemove(Key, Hash(Key), cnRemoved)).Free;
end;

procedure TFslMap<T>.RemoveKeys(const keyList: TStringList);
var
  key : String;
begin
  for key in KeyList do
    Remove(key);
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
  raise ELibraryException.create('unimplemented');
//  result := ToArrayImpl(Count);
end;

procedure TFslMap<T>.trackOrder;
begin
  if FAsAddedKeys <> nil then
    raise EFSLException.Create('Map '+FName+' is already tracking order');
  if Count > 0 then
    raise EFSLException.Create('Map '+FName+' already contains content');
  FAsAddedKeys := TStringList.create;
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
  else if FHasDefault then
  begin
    result := true;
    value := FDefault;
  end
  else
    Value := T(nil);
end;

procedure TFslMap<T>.DoAdd(HashCode, Index: Integer; const Key: String; const Value: T);
var
  i : integer;
begin
  FItems[Index].HashCode := HashCode;
  FItems[Index].Key := Key;
  FItems[Index].Value := Value;
  Inc(FCount);

  FreeAndNil(FSortedKeys);
  if (FAsAddedKeys <> nil) then
  begin
    i := FAsAddedKeys.indexOf(Key);
    if (i > -1) then
      FAsAddedKeys.delete(i);
    FAsAddedKeys.add(Key);
  end;
  KeyNotify(Key, cnAdded);
  ValueNotify(Value, cnAdded);
end;

function TFslMap<T>.DoGetEnumerator: TEnumerator<TFslPair<T>>;
begin
  Result := GetEnumerator;
end;

{$IFDEF FPC}
function TFslMap<T>.GetPtrEnumerator: TEnumerator<PT>;
begin
  result := nil;
end;
{$ENDIF}

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
    AddOrSetValue(s, T(other[s].link));
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
  Result := (GetBucketIndex(Key, Hash(Key)) >= 0);
end;

function TFslMap<T>.ContainsValue(const Value: T): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(FItems) - 1 do
    if (FItems[i].HashCode <> EMPTY_HASH) and (FItems[i].Value = Value) then
      Exit(True);
  if FHasDefault then
    result := FDefault = value
  else
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

function TFslMap<T>.sizeInBytes : cardinal;
var
  p : TFslPair<T>;
begin
  if self = nil then
    result := 0
  else
  begin
    result := sizeOf(self);
    inc(result, length(FItems) * Sizeof(TItem));
    if FSortedKeys <> nil then
      inc(result, sizeof(FSortedKeys) + FSortedKeys.Count * 2 * sizeof(pointer));
    if FAsAddedKeys <> nil then
      inc(result, sizeof(FAsAddedKeys) + FAsAddedKeys.Count * 2 * sizeof(pointer));
    if FDefault <> nil then
      inc(result, TFslObject(FDefault).sizeInBytes);
    inc(result, (length(FName) * sizeof(char))+12);
    for p in self do
    begin
      inc(result, (length(p.Key) * sizeof(char))+12);
      inc(result, p.Value.sizeInBytes);
    end;
  end;
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

{$IFDEF FPC}
function TFslMap<T>.TValueCollection.GetPtrEnumerator: TEnumerator<PT>;
begin
  result := nil;
end;
{$ENDIF}

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
  raise ELibraryException.create('unimplemented');
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

{$IFDEF FPC}
function TFslMap<T>.TKeyCollection.GetPtrEnumerator: TEnumerator<PT>;
begin
  result := nil;
end;
{$ENDIF}

function TFslMap<T>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(FMap);
end;

function TFslMap<T>.TKeyCollection.ToArray: TArray<String>;
begin
  raise ELibraryException.create('unimplemented');
//  Result := ToArrayImpl(FMap.Count);
end;

{ TFslStringDictionary }

procedure TFslStringDictionary.Free;
begin
  If Assigned(Self) and (InterlockedDecrement(FFslObjectReferenceCount) < 0) Then
    Destroy;
end;

function TFslStringDictionary.GetValue(const Key: String): String;
begin
  if not TryGetValue(key, result) then
    result := '';
end;

function TFslStringDictionary.Link: TFslStringDictionary;
begin
  Result := Self;
  If Assigned(Self) Then
    InterlockedIncrement(FFslObjectReferenceCount);
end;

procedure TFslStringDictionary.SetValue(const Key, Value: String);
begin
  Items[key] := value;
end;

function TFslStringDictionary.sizeInBytes : cardinal;
begin
  if self = nil then
    result := 0
  else
    result := sizeOf(self);
end;

procedure TFslStringDictionary.assign(source : TFslStringDictionary);
var
  s : String;
begin
  Clear;
  for s in source.Keys do
    Add(s, source[s]);
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
    raise ETodo.create('TFslStringSet.remove');
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

function TFslStringSet.sizeInBytesV : cardinal;
var
  s : String;
begin
  result := inherited sizeInBytesV;
  inc(result, length(FItems) * sizeof(pointer));
  for s in FItems do
    inc(result, (length(s) * 2) + 12);

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

procedure TFslStringMap.clear;
begin
  FDict.Clear;
end;

constructor TFslStringMap.Create;
begin
  inherited;
  FDict := TFslStringDictionary.create;
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
  FDict.AddOrSetValue(key, value);
end;

function TFslStringMap.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDict.sizeInBytes);
end;

{ ETodo }

constructor ETodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

{ EXmlTodo }

constructor EXmlTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

{ EDBTodo }

constructor EDBTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

{ ETerminologyTodo }

constructor ETerminologyTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

{ EJsonTodo }

constructor EJsonTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

{ TFslList<T>.TDefaultComparer }

function TFslList<T>.TDefaultComparer.compare(const l, r: T): integer;
var
  li, ri : NativeUInt;
begin
  li := NativeUInt(l);
  ri := NativeUInt(r);
  if (li = ri) then
    result := 0
  else if (li < ri) then
    result := -1
  else
    result := 1;
end;

{ TFslComparer<T> }

function TFslComparer<T>.link: TFslComparer<T>;
begin
  result  := TFslComparer<T>(inherited link);
end;

Initialization
  initUnit;
{$IFNDEF FPC}
  System.AbstractErrorProc := @AbstractHandler;
{$ENDIF}
//System.AssertErrorProc := @AssertionHandler;
finalization
  endUnit;
End.

