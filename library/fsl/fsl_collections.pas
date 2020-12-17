Unit fsl_collections;

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

Interface


Uses
  SysUtils,
  fsl_base, fsl_utilities;


Type
  TFslIterator = Class(TFslObject)
    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

    Public
      Procedure First; Virtual;
      Procedure Last; Virtual;
      Procedure Next; Virtual;
      Procedure Back; Virtual;
      Procedure Previous; Virtual;

      Function More : Boolean; Virtual;
  End;

  TFslIteratorClass = Class Of TFslIterator;

  TFslCollection = Class(TFslObject)
    Protected
      Function ErrorClass : EFslExceptionClass; Override;

      Procedure InternalClear; Virtual;

    Public
      Procedure BeforeDestruction; Override;

      Procedure Clear; Virtual;

      Function Iterator : TFslIterator; Overload; Virtual;
  End;

  EFslCollection = Class(EFslException);

  TFslItemListCompare = Function (pA, pB : Pointer) : Integer Of Object;
  PFslItemsCompare = ^TFslItemListCompare;

  TFslItemListDuplicates = (dupAccept, dupIgnore, dupException);

  TFslItemListDirection = Integer;

  TFslItemList = Class(TFslCollection)
    Private
      FCount : Integer;
      FCapacity : Integer;
      FSorted : Boolean;
      FComparison : TFslItemListCompare;
      FDuplicates : TFslItemListDuplicates;
      FDirection : TFslItemListDirection;

    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

      Function ValidateIndex(Const sMethod : String; iIndex : Integer) : Boolean; Virtual;

      Procedure SetCapacity(Const iValue: Integer); Virtual;
      Procedure SetCount(Const iValue : Integer); Virtual;

      Function GetItem(iIndex : Integer) : Pointer; Virtual; Abstract;
      Procedure SetItem(iIndex : Integer; pValue : Pointer); Virtual; Abstract;

      Procedure AssignItem(oItems : TFslItemList; iIndex : Integer); Virtual;
    function sizeInBytesV : cardinal; override;

      Function CompareItem(pA, pB : Pointer) : Integer; Virtual;
      Procedure DefaultCompare(Out aCompare : TFslItemListCompare); Virtual;
      Function Find(pValue : Pointer; Out iIndex : Integer; aCompare : TFslItemListCompare = Nil): Boolean; Overload; Virtual;

      Procedure DirectionBy(Const Value : TFslItemListDirection); Virtual;
      Procedure DuplicateBy(Const Value : TFslItemListDuplicates); Virtual;

      Procedure SortedBy(Const bValue : Boolean); Overload; Virtual;

      Procedure InternalGrow;
      Procedure InternalInsert(iIndex : Integer); Virtual;
      Procedure InternalResize(iValue : Integer); Virtual;
      Procedure InternalTruncate(iValue : Integer); Virtual;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Virtual;
      Procedure InternalEmpty(iIndex, iLength : Integer); Virtual;
      Procedure InternalExchange(iA, iB : Integer); Virtual;
      Procedure InternalDelete(iIndex : Integer); Virtual;

      Procedure InternalClear; Override;
      Procedure InternalSort; Overload;
      Procedure InternalSort(aCompare : TFslItemListCompare; iDirection : TFslItemListDirection = 0); Overload;

      Function Insertable(Const sMethod : String; iIndex : Integer) : Boolean; Overload; Virtual;
      Function Deleteable(Const sMethod : String; iIndex : Integer) : Boolean; Overload; Virtual;
      Function Deleteable(Const sMethod : String; iFromIndex, iToIndex : Integer) : Boolean; Overload; Virtual;
      Function Replaceable(Const sMethod : String; iIndex : Integer) : Boolean; Overload; Virtual;
      Function Extendable(Const sMethod : String; iCount : Integer) : Boolean; Overload; Virtual;

      // Attribute: items are allowed to be replaced with new items.
      Function Replacable : Boolean; Virtual;

      Function CapacityLimit : Integer; Virtual;
      Function CountLimit : Integer; Virtual;

      Property ItemByIndex[iIndex : Integer] : Pointer Read GetItem Write SetItem; Default;
      Property DefaultComparison : TFslItemListCompare Read FComparison;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Assign(oSource : TFslObject); Override;

      Procedure DeleteByIndex(iIndex : Integer); Virtual;
      Procedure DeleteRange(iFromIndex, iToIndex: Integer);
      Function ExistsByIndex(Const iIndex : Integer) : Boolean;
      Procedure Exchange(iA, iB : Integer); Virtual;

      Procedure IgnoreDuplicates;
      Procedure AllowDuplicates;
      Procedure PreventDuplicates;

      Function IsIgnoreDuplicates : Boolean;
      Function IsAllowDuplicates : Boolean;
      Function IsPreventDuplicates : Boolean;

      Function IsComparedBy(Const aCompare : TFslItemListCompare) : Boolean;
      Procedure ComparedBy(Const Value : TFslItemListCompare);

      Procedure Uncompared;
      Function IsCompared : Boolean;
      Function IsUnCompared : Boolean;

      Function IsSortedBy(Const aCompare : TFslItemListCompare) : Boolean;
      Procedure SortedBy(Const aCompare : TFslItemListCompare); Overload; Virtual;

      Procedure SortAscending;
      Procedure SortDescending;

      Function IsSortAscending : Boolean;
      Function IsSortDescending : Boolean;

      Procedure Sorted;
      Procedure Unsorted;
      Function IsSorted : Boolean;
      Function IsUnsorted : Boolean;

      Function IsOrderedBy(Const Value : TFslItemListCompare) : Boolean;
      Procedure OrderedBy(Const Value : TFslItemListCompare);
      Procedure Unordered;

      Function IsEmpty : Boolean;

      Property Count : Integer Read FCount Write SetCount;
      Property Capacity : Integer Read FCapacity Write SetCapacity;
  End;

  EFslItemList = Class(EFslCollection);

  TFslObjectIterator = Class(TFslIterator)
    Public
      Function Current : TFslObject; Virtual;
  End;

  TFslStringIterator = Class(TFslIterator)
    Public
      Function Current : String; Virtual;
  End;

  TFslIntegerIterator = Class(TFslIterator)
    Public
      Function Current : Integer; Virtual;
  End;

  TFslRealIterator = Class(TFslIterator)
    Public
      Function Current : Real; Virtual;
  End;

  TFslExtendedIterator = Class(TFslIterator)
    Public
      Function Current : Extended; Virtual;
  End;

  TFslBooleanIterator = Class(TFslIterator)
    Public
      Function Current : Boolean; Virtual;
  End;

  TFslLargeIntegerIterator = Class(TFslIterator)
    Public
      Function Current : Int64; Virtual;
  End;

  TFslPointerIterator = Class(TFslIterator)
    Public
      Function Current : Pointer; Virtual;
  End;

  TFslObjectClassIterator = Class(TFslIterator)
    Public
      Function Current : TClass; Virtual;
  End;

  TFslDateTimeIterator = Class(TFslIterator)
    Public
      Function Current : TDateTime; Virtual;
  End;

  TFslDurationIterator = Class(TFslIterator)
    Public
      Function Current : TDurationMS; Virtual;
  End;

  TFslCurrencyIterator = Class(TFslIterator)
    Public
      Function Current : TCurrency; Virtual;
  End;

  EFslIterator = Class(EFslException);


  TFslIntegerListItem = Integer;
  PFslIntegerListItem = ^TFslIntegerListItem;
  TFslIntegerListItemArray = Array[0..(MaxInt Div SizeOf(TFslIntegerListItem)) - 1] Of TFslIntegerListItem;
  PFslIntegerListItemArray = ^TFslIntegerListItemArray;

  TFslIntegerList = Class(TFslItemList)
    Private
      FIntegerArray : PFslIntegerListItemArray;

      Function GetIntegerByIndex(iIndex : Integer) : TFslIntegerListItem;
      Procedure SetIntegerByIndex(iIndex : Integer; Const aValue : TFslIntegerListItem);

      Function GetIntegers(iIndex : Integer) : TFslIntegerListItem;
      Procedure SetIntegers(iIndex : Integer; Const Value : TFslIntegerListItem);

    Protected
      Function GetAsText : String; 

      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue : Pointer); Override;

      Procedure AssignItem(oItems : TFslItemList; iIndex : Integer); Override;
    function sizeInBytesV : cardinal; override;

      Procedure InternalResize(iCapacity : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CapacityLimit : Integer; Override;

    Public
      Function Link : TFslIntegerList;
      Function Clone : TFslIntegerList;

      Function Add(aValue : TFslIntegerListItem) : Integer; Overload; 
      Procedure Insert(iIndex : Integer; aValue : TFslIntegerListItem); 
      Procedure Toggle(aValue : TFslIntegerListItem); 

      Function IndexByValue(Const iValue : TFslIntegerListItem) : Integer; 
      Function ExistsByValue(Const iValue : TFslIntegerListItem) : Boolean; 
      Procedure DeleteByValue(Const iValue : TFslIntegerListItem); 
      Procedure AddAll(oIntegers : TFslIntegerList);
      Procedure DeleteAllByValue(oIntegers : TFslIntegerList);
      Function EqualTo(oIntegers : TFslIntegerList) : Boolean;

      Function ExistsAll(oIntegerList : TFslIntegerList) : Boolean; 

      Function Iterator : TFslIterator; Override;

      Function Sum : Int64; 
      Function Mean : TFslIntegerListItem;

      Property AsText : String Read GetAsText;
      Property Integers[iIndex : Integer] : TFslIntegerListItem Read GetIntegers Write SetIntegers;
      Property IntegerByIndex[iIndex : Integer] : TFslIntegerListItem Read GetIntegerByIndex Write SetIntegerByIndex; Default;
  End; 

  TFslIntegerListIterator = Class(TFslIntegerIterator)
    Private
      FIntegerList : TFslIntegerList;
      FIndex : Integer;

      Procedure SetIntegerList(Const Value: TFslIntegerList);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : Integer; Override;

      Property IntegerList : TFslIntegerList Read FIntegerList Write SetIntegerList;
  End;


  TPointerItem = Pointer;
  TPointerItems = Array[0..(MaxInt Div SizeOf(TPointerItem)) - 1] Of TPointerItem;
  PPointerItems = ^TPointerItems;

  TFslPointerList = Class(TFslItemList)
    Private
      FPointerArray : PPointerItems;

      Function GetPointerByIndex(iIndex: Integer): TPointerItem;
      Procedure SetPointerByIndex(iIndex: Integer; Const pValue: TPointerItem);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue: Pointer); Override;

      Procedure AssignItem(oItems : TFslItemList; iIndex : Integer); Override;
    
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Override;

      Function CapacityLimit : Integer; Override;

    Public
      Function Iterator : TFslIterator; Override;

      Function IndexByValue(pValue : TPointerItem) : Integer;
      Function ExistsByValue(pValue : TPointerItem) : Boolean;
      Function Add(pValue : TPointerItem) : Integer;
      Procedure Insert(iIndex : Integer; pValue : TPointerItem);
      Procedure DeleteByValue(pValue : TPointerItem);
      Function RemoveFirst : TPointerItem;
      Function RemoveLast : TPointerItem;

      Property PointerByIndex[iIndex : Integer] : Pointer Read GetPointerByIndex Write SetPointerByIndex; Default;
  End;

  TFslPointerListIterator = Class(TFslPointerIterator)
    Private
      FPointerArray : TFslPointerList;
      FIndex : Integer;

      Procedure SetPointers(Const Value: TFslPointerList);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : Pointer; Override;

      Property Pointers : TFslPointerList Read FPointerArray Write SetPointers;
  End;

  TFslPointers = TFslPointerList;


  TFslClassList = Class(TFslPointerList)
    Private
      Function GetClassByIndex(Const iIndex : Integer): TClass;
      Procedure SetClassByIndex(Const iIndex : Integer; Const Value : TClass);

    Protected
      Function ItemClass : TFslObjectClass; Virtual;

    Public
      Function Iterator : TFslIterator; Override;

      Function Add(Const aClass : TClass) : Integer;
      Procedure AddAll(Const oClassList : TFslClassList);
      Procedure AddArray(Const aClasses : Array Of TClass);
      Function IndexByClassType(aClass : TClass) : Integer;
      Function ExistsByClassType(aClass : TClass) : Boolean;
      Function Find(Const aClass : TClass; Out iIndex : Integer) : Boolean;

      Property ClassByIndex[Const iIndex : Integer] : TClass Read GetClassByIndex Write SetClassByIndex; Default;
  End; 

  TFslClassListIterator = Class(TFslObjectClassIterator)
    Private
      FClassList : TFslClassList;
      FIndex : Integer;

      Procedure SetClassList(Const Value : TFslClassList);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : TClass; Override;

      Property ClassList : TFslClassList Read FClassList Write SetClassList;
  End;

  TFslHashEntryCode = Integer;

  TFslHashEntry = Class(TFslObject)
    Private
      FCode : TFslHashEntryCode;
      FNextHashEntry : TFslHashEntry;

    Protected
      Procedure Generate; Virtual;

      Property Code : TFslHashEntryCode Read FCode Write FCode;

    Public
      Procedure Assign(oSource : TFslObject); Override;

      Function Link : TFslHashEntry;
      Function Clone : TFslHashEntry;
  End;

  PFslHashEntry = ^TFslHashEntry;
  TFslHashEntryClass = Class Of TFslHashEntry;

  TFslHashEntryArray = Array [0..MaxInt Div SizeOf(TFslHashEntry) - 1] Of TFslHashEntry;
  PFslHashEntryArray = ^TFslHashEntryArray;

  TFslHashTable = Class(TFslCollection)
    Private
      FTable : PFslHashEntryArray;
      FCount : Integer;
      FCapacity : Integer;
      FThreshold : Integer;
      FBalance : Real;
      FPreventRehash : Boolean;

      Procedure SetCapacity(Const Value : Integer);
      Procedure SetBalance(Const Value : Real);

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

      Function Resolve(oHashEntry : TFslHashEntry) : Cardinal;
      Function Duplicate(oHashEntry : TFslHashEntry) : TFslHashEntry;

      Procedure Rehash;

      Procedure InternalClear; Override;

      Function Find(oSource, oHashEntry : TFslHashEntry) : TFslHashEntry;
      Procedure Insert(iIndex : Integer; oHashEntry: TFslHashEntry);

      Function Equal(oA, oB : TFslHashEntry) : Integer; Virtual;

      Function ItemClass : TFslHashEntryClass; Virtual;
      Function ItemNew : TFslHashEntry; Virtual;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslHashTable;
      Function Clone : TFslHashTable;

      Procedure Assign(oObject : TFslObject); Override;
      
      Procedure PreventRehash;
      Procedure AllowRehash;

      Procedure PredictCapacityByExpectedCount(Const iCount : Integer);

      Function ProduceHashEntry : TFslHashEntry;
      Procedure ConsumeHashEntry(oHashEntry : TFslHashEntry);

      Function Iterator : TFslIterator; Override;

      Function IsEmpty : Boolean;

      Procedure Add(oHashEntry : TFslHashEntry); Overload; Virtual;
      Function Delete(oHashEntry : TFslHashEntry) : Boolean; Overload; Virtual;
      Function Force(oHashEntry : TFslHashEntry) : TFslHashEntry; Virtual;
      Function Replace(oHashEntry : TFslHashEntry) : TFslHashEntry; Overload; Virtual;
      Function Get(oHashEntry : TFslHashEntry) : TFslHashEntry; Virtual;
      Function Exists(oHashEntry : TFslHashEntry) : Boolean; Overload; Virtual;

      Property Capacity : Integer Read FCapacity Write SetCapacity;
      Property Balance : Real Read FBalance Write SetBalance;
      Property Count : Integer Read FCount;
  End;

  PFslObject = ^TFslObject;
  TFslObjectArray = array[0..(MaxInt div SizeOf(TFslObject)) - 1] of TFslObject;
  PFslObjectArray = ^TFslObjectArray;

  TFslObjectListClass = class of TFslObjectList;
  TFslObjectListIterator = class;
  TFslObjectListIteratorClass = class of TFslObjectListIterator;

  TFslObjectList = class(TFslItemList)
  private
    FObjectArray: PFslObjectArray;

    FItemClass: TFslObjectClass;

    function GetObject(iIndex: integer): TFslObject;
    procedure SetObject(iIndex: integer; const oValue: TFslObject);

    function GetObjectByIndex(const iIndex: integer): TFslObject;
    procedure SetObjectByIndex(const iIndex: integer; const Value: TFslObject);

    function GetItemClass: TFslObjectClass;
    procedure SetItemClass(const Value: TFslObjectClass);

  protected
    function ErrorClass: EFslExceptionClass; override;

    function GetItem(iIndex: integer): Pointer; override;
    procedure SetItem(iIndex: integer; pValue: Pointer); override;
    function AddressOfItem(iIndex: integer): PFslObject;

    procedure AssignItem(oItems: TFslItemList; iIndex: integer); override;
    function sizeInBytesV : cardinal; override;

    procedure InternalTruncate(iValue: integer); override;
    procedure InternalResize(iValue: integer); override;
    procedure InternalCopy(iSource, iTarget, iCount: integer); override;
    procedure InternalEmpty(iIndex, iLength: integer); override;
    procedure InternalInsert(iIndex: integer); override;
    procedure InternalExchange(iA, iB: integer); override;
    procedure InternalDelete(iIndex: integer); override;

    function ValidateIndex(const sMethod: string; iIndex: integer): boolean;
      override;
    function ValidateItem(const sMethod: string; oObject: TFslObject;
      const sObject: string): boolean; virtual;

    function Find(pValue: Pointer; Out iIndex: integer;
      aCompare: TFslItemListCompare = nil): boolean; overload; override;
    function Find(oValue: TFslObject): integer; overload;

    function GetByIndex(iIndex: integer): TFslObject; overload;

    function CompareByClass(pA, pB: Pointer): integer; overload;
    function CompareByReference(pA, pB: Pointer): integer; overload;

    function ItemNew: TFslObject; virtual;
    function ItemClass: TFslObjectClass; virtual;
    function IteratorClass: TFslObjectListIteratorClass; virtual;
    function CapacityLimit: integer; override;

    function Insertable(const sMethod: string; oObject: TFslObject): boolean;
      overload;
    function Replaceable(const sMethod: string; oOld, oNew: TFslObject): boolean;
      overload;
    function Deleteable(const sMethod: string; oObject: TFslObject): boolean;
      overload;

    function Deleteable(const sMethod: string; iIndex: integer): boolean;
      overload; override;
    function Extendable(const sMethod: string; iCount: integer): boolean; override;

    procedure InternalAfterInclude(iIndex: integer; oObject : TFslObject); virtual;
    procedure InternalBeforeExclude(iIndex: integer; oObject : TFslObject); virtual;

    // Attribute
    function AllowUnassigned: boolean; virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Link: TFslObjectList;
    function Clone: TFslObjectList;

    procedure Collect(oList: TFslObjectList);
    procedure AddAll(oList: TFslObjectList);

    function Add(oValue: TFslObject): integer; overload; virtual;
    procedure Insert(iIndex: integer; oValue: TFslObject); overload;
    procedure Move(iSource, iTarget: integer); overload;

    function IndexBy(oValue: TFslObject; aCompare: TFslItemListCompare): integer;
    function ExistsBy(oValue: TFslObject; aCompare: TFslItemListCompare): boolean;
    function GetBy(oValue: TFslObject; aCompare: TFslItemListCompare): TFslObject;
    function ForceBy(oValue: TFslObject; aCompare: TFslItemListCompare): TFslObject;
    procedure DeleteBy(oValue: TFslObject; aCompare: TFslItemListCompare);

    procedure SortedByClass;
    procedure OrderedByClass;
    function IsSortedByClass: boolean;
    function IsOrderedByClass: boolean;
    function IndexByClass(aClass: TFslObjectClass): integer; overload;
    function IndexByClass(oValue: TFslObject): integer; overload;
    function GetByClass(aClass: TFslObjectClass): TFslObject;
    function ExistsByClass(aClass: TFslObjectClass): boolean;
    procedure DeleteByClass(aClass: TFslObjectClass);

    procedure SortedByReference;
    procedure OrderedByReference;
    function IsSortedByReference: boolean;
    function IsOrderedByReference: boolean;
    function IndexByReference(oValue: TFslObject): integer;
    function ExistsByReference(oValue: TFslObject): boolean;
    procedure DeleteByReference(oValue: TFslObject);
    procedure DeleteAllByReference(oValue: TFslObjectList);

    function ContainsAllByReference(oObjectList: TFslObjectList): boolean;
    function ContainsAnyByReference(oObjectList: TFslObjectList): boolean;

    function ExistsByDefault(oValue: TFslObject): boolean;
    function IndexByDefault(oValue: TFslObject): integer;
    procedure DeleteByDefault(oValue: TFslObject);

    function RemoveFirst: TFslObject;
    function RemoveLast: TFslObject;

    function Iterator: TFslIterator; override;
    function ProduceIterator: TFslObjectListIterator; overload;
    procedure ConsumeIterator(oIterator: TFslObjectListIterator); overload;

    function Get(iIndex: integer): TFslObject; overload;
    function New: TFslObject; overload; virtual;

    property ObjectByIndex[const iIndex: integer]: TFslObject
      read GetObjectByIndex write SetObjectByIndex; default;
    property Objects[const iIndex: integer]: TFslObject
      read GetObjectByIndex write SetObjectByIndex;
    property NominatedClass: TFslObjectClass read GetItemClass write SetItemClass;
  end;

  EFslObjectList = class(EFslItemList);

  TFslObjectListIterator = class(TFslObjectIterator)
  private
    FList: TFslObjectList;
    FIndex: integer;
    FDeleted: boolean;

    procedure SetList(const Value: TFslObjectList);
    function GetList: TFslObjectList;

  protected
    procedure StepBack;
    procedure StepNext;
    function Skip: boolean; virtual;

    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure First; override;
    procedure Last; override;
    procedure Next; override;
    procedure Back; override;
    function More: boolean; override;

    function Current: TFslObject; override;
    procedure Delete;

    property Index: integer read FIndex write FIndex;
    property List: TFslObjectList read GetList write SetList;
  end;

  TFslObject = fsl_base.TFslObject;
  TFslObjectClass = fsl_base.TFslObjectClass;

  TFslHashTableList = Class(TFslObjectList)
    Private
      Function GetHash(iIndex: Integer): TFslHashTable;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property Hashes[iIndex : Integer] : TFslHashTable Read GetHash; Default;
  End;

  TFslHashTableIterator = Class(TFslObjectIterator)
    Private
      FHashTable : TFslHashTable;
      FIndex : Integer;
      FCurrentHashEntry : TFslHashEntry;
      FNextHashEntry : TFslHashEntry;

      Function GetHashTable: TFslHashTable;
      Procedure SetHashTable(Const Value: TFslHashTable);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure First; Override;
      Procedure Next; Override;
      Function More : Boolean; Override;

      Function Count : Integer;
      Function Current : TFslObject; Override;

      Property HashTable : TFslHashTable Read GetHashTable Write SetHashTable;
  End;

  EFslHashTable = Class(EFslCollection);



  TFslStringObjectMatchKey = String;
  TFslStringObjectMatchValue = TFslObject;

  TFslStringObjectMatchItem = Record
    Key   : TFslStringObjectMatchKey;
    Value : TFslStringObjectMatchValue;
  End;

  PFslStringObjectMatchItem = ^TFslStringObjectMatchItem;

  TFslStringObjectMatchItemArray = Array[0..(MaxInt Div SizeOf(TFslStringObjectMatchItem)) - 1] Of TFslStringObjectMatchItem;
  PFslStringObjectMatchItemArray = ^TFslStringObjectMatchItemArray;

  TFslStringCompareCallback = Function (Const sA, sB : String) : Integer;

  TFslStringObjectMatch = Class(TFslItemList)
    Private
      FMatchArray : PFslStringObjectMatchItemArray;
      FDefaultKey : TFslStringObjectMatchKey;
      FDefaultValue : TFslStringObjectMatchValue;
      FCompareKey : TFslStringCompareCallback;
      FNominatedValueClass : TFslObjectClass;
      FSymbol : String;
      FForced : Boolean;

      Function GetKeyByIndex(iIndex: Integer): TFslStringObjectMatchKey;
      Procedure SetKeyByIndex(iIndex: Integer; Const aKey : TFslStringObjectMatchKey);

      Function GetValueByIndex(iIndex: Integer): TFslStringObjectMatchValue;
      Procedure SetValueByIndex(iIndex: Integer; Const aValue: TFslStringObjectMatchValue);

      Function GetMatch(Const aKey : TFslStringObjectMatchKey): TFslStringObjectMatchValue;
      Procedure SetMatch(Const aKey : TFslStringObjectMatchKey; Const aValue : TFslStringObjectMatchValue);

      Function GetMatchByIndex(iIndex: Integer): TFslStringObjectMatchItem;

      Function GetAsText: String;
      Procedure SetAsText(Const Value: String);

      Function GetSensitive: Boolean;
      Procedure SetSensitive(Const Value: Boolean);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue: Pointer); Override;

      Procedure AssignItem(oItems : TFslItemList; iIndex : Integer); Override;
    function sizeInBytesV : cardinal; override;

      Function ValidateIndex(Const sMethod: String; iIndex: Integer): Boolean; Override;
      Function ValidateItem(Const sMethod: String; oObject: TFslObject; Const sObject: String): Boolean; Virtual;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Override;
      Procedure InternalDelete(iIndex : Integer); Override;
      Procedure InternalExchange(iA, iB : Integer); Override;

      Function CompareByKey(pA, pB : Pointer): Integer; Virtual;
      Function CompareByValue(pA, pB : Pointer): Integer; Virtual;

      Procedure DefaultCompare(Out aCompare : TFslItemListCompare); Override;

      Function Find(Const aKey : TFslStringObjectMatchKey; Const aValue : TFslStringObjectMatchValue; Out iIndex : Integer; aCompare : TFslItemListCompare = Nil) : Boolean;

      Function CapacityLimit : Integer; Override;

      Function ItemClass : TFslObjectClass; Virtual;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslStringObjectMatch; Overload;

      Function IndexByKey(Const aKey : TFslStringObjectMatchKey) : Integer;
      Function IndexByValue(Const aValue : TFslStringObjectMatchValue) : Integer;

      Function ExistsByKey(Const aKey : TFslStringObjectMatchKey) : Boolean;
      Function ExistsByValue(Const aValue : TFslStringObjectMatchValue) : Boolean;

      Procedure AddAll(oSourceStringObjectMatch : TFslStringObjectMatch);

      Function Add(Const aKey : TFslStringObjectMatchKey; Const aValue : TFslStringObjectMatchValue): Integer; Overload;
      Procedure Insert(iIndex : Integer; Const aKey : TFslStringObjectMatchKey; Const aValue : TFslStringObjectMatchValue); Overload;
      Function FindByKey(Const aKey : TFslStringObjectMatchKey; Out iIndex : Integer) : Boolean; Overload;
      Function FindByValue(Const aValue : TFslStringObjectMatchValue; Out iIndex : Integer) : Boolean; Overload;

      Procedure SortedByKey;
      Procedure SortedByValue;

      Function IsSortedByKey : Boolean;
      Function IsSortedByValue : Boolean;

      Function GetKeyByValue(Const aValue : TFslStringObjectMatchValue) : TFslStringObjectMatchKey;
      Function GetValueByKey(Const aKey : TFslStringObjectMatchKey) : TFslStringObjectMatchValue;
      Procedure SetValueByKey(Const aKey : TFslStringObjectMatchKey; Const aValue : TFslStringObjectMatchValue);

      Procedure DeleteByKey(Const aKey : TFslStringObjectMatchKey);
      Procedure DeleteByValue(Const aValue : TFslStringObjectMatchValue);

      Property Matches[Const aKey : TFslStringObjectMatchKey] : TFslStringObjectMatchValue Read GetMatch Write SetMatch; Default;
      Property Forced : Boolean Read FForced Write FForced;
      Property DefaultKey : TFslStringObjectMatchKey Read FDefaultKey Write FDefaultKey;
      Property DefaultValue : TFslStringObjectMatchValue Read FDefaultValue Write FDefaultValue;
      Property AsText : String Read GetAsText Write SetAsText;
      Property Symbol : String Read FSymbol Write FSymbol;
      Property Sensitive : Boolean Read GetSensitive Write SetSensitive;
      Property NominatedValueClass : TFslObjectClass Read FNominatedValueClass Write FNominatedValueClass;
      Property MatchByIndex[iIndex : Integer] : TFslStringObjectMatchItem Read GetMatchByIndex;
      Property KeyByIndex[iIndex : Integer] : TFslStringObjectMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property Keys[iIndex : Integer] : TFslStringObjectMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property ValueByIndex[iIndex : Integer] : TFslStringObjectMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property Values[iIndex : Integer] : TFslStringObjectMatchValue Read GetValueByIndex Write SetValueByIndex;
  End;

Type
  TFslStringMatchKey = String;
  TFslStringMatchValue = String;

  TFslStringMatchItem = Record
    Key : TFslStringMatchKey;
    Value : TFslStringMatchValue;
  End;

  PFslStringMatchItem = ^TFslStringMatchItem;

  TFslStringMatchItems = Array[0..(MaxInt Div SizeOf(TFslStringMatchItem)) - 1] Of TFslStringMatchItem;
  PFslStringMatchItems = ^TFslStringMatchItems;

  TFslStringMatch = Class(TFslItemList)
    Private
      FMatchArray : PFslStringMatchItems;
      FDefaultValue : TFslStringMatchValue;
      FSymbol : String;
      FSeparator : String;
      FForced : Boolean;

      Function GetKeyByIndex(iIndex: Integer): TFslStringMatchKey;
      Procedure SetKeyByIndex(iIndex: Integer; Const aKey : TFslStringMatchKey);

      Function GetValueByIndex(iIndex: Integer): TFslStringMatchValue;
      Procedure SetValueByIndex(iIndex: Integer; Const aValue: TFslStringMatchValue);

      Function GetMatchByIndex(Const iIndex : Integer) : TFslStringMatchItem;
      Procedure SetMatchByIndex(Const iIndex : Integer; Const Value : TFslStringMatchItem);

      Function GetMatch(Const aKey : TFslStringMatchKey): TFslStringMatchValue;
      Procedure SetMatch(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue);

    Protected
      Function CapacityLimit : Integer; Override;

      Function ErrorClass : EFslExceptionClass; Override;

      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue: Pointer); Override;

      Procedure AssignItem(oItems : TFslItemList; iIndex : Integer); Override;
    function sizeInBytesV : cardinal; override;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CompareByCaseSensitiveKey(pA, pB : Pointer): Integer; Virtual;      
      Function CompareByKey(pA, pB : Pointer): Integer; Virtual;
      Function CompareByValue(pA, pB : Pointer): Integer; Virtual;
      Function CompareMatch(pA, pB: Pointer): Integer; Virtual;

      Procedure DefaultCompare(Out aCompare : TFslItemListCompare); Overload; Override;

      Function Find(Const aKey : TFslStringMatchKey; Const aValue: TFslStringMatchValue; Out iIndex : Integer; aCompare : TFslItemListCompare = Nil) : Boolean; Overload;

      Function GetAsText : String; 
      Procedure SetAsText(Const sValue: String); 

      Function DefaultValue(Const aKey : TFslStringMatchKey) : TFslStringMatchValue; Virtual; 

    Public
      constructor Create; Override;

      Procedure Assign(oObject : TFslObject); Override;

      Function Link : TFslStringMatch;
      Function Clone : TFslStringMatch; 

      Procedure AddAll(oStringMatch : TFslStringMatch);

      Function IndexByKey(Const aKey : TFslStringMatchKey) : Integer;
      Function IndexByCaseSensitiveKey(Const aKey : TFslStringMatchKey) : Integer;
      Function IndexByValue(Const aValue : TFslStringMatchValue) : Integer;
      Function ForceByKey(Const aKey : TFslStringMatchKey) : TFslStringMatchValue;
      Function ExistsByKey(Const aKey : TFslStringMatchKey) : Boolean;
      Procedure DeleteByKey(Const aKey : TFslStringMatchKey);
      Function ExistsByValue(Const aValue : TFslStringMatchValue) : Boolean;
      Function ExistsByKeyAndValue(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue) : Boolean;

      Function IndexOf(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue) : Integer;
      Function Add(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue): Integer;
      Procedure Insert(iIndex : Integer; Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue);

      Function EqualTo(oMatch : TFslStringMatch) : Boolean;

      Function GetValueByKey(Const aKey : TFslStringMatchKey): TFslStringMatchValue;
      Procedure SetValueByKey(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue);

      Procedure SortedByCaseSensitiveKey;
      Procedure SortedByKey;
      Procedure SortedByValue;

      Property Matches[Const aKey : TFslStringMatchKey] : TFslStringMatchValue Read GetMatch Write SetMatch; Default;
      Property Forced : Boolean Read FForced Write FForced;
      Property Default : TFslStringMatchValue Read FDefaultValue Write FDefaultValue;
      Property Symbol : String Read FSymbol Write FSymbol;
      Property Separator : String Read FSeparator Write FSeparator;
      Property MatchByIndex[Const iIndex : Integer] : TFslStringMatchItem Read GetMatchByIndex Write SetMatchByIndex;
      Property KeyByIndex[iIndex : Integer] : TFslStringMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property ValueByIndex[iIndex : Integer] : TFslStringMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property Keys[iIndex : Integer] : TFslStringMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property Values[iIndex : Integer] : TFslStringMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property AsText : String Read GetAsText Write SetAsText;
  End;

  EFslStringMatch = Class(EFslItemList);

  TFslStringListItem = String;
  TFslStringListItemArray = Array[0..(MaxInt Div SizeOf(TFslStringListItem)) - 1] Of TFslStringListItem;
  PFslStringListItemArray = ^TFslStringListItemArray;

  TFslStringHashEntry = Class(TFslHashEntry)
    Private
      FName : String;

      Procedure SetName(Const Value: String);

    Protected
      Procedure Generate; Override;

    function sizeInBytesV : cardinal; override;
    Public
      Procedure Assign(oSource : TFslObject); Override;
      
      Property Name : String Read FName Write SetName;
  End;

  TFslStringHashEntryClass = Class Of TFslStringHashEntry;

  TFslStringHashTable = Class(TFslHashTable)
    Protected
      Function Equal(oA, oB : TFslHashEntry) : Integer; Override;

      Function ItemClass : TFslHashEntryClass; Override;

    Public
      Function Link : TFslStringHashTable;

      Function Iterator : TFslIterator; Override;
  End;

  TFslStringHashTableIterator = Class(TFslHashTableIterator)
    Public
      Function Current : TFslStringHashEntry; Reintroduce;
  End;




  TFslObjectClassHashEntry = Class(TFslStringHashEntry)
    Private
      FData : TClass;

    Public
      Procedure Assign(oSource : TFslObject); Override;

      Property Data : TClass Read FData Write FData; // no set data as hashed classname may be different to FData.ClassName.
  End;

  TFslObjectClassHashTable = Class(TFslStringHashTable)
    Protected
      Function ItemClass : TFslHashEntryClass; Override;

    Public
      Function Iterator : TFslIterator; Override;
  End;

  TFslObjectClassHashTableIterator = Class(TFslObjectClassIterator)
    Private
      FInternal : TFslStringHashTableIterator;

      Function GetHashTable: TFslObjectClassHashTable;
      Procedure SetHashTable(Const Value: TFslObjectClassHashTable);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : TClass; Override;

      Property HashTable : TFslObjectClassHashTable Read GetHashTable Write SetHashTable;
  End;

  TFslStringList = Class(TFslItemList)
    Private
      FStringArray : PFslStringListItemArray;
      FSymbol : String;

      Function GetStringByIndex(iIndex : Integer) : TFslStringListItem;
      Procedure SetStringByIndex(iIndex : Integer; Const sValue : TFslStringListItem);

      Function GetSensitive : Boolean;
      Procedure SetSensitive(Const bValue: Boolean);

      Function GetAsCSV : String;
      Procedure SetAsCSV(Const sValue : String);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue : Pointer); Override;

      Function GetAsText : String; Virtual;
      Procedure SetAsText(Const sValue : String); Virtual;

      Procedure AssignItem(Items : TFslItemList; iIndex : Integer); Override;
    function sizeInBytesV : cardinal; override;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Procedure DefaultCompare(Out aCompare : TFslItemListCompare); Overload; Override;

      Function CompareInsensitive(pA, pB : Pointer): Integer;
      Function CompareSensitive(pA, pB : Pointer): Integer;

      Function CapacityLimit : Integer; Override;

    Public
      constructor Create; Override;

      Function Link : TFslStringList;
      Function Clone : TFslStringList;

      Procedure SaveToText(Const sFilename : String);
      Procedure LoadFromText(Const sFilename : String);

      Function Iterator : TFslIterator; Override;

      Procedure AddAll(oStrings : TFslStringList);
      Procedure AddAllStringArray(Const aValues : Array Of String);

      Function IndexByValue(Const sValue : TFslStringListItem) : Integer;
      Function ExistsByValue(Const sValue : TFslStringListItem) : Boolean;
      Function Add(Const sValue : TFslStringListItem) : Integer;
      Procedure Insert(iIndex : Integer; Const sValue : TFslStringListItem);
      Procedure DeleteByValue(Const sValue : TFslStringListItem); 

      Function Equals(oStrings : TFslStringList) : Boolean; Reintroduce;
      Function Compare(oStrings : TFslStringList) : Integer;
      Function ExistsAny(oStrings : TFslStringList) : Boolean;

      Function Find(Const sValue : TFslStringListItem; Out iIndex : Integer; aCompare : TFslItemListCompare = Nil) : Boolean; Overload;

      // script wrappers
      procedure SetItems(iIndex : integer; sValue : String);
      function GetItems(iIndex : integer) : String;
      procedure delete(iIndex : integer);
      procedure populate(iCount : integer);
      Function IndexOf(value : String): Integer;
      Property Items[iIndex : Integer] : TFslStringListItem Read GetStringByIndex Write SetStringByIndex;
      Property Text : String read GetAsText write SetAsText;

      Property StringByIndex[iIndex : Integer] : TFslStringListItem Read GetStringByIndex Write SetStringByIndex; Default;
      Property AsText : String Read GetAsText Write SetAsText;
      Property AsCSV : String Read GetAsCSV Write SetAsCSV;
      Property Symbol : String Read FSymbol Write FSymbol;
      Property Sensitive : Boolean Read GetSensitive Write SetSensitive;
  End;

  TFslStringListIterator = Class(TFslStringIterator)
    Private
      FStringList : TFslStringList;
      FIndex : Integer;

      Procedure SetStringList(Const Value: TFslStringList);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : String; Override;

      Property StringList : TFslStringList Read FStringList Write SetStringList;
  End;


  TFslStringIntegerMatchKey = String;
  TFslStringIntegerMatchValue = Integer;

  TFslStringIntegerMatchItem = Record
    Key : TFslStringIntegerMatchKey;
    Value : TFslStringIntegerMatchValue;
  End;

  PFslStringIntegerMatchItem = ^TFslStringIntegerMatchItem;

  TFslStringIntegerMatchItemArray = Array[0..(MaxInt Div SizeOf(TFslStringIntegerMatchItem)) - 1] Of TFslStringIntegerMatchItem;
  PFslStringIntegerMatchItemArray = ^TFslStringIntegerMatchItemArray;

  TFslStringIntegerMatch = Class(TFslItemList)
    Private
      FMatchArray : PFslStringIntegerMatchItemArray;
      FDefaultKey : TFslStringIntegerMatchKey;
      FDefaultValue : TFslStringIntegerMatchValue;
      FForced : Boolean;
      FCompareKey : TFslStringCompareCallback;
      FSymbol : String;

      Function GetKey(iIndex : Integer): String;
      Procedure SetKey(iIndex : Integer; Const aKey : TFslStringIntegerMatchKey);

      Function GetValue(iIndex : Integer): Integer;
      Procedure SetValue(iIndex : Integer; Const aValue : TFslStringIntegerMatchValue);

      Function GetMatch(Const aKey : TFslStringIntegerMatchKey): TFslStringIntegerMatchValue;
      Procedure SetMatch(Const aKey : TFslStringIntegerMatchKey; Const aValue : TFslStringIntegerMatchValue);

      Function GetAsText : String;
      Function GetKeysAsText : String;

      Function GetSensitive: Boolean;
      Procedure SetSensitive(Const Value: Boolean);

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex: Integer; pValue: Pointer); Override;

      Procedure AssignItem(oItems : TFslItemList; iIndex : Integer); Override;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;

      Function CompareKey(pA, pB : Pointer) : Integer; Virtual;
      Function CompareValue(pA, pB : Pointer) : Integer; Virtual;

      Procedure DefaultCompare(Out aCompare : TFslItemListCompare); Overload; Override;

      Function Find(Const aKey : TFslStringIntegerMatchKey; Const aValue : TFslStringIntegerMatchValue; Out iIndex : Integer; aCompare : TFslItemListCompare = Nil) : Boolean; Overload;

      Function CapacityLimit : Integer; Override;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslStringIntegerMatch; 
      Function Clone : TFslStringIntegerMatch;

      Procedure AddAll(oStringIntegerMatch : TFslStringIntegerMatch);

      Function IndexByKey(Const aKey : TFslStringIntegerMatchKey) : Integer; 
      Function ExistsByKey(Const aKey : TFslStringIntegerMatchKey) : Boolean;
      Function IndexByValue(Const aValue : TFslStringIntegerMatchValue) : Integer;
      Function ExistsByValue(Const aValue : TFslStringIntegerMatchValue) : Boolean;
      Function Add(Const aKey : TFslStringIntegerMatchKey; Const aValue : TFslStringIntegerMatchValue) : Integer; Overload;
      Function ForceByKey(Const aKey : TFslStringIntegerMatchKey) : TFslStringIntegerMatchValue;
      Procedure Insert(iIndex : Integer; Const aKey : TFslStringIntegerMatchKey; Const aValue : TFslStringIntegerMatchValue); Overload;

      Function GetValueByKey(Const aKey : TFslStringIntegerMatchKey) : TFslStringIntegerMatchValue;
      Function GetKeyByValue(Const aValue : TFslStringIntegerMatchValue) : TFslStringIntegerMatchKey;

      Function IsSortedByKey: Boolean;
      Function IsSortedByValue: Boolean;

      Procedure SortedByKey;
      Procedure SortedByValue;

      Property Matches[Const sIndex : TFslStringIntegerMatchKey] : TFslStringIntegerMatchValue Read GetMatch Write SetMatch; Default;
      Property KeyByIndex[iIndex : Integer] : TFslStringIntegerMatchKey Read GetKey Write SetKey;
      Property ValueByIndex[iIndex : Integer] : TFslStringIntegerMatchValue Read GetValue Write SetValue;
      Property Forced : Boolean Read FForced Write FForced;
      Property DefaultKey : TFslStringIntegerMatchKey Read FDefaultKey Write FDefaultKey;
      Property DefaultValue : TFslStringIntegerMatchValue Read FDefaultValue Write FDefaultValue;
      Property KeysAsText : String Read GetKeysAsText;
      Property AsText : String Read GetAsText;
      Property Symbol : String Read FSymbol Write FSymbol;
      Property Sensitive : Boolean Read GetSensitive Write SetSensitive;
  End;

  EFslStringIntegerMatch = Class(EFslException);



  TFslOrdinalSetPart = Integer;
  PFslOrdinalSetPart = ^TFslOrdinalSetPart;
  TFslOrdinalSetPartArray = Array[0..7] Of TFslOrdinalSetPart;
  PFslOrdinalSetPartArray = ^TFslOrdinalSetPartArray;

  TFslOrdinalSet = Class(TFslCollection)
    Private
      FOwns : Boolean;
      FPartArray : PFslOrdinalSetPartArray; // pointer to the block of memory associated with the set.
      FCount : Integer;             // number of used bits in the Parts data.
      FSize : Integer;

      Procedure SetCount(Const iValue : Integer);
      Procedure SetSize(Const Value: Integer);

      Function GetIsChecked(Const iIndex: Integer): Boolean;
      Procedure SetIsChecked(Const iIndex: Integer; Const Value: Boolean);

    Protected
      Procedure PartsNew;
      Procedure PartsDispose;

      Procedure Resize(Const iValue : Integer);

      Procedure Fill(bChecked : Boolean);

      Function ValidateIndex(Const sMethod : String; Const iIndex : Integer) : Boolean;

    Public
      destructor Destroy; Override;

      Procedure Assign(oObject : TFslObject); Override;
      
      Function Iterator : TFslIterator; Override;

      Procedure New(Const iCount : Integer);
      Procedure Hook(Const aValue; iCount : Integer); Virtual; // no overload as we have an untyped parameter.
      Procedure Unhook;

      Procedure CheckRange(iFromIndex, iToIndex : Integer);
      Procedure Check(iIndex : Integer);
      Procedure Uncheck(iIndex : Integer);
      Procedure UncheckRange(iFromIndex, iToIndex : Integer);
      Procedure Toggle(iIndex : Integer);
      Procedure CheckAll;
      Procedure UncheckAll;

      Function Checked(iIndex: Integer): Boolean;
      Function CheckedRange(iFromIndex, iToIndex : Integer): Boolean;      
      Function AnyChecked : Boolean;
      Function AllChecked : Boolean;
      Function NoneChecked : Boolean;

      Property Parts : PFslOrdinalSetPartArray Read FPartArray Write FPartArray;
      Property Owns : Boolean Read FOwns Write FOwns;
      Property Count : Integer Read FCount Write SetCount;
      Property Size : Integer Read FSize Write SetSize;
      Property IsChecked[Const iIndex : Integer] : Boolean Read GetIsChecked Write SetIsChecked; Default;
  End;

  TFslOrdinalSetIterator = Class(TFslIterator)
    Private
      FOrdinalSet : TFslOrdinalSet;
      FValue : PFslOrdinalSetPart;
      FPart : Integer;
      FLoop : Integer;
      FIndex : Integer;

      Procedure SetOrdinalSet(Const Value: TFslOrdinalSet);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure First; Overload; Override;
      Procedure Next; Overload; Override;
      Function More : Boolean; Overload; Override;
      Function Checked : Boolean;

      Procedure Check;
      Procedure Uncheck;

      Property Index : Integer Read FIndex;
      Property OrdinalSet : TFslOrdinalSet Read FOrdinalSet Write SetOrdinalSet;
  End;

  TFslObjectMatchKey = TFslObject;
  TFslObjectMatchValue = TFslObject;

  TFslObjectMatchItem = Record
    Key : TFslObjectMatchKey;
    Value : TFslObjectMatchValue;
  End;

  PFslObjectMatchItem = ^TFslObjectMatchItem;

  TFslObjectMatchItemArray = Array[0..(MaxInt Div SizeOf(TFslObjectMatchItem)) - 1] Of TFslObjectMatchItem;
  PFslObjectMatchItemArray = ^TFslObjectMatchItemArray;

  TFslObjectMatch = Class(TFslItemList)
    Private
      FMatchArray : PFslObjectMatchItemArray;
      FDefaultKey : TFslObjectMatchKey;
      FDefaultValue : TFslObjectMatchValue;
      FForced : Boolean;
      FNominatedKeyClass : TFslObjectClass;
      FNominatedValueClass : TFslObjectClass;

      FKeyComparisonDelegate : TFslItemListCompare;
      FValueComparisonDelegate : TFslItemListCompare;

      Function GetKeyByIndex(iIndex: Integer): TFslObjectMatchKey;
      Procedure SetKeyByIndex(iIndex: Integer; Const aKey : TFslObjectMatchKey);

      Function GetValueByIndex(iIndex: Integer): TFslObjectMatchValue;
      Procedure SetValueByIndex(iIndex: Integer; Const aValue: TFslObjectMatchValue);

      Function GetMatchByIndex(iIndex : Integer) : TFslObjectMatchItem;

      Function GetAsText : String;

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; aValue: Pointer); Override;

      Procedure AssignItem(oItems : TFslItemList; iIndex : Integer); Override;
    function sizeInBytesV : cardinal; override;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function ValidateIndex(Const sMethod : String; iIndex : Integer): Boolean; Overload; Override;

      Function CapacityLimit : Integer; Override;

      Procedure DefaultCompare(Out aCompare : TFslItemListCompare); Overload; Override;

      Function CompareByKeyReference(pA, pB : Pointer): Integer;
      Function CompareByValueReference(pA, pB : Pointer): Integer;

      Function Find(Const aKey : TFslObjectMatchKey; Const aValue: TFslObjectMatchValue; Out iIndex : Integer; aCompare : TFslItemListCompare = Nil): Boolean; Overload;

      Function ValidateKey(Const sMethod : String; oObject : TFslObject; Const sObject : String) : Boolean; Virtual;
      Function ValidateValue(Const sMethod : String; oObject : TFslObject; Const sObject : String) : Boolean; Virtual;

      Function ItemKeyClass : TFslObjectClass; Virtual;
      Function ItemValueClass : TFslObjectClass; Virtual;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslObjectMatch; Overload;

      Function IndexByValue(Const aValue : TFslObjectMatchValue) : Integer;
      Function IndexByKey(Const aKey : TFslObjectMatchKey) : Integer;
      Function ExistsByValue(Const aValue : TFslObjectMatchValue) : Boolean;
      Function ExistsByKey(Const aKey : TFslObjectMatchKey) : Boolean;
      Function Add(Const aKey : TFslObjectMatchKey; Const aValue : TFslObjectMatchValue): Integer;
      Procedure Insert(iIndex : Integer; Const aKey : TFslObjectMatchKey; Const aValue : TFslObjectMatchValue);
      Procedure Delete(Const aKey : TFslObjectMatchKey; Const aValue : TFslObjectMatchValue);
      Function GetKeyByValue(Const aValue : TFslObjectMatchValue) : TFslObjectMatchKey;

      Function GetValueByKey(Const aKey : TFslObjectMatchKey): TFslObjectMatchValue;
      Procedure SetValueByKey(Const aKey: TFslObjectMatchKey; Const aValue: TFslObjectMatchValue);

      Procedure Merge(oMatch : TFslObjectMatch);

      Procedure SortedByKey;
      Procedure SortedByValue;

      Function IsSortedByKey : Boolean;
      Function IsSortedByValue : Boolean;

      Property Forced : Boolean Read FForced Write FForced;
      Property DefaultKey : TFslObjectMatchKey Read FDefaultKey Write FDefaultKey;
      Property DefaultValue : TFslObjectMatchValue Read FDefaultValue Write FDefaultValue;
      Property KeyComparisonDelegate : TFslItemListCompare Read FKeyComparisonDelegate Write FKeyComparisonDelegate;
      Property ValueComparisonDelegate : TFslItemListCompare Read FValueComparisonDelegate Write FValueComparisonDelegate;
      Property MatchByIndex[iIndex : Integer] : TFslObjectMatchItem Read GetMatchByIndex;
      Property KeyByIndex[iIndex : Integer] : TFslObjectMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property ValueByIndex[iIndex : Integer] : TFslObjectMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property Keys[iIndex : Integer] : TFslObjectMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property Values[iIndex : Integer] : TFslObjectMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property NominatedKeyClass : TFslObjectClass Read FNominatedKeyClass Write FNominatedKeyClass;
      Property NominatedValueClass : TFslObjectClass Read FNominatedValueClass Write FNominatedValueClass;
      Property AsText : String Read GetAsText;
  End;

  TFslName = Class(TFslObject)
    Private
      FName : String;

    Protected
      Function GetName: String; Virtual;
      Procedure SetName(Const Value: String); Virtual;

    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TFslName;
      Function Clone : TFslName;

      Procedure Assign(oSource : TFslObject); Override;
      
      Property Name : String Read GetName Write SetName;
  End;

  TFslNameClass = Class Of TFslName;

  TFslNameList = Class(TFslObjectList)
    Private
      FSymbol : String;

      Function GetName(iIndex : Integer) : TFslName;
      Procedure SetName(iIndex : Integer; oName : TFslName);

      Function GetAsText: String;
      Procedure SetAsText(Const Value: String);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByName(pA, pB: Pointer): Integer; Virtual;

      Procedure DefaultCompare(Out aEvent : TFslItemListCompare); Override;

      Function FindByName(Const sName: String; Out iIndex: Integer): Boolean; Overload;
      Function FindByName(oName : TFslName; Out iIndex: Integer): Boolean; Overload;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;

      Function Link : TFslNameList;
      Function Clone : TFslNameList;

      Procedure SortedByName;
      Function IsSortedByName : Boolean;

      Function IndexByName(Const sName : String) : Integer; Overload;
      Function IndexByName(Const oName : TFslName) : Integer; Overload;
      Function ExistsByName(Const oName : TFslName) : Boolean; Overload;
      Function ExistsByName(Const sName : String) : Boolean; Overload;
      Function GetByName(Const sName : String) : TFslName; Overload;
      Function GetByName(oName : TFslName) : TFslName; Overload;
      Function EnsureByName(Const sName : String) : TFslName; Overload;
      Function ForceByName(Const sName : String) : TFslName;
      Procedure RemoveByName(Const sName : String);
      Function AddByName(Const sName : String) : Integer;

      Property Names[iIndex : Integer] : TFslName Read GetName Write SetName; Default;
      Property AsText : String Read GetAsText Write SetAsText;
      Property Symbol : String Read FSymbol Write FSymbol;
  End;

  TFslCharacter = Class(TFslObject)
    Private
      FValue : Char;

    Public
      Procedure Assign(oObject : TFslObject); Override;
      
      Property Value : Char Read FValue Write FValue;
  End;

  TFslCharacterList = Class(TFslObjectList)
    Private
      Function GetCharacterByIndex(Const iIndex : Integer) : TFslCharacter;

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByValue(pA, pB : Pointer) : Integer;

    Public
      Procedure AddCharacter(Const cValue : Char);
      Procedure AddCharacterSet(Const aValueSet : TCharSet);

      Procedure SortByValue;

      Function ExistsByValue(Const cValue : Char) : Boolean;

      Property CharacterByIndex[Const iIndex : Integer] : TFslCharacter Read GetCharacterByIndex; Default;
  End;

  TFslObjectChoice = Class(TFslObject)
    Private
      FObject : TFslObject;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function RetrieveObject(Const aObjectClass: TFslObjectClass): TFslObject;
      Procedure StoreObject(Const oObject : TFslObject; Const aObjectClass : TFslObjectClass);

      Function RetrieveIsObjectClass(Const aObjectClass: TFslObjectClass) : Boolean;
      Procedure StoreIsObjectClass(Const aObjectClass: TFslObjectClass; Const bStored : Boolean);

      Function RetrieveIsNull : Boolean;
      Procedure StoreIsNull;
  End;


Type
  TFslBooleanItem = Boolean;
  TFslBooleanItemArray = Array[0..(MaxInt Div SizeOf(TFslBooleanItem)) - 1] Of TFslBooleanItem;
  PFslBooleanItemArray = ^TFslBooleanItemArray;

  TFslBooleanList = Class(TFslItemList)
    Private
      FBooleanArray : PFslBooleanItemArray;

      Function GetBooleanByIndex(iIndex : Integer) : TFslBooleanItem;
      Procedure SetBooleanByIndex(iIndex : Integer; Const iValue : TFslBooleanItem);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue : Pointer); Override;

      Procedure AssignItem(oItems : TFslItemList; iIndex : Integer); Override;

      // CompareItem implemented by TFslItems integer comparison of the return of GetItem.

      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CapacityLimit : Integer; Override;

    Public
      Function Iterator : TFslIterator; Override;

      Function IndexByValue(aValue : TFslBooleanItem) : Integer;
      Function ExistsByValue(aValue : TFslBooleanItem) : Boolean;
      Function Add(aValue : TFslBooleanItem) : Integer; Overload;
      Procedure Insert(iIndex : Integer; aValue : TFslBooleanItem);
      Procedure DeleteByValue(aValue : TFslBooleanItem);

      Procedure Add(oBooleans: TFslBooleanList); Overload;

      Procedure Select(bValue : Boolean);
      Procedure Invert;
      Function CountOf(bValue : Boolean) : Integer;

      Property BooleanByIndex[iIndex : Integer] : TFslBooleanItem Read GetBooleanByIndex Write SetBooleanByIndex; Default;
  End;

  TFslBooleanListIterator = Class(TFslBooleanIterator)
    Private
      FBooleanList : TFslBooleanList;
      FIndex    : Integer;

      Function GetBooleanList : TFslBooleanList;
      Procedure SetBooleanList(Const Value : TFslBooleanList);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : Boolean; Override;

      Property BooleanList : TFslBooleanList Read GetBooleanList Write SetBooleanList;
  End;

  TFslCharacterSet = Class(TFslOrdinalSet)
  Private
    FDataSet : TCharSet;

    Function GetAsText: String;
    Procedure SetAsText(Const Value: String);

  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Procedure AddRange(Const aFromValue, aToValue : Char);
    Procedure AddValue(Const aValue : Char);
    Function ContainsValue(Const aValue : Char) : Boolean;

    Property AsText : String Read GetAsText Write SetAsText;
    Property Data : TCharSet Read FDataSet Write FDataSet;
  End;



Type
  TFslTreeList = Class;

  TFslTreeListClass = Class Of TFslTreeList;

  TFslTreeClass = Class Of TFslTree;

  TFslTree = Class(TFslObject)
    Private
      FParent : TFslTree;
      FChildren : TFslTreeList;

      Function GetParent: TFslTree;
      Procedure SetParent(Const Value: TFslTree);

      Function GetChildren: TFslTreeList;
      Procedure SetChildren(Const Value: TFslTreeList);

    Protected
      Function ChildrenClass : TFslTreeListClass; Virtual;
      Function ChildrenNew : TFslTreeList; Virtual;
      Function ParentClass : TFslTreeClass; Virtual;

      Procedure Reparent; Virtual;

    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function ClassType : TFslTreeClass;
      Function Clone : TFslTree;
      Function Link : TFslTree;

      Procedure Assign(oObject : TFslObject); Override;

      Function HasParent : Boolean;
      Function HasChildren : Boolean;

      Property Parent : TFslTree Read GetParent Write SetParent;
      Property Children : TFslTreeList Read GetChildren Write SetChildren;
  End;

  TFslTreeList = Class(TFslObjectList)
    Private
      FParent : TFslTree;

      Function GetTree(iIndex: Integer): TFslTree;

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Procedure InternalAfterInclude(iIndex : Integer; oObject : TFslObject); Override;
      Procedure InternalBeforeExclude(iIndex : Integer; oObject : TFslObject); Override;

      Property Parent : TFslTree Read FParent Write FParent; // the only circumstance when you use this is when you have a node with other sets of children

    Public
      Property Trees[iIndex : Integer] : TFslTree Read GetTree; Default;
  End;


Implementation

uses
  fsl_stream;


Procedure TFslCollection.BeforeDestruction;
Begin
  InternalClear;

  Inherited;
End;


Procedure TFslCollection.Clear;
Begin 
  InternalClear;
End;  


Procedure TFslCollection.InternalClear;
Begin
End;  


Function TFslCollection.ErrorClass : EFslExceptionClass;
Begin 
  Result := EFslCollection;
End;  


Function TFslCollection.Iterator : TFslIterator;
Begin 
  RaiseError('Iterator', 'No iterator specified.');

  Result := Nil;
End;

Procedure TFslIntegerList.AssignItem(oItems : TFslItemList; iIndex : Integer);
Begin 
  FIntegerArray^[iIndex] := TFslIntegerList(oItems).FIntegerArray^[iIndex];
End;  


Function TFslIntegerList.Clone: TFslIntegerList;
Begin 
  Result := TFslIntegerList(Inherited Clone);
End;


Function TFslIntegerList.Link: TFslIntegerList;
Begin 
  Result := TFslIntegerList(Inherited Link);
End;  


Procedure TFslIntegerList.InternalEmpty(iIndex, iLength : Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(NativeUInt(FIntegerArray) + NativeUInt((iIndex * SizeOf(TFslIntegerListItem)))), (iLength * SizeOf(TFslIntegerListItem)));
End;  


Procedure TFslIntegerList.InternalResize(iCapacity : Integer);
Begin 
  Inherited;
  
  MemoryResize(FIntegerArray, Capacity * SizeOf(TFslIntegerListItem), iCapacity * SizeOf(TFslIntegerListItem));
End;


Procedure TFslIntegerList.InternalCopy(iSource, iTarget, iCount : Integer);
Begin 
  Inherited;

  MemoryMove(@FIntegerArray^[iSource], @FIntegerArray^[iTarget], iCount * SizeOf(TFslIntegerListItem));
End;  


Function TFslIntegerList.IndexByValue(Const iValue : TFslIntegerListItem): Integer;
Begin
  If Not Find(Pointer(iValue), Result) Then
    Result := -1;
End;


Function TFslIntegerList.ExistsByValue(Const iValue : TFslIntegerListItem) : Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(iValue));
End;


Function TFslIntegerList.Add(aValue : TFslIntegerListItem): Integer;
Begin 
  Result := -1;

  If Not IsAllowDuplicates And Find(Pointer(aValue), Result) Then
  Begin 
    If IsPreventDuplicates Then
      RaiseError('Add', StringFormat('Item already exists in list (%d)', [aValue]));
  End
  Else
  Begin 
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(Pointer(aValue), Result);

    Insert(Result, aValue);
  End;  
End;  


Procedure TFslIntegerList.Insert(iIndex : Integer; aValue : TFslIntegerListItem);
Begin 
  InternalInsert(iIndex);

  FIntegerArray^[iIndex] := aValue;
End;


Procedure TFslIntegerList.DeleteByValue(Const iValue: TFslIntegerListItem);
Var
  iIndex : Integer;
Begin
  If Not Find(Pointer(iValue), iIndex) Then
    RaiseError('DeleteByValue', StringFormat('''%d'' not found in list', [iValue]));

  DeleteByIndex(iIndex);
End;


Procedure TFslIntegerList.DeleteAllByValue(oIntegers: TFslIntegerList);
Var
  iIndex : Integer;
Begin
  For iIndex := 0 To oIntegers.Count - 1 Do
    DeleteByValue(oIntegers[iIndex]);
End;


Procedure TFslIntegerList.InternalExchange(iA, iB : Integer);
Var
  aTemp : TFslIntegerListItem;
  pA    : Pointer;
  pB    : Pointer;
Begin 
  pA := @FIntegerArray^[iA];
  pB := @FIntegerArray^[iB];

  aTemp := TFslIntegerListItem(pA^);
  TFslIntegerListItem(pA^) := TFslIntegerListItem(pB^);
  TFslIntegerListItem(pB^) := aTemp;
End;  


Procedure TFslIntegerList.AddAll(oIntegers: TFslIntegerList);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oIntegers.Count - 1 Do
    Add(oIntegers[iLoop]);
End;


Function TFslIntegerList.EqualTo(oIntegers : TFslIntegerList) : Boolean;
Var
  iLoop : Integer;
  iCount : Integer;
Begin 
  Assert(Invariants('EqualTo', oIntegers, TFslIntegerList, 'oIntegers'));

  If IsAllowDuplicates Then
    RaiseError('EqualTo', 'Equality checking not supported by integer collection containing duplicates.');

  Result := oIntegers.Count = Count;

  iLoop := 0;
  iCount := Count;

  While Result And (iLoop < iCount) Do
  Begin 
    Result := oIntegers.ExistsByValue(IntegerByIndex[iLoop]);

    Inc(iLoop);
  End;  
End;


Procedure TFslIntegerList.Toggle(aValue : TFslIntegerListItem);
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByValue(aValue);

  If ExistsByIndex(iIndex) Then
    DeleteByIndex(iIndex)
  Else
    Add(aValue);
End;  


Function TFslIntegerList.GetAsText : String;
Var
  iIndex : Integer;
Begin
  Result := '';

  For iIndex := 0 To Count-1 Do
    StringAppend(Result, IntegerToString(IntegerByIndex[iIndex]), ', ');
End;


Function TFslIntegerList.GetItem(iIndex : Integer): Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := Pointer(FIntegerArray^[iIndex]);
End;


Procedure TFslIntegerList.SetItem(iIndex : Integer; pValue : Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FIntegerArray^[iIndex] := TFslIntegerListItem(pValue);
End;  


Function TFslIntegerList.GetIntegerByIndex(iIndex : Integer): TFslIntegerListItem;
Begin
  Assert(ValidateIndex('GetIntegerByIndex', iIndex));

  Result := FIntegerArray^[iIndex];
End;  


Procedure TFslIntegerList.SetIntegerByIndex(iIndex : Integer; Const aValue : TFslIntegerListItem);
Begin
  Assert(ValidateIndex('SetIntegerByIndex', iIndex));

  FIntegerArray^[iIndex] := aValue;
End;  


Function TFslIntegerList.Sum : Int64;
Var
  iLoop : Integer;
Begin 
  Result := 0;
  For iLoop := 0 To Count - 1 Do
    Inc(Result, IntegerByIndex[iLoop]);
End;  


Function TFslIntegerList.Mean : TFslIntegerListItem;
Begin 
  If Count > 0 Then
    Result := Sum Div Count
  Else
    Result := 0;
End;  


Function TFslIntegerList.CapacityLimit : Integer;
Begin
  Result := High(TFslIntegerListItemArray);
End;  


Function TFslIntegerList.Iterator : TFslIterator;
Begin 
  Result := TFslIntegerListIterator.Create;
  TFslIntegerListIterator(Result).IntegerList := TFslIntegerList(Self.Link);
End;


function TFslIntegerList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Capacity * sizeof(Integer));
end;

Constructor TFslIntegerListIterator.Create;
Begin 
  Inherited;

  FIntegerList := Nil;
End;


Destructor TFslIntegerListIterator.Destroy;
Begin
  FIntegerList.Free;

  Inherited;
End;


Procedure TFslIntegerListIterator.First;
Begin
  Inherited;

  FIndex := 0;
End;


Procedure TFslIntegerListIterator.Last;
Begin
  Inherited;

  FIndex := FIntegerList.Count - 1;
End;


Procedure TFslIntegerListIterator.Next;
Begin
  Inherited;

  Inc(FIndex);
End;  


Procedure TFslIntegerListIterator.Back;
Begin
  Inherited;

  Dec(FIndex);
End;  


Function TFslIntegerListIterator.Current : Integer;
Begin
  Result := FIntegerList[FIndex];
End;  


Function TFslIntegerListIterator.More : Boolean;
Begin 
  Result := FIntegerList.ExistsByIndex(FIndex);
End;  


Procedure TFslIntegerListIterator.SetIntegerList(Const Value : TFslIntegerList);
Begin
  FIntegerList.Free;
  FIntegerList := Value;
End;


function TFslIntegerListIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FIntegerList.sizeInBytes);
end;

Function TFslIntegerList.GetIntegers(iIndex : Integer) : TFslIntegerListItem;
Begin
  Result := IntegerByIndex[iIndex];
End;


Procedure TFslIntegerList.SetIntegers(iIndex : Integer; Const Value : TFslIntegerListItem);
Begin
  IntegerByIndex[iIndex] := Value;
End;


Function TFslIntegerList.ExistsAll(oIntegerList : TFslIntegerList) : Boolean;
Var
  iIndex : Integer;
Begin
  Assert(Invariants('ExistsAll', oIntegerList, TFslIntegerList, 'oIntegerList'));

  Result := True;

  iIndex := 0;

  While Result And oIntegerList.ExistsByIndex(iIndex) Do
  Begin
    Result := ExistsByValue(oIntegerList[iIndex]);

    Inc(iIndex);
  End;
End;


Procedure TFslClassList.AddArray(Const aClasses: Array Of TClass);
Var
  iLoop : Integer;
Begin 
  For iLoop := Low(aClasses) To High(aClasses) Do
    Inherited Add(aClasses[iLoop]);
End;


Function TFslClassList.IndexByClassType(aClass: TClass): Integer;
Begin
  If Not Find(aClass, Result) Then
    Result := -1;
End;


Function TFslClassList.ExistsByClassType(aClass : TClass) : Boolean;
Begin
  Result := ExistsByIndex(IndexByClassType(aClass));
End;


Function TFslClassList.Iterator : TFslIterator;
Begin 
  Result := TFslClassListIterator.Create;

  TFslClassListIterator(Result).ClassList := TFslClassList(Self.Link);
End;  


Function TFslClassList.Add(Const aClass: TClass): Integer;
Begin
  Result := Inherited Add(Pointer(aClass));
End;


Procedure TFslClassList.AddAll(Const oClassList : TFslClassList);
Var
  iClassIndex : Integer;
Begin
  For iClassIndex := 0 To oClassList.Count - 1 Do
    Add(oClassList[iClassIndex]);
End;


Function TFslClassList.ItemClass : TFslObjectClass;
Begin
  // TODO: have to constrain this class to lists of TFslObjectClass's only to enforce this

  Result := TFslObject;
End;


Function TFslClassList.GetClassByIndex(Const iIndex : Integer) : TClass;
Begin
  Result := TClass(PointerByIndex[iIndex]);
End;


Procedure TFslClassList.SetClassByIndex(Const iIndex : Integer; Const Value : TClass);
Begin
  PointerByIndex[iIndex] := Value;
End;


Function TFslClassList.Find(Const aClass: TClass; Out iIndex: Integer): Boolean;
Begin
  Result := Inherited Find(aClass, iIndex)
End;


Constructor TFslClassListIterator.Create;
Begin
  Inherited;

  FClassList := Nil;
End;


Destructor TFslClassListIterator.Destroy;
Begin
  FClassList.Free;

  Inherited;
End;


Procedure TFslClassListIterator.First;
Begin
  Inherited;

  FIndex := 0;
End;  


Procedure TFslClassListIterator.Last;
Begin 
  Inherited;

  FIndex := FClassList.Count - 1;
End;


Procedure TFslClassListIterator.Next;
Begin
  Inherited;

  Inc(FIndex);
End;


Procedure TFslClassListIterator.Back;
Begin
  Inherited;

  Dec(FIndex);
End;


Function TFslClassListIterator.Current : TClass;
Begin
  Result := FClassList[FIndex];
End;


Function TFslClassListIterator.More : Boolean;
Begin
  Result := FClassList.ExistsByIndex(FIndex);
End;


Procedure TFslClassListIterator.SetClassList(Const Value : TFslClassList);
Begin
  FClassList.Free;
  FClassList := Value;
End;

function TFslClassListIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FClassList.sizeInBytes);
end;

Procedure TFslObjectClassHashEntry.Assign(oSource: TFslObject);
Begin
  Inherited;

  FData := TFslObjectClassHashEntry(oSource).Data;
End;  


Constructor TFslObjectClassHashTableIterator.Create;
Begin 
  Inherited;

  FInternal := TFslStringHashTableIterator.Create;
End;  


Destructor TFslObjectClassHashTableIterator.Destroy;
Begin 
  FInternal.Free;

  Inherited;
End;  


function TFslObjectClassHashTableIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FInternal.sizeInBytes);
end;

Function TFslObjectClassHashTable.ItemClass : TFslHashEntryClass;
Begin 
  Result := TFslObjectClassHashEntry;
End;  


Function TFslObjectClassHashTable.Iterator : TFslIterator;
Begin
  Result := TFslObjectClassHashTableIterator.Create;
  TFslObjectClassHashTableIterator(Result).HashTable := TFslObjectClassHashTable(Self.Link);
End;  


Function TFslObjectClassHashTableIterator.Current : TClass;
Begin 
  Result := TFslObjectClassHashEntry(FInternal.Current).Data;
End;


Procedure TFslObjectClassHashTableIterator.First;
Begin 
  Inherited;

  FInternal.First;
End;  


Procedure TFslObjectClassHashTableIterator.Last;
Begin
  Inherited;

  FInternal.Last;
End;  


Procedure TFslObjectClassHashTableIterator.Next;
Begin
  Inherited;

  FInternal.Next;
End;  


Procedure TFslObjectClassHashTableIterator.Back;
Begin
  Inherited;

  FInternal.Back;
End;  


Function TFslObjectClassHashTableIterator.More : Boolean;
Begin
  Result := FInternal.More;
End;  


Function TFslObjectClassHashTableIterator.GetHashTable : TFslObjectClassHashTable;
Begin 
  Result := TFslObjectClassHashTable(FInternal.HashTable);
End;


Procedure TFslObjectClassHashTableIterator.SetHashTable(Const Value: TFslObjectClassHashTable);
Begin 
  FInternal.HashTable := Value;
End;


Constructor TFslStringObjectMatch.Create;
Begin
  Inherited;

  FNominatedValueClass := ItemClass;
End;


Destructor TFslStringObjectMatch.Destroy;
Begin
  Inherited;
End;


Procedure TFslStringObjectMatch.AssignItem(oItems : TFslItemList; iIndex: Integer);
Begin
  FMatchArray^[iIndex].Key := TFslStringObjectMatch(oItems).FMatchArray^[iIndex].Key;
  FMatchArray^[iIndex].Value := TFslStringObjectMatch(oItems).FMatchArray^[iIndex].Value.Clone;
End;



Procedure TFslStringObjectMatch.InternalEmpty(iIndex, iLength : Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatchArray) + NativeUInt(iIndex * SizeOf(TFslStringObjectMatchItem))), (iLength * SizeOf(TFslStringObjectMatchItem)));
End;  


Procedure TFslStringObjectMatch.InternalTruncate(iValue : Integer);
Var
  iLoop : Integer;
  oValue : TFslObject;
Begin
  Inherited;

  // finalize the strings that will be truncated.
  If iValue < Count Then
    Finalize(FMatchArray^[iValue], Count - iValue);

  For iLoop := iValue To Count - 1 Do
  Begin
    oValue := FMatchArray^[iLoop].Value;
    FMatchArray^[iLoop].Value := Nil;
    oValue.Free;
  End;
End;


Procedure TFslStringObjectMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMatchArray, Capacity * SizeOf(TFslStringObjectMatchItem), iValue * SizeOf(TFslStringObjectMatchItem));
End;


Procedure TFslStringObjectMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatchArray^[iSource], @FMatchArray^[iTarget], iCount * SizeOf(TFslStringObjectMatchItem));
End;  


Function TFslStringObjectMatch.CompareByKey(pA, pB: Pointer): Integer;
Begin
  Result := FCompareKey(PFslStringObjectMatchItem(pA)^.Key, PFslStringObjectMatchItem(pB)^.Key);
End;


Function TFslStringObjectMatch.CompareByValue(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(Integer(PFslStringObjectMatchItem(pA)^.Value), Integer(PFslStringObjectMatchItem(pB)^.Value));
End;


Function TFslStringObjectMatch.Find(Const aKey: TFslStringObjectMatchKey; Const aValue: TFslStringObjectMatchValue; Out iIndex: Integer; aCompare: TFslItemListCompare): Boolean;
Var
  aItem : TFslStringObjectMatchItem;
Begin
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;


Function TFslStringObjectMatch.FindByKey(Const aKey: TFslStringObjectMatchKey; Out iIndex: Integer): Boolean;
Begin
  Result := Find(aKey, Nil, iIndex, CompareByKey);
End;


Function TFslStringObjectMatch.FindByValue(Const aValue: TFslStringObjectMatchValue; Out iIndex: Integer) : Boolean;
Begin
  Result := Find('', aValue, iIndex, CompareByValue);
End;


Function TFslStringObjectMatch.IndexByKey(Const aKey : TFslStringObjectMatchKey) : Integer;
Begin
  If Not Find(aKey, Nil, Result, CompareByKey) Then
    Result := -1;
End;


Function TFslStringObjectMatch.IndexByValue(Const aValue : TFslStringObjectMatchValue) : Integer;
Begin
  If Not Find('', aValue, Result, CompareByValue) Then
    Result := -1;
End;


Function TFslStringObjectMatch.Add(Const aKey : TFslStringObjectMatchKey; Const aValue : TFslStringObjectMatchValue) : Integer;
Begin 
  Assert(ValidateItem('Add', aValue, 'aValue'));

  Result := -1;

  If Not IsAllowDuplicates And Find(aKey, aValue, Result) Then
  Begin
    aValue.Free; // free ignored object

    If IsPreventDuplicates Then
      RaiseError('Add', StringFormat('Item already exists in list (%s=%x)', [aKey, Integer(aValue)]));
  End
  Else
  Begin
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(aKey, aValue, Result);

    Insert(Result, aKey, aValue);
  End;  
End;  


Procedure TFslStringObjectMatch.Insert(iIndex : Integer; Const aKey : TFslStringObjectMatchKey; Const aValue : TFslStringObjectMatchValue);
Begin 
  Assert(ValidateItem('Insert', aValue, 'aValue'));

  InternalInsert(iIndex);

  FMatchArray^[iIndex].Key := aKey;
  FMatchArray^[iIndex].Value := aValue;
End;  


Procedure TFslStringObjectMatch.InternalInsert(iIndex : Integer);
Begin 
  Inherited;

  Pointer(FMatchArray^[iIndex].Key) := Nil;
  Pointer(FMatchArray^[iIndex].Value) := Nil;
End;  


Procedure TFslStringObjectMatch.InternalDelete(iIndex : Integer);
Begin
  Inherited;

  Finalize(FMatchArray^[iIndex].Key);
  FMatchArray^[iIndex].Value.Free;
  FMatchArray^[iIndex].Value := Nil;
End;  


Procedure TFslStringObjectMatch.InternalExchange(iA, iB: Integer);
Var
  aTemp : TFslStringObjectMatchItem;
  pA    : PFslStringObjectMatchItem;
  pB    : PFslStringObjectMatchItem;
Begin 
  pA := @FMatchArray^[iA];
  pB := @FMatchArray^[iB];

  aTemp := pA^;
  pA^ := pB^;
  pB^ := aTemp;
End;


Function TFslStringObjectMatch.GetItem(iIndex : Integer) : Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatchArray^[iIndex];
End;  


Procedure TFslStringObjectMatch.SetItem(iIndex: Integer; pValue: Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FMatchArray^[iIndex] := PFslStringObjectMatchItem(pValue)^;
End;  


Function TFslStringObjectMatch.GetKeyByIndex(iIndex : Integer): TFslStringObjectMatchKey;
Begin
  Assert(ValidateIndex('GetKeyByIndex', iIndex));

  Result := FMatchArray^[iIndex].Key;
End;  


Procedure TFslStringObjectMatch.SetKeyByIndex(iIndex : Integer; Const aKey : TFslStringObjectMatchKey);
Begin
  Assert(ValidateIndex('SetKeyByIndex', iIndex));

  FMatchArray^[iIndex].Key := aKey;
End;  


Function TFslStringObjectMatch.GetValueByIndex(iIndex : Integer): TFslStringObjectMatchValue;
Begin
  Assert(ValidateIndex('GetValueByIndex', iIndex));

  Result := FMatchArray^[iIndex].Value;
End;


Procedure TFslStringObjectMatch.SetValueByIndex(iIndex : Integer; Const aValue: TFslStringObjectMatchValue);
Begin
  Assert(ValidateIndex('SetValueByIndex', iIndex));
  Assert(ValidateItem('SetValueByIndex', aValue, 'aValue'));

  FMatchArray^[iIndex].Value.Free;
  FMatchArray^[iIndex].Value := aValue;
End;


Procedure TFslStringObjectMatch.SetValueByKey(Const aKey : TFslStringObjectMatchKey; Const aValue : TFslStringObjectMatchValue);
Var
  iIndex : Integer;
Begin
  Assert(ValidateItem('SetValueByKey', aValue, 'aValue'));

  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    ValueByIndex[iIndex] := aValue
  Else If FForced Then
    Add(aKey, aValue)
  Else
    RaiseError('SetValueByKey', StringFormat('Unable to set the value for the specified key ''%s''.', [aKey]));
End;


Function TFslStringObjectMatch.GetSensitive : Boolean;
Var
  aCompare : TFslStringCompareCallback;
Begin
  aCompare := StringCompareSensitive;

  Result := @FCompareKey = @aCompare;
End;


Procedure TFslStringObjectMatch.SetSensitive(Const Value: Boolean);
Begin
  If Value Then
    FCompareKey := StringCompareSensitive
  Else
    FCompareKey := StringCompareInsensitive;
End;


Function TFslStringObjectMatch.GetAsText: String;
Var
  iItem, iSymbol : Integer;
  iLoop, iSize : Integer;
  Ptr : PChar;
  sItem : String;
Begin 
  iSymbol := Length(FSymbol);

  iSize := 0;
  For iLoop := 0 To Count - 1 Do
    Inc(iSize, Length(FMatchArray^[iLoop].Key) + iSymbol);

  SetLength(Result, iSize);

  Ptr := PChar(Result);
  For iLoop := 0 To Count - 1 Do
  Begin 
    sItem := FMatchArray^[iLoop].Key;
    iItem := Length(sItem);

    If (iItem > 0) Then
    Begin 
      MemoryMove(Pointer(sItem), Ptr, iItem);
      Inc(Ptr, iItem);

      MemoryMove(Pointer(FSymbol), Ptr, iSymbol);
      Inc(Ptr, iSymbol);
    End;  
  End;  
End;


Procedure TFslStringObjectMatch.SetAsText(Const Value: String);
Var
  sTemp : String;
  iPos : Integer;
  SymbolLength : Integer;
Begin 
  Clear;

  sTemp := Value;
  iPos := Pos(FSymbol, sTemp);
  SymbolLength := Length(FSymbol);

  While (iPos > 0) Do
  Begin 
    Add(System.Copy(sTemp, 1, iPos - 1), Nil);
    System.Delete(sTemp, 1, iPos + SymbolLength - 1);

    iPos := Pos(FSymbol, sTemp);
  End;  

  If (sTemp <> '') Then
    Add(sTemp, Nil);
End;


Function TFslStringObjectMatch.CapacityLimit : Integer;
Begin 
  Result := High(TFslStringObjectMatchItemArray);
End;  


Procedure TFslStringObjectMatch.DefaultCompare(Out aCompare: TFslItemListCompare);
Begin 
  aCompare := CompareByKey;
  FCompareKey := StringCompareInsensitive;
End;


Function TFslStringObjectMatch.ItemClass : TFslObjectClass;
Begin 
  Result := TFslObject;
End;  


Function TFslStringObjectMatch.ValidateIndex(Const sMethod : String; iIndex: Integer): Boolean;
Begin 
  Inherited ValidateIndex(sMethod, iIndex);

  ValidateItem(sMethod, FMatchArray^[iIndex].Value, 'FMatchArray^[' + IntegerToString(iIndex) + '].Value');

  Result := True;
End;  


Function TFslStringObjectMatch.ValidateItem(Const sMethod : String; oObject : TFslObject; Const sObject : String) : Boolean;
Begin 
  If Assigned(oObject) Then
    Invariants(sMethod, oObject, NominatedValueClass, sObject);

  Result := True;
End;  


Function TFslStringObjectMatch.ExistsByKey(Const aKey : TFslStringObjectMatchKey) : Boolean;
Begin
  Result := IndexByKey(aKey) >= 0;
End;


Function TFslStringObjectMatch.ExistsByValue(Const aValue : TFslStringObjectMatchValue) : Boolean;
Begin
  Result := IndexByValue(aValue) >= 0;
End;


Procedure TFslStringObjectMatch.SortedByKey;
Begin 
  SortedBy(CompareByKey);
End;


Procedure TFslStringObjectMatch.SortedByValue;
Begin
  SortedBy(CompareByValue);
End;  


Function TFslStringObjectMatch.IsSortedByKey : Boolean;
Begin 
  Result := IsSortedBy(CompareByKey);
End;


Function TFslStringObjectMatch.IsSortedByValue : Boolean;
Begin 
  Result := IsSortedBy(CompareByValue);
End;  


Function TFslStringObjectMatch.GetKeyByValue(Const aValue: TFslStringObjectMatchValue): TFslStringObjectMatchKey;
Var
  iIndex : Integer;
Begin
  If FindByValue(aValue, iIndex) Then
  Begin
    Result := KeyByIndex[iIndex]
  End
  Else If FForced Then
  Begin
    Result := DefaultKey;
  End
  Else
  Begin
    RaiseError('GetKeyByValue', StringFormat('Unable to get the key for the specified value ''%d''.', [Integer(aValue)]));

    Result := '';
  End;
End;


Function TFslStringObjectMatch.GetValueByKey(Const aKey: TFslStringObjectMatchKey): TFslStringObjectMatchValue;
Var
  iIndex : Integer;
Begin
  If FindByKey(aKey, iIndex) Then
  Begin
    Result := ValueByIndex[iIndex]
  End
  Else If FForced Then
  Begin
    Result := DefaultValue;
  End
  Else
  Begin
    RaiseError('GetValueByKey', StringFormat('Unable to get the value for the specified key ''%s''.', [aKey]));

    Result := Nil;
  End;
End;


Procedure TFslStringObjectMatch.AddAll(oSourceStringObjectMatch: TFslStringObjectMatch);
Var
  iIndex : Integer;
Begin
  Assert(CheckCondition(oSourceStringObjectMatch <> Self, 'AddAll', 'Cannot addall items from a list to itself.'));

  Capacity := IntegerMax(Count + oSourceStringObjectMatch.Count, Capacity);
  For iIndex := 0 To oSourceStringObjectMatch.Count - 1 Do
    Add(oSourceStringObjectMatch.KeyByIndex[iIndex], oSourceStringObjectMatch.ValueByIndex[iIndex].Link);
End;


Function TFslStringObjectMatch.Link: TFslStringObjectMatch;
Begin
  Result := TFslStringObjectMatch(Inherited Link);
End;


Function TFslStringObjectMatch.GetMatchByIndex(iIndex: Integer): TFslStringObjectMatchItem;
Begin
  Assert(ValidateIndex('GetMatchByIndex', iIndex));

  Result := FMatchArray^[iIndex];
End;


Function TFslStringObjectMatch.GetMatch(Const aKey : TFslStringObjectMatchKey): TFslStringObjectMatchValue;
Var
  iIndex : Integer;
Begin
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else If FForced Then
    Result := DefaultValue
  Else
  Begin
    RaiseError('GetMatch', StringFormat('Unable to get the value for the specified key ''%s''.', [aKey]));
    Result := Nil;
  End;
End;


Procedure TFslStringObjectMatch.SetMatch(Const aKey : TFslStringObjectMatchKey; Const aValue : TFslStringObjectMatchValue);
Var
  iIndex : Integer;
Begin
  Assert(ValidateItem('SetMatch', aValue, 'aValue'));

  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    ValueByIndex[iIndex] := aValue
  Else If FForced Then
    Add(aKey, aValue)
  Else
    RaiseError('SetMatch', StringFormat('Unable to set the value for the specified key ''%s''.', [aKey]));
End;

Procedure TFslStringObjectMatch.DeleteByKey(Const aKey: TFslStringObjectMatchKey);
Begin
  DeleteByIndex(IndexByKey(aKey));
End;


Procedure TFslStringObjectMatch.DeleteByValue(Const aValue: TFslStringObjectMatchValue);
Begin
  DeleteByIndex(IndexByValue(aValue));
End;


function TFslStringObjectMatch.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Capacity * sizeof(TFslStringObjectMatchItem));
  inc(result, FDefaultValue.sizeInBytes);
  inc(result, (FSymbol.length * sizeof(char)) + 12);
end;

Constructor TFslStringMatch.Create;
Begin
  Inherited;

  FSymbol := cReturn;
  FSeparator := '=';
End;  


Procedure TFslStringMatch.Assign(oObject: TFslObject);
Begin 
  Inherited;

  FDefaultValue := TFslStringMatch(oObject).FDefaultValue;
  FForced := TFslStringMatch(oObject).FForced;
  FSymbol := TFslStringMatch(oObject).FSymbol;
End;  


Function TFslStringMatch.Link : TFslStringMatch;
Begin 
  Result := TFslStringMatch(Inherited Link);
End;


Function TFslStringMatch.Clone : TFslStringMatch;
Begin 
  Result := TFslStringMatch(Inherited Clone);
End;  


Procedure TFslStringMatch.DefaultCompare(Out aCompare : TFslItemListCompare);
Begin 
  aCompare := CompareByKey;
End;  


Function TFslStringMatch.CompareMatch(pA, pB: Pointer): Integer;
Begin 
  Result := StringCompare(PFslStringMatchItem(pA)^.Key, PFslStringMatchItem(pB)^.Key);

  If Result = 0 Then
    Result := StringCompare(PFslStringMatchItem(pA)^.Value, PFslStringMatchItem(pB)^.Value);
End;  


Function TFslStringMatch.CompareByKey(pA, pB: Pointer): Integer;
Begin 
  Result := StringCompareInsensitive(PFslStringMatchItem(pA)^.Key, PFslStringMatchItem(pB)^.Key);
End;


Function TFslStringMatch.CompareByValue(pA, pB: Pointer): Integer;
Begin
  Result := StringCompareInsensitive(PFslStringMatchItem(pA)^.Value, PFslStringMatchItem(pB)^.Value);
End;


Function TFslStringMatch.CompareByCaseSensitiveKey(pA, pB: Pointer): Integer;
Begin
  Result := StringCompareSensitive(PFslStringMatchItem(pA)^.Key, PFslStringMatchItem(pB)^.Key);
End;


Procedure TFslStringMatch.AssignItem(oItems : TFslItemList; iIndex: Integer);
Begin 
  Inherited;

  FMatchArray^[iIndex] := TFslStringMatch(oItems).FMatchArray^[iIndex];
End;  


Procedure TFslStringMatch.InternalEmpty(iIndex, iLength : Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatchArray) + NativeUInt(iIndex * SizeOf(TFslStringMatchItem))), (iLength * SizeOf(TFslStringMatchItem)));
End;  


Procedure TFslStringMatch.InternalTruncate(iValue : Integer);
Begin
  Inherited;

  // finalize the strings that will be truncated.
  If iValue < Count Then
    Finalize(FMatchArray^[iValue], Count - iValue);
End;


Procedure TFslStringMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMatchArray, Capacity * SizeOf(TFslStringMatchItem), iValue * SizeOf(TFslStringMatchItem));
End;


Procedure TFslStringMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatchArray^[iSource], @FMatchArray^[iTarget], iCount * SizeOf(TFslStringMatchItem));
End;


Function TFslStringMatch.IndexByKey(Const aKey: TFslStringMatchKey): Integer;
Begin
  If Not Find(aKey, '', Result, CompareByKey) Then
    Result := -1;
End;


Function TFslStringMatch.IndexByCaseSensitiveKey(Const aKey: TFslStringMatchKey): Integer;
Begin
  If Not Find(aKey, '', Result, CompareByCaseSensitiveKey) Then
    Result := -1;
End;  


Function TFslStringMatch.IndexByValue(Const aValue: TFslStringMatchValue): Integer;
Begin 
  If Not Find('', aValue, Result, CompareByValue) Then
    Result := -1;
End;


Function TFslStringMatch.IndexOf(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue): Integer;
Begin
  If Not Find(aKey, aValue, Result, CompareMatch) Then
    Result := -1;
End;


Function TFslStringMatch.ExistsByKeyAndValue(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue) : Boolean;
Begin
  Result := IndexOf(aKey, aValue) >= 0;
End;


Function TFslStringMatch.ExistsByKey(Const aKey: TFslStringMatchKey): Boolean;
Begin
  Result := IndexByKey(aKey) >= 0;
End;


Procedure TFslStringMatch.DeleteByKey(Const aKey : TFslStringMatchKey);
Var
  iIndex : Integer;
Begin
  iIndex := IndexByKey(aKey);
  If ExistsByIndex(iIndex) Then
    DeleteByIndex(iIndex)
  Else If Not Forced Then
    RaiseError('DeleteByKey', StringFormat('Unable to find a value for the specified key ''%s''.', [aKey]));
End;



Function TFslStringMatch.ExistsByValue(Const aValue: TFslStringMatchValue): Boolean;
Begin
  Result := IndexByValue(aValue) >= 0;
End;


Function TFslStringMatch.Add(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue) : Integer;
Begin
  Result := -1;

  If Not IsAllowDuplicates And Find(aKey, aValue, Result) Then
  Begin
    If IsPreventDuplicates Then
      RaiseError('Add', StringFormat('Key already exists in list (%s=%s)', [aKey, aValue]));

    // Result is the index of the existing key
  End   
  Else
  Begin 
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(aKey, aValue, Result);

    Insert(Result, aKey, aValue);
  End;  
End;  


Procedure TFslStringMatch.Insert(iIndex : Integer; Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue);
Begin 
  InternalInsert(iIndex);

  FMatchArray^[iIndex].Key := aKey;
  FMatchArray^[iIndex].Value := aValue;
End;  


Procedure TFslStringMatch.InternalInsert(iIndex : Integer);
Begin 
  Inherited;

  Pointer(FMatchArray^[iIndex].Key) := Nil;
  Pointer(FMatchArray^[iIndex].Value) := Nil;
End;  


Procedure TFslStringMatch.InternalDelete(iIndex : Integer);
Begin
  Inherited;

  Finalize(FMatchArray^[iIndex]);  
End;  


Procedure TFslStringMatch.InternalExchange(iA, iB: Integer);
Var
  aTemp : TFslStringMatchItem;
  pA    : PFslStringMatchItem;
  pB    : PFslStringMatchItem;
Begin 
  pA := @FMatchArray^[iA];
  pB := @FMatchArray^[iB];

  aTemp := pA^;
  pA^ := pB^;
  pB^ := aTemp;
End;  


Function TFslStringMatch.GetItem(iIndex : Integer) : Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatchArray^[iIndex];
End;  


Procedure TFslStringMatch.SetItem(iIndex: Integer; pValue: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatchArray^[iIndex] := PFslStringMatchItem(pValue)^;
End;  


Function TFslStringMatch.GetKeyByIndex(iIndex : Integer): TFslStringMatchKey;
Begin 
  Assert(ValidateIndex('GetKeyByIndex', iIndex));

  Result := FMatchArray^[iIndex].Key;
End;  


Procedure TFslStringMatch.SetKeyByIndex(iIndex : Integer; Const aKey : TFslStringMatchKey);
Begin 
  Assert(ValidateIndex('SetKeyByIndex', iIndex));

  FMatchArray^[iIndex].Key := aKey;
End;  


Function TFslStringMatch.GetValueByIndex(iIndex : Integer): TFslStringMatchValue;
Begin 
  Assert(ValidateIndex('GetValueByIndex', iIndex));

  Result := FMatchArray^[iIndex].Value;
End;  


Procedure TFslStringMatch.SetValueByIndex(iIndex : Integer; Const aValue: TFslStringMatchValue);
Begin 
  Assert(ValidateIndex('SetValueByIndex', iIndex));

  FMatchArray^[iIndex].Value := aValue;
End;  


Function TFslStringMatch.GetValueByKey(Const aKey : TFslStringMatchKey): TFslStringMatchValue;
Var
  iIndex : Integer;
Begin
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else If FForced Then
    Result := DefaultValue(aKey)
  Else
  Begin
    RaiseError('GetMatch', StringFormat('Unable to get the value for the specified key ''%s''.', [aKey]));
    Result := '';
  End;
End;


Procedure TFslStringMatch.SetValueByKey(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue);
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    ValueByIndex[iIndex] := aValue
  Else If FForced Then
    Add(aKey, aValue)
  Else
    RaiseError('SetMatch', 'Unable to set the value for the specified key.');
End;  


Function TFslStringMatch.GetAsText : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := 0 To Count - 1 Do
    Result := Result + KeyByIndex[iLoop] + FSeparator + ValueByIndex[iLoop] + FSymbol;
End;  


Procedure TFslStringMatch.SetAsText(Const sValue: String);
Var
  sTemp   : String;
  sItem   : String;
  iPos    : Integer;
  iSymbol : Integer;
  sLeft   : String;
  sRight  : String;
Begin
  Clear;

  sTemp := sValue;
  iPos := Pos(FSymbol, sTemp);
  iSymbol := Length(FSymbol);

  While (iPos > 0) Do
  Begin
    sItem := System.Copy(sTemp, 1, iPos - 1);

    If StringSplit(sItem, FSeparator, sLeft, sRight) Then
      Add(sLeft, sRight)
    Else
      Add(sItem, '');

    System.Delete(sTemp, 1, iPos + iSymbol - 1);

    iPos := Pos(FSymbol, sTemp);
  End;

  If sTemp <> '' Then
  Begin
    If StringSplit(sTemp, FSeparator, sLeft, sRight) Then
      Add(sLeft, sRight)
    Else
      Add(sTemp, '');
  End;  
End;


Function TFslStringMatch.CapacityLimit : Integer;
Begin 
  Result := High(TFslStringMatchItems);
End;  


Function TFslStringMatch.Find(Const aKey : TFslStringMatchKey; Const aValue: TFslStringMatchValue; Out iIndex: Integer; aCompare: TFslItemListCompare): Boolean;
Var
  aItem : TFslStringMatchItem;
Begin 
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;  


Function TFslStringMatch.DefaultValue(Const aKey: TFslStringMatchKey): TFslStringMatchValue;
Begin 
  Result := FDefaultValue;
End;  


Function TFslStringMatch.ForceByKey(Const aKey: TFslStringMatchKey): TFslStringMatchValue;
Var
  iIndex : Integer;
Begin 
  If Not Find(aKey, '', iIndex, CompareByKey) Then
    Insert(iIndex, aKey, DefaultValue(aKey));

  Result := ValueByIndex[iIndex];
End;  


Function TFslStringMatch.EqualTo(oMatch: TFslStringMatch): Boolean;
Var
  iLoop : Integer;
  iIndex : Integer;
Begin 
  Result := Count = oMatch.Count;
  iLoop := 0;

  If Not IsAllowDuplicates Then
  Begin 
    While Result And (iLoop < oMatch.Count) Do
    Begin 
      iIndex := IndexByKey(oMatch.KeyByIndex[iLoop]);

      Result := ExistsByIndex(iIndex) And StringEquals(oMatch.ValueByIndex[iLoop], ValueByIndex[iIndex]);

      Inc(iLoop);
    End;  
  End   
  Else
  Begin 
    RaiseError('EqualTo', 'Equality checking not available to duplicate allowing string matches.');
  End;
End;


Procedure TFslStringMatch.SortedByKey;
Begin
  SortedBy(CompareByKey);
End;


Procedure TFslStringMatch.SortedByCaseSensitiveKey;
Begin
  SortedBy(CompareByCaseSensitiveKey);
End;


Procedure TFslStringMatch.SortedByValue;
Begin
  SortedBy(CompareByValue);
End;


Function TFslStringMatch.GetMatchByIndex(Const iIndex : Integer) : TFslStringMatchItem;
Begin
  Result := PFslStringMatchItem(Inherited ItemByIndex[iIndex])^;
End;


Procedure TFslStringMatch.SetMatchByIndex(Const iIndex : Integer; Const Value : TFslStringMatchItem);
Begin
  PFslStringMatchItem(Inherited ItemByIndex[iIndex])^ := Value;
End;


Function TFslStringMatch.ErrorClass: EFslExceptionClass;
Begin
  Result := EFslStringMatch;
End;


Procedure TFslStringMatch.AddAll(oStringMatch: TFslStringMatch);
Var
  iIndex : Integer;
Begin
  Assert(CheckCondition(oStringMatch <> Self, 'AddAll', 'Cannot add all items from a list to itself.'));

  Capacity := IntegerMax(Count + oStringMatch.Count, Capacity);

  For iIndex := 0 To oStringMatch.Count - 1 Do
    Add(oStringMatch.KeyByIndex[iIndex], oStringMatch.ValueByIndex[iIndex]);
End;


Function TFslStringMatch.GetMatch(Const aKey : TFslStringMatchKey): TFslStringMatchValue;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else If FForced Then
    Result := DefaultValue(aKey)
  Else
  Begin
    RaiseError('GetMatch', StringFormat('Unable to get the value for the specified key ''%s''.', [aKey]));
    Result := '';
  End;
End;


Procedure TFslStringMatch.SetMatch(Const aKey : TFslStringMatchKey; Const aValue : TFslStringMatchValue);
Var
  iIndex : Integer;
Begin
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    ValueByIndex[iIndex] := aValue
  Else If FForced Then
    Add(aKey, aValue)
  Else
    RaiseError('SetMatch', 'Unable to set the value for the specified key.');
End;  



function TFslStringMatch.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, capacity * sizeof(TFslStringMatchItem));
  inc(result, (FSymbol.length * sizeof(char)) + 12);
  inc(result, (FSeparator.length * sizeof(char)) + 12);
end;

Constructor TFslStringList.Create;
Begin
  Inherited;

  FSymbol := cReturn;
End;


Function TFslStringList.Clone: TFslStringList;
Begin
  Result := TFslStringList(Inherited Clone);
End;


Function TFslStringList.Link: TFslStringList;
Begin
  Result := TFslStringList(Inherited Link);
End;



Procedure TFslStringList.AssignItem(Items: TFslItemList; iIndex: Integer);
Begin
  FStringArray^[iIndex] := TFslStringList(Items).FStringArray^[iIndex];
End;


Function TFslStringList.Find(Const sValue : TFslStringListItem; Out iIndex : Integer; aCompare : TFslItemListCompare): Boolean;
Begin
  Result := Inherited Find(@sValue, iIndex, aCompare);
End;


Procedure TFslStringList.InternalEmpty(iIndex, iLength: Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FStringArray) + NativeUInt(iIndex * SizeOf(TFslStringListItem))), (iLength * SizeOf(TFslStringListItem)));
End;


Procedure TFslStringList.InternalTruncate(iValue : Integer);
Begin
  Inherited;

  // finalize the strings that will be truncated.
  If iValue < Count Then
    Finalize(FStringArray^[iValue], Count - iValue);
End;


Procedure TFslStringList.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FStringArray, Capacity * SizeOf(TFslStringListItem), iValue * SizeOf(TFslStringListItem));
End;


Procedure TFslStringList.InternalCopy(iSource, iTarget, iCount : Integer);
Begin
  Inherited;

  MemoryMove(@FStringArray^[iSource], @FStringArray^[iTarget], iCount * SizeOf(TFslStringListItem));
End;


Function TFslStringList.IndexByValue(Const sValue : TFslStringListItem): Integer;
Begin
  If Not Find(sValue, Result) Then
    Result := -1;
End;


Function TFslStringList.ExistsByValue(Const sValue : TFslStringListItem): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(sValue));
End;


Procedure TFslStringList.AddAll(oStrings: TFslStringList);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oStrings.Count - 1 Do
    Add(oStrings[iLoop]);
End;


Procedure TFslStringList.AddAllStringArray(Const aValues : Array Of String);
Var
  iLoop : Integer;
Begin
  For iLoop := Low(aValues) To High(aValues) Do
    Add(aValues[iLoop]);
End;


Function TFslStringList.Add(Const sValue : TFslStringListItem) : Integer;
Begin
  Result := -1;

  If Not IsAllowDuplicates And Find(sValue, Result) Then
  Begin { If }
    If IsPreventDuplicates Then
      RaiseError('Add', StringFormat('Item already exists in list (%s)', [sValue]));
  End   { If }
  Else
  Begin { Else }
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(sValue, Result);

    Insert(Result, sValue);
  End;  { Else }
End;


Procedure TFslStringList.InternalInsert(iIndex : Integer);
Begin
  Inherited;

  Pointer(FStringArray^[iIndex]) := Nil;
End;


Procedure TFslStringList.Insert(iIndex : Integer; Const sValue : TFslStringListItem);
Begin
  InternalInsert(iIndex);

  FStringArray^[iIndex] := sValue;
End;


Procedure TFslStringList.InternalDelete(iIndex: Integer);
Begin
  Inherited;

  Finalize(FStringArray^[iIndex]);
End;


Procedure TFslStringList.DeleteByValue(Const sValue : TFslStringListItem);
Var
  iIndex : Integer;
Begin
  If Not Find(sValue, iIndex) Then
    RaiseError('DeleteByValue', StringFormat('''%s'' not found in list', [sValue]));

  DeleteByIndex(iIndex);
End;


Procedure TFslStringList.InternalExchange(iA, iB : Integer);
Var
  iTemp : Integer;
  pA  : Pointer;
  pB  : Pointer;
Begin
  pA := @FStringArray^[iA];
  pB := @FStringArray^[iB];

  iTemp := Integer(pA^);
  Integer(pA^) := Integer(pB^);
  Integer(pB^) := iTemp;
End;


Function TFslStringList.GetItem(iIndex : Integer) : Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FStringArray^[iIndex];
End;


Procedure TFslStringList.SetItem(iIndex : Integer; pValue : Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FStringArray^[iIndex] := String(pValue^);
End;


Function TFslStringList.GetStringByIndex(iIndex : Integer) : String;
Begin
  Assert(ValidateIndex('GetStringByIndex', iIndex));

  Result := FStringArray^[iIndex];
End;


Procedure TFslStringList.SetStringByIndex(iIndex : Integer; Const sValue : TFslStringListItem);
Begin
  Assert(ValidateIndex('SetStringByIndex', iIndex));

  FStringArray^[iIndex] := sValue;
End;


Function TFslStringList.GetAsText : String;
Var
  iItem, iSymbol : Integer;
  iLoop, iSize : Integer;
  pCurrent : PChar;
  sItem : String;
Begin
  If Count <= 0 Then
    Result := ''
  Else
  Begin
    iSymbol := Length(FSymbol);

    iSize := iSymbol * (Count - 1);
    For iLoop := 0 To Count - 1 Do
      Inc(iSize, Length(FStringArray^[iLoop]));

    SetLength(Result, iSize);

    pCurrent := PChar(Result);

    For iLoop := 0 To Count - 1 Do
    Begin
      sItem := FStringArray^[iLoop];
      iItem := Length(sItem){$IFDEF UNICODE} * 2{$ENDIF};

      If (iItem > 0) Then
      Begin
        MemoryMove(Pointer(sItem), pCurrent, iItem);
        Inc(pCurrent, Length(sItem));
      End;

      If iLoop < Count - 1 Then
      Begin
        MemoryMove(Pointer(FSymbol), pCurrent, iSymbol{$IFDEF UNICODE} * 2{$ENDIF});
        Inc(pCurrent, iSymbol);
      End;
    End;
  End;
End;


Procedure TFslStringList.SetAsText(Const sValue: String);
Var
  sTemp : String;
  iPos : Integer;
  iSymbol : Integer;
Begin
  Clear;

  sTemp := sValue;
  iPos := Pos(FSymbol, sTemp);
  iSymbol := Length(FSymbol);

  While (iPos > 0) Do
  Begin
    Add(System.Copy(sTemp, 1, iPos - 1));
    System.Delete(sTemp, 1, iPos + iSymbol - 1);

    iPos := Pos(FSymbol, sTemp);
  End;

  If (sTemp <> '') Then
    Add(sTemp);
End;  


Procedure TFslStringList.LoadFromText(Const sFilename: String);
Var
  aFile : TextFile;
  sTemp : String;
Begin
  AssignFile(aFile, sFilename);
  Try
    Reset(aFile);

    While Not EOF(aFile) Do
    Begin
      ReadLn(aFile, sTemp);
      Add(sTemp);
    End;
  Finally
    CloseFile(aFile);
  End;
End;


Procedure TFslStringList.SaveToText(Const sFilename : String);
Var
  aFile : TextFile;
  iLoop : Integer;
Begin
  AssignFile(aFile, sFilename);
  Try
    ReWrite(aFile);

    For iLoop := 0 To Count - 1 Do
      WriteLn(aFile, StringByIndex[iLoop]);
  Finally
    CloseFile(aFile);
  End;
End;


Function TFslStringList.CapacityLimit : Integer;
Begin
  Result := High(TFslStringListItemArray);
End;


Function TFslStringList.CompareInsensitive(pA, pB: Pointer): Integer;
Begin
  Result := StringCompareInsensitive(String(pA^), String(pB^));
End;


Function TFslStringList.CompareSensitive(pA, pB: Pointer): Integer;
Begin
  Result := StringCompareSensitive(String(pA^), String(pB^));
End;


Procedure TFslStringList.DefaultCompare(Out aCompare: TFslItemListCompare);
Begin
  aCompare := CompareInsensitive;
End;


Function TFslStringList.GetSensitive : Boolean;
Begin
  Result := IsComparedBy(CompareSensitive);
End;


Procedure TFslStringList.SetSensitive(Const bValue: Boolean);
Begin
  If bValue Then
    ComparedBy(CompareSensitive)
  Else
    ComparedBy(CompareInsensitive);
End;


Function TFslStringList.Iterator : TFslIterator;
Begin
  Result := TFslStringListIterator.Create;
  TFslStringListIterator(Result).StringList := TFslStringList(Self.Link);
End;


function TFslStringList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSymbol.length * sizeof(char)) + 12);
end;

Constructor TFslStringListIterator.Create;
Begin
  Inherited;

  FStringList := Nil;
End;


Destructor TFslStringListIterator.Destroy;
Begin
  FStringList.Free;

  Inherited;
End;


Procedure TFslStringListIterator.First;
Begin
  Inherited;

  FIndex := 0;
End;


Procedure TFslStringListIterator.Last;
Begin
  Inherited;

  FIndex := FStringList.Count - 1;
End;


Procedure TFslStringListIterator.Next;
Begin
  Inherited;

  Inc(FIndex);
End;


Procedure TFslStringListIterator.Back;
Begin
  Inherited;

  Dec(FIndex);
End;


Function TFslStringListIterator.Current : String;
Begin
  Result := FStringList[FIndex];
End;


Function TFslStringListIterator.More : Boolean;
Begin
  Result := FStringList.ExistsByIndex(FIndex);
End;


Procedure TFslStringListIterator.SetStringList(Const Value : TFslStringList);
Begin
  FStringList.Free;
  FStringList := Value;
End;


function TFslStringListIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStringList.sizeInBytes);
end;

Function TFslStringList.GetAsCSV : String;
Var
  oStream : TFslStringStream;
  oFormatter : TFslCSVFormatter;
Begin
  oStream := TFslStringStream.Create;
  Try
    oFormatter := TFslCSVFormatter.Create;
    Try
      oFormatter.Stream := oStream.Link;

      oFormatter.ProduceEntryStringList(Self);
    Finally
      oFormatter.Free;
    End;

    result := TEncoding.UTF8.GetString(oStream.Bytes);
  Finally
    oStream.Free;
  End;
End;


Procedure TFslStringList.SetAsCSV(Const sValue: String);
Var
  oStream : TFslStringStream;
  oExtractor : TFslCSVExtractor;
Begin
  oStream := TFslStringStream.Create;
  Try
    oStream.Bytes := TEncoding.UTF8.GetBytes(sValue);
    oExtractor := TFslCSVExtractor.Create(oStream.Link, TEncoding.UTF8);
    Try
      oExtractor.ConsumeEntries(Self);
    Finally
      oExtractor.Free;
    End;
  Finally
    oStream.Free;
  End;
End;


Function TFslStringList.Compare(oStrings : TFslStringList) : Integer;
Var
  iLoop : Integer;
Begin
  Assert(Invariants('Compare', oStrings, TFslStringList, 'oStrings'));

  Result := IntegerCompare(Count, oStrings.Count);
  iLoop := 0;

  While (iLoop < oStrings.Count) And (Result = 0) Do
  Begin
    Result := StringCompare(StringByIndex[iLoop], oStrings[iLoop]);

    Inc(iLoop);
  End;
End; 


Function TFslStringList.Equals(oStrings : TFslStringList) : Boolean;
Begin
  Result := Compare(oStrings) = 0;
End;


Function TFslStringList.ExistsAny(oStrings : TFslStringList) : Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;

  iLoop := 0;

  While Not Result And (iLoop < oStrings.Count) Do
  Begin
    Result := ExistsByValue(oStrings[iLoop]);

    Inc(iLoop);
  End;
End;


procedure TFslStringList.SetItems(iIndex: integer; sValue: String);
begin
  StringByIndex[iIndex] := sValue;
end;

function TFslStringList.GetItems(iIndex: integer): String;
begin
  result := StringByIndex[iIndex];
end;

procedure TFslStringList.delete(iIndex: integer);
begin
  DeleteByIndex(iIndex);
end;

procedure TFslStringList.populate(iCount: integer);
begin
  while Count < iCount Do
    Add('');
end;

function TFslStringList.IndexOf(value: String): Integer;
begin
  result := IndexByValue(value);
end;


Constructor TFslStringIntegerMatch.Create;
Begin
  Inherited;

  FSymbol := cReturn;
End;


Destructor TFslStringIntegerMatch.Destroy;
Begin
  Inherited;
End;


Function TFslStringIntegerMatch.ErrorClass: EFslExceptionClass;
Begin
  Result := EFslStringIntegerMatch;
End;


Function TFslStringIntegerMatch.Clone: TFslStringIntegerMatch;
Begin
  Result := TFslStringIntegerMatch(Inherited Clone);
End;


Function TFslStringIntegerMatch.Link: TFslStringIntegerMatch;
Begin
  Result := TFslStringIntegerMatch(Inherited Link);
End;


Procedure TFslStringIntegerMatch.AssignItem(oItems: TFslItemList; iIndex: Integer);
Begin
  FMatchArray^[iIndex] := TFslStringIntegerMatch(oItems).FMatchArray^[iIndex];
End;



Procedure TFslStringIntegerMatch.InternalEmpty(iIndex, iLength: Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatchArray) + NativeUInt(iIndex * SizeOf(TFslStringIntegerMatchItem))), (iLength * SizeOf(TFslStringIntegerMatchItem)));
End;  


Procedure TFslStringIntegerMatch.InternalTruncate(iValue: Integer);
Begin
  Inherited;

  // finalize the strings that will be truncated
  If iValue < Count Then
    Finalize(FMatchArray^[iValue], Count - iValue);
End;


Procedure TFslStringIntegerMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMatchArray, Capacity * SizeOf(TFslStringIntegerMatchItem), iValue * SizeOf(TFslStringIntegerMatchItem));
End;  


Procedure TFslStringIntegerMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatchArray^[iSource], @FMatchArray^[iTarget], iCount * SizeOf(TFslStringIntegerMatchItem));
End;


Function TFslStringIntegerMatch.CompareKey(pA, pB: Pointer): Integer;
Begin 
  Result := FCompareKey(PFslStringIntegerMatchItem(pA)^.Key, PFslStringIntegerMatchItem(pB)^.Key);
End;  


Function TFslStringIntegerMatch.CompareValue(pA, pB: Pointer): Integer;
Begin 
  Result := IntegerCompare(PFslStringIntegerMatchItem(pA)^.Value, PFslStringIntegerMatchItem(pB)^.Value);
End;  


Procedure TFslStringIntegerMatch.DefaultCompare(Out aCompare: TFslItemListCompare);
Begin
  aCompare := CompareKey;
  FCompareKey := StringCompareInsensitive;
End;  


Function TFslStringIntegerMatch.Find(Const aKey: TFslStringIntegerMatchKey; Const aValue: TFslStringIntegerMatchValue; Out iIndex: Integer; aCompare: TFslItemListCompare): Boolean;
Var
  aItem : TFslStringIntegerMatchItem;
Begin 
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;  


Function TFslStringIntegerMatch.IndexByKey(Const aKey : TFslStringIntegerMatchKey) : Integer;
Begin 
  If Not Find(aKey, 0, Result, CompareKey) Then
    Result := -1;
End;  


Function TFslStringIntegerMatch.ExistsByKey(Const aKey : TFslStringIntegerMatchKey) : Boolean;
Begin 
  Result := ExistsByIndex(IndexByKey(aKey));
End;


Function TFslStringIntegerMatch.IndexByValue(Const aValue : TFslStringIntegerMatchValue) : Integer;
Begin 
  If Not Find('', aValue, Result, CompareValue) Then
    Result := -1;
End;  


Function TFslStringIntegerMatch.ExistsByValue(Const aValue : TFslStringIntegerMatchValue) : Boolean;
Begin 
  Result := ExistsByIndex(IndexByValue(aValue));
End;  


Function TFslStringIntegerMatch.Add(Const aKey : TFslStringIntegerMatchKey; Const aValue : TFslStringIntegerMatchValue) : Integer;
Begin
  Result := -1;

  If Not IsAllowDuplicates And Find(aKey, aValue, Result) Then
  Begin
    If IsPreventDuplicates Then
      RaiseError('Add', StringFormat('Key already exists in list (%s=%d)', [aKey, aValue]));
  End   
  Else
  Begin
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(aKey, aValue, Result);

    Insert(Result, aKey, aValue);
  End;
End;  


Procedure TFslStringIntegerMatch.Insert(iIndex : Integer; Const aKey : TFslStringIntegerMatchKey; Const aValue : TFslStringIntegerMatchValue);
Begin 
  InternalInsert(iIndex);

  FMatchArray^[iIndex].Key := aKey;
  FMatchArray^[iIndex].Value := aValue;
End;  


Procedure TFslStringIntegerMatch.InternalInsert(iIndex : Integer);
Begin 
  Inherited;

  Pointer(FMatchArray^[iIndex].Key) := Nil;
  FMatchArray^[iIndex].Value := 0;
End;  


Procedure TFslStringIntegerMatch.InternalDelete(iIndex : Integer);
Begin 
  Inherited;

  Finalize(FMatchArray^[iIndex]);  
End;


Procedure TFslStringIntegerMatch.InternalExchange(iA, iB : Integer);
Var
  aTemp : TFslStringIntegerMatchItem;
  pA : Pointer;
  pB : Pointer;
Begin
  pA := @FMatchArray^[iA];
  pB := @FMatchArray^[iB];

  aTemp := PFslStringIntegerMatchItem(pA)^;
  PFslStringIntegerMatchItem(pA)^ := PFslStringIntegerMatchItem(pB)^;
  PFslStringIntegerMatchItem(pB)^ := aTemp;
End;


Function TFslStringIntegerMatch.ForceByKey(Const aKey: TFslStringIntegerMatchKey): TFslStringIntegerMatchValue;
Var
  iIndex : Integer;
Begin
  If Not Find(aKey, 0, iIndex, CompareKey) Then
    Insert(iIndex, aKey, DefaultValue);

  Result := ValueByIndex[iIndex]
End;


Function TFslStringIntegerMatch.GetItem(iIndex : Integer) : Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatchArray^[iIndex];
End;  


Procedure TFslStringIntegerMatch.SetItem(iIndex : Integer; pValue : Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatchArray^[iIndex] := PFslStringIntegerMatchItem(pValue)^;
End;  


Function TFslStringIntegerMatch.GetKey(iIndex : Integer) : String;
Begin 
  Assert(ValidateIndex('GetKey', iIndex));

  Result := FMatchArray^[iIndex].Key;
End;  


Procedure TFslStringIntegerMatch.SetKey(iIndex : Integer; Const aKey : TFslStringIntegerMatchKey);
Begin 
  Assert(ValidateIndex('SetKey', iIndex));

  FMatchArray^[iIndex].Key := aKey;
End;  


Function TFslStringIntegerMatch.GetValue(iIndex : Integer) : TFslStringIntegerMatchValue;
Begin 
  Assert(ValidateIndex('GetValue', iIndex));

  Result := FMatchArray^[iIndex].Value;
End;  


Procedure TFslStringIntegerMatch.SetValue(iIndex : Integer; Const aValue : TFslStringIntegerMatchValue);
Begin 
  Assert(ValidateIndex('SetValue', iIndex));

  FMatchArray^[iIndex].Value := aValue;
End;  


Function TFslStringIntegerMatch.GetMatch(Const aKey : TFslStringIntegerMatchKey) : TFslStringIntegerMatchValue;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else
  Begin 
    Result := FDefaultValue;

    If Not FForced Then
      RaiseError('GetMatch', 'Unable to get the value for the specified key.');
  End;  
End;  


Procedure TFslStringIntegerMatch.SetMatch(Const aKey : TFslStringIntegerMatchKey; Const aValue : TFslStringIntegerMatchValue);
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    ValueByIndex[iIndex] := aValue
  Else If FForced Then
    Add(aKey, aValue)
  Else
    RaiseError('SetMatch', 'Unable to set the value for the specified key.');
End;  


Function TFslStringIntegerMatch.GetAsText : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, KeyByIndex[iLoop] + '=' + IntegerToString(ValueByIndex[iLoop]), FSymbol);
End;


Function TFslStringIntegerMatch.GetKeysAsText : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, KeyByIndex[iLoop], FSymbol);
End;  


Function TFslStringIntegerMatch.CapacityLimit : Integer;
Begin 
  Result := High(TFslStringIntegerMatchItemArray);
End;


Function TFslStringIntegerMatch.GetSensitive : Boolean;
Var
  aCompare : TFslStringCompareCallback;
Begin 
  aCompare := StringCompareSensitive;

  Result := @FCompareKey = @aCompare;
End;


Procedure TFslStringIntegerMatch.SetSensitive(Const Value: Boolean);
Begin
  If Value Then
    FCompareKey := StringCompareSensitive
  Else
    FCompareKey := StringCompareInsensitive;
End;  


Function TFslStringIntegerMatch.GetKeyByValue(Const aValue: TFslStringIntegerMatchValue): TFslStringIntegerMatchKey;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByValue(aValue);

  If ExistsByIndex(iIndex) Then
    Result := KeyByIndex[iIndex]
  Else
    Result := DefaultKey;
End;


Function TFslStringIntegerMatch.GetValueByKey(Const aKey : TFslStringIntegerMatchKey): TFslStringIntegerMatchValue;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else
    Result := DefaultValue;
End;  


Function TFslStringIntegerMatch.IsSortedByKey : Boolean;
Begin 
  Result := IsSortedBy(CompareKey);
End;  


Procedure TFslStringIntegerMatch.SortedByKey;
Begin 
  SortedBy(CompareKey);
End;  


Function TFslStringIntegerMatch.IsSortedByValue : Boolean;
Begin 
  Result := IsSortedBy(CompareValue);
End;  


Procedure TFslStringIntegerMatch.SortedByValue;
Begin 
  SortedBy(CompareValue);
End;  


Procedure TFslStringIntegerMatch.AddAll(oStringIntegerMatch: TFslStringIntegerMatch);
Var
  iIndex : Integer;
Begin
  Assert(CheckCondition(oStringIntegerMatch <> Self, 'AddAll', 'Cannot add all items from a list to itself.'));

  Capacity := IntegerMax(Count + oStringIntegerMatch.Count, Capacity);

  For iIndex := 0 To oStringIntegerMatch.Count - 1 Do
    Add(oStringIntegerMatch.KeyByIndex[iIndex], oStringIntegerMatch.ValueByIndex[iIndex]);
End;


Procedure TFslStringHashEntry.Generate;
Begin 
  Code := fsl_utilities.HashStringToCode32(FName);
End;


Procedure TFslStringHashEntry.SetName(Const Value: String);
Begin 
  FName := Value;

  Generate;
End;


Procedure TFslStringHashEntry.Assign(oSource: TFslObject);
Begin 
  Inherited;

  Name := TFslStringHashEntry(oSource).Name;
End;



function TFslStringHashEntry.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

Function TFslStringHashTable.Equal(oA, oB: TFslHashEntry): Integer;
Begin
  Result := Inherited Equal(oA, oB);

  If Result = 0 Then
    Result := StringCompare(TFslStringHashEntry(oA).Name, TFslStringHashEntry(oB).Name);
End;  


Function TFslStringHashTable.ItemClass : TFslHashEntryClass;
Begin 
  Result := TFslStringHashEntry;
End;  


Function TFslStringHashTable.Iterator : TFslIterator;
Begin
  Result := TFslStringHashTableIterator.Create;
  TFslStringHashTableIterator(Result).HashTable := Self.Link;
End;  


Function TFslStringHashTableIterator.Current : TFslStringHashEntry;
Begin 
  Result := TFslStringHashEntry(Inherited Current);
End;


Function TFslStringHashTable.Link: TFslStringHashTable;
Begin
  Result := TFslStringHashTable(Inherited Link);
End;


Function TFslObjectList.ItemClass : TFslObjectClass;
Begin
  Result := TFslObject;
End;

function TFslObjectList.sizeInBytesV : cardinal;
var
  i : integer;
begin
  result := inherited sizeInBytesV;
  inc(result, Capacity * Sizeof(pointer));
  for i := 0 to count - 1 do
    inc(result, Objects[i].sizeInBytes);
end;

Destructor TFslOrdinalSet.Destroy;
Begin
  PartsDispose;

  Inherited;
End;


Procedure TFslOrdinalSet.PartsDispose;
Begin
  If FOwns Then
    MemoryDestroy(FPartArray, FSize);

  FOwns := False;
  FPartArray := Nil;
  FSize := 0;
  FCount := 0;
End;


Procedure TFslOrdinalSet.PartsNew;
Begin
  If Not FOwns Then
  Begin
    FPartArray := Nil;
    MemoryCreate(FPartArray, FSize);
    MemoryZero(FPartArray, FSize);
    FOwns := True;
  End;
End;


Procedure TFslOrdinalSet.SetCount(Const iValue: Integer);
Begin
  FCount := iValue;
  Resize(RealCeiling(Count / 8));
End;


Procedure TFslOrdinalSet.SetSize(Const Value: Integer);
Begin
  Resize(Value);
  FCount := (FSize * 8);
End;  


Procedure TFslOrdinalSet.Assign(oObject: TFslObject);
Begin
  Inherited;

  PartsDispose;

  FSize := TFslOrdinalSet(oObject).Size;
  FCount := TFslOrdinalSet(oObject).Count;

  PartsNew;

  MemoryMove(TFslOrdinalSet(oObject).FPartArray, FPartArray, FSize);
End;



Function TFslOrdinalSet.ValidateIndex(Const sMethod : String; Const iIndex : Integer) : Boolean;
Begin
  If Not IntegerBetweenInclusive(0, iIndex, FCount - 1) Then
    Invariant(sMethod, StringFormat('Invalid index (%d In [0..%d])', [iIndex, FCount - 1]));

  Result := True;
End;


Procedure TFslOrdinalSet.Fill(bChecked : Boolean);
Begin
  If bChecked Then
    MemoryFill(FPartArray, FSize)
  Else
    MemoryZero(FPartArray, FSize);
End;


Procedure TFslOrdinalSet.Resize(Const iValue: Integer);
Begin
  If FOwns Then
  Begin
    MemoryResize(FPartArray, FSize, iValue);
    MemoryZero(Pointer(NativeUInt(FPartArray) + NativeUInt(FSize)), iValue - FSize);
  End;

  FSize := iValue;
End;


Function TFslOrdinalSet.Iterator : TFslIterator;
Begin
  Result := TFslOrdinalSetIterator.Create;
  TFslOrdinalSetIterator(Result).OrdinalSet := TFslOrdinalSet(Self.Link);
End;


Procedure TFslOrdinalSet.New(Const iCount: Integer);
Begin
  PartsDispose;

  FCount := iCount;
  FSize := RealCeiling(iCount / 8);

  PartsNew;
End;


Procedure TFslOrdinalSet.Hook(Const aValue; iCount : Integer);
Begin
  FPartArray := PFslOrdinalSetPartArray(@aValue);
  FCount := iCount;
  FSize := RealCeiling(iCount / 8);
  FOwns := False;
End;


Procedure TFslOrdinalSet.Unhook;
Begin
  FPartArray := Nil;
  FCount := 0;
  FSize := 0;
End;


Procedure TFslOrdinalSet.Check(iIndex: Integer);
Var
  pPart : PFslOrdinalSetPart;
Begin
  Assert(ValidateIndex('Check', iIndex));

  pPart := @FPartArray^[iIndex Div 32];

  pPart^ := pPart^ Or (1 Shl SignedMod(iIndex, 32));
End;


Procedure TFslOrdinalSet.CheckRange(iFromIndex, iToIndex : Integer);
Var
  iLoop : Integer;
Begin
  For iLoop := iFromIndex To iToIndex Do
    Check(iLoop);
End;


Procedure TFslOrdinalSet.UncheckRange(iFromIndex, iToIndex : Integer);
Var
  iLoop : Integer;
Begin
  For iLoop := iFromIndex To iToIndex Do
    Uncheck(iLoop);
End;


Procedure TFslOrdinalSet.Uncheck(iIndex: Integer);
Var
  pPart : PFslOrdinalSetPart;
Begin
  Assert(ValidateIndex('Uncheck', iIndex));

  pPart := @FPartArray^[iIndex Div 32];

  pPart^ := pPart^ And Not (1 Shl SignedMod(iIndex, 32));
End;


Procedure TFslOrdinalSet.Toggle(iIndex: Integer);
Var
  pPart : PFslOrdinalSetPart;
  iFlag : Integer;
Begin
  Assert(ValidateIndex('Toggle', iIndex));

  pPart := @FPartArray^[iIndex Div 32];

  iFlag := (1 Shl SignedMod(iIndex, 32));

  If (pPart^ And iFlag = 0) Then
    pPart^ := pPart^ Or iFlag
  Else
    pPart^ := pPart^ And Not iFlag;
End;


Function TFslOrdinalSet.Checked(iIndex: Integer): Boolean;
Begin
  Assert(ValidateIndex('Checked', iIndex));

  Result := FPartArray^[iIndex Div 32] And (1 Shl SignedMod(iIndex, 32)) <> 0;
End;


Function TFslOrdinalSet.CheckedRange(iFromIndex, iToIndex: Integer): Boolean;
Var
  iIndex : Integer;
Begin
  Assert(CheckCondition(iFromIndex <= iToIndex, 'CheckedRange', 'From Index and To Index are not valid.'));

  iIndex := iFromIndex;
  Result := True;
  While Result And (iIndex <= iToIndex) Do
  Begin
    Result := Checked(iIndex);
    Inc(iIndex);
  End;
End;


Function TFslOrdinalSet.AllChecked : Boolean;
Var
  iLoop : Integer;
Begin
  // TODO: optimisation possible?

  Result := True;
  iLoop := 0;
  While (iLoop < Count) And Result Do
  Begin
    Result := Checked(iLoop);
    Inc(iLoop);
  End;
End;


Function TFslOrdinalSet.AnyChecked : Boolean;
Var
  iLoop : Integer;
Begin
  // TODO: optimisation possible?

  Result := False;
  iLoop := 0;
  While (iLoop < Count) And Not Result Do
  Begin
    Result := Checked(iLoop);
    Inc(iLoop);
  End;
End;


Function TFslOrdinalSet.NoneChecked : Boolean;
Var
  iLoop : Integer;
Begin
  // TODO: optimisation possible?

  Result := True;
  iLoop := 0;
  While (iLoop < Count) And Result Do
  Begin
    Result := Not Checked(iLoop);
    Inc(iLoop);
  End;
End;


Procedure TFslOrdinalSetIterator.First;
Begin
  FPart := Low(FOrdinalSet.FPartArray^);
  FIndex := 0;
  FLoop := 0;

  // TODO: can FLoop be cached as (1 Shl FLoop) for optimisation?

  If FPart <= High(FOrdinalSet.FPartArray^) Then
    FValue := @(FOrdinalSet.FPartArray^[FPart]);
End;


Function TFslOrdinalSetIterator.Checked : Boolean;
Begin
  Result := (FValue^ And (1 Shl FLoop)) <> 0;
End;  


Procedure TFslOrdinalSetIterator.Check;
Begin 
  FValue^ := FValue^ Or (1 Shl FLoop);
End;  


Procedure TFslOrdinalSetIterator.Uncheck;
Begin 
  FValue^ := FValue^ And Not (1 Shl FLoop);
End;  


Procedure TFslOrdinalSetIterator.Next;
Begin
  Inc(FLoop);
  Inc(FIndex);

  If FLoop >= 32 Then
  Begin
    Inc(FPart);

    If FPart <= High(FOrdinalSet.FPartArray^) Then
      FValue := @FOrdinalSet.FPartArray^[FPart];

    FLoop := 0;
  End;  
End;  


Function TFslOrdinalSetIterator.More : Boolean;
Begin 
  Result := (FIndex < FOrdinalSet.Count) And (FPart <= High(FOrdinalSet.FPartArray^));
End;  


Constructor TFslOrdinalSetIterator.Create;
Begin
  Inherited;

  OrdinalSet := Nil;
End;  


Destructor TFslOrdinalSetIterator.Destroy;
Begin 
  FOrdinalSet.Free;

  Inherited;
End;  


Procedure TFslOrdinalSetIterator.SetOrdinalSet(Const Value: TFslOrdinalSet);
Begin
  FOrdinalSet.Free;
  FOrdinalSet := Value;
End;  


Function TFslOrdinalSet.GetIsChecked(Const iIndex: Integer): Boolean;
Begin
  Result := Checked(iIndex);
End;


Procedure TFslOrdinalSet.SetIsChecked(Const iIndex: Integer; Const Value: Boolean);
Begin
  If Value Then
    Check(iIndex)
  Else
    Uncheck(iIndex);
End;


Procedure TFslOrdinalSet.CheckAll;
Begin
  Fill(True);
End;


Procedure TFslOrdinalSet.UncheckAll;
Begin
  Fill(False);
End;



Constructor TFslObjectMatch.Create;
Begin
  Inherited;

  FKeyComparisonDelegate := CompareByKeyReference;
  FValueComparisonDelegate := CompareByValueReference;

  FNominatedKeyClass := ItemKeyClass;
  FNominatedValueClass := ItemValueClass;
End;


Destructor TFslObjectMatch.Destroy;
Begin
  Inherited;
End;


Function TFslObjectMatch.Link : TFslObjectMatch;
Begin
  Result := TFslObjectMatch(Inherited Link);
End;


Function TFslObjectMatch.CompareByKeyReference(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(Integer(PFslObjectMatchItem(pA)^.Key), Integer(PFslObjectMatchItem(pB)^.Key));
End;  


Function TFslObjectMatch.CompareByValueReference(pA, pB: Pointer): Integer;
Begin 
  Result := IntegerCompare(Integer(PFslObjectMatchItem(pA)^.Value), Integer(PFslObjectMatchItem(pB)^.Value));
End;


Procedure TFslObjectMatch.AssignItem(oItems : TFslItemList; iIndex: Integer);
Begin 
  Inherited;

  FMatchArray^[iIndex].Key := TFslObjectMatch(oItems).FMatchArray^[iIndex].Key.Clone;
  FMatchArray^[iIndex].Value := TFslObjectMatch(oItems).FMatchArray^[iIndex].Value.Clone;
End;  


Procedure TFslObjectMatch.InternalEmpty(iIndex, iLength : Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatchArray) + NativeUInt(iIndex * SizeOf(TFslObjectMatchItem))), (iLength * SizeOf(TFslObjectMatchItem)));
End;


Procedure TFslObjectMatch.InternalTruncate(iValue : Integer);
Var
  iLoop : Integer;
  oKey  : TFslObjectMatchKey;
  oValue : TFslObjectMatchValue;
Begin
  Inherited;

  For iLoop := iValue To Count - 1 Do
  Begin
    oKey := FMatchArray^[iLoop].Key;
    oValue := FMatchArray^[iLoop].Value;

    FMatchArray^[iLoop].Key := Nil;
    FMatchArray^[iLoop].Value := Nil;

    oKey.Free;
    oValue.Free;
  End;
End;


Procedure TFslObjectMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMatchArray, Capacity * SizeOf(TFslObjectMatchItem), iValue * SizeOf(TFslObjectMatchItem));
End;


Procedure TFslObjectMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin
  MemoryMove(@FMatchArray^[iSource], @FMatchArray^[iTarget], iCount * SizeOf(TFslObjectMatchItem));
End;  


Function TFslObjectMatch.IndexByKey(Const aKey: TFslObjectMatchKey): Integer;
Begin
  If Not Find(aKey, Nil, Result, FKeyComparisonDelegate) Then
    Result := -1;
End;


Function TFslObjectMatch.IndexByValue(Const aValue: TFslObjectMatchValue): Integer;
Begin
  If Not Find(Nil, aValue, Result, FValueComparisonDelegate) Then
    Result := -1;
End;


Function TFslObjectMatch.ExistsByKey(Const aKey: TFslObjectMatchKey): Boolean;
Begin
  Result := ExistsByIndex(IndexByKey(aKey));
End;


Function TFslObjectMatch.ExistsByValue(Const aValue: TFslObjectMatchValue): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(aValue));
End;


Function TFslObjectMatch.Add(Const aKey : TFslObjectMatchKey; Const aValue : TFslObjectMatchValue) : Integer;
Begin 
  Assert(ValidateKey('Add', aKey, 'aKey'));
  Assert(ValidateValue('Add', aValue, 'aValue'));

  Result := -1;

  If Not IsAllowDuplicates And Find(aKey, aValue, Result) Then
  Begin 
    aKey.Free;
    aValue.Free;

    If IsPreventDuplicates Then
      RaiseError('Add', StringFormat('Item already exists in match (%x=%x)', [Integer(aKey), Integer(aValue)]));
  End
  Else
  Begin 
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(aKey, aValue, Result);

    Insert(Result, aKey, aValue);
  End;  
End;  


Procedure TFslObjectMatch.Insert(iIndex : Integer; Const aKey : TFslObjectMatchKey; Const aValue : TFslObjectMatchValue);
Begin
  Assert(ValidateKey('Insert', aKey, 'aKey'));
  Assert(ValidateValue('Insert', aValue, 'aValue'));

  InternalInsert(iIndex);

  FMatchArray^[iIndex].Key := aKey;
  FMatchArray^[iIndex].Value := aValue;
End;


Procedure TFslObjectMatch.InternalInsert(iIndex : Integer);
Begin
  Inherited;

  Pointer(FMatchArray^[iIndex].Key) := Nil;
  Pointer(FMatchArray^[iIndex].Value) := Nil;
End;


Procedure TFslObjectMatch.InternalDelete(iIndex : Integer);
Begin
  Inherited;

  FMatchArray^[iIndex].Key.Free;
  FMatchArray^[iIndex].Key := Nil;

  FMatchArray^[iIndex].Value.Free;
  FMatchArray^[iIndex].Value := Nil;
End;


Procedure TFslObjectMatch.InternalExchange(iA, iB: Integer);
Var
  aTemp : TFslObjectMatchItem;
  pA : Pointer;
  pB : Pointer;
Begin
  pA := @FMatchArray^[iA];
  pB := @FMatchArray^[iB];

  aTemp := PFslObjectMatchItem(pA)^;
  PFslObjectMatchItem(pA)^ := PFslObjectMatchItem(pB)^;
  PFslObjectMatchItem(pB)^ := aTemp;
End;


Procedure TFslObjectMatch.Delete(Const aKey: TFslObjectMatchKey; Const aValue: TFslObjectMatchValue);
Var
  iIndex : Integer;
Begin
  If Not Find(aKey, aValue, iIndex) Then
    RaiseError('Delete', 'Key/Value pair could not be found in the match.');

  DeleteByIndex(iIndex);
End;


Function TFslObjectMatch.GetItem(iIndex : Integer): Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatchArray^[iIndex];
End;


Procedure TFslObjectMatch.SetItem(iIndex: Integer; aValue: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatchArray^[iIndex] := PFslObjectMatchItem(aValue)^;
End;  


Function TFslObjectMatch.GetKeyByIndex(iIndex : Integer): TFslObjectMatchKey;
Begin 
  Assert(ValidateIndex('GetKeyByIndex', iIndex));

  Result := FMatchArray^[iIndex].Key;
End;


Procedure TFslObjectMatch.SetKeyByIndex(iIndex : Integer; Const aKey : TFslObjectMatchKey);
Begin 
  Assert(ValidateIndex('SetKeyByIndex', iIndex));
  Assert(ValidateKey('SetKeyByIndex', aKey, 'aKey'));

  FMatchArray^[iIndex].Key.Free;
  FMatchArray^[iIndex].Key := aKey;
End;  


Function TFslObjectMatch.GetValueByIndex(iIndex : Integer): TFslObjectMatchValue;
Begin 
  Assert(ValidateIndex('GetValueByIndex', iIndex));

  Result := FMatchArray^[iIndex].Value;
End;  


Procedure TFslObjectMatch.SetValueByIndex(iIndex : Integer; Const aValue: TFslObjectMatchValue);
Begin 
  Assert(ValidateIndex('SetValueByIndex', iIndex));
  Assert(ValidateValue('SetValueByIndex', aValue, 'aValue'));

  FMatchArray^[iIndex].Value.Free;
  FMatchArray^[iIndex].Value := aValue;
End;


Function TFslObjectMatch.GetValueByKey(Const aKey : TFslObjectMatchKey): TFslObjectMatchValue;
Var
  iIndex : Integer;
Begin
  Assert(ValidateKey('GetValueByKey', aKey, 'aKey'));

  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else
  Begin 
    Result := FDefaultValue;

    If Not FForced Then
      RaiseError('GetValueByKey', 'Unable to get the value for the specified key.');
  End;  
End;  


Procedure TFslObjectMatch.SetValueByKey(Const aKey : TFslObjectMatchKey; Const aValue: TFslObjectMatchValue);
Var
  iIndex : Integer;
Begin 
  Assert(ValidateKey('SetValueByKey', aKey, 'aKey'));
  Assert(ValidateValue('SetValueByKey', aValue, 'aValue'));

  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    ValueByIndex[iIndex] := aValue
  Else If FForced Then
    Add(aKey.Link, aValue)
  Else
  Begin 
    aValue.Free;
    RaiseError('SetValueByKey', 'Unable to set the value for the specified key.');
  End;  
End;


Function TFslObjectMatch.GetAsText : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := 0 To Count - 1 Do
    Result := Result + KeyByIndex[iLoop].ClassName + '=' + ValueByIndex[iLoop].ClassName + #13#10;
End;  


Procedure TFslObjectMatch.Merge(oMatch: TFslObjectMatch);
Var
  iLoop : Integer;
Begin 
  For iLoop := 0 To oMatch.Count - 1 Do
    Add(oMatch.KeyByIndex[iLoop].Link, oMatch.ValueByIndex[iLoop].Link);
End;


Function TFslObjectMatch.CapacityLimit : Integer;
Begin 
  Result := High(TFslObjectMatchItemArray);
End;  


Procedure TFslObjectMatch.DefaultCompare(Out aCompare: TFslItemListCompare);
Begin
  aCompare := CompareByKeyReference;
End;  


Function TFslObjectMatch.Find(Const aKey : TFslObjectMatchKey; Const aValue : TFslObjectMatchValue; Out iIndex : Integer; aCompare : TFslItemListCompare) : Boolean;
Var
  aItem : TFslObjectMatchItem;
Begin
  Assert(ValidateKey('Find', aKey, 'aKey'));
  Assert(ValidateValue('Find', aValue, 'aValue'));

  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Find(@aItem, iIndex, aCompare);
End;


Function TFslObjectMatch.IsSortedByKey : Boolean;
Begin 
  Result := IsSortedBy(FKeyComparisonDelegate);
End;


Function TFslObjectMatch.IsSortedByValue : Boolean;
Begin
  Result := IsSortedBy(FValueComparisonDelegate);
End;  


Function TFslObjectMatch.ValidateIndex(Const sMethod : String; iIndex: Integer): Boolean;
Begin 
  Inherited ValidateIndex(sMethod, iIndex);

  ValidateKey(sMethod, FMatchArray^[iIndex].Key, 'FMatchArray^[' + IntegerToString(iIndex) + '].Key');
  ValidateValue(sMethod, FMatchArray^[iIndex].Value, 'FMatchArray^[' + IntegerToString(iIndex) + '].Value');

  Result := True;
End;  


Function TFslObjectMatch.ValidateKey(Const sMethod : String; oObject : TFslObject; Const sObject : String) : Boolean;
Begin 
  If Assigned(oObject) Then
    Invariants(sMethod, oObject, FNominatedKeyClass, sObject);

  Result := True;
End;  


Function TFslObjectMatch.ValidateValue(Const sMethod : String; oObject : TFslObject; Const sObject : String) : Boolean;
Begin 
  If Assigned(oObject) Then
    Invariants(sMethod, oObject, FNominatedValueClass, sObject);

  Result := True;
End;


Function TFslObjectMatch.ItemKeyClass : TFslObjectClass;
Begin
  Result := TFslObject;
End;


Function TFslObjectMatch.ItemValueClass : TFslObjectClass;
Begin
  Result := TFslObject;
End;


Procedure TFslObjectMatch.SortedByKey;
Begin
  SortedBy(FKeyComparisonDelegate);
End;


Procedure TFslObjectMatch.SortedByValue;
Begin
  SortedBy(FValueComparisonDelegate);
End;


Function TFslObjectMatch.GetKeyByValue(Const aValue: TFslObjectMatchValue): TFslObjectMatchKey;
Var
  iIndex : Integer;
Begin
  iIndex := IndexByValue(aValue);

  If ExistsByIndex(iIndex) Then
    Result := KeyByIndex[iIndex]
  Else
    Result := FDefaultKey;
End;


Function TFslObjectMatch.GetMatchByIndex(iIndex : Integer) : TFslObjectMatchItem;
Begin
  Assert(ValidateIndex('GetMatchByIndex', iIndex));

  Result := FMatchArray^[iIndex];
End;


function TFslObjectMatch.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, capacity * sizeof(TFslObjectMatchItem));
  inc(result, FDefaultKey.sizeInBytes);
  inc(result, FDefaultValue.sizeInBytes);
end;

constructor TFslObjectList.Create;
begin
  inherited;

  FItemClass := ItemClass;
end;


destructor TFslObjectList.Destroy;
begin
  inherited;
end;


function TFslObjectList.Link: TFslObjectList;
begin
  Result := TFslObjectList(inherited Link);
end;


function TFslObjectList.Clone: TFslObjectList;
begin
  Result := TFslObjectList(inherited Clone);
end;


function TFslObjectList.ErrorClass: EFslExceptionClass;
begin
  Result := EFslObjectList;
end;


procedure TFslObjectList.AssignItem(oItems: TFslItemList; iIndex: integer);
begin
  FObjectArray^[iIndex] := TFslObjectList(oItems).FObjectArray^[iIndex].Clone;
end;


function TFslObjectList.ValidateIndex(const sMethod: string; iIndex: integer): boolean;
begin
  Result := inherited ValidateIndex(sMethod, iIndex);

  ValidateItem(sMethod, FObjectArray^[iIndex], 'FObjectArray^[' +
    IntegerToString(iIndex) + ']');
end;


function TFslObjectList.ValidateItem(const sMethod: string;
  oObject: TFslObject; const sObject: string): boolean;
begin
  if Assigned(oObject) or not AllowUnassigned then
    Invariants(sMethod, oObject, FItemClass, sObject);

  Result := True;
end;


procedure TFslObjectList.InternalTruncate(iValue: integer);
var
  oValue: TFslObject;
  iLoop: integer;
begin
  inherited;

  for iLoop := iValue to Count - 1 do
  begin
    oValue := FObjectArray^[iLoop];

    InternalBeforeExclude(iLoop, oValue);

    FObjectArray^[iLoop] := nil;

    Assert(not Assigned(oValue) or Invariants('InternalResize', oValue,
      FItemClass, 'oValue'));

    oValue.Free;
  end;
end;


procedure TFslObjectList.InternalResize(iValue: integer);
begin
  inherited;

  MemoryResize(FObjectArray, Capacity * SizeOf(TFslObject), iValue * SizeOf(TFslObject));
end;


procedure TFslObjectList.InternalEmpty(iIndex, iLength: integer);
begin
  inherited;

  MemoryZero(Pointer(NativeUInt(FObjectArray) + NativeUInt((iIndex * SizeOf(TFslObject)))),
    (iLength * SizeOf(TFslObject)));
end;


procedure TFslObjectList.InternalCopy(iSource, iTarget, iCount: integer);
begin
  inherited;

  MemoryMove(@FObjectArray^[iSource], @FObjectArray^[iTarget], iCount *
    SizeOf(TFslObject));
end;


procedure TFslObjectList.InternalInsert(iIndex: integer);
begin
  inherited;

  FObjectArray^[iIndex] := nil;
end;


procedure TFslObjectList.InternalDelete(iIndex: integer);
Var
  oValue : TFslObject;
begin
  inherited;

  oValue := FObjectArray^[iIndex];

  InternalBeforeExclude(iIndex, oValue);

  FObjectArray^[iIndex].Free;
  FObjectArray^[iIndex] := nil;
end;


function TFslObjectList.ItemNew: TFslObject;
begin
  Result := FItemClass.Create;
end;


procedure TFslObjectList.Collect(oList: TFslObjectList);
begin
  oList.Clear;
  oList.AddAll(Self);
end;


procedure TFslObjectList.AddAll(oList: TFslObjectList);
var
  iIndex: integer;
begin
  Assert(CheckCondition(oList <> Self, 'AddAll',
    'Cannot addall items from a list to itself.'));

  if (oList <> nil) then
  begin
    Capacity := IntegerMax(Count + oList.Count, Capacity);
    for iIndex := 0 to oList.Count - 1 do
      Add(oList[iIndex].Link);
  end;
end;


procedure TFslObjectList.DeleteByDefault(oValue: TFslObject);
begin
  DeleteBy(oValue, DefaultComparison);
end;


procedure TFslObjectList.DeleteBy(oValue: TFslObject; aCompare: TFslItemListCompare);
var
  iIndex: integer;
begin
  Assert(ValidateItem('DeleteBy', oValue, 'oValue'));

  if not Find(oValue, iIndex, aCompare) then
    RaiseError('DeleteBy', 'Object not found in list.');

  DeleteByIndex(iIndex);
end;


function TFslObjectList.IndexBy(oValue: TFslObject; aCompare: TFslItemListCompare): integer;
begin
  if not Find(oValue, Result, aCompare) then
    Result := -1;
end;


function TFslObjectList.ExistsBy(oValue: TFslObject;
  aCompare: TFslItemListCompare): boolean;
var
  iIndex: integer;
begin
  Result := not IsEmpty and Find(oValue, iIndex, aCompare);
end;


function TFslObjectList.GetBy(oValue: TFslObject;
  aCompare: TFslItemListCompare): TFslObject;
var
  iIndex: integer;
begin
  if Find(oValue, iIndex, aCompare) then
    Result := ObjectByIndex[iIndex]
  else
    Result := nil;
end;


function TFslObjectList.ForceBy(oValue: TFslObject;
  aCompare: TFslItemListCompare): TFslObject;
var
  iIndex: integer;
begin
  if Find(oValue, iIndex, aCompare) then
    Result := ObjectByIndex[iIndex]
  else
  begin
    Result := oValue;

    Insert(iIndex, oValue.Link);
  end;
end;


function TFslObjectList.CompareByReference(pA, pB: Pointer): integer;
begin
  Result := IntegerCompare(integer(pA), integer(pB));
end;


function TFslObjectList.CompareByClass(pA, pB: Pointer): integer;
var
  aClassA: TFslObjectClass;
  aClassB: TFslObjectClass;
begin
  if Assigned(pA) then
    aClassA := TFslObject(pA).ClassType
  else
    aClassA := nil;

  if Assigned(pB) then
    aClassB := TFslObject(pB).ClassType
  else
    aClassB := nil;

  Result := IntegerCompare(integer(aClassA), integer(aClassB));
end;


function TFslObjectList.Find(oValue: TFslObject): integer;
begin
  Find(oValue, Result);
end;


function TFslObjectList.IndexByClass(aClass: TFslObjectClass): integer;
var
  oValue: TFslObject;
begin
  Assert(Invariants('IndexByClass', aClass, FItemClass, 'aClass'));

  oValue := aClass.Create;
  try
    Result := IndexByClass(oValue);
  finally
    oValue.Free;
  end;
end;


function TFslObjectList.IndexByClass(oValue: TFslObject): integer;
begin
  Assert(ValidateItem('IndexByClass', oValue, 'oValue'));

  Result := IndexBy(oValue, CompareByClass);
end;


function TFslObjectList.ExistsByClass(aClass: TFslObjectClass): boolean;
begin
  Result := ExistsByIndex(IndexByClass(aClass));
end;


function TFslObjectList.GetByClass(aClass: TFslObjectClass): TFslObject;
begin
  Result := Get(IndexByClass(aClass));
end;


function TFslObjectList.IndexByReference(oValue: TFslObject): integer;
begin
  Assert(ValidateItem('IndexByReference', oValue, 'oValue'));

  Result := IndexBy(oValue,CompareByReference);
end;


function TFslObjectList.ExistsByReference(oValue: TFslObject): boolean;
begin
  Result := ExistsByIndex(IndexByReference(oValue));
end;


function TFslObjectList.IndexByDefault(oValue: TFslObject): integer;
begin
  Assert(ValidateItem('IndexByDefault', oValue, 'oValue'));

  Result := IndexBy(oValue, DefaultComparison);
end;


function TFslObjectList.ExistsByDefault(oValue: TFslObject): boolean;
begin
  Result := ExistsByIndex(IndexByDefault(oValue));
end;


function TFslObjectList.Add(oValue: TFslObject): integer;
begin
  Assert(ValidateItem('Add', oValue, 'oValue'));

  Result := -1;

  if not IsAllowDuplicates and Find(oValue, Result) then
  begin
    oValue.Free; // free ignored object

    if IsPreventDuplicates then
      RaiseError('Add', 'Object already exists in list.');
  end
  else
  begin
    if not IsSorted then
      Result := Count
    else if (Result < 0) then
      Find(oValue, Result);

    Insert(Result, oValue);
  end;
end;


procedure TFslObjectList.Insert(iIndex: integer; oValue: TFslObject);
begin
  Assert(ValidateItem('Insert', oValue, 'oValue'));
  Assert(CheckCondition(not IsSorted or (Find(oValue) = iIndex), 'Insert',
    'Cannot insert into a sorted list unless the index is correct.'));
  Assert(Insertable('Insert', oValue));

  InternalInsert(iIndex);

  FObjectArray^[iIndex] := oValue;

  InternalAfterInclude(iIndex, oValue);
end;


procedure TFslObjectList.DeleteByReference(oValue: TFslObject);
begin
  DeleteBy(oValue, CompareByReference);
end;


procedure TFslObjectList.DeleteAllByReference(oValue: TFslObjectList);
var
  iIndex: integer;
begin
  for iIndex := 0 to oValue.Count - 1 do
    DeleteByReference(oValue[iIndex]);
end;


procedure TFslObjectList.DeleteByClass(aClass: TFslObjectClass);
var
  iIndex: integer;
begin
  Assert(Invariants('DeleteByClass', aClass, FItemClass, 'aClass'));

  iIndex := IndexByClass(aClass);

  if not ExistsByIndex(iIndex) then
    RaiseError('DeleteByClass', StringFormat('Object of class ''%s'' not found in list.',
      [aClass.ClassName]));

  DeleteByIndex(iIndex);
end;


function TFslObjectList.RemoveLast: TFslObject;
begin
  if Count <= 0 then
    Result := nil
  else
  begin
    Result := FObjectArray^[Count - 1].Link;
    DeleteByIndex(Count - 1);

    Assert(ValidateItem('RemoveLast', Result, 'Result'));
  end;
end;


function TFslObjectList.RemoveFirst: TFslObject;
begin
  if Count <= 0 then
    Result := nil
  else
  begin
    Result := FObjectArray^[0].Link;
    try
      DeleteByIndex(0);
    except
      Result.Free;

      raise;
    end;

    Assert(ValidateItem('RemoveFirst', Result, 'Result'));
  end;
end;


procedure TFslObjectList.InternalExchange(iA, iB: integer);
var
  pTemp: TFslObject;
begin
  pTemp := FObjectArray[iA];
  FObjectArray[iA] := FObjectArray[iB];
  FObjectArray[iB] := pTemp;
end;


procedure TFslObjectList.Move(iSource, iTarget: integer);
var
  oObject: TFslObject;
begin
  Assert(CheckCondition(iSource <> iTarget, 'Move', 'Can''t move the same index.'));

  oObject := ObjectByIndex[iSource].Link;
  try
    DeleteByIndex(iSource);
    Insert(iTarget, oObject.Link);
  finally
    oObject.Free;
  end;
end;


function TFslObjectList.Find(pValue: Pointer; Out iIndex: integer;
  aCompare: TFslItemListCompare): boolean;
begin
  Assert(Invariants('Find', TFslObject(pValue), FItemClass, 'pValue'));

  Result := inherited Find(pValue, iIndex, aCompare);
end;


function TFslObjectList.AddressOfItem(iIndex: integer): PFslObject;
begin
  Assert(ValidateIndex('AddressOfItem', iIndex));

  Result := @(FObjectArray^[iIndex]);
end;


function TFslObjectList.GetItemClass: TFslObjectClass;
begin
  Result := FItemClass;
end;


procedure TFslObjectList.SetItemClass(const Value: TFslObjectClass);
begin
  Assert(CheckCondition(Count = 0, 'SetItemClass',
    'Cannot change ItemClass once objects are present in the list.'));

  FItemClass := Value;
end;


function TFslObjectList.GetItem(iIndex: integer): Pointer;
begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := FObjectArray^[iIndex];
end;


procedure TFslObjectList.SetItem(iIndex: integer; pValue: Pointer);
begin
  Assert(ValidateIndex('SetItem', iIndex));

  FObjectArray^[iIndex] := TFslObject(pValue);
end;


function TFslObjectList.GetObject(iIndex: integer): TFslObject;
begin
  Assert(ValidateIndex('GetObject', iIndex));

  Result := FObjectArray^[iIndex];
end;


procedure TFslObjectList.SetObject(iIndex: integer; const oValue: TFslObject);
Var
  oExclude : TFslObject;
begin
  Assert(ValidateIndex('SetObject', iIndex));
  Assert(Replaceable('SetObject', iIndex));
  Assert(Replaceable('SetObject', FObjectArray^[iIndex], oValue));

  oExclude := FObjectArray^[iIndex];

  InternalBeforeExclude(iIndex, oExclude);

  FObjectArray^[iIndex].Free;
  FObjectArray^[iIndex] := oValue;

  InternalAfterInclude(iIndex, oValue);
end;


function TFslObjectList.CapacityLimit: integer;
begin
  Result := High(TFslObjectArray);
end;


function TFslObjectList.Get(iIndex: integer): TFslObject;
begin
  if ExistsByIndex(iIndex) then
    Result := ObjectByIndex[iIndex]
  else
    Result := nil;
end;


function TFslObjectList.GetByIndex(iIndex: integer): TFslObject;
begin
  Result := ObjectByIndex[iIndex];
end;


function TFslObjectList.Iterator: TFslIterator;
begin
  Result := ProduceIterator;
end;


procedure TFslObjectList.SortedByClass;
begin
  SortedBy(CompareByClass);
end;


procedure TFslObjectList.OrderedByClass;
begin
  OrderedBy(CompareByClass);
end;


function TFslObjectList.IsSortedByClass: boolean;
begin
  Result := IsSortedBy(CompareByClass);
end;


function TFslObjectList.IsOrderedByClass: boolean;
begin
  Result := IsOrderedBy(CompareByClass);
end;


procedure TFslObjectList.SortedByReference;
begin
  SortedBy(CompareByReference);
end;


procedure TFslObjectList.OrderedByReference;
begin
  OrderedBy(CompareByReference);
end;


function TFslObjectList.IsSortedByReference: boolean;
begin
  Result := IsSortedBy(CompareByReference);
end;


function TFslObjectList.IsOrderedByReference: boolean;
begin
  Result := IsOrderedBy(CompareByReference);
end;


function TFslObjectList.Deleteable(const sMethod: string; iIndex: integer): boolean;
begin
  Result := inherited Deleteable(sMethod, iIndex);

  Deleteable(sMethod, FObjectArray^[iIndex]);
end;


function TFslObjectList.Deleteable(const sMethod: string; oObject: TFslObject): boolean;
begin
  Alterable(sMethod);

  Result := True;
end;


function TFslObjectList.Insertable(const sMethod: string; oObject: TFslObject): boolean;
begin
  Alterable(sMethod);

  Result := True;
end;


function TFslObjectList.Replaceable(const sMethod: string;
  oOld, oNew: TFslObject): boolean;
begin
  Alterable(sMethod);
  ValidateItem(sMethod, oOld, 'oOld');
  ValidateItem(sMethod, oNew, 'oNew');

  Result := True;
end;


function TFslObjectList.Extendable(const sMethod: string; iCount: integer): boolean;
begin
  Result := inherited Extendable(sMethod, iCount);

  if (iCount > Count) and not AllowUnassigned then
    Invariant(sMethod,
      'Unable to extend the Count of the list as it does not allow unassigned entries.');
end;


function TFslObjectList.New: TFslObject;
begin
  Result := ItemNew;
end;


procedure TFslObjectList.InternalAfterInclude(iIndex: integer; oObject : TFslObject);
begin
end;


procedure TFslObjectList.InternalBeforeExclude(iIndex: integer; oObject : TFslObject);
begin
end;


function TFslObjectList.AllowUnassigned: boolean;
begin
  Result := True;
end;


function TFslObjectList.IteratorClass: TFslObjectListIteratorClass;
begin
  Result := TFslObjectListIterator;
end;


procedure TFslObjectList.ConsumeIterator(oIterator: TFslObjectListIterator);
begin
  Assert(Invariants('ConsumeIterator', oIterator, IteratorClass, 'oIterator'));
  Assert(CheckCondition(oIterator.List = Self, 'ConsumeIterator',
    'Iterator was not produced by this list.'));

  oIterator.Free;
end;


function TFslObjectList.ProduceIterator: TFslObjectListIterator;
begin
  Result := IteratorClass.Create;
  Result.List := Self.Link;
end;


function TFslObjectList.GetObjectByIndex(const iIndex: integer): TFslObject;
begin
  Result := GetObject(iIndex);
end;


procedure TFslObjectList.SetObjectByIndex(const iIndex: integer;
  const Value: TFslObject);
begin
  SetObject(iIndex, Value);
end;


constructor TFslObjectListIterator.Create;
begin
  inherited;

  FList := nil;
end;


destructor TFslObjectListIterator.Destroy;
begin
  FList.Free;

  inherited;
end;


procedure TFslObjectListIterator.First;
begin
  inherited;

  FIndex := 0;
  FDeleted := False;

  StepNext;
end;


procedure TFslObjectListIterator.Last;
begin
  inherited;

  FIndex := List.Count - 1;
  FDeleted := False;

  StepBack;
end;


procedure TFslObjectListIterator.Back;
begin
  inherited;

  Dec(FIndex);
  StepBack;

  FDeleted := False;
end;


procedure TFslObjectListIterator.Next;
begin
  inherited;

  if not FDeleted then
    Inc(FIndex);

  StepNext;

  FDeleted := False;
end;


function TFslObjectListIterator.Current: TFslObject;
begin
  Assert(CheckCondition(not FDeleted, 'Current', 'Current element has been deleted'));

  Result := List[FIndex];
end;


function TFslObjectListIterator.More: boolean;
begin
  Result := List.ExistsByIndex(FIndex);
end;


procedure TFslObjectListIterator.Delete;
begin
  Assert(CheckCondition(not FDeleted, 'Delete', 'Current element has already been deleted'));

  List.DeleteByIndex(FIndex);
  FDeleted := True;
end;


function TFslObjectListIterator.GetList: TFslObjectList;
begin
  Assert(Invariants('GetList', FList, TFslObjectList, 'FList'));

  Result := FList;
end;


procedure TFslObjectListIterator.SetList(const Value: TFslObjectList);
begin
  Assert(not Assigned(FList) or Invariants('SetList', Value, TFslObjectList, 'Value'));

  FList.Free;
  FList := Value;
end;


procedure TFslObjectListIterator.StepBack;
begin
  while More and Skip do
    Dec(FIndex);
end;


procedure TFslObjectListIterator.StepNext;
begin
  while More and Skip do
    Inc(FIndex);
end;


function TFslObjectListIterator.Skip: boolean;
begin
  Result := False;
end;


function TFslObjectListIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

function TFslObjectList.ContainsAllByReference(oObjectList: TFslObjectList): boolean;
var
  iObjectIndex: integer;
begin
  Result := True;
  iObjectIndex := 0;
  while (iObjectIndex < oObjectList.Count) and Result do
  begin
    Result := ExistsByReference(oObjectList[iObjectIndex]);
    Inc(iObjectIndex);
  end;
end;


function TFslObjectList.ContainsAnyByReference(oObjectList: TFslObjectList): boolean;
var
  iObjectIndex: integer;
begin
  Result := False;
  iObjectIndex := 0;
  while (iObjectIndex < oObjectList.Count) and not Result do
  begin
    Result := ExistsByReference(oObjectList[iObjectIndex]);
    Inc(iObjectIndex);
  end;
end;



Type
  TMethod = Record
    Code, Data : Pointer;
  End;


Constructor TFslItemList.Create;
Begin
  Inherited;

  DefaultCompare(FComparison);

  FDirection := 1;
End;


Destructor TFslItemList.Destroy;
Begin
  InternalResize(0);

  Inherited;
End;


Function TFslItemList.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslItemList;
End;


Procedure TFslItemList.Assign(oSource: TFslObject);
Var
  iLoop : Integer;
Begin
  Inherited;

  Clear;

  Assert(CheckCondition(Count = 0, 'Assign', 'Collection must be empty of all items after clear.'));

  Count := TFslItemList(oSource).Count;

  For iLoop := 0 To Count - 1 Do
    AssignItem(TFslItemList(oSource), iLoop);

  If Count > 0 Then
  Begin
    // TODO: below reflects what is currently happening due to AssignItem. This
    //       should be changed to use an 'Add' approach similar to LoadItem so that
    //       you can assign from one ordered list into another and retain individual ordering rules.

    // Comparison is set this way because we can't directly assign a method pointer off another object.
    TMethod(FComparison).Data := Self;
    TMethod(FComparison).Code := TMethod(TFslItemList(oSource).FComparison).Code;

    FSorted := TFslItemList(oSource).FSorted;
    FDirection := TFslItemList(oSource).FDirection;
    FDuplicates := TFslItemList(oSource).FDuplicates;
  End;
End;



Procedure TFslItemList.AssignItem(oItems : TFslItemList; iIndex: Integer);
Begin
End;


Function TFslItemList.ExistsByIndex(Const iIndex : Integer) : Boolean;
Begin
  Result := (iIndex >= 0) And (iIndex < FCount);
End;


Function TFslItemList.ValidateIndex(Const sMethod: String; iIndex: Integer) : Boolean;
Begin
  If Not ExistsByIndex(iIndex) Then
    Invariant(sMethod, StringFormat('Invalid index (%d In [0..%d])', [iIndex, FCount - 1]));

  Result := True;
End;


Procedure TFslItemList.InternalClear;
Begin
  Inherited;

  InternalTruncate(0);
  FCount := 0;
End;


Procedure TFslItemList.InternalGrow;
Var
  iDelta : Integer;
Begin
  If FCapacity > 64 Then           // FCapacity : iDelta
    iDelta := FCapacity Div 4      // >  64     : >16
  Else If FCapacity > 8 Then
    iDelta := 16                   // <= 64     : +16
  Else
    iDelta := 4;                   // <=  8     : +4

  SetCapacity(FCapacity + iDelta);
End;


Procedure TFslItemList.InternalEmpty(iIndex, iLength: Integer);
Begin
End;


Procedure TFslItemList.InternalResize(iValue : Integer);
Begin
End;


Procedure TFslItemList.InternalTruncate(iValue: Integer);
Begin
End;


Procedure TFslItemList.InternalCopy(iSource, iTarget, iCount: Integer);
Begin
End;


Procedure TFslItemList.InternalSort;

  Procedure QuickSort(L, R: Integer);
  Var
    I, J, K : Integer;
    pK : Pointer;
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
        pK := ItemByIndex[K];

        While (FComparison(ItemByIndex[I], pK) * FDirection < 0) Do
          Inc(I);

        While (FComparison(ItemByIndex[J], pK) * FDirection > 0) Do
          Dec(J);

        If I <= J Then
        Begin
          InternalExchange(I, J);

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
        QuickSort(L, J);

      L := I;
    Until I >= R;
  End;

Begin
  Assert(CheckCondition(Assigned(FComparison), 'Sort', 'Comparison property must be assigned.'));
  Assert(CheckCondition(FDirection <> 0, 'Sort', 'Direction must be non-zero'));

  If (FCount > 1) Then
    QuickSort(0, FCount - 1);              // call the quicksort routine
End;


Procedure TFslItemList.InternalSort(aCompare: TFslItemListCompare; iDirection : TFslItemListDirection);
Begin
  Assert(CheckCondition(Assigned(aCompare), 'Sort', 'Comparison parameter must be assigned.'));

  If iDirection <> 0 Then
    FDirection := iDirection;

  FComparison := aCompare;

  FSorted := False;
  InternalSort;
  FSorted := True;
End;


Function TFslItemList.Find(pValue : Pointer; Out iIndex: Integer; aCompare : TFslItemListCompare): Boolean;
Var
  L, H, I, C : Integer;
Begin
  // Ensure we have a compare event specified
  If Not Assigned(aCompare) Then
    aCompare := FComparison;

  If Not IsSortedBy(aCompare) Then
  Begin
    iIndex := 0;
    While (iIndex < FCount) And (aCompare(ItemByIndex[iIndex], pValue) <> 0) Do
      Inc(iIndex);

    Result := iIndex < FCount; // iIndex will be FCount if it doesn't find the Item
  End
  Else
  Begin
    Result := False;
    L := 0;
    H := FCount - 1;

    While L <= H Do
    Begin
      I := (L + H) Shr 1;
      C := aCompare(ItemByIndex[I], pValue) * FDirection;

      If C < 0 Then
        L := I + 1
      Else
      Begin
        H := I - 1;

        If C = 0 Then
        Begin
          Result := True;

          If FDuplicates <> dupAccept Then
            L := I;
        End;
      End;
    End;

    iIndex := L;
  End;
End;


Function TFslItemList.CompareItem(pA, pB : Pointer): Integer;
Begin
  Result := IntegerCompare(Integer(pA), Integer(pB));
End;


Procedure TFslItemList.DefaultCompare(Out aCompare : TFslItemListCompare);
Begin
  aCompare := CompareItem;
End;


Procedure TFslItemList.InternalInsert(iIndex : Integer);
Begin
  // Insert is allowed one element past the end of the items
  Assert(Insertable('InternalInsert', iIndex));

  If FCount >= CountLimit Then
    Invariant('InternalInsert', StringFormat('Cannot set count to %d as it not compatible with the range [0..%d]', [FCount+1, CountLimit]));

  If FCount >= FCapacity Then
    InternalGrow;

  If iIndex < FCount Then
    InternalCopy(iIndex, iIndex + 1, FCount - iIndex);

  Inc(FCount);
End;


Procedure TFslItemList.InternalDelete(iIndex : Integer);
Begin
End;


Procedure TFslItemList.DeleteByIndex(iIndex: Integer);
Begin
  Assert(Deleteable('DeleteByIndex', iIndex));

  InternalDelete(iIndex);

  Dec(FCount);

  If iIndex < FCount Then
    InternalCopy(iIndex + 1, iIndex, FCount - iIndex);
End;


Procedure TFslItemList.DeleteRange(iFromIndex, iToIndex : Integer);
Var
  iLoop : Integer;
Begin
  Assert(Deleteable('DeleteRange', iFromIndex, iToIndex));

  For iLoop := iFromIndex To iToIndex Do
    InternalDelete(iLoop);

  If iToIndex < FCount Then
    InternalCopy(iToIndex + 1, iFromIndex, FCount - iToIndex - 1);

  Dec(FCount, iToIndex - iFromIndex + 1);
End;


Procedure TFslItemList.InternalExchange(iA, iB : Integer);
Var
  pTemp : Pointer;
Begin
  pTemp := ItemByIndex[iA];
  ItemByIndex[iA] := ItemByIndex[iB];
  ItemByIndex[iB] := pTemp;
End;


Procedure TFslItemList.SetCapacity(Const iValue: Integer);
Begin
  Assert(CheckCondition((iValue >= FCount) And (iValue <= CapacityLimit), 'SetCapacity', StringFormat('Unable to change the capacity to %d', [iValue])));

  InternalResize(iValue);
  FCapacity := iValue;
End;


Procedure TFslItemList.SetCount(Const iValue: Integer);
Var
  iIndex : Integer;
Begin
  Assert(Extendable('SetCount', iValue));

  // Remove the lost items from the list
  For iIndex := FCount - 1 DownTo iValue Do
    InternalDelete(iIndex);

  If iValue > FCapacity Then
    SetCapacity(iValue);

  // Clear the items which are now in the list
  If iValue > FCount Then
    InternalEmpty(FCount, iValue - FCount);

  FCount := iValue;
End;


Procedure TFslItemList.SortedBy(Const bValue : Boolean);
Begin

  If FSorted <> bValue Then
  Begin
    FSorted := bValue;

    If FSorted Then
      InternalSort;
  End;
End;


Procedure TFslItemList.ComparedBy(Const Value: TFslItemListCompare);
Begin
  Assert(Alterable('ComparedBy'));

  If Not IsComparedBy(Value) Then
  Begin
    FComparison := Value;
    FSorted := False;
  End;
End;


Procedure TFslItemList.DirectionBy(Const Value: TFslItemListDirection);
Begin
  Assert(Alterable('DirectionBy'));

  If FDirection <> Value Then
  Begin
    FDirection := Value;
    FSorted := False;
  End;
End;


Procedure TFslItemList.DuplicateBy(Const Value: TFslItemListDuplicates);
Begin
  Assert(Alterable('DuplicateBy'));

  FDuplicates := Value;
End;


Function TFslItemList.CapacityLimit : Integer;
Begin
  Invariant('CapacityLimit', 'CapacityLimit not specified.');

  Result := 0;
End;


Function TFslItemList.CountLimit : Integer;
Begin
  Result := CapacityLimit;
End;


Function TFslItemList.IsComparedBy(Const aCompare: TFslItemListCompare) : Boolean;
Begin
  Result := (TMethod(FComparison).Data = TMethod(aCompare).Data) And (TMethod(FComparison).Code = TMethod(aCompare).Code);
End;


Function TFslItemList.IsCompared : Boolean;
Begin
  Result := Assigned(@FComparison);
End;


Function TFslItemList.IsUncompared : Boolean;
Begin
  Result := Not IsCompared;
End;


Function TFslItemList.IsSortedBy(Const aCompare: TFslItemListCompare) : Boolean;
Begin
  Result := FSorted And IsComparedBy(aCompare);
End;


Procedure TFslItemList.SortedBy(Const aCompare: TFslItemListCompare);
Begin
  ComparedBy(aCompare);
  SortedBy(True);
End;


Procedure TFslItemList.AllowDuplicates;
Begin
  DuplicateBy(dupAccept);
End;


Procedure TFslItemList.IgnoreDuplicates;
Begin
  DuplicateBy(dupIgnore);

  // TODO: Delete duplicates?
End;


Procedure TFslItemList.PreventDuplicates;
Begin
  DuplicateBy(dupException);

  // TODO: Assert that there are no duplicates?
End;


Function TFslItemList.IsAllowDuplicates : Boolean;
Begin
  Result := FDuplicates = dupAccept;
End;


Function TFslItemList.IsIgnoreDuplicates : Boolean;
Begin
  Result := FDuplicates = dupIgnore;
End;


Function TFslItemList.IsPreventDuplicates : Boolean;
Begin
  Result := FDuplicates = dupException;
End;


Procedure TFslItemList.Sorted;
Begin
  SortedBy(True);
End;


Procedure TFslItemList.Unsorted;
Begin
  SortedBy(False);
End;


Procedure TFslItemList.Uncompared;
Begin
  FComparison := Nil;
  FSorted := False;
End;


Procedure TFslItemList.SortAscending;
Begin
  DirectionBy(1);
End;


Procedure TFslItemList.SortDescending;
Begin
  DirectionBy(-1);
End;


Function TFslItemList.IsSortAscending : Boolean;
Begin
  Result := FDirection > 0;
End;


Function TFslItemList.IsSortDescending : Boolean;
Begin
  Result := FDirection < 0;
End;


Function TFslItemList.IsSorted : Boolean;
Begin
  Result := FSorted;
End;


Function TFslItemList.IsUnsorted : Boolean;
Begin
  Result := Not FSorted;
End;


Function TFslItemList.IsEmpty : Boolean;
Begin
  Result := FCount <= 0;
End;


Function TFslItemList.Deleteable(Const sMethod: String; iIndex: Integer): Boolean;
Begin
  Result := Alterable(sMethod);

  ValidateIndex(sMethod, iIndex);
End;


Function TFslItemList.Deleteable(Const sMethod: String; iFromIndex, iToIndex: Integer): Boolean;
Var
  iLoop : Integer;
Begin
  Result := Alterable(sMethod);

  ValidateIndex(sMethod, iFromIndex);
  ValidateIndex(sMethod, iToIndex);

  If iFromIndex > iToIndex Then
    Invariant(sMethod, StringFormat('Invalid range for deletion (%d..%d)', [iFromIndex, iToIndex]));

  For iLoop := iFromIndex To iToIndex Do
    Deleteable(sMethod, iLoop);
End;


Function TFslItemList.Insertable(Const sMethod: String; iIndex: Integer): Boolean;
Begin
  Result := Alterable(sMethod);

  If iIndex <> Count Then
    ValidateIndex(sMethod, iIndex);
End;


Function TFslItemList.Replaceable(Const sMethod: String; iIndex: Integer): Boolean;
Begin
  Result := Alterable(sMethod);

  If Not Replacable Then
    Invariant(sMethod, 'List does not allow replacing of items.');
End;


Function TFslItemList.Extendable(Const sMethod: String; iCount: Integer): Boolean;
Begin
  Result := Alterable(sMethod);

  If (iCount < 0) Or (iCount > CountLimit) Then
    Invariant(sMethod, StringFormat('Cannot set count to %d as it not compatible with the range [0..%d]', [iCount, CountLimit]));
End;


Procedure TFslItemList.Exchange(iA, iB: Integer);
Begin
  Assert(ValidateIndex('Exchange', iA));
  Assert(ValidateIndex('Exchange', iB));
  Assert(CheckCondition(iA <> iB, 'Exchange', 'Cannot exchange with the same index position.'));
  Assert(CheckCondition(IsUnsorted, 'Exchange', 'Cannot exchange in sorted items.'));
  Assert(Alterable('Exchange'));

  InternalExchange(iA, iB);
End;


Function TFslItemList.Replacable : Boolean;
Begin
  Result := True;
End;


Function TFslItemList.IsOrderedBy(Const Value: TFslItemListCompare): Boolean;
Var
  iIndex : Integer;
Begin
  Result := True;
  iIndex := 0;
  While (iIndex < Count - 1) And Result Do
  Begin
    Result := Value(ItemByIndex[iIndex], ItemByIndex[iIndex + 1]) * FDirection <= 0;
    Inc(iIndex);
  End;
End;


Procedure TFslItemList.OrderedBy(Const Value: TFslItemListCompare);
Begin
  FComparison := Value;
  FSorted := False;
  InternalSort;
End;


Procedure TFslItemList.Unordered;
Begin
  FComparison := Nil;
  FSorted := False;
End;


function TFslItemList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Procedure TFslPointerList.AssignItem(oItems : TFslItemList; iIndex : Integer);
Begin
  FPointerArray^[iIndex] := TFslPointerList(oItems).FPointerArray^[iIndex];
End;


Procedure TFslPointerList.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FPointerArray, Capacity * SizeOf(TPointerItem), iValue * SizeOf(TPointerItem));
End;


Procedure TFslPointerList.InternalEmpty(iIndex, iLength: Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FPointerArray) + NativeUInt(iIndex * SizeOf(TPointerItem))), (iLength * SizeOf(TPointerItem)));
End;


Procedure TFslPointerList.InternalCopy(iSource, iTarget, iCount: Integer);
Begin
  Inherited;

  MemoryMove(@FPointerArray^[iSource], @FPointerArray^[iTarget], iCount * SizeOf(TPointerItem));
End;


Function TFslPointerList.IndexByValue(pValue : TPointerItem): Integer;
Begin
  If Not Find(pValue, Result) Then
    Result := -1;
End;


Function TFslPointerList.ExistsByValue(pValue : TPointerItem): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(pValue));
End;


Function TFslPointerList.Add(pValue : TPointerItem): Integer;
Begin
  Result := -1;

  If Not IsAllowDuplicates And Find(pValue, Result) Then
  Begin
    If IsPreventDuplicates Then
      RaiseError('Add', StringFormat('Item already exists in list ($%x)', [Integer(pValue)]));
  End
  Else
  Begin
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(pValue, Result);

    Insert(Result, pValue);
  End;
End;


Procedure TFslPointerList.Insert(iIndex : Integer; pValue: TPointerItem);
Begin
  InternalInsert(iIndex);

  FPointerArray^[iIndex] := pValue;
End;


Procedure TFslPointerList.InternalInsert(iIndex : Integer);
Begin
  Inherited;

  FPointerArray^[iIndex] := Nil;
End;


Procedure TFslPointerList.DeleteByValue(pValue : TPointerItem);
Var
  iIndex : Integer;
Begin
  If Not Find(pValue, iIndex) Then
    RaiseError('DeleteByValue', 'Pointer not found in list');

  DeleteByIndex(iIndex);
End;


Function TFslPointerList.RemoveFirst : TPointerItem;
Begin
  If Count <= 0 Then
    Result := Nil
  Else
  Begin
    Result := FPointerArray^[0];
    DeleteByIndex(0);
  End;
End;


Function TFslPointerList.RemoveLast : TPointerItem;
Begin
  If Count <= 0 Then
    Result := Nil
  Else
  Begin
    Result := FPointerArray^[Count - 1];
    DeleteByIndex(Count - 1);
  End;
End;

Procedure TFslPointerList.InternalExchange(iA, iB : Integer);
Var
  iTemp : Integer;
  ptrA  : Pointer;
  ptrB  : Pointer;
Begin
  ptrA := @FPointerArray^[iA];
  ptrB := @FPointerArray^[iB];

  iTemp := Integer(ptrA^);
  Integer(ptrA^) := Integer(ptrB^);
  Integer(ptrB^) := iTemp;
End;


Function TFslPointerList.GetItem(iIndex: Integer): Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := FPointerArray^[iIndex];
End;


Procedure TFslPointerList.SetItem(iIndex : Integer; pValue: Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FPointerArray^[iIndex] := pValue;
End;


Function TFslPointerList.GetPointerByIndex(iIndex : Integer): TPointerItem;
Begin
  Assert(ValidateIndex('GetPointerByIndex', iIndex));

  Result := FPointerArray^[iIndex];
End;


Procedure TFslPointerList.SetPointerByIndex(iIndex : Integer; Const pValue : TPointerItem);
Begin
  Assert(ValidateIndex('SetPointerByIndex', iIndex));

  FPointerArray^[iIndex] := pValue;
End;


Function TFslPointerList.CapacityLimit : Integer;
Begin
  Result := High(TPointerItems);
End;


Function TFslPointerList.Iterator : TFslIterator;
Begin
  Result := TFslPointerListIterator.Create;
  TFslPointerListIterator(Result).Pointers := TFslPointerList(Self.Link);
End;

Constructor TFslPointerListIterator.Create;
Begin
  Inherited;

  FPointerArray := Nil;
End;


Destructor TFslPointerListIterator.Destroy;
Begin
  FPointerArray.Free;

  Inherited;
End;


Procedure TFslPointerListIterator.First;
Begin
  Inherited;

  FIndex := 0;
End;


Procedure TFslPointerListIterator.Last;
Begin
  Inherited;

  FIndex := FPointerArray.Count - 1;
End;


Procedure TFslPointerListIterator.Next;
Begin
  Inherited;

  Inc(FIndex);
End;


Procedure TFslPointerListIterator.Back;
Begin
  Inherited;

  Dec(FIndex);
End;


Function TFslPointerListIterator.Current : Pointer;
Begin
  Result := FPointerArray[FIndex];
End;


Function TFslPointerListIterator.More : Boolean;
Begin
  Result := FPointerArray.ExistsByIndex(FIndex);
End;


Procedure TFslPointerListIterator.SetPointers(Const Value: TFslPointerList);
Begin
  FPointerArray.Free;
  FPointerArray := Value;
End;



function TFslPointerListIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPointerArray.sizeInBytes);
end;

Procedure TFslHashEntry.Assign(oSource: TFslObject);
Begin
  Inherited;

  FCode := TFslHashEntry(oSource).Code;
End;



Function TFslHashEntry.Link : TFslHashEntry;
Begin
  Result := TFslHashEntry(Inherited Link);
End;


Function TFslHashEntry.Clone : TFslHashEntry;
Begin
  Result := TFslHashEntry(Inherited Clone);
End;


Procedure TFslHashEntry.Generate;
Begin
End;


Constructor TFslHashTable.Create;
Begin
  Inherited;

  Balance := 0.85;
End;


Destructor TFslHashTable.Destroy;
Begin
  MemoryDestroy(FTable, FCapacity * SizeOf(TFslHashEntry));

  Inherited;
End;


Function TFslHashTable.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslHashTable;
End;


Function TFslHashTable.Link : TFslHashTable;
Begin
  Result := TFslHashTable(Inherited Link);
End;


Function TFslHashTable.Clone : TFslHashTable;
Begin
  Result := TFslHashTable(Inherited Clone);
End;


Procedure TFslHashTable.Assign(oObject : TFslObject);
Var
  oIterator : TFslHashTableIterator;
Begin
  Inherited;

  Clear;

  Capacity := TFslHashTable(oObject).Capacity;
  Balance := TFslHashTable(oObject).Balance;

  // TODO: Implement without iterator for optimal algorithm.

  oIterator := TFslHashTableIterator(Iterator);
  Try
    oIterator.First;
    While oIterator.More Do
    Begin
      Add(Duplicate(TFslHashEntry(oIterator.Current)));

      oIterator.Next;
    End;
  Finally
    oIterator.Free;
  End;
End;


Function TFslHashTable.ItemClass : TFslHashEntryClass;
Begin
  Result := TFslHashEntry;
End;


Function TFslHashTable.ItemNew : TFslHashEntry;
Begin
  Result := ItemClass.Create;
End;


Procedure TFslHashTable.InternalClear;
Var
  iLoop : Integer;
  oHashEntry : TFslHashEntry;
  oNext : TFslHashEntry;
Begin
  Inherited;

  If FCapacity > 0 Then
  Begin
    For iLoop := 0 To FCapacity - 1 Do
    Begin
      oHashEntry := FTable^[iLoop];
      FTable^[iLoop] := Nil;

      While Assigned(oHashEntry) Do
      Begin
        Assert(Invariants('Clear', oHashEntry, ItemClass, 'oHashEntry'));

        oNext := oHashEntry.FNextHashEntry;
        oHashEntry.Free;
        oHashEntry := oNext;
      End;
    End;
  End;

  FCount := 0;
End;


Function TFslHashTable.Find(oSource, oHashEntry : TFslHashEntry): TFslHashEntry;
Begin
  Assert(Not Assigned(oSource) Or Invariants('Find', oSource, ItemClass, 'oSource'));
  Assert(Invariants('Find', oHashEntry, ItemClass, 'oHashEntry'));

  Result := oSource;
  While Assigned(Result) And (Equal(Result, oHashEntry) <> 0) Do
    Result := Result.FNextHashEntry;
End;


Function TFslHashTable.Equal(oA, oB: TFslHashEntry): Integer;
Begin
  Result := IntegerCompare(oA.Code, oB.Code);
End;


Function TFslHashTable.Resolve(oHashEntry: TFslHashEntry): Cardinal;
Begin
  Assert(CheckCondition(FCapacity > 0, 'Resolve', 'Capacity must be greater than zero'));

  Result := UnsignedMod(oHashEntry.Code, FCapacity);
End;


Procedure TFslHashTable.Rehash;
Var
  pTable : PFslHashEntryArray;
  oHashEntry : TFslHashEntry;
  oNext : TFslHashEntry;
  iCapacity : Integer;
  iLoop : Integer;
Begin
  Assert(CheckCondition(Not FPreventRehash, 'Rehash', 'Rehash has been prevented on this hash table as you are required to set Capacity more appropriately in advance.'));

  pTable := FTable;
  FTable := Nil;
  iCapacity := FCapacity;
  FCapacity := 0;

  FCount := 0;
  Try
    Try
      Capacity := (iCapacity * 2) + 1;
    Except
      // Revert to hash table before out of memory exception.
      If ExceptObject.ClassType = EOutOfMemory Then
      Begin
        FTable := pTable;
        FCapacity := iCapacity;

        pTable := Nil;
        iCapacity := 0;
      End;

      Raise;
    End;

    For iLoop := 0 To iCapacity - 1 Do
    Begin
      oHashEntry := pTable^[iLoop];

      While Assigned(oHashEntry) Do
      Begin
        oNext := oHashEntry.FNextHashEntry;

        Insert(Resolve(oHashEntry), oHashEntry);

        oHashEntry := oNext;
      End;
    End;
  Finally
    MemoryDestroy(pTable, iCapacity * SizeOf(TFslHashEntry));
  End;
End;


Procedure TFslHashTable.Add(oHashEntry : TFslHashEntry);
Begin
  Assert(Invariants('Add', oHashEntry, ItemClass, 'oHashEntry'));

  Try
    Assert(CheckCondition(Not Exists(oHashEntry), 'Add', 'Object already exists in the hash.'));

    If FCount > FThreshold Then
      Rehash;

    Insert(Resolve(oHashEntry), oHashEntry.Link);
  Finally
    oHashEntry.Free;
  End;
End;


Procedure TFslHashTable.Insert(iIndex : Integer; oHashEntry : TFslHashEntry);
Var
  pFirst : PFslHashEntry;
Begin
  Assert(Invariants('Insert', oHashEntry, TFslHashEntry, 'oHashEntry'));
  Assert(CheckCondition((iIndex >= 0) And (iIndex < FCapacity), 'Insert', 'Index must be within the hash table'));

  pFirst := @FTable^[iIndex];

  oHashEntry.FNextHashEntry := pFirst^;
  pFirst^ := oHashEntry;

  Inc(FCount);
End;


Function TFslHashTable.Force(oHashEntry: TFslHashEntry): TFslHashEntry;
Var
  iIndex : Integer;
Begin
  Assert(Invariants('Force', oHashEntry, ItemClass, 'oHashEntry'));

  If FCount > FThreshold Then
    Rehash;

  iIndex := Resolve(oHashEntry);

  Result := Find(FTable^[iIndex], oHashEntry);

  If Not Assigned(Result) Then
  Begin
    Result := Duplicate(oHashEntry);

    Insert(iIndex, Result);
  End;
End;


Function TFslHashTable.Replace(oHashEntry : TFslHashEntry) : TFslHashEntry;
Var
  iIndex : Integer;
Begin
  Assert(Invariants('Replace', oHashEntry, ItemClass, 'oHashEntry'));

  If FCount > FThreshold Then
    Rehash;

  iIndex := Resolve(oHashEntry);

  Result := Find(FTable^[iIndex], oHashEntry);

  If Assigned(Result) Then
  Begin
    Result.Assign(oHashEntry);
  End
  Else
  Begin
    Result := Duplicate(oHashEntry);

    Insert(iIndex, Result);
  End;
End;


Function TFslHashTable.Delete(oHashEntry: TFslHashEntry) : Boolean;
Var
  oLast  : TFslHashEntry;
  oNext  : TFslHashEntry;
  pFirst : PFslHashEntry;
Begin
  Assert(Invariants('Delete', oHashEntry, ItemClass, 'oHashEntry'));

  pFirst := @(FTable^[Resolve(oHashEntry)]);

  Result := Assigned(pFirst^);

  If Result Then
  Begin
    oLast := pFirst^;

    Assert(Invariants('Delete', oLast, ItemClass, 'oLast'));

    If (Equal(oLast, oHashEntry) = 0) Then
    Begin
      pFirst^ := oLast.FNextHashEntry;
      oLast.Free;
    End
    Else
    Begin
      oNext := oLast.FNextHashEntry;
      While Assigned(oNext) And (Equal(oNext, oHashEntry) <> 0) Do
      Begin
        oLast := oNext;
        oNext := oLast.FNextHashEntry;
      End;

      Result := Assigned(oNext);

      If Result Then
      Begin
        oLast.FNextHashEntry := oNext.FNextHashEntry;
        oNext.Free;
      End
    End;

    If Result Then
      Dec(FCount);
  End;
End;


Function TFslHashTable.Get(oHashEntry : TFslHashEntry) : TFslHashEntry;
Begin
  Assert(Invariants('Get', oHashEntry, ItemClass, 'oHashEntry'));

  // Returns the hash entry in the hash table matching the parameter.
  // If there is no matching object in the hash table, Nil is returned.

  Result := Find(FTable^[Resolve(oHashEntry)], oHashEntry);
End;


Function TFslHashTable.Duplicate(oHashEntry: TFslHashEntry): TFslHashEntry;
Begin
  Assert(Invariants('Duplicate', oHashEntry, ItemClass, 'oHashEntry'));

  Result := TFslHashEntry(oHashEntry.Clone);
End;


Function TFslHashTable.Exists(oHashEntry: TFslHashEntry): Boolean;
Begin
  Assert(Invariants('Exists', oHashEntry, ItemClass, 'oHashEntry'));

  Result := Assigned(Get(oHashEntry));
End;


Function TFslHashTable.Iterator : TFslIterator;
Begin
  Result := TFslHashTableIterator.Create;
  TFslHashTableIterator(Result).HashTable := Self.Link;
End;


Procedure TFslHashTable.SetCapacity(Const Value: Integer);
Begin

  If Value <> FCapacity Then
  Begin
    Assert(CheckCondition(FCount = 0, 'SetCapacity', StringFormat('Unable to change capacity to %d when there are entries in the hash table', [Value])));

    MemoryResize(FTable, FCapacity * SizeOf(TFslHashEntry), Value * SizeOf(TFslHashEntry));

    If Value > FCapacity Then
      MemoryZero(Pointer(NativeUInt(FTable) + NativeUInt(FCapacity * SizeOf(TFslHashEntry))), (Value - FCapacity) * SizeOf(TFslHashEntry));

    FCapacity := Value;
    FThreshold := Trunc(FCapacity * FBalance);
  End;
End;


Procedure TFslHashTable.SetBalance(Const Value: Real);
Begin
  Assert(CheckCondition((Value > 0.0) And (Value < 1.0), 'SetBalance', 'Balance must be set to valid positive percentage.'));

  FBalance := Value;
  FThreshold := Trunc(FCapacity * FBalance);
End;


Function TFslHashTableList.GetHash(iIndex: Integer): TFslHashTable;
Begin
  Result := TFslHashTable(ObjectByIndex[iIndex]);
End;


Function TFslHashTableList.ItemClass : TFslObjectClass;
Begin
  Result := TFslHashTable;
End;


Constructor TFslHashTableIterator.Create;
Begin
  Inherited;

  FHashTable := Nil;
End;


Destructor TFslHashTableIterator.Destroy;
Begin
  FHashTable.Free;

  Inherited;
End;


Procedure TFslHashTableIterator.First;
Begin
  FIndex := 0;
  FCurrentHashEntry := Nil;
  FNextHashEntry := Nil;

  If FHashTable.Count > 0 Then
    Next;
End;


Function TFslHashTableIterator.Count : Integer;
Begin
  Result := FHashTable.Count;
End;


Function TFslHashTableIterator.Current : TFslObject;
Begin
  Assert(Invariants('Current', FCurrentHashEntry, FHashTable.ItemClass, 'FCurrentHashEntry'));

  Result := FCurrentHashEntry;
End;


Function TFslHashTableIterator.More : Boolean;
Begin
  Result := Assigned(FCurrentHashEntry);
End;


Procedure TFslHashTableIterator.Next;
Begin
  FCurrentHashEntry := FNextHashEntry;

  While Not Assigned(FCurrentHashEntry) And (FIndex < FHashTable.Capacity) Do
  Begin
    FCurrentHashEntry := FHashTable.FTable^[FIndex];
    Inc(FIndex);
  End;

  If Assigned(FCurrentHashEntry) Then
    FNextHashEntry := FCurrentHashEntry.FNextHashEntry
  Else
    FNextHashEntry := Nil;
End;


Function TFslHashTableIterator.GetHashTable: TFslHashTable;
Begin
  Result := FHashTable;
End;


Procedure TFslHashTableIterator.SetHashTable(Const Value: TFslHashTable);
Begin
  FHashTable.Free;
  FHashTable := Value;
End;


function TFslHashTableIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FHashTable.sizeInBytes);
  inc(result, FCurrentHashEntry.sizeInBytes);
  inc(result, FNextHashEntry.sizeInBytes);
end;

Function TFslHashTable.ProduceHashEntry: TFslHashEntry;
Begin
  Result := ItemNew;
End;


Procedure TFslHashTable.ConsumeHashEntry(oHashEntry : TFslHashEntry);
Begin
  oHashEntry.Free;
End;


Procedure TFslHashTable.PredictCapacityByExpectedCount(Const iCount: Integer);
Begin
  Capacity := RealCeiling(iCount / Balance) + 1;

  Assert(CheckCondition(FThreshold >= iCount, 'PredictCapacityByExpectedCount', StringFormat('Threshold %d was expected to be the same as the expected count %d.', [FThreshold, iCount])));
End;


Procedure TFslHashTable.AllowRehash;
Begin
  FPreventRehash := False;
End;


Procedure TFslHashTable.PreventRehash;
Begin
  FPreventRehash := True;
End;


Function TFslHashTable.IsEmpty: Boolean;
Begin
  Result := Count = 0;
End;


Function TFslIterator.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslIterator;
End;


Procedure TFslIterator.First;
Begin
End;


Procedure TFslIterator.Last;
Begin
End;


Function TFslIterator.More : Boolean;
Begin
  Result := False;
End;


Procedure TFslIterator.Next;
Begin
End;


Procedure TFslIterator.Back;
Begin
End;


Procedure TFslIterator.Previous;
Begin
  Back;
End;


Function TFslObjectIterator.Current : TFslObject;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := Nil;
End;


Function TFslStringIterator.Current : String;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := '';
End;


Function TFslIntegerIterator.Current : Integer;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TFslRealIterator.Current : Real;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TFslExtendedIterator.Current : Extended;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TFslBooleanIterator.Current : Boolean;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := False;
End;


Function TFslLargeIntegerIterator.Current : Int64;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TFslPointerIterator.Current : Pointer;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := Nil;
End;


Function TFslObjectClassIterator.Current : TClass;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := Nil;
End;


Function TFslDateTimeIterator.Current : TDateTime;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TFslDurationIterator.Current : TDurationMS;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TFslCurrencyIterator.Current : TCurrency;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;



Function TFslName.Link : TFslName;
Begin
  Result := TFslName(Inherited Link);
End;


Function TFslName.Clone : TFslName;
Begin
  Result := TFslName(Inherited Clone);
End;


Procedure TFslName.Assign(oSource : TFslObject);
Begin
  Inherited;

  FName := TFslName(oSource).Name;
End;



Function TFslName.GetName: String;
Begin
  Result := FName;
End;


Procedure TFslName.SetName(Const Value: String);
Begin
  FName := Value;
End;


function TFslName.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

Constructor TFslNameList.Create;
Begin
  Inherited;

  FSymbol := cReturn;
End;


Function TFslNameList.Link : TFslNameList;
Begin
  Result := TFslNameList(Inherited Link);
End;


Function TFslNameList.Clone : TFslNameList;
Begin
  Result := TFslNameList(Inherited Clone);
End;


Function TFslNameList.ItemClass : TFslObjectClass;
Begin
  Result := TFslName;
End;


Procedure TFslNameList.DefaultCompare(Out aEvent: TFslItemListCompare);
Begin
  aEvent := CompareByName;
End;


Function TFslNameList.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TFslName(pA).Name, TFslName(pB).Name);
End;


Function TFslNameList.FindByName(oName: TFslName; Out iIndex: Integer): Boolean;
Begin
  Result := Find(oName, iIndex, CompareByName);
End;


Function TFslNameList.FindByName(Const sName : String; Out iIndex : Integer) : Boolean;
Var
  oName : TFslName;
Begin
  oName := TFslName(ItemNew);
  Try
    oName.Name := sName;

    Result := FindByName(oName, iIndex);
  Finally
    oName.Free;
  End;
End;


Function TFslNameList.EnsureByName(Const sName : String) : TFslName;
Begin
  Result := GetByName(sName);

  Assert(Invariants('EnsureByName', Result, ItemClass, 'Result'));
End;


Function TFslNameList.GetByName(Const sName: String): TFslName;
Var
  iIndex : Integer;
Begin
  If FindByName(sName, iIndex) Then
    Result := Names[iIndex]
  Else
    Result := Nil;
End;


Function TFslNameList.IndexByName(Const sName : String) : Integer;
Begin
  If Not FindByName(sName, Result) Then
    Result := -1;
End;


Function TFslNameList.ExistsByName(Const sName: String): Boolean;
Begin
  Result := ExistsByIndex(IndexByName(sName));
End;


Function TFslNameList.IndexByName(Const oName : TFslName) : Integer;
Begin
  If Not FindByName(oName, Result) Then
    Result := -1;
End;


Function TFslNameList.ExistsByName(Const oName : TFslName): Boolean;
Begin
  Result := ExistsByIndex(IndexByName(oName));
End;


Function TFslNameList.ForceByName(Const sName: String): TFslName;
Var
  oName  : TFslName;
  iIndex : Integer;
Begin
  oName := TFslName(ItemNew);
  Try
    oName.Name := sName;

    If FindByName(oName, iIndex) Then
      Result := Names[iIndex]
    Else
    Begin
      Insert(iIndex, oName.Link);
      Result := oName;
    End;
  Finally
    oName.Free;
  End;
End;


Function TFslNameList.GetByName(oName : TFslName): TFslName;
Var
  iIndex : Integer;
Begin
  If FindByName(oName, iIndex) Then
    Result := Names[iIndex]
  Else
    Result := Nil;
End;


Procedure TFslNameList.RemoveByName(Const sName: String);
Var
  iIndex : Integer;
Begin
  If Not FindByName(sName, iIndex) Then
    RaiseError('RemoveByName', StringFormat('Object ''%s'' not found in list.', [sName]));

  DeleteByIndex(iIndex);
End;


Function TFslNameList.AddByName(Const sName: String): Integer;
Var
  oItem : TFslName;
Begin
  oItem := TFslName(ItemNew);
  Try
    oItem.Name := sName;

    Result := Add(oItem.Link);
  Finally
    oItem.Free;
  End;
End;


Function TFslNameList.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;


Procedure TFslNameList.SortedByName;
Begin
  SortedBy(CompareByName);
End;


Function TFslNameList.GetName(iIndex : Integer) : TFslName;
Begin
  Result := TFslName(ObjectByIndex[iIndex]);
End;


Procedure TFslNameList.SetName(iIndex : Integer; oName : TFslName);
Begin
  ObjectByIndex[iIndex] := oName;
End;


Function TFslNameList.GetAsText : String;
Var
  oStrings : TFslStringList;
  iLoop    : Integer;
Begin
  oStrings := TFslStringList.Create;
  Try
    oStrings.Symbol := FSymbol;

    For iLoop := 0 To Count - 1 Do
      oStrings.Add(Names[iLoop].Name);

    Result := oStrings.AsText;
  Finally
    oStrings.Free;
  End;
End;


Procedure TFslNameList.SetAsText(Const Value: String);
Var
  oStrings : TFslStringList;
  iLoop    : Integer;
  oItem    : TFslName;
Begin
  Clear;

  oStrings := TFslStringList.Create;
  Try
    oStrings.Symbol := FSymbol;

    oStrings.AsText := Value;

    For iLoop := 0 To oStrings.Count - 1 Do
    Begin
      oItem := TFslName(ItemNew);
      Try
        oItem.Name := StringTrimWhitespace(oStrings[iLoop]);

        Add(oItem.Link);
      Finally
        oItem.Free;
      End;
    End;
  Finally
    oStrings.Free;
  End;
End;


function TFslNameList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSymbol.length * sizeof(char)) + 12);
end;

Procedure TFslCharacter.Assign(oObject : TFslObject);
Begin
  Inherited;

  FValue := TFslCharacter(oObject).Value;
End;



Function TFslCharacterList.ItemClass : TFslObjectClass;
Begin
  Result := TFslCharacter;
End;


Function TFslCharacterList.CompareByValue(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TFslCharacter(pA).Value, TFslCharacter(pB).Value);
End;


Procedure TFslCharacterList.SortByValue;
Begin
  SortedBy(CompareByValue);
End;


Function TFslCharacterList.ExistsByValue(Const cValue: Char): Boolean;
Var
  oCharacter : TFslCharacter;
Begin
  oCharacter := TFslCharacter(ItemNew);
  Try
    oCharacter.Value := cValue;

    Result := ExistsBy(oCharacter, CompareByValue);
  Finally
    oCharacter.Free;
  End;
End;


Procedure TFslCharacterList.AddCharacter(Const cValue : Char);
Var
  oCharacter : TFslCharacter;
Begin
  oCharacter := TFslCharacter(ItemNew);
  Try
    oCharacter.Value := cValue;

    Add(oCharacter.Link);
  Finally
    oCharacter.Free;
  End;
End;


Procedure TFslCharacterList.AddCharacterSet(Const aValueSet : TCharSet);
Var
  aSetValue : Char;
Begin
  For aSetValue := Low(Char) To High(Char) Do
  Begin
    If CharInSet(aSetValue, aValueSet) Then
      AddCharacter(aSetValue);
  End;
End;


Function TFslCharacterList.GetCharacterByIndex(Const iIndex : Integer) : TFslCharacter;
Begin
  Result := TFslCharacter(ObjectByIndex[iIndex]);
End;


Constructor TFslObjectChoice.Create;
Begin
  Inherited;

  FObject := Nil;
End;


Destructor TFslObjectChoice.Destroy;
Begin
  FObject.Free;

  Inherited;
End;


Function TFslObjectChoice.RetrieveIsObjectClass(Const aObjectClass: TFslObjectClass): Boolean;
Begin
  Assert(Invariants('RetrieveIsObjectClass', aObjectClass, TFslObject, 'aObjectClass'));

  Result := Assigned(FObject);

  If Result Then
  Begin
    Assert(Invariants('RetrieveIsObjectClass', FObject, TFslObject, 'FObject'));

    Result := (FObject.ClassType = aObjectClass);
  End;
End;


Procedure TFslObjectChoice.StoreIsObjectClass(Const aObjectClass: TFslObjectClass; Const bStored: Boolean);
Begin
  If bStored Then
  Begin
    If Not Assigned(FObject) Or (FObject.ClassType <> aObjectClass) Then
    Begin
      FObject.Free;
      FObject := Nil;
      FObject := aObjectClass.Create;
    End;
  End
  Else
  Begin
  {$IFOPT C+}
    If Not Assigned(FObject) Then
      Invariant('StoreIsObjectClass', StringFormat('Cannot unstore ''%s'' as choice is null.', [aObjectClass.ClassName]));

    If FObject.ClassType <> aObjectClass Then
      Invariant('StoreIsObjectClass', StringFormat('Cannot unstore ''%s'' as choice is ''%s''.', [aObjectClass.ClassName, FObject.ClassName]));
  {$ENDIF}

    FObject.Free;
    FObject := Nil;
  End;
End;


Function TFslObjectChoice.RetrieveObject(Const aObjectClass: TFslObjectClass): TFslObject;
Begin
  Assert(Invariants('RetrieveObject', aObjectClass, TFslObject, 'aObjectClass'));
  Assert(Invariants('RetrieveObject', FObject, aObjectClass, 'FObject'));

  Result := FObject;
End;


Procedure TFslObjectChoice.StoreObject(Const oObject : TFslObject; Const aObjectClass : TFslObjectClass);
Begin
  Assert(Invariants('RetrieveObject', aObjectClass, TFslObject, 'aObjectClass'));
  Assert(Not Assigned(oObject) Or Invariants('StoreObject', oObject, aObjectClass, 'oObject'));

  FObject.Free;
  FObject := oObject;
End;


Function TFslObjectChoice.RetrieveIsNull: Boolean;
Begin
  Result := Not Assigned(FObject);
End;


Procedure TFslObjectChoice.StoreIsNull;
Begin
  Assert(Not Assigned(FObject) Or Invariants('StoreIsNull', FObject, TFslObject, 'FObject'));

  FObject.Free;
  FObject := Nil;
End;



function TFslObjectChoice.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FObject.sizeInBytes);
end;

Procedure TFslBooleanList.AssignItem(oItems : TFslItemList; iIndex : Integer);
Begin
  Inherited;

  FBooleanArray^[iIndex] := TFslBooleanList(oItems).FBooleanArray^[iIndex];
End;


Procedure TFslBooleanList.InternalEmpty(iIndex, iLength : Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeInt(FBooleanArray) + (iIndex * SizeOf(TFslBooleanItem))), (iLength * SizeOf(TFslBooleanItem)));
End;


Procedure TFslBooleanList.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FBooleanArray, Capacity * SizeOf(TFslBooleanItem), iValue * SizeOf(TFslBooleanItem));
End;


Procedure TFslBooleanList.InternalCopy(iSource, iTarget, iCount : Integer);
Begin
  Inherited;

  MemoryMove(@FBooleanArray^[iSource], @FBooleanArray^[iTarget], iCount * SizeOf(TFslBooleanItem));
End;


Function TFslBooleanList.IndexByValue(aValue : TFslBooleanItem): Integer;
Begin
  If Not Find(Pointer(aValue), Result) Then
    Result := -1;
End;


Function TFslBooleanList.ExistsByValue(aValue : TFslBooleanItem): Boolean;
Begin
  Result := IndexByValue(aValue) >= 0;
End;


Function TFslBooleanList.Add(aValue : TFslBooleanItem): Integer;
Begin
  Result := -1;

  If Not IsAllowDuplicates And Find(Pointer(aValue), Result) Then
  Begin
    If IsPreventDuplicates Then
      RaiseError('Add', StringFormat('Item already exists in list (%s)', [BooleanToString(aValue)]));
  End
  Else
  Begin
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(Pointer(aValue), Result);

    Insert(Result, aValue);
  End;
End;


Procedure TFslBooleanList.Insert(iIndex : Integer; aValue : TFslBooleanItem);
Begin
  InternalInsert(iIndex);

  FBooleanArray^[iIndex] := aValue;
End;


Procedure TFslBooleanList.DeleteByValue(aValue : TFslBooleanItem);
Var
  iIndex : Integer;
Begin
  If Not Find(@aValue, iIndex) Then
    RaiseError('DeleteByValue', StringFormat('''%s'' not found in list', [BooleanToString(aValue)]));

  DeleteByIndex(iIndex);
End;


Procedure TFslBooleanList.InternalExchange(iA, iB : Integer);
Var
  iTemp : TFslBooleanItem;
  pA    : Pointer;
  pB    : Pointer;
Begin
  pA := @FBooleanArray^[iA];
  pB := @FBooleanArray^[iB];

  iTemp := TFslBooleanItem(pA^);
  TFslBooleanItem(pA^) := TFslBooleanItem(pB^);
  TFslBooleanItem(pB^) := iTemp;
End;


Procedure TFslBooleanList.Add(oBooleans: TFslBooleanList);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oBooleans.Count - 1 Do
    Add(oBooleans[iLoop]);
End;


Procedure TFslBooleanList.Invert;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    FBooleanArray^[iLoop] := Not FBooleanArray^[iLoop];
End;


Procedure TFslBooleanList.Select(bValue: Boolean);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    FBooleanArray^[iLoop] := bValue;
End;


Function TFslBooleanList.CountOf(bValue: Boolean): Integer;
Var
  iLoop : Integer;
Begin
  Result := 0;
  For iLoop := 0 To Count - 1 Do
  Begin
    If FBooleanArray^[iLoop] = bValue Then
      Inc(Result);
  End;
End;


Function TFslBooleanList.GetItem(iIndex : Integer): Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := Pointer(FBooleanArray^[iIndex]);
End;


Procedure TFslBooleanList.SetItem(iIndex : Integer; pValue : Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FBooleanArray^[iIndex] := TFslBooleanItem(pValue);
End;


Function TFslBooleanList.GetBooleanByIndex(iIndex : Integer): TFslBooleanItem;
Begin
  Assert(ValidateIndex('GetBooleanByIndex', iIndex));

  Result := FBooleanArray^[iIndex];
End;


Procedure TFslBooleanList.SetBooleanByIndex(iIndex : Integer; Const iValue : TFslBooleanItem);
Begin
  Assert(ValidateIndex('SetBooleanByIndex', iIndex));

  FBooleanArray^[iIndex] := iValue;
End;


Function TFslBooleanList.CapacityLimit : Integer;
Begin
  Result := High(TFslBooleanItemArray);
End;


Function TFslBooleanList.Iterator : TFslIterator;
Begin
  Result := TFslBooleanListIterator.Create;
  TFslBooleanListIterator(Result).BooleanList := TFslBooleanList(Self.Link);
End;


Constructor TFslBooleanListIterator.Create;
Begin
  Inherited;

  FBooleanList := Nil;
End;


Destructor TFslBooleanListIterator.Destroy;
Begin
  FBooleanList.Free;

  Inherited;
End;


Procedure TFslBooleanListIterator.First;
Begin
  Inherited;

  FIndex := 0;
End;


Procedure TFslBooleanListIterator.Last;
Begin
  Inherited;

  FIndex := FBooleanList.Count - 1;
End;


Procedure TFslBooleanListIterator.Next;
Begin
  Inherited;

  Inc(FIndex);
End;


Procedure TFslBooleanListIterator.Back;
Begin
  Inherited;

  Dec(FIndex);
End;


Function TFslBooleanListIterator.Current : Boolean;
Begin
  Result := FBooleanList[FIndex];
End;


Function TFslBooleanListIterator.More : Boolean;
Begin
  Result := FBooleanList.ExistsByIndex(FIndex);
End;


Function TFslBooleanListIterator.GetBooleanList: TFslBooleanList;
Begin
  Assert(Invariants('GetBooleans', FBooleanList, TFslBooleanList, 'FBooleanList'));

  Result := FBooleanList;
End;


Procedure TFslBooleanListIterator.SetBooleanList(Const Value: TFslBooleanList);
Begin
  FBooleanList.Free;
  FBooleanList := Value;
End;

function TFslBooleanListIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBooleanList.sizeInBytes);
end;

Constructor TFslCharacterSet.Create;
Begin
  Inherited;

  Owns := False;
  Parts := @FDataSet;
  Size := SizeOf(FDataSet);
End;


Destructor TFslCharacterSet.Destroy;
Begin
  Inherited;
End;


Procedure TFslCharacterSet.AddRange(Const aFromValue, aToValue: Char);
Begin
  FDataSet := FDataSet + [aFromValue..aToValue];
End;


Procedure TFslCharacterSet.AddValue(Const aValue: Char);
Begin
  FDataSet := FDataSet + [aValue];
End;


Function TFslCharacterSet.ContainsValue(Const aValue: Char): Boolean;
Begin
  Result := CharInSet(aValue, FDataSet);
End;


Function TFslCharacterSet.GetAsText : String;
Var
  iLoop : Integer;
  iStart : Integer;
Begin
  iLoop := 0;
  Result := '';

  While (iLoop < Count) Do
  Begin
    iStart := iLoop;
    While (iLoop < Count) And Checked(iLoop) Do
      Inc(iLoop);

    If iLoop = iStart + 1 Then
      StringAppend(Result, Char(iStart), ', ')
    Else If iLoop > iStart + 1 Then
      StringAppend(Result, Char(iStart) + '-' + Char(iLoop - 1), ', ');

    Inc(iLoop);
  End;
End;


Procedure TFslCharacterSet.SetAsText(Const Value: String);
Var
  oStrings : TFslStringList;
  iLoop    : Integer;
  sField   : String;
  sLeft    : String;
  sRight   : String;
Begin
  Fill(False);

  oStrings := TFslStringList.Create;
  Try
    oStrings.Symbol := ',';

    oStrings.AsText := Value;

    For iLoop := 0 To oStrings.Count - 1 Do
    Begin
      sField := StringTrimWhitespace(oStrings[iLoop]);

      If sField <> '' Then
      Begin
        If Length(sField) = 1 Then
          Check(Ord(sField[1]))
        Else If StringSplit(sField, '-', sLeft, sRight) And (Length(sLeft) = 1) And (Length(sRight) = 1) Then
          CheckRange(Ord(sLeft[1]), Ord(sRight[1]));
      End;
    End;
  Finally
    oStrings.Free;
  End;
End;



Constructor TFslTree.Create;
Begin
  Inherited;

  FChildren := ChildrenNew;

  Reparent;
End;


Destructor TFslTree.Destroy;
Begin
  FChildren.Free;

  Inherited;
End;


Function TFslTree.Clone: TFslTree;
Begin
  Result := TFslTree(Inherited Clone);
End;


Function TFslTree.Link: TFslTree;
Begin
  Result := TFslTree(Inherited Link);
End;


Function TFslTree.ClassType : TFslTreeClass;
Begin
  Result := TFslTreeClass(Inherited ClassType);
End;


Procedure TFslTree.Assign(oObject: TFslObject);
Begin
  Inherited;

  FChildren.Assign(TFslTree(oObject).Children);

  Reparent;
End;


Function TFslTree.ParentClass: TFslTreeClass;
Begin
  // Cannot be ClassType as may be owned by a different type.

  Result := TFslTree;
End;


Function TFslTree.ChildrenClass: TFslTreeListClass;
Begin
  Result := TFslTreeList;
End;


Function TFslTree.ChildrenNew: TFslTreeList;
Begin
  Result := ChildrenClass.Create;
End;


Function TFslTree.HasChildren: Boolean;
Begin
  Result := Children.Count > 0;
End;


Function TFslTree.HasParent: Boolean;
Begin
  Result := Assigned(FParent);
End;


Procedure TFslTree.Reparent;
Var
  iLoop : Integer;
Begin
  Assert(Invariants('Reparent', FChildren, ChildrenClass, 'FChildren'));

  FChildren.FParent := Self;
  For iLoop := 0 To FChildren.Count - 1 Do
    FChildren[iLoop].Parent := Self;
End;


Function TFslTree.GetChildren: TFslTreeList;
Begin
  Assert(Invariants('GetChildren', FChildren, ChildrenClass, 'FChildren'));

  Result := FChildren;
End;


Procedure TFslTree.SetChildren(Const Value: TFslTreeList);
Begin
  Assert(Invariants('SetChildren', Value, ChildrenClass, 'Value'));

  FChildren.Free;
  FChildren := Value;

  Reparent;
End;


Function TFslTree.GetParent: TFslTree;
Begin
  Assert(Invariants('GetParent', FParent, ParentClass, 'FParent'));

  Result := FParent;
End;


Procedure TFslTree.SetParent(Const Value: TFslTree);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetParent', Value, ParentClass, 'Value'));
  Assert(CheckCondition(Not Assigned(FParent) Or (Value = FParent), 'SetParent', 'Cannot set parent as already owned by another tree.'));

  FParent := Value;
End;


function TFslTree.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FChildren.sizeInBytes);
end;

Function TFslTreeList.GetTree(iIndex: Integer): TFslTree;
Begin
  Result := TFslTree(ObjectByIndex[iIndex]);
End;


Procedure TFslTreeList.InternalBeforeExclude(iIndex : Integer; oObject : TFslObject);
Var
  oTree : TFslTree;
Begin
  Inherited;

  oTree := TFslTree(oObject);
  oTree.FParent := Nil;
End;


Procedure TFslTreeList.InternalAfterInclude(iIndex : Integer; oObject : TFslObject);
Var
  oTree : TFslTree;
Begin
  Inherited;

  oTree := TFslTree(oObject);

  Assert(CheckCondition(Not oTree.HasParent, 'InternalAfterInclude', 'Attempt to add a node to a tree when it already belongs to a tree'));

  oTree.FParent := FParent;
End;


Function TFslTreeList.ItemClass: TFslObjectClass;
Begin
  Result := TFslTree;
End;



End.



