Unit FHIR.Support.Collections;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

Interface


Uses
  SysUtils,
  FHIR.Support.Math, FHIR.Support.Strings, FHIR.Support.System,
  FHIR.Support.Exceptions, FHIR.Support.Objects, FHIR.Support.Filers;


Type
  TAdvIterator = Class(TAdvPersistent)
    Protected
      Function ErrorClass : EAdvExceptionClass; Overload; Override;

    Public
      Procedure First; Virtual;
      Procedure Last; Virtual;
      Procedure Next; Virtual;
      Procedure Back; Virtual;
      Procedure Previous; Virtual;

      Function More : Boolean; Virtual;
  End;

  TAdvIteratorClass = Class Of TAdvIterator;

  TAdvCollection = Class(TAdvPersistent)
    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

      Procedure InternalClear; Virtual;

    Public
      Procedure BeforeDestruction; Override;

      Procedure Clear; Virtual;

      Function Iterator : TAdvIterator; Overload; Virtual;
  End;

  EAdvCollection = Class(EAdvException);

  TAdvItemListCompare = Function (pA, pB : Pointer) : Integer Of Object;
  PAdvItemsCompare = ^TAdvItemListCompare;

  TAdvItemListDuplicates = (dupAccept, dupIgnore, dupException);

  TAdvItemListDirection = Integer;

  TAdvItemList = Class(TAdvCollection)
    Private
      FCount : Integer;
      FCapacity : Integer;
      FSorted : Boolean;
      FComparison : TAdvItemListCompare;
      FDuplicates : TAdvItemListDuplicates;
      FDirection : TAdvItemListDirection;

    Protected
      Function ErrorClass : EAdvExceptionClass; Overload; Override;

      Function ValidateIndex(Const sMethod : String; iIndex : Integer) : Boolean; Virtual;

      Procedure SetCapacity(Const iValue: Integer); Virtual;
      Procedure SetCount(Const iValue : Integer); Virtual;

      Function GetItem(iIndex : Integer) : Pointer; Virtual; Abstract;
      Procedure SetItem(iIndex : Integer; pValue : Pointer); Virtual; Abstract;

      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Virtual;
      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Virtual;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Virtual;

      Function CompareItem(pA, pB : Pointer) : Integer; Virtual;
      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Virtual;
      Function Find(pValue : Pointer; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil): Boolean; Overload; Virtual;

      Procedure DirectionBy(Const Value : TAdvItemListDirection); Virtual;
      Procedure DuplicateBy(Const Value : TAdvItemListDuplicates); Virtual;

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
      Procedure InternalSort(aCompare : TAdvItemListCompare; iDirection : TAdvItemListDirection = 0); Overload;

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
      Property DefaultComparison : TAdvItemListCompare Read FComparison;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Assign(oSource : TAdvObject); Override;

      Procedure Load(oFiler : TAdvFiler); Override;
      Procedure Save(oFiler : TAdvFiler); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

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

      Function IsComparedBy(Const aCompare : TAdvItemListCompare) : Boolean;
      Procedure ComparedBy(Const Value : TAdvItemListCompare);

      Procedure Uncompared;
      Function IsCompared : Boolean;
      Function IsUnCompared : Boolean;

      Function IsSortedBy(Const aCompare : TAdvItemListCompare) : Boolean;
      Procedure SortedBy(Const aCompare : TAdvItemListCompare); Overload; Virtual;

      Procedure SortAscending;
      Procedure SortDescending;

      Function IsSortAscending : Boolean;
      Function IsSortDescending : Boolean;

      Procedure Sorted;
      Procedure Unsorted;
      Function IsSorted : Boolean;
      Function IsUnsorted : Boolean;

      Function IsOrderedBy(Const Value : TAdvItemListCompare) : Boolean;
      Procedure OrderedBy(Const Value : TAdvItemListCompare);
      Procedure Unordered;

      Function IsEmpty : Boolean;

      Property Count : Integer Read FCount Write SetCount;
      Property Capacity : Integer Read FCapacity Write SetCapacity;
  End;

  EAdvItemList = Class(EAdvCollection);


Type
  TAdvLargeIntegerMatchKey = Int64;
  TAdvLargeIntegerMatchValue = Int64;

  TAdvLargeIntegerMatchItem = Record
    Key   : TAdvLargeIntegerMatchKey;
    Value : TAdvLargeIntegerMatchValue;
  End; 

  PAdvLargeIntegerMatchItem = ^TAdvLargeIntegerMatchItem;

  TAdvLargeIntegerMatchItems = Array[0..(MaxInt Div SizeOf(TAdvLargeIntegerMatchItem)) - 1] Of TAdvLargeIntegerMatchItem;
  PAdvLargeIntegerMatchItems = ^TAdvLargeIntegerMatchItems;

  TAdvLargeIntegerMatch = Class(TAdvItemList)
    Private
      FMatches : PAdvLargeIntegerMatchItems;
      FDefault : TAdvLargeIntegerMatchValue;
      FForced  : Boolean;

      Function GetKey(iIndex: Integer): TAdvLargeIntegerMatchKey;
      Procedure SetKey(iIndex: Integer; Const iValue: TAdvLargeIntegerMatchKey);

      Function GetValue(iIndex: Integer): TAdvLargeIntegerMatchValue;
      Procedure SetValue(iIndex: Integer; Const iValue: TAdvLargeIntegerMatchValue);

      Function GetMatch(iKey : TAdvLargeIntegerMatchKey): TAdvLargeIntegerMatchValue;
      Procedure SetMatch(iKey: TAdvLargeIntegerMatchKey; Const iValue: TAdvLargeIntegerMatchValue);

      Function GetPair(iIndex: Integer): TAdvLargeIntegerMatchItem;
      Procedure SetPair(iIndex: Integer; Const Value: TAdvLargeIntegerMatchItem);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex: Integer; pValue: Pointer); Override;

      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Override;
      Procedure InternalExchange(iA, iB : Integer); Override;

      Function CompareByKey(pA, pB : Pointer): Integer;
      Function CompareByValue(pA, pB : Pointer): Integer; 
      Function CompareByKeyValue(pA, pB : Pointer) : Integer; 

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Override;

      Function CapacityLimit : Integer; Override;

      Function Find(Const aKey : TAdvLargeIntegerMatchKey; Const aValue: TAdvLargeIntegerMatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean;
      Function FindByKey(Const aKey : TAdvLargeIntegerMatchKey; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean;

    Public
      Function Link : TAdvLargeIntegerMatch;

      Function Add(aKey : TAdvLargeIntegerMatchKey; aValue : TAdvLargeIntegerMatchValue): Integer; 
      Procedure Insert(iIndex : Integer; iKey : TAdvLargeIntegerMatchKey; iValue : TAdvLargeIntegerMatchValue); 

      Function IndexByKey(aKey : TAdvLargeIntegerMatchKey) : Integer; 
      Function IndexByKeyValue(Const aKey : TAdvLargeIntegerMatchKey; Const aValue : TAdvLargeIntegerMatchValue) : Integer; 

      Function ExistsByKey(aKey : TAdvLargeIntegerMatchKey) : Boolean; 
      Function ExistsByKeyValue(Const aKey : TAdvLargeIntegerMatchKey; Const aValue : TAdvLargeIntegerMatchValue) : Boolean; 

      Function EqualTo(Const oIntegerMatch : TAdvLargeIntegerMatch) : Boolean;

      Procedure DeleteByKey(aKey : TAdvLargeIntegerMatchKey); 

      Procedure ForceIncrementByKey(Const aKey : TAdvLargeIntegerMatchKey); 

      Procedure SortedByKey; 
      Procedure SortedByValue; 
      Procedure SortedByKeyValue; 

      Property Matches[iKey : TAdvLargeIntegerMatchKey] : TAdvLargeIntegerMatchValue Read GetMatch Write SetMatch; Default;
      Property Keys[iIndex : Integer] : TAdvLargeIntegerMatchKey Read GetKey Write SetKey;
      Property Values[iIndex : Integer] : TAdvLargeIntegerMatchValue Read GetValue Write SetValue;
      Property Pairs[iIndex : Integer] : TAdvLargeIntegerMatchItem Read GetPair Write SetPair;
      Property Forced : Boolean Read FForced Write FForced;
      Property Default : TAdvLargeIntegerMatchValue Read FDefault Write FDefault;
  End;
  
  TAdvObjectIterator = Class(TAdvIterator)
    Public
      Function Current : TAdvObject; Virtual;
  End;

  TAdvStringIterator = Class(TAdvIterator)
    Public
      Function Current : String; Virtual;
  End;

  TAdvIntegerIterator = Class(TAdvIterator)
    Public
      Function Current : Integer; Virtual;
  End;

  TAdvRealIterator = Class(TAdvIterator)
    Public
      Function Current : Real; Virtual;
  End;

  TAdvExtendedIterator = Class(TAdvIterator)
    Public
      Function Current : Extended; Virtual;
  End;

  TAdvBooleanIterator = Class(TAdvIterator)
    Public
      Function Current : Boolean; Virtual;
  End;

  TAdvLargeIntegerIterator = Class(TAdvIterator)
    Public
      Function Current : Int64; Virtual;
  End;

  TAdvPointerIterator = Class(TAdvIterator)
    Public
      Function Current : Pointer; Virtual;
  End;

  TAdvObjectClassIterator = Class(TAdvIterator)
    Public
      Function Current : TClass; Virtual;
  End;

  TAdvDateTimeIterator = Class(TAdvIterator)
    Public
      Function Current : TDateTime; Virtual;
  End;

  TAdvDurationIterator = Class(TAdvIterator)
    Public
      Function Current : TDuration; Virtual;
  End;

  TAdvCurrencyIterator = Class(TAdvIterator)
    Public
      Function Current : TCurrency; Virtual;
  End;

  EAdvIterator = Class(EAdvException);




Type
  TAdvIntegerObjectMatchKey = Integer;
  TAdvIntegerObjectMatchValue = TAdvObject;

  TAdvIntegerObjectMatchItem = Record
    Key : TAdvIntegerObjectMatchKey;
    Value : TAdvIntegerObjectMatchValue;
  End;

  PAdvIntegerObjectMatchItem = ^TAdvIntegerObjectMatchItem;

  TAdvIntegerObjectMatchItemArray = Array[0..(MaxInt Div SizeOf(TAdvIntegerObjectMatchItem)) - 1] Of TAdvIntegerObjectMatchItem;
  PAdvIntegerObjectMatchItemArray = ^TAdvIntegerObjectMatchItemArray;

  TAdvIntegerObjectMatch = Class(TAdvItemList)
    Private
      FItemArray : PAdvIntegerObjectMatchItemArray;
      FDefaultKey : TAdvIntegerObjectMatchKey;
      FDefaultValue : TAdvIntegerObjectMatchValue;
      FNominatedValueClass : TAdvObjectClass;
      FForced : Boolean;

      Function GetKeyByIndex(iIndex: Integer): TAdvIntegerObjectMatchKey;
      Procedure SetKeyByIndex(iIndex: Integer; Const aKey : TAdvIntegerObjectMatchKey);

      Function GetValueByIndex(iIndex: Integer): TAdvIntegerObjectMatchValue;
      Procedure SetValueByIndex(iIndex: Integer; Const aValue: TAdvIntegerObjectMatchValue);
    Function GetMatchByIndex(iIndex: Integer): TAdvIntegerObjectMatchItem;

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue: Pointer); Override;

      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CompareByKey(pA, pB : Pointer): Integer; Virtual;
      Function CompareByValue(pA, pB : Pointer): Integer; Virtual;

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Overload; Override;

      Function Find(Const aKey : TAdvIntegerObjectMatchKey; Const aValue : TAdvIntegerObjectMatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean; Overload;

      Function CapacityLimit : Integer; Override;

      Function ValidateIndex(Const sMethod: String; iIndex: Integer): Boolean; Overload; Override;
      Function ValidateValue(Const sMethod: String; oObject: TAdvObject; Const sObject: String): Boolean; Virtual;

      Function ItemClass : TAdvObjectClass;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvIntegerObjectMatch;

      Function Add(Const aKey : TAdvIntegerObjectMatchKey; Const aValue : TAdvIntegerObjectMatchValue): Integer;
      Procedure AddAll(Const oIntegerObjectMatch : TAdvIntegerObjectMatch);
      Procedure Insert(iIndex : Integer; Const aKey : TAdvIntegerObjectMatchKey; Const aValue : TAdvIntegerObjectMatchValue);

      Function IndexByKey(Const aKey : TAdvIntegerObjectMatchKey) : Integer;
      Function IndexByValue(Const aValue : TAdvIntegerObjectMatchValue) : Integer;
      Function ExistsByKey(Const aKey : TAdvIntegerObjectMatchKey) : Boolean;
      Function ExistsByValue(Const aValue : TAdvIntegerObjectMatchValue) : Boolean;

      Procedure SortedByValue;
      Procedure SortedByKey;

      Function IsSortedByKey : Boolean;
      Function IsSortedByValue : Boolean;

      Function GetKeyByValue(Const aValue : TAdvIntegerObjectMatchValue) : TAdvIntegerObjectMatchKey;
      Function GetValueByKey(Const aKey : TAdvIntegerObjectMatchKey) : TAdvIntegerObjectMatchValue;
      Procedure SetValueByKey(Const aKey : TAdvIntegerObjectMatchKey; Const aValue: TAdvIntegerObjectMatchValue);

      Procedure DeleteByKey(Const aKey : TAdvIntegerObjectMatchKey);
      Procedure DeleteByValue(Const aValue : TAdvIntegerObjectMatchValue);

      Property Forced : Boolean Read FForced Write FForced;
      Property DefaultKey : TAdvIntegerObjectMatchKey Read FDefaultKey Write FDefaultKey;
      Property DefaultValue : TAdvIntegerObjectMatchValue Read FDefaultValue Write FDefaultValue;
      Property NominatedValueClass : TAdvObjectClass Read FNominatedValueClass Write FNominatedValueClass;
      Property MatchByIndex[iIndex : Integer] : TAdvIntegerObjectMatchItem Read GetMatchByIndex; Default;
      Property KeyByIndex[iIndex : Integer] : TAdvIntegerObjectMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property ValueByIndex[iIndex : Integer] : TAdvIntegerObjectMatchValue Read GetValueByIndex Write SetValueByIndex;
  End;


  TAdvIntegerMatchKey = Integer;
  TAdvIntegerMatchValue = Integer;

  TAdvIntegerMatchItem = Record
    Key   : TAdvIntegerMatchKey;
    Value : TAdvIntegerMatchValue;
  End;

  PAdvInteger32MatchItem = ^TAdvIntegerMatchItem;

  TAdvIntegerMatchItems = Array[0..(MaxInt Div SizeOf(TAdvIntegerMatchItem)) - 1] Of TAdvIntegerMatchItem;
  PAdvInteger32MatchItems = ^TAdvIntegerMatchItems;

  TAdvIntegerMatch = Class(TAdvItemList)
    Private
      FMatches : PAdvInteger32MatchItems;
      FDefault : TAdvIntegerMatchValue;
      FForced  : Boolean;

      Function GetKey(iIndex: Integer): TAdvIntegerMatchKey;
      Procedure SetKey(iIndex: Integer; Const iValue: TAdvIntegerMatchKey);

      Function GetValue(iIndex: Integer): TAdvIntegerMatchValue;
      Procedure SetValue(iIndex: Integer; Const iValue: TAdvIntegerMatchValue);

      Function GetMatch(iKey : TAdvIntegerMatchKey): Integer;
      Procedure SetMatch(iKey: TAdvIntegerMatchKey; Const iValue: TAdvIntegerMatchValue);

      Function GetPair(iIndex: Integer): TAdvIntegerMatchItem;
      Procedure SetPair(iIndex: Integer; Const Value: TAdvIntegerMatchItem);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex: Integer; pValue: Pointer); Override;

      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CompareByKey(pA, pB : Pointer): Integer; 
      Function CompareByValue(pA, pB : Pointer): Integer; 
      Function CompareByKeyValue(pA, pB : Pointer) : Integer; 

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Overload; Override;

      Function CapacityLimit : Integer; Override;

      Function Find(Const aKey : TAdvIntegerMatchKey; Const aValue: TAdvIntegerMatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean;
      Function FindByKey(Const aKey : TAdvIntegerMatchKey; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean;

    Public
      Function Link : TAdvIntegerMatch; 

      Function Add(aKey : TAdvIntegerMatchKey; aValue : TAdvIntegerMatchValue): Integer; 
      Procedure Insert(iIndex : Integer; iKey : TAdvIntegerMatchKey; iValue : TAdvIntegerMatchValue); 

      Function IndexByKey(aKey : TAdvIntegerMatchKey) : Integer; 
      Function IndexByKeyValue(Const aKey : TAdvIntegerMatchKey; Const aValue : TAdvIntegerMatchValue) : Integer; 

      Function ExistsByKey(aKey : TAdvIntegerMatchKey) : Boolean; 
      Function ExistsByKeyValue(Const aKey : TAdvIntegerMatchKey; Const aValue : TAdvIntegerMatchValue) : Boolean; 

      Function EqualTo(Const oIntegerMatch : TAdvIntegerMatch) : Boolean;

      Procedure DeleteByKey(aKey : TAdvIntegerMatchKey); 

      Procedure ForceIncrementByKey(Const aKey : TAdvIntegerMatchKey); 

      Procedure SortedByKey; 
      Procedure SortedByValue; 
      Procedure SortedByKeyValue;

      Property Matches[iKey : TAdvIntegerMatchKey] : TAdvIntegerMatchValue Read GetMatch Write SetMatch; Default;
      Property KeyByIndex[iIndex : Integer] : TAdvIntegerMatchKey Read GetKey Write SetKey;
      Property ValueByIndex[iIndex : Integer] : TAdvIntegerMatchValue Read GetValue Write SetValue;
      Property Pairs[iIndex : Integer] : TAdvIntegerMatchItem Read GetPair Write SetPair;
      Property Forced : Boolean Read FForced Write FForced;
      Property Default : TAdvIntegerMatchValue Read FDefault Write FDefault;
  End;

  TAdvIntegerListItem = Integer;
  PAdvIntegerListItem = ^TAdvIntegerListItem;
  TAdvIntegerListItemArray = Array[0..(MaxInt Div SizeOf(TAdvIntegerListItem)) - 1] Of TAdvIntegerListItem;
  PAdvIntegerListItemArray = ^TAdvIntegerListItemArray;

  TAdvIntegerList = Class(TAdvItemList)
    Private
      FIntegerArray : PAdvIntegerListItemArray;

      Function GetIntegerByIndex(iIndex : Integer) : TAdvIntegerListItem;
      Procedure SetIntegerByIndex(iIndex : Integer; Const aValue : TAdvIntegerListItem);

      Function GetIntegers(iIndex : Integer) : TAdvIntegerListItem;
      Procedure SetIntegers(iIndex : Integer; Const Value : TAdvIntegerListItem);

    Protected
      Function GetAsText : String; 

      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue : Pointer); Override;

      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalResize(iCapacity : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CapacityLimit : Integer; Override;

    Public
      Function Link : TAdvIntegerList;
      Function Clone : TAdvIntegerList;

      Function Add(aValue : TAdvIntegerListItem) : Integer; Overload; 
      Procedure Insert(iIndex : Integer; aValue : TAdvIntegerListItem); 
      Procedure Toggle(aValue : TAdvIntegerListItem); 

      Function IndexByValue(Const iValue : TAdvIntegerListItem) : Integer; 
      Function ExistsByValue(Const iValue : TAdvIntegerListItem) : Boolean; 
      Procedure DeleteByValue(Const iValue : TAdvIntegerListItem); 
      Procedure AddAll(oIntegers : TAdvIntegerList);
      Procedure DeleteAllByValue(oIntegers : TAdvIntegerList);
      Function EqualTo(oIntegers : TAdvIntegerList) : Boolean;

      Function ExistsAll(oIntegerList : TAdvIntegerList) : Boolean; 

      Function Iterator : TAdvIterator; Override;

      Function Sum : Int64; 
      Function Mean : TAdvIntegerListItem;

      Property AsText : String Read GetAsText;
      Property Integers[iIndex : Integer] : TAdvIntegerListItem Read GetIntegers Write SetIntegers;
      Property IntegerByIndex[iIndex : Integer] : TAdvIntegerListItem Read GetIntegerByIndex Write SetIntegerByIndex; Default;
  End; 

  TAdvIntegerListIterator = Class(TAdvIntegerIterator)
    Private
      FIntegerList : TAdvIntegerList;
      FIndex : Integer;

      Procedure SetIntegerList(Const Value: TAdvIntegerList);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : Integer; Override;

      Property IntegerList : TAdvIntegerList Read FIntegerList Write SetIntegerList;
  End; 
  
  TAdvInt64MatchKey = Int64;
  TAdvInt64MatchValue = Int64;

  TAdvInt64MatchItem = Record
    Key   : TAdvInt64MatchKey;
    Value : TAdvInt64MatchValue;
  End; 

  PAdvInteger64MatchItem = ^TAdvInt64MatchItem;

  TAdvInt64MatchItems = Array[0..(MaxInt Div SizeOf(TAdvInt64MatchItem)) - 1] Of TAdvInt64MatchItem;
  PAdvInteger64MatchItems = ^TAdvInt64MatchItems;

  TAdvInt64Match = Class(TAdvItemList)
    Private
      FMatches : PAdvInteger64MatchItems;
      FDefault : TAdvInt64MatchValue;
      FForced  : Boolean;

      Function GetKey(iIndex: Integer): TAdvInt64MatchKey;
      Procedure SetKey(iIndex: Integer; Const iValue: TAdvInt64MatchKey);

      Function GetValue(iIndex: Integer): TAdvInt64MatchValue;
      Procedure SetValue(iIndex: Integer; Const iValue: TAdvInt64MatchValue);

      Function GetMatch(iKey : TAdvInt64MatchKey): TAdvInt64MatchValue;
      Procedure SetMatch(iKey: TAdvInt64MatchKey; Const iValue: TAdvInt64MatchValue);

      Function GetPair(iIndex: Integer): TAdvInt64MatchItem;
      Procedure SetPair(iIndex: Integer; Const Value: TAdvInt64MatchItem);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex: Integer; pValue: Pointer); Override;

      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CompareByKey(pA, pB : Pointer): Integer; 
      Function CompareByValue(pA, pB : Pointer): Integer; 
      Function CompareByKeyValue(pA, pB : Pointer) : Integer; 

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Overload; Override;

      Function CapacityLimit : Integer; Override;

      Function Find(Const aKey : TAdvInt64MatchKey; Const aValue: TAdvInt64MatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean;
      Function FindByKey(Const aKey : TAdvInt64MatchKey; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean;

    Public
      Function Link : TAdvInt64Match;

      Function Add(aKey : TAdvInt64MatchKey; aValue : TAdvInt64MatchValue): Integer;
      Procedure Insert(iIndex : Integer; iKey : TAdvInt64MatchKey; iValue : TAdvInt64MatchValue);

      Function IndexByKey(aKey : TAdvInt64MatchKey) : Integer;
      Function IndexByKeyValue(Const aKey : TAdvInt64MatchKey; Const aValue : TAdvInt64MatchValue) : Integer;

      Function ExistsByKey(aKey : TAdvInt64MatchKey) : Boolean;
      Function ExistsByKeyValue(Const aKey : TAdvInt64MatchKey; Const aValue : TAdvInt64MatchValue) : Boolean;

      Function EqualTo(Const oIntegerMatch : TAdvInt64Match) : Boolean;

      Procedure DeleteByKey(aKey : TAdvInt64MatchKey);

      Procedure ForceIncrementByKey(Const aKey : TAdvInt64MatchKey);

      Procedure SortedByKey; 
      Procedure SortedByValue; 
      Procedure SortedByKeyValue; 

      Property Matches[iKey : TAdvInt64MatchKey] : TAdvInt64MatchValue Read GetMatch Write SetMatch; Default;
      Property KeyByIndex[iIndex : Integer] : TAdvInt64MatchKey Read GetKey Write SetKey;
      Property ValueByIndex[iIndex : Integer] : TAdvInt64MatchValue Read GetValue Write SetValue;
      Property Pairs[iIndex : Integer] : TAdvInt64MatchItem Read GetPair Write SetPair;
      Property Forced : Boolean Read FForced Write FForced;
      Property Default : TAdvInt64MatchValue Read FDefault Write FDefault;
  End;


  TPointerItem = Pointer;
  TPointerItems = Array[0..(MaxInt Div SizeOf(TPointerItem)) - 1] Of TPointerItem;
  PPointerItems = ^TPointerItems;

  TAdvPointerList = Class(TAdvItemList)
    Private
      FPointerArray : PPointerItems;

      Function GetPointerByIndex(iIndex: Integer): TPointerItem;
      Procedure SetPointerByIndex(iIndex: Integer; Const pValue: TPointerItem);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue: Pointer); Override;

      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Override;

      Function CapacityLimit : Integer; Override;

    Public
      Function Iterator : TAdvIterator; Override;

      Function IndexByValue(pValue : TPointerItem) : Integer;
      Function ExistsByValue(pValue : TPointerItem) : Boolean;
      Function Add(pValue : TPointerItem) : Integer;
      Procedure Insert(iIndex : Integer; pValue : TPointerItem);
      Procedure DeleteByValue(pValue : TPointerItem);
      Function RemoveFirst : TPointerItem;
      Function RemoveLast : TPointerItem;

      Property PointerByIndex[iIndex : Integer] : Pointer Read GetPointerByIndex Write SetPointerByIndex; Default;
  End;

  TAdvPointerListIterator = Class(TAdvPointerIterator)
    Private
      FPointerArray : TAdvPointerList;
      FIndex : Integer;

      Procedure SetPointers(Const Value: TAdvPointerList);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : Pointer; Override;

      Property Pointers : TAdvPointerList Read FPointerArray Write SetPointers;
  End;

  TAdvPointers = TAdvPointerList;


  TAdvClassList = Class(TAdvPointerList)
    Private
      Function GetClassByIndex(Const iIndex : Integer): TClass;
      Procedure SetClassByIndex(Const iIndex : Integer; Const Value : TClass);

    Protected
      Function ItemClass : TAdvObjectClass; Virtual;

    Public
      Function Iterator : TAdvIterator; Override;

      Function Add(Const aClass : TClass) : Integer;
      Procedure AddAll(Const oClassList : TAdvClassList);
      Procedure AddArray(Const aClasses : Array Of TClass);
      Function IndexByClassType(aClass : TClass) : Integer;
      Function ExistsByClassType(aClass : TClass) : Boolean;
      Function Find(Const aClass : TClass; Out iIndex : Integer) : Boolean;

      Property ClassByIndex[Const iIndex : Integer] : TClass Read GetClassByIndex Write SetClassByIndex; Default;
  End; 

  TAdvClassListIterator = Class(TAdvObjectClassIterator)
    Private
      FClassList : TAdvClassList;
      FIndex : Integer;

      Procedure SetClassList(Const Value : TAdvClassList);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : TClass; Override;

      Property ClassList : TAdvClassList Read FClassList Write SetClassList;
  End;

  TAdvHashEntryCode = Integer;

  TAdvHashEntry = Class(TAdvPersistent)
    Private
      FCode : TAdvHashEntryCode;
      FNextHashEntry : TAdvHashEntry;

    Protected
      Procedure Generate; Virtual;

      Property Code : TAdvHashEntryCode Read FCode Write FCode;

    Public
      Procedure Assign(oSource : TAdvObject); Override;
      Procedure Load(oFiler : TAdvFiler); Override;

      Function Link : TAdvHashEntry;
      Function Clone : TAdvHashEntry;
  End;

  PAdvHashEntry = ^TAdvHashEntry;
  TAdvHashEntryClass = Class Of TAdvHashEntry;

  TAdvHashEntryArray = Array [0..MaxInt Div SizeOf(TAdvHashEntry) - 1] Of TAdvHashEntry;
  PAdvHashEntryArray = ^TAdvHashEntryArray;

  TAdvHashTable = Class(TAdvCollection)
    Private
      FTable : PAdvHashEntryArray;
      FCount : Integer;
      FCapacity : Integer;
      FThreshold : Integer;
      FBalance : Real;
      FPreventRehash : Boolean;

      Procedure SetCapacity(Const Value : Integer);
      Procedure SetBalance(Const Value : Real);

    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

      Function Resolve(oHashEntry : TAdvHashEntry) : Cardinal;
      Function Duplicate(oHashEntry : TAdvHashEntry) : TAdvHashEntry;

      Procedure Rehash;

      Procedure InternalClear; Override;

      Function Find(oSource, oHashEntry : TAdvHashEntry) : TAdvHashEntry;
      Procedure Insert(iIndex : Integer; oHashEntry: TAdvHashEntry);

      Function Equal(oA, oB : TAdvHashEntry) : Integer; Virtual;

      Function ItemClass : TAdvHashEntryClass; Virtual;
      Function ItemNew : TAdvHashEntry; Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvHashTable;
      Function Clone : TAdvHashTable;

      Procedure Assign(oObject : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;
      Procedure Load(oFiler : TAdvFiler); Override;
      Procedure Save(oFiler : TAdvFiler); Override;

      Procedure PreventRehash;
      Procedure AllowRehash;

      Procedure PredictCapacityByExpectedCount(Const iCount : Integer);

      Function ProduceHashEntry : TAdvHashEntry;
      Procedure ConsumeHashEntry(oHashEntry : TAdvHashEntry);

      Function Iterator : TAdvIterator; Override;

      Function IsEmpty : Boolean;

      Procedure Add(oHashEntry : TAdvHashEntry); Overload; Virtual;
      Function Delete(oHashEntry : TAdvHashEntry) : Boolean; Overload; Virtual;
      Function Force(oHashEntry : TAdvHashEntry) : TAdvHashEntry; Virtual;
      Function Replace(oHashEntry : TAdvHashEntry) : TAdvHashEntry; Overload; Virtual;
      Function Get(oHashEntry : TAdvHashEntry) : TAdvHashEntry; Virtual;
      Function Exists(oHashEntry : TAdvHashEntry) : Boolean; Overload; Virtual;

      Property Capacity : Integer Read FCapacity Write SetCapacity;
      Property Balance : Real Read FBalance Write SetBalance;
      Property Count : Integer Read FCount;
  End;

  PAdvObject = ^TAdvObject;
  TAdvObjectArray = array[0..(MaxInt div SizeOf(TAdvObject)) - 1] of TAdvObject;
  PAdvObjectArray = ^TAdvObjectArray;

  TAdvObjectListClass = class of TAdvObjectList;
  TAdvObjectListIterator = class;
  TAdvObjectListIteratorClass = class of TAdvObjectListIterator;

  TAdvObjectList = class(TAdvItemList)
  private
    FObjectArray: PAdvObjectArray;

    FItemClass: TAdvObjectClass;

    function GetObject(iIndex: integer): TAdvObject;
    procedure SetObject(iIndex: integer; const oValue: TAdvObject);

    function GetObjectByIndex(const iIndex: integer): TAdvObject;
    procedure SetObjectByIndex(const iIndex: integer; const Value: TAdvObject);

    function GetItemClass: TAdvObjectClass;
    procedure SetItemClass(const Value: TAdvObjectClass);

  protected
    function ErrorClass: EAdvExceptionClass; override;

    function GetItem(iIndex: integer): Pointer; override;
    procedure SetItem(iIndex: integer; pValue: Pointer); override;
    function AddressOfItem(iIndex: integer): PAdvObject;

    procedure LoadItem(oFiler: TAdvFiler; iIndex: integer); override;
    procedure SaveItem(oFiler: TAdvFiler; iIndex: integer); override;
    procedure AssignItem(oItems: TAdvItemList; iIndex: integer); override;

    procedure InternalTruncate(iValue: integer); override;
    procedure InternalResize(iValue: integer); override;
    procedure InternalCopy(iSource, iTarget, iCount: integer); override;
    procedure InternalEmpty(iIndex, iLength: integer); override;
    procedure InternalInsert(iIndex: integer); override;
    procedure InternalExchange(iA, iB: integer); override;
    procedure InternalDelete(iIndex: integer); override;

    function ValidateIndex(const sMethod: string; iIndex: integer): boolean;
      override;
    function ValidateItem(const sMethod: string; oObject: TAdvObject;
      const sObject: string): boolean; virtual;

    function Find(pValue: Pointer; Out iIndex: integer;
      aCompare: TAdvItemListCompare = nil): boolean; overload; override;
    function Find(oValue: TAdvObject): integer; overload;

    function GetByIndex(iIndex: integer): TAdvObject; overload;

    function CompareByClass(pA, pB: Pointer): integer; overload;
    function CompareByReference(pA, pB: Pointer): integer; overload;

    function ItemNew: TAdvObject; virtual;
    function ItemClass: TAdvObjectClass; virtual;
    function IteratorClass: TAdvObjectListIteratorClass; virtual;
    function CapacityLimit: integer; override;

    function Insertable(const sMethod: string; oObject: TAdvObject): boolean;
      overload;
    function Replaceable(const sMethod: string; oOld, oNew: TAdvObject): boolean;
      overload;
    function Deleteable(const sMethod: string; oObject: TAdvObject): boolean;
      overload;

    function Deleteable(const sMethod: string; iIndex: integer): boolean;
      overload; override;
    function Extendable(const sMethod: string; iCount: integer): boolean; override;

    procedure InternalAfterInclude(iIndex: integer); virtual;
    procedure InternalBeforeExclude(iIndex: integer); virtual;

    // Attribute
    function AllowUnassigned: boolean; virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Link: TAdvObjectList;
    function Clone: TAdvObjectList;

    procedure Collect(oList: TAdvObjectList);
    procedure AddAll(oList: TAdvObjectList);

    function Add(oValue: TAdvObject): integer; overload; virtual;
    procedure Insert(iIndex: integer; oValue: TAdvObject); overload;
    procedure Move(iSource, iTarget: integer); overload;

    function IndexBy(oValue: TAdvObject; aCompare: TAdvItemListCompare): integer;
    function ExistsBy(oValue: TAdvObject; aCompare: TAdvItemListCompare): boolean;
    function GetBy(oValue: TAdvObject; aCompare: TAdvItemListCompare): TAdvObject;
    function ForceBy(oValue: TAdvObject; aCompare: TAdvItemListCompare): TAdvObject;
    procedure DeleteBy(oValue: TAdvObject; aCompare: TAdvItemListCompare);

    procedure SortedByClass;
    procedure OrderedByClass;
    function IsSortedByClass: boolean;
    function IsOrderedByClass: boolean;
    function IndexByClass(aClass: TAdvObjectClass): integer; overload;
    function IndexByClass(oValue: TAdvObject): integer; overload;
    function GetByClass(aClass: TAdvObjectClass): TAdvObject;
    function ExistsByClass(aClass: TAdvObjectClass): boolean;
    procedure DeleteByClass(aClass: TAdvObjectClass);

    procedure SortedByReference;
    procedure OrderedByReference;
    function IsSortedByReference: boolean;
    function IsOrderedByReference: boolean;
    function IndexByReference(oValue: TAdvObject): integer;
    function ExistsByReference(oValue: TAdvObject): boolean;
    procedure DeleteByReference(oValue: TAdvObject);
    procedure DeleteAllByReference(oValue: TAdvObjectList);

    function ContainsAllByReference(oObjectList: TAdvObjectList): boolean;
    function ContainsAnyByReference(oObjectList: TAdvObjectList): boolean;

    function ExistsByDefault(oValue: TAdvObject): boolean;
    function IndexByDefault(oValue: TAdvObject): integer;
    procedure DeleteByDefault(oValue: TAdvObject);

    function RemoveFirst: TAdvObject;
    function RemoveLast: TAdvObject;

    function Iterator: TAdvIterator; override;
    function ProduceIterator: TAdvObjectListIterator; overload;
    procedure ConsumeIterator(oIterator: TAdvObjectListIterator); overload;

    function Get(iIndex: integer): TAdvObject; overload;
    function New: TAdvObject; overload; virtual;

    property ObjectByIndex[const iIndex: integer]: TAdvObject
      read GetObjectByIndex write SetObjectByIndex; default;
    property Objects[const iIndex: integer]: TAdvObject
      read GetObjectByIndex write SetObjectByIndex;
    property NominatedClass: TAdvObjectClass read GetItemClass write SetItemClass;
  end;

  EAdvObjectList = class(EAdvItemList);

  TAdvObjectListIterator = class(TAdvObjectIterator)
  private
    FList: TAdvObjectList;
    FIndex: integer;
    FDeleted: boolean;

    procedure SetList(const Value: TAdvObjectList);
    function GetList: TAdvObjectList;

  protected
    procedure StepBack;
    procedure StepNext;
    function Skip: boolean; virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure First; override;
    procedure Last; override;
    procedure Next; override;
    procedure Back; override;
    function More: boolean; override;

    function Current: TAdvObject; override;
    procedure Delete;

    property Index: integer read FIndex write FIndex;
    property List: TAdvObjectList read GetList write SetList;
  end;

  TAdvObject = FHIR.Support.Objects.TAdvObject;
  TAdvObjectClass = FHIR.Support.Objects.TAdvObjectClass;

  TAdvPersistentList = Class(TAdvObjectList)
    Private
      Function GetPersistentByIndex(Const iIndex : Integer) : TAdvPersistent;
      Procedure SetPersistentByIndex(Const iIndex : Integer; Const oValue : TAdvPersistent);

    Protected
      Function ItemClass : TAdvObjectClass; Override;

    Public
      Property PersistentByIndex[Const iIndex : Integer] : TAdvPersistent Read GetPersistentByIndex Write SetPersistentByIndex; Default;
  End;

  TAdvPersistentListIterator = Class(TAdvObjectListIterator)
  End;

  TAdvPersistentListClass = Class Of TAdvPersistentList;

  TAdvHashTableList = Class(TAdvPersistentList)
    Private
      Function GetHash(iIndex: Integer): TAdvHashTable;

    Protected
      Function ItemClass : TAdvObjectClass; Override;

    Public
      Property Hashes[iIndex : Integer] : TAdvHashTable Read GetHash; Default;
  End;

  TAdvHashTableIterator = Class(TAdvObjectIterator)
    Private
      FHashTable : TAdvHashTable;
      FIndex : Integer;
      FCurrentHashEntry : TAdvHashEntry;
      FNextHashEntry : TAdvHashEntry;

      Function GetHashTable: TAdvHashTable;
      Procedure SetHashTable(Const Value: TAdvHashTable);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure First; Override;
      Procedure Next; Override;
      Function More : Boolean; Override;

      Function Count : Integer;
      Function Current : TAdvObject; Override;

      Property HashTable : TAdvHashTable Read GetHashTable Write SetHashTable;
  End;

  EAdvHashTable = Class(EAdvCollection);



  TAdvStringObjectMatchKey = String;
  TAdvStringObjectMatchValue = TAdvObject;

  TAdvStringObjectMatchItem = Record
    Key   : TAdvStringObjectMatchKey;
    Value : TAdvStringObjectMatchValue;
  End;

  PAdvStringObjectMatchItem = ^TAdvStringObjectMatchItem;

  TAdvStringObjectMatchItemArray = Array[0..(MaxInt Div SizeOf(TAdvStringObjectMatchItem)) - 1] Of TAdvStringObjectMatchItem;
  PAdvStringObjectMatchItemArray = ^TAdvStringObjectMatchItemArray;

  TAdvStringCompareCallback = Function (Const sA, sB : String) : Integer;

  TAdvStringObjectMatch = Class(TAdvItemList)
    Private
      FMatchArray : PAdvStringObjectMatchItemArray;
      FDefaultKey : TAdvStringObjectMatchKey;
      FDefaultValue : TAdvStringObjectMatchValue;
      FCompareKey : TAdvStringCompareCallback;
      FNominatedValueClass : TAdvObjectClass;
      FSymbol : String;
      FForced : Boolean;

      Function GetKeyByIndex(iIndex: Integer): TAdvStringObjectMatchKey;
      Procedure SetKeyByIndex(iIndex: Integer; Const aKey : TAdvStringObjectMatchKey);

      Function GetValueByIndex(iIndex: Integer): TAdvStringObjectMatchValue;
      Procedure SetValueByIndex(iIndex: Integer; Const aValue: TAdvStringObjectMatchValue);

      Function GetMatch(Const aKey : TAdvStringObjectMatchKey): TAdvStringObjectMatchValue;
      Procedure SetMatch(Const aKey : TAdvStringObjectMatchKey; Const aValue : TAdvStringObjectMatchValue);

      Function GetMatchByIndex(iIndex: Integer): TAdvStringObjectMatchItem;

      Function GetAsText: String;
      Procedure SetAsText(Const Value: String);

      Function GetSensitive: Boolean;
      Procedure SetSensitive(Const Value: Boolean);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue: Pointer); Override;

      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Function ValidateIndex(Const sMethod: String; iIndex: Integer): Boolean; Override;
      Function ValidateItem(Const sMethod: String; oObject: TAdvObject; Const sObject: String): Boolean; Virtual;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Override;
      Procedure InternalDelete(iIndex : Integer); Override;
      Procedure InternalExchange(iA, iB : Integer); Override;

      Function CompareByKey(pA, pB : Pointer): Integer; Virtual;
      Function CompareByValue(pA, pB : Pointer): Integer; Virtual;

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Override;

      Function Find(Const aKey : TAdvStringObjectMatchKey; Const aValue : TAdvStringObjectMatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean;

      Function CapacityLimit : Integer; Override;

      Function ItemClass : TAdvObjectClass; Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvStringObjectMatch; Overload;

      Function IndexByKey(Const aKey : TAdvStringObjectMatchKey) : Integer;
      Function IndexByValue(Const aValue : TAdvStringObjectMatchValue) : Integer;

      Function ExistsByKey(Const aKey : TAdvStringObjectMatchKey) : Boolean;
      Function ExistsByValue(Const aValue : TAdvStringObjectMatchValue) : Boolean;

      Procedure AddAll(oSourceStringObjectMatch : TAdvStringObjectMatch);

      Function Add(Const aKey : TAdvStringObjectMatchKey; Const aValue : TAdvStringObjectMatchValue): Integer; Overload;
      Procedure Insert(iIndex : Integer; Const aKey : TAdvStringObjectMatchKey; Const aValue : TAdvStringObjectMatchValue); Overload;
      Function FindByKey(Const aKey : TAdvStringObjectMatchKey; Out iIndex : Integer) : Boolean; Overload;
      Function FindByValue(Const aValue : TAdvStringObjectMatchValue; Out iIndex : Integer) : Boolean; Overload;

      Procedure SortedByKey;
      Procedure SortedByValue;

      Function IsSortedByKey : Boolean;
      Function IsSortedByValue : Boolean;

      Function GetKeyByValue(Const aValue : TAdvStringObjectMatchValue) : TAdvStringObjectMatchKey;
      Function GetValueByKey(Const aKey : TAdvStringObjectMatchKey) : TAdvStringObjectMatchValue;
      Procedure SetValueByKey(Const aKey : TAdvStringObjectMatchKey; Const aValue : TAdvStringObjectMatchValue);

      Procedure DeleteByKey(Const aKey : TAdvStringObjectMatchKey);
      Procedure DeleteByValue(Const aValue : TAdvStringObjectMatchValue);

      Property Matches[Const aKey : TAdvStringObjectMatchKey] : TAdvStringObjectMatchValue Read GetMatch Write SetMatch; Default;
      Property Forced : Boolean Read FForced Write FForced;
      Property DefaultKey : TAdvStringObjectMatchKey Read FDefaultKey Write FDefaultKey;
      Property DefaultValue : TAdvStringObjectMatchValue Read FDefaultValue Write FDefaultValue;
      Property AsText : String Read GetAsText Write SetAsText;
      Property Symbol : String Read FSymbol Write FSymbol;
      Property Sensitive : Boolean Read GetSensitive Write SetSensitive;
      Property NominatedValueClass : TAdvObjectClass Read FNominatedValueClass Write FNominatedValueClass;
      Property MatchByIndex[iIndex : Integer] : TAdvStringObjectMatchItem Read GetMatchByIndex;
      Property KeyByIndex[iIndex : Integer] : TAdvStringObjectMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property Keys[iIndex : Integer] : TAdvStringObjectMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property ValueByIndex[iIndex : Integer] : TAdvStringObjectMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property Values[iIndex : Integer] : TAdvStringObjectMatchValue Read GetValueByIndex Write SetValueByIndex;
  End;

Type
  TAdvStringMatchKey = String;
  TAdvStringMatchValue = String;

  TAdvStringMatchItem = Record
    Key : TAdvStringMatchKey;
    Value : TAdvStringMatchValue;
  End;

  PAdvStringMatchItem = ^TAdvStringMatchItem;

  TAdvStringMatchItems = Array[0..(MaxInt Div SizeOf(TAdvStringMatchItem)) - 1] Of TAdvStringMatchItem;
  PAdvStringMatchItems = ^TAdvStringMatchItems;

  TAdvStringMatch = Class(TAdvItemList)
    Private
      FMatchArray : PAdvStringMatchItems;
      FDefaultValue : TAdvStringMatchValue;
      FSymbol : String;
      FSeparator : String;
      FForced : Boolean;

      Function GetKeyByIndex(iIndex: Integer): TAdvStringMatchKey;
      Procedure SetKeyByIndex(iIndex: Integer; Const aKey : TAdvStringMatchKey);

      Function GetValueByIndex(iIndex: Integer): TAdvStringMatchValue;
      Procedure SetValueByIndex(iIndex: Integer; Const aValue: TAdvStringMatchValue);

      Function GetMatchByIndex(Const iIndex : Integer) : TAdvStringMatchItem;
      Procedure SetMatchByIndex(Const iIndex : Integer; Const Value : TAdvStringMatchItem);

      Function GetMatch(Const aKey : TAdvStringMatchKey): TAdvStringMatchValue;
      Procedure SetMatch(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue);

    Protected
      Function CapacityLimit : Integer; Override;

      Function ErrorClass : EAdvExceptionClass; Override;

      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue: Pointer); Override;

      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

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

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Overload; Override;

      Function Find(Const aKey : TAdvStringMatchKey; Const aValue: TAdvStringMatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean; Overload;

      Function GetAsText : String; 
      Procedure SetAsText(Const sValue: String); 

      Function DefaultValue(Const aKey : TAdvStringMatchKey) : TAdvStringMatchValue; Virtual; 

    Public
      Constructor Create; Override;

      Procedure Assign(oObject : TAdvObject); Override;

      Function Link : TAdvStringMatch;
      Function Clone : TAdvStringMatch; 

      Procedure AddAll(oStringMatch : TAdvStringMatch);

      Function IndexByKey(Const aKey : TAdvStringMatchKey) : Integer;
      Function IndexByCaseSensitiveKey(Const aKey : TAdvStringMatchKey) : Integer;
      Function IndexByValue(Const aValue : TAdvStringMatchValue) : Integer;
      Function ForceByKey(Const aKey : TAdvStringMatchKey) : TAdvStringMatchValue;
      Function ExistsByKey(Const aKey : TAdvStringMatchKey) : Boolean;
      Procedure DeleteByKey(Const aKey : TAdvStringMatchKey);
      Function ExistsByValue(Const aValue : TAdvStringMatchValue) : Boolean;
      Function ExistsByKeyAndValue(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue) : Boolean;

      Function IndexOf(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue) : Integer;
      Function Add(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue): Integer;
      Procedure Insert(iIndex : Integer; Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue);

      Function EqualTo(oMatch : TAdvStringMatch) : Boolean;

      Function GetValueByKey(Const aKey : TAdvStringMatchKey): TAdvStringMatchValue;
      Procedure SetValueByKey(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue);

      Procedure SortedByCaseSensitiveKey;
      Procedure SortedByKey;
      Procedure SortedByValue;

      Property Matches[Const aKey : TAdvStringMatchKey] : TAdvStringMatchValue Read GetMatch Write SetMatch; Default;
      Property Forced : Boolean Read FForced Write FForced;
      Property Default : TAdvStringMatchValue Read FDefaultValue Write FDefaultValue;
      Property Symbol : String Read FSymbol Write FSymbol;
      Property Separator : String Read FSeparator Write FSeparator;
      Property MatchByIndex[Const iIndex : Integer] : TAdvStringMatchItem Read GetMatchByIndex Write SetMatchByIndex;
      Property KeyByIndex[iIndex : Integer] : TAdvStringMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property ValueByIndex[iIndex : Integer] : TAdvStringMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property Keys[iIndex : Integer] : TAdvStringMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property Values[iIndex : Integer] : TAdvStringMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property AsText : String Read GetAsText Write SetAsText;
  End;

  EAdvStringMatch = Class(EAdvItemList);

  TAdvStringListItem = String;
  TAdvStringListItemArray = Array[0..(MaxInt Div SizeOf(TAdvStringListItem)) - 1] Of TAdvStringListItem;
  PAdvStringListItemArray = ^TAdvStringListItemArray;

  TAdvStringHashEntry = Class(TAdvHashEntry)
    Private
      FName : String;

      Procedure SetName(Const Value: String);

    Protected
      Procedure Generate; Override;

    Public
      Procedure Assign(oSource : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

      Property Name : String Read FName Write SetName;
  End;

  TAdvStringHashEntryClass = Class Of TAdvStringHashEntry;

  TAdvStringHashTable = Class(TAdvHashTable)
    Protected
      Function Equal(oA, oB : TAdvHashEntry) : Integer; Override;

      Function ItemClass : TAdvHashEntryClass; Override;

    Public
      Function Link : TAdvStringHashTable;

      Function Iterator : TAdvIterator; Override;
  End;

  TAdvStringHashTableIterator = Class(TAdvHashTableIterator)
    Public
      Function Current : TAdvStringHashEntry; Reintroduce;
  End;




  TAdvObjectClassHashEntry = Class(TAdvStringHashEntry)
    Private
      FData : TClass;

    Public
      Procedure Assign(oSource : TAdvObject); Override;

      Property Data : TClass Read FData Write FData; // no set data as hashed classname may be different to FData.ClassName.
  End;

  TAdvObjectClassHashTable = Class(TAdvStringHashTable)
    Protected
      Function ItemClass : TAdvHashEntryClass; Override;

    Public
      Function Iterator : TAdvIterator; Override;
  End;

  TAdvObjectClassHashTableIterator = Class(TAdvObjectClassIterator)
    Private
      FInternal : TAdvStringHashTableIterator;

      Function GetHashTable: TAdvObjectClassHashTable;
      Procedure SetHashTable(Const Value: TAdvObjectClassHashTable);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : TClass; Override;

      Property HashTable : TAdvObjectClassHashTable Read GetHashTable Write SetHashTable;
  End;

  TAdvStringList = Class(TAdvItemList)
    Private
      FStringArray : PAdvStringListItemArray;
      FSymbol : String;

      Function GetStringByIndex(iIndex : Integer) : TAdvStringListItem;
      Procedure SetStringByIndex(iIndex : Integer; Const sValue : TAdvStringListItem);

      Function GetSensitive : Boolean;
      Procedure SetSensitive(Const bValue: Boolean);

      Function GetAsCSV : String;
      Procedure SetAsCSV(Const sValue : String);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue : Pointer); Override;

      Function GetAsText : String; Virtual;
      Procedure SetAsText(Const sValue : String); Virtual;

      Procedure LoadItem(Filer : TAdvFiler; iIndex : Integer); Override;
      Procedure SaveItem(Filer : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(Items : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Overload; Override;

      Function CompareInsensitive(pA, pB : Pointer): Integer;
      Function CompareSensitive(pA, pB : Pointer): Integer;

      Function CapacityLimit : Integer; Override;

    Public
      Constructor Create; Override;

      Function Link : TAdvStringList;
      Function Clone : TAdvStringList;

      Procedure SaveToText(Const sFilename : String);
      Procedure LoadFromText(Const sFilename : String);

      Function Iterator : TAdvIterator; Override;

      Procedure AddAll(oStrings : TAdvStringList);
      Procedure AddAllStringArray(Const aValues : Array Of String);

      Function IndexByValue(Const sValue : TAdvStringListItem) : Integer;
      Function ExistsByValue(Const sValue : TAdvStringListItem) : Boolean;
      Function Add(Const sValue : TAdvStringListItem) : Integer;
      Procedure Insert(iIndex : Integer; Const sValue : TAdvStringListItem);
      Procedure DeleteByValue(Const sValue : TAdvStringListItem); 

      Function Equals(oStrings : TAdvStringList) : Boolean; Reintroduce;
      Function Compare(oStrings : TAdvStringList) : Integer;
      Function ExistsAny(oStrings : TAdvStringList) : Boolean;

      Function Find(Const sValue : TAdvStringListItem; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean; Overload;

      // script wrappers
      procedure SetItems(iIndex : integer; sValue : String);
      function GetItems(iIndex : integer) : String;
      procedure delete(iIndex : integer);
      procedure populate(iCount : integer);
      Function IndexOf(value : String): Integer;
      Property Items[iIndex : Integer] : TAdvStringListItem Read GetStringByIndex Write SetStringByIndex;
      Property Text : String read GetAsText write SetAsText;

      Property StringByIndex[iIndex : Integer] : TAdvStringListItem Read GetStringByIndex Write SetStringByIndex; Default;
      Property AsText : String Read GetAsText Write SetAsText;
      Property AsCSV : String Read GetAsCSV Write SetAsCSV;
      Property Symbol : String Read FSymbol Write FSymbol;
      Property Sensitive : Boolean Read GetSensitive Write SetSensitive;
  End;

  TAdvStringListIterator = Class(TAdvStringIterator)
    Private
      FStringList : TAdvStringList;
      FIndex : Integer;

      Procedure SetStringList(Const Value: TAdvStringList);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure First; Override;
      Procedure Last; Override;
      Procedure Next; Override;
      Procedure Back; Override;

      Function More : Boolean; Override;
      Function Current : String; Override;

      Property StringList : TAdvStringList Read FStringList Write SetStringList;
  End;


Type
  TAdvStringLargeIntegerMatchKey = String;
  TAdvStringLargeIntegerMatchValue = Int64;

  TAdvStringLargeIntegerMatchItem = Record
    Key : TAdvStringLargeIntegerMatchKey;
    Value : TAdvStringLargeIntegerMatchValue;
  End;

  PAdvStringLargeIntegerMatchItem = ^TAdvStringLargeIntegerMatchItem;

  TAdvStringLargeIntegerMatchItemArray = Array[0..(MaxInt Div SizeOf(TAdvStringLargeIntegerMatchItem)) - 1] Of TAdvStringLargeIntegerMatchItem;
  PAdvStringLargeIntegerMatchItemArray = ^TAdvStringLargeIntegerMatchItemArray;

  TAdvStringLargeIntegerMatch = Class(TAdvItemList)
    Private
      FMatchArray : PAdvStringLargeIntegerMatchItemArray;
      FDefault : Integer;
      FForced : Boolean;
      FCompareKey : TAdvStringCompareCallback;
      FSymbol : String;

      Function GetKey(iIndex : Integer): String;
      Procedure SetKey(iIndex : Integer; Const aKey : TAdvStringLargeIntegerMatchKey);

      Function GetValue(iIndex : Integer): TAdvStringLargeIntegerMatchValue;
      Procedure SetValue(iIndex : Integer; Const aValue : TAdvStringLargeIntegerMatchValue);

      Function GetMatch(Const aKey : TAdvStringLargeIntegerMatchKey): TAdvStringLargeIntegerMatchValue;
      Procedure SetMatch(Const aKey : TAdvStringLargeIntegerMatchKey; Const aValue : TAdvStringLargeIntegerMatchValue);

      Function GetAsText : String;
      Function GetKeysAsText : String;

      Function GetSensitive: Boolean;
      Procedure SetSensitive(Const Value: Boolean);

    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex: Integer; pValue: Pointer); Override;

      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalTruncate(iValue : Integer); Override;      
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;

      Function CompareKey(pA, pB : Pointer) : Integer; Virtual;
      Function CompareValue(pA, pB : Pointer) : Integer; Virtual;

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Overload; Override;

      Function Find(Const aKey : TAdvStringLargeIntegerMatchKey; Const aValue : TAdvStringLargeIntegerMatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean; Overload;

      Function CapacityLimit : Integer; Override;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvStringLargeIntegerMatch; 
      Function Clone : TAdvStringLargeIntegerMatch;

      Function IndexByKey(Const aKey : TAdvStringLargeIntegerMatchKey) : Integer; 
      Function ExistsByKey(Const aKey : TAdvStringLargeIntegerMatchKey) : Boolean;
      Function IndexByValue(Const aValue : TAdvStringLargeIntegerMatchValue) : Integer;
      Function ExistsByValue(Const aValue : TAdvStringLargeIntegerMatchValue) : Boolean;
      Function Add(Const aKey : TAdvStringLargeIntegerMatchKey; Const aValue : TAdvStringLargeIntegerMatchValue) : Integer; Overload;
      Function Force(Const aKey : TAdvStringLargeIntegerMatchKey) : TAdvStringLargeIntegerMatchValue; Overload;
      Procedure Insert(iIndex : Integer; Const aKey : TAdvStringLargeIntegerMatchKey; Const aValue : TAdvStringLargeIntegerMatchValue); Overload;

      Function GetByKey(Const aKey : TAdvStringLargeIntegerMatchKey) : TAdvStringLargeIntegerMatchValue; 
      Function GetByValue(Const aValue : TAdvStringLargeIntegerMatchValue) : TAdvStringLargeIntegerMatchKey;

      Function IsSortedByKey: Boolean;
      Function IsSortedByValue: Boolean;

      Procedure SortedByKey;
      Procedure SortedByValue;

      Property Matches[Const sIndex : TAdvStringLargeIntegerMatchKey] : TAdvStringLargeIntegerMatchValue Read GetMatch Write SetMatch; Default;
      Property KeyByIndex[iIndex : Integer] : TAdvStringLargeIntegerMatchKey Read GetKey Write SetKey;
      Property ValueByIndex[iIndex : Integer] : TAdvStringLargeIntegerMatchValue Read GetValue Write SetValue;
      Property Forced : Boolean Read FForced Write FForced;
      Property Default : Integer Read FDefault Write FDefault;
      Property KeysAsText : String Read GetKeysAsText;
      Property AsText : String Read GetAsText;
      Property Symbol : String Read FSymbol Write FSymbol;
      Property Sensitive : Boolean Read GetSensitive Write SetSensitive;
  End;

  EAdvStringLargeIntegerMatch = Class(EAdvException);

  TAdvStringIntegerMatchKey = String;
  TAdvStringIntegerMatchValue = Integer;

  TAdvStringIntegerMatchItem = Record
    Key : TAdvStringIntegerMatchKey;
    Value : TAdvStringIntegerMatchValue;
  End;

  PAdvStringIntegerMatchItem = ^TAdvStringIntegerMatchItem;

  TAdvStringIntegerMatchItemArray = Array[0..(MaxInt Div SizeOf(TAdvStringIntegerMatchItem)) - 1] Of TAdvStringIntegerMatchItem;
  PAdvStringIntegerMatchItemArray = ^TAdvStringIntegerMatchItemArray;

  TAdvStringIntegerMatch = Class(TAdvItemList)
    Private
      FMatchArray : PAdvStringIntegerMatchItemArray;
      FDefaultKey : TAdvStringIntegerMatchKey;
      FDefaultValue : TAdvStringIntegerMatchValue;
      FForced : Boolean;
      FCompareKey : TAdvStringCompareCallback;
      FSymbol : String;

      Function GetKey(iIndex : Integer): String;
      Procedure SetKey(iIndex : Integer; Const aKey : TAdvStringIntegerMatchKey);

      Function GetValue(iIndex : Integer): Integer;
      Procedure SetValue(iIndex : Integer; Const aValue : TAdvStringIntegerMatchValue);

      Function GetMatch(Const aKey : TAdvStringIntegerMatchKey): TAdvStringIntegerMatchValue;
      Procedure SetMatch(Const aKey : TAdvStringIntegerMatchKey; Const aValue : TAdvStringIntegerMatchValue);

      Function GetAsText : String;
      Function GetKeysAsText : String;

      Function GetSensitive: Boolean;
      Procedure SetSensitive(Const Value: Boolean);

    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex: Integer; pValue: Pointer); Override;

      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;

      Function CompareKey(pA, pB : Pointer) : Integer; Virtual;
      Function CompareValue(pA, pB : Pointer) : Integer; Virtual;

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Overload; Override;

      Function Find(Const aKey : TAdvStringIntegerMatchKey; Const aValue : TAdvStringIntegerMatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil) : Boolean; Overload;

      Function CapacityLimit : Integer; Override;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvStringIntegerMatch; 
      Function Clone : TAdvStringIntegerMatch;

      Procedure AddAll(oStringIntegerMatch : TAdvStringIntegerMatch);

      Function IndexByKey(Const aKey : TAdvStringIntegerMatchKey) : Integer; 
      Function ExistsByKey(Const aKey : TAdvStringIntegerMatchKey) : Boolean;
      Function IndexByValue(Const aValue : TAdvStringIntegerMatchValue) : Integer;
      Function ExistsByValue(Const aValue : TAdvStringIntegerMatchValue) : Boolean;
      Function Add(Const aKey : TAdvStringIntegerMatchKey; Const aValue : TAdvStringIntegerMatchValue) : Integer; Overload;
      Function ForceByKey(Const aKey : TAdvStringIntegerMatchKey) : TAdvStringIntegerMatchValue;
      Procedure Insert(iIndex : Integer; Const aKey : TAdvStringIntegerMatchKey; Const aValue : TAdvStringIntegerMatchValue); Overload;

      Function GetValueByKey(Const aKey : TAdvStringIntegerMatchKey) : TAdvStringIntegerMatchValue;
      Function GetKeyByValue(Const aValue : TAdvStringIntegerMatchValue) : TAdvStringIntegerMatchKey;

      Function IsSortedByKey: Boolean;
      Function IsSortedByValue: Boolean;

      Procedure SortedByKey;
      Procedure SortedByValue;

      Property Matches[Const sIndex : TAdvStringIntegerMatchKey] : TAdvStringIntegerMatchValue Read GetMatch Write SetMatch; Default;
      Property KeyByIndex[iIndex : Integer] : TAdvStringIntegerMatchKey Read GetKey Write SetKey;
      Property ValueByIndex[iIndex : Integer] : TAdvStringIntegerMatchValue Read GetValue Write SetValue;
      Property Forced : Boolean Read FForced Write FForced;
      Property DefaultKey : TAdvStringIntegerMatchKey Read FDefaultKey Write FDefaultKey;
      Property DefaultValue : TAdvStringIntegerMatchValue Read FDefaultValue Write FDefaultValue;
      Property KeysAsText : String Read GetKeysAsText;
      Property AsText : String Read GetAsText;
      Property Symbol : String Read FSymbol Write FSymbol;
      Property Sensitive : Boolean Read GetSensitive Write SetSensitive;
  End;

  EAdvStringIntegerMatch = Class(EAdvException);



  TAdvOrdinalSetPart = Integer;
  PAdvOrdinalSetPart = ^TAdvOrdinalSetPart;
  TAdvOrdinalSetPartArray = Array[0..7] Of TAdvOrdinalSetPart;
  PAdvOrdinalSetPartArray = ^TAdvOrdinalSetPartArray;

  TAdvOrdinalSet = Class(TAdvCollection)
    Private
      FOwns : Boolean;
      FPartArray : PAdvOrdinalSetPartArray; // pointer to the block of memory associated with the set.
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
      Destructor Destroy; Override;

      Procedure Assign(oObject : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

      Function Iterator : TAdvIterator; Override;

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

      Property Parts : PAdvOrdinalSetPartArray Read FPartArray Write FPartArray;
      Property Owns : Boolean Read FOwns Write FOwns;
      Property Count : Integer Read FCount Write SetCount;
      Property Size : Integer Read FSize Write SetSize;
      Property IsChecked[Const iIndex : Integer] : Boolean Read GetIsChecked Write SetIsChecked; Default;
  End;

  TAdvOrdinalSetIterator = Class(TAdvIterator)
    Private
      FOrdinalSet : TAdvOrdinalSet;
      FValue : PAdvOrdinalSetPart;
      FPart : Integer;
      FLoop : Integer;
      FIndex : Integer;

      Procedure SetOrdinalSet(Const Value: TAdvOrdinalSet);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure First; Overload; Override;
      Procedure Next; Overload; Override;
      Function More : Boolean; Overload; Override;
      Function Checked : Boolean;

      Procedure Check;
      Procedure Uncheck;

      Property Index : Integer Read FIndex;
      Property OrdinalSet : TAdvOrdinalSet Read FOrdinalSet Write SetOrdinalSet;
  End;

  TAdvObjectMatchKey = TAdvObject;
  TAdvObjectMatchValue = TAdvObject;

  TAdvObjectMatchItem = Record
    Key : TAdvObjectMatchKey;
    Value : TAdvObjectMatchValue;
  End;

  PAdvObjectMatchItem = ^TAdvObjectMatchItem;

  TAdvObjectMatchItemArray = Array[0..(MaxInt Div SizeOf(TAdvObjectMatchItem)) - 1] Of TAdvObjectMatchItem;
  PAdvObjectMatchItemArray = ^TAdvObjectMatchItemArray;

  TAdvObjectMatch = Class(TAdvItemList)
    Private
      FMatchArray : PAdvObjectMatchItemArray;
      FDefaultKey : TAdvObjectMatchKey;
      FDefaultValue : TAdvObjectMatchValue;
      FForced : Boolean;
      FNominatedKeyClass : TAdvObjectClass;
      FNominatedValueClass : TAdvObjectClass;

      FKeyComparisonDelegate : TAdvItemListCompare;
      FValueComparisonDelegate : TAdvItemListCompare;

      Function GetKeyByIndex(iIndex: Integer): TAdvObjectMatchKey;
      Procedure SetKeyByIndex(iIndex: Integer; Const aKey : TAdvObjectMatchKey);

      Function GetValueByIndex(iIndex: Integer): TAdvObjectMatchValue;
      Procedure SetValueByIndex(iIndex: Integer; Const aValue: TAdvObjectMatchValue);

      Function GetMatchByIndex(iIndex : Integer) : TAdvObjectMatchItem;

      Function GetAsText : String;

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; aValue: Pointer); Override;

      Procedure LoadItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure SaveItem(oFiler : TAdvFiler; iIndex : Integer); Override;
      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function ValidateIndex(Const sMethod : String; iIndex : Integer): Boolean; Overload; Override;

      Function CapacityLimit : Integer; Override;

      Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Overload; Override;

      Function CompareByKeyReference(pA, pB : Pointer): Integer;
      Function CompareByValueReference(pA, pB : Pointer): Integer;

      Function Find(Const aKey : TAdvObjectMatchKey; Const aValue: TAdvObjectMatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare = Nil): Boolean; Overload;

      Function ValidateKey(Const sMethod : String; oObject : TAdvObject; Const sObject : String) : Boolean; Virtual;
      Function ValidateValue(Const sMethod : String; oObject : TAdvObject; Const sObject : String) : Boolean; Virtual;

      Function ItemKeyClass : TAdvObjectClass; Virtual;
      Function ItemValueClass : TAdvObjectClass; Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvObjectMatch; Overload;

      Function IndexByValue(Const aValue : TAdvObjectMatchValue) : Integer;
      Function IndexByKey(Const aKey : TAdvObjectMatchKey) : Integer;
      Function ExistsByValue(Const aValue : TAdvObjectMatchValue) : Boolean;
      Function ExistsByKey(Const aKey : TAdvObjectMatchKey) : Boolean;
      Function Add(Const aKey : TAdvObjectMatchKey; Const aValue : TAdvObjectMatchValue): Integer;
      Procedure Insert(iIndex : Integer; Const aKey : TAdvObjectMatchKey; Const aValue : TAdvObjectMatchValue);
      Procedure Delete(Const aKey : TAdvObjectMatchKey; Const aValue : TAdvObjectMatchValue);
      Function GetKeyByValue(Const aValue : TAdvObjectMatchValue) : TAdvObjectMatchKey;

      Function GetValueByKey(Const aKey : TAdvObjectMatchKey): TAdvObjectMatchValue;
      Procedure SetValueByKey(Const aKey: TAdvObjectMatchKey; Const aValue: TAdvObjectMatchValue);

      Procedure Merge(oMatch : TAdvObjectMatch);

      Procedure SortedByKey;
      Procedure SortedByValue;

      Function IsSortedByKey : Boolean;
      Function IsSortedByValue : Boolean;

      Property Forced : Boolean Read FForced Write FForced;
      Property DefaultKey : TAdvObjectMatchKey Read FDefaultKey Write FDefaultKey;
      Property DefaultValue : TAdvObjectMatchValue Read FDefaultValue Write FDefaultValue;
      Property KeyComparisonDelegate : TAdvItemListCompare Read FKeyComparisonDelegate Write FKeyComparisonDelegate;
      Property ValueComparisonDelegate : TAdvItemListCompare Read FValueComparisonDelegate Write FValueComparisonDelegate;
      Property MatchByIndex[iIndex : Integer] : TAdvObjectMatchItem Read GetMatchByIndex;
      Property KeyByIndex[iIndex : Integer] : TAdvObjectMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property ValueByIndex[iIndex : Integer] : TAdvObjectMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property Keys[iIndex : Integer] : TAdvObjectMatchKey Read GetKeyByIndex Write SetKeyByIndex;
      Property Values[iIndex : Integer] : TAdvObjectMatchValue Read GetValueByIndex Write SetValueByIndex;
      Property NominatedKeyClass : TAdvObjectClass Read FNominatedKeyClass Write FNominatedKeyClass;
      Property NominatedValueClass : TAdvObjectClass Read FNominatedValueClass Write FNominatedValueClass;
      Property AsText : String Read GetAsText;
  End;

  TAdvName = Class(TAdvPersistent)
    Private
      FName : String;

    Protected
      Function GetName: String; Virtual;
      Procedure SetName(Const Value: String); Virtual;

    Public
      Function Link : TAdvName;
      Function Clone : TAdvName;

      Procedure Assign(oSource : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

      Property Name : String Read GetName Write SetName;
  End;

  TAdvNameClass = Class Of TAdvName;

  TAdvNameList = Class(TAdvPersistentList)
    Private
      FSymbol : String;

      Function GetName(iIndex : Integer) : TAdvName;
      Procedure SetName(iIndex : Integer; oName : TAdvName);

      Function GetAsText: String;
      Procedure SetAsText(Const Value: String);

    Protected
      Function ItemClass : TAdvObjectClass; Override;

      Function CompareByName(pA, pB: Pointer): Integer; Virtual;

      Procedure DefaultCompare(Out aEvent : TAdvItemListCompare); Override;

      Function FindByName(Const sName: String; Out iIndex: Integer): Boolean; Overload;
      Function FindByName(oName : TAdvName; Out iIndex: Integer): Boolean; Overload;

    Public
      Constructor Create; Override;

      Function Link : TAdvNameList;
      Function Clone : TAdvNameList;

      Procedure SortedByName;
      Function IsSortedByName : Boolean;

      Function IndexByName(Const sName : String) : Integer; Overload;
      Function IndexByName(Const oName : TAdvName) : Integer; Overload;
      Function ExistsByName(Const oName : TAdvName) : Boolean; Overload;
      Function ExistsByName(Const sName : String) : Boolean; Overload;
      Function GetByName(Const sName : String) : TAdvName; Overload;
      Function GetByName(oName : TAdvName) : TAdvName; Overload;
      Function EnsureByName(Const sName : String) : TAdvName; Overload;
      Function ForceByName(Const sName : String) : TAdvName;
      Procedure RemoveByName(Const sName : String);
      Function AddByName(Const sName : String) : Integer;

      Property Names[iIndex : Integer] : TAdvName Read GetName Write SetName; Default;
      Property AsText : String Read GetAsText Write SetAsText;
      Property Symbol : String Read FSymbol Write FSymbol;
  End;



Implementation

uses
  FHIR.Support.Stream, FHIR.Support.Text;


Procedure TAdvCollection.BeforeDestruction;
Begin
  InternalClear;

  Inherited;
End;


Procedure TAdvCollection.Clear;
Begin 
  InternalClear;
End;  


Procedure TAdvCollection.InternalClear;
Begin
End;  


Function TAdvCollection.ErrorClass : EAdvExceptionClass;
Begin 
  Result := EAdvCollection;
End;  


Function TAdvCollection.Iterator : TAdvIterator;
Begin 
  RaiseError('Iterator', 'No iterator specified.');

  Result := Nil;
End;  


Function TAdvLargeIntegerMatch.Link : TAdvLargeIntegerMatch;
Begin
  Result := TAdvLargeIntegerMatch(Inherited Link);
End;


Function TAdvLargeIntegerMatch.CompareByKey(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(PAdvLargeIntegerMatchItem(pA)^.Key, PAdvLargeIntegerMatchItem(pB)^.Key);
End;


Function TAdvLargeIntegerMatch.CompareByValue(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(PAdvLargeIntegerMatchItem(pA)^.Value, PAdvLargeIntegerMatchItem(pB)^.Value);
End;  


Function TAdvLargeIntegerMatch.CompareByKeyValue(pA, pB: Pointer): Integer;
Begin
  Result := CompareByKey(pA, pB);

  If Result = 0 Then
    Result := CompareByValue(pA, pB);
End;


Procedure TAdvLargeIntegerMatch.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin 
  aCompare := {$IFDEF FPC}@{$ENDIF}CompareByKey;
End;  


Procedure TAdvLargeIntegerMatch.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Var
  iKey : TAdvLargeIntegerMatchKey;
  iValue : TAdvLargeIntegerMatchValue;
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineInteger(iKey);
  oFiler['Value'].DefineInteger(iValue);

  oFiler['Match'].DefineEnd;

  Add(iKey, iValue);
End;  


Procedure TAdvLargeIntegerMatch.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineInteger(FMatches^[iIndex].Key);
  oFiler['Value'].DefineInteger(FMatches^[iIndex].Value);

  oFiler['Match'].DefineEnd;
End;  


Procedure TAdvLargeIntegerMatch.AssignItem(oItems: TAdvItemList; iIndex: Integer);
Begin 
  Inherited;

  FMatches^[iIndex] := TAdvLargeIntegerMatch(oItems).FMatches^[iIndex];
End;  


Procedure TAdvLargeIntegerMatch.InternalEmpty(iIndex, iLength: Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatches) + NativeUInt(iIndex * SizeOf(TAdvLargeIntegerMatchItem))), (iLength * SizeOf(TAdvLargeIntegerMatchItem)));
End;  


Procedure TAdvLargeIntegerMatch.InternalResize(iValue : Integer);
Begin 
  Inherited;

  MemoryResize(FMatches, Capacity * SizeOf(TAdvLargeIntegerMatchItem), iValue * SizeOf(TAdvLargeIntegerMatchItem));
End;


Procedure TAdvLargeIntegerMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatches^[iSource], @FMatches^[iTarget], iCount * SizeOf(TAdvLargeIntegerMatchItem));
End;  


Function TAdvLargeIntegerMatch.IndexByKey(aKey : TAdvLargeIntegerMatchKey): Integer;
Begin 
  If Not FindByKey(aKey, Result, {$IFDEF FPC}@{$ENDIF}CompareByKey) Then
    Result := -1;
End;  


Function TAdvLargeIntegerMatch.IndexByKeyValue(Const aKey : TAdvLargeIntegerMatchKey; Const aValue : TAdvLargeIntegerMatchValue) : Integer;
Begin
  If Not Find(aKey, aValue, Result, {$IFDEF FPC}@{$ENDIF}CompareByKeyValue) Then
    Result := -1;
End;


Function TAdvLargeIntegerMatch.ExistsByKey(aKey : TAdvLargeIntegerMatchKey): Boolean;
Begin
  Result := ExistsByIndex(IndexByKey(aKey));
End;  


Function TAdvLargeIntegerMatch.ExistsByKeyValue(Const aKey : TAdvLargeIntegerMatchKey; Const aValue : TAdvLargeIntegerMatchValue) : Boolean;
Begin
  Result := ExistsByIndex(IndexByKeyValue(aKey, aValue));
End;


Function TAdvLargeIntegerMatch.Add(aKey : TAdvLargeIntegerMatchKey; aValue : TAdvLargeIntegerMatchValue): Integer;
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


Procedure TAdvLargeIntegerMatch.Insert(iIndex: Integer; iKey : TAdvLargeIntegerMatchKey; iValue : TAdvLargeIntegerMatchValue);
Begin 
  InternalInsert(iIndex);

  FMatches^[iIndex].Key := iKey;
  FMatches^[iIndex].Value := iValue;
End;  


Procedure TAdvLargeIntegerMatch.InternalInsert(iIndex: Integer);
Begin 
  Inherited;

  FMatches^[iIndex].Key := 0;
  FMatches^[iIndex].Value := 0;
End;  


Procedure TAdvLargeIntegerMatch.InternalExchange(iA, iB : Integer);
Var
  aTemp : TAdvLargeIntegerMatchItem;
  pA    : Pointer;
  pB    : Pointer;
Begin 
  pA := @FMatches^[iA];
  pB := @FMatches^[iB];

  aTemp := PAdvLargeIntegerMatchItem(pA)^;
  PAdvLargeIntegerMatchItem(pA)^ := PAdvLargeIntegerMatchItem(pB)^;
  PAdvLargeIntegerMatchItem(pB)^ := aTemp;
End;  


Function TAdvLargeIntegerMatch.GetItem(iIndex: Integer): Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatches^[iIndex];
End;  


Procedure TAdvLargeIntegerMatch.SetItem(iIndex: Integer; pValue: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatches^[iIndex] := PAdvLargeIntegerMatchItem(pValue)^;
End;  


Function TAdvLargeIntegerMatch.GetKey(iIndex: Integer): TAdvLargeIntegerMatchKey;
Begin 
  Assert(ValidateIndex('GetKey', iIndex));

  Result := FMatches^[iIndex].Key;
End;  


Procedure TAdvLargeIntegerMatch.SetKey(iIndex: Integer; Const iValue: TAdvLargeIntegerMatchKey);
Begin 
  Assert(ValidateIndex('SetKey', iIndex));

  FMatches^[iIndex].Key := iValue;
End;


Function TAdvLargeIntegerMatch.GetValue(iIndex: Integer): TAdvLargeIntegerMatchValue;
Begin 
  Assert(ValidateIndex('GetValue', iIndex));

  Result := FMatches^[iIndex].Value;
End;  


Procedure TAdvLargeIntegerMatch.SetValue(iIndex: Integer; Const iValue: TAdvLargeIntegerMatchValue);
Begin 
  Assert(ValidateIndex('SetValue', iIndex));

  FMatches^[iIndex].Value := iValue;
End;  


Function TAdvLargeIntegerMatch.GetPair(iIndex: Integer): TAdvLargeIntegerMatchItem;
Begin 
  Assert(ValidateIndex('GetPair', iIndex));

  Result := FMatches^[iIndex];
End;  


Procedure TAdvLargeIntegerMatch.SetPair(iIndex: Integer; Const Value: TAdvLargeIntegerMatchItem);
Begin 
  Assert(ValidateIndex('SetPair', iIndex));

  FMatches^[iIndex] := Value;
End;  


Function TAdvLargeIntegerMatch.GetMatch(iKey : TAdvLargeIntegerMatchKey): TAdvLargeIntegerMatchValue;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(iKey);

  If ExistsByIndex(iIndex) Then
    Result := Values[iIndex]
  Else
  Begin
    Result := FDefault;
    If Not FForced Then
      RaiseError('GetMatch', 'Unable to get the value for the specified key.');
  End;  
End;  


Procedure TAdvLargeIntegerMatch.SetMatch(iKey : TAdvLargeIntegerMatchKey; Const iValue: TAdvLargeIntegerMatchValue);
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(iKey);

  If ExistsByIndex(iIndex) Then
    Values[iIndex] := iValue
  Else If FForced Then
    Add(iKey, iValue)
  Else
    RaiseError('SetMatch', 'Unable to set the value for the specified key.');
End;  


Function TAdvLargeIntegerMatch.CapacityLimit : Integer;
Begin
  Result := High(TAdvLargeIntegerMatchItems);
End;


Function TAdvLargeIntegerMatch.Find(Const aKey: TAdvLargeIntegerMatchKey; Const aValue: TAdvLargeIntegerMatchValue; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Var
  aItem : TAdvLargeIntegerMatchItem;
Begin
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;


Function TAdvLargeIntegerMatch.FindByKey(Const aKey: TAdvLargeIntegerMatchKey; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Begin
  Result := Find(aKey, 0, iIndex, aCompare);
End;


Procedure TAdvLargeIntegerMatch.SortedByKey;
Begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByKey);
End;


Procedure TAdvLargeIntegerMatch.SortedByValue;
Begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByValue);
End;


Procedure TAdvLargeIntegerMatch.SortedByKeyValue;
Begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByKeyValue);
End;


Procedure TAdvLargeIntegerMatch.ForceIncrementByKey(Const aKey: TAdvLargeIntegerMatchKey);
Var
  iIndex : Integer;
Begin
  If Not FindByKey(aKey, iIndex) Then
    Insert(iIndex, aKey, 1)
  Else
    Values[iIndex] := Values[iIndex] + 1;
End;


Procedure TAdvLargeIntegerMatch.DeleteByKey(aKey: TAdvLargeIntegerMatchKey);
Begin
  DeleteByIndex(IndexByKey(aKey));
End;


Function TAdvLargeIntegerMatch.EqualTo(Const oIntegerMatch : TAdvLargeIntegerMatch) : Boolean;
Var
  aPair : TAdvLargeIntegerMatchItem;
  iIndex : Integer;
Begin
  Result := oIntegerMatch.Count = Count;

  iIndex := 0;

  While Result And ExistsByIndex(iIndex) Do
  Begin
    aPair := Pairs[iIndex];

    Result := oIntegerMatch.ExistsByKeyValue(aPair.Key, aPair.Value);

    Inc(iIndex);
  End;
End;



Constructor TAdvIntegerObjectMatch.Create;
Begin
  Inherited;

  FNominatedValueClass := ItemClass;
End;


Destructor TAdvIntegerObjectMatch.Destroy;
Begin
  Inherited;
End;


Function TAdvIntegerObjectMatch.Link : TAdvIntegerObjectMatch;
Begin
  Result := TAdvIntegerObjectMatch(Inherited Link);
End;


Function TAdvIntegerObjectMatch.CompareByKey(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(PAdvIntegerObjectMatchItem(pA)^.Key, PAdvIntegerObjectMatchItem(pB)^.Key);
End;


Function TAdvIntegerObjectMatch.CompareByValue(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(Integer(PAdvIntegerObjectMatchItem(pA)^.Value), Integer(PAdvIntegerObjectMatchItem(pB)^.Value));
End;  


Procedure TAdvIntegerObjectMatch.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineInteger(FItemArray^[iIndex].Key);
  oFiler['Value'].DefineObject(FItemArray^[iIndex].Value);

  oFiler['Match'].DefineEnd;
End;  


Procedure TAdvIntegerObjectMatch.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Var
  iKey : TAdvIntegerObjectMatchKey;
  oValue : TAdvIntegerObjectMatchValue;
Begin 
  oValue := Nil;
  Try
    oFiler['Match'].DefineBegin;

    oFiler['Key'].DefineInteger(iKey);
    oFiler['Value'].DefineObject(oValue);

    oFiler['Match'].DefineEnd;

    Add(iKey, oValue.Link);
  Finally
    oValue.Free;
  End;  
End;


Procedure TAdvIntegerObjectMatch.AssignItem(oItems : TAdvItemList; iIndex: Integer);
Begin
  Inherited;

  FItemArray^[iIndex].Key := TAdvIntegerObjectMatch(oItems).FItemArray^[iIndex].Key;
  FItemArray^[iIndex].Value := TAdvIntegerObjectMatch(oItems).FItemArray^[iIndex].Value.Clone;
End;  


Procedure TAdvIntegerObjectMatch.InternalEmpty(iIndex, iLength : Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FItemArray) + (iIndex * SizeOf(TAdvIntegerObjectMatchItem))), (iLength * SizeOf(TAdvIntegerObjectMatchItem)));
End;  


Procedure TAdvIntegerObjectMatch.InternalTruncate(iValue : Integer);
Var
  iLoop : Integer;
  oValue : TAdvObject;
Begin
  Inherited;

  For iLoop := iValue To Count - 1 Do
  Begin
    oValue := FItemArray^[iLoop].Value;
    FItemArray^[iLoop].Value := Nil;
    oValue.Free;
  End;
End;


Procedure TAdvIntegerObjectMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FItemArray, Capacity * SizeOf(TAdvIntegerObjectMatchItem), iValue * SizeOf(TAdvIntegerObjectMatchItem));
End;


Procedure TAdvIntegerObjectMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FItemArray^[iSource], @FItemArray^[iTarget], iCount * SizeOf(TAdvIntegerObjectMatchItem));
End;  


Function TAdvIntegerObjectMatch.IndexByKey(Const aKey : TAdvIntegerObjectMatchKey): Integer;
Begin 
  If Not Find(aKey, Nil, Result, CompareByKey) Then
    Result := -1;
End;  


Function TAdvIntegerObjectMatch.IndexByValue(Const aValue : TAdvIntegerObjectMatchValue): Integer;
Begin
  If Not Find(0, aValue, Result, CompareByValue) Then
    Result := -1;
End;  


Function TAdvIntegerObjectMatch.ExistsByKey(Const aKey : TAdvIntegerObjectMatchKey): Boolean;
Begin
  Result := ExistsByIndex(IndexByKey(aKey));
End;


Function TAdvIntegerObjectMatch.ExistsByValue(Const aValue : TAdvIntegerObjectMatchValue): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(aValue));
End;  


Function TAdvIntegerObjectMatch.Add(Const aKey : TAdvIntegerObjectMatchKey; Const aValue : TAdvIntegerObjectMatchValue) : Integer;
Begin
  Assert(ValidateValue('Add', aValue, 'aValue'));

  Result := -1;

  If Not IsAllowDuplicates And Find(aKey, aValue, Result) Then
  Begin
    aValue.Free; // free ignored object

    If IsPreventDuplicates Then
      RaiseError('Add', StringFormat('Item already exists in list (%d=%x)', [aKey, Integer(aValue)]));

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


Procedure TAdvIntegerObjectMatch.AddAll(Const oIntegerObjectMatch : TAdvIntegerObjectMatch);
Var
  iIntegerObjectMatchIndex : Integer;
Begin
  Assert(Invariants('AddAll', oIntegerObjectMatch, TAdvIntegerObjectMatch, 'oIntegerObjectMatch'));

  For iIntegerObjectMatchIndex := 0 To oIntegerObjectMatch.Count - 1 Do
    Add(oIntegerObjectMatch.KeyByIndex[iIntegerObjectMatchIndex], oIntegerObjectMatch.ValueByIndex[iIntegerObjectMatchIndex].Link);
End;


Procedure TAdvIntegerObjectMatch.Insert(iIndex : Integer; Const aKey : TAdvIntegerObjectMatchKey; Const aValue : TAdvIntegerObjectMatchValue);
Begin
  Assert(ValidateValue('Insert', aValue, 'aValue'));

  InternalInsert(iIndex);

  FItemArray^[iIndex].Key := aKey;
  FItemArray^[iIndex].Value := aValue;
End;  


Procedure TAdvIntegerObjectMatch.InternalInsert(iIndex : Integer);
Begin 
  Inherited;

  Integer(FItemArray^[iIndex].Key) := 0;
  Pointer(FItemArray^[iIndex].Value) := Nil;
End;  


Procedure TAdvIntegerObjectMatch.InternalDelete(iIndex : Integer);
Begin 
  Inherited;

  FItemArray^[iIndex].Value.Free;
  FItemArray^[iIndex].Value := Nil;
End;  


Procedure TAdvIntegerObjectMatch.InternalExchange(iA, iB: Integer);
Var
  aTemp : TAdvIntegerObjectMatchItem;
  pA    : PAdvIntegerObjectMatchItem;
  pB    : PAdvIntegerObjectMatchItem;
Begin 
  pA := @FItemArray^[iA];
  pB := @FItemArray^[iB];

  aTemp := pA^;
  pA^ := pB^;
  pB^ := aTemp;
End;  


Function TAdvIntegerObjectMatch.GetItem(iIndex : Integer) : Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FItemArray^[iIndex];
End;  


Procedure TAdvIntegerObjectMatch.SetItem(iIndex: Integer; pValue: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FItemArray^[iIndex] := PAdvIntegerObjectMatchItem(pValue)^;
End;  


Function TAdvIntegerObjectMatch.GetKeyByIndex(iIndex : Integer): TAdvIntegerObjectMatchKey;
Begin
  Assert(ValidateIndex('GetKeyByIndex', iIndex));

  Result := FItemArray^[iIndex].Key;
End;  


Procedure TAdvIntegerObjectMatch.SetKeyByIndex(iIndex : Integer; Const aKey : TAdvIntegerObjectMatchKey);
Begin
  Assert(ValidateIndex('SetKeyByIndex', iIndex));

  FItemArray^[iIndex].Key := aKey;
End;


Function TAdvIntegerObjectMatch.GetValueByIndex(iIndex : Integer): TAdvIntegerObjectMatchValue;
Begin
  Assert(ValidateIndex('GetValueByIndex', iIndex));

  Result := FItemArray^[iIndex].Value;
End;


Procedure TAdvIntegerObjectMatch.SetValueByIndex(iIndex : Integer; Const aValue: TAdvIntegerObjectMatchValue);
Begin
  Assert(ValidateIndex('SetValueByIndex', iIndex));
  Assert(ValidateValue('SetValueByIndex', aValue, 'aValue'));

  FItemArray^[iIndex].Value.Free;
  FItemArray^[iIndex].Value := aValue;
End;


Function TAdvIntegerObjectMatch.GetKeyByValue(Const aValue: TAdvIntegerObjectMatchValue): TAdvIntegerObjectMatchKey;
Var
  iIndex : Integer;
Begin
  iIndex := IndexByValue(aValue);

  If ExistsByIndex(iIndex) Then
    Result := KeyByIndex[iIndex]
  Else If Forced Then
    Result := DefaultKey
  Else
  Begin
    RaiseError('GetKeyByValue', 'Could not find key by value.');
    Result := 0;
  End;
End;


Function TAdvIntegerObjectMatch.GetValueByKey(Const aKey : TAdvIntegerObjectMatchKey): TAdvIntegerObjectMatchValue;
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
    RaiseError('GetValueByKey', StringFormat('Unable to get the value for the specified key ''%d''.', [aKey]));
    Result := Nil;
  End;
End;


Procedure TAdvIntegerObjectMatch.SetValueByKey(Const aKey : TAdvIntegerObjectMatchKey; Const aValue : TAdvIntegerObjectMatchValue);
Var
  iIndex : Integer;
Begin
  Assert(ValidateValue('SetValueByKey', aValue, 'aValue'));

  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    ValueByIndex[iIndex] := aValue
  Else If FForced Then
    Add(aKey, aValue)
  Else
    RaiseError('SetValueByKey', 'Unable to set the value for the specified key.');
End;


Function TAdvIntegerObjectMatch.CapacityLimit : Integer;
Begin
  Result := High(TAdvIntegerObjectMatchItemArray);
End;  


Procedure TAdvIntegerObjectMatch.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin
  aCompare := CompareByKey;
End;  


Function TAdvIntegerObjectMatch.Find(Const aKey: TAdvIntegerObjectMatchKey; Const aValue: TAdvIntegerObjectMatchValue; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Var
  aItem : TAdvIntegerObjectMatchItem;
Begin 
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;  


Procedure TAdvIntegerObjectMatch.SortedByKey;
Begin 
  SortedBy(CompareByKey);
End;  


Procedure TAdvIntegerObjectMatch.SortedByValue;
Begin 
  SortedBy(CompareByValue);
End;  


Function TAdvIntegerObjectMatch.ItemClass : TAdvObjectClass;
Begin 
  Result := TAdvObject;
End;  


Function TAdvIntegerObjectMatch.ValidateIndex(Const sMethod : String; iIndex: Integer): Boolean;
Begin
  Inherited ValidateIndex(sMethod, iIndex);

  ValidateValue(sMethod, FItemArray^[iIndex].Value, 'FItemArray^[' + IntegerToString(iIndex) + '].Value');

  Result := True;
End;  


Function TAdvIntegerObjectMatch.ValidateValue(Const sMethod : String; oObject : TAdvObject; Const sObject : String) : Boolean;
Begin
  If Assigned(oObject) Then
    Invariants(sMethod, oObject, FNominatedValueClass, sObject);

  Result := True;
End;  


Function TAdvIntegerObjectMatch.IsSortedByKey : Boolean;
Begin
  Result := IsSortedBy(CompareByKey);
End;  


Function TAdvIntegerObjectMatch.IsSortedByValue : Boolean;
Begin 
  Result := IsSortedBy(CompareByValue);
End;


Procedure TAdvIntegerObjectMatch.DeleteByKey(Const aKey: TAdvIntegerObjectMatchKey);
Begin
  DeleteByIndex(IndexByKey(aKey));
End;


Procedure TAdvIntegerObjectMatch.DeleteByValue(Const aValue: TAdvIntegerObjectMatchValue);
Begin
  DeleteByIndex(IndexByValue(aValue));
End;


Function TAdvIntegerObjectMatch.GetMatchByIndex(iIndex: Integer): TAdvIntegerObjectMatchItem;
Begin
  Assert(ValidateIndex('GetMatchByIndex', iIndex));

  Result := FItemArray^[iIndex];
End;


Function TAdvIntegerMatch.Link : TAdvIntegerMatch;
Begin
  Result := TAdvIntegerMatch(Inherited Link);
End;


Function TAdvIntegerMatch.CompareByKey(pA, pB: Pointer): Integer;
Begin 
  Result := IntegerCompare(PAdvInteger32MatchItem(pA)^.Key, PAdvInteger32MatchItem(pB)^.Key);
End;


Function TAdvIntegerMatch.CompareByValue(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(PAdvInteger32MatchItem(pA)^.Value, PAdvInteger32MatchItem(pB)^.Value);
End;  


Function TAdvIntegerMatch.CompareByKeyValue(pA, pB: Pointer): Integer;
Begin
  Result := CompareByKey(pA, pB);

  If Result = 0 Then
    Result := CompareByValue(pA, pB);
End;


Procedure TAdvIntegerMatch.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin 
  aCompare := {$IFDEF FPC}@{$ENDIF}CompareByKey;
End;  


Procedure TAdvIntegerMatch.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Var
  iKey : TAdvIntegerMatchKey;
  iValue : TAdvIntegerMatchValue;
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineInteger(iKey);
  oFiler['Value'].DefineInteger(iValue);

  oFiler['Match'].DefineEnd;

  Add(iKey, iValue);
End;  


Procedure TAdvIntegerMatch.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineInteger(FMatches^[iIndex].Key);
  oFiler['Value'].DefineInteger(FMatches^[iIndex].Value);

  oFiler['Match'].DefineEnd;
End;  


Procedure TAdvIntegerMatch.AssignItem(oItems: TAdvItemList; iIndex: Integer);
Begin 
  Inherited;

  FMatches^[iIndex] := TAdvIntegerMatch(oItems).FMatches^[iIndex];
End;  


Procedure TAdvIntegerMatch.InternalEmpty(iIndex, iLength: Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatches) + NativeUInt(iIndex * SizeOf(TAdvIntegerMatchItem))), (iLength * SizeOf(TAdvIntegerMatchItem)));
End;  


Procedure TAdvIntegerMatch.InternalResize(iValue : Integer);
Begin 
  Inherited;

  MemoryResize(FMatches, Capacity * SizeOf(TAdvIntegerMatchItem), iValue * SizeOf(TAdvIntegerMatchItem));
End;  


Procedure TAdvIntegerMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatches^[iSource], @FMatches^[iTarget], iCount * SizeOf(TAdvIntegerMatchItem));
End;  


Function TAdvIntegerMatch.IndexByKey(aKey : TAdvIntegerMatchKey): Integer;
Begin
  If Not FindByKey(aKey, Result, {$IFDEF FPC}@{$ENDIF}CompareByKey) Then
    Result := -1;
End;  


Function TAdvIntegerMatch.IndexByKeyValue(Const aKey : TAdvIntegerMatchKey; Const aValue : TAdvIntegerMatchValue) : Integer;
Begin
  If Not Find(aKey, aValue, Result, {$IFDEF FPC}@{$ENDIF}CompareByKeyValue) Then
    Result := -1;
End;


Function TAdvIntegerMatch.ExistsByKey(aKey : TAdvIntegerMatchKey): Boolean;
Begin
  Result := ExistsByIndex(IndexByKey(aKey));
End;


Function TAdvIntegerMatch.ExistsByKeyValue(Const aKey : TAdvIntegerMatchKey; Const aValue : TAdvIntegerMatchValue) : Boolean;
Begin
  Result := ExistsByIndex(IndexByKeyValue(aKey, aValue));
End;


Function TAdvIntegerMatch.Add(aKey : TAdvIntegerMatchKey; aValue : TAdvIntegerMatchValue): Integer;
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


Procedure TAdvIntegerMatch.Insert(iIndex: Integer; iKey : TAdvIntegerMatchKey; iValue : TAdvIntegerMatchValue);
Begin 
  InternalInsert(iIndex);

  FMatches^[iIndex].Key := iKey;
  FMatches^[iIndex].Value := iValue;
End;  


Procedure TAdvIntegerMatch.InternalInsert(iIndex: Integer);
Begin 
  Inherited;

  FMatches^[iIndex].Key := 0;
  FMatches^[iIndex].Value := 0;
End;  


Procedure TAdvIntegerMatch.InternalExchange(iA, iB : Integer);
Var
  aTemp : TAdvIntegerMatchItem;
  pA    : Pointer;
  pB    : Pointer;
Begin 
  pA := @FMatches^[iA];
  pB := @FMatches^[iB];

  aTemp := PAdvInteger32MatchItem(pA)^;
  PAdvInteger32MatchItem(pA)^ := PAdvInteger32MatchItem(pB)^;
  PAdvInteger32MatchItem(pB)^ := aTemp;
End;  


Function TAdvIntegerMatch.GetItem(iIndex: Integer): Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatches^[iIndex];
End;  


Procedure TAdvIntegerMatch.SetItem(iIndex: Integer; pValue: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatches^[iIndex] := PAdvInteger32MatchItem(pValue)^;
End;  


Function TAdvIntegerMatch.GetKey(iIndex: Integer): TAdvIntegerMatchKey;
Begin 
  Assert(ValidateIndex('GetKey', iIndex));

  Result := FMatches^[iIndex].Key;
End;  


Procedure TAdvIntegerMatch.SetKey(iIndex: Integer; Const iValue: TAdvIntegerMatchKey);
Begin 
  Assert(ValidateIndex('SetKey', iIndex));

  FMatches^[iIndex].Key := iValue;
End;  


Function TAdvIntegerMatch.GetValue(iIndex: Integer): TAdvIntegerMatchValue;
Begin 
  Assert(ValidateIndex('GetValue', iIndex));

  Result := FMatches^[iIndex].Value;
End;  


Procedure TAdvIntegerMatch.SetValue(iIndex: Integer; Const iValue: TAdvIntegerMatchValue);
Begin 
  Assert(ValidateIndex('SetValue', iIndex));

  FMatches^[iIndex].Value := iValue;
End;  


Function TAdvIntegerMatch.GetPair(iIndex: Integer): TAdvIntegerMatchItem;
Begin 
  Assert(ValidateIndex('GetPair', iIndex));

  Result := FMatches^[iIndex];
End;  


Procedure TAdvIntegerMatch.SetPair(iIndex: Integer; Const Value: TAdvIntegerMatchItem);
Begin 
  Assert(ValidateIndex('SetPair', iIndex));

  FMatches^[iIndex] := Value;
End;  


Function TAdvIntegerMatch.GetMatch(iKey : TAdvIntegerMatchKey): TAdvIntegerMatchValue;
Var
  iIndex : Integer;
Begin
  iIndex := IndexByKey(iKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else
  Begin 
    Result := FDefault;
    If Not FForced Then
      RaiseError('GetMatch', 'Unable to get the value for the specified key.');
  End;  
End;  


Procedure TAdvIntegerMatch.SetMatch(iKey : TAdvIntegerMatchKey; Const iValue: TAdvIntegerMatchValue);
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(iKey);

  If ExistsByIndex(iIndex) Then
    ValueByIndex[iIndex] := iValue
  Else If FForced Then
    Add(iKey, iValue)
  Else
    RaiseError('SetMatch', 'Unable to set the value for the specified key.');
End;  


Function TAdvIntegerMatch.CapacityLimit : Integer;
Begin
  Result := High(TAdvIntegerMatchItems);
End;


Function TAdvIntegerMatch.Find(Const aKey: TAdvIntegerMatchKey; Const aValue: TAdvIntegerMatchValue; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Var
  aItem : TAdvIntegerMatchItem;
Begin
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;


Function TAdvIntegerMatch.FindByKey(Const aKey: TAdvIntegerMatchKey; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Begin
  Result := Find(aKey, 0, iIndex, aCompare);
End;


Procedure TAdvIntegerMatch.SortedByKey;
Begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByKey);
End;


Procedure TAdvIntegerMatch.SortedByValue;
Begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByValue);
End;


Procedure TAdvIntegerMatch.SortedByKeyValue;
Begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByKeyValue);
End;


Procedure TAdvIntegerMatch.ForceIncrementByKey(Const aKey: TAdvIntegerMatchKey);
Var
  iIndex : Integer;
Begin
  If Not FindByKey(aKey, iIndex) Then
    Insert(iIndex, aKey, 1)
  Else
    ValueByIndex[iIndex] := ValueByIndex[iIndex] + 1;
End;


Procedure TAdvIntegerMatch.DeleteByKey(aKey: TAdvIntegerMatchKey);
Begin
  DeleteByIndex(IndexByKey(aKey));
End;


Function TAdvIntegerMatch.EqualTo(Const oIntegerMatch : TAdvIntegerMatch) : Boolean;
Var
  aPair : TAdvIntegerMatchItem;
  iIndex : Integer;
Begin
  Result := oIntegerMatch.Count = Count;

  iIndex := 0;

  While Result And ExistsByIndex(iIndex) Do
  Begin
    aPair := Pairs[iIndex];

    Result := oIntegerMatch.ExistsByKeyValue(aPair.Key, aPair.Value);

    Inc(iIndex);
  End;
End;


Procedure TAdvIntegerList.SaveItem(oFiler : TAdvFiler; iIndex : Integer);
Begin 
  oFiler['Integer'].DefineInteger(FIntegerArray^[iIndex]);
End;  


Procedure TAdvIntegerList.LoadItem(oFiler : TAdvFiler; iIndex : Integer);
Var
  iValue : TAdvIntegerListItem;
Begin
  oFiler['Integer'].DefineInteger(iValue);

  Add(iValue);
End;  


Procedure TAdvIntegerList.AssignItem(oItems : TAdvItemList; iIndex : Integer);
Begin 
  FIntegerArray^[iIndex] := TAdvIntegerList(oItems).FIntegerArray^[iIndex];
End;  


Function TAdvIntegerList.Clone: TAdvIntegerList;
Begin 
  Result := TAdvIntegerList(Inherited Clone);
End;


Function TAdvIntegerList.Link: TAdvIntegerList;
Begin 
  Result := TAdvIntegerList(Inherited Link);
End;  


Procedure TAdvIntegerList.InternalEmpty(iIndex, iLength : Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(NativeUInt(FIntegerArray) + (iIndex * SizeOf(TAdvIntegerListItem))), (iLength * SizeOf(TAdvIntegerListItem)));
End;  


Procedure TAdvIntegerList.InternalResize(iCapacity : Integer);
Begin 
  Inherited;
  
  MemoryResize(FIntegerArray, Capacity * SizeOf(TAdvIntegerListItem), iCapacity * SizeOf(TAdvIntegerListItem));
End;


Procedure TAdvIntegerList.InternalCopy(iSource, iTarget, iCount : Integer);
Begin 
  Inherited;

  MemoryMove(@FIntegerArray^[iSource], @FIntegerArray^[iTarget], iCount * SizeOf(TAdvIntegerListItem));
End;  


Function TAdvIntegerList.IndexByValue(Const iValue : TAdvIntegerListItem): Integer;
Begin
  If Not Find(Pointer(iValue), Result) Then
    Result := -1;
End;


Function TAdvIntegerList.ExistsByValue(Const iValue : TAdvIntegerListItem) : Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(iValue));
End;


Function TAdvIntegerList.Add(aValue : TAdvIntegerListItem): Integer;
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


Procedure TAdvIntegerList.Insert(iIndex : Integer; aValue : TAdvIntegerListItem);
Begin 
  InternalInsert(iIndex);

  FIntegerArray^[iIndex] := aValue;
End;


Procedure TAdvIntegerList.DeleteByValue(Const iValue: TAdvIntegerListItem);
Var
  iIndex : Integer;
Begin
  If Not Find(Pointer(iValue), iIndex) Then
    RaiseError('DeleteByValue', StringFormat('''%d'' not found in list', [iValue]));

  DeleteByIndex(iIndex);
End;


Procedure TAdvIntegerList.DeleteAllByValue(oIntegers: TAdvIntegerList);
Var
  iIndex : Integer;
Begin
  For iIndex := 0 To oIntegers.Count - 1 Do
    DeleteByValue(oIntegers[iIndex]);
End;


Procedure TAdvIntegerList.InternalExchange(iA, iB : Integer);
Var
  aTemp : TAdvIntegerListItem;
  pA    : Pointer;
  pB    : Pointer;
Begin 
  pA := @FIntegerArray^[iA];
  pB := @FIntegerArray^[iB];

  aTemp := TAdvIntegerListItem(pA^);
  TAdvIntegerListItem(pA^) := TAdvIntegerListItem(pB^);
  TAdvIntegerListItem(pB^) := aTemp;
End;  


Procedure TAdvIntegerList.AddAll(oIntegers: TAdvIntegerList);
Var
  iLoop : Integer;
Begin 
  For iLoop := 0 To oIntegers.Count - 1 Do
    Add(oIntegers[iLoop]);
End;


Function TAdvIntegerList.EqualTo(oIntegers : TAdvIntegerList) : Boolean;
Var
  iLoop : Integer;
  iCount : Integer;
Begin 
  Assert(Invariants('EqualTo', oIntegers, TAdvIntegerList, 'oIntegers'));

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


Procedure TAdvIntegerList.Toggle(aValue : TAdvIntegerListItem);
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByValue(aValue);

  If ExistsByIndex(iIndex) Then
    DeleteByIndex(iIndex)
  Else
    Add(aValue);
End;  


Function TAdvIntegerList.GetAsText : String;
Var
  iIndex : Integer;
Begin
  Result := '';

  For iIndex := 0 To Count-1 Do
    StringAppend(Result, IntegerToString(IntegerByIndex[iIndex]), ', ');
End;


Function TAdvIntegerList.GetItem(iIndex : Integer): Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := Pointer(FIntegerArray^[iIndex]);
End;  


Procedure TAdvIntegerList.SetItem(iIndex : Integer; pValue : Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FIntegerArray^[iIndex] := TAdvIntegerListItem(pValue);
End;  


Function TAdvIntegerList.GetIntegerByIndex(iIndex : Integer): TAdvIntegerListItem;
Begin
  Assert(ValidateIndex('GetIntegerByIndex', iIndex));

  Result := FIntegerArray^[iIndex];
End;  


Procedure TAdvIntegerList.SetIntegerByIndex(iIndex : Integer; Const aValue : TAdvIntegerListItem);
Begin
  Assert(ValidateIndex('SetIntegerByIndex', iIndex));

  FIntegerArray^[iIndex] := aValue;
End;  


Function TAdvIntegerList.Sum : Int64;
Var
  iLoop : Integer;
Begin 
  Result := 0;
  For iLoop := 0 To Count - 1 Do
    Inc(Result, IntegerByIndex[iLoop]);
End;  


Function TAdvIntegerList.Mean : TAdvIntegerListItem;
Begin 
  If Count > 0 Then
    Result := Sum Div Count
  Else
    Result := 0;
End;  


Function TAdvIntegerList.CapacityLimit : Integer;
Begin
  Result := High(TAdvIntegerListItemArray);
End;  


Function TAdvIntegerList.Iterator : TAdvIterator;
Begin 
  Result := TAdvIntegerListIterator.Create;
  TAdvIntegerListIterator(Result).IntegerList := TAdvIntegerList(Self.Link);
End;


Constructor TAdvIntegerListIterator.Create;
Begin 
  Inherited;

  FIntegerList := Nil;
End;


Destructor TAdvIntegerListIterator.Destroy;
Begin
  FIntegerList.Free;

  Inherited;
End;


Procedure TAdvIntegerListIterator.First;
Begin
  Inherited;

  FIndex := 0;
End;


Procedure TAdvIntegerListIterator.Last;
Begin
  Inherited;

  FIndex := FIntegerList.Count - 1;
End;


Procedure TAdvIntegerListIterator.Next;
Begin
  Inherited;

  Inc(FIndex);
End;  


Procedure TAdvIntegerListIterator.Back;
Begin
  Inherited;

  Dec(FIndex);
End;  


Function TAdvIntegerListIterator.Current : Integer;
Begin
  Result := FIntegerList[FIndex];
End;  


Function TAdvIntegerListIterator.More : Boolean;
Begin 
  Result := FIntegerList.ExistsByIndex(FIndex);
End;  


Procedure TAdvIntegerListIterator.SetIntegerList(Const Value : TAdvIntegerList);
Begin
  FIntegerList.Free;
  FIntegerList := Value;
End;


Function TAdvIntegerList.GetIntegers(iIndex : Integer) : TAdvIntegerListItem;
Begin
  Result := IntegerByIndex[iIndex];
End;


Procedure TAdvIntegerList.SetIntegers(iIndex : Integer; Const Value : TAdvIntegerListItem);
Begin
  IntegerByIndex[iIndex] := Value;
End;


Function TAdvIntegerList.ExistsAll(oIntegerList : TAdvIntegerList) : Boolean;
Var
  iIndex : Integer;
Begin
  Assert(Invariants('ExistsAll', oIntegerList, TAdvIntegerList, 'oIntegerList'));

  Result := True;

  iIndex := 0;

  While Result And oIntegerList.ExistsByIndex(iIndex) Do
  Begin
    Result := ExistsByValue(oIntegerList[iIndex]);

    Inc(iIndex);
  End;
End;


Function TAdvInt64Match.Link : TAdvInt64Match;
Begin
  Result := TAdvInt64Match(Inherited Link);
End;


Function TAdvInt64Match.CompareByKey(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(PAdvInteger32MatchItem(pA)^.Key, PAdvInteger32MatchItem(pB)^.Key);
End;


Function TAdvInt64Match.CompareByValue(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(PAdvInteger32MatchItem(pA)^.Value, PAdvInteger32MatchItem(pB)^.Value);
End;  


Function TAdvInt64Match.CompareByKeyValue(pA, pB: Pointer): Integer;
Begin
  Result := CompareByKey(pA, pB);

  If Result = 0 Then
    Result := CompareByValue(pA, pB);
End;


Procedure TAdvInt64Match.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin 
  aCompare := {$IFDEF FPC}@{$ENDIF}CompareByKey;
End;  


Procedure TAdvInt64Match.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Var
  iKey : TAdvInt64MatchKey;
  iValue : TAdvInt64MatchValue;
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineInteger(iKey);
  oFiler['Value'].DefineInteger(iValue);

  oFiler['Match'].DefineEnd;

  Add(iKey, iValue);
End;  


Procedure TAdvInt64Match.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineInteger(FMatches^[iIndex].Key);
  oFiler['Value'].DefineInteger(FMatches^[iIndex].Value);

  oFiler['Match'].DefineEnd;
End;  


Procedure TAdvInt64Match.AssignItem(oItems: TAdvItemList; iIndex: Integer);
Begin 
  Inherited;

  FMatches^[iIndex] := TAdvInt64Match(oItems).FMatches^[iIndex];
End;  


Procedure TAdvInt64Match.InternalEmpty(iIndex, iLength: Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatches) + (iIndex * SizeOf(TAdvInt64MatchItem))), (iLength * SizeOf(TAdvInt64MatchItem)));
End;  


Procedure TAdvInt64Match.InternalResize(iValue : Integer);
Begin 
  Inherited;

  MemoryResize(FMatches, Capacity * SizeOf(TAdvInt64MatchItem), iValue * SizeOf(TAdvInt64MatchItem));
End;  


Procedure TAdvInt64Match.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatches^[iSource], @FMatches^[iTarget], iCount * SizeOf(TAdvInt64MatchItem));
End;  


Function TAdvInt64Match.IndexByKey(aKey : TAdvInt64MatchKey): Integer;
Begin 
  If Not FindByKey(aKey, Result, {$IFDEF FPC}@{$ENDIF}CompareByKey) Then
    Result := -1;
End;  


Function TAdvInt64Match.IndexByKeyValue(Const aKey : TAdvInt64MatchKey; Const aValue : TAdvInt64MatchValue) : Integer;
Begin
  If Not Find(aKey, aValue, Result, {$IFDEF FPC}@{$ENDIF}CompareByKeyValue) Then
    Result := -1;
End;


Function TAdvInt64Match.ExistsByKey(aKey : TAdvInt64MatchKey): Boolean;
Begin
  Result := ExistsByIndex(IndexByKey(aKey));
End;  


Function TAdvInt64Match.ExistsByKeyValue(Const aKey : TAdvInt64MatchKey; Const aValue : TAdvInt64MatchValue) : Boolean;
Begin
  Result := ExistsByIndex(IndexByKeyValue(aKey, aValue));
End;


Function TAdvInt64Match.Add(aKey : TAdvInt64MatchKey; aValue : TAdvInt64MatchValue): Integer;
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


Procedure TAdvInt64Match.Insert(iIndex: Integer; iKey : TAdvInt64MatchKey; iValue : TAdvInt64MatchValue);
Begin 
  InternalInsert(iIndex);

  FMatches^[iIndex].Key := iKey;
  FMatches^[iIndex].Value := iValue;
End;  


Procedure TAdvInt64Match.InternalInsert(iIndex: Integer);
Begin 
  Inherited;

  FMatches^[iIndex].Key := 0;
  FMatches^[iIndex].Value := 0;
End;


Procedure TAdvInt64Match.InternalExchange(iA, iB : Integer);
Var
  aTemp : TAdvInt64MatchItem;
  pA    : Pointer;
  pB    : Pointer;
Begin 
  pA := @FMatches^[iA];
  pB := @FMatches^[iB];

  aTemp := PAdvInteger64MatchItem(pA)^;
  PAdvInteger64MatchItem(pA)^ := PAdvInteger64MatchItem(pB)^;
  PAdvInteger64MatchItem(pB)^ := aTemp;
End;  


Function TAdvInt64Match.GetItem(iIndex: Integer): Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatches^[iIndex];
End;  


Procedure TAdvInt64Match.SetItem(iIndex: Integer; pValue: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatches^[iIndex] := PAdvInteger64MatchItem(pValue)^;
End;  


Function TAdvInt64Match.GetKey(iIndex: Integer): TAdvInt64MatchKey;
Begin 
  Assert(ValidateIndex('GetKey', iIndex));

  Result := FMatches^[iIndex].Key;
End;  


Procedure TAdvInt64Match.SetKey(iIndex: Integer; Const iValue: TAdvInt64MatchKey);
Begin 
  Assert(ValidateIndex('SetKey', iIndex));

  FMatches^[iIndex].Key := iValue;
End;  


Function TAdvInt64Match.GetValue(iIndex: Integer): TAdvInt64MatchValue;
Begin 
  Assert(ValidateIndex('GetValue', iIndex));

  Result := FMatches^[iIndex].Value;
End;  


Procedure TAdvInt64Match.SetValue(iIndex: Integer; Const iValue: TAdvInt64MatchValue);
Begin 
  Assert(ValidateIndex('SetValue', iIndex));

  FMatches^[iIndex].Value := iValue;
End;  


Function TAdvInt64Match.GetPair(iIndex: Integer): TAdvInt64MatchItem;
Begin 
  Assert(ValidateIndex('GetPair', iIndex));

  Result := FMatches^[iIndex];
End;  


Procedure TAdvInt64Match.SetPair(iIndex: Integer; Const Value: TAdvInt64MatchItem);
Begin 
  Assert(ValidateIndex('SetPair', iIndex));

  FMatches^[iIndex] := Value;
End;  


Function TAdvInt64Match.GetMatch(iKey : TAdvInt64MatchKey): TAdvInt64MatchValue;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(iKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else
  Begin 
    Result := FDefault;
    If Not FForced Then
      RaiseError('GetMatch', 'Unable to get the value for the specified key.');
  End;  
End;  


Procedure TAdvInt64Match.SetMatch(iKey : TAdvInt64MatchKey; Const iValue: TAdvInt64MatchValue);
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(iKey);

  If ExistsByIndex(iIndex) Then
    ValueByIndex[iIndex] := iValue
  Else If FForced Then
    Add(iKey, iValue)
  Else
    RaiseError('SetMatch', 'Unable to set the value for the specified key.');
End;  


Function TAdvInt64Match.CapacityLimit : Integer;
Begin
  Result := High(TAdvInt64MatchItems);
End;


Function TAdvInt64Match.Find(Const aKey: TAdvInt64MatchKey; Const aValue: TAdvInt64MatchValue; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Var
  aItem : TAdvInt64MatchItem;
Begin
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;


Function TAdvInt64Match.FindByKey(Const aKey: TAdvInt64MatchKey; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Begin
  Result := Find(aKey, 0, iIndex, aCompare);
End;


Procedure TAdvInt64Match.SortedByKey;
Begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByKey);
End;


Procedure TAdvInt64Match.SortedByValue;
Begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByValue);
End;


Procedure TAdvInt64Match.SortedByKeyValue;
Begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByKeyValue);
End;


Procedure TAdvInt64Match.ForceIncrementByKey(Const aKey: TAdvInt64MatchKey);
Var
  iIndex : Integer;
Begin
  If Not FindByKey(aKey, iIndex) Then
    Insert(iIndex, aKey, 1)
  Else
    ValueByIndex[iIndex] := ValueByIndex[iIndex] + 1;
End;


Procedure TAdvInt64Match.DeleteByKey(aKey: TAdvInt64MatchKey);
Begin
  DeleteByIndex(IndexByKey(aKey));
End;


Function TAdvInt64Match.EqualTo(Const oIntegerMatch : TAdvInt64Match) : Boolean;
Var
  aPair : TAdvInt64MatchItem;
  iIndex : Integer;
Begin
  Result := oIntegerMatch.Count = Count;

  iIndex := 0;

  While Result And ExistsByIndex(iIndex) Do
  Begin
    aPair := Pairs[iIndex];

    Result := oIntegerMatch.ExistsByKeyValue(aPair.Key, aPair.Value);

    Inc(iIndex);
  End;
End;


Procedure TAdvClassList.AddArray(Const aClasses: Array Of TClass);
Var
  iLoop : Integer;
Begin 
  For iLoop := Low(aClasses) To High(aClasses) Do
    Inherited Add(aClasses[iLoop]);
End;


Function TAdvClassList.IndexByClassType(aClass: TClass): Integer;
Begin
  If Not Find(aClass, Result) Then
    Result := -1;
End;


Function TAdvClassList.ExistsByClassType(aClass : TClass) : Boolean;
Begin
  Result := ExistsByIndex(IndexByClassType(aClass));
End;


Function TAdvClassList.Iterator : TAdvIterator;
Begin 
  Result := TAdvClassListIterator.Create;

  TAdvClassListIterator(Result).ClassList := TAdvClassList(Self.Link);
End;  


Function TAdvClassList.Add(Const aClass: TClass): Integer;
Begin
  Result := Inherited Add(Pointer(aClass));
End;


Procedure TAdvClassList.AddAll(Const oClassList : TAdvClassList);
Var
  iClassIndex : Integer;
Begin
  For iClassIndex := 0 To oClassList.Count - 1 Do
    Add(oClassList[iClassIndex]);
End;


Function TAdvClassList.ItemClass : TAdvObjectClass;
Begin
  // TODO: have to constrain this class to lists of TAdvObjectClass's only to enforce this

  Result := TAdvObject;
End;


Function TAdvClassList.GetClassByIndex(Const iIndex : Integer) : TClass;
Begin
  Result := TClass(PointerByIndex[iIndex]);
End;


Procedure TAdvClassList.SetClassByIndex(Const iIndex : Integer; Const Value : TClass);
Begin
  PointerByIndex[iIndex] := Value;
End;


Function TAdvClassList.Find(Const aClass: TClass; Out iIndex: Integer): Boolean;
Begin
  Result := Inherited Find(aClass, iIndex)
End;


Constructor TAdvClassListIterator.Create;
Begin
  Inherited;

  FClassList := Nil;
End;


Destructor TAdvClassListIterator.Destroy;
Begin
  FClassList.Free;

  Inherited;
End;


Procedure TAdvClassListIterator.First;
Begin
  Inherited;

  FIndex := 0;
End;  


Procedure TAdvClassListIterator.Last;
Begin 
  Inherited;

  FIndex := FClassList.Count - 1;
End;


Procedure TAdvClassListIterator.Next;
Begin
  Inherited;

  Inc(FIndex);
End;


Procedure TAdvClassListIterator.Back;
Begin
  Inherited;

  Dec(FIndex);
End;


Function TAdvClassListIterator.Current : TClass;
Begin
  Result := FClassList[FIndex];
End;


Function TAdvClassListIterator.More : Boolean;
Begin
  Result := FClassList.ExistsByIndex(FIndex);
End;


Procedure TAdvClassListIterator.SetClassList(Const Value : TAdvClassList);
Begin
  FClassList.Free;
  FClassList := Value;
End;

Procedure TAdvObjectClassHashEntry.Assign(oSource: TAdvObject);
Begin 
  Inherited;

  FData := TAdvObjectClassHashEntry(oSource).Data;
End;  


Constructor TAdvObjectClassHashTableIterator.Create;
Begin 
  Inherited;

  FInternal := TAdvStringHashTableIterator.Create;
End;  


Destructor TAdvObjectClassHashTableIterator.Destroy;
Begin 
  FInternal.Free;

  Inherited;
End;  


Function TAdvObjectClassHashTable.ItemClass : TAdvHashEntryClass;
Begin 
  Result := TAdvObjectClassHashEntry;
End;  


Function TAdvObjectClassHashTable.Iterator : TAdvIterator;
Begin 
  Result := TAdvObjectClassHashTableIterator.Create;
  TAdvObjectClassHashTableIterator(Result).HashTable := TAdvObjectClassHashTable(Self.Link);
End;  


Function TAdvObjectClassHashTableIterator.Current : TClass;
Begin 
  Result := TAdvObjectClassHashEntry(FInternal.Current).Data;
End;


Procedure TAdvObjectClassHashTableIterator.First;
Begin 
  Inherited;

  FInternal.First;
End;  


Procedure TAdvObjectClassHashTableIterator.Last;
Begin
  Inherited;

  FInternal.Last;
End;  


Procedure TAdvObjectClassHashTableIterator.Next;
Begin 
  Inherited;

  FInternal.Next;
End;  


Procedure TAdvObjectClassHashTableIterator.Back;
Begin
  Inherited;

  FInternal.Back;
End;  


Function TAdvObjectClassHashTableIterator.More : Boolean;
Begin
  Result := FInternal.More;
End;  


Function TAdvObjectClassHashTableIterator.GetHashTable : TAdvObjectClassHashTable;
Begin 
  Result := TAdvObjectClassHashTable(FInternal.HashTable);
End;


Procedure TAdvObjectClassHashTableIterator.SetHashTable(Const Value: TAdvObjectClassHashTable);
Begin 
  FInternal.HashTable := Value;
End;


Constructor TAdvStringObjectMatch.Create;
Begin
  Inherited;

  FNominatedValueClass := ItemClass;
End;


Destructor TAdvStringObjectMatch.Destroy;
Begin
  Inherited;
End;


Procedure TAdvStringObjectMatch.AssignItem(oItems : TAdvItemList; iIndex: Integer);
Begin
  FMatchArray^[iIndex].Key := TAdvStringObjectMatch(oItems).FMatchArray^[iIndex].Key;
  FMatchArray^[iIndex].Value := TAdvStringObjectMatch(oItems).FMatchArray^[iIndex].Value.Clone;
End;


Procedure TAdvStringObjectMatch.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineString(FMatchArray^[iIndex].Key);
  oFiler['Value'].DefineObject(FMatchArray^[iIndex].Value);

  oFiler['Match'].DefineEnd;
End;


Procedure TAdvStringObjectMatch.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Var
  sKey   : TAdvStringObjectMatchKey;
  oValue : TAdvStringObjectMatchValue;
Begin 
  oValue := Nil;
  Try
    oFiler['Match'].DefineBegin;

    oFiler['Key'].DefineString(sKey);
    oFiler['Value'].DefineObject(oValue);

    oFiler['Match'].DefineEnd;

    Add(sKey, oValue.Link);
  Finally
    oValue.Free;
  End;  
End;  


Procedure TAdvStringObjectMatch.InternalEmpty(iIndex, iLength : Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatchArray) + NativeUInt(iIndex * SizeOf(TAdvStringObjectMatchItem))), (iLength * SizeOf(TAdvStringObjectMatchItem)));
End;  


Procedure TAdvStringObjectMatch.InternalTruncate(iValue : Integer);
Var
  iLoop : Integer;
  oValue : TAdvObject;
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


Procedure TAdvStringObjectMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMatchArray, Capacity * SizeOf(TAdvStringObjectMatchItem), iValue * SizeOf(TAdvStringObjectMatchItem));
End;


Procedure TAdvStringObjectMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatchArray^[iSource], @FMatchArray^[iTarget], iCount * SizeOf(TAdvStringObjectMatchItem));
End;  


Function TAdvStringObjectMatch.CompareByKey(pA, pB: Pointer): Integer;
Begin
  Result := FCompareKey(PAdvStringObjectMatchItem(pA)^.Key, PAdvStringObjectMatchItem(pB)^.Key);
End;


Function TAdvStringObjectMatch.CompareByValue(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(Integer(PAdvStringObjectMatchItem(pA)^.Value), Integer(PAdvStringObjectMatchItem(pB)^.Value));
End;


Function TAdvStringObjectMatch.Find(Const aKey: TAdvStringObjectMatchKey; Const aValue: TAdvStringObjectMatchValue; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Var
  aItem : TAdvStringObjectMatchItem;
Begin
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;


Function TAdvStringObjectMatch.FindByKey(Const aKey: TAdvStringObjectMatchKey; Out iIndex: Integer): Boolean;
Begin
  Result := Find(aKey, Nil, iIndex, CompareByKey);
End;


Function TAdvStringObjectMatch.FindByValue(Const aValue: TAdvStringObjectMatchValue; Out iIndex: Integer) : Boolean;
Begin
  Result := Find('', aValue, iIndex, CompareByValue);
End;


Function TAdvStringObjectMatch.IndexByKey(Const aKey : TAdvStringObjectMatchKey) : Integer;
Begin
  If Not Find(aKey, Nil, Result, CompareByKey) Then
    Result := -1;
End;


Function TAdvStringObjectMatch.IndexByValue(Const aValue : TAdvStringObjectMatchValue) : Integer;
Begin
  If Not Find('', aValue, Result, CompareByValue) Then
    Result := -1;
End;


Function TAdvStringObjectMatch.Add(Const aKey : TAdvStringObjectMatchKey; Const aValue : TAdvStringObjectMatchValue) : Integer;
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


Procedure TAdvStringObjectMatch.Insert(iIndex : Integer; Const aKey : TAdvStringObjectMatchKey; Const aValue : TAdvStringObjectMatchValue);
Begin 
  Assert(ValidateItem('Insert', aValue, 'aValue'));

  InternalInsert(iIndex);

  FMatchArray^[iIndex].Key := aKey;
  FMatchArray^[iIndex].Value := aValue;
End;  


Procedure TAdvStringObjectMatch.InternalInsert(iIndex : Integer);
Begin 
  Inherited;

  Pointer(FMatchArray^[iIndex].Key) := Nil;
  Pointer(FMatchArray^[iIndex].Value) := Nil;
End;  


Procedure TAdvStringObjectMatch.InternalDelete(iIndex : Integer);
Begin 
  Inherited;

  Finalize(FMatchArray^[iIndex].Key);
  FMatchArray^[iIndex].Value.Free;
  FMatchArray^[iIndex].Value := Nil;
End;  


Procedure TAdvStringObjectMatch.InternalExchange(iA, iB: Integer);
Var
  aTemp : TAdvStringObjectMatchItem;
  pA    : PAdvStringObjectMatchItem;
  pB    : PAdvStringObjectMatchItem;
Begin 
  pA := @FMatchArray^[iA];
  pB := @FMatchArray^[iB];

  aTemp := pA^;
  pA^ := pB^;
  pB^ := aTemp;
End;


Function TAdvStringObjectMatch.GetItem(iIndex : Integer) : Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatchArray^[iIndex];
End;  


Procedure TAdvStringObjectMatch.SetItem(iIndex: Integer; pValue: Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FMatchArray^[iIndex] := PAdvStringObjectMatchItem(pValue)^;
End;  


Function TAdvStringObjectMatch.GetKeyByIndex(iIndex : Integer): TAdvStringObjectMatchKey;
Begin
  Assert(ValidateIndex('GetKeyByIndex', iIndex));

  Result := FMatchArray^[iIndex].Key;
End;  


Procedure TAdvStringObjectMatch.SetKeyByIndex(iIndex : Integer; Const aKey : TAdvStringObjectMatchKey);
Begin
  Assert(ValidateIndex('SetKeyByIndex', iIndex));

  FMatchArray^[iIndex].Key := aKey;
End;  


Function TAdvStringObjectMatch.GetValueByIndex(iIndex : Integer): TAdvStringObjectMatchValue;
Begin
  Assert(ValidateIndex('GetValueByIndex', iIndex));

  Result := FMatchArray^[iIndex].Value;
End;


Procedure TAdvStringObjectMatch.SetValueByIndex(iIndex : Integer; Const aValue: TAdvStringObjectMatchValue);
Begin
  Assert(ValidateIndex('SetValueByIndex', iIndex));
  Assert(ValidateItem('SetValueByIndex', aValue, 'aValue'));

  FMatchArray^[iIndex].Value.Free;
  FMatchArray^[iIndex].Value := aValue;
End;


Procedure TAdvStringObjectMatch.SetValueByKey(Const aKey : TAdvStringObjectMatchKey; Const aValue : TAdvStringObjectMatchValue);
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


Function TAdvStringObjectMatch.GetSensitive : Boolean;
Var
  aCompare : TAdvStringCompareCallback;
Begin
  aCompare := StringCompareSensitive;

  Result := @FCompareKey = @aCompare;
End;


Procedure TAdvStringObjectMatch.SetSensitive(Const Value: Boolean);
Begin
  If Value Then
    FCompareKey := StringCompareSensitive
  Else
    FCompareKey := StringCompareInsensitive;
End;


Function TAdvStringObjectMatch.GetAsText: String;
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


Procedure TAdvStringObjectMatch.SetAsText(Const Value: String);
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


Function TAdvStringObjectMatch.CapacityLimit : Integer;
Begin 
  Result := High(TAdvStringObjectMatchItemArray);
End;  


Procedure TAdvStringObjectMatch.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin 
  aCompare := CompareByKey;
  FCompareKey := StringCompareInsensitive;
End;


Function TAdvStringObjectMatch.ItemClass : TAdvObjectClass;
Begin 
  Result := TAdvObject;
End;  


Function TAdvStringObjectMatch.ValidateIndex(Const sMethod : String; iIndex: Integer): Boolean;
Begin 
  Inherited ValidateIndex(sMethod, iIndex);

  ValidateItem(sMethod, FMatchArray^[iIndex].Value, 'FMatchArray^[' + IntegerToString(iIndex) + '].Value');

  Result := True;
End;  


Function TAdvStringObjectMatch.ValidateItem(Const sMethod : String; oObject : TAdvObject; Const sObject : String) : Boolean;
Begin 
  If Assigned(oObject) Then
    Invariants(sMethod, oObject, NominatedValueClass, sObject);

  Result := True;
End;  


Function TAdvStringObjectMatch.ExistsByKey(Const aKey : TAdvStringObjectMatchKey) : Boolean;
Begin
  Result := IndexByKey(aKey) >= 0;
End;


Function TAdvStringObjectMatch.ExistsByValue(Const aValue : TAdvStringObjectMatchValue) : Boolean;
Begin
  Result := IndexByValue(aValue) >= 0;
End;


Procedure TAdvStringObjectMatch.SortedByKey;
Begin 
  SortedBy(CompareByKey);
End;  


Procedure TAdvStringObjectMatch.SortedByValue;
Begin
  SortedBy(CompareByValue);
End;  


Function TAdvStringObjectMatch.IsSortedByKey : Boolean;
Begin 
  Result := IsSortedBy(CompareByKey);
End;


Function TAdvStringObjectMatch.IsSortedByValue : Boolean;
Begin 
  Result := IsSortedBy(CompareByValue);
End;  


Function TAdvStringObjectMatch.GetKeyByValue(Const aValue: TAdvStringObjectMatchValue): TAdvStringObjectMatchKey;
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


Function TAdvStringObjectMatch.GetValueByKey(Const aKey: TAdvStringObjectMatchKey): TAdvStringObjectMatchValue;
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


Procedure TAdvStringObjectMatch.AddAll(oSourceStringObjectMatch: TAdvStringObjectMatch);
Var
  iIndex : Integer;
Begin
  Assert(CheckCondition(oSourceStringObjectMatch <> Self, 'AddAll', 'Cannot addall items from a list to itself.'));

  Capacity := IntegerMax(Count + oSourceStringObjectMatch.Count, Capacity);
  For iIndex := 0 To oSourceStringObjectMatch.Count - 1 Do
    Add(oSourceStringObjectMatch.KeyByIndex[iIndex], oSourceStringObjectMatch.ValueByIndex[iIndex].Link);
End;


Function TAdvStringObjectMatch.Link: TAdvStringObjectMatch;
Begin
  Result := TAdvStringObjectMatch(Inherited Link);
End;


Function TAdvStringObjectMatch.GetMatchByIndex(iIndex: Integer): TAdvStringObjectMatchItem;
Begin
  Assert(ValidateIndex('GetMatchByIndex', iIndex));

  Result := FMatchArray^[iIndex];
End;


Function TAdvStringObjectMatch.GetMatch(Const aKey : TAdvStringObjectMatchKey): TAdvStringObjectMatchValue;
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


Procedure TAdvStringObjectMatch.SetMatch(Const aKey : TAdvStringObjectMatchKey; Const aValue : TAdvStringObjectMatchValue);
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

Procedure TAdvStringObjectMatch.DeleteByKey(Const aKey: TAdvStringObjectMatchKey);
Begin
  DeleteByIndex(IndexByKey(aKey));
End;


Procedure TAdvStringObjectMatch.DeleteByValue(Const aValue: TAdvStringObjectMatchValue);
Begin
  DeleteByIndex(IndexByValue(aValue));
End;


Constructor TAdvStringMatch.Create;
Begin
  Inherited;

  FSymbol := cReturn;
  FSeparator := '=';
End;  


Procedure TAdvStringMatch.Assign(oObject: TAdvObject);
Begin 
  Inherited;

  FDefaultValue := TAdvStringMatch(oObject).FDefaultValue;
  FForced := TAdvStringMatch(oObject).FForced;
  FSymbol := TAdvStringMatch(oObject).FSymbol;
End;  


Function TAdvStringMatch.Link : TAdvStringMatch;
Begin 
  Result := TAdvStringMatch(Inherited Link);
End;  


Function TAdvStringMatch.Clone : TAdvStringMatch;
Begin 
  Result := TAdvStringMatch(Inherited Clone);
End;  


Procedure TAdvStringMatch.DefaultCompare(Out aCompare : TAdvItemListCompare);
Begin 
  aCompare := CompareByKey;
End;  


Function TAdvStringMatch.CompareMatch(pA, pB: Pointer): Integer;
Begin 
  Result := StringCompare(PAdvStringMatchItem(pA)^.Key, PAdvStringMatchItem(pB)^.Key);

  If Result = 0 Then
    Result := StringCompare(PAdvStringMatchItem(pA)^.Value, PAdvStringMatchItem(pB)^.Value);
End;  


Function TAdvStringMatch.CompareByKey(pA, pB: Pointer): Integer;
Begin 
  Result := StringCompareInsensitive(PAdvStringMatchItem(pA)^.Key, PAdvStringMatchItem(pB)^.Key);
End;


Function TAdvStringMatch.CompareByValue(pA, pB: Pointer): Integer;
Begin
  Result := StringCompareInsensitive(PAdvStringMatchItem(pA)^.Value, PAdvStringMatchItem(pB)^.Value);
End;


Function TAdvStringMatch.CompareByCaseSensitiveKey(pA, pB: Pointer): Integer;
Begin
  Result := StringCompareSensitive(PAdvStringMatchItem(pA)^.Key, PAdvStringMatchItem(pB)^.Key);
End;


Procedure TAdvStringMatch.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Var
  sKey : TAdvStringMatchKey;
  sValue : TAdvStringMatchValue;
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineString(sKey);
  oFiler['Value'].DefineString(sValue);

  oFiler['Match'].DefineEnd;

  Add(sKey, sValue);
End;  


Procedure TAdvStringMatch.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineString(FMatchArray^[iIndex].Key);
  oFiler['Value'].DefineString(FMatchArray^[iIndex].Value);

  oFiler['Match'].DefineEnd;
End;


Procedure TAdvStringMatch.AssignItem(oItems : TAdvItemList; iIndex: Integer);
Begin 
  Inherited;

  FMatchArray^[iIndex] := TAdvStringMatch(oItems).FMatchArray^[iIndex];
End;  


Procedure TAdvStringMatch.InternalEmpty(iIndex, iLength : Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatchArray) + NativeUInt(iIndex * SizeOf(TAdvStringMatchItem))), (iLength * SizeOf(TAdvStringMatchItem)));
End;  


Procedure TAdvStringMatch.InternalTruncate(iValue : Integer);
Begin
  Inherited;

  // finalize the strings that will be truncated.
  If iValue < Count Then
    Finalize(FMatchArray^[iValue], Count - iValue);
End;


Procedure TAdvStringMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMatchArray, Capacity * SizeOf(TAdvStringMatchItem), iValue * SizeOf(TAdvStringMatchItem));
End;


Procedure TAdvStringMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatchArray^[iSource], @FMatchArray^[iTarget], iCount * SizeOf(TAdvStringMatchItem));
End;  


Function TAdvStringMatch.IndexByKey(Const aKey: TAdvStringMatchKey): Integer;
Begin
  If Not Find(aKey, '', Result, CompareByKey) Then
    Result := -1;
End;


Function TAdvStringMatch.IndexByCaseSensitiveKey(Const aKey: TAdvStringMatchKey): Integer;
Begin
  If Not Find(aKey, '', Result, CompareByCaseSensitiveKey) Then
    Result := -1;
End;  


Function TAdvStringMatch.IndexByValue(Const aValue: TAdvStringMatchValue): Integer;
Begin 
  If Not Find('', aValue, Result, CompareByValue) Then
    Result := -1;
End;


Function TAdvStringMatch.IndexOf(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue): Integer;
Begin
  If Not Find(aKey, aValue, Result, CompareMatch) Then
    Result := -1;
End;


Function TAdvStringMatch.ExistsByKeyAndValue(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue) : Boolean;
Begin
  Result := IndexOf(aKey, aValue) >= 0;
End;


Function TAdvStringMatch.ExistsByKey(Const aKey: TAdvStringMatchKey): Boolean;
Begin
  Result := IndexByKey(aKey) >= 0;
End;


Procedure TAdvStringMatch.DeleteByKey(Const aKey : TAdvStringMatchKey);
Var
  iIndex : Integer;
Begin
  iIndex := IndexByKey(aKey);
  If ExistsByIndex(iIndex) Then
    DeleteByIndex(iIndex)
  Else If Not Forced Then
    RaiseError('DeleteByKey', StringFormat('Unable to find a value for the specified key ''%s''.', [aKey]));
End;



Function TAdvStringMatch.ExistsByValue(Const aValue: TAdvStringMatchValue): Boolean;
Begin
  Result := IndexByValue(aValue) >= 0;
End;


Function TAdvStringMatch.Add(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue) : Integer;
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


Procedure TAdvStringMatch.Insert(iIndex : Integer; Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue);
Begin 
  InternalInsert(iIndex);

  FMatchArray^[iIndex].Key := aKey;
  FMatchArray^[iIndex].Value := aValue;
End;  


Procedure TAdvStringMatch.InternalInsert(iIndex : Integer);
Begin 
  Inherited;

  Pointer(FMatchArray^[iIndex].Key) := Nil;
  Pointer(FMatchArray^[iIndex].Value) := Nil;
End;  


Procedure TAdvStringMatch.InternalDelete(iIndex : Integer);
Begin 
  Inherited;

  Finalize(FMatchArray^[iIndex]);  
End;  


Procedure TAdvStringMatch.InternalExchange(iA, iB: Integer);
Var
  aTemp : TAdvStringMatchItem;
  pA    : PAdvStringMatchItem;
  pB    : PAdvStringMatchItem;
Begin 
  pA := @FMatchArray^[iA];
  pB := @FMatchArray^[iB];

  aTemp := pA^;
  pA^ := pB^;
  pB^ := aTemp;
End;  


Function TAdvStringMatch.GetItem(iIndex : Integer) : Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatchArray^[iIndex];
End;  


Procedure TAdvStringMatch.SetItem(iIndex: Integer; pValue: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatchArray^[iIndex] := PAdvStringMatchItem(pValue)^;
End;  


Function TAdvStringMatch.GetKeyByIndex(iIndex : Integer): TAdvStringMatchKey;
Begin 
  Assert(ValidateIndex('GetKeyByIndex', iIndex));

  Result := FMatchArray^[iIndex].Key;
End;  


Procedure TAdvStringMatch.SetKeyByIndex(iIndex : Integer; Const aKey : TAdvStringMatchKey);
Begin 
  Assert(ValidateIndex('SetKeyByIndex', iIndex));

  FMatchArray^[iIndex].Key := aKey;
End;  


Function TAdvStringMatch.GetValueByIndex(iIndex : Integer): TAdvStringMatchValue;
Begin 
  Assert(ValidateIndex('GetValueByIndex', iIndex));

  Result := FMatchArray^[iIndex].Value;
End;  


Procedure TAdvStringMatch.SetValueByIndex(iIndex : Integer; Const aValue: TAdvStringMatchValue);
Begin 
  Assert(ValidateIndex('SetValueByIndex', iIndex));

  FMatchArray^[iIndex].Value := aValue;
End;  


Function TAdvStringMatch.GetValueByKey(Const aKey : TAdvStringMatchKey): TAdvStringMatchValue;
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


Procedure TAdvStringMatch.SetValueByKey(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue);
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


Function TAdvStringMatch.GetAsText : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := 0 To Count - 1 Do
    Result := Result + KeyByIndex[iLoop] + FSeparator + ValueByIndex[iLoop] + FSymbol;
End;  


Procedure TAdvStringMatch.SetAsText(Const sValue: String);
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


Function TAdvStringMatch.CapacityLimit : Integer;
Begin 
  Result := High(TAdvStringMatchItems);
End;  


Function TAdvStringMatch.Find(Const aKey : TAdvStringMatchKey; Const aValue: TAdvStringMatchValue; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Var
  aItem : TAdvStringMatchItem;
Begin 
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;  


Function TAdvStringMatch.DefaultValue(Const aKey: TAdvStringMatchKey): TAdvStringMatchValue;
Begin 
  Result := FDefaultValue;
End;  


Function TAdvStringMatch.ForceByKey(Const aKey: TAdvStringMatchKey): TAdvStringMatchValue;
Var
  iIndex : Integer;
Begin 
  If Not Find(aKey, '', iIndex, CompareByKey) Then
    Insert(iIndex, aKey, DefaultValue(aKey));

  Result := ValueByIndex[iIndex];
End;  


Function TAdvStringMatch.EqualTo(oMatch: TAdvStringMatch): Boolean;
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


Procedure TAdvStringMatch.SortedByKey;
Begin
  SortedBy(CompareByKey);
End;


Procedure TAdvStringMatch.SortedByCaseSensitiveKey;
Begin
  SortedBy(CompareByCaseSensitiveKey);
End;


Procedure TAdvStringMatch.SortedByValue;
Begin
  SortedBy(CompareByValue);
End;


Function TAdvStringMatch.GetMatchByIndex(Const iIndex : Integer) : TAdvStringMatchItem;
Begin
  Result := PAdvStringMatchItem(Inherited ItemByIndex[iIndex])^;
End;


Procedure TAdvStringMatch.SetMatchByIndex(Const iIndex : Integer; Const Value : TAdvStringMatchItem);
Begin
  PAdvStringMatchItem(Inherited ItemByIndex[iIndex])^ := Value;
End;


Function TAdvStringMatch.ErrorClass: EAdvExceptionClass;
Begin
  Result := EAdvStringMatch;
End;


Procedure TAdvStringMatch.AddAll(oStringMatch: TAdvStringMatch);
Var
  iIndex : Integer;
Begin
  Assert(CheckCondition(oStringMatch <> Self, 'AddAll', 'Cannot add all items from a list to itself.'));

  Capacity := IntegerMax(Count + oStringMatch.Count, Capacity);

  For iIndex := 0 To oStringMatch.Count - 1 Do
    Add(oStringMatch.KeyByIndex[iIndex], oStringMatch.ValueByIndex[iIndex]);
End;


Function TAdvStringMatch.GetMatch(Const aKey : TAdvStringMatchKey): TAdvStringMatchValue;
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


Procedure TAdvStringMatch.SetMatch(Const aKey : TAdvStringMatchKey; Const aValue : TAdvStringMatchValue);
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



Constructor TAdvStringList.Create;
Begin
  Inherited;

  FSymbol := cReturn;
End;


Function TAdvStringList.Clone: TAdvStringList;
Begin
  Result := TAdvStringList(Inherited Clone);
End;


Function TAdvStringList.Link: TAdvStringList;
Begin
  Result := TAdvStringList(Inherited Link);
End;


Procedure TAdvStringList.SaveItem(Filer: TAdvFiler; iIndex: Integer);
Begin
  Filer['String'].DefineString(FStringArray^[iIndex]);
End;


Procedure TAdvStringList.LoadItem(Filer: TAdvFiler; iIndex: Integer);
Var
  sValue : TAdvStringListItem;
Begin
  Filer['String'].DefineString(sValue);

  Add(sValue);
End;


Procedure TAdvStringList.AssignItem(Items: TAdvItemList; iIndex: Integer);
Begin
  FStringArray^[iIndex] := TAdvStringList(Items).FStringArray^[iIndex];
End;


Function TAdvStringList.Find(Const sValue : TAdvStringListItem; Out iIndex : Integer; aCompare : TAdvItemListCompare): Boolean;
Begin
  Result := Inherited Find(@sValue, iIndex, aCompare);
End;


Procedure TAdvStringList.InternalEmpty(iIndex, iLength: Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FStringArray) + NativeUInt(iIndex * SizeOf(TAdvStringListItem))), (iLength * SizeOf(TAdvStringListItem)));
End;


Procedure TAdvStringList.InternalTruncate(iValue : Integer);
Begin
  Inherited;

  // finalize the strings that will be truncated.
  If iValue < Count Then
    Finalize(FStringArray^[iValue], Count - iValue);
End;


Procedure TAdvStringList.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FStringArray, Capacity * SizeOf(TAdvStringListItem), iValue * SizeOf(TAdvStringListItem));
End;


Procedure TAdvStringList.InternalCopy(iSource, iTarget, iCount : Integer);
Begin
  Inherited;

  MemoryMove(@FStringArray^[iSource], @FStringArray^[iTarget], iCount * SizeOf(TAdvStringListItem));
End;


Function TAdvStringList.IndexByValue(Const sValue : TAdvStringListItem): Integer;
Begin
  If Not Find(sValue, Result) Then
    Result := -1;
End;


Function TAdvStringList.ExistsByValue(Const sValue : TAdvStringListItem): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(sValue));
End;


Procedure TAdvStringList.AddAll(oStrings: TAdvStringList);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oStrings.Count - 1 Do
    Add(oStrings[iLoop]);
End;


Procedure TAdvStringList.AddAllStringArray(Const aValues : Array Of String);
Var
  iLoop : Integer;
Begin
  For iLoop := Low(aValues) To High(aValues) Do
    Add(aValues[iLoop]);
End;


Function TAdvStringList.Add(Const sValue : TAdvStringListItem) : Integer;
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


Procedure TAdvStringList.InternalInsert(iIndex : Integer);
Begin
  Inherited;

  Pointer(FStringArray^[iIndex]) := Nil;
End;


Procedure TAdvStringList.Insert(iIndex : Integer; Const sValue : TAdvStringListItem);
Begin
  InternalInsert(iIndex);

  FStringArray^[iIndex] := sValue;
End;


Procedure TAdvStringList.InternalDelete(iIndex: Integer);
Begin
  Inherited;

  Finalize(FStringArray^[iIndex]);
End;


Procedure TAdvStringList.DeleteByValue(Const sValue : TAdvStringListItem);
Var
  iIndex : Integer;
Begin
  If Not Find(sValue, iIndex) Then
    RaiseError('DeleteByValue', StringFormat('''%s'' not found in list', [sValue]));

  DeleteByIndex(iIndex);
End;


Procedure TAdvStringList.InternalExchange(iA, iB : Integer);
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


Function TAdvStringList.GetItem(iIndex : Integer) : Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FStringArray^[iIndex];
End;


Procedure TAdvStringList.SetItem(iIndex : Integer; pValue : Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FStringArray^[iIndex] := String(pValue^);
End;


Function TAdvStringList.GetStringByIndex(iIndex : Integer) : String;
Begin
  Assert(ValidateIndex('GetStringByIndex', iIndex));

  Result := FStringArray^[iIndex];
End;


Procedure TAdvStringList.SetStringByIndex(iIndex : Integer; Const sValue : TAdvStringListItem);
Begin
  Assert(ValidateIndex('SetStringByIndex', iIndex));

  FStringArray^[iIndex] := sValue;
End;


Function TAdvStringList.GetAsText : String;
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


Procedure TAdvStringList.SetAsText(Const sValue: String);
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


Procedure TAdvStringList.LoadFromText(Const sFilename: String);
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


Procedure TAdvStringList.SaveToText(Const sFilename : String);
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


Function TAdvStringList.CapacityLimit : Integer;
Begin
  Result := High(TAdvStringListItemArray);
End;


Function TAdvStringList.CompareInsensitive(pA, pB: Pointer): Integer;
Begin
  Result := StringCompareInsensitive(String(pA^), String(pB^));
End;


Function TAdvStringList.CompareSensitive(pA, pB: Pointer): Integer;
Begin
  Result := StringCompareSensitive(String(pA^), String(pB^));
End;


Procedure TAdvStringList.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin
  aCompare := CompareInsensitive;
End;


Function TAdvStringList.GetSensitive : Boolean;
Begin
  Result := IsComparedBy(CompareSensitive);
End;


Procedure TAdvStringList.SetSensitive(Const bValue: Boolean);
Begin
  If bValue Then
    ComparedBy(CompareSensitive)
  Else
    ComparedBy(CompareInsensitive);
End;


Function TAdvStringList.Iterator : TAdvIterator;
Begin
  Result := TAdvStringListIterator.Create;
  TAdvStringListIterator(Result).StringList := TAdvStringList(Self.Link);
End;


Constructor TAdvStringListIterator.Create;
Begin
  Inherited;

  FStringList := Nil;
End;


Destructor TAdvStringListIterator.Destroy;
Begin
  FStringList.Free;

  Inherited;
End;


Procedure TAdvStringListIterator.First;
Begin
  Inherited;

  FIndex := 0;
End;


Procedure TAdvStringListIterator.Last;
Begin
  Inherited;

  FIndex := FStringList.Count - 1;
End;


Procedure TAdvStringListIterator.Next;
Begin
  Inherited;

  Inc(FIndex);
End;


Procedure TAdvStringListIterator.Back;
Begin
  Inherited;

  Dec(FIndex);
End;


Function TAdvStringListIterator.Current : String;
Begin
  Result := FStringList[FIndex];
End;


Function TAdvStringListIterator.More : Boolean;
Begin
  Result := FStringList.ExistsByIndex(FIndex);
End;


Procedure TAdvStringListIterator.SetStringList(Const Value : TAdvStringList);
Begin
  FStringList.Free;
  FStringList := Value;
End;


Function TAdvStringList.GetAsCSV : String;
Var
  oStream : TAdvStringStream;
  oFormatter : TAdvCSVFormatter;
  chars: SysUtils.TCharArray;
Begin
  oStream := TAdvStringStream.Create;
  Try
    oFormatter := TAdvCSVFormatter.Create;
    Try
      oFormatter.Stream := oStream.Link;

      oFormatter.ProduceEntryStringList(Self);
    Finally
      oFormatter.Free;
    End;

    chars := TEncoding.UTF8.GetChars(oStream.Bytes);
    SetString(Result, PChar(chars), Length(chars));
  Finally
    oStream.Free;
  End;
End;


Procedure TAdvStringList.SetAsCSV(Const sValue: String);
Var
  oStream : TAdvStringStream;
  oExtractor : TAdvCSVExtractor;
Begin
  oStream := TAdvStringStream.Create;
  Try
    oStream.Bytes := TEncoding.UTF8.GetBytes(sValue);
    oExtractor := TAdvCSVExtractor.Create(oStream.Link, TEncoding.UTF8);
    Try
      oExtractor.ConsumeEntries(Self);
    Finally
      oExtractor.Free;
    End;
  Finally
    oStream.Free;
  End;
End;


Function TAdvStringList.Compare(oStrings : TAdvStringList) : Integer;
Var
  iLoop : Integer;
Begin
  Assert(Invariants('Compare', oStrings, TAdvStringList, 'oStrings'));

  Result := IntegerCompare(Count, oStrings.Count);
  iLoop := 0;

  While (iLoop < oStrings.Count) And (Result = 0) Do
  Begin
    Result := StringCompare(StringByIndex[iLoop], oStrings[iLoop]);

    Inc(iLoop);
  End;
End; 


Function TAdvStringList.Equals(oStrings : TAdvStringList) : Boolean;
Begin
  Result := Compare(oStrings) = 0;
End;


Function TAdvStringList.ExistsAny(oStrings : TAdvStringList) : Boolean;
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


procedure TAdvStringList.SetItems(iIndex: integer; sValue: String);
begin
  StringByIndex[iIndex] := sValue;
end;

function TAdvStringList.GetItems(iIndex: integer): String;
begin
  result := StringByIndex[iIndex];
end;

procedure TAdvStringList.delete(iIndex: integer);
begin
  DeleteByIndex(iIndex);
end;

procedure TAdvStringList.populate(iCount: integer);
begin
  while Count < iCount Do
    Add('');
end;

function TAdvStringList.IndexOf(value: String): Integer;
begin
  result := IndexByValue(value);
end;


Constructor TAdvStringLargeIntegerMatch.Create;
Begin
  Inherited;

  FSymbol := cReturn;
End;


Destructor TAdvStringLargeIntegerMatch.Destroy;
Begin
  Inherited;
End;


Function TAdvStringLargeIntegerMatch.ErrorClass: EAdvExceptionClass;
Begin
  Result := EAdvStringLargeIntegerMatch;
End;


Function TAdvStringLargeIntegerMatch.Clone: TAdvStringLargeIntegerMatch;
Begin
  Result := TAdvStringLargeIntegerMatch(Inherited Clone);
End;


Function TAdvStringLargeIntegerMatch.Link: TAdvStringLargeIntegerMatch;
Begin
  Result := TAdvStringLargeIntegerMatch(Inherited Link);
End;


Procedure TAdvStringLargeIntegerMatch.AssignItem(oItems: TAdvItemList; iIndex: Integer);
Begin
  FMatchArray^[iIndex] := TAdvStringLargeIntegerMatch(oItems).FMatchArray^[iIndex];
End;


Procedure TAdvStringLargeIntegerMatch.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineString(FMatchArray^[iIndex].Key);
  oFiler['Value'].DefineInteger(FMatchArray^[iIndex].Value);

  oFiler['Match'].DefineEnd;
End;  


Procedure TAdvStringLargeIntegerMatch.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Var
  sKey : TAdvStringLargeIntegerMatchKey;
  iValue : TAdvStringLargeIntegerMatchValue;
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineString(sKey);
  oFiler['Value'].DefineInteger(iValue);

  oFiler['Match'].DefineEnd;

  Add(sKey, iValue);  
End;  


Procedure TAdvStringLargeIntegerMatch.InternalEmpty(iIndex, iLength: Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatchArray) + NativeUInt(iIndex * SizeOf(TAdvStringLargeIntegerMatchItem))), (iLength * SizeOf(TAdvStringLargeIntegerMatchItem)));
End;


Procedure TAdvStringLargeIntegerMatch.InternalTruncate(iValue: Integer);
Begin
  Inherited;

  // finalize the strings that will be truncated
  If iValue < Count Then
    Finalize(FMatchArray^[iValue], Count - iValue);
End;


Procedure TAdvStringLargeIntegerMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMatchArray, Capacity * SizeOf(TAdvStringLargeIntegerMatchItem), iValue * SizeOf(TAdvStringLargeIntegerMatchItem));
End;


Procedure TAdvStringLargeIntegerMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatchArray^[iSource], @FMatchArray^[iTarget], iCount * SizeOf(TAdvStringLargeIntegerMatchItem));
End;  


Function TAdvStringLargeIntegerMatch.CompareKey(pA, pB: Pointer): Integer;
Begin 
  Result := FCompareKey(PAdvStringLargeIntegerMatchItem(pA)^.Key, PAdvStringLargeIntegerMatchItem(pB)^.Key);
End;


Function TAdvStringLargeIntegerMatch.CompareValue(pA, pB: Pointer): Integer;
Begin 
  Result := IntegerCompare(PAdvStringLargeIntegerMatchItem(pA)^.Value, PAdvStringLargeIntegerMatchItem(pB)^.Value);
End;  


Procedure TAdvStringLargeIntegerMatch.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin 
  aCompare := {$IFDEF FPC}@{$ENDIF}CompareKey;
  FCompareKey := {$IFDEF FPC}@{$ENDIF}StringCompareInsensitive;
End;  


Function TAdvStringLargeIntegerMatch.Find(Const aKey: TAdvStringLargeIntegerMatchKey; Const aValue: TAdvStringLargeIntegerMatchValue; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Var
  aItem : TAdvStringLargeIntegerMatchItem;
Begin 
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;


Function TAdvStringLargeIntegerMatch.IndexByKey(Const aKey : TAdvStringLargeIntegerMatchKey) : Integer;
Begin 
  If Not Find(aKey, 0, Result, {$IFDEF FPC}@{$ENDIF}CompareKey) Then
    Result := -1;
End;  


Function TAdvStringLargeIntegerMatch.ExistsByKey(Const aKey : TAdvStringLargeIntegerMatchKey) : Boolean;
Begin 
  Result := ExistsByIndex(IndexByKey(aKey));
End;


Function TAdvStringLargeIntegerMatch.IndexByValue(Const aValue : TAdvStringLargeIntegerMatchValue) : Integer;
Begin 
  If Not Find('', aValue, Result, {$IFDEF FPC}@{$ENDIF}CompareValue) Then
    Result := -1;
End;


Function TAdvStringLargeIntegerMatch.ExistsByValue(Const aValue : TAdvStringLargeIntegerMatchValue) : Boolean;
Begin 
  Result := ExistsByIndex(IndexByValue(aValue));
End;  


Function TAdvStringLargeIntegerMatch.Add(Const aKey : TAdvStringLargeIntegerMatchKey; Const aValue : TAdvStringLargeIntegerMatchValue) : Integer;
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


Procedure TAdvStringLargeIntegerMatch.Insert(iIndex : Integer; Const aKey : TAdvStringLargeIntegerMatchKey; Const aValue : TAdvStringLargeIntegerMatchValue);
Begin 
  InternalInsert(iIndex);

  FMatchArray^[iIndex].Key := aKey;
  FMatchArray^[iIndex].Value := aValue;
End;  


Procedure TAdvStringLargeIntegerMatch.InternalInsert(iIndex : Integer);
Begin 
  Inherited;

  Pointer(FMatchArray^[iIndex].Key) := Nil;
  FMatchArray^[iIndex].Value := 0;
End;  


Procedure TAdvStringLargeIntegerMatch.InternalDelete(iIndex : Integer);
Begin 
  Inherited;

  Finalize(FMatchArray^[iIndex]);  
End;  


Procedure TAdvStringLargeIntegerMatch.InternalExchange(iA, iB : Integer);
Var
  aTemp : TAdvStringLargeIntegerMatchItem;
  pA : Pointer;
  pB : Pointer;
Begin
  pA := @FMatchArray^[iA];
  pB := @FMatchArray^[iB];

  aTemp := PAdvStringLargeIntegerMatchItem(pA)^;
  PAdvStringLargeIntegerMatchItem(pA)^ := PAdvStringLargeIntegerMatchItem(pB)^;
  PAdvStringLargeIntegerMatchItem(pB)^ := aTemp;
End;


Function TAdvStringLargeIntegerMatch.Force(Const aKey: TAdvStringLargeIntegerMatchKey): TAdvStringLargeIntegerMatchValue;
Var
  iIndex : Integer;
Begin
  If Not Find(aKey, 0, iIndex, {$IFDEF FPC}@{$ENDIF}CompareKey) Then
    Insert(iIndex, aKey, Default);

  Result := ValueByIndex[iIndex]
End;


Function TAdvStringLargeIntegerMatch.GetItem(iIndex : Integer) : Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatchArray^[iIndex];
End;  


Procedure TAdvStringLargeIntegerMatch.SetItem(iIndex : Integer; pValue : Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatchArray^[iIndex] := PAdvStringLargeIntegerMatchItem(pValue)^;
End;  


Function TAdvStringLargeIntegerMatch.GetKey(iIndex : Integer) : String;
Begin 
  Assert(ValidateIndex('GetKey', iIndex));

  Result := FMatchArray^[iIndex].Key;
End;


Procedure TAdvStringLargeIntegerMatch.SetKey(iIndex : Integer; Const aKey : TAdvStringLargeIntegerMatchKey);
Begin 
  Assert(ValidateIndex('SetKey', iIndex));

  FMatchArray^[iIndex].Key := aKey;
End;  


Function TAdvStringLargeIntegerMatch.GetValue(iIndex : Integer) : TAdvStringLargeIntegerMatchValue;
Begin 
  Assert(ValidateIndex('GetValue', iIndex));

  Result := FMatchArray^[iIndex].Value;
End;  


Procedure TAdvStringLargeIntegerMatch.SetValue(iIndex : Integer; Const aValue : TAdvStringLargeIntegerMatchValue);
Begin 
  Assert(ValidateIndex('SetValue', iIndex));

  FMatchArray^[iIndex].Value := aValue;
End;  


Function TAdvStringLargeIntegerMatch.GetMatch(Const aKey : TAdvStringLargeIntegerMatchKey) : TAdvStringLargeIntegerMatchValue;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else
  Begin 
    Result := FDefault;

    If Not FForced Then
      RaiseError('GetMatch', 'Unable to get the value for the specified key.');
  End;  
End;  


Procedure TAdvStringLargeIntegerMatch.SetMatch(Const aKey : TAdvStringLargeIntegerMatchKey; Const aValue : TAdvStringLargeIntegerMatchValue);
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


Function TAdvStringLargeIntegerMatch.GetAsText : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, KeyByIndex[iLoop] + '=' + IntegerToString(ValueByIndex[iLoop]), FSymbol);
End;  


Function TAdvStringLargeIntegerMatch.GetKeysAsText : String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, KeyByIndex[iLoop], FSymbol);
End;  


Function TAdvStringLargeIntegerMatch.CapacityLimit : Integer;
Begin 
  Result := High(TAdvStringLargeIntegerMatchItemArray);
End;  


Function TAdvStringLargeIntegerMatch.GetSensitive : Boolean;
Var
  aCompare : TAdvStringCompareCallback;
Begin
  aCompare := {$IFDEF FPC}@{$ENDIF}StringCompareSensitive;

  Result := @FCompareKey = @aCompare;
End;


Procedure TAdvStringLargeIntegerMatch.SetSensitive(Const Value: Boolean);
Begin
  If Value Then
    FCompareKey := {$IFDEF FPC}@{$ENDIF}StringCompareSensitive
  Else
    FCompareKey := {$IFDEF FPC}@{$ENDIF}StringCompareInsensitive;
End;  


Function TAdvStringLargeIntegerMatch.GetByValue(Const aValue: TAdvStringLargeIntegerMatchValue): TAdvStringLargeIntegerMatchKey;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByValue(aValue);

  If ExistsByIndex(iIndex) Then
    Result := KeyByIndex[iIndex]
  Else
    Result := '';
End;  


Function TAdvStringLargeIntegerMatch.GetByKey(Const aKey : TAdvStringLargeIntegerMatchKey): TAdvStringLargeIntegerMatchValue;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else
    Result := 0;
End;  


Function TAdvStringLargeIntegerMatch.IsSortedByKey : Boolean;
Begin
  Result := IsSortedBy({$IFDEF FPC}@{$ENDIF}CompareKey);
End;  


Procedure TAdvStringLargeIntegerMatch.SortedByKey;
Begin 
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareKey);
End;  


Function TAdvStringLargeIntegerMatch.IsSortedByValue : Boolean;
Begin 
  Result := IsSortedBy({$IFDEF FPC}@{$ENDIF}CompareValue);
End;  


Procedure TAdvStringLargeIntegerMatch.SortedByValue;
Begin 
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareValue);
End;


Constructor TAdvStringIntegerMatch.Create;
Begin
  Inherited;

  FSymbol := cReturn;
End;


Destructor TAdvStringIntegerMatch.Destroy;
Begin
  Inherited;
End;


Function TAdvStringIntegerMatch.ErrorClass: EAdvExceptionClass;
Begin
  Result := EAdvStringIntegerMatch;
End;


Function TAdvStringIntegerMatch.Clone: TAdvStringIntegerMatch;
Begin
  Result := TAdvStringIntegerMatch(Inherited Clone);
End;


Function TAdvStringIntegerMatch.Link: TAdvStringIntegerMatch;
Begin
  Result := TAdvStringIntegerMatch(Inherited Link);
End;


Procedure TAdvStringIntegerMatch.AssignItem(oItems: TAdvItemList; iIndex: Integer);
Begin
  FMatchArray^[iIndex] := TAdvStringIntegerMatch(oItems).FMatchArray^[iIndex];
End;


Procedure TAdvStringIntegerMatch.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineString(FMatchArray^[iIndex].Key);
  oFiler['Value'].DefineInteger(FMatchArray^[iIndex].Value);

  oFiler['Match'].DefineEnd;
End;  


Procedure TAdvStringIntegerMatch.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Var
  sKey : TAdvStringIntegerMatchKey;
  iValue : TAdvStringIntegerMatchValue;
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineString(sKey);
  oFiler['Value'].DefineInteger(iValue);

  oFiler['Match'].DefineEnd;

  Add(sKey, iValue);  
End;  


Procedure TAdvStringIntegerMatch.InternalEmpty(iIndex, iLength: Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatchArray) + NativeUInt(iIndex * SizeOf(TAdvStringIntegerMatchItem))), (iLength * SizeOf(TAdvStringIntegerMatchItem)));
End;  


Procedure TAdvStringIntegerMatch.InternalTruncate(iValue: Integer);
Begin
  Inherited;

  // finalize the strings that will be truncated
  If iValue < Count Then
    Finalize(FMatchArray^[iValue], Count - iValue);
End;


Procedure TAdvStringIntegerMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMatchArray, Capacity * SizeOf(TAdvStringIntegerMatchItem), iValue * SizeOf(TAdvStringIntegerMatchItem));
End;  


Procedure TAdvStringIntegerMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatchArray^[iSource], @FMatchArray^[iTarget], iCount * SizeOf(TAdvStringIntegerMatchItem));
End;


Function TAdvStringIntegerMatch.CompareKey(pA, pB: Pointer): Integer;
Begin 
  Result := FCompareKey(PAdvStringIntegerMatchItem(pA)^.Key, PAdvStringIntegerMatchItem(pB)^.Key);
End;  


Function TAdvStringIntegerMatch.CompareValue(pA, pB: Pointer): Integer;
Begin 
  Result := IntegerCompare(PAdvStringIntegerMatchItem(pA)^.Value, PAdvStringIntegerMatchItem(pB)^.Value);
End;  


Procedure TAdvStringIntegerMatch.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin
  aCompare := {$IFDEF FPC}@{$ENDIF}CompareKey;
  FCompareKey := {$IFDEF FPC}@{$ENDIF}StringCompareInsensitive;
End;  


Function TAdvStringIntegerMatch.Find(Const aKey: TAdvStringIntegerMatchKey; Const aValue: TAdvStringIntegerMatchValue; Out iIndex: Integer; aCompare: TAdvItemListCompare): Boolean;
Var
  aItem : TAdvStringIntegerMatchItem;
Begin 
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;  


Function TAdvStringIntegerMatch.IndexByKey(Const aKey : TAdvStringIntegerMatchKey) : Integer;
Begin 
  If Not Find(aKey, 0, Result, {$IFDEF FPC}@{$ENDIF}CompareKey) Then
    Result := -1;
End;  


Function TAdvStringIntegerMatch.ExistsByKey(Const aKey : TAdvStringIntegerMatchKey) : Boolean;
Begin 
  Result := ExistsByIndex(IndexByKey(aKey));
End;


Function TAdvStringIntegerMatch.IndexByValue(Const aValue : TAdvStringIntegerMatchValue) : Integer;
Begin 
  If Not Find('', aValue, Result, {$IFDEF FPC}@{$ENDIF}CompareValue) Then
    Result := -1;
End;  


Function TAdvStringIntegerMatch.ExistsByValue(Const aValue : TAdvStringIntegerMatchValue) : Boolean;
Begin 
  Result := ExistsByIndex(IndexByValue(aValue));
End;  


Function TAdvStringIntegerMatch.Add(Const aKey : TAdvStringIntegerMatchKey; Const aValue : TAdvStringIntegerMatchValue) : Integer;
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


Procedure TAdvStringIntegerMatch.Insert(iIndex : Integer; Const aKey : TAdvStringIntegerMatchKey; Const aValue : TAdvStringIntegerMatchValue);
Begin 
  InternalInsert(iIndex);

  FMatchArray^[iIndex].Key := aKey;
  FMatchArray^[iIndex].Value := aValue;
End;  


Procedure TAdvStringIntegerMatch.InternalInsert(iIndex : Integer);
Begin 
  Inherited;

  Pointer(FMatchArray^[iIndex].Key) := Nil;
  FMatchArray^[iIndex].Value := 0;
End;  


Procedure TAdvStringIntegerMatch.InternalDelete(iIndex : Integer);
Begin 
  Inherited;

  Finalize(FMatchArray^[iIndex]);  
End;


Procedure TAdvStringIntegerMatch.InternalExchange(iA, iB : Integer);
Var
  aTemp : TAdvStringIntegerMatchItem;
  pA : Pointer;
  pB : Pointer;
Begin
  pA := @FMatchArray^[iA];
  pB := @FMatchArray^[iB];

  aTemp := PAdvStringIntegerMatchItem(pA)^;
  PAdvStringIntegerMatchItem(pA)^ := PAdvStringIntegerMatchItem(pB)^;
  PAdvStringIntegerMatchItem(pB)^ := aTemp;
End;


Function TAdvStringIntegerMatch.ForceByKey(Const aKey: TAdvStringIntegerMatchKey): TAdvStringIntegerMatchValue;
Var
  iIndex : Integer;
Begin
  If Not Find(aKey, 0, iIndex, {$IFDEF FPC}@{$ENDIF}CompareKey) Then
    Insert(iIndex, aKey, DefaultValue);

  Result := ValueByIndex[iIndex]
End;


Function TAdvStringIntegerMatch.GetItem(iIndex : Integer) : Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatchArray^[iIndex];
End;  


Procedure TAdvStringIntegerMatch.SetItem(iIndex : Integer; pValue : Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatchArray^[iIndex] := PAdvStringIntegerMatchItem(pValue)^;
End;  


Function TAdvStringIntegerMatch.GetKey(iIndex : Integer) : String;
Begin 
  Assert(ValidateIndex('GetKey', iIndex));

  Result := FMatchArray^[iIndex].Key;
End;  


Procedure TAdvStringIntegerMatch.SetKey(iIndex : Integer; Const aKey : TAdvStringIntegerMatchKey);
Begin 
  Assert(ValidateIndex('SetKey', iIndex));

  FMatchArray^[iIndex].Key := aKey;
End;  


Function TAdvStringIntegerMatch.GetValue(iIndex : Integer) : TAdvStringIntegerMatchValue;
Begin 
  Assert(ValidateIndex('GetValue', iIndex));

  Result := FMatchArray^[iIndex].Value;
End;  


Procedure TAdvStringIntegerMatch.SetValue(iIndex : Integer; Const aValue : TAdvStringIntegerMatchValue);
Begin 
  Assert(ValidateIndex('SetValue', iIndex));

  FMatchArray^[iIndex].Value := aValue;
End;  


Function TAdvStringIntegerMatch.GetMatch(Const aKey : TAdvStringIntegerMatchKey) : TAdvStringIntegerMatchValue;
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


Procedure TAdvStringIntegerMatch.SetMatch(Const aKey : TAdvStringIntegerMatchKey; Const aValue : TAdvStringIntegerMatchValue);
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


Function TAdvStringIntegerMatch.GetAsText : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, KeyByIndex[iLoop] + '=' + IntegerToString(ValueByIndex[iLoop]), FSymbol);
End;


Function TAdvStringIntegerMatch.GetKeysAsText : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, KeyByIndex[iLoop], FSymbol);
End;  


Function TAdvStringIntegerMatch.CapacityLimit : Integer;
Begin 
  Result := High(TAdvStringIntegerMatchItemArray);
End;


Function TAdvStringIntegerMatch.GetSensitive : Boolean;
Var
  aCompare : TAdvStringCompareCallback;
Begin 
  aCompare := {$IFDEF FPC}@{$ENDIF}StringCompareSensitive;

  Result := @FCompareKey = @aCompare;
End;


Procedure TAdvStringIntegerMatch.SetSensitive(Const Value: Boolean);
Begin
  If Value Then
    FCompareKey := {$IFDEF FPC}@{$ENDIF}StringCompareSensitive
  Else
    FCompareKey := {$IFDEF FPC}@{$ENDIF}StringCompareInsensitive;
End;  


Function TAdvStringIntegerMatch.GetKeyByValue(Const aValue: TAdvStringIntegerMatchValue): TAdvStringIntegerMatchKey;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByValue(aValue);

  If ExistsByIndex(iIndex) Then
    Result := KeyByIndex[iIndex]
  Else
    Result := DefaultKey;
End;


Function TAdvStringIntegerMatch.GetValueByKey(Const aKey : TAdvStringIntegerMatchKey): TAdvStringIntegerMatchValue;
Var
  iIndex : Integer;
Begin 
  iIndex := IndexByKey(aKey);

  If ExistsByIndex(iIndex) Then
    Result := ValueByIndex[iIndex]
  Else
    Result := DefaultValue;
End;  


Function TAdvStringIntegerMatch.IsSortedByKey : Boolean;
Begin 
  Result := IsSortedBy({$IFDEF FPC}@{$ENDIF}CompareKey);
End;  


Procedure TAdvStringIntegerMatch.SortedByKey;
Begin 
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareKey);
End;  


Function TAdvStringIntegerMatch.IsSortedByValue : Boolean;
Begin 
  Result := IsSortedBy({$IFDEF FPC}@{$ENDIF}CompareValue);
End;  


Procedure TAdvStringIntegerMatch.SortedByValue;
Begin 
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareValue);
End;  


Procedure TAdvStringIntegerMatch.AddAll(oStringIntegerMatch: TAdvStringIntegerMatch);
Var
  iIndex : Integer;
Begin
  Assert(CheckCondition(oStringIntegerMatch <> Self, 'AddAll', 'Cannot add all items from a list to itself.'));

  Capacity := IntegerMax(Count + oStringIntegerMatch.Count, Capacity);

  For iIndex := 0 To oStringIntegerMatch.Count - 1 Do
    Add(oStringIntegerMatch.KeyByIndex[iIndex], oStringIntegerMatch.ValueByIndex[iIndex]);
End;


Procedure TAdvStringHashEntry.Generate;
Begin 
  Code := FHIR.Support.System.HashStringToCode32(FName);
End;


Procedure TAdvStringHashEntry.SetName(Const Value: String);
Begin 
  FName := Value;

  Generate;
End;


Procedure TAdvStringHashEntry.Assign(oSource: TAdvObject);
Begin 
  Inherited;

  Name := TAdvStringHashEntry(oSource).Name;
End;


Procedure TAdvStringHashEntry.Define(oFiler : TAdvFiler);
Begin 
  Inherited;

  oFiler['Name'].DefineString(FName);
End;  


Function TAdvStringHashTable.Equal(oA, oB: TAdvHashEntry): Integer;
Begin
  Result := Inherited Equal(oA, oB);

  If Result = 0 Then
    Result := StringCompare(TAdvStringHashEntry(oA).Name, TAdvStringHashEntry(oB).Name);
End;  


Function TAdvStringHashTable.ItemClass : TAdvHashEntryClass;
Begin 
  Result := TAdvStringHashEntry;
End;  


Function TAdvStringHashTable.Iterator : TAdvIterator;
Begin
  Result := TAdvStringHashTableIterator.Create;
  TAdvStringHashTableIterator(Result).HashTable := Self.Link;
End;  


Function TAdvStringHashTableIterator.Current : TAdvStringHashEntry;
Begin 
  Result := TAdvStringHashEntry(Inherited Current);
End;


Function TAdvStringHashTable.Link: TAdvStringHashTable;
Begin
  Result := TAdvStringHashTable(Inherited Link);
End;


Function TAdvPersistentList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvPersistent;
End;


Function TAdvPersistentList.GetPersistentByIndex(Const iIndex : Integer) : TAdvPersistent;
Begin
  Result := TAdvPersistent(ObjectByIndex[iIndex]);
End;


Procedure TAdvPersistentList.SetPersistentByIndex(Const iIndex : Integer; Const oValue : TAdvPersistent);
Begin
  ObjectByIndex[iIndex] := oValue;
End;


Destructor TAdvOrdinalSet.Destroy;
Begin
  PartsDispose;

  Inherited;
End;


Procedure TAdvOrdinalSet.PartsDispose;
Begin
  If FOwns Then
    MemoryDestroy(FPartArray, FSize);

  FOwns := False;
  FPartArray := Nil;
  FSize := 0;
  FCount := 0;
End;


Procedure TAdvOrdinalSet.PartsNew;
Begin
  If Not FOwns Then
  Begin
    FPartArray := Nil;
    MemoryCreate(FPartArray, FSize);
    MemoryZero(FPartArray, FSize);
    FOwns := True;
  End;
End;


Procedure TAdvOrdinalSet.SetCount(Const iValue: Integer);
Begin
  FCount := iValue;
  Resize(RealCeiling(Count / 8));
End;


Procedure TAdvOrdinalSet.SetSize(Const Value: Integer);
Begin
  Resize(Value);
  FCount := (FSize * 8);
End;  


Procedure TAdvOrdinalSet.Assign(oObject: TAdvObject);
Begin
  Inherited;

  PartsDispose;

  FSize := TAdvOrdinalSet(oObject).Size;
  FCount := TAdvOrdinalSet(oObject).Count;

  PartsNew;

  MemoryMove(TAdvOrdinalSet(oObject).FPartArray, FPartArray, FSize);
End;


Procedure TAdvOrdinalSet.Define(oFiler: TAdvFiler);
Begin
  Inherited;
End;


Function TAdvOrdinalSet.ValidateIndex(Const sMethod : String; Const iIndex : Integer) : Boolean;
Begin
  If Not IntegerBetweenInclusive(0, iIndex, FCount - 1) Then
    Invariant(sMethod, StringFormat('Invalid index (%d In [0..%d])', [iIndex, FCount - 1]));

  Result := True;
End;


Procedure TAdvOrdinalSet.Fill(bChecked : Boolean);
Begin
  If bChecked Then
    MemoryFill(FPartArray, FSize)
  Else
    MemoryZero(FPartArray, FSize);
End;


Procedure TAdvOrdinalSet.Resize(Const iValue: Integer);
Begin
  If FOwns Then
  Begin
    MemoryResize(FPartArray, FSize, iValue);
    MemoryZero(Pointer(NativeUInt(FPartArray) + NativeUInt(FSize)), iValue - FSize);
  End;

  FSize := iValue;
End;


Function TAdvOrdinalSet.Iterator : TAdvIterator;
Begin
  Result := TAdvOrdinalSetIterator.Create;
  TAdvOrdinalSetIterator(Result).OrdinalSet := TAdvOrdinalSet(Self.Link);
End;


Procedure TAdvOrdinalSet.New(Const iCount: Integer);
Begin
  PartsDispose;

  FCount := iCount;
  FSize := RealCeiling(iCount / 8);

  PartsNew;
End;


Procedure TAdvOrdinalSet.Hook(Const aValue; iCount : Integer);
Begin
  FPartArray := PAdvOrdinalSetPartArray(@aValue);
  FCount := iCount;
  FSize := RealCeiling(iCount / 8);
  FOwns := False;
End;


Procedure TAdvOrdinalSet.Unhook;
Begin
  FPartArray := Nil;
  FCount := 0;
  FSize := 0;
End;


Procedure TAdvOrdinalSet.Check(iIndex: Integer);
Var
  pPart : PAdvOrdinalSetPart;
Begin
  Assert(ValidateIndex('Check', iIndex));

  pPart := @FPartArray^[iIndex Div 32];

  pPart^ := pPart^ Or (1 Shl SignedMod(iIndex, 32));
End;


Procedure TAdvOrdinalSet.CheckRange(iFromIndex, iToIndex : Integer);
Var
  iLoop : Integer;
Begin
  For iLoop := iFromIndex To iToIndex Do
    Check(iLoop);
End;


Procedure TAdvOrdinalSet.UncheckRange(iFromIndex, iToIndex : Integer);
Var
  iLoop : Integer;
Begin
  For iLoop := iFromIndex To iToIndex Do
    Uncheck(iLoop);
End;


Procedure TAdvOrdinalSet.Uncheck(iIndex: Integer);
Var
  pPart : PAdvOrdinalSetPart;
Begin
  Assert(ValidateIndex('Uncheck', iIndex));

  pPart := @FPartArray^[iIndex Div 32];

  pPart^ := pPart^ And Not (1 Shl SignedMod(iIndex, 32));
End;


Procedure TAdvOrdinalSet.Toggle(iIndex: Integer);
Var
  pPart : PAdvOrdinalSetPart;
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


Function TAdvOrdinalSet.Checked(iIndex: Integer): Boolean;
Begin
  Assert(ValidateIndex('Checked', iIndex));

  Result := FPartArray^[iIndex Div 32] And (1 Shl SignedMod(iIndex, 32)) <> 0;
End;


Function TAdvOrdinalSet.CheckedRange(iFromIndex, iToIndex: Integer): Boolean;
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


Function TAdvOrdinalSet.AllChecked : Boolean;
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


Function TAdvOrdinalSet.AnyChecked : Boolean;
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


Function TAdvOrdinalSet.NoneChecked : Boolean;
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


Procedure TAdvOrdinalSetIterator.First;
Begin
  FPart := Low(FOrdinalSet.FPartArray^);
  FIndex := 0;
  FLoop := 0;

  // TODO: can FLoop be cached as (1 Shl FLoop) for optimisation?

  If FPart <= High(FOrdinalSet.FPartArray^) Then
    FValue := @(FOrdinalSet.FPartArray^[FPart]);
End;


Function TAdvOrdinalSetIterator.Checked : Boolean;
Begin
  Result := (FValue^ And (1 Shl FLoop)) <> 0;
End;  


Procedure TAdvOrdinalSetIterator.Check;
Begin 
  FValue^ := FValue^ Or (1 Shl FLoop);
End;  


Procedure TAdvOrdinalSetIterator.Uncheck;
Begin 
  FValue^ := FValue^ And Not (1 Shl FLoop);
End;  


Procedure TAdvOrdinalSetIterator.Next;
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


Function TAdvOrdinalSetIterator.More : Boolean;
Begin 
  Result := (FIndex < FOrdinalSet.Count) And (FPart <= High(FOrdinalSet.FPartArray^));
End;  


Constructor TAdvOrdinalSetIterator.Create;
Begin
  Inherited;

  OrdinalSet := Nil;
End;  


Destructor TAdvOrdinalSetIterator.Destroy;
Begin 
  FOrdinalSet.Free;

  Inherited;
End;  


Procedure TAdvOrdinalSetIterator.SetOrdinalSet(Const Value: TAdvOrdinalSet);
Begin 
  FOrdinalSet.Free;
  FOrdinalSet := Value;
End;  


Function TAdvOrdinalSet.GetIsChecked(Const iIndex: Integer): Boolean;
Begin
  Result := Checked(iIndex);
End;


Procedure TAdvOrdinalSet.SetIsChecked(Const iIndex: Integer; Const Value: Boolean);
Begin
  If Value Then
    Check(iIndex)
  Else
    Uncheck(iIndex);
End;


Procedure TAdvOrdinalSet.CheckAll;
Begin
  Fill(True);
End;


Procedure TAdvOrdinalSet.UncheckAll;
Begin
  Fill(False);
End;



Constructor TAdvObjectMatch.Create;
Begin
  Inherited;

  FKeyComparisonDelegate := CompareByKeyReference;
  FValueComparisonDelegate := CompareByValueReference;

  FNominatedKeyClass := ItemKeyClass;
  FNominatedValueClass := ItemValueClass;
End;


Destructor TAdvObjectMatch.Destroy;
Begin
  Inherited;
End;


Function TAdvObjectMatch.Link : TAdvObjectMatch;
Begin
  Result := TAdvObjectMatch(Inherited Link);
End;


Function TAdvObjectMatch.CompareByKeyReference(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(Integer(PAdvObjectMatchItem(pA)^.Key), Integer(PAdvObjectMatchItem(pB)^.Key));
End;  


Function TAdvObjectMatch.CompareByValueReference(pA, pB: Pointer): Integer;
Begin 
  Result := IntegerCompare(Integer(PAdvObjectMatchItem(pA)^.Value), Integer(PAdvObjectMatchItem(pB)^.Value));
End;  


Procedure TAdvObjectMatch.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin 
  oFiler['Match'].DefineBegin;

  oFiler['Key'].DefineObject(FMatchArray^[iIndex].Key);
  oFiler['Value'].DefineObject(FMatchArray^[iIndex].Value);

  oFiler['Match'].DefineEnd;
End;


Procedure TAdvObjectMatch.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Var
  oKey : TAdvObjectMatchKey;
  oValue : TAdvObjectMatchValue;
Begin 
  oKey := Nil;
  oValue := Nil;
  Try
    oFiler['Match'].DefineBegin;

    oFiler['Key'].DefineObject(oKey);
    oFiler['Value'].DefineObject(oValue);

    oFiler['Match'].DefineEnd;

    Add(oKey.Link, oValue.Link);
  Finally
    oKey.Free;
    oValue.Free;
  End;  
End;  


Procedure TAdvObjectMatch.AssignItem(oItems : TAdvItemList; iIndex: Integer);
Begin 
  Inherited;

  FMatchArray^[iIndex].Key := TAdvObjectMatch(oItems).FMatchArray^[iIndex].Key.Clone;
  FMatchArray^[iIndex].Value := TAdvObjectMatch(oItems).FMatchArray^[iIndex].Value.Clone;  
End;  


Procedure TAdvObjectMatch.InternalEmpty(iIndex, iLength : Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMatchArray) + NativeUInt(iIndex * SizeOf(TAdvObjectMatchItem))), (iLength * SizeOf(TAdvObjectMatchItem)));
End;


Procedure TAdvObjectMatch.InternalTruncate(iValue : Integer);
Var
  iLoop : Integer;
  oKey  : TAdvObjectMatchKey;
  oValue : TAdvObjectMatchValue;
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


Procedure TAdvObjectMatch.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMatchArray, Capacity * SizeOf(TAdvObjectMatchItem), iValue * SizeOf(TAdvObjectMatchItem));
End;


Procedure TAdvObjectMatch.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  MemoryMove(@FMatchArray^[iSource], @FMatchArray^[iTarget], iCount * SizeOf(TAdvObjectMatchItem));
End;  


Function TAdvObjectMatch.IndexByKey(Const aKey: TAdvObjectMatchKey): Integer;
Begin
  If Not Find(aKey, Nil, Result, FKeyComparisonDelegate) Then
    Result := -1;
End;


Function TAdvObjectMatch.IndexByValue(Const aValue: TAdvObjectMatchValue): Integer;
Begin
  If Not Find(Nil, aValue, Result, FValueComparisonDelegate) Then
    Result := -1;
End;


Function TAdvObjectMatch.ExistsByKey(Const aKey: TAdvObjectMatchKey): Boolean;
Begin
  Result := ExistsByIndex(IndexByKey(aKey));
End;


Function TAdvObjectMatch.ExistsByValue(Const aValue: TAdvObjectMatchValue): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(aValue));
End;


Function TAdvObjectMatch.Add(Const aKey : TAdvObjectMatchKey; Const aValue : TAdvObjectMatchValue) : Integer;
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


Procedure TAdvObjectMatch.Insert(iIndex : Integer; Const aKey : TAdvObjectMatchKey; Const aValue : TAdvObjectMatchValue);
Begin
  Assert(ValidateKey('Insert', aKey, 'aKey'));
  Assert(ValidateValue('Insert', aValue, 'aValue'));

  InternalInsert(iIndex);

  FMatchArray^[iIndex].Key := aKey;
  FMatchArray^[iIndex].Value := aValue;
End;


Procedure TAdvObjectMatch.InternalInsert(iIndex : Integer);
Begin
  Inherited;

  Pointer(FMatchArray^[iIndex].Key) := Nil;
  Pointer(FMatchArray^[iIndex].Value) := Nil;
End;


Procedure TAdvObjectMatch.InternalDelete(iIndex : Integer);
Begin
  Inherited;

  FMatchArray^[iIndex].Key.Free;
  FMatchArray^[iIndex].Key := Nil;

  FMatchArray^[iIndex].Value.Free;
  FMatchArray^[iIndex].Value := Nil;
End;


Procedure TAdvObjectMatch.InternalExchange(iA, iB: Integer);
Var
  aTemp : TAdvObjectMatchItem;
  pA : Pointer;
  pB : Pointer;
Begin
  pA := @FMatchArray^[iA];
  pB := @FMatchArray^[iB];

  aTemp := PAdvObjectMatchItem(pA)^;
  PAdvObjectMatchItem(pA)^ := PAdvObjectMatchItem(pB)^;
  PAdvObjectMatchItem(pB)^ := aTemp;
End;


Procedure TAdvObjectMatch.Delete(Const aKey: TAdvObjectMatchKey; Const aValue: TAdvObjectMatchValue);
Var
  iIndex : Integer;
Begin
  If Not Find(aKey, aValue, iIndex) Then
    RaiseError('Delete', 'Key/Value pair could not be found in the match.');

  DeleteByIndex(iIndex);
End;


Function TAdvObjectMatch.GetItem(iIndex : Integer): Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatchArray^[iIndex];
End;


Procedure TAdvObjectMatch.SetItem(iIndex: Integer; aValue: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatchArray^[iIndex] := PAdvObjectMatchItem(aValue)^;
End;  


Function TAdvObjectMatch.GetKeyByIndex(iIndex : Integer): TAdvObjectMatchKey;
Begin 
  Assert(ValidateIndex('GetKeyByIndex', iIndex));

  Result := FMatchArray^[iIndex].Key;
End;


Procedure TAdvObjectMatch.SetKeyByIndex(iIndex : Integer; Const aKey : TAdvObjectMatchKey);
Begin 
  Assert(ValidateIndex('SetKeyByIndex', iIndex));
  Assert(ValidateKey('SetKeyByIndex', aKey, 'aKey'));

  FMatchArray^[iIndex].Key.Free;
  FMatchArray^[iIndex].Key := aKey;
End;  


Function TAdvObjectMatch.GetValueByIndex(iIndex : Integer): TAdvObjectMatchValue;
Begin 
  Assert(ValidateIndex('GetValueByIndex', iIndex));

  Result := FMatchArray^[iIndex].Value;
End;  


Procedure TAdvObjectMatch.SetValueByIndex(iIndex : Integer; Const aValue: TAdvObjectMatchValue);
Begin 
  Assert(ValidateIndex('SetValueByIndex', iIndex));
  Assert(ValidateValue('SetValueByIndex', aValue, 'aValue'));

  FMatchArray^[iIndex].Value.Free;
  FMatchArray^[iIndex].Value := aValue;
End;


Function TAdvObjectMatch.GetValueByKey(Const aKey : TAdvObjectMatchKey): TAdvObjectMatchValue;
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


Procedure TAdvObjectMatch.SetValueByKey(Const aKey : TAdvObjectMatchKey; Const aValue: TAdvObjectMatchValue);
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


Function TAdvObjectMatch.GetAsText : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := 0 To Count - 1 Do
    Result := Result + KeyByIndex[iLoop].ClassName + '=' + ValueByIndex[iLoop].ClassName + #13#10;
End;  


Procedure TAdvObjectMatch.Merge(oMatch: TAdvObjectMatch);
Var
  iLoop : Integer;
Begin 
  For iLoop := 0 To oMatch.Count - 1 Do
    Add(oMatch.KeyByIndex[iLoop].Link, oMatch.ValueByIndex[iLoop].Link);
End;


Function TAdvObjectMatch.CapacityLimit : Integer;
Begin 
  Result := High(TAdvObjectMatchItemArray);
End;  


Procedure TAdvObjectMatch.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin
  aCompare := CompareByKeyReference;
End;  


Function TAdvObjectMatch.Find(Const aKey : TAdvObjectMatchKey; Const aValue : TAdvObjectMatchValue; Out iIndex : Integer; aCompare : TAdvItemListCompare) : Boolean;
Var
  aItem : TAdvObjectMatchItem;
Begin
  Assert(ValidateKey('Find', aKey, 'aKey'));
  Assert(ValidateValue('Find', aValue, 'aValue'));

  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Find(@aItem, iIndex, aCompare);
End;


Function TAdvObjectMatch.IsSortedByKey : Boolean;
Begin 
  Result := IsSortedBy(FKeyComparisonDelegate);
End;


Function TAdvObjectMatch.IsSortedByValue : Boolean;
Begin
  Result := IsSortedBy(FValueComparisonDelegate);
End;  


Function TAdvObjectMatch.ValidateIndex(Const sMethod : String; iIndex: Integer): Boolean;
Begin 
  Inherited ValidateIndex(sMethod, iIndex);

  ValidateKey(sMethod, FMatchArray^[iIndex].Key, 'FMatchArray^[' + IntegerToString(iIndex) + '].Key');
  ValidateValue(sMethod, FMatchArray^[iIndex].Value, 'FMatchArray^[' + IntegerToString(iIndex) + '].Value');

  Result := True;
End;  


Function TAdvObjectMatch.ValidateKey(Const sMethod : String; oObject : TAdvObject; Const sObject : String) : Boolean;
Begin 
  If Assigned(oObject) Then
    Invariants(sMethod, oObject, FNominatedKeyClass, sObject);

  Result := True;
End;  


Function TAdvObjectMatch.ValidateValue(Const sMethod : String; oObject : TAdvObject; Const sObject : String) : Boolean;
Begin 
  If Assigned(oObject) Then
    Invariants(sMethod, oObject, FNominatedValueClass, sObject);

  Result := True;
End;


Function TAdvObjectMatch.ItemKeyClass : TAdvObjectClass;
Begin
  Result := TAdvObject;
End;


Function TAdvObjectMatch.ItemValueClass : TAdvObjectClass;
Begin
  Result := TAdvObject;
End;


Procedure TAdvObjectMatch.SortedByKey;
Begin
  SortedBy(FKeyComparisonDelegate);
End;


Procedure TAdvObjectMatch.SortedByValue;
Begin
  SortedBy(FValueComparisonDelegate);
End;


Function TAdvObjectMatch.GetKeyByValue(Const aValue: TAdvObjectMatchValue): TAdvObjectMatchKey;
Var
  iIndex : Integer;
Begin
  iIndex := IndexByValue(aValue);

  If ExistsByIndex(iIndex) Then
    Result := KeyByIndex[iIndex]
  Else
    Result := FDefaultKey;
End;


Function TAdvObjectMatch.GetMatchByIndex(iIndex : Integer) : TAdvObjectMatchItem;
Begin
  Assert(ValidateIndex('GetMatchByIndex', iIndex));

  Result := FMatchArray^[iIndex];
End;


constructor TAdvObjectList.Create;
begin
  inherited;

  FItemClass := ItemClass;
end;


destructor TAdvObjectList.Destroy;
begin
  inherited;
end;


function TAdvObjectList.Link: TAdvObjectList;
begin
  Result := TAdvObjectList(inherited Link);
end;


function TAdvObjectList.Clone: TAdvObjectList;
begin
  Result := TAdvObjectList(inherited Clone);
end;


function TAdvObjectList.ErrorClass: EAdvExceptionClass;
begin
  Result := EAdvObjectList;
end;


procedure TAdvObjectList.AssignItem(oItems: TAdvItemList; iIndex: integer);
begin
  FObjectArray^[iIndex] := TAdvObjectList(oItems).FObjectArray^[iIndex].Clone;
end;


procedure TAdvObjectList.SaveItem(oFiler: TAdvFiler; iIndex: integer);
begin
  oFiler['Object'].DefineObject(FObjectArray^[iIndex]);
end;


procedure TAdvObjectList.LoadItem(oFiler: TAdvFiler; iIndex: integer);
var
  oObject: TAdvObject;
begin
  oObject := nil;
  try
    oFiler['Object'].DefineObject(oObject);

    Add(oObject.Link);
  finally
    oObject.Free;
  end;
end;


function TAdvObjectList.ValidateIndex(const sMethod: string; iIndex: integer): boolean;
begin
  Result := inherited ValidateIndex(sMethod, iIndex);

  ValidateItem(sMethod, FObjectArray^[iIndex], 'FObjectArray^[' +
    IntegerToString(iIndex) + ']');
end;


function TAdvObjectList.ValidateItem(const sMethod: string;
  oObject: TAdvObject; const sObject: string): boolean;
begin
  if Assigned(oObject) or not AllowUnassigned then
    Invariants(sMethod, oObject, FItemClass, sObject);

  Result := True;
end;


procedure TAdvObjectList.InternalTruncate(iValue: integer);
var
  oValue: TAdvObject;
  iLoop: integer;
begin
  inherited;

  for iLoop := iValue to Count - 1 do
  begin
    oValue := FObjectArray^[iLoop];

    InternalBeforeExclude(iLoop);

    FObjectArray^[iLoop] := nil;

    Assert(not Assigned(oValue) or Invariants('InternalResize', oValue,
      FItemClass, 'oValue'));

    oValue.Free;
  end;
end;


procedure TAdvObjectList.InternalResize(iValue: integer);
begin
  inherited;

  MemoryResize(FObjectArray, Capacity * SizeOf(TAdvObject), iValue * SizeOf(TAdvObject));
end;


procedure TAdvObjectList.InternalEmpty(iIndex, iLength: integer);
begin
  inherited;

  MemoryZero(Pointer(NativeUInt(FObjectArray) + NativeUInt((iIndex * SizeOf(TAdvObject)))),
    (iLength * SizeOf(TAdvObject)));
end;


procedure TAdvObjectList.InternalCopy(iSource, iTarget, iCount: integer);
begin
  inherited;

  MemoryMove(@FObjectArray^[iSource], @FObjectArray^[iTarget], iCount *
    SizeOf(TAdvObject));
end;


procedure TAdvObjectList.InternalInsert(iIndex: integer);
begin
  inherited;

  FObjectArray^[iIndex] := nil;
end;


procedure TAdvObjectList.InternalDelete(iIndex: integer);
begin
  inherited;

  InternalBeforeExclude(iIndex);

  FObjectArray^[iIndex].Free;
  FObjectArray^[iIndex] := nil;
end;


function TAdvObjectList.ItemClass: TAdvObjectClass;
begin
  Result := TAdvObject;
end;


function TAdvObjectList.ItemNew: TAdvObject;
begin
  Result := FItemClass.Create;
end;


procedure TAdvObjectList.Collect(oList: TAdvObjectList);
begin
  oList.Clear;
  oList.AddAll(Self);
end;


procedure TAdvObjectList.AddAll(oList: TAdvObjectList);
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


procedure TAdvObjectList.DeleteByDefault(oValue: TAdvObject);
begin
  DeleteBy(oValue, DefaultComparison);
end;


procedure TAdvObjectList.DeleteBy(oValue: TAdvObject; aCompare: TAdvItemListCompare);
var
  iIndex: integer;
begin
  Assert(ValidateItem('DeleteBy', oValue, 'oValue'));

  if not Find(oValue, iIndex, aCompare) then
    RaiseError('DeleteBy', 'Object not found in list.');

  DeleteByIndex(iIndex);
end;


function TAdvObjectList.IndexBy(oValue: TAdvObject; aCompare: TAdvItemListCompare): integer;
begin
  if not Find(oValue, Result, aCompare) then
    Result := -1;
end;


function TAdvObjectList.ExistsBy(oValue: TAdvObject;
  aCompare: TAdvItemListCompare): boolean;
var
  iIndex: integer;
begin
  Result := not IsEmpty and Find(oValue, iIndex, aCompare);
end;


function TAdvObjectList.GetBy(oValue: TAdvObject;
  aCompare: TAdvItemListCompare): TAdvObject;
var
  iIndex: integer;
begin
  if Find(oValue, iIndex, aCompare) then
    Result := ObjectByIndex[iIndex]
  else
    Result := nil;
end;


function TAdvObjectList.ForceBy(oValue: TAdvObject;
  aCompare: TAdvItemListCompare): TAdvObject;
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


function TAdvObjectList.CompareByReference(pA, pB: Pointer): integer;
begin
  Result := IntegerCompare(integer(pA), integer(pB));
end;


function TAdvObjectList.CompareByClass(pA, pB: Pointer): integer;
var
  aClassA: TAdvObjectClass;
  aClassB: TAdvObjectClass;
begin
  if Assigned(pA) then
    aClassA := TAdvObject(pA).ClassType
  else
    aClassA := nil;

  if Assigned(pB) then
    aClassB := TAdvObject(pB).ClassType
  else
    aClassB := nil;

  Result := IntegerCompare(integer(aClassA), integer(aClassB));
end;


function TAdvObjectList.Find(oValue: TAdvObject): integer;
begin
  Find(oValue, Result);
end;


function TAdvObjectList.IndexByClass(aClass: TAdvObjectClass): integer;
var
  oValue: TAdvObject;
begin
  Assert(Invariants('IndexByClass', aClass, FItemClass, 'aClass'));

  oValue := aClass.Create;
  try
    Result := IndexByClass(oValue);
  finally
    oValue.Free;
  end;
end;


function TAdvObjectList.IndexByClass(oValue: TAdvObject): integer;
begin
  Assert(ValidateItem('IndexByClass', oValue, 'oValue'));

  Result := IndexBy(oValue,
{$IFDEF FPC}
    @
{$ENDIF}
    CompareByClass);
end;


function TAdvObjectList.ExistsByClass(aClass: TAdvObjectClass): boolean;
begin
  Result := ExistsByIndex(IndexByClass(aClass));
end;


function TAdvObjectList.GetByClass(aClass: TAdvObjectClass): TAdvObject;
begin
  Result := Get(IndexByClass(aClass));
end;


function TAdvObjectList.IndexByReference(oValue: TAdvObject): integer;
begin
  Assert(ValidateItem('IndexByReference', oValue, 'oValue'));

  Result := IndexBy(oValue,
{$IFDEF FPC}
    @
{$ENDIF}
    CompareByReference);
end;


function TAdvObjectList.ExistsByReference(oValue: TAdvObject): boolean;
begin
  Result := ExistsByIndex(IndexByReference(oValue));
end;


function TAdvObjectList.IndexByDefault(oValue: TAdvObject): integer;
begin
  Assert(ValidateItem('IndexByDefault', oValue, 'oValue'));

  Result := IndexBy(oValue, DefaultComparison);
end;


function TAdvObjectList.ExistsByDefault(oValue: TAdvObject): boolean;
begin
  Result := ExistsByIndex(IndexByDefault(oValue));
end;


function TAdvObjectList.Add(oValue: TAdvObject): integer;
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


procedure TAdvObjectList.Insert(iIndex: integer; oValue: TAdvObject);
begin
  Assert(ValidateItem('Insert', oValue, 'oValue'));
  Assert(CheckCondition(not IsSorted or (Find(oValue) = iIndex), 'Insert',
    'Cannot insert into a sorted list unless the index is correct.'));
  Assert(Insertable('Insert', oValue));

  InternalInsert(iIndex);

  FObjectArray^[iIndex] := oValue;

  InternalAfterInclude(iIndex);
end;


procedure TAdvObjectList.DeleteByReference(oValue: TAdvObject);
begin
  DeleteBy(oValue,
{$IFDEF FPC}
    @
{$ENDIF}
    CompareByReference);
end;


procedure TAdvObjectList.DeleteAllByReference(oValue: TAdvObjectList);
var
  iIndex: integer;
begin
  for iIndex := 0 to oValue.Count - 1 do
    DeleteByReference(oValue[iIndex]);
end;


procedure TAdvObjectList.DeleteByClass(aClass: TAdvObjectClass);
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


function TAdvObjectList.RemoveLast: TAdvObject;
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


function TAdvObjectList.RemoveFirst: TAdvObject;
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


procedure TAdvObjectList.InternalExchange(iA, iB: integer);
var
  iTemp: integer;
  pA: Pointer;
  pB: Pointer;
begin
  pA := @FObjectArray^[iA];
  pB := @FObjectArray^[iB];

  iTemp := integer(pA^);
  integer(pA^) := integer(pB^);
  integer(pB^) := iTemp;
end;


procedure TAdvObjectList.Move(iSource, iTarget: integer);
var
  oObject: TAdvObject;
begin
  Assert(CheckCondition(iSource <> iTarget, 'Move', 'Can''t move the same index.'));

  oObject := ObjectByIndex[iSource].Link;
  try
    if iSource < iTarget then
    begin
      Insert(iTarget, oObject.Link);
      DeleteByIndex(iSource);
    end
    else
    begin
      DeleteByIndex(iSource);
      Insert(iTarget, oObject.Link);
    end;
  finally
    oObject.Free;
  end;
end;


function TAdvObjectList.Find(pValue: Pointer; Out iIndex: integer;
  aCompare: TAdvItemListCompare): boolean;
begin
  Assert(Invariants('Find', TAdvObject(pValue), FItemClass, 'pValue'));

  Result := inherited Find(pValue, iIndex, aCompare);
end;


function TAdvObjectList.AddressOfItem(iIndex: integer): PAdvObject;
begin
  Assert(ValidateIndex('AddressOfItem', iIndex));

  Result := @(FObjectArray^[iIndex]);
end;


function TAdvObjectList.GetItemClass: TAdvObjectClass;
begin
  Result := FItemClass;
end;


procedure TAdvObjectList.SetItemClass(const Value: TAdvObjectClass);
begin
  Assert(CheckCondition(Count = 0, 'SetItemClass',
    'Cannot change ItemClass once objects are present in the list.'));

  FItemClass := Value;
end;


function TAdvObjectList.GetItem(iIndex: integer): Pointer;
begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := FObjectArray^[iIndex];
end;


procedure TAdvObjectList.SetItem(iIndex: integer; pValue: Pointer);
begin
  Assert(ValidateIndex('SetItem', iIndex));

  FObjectArray^[iIndex] := TAdvObject(pValue);
end;


function TAdvObjectList.GetObject(iIndex: integer): TAdvObject;
begin
  Assert(ValidateIndex('GetObject', iIndex));

  Result := FObjectArray^[iIndex];
end;


procedure TAdvObjectList.SetObject(iIndex: integer; const oValue: TAdvObject);
begin
  Assert(ValidateIndex('SetObject', iIndex));
  Assert(Replaceable('SetObject', iIndex));
  Assert(Replaceable('SetObject', FObjectArray^[iIndex], oValue));

  InternalBeforeExclude(iIndex);

  FObjectArray^[iIndex].Free;
  FObjectArray^[iIndex] := oValue;

  InternalAfterInclude(iIndex);
end;


function TAdvObjectList.CapacityLimit: integer;
begin
  Result := High(TAdvObjectArray);
end;


function TAdvObjectList.Get(iIndex: integer): TAdvObject;
begin
  if ExistsByIndex(iIndex) then
    Result := ObjectByIndex[iIndex]
  else
    Result := nil;
end;


function TAdvObjectList.GetByIndex(iIndex: integer): TAdvObject;
begin
  Result := ObjectByIndex[iIndex];
end;


function TAdvObjectList.Iterator: TAdvIterator;
begin
  Result := ProduceIterator;
end;


procedure TAdvObjectList.SortedByClass;
begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByClass);
end;


procedure TAdvObjectList.OrderedByClass;
begin
  OrderedBy({$IFDEF FPC}@{$ENDIF}CompareByClass);
end;


function TAdvObjectList.IsSortedByClass: boolean;
begin
  Result := IsSortedBy({$IFDEF FPC}@{$ENDIF}CompareByClass);
end;


function TAdvObjectList.IsOrderedByClass: boolean;
begin
  Result := IsOrderedBy({$IFDEF FPC}@{$ENDIF}CompareByClass);
end;


procedure TAdvObjectList.SortedByReference;
begin
  SortedBy({$IFDEF FPC}@{$ENDIF}CompareByReference);
end;


procedure TAdvObjectList.OrderedByReference;
begin
  OrderedBy({$IFDEF FPC}@{$ENDIF}CompareByReference);
end;


function TAdvObjectList.IsSortedByReference: boolean;
begin
  Result := IsSortedBy({$IFDEF FPC}@{$ENDIF}CompareByReference);
end;


function TAdvObjectList.IsOrderedByReference: boolean;
begin
  Result := IsOrderedBy({$IFDEF FPC}@{$ENDIF}CompareByReference);
end;


function TAdvObjectList.Deleteable(const sMethod: string; iIndex: integer): boolean;
begin
  Result := inherited Deleteable(sMethod, iIndex);

  Deleteable(sMethod, FObjectArray^[iIndex]);
end;


function TAdvObjectList.Deleteable(const sMethod: string; oObject: TAdvObject): boolean;
begin
  Alterable(sMethod);

  Result := True;
end;


function TAdvObjectList.Insertable(const sMethod: string; oObject: TAdvObject): boolean;
begin
  Alterable(sMethod);

  Result := True;
end;


function TAdvObjectList.Replaceable(const sMethod: string;
  oOld, oNew: TAdvObject): boolean;
begin
  Alterable(sMethod);
  ValidateItem(sMethod, oOld, 'oOld');
  ValidateItem(sMethod, oNew, 'oNew');

  Result := True;
end;


function TAdvObjectList.Extendable(const sMethod: string; iCount: integer): boolean;
begin
  Result := inherited Extendable(sMethod, iCount);

  if (iCount > Count) and not AllowUnassigned then
    Invariant(sMethod,
      'Unable to extend the Count of the list as it does not allow unassigned entries.');
end;


function TAdvObjectList.New: TAdvObject;
begin
  Result := ItemNew;
end;


procedure TAdvObjectList.InternalAfterInclude(iIndex: integer);
begin
end;


procedure TAdvObjectList.InternalBeforeExclude(iIndex: integer);
begin
end;


function TAdvObjectList.AllowUnassigned: boolean;
begin
  Result := True;
end;


function TAdvObjectList.IteratorClass: TAdvObjectListIteratorClass;
begin
  Result := TAdvObjectListIterator;
end;


procedure TAdvObjectList.ConsumeIterator(oIterator: TAdvObjectListIterator);
begin
  Assert(Invariants('ConsumeIterator', oIterator, IteratorClass, 'oIterator'));
  Assert(CheckCondition(oIterator.List = Self, 'ConsumeIterator',
    'Iterator was not produced by this list.'));

  oIterator.Free;
end;


function TAdvObjectList.ProduceIterator: TAdvObjectListIterator;
begin
  Result := IteratorClass.Create;
  Result.List := Self.Link;
end;


function TAdvObjectList.GetObjectByIndex(const iIndex: integer): TAdvObject;
begin
  Result := GetObject(iIndex);
end;


procedure TAdvObjectList.SetObjectByIndex(const iIndex: integer;
  const Value: TAdvObject);
begin
  SetObject(iIndex, Value);
end;


constructor TAdvObjectListIterator.Create;
begin
  inherited;

  FList := nil;
end;


destructor TAdvObjectListIterator.Destroy;
begin
  FList.Free;

  inherited;
end;


procedure TAdvObjectListIterator.First;
begin
  inherited;

  FIndex := 0;
  FDeleted := False;

  StepNext;
end;


procedure TAdvObjectListIterator.Last;
begin
  inherited;

  FIndex := List.Count - 1;
  FDeleted := False;

  StepBack;
end;


procedure TAdvObjectListIterator.Back;
begin
  inherited;

  Dec(FIndex);
  StepBack;

  FDeleted := False;
end;


procedure TAdvObjectListIterator.Next;
begin
  inherited;

  if not FDeleted then
    Inc(FIndex);

  StepNext;

  FDeleted := False;
end;


function TAdvObjectListIterator.Current: TAdvObject;
begin
  Assert(CheckCondition(not FDeleted, 'Current', 'Current element has been deleted'));

  Result := List[FIndex];
end;


function TAdvObjectListIterator.More: boolean;
begin
  Result := List.ExistsByIndex(FIndex);
end;


procedure TAdvObjectListIterator.Delete;
begin
  Assert(CheckCondition(not FDeleted, 'Delete', 'Current element has already been deleted'));

  List.DeleteByIndex(FIndex);
  FDeleted := True;
end;


function TAdvObjectListIterator.GetList: TAdvObjectList;
begin
  Assert(Invariants('GetList', FList, TAdvObjectList, 'FList'));

  Result := FList;
end;


procedure TAdvObjectListIterator.SetList(const Value: TAdvObjectList);
begin
  Assert(not Assigned(FList) or Invariants('SetList', Value, TAdvObjectList, 'Value'));

  FList.Free;
  FList := Value;
end;


procedure TAdvObjectListIterator.StepBack;
begin
  while More and Skip do
    Dec(FIndex);
end;


procedure TAdvObjectListIterator.StepNext;
begin
  while More and Skip do
    Inc(FIndex);
end;


function TAdvObjectListIterator.Skip: boolean;
begin
  Result := False;
end;


function TAdvObjectList.ContainsAllByReference(oObjectList: TAdvObjectList): boolean;
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


function TAdvObjectList.ContainsAnyByReference(oObjectList: TAdvObjectList): boolean;
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


Constructor TAdvItemList.Create;
Begin
  Inherited;

  DefaultCompare(FComparison);

  FDirection := 1;
End;


Destructor TAdvItemList.Destroy;
Begin
  InternalResize(0);

  Inherited;
End;


Function TAdvItemList.ErrorClass : EAdvExceptionClass;
Begin
  Result := EAdvItemList;
End;


Procedure TAdvItemList.Assign(oSource: TAdvObject);
Var
  iLoop : Integer;
Begin
  Inherited;

  Clear;

  Assert(CheckCondition(Count = 0, 'Assign', 'Collection must be empty of all items after clear.'));

  Count := TAdvItemList(oSource).Count;

  For iLoop := 0 To Count - 1 Do
    AssignItem(TAdvItemList(oSource), iLoop);

  If Count > 0 Then
  Begin
    // TODO: below reflects what is currently happening due to AssignItem. This
    //       should be changed to use an 'Add' approach similar to LoadItem so that
    //       you can assign from one ordered list into another and retain individual ordering rules.

    // Comparison is set this way because we can't directly assign a method pointer off another object.
    TMethod(FComparison).Data := Self;
    TMethod(FComparison).Code := TMethod(TAdvItemList(oSource).FComparison).Code;

    FSorted := TAdvItemList(oSource).FSorted;
    FDirection := TAdvItemList(oSource).FDirection;
    FDuplicates := TAdvItemList(oSource).FDuplicates;
  End;
End;


Procedure TAdvItemList.Load(oFiler : TAdvFiler);
Begin
  Assert(Invariants('Load', oFiler, TAdvFiler, 'oFiler'));

  Clear;

  Define(oFiler);

  oFiler['Items'].DefineBegin;

  While oFiler.Peek <> atEnd Do
    LoadItem(oFiler, Count);

  oFiler['Items'].DefineEnd;
End;


Procedure TAdvItemList.Save(oFiler : TAdvFiler);
Var
  iLoop  : Integer;
Begin
  Assert(Invariants('Save', oFiler, TAdvFiler, 'oFiler'));

  Define(oFiler);

  oFiler['Items'].DefineBegin;

  For iLoop := 0 To Count - 1 Do
    SaveItem(oFiler, iLoop);

  oFiler['Items'].DefineEnd;
End;


Procedure TAdvItemList.Define(oFiler: TAdvFiler);
Begin
  Inherited;
End;


Procedure TAdvItemList.LoadItem(oFiler: TAdvFiler; iIndex: Integer);
Begin
End;


Procedure TAdvItemList.SaveItem(oFiler: TAdvFiler; iIndex: Integer);
Begin
End;


Procedure TAdvItemList.AssignItem(oItems : TAdvItemList; iIndex: Integer);
Begin
End;


Function TAdvItemList.ExistsByIndex(Const iIndex : Integer) : Boolean;
Begin
  Result := (iIndex >= 0) And (iIndex < FCount);
End;


Function TAdvItemList.ValidateIndex(Const sMethod: String; iIndex: Integer) : Boolean;
Begin
  If Not ExistsByIndex(iIndex) Then
    Invariant(sMethod, StringFormat('Invalid index (%d In [0..%d])', [iIndex, FCount - 1]));

  Result := True;
End;


Procedure TAdvItemList.InternalClear;
Begin
  Inherited;

  InternalTruncate(0);
  FCount := 0;
End;


Procedure TAdvItemList.InternalGrow;
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


Procedure TAdvItemList.InternalEmpty(iIndex, iLength: Integer);
Begin
End;


Procedure TAdvItemList.InternalResize(iValue : Integer);
Begin
End;


Procedure TAdvItemList.InternalTruncate(iValue: Integer);
Begin
End;


Procedure TAdvItemList.InternalCopy(iSource, iTarget, iCount: Integer);
Begin
End;


Procedure TAdvItemList.InternalSort;

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


Procedure TAdvItemList.InternalSort(aCompare: TAdvItemListCompare; iDirection : TAdvItemListDirection);
Begin
  Assert(CheckCondition(Assigned(aCompare), 'Sort', 'Comparison parameter must be assigned.'));

  If iDirection <> 0 Then
    FDirection := iDirection;

  FComparison := aCompare;

  FSorted := False;
  InternalSort;
  FSorted := True;
End;


Function TAdvItemList.Find(pValue : Pointer; Out iIndex: Integer; aCompare : TAdvItemListCompare): Boolean;
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


Function TAdvItemList.CompareItem(pA, pB : Pointer): Integer;
Begin
  Result := IntegerCompare(Integer(pA), Integer(pB));
End;


Procedure TAdvItemList.DefaultCompare(Out aCompare : TAdvItemListCompare);
Begin
  aCompare := {$IFDEF FPC}@{$ENDIF}CompareItem;
End;


Procedure TAdvItemList.InternalInsert(iIndex : Integer);
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


Procedure TAdvItemList.InternalDelete(iIndex : Integer);
Begin
End;


Procedure TAdvItemList.DeleteByIndex(iIndex: Integer);
Begin
  Assert(Deleteable('DeleteByIndex', iIndex));

  InternalDelete(iIndex);

  Dec(FCount);

  If iIndex < FCount Then
    InternalCopy(iIndex + 1, iIndex, FCount - iIndex);
End;


Procedure TAdvItemList.DeleteRange(iFromIndex, iToIndex : Integer);
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


Procedure TAdvItemList.InternalExchange(iA, iB : Integer);
Var
  pTemp : Pointer;
Begin
  pTemp := ItemByIndex[iA];
  ItemByIndex[iA] := ItemByIndex[iB];
  ItemByIndex[iB] := pTemp;
End;


Procedure TAdvItemList.SetCapacity(Const iValue: Integer);
Begin
  Assert(CheckCondition((iValue >= FCount) And (iValue <= CapacityLimit), 'SetCapacity', StringFormat('Unable to change the capacity to %d', [iValue])));

  InternalResize(iValue);
  FCapacity := iValue;
End;


Procedure TAdvItemList.SetCount(Const iValue: Integer);
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


Procedure TAdvItemList.SortedBy(Const bValue : Boolean);
Begin

  If FSorted <> bValue Then
  Begin
    FSorted := bValue;

    If FSorted Then
      InternalSort;
  End;
End;


Procedure TAdvItemList.ComparedBy(Const Value: TAdvItemListCompare);
Begin
  Assert(Alterable('ComparedBy'));

  If Not IsComparedBy(Value) Then
  Begin
    FComparison := Value;
    FSorted := False;
  End;
End;


Procedure TAdvItemList.DirectionBy(Const Value: TAdvItemListDirection);
Begin
  Assert(Alterable('DirectionBy'));

  If FDirection <> Value Then
  Begin
    FDirection := Value;
    FSorted := False;
  End;
End;


Procedure TAdvItemList.DuplicateBy(Const Value: TAdvItemListDuplicates);
Begin
  Assert(Alterable('DuplicateBy'));

  FDuplicates := Value;
End;


Function TAdvItemList.CapacityLimit : Integer;
Begin
  Invariant('CapacityLimit', 'CapacityLimit not specified.');

  Result := 0;
End;


Function TAdvItemList.CountLimit : Integer;
Begin
  Result := CapacityLimit;
End;


Function TAdvItemList.IsComparedBy(Const aCompare: TAdvItemListCompare) : Boolean;
Begin
  Result := (TMethod(FComparison).Data = TMethod(aCompare).Data) And (TMethod(FComparison).Code = TMethod(aCompare).Code);
End;


Function TAdvItemList.IsCompared : Boolean;
Begin
  Result := Assigned(@FComparison);
End;


Function TAdvItemList.IsUncompared : Boolean;
Begin
  Result := Not IsCompared;
End;


Function TAdvItemList.IsSortedBy(Const aCompare: TAdvItemListCompare) : Boolean;
Begin
  Result := FSorted And IsComparedBy(aCompare);
End;


Procedure TAdvItemList.SortedBy(Const aCompare: TAdvItemListCompare);
Begin
  ComparedBy(aCompare);
  SortedBy(True);
End;


Procedure TAdvItemList.AllowDuplicates;
Begin
  DuplicateBy(dupAccept);
End;


Procedure TAdvItemList.IgnoreDuplicates;
Begin
  DuplicateBy(dupIgnore);

  // TODO: Delete duplicates?
End;


Procedure TAdvItemList.PreventDuplicates;
Begin
  DuplicateBy(dupException);

  // TODO: Assert that there are no duplicates?
End;


Function TAdvItemList.IsAllowDuplicates : Boolean;
Begin
  Result := FDuplicates = dupAccept;
End;


Function TAdvItemList.IsIgnoreDuplicates : Boolean;
Begin
  Result := FDuplicates = dupIgnore;
End;


Function TAdvItemList.IsPreventDuplicates : Boolean;
Begin
  Result := FDuplicates = dupException;
End;


Procedure TAdvItemList.Sorted;
Begin
  SortedBy(True);
End;


Procedure TAdvItemList.Unsorted;
Begin
  SortedBy(False);
End;


Procedure TAdvItemList.Uncompared;
Begin
  FComparison := Nil;
  FSorted := False;
End;


Procedure TAdvItemList.SortAscending;
Begin
  DirectionBy(1);
End;


Procedure TAdvItemList.SortDescending;
Begin
  DirectionBy(-1);
End;


Function TAdvItemList.IsSortAscending : Boolean;
Begin
  Result := FDirection > 0;
End;


Function TAdvItemList.IsSortDescending : Boolean;
Begin
  Result := FDirection < 0;
End;


Function TAdvItemList.IsSorted : Boolean;
Begin
  Result := FSorted;
End;


Function TAdvItemList.IsUnsorted : Boolean;
Begin
  Result := Not FSorted;
End;


Function TAdvItemList.IsEmpty : Boolean;
Begin
  Result := FCount <= 0;
End;


Function TAdvItemList.Deleteable(Const sMethod: String; iIndex: Integer): Boolean;
Begin
  Result := Alterable(sMethod);

  ValidateIndex(sMethod, iIndex);
End;


Function TAdvItemList.Deleteable(Const sMethod: String; iFromIndex, iToIndex: Integer): Boolean;
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


Function TAdvItemList.Insertable(Const sMethod: String; iIndex: Integer): Boolean;
Begin
  Result := Alterable(sMethod);

  If iIndex <> Count Then
    ValidateIndex(sMethod, iIndex);
End;


Function TAdvItemList.Replaceable(Const sMethod: String; iIndex: Integer): Boolean;
Begin
  Result := Alterable(sMethod);

  If Not Replacable Then
    Invariant(sMethod, 'List does not allow replacing of items.');
End;


Function TAdvItemList.Extendable(Const sMethod: String; iCount: Integer): Boolean;
Begin
  Result := Alterable(sMethod);

  If (iCount < 0) Or (iCount > CountLimit) Then
    Invariant(sMethod, StringFormat('Cannot set count to %d as it not compatible with the range [0..%d]', [iCount, CountLimit]));
End;


Procedure TAdvItemList.Exchange(iA, iB: Integer);
Begin
  Assert(ValidateIndex('Exchange', iA));
  Assert(ValidateIndex('Exchange', iB));
  Assert(CheckCondition(iA <> iB, 'Exchange', 'Cannot exchange with the same index position.'));
  Assert(CheckCondition(IsUnsorted, 'Exchange', 'Cannot exchange in sorted items.'));
  Assert(Alterable('Exchange'));

  InternalExchange(iA, iB);
End;


Function TAdvItemList.Replacable : Boolean;
Begin
  Result := True;
End;


Function TAdvItemList.IsOrderedBy(Const Value: TAdvItemListCompare): Boolean;
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


Procedure TAdvItemList.OrderedBy(Const Value: TAdvItemListCompare);
Begin
  FComparison := Value;
  FSorted := False;
  InternalSort;
End;


Procedure TAdvItemList.Unordered;
Begin
  FComparison := Nil;
  FSorted := False;
End;


Procedure TAdvPointerList.AssignItem(oItems : TAdvItemList; iIndex : Integer);
Begin
  FPointerArray^[iIndex] := TAdvPointerList(oItems).FPointerArray^[iIndex];
End;


Procedure TAdvPointerList.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FPointerArray, Capacity * SizeOf(TPointerItem), iValue * SizeOf(TPointerItem));
End;


Procedure TAdvPointerList.InternalEmpty(iIndex, iLength: Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FPointerArray) + (iIndex * SizeOf(TPointerItem))), (iLength * SizeOf(TPointerItem)));
End;


Procedure TAdvPointerList.InternalCopy(iSource, iTarget, iCount: Integer);
Begin
  Inherited;

  MemoryMove(@FPointerArray^[iSource], @FPointerArray^[iTarget], iCount * SizeOf(TPointerItem));
End;


Function TAdvPointerList.IndexByValue(pValue : TPointerItem): Integer;
Begin
  If Not Find(pValue, Result) Then
    Result := -1;
End;


Function TAdvPointerList.ExistsByValue(pValue : TPointerItem): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(pValue));
End;


Function TAdvPointerList.Add(pValue : TPointerItem): Integer;
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


Procedure TAdvPointerList.Insert(iIndex : Integer; pValue: TPointerItem);
Begin
  InternalInsert(iIndex);

  FPointerArray^[iIndex] := pValue;
End;


Procedure TAdvPointerList.InternalInsert(iIndex : Integer);
Begin
  Inherited;

  FPointerArray^[iIndex] := Nil;
End;


Procedure TAdvPointerList.DeleteByValue(pValue : TPointerItem);
Var
  iIndex : Integer;
Begin
  If Not Find(pValue, iIndex) Then
    RaiseError('DeleteByValue', 'Pointer not found in list');

  DeleteByIndex(iIndex);
End;


Function TAdvPointerList.RemoveFirst : TPointerItem;
Begin
  If Count <= 0 Then
    Result := Nil
  Else
  Begin
    Result := FPointerArray^[0];
    DeleteByIndex(0);
  End;
End;


Function TAdvPointerList.RemoveLast : TPointerItem;
Begin
  If Count <= 0 Then
    Result := Nil
  Else
  Begin
    Result := FPointerArray^[Count - 1];
    DeleteByIndex(Count - 1);
  End;
End;

Procedure TAdvPointerList.InternalExchange(iA, iB : Integer);
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


Function TAdvPointerList.GetItem(iIndex: Integer): Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := FPointerArray^[iIndex];
End;


Procedure TAdvPointerList.SetItem(iIndex : Integer; pValue: Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FPointerArray^[iIndex] := pValue;
End;


Function TAdvPointerList.GetPointerByIndex(iIndex : Integer): TPointerItem;
Begin
  Assert(ValidateIndex('GetPointerByIndex', iIndex));

  Result := FPointerArray^[iIndex];
End;


Procedure TAdvPointerList.SetPointerByIndex(iIndex : Integer; Const pValue : TPointerItem);
Begin
  Assert(ValidateIndex('SetPointerByIndex', iIndex));

  FPointerArray^[iIndex] := pValue;
End;


Function TAdvPointerList.CapacityLimit : Integer;
Begin
  Result := High(TPointerItems);
End;


Function TAdvPointerList.Iterator : TAdvIterator;
Begin
  Result := TAdvPointerListIterator.Create;
  TAdvPointerListIterator(Result).Pointers := TAdvPointerList(Self.Link);
End;


Constructor TAdvPointerListIterator.Create;
Begin
  Inherited;

  FPointerArray := Nil;
End;


Destructor TAdvPointerListIterator.Destroy;
Begin
  FPointerArray.Free;

  Inherited;
End;


Procedure TAdvPointerListIterator.First;
Begin
  Inherited;

  FIndex := 0;
End;


Procedure TAdvPointerListIterator.Last;
Begin
  Inherited;

  FIndex := FPointerArray.Count - 1;
End;


Procedure TAdvPointerListIterator.Next;
Begin
  Inherited;

  Inc(FIndex);
End;


Procedure TAdvPointerListIterator.Back;
Begin
  Inherited;

  Dec(FIndex);
End;


Function TAdvPointerListIterator.Current : Pointer;
Begin
  Result := FPointerArray[FIndex];
End;


Function TAdvPointerListIterator.More : Boolean;
Begin
  Result := FPointerArray.ExistsByIndex(FIndex);
End;


Procedure TAdvPointerListIterator.SetPointers(Const Value: TAdvPointerList);
Begin
  FPointerArray.Free;
  FPointerArray := Value;
End;



Procedure TAdvHashEntry.Assign(oSource: TAdvObject);
Begin
  Inherited;

  FCode := TAdvHashEntry(oSource).Code;
End;


Procedure TAdvHashEntry.Load(oFiler: TAdvFiler);
Begin
  Inherited;

  Generate;
End;


Function TAdvHashEntry.Link : TAdvHashEntry;
Begin
  Result := TAdvHashEntry(Inherited Link);
End;


Function TAdvHashEntry.Clone : TAdvHashEntry;
Begin
  Result := TAdvHashEntry(Inherited Clone);
End;


Procedure TAdvHashEntry.Generate;
Begin
End;


Constructor TAdvHashTable.Create;
Begin
  Inherited;

  Balance := 0.85;
End;


Destructor TAdvHashTable.Destroy;
Begin
  MemoryDestroy(FTable, FCapacity * SizeOf(TAdvHashEntry));

  Inherited;
End;


Function TAdvHashTable.ErrorClass : EAdvExceptionClass;
Begin
  Result := EAdvHashTable;
End;


Function TAdvHashTable.Link : TAdvHashTable;
Begin
  Result := TAdvHashTable(Inherited Link);
End;


Function TAdvHashTable.Clone : TAdvHashTable;
Begin
  Result := TAdvHashTable(Inherited Clone);
End;


Procedure TAdvHashTable.Assign(oObject : TAdvObject);
Var
  oIterator : TAdvHashTableIterator;
Begin
  Inherited;

  Clear;

  Capacity := TAdvHashTable(oObject).Capacity;
  Balance := TAdvHashTable(oObject).Balance;

  // TODO: Implement without iterator for optimal algorithm.

  oIterator := TAdvHashTableIterator(Iterator);
  Try
    oIterator.First;
    While oIterator.More Do
    Begin
      Add(Duplicate(TAdvHashEntry(oIterator.Current)));

      oIterator.Next;
    End;
  Finally
    oIterator.Free;
  End;
End;


Procedure TAdvHashTable.Define(oFiler: TAdvFiler);
Begin
  Inherited;

//oFiler['Balance'].DefineReal(FBalance);
End;


Procedure TAdvHashTable.Load(oFiler: TAdvFiler);
Var
  oHashEntry : TAdvHashEntry;
Begin
  Define(oFiler);

  oFiler['Items'].DefineBegin;

  While (oFiler.Peek <> atEnd) Do
  Begin
    oHashEntry := Nil;
    Try
      oFiler['Item'].DefineObject(oHashEntry);

      Add(oHashEntry.Link);
    Finally
      oHashEntry.Free;
    End;
  End;

  oFiler['Items'].DefineEnd;
End;


Procedure TAdvHashTable.Save(oFiler: TAdvFiler);
Var
  oHashEntry : TAdvHashEntry;
  iLoop     : Integer;
Begin
  Define(oFiler);

  oFiler['Items'].DefineBegin;

  For iLoop := 0 To FCapacity - 1 Do
  Begin
    oHashEntry := FTable^[iLoop];

    While Assigned(oHashEntry) Do
    Begin
      oFiler['Item'].DefineObject(oHashEntry);

      oHashEntry := oHashEntry.FNextHashEntry;
    End;
  End;

  oFiler['Items'].DefineEnd;
End;


Function TAdvHashTable.ItemClass : TAdvHashEntryClass;
Begin
  Result := TAdvHashEntry;
End;


Function TAdvHashTable.ItemNew : TAdvHashEntry;
Begin
  Result := ItemClass.Create;
End;


Procedure TAdvHashTable.InternalClear;
Var
  iLoop : Integer;
  oHashEntry : TAdvHashEntry;
  oNext : TAdvHashEntry;
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


Function TAdvHashTable.Find(oSource, oHashEntry : TAdvHashEntry): TAdvHashEntry;
Begin
  Assert(Not Assigned(oSource) Or Invariants('Find', oSource, ItemClass, 'oSource'));
  Assert(Invariants('Find', oHashEntry, ItemClass, 'oHashEntry'));

  Result := oSource;
  While Assigned(Result) And (Equal(Result, oHashEntry) <> 0) Do
    Result := Result.FNextHashEntry;
End;


Function TAdvHashTable.Equal(oA, oB: TAdvHashEntry): Integer;
Begin
  Result := IntegerCompare(oA.Code, oB.Code);
End;


Function TAdvHashTable.Resolve(oHashEntry: TAdvHashEntry): Cardinal;
Begin
  Assert(CheckCondition(FCapacity > 0, 'Resolve', 'Capacity must be greater than zero'));

  Result := UnsignedMod(oHashEntry.Code, FCapacity);
End;


Procedure TAdvHashTable.Rehash;
Var
  pTable : PAdvHashEntryArray;
  oHashEntry : TAdvHashEntry;
  oNext : TAdvHashEntry;
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
    MemoryDestroy(pTable, iCapacity * SizeOf(TAdvHashEntry));
  End;
End;


Procedure TAdvHashTable.Add(oHashEntry : TAdvHashEntry);
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


Procedure TAdvHashTable.Insert(iIndex : Integer; oHashEntry : TAdvHashEntry);
Var
  pFirst : PAdvHashEntry;
Begin
  Assert(Invariants('Insert', oHashEntry, TAdvHashEntry, 'oHashEntry'));
  Assert(CheckCondition((iIndex >= 0) And (iIndex < FCapacity), 'Insert', 'Index must be within the hash table'));

  pFirst := @FTable^[iIndex];

  oHashEntry.FNextHashEntry := pFirst^;
  pFirst^ := oHashEntry;

  Inc(FCount);
End;


Function TAdvHashTable.Force(oHashEntry: TAdvHashEntry): TAdvHashEntry;
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


Function TAdvHashTable.Replace(oHashEntry : TAdvHashEntry) : TAdvHashEntry;
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


Function TAdvHashTable.Delete(oHashEntry: TAdvHashEntry) : Boolean;
Var
  oLast  : TAdvHashEntry;
  oNext  : TAdvHashEntry;
  pFirst : PAdvHashEntry;
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


Function TAdvHashTable.Get(oHashEntry : TAdvHashEntry) : TAdvHashEntry;
Begin
  Assert(Invariants('Get', oHashEntry, ItemClass, 'oHashEntry'));

  // Returns the hash entry in the hash table matching the parameter.
  // If there is no matching object in the hash table, Nil is returned.

  Result := Find(FTable^[Resolve(oHashEntry)], oHashEntry);
End;


Function TAdvHashTable.Duplicate(oHashEntry: TAdvHashEntry): TAdvHashEntry;
Begin
  Assert(Invariants('Duplicate', oHashEntry, ItemClass, 'oHashEntry'));

  Result := TAdvHashEntry(oHashEntry.Clone);
End;


Function TAdvHashTable.Exists(oHashEntry: TAdvHashEntry): Boolean;
Begin
  Assert(Invariants('Exists', oHashEntry, ItemClass, 'oHashEntry'));

  Result := Assigned(Get(oHashEntry));
End;


Function TAdvHashTable.Iterator : TAdvIterator;
Begin
  Result := TAdvHashTableIterator.Create;
  TAdvHashTableIterator(Result).HashTable := Self.Link;
End;


Procedure TAdvHashTable.SetCapacity(Const Value: Integer);
Begin

  If Value <> FCapacity Then
  Begin
    Assert(CheckCondition(FCount = 0, 'SetCapacity', StringFormat('Unable to change capacity to %d when there are entries in the hash table', [Value])));

    MemoryResize(FTable, FCapacity * SizeOf(TAdvHashEntry), Value * SizeOf(TAdvHashEntry));

    If Value > FCapacity Then
      MemoryZero(Pointer(NativeUInt(FTable) + NativeUInt(FCapacity * SizeOf(TAdvHashEntry))), (Value - FCapacity) * SizeOf(TAdvHashEntry));

    FCapacity := Value;
    FThreshold := Trunc(FCapacity * FBalance);
  End;
End;


Procedure TAdvHashTable.SetBalance(Const Value: Real);
Begin
  Assert(CheckCondition((Value > 0.0) And (Value < 1.0), 'SetBalance', 'Balance must be set to valid positive percentage.'));

  FBalance := Value;
  FThreshold := Trunc(FCapacity * FBalance);
End;


Function TAdvHashTableList.GetHash(iIndex: Integer): TAdvHashTable;
Begin
  Result := TAdvHashTable(ObjectByIndex[iIndex]);
End;


Function TAdvHashTableList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvHashTable;
End;


Constructor TAdvHashTableIterator.Create;
Begin
  Inherited;

  FHashTable := Nil;
End;


Destructor TAdvHashTableIterator.Destroy;
Begin
  FHashTable.Free;

  Inherited;
End;


Procedure TAdvHashTableIterator.First;
Begin
  FIndex := 0;
  FCurrentHashEntry := Nil;
  FNextHashEntry := Nil;

  If FHashTable.Count > 0 Then
    Next;
End;


Function TAdvHashTableIterator.Count : Integer;
Begin
  Result := FHashTable.Count;
End;


Function TAdvHashTableIterator.Current : TAdvObject;
Begin
  Assert(Invariants('Current', FCurrentHashEntry, FHashTable.ItemClass, 'FCurrentHashEntry'));

  Result := FCurrentHashEntry;
End;


Function TAdvHashTableIterator.More : Boolean;
Begin
  Result := Assigned(FCurrentHashEntry);
End;


Procedure TAdvHashTableIterator.Next;
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


Function TAdvHashTableIterator.GetHashTable: TAdvHashTable;
Begin
  Result := FHashTable;
End;


Procedure TAdvHashTableIterator.SetHashTable(Const Value: TAdvHashTable);
Begin
  FHashTable.Free;
  FHashTable := Value;
End;


Function TAdvHashTable.ProduceHashEntry: TAdvHashEntry;
Begin
  Result := ItemNew;
End;


Procedure TAdvHashTable.ConsumeHashEntry(oHashEntry : TAdvHashEntry);
Begin
  oHashEntry.Free;
End;


Procedure TAdvHashTable.PredictCapacityByExpectedCount(Const iCount: Integer);
Begin
  Capacity := RealCeiling(iCount / Balance) + 1;

  Assert(CheckCondition(FThreshold >= iCount, 'PredictCapacityByExpectedCount', StringFormat('Threshold %d was expected to be the same as the expected count %d.', [FThreshold, iCount])));
End;


Procedure TAdvHashTable.AllowRehash;
Begin
  FPreventRehash := False;
End;


Procedure TAdvHashTable.PreventRehash;
Begin
  FPreventRehash := True;
End;


Function TAdvHashTable.IsEmpty: Boolean;
Begin
  Result := Count = 0;
End;


Function TAdvIterator.ErrorClass : EAdvExceptionClass;
Begin
  Result := EAdvIterator;
End;


Procedure TAdvIterator.First;
Begin
End;


Procedure TAdvIterator.Last;
Begin
End;


Function TAdvIterator.More : Boolean;
Begin
  Result := False;
End;


Procedure TAdvIterator.Next;
Begin
End;


Procedure TAdvIterator.Back;
Begin
End;


Procedure TAdvIterator.Previous;
Begin
  Back;
End;


Function TAdvObjectIterator.Current : TAdvObject;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := Nil;
End;


Function TAdvStringIterator.Current : String;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := '';
End;


Function TAdvIntegerIterator.Current : Integer;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TAdvRealIterator.Current : Real;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TAdvExtendedIterator.Current : Extended;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TAdvBooleanIterator.Current : Boolean;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := False;
End;


Function TAdvLargeIntegerIterator.Current : Int64;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TAdvPointerIterator.Current : Pointer;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := Nil;
End;


Function TAdvObjectClassIterator.Current : TClass;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := Nil;
End;


Function TAdvDateTimeIterator.Current : TDateTime;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TAdvDurationIterator.Current : TDuration;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;


Function TAdvCurrencyIterator.Current : TCurrency;
Begin
  RaiseError('Current', 'Current not implemented.');

  Result := 0;
End;



Function TAdvName.Link : TAdvName;
Begin
  Result := TAdvName(Inherited Link);
End;


Function TAdvName.Clone : TAdvName;
Begin
  Result := TAdvName(Inherited Clone);
End;


Procedure TAdvName.Assign(oSource : TAdvObject);
Begin
  Inherited;

  FName := TAdvName(oSource).Name;
End;


Procedure TAdvName.Define(oFiler : TAdvFiler);
Begin
  Inherited;

  oFiler['Name'].DefineString(FName);
End;


Function TAdvName.GetName: String;
Begin
  Result := FName;
End;


Procedure TAdvName.SetName(Const Value: String);
Begin
  FName := Value;
End;


Constructor TAdvNameList.Create;
Begin
  Inherited;

  FSymbol := cReturn;
End;


Function TAdvNameList.Link : TAdvNameList;
Begin
  Result := TAdvNameList(Inherited Link);
End;


Function TAdvNameList.Clone : TAdvNameList;
Begin
  Result := TAdvNameList(Inherited Clone);
End;


Function TAdvNameList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvName;
End;


Procedure TAdvNameList.DefaultCompare(Out aEvent: TAdvItemListCompare);
Begin
  aEvent := CompareByName;
End;


Function TAdvNameList.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TAdvName(pA).Name, TAdvName(pB).Name);
End;


Function TAdvNameList.FindByName(oName: TAdvName; Out iIndex: Integer): Boolean;
Begin
  Result := Find(oName, iIndex, CompareByName);
End;


Function TAdvNameList.FindByName(Const sName : String; Out iIndex : Integer) : Boolean;
Var
  oName : TAdvName;
Begin
  oName := TAdvName(ItemNew);
  Try
    oName.Name := sName;

    Result := FindByName(oName, iIndex);
  Finally
    oName.Free;
  End;
End;


Function TAdvNameList.EnsureByName(Const sName : String) : TAdvName;
Begin
  Result := GetByName(sName);

  Assert(Invariants('EnsureByName', Result, ItemClass, 'Result'));
End;


Function TAdvNameList.GetByName(Const sName: String): TAdvName;
Var
  iIndex : Integer;
Begin
  If FindByName(sName, iIndex) Then
    Result := Names[iIndex]
  Else
    Result := Nil;
End;


Function TAdvNameList.IndexByName(Const sName : String) : Integer;
Begin
  If Not FindByName(sName, Result) Then
    Result := -1;
End;


Function TAdvNameList.ExistsByName(Const sName: String): Boolean;
Begin
  Result := ExistsByIndex(IndexByName(sName));
End;


Function TAdvNameList.IndexByName(Const oName : TAdvName) : Integer;
Begin
  If Not FindByName(oName, Result) Then
    Result := -1;
End;


Function TAdvNameList.ExistsByName(Const oName : TAdvName): Boolean;
Begin
  Result := ExistsByIndex(IndexByName(oName));
End;


Function TAdvNameList.ForceByName(Const sName: String): TAdvName;
Var
  oName  : TAdvName;
  iIndex : Integer;
Begin
  oName := TAdvName(ItemNew);
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


Function TAdvNameList.GetByName(oName : TAdvName): TAdvName;
Var
  iIndex : Integer;
Begin
  If FindByName(oName, iIndex) Then
    Result := Names[iIndex]
  Else
    Result := Nil;
End;


Procedure TAdvNameList.RemoveByName(Const sName: String);
Var
  iIndex : Integer;
Begin
  If Not FindByName(sName, iIndex) Then
    RaiseError('RemoveByName', StringFormat('Object ''%s'' not found in list.', [sName]));

  DeleteByIndex(iIndex);
End;


Function TAdvNameList.AddByName(Const sName: String): Integer;
Var
  oItem : TAdvName;
Begin
  oItem := TAdvName(ItemNew);
  Try
    oItem.Name := sName;

    Result := Add(oItem.Link);
  Finally
    oItem.Free;
  End;
End;


Function TAdvNameList.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;


Procedure TAdvNameList.SortedByName;
Begin
  SortedBy(CompareByName);
End;


Function TAdvNameList.GetName(iIndex : Integer) : TAdvName;
Begin
  Result := TAdvName(ObjectByIndex[iIndex]);
End;


Procedure TAdvNameList.SetName(iIndex : Integer; oName : TAdvName);
Begin
  ObjectByIndex[iIndex] := oName;
End;


Function TAdvNameList.GetAsText : String;
Var
  oStrings : TAdvStringList;
  iLoop    : Integer;
Begin
  oStrings := TAdvStringList.Create;
  Try
    oStrings.Symbol := FSymbol;

    For iLoop := 0 To Count - 1 Do
      oStrings.Add(Names[iLoop].Name);

    Result := oStrings.AsText;
  Finally
    oStrings.Free;
  End;
End;


Procedure TAdvNameList.SetAsText(Const Value: String);
Var
  oStrings : TAdvStringList;
  iLoop    : Integer;
  oItem    : TAdvName;
Begin
  Clear;

  oStrings := TAdvStringList.Create;
  Try
    oStrings.Symbol := FSymbol;

    oStrings.AsText := Value;

    For iLoop := 0 To oStrings.Count - 1 Do
    Begin
      oItem := TAdvName(ItemNew);
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


End. // FHIR.Support.Collections //
