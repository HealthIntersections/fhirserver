Unit AdvInt64Matches;

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
  MemorySupport, StringSupport, MathSupport,
  AdvObjects, AdvItems, AdvFilers;


Type
  TAdvInt64MatchKey = Int64;
  TAdvInt64MatchValue = Int64;

  TAdvInt64MatchItem = Record
    Key   : TAdvInt64MatchKey;
    Value : TAdvInt64MatchValue;
  End; 

  PAdvIntegerMatchItem = ^TAdvInt64MatchItem;

  TAdvInt64MatchItems = Array[0..(MaxInt Div SizeOf(TAdvInt64MatchItem)) - 1] Of TAdvInt64MatchItem;
  PAdvIntegerMatchItems = ^TAdvInt64MatchItems;

  TAdvInt64Match = Class(TAdvItems)
    Private
      FMatches : PAdvIntegerMatchItems;
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
      Procedure AssignItem(oItems : TAdvItems; iIndex : Integer); Override;

      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CompareByKey(pA, pB : Pointer): Integer; 
      Function CompareByValue(pA, pB : Pointer): Integer; 
      Function CompareByKeyValue(pA, pB : Pointer) : Integer; 

      Procedure DefaultCompare(Out aCompare : TAdvItemsCompare); Overload; Override;

      Function CapacityLimit : Integer; Override;

      Function Find(Const aKey : TAdvInt64MatchKey; Const aValue: TAdvInt64MatchValue; Out iIndex : Integer; aCompare : TAdvItemsCompare = Nil) : Boolean;
      Function FindByKey(Const aKey : TAdvInt64MatchKey; Out iIndex : Integer; aCompare : TAdvItemsCompare = Nil) : Boolean;

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


Implementation


Function TAdvInt64Match.Link : TAdvInt64Match;
Begin
  Result := TAdvInt64Match(Inherited Link);
End;


Function TAdvInt64Match.CompareByKey(pA, pB: Pointer): Integer;
Begin 
  Result := IntegerCompare(PAdvIntegerMatchItem(pA)^.Key, PAdvIntegerMatchItem(pB)^.Key);
End;


Function TAdvInt64Match.CompareByValue(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(PAdvIntegerMatchItem(pA)^.Value, PAdvIntegerMatchItem(pB)^.Value);
End;  


Function TAdvInt64Match.CompareByKeyValue(pA, pB: Pointer): Integer;
Begin
  Result := CompareByKey(pA, pB);

  If Result = 0 Then
    Result := CompareByValue(pA, pB);
End;


Procedure TAdvInt64Match.DefaultCompare(Out aCompare: TAdvItemsCompare);
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


Procedure TAdvInt64Match.AssignItem(oItems: TAdvItems; iIndex: Integer);
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

  aTemp := PAdvIntegerMatchItem(pA)^;
  PAdvIntegerMatchItem(pA)^ := PAdvIntegerMatchItem(pB)^;
  PAdvIntegerMatchItem(pB)^ := aTemp;
End;  


Function TAdvInt64Match.GetItem(iIndex: Integer): Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMatches^[iIndex];
End;  


Procedure TAdvInt64Match.SetItem(iIndex: Integer; pValue: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMatches^[iIndex] := PAdvIntegerMatchItem(pValue)^;
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


Function TAdvInt64Match.Find(Const aKey: TAdvInt64MatchKey; Const aValue: TAdvInt64MatchValue; Out iIndex: Integer; aCompare: TAdvItemsCompare): Boolean;
Var
  aItem : TAdvInt64MatchItem;
Begin
  aItem.Key := aKey;
  aItem.Value := aValue;

  Result := Inherited Find(@aItem, iIndex, aCompare);
End;


Function TAdvInt64Match.FindByKey(Const aKey: TAdvInt64MatchKey; Out iIndex: Integer; aCompare: TAdvItemsCompare): Boolean;
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


End. // AdvIntegerMatches //
