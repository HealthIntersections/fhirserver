Unit AdvIntegerObjectMatches;

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
  MemorySupport, MathSupport, StringSupport,
  AdvObjects, AdvItems, AdvFilers;


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

  TAdvIntegerObjectMatch = Class(TAdvItems)
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
      Procedure AssignItem(oItems : TAdvItems; iIndex : Integer); Override;

      Procedure InternalTruncate(iValue : Integer); Override;
      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount : Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalDelete(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CompareByKey(pA, pB : Pointer): Integer; Virtual;
      Function CompareByValue(pA, pB : Pointer): Integer; Virtual;

      Procedure DefaultCompare(Out aCompare : TAdvItemsCompare); Overload; Override;

      Function Find(Const aKey : TAdvIntegerObjectMatchKey; Const aValue : TAdvIntegerObjectMatchValue; Out iIndex : Integer; aCompare : TAdvItemsCompare = Nil) : Boolean; Overload;

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


Implementation



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


Procedure TAdvIntegerObjectMatch.AssignItem(oItems : TAdvItems; iIndex: Integer);
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


Procedure TAdvIntegerObjectMatch.DefaultCompare(Out aCompare: TAdvItemsCompare);
Begin 
  aCompare := CompareByKey;
End;  


Function TAdvIntegerObjectMatch.Find(Const aKey: TAdvIntegerObjectMatchKey; Const aValue: TAdvIntegerObjectMatchValue; Out iIndex: Integer; aCompare: TAdvItemsCompare): Boolean;
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

End. // AdvIntegerObjectMatches //
