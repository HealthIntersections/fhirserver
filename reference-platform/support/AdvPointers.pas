Unit AdvPointers;

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
  MemorySupport, StringSupport,
  AdvItems, AdvIterators;


Type
  TPointerItem = Pointer;
  TPointerItems = Array[0..(MaxInt Div SizeOf(TPointerItem)) - 1] Of TPointerItem;
  PPointerItems = ^TPointerItems;

  TAdvPointerList = Class(TAdvItems)
    Private
      FPointerArray : PPointerItems;

      Function GetPointerByIndex(iIndex: Integer): TPointerItem;
      Procedure SetPointerByIndex(iIndex: Integer; Const pValue: TPointerItem);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; pValue: Pointer); Override;

      Procedure AssignItem(oItems : TAdvItems; iIndex : Integer); Override;

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


Implementation


Procedure TAdvPointerList.AssignItem(oItems : TAdvItems; iIndex : Integer);
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


End. // AdvPointers //

