Unit AdvIntegerLists;

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
  AdvObjects, AdvItems, AdvFilers, AdvIterators;


Type
  TAdvIntegerListItem = Integer;
  PAdvIntegerListItem = ^TAdvIntegerListItem;
  TAdvIntegerListItemArray = Array[0..(MaxInt Div SizeOf(TAdvIntegerListItem)) - 1] Of TAdvIntegerListItem;
  PAdvIntegerListItemArray = ^TAdvIntegerListItemArray;

  TAdvIntegerList = Class(TAdvItems)
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
      Procedure AssignItem(oItems : TAdvItems; iIndex : Integer); Override;

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


Implementation


Uses
  MemorySupport, StringSupport;


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


Procedure TAdvIntegerList.AssignItem(oItems : TAdvItems; iIndex : Integer);
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

End. // AdvIntegerLists //
