Unit AdvMethods;


{! 14 !}


Interface


Uses
  MathSupport, MemorySupport,
  AdvObjects, AdvItems;


Type
  TAdvMethod = Procedure Of Object;
  PAdvMethod = ^TAdvMethod;

  TAdvMethodItem = Record
    Code, Data : Pointer;
  End;

  PAdvMethodItem = ^TAdvMethodItem;

  TAdvMethodItemArray = Array[0..(MaxInt Div SizeOf(TAdvMethodItem)) - 1] Of TAdvMethodItem;
  PAdvMethodItemArray = ^TAdvMethodItemArray;

  TAdvMethodList = Class(TAdvItems)
    Private
      FMethodArray : PAdvMethodItemArray;

      Function GetMethodItem(iIndex : Integer): TAdvMethod;
      Procedure SetMethodItem(iIndex : Integer; Const aValue : TAdvMethod);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; Value: Pointer); Override;

      Procedure AssignItem(oItems : TAdvItems; iIndex : Integer); Override;

      Function CompareItem(pA, pB: Pointer): Integer; Overload; Override;

      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount: Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CapacityLimit : Integer; Override;

      Function Find(aValue : TAdvMethod; Out iResult : Integer) : Boolean; 

    Public
      Function IndexByValue(aValue : TAdvMethod) : Integer; 
      Function ExistsByValue(aValue: TAdvMethod): Boolean;

      Function Add(aValue : TAdvMethod) : Integer;
      Procedure Insert(iIndex : Integer; aValue : TAdvMethod);
      Procedure DeleteByValue(aValue : TAdvMethod);

      Property MethodByIndex[iIndex : Integer] : TAdvMethod Read GetMethodItem Write SetMethodItem; Default;
  End;


Implementation


Procedure TAdvMethodList.AssignItem(oItems : TAdvItems; iIndex : Integer);
Begin 
  FMethodArray^[iIndex] := TAdvMethodList(oItems).FMethodArray^[iIndex];
End;  


Function TAdvMethodList.CompareItem(pA, pB : Pointer) : Integer;
Begin 
  Result := IntegerCompare(Integer(PAdvMethodItem(pA)^.Code), Integer(PAdvMethodItem(pB)^.Code));

  If Result = 0 Then
    Result := IntegerCompare(Integer(PAdvMethodItem(pA)^.Data), Integer(PAdvMethodItem(pB)^.Data));
End;  


Procedure TAdvMethodList.InternalResize(iValue : Integer);
Begin 
  Inherited;

  MemoryResize(FMethodArray, Capacity * SizeOf(TAdvMethodItem), iValue * SizeOf(TAdvMethodItem));
End;  


Procedure TAdvMethodList.InternalEmpty(iIndex, iLength: Integer);
Begin 
  Inherited;

  MemoryZero(Pointer(Integer(FMethodArray) + (iIndex * SizeOf(TAdvMethodItem))), (iLength * SizeOf(TAdvMethodItem)));
End;  


Procedure TAdvMethodList.InternalCopy(iSource, iTarget, iCount: Integer);
Begin 
  Inherited;

  System.Move(FMethodArray^[iSource], FMethodArray^[iTarget], iCount * SizeOf(TAdvMethodItem));
End;  


Function TAdvMethodList.IndexByValue(aValue : TAdvMethod): Integer;
Begin
  If Not Find(aValue, Result) Then
    Result := -1;
End;


Function TAdvMethodList.ExistsByValue(aValue: TAdvMethod): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(aValue));
End;  


Function TAdvMethodList.Add(aValue : TAdvMethod): Integer;
Begin 
  Result := -1;

  If Not IsAllowDuplicates And Find(aValue, Result) Then
  Begin 
    If IsPreventDuplicates Then
      Error('Add', 'Item already exists in list');

    // Result is the index of the existing key
  End   
  Else
  Begin 
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(aValue, Result);

    Insert(Result, aValue);
  End;  
End;  


Procedure TAdvMethodList.Insert(iIndex : Integer; aValue : TAdvMethod);
Begin 
  InternalInsert(iIndex);

  FMethodArray^[iIndex] := TAdvMethodItem(aValue);
End;  


Procedure TAdvMethodList.InternalInsert(iIndex : Integer);
Begin 
  Inherited;

  FMethodArray^[iIndex].Code := Nil;
  FMethodArray^[iIndex].Data := Nil;  
End;  


Procedure TAdvMethodList.DeleteByValue(aValue : TAdvMethod);
Var
  iIndex : Integer;
Begin
  If Not Find(aValue, iIndex) Then
    Error('DeleteByValue', 'Method not found in list');

  DeleteByIndex(iIndex);
End;  


Procedure TAdvMethodList.InternalExchange(iA, iB: Integer);
Var
  aTemp : TAdvMethodItem;
Begin 
  aTemp := FMethodArray^[iA];
  FMethodArray^[iA] := FMethodArray^[iB];
  FMethodArray^[iB] := aTemp;
End;  


Function TAdvMethodList.GetItem(iIndex : Integer): Pointer;
Begin 
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMethodArray^[iIndex];
End;  


Procedure TAdvMethodList.SetItem(iIndex : Integer; Value: Pointer);
Begin 
  Assert(ValidateIndex('SetItem', iIndex));

  FMethodArray^[iIndex] := PAdvMethodItem(Value)^;
End;  


Function TAdvMethodList.GetMethodItem(iIndex : Integer): TAdvMethod;
Begin 
  Assert(ValidateIndex('GetMethodItem', iIndex));

  Result := TAdvMethod(FMethodArray^[iIndex]);
End;  


Procedure TAdvMethodList.SetMethodItem(iIndex : Integer; Const aValue : TAdvMethod);
Begin 
  Assert(ValidateIndex('SetMethodItem', iIndex));

  FMethodArray^[iIndex] := TAdvMethodItem(aValue);
End;  


Function TAdvMethodList.CapacityLimit : Integer;
Begin 
  Result := High(TAdvMethodItemArray);
End;  


Function TAdvMethodList.Find(aValue: TAdvMethod; Out iResult: Integer): Boolean;
Begin 
  Result := Inherited Find(@TAdvMethodItem(aValue), iResult);
End;


End. // AdvMethods //
