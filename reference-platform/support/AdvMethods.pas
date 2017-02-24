Unit AdvMethods;

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

  MemoryZero(Pointer(NativeUInt(FMethodArray) + (iIndex * SizeOf(TAdvMethodItem))), (iLength * SizeOf(TAdvMethodItem)));
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
      RaiseError('Add', 'Item already exists in list');

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
    RaiseError('DeleteByValue', 'Method not found in list');

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
