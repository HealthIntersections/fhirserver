Unit AdvEvents;


{! 8 !}


Interface


Uses
  AdvObjects, AdvItems, AdvMethods;


Type
  TAdvEvent = Procedure (Sender : TAdvObject) Of Object;

  TAdvEventList = Class(TAdvMethodList)
    Private
      Function GetEvent(iIndex : Integer): TAdvEvent;
      Procedure SetEvent(iIndex : Integer; Const aValue : TAdvEvent);

    Public
      Function IndexByValue(aValue : TAdvEvent) : Integer;
      Function ExistsByValue(aValue : TAdvEvent) : Boolean;
      Function Add(aValue : TAdvEvent) : Integer;
      Procedure Insert(iIndex : Integer; aValue : TAdvEvent);
      Procedure DeleteByValue(aValue : TAdvEvent);

      Property EventByIndex[iIndex : Integer] : TAdvEvent Read GetEvent Write SetEvent; Default;
  End;


Implementation


Function TAdvEventList.IndexByValue(aValue : TAdvEvent): Integer;
Begin
  Result := Inherited IndexByValue(TAdvMethod(aValue));
End;


Function TAdvEventList.ExistsByValue(aValue: TAdvEvent): Boolean;
Begin
  Result := Inherited ExistsByValue(TAdvMethod(aValue));
End;


Function TAdvEventList.Add(aValue : TAdvEvent): Integer;
Begin
  Result := Inherited Add(TAdvMethod(aValue));
End;


Procedure TAdvEventList.Insert(iIndex : Integer; aValue : TAdvEvent);
Begin
  Inherited Insert(iIndex, TAdvMethod(aValue));
End;


Procedure TAdvEventList.DeleteByValue(aValue : TAdvEvent);
Begin
  Inherited DeleteByValue(TAdvMethod(aValue));
End;


Function TAdvEventList.GetEvent(iIndex : Integer): TAdvEvent;
Begin
  Assert(ValidateIndex('GetEvent', iIndex));

  Result := TAdvEvent(MethodByIndex[iIndex]);
End;


Procedure TAdvEventList.SetEvent(iIndex : Integer; Const aValue : TAdvEvent);
Begin
  Assert(ValidateIndex('SetEvent', iIndex));

  MethodByIndex[iIndex] := TAdvMethod(aValue);
End;


End. // AdvEvents //
