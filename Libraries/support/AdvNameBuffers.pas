Unit AdvNameBuffers;


{! 13 !}


Interface


Uses
  StringSupport,
  AdvObjects,
  AdvFactories, AdvBuffers;


Type
  TAdvNameBuffer = Class(TAdvBuffer)
    Private
      FName : String;

    Public
      Function Link : TAdvNameBuffer;
      Function Clone : TAdvNameBuffer;

      Procedure Assign(oObject : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

      Property Name : String Read FName Write FName;
  End;

  TAdvNameBufferList = Class(TAdvBufferList)
    Private
      Function GetBuffer(iIndex : Integer) : TAdvNameBuffer;

    Protected
      Function ItemClass : TAdvObjectClass; Override;

      Procedure DefaultCompare(Out aEvent : TAdvItemsCompare); Override;

      Function CompareByName(pA, pB : Pointer) : Integer; Virtual;

    Public
      Function Link : TAdvNameBufferList;
      Function Clone : TAdvNameBufferList; 

      Function GetByName(Const sName : String) : TAdvNameBuffer;
      Function IndexByName(Const sName : String) : Integer;
      Function ExistsByName(Const sName : String) : Boolean;
      Procedure Merge(oBuffers : TAdvNameBufferList);

      Property Buffer[iIndex : Integer] : TAdvNameBuffer Read GetBuffer; Default;
  End;


Implementation


Procedure TAdvNameBuffer.Assign(oObject : TAdvObject);
Begin
  Inherited;

  FName := TAdvNameBuffer(oObject).FName;
End;


Procedure TAdvNameBuffer.Define(oFiler : TAdvFiler);
Begin
  Inherited;

  oFiler['Name'].DefineString(FName);
End;


Function TAdvNameBuffer.Link : TAdvNameBuffer;
Begin
  Result := TAdvNameBuffer(Inherited Link);
End;


Function TAdvNameBuffer.Clone : TAdvNameBuffer;
Begin
  Result := TAdvNameBuffer(Inherited Clone);
End;


Function TAdvNameBufferList.Clone : TAdvNameBufferList;
Begin
  Result := TAdvNameBufferList(Inherited Clone);
End;


Function TAdvNameBufferList.Link : TAdvNameBufferList;
Begin
  Result := TAdvNameBufferList(Inherited Link);
End;


Function TAdvNameBufferList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvNameBuffer;
End;


Procedure TAdvNameBufferList.Merge(oBuffers : TAdvNameBufferList);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oBuffers.Count - 1 Do
    Add(oBuffers[iLoop].Clone);
End;


Function TAdvNameBufferList.GetBuffer(iIndex : Integer) : TAdvNameBuffer;
Begin
  Result := TAdvNameBuffer(ObjectByIndex[iIndex]);
End;


Function TAdvNameBufferList.GetByName(Const sName : String) : TAdvNameBuffer;
Begin
  Result := TAdvNameBuffer(Get(IndexByName(sName)));
End;


Function TAdvNameBufferList.ExistsByName(Const sName : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(sName));
End;


Function TAdvNameBufferList.CompareByName(pA, pB: Pointer): Integer;
Begin
  Result := StringCompare(TAdvNameBuffer(pA).Name, TAdvNameBuffer(pB).Name);
End;


Function TAdvNameBufferList.IndexByName(Const sName: String): Integer;
Var
  oBuffer : TAdvNameBuffer;
Begin
  oBuffer := TAdvNameBuffer(ItemNew);
  Try
    oBuffer.Name := sName;

    If Not Find(oBuffer, Result, CompareByName) Then
      Result := -1;
  Finally
    oBuffer.Free;
  End;
End;


Procedure TAdvNameBufferList.DefaultCompare(Out aEvent: TAdvItemsCompare);
Begin
  aEvent := CompareByName;
End;


Initialization
  Factory.RegisterClassArray([TAdvNameBuffer, TAdvNameBufferList]);
End. // AdvNameBuffers //
