Unit AdvZipParts;

{! 1 !}

Interface

Uses
  AdvNameBuffers,
  AdvFactories, AdvBuffers;


Type
  TAdvZipPart = Class(TAdvNameBuffer)
    Private
      FTimestamp: TDateTime;
      FComment : String;

    Public
      Function Link : TAdvZipPart;
      Function Clone : TAdvZipPart;

      Procedure Assign(oObject : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

      Property Timestamp : TDateTime Read FTimestamp Write FTimestamp;
      Property Comment : String Read FComment Write FComment;
  End;

  TAdvZipPartList = Class(TAdvNameBufferList)
    Private
      Function GetPart(iIndex : Integer) : TAdvZipPart;

    Protected
      Function ItemClass : TAdvObjectClass; Override;

    Public
      Function Link : TAdvZipPartList;
      Function Clone : TAdvZipPartList;

      Function GetByName(Const sName : String) : TAdvZipPart;

      Property Part[iIndex : Integer] : TAdvZipPart Read GetPart; Default;
  End;


Implementation


Procedure TAdvZipPart.Assign(oObject : TAdvObject);
Begin
  Inherited;
  FTimestamp := TAdvZipPart(oObject).FTimestamp;
  FComment := TAdvZipPart(oObject).FComment;
End;


Procedure TAdvZipPart.Define(oFiler : TAdvFiler);
Begin
  Inherited;
  oFiler['Timestamp'].DefineDateTime(FTimestamp);
  oFiler['Comment'].DefineString(FComment);
End;


Function TAdvZipPart.Link : TAdvZipPart;
Begin
  Result := TAdvZipPart(Inherited Link);
End;


Function TAdvZipPart.Clone : TAdvZipPart;
Begin
  Result := TAdvZipPart(Inherited Clone);
End;


Function TAdvZipPartList.Clone : TAdvZipPartList;
Begin
  Result := TAdvZipPartList(Inherited Clone);
End;


Function TAdvZipPartList.Link : TAdvZipPartList;
Begin
  Result := TAdvZipPartList(Inherited Link);
End;


Function TAdvZipPartList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvZipPart;
End;



Function TAdvZipPartList.GetPart(iIndex : Integer) : TAdvZipPart;
Begin
  Result := TAdvZipPart(ObjectByIndex[iIndex]);
End;


Function TAdvZipPartList.GetByName(Const sName: String): TAdvZipPart;
Begin
  Result := TAdvZipPart(Inherited GetByName(sName));
End;

Initialization
  Factory.RegisterClassArray([TAdvZipPart, TAdvZipPartList]);
End. // AdvZipParts //

