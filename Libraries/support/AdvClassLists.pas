Unit AdvClassLists;


{! 7 !}


Interface


Uses
  StringSupport,
  AdvObjects, AdvPointers, AdvIterators;


Type
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


Implementation


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


End. // AdvClassLists //
