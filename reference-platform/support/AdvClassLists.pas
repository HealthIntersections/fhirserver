Unit AdvClassLists;

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
