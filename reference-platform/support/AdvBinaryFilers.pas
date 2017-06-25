Unit AdvBinaryFilers;

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
  SysUtils,
  MathSupport, StringSupport,
  AdvPersistents, AdvFilers, AdvStreamFilers, AdvClassLists;


Type
  TAdvBinaryFiler = Class(TAdvStreamFiler)
    Private
//    FReducedClassNames : Boolean;
//    FReducedClassList : TAdvClassList;

    Protected
      Procedure DefineBlock(Var Value; Count : Integer); Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvBinaryFiler;

      Procedure Clear; Override;

      Procedure DefineInteger(Var Value : Integer); Override;
      Procedure DefineInteger(Var Value : Int64); Override;
      Procedure DefineInteger(Var Value : Cardinal); Override;
      Procedure DefineInteger(Var Value : Word); Override;
      Procedure DefineInteger(Var Value : Byte); Override;

      Procedure DefineReal(Var Value : Real); Override;
      Procedure DefineReal(Var Value : Extended); Override;

      Procedure DefineBoolean(Var Value : Boolean); Override;

      Procedure DefineString(Var Value : TShortString); Overload; Override;
      Procedure DefineString(Var Value : TLongString); Overload; Override;

      Procedure DefineBinary(Var Buffer; iCount : Integer); Override;

      Procedure DefineEnumerated(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = ''); Override;
      Procedure DefineSet(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = ''); Override;

      Procedure DefineDateTime(Var Value : TDateTime); Override;
      Procedure DefineDuration(Var Value : TDuration); Override;
      Procedure DefineCurrency(Var Value : TCurrency); Override;
      Procedure DefineColour(Var Value : TColour); Override;

//    Property ReducedClassNames : Boolean Read FReducedClassNames Write FReducedClassNames;
  End;

  TAdvBinaryWriter = Class(TAdvBinaryFiler)
    Private
      Procedure WriteString(Const sValue : String);
      Procedure WriteClass(Const sValue : String);

    Protected
      Procedure DefineBlock(Var Value; Count : Integer); Override;

    Public
      Procedure DefineValue(Value : TAdvTag); Override;

      Procedure DefineClass(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineReference(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineObject(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineResource(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineChar(Var Value : Char); Override;
      Procedure DefineString(Var Value : TLongString); Override;
  End;

  TAdvBinaryReader = Class(TAdvBinaryFiler)
    Private
      FCache : TAdvTag;

      Function ReadString: String;
      Function ReadClass: String;

    Protected
      Procedure DefineBlock(Var Value; Count : Integer); Override;

    Public
      Procedure Clear; Override;

      Function Peek : TAdvTag; Override;

      Procedure DefineValue(Value : TAdvTag); Override;

      Procedure DefineClass(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineReference(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineObject(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineResource(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineChar(Var Value : Char); Override;
      Procedure DefineString(Var Value : TLongString); Override;
  End;


Implementation



Constructor TAdvBinaryFiler.Create;
Begin
  Inherited;

//FReducedClassList := TAdvClassList.Create;
//FReducedClassList.Sorted;
End;


Destructor TAdvBinaryFiler.Destroy;
Begin
//FReducedClassList.Free;

  Inherited;
End;


Function TAdvBinaryFiler.Link : TAdvBinaryFiler;
Begin
  Result := TAdvBinaryFiler(Inherited Link);
End;


Procedure TAdvBinaryFiler.Clear;
Begin
  Inherited;

//FReducedClassList.Clear;
End;


Procedure TAdvBinaryFiler.DefineBlock(Var Value; Count: Integer);
Begin
End;


Procedure TAdvBinaryFiler.DefineBinary(Var Buffer; iCount : Integer);
Begin 
  Inherited;

  DefineBlock(iCount, SizeOf(iCount));

  DefineBlock(Buffer, iCount);
End;  


Procedure TAdvBinaryFiler.DefineBoolean(Var Value: Boolean);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Integer);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Int64);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Cardinal);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Word);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Byte);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End; 


Procedure TAdvBinaryFiler.DefineReal(Var Value: Real);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineReal(Var Value: Extended);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineString(Var Value: TShortString);
Begin 
  Inherited;

  DefineBlock(Value[0], 1);
  DefineBlock(Value[1], Ord(Value[0]));
End;  


Procedure TAdvBinaryFiler.DefineString(Var Value: TLongString);
Begin
  Inherited;
End;


Procedure TAdvBinaryFiler.DefineEnumerated(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = '');
Begin
  // TODO: remove (Peek = atEnumerated8) as it only required for legacy streams

  If (Length(aNames) <= 256) Or (Peek = atEnumerated8) Then
  Begin
    DefineValue(atEnumerated8);
    DefineBlock(Value, 1);

    Assert(CheckCondition((Byte(Value) <= High(aNames)), 'DefineEnumerated', 'Enumeration defined out of range.'));
  End
  Else
  Begin
    DefineValue(atEnumerated16);
    DefineBlock(Value, 2);

    Assert(CheckCondition((Word(Value) <= High(aNames)), 'DefineEnumerated', 'Enumeration defined out of range.'));
  End;
End;


Procedure TAdvBinaryFiler.DefineSet(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = '');
Begin 
  Inherited;

  DefineBlock(Value, RealCeiling(Length(aNames) / 8));
End;  


Procedure TAdvBinaryFiler.DefineDateTime(Var Value: TDateTime);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineCurrency(Var Value: TCurrency);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineDuration(Var Value: TDuration);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineColour(Var Value: TColour);
Begin
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;


Procedure TAdvBinaryWriter.DefineBlock(Var Value; Count: Integer);
Begin
  Stream.Write(Value, Count);
End;


Procedure TAdvBinaryWriter.DefineValue(Value: TAdvTag);
Begin
  DefineBlock(Value, SizeOf(Value));
End;


Procedure TAdvBinaryWriter.WriteString(const sValue: String);
Var
  iLength : Integer;
{$IFNDEF VER130}
  Bytes : TBytes;
{$ENDIF}
Begin
  iLength := Length(sValue);
  DefineBlock(iLength, SizeOf(iLength));

{$IFDEF VER130}
  DefineBlock(Pointer(sValue)^, iLength);
{$ELSE}
  Bytes := SysUtils.TEncoding.ASCII.GetBytes(sValue);
  DefineBlock(Bytes[0], Length(Bytes));
{$ENDIF}
end;


Procedure TAdvBinaryWriter.WriteClass(const sValue: String);
Var
  iLength : Byte;
{$IFNDEF VER130}
  Bytes : TBytes;
{$ENDIF}
Begin
  Assert(CheckCondition(Length(sValue) <= 256, 'WriteClass', 'Class must be less than 256 characters.'));

  iLength := Length(sValue);
  DefineBlock(iLength, SizeOf(iLength));

{$IFDEF VER130}
  DefineBlock(Pointer(sValue)^, iLength);
{$ELSE}
  Bytes := SysUtils.TEncoding.ASCII.GetBytes(sValue);
  DefineBlock(Bytes[0], Length(Bytes));
{$ENDIF}
end;


Procedure TAdvBinaryWriter.DefineString(Var Value: TLongString);
Begin
  Inherited;

  WriteString(Value);
End;


Procedure TAdvBinaryWriter.DefineChar(Var Value : Char);
{$IFNDEF VER130}
Var
  Bytes : TBytes;
{$ENDIF}
Begin
  Inherited;

{$IFDEF VER130}
  DefineBlock(Value, SizeOf(Value));
{$ELSE}
  Bytes := SysUtils.TEncoding.ASCII.GetBytes(Value);
  DefineBlock(Bytes[0], Length(Bytes));
{$ENDIF}
End;


Procedure TAdvBinaryWriter.DefineClass(Var Value; aClass : TAdvObjectClass);
Begin
  If Assigned(TClass(Value)) Then
  Begin
    Assert(Not Assigned(aClass) Or Invariants('DefineClass', TClass(Value), aClass, 'Value'));

    DefineValue(atClass);

    WriteClass(TClass(Value).ClassName);
  End
  Else
  Begin 
    DefineValue(atNil);
  End;  
End;  


Procedure TAdvBinaryWriter.DefineReference(Var Value; aClass : TAdvObjectClass);
Var
  oValue : TObject;
Begin 
  oValue := TObject(Value);

  If Assigned(oValue) Then
  Begin 
    Assert(Assigned(aClass) Or Invariants('DefineReference', oValue, TObject, 'oValue'));
    Assert(Not Assigned(aClass) Or Invariants('DefineReference', oValue, aClass, 'oValue'));

    DefineValue(atReference);
    DefineBlock(Value, SizeOf(TObject));
  End   
  Else
  Begin 
    DefineValue(atNil);
  End;  
End;  


Procedure TAdvBinaryWriter.DefineObject(Var Value; aClass : TAdvObjectClass);
Var
  oObject : TAdvPersistent;
//aClassType : TClass;
//iClassTypeIndex : Integer;
Begin
  oObject := TAdvPersistent(Value);

  If Not Assigned(oObject) Then
  Begin
    DefineValue(atNil);
  End
  Else If Referential And ReferenceManager.Exists(oObject) Then
  Begin
    DefineReference(oObject, aClass);
  End
  Else
  Begin
    Assert(Assigned(aClass) Or Invariants('DefineObject', oObject, TAdvPersistent, 'oObject'));
    Assert(Not Assigned(aClass) Or Invariants('DefineObject', oObject, aClass, 'oObject'));

    // Write the object tag.
    DefineValue(atObject);

    // Write the classname
(*
    aClassType := oObject.ClassType;

    If FReducedClassNames And FReducedClassList.Find(aClassType, iClassTypeIndex) Then
    Begin
      iLength := 0;
      DefineBlock(iLength, SizeOf(iLength));
      DefineBlock(aClassType, SizeOf(aClassType));
    End
    Else
    Begin
*)
      WriteClass(oObject.ClassName);

(*
      If FReducedClassNames Then
        FReducedClassList.Insert(iClassTypeIndex, aClassType);
    End;
*)
    // Write the object reference identifier.
    DefineBlock(oObject, SizeOf(oObject));

    // Remember that this instance has been filed once.
    If Referential Then
      ReferenceManager.Bind(oObject, Nil);

    // DefineBlock the persistent object
    DefineBegin;

    oObject.Save(Self);

    DefineEnd;
  End;
End;


Procedure TAdvBinaryWriter.DefineResource(Var Value; aClass: TAdvObjectClass);
Var
  oObject : TAdvObject;
  sResource : String;
Begin
  oObject := TAdvObject(Value);

  If Not Assigned(oObject) Then
    DefineValue(atNil)
  Else
  Begin
    Assert(Assigned(aClass) Or Invariants('DefineResource', oObject, TAdvObject, 'oObject'));
    Assert(Not Assigned(aClass) Or Invariants('DefineResource', oObject, aClass, 'oObject'));

    // Write the resource tag.
    DefineValue(atResource);

    sResource := ResourceManager.ResolveID(oObject);

    Assert(CheckCondition(sResource <> '', 'DefineResource', 'Resource string must be specified.'));

    WriteString(sResource);
  End;
End;  


Procedure TAdvBinaryReader.Clear;
Begin
  Inherited;

  FCache := atUnknown;
End;


Procedure TAdvBinaryReader.DefineBlock(Var Value; Count: Integer);
Begin
  Stream.Read(Value, Count);
End;


Function TAdvBinaryReader.Peek : TAdvTag;
Begin 
  If FCache = atUnknown Then
    DefineBlock(FCache, SizeOf(FCache)); // Read the tag off the stream

  Result := FCache;
End;  


Procedure TAdvBinaryReader.DefineValue(Value: TAdvTag);
Begin 
  If FCache = atUnknown Then
    DefineBlock(FCache, SizeOf(TAdvTag));

  // If read tag doesn't match the supplied type then an exception has occurred
  If FCache <> Value Then
    RaiseError('DefineValue', StringFormat('Expected ''%s'' but found ''%s''', [TagToString(Value), TagToString(FCache)]));

  // Cache will be remember for the next call if the above exception is raised.
  FCache := atUnknown;
End;  


Procedure TAdvBinaryReader.DefineClass(Var Value; aClass : TAdvObjectClass);
Var
  sClass : String;
Begin
  Case Peek Of
    atNil :
    Begin
      DefineValue(atNil);
      TClass(Value) := Nil;
    End;  

    atClass :
    Begin
      DefineValue(atClass);

      sClass := ReadClass;

      TClass(Value) := Factory.Get(sClass);

      Assert(CheckCondition(Assigned(TClass(Value)), 'DefineClass', 'Class not registered ' + sClass));

      Assert(Not Assigned(aClass) Or Invariants('DefineClass', TClass(Value), aClass, 'Value'));
    End;  
  Else
    RaiseError('DefineClass', StringFormat('Expected ''%s'' or ''%s'' but found ''%s''', [TagToString(atClass), TagToString(atNil), TagToString(Peek)]));
  End;  
End;  


Procedure TAdvBinaryReader.DefineReference(Var Value; aClass : TAdvObjectClass);
Var
  pObject : ^TObject;
  oReference : TAdvPersistent;
  oObject : TAdvPersistent;
Begin 
  pObject := @TObject(Value);

  Case Peek Of
    atNil :
    Begin
      DefineValue(atNil);

      pObject^.Free;
      pObject^ := Nil;
    End;  

    atReference :
    Begin
      DefineValue(atReference);
      DefineBlock(oReference, SizeOf(oReference));

      oObject := ReferenceManager.Get(oReference);
      If Not Assigned(oObject) Then
        RaiseError('DefineReference', 'Reference does not have an associated object.');

      pObject^.Free;
      pObject^ := oObject;
    End;   
  Else
    RaiseError('DefineReference', StringFormat('Expected ''%s'' or ''%s'' but found ''%s''', [TagToString(atReference), TagToString(atNil), TagToString(Peek)]));
  End;  
End;  


Procedure TAdvBinaryReader.DefineObject(Var Value; aClass : TAdvObjectClass);
Var
  sClass  : String;
  pObject : ^TAdvPersistent;
  oReference : TAdvPersistent;
{$IFOPT C+}
  aRequiredClass : TAdvObjectClass;
{$ENDIF}
Begin
{$IFOPT C+}
  If aClass = Nil Then
    aRequiredClass := TAdvPersistent
  Else
    aRequiredClass := aClass;
{$ENDIF}

  // Pointer to the variable parameter, this is done so we can change its value.
  pObject := @TAdvPersistent(Value);

  // Read the type off the stream
  Case Peek Of
    atNil :
    Begin
      DefineValue(atNil);

      pObject^.Free;
      pObject^ := Nil;
    End;

    atObject :
    Begin
      DefineValue(atObject);

      // Read the class name.
      sClass := ReadClass;

      If Not Assigned(pObject^) Then
      Begin
        pObject^ := TAdvPersistent(Factory.Make(sClass));
      End
      Else
      Begin
      {$IFOPT C+}
        Assert(Invariants('DefineObject', pObject^, aRequiredClass, 'pObject^'));
      {$ENDIF}

        If Not Factory.IsEquivalentClass(pObject^.ClassType, sClass) Then
          RaiseError('DefineObject', StringFormat('Expected object ''%s'' but found object ''%s''', [pObject^.ClassName, sClass]));
      End;

      // Read the objects old reference and match to the new reference.
      DefineBlock(oReference, SizeOf(oReference));

      If Referential Then
      Begin
        ReferenceManager.Bind(oReference, pObject^);
      {$IFOPT C+}
        Assert(Invariants('DefineObject', pObject^, aRequiredClass, 'pObject^'));
      {$ENDIF}
      End;

      // Load the object's properties
      DefineBegin;

      pObject^.Load(Self);

      DefineEnd;
    End;

    atReference :
    Begin
      If Not Referential Then
        RaiseError('DefineObject', 'Can only load from references if the filer is in referential mode.');

      DefineValue(atReference);
      DefineBlock(oReference, SizeOf(oReference));

      pObject^.Free;
      pObject^ := TAdvPersistent(TAdvObject(ReferenceManager.Get(oReference)).Link);
    End; 
  Else
    RaiseError('DefineObject', StringFormat('Expected ''%s'', ''%s'' or ''%s'' but found ''%s''', [TagToString(atObject), TagToString(atReference), TagToString(atNil), TagToString(Peek)]));
  End; 
End;


Procedure TAdvBinaryReader.DefineResource(Var Value; aClass: TAdvObjectClass);
Var
  sResource : String;
  oObject : TAdvObject;
  pObject : ^TAdvObject;
Begin
  // Pointer to the variable parameter, this is done so we can change its value.
  pObject := @TAdvObject(Value);

  Case Peek Of
    atNil :
    Begin
      DefineValue(atNil);

      pObject^.Free;
      pObject^ := Nil;
    End;

    atResource :
    Begin
      DefineValue(atResource);

      sResource := ReadString;

      oObject := ResourceManager.ResolveObject(sResource, aClass);

    {$IFOPT C+}
      If Assigned(oObject) Then
      Begin
        If Assigned(aClass) Then
          Invariants('DefineResource', oObject, aClass, 'oObject')
        Else
          Invariants('DefineResource', oObject, TAdvObject, 'oObject');
      End;
    {$ENDIF}

      pObject^.Free;
      pObject^ := oObject.Link;
    End;
  Else
    RaiseError('DefineResource', StringFormat('Expected ''%s'' or ''%s'' but found ''%s''', [TagToString(atResource), TagToString(atNil), TagToString(Peek)]));
  End;
End;


Function TAdvBinaryReader.ReadClass : String;
Var
  iLength : Byte;
{$IFNDEF VER130}
  aBuffer : TBytes;
{$ENDIF}
Begin
  DefineBlock(iLength, SizeOf(iLength));

{$IFDEF VER130}
  SetLength(Result, iLength);
  DefineBlock(Pointer(Result)^, iLength);
{$ELSE}
  SetLength(aBuffer, iLength);
  DefineBlock(aBuffer[0], iLength);

  Result := SysUtils.TEncoding.ASCII.GetString(aBuffer);
{$ENDIF}
End;


Function TAdvBinaryReader.ReadString : String;
Var
  iLength : Integer;
{$IFNDEF VER130}
  aBuffer : TBytes;
{$ENDIF}
Begin
  DefineBlock(iLength, SizeOf(iLength));

{$IFDEF VER130}
  SetLength(Result, iLength);
  DefineBlock(Pointer(Result)^, iLength);
{$ELSE}
  SetLength(aBuffer, iLength);
  DefineBlock(aBuffer[0], iLength);

  Result := SysUtils.TEncoding.ASCII.GetString(aBuffer);
{$ENDIF}
End;


Procedure TAdvBinaryReader.DefineString(Var Value: TLongString);
Begin
  Inherited;

  Value := ReadString;
End;


Procedure TAdvBinaryReader.DefineChar(Var Value : Char);
{$IFNDEF VER130}
Var
  aBuffer : TBytes;
{$ENDIF}
Begin
  Inherited;

{$IFDEF VER130}
  DefineBlock(Value, SizeOf(Value));
{$ELSE}
  SetLength(aBuffer, 1);
  DefineBlock(aBuffer[0], Length(aBuffer));
  Value := SysUtils.TEncoding.ASCII.GetString(aBuffer)[1];
{$ENDIF}
End;


End. // AdvBinaryFilers //
