Unit FHIR.Support.Filers;

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
  FHIR.Support.Strings, FHIR.Support.DateTime, FHIR.Support.System,
  FHIR.Support.Objects, FHIR.Support.Exceptions;

type
  TFslTag =
    (atUnknown, atBoolean, atInteger32, atLongString, atReal, atBinary, atEnumerated8,
     atObject, atClass, atBegin, atEnd, atNil, atInteger64, atInteger16, atInteger8,
     atShortString, atSet, atCharacter, atReference, atResource, atDateTime, atColour, atExtended,
     atDuration, atCurrency, atEnumerated16);

  TFslFiler = Class(TFslObject)
    Private
      FField : String;

    Protected
      Procedure RaiseError(Const sMethod, sMessage : String); Override;

      Function GetField : String; Virtual;
      Function SetField(Const sField : String) : TFslFiler; Virtual;
      Function UseField : String; Virtual;

    Public
      Procedure DefineValue(Value : TFslTag); Virtual;
      Procedure DefineBegin; Virtual;
      Procedure DefineEnd; Virtual;

      Procedure DefineInteger(Var Value : Integer); Overload; Virtual;
      Procedure DefineInteger(Var Value : Int64); Overload; Virtual;
      Procedure DefineInteger(Var Value : Cardinal); Overload; Virtual;
      Procedure DefineInteger(Var Value : Word); Overload; Virtual;
      Procedure DefineInteger(Var Value : Byte); Overload; Virtual;

      Procedure DefineReal(Var Value : Real); Overload; Virtual;
      Procedure DefineReal(Var Value : Extended); Overload; Virtual;

      Procedure DefineBoolean(Var Value : Boolean); Virtual;

      Procedure DefineChar(Var Value : Char); Overload; Virtual;
      {$IFNDEF FPC}
      {$IFNDEF VER130}
      Procedure DefineString(Var Value : TLongString); Overload; Virtual;
      Procedure DefineString(Var Value : AnsiChar); Overload; Virtual;
      Procedure DefineString(Var Value : TShortString); Overload; Virtual;
      {$ENDIF}
      {$ENDIF}
      Procedure DefineString(Var Value : AnsiString); Overload; Virtual;

      Procedure DefineBinary(Var Value; iCount : Integer); Virtual;

      Procedure DefineEnumerated(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = ''); Virtual;
      Procedure DefineSet(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = ''); Virtual;

      Procedure DefineDateTime(Var Value : TDateTime); Virtual;
      Procedure DefineDuration(Var Value : TDuration); Virtual;
      Procedure DefineColour(Var Value : TColour); Virtual;
      Procedure DefineCurrency(Var Value : TCurrency); Virtual;

      Procedure DefineClass(Var Value; aClass : TFslObjectClass = Nil); Virtual;
      Procedure DefineObject(Var Value; aClass : TFslObjectClass = Nil); Virtual;
      Procedure DefineReference(Var Value; aClass : TFslObjectClass = Nil); Virtual;
      Procedure DefineResource(Var Value; aClass : TFslObjectClass = Nil); Virtual;

      Function Peek : TFslTag; Overload; Virtual;
      Function PeekField : String; Overload; Virtual;

      Property Fields[Const sField : String] : TFslFiler Read SetField; Default;
      Property Field : String Read GetField;
  End;

  TFslFilerClass = Class Of TFslFiler;

  EAdvFiler = Class(EAdvException);

  TFslObjectClass = FHIR.Support.Objects.TFslObjectClass;

  TLongString = FHIR.Support.Strings.TLongString;
  TShortString = FHIR.Support.Strings.TShortString;
  TDuration = FHIR.Support.DateTime.TDuration;


Function StringToTag(Const sValue : String) : TFslTag;
Function TagToString(atType : TFslTag) : String;

type
  {$TYPEINFO ON}
  TFslPersistent = Class(TFslObject)
    Protected
      Function Fileable(Const sLocation : String) : Boolean; Overload; Virtual;

    Public
      Function Link : TFslPersistent; Overload;
      Function Clone : TFslPersistent; Overload;

      Procedure Define(oFiler : TFslFiler); Overload; Virtual;
      Procedure Load(oFiler : TFslFiler); Overload; Virtual;
      Procedure Save(oFiler : TFslFiler); Overload; Virtual;

      Function Fileable : Boolean; Overload; Virtual;
  End;

  TFslPersistentClass = Class Of TFslPersistent;

  EAdvExceptionClass = FHIR.Support.Objects.EAdvExceptionClass;
  EAdvException = FHIR.Support.Objects.EAdvException;
  TFslObject = FHIR.Support.Objects.TFslObject;

Implementation

Const
  TAG_STRING : Array[TFslTag] Of String =
    ('Unknown', 'Boolean', 'Integer', 'String', 'Real', 'Binary', 'Enumerated',
     'Object', 'Class', 'Begin', 'End', 'Nil', 'Integer64', 'Integer16', 'Integer8',
     'ShortString', 'Set', 'Character', 'Reference', 'Resource', 'DateTime', 'Colour',
     'Extended', 'Duration', 'Currency', 'Enumerated16');


Function StringToTag(Const sValue : String) : TFslTag;
Begin
  // Returns atUnknown if it isn't a valid string.

  Result := High(TFslTag);
  While (Result > Low(TFslTag)) And Not StringEquals(TAG_STRING[Result], sValue) Do
    Dec(Result);
End;


Function TagToString(atType : TFslTag) : String;
Begin
  If (Integer(atType) > Integer(High(TFslTag))) Or (Integer(atType) < 0) Then
    Result := StringFormat('Invalid (%d)', [Integer(atType)])
  Else
    Result := TAG_STRING[atType];
End;


Procedure TFslFiler.RaiseError(Const sMethod, sMessage: String);
Begin
  RaiseError(EAdvFiler, sMethod, sMessage);
End;


Procedure TFslFiler.DefineValue(Value : TFslTag);
Begin
End;


Procedure TFslFiler.DefineBegin;
Begin
  DefineValue(atBegin);
End;


Procedure TFslFiler.DefineEnd;
Begin
  DefineValue(atEnd);
End;


Procedure TFslFiler.DefineBinary(Var Value; iCount : Integer);
Begin
  DefineValue(atBinary);
End;


Procedure TFslFiler.DefineBoolean(Var Value : Boolean);
Begin
  DefineValue(atBoolean);
End;


Procedure TFslFiler.DefineInteger(Var Value : Integer);
Begin
  DefineValue(atInteger32);
End;


Procedure TFslFiler.DefineInteger(Var Value : Int64);
Begin
  DefineValue(atInteger64);
End;


Procedure TFslFiler.DefineInteger(Var Value : Cardinal);
Begin
  DefineValue(atInteger32);
End;


Procedure TFslFiler.DefineInteger(Var Value : Word);
Begin
  DefineValue(atInteger16);
End;


Procedure TFslFiler.DefineInteger(Var Value : Byte);
Begin
  DefineValue(atInteger8);
End;


Procedure TFslFiler.DefineReal(Var Value : Real);
Begin
  DefineValue(atReal);
End;


Procedure TFslFiler.DefineReal(Var Value : Extended);
Begin
  DefineValue(atExtended);
End;

Procedure TFslFiler.DefineString(Var Value : AnsiString);
Begin
  DefineValue(atLongString);
End;

Procedure TFslFiler.DefineChar(Var Value : Char);
Begin
  DefineValue(atCharacter);
End;

{$IFNDEF FPC}
{$IFNDEF VER130}
Procedure TFslFiler.DefineString(Var Value : TLongString);
Begin
  DefineValue(atLongString);
End;



procedure TFslFiler.DefineString(var Value: AnsiChar);
begin
  DefineValue(atCharacter);
end;



Procedure TFslFiler.DefineString(Var Value : TShortString);
Begin
  DefineValue(atShortString);
End;
{$ENDIF}
{$ENDIF}

Procedure TFslFiler.DefineEnumerated(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = '');
Begin
  DefineValue(atEnumerated8);
End;


Procedure TFslFiler.DefineSet(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = '');
Begin
  DefineValue(atSet);
End;


Procedure TFslFiler.DefineDateTime(Var Value : TDateTime);
Begin
  DefineValue(atDateTime);
End;


Procedure TFslFiler.DefineDuration(Var Value : TDuration);
Begin
  DefineValue(atDuration);
End;


Procedure TFslFiler.DefineColour(Var Value : TColour);
Begin
  DefineValue(atColour);
End;


Procedure TFslFiler.DefineCurrency(Var Value : TCurrency);
Begin
  DefineValue(atCurrency);
End;


Procedure TFslFiler.DefineClass(Var Value; aClass : TFslObjectClass);
Begin
  DefineValue(atClass);
End;


Procedure TFslFiler.DefineObject(Var Value; aClass : TFslObjectClass);
Begin
  DefineValue(atObject);
End;


Procedure TFslFiler.DefineReference(Var Value; aClass : TFslObjectClass);
Begin
  DefineValue(atReference);
End;


Procedure TFslFiler.DefineResource(Var Value; aClass: TFslObjectClass);
Begin
  DefineValue(atResource);
End;


Function TFslFiler.GetField : String;
Begin
  Result := FField;
End;


Function TFslFiler.SetField(Const sField: String): TFslFiler;
Begin
  FField := sField;
  Result := Self;
End;


Function TFslFiler.UseField : String;
Begin
  Result := FField;
  FField := '';
End;


Function TFslFiler.PeekField : String;
Begin
  Peek;
  Result := Field;
End;


Function TFslFiler.Peek : TFslTag;
Begin
  Result := atUnknown;
End;


Function TFslPersistent.Clone : TFslPersistent;
Begin
  Result := TFslPersistent(Inherited Clone);
End;


Function TFslPersistent.Link : TFslPersistent;
Begin
  Result := TFslPersistent(Inherited Link);
End;


Procedure TFslPersistent.Define(oFiler: TFslFiler);
Begin
  Assert(Fileable('Define'));
End;


Procedure TFslPersistent.Save(oFiler : TFslFiler);
Begin
  Assert(Invariants('Save', oFiler, TFslFiler, 'oFiler'));

  Define(oFiler);
End;


Procedure TFslPersistent.Load(oFiler : TFslFiler);
Begin
  Assert(Invariants('Load', oFiler, TFslFiler, 'oFiler'));

  Define(oFiler);
End;


Function TFslPersistent.Fileable : Boolean;
Begin
  Result := True;
End;


Function TFslPersistent.Fileable(Const sLocation: String): Boolean;
Begin
  Invariants(sLocation, TFslObject);

  If Not Fileable Then
    Invariant(sLocation, 'Object is marked as unfileable.');

  Result := True;
End;



End. // FHIR.Support.Filers //
