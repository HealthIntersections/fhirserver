Unit fsl_ole;


{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  ActiveX, Windows,
  ComObj;


Type
  OleVariant = System.OleVariant;
  EOleException = ComObj.EOleException;

  IRecordInfo = Interface(IUnknown)
  ['{0000002F-0000-0000-C000-000000000046}']
    Function RecordInit(
        (* [out] *) Out pvNew : Pointer) : HRESULT; Stdcall;

    Function RecordClear(
        (* [in] *) pvExisting : Pointer) : HRESULT; Stdcall;

    Function RecordCopy(
        (* [in] *) pvExisting : Pointer;
        (* [out] *) Out pvNew : Pointer) : HRESULT; Stdcall;

    Function GetGuid(
        (* [out] *) Out pguid : TGUID) : HRESULT; Stdcall;

    Function GetName(
        (* [out] *) Out pbstrName : WideString) : HRESULT; Stdcall;

    Function GetSize(
        (* [out] *) Out pcbSize : Cardinal) : HRESULT; Stdcall;

    Function GetTypeInfo(
        (* [out] *) Out ppTypeInfo : ITypeInfo) : HRESULT; Stdcall;

    Function GetField(
        (* [in] *) pvData : Pointer;
        (* [in] *) Const szFieldName : WideString;
        (* [out] *) Out pvarField : OleVariant) : HRESULT; Stdcall;

    Function GetFieldNoCopy(
        (* [in] *) pvData : Pointer;
        (* [in] *) Const szFieldName : WideString;
        (* [out] *) Out pvarField : OleVariant;
        (* [out] *) Out ppvDataCArray : Pointer) : HRESULT; Stdcall;

    Function PutField(
        (* [in] *) wFlags : Cardinal;
        (* [out][in] *) Var pvData : Pointer;
        (* [in] *) szFieldName : WideString;
        (* [in] *) pvarField : OleVariant) : HRESULT; Stdcall;

    Function PutFieldNoCopy(
        (* [in] *) wFlags : Cardinal;
        (* [out][in] *) Var pvData : Pointer;
        (* [in] *) szFieldName : WideString;
        (* [in] *) pvarField : OleVariant) : HRESULT; Stdcall;

    Function GetFieldNames(
        (* [out][in] *) Var pcNames : Cardinal;
        (* [length_is][size_is][out] *) Out rgBstrNames : PWideString) : HRESULT; Stdcall;

    Function IsMatchingType(
        (* [in] *) pRecordInfo : IRecordInfo) : Boolean; Stdcall;

    Function RecordCreate : Pointer; Stdcall;

    Function RecordCreateCopy(
        (* [in] *) pvSource : Pointer;
        (* [out] *) Out ppvDest : Pointer) : HRESULT; Stdcall;

    Function RecordDestroy(
        (* [in] *) pvRecord : Pointer) : HRESULT; Stdcall;
  End;


Const
  VT_RECORD = 36;


Function VariantUnassigned : Variant;
Function VariantEmptyParameter : OleVariant;
Function VariantNull : Variant;


Function VariantArrayCreate(Const aBounds : Array Of Integer; iVarType : Integer) : Variant;
Function VariantArrayLowBound(Const aVariant : Variant; iDim : Integer): Integer;
Function VariantArrayHighBound(Const aVariant : Variant; iDim : Integer): Integer;
Function VariantArrayOf(Const Values : Array Of Variant) : Variant;

Function VariantIsArray(Const aVariant : Variant): Boolean;
Function VariantType(Const aVariant : Variant) : Integer;
Function VariantIsNull(Const aVariant : Variant) : Boolean;
Function VariantIsEmpty(Const aVariant : Variant) : Boolean;
Function VariantToString(Const aVariant : Variant) : String;

Function COMClassCreate(Const ClassID: TGUID) : IUnknown;
Function COMClassExists(Const ClassID: TGUID) : Boolean;
Function OLEClassCreate(Const ClassName: String) : IDispatch;
Function OLEClassExists(Const ClassName: String) : Boolean;

Function SafeArrayCreateVectorEx(vt: TVarType; Lbound, cElements : LongInt; pvExtra : Pointer) : PSafeArray; Stdcall;
Function GetRecordInfoFromGuids(Const rGuidTypeLib : TGuid; uVerMajor, uVerMinor : LongInt; lcid : Cardinal; Const rGuidTypeInfo : TGUID; Out ppRecInfo : IRecordInfo) : HResult; Stdcall;

Function InterfaceSupported(Const oInstance : IUnknown; Const aInterfaceGUID: TGUID) : Boolean;
Function VarTypeAsString(Const aVariant : Variant) : String;


Implementation


Function VariantUnassigned : Variant;
Begin
{$IFDEF VER130}
  Result := System.Unassigned;
{$ELSE}
  Result := Variants.Unassigned;
{$ENDIF}
End;


Function VariantEmptyParameter : OleVariant;
Begin
{$IFDEF VER130}
  Result := System.EmptyParam;
{$ELSE}
  Result := Variants.EmptyParam;
{$ENDIF}
End;


Function VariantNull : Variant;
Begin
{$IFDEF VER130}
  Result := System.Null;
{$ELSE}
  Result := Variants.Null;
{$ENDIF}
End;


Function VariantArrayCreate(Const aBounds : Array Of Integer; iVarType : Integer) : Variant;
Begin
{$IFDEF VER130}
  Result := System.VarArrayCreate(aBounds, iVarType);
{$ELSE}
  Result := Variants.VarArrayCreate(aBounds, iVarType);
{$ENDIF}
End;


Function VariantIsArray(const aVariant : Variant): Boolean;
Begin
{$IFDEF VER130}
  Result := System.VarIsArray(aVariant);
{$ELSE}
  Result := Variants.VarIsArray(aVariant);
{$ENDIF}
End;


Function VariantArrayLowBound(Const aVariant : Variant; iDim : Integer): Integer;
Begin
{$IFDEF VER130}
  Result := System.VarArrayLowBound(aVariant, iDim);
{$ELSE}
  Result := Variants.VarArrayLowBound(aVariant, iDim);
{$ENDIF}
End;


Function VariantArrayHighBound(Const aVariant : Variant; iDim : Integer): Integer;
Begin
{$IFDEF VER130}
  Result := System.VarArrayHighBound(aVariant, iDim);
{$ELSE}
  Result := Variants.VarArrayHighBound(aVariant, iDim);
{$ENDIF}
End;


Function VariantArrayOf(Const Values : Array Of Variant): Variant;
Begin
{$IFDEF VER130}
  Result := System.VarArrayOf(Values);
{$ELSE}
  Result := Variants.VarArrayOf(Values);
{$ENDIF}
End;


Function VariantType(Const aVariant : Variant) : Integer;
Begin
{$IFDEF VER130}
  Result := System.VarType(aVariant);
{$ELSE}
  Result := Variants.VarType(aVariant);
{$ENDIF}
End;


Function VariantIsNull(Const aVariant : Variant) : Boolean; Overload;
Begin
{$IFDEF VER130}
  Result := System.VarIsNull(aVariant);
{$ELSE}
  Result := Variants.VarIsNull(aVariant);
{$ENDIF}
End;


Function VariantIsEmpty(Const aVariant : Variant) : Boolean; Overload;
Begin
{$IFDEF VER130}
  Result := System.VarIsEmpty(aVariant);
{$ELSE}
  Result := Variants.VarIsEmpty(aVariant);
{$ENDIF}
End;


Function VariantToString(Const aVariant : Variant) : String;
Begin
{$IFDEF VER130}
  Result := System.VarToStr(aVariant);
{$ELSE}
  Result := Variants.VarToStr(aVariant);
{$ENDIF}
End;


Function OLEClassCreate(Const ClassName: String) : IDispatch;
Var
  ClassID : TGUID;
Begin
  If Not Succeeded(CLSIDFromProgID(PWideChar(WideString(ClassName)), ClassID)) Or
     Not Succeeded(CoCreateInstance(ClassID, Nil, CLSCTX_INPROC_SERVER Or CLSCTX_LOCAL_SERVER, IDispatch, Result)) Then
    Result := Nil;
End;


Function OLEClassExists(Const ClassName: String) : Boolean;
Var
  ClassID : TGUID;
Begin
  Result := Succeeded(CLSIDFromProgID(PWideChar(WideString(ClassName)), ClassID));
End;


Function COMClassCreate(Const ClassID: TGUID) : IUnknown;
Begin
  If Not Succeeded(CoCreateInstance(ClassID, Nil, CLSCTX_INPROC_SERVER Or CLSCTX_LOCAL_SERVER, IUnknown, Result)) Then
    Result := Nil;
End;


Function COMClassExists(Const ClassID: TGUID) : Boolean;
Var
  pProgID:  PWideChar;
Begin
  Result := Succeeded(ProgIDFromCLSID(ClassID, pProgID));

  If Result Then
    CoTaskMemFree(pProgID);
End;


Function InterfaceSupported(Const oInstance : IUnknown; Const aInterfaceGUID : TGUID) : Boolean;
Var
  oInterface : IUnknown;
Begin
  Result := oInstance.QueryInterface(aInterfaceGUID, oInterface) = S_OK;
End;


Function SafeArrayCreateVectorEx; External 'oleaut32.dll' name 'SafeArrayCreateVectorEx';
Function GetRecordInfoFromGuids; External 'oleaut32.dll' name 'GetRecordInfoFromGuids';


Function VarTypeAsString(Const aVariant : Variant) : String;
Begin
  case VarType(aVariant) and varTypeMask of
    vtInteger: Result := 'integer';
    vtBoolean: Result := 'Boolean';
    vtChar: Result := 'Char';
    vtExtended: Result := 'Extended';
    vtString: Result := 'String';
    vtPointer: Result := 'Pointer';
    vtPChar: Result := 'PChar';
    vtObject: Result := 'Object';
    vtClass: Result := 'Class';
    vtWideChar: Result := 'WideChar';
    vtPWideChar : Result := 'PWideChar';
    vtAnsiString: Result := 'AnsiString';
    vtCurrency  : Result := 'Currency';
    vtVariant   : Result := 'Variant';
    vtInterface : Result := 'Interface';
    vtWideString: Result := 'WideString';
    vtInt64     : Result := 'Int64';
  Else
    result := 'Unknown';
  end;

  if VarType(aVariant) and varArray <> 0 then
    Result := 'array of ' + Result;
  if VarType(aVariant) and varByRef <> 0 then
    Result := Result + ' by ref';
End;


End.

