{******************************************************************************
                Copyright (c) PilotLogic Software House
                     
 Package pl_Win_DirectX
 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)      
                                                                               
   ****** BEGIN LICENSE BLOCK *****     
                                                              
   The contents of this file are used with permission, subject to the Mozilla   
   Public License Version 2.0 (the "License"); you may not use this file except 
   in compliance with the License. You may obtain a copy of the License at      
   https://www.mozilla.org/en-US/MPL/2.0/                                     
                                                                               
   Software distributed under the License is distributed on an "AS IS" basis,   
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for 
   the specific language governing rights and limitations under the License. 
   
   ****** END LICENSE BLOCK *****  
  
**********************************************************************************}

{$WEAKPACKAGEUNIT}
{$ALIGN ON}
{$MINENUMSIZE 4}

unit DXTypes;

{$I DirectX.inc}

interface

{$DEFINE TYPE_IDENTITY}

uses Windows;

const
  _DXSDK_PRODUCT_MAJOR  = 9;
  _DXSDK_PRODUCT_MINOR  = 15;
  _DXSDK_BUILD_MAJOR    = 779;
  _DXSDK_BUILD_MINOR    = 0000;

type

  UInt64 = Int64; // for a while

// ========= OLD CODE before CodeTyphon 4.7
  {.$IFDEF WIN64}
 { INT_PTR = PtrInt;//Int64;
  UINT_PTR = PtrUInt;//UInt64;
  LONG_PTR = PtrInt;//Int64;
  ULONG_PTR = PtrUInt;//UInt64;
  DWORD_PTR = PtrUInt;//UInt64; }
  {.$ELSE}
 { INT_PTR = Longint;
  UINT_PTR = LongWord;
  LONG_PTR = Longint;
  ULONG_PTR = LongWord;
  DWORD_PTR = LongWord; }
  {.$ENDIF}
// =========   

  INT_PTR = PtrInt;
  UINT_PTR = PtrUInt;
  LONG_PTR = PtrInt;
  ULONG_PTR = PtrUInt;
  DWORD_PTR = PtrUInt;

  PINT_PTR = ^INT_PTR;
  PUINT_PTR = ^UINT_PTR;
  PLONG_PTR = ^LONG_PTR;
  PULONG_PTR = ^ULONG_PTR;

  PPtrInt = ^PtrInt;
  PPtrUInt = ^PtrUInt;

  //
  // SIZE_T used for counts or ranges which need to span the range of
  // of a pointer.  SSIZE_T is the signed variation.
  //

  SIZE_T = ULONG_PTR;
  SSIZE_T = LONG_PTR;
  PSIZE_T = ^SIZE_T;
  PSSIZE_T = ^SSIZE_T;

  SizeInt = SSIZE_T;
  SizeUInt = SIZE_T;
  PSizeInt = PSSIZE_T;
  PSizeUInt = PSIZE_T;

  xxUINT_PTR=PAnsiChar; //UINT_PTR


  // TD3DValue is the fundamental Direct3D fractional data type
  D3DVALUE = Single;
  TD3DValue = D3DVALUE;
  PD3DValue = ^TD3DValue;
  D3DCOLOR = type DWord;
  TD3DColor = D3DCOLOR;
  PD3DColor = ^TD3DColor;


  _D3DVECTOR = record
    x: Single;
    y: Single;
    z: Single;
  end ;
  D3DVECTOR = _D3DVECTOR;
  TD3DVector = _D3DVECTOR;
  PD3DVector = ^TD3DVector;

function TD3DVector_Create(const X, Y, Z: Single): TD3DVector;
function TD3DVector_Equal(const Left, Right: TD3DVector): Boolean;
function TD3DVector_Zero: TD3DVector;

//-------------------------------------------------
type

  REFERENCE_TIME = LONGLONG;
  TReferenceTime = REFERENCE_TIME;
  PReferenceTime = ^TReferenceTime;

  PD3DColorValue = ^TD3DColorValue;
  _D3DCOLORVALUE = record
    r: Single;
    g: Single;
    b: Single;
    a: Single;
  end ;
  D3DCOLORVALUE = _D3DCOLORVALUE;
  TD3DColorValue = _D3DCOLORVALUE;

function TD3DColorValue_Create(const R, G, B, A: Single): TD3DColorValue;
function TD3DColorValue_Implicit(const Value: DWORD): TD3DColorValue;
function TD3DColorValue_Implicit(const Value: TD3DColorValue): DWORD;
function TD3DColorValue_Equal(const Left, Right: TD3DColorValue): Boolean;

//-------------------------------------------------
type

  PD3DRect = ^TD3DRect;
  _D3DRECT = record
    x1: LongInt;
    y1: LongInt;
    x2: LongInt;
    y2: LongInt;
  end ;
  D3DRECT = _D3DRECT;
  TD3DRect = _D3DRECT;

  PD3DMatrix = ^TD3DMatrix;
  _D3DMATRIX = record
    case integer of
      0 : (_11, _12, _13, _14: Single;
           _21, _22, _23, _24: Single;
           _31, _32, _33, _34: Single;
           _41, _42, _43, _44: Single);
      1 : (m : array [0..3, 0..3] of Single);
  end ;
  D3DMATRIX = _D3DMATRIX;
  TD3DMatrix = _D3DMATRIX;

  PPD3DXMatrix = ^PD3DXMatrix;
  PD3DXMatrix = ^TD3DXMatrix;
  TD3DXMatrix = TD3DMatrix;

function TD3DMatrix_Create(_m00, _m01, _m02, _m03,
                           _m10, _m11, _m12, _m13,
                           _m20, _m21, _m22, _m23,
                           _m30, _m31, _m32, _m33: Single): TD3DMatrix;
function TD3DMatrix_Add(const M1, M2: TD3DMatrix): TD3DMatrix;
function TD3DMatrix_Subtract(const M1, M2: TD3DMatrix): TD3DMatrix;
function TD3DMatrix_Multiply(const M: TD3DMatrix; const S: Single): TD3DMatrix;
function TD3DMatrix_Equal(const M1, M2: TD3DMatrix): Boolean;


implementation

uses SysUtils;

//======================== _D3DVECTOR =========================================

function TD3DVector_Create(const X, Y, Z: Single): TD3DVector;
begin
  Result.x := X;
  Result.y := Y;
  Result.z := Z;
end;

function TD3DVector_Equal(const Left, Right: TD3DVector): Boolean;
begin
  Result:= (Left.x = Right.x) and (Left.y = Right.y) and (Left.z = Right.z);
end;

function TD3DVector_Zero: TD3DVector;
begin
  Result.x := 0.0;
  Result.y := 0.0;
  Result.z := 0.0;
end;

//======================== _D3DMATRIX ==========================================

function TD3DMatrix_Add(const M1, M2: TD3DMatrix): TD3DMatrix;
var
  pOut, p1, p2: PSingle;
  I: Integer;
begin
  pOut:= @Result._11; p1:= @M1._11; p2:= @M2._11;
  for I := 0 to 15 do
  begin
    pOut^:= p1^ + p2^;
    Inc(pOut);
    Inc(p1);
    Inc(p2);
  end;
end;

function TD3DMatrix_Create(_m00, _m01, _m02, _m03,
                           _m10, _m11, _m12, _m13,
                           _m20, _m21, _m22, _m23,
                           _m30, _m31, _m32, _m33: Single): TD3DMatrix;
begin
  with Result do
  begin
    m[0,0]:= _m00; m[0,1]:= _m01; m[0,2]:= _m02; m[0,3]:= _m03;
    m[1,0]:= _m10; m[1,1]:= _m11; m[1,2]:= _m12; m[1,3]:= _m13;
    m[2,0]:= _m20; m[2,1]:= _m21; m[2,2]:= _m22; m[2,3]:= _m23;
    m[3,0]:= _m30; m[3,1]:= _m31; m[3,2]:= _m32; m[3,3]:= _m33;
  end;
end;

function TD3DMatrix_Equal(const M1, M2: TD3DMatrix): Boolean;
begin
  Result:= CompareMem(@m1, @m2, SizeOf(_D3DMATRIX));
end;

function TD3DMatrix_Multiply(const M: TD3DMatrix; const S: Single): TD3DMatrix;
var
  pOut, p: PSingle;
  I: Integer;
begin
  pOut:= @Result._11; p:= @M._11;
  for I := 0 to 15 do
  begin
    pOut^:= p^ * S;
    Inc(pOut);
    Inc(p);
  end;
end;

function TD3DMatrix_Subtract(const M1, M2: TD3DMatrix): TD3DMatrix;
var
  pOut, p1, p2: PSingle;
  I: Integer;
begin
  pOut:= @Result._11; p1:= @M1._11; p2:= @M2._11;
  for I := 0 to 15 do
  begin
    pOut^:= p1^ - p2^;
    Inc(pOut);
    Inc(p1);
    Inc(p2);
  end;
end;

//==================== _D3DCOLORVALUE =================================

function TD3DColorValue_Create(const R, G, B, A: Single): TD3DColorValue;
begin
  Result.r := R;
  Result.g := G;
  Result.b := B;
  Result.a := A;
end;

function TD3DColorValue_Equal(const Left, Right: TD3DColorValue): Boolean;
begin
  Result:= (Left.r = Right.r) and (Left.g = Right.g) and (Left.b = Right.b) and (Left.a = Right.a);
end;

function TD3DColorValue_Implicit(const Value: DWORD): TD3DColorValue;
const
  f = 1/255;
begin
  with Result do
  begin
    r:= f * Byte(Value shr 16);
    g:= f * Byte(Value shr  8);
    b:= f * Byte(Value{shr 0});
    a:= f * Byte(Value shr 24);
  end;
end;

function TD3DColorValue_Implicit(const Value: TD3DColorValue): DWORD;
var
  dwR, dwG, dwB, dwA: DWORD;
begin
  if Value.r > 1.0 then dwR:= 255 else if Value.r < 0 then dwR:= 0 else dwR:= DWORD(Trunc(Value.r * 255.0 + 0.5));
  if Value.g > 1.0 then dwG:= 255 else if Value.g < 0 then dwG:= 0 else dwG:= DWORD(Trunc(Value.g * 255.0 + 0.5));
  if Value.b > 1.0 then dwB:= 255 else if Value.b < 0 then dwB:= 0 else dwB:= DWORD(Trunc(Value.b * 255.0 + 0.5));
  if Value.a > 1.0 then dwA:= 255 else if Value.a < 0 then dwA:= 0 else dwA:= DWORD(Trunc(Value.a * 255.0 + 0.5));

  Result := (dwA shl 24) or (dwR shl 16) or (dwG shl 8) or dwB;
end;

end.

