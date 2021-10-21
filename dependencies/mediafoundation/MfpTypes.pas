// FactoryX
// factoryx.code@gmail.com
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: MfpTypes.pas
// Kind: Pascal Unit
// Release date: 29-07-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
//              Holds common types used by Media Foundation and Core Audio
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
// LastEdited by: Tony (maXcomX)
// EditDate: 290712b, updt 120914b
//
// Remarks:
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: General check
// =============================================================================
// Source: -
//
// Copyright (c) 1997-2012 Microsoft Corporation. All rights reserved
// Copyright (c) 2007 clootie (DirectX related)
//==============================================================================
// The contents of this file are subject to the Mozilla Public
// License Version 1.1 (the "License"). you may not use this file
// except in compliance with the License. You may obtain a copy of
// the License at http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
//
// Licence Supplement:
// All kinds of commercial use, including distribution, of any kind, of the sources is strictly forbidden,
// unless you have a written permission or agreement from or with FactoryX.
//==============================================================================

unit MfpTypes;

interface


uses
	Windows, ComObj, ActiveX, Controls;

////////////////////////////////////////////////////////////////////////////////
// Generic Windows types for Win32 / Win64 compatibility
// used by DirectX, MFPACK, Media Foundation and Core Audio
////////////////////////////////////////////////////////////////////////////////


type
// DirectX ///////////////////////////////////////////////////////////////////
  //Clootie

  {$IFDEF WIN64}
  INT_PTR    = Int64;
  UINT_PTR   = UInt64;
  LONG_PTR   = Int64;
  ULONG_PTR  = UInt64;
  DWORD_PTR  = UInt64;
  {$ELSE}
	INT_PTR    = LongInt;
  UINT_PTR   = LongWord;
  LONG_PTR   = Longint;
	ULONG_PTR  = LongWord;
  DWORD_PTR  = LongWord;
  {$ENDIF}
  PINT_PTR   = ^INT_PTR;
  PUINT_PTR  = ^UINT_PTR;
  PLONG_PTR  = ^LONG_PTR;
  PULONG_PTR = ^ULONG_PTR;

{$IFNDEF FPC}
  {$IFDEF WIN64}
  PtrInt     = Int64;
  PtrUInt    = UInt64;
  {$ELSE}
  PtrInt     = Longint;
  PtrUInt    = Longword;
  {$ENDIF}
  PPtrInt    = ^PtrInt;
  PPtrUInt   = ^PtrUInt;
{$ENDIF}

  //////////////////////////////////////////////////////////////////////////////
  // SIZE_T used for counts or ranges which need to span the range of
  // of a pointer.  SSIZE_T is the signed variation.
  //////////////////////////////////////////////////////////////////////////////

  SIZE_T     = ULONG_PTR;
  SSIZE_T    = LONG_PTR;
  PSIZE_T    = ^SIZE_T;
  PSSIZE_T   = ^SSIZE_T;
{$IFNDEF FPC}

  SizeInt    = SSIZE_T;
  SizeUInt   = SIZE_T;
  PSizeInt   = PSSIZE_T;
  PSizeUInt  = PSIZE_T;
{$ENDIF}
	//Clootie - end


//MFPACK - Common
	INT             = Integer;
	LONG            = Integer;
	PLONG           = ^LONG;
	PULONG          = ^ULONG;
	LONGLONG        = Int64;
	DWORDLONG       = LONGLONG;  //Unsigned Int64
	ULONGLONG       = DWORDLONG; //Unsigned Int64
	UINT8           = Byte;
	UINT32          = Cardinal;
	UINT64          = Int64;
	PSHORT          = ^SHORT;
	PUSHORT         = PWORD;
	FLOAT           = Single;
	PFLOAT          = PSingle;
	VARIANT_BOOL    = WordBool;
	_VARIANT_BOOL   = VARIANT_BOOL;
	PVARIANT_BOOL   = ^VARIANT_BOOL;
	SYSINT          = Integer;
	CY              = Currency;
	PCY             = ^CY;
	LARGE_INTEGER   = LongInt;
	PLARGE_INTEGER  = ^LARGE_INTEGER;
	ULARGE_INTEGER  = Cardinal;
	PULARGE_INTEGER = ^ULARGE_INTEGER;
	//
	bstrVal         = WideString;
	PBSTRBLOB       = WideString;
	BSTR            = WideChar;
	//
	DATE            = TDate;
	
{$IFNDEF GUID}
	GUID = TGUID;
{$ENDIF}

//GUID related
	//REFGUID    = GUID;  see: WTypes
	//REFIID     = IID;  see: WTypes

{$IFNDEF CLSID}
	CLSID      = GUID;
{$ENDIF}

{$IFNDEF IID}
	IID        = GUID;
{$ENDIF}

	PGUID      = ^GUID;
	LPCGUID    = PGUID; //See also: ComObj for TGuid specs

//
	Hyper = int64; // Signed or unsigned 64 bit
	PHyper = ^Hyper;

//-----------------------
//SPECIFIC PLATFORM TYPES
//-----------------------

//Core Audio

  REFERENCE_TIME   = LONGLONG;
  PReferenceTime   = ^REFERENCE_TIME;

  //used by Endpoint Volume
const
  MAX_INPUT_VALUE  = High(WORD);
  MIN_INPUT_VALUE  = 0;
  MAX_OUTPUT_VALUE = High(WORD);
  MIN_OUTPUT_VALUE = 0;


//Media Foundation


//Special types
{$IFNDEF MemoryAllocator}
type
 PMemoryAllocator = ^MemoryAllocator;
 MemoryAllocator = function(Size: DWORD): Pointer; stdcall;
{$ENDIF}

//Special functions

  // A special function to translate Delphi boolean to a C++ BOOL (4 bytes) in some rare cases,
	// like the Get/set Mute functions, Core Audio on Windows Vista, 7 and 8
  // See: http://blog.delphi-jedi.net/2008/09/25/bool-boolean-and-integer
  function CBOOL(Value: boolean): DWORD;


implementation


function CBOOL(Value: boolean): DWORD;
begin
  if Value = true then
    Result:= 1
  else
    Result:= 0;
  //as an altenative you could use: Result:= integer(Value), which does the same
end;

end.