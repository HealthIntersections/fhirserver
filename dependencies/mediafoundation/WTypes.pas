// FactoryX
// factoryx.code@gmail.com
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: WTypes.pas
// Kind: Pascal Unit
// Release date: 25-09-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
//              Holds common types used by Media Foundation and Core Audio
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
// LastEdited by: Tony (maXcomX)
// EditDate: 290912b
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
// Microsoft Windows
// Copyright (c) 1997-2012 Microsoft Corporation. All rights reserved
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
unit WTypes;

interface

uses
	Windows, ComObj, ActiveX, MfpTypes;

// Forward Declarations

// header files for imported files
//#include "basetsd.h"
//#include "guiddef.h"


// interface IWinTypes

type

  tagRemHGLOBAL = record
    fNullHGlobal: Longint;
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  RemHGLOBAL = tagRemHGLOBAL;

  tagRemHMETAFILEPICT = record
    mm: Longint;
    xExt: Longint;
    yExt: Longint;
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  RemHMETAFILEPICT = tagRemHMETAFILEPICT;

	tagRemHENHMETAFILE = record
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  RemHENHMETAFILE = tagRemHENHMETAFILE;

  tagRemHBITMAP = record
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  RemHBITMAP = tagRemHBITMAP;

  tagRemHPALETTE = record
    cbData: Longword;
    data: array[0..1] of byte;
  end;
  RemHPALETTE = tagRemHPALETTE;

	tagRemBRUSH = record
		cbData: Longword;
    data: array[0..1] of byte;
  end;
  RemHBRUSH = tagRemBRUSH;

	//UINT = Cardinal;   see: Windows
	//INT = Integer;     see: Windows
	//BOOL = Longint;    see: Windows

{$IFNDEF LONG}
	LONG = Longint;
{$ENDIF}

{$IFNDEF WPARAM}
	WPARAM = UINT_PTR;
{$ENDIF}

{$IFNDEF DWORD}
	DWORD = Longword;
{$ENDIF}

{$IFNDEF LPARAM}
	LPARAM = LONG_PTR;
{$ENDIF}

{$IFNDEF LRESULT}
	LRESULT = LONG_PTR;
{$ENDIF}


	HANDLE = Pointer;
	HMODULE = Pointer;
	HINSTANCE = Pointer;
  HTASK = Pointer;
  HKEY = Pointer;
  THkey = Pointer;
  HDESK = Pointer;
  HMF = Pointer;
  HEMF = Pointer;
  THemf = Pointer;
  HPEN = Pointer;
  HRSRC = Pointer;
  HSTR = Pointer;
  HWINSTA = Pointer;
  THwinsta = Pointer;
  HKL = Pointer;
  THkl = Pointer;
  HGDIOBJ = Pointer;
  HDWP = THandle;
  THdwp = THandle;

{$IFNDEF HFILE}
	PHfile = ^Hfile;
	HFILE = INT;
{$ENDIF}

{$IFNDEF LPWORD}
	LPWORD = ^WORD;
{$ENDIF}

{$IFNDEF LPDWORD}
  LPDWORD = ^DWORD;
{$ENDIF}

{
typedef char CHAR;

typedef /* [string] */  __RPC_string CHAR *LPSTR;

typedef /* [string] */  __RPC_string const CHAR *LPCSTR;

#ifndef _WCHAR_DEFINED
#define _WCHAR_DEFINED
typedef wchar_t WCHAR;

typedef WCHAR TCHAR;

#endif // !_WCHAR_DEFINED
typedef /* [string] */  __RPC_string WCHAR *LPWSTR;

typedef /* [string] */  __RPC_string TCHAR *LPTSTR;

typedef /* [string] */  __RPC_string const WCHAR *LPCWSTR;

typedef /* [string] */  __RPC_string const TCHAR *LPCTSTR;
}

{$IFNDEF COLORREF}
	COLORREF = DWORD;
	TColorref = DWORD;
{$ENDIF}

{$IFNDEF LPCOLORREF}
	LPCOLORREF = ^COLORREF;
{$ENDIF}


{$IFNDEF LPHANDLE}
	LPHANDLE = ^THandle;
{$ENDIF}

  _RECTL = record
    left: LONG;
    top: LONG;
    right: LONG;
    bottom: LONG;
  end;
  RECTL = _RECTL;
  PRECTL = ^_RECTL;
  LPRECTL = ^_RECTL;


  tagPOINT = record
    x: LONG;
    y: LONG;
  end;
  POINT = tagPOINT;
  PPOINT = ^tagPOINT;
  LPPOINT = ^tagPOINT;


  _POINTL = record
    x: LONG;
    y: LONG;
  end;
  POINTL = _POINTL;
  PPOINTL = ^_POINTL;

{$IFNDEF WIN16}

  tagSIZE = record
    cx: LONG;
    cy: LONG;
  end;
  SIZE = tagSIZE;
  PSIZE = ^tagSIZE;
  LPSIZE = ^tagSIZE;

{$ELSE} // WIN16

  tagSIZE = record
    cx: INT;
    cy: INT;
  end;
  SIZE = tagSIZE;
  PSIZE = ^tagSIZE;
  LPSIZE = ^tagSIZE;
{$ENDIF} // WIN16

  tagSIZEL = record
    cx: LONG;
    cy: LONG;
  end;
  SIZEL = tagSIZEL;
  TSizel = tagSIZEL;
  PSIZEL = ^tagSIZEL;
  LPSIZEL = ^tagSIZEL;


{$IFNDEF PALETTEENTRY}
  tagPALETTEENTRY = record
    peRed: Byte;
    peGreen: Byte;
    peBlue: Byte;
    peFlags: Byte;
  end;
	PALETTEENTRY = tagPALETTEENTRY;
  PPALETTEENTRY = ^tagPALETTEENTRY;
	LPPALETTEENTRY = ^tagPALETTEENTRY;
{$ENDIF}


{$IFNDEF LOGPALETTE}
  tagLOGPALETTE = record
    palVersion: WORD;
		palNumEntries: WORD;
    palPalEntry: array[0..0] of PALETTEENTRY;
  end;
	LOGPALETTE = tagLOGPALETTE;
  PLOGPALETTE = ^tagLOGPALETTE;
	LPLOGPALETTE = ^tagLOGPALETTE;
{$ENDIF}

//OLE >>> Declared in the Delphi System unit
//#if defined(_WIN32) && !defined(OLE2ANSI)
//typedef WCHAR OLECHAR;

//typedef /* [string] */  __RPC_string OLECHAR *LPOLESTR;

//typedef /* [string] */  __RPC_string const OLECHAR *LPCOLESTR;

//#define OLESTR(str) L##str

//#else

//typedef char      OLECHAR;
//typedef LPSTR     LPOLESTR;
//typedef LPCSTR    LPCOLESTR;
//#define OLESTR(str) str
//#endif

//Other System
//#ifndef _WINDEF_
//typedef const RECTL *LPCRECTL;

//typedef void *PVOID;

//typedef void *LPVOID;

//typedef float FLOAT;

//typedef struct tagRECT
//    {
//    LONG left;
//    LONG top;
//    LONG right;
//    LONG bottom;
//    } 	RECT;

//typedef struct tagRECT *PRECT;

//typedef struct tagRECT *LPRECT;

//typedef const RECT *LPCRECT;

//#endif  //_WINDEF_

	UCHAR = Byte;
	//SHORT = Smallint;  see: Windows
	USHORT = Word;
	ULONG = DWORD;

//typedef double DOUBLE;   //Delphi Double

{$IFNDEF DWORDLONG}
  DWORDLONG = UInt64;
  PDWORDLONG = ^DWORDLONG;
{$ENDIF}

{$IFNDEF ULONGLONG}
	LONGLONG = int64;
	ULONGLONG = UInt64;
  PLONGLONG = ^LONGLONG;
	PULONGLONG = ^ULONGLONG;
{$ENDIF}


  _LARGE_INTEGER = record
    QuadPart: LONGLONG;
  end;
  LARGE_INTEGER = _LARGE_INTEGER;
  PLARGE_INTEGER = ^LARGE_INTEGER;
  _ULARGE_INTEGER = record
    QuadPart: ULONGLONG;
  end;
  ULARGE_INTEGER = _ULARGE_INTEGER;
{$IFNDEF _WINBASE_}


{$IFNDEF FILETIME}
  _FILETIME = record
    dwLowDateTime: DWORD;
		dwHighDateTime: DWORD;
	end;
  FILETIME = _FILETIME;
  PFILETIME = ^_FILETIME;
  LPFILETIME = ^_FILETIME;
{$ENDIF}


{$IFNDEF SYSTEMTIME}
  _SYSTEMTIME = record
    wYear: WORD;
    wMonth: WORD;
    wDayOfWeek: WORD;
    wDay: WORD;
    wHour: WORD;
    wMinute: WORD;
    wSecond: WORD;
    wMilliseconds: WORD;
  end;
  SYSTEMTIME = _SYSTEMTIME;
  PSYSTEMTIME = ^_SYSTEMTIME;
  LPSYSTEMTIME = ^_SYSTEMTIME;
{$ENDIF}


{$IFNDEF SECURITY_ATTRIBUTES}
  _SECURITY_ATTRIBUTES = record
		nLength: DWORD;
		lpSecurityDescriptor: Pointer;
		bInheritHandle: BOOL;
	end;
	SECURITY_ATTRIBUTES = _SECURITY_ATTRIBUTES;
  PSECURITY_ATTRIBUTES = ^_SECURITY_ATTRIBUTES;
  LPSECURITY_ATTRIBUTES = ^_SECURITY_ATTRIBUTES;
{$ENDIF}


{$IFNDEF SECURITY_DESCRIPTOR_REVISION}
	SECURITY_DESCRIPTOR_CONTROL = USHORT;
	PSECURITY_DESCRIPTOR_CONTROL = ^USHORT;

  PSID = Pointer;

  _ACL = record
    AclRevision: UCHAR;
    Sbz1: UCHAR;
    AclSize: USHORT;
    AceCount: USHORT;
    Sbz2: USHORT;
  end;
  ACL = _ACL;
  TAcl = _ACL;
  PACL = ^ACL;

  _SECURITY_DESCRIPTOR = record
    Revision: UCHAR;
    Sbz1: UCHAR;
    Control: SECURITY_DESCRIPTOR_CONTROL;
    Owner: PSID;
    Group: PSID;
    Sacl: PACL;
    Dacl: PACL;
  end;
  SECURITY_DESCRIPTOR = _SECURITY_DESCRIPTOR;
  PISECURITY_DESCRIPTOR = ^_SECURITY_DESCRIPTOR;

{$ENDIF} // SECURITY_DESCRIPTOR_REVISION
{$ENDIF} // _WINBASE_


  _COAUTHIDENTITY = record
    User: PUSHORT;
    UserLength: ULONG;
    Domain: PUSHORT;
    DomainLength: ULONG;
    Password: PUSHORT;
    PasswordLength: ULONG;
    Flags: ULONG;
  end;
	COAUTHIDENTITY = _COAUTHIDENTITY;
	PCOAUTHIDENTITY = ^COAUTHIDENTITY;

  _COAUTHINFO = record
    dwAuthnSvc: DWORD;
    dwAuthzSvc: DWORD;
    pwszServerPrincName: PWideChar;
    dwAuthnLevel: DWORD;
    dwImpersonationLevel: DWORD;
    pAuthIdentityData: PCOAUTHIDENTITY;
    dwCapabilities: DWORD;
  end;
  COAUTHINFO = _COAUTHINFO;

  SCODE = LONG;
  PSCODE = ^SCODE;



{$IFNDEF OBJECTID}
  _OBJECTID = record
		Lineage: TGUID;
    Uniquifier: Longword;
	end;
  OBJECTID = _OBJECTID;
{$ENDIF}


{$IFNDEF GUID}
	REFGUID = ^TGUID;
	REFIID = ^IID;
	REFCLSID = ^CLSID;
  REFFMTID = ^FMTID;
{$ENDIF}


	tagMEMCTX          = (
    MEMCTX_TASK      = 1,
    MEMCTX_SHARED    = 2,
    MEMCTX_MACSYSTEM = 3,
		MEMCTX_UNKNOWN   =  - 1,
    MEMCTX_SAME      =  - 2
  );
  MEMCTX = tagMEMCTX;


const
{$IFNDEF ROTFLAGS_REGISTRATIONKEEPSALIVE}
	ROTFLAGS_REGISTRATIONKEEPSALIVE     = $1;
	ROTFLAGS_ALLOWANYCLIENT             = $2;
{$ENDIF}

{$IFNDEF OBJECTID}
	ROTREGFLAGS_ALLOWANYCLIENT          = $1;
{$ENDIF}

{$IFNDEF OBJECTID}
	APPIDREGFLAGS_ACTIVATE_IUSERVER_INDESKTOP       = $1;
	APPIDREGFLAGS_SECURE_SERVER_PROCESS_SD_AND_BIND = $2;
  APPIDREGFLAGS_ISSUE_ACTIVATION_RPC_AT_IDENTIFY  = $4;
{$ENDIF}

{$IFNDEF ROT_COMPARE_MAX}
	ROT_COMPARE_MAX                     = 2048;
{$ENDIF}

{$IFNDEF DCOMSCM_REMOTECALL_FLAGS}
	DCOMSCM_ACTIVATION_USE_ALL_AUTHNSERVICES  = $1;
	DCOMSCM_ACTIVATION_DISALLOW_UNSECURE_CALL = $2;
	DCOMSCM_RESOLVE_USE_ALL_AUTHNSERVICES     = $4;
	DCOMSCM_RESOLVE_DISALLOW_UNSECURE_CALL    = $8;
  DCOMSCM_PING_USE_MID_AUTHNSERVICE         = $10;
  DCOMSCM_PING_DISALLOW_UNSECURE_CALL       = $20;
{$ENDIF}


type
  tagCLSCTX                       = (
    CLSCTX_INPROC_SERVER          = $1,
    CLSCTX_INPROC_HANDLER         = $2,
    CLSCTX_LOCAL_SERVER           = $4,
		CLSCTX_INPROC_SERVER16        = $8,
    CLSCTX_REMOTE_SERVER          = $10,
    CLSCTX_INPROC_HANDLER16       = $20,
    CLSCTX_RESERVED1              = $40,
    CLSCTX_RESERVED2              = $80,
    CLSCTX_RESERVED3              = $100,
    CLSCTX_RESERVED4              = $200,
    CLSCTX_NO_CODE_DOWNLOAD       = $400,
    CLSCTX_RESERVED5              = $800,
    CLSCTX_NO_CUSTOM_MARSHAL      = $1000,
    CLSCTX_ENABLE_CODE_DOWNLOAD   = $2000,
    CLSCTX_NO_FAILURE_LOG         = $4000,
    CLSCTX_DISABLE_AAA            = $8000,
    CLSCTX_ENABLE_AAA             = $10000,
    CLSCTX_FROM_DEFAULT_CONTEXT   = $20000,
    CLSCTX_ACTIVATE_32_BIT_SERVER = $40000,
    CLSCTX_ACTIVATE_64_BIT_SERVER = $80000,
		CLSCTX_ENABLE_CLOAKING        = $100000,
		CLSCTX_PS_DLL                 = Integer($80000000)
		); CLSCTX = tagCLSCTX;
		

	tagMSHLFLAGS            = (
    MSHLFLAGS_NORMAL      = 0,
    MSHLFLAGS_TABLESTRONG = 1,
    MSHLFLAGS_TABLEWEAK   = 2,
    MSHLFLAGS_NOPING      = 4,
    MSHLFLAGS_RESERVED1   = 8,
    MSHLFLAGS_RESERVED2   = 16,
    MSHLFLAGS_RESERVED3   = 32,
    MSHLFLAGS_RESERVED4   = 64
  );
	MSHLFLAGS = tagMSHLFLAGS;


  tagMSHCTX                 = (
    MSHCTX_LOCAL            = 0,
    MSHCTX_NOSHAREDMEM      = 1,
    MSHCTX_DIFFERENTMACHINE = 2,
    MSHCTX_INPROC           = 3,
    MSHCTX_CROSSCTX         = 4
  );
  MSHCTX = tagMSHCTX;


  tagDVASPECT          = (
    DVASPECT_CONTENT   = 1,
    DVASPECT_THUMBNAIL = 2,
    DVASPECT_ICON      = 4,
    DVASPECT_DOCPRINT  = 8
  );
  DVASPECT = tagDVASPECT;


  tagSTGC                                   = (
    STGC_DEFAULT                            = 0,
    STGC_OVERWRITE                          = 1,
    STGC_ONLYIFCURRENT                      = 2,
    STGC_DANGEROUSLYCOMMITMERELYTODISKCACHE = 4,
    STGC_CONSOLIDATE                        = 8
  );
  STGC = tagSTGC;


  tagSTGMOVE            = (
    STGMOVE_MOVE        = 0,
    STGMOVE_COPY        = 1,
    STGMOVE_SHALLOWCOPY = 2
  );
  STGMOVE = tagSTGMOVE;


  tagSTATFLAG        = (
    STATFLAG_DEFAULT = 0,
    STATFLAG_NONAME  = 1,
    STATFLAG_NOOPEN  = 2
  );
  STATFLAG = tagSTATFLAG;

{$IFNDEF HCONTEXT}
	HCONTEXT = Pointer;
{$ENDIF}

{$IFNDEF LCID}
  LCID = DWORD;
{$ENDIF}


{$IFNDEF LANGID}
  LANGID = USHORT;
{$ENDIF}


  _BYTE_BLOB = record
    clSize: Longword;
    abData: array[0..1] of byte;
  end;
	BYTE_BLOB = _BYTE_BLOB;
  UP_BYTE_BLOB = ^BYTE_BLOB;

  _WORD_BLOB = record
    clSize: Longword;
    asData: array[0..1] of Word;
  end;
  WORD_BLOB = _WORD_BLOB;
  UP_WORD_BLOB = ^WORD_BLOB;

  _DWORD_BLOB = record
    clSize: Longword;
    alData: array[0..0] of Longword;
  end;
  DWORD_BLOB = _DWORD_BLOB;
  UP_DWORD_BLOB = ^DWORD_BLOB;


  _FLAGGED_BYTE_BLOB = record
    fFlags: Longword;
    clSize: Longword;
    abData: array[0..0] of byte;
  end;
  FLAGGED_BYTE_BLOB = _FLAGGED_BYTE_BLOB;
  UP_FLAGGED_BYTE_BLOB = ^FLAGGED_BYTE_BLOB;


  _FLAGGED_WORD_BLOB = record
    fFlags: Longword;
    clSize: Longword;
    asData: array[0..1] of Word;
  end;
  FLAGGED_WORD_BLOB = _FLAGGED_WORD_BLOB;
  UP_FLAGGED_WORD_BLOB = ^FLAGGED_WORD_BLOB;


  _BYTE_SIZEDARR = record
    clSize: Longword;
    pData: Pbyte;
  end;
  BYTE_SIZEDARR = _BYTE_SIZEDARR;


  _SHORT_SIZEDARR = record
    clSize: Longword;
    pData: PWord;
  end;
  WORD_SIZEDARR = _SHORT_SIZEDARR;


  _LONG_SIZEDARR = record
    clSize: Longword;
    pData: PLongword;
  end;
  DWORD_SIZEDARR = _LONG_SIZEDARR;


	_HYPER_SIZEDARR = record
    clSize: Longword;
		pData: Phyper;
	end;
	HYPER_SIZEDARR = _HYPER_SIZEDARR;

const
	WDT_INPROC_CALL                     = $48746457;
  WDT_REMOTE_CALL                     = $52746457;
  WDT_INPROC64_CALL                   = $50746457;


 //

////////////////////////////////////////////////////////////////////////////////
// The BSTRBLOB structure is used by some implementations
// of the IPropertyStorage interface when marshaling BSTRs
// on systems which don't support BSTR marshaling.
////////////////////////////////////////////////////////////////////////////////
type
{$IFNDEF tagBSTRBLOB}
	tagBSTRBLOB = record
    cbSize: ULONG;
    pData: PByte;
  end;
  BSTRBLOB = tagBSTRBLOB;
  LPBSTRBLOB = ^tagBSTRBLOB;
{$ENDIF}


const
	VARIANT_TRUE                        = VARIANT_BOOL(-1);
	VARIANT_FALSE                       = VARIANT_BOOL(0);


type
{$IFNDEF tagBLOB}
	{$DEFINE BLOB}
	{$DEFINE LPBLOB}
	tagBLOB = record
		cbSize: ULONG;
    pBlobData: PByte;
	end;
	BLOB = tagBLOB;
  LPBLOB = ^tagBLOB;
{$ENDIF}


  tagCLIPDATA = record
    cbSize: ULONG;
    ulClipFmt: Longint;
    pClipData: PByte;
  end;
  CLIPDATA = tagCLIPDATA;

// function to calculate the size of the above pClipData
function CBPCLIPDATA(_clipdata: CLIPDATA): PByte;

type
	VARTYPE = Word;

//
// VARENUM usage key,
//
// [V] - may appear in a VARIANT
// [T] - may appear in a TYPEDESC
// [P] - may appear in an OLE property set
// [S] - may appear in a Safe Array
//
//
//   VT_EMPTY            [V]   [P]     nothing
//   VT_NULL             [V]   [P]     SQL style Null
//   VT_I2               [V][T][P][S]  2 byte signed int
//   VT_I4               [V][T][P][S]  4 byte signed int
//   VT_R4               [V][T][P][S]  4 byte real
//   VT_R8               [V][T][P][S]  8 byte real
//   VT_CY               [V][T][P][S]  currency
//   VT_DATE             [V][T][P][S]  date
//   VT_BSTR             [V][T][P][S]  OLE Automation string
//   VT_DISPATCH         [V][T]   [S]  IDispatch *
//   VT_ERROR            [V][T][P][S]  SCODE
//   VT_BOOL             [V][T][P][S]  True=-1, False=0
//   VT_VARIANT          [V][T][P][S]  VARIANT *
//   VT_UNKNOWN          [V][T]   [S]  IUnknown *
//   VT_DECIMAL          [V][T]   [S]  16 byte fixed point
//   VT_RECORD           [V]   [P][S]  user defined type
//   VT_I1               [V][T][P][s]  signed char
//   VT_UI1              [V][T][P][S]  unsigned char
//   VT_UI2              [V][T][P][S]  unsigned short
//   VT_UI4              [V][T][P][S]  unsigned long
//   VT_I8                  [T][P]     signed 64-bit int
//   VT_UI8                 [T][P]     unsigned 64-bit int
//   VT_INT              [V][T][P][S]  signed machine int
//   VT_UINT             [V][T]   [S]  unsigned machine int
//   VT_INT_PTR             [T]        signed machine register size width
//   VT_UINT_PTR            [T]        unsigned machine register size width
//   VT_VOID                [T]        C style void
//   VT_HRESULT             [T]        Standard return type
//   VT_PTR                 [T]        pointer type
//   VT_SAFEARRAY           [T]        (use VT_ARRAY in VARIANT)
//   VT_CARRAY              [T]        C style array
//   VT_USERDEFINED         [T]        user defined type
//   VT_LPSTR               [T][P]     null terminated string
//   VT_LPWSTR              [T][P]     wide null terminated string
//   VT_FILETIME               [P]     FILETIME
//   VT_BLOB                   [P]     Length prefixed bytes
//   VT_STREAM                 [P]     Name of the stream follows
//   VT_STORAGE                [P]     Name of the storage follows
//   VT_STREAMED_OBJECT        [P]     Stream contains an object
//   VT_STORED_OBJECT          [P]     Storage contains an object
//   VT_VERSIONED_STREAM       [P]     Stream with a GUID version
//   VT_BLOB_OBJECT            [P]     Blob contains an object
//   VT_CF                     [P]     Clipboard format
//   VT_CLSID                  [P]     A Class ID
//   VT_VECTOR                 [P]     simple counted array
//   VT_ARRAY            [V]           SAFEARRAY*
//   VT_BYREF            [V]           void* for local use
//   VT_BSTR_BLOB                      Reserved for system use
///////////////////////////////////////////////////////////////////////////////

const

		VT_EMPTY            = 0;
		VT_NULL             = 1;
		VT_I2               = 2;
		VT_I4               = 3;
		VT_R4               = 4;
		VT_R8               = 5;
		VT_CY               = 6;
		VT_DATE             = 7;
		VT_BSTR             = 8;
		VT_DISPATCH         = 9;
    VT_ERROR            = 10;
		VT_BOOL             = 11;
    VT_VARIANT          = 12;
		VT_UNKNOWN          = 13;
    VT_DECIMAL          = 14;
		VT_I1               = 16;
    VT_UI1              = 17;
		VT_UI2              = 18;
    VT_UI4              = 19;
		VT_I8               = 20;
    VT_UI8              = 21;
		VT_INT              = 22;
    VT_UINT             = 23;
		VT_VOID             = 24;
    VT_HRESULT          = 25;
		VT_PTR              = 26;
    VT_SAFEARRAY        = 27;
		VT_CARRAY           = 28;
    VT_USERDEFINED      = 29;
		VT_LPSTR            = 30;
    VT_LPWSTR           = 31;
		VT_RECORD           = 36;
    VT_INT_PTR          = 37;
		VT_UINT_PTR         = 38;
    VT_FILETIME         = 64;
		VT_BLOB             = 65;
    VT_STREAM           = 66;
		VT_STORAGE          = 67;
    VT_STREAMED_OBJECT  = 68;
		VT_STORED_OBJECT    = 69;
    VT_BLOB_OBJECT      = 70;
		VT_CF               = 71;
    VT_CLSID            = 72;
		VT_VERSIONED_STREAM = 73;
    VT_BSTR_BLOB        = $FFF;
		VT_VECTOR           = $1000;
    VT_ARRAY            = $2000;
		VT_BYREF            = $4000;
    VT_RESERVED         = $8000;
		VT_ILLEGAL          = $FFFF;
		VT_ILLEGALMASKED    = $FFF;
		VT_TYPEMASK         = $FFF;

type
	PROPID = ULONG;


{$IFNDEF PROPERTYKEY}
	_tagpropertykey = record
		fmtid: TGUID;
		pid: DWORD;
  end;
  PROPERTYKEY = _tagpropertykey;
{$ENDIF}


{$IFNDEF SID_IDENTIFIER_AUTHORITY}
  _SID_IDENTIFIER_AUTHORITY = record
		Value: array[0..5] of UCHAR;
  end;
  SID_IDENTIFIER_AUTHORITY = _SID_IDENTIFIER_AUTHORITY;
  PSID_IDENTIFIER_AUTHORITY = ^_SID_IDENTIFIER_AUTHORITY;
{$ENDIF}


{$IFNDEF SID}
  _SID = record
    Revision: Byte;
		SubAuthorityCount: Byte;
    IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
    SubAuthority: array[0..0] of ULONG;
  end;
  SID = _SID;
  PISID = ^_SID;


  _SID_AND_ATTRIBUTES = record
    Sid: PSID;
    Attributes: DWORD;
  end;
  SID_AND_ATTRIBUTES = _SID_AND_ATTRIBUTES;
  PSID_AND_ATTRIBUTES = ^_SID_AND_ATTRIBUTES;
{$ENDIF}


  tagCSPLATFORM = record
    dwPlatformId: DWORD;
    dwVersionHi: DWORD;
    dwVersionLo: DWORD;
    dwProcessorArch: DWORD;
  end;
  CSPLATFORM = tagCSPLATFORM;


  tagQUERYCONTEXT = record
    dwContext: DWORD;
    Platform: CSPLATFORM;
    Locale: LCID;
    dwVersionHi: DWORD;
    dwVersionLo: DWORD;
  end;
  QUERYCONTEXT = tagQUERYCONTEXT;


  tagTYSPEC            = (
    TYSPEC_CLSID       = 0,
    TYSPEC_FILEEXT     = 1,
    TYSPEC_MIMETYPE    = 2,
    TYSPEC_FILENAME    = 3,
    TYSPEC_PROGID      = 4,
    TYSPEC_PACKAGENAME = 5,
    TYSPEC_OBJECTID    = 6
  );
  TYSPEC = tagTYSPEC;

//TODO:
//typedef /* [public] */ struct __MIDL___MIDL_itf_wtypes_0000_0001_0001
//    {
//    DWORD tyspec;
//    /* [switch_type] */ union __MIDL___MIDL_itf_wtypes_0000_0001_0005
//        {
//        CLSID clsid;
//        LPOLESTR pFileExt;
//        LPOLESTR pMimeType;
//        LPOLESTR pProgId;
//        LPOLESTR pFileName;
//        struct
//            {
//            LPOLESTR pPackageName;
//            GUID PolicyId;
//            } 	ByName;
//        struct
//            {
//            GUID ObjectId;
//            GUID PolicyId;
//            } 	ByObjectId;
//        } 	tagged_union;
//    } 	uCLSSPEC;



// Additional Prototypes for ALL interfaces

// end of Additional Prototypes


implementation


function CBPCLIPDATA(_clipdata: CLIPDATA): PByte;
begin
	Result:= Pbyte(_clipdata.cbSize - sizeof(_clipdata.ulClipFmt));
end;


end.
