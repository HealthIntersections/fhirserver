// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: PropIdl.pas
// Kind: Pascal Unit
// Release date: 27-06-2012
// Language: ENU
//
// Version: 1.0.0.1 
// Description: Requires Windows Vista or later. 
//              Structured Storage. 
// 
// Intiator(s): Tony (maXcomX), Peter (OzShips) 
// 
// LastEdited by: Tony (maXcomX)
// EditDate: updt 100712b, 260912B
//
// Remarks:
//          Delphi : The IUnknown entries of functions should be casted like this:
//                   IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: OaIdl translation
// =============================================================================
// Source: propidl.h
//
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
//==============================================================================

unit PropIdl;

	{$MINENUMSIZE 4}
	{$WEAKPACKAGEUNIT}

interface

uses
	Windows, ComObj, ActiveX, Unknwn, MfpTypes, WTypes;

const
	IID_IPropertyStorage                 : TGUID = '{00000138-0000-0000-C000-000000000046}';
	IID_IPropertySetStorage              : TGUID = '{0000013A-0000-0000-C000-000000000046}';
  IID_IEnumSTATPROPSTG                 : TGUID = '{00000139-0000-0000-C000-000000000046}';
  IID_IEnumSTATPROPSETSTG              : TGUID = '{00000139-0000-0000-C000-000000000046}';

	CCH_MAX_PROPSTG_NAME                = 31;


type
  tagVersionedStream = record
    guidVersion: TGUID;
		pStream: ^IStream;
	end;
	VERSIONEDSTREAM = tagVersionedStream;
	LPVERSIONEDSTREAM = ^tagVersionedStream;

                            
const
	// Flags for IPropertySetStorage.Create

	PROPSETFLAG_DEFAULT                 = 0;
	PROPSETFLAG_NONSIMPLE               = 1;
	PROPSETFLAG_ANSI                    = 2;

	// This flag is only supported on StgCreatePropStg & StgOpenPropStg
	PROPSETFLAG_UNBUFFERED              = 4;

  // This flag causes a version-1 property set to be created
	PROPSETFLAG_CASE_SENSITIVE          = 8;

// Flags for the reservied PID_BEHAVIOR property
	PROPSET_BEHAVIOR_CASE_SENSITIVE     = 1;

type
  // This is the standard C layout of the PROPVARIANT.
	//typedef struct tagPROPVARIANT PROPVARIANT;

	tagCAC = record
		cElems: ULONG;
		pElems: PAnsiChar;
	end;
	CAC = tagCAC;

  tagCAUB = record
    cElems: ULONG;
    pElems: PUCHAR;
	end;
  CAUB = tagCAUB;

  tagCAI = record
    cElems: ULONG;
		pElems: PSHORT;
	end;
  CAI = tagCAI;

  tagCAUI = record
		cElems: ULONG;
    pElems: PUSHORT;
	end;
  CAUI = tagCAUI;

  tagCAL = record
    cElems: ULONG;
    pElems: PLONG;
	end;
  CAL = tagCAL;

  tagCAUL = record
    cElems: ULONG;
    pElems: PULONG;
	end;
  CAUL = tagCAUL;

  tagCAFLT = record
    cElems: ULONG;
    pElems: PFLOAT;
	end;
	CAFLT = tagCAFLT;

  tagCADBL = record
    cElems: ULONG;
    pElems: PDOUBLE;
	end;
  CADBL = tagCADBL;

  tagCACY = record
    cElems: ULONG;
    pElems: PCY;
	end;
  CACY = tagCACY;

	tagCADATE = record
    cElems: ULONG;
    pElems: PDATE;
	end;
  CADATE = tagCADATE;

  tagCABSTR = record
    cElems: ULONG;
    pElems: PBSTR;
	end;
  CABSTR = tagCABSTR;

	tagCABSTRBLOB = record
    cElems: ULONG;
		pElems: PBSTR;
	end;
	CABSTRBLOB = tagCABSTRBLOB;

	tagCABOOL = record
    cElems: ULONG;
    pElems: PVARIANT_BOOL;
	end;
  CABOOL = tagCABOOL;

  tagCASCODE = record
    cElems: ULONG;
    pElems: PSCODE;
	end;
  CASCODE = tagCASCODE;

  tagCAPROPVARIANT = record
    cElems: ULONG;
    pElems: PPROPVARIANT;
	end;
  CAPROPVARIANT = tagCAPROPVARIANT;

  tagCAH = record
    cElems: ULONG;
    pElems: PLARGE_INTEGER;
	end;
  CAH = tagCAH;

  tagCAUH = record
    cElems: ULONG;
    pElems: PULARGE_INTEGER;
	end;
  CAUH = tagCAUH;

  tagCALPSTR = record
    cElems: ULONG;
    pElems: PPAnsiChar;
	end;
  CALPSTR = tagCALPSTR;

	tagCALPWSTR = record
    cElems: ULONG;
    pElems: PPWideChar;
	end;
	CALPWSTR = tagCALPWSTR;

  tagCAFILETIME = record
    cElems: ULONG;
    pElems: PFILETIME;
	end;
	CAFILETIME = tagCAFILETIME;

  tagCACLIPDATA = record
    cElems: ULONG;
    pElems: PCLIPDATA;
	end;
  CACLIPDATA = tagCACLIPDATA;

  tagCACLSID = record
    cElems: ULONG;
    pElems: PCLSID;
	end;
  CACLSID = tagCACLSID;


	// This is the PROPVARIANT padding layout for marshaling.
	PROPVAR_PAD1 = Byte;
	PROPVAR_PAD2 = Byte;
  PROPVAR_PAD3 = ULONG;


	cwPROPVARIANT = packed record
		vt: TVarType;
		wReserved1: PROPVAR_PAD1;
		wReserved2: PROPVAR_PAD2;
		wReserved3: PROPVAR_PAD3;
		case WORD of
			0: (cVal: AnsiChar);
			1: (bVal: UCHAR);
			2: (iVal: SHORT);
			3: (uiVal: USHORT);
			4: (lVal: LONG);
			5: (ulVal: ULONG);
			6: (intVal: INT);
      7: (uintVal: UINT;);
      8: (hVal: LARGE_INTEGER);
			9: (uhVal: ULARGE_INTEGER);
     10: (fltVal: FLOAT);
     11: (dblVal: DOUBLE);
		 12: (boolVal: VARIANT_BOOL);
     13: (scode: SCODE);
     14: (cyVal: CY);
		 15: (date: DATE);
     16: (filetime: FILETIME);
     17: (puuid: PCLSID);
		 18: (pclipdata: PCLIPDATA);
     19: (bstrVal: BSTR);
     20: (bstrblobVal: BSTRBLOB);
		 21: (blob: BLOB);
     22: (pszVal: PAnsiChar);
     23: (pwszVal: PWideChar);
		 24: (punkVal: PIUnknown);
		 25: (pdispVal: ^IDispatch);
		 26: (pStream: ^IStream);
		 27: (pStorage: ^IStorage);
		 28: (pVersionedStream: LPVERSIONEDSTREAM);
		 29: (pArray: PSafeArray);
		 30: (cac: CAC);
		 31: (caub: CAUB);
     32: (cai: CAI);
		 33: (caui: CAUI);
     34: (cal: CAL);
     35: (caul: CAUL);
		 36: (cah: CAH);
     37: (cauh: CAUH);
     38: (caflt: CAFLT);
		 39: (cadbl: CADBL);
     40: (cabool: CABOOL);
     41: (cascode: CASCODE);
		 42: (cacy: CACY);
     43: (cadate: CADATE);
     44: (cafiletime: CAFILETIME);
		 45: (cauuid: CACLSID);
     46: (caclipdata: CACLIPDATA);
		 47: (cabstr: CABSTR);
		 48: (cabstrblob: CABSTRBLOB);
		 49: (calpstr: CALPSTR);
		 50: (calpwstr: CALPWSTR);
		 51: (capropvar: CAPROPVARIANT);
		 52: (pcVal: PAnsiChar);
     53: (pbVal: PUCHAR);
		 54: (piVal: PSHORT);
     55: (puiVal: PUSHORT);
     56: (plVal: PLONG);
		 57: (pulVal: PULONG);
     58: (pintVal: PINT);
     59: (puintVal: PUINT);
		 60: (pfltVal: PFLOAT);
     61: (pdblVal: PDOUBLE);
     62: (pboolVal: PVARIANT_BOOL);
		 63: (pdecVal: PDECIMAL);
     64: (pscode: PSCODE);
     65: (pcyVal: PCY);
		 66: (pdate: PDATE);
		 67: (pbstrVal: PBSTR);
		 68: (ppunkVal: PIUnknown);
		 69: (ppdispVal: ^IDispatch);
		 70: (pparray: ^PSafeArray);
     71: (pvarVal: PPROPVARIANT);
	end;
	PROPVARIANT = cwPROPVARIANT;

	// This is the LPPROPVARIANT definition for marshaling.
	LPPROPVARIANT = ^cwPROPVARIANT;
	REFPROPVARIANT = ^cwPROPVARIANT;

const
	// Reserved global Property IDs
	PID_DICTIONARY                      = 0;
	PID_CODEPAGE                        = $1;
	PID_FIRST_USABLE                    = $2;
	PID_FIRST_NAME_DEFAULT              = $FFF;
	PID_LOCALE                          = $80000000;
	PID_MODIFY_TIME                     = $80000001;
	PID_SECURITY                        = $80000002;
	PID_BEHAVIOR                        = $80000003;
	PID_ILLEGAL                         = $FFFFFFFF;

	// Range which is read-only to downlevel implementations
	PID_MIN_READONLY                    = $80000000;
	PID_MAX_READONLY                    = $BFFFFFFF;

	// Property IDs for the DiscardableInformation Property Set
  PIDDI_THUMBNAIL                     = $00000002;  // VT_BLOB

	// Property IDs for the SummaryInformation Property Set
	PIDSI_TITLE                         = $00000002;  // VT_LPSTR
	PIDSI_SUBJECT                       = $00000003;  // VT_LPSTR
	PIDSI_AUTHOR                        = $00000004;  // VT_LPSTR
	PIDSI_KEYWORDS                      = $00000005;  // VT_LPSTR
	PIDSI_COMMENTS                      = $00000006;  // VT_LPSTR
	PIDSI_TEMPLATE                      = $00000007;  // VT_LPSTR
	PIDSI_LASTAUTHOR                    = $00000008;  // VT_LPSTR
	PIDSI_REVNUMBER                     = $00000009;  // VT_LPSTR
	PIDSI_EDITTIME                      = $0000000A;  // VT_FILETIME (UTC)
	PIDSI_LASTPRINTED                   = $0000000B;  // VT_FILETIME (UTC)
	PIDSI_CREATE_DTM                    = $0000000C;  // VT_FILETIME (UTC)
	PIDSI_LASTSAVE_DTM                  = $0000000D;  // VT_FILETIME (UTC)
	PIDSI_PAGECOUNT                     = $0000000E;  // VT_I4
	PIDSI_WORDCOUNT                     = $0000000F;  // VT_I4
	PIDSI_CHARCOUNT                     = $00000010;  // VT_I4
	PIDSI_THUMBNAIL                     = $00000011;  // VT_CF
	PIDSI_APPNAME                       = $00000012;  // VT_LPSTR
  PIDSI_DOC_SECURITY                  = $00000013;  // VT_I4

  // Property IDs for the DocSummaryInformation Property Set


	PIDDSI_CATEGORY                     = $00000002;  // VT_LPSTR
	PIDDSI_PRESFORMAT                   = $00000003;  // VT_LPSTR
	PIDDSI_BYTECOUNT                    = $00000004;  // VT_I4
	PIDDSI_LINECOUNT                    = $00000005;  // VT_I4
	PIDDSI_PARCOUNT                     = $00000006;  // VT_I4
	PIDDSI_SLIDECOUNT                   = $00000007;  // VT_I4
	PIDDSI_NOTECOUNT                    = $00000008;  // VT_I4
	PIDDSI_HIDDENCOUNT                  = $00000009;  // VT_I4
	PIDDSI_MMCLIPCOUNT                  = $0000000A;  // VT_I4
	PIDDSI_SCALE                        = $0000000B;  // VT_BOOL
	PIDDSI_HEADINGPAIR                  = $0000000C;  // VT_VARIANT | VT_VECTOR
	PIDDSI_DOCPARTS                     = $0000000D;  // VT_LPSTR | VT_VECTOR
	PIDDSI_MANAGER                      = $0000000E;  // VT_LPSTR
	PIDDSI_COMPANY                      = $0000000F;  // VT_LPSTR
  PIDDSI_LINKSDIRTY                   = $00000010;  // VT_BOOL


	// FMTID_MediaFileSummaryInfo - Property IDs
	PIDMSI_EDITOR                       = $00000002;  // VT_LPWSTR
	PIDMSI_SUPPLIER                     = $00000003;  // VT_LPWSTR
	PIDMSI_SOURCE                       = $00000004;  // VT_LPWSTR
	PIDMSI_SEQUENCE_NO                  = $00000005;  // VT_LPWSTR
	PIDMSI_PROJECT                      = $00000006;  // VT_LPWSTR
	PIDMSI_STATUS                       = $00000007;  // VT_UI4
	PIDMSI_OWNER                        = $00000008;  // VT_LPWSTR
	PIDMSI_RATING                       = $00000009;  // VT_LPWSTR
	PIDMSI_PRODUCTION                   = $0000000A;  // VT_FILETIME (UTC)
	PIDMSI_COPYRIGHT                    = $0000000B;  // VT_LPWSTR


type
	//  PIDMSI_STATUS value definitions
	PIDMSI_STATUS_VALUE                 = (
  PIDMSI_STATUS_NORMAL                = 0,
	PIDMSI_STATUS_NEW	                  = 1,
	PIDMSI_STATUS_PRELIM	              = 2,
	PIDMSI_STATUS_DRAFT	                = 3,
	PIDMSI_STATUS_INPROGRESS	          = 4,
	PIDMSI_STATUS_EDIT	                = 4,
	PIDMSI_STATUS_REVIEW	              = 5,
	PIDMSI_STATUS_PROOF	                = 6,
	PIDMSI_STATUS_FINAL	                = 7,
	PIDMSI_STATUS_OTHER	                = $7fff
  );

const
	PRSPEC_INVALID                      = $FFFFFFFF;
	PRSPEC_LPWSTR                       = 0;
  PRSPEC_PROPID                       = 1;


{$IFNDEF PROPSPEC}
type
	tagPROPSPEC = packed record
    ulKind: ULONG;
    case Integer of
			0: (propid: PropID);
			1: (lpwstr: POleStr);
	end;
	PROPSPEC = tagPROPSPEC;
	PPropSpec = ^TPropSpec;
{$ENDIF}


type
	tagSTATPROPSTG = record
		lpwstrName: POLESTR;
    propid: PROPID;
		vt: VARTYPE;
	end;
  STATPROPSTG = tagSTATPROPSTG;


	// Macros for parsing the OS Version of the Property Set Header
	function PROPSETHDR_OSVER_KIND(dwOSVer: DWORD): DWORD;
	function PROPSETHDR_OSVER_MAJOR(dwOSVer: DWORD): DWORD;
	function PROPSETHDR_OSVER_MINOR(dwOSVer: DWORD): DWORD;


const
	PROPSETHDR_OSVERSION_UNKNOWN        = $FFFFFFFF;

type
  tagSTATPROPSETSTG = record
		fmtid: FMTID;
    clsid: CLSID;
    grfFlags: DWORD;
    mtime: FILETIME;
		ctime: FILETIME;
    atime: FILETIME;
    dwOSVersion: DWORD;
	end;
  STATPROPSETSTG = tagSTATPROPSETSTG;


type
  PSerializedpropertyvalue = ^TSerializedpropertyvalue;
  {$EXTERNALSYM tagSERIALIZEDPROPERTYVALUE}
  tagSERIALIZEDPROPERTYVALUE = record
    dwType: DWORD;
    rgb: array[0..0] of Byte;
  end;
  {$EXTERNALSYM SERIALIZEDPROPERTYVALUE}
  SERIALIZEDPROPERTYVALUE = tagSERIALIZEDPROPERTYVALUE;
	TSerializedpropertyvalue = tagSERIALIZEDPROPERTYVALUE;



type
	// Forward Interfaces Declarations ///////////////////////////////////////////

	PIPropertyStorage = ^IPropertyStorage;
	IPropertyStorage = interface;

	PIPropertySetStorage = ^IPropertySetStorage;
  IPropertySetStorage = interface;

	PIEnumSTATPROPSTG = ^IEnumSTATPROPSTG;
  IEnumSTATPROPSTG = interface;

	PIEnumSTATPROPSETSTG = ^IEnumSTATPROPSETSTG;
	IEnumSTATPROPSETSTG = interface;

  // Interfaces ////////////////////////////////////////////////////////////////

	//Interface IPropertyStorage
  IPropertyStorage = interface(IUnknown)
  ['{00000138-0000-0000-C000-000000000046}']
    function ReadMultiple(const cpspec: ULONG; const rgpspec: PROPSPEC; out rgpropvar: PROPVARIANT): HResult; stdcall;
    function WriteMultiple(const cpspec: ULONG; const rgpspec: PROPSPEC; const rgpropvar: PROPVARIANT; const propidNameFirst: PROPID): HResult; stdcall;
    function DeleteMultiple(const cpspec: ULONG; const rgpspec: PROPSPEC): HResult; stdcall;
		function ReadPropertyNames(const cpropid: ULONG; const rgpropid: PROPID; out rglpwstrName: POLESTR): HResult; stdcall;
		function WritePropertyNames(const cpropid: ULONG; const rgpropid: PROPID; const rglpwstrName: POLESTR): HResult; stdcall;
    function DeletePropertyNames(const cpropid: ULONG; const rgpropid: PROPID): HResult; stdcall;
    function Commit(const grfCommitFlags: DWORD): HResult; stdcall;
    function Revert(): HResult; stdcall;
    function Enum(out ppenum: PIEnumSTATPROPSTG): HResult; stdcall;
    function SetTimes(const pctime: FILETIME; const patime: FILETIME; const pmtime: FILETIME): HResult; stdcall;
    function SetClass(const clsid: REFCLSID): HResult; stdcall;
    function Stat(out pstatpsstg: STATPROPSETSTG): HResult; stdcall;
  end;


  //Interface IPropertySetStorage
  IPropertySetStorage = interface(IUnknown)
  ['{0000013A-0000-0000-C000-000000000046}']
		function Create(const rfmtid: REFFMTID; const pclsid: CLSID; const grfFlags: DWORD; const grfMode: DWORD; out ppprstg: PIPropertyStorage): HResult; stdcall;
		function Open(const rfmtid: REFFMTID; const grfMode: DWORD; var ppprstg: PIPropertyStorage): HResult; stdcall;
    function Delete(const rfmtid: REFFMTID): HResult; stdcall;
		function Enum(out ppenum: PIEnumSTATPROPSETSTG): HResult; stdcall;
  end;


  //Interface IEnumSTATPROPSTG
  IEnumSTATPROPSTG = interface(IUnknown)
  ['{00000139-0000-0000-C000-000000000046}']
    function Next(const celt: ULONG; const rgelt: STATPROPSTG; out pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset(): HResult; stdcall;
    function Clone(out ppenum: PIEnumSTATPROPSTG): HResult; stdcall;
  end;


  //Interface IEnumSTATPROPSETSTG
  IEnumSTATPROPSETSTG = interface(IUnknown)
  ['{00000139-0000-0000-C000-000000000046}']
    function Next(const celt: ULONG; out rgelt: STATPROPSETSTG; out pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset(): HResult; stdcall;
    function Clone(out ppenum: PIEnumSTATPROPSETSTG): HResult; stdcall;
  end;



  // functions /////////////////////////////////////////////////////////////////
	// TODO: implementation, research if needed by MF or AC
	//////////////////////////////////////////////////////////////////////////////
	//function PropVariantCopy(var pvarDest: PROPVARIANT; var pvarSrc: PROPVARIANT): HResult; stdcall;
	//function PropVariantClear(var pvar: PROPVARIANT): HResult; stdcall;
	//function FreePropVariantArray(const cVariants: ULONG; var rgvars: PROPVARIANT): HResult; stdcall;
	//procedure PropVariantInit(var pv: TPropVariant);  >> See: PropSys.pas
	//function StgCreatePropStg(const pUnk: IUnknown; const fmtid: REFFMTID; const pclsid: CLSID;
	//													const grfFlags: DWORD; const dwReserved: DWORD; out ppPropStg: PIPropertyStorage): HResult; stdcall;
	//function StgOpenPropStg(const pUnk: IUnknown; const fmtid: REFFMTID; const grfFlags: DWORD;
	//												const dwReserved: DWORD; out ppPropStg: PIPropertyStorage): HResult; stdcall;
	//function StgCreatePropSetStg(const pStorage: IStorage; const dwReserved: DWORD; out ppPropSetStg: PIPropertySetStorage): HResult; stdcall;
	//function FmtIdToPropStgName(const pfmtid: REFFMTID; out oszName: POLESTR): HResult; stdcall;
	//function PropStgNameToFmtId(const oszName: POLESTR; out pfmtid: REFFMTID): HResult; stdcall;
	//function StgConvertVariantToProperty(const pvar: PPROPVARIANT; const CodePage: USHORT; out pprop: PSERIALIZEDPROPERTYVALUE; var pcb: PULONG;
	//																		 const pid: PROPID; const __reserved: BOOLEAN; var pcIndirect: PULONG): PSERIALIZEDPROPERTYVALUE; stdcall;
	//function StgConvertPropertyToVariant(const prop: SERIALIZEDPROPERTYVALUE; const CodePage: USHORT; out pvar: PROPVARIANT; const pma: PMemoryAllocator): BOOLEAN; stdcall;



implementation


	// Macros for parsing the OS Version of the Property Set Header
	function PROPSETHDR_OSVER_KIND(dwOSVer: DWORD): DWORD;
	begin
		Result:= HIWORD(dwOSVer);
	end;


  function PROPSETHDR_OSVER_MAJOR(dwOSVer: DWORD): DWORD;
  begin
    Result:= LOBYTE(LOWORD(dwOSVer));
	end;

  function PROPSETHDR_OSVER_MINOR(dwOSVer: DWORD): DWORD;
  begin
		Result:= HIBYTE(LOWORD(dwOSVer));
	end;

	//See: PropSys.pas
	//procedure PropVariantInit(var pv: TPropVariant);
	//begin
	//	FillChar(pv, sizeof(TPropVariant), 0);
	//end;  

end.
