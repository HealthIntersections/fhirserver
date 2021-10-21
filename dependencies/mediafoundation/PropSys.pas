// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: PropSys.pas
// Kind: Pascal Unit
// Release date: 27-06-2012
// Language: ENU
//
// Version: 1.0.0.2
// Description: Requires Windows Vista or later. 
// 
// Intiator(s): Tony (maXcomX), Peter (OzShips) 
// 
// LastEdited by: Tony (maXcomX)
// EditDate: updt 040512a
//
// Remarks:
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: -
// =============================================================================
// Source: propsys.h
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
unit PropSys;

  //{$MINENUMSIZE 4}
  //{$WEAKPACKAGEUNIT}

interface

uses
	Windows, ActiveX, ComObj, shlobj;

type
	 {$EXTERNALSYM _tagPROPERTYKEY}
		_tagPROPERTYKEY = packed record
		 fmtid: TGuid;
		 pid: DWord;
	 end;
	 {$EXTERNALSYM PROPERTYKEY}
	 PROPERTYKEY = _tagpropertykey;
	 PPROPERTYKEY = ^PROPERTYKEY;


	 IInitializeWithFile = interface(IUnknown)
   ['{b7d14566-0509-4cce-a71f-0a554233bd9b}']
		 function Initialize(pszFilePath: PAnsiChar; grfMode: DWord): HRESULT; stdcall;
   end;


	 IInitializeWithStream = interface(IUnknown)
   ['{b824b49d-22ac-4161-ac8a-9916e8fa3f7f}']
		 function Initialize(var pIStream: IStream; grfMode: DWord): HRESULT; stdcall;
	 end;


	 IPropertyStore = interface(IUnknown)
     ['{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}']
     function GetCount(out cProps: DWORD): HResult; stdcall;
		 function GetAt(iProp: DWORD; out pkey: PPROPERTYKEY): HResult; stdcall;
		 function GetValue(const key: PROPERTYKEY; out pv: TPropVariant): HResult; stdcall;
		 function SetValue(const key: PROPERTYKEY; const propvar: TPropVariant): HResult; stdcall;
     function Commit: HResult; stdcall;
	 end;


	 IPropertyStoreCapabilities = interface(IUnknown)
   ['{c8e2d566-186e-4d49-bf41-6909ead56acc}']
		 function IsPropertyWritable(pPropKey: PPROPERTYKEY): HRESULT; stdcall;
	 end;


	//Forward functions
	procedure PropVariantInit(var pv: TPropVariant);
	procedure PropVariantClear(var pv: TPropVariant);


implementation


procedure PropVariantInit(var pv: TPropVariant);
begin
	FillChar(pv, sizeof(TPropVariant), 0);
end;

procedure PropVariantClear(var pv: TPropVariant);
begin
	ZeroMemory(@pv, SizeOf(pv));
end;

end.
