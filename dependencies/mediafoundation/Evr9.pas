// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - Evr9.pas
// Kind: Pascal Unit
// Release date: 07-07-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
// LastEdited by: Tony
// EditDate: updt 070712b
//
// Remarks: The enhanced video renderer (EVR9) is an extension of Evr.
//
//
//          When reading the original headers (.h) you may see "STDAPI", a macro.
//          "STDAPI" means it uses the "stdcall" calling convention and it returns always a HRESULT,
//          unless it's marked with, for example _BOOL, a boolean is returned.
//          In Delphi it's declared as:
//          [uses Windows;]
//          [function FunctionName(vars: -const, out or var-): HResult; stdcall;]
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//                   IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: Overall Check
// =============================================================================
// Source: evr9.h
//
// Microsoft Windows Media Foundation
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

unit Evr9;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}

interface

uses
  Windows, ComObj, Unknwn, MFObjects, MFTransform, Evr, DXVa2Api;

type
  RECT                  = tRect;
  float                 = Single;
  PPTGUID               = tGUID;
  LPGUID                = tGUID;

const
  IID_IEVRVideoStreamControl           : TGUID = '{d0cfe38b-93e7-4772-8957-0400c49a4485}';
  IID_IMFVideoProcessor                : TGUID = '{6AB0000C-FECE-4d1f-A2AC-A9573530656E}';
  IID_IMFVideoMixerBitmap              : TGUID = '{814C7B20-0FDB-4eec-AF8F-F957C8F69EDC}';



type
  PMFVideoAlphaBitmapParams = ^TMFVideoAlphaBitmapParams;
  {$EXTERNALSYM MFVideoAlphaBitmapParams}
  MFVideoAlphaBitmapParams = record
    dwFlags: DWORD;
    clrSrcKey: COLORREF;
    rcSrc: RECT;
    nrcDest: MFVideoNormalizedRect;
    fAlpha: FLOAT;
    dwFilterMode: DWORD;
  end;
  TMFVideoAlphaBitmapParams = MFVideoAlphaBitmapParams;

type
  tMFPackSource = Record
    hdc:                HDC;
    pDDS:               IDirect3DSurface9;
  End;

  PMFVideoAlphaBitmap = ^TMFVideoAlphaBitmap;
  {$EXTERNALSYM MFVideoAlphaBitmap}
  MFVideoAlphaBitmap = record
    GetBitmapFromDC: BOOL;
    Source:          tMFPackSource;
//    bitmap: union
//      hdc: HDC;
//      pDDS: PIDirect3DSurface9;
//    end;
    params: MFVideoAlphaBitmapParams;
  end;
  TMFVideoAlphaBitmap = MFVideoAlphaBitmap;

type
  PMFVideoAlphaBitmapFlags = ^TMFVideoAlphaBitmapFlags;
  {$EXTERNALSYM __MIDL___MIDL_itf_evr9_0000_0002_0002}
  __MIDL___MIDL_itf_evr9_0000_0002_0002 = (
    MFVideoAlphaBitmap_EntireDDS   = $1,
    MFVideoAlphaBitmap_SrcColorKey = $2,
    MFVideoAlphaBitmap_SrcRect     = $4,
    MFVideoAlphaBitmap_DestRect    = $8,
    MFVideoAlphaBitmap_FilterMode  = $10,
    MFVideoAlphaBitmap_Alpha       = $20,
    MFVideoAlphaBitmap_BitMask     = $3F
  );
  {$EXTERNALSYM MFVideoAlphaBitmapFlags}
  MFVideoAlphaBitmapFlags = __MIDL___MIDL_itf_evr9_0000_0002_0002;
  TMFVideoAlphaBitmapFlags = __MIDL___MIDL_itf_evr9_0000_0002_0002;


type
  // Forward Declarations

  PIEVRVideoStreamControl = ^TIEVRVideoStreamControl;
  {$EXTERNALSYM IEVRVideoStreamControl}
  IEVRVideoStreamControl = interface;
  TIEVRVideoStreamControl = interface;

  PIMFVideoProcessor = ^TIMFVideoProcessor;
  {$EXTERNALSYM IMFVideoProcessor}
  IMFVideoProcessor = interface;
  TIMFVideoProcessor = interface;

  PIMFVideoMixerBitmap = ^TIMFVideoMixerBitmap;
  {$EXTERNALSYM IMFVideoMixerBitmap}
  IMFVideoMixerBitmap = interface;
  TIMFVideoMixerBitmap = interface;


  // INTERFACES ////////////////////////////////////////////////////////////////

  //Interface IEVRVideoStreamControl
  IEVRVideoStreamControl = interface(IUnknown)
  ['{d0cfe38b-93e7-4772-8957-0400c49a4485}']
    function SetStreamActiveState(const fActive: BOOL): HResult; stdcall;
    function GetStreamActiveState(out lpfActive: BOOL): HResult; stdcall;
  end;


  //Interface IMFVideoProcessor
  IMFVideoProcessor = interface(IUnknown)
  ['{6AB0000C-FECE-4d1f-A2AC-A9573530656E}']
    function GetAvailableVideoProcessorModes(var lpdwNumProcessingModes: PUINT; out ppVideoProcessingModes: PPTGUID): HResult; stdcall;
    function GetVideoProcessorCaps(const lpVideoProcessorMode: LPGUID; out lpVideoProcessorCaps: PDXVA2_VideoProcessorCaps): HResult; stdcall;
    function GetVideoProcessorMode(out lpMode: LPGUID): HResult; stdcall;
    function SetVideoProcessorMode(const lpMode: LPGUID): HResult; stdcall;
    function GetProcAmpRange(const dwProperty: DWORD; out pPropRange: DXVA2_ValueRange): HResult; stdcall;
    function GetProcAmpValues(const dwFlags: DWORD; out Values: DXVA2_ProcAmpValues): HResult; stdcall;
    function SetProcAmpValues(const pValues: PDXVA2_ProcAmpValues): HResult; stdcall;
    function GetFilteringRange(const dwProperty: DWORD; out pPropRange: DXVA2_ValueRange): HResult; stdcall;
    function GetFilteringValue(const dwProperty: DWORD; out pValue: DXVA2_Fixed32): HResult; stdcall;
    function SetFilteringValue(const pValue: PDXVA2_Fixed32): HResult; stdcall;
    function GetBackgroundColor(out lpClrBkg: COLORREF): HResult; stdcall;
    function SetBackgroundColor(const ClrBkg: COLORREF): HResult; stdcall;
  end;


  //Interface IMFVideoMixerBitmap
  IMFVideoMixerBitmap = interface(IUnknown)
  ['{814C7B20-0FDB-4eec-AF8F-F957C8F69EDC}']
    function SetAlphaBitmap(var pBmpParms: MFVideoAlphaBitmap): HResult; stdcall;
    function ClearAlphaBitmap(): HResult; stdcall;
    function UpdateAlphaBitmapParameters(var pBmpParms: MFVideoAlphaBitmapParams): HResult; stdcall;
    function GetAlphaBitmapParameters(out pBmpParms: MFVideoAlphaBitmapParams): HResult; stdcall;
  end;


implementation

end.
