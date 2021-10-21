// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - Evr.pas
// Kind: Pascal Unit
// Release date: 07-07-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
// 
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
//
// LastEdited by: Tony
// EditDate: updp 080712b, updt 290712b
//----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person               Reason
// ---------- -------------------- --------------------------------------------
// 2012/08/28 Peter Larson         Remove txxxxx interface definitions, renamed
//                                 duplicate interface definitions
// 2012/12/19 Tony  (maXcomX)      modified some interfaces (Reported by eric.c.fortier, Oct 5, 2012)
//----------------------------------------------------------------------------
//
// Remarks: The enhanced video renderer (EVR) is a component that displays video on the user's monitor.
//          Two versions of the EVR exist:
//           - The EVR media sink, for Media Foundation applications.
//           - The EVR filter, for DirectShow applications.
//
//          Both versions use the same internal objects to render video, and they share many of the same interfaces.
//
//          When reading the original headers (.h) you may see "STDAPI", a macro.
//          "STDAPI" means it uses the "stdcall" calling convention and it returns always a HRESULT,
//          unless it's marked with, for example _BOOL, a boolean is returned.
//          In Delphi it's declared as:
//          [uses Windows;]
//          [function FunctionName(vars: -const, out or var-): HRESULT; stdcall;]
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
// Source: evr.h
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

unit Evr;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}

interface

uses
  Windows, ActiveX, MfObjects, MFTransform;
//   ComObj, Unknwn, PropIdl, MfIdl, StrmIf, DirectShow9;                //updt 290712b

Const
  Version               = '0.1.0001';

type
  LPRECT =              tRect;
  IID =                 tGUID;
  PHWND =               HWND;
  PPByte =              ^Byte;
  PLONGLONG =           ^LONGLONG;

const
  IID_IMFVideoPositionMapper           : TGUID = '{1F6A9F17-E70B-4e24-8AE4-0B2C3BA7A4AE}';
  IID_IMFVideoDeviceID                 : TGUID = '{A38D9567-5A9C-4f3c-B293-8EB415B279BA}';
  IID_IMFVideoDisplayControl           : TGUID = '{a490b1e4-ab84-4d31-a1b2-181e03b1077a}';
  IID_IMFVideoPresenter                : TGUID = '{29AFF080-182A-4a5d-AF3B-448F3A6346CB}';
  IID_IMFDesiredSample                 : TGUID = '{56C294D0-753E-4260-8D61-A3D8820B1D54}';
  IID_IMFTrackedSample                 : TGUID = '{245BF8E9-0755-40f7-88A5-AE0F18D55E17}';
  IID_IMFVideoMixerControl             : TGUID = '{A5C6C53F-C202-4aa5-9695-175BA8C508A5}';
  IID_IMFVideoMixerControl2            : TGUID = '{8459616d-966e-4930-b658-54fa7e5a16d3}';
  IID_IMFVideoRenderer                 : TGUID = '{DFDFD197-A9CA-43d8-B341-6AF3503792CD}';
  IID_IEVRFilterConfig                 : TGUID = '{83E91E85-82C1-4ea7-801D-85DC50B75086}';
  IID_IEVRFilterConfigEx               : TGUID = '{aea36028-796d-454f-beee-b48071e24304}';
  IID_IMFTopologyServiceLookup         : TGUID = '{fa993889-4383-415a-a930-dd472a8cf6f7}';
  IID_IMFTopologyServiceLookupClient   : TGUID = '{fa99388a-4383-415a-a930-dd472a8cf6f7}';
  IID_IEVRTrustedVideoPlugin           : TGUID = '{83A4CE40-7710-494b-A893-A472049AF630}';

type                                                                            //updt 290712b
  PD3dformat = ^TD3dformat;
  {$EXTERNALSYM _D3DFORMAT}
  _D3DFORMAT            = (
    D3DFMT_UNKNOWN      = 0,
    D3DFMT_R8G8B8       = 20,
    D3DFMT_A8R8G8B8     = 21,
    D3DFMT_X8R8G8B8     = 22,
    D3DFMT_R5G6B5       = 23,
    D3DFMT_X1R5G5B5     = 24,
    D3DFMT_A1R5G5B5     = 25,
    D3DFMT_A4R4G4B4     = 26,
    D3DFMT_R3G3B2       = 27,
    D3DFMT_A8           = 28,
    D3DFMT_A8R3G3B2     = 29,
    D3DFMT_X4R4G4B4     = 30,
    D3DFMT_A2B10G10R10  = 31,
    D3DFMT_G16R16       = 34,
    D3DFMT_A8P8         = 40,
    D3DFMT_P8           = 41,
    D3DFMT_L8           = 50,
    D3DFMT_A8L8         = 51,
    D3DFMT_A4L4         = 52,
    D3DFMT_V8U8         = 60,
    D3DFMT_L6V5U5       = 61,
    D3DFMT_X8L8V8U8     = 62,
    D3DFMT_Q8W8V8U8     = 63,
    D3DFMT_V16U16       = 64,
    D3DFMT_W11V11U10    = 65,
    D3DFMT_A2W10V10U10  = 67,
    D3DFMT_D16_LOCKABLE = 70,
    D3DFMT_D32          = 71,
    D3DFMT_D15S1        = 73,
    D3DFMT_D24S8        = 75,
    D3DFMT_D16          = 80,
    D3DFMT_D24X8        = 77,
    D3DFMT_D24X4S4      = 79,
    D3DFMT_VERTEXDATA   = 100,
    D3DFMT_INDEX16      = 101,
    D3DFMT_INDEX32      = 102,
    D3DFMT_FORCE_DWORD  = $7FFFFFFF
  );
  {$EXTERNALSYM D3DFORMAT}
  D3DFORMAT = _D3DFORMAT;
  TD3dformat = _D3DFORMAT;

const                                                                           //updt 290712b
  MR_VIDEO_RENDER_SERVICE           : TGuid =  '{1092a86c-ab1a-459a-a336-831fbc4d11ff}';
  MR_VIDEO_MIXER_SERVICE            : TGuid =  '{073cd2fc-6cf4-40b7-8859-e89552c841f8}';
  MR_VIDEO_ACCELERATION_SERVICE     : TGuid =  '{efef5175-5c7d-4ce2-bbbd-34ff8bca6554}';
  MR_BUFFER_SERVICE                 : TGuid =  '{a562248c-9ac6-4ffc-9fba-3af8f8ad1a4d}';
  VIDEO_ZOOM_RECT                   : TGuid =  '{7aaa1638-1b7f-4c93-bd89-5b9c9fb6fcf0}';


type
  PMFVideoAspectRatioMode = ^TMFVideoAspectRatioMode;
  {$EXTERNALSYM MFVideoAspectRatioMode}
  MFVideoAspectRatioMode           = (
    MFVideoARMode_None             = 0,
    MFVideoARMode_PreservePicture  = $1,
    MFVideoARMode_PreservePixel    = $2,
    MFVideoARMode_NonLinearStretch = $4,
    MFVideoARMode_Mask             = $7
  );
  TMFVideoAspectRatioMode = MFVideoAspectRatioMode;

type
  PMFVideoRenderPrefs = ^TMFVideoRenderPrefs;
  {$EXTERNALSYM MFVideoRenderPrefs}
  MFVideoRenderPrefs                         = (
    MFVideoRenderPrefs_DoNotRenderBorder     = $1,
    MFVideoRenderPrefs_DoNotClipToDevice     = $2,
    MFVideoRenderPrefs_AllowOutputThrottling = $4,
    MFVideoRenderPrefs_ForceOutputThrottling = $8,
    MFVideoRenderPrefs_ForceBatching         = $10,
    MFVideoRenderPrefs_AllowBatching         = $20,
    MFVideoRenderPrefs_ForceScaling          = $40,
    MFVideoRenderPrefs_AllowScaling          = $80,
    MFVideoRenderPrefs_DoNotRepaintOnStop    = $100,
    MFVideoRenderPrefs_Mask                  = $1FF
  );
  TMFVideoRenderPrefs = MFVideoRenderPrefs;

type
  PMFVideoNormalizedRect = ^TMFVideoNormalizedRect;
  {$EXTERNALSYM MFVideoNormalizedRect}
  MFVideoNormalizedRect = record
    left: Single;
    top: Single;
    right: Single;
    bottom: Single;
  end;
  TMFVideoNormalizedRect = MFVideoNormalizedRect;

type
  PMfvpMessageType = ^TMfvpMessageType;
  {$EXTERNALSYM MFVP_MESSAGE_TYPE}
  MFVP_MESSAGE_TYPE                  = (
    MFVP_MESSAGE_FLUSH               = 0,
    MFVP_MESSAGE_INVALIDATEMEDIATYPE = $1,
    MFVP_MESSAGE_PROCESSINPUTNOTIFY  = $2,
    MFVP_MESSAGE_BEGINSTREAMING      = $3,
    MFVP_MESSAGE_ENDSTREAMING        = $4,
    MFVP_MESSAGE_ENDOFSTREAM         = $5,
    MFVP_MESSAGE_STEP                = $6,
    MFVP_MESSAGE_CANCELSTEP          = $7
  );
  TMfvpMessageType = MFVP_MESSAGE_TYPE;

type
  PMFVideoMixPrefs = ^TMFVideoMixPrefs;
  {$EXTERNALSYM _MFVideoMixPrefs}
  _MFVideoMixPrefs                           = (
    MFVideoMixPrefs_ForceHalfInterlace       = $1,
    MFVideoMixPrefs_AllowDropToHalfInterlace = $2,
    MFVideoMixPrefs_AllowDropToBob           = $4,
    MFVideoMixPrefs_ForceBob                 = $8,
    MFVideoMixPrefs_Mask                     = $F
  );
  {$EXTERNALSYM MFVideoMixPrefs}
  MFVideoMixPrefs = _MFVideoMixPrefs;
  TMFVideoMixPrefs = _MFVideoMixPrefs;

type
  PEVRFilterConfigPrefs = ^TEVRFilterConfigPrefs;
  {$EXTERNALSYM _EVRFilterConfig_Prefs}
  _EVRFilterConfig_Prefs           = (
    EVRFilterConfigPrefs_EnableQoS = $1,
    EVRFilterConfigPrefs_Mask      = $1
  );
  {$EXTERNALSYM EVRFilterConfigPrefs}
  EVRFilterConfigPrefs = _EVRFilterConfig_Prefs;
  TEVRFilterConfigPrefs = _EVRFilterConfig_Prefs;

type
  PMfServiceLookupType = ^TMfServiceLookupType;
  {$EXTERNALSYM _MF_SERVICE_LOOKUP_TYPE}
  _MF_SERVICE_LOOKUP_TYPE               = (
    MF_SERVICE_LOOKUP_UPSTREAM          = 0,
    MF_SERVICE_LOOKUP_UPSTREAM_DIRECT   = (MF_SERVICE_LOOKUP_UPSTREAM  + 1),
    MF_SERVICE_LOOKUP_DOWNSTREAM        = (MF_SERVICE_LOOKUP_UPSTREAM_DIRECT  + 1),
    MF_SERVICE_LOOKUP_DOWNSTREAM_DIRECT = (MF_SERVICE_LOOKUP_DOWNSTREAM  + 1),
    MF_SERVICE_LOOKUP_ALL               = (MF_SERVICE_LOOKUP_DOWNSTREAM_DIRECT  + 1),
    MF_SERVICE_LOOKUP_GLOBAL            = (MF_SERVICE_LOOKUP_ALL  + 1)
  );
  {$EXTERNALSYM MF_SERVICE_LOOKUP_TYPE}
  MF_SERVICE_LOOKUP_TYPE = _MF_SERVICE_LOOKUP_TYPE;
  TMfServiceLookupType = _MF_SERVICE_LOOKUP_TYPE;

type
  // Forward Interfaces Declarations

//  PIMFVideoPositionMapper = ^TIMFVideoPositionMapper;
  {$EXTERNALSYM IMFVideoPositionMapper}
  IMFVideoPositionMapper = interface;
//  TIMFVideoPositionMapper = interface;

//  PIMFVideoDeviceID = ^TIMFVideoDeviceID;
  {$EXTERNALSYM IMFVideoDeviceID}
  IMFVideoDeviceID = interface;
//  TIMFVideoDeviceID = interface;

//  PIMFVideoDisplayControl = ^TIMFVideoDisplayControl;
  {$EXTERNALSYM IMFVideoDisplayControl}
  IMFVideoDisplayControl = interface;
//  TIMFVideoDisplayControl = interface;

//  PIMFVideoPresenter = ^TIMFVideoPresenter;
  {$EXTERNALSYM IMFVideoPresenter}
  IMFVideoPresenter = interface;
//  TIMFVideoPresenter = interface;

//  PIMFDesiredSample = ^TIMFDesiredSample;
  {$EXTERNALSYM IMFDesiredSample}
  IMFDesiredSample = interface;
//  TIMFDesiredSample = interface;

//  PIMFTrackedSample = ^TIMFTrackedSample;
  {$EXTERNALSYM IMFTrackedSample}
  IMFTrackedSample = interface;
//  TIMFTrackedSample = interface;

//  PIMFVideoMixerControl = ^TIMFVideoMixerControl;
  {$EXTERNALSYM IMFVideoMixerControl}
  IMFVideoMixerControl = interface;
//  TIMFVideoMixerControl = interface;

//  PIMFVideoMixerControl2 = ^TIMFVideoMixerControl2;
  {$EXTERNALSYM IMFVideoMixerControl2}
  IMFVideoMixerControl2 = interface;
//  TIMFVideoMixerControl2 = interface;

//  PIMFVideoRenderer = ^TIMFVideoRenderer;
  {$EXTERNALSYM IMFVideoRenderer}
  IMFVideoRenderer = interface;
//  TIMFVideoRenderer = interface;

//  PIEVRFilterConfig = ^TIEVRFilterConfig;
  {$EXTERNALSYM IEVRFilterConfig}
  IEVRFilterConfig = interface;
//  TIEVRFilterConfig = interface;

//  PIEVRFilterConfigEx = ^TIEVRFilterConfigEx;
  {$EXTERNALSYM IEVRFilterConfigEx}
  IEVRFilterConfigEx = interface;
//  TIEVRFilterConfigEx = interface;

//  PIMFTopologyServiceLookup = ^TIMFTopologyServiceLookup;
  {$EXTERNALSYM IMFTopologyServiceLookup}
  IMFTopologyServiceLookup = interface;
//  TIMFTopologyServiceLookup = interface;

//  PIMFTopologyServiceLookupClient = ^TIMFTopologyServiceLookupClient;
  {$EXTERNALSYM IMFTopologyServiceLookupClient}
  IMFTopologyServiceLookupClient = interface;
//  TIMFTopologyServiceLookupClient = interface;

//  PIEVRTrustedVideoPlugin = ^TIEVRTrustedVideoPlugin;
  {$EXTERNALSYM IEVRTrustedVideoPlugin}
  IEVRTrustedVideoPlugin = interface;
//  TIEVRTrustedVideoPlugin = interface;

  // INTERFACES  ///////////////////////////////////////////////////////////////

  //Interface IMFVideoPositionMapper
  IMFVideoPositionMapper = interface(IUnknown)
  ['{1F6A9F17-E70B-4e24-8AE4-0B2C3BA7A4AE}']
    function MapOutputCoordinateToInputStream(const xOut: Single; const yOut: Single; const dwOutputStreamIndex: DWord;
                                              const dwInputStreamIndex: DWord; out pxIn: Single; out pyIn: Single): HRESULT; stdcall;
  end;

  //Interface IMFVideoDeviceID
  IMFVideoDeviceID = interface(IUnknown)
  ['{A38D9567-5A9C-4f3c-B293-8EB415B279BA}']
    function GetDeviceID(out pDeviceID: IID): HRESULT; stdcall;
  end;

  //Interface IMFVideoDisplayControl
  IMFVideoDisplayControl = interface(IUnknown)
  ['{a490b1e4-ab84-4d31-a1b2-181e03b1077a}']
    function GetNativeVideoSize(var pszVideo: SIZE; var pszARVideo: SIZE): HRESULT; stdcall;
    function GetIdealVideoSize(var pszMin: SIZE; var pszMax: SIZE): HRESULT; stdcall;
    function SetVideoPosition(const pnrcSource: PMFVideoNormalizedRect; const prcDest: LPRECT): HRESULT; stdcall;
    function GetVideoPosition(out pnrcSource: MFVideoNormalizedRect; out prcDest: LPRECT): HRESULT; stdcall;
    function SetAspectRatioMode(const dwAspectRatioMode: DWORD): HRESULT; stdcall;
    function GetAspectRatioMode(out pdwAspectRatioMode: DWORD): HRESULT; stdcall;
    function SetVideoWindow(const hwndVideo: HWND): HRESULT; stdcall;
    function GetVideoWindow(out phwndVideo: PHWND): HRESULT; stdcall;
    function RepaintVideo(): HRESULT; stdcall;
    function GetCurrentImage(var pBih: PBITMAPINFOHEADER; out pDib: PPByte; out pcbDib: PDWORD; var pTimeStamp: PLONGLONG): HRESULT; stdcall;
    function SetBorderColor(const Clr: COLORREF): HRESULT; stdcall;
    function GetBorderColor(out pClr: COLORREF): HRESULT; stdcall;
    function SetRenderingPrefs(const dwRenderFlags: DWORD): HRESULT; stdcall;
    function GetRenderingPrefs(out pdwRenderFlags: DWORD): HRESULT; stdcall;
    function SetFullscreen(const fFullscreen: BOOL): HRESULT; stdcall;
    function GetFullscreen(out pfFullscreen: BOOL): HRESULT; stdcall;
  end;

  // interface IMFVideoPresenter
  IMFVideoPresenter = interface(IUnknown)
  ['{29AFF080-182A-4a5d-AF3B-448F3A6346CB}']
    function ProcessMessage(eMessage: MFVP_MESSAGE_TYPE; ulParam: ULONG_PTR): HRESULT; stdcall;
    function GetCurrentMediaType(out ppMediaType: IMFVideoMediaType): HRESULT; stdcall;
  end;

  //Interface IMFDesiredSample
  IMFDesiredSample = interface(IUnknown)
  ['{56C294D0-753E-4260-8D61-A3D8820B1D54}']
    function GetDesiredSampleTimeAndDuration(out phnsSampleTime: LONGLONG; out phnsSampleDuration: LONGLONG): HRESULT; stdcall;
    procedure SetDesiredSampleTimeAndDuration(const hnsSampleTime: LONGLONG; const hnsSampleDuration: LONGLONG); stdcall;
    procedure Clear(); stdcall;
  end;

  //Interface IMFTrackedSample
  IMFTrackedSample = interface(IUnknown)
  ['{245BF8E9-0755-40f7-88A5-AE0F18D55E17}']
    function SetAllocator(const pSampleAllocator: IMFAsyncCallback; const pUnkState: IUnknown): HRESULT; stdcall;
  end;

  //Interface IMFVideoMixerControl
  IMFVideoMixerControl = interface(IUnknown)
  ['{245BF8E9-0755-40f7-88A5-AE0F18D55E17}']
    function SetStreamZOrder(const dwStreamID: DWORD; const dwZ: DWORD): HRESULT; stdcall;
    function GetStreamZOrder(const dwStreamID: DWORD; out pdwZ: PDWORD): HRESULT; stdcall;
    function SetStreamOutputRect(const dwStreamID: DWORD; const pnrcOutput: PMFVideoNormalizedRect): HRESULT; stdcall;
    function GetStreamOutputRect(const dwStreamID: DWORD; out pnrcOutput: MFVideoNormalizedRect): HRESULT; stdcall;
  end;

  //Interface IMFVideoMixerControl2
  IMFVideoMixerControl2 = interface(IMFVideoMixerControl)
  ['{8459616d-966e-4930-b658-54fa7e5a16d3}']
    function SetMixingPrefs(const dwMixFlags: DWORD): HRESULT; stdcall;
    function GetMixingPrefs(out pdwMixFlags: DWORD): HRESULT; stdcall;
  end;

  //Interface IMFVideoRenderer
  IMFVideoRenderer = interface(IUnknown)
  ['{DFDFD197-A9CA-43d8-B341-6AF3503792CD}']
    function InitializeRenderer(const pVideoMixer: IMFTransform; const pVideoPresenter: IMFVideoPresenter): HRESULT; stdcall;
  end;

  //Iterface IEVRFilterConfig
  IEVRFilterConfig = interface(IUnknown)
  ['{DFDFD197-A9CA-43d8-B341-6AF3503792CD}']
    function SetNumberOfStreams(const dwMaxStreams: DWORD): HRESULT; stdcall;
    function GetNumberOfStreams(out pdwMaxStreams: DWORD): HRESULT; stdcall;
  end;

  //Interface IEVRFilterConfigEx
  IEVRFilterConfigEx = interface(IUnknown)
  ['{aea36028-796d-454f-beee-b48071e24304}']
    function SetConfigPrefs(const dwConfigFlags: DWORD): HRESULT; stdcall;
    function GetConfigPrefs(out pdwConfigFlags: DWORD): HRESULT; stdcall;
  end;

  //Interface IMFTopologyServiceLookup
  IMFTopologyServiceLookup = interface(IUnknown)
  ['{fa993889-4383-415a-a930-dd472a8cf6f7}']
    function LookupService(const aType: MF_SERVICE_LOOKUP_TYPE; const dwIndex: DWORD; const guidService: REFGUID; const riid: REFIID;
                           out ppvObjects: PPointer; var pnObjects: PDWORD): HRESULT; stdcall;
  end;

  //Interface IMFTopologyServiceLookupClient
  IMFTopologyServiceLookupClient = interface(IUnknown)
  ['{fa99388a-4383-415a-a930-dd472a8cf6f7}']
    function InitServicePointers(const pLookup: IMFTopologyServiceLookup): HRESULT; stdcall;
    function ReleaseServicePointers(): HRESULT; stdcall;
  end;

  //Interface IEVRTrustedVideoPlugin
  IEVRTrustedVideoPlugin = interface(IUnknown)
  ['{83A4CE40-7710-494b-A893-A472049AF630}']
    function IsInTrustedVideoMode(out pYes: BOOL): HRESULT; stdcall;
    function CanConstrict(out pYes: BOOL): HRESULT; stdcall;
    function SetConstriction(const dwKPix: DWORD): HRESULT; stdcall;
    function DisableImageExport(const bDisable: BOOL): HRESULT; stdcall;
  end;

  //function MFCreateVideoPresenter
  //Creates the default video presenter for the enhanced video renderer (EVR).
  //NOTES:
  //pOwner :  Pointer to the owner of the object.
  //          If the object is aggregated, pass a pointer to the aggregating object's IUnknown interface.
  //          Otherwise, set this parameter to nil.
  //riidDevice : Interface identifier (IID) of the video device interface that will be used for processing the video.
  //             Currently the only supported value is IID_IDirect3DDevice9.
  //riid : IID of the requested interface on the video presenter. The video presenter exposes the IMFVideoPresenter interface.
  //ppVideoPresenter : Receives a pointer to the requested interface on the video presenter.
  //                   The caller must release the interface.
  {$EXTERNALSYM MFCreateVideoPresenter}
  function MFCreateVideoPresenter(const pOwner: IUnknown; const riidDevice: REFIID; const riid: REFIID; out ppVideoPresenter: IMFVideoPresenter): HRESULT; stdcall;

  //updp 070712b, updt 080712b
  //function MFCreateVideoMixer
  //Creates the default video mixer for the enhanced video renderer (EVR).
  //NOTES:
  //pOwner :  Pointer to the owner of this object.
  //          If the object is aggregated, pass a pointer to the aggregating object's IUnknown interface.
  //          Otherwise, set this parameter to Nil ( IUnknown(nil) ).
  //riidDevice :  Interface identifier (IID) of the video device interface that will be used for processing the video.
  //              Currently the only supported value is IID_IDirect3DDevice9.
  //riid : IID of the requested interface on the video mixer. The video mixer exposes the IMFTransform interface.
  //ppVideoMixer : Receives a pointer to the requested interface. The caller must release the interface.
  {$EXTERNALSYM MFCreateVideoMixer}
  function MFCreateVideoMixer(pOwner: IUnknown; const riidDevice: REFIID; const riid: REFIID; out ppVideoMixer: IUnknown): HRESULT; stdcall; //>> two mixers,

  //updp 070712b, updt 080712b
  //function MFCreateVideoMixerAndPresenter
  //Creates the default video mixer and video presenter for the enhanced video renderer (EVR).
  //NOTES:
  //pMixerOwner : Pointer to the owner of the video mixer.
  //              If the mixer is aggregated, pass a pointer to the aggregating object's IUnknown interface.
  //              Otherwise, set this parameter to Nil ( IUnknown(nil) ).
  //pPresenterOwner : Pointer to the owner of the video presenter.
  //                  If the presenter is aggregated, pass a pointer to the aggregating object's IUnknown interface.
  //                  Otherwise, set this parameter to Nil ( IUnknown(nil) ).
  //riidMixer : Interface identifier (IID) of the requested interface on the video mixer. The video mixer exposes the IMFTransform interface.
  //ppvVideoMixer : Receives a pointer to the requested interface on the video mixer. The caller must release the interface.
  //riidPresenter : IID of the requested interface on the video presenter. The video presenter exposes the IMFVideoPresenter interface.
  //ppvVideoPresenter : Receives a pointer to the requested interface on the video presenter. The caller must release the interface.
  {$EXTERNALSYM MFCreateVideoMixerAndPresenter}
  function MFCreateVideoMixerAndPresenter(pMixerOwner: IUnknown; pPresenterOwner: IUnknown; riidMixer: REFIID; out ppvVideoMixer: IUnknown; riidPresenter: REFIID; out ppvVideoPresenter: IUnknown): HRESULT; stdcall;

  //updp 070712b, updt 080712b
  //function MFCreateVideoRenderer
  //Creates an instance of the enhanced video renderer (EVR) media sink.
  //NOTES:
  //riidRenderer : Interface identifier (IID) of the requested interface on the EVR.
  //ppVideoRenderer : Receives a pointer to the requested interface. The caller must release the interface.
  {$EXTERNALSYM MFCreateVideoRenderer}
  function MFCreateVideoRenderer(const riidRenderer: REFIID; out ppVideoRenderer: IUnknown): HRESULT; stdcall;

  //updp 070712b, updt 080712b
  //function MFCreateVideoSampleFromSurface
  //Creates a media sample that manages a Direct3D surface.
  //NOTES:
  //pUnkSurface : A pointer to the IUnknown interface of the Direct3D surface.
  //              This parameter can be Nil ( IUnknown(Nil) ).
  //ppSample : Receives a pointer to the sample's IMFSample interface. The caller must release the interface.
  {$EXTERNALSYM MFCreateVideoSampleFromSurface}
  function MFCreateVideoSampleFromSurface(pUnkSurface: IUnknown; out ppSample: IMFSample): HRESULT; stdcall;

implementation
  //updp 080712b
  function MFCreateVideoPresenter;         external 'evr.dll' name 'MFCreateVideoPresenter';
  function MFCreateVideoMixer;             external 'evr.dll' name 'MFCreateVideoMixer';
  function MFCreateVideoMixerAndPresenter; external 'evr.dll' name 'MFCreateVideoMixerAndPresenter';
  function MFCreateVideoRenderer;          external 'evr.dll' name 'MFCreateVideoRenderer';
  function MFCreateVideoSampleFromSurface; external 'evr.dll' name 'MFCreateVideoSampleFromSurface';

end.
