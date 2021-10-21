// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: DxVaHd.pas
// Kind: Pascal Unit
// Release date: 23-07-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
// LastEdited by: Tony (maXcomX)
// EditDate: updt 230712b
//
// Remarks:
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: See todo: and general check
// =============================================================================
// Source: dxvahd.h
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
//
// Licence Supplement:
// All kinds of commercial use, including distribution, of any kind, of the sources is strictly forbidden,
// unless you have a written permission or agreement from or with FactoryX.
//==============================================================================

unit DxVaHd;

interface

uses
  Windows, ComObj, Ole2, UnKnwn;

// ver WIN32

const
  //  IID_IDXVAHD_Device                     : TGUID = '{95f12dfd-d77e-49be-815f-57d579634d6d}';  IID_IDXVAHD_VideoProcessor             : TGUID = '{95f4edf4-6e03-4cd7-be1b-3075d665aa52}';

  //
  DXVAHD_STREAM_STATE_PRIVATE_IVTC       : TGUID = '{9c601e3c-0f33-414c-a739-99540ee42da5}';

  //
  DXVAHDControlGuid                      : TGUID = '{a0386e75-f70c-464c-a9ce-33c44e091623}';  DXVAHDETWGUID_CREATEVIDEOPROCESSOR     : TGUID = '{681e3d1e-5674-4fb3-a503-2f2055e91f60}';  DXVAHDETWGUID_VIDEOPROCESSBLTSTATE     : TGUID = '{76c94b5a-193f-4692-9484-a4d999da81a8}';  DXVAHDETWGUID_VIDEOPROCESSSTREAMSTATE  : TGUID = '{262c0b02-209d-47ed-94d8-82ae02b84aa7}';  DXVAHDETWGUID_VIDEOPROCESSBLTHD        : TGUID = '{bef3d435-78c7-4de3-9707-cd1b083b160a}';  DXVAHDETWGUID_VIDEOPROCESSBLTHD_STREAM : TGUID = '{27ae473e-a5fc-4be5-b4e3-f24994d3c495}';  DXVAHDETWGUID_DESTROYVIDEOPROCESSOR    : TGUID = '{f943f0a0-3f16-43e0-8093-105a986aa5f1}';



type
  IDirect3DDevice9Ex = DWORD;
  IDirect3DSurface9 = DWORD;
  D3DCOLOR = DWORD;
  D3DFORMAT = DWORD;
  D3DPOOL = DWORD;



type  _DXVAHD_FRAME_FORMAT                                = (
    DXVAHD_FRAME_FORMAT_PROGRESSIVE                   = 0,
    DXVAHD_FRAME_FORMAT_INTERLACED_TOP_FIELD_FIRST    = 1,
    DXVAHD_FRAME_FORMAT_INTERLACED_BOTTOM_FIELD_FIRST = 2
  );
  DXVAHD_FRAME_FORMAT = _DXVAHD_FRAME_FORMAT;

type
  _DXVAHD_DEVICE_USAGE                  = (
    DXVAHD_DEVICE_USAGE_PLAYBACK_NORMAL = 0,
    DXVAHD_DEVICE_USAGE_OPTIMAL_SPEED   = 1,
    DXVAHD_DEVICE_USAGE_OPTIMAL_QUALITY = 2
  );
  DXVAHD_DEVICE_USAGE = _DXVAHD_DEVICE_USAGE;


type
  _DXVAHD_SURFACE_TYPE                      = (
    DXVAHD_SURFACE_TYPE_VIDEO_INPUT         = 0,
    DXVAHD_SURFACE_TYPE_VIDEO_INPUT_PRIVATE = 1,
    DXVAHD_SURFACE_TYPE_VIDEO_OUTPUT        = 2
  );
  DXVAHD_SURFACE_TYPE = _DXVAHD_SURFACE_TYPE;

type
  _DXVAHD_DEVICE_TYPE            = (
    DXVAHD_DEVICE_TYPE_HARDWARE  = 0,
    DXVAHD_DEVICE_TYPE_SOFTWARE  = 1,
    DXVAHD_DEVICE_TYPE_REFERENCE = 2,
    DXVAHD_DEVICE_TYPE_OTHER     = 3
  );
  DXVAHD_DEVICE_TYPE = _DXVAHD_DEVICE_TYPE;

type
  _DXVAHD_DEVICE_CAPS                          = (
    DXVAHD_DEVICE_CAPS_LINEAR_SPACE            = $1,
    DXVAHD_DEVICE_CAPS_xvYCC                   = $2,
    DXVAHD_DEVICE_CAPS_RGB_RANGE_CONVERSION    = $4,
    DXVAHD_DEVICE_CAPS_YCbCr_MATRIX_CONVERSION = $8
  );
  DXVAHD_DEVICE_CAPS = _DXVAHD_DEVICE_CAPS;

type
  _DXVAHD_FEATURE_CAPS                = (
    DXVAHD_FEATURE_CAPS_ALPHA_FILL    = $1,
    DXVAHD_FEATURE_CAPS_CONSTRICTION  = $2,
    DXVAHD_FEATURE_CAPS_LUMA_KEY      = $4,
    DXVAHD_FEATURE_CAPS_ALPHA_PALETTE = $8
  );
  DXVAHD_FEATURE_CAPS = _DXVAHD_FEATURE_CAPS;

type
  _DXVAHD_FILTER_CAPS                     = (
    DXVAHD_FILTER_CAPS_BRIGHTNESS         = $1,
    DXVAHD_FILTER_CAPS_CONTRAST           = $2,
    DXVAHD_FILTER_CAPS_HUE                = $4,
    DXVAHD_FILTER_CAPS_SATURATION         = $8,
    DXVAHD_FILTER_CAPS_NOISE_REDUCTION    = $10,
    DXVAHD_FILTER_CAPS_EDGE_ENHANCEMENT   = $20,
    DXVAHD_FILTER_CAPS_ANAMORPHIC_SCALING = $40
  );
  DXVAHD_FILTER_CAPS = _DXVAHD_FILTER_CAPS;

type
  _DXVAHD_INPUT_FORMAT_CAPS                     = (
    DXVAHD_INPUT_FORMAT_CAPS_RGB_INTERLACED     = $1,
    DXVAHD_INPUT_FORMAT_CAPS_RGB_PROCAMP        = $2,
    DXVAHD_INPUT_FORMAT_CAPS_RGB_LUMA_KEY       = $4,
    DXVAHD_INPUT_FORMAT_CAPS_PALETTE_INTERLACED = $8
  );
  DXVAHD_INPUT_FORMAT_CAPS = _DXVAHD_INPUT_FORMAT_CAPS;

type
  _DXVAHD_PROCESSOR_CAPS                                  = (
    DXVAHD_PROCESSOR_CAPS_DEINTERLACE_BLEND               = $1,
    DXVAHD_PROCESSOR_CAPS_DEINTERLACE_BOB                 = $2,
    DXVAHD_PROCESSOR_CAPS_DEINTERLACE_ADAPTIVE            = $4,
    DXVAHD_PROCESSOR_CAPS_DEINTERLACE_MOTION_COMPENSATION = $8,
    DXVAHD_PROCESSOR_CAPS_INVERSE_TELECINE                = $10,
    DXVAHD_PROCESSOR_CAPS_FRAME_RATE_CONVERSION           = $20
  );
  DXVAHD_PROCESSOR_CAPS = _DXVAHD_PROCESSOR_CAPS;

type
  _DXVAHD_ITELECINE_CAPS               = (
    DXVAHD_ITELECINE_CAPS_32           = $1,
    DXVAHD_ITELECINE_CAPS_22           = $2,
    DXVAHD_ITELECINE_CAPS_2224         = $4,
    DXVAHD_ITELECINE_CAPS_2332         = $8,
    DXVAHD_ITELECINE_CAPS_32322        = $10,
    DXVAHD_ITELECINE_CAPS_55           = $20,
    DXVAHD_ITELECINE_CAPS_64           = $40,
    DXVAHD_ITELECINE_CAPS_87           = $80,
    DXVAHD_ITELECINE_CAPS_222222222223 = $100,
    DXVAHD_ITELECINE_CAPS_OTHER        = $80000000
  );
  DXVAHD_ITELECINE_CAPS = _DXVAHD_ITELECINE_CAPS;

type
  _DXVAHD_FILTER                     = (
    DXVAHD_FILTER_BRIGHTNESS         = 0,
    DXVAHD_FILTER_CONTRAST           = 1,
    DXVAHD_FILTER_HUE                = 2,
    DXVAHD_FILTER_SATURATION         = 3,
    DXVAHD_FILTER_NOISE_REDUCTION    = 4,
    DXVAHD_FILTER_EDGE_ENHANCEMENT   = 5,
    DXVAHD_FILTER_ANAMORPHIC_SCALING = 6
  );
  DXVAHD_FILTER = _DXVAHD_FILTER;

type
  _DXVAHD_BLT_STATE                     = (
    DXVAHD_BLT_STATE_TARGET_RECT        = 0,
    DXVAHD_BLT_STATE_BACKGROUND_COLOR   = 1,
    DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE = 2,
    DXVAHD_BLT_STATE_ALPHA_FILL         = 3,
    DXVAHD_BLT_STATE_CONSTRICTION       = 4,
    DXVAHD_BLT_STATE_PRIVATE            = 1000
  );
  DXVAHD_BLT_STATE = _DXVAHD_BLT_STATE;

type
  _DXVAHD_ALPHA_FILL_MODE                = (
    DXVAHD_ALPHA_FILL_MODE_OPAQUE        = 0,
    DXVAHD_ALPHA_FILL_MODE_BACKGROUND    = 1,
    DXVAHD_ALPHA_FILL_MODE_DESTINATION   = 2,
    DXVAHD_ALPHA_FILL_MODE_SOURCE_STREAM = 3
  );
  DXVAHD_ALPHA_FILL_MODE = _DXVAHD_ALPHA_FILL_MODE;

type
  _DXVAHD_STREAM_STATE                            = (
    DXVAHD_STREAM_STATE_D3DFORMAT                 = 0,
    DXVAHD_STREAM_STATE_FRAME_FORMAT              = 1,
    DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE         = 2,
    DXVAHD_STREAM_STATE_OUTPUT_RATE               = 3,
    DXVAHD_STREAM_STATE_SOURCE_RECT               = 4,
    DXVAHD_STREAM_STATE_DESTINATION_RECT          = 5,
    DXVAHD_STREAM_STATE_ALPHA                     = 6,
    DXVAHD_STREAM_STATE_PALETTE                   = 7,
    DXVAHD_STREAM_STATE_LUMA_KEY                  = 8,
    DXVAHD_STREAM_STATE_ASPECT_RATIO              = 9,
    DXVAHD_STREAM_STATE_FILTER_BRIGHTNESS         = 100,
    DXVAHD_STREAM_STATE_FILTER_CONTRAST           = 101,
    DXVAHD_STREAM_STATE_FILTER_HUE                = 102,
    DXVAHD_STREAM_STATE_FILTER_SATURATION         = 103,
    DXVAHD_STREAM_STATE_FILTER_NOISE_REDUCTION    = 104,
    DXVAHD_STREAM_STATE_FILTER_EDGE_ENHANCEMENT   = 105,
    DXVAHD_STREAM_STATE_FILTER_ANAMORPHIC_SCALING = 106,
    DXVAHD_STREAM_STATE_PRIVATE                   = 1000
  );
  DXVAHD_STREAM_STATE = _DXVAHD_STREAM_STATE;

type
  _DXVAHD_OUTPUT_RATE         = (
    DXVAHD_OUTPUT_RATE_NORMAL = 0,
    DXVAHD_OUTPUT_RATE_HALF   = 1,
    DXVAHD_OUTPUT_RATE_CUSTOM = 2
  );
  DXVAHD_OUTPUT_RATE = _DXVAHD_OUTPUT_RATE;

type
  _DXVAHD_RATIONAL = record
    Numerator: UINT;
    Denominator: UINT;
  end;
  DXVAHD_RATIONAL = _DXVAHD_RATIONAL;

type
  _DXVAHD_COLOR_RGBA = record
    R: FLOAT;
    G: FLOAT;
    B: FLOAT;
    A: FLOAT;
  end;
  DXVAHD_COLOR_RGBA = _DXVAHD_COLOR_RGBA;

type
  _DXVAHD_COLOR_YCbCrA = record
    Y: FLOAT;
    Cb: FLOAT;
    Cr: FLOAT;
    A: FLOAT;
  end;
  DXVAHD_COLOR_YCbCrA = _DXVAHD_COLOR_YCbCrA;

type
  _DXVAHD_COLOR = union
    RGB: DXVAHD_COLOR_RGBA;
    YCbCr: DXVAHD_COLOR_YCbCrA;
  end;
  DXVAHD_COLOR = _DXVAHD_COLOR;

type
  _DXVAHD_CONTENT_DESC = record
    InputFrameFormat: DXVAHD_FRAME_FORMAT;
    InputFrameRate: DXVAHD_RATIONAL;
    InputWidth: UINT;
    InputHeight: UINT;
    OutputFrameRate: DXVAHD_RATIONAL;
    OutputWidth: UINT;
    OutputHeight: UINT;
  end;
  DXVAHD_CONTENT_DESC = _DXVAHD_CONTENT_DESC;

type
  _DXVAHD_VPDEVCAPS = record
    DeviceType: DXVAHD_DEVICE_TYPE;
    DeviceCaps: UINT;
    FeatureCaps: UINT;
    FilterCaps: UINT;
    InputFormatCaps: UINT;
    InputPool: D3DPOOL;
    OutputFormatCount: UINT;
    InputFormatCount: UINT;
    VideoProcessorCount: UINT;
    MaxInputStreams: UINT;
    MaxStreamStates: UINT;
  end;
  DXVAHD_VPDEVCAPS = _DXVAHD_VPDEVCAPS;

type
  _DXVAHD_VPCAPS = record
    VPGuid: TGUID;
    PastFrames: UINT;
    FutureFrames: UINT;
    ProcessorCaps: UINT;
    ITelecineCaps: UINT;
    CustomRateCount: UINT;
  end;
  DXVAHD_VPCAPS = _DXVAHD_VPCAPS;

type
  _DXVAHD_CUSTOM_RATE_DATA = record
    CustomRate: DXVAHD_RATIONAL;
    OutputFrames: UINT;
    InputInterlaced: BOOL;
    InputFramesOrFields: UINT;
  end;
  DXVAHD_CUSTOM_RATE_DATA = _DXVAHD_CUSTOM_RATE_DATA;

type
  _DXVAHD_FILTER_RANGE_DATA = record
    Minimum: INT;
    Maximum: INT;
    Default: INT;
    Multiplier: FLOAT;
  end;
  DXVAHD_FILTER_RANGE_DATA = _DXVAHD_FILTER_RANGE_DATA;

type
  _DXVAHD_BLT_STATE_TARGET_RECT_DATA = record
    Enable: BOOL;
    TargetRect: RECT;
  end;
  DXVAHD_BLT_STATE_TARGET_RECT_DATA = _DXVAHD_BLT_STATE_TARGET_RECT_DATA;

type
  _DXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA = record
    YCbCr: BOOL;
    BackgroundColor: DXVAHD_COLOR;
  end;
  DXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA = _DXVAHD_BLT_STATE_BACKGROUND_COLOR_DATA;

//todo: D7-2005  and  D2006-XE2
typedef struct _DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA
    {
    union
        {
        struct
            {
            UINT Usage	: 1;
            UINT RGB_Range	: 1;
            UINT YCbCr_Matrix	: 1;
            UINT YCbCr_xvYCC	: 1;
            UINT Reserved	: 28;
            } 	;
        UINT Value;
        } 	;
    } 	DXVAHD_BLT_STATE_OUTPUT_COLOR_SPACE_DATA;


type
  _DXVAHD_BLT_STATE_ALPHA_FILL_DATA = record
    Mode: DXVAHD_ALPHA_FILL_MODE;
    StreamNumber: UINT;
  end;
  DXVAHD_BLT_STATE_ALPHA_FILL_DATA = _DXVAHD_BLT_STATE_ALPHA_FILL_DATA;

type
  _DXVAHD_BLT_STATE_CONSTRICTION_DATA = record
    Enable: BOOL;
    Size: SIZE;
  end;
  DXVAHD_BLT_STATE_CONSTRICTION_DATA = _DXVAHD_BLT_STATE_CONSTRICTION_DATA;

type
  _DXVAHD_BLT_STATE_PRIVATE_DATA = record
    Guid: TGUID;
    DataSize: UINT;
    pData: Pointer;
  end;
  DXVAHD_BLT_STATE_PRIVATE_DATA = _DXVAHD_BLT_STATE_PRIVATE_DATA;

type
  _DXVAHD_STREAM_STATE_D3DFORMAT_DATA = record
    Format: D3DFORMAT;
  end;
  DXVAHD_STREAM_STATE_D3DFORMAT_DATA = _DXVAHD_STREAM_STATE_D3DFORMAT_DATA;

type
  _DXVAHD_STREAM_STATE_FRAME_FORMAT_DATA = record
    FrameFormat: DXVAHD_FRAME_FORMAT;
  end;
  DXVAHD_STREAM_STATE_FRAME_FORMAT_DATA = _DXVAHD_STREAM_STATE_FRAME_FORMAT_DATA;

//todo: D7-2005  and  D2006-XE2
typedef struct _DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA
    {
    union
        {
        struct
            {
            UINT Type	: 1;
            UINT RGB_Range	: 1;
            UINT YCbCr_Matrix	: 1;
            UINT YCbCr_xvYCC	: 1;
            UINT Reserved	: 28;
            } 	;
        UINT Value;
        } 	;
    } 	DXVAHD_STREAM_STATE_INPUT_COLOR_SPACE_DATA;

type
  _DXVAHD_STREAM_STATE_OUTPUT_RATE_DATA = record
    RepeatFrame: BOOL;
    OutputRate: DXVAHD_OUTPUT_RATE;
    CustomRate: DXVAHD_RATIONAL;
  end;
  DXVAHD_STREAM_STATE_OUTPUT_RATE_DATA = _DXVAHD_STREAM_STATE_OUTPUT_RATE_DATA;

type
  _DXVAHD_STREAM_STATE_SOURCE_RECT_DATA = record
    Enable: BOOL;
    SourceRect: RECT;
  end;
  DXVAHD_STREAM_STATE_SOURCE_RECT_DATA = _DXVAHD_STREAM_STATE_SOURCE_RECT_DATA;

type
  _DXVAHD_STREAM_STATE_DESTINATION_RECT_DATA = record
    Enable: BOOL;
    DestinationRect: RECT;
  end;
  DXVAHD_STREAM_STATE_DESTINATION_RECT_DATA = _DXVAHD_STREAM_STATE_DESTINATION_RECT_DATA;

type
  _DXVAHD_STREAM_STATE_ALPHA_DATA = record
    Enable: BOOL;
    Alpha: FLOAT;
  end;
  DXVAHD_STREAM_STATE_ALPHA_DATA = _DXVAHD_STREAM_STATE_ALPHA_DATA;

type
  _DXVAHD_STREAM_STATE_PALETTE_DATA = record
    Count: UINT;
    pEntries: PD3DCOLOR;
  end;
  DXVAHD_STREAM_STATE_PALETTE_DATA = _DXVAHD_STREAM_STATE_PALETTE_DATA;

type
  _DXVAHD_STREAM_STATE_LUMA_KEY_DATA = record
    Enable: BOOL;
    Lower: FLOAT;
    Upper: FLOAT;
  end;
  DXVAHD_STREAM_STATE_LUMA_KEY_DATA = _DXVAHD_STREAM_STATE_LUMA_KEY_DATA;

type
  _DXVAHD_STREAM_STATE_ASPECT_RATIO_DATA = record
    Enable: BOOL;
    SourceAspectRatio: DXVAHD_RATIONAL;
    DestinationAspectRatio: DXVAHD_RATIONAL;
  end;
  DXVAHD_STREAM_STATE_ASPECT_RATIO_DATA = _DXVAHD_STREAM_STATE_ASPECT_RATIO_DATA;

type
  _DXVAHD_STREAM_STATE_FILTER_DATA = record
    Enable: BOOL;
    Level: INT;
  end;
  DXVAHD_STREAM_STATE_FILTER_DATA = _DXVAHD_STREAM_STATE_FILTER_DATA;

type
  _DXVAHD_STREAM_STATE_PRIVATE_DATA = record
    Guid: TGUID;
    DataSize: UINT;
    pData: Pointer;
  end;
  DXVAHD_STREAM_STATE_PRIVATE_DATA = _DXVAHD_STREAM_STATE_PRIVATE_DATA;

type
  _DXVAHD_STREAM_DATA = record
    Enable: BOOL;
    OutputIndex: UINT;
    InputFrameOrField: UINT;
    PastFrames: UINT;
    FutureFrames: UINT;
    ppPastSurfaces: PPIDirect3DSurface9;
    pInputSurface: PIDirect3DSurface9;
    ppFutureSurfaces: PPIDirect3DSurface9;
  end;
  DXVAHD_STREAM_DATA = _DXVAHD_STREAM_DATA;

type
  _DXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA = record
    Enable: BOOL;
    ITelecineFlags: UINT;
    Frames: UINT;
    InputField: UINT;
  end;
  DXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA = _DXVAHD_STREAM_STATE_PRIVATE_IVTC_DATA;

type//Callback functions
  function PDXVAHDSW_CreateDevice(const pD3DDevice: PIDirect3DDevice9Ex; out phDevice: PTHandle): HResult; stdcall;
  function PDXVAHDSW_ProposeVideoPrivateFormat(const hDevice: THandle; var pFormat: D3DFORMAT): HResult; stdcall;  function PDXVAHDSW_GetVideoProcessorDeviceCaps(const hDevice: THandle; const pContentDesc: DXVAHD_CONTENT_DESC; const Usage: DXVAHD_DEVICE_USAGE; out pCaps: DXVAHD_VPDEVCAPS): HResult; stdcall;
  function PDXVAHDSW_GetVideoProcessorOutputFormats(const hDevice: THandle; const pContentDesc: DXVAHD_CONTENT_DESC const Usage: DXVAHD_DEVICE_USAGE;
                                                  const Count: UINT; out pFormats: D3DFORMAT ): HResult; stdcall;
  function PDXVAHDSW_GetVideoProcessorInputFormats(const hDevice: THandle; const pContentDesc: DXVAHD_CONTENT_DESC; const Usage: DXVAHD_DEVICE_USAGE;
                                                   const Count: UINT; out pFormats: D3DFORMAT): HResult; stdcall;
  function PDXVAHDSW_GetVideoProcessorCaps(const hDevice: THandle; const pContentDesc: DXVAHD_CONTENT_DESC; const Usage: DXVAHD_DEVICE_USAGE;
                                           const Count: UINT; out pCaps: DXVAHD_VPCAPS): HResult; stdcall;
  function PDXVAHDSW_GetVideoProcessorCustomRates(const  hDevice: THandle; const pVPGuid: TGUID; const Count: UINT; out pRates: DXVAHD_CUSTOM_RATE_DATA): HResult; stdcall;
  function PDXVAHDSW_GetVideoProcessorFilterRange(const hDevice: THandle; const Filter: DXVAHD_FILTER; out pRange: DXVAHD_FILTER_RANGE_DATA): HResult; stdcall;
  function PDXVAHDSW_DestroyDevice(const hDevice: THandle): HResult; stdcall;
  function PDXVAHDSW_CreateVideoProcessor(const hDevice: THandle; const pVPGuid: TGUID; out phVideoProcessor: THandle): HResult; stdcall;
  function PDXVAHDSW_SetVideoProcessBltState(const hVideoProcessor: THandle; const State: DXVAHD_BLT_STATE; const DataSize: UINT; const pData: Pointer): HResult; stdcall;
  function PDXVAHDSW_GetVideoProcessBltStatePrivate(const hVideoProcessor: THandle; var pData: DXVAHD_BLT_STATE_PRIVATE_DATA): HResult; stdcall;
  function PDXVAHDSW_SetVideoProcessStreamState(const hVideoProcessor: THandle; const StreamNumber: UINT; const State: DXVAHD_STREAM_STATE; const DataSize: UINT; const pData: Pointer): HResult; stdcall;
  function PDXVAHDSW_GetVideoProcessStreamStatePrivate(const hVideoProcessor: THandle; const StreamNumber: UINT; var pData: DXVAHD_STREAM_STATE_PRIVATE_DATA): HResult; stdcall;
  function PDXVAHDSW_VideoProcessBltHD(const hVideoProcessor: THandle; const pOutputSurface: IDirect3DSurface9; const OutputFrame: UINT;
                                       const StreamCount: UINT; const pStreams: DXVAHD_STREAM_DATA): HResult; stdcall;
  function PDXVAHDSW_DestroyVideoProcessor(const hVideoProcessor: THandle): HResult; stdcall;


type
  _DXVAHDSW_CALLBACKS = record
    CreateDevice: PDXVAHDSW_CreateDevice;
    ProposeVideoPrivateFormat: PDXVAHDSW_ProposeVideoPrivateFormat;
    GetVideoProcessorDeviceCaps: PDXVAHDSW_GetVideoProcessorDeviceCaps;
    GetVideoProcessorOutputFormats: PDXVAHDSW_GetVideoProcessorOutputFormats;
    GetVideoProcessorInputFormats: PDXVAHDSW_GetVideoProcessorInputFormats;
    GetVideoProcessorCaps: PDXVAHDSW_GetVideoProcessorCaps;
    GetVideoProcessorCustomRates: PDXVAHDSW_GetVideoProcessorCustomRates;
    GetVideoProcessorFilterRange: PDXVAHDSW_GetVideoProcessorFilterRange;
    DestroyDevice: PDXVAHDSW_DestroyDevice;
    CreateVideoProcessor: PDXVAHDSW_CreateVideoProcessor;
    SetVideoProcessBltState: PDXVAHDSW_SetVideoProcessBltState;
    GetVideoProcessBltStatePrivate: PDXVAHDSW_GetVideoProcessBltStatePrivate;
    SetVideoProcessStreamState: PDXVAHDSW_SetVideoProcessStreamState;
    GetVideoProcessStreamStatePrivate: PDXVAHDSW_GetVideoProcessStreamStatePrivate;
    VideoProcessBltHD: PDXVAHDSW_VideoProcessBltHD;
    DestroyVideoProcessor: PDXVAHDSW_DestroyVideoProcessor;
  end;
  DXVAHDSW_CALLBACKS = _DXVAHDSW_CALLBACKS;

type
  function PDXVAHDSW_Plugin(const Size: UINT; out pCallbacks: Pointer ): HResult; stdcall;

type
  _DXVAHDETW_CREATEVIDEOPROCESSOR = record
    pObject: ULONGLONG;
    pD3D9Ex: ULONGLONG;
    VPGuid: TGUID;
  end;
  DXVAHDETW_CREATEVIDEOPROCESSOR = _DXVAHDETW_CREATEVIDEOPROCESSOR;

type
  _DXVAHDETW_VIDEOPROCESSBLTSTATE = record
    pObject: ULONGLONG;
    State: DXVAHD_BLT_STATE;
    DataSize: UINT;
    SetState: BOOL;
  end;
  DXVAHDETW_VIDEOPROCESSBLTSTATE = _DXVAHDETW_VIDEOPROCESSBLTSTATE;

type
  _DXVAHDETW_VIDEOPROCESSSTREAMSTATE = record
    pObject: ULONGLONG;
    StreamNumber: UINT;
    State: DXVAHD_STREAM_STATE;
    DataSize: UINT;
    SetState: BOOL;
  end;
  DXVAHDETW_VIDEOPROCESSSTREAMSTATE = _DXVAHDETW_VIDEOPROCESSSTREAMSTATE;

type
  _DXVAHDETW_VIDEOPROCESSBLTHD = record
    pObject: ULONGLONG;
    pOutputSurface: ULONGLONG;
    TargetRect: RECT;
    OutputFormat: D3DFORMAT;
    ColorSpace: UINT;
    OutputFrame: UINT;
    StreamCount: UINT;
    Enter: BOOL;
  end;
  DXVAHDETW_VIDEOPROCESSBLTHD = _DXVAHDETW_VIDEOPROCESSBLTHD;

type
  _DXVAHDETW_VIDEOPROCESSBLTHD_STREAM = record
    pObject: ULONGLONG;
    pInputSurface: ULONGLONG;
    SourceRect: RECT;
    DestinationRect: RECT;
    InputFormat: D3DFORMAT;
    FrameFormat: DXVAHD_FRAME_FORMAT;
    ColorSpace: UINT;
    StreamNumber: UINT;
    OutputIndex: UINT;
    InputFrameOrField: UINT;
    PastFrames: UINT;
    FutureFrames: UINT;
  end;
  DXVAHDETW_VIDEOPROCESSBLTHD_STREAM = _DXVAHDETW_VIDEOPROCESSBLTHD_STREAM;

type
  _DXVAHDETW_DESTROYVIDEOPROCESSOR = record
    pObject: ULONGLONG;
  end;
  DXVAHDETW_DESTROYVIDEOPROCESSOR = _DXVAHDETW_DESTROYVIDEOPROCESSOR;



type
  //Forward Interfaces Declarations
  IDXVAHD_Device = interface;
  IDXVAHD_VideoProcessor = interface;   // INTERFACES ///////////////////////////////////////////////////////////////


  //Interface IDXVAHD_Device
  IDXVAHD_Device = interface(IUnknown)
  ['{95f12dfd-d77e-49be-815f-57d579634d6d}']
    function CreateVideoSurface(const Width: UINT; const Height: UINT; const Format: D3DFORMAT;
                            const Pool: D3DPOOL; const Usage: DWORD; const tType: DXVAHD_SURFACE_TYPE;
                            const NumSurfaces: UINT; out ppSurfaces: PIDirect3DSurface9; var pSharedHandle: THandle): HResult; stdcall;
    function GetVideoProcessorDeviceCaps(out pCaps: DXVAHD_VPDEVCAPS): HResult; stdcall;
    function GetVideoProcessorOutputFormats(const Count: UINT; out pFormats: D3DFORMAT): HResult; stdcall;
    function GetVideoProcessorInputFormats(const Count: UINT; out pFormats: D3DFORMAT): HResult; stdcall;
    function GetVideoProcessorCaps(const Count: UINT; out pCaps: DXVAHD_VPCAPS): HResult; stdcall;
    function GetVideoProcessorCustomRates(const pVPGuid: TGUID; const Count: UINT; out pRates: DXVAHD_CUSTOM_RATE_DATA): HResult; stdcall;
    function GetVideoProcessorFilterRange(const Filter: DXVAHD_FILTER; out pRange: DXVAHD_FILTER_RANGE_DATA): HResult; stdcall;
    function CreateVideoProcessor(const pVPGuid: TGUID; out ppVideoProcessor: PIDXVAHD_VideoProcessor): HResult; stdcall;
  end;


  //Interface IDXVAHD_VideoProcessor
  IDXVAHD_Device = interface(IUnknown)
  ['{95f4edf4-6e03-4cd7-be1b-3075d665aa52}']
    function SetVideoProcessBltState(const DXVAHD_BLT_STATE; const DataSize: UINT; const pData: Pointer): HResult; stdcall;
    function GetVideoProcessBltState(const State: DXVAHD_BLT_STATE; const DataSize: UINT; out pData: Pointer): HResult; stdcall;
    function SetVideoProcessStreamState(const StreamNumber: UINT; const State: DXVAHD_STREAM_STATE; const DataSize: UINT; const pData: Pointer): HResult; stdcall;
    function GetVideoProcessStreamState(const StreamNumber: UINT; const State: DXVAHD_STREAM_STATE; const DataSize: UINT; out pData: Pointer): HResult; stdcall;
    function VideoProcessBltHD(const pOutputSurface: IDirect3DSurface9; const OutputFrame: UINT; const StreamCount: UINT; const pStreams: DXVAHD_STREAM_DATA): HResult; stdcall;
  end;

type
  function DXVAHD_CreateDevice(const pD3DDevice: IDirect3DDevice9Ex; const pContentDesc: DXVAHD_CONTENT_DESC; const Usage: DXVAHD_DEVICE_USAGE;
                               const pPlugin: PDXVAHDSW_Plugin; out ppDevice: PIDXVAHD_Device): HResult; stdcall;
  function PDXVAHD_CreateDevice(const pD3DDevice: IDirect3DDevice9Ex; const pContentDesc: DXVAHD_CONTENT_DESC; const Usage: DXVAHD_DEVICE_USAGE;
                                const pPlugin: PDXVAHDSW_Plugin; out ppDevice: PIDXVAHD_Device): HResult; stdcall;

  //Additional Prototypes for ALL interfaces

  //end of Additional Prototypes

implementation

end.
