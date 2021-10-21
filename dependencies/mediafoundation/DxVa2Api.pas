// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: DxVa2Api.pas
// Kind: Pascal Unit
// Release date: 20-07-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
// LastEdited by: Tony (maXcomX)
// EditDate: updt 200712a
//
// Remarks:
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: -
//-------------------------------------------------------------------------------
// Header converted using HtoPAS version 1.0.0000
//
// Converted:   20-7-2012 0:09:51
//
// Validations and corrections by Tony
//
//-------------------------------------------------------------------------------
// =============================================================================
// Source: dxva2api.h
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

unit DxVa2Api;

interface

uses
  Windows, ComObj, Ole2, UnKnwn;


const
  IID_IDirect3DDeviceManager9 =        : TGUID = '{a0cade0f-06d5-4cf4-a1c7-f3cdd725aa75}';
  IID_IDirectXVideoAccelerationService : TGUID = '{fc51a550-d5e7-11d9-af55-00054e43ff02}';
  IID_IDirectXVideoDecoderService      : TGUID = '{fc51a551-d5e7-11d9-af55-00054e43ff02}';
  IID_IDirectXVideoProcessorService    : TGUID = '{fc51a552-d5e7-11d9-af55-00054e43ff02}';
  IID_IDirectXVideoDecoder             : TGUID = '{f2b0810a-fd00-43c9-918c-df94e2d8ef7d}';
  IID_IDirectXVideoProcessor           : TGUID = '{8c3a39f0-916e-4690-804f-4c8001355d25}';
  IID_IDirectXVideoMemoryConfiguration : TGUID = '{b7f916dd-db3b-49c1-84d7-e45ef99ec726}';

  //
  XVA2_ModeMPEG2_MoComp                : TGUID = '{e6a9f44b-61b0-4563-9ea4-63d2a3c6fe66}';
  DXVA2_ModeMPEG2_IDCT                 : TGUID = '{bf22ad00-03ea-4690-8077-473346209b7e}';
  DXVA2_ModeMPEG2_VLD                  : TGUID = '{ee27417f-5e28-4e65-beea-1d26b508adc9}';
  DXVA2_ModeH264_A                     : TGUID = '{1b81be64-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeH264_B                     : TGUID = '{1b81be65-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeH264_C                     : TGUID = '{1b81be66-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeH264_D                     : TGUID = '{1b81be67-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeH264_E                     : TGUID = '{1b81be68-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeH264_F                     : TGUID = '{1b81be69-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeWMV8_A                     : TGUID = '{1b81be80-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeWMV8_B                     : TGUID = '{1b81be81-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeWMV9_A                     : TGUID = '{1b81be90-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeWMV9_B                     : TGUID = '{1b81be91-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeWMV9_C                     : TGUID = '{1b81be94-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeVC1_A                      : TGUID = '{1b81beA0-a0c7-11d3-b984-00c0-f2e73c5}';
  DXVA2_ModeVC1_B                      : TGUID = '{1b81beA1-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeVC1_C                      : TGUID = '{1b81beA2-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_ModeVC1_D                      : TGUID = '{1b81beA3-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_NoEncrypt                      : TGUID = '{1b81beD0-a0c7-11d3-b984-00c04f2e73c5}';
  DXVA2_VideoProcProgressiveDevice     : TGUID = '{5a54a0c9-c7ec-4bd9-8ede-f3c75dc4393b}';
  DXVA2_VideoProcBobDevice             : TGUID = '{335aa36e-7884-43a4-9c91-7f87faf3e37e}';
  DXVA2_VideoProcSoftwareDevice        : TGUID = '{4553d47f-ee7e-4e3f-9475-dbf1376c4810}';

  //Re-defined
  DXVA2_ModeMPEG2_MOCOMP              = DXVA2_ModeMPEG2_MoComp;
  DXVA2_ModeWMV8_PostProc             = DXVA2_ModeWMV8_A;
  DXVA2_ModeWMV8_MoComp               = DXVA2_ModeWMV8_B;
  DXVA2_ModeWMV9_PostProc             = DXVA2_ModeWMV9_A;
  DXVA2_ModeWMV9_MoComp               = DXVA2_ModeWMV9_B;
  DXVA2_ModeWMV9_IDCT                 = DXVA2_ModeWMV9_C;
  DXVA2_ModeVC1_PostProc              = DXVA2_ModeVC1_A;
  DXVA2_ModeVC1_MoComp                = DXVA2_ModeVC1_B;
  DXVA2_ModeVC1_IDCT                  = DXVA2_ModeVC1_C;
  DXVA2_ModeVC1_VLD                   = DXVA2_ModeVC1_D;
  DXVA2_ModeH264_MoComp_NoFGT         = DXVA2_ModeH264_A;
  DXVA2_ModeH264_MoComp_FGT           = DXVA2_ModeH264_B;
  DXVA2_ModeH264_IDCT_NoFGT           = DXVA2_ModeH264_C;
  DXVA2_ModeH264_IDCT_FGT             = DXVA2_ModeH264_D;
  DXVA2_ModeH264_VLD_NoFGT            = DXVA2_ModeH264_E;
  DXVA2_ModeH264_VLD_FGT              = DXVA2_ModeH264_F;
  //Error codes
  DXVA2_E_NOT_INITIALIZED             = HRESULT($80041000);
  DXVA2_E_NEW_VIDEO_DEVICE            = HRESULT($80041001);
  DXVA2_E_VIDEO_DEVICE_LOCKED         = HRESULT($80041002);
  DXVA2_E_NOT_AVAILABLE               = HRESULT($80041003);

type
  //The following declarations within the 'if 0' block are dummy typedefs used to make
  //the evr.idl file build. The actual definitions are contained in d3d9.h

typedef struct _DXVA2_ExtendedFormat
    {
    union
        {
        struct
            {
            SampleFormat: UINT 8;
            VideoChromaSubsampling: UINT 4;
            NominalRange: UINT 3;
            VideoTransferMatrix: UINT 3;
            VideoLighting: UINT 4;
            VideoPrimaries: UINT 5;
            VideoTransferFunction: UINT 5;
            } 	;
        UINT value;
        } 	;
    } 	DXVA2_ExtendedFormat;

type
  _DXVA2_ExtendedFormat = (
    case Value of

    end;

  _DXVA2_SampleFormat                     = (
    DXVA2_SampleFormatMask                = $FF,
    DXVA2_SampleUnknown                   = 0,
    DXVA2_SampleProgressiveFrame          = 2,
    DXVA2_SampleFieldInterleavedEvenFirst = 3,
    DXVA2_SampleFieldInterleavedOddFirst  = 4,
    DXVA2_SampleFieldSingleEven           = 5,
    DXVA2_SampleFieldSingleOdd            = 6,
    DXVA2_SampleSubStream                 = 7
  );
  DXVA2_SampleFormat = _DXVA2_SampleFormat;


  _DXVA2_VideoChromaSubSampling                                 = (
    DXVA2_VideoChromaSubsamplingMask                            = $F,
    DXVA2_VideoChromaSubsampling_Unknown                        = 0,
    DXVA2_VideoChromaSubsampling_ProgressiveChroma              = $8,
    DXVA2_VideoChromaSubsampling_Horizontally_Cosited           = $4,
    DXVA2_VideoChromaSubsampling_Vertically_Cosited             = $2,
    DXVA2_VideoChromaSubsampling_Vertically_AlignedChromaPlanes = $1,
    DXVA2_VideoChromaSubsampling_MPEG2                          = (DXVA2_VideoChromaSubsampling_Horizontally_Cosited  or DXVA2_VideoChromaSubsampling_Vertically_AlignedChromaPlanes),
    DXVA2_VideoChromaSubsampling_MPEG1                          = DXVA2_VideoChromaSubsampling_Vertically_AlignedChromaPlanes,
    DXVA2_VideoChromaSubsampling_DV_PAL                         = (DXVA2_VideoChromaSubsampling_Horizontally_Cosited  or DXVA2_VideoChromaSubsampling_Vertically_Cosited),
    DXVA2_VideoChromaSubsampling_Cosited                        = ((DXVA2_VideoChromaSubsampling_Horizontally_Cosited  or DXVA2_VideoChromaSubsampling_Vertically_Cosited) or DXVA2_VideoChromaSubsampling_Vertically_AlignedChromaPlanes)
  );
  DXVA2_VideoChromaSubSampling = _DXVA2_VideoChromaSubSampling;


  _DXVA2_NominalRange          = (
    DXVA2_NominalRangeMask     = $7,
    DXVA2_NominalRange_Unknown = 0,
    DXVA2_NominalRange_Normal  = 1,
    DXVA2_NominalRange_Wide    = 2,
    DXVA2_NominalRange_0_255   = 1,
    DXVA2_NominalRange_16_235  = 2,
    DXVA2_NominalRange_48_208  = 3
  );
  DXVA2_NominalRange = _DXVA2_NominalRange;


  _DXVA2_VideoTransferMatrix            = (
    DXVA2_VideoTransferMatrixMask       = $7,
    DXVA2_VideoTransferMatrix_Unknown   = 0,
    DXVA2_VideoTransferMatrix_BT709     = 1,
    DXVA2_VideoTransferMatrix_BT601     = 2,
    DXVA2_VideoTransferMatrix_SMPTE240M = 3
  );
  DXVA2_VideoTransferMatrix = _DXVA2_VideoTransferMatrix;

  PDXVA2VideoLighting = ^TDXVA2VideoLighting;
  {$EXTERNALSYM _DXVA2_VideoLighting}
  _DXVA2_VideoLighting          = (
    DXVA2_VideoLightingMask     = $F,
    DXVA2_VideoLighting_Unknown = 0,
    DXVA2_VideoLighting_bright  = 1,
    DXVA2_VideoLighting_office  = 2,
    DXVA2_VideoLighting_dim     = 3,
    DXVA2_VideoLighting_dark    = 4
  );
  DXVA2_VideoLighting = _DXVA2_VideoLighting;


  _DXVA2_VideoPrimaries                = (
    DXVA2_VideoPrimariesMask           = $1F,
    DXVA2_VideoPrimaries_Unknown       = 0,
    DXVA2_VideoPrimaries_reserved      = 1,
    DXVA2_VideoPrimaries_BT709         = 2,
    DXVA2_VideoPrimaries_BT470_2_SysM  = 3,
    DXVA2_VideoPrimaries_BT470_2_SysBG = 4,
    DXVA2_VideoPrimaries_SMPTE170M     = 5,
    DXVA2_VideoPrimaries_SMPTE240M     = 6,
    DXVA2_VideoPrimaries_EBU3213       = 7,
    DXVA2_VideoPrimaries_SMPTE_C       = 8
  );
  DXVA2_VideoPrimaries = _DXVA2_VideoPrimaries;


  _DXVA2_VideoTransferFunction   = (
    DXVA2_VideoTransFuncMask     = $1F,
    DXVA2_VideoTransFunc_Unknown = 0,
    DXVA2_VideoTransFunc_10      = 1,
    DXVA2_VideoTransFunc_18      = 2,
    DXVA2_VideoTransFunc_20      = 3,
    DXVA2_VideoTransFunc_22      = 4,
    DXVA2_VideoTransFunc_709     = 5,
    DXVA2_VideoTransFunc_240M    = 6,
    DXVA2_VideoTransFunc_sRGB    = 7,
    DXVA2_VideoTransFunc_28      = 8
  );
  {$EXTERNALSYM DXVA2_VideoTransferFunction}
  DXVA2_VideoTransferFunction = _DXVA2_VideoTransferFunction;



  //////////////////////////////////////////////////////////////////////////////////////
  // Deprecated labels - please use the ones in the DXVA2_VideoTransferFunction enum. //
  //////////////////////////////////////////////////////////////////////////////////////

  _DXVA2_Frequency = record
    Numerator: UINT;
    Denominator: UINT;
  end;
  DXVA2_Frequency = _DXVA2_Frequency;


  PDXVA2VideoDesc = ^TDXVA2VideoDesc;
  {$EXTERNALSYM _DXVA2_VideoDesc}
  _DXVA2_VideoDesc = record
    SampleWidth: UINT;
    SampleHeight: UINT;
    SampleFormat: DXVA2_ExtendedFormat;
    Format: D3DFORMAT;
    InputSampleFreq: DXVA2_Frequency;
    OutputFrameFreq: DXVA2_Frequency;
    UABProtectionLevel: UINT;
    Reserved: UINT;
  end;
  DXVA2_VideoDesc = _DXVA2_VideoDesc;


  _dxva2api_0000_0000_0003        = (
    DXVA2_DeinterlaceTech_Unknown                = 0,
    DXVA2_DeinterlaceTech_BOBLineReplicate       = $1,
    DXVA2_DeinterlaceTech_BOBVerticalStretch     = $2,
    DXVA2_DeinterlaceTech_BOBVerticalStretch4Tap = $4,
    DXVA2_DeinterlaceTech_MedianFiltering        = $8,
    DXVA2_DeinterlaceTech_EdgeFiltering          = $10,
    DXVA2_DeinterlaceTech_FieldAdaptive          = $20,
    DXVA2_DeinterlaceTech_PixelAdaptive          = $40,
    DXVA2_DeinterlaceTech_MotionVectorSteered    = $80,
    DXVA2_DeinterlaceTech_InverseTelecine        = $100,
    DXVA2_DeinterlaceTech_Mask                   = $1FF
  );
  Dxva2api000000000003 = _dxva2api_0000_0000_0003;

  PDxva2api000000000004 = ^TDxva2api000000000004;
  dxva2api_0000_0000_0004 = (
    DXVA2_NoiseFilterLumaLevel        = 1,
    DXVA2_NoiseFilterLumaThreshold    = 2,
    DXVA2_NoiseFilterLumaRadius       = 3,
    DXVA2_NoiseFilterChromaLevel      = 4,
    DXVA2_NoiseFilterChromaThreshold  = 5,
    DXVA2_NoiseFilterChromaRadius     = 6,
    DXVA2_DetailFilterLumaLevel       = 7,
    DXVA2_DetailFilterLumaThreshold   = 8,
    DXVA2_DetailFilterLumaRadius      = 9,
    DXVA2_DetailFilterChromaLevel     = 10,
    DXVA2_DetailFilterChromaThreshold = 11,
    DXVA2_DetailFilterChromaRadius    = 12
  );
  Dxva2api000000000004 = _dxva2api_0000_0000_0004;


  _dxva2api_0000_0000_0005              = (
    DXVA2_NoiseFilterTech_Unsupported   = 0,
    DXVA2_NoiseFilterTech_Unknown       = $1,
    DXVA2_NoiseFilterTech_Median        = $2,
    DXVA2_NoiseFilterTech_Temporal      = $4,
    DXVA2_NoiseFilterTech_BlockNoise    = $8,
    DXVA2_NoiseFilterTech_MosquitoNoise = $10,
    DXVA2_NoiseFilterTech_Mask          = $1F
  );
  Dxva2api000000000005 = _dxva2api_0000_0000_0005;


  _dxva2api_0000_0000_0006             = (
    DXVA2_DetailFilterTech_Unsupported = 0,
    DXVA2_DetailFilterTech_Unknown     = $1,
    DXVA2_DetailFilterTech_Edge        = $2,
    DXVA2_DetailFilterTech_Sharpening  = $4,
    DXVA2_DetailFilterTech_Mask        = $7
  );
  Dxva2api000000000006 = _dxva2api_0000_0000_0006;


  _Dxva2api_0000_0000_0007   = (
    DXVA2_ProcAmp_None       = 0,
    DXVA2_ProcAmp_Brightness = $1,
    DXVA2_ProcAmp_Contrast   = $2,
    DXVA2_ProcAmp_Hue        = $4,
    DXVA2_ProcAmp_Saturation = $8,
    DXVA2_ProcAmp_Mask       = $F
  );
  Dxva2api000000000007 = _Dxva2api_0000_0000_0007;


  _dxva2api_0000_0000_0008                        = (
    DXVA2_VideoProcess_None                       = 0,
    DXVA2_VideoProcess_YUV2RGB                    = $1,
    DXVA2_VideoProcess_StretchX                   = $2,
    DXVA2_VideoProcess_StretchY                   = $4,
    DXVA2_VideoProcess_AlphaBlend                 = $8,
    DXVA2_VideoProcess_SubRects                   = $10,
    DXVA2_VideoProcess_SubStreams                 = $20,
    DXVA2_VideoProcess_SubStreamsExtended         = $40,
    DXVA2_VideoProcess_YUV2RGBExtended            = $80,
    DXVA2_VideoProcess_AlphaBlendExtended         = $100,
    DXVA2_VideoProcess_Constriction               = $200,
    DXVA2_VideoProcess_NoiseFilter                = $400,
    DXVA2_VideoProcess_DetailFilter               = $800,
    DXVA2_VideoProcess_PlanarAlpha                = $1000,
    DXVA2_VideoProcess_LinearScaling              = $2000,
    DXVA2_VideoProcess_GammaCompensated           = $4000,
    DXVA2_VideoProcess_MaintainsOriginalFieldData = $8000,
    DXVA2_VideoProcess_Mask                       = $FFFF
  );
  Dxva2api000000000008 = _dxva2api_0000_0000_0008;


  _dxva2api_0000_0000_0009     = (
    DXVA2_VPDev_HardwareDevice = $1,
    DXVA2_VPDev_EmulatedDXVA1  = $2,
    DXVA2_VPDev_SoftwareDevice = $4,
    DXVA2_VPDev_Mask           = $7
  );
  Dxva2api000000000009 = _dxva2api_0000_0000_0009;


  _dxva2api_0000_0000_0010           = (
    DXVA2_SampleData_RFF             = $1,
    DXVA2_SampleData_TFF             = $2,
    DXVA2_SampleData_RFF_TFF_Present = $4,
    DXVA2_SampleData_Mask            = $FFFF
  );
  Dxva2api000000000010 = _dxva2api_0000_0000_0010;


  _dxva2api_0000_0000_0011         = (
    DXVA2_DestData_RFF             = $1,
    DXVA2_DestData_TFF             = $2,
    DXVA2_DestData_RFF_TFF_Present = $4,
    DXVA2_DestData_Mask            = $FFFF
  );
  Dxva2api000000000011 = _dxva2api_0000_0000_0011;


  _DXVA2_VideoProcessorCaps = record
    DeviceCaps: UINT;
    InputPool: D3DPOOL;
    NumForwardRefSamples: UINT;
    NumBackwardRefSamples: UINT;
    Reserved: UINT;
    DeinterlaceTechnology: UINT;
    ProcAmpControlCaps: UINT;
    VideoProcessorOperations: UINT;
    NoiseFilterTechnology: UINT;
    DetailFilterTechnology: UINT;
  end;
  DXVA2_VideoProcessorCaps = _DXVA2_VideoProcessorCaps;


  DXVA2_Fixed32 = LONG;


  _DXVA2_AYUVSample8 = record
    Cr: UCHAR;
    Cb: UCHAR;
    Y: UCHAR;
    Alpha: UCHAR;
  end;
  DXVA2_AYUVSample8 = _DXVA2_AYUVSample8;


  _DXVA2_AYUVSample16 = record
    Cr: USHORT;
    Cb: USHORT;
    Y: USHORT;
    Alpha: USHORT;
  end;
  DXVA2_AYUVSample16 = _DXVA2_AYUVSample16;


  REFERENCE_TIME = LONGLONG;


  _DXVA2_ValueRange = record
    MinValue: DXVA2_Fixed32;
    MaxValue: DXVA2_Fixed32;
    DefaultValue: DXVA2_Fixed32;
    StepSize: DXVA2_Fixed32;
  end;
  DXVA2_ValueRange = _DXVA2_ValueRange;


  _DXVA2_ProcAmpValues = record
    Brightness: DXVA2_Fixed32;
    Contrast: DXVA2_Fixed32;
    Hue: DXVA2_Fixed32;
    Saturation: DXVA2_Fixed32;
  end;
  DXVA2_ProcAmpValues = _DXVA2_ProcAmpValues;


  _DXVA2_FilterValues = record
    Level: DXVA2_Fixed32;
    Threshold: DXVA2_Fixed32;
    Radius: DXVA2_Fixed32;
  end;
  DXVA2_FilterValues = _DXVA2_FilterValues;



  _DXVA2_VideoProcessBltParams = record
    TargetFrame: REFERENCE_TIME;
    TargetRect: RECT;
    ConstrictionSize: SIZE;
    StreamingFlags: UINT;
    BackgroundColor: DXVA2_AYUVSample16;
    DestFormat: DXVA2_ExtendedFormat;
    ProcAmpValues: DXVA2_ProcAmpValues;
    Alpha: DXVA2_Fixed32;
    NoiseFilterLuma: DXVA2_FilterValues;
    NoiseFilterChroma: DXVA2_FilterValues;
    DetailFilterLuma: DXVA2_FilterValues;
    DetailFilterChroma: DXVA2_FilterValues;
    DestData: DWORD;
  end;
  DXVA2_VideoProcessBltParams = _DXVA2_VideoProcessBltParams;



  _dxva2api_0000_0000_0012;                   = (
    DXVA2_PictureParametersBufferType         = 0,
    DXVA2_MacroBlockControlBufferType         = 1,
    DXVA2_ResidualDifferenceBufferType        = 2,
    DXVA2_DeblockingControlBufferType         = 3,
    DXVA2_InverseQuantizationMatrixBufferType = 4,
    DXVA2_SliceControlBufferType              = 5,
    DXVA2_BitStreamDateBufferType             = 6,
    DXVA2_MotionVectorBuffer                  = 7,
    DXVA2_FilmGrainBuffer                     = 8
  );
  Dxva2api_0000_0000_0012 = _dxva2api_0000_0000_0012;

  _dxva2api_0000_0000_0013           = (
    DXVA2_VideoDecoderRenderTarget   = 0,
    DXVA2_VideoProcessorRenderTarget = 1,
    DXVA2_VideoSoftwareRenderTarget  = 2
  );
  Dxva2api000000000013 = _dxva2api_0000_0000_0013;


  _DXVA2_ConfigPictureDecode = record
    guidConfigBitstreamEncryption: GUID;
    guidConfigMBcontrolEncryption: GUID;
    guidConfigResidDiffEncryption: GUID;
    ConfigBitstreamRaw: UINT;
    ConfigMBcontrolRasterOrder: UINT;
    ConfigResidDiffHost: UINT;
    ConfigSpatialResid8: UINT;
    ConfigResid8Subtraction: UINT;
    ConfigSpatialHost8or9Clipping: UINT;
    ConfigSpatialResidInterleaved: UINT;
    ConfigIntraResidUnsigned: UINT;
    ConfigResidDiffAccelerator: UINT;
    ConfigHostInverseScan: UINT;
    ConfigSpecificIDCT: UINT;
    Config4GroupedCoefs: UINT;
    ConfigMinRenderTargetBuffCount: USHORT;
    ConfigDecoderSpecific: USHORT;
  end;
  DXVA2_ConfigPictureDecode = _DXVA2_ConfigPictureDecode;


  _DXVA2_DecodeBufferDesc = record
    CompressedBufferType: DWORD;
    BufferIndex: UINT;
    DataOffset: UINT;
    DataSize: UINT;
    FirstMBaddress: UINT;
    NumMBsInBuffer: UINT;
    Width: UINT;
    Height: UINT;
    Stride: UINT;
    ReservedBits: UINT;
    pvPVPState: Pvoid;
  end;
  DXVA2_DecodeBufferDesc = _DXVA2_DecodeBufferDesc;

  //The value in pvPVPState depends on the type of crypo used.  For
  //D3DCRYPTOTYPE_AES128_CTR, pvPState points to the following structure:

  _DXVA2_AES_CTR_IV = record
    IV: UINT64;
    Count: UINT64;
  end;
  DXVA2_AES_CTR_IV = _DXVA2_AES_CTR_IV;

  _DXVA2_DecodeExtensionData = record
    Funktion: UINT;
    pPrivateInputData: Pvoid;
    PrivateInputDataSize: UINT;
    pPrivateOutputData: Pvoid;
    PrivateOutputDataSize: UINT;
  end;
  DXVA2_DecodeExtensionData = _DXVA2_DecodeExtensionData;


// DXVA2_DECODE_GET_DRIVER_HANDLE is an extension function that allows the
// driver to return a handle for the DXVA2 decode device that can be used to
// associate it with a IDirect3DCryptoSession9 interface.  When this function
// is used:
//     pPrivateInputData = Nil
//     pPrivateInputDataSize = 0
//     pPrivateOutputData = ^HANDLE
//     pPrivateOutputDataSize = SizeOf(PHANDLE)
//
// DXVA2_DECODE_SPECIFY_ENCRYPTED_BLOCKS is an extension function that that allows
// the decoder to specify which portions of the compressed buffers are encrypted.
// If this fucntion is not used to specify this information, it is assumed that
// the entire buffer is encrypted.
//     pPrivateInputData = ^D3DENCRYPTED_BLOCK_INFO;
//     PrivateInputDataSize = SizeOf(D3DENCRYPTED_BLOCK_INFO);
//     pPrivateOutputData = Nil;
//     PrivateOutputDataSize = 0;


  PDXVA2DecodeExecuteParams = ^TDXVA2DecodeExecuteParams;
  {$EXTERNALSYM _DXVA2_DecodeExecuteParams}
  _DXVA2_DecodeExecuteParams = record
    NumCompBuffers: UINT;
    pCompressedBuffers: PDXVA2_DecodeBufferDesc;
    pExtensionData: PDXVA2_DecodeExtensionData;
  end;
  {$EXTERNALSYM DXVA2_DecodeExecuteParams}
  DXVA2_DecodeExecuteParams = _DXVA2_DecodeExecuteParams;
  TDXVA2DecodeExecuteParams = _DXVA2_DecodeExecuteParams;


type
  //Forward Interfaces Declarations
  IDirect3DDeviceManager9 = interface;
  IDirectXVideoAccelerationService = interface;
  IDirectXVideoDecoderService = interface;
  IDirectXVideoProcessorService = interface;
  IDirectXVideoDecoder = interface;
  IDirectXVideoProcessor = interface;
  IDirectXVideoMemoryConfiguration = interface;

  // INTERFACES ////////////////////////////////////////////////////////////////

  //Interface IDirect3DDeviceManager9
  IDirect3DDeviceManager9 = interface(IUnknown)
  ['{a0cade0f-06d5-4cf4-a1c7-f3cdd725aa75}']
    function ResetDevice(const pDevice: IDirect3DDevice9; const resetToken: UINT): HResult; stdcall;
    function OpenDeviceHandle(out phDevice: HANDLE): HResult; stdcall;
    function CloseDeviceHandle(const hDevice: HANDLE): HResult; stdcall;
    function TestDevice(const hDevice: HANDLE): HResult; stdcall;
    function LockDevice(const hDevice: HANDLE; out ppDevice: PIDirect3DDevice9; const fBlock: BOOL): HResult; stdcall;
    function UnlockDevice(const hDevice: HANDLE; const fSaveState: BOOL): HResult; stdcall;
    function GetVideoService(const hDevice: HANDLE; const riid: REFIID; out ppService: Pvoid): HResult; stdcall;
  end;


  //Interface IDirectXVideoAccelerationService
  IDirectXVideoAccelerationService  = interface(IUnknown)
  ['{fc51a550-d5e7-11d9-af55-00054e43ff02}']
    function CreateSurface(const Width: UINT; const Height: UINT; const BackBuffers: UINT; const Format: D3DFORMAT;
                           const Pool: D3DPOOL; const Usage: DWORD; const DxvaType: DWORD; out ppSurface: PIDirect3DSurface9;
                           var pSharedHandle: HANDLE): HResult; stdcall;

  end;


  //Interface IDirectXVideoDecoderService
  IDirectXVideoDecoderService = interface(IUnknown)
  ['{fc51a551-d5e7-11d9-af55-00054e43ff02}']
    function GetDecoderDeviceGuids(out pCount: UINT; out pGuids: PGUID): HResult; stdcall;
    function GetDecoderRenderTargets(const Guid: REFGUID; out pCount: UINT; out pFormats: PD3DFORMAT): HResult; stdcall;
    function GetDecoderConfigurations(const Guid: REFGUID; const pVideoDesc: DXVA2_VideoDesc; const pReserved: Pointer;
                                      out pCount: UINT; out ppConfigs: PDXVA2_ConfigPictureDecode): HResult; stdcall;
    function CreateVideoDecoder(const Guid: REFGUID; const pVideoDesc: DXVA2_VideoDesc; const pConfig: DXVA2_ConfigPictureDecode; const ppDecoderRenderTargets: PIDirect3DSurface9;
                                const NumRenderTargets: UINT; out ppDecode: PIDirectXVideoDecoder): HResult; stdcall;
  end;


  //Interface IDirectXVideoProcessorService
  IDirectXVideoProcessorService = interface(IUnknown)
  ['{fc51a552-d5e7-11d9-af55-00054e43ff02}']
    function RegisterVideoProcessorSoftwareDevice(const pCallbacks: Pointer): HResult; stdcall;
    function GetVideoProcessorDeviceGuids(const pVideoDesc: DXVA2_VideoDesc; out pCount: UINT; out pGuids: PGUID): HResult; stdcall;
    function GetVideoProcessorRenderTargets(const VideoProcDeviceGuid: REFGUID; const pVideoDesc: DXVA2_VideoDesc; out pCount: UINT; out pFormats: PD3DFORMAT): HResult; stdcall;
    function GetVideoProcessorSubStreamFormats(const VideoProcDeviceGuid: REFGUID; const pVideoDesc: DXVA2_VideoDesc; const RenderTargetFormat: D3DFORMAT;
                                               out pCount: UINT; out pFormats: PD3DFORMAT): HResult; stdcall;
    function GetVideoProcessorCaps(const VideoProcDeviceGuid: REFGUID; const pVideoDesc: DXVA2_VideoDesc; const RenderTargetFormat: D3DFORMAT; out pCaps: DXVA2_VideoProcessorCaps): HResult; stdcall;
    function GetProcAmpRange(const VideoProcDeviceGuid: REFGUID; const pVideoDesc: DXVA2_VideoDesc; const RenderTargetFormat: D3DFORMAT; const ProcAmpCap: UINT; out pRange: DXVA2_ValueRange): HResult; stdcall;
    function GetFilterPropertyRange(const VideoProcDeviceGuid: REFGUID; const pVideoDesc: DXVA2_VideoDesc; const RenderTargetFormat: D3DFORMAT;
                                    const FilterSetting: UINT; out pRange: DXVA2_ValueRange): HResult; stdcall;
    function CreateVideoProcessor(const VideoProcDeviceGuid: REFGUID; const pVideoDesc: DXVA2_VideoDesc; const RenderTargetFormat: D3DFORMAT;
                                  const MaxNumSubStreams: UINT; out ppVidProcess: PIDirectXVideoProcessor): HResult; stdcall;
  end;


  //Interface IDirectXVideoDecoder
  IDirectXVideoDecoder = interface(IUnknown)
  ['{f2b0810a-fd00-43c9-918c-df94e2d8ef7d}']
    function GetVideoDecoderService(out ppService: PIDirectXVideoDecoderService): HResult; stdcall;
    function GetCreationParameters(out pDeviceGuid: GUID; out pVideoDesc: DXVA2_VideoDesc; out pConfig: DXVA2_ConfigPictureDecode;
                                   out pDecoderRenderTargets: PPIDirect3DSurface9; out pNumSurfaces: UINT): HResult; stdcall;
    function GetBuffer(const BufferType: UINT; out ppBuffer: Pointer; out pBufferSize: UINT): HResult; stdcall;
    function ReleaseBuffer(const BufferType: UINT): HResult; stdcall;
    function BeginFrame(const pRenderTarget: IDirect3DSurface9; const pvPVPData: void): HResult; stdcall;
    function EndFrame(out pHandleComplete: HANDLE): HResult; stdcall;
    function Execute(const pExecuteParams: DXVA2_DecodeExecuteParams): HResult; stdcall;
  end;


  //Interface IDirectXVideoProcessor
  IDirectXVideoProcessor = interface(IUnknown)
  ['{8c3a39f0-916e-4690-804f-4c8001355d25}']
    function GetVideoProcessorService(out ppService: PIDirectXVideoProcessorService): HResult; stdcall;
    function GetCreationParameters(out pDeviceGuid: GUID; out pVideoDesc: DXVA2_VideoDesc; out pRenderTargetFormat: D3DFORMAT; out pMaxNumSubStreams: UINT): HResult; stdcall;
    function GetVideoProcessorCaps(out pCaps: DXVA2_VideoProcessorCaps): HResult; stdcall;
    function GetProcAmpRange(const ProcAmpCap: UINT; out pRange: DXVA2_ValueRange): HResult; stdcall;
    function GetFilterPropertyRange(const FilterSetting: UINT; out DXVA2_ValueRange): HResult; stdcall;
    function VideoProcessBlt(const pRenderTarget: IDirect3DSurface9; const pBltParams: DXVA2_VideoProcessBltParams; const pSamples: DXVA2_VideoSample;
                             const NumSamples: UINT; out pHandleComplete: HANDLE): HResult; stdcall;                                ;

  end;


  //Interface IDirectXVideoMemoryConfiguration
  IDirectXVideoMemoryConfiguration = interface(IUnknown)
  ['{b7f916dd-db3b-49c1-84d7-e45ef99ec726}']
    function GetAvailableSurfaceTypeByIndex(const dwTypeIndex: DWORD; out pdwType: DXVA2_SurfaceType): HResult; stdcall;
    function SetSurfaceType(const dwType: DXVA2_SurfaceType): HResult; stdcall;
  end;



type
__inline DXVA2_Fixed32 DXVA2FloatToFixed(__in const float _float_)
{
    DXVA2_Fixed32 _fixed_;
    _fixed_.Fraction = LOWORD(_float_ * 0x10000);
    _fixed_.Value = HIWORD(_float_ * 0x10000);
    return _fixed_;
}
__inline float DXVA2FixedToFloat(__in const DXVA2_Fixed32 _fixed_)
{
    return (FLOAT)_fixed_.Value + (FLOAT)_fixed_.Fraction / 0x10000;
}
__inline const DXVA2_Fixed32 DXVA2_Fixed32TransparentAlpha()
{
    DXVA2_Fixed32 _fixed_ = {0, 0};
    return _fixed_;
}
__inline const DXVA2_Fixed32 DXVA2_Fixed32OpaqueAlpha()
{
    DXVA2_Fixed32 _fixed_ = {0, 1};
    return _fixed_;
}


//Additional Prototypes for ALL interfaces

procedure GetDXVA2ExtendedFormatFromMFMediaType(var pType: IMFMediaType; var pFormat: DXVA2_ExtendedFormat);

//end of Additional Prototypes


implementation


// Fills in the DXVA2_ExtendedFormat structure.
// uses Mfapi.pas
procedure GetDXVA2ExtendedFormatFromMFMediaType(var pType: IMFMediaType; var pFormat: DXVA2_ExtendedFormat);
var
  interlace: MFVideoInterlaceMode;

begin

    // Get the interlace mode.
    interlace:= MFVideoInterlaceMode(MFGetAttributeUINT32(pType, MF_MT_INTERLACE_MODE, MFVideoInterlace_Unknown));

    // The values for interlace mode translate directly, except for mixed
    // interlace or progressive mode.

    if (interlace = MFVideoInterlace_MixedInterlaceOrProgressive) then
      begin    // Default to interleaved fields.
        pFormat.SampleFormat:= DXVA2_SampleFieldInterleavedEvenFirst;
      end
    else
      begin
        pFormat.SampleFormat:= UINT(interlace);
      end;

    // The remaining values translate directly.
    // Use the "no-fail" attribute functions and default to "unknown."
    pFormat.VideoChromaSubsampling = MFGetAttributeUINT32(pType, MF_MT_VIDEO_CHROMA_SITING, MFVideoChromaSubsampling_Unknown);
    pFormat.NominalRange = MFGetAttributeUINT32(pType, MF_MT_VIDEO_NOMINAL_RANGE, MFNominalRange_Unknown);
    pFormat.VideoTransferMatrix = MFGetAttributeUINT32(pType, MF_MT_YUV_MATRIX, MFVideoTransferMatrix_Unknown);
    pFormat.VideoLighting = MFGetAttributeUINT32(pType, MF_MT_VIDEO_LIGHTING, MFVideoLighting_Unknown);
    pFormat.VideoPrimaries = MFGetAttributeUINT32(pType, MF_MT_VIDEO_PRIMARIES, MFVideoPrimaries_Unknown);
    pFormat.VideoTransferFunction = MFGetAttributeUINT32(pType, MF_MT_TRANSFER_FUNCTION, MFVideoTransFunc_Unknown);

end;

end.
