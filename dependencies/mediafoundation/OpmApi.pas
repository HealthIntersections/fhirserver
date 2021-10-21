// FactoryX
// factoryx.code@gmail.com
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: OpmApi.pas
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
// EditDate: updt 260712b
//
// Remarks:
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: General check
// =============================================================================
// Source: opmapi.h
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


unit OpmApi;

interface

uses
  Windows, ComObj, {Ole2,} UnKnwn, DxVa2Api;

const
  //#if defined( _WIN32 ) && !defined( _NO_COM )
  IID_IOPMVideoOutput                            : TGUID = '{0A15159D-41C7-4456-93E1-284CD61D4E8D}';
  //#endif // defined( _WIN32 ) && !defined( _NO_COM )


const
  OPM_GET_CURRENT_HDCP_SRM_VERSION               : TGUID = '{99c5ceff-5f1d-4879-81c1-c52443c9482b}';
  OPM_GET_CONNECTED_HDCP_DEVICE_INFORMATION      : TGUID = '{0db59d74-a992-492e-a0bd-c23fda564e00}';
  OPM_GET_ACP_AND_CGMSA_SIGNALING                : TGUID = '{6629a591-3b79-4cf3-924a-11e8e7811671}';
  OPM_GET_CONNECTOR_TYPE                         : TGUID = '{81d0bfd5-6afe-48c2-99c0-95a08f97c5da}';
  OPM_GET_SUPPORTED_PROTECTION_TYPES             : TGUID = '{38f2a801-9a6c-48bb-9107-b6696e6f1797}';
  OPM_GET_VIRTUAL_PROTECTION_LEVEL               : TGUID = '{b2075857-3eda-4d5d-88db-748f8c1a0549}';
  OPM_GET_ACTUAL_PROTECTION_LEVEL                : TGUID = '{1957210a-7766-452a-b99a-d27aed54f03a}';
  OPM_GET_ACTUAL_OUTPUT_FORMAT                   : TGUID = '{d7bf1ba3-ad13-4f8e-af98-0dcb3ca204cc}';
  OPM_GET_ADAPTER_BUS_TYPE                       : TGUID = '{c6f4d673-6174-4184-8e35-f6db5200bcba}';
  OPM_GET_OUTPUT_ID                              : TGUID = '{72cb6df3-244f-40ce-b09e-20506af6302f}';
  OPM_GET_DVI_CHARACTERISTICS                    : TGUID = '{a470b3bb-5dd7-4172-839c-3d3776e0ebf5}';
  OPM_GET_CODEC_INFO                             : TGUID = '{4f374491-8f5f-4445-9dba-95588f6b58b4}';
  OPM_SET_PROTECTION_LEVEL                       : TGUID = '{9bb9327c-4eb5-4727-9f00-b42b0919c0da}';
  OPM_SET_ACP_AND_CGMSA_SIGNALING                : TGUID = '{09a631a5-d684-4c60-8e4d-d3bb0f0be3ee}';
  OPM_SET_HDCP_SRM                               : TGUID = '{8b5ef5d1-c30d-44ff-84a5-ea71dce78f13}';
  OPM_SET_PROTECTION_LEVEL_ACCORDING_TO_CSS_DVD  : TGUID = '{39ce333e-4cc0-44ae-bfcc-da50b5f82e72}';

  KSPROPSETID_OPMVideoOutput                     : TGUID = '{06F414BB-F43A-4fe2-A566-774B4C81F0DB}';



type
  _opmapi_0000_0000_0001                         = (
    OPM_OMAC_SIZE                                = 16,
    OPM_128_BIT_RANDOM_NUMBER_SIZE               = 16,
    OPM_ENCRYPTED_INITIALIZATION_PARAMETERS_SIZE = 256,
    OPM_CONFIGURE_SETTING_DATA_SIZE              = 4056,
    OPM_GET_INFORMATION_PARAMETERS_SIZE          = 4056,
    OPM_REQUESTED_INFORMATION_SIZE               = 4076,
    OPM_HDCP_KEY_SELECTION_VECTOR_SIZE           = 5,
    OPM_PROTECTION_TYPE_SIZE                     = 4,
    OPM_BUS_TYPE_MASK                            = $FFFF,
    OPM_BUS_IMPLEMENTATION_MODIFIER_MASK         = $7FFF
  );
  opmapi_0000_0000_0001 = _opmapi_0000_0000_0001;

type
  _OPM_VIDEO_OUTPUT_SEMANTICS = (
    OPM_VOS_COPP_SEMANTICS = 0,
    OPM_VOS_OPM_SEMANTICS  = 1
  );
  OPM_VIDEO_OUTPUT_SEMANTICS = _OPM_VIDEO_OUTPUT_SEMANTICS;



  _opmapi_0000_0000_0002 = (
    OPM_HDCP_FLAG_NONE     = 0,
    OPM_HDCP_FLAG_REPEATER = $1
  );
  opmapi_0000_0000_0002 = _opmapi_0000_0000_0002;

type
  _opmapi_0000_0000_0003                  = (
    OPM_STATUS_NORMAL                       = 0,
    OPM_STATUS_LINK_LOST                    = $1,
    OPM_STATUS_RENEGOTIATION_REQUIRED       = $2,
    OPM_STATUS_TAMPERING_DETECTED           = $4,
    OPM_STATUS_REVOKED_HDCP_DEVICE_ATTACHED = $8
  );
  opmapi_0000_0000_0003 = __opmapi_0000_0000_0003;

type
  _opmapi_0000_0000_0004                      = (
    OPM_CONNECTOR_TYPE_OTHER                    =  - 1,
    OPM_CONNECTOR_TYPE_VGA                      = 0,
    OPM_CONNECTOR_TYPE_SVIDEO                   = 1,
    OPM_CONNECTOR_TYPE_COMPOSITE_VIDEO          = 2,
    OPM_CONNECTOR_TYPE_COMPONENT_VIDEO          = 3,
    OPM_CONNECTOR_TYPE_DVI                      = 4,
    OPM_CONNECTOR_TYPE_HDMI                     = 5,
    OPM_CONNECTOR_TYPE_LVDS                     = 6,
    OPM_CONNECTOR_TYPE_D_JPN                    = 8,
    OPM_CONNECTOR_TYPE_SDI                      = 9,
    OPM_CONNECTOR_TYPE_DISPLAYPORT_EXTERNAL     = 10,
    OPM_CONNECTOR_TYPE_DISPLAYPORT_EMBEDDED     = 11,
    OPM_CONNECTOR_TYPE_UDI_EXTERNAL             = 12,
    OPM_CONNECTOR_TYPE_UDI_EMBEDDED             = 13,
    OPM_COPP_COMPATIBLE_CONNECTOR_TYPE_INTERNAL = $80000000
  );
  opmapi_0000_0000_0004 = _opmapi_0000_0000_0004;

type
  _opmapi_0000_0000_0005 = (
    OPM_DVI_CHARACTERISTIC_1_0          = 1,
    OPM_DVI_CHARACTERISTIC_1_1_OR_ABOVE = 2
  );
  itf_opmapi_0000_0000_0005 = _opmapi_0000_0000_0005;

type

  _opmapi_0000_0000_0006                                                    = (
    OPM_BUS_TYPE_OTHER                                                      = 0,
    OPM_BUS_TYPE_PCI                                                        = $1,
    OPM_BUS_TYPE_PCIX                                                       = $2,
    OPM_BUS_TYPE_PCIEXPRESS                                                 = $3,
    OPM_BUS_TYPE_AGP                                                        = $4,
    OPM_BUS_IMPLEMENTATION_MODIFIER_INSIDE_OF_CHIPSET                       = $10000,
    OPM_BUS_IMPLEMENTATION_MODIFIER_TRACKS_ON_MOTHER_BOARD_TO_CHIP          = $20000,
    OPM_BUS_IMPLEMENTATION_MODIFIER_TRACKS_ON_MOTHER_BOARD_TO_SOCKET        = $30000,
    OPM_BUS_IMPLEMENTATION_MODIFIER_DAUGHTER_BOARD_CONNECTOR                = $40000,
    OPM_BUS_IMPLEMENTATION_MODIFIER_DAUGHTER_BOARD_CONNECTOR_INSIDE_OF_NUAE = $50000,
    OPM_BUS_IMPLEMENTATION_MODIFIER_NON_STANDARD                            = $80000000,
    OPM_COPP_COMPATIBLE_BUS_TYPE_INTEGRATED                                 = $80000000
  );
  opmapi_0000_0000_0006 = _opmapi_0000_0000_0006;

type
  _OPM_DPCP_PROTECTION_LEVEL = (
    OPM_DPCP_OFF         = 0,
    OPM_DPCP_ON          = 1,
    OPM_DPCP_FORCE_ULONG = $7FFFFFFF
  );
  OPM_DPCP_PROTECTION_LEVEL = _OPM_DPCP_PROTECTION_LEVEL;
  TOpmDpcpProtectionLevel = _OPM_DPCP_PROTECTION_LEVEL;

type
  _OPM_HDCP_PROTECTION_LEVEL = (
    OPM_HDCP_OFF         = 0,
    OPM_HDCP_ON          = 1,
    OPM_HDCP_FORCE_ULONG = $7FFFFFFF
  );
  OPM_HDCP_PROTECTION_LEVEL = _OPM_HDCP_PROTECTION_LEVEL;


type
  _opmapi_0000_0000_0007     = (
    OPM_CGMSA_OFF                             = 0,
    OPM_CGMSA_COPY_FREELY                     = $1,
    OPM_CGMSA_COPY_NO_MORE                    = $2,
    OPM_CGMSA_COPY_ONE_GENERATION             = $3,
    OPM_CGMSA_COPY_NEVER                      = $4,
    OPM_CGMSA_REDISTRIBUTION_CONTROL_REQUIRED = $8
  );
  opmapi_0000_0000_0007 = _opmapi_0000_0000_0007;


type
  _OPM_ACP_PROTECTION_LEVEL = (
    OPM_ACP_OFF         = 0,
    OPM_ACP_LEVEL_ONE   = 1,
    OPM_ACP_LEVEL_TWO   = 2,
    OPM_ACP_LEVEL_THREE = 3,
    OPM_ACP_FORCE_ULONG = $7FFFFFFF
  );
  OPM_ACP_PROTECTION_LEVEL = _OPM_ACP_PROTECTION_LEVEL;


type
  _opmapi_0000_0000_0008                     = (
    OPM_PROTECTION_TYPE_OTHER                = $80000000,
    OPM_PROTECTION_TYPE_NONE                 = 0,
    OPM_PROTECTION_TYPE_COPP_COMPATIBLE_HDCP = $1,
    OPM_PROTECTION_TYPE_ACP                  = $2,
    OPM_PROTECTION_TYPE_CGMSA                = $4,
    OPM_PROTECTION_TYPE_HDCP                 = $8,
    OPM_PROTECTION_TYPE_DPCP                 = $10
  );
  opmapi_0000_0000_0008 = _opmapi_0000_0000_0008;

type
  _opmapi_0000_0000_0009                        = (
    OPM_PROTECTION_STANDARD_OTHER               = $80000000,
    OPM_PROTECTION_STANDARD_NONE                = 0,
    OPM_PROTECTION_STANDARD_IEC61880_525I       = $1,
    OPM_PROTECTION_STANDARD_IEC61880_2_525I     = $2,
    OPM_PROTECTION_STANDARD_IEC62375_625P       = $4,
    OPM_PROTECTION_STANDARD_EIA608B_525         = $8,
    OPM_PROTECTION_STANDARD_EN300294_625I       = $10,
    OPM_PROTECTION_STANDARD_CEA805A_TYPEA_525P  = $20,
    OPM_PROTECTION_STANDARD_CEA805A_TYPEA_750P  = $40,
    OPM_PROTECTION_STANDARD_CEA805A_TYPEA_1125I = $80,
    OPM_PROTECTION_STANDARD_CEA805A_TYPEB_525P  = $100,
    OPM_PROTECTION_STANDARD_CEA805A_TYPEB_750P  = $200,
    OPM_PROTECTION_STANDARD_CEA805A_TYPEB_1125I = $400,
    OPM_PROTECTION_STANDARD_ARIBTRB15_525I      = $800,
    OPM_PROTECTION_STANDARD_ARIBTRB15_525P      = $1000,
    OPM_PROTECTION_STANDARD_ARIBTRB15_750P      = $2000,
    OPM_PROTECTION_STANDARD_ARIBTRB15_1125I     = $4000
  );
  opmapi_0000_0000_0009 = _opmapi_0000_0000_0009;


type
  _OPM_IMAGE_ASPECT_RATIO_EN300294                                = (
    OPM_ASPECT_RATIO_EN300294_FULL_FORMAT_4_BY_3                  = 0,
    OPM_ASPECT_RATIO_EN300294_BOX_14_BY_9_CENTER                  = 1,
    OPM_ASPECT_RATIO_EN300294_BOX_14_BY_9_TOP                     = 2,
    OPM_ASPECT_RATIO_EN300294_BOX_16_BY_9_CENTER                  = 3,
    OPM_ASPECT_RATIO_EN300294_BOX_16_BY_9_TOP                     = 4,
    OPM_ASPECT_RATIO_EN300294_BOX_GT_16_BY_9_CENTER               = 5,
    OPM_ASPECT_RATIO_EN300294_FULL_FORMAT_4_BY_3_PROTECTED_CENTER = 6,
    OPM_ASPECT_RATIO_EN300294_FULL_FORMAT_16_BY_9_ANAMORPHIC      = 7,
    OPM_ASPECT_RATIO_FORCE_ULONG                                  = $7FFFFFFF
  );
  OPM_IMAGE_ASPECT_RATIO_EN300294 = _OPM_IMAGE_ASPECT_RATIO_EN300294;


type
  _OPM_RANDOM_NUMBER = record
    abRandomNumber: array[0..15] of Byte;
  end;
  OPM_RANDOM_NUMBER = _OPM_RANDOM_NUMBER;


type
  _OPM_OMAC = record
    abOMAC: array[0..15] of Byte;
  end;
  OPM_OMAC = _OPM_OMAC;


type
  _OPM_ENCRYPTED_INITIALIZATION_PARAMETERS = record
    abEncryptedInitializationParameters: array[0..255] of Byte;
  end;
  OPM_ENCRYPTED_INITIALIZATION_PARAMETERS = _OPM_ENCRYPTED_INITIALIZATION_PARAMETERS;


type
  _OPM_GET_INFO_PARAMETERS = record
    omac: OPM_OMAC;
    rnRandomNumber: OPM_RANDOM_NUMBER;
    guidInformation: TGUID;
    ulSequenceNumber: ULONG;
    cbParametersSize: ULONG;
    abParameters: array[0..4055] of Byte;
  end;
  OPM_GET_INFO_PARAMETERS = _OPM_GET_INFO_PARAMETERS;


type
  _OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    guidInformation: TGUID;
    ulSequenceNumber: ULONG;
    cbParametersSize: ULONG;
    abParameters: array[0..4055] of Byte;
  end;
  OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS = _OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS;


type
  _OPM_HDCP_KEY_SELECTION_VECTOR = record
    abKeySelectionVector: array[0..4] of Byte;
  end;
  OPM_HDCP_KEY_SELECTION_VECTOR = _OPM_HDCP_KEY_SELECTION_VECTOR;


type
  _OPM_CONNECTED_HDCP_DEVICE_INFORMATION = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    ulHDCPFlags: ULONG;
    ksvB: OPM_HDCP_KEY_SELECTION_VECTOR;
    Reserved: array[0..10] of Byte;
    Reserved2: array[0..15] of Byte;
    Reserved3: array[0..15] of Byte;
  end;
  OPM_CONNECTED_HDCP_DEVICE_INFORMATION = _OPM_CONNECTED_HDCP_DEVICE_INFORMATION;

type
  _OPM_REQUESTED_INFORMATION = record
    omac: OPM_OMAC;
    cbRequestedInformationSize: ULONG;
    abRequestedInformation: array[0..4075] of Byte;
  end;
  OPM_REQUESTED_INFORMATION = _OPM_REQUESTED_INFORMATION;

type
  _OPM_STANDARD_INFORMATION = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    ulInformation: ULONG;
    ulReserved: ULONG;
    ulReserved2: ULONG;
  end;
  OPM_STANDARD_INFORMATION = _OPM_STANDARD_INFORMATION;

{$IFNDEF DO_NOT_USE_DIRECTX_OR_DXVA2}
type
  _OPM_ACTUAL_OUTPUT_FORMAT = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    ulDisplayWidth: ULONG;
    ulDisplayHeight: ULONG;
    dsfSampleInterleaveFormat: DXVA2_SampleFormat;
    d3dFormat: D3DFORMAT;
    ulFrequencyNumerator: ULONG;
    ulFrequencyDenominator: ULONG;
  end;
  OPM_ACTUAL_OUTPUT_FORMAT = _OPM_ACTUAL_OUTPUT_FORMAT;
{$ENDIF} // DO_NOT_USE_DIRECTX_OR_DXVA2

type
  _OPM_ACP_AND_CGMSA_SIGNALING = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    ulAvailableTVProtectionStandards: ULONG;
    ulActiveTVProtectionStandard: ULONG;
    ulReserved: ULONG;
    ulAspectRatioValidMask1: ULONG;
    ulAspectRatioData1: ULONG;
    ulAspectRatioValidMask2: ULONG;
    ulAspectRatioData2: ULONG;
    ulAspectRatioValidMask3: ULONG;
    ulAspectRatioData3: ULONG;
    ulReserved2: array[0..3] of ULONG;
    ulReserved3: array[0..3] of ULONG;
  end;
  OPM_ACP_AND_CGMSA_SIGNALING = _OPM_ACP_AND_CGMSA_SIGNALING;

type
  _OPM_OUTPUT_ID_DATA = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    ulStatusFlags: ULONG;
    OutputId: UINT64;
  end;
  OPM_OUTPUT_ID_DATA = _OPM_OUTPUT_ID_DATA;

type
  _OPM_CONFIGURE_PARAMETERS = record
    omac: OPM_OMAC;
    guidSetting: TGUID;
    ulSequenceNumber: ULONG;
    cbParametersSize: ULONG;
    abParameters: array[0..4055] of Byte;
  end;
  OPM_CONFIGURE_PARAMETERS = _OPM_CONFIGURE_PARAMETERS;

type
  _OPM_SET_PROTECTION_LEVEL_PARAMETERS = record
    ulProtectionType: ULONG;
    ulProtectionLevel: ULONG;
    Reserved: ULONG;
    Reserved2: ULONG;
  end;
  OPM_SET_PROTECTION_LEVEL_PARAMETERS = _OPM_SET_PROTECTION_LEVEL_PARAMETERS;

type
  _OPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS = record
    ulNewTVProtectionStandard: ULONG;
    ulAspectRatioChangeMask1: ULONG;
    ulAspectRatioData1: ULONG;
    ulAspectRatioChangeMask2: ULONG;
    ulAspectRatioData2: ULONG;
    ulAspectRatioChangeMask3: ULONG;
    ulAspectRatioData3: ULONG;
    ulReserved: array[0..3] of ULONG;
    ulReserved2: array[0..3] of ULONG;
    ulReserved3: ULONG;
  end;
  OPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS = _OPM_SET_ACP_AND_CGMSA_SIGNALING_PARAMETERS;

type
  _OPM_SET_HDCP_SRM_PARAMETERS = record
    ulSRMVersion: ULONG;
  end;
  OPM_SET_HDCP_SRM_PARAMETERS = _OPM_SET_HDCP_SRM_PARAMETERS;

type
  _OPM_GET_CODEC_INFO_PARAMETERS = record
    cbVerifier: DWORD;
    Verifier: array[0..4051] of Byte;
  end;
  OPM_GET_CODEC_INFO_PARAMETERS = _OPM_GET_CODEC_INFO_PARAMETERS;

type
  _OPM_GET_CODEC_INFO_INFORMATION = record
    rnRandomNumber: OPM_RANDOM_NUMBER;
    Merit: DWORD;
  end;
  OPM_GET_CODEC_INFO_INFORMATION = _OPM_GET_CODEC_INFO_INFORMATION;


type
  //Forward Interface Declarations

  IOPMVideoOutput = interface;

  // INTERFACES ////////////////////////////////////////////////////////////////

  //Interface IOPMVideoOutput
  IEVRVideoStreamControl = interface(IUnknown)
  ['{0A15159D-41C7-4456-93E1-284CD61D4E8D}']
    function StartInitialization(out prnRandomNumber: OPM_RANDOM_NUMBER; out ppbCertificate: PByte; out pulCertificateLength: ULONG): HResult; stdcall;
    function FinishInitialization(const pParameters: OPM_ENCRYPTED_INITIALIZATION_PARAMETERS): HResult; stdcall;
    function GetInformation(const pParameters: OPM_GET_INFO_PARAMETERS; out pRequestedInformation: OPM_REQUESTED_INFORMATION): HResult; stdcall;
    function COPPCompatibleGetInformation(const pParameters: OPM_COPP_COMPATIBLE_GET_INFO_PARAMETERS; out pRequestedInformation: OPM_REQUESTED_INFORMATION): HResult; stdcall;
    function Configure(const pParameters: OPM_CONFIGURE_PARAMETERS; const ulAdditionalParametersSize: ULONG; const pbAdditionalParameters: PByte): HResult; stdcall;
  end;


type
  cwKSMETHOD_OPMVIDEOOUTPUT                      = (
    //  Output is OPM_RANDOM_NUMBER followed by certifiate
    KSMETHOD_OPMVIDEOOUTPUT_STARTINITIALIZATION  = 0,
    //  Input OPM_ENCRYPTED_INITIALIZATION_PARAMETERS
    //  Output OPM_STANDARD_INFORMATION
    KSMETHOD_OPMVIDEOOUTPUT_FINISHINITIALIZATION = 1,
    //  Input is OPM_GET_INFO_PARAMETERS, output is OPM_REQUESTED_INFORMATION
    //  Use KsMethod - both input and output in the buffer (not after the KSMETHOD structure)
    KSMETHOD_OPMVIDEOOUTPUT_GETINFORMATION       = 2
  );
  KSMETHOD_OPMVIDEOOUTPUT = cwKSMETHOD_OPMVIDEOOUTPUT;


  //public
  function OPMGetVideoOutputsFromHMONITOR(const hMonitor: HMONITOR; const OPM_VIDEO_OUTPUT_SEMANTICS;
                                        out pulNumVideoOutputs: ULONG; out pppOPMVideoOutputArray: PPIOPMVideoOutput): HResult; stdcall;

{$IFNDEF DO_NOT_USE_DIRECTX_OR_DXVA2}
  //public
  function OPMGetVideoOutputsFromIDirect3DDevice9Object(const pDirect3DDevice9: IDirect3DDevice9; const OPM_VIDEO_OUTPUT_SEMANTICS;
                                                      out pulNumVideoOutputs: ULONG; out pppOPMVideoOutputArray: PPIOPMVideoOutput): HResult; stdcall;
{$ENDIF} // DO_NOT_USE_DIRECTX_OR_DXVA2

  //private
  function GetBusType(ulBusTypeAndImplementation: ULONG): ULONG;
  function GetBusImplementation(ulBusTypeAndImplementation: ULONG): ULONG;
  function IsNonStandardBusImplementation(ulBusTypeAndImplementation: ULONG): ULONG;

  //Additional Prototypes for ALL interfaces

  //end of Additional Prototypes


implementation

//private
function GetBusType(ulBusTypeAndImplementation: ULONG): ULONG;
begin
  Result:= ulBusTypeAndImplementation and OPM_BUS_TYPE_MASK);
end;

function GetBusImplementation(ulBusTypeAndImplementation: ULONG): ULONG;
begin
  Result:= ulBusTypeAndImplementation and OPM_BUS_IMPLEMENTATION_MODIFIER_MASK shr 16;
end;

function IsNonStandardBusImplementation(ulBusTypeAndImplementation: ULONG): ULONG;
begin
  Result:= ulBusTypeAndImplementation and OPM_BUS_IMPLEMENTATION_MODIFIER_NON_STANDARD;
end;

end.
