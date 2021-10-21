// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - MFTransform.pas
// Kind: Pascal Unit
// Release date: 11-07-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
// LastEdited by: Tony
// EditDate: updt 080712a
//----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person               Reason
// ---------- -------------------- --------------------------------------------
//updt 280812 Peter Larson         Remove txx and pxx interface definitions and
//                                 minor bug fixes, remove pointers to interfaces,
//                                 remove Ole2
//----------------------------------------------------------------------------
//
// Remarks:   Delphi : The IUnknown entries of functions should be casted like this:
//            IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: Overall Check
// =============================================================================
// Source: mftransform.h
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

unit MFTransform;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}

interface

uses
  Windows,{ ComObj,} MFObjects, ActiveX, PropSys;

type
  GUID =                tGUID;

// Found in BaseTsd.h:
//#define MAXULONGLONG ((ULONGLONG)~((ULONGLONG)0))
//#define MINLONGLONG ((LONGLONG)~MAXLONGLONG)
Const
  Version               = '0.1.0001';
  MAXLONGLONG =         9223372036854775807;
  MINLONGLONG =         -9223372036854775808;

  //Forward Declarations
const
  IID_IMFTransform                     : TGUID = '{bf94c121-5b05-4e6f-8000-ba598961414d}';


const
    MFT_INPUT_DATA_BUFFER_PLACEHOLDER	= $ffffffff;

type
//  PIMFTransform = ^TIMFTransform;
//  {$EXTERNALSYM IMFTransform}
//  IMFTransform = interface;      //###### DOES NOT COMPILE IF THIS not commented out
//  TIMFTransform = interface;

 {$EXTERNALSYM _MFT_INPUT_DATA_BUFFER_FLAGS}

//  MFT_INPUT_DATA_BUFFER_FLAGS = _MFT_INPUT_DATA_BUFFER_FLAGS;

  {$EXTERNALSYM _MFT_OUTPUT_DATA_BUFFER_FLAGS}
  _MFT_OUTPUT_DATA_BUFFER_FLAGS = (
    MFT_OUTPUT_DATA_BUFFER_INCOMPLETE	= $1000000,
	  MFT_OUTPUT_DATA_BUFFER_FORMAT_CHANGE	= $100,
	  MFT_OUTPUT_DATA_BUFFER_STREAM_END	= $200,
	  MFT_OUTPUT_DATA_BUFFER_NO_SAMPLE	= $300
    );
  MFT_OUTPUT_DATA_BUFFER_FLAGS = _MFT_OUTPUT_DATA_BUFFER_FLAGS;

  {$EXTERNALSYM _MFT_INPUT_STATUS_FLAGS}
  _MFT_INPUT_STATUS_FLAGS = (
    MFT_INPUT_STATUS_ACCEPT_DATA	= $1
    );
  MFT_INPUT_STATUS_FLAGS = _MFT_INPUT_STATUS_FLAGS;

type
  {$EXTERNALSYM _MFT_OUTPUT_STATUS_FLAGS}
  _MFT_OUTPUT_STATUS_FLAGS = (
    MFT_OUTPUT_STATUS_SAMPLE_READY	= $1
    );

type
  {$EXTERNALSYM _MFT_INPUT_STREAM_INFO_FLAGS}
  _MFT_INPUT_STREAM_INFO_FLAGS = (
    MFT_INPUT_STREAM_WHOLE_SAMPLES	          = $1,
	  MFT_INPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER	= $2,
	  MFT_INPUT_STREAM_FIXED_SAMPLE_SIZE	      = $4,
	  MFT_INPUT_STREAM_HOLDS_BUFFERS	          = $8,
	  MFT_INPUT_STREAM_DOES_NOT_ADDREF	        = $100,
	  MFT_INPUT_STREAM_REMOVABLE	              = $200,
	  MFT_INPUT_STREAM_OPTIONAL	                = $400,
	  MFT_INPUT_STREAM_PROCESSES_IN_PLACE	      = $800
    );
  MFT_INPUT_STREAM_INFO_FLAGS = _MFT_INPUT_STREAM_INFO_FLAGS;

type
  {$EXTERNALSYM _MFT_OUTPUT_STREAM_INFO_FLAGS}
  _MFT_OUTPUT_STREAM_INFO_FLAGS = (
    MFT_OUTPUT_STREAM_WHOLE_SAMPLES	            = $1,
	  MFT_OUTPUT_STREAM_SINGLE_SAMPLE_PER_BUFFER	= $2,
	  MFT_OUTPUT_STREAM_FIXED_SAMPLE_SIZE	        = $4,
	  MFT_OUTPUT_STREAM_DISCARDABLE	              = $8,
	  MFT_OUTPUT_STREAM_OPTIONAL	                = $10,
	  MFT_OUTPUT_STREAM_PROVIDES_SAMPLES	        = $100,
	  MFT_OUTPUT_STREAM_CAN_PROVIDE_SAMPLES	      = $200,
	  MFT_OUTPUT_STREAM_LAZY_READ	                = $400,
	  MFT_OUTPUT_STREAM_REMOVABLE	                = $800
    );
  MFT_OUTPUT_STREAM_INFO_FLAGS = _MFT_OUTPUT_STREAM_INFO_FLAGS;

type
  {$EXTERNALSYM _MFT_SET_TYPE_FLAGS}
  _MFT_SET_TYPE_FLAGS = (
    MFT_SET_TYPE_TEST_ONLY	= $1
    );

type
  {$EXTERNALSYM _MFT_PROCESS_OUTPUT_FLAGS}
  _MFT_PROCESS_OUTPUT_FLAGS = (
    MFT_PROCESS_OUTPUT_DISCARD_WHEN_NO_BUFFER	= $1
    );
  MFT_PROCESS_OUTPUT_FLAGS = _MFT_PROCESS_OUTPUT_FLAGS;

type
  {$EXTERNALSYM _MFT_PROCESS_OUTPUT_STATUS}
  _MFT_PROCESS_OUTPUT_STATUS = (
    MFT_PROCESS_OUTPUT_STATUS_NEW_STREAMS	= $100
    );

type
  {$EXTERNALSYM _MFT_DRAIN_TYPE}
  _MFT_DRAIN_TYPE = (
    MFT_DRAIN_PRODUCE_TAILS	= 0,
	  MFT_DRAIN_NO_TAILS	= $1
    );
  MFT_DRAIN_TYPE = _MFT_DRAIN_TYPE;

const
  {$EXTERNALSYM MFT_STREAMS_UNLIMITED}
  MFT_STREAMS_UNLIMITED               = $FFFFFFFF;
  {$EXTERNALSYM MFT_OUTPUT_BOUND_LOWER_UNBOUNDED}
  MFT_OUTPUT_BOUND_LOWER_UNBOUNDED    = MINLONGLONG;
  {$EXTERNALSYM MFT_OUTPUT_BOUND_UPPER_UNBOUNDED}
  MFT_OUTPUT_BOUND_UPPER_UNBOUNDED    = MAXLONGLONG;

type
  PMftMessageType = ^TMftMessageType;
  {$EXTERNALSYM _MFT_MESSAGE_TYPE}
  _MFT_MESSAGE_TYPE                    = (
    MFT_MESSAGE_COMMAND_FLUSH          = 0,
    MFT_MESSAGE_COMMAND_DRAIN          = $1,
    MFT_MESSAGE_SET_D3D_MANAGER        = $2,
    MFT_MESSAGE_DROP_SAMPLES           = $3,
    MFT_MESSAGE_NOTIFY_BEGIN_STREAMING = $10000000,
    MFT_MESSAGE_NOTIFY_END_STREAMING   = $10000001,
    MFT_MESSAGE_NOTIFY_END_OF_STREAM   = $10000002,
    MFT_MESSAGE_NOTIFY_START_OF_STREAM = $10000003,
    MFT_MESSAGE_COMMAND_MARKER         = $20000000
  );
  {$EXTERNALSYM MFT_MESSAGE_TYPE}
  MFT_MESSAGE_TYPE = _MFT_MESSAGE_TYPE;
  TMftMessageType = _MFT_MESSAGE_TYPE;

type
  PMftInputStreamInfo = ^TMftInputStreamInfo;
  {$EXTERNALSYM _MFT_INPUT_STREAM_INFO}
  _MFT_INPUT_STREAM_INFO = record
    hnsMaxLatency: LONGLONG;
    dwFlags: DWORD;
    cbSize: DWORD;
    cbMaxLookahead: DWORD;
    cbAlignment: DWORD;
  end;
  {$EXTERNALSYM MFT_INPUT_STREAM_INFO}
  MFT_INPUT_STREAM_INFO = _MFT_INPUT_STREAM_INFO;
  TMftInputStreamInfo = _MFT_INPUT_STREAM_INFO;

type
  PMftOutputStreamInfo = ^TMftOutputStreamInfo;
  {$EXTERNALSYM _MFT_OUTPUT_STREAM_INFO}
  _MFT_OUTPUT_STREAM_INFO = record
    dwFlags: DWORD;
    cbSize: DWORD;
    cbAlignment: DWORD;
  end;
  {$EXTERNALSYM MFT_OUTPUT_STREAM_INFO}
  MFT_OUTPUT_STREAM_INFO = _MFT_OUTPUT_STREAM_INFO;
  TMftOutputStreamInfo = _MFT_OUTPUT_STREAM_INFO;

type
  PMftOutputDataBuffer = ^TMftOutputDataBuffer;
  {$EXTERNALSYM _MFT_OUTPUT_DATA_BUFFER}
  _MFT_OUTPUT_DATA_BUFFER = record
    dwStreamID: DWORD;
    pSample:            IMFSample;
    dwStatus:           DWORD;
    pEvents:            IMFCollection;
  end;
  {$EXTERNALSYM MFT_OUTPUT_DATA_BUFFER}
  MFT_OUTPUT_DATA_BUFFER = _MFT_OUTPUT_DATA_BUFFER;
  TMftOutputDataBuffer = _MFT_OUTPUT_DATA_BUFFER;
  {$EXTERNALSYM PMFT_OUTPUT_DATA_BUFFER}
  PMFT_OUTPUT_DATA_BUFFER = ^_MFT_OUTPUT_DATA_BUFFER;

type
  PStreamMedium = ^TStreamMedium;
  {$EXTERNALSYM _STREAM_MEDIUM}
  _STREAM_MEDIUM = record
    gidMedium: GUID;
    unMediumInstance: UINT32;
  end;
  {$EXTERNALSYM STREAM_MEDIUM}
  STREAM_MEDIUM = _STREAM_MEDIUM;
  TStreamMedium = _STREAM_MEDIUM;
  {$EXTERNALSYM PSTREAM_MEDIUM}
  PSTREAM_MEDIUM = ^_STREAM_MEDIUM;

type

{ How to implement this?   I think, we could do without with Delphi
  // redefine all the method names to have MFT at the beginning so they don't class with DMO methods.

  GetStreamLimits                     = MFTGetStreamLimits;

  GetStreamCount                      = MFTGetStreamCount;

  GetStreamIDs                        = MFTGetStreamIDs;

  GetInputStreamInfo                  = MFTGetInputStreamInfo;

  GetOutputStreamInfo                 = MFTGetOutputStreamInfo;

  DeleteInputStream                   = MFTDeleteInputStream;

  AddInputStreams                     = MFTAddInputStreams;

  GetInputAvailableType               = MFTGetInputAvailableType;

  GetOutputAvailableType              = MFTGetOutputAvailableType;

  SetInputType                        = MFTSetInputType;

  SetOutputType                       = MFTSetOutputType;

  GetInputCurrentType                 = MFTGetInputCurrentType;

  GetOutputCurrentType                = MFTGetOutputCurrentType;

  GetInputStatus                      = MFTGetInputStatus;

  GetOutputStatus                     = MFTGetOutputStatus;

  SetOutputBounds                     = MFTSetOutputBounds;

  ProcessEvent                        = MFTProcessEvent;

  ProcessMessage                      = MFTProcessMessage;

  ProcessInput                        = MFTProcessInput;

  ProcessOutput                       = MFTProcessOutput;
}

  //Interface IMFTransform
  IMFTransform = interface(IUnknown)
    ['{bf94c121-5b05-4e6f-8000-ba598961414d}']
    function GetStreamLimits(out pdwInputMinimum: DWORD; out pdwInputMaximum: DWORD; out pdwOutputMinimum: DWORD; out pdwOutputMaximum: DWORD): HResult; stdcall;
    function GetStreamCount(out pcInputStreams: DWORD; out pcOutputStreams: DWORD): HResult; stdcall;
    function GetStreamIDs(const dwInputIDArraySize: DWORD; out pdwInputIDs: DWORD; const dwOutputIDArraySize: DWORD; pdwOutputIDs: DWORD): HResult; stdcall;
    function GetInputStreamInfo(const dwInputStreamID: DWORD; out pStreamInfo: MFT_INPUT_STREAM_INFO): HResult; stdcall;
    function GetOutputStreamInfo(const dwOutputStreamID: DWORD; out pStreamInfo: MFT_OUTPUT_STREAM_INFO): HResult; stdcall;
    function GetAttributes(out pAttributes: IMFAttributes): HResult; stdcall;
    function GetInputStreamAttributes(const dwInputStreamID: DWORD; out pAttributes: IMFAttributes): HResult; stdcall;
    function GetOutputStreamAttributes(const dwOutputStreamID: DWORD; out pAttributes: IMFAttributes): HResult; stdcall;
    function DeleteInputStream(const dwStreamID: DWORD): HResult; stdcall;
    function AddInputStreams(const cStreams: DWORD; const adwStreamIDs: DWORD): HResult; stdcall;
    function GetInputAvailableType(const dwInputStreamID: DWORD; const dwTypeIndex: DWORD; out ppType: IMFMediaType): HResult; stdcall;
    function GetOutputAvailableType(const dwOutputStreamID: DWORD; const dwTypeIndex: DWORD; var ppType: IMFMediaType): HResult; stdcall;
    function SetInputType(const dwInputStreamID: DWORD; const pType: IMFMediaType; const dwFlags: DWORD): HResult; stdcall;
    function SetOutputType(const dwOutputStreamID: DWORD; const pType: IMFMediaType; const dwFlags: DWORD): HResult; stdcall;
    function GetInputCurrentType(const dwInputStreamID: DWORD; out ppType: IMFMediaType): HResult; stdcall;
    function GetOutputCurrentType(const dwOutputStreamID: DWORD; out ppType: IMFMediaType): HResult; stdcall;
    function GetInputStatus(const dwInputStreamID: DWORD; out pdwFlags: DWORD): HResult; stdcall;
    function GetOutputStatus(out pdwFlags: DWORD): HResult; stdcall;
    function SetOutputBounds(const hnsLowerBound: LONGLONG; const hnsUpperBound: LONGLONG): HResult; stdcall;
    function ProcessEvent(const dwInputStreamID: DWORD; const pEvent: IMFMediaEvent): HResult; stdcall;
    function ProcessMessage(const eMessage: MFT_MESSAGE_TYPE; const ulParam: ULONG_PTR): HResult; stdcall;
    function ProcessInput(const dwInputStreamID: DWORD; const pSample: IMFSample; const  dwFlags: DWORD): HResult; stdcall;
    function ProcessOutput(const dwFlags: DWORD; const cOutputBufferCount: DWORD; var pOutputSamples: MFT_OUTPUT_DATA_BUFFER; out pdwStatus: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM MFCreateTransformActivate}
  function MFCreateTransformActivate(const ppActivate: IMFActivate): HResult; winapi;

const
  // PROPERTYKEYS
  MFPKEY_CLSID                     : PROPERTYKEY = (fmtid: (D1: $57a84c0; D2: $1a80; D3: $40a3;
                                                            D4: ($97, $b5, $92, $72, $a4, $3, $c8, $ae)); pid: $01);

  MFPKEY_CATEGORY                  : PROPERTYKEY = (fmtid: (D1: $c57a84c0; D2: $1a80; D3: $40a3;
                                                            D4: ($97, $b5, $92, $72, $a4, $3, $c8, $ae)); pid: $02);

  MFPKEY_EXATTRIBUTE_SUPPORTED     : PROPERTYKEY = (fmtid: (D1: $456fe843; D2: $3c87; D3: $40c0;
                                                            D4: ($94, $9d, $14, $9, $c9, $7d, $ab, $2c)); pid: $01);

  MFPKEY_MULTICHANNEL_CHANNEL_MASK : PROPERTYKEY = (fmtid: (D1: $58bdaf8c; D2: $3224; D3: $4692;
                                                            D4: ($86, $d0, $44, $d6, $5c, $5b, $f8, $2b)); pid: $01);

  //GUIDS
  MF_SA_D3D_AWARE                           : TGuid = '{eaa35c29-775e-488e-9b61-b3283e49583b}';
  MF_SA_REQUIRED_SAMPLE_COUNT               : TGuid = '{18802c61-324b-4952-abd0-176ff5c696ff}';
  MF_TRANSFORM_ASYNC                        : TGuid = '{f81a699a-649a-497d-8c73-29f8fed6ad7a}';
  MF_TRANSFORM_ASYNC_UNLOCK                 : TGuid = '{e5666d6b-3422-4eb6-a421-da7db1f8e207}';
  MF_TRANSFORM_FLAGS_Attribute              : TGuid = '{9359bb7e-6275-46c4-a025-1c01e45f1a86}';
  MF_TRANSFORM_CATEGORY_Attribute           : TGuid = '{ceabba49-506d-4757-a6ff-66c184987e4e}';
  MFT_TRANSFORM_CLSID_Attribute             : TGuid = '{6821c42b-65a4-4e82-99bc-9a88205ecd0c}';
  MFT_INPUT_TYPES_Attributes                : TGuid = '{4276c9b1-759d-4bf3-9cd0-0d723d138f96}';
  MFT_OUTPUT_TYPES_Attributes               : TGuid = '{8eae8cf3-a44f-4306-ba5c-bf5dda242818}';
  MFT_ENUM_HARDWARE_URL_Attribute           : TGuid = '{2fb866ac-b078-4942-ab6c-003d05cda674}';
  MFT_FRIENDLY_NAME_Attribute               : TGuid = '{314ffbae-5b41-4c95-9c19-4e7d586face3}';
  MFT_CONNECTED_STREAM_ATTRIBUTE            : TGuid = '{71eeb820-a59f-4de2-bcec-38db1dd611a4}';
  MFT_CONNECTED_TO_HW_STREAM                : TGuid = '{34e6e728-06d6-4491-a553-4795650db912}';
  MFT_PREFERRED_OUTPUTTYPE_Attribute        : TGuid = '{7e700499-396a-49ee-b1b4-f628021e8c9d}';
  MFT_PROCESS_LOCAL_Attribute               : TGuid = '{543186e4-4649-4e65-b588-4aa352aff379}';
  MFT_PREFERRED_ENCODER_PROFILE             : TGuid = '{53004909-1ef5-46d7-a18e-5a75f8b5905f}';
  MFT_HW_TIMESTAMP_WITH_QPC_Attribute       : TGuid = '{8d030fb8-cc43-4258-a22e-9210bef89be4}';
  MFT_FIELDOFUSE_UNLOCK_Attribute           : TGuid = '{8ec2e9fd-9148-410d-831e-702439461a8e}';
  MFT_CODEC_MERIT_Attribute                 : TGuid = '{88a7cb15-7b07-4a34-9128-e64c6703c4d3}';
  MFT_ENUM_TRANSCODE_ONLY_ATTRIBUTE         : TGuid = '{111ea8cd-b62a-4bdb-89f6-67ffcdc2458b}';

implementation

  function MFCreateTransformActivate(const ppActivate: IMFActivate): HResult; winapi;  external 'Mfplat.dll' name 'MFCreateTransformActivate';

end.
