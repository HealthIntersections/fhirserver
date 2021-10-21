// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - MediaObj.pas
// Kind: Pascal Unit
// Release date: 08-07-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
// LastEdited by: Tony
// EditDate: updt 080712a, updt 140912b
//
// Remarks: DMO
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
// Source: mediaobj.h
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

unit MediaObj;


  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}


interface

uses
	Windows, ComObj, ActiveX, Unknwn {, ObjIdl}, MfpTypes;


const
	IID_IMediaBuffer                     : TGUID = '{59eff8b9-938c-4a26-82f2-95cb84cdc837}';
	IID_IMediaObject                     : TGUID = '{d8ad0f58-5494-4102-97c5-ec798e59bcf4}';
	IID_IEnumDMO                         : TGUID = '{2c3cd98a-2bfa-4a53-9c27-5249ba64ba0f}';
	IID_IDMOQualityControl               : TGUID = '{65abea96-cf36-453f-af8a-705e98f16260}';
	IID_IMediaObjectInPlace              : TGUID = '{651b9ad0-0fc7-4aa9-9538-d89931010741}';
	IID_IDMOVideoOutputOptimizations     : TGUID = '{be8f4f4e-5b16-4d29-b350-7f6b5d9298ac}';




type
  //  PDmoMediaType = ^TDmoMediaType;
  //  {$EXTERNALSYM DMO_MEDIA_TYPE}
  //  DMO_MEDIA_TYPE = AM_MEDIA_TYPE;
  //  TDmoMediaType = AM_MEDIA_TYPE;

  PDmoMediaType = ^TDmoMediaType;
  {$EXTERNALSYM _DMOMediaType}
  _DMOMediaType = record
    majortype: TGUID;
    subtype: TGUID;
    bFixedSizeSamples: BOOL;
    bTemporalCompression: BOOL;
    lSampleSize: ULONG;
    formattype: TGUID;
    pUnk: PIUnknown;
    cbFormat: ULONG;
    pbFormat: PByte;
  end;
  {$EXTERNALSYM DMO_MEDIA_TYPE}
  DMO_MEDIA_TYPE = _DMOMediaType;
  TDmoMediaType = _DMOMediaType;

type
  PReferenceTime = ^TReferenceTime;
  {$EXTERNALSYM REFERENCE_TIME}
  REFERENCE_TIME = LONGLONG;
  TReferenceTime = LONGLONG;


type
  {$EXTERNALSYM _DMO_INPUT_DATA_BUFFER_FLAGS}
  _DMO_INPUT_DATA_BUFFER_FLAGS        = (
    DMO_INPUT_DATA_BUFFERF_SYNCPOINT  = $1,
	  DMO_INPUT_DATA_BUFFERF_TIME	      = $2,
	  DMO_INPUT_DATA_BUFFERF_TIMELENGTH	= $4
    );
  DMO_INPUT_DATA_BUFFER_FLAGS = _DMO_INPUT_DATA_BUFFER_FLAGS;

type
  {$EXTERNALSYM _DMO_OUTPUT_DATA_BUFFER_FLAGS}
  _DMO_OUTPUT_DATA_BUFFER_FLAGS         = (
    DMO_OUTPUT_DATA_BUFFERF_SYNCPOINT   = $1,
	  DMO_OUTPUT_DATA_BUFFERF_TIME	      = $2,
	  DMO_OUTPUT_DATA_BUFFERF_TIMELENGTH	= $4,
	  DMO_OUTPUT_DATA_BUFFERF_INCOMPLETE	= $1000000
    );
  DMO_OUTPUT_DATA_BUFFER_FLAGS = _DMO_OUTPUT_DATA_BUFFER_FLAGS;

type
  {$EXTERNALSYM _DMO_INPUT_STATUS_FLAGS}
  _DMO_INPUT_STATUS_FLAGS               = (
		DMO_INPUT_STATUSF_ACCEPT_DATA = $1
    );
  DMO_INPUT_STATUS_FLAGS = _DMO_INPUT_STATUS_FLAGS;

type
  {$EXTERNALSYM _DMO_INPUT_STREAM_INFO_FLAGS}
  _DMO_INPUT_STREAM_INFO_FLAGS                  = (
    DMO_INPUT_STREAMF_WHOLE_SAMPLES             = $1,
	  DMO_INPUT_STREAMF_SINGLE_SAMPLE_PER_BUFFER	= $2,
	  DMO_INPUT_STREAMF_FIXED_SAMPLE_SIZE	        = $4,
	  DMO_INPUT_STREAMF_HOLDS_BUFFERS	            = $8
    );
  DMO_INPUT_STREAM_INFO_FLAGS  = _DMO_INPUT_STREAM_INFO_FLAGS;

type
	_DMO_OUTPUT_STREAM_INFO_FLAGS                 = (
    DMO_OUTPUT_STREAMF_WHOLE_SAMPLES            = $1,
	  DMO_OUTPUT_STREAMF_SINGLE_SAMPLE_PER_BUFFER	= $2,
		DMO_OUTPUT_STREAMF_FIXED_SAMPLE_SIZE	      = $4,
		DMO_OUTPUT_STREAMF_DISCARDABLE	            = $8,
	  DMO_OUTPUT_STREAMF_OPTIONAL	                = $10
		);
	DMO_OUTPUT_STREAM_INFO_FLAGS = _DMO_OUTPUT_STREAM_INFO_FLAGS;

type
  {$EXTERNALSYM _DMO_SET_TYPE_FLAGS}
	_DMO_SET_TYPE_FLAGS                           = (
    DMO_SET_TYPEF_TEST_ONLY                     = $1,
		DMO_SET_TYPEF_CLEAR	                        = $2
		);
	DMO_SET_TYPE_FLAGS = _DMO_SET_TYPE_FLAGS;


type
  {$EXTERNALSYM _DMO_PROCESS_OUTPUT_FLAGS}
  _DMO_PROCESS_OUTPUT_FLAGS                     = (
    DMO_PROCESS_OUTPUT_DISCARD_WHEN_NO_BUFFER     = $1
    );
  DMO_PROCESS_OUTPUT_FLAGS = _DMO_PROCESS_OUTPUT_FLAGS;


type
  {$EXTERNALSYM _DMO_INPLACE_PROCESS_FLAGS}
  _DMO_INPLACE_PROCESS_FLAGS          = (
    DMO_INPLACE_NORMAL = 0,
	  DMO_INPLACE_ZERO	= $1
    );
  DMO_INPLACE_PROCESS_FLAGS = _DMO_INPLACE_PROCESS_FLAGS;

type
  {$EXTERNALSYM _DMO_VIDEO_OUTPUT_STREAM_FLAGS}
  _DMO_VIDEO_OUTPUT_STREAM_FLAGS      = (
		DMO_VOSF_NEEDS_PREVIOUS_SAMPLE    = $1
    );
  DMO_VIDEO_OUTPUT_STREAM_FLAGS = _DMO_VIDEO_OUTPUT_STREAM_FLAGS;



type
  // Forward Interfaces Declarations


	PIMediaBuffer = ^TIMediaBuffer;
  {$EXTERNALSYM IMediaBuffer}
  IMediaBuffer = interface;
	TIMediaBuffer = IMediaBuffer;

  PIMediaObject = ^TIMediaObject;
  {$EXTERNALSYM IMediaObject}
  IMediaObject = interface;
	TIMediaObject = IMediaObject;

  PIEnumDMO = ^TIEnumDMO;
  {$EXTERNALSYM IEnumDMO}
	IEnumDMO = interface;
	TIEnumDMO = IEnumDMO;

  PIMediaObjectInPlace = ^TIMediaObjectInPlace;
  {$EXTERNALSYM IMediaObjectInPlace}
	IMediaObjectInPlace = interface;
	TIMediaObjectInPlace = IMediaObjectInPlace;

  PIDMOQualityControl = ^TIDMOQualityControl;
	{$EXTERNALSYM IDMOQualityControl}
	IDMOQualityControl = interface;
	TIDMOQualityControl = IDMOQualityControl;

  PIDMOVideoOutputOptimizations = ^TIDMOVideoOutputOptimizations;
  {$EXTERNALSYM IDMOVideoOutputOptimizations}
	IDMOVideoOutputOptimizations = interface;
	TIDMOVideoOutputOptimizations = IDMOVideoOutputOptimizations;


	PDmoOutputDataBuffer = ^TDmoOutputDataBuffer;
	{$EXTERNALSYM _DMO_OUTPUT_DATA_BUFFER}
	_DMO_OUTPUT_DATA_BUFFER = record
		pBuffer: PIMediaBuffer;
		dwStatus: DWORD;
		rtTimestamp: REFERENCE_TIME;
		rtTimelength: REFERENCE_TIME;
	end;
	{$EXTERNALSYM DMO_OUTPUT_DATA_BUFFER}
	DMO_OUTPUT_DATA_BUFFER = _DMO_OUTPUT_DATA_BUFFER;
	TDmoOutputDataBuffer = _DMO_OUTPUT_DATA_BUFFER;

  // INTERFACES ////////////////////////////////////////////////////////////////

  //Interface IMediaBuffer
  //The IMediaBuffer interface provides methods for manipulating a data buffer.
	//Buffers passed to the IMediaObject.ProcessInput and ProcessOutput methods must implement this interface.
	IMediaBuffer = interface(IUnknown)
  ['{59eff8b9-938c-4a26-82f2-95cb84cdc837}']
		function SetLength(const cbLength: DWORD): HResult; stdcall;
    function GetMaxLength(out pcbMaxLength: DWORD): HResult; stdcall;
    function GetBufferAndLength(out ppBuffer: PByte; out pcbLength: DWORD): HResult; stdcall;
  end;


  //Interface IMediaObject
  //The IMediaObject interface provides methods for manipulating a Microsoft DirectX Media Object (DMO).
  IMediaObject = interface(IUnknown)
  ['{d8ad0f58-5494-4102-97c5-ec798e59bcf4}']
    function GetStreamCount(out pcInputStreams: DWORD; out pcOutputStreams: DWORD): HResult; stdcall;
    function GetInputStreamInfo(const dwInputStreamIndex: DWORD; out pdwFlags: DWORD): HResult; stdcall;
    function GetOutputStreamInfo(const dwOutputStreamIndex: DWORD; out pdwFlags: DWORD): HResult; stdcall;
    function GetInputType(const dwInputStreamIndex: DWORD; const dwTypeIndex: DWORD; out pmt: DMO_MEDIA_TYPE): HResult; stdcall;
    function GetOutputType(const dwOutputStreamIndex: DWORD; const dwTypeIndex: DWORD; out pmt: DMO_MEDIA_TYPE): HResult; stdcall;
    function SetInputType(const dwInputStreamIndex: DWORD; const pmt: DMO_MEDIA_TYPE; const dwFlags: DWORD): HResult; stdcall;
    function SetOutputType(const dwOutputStreamIndex: DWORD; const pmt: DMO_MEDIA_TYPE; const dwFlags: DWORD): HResult; stdcall;
    function GetInputCurrentType(const dwInputStreamIndex: DWORD; out pmt: DMO_MEDIA_TYPE): HResult; stdcall;
    function GetOutputCurrentType(const dwOutputStreamIndex: DWORD; out pmt: DMO_MEDIA_TYPE): HResult; stdcall;
    function GetInputSizeInfo(const dwInputStreamIndex: DWORD; out pcbSize: DWORD; out pcbMaxLookahead: DWORD; out pcbAlignment: DWORD): HResult; stdcall;
    function GetOutputSizeInfo(const dwOutputStreamIndex: DWORD; out pcbSize: DWORD; out pcbAlignment: DWORD): HResult; stdcall;
    function GetInputMaxLatency(const dwInputStreamIndex: DWORD; out prtMaxLatency: REFERENCE_TIME): HResult; stdcall;
    function SetInputMaxLatency(const dwInputStreamIndex: DWORD; const rtMaxLatency: REFERENCE_TIME): HResult; stdcall;
    function Flush(): HResult; stdcall;
    function Discontinuity(const dwInputStreamIndex: DWORD): HResult; stdcall;
    function AllocateStreamingResources(): HResult; stdcall;
    function FreeStreamingResources(): HResult; stdcall;
    function GetInputStatus(const dwInputStreamIndex: DWORD; out dwFlags: DWORD): HResult; stdcall;
    function ProcessInput(const dwInputStreamIndex: DWORD; const pBuffer: IMediaBuffer; const dwFlags: DWORD; const rtTimestamp: REFERENCE_TIME; const rtTimelength: REFERENCE_TIME): HResult; stdcall;
		function ProcessOutput(const dwFlags: DWORD; const cOutputBufferCount: DWORD; var pOutputBuffers: DMO_OUTPUT_DATA_BUFFER; out pdwStatus: DWORD): HResult; stdcall;
		function Lock(const bLock: LONG): HResult; stdcall;
  end;


  //interface IEnumDMO
  //The IEnumDMO interface provides methods for enumerating Microsoft DirectX Media Objects (DMOs).
  //It is based on the OLE enumeration interfaces.
  //For more information, see the IEnumXXXX topic in the Platform SDK.
  //To enumerate registered DMOs, call the DMOEnum function.
  IEnumDMO = interface(IUnknown)
  ['{2c3cd98a-2bfa-4a53-9c27-5249ba64ba0f}']
    function Next(const cItemsToFetch: DWORD; out pCLSID: CLSID; out Names: PWideChar; out pcItemsFetched: DWORD): HResult; stdcall;
    function Skip(const cItemsToSkip: DWORD): HResult; stdcall;
    function Reset(): HResult; stdcall;
    function Clone(out ppEnum: PIEnumDMO): HResult; stdcall;
  end;



  //Interface IMediaObjectInPlace
  //The IMediaObjectInPlace interface provides methods for processing data in place.
  //A Microsoft DirectX Media Object (DMO) can expose this interface if it meets the following conditions:
  //   - It has one input stream and one output stream.
  //   - Both streams use the same media type.
  //   - The output is produced in place on the buffer; that is, without copying data.
  //This interface provides an optimized way to process data.
  //The application calls a single IMediaObjectInPlace.Process method instead of the IMediaObject::ProcessInput and
  //IMediaObject.ProcessOutput methods.
  //However, any DMO that implements this interface must also implement the IMediaObject interface.
  //Therefore, an application is never obligated to use this interface, and a DMO is never guaranteed to implement it.
	IMediaObjectInPlace = interface(IUnknown)
	['{651b9ad0-0fc7-4aa9-9538-d89931010741}']
		function Process(const ulSize: ULONG; var pData: PByte; const refTimeStart: REFERENCE_TIME;  const dwFlags: DWORD): HResult; stdcall;
    function Clone(out ppMediaObject: PIMediaObjectInPlace): HResult; stdcall;
		function GetLatency(out pLatencyTime: REFERENCE_TIME): HResult; stdcall;
	end;

	//Interface IDMOQualityControl
	//The IDMOQualityControl interface supports quality control on a Microsoft DirectX Media Object (DMO).
	//A DMO exposes this interface if it can respond to late samples.
	//When quality control is enabled, the DMO attempts to process samples on time,
	//discarding late samples if necessary.
	//When quality control is disabled, the DMO processes every sample. By default, quality control is disabled.
	//Applications use this interface to enable or disable quality control.
	//Using quality control is appropriate when you are viewing media data in real time.
	//If you are capturing data to a file, do not enable quality control, because the DMO might discard samples.
	//It does not matter in file capture whether samples arrive late, and you do not want to lose the data.
	//COMMENT:
	// To use quality control, perform the following steps:
	//  Call the IDMOQualityControl.SetNow method with the reference time of the earliest sample to be processed.
	//  Call the IDMOQualityControl.SetStatus method with the DMO_QUALITY_STATUS_ENABLED flag.
	//  To disable quality control, call SetStatus with no flag.
	IDMOQualityControl = interface(IUnknown)
	['{65abea96-cf36-453f-af8a-705e98f16260}']
		function SetNow(const rtNow: REFERENCE_TIME): HResult; stdcall;
		function SetStatus(const dwFlags: DWORD): HResult; stdcall;
		function GetStatus(out pdwFlags: DWORD): HResult; stdcall;
	end;


	//Interface IDMOVideoOutputOptimizations
	//The IDMOVideoOutputOptimizations interface supports video optimizations on a Microsoft DirectX Media Object (DMO).
	//NOTE: This interface enables an application to negotiate with a DMO about video output optimizations.
	//      A DMO exposes this interface when it can perform optimizations that require support from the application.
	//      The application can query the DMO for its preferred features, and then agree (or not agree) to provide them.
  //      The DMO must process output even if the application rejects the optimizations.
  //
  //For example, a video decoder might generate an output frame by applying deltas to the previous output frame.
  //When queried, it requests that the application supply the previous frame in the output buffer.
  //The application can agree to this request or not.
  //
  //Video optimizations are negotiated separately for each output stream.
  IDMOVideoOutputOptimizations = interface(IUnknown)
  ['{be8f4f4e-5b16-4d29-b350-7f6b5d9298ac}']
    function QueryOperationModePreferences(const ulOutputStreamIndex: ULONG; const pdwRequestedCapabilities: DWORD): HResult; stdcall;
    function SetOperationMode(const ulOutputStreamIndex: ULONG; const dwEnabledFeatures: DWORD): HResult; stdcall;
    function GetCurrentOperationMode(const ulOutputStreamIndex: ULONG; var pdwEnabledFeatures: DWORD): HResult; stdcall;
    function GetCurrentSampleRequirements( const ulOutputStreamIndex: ULONG; var pdwRequestedFeatures: DWORD): HResult; stdcall;
  end;


// Additional Prototypes for ALL interfaces

// end of Additional Prototypes

implementation

end.
