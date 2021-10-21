// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - MfReadWrite.pas
// Kind: Pascal Unit
// Release date: 27-06-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows 7 or later (See: Remarks). 
// 
// Intiator(s): Tony (maXcomX), Peter (OzShips) 
// 
// LastEdited by: Tony (maXcomX)
// EditDate: updt 270612a
//
// Remarks: These interfaces are available on Windows Vista if Platform Update Supplement for Windows Vista is installed.
//
//          Delphi : The IUnknown entries of functions should be casted like this:
//                   IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 9.0, upd 4
// Todo: Check some Guids (see: Todo comments)
// =============================================================================
// Source: mfreadwrite.h
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

unit MfReadWrite;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}

interface

uses
  Windows, ComObj, MfObjects, Unknwn;

const

  IID_IMFReadWriteClassFactory                          : TGUID = '{E7FE2E12-661C-40DA-92F9-4F002AB67627}';
  IID_IMFSourceReader                                   : TGUID = '{70ae66f2-c809-4e4f-8915-bdcb406b7993}';
  IID_IMFSourceReaderCallback                           : TGUID = '{deec8d99-fa1d-4d82-84c2-2c8969944867}';
  IID_IMFSinkWriter                                     : TGUID = '{3137f1cd-fe5e-4805-a5d8-fb477448cb3d}';
  IID_IMFSinkWriterCallback                             : TGUID = '{666f76de-33d2-41b9-a458-29ed0a972c58}';


  CLSID_MFReadWriteClassFactory                         : TGUID = '{48e2ed0f-98c2-4a37-bed5-166312ddd83f}';
  CLSID_MFSourceReader                                  : TGUID = '{1777133c-0881-411b-a577-ad545f0714c4}';

  //Interface IMFSourceReader
  MF_SOURCE_READER_ASYNC_CALLBACK                       : TGUID = '{1e3dbeac-bb43-4c35-b507-cd644464c965}';
  MF_SOURCE_READER_D3D_MANAGER                          : TGUID = '{ec822da2-e1e9-4b29-a0d8-563c719f5269}';
  MF_SOURCE_READER_DISABLE_DXVA                         : TGUID = '{aa456cfd-3943-4a1e-a77d-1838c0ea2e35}';
  MF_SOURCE_READER_MEDIASOURCE_CONFIG                   : TGUID = '{9085abeb-0354-48f9-abb5-200df838c68e}';
  MF_SOURCE_READER_MEDIASOURCE_CHARACTERISTICS          : TGUID = '{6d23f5c8-c5d7-4a9b-9971-5d11f8bca880}';
  MF_SOURCE_READER_ENABLE_VIDEO_PROCESSING              : TGUID = '{fb394f3d-ccf1-42ee-bbb3-f9b845d5681d}';
  MF_SOURCE_READER_DISCONNECT_MEDIASOURCE_ON_SHUTDOWN   : TGUID = '{56b67165-219e-456d-a22e-2d3004c7fe56}';

  //Interface IMFSourceReader
  MF_SOURCE_READER_INVALID_STREAM_INDEX	= $ffffffff;
	MF_SOURCE_READER_ALL_STREAMS	        = $fffffffe;
	MF_SOURCE_READER_ANY_STREAM	          = $fffffffe;
	MF_SOURCE_READER_FIRST_AUDIO_STREAM	  = $fffffffd;
	MF_SOURCE_READER_FIRST_VIDEO_STREAM	  = $fffffffc;
	MF_SOURCE_READER_MEDIASOURCE	        = $ffffffff;

  //Interface IMFSinkWriter
  CLSID_MFSinkWriter                                    : TGUID = '{a3bbfb17-8273-4e52-9e0e-9739dc887990}';

  MF_SINK_WRITER_ASYNC_CALLBACK                         : TGUID = '{48cb183e-7b0b-46f4-822e-5e1d2dda4354}';
  MF_SINK_WRITER_DISABLE_THROTTLING                     : TGUID = '{08b845d8-2b74-4afe-9d53-be16d2d5ae4f}';

  MF_SINK_WRITER_INVALID_STREAM_INDEX	= $ffffffff;
	MF_SINK_WRITER_ALL_STREAMS	        = $fffffffe;
	MF_SINK_WRITER_MEDIASINK	          = $ffffffff;

  //
  MF_READWRITE_DISABLE_CONVERTERS                       : TGUID = '{98d5b065-1374-4847-8d5d-31520fee7156}';
  MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS               : TGUID = '{a634a91c-822b-41b9-a494-4de4643612b0}';

type
  {$EXTERNALSYM MF_SOURCE_READER_FLAG}
  cwMF_SOURCE_READER_FLAG                     = (
    MF_SOURCE_READERF_ERROR                   = $1,
    MF_SOURCE_READERF_ENDOFSTREAM             = $2,
    MF_SOURCE_READERF_NEWSTREAM               = $4,
    MF_SOURCE_READERF_NATIVEMEDIATYPECHANGED  = $10,
    MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED = $20,
    MF_SOURCE_READERF_STREAMTICK              = $100
  );
  MF_SOURCE_READER_FLAG = cwMF_SOURCE_READER_FLAG;
  //DEFINE_ENUM_FLAG_OPERATORS(MF_SOURCE_READER_FLAG)

type
  {$EXTERNALSYM MF_SOURCE_READER_CONTROL_FLAG}
  cwMF_SOURCE_READER_CONTROL_FLAG   = (
    MF_SOURCE_READER_CONTROLF_DRAIN = $1
  );
  MF_SOURCE_READER_CONTROL_FLAG = cwMF_SOURCE_READER_CONTROL_FLAG;
  //DEFINE_ENUM_FLAG_OPERATORS(MF_SOURCE_READER_CONTROL_FLAG)

type
{$EXTERNALSYM _MF_SINK_WRITER_STATISTICS}
  _MF_SINK_WRITER_STATISTICS = record
    cb: DWORD;
    llLastTimestampReceived: LONGLONG;
    llLastTimestampEncoded: LONGLONG;
    llLastTimestampProcessed: LONGLONG;
    llLastStreamTickReceived: LONGLONG;
    llLastSinkSampleRequest: LONGLONG;
    qwNumSamplesReceived: QWORD;
    qwNumSamplesEncoded: QWORD;
    qwNumSamplesProcessed: QWORD;
    qwNumStreamTicksReceived: QWORD;
    dwByteCountQueued: DWORD;
    qwByteCountProcessed: QWORD;
    dwNumOutstandingSinkSampleRequests: DWORD;
    dwAverageSampleRateReceived: DWORD;
    dwAverageSampleRateEncoded: DWORD;
    dwAverageSampleRateProcessed: DWORD;
  end;
  {$EXTERNALSYM MF_SINK_WRITER_STATISTICS}
  MF_SINK_WRITER_STATISTICS = _MF_SINK_WRITER_STATISTICS;;

type
  //Forward Declarations

  {$EXTERNALSYM IMFReadWriteClassFactory}
  IMFReadWriteClassFactory = interface;
  {$EXTERNALSYM IMFSourceReader}
  IMFSourceReader = interface;
  {$EXTERNALSYM IMFSourceReaderCallback}
  IMFSourceReaderCallback = interface;
  {$EXTERNALSYM IMFSinkWriter}
  IMFSinkWriter = interface;
  {$EXTERNALSYM IMFSinkWriterCallback}
  IMFSinkWriterCallback = interface;


  //INTERFACES

  //Interface IMFReadWriteClassFactory
  {
   Creates an instance of either the sink writer or the source reader.
   NOTE:  To get a pointer to this interface, call the CoCreateInstance function.
          The CLSID is CLSID_MFReadWriteClassFactory. Call the MFStartup function before using the interface.

          As an alternative to using this interface, you can call any of the following functions:
          MFCreateSinkWriterFromMediaSink
          MFCreateSinkWriterFromURL
          MFCreateSourceReaderFromByteStream
          MFCreateSourceReaderFromMediaSource
          MFCreateSourceReaderFromURL
          Internally, these functions use the IMFReadWriteClassFactory interface.

          This interface is available on Windows Vista if Platform Update Supplement for Windows Vista is installed.
  }
  IMFReadWriteClassFactory = interface(IUnknown)
	['{E7FE2E12-661C-40DA-92F9-4F002AB67627}']
    function CreateInstanceFromURL(const clsid: REFCLSID; const pwszURL: LPCWSTR; const pAttributes: IMFAttributes;
                                   const riid: REFIID; out ppvObject: Pointer): HResult; stdcall;
    function CreateInstanceFromObject(const clsid: REFCLSID; const punkObject: IUnknown; const pAttributes: IMFAttributes;
                                      const riid: REFIID; out ppvObject: Pointer): HResult; stdcall;
  end;

  //Interface IMFSourceReader
  {
   Implemented by the Microsoft Media Foundation source reader object.
   To create the source reader, call one of the following functions:
    MFCreateSourceReaderFromByteStream
    MFCreateSourceReaderFromMediaSource
    MFCreateSourceReaderFromURL

   Alternatively, use the IMFReadWriteClassFactory interface.

   This interface is available on Windows Vista if Platform Update Supplement for Windows Vista is installed.
  }
  IMFSourceReader = interface(IUnknown)
	['{70ae66f2-c809-4e4f-8915-bdcb406b7993}']
    function GetStreamSelection(const dwStreamIndex: DWord; out pfSelected: Boolean): HResult; stdcall;
    function SetStreamSelection(const dwStreamIndex: DWord; const fSelected: Boolean): HResult; stdcall;
    function GetNativeMediaType(const dwStreamIndex: DWord; const dwMediaTypeIndex: DWord; out ppMediaType: IMFMediaType): HResult; stdcall;
    function GetCurrentMediaType(const dwStreamIndex: DWord; out ppMediaType: IMFMediaType): HResult; stdcall;
    function SetCurrentMediaType(const dwStreamIndex: DWord; var pdwReserved: DWord; pMediaType: IMFMediaType): HResult; stdcall;
    function SetCurrentPosition(const guidTimeFormat: REFGUID; const varPosition: REFPROPVARIANT): HResult; stdcall;
    function ReadSample(const dwStreamIndex: DWord; const dwControlFlags: DWord; out pdwActualStreamIndex: DWord;
                        out pdwStreamFlags: DWord; out pllTimestamp: LONGLONG; out ppSample: IMFSample;
    function Flush(const dwStreamIndex: DWord): HResult; stdcall;
    function GetServiceForStream(const dwStreamIndex: DWord; const guidService: REFGUID; const riid: REFIID; out ppvObject: Pointer): HResult; stdcall;
    function GetPresentationAttribute(const dwStreamIndex: DWord; const guidAttribute: REFGUID; out pvarAttribute: PROPVARIANT): HResult; stdcall;
  end;

  //Interface IMFSourceReaderCallback
  {
   Callback interface for the Microsoft Media Foundation source reader.
   Implement this interface if you use the IMFSourceReader interface and want to receive asynchronous notifications.
  }
  IMFSourceReaderCallback = interface(IUnknown)
	['{deec8d99-fa1d-4d82-84c2-2c8969944867}']
    function OnReadSample(const hrStatus: HRESULT; const dwStreamIndex: DWord; const dwStreamFlags: DWord;
                          const llTimestamp: LONGLONG; const pSample: IMFSample): HResult; stdcall;
    function OnFlush(const dwStreamIndex: DWord): HResult; stdcall;
    function OnEvent(const dwStreamIndex: DWord; const pEvent: IMFMediaEvent): HResult; stdcall;
  end;

  //Interface IMFSinkWriter
  {
   Implemented by the Microsoft Media Foundation sink writer object.
   NOTE:  To create the sink writer, call one of the following functions:
          MFCreateSinkWriterFromMediaSink
          MFCreateSinkWriterFromURL

          Alternatively, use the IMFReadWriteClassFactory interface.

          This interface is available on Windows Vista if Platform Update Supplement for Windows Vista is installed.
  }
  IMFSinkWriter = interface(IUnknown)
	['{3137f1cd-fe5e-4805-a5d8-fb477448cb3d}']
    function AddStream(const pTargetMediaType: IMFMediaType; out pdwStreamIndex: DWord): HResult; stdcall;
    function SetInputMediaType(const dwStreamIndex: DWord; const pInputMediaType: IMFMediaType; const pEncodingParameters: IMFAttributes): HResult; stdcall;
    function BeginWriting(): HResult; stdcall;
    function WriteSample(const dwStreamIndex: DWord; const pSample: IMFSample): HResult; stdcall;
    function SendStreamTick(const dwStreamIndex: DWord; const llTimestamp: LONGLONG): HResult; stdcall;
    function PlaceMarker(const dwStreamIndex: DWord; const pvContext: Pointer): HResult; stdcall;
    function NotifyEndOfSegment(const dwStreamIndex: DWord): HResult; stdcall;
    function Flush(const dwStreamIndex: DWord): HResult; stdcall;
    function Finalize(): HResult; stdcall;
    function GetServiceForStream(const dwStreamIndex: DWord; const guidService: REFGUID; const riid: REFIID; out ppvObject: Pointer): HResult; stdcall;
    function GetStatistics(const dwStreamIndex: DWord; out pStats: MF_SINK_WRITER_STATISTICS): HResult; stdcall;
  end;

  //Interface IMFSinkWriterCallback
  {
   Callback interface for the Microsoft Media Foundation sink writer.
   Implement this interface if you use the IMFSinkWriter interface and want to receive asynchronous notifications.
  }
  IMFSinkWriterCallback = interface(IUnknown)
	['{666f76de-33d2-41b9-a458-29ed0a972c58}']
    function OnFinalize(const hrStatus: HRESULT): HResult; stdcall;
    function OnMarker(const dwStreamIndex: DWord; const pvContext: Pointer): HResult; stdcall;
  end;

  //CREATE functions
  function MFCreateSourceReaderFromURL(const pwszURL: LPCWSTR; const pAttributes: IMFAttributes; out ppSourceReader: IMFSourceReader): HResult; stdcall;
  function MFCreateSourceReaderFromByteStream(const pByteStream: IMFByteStream; const pAttributes: IMFAttributes; out ppSourceReader: IMFSourceReader): HResult; stdcall;
  function MFCreateSourceReaderFromMediaSource(const pMediaSource: IMFMediaSource; const pAttributes: IMFAttributes; out ppSourceReader: IMFSourceReader): HResult; stdcall;
  function MFCreateSinkWriterFromURL(const pwszOutputURL: LPCWSTR; const pByteStream: IMFByteStream;
                                     const pAttributes: IMFAttributes out ppSinkWriter: IMFSinkWriter): HResult; stdcall;
  function MFCreateSinkWriterFromMediaSink(const pMediaSink: IMFMediaSink; const pAttributes: IMFAttributes; out ppSinkWriter: IMFSinkWriter): HResult; stdcall;


// Additional Prototypes for ALL interfaces
// end of Additional Prototypes

implementation

end.
