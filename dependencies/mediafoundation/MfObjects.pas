// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - MfObjects.pas
// Kind: Pascal Unit
// Release date: 29-06-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
//
// LastEdited by: Peter (ozships)
// EditDate: updp 080712a, updt 290712b, updt 210912b
//----------------------------------------------------------------------------
// Changes
//----------------------------------------------------------------------------
// 2012/07/08 Peter (ozships)    Added ole2 to the Uses clause, required for
//                               variant definitions (eg VT_UI4)
//                               Correct typo
//>> look for "//## old" for the old lines, new lines underneath
//----------------------------------------------------------------------------
//
// Remarks:
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, updt 4
// Todo: -
// =============================================================================
// Source: mfobjects.h
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

unit MfObjects;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}

interface

uses
	Windows, ComObj, ActiveX, {Ole2,} PropSys, PropIdl, MediaObj, unknwn,
	DirectShow9, WTypes, MfpTypes;  //updt 290712b, updt 210912b


const
	IID_IMFAttributes                     : TGUID = '{2cd2d921-c447-44a7-a13c-4adabfc247e3}';
  IID_IMFMediaBuffer                    : TGUID = '{045FA593-8799-42b8-BC8D-8968C6453507}';
  IID_IMFSample                         : TGUID = '{c40a00f2-b93a-4d80-ae8c-5a1c634f58e4}';
  IID_IMF2DBuffer                       : TGUID = '{7DC9D5F9-9ED9-44ec-9BBF-0600BB589FBB}';
  IID_IMFMediaType                      : TGUID = '{44ae0fa8-ea31-4109-8d2e-4cae4997c555}';
  IID_IMFAudioMediaType                 : TGUID = '{26a0adc3-ce26-4672-9304-69552edd3faf}';
  IID_IMFVideoMediaType                 : TGUID = '{b99f381f-a8f9-47a2-a5af-ca3a225a3890}';
  IID_IMFAsyncResult                    : TGUID = '{ac6b7889-0740-4d51-8619-905994a55cc6}';
  IID_IMFAsyncCallback                  : TGUID = '{a27003cf-2354-4f2a-8d6a-ab7cff15437e}';
  IID_IMFMediaEvent                     : TGUID = '{DF598932-F10C-4E39-BBA2-C308F101DAA3}';
  IID_IMFMediaEventGenerator            : TGUID = '{2CD0BD52-BCD5-4B89-B62C-EADC0C031E7D}';
  IID_IMFRemoteAsyncCallback            : TGUID = '{a27003d0-2354-4f2a-8d6a-ab7cff15437e}';
  IID_IMFCollection                     : TGUID = '{5BC8A76B-869A-46a3-9B03-FA218A66AEBE}';
  IID_IMFMediaEventQueue                : TGUID = '{36f846fc-2256-48b6-b58e-e2b638316581}';
  IID_IMFActivate                       : TGUID = '{7FEE9E9A-4A89-47a6-899C-B6A53A70FB67}';
  IID_IMFPluginControl                  : TGUID = '{5c6c44bf-1db6-435b-9249-e8cd10fdec96}';


  //Interface IMFMediaType
  {$EXTERNALSYM MF_MEDIATYPE_EQUAL_MAJOR_TYPES}
  MF_MEDIATYPE_EQUAL_MAJOR_TYPES      = $00000001;
  {$EXTERNALSYM MF_MEDIATYPE_EQUAL_FORMAT_TYPES}
  MF_MEDIATYPE_EQUAL_FORMAT_TYPES     = $00000002;
  {$EXTERNALSYM MF_MEDIATYPE_EQUAL_FORMAT_DATA}
  MF_MEDIATYPE_EQUAL_FORMAT_DATA      = $00000004;
  {$EXTERNALSYM MF_MEDIATYPE_EQUAL_FORMAT_USER_DATA}
  MF_MEDIATYPE_EQUAL_FORMAT_USER_DATA = $00000008;

  //Interface IMFAsyncCallback
  {$EXTERNALSYM MFASYNC_FAST_IO_PROCESSING_CALLBACK}
  MFASYNC_FAST_IO_PROCESSING_CALLBACK = $00000001;
  {$EXTERNALSYM MFASYNC_SIGNAL_CALLBACK}
  MFASYNC_SIGNAL_CALLBACK             = $00000002;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_UNDEFINED}
  MFASYNC_CALLBACK_QUEUE_UNDEFINED    = $00000000;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_STANDARD}
  MFASYNC_CALLBACK_QUEUE_STANDARD     = $00000001;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_RT}
  MFASYNC_CALLBACK_QUEUE_RT           = $00000002;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_IO}
  MFASYNC_CALLBACK_QUEUE_IO           = $00000003;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_TIMER}
  MFASYNC_CALLBACK_QUEUE_TIMER        = $00000004;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_LONG_FUNCTION}
  MFASYNC_CALLBACK_QUEUE_LONG_FUNCTION= $00000007;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_PRIVATE_MASK}
  MFASYNC_CALLBACK_QUEUE_PRIVATE_MASK = $FFFF0000;
  {$EXTERNALSYM MFASYNC_CALLBACK_QUEUE_ALL}
  MFASYNC_CALLBACK_QUEUE_ALL          = $FFFFFFFF;

type
  {$EXTERNALSYM MediaEventType}
  MediaEventType = DWord;

const
  MEUnknown                             : MediaEventType  = 0;
  MEError                               : MediaEventType	= 1;
	MEExtendedType                        : MediaEventType	= 2;
//## old	MENonFatalError                       : MediaEventType	= 3;
//## old	MEGenericV1Anchor                     : MediaEventType	= MENonFatalError;
  MENonFatalError = MediaEventType(3);
  MEGenericV1Anchor = MediaEventType(MENonFatalError);
	MESessionUnknown                      : MediaEventType	= 100;
	MESessionTopologySet                  : MediaEventType	= 101;
	MESessionTopologiesCleared            : MediaEventType	= 102;
	MESessionStarted                      : MediaEventType	= 103;
	MESessionPaused                       : MediaEventType	= 104;
	MESessionStopped                      : MediaEventType	= 105;
	MESessionClosed                       : MediaEventType	= 106;
	MESessionEnded                        : MediaEventType	= 107;
	MESessionRateChanged                  : MediaEventType	= 108;
	MESessionScrubSampleComplete          : MediaEventType	= 109;
	MESessionCapabilitiesChanged          : MediaEventType	= 110;
	MESessionTopologyStatus               : MediaEventType	= 111;
	MESessionNotifyPresentationTime       : MediaEventType	= 112;
	MENewPresentation                     : MediaEventType	= 113;
	MELicenseAcquisitionStart             : MediaEventType	= 114;
	MELicenseAcquisitionCompleted         : MediaEventType	= 115;
	MEIndividualizationStart              : MediaEventType	= 116;
	MEIndividualizationCompleted          : MediaEventType	= 117;
	MEEnablerProgress                     : MediaEventType	= 118;
	MEEnablerCompleted                    : MediaEventType	= 119;
	MEPolicyError                         : MediaEventType	= 120;
	MEPolicyReport                        : MediaEventType	= 121;
	MEBufferingStarted                    : MediaEventType	= 122;
	MEBufferingStopped                    : MediaEventType	= 123;
	MEConnectStart                        : MediaEventType	= 124;
	MEConnectEnd                          : MediaEventType	= 125;
	MEReconnectStart                      : MediaEventType	= 126;
	MEReconnectEnd                        : MediaEventType	= 127;
	MERendererEvent                       : MediaEventType	= 128;
	MESessionStreamSinkFormatChanged      : MediaEventType	= 129;
	MESessionV1Anchor                     : MediaEventType	= 129; //MESessionStreamSinkFormatChanged;   d7 compiler does not eat this.
	MESourceUnknown                       : MediaEventType	= 200;
	MESourceStarted                       : MediaEventType	= 201;
	MEStreamStarted                       : MediaEventType	= 202;
	MESourceSeeked                        : MediaEventType	= 203;
	MEStreamSeeked                        : MediaEventType	= 204;
	MENewStream                           : MediaEventType	= 205;
	MEUpdatedStream                       : MediaEventType	= 206;
	MESourceStopped                       : MediaEventType	= 207;
	MEStreamStopped                       : MediaEventType	= 208;
	MESourcePaused                        : MediaEventType	= 209;
	MEStreamPaused                        : MediaEventType	= 210;
	MEEndOfPresentation                   : MediaEventType	= 211;
	MEEndOfStream                         : MediaEventType	= 212;
	MEMediaSample                         : MediaEventType	= 213;
	MEStreamTick                          : MediaEventType	= 214;
	MEStreamThinMode                      : MediaEventType	= 215;
	MEStreamFormatChanged                 : MediaEventType	= 216;
	MESourceRateChanged                   : MediaEventType	= 217;
	MEEndOfPresentationSegment            : MediaEventType	= 218;
	MESourceCharacteristicsChanged        : MediaEventType	= 219;
	MESourceRateChangeRequested           : MediaEventType	= 220;
	MESourceMetadataChanged               : MediaEventType	= 221;
	MESequencerSourceTopologyUpdated      : MediaEventType	= 222;
	MESourceV1Anchor                      : MediaEventType	= 222; //MESequencerSourceTopologyUpdated;
	MESinkUnknown                         : MediaEventType	= 300;
	MEStreamSinkStarted                   : MediaEventType	= 301;
	MEStreamSinkStopped                   : MediaEventType	= 302;
	MEStreamSinkPaused                    : MediaEventType	= 303;
	MEStreamSinkRateChanged               : MediaEventType	= 304;
	MEStreamSinkRequestSample             : MediaEventType	= 305;
	MEStreamSinkMarker                    : MediaEventType	= 306;
	MEStreamSinkPrerolled                 : MediaEventType	= 307;
	MEStreamSinkScrubSampleComplete       : MediaEventType	= 308;
	MEStreamSinkFormatChanged             : MediaEventType	= 309;
	MEStreamSinkDeviceChanged             : MediaEventType	= 310;
	MEQualityNotify                       : MediaEventType	= 311;
	MESinkInvalidated                     : MediaEventType	= 312;
	MEAudioSessionNameChanged             : MediaEventType	= 313;
	MEAudioSessionVolumeChanged           : MediaEventType	= 314;
	MEAudioSessionDeviceRemoved           : MediaEventType	= 315;
	MEAudioSessionServerShutdown          : MediaEventType	= 316;
	MEAudioSessionGroupingParamChanged    : MediaEventType	= 317;
	MEAudioSessionIconChanged             : MediaEventType	= 318;
	MEAudioSessionFormatChanged           : MediaEventType	= 319;
	MEAudioSessionDisconnected            : MediaEventType	= 320;
	MEAudioSessionExclusiveModeOverride   : MediaEventType	= 321;
	MESinkV1Anchor                        : MediaEventType	= 321; //MEAudioSessionExclusiveModeOverride;
	METrustUnknown                        : MediaEventType	= 400;
	MEPolicyChanged                       : MediaEventType	= 401;
	MEContentProtectionMessage            : MediaEventType	= 402;
	MEPolicySet                           : MediaEventType	= 403;
	METrustV1Anchor                       : MediaEventType	= 403; //MEPolicySet;
	MEWMDRMLicenseBackupCompleted         : MediaEventType	= 500;
	MEWMDRMLicenseBackupProgress          : MediaEventType	= 501;
	MEWMDRMLicenseRestoreCompleted        : MediaEventType	= 502;
	MEWMDRMLicenseRestoreProgress         : MediaEventType	= 503;
	MEWMDRMLicenseAcquisitionCompleted    : MediaEventType	= 506;
	MEWMDRMIndividualizationCompleted     : MediaEventType	= 508;
	MEWMDRMIndividualizationProgress      : MediaEventType	= 513;
	MEWMDRMProximityCompleted             : MediaEventType	= 514;
	MEWMDRMLicenseStoreCleaned            : MediaEventType	= 515;
	MEWMDRMRevocationDownloadCompleted    : MediaEventType	= 516;
	MEWMDRMV1Anchor                       : MediaEventType	= 516; //MEWMDRMRevocationDownloadCompleted;
	METransformUnknown                    : MediaEventType	= 600;
	METransformNeedInput                  : MediaEventType	= 601; //( METransformUnknown + 1 );
	METransformHaveOutput                 : MediaEventType	= 602; //( METransformNeedInput + 1);
	METransformDrainComplete              : MediaEventType	= 603; //( METransformHaveOutput + 1);
	METransformMarker                     : MediaEventType	= 604; //( METransformDrainComplete + 1);
	MEReservedMax                         : MediaEventType	= 10000;

  //Interface IMFMediaEventGenerator
  {$EXTERNALSYM MF_EVENT_FLAG_NO_WAIT}
  MF_EVENT_FLAG_NO_WAIT = $00000001;

  //Interface IMFByteStream
   {$EXTERNALSYM MFBYTESTREAM_IS_READABLE}
  MFBYTESTREAM_IS_READABLE            = $00000001;
  {$EXTERNALSYM MFBYTESTREAM_IS_WRITABLE}
  MFBYTESTREAM_IS_WRITABLE             = $00000002;
  {$EXTERNALSYM MFBYTESTREAM_IS_SEEKABLE}
  MFBYTESTREAM_IS_SEEKABLE             = $00000004;
  {$EXTERNALSYM MFBYTESTREAM_IS_REMOTE}
  MFBYTESTREAM_IS_REMOTE               = $00000008;
  {$EXTERNALSYM MFBYTESTREAM_IS_DIRECTORY}
  MFBYTESTREAM_IS_DIRECTORY            = $00000080;
  {$EXTERNALSYM MFBYTESTREAM_HAS_SLOW_SEEK}
  MFBYTESTREAM_HAS_SLOW_SEEK           = $00000100;
  {$EXTERNALSYM MFBYTESTREAM_IS_PARTIALLY_DOWNLOADED}
  MFBYTESTREAM_IS_PARTIALLY_DOWNLOADED = $00000200;
  //>= Windows 7
  //#if (WINVER >= _WIN32_WINNT_WIN7)
  {$EXTERNALSYM MFBYTESTREAM_SHARE_WRITE}
  MFBYTESTREAM_SHARE_WRITE            = $00000400;
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)
  //end >= Windows 7
  {$EXTERNALSYM MFBYTESTREAM_SEEK_FLAG_CANCEL_PENDING_IO}
  MFBYTESTREAM_SEEK_FLAG_CANCEL_PENDING_IO = $00000001;

  //Interface IMFByteStream
	MF_BYTESTREAM_ORIGIN_NAME                        : TGUID = '{fc358288-3cb6-460c-a424-b6681260375a}';
	MF_BYTESTREAM_CONTENT_TYPE                       : TGUID = '{fc358289-3cb6-460c-a424-b6681260375a}';
	MF_BYTESTREAM_DURATION                        	 : TGUID = '{fc35828a-3cb6-460c-a424-b6681260375a}';
	MF_BYTESTREAM_LAST_MODIFIED_TIME                 : TGUID = '{fc35828b-3cb6-460c-a424-b6681260375a}';
  // >= Windows 7
	//#if (WINVER >= _WIN32_WINNT_WIN7)
	MF_BYTESTREAM_IFO_FILE_URI                       : TGUID = '{fc35828c-3cb6-460c-a424-b6681260375a}';
	MF_BYTESTREAM_DLNA_PROFILE_ID                    : TGUID = '{fc35828d-3cb6-460c-a424-b6681260375a}';
	//#endif // (WINVER >= _WIN32_WINNT_WIN7)
  //end >= Windows 7

  //Interface IMFByteStream
type
  {$EXTERNALSYM QWORD}
	QWORD = ULONGLONG;

type
  {$EXTERNALSYM RGBQUAD}
  RGBQUAD = DWORD;

type
  {$EXTERNALSYM tWAVEFORMATEX}
  tWAVEFORMATEX = record
	wFormatTag: WORD;
	nChannels: WORD;
	nSamplesPerSec: DWORD;
	nAvgBytesPerSec: DWORD;
	nBlockAlign: WORD;
	wBitsPerSample: WORD;
	cbSize: WORD;
	pExtraBytes: array[0..0] of Byte;
  end;
  {$EXTERNALSYM WAVEFORMATEX}
  WAVEFORMATEX = tWAVEFORMATEX;
  {$EXTERNALSYM PWAVEFORMATEX}
  PWAVEFORMATEX = ^tWAVEFORMATEX;
  {$EXTERNALSYM NPWAVEFORMATEX}
  NPWAVEFORMATEX = ^tWAVEFORMATEX;
  {$EXTERNALSYM LPWAVEFORMATEX}
  LPWAVEFORMATEX = ^tWAVEFORMATEX;

type
  cwWAVEFORMATEXTENSIBLE = record
	wFormatTag: WORD;
	nChannels: WORD;
	nSamplesPerSec: DWORD;
	nAvgBytesPerSec: DWORD;
	nBlockAlign: WORD;
	wBitsPerSample: WORD;
	cbSize: WORD;
	wValidBitsPerSample: WORD;
	dwChannelMask: DWORD;
	SubFormat: TGuid;
  end;
  {$EXTERNALSYM WAVEFORMATEXTENSIBLE}
  WAVEFORMATEXTENSIBLE = cwWAVEFORMATEXTENSIBLE;
  {$EXTERNALSYM PWAVEFORMATEXTENSIBLE}
  PWAVEFORMATEXTENSIBLE = ^cwWAVEFORMATEXTENSIBLE;


type
	{$EXTERNALSYM _MF_ATTRIBUTE_TYPE}
	_MF_ATTRIBUTE_TYPE    = (
	MF_ATTRIBUTE_UINT32   = VT_UI4,
	MF_ATTRIBUTE_UINT64   = VT_UI8,
	MF_ATTRIBUTE_DOUBLE   = VT_R8,
	MF_ATTRIBUTE_GUID     = VT_CLSID,
	MF_ATTRIBUTE_STRING   = VT_LPWSTR,
	MF_ATTRIBUTE_BLOB     = (VT_VECTOR or VT_UI1),
	MF_ATTRIBUTE_IUNKNOWN = VT_UNKNOWN
	);
	{$EXTERNALSYM MF_ATTRIBUTE_TYPE}
	MF_ATTRIBUTE_TYPE = _MF_ATTRIBUTE_TYPE;

type
  {$EXTERNALSYM _MF_ATTRIBUTES_MATCH_TYPE}
  _MF_ATTRIBUTES_MATCH_TYPE        = (
	MF_ATTRIBUTES_MATCH_OUR_ITEMS    = 0,
	MF_ATTRIBUTES_MATCH_THEIR_ITEMS  = 1,
	MF_ATTRIBUTES_MATCH_ALL_ITEMS    = 2,
	MF_ATTRIBUTES_MATCH_INTERSECTION = 3,
	MF_ATTRIBUTES_MATCH_SMALLER      = 4
  );
  {$EXTERNALSYM MF_ATTRIBUTES_MATCH_TYPE}
  MF_ATTRIBUTES_MATCH_TYPE = _MF_ATTRIBUTES_MATCH_TYPE;

type
  {$EXTERNALSYM MF_ATTRIBUTE_SERIALIZE_OPTIONS}
  cwMF_ATTRIBUTE_SERIALIZE_OPTIONS     = (
	MF_ATTRIBUTE_SERIALIZE_UNKNOWN_BYREF = $1
  );
  MF_ATTRIBUTE_SERIALIZE_OPTIONS = cwMF_ATTRIBUTE_SERIALIZE_OPTIONS;

type
  cwBITMAPINFOHEADER = record
	biSize: DWORD;
	biWidth: LONG;
	biHeight: LONG;
	biPlanes: WORD;
	biBitCount: WORD;
	biCompression: DWORD;
	biSizeImage: DWORD;
	biXPelsPerMeter: LONG;
  biYPelsPerMeter: LONG;
	biClrUsed: DWORD;
	biClrImportant: DWORD;
  end;
  {$EXTERNALSYM BITMAPINFOHEADER}
  BITMAPINFOHEADER = cwBITMAPINFOHEADER;

type
  cwBITMAPINFO = record
	bmiHeader: BITMAPINFOHEADER;
	bmiColors: array[0..0] of RGBQUAD;
  end;
  {$EXTERNALSYM BITMAPINFO}
  BITMAPINFO = cwBITMAPINFO;

type
  cwMFT_REGISTER_TYPE_INFO = record
	guidMajorType: TGUID;
	guidSubtype: TGUID;
  end;
  {$EXTERNALSYM MFT_REGISTER_TYPE_INFO}
  MFT_REGISTER_TYPE_INFO = cwMFT_REGISTER_TYPE_INFO;

type
  {$EXTERNALSYM _MFVideoInterlaceMode}
  _MFVideoInterlaceMode                        = (
	MFVideoInterlace_Unknown                     = 0,
	MFVideoInterlace_Progressive                 = 2,
	MFVideoInterlace_FieldInterleavedUpperFirst  = 3,
	MFVideoInterlace_FieldInterleavedLowerFirst  = 4,
	MFVideoInterlace_FieldSingleUpper            = 5,
	MFVideoInterlace_FieldSingleLower            = 6,
	MFVideoInterlace_MixedInterlaceOrProgressive = 7,
	MFVideoInterlace_Last                        = 8,
	MFVideoInterlace_ForceDWORD                  = $7FFFFFFF
  );
  {$EXTERNALSYM MFVideoInterlaceMode}
  MFVideoInterlaceMode = _MFVideoInterlaceMode;

const
  {$EXTERNALSYM MFVideoInterlace_FieldSingleUpperFirst}
  MFVideoInterlace_FieldSingleUpperFirst = MFVideoInterlace_FieldSingleUpper;
  {$EXTERNALSYM MFVideoInterlace_FieldSingleLowerFirst}
  MFVideoInterlace_FieldSingleLowerFirst = MFVideoInterlace_FieldSingleLower;

type
  {$EXTERNALSYM _MFVideoTransferFunction}
  _MFVideoTransferFunction    = (
	MFVideoTransFunc_Unknown    = 0,
	MFVideoTransFunc_10         = 1,
	MFVideoTransFunc_18         = 2,
	MFVideoTransFunc_20         = 3,
	MFVideoTransFunc_22         = 4,
	MFVideoTransFunc_709        = 5,
	MFVideoTransFunc_240M       = 6,
	MFVideoTransFunc_sRGB       = 7,
	MFVideoTransFunc_28         = 8,
	MFVideoTransFunc_Log_100    = 9,
	MFVideoTransFunc_Log_316    = 10,
	MFVideoTransFunc_709_sym    = 11,
	MFVideoTransFunc_Last       = 12,
	MFVideoTransFunc_ForceDWORD = $7FFFFFFF
  );
  {$EXTERNALSYM MFVideoTransferFunction}
  MFVideoTransferFunction = _MFVideoTransferFunction;

type
  {$EXTERNALSYM _MFVideoPrimaries}
  _MFVideoPrimaries              = (
	MFVideoPrimaries_Unknown       = 0,
	MFVideoPrimaries_reserved      = 1,
	MFVideoPrimaries_BT709         = 2,
	MFVideoPrimaries_BT470_2_SysM  = 3,
	MFVideoPrimaries_BT470_2_SysBG = 4,
	MFVideoPrimaries_SMPTE170M     = 5,
	MFVideoPrimaries_SMPTE240M     = 6,
	MFVideoPrimaries_EBU3213       = 7,
	MFVideoPrimaries_SMPTE_C       = 8,
	MFVideoPrimaries_Last          = 9,
	MFVideoPrimaries_ForceDWORD    = $7FFFFFFF
  );
  {$EXTERNALSYM MFVideoPrimaries}
  MFVideoPrimaries = _MFVideoPrimaries;

type
  {$EXTERNALSYM _MFVideoLighting}
  _MFVideoLighting           = (
	MFVideoLighting_Unknown    = 0,
	MFVideoLighting_bright     = 1,
	MFVideoLighting_office     = 2,
	MFVideoLighting_dim        = 3,
	MFVideoLighting_dark       = 4,
	MFVideoLighting_Last       = 5,
	MFVideoLighting_ForceDWORD = $7FFFFFFF
  );
  {$EXTERNALSYM MFVideoLighting}
  MFVideoLighting = _MFVideoLighting;

type
  {$EXTERNALSYM _MFVideoTransferMatrix}
  _MFVideoTransferMatrix           = (
	MFVideoTransferMatrix_Unknown    = 0,
	MFVideoTransferMatrix_BT709      = 1,
	MFVideoTransferMatrix_BT601      = 2,
	MFVideoTransferMatrix_SMPTE240M  = 3,
	MFVideoTransferMatrix_Last       = 4,
	MFVideoTransferMatrix_ForceDWORD = $7FFFFFFF
  );
  {$EXTERNALSYM MFVideoTransferMatrix}
  MFVideoTransferMatrix = _MFVideoTransferMatrix;

type
  {$EXTERNALSYM _MFVideoChromaSubsampling}
  _MFVideoChromaSubsampling                               = (
	MFVideoChromaSubsampling_Unknown                        = 0,
	MFVideoChromaSubsampling_ProgressiveChroma              = $8,
	MFVideoChromaSubsampling_Horizontally_Cosited           = $4,
	MFVideoChromaSubsampling_Vertically_Cosited             = $2,
	MFVideoChromaSubsampling_Vertically_AlignedChromaPlanes = $1,
	MFVideoChromaSubsampling_MPEG2                          = $4 or $1,
	MFVideoChromaSubsampling_MPEG1                          = $1,
	MFVideoChromaSubsampling_DV_PAL                         = $4 or $2,
	MFVideoChromaSubsampling_Cosited                        = $4 or $2 or $1,
	MFVideoChromaSubsampling_Last                           = ($4 or $2 or $1 + 1),
	MFVideoChromaSubsampling_ForceDWORD                     = $7FFFFFFF
  );
  {$EXTERNALSYM MFVideoChromaSubsampling}
  MFVideoChromaSubsampling = _MFVideoChromaSubsampling;

type
  {$EXTERNALSYM _MFNominalRange}
  _MFNominalRange           = (
	MFNominalRange_Unknown    = 0,
	MFNominalRange_Normal     = 1,
	MFNominalRange_Wide       = 2,
	MFNominalRange_0_255      = 1,
	MFNominalRange_16_235     = 2,
	MFNominalRange_48_208     = 3,
	MFNominalRange_64_127     = 4,
	MFNominalRange_Last       = 5,
	MFNominalRange_ForceDWORD = $7FFFFFFF
  );
  {$EXTERNALSYM MFNominalRange}
  MFNominalRange = _MFNominalRange;

type
{$EXTERNALSYM _MFVideoFlags}
  _MFVideoFlags                     = (
	MFVideoFlag_PAD_TO_Mask           = ($1 or $2),
	MFVideoFlag_PAD_TO_None           = (0 * $1),
	MFVideoFlag_PAD_TO_4x3            = (1 * $1),
	MFVideoFlag_PAD_TO_16x9           = (2 * $1),
	MFVideoFlag_SrcContentHintMask    = (($4 or $8) or $10),
	MFVideoFlag_SrcContentHintNone    = (0 * $4),
	MFVideoFlag_SrcContentHint16x9    = (1 * $4),
	MFVideoFlag_SrcContentHint235_1   = (2 * $4),
	MFVideoFlag_AnalogProtected       = $20,
	MFVideoFlag_DigitallyProtected    = $40,
	MFVideoFlag_ProgressiveContent    = $80,
	MFVideoFlag_FieldRepeatCountMask  = (($100 or $200) or $400),
	MFVideoFlag_FieldRepeatCountShift = 8,
	MFVideoFlag_ProgressiveSeqReset   = $800,
	MFVideoFlag_PanScanEnabled        = $20000,
	MFVideoFlag_LowerFieldFirst       = $40000,
	MFVideoFlag_BottomUpLinearRep     = $80000,
	MFVideoFlags_DXVASurface          = $100000,
	MFVideoFlags_RenderTargetSurface  = $400000,
	MFVideoFlags_ForceQWORD           = $7FFFFFFF
  );
  {$EXTERNALSYM MFVideoFlags}
  MFVideoFlags = _MFVideoFlags;

type
  {$EXTERNALSYM _MFRatio}
  _MFRatio = record
	Numerator: DWORD;
	Denominator: DWORD;
  end;
  {$EXTERNALSYM MFRatio}
  MFRatio = _MFRatio;

type
  {$EXTERNALSYM _MFOffset}
  _MFOffset = record
	fract: WORD;
	value: Smallint;
  end;
  {$EXTERNALSYM MFOffset}
  MFOffset = _MFOffset;

type
  {$EXTERNALSYM _MFVideoArea}
  _MFVideoArea = record
	OffsetX: MFOffset;
	OffsetY: MFOffset;
	Area: SIZE;
  end;
  {$EXTERNALSYM MFVideoArea}
  MFVideoArea = _MFVideoArea;

type
  {$EXTERNALSYM _MFVideoInfo}
  _MFVideoInfo = record
	dwWidth: DWORD;
	dwHeight: DWORD;
	PixelAspectRatio: MFRatio;
	SourceChromaSubsampling: MFVideoChromaSubsampling;
	InterlaceMode: MFVideoInterlaceMode;
	TransferFunction: MFVideoTransferFunction;
	ColorPrimaries: MFVideoPrimaries;
	TransferMatrix: MFVideoTransferMatrix;
	SourceLighting: MFVideoLighting;
	FramesPerSecond: MFRatio;
	NominalRange: MFNominalRange;
	GeometricAperture: MFVideoArea;
	MinimumDisplayAperture: MFVideoArea;
	PanScanAperture: MFVideoArea;
	VideoFlags: UInt64;
  end;
  {$EXTERNALSYM MFVideoInfo}
//####old  MFVideoInfo = _MFVideoInfo;i                      
  MFVideoInfo = _MFVideoInfo; 
type
  {$EXTERNALSYM __MFAYUVSample}
  __MFAYUVSample = record
	bCrValue: Byte;
	bCbValue: Byte;
	bYValue: Byte;
	bSampleAlpha8: Byte;
  end;
  {$EXTERNALSYM MFAYUVSample}
  MFAYUVSample = __MFAYUVSample;

type
  {$EXTERNALSYM _MFARGB}
  _MFARGB = record
	rgbBlue: Byte;
	rgbGreen: Byte;
	rgbRed: Byte;
	rgbAlpha: Byte;
  end;
  {$EXTERNALSYM MFARGB}
  MFARGB = _MFARGB;

type
  {$EXTERNALSYM _MFPaletteEntry}
	_MFPaletteEntry = record
	ARGB: MFARGB;
	AYCbCr: MFAYUVSample;
  end;
	{$EXTERNALSYM MFPaletteEntry}
  MFPaletteEntry = _MFPaletteEntry;

type
  {$EXTERNALSYM _MFVideoSurfaceInfo}
  _MFVideoSurfaceInfo = record
	Format: DWORD;
	PaletteEntries: DWORD;
	Palette: array[0..0] of MFPaletteEntry;
  end;
  {$EXTERNALSYM MFVideoSurfaceInfo}
  MFVideoSurfaceInfo = _MFVideoSurfaceInfo;

type
  {$EXTERNALSYM _MFVideoCompressedInfo}
  _MFVideoCompressedInfo = record
	AvgBitrate: LONGLONG;
	AvgBitErrorRate: LONGLONG;
	MaxKeyFrameSpacing: DWORD;
  end;
  {$EXTERNALSYM MFVideoCompressedInfo}
  MFVideoCompressedInfo = _MFVideoCompressedInfo;

type
  {$EXTERNALSYM _MFVIDEOFORMAT}
  _MFVIDEOFORMAT = record
	dwSize: DWORD;
	videoInfo: MFVideoInfo;
	guidFormat: TGUID;
	compressedInfo: MFVideoCompressedInfo;
	surfaceInfo: MFVideoSurfaceInfo;
  end;
	{$EXTERNALSYM MFVIDEOFORMAT}
	MFVIDEOFORMAT = _MFVIDEOFORMAT;
	PMFVIDEOFORMAT = ^MFVIDEOFORMAT;

type
	{$EXTERNALSYM _MFStandardVideoFormat}
	_MFStandardVideoFormat        = (
	MFStdVideoFormat_reserved     = 0,
	MFStdVideoFormat_NTSC         = 1,
	MFStdVideoFormat_PAL          = 2,
	MFStdVideoFormat_DVD_NTSC     = 3,
	MFStdVideoFormat_DVD_PAL      = 4,
	MFStdVideoFormat_DV_PAL       = 5,
	MFStdVideoFormat_DV_NTSC      = 6,
	MFStdVideoFormat_ATSC_SD480i  = 7,
	MFStdVideoFormat_ATSC_HD1080i = 8,
	MFStdVideoFormat_ATSC_HD720p  = 9
  );
  {$EXTERNALSYM MFStandardVideoFormat}
  MFStandardVideoFormat = _MFStandardVideoFormat;

type
  {$EXTERNALSYM _MFBYTESTREAM_SEEK_ORIGIN}
  _MFBYTESTREAM_SEEK_ORIGIN = (
	msoBegin                  = 0,
	msoCurrent                = 1
  );
  {$EXTERNALSYM MFBYTESTREAM_SEEK_ORIGIN}
  MFBYTESTREAM_SEEK_ORIGIN = _MFBYTESTREAM_SEEK_ORIGIN;

type
  {$EXTERNALSYM __MIDL___MIDL_itf_mfobjects_0000_0013_0001}
  cwMF_FILE_ACCESSMODE    = (
	MF_ACCESSMODE_READ      = 1,
	MF_ACCESSMODE_WRITE     = 2,
	MF_ACCESSMODE_READWRITE = 3
  );
  {$EXTERNALSYM MF_FILE_ACCESSMODE}
  MF_FILE_ACCESSMODE = cwMF_FILE_ACCESSMODE;

type
  {$EXTERNALSYM __MIDL___MIDL_itf_mfobjects_0000_0013_0002}
  cwMF_OPENMODE_FAIL_IF_NOT_EXIST = (
  MF_OPENMODE_FAIL_IF_NOT_EXIST   = 0,
	MF_OPENMODE_FAIL_IF_EXIST       = 1,
	MF_OPENMODE_RESET_IF_EXIST      = 2,
	MF_OPENMODE_APPEND_IF_EXIST     = 3,
	MF_OPENMODE_DELETE_IF_EXIST     = 4
  );
  {$EXTERNALSYM MF_FILE_OPENMODE}
  MF_FILE_OPENMODE = cwMF_OPENMODE_FAIL_IF_NOT_EXIST;


type
  {$EXTERNALSYM __MIDL___MIDL_itf_mfobjects_0000_0013_0003}
  cwMF_FILE_FLAGS                     = (
    MF_FILEFLAGS_NONE                 = 0,
	MF_FILEFLAGS_NOBUFFERING            = $1,
    MF_FILEFLAGS_ALLOW_WRITE_SHARING  = $2
  );
  {$EXTERNALSYM MF_FILE_FLAGS}
  MF_FILE_FLAGS = cwMF_FILE_FLAGS;

type
  // >= Windows 7
  {$EXTERNALSYM _MF_Plugin_Type}
  _MF_Plugin_Type            = (
	MF_Plugin_Type_MFT         = 0,
	MF_Plugin_Type_MediaSource = 1
  );
  {$EXTERNALSYM MF_Plugin_Type}
  MF_Plugin_Type = _MF_Plugin_Type;
  //end  >= Windows 7

//==============================================================================
//Forward Interface Declarations
//==============================================================================
type
  {$EXTERNALSYM IMFAttributes}
  IMFAttributes = interface;
  {$EXTERNALSYM IMFMediaBuffer}
  IMFMediaBuffer = interface;
  {$EXTERNALSYM IMFSample}
  IMFSample = interface;
  {$EXTERNALSYM IMF2DBuffer}
  IMF2DBuffer = interface;
  {$EXTERNALSYM IMFMediaType}
  IMFMediaType = interface;
  {$EXTERNALSYM IMFAudioMediaType}
  IMFAudioMediaType = interface;
  {$EXTERNALSYM IMFVideoMediaType}
  IMFVideoMediaType = interface;
  {$EXTERNALSYM IMFAsyncResult}
  IMFAsyncResult = interface;
  {$EXTERNALSYM IMFAsyncCallback}
  IMFAsyncCallback = interface;
  {$EXTERNALSYM IMFMediaEvent}
  IMFMediaEvent = interface;
  {$EXTERNALSYM IMFMediaEventGenerator}
  IMFMediaEventGenerator = interface;
  {$EXTERNALSYM IMFRemoteAsyncCallback}
  IMFRemoteAsyncCallback = interface;
  {$EXTERNALSYM IMFByteStream}
  IMFByteStream = interface;
  {$EXTERNALSYM IMFCollection}
  IMFCollection = interface;
  {$EXTERNALSYM IMFMediaEventQueue}
  IMFMediaEventQueue = interface;
  {$EXTERNALSYM IMFActivate}
  IMFActivate = interface;
  {$EXTERNALSYM IMFPluginControl}
  IMFPluginControl = interface;


  //============================================================================
  //Interfaces
  //============================================================================


  //Interface IMFAttributes
  {
   Provides a generic way to store key/value pairs on an object.
   The keys are GUIDs, and the values can be any of the following data types:
   UINT32, UINT64, double, GUID, wide-character string, byte array, or IUnknown pointer.
   The standard implementation of this interface holds a thread lock while values are added,
   deleted, or retrieved.
  }
	IMFAttributes = interface(IUnknown)
	['{2cd2d921-c447-44a7-a13c-4adabfc247e3}']
    function GetItem(const guidKey: REFGUID; var pValue: PROPVARIANT): HResult; stdcall;
    function GetItemType(const guidKey: REFGUID; out pType: MF_ATTRIBUTE_TYPE): HResult; stdcall;
		function CompareItem(const guidKey: REFGUID; const Value: REFPROPVARIANT; out pbResult: Boolean): HResult; stdcall;
    function Compare(const pTheirs: IMFAttributes; const MatchType: MF_ATTRIBUTES_MATCH_TYPE; out pbResult: Boolean): HResult; stdcall;
    function GetUINT32(const guidKey: REFGUID; out punValue: UINT32): HResult; stdcall;
    function GetUINT64(const guidKey: REFGUID; out punValue: UINT64): HResult; stdcall;
    function GetDouble(const guidKey: REFGUID; out pfValue: Double): HResult; stdcall;
    function GetGUID(const guidKey: REFGUID; out pguidValue: TGuid): HResult; stdcall;
    function GetStringLength(const guidKey: REFGUID; out pcchLength: UINT32): HResult; stdcall;
    function GetString(const guidKey: REFGUID; out pwszValue: LPWSTR; out cchBufSize: UINT32; var pcchLength: UINT32): HResult; stdcall;
    function GetAllocatedString(const guidKey: REFGUID; out ppwszValue: LPWSTR; out pcchLength: UINT32): HResult; stdcall;
    function GetBlobSize(const guidKey: REFGUID; out pcbBlobSize: UINT32): HResult; stdcall;
    function GetBlob(const guidKey: REFGUID; out pBuf: UINT8; out cbBufSize: UINT32; var pcbBlobSize: UINT32): HResult; stdcall;
    function GetAllocatedBlob(const guidKey: REFGUID; out ppBuf: UINT8; out pcbSize: UINT32): HResult; stdcall;
    function GetUnknown(const guidKey: REFGUID; const riid: REFIID; out ppv: Pointer): HResult; stdcall;
    function SetItem(const guidKey: REFGUID; const Value: REFPROPVARIANT): HResult; stdcall;
    function DeleteItem(const guidKey: REFGUID): HResult; stdcall;
    function DeleteAllItems(): HResult; stdcall;
    function SetUINT32(const guidKey: REFGUID; const unValue: UINT32): HResult; stdcall;
    function SetUINT64(const guidKey: REFGUID; const unValue: UINT64): HResult; stdcall;
    function SetDouble(const guidKey: REFGUID; const fValue: Double): HResult; stdcall;
    function SetGUID(const guidKey: REFGUID; const guidValue: REFGUID): HResult; stdcall;
    function SetString(const guidKey: REFGUID; const wszValue: LPCWSTR): HResult; stdcall;
    function SetBlob(const guidKey: REFGUID; const pBuf: UINT8; const cbBufSize: UINT32): HResult; stdcall;
    function SetUnknown(const guidKey: REFGUID; const pUnknown: IUnknown): HResult; stdcall;
    function LockStore(): HResult; stdcall;
    function UnlockStore(): HResult; stdcall;
    function GetCount(out pcItems: UINT32): HResult; stdcall;
    function GetItemByIndex(const unIndex: UINT32; const guidKey: REFGUID; var pValue: PROPVARIANT): HResult; stdcall;
    function CopyAllItems(const pDest: IMFAttributes): HResult; stdcall;
	end;

  //Interface IMFMediaBuffer
  {
   Represents a block of memory that contains media data. Use this interface to access the data in the buffer.
  }
  IMFMediaBuffer = interface(IUnknown)
	['{045FA593-8799-42b8-BC8D-8968C6453507}']
    function Lock(out ppbBuffer: Byte; out pcbMaxLength: DWord; out pcbCurrentLength: DWord): HResult; stdcall;
    function Unlock(): HResult; stdcall;
    function GetCurrentLength(out pcbCurrentLength: DWord): HResult; stdcall;
    function SetCurrentLength(const cbCurrentLength: DWord): HResult; stdcall;
    function GetMaxLength(out pcbMaxLength: DWord): HResult; stdcall;
  end;

  //Interface IMFSample
  {
   Represents a media sample, which is a container object for media data.
   For video, a sample typically contains one video frame.
   For audio data, a sample typically contains multiple audio samples, rather than a single sample of audio.
   A media sample contains zero or more buffers.
   Each buffer manages a block of memory, and is represented by the IMFMediaBuffer interface.
   A sample can have multiple buffers.
   The buffers are kept in an ordered list and accessed by index value.
   It is also valid to have an empty sample with no buffers.
  }
  IMFSample = interface(IUnknown)
	['{c40a00f2-b93a-4d80-ae8c-5a1c634f58e4}']
    function GetSampleFlags(out pdwSampleFlags: DWord): HResult; stdcall;
    function SetSampleFlags(const dwSampleFlags: DWord): HResult; stdcall;
    function GetSampleTime(out phnsSampleTime: LONGLONG): HResult; stdcall;
    function SetSampleTime(const hnsSampleTime: LONGLONG): HResult; stdcall;
    function GetSampleDuration(out phnsSampleDuration: LONGLONG): HResult; stdcall;
    function SetSampleDuration(const hnsSampleDuration: LONGLONG): HResult; stdcall;
    function GetBufferCount(out pdwBufferCount: DWord): HResult; stdcall;
    function GetBufferByIndex(const dwIndex: DWord; out ppBuffer: IMFMediaBuffer): HResult; stdcall;
    function ConvertToContiguousBuffer(out ppBuffer: IMFMediaBuffer): HResult; stdcall;
    function AddBuffer(const pBuffer: IMFMediaBuffer): HResult; stdcall;
    function RemoveBufferByIndex(const dwIndex: DWord): HResult; stdcall;
    function RemoveAllBuffers(): HResult; stdcall;
    function GetTotalLength(out pcbTotalLength: DWord): HResult; stdcall;
    function CopyToBuffer(const pBuffer: IMFMediaBuffer): HResult; stdcall;
  end;

  //Interface IMF2DBuffer
  {
  Represents a buffer that contains a two-dimensional surface, such as a video frame.
  }
  IMF2DBuffer = interface(IUnknown)
	['{7DC9D5F9-9ED9-44ec-9BBF-0600BB589FBB}']
    function Lock2D(out pbScanline0: Byte; out plPitch: LONG): HResult; stdcall;
    function Unlock2D(): HResult; stdcall;
    function GetScanline0AndPitch(out pbScanline0: Byte; out plPitch: LONG): HResult; stdcall;
    function IsContiguousFormat(out pfIsContiguous: Boolean): HResult; stdcall;
    function GetContiguousLength(out pcbLength: DWord): HResult; stdcall;
    function ContiguousCopyTo(out pbDestBuffer: Byte; const cbDestBuffer: DWord): HResult; stdcall;
    function ContiguousCopyFrom(const pbSrcBuffer: Byte; const cbSrcBuffer: DWord): HResult; stdcall;
  end;

  //Interface IMFMediaType
  {
   Represents a description of a media format.
  }
  IMFMediaType = interface(IUnknown)
	['{44ae0fa8-ea31-4109-8d2e-4cae4997c555}']
    function GetMajorType(out pguidMajorType: TGuid): HResult; stdcall;
    function IsCompressedFormat(out pfCompressed: Boolean): HResult; stdcall;
    function IsEqual(const pIMediaType: IMFMediaType; out pdwFlags: DWord): HResult; stdcall;
    function GetRepresentation(const guidRepresentation: TGuid; out ppvRepresentation: Pointer): HResult; stdcall;
    function FreeRepresentation(const guidRepresentation: TGuid; const pvRepresentation: Pointer): HResult; stdcall;
  end;

  //Interface IMFAudioMediaType
  {
   Represents a description of an audio format.
   (Vista only)
   NOTE:  No longer available for use as of Windows 7
          Instead, use the media type attributes to get the properties of the audio format.
  }
  IMFAudioMediaType = interface(IMFMediaType)
	['{26a0adc3-ce26-4672-9304-69552edd3faf}']
		function GetAudioFormat(): PWAVEFORMATEX; stdcall;
  end;

  //Interface IMFVideoMediaType
  {
	 Represents a description of a video format.
   NOTE: Applications should avoid using this interface except when a method or function requires an
         IMFVideoMediaType pointer as a parameter. You can get all of the format information from a
         video media type through the IMFAttributes interface, which IMFMediaType inherits.
  }
  IMFVideoMediaType = interface(IMFMediaType)
	['{b99f381f-a8f9-47a2-a5af-ca3a225a3890}']
    //Returns a pointer to an MFVIDEOFORMAT structure that describes the video format. (Deprecated)
		function GetVideoFormat(): PMFVIDEOFORMAT; stdcall;
    //Retrieves an alternative representation of the media type. (Deprecated)
		function GetVideoRepresentation(const guidRepresentation: TGuid; out ppvRepresentation: Pointer; const lStride: LONG): HResult; stdcall;
	end;

  //Interface IMFAsyncResult
  {
   Provides information about the result of an asynchronous operation.
  }
  IMFAsyncResult = interface(IUnknown)
	['{ac6b7889-0740-4d51-8619-905994a55cc6}']
    function GetState(out ppunkState: IUnknown): HResult; stdcall;
    function GetStatus(): HResult; stdcall;
    function SetStatus(const hrStatus: HRESULT): HResult; stdcall;
    function GetObject(out ppObject: IUnknown): HResult; stdcall;
    function GetStateNoAddRef(): IUnknown; stdcall;
  end;


  //Interface IMFAsyncCallback
  {
  Callback interface to notify the application when an asynchronous method completes.
  }
  IMFAsyncCallback = interface(IUnknown)
	['{a27003cf-2354-4f2a-8d6a-ab7cff15437e}']
    function GetParameters(out pdwFlags: DWord; out pdwQueue: DWord): HResult; stdcall;
    function Invoke(const pAsyncResult: IMFAsyncResult): HResult; stdcall;
  end;

  //Interface IMFMediaEvent
  {
   Represents an event generated by a Media Foundation object.
   Use this interface to get information about the event.
   To get a pointer to this interface, call IMFMediaEventGenerator.BeginGetEvent or IMFMediaEventGenerator.GetEvent on the event generator.
  }
  IMFMediaEvent = interface(IUnknown)
	['{DF598932-F10C-4E39-BBA2-C308F101DAA3}']
    function GetType(out pmet: MediaEventType): HResult; stdcall;
    function GetExtendedType(out pguidExtendedType: TGuid): HResult; stdcall;
    function GetStatus(out phrStatus: HRESULT): HResult; stdcall;
    function GetValue(out pvValue: PROPVARIANT): HResult; stdcall;
  end;


  //Interface IMFMediaEventGenerator
  {
   Retrieves events from any Media Foundation object that generates events.
   NOTE:  An object that supports this interface maintains a queue of events.
          The client of the object can retrieve the events either synchronously or asynchronously.
          The synchronous method is GetEvent.
          The asynchronous methods are BeginGetEvent and EndGetEvent.
  }
  IMFMediaEventGenerator = interface(IUnknown)
	['{2CD0BD52-BCD5-4B89-B62C-EADC0C031E7D}']
    function GetEvent(const dwFlags: DWord; out ppEvent: IMFMediaEvent): HResult; stdcall;
    function BeginGetEvent(const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndGetEvent(const pResult: IMFAsyncResult; out ppEvent: IMFMediaEvent): HResult; stdcall;
    function QueueEvent(const met: MediaEventType; const guidExtendedType: REFGUID; hrStatus: HRESULT; const pvValue: PROPVARIANT): HResult; stdcall;
  end;


  //Interface IMFRemoteAsyncCallback
  {
   Used by the Microsoft Media Foundation proxy/stub DLL to marshal certain asynchronous method calls across process boundaries.
   Applications do not use or implement this interface.
   So, it's just here to complete the header translation, for your convenience...
  }
  IMFRemoteAsyncCallback = interface(IUnknown)
	['{a27003d0-2354-4f2a-8d6a-ab7cff15437e}']
    function Invoke(const hr: HRESULT; const pRemoteResult: IUnknown): HResult; stdcall;
  end;

  //Interface IMFByteStream
  {
   Represents a byte stream from some data source, which might be a local file, a network file,
   or some other source.
   The IMFByteStream interface supports the typical stream operations, such as reading, writing, and seeking.
  }
  IMFByteStream = interface(IUnknown)
	['{a27003d0-2354-4f2a-8d6a-ab7cff15437e}']
    function GetCapabilities(out pdwCapabilities: DWord): HResult; stdcall;
    function GetLength(out pqwLength: QWORD): HResult; stdcall;
    function SetLength(const qwLength: QWORD): HResult; stdcall;
    function GetCurrentPosition(out pqwPosition: QWORD): HResult; stdcall;
    function SetCurrentPosition(const qwPosition: QWORD): HResult; stdcall;
    function IsEndOfStream(out pfEndOfStream: Boolean): HResult; stdcall;
    function Read(const pb: Byte; const cb: ULONG; out pcbRead: Byte): HResult; stdcall;
    function BeginRead(const pb: Byte; const cb: ULONG; const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndRead(const pResult: IMFAsyncResult; out pcbRead: ULONG): HResult; stdcall;
    function Write(const pb: Byte; const cb: ULONG; out pcbWritten: ULONG): HResult; stdcall;
    function BeginWrite(const pb: Byte; const cb: ULONG; const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndWrite(const pResult: IMFAsyncResult; out pcbWritten: ULONG): HResult; stdcall;
    function Seek(const SeekOrigin: MFBYTESTREAM_SEEK_ORIGIN; const llSeekOffset: LONGLONG; const dwSeekFlags: DWord; out pqwCurrentPosition: QWORD): HResult; stdcall;
    function Flush(): HResult; stdcall;
    function Close(): HResult; stdcall;
  end;

  //Interface IMFCollection
  {
   Represents a generic collection of IUnknown pointers.
   NOTE: To create an empty collection object, call MFCreateCollection.
  }
  IMFCollection = interface(IUnknown)
	['{5BC8A76B-869A-46a3-9B03-FA218A66AEBE}']
    function GetElementCount(out pcElements: DWord): HResult; stdcall;
    function GetElement(const dwElementIndex: DWord; out ppUnkElement: IUnknown): HResult; stdcall;
    function AddElement(const pUnkElement: IUnknown): HResult; stdcall;
    function RemoveElement(const dwElementIndex: DWord; ppUnkElement: IUnknown): HResult; stdcall;
    function InsertElementAt(const dwIndex: DWord; const pUnknown: IUnknown): HResult; stdcall;
    function RemoveAllElements(): HResult; stdcall;
  end;


  //Interface IMFMediaEventQueue
  {
   Provides an event queue for applications that need to implement the IMFMediaEventGenerator interface.
   This interface is exposed by a helper object that implements an event queue.
   If you are writing a component that implements the IMFMediaEventGenerator interface,
   you can use this object in your implementation.
   The event queue object is thread safe and provides methods to queue events and to pull them from
   the queue either synchronously or asynchronously.
   To create the event queue object, call MFCreateEventQueue.
  }
  IMFMediaEventQueue = interface(IUnknown)
	['{36f846fc-2256-48b6-b58e-e2b638316581}']
    function GetEvent(const dwFlags: DWord; out ppEvent: IMFMediaEvent): HResult; stdcall;
    function BeginGetEvent(const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndGetEvent(const pResult: IMFAsyncResult; out ppEvent: IMFMediaEvent): HResult; stdcall;
    function QueueEvent(const pEvent: IMFMediaEvent): HResult; stdcall;
    function QueueEventParamVar(const met: MediaEventType; const guidExtendedType: REFGUID; const hrStatus: HRESULT; const pvValue: PROPVARIANT): HResult; stdcall;
    function QueueEventParamUnk(const met: MediaEventType; const guidExtendedType: REFGUID; const hrStatus: HRESULT; const pUnk: IUnknown): HResult; stdcall;
    function Shutdown(): HResult; stdcall;
  end;

  //Interface IMFActivate
  {
   Enables the application to defer the creation of an object.
   This interface is exposed by activation objects.
   NOTE:  Typically, the application calls some function that returns an IMFActivate pointer and
          then passes that pointer to another component.
          The other component calls ActivateObject at a later time to create the object.
          In the protected media path (PMP), the IMFActivate pointer might be marshaled to the
          protected process, so that the object can be created in that process.
  }
  IMFActivate = interface(IUnknown)
	['{7FEE9E9A-4A89-47a6-899C-B6A53A70FB67}']
    function ActivateObject(const riid: REFIID; out ppv: Pointer): HResult; stdcall;
    function ShutdownObject(): HResult; stdcall;
    function DetachObject(): HResult; stdcall;
   end;

  // >= Windows 7
  //Interface IMFPluginControl
  {
   Controls how media sources and transforms are enumerated in Microsoft Media Foundation.
   To get a pointer to this interface, call MFGetPluginControl.
  }
  IMFPluginControl = interface(IUnknown)
	['{5c6c44bf-1db6-435b-9249-e8cd10fdec96}']
    function GetPreferredClsid(const pluginType: DWord; const selector: LPCWSTR; out clsid: CLSID): HResult; stdcall;
    function GetPreferredClsidByIndex(const pluginType: DWord; const index: Dword; out selector: LPWSTR; out clsid: CLSID): HResult; stdcall;
    function SetPreferredClsid(const pluginType: DWord; const selector: LPCWSTR; const clsid: CLSID): HResult; stdcall;
		function IsDisabled(const pluginTypee: DWord; const clsid: REFCLSID): HResult; stdcall;
    function GetDisabledByIndex(const pluginType: DWord; const index: DWord; out clsid: CLSID): HResult; stdcall;
    function SetDisabled(const pluginType: DWord; const clsid: REFCLSID; const disabled: Boolean): HResult; stdcall;
	end;
  // end >= Windows 7


	 //TODO: D7 won't eat this
	//function MFSerializeAttributesToStream(pAttr: IMFAttributes; dwOptions: DWord; pStm: IStream): HResult; stdcall;
	//function MFDeserializeAttributesFromStream(pAttr: IMFAttributes; dwOptions: DWord; pStm: IStream): HResult; stdcall;


implementation

end.

