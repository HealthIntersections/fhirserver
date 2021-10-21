// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: WmContainer.pas
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
// Todo: General check
// =============================================================================
// Source: wmcontainer.h
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


unit WmContainer;

interface

uses
  Windows, ComObj, Ole2, MfIdl, UnKnwn, PropSys;


const
  //Interface Identifiers
  IID_IMFASFContentInfo                : TGUID = '{B1DCA5CD-D5DA-4451-8E9E-DB5C59914EAD}';
  IID_IMFASFProfile                    : TGUID = '{D267BF6A-028B-4e0d-903D-43F0EF82D0D4}';
  IID_IMFASFStreamConfig               : TGUID = '{9E8AE8D2-DBBD-4200-9ACA-06E6DF484913}';
  IID_IMFASFMutualExclusion            : TGUID = '{12558291-E399-11D5-BC2A-00B0D0F3F4AB}';
  IID_IMFASFStreamPrioritization       : TGUID = '{699bdc27-bbaf-49ff-8e38-9c39c9b5e088}';
  IID_IMFASFSplitter                   : TGUID = '{12558295-E399-11D5-BC2A-00B0D0F3F4AB}';
  IID_IMFASFMultiplexer                : TGUID = '{57BDD80A-9B38-4838-B737-C58F670D7D4F}';
  IID_IMFASFIndexer                    : TGUID = '{53590F48-DC3B-4297-813F-787761AD7B3E}';
  IID_IMFASFStreamSelector             : TGUID = '{d01bad4a-4fa0-4a60-9349-c27e62da9d41}';
  // >= Windows 7
  IID_IMFDRMNetHelper                  : TGUID = '{3D1FF0EA-679A-4190-8D46-7FA69E8C7E15}';
  //end >= windows 7



  MFASF_MIN_HEADER_BYTES  = (SizeOf(GUID) + SizeOf(QWORD));

  MF_PD_ASF_FILEPROPERTIES_FILE_ID               : TGUID = '{3de649b4-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_FILEPROPERTIES_CREATION_TIME         : TGUID = '{3de649b6-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_FILEPROPERTIES_PACKETS               : TGUID = '{3de649b7-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_FILEPROPERTIES_PLAY_DURATION         : TGUID = '{3de649b8-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_FILEPROPERTIES_SEND_DURATION         : TGUID = '{3de649b9-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_FILEPROPERTIES_PREROLL               : TGUID = '{3de649ba-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_FILEPROPERTIES_FLAGS                 : TGUID = '{3de649bb-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_FILEPROPERTIES_MIN_PACKET_SIZE       : TGUID = '{3de649bc-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_FILEPROPERTIES_MAX_PACKET_SIZE       : TGUID = '{3de649bd-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_FILEPROPERTIES_MAX_BITRATE           : TGUID = '{3de649be-d76d-4e66-9ec9-78120fb4c7e3}';
  MF_PD_ASF_CONTENTENCRYPTION_TYPE               : TGUID = '{8520fe3d-277e-46ea-99e4-e30a86db12be}';
  MF_PD_ASF_CONTENTENCRYPTION_KEYID              : TGUID = '{8520fe3e-277e-46ea-99e4-e30a86db12be}';
  MF_PD_ASF_CONTENTENCRYPTION_SECRET_DATA        : TGUID = '{8520fe3f-277e-46ea-99e4-e30a86db12be}';
  MF_PD_ASF_CONTENTENCRYPTION_LICENSE_URL        : TGUID = '{8520fe40-277e-46ea-99e4-e30a86db12be}';
  MF_PD_ASF_CONTENTENCRYPTIONEX_ENCRYPTION_DATA  : TGUID = '{62508be5-ecdf-4924-a359-72bab3397b9d}';
  MF_PD_ASF_LANGLIST                             : TGUID = '{f23de43c-9977-460d-a6ec-32937f160f7d}';

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  MF_PD_ASF_LANGLIST_LEGACYORDER                 : TGUID = '{f23de43d-9977-460d-a6ec-32937f160f7d}';
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MF_PD_ASF_MARKER                               : TGUID = '{5134330e-83a6-475e-a9d5-4fb875fb2e31}';
  MF_PD_ASF_SCRIPT                               : TGUID = '{e29cd0d7-d602-4923-a7fe-73fd97ecc650}';
  MF_PD_ASF_CODECLIST                            : TGUID = '{e4bb3509-c18d-4df1-bb99-7a36b3cc4119}';
  MF_PD_ASF_METADATA_IS_VBR                      : TGUID = '{5fc6947a-ef60-445d-b449-442ecc78b4c1}';
  MF_PD_ASF_METADATA_V8_VBRPEAK                  : TGUID = '{5fc6947b-ef60-445d-b449-442ecc78b4c1}';
  MF_PD_ASF_METADATA_V8_BUFFERAVERAGE            : TGUID = '{5fc6947c-ef60-445d-b449-442ecc78b4c1}';
  MF_PD_ASF_METADATA_LEAKY_BUCKET_PAIRS          : TGUID = '{5fc6947d-ef60-445d-b449-442ecc78b4c1}';
  MF_PD_ASF_DATA_START_OFFSET                    : TGUID = '{e7d5b3e7-1f29-45d3-8822-3e78fae272ed}';
  MF_PD_ASF_DATA_LENGTH                          : TGUID = '{e7d5b3e8-1f29-45d3-8822-3e78fae272ed}';
  MF_SD_ASF_EXTSTRMPROP_LANGUAGE_ID_INDEX        : TGUID = '{48f8a522-305d-422d-8524-2502dda33680}';
  MF_SD_ASF_EXTSTRMPROP_AVG_DATA_BITRATE         : TGUID = '{48f8a523-305d-422d-8524-2502dda33680}';
  MF_SD_ASF_EXTSTRMPROP_AVG_BUFFERSIZE           : TGUID = '{48f8a524-305d-422d-8524-2502dda33680}';
  MF_SD_ASF_EXTSTRMPROP_MAX_DATA_BITRATE         : TGUID = '{48f8a525-305d-422d-8524-2502dda33680}';
  MF_SD_ASF_EXTSTRMPROP_MAX_BUFFERSIZE           : TGUID = '{48f8a526-305d-422d-8524-2502dda33680}';
  MF_SD_ASF_STREAMBITRATES_BITRATE               : TGUID = '{a8e182ed-afc8-43d0-b0d1-f65bad9da558}';
  MF_SD_ASF_METADATA_DEVICE_CONFORMANCE_TEMPLATE : TGUID = '{245e929d-c44e-4f7e-bb3c-77d4dfd27f8a}';
  MF_PD_ASF_INFO_HAS_AUDIO                       : TGUID = '{80e62295-2296-4a44-b31c-d103c6fed23c}';
  MF_PD_ASF_INFO_HAS_VIDEO                       : TGUID = '{80e62296-2296-4a44-b31c-d103c6fed23c}';
  MF_PD_ASF_INFO_HAS_NON_AUDIO_VIDEO             : TGUID = '{80e62297-2296-4a44-b31c-d103c6fed23c}';


  MF_ASFPROFILE_MINPACKETSIZE                    : TGUID = '{22587626-47de-4168-87f5-b5aa9b12a8f0}';
  MF_ASFPROFILE_MAXPACKETSIZE                    : TGUID = '{22587627-47de-4168-87f5-b5aa9b12a8f0}';


  MF_ASFSTREAMCONFIG_LEAKYBUCKET1                : TGUID = '{c69b5901-ea1a-4c9b-b692-e2a0d29a8add}';
  MF_ASFSTREAMCONFIG_LEAKYBUCKET2                : TGUID = '{c69b5902-ea1a-4c9b-b692-e2a0d29a8add}';
  MFASFSampleExtension_SampleDuration            : TGUID = '{c6bd9450-867f-4907-83a3-c77921b733ad}';
  MFASFSampleExtension_OutputCleanPoint          : TGUID = '{f72a3c6f-6eb4-4ebc-b192-09ad9759e828}';
  MFASFSampleExtension_SMPTE                     : TGUID = '{399595ec-8667-4e2d-8fdb-98814ce76c1e}';
  MFASFSampleExtension_FileName                  : TGUID = '{e165ec0e-19ed-45d7-b4a7-25cbd1e28e9b}';
  MFASFSampleExtension_ContentType               : TGUID = '{d590dc20-07bc-436c-9cf7-f3bbfbf1a4dc}';
  MFASFSampleExtension_PixelAspectRatio          : TGUID = '{1b1ee554-f9ea-4bc8-821a-376b74e4c4b8}';
  MFASFSampleExtension_Encryption_SampleID       : TGUID = '{6698B84E-0AFA-4330-AEB2-1C0A98D7A44D}';
  MFASFSampleExtension_Encryption_KeyID          : TGUID = '{76376591-795f-4da1-86ed-9d46eca109a9}';


  MFASFMutexType_Language                        : TGUID = '{72178C2B-E45B-11D5-BC2A-00B0D0F3F4AB}';
  MFASFMutexType_Bitrate                         : TGUID = '{72178C2C-E45B-11D5-BC2A-00B0D0F3F4AB}';
  MFASFMutexType_Presentation                    : TGUID = '{72178C2D-E45B-11D5-BC2A-00B0D0F3F4AB}';
  MFASFMutexType_Unknown                         : TGUID = '{72178C2E-E45B-11D5-BC2A-00B0D0F3F4AB}';


  MFASFSPLITTER_PACKET_BOUNDARY                  : TGUID = '{fe584a05-e8d6-42e3-b176-f121175fb6f}';


  //#if (WINVER >= _WIN32_WINNT_WIN7)
  MFASFINDEXER_TYPE_TIMECODE                     : TGUID = '{49815231-6bad-44fd-810a-3f60984ec7fd}';
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

  MFASFINDEXER_PER_ENTRY_BYTES_DYNAMIC= $FFFF;
  MFASFINDEXER_NO_FIXED_INTERVAL      = $FFFFFFFF;
  MFASFINDEXER_READ_FOR_REVERSEPLAYBACK_OUTOFDATASEGMENT= $FFFFFFFFFFFFFFFF;
  MFASFINDEXER_APPROX_SEEK_TIME_UNKNOWN= $FFFFFFFFFFFFFFFF;


  MFPKEY_ASFMEDIASINK_BASE_SENDTIME          : PROPERTYKEY = (fmtid: (D1: $cddcbc82; D2: $3411; D3: $4119;
                                                              D4: ($91, $35, $84, $23, $c4, $1b, $39, $57)); pid: 3);
  MFPKEY_ASFMEDIASINK_AUTOADJUST_BITRATE     : PROPERTYKEY = (fmtid: (D1: $cddcbc82; D2: $3411; D3: $4119;
                                                              D4: ($91, $35, $84, $23, $c4, $1b, $39, $57)); pid: 4);
  MFPKEY_ASFMEDIASINK_DRMACTION              : PROPERTYKEY = (fmtid: (D1: $a1db6f6c; D2: $1d0a; D3: $4cb6;
                                                              D4: ($82, $54, $cb, $36, $be, $ed, $bc, $48)); pid: 5);
  MFPKEY_ASFSTREAMSINK_CORRECTED_LEAKYBUCKET : PROPERTYKEY = (fmtid: (D1: $a2f152fb; D2: $8ad9; D3: $0a11;
                                                              D4: ($b3, $45, $2c, $e2, $fa, $d8, $72, $3d)); pid: 1);


  //Define WMContainer constants
  MFASF_MAX_STREAM_NUMBER             = 127;
  MFASF_INVALID_STREAM_NUMBER         = (MFASF_MAX_STREAM_NUMBER + 1);
  MFASF_PAYLOADEXTENSION_MAX_SIZE     = $FF;
  MFASF_PAYLOADEXTENSION_VARIABLE_SIZE= $FFFF;
  MFASF_DEFAULT_BUFFER_WINDOW_MS      = 3000;




type
  MFASF_SPLITTERFLAGS      = (
    MFASF_SPLITTER_REVERSE = $1,
    MFASF_SPLITTER_WMDRM   = $2
  );
  TMfasfSplitterflags = MFASF_SPLITTERFLAGS;

type
  cwASF_STATUSFLAGS                  = (
    ASF_STATUSFLAGS_INCOMPLETE     = $1,
    ASF_STATUSFLAGS_NONFATAL_ERROR = $2
  );
  ASF_STATUSFLAGS = cwASF_STATUSFLAGS;

type
  cwMFASF_MULTIPLEXERFLAGS                 = (
    MFASF_MULTIPLEXER_AUTOADJUST_BITRATE = $1
  );
  MFASF_MULTIPLEXERFLAGS = cwMFASF_MULTIPLEXERFLAGS;

type
  cwASF_MUX_STATISTICS = record
    cFramesWritten: DWORD;
    cFramesDropped: DWORD;
  end;
  ASF_MUX_STATISTICS = cwASF_MUX_STATISTICS;

type
  MFASF_INDEXERFLAGS                       = (
    MFASF_INDEXER_WRITE_NEW_INDEX          = $1,
    MFASF_INDEXER_READ_FOR_REVERSEPLAYBACK = $2,
    MFASF_INDEXER_WRITE_FOR_LIVEREAD       = $4
  );
  MFASF_INDEXER_FLAGS = MFASF_INDEXERFLAGS;

type
  _ASF_INDEX_IDENTIFIER = record
    guidIndexType: TGUID;
    wStreamNumber: WORD;
  end;
  ASF_INDEX_IDENTIFIER = _ASF_INDEX_IDENTIFIER;

type
  _ASF_INDEX_DESCRIPTOR = record
    Identifier: ASF_INDEX_IDENTIFIER;
    cPerEntryBytes: WORD;
    szDescription: array[0..31] of WideChar;
    dwInterval: DWORD;
  end;
  ASF_INDEX_DESCRIPTOR = _ASF_INDEX_DESCRIPTOR;

type
  MFASF_STREAMSELECTORFLAGS                  = (
    MFASF_STREAMSELECTOR_DISABLE_THINNING    = $1,
    MFASF_STREAMSELECTOR_USE_AVERAGE_BITRATE = $2
  );
  MFASF_STREAMSELECTOR_FLAGS = MFASF_STREAMSELECTORFLAGS;

type
  cwASF_SELECTION_STATUS         = (
    ASF_STATUS_NOTSELECTED     = 0,
    ASF_STATUS_CLEANPOINTSONLY = 1,
    ASF_STATUS_ALLDATAUNITS    = 2
  );
  ASF_SELECTION_STATUS = cwASF_SELECTION_STATUS;

type
  _MFSINK_WMDRMACTION             = (
    MFSINK_WMDRMACTION_UNDEFINED  = 0,
    MFSINK_WMDRMACTION_ENCODE     = 1,
    MFSINK_WMDRMACTION_TRANSCODE  = 2,
    MFSINK_WMDRMACTION_TRANSCRYPT = 3,
    MFSINK_WMDRMACTION_LAST       = 3
  );
  MFSINK_WMDRMACTION = _MFSINK_WMDRMACTION;

type
  //Forward Interfaces Declarations
  IMFASFContentInfo = interface;
  IMFASFProfile = interface;
  IMFASFStreamConfig = interface;
  IMFASFMutualExclusion = interface;
  IMFASFStreamPrioritization = interface;
  IMFASFSplitter = interface;
  IMFASFMultiplexer = interface;
  IMFASFIndexer = interface;
  IMFASFStreamSelector = interface;
  IMFDRMNetHelper = interface;

  // INTERFACES ///////////////////////////////////////////////////////////////

  //Interface IMFASFContentInfo
  //Provides methods to work with the header section of files conforming to the Advanced Systems Format (ASF) specification.
  //The ASF ContentInfo Object exposes this interface.
  //To create the get a pointer to the IMFASFContentInfo interface, call MFCreateASFContentInfo.
  IMFASFContentInfo = interface(IUnknown)
  ['{B1DCA5CD-D5DA-4451-8E9E-DB5C59914EAD}']
    function GetHeaderSize(const pIStartOfContent: IMFMediaBuffer; out cbHeaderSize: QWORD): HResult; stdcall;
    function ParseHeader(const pIHeaderBuffer: IMFMediaBuffer; const cbOffsetWithinHeader: QWORD): HResult; stdcall;
    function GenerateHeader(var pIHeader: IMFMediaBuffer; out pcbHeader: DWORD): HResult; stdcall;
    function GetProfile(out ppIProfile: PIMFASFProfile): HResult; stdcall;
    function SetProfile(const pIProfile: IMFASFProfile): HResult; winapi;
    function GeneratePresentationDescriptor(out ppIPresentationDescriptor: PIMFPresentationDescriptor): HResult; stdcall;
    function GetEncodingConfigurationPropertyStore(const wStreamNumber: WORD; out ppIStore: PIPropertyStore): HResult; stdcall;
  end;


  //interface IMFASFProfile
  //Manages an Advanced Systems Format (ASF) profile.
  //A profile is a collection of information that describes the configuration of streams that will be
  //included in an ASF file. Information about the relationships between streams is also included in the profile.
  //An IMFASFProfile interface exists for every ASF profile object.
  //To create an ASF profile object, call MFCreateASFProfile or MFCreateASFProfileFromPresentationDescriptor.
  IMFASFProfile = interface(IUnknown)
  ['{D267BF6A-028B-4e0d-903D-43F0EF82D0D4}']
    function GetStreamCount(out pcStreams: DWORD): HResult; stdcall;
    function GetStream(const dwStreamIndex: DWORD; out pwStreamNumber: WORD; out ppIStream: PIMFASFStreamConfig): HResult; stdcall;
    function GetStreamByNumber(const wStreamNumber: WORD; out ppIStream: PIMFASFStreamConfig): HResult; stdcall;
    function SetStream(const pIStream: IMFASFStreamConfig): HResult; stdcall;
    function RemoveStream(const wStreamNumber: WORD): HResult; stdcall;
    function CreateStream(const pIMediaType: IMFMediaType; out ppIStream: PIMFASFStreamConfig): HResult; stdcall;
    function GetMutualExclusionCount(out pcMutexs: DWORD): HResult; stdcall;
    function GetMutualExclusion(const dwMutexIndex: DWORD; out ppIMutex: PIMFASFMutualExclusion): HResult; stdcall;
    function AddMutualExclusion(const pIMutex: IMFASFMutualExclusion): HResult; stdcall;
    function RemoveMutualExclusion(const dwMutexIndex: DWORD): HResult; stdcall;
    function CreateMutualExclusion(out ppIMutex: PIMFASFMutualExclusion): HResult; stdcall;
    function GetStreamPrioritization(out ppIStreamPrioritization: PIMFASFStreamPrioritization): HResult; stdcall;
    function AddStreamPrioritization(const pIStreamPrioritization: IMFASFStreamPrioritization): HResult; stdcall;
    function RemoveStreamPrioritization(): HResult; stdcall;
    function CreateStreamPrioritization(out ppIStreamPrioritization: PIMFASFStreamPrioritization): HResult; stdcall;
    function Clone(out ppIProfile: PIMFASFProfile): HResult; stdcall;
  end;


  //Interface IMFASFStreamConfig
  //Configures the settings of a stream in an ASF file.
  //The ASF stream configuration object exposes this interface.
  //To obtain a pointer to this interface, call the IMFASFProfile.CreateStream method.
  IMFASFStreamConfig = interface(IMFAttributes)
  ['{9E8AE8D2-DBBD-4200-9ACA-06E6DF484913}']
    function GetStreamType(out pguidStreamType: TGUID): HResult; stdcall;
    function GetStreamNumber(): WORD; stdcall;
    function SetStreamNumber(const wStreamNum: WORD): HResult; stdcall;
    function GetMediaType(out ppIMediaType: PIMFMediaType): HResult; stdcall;
    function SetMediaType(const pIMediaType: IMFMediaType): HResult; stdcall;
    function GetPayloadExtensionCount(out pcPayloadExtensions: WORD): HResult; stdcall;
    function GetPayloadExtension(const wPayloadExtensionNumber: WORD; out pguidExtensionSystemID: TGUID; out pcbExtensionDataSize: WORD;
                                 out pbExtensionSystemInfo: PByte; var pcbExtensionSystemInfo: DWORD): HResult; stdcall;
    function AddPayloadExtension(const guidExtensionSystemID: TGUID; const cbExtensionDataSize: WORD; const pbExtensionSystemInfo: PByte; const cbExtensionSystemInfo: DWORD): HResult; stdcall;
    function RemoveAllPayloadExtensions(): HResult; stdcall;
    function Clone(out ppIStreamConfig: PIMFASFStreamConfig): HResult; stdcall;
  end;


  //Interface IMFASFMutualExclusion
  //Configures an Advanced Systems Format (ASF) mutual exclusion object,
  //which manages information about a group of streams in an ASF profile that are mutually exclusive.
  //When streams or groups of streams are mutually exclusive, only one of them is read at a time, they are not read concurrently.
  //A common example of mutual exclusion is a set of streams that each include the same content encoded at a different bit rate.
  //The stream that is used is determined by the available bandwidth to the reader.
  //An IMFASFMutualExclusion interface exists for every ASF mutual exclusion object.
  //A pointer to this interface is obtained when you create the object using the IMFASFProfile.CreateMutualExclusion method.
  //NOTE: An ASF profile object can support multiple mutual exclusions. Each must be configured using a separate ASF mutual exclusion object.
  IMFASFMutualExclusion = interface(IUnknown)
  ['{9E8AE8D2-DBBD-4200-9ACA-06E6DF484913}']
    function GetType(out pguidType: TGUID): HResult; stdcall;
    function SetType(const guidType: REFGUID): HResult; stdcall;
    function GetRecordCount(out pdwRecordCount: DWORD): HResult; stdcall;
    function GetStreamsForRecord(const dwRecordNumber: DWORD; out pwStreamNumArray: WORD; var var pcStreams: DWORD): HResult; stdcall;
    function AddStreamForRecord(const dwRecordNumber: DWORD; const wStreamNumber: WORD): HResult; stdcall;
    function RemoveStreamFromRecord(const dwRecordNumber: DWORD; const wStreamNumber: WORD): HResult; stdcall;
    function RemoveRecord(const dwRecordNumber: DWORD): HResult; stdcall;
    function AddRecord(out pdwRecordNumber: DWORD): HResult; stdcall;
    function Clone(out ppIMutex: PIMFASFMutualExclusion): HResult; stdcall;
  end;


  //Interface IMFASFStreamPrioritization (not implemented)
  //Manages information about the relative priorities of a group of streams in an Advanced Systems Format (ASF) profile.
  //This interface manages information about the relative priorities of a group of streams in an ASF profile.
  //Priority is used in streaming to determine which streams should be dropped first when available bandwidth decreases.
  //The ASF stream prioritization object exposes this interface.
  //The stream prioritization object maintains a list of stream numbers in priority order.
  //The methods of this interface manipulate and interrogate that list.
  //To obtain a pointer to this interface, call the IMFASFProfile.CreateStreamPrioritization method.
  //NOTE: Date 6/4/2012: This interface is not implemented.
  IMFASFStreamPrioritization = interface(IUnknown)
  ['{699bdc27-bbaf-49ff-8e38-9c39c9b5e088}']
    function GetStreamCount(out pdwStreamCount: DWORD): HResult; stdcall;
    function GetStream(const dwStreamIndex: DWORD; out pwStreamNumber: WORD; out pwStreamFlags: WORD): HResult; stdcall;
    function AddStream(const wStreamNumber: WORD; const wStreamFlags: WORD): HResult; stdcall;
    function RemoveStream(const dwStreamIndex: DWORD): HResult; stdcall;
    function Clone(out ppIStreamPrioritization: PIMFASFStreamPrioritization): HResult; stdcall;
  end;


  //Interface IMFASFSplitter
  //Provides methods to read data from an Advanced Systems Format (ASF) file.
  //The ASF splitter object exposes this interface.
  //To create the ASF splitter, MFCreateASFSplitter.
  IMFASFSplitter = interface(IUnknown)
  ['{12558295-E399-11D5-BC2A-00B0D0F3F4AB}']
    function Initialize(const pIContentInfo: IMFASFContentInfo): HResult; stdcall;
    function SetFlags(const dwFlags: DWORD): HResult; stdcall;
    function GetFlags(out pdwFlags: DWORD): HResult; stdcall;
    function SelectStreams(const pwStreamNumbers: WORD; const wNumStreams: WORD): HResult; stdcall;
    function GetSelectedStreams(out pwStreamNumbers: WORD; var pwNumStreams: WORD): HResult; stdcall;
    function ParseData(const pIBuffer: IMFMediaBuffer; const cbBufferOffset: DWORD; const cbLength: DWORD): HResult; stdcall;
    function GetNextSample(out pdwStatusFlags: DWORD; out pwStreamNumber: WORD; out ppISample: PIMFSample): HResult; stdcall;
    function Flush(): HResult; stdcall;
    function GetLastSendTime(pdwLastSendTime: DWORD): HResult; stdcall;
  end;


  //Interface IMFASFMultiplexer
  //Provides methods to create Advanced Systems Format (ASF) data packets.
  //The methods of this interface process input samples into the packets that make up an ASF data section.
  //The ASF multiplexer exposes this interface. To create the ASF multiplexer, call MFCreateASFMultiplexer defined in this unit.
  IMFASFMultiplexer = interface(IUnknown)
  ['{57BDD80A-9B38-4838-B737-C58F670D7D4F}']
    function Initialize(const pIContentInfo: IMFASFContentInfo): HResult; stdcall;
    function SetFlags(const dwFlags: DWORD): HResult; stdcall;
    function GetFlags(out pdwFlags: DWORD): HResult; stdcall;
    function ProcessSample(const wStreamNumber: WORD; const pISample: IMFSample; const hnsTimestampAdjust: LONGLONG): HResult; stdcall;
    function GetNextPacket(out pdwStatusFlags: DWORD; out var ppIPacket: PIMFSample): HResult; stdcall;
    function Flush(): HResult; stdcall;
    function End(var pIContentInfo: IMFASFContentInfo): HResult; stdcall;
    function GetStatistics(const wStreamNumber: WORD; out pMuxStats: ASF_MUX_STATISTICS): HResult; stdcall;
    function SetSyncTolerance(const msSyncTolerance: DWORD): HResult; stdcall;
  end;


  //Interface IMFASFIndexer
  //Provides methods to work with indexes in Systems Format (ASF) files.
  //The ASF indexer object exposes this interface.
  //To create the ASF indexer, call MFCreateASFIndexer defined in this unit
  IMFASFIndexer = interface(IUnknown)
  ['{53590F48-DC3B-4297-813F-787761AD7B3E}']
    function SetFlags(const dwFlags: DWORD): HResult; stdcall;
    function GetFlags(out pdwFlags: DWORD): HResult; stdcall;
    function Initialize(const pIContentInfo: IMFASFContentInfo): HResult; stdcall;
    function GetIndexPosition(const pIContentInfo: IMFASFContentInfo; out pcbIndexOffset: QWORD): HResult; stdcall;
    function SetIndexByteStreams(const ppIByteStreams: PIMFByteStream; const cByteStreams: DWORD): HResult; stdcall;
    function GetIndexByteStreamCount(out pcByteStreams: DWORD): HResult; stdcall;
    function GetIndexStatus(const pIndexIdentifier: ASF_INDEX_IDENTIFIER; out pfIsIndexed: BOOL; out pbIndexDescriptor: PByte; var pcbIndexDescriptor: DWORD): HResult; stdcall;
    function SetIndexStatus(const pbIndexDescriptor: PByte; const cbIndexDescriptor: DWORD; const fGenerateIndex: BOOL): HResult; stdcall;
    function GetSeekPositionForValue(const pvarValue: PROPVARIANT; const pIndexIdentifier: ASF_INDEX_IDENTIFIER; out pcbOffsetWithinData: QWORD;
                                     out phnsApproxTime: MFTIME; out pdwPayloadNumberOfStreamWithinPacket: DWORD): HResult; stdcall;
    function GenerateIndexEntries(const pIASFPacketSample: IMFSample): HResult; stdcall;
    function CommitIndex(const pIContentInfo: IMFASFContentInfo): HResult; stdcall;
    function GetIndexWriteSpace(out pcbIndexWriteSpace: QWORD): HResult; stdcall;
    function GetCompletedIndex(const pIIndexBuffer: IMFMediaBuffer; const cbOffsetWithinIndex: QWORD): HResult; stdcall;
  end;


  //Interface IMFASFStreamSelector
  //Selects streams in an Advanced Systems Format (ASF) file, based on the mutual exclusion information in the ASF header.
  //The ASF stream selector object exposes this interface.
  //To create the ASF stream selector, call MFCreateASFStreamSelector defined in this unit
  IMFASFStreamSelector = interface(IUnknown)
  ['{d01bad4a-4fa0-4a60-9349-c27e62da9d41}']
    function GetStreamCount(out pcStreams: DWORD): HResult; stdcall;
    function GetOutputCount(out pcOutputs: DWORD): HResult; stdcall;
    function GetOutputStreamCount(const dwOutputNum: DWORD; pcStreams: DWORD): HResult; stdcall;
    function GetOutputStreamNumbers(const dwOutputNum: DWORD; rgwStreamNumbers: WORD): HResult; stdcall;
    function GetOutputFromStream(const wStreamNum: WORD; out pdwOutput: DWORD): HResult; stdcall;
    function GetOutputOverride(const dwOutputNum: DWORD; out pSelection: ASF_SELECTION_STATUS): HResult; stdcall;
    function SetOutputOverride(const dwOutputNum: DWORD; const Selection: ASF_SELECTION_STATUS): HResult; stdcall;
    function GetOutputMutexCount(const dwOutputNum: DWORD; out pcMutexes: DWORD): HResult; stdcall;
    function GetOutputMutex(const dwOutputNum: DWORD; const dwMutexNum: DWORD; out ppMutex: PIUnknown): HResult; stdcall;
    function SetOutputMutexSelection(const dwOutputNum: DWORD; const dwMutexNum: DWORD; const wSelectedRecord: WORD): HResult; stdcall;
    function GetBandwidthStepCount(out pcStepCount: DWORD): HResult; stdcall;
    function GetBandwidthStep(const dwStepNum: DWORD; out pdwBitrate: DWORD; out rgwStreamNumbers: WORD; out rgSelections: ASF_SELECTION_STATUS): HResult; stdcall;
    function BitrateToStepNumber(const dwBitrate: DWORD; out pdwStepNum: DWORD): HResult; stdcall;
    function SetStreamSelectorFlags(const dwStreamSelectorFlags: DWORD): HResult; stdcall;
  end;


  //#if (WINVER >= _WIN32_WINNT_WIN7)
  //Interface IMFDRMNetHelper
  IMFDRMNetHelper = interface(IUnknown)
  ['{3D1FF0EA-679A-4190-8D46-7FA69E8C7E15}']
    function ProcessLicenseRequest(const pLicenseRequest: PByte; const cbLicenseRequest: DWORD; out ppLicenseResponse: PByte;
                                   out pcbLicenseResponse: DWORD; out pbstrKID: BSTR): HResult; winapi;
    function GetChainedLicenseResponse(out ppLicenseResponse: PByte; out pcbLicenseResponse: DWORD): HResult; winapi;
  end;
  //#endif (WINVER >= _WIN32_WINNT_WIN7)


  function MFCreateASFContentInfo(out ppIContentInfo: PIMFASFContentInfo): HResult; stdcall;
  function MFCreateASFProfile(out ppIProfile: PIMFASFProfile): HResult; stdcall;
  function MFCreateASFProfileFromPresentationDescriptor(const pIPD: IMFPresentationDescriptor; out ppIProfile: PIMFASFProfile): HResult; stdcall;
  function MFCreatePresentationDescriptorFromASFProfile(const pIProfile: IMFASFProfile; out ppIPD: PIMFPresentationDescriptor): HResult; stdcall;
  function MFCreateASFMultiplexer(out ppIMultiplexer: PIMFASFMultiplexer): HResult; stdcall;
  function MFCreateASFIndexer(out ppIIndexer: PIMFASFIndexer): HResult; stdcall;
  function MFCreateASFIndexerByteStream(const pIContentByteStream: IMFByteStream; const cbIndexStartOffset: QWORD; out pIIndexByteStream: PIMFByteStream): HResult; stdcall;
  function MFCreateASFStreamSelector(const pIASFProfile: IMFASFProfile; out ppSelector: PIMFASFStreamSelector): HResult; winapi;
  function MFCreateASFMediaSink(var pIByteStream: IMFByteStream; var ppIMediaSink: PIMFMediaSink): HResult; winapi;
  function MFCreateASFMediaSinkActivate(const pwszFileName: PWideChar; var pContentInfo: IMFASFContentInfo; var ppIActivate: PIMFActivate): HResult; winapi;
  function MFCreateWMVEncoderActivate(var pMediaType: IMFMediaType; var pEncodingConfigurationProperties: IPropertyStore; var ppActivate: PIMFActivate): HResult; winapi;
  function MFCreateWMAEncoderActivate(var pMediaType: IMFMediaType; var pEncodingConfigurationProperties: IPropertyStore; var ppActivate: PIMFActivate): HResult; winapi;
  function MFCreateASFStreamingMediaSink(var pIByteStream: IMFByteStream; var ppIMediaSink: PIMFMediaSink): HResult; winapi;
  function MFCreateASFStreamingMediaSinkActivate(var pByteStreamActivate: IMFActivate; var pContentInfo: IMFASFContentInfo; var ppIActivate: PIMFActivate): HResult; winapi;
  function MFCreateASFSplitter(out ppISplitter: PIMFASFSplitter): HResult; stdcall;


  //Additional Prototypes for ALL interfaces */

  //end of Additional Prototypes */


implementation

end.
