// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - MfApi.pas
// Kind: Pascal Unit
// Release date: 27-06-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: Requires Windows Vista or later.
//
// Intiator(s): Tony (maXcomX), Peter (OzShips)
//
//----------------------------------------------------------------------------
// CHANGE LOG
// Date       Person               Reason
// ---------- -------------------- --------------------------------------------
// 2012Jul07  Tony Kalf            Initial conversion
// 2012Jul29  Peter Larson         Add external function references, minor changes
//updt 090812 Peter Larson         minor fixes
//----------------------------------------------------------------------------
//
// Remarks: MFAPI.pas is the unit containing the APIs for using the MF platform.
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
// Todo: -
// =============================================================================
// Source: mfapi.h
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


unit MfApi;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}

interface

uses
  ActiveX, Direct3d, DirectShow9,                       //updt 090812 for IMediaBuffer, includes
  MMSystem,
  MMReg,
  Windows, MfObjects;
// ComObj,

Const
  Version               = '0.1.0001';

type
  MFWORKITEM_KEY = UInt64;

{$I mfpack.inc}

const

//--------------------- Media types

// Major types /////////////////////////////////////////////////////////////////

  MFMediaType_Default                         : TGuid = '{81A412E6-8103-4B06-857F-1862781024AC}';
  MFMediaType_Audio                           : TGuid = '{73647561-0000-0010-8000-00AA00389B71}';
  MFMediaType_Video                           : TGuid = '{73646976-0000-0010-8000-00AA00389B71}';
  MFMediaType_Protected                       : TGuid = '{7b4b6fe6-9d04-4494-be14-7e0bd076c8e4}';
  MFMediaType_SAMI                            : TGuid = '{e69669a0-3dcd-40cb-9e2e-3708387c0616}';
  MFMediaType_Script                          : TGuid = '{72178C22-E45B-11D5-BC2A-00B0D0F3F4AB}';
  MFMediaType_Image                           : TGuid = '{72178C23-E45B-11D5-BC2A-00B0D0F3F4AB}';
  MFMediaType_HTML                            : TGuid = '{72178C24-E45B-11D5-BC2A-00B0D0F3F4AB}';
  MFMediaType_Binary                          : TGuid = '{72178C25-E45B-11D5-BC2A-00B0D0F3F4AB}';
  MFMediaType_FileTransfer                    : TGuid = '{72178C26-E45B-11D5-BC2A-00B0D0F3F4AB}';

//--------------------- Various status/attribute GUIDs
  MF_EVENT_TOPOLOGY_STATUS                    : TGuid = '{30c5018d-9a53-454b-ad9e-6d5f8fa7c43b}';
  MF_EVENT_START_PRESENTATION_TIME            : TGuid = '{5ad914d0-9b45-4a8d-a2c0-81d1e50bfb07}';
  MF_EVENT_PRESENTATION_TIME_OFFSET           : TGuid = '{5ad914d1-9b45-4a8d-a2c0-81d1e50bfb07}';  //updt 090812 correct GUID string
  MF_EVENT_START_PRESENTATION_TIME_AT_OUTPUT  : TGuid = '{5AD914D2-9B45-4a8d-A2C0-81D1E50BFB07}';
  MF_EVENT_SOURCE_FAKE_START                  : TGuid = '{a8cc55a7-6b31-419f-845d-ffb351a2434b}';
  MF_EVENT_SOURCE_PROJECTSTART                : TGuid = '{a8cc55a8-6b31-419f-845d-ffb351a2434b}';
  MF_EVENT_SOURCE_ACTUAL_START                : TGuid = '{a8cc55a9-6b31-419f-845d-ffb351a2434b}';
  MF_EVENT_SOURCE_TOPOLOGY_CANCELED           : TGuid = '{DB62F650-9A5E-4704-ACF3-563BC6A73364}';
  MF_EVENT_SOURCE_CHARACTERISTICS             : TGuid = '{47DB8490-8B22-4f52-AFDA-9CE1B2D3CFA8}';
  MF_EVENT_SOURCE_CHARACTERISTICS_OLD         : TGuid = '{47DB8491-8B22-4f52-AFDA-9CE1B2D3CFA8}';
  MF_EVENT_DO_THINNING                        : TGuid = '{321EA6FB-DAD9-46e4-B31D-D2EAE7090E30}';
  MF_EVENT_SCRUBSAMPLE_TIME                   : TGuid = '{9AC712B3-DCB8-44d5-8D0C-37455A2782E3}';
  MF_EVENT_OUTPUT_NODE                        : TGuid = '{830f1a8b-c060-46dd-a801-1c95dec9b107}';
  MF_EVENT_MFT_INPUT_STREAM_ID                : TGuid = '{F29C2CCA-7AE6-42d2-B284-BF837CC874E2}';
  MF_EVENT_MFT_CONTEXT                        : TGuid = '{B7CD31F1-899E-4b41-80C9-26A896D32977}';

const
  {$EXTERNALSYM MF_SDK_VERSION}
  MF_SDK_VERSION                      = $0002;

  {$EXTERNALSYM MF_SDK_VERSION}
//##TEMP: duplicate  MF_SDK_VERSION                      = $0001;

  {$EXTERNALSYM MF_API_VERSION}
  MF_API_VERSION                      = $0070;  // This value is unused in the Win7 release and left at its Vista release value
  {$EXTERNALSYM MF_VERSION}
  MF_VERSION                          = (MF_SDK_VERSION shl 16 or MF_API_VERSION);
  {$EXTERNALSYM MFSTARTUP_NOSOCKET}
  MFSTARTUP_NOSOCKET                  = $1;
  {$EXTERNALSYM MFSTARTUP_LITE}
  MFSTARTUP_LITE                      = (MFSTARTUP_NOSOCKET);
  {$EXTERNALSYM MFSTARTUP_FULL}
  MFSTARTUP_FULL                      = 0;

const //updt 090812 replace: type
//  cwMF_TOPOSTATUS = record                                      //updt 090812 remove: not a record
    // MF_TOPOSTATUS_INVALID: Invalid value; will not be sent
    MF_TOPOSTATUS_INVALID = 0;

    // MF_TOPOSTATUS_READY: The topology has been put in place and is
    // ready to start.  All GetService calls to the Media Session will use
    // this topology.
    MF_TOPOSTATUS_READY     = 100;

    // MF_TOPOSTATUS_STARTED_SOURCE: The Media Session has started to read
    // and process data from the Media Source(s) in this topology.
    MF_TOPOSTATUS_STARTED_SOURCE = 200;

    // MF_TOPOSTATUS_DYNAMIC_CHANGED: The topology has been dynamic changed
    // due to the format change.
    MF_TOPOSTATUS_DYNAMIC_CHANGED = 210;

    // MF_TOPOSTATUS_SINK_SWITCHED: The Media Sinks in the pipeline have
    // switched from a previous topology to this topology.
    // Note that this status does not get sent for the first topology;
    // applications can assume that the sinks are playing the first
    // topology when they receive MESessionStarted.
    MF_TOPOSTATUS_SINK_SWITCHED = 300;

    // MF_TOPOSTATUS_ENDED: Playback of this topology is complete.
    // Before deleting this topology, however, the application should wait
    // for either MESessionEnded or the MF_TOPOSTATUS_STARTED_SOURCE status
    // on the next topology to ensure that the Media Session is no longer
    // using this topology.
    MF_TOPOSTATUS_ENDED = 400;
//    end;                           //updt 090812 Remove record

  // Session capabilities bitflags
  {$EXTERNALSYM MFSESSIONCAP_START}
  MFSESSIONCAP_START                  = $00000001;
  {$EXTERNALSYM MFSESSIONCAP_SEEK}
  MFSESSIONCAP_SEEK                   = $00000002;
  {$EXTERNALSYM MFSESSIONCAP_PAUSE}
  MFSESSIONCAP_PAUSE                  = $00000004;
  {$EXTERNALSYM MFSESSIONCAP_RATE_FORWARD}
  MFSESSIONCAP_RATE_FORWARD           = $00000010;
  {$EXTERNALSYM MFSESSIONCAP_RATE_REVERSE}
  MFSESSIONCAP_RATE_REVERSE           = $00000020;

////////////////////////////////////////////////////////////////////////////////
///////////////////////////////   Startup/Shutdown  ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  // Initializes the platform object.
  // Must be called before using Media Foundation.
  // A matching MFShutdown call must be made when the application is done using
  // Media Foundation.
  // The "Version" parameter should be set to MF_API_VERSION.
  // Application should not call MFStartup / MFShutdown from workqueue threads
  //
  // Default = MFSTARTUP_FULL
  function MFStartup(const Version: ULONG; const dwFlags: DWORD): HRESULT; stdcall;


  // Shuts down the platform object.
  // Releases all resources including threads.
  // Application should call MFShutdown the same number of times as MFStartup
  // Application should not call MFStartup / MFShutdown from workqueue threads
  function MFShutdown: HRESULT; stdcall;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////    Platform    ///////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  // These functions can be used to keep the MF platform object in place.
  // Every call to MFLockPlatform should have a matching call to MFUnlockPlatform

  function MFLockPlatform: HResult; stdcall;
  function MFUnlockPlatform: HResult; stdcall;

////////////////////////////////////////////////////////////////////////////////
///
  function MFPutWorkItem(dwQueue: DWORD; pCallback: IMFAsyncCallback; pState: IUnknown): HResult; stdcall;
  function MFPutWorkItemEx(dwQueue: DWORD; pResult: IMFAsyncResult): HResult; stdcall;
  function MFScheduleWorkItem(pCallback: IMFAsyncCallback; pState: IUnknown; Timeout: INT64; pKey: MFWORKITEM_KEY): HResult; stdcall;
  function MFScheduleWorkItemEx(pResult: IMFAsyncResult; Timeout: INT64; pKey: MFWORKITEM_KEY): HResult; stdcall;
  //   The CancelWorkItem method is used by objects to cancel scheduled operation
  //   Due to asynchronous nature of timers, application might still get a
  //   timer callback after MFCancelWorkItem has returned.

  function MFCancelWorkItem(Key: MFWORKITEM_KEY): HResult; stdcall;
  ///////////////////////////////////////////////////////////////////////////////



  // MF periodic callbacks
  function MFGetTimerPeriodicity(out Periodicity: DWord): HResult; stdcall;


  //typedef void (*MFPERIODICCALLBACK)(IUnknown* pContext);
type
  MFPERIODICCALLBACK = procedure(out pContext: IUnknown); cdecl;

  function MFAddPeriodicCallback(const Callback: MFPERIODICCALLBACK; const pContext: IUnknown; out pdwKey: DWord): HResult; stdcall;
  function MFRemovePeriodicCallback(const dwKey: DWord): HResult; stdcall;


///////////////////////////////////////////////////////////////////////////////


  // MF work queues


  //#if (WINVER >= _WIN32_WINNT_WIN7)

  // MFASYNC_WORKQUEUE_TYPE: types of work queue used by MFAllocateWorkQueueEx


  {$EXTERNALSYM MFASYNC_WORKQUEUE_TYPE}
type                                   //2012aug07 convert
  MFASYNC_WORKQUEUE_TYPE = DWORD;
//  cwMFASYNC_WORKQUEUE_TYPE  = (
    // MF_STANDARD_WORKQUEUE: Work queue in a thread without Window
    // message loop.
const
    MF_STANDARD_WORKQUEUE = 0;
    // MF_WINDOW_WORKQUEUE: Work queue in a thread running Window
    // Message loop that calls PeekMessage() / DispatchMessage()..
    MF_WINDOW_WORKQUEUE   = 1;
//  );
//  MFASYNC_WORKQUEUE_TYPE = cwMFASYNC_WORKQUEUE_TYPE;

  function MFAllocateWorkQueueEx(const WorkQueueType: MFASYNC_WORKQUEUE_TYPE; out pdwWorkQueue: DWord): HResult; stdcall;

  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

  // Allocate a standard work queue. the behaviour is the same with:
  // MFAllocateWorkQueueEx( MF_STANDARD_WORKQUEUE, pdwWorkQueue )

  function MFAllocateWorkQueue(out pdwWorkQueue: DWord): HResult; stdcall;
  function MFLockWorkQueue(const dwWorkQueue: DWord): HResult; stdcall;
  function MFUnlockWorkQueue(const dwWorkQueue: DWord): HResult; stdcall;
  function MFBeginRegisterWorkQueueWithMMCSS(const dwWorkQueueId: DWord; const wszClass: LPCWSTR;  //updt 090812 > error> DWord): HResult; stdcall;
                                             const dwTaskId: DWord; const pDoneCallback: IMFAsyncCallback; pDoneState: IUnknown): HResult; stdcall;
  function MFEndRegisterWorkQueueWithMMCSS(const pResult: IMFAsyncResult; pdwTaskId: DWord): HResult; stdcall;
  function MFBeginUnregisterWorkQueueWithMMCSS(const dwWorkQueueId: DWord; const pDoneCallback: IMFAsyncCallback; const pDoneState: IUnknown): HResult; stdcall;
  function MFEndUnregisterWorkQueueWithMMCSS(const pResult: IMFAsyncResult): HResult; stdcall;


  function MFGetWorkQueueMMCSSClass(const dwWorkQueueId: DWord; out pwszClass: LPWSTR ; var pcchClass: DWord): HResult; stdcall;

  function MFGetWorkQueueMMCSSTaskId(const dwWorkQueueId: DWord; out pdwTaskId: LPDWORD): HResult; stdcall;


///////////////////////////////////////////////////////////////////////////////
/////////////////////////////////    Async Model //////////////////////////////
///////////////////////////////////////////////////////////////////////////////


  // Instantiates the MF-provided Async Result implementation

  function MFCreateAsyncResult(const punkObject: IUnknown; const pCallback: IMFAsyncCallback; const punkState: IUnknown; out ppAsyncResult: IMFAsyncResult): HResult; stdcall;

  // Helper for calling IMFAsyncCallback.Invoke
    function MFInvokeCallback(const pAsyncResult: IMFAsyncResult): HResult; stdcall;

type

  // MFASYNCRESULT struct.
  // Any implementation of IMFAsyncResult must inherit from this struct;
  // the Media Foundation workqueue implementation depends on this.
  //
  {$EXTERNALSYM tagMFASYNCRESULT}
  tagMFASYNCRESULT = record
    AsyncResult: IMFAsyncResult;
    overlapped: OVERLAPPED;
    pCallback: IMFAsyncCallback;                 //updt 090812 remove p from  PIMFAsyncCallback
    hrStatusResult: HResult;
    dwBytesTransferred: DWORD;
    hEvent: THandle;
  end;
  {$EXTERNALSYM MFASYNCRESULT}
  MFASYNCRESULT = tagMFASYNCRESULT;



///////////////////////////////////////////////////////////////////////////////
/////////////////////////////////    Files       //////////////////////////////
///////////////////////////////////////////////////////////////////////////////


  // Regardless of the access mode with which the file is opened, the sharing
  // permissions will allow shared reading and deleting.

  function MFCreateFile(const AccessMode: MF_FILE_ACCESSMODE; const OpenMode: MF_FILE_OPENMODE;
                        const fFlags: MF_FILE_FLAGS; const pwszFileURL: LPCWSTR; out ppIByteStream: IMFByteStream): HResult; stdcall;
  function MFCreateTempFile(const AccessMode: MF_FILE_ACCESSMODE; const OpenMode: MF_FILE_OPENMODE;
                            const fFlags: MF_FILE_FLAGS; out ppIByteStream: IMFByteStream): HResult; stdcall;
  function MFBeginCreateFile(const AccessMode: MF_FILE_ACCESSMODE; const OpenMode: MF_FILE_OPENMODE;
                             const fFlags: MF_FILE_FLAGS; const pwszFilePath: LPCWSTR;const  pCallback: IMFAsyncCallback;
                             const pState: IUnknown; out ppCancelCookie: IUnknown): HResult; stdcall;
  function MFEndCreateFile(const pResult: IMFAsyncResult; out ppFile: IMFByteStream): HResult; stdcall;
  function MFCancelCreateFile(const pCancelCookie: IUnknown): HResult; stdcall;


///////////////////////////////////////////////////////////////////////////////
/////////////////////////////////    Buffers     //////////////////////////////
///////////////////////////////////////////////////////////////////////////////


  // Creates an IMFMediaBuffer in memory
  function MFCreateMemoryBuffer(const cbMaxLength: DWord; out ppBuffer: IMFMediaBuffer): HResult; stdcall;


  // Creates an IMFMediaBuffer wrapper at the given offset and length
  // within an existing IMFMediaBuffer
  function MFCreateMediaBufferWrapper(const pBuffer: IMFMediaBuffer; const cbOffset: DWord;
                                      const dwLength: DWord; out ppBuffer: IMFMediaBuffer): HResult; stdcall;


  // Creates a legacy buffer (IMediaBuffer) wrapper at the given offset within
  // an existing IMFMediaBuffer.
  // pSample is optional.  It can point to the original IMFSample from which this
  // IMFMediaBuffer came.  If provided, then *ppMediaBuffer will succeed
  // QueryInterface for IID_IMFSample, from which the original sample's attributes
  // can be obtained
  function MFCreateLegacyMediaBufferOnMFMediaBuffer(const pSample: IMFSample; const pMFMediaBuffer: IMFMediaBuffer;
                                                  const cbOffset: DWord; out ppMediaBuffer: IMediaBuffer): HResult; stdcall;

  // Create a DirectX surface buffer
  function MFCreateDXSurfaceBuffer(const riid: REFIID; const punkSurface: IUnknown; const fBottomUpWhenLinear: Boolean; out ppBuffer: IMFMediaBuffer): HResult; stdcall;


const

  // Create an aligned memory buffer.
  // The following constants were chosen for parity with the alignment constants
  // in ntioapi.h

  {$EXTERNALSYM MF_1_BYTE_ALIGNMENT}
  MF_1_BYTE_ALIGNMENT                 = $00000000;
  {$EXTERNALSYM MF_2_BYTE_ALIGNMENT}
  MF_2_BYTE_ALIGNMENT                 = $00000001;
  {$EXTERNALSYM MF_4_BYTE_ALIGNMENT}
  MF_4_BYTE_ALIGNMENT                 = $00000003;
  {$EXTERNALSYM MF_8_BYTE_ALIGNMENT}
  MF_8_BYTE_ALIGNMENT                 = $00000007;
  {$EXTERNALSYM MF_16_BYTE_ALIGNMENT}
  MF_16_BYTE_ALIGNMENT                = $0000000F;
  {$EXTERNALSYM MF_32_BYTE_ALIGNMENT}
  MF_32_BYTE_ALIGNMENT                = $0000001F;
  {$EXTERNALSYM MF_64_BYTE_ALIGNMENT}
  MF_64_BYTE_ALIGNMENT                = $0000003F;
  {$EXTERNALSYM MF_128_BYTE_ALIGNMENT}
  MF_128_BYTE_ALIGNMENT               = $0000007F;
  {$EXTERNALSYM MF_256_BYTE_ALIGNMENT}
  MF_256_BYTE_ALIGNMENT               = $000000FF;
  {$EXTERNALSYM MF_512_BYTE_ALIGNMENT}
  MF_512_BYTE_ALIGNMENT               = $000001FF;

//updt 090812 remove: type
  function MFCreateAlignedMemoryBuffer(const cbMaxLength: DWord; const cbAligment: DWord; out ppBuffer: IMFMediaBuffer): HResult; stdcall;


type //updt 090812 replace: const
  // This GUID is used in IMFGetService.GetService calls to retrieve
  // interfaces from the buffer.  Its value is defined in Evr.pas
  MR_BUFFER_SERVICE = TGuid;



  ///////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////    Events      //////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////


  // Instantiates the MF-provided Media Event implementation.
  function MFCreateMediaEvent(const met: MediaEventType; const guidExtendedType: REFGUID; const hrStatus: HRESULT;
                              const pvValue: PROPVARIANT; ppEvent: IMFMediaEvent): HResult; stdcall;


  // Instantiates an object that implements IMFMediaEventQueue.
  // Components that provide an IMFMediaEventGenerator can use this object
  // internally to do their Media Event Generator work for them.
  // IMFMediaEventGenerator calls should be forwarded to the similar call
  // on this object's IMFMediaEventQueue interface (e.g. BeginGetEvent,
  // EndGetEvent), and the various IMFMediaEventQueue::QueueEventXXX methods
  // can be used to queue events that the caller will consume.
  function MFCreateEventQueue(out ppMediaEventQueue: IMFMediaEventQueue): HResult; stdcall;

  // Event attributes
  // Some of the common Media Foundation events have associated attributes
  // that go in their IMFAttributes stores


  // MESessionCapabilitiesChanged attributes

  // MF_EVENT_SESSIONCAPS {7E5EBCD0-11B8-4abe-AFAD-10F6599A7F42}
  // Type: UINT32

Const  //updt 090812 replace type
  MF_EVENT_SESSIONCAPS : TGuid =  '{7e5ebcd0-11b8-4abe-afad-10f6599a7f42}';


  // MF_EVENT_SESSIONCAPS_DELTA {7E5EBCD1-11B8-4abe-AFAD-10F6599A7F42}
  // Type: UINT32
  MF_EVENT_SESSIONCAPS_DELTA : TGuid =  '{7e5ebcd1-11b8-4abe-afad-10f6599a7f42}'; //updt 090812 correct guid value




  // MESessionTopologyStatus attributes


  // Possible values for MF_EVENT_TOPOLOGY_STATUS attribute.
  //
  // For a given topology, these status values will arrive via
  // MESessionTopologyStatus in the order below.
  //
  // However, there are no guarantees about how these status values will be
  // ordered between two consecutive topologies.  For example,
  // MF_TOPOSTATUS_READY could arrive for topology n+1 before
  // MF_TOPOSTATUS_ENDED arrives for topology n if the application called
  // IMFMediaSession::SetTopology for topology n+1 well enough in advance of the
  // end of topology n.  Conversely, if topology n ends before the application
  // calls IMFMediaSession::SetTopology for topology n+1, then
  // MF_TOPOSTATUS_ENDED will arrive for topology n before MF_TOPOSTATUS_READY
  // arrives for topology n+1.

type
  MF_TOPOSTATUS = DWORD;             //updt 090812 Remove: cwMF_TOPOSTATUS;



  ////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////  Samples  //////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////


  // Creates an instance of the Media Foundation implementation of IMFSample
  function MFCreateSample(out ppIMFSample: IMFSample): HResult; stdcall;


  // Sample attributes
  // These are the well-known attributes that can be present on an MF Sample's
  // IMFAttributes store


  // MFSampleExtension_CleanPoint {9cdf01d8-a0f0-43ba-b077-eaa06cbd728a}
  // Type: UINT32
  // If present and nonzero, indicates that the sample is a clean point (key
  // frame), and decoding can begin at this sample.
const   //updt 090812 add
  MFSampleExtension_CleanPoint              : TGuid = '{9cdf01d8-a0f0-43ba-b077-eaa06cbd728a}';

  // MFSampleExtension_Discontinuity {9cdf01d9-a0f0-43ba-b077-eaa06cbd728a}
  // Type: UINT32
  // If present and nonzero, indicates that the sample data represents the first
  // sample following a discontinuity (gap) in the stream of samples.
  // This can happen, for instance, if the previous sample was lost in
  // transmission.
  MFSampleExtension_Discontinuity           : TGuid = '{9cdf01d9-a0f0-43ba-b077-eaa06cbd728a}';

  // MFSampleExtension_Token {8294da66-f328-4805-b551-00deb4c57a61}
  // Type: IUNKNOWN
  // When an IMFMediaStream delivers a sample via MEMediaStream, this attribute
  // should be set to the IUnknown *pToken argument that was passed with the
  // IMFMediaStream::RequestSample call to which this sample corresponds.
  MFSampleExtension_Token                   : TGuid = '{8294da66-f328-4805-b551-00deb4c57a61}';

  // The following four sample attributes are used for encrypted samples
  MFSampleExtension_DescrambleData          : TGuid = '{43483be6-4903-4314-b032-2951365936fc}'; // UINT64
  MFSampleExtension_SampleKeyID             : TGuid = '{9ed713c8-9b87-4b26-8297-a93b0c5a8acc}'; // UINT32
  MFSampleExtension_GenKeyFunc              : TGuid = '{441ca1ee-6b1f-4501-903a-de87df42f6ed}'; // UINT64
  MFSampleExtension_GenKeyCtx               : TGuid = '{188120cb-d7da-4b59-9b3e-9252fd37301c}'; // UINT64
  MFSampleExtension_PacketCrossOffsets      : TGuid = '{2789671d-389f-40bb-90d9-c282f77f9abd}'; // BLOB

  /////////////////////////////////////////////////////////////////////////////
  //
  // MFSample STANDARD EXTENSION ATTRIBUTE GUIDs
  //
  /////////////////////////////////////////////////////////////////////////////

  // {b1d5830a-deb8-40e3-90fa-389943716461}   MFSampleExtension_Interlaced                {UINT32 (BOOL)}
  MFSampleExtension_Interlaced              : TGuid = '{b1d5830a-deb8-40e3-90fa-389943716461}';
  // {941ce0a3-6ae3-4dda-9a08-a64298340617}   MFSampleExtension_BottomFieldFirst          {UINT32 (BOOL)}
  MFSampleExtension_BottomFieldFirst        : TGuid = '{941ce0a3-6ae3-4dda-9a08-a64298340617}';
  // {304d257c-7493-4fbd-b149-9228de8d9a99}   MFSampleExtension_RepeatFirstField          {UINT32 (BOOL)}
  MFSampleExtension_RepeatFirstField        : TGuid = '{304d257c-7493-4fbd-b149-9228de8d9a99}';
  // {9d85f816-658b-455a-bde0-9fa7e15ab8f9}   MFSampleExtension_SingleField               {UINT32 (BOOL)}
  MFSampleExtension_SingleField             : TGuid = '{9d85f816-658b-455a-bde0-9fa7e15ab8f9}';
  // {6852465a-ae1c-4553-8e9b-c3420fcb1637}   MFSampleExtension_DerivedFromTopField       {UINT32 (BOOL)}
  MFSampleExtension_DerivedFromTopField     : TGuid = '{6852465a-ae1c-4553-8e9b-c3420fcb1637}';

  ///////////////////////////////  Attributes /////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  function MFCreateAttributes(out ppMFAttributes: IMFAttributes; const cInitialSize: UINT32): HResult; stdcall;
  function MFInitAttributesFromBlob(const pAttributes: IMFAttributes; const pBuf: UINT8; const cbBufSize: UINT): HResult; stdcall;
  function MFGetAttributesAsBlobSize(const pAttributes: IMFAttributes; out pcbBufSize: UINT32): HResult; stdcall;
  function MFGetAttributesAsBlob(const pAttributes: IMFAttributes; out pBuf: UINT8; const cbBufSize: UINT): HResult; stdcall;

  ///////////////////////////  MFT Register & Enum ////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  // MFT Registry categories


  //#ifdef MF_INIT_GUIDS
  //#include <initguid.h>
  //#endif

  // {d6c02d4b-6833-45b4-971a-05a4b04bab91}   MFT_CATEGORY_VIDEO_DECODER
const           //updt 090812 add
  MFT_CATEGORY_VIDEO_DECODER          : TGuid = '{d6c02d4b-6833-45b4-971a-05a4b04bab91}';
  // {f79eac7d-e545-4387-bdee-d647d7bde42a}   MFT_CATEGORY_VIDEO_ENCODER
  MFT_CATEGORY_VIDEO_ENCODER          : TGuid = '{f79eac7d-e545-4387-bdee-d647d7bde42a}';
  // {12e17c21-532c-4a6e-8a1c-40825a736397}   MFT_CATEGORY_VIDEO_EFFECT
  MFT_CATEGORY_VIDEO_EFFECT           : TGuid = '{12e17c21-532c-4a6e-8a1c-40825a736397}';
  // {059c561e-05ae-4b61-b69d-55b61ee54a7b}   MFT_CATEGORY_MULTIPLEXER
  MFT_CATEGORY_MULTIPLEXER            : TGuid = '{059c561e-05ae-4b61-b69d-55b61ee54a7b}';
  // {a8700a7a-939b-44c5-99d7-76226b23b3f1}   MFT_CATEGORY_DEMULTIPLEXER
  MFT_CATEGORY_DEMULTIPLEXER          : TGuid = '{a8700a7a-939b-44c5-99d7-76226b23b3f1}';
  // {9ea73fb4-ef7a-4559-8d5d-719d8f0426c7}   MFT_CATEGORY_AUDIO_DECODER
  MFT_CATEGORY_AUDIO_DECODER          : TGuid = '{9ea73fb4-ef7a-4559-8d5d-719d8f0426c7}';
  // {91c64bd0-f91e-4d8c-9276-db248279d975}   MFT_CATEGORY_AUDIO_ENCODER
  MFT_CATEGORY_AUDIO_ENCODER          : TGuid = '{91c64bd0-f91e-4d8c-9276-db248279d975}';
  // {11064c48-3648-4ed0-932e-05ce8ac811b7}   MFT_CATEGORY_AUDIO_EFFECT
  MFT_CATEGORY_AUDIO_EFFECT           : TGuid = '{11064c48-3648-4ed0-932e-05ce8ac811b7}';

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // {302EA3FC-AA5F-47f9-9F7A-C2188BB163021}...MFT_CATEGORY_VIDEO_PROCESSOR
  MFT_CATEGORY_VIDEO_PROCESSOR        : TGuid = '{302ea3fc-aa5f-47f9-9f7a-c2188bb16302}'; //updt 090812 correct GUID
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

  // {90175d57-b7ea-4901-aeb3-933a8747756f}   MFT_CATEGORY_OTHER
  MFT_CATEGORY_OTHER        : TGuid = '{90175d57-b7ea-4901-aeb3-933a8747756f}';


  // "Flags" is for future expansion - for now must be 0

  function MFTRegister(const clsidMFT: CLSID; const guidCategory: TGUID; const pszName: LPCWSTR;
                     const Flags: UINT32; const cInputTypes: UINT32; const pInputTypes: MFT_REGISTER_TYPE_INFO;
                     const cOutputTypes: UINT32; const pOutputTypes: MFT_REGISTER_TYPE_INFO; const pAttributes: IMFAttributes): HResult; stdcall;
  function MFTUnregister(const clsidMFT: CLSID): HResult; stdcall;

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  //  Register an MFT class in-process
  function MFTRegisterLocal(const pClassFactory: IClassFactory; const guidCategory: REFGUID; const pszName: LPCWSTR;
                            const Flags: UINT32; const cInputTypes: UINT32; const pInputTypes: MFT_REGISTER_TYPE_INFO;
                            const cOutputTypes: UINT32; const pOutputTypes: MFT_REGISTER_TYPE_INFO): HResult; stdcall;
  //  Unregister locally registered MFT
  //  If pClassFactory is NULL all local MFTs are unregistered
  function  MFTUnregisterLocal(const pClassFactory: IClassFactory): HResult; stdcall;     //updt 090812 remove p from IClassFactory
  // Register an MFT class in-process, by CLSID
  function MFTRegisterLocalByCLSID(const clisdMFT: REFCLSID; const guidCategory: REFGUID; const pszName: LPCWSTR;
                                   const Flags: UINT32; const cInputTypes: UINT32; const pInputTypes: MFT_REGISTER_TYPE_INFO;
                                   const cOutputTypes: UINT32; const pOutputTypes: MFT_REGISTER_TYPE_INFO): HResult; stdcall;
  // Unregister locally registered MFT by CLSID
  function MFTUnregisterLocalByCLSID(const clsidMFT: CLSID): HResult; stdcall;
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)


  // result *ppclsidMFT must be freed with CoTaskMemFree.
  function MFTEnum(const guidCategory: TGUID; const Flags: UINT32; const pInputType: MFT_REGISTER_TYPE_INFO;
                   const pOutputType: MFT_REGISTER_TYPE_INFO; const pAttributes: IMFAttributes; out ppclsidMFT: CLSID {must be freed with CoTaskMemFree};
                   out pcMFTs: UINT32): HResult; stdcall;


  //#if (WINVER >= _WIN32_WINNT_WIN7)

//updt 090812 remove:  cw_MFT_ENUM_FLAG = record
const     //updt 090812 add
    MFT_ENUM_FLAG_SYNCMFT        = $00000001;   // Enumerates V1 MFTs. This is default.
    MFT_ENUM_FLAG_ASYNCMFT       = $00000002;   // Enumerates only software async MFTs also known as V2 MFTs
    MFT_ENUM_FLAG_HARDWARE       = $00000004;   // Enumerates V2 hardware async MFTs
    MFT_ENUM_FLAG_FIELDOFUSE     = $00000008;   // Enumerates MFTs that require unlocking
    MFT_ENUM_FLAG_LOCALMFT       = $00000010;   // Enumerates Locally (in-process) registered MFTs
    MFT_ENUM_FLAG_TRANSCODE_ONLY = $00000020;   // Enumerates decoder MFTs used by transcode only
    MFT_ENUM_FLAG_SORTANDFILTER  = $00000040;   // Apply system local, do not use and preferred sorting and filtering
    MFT_ENUM_FLAG_ALL            = $0000003F;   // Enumerates all MFTs including SW and HW MFTs and applies filtering
//updt 090812 remove:   end;
type    //upd 090812 add
  _MFT_ENUM_FLAG = DWORD; //updt 090812 cw_MFT_ENUM_FLAG;


  // result *pppMFTActivate must be freed with CoTaskMemFree. Each IMFActivate pointer inside this
  // buffer should be released.
  function MFTEnumEx(const guidCategory: TGuid; const Flags: UINT32; const pInputType: MFT_REGISTER_TYPE_INFO;
                   const pOutputType: MFT_REGISTER_TYPE_INFO; out pppMFTActivate: IMFActivate; out pnumMFTActivate: UINT32): HResult; stdcall;

  //#endif // (WINVER >= _WIN32_WINNT_WIN7)


  // results *pszName, *ppInputTypes, and *ppOutputTypes must be freed with CoTaskMemFree.
  // *ppAttributes must be released.
  function MFTGetInfo(const clsidMFT: CLSID; out pszName: PPWideChar; out ppInputTypes: MFT_REGISTER_TYPE_INFO;
                    out pcInputTypes: UINT32; out ppOutputTypes: MFT_REGISTER_TYPE_INFO; out pcOutputTypes: UINT32; //updt 090812 typo fix
                    out ppAttributes: IMFAttributes): HResult; stdcall;     //updt 090812 remove pp from IMFAttributes

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  //  Get the plugin control API
  function MFGetPluginControl(out ppPluginControl: IMFPluginControl): HResult; stdcall;  //updt 090812 remove pp from IMFPluginControl
  //  Get MFT's merit - checking that is has a valid certificate
  function MFGetMFTMerit(var pMFT: IUnknown; const cbVerifier: UINT32; const verifier: Byte; out merit: DWord): HResult; stdcall;
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)



  /////////////////////////  MFT  Attributes GUIDs ////////////////////////////
  // {53476A11-3F13-49fb-AC42-EE2733C96741} MFT_SUPPORT_DYNAMIC_FORMAT_CHANGE {UINT32 (BOOL)}
const    //updt 090812 add
  MFT_SUPPORT_DYNAMIC_FORMAT_CHANGE        : TGuid = '{53476A11-3F13-49fb-AC42-EE2733C96741}';

  //////////////////////////////  Media Type GUIDs ////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  // GUIDs for media types

  // In MF, media types for uncompressed video formats MUST be composed from a FourCC or D3DFORMAT combined with
  // the "base GUID" {00000000-0000-0010-8000-00AA00389B71} by replacing the initial 32 bits with the FourCC/D3DFORMAT
  //
  // Audio media types for types which already have a defined wFormatTag value can be constructed similarly, by
  // putting the wFormatTag (zero-extended to 32 bits) into the first 32 bits of the base GUID.
  //
  // Compressed video or audio can also use any well-known GUID that exists, or can create a new GUID.
  //
  // GUIDs for common media types are defined below.


//todo: Commented out (like in Directshow.pas), need a solution to this

{
#ifndef FCC
#define FCC(ch4) ((((DWORD)(ch4) & 0xFF) << 24) |     \
                  (((DWORD)(ch4) & 0xFF00) << 8) |    \
                  (((DWORD)(ch4) & 0xFF0000) >> 8) |  \
                  (((DWORD)(ch4) & 0xFF000000) >> 24))
#endif


//
// this macro creates a media type GUID from a FourCC, D3DFMT, or WAVE_FORMAT
//
#ifndef DEFINE_MEDIATYPE_GUID
#define DEFINE_MEDIATYPE_GUID(name, format) \
    DEFINE_GUID(name,                       \
    format, 0x0000, 0x0010, 0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71);
#endif

}

// REMARK#1
// To use these converted macro's and/or add other TGuid's,
// you have to use function DefineMediaTypeGuid defined at the Implementations section of this file.
// The following steps should be taken:
// 1 Declare	MFYourAudioOrVideo_Guid as TGuid. (var MFYourAudioOrVideo_Guid : TGuid)
// 2 Assign a valid DWord or FourCC value to get a proper Guid:
//   MFYourAudioOrVideo_Guid:= DefineMediaTypeGuid(FourCC or Dword - NOT BOTH! -)
//   NOTE: Default values for both are '' (sFcc) or 0 (dwConst)


// Peter
Type
  tCh4 = Array [0..3] of Char;

function FCC(ch4: TCh4): DWord;

// Tony
function DEFINE_MEDIATYPE_GUID(format: DWord): TGuid;

// video media types ///////////////////////////////////////////////////////////


const

// If no D3D headers have been included yet, define local versions of D3DFMT constants we use.
// We can't include D3D headers from this header because we need it to be compatible with all versions
// of D3D.
  {$EXTERNALSYM D3DFMT_R8G8B8}
  D3DFMT_R8G8B8                       = 20;
  {$EXTERNALSYM D3DFMT_A8R8G8B8}
  D3DFMT_A8R8G8B8                     = 21;
  {$EXTERNALSYM D3DFMT_X8R8G8B8}
  D3DFMT_X8R8G8B8                     = 22;
  {$EXTERNALSYM D3DFMT_R5G6B5}
  D3DFMT_R5G6B5                       = 23;
  {$EXTERNALSYM D3DFMT_X1R5G5B5}
  D3DFMT_X1R5G5B5                     = 24;
  {$EXTERNALSYM D3DFMT_P8}
  D3DFMT_P8                           = 41;
  {$EXTERNALSYM LOCAL_D3DFMT_DEFINES}
  LOCAL_D3DFMT_DEFINES                = 1;

// const of MEDIATYPE DWORD or FourCC
// See: REMARK#1
const
  //MFVideoFormat_Base
  dwMFVideoFormat_Base    =    $00000000;
  //MFVideoFormat_RGB32
  dwMFVideoFormat_RGB32   =    D3DFMT_X8R8G8B8;
  //MFVideoFormat_ARGB32
  dwMFVideoFormat_ARGB32  =    D3DFMT_A8R8G8B8;
  //MFVideoFormat_RGB24
  dwMFVideoFormat_RGB24   =    D3DFMT_R8G8B8;
  //MFVideoFormat_RGB555
  dwMFVideoFormat_RGB555  =    D3DFMT_X1R5G5B5;
  //MFVideoFormat_RGB565
  dwMFVideoFormat_RGB565  =    D3DFMT_R5G6B5;
  //MFVideoFormat_RGB8
  dwMFVideoFormat_RGB8    =    D3DFMT_P8;
  //MFVideoFormat_AI44
  fccMFVideoFormat_AI44   =    'AI44';
  //MFVideoFormat_AYUV
  fccMFVideoFormat_AYUV =      'AYUV';
  //MFVideoFormat_YUY2
  fccMFVideoFormat_YUY2 =      'YUY2';
  //MFVideoFormat_YVYU
  fccMFVideoFormat_YVYU =      'YVYU';
  //MFVideoFormat_YVU9
  fccMFVideoFormat_YVU9 =      'YVU9';
  //MFVideoFormat_UYVY
  fccMFVideoFormat_UYVY =      'UYVY';
  //MFVideoFormat_NV11
  fccMFVideoFormat_NV11 =      'NV11';
  //MFVideoFormat_NV12
  fccMFVideoFormat_NV12 =      'NV12';
  //MFVideoFormat_YV12
  fccMFVideoFormat_YV12 =      'YV12';
  //MFVideoFormat_I420
  fccMFVideoFormat_I420 =      'I420';
  //MFVideoFormat_IYUV
  fccMFVideoFormat_IYUV =      'IYUV';
  //MFVideoFormat_Y210
  fccMFVideoFormat_Y210 =      'Y210';
  //MFVideoFormat_Y216
  fccMFVideoFormat_Y216 =      'Y216';
  //MFVideoFormat_Y410
  fccMFVideoFormat_Y410 =      'Y410';
  //MFVideoFormat_Y416
  fccMFVideoFormat_Y416 =      'Y416';
  //MFVideoFormat_Y41P
  fccMFVideoFormat_Y41P =      'Y41P';
  //MFVideoFormat_Y41T
  fccMFVideoFormat_Y41T =      'Y41T';
  //MFVideoFormat_Y42T
  fccMFVideoFormat_Y42T =      'Y42T';
  //MFVideoFormat_P210
  fccMFVideoFormat_P210 =      'P210';
  //MFVideoFormat_P216
  fccMFVideoFormat_P216 =      'P216';
  //MFVideoFormat_P010
  fccMFVideoFormat_P010 =      'P010';
  //MFVideoFormat_P016
  fccMFVideoFormat_P016 =      'P016';
  //MFVideoFormat_v210
  fccMFVideoFormat_v210 =      'v210';
  //MFVideoFormat_v216
  fccMFVideoFormat_v216 =      'v216';
  //MFVideoFormat_v410
  fccMFVideoFormat_v410 =      'v410';
  //MFVideoFormat_MP43
  fccMFVideoFormat_MP43 =      'MP43';
  //MFVideoFormat_MP4S
  fccMFVideoFormat_MP4S =      'MP4S';
  //MFVideoFormat_M4S2
  fccMFVideoFormat_M4S2 =      'M4S2';
  //MFVideoFormat_MP4V
  fccMFVideoFormat_MP4V =      'MP4V';
  //MFVideoFormat_WMV1
  fccMFVideoFormat_WMV1 =      'WMV1';
  //MFVideoFormat_WMV2
  fccMFVideoFormat_WMV2 =      'WMV2';
  //MFVideoFormat_WMV3
  fccMFVideoFormat_WMV3 =      'WMV3';
  //MFVideoFormat_WVC1
  fccMFVideoFormat_WVC1 =      'WVC1';
  //MFVideoFormat_MSS1
  fccMFVideoFormat_MSS1 =      'MSS1';
  //MFVideoFormat_MSS2
  fccMFVideoFormat_MSS2 =      'MSS2';
  //MFVideoFormat_MPG1
  fccMFVideoFormat_MPG1 =      'MPG1';
  //MFVideoFormat_DVSL
  fccMFVideoFormat_DVSL =      'dvsl';
  //MFVideoFormat_DVSD
  fccMFVideoFormat_DVSD =      'dvsd';
  //MFVideoFormat_DVHD
  fccMFVideoFormat_DVHD =      'dvhd';
  //MFVideoFormat_DV25
  fccMFVideoFormat_DV25 =      'dv25';
  //MFVideoFormat_DV50
  fccMFVideoFormat_DV50 =      'dv50';
  //MFVideoFormat_DVH1
  fccMFVideoFormat_DVH1 =      'dvh1';
  //MFVideoFormat_DVC
  fccMFVideoFormat_DVC =       'dvc ';
  //MFVideoFormat_H264
  fccMFVideoFormat_H264 =      'H264';
  //MFVideoFormat_MJPG
  fccMFVideoFormat_MJPG =      'MJPG';

//
// undef the local D3DFMT definitions to avoid later clashes with D3D headers
//
(*
#ifdef LOCAL_D3DFMT_DEFINES
#undef D3DFMT_R8G8B8
#undef D3DFMT_A8R8G8B8
#undef D3DFMT_X8R8G8B8
#undef D3DFMT_R5G6B5
#undef D3DFMT_X1R5G5B5
#undef D3DFMT_P8
#undef LOCAL_D3DFMT_DEFINES
#endif
*)

// some legacy formats that don't fit the common pattern ///////////////////////

// {e06d8026-db46-11cf-b4d1-00805f6cbbea}       MFVideoFormat_MPEG2
  MFVideoFormat_MPEG2 : TGuid = '{e06d8026-db46-11cf-b4d1-00805f6cbbea}';

//  {$EXTERNALSYM MFVideoFormat_MPG2}
//updt 090832 doesn't work  dwMFVideoFormat_MPG2                = MFVideoFormat_MPEG2;


// audio media types ///////////////////////////////////////////////////////////

  //MFAudioFormat_Base
  dwMFAudioFormat_Base              = $00000000;
  //MFAudioFormat_PCM
  dwMFAudioFormat_PCM               = WAVE_FORMAT_PCM;
  //MFAudioFormat_Float
  dwMFAudioFormat_Float             = WAVE_FORMAT_IEEE_FLOAT;
  //MFAudioFormat_DTS
  dwMFAudioFormat_DTS               = WAVE_FORMAT_DTS;
  //MFAudioFormat_Dolby_AC3_SPDIF
  dwMFAudioFormat_Dolby_AC3_SPDIF   = WAVE_FORMAT_DOLBY_AC3_SPDIF;
  //MFAudioFormat_DRM
  dwMFAudioFormat_DRM               = WAVE_FORMAT_DRM;
  //MFAudioFormat_WMAudioV8
  dwMFAudioFormat_WMAudioV8         = WAVE_FORMAT_WMAUDIO2;
  //MFAudioFormat_WMAudioV9
  dwMFAudioFormat_WMAudioV9         = WAVE_FORMAT_WMAUDIO3;
  //MFAudioFormat_WMAudio_Lossless
  dwMFAudioFormat_WMAudio_Lossless  = WAVE_FORMAT_WMAUDIO_LOSSLESS;
  //MFAudioFormat_WMASPDIF
  dwMFAudioFormat_WMASPDIF          = WAVE_FORMAT_WMASPDIF;
  //MFAudioFormat_MSP1
  dwMFAudioFormat_MSP1              = WAVE_FORMAT_WMAVOICE9;
  //MFAudioFormat_MP3
  dwMFAudioFormat_MP3               = WAVE_FORMAT_MPEGLAYER3;
  //MFAudioFormat_MPEG
  dwMFAudioFormat_MPEG              = WAVE_FORMAT_MPEG;
  //MFAudioFormat_AAC
  dwMFAudioFormat_AAC               = WAVE_FORMAT_MPEG_HEAAC;
  //MFAudioFormat_ADTS
  dwMFAudioFormat_ADTS              = WAVE_FORMAT_MPEG_ADTS_AAC;

// MPEG-4 media types //////////////////////////////////////////////////////////

  // {00000000-767a-494d-b478-f29d25dc9037}   MFMPEG4Format_Base
  MFMPEG4Format_Base                          : TGuid = '{00000000-767a-494d-b478-f29d25dc9037}';

//////////////////////  Media Type Attributes GUIDs ////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// GUIDs for IMFMediaType properties - prefix 'MF_MT_' - basic prop type in {},
// with type to cast to in ().



// Core info for all types /////////////////////////////////////////////////////
const
  // {48eba18e-f8c9-4687-bf11-0a74c9f96a8f}   MF_MT_MAJOR_TYPE                {GUID}
  MF_MT_MAJOR_TYPE                            : TGuid = '{48eba18e-f8c9-4687-bf11-0a74c9f96a8f}';

  // {f7e34c9a-42e8-4714-b74b-cb29d72c35e5}   MF_MT_SUBTYPE                   {GUID}
  MF_MT_SUBTYPE                               : TGuid = '{f7e34c9a-42e8-4714-b74b-cb29d72c35e5}';

  // {c9173739-5e56-461c-b713-46fb995cb95f}   MF_MT_ALL_SAMPLES_INDEPENDENT   {UINT32 (BOOL)}
  MF_MT_ALL_SAMPLES_INDEPENDENT               : TGuid = '{c9173739-5e56-461c-b713-46fb995cb95f}';

  // {b8ebefaf-b718-4e04-b0a9-116775e3321b}     MF_MT_FIXED_SIZE_SAMPLES        {UINT32 (BOOL)}
  MF_MT_FIXED_SIZE_SAMPLES                    : TGuid = '{b8ebefaf-b718-4e04-b0a9-116775e3321b}';

  // {3afd0cee-18f2-4ba5-a110-8bea502e1f92}     MF_MT_COMPRESSED                {UINT32 (BOOL)}
  MF_MT_COMPRESSED                            : TGuid = '{3afd0cee-18f2-4ba5-a110-8bea502e1f92}';

  // MF_MT_SAMPLE_SIZE is only valid if MF_MT_FIXED_SIZED_SAMPLES is TRUE
  // {dad3ab78-1990-408b-bce2-eba673dacc10}     MF_MT_SAMPLE_SIZE               {UINT32}
  MF_MT_SAMPLE_SIZE                           : TGuid = '{dad3ab78-1990-408b-bce2-eba673dacc10}';

  // 4d3f7b23-d02f-4e6c-9bee-e4bf2c6c695d       MF_MT_WRAPPED_TYPE              {Blob}
  MF_MT_WRAPPED_TYPE                          : TGuid = '{4d3f7b23-d02f-4e6c-9bee-e4bf2c6c695d}';



// AUDIO data //////////////////////////////////////////////////////////////////
const
  // {37e48bf5-645e-4c5b-89de-ada9e29b696a}   MF_MT_AUDIO_NUM_CHANNELS            {UINT32}
  MF_MT_AUDIO_NUM_CHANNELS                    : TGuid = '{37e48bf5-645e-4c5b-89de-ada9e29b696a}';

  // {5faeeae7-0290-4c31-9e8a-c534f68d9dba}   MF_MT_AUDIO_SAMPLES_PER_SECOND      {UINT32}
  MF_MT_AUDIO_SAMPLES_PER_SECOND              : TGuid = '{5faeeae7-0290-4c31-9e8a-c534f68d9dba}';

  // {fb3b724a-cfb5-4319-aefe-6e42b2406132}   MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND {double}
  MF_MT_AUDIO_FLOAT_SAMPLES_PER_SECOND        : TGuid = '{fb3b724a-cfb5-4319-aefe-6e42b2406132}';

  // {1aab75c8-cfef-451c-ab95-ac034b8e1731}   MF_MT_AUDIO_AVG_BYTES_PER_SECOND    {UINT32}
  MF_MT_AUDIO_AVG_BYTES_PER_SECOND            : TGuid = '{1aab75c8-cfef-451c-ab95-ac034b8e1731}';

  // {322de230-9eeb-43bd-ab7a-ff412251541d}   MF_MT_AUDIO_BLOCK_ALIGNMENT         {UINT32}
  MF_MT_AUDIO_BLOCK_ALIGNMENT                 : TGuid = '{322de230-9eeb-43bd-ab7a-ff412251541d}';

  // {f2deb57f-40fa-4764-aa33-ed4f2d1ff669}   MF_MT_AUDIO_BITS_PER_SAMPLE         {UINT32}
  MF_MT_AUDIO_BITS_PER_SAMPLE                 : TGuid = '{f2deb57f-40fa-4764-aa33-ed4f2d1ff669}';

  // {d9bf8d6a-9530-4b7c-9ddf-ff6fd58bbd06}   MF_MT_AUDIO_VALID_BITS_PER_SAMPLE   {UINT32}
  MF_MT_AUDIO_VALID_BITS_PER_SAMPLE           : TGuid = '{d9bf8d6a-9530-4b7c-9ddf-ff6fd58bbd06}';

  // {aab15aac-e13a-4995-9222-501ea15c6877}   MF_MT_AUDIO_SAMPLES_PER_BLOCK       {UINT32}
  MF_MT_AUDIO_SAMPLES_PER_BLOCK               : TGuid = '{aab15aac-e13a-4995-9222-501ea15c6877}';

  // {55fb5765-644a-4caf-8479-938983bb1588}   MF_MT_AUDIO_CHANNEL_MASK            {UINT32}
  MF_MT_AUDIO_CHANNEL_MASK                    : TGuid = '{55fb5765-644a-4caf-8479-938983bb1588}';


// MF_MT_AUDIO_FOLDDOWN_MATRIX stores folddown structure from multichannel to stereo

type
  PMffolddownMatrix = ^TMffolddownMatrix;
  {$EXTERNALSYM _MFFOLDDOWN_MATRIX}
  _MFFOLDDOWN_MATRIX = record
    cbSize: UINT32;
    cSrcChannels: UINT32;            // number of source channels
    cDstChannels: UINT32;            // number of destination channels
    dwChannelMask: UINT32;           // mask
    Coeff: array[0..63] of LONG;
  end;
  {$EXTERNALSYM MFFOLDDOWN_MATRIX}
  MFFOLDDOWN_MATRIX = _MFFOLDDOWN_MATRIX;
  TMffolddownMatrix = _MFFOLDDOWN_MATRIX;

const
  // {9d62927c-36be-4cf2-b5c4-a3926e3e8711}     MF_MT_AUDIO_FOLDDOWN_MATRIX         {BLOB, MFFOLDDOWN_MATRIX}
  MF_MT_AUDIO_FOLDDOWN_MATRIX                   : TGuid = '{9d62927c-36be-4cf2-b5c4-a3926e3e8711}';

  // {9d62927d-36be-4cf2-b5c4-a3926e3e8711}   MF_MT_AUDIO_WMADRC_PEAKREF         {UINT32}
  MF_MT_AUDIO_WMADRC_PEAKREF                    : TGuid = '{9d62927d-36be-4cf2-b5c4-a3926e3e8711}';

  // {9d62927e-36be-4cf2-b5c4-a3926e3e8711}   MF_MT_AUDIO_WMADRC_PEAKTARGET        {UINT32}
  MF_MT_AUDIO_WMADRC_PEAKTARGET                 : TGuid = '{9d62927e-36be-4cf2-b5c4-a3926e3e8711}';

  // {9d62927f-36be-4cf2-b5c4-a3926e3e8711}   MF_MT_AUDIO_WMADRC_AVGREF         {UINT32}
  MF_MT_AUDIO_WMADRC_AVGREF                     : TGuid = '{9d62927f-36be-4cf2-b5c4-a3926e3e8711}';

  // {9d629280-36be-4cf2-b5c4-a3926e3e8711}   MF_MT_AUDIO_WMADRC_AVGTARGET      {UINT32}
  MF_MT_AUDIO_WMADRC_AVGTARGET                  : TGuid = '{9d629280-36be-4cf2-b5c4-a3926e3e8711}';



// MF_MT_AUDIO_PREFER_WAVEFORMATEX tells the converter to prefer a plain WAVEFORMATEX rather than
// a WAVEFORMATEXTENSIBLE when converting to a legacy type. It is set by the WAVEFORMATEX->IMFMediaType
// conversion routines when the original format block is a non-extensible WAVEFORMATEX.
//
// This preference can be overridden and does not guarantee that the type can be correctly expressed
// by a non-extensible type.


  // {a901aaba-e037-458a-bdf6-545be2074042}     MF_MT_AUDIO_PREFER_WAVEFORMATEX     {UINT32 (BOOL)}
  MF_MT_AUDIO_PREFER_WAVEFORMATEX               : TGuid = '{a901aaba-e037-458a-bdf6-545be2074042}';


  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // >= Windows 7

// AUDIO - AAC extra data //////////////////////////////////////////////////////


  // {BFBABE79-7434-4d1c-94F0-72A3B9E17188}     MF_MT_AAC_PAYLOAD_TYPE       {UINT32}
  MF_MT_AAC_PAYLOAD_TYPE                        : TGuid = '{BFBABE79-7434-4d1c-94F0-72A3B9E17188}';

  // {7632F0E6-9538-4d61-ACDA-EA29C8C14456}     MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION       {UINT32}
  MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION      : TGuid = '{7632F0E6-9538-4d61-ACDA-EA29C8C14456}';

  //#endif // (WINVER >= _WIN32_WINNT_WIN7)
  // end >= Windows 7

const
// VIDEO core data /////////////////////////////////////////////////////////////

  // {1652c33d-d6b2-4012-b834-72030849a37d}     MF_MT_FRAME_SIZE                {UINT64 (HI32(Width),LO32(Height))}
  MF_MT_FRAME_SIZE                              : TGuid = '{1652c33d-d6b2-4012-b834-72030849a37d}';

  // {c459a2e8-3d2c-4e44-b132-fee5156c7bb0}     MF_MT_FRAME_RATE                {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_FRAME_RATE                              : TGuid = '{c459a2e8-3d2c-4e44-b132-fee5156c7bb0}';

  // {c6376a1e-8d0a-4027-be45-6d9a0ad39bb6}     MF_MT_PIXEL_ASPECT_RATIO        {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_PIXEL_ASPECT_RATIO                      : TGuid = '{c6376a1e-8d0a-4027-be45-6d9a0ad39bb6}';

  // {8772f323-355a-4cc7-bb78-6d61a048ae82}     MF_MT_DRM_FLAGS                 {UINT32 (anyof MFVideoDRMFlags)}
  MF_MT_DRM_FLAGS                               : TGuid = '{8772f323-355a-4cc7-bb78-6d61a048ae82}';


type
  PMFVideoDRMFlags = ^TMFVideoDRMFlags;
  {$EXTERNALSYM _MFVideoDRMFlags}
  _MFVideoDRMFlags                    = (
    MFVideoDRMFlag_None               = 0,
    MFVideoDRMFlag_AnalogProtected    = 1,
    MFVideoDRMFlag_DigitallyProtected = 2
  );
  {$EXTERNALSYM MFVideoDRMFlags}
  MFVideoDRMFlags = _MFVideoDRMFlags;
  TMFVideoDRMFlags = _MFVideoDRMFlags;


const
  // {4d0e73e5-80ea-4354-a9d0-1176ceb028ea}     MF_MT_PAD_CONTROL_FLAGS         {UINT32 (oneof MFVideoPadFlags)}
  MF_MT_PAD_CONTROL_FLAGS           : TGuid = '{4d0e73e5-80ea-4354-a9d0-1176ceb028ea}';

type
  PMFVideoPadFlags = ^TMFVideoPadFlags;
  {$EXTERNALSYM _MFVideoPadFlags}
  _MFVideoPadFlags             = (
    MFVideoPadFlag_PAD_TO_None = 0,
    MFVideoPadFlag_PAD_TO_4x3  = 1,
    MFVideoPadFlag_PAD_TO_16x9 = 2
  );
  {$EXTERNALSYM MFVideoPadFlags}
  MFVideoPadFlags = _MFVideoPadFlags;
  TMFVideoPadFlags = _MFVideoPadFlags;

const
  // {68aca3cc-22d0-44e6-85f8-28167197fa38}     MF_MT_SOURCE_CONTENT_HINT       {UINT32 (oneof MFVideoSrcContentHintFlags)}
  MF_MT_SOURCE_CONTENT_HINT                     : TGuid = '{68aca3cc-22d0-44e6-85f8-28167197fa38}';

type
  PMFVideoSrcContentHintFlags = ^TMFVideoSrcContentHintFlags;
  {$EXTERNALSYM _MFVideoSrcContentHintFlags}
  _MFVideoSrcContentHintFlags       = (
    MFVideoSrcContentHintFlag_None  = 0,
    MFVideoSrcContentHintFlag_16x9  = 1,
    MFVideoSrcContentHintFlag_235_1 = 2
  );
  {$EXTERNALSYM MFVideoSrcContentHintFlags}
  MFVideoSrcContentHintFlags = _MFVideoSrcContentHintFlags;
  TMFVideoSrcContentHintFlags = _MFVideoSrcContentHintFlags;

const
  // {65df2370-c773-4c33-aa64-843e068efb0c}     MF_MT_CHROMA_SITING             {UINT32 (anyof MFVideoChromaSubsampling)}
  MF_MT_VIDEO_CHROMA_SITING                     : TGuid = '{65df2370-c773-4c33-aa64-843e068efb0c}';

  // {e2724bb8-e676-4806-b4b2-a8d6efb44ccd}     MF_MT_INTERLACE_MODE            {UINT32 (oneof MFVideoInterlaceMode)}
  MF_MT_INTERLACE_MODE                          : TGuid = '{e2724bb8-e676-4806-b4b2-a8d6efb44ccd}';

  // {5fb0fce9-be5c-4935-a811-ec838f8eed93}     MF_MT_TRANSFER_FUNCTION         {UINT32 (oneof MFVideoTransferFunction)}
  MF_MT_TRANSFER_FUNCTION                       : TGuid = '{5fb0fce9-be5c-4935-a811-ec838f8eed93}';

  // {dbfbe4d7-0740-4ee0-8192-850ab0e21935}     MF_MT_VIDEO_PRIMARIES           {UINT32 (oneof MFVideoPrimaries)}
  MF_MT_VIDEO_PRIMARIES                         : TGuid = '{dbfbe4d7-0740-4ee0-8192-850ab0e21935}';

  // {47537213-8cfb-4722-aa34-fbc9e24d77b8}     MF_MT_CUSTOM_VIDEO_PRIMARIES    {BLOB (MT_CUSTOM_VIDEO_PRIMARIES)}
  MF_MT_CUSTOM_VIDEO_PRIMARIES                  : TGuid = '{47537213-8cfb-4722-aa34-fbc9e24d77b8}';

type
  PMtCustomVideoPrimaries = ^TMtCustomVideoPrimaries;
  {$EXTERNALSYM _MT_CUSTOM_VIDEO_PRIMARIES}
  _MT_CUSTOM_VIDEO_PRIMARIES = record
    fRx: Single;
    fRy: Single;
    fGx: Single;
    fGy: Single;
    fBx: Single;
    fBy: Single;
    fWx: Single;
    fWy: Single;
  end;
  {$EXTERNALSYM MT_CUSTOM_VIDEO_PRIMARIES}
  MT_CUSTOM_VIDEO_PRIMARIES = _MT_CUSTOM_VIDEO_PRIMARIES;
  TMtCustomVideoPrimaries = _MT_CUSTOM_VIDEO_PRIMARIES;

const
  // {3e23d450-2c75-4d25-a00e-b91670d12327}     MF_MT_YUV_MATRIX                {UINT32 (oneof MFVideoTransferMatrix)}
  MF_MT_YUV_MATRIX                              : TGuid = '{3e23d450-2c75-4d25-a00e-b91670d12327}';

  // {53a0529c-890b-4216-8bf9-599367ad6d20}     MF_MT_VIDEO_LIGHTING            {UINT32 (oneof MFVideoLighting)}
  MF_MT_VIDEO_LIGHTING                          : TGuid = '{53a0529c-890b-4216-8bf9-599367ad6d20}';

  // {c21b8ee5-b956-4071-8daf-325edf5cab11}     MF_MT_VIDEO_NOMINAL_RANGE       {UINT32 (oneof MFNominalRange)}
    MF_MT_VIDEO_NOMINAL_RANGE                   : TGuid = '{c21b8ee5-b956-4071-8daf-325edf5cab11}';

  // {66758743-7e5f-400d-980a-aa8596c85696}     MF_MT_GEOMETRIC_APERTURE        {BLOB (MFVideoArea)}
  MF_MT_GEOMETRIC_APERTURE                      : TGuid = '{66758743-7e5f-400d-980a-aa8596c85696}';

  // {d7388766-18fe-48c6-a177-ee894867c8c4}     MF_MT_MINIMUM_DISPLAY_APERTURE  {BLOB (MFVideoArea)}
  MF_MT_MINIMUM_DISPLAY_APERTURE                : TGuid = '{d7388766-18fe-48c6-a177-ee894867c8c4}';

  // {79614dde-9187-48fb-b8c7-4d52689de649}     MF_MT_PAN_SCAN_APERTURE         {BLOB (MFVideoArea)}
  MF_MT_PAN_SCAN_APERTURE                       : TGuid = '{79614dde-9187-48fb-b8c7-4d52689de649}';

  // {4b7f6bc3-8b13-40b2-a993-abf630b8204e}     MF_MT_PAN_SCAN_ENABLED          {UINT32 (BOOL)}
  MF_MT_PAN_SCAN_ENABLED                        : TGuid = '{4b7f6bc3-8b13-40b2-a993-abf630b8204e}';

  // {20332624-fb0d-4d9e-bd0d-cbf6786c102e}     MF_MT_AVG_BITRATE               {UINT32}
  MF_MT_AVG_BITRATE                             : TGuid = '{20332624-fb0d-4d9e-bd0d-cbf6786c102e}';

  // {799cabd6-3508-4db4-a3c7-569cd533deb1}     MF_MT_AVG_BIT_ERROR_RATE        {UINT32}
  MF_MT_AVG_BIT_ERROR_RATE                      : TGuid = '{799cabd6-3508-4db4-a3c7-569cd533deb1}';

  // {c16eb52b-73a1-476f-8d62-839d6a020652}     MF_MT_MAX_KEYFRAME_SPACING      {UINT32}
  MF_MT_MAX_KEYFRAME_SPACING                    : TGuid = '{c16eb52b-73a1-476f-8d62-839d6a020652}';



// VIDEO - uncompressed format data ////////////////////////////////////////////

  // {644b4e48-1e02-4516-b0eb-c01ca9d49ac6}     MF_MT_DEFAULT_STRIDE            {UINT32 (INT32)} // in bytes
  MF_MT_DEFAULT_STRIDE                          : TGuid = '{644b4e48-1e02-4516-b0eb-c01ca9d49ac6}';

  // {6d283f42-9846-4410-afd9-654d503b1a54}     MF_MT_PALETTE                   {BLOB (array of MFPaletteEntry - usually 256)}
  MF_MT_PALETTE                                 : TGuid = '{6d283f42-9846-4410-afd9-654d503b1a54}';


// the following is only used for legacy data that was stuck at the end of the format block when the type
// was converted from a VIDEOINFOHEADER or VIDEOINFOHEADER2 block in an AM_MEDIA_TYPE.

  // {b6bc765f-4c3b-40a4-bd51-2535b66fe09d}     MF_MT_USER_DATA                 {BLOB}
  MF_MT_USER_DATA                               : TGuid = '{b6bc765f-4c3b-40a4-bd51-2535b66fe09d}';

  // {73d1072d-1870-4174-a063-29ff4ff6c11e}     MF_MT_AM_FORMAT_TYPE
  MF_MT_AM_FORMAT_TYPE                          : TGuid = '{73d1072d-1870-4174-a063-29ff4ff6c11e}';


// VIDEO - MPEG1/2 extra data //////////////////////////////////////////////////


  // {91f67885-4333-4280-97cd-bd5a6c03a06e}     MF_MT_MPEG_START_TIME_CODE      {UINT32}
  MF_MT_MPEG_START_TIME_CODE                    : TGuid = '{91f67885-4333-4280-97cd-bd5a6c03a06e}';

  // {ad76a80b-2d5c-4e0b-b375-64e520137036}     MF_MT_MPEG2_PROFILE             {UINT32 (oneof AM_MPEG2Profile)}
  MF_MT_MPEG2_PROFILE                           : TGuid = '{ad76a80b-2d5c-4e0b-b375-64e520137036}';

  // {96f66574-11c5-4015-8666-bff516436da7}     MF_MT_MPEG2_LEVEL               {UINT32 (oneof AM_MPEG2Level)}
  MF_MT_MPEG2_LEVEL                             : TGuid = '{96f66574-11c5-4015-8666-bff516436da7}';

  // {31e3991d-f701-4b2f-b426-8ae3bda9e04b}     MF_MT_MPEG2_FLAGS               {UINT32 (anyof AMMPEG2_xxx flags)}
  MF_MT_MPEG2_FLAGS                             : TGuid = '{31e3991d-f701-4b2f-b426-8ae3bda9e04b}';

  // {3c036de7-3ad0-4c9e-9216-ee6d6ac21cb3}     MF_MT_MPEG_SEQUENCE_HEADER      {BLOB}
  MF_MT_MPEG_SEQUENCE_HEADER                    : TGuid = '{3c036de7-3ad0-4c9e-9216-ee6d6ac21cb3}';


// INTERLEAVED - DV extra data /////////////////////////////////////////////////

  // {84bd5d88-0fb8-4ac8-be4b-a8848bef98f3}     MF_MT_DV_AAUX_SRC_PACK_0        {UINT32}
  MF_MT_DV_AAUX_SRC_PACK_0                      : TGuid = '{84bd5d88-0fb8-4ac8-be4b-a8848bef98f3}';

  // {f731004e-1dd1-4515-aabe-f0c06aa536ac}     MF_MT_DV_AAUX_CTRL_PACK_0       {UINT32}
  MF_MT_DV_AAUX_CTRL_PACK_0         : TGuid = '{f731004e-1dd1-4515-aabe-f0c06aa536ac}';

  // {720e6544-0225-4003-a651-0196563a958e}     MF_MT_DV_AAUX_SRC_PACK_1        {UINT32}
  MF_MT_DV_AAUX_SRC_PACK_1                      : TGuid = '{720e6544-0225-4003-a651-0196563a958e}';

  // {cd1f470d-1f04-4fe0-bfb9-d07ae0386ad8}     MF_MT_DV_AAUX_CTRL_PACK_1       {UINT32}
  MF_MT_DV_AAUX_CTRL_PACK_1                     : TGuid = '{cd1f470d-1f04-4fe0-bfb9-d07ae0386ad8}';

  // {41402d9d-7b57-43c6-b129-2cb997f15009}     MF_MT_DV_VAUX_SRC_PACK          {UINT32}
  MF_MT_DV_VAUX_SRC_PACK                        : TGuid = '{41402d9d-7b57-43c6-b129-2cb997f15009}';

  // {2f84e1c4-0da1-4788-938e-0dfbfbb34b48}     MF_MT_DV_VAUX_CTRL_PACK         {UINT32}
  MF_MT_DV_VAUX_CTRL_PACK                       : TGuid = '{2f84e1c4-0da1-4788-938e-0dfbfbb34b48}';


  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // >= Windows 7

// ARBITRARY ///////////////////////////////////////////////////////////////////

// MT_ARBITRARY_HEADER stores information about the format of an arbitrary media type

type
  PMtArbitraryHeader = ^TMtArbitraryHeader;
  {$EXTERNALSYM _MT_ARBITRARY_HEADER}
  _MT_ARBITRARY_HEADER = record
    majortype: TGUID;
    subtype: TGUID;
    bFixedSizeSamples: BOOL;
    bTemporalCompression: BOOL;
    lSampleSize: ULONG;
    formattype: TGUID;
  end;
  {$EXTERNALSYM MT_ARBITRARY_HEADER}
  MT_ARBITRARY_HEADER = _MT_ARBITRARY_HEADER;
  TMtArbitraryHeader = _MT_ARBITRARY_HEADER;

const
  // {9E6BD6F5-0109-4f95-84AC-9309153A19FC}   MF_MT_ARBITRARY_HEADER          {MT_ARBITRARY_HEADER}
  MF_MT_ARBITRARY_HEADER                      : TGuid = '{9E6BD6F5-0109-4f95-84AC-9309153A19FC}';

  // {5A75B249-0D7D-49a1-A1C3-E0D87F0CADE5}   MF_MT_ARBITRARY_FORMAT          {Blob}
  MF_MT_ARBITRARY_FORMAT                      : TGuid = '{5A75B249-0D7D-49a1-A1C3-E0D87F0CADE5}';



// IMAGE ///////////////////////////////////////////////////////////////////////

// {ED062CF4-E34E-4922-BE99-934032133D7C}     MF_MT_IMAGE_LOSS_TOLERANT       {UINT32 (BOOL)}
  MF_MT_IMAGE_LOSS_TOLERANT                   : TGuid = '{ED062CF4-E34E-4922-BE99-934032133D7C}';



// MPEG-4 Media Type Attributes ////////////////////////////////////////////////

  // {261E9D83-9529-4B8F-A111-8B9C950A81A9}   MF_MT_MPEG4_SAMPLE_DESCRIPTION   {BLOB}
  MF_MT_MPEG4_SAMPLE_DESCRIPTION              : TGuid = '{261E9D83-9529-4B8F-A111-8B9C950A81A9}';

  // {9aa7e155-b64a-4c1d-a500-455d600b6560}   MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY {UINT32}
  MF_MT_MPEG4_CURRENT_SAMPLE_ENTRY            : TGuid = '{9aa7e155-b64a-4c1d-a500-455d600b6560}';


  // Save original format information for AVI and WAV files

  // {d7be3fe0-2bc7-492d-b843-61a1919b70c3}   MF_MT_ORIGINAL_4CC               (UINT32)
  MF_MT_ORIGINAL_4CC                          : TGuid = '{d7be3fe0-2bc7-492d-b843-61a1919b70c3}';

  // {8cbbc843-9fd9-49c2-882f-a72586c408ad}   MF_MT_ORIGINAL_WAVE_FORMAT_TAG   (UINT32)
  MF_MT_ORIGINAL_WAVE_FORMAT_TAG              : TGuid = '{8cbbc843-9fd9-49c2-882f-a72586c408ad}';



  // Video Capture Media Type Attributes /////////////////////////////////////////


  // {D2E7558C-DC1F-403f-9A72-D28BB1EB3B5E}   MF_MT_FRAME_RATE_RANGE_MIN      {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_FRAME_RATE_RANGE_MIN                  : TGuid = '{D2E7558C-DC1F-403f-9A72-D28BB1EB3B5E}';

  // {E3371D41-B4CF-4a05-BD4E-20B88BB2C4D6}   MF_MT_FRAME_RATE_RANGE_MAX      {UINT64 (HI32(Numerator),LO32(Denominator))}
  MF_MT_FRAME_RATE_RANGE_MAX                  : TGuid = '{E3371D41-B4CF-4a05-BD4E-20B88BB2C4D6}';


  //#endif // (WINVER >= _WIN32_WINNT_WIN7)
  // end >= Windows 7



////////////////////////////////////////////////////////////////////////////////
///////////////////////////////  Media Type GUIDs //////////////////////////////
////////////////////////////////////////////////////////////////////////////////





// Representations /////////////////////////////////////////////////////////////

  AM_MEDIA_TYPE_REPRESENTATION                : TGuid = '{e2e42ad2-132c-491e-a268-3c7c2dca181f}';
  FORMAT_MFVideoFormat                        : TGuid = '{aed4ab2d-7326-43cb-9464-c879cab9c43d}';


////////////////////////  Media Type functions /////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


// Forward declaration /////////////////////////////////////////////////////////


type

  //struct tagVIDEOINFOHEADER;
  PVideoinfoheader = ^TVideoinfoheader;
  {$EXTERNALSYM VIDEOINFOHEADER}
  VIDEOINFOHEADER = tagVIDEOINFOHEADER;
  TVideoinfoheader = tagVIDEOINFOHEADER;

  //struct tagVIDEOINFOHEADER2;
  PVideoinfoheader2 = ^TVideoinfoheader2;
  {$EXTERNALSYM VIDEOINFOHEADER2}
  VIDEOINFOHEADER2 = tagVIDEOINFOHEADER2;
  TVideoinfoheader2 = tagVIDEOINFOHEADER2;

  //struct tagMPEG1VIDEOINFO;
  PMpeg1videoinfo = ^TMpeg1videoinfo;
  {$EXTERNALSYM MPEG1VIDEOINFO}
  MPEG1VIDEOINFO = tagMPEG1VIDEOINFO;
  TMpeg1videoinfo = tagMPEG1VIDEOINFO;

  //struct tagMPEG2VIDEOINFO;
  PMpeg2videoinfo = ^TMpeg2videoinfo;
  {$EXTERNALSYM MPEG2VIDEOINFO}
  MPEG2VIDEOINFO = tagMPEG2VIDEOINFO;
  TMpeg2videoinfo = tagMPEG2VIDEOINFO;

  //struct _AMMediaType;
  PAmMediaType = ^TAmMediaType;
  {$EXTERNALSYM AM_MEDIA_TYPE}
  AM_MEDIA_TYPE = _AMMediaType;
  TAmMediaType = _AMMediaType;


  function MFValidateMediaTypeSize(const FormatType: TGUID; const pBlock: UINT8; const cbSize: UINT32): HResult; stdcall;
  function MFCreateMediaType(out ppMFType: IMFMediaType): HResult; stdcall;
  function MFCreateMFVideoFormatFromMFMediaType(const pMFType: IMFMediaType; out ppMFVF: MFVIDEOFORMAT;{must be deleted with CoTaskMemFree} out pcbSize: UINT32): HResult; stdcall;

type
  PMFWaveFormatExConvertFlags = ^TMFWaveFormatExConvertFlags;
  {$EXTERNALSYM _MFWaveFormatExConvertFlags}
  _MFWaveFormatExConvertFlags                 = (
    MFWaveFormatExConvertFlag_Normal          = 0,
    MFWaveFormatExConvertFlag_ForceExtensible = 1
  );
  {$EXTERNALSYM MFWaveFormatExConvertFlags}
  MFWaveFormatExConvertFlags = _MFWaveFormatExConvertFlags;
  TMFWaveFormatExConvertFlags = _MFWaveFormatExConvertFlags;


  // declarations with default parameters ////////////////////////////////////////

  function MFCreateWaveFormatExFromMFMediaType(const pMFType: IMFMediaType; out ppWF: WAVEFORMATEX; out pcbSize: UINT32; const Flags: UINT32{MFWaveFormatExConvertFlag_Normal}): HResult; stdcall;
  function MFInitMediaTypeFromVideoInfoHeader(const pMFType: IMFMediaType; const pVIH: PVIDEOINFOHEADER; const cbBufSize: UINT32; const pSubtype: TGUID{Nil}): HResult; stdcall;
  function MFInitMediaTypeFromVideoInfoHeader2(const pMFType: IMFMediaType; const pVIH2: PVIDEOINFOHEADER2; const cbBufSize: UINT32; const pSubtype: TGUID{Nil}): HResult; stdcall;
  function MFInitMediaTypeFromMPEG1VideoInfo(const pMFType: IMFMediaType; const pMP1VI: PMPEG1VIDEOINFO; const cbBufSize: UINT32; const pSubtype: TGUID{Nil}): HResult; stdcall;
  function MFInitMediaTypeFromMPEG2VideoInfo(const pMFType: IMFMediaType; const pMP2VI: PMPEG2VIDEOINFO; const cbBufSize: UINT32; const pSubtype: TGUID{Nil}): HResult; stdcall;
  function MFCalculateBitmapImageSize(const pBMIH: PBITMAPINFOHEADER; const cbBufSize: UINT32; out pcbImageSize: UINT32; out pbKnown: PBOOL{Nil}): HResult; stdcall;
  function MFCalculateImageSize(const guidSubtype: REFGUID; const unWidth: UINT32; const unHeight: UINT32; out pcbImageSize: UINT32): HResult; stdcall;
  function MFFrameRateToAverageTimePerFrame(const unNumerator: UINT32; const unDenominator: UINT32; out punAverageTimePerFrame: PUINT64): HResult; stdcall;
  function MFAverageTimePerFrameToFrameRate(const unAverageTimePerFrame: UINT64; out punNumerator: UINT32; out punDenominator: UINT32): HResult; stdcall;
  function MFInitMediaTypeFromMFVideoFormat(const pMFType: IMFMediaType; const pMFVF: PMFVIDEOFORMAT; const cbBufSize: UINT32): HResult; stdcall;
  function MFInitMediaTypeFromWaveFormatEx(const pMFType: IMFMediaType; const pWaveFormat: PWAVEFORMATEX; const cbBufSize: UINT32): HResult; stdcall;
  function MFInitMediaTypeFromAMMediaType(const pMFType: IMFMediaType; const pAMType: AM_MEDIA_TYPE): HResult; stdcall;
  function MFInitAMMediaTypeFromMFMediaType(const pMFType: IMFMediaType; const guidFormatBlockType: TGUID; var PAM_MEDIA_TYPE): HResult; stdcall;
  function MFCreateAMMediaTypeFromMFMediaType(const pMFType: IMFMediaType; const guidFormatBlockType: TGUID; var PPAM_MEDIA_TYPE): HResult; stdcall;


// This function compares a full media type to a partial media type.
//
// A "partial" media type is one that is given out by a component as a possible
// media type it could accept. Many attributes may be unset, which represents
// a "don't care" status for that attribute.
//
// For example, a video effect may report that it supports YV12,
// but not want to specify a particular size. It simply creates a media type and sets
// the major type to MFMediaType_Video and the subtype to MEDIASUBTYPE_YV12.
//
// The comparison function succeeds if the partial type contains at least a major type,
// and all of the attributes in the partial type exist in the full type and are set to
// the same value.

  //STDAPI_(BOOL)
  function MFCompareFullToPartialMediaType(const pMFTypeFull: IMFMediaType; const pMFTypePartial: IMFMediaType): Boolean; stdcall;
  function MFWrapMediaType(const pOrig: IMFMediaType; const MajorType: REFGUID; const SubType: REFGUID; out ppWrap: IMFMediaType): HResult; stdcall;
  function MFUnwrapMediaType(const pWrap: IMFMediaType; out ppOrig: IMFMediaType): HResult; stdcall;


// MFCreateVideoMediaType //////////////////////////////////////////////////////

  //#ifdef _KSMEDIA_
  function MFCreateVideoMediaTypeFromVideoInfoHeader(const pVideoInfoHeader: PKS_VIDEOINFOHEADER; const cbVideoInfoHeader: DWORD; const dwPixelAspectRatioX: DWORD;
                                                    const dwPixelAspectRatioY: DWORD; const InterlaceMode: MFVideoInterlaceMode; const VideoFlags: QWORD;
                                                    const pSubtype: TGUID; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;
  function MFCreateVideoMediaTypeFromVideoInfoHeader2(const pVideoInfoHeader: PKS_VIDEOINFOHEADER2; const cbVideoInfoHeader: DWORD; const AdditionalVideoFlags: QWORD;
                                                      const pSubtype: TGUID; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;
  // #endif

  function MFCreateVideoMediaType(const pVideoFormat: PMFVIDEOFORMAT; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;
  function MFCreateVideoMediaTypeFromSubtype(const pAMSubtype: TGUID; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;


  //STDAPI_(BOOL)
  function MFIsFormatYUV(const Format: DWORD): Boolean; stdcall;



  //These depend on BITMAPINFOHEADER being defined ///////////////////////////
  //This function is not implemented.
  function MFCreateVideoMediaTypeFromBitMapInfoHeader(const pbmihBitMapInfoHeader: PBITMAPINFOHEADER; const dwPixelAspectRatioX: DWORD; const dwPixelAspectRatioY: DWORD;
                                                      const InterlaceMode: MFVideoInterlaceMode; const VideoFlags: QWORD; const qwFramesPerSecondNumerator: QWORD;
                                                      const qwFramesPerSecondDenominator: QWORD; const dwMaxBitRate: DWORD; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;
  //Calculates the minimum surface stride for a video format.
  function MFGetStrideForBitmapInfoHeader(const format: DWORD; const dwWidth: DWORD; out pStride: LONG): HResult; stdcall;
  //Retrieves the image size, in bytes, for an uncompressed video format.
  function MFGetPlaneSize(const format: DWORD; const dwWidth: DWORD; const dwHeight: DWORD; out pdwPlaneSize: PDWORD): HResult; stdcall;


  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // >= Windows 7
  //MFCreateVideoMediaTypeFromBitMapInfoHeaderEx
  function MFCreateVideoMediaTypeFromBitMapInfoHeaderEx(const pbmihBitMapInfoHeader: PBITMAPINFOHEADER; const cbBitMapInfoHeader: UINT32; const dwPixelAspectRatioX: DWORD;
                                                        const dwPixelAspectRatioY: DWORD; const InterlaceMode: MFVideoInterlaceMode; const VideoFlags: QWORD;
                                                        const dwFramesPerSecondNumerator: DWORD; const dwFramesPerSecondDenominator: DWORD; const dwMaxBitRate: DWORD;
                                                        out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)
  // end >= Windows 7


  //MFCreateMediaTypeFromRepresentation
  //Creates a Media Foundation media type from another format representation.
  function MFCreateMediaTypeFromRepresentation(guidRepresentation: TGUID; const pvRepresentation: Pointer; out ppIMediaType: IMFMediaType): HResult; stdcall;
  //MFCreateAudioMediaType
  //Creates an audio media type from a WAVEFORMATEX structure.
  //NOTE: This API is not supported and may be altered or unavailable in the future.
  function MFCreateAudioMediaType(const pAudioFormat: PWAVEFORMATEX; out ppIAudioMediaType: IMFAudioMediaType): HResult; stdcall;


  //Returns the FOURCC or D3DFORMAT value for an uncompressed video format.
  //NOTE: This API is not supported and may be altered or unavailable in the future.
  //Applications should avoid using the MFVIDEOFORMAT structure,
  //and use media type attributes instead. For more information, see Video Media Types.
  function MFGetUncompressedVideoFormat(const pVideoFormat: MFVIDEOFORMAT): DWord; stdcall;

  //Initializes an MFVIDEOFORMAT structure for a standard video format such as DVD, analog television, or ATSC digital television.
  //NOTE: This API is not supported and may be altered or unavailable in the future.
  //Applications should avoid using the MFVIDEOFORMAT structure,
  //and use media type attributes instead. For more information, see Video Media Types.
  function MFInitVideoFormat(const pVideoFormat: PMFVIDEOFORMAT; const _type: MFStandardVideoFormat): HResult; stdcall;

  //Initializes an MFVIDEOFORMAT structure for an uncompressed RGB video format.
  //NOTE: [This API is not supported and may be altered or unavailable in the future.
  //Applications should avoid using the MFVIDEOFORMAT structure,
  //and use media type attributes instead. For more information, see Video Media Types.
  function MFInitVideoFormat_RGB(const pVideoFormat: PMFVIDEOFORMAT; const dwWidth: DWORD; const dwHeight: DWORD; const D3Dfmt: DWORD): HResult; stdcall;

  //Converts the extended color information from an MFVIDEOFORMAT to the equivalent DirectX Video Acceleration (DXVA) color information.
  //NOTE: This API is not supported and may be altered or unavailable in the future.
  //Applications should avoid using the MFVIDEOFORMAT structure,
  //and use media type attributes instead. For more information, see Extended Color Information.
  function MFConvertColorInfoToDXVA(out pdwToDXVA: PDWORD; const pFromFormat: PMFVIDEOFORMAT): HResult; stdcall;

  //Sets the extended color information in a MFVIDEOFORMAT structure.
  //NOTE: This API is not supported and may be altered or unavailable in the future.
  //Applications should avoid using the MFVIDEOFORMAT structure,
  //and use media type attributes instead. For more information, see Extended Color Information.
  function MFConvertColorInfoFromDXVA(var pToFormat: PMFVIDEOFORMAT; const dwFromDXVA: DWORD): HResult; stdcall;




  // Optimized stride copy function

  //Copies an image or image plane from one buffer to another.
  function MFCopyImage(const pDest: PByte; const lDestStride: LONG; const pSrc: PByte;
                       const lSrcStride: LONG; const dwWidthInBytes: DWORD; const dwLines: DWORD ): HResult; stdcall;
  //Converts an array of 16-bit floating-point numbers into an array of 32-bit floating-point numbers.
  function MFConvertFromFP16Array(var pDest: Single; var pSrc: WORD; dwCount: DWORD): HResult; stdcall;
  //Converts an array of 32-bit floating-point numbers into an array of 16-bit floating-point numbers.
  function MFConvertToFP16Array(out pDest: WORD; const pSrc: Single; dwCount: DWORD): HResult; stdcall;


  ///////////////////  Attributes Utility functions ////////////////////////////
  //////////////////////////////////////////////////////////////////////////////


// IMFAttributes inline UTILITY FUNCTIONS - used for IMFMediaType as well //////


  {$EXTERNALSYM HI32}
  function HI32(unPacked: UINT64): UINT32; stdcall;

  {$EXTERNALSYM LO32}
  function LO32(unPacked: UINT64): UINT32; stdcall;
  //return (UINT32)unPacked;

  //inline
  {$EXTERNALSYM Pack2UINT32AsUINT64}
  //Packs two UINT32 values into a UINT64 value.
  //Returns the packed UINT64 value.
  //NOTE: This function stores two 32-bit values in a 64-bit value that is suitable for
  //      the IMFAttributes.SetUINT64 method.
  function Pack2UINT32AsUINT64(unHigh: UINT32; unLow: UINT32): UINT64; stdcall;
  //return ((UINT64)unHigh << 32) | unLow;

  //inline
  {$EXTERNALSYM Unpack2UINT32AsUINT64}
  //Gets the low-order and high-order UINT32 values from a UINT64 value.
  //This procedure does not return a value.
  procedure Unpack2UINT32AsUINT64(unPacked: UINT64; var punHigh: UINT32; var punLow: UINT32); stdcall;
  //*punHigh = HI32(unPacked);
  //*punLow = LO32(unPacked);

  //inline
//  {$EXTERNALSYM PackSize}
  //Packs a UINT32 width value and a UINT32 height value into a UINT64 value that represents a size.
  //Returns the packed UINT64 value.
  //NOTE: This function stores two 32-bit values in a 64-bit value that is suitable for
  //      the IMFAttributes.SetUINT64 method.
//#### TO DO ####  function PackSize(unWidth: UINT32; unHeight: UINT32): UINT64; stdcall;
  //Return Pack2UINT32AsUINT64(unWidth, unHeight);

  //inline
//  {$EXTERNALSYM UnpackSize}
  //Gets the low-order and high-order UINT32 values from a UINT64 value that represnets a size.
  //You can use this function to unpack a UINT64 value that you receive from the IMFAttributes.GetUINT64 method.
//#### TO DO ####  procedure UnpackSize(unPacked: UINT64; var punWidth: UINT32; var punHeight: UINT32); stdcall;
  // return Unpack2UINT32AsUINT64(unPacked, punWidth, punHeight);

  //inline
//  {$EXTERNALSYM PackRatio}
  //Packs two UINT32 values, which represent a ratio, into a UINT64 value.
  //Returns the packed UINT64 value.
  //NOTE: This function stores two 32-bit values in a 64-bit value that is suitable for
  //      the IMFAttributes.SetUINT64 method.
//#### TO DO ####  function PackRatio(nNumerator: INT32; unDenominator: UINT32): UINT64; stdcall;
  // return Pack2UINT32AsUINT64((UINT32)nNumerator, unDenominator);

  //inline
//  {$EXTERNALSYM UnpackRatio}
  //Gets the low-order and high-order UINT32 values from a UINT64 value that represents a ratio.
  //NOTE: You can use this function to unpack a UINT64 value that you receive from
  //      the IMFAttributes.GetUINT64 method.
//#### TO DO ####  procedure UnpackRatio(unPacked: UINT64; var pnNumerator: INT32; var punDenominator: UINT32); stdcall;
  // Unpack2UINT32AsUINT64(unPacked, (UINT32*)pnNumerator, punDenominator);



// "failsafe" inline get methods - return the stored value or return a default /

  //inline
//  {$EXTERNALSYM MFGetAttributeUINT32}
  //Returns a UINT32 value from an attribute store, or a default value if the attribute is not present.
  //NOTE: This helper function queries the attribute store for the UINT32 value specified by guidKey.
  //      If the value is not present or does not have type UINT32, the function returns unDefault.
//#### TO DO ####  function MFGetAttributeUINT32(const pAttributes: IMFAttributes; const guidKey: REFGUID; unDefault: UINT32): UINT32; stdcall;
  //UINT32 unRet;
  //    if (FAILED(pAttributes->GetUINT32(guidKey, &unRet))) {
  //        unRet = unDefault;
  //    }
  //    return unRet;


  //inline
//  {$EXTERNALSYM MFGetAttributeUINT64}
  //Returns a UINT64 value from an attribute store, or a default value if the attribute is not present.
  //NOTE: This helper function queries the attribute store for the UINT64 value specified by guidKey.
  //      If the value is not present, the function returns unDefault.
//#### TO DO ####  function MFGetAttributeUINT64(const pAttributes: IMFAttributes; const guidKey: REFGUID; unDefault: UINT64): UINT64; stdcall;
  //UINT64 unRet;
  //    if (FAILED(pAttributes->GetUINT64(guidKey, &unRet))) {
  //        unRet = unDefault;
  //    }
  //    return unRet;

  //inline
//  {$EXTERNALSYM MFGetAttributeDouble}
  //Returns a double value from an attribute store, or a default value if the attribute is not present.
  //NOTE: This helper function queries the attribute store for the attribute specified by guidKey.
  //      If the attribute is not present or does not have type double, the function returns fDefault.
  //      This function is convenient because it never returns a failure code.
  //      However, if the attribute in question does not have a meaningful default value,
  //      you should call IMFAttributes.GetDouble and check for MF_E_ATTRIBUTENOTFOUND.
//#### TO DO ####  function MFGetAttributeDouble(const pAttributes: IMFAttributes; const guidKey: REFGUID; fDefault: Double ): Double; stdcall;
  //double fRet;
  //    if (FAILED(pAttributes->GetDouble(guidKey, &fRet))) {
  //       fRet = fDefault;
  //    }
  //    return fRet;



// helpers for getting/setting ratios and sizes ////////////////////////////////


  //inline
//  {$EXTERNALSYM MFGetAttribute2UINT32asUINT64}
  //Gets an attribute whose value is two UINT32 values packed into a UINT64.
  //NOTE: Internally, this function calls IMFAttributes.GetUINT64 to get the UINT64 value,
  //      and Unpack2UINT32AsUINT64 to unpack the two 32-bit values.
//#### TO DO ####  function MFGetAttribute2UINT32asUINT64(const pAttributes: IMFAttributes; const guidKey: REFGUID; out punHigh32: UINT32; out punLow32: UINT32): HResult; stdcall;
  //{
  //  NT64 unPacked;
  //  HRESULT hr = S_OK;
  //
  //  hr = pAttributes->GetUINT64(guidKey, &unPacked);
  //  if (FAILED(hr)) {
  //      return hr;
  //  }
  //  Unpack2UINT32AsUINT64(unPacked, punHigh32, punLow32);
  //
  //  return hr;
  //}

  //inline
//  {$EXTERNALSYM MFSetAttribute2UINT32asUINT64}
  //Packs two UINT32 values into a UINT64 attribute value.
  //NOTE: Internally, this functions calls Pack2UINT32AsUINT64 to create the 64-bit value,
  //      and IMFAttributes.SetUINT64 to set the attribute.
//#### TO DO ####  function MFSetAttribute2UINT32asUINT64(const pAttributes: IMFAttributes; const guidKey: REFGUID; unHigh32: UINT32; unLow32: UINT32): HResult; stdcall;
  // {
  //  return pAttributes->SetUINT64(guidKey, Pack2UINT32AsUINT64(unHigh32, unLow32));
  // }

  //inline
//  {$EXTERNALSYM MFGetAttributeRatio}
  //Retrieves an attribute whose value is a ratio.
  //NOTE: Some attributes specify a ratio as a packed UINT64 value.
  //      Use this function to get the numerator and denominator as separate 32-bit values.
//#### TO DO ####  function MFGetAttributeRatio(const pAttributes: IMFAttributes; const guidKey: REFGUID; out punNumerator: UINT32; out punDenominator: UINT32): HResult; stdcall;
  {
    return MFGetAttribute2UINT32asUINT64(pAttributes, guidKey, punNumerator, punDenominator);
  }

  //inline
//  {$EXTERNALSYM MFGetAttributeSize}
  //Retrieves an attribute whose value is a size, expressed as a width and height.
  //NOTE: Some attributes specify a size as a packed UINT64 value.
  //      Use this function to get the numerator and denominator as separate 32-bit values.
//#### TO DO ####  function MFGetAttributeSize(const pAttributes: IMFAttributes; const guidKey: REFGUID; out punWidth: UINT32; out punHeight: UINT32): HResult; stdcall;
  {
    return MFGetAttribute2UINT32asUINT64(pAttributes, guidKey, punWidth, punHeight);
  }

  //inline
//  {$EXTERNALSYM MFSetAttributeRatio}
  //Sets a ratio as a 64-bit attribute value.
  //NOTE: Some attributes specify a ratio as a packed UINT64 value.
  //      This function packs the numerator and denominator into a single UINT64 value.
//#### TO DO ####  function MFSetAttributeRatio(const pAttributes: IMFAttributes; const guidKey: REFGUID; unNumerator: UINT32; unDenominator: UINT32): HResult; stdcall;
  {
    return MFSetAttribute2UINT32asUINT64(pAttributes, guidKey, unNumerator, unDenominator);
  }

  //inline
//  {$EXTERNALSYM MFSetAttributeSize}
//#### TO DO ####  function MFSetAttributeSize(const pAttributes: IMFAttributes; const guidKey: REFGUID; out unWidth: UINT32; out unHeight: UINT32): HResult; stdcall;
  {
    return MFSetAttribute2UINT32asUINT64(pAttributes, guidKey, unWidth, unHeight);
  }


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////  Memory Management ////////////////////////////
////////////////////////////////////////////////////////////////////////////////


type
// Heap alloc/free
//  PEAllocationType = ^TEAllocationType;
  {$EXTERNALSYM _EAllocationType}
  _EAllocationType = (
    eAllocationTypeDynamic,
    eAllocationTypeRT,
    eAllocationTypePageable,
    eAllocationTypeIgnore
  );
  {$EXTERNALSYM EAllocationType}
  EAllocationType = _EAllocationType;
  TEAllocationType = _EAllocationType;


  //Allocates a block of memory.
  function MFHeapAlloc(const nSize: size_t; const dwFlags: ULONG; const pszFile: PAnsiChar;
                      const line: Integer; const eat: EAllocationType ): Pointer;

  //Frees a block of memory that was allocated by calling the MFHeapAlloc function.
  //NOTE: This function does not return a value.
  procedure MFHeapFree(pv: Pointer);

///////////////////////////////  Collection         ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  // Instantiates the MF-provided IMFCollection implementation
  function MFCreateCollection(out ppIMFCollection: IMFCollection): HResult; stdcall;


////////////////////////////////////////////////////////////////////////////////
//////////////////////////       SourceResolver     ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

const
  CLSID_MFSourceResolver                         : TGuid = '{90eab60f-e43a-4188-bcc4-e47fdf04868c}';


  //  Return (a * b + d) / c
  //  Returns _I64_MAX or LLONG_MIN on failure or _I64_MAX if mplat.dll is not available
  {$EXTERNALSYM MFllMulDiv}
  function MFllMulDiv(a: LONGLONG; b: LONGLONG; c: LONGLONG; d: LONGLONG): LONGLONG; winapi;

////////////////////////////////////////////////////////////////////////////////
//// Delphi Helpers  ///////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

  //See for usage:  function FCC(ch4: TCh4) and function DEFINE_MEDIATYPE_GUID(format: DWord)
  function DefineMediaTypeGuidByFourCC(sFcc: TCh4): TGuid;
  //See for usage:  function FCC(ch4: TCh4) and function DEFINE_MEDIATYPE_GUID(format: DWord)
  function DefineMediaTypeGuidByDWord(dwConst: DWord = 0): TGuid;


implementation

  function HI32(unPacked: UINT64): UINT32; stdcall;
  Begin
    Result:=unPacked shr 32;
  End;

  function LO32(unPacked: UINT64): UINT32; stdcall;
  Begin
    Result:=unPacked and $0ffffffff;
  End;

  function Pack2UINT32AsUINT64(unHigh: UINT32; unLow: UINT32): UINT64; stdcall;
  Begin
    Result:= unHigh shl 32 or unLow;
  End;

  procedure Unpack2UINT32AsUINT64(unPacked: UINT64; var punHigh: UINT32; var punLow: UINT32); stdcall;
  Begin
    punHigh:= HI32(unPacked);
    punLow:= LO32(unPacked);
  End;

  // Helper function to access the macro translations, mentioned under REMARK#1
  function DefineMediaTypeGuidByFourCC(sFcc: TCh4): TGuid;
    begin
	    Result:= DEFINE_MEDIATYPE_GUID(FCC(sFcc));
    end;

  // Helper function to access the macro translations, mentioned under REMARK#1
  function DefineMediaTypeGuidByDWord(dwConst: DWord): TGuid;
    begin
      Result:= DEFINE_MEDIATYPE_GUID(dwConst);
    end;

//--------------------- Macros converted to functions ------------------------

// Peter
function FCC(ch4: TCh4): DWord;
begin
	Result:=  DWord(ch4[0]) or
						DWord(ch4[1]) shl 8 or
						DWord(ch4[2]) shl 16 or
						DWord(ch4[3]) shl 24;
end;

// Tony
function DEFINE_MEDIATYPE_GUID(format: DWord): TGuid;
begin
//  with Result do
//		begin
			Result.D1:= format;
			Result.D2:= $0000;
			Result.D3:= $0010;
			Result.D4[0]:= $80; Result.D4[1]:= $00; Result.D4[2]:= $00; Result.D4[3]:= $aa;
			Result.D4[4]:= $00; Result.D4[5]:= $38; Result.D4[6]:= $9b; Result.D4[7]:= $71;
//		end;
end;

//--------------------- External definitions ---------------------------------

  function MFStartup(const Version: ULONG; const dwFlags: DWORD): HRESULT; stdcall; external 'Mfplat.dll' name 'MFStartup';
  function MFShutdown: HRESULT; stdcall;                        external 'Mfplat.dll' name 'MFShutdown';
  function MFLockPlatform: HResult; stdcall;                    external 'Mfplat.dll' name 'MFLockPlatform';
  function MFUnlockPlatform: HResult; stdcall;          external 'Mfplat.dll' name 'MFUnlockPlatform';
  function MFPutWorkItem(dwQueue: DWORD; pCallback: IMFAsyncCallback; pState: IUnknown): HResult; stdcall;  external 'Mfplat.dll' name 'MFPutWorkItem';
  function MFPutWorkItemEx(dwQueue: DWORD; pResult: IMFAsyncResult): HResult; stdcall;  external 'Mfplat.dll' name 'MFPutWorkItemEx';
  function MFScheduleWorkItem(pCallback: IMFAsyncCallback; pState: IUnknown; Timeout: INT64; pKey: MFWORKITEM_KEY): HResult; stdcall;  external 'Mfplat.dll' name 'MFScheduleWorkItem';
  function MFScheduleWorkItemEx(pResult: IMFAsyncResult; Timeout: INT64; pKey: MFWORKITEM_KEY): HResult; stdcall;  external 'Mfplat.dll' name 'MFScheduleWorkItemEx';
  function MFCancelWorkItem(Key: MFWORKITEM_KEY): HResult; stdcall;  external 'Mfplat.dll' name 'MFCancelWorkItem';
  function MFGetTimerPeriodicity(out Periodicity: DWord): HResult; stdcall;  external 'Mfplat.dll' name 'MFGetTimerPeriodicity';
  function MFAddPeriodicCallback(const Callback: MFPERIODICCALLBACK; const pContext: IUnknown; out pdwKey: DWord): HResult; stdcall;  external 'Mfplat.dll' name 'MFAddPeriodicCallback';
  function MFRemovePeriodicCallback(const dwKey: DWord): HResult; stdcall;  external 'Mfplat.dll' name 'MFRemovePeriodicCallback';
  function MFAllocateWorkQueueEx(const WorkQueueType: MFASYNC_WORKQUEUE_TYPE; out pdwWorkQueue: DWord): HResult; stdcall;  external 'Mfplat.dll' name 'MFAllocateWorkQueueEx';
  function MFAllocateWorkQueue(out pdwWorkQueue: DWord): HResult; stdcall;  external 'Mfplat.dll' name 'MFAllocateWorkQueue';
  function MFLockWorkQueue(const dwWorkQueue: DWord): HResult; stdcall;  external 'Mfplat.dll' name 'MFLockWorkQueue';
  function MFUnlockWorkQueue(const dwWorkQueue: DWord): HResult; stdcall;  external 'Mfplat.dll' name 'MFUnlockWorkQueue';
  function MFBeginRegisterWorkQueueWithMMCSS(const dwWorkQueueId: DWord; const wszClass: LPCWSTR; const dwTaskId: DWord; const pDoneCallback: IMFAsyncCallback; pDoneState: IUnknown): HResult; stdcall; external 'Mfplat.dll' name 'MFBeginRegisterWorkQueueWithMMCSS';
  function MFEndRegisterWorkQueueWithMMCSS(const pResult: IMFAsyncResult; pdwTaskId: DWord): HResult; stdcall; external 'Mfplat.dll' name 'MFEndRegisterWorkQueueWithMMCSS';
  function MFBeginUnregisterWorkQueueWithMMCSS(const dwWorkQueueId: DWord; const pDoneCallback: IMFAsyncCallback; const pDoneState: IUnknown): HResult; stdcall; external 'Mfplat.dll' name 'MFBeginUnregisterWorkQueueWithMMCSS';
  function MFEndUnregisterWorkQueueWithMMCSS(const pResult: IMFAsyncResult): HResult; stdcall; external 'Mfplat.dll' name 'MFEndUnregisterWorkQueueWithMMCSS';
  function MFGetWorkQueueMMCSSClass(const dwWorkQueueId: DWord; out pwszClass: LPWSTR ; var pcchClass: DWord): HResult; stdcall; external 'Mfplat.dll' name 'MFGetWorkQueueMMCSSClass';
  function MFGetWorkQueueMMCSSTaskId(const dwWorkQueueId: DWord; out pdwTaskId: LPDWORD): HResult; stdcall; external 'Mfplat.dll' name 'MFGetWorkQueueMMCSSTaskId';
  function MFCreateAsyncResult(const punkObject: IUnknown; const pCallback: IMFAsyncCallback; const punkState: IUnknown; out ppAsyncResult: IMFAsyncResult): HResult; stdcall; external 'Mfplat.dll' name 'MFCreateAsyncResult';
  function MFInvokeCallback(const pAsyncResult: IMFAsyncResult): HResult; stdcall; external 'Mfplat.dll' name 'MFInvokeCallback';
  function MFCreateFile(const AccessMode: MF_FILE_ACCESSMODE; const OpenMode: MF_FILE_OPENMODE; const fFlags: MF_FILE_FLAGS; const pwszFileURL: LPCWSTR; out ppIByteStream: IMFByteStream): HResult; stdcall;  external 'Mfplat.dll' name 'MFCreateFile';
  function MFCreateTempFile(const AccessMode: MF_FILE_ACCESSMODE; const OpenMode: MF_FILE_OPENMODE; const fFlags: MF_FILE_FLAGS; out ppIByteStream: IMFByteStream): HResult; stdcall;  external 'Mfplat.dll' name 'MFCreateTempFile';
  function MFBeginCreateFile(const AccessMode: MF_FILE_ACCESSMODE; const OpenMode: MF_FILE_OPENMODE; const fFlags: MF_FILE_FLAGS; const pwszFilePath: LPCWSTR;const  pCallback: IMFAsyncCallback; const pState: IUnknown; out ppCancelCookie: IUnknown): HResult; stdcall;  external 'Mfplat.dll' name 'MFBeginCreateFile';
  function MFEndCreateFile(const pResult: IMFAsyncResult; out ppFile: IMFByteStream): HResult; stdcall;  external 'Mfplat.dll' name 'MFEndCreateFile';
  function MFCancelCreateFile(const pCancelCookie: IUnknown): HResult; stdcall;  external 'Mfplat.dll' name 'MFCancelCreateFile';
  function MFCreateMemoryBuffer(const cbMaxLength: DWord; out ppBuffer: IMFMediaBuffer): HResult; stdcall;  external 'Mfplat.dll' name 'MFCreateMemoryBuffer';
  function MFCreateMediaBufferWrapper(const pBuffer: IMFMediaBuffer; const cbOffset: DWord; const dwLength: DWord; out ppBuffer: IMFMediaBuffer): HResult; stdcall;  external 'Mfplat.dll' name 'MFCreateMediaBufferWrapper';
  function MFCreateLegacyMediaBufferOnMFMediaBuffer(const pSample: IMFSample; const pMFMediaBuffer: IMFMediaBuffer; const cbOffset: DWord; out ppMediaBuffer: IMediaBuffer): HResult; stdcall;  external 'Mfplat.dll' name 'MFCreateLegacyMediaBufferOnMFMediaBuffer';
  function MFCreateDXSurfaceBuffer(const riid: REFIID; const punkSurface: IUnknown; const fBottomUpWhenLinear: Boolean; out ppBuffer: IMFMediaBuffer): HResult; stdcall;  external 'Mfplat.dll' name 'MFCreateDXSurfaceBuffer';
  function MFCreateAlignedMemoryBuffer(const cbMaxLength: DWord; const cbAligment: DWord; out ppBuffer: IMFMediaBuffer): HResult; stdcall;  external 'Mfplat.dll' name 'MFCreateAlignedMemoryBuffer';
  function MFCreateMediaEvent(const met: MediaEventType; const guidExtendedType: REFGUID; const hrStatus: HRESULT; const pvValue: PROPVARIANT; ppEvent: IMFMediaEvent): HResult; stdcall;  external 'Mfplat.dll' name 'MFCreateMediaEvent';
  function MFCreateEventQueue(out ppMediaEventQueue: IMFMediaEventQueue): HResult; stdcall;  external 'Mfplat.dll' name 'MFCreateEventQueue';
  function MFCreateSample(out ppIMFSample: IMFSample): HResult; stdcall;               external 'Mfplat.dll' name 'MFCreateSample';
  function MFCreateAttributes(out ppMFAttributes: IMFAttributes; const cInitialSize: UINT32): HResult; stdcall; external 'Mfplat.dll' name 'MFCreateAttributes';
  function MFInitAttributesFromBlob(const pAttributes: IMFAttributes; const pBuf: UINT8; const cbBufSize: UINT): HResult; stdcall; external 'Mfplat.dll' name 'MFInitAttributesFromBlob';
  function MFGetAttributesAsBlobSize(const pAttributes: IMFAttributes; out pcbBufSize: UINT32): HResult; stdcall; external 'Mfplat.dll' name 'MFGetAttributesAsBlobSize';
  function MFGetAttributesAsBlob(const pAttributes: IMFAttributes; out pBuf: UINT8; const cbBufSize: UINT): HResult; stdcall; external 'Mfplat.dll' name 'MFGetAttributesAsBlob';

  function MFTRegister(const clsidMFT: CLSID; const guidCategory: TGUID; const pszName: LPCWSTR; const Flags: UINT32; const cInputTypes: UINT32; const pInputTypes: MFT_REGISTER_TYPE_INFO; const cOutputTypes: UINT32; const pOutputTypes: MFT_REGISTER_TYPE_INFO; const pAttributes: IMFAttributes): HResult; stdcall;external 'Mfplat.dll' name 'MFTRegister';
  function MFTUnregister(const clsidMFT: CLSID): HResult; stdcall;external 'Mfplat.dll' name 'MFTUnregister';
  function MFTRegisterLocal(const pClassFactory: IClassFactory; const guidCategory: REFGUID; const pszName: LPCWSTR; const Flags: UINT32; const cInputTypes: UINT32; const pInputTypes: MFT_REGISTER_TYPE_INFO; const cOutputTypes: UINT32; const pOutputTypes: MFT_REGISTER_TYPE_INFO): HResult; stdcall;external 'Mfplat.dll' name 'MFTRegisterLocal';
  function MFTUnregisterLocal(const pClassFactory: IClassFactory): HResult; stdcall; external 'Mfplat.dll' name 'MFTUnregisterLocal';
  function MFTRegisterLocalByCLSID(const clisdMFT: REFCLSID; const guidCategory: REFGUID; const pszName: LPCWSTR; const Flags: UINT32; const cInputTypes: UINT32; const pInputTypes: MFT_REGISTER_TYPE_INFO; const cOutputTypes: UINT32; const pOutputTypes: MFT_REGISTER_TYPE_INFO): HResult; stdcall; external 'Mfplat.dll' name 'MFTRegisterLocalByCLSID';
  function MFTUnregisterLocalByCLSID(const clsidMFT: CLSID): HResult; stdcall; external 'Mfplat.dll' name 'MFTUnregisterLocalByCLSID';
  function MFTEnum(const guidCategory: TGUID; const Flags: UINT32; const pInputType: MFT_REGISTER_TYPE_INFO; const pOutputType: MFT_REGISTER_TYPE_INFO; const pAttributes: IMFAttributes; out ppclsidMFT: CLSID {must be freed with CoTaskMemFree}; out pcMFTs: UINT32): HResult; stdcall;external 'Mfplat.dll' name 'MFTEnum';
  function MFTEnumEx(const guidCategory: TGuid; const Flags: UINT32; const pInputType: MFT_REGISTER_TYPE_INFO; const pOutputType: MFT_REGISTER_TYPE_INFO; out pppMFTActivate: IMFActivate; out pnumMFTActivate: UINT32): HResult; stdcall;external 'Mfplat.dll' name 'MFTEnumEx';
  function MFTGetInfo(const clsidMFT: CLSID; out pszName: PPWideChar; out ppInputTypes: MFT_REGISTER_TYPE_INFO; out pcInputTypes: UINT32; out ppOutputTypes: MFT_REGISTER_TYPE_INFO; out pcOutputTypes: UINT32; out ppAttributes: IMFAttributes): HResult; stdcall;     external 'Mfplat.dll' name 'MFTGetInfo';
  function MFGetPluginControl(out ppPluginControl: IMFPluginControl): HResult; stdcall;  external 'Mfplat.dll' name 'MFGetPluginControl';
  function MFGetMFTMerit(var pMFT: IUnknown; const cbVerifier: UINT32; const verifier: Byte; out merit: DWord): HResult; stdcall;external 'Mfplat.dll' name 'MFGetMFTMerit';
  function MFValidateMediaTypeSize(const FormatType: TGUID; const pBlock: UINT8; const cbSize: UINT32): HResult; stdcall;external 'Mfplat.dll' name 'MFValidateMediaTypeSize';
  function MFCreateMediaType(out ppMFType: IMFMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateMediaType';
  function MFCreateMFVideoFormatFromMFMediaType(const pMFType: IMFMediaType; out ppMFVF: MFVIDEOFORMAT;{must be deleted with CoTaskMemFree} out pcbSize: UINT32): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateMFVideoFormatFromMFMediaType';
  function MFCreateWaveFormatExFromMFMediaType(const pMFType: IMFMediaType; out ppWF: WAVEFORMATEX; out pcbSize: UINT32; const Flags: UINT32{MFWaveFormatExConvertFlag_Normal}): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateWaveFormatExFromMFMediaType';
  function MFInitMediaTypeFromVideoInfoHeader(const pMFType: IMFMediaType; const pVIH: PVIDEOINFOHEADER; const cbBufSize: UINT32; const pSubtype: TGUID{Nil}): HResult; stdcall;external 'Mfplat.dll' name 'MFInitMediaTypeFromVideoInfoHeader';
  function MFInitMediaTypeFromVideoInfoHeader2(const pMFType: IMFMediaType; const pVIH2: PVIDEOINFOHEADER2; const cbBufSize: UINT32; const pSubtype: TGUID{Nil}): HResult; stdcall;external 'Mfplat.dll' name 'MFInitMediaTypeFromVideoInfoHeader2';
  function MFInitMediaTypeFromMPEG1VideoInfo(const pMFType: IMFMediaType; const pMP1VI: PMPEG1VIDEOINFO; const cbBufSize: UINT32; const pSubtype: TGUID{Nil}): HResult; stdcall;external 'Mfplat.dll' name 'MFInitMediaTypeFromMPEG1VideoInfo';
  function MFInitMediaTypeFromMPEG2VideoInfo(const pMFType: IMFMediaType; const pMP2VI: PMPEG2VIDEOINFO; const cbBufSize: UINT32; const pSubtype: TGUID{Nil}): HResult; stdcall;external 'Mfplat.dll' name 'MFInitMediaTypeFromMPEG2VideoInfo';
  function MFCalculateBitmapImageSize(const pBMIH: PBITMAPINFOHEADER; const cbBufSize: UINT32; out pcbImageSize: UINT32; out pbKnown: PBOOL{Nil}): HResult; stdcall;external 'Mfplat.dll' name 'MFCalculateBitmapImageSize';
  function MFCalculateImageSize(const guidSubtype: REFGUID; const unWidth: UINT32; const unHeight: UINT32; out pcbImageSize: UINT32): HResult; stdcall;external 'Mfplat.dll' name 'MFCalculateImageSize';
  function MFFrameRateToAverageTimePerFrame(const unNumerator: UINT32; const unDenominator: UINT32; out punAverageTimePerFrame: PUINT64): HResult; stdcall;external 'Mfplat.dll' name 'MFFrameRateToAverageTimePerFrame';
  function MFAverageTimePerFrameToFrameRate(const unAverageTimePerFrame: UINT64; out punNumerator: UINT32; out punDenominator: UINT32): HResult; stdcall;external 'Mfplat.dll' name 'MFAverageTimePerFrameToFrameRate';
  function MFInitMediaTypeFromMFVideoFormat(const pMFType: IMFMediaType; const pMFVF: PMFVIDEOFORMAT; const cbBufSize: UINT32): HResult; stdcall;external 'Mfplat.dll' name 'MFInitMediaTypeFromMFVideoFormat';
  function MFInitMediaTypeFromWaveFormatEx(const pMFType: IMFMediaType; const pWaveFormat: PWAVEFORMATEX; const cbBufSize: UINT32): HResult; stdcall;external 'Mfplat.dll' name 'MFInitMediaTypeFromWaveFormatEx';
  function MFInitMediaTypeFromAMMediaType(const pMFType: IMFMediaType; const pAMType: AM_MEDIA_TYPE): HResult; stdcall;external 'Mfplat.dll' name 'MFInitMediaTypeFromAMMediaType';
  function MFInitAMMediaTypeFromMFMediaType(const pMFType: IMFMediaType; const guidFormatBlockType: TGUID; var PAM_MEDIA_TYPE): HResult; stdcall;external 'Mfplat.dll' name 'MFInitAMMediaTypeFromMFMediaType';
  function MFCreateAMMediaTypeFromMFMediaType(const pMFType: IMFMediaType; const guidFormatBlockType: TGUID; var PPAM_MEDIA_TYPE): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateAMMediaTypeFromMFMediaType';
  function MFCompareFullToPartialMediaType(const pMFTypeFull: IMFMediaType; const pMFTypePartial: IMFMediaType): Boolean; stdcall;external 'Mfplat.dll' name 'MFCompareFullToPartialMediaType';
  function MFWrapMediaType(const pOrig: IMFMediaType; const MajorType: REFGUID; const SubType: REFGUID; out ppWrap: IMFMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFWrapMediaType';
  function MFUnwrapMediaType(const pWrap: IMFMediaType; out ppOrig: IMFMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFUnwrapMediaType';
  function MFCreateVideoMediaTypeFromVideoInfoHeader(const pVideoInfoHeader: PKS_VIDEOINFOHEADER; const cbVideoInfoHeader: DWORD; const dwPixelAspectRatioX: DWORD; const dwPixelAspectRatioY: DWORD; const InterlaceMode: MFVideoInterlaceMode; const VideoFlags: QWORD; const pSubtype: TGUID; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateVideoMediaTypeFromVideoInfoHeader';
  function MFCreateVideoMediaTypeFromVideoInfoHeader2(const pVideoInfoHeader: PKS_VIDEOINFOHEADER2; const cbVideoInfoHeader: DWORD; const AdditionalVideoFlags: QWORD; const pSubtype: TGUID; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateVideoMediaTypeFromVideoInfoHeader2';
  function MFCreateVideoMediaType(const pVideoFormat: PMFVIDEOFORMAT; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateVideoMediaType';
  function MFCreateVideoMediaTypeFromSubtype(const pAMSubtype: TGUID; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateVideoMediaTypeFromSubtype';
  function MFIsFormatYUV(const Format: DWORD): Boolean; stdcall;external 'Mfplat.dll' name 'MFIsFormatYUV';
  function MFCreateVideoMediaTypeFromBitMapInfoHeader(const pbmihBitMapInfoHeader: PBITMAPINFOHEADER; const dwPixelAspectRatioX: DWORD; const dwPixelAspectRatioY: DWORD; const InterlaceMode: MFVideoInterlaceMode; const VideoFlags: QWORD; const qwFramesPerSecondNumerator: QWORD; const qwFramesPerSecondDenominator: QWORD; const dwMaxBitRate: DWORD; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateVideoMediaTypeFromBitMapInfoHeader';
  function MFGetStrideForBitmapInfoHeader(const format: DWORD; const dwWidth: DWORD; out pStride: LONG): HResult; stdcall;external 'Mfplat.dll' name 'MFGetStrideForBitmapInfoHeader';
  function MFGetPlaneSize(const format: DWORD; const dwWidth: DWORD; const dwHeight: DWORD; out pdwPlaneSize: PDWORD): HResult; stdcall;external 'Mfplat.dll' name 'MFGetPlaneSize';
  function MFCreateVideoMediaTypeFromBitMapInfoHeaderEx(const pbmihBitMapInfoHeader: PBITMAPINFOHEADER; const cbBitMapInfoHeader: UINT32; const dwPixelAspectRatioX: DWORD; const dwPixelAspectRatioY: DWORD; const InterlaceMode: MFVideoInterlaceMode; const VideoFlags: QWORD; const dwFramesPerSecondNumerator: DWORD; const dwFramesPerSecondDenominator: DWORD; const dwMaxBitRate: DWORD; out ppIVideoMediaType: IMFVideoMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateVideoMediaTypeFromBitMapInfoHeaderEx';
  function MFCreateMediaTypeFromRepresentation(guidRepresentation: TGUID; const pvRepresentation: Pointer; out ppIMediaType: IMFMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateMediaTypeFromRepresentation';
  function MFCreateAudioMediaType(const pAudioFormat: PWAVEFORMATEX; out ppIAudioMediaType: IMFAudioMediaType): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateAudioMediaType';
  function MFInitVideoFormat(const pVideoFormat: PMFVIDEOFORMAT; const _type: MFStandardVideoFormat): HResult; stdcall; external 'Mfplat.dll' name 'MFInitVideoFormat';
  function MFInitVideoFormat_RGB(const pVideoFormat: PMFVIDEOFORMAT; const dwWidth: DWORD; const dwHeight: DWORD; const D3Dfmt: DWORD): HResult; stdcall;external 'Mfplat.dll' name 'MFInitVideoFormat_RGB';
  function MFConvertColorInfoToDXVA(out pdwToDXVA: PDWORD; const pFromFormat: PMFVIDEOFORMAT): HResult; stdcall;external 'Mfplat.dll' name 'MFConvertColorInfoToDXVA';
  function MFConvertColorInfoFromDXVA(var pToFormat: PMFVIDEOFORMAT; const dwFromDXVA: DWORD): HResult; stdcall;external 'Mfplat.dll' name 'MFConvertColorInfoFromDXVA';
  function MFCopyImage(const pDest: PByte; const lDestStride: LONG; const pSrc: PByte; const lSrcStride: LONG; const dwWidthInBytes: DWORD; const dwLines: DWORD ): HResult; stdcall;external 'Mfplat.dll' name 'MFCopyImage';
  function MFConvertFromFP16Array(var pDest: Single; var pSrc: WORD; dwCount: DWORD): HResult; stdcall;external 'Mfplat.dll' name 'MFConvertFromFP16Array';
  function MFConvertToFP16Array(out pDest: WORD; const pSrc: Single; dwCount: DWORD): HResult; stdcall;external 'Mfplat.dll' name 'MFConvertToFP16Array';
  function MFCreateCollection(out ppIMFCollection: IMFCollection): HResult; stdcall;external 'Mfplat.dll' name 'MFCreateCollection';
  function MFGetUncompressedVideoFormat(const pVideoFormat: MFVIDEOFORMAT): DWord; stdcall; external 'Mfplat.dll' name 'MFGetUncompressedVideoFormat';
  function MFHeapAlloc(const nSize: size_t; const dwFlags: ULONG; const pszFile: PAnsiChar; const line: Integer; const eat: EAllocationType ): Pointer;  external 'Mfplat.dll' name 'MFHeapAlloc';
  procedure MFHeapFree(pv: Pointer); external 'Mfplat.dll' name 'MFHeapFree';
  function MFllMulDiv(a: LONGLONG; b: LONGLONG; c: LONGLONG; d: LONGLONG): LONGLONG; winapi;  external 'Mfplat.dll' name 'MFllMulDiv';
end.
