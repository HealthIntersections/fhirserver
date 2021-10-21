// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - MfPlay.pas
// Kind: Pascal Unit
// Release date: 28-06-2012
// Language: ENU
//
// Version: 1.0.0.1
// Description: DEPRECATED, Windows 7 ONLY. 
// 
// Intiator(s): Tony (maXcomX), Peter (OzShips) 
//
// LastEdited by: Tony (maXcomX)
// EditDate: updt 280612a
//
// Remarks:
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
// Source: mfplay.h
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

unit MfPlay;

  {$MINENUMSIZE 4}
  {$WEAKPACKAGEUNIT}

interface

uses
  Windows, ComObj, PropSys, MfIdl, Evr, Unknwn;


const
  IID_IMFPMediaPlayer                     : TGUID = '{A714590A-58AF-430a-85BF-44F5EC838D85}';
  IID_IMFPMediaItem                       : TGUID = '{90EB3E6B-ECBF-45cc-B1DA-C6FE3EA70D57}';
  IID_IMFPMediaPlayerCallback             : TGUID = '{766C8FFB-5FDB-4fea-A28D-B912996F51BD}';

  MFP_POSITIONTYPE_100NS                  : TGUID = '{00000000-0000-0000-0000-000000000000}';


  MFP_PKEY_StreamIndex            : PROPERTYKEY = (fmtid: (D1: $a7cf9740; D2: $e8d9;
		                                               D3: $4a87; D4: ($bd, $8e, $29, $67, $0, $1f, $d3, $ad));
                                                   pid: $0);

  MFP_PKEY_StreamRenderingResults : PROPERTYKEY = (fmtid: (D1: $a7cf9740; D2: $e8d9;
		                                               D3: $4a87; D4: ($bd, $8e, $29, $67, $0, $1f, $d3, $ad));
                                                   pid: $01);


type
  DWORD_PTR = ^DWord;

type
  {$EXTERNALSYM tagSIZE}
  tagSIZE = record
    cx: LONG;
    cy: LONG;
  end;
  {$EXTERNALSYM SIZE}
  SIZE = tagSIZE;
  {$EXTERNALSYM PSIZE}
  PSIZE = ^tagSIZE;

type
  {$EXTERNALSYM COLORREF}
  COLORREF = DWORD;
  {$EXTERNALSYM LPCOLORREF}
  LPCOLORREF = ^DWORD;

type
  {$EXTERNALSYM MFP_CREATION_OPTIONS}
  MFP_CREATION_OPTIONS = UINT32;
const
  MFP_OPTION_NONE                             : MFP_CREATION_OPTIONS = 0;
  MFP_OPTION_FREE_THREADED_CALLBACK           : MFP_CREATION_OPTIONS = $1;
  MFP_OPTION_NO_MMCSS                         : MFP_CREATION_OPTIONS = $2;
  MFP_OPTION_NO_REMOTE_DESKTOP_OPTIMIZATION   : MFP_CREATION_OPTIONS = $4;

type
  {$EXTERNALSYM MFP_MEDIAPLAYER_STATE}
  cwMFP_MEDIAPLAYER_STATE            = (
    MFP_MEDIAPLAYER_STATE_EMPTY    = 0,
    MFP_MEDIAPLAYER_STATE_STOPPED  = $1,
    MFP_MEDIAPLAYER_STATE_PLAYING  = $2,
    MFP_MEDIAPLAYER_STATE_PAUSED   = $3,
    MFP_MEDIAPLAYER_STATE_SHUTDOWN = $4
  );
  MFP_MEDIAPLAYER_STATE = cwMFP_MEDIAPLAYER_STATE;

type
  {$EXTERNALSYM MFP_MEDIAITEM_CHARACTERISTICS}
  MFP_MEDIAITEM_CHARACTERISTICS = UINT32;
const
  MFP_MEDIAITEM_IS_LIVE       : MFP_MEDIAITEM_CHARACTERISTICS = $1;
  MFP_MEDIAITEM_CAN_SEEK      : MFP_MEDIAITEM_CHARACTERISTICS = $2;
  MFP_MEDIAITEM_CAN_PAUSE     : MFP_MEDIAITEM_CHARACTERISTICS = $4;
  MFP_MEDIAITEM_HAS_SLOW_SEEK : MFP_MEDIAITEM_CHARACTERISTICS = $8;

type
  {$EXTERNALSYM MFP_CREDENTIAL_FLAGS}
  MFP_CREDENTIAL_FLAGS = UINT32;
const
  MFP_CREDENTIAL_PROMPT         : MFP_CREDENTIAL_FLAGS = $1;
  MFP_CREDENTIAL_SAVE           : MFP_CREDENTIAL_FLAGS = $2;
  MFP_CREDENTIAL_DO_NOT_CACHE   : MFP_CREDENTIAL_FLAGS = $4;
  MFP_CREDENTIAL_CLEAR_TEXT     : MFP_CREDENTIAL_FLAGS = $8;
  MFP_CREDENTIAL_PROXY          : MFP_CREDENTIAL_FLAGS = $10;
  MFP_CREDENTIAL_LOGGED_ON_USER : MFP_CREDENTIAL_FLAGS = $20;

type
  {$EXTERNALSYM MFP_EVENT_TYPE}
  cwMFP_EVENT_TYPE                           = (
    MFP_EVENT_TYPE_PLAY                    = 0,
    MFP_EVENT_TYPE_PAUSE                   = 1,
    MFP_EVENT_TYPE_STOP                    = 2,
    MFP_EVENT_TYPE_POSITION_SET            = 3,
    MFP_EVENT_TYPE_RATE_SET                = 4,
    MFP_EVENT_TYPE_MEDIAITEM_CREATED       = 5,
    MFP_EVENT_TYPE_MEDIAITEM_SET           = 6,
    MFP_EVENT_TYPE_FRAME_STEP              = 7,
    MFP_EVENT_TYPE_MEDIAITEM_CLEARED       = 8,
    MFP_EVENT_TYPE_MF                      = 9,
    MFP_EVENT_TYPE_ERROR                   = 10,
    MFP_EVENT_TYPE_PLAYBACK_ENDED          = 11,
    MFP_EVENT_TYPE_ACQUIRE_USER_CREDENTIAL = 12
  );
  MFP_EVENT_TYPE = cwMFP_EVENT_TYPE;

type
  {$EXTERNALSYM MFP_EVENT_HEADER}
  cwMFP_EVENT_HEADER = record
    eEventType: MFP_EVENT_TYPE;
    hrEvent: HResult;
    pMediaPlayer: PIMFPMediaPlayer;
    eState: MFP_MEDIAPLAYER_STATE;
    pPropertyStore: PIPropertyStore;
  end;
  MFP_EVENT_HEADER = cwMFP_EVENT_HEADER;

type
  {$EXTERNALSYM MFP_PLAY_EVENT}
  cwMFP_PLAY_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
  end;
  MFP_PLAY_EVENT = cwMFP_PLAY_EVENT;

type
  {$EXTERNALSYM MFP_PAUSE_EVENT}
  cwMFP_PAUSE_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
  end;
  MFP_PAUSE_EVENT = cwMFP_PAUSE_EVENT;

type
  {$EXTERNALSYM MFP_STOP_EVENT}
  cwMFP_STOP_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
  end;
  MFP_STOP_EVENT = cwMFP_STOP_EVENT;

type
  {$EXTERNALSYM MFP_POSITION_SET_EVENT}
  cwMFP_POSITION_SET_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
  end;
  MFP_POSITION_SET_EVENT = cwMFP_POSITION_SET_EVENT;

type
  {$EXTERNALSYM MFP_RATE_SET_EVENT}
  cwMFP_RATE_SET_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
    flRate: Single;
  end;
  MFP_RATE_SET_EVENT = cMFP_RATE_SET_EVENT;

type
  {$EXTERNALSYM MFP_MEDIAITEM_CREATED_EVENT}
  cwMFP_MEDIAITEM_CREATED_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
    dwUserData: DWORD_PTR;
  end;
  MFP_MEDIAITEM_CREATED_EVENT = cwMFP_MEDIAITEM_CREATED_EVENT;

type
  {$EXTERNALSYM MFP_MEDIAITEM_SET_EVENT}
  cwMFP_MEDIAITEM_SET_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
  end;
  MFP_MEDIAITEM_SET_EVENT = cwMFP_MEDIAITEM_SET_EVENT;

type
  {$EXTERNALSYM MFP_FRAME_STEP_EVENT}
  cwMFP_FRAME_STEP_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
  end;
  MFP_FRAME_STEP_EVENT = cwMFP_FRAME_STEP_EVENT;

type
  {$EXTERNALSYM MFP_MEDIAITEM_CLEARED_EVENT}
  cwMFP_MEDIAITEM_CLEARED_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
  end;
  MFP_MEDIAITEM_CLEARED_EVENT = cwMFP_MEDIAITEM_CLEARED_EVENT;

type
  {$EXTERNALSYM MFP_MF_EVENT}
  cwMFP_MF_EVENT = record
    header: MFP_EVENT_HEADER;
    MFEventType: MediaEventType;
    pMFMediaEvent: PIMFMediaEvent;
    pMediaItem: PIMFPMediaItem;
  end;
  MFP_MF_EVENT = cwMFP_MF_EVENT;

type
  {$EXTERNALSYM MFP_ERROR_EVENT}
  MFP_ERROR_EVENT = record
    header: MFP_EVENT_HEADER;
  end;
  TMfpErrorEvent = MFP_ERROR_EVENT;

type
  {$EXTERNALSYM MFP_PLAYBACK_ENDED_EVENT}
  cwMFP_PLAYBACK_ENDED_EVENT = record
    header: MFP_EVENT_HEADER;
    pMediaItem: PIMFPMediaItem;
  end;
  MFP_PLAYBACK_ENDED_EVENT = cwMFP_PLAYBACK_ENDED_EVENT;

type
  {$EXTERNALSYM MFP_ACQUIRE_USER_CREDENTIAL_EVENT}
  cwMFP_ACQUIRE_USER_CREDENTIAL_EVENT = record
    header: MFP_EVENT_HEADER;
    dwUserData: DWORD_PTR;
    fProceedWithAuthentication: BOOL;
    hrAuthenticationStatus: HResult;
    pwszURL: PWideChar;
    pwszSite: PWideChar;
    pwszRealm: PWideChar;
    pwszPackage: PWideChar;
    nRetries: LONG;
    flags: MFP_CREDENTIAL_FLAGS;
    pCredential: PIMFNetCredential;
  end;
  MFP_ACQUIRE_USER_CREDENTIAL_EVENT = cwMFP_ACQUIRE_USER_CREDENTIAL_EVENT;


type
  //Forward Interface Declarations

  {$EXTERNALSYM IMFPMediaPlayer}
  IMFPMediaPlayer = interface;
  {$EXTERNALSYM IMFPMediaItem}
  IMFPMediaItem = interface;
  {$EXTERNALSYM IMFPMediaPlayerCallback}
  IMFPMediaPlayerCallback = interface;


  //Interface IMFPMediaPlayer
  {
   NOTE: Deprecated.
   This API may be removed from future releases of Windows (> Windows 7).
   Applications should use the Media Session for playback.
   Contains methods to play media files.
   The MFPlay player object exposes this interface.
   To get a pointer to this interface, call MFPCreateMediaPlayer.
  }
  IMFPMediaPlayer = interface(IUnknown)
	['{A714590A-58AF-430a-85BF-44F5EC838D85}']
    function Play(): HResult; stdcall;
    function Pause(): HResult; stdcall;
    function Stop(): HResult; stdcall;
    function FrameStep(): HResult; stdcall;
    function SetPosition(const guidPositionType: REFGUID; const pvPositionValue: PROPVARIANT): HResult; stdcall;
    function GetPosition(const guidPositionType: REFGUID; out pvPositionValue: PROPVARIANT): HResult; stdcall;
    function GetDuration(const guidPositionType: REFGUID; out pvDurationValue: PROPVARIANT): HResult; stdcall;
    function SetRate(const flRate: Single): HResult; stdcall;
    function GetRate(out pflRate: Single): HResult; stdcall;
    function GetSupportedRates(const fForwardDirection: Boolean; out pflSlowestRate: Single; out pflFastestRate: Single): HResult; stdcall;
    function GetState(out peState: MFP_MEDIAPLAYER_STATE): HResult; stdcall;
    function CreateMediaItemFromURL(const pwszURL: LPCWSTR; const fSync: Boolean; const dwUserData: DWORD_PTR; out ppMediaItem: IMFPMediaItem): HResult; stdcall;
    function CreateMediaItemFromObject(const pIUnknownObj: IUnknown; const fSync: Boolean; const dwUserData: DWORD_PTR; out ppMediaItem: IMFPMediaItem): HResult; stdcall
    function SetMediaItem(const pIMFPMediaItem: IMFPMediaItem): HResult; stdcall;
    function ClearMediaItem(): HResult; stdcall;
    function GetMediaItem(out ppIMFPMediaItem: IMFPMediaItem): HResult; stdcall;
    function GetVolume(out pflVolume: Double): HResult; stdcall;
    function SetVolume(const flVolume: Double): HResult; stdcall;
    function GetBalance(out pflBalance: Double): HResult; stdcall;
    function SetBalance(const flBalance: Double): HResult; stdcall;
    function GetMute(out pfMute: CBOOL): HResult; stdcall; // Exception on boolean rule > must be 4 bit
    function SetMute(const fMute: CBOOL): HResult; stdcall; // Exception on boolean rule > must be 4 bit
    function GetNativeVideoSize(out pszVideo: SIZE; out pszARVideo: SIZE): HResult; stdcall;
    function GetIdealVideoSize(out pszMin: SIZE; out pszMax: SIZE): HResult; stdcall;
    function SetVideoSourceRect(const pnrcSource: MFVideoNormalizedRect): HResult; stdcall;
    function GetVideoSourceRect(out pnrcSource: MFVideoNormalizedRect): HResult; stdcall;
    function SetAspectRatioMode(const dwAspectRatioMode: DWord): HResult; stdcall;
    function GetAspectRatioMode(out pdwAspectRatioMode: DWord): HResult; stdcall;
    function GetVideoWindow(out phwndVideo: HWND): HResult; stdcall;
    function UpdateVideo(): HResult; stdcall;
    function SetBorderColor(const Clr: COLORREF): HResult; stdcall;
    function GetBorderColor(out pClr: COLORREF): HResult; stdcall;
    function InsertEffect(const pEffect: IUnknown; const fOptional: Boolean): HResult; stdcall;
    function RemoveEffect(const pEffect: IUnknown): HResult; stdcall;
    function RemoveAllEffects(): HResult; stdcall;
    function Shutdown(): HResult; stdcall;
  end;


  //Interface IMFPMediaItem
  {
   NOTE:  Deprecated.
   This API may be removed from future releases of Windows.
   Applications should use the Media Session for playback.
   Represents a media item. A media item is an abstraction for a source of media data, such as a video file.
   Use this interface to get information about the source, or to change certain playback settings,
   such as the start and stop times. To get a pointer to this interface, call one of the following methods:
      IMFPMediaPlayer.CreateMediaItemFromObject
      IMFPMediaPlayer.CreateMediaItemFromURL
  }
  IMFPMediaItem = interface(IUnknown)
	['{90EB3E6B-ECBF-45cc-B1DA-C6FE3EA70D57}']
    function GetMediaPlayer(out ppMediaPlayer: IMFPMediaPlayer): HResult; stdcall;
    function GetURL(out ppwszURL: LPWSTR): HResult; stdcall;
    function GetObject(out ppIUnknown: IUnknown): HResult; stdcall;
    function GetUserData(out pdwUserData: DWORD_PTR): HResult; stdcall;
    function SetUserData(const dwUserData: DWORD_PTR): HResult; stdcall;
    function GetStartStopPosition(out pguidStartPositionType: TGuid; out pvStartValue: PROPVARIANT;
                                  out pguidStopPositionType: TGuid; out pvStopValue: PROPVARIANT): HResult; stdcall;
    function SetStartStopPosition(const pguidStartPositionType: TGuid; const pvStartValue: PROPVARIANT;
                                  const pguidStopPositionType: TGuid; const pvStopValue: PROPVARIANT): HResult; stdcall;
    function HasVideo(out pfHasVideo: Boolean; out pfSelected: Boolean): HResult; stdcall;
    function HasAudio(out pfHasAudio: Boolean; out pfSelected: Boolean): HResult; stdcall;
    function IsProtected(out pfProtected: Boolean): HResult; stdcall;
    function GetDuration(const guidPositionType: REFGUID; pvDurationValue: PROPVARIANT): HResult; stdcall;
    function GetNumberOfStreams(out pdwStreamCount: DWord): HResult; stdcall;
    function GetStreamSelection(const dwStreamIndex: DWord; out pfEnabled: Boolean): HResult; stdcall;
    function SetStreamSelection(const dwStreamIndex: DWord; const fEnabled: Boolean): HResult; stdcall;
    function GetStreamAttribute(const dwStreamIndex: DWord; const guidMFAttribute: REFGUID; out pvValue: PROPVARIANT): HResult; stdcall;
    function GetPresentationAttribute(const guidMFAttribute: REFGUID; out pvValue: PROPVARIANT): HResult; stdcall;
    function GetCharacteristics(out pCharacteristics: MFP_MEDIAITEM_CHARACTERISTICS): HResult; stdcall;
    function SetStreamSink(const dwStreamIndex: DWord; const pMediaSink: IUnknown): HResult; stdcall;
    function GetMetadata(out ppMetadataStore: IPropertyStore): HResult; stdcall;
  end;


  //Interface IMFPMediaPlayerCallback
  {
   NOTE: Deprecated.
   This API may be removed from future releases of Windows (Windows 7).
   Applications should use the Media Session for playback.
   Callback interface for the IMFPMediaPlayer interface.
   To set the callback, pass an IMFPMediaPlayerCallback pointer to the MFPCreateMediaPlayer function in the pCallback parameter.
   The application implements the IMFPMediaPlayerCallback interface.
  }
  IMFPMediaPlayerCallback = interface(IUnknown)
	['{766C8FFB-5FDB-4fea-A28D-B912996F51BD}']
    function OnMediaPlayerEvent(const pEventHeader: MFP_EVENT_HEADER): HResult; stdcall;
  end;

  {
   NOTE: Deprecated.
   This API may be removed from future releases of Windows (Windows 7).
   Applications should use the Media Session for playback.
   Creates a new instance of the MFPlay player object.
  }
  function MFPCreateMediaPlayer(const pwszURL; LPCWSTR; const fStartPlayback: Boolean;
                                const creationOptions: MFP_CREATION_OPTIONS; const pCallback: IMFPMediaPlayerCallback;
                                const hWnd: HWND; out ppMediaPlayer: IMFPMediaPlayer): HResult; stdcall;

implementation

end.
