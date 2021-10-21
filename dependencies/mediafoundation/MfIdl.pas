// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - MfIdl.pas
// Kind: Pascal Unit
// Release date: 27-06-2012
// Language: ENU
//
// Version: 1.0
// Description: MfIdl <= Win 8
//
// LastEdited by: Tony
// EditDate: updt 270612a
//----------------------------------------------------------------------------
// Changes
//----------------------------------------------------------------------------
// Date
// ---------- ------------------ ---------------------------------------------
// 2012/07/08 Peter (ozships)    Initial corrections
// 2012/08/28 Peter (ozships)    Corrected IMFMediaSession, was named IAudioMeterInformation
//                               (which may be missing)
// 2012/12/19 Tony  (maXcomX)    modified some interfaces (Reported by eric.c.fortier, Oct 5, 2012)
//----------------------------------------------------------------------------
//
// Remarks:
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: Check some Guids (see: Todo comments)
// =============================================================================
// Source: MfIdl.h
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

unit MfIdl;

{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

{$IFDEF WINDOWS}

Uses
  Windows, ActiveX, MFObjects, mftransform,
  PropSys;

//## old    WinDef,

Const
  Version               = '0.1.0001';

{$i mfpack.inc}

const

//--------------------- Interface IIDs

  IID_IMFTopology                         : TGUID = '{83CF873A-F6DA-4bc8-823F-BACFD55DC433}';
  IID_IMFTopologyNode                     : TGUID = '{83CF873A-F6DA-4bc8-823F-BACFD55DC430}';
  IID_IMFMediaSession                     : TGUID = '{90377834-21D0-4dee-8214-BA2E3E6C1127}';
  IID_IMFClock                            : TGUID = '{2eb1e945-18b8-4139-9b1a-d5d584818530}';
  IID_IMFMediaSource                      : TGUID = '{279a808d-aec7-40c8-9c6b-a6b492c78a66}';
  IID_IMFPresentationDescriptor           : TGUID = '{03cb2711-24d7-4db6-a17f-f3a7a479a536}';
  IID_IMFStreamDescriptor                 : TGUID = '{56c03d9c-9dbb-45f5-ab4b-d80f47c05938}';
  IID_IMFMediaTypeHandler                 : TGUID = '{e93dcf6c-4b07-4e1e-8123-aa16ed6eadf5}';
  IID_IMFSourceResolver                   : TGUID = '{FBE5A32D-A497-4b61-BB85-97B1A848A6E3}';
  IID_IMFTopoLoader                       : TGUID = '{DE9A6157-F660-4643-B56A-DF9F7998C7CD}';
  IID_IMFMediaSink                        : TGUID = '{6ef2a660-47c0-4666-b13d-cbb717f2fa2c}';
  IID_IMFStreamSink                       : TGUID = '{0A97B3CF-8E7C-4a3d-8F8C-0C843DC247FB}';
  IID_IMFPresentationClock                : TGUID = '{868CE85C-8EA9-4f55-AB82-B009A910A805}';
  IID_IMFPresentationTimeSource           : TGUID = '{7FF12CCE-F76F-41c2-863B-1666C8E5E139}';
  IID_IMFClockStateSink                   : TGUID = '{F6696E82-74F7-4f3d-A178-8A5E09C3659F}';
  IID_IMFGetService                       : TGUID = '{fa993888-4383-415a-a930-dd472a8cf6f7}';
  IID_IMFMediaStream                      : TGUID = '{D182108F-4EC6-443f-AA42-A71106EC825F}';
  IID_IMFVideoSampleAllocator             : TGUID = '{86cbc910-e533-4751-8e3b-f19b5b806a03}';
  IID_IMFVideoSampleAllocatorNotify       : TGUID = '{A792CDBE-C374-4e89-8335-278E7B9956A4}';
  IID_IMFVideoSampleAllocatorCallback     : TGUID = '{992388B4-3372-4f67-8B6F-C84C071F4751}';
  IID_IMFTimer                            : TGUID = '{e56e4cbd-8f70-49d8-a0f8-edb3d6ab9bf2}';
  IID_IMFShutdown                         : TGUID = '{97ec2ea4-0e42-4937-97ac-9d6d328824e1}';
  IID_IMFContentProtectionManager         : TGUID = '{ACF92459-6A61-42bd-B57C-B43E51203CB0}';
  IID_IMFContentEnabler                   : TGUID = '{D3C4EF59-49CE-4381-9071-D5BCD044C770}';
  IID_IMFMetadata                         : TGUID = '{F88CFB8C-EF16-4991-B450-CB8C69E51704}';
  IID_IMFMetadataProvider                 : TGUID = '{56181D2D-E221-4adb-B1C8-3CEE6A53F76F}';
  IID_IMFRateSupport                      : TGUID = '{0a9ccdbc-d797-4563-9667-94ec5d79292d}';
  IID_IMFRateControl                      : TGUID = '{88ddcd21-03c3-4275-91ed-55ee3929328f}';
  IID_IMFTimecodeTranslate                : TGUID = '{ab9d8661-f7e8-4ef4-9861-89f334f94e74}';
  IID_IMFSimpleAudioVolume                : TGUID = '{089EDF13-CF71-4338-8D13-9E569DBDC319}';
  IID_IMFAudioStreamVolume                : TGUID = '{76B1BBDB-4EC8-4f36-B106-70A9316DF593}';
  IID_IMFAudioPolicy                      : TGUID = '{a0638c2b-6465-4395-9ae7-a321a9fd2856}';
  IID_IMFSampleGrabberSinkCallback        : TGUID = '{8C7B80BF-EE42-4b59-B1DF-55668E1BDCA8}';
  IID_IMFSampleGrabberSinkCallback2       : TGUID = '{ca86aa50-c46e-429e-ab27-16d6ac6844cb}';
  IID_IMFWorkQueueServices                : TGUID = '{35FE1BB8-A3A9-40fe-BBEC-EB569C9CCCA3}';
  IID_IMFQualityManager                   : TGUID = '{8D009D86-5B9F-4115-B1FC-9F80D52AB8AB}';
  IID_IMFQualityAdvise                    : TGUID = '{EC15E2E9-E36B-4f7c-8758-77D452EF4CE7}';
  IID_IMFQualityAdvise2                   : TGUID = '{F3706F0D-8EA2-4886-8000-7155E9EC2EAE}';
  IID_IMFQualityAdviseLimits              : TGUID = '{dfcd8e4d-30b5-4567-acaa-8eb5b7853dc9}';
  IID_IMFRealTimeClient                   : TGUID = '{2347D60B-3FB5-480c-8803-8DF3ADCD3EF0}';
  IID_IMFSequencerSource                  : TGUID = '{197CD219-19CB-4de1-A64C-ACF2EDCBE59E}';
  IID_IMFMediaSourceTopologyProvider      : TGUID = '{0E1D6009-C9F3-442d-8C51-A42D2D49452F}';
  IID_IMFMediaSourcePresentationProvider  : TGUID = '{0E1D600a-C9F3-442d-8C51-A42D2D49452F}';
  IID_IMFTopologyNodeAttributeEditor      : TGUID = '{676aa6dd-238a-410d-bb99-65668d01605a}';
  IID_IMFByteStreamBuffering              : TGUID = '{6d66d782-1d4f-4db7-8c63-cb8c77f1ef5e}';
  IID_IMFByteStreamCacheControl           : TGUID = '{F5042EA4-7A96-4a75-AA7B-2BE1EF7F88D5}';
  IID_IMFNetCredential                    : TGUID = '{5b87ef6a-7ed8-434f-ba0e-184fac1628d1}';
  IID_IMFNetCredentialManager             : TGUID = '{5b87ef6b-7ed8-434f-ba0e-184fac1628d1}';
  IID_IMFNetCredentialCache               : TGUID = '{5b87ef6c-7ed8-434f-ba0e-184fac1628d1}';
  IID_IMFSSLCertificateManager            : TGUID = '{61f7d887-1230-4a8b-aeba-8ad434d1a64d}';
  IID_IMFSourceOpenMonitor                : TGUID = '{059054B3-027C-494C-A27D-9113291CF87F}';
  IID_IMFNetProxyLocator                  : TGUID = '{e9cd0383-a268-4bb4-82de-658d53574d41}';
  IID_IMFNetProxyLocatorFactory           : TGUID = '{e9cd0384-a268-4bb4-82de-658d53574d41}';
  IID_IMFSaveJob                          : TGUID = '{e9931663-80bf-4c6e-98af-5dcf58747d1f}';
  IID_IMFNetSchemeHandlerConfig           : TGUID = '{7BE19E73-C9BF-468a-AC5A-A5E8653BEC87}';
  IID_IMFSchemeHandler                    : TGUID = '{6D4C7B74-52A0-4bb7-B0DB-55F29F47A668}';
  IID_IMFByteStreamHandler                : TGUID = '{BB420AA4-765B-4a1f-91FE-D6A8A143924C}';
  IID_IMFTrustedInput                     : TGUID = '{542612C4-A1B8-4632-B521-DE11EA64A0B0}';
  IID_IMFInputTrustAuthority              : TGUID = '{D19F8E98-B126-4446-890C-5DCB7AD71453}';
  IID_IMFTrustedOutput                    : TGUID = '{D19F8E95-B126-4446-890C-5DCB7AD71453}';
  IID_IMFOutputPolicy                     : TGUID = '{7F00F10A-DAED-41AF-AB26-5FDFA4DFBA3C}';
  IID_IMFOutputSchema                     : TGUID = '{7BE0FC5B-ABD9-44FB-A5C8-F50136E71599}';
  IID_IMFSecureChannel                    : TGUID = '{d0ae555d-3b12-4d97-b060-0990bc5aeb67}';
  IID_IMFSampleProtection                 : TGUID = '{8e36395f-c7b9-43c4-a54d-512b4af63c95}';
  IID_IMFFinalizableMediaSink             : TGUID = '{EAECB74A-9A50-42ce-9541-6A7F57AA4AD7}';
  IID_IMFRemoteProxy                      : TGUID = '{994e23ad-1cc2-493c-b9fa-46f1cb040fa4}';
  IID_IMFPMPHost                          : TGUID = '{F70CA1A9-FDC7-4782-B994-ADFFB1C98606}';
  IID_IMFPMPClient                        : TGUID = '{6C4E655D-EAD8-4421-B6B9-54DCDBBDF820}';
  IID_IMFPMPServer                        : TGUID = '{994e23af-1cc2-493c-b9fa-46f1cb040fa4}';
  IID_IMFSAMIStyle                        : TGUID = '{A7E025DD-5303-4a62-89D6-E747E1EFAC73}';
  IID_IMFTranscodeProfile                 : TGUID = '{4ADFDBA3-7AB0-4953-A62B-461E7FF3DA1E}';
  IID_IMFTranscodeSinkInfoProvider        : TGUID = '{8CFFCD2E-5A03-4a3a-AFF7-EDCD107C620E}';
  IID_IMFFieldOfUseMFTUnlock              : TGUID = '{508E71D3-EC66-4fc3-8775-B4B9ED6BA847}';

//--------------------- Topology GUIDs

  MF_TOPOLOGY_PROJECTSTART                      : TGUID = '{7ed3f802-86bb-4b3f-b7e4-7cb43afd4b80}';
  MF_TOPOLOGY_PROJECTSTOP                       : TGUID = '{7ed3f803-86bb-4b3f-b7e4-7cb43afd4b80}';
  MF_TOPOLOGY_NO_MARKIN_MARKOUT                 : TGUID = '{7ed3f804-86bb-4b3f-b7e4-7cb43afd4b80}';
  MF_TOPOLOGY_DXVA_MODE                         : TGUID = '{1e8d34f6-f5ab-4e23-bb88-874aa3a1a74d}';
  MF_TOPOLOGY_STATIC_PLAYBACK_OPTIMIZATIONS     : TGUID = '{b86cac42-f5ab-4e23-bb88-874aa3a1a74d}';
  MF_TOPOLOGY_PLAYBACK_MAX_DIMS                 : TGUID = '{5715cf19-5768-44aa-ad6e-8721f1b0f9bb}';
  MF_TOPOLOGY_HARDWARE_MODE                     : TGUID = '{d2d362fd-4e4f-4191-a579-c618b66706af}';
  MF_TOPOLOGY_PLAYBACK_FRAMERATE                : TGUID = '{c164737a-c2b1-4553-83bb-5a526072448f}';
  MF_TOPOLOGY_DYNAMIC_CHANGE_NOT_ALLOWED        : TGUID = '{d529950b-d484-4527-a9cd-b1909532b5b0}';
  MF_TOPOLOGY_ENUMERATE_SOURCE_TYPES            : TGUID = '{6248c36d-5d0b-4f40-a0bb-b0b305f77698}';
  MF_TOPOLOGY_START_TIME_ON_PRESENTATION_SWITCH : TGUID = '{c8cc113f-7951-4548-aad6-9ed6202e62b3}';
  MF_TOPONODE_FLUSH                             : TGUID = '{494bbce8-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_DRAIN                             : TGUID = '{494bbce9-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_D3DAWARE                          : TGUID = '{494bbced-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPOLOGY_RESOLUTION_STATUS                 : TGUID = '{494bbcde-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_ERRORCODE                         : TGUID = '{494bbcee-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_CONNECT_METHOD                    : TGUID = '{494bbcf1-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_LOCKED                            : TGUID = '{494bbcf7-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_WORKQUEUE_ID                      : TGUID = '{494bbcf8-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_WORKQUEUE_MMCSS_CLASS             : TGUID = '{494bbcf9-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_DECRYPTOR                         : TGUID = '{494bbcfa-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_DISCARDABLE                       : TGUID = '{494bbcfb-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_ERROR_MAJORTYPE                   : TGUID = '{494bbcfd-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_ERROR_SUBTYPE                     : TGUID = '{494bbcfe-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_WORKQUEUE_MMCSS_TASKID            : TGUID = '{494bbcff-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_MARKIN_HERE                       : TGUID = '{494bbd00-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_MARKOUT_HERE                      : TGUID = '{494bbd01-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_DECODER                           : TGUID = '{494bbd02-b031-4e38-97c4-d5422dd618dc}';
  MF_TOPONODE_MEDIASTART                        : TGUID = '{835c58ea-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_MEDIASTOP                         : TGUID = '{835c58eb-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_SOURCE                            : TGUID = '{835c58ec-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_PRESENTATION_DESCRIPTOR           : TGUID = '{835c58ed-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_STREAM_DESCRIPTOR                 : TGUID = '{835c58ee-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_SEQUENCE_ELEMENTID                : TGUID = '{835c58ef-e075-4bc7-bcba-4de000df9ae6}';
  MF_TOPONODE_TRANSFORM_OBJECTID                : TGUID = '{88dcc0c9-293e-4e8b-9aeb-0ad64cc016b0}';
  MF_TOPONODE_STREAMID                          : TGUID = '{14932f9b-9087-4bb4-8412-5167145cbe04}';
  MF_TOPONODE_NOSHUTDOWN_ON_REMOVE              : TGUID = '{14932f9c-9087-4bb4-8412-5167145cbe04}';
  MF_TOPONODE_RATELESS                          : TGUID = '{14932f9d-9087-4bb4-8412-5167145cbe04}';
  MF_TOPONODE_DISABLE_PREROLL                   : TGUID = '{14932f9e-9087-4bb4-8412-5167145cbe04}';
  MF_TOPONODE_PRIMARYOUTPUT                     : TGUID = '{6304ef99-16b2-4ebe-9d67-e4c539b3a259}';
  MF_SESSION_TOPOLOADER                         : TGUID = '{1e83d482-1f1c-4571-8405-88f4b2181f71}';
  MF_SESSION_GLOBAL_TIME                        : TGUID = '{1e83d482-1f1c-4571-8405-88f4b2181f72}';
  MF_SESSION_QUALITY_MANAGER                    : TGUID = '{1e83d482-1f1c-4571-8405-88f4b2181f73}';
  MF_SESSION_CONTENT_PROTECTION_MANAGER         : TGUID = '{1e83d482-1f1c-4571-8405-88f4b2181f74}';
  MF_SESSION_SERVER_CONTEXT                     : TGUID = '{afe5b291-50fa-46e8-b9be-0c0c3ce4b3a5}';
  MF_SESSION_REMOTE_SOURCE_MODE                 : TGUID = '{f4033ef4-9bb3-4378-941f-85a0856bc244}';
  MF_SESSION_APPROX_EVENT_OCCURRENCE_TIME       : TGUID = '{190e852f-6238-42d1-b5af-69ea338ef850}';
  MF_PMP_SERVER_CONTEXT                         : TGUID = '{2f00c910-d2cf-4278-8b6a-d077fac3a25f}';

//--------------------- Presentation Descriptor GUIDs
  MF_PD_PMPHOST_CONTEXT                         : TGUID = '{6c990d31-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_APP_CONTEXT                             : TGUID = '{6c990d32-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_DURATION                                : TGUID = '{6c990d33-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_TOTAL_FILE_SIZE                         : TGUID = '{6c990d34-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_AUDIO_ENCODING_BITRATE                  : TGUID = '{6c990d35-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_VIDEO_ENCODING_BITRATE                  : TGUID = '{6c990d36-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_MIME_TYPE                               : TGUID = '{6c990d37-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_LAST_MODIFIED_TIME                      : TGUID = '{6c990d38-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_PLAYBACK_ELEMENT_ID                     : TGUID = '{6c990d39-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_PREFERRED_LANGUAGE                      : TGUID = '{6c990d3A-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_PLAYBACK_BOUNDARY_TIME                  : TGUID = '{6c990d3b-bb8e-477a-8598-0d5d96fcd88a}';
  MF_PD_AUDIO_ISVARIABLEBITRATE                 : TGUID = '{33026ee0-e387-4582-ae0a-34a2ad3baa18}';

//--------------------- Stream Descriptor GUIDs
  MF_SD_LANGUAGE                                : TGUID = '{00af2180-bdc2-423c-abca-f503593bc121}';
  MF_SD_PROTECTED                               : TGUID = '{00af2181-bdc2-423c-abca-f503593bc121}';
  MF_SD_STREAM_NAME                             : TGUID = '{4f1b099d-d314-41e5-a781-7fefaa4c501f}';
  MF_SD_MUTUALLY_EXCLUSIVE                      : TGUID = '{023ef79c-388d-487f-ac17-696cd6e3c6f5}';

//--------------------- Audio renderer GUIDs
  MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS             : TGUID = '{ede4b5e0-f805-4d6c-99b3-db01bf95dfab}';
  MF_AUDIO_RENDERER_ATTRIBUTE_SESSION_ID        : TGUID = '{ede4b5e3-f805-4d6c-99b3-db01bf95dfab}';
  MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ID       : TGUID = '{b10aaec3-ef71-4cc3-b873-05a9a08b9f8e}';
  MF_AUDIO_RENDERER_ATTRIBUTE_ENDPOINT_ROLE     : TGUID = '{6ba644ff-27c5-4d02-9887-c28619fdb91b}';

  MFNETSOURCE_STATISTICS_SERVICE                 : TGUID = '{3cb1f275-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_STATISTICS                         : TGUID = '{3cb1f274-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_BUFFERINGTIME                      : TGUID = '{3cb1f276-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_ACCELERATEDSTREAMINGDURATION       : TGUID = '{3cb1f277-0505-0c5d-ae71-0a556344efa1}';
  MFNETSOURCE_MAXUDPACCELERATEDSTREAMINGDURATION : TGUID = '{4aab2879-bbe1-4994-9ff0-5495bd250129}';
  MFNETSOURCE_MAXBUFFERTIMEMS                    : TGUID = '{408b24e6-4038-4401-b5b2-fe701a9ebf10}';
  MFNETSOURCE_CONNECTIONBANDWIDTH                : TGUID = '{3cb1f278-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_CACHEENABLED                       : TGUID = '{3cb1f279-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_AUTORECONNECTLIMIT                 : TGUID = '{3cb1f27a-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_RESENDSENABLED                     : TGUID = '{3cb1f27b-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_THINNINGENABLED                    : TGUID = '{3cb1f27c-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PROTOCOL                           : TGUID = '{3cb1f27d-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_TRANSPORT                          : TGUID = '{3cb1f27e-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PREVIEWMODEENABLED                 : TGUID = '{3cb1f27f-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_CREDENTIAL_MANAGER                 : TGUID = '{3cb1f280-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PPBANDWIDTH                        : TGUID = '{3cb1f281-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_AUTORECONNECTPROGRESS              : TGUID = '{3cb1f282-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PROXYLOCATORFACTORY                : TGUID = '{3cb1f283-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_BROWSERUSERAGENT                   : TGUID = '{3cb1f28b-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_BROWSERWEBPAGE                     : TGUID = '{3cb1f28c-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PLAYERVERSION                      : TGUID = '{3cb1f28d-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PLAYERID                           : TGUID = '{3cb1f28e-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_HOSTEXE                            : TGUID = '{3cb1f28f-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_HOSTVERSION                        : TGUID = '{3cb1f291-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PLAYERUSERAGENT                    : TGUID = '{3cb1f292-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_CLIENTGUID                         : TGUID = '{60a2c4a6-f197-4c14-a5bf-88830d2458af}';
  MFNETSOURCE_LOGURL                             : TGUID = '{3cb1f293-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_ENABLE_UDP                         : TGUID = '{3cb1f294-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_ENABLE_TCP                         : TGUID = '{3cb1f295-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_ENABLE_MSB                         : TGUID = '{3cb1f296-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_ENABLE_RTSP                        : TGUID = '{3cb1f298-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_ENABLE_HTTP                        : TGUID = '{3cb1f299-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_ENABLE_STREAMING                   : TGUID = '{3cb1f290-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_ENABLE_DOWNLOAD                    : TGUID = '{3cb1f29d-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_UDP_PORT_RANGE                     : TGUID = '{3cb1f29a-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PROXYINFO                          : TGUID = '{3cb1f29b-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_DRMNET_LICENSE_REPRESENTATION      : TGUID = '{47eae1bd-bdfe-42e2-82f3-54a48c17962d}';
  MFNETSOURCE_PROXYSETTINGS                      : TGUID = '{3cb1f287-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PROXYHOSTNAME                      : TGUID = '{3cb1f284-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PROXYPORT                          : TGUID = '{3cb1f288-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PROXYEXCEPTIONLIST                 : TGUID = '{3cb1f285-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PROXYBYPASSFORLOCAL                : TGUID = '{3cb1f286-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_PROXYRERUNAUTODETECTION            : TGUID = '{3cb1f289-0505-4c5d-ae71-0a556344efa1}';
  MFNETSOURCE_STREAM_LANGUAGE                    : TGUID = '{9ab44318-f7cd-4f2d-8d6d-fa35b492cecb}';
  MFNETSOURCE_LOGPARAMS                          : TGUID = '{64936ae8-9418-453a-8cda-3e0a668b353b}';

  MFCONNECTOR_SPDIF                              : TGUID = '{0b94a712-ad3e-4cee-83ce-ce32e3db6522}';
  MFCONNECTOR_UNKNOWN                            : TGUID = '{ac3aef5c-ce43-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_PCI                                : TGUID = '{ac3aef5d-ce43-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_PCIX                               : TGUID = '{ac3aef5e-ce43-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_PCI_Express                        : TGUID = '{ac3aef5f-ce43-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_AGP                                : TGUID = '{ac3aef60-ce43-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_VGA                                : TGUID = '{57cd5968-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_SVIDEO                             : TGUID = '{57cd5969-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_COMPOSITE                          : TGUID = '{57cd596a-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_COMPONENT                          : TGUID = '{57cd596b-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_DVI                                : TGUID = '{57cd596c-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_HDMI                               : TGUID = '{57cd596d-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_LVDS                               : TGUID = '{57cd596e-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_D_JPN                              : TGUID = '{57cd5970-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_SDI                                : TGUID = '{57cd5971-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_DISPLAYPORT_EXTERNAL               : TGUID = '{57cd5972-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_DISPLAYPORT_EMBEDDED               : TGUID = '{57cd5973-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_UDI_EXTERNAL                       : TGUID = '{57cd5974-ce47-11d9-92db-000bdb28ff98}';
  MFCONNECTOR_UDI_EMBEDDED                       : TGUID = '{57cd5975-ce47-11d9-92db-000bdb28ff98}';

  MF_RATE_CONTROL_SERVICE                        : TGUID = '{866fa297-b802-4bf8-9dc9-5e3b6a9f53c9}';
  MF_METADATA_PROVIDER_SERVICE                   : TGUID = '{db214084-58a4-4d2e-b84f-6f755b2f7a0d}';
  MF_PROPERTY_HANDLER_SERVICE                    : TGUID = '{a3face02-32b8-41dd-90e7-5fef7c8991b5}';
  MF_TIMECODE_SERVICE                            : TGUID = '{a0d502a7-0eb3-4885-b1b9-9feb0d083454}';
  MR_STREAM_VOLUME_SERVICE                       : TGUID = '{f8b5fa2f-32ef-46f5-b172-1321212fb2c4}';
  MR_AUDIO_POLICY_SERVICE                        : TGUID = '{911fd737-6775-4ab0-a614-297862fdac88}';
  MR_POLICY_VOLUME_SERVICE                       : TGUID = '{1abaa2ac-9d3b-47c6-ab48-c59506de784d}';
  MF_SAMPLEGRABBERSINK_SAMPLE_TIME_OFFSET        : TGUID = '{62e3d776-8100-4e03-a6e8-bd3857ac9c47}';
  MF_SAMPLEGRABBERSINK_IGNORE_CLOCK              : TGUID = '{0efda2c0-0b69-4e2e-ab8d-46dcbff7d25d}';
  MF_QUALITY_SERVICES                            : TGUID = '{b7e2be11-2f96-4640-b52c-282365bdf16c}';
  MF_WORKQUEUE_SERVICES                          : TGUID = '{8e37d489-41e0-413a-9068-287c886d8dda}';

//MF_RESOLUTION
  MF_RESOLUTION_MEDIASOURCE	= $1;
	MF_RESOLUTION_BYTESTREAM	= $2;
	MF_RESOLUTION_CONTENT_DOES_NOT_HAVE_TO_MATCH_EXTENSION_OR_MIME_TYPE	= $10;
	MF_RESOLUTION_KEEP_BYTE_STREAM_ALIVE_ON_FAIL	= $20;
	MF_RESOLUTION_READ = $10000;
	MF_RESOLUTION_WRITE	= $20000;


type

  {$EXTERNALSYM TOPOID}
  TOPOID = UInt64;
  PTopoid = ^TOPOID;
  {$EXTERNALSYM LONG}
  LONG = Cardinal;
  {$EXTERNALSYM LONGLONG}
  LONGLONG = Int64;
  {$EXTERNALSYM MFTIME}
  MFTIME = LONGLONG;                        // Time in 100 nanosecond slices

type
  MFMEDIASOURCE_CHARACTERISTICS = DWORD;

const
  MFMEDIASOURCE_IS_LIVE                    = $1;
  MFMEDIASOURCE_CAN_SEEK                   = $2;
  MFMEDIASOURCE_CAN_PAUSE                  = $4;
  MFMEDIASOURCE_HAS_SLOW_SEEK              = $8;
  MFMEDIASOURCE_HAS_MULTIPLE_PRESENTATIONS = $10;
  MFMEDIASOURCE_CAN_SKIPFORWARD            = $20;
  MFMEDIASOURCE_CAN_SKIPBACKWARD           = $40;

//type
//{$EXTERNALSYM _MFMEDIASOURCE_CHARACTERISTICS}
//  _MFMEDIASOURCE_CHARACTERISTICS             = (
//    MFMEDIASOURCE_IS_LIVE                    = $1,
//    MFMEDIASOURCE_CAN_SEEK                   = $2,
//    MFMEDIASOURCE_CAN_PAUSE                  = $4,
//    MFMEDIASOURCE_HAS_SLOW_SEEK              = $8,
//    MFMEDIASOURCE_HAS_MULTIPLE_PRESENTATIONS = $10,
//    MFMEDIASOURCE_CAN_SKIPFORWARD            = $20,
//    MFMEDIASOURCE_CAN_SKIPBACKWARD           = $40
//  );
//  {$EXTERNALSYM MFMEDIASOURCE_CHARACTERISTICS}
//  MFMEDIASOURCE_CHARACTERISTICS = _MFMEDIASOURCE_CHARACTERISTICS;

  MFPMPSESSION_UNPROTECTED_PROCESS = $1;
//  {$EXTERNALSYM MFPMPSESSION_CREATION_FLAGS}
//  cwMFPMPSESSION_CREATION_FLAGS      = (
//    MFPMPSESSION_UNPROTECTED_PROCESS = $1
//  );
//  MFPMPSESSION_CREATION_FLAGS = cwMFPMPSESSION_CREATION_FLAGS;


type
  _MFSHUTDOWN_STATUS = (
     	MFSHUTDOWN_INITIATED	= 0,
     	MFSHUTDOWN_COMPLETED	= 1
    );
 	MFSHUTDOWN_STATUS=_MFSHUTDOWN_STATUS;

//--------------------- Forward interface deninitions ------------------------
type
  {$EXTERNALSYM IMFTopology}
  IMFTopology = interface;
  {$EXTERNALSYM IMFTopologyNode}
  IMFTopologyNode = interface;
  {$EXTERNALSYM IMFMediaSession}
  IMFMediaSession = interface;
  {$EXTERNALSYM IMFClock}
  IMFClock = interface;
  {$EXTERNALSYM IMFMediaSource}
  IMFMediaSource = interface;
  {$EXTERNALSYM IMFPresentationDescriptor}
  IMFPresentationDescriptor = interface;
  {$EXTERNALSYM IMFStreamDescriptor}
  IMFStreamDescriptor = interface;
  {$EXTERNALSYM IMFMediaTypeHandler}
  IMFMediaTypeHandler = interface;
  {$EXTERNALSYM IMFSourceResolver}
  IMFSourceResolver = interface;
  {$EXTERNALSYM IMFTopoLoader}
  IMFTopoLoader = interface;
  {$EXTERNALSYM IMFMediaSink}
  IMFMediaSink = interface;
  {$EXTERNALSYM IMFStreamSink}
  IMFStreamSink = interface;
  {$EXTERNALSYM IMFPresentationClock}
  IMFPresentationClock = interface;
  {$EXTERNALSYM IMFPresentationTimeSource}
  IMFPresentationTimeSource = interface;
  {$EXTERNALSYM IMFClockStateSink}
  IMFClockStateSink = interface;
  {$EXTERNALSYM IMFGetService}
  IMFGetService = interface;
  {$EXTERNALSYM IMFRateSupport}
  IMFRateSupport = interface;
  {$EXTERNALSYM IMFRateControl}
  IMFRateControl = interface;
 {$EXTERNALSYM IMFSimpleAudioVolume}
  IMFSimpleAudioVolume = interface;
  {$EXTERNALSYM IMFAudioStreamVolume}
  IMFAudioStreamVolume = interface;
  {$EXTERNALSYM IMFAudioPolicy}
  IMFAudioPolicy = interface;
  {$EXTERNALSYM IMFVideoSampleAllocator}
  IMFVideoSampleAllocator = interface;
  {$EXTERNALSYM IMFVideoSampleAllocatorCallback}
  IMFVideoSampleAllocatorCallback = interface;
  {$EXTERNALSYM IMFVideoSampleAllocatorNotify}
  IMFVideoSampleAllocatorNotify = interface;


//--------------------- Types and records ------------------------------------

  {$EXTERNALSYM MF_TOPOLOGY_TYPE}
  cwMF_TOPOLOGY_TYPE              = (
    MF_TOPOLOGY_OUTPUT_NODE       = 0,
    MF_TOPOLOGY_SOURCESTREAM_NODE = 1,
    MF_TOPOLOGY_TRANSFORM_NODE    = 2,
    MF_TOPOLOGY_TEE_NODE          = 3,
    MF_TOPOLOGY_MAX               = $7fffffff
  );
  MF_TOPOLOGY_TYPE = cwMF_TOPOLOGY_TYPE;

  {$EXTERNALSYM _MFCLOCK_STATE}
  _MFCLOCK_STATE          = (
    MFCLOCK_STATE_INVALID = 0,
    MFCLOCK_STATE_RUNNING = 1,
    MFCLOCK_STATE_STOPPED = 2,
    MFCLOCK_STATE_PAUSED  = 3
  );
  {$EXTERNALSYM MF_CLOCK_STATE}
  MF_CLOCK_STATE = _MFCLOCK_STATE;

  {$EXTERNALSYM _MFCLOCK_PROPERTIES}
  _MFCLOCK_PROPERTIES = record
    qwCorrelationRate: UInt64;
    guidClockId: TGUID;
    dwClockFlags: DWORD;
    qwClockFrequency: UInt64;
    dwClockTolerance: DWORD;
    dwClockJitter: DWORD;
  end;
  {$EXTERNALSYM MFCLOCK_PROPERTIES}
  MFCLOCK_PROPERTIES = _MFCLOCK_PROPERTIES;

  {$EXTERNALSYM MF_OBJECT_TYPE}
  cwMF_OBJECT_TYPE          = (
    MF_OBJECT_MEDIASOURCE = 0,
    MF_OBJECT_BYTESTREAM  = 1,
    MF_OBJECT_INVALID     = 2
  );
  MF_OBJECT_TYPE = cwMF_OBJECT_TYPE;

  //MFSTREAMSINK
  {$EXTERNALSYM _MFSTREAMSINK_MARKER_TYPE}
  _MFSTREAMSINK_MARKER_TYPE          = (
    MFSTREAMSINK_MARKER_DEFAULT      = 0,
    MFSTREAMSINK_MARKER_ENDOFSEGMENT = 1,
    MFSTREAMSINK_MARKER_TICK         = 2,
    MFSTREAMSINK_MARKER_EVENT        = 3
  );
  {$EXTERNALSYM MFSTREAMSINK_MARKER_TYPE}
  MFSTREAMSINK_MARKER_TYPE = _MFSTREAMSINK_MARKER_TYPE;

  {$EXTERNALSYM _MFRATE_DIRECTION}
  _MFRATE_DIRECTION = (
    MFRATE_FORWARD = 0,
    MFRATE_REVERSE = 1
  );
  {$EXTERNALSYM MFRATE_DIRECTION}
  MFRATE_DIRECTION = _MFRATE_DIRECTION;

//--------------------- Interfaces -------------------------------------------

  //Interface IMFTopology
  IMFTopology = interface(IMFAttributes) ['{83CF873A-F6DA-4bc8-823F-BACFD55DC433}']
    function GetTopologyID(out pID: TOPOID): HResult; stdcall;
    function AddNode(const pNode: IMFTopologyNode): HResult; stdcall;
    function RemoveNode(const pNode: IMFTopologyNode): HResult; stdcall;
    function GetNodeCount(out pwNodes: Word): HResult; stdcall;
    function GetNode(const wIndex: Word; out ppNode: IMFTopologyNode): HResult; stdcall;
    function Clear(): HResult; stdcall;
    function CloneFrom(const pTopology: IMFTopology): HResult; stdcall;
    function GetNodeByID(const qwTopoNodeID: TOPOID; out ppNode: IMFTopologyNode): HResult; stdcall;
    function GetSourceNodeCollection(out ppCollection: IMFCollection): HResult; stdcall;
    function GetOutputNodeCollection(out ppCollection: IMFCollection): HResult; stdcall;
  end;

  //Interface IMFTopologyNode
  IMFTopologyNode = interface(IMFAttributes) ['{83CF873A-F6DA-4bc8-823F-BACFD55DC430}']
    function SetObject(const pObject: IUnknown): HResult; stdcall;
    function GetObject(out ppObject: IUnknown): HResult; stdcall;
    function GetNodeType(out pType: MF_TOPOLOGY_TYPE): HResult; stdcall;
    function GetTopoNodeID(out pID: TOPOID): HResult; stdcall;
    function SetTopoNodeID(const ullTopoID: TOPOID): HResult; stdcall;
    function GetInputCount(out pcInputs: DWord): HResult; stdcall;
    function GetOutputCount(out pcOutputs: DWord): HResult; stdcall;
    function ConnectOutput(const dwOutputIndex: DWord; const pDownstreamNode: IMFTopologyNode; const dwInputIndexOnDownstreamNode: DWord): HResult; stdcall;
    function DisconnectOutput(const dwOutputIndex: DWord): HResult; stdcall;
    function GetInput(const dwInputIndex: DWord; out ppUpstreamNode: IMFTopologyNode; out pdwOutputIndexOnUpstreamNode: DWord): HResult; stdcall;
    function GetOutput(const dwOutputIndex : DWord; out ppDownstreamNode: IMFTopologyNode; out pdwInputIndexOnDownstreamNode: DWord): HResult; stdcall;
    function SetOutputPrefType(const dwOutputIndex: DWord; const pType: IMFMediaType): HResult; stdcall;
    function GetOutputPrefType(const dwOutputIndex: DWord; out ppType: IMFMediaType): HResult; stdcall;
    function SetInputPrefType(const dwInputIndex: DWord; const pType: IMFMediaType): HResult; stdcall;
    function GetInputPrefType(const dwInputIndex: DWord; out ppType: IMFMediaType): HResult; stdcall;
    function CloneFrom(const pNode: IMFTopologyNode): HResult; stdcall;
  end;

  //Interface IMFMediaSession
  IMFMediaSession = interface(IMFMediaEventGenerator) ['{90377834-21D0-4dee-8214-BA2E3E6C1127}']
    function SetTopology(const dwSetTopologyFlags: Dword; const pTopology: IMFTopology): HResult; stdcall;
    function ClearTopologies(): HResult; stdcall;
    function Start(const pguidTimeFormat: TGUID; const pvarStartPosition: PROPVARIANT): HResult; stdcall;
    function Pause(): HResult; stdcall;
    function Stop(): HResult; stdcall;
    function Close(): HResult; stdcall;
    function Shutdown(): HResult; stdcall;
    function GetClock(out ppClock: IMFClock): HResult; stdcall;
    function GetSessionCapabilities(out pdwCaps: DWord): HResult; stdcall;
    function GetFullTopology(const dwGetFullTopologyFlags: DWord; const TopoId: TOPOID; out ppFullTopology: IMFTopology): HResult; stdcall;
  end;

//Interface IMFClock
  IMFClock = interface(IUnknown) ['{2eb1e945-18b8-4139-9b1a-d5d584818530}']
    function GetClockCharacteristics(out pdwCharacteristics: DWord): HResult; stdcall;
    function GetCorrelatedTime(const dwReserved: DWord; out pllClockTime: LongLong; out phnsSystemTime: MFTIME): HResult; stdcall;
    function GetContinuityKey(out pdwContinuityKey: Dword): HResult; stdcall;
    function GetState(const dwReserved: DWord; out peClockState: MF_CLOCK_STATE): HResult; stdcall;
    function GetProperties(out pClockProperties: MFCLOCK_PROPERTIES): HResult; stdcall;
  end;

  //Interface IMFMediaSource
  IMFMediaSource  = interface(IMFMediaEventGenerator) ['{90377834-21D0-4dee-8214-BA2E3E6C1127}']
    function GetCharacteristics(out pdwCharacteristics: DWord): HResult; stdcall;
    function CreatePresentationDescriptor(out ppPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall;
    function Start(const pPresentationDescriptor: IMFPresentationDescriptor; const pguidTimeFormat: TGuid; pvarStartPosition: PROPVARIANT): HResult; stdcall;
    function Stop(): HResult; stdcall;
    function Pause(): HResult; stdcall;
    function Shutdown(): HResult; stdcall;
  end;

  //Interface IMFPresentationDescriptor */
  IMFPresentationDescriptor = interface(IMFAttributes) ['{03cb2711-24d7-4db6-a17f-f3a7a479a536}']
    function GetStreamDescriptorCount(out pdwDescriptorCount: DWord): HResult; stdcall;
    function GetStreamDescriptorByIndex(const dwIndex: Dword; out pfSelected: Bool; out ppDescriptor: IMFStreamDescriptor): HResult; stdcall;
    function SelectStream(const dwDescriptorIndex: DWord): HResult; stdcall;
    function DeselectStream(const dwDescriptorIndex: DWord): HResult; stdcall;
    function Clone(out ppPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall;
  end;

  //Interface IMFStreamDescriptor
  IMFStreamDescriptor = interface(IMFAttributes) ['{56c03d9c-9dbb-45f5-ab4b-d80f47c05938}']
    function GetStreamIdentifier(out pdwStreamIdentifier: DWord): HResult; stdcall;
    function GetMediaTypeHandler(out ppMediaTypeHandler: IMFMediaTypeHandler): HResult; stdcall;
  end;

  //Interface IMFMediaTypeHandler
  IMFMediaTypeHandler = interface(IUnknown) ['{e93dcf6c-4b07-4e1e-8123-aa16ed6eadf5}']
    function IsMediaTypeSupported(const pMediaType: IMFMediaType; out ppMediaType: IMFMediaType): HResult; stdcall;
    function GetMediaTypeCount(out pdwTypeCount: DWord): HResult; stdcall;
    function GetMediaTypeByIndex(const dwIndex: DWord; out ppType: IMFMediaType): HResult; stdcall;
    function SetCurrentMediaType(const pMediaType: IMFMediaType): HResult; stdcall;
    function GetCurrentMediaType(out ppMediaType: IMFMediaType): HResult; stdcall;
    function GetMajorType(out pguidMajorType: TGuid): HResult; stdcall;
  end;

  //Interface IMFSourceResolver
  IMFSourceResolver = interface(IUnknown) ['{90377834-21D0-4dee-8214-BA2E3E6C1127}']
    function CreateObjectFromURL(const pwszURLL: LPCWSTR; const dwFlags: DWord;
                                 const pProps: IPropertyStore; out pObjectType: MF_OBJECT_TYPE;
                                 out ppObject: IUnknown): HResult; stdcall;
    function CreateObjectFromByteStream(const pByteStream: IMFByteStream; const pwszURL: LPCWSTR;
                                        const dwFlags: DWord; const pProps: IPropertyStore;
                                        out pObjectType: MF_OBJECT_TYPE; out ppObject: IUnknown): HResult; stdcall;
    function BeginCreateObjectFromURL(const pwszURL: LPCWSTR; const dwFlags: DWord;
                                      const pProps: IPropertyStore; out ppIUnknownCancelCookie: IUnknown;
                                      const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndCreateObjectFromURL(const pResult: IMFAsyncResult; out pObjectType: MF_OBJECT_TYPE; ppObject: IUnknown): HResult; stdcall;
    function BeginCreateObjectFromByteStream(const pByteStream: IMFByteStream; const pwszURL: LPCWSTR;
                                             const dwFlags: DWord; const pProps: IPropertyStore;
                                             out ppIUnknownCancelCookie: IUnknown; const pCallback: IMFAsyncCallback;
                                             const punkState: IUnknown): HResult; stdcall;
    function EndCreateObjectFromByteStream(const pResult: IMFAsyncResult; out pObjectType: MF_OBJECT_TYPE;
                                           out ppObject: IUnknown): HResult; stdcall;
    function CancelObjectCreation(const pIUnknownCancelCookie: IUnknown): HResult; stdcall;
  end;

  //Interface IMFTopoLoader
  IMFTopoLoader = interface(IUnknown) ['{DE9A6157-F660-4643-B56A-DF9F7998C7CD}']
    function Load(const pInputTopo: IMFTopology; out ppOutputTopo: IMFTopology; const pCurrentTopo: IMFTopology): HResult; stdcall;
  end;

  //Interface IMFMediaSink
  IMFMediaSink = interface(IUnknown) ['{6ef2a660-47c0-4666-b13d-cbb717f2fa2c}']
    function GetCharacteristics(out pdwCharacteristics: DWord): HResult; stdcall;
    function AddStreamSink(const dwStreamSinkIdentifier: DWord; const pMediaType: IMFMediaType; out ppStreamSink: IMFStreamSink): HResult; stdcall;
    function RemoveStreamSink(const dwStreamSinkIdentifier: DWord): HResult; stdcall;
    function GetStreamSinkCount(out pcStreamSinkCount: DWord): HResult; stdcall;
    function GetStreamSinkByIndex(const dwIndex: DWord; out ppStreamSink: IMFStreamSink): HResult; stdcall;
    function GetStreamSinkById(const dwStreamSinkIdentifier: DWord; out ppStreamSink: IMFStreamSink): HResult; stdcall;
    function SetPresentationClock(const pPresentationClock: IMFPresentationClock): HResult; stdcall;
    function GetPresentationClock(out ppPresentationClock: IMFPresentationClock): HResult; stdcall;
    function Shutdown: HResult; stdcall;
  end;

  //Interface IMFStreamSink
  IMFStreamSink = interface(IUnknown) ['{0A97B3CF-8E7C-4a3d-8F8C-0C843DC247FB}']
    function GetMediaSink(out ppMediaSink: IMFMediaSink): HResult; stdcall;
    function GetIdentifier(out pdwIdentifier: DWord): HResult; stdcall;
    function GetMediaTypeHandler(out ppHandler: IMFMediaTypeHandler): HResult; stdcall;
    function ProcessSample(const pSample: IMFSample): HResult; stdcall;
    function PlaceMarker(const eMarkerType: MFSTREAMSINK_MARKER_TYPE; const pvarMarkerValue: PROPVARIANT; const pvarContextValue: PROPVARIANT): HResult; stdcall;
    function Flush(): HResult; stdcall;
  end;

  //Interface IMFPresentationClock
  IMFPresentationClock = interface(IUnknown) ['{868CE85C-8EA9-4f55-AB82-B009A910A805}']
    function SetTimeSource(const pTimeSource: IMFPresentationTimeSource): HResult; stdcall;
    function GetTimeSource(out ppTimeSource: IMFPresentationTimeSource): HResult; stdcall;
    function GetTime(out phnsClockTime: MFTIME): HResult; stdcall;
    function AddClockStateSink(const pStateSink: IMFClockStateSink): HResult; stdcall;
    function RemoveClockStateSink(const pStateSink: IMFClockStateSink): HResult; stdcall;
    function Start(const llClockStartOffset: LongLong): HResult; stdcall;
    function Stop(): HResult; stdcall;
    function Pause(): HResult; stdcall;
  end;

  //Interface IMFPresentationTimeSource
  IMFPresentationTimeSource = interface(IUnknown) ['{7FF12CCE-F76F-41c2-863B-1666C8E5E139}']
    function GetUnderlyingClock(out ppClock: IMFClock): HResult; stdcall;
  end;

  //Interface IMFClockStateSink
  IMFClockStateSink = interface(IUnknown) ['{F6696E82-74F7-4f3d-A178-8A5E09C3659F}']
    function OnClockStart(const hnsSystemTime: MFTIME; const llClockStartOffset: LongLong): HResult; stdcall;
    function OnClockStop(const hnsSystemTime: MFTIME): HResult; stdcall;
    function OnClockPause(const hnsSystemTime: MFTIME): HResult; stdcall;
    function OnClockRestart(const hnsSystemTime: MFTIME): HResult; stdcall;
    function OnClockSetRate(const hnsSystemTime: MFTIME; const flRate: Single): HResult; stdcall;
  end;

  //Interface IMFGetService
  IMFGetService = interface(IUnknown) ['{83CF873A-F6DA-4bc8-823F-BACFD55DC430}']
    function GetService(const  guidService: tGUID; const riid: tGUID; ppvObject: IUnknown): HResult; stdcall;
//    function GetService(const  guidService: REFGUID; const riid: REFIID; ppvObject: Pointer): HResult; stdcall;
  end;

  //Interface IMFRateSupport
  IMFRateSupport = interface(IUnknown) ['{0a9ccdbc-d797-4563-9667-94ec5d79292d}']
    function GetSlowestRate(const eDirection: MFRATE_DIRECTION; const fThin: Bool; out pflRate: Single): HResult; stdcall;
    function GetFastestRate(const eDirection: MFRATE_DIRECTION; const fThin: Bool; out pflRate: Single): HResult; stdcall;
    function IsRateSupported(const fThin: Bool; const flRate: Single; var pflNearestSupportedRate: Single): HResult; stdcall;
  end;

//--------------------- IMFRateControl
  IMFRateControl = interface(IUnknown) ['{88ddcd21-03c3-4275-91ed-55ee3929328f}']
    function SetRate(const fThin: BOOL; const flRate: Single): HRESULT; stdcall;
    function GetRate(var fThin: BOOL; var flRate: Single): HRESULT; stdcall;
  end;

  //Interface IMFSimpleAudioVolume
  IMFSimpleAudioVolume = interface(IUnknown) ['{089EDF13-CF71-4338-8D13-9E569DBDC319}']
    function SetMasterVolume(const fLevel: Single): HResult; stdcall;
    function GetMasterVolume(out pfLevel: Single): HResult; stdcall;
    function SetMute(const bMute: Bool): HResult; stdcall;   //todo: Convert to CBool
    function GetMute(out pbMute: Bool): HResult; stdcall;    //todo: Convert to CBool
  end;

  //Interface IMFAudioStreamVolume
  IMFAudioStreamVolume = interface(IUnknown) ['{089EDF13-CF71-4338-8D13-9E569DBDC319}']
    function GetChannelCount(out pdwCount: UINT32): HResult; stdcall;
    function SetChannelVolume(const dwIndex: UINT32; const fLevel: Single): HResult; stdcall;
    function GetChannelVolume(const dwIndex: UINT32; out pfLevel: Single): HResult; stdcall;
    function SetAllVolumes(const dwCount: UINT32; const pfVolumes: Single): HResult; stdcall;
    function GetAllVolumes(const dwCount: UINT32; out pfVolumes: Single): HResult; stdcall;
  end;

  //Interface IMFAudioPolicy
  IMFAudioPolicy = interface(IUnknown) ['{a0638c2b-6465-4395-9ae7-a321a9fd2856}']
    function SetGroupingParam(const rguidClass: REFGUID): HResult; stdcall;
    function GetGroupingParam(out pguidClass: TGuid): HResult; stdcall;
    function SetDisplayName(const pszName: LPCWSTR): HResult; stdcall;
    function GetDisplayName(out pszName: LPWSTR): HResult; stdcall;
    function SetIconPath(const pszPath: LPCWSTR): HResult; stdcall;
    function GetIconPath(out pszPath: LPWSTR): HResult; stdcall;
  end;

  //Interface IMFVideoSampleAllocator
  IMFVideoSampleAllocator = interface(IUnknown) ['{86cbc910-e533-4751-8e3b-f19b5b806a03}']
    function SetDirectXManager(const pManager: IUnknown): HResult; stdcall;
    function UninitializeSampleAllocator(): HResult; stdcall;
    function InitializeSampleAllocator(const cRequestedFrames: DWord; const pMediaType: IMFMediaType): HResult; stdcall;
    function AllocateSample(out ppSample: IMFSample): HResult; stdcall;
  end;

  //Interface IMFVideoSampleAllocatorCallback
  IMFVideoSampleAllocatorCallback = interface(IUnknown) ['{992388B4-3372-4f67-8B6F-C84C071F4751}']
    function SetCallback(const pNotify: IMFVideoSampleAllocatorNotify): HResult; stdcall;
    function GetFreeSampleCount(out plSamples: LONG): HResult; stdcall;
  end;

  //Interface IMFVideoSampleAllocatorNotify
  IMFVideoSampleAllocatorNotify = interface(IUnknown)['{A792CDBE-C374-4e89-8335-278E7B9956A4}']
    function NotifyRelease(): HResult; stdcall;
  end;

const






  MFPKEY_SourceOpenMonitor: PROPERTYKEY = (fmtid: (D1: $074d4637; D2: $b5ae;
		                                       D3: $465d; D4: ($af, $17, $1a, $53, $8d, $28, $59, $dd));
                                           pid: $02);
  MFPKEY_ASFMediaSource_ApproxSeek: PROPERTYKEY = (fmtid: (D1: $b4cd270f; D2: $244d;
	                                                 D3: $4969; D4: ($bb, $92, $3f, $0f, $b8, $31, $6f, $10));
                                                   pid: $01);

  // >= Windows 7
  MFPKEY_ASFMediaSource_IterativeSeekIfNoIndex: PROPERTYKEY = (fmtid: (D1: $170b65dc; D2: $4a4e;
	                                                             D3: $407a; D4: ($ac, $22, $57, $7f, $50, $e4, $a3, $7c));
                                                               pid: $01);
  // >= Windows 7
  MFPKEY_ASFMediaSource_IterativeSeek_Max_Count: PROPERTYKEY = (fmtid: (D1: $170b65dc; D2: $4a4e;
	                                       D3: $407a; D4: ($ac, $22, $57, $7f, $50, $e4, $a3, $7c));
                                         pid: $02);
  // >= Windows 7
  MFPKEY_ASFMediaSource_IterativeSeek_Tolerance_In_MilliSecond: PROPERTYKEY = (fmtid: (D1: $170b65dc; D2: $4a4e;
	                                       D3: $407a; D4: ($ac, $22, $57, $7f, $50, $e4, $a3, $7c));
                                         pid: $03);
  // >= Windows 7
  MFPKEY_Content_DLNA_Profile_ID: PROPERTYKEY = (fmtid: (D1: $cfa31b45; D2: $525d;
	                                       D3: $4998; D4: ($bb, $44, $3f, $7d, $81, $54, $2f, $a4));
                                         pid: $01);
  // >= Windows 7
  MFPKEY_MediaSource_DisableReadAhead: PROPERTYKEY = (fmtid: (D1: $26366c14; D2: $c5bf;
	                                       D3: $4c76; D4: ($88, $7b, $9f, $17, $54, $db, $5f, $9));
                                         pid: $01);


  // >= Windows 7
  MF_TIME_FORMAT_ENTRY_RELATIVE           : TGUID =  '{4399f178-46d3-4504-afda-20d32e9ba360}';

  //MEDIASINK
  {$EXTERNALSYM MEDIASINK_FIXED_STREAMS}
  MEDIASINK_FIXED_STREAMS               = $00000001;
  {$EXTERNALSYM MEDIASINK_CANNOT_MATCH_CLOCK}
  MEDIASINK_CANNOT_MATCH_CLOCK          = $00000002;
  {$EXTERNALSYM MEDIASINK_RATELESS}
  MEDIASINK_RATELESS                    = $00000004;
  {$EXTERNALSYM MEDIASINK_CLOCK_REQUIRED}
  MEDIASINK_CLOCK_REQUIRED              = $00000008;
  {$EXTERNALSYM MEDIASINK_CAN_PREROLL}
  MEDIASINK_CAN_PREROLL                 = $00000010;
  {$EXTERNALSYM MEDIASINK_REQUIRE_REFERENCE_MEDIATYPE}
  MEDIASINK_REQUIRE_REFERENCE_MEDIATYPE = $00000016;



  {$EXTERNALSYM MFCLOCK_FREQUENCY_HNS}
  MFCLOCK_FREQUENCY_HNS               = 10000000;
  {$EXTERNALSYM MFCLOCK_TOLERANCE_UNKNOWN}
  MFCLOCK_TOLERANCE_UNKNOWN           = 50000;
  {$EXTERNALSYM MFCLOCK_JITTER_ISR}
  MFCLOCK_JITTER_ISR                  = 1000;
  {$EXTERNALSYM MFCLOCK_JITTER_DPC}
  MFCLOCK_JITTER_DPC                  = 4000;
  {$EXTERNALSYM MFCLOCK_JITTER_PASSIVE}
  MFCLOCK_JITTER_PASSIVE              = 10000;




  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_CROSSPROCESS}
  MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_CROSSPROCESS = $1;
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_NOPERSIST}
  MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_NOPERSIST = $2;
  {$EXTERNALSYM MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_DONT_ALLOW_FORMAT_CHANGES}
  MF_AUDIO_RENDERER_ATTRIBUTE_FLAGS_DONT_ALLOW_FORMAT_CHANGES = $4;

  MFENABLETYPE_WMDRMV1_LicenseAcquisition       : TGUID = '{4ff6eeaf-0b43-4797-9b85-abf31815e7b0}';
  MFENABLETYPE_WMDRMV7_LicenseAcquisition       : TGUID = '{003306df-4a06-4884-a097-ef6d22ec84a3}';
  MFENABLETYPE_WMDRMV7_Individualization        : TGUID = '{acd2c84a-b303-4f65-bc2c-2c848d01a989}';
  MFENABLETYPE_MF_UpdateRevocationInformation   : TGUID = '{e558b0b5-b3c4-44a0-924c-50d178932385}';
  MFENABLETYPE_MF_UpdateUntrustedComponent      : TGUID = '{9879f3d6-cee2-48e6-b573-9767ab172f16}';
  MFENABLETYPE_MF_RebootRequired                : TGUID = '{6d4d3d4b-0ece-4652-8b3a-f2d24260d887}';

  //Structs that contain information about revoked or unsigned binaries,
  //returned by the IMFContentEnabler.GetEnableData() method of
  //the Revocation content enabler

  //The values for MFRR_COMPONENT_HASH_INFO.ulReason

  {$EXTERNALSYM MF_USER_MODE_COMPONENT_LOAD}
  MF_USER_MODE_COMPONENT_LOAD         = $1;
  {$EXTERNALSYM MF_KERNEL_MODE_COMPONENT_LOAD}
  MF_KERNEL_MODE_COMPONENT_LOAD       = $2;
  {$EXTERNALSYM MF_GRL_LOAD_FAILED}
  MF_GRL_LOAD_FAILED                  = $8;
  {$EXTERNALSYM MF_INVALID_GRL_SIGNATURE}
  MF_INVALID_GRL_SIGNATURE            = $16;
  {$EXTERNALSYM MF_GRL_ABSENT}
  MF_GRL_ABSENT                       = $512;
  {$EXTERNALSYM MF_COMPONENT_REVOKED}
  MF_COMPONENT_REVOKED                = $1024;
  {$EXTERNALSYM MF_COMPONENT_INVALID_EKU}
  MF_COMPONENT_INVALID_EKU            = $2048;
  {$EXTERNALSYM MF_COMPONENT_CERT_REVOKED}
  MF_COMPONENT_CERT_REVOKED           = $08000;   //todo; check
  {$EXTERNALSYM MF_COMPONENT_INVALID_ROOT}
  MF_COMPONENT_INVALID_ROOT           = $4096;
  {$EXTERNALSYM MF_COMPONENT_HS_CERT_REVOKED}
  MF_COMPONENT_HS_CERT_REVOKED        = $8192;
  {$EXTERNALSYM MF_COMPONENT_LS_CERT_REVOKED}
  MF_COMPONENT_LS_CERT_REVOKED        = $16384;
  {$EXTERNALSYM MF_BOOT_DRIVER_VERIFICATION_FAILED}
  MF_BOOT_DRIVER_VERIFICATION_FAILED  = $32768;
  {$EXTERNALSYM MF_TEST_SIGNED_COMPONENT_LOADING}
  MF_TEST_SIGNED_COMPONENT_LOADING    = $262144;
  {$EXTERNALSYM MF_MINCRYPT_FAILURE}
  MF_MINCRYPT_FAILURE                 = $10000000;

  //STR_HASH_LEN: Number of characters required to represent a SHA-1 hash
  //(RTL_MAX_HASH_LEN_V1) as a string of the form "$5a3b53463b672a4f..."
  //Each byte of a SHA-1 hash takes two characters to represent, and
  //we add in two leading characters "$" as well as the NULL terminator

  {$EXTERNALSYM SHA_HASH_LEN}
  SHA_HASH_LEN                        = 20;
  {$EXTERNALSYM STR_HASH_LEN}
  STR_HASH_LEN                        = (SHA_HASH_LEN * 2 + 3);

  {$EXTERNALSYM MFSEQUENCER_INVALID_ELEMENT_ID}
  MFSEQUENCER_INVALID_ELEMENT_ID = $ffffffff;

  //
  MF_QUALITY_NOTIFY_PROCESSING_LATENCY            : TGUID = '{f6b44af8-604d-46fe-a95d-45479b10c9bc}';
  MF_QUALITY_NOTIFY_SAMPLE_LAG                    : TGUID = '{30d15206-ed2a-4760-be17-eb4a9f12295c}';
  MF_TIME_FORMAT_SEGMENT_OFFSET                   : TGUID = '{c8b8be77-869c-431d-812e-169693f65a39}';
  MF_SOURCE_PRESENTATION_PROVIDER_SERVICE         : TGUID = '{e002aadc-f4af-4ee5-9847-053edf840426}';
  MF_TOPONODE_ATTRIBUTE_EDITOR_SERVICE            : TGUID = '{65656e1a-077f-4472-83ef-316f11d5087a}';
  MFNET_SAVEJOB_SERVICE                           : TGUID = '{b85a587f-3d02-4e52-9565-55d3ec1e7ff7}';
  MFNETSOURCE_SSLCERTIFICATE_MANAGER              : TGUID = '{55e6cb27-e69b-4267-940c-2d7ec5bb8a0f}';
  MF_BYTESTREAMHANDLER_ACCEPTS_SHARE_WRITE        : TGUID = '{a6e1f733-3001-4915-8150-1558a2180ec8}';


  // [local]
  // MFPROTECTION_DISABLE schema data
  //
  //  Schema data is a 32 bit value defined as follows:
  //
  //   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
  //   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
  //  +-------------------------------------------------------------+-+
  //  |                         Reserved                            |S|
  //  +-------------------------------------------------------------+-+
  //
  //  where
  //
  //      Reserved - must be zero
  //
  //      S - Status
  //          0 = off, i.e. do not disable
  //          1 = on, i.e. disable
  //
  //  The MAKE macro is useful to ITA policy objects for building the DWORD
  //  schema data. Note it does not validate the parameters.
  //
  //  The EXTRACT macros are normally useful only to implementors of this type of
  //  protection for breaking out parameters from the DWORD schema data.
  //  Extracting the Reserved data is useful only to verify that it is zero.
  //
  //  Source:  T. Forsberg, tforsberg@gmail.com
  //

  MFPROTECTION_DISABLE                            : TGUID = '{8cc6d81b-fec6-4d8f-964b-cfba0b0dad0d}';
  MFPROTECTION_CONSTRICTVIDEO                     : TGUID = '{193370ce-c5e4-4c3a-8a66-6959b4da4442}';
  MFPROTECTION_CONSTRICTAUDIO                     : TGUID = '{ffc99b44-df48-4e16-8e66-096892c1578a}';
  MFPROTECTION_TRUSTEDAUDIODRIVERS                : TGUID = '{65bdf3d2-0168-4816-a533-55d47b027101}';
  MFPROTECTION_HDCP                               : TGUID = '{AE7CC03D-C828-4021-acb7-d578d27aaf13}';
  MFPROTECTION_CGMSA                              : TGUID = '{E57E69E9-226B-4d31-B4E3-D3DB008736DD}';
  MFPROTECTION_ACP                                : TGUID = '{c3fd11c6-f8b7-4d20-b008-1db17d61f2da}';
  MFPROTECTION_WMDRMOTA                           : TGUID = '{a267a6a1-362e-47d0-8805-4628598a23e4}';
  MFPROTECTION_FFT                                : TGUID = '{462a56b2-2866-4bb6-980d-6d8d9edb1a8c}';
  MFPROTECTIONATTRIBUTE_CONSTRICTVIDEO_IMAGESIZE  : TGUID = '{008476fc-4b58-4d80-a790-e7297673161d}';
  MFPROTECTIONATTRIBUTE_HDCP_SRM                  : TGUID = '{6f302107-3477-4468-8a08-eef9db10e20f}';
  MF_SampleProtectionSalt                         : TGUID = '{5403deee-b9ee-438f-aa83-3804997e569d}';
//  MF_PMP_SERVICE: TGuid;
  MF_SAMI_SERVICE                                 : TGUID = '{49a89ae7-b4d9-4ef2-aa5c-f65a3e05ae4e}';
  MF_PD_SAMI_STYLELIST                            : TGUID = '{e0b73c7f-486d-484e-9872-4de5192a7bf8}';
  MF_SD_SAMI_LANGUAGE                             : TGUID = '{36fcb98a-6cd0-44cb-acb9-a8f5600dd0bb}';

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  MF_TRANSCODE_CONTAINERTYPE                      : TGUID = '{150ff23f-4abc-478b-ac4f-e1916fba1cca}';
  MFTranscodeContainerType_ASF                    : TGUID = '{430f6f6e-b6bf-4fc1-a0bd-9ee46eee2afb}';
  MFTranscodeContainerType_MPEG4                  : TGUID = '{dc6cd05d-b9d0-40ef-bd35-fa622c1ab28a}';
  MFTranscodeContainerType_MP3                    : TGUID = '{e438b912-83f1-4de6-9e3a-9ffbc6dd24d1}';
  MFTranscodeContainerType_3GP                    : TGUID = '{34c50167-4472-4f34-9ea0-c49fbacf037d}';
  MF_TRANSCODE_SKIP_METADATA_TRANSFER             : TGUID = '{4e4469ef-b571-4959-8f83-3dcfba33a393}';
  MF_TRANSCODE_TOPOLOGYMODE                       : TGUID = '{3e3df610-394a-40b2-9dea-3bab650bebf2}';

  MF_TRANSCODE_ADJUST_PROFILE                     : TGUID = '{9c37c21b-060f-487c-a690-80d7f50d1c72}';

  MF_TRANSCODE_ENCODINGPROFILE                    : TGUID = '{6947787c-f508-4ea9-b1e9-a1fe3a49fbc9}';
  MF_TRANSCODE_QUALITYVSSPEED                     : TGUID = '{98332df8-03cd-476b-89fa-3f9e442dec9f}';
  MF_TRANSCODE_DONOT_INSERT_ENCODER               : TGUID = '{f45aa7ce-ab24-4012-a11b-dc8220201410}';

  MF_LOCAL_MFT_REGISTRATION_SERVICE               : TGUID = '{ddf5cf9c-4506-45aa-abf0-6d5d94dd1b4a}';

  CLSID_UrlmonSchemePlugin                        : TGUID = '{9ec4b4f9-3029-45ad-947b-344de2a249e2}';

  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE                        : TGUID = '{c60ac5fe-252a-478f-a0ef-bc8fa5f7cad3}';
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_HW_SOURCE       : TGUID = '{de7046ba-54d6-4487-a2a4-ec7c0d1bd163}';
  MF_DEVSOURCE_ATTRIBUTE_FRIENDLY_NAME                      : TGUID = '{60d0e559-52f8-4fa2-bbce-acdb34a8ec01}';
  MF_DEVSOURCE_ATTRIBUTE_MEDIA_TYPE                         : TGUID = '{56a819ca-0c78-4de4-a0a7-3ddaba0f24d4}';
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_CATEGORY        : TGUID = '{77f0ae69-c3bd-4509-941d-467e4d24899e}';
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_SYMBOLIC_LINK   : TGUID = '{58f0aad8-22bf-4f8a-bb3d-d2c4978c6e2f}';
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_MAX_BUFFERS     : TGUID = '{7dd9b730-4f2d-41d5-8f95-0cc9a912ba26}';
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ENDPOINT_ID     : TGUID = '{30da9258-feb9-47a7-a453-763a7a8e1c5f}';
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_ROLE            : TGUID = '{bc9d118e-8c67-4a18-85d4-12d300400552}';
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_AUDCAP_GUID            : TGUID = '{14dd9a1c-7cff-41be-b1b9-ba1ac6ecb571}';
  MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID            : TGUID = '{8ac3587a-4ae7-42d8-99e0-0a6013eef90f}';
  MFSampleExtension_DeviceTimestamp                         : TGUID = '{8f3e35e7-2dcd-4887-8622-2a58baa652b0}';
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

type
  {$EXTERNALSYM MFSESSION_SETTOPOLOGY_FLAGS}
  cwMFSESSION_SETTOPOLOGY_FLAGS         = (
    MFSESSION_SETTOPOLOGY_IMMEDIATE     = $1,
    MFSESSION_SETTOPOLOGY_NORESOLUTION  = $2,
    MFSESSION_SETTOPOLOGY_CLEAR_CURRENT = $4
  );
  MFSESSION_SETTOPOLOGY_FLAGS = cwMFSESSION_SETTOPOLOGY_FLAGS;
type
  {$EXTERNALSYM MFSESSION_GETFULLTOPOLOGY_FLAGS}
  cwMFSESSION_GETFULLTOPOLOGY_FLAGS   = (
    MFSESSION_GETFULLTOPOLOGY_CURRENT = $1
  );
  MFSESSION_GETFULLTOPOLOGY_FLAGS = cwMFSESSION_GETFULLTOPOLOGY_FLAGS;

type
  {$EXTERNALSYM _MF_CONNECT_METHOD}
  _MF_CONNECT_METHOD                           = (
    MF_CONNECT_DIRECT                          = 0,
    MF_CONNECT_ALLOW_CONVERTER                 = $1,
    MF_CONNECT_ALLOW_DECODER                   = $3,
    MF_CONNECT_RESOLVE_INDEPENDENT_OUTPUTTYPES = $4,
    MF_CONNECT_AS_OPTIONAL                     = $10000,
    MF_CONNECT_AS_OPTIONAL_BRANCH              = $20000
  );
  {$EXTERNALSYM MF_CONNECT_METHOD}
  MF_CONNECT_METHOD = _MF_CONNECT_METHOD;

type
  {$EXTERNALSYM _MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS}
  _MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS          = (
    MF_TOPOLOGY_RESOLUTION_SUCCEEDED            = 0,
    MF_OPTIONAL_NODE_REJECTED_MEDIA_TYPE        = $1,
    MF_OPTIONAL_NODE_REJECTED_PROTECTED_PROCESS = $2
  );
  {$EXTERNALSYM MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS}
  MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS = _MF_TOPOLOGY_RESOLUTION_STATUS_FLAGS;


type
  // >= Windows 7
  {$EXTERNALSYM MFTOPOLOGY_DXVA_MODE}
  cwMFTOPOLOGY_DXVA_MODE      = (
    MFTOPOLOGY_DXVA_DEFAULT = 0,
    MFTOPOLOGY_DXVA_NONE    = 1,
    MFTOPOLOGY_DXVA_FULL    = 2
  );
  MFTOPOLOGY_DXVA_MODE = cwMFTOPOLOGY_DXVA_MODE;

type
  // >= Windows 7
  {$EXTERNALSYM MFTOPOLOGY_HARDWARE_MODE}
  cwMFTOPOLOGY_HARDWARE_MODE          = (
    MFTOPOLOGY_HWMODE_SOFTWARE_ONLY = 0,
    MFTOPOLOGY_HWMODE_USE_HARDWARE  = 1
  );
  MFTOPOLOGY_HARDWARE_MODE = cwMFTOPOLOGY_HARDWARE_MODE;

type
{$EXTERNALSYM _MF_TOPONODE_FLUSH_MODE}
  _MF_TOPONODE_FLUSH_MODE    = (
    MF_TOPONODE_FLUSH_ALWAYS = 0,
    MF_TOPONODE_FLUSH_SEEK   = 1,
    MF_TOPONODE_FLUSH_NEVER  = 2
  );
  {$EXTERNALSYM MF_TOPONODE_FLUSH_MODE}
  MF_TOPONODE_FLUSH_MODE = _MF_TOPONODE_FLUSH_MODE;

type
  {$EXTERNALSYM _MF_TOPONODE_DRAIN_MODE}
  _MF_TOPONODE_DRAIN_MODE     = (
    MF_TOPONODE_DRAIN_DEFAULT = 0,
    MF_TOPONODE_DRAIN_ALWAYS  = 1,
    MF_TOPONODE_DRAIN_NEVER   = 2
  );
  {$EXTERNALSYM MF_TOPONODE_DRAIN_MODE}
  MF_TOPONODE_DRAIN_MODE = _MF_TOPONODE_DRAIN_MODE;


type
  {$EXTERNALSYM _MFCLOCK_CHARACTERISTICS_FLAGS}
  _MFCLOCK_CHARACTERISTICS_FLAGS                 = (
    MFCLOCK_CHARACTERISTICS_FLAG_FREQUENCY_10MHZ = $2,
    MFCLOCK_CHARACTERISTICS_FLAG_ALWAYS_RUNNING  = $4,
    MFCLOCK_CHARACTERISTICS_FLAG_IS_SYSTEM_CLOCK = $8
  );
  {$EXTERNALSYM MFCLOCK_CHARACTERISTICS_FLAGS}
  MFCLOCK_CHARACTERISTICS_FLAGS = _MFCLOCK_CHARACTERISTICS_FLAGS;


type
  {$EXTERNALSYM _MFCLOCK_RELATIONAL_FLAGS}
  _MFCLOCK_RELATIONAL_FLAGS                    = (
    MFCLOCK_RELATIONAL_FLAG_JITTER_NEVER_AHEAD = $1
  );
  {$EXTERNALSYM MFCLOCK_RELATIONAL_FLAGS}
  MFCLOCK_RELATIONAL_FLAGS = _MFCLOCK_RELATIONAL_FLAGS;
  TMfclockRelationalFlags = _MFCLOCK_RELATIONAL_FLAGS;

type
  {$EXTERNALSYM MFTIMER_FLAGS}
  cwMFTIMER_FLAGS    = (
    MFTIMER_RELATIVE = $1
  );
  MFTIMER_FLAGS = cwMFTIMER_FLAGS;

type
  cwMF_URL_TRUST_STATUS = (
    MF_LICENSE_URL_UNTRUSTED = 0,
    MF_LICENSE_URL_TRUSTED = 1,
    MF_LICENSE_URL_TAMPERED = 2
  );
  {$EXTERNALSYM MF_URL_TRUST_STATUS}
  MF_URL_TRUST_STATUS = cwMF_URL_TRUST_STATUS;

type
  {$EXTERNALSYM _MFRR_COMPONENT_HASH_INFO}
  _MFRR_COMPONENT_HASH_INFO = record
    //Reason for failure (revoked or unsigned or badly signed).
    ulReason: DWORD;
    //Header hash of the component
    rgHeaderHash: array[0..STR_HASH_LEN - 1] of WideChar;
    //Hash of public key if one of the certificates
    //in the signing certificate chain is revoked
    rgPublicKeyHash: array[0..STR_HASH_LEN - 1] of WideChar;
    //Component name (full path name)
    wszName: array[0..MAX_PATH - 1] of WideChar;
  end;
  {$EXTERNALSYM MFRR_COMPONENT_HASH_INFO}
  MFRR_COMPONENT_HASH_INFO = _MFRR_COMPONENT_HASH_INFO;
  {$EXTERNALSYM PMFRR_COMPONENT_HASH_INFO}
  PMFRR_COMPONENT_HASH_INFO = ^_MFRR_COMPONENT_HASH_INFO;

type
  {$EXTERNALSYM _MFRR_COMPONENTS}
  _MFRR_COMPONENTS = record
    // Version number
    dwRRInfoVersion: DWORD;
    // Number of components in list
    dwRRComponents: DWORD;
    // points to the end of this structure that has
    // allocated memory for the array of component info structures
    pRRComponents: PMFRR_COMPONENT_HASH_INFO;
  end;
  {$EXTERNALSYM MFRR_COMPONENTS}
  MFRR_COMPONENTS = _MFRR_COMPONENTS;
  {$EXTERNALSYM PMFRR_COMPONENTS}
  PMFRR_COMPONENTS = ^_MFRR_COMPONENTS;

type
  {$EXTERNALSYM _ASFFlatPicture}
  _ASFFlatPicture = record
    //Direct mapped fields
    bPictureType: Byte;
    dwDataLen: DWord;
  end;
  {$EXTERNALSYM ASF_FLAT_PICTURE}
  ASF_FLAT_PICTURE = _ASFFlatPicture;

type
  {$EXTERNALSYM _ASFFlatSynchronisedLyrics}
  _ASFFlatSynchronisedLyrics = record
    //Direct mapped fields
    bTimeStampFormat: Byte;
    bContentType: Byte;
    dwLyricsLen: DWord;
  end;
  {$EXTERNALSYM ASF_FLAT_SYNCHRONISED_LYRICS}
  ASF_FLAT_SYNCHRONISED_LYRICS = _ASFFlatSynchronisedLyrics;

type  //MFQualityManager
 {$EXTERNALSYM _MF_QUALITY_DROP_MODE}
  _MF_QUALITY_DROP_MODE = (
    MF_DROP_MODE_NONE = 0,
    MF_DROP_MODE_1    = $1,
    MF_DROP_MODE_2    = $2,
    MF_DROP_MODE_3    = $3,
    MF_DROP_MODE_4    = $4,
    MF_DROP_MODE_5    = $5,
    MF_NUM_DROP_MODES = $6
  );
  {$EXTERNALSYM MF_QUALITY_DROP_MODE}
  MF_QUALITY_DROP_MODE = _MF_QUALITY_DROP_MODE;

type  //MFQualityManager
  {$EXTERNALSYM _MF_QUALITY_LEVEL}
  _MF_QUALITY_LEVEL           = (
    MF_QUALITY_NORMAL         = 0,
    MF_QUALITY_NORMAL_MINUS_1 = $1,
    MF_QUALITY_NORMAL_MINUS_2 = $2,
    MF_QUALITY_NORMAL_MINUS_3 = $3,
    MF_QUALITY_NORMAL_MINUS_4 = $4,
    MF_QUALITY_NORMAL_MINUS_5 = $5,
    MF_NUM_QUALITY_LEVELS     = $6
  );
  {$EXTERNALSYM MF_QUALITY_LEVEL}
  MF_QUALITY_LEVEL = _MF_QUALITY_LEVEL;

type  //MFQualityManager
  //#if (WINVER >= _WIN32_WINNT_WIN7)
  {$EXTERNALSYM _MF_QUALITY_ADVISE_FLAGS}
  _MF_QUALITY_ADVISE_FLAGS    = (
    MF_QUALITY_CANNOT_KEEP_UP = $1
  );
  {$EXTERNALSYM MF_QUALITY_ADVISE_FLAGS}
  MF_QUALITY_ADVISE_FLAGS = _MF_QUALITY_ADVISE_FLAGS;
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

type
  {$EXTERNALSYM MFSequencerElementId}
  MFSequencerElementId = DWord;

type
  {$EXTERNALSYM _MFSequencerTopologyFlags}
  _MFSequencerTopologyFlags     = (
    SequencerTopologyFlags_Last = $1
  );
  {$EXTERNALSYM MFSequencerTopologyFlags}
  MFSequencerTopologyFlags = _MFSequencerTopologyFlags;

type
   {$EXTERNALSYM MF_ATTRIBUTE_TYPE}
    MF_ATTRIBUTE_TYPE = record
      u32: UINT32;
      u64: UINT64;
      d  : Double;
    end;

type
   {$EXTERNALSYM _MFTOPONODE_ATTRIBUTE_UPDATE}
   _MFTOPONODE_ATTRIBUTE_UPDATE  = record
         NodeId:           TOPOID;
         guidAttributeKey: TGuid;
         attrType:         MF_ATTRIBUTE_TYPE;
         end;
   {$EXTERNALSYM MFTOPONODE_ATTRIBUTE_UPDATE}
   MFTOPONODE_ATTRIBUTE_UPDATE = _MFTOPONODE_ATTRIBUTE_UPDATE;

type
  {$EXTERNALSYM _MF_LEAKY_BUCKET_PAIR}
  _MF_LEAKY_BUCKET_PAIR = record
    dwBitrate: DWORD;
    msBufferWindow: DWORD;
  end;
  {$EXTERNALSYM MF_LEAKY_BUCKET_PAIR}
  MF_LEAKY_BUCKET_PAIR = _MF_LEAKY_BUCKET_PAIR;
  PMF_LEAKY_BUCKET_PAIR = ^MF_LEAKY_BUCKET_PAIR;

type
  {$EXTERNALSYM _MFBYTESTREAM_BUFFERING_PARAMS}
  _MFBYTESTREAM_BUFFERING_PARAMS = record
    cbTotalFileSize: QWORD;
    cbPlayableDataSize: QWORD;
    prgBuckets: PMF_LEAKY_BUCKET_PAIR;
    cBuckets: DWORD;
    qwNetBufferingTime: QWORD;
    qwExtraBufferingTimeDuringSeek: QWORD;
    qwPlayDuration: QWORD;
    dRate: Single;
  end;
  {$EXTERNALSYM MFBYTESTREAM_BUFFERING_PARAMS}
  MFBYTESTREAM_BUFFERING_PARAMS = _MFBYTESTREAM_BUFFERING_PARAMS;

type
  //IMFNetCredentialManager
  {$EXTERNALSYM _MFNetCredentialManagerGetParam}
  _MFNetCredentialManagerGetParam = record
    hrOp: HResult;
    fAllowLoggedOnUser: BOOL;
    fClearTextPackage: BOOL;
    pszUrl: PWideChar;
    pszSite: PWideChar;
    pszRealm: PWideChar;
    pszPackage: PWideChar;
    nRetries: Long;
  end;
  {$EXTERNALSYM MFNetCredentialManagerGetParam}
  MFNetCredentialManagerGetParam = _MFNetCredentialManagerGetParam;

type
  //IMFNetCredentialCache
  {$EXTERNALSYM _MFNetCredentialRequirements}
  _MFNetCredentialRequirements = (
    REQUIRE_PROMPT        = $1,
    REQUIRE_SAVE_SELECTED = $2
  );
  {$EXTERNALSYM MFNetCredentialRequirements}
  MFNetCredentialRequirements = _MFNetCredentialRequirements;

type
  {$EXTERNALSYM _MFNetCredentialOptions}
  _MFNetCredentialOptions             = (
    MFNET_CREDENTIAL_SAVE             = $1,
    MFNET_CREDENTIAL_DONT_CACHE       = $2,
    MFNET_CREDENTIAL_ALLOW_CLEAR_TEXT = $4
  );
  {$EXTERNALSYM MFNetCredentialOptions}
  MFNetCredentialOptions = _MFNetCredentialOptions;

type
  {$EXTERNALSYM _MFNetAuthenticationFlags}
  _MFNetAuthenticationFlags             = (
    MFNET_AUTHENTICATION_PROXY          = $1,
    MFNET_AUTHENTICATION_CLEAR_TEXT     = $2,
    MFNET_AUTHENTICATION_LOGGED_ON_USER = $4
  );
  {$EXTERNALSYM MFNetAuthenticationFlags}
  MFNetAuthenticationFlags = _MFNetAuthenticationFlags;

type
{$EXTERNALSYM _MFNETSOURCE_PROTOCOL_TYPE}
  _MFNETSOURCE_PROTOCOL_TYPE = (
    MFNETSOURCE_UNDEFINED = 0,
    MFNETSOURCE_HTTP      = $1,
    MFNETSOURCE_RTSP      = $2,
    MFNETSOURCE_FILE      = $3,
    MFNETSOURCE_MULTICAST = $4
  );
  {$EXTERNALSYM MFNETSOURCE_PROTOCOL_TYPE}
  MFNETSOURCE_PROTOCOL_TYPE = _MFNETSOURCE_PROTOCOL_TYPE;

type
  {$EXTERNALSYM _MFNETSOURCE_TRANSPORT_TYPE}
  _MFNETSOURCE_TRANSPORT_TYPE = (
    MFNETSOURCE_UDP = 0,
    MFNETSOURCE_TCP = 1
  );
  {$EXTERNALSYM MFNETSOURCE_TRANSPORT_TYPE}
  MFNETSOURCE_TRANSPORT_TYPE = _MFNETSOURCE_TRANSPORT_TYPE;

type
  {$EXTERNALSYM _MFNETSOURCE_CACHE_STATE}
  _MFNETSOURCE_CACHE_STATE            = (
    MFNETSOURCE_CACHE_UNAVAILABLE     = 0,
    MFNETSOURCE_CACHE_ACTIVE_WRITING  = 1,
    MFNETSOURCE_CACHE_ACTIVE_COMPLETE = 2
  );
  {$EXTERNALSYM MFNETSOURCE_CACHE_STATE}
  MFNETSOURCE_CACHE_STATE = _MFNETSOURCE_CACHE_STATE;

type
  {$EXTERNALSYM _MFNETSOURCE_STATISTICS_IDS}
  _MFNETSOURCE_STATISTICS_IDS               = (
    MFNETSOURCE_RECVPACKETS_ID              = 0,
    MFNETSOURCE_LOSTPACKETS_ID              = 1,
    MFNETSOURCE_RESENDSREQUESTED_ID         = 2,
    MFNETSOURCE_RESENDSRECEIVED_ID          = 3,
    MFNETSOURCE_RECOVEREDBYECCPACKETS_ID    = 4,
    MFNETSOURCE_RECOVEREDBYRTXPACKETS_ID    = 5,
    MFNETSOURCE_OUTPACKETS_ID               = 6,
    MFNETSOURCE_RECVRATE_ID                 = 7,
    MFNETSOURCE_AVGBANDWIDTHBPS_ID          = 8,
    MFNETSOURCE_BYTESRECEIVED_ID            = 9,
    MFNETSOURCE_PROTOCOL_ID                 = 10,
    MFNETSOURCE_TRANSPORT_ID                = 11,
    MFNETSOURCE_CACHE_STATE_ID              = 12,
    MFNETSOURCE_LINKBANDWIDTH_ID            = 13,
    MFNETSOURCE_CONTENTBITRATE_ID           = 14,
    MFNETSOURCE_SPEEDFACTOR_ID              = 15,
    MFNETSOURCE_BUFFERSIZE_ID               = 16,
    MFNETSOURCE_BUFFERPROGRESS_ID           = 17,
    MFNETSOURCE_LASTBWSWITCHTS_ID           = 18,
    MFNETSOURCE_SEEKRANGESTART_ID           = 19,
    MFNETSOURCE_SEEKRANGEEND_ID             = 20,
    MFNETSOURCE_BUFFERINGCOUNT_ID           = 21,
    MFNETSOURCE_INCORRECTLYSIGNEDPACKETS_ID = 22,
    MFNETSOURCE_SIGNEDSESSION_ID            = 23,
    MFNETSOURCE_MAXBITRATE_ID               = 24,
    MFNETSOURCE_RECEPTION_QUALITY_ID        = 25,
    MFNETSOURCE_RECOVEREDPACKETS_ID         = 26,
    MFNETSOURCE_VBR_ID                      = 27,
    MFNETSOURCE_DOWNLOADPROGRESS_ID         = 28,
    MFNETSOURCE_UNPREDEFINEDPROTOCOLNAME_ID = 29
  );
  {$EXTERNALSYM MFNETSOURCE_STATISTICS_IDS}
  MFNETSOURCE_STATISTICS_IDS = _MFNETSOURCE_STATISTICS_IDS;

type
  {$EXTERNALSYM _MFNET_PROXYSETTINGS}
  _MFNET_PROXYSETTINGS         = (
    MFNET_PROXYSETTING_NONE    = 0,
    MFNET_PROXYSETTING_MANUAL  = 1,
    MFNET_PROXYSETTING_AUTO    = 2,
    MFNET_PROXYSETTING_BROWSER = 3
  );
  {$EXTERNALSYM MFNET_PROXYSETTINGS}
  MFNET_PROXYSETTINGS = _MFNET_PROXYSETTINGS;

type
  {$EXTERNALSYM _MFPOLICYMANAGER_ACTION}
  _MFPOLICYMANAGER_ACTION = (
    PEACTION_NO        = 0,
    PEACTION_PLAY      = 1,
    PEACTION_COPY      = 2,
    PEACTION_EXPORT    = 3,
    PEACTION_EXTRACT   = 4,
    PEACTION_RESERVED1 = 5,
    PEACTION_RESERVED2 = 6,
    PEACTION_RESERVED3 = 7,
    PEACTION_LAST      = 7
  );
  {$EXTERNALSYM MFPOLICYMANAGER_ACTION}
  MFPOLICYMANAGER_ACTION = _MFPOLICYMANAGER_ACTION;

type
  {$EXTERNALSYM _MFINPUTTRUSTAUTHORITY_ACTION}
  _MFINPUTTRUSTAUTHORITY_ACTION = record
    Action: MFPOLICYMANAGER_ACTION;
    pbTicket: PByte;
    cbTicket: DWORD;
  end;
  {$EXTERNALSYM MFINPUTTRUSTAUTHORITY_ACCESS_ACTION}
  MFINPUTTRUSTAUTHORITY_ACCESS_ACTION = _MFINPUTTRUSTAUTHORITY_ACTION;

type
  {$EXTERNALSYM _MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS}
  _MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS = record
    dwSize: DWORD;
    dwVer: DWORD;
    cbSignatureOffset: DWORD;
    cbSignatureSize: DWORD;
    cbExtensionOffset: DWORD;
    cbExtensionSize: DWORD;
    cActions: DWORD;
    rgOutputActions: array[0..0] of MFINPUTTRUSTAUTHORITY_ACCESS_ACTION;
  end;
  {$EXTERNALSYM MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS}
  MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS = _MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS;

const
  MFOUTPUTATTRIBUTE_DIGITAL                  =DWORD( $00000001);
  MFOUTPUTATTRIBUTE_NONSTANDARDIMPLEMENTATION=DWORD( $00000002);
  MFOUTPUTATTRIBUTE_VIDEO                    =DWORD( $00000004);
  MFOUTPUTATTRIBUTE_COMPRESSED               =DWORD( $00000008);
  MFOUTPUTATTRIBUTE_SOFTWARE                 =DWORD( $00000010);
  MFOUTPUTATTRIBUTE_BUS                      =DWORD( $00000020);
  MFOUTPUTATTRIBUTE_BUSIMPLEMENTATION        =DWORD( $0000FF00);

type
  cwSAMPLE_PROTECTION_VERSION            = (
    SAMPLE_PROTECTION_VERSION_NO         = 0,
    SAMPLE_PROTECTION_VERSION_BASIC_LOKI = 1,
    SAMPLE_PROTECTION_VERSION_SCATTER    = 2,
    SAMPLE_PROTECTION_VERSION_RC4        = 3
  );
  {$EXTERNALSYM SAMPLE_PROTECTION_VERSION}
  SAMPLE_PROTECTION_VERSION = cwSAMPLE_PROTECTION_VERSION;

type
 //#if (WINVER >= _WIN32_WINNT_WIN7)
  {$EXTERNALSYM _MF_TRANSCODE_TOPOLOGYMODE_FLAGS}
  _MF_TRANSCODE_TOPOLOGYMODE_FLAGS             = (
    MF_TRANSCODE_TOPOLOGYMODE_SOFTWARE_ONLY    = 0,
    MF_TRANSCODE_TOPOLOGYMODE_HARDWARE_ALLOWED = 1
  );
  {$EXTERNALSYM MF_TRANSCODE_TOPOLOGYMODE_FLAGS}
  MF_TRANSCODE_TOPOLOGYMODE_FLAGS = _MF_TRANSCODE_TOPOLOGYMODE_FLAGS;

type
  {$EXTERNALSYM _MF_TRANSCODE_ADJUST_PROFILE_FLAGS}
  _MF_TRANSCODE_ADJUST_PROFILE_FLAGS                  = (
    MF_TRANSCODE_ADJUST_PROFILE_DEFAULT               = 0,
    MF_TRANSCODE_ADJUST_PROFILE_USE_SOURCE_ATTRIBUTES = 1
  );
  {$EXTERNALSYM MF_TRANSCODE_ADJUST_PROFILE_FLAGS}
  MF_TRANSCODE_ADJUST_PROFILE_FLAGS = _MF_TRANSCODE_ADJUST_PROFILE_FLAGS;

type
  {$EXTERNALSYM _MF_TRANSCODE_SINK_INFO}
  _MF_TRANSCODE_SINK_INFO = record
    dwVideoStreamID: DWORD;
    pVideoMediaType: IMFMediaType;
    dwAudioStreamID: DWORD;
    pAudioMediaType: IMFMediaType;
  end;
  {$EXTERNALSYM MF_TRANSCODE_SINK_INFO}
  MF_TRANSCODE_SINK_INFO = _MF_TRANSCODE_SINK_INFO;
  PMFT_REGISTER_TYPE_INFO = ^MFT_REGISTER_TYPE_INFO;

type
  {$EXTERNALSYM _MFT_REGISTRATION_INFO}
  _MFT_REGISTRATION_INFO = record
    clsid: CLSID;
    guidCategory: TGUID;
    uiFlags: UINT32;
    pszName: PWideChar;
    cInTypes: DWORD;
    pInTypes: PMFT_REGISTER_TYPE_INFO;
    cOutTypes: DWORD;
    pOutTypes: PMFT_REGISTER_TYPE_INFO;
  end;
  {$EXTERNALSYM MFT_REGISTRATION_INFO}
  MFT_REGISTRATION_INFO = _MFT_REGISTRATION_INFO;
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)
  // end >= Windows 7



//Forward Interface Declarations

type
  {$EXTERNALSYM IMFMediaStream}
  IMFMediaStream = interface;
  {$EXTERNALSYM IMFTimer}
  IMFTimer = interface;
  {$EXTERNALSYM IMFShutdown}
  IMFShutdown = interface;
  {$EXTERNALSYM IMFContentProtectionManager}
  IMFContentProtectionManager = interface;
  {$EXTERNALSYM IMFContentEnabler}
  IMFContentEnabler = interface;
  {$EXTERNALSYM IMFMetadata}
  IMFMetadata = interface;
  {$EXTERNALSYM IMFMetadataProvider}
  IMFMetadataProvider = interface;
  {$EXTERNALSYM IMFTimecodeTranslate}
  IMFTimecodeTranslate = interface;
  {$EXTERNALSYM IMFSampleGrabberSinkCallback}
  IMFSampleGrabberSinkCallback = interface;
  {$EXTERNALSYM IMFSampleGrabberSinkCallback2}
  IMFSampleGrabberSinkCallback2 = interface;
  {$EXTERNALSYM IMFWorkQueueServices}
  IMFWorkQueueServices = interface;
  {$EXTERNALSYM IMFQualityManager}
  IMFQualityManager = interface;
  {$EXTERNALSYM IMFQualityAdvise}
  IMFQualityAdvise = interface;
  {$EXTERNALSYM IMFQualityAdvise2}
  IMFQualityAdvise2 = interface;
  {$EXTERNALSYM IMFQualityAdviseLimits}
  IMFQualityAdviseLimits = interface;
  {$EXTERNALSYM IMFRealTimeClient}
  IMFRealTimeClient = interface;
  {$EXTERNALSYM IMFSequencerSource}
  IMFSequencerSource = interface;
  {$EXTERNALSYM IMFMediaSourceTopologyProvider}
  IMFMediaSourceTopologyProvider = interface;
  {$EXTERNALSYM IMFMediaSourcePresentationProvider}
  IMFMediaSourcePresentationProvider = interface;
  {$EXTERNALSYM IMFTopologyNodeAttributeEditor}
  IMFTopologyNodeAttributeEditor = interface;
  {$EXTERNALSYM IMFByteStreamBuffering}
  IMFByteStreamBuffering = interface;
  {$EXTERNALSYM IMFByteStreamCacheControl}
  IMFByteStreamCacheControl = interface;
  {$EXTERNALSYM IMFNetCredential}
  IMFNetCredential = interface;
   {$EXTERNALSYM IMFNetCredentialManager}
  IMFNetCredentialManager = interface;
  {$EXTERNALSYM IMFNetCredentialCache}
  IMFNetCredentialCache = interface;
  {$EXTERNALSYM IMFSSLCertificateManager}
  IMFSSLCertificateManager = interface;
  {$EXTERNALSYM IMFSourceOpenMonitor}
  IMFSourceOpenMonitor = interface;
  {$EXTERNALSYM IMFNetProxyLocator}
  IMFNetProxyLocator = interface;
  {$EXTERNALSYM IMFNetProxyLocatorFactory}
  IMFNetProxyLocatorFactory = interface;
  {$EXTERNALSYM IMFSaveJob}
  IMFSaveJob = interface;
  {$EXTERNALSYM IMFNetSchemeHandlerConfig}
  IMFNetSchemeHandlerConfig = interface;
  {$EXTERNALSYM IMFSchemeHandler}
  IMFSchemeHandler = interface;
  {$EXTERNALSYM IMFByteStreamHandler}
  IMFByteStreamHandler = interface;
  {$EXTERNALSYM IMFTrustedInput}
  IMFTrustedInput = interface;
  {$EXTERNALSYM IMFInputTrustAuthority}
  IMFInputTrustAuthority = interface;
  {$EXTERNALSYM IMFTrustedOutput}
  IMFTrustedOutput = interface;
  {$EXTERNALSYM IMFOutputTrustAuthority}
  IMFOutputTrustAuthority = interface;
  {$EXTERNALSYM IMFOutputPolicy}
  IMFOutputPolicy = interface;
  {$EXTERNALSYM IMFOutputSchema}
  IMFOutputSchema = interface;
  {$EXTERNALSYM IMFSecureChannel}
  IMFSecureChannel = interface;
  {$EXTERNALSYM IMFSampleProtection}
  IMFSampleProtection = interface;
  {$EXTERNALSYM IMFMediaSinkPreroll}
  IMFMediaSinkPreroll = interface;
  {$EXTERNALSYM IMFFinalizableMediaSink}
  IMFFinalizableMediaSink = interface;
  {$EXTERNALSYM IMFStreamingSinkConfig}
  IMFStreamingSinkConfig = interface;
  {$EXTERNALSYM IMFRemoteProxy}
  IMFRemoteProxy = interface;
  {$EXTERNALSYM IMFObjectReferenceStream}
  IMFObjectReferenceStream = interface;
  {$EXTERNALSYM IMFPMPHost}
  IMFPMPHost = interface;
  {$EXTERNALSYM IMFPMPClient}
  IMFPMPClient = interface;
  {$EXTERNALSYM IMFPMPServer}
  IMFPMPServer = interface;
  {$EXTERNALSYM IMFRemoteDesktopPlugin}
  IMFRemoteDesktopPlugin = interface;
  {$EXTERNALSYM IMFSAMIStyle}
  IMFSAMIStyle = interface;
  {$EXTERNALSYM IMFTranscodeProfile}
  IMFTranscodeProfile = interface;
  {$EXTERNALSYM IMFTranscodeSinkInfoProvider}
  IMFTranscodeSinkInfoProvider = interface;
  {$EXTERNALSYM IMFFieldOfUseMFTUnlock}
  IMFFieldOfUseMFTUnlock = interface;
  {$EXTERNALSYM IMFLocalMFTRegistration}
  IMFLocalMFTRegistration = interface;

  //INTERFACES

  //Interface IMFMediaStream
  IMFMediaStream = interface(IUnknown)
	['{D182108F-4EC6-443f-AA42-A71106EC825F}']
    function GetMediaSource(out ppMediaSource: IMFMediaSource): HResult; stdcall;
    function GetStreamDescriptor(out ppStreamDescriptor: IMFStreamDescriptor): HResult; stdcall;
    function RequestSample(const pToken: IUnknown): HResult; stdcall;
  end;

  IMFOutputTrustAuthority = interface(IUnknown)
  ['{D19F8E94-B126-4446-890C-5DCB7AD71453}']
    function GetAction(out Action: MFPOLICYMANAGER_ACTION): HRESULT; stdcall;
    function SetPolicy(const Policy: IMFOutputPolicy;
                       const nPolicy: DWORD;
                       out bTicket: BYTE;
                       out cbTicket: DWORD): HRESULT; stdcall;
  end;

  IMFMediaSinkPreroll = interface(IUnknown)
  ['{5dfd4b2a-7674-4110-a4e6-8a68fd5f3688}']
    function NotifyPreroll(const hnsUpcomingStartTime: MFTIME): HRESULT; stdcall;
  end;

  IMFStreamingSinkConfig = interface(IUnknown)
  ['{9db7aa41-3cc5-40d4-8509-555804ad34cc}']
     function StartStreaming(const fSeekOffsetIsByteOffset: BOOL;
                             const qwSeekOffset: QWORD): HRESULT; stdcall;
  end;

  IMFObjectReferenceStream = interface(IUnknown)
  ['{09EF5BE3-C8A7-469e-8B70-73BF25BB193F}']
    function SaveReference(const riid: REFIID; const pUnk: IUnknown): HRESULT; stdcall;
    function LoadReference(const riid: REFIID; out ppv: Pointer): HRESULT; stdcall;
  end;

  IMFRemoteDesktopPlugin = interface(IUnknown)
  ['{1cde6309-cae0-4940-907e-c1ec9c3d1d4a}']
    function UpdateTopology(var Topology: IMFTopology): HRESULT; stdcall;
  end;






  //Interface IMFTimer
  IMFTimer = interface(IUnknown)
	['{e56e4cbd-8f70-49d8-a0f8-edb3d6ab9bf2}']
    function SetTimer(const dwFlags: Dword; const llClockTime: LongLong;
                      const pCallback: IMFAsyncCallback; const punkState: IUnknown;
                      out ppunkKey: IUnknown): HResult; stdcall;
    function CancelTimer(const punkKey: IUnknown): HResult; stdcall;
  end;

  //Interface IMFShutdown */
  IMFShutdown = interface(IUnknown)
	['{97ec2ea4-0e42-4937-97ac-9d6d328824e1}']
    function Shutdown(): HResult; stdcall;
    function GetShutdownStatus(out pStatus: MFSHUTDOWN_STATUS): HResult; stdcall;
  end;


  //Interface IMFContentProtectionManager
  IMFContentProtectionManager = interface(IUnknown)
	['{ACF92459-6A61-42bd-B57C-B43E51203CB0}']
    function BeginEnableContent(const pEnablerActivate: IMFActivate; const pTopo: IMFTopology; const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndEnableContent(const pResult: IMFAsyncResult): HResult; stdcall;
  end;

  //Interface IMFContentEnabler
  IMFContentEnabler = interface(IUnknown)
	['{D3C4EF59-49CE-4381-9071-D5BCD044C770}']
    function GetEnableType(out pType: TGuid): HResult; stdcall;
    function GetEnableURL(out ppwszURL: LPWSTR; out pcchURL: DWord; var pTrustStatus: MF_URL_TRUST_STATUS): HResult; stdcall;
    function GetEnableData(out ppbData: Byte; out pcbData: Dword): HResult; stdcall;
    function IsAutomaticSupported(out pfAutomatic: Bool): HResult; stdcall;
    function AutomaticEnable(): HResult; stdcall;
    function MonitorEnable(): HResult; stdcall;
    function Cancel(): HResult; stdcall;
  end;

  // interface IMFMetadata
  IMFMetadata = interface(IUnknown)
	['{F88CFB8C-EF16-4991-B450-CB8C69E51704}']
    function SetLanguage(const pwszRFC1766: LPCWSTR): HResult; stdcall;
    function GetLanguage(out ppwszRFC1766: LPCWSTR): HResult; stdcall;
    function GetAllLanguages(out ppvLanguages: PROPVARIANT): HResult; stdcall;
    function SetProperty(const pwszName: LPCWSTR; const ppvValue: PROPVARIANT): HResult; stdcall;
    function GetProperty(const pwszName: LPCWSTR; out ppvValue: PROPVARIANT): HResult; stdcall;
    function DeleteProperty(const pwszName: LPCWSTR): HResult; stdcall;
    function GetAllPropertyNames(out ppvNames: PROPVARIANT): HResult; stdcall;
  end;

  //Interface IMFMetadataProvider
  IMFMetadataProvider = interface(IUnknown)
	['{56181D2D-E221-4adb-B1C8-3CEE6A53F76F}']
    function GetMFMetadata(const pPresentationDescriptor: IMFPresentationDescriptor;
                           const dwStreamIdentifier: DWord;
                           const dwFlags: DWord;
                           out ppMFMetadata: IMFMetadata): HResult; stdcall;
  end;


  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // >= Windows 7
  //Interface IMFTimecodeTranslate
  IMFTimecodeTranslate = interface(IUnknown)
	['{ab9d8661-f7e8-4ef4-9861-89f334f94e74}']
    function BeginConvertTimecodeToHNS(const pPropVarTimecode: PROPVARIANT; const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndConvertTimecodeToHNS(const pResult: IMFAsyncResult; out phnsTime: MFTIME): HResult; stdcall;
    function BeginConvertHNSToTimecode(const hnsTime: MFTIME; const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndConvertHNSToTimecode(const pResult: IMFAsyncResult; out pPropVarTimecode: PROPVARIANT): HResult; stdcall;
  end;

  //Interface IMFSampleGrabberSinkCallback
  IMFSampleGrabberSinkCallback = interface(IUnknown)
	['{8C7B80BF-EE42-4b59-B1DF-55668E1BDCA8}']
    function OnSetPresentationClock(const pPresentationClock:  IMFPresentationClock): HResult; stdcall;
    function OnProcessSample( const guidMajorMediaType: REFGUID; const dwSampleFlags: DWord;
                              const llSampleTime: LONGLONG; const llSampleDuration: LONGLONG;
                              const pSampleBuffer: Byte; const dwSampleSize: Dword): HResult; stdcall;
    function OnShutdown(): HResult; stdcall;
  end;

  //Interface IMFSampleGrabberSinkCallback2
  IMFSampleGrabberSinkCallback2 = interface(IMFSampleGrabberSinkCallback)   //modified t19122012b, Reported by eric.c.fortier, Oct 5, 2012
	['{ca86aa50-c46e-429e-ab27-16d6ac6844cb}']
    function nProcessSampleEx(const guidMajorMediaType: REFGUID; const dwSampleFlags: DWord;
                              const llSampleTime: LONGLONG; const llSampleDuration: LONGLONG;
                              const pSampleBuffer: Byte; const pAttributes: IMFAttributes): HResult; stdcall;
  end;
  //#endif (WINVER >= _WIN32_WINNT_WIN7)
  // End >= Windows 7

  //Interface IMFWorkQueueServices
  IMFWorkQueueServices = interface(IUnknown)
	['{35FE1BB8-A3A9-40fe-BBEC-EB569C9CCCA3}']
    function BeginRegisterTopologyWorkQueuesWithMMCSS(const pCallback: IMFAsyncCallback; const pState: IUnknown): HResult; stdcall;
    function EndRegisterTopologyWorkQueuesWithMMCSS(const pResult: IMFAsyncResult): HResult; stdcall;
    function BeginUnregisterTopologyWorkQueuesWithMMCSS(const pCallback: IMFAsyncCallback; const pState: IUnknown): HResult; stdcall;
    function EndUnregisterTopologyWorkQueuesWithMMCSS(const pResult: IMFAsyncResult): HResult; stdcall;
    function GetTopologyWorkQueueMMCSSClass(const dwTopologyWorkQueueId: Dword; out pwszClass: LPWSTR; var pcchClass: DWord): HResult; stdcall;
    function GetTopologyWorkQueueMMCSSTaskId(const dwTopologyWorkQueueId: Dword; out pdwTaskId: DWord): HResult; stdcall;
    function BeginRegisterPlatformWorkQueueWithMMCSS(const dwPlatformWorkQueue: DWord; const wszClass: LPCWSTR;
                                                     const dwTaskId: Dword; const pCallback: IMFAsyncCallback;
                                                     const pState: IUnknown): HResult; stdcall;
    function EndRegisterPlatformWorkQueueWithMMCSS(const pResult: IMFAsyncResult; out pdwTaskId: DWord): HResult; stdcall;
    function BeginUnregisterPlatformWorkQueueWithMMCSS(const dwPlatformWorkQueue: Dword; const pCallback: IMFAsyncCallback; const pState: IUnknown): HResult; stdcall;
    function EndUnregisterPlatformWorkQueueWithMMCSS(const pResult: IMFAsyncResult): HResult; stdcall;
    function GetPlaftormWorkQueueMMCSSClass(const dwPlatformWorkQueueId: Dword; out pwszClass: LPWSTR; var pcchClass: Dword): HResult; stdcall;
    function GetPlatformWorkQueueMMCSSTaskId(const dwPlatformWorkQueueId: DWord; out pdwTaskId: Dword): HResult; stdcall;
  end;

  //Interface IMFQualityManager
  IMFQualityManager = interface(IUnknown)
	['{8D009D86-5B9F-4115-B1FC-9F80D52AB8AB}']
    function NotifyTopology(const pTopology: IMFTopology): HResult; stdcall;
    function NotifyPresentationClock(const pClock: IMFPresentationClock): HResult; stdcall;
    function NotifyProcessInput(const pNode: IMFTopologyNode; const lInputIndex: Long; const pSample: IMFSample): HResult; stdcall;
    function NotifyProcessOutput(const pNode: IMFTopologyNode; const lOutputIndex: Long; const pSample: IMFSample): HResult; stdcall;
    function NotifyQualityEvent(const pObject: IUnknown; const pEvent: IMFMediaEvent): HResult; stdcall;
    function Shutdown(): HResult; stdcall;
  end;

  //Interface IMFQualityAdvise
  IMFQualityAdvise = interface(IUnknown)
	['{EC15E2E9-E36B-4f7c-8758-77D452EF4CE7}']
    function SetDropMode(const eDropMode: MF_QUALITY_DROP_MODE): HResult; stdcall;
    function SetQualityLevel(const eQualityLevel: MF_QUALITY_LEVEL): HResult; stdcall;
    function GetDropMode(out peDropMode: MF_QUALITY_DROP_MODE): HResult; stdcall;
    function GetQualityLevel(out peQualityLevel: MF_QUALITY_LEVEL): HResult; stdcall;
    function DropTime(const hnsAmountToDrop: LONGLONG): HResult; stdcall;
   end;

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // >= Windiows 7
  //Interface IMFQualityAdvise2
  IMFQualityAdvise2 = interface(IMFQualityAdvise)
	['{F3706F0D-8EA2-4886-8000-7155E9EC2EAE}']
    function NotifyQualityEvent(const pEvent: IMFMediaEvent; out pdwFlags: DWord): HResult; stdcall;
  end;

  //Interface IMFQualityAdviseLimits
  IMFQualityAdviseLimits = interface(IUnknown)
	['{dfcd8e4d-30b5-4567-acaa-8eb5b7853dc9}']
    function GetMaximumDropMode(out peDropMode: MF_QUALITY_DROP_MODE): HResult; stdcall;
    function GetMinimumQualityLevel(out peQualityLevel: MF_QUALITY_LEVEL): HResult; stdcall;
  end;

  //#endif // (WINVER >= _WIN32_WINNT_WIN7)
  // end >= Windiows 7

  //Interface IMFRealTimeClient
  IMFRealTimeClient = interface(IUnknown)
	['{2347D60B-3FB5-480c-8803-8DF3ADCD3EF0}']
    function RegisterThreads(const dwTaskIndex: DWord; const wszClass: LPCWSTR): HResult; stdcall;
    function UnregisterThreads(): HResult; stdcall;
    function SetWorkQueue(const dwWorkQueueId: Dword): HResult; stdcall;
  end;

  //Interface IMFSequencerSource
  IMFSequencerSource = interface(IUnknown)
	['{197CD219-19CB-4de1-A64C-ACF2EDCBE59E}']
    function AppendTopology(const pTopology: IMFTopology; const dwFlags: Dword; out pdwId: MFSequencerElementId): HResult; stdcall;
    function DeleteTopology(const dwId: MFSequencerElementId): HResult; stdcall;
    function GetPresentationContext(const pPD: IMFPresentationDescriptor; out pId: MFSequencerElementId; out ppTopology: IMFTopology): HResult; stdcall;
    function UpdateTopology(const dwId: MFSequencerElementId; const pTopology: IMFTopology): HResult; stdcall;
    function UpdateTopologyFlags(const dwId: MFSequencerElementId; const dwFlags: Dword): HResult; stdcall;
  end;

  //Interface IMFMediaSourceTopologyProvider
  IMFMediaSourceTopologyProvider = interface(IUnknown)
	['{0E1D6009-C9F3-442d-8C51-A42D2D49452F}']
    function GetMediaSourceTopology(const pPresentationDescriptor: IMFPresentationDescriptor; out ppTopology: IMFTopology): HResult; stdcall;
  end;

  IMFMediaSourcePresentationProvider = interface(IUnknown)
  ['{0E1D600a-C9F3-442d-8C51-A42D2D49452F}']
    function ForceEndOfPresentation(const PresentationDescriptor: IMFPresentationDescriptor ): HRESULT; stdcall;
  end;

  //Interface IMFTopologyNodeAttributeEditor
  IMFTopologyNodeAttributeEditor = interface(IUnknown)
	['{676aa6dd-238a-410d-bb99-65668d01605a}']
    function UpdateNodeAttributes(const TopoId: TOPOID; const cUpdates: DWord; const pUpdates: MFTOPONODE_ATTRIBUTE_UPDATE): HResult; stdcall;
  end;

  //Interface IMFByteStreamBuffering
  IMFByteStreamBuffering = interface(IUnknown)
	['{6d66d782-1d4f-4db7-8c63-cb8c77f1ef5e}']
    function SetBufferingParams(const pParams: MFBYTESTREAM_BUFFERING_PARAMS): HResult; stdcall;
    function EnableBuffering(const fEnable: boolean): HResult; stdcall;
    function StopBuffering(): HResult; stdcall;
  end;

  //Interface IMFByteStreamCacheControl
  IMFByteStreamCacheControl = interface(IUnknown)
	['{F5042EA4-7A96-4a75-AA7B-2BE1EF7F88D5}']
    function StopBackgroundTransfer(): HResult; stdcall;
  end;

  //Interface IMFNetCredential
  IMFNetCredential = interface(IUnknown)
	['{5b87ef6a-7ed8-434f-ba0e-184fac1628d1}']
    function SetUser(const pbData: Byte; const cbData: DWord; const fDataIsEncrypted: Boolean): HResult; stdcall;
    function SetPassword(const pbData: Byte; cbData: DWord; const fDataIsEncrypted: Boolean): HResult; stdcall;
    function GetUser(const pbData: Byte; pcbData: DWord; const fEncryptData: Boolean): HResult; stdcall;
    function GetPassword(out pbData: Byte; var pcbData: DWord; const fEncryptData: Boolean): HResult; stdcall;
    function LoggedOnUser(out pfLoggedOnUser: Boolean): HResult; stdcall;
  end;

  //Interface IMFNetCredentialManager
  IMFNetCredentialManager = interface(IUnknown)
	['{5b87ef6b-7ed8-434f-ba0e-184fac1628d1}']
    function BeginGetCredentials(const pParam: MFNetCredentialManagerGetParam; const pCallback: IMFAsyncCallback; const pState: IUnknown): HResult; stdcall;
    function EndGetCredentials(const pResult: IMFAsyncResult; out ppCred: IMFNetCredential): HResult; stdcall;
    function SetGood(const pCred: IMFNetCredential; const fGood: Boolean): HResult; stdcall;
   end;

  //Interface IMFNetCredentialCache
  IMFNetCredentialCache = interface(IUnknown)
	['{5b87ef6c-7ed8-434f-ba0e-184fac1628d1}']
    function GetCredential(const pszUrl: LPCWSTR; const pszRealm: LPCWSTR; const dwAuthenticationFlags: DWord;
                           out ppCred: IMFNetCredential; out pdwRequirementsFlags: DWord): HResult; stdcall;
    function SetGood(const pCred: IMFNetCredential; const fGood: BOOL): HResult; stdcall;
    function SetUserOptions(const pCred: IMFNetCredential; const dwOptionsFlags: DWord): HResult; stdcall;
  end;

  //Interface IMFSSLCertificateManager
  IMFSSLCertificateManager = interface(IUnknown)
	['{61f7d887-1230-4a8b-aeba-8ad434d1a64d}']
    function GetClientCertificate(const pszURL: LPCWSTR; out ppbData: Byte; out pcbData: DWord): HResult; stdcall;
    function BeginGetClientCertificate(const pszURL: LPCWSTR; const pCallback: IMFAsyncCallback; const pState: IUnknown): HResult; stdcall;
    function EndGetClientCertificate(const pResult: IMFAsyncResult; out ppbData: Byte; out pcbData: DWord): HResult; stdcall;
    function GetCertificatePolicy(const pszURL: LPCWSTR; out pfOverrideAutomaticCheck: Boolean; out pfClientCertificateAvailable: Boolean): HResult; stdcall;
    function OnServerCertificate(const pszURL: LPCWSTR; const pbData: Byte; out pfIsGood: Boolean): HResult; stdcall;
  end;
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)
  // end >= Windows 7


  //Interface IMFSourceOpenMonitor
  IMFSourceOpenMonitor = interface(IUnknown)
	['{059054B3-027C-494C-A27D-9113291CF87F}']
    function OnSourceEvent(const pEvent: IMFMediaEvent): HResult; stdcall;
  end;

  //Interface IMFNetProxyLocator
  IMFNetProxyLocator = interface(IUnknown)
	['{e9cd0383-a268-4bb4-82de-658d53574d41}']
    function FindFirstProxy(const pszHost: LPCWSTR; const pszUrl: LPCWSTR; const fReserved: Boolean): HResult; stdcall;
    function FindNextProxy(): HResult; stdcall;
    function RegisterProxyResult(const hrOp: HResult): HResult; stdcall;
    function GetCurrentProxy(out pszStr: LPWSTR; var pcchStr: DWord): HResult; stdcall;
    function Clone(out ppProxyLocator: IMFNetProxyLocator): HResult; stdcall;
  end;

  //Interface IMFNetProxyLocatorFactory
  IMFNetProxyLocatorFactory = interface(IUnknown)
	['{e9cd0384-a268-4bb4-82de-658d53574d41}']
    function CreateProxyLocator(const pszProtocol: LPCWSTR; out ppProxyLocator: IMFNetProxyLocator): HResult; stdcall;
  end;

  //Interface IMFSaveJob
  IMFSaveJob = interface(IUnknown)
	['{e9931663-80bf-4c6e-98af-5dcf58747d1f}']
    function BeginSave(const pStream: IMFByteStream; const pCallback: IMFAsyncCallback; const pState: IUnknown): HResult; stdcall;
    function EndSave(const pResult: IMFAsyncResult): HResult; stdcall;
    function CancelSave(): HResult; stdcall;
    function GetProgress(out pdwPercentComplete: DWord): HResult; stdcall;
  end;

  //Interface IMFNetSchemeHandlerConfig
  IMFNetSchemeHandlerConfig = interface(IUnknown)
	['{7BE19E73-C9BF-468a-AC5A-A5E8653BEC87}']
    function GetNumberOfSupportedProtocols(out pcProtocols: ULONG): HResult; stdcall;
    function GetSupportedProtocolType(const nProtocolIndex: ULONG; out pnProtocolType: MFNETSOURCE_PROTOCOL_TYPE): HResult; stdcall;
    function ResetProtocolRolloverSettings(): HResult; stdcall;
  end;

  //Interface IMFSchemeHandler */
  IMFSchemeHandler = interface(IUnknown)
	['{6D4C7B74-52A0-4bb7-B0DB-55F29F47A668}']
    function BeginCreateObject(const pwszURL: LPCWSTR; const dwFlags: DWord; const pProps: IPropertyStore;
                               out ppIUnknownCancelCookie: IUnknown; const pCallback: IMFAsyncCallback; punkState: IUnknown): HResult; stdcall;
    function EndCreateObject(const pResult: IMFAsyncResult; out pObjectType: MF_OBJECT_TYPE; out ppObject: IUnknown): HResult; stdcall;
    function CancelObjectCreation(const pIUnknownCancelCookie: IUnknown): HResult; stdcall;
  end;

  //Interface IMFByteStreamHandler
  IMFByteStreamHandler = interface(IUnknown)
	['{BB420AA4-765B-4a1f-91FE-D6A8A143924C}']
    function BeginCreateObject(const pByteStream: IMFByteStream; const pwszURL: LPCWSTR;
                               const dwFlags: DWord; const pProps: IPropertyStore;
                               out ppIUnknownCancelCookie: IUnknown; const pCallback: IMFAsyncCallback;
                               const punkState: IUnknown): HResult; stdcall;
    function EndCreateObject(const pResult: IMFAsyncResult; out pObjectType: MF_OBJECT_TYPE; out ppObject: IUnknown): HResult; stdcall;
    function CancelObjectCreation(const pIUnknownCancelCookie: IUnknown): HResult; stdcall;
    function GetMaxNumberOfBytesRequiredForResolution(out pqwBytes: QWORD): HResult; stdcall; //todo: implement QWORD
  end;

  //Interface IMFTrustedInput
  IMFTrustedInput = interface(IUnknown)
	['{542612C4-A1B8-4632-B521-DE11EA64A0B0}']
    function GetInputTrustAuthority(const dwStreamID: DWord; const riid: REFIID; out ppunkObject: IUnknown): HResult; stdcall;
  end;

  //Interface IMFInputTrustAuthority
  IMFInputTrustAuthority = interface(IUnknown)
	['{D19F8E98-B126-4446-890C-5DCB7AD71453}']
    function GetDecrypter(const riid: REFIID; out ppv: Pointer): HResult; stdcall;
    function RequestAccess(const Action: MFPOLICYMANAGER_ACTION; out ppContentEnablerActivate: IMFActivate): HResult; stdcall;
    function GetPolicy(const Action: MFPOLICYMANAGER_ACTION; out ppPolicy: IMFOutputPolicy): HResult; stdcall;
    function BindAccess(const pParam: MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS): HResult; stdcall;
    function UpdateAccess(const pParam: MFINPUTTRUSTAUTHORITY_ACCESS_PARAMS): HResult; stdcall;
    function Reset(): HResult; stdcall;
  end;

  //Interface IMFTrustedOutput
  IMFTrustedOutput = interface(IUnknown)
	['{D19F8E95-B126-4446-890C-5DCB7AD71453}']
    function GetOutputTrustAuthorityCount(out pcOutputTrustAuthorities: DWord): HResult; stdcall;
    function GetOutputTrustAuthorityByIndex(const dwIndex: Dword; out ppauthority: IMFOutputTrustAuthority): HResult; stdcall;
    function IsFinal(out pfIsFinal: Boolean): HResult; stdcall;
  end;

  //Interface IMFOutputPolicy
  IMFOutputPolicy = interface(IUnknown)
	['{7F00F10A-DAED-41AF-AB26-5FDFA4DFBA3C}']
    function GenerateRequiredSchemas(const dwAttributes: DWord; const guidOutputSubType: TGuid;
                                     const rgGuidProtectionSchemasSupported: TGuid; const cProtectionSchemasSupported: DWord;
                                     out ppRequiredProtectionSchemas: IMFCollection): HResult; stdcall;
    function GetOriginatorID(out pguidOriginatorID: TGuid): HResult; stdcall;
    function GetMinimumGRLVersion(out pdwMinimumGRLVersion: DWord): HResult; stdcall;
  end;

  //Interface IMFOutputSchema
  IMFOutputSchema = interface(IUnknown)
	['{7BE0FC5B-ABD9-44FB-A5C8-F50136E71599}']
    function GetSchemaType(out pguidSchemaType: TGuid): HResult; stdcall;
    function GetConfigurationData(out pdwVal: DWord): HResult; stdcall;
    function GetOriginatorID(out pguidOriginatorID: TGuid): HResult; stdcall;
  end;

  //Interface IMFSecureChannel
  IMFSecureChannel = interface(IUnknown)
	['{d0ae555d-3b12-4d97-b060-0990bc5aeb67}']
    function GetCertificate(out ppCert: Byte; out pcbCert: DWord): HResult; stdcall;
    function SetupSession(const pbEncryptedSessionKey: Byte; const cbSessionKey: DWord): HResult; stdcall;
   end;

  //Interface IMFSampleProtection
  IMFSampleProtection = interface(IUnknown)
	['{8e36395f-c7b9-43c4-a54d-512b4af63c95}']
    function GetInputProtectionVersion(out pdwVersion: DWord): HResult; stdcall;
    function GetOutputProtectionVersion(out pdwVersion: DWord): HResult; stdcall;
    function GetProtectionCertificate(const dwVersion: DWord; out ppCert: Byte; out pcbCert: DWord): HResult; stdcall;
    function InitOutputProtection(const dwVersion: DWord; const dwOutputId: DWord; const pbCert: Byte; const cbCert: DWord;
                                  out ppbSeed: Byte; out pcbSeed: DWord): HResult; stdcall;
    function InitInputProtection(const dwVersion: DWord; const dwInputId: DWord; const pbSeed: Byte; const cbSeed: DWord): HResult; stdcall;
  end;

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  //Interface IMFFinalizableMediaSink
  IMFFinalizableMediaSink = interface(IUnknown)
	['{EAECB74A-9A50-42ce-9541-6A7F57AA4AD7}']
    function BeginFinalize(const pCallback: IMFAsyncCallback; const punkState: IUnknown): HResult; stdcall;
    function EndFinalize(const pResult: IMFAsyncResult): HResult; stdcall;
  end;
  //#endif // (WINVER >= _WIN32_WINNT_WIN7)

  //Interface IMFRemoteProxy
  IMFRemoteProxy = interface(IUnknown)
	['{994e23ad-1cc2-493c-b9fa-46f1cb040fa4}']
    function GetRemoteObject(const riid: REFIID; out ppv: Pointer): HResult; stdcall;
    function GetRemoteHost(const riid: REFIID; out ppv: Pointer): HResult; stdcall;
  end;

  //Interface IMFPMPHost
  IMFPMPHost = interface(IUnknown)
	['{F70CA1A9-FDC7-4782-B994-ADFFB1C98606}']
    function LockProcess(): HResult; stdcall;
    function UnlockProcess(): HResult; stdcall;
  end;

  //Interface IMFPMPClient
  IMFPMPClient = interface(IUnknown)
	['{6C4E655D-EAD8-4421-B6B9-54DCDBBDF820}']
    function SetPMPHost(const pPMPHost: IMFPMPHost): HResult; stdcall;
  end;

  //Interface IMFPMPServer
  IMFPMPServer = interface(IUnknown)
	['{994e23af-1cc2-493c-b9fa-46f1cb040fa4}']
    function LockProcess(): HResult; stdcall;
    function UnlockProcess(): HResult; stdcall;
    function CreateObjectByCLSID(const clsid: REFCLSID; const riid: REFIID; out ppObject: Pointer): HResult; stdcall;
  end;

  //Interface IMFSAMIStyle
  IMFSAMIStyle = interface(IUnknown)
	['{A7E025DD-5303-4a62-89D6-E747E1EFAC73}']
    function GetStyleCount(out pdwCount: DWord): HResult; stdcall;
    function GetStyles(out pPropVarStyleArray: PROPVARIANT): HResult; stdcall;
    function SetSelectedStyle(const pwszStyle: LPCWSTR): HResult; stdcall;
    function GetSelectedStyle(out ppwszStyle: LPWSTR): HResult; stdcall;
  end;

  //#if (WINVER >= _WIN32_WINNT_WIN7)
  // >= Windows 7
  //Interface IMFTranscodeProfile
  IMFTranscodeProfile = interface(IUnknown)
	['{4ADFDBA3-7AB0-4953-A62B-461E7FF3DA1E}']
    function SetAudioAttributes(const pAttrs: IMFAttributes): HResult; stdcall;
    function GetAudioAttributes(out ppAttrs: IMFAttributes): HResult; stdcall;
    function SetVideoAttributes(const pAttrs: IMFAttributes): HResult; stdcall;
    function GetVideoAttributes(out ppAttrs: IMFAttributes): HResult; stdcall;
    function SetContainerAttributes(const pAttrs: IMFAttributes): HResult; stdcall;
    function GetContainerAttributes(out ppAttrs: IMFAttributes): HResult; stdcall;
  end;

  //Interface IMFTranscodeSinkInfoProvider
  IMFTranscodeSinkInfoProvider = interface(IUnknown)
	['{8CFFCD2E-5A03-4a3a-AFF7-EDCD107C620E}']
    function SetOutputFile(const pwszFileName: LPCWSTR): HResult; stdcall;
    function SetOutputByteStream(const pByteStreamActivate: IMFActivate): HResult; stdcall;
    function SetProfile(const pProfile: IMFTranscodeProfile): HResult; stdcall;
    function GetSinkInfo(out pSinkInfo: MF_TRANSCODE_SINK_INFO): HResult; stdcall;
  end;

  //Interface IMFFieldOfUseMFTUnlock
  IMFFieldOfUseMFTUnlock = interface(IUnknown)
	['{508E71D3-EC66-4fc3-8775-B4B9ED6BA847}']
    function Unlock(const pUnkMFT: IUnknown): HResult; stdcall;
  end;

  //Interface IMFLocalMFTRegistration
  IMFLocalMFTRegistration = interface(IUnknown)
	['{149c4d73-b4be-4f8d-8b87-079e926b6add}']
    function RegisterMFTs(const pMFTs: MFT_REGISTRATION_INFO; cMFTs: DWord): HResult; stdcall;
  end;

//OTHER FUNCTIONS

  function CreatePropertyStore(out ppStore: IPropertyStore): HResult; stdcall;
  function MFGetSupportedSchemes(out pPropVarSchemeArray: PROPVARIANT): HResult; stdcall;
  function MFGetSupportedMimeTypes(out pPropVarMimeTypeArray: PROPVARIANT): HResult; stdcall;
  function MFCreateSequencerSource(const pReserved: IUnknown; out ppSequencerSource: IMFSequencerSource): HResult; stdcall;
  function MFCreateSequencerSegmentOffset(const dwId: MFSequencerElementId; const hnsOffset: MFTIME; out pvarSegmentOffset: PROPVARIANT): HResult; stdcall;
  function MFGetTopoNodeCurrentType(const pNode: IMFTopologyNode; const dwStreamIndex: DWord; const fOutput: Bool; out ppType: IMFMediaType): HResult; stdcall;
  //todo: check: cStreamDescriptors
  function MFCreatePresentationDescriptor(const cStreamDescriptors: Dword; apStreamDescriptors: IMFStreamDescriptor; out ppPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall;
  function MFRequireProtectedEnvironment(const pPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall;
  //todo: find solution pcbData > ppbData
  function MFSerializePresentationDescriptor(const pPD: IMFPresentationDescriptor; out pcbData: Dword; out ppbData {with size of pcbData}: Byte): HResult; stdcall;
  //todo: find solution cbData > pbData
  function MFDeserializePresentationDescriptor(const cbData: Dword; const pbData: Byte; out ppPD: IMFPresentationDescriptor): HResult; stdcall;
  //todo: cMediaTypes count
  function MFCreateStreamDescriptor(const dwStreamIdentifier: DWord; const cMediaTypes: DWord; const apMediaTypes: IMFMediaType; out ppDescriptor: IMFStreamDescriptor): HResult; stdcall;
  function MFCreateSimpleTypeHandler(out ppHandler: IMFMediaTypeHandler): HResult; stdcall;
  function MFCreateMPEG4MediaSink(const pIByteStream: IMFByteStream; const pVideoMediaType: IMFMediaType; const pAudioMediaType: IMFMediaType; out ppIMediaSink: IMFMediaSink): HResult; stdcall;
  function MFCreate3GPMediaSink(const pIByteStream: IMFByteStream; const pVideoMediaType: IMFMediaType; const pAudioMediaType: IMFMediaType; out ppIMediaSink: IMFMediaSink): HResult; stdcall;
  function MFCreateMP3MediaSink(const pTargetByteStream: IMFByteStream; out ppMediaSink: IMFMediaSink): HResult; stdcall;
  function MFCreateSampleGrabberSinkActivate(const pIMFMediaType: IMFMediaType; const pIMFSampleGrabberSinkCallback: IMFSampleGrabberSinkCallback; out ppIActivate: IMFActivate): HResult; stdcall;
  function MFCreateStandardQualityManager(out ppQualityManager: IMFQualityManager): HResult; stdcall;
  function MFCreateAggregateSource(const pSourceCollection: IMFCollection; out ppAggSource: IMFMediaSource): HResult; stdcall;
  function MFCreateCredentialCache(out ppCache: IMFNetCredentialCache): HResult; stdcall;
//  function MFCreateNetSchemePlugin(const riid: REFIID; ppvHandler: Pointer): HResult; stdcall;  //todo: check
  function MFCreateRemoteDesktopPlugin(out ppPlugin: IMFRemoteDesktopPlugin): HResult; stdcall;
  //function CreateNamedPropertyStore(out ppStore: INamedPropertyStore): HResult; stdcall;
  function MFCreateSampleCopierMFT(out ppCopierMFT: IMFTransform): HResult; stdcall;
  function MFCreateTranscodeProfile(out ppTranscodeProfile: IMFTranscodeProfile): HResult; stdcall;
  function MFCreateTranscodeTopology(const pSrc: IMFMediaSource; const pwszOutputFilePath: LPCWSTR;
                                     const pProfile: IMFTranscodeProfile; out ppTranscodeTopo: IMFTopology): HResult; stdcall;
  function MFTranscodeGetAudioOutputAvailableTypes(const guidSubType: REFGUID; const dwMFTFlags: DWord; const pCodecConfig: IMFAttributes;
                                                   out ppAvailableTypes: IMFCollection): HResult; stdcall;
  function MFCreateTranscodeSinkActivate(out ppActivate: IMFActivate): HResult; stdcall;
  function MFCreateMFByteStreamOnStream(const pStream: IStream): HResult; stdcall;
  function MFEnumDeviceSources(const pAttributes: IMFAttributes; out pppSourceActivate: IMFActivate; out pcSourceActivate: UINT32): HResult; stdcall;
  function MFCreateDeviceSource(const pAttributes: IMFAttributes; out ppSource: IMFMediaSource): HResult; stdcall;
  function MFCreateDeviceSourceActivate(const pAttributes: IMFAttributes; out ppActivate: IMFActivate): HResult; stdcall;


//--------------------- Helper functions -------------------------------------

  //Creates the Media Session in the application's process.
  //If your application does not play protected content, you can use this function to create the Media Session in
  //the application's process. To use the Media Session for protected content, you must call MFCreatePMPMediaSession.
  //pConfiguration > Pointer to the IMFAttributes interface. This parameter can be NIL.
  function MFCreateMediaSession(const pConfiguration: IMFAttributes; out ppMediaSession: IMFMediaSession): HResult; stdcall;
  function MFCreateSourceResolver(out ppISourceResolver: IMFSourceResolver): HResult; stdcall;
  function MFCreateTopologyNode(const NodeType: MF_TOPOLOGY_TYPE; out ppNode: IMFTopologyNode): HResult; stdcall;
  function MFCreateTopology(out ppTopo: IMFTopology): HResult; stdcall;
  function MFCreateTopoLoader(out ppObj: IMFTopoLoader): HResult; stdcall;
  function MFShutdownObject(const pUnk: IUnknown): HResult; stdcall;
  function MFCreateAudioRenderer(const pAudioAttributes: IMFAttributes; out ppSink: IMFMediaSink): HResult; stdcall;
  function MFCreateAudioRendererActivate(out ppActivate: IMFActivate): HResult; stdcall;
  function MFCreateVideoRendererActivate(const hwndVideo: HWND; out ppActivate: IMFActivate): HResult; stdcall;
  function MFGetService(const punkObject: IUnknown; const guidService: tGUID; const riid: tGUID; out ppvObject: IUnknown): HResult; stdcall;
  function MFCreatePresentationClock(out ppPresentationClock: IMFPresentationClock): HResult; stdcall;
  function MFCreateSystemTimeSource(out ppSystemTimeSource: IMFPresentationTimeSource): HResult; stdcall;
  function MFCreatePMPMediaSession(const dwCreationFlags: DWORD;
                                   const pConfiguration: IMFAttributes;
                                   out ppMediaSession: IMFMediaSession;
                                   out ppEnablerActivate: IMFActivate): HResult; stdcall;

implementation

  function MFCreateMediaSession(const pConfiguration: IMFAttributes; out ppMediaSession: IMFMediaSession): HResult; stdcall;   external 'Mf.dll' name 'MFCreateMediaSession';
  function MFCreatePMPMediaSession(const dwCreationFlags: DWORD; const pConfiguration: IMFAttributes; out ppMediaSession: IMFMediaSession; out ppEnablerActivate: IMFActivate): HResult; stdcall;       external 'Mf.dll' name 'MFCreatePMPMediaSession';
  function MFCreateSourceResolver(out ppISourceResolver: IMFSourceResolver): HResult; stdcall;        external 'Mf.dll' name 'MFCreateSourceResolver';
  function MFCreateTopologyNode(const NodeType: MF_TOPOLOGY_TYPE; out ppNode: IMFTopologyNode): HResult; stdcall;          external 'Mf.dll' name 'MFCreateTopologyNode';
  function MFCreateTopology(out ppTopo: IMFTopology): HResult; stdcall;              external 'Mf.dll' name 'MFCreateTopology';
  function MFCreateTopoLoader(out ppObj: IMFTopoLoader): HResult; stdcall;                 external 'Mf.dll' name 'MFCreateTopoLoader';
  function MFShutdownObject(const pUnk: IUnknown): HResult; stdcall;             external 'Mf.dll' name 'MFShutdownObject';
  function MFCreateAudioRenderer(const pAudioAttributes: IMFAttributes; out ppSink: IMFMediaSink): HResult; stdcall;          external 'Mf.dll' name 'MFCreateAudioRenderer';
  function MFCreateVideoRendererActivate(const hwndVideo: HWND; out ppActivate: IMFActivate): HResult; stdcall; external 'Mf.dll' name 'MFCreateVideoRendererActivate';
  function MFCreateAudioRendererActivate(out ppActivate: IMFActivate): HResult; stdcall;  external 'Mf.dll' name 'MFCreateAudioRendererActivate';
  function MFGetService(const punkObject: IUnknown; const guidService: tGUID; const riid: tGUID; out ppvObject: IUnknown): HResult; stdcall;                 external 'Mf.dll' name 'MFGetService';
  function MFCreatePresentationClock(out ppPresentationClock: IMFPresentationClock): HResult; stdcall;     external 'Mf.dll' name 'MFCreatePresentationClock';
  function MFCreateSystemTimeSource(out ppSystemTimeSource: IMFPresentationTimeSource): HResult; stdcall;     external 'Mf.dll' name 'MFCreateSystemTimeSource';


  function CreatePropertyStore(out ppStore: IPropertyStore): HResult; stdcall; external 'Mf.dll' name 'CreatePropertyStore';
  function MFGetSupportedSchemes(out pPropVarSchemeArray: PROPVARIANT): HResult; stdcall; external 'Mf.dll' name 'MFGetSupportedSchemes';
  function MFGetSupportedMimeTypes(out pPropVarMimeTypeArray: PROPVARIANT): HResult; stdcall; external 'Mf.dll' name 'MFGetSupportedSchemes';
  function MFCreateSequencerSource(const pReserved: IUnknown; out ppSequencerSource: IMFSequencerSource): HResult; stdcall; external 'Mf.dll' name 'MFCreateSequencerSource';
  function MFCreateSequencerSegmentOffset(const dwId: MFSequencerElementId; const hnsOffset: MFTIME; out pvarSegmentOffset: PROPVARIANT): HResult; stdcall; external 'Mf.dll' name 'MFCreateSequencerSegmentOffset';
  function MFGetTopoNodeCurrentType(const pNode: IMFTopologyNode; const dwStreamIndex: DWord; const fOutput: Bool; out ppType: IMFMediaType): HResult; stdcall; external 'Mf.dll' name 'MFGetTopoNodeCurrentType';
  function MFCreatePresentationDescriptor(const cStreamDescriptors: Dword; apStreamDescriptors: IMFStreamDescriptor; out ppPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall; external 'Mf.dll' name 'MFCreatePresentationDescriptor';
  function MFRequireProtectedEnvironment(const pPresentationDescriptor: IMFPresentationDescriptor): HResult; stdcall; external 'Mf.dll' name 'MFRequireProtectedEnvironment';
  function MFSerializePresentationDescriptor(const pPD: IMFPresentationDescriptor; out pcbData: Dword; out ppbData {with size of pcbData}: Byte): HResult; stdcall; external 'Mf.dll' name 'MFSerializePresentationDescriptor';
  function MFDeserializePresentationDescriptor(const cbData: Dword; const pbData: Byte; out ppPD: IMFPresentationDescriptor): HResult; stdcall; external 'Mf.dll' name 'MFDeserializePresentationDescriptor';
  function MFCreateStreamDescriptor(const dwStreamIdentifier: DWord; const cMediaTypes: DWord; const apMediaTypes: IMFMediaType; out ppDescriptor: IMFStreamDescriptor): HResult; stdcall; external 'Mf.dll' name 'MFCreateStreamDescriptor';
  function MFCreateSimpleTypeHandler(out ppHandler: IMFMediaTypeHandler): HResult; stdcall; external 'Mf.dll' name 'MFCreateSimpleTypeHandler';
  function MFCreateMPEG4MediaSink(const pIByteStream: IMFByteStream; const pVideoMediaType: IMFMediaType; const pAudioMediaType: IMFMediaType; out ppIMediaSink: IMFMediaSink): HResult; stdcall; external 'Mf.dll' name 'MFCreateMPEG4MediaSink';
  function MFCreate3GPMediaSink(const pIByteStream: IMFByteStream; const pVideoMediaType: IMFMediaType; const pAudioMediaType: IMFMediaType; out ppIMediaSink: IMFMediaSink): HResult; stdcall; external 'Mf.dll' name 'MFCreate3GPMediaSink';
  function MFCreateMP3MediaSink(const pTargetByteStream: IMFByteStream; out ppMediaSink: IMFMediaSink): HResult; stdcall; external 'Mf.dll' name 'MFCreateMP3MediaSink';
  function MFCreateSampleGrabberSinkActivate(const pIMFMediaType: IMFMediaType; const pIMFSampleGrabberSinkCallback: IMFSampleGrabberSinkCallback; out ppIActivate: IMFActivate): HResult; stdcall; external 'Mf.dll' name 'MFCreateSampleGrabberSinkActivate';
  function MFCreateStandardQualityManager(out ppQualityManager: IMFQualityManager): HResult; stdcall; external 'Mf.dll' name 'MFCreateSampleGrabberSinkActivate';
  function MFCreateAggregateSource(const pSourceCollection: IMFCollection; out ppAggSource: IMFMediaSource): HResult; stdcall; external 'Mf.dll' name 'MFCreateAggregateSource';
  function MFCreateCredentialCache(out ppCache: IMFNetCredentialCache): HResult; stdcall; external 'Mf.dll' name 'MFCreateCredentialCache';
  //todo: function MFCreateNetSchemePlugin(const riid: REFIID; ppvHandler: Pointer): HResult; stdcall;  check external 'Mf.dll' name 'MFCreateNetSchemePlugin';
  function MFCreateRemoteDesktopPlugin(out ppPlugin: IMFRemoteDesktopPlugin): HResult; stdcall; external 'Mf.dll' name 'MFCreateRemoteDesktopPlugin';
  // function CreateNamedPropertyStore(out ppStore: INamedPropertyStore): HResult; stdcall; external 'Mf.dll' name 'CreateNamedPropertyStore';
  function MFCreateSampleCopierMFT(out ppCopierMFT: IMFTransform): HResult; stdcall; external 'Mf.dll' name 'MFCreateSampleCopierMFT';
  function MFCreateTranscodeProfile(out ppTranscodeProfile: IMFTranscodeProfile): HResult; stdcall; external 'Mf.dll' name 'MFCreateTranscodeProfile';
  function MFCreateTranscodeTopology(const pSrc: IMFMediaSource; const pwszOutputFilePath: LPCWSTR; const pProfile: IMFTranscodeProfile; out ppTranscodeTopo: IMFTopology): HResult; stdcall; external 'Mf.dll' name 'MFCreateTranscodeTopology';
  function MFTranscodeGetAudioOutputAvailableTypes(const guidSubType: REFGUID; const dwMFTFlags: DWord; const pCodecConfig: IMFAttributes; out ppAvailableTypes: IMFCollection): HResult; stdcall; external 'Mf.dll' name 'MFTranscodeGetAudioOutputAvailableTypes';
  function MFCreateTranscodeSinkActivate(out ppActivate: IMFActivate): HResult; stdcall; external 'Mf.dll' name 'MFCreateTranscodeSinkActivate';
  function MFCreateMFByteStreamOnStream(const pStream: IStream): HResult; stdcall; external 'Mf.dll' name 'MFCreateMFByteStreamOnStream';
  function MFEnumDeviceSources(const pAttributes: IMFAttributes; out pppSourceActivate: IMFActivate; out pcSourceActivate: UINT32): HResult; stdcall; external 'Mf.dll' name 'MFEnumDeviceSources';
  function MFCreateDeviceSource(const pAttributes: IMFAttributes; out ppSource: IMFMediaSource): HResult; stdcall; external 'Mf.dll' name 'MFCreateDeviceSource';
  function MFCreateDeviceSourceActivate(const pAttributes: IMFAttributes; out ppActivate: IMFActivate): HResult; stdcall; external 'Mf.dll' name 'MFCreateDeviceSourceActivate';

{$ELSE}
implementation
{$ENDIF}

end.
