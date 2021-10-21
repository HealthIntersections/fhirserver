// FactoryX
//
// Copyright ©2003 - 2012 by FactoryX, Sefferweich Germany (EU)
// Project: Media Foundation - MFPack
// Module: Media Foundation interfaces - MmReg.pas
// Kind: Pascal Unit
// Release date: 27-06-2012
// Language: ENU
//
// Version: 1.0.0.2 
// Description: Requires Windows Vista or later. 
// 
// Intiator(s): Tony (maXcomX), Peter (OzShips) 
// 
// LastEdited by: Tony
// EditDate: updt 270612a
//
// Remarks:
//          Delphi : The IUnknown entries of functions should be casted like this:
//                   IUnknown(Pointer), IUnknown(Object), IUnknown(Nil) etc.
//
// Related objects: -
// Related projects: -
// Known Issues: -
// Compiler version: 23, upd 4
// Todo: Check some Guids (see: Todo comments)
// =============================================================================
// Source: mmreg.h
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

//==========================================================================
//
// Copyright (C) Microsoft Corporation, 1991 - 1997
//
// Module Name:
//
//     mmreg.h
//
// Abstract:
//
//     Multimedia Registration
//
// Revision History:
//
//==========================================================================
//
// Translated to Delphi 1998-03-05 by Volker Siebert, Unna, Germany.
// Partially upgraded to current version 2005-09-09 by Volker Siebert, Unna, Germany.
//
//==========================================================================

unit MmReg;

interface

uses
  SysUtils, Windows, Messages, MMSystem {, ActiveX};

//***************************************************************************
//  MMREG.H (standard include file for MM defines, like FOURCC and things)
//***************************************************************************


const
  _INC_MMREG = 158;      // version * 100 + revision

const
  // manufacturer IDs
  MM_MICROSOFT                  =  1;  //  Microsoft Corporation
  MM_CREATIVE                   =  2;  //  Creative Labs, Inc
  MM_MEDIAVISION                =  3;  //  Media Vision, Inc.
  MM_FUJITSU                    =  4;  //  Fujitsu Corp.
  MM_PRAGMATRAX                 =  5;  // PRAGMATRAX Software
  MM_CYRIX                      =  6;  // Cyrix Corporation
  MM_PHILIPS_SPEECH_PROCESSING  =  7;  // Philips Speech Processing
  MM_NETXL                      =  8;  // NetXL, Inc.
  MM_ZYXEL                      =  9;  // ZyXEL Communications, Inc.
  MM_BECUBED                    = 10;  // BeCubed Software Inc.
  MM_AARDVARK                   = 11;  // Aardvark Computer Systems, Inc.
  MM_BINTEC                     = 12;  // Bin Tec Communications GmbH
  MM_HEWLETT_PACKARD            = 13;  // Hewlett-Packard Company
  MM_ACULAB                     = 14;  // Aculab plc
  MM_FAITH                      = 15;  // Faith,Inc.
  MM_MITEL                      = 16;  // Mitel Corporation
  MM_QUANTUM3D                  = 17;  // Quantum3D, Inc.
  MM_SNI                        = 18;  // Siemens-Nixdorf
  MM_EMU                        = 19;  // E-mu Systems, Inc.
  MM_ARTISOFT                   = 20;  //  Artisoft, Inc.
  MM_TURTLE_BEACH               = 21;  //  Turtle Beach, Inc.
  MM_IBM                        = 22;  //  IBM Corporation
  MM_VOCALTEC                   = 23;  //  Vocaltec LTD.
  MM_ROLAND                     = 24;  //  Roland
  MM_DSP_SOLUTIONS              = 25;  //  DSP Solutions, Inc.
  MM_NEC                        = 26;  //  NEC
  MM_ATI                        = 27;  //  ATI
  MM_WANGLABS                   = 28;  //  Wang Laboratories, Inc
  MM_TANDY                      = 29;  //  Tandy Corporation
  MM_VOYETRA                    = 30;  //  Voyetra
  MM_ANTEX                      = 31;  //  Antex Electronics Corporation
  MM_ICL_PS                     = 32;  //  ICL Personal Systems
  MM_INTEL                      = 33;  //  Intel Corporation
  MM_GRAVIS                     = 34;  //  Advanced Gravis
  MM_VAL                        = 35;  //  Video Associates Labs, Inc.
  MM_INTERACTIVE                = 36;  //  InterActive Inc
  MM_YAMAHA                     = 37;  //  Yamaha Corporation of America
  MM_EVEREX                     = 38;  //  Everex Systems, Inc
  MM_ECHO                       = 39;  //  Echo Speech Corporation
  MM_SIERRA                     = 40;  //  Sierra Semiconductor Corp
  MM_CAT                        = 41;  //  Computer Aided Technologies
  MM_APPS                       = 42;  //  APPS Software International
  MM_DSP_GROUP                  = 43;  //  DSP Group, Inc
  MM_MELABS                     = 44;  //  microEngineering Labs
  MM_COMPUTER_FRIENDS           = 45;  //  Computer Friends, Inc.
  MM_ESS                        = 46;  //  ESS Technology
  MM_AUDIOFILE                  = 47;  //  Audio, Inc.
  MM_MOTOROLA                   = 48;  //  Motorola, Inc.
  MM_CANOPUS                    = 49;  //  Canopus, co., Ltd.
  MM_EPSON                      = 50;  //  Seiko Epson Corporation
  MM_TRUEVISION                 = 51;  //  Truevision
  MM_AZTECH                     = 52;  //  Aztech Labs, Inc.
  MM_VIDEOLOGIC                 = 53;  //  Videologic
  MM_SCALACS                    = 54;  //  SCALACS
  MM_KORG                       = 55;  //  Toshihiko Okuhura, Korg Inc.
  MM_APT                        = 56;  //  Audio Processing Technology
  MM_ICS                        = 57;  //  Integrated Circuit Systems, Inc.
  MM_ITERATEDSYS                = 58;  //  Iterated Systems, Inc.
  MM_METHEUS                    = 59;  //  Metheus
  MM_LOGITECH                   = 60;  //  Logitech, Inc.
  MM_WINNOV                     = 61;  //  Winnov, Inc.
  MM_NCR                        = 62;  //  NCR Corporation
  MM_EXAN                       = 63;  //  EXAN
  MM_AST                        = 64;  //  AST Research Inc.
  MM_WILLOWPOND                 = 65;  //  Willow Pond Corporation
  MM_SONICFOUNDRY               = 66;  //  Sonic Foundry
  MM_VITEC                      = 67;  //  Vitec Multimedia
  MM_MOSCOM                     = 68;  //  MOSCOM Corporation
  MM_SILICONSOFT                = 69;  //  Silicon Soft, Inc.
  MM_TERRATEC                   = 70;  //  TerraTec Electronic GmbH
  MM_MEDIASONIC                 = 71;  //  MediaSonic Ltd.
  MM_SANYO                      = 72;  //  SANYO Electric Co., Ltd.
  MM_SUPERMAC                   = 73;  //  Supermac
  MM_AUDIOPT                    = 74;  //  Audio Processing Technology
  MM_NOGATECH                   = 75;  // NOGATECH Ltd.
  MM_SPEECHCOMP                 = 76;  //  Speech Compression
  MM_DOLBY                      = 78;  //  Dolby Laboratories
  MM_OKI                        = 79;  //  OKI
  MM_AURAVISION                 = 80;  //  AuraVision Corporation
  MM_OLIVETTI                   = 81;  //  Olivetti
  MM_IOMAGIC                    = 82;  //  I/O Magic Corporation
  MM_MATSUSHITA                 = 83;  //  Matsushita Electric Industrial Co., LTD.
  MM_CONTROLRES                 = 84;  //  Control Resources Limited
  MM_XEBEC                      = 85;  //  Xebec Multimedia Solutions Limited
  MM_NEWMEDIA                   = 86;  //  New Media Corporation
  MM_NMS                        = 87;  //  Natural MicroSystems
  MM_LYRRUS                     = 88;  //  Lyrrus Inc.
  MM_COMPUSIC                   = 89;  //  Compusic
  MM_OPTI                       = 90;  //  OPTi Computers Inc.
  MM_DIALOGIC                   = 93;  //  Dialogic Corporation
  MM_INSOFT                     = 94;  //  InSoft, Inc.
  MM_MPTUS                      = 95;  //  M.P. Technologies, Inc.
  MM_WEITEK                     = 96;  //  Weitek
  MM_LERNOUT_AND_HAUSPIE        = 97;  //  Lernout & Hauspie
  MM_QCIAR                      = 98;  //  Quanta Computer Inc.
  MM_APPLE                      = 99;  //  Apple Computer, Inc.
  MM_DIGITAL                    = 100; //  Digital Equipment Corporation
  MM_MOTU                       = 101; //  Mark of the Unicorn
  MM_WORKBIT                    = 102; //  Workbit Corporation
  MM_OSITECH                    = 103; //  Ositech Communications Inc.
  MM_MIRO                       = 104; //  miro Computer Products AG
  MM_CIRRUSLOGIC                = 105; //  Cirrus Logic
  MM_ISOLUTION                  = 106; //  ISOLUTION  B.V.
  MM_HORIZONS                   = 107; //  Horizons Technology, Inc.
  MM_CONCEPTS                   = 108; //  Computer Concepts Ltd.
  MM_VTG                        = 109; //  Voice Technologies Group, Inc.
  MM_RADIUS                     = 110; //  Radius
  MM_ROCKWELL                   = 111; //  Rockwell International
  MM_XYZ                        = 112; //  Co. XYZ for testing
  MM_OPCODE                     = 113; //  Opcode Systems
  MM_VOXWARE                    = 114; //  Voxware Inc.
  MM_NORTHERN_TELECOM           = 115; //  Northern Telecom Limited
  MM_APICOM                     = 116; //  APICOM
  MM_GRANDE                     = 117; //  Grande Software
  MM_ADDX                       = 118; //  ADDX
  MM_WILDCAT                    = 119; //  Wildcat Canyon Software
  MM_RHETOREX                   = 120; //  Rhetorex Inc.
  MM_BROOKTREE                  = 121; //  Brooktree Corporation
  MM_ENSONIQ                    = 125; //  ENSONIQ Corporation
  MM_FAST                       = 126; //  FAST Multimedia AG
  MM_NVIDIA                     = 127; //  NVidia Corporation
  MM_OKSORI                     = 128; //  OKSORI Co., Ltd.
  MM_DIACOUSTICS                = 129; //  DiAcoustics, Inc.
  MM_GULBRANSEN                 = 130; //  Gulbransen, Inc.
  MM_KAY_ELEMETRICS             = 131; //  Kay Elemetrics, Inc.
  MM_CRYSTAL                    = 132; //  Crystal Semiconductor Corporation
  MM_SPLASH_STUDIOS             = 133; //  Splash Studios
  MM_QUARTERDECK                = 134; //  Quarterdeck Corporation
  MM_TDK                        = 135; //  TDK Corporation
  MM_DIGITAL_AUDIO_LABS         = 136; //  Digital Audio Labs, Inc.
  MM_SEERSYS                    = 137; //  Seer Systems, Inc.
  MM_PICTURETEL                 = 138; //  PictureTel Corporation
  MM_ATT_MICROELECTRONICS       = 139; //  AT&T Microelectronics
  MM_OSPREY                     = 140; //  Osprey Technologies, Inc.
  MM_MEDIATRIX                  = 141; //  Mediatrix Peripherals
  MM_SOUNDESIGNS                = 142; //  SounDesignS M.C.S. Ltd.
  MM_ALDIGITAL                  = 143; //  A.L. Digital Ltd.
  MM_SPECTRUM_SIGNAL_PROCESSING = 144; //  Spectrum Signal Processing, Inc.
  MM_ECS                        = 145; //  Electronic Courseware Systems, Inc.
  MM_AMD                        = 146; //  AMD
  MM_COREDYNAMICS               = 147; //  Core Dynamics
  MM_CANAM                      = 148; //  CANAM Computers
  MM_SOFTSOUND                  = 149; //  Softsound, Ltd.
  MM_NORRIS                     = 150; //  Norris Communications, Inc.
  MM_DDD                        = 151; //  Danka Data Devices
  MM_EUPHONICS                  = 152; //  EuPhonics
  MM_PRECEPT                    = 153; //  Precept Software, Inc.
  MM_CRYSTAL_NET                = 154; //  Crystal Net Corporation
  MM_CHROMATIC                  = 155; //  Chromatic Research, Inc.
  MM_VOICEINFO                  = 156; //  Voice Information Systems, Inc.
  MM_VIENNASYS                  = 157; //  Vienna Systems
  MM_CONNECTIX                  = 158; //  Connectix Corporation
  MM_GADGETLABS                 = 159; //  Gadget Labs LLC
  MM_FRONTIER                   = 160; //  Frontier Design Group LLC
  MM_VIONA                      = 161; //  Viona Development GmbH
  MM_CASIO                      = 162; //  Casio Computer Co., LTD
  MM_DIAMONDMM                  = 163; //  Diamond Multimedia
  MM_S3                         = 164; //  S3
  MM_DVISION                    = 165; //  D-Vision Systems, Inc.
  MM_NETSCAPE                   = 166; //  Netscape Communications
  MM_SOUNDSPACE                 = 167; //  Soundspace Audio
  MM_VANKOEVERING               = 168; //  VanKoevering Company
  MM_QTEAM                      = 169; //  Q-Team
  MM_ZEFIRO                     = 170; //  Zefiro Acoustics
  MM_STUDER                     = 171; //  Studer Professional Audio AG
  MM_FRAUNHOFER_IIS             = 172; //  Fraunhofer IIS
  MM_QUICKNET                   = 173; //  Quicknet Technologies
  MM_ALARIS                     = 174; //  Alaris, Inc.
  MM_SICRESOURCE                = 175; //  SIC Resource Inc.
  MM_NEOMAGIC                   = 176; //  NeoMagic Corporation
  MM_MERGING_TECHNOLOGIES       = 177; //  Merging Technologies S.A.
  MM_XIRLINK                    = 178; //  Xirlink, Inc.
  MM_COLORGRAPH                 = 179; //  Colorgraph (UK) Ltd
  MM_OTI                        = 180; //  Oak Technology, Inc.
  MM_AUREAL                     = 181; //  Aureal Semiconductor
  MM_VIVO                       = 182; //  Vivo Software
  MM_SHARP                      = 183; //  Sharp
  MM_LUCENT                     = 184; //  Lucent Technologies
  MM_ATT                        = 185; //  AT&T Labs, Inc.
  MM_SUNCOM                     = 186; //  Sun Communications, Inc.
  MM_SORVIS                     = 187; //  Sorenson Vision
  MM_INVISION                   = 188; //  InVision Interactive
  MM_BERKOM                     = 189; //  Deutsche Telekom Berkom GmbH
  MM_MARIAN                     = 190; //  Marian GbR Leipzig
  MM_DPSINC                     = 191; //  Digital Processing Systems, Inc.
  MM_BCB                        = 192; //  BCB Holdings Inc.
  MM_MOTIONPIXELS               = 193; //  Motion Pixels
  MM_QDESIGN                    = 194; //  QDesign Corporation
  MM_NMP                        = 195; //  Nokia Mobile Phones
  MM_DATAFUSION                 = 196; //  DataFusion Systems (Pty) (Ltd)
  MM_DUCK                       = 197; //  The Duck Corporation
  MM_FTR                        = 198; //  Future Technology Resources Pty Ltd
  MM_BERCOS                     = 199; //  BERCOS GmbH
  MM_ONLIVE                     = 200; //  OnLive! Technologies, Inc.
  MM_SIEMENS_SBC                = 201; //  Siemens Business Communications Systems
  MM_TERALOGIC                  = 202; //  TeraLogic, Inc.
  MM_PHONET                     = 203; //  PhoNet Communications Ltd.
  MM_WINBOND                    = 204; //  Winbond Electronics Corp
  MM_VIRTUALMUSIC               = 205; //  Virtual Music, Inc.
  MM_ENET                       = 206; //  e-Net, Inc.
  MM_GUILLEMOT                  = 207; //  Guillemot International
  MM_EMAGIC                     = 208; //  Emagic Soft- und Hardware GmbH
  MM_MWM                        = 209; //  MWM Acoustics LLC
  MM_PACIFICRESEARCH            = 210; //  Pacific Research and Engineering Corporation
  MM_SIPROLAB                   = 211; //  Sipro Lab Telecom Inc.
  MM_LYNX                       = 212; //  Lynx Studio Technology, Inc.
  MM_SPECTRUM_PRODUCTIONS       = 213; //  Spectrum Productions
  MM_DICTAPHONE                 = 214; //  Dictaphone Corporation
  MM_QUALCOMM                   = 215; //  QUALCOMM, Inc.
  MM_RZS                        = 216; //  Ring Zero Systems, Inc
  MM_AUDIOSCIENCE               = 217; //  AudioScience Inc.
  MM_PINNACLE                   = 218; //  Pinnacle Systems, Inc.
  MM_EES                        = 219; //  EES Technik für Musik GmbH
  MM_HAFTMANN                   = 220; //  haftmann#software
  MM_LUCID                      = 221; //  Lucid Technology, Symetrix Inc.
  MM_HEADSPACE                  = 222; //  Headspace, Inc
  MM_UNISYS                     = 223; //  UNISYS CORPORATION
  MM_LUMINOSITI                 = 224; //  Luminositi, Inc.
  MM_ACTIVEVOICE                = 225; //  ACTIVE VOICE CORPORATION
  MM_DTS                        = 226; //  Digital Theater Systems, Inc.
  MM_DIGIGRAM                   = 227; //  DIGIGRAM
  MM_SOFTLAB_NSK                = 228; //  Softlab-Nsk
  MM_FORTEMEDIA                 = 229; //  ForteMedia, Inc
  MM_SONORUS                    = 230; //  Sonorus, Inc.
  MM_ARRAY                      = 231; //  Array Microsystems, Inc.
  MM_DATARAN                    = 232; //  Data Translation, Inc.
  MM_I_LINK                     = 233; //  I-link Worldwide
  MM_SELSIUS_SYSTEMS            = 234; //  Selsius Systems Inc.
  MM_ADMOS                      = 235; //  AdMOS Technology, Inc.
  MM_LEXICON                    = 236; //  Lexicon Inc.
  MM_SGI                        = 237; //  Silicon Graphics Inc.
  MM_IPI                        = 238; //  Interactive Product Inc.
  MM_ICE                        = 239; //  IC Ensemble, Inc.
  MM_VQST                       = 240; //  ViewQuest Technologies Inc.
  MM_ETEK                       = 241; //  eTEK Labs Inc.
  MM_CS                         = 242; //  Consistent Software
  MM_ALESIS                     = 243; //  Alesis Studio Electronics
  MM_INTERNET                   = 244; //  INTERNET Corporation
  MM_SONY                       = 245; //  Sony Corporation
  MM_HYPERACTIVE                = 246; //  Hyperactive Audio Systems, Inc.
  MM_UHER_INFORMATIC            = 247; //  UHER informatic GmbH
  MM_SYDEC_NV                   = 248; //  Sydec NV
  MM_FLEXION                    = 249; //  Flexion Systems Ltd.
  MM_VIA                        = 250; //  Via Technologies, Inc.
  MM_MICRONAS                   = 251; //  Micronas Semiconductors, Inc.
  MM_ANALOGDEVICES              = 252; //  Analog Devices, Inc.
  MM_HP                         = 253; //  Hewlett Packard Company
  MM_MATROX_DIV                 = 254; //  Matrox
  MM_QUICKAUDIO                 = 255; //  Quick Audio, GbR
  MM_YOUCOM                     = 256; //  You/Com Audiocommunicatie BV
  MM_RICHMOND                   = 257; //  Richmond Sound Design Ltd.
  MM_IODD                       = 258; //  I-O Data Device, Inc.
  MM_ICCC                       = 259; //  ICCC A/S
  MM_3COM                       = 260; //  3COM Corporation
  MM_MALDEN                     = 261; //  Malden Electronics Ltd.
  MM_3DFX                       = 262; //  3Dfx Interactive, Inc.
  MM_MINDMAKER                  = 263; //  Mindmaker, Inc.
  MM_TELEKOL                    = 264; //  Telekol Corp.
  MM_ST_MICROELECTRONICS        = 265; //  ST Microelectronics
  MM_ALGOVISION                 = 266; //  Algo Vision Systems GmbH

  MM_UNMAPPED                   = $ffff; // extensible MID mapping
  MM_PID_UNMAPPED               = MM_UNMAPPED;  // extensible PID mapping

//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
// GUID stuff omitted
//<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  // MM_MICROSOFT product IDs
  MM_MIDI_MAPPER     =  1;  //  Midi Mapper
  MM_WAVE_MAPPER     =  2;  //  Wave Mapper
  MM_SNDBLST_MIDIOUT =  3;  //  Sound Blaster MIDI output port
  MM_SNDBLST_MIDIIN  =  4;  //  Sound Blaster MIDI input port
  MM_SNDBLST_SYNTH   =  5;  //  Sound Blaster internal synth
  MM_SNDBLST_WAVEOUT =  6;  //  Sound Blaster waveform output
  MM_SNDBLST_WAVEIN  =  7;  //  Sound Blaster waveform input
  MM_ADLIB           =  9;  //  Ad Lib Compatible synth
  MM_MPU401_MIDIOUT  = 10;  //  MPU 401 compatible MIDI output port
  MM_MPU401_MIDIIN   = 11;  //  MPU 401 compatible MIDI input port
  MM_PC_JOYSTICK     = 12;  //  Joystick adapter

  MM_PCSPEAKER_WAVEOUT           = 13;  //  PC speaker waveform output
  MM_MSFT_WSS_WAVEIN             = 14;  //  MS Audio Board waveform input
  MM_MSFT_WSS_WAVEOUT            = 15;  //  MS Audio Board waveform output
  MM_MSFT_WSS_FMSYNTH_STEREO     = 16;  //  MS Audio Board  Stereo FM synth
  MM_MSFT_WSS_MIXER              = 17;  //  MS Audio Board Mixer Driver
  MM_MSFT_WSS_OEM_WAVEIN         = 18;  //  MS OEM Audio Board waveform input
  MM_MSFT_WSS_OEM_WAVEOUT        = 19;  //  MS OEM Audio Board waveform output
  MM_MSFT_WSS_OEM_FMSYNTH_STEREO = 20;  //  MS OEM Audio Board Stereo FM Synth
  MM_MSFT_WSS_AUX                = 21;  //  MS Audio Board Aux. Port
  MM_MSFT_WSS_OEM_AUX            = 22;  //  MS OEM Audio Aux Port
  MM_MSFT_GENERIC_WAVEIN         = 23;  //  MS Vanilla driver waveform input
  MM_MSFT_GENERIC_WAVEOUT        = 24;  //  MS Vanilla driver wavefrom output
  MM_MSFT_GENERIC_MIDIIN         = 25;  //  MS Vanilla driver MIDI in
  MM_MSFT_GENERIC_MIDIOUT        = 26;  //  MS Vanilla driver MIDI  external out
  MM_MSFT_GENERIC_MIDISYNTH      = 27;  //  MS Vanilla driver MIDI synthesizer
  MM_MSFT_GENERIC_AUX_LINE       = 28;  //  MS Vanilla driver aux (line in)
  MM_MSFT_GENERIC_AUX_MIC        = 29;  //  MS Vanilla driver aux (mic)
  MM_MSFT_GENERIC_AUX_CD         = 30;  //  MS Vanilla driver aux (CD)
  MM_MSFT_WSS_OEM_MIXER          = 31;  //  MS OEM Audio Board Mixer Driver
  MM_MSFT_MSACM                  = 32;  //  MS Audio Compression Manager
  MM_MSFT_ACM_MSADPCM            = 33;  //  MS ADPCM Codec
  MM_MSFT_ACM_IMAADPCM           = 34;  //  IMA ADPCM Codec
  MM_MSFT_ACM_MSFILTER           = 35;  //  MS Filter
  MM_MSFT_ACM_GSM610             = 36;  //  GSM 610 codec
  MM_MSFT_ACM_G711               = 37;  //  G.711 codec
  MM_MSFT_ACM_PCM                = 38;  //  PCM converter

  // Microsoft Windows Sound System drivers

  MM_WSS_SB16_WAVEIN    = 39;  //  Sound Blaster 16 waveform input
  MM_WSS_SB16_WAVEOUT   = 40;  //  Sound Blaster 16  waveform output
  MM_WSS_SB16_MIDIIN    = 41;  //  Sound Blaster 16 midi-in
  MM_WSS_SB16_MIDIOUT   = 42;  //  Sound Blaster 16 midi out
  MM_WSS_SB16_SYNTH     = 43;  //  Sound Blaster 16 FM Synthesis
  MM_WSS_SB16_AUX_LINE  = 44;  //  Sound Blaster 16 aux (line in)
  MM_WSS_SB16_AUX_CD    = 45;  //  Sound Blaster 16 aux (CD)
  MM_WSS_SB16_MIXER     = 46;  //  Sound Blaster 16 mixer device
  MM_WSS_SBPRO_WAVEIN   = 47;  //  Sound Blaster Pro waveform input
  MM_WSS_SBPRO_WAVEOUT  = 48;  //  Sound Blaster Pro waveform output
  MM_WSS_SBPRO_MIDIIN   = 49;  //  Sound Blaster Pro midi in
  MM_WSS_SBPRO_MIDIOUT  = 50;  //  Sound Blaster Pro midi out
  MM_WSS_SBPRO_SYNTH    = 51;  //  Sound Blaster Pro FM synthesis
  MM_WSS_SBPRO_AUX_LINE = 52;  //  Sound Blaster Pro aux (line in )
  MM_WSS_SBPRO_AUX_CD   = 53;  //  Sound Blaster Pro aux (CD)
  MM_WSS_SBPRO_MIXER    = 54;  //  Sound Blaster Pro mixer

  MM_MSFT_WSS_NT_WAVEIN         = 55;  //  WSS NT wave in
  MM_MSFT_WSS_NT_WAVEOUT        = 56;  //  WSS NT wave out
  MM_MSFT_WSS_NT_FMSYNTH_STEREO = 57;  //  WSS NT FM synth
  MM_MSFT_WSS_NT_MIXER          = 58;  //  WSS NT mixer
  MM_MSFT_WSS_NT_AUX            = 59;  //  WSS NT aux

  MM_MSFT_SB16_WAVEIN    = 60;  //  Sound Blaster 16 waveform input
  MM_MSFT_SB16_WAVEOUT   = 61;  //  Sound Blaster 16  waveform output
  MM_MSFT_SB16_MIDIIN    = 62;  //  Sound Blaster 16 midi-in
  MM_MSFT_SB16_MIDIOUT   = 63;  //  Sound Blaster 16 midi out
  MM_MSFT_SB16_SYNTH     = 64;  //  Sound Blaster 16 FM Synthesis
  MM_MSFT_SB16_AUX_LINE  = 65;  //  Sound Blaster 16 aux (line in)
  MM_MSFT_SB16_AUX_CD    = 66;  //  Sound Blaster 16 aux (CD)
  MM_MSFT_SB16_MIXER     = 67;  //  Sound Blaster 16 mixer device
  MM_MSFT_SBPRO_WAVEIN   = 68;  //  Sound Blaster Pro waveform input
  MM_MSFT_SBPRO_WAVEOUT  = 69;  //  Sound Blaster Pro waveform output
  MM_MSFT_SBPRO_MIDIIN   = 70;  //  Sound Blaster Pro midi in
  MM_MSFT_SBPRO_MIDIOUT  = 71;  //  Sound Blaster Pro midi out
  MM_MSFT_SBPRO_SYNTH    = 72;  //  Sound Blaster Pro FM synthesis
  MM_MSFT_SBPRO_AUX_LINE = 73;  //  Sound Blaster Pro aux (line in )
  MM_MSFT_SBPRO_AUX_CD   = 74;  //  Sound Blaster Pro aux (CD)
  MM_MSFT_SBPRO_MIXER    = 75;  //  Sound Blaster Pro mixer
  MM_MSFT_MSOPL_SYNTH    = 76;  // Yamaha OPL2/OPL3 compatible FM synthesis

  MM_MSFT_VMDMS_LINE_WAVEIN         = 80;  // Voice Modem Serial Line Wave Input
  MM_MSFT_VMDMS_LINE_WAVEOUT        = 81;  // Voice Modem Serial Line Wave Output
  MM_MSFT_VMDMS_HANDSET_WAVEIN      = 82;  // Voice Modem Serial Handset Wave Input
  MM_MSFT_VMDMS_HANDSET_WAVEOUT     = 83;  // Voice Modem Serial Handset Wave Output
  MM_MSFT_VMDMW_LINE_WAVEIN         = 84;  // Voice Modem Wrapper Line Wave Input
  MM_MSFT_VMDMW_LINE_WAVEOUT        = 85;  // Voice Modem Wrapper Line Wave Output
  MM_MSFT_VMDMW_HANDSET_WAVEIN      = 86;  // Voice Modem Wrapper Handset Wave Input
  MM_MSFT_VMDMW_HANDSET_WAVEOUT     = 87;  // Voice Modem Wrapper Handset Wave Output
  MM_MSFT_VMDMW_MIXER               = 88;  // Voice Modem Wrapper Mixer
  MM_MSFT_VMDM_GAME_WAVEOUT         = 89;  // Voice Modem Game Compatible Wave Device
  MM_MSFT_VMDM_GAME_WAVEIN          = 90;  // Voice Modem Game Compatible Wave Device

  MM_MSFT_ACM_MSNAUDIO              = 91;  //
  MM_MSFT_ACM_MSG723                = 92;  //
  MM_MSFT_ACM_MSRT24                = 93;  //

  MM_MSFT_WDMAUDIO_WAVEOUT          = 100;  // Generic id for WDM Audio drivers
  MM_MSFT_WDMAUDIO_WAVEIN           = 101;  // Generic id for WDM Audio drivers
  MM_MSFT_WDMAUDIO_MIDIOUT          = 102;  // Generic id for WDM Audio drivers
  MM_MSFT_WDMAUDIO_MIDIIN           = 103;  // Generic id for WDM Audio drivers
  MM_MSFT_WDMAUDIO_MIXER            = 104;  // Generic id for WDM Audio drivers
  MM_MSFT_WDMAUDIO_AUX              = 105;  // Generic id for WDM Audio drivers

//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
// Vendor media/product ID stuff omitted
//<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

//////////////////////////////////////////////////////////////////////////

const
  // INFO LIST CHUNKS (from the Multimedia Programmer's Reference plus new ones)
  RIFFINFO_IARL = $4c524149;  // 'IARL' Archival location
  RIFFINFO_IART = $544d4349;  // 'IART' Artist
  RIFFINFO_ICMS = $534d4349;  // 'ICMS' Commissioned
  RIFFINFO_ICMT = $544d4349;  // 'ICMT' Comments
  RIFFINFO_ICOP = $504f4349;  // 'ICOP' Copyright
  RIFFINFO_ICRD = $44524349;  // 'ICRD' Creation date of subject
  RIFFINFO_ICRP = $50524349;  // 'ICRP' Cropped
  RIFFINFO_IDIM = $4d494449;  // 'IDIM' Dimensions
  RIFFINFO_IDPI = $49504449;  // 'IDPI' Dots per inch
  RIFFINFO_IENG = $474e4549;  // 'IENG' Engineer
  RIFFINFO_IGNR = $524e4749;  // 'IGNR' Genre
  RIFFINFO_IKEY = $59454b49;  // 'IKEY' Keywords
  RIFFINFO_ILGT = $54474c49;  // 'ILGT' Lightness settings
  RIFFINFO_IMED = $44454d49;  // 'IMED' Medium
  RIFFINFO_INAM = $4d414e49;  // 'INAM' Name of subject
  RIFFINFO_IPLT = $544c5049;  // 'IPLT' Palette Settings. No. of colors requested.
  RIFFINFO_IPRD = $44525049;  // 'IPRD' Product
  RIFFINFO_ISBJ = $4a425349;  // 'ISBJ' Subject description
  RIFFINFO_ISFT = $54465349;  // 'ISFT' Software. Name of package used to create file.
  RIFFINFO_ISHP = $50485349;  // 'ISHP' Sharpness.
  RIFFINFO_ISRC = $43525349;  // 'ISRC' Source.
  RIFFINFO_ISRF = $46525349;  // 'ISRF' Source Form. ie slide, paper
  RIFFINFO_ITCH = $47435449;  // 'ITCH' Technician who digitized the subject.
  RIFFINFO_ISMP = $504d5349;  // 'ISMP' SMPTE time code
  RIFFINFO_IDIT = $54494449;  // 'IDIT' Digitization Time

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

const
  // WAVE form wFormatTag IDs
  WAVE_FORMAT_UNKNOWN               = $0000;  // Microsoft Corporation
  WAVE_FORMAT_PCM                   = $0001;  // updt 100812
  WAVE_FORMAT_ADPCM                 = $0002;  // Microsoft Corporation
  WAVE_FORMAT_IEEE_FLOAT            = $0003;  // Microsoft Corporation
                                              //  IEEE754: range (+1, -1]
                                              //  32-bit/64-bit format as defined by
                                              //  MSVC++ float/double type
  WAVE_FORMAT_VSELP                 = $0004;  // Compaq Computer Corp. */
  WAVE_FORMAT_IBM_CVSD              = $0005;  // IBM Corporation
  WAVE_FORMAT_ALAW                  = $0006;  // Microsoft Corporation
  WAVE_FORMAT_MULAW                 = $0007;  // Microsoft Corporation
  WAVE_FORMAT_DTS                   = $0008;  // Microsoft Corporation */
  WAVE_FORMAT_DRM                   = $0009;  // Microsoft Corporation */
  WAVE_FORMAT_WMAVOICE9             = $000A;  //* Microsoft Corporation *//updt 100812
  WAVE_FORMAT_WMAVOICE10            = $000B;  //* Microsoft Corporation *//updt 100812
  WAVE_FORMAT_OKI_ADPCM             = $0010;  // OKI
  WAVE_FORMAT_DVI_ADPCM             = $0011;  // Intel Corporation
  WAVE_FORMAT_IMA_ADPCM             = WAVE_FORMAT_DVI_ADPCM; // Intel Corporation
  WAVE_FORMAT_MEDIASPACE_ADPCM      = $0012;  // Videologic
  WAVE_FORMAT_SIERRA_ADPCM          = $0013;  // Sierra Semiconductor Corp
  WAVE_FORMAT_G723_ADPCM            = $0014;  // Antex Electronics Corporation
  WAVE_FORMAT_DIGISTD               = $0015;  // DSP Solutions, Inc.
  WAVE_FORMAT_DIGIFIX               = $0016;  // DSP Solutions, Inc.
  WAVE_FORMAT_DIALOGIC_OKI_ADPCM    = $0017;  // Dialogic Corporation
  WAVE_FORMAT_MEDIAVISION_ADPCM     = $0018;  // Media Vision, Inc. */
  WAVE_FORMAT_CU_CODEC              = $0019;  // Hewlett-Packard Company */
  WAVE_FORMAT_YAMAHA_ADPCM          = $0020;  // Yamaha Corporation of America
  WAVE_FORMAT_SONARC                = $0021;  // Speech Compression
  WAVE_FORMAT_DSPGROUP_TRUESPEECH   = $0022;  // DSP Group, Inc
  WAVE_FORMAT_ECHOSC1               = $0023;  // Echo Speech Corporation
  WAVE_FORMAT_AUDIOFILE_AF36        = $0024;  //
  WAVE_FORMAT_APTX                  = $0025;  // Audio Processing Technology
  WAVE_FORMAT_AUDIOFILE_AF10        = $0026;  //
  WAVE_FORMAT_PROSODY_1612          = $0027;  // Aculab plc */
  WAVE_FORMAT_LRC                   = $0028;  // Merging Technologies S.A. */
  WAVE_FORMAT_DOLBY_AC2             = $0030;  // Dolby Laboratories
  WAVE_FORMAT_GSM610                = $0031;  // Microsoft Corporation
  WAVE_FORMAT_MSNAUDIO              = $0032;  // Microsoft Corporation */
  WAVE_FORMAT_ANTEX_ADPCME          = $0033;  // Antex Electronics Corporation
  WAVE_FORMAT_CONTROL_RES_VQLPC     = $0034;  // Control Resources Limited
  WAVE_FORMAT_DIGIREAL              = $0035;  // DSP Solutions, Inc.
  WAVE_FORMAT_DIGIADPCM             = $0036;  // DSP Solutions, Inc.
  WAVE_FORMAT_CONTROL_RES_CR10      = $0037;  // Control Resources Limited
  WAVE_FORMAT_NMS_VBXADPCM          = $0038;  // Natural MicroSystems
  WAVE_FORMAT_CS_IMAADPCM           = $0039;  // Crystal Semiconductor IMA ADPCM
  WAVE_FORMAT_ECHOSC3               = $003A;  // Echo Speech Corporation */
  WAVE_FORMAT_ROCKWELL_ADPCM        = $003B;  // Rockwell International */
  WAVE_FORMAT_ROCKWELL_DIGITALK     = $003C;  // Rockwell International */
  WAVE_FORMAT_XEBEC                 = $003D;  // Xebec Multimedia Solutions Limited */
  WAVE_FORMAT_G721_ADPCM            = $0040;  // Antex Electronics Corporation
  WAVE_FORMAT_G728_CELP             = $0041;  // Antex Electronics Corporation */
  WAVE_FORMAT_MSG723                = $0042;  // Microsoft Corporation */
  WAVE_FORMAT_MPEG                  = $0050;  // Microsoft Corporation
  WAVE_FORMAT_RT24                  = $0052;  // InSoft, Inc. */
  WAVE_FORMAT_PAC                   = $0053;  // InSoft, Inc. */
  WAVE_FORMAT_MPEGLAYER3            = $0055;  // ISO/MPEG Layer3 Format Tag */
  WAVE_FORMAT_LUCENT_G723           = $0059;  // Lucent Technologies */
  WAVE_FORMAT_CIRRUS                = $0060;  // Cirrus Logic */
  WAVE_FORMAT_ESPCM                 = $0061;  // ESS Technology */
  WAVE_FORMAT_VOXWARE               = $0062;  // Voxware Inc */
  WAVE_FORMAT_CANOPUS_ATRAC         = $0063;  // Canopus, co., Ltd. */
  WAVE_FORMAT_G726_ADPCM            = $0064;  // APICOM */
  WAVE_FORMAT_G722_ADPCM            = $0065;  // APICOM */
  WAVE_FORMAT_DSAT_DISPLAY          = $0067;  // Microsoft Corporation */
  WAVE_FORMAT_VOXWARE_BYTE_ALIGNED  = $0069;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_AC8           = $0070;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_AC10          = $0071;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_AC16          = $0072;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_AC20          = $0073;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_RT24          = $0074;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_RT29          = $0075;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_RT29HW        = $0076;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_VR12          = $0077;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_VR18          = $0078;  // Voxware Inc */
  WAVE_FORMAT_VOXWARE_TQ40          = $0079;  // Voxware Inc */
  WAVE_FORMAT_SOFTSOUND             = $0080;  // Softsound, Ltd. */
  WAVE_FORMAT_VOXWARE_TQ60          = $0081;  // Voxware Inc */
  WAVE_FORMAT_MSRT24                = $0082;  // Microsoft Corporation */
  WAVE_FORMAT_G729A                 = $0083;  // AT&T Labs, Inc. */
  WAVE_FORMAT_MVI_MVI2              = $0084;  // Motion Pixels */
  WAVE_FORMAT_DF_G726               = $0085;  // DataFusion Systems (Pty) (Ltd) */
  WAVE_FORMAT_DF_GSM610             = $0086;  // DataFusion Systems (Pty) (Ltd) */
  WAVE_FORMAT_ISIAUDIO              = $0088;  // Iterated Systems, Inc. */
  WAVE_FORMAT_ONLIVE                = $0089;  // OnLive! Technologies, Inc. */
  WAVE_FORMAT_SBC24                 = $0091;  // Siemens Business Communications Sys */
  WAVE_FORMAT_DOLBY_AC3_SPDIF       = $0092;  // Sonic Foundry */
  WAVE_FORMAT_MEDIASONIC_G723       = $0093;  // MediaSonic */
  WAVE_FORMAT_PROSODY_8KBPS         = $0094;  // Aculab plc */
  WAVE_FORMAT_ZYXEL_ADPCM           = $0097;  // ZyXEL Communications, Inc. */
  WAVE_FORMAT_PHILIPS_LPCBB         = $0098;  // Philips Speech Processing */
  WAVE_FORMAT_PACKED                = $0099;  // Studer Professional Audio AG */
  WAVE_FORMAT_MALDEN_PHONYTALK      = $00A0;  // Malden Electronics Ltd. */
  WAVE_FORMAT_RHETOREX_ADPCM        = $0100;  // Rhetorex Inc. */
  WAVE_FORMAT_IRAT                  = $0101;  // BeCubed Software Inc. */
  WAVE_FORMAT_VIVO_G723             = $0111;  // Vivo Software */
  WAVE_FORMAT_VIVO_SIREN            = $0112;  // Vivo Software */
  WAVE_FORMAT_DIGITAL_G723          = $0123;  // Digital Equipment Corporation */
  WAVE_FORMAT_SANYO_LD_ADPCM        = $0125;  // Sanyo Electric Co., Ltd. */
  WAVE_FORMAT_SIPROLAB_ACEPLNET     = $0130;  // Sipro Lab Telecom Inc. */
  WAVE_FORMAT_SIPROLAB_ACELP4800    = $0131;  // Sipro Lab Telecom Inc. */
  WAVE_FORMAT_SIPROLAB_ACELP8V3     = $0132;  // Sipro Lab Telecom Inc. */
  WAVE_FORMAT_SIPROLAB_G729         = $0133;  // Sipro Lab Telecom Inc. */
  WAVE_FORMAT_SIPROLAB_G729A        = $0134;  // Sipro Lab Telecom Inc. */
  WAVE_FORMAT_SIPROLAB_KELVIN       = $0135;  // Sipro Lab Telecom Inc. */
  WAVE_FORMAT_G726ADPCM             = $0140;  // Dictaphone Corporation */
  WAVE_FORMAT_QUALCOMM_PUREVOICE    = $0150;  // Qualcomm, Inc. */
  WAVE_FORMAT_QUALCOMM_HALFRATE     = $0151;  // Qualcomm, Inc. */
  WAVE_FORMAT_TUBGSM                = $0155;  // Ring Zero Systems, Inc. */
  WAVE_FORMAT_MSAUDIO1              = $0160;  // Microsoft Corporation */
  WAVE_FORMAT_WMAUDIO2              = $0161;  //* Microsoft Corporation *// updt 100812
  WAVE_FORMAT_WMAUDIO3              = $0162;  //* Microsoft Corporation *// updt 100812
  WAVE_FORMAT_WMAUDIO_LOSSLESS      = $0163;  //* Microsoft Corporation *// updt 100812
  WAVE_FORMAT_WMASPDIF              = $0164;  //* Microsoft Corporation *// updt 100812
  WAVE_FORMAT_UNISYS_NAP_ADPCM      = $0170;  // Unisys Corp. */
  WAVE_FORMAT_UNISYS_NAP_ULAW       = $0171;  // Unisys Corp. */
  WAVE_FORMAT_UNISYS_NAP_ALAW       = $0172;  // Unisys Corp. */
  WAVE_FORMAT_UNISYS_NAP_16K        = $0173;  // Unisys Corp. */
  WAVE_FORMAT_CREATIVE_ADPCM        = $0200;  // Creative Labs, Inc
  WAVE_FORMAT_CREATIVE_FASTSPEECH8  = $0202;  // Creative Labs, Inc
  WAVE_FORMAT_CREATIVE_FASTSPEECH10 = $0203;  // Creative Labs, Inc
  WAVE_FORMAT_UHER_ADPCM            = $0210;  // UHER informatic GmbH */
  WAVE_FORMAT_QUARTERDECK           = $0220;  // Quarterdeck Corporation */
  WAVE_FORMAT_ILINK_VC              = $0230;  // I-link Worldwide */
  WAVE_FORMAT_RAW_SPORT             = $0240;  // Aureal Semiconductor */
  WAVE_FORMAT_ESST_AC3              = $0241;  // ESS Technology, Inc. */
  WAVE_FORMAT_IPI_HSX               = $0250;  // Interactive Products, Inc. */
  WAVE_FORMAT_IPI_RPELP             = $0251;  // Interactive Products, Inc. */
  WAVE_FORMAT_CS2                   = $0260;  // Consistent Software */
  WAVE_FORMAT_SONY_SCX              = $0270;  // Sony Corp. */
  WAVE_FORMAT_FM_TOWNS_SND          = $0300;  // Fujitsu Corp.
  WAVE_FORMAT_BTV_DIGITAL           = $0400;  // Brooktree Corporation */
  WAVE_FORMAT_QDESIGN_MUSIC         = $0450;  // QDesign Corporation */
  WAVE_FORMAT_VME_VMPCM             = $0680;  // AT&T Labs, Inc. */
  WAVE_FORMAT_TPC                   = $0681;  // AT&T Labs, Inc. */
  WAVE_FORMAT_OLIGSM                = $1000;  // Ing C. Olivetti & C., S.p.A.
  WAVE_FORMAT_OLIADPCM              = $1001;  // Ing C. Olivetti & C., S.p.A.
  WAVE_FORMAT_OLICELP               = $1002;  // Ing C. Olivetti & C., S.p.A.
  WAVE_FORMAT_OLISBC                = $1003;  // Ing C. Olivetti & C., S.p.A.
  WAVE_FORMAT_OLIOPR                = $1004;  // Ing C. Olivetti & C., S.p.A.
  WAVE_FORMAT_LH_CODEC              = $1100;  // Lernout & Hauspie */
  WAVE_FORMAT_NORRIS                = $1400;  // Norris Communications, Inc. */
  WAVE_FORMAT_SOUNDSPACE_MUSICOMPRESS = $1500;  // AT&T Labs, Inc. */
  WAVE_FORMAT_MPEG_ADTS_AAC           = $1600; //* Microsoft Corporation *//updt 100812 added
  WAVE_FORMAT_MPEG_RAW_AAC            = $1601; //* Microsoft Corporation *//updt 100812 added
  WAVE_FORMAT_MPEG_LOAS               = $1602; //* Microsoft Corporation (MPEG-4 Audio Transport Streams (LOAS/LATM) *//updt 100812 added
  WAVE_FORMAT_NOKIA_MPEG_ADTS_AAC     = $1608; //* Microsoft Corporation *//updt 100812 added
  WAVE_FORMAT_NOKIA_MPEG_RAW_AAC      = $1609; //* Microsoft Corporation *//updt 100812 added
  WAVE_FORMAT_VODAFONE_MPEG_ADTS_AAC  = $160A; //* Microsoft Corporation *//updt 100812 added
  WAVE_FORMAT_VODAFONE_MPEG_RAW_AAC   = $160B; //* Microsoft Corporation *//updt 100812 added
  WAVE_FORMAT_MPEG_HEAAC              = $1610; //* Microsoft Corporation (MPEG-2 AAC or MPEG-4 HE-AAC v1/v2 streams with any payload (ADTS, ADIF, LOAS/LATM, RAW). Format block includes MP4 AudioSpecificConfig() -- see HEAACWAVEFORMAT below *//updt 100812 added
  WAVE_FORMAT_DVM                   = $2000;  // FAST Multimedia AG */

  WAVE_FORMAT_EXTENSIBLE            = $FFFE;  // Microsoft */

  // the WAVE_FORMAT_DEVELOPMENT format tag can be used during the
  // development phase of a new wave format.  Before shipping, you
  // MUST acquire an official format tag from Microsoft.
  WAVE_FORMAT_DEVELOPMENT           = $FFFF;

type
  // Define data for MS ADPCM
  PADPCMCoefSet = ^TADPCMCoefSet;
  TADPCMCoefSet = packed record
    iCoef1: Word;
    iCoef2: Word;
  end;

  PADPCMWaveFormat = ^TADPCMWaveFormat;
  TADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
    wNumCoef: Word;
    aCoef: array [0 .. 0] of TADPCMCoefSet;
  end;

  //  Intel's DVI ADPCM structure definitions
  //      for WAVE_FORMAT_DVI_ADPCM   (0x0011)
  PDVIADPCMWaveFormat = ^TDVIADPCMWaveFormat;
  TDVIADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;

  //  IMA endorsed ADPCM structure definitions--note that this
  //  is exactly the same format as Intel's DVI ADPCM.
  //  for WAVE_FORMAT_IMA_ADPCM   (0x0011)
  PIMAADPCMWaveFormat = ^TIMAADPCMWaveFormat;
  TIMAADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;

  // VideoLogic's Media Space ADPCM Structure definitions
  // for  WAVE_FORMAT_MEDIASPACE_ADPCM    (0x0012)
  PMediaSpaceADPCMWaveFormat = ^TMediaSpaceADPCMWaveFormat;
  TMediaSpaceADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    wRevision: Word;
  end;

  //  Sierra Semiconductor
  //  for WAVE_FORMAT_SIERRA_ADPCM   (0x0013)
  PSierraADPCMWaveFormat = ^TSierraADPCMWaveFormat;
  TSierraADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    wRevision: Word;
  end;

  //  Antex Electronics  structure definitions
  //      for WAVE_FORMAT_G723_ADPCM   (0x0014)
  PG723_ADPCMWaveFormat = ^TG723_ADPCMWaveFormat;
  TG723_ADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    cbExtraSize: Word;
    nAuxBlockSize: Word;
  end;

  //  DSP Solutions (formerly DIGISPEECH) structure definitions
  //      for WAVE_FORMAT_DIGISTD   (0x0015)
  PDigiStdWaveFormat = ^TDigiStdWaveFormat;
  TDigiStdWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  DSP Solutions (formerly DIGISPEECH) structure definitions
  //      for WAVE_FORMAT_DIGIFIX   (0x0016)
  PDigiFixWaveFormat = ^TDigiFixWaveFormat;
  TDigiFixWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //   Dialogic Corporation
  // WAVEFORMAT_DIALOGIC_OKI_ADPCM   (0x0017)
  PDialogIkokiADPCMWaveFormat = ^TDialogIkokiADPCMWaveFormat;
  TDialogIkokiADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Yamaha Compression's ADPCM structure definitions
  //      for WAVE_FORMAT_YAMAHA_ADPCM   (0x0020)
  PYamaha_ADPCMWaveFormat = ^TYamaha_ADPCMWaveFormat;
  TYamaha_ADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Speech Compression's Sonarc structure definitions
  //      for WAVE_FORMAT_SONARC   (0x0021)
  PSonarcWaveFormat = ^TSonarcWaveFormat;
  TSonarcWaveFormat = packed record
    wfx: TWaveFormatEx;
    wCompType: Word;
  end;

  //  DSP Groups's TRUESPEECH structure definitions
  //      for WAVE_FORMAT_DSPGROUP_TRUESPEECH   (0x0022)
  PTrueSpeechWaveFormat = ^TTrueSpeechWaveFormat;
  TTrueSpeechWaveFormat = packed record
    wfx: TWaveFormatEx;
    wRevision: Word;
    wSamplesPerBlock: Word;
    abReserved: array [0 .. 27] of Byte;
  end;

  //  Echo Speech Corp structure definitions
  //      for WAVE_FORMAT_ECHOSC1   (0x0023)
  PEchoSC1WaveFormat = ^TEchoSC1WaveFormat;
  TEchoSC1WaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Audiofile Inc.structure definitions
  //      for WAVE_FORMAT_AUDIOFILE_AF36   (0x0024)
  PAudioFile_AF36WaveFormat = ^TAudioFile_AF36WaveFormat;
  TAudioFile_AF36WaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Audio Processing Technology structure definitions
  //      for WAVE_FORMAT_APTX   (0x0025)
  PAPTXWaveFormat = ^TAPTXWaveFormat;
  TAPTXWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Audiofile Inc.structure definitions
  //      for WAVE_FORMAT_AUDIOFILE_AF10   (0x0026)
  PAudioFile_AF10WaveFormat = ^TAudioFile_AF10WaveFormat;
  TAudioFile_AF10WaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  // Dolby's AC-2 wave format structure definition
  //           WAVE_FORMAT_DOLBY_AC2    (0x0030)*/
  PDolbyAC2WaveFormat = ^TDolbyAC2WaveFormat;
  TDolbyAC2WaveFormat = packed record
    wfx: TWaveFormatEx;
    nAuxBitsCode: Word;
  end;

  // Microsoft's
  // WAVE_FORMAT_GSM 610           0x0031
  PGSM610WaveFormat = ^TGSM610WaveFormat;
  TGSM610WaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  // Antex Electronics Corp
  // for WAVE_FORMAT_ADPCME                  (0x0033)
  PADPCMEWaveFormat = ^TADPCMEWaveFormat;
  TADPCMEWaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;

  // Control Resources Limited */
  // WAVE_FORMAT_CONTROL_RES_VQLPC                 0x0034
  PContresVQLPCWaveFormat = ^TContresVQLPCWaveFormat;
  TContresVQLPCWaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;

  // for WAVE_FORMAT_DIGIREAL                   (0x0035)
  PDigiRealWaveFormat = ^TDigiRealWaveFormat;
  TDigiRealWaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;

  //  DSP Solutions
  //      for WAVE_FORMAT_DIGIADPCM   (0x0036)
  PDigiADPCMWaveFormat = ^TDigiADPCMWaveFormat;
  TDigiADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;

  //       Control Resources Limited */
  // WAVE_FORMAT_CONTROL_RES_CR10          0x0037
  PContResCR10WaveFormat = ^TContResCR10WaveFormat;
  TContResCR10WaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;

  //  Natural Microsystems
  //      for WAVE_FORMAT_NMS_VBXADPCM   (0x0038)
  PVBXADPCMWaveFormat = ^TVBXADPCMWaveFormat;
  TVBXADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;

  //  Antex Electronics  structure definitions
  //      for WAVE_FORMAT_G721_ADPCM   (0x0040)
  PG721_ADPCMWaveFormat = ^TG721_ADPCMWaveFormat;
  TG721_ADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    wSamplesPerBlock: Word;
  end;

  // Microsoft MPEG audio WAV definition
  //  MPEG-1 audio wave format (audio layer only).   (0x0050)
  PMPEG1WaveFormat = ^TMPEG1WaveFormat;
  TMPEG1WaveFormat = packed record
    wfx: TWaveFormatEx;
    fwHeadLayer: Word;
    dwHeadBitrate: DWORD;
    fwHeadMode: Word;
    fwHeadModeExt: Word;
    wHeadEmphasis: Word;
    fwHeadFlags: Word;
    dwPTSlow: DWORD;
    dwPTShigh: DWORD;
  end;

const
  ACM_MPEG_LAYER1             = $0001;
  ACM_MPEG_LAYER2             = $0002;
  ACM_MPEG_LAYER3             = $0004;
  ACM_MPEG_STEREO             = $0001;
  ACM_MPEG_JOINTSTEREO        = $0002;
  ACM_MPEG_DUALCHANNEL        = $0004;
  ACM_MPEG_SINGLECHANNEL      = $0008;
  ACM_MPEG_PRIVATEBIT         = $0001;
  ACM_MPEG_COPYRIGHT          = $0002;
  ACM_MPEG_ORIGINALHOME       = $0004;
  ACM_MPEG_PROTECTIONBIT      = $0008;
  ACM_MPEG_ID_MPEG1           = $0010;

type
  //  Creative's ADPCM structure definitions
  //      for WAVE_FORMAT_CREATIVE_ADPCM   (0x0200)
  PCreativeADPCMWaveFormat = ^TCreativeADPCMWaveFormat;
  TCreativeADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
    wRevision: Word;
  end;

  //    Creative FASTSPEECH
  // WAVEFORMAT_CREATIVE_FASTSPEECH8   (0x0202)
  PCreativeFastSpeech8WaveFormat = ^TCreativeFastSpeech8WaveFormat;
  TCreativeFastSpeech8WaveFormat = packed record
    wfx: TWaveFormatEx;
    wRevision: Word;
  end;

  //    Creative FASTSPEECH
  // WAVEFORMAT_CREATIVE_FASTSPEECH10   (0x0203)
  PCreativeFastSpeech10WaveFormat = ^TCreativeFastSpeech10WaveFormat;
  TCreativeFastSpeech10WaveFormat = packed record
    wfx: TWaveFormatEx;
    wRevision: Word;
  end;

  //  Fujitsu FM Towns 'SND' structure
  //      for WAVE_FORMAT_FMMTOWNS_SND   (0x0300)
  PFMTowns_Snd_WaveFormat = ^TFMTowns_Snd_WaveFormat;
  TFMTowns_Snd_WaveFormat = packed record
    wfx: TWaveFormatEx;
    wRevision: Word;
  end;

  //  Olivetti structure
  //      for WAVE_FORMAT_OLIGSM   (0x1000)
  POliGSMWaveFormat = ^TOliGSMWaveFormat;
  TOliGSMWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Olivetti structure
  //      for WAVE_FORMAT_OLIADPCM   (0x1001)
  POliADPCMWaveFormat = ^TOliADPCMWaveFormat;
  TOliADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Olivetti structure
  //      for WAVE_FORMAT_OLICELP   (0x1002)
  POliCelpWaveFormat = ^TOliCelpWaveFormat;
  TOliCelpWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Olivetti structure
  //      for WAVE_FORMAT_OLISBC   (0x1003)
  POliSbcWaveFormat = ^TOliSbcWaveFormat;
  TOliSbcWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Olivetti structure
  //      for WAVE_FORMAT_OLIOPR   (0x1004)
  POliOprWaveFormat = ^TOliOprWaveFormat;
  TOliOprWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

  //  Crystal Semiconductor IMA ADPCM format
  //      for WAVE_FORMAT_CS_IMAADPCM   (0x0039)
  PCSIMAADPCMWaveFormat = ^TCSIMAADPCMWaveFormat;
  TCSIMAADPCMWaveFormat = packed record
    wfx: TWaveFormatEx;
  end;

//======================================================================

const
//WAVE_FORMAT_MPEGLAYER3           = $55;
  MPEGLAYER3_WFX_EXTRA_BYTES       = 12;

  MPEGLAYER3_ID_UNKNOWN            = 0;
  MPEGLAYER3_ID_MPEG               = 1;
  MPEGLAYER3_ID_CONSTANTFRAMESIZE  = 2;

  MPEGLAYER3_FLAG_PADDING_ISO      = $00000000;
  MPEGLAYER3_FLAG_PADDING_ON       = $00000001;
  MPEGLAYER3_FLAG_PADDING_OFF      = $00000002;

{ Die Encoder unterscheiden sich ein wenig in der Art, wie ein MP3
  "verunstaltet" wird.

  Alle Encoder platzieren vor den originalen (umgewandelten) Samples einen
  Block Stille von einer gewissen Länge. Diese hat mindestens einen Frame
  Länge. In einer Wave-Datei wird die Länge dieser Stille durch das Feld
  nCodecDelay angegeben (in vollen Samples, also ggf. stereo).

  Bei Microsoft und dem Fraunhofer Codec steht dort (immer) der Wert 1393.

    Decoder
      1 Granule Stille:          576
    Encoder
      Standard MDTC Filterbank:  528
      Attenuation (max):         288
                                ====
      Stille am Anfang:         1392

  LAME erzeugt unterschiedliche Werte um die 2257 Samples, allerdings nur
  ~1105 bei VBR (also etwas mehr als eine Framelänge).

                                 CBR    VBR
    Decoder                     ----   ----
      1 Granule Stille:          576    576
    Encoder
      1 Frame Stille:           1152
      Standard MDTC Filterbank:  528    528
                                ====   ====
                                2256   1104

  Außerdem generieren alle Encoder ebenso Stille am Ende der Datei.

  Der MS generiert hier etwa 1/4 Sekunde, der Fraunhofer etwa 1/2 Sekunde.
  LAME ist hier sehr sparsam: der letzte Frame wird abgeschlossen und darauf
  folgen noch zwei leere Frames, bei VBR noch nicht einmal dies: nur der
  letzte Frame wird abgeschlossen.

  Da diese Werte automatisch generiert sind, sind sie in der Sample-Länge,
  die im "fact"-Chunk angegeben wird, nicht enthalten!
}

type
  PMpegLayer3WaveFormat = ^TMpegLayer3WaveFormat;
  TMpegLayer3WaveFormat = packed record
    wfx: TWaveFormatEx;
    wID: word;                  // MPEGLAYER3_ID_MPEG
    fdwFlags: cardinal;         // MPEGLAYER3_FLAG_...
    nBlockSize: word;           // Größe eines MP3-Frames
    nFramesPerBlock: word;      // Norm. 1
    nCodecDelay: word;          // Leere Samples, die der Codec am Beginn hinzugefügt hat
                                // ACM codecs (MS, Fraunhofer): 1393 zu 1152
                                // Lame: 2257 zu 1152
  end;

//==========================================================================;
//  ACM Wave Filters
//==========================================================================;

const
  WAVE_FILTER_UNKNOWN     = $0000;
  WAVE_FILTER_DEVELOPMENT = $FFFF;

type
  PWaveFilter = ^TWaveFilter;
  TWaveFilter = packed record
    cbStruct: DWORD;                      // Size of the filter in bytes
    dwFilterTag: DWORD;                   // filter type
    fdwFilter: DWORD;                     // Flags for the filter (Universal Dfns)
    dwReserved: array [0 .. 4] of DWORD;  // Reserved for system use
  end;

const
  WAVE_FILTER_VOLUME      = $0001;

type
  PVolumeWaveFilter = ^TVolumeWaveFilter;
  TVolumeWaveFilter = packed record
    wfltr: TWaveFilter;
    dwVolume: DWORD;
  end;

const
  WAVE_FILTER_ECHO        = $0002;

type
  PEchoWaveFilter = ^TEchoWaveFilter;
  TEchoWaveFilter = packed record
    wfltr: TWaveFilter;
    dwVolume: DWORD;
    dwDelay: DWORD;
  end;

//////////////////////////////////////////////////////////////////////////
// New RIFF WAVE Chunks

const
  RIFFWAVE_INST = $74736e69;  // 'inst'

type
  Ps_RIFFWAVE_INST = ^Ts_RIFFWAVE_INST;
  Ts_RIFFWAVE_INST = packed record
    bUnshiftedNote: Byte;
    chFineTune: ShortInt;
    chGain: ShortInt;
    bLowNote: Byte;
    bHighNote: Byte;
    bLowVelocity: Byte;
    bHighVelocity: Byte;
  end;

//////////////////////////////////////////////////////////////////////////
// New RIFF Forms

// RIFF AVI
//
// AVI file format is specified in a seperate file (AVIFMT.H),
// which is available in the VfW and Win 32 SDK

// RIFF CPPO

const
  RIFFCPPO      = $4f505043;   // 'CPPO'

  RIFFCPPO_objr = $726a626f;   // 'objr'
  RIFFCPPO_obji = $696a626f;   // 'obji'

  RIFFCPPO_clsr = $72736c63;   // 'clsr'
  RIFFCPPO_clsi = $69736c63;   // 'clsi'

  RIFFCPPO_mbr  = $2072626d;   // 'mbr '

  RIFFCPPO_char = $72616863;   // 'char'

  RIFFCPPO_byte = $65747962;   // 'byte'
  RIFFCPPO_int  = $20746e69;   // 'int '
  RIFFCPPO_word = $64726f77;   // 'word'
  RIFFCPPO_long = $676e6f6c;   // 'long'
  RIFFCPPO_dwrd = $64627764;   // 'dwrd'
  RIFFCPPO_flt  = $20746c66;   // 'flt '
  RIFFCPPO_dbl  = $206c6264;   // 'dbl '
  RIFFCPPO_str  = $20727473;   // 'str '

//////////////////////////////////////////////////////////////////////////
// DIB Compression Defines

const
  BI_BITFIELDS = 3;

  QUERYDIBSUPPORT = 3073;
  QDI_SETDIBITS   = $0001;
  QDI_GETDIBITS   = $0002;
  QDI_DIBTOSCREEN = $0004;
  QDI_STRETCHDIB  = $0008;

type
  // Structure definitions
  PExBmInfoHeader = ^TExBmInfoHeader;
  TExBmInfoHeader = packed record
    bmi: TBitmapInfoHeader;
    // extended BITMAPINFOHEADER fields
    biExtDataOffset: DWORD;
  end;

const
  // New DIB Compression Defines
  BICOMP_IBMULTIMOTION  = $49544c55;  // 'ULTI'
  BICOMP_IBMPHOTOMOTION = $4f4d4850;  // 'PHMO'
  BICOMP_CREATIVEYUV    = $56555943;  // 'cyuv'

  // New DIB Compression Defines
  JPEG_DIB              = $4745504a;  // 'JPEG' Still image JPEG DIB biCompression
  MJPG_DIB              = $4745504d;  // 'MJPG' Motion JPEG DIB biCompression

  // JPEGProcess Definitions
  JPEG_PROCESS_BASELINE = 0;          // Baseline DCT

  // AVI File format extensions
  AVIIF_CONTROLFRAME    = $00000200;  // This is a control frame


// JIF Marker byte pairs in JPEG Interchange Format sequence
  {$EXTERNALSYM JIFMK_SOF0}
  JIFMK_SOF0                          = $FFC0;  { SOF Huff  - Baseline DCT}
  {$EXTERNALSYM JIFMK_SOF1}
  JIFMK_SOF1                          = $FFC1;  { SOF Huff  - Extended sequential DCT}
  {$EXTERNALSYM JIFMK_SOF2}
  JIFMK_SOF2                          = $FFC2;  { SOF Huff  - Progressive DCT}
  {$EXTERNALSYM JIFMK_SOF3}
  JIFMK_SOF3                          = $FFC3;  { SOF Huff  - Spatial (sequential) lossless}
  {$EXTERNALSYM JIFMK_SOF5}
  JIFMK_SOF5                          = $FFC5;  { SOF Huff  - Differential sequential DCT}
  {$EXTERNALSYM JIFMK_SOF6}
  JIFMK_SOF6                          = $FFC6;  { SOF Huff  - Differential progressive DCT}
  {$EXTERNALSYM JIFMK_SOF7}
  JIFMK_SOF7                          = $FFC7;  { SOF Huff  - Differential spatial}
  {$EXTERNALSYM JIFMK_JPG}
  JIFMK_JPG                           = $FFC8;  { SOF Arith - Reserved for JPEG extensions}
  {$EXTERNALSYM JIFMK_SOF9}
  JIFMK_SOF9                          = $FFC9;  { SOF Arith - Extended sequential DCT}
  {$EXTERNALSYM JIFMK_SOF10}
  JIFMK_SOF10                         = $FFCA;  { SOF Arith - Progressive DCT}
  {$EXTERNALSYM JIFMK_SOF11}
  JIFMK_SOF11                         = $FFCB;  { SOF Arith - Spatial (sequential) lossless}
  {$EXTERNALSYM JIFMK_SOF13}
  JIFMK_SOF13                         = $FFCD;  { SOF Arith - Differential sequential DCT}
  {$EXTERNALSYM JIFMK_SOF14}
  JIFMK_SOF14                         = $FFCE;  { SOF Arith - Differential progressive DCT}
  {$EXTERNALSYM JIFMK_SOF15}
  JIFMK_SOF15                         = $FFCF;  { SOF Arith - Differential spatial}
  {$EXTERNALSYM JIFMK_DHT}
  JIFMK_DHT                           = $FFC4;  { Define Huffman Table(s) }
  {$EXTERNALSYM JIFMK_DAC}
  JIFMK_DAC                           = $FFCC;  { Define Arithmetic coding conditioning(s) }
  {$EXTERNALSYM JIFMK_RST0}
  JIFMK_RST0                          = $FFD0;  { Restart with modulo 8 count 0 }
  {$EXTERNALSYM JIFMK_RST1}
  JIFMK_RST1                          = $FFD1;  { Restart with modulo 8 count 1 }
  {$EXTERNALSYM JIFMK_RST2}
  JIFMK_RST2                          = $FFD2;  { Restart with modulo 8 count 2 }
  {$EXTERNALSYM JIFMK_RST3}
  JIFMK_RST3                          = $FFD3;  { Restart with modulo 8 count 3 }
  {$EXTERNALSYM JIFMK_RST4}
  JIFMK_RST4                          = $FFD4;  { Restart with modulo 8 count 4 }
  {$EXTERNALSYM JIFMK_RST5}
  JIFMK_RST5                          = $FFD5;  { Restart with modulo 8 count 5 }
  {$EXTERNALSYM JIFMK_RST6}
  JIFMK_RST6                          = $FFD6;  { Restart with modulo 8 count 6 }
  {$EXTERNALSYM JIFMK_RST7}
  JIFMK_RST7                          = $FFD7;  { Restart with modulo 8 count 7 }
  {$EXTERNALSYM JIFMK_SOI}
  JIFMK_SOI                           = $FFD8;  { Start of Image }
  {$EXTERNALSYM JIFMK_EOI}
  JIFMK_EOI                           = $FFD9;  { End of Image }
  {$EXTERNALSYM JIFMK_SOS}
  JIFMK_SOS                           = $FFDA;  { Start of Scan }
  {$EXTERNALSYM JIFMK_DQT}
  JIFMK_DQT                           = $FFDB;  { Define quantization Table(s) }
  {$EXTERNALSYM JIFMK_DNL}
  JIFMK_DNL                           = $FFDC;  { Define Number of Lines }
  {$EXTERNALSYM JIFMK_DRI}
  JIFMK_DRI                           = $FFDD;  { Define Restart Interval }
  {$EXTERNALSYM JIFMK_DHP}
  JIFMK_DHP                           = $FFDE;  { Define Hierarchical progression }
  {$EXTERNALSYM JIFMK_EXP}
  JIFMK_EXP                           = $FFDF;  { Expand Reference Component(s) }
  {$EXTERNALSYM JIFMK_APP0}
  JIFMK_APP0                          = $FFE0;  { Application Field 0}
  {$EXTERNALSYM JIFMK_APP1}
  JIFMK_APP1                          = $FFE1;  { Application Field 1}
  {$EXTERNALSYM JIFMK_APP2}
  JIFMK_APP2                          = $FFE2;  { Application Field 2}
  {$EXTERNALSYM JIFMK_APP3}
  JIFMK_APP3                          = $FFE3;  { Application Field 3}
  {$EXTERNALSYM JIFMK_APP4}
  JIFMK_APP4                          = $FFE4;  { Application Field 4}
  {$EXTERNALSYM JIFMK_APP5}
  JIFMK_APP5                          = $FFE5;  { Application Field 5}
  {$EXTERNALSYM JIFMK_APP6}
  JIFMK_APP6                          = $FFE6;  { Application Field 6}
  {$EXTERNALSYM JIFMK_APP7}
  JIFMK_APP7                          = $FFE7;  { Application Field 7}
  {$EXTERNALSYM JIFMK_JPG0}
  JIFMK_JPG0                          = $FFF0;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG1}
  JIFMK_JPG1                          = $FFF1;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG2}
  JIFMK_JPG2                          = $FFF2;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG3}
  JIFMK_JPG3                          = $FFF3;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG4}
  JIFMK_JPG4                          = $FFF4;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG5}
  JIFMK_JPG5                          = $FFF5;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG6}
  JIFMK_JPG6                          = $FFF6;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG7}
  JIFMK_JPG7                          = $FFF7;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG8}
  JIFMK_JPG8                          = $FFF8;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG9}
  JIFMK_JPG9                          = $FFF9;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG10}
  JIFMK_JPG10                         = $FFFA;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG11}
  JIFMK_JPG11                         = $FFFB;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG12}
  JIFMK_JPG12                         = $FFFC;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_JPG13}
  JIFMK_JPG13                         = $FFFD;  { Reserved for JPEG extensions }
  {$EXTERNALSYM JIFMK_COM}
  JIFMK_COM                           = $FFFE;  { Comment }
  {$EXTERNALSYM JIFMK_TEM}
  JIFMK_TEM                           = $FF01;  { for temp private use arith code }
  {$EXTERNALSYM JIFMK_RES}
  JIFMK_RES                           = $FF02;  { Reserved }
  {$EXTERNALSYM JIFMK_00}
  JIFMK_00                            = $FF00;  { Zero stuffed byte - entropy data }
  {$EXTERNALSYM JIFMK_FF}
  JIFMK_FF                            = $FFFF;  { Fill byte }

// JPEGColorSpaceID Definitions
  {$EXTERNALSYM JPEG_Y}
  JPEG_Y                              = 1;  { Y only component of YCbCr }
  {$EXTERNALSYM JPEG_YCbCr}
  JPEG_YCbCr                          = 2;  { YCbCr as define by CCIR 601 }
  {$EXTERNALSYM JPEG_RGB}
  JPEG_RGB                            = 3;  { 3 component RGB }


type
  // Structure definitions
  PJPEGInfoHeader = ^TJPEGInfoHeader;
  TJPEGInfoHeader = packed record
    // compression-specific fields
    // these fields are defined for 'JPEG' and 'MJPG'
    JPEGSize: DWORD;
    JPEGProcess: DWORD;

    // Process specific fields
    JPEGColorSpaceID: DWORD;
    JPEGBitsPerSample: DWORD;
    JPEGHSubSampling: DWORD;
    JPEGVSubSampling: DWORD;
  end;

const
  // Default DHT Segment
  MJPGDHTSeg: array [0 .. $1a3] of byte = (
    // JPEG DHT Segment for YCrCb omitted from MJPG data
    $FF,$C4,$01,$A2,
    $00,$00,$01,$05,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,
    $01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$01,$00,$03,$01,$01,$01,$01,
    $01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$05,$06,$07,
    $08,$09,$0A,$0B,$10,$00,$02,$01,$03,$03,$02,$04,$03,$05,$05,$04,$04,$00,
    $00,$01,$7D,$01,$02,$03,$00,$04,$11,$05,$12,$21,$31,$41,$06,$13,$51,$61,
    $07,$22,$71,$14,$32,$81,$91,$A1,$08,$23,$42,$B1,$C1,$15,$52,$D1,$F0,$24,
    $33,$62,$72,$82,$09,$0A,$16,$17,$18,$19,$1A,$25,$26,$27,$28,$29,$2A,$34,
    $35,$36,$37,$38,$39,$3A,$43,$44,$45,$46,$47,$48,$49,$4A,$53,$54,$55,$56,
    $57,$58,$59,$5A,$63,$64,$65,$66,$67,$68,$69,$6A,$73,$74,$75,$76,$77,$78,
    $79,$7A,$83,$84,$85,$86,$87,$88,$89,$8A,$92,$93,$94,$95,$96,$97,$98,$99,
    $9A,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,
    $BA,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,
    $DA,$E1,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$F1,$F2,$F3,$F4,$F5,$F6,$F7,
    $F8,$F9,$FA,$11,$00,$02,$01,$02,$04,$04,$03,$04,$07,$05,$04,$04,$00,$01,
    $02,$77,$00,$01,$02,$03,$11,$04,$05,$21,$31,$06,$12,$41,$51,$07,$61,$71,
    $13,$22,$32,$81,$08,$14,$42,$91,$A1,$B1,$C1,$09,$23,$33,$52,$F0,$15,$62,
    $72,$D1,$0A,$16,$24,$34,$E1,$25,$F1,$17,$18,$19,$1A,$26,$27,$28,$29,$2A,
    $35,$36,$37,$38,$39,$3A,$43,$44,$45,$46,$47,$48,$49,$4A,$53,$54,$55,$56,
    $57,$58,$59,$5A,$63,$64,$65,$66,$67,$68,$69,$6A,$73,$74,$75,$76,$77,$78,
    $79,$7A,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$92,$93,$94,$95,$96,$97,$98,
    $99,$9A,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$B2,$B3,$B4,$B5,$B6,$B7,$B8,
    $B9,$BA,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$D2,$D3,$D4,$D5,$D6,$D7,$D8,
    $D9,$DA,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$F2,$F3,$F4,$F5,$F6,$F7,$F8,
    $F9,$FA
  );

//////////////////////////////////////////////////////////////////////////
// Defined IC types

const
  // Misc. FOURCC registration

  // Sierra Semiconductor: RDSP- Confidential RIFF file format
  //       for the storage and downloading of DSP
  //       code for Audio and communications devices.
  FOURCC_RDSP  = $50534452;  // 'RDSP'

  MIXERCONTROL_CONTROLTYPE_SRS_MTS         = MIXERCONTROL_CONTROLTYPE_BOOLEAN + 6;
  MIXERCONTROL_CONTROLTYPE_SRS_ONOFF       = MIXERCONTROL_CONTROLTYPE_BOOLEAN + 7;
  MIXERCONTROL_CONTROLTYPE_SRS_SYNTHSELECT = MIXERCONTROL_CONTROLTYPE_BOOLEAN + 8;

implementation

end.
