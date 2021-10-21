unit Mferror;

//----------------------------------------------------------------------------
//
// Source file: Mferror.h
// Converted:   29/07/2012 7:52:19 PM
//
// Header initially converted using HtoPAS version 3.01.0001
//
// Portions created by Microsoft are
// Copyright (c) 1995+ Microsoft Corporation.
// All Rights Reserved.
//
// The initial developer of this module is Tony Kalf & Peter Larson
// Portions created by Tony Kalf & Peter Larson are
// Copyright (c) 2012 Tony Kalf & Peter Larson: All rights reserved.
//
//----------------------------------------------------------------------------
// CHANGE LOG
// Version    Date        Name                 Details
// ---------- ----------- -------------------- -------------------------------
// 01.00.0000 2012 Jul 29 HtoPas               Header conversion
//                        Peter Larson         Minor tweaks
//
//----------------------------------------------------------------------------
//
// LICENCE
//
// This software  is distributed on an  "AS IS" basis, without WARRANTY OF ANY
// KIND, either  express or implied.  Users must review the  code to determine
// its suitability for use.
//
// This software is NOT in the public  domain and copyright  remains with  the
// original developer. The software may be used free of charge for private use
// and may be  used in  derivative  end  user  products  such  as  application
// programs, both commercial and non-commercial free of charge.
//
// This software,  or  binary versions,  must  not  be  distributed  with  any
// development  tools,  Integrated  Development  Environments,  compilers,  or
// similar products without  the express  written  permission of the copyright
// owner.
//
// Users may distribute this source code provided that this header is included
// in full at the top of the file.
//
//----------------------------------------------------------------------------

interface

(*++                                                                                      // 00000.001
                                                                                          // 00001.001
  Microsoft Windows Media Foundation                                                      // 00002.001
  Copyright (C) Microsoft Corporation. All rights reserved.                               // 00003.001
                                                                                          // 00004.001
Module Name:                                                                              // 00005.001
                                                                                          // 00006.001
    mferror.mc                                                                            // 00007.001
                                                                                          // 00008.001
Abstract:                                                                                 // 00009.001
                                                                                          // 00010.001
    Definitions for MediaFoundation events.                                               // 00011.001
                                                                                          // 00012.001
Author:                                                                                   // 00013.001
                                                                                          // 00014.001
                                                                                          // 00015.001
Revision History:                                                                         // 00016.001
                                                                                          // 00017.001
Notes:                                                                                    // 00018.001
                                                                                          // 00019.001
    This file is used by the MC tool to generate the mferror.h file                       // 00020.001
                                                                                          // 00021.001
**************************** READ ME ******************************************           // 00022.001
                                                                                          // 00023.001
 Here are the commented error ranges for the Windows Media Technologies Group             // 00024.001
                                                                                          // 00025.001
                                                                                          // 00026.001
 RANGES                                                                                   // 00027.001
                                                                                          // 00028.001
 14000 - 14999 = General Media Foundation errors                                          // 00029.001
                                                                                          // 00030.001
 15000 - 15999 = ASF parsing errors                                                       // 00031.001
                                                                                          // 00032.001
 16000 - 16999 = Media Source errors                                                      // 00033.001
                                                                                          // 00034.001
 17000 - 17999 = MEDIAFOUNDATION Network Error Events                                     // 00035.001
                                                                                          // 00036.001
 18000 - 18999 = MEDIAFOUNDATION WMContainer Error Events                                 // 00037.001
                                                                                          // 00038.001
 19000 - 19999 = MEDIAFOUNDATION Media Sink Error Events                                  // 00039.001
                                                                                          // 00040.001
 20000 - 20999 = Renderer errors                                                          // 00041.001
                                                                                          // 00042.001
 21000 - 21999 = Topology Errors                                                          // 00043.001
                                                                                          // 00044.001
 25000 - 25999 = Timeline Errors                                                          // 00045.001
                                                                                          // 00046.001
 26000 - 26999 = Unused                                                                   // 00047.001
                                                                                          // 00048.001
 28000 - 28999 = Transform errors                                                         // 00049.001
                                                                                          // 00050.001
 29000 - 29999 = Content Protection errors                                                // 00051.001
                                                                                          // 00052.001
 40000 - 40999 = Clock errors                                                             // 00053.001
                                                                                          // 00054.001
 41000 - 41999 = MF Quality Management Errors                                             // 00055.001
                                                                                          // 00056.001
 42000 - 42999 = MF Transcode API Errors                                                  // 00057.001
                                                                                          // 00058.001
**************************** READ ME ******************************************           // 00059.001
                                                                                          // 00060.001
--*)
Uses
  windows;
                                                                                          // 00061.001
Const
  Version               = '0.1.0001';
                                                                                          // 00062.001                                                                                          // 00063.001#ifndef _MFERROR_H
//_MFERROR_H                            = true;                                           // 00064.001#define _MFERROR_H
                                                                                          // 00065.001
                                                                                          // 00066.001
//  STATUS_SEVERITY                       = (hr)(((hr)>>30)&$3);                            // 00067.001#define STATUS_SEVERITY(hr)  (((hr) >> 30) & 0x3)
  function STATUS_SEVERITY(hr: HRESULT): LongWord;
                                                                                          // 00068.001
Type                                                                                          // 00069.001#ifdef RC_INVOKED
  _HRESULT_TYPEDEF_                     = DWORD;                                         // 00070.001#define _HRESULT_TYPEDEF_(_sc) _sc
                                                                                          // 00071.001#else // RC_INVOKED
//  _HRESULT_TYPEDEF_                     = (_sc)((HRESULT)_sc);                            // 00072.001#define _HRESULT_TYPEDEF_(_sc) ((HRESULT)_sc)
//## Error: Line: 72, Param: 11. "_HRESULT_TYPEDEF_" already defined                        // 00072.002
                                                                                          // 00073.001#endif // RC_INVOKED
Const                                                                                          // 00074.001
                                                                                          // 00075.001
/////////////////////////////////////////////////////////////////////////                 // 00076.001
//                                                                                        // 00077.001
// MEDIAFOUNDATION Success Events                                                         // 00078.001
//                                                                                        // 00079.001
/////////////////////////////////////////////////////////////////////////                 // 00080.001
                                                                                          // 00081.001
                                                                                          // 00082.001
/////////////////////////////////////////////////////////////////////////                 // 00083.001
//                                                                                        // 00084.001
// MEDIAFOUNDATION Warning Events                                                         // 00085.001
//                                                                                        // 00086.001
/////////////////////////////////////////////////////////////////////////                 // 00087.001
                                                                                          // 00088.001
                                                                                          // 00089.001 
/////////////////////////////////////////////////////////////////////////                 // 00090.001 
//                                                                                        // 00091.001 
// MEDIAFOUNDATION Error Events                                                           // 00092.001 
//                                                                                        // 00093.001 
/////////////////////////////////////////////////////////////////////////                 // 00094.001 
                                                                                          // 00095.001 
//                                                                                        // 00096.001 
//  Values are 32 bit values laid out as follows:                                         // 00097.001 
//                                                                                        // 00098.001 
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1                                          // 00099.001 
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0                      // 00100.001 
//  +---+-+-+-----------------------+-------------------------------+                     // 00101.001 
//  |Sev|C|R|     Facility          |               Code            |                     // 00102.001 
//  +---+-+-+-----------------------+-------------------------------+                     // 00103.001 
//                                                                                        // 00104.001 
//  where                                                                                 // 00105.001 
//                                                                                        // 00106.001 
//      Sev - is the severity code                                                        // 00107.001 
//                                                                                        // 00108.001 
//          00 - Success                                                                  // 00109.001 
//          01 - Informational                                                            // 00110.001 
//          10 - Warning                                                                  // 00111.001 
//          11 - Error                                                                    // 00112.001 
//                                                                                        // 00113.001 
//      C - is the Customer code flag                                                     // 00114.001 
//                                                                                        // 00115.001 
//      R - is a reserved bit                                                             // 00116.001 
//                                                                                        // 00117.001 
//      Facility - is the facility code                                                   // 00118.001 
//                                                                                        // 00119.001 
//      Code - is the facility's status code                                              // 00120.001 
//                                                                                        // 00121.001 
//                                                                                        // 00122.001 
// Define the facility codes                                                              // 00123.001 
//                                                                                        // 00124.001 
  FACILITY_MF_WIN32                     = $7;                                             // 00125.001#define FACILITY_MF_WIN32                0x7
  FACILITY_MF                           = $D;                                             // 00126.001#define FACILITY_MF                      0xD
                                                                                          // 00127.001 
                                                                                          // 00128.001 
//                                                                                        // 00129.001 
// Define the severity codes                                                              // 00130.001 
//                                                                                        // 00131.001 
  STATUS_SEVERITY_WARNING               = $2;                                             // 00132.001#define STATUS_SEVERITY_WARNING          0x2
  STATUS_SEVERITY_SUCCESS               = $0;                                             // 00133.001#define STATUS_SEVERITY_SUCCESS          0x0
  STATUS_SEVERITY_INFORMATIONAL         = $1;                                             // 00134.001#define STATUS_SEVERITY_INFORMATIONAL    0x1
  STATUS_SEVERITY_ERROR                 = $3;                                             // 00135.001#define STATUS_SEVERITY_ERROR            0x3
                                                                                          // 00136.001 
                                                                                          // 00137.001 
//                                                                                        // 00138.001 
// MessageId: MF_E_PLATFORM_NOT_INITIALIZED                                               // 00139.001 
//                                                                                        // 00140.001 
// MessageText:                                                                           // 00141.001 
//                                                                                        // 00142.001 
// Platform not initialized. Please call MFStartup().%0                                   // 00143.001 
//                                                                                        // 00144.001 
  MF_E_PLATFORM_NOT_INITIALIZED         = _HRESULT_TYPEDEF_($C00D36B0);                   // 00145.001#define MF_E_PLATFORM_NOT_INITIALIZED    _HRESULT_TYPEDEF_(0xC00D36B0L)
                                                                                          // 00146.001 
//                                                                                        // 00147.001 
// MessageId: MF_E_BUFFERTOOSMALL                                                         // 00148.001 
//                                                                                        // 00149.001 
// MessageText:                                                                           // 00150.001 
//                                                                                        // 00151.001 
// The buffer was too small to carry out the requested action.%0                          // 00152.001 
//                                                                                        // 00153.001 
  MF_E_BUFFERTOOSMALL                   = _HRESULT_TYPEDEF_($C00D36B1);                   // 00154.001#define MF_E_BUFFERTOOSMALL              _HRESULT_TYPEDEF_(0xC00D36B1L)
                                                                                          // 00155.001 
//                                                                                        // 00156.001 
// MessageId: MF_E_INVALIDREQUEST                                                         // 00157.001 
//                                                                                        // 00158.001 
// MessageText:                                                                           // 00159.001 
//                                                                                        // 00160.001 
// The request is invalid in the current state.%0                                         // 00161.001 
//                                                                                        // 00162.001 
  MF_E_INVALIDREQUEST                   = _HRESULT_TYPEDEF_($C00D36B2);                   // 00163.001#define MF_E_INVALIDREQUEST              _HRESULT_TYPEDEF_(0xC00D36B2L)
                                                                                          // 00164.001 
//                                                                                        // 00165.001 
// MessageId: MF_E_INVALIDSTREAMNUMBER                                                    // 00166.001 
//                                                                                        // 00167.001 
// MessageText:                                                                           // 00168.001 
//                                                                                        // 00169.001 
// The stream number provided was invalid.%0                                              // 00170.001 
//                                                                                        // 00171.001 
  MF_E_INVALIDSTREAMNUMBER              = _HRESULT_TYPEDEF_($C00D36B3);                   // 00172.001#define MF_E_INVALIDSTREAMNUMBER         _HRESULT_TYPEDEF_(0xC00D36B3L)
                                                                                          // 00173.001 
//                                                                                        // 00174.001 
// MessageId: MF_E_INVALIDMEDIATYPE                                                       // 00175.001 
//                                                                                        // 00176.001 
// MessageText:                                                                           // 00177.001 
//                                                                                        // 00178.001 
// The data specified for the media type is invalid, inconsistent, or not supported by this object.%0 // 00179.001 
//                                                                                        // 00180.001 
  MF_E_INVALIDMEDIATYPE                 = _HRESULT_TYPEDEF_($C00D36B4);                   // 00181.001#define MF_E_INVALIDMEDIATYPE            _HRESULT_TYPEDEF_(0xC00D36B4L)
                                                                                          // 00182.001 
//                                                                                        // 00183.001 
// MessageId: MF_E_NOTACCEPTING                                                           // 00184.001 
//                                                                                        // 00185.001 
// MessageText:                                                                           // 00186.001 
//                                                                                        // 00187.001 
// The callee is currently not accepting further input.%0                                 // 00188.001 
//                                                                                        // 00189.001 
  MF_E_NOTACCEPTING                     = _HRESULT_TYPEDEF_($C00D36B5);                   // 00190.001#define MF_E_NOTACCEPTING                _HRESULT_TYPEDEF_(0xC00D36B5L)
                                                                                          // 00191.001 
//                                                                                        // 00192.001 
// MessageId: MF_E_NOT_INITIALIZED                                                        // 00193.001 
//                                                                                        // 00194.001 
// MessageText:                                                                           // 00195.001 
//                                                                                        // 00196.001 
// This object needs to be initialized before the requested operation can be carried out.%0 // 00197.001 
//                                                                                        // 00198.001 
  MF_E_NOT_INITIALIZED                  = _HRESULT_TYPEDEF_($C00D36B6);                   // 00199.001#define MF_E_NOT_INITIALIZED             _HRESULT_TYPEDEF_(0xC00D36B6L)
                                                                                          // 00200.001 
//                                                                                        // 00201.001 
// MessageId: MF_E_UNSUPPORTED_REPRESENTATION                                             // 00202.001 
//                                                                                        // 00203.001 
// MessageText:                                                                           // 00204.001 
//                                                                                        // 00205.001 
// The requested representation is not supported by this object.%0                        // 00206.001 
//                                                                                        // 00207.001 
  MF_E_UNSUPPORTED_REPRESENTATION       = _HRESULT_TYPEDEF_($C00D36B7);                   // 00208.001#define MF_E_UNSUPPORTED_REPRESENTATION  _HRESULT_TYPEDEF_(0xC00D36B7L)
                                                                                          // 00209.001 
//                                                                                        // 00210.001 
// MessageId: MF_E_NO_MORE_TYPES                                                          // 00211.001 
//                                                                                        // 00212.001 
// MessageText:                                                                           // 00213.001 
//                                                                                        // 00214.001 
// An object ran out of media types to suggest therefore the requested chain of streaming objects cannot be completed.%0 // 00215.001 
//                                                                                        // 00216.001 
  MF_E_NO_MORE_TYPES                    = _HRESULT_TYPEDEF_($C00D36B9);                   // 00217.001#define MF_E_NO_MORE_TYPES               _HRESULT_TYPEDEF_(0xC00D36B9L)
                                                                                          // 00218.001 
//                                                                                        // 00219.001 
// MessageId: MF_E_UNSUPPORTED_SERVICE                                                    // 00220.001 
//                                                                                        // 00221.001 
// MessageText:                                                                           // 00222.001 
//                                                                                        // 00223.001 
// The object does not support the specified service.%0                                   // 00224.001
//                                                                                        // 00225.001 
  MF_E_UNSUPPORTED_SERVICE              = _HRESULT_TYPEDEF_($C00D36BA);                   // 00226.001#define MF_E_UNSUPPORTED_SERVICE         _HRESULT_TYPEDEF_(0xC00D36BAL)
                                                                                          // 00227.001 
//                                                                                        // 00228.001 
// MessageId: MF_E_UNEXPECTED                                                             // 00229.001 
//                                                                                        // 00230.001 
// MessageText:                                                                           // 00231.001 
//                                                                                        // 00232.001
// An unexpected error has occurred in the operation requested.%0                         // 00233.001
//                                                                                        // 00234.001 
  MF_E_UNEXPECTED                       = _HRESULT_TYPEDEF_($C00D36BB);                   // 00235.001#define MF_E_UNEXPECTED                  _HRESULT_TYPEDEF_(0xC00D36BBL)
                                                                                          // 00236.001
//                                                                                        // 00237.001 
// MessageId: MF_E_INVALIDNAME                                                            // 00238.001 
//                                                                                        // 00239.001 
// MessageText:                                                                           // 00240.001 
//                                                                                        // 00241.001 
// Invalid name.%0                                                                        // 00242.001 
//                                                                                        // 00243.001 
  MF_E_INVALIDNAME                      = _HRESULT_TYPEDEF_($C00D36BC);                   // 00244.001#define MF_E_INVALIDNAME                 _HRESULT_TYPEDEF_(0xC00D36BCL)
                                                                                          // 00245.001 
//                                                                                        // 00246.001 
// MessageId: MF_E_INVALIDTYPE                                                            // 00247.001 
//                                                                                        // 00248.001 
// MessageText:                                                                           // 00249.001 
//                                                                                        // 00250.001 
// Invalid type.%0                                                                        // 00251.001 
//                                                                                        // 00252.001 
  MF_E_INVALIDTYPE                      = _HRESULT_TYPEDEF_($C00D36BD);                   // 00253.001#define MF_E_INVALIDTYPE                 _HRESULT_TYPEDEF_(0xC00D36BDL)
                                                                                          // 00254.001 
//                                                                                        // 00255.001 
// MessageId: MF_E_INVALID_FILE_FORMAT                                                    // 00256.001 
//                                                                                        // 00257.001 
// MessageText:                                                                           // 00258.001 
//                                                                                        // 00259.001 
// The file does not conform to the relevant file format specification.                   // 00260.001 
//                                                                                        // 00261.001 
  MF_E_INVALID_FILE_FORMAT              = _HRESULT_TYPEDEF_($C00D36BE);                   // 00262.001#define MF_E_INVALID_FILE_FORMAT         _HRESULT_TYPEDEF_(0xC00D36BEL)
                                                                                          // 00263.001 
//                                                                                        // 00264.001 
// MessageId: MF_E_INVALIDINDEX                                                           // 00265.001 
//                                                                                        // 00266.001 
// MessageText:                                                                           // 00267.001 
//                                                                                        // 00268.001 
// Invalid index.%0                                                                       // 00269.001 
//                                                                                        // 00270.001 
  MF_E_INVALIDINDEX                     = _HRESULT_TYPEDEF_($C00D36BF);                   // 00271.001#define MF_E_INVALIDINDEX                _HRESULT_TYPEDEF_(0xC00D36BFL)
                                                                                          // 00272.001 
//                                                                                        // 00273.001 
// MessageId: MF_E_INVALID_TIMESTAMP                                                      // 00274.001 
//                                                                                        // 00275.001 
// MessageText:                                                                           // 00276.001 
//                                                                                        // 00277.001 
// An invalid timestamp was given.%0                                                      // 00278.001 
//                                                                                        // 00279.001 
  MF_E_INVALID_TIMESTAMP                = _HRESULT_TYPEDEF_($C00D36C0);                   // 00280.001#define MF_E_INVALID_TIMESTAMP           _HRESULT_TYPEDEF_(0xC00D36C0L)
                                                                                          // 00281.001 
//                                                                                        // 00282.001 
// MessageId: MF_E_UNSUPPORTED_SCHEME                                                     // 00283.001 
//                                                                                        // 00284.001 
// MessageText:                                                                           // 00285.001 
//                                                                                        // 00286.001 
// The scheme of the given URL is unsupported.%0                                          // 00287.001 
//                                                                                        // 00288.001 
  MF_E_UNSUPPORTED_SCHEME               = _HRESULT_TYPEDEF_($C00D36C3);                   // 00289.001#define MF_E_UNSUPPORTED_SCHEME          _HRESULT_TYPEDEF_(0xC00D36C3L)
                                                                                          // 00290.001 
//                                                                                        // 00291.001 
// MessageId: MF_E_UNSUPPORTED_BYTESTREAM_TYPE                                            // 00292.001 
//                                                                                        // 00293.001 
// MessageText:                                                                           // 00294.001 
//                                                                                        // 00295.001 
// The byte stream type of the given URL is unsupported.%0                                // 00296.001 
//                                                                                        // 00297.001 
  MF_E_UNSUPPORTED_BYTESTREAM_TYPE      = _HRESULT_TYPEDEF_($C00D36C4);                   // 00298.001#define MF_E_UNSUPPORTED_BYTESTREAM_TYPE _HRESULT_TYPEDEF_(0xC00D36C4L)
                                                                                          // 00299.001 
//                                                                                        // 00300.001 
// MessageId: MF_E_UNSUPPORTED_TIME_FORMAT                                                // 00301.001 
//                                                                                        // 00302.001 
// MessageText:                                                                           // 00303.001 
//                                                                                        // 00304.001 
// The given time format is unsupported.%0                                                // 00305.001 
//                                                                                        // 00306.001 
  MF_E_UNSUPPORTED_TIME_FORMAT          = _HRESULT_TYPEDEF_($C00D36C5);                   // 00307.001#define MF_E_UNSUPPORTED_TIME_FORMAT     _HRESULT_TYPEDEF_(0xC00D36C5L)
                                                                                          // 00308.001 
//                                                                                        // 00309.001 
// MessageId: MF_E_NO_SAMPLE_TIMESTAMP                                                    // 00310.001 
//                                                                                        // 00311.001 
// MessageText:                                                                           // 00312.001 
//                                                                                        // 00313.001 
// The Media Sample does not have a timestamp.%0                                          // 00314.001 
//                                                                                        // 00315.001 
  MF_E_NO_SAMPLE_TIMESTAMP              = _HRESULT_TYPEDEF_($C00D36C8);                   // 00316.001#define MF_E_NO_SAMPLE_TIMESTAMP         _HRESULT_TYPEDEF_(0xC00D36C8L)
                                                                                          // 00317.001 
//                                                                                        // 00318.001 
// MessageId: MF_E_NO_SAMPLE_DURATION                                                     // 00319.001 
//                                                                                        // 00320.001 
// MessageText:                                                                           // 00321.001 
//                                                                                        // 00322.001 
// The Media Sample does not have a duration.%0                                           // 00323.001 
//                                                                                        // 00324.001 
  MF_E_NO_SAMPLE_DURATION               = _HRESULT_TYPEDEF_($C00D36C9);                   // 00325.001#define MF_E_NO_SAMPLE_DURATION          _HRESULT_TYPEDEF_(0xC00D36C9L)
                                                                                          // 00326.001 
//                                                                                        // 00327.001 
// MessageId: MF_E_INVALID_STREAM_DATA                                                    // 00328.001 
//                                                                                        // 00329.001 
// MessageText:                                                                           // 00330.001 
//                                                                                        // 00331.001 
// The request failed because the data in the stream is corrupt.%0\n.                     // 00332.001 
//                                                                                        // 00333.001 
  MF_E_INVALID_STREAM_DATA              = _HRESULT_TYPEDEF_($C00D36CB);                   // 00334.001#define MF_E_INVALID_STREAM_DATA         _HRESULT_TYPEDEF_(0xC00D36CBL)
                                                                                          // 00335.001 
//                                                                                        // 00336.001 
// MessageId: MF_E_RT_UNAVAILABLE                                                         // 00337.001 
//                                                                                        // 00338.001 
// MessageText:                                                                           // 00339.001 
//                                                                                        // 00340.001 
// Real time services are not available.%0                                                // 00341.001 
//                                                                                        // 00342.001 
  MF_E_RT_UNAVAILABLE                   = _HRESULT_TYPEDEF_($C00D36CF);                   // 00343.001#define MF_E_RT_UNAVAILABLE              _HRESULT_TYPEDEF_(0xC00D36CFL)
                                                                                          // 00344.001 
//                                                                                        // 00345.001 
// MessageId: MF_E_UNSUPPORTED_RATE                                                       // 00346.001 
//                                                                                        // 00347.001 
// MessageText:                                                                           // 00348.001 
//                                                                                        // 00349.001 
// The specified rate is not supported.%0                                                 // 00350.001 
//                                                                                        // 00351.001 
  MF_E_UNSUPPORTED_RATE                 = _HRESULT_TYPEDEF_($C00D36D0);                   // 00352.001#define MF_E_UNSUPPORTED_RATE            _HRESULT_TYPEDEF_(0xC00D36D0L)
                                                                                          // 00353.001 
//                                                                                        // 00354.001 
// MessageId: MF_E_THINNING_UNSUPPORTED                                                   // 00355.001 
//                                                                                        // 00356.001 
// MessageText:                                                                           // 00357.001 
//                                                                                        // 00358.001 
// This component does not support stream-thinning.%0                                     // 00359.001 
//                                                                                        // 00360.001 
  MF_E_THINNING_UNSUPPORTED             = _HRESULT_TYPEDEF_($C00D36D1);                   // 00361.001#define MF_E_THINNING_UNSUPPORTED        _HRESULT_TYPEDEF_(0xC00D36D1L)
                                                                                          // 00362.001 
//                                                                                        // 00363.001 
// MessageId: MF_E_REVERSE_UNSUPPORTED                                                    // 00364.001 
//                                                                                        // 00365.001 
// MessageText:                                                                           // 00366.001 
//                                                                                        // 00367.001 
// The call failed because no reverse playback rates are available.%0                     // 00368.001 
//                                                                                        // 00369.001 
  MF_E_REVERSE_UNSUPPORTED              = _HRESULT_TYPEDEF_($C00D36D2);                   // 00370.001#define MF_E_REVERSE_UNSUPPORTED         _HRESULT_TYPEDEF_(0xC00D36D2L)
                                                                                          // 00371.001 
//                                                                                        // 00372.001 
// MessageId: MF_E_UNSUPPORTED_RATE_TRANSITION                                            // 00373.001 
//                                                                                        // 00374.001 
// MessageText:                                                                           // 00375.001 
//                                                                                        // 00376.001 
// The requested rate transition cannot occur in the current state.%0                     // 00377.001 
//                                                                                        // 00378.001 
  MF_E_UNSUPPORTED_RATE_TRANSITION      = _HRESULT_TYPEDEF_($C00D36D3);                   // 00379.001#define MF_E_UNSUPPORTED_RATE_TRANSITION _HRESULT_TYPEDEF_(0xC00D36D3L)
                                                                                          // 00380.001 
//                                                                                        // 00381.001 
// MessageId: MF_E_RATE_CHANGE_PREEMPTED                                                  // 00382.001 
//                                                                                        // 00383.001 
// MessageText:                                                                           // 00384.001 
//                                                                                        // 00385.001 
// The requested rate change has been pre-empted and will not occur.%0                    // 00386.001 
//                                                                                        // 00387.001 
  MF_E_RATE_CHANGE_PREEMPTED            = _HRESULT_TYPEDEF_($C00D36D4);                   // 00388.001#define MF_E_RATE_CHANGE_PREEMPTED       _HRESULT_TYPEDEF_(0xC00D36D4L)
                                                                                          // 00389.001 
//                                                                                        // 00390.001 
// MessageId: MF_E_NOT_FOUND                                                              // 00391.001 
//                                                                                        // 00392.001 
// MessageText:                                                                           // 00393.001 
//                                                                                        // 00394.001 
// The specified object or value does not exist.%0                                        // 00395.001 
//                                                                                        // 00396.001 
  MF_E_NOT_FOUND                        = _HRESULT_TYPEDEF_($C00D36D5);                   // 00397.001#define MF_E_NOT_FOUND                   _HRESULT_TYPEDEF_(0xC00D36D5L)
                                                                                          // 00398.001 
//                                                                                        // 00399.001 
// MessageId: MF_E_NOT_AVAILABLE                                                          // 00400.001 
//                                                                                        // 00401.001 
// MessageText:                                                                           // 00402.001 
//                                                                                        // 00403.001 
// The requested value is not available.%0                                                // 00404.001 
//                                                                                        // 00405.001 
  MF_E_NOT_AVAILABLE                    = _HRESULT_TYPEDEF_($C00D36D6);                   // 00406.001#define MF_E_NOT_AVAILABLE               _HRESULT_TYPEDEF_(0xC00D36D6L)
                                                                                          // 00407.001 
//                                                                                        // 00408.001 
// MessageId: MF_E_NO_CLOCK                                                               // 00409.001 
//                                                                                        // 00410.001 
// MessageText:                                                                           // 00411.001 
//                                                                                        // 00412.001 
// The specified operation requires a clock and no clock is available.%0                  // 00413.001 
//                                                                                        // 00414.001 
  MF_E_NO_CLOCK                         = _HRESULT_TYPEDEF_($C00D36D7);                   // 00415.001#define MF_E_NO_CLOCK                    _HRESULT_TYPEDEF_(0xC00D36D7L)
                                                                                          // 00416.001 
//                                                                                        // 00417.001 
// MessageId: MF_S_MULTIPLE_BEGIN                                                         // 00418.001 
//                                                                                        // 00419.001 
// MessageText:                                                                           // 00420.001 
//                                                                                        // 00421.001 
// This callback and state had already been passed in to this event generator earlier.%0  // 00422.001 
//                                                                                        // 00423.001 
  MF_S_MULTIPLE_BEGIN                   = _HRESULT_TYPEDEF_($000D36D8);                   // 00424.001#define MF_S_MULTIPLE_BEGIN              _HRESULT_TYPEDEF_(0x000D36D8L)
                                                                                          // 00425.001 
//                                                                                        // 00426.001 
// MessageId: MF_E_MULTIPLE_BEGIN                                                         // 00427.001 
//                                                                                        // 00428.001 
// MessageText:                                                                           // 00429.001 
//                                                                                        // 00430.001 
// This callback has already been passed in to this event generator.%0                    // 00431.001 
//                                                                                        // 00432.001 
  MF_E_MULTIPLE_BEGIN                   = _HRESULT_TYPEDEF_($C00D36D9);                   // 00433.001#define MF_E_MULTIPLE_BEGIN              _HRESULT_TYPEDEF_(0xC00D36D9L)
                                                                                          // 00434.001 
//                                                                                        // 00435.001 
// MessageId: MF_E_MULTIPLE_SUBSCRIBERS                                                   // 00436.001 
//                                                                                        // 00437.001 
// MessageText:                                                                           // 00438.001 
//                                                                                        // 00439.001 
// Some component is already listening to events on this event generator.%0               // 00440.001 
//                                                                                        // 00441.001 
  MF_E_MULTIPLE_SUBSCRIBERS             = _HRESULT_TYPEDEF_($C00D36DA);                   // 00442.001#define MF_E_MULTIPLE_SUBSCRIBERS        _HRESULT_TYPEDEF_(0xC00D36DAL)
                                                                                          // 00443.001 
//                                                                                        // 00444.001 
// MessageId: MF_E_TIMER_ORPHANED                                                         // 00445.001 
//                                                                                        // 00446.001 
// MessageText:                                                                           // 00447.001 
//                                                                                        // 00448.001 
// This timer was orphaned before its callback time arrived.%0                            // 00449.001 
//                                                                                        // 00450.001 
  MF_E_TIMER_ORPHANED                   = _HRESULT_TYPEDEF_($C00D36DB);                   // 00451.001#define MF_E_TIMER_ORPHANED              _HRESULT_TYPEDEF_(0xC00D36DBL)
                                                                                          // 00452.001 
//                                                                                        // 00453.001 
// MessageId: MF_E_STATE_TRANSITION_PENDING                                               // 00454.001 
//                                                                                        // 00455.001 
// MessageText:                                                                           // 00456.001 
//                                                                                        // 00457.001 
// A state transition is already pending.%0                                               // 00458.001 
//                                                                                        // 00459.001 
  MF_E_STATE_TRANSITION_PENDING         = _HRESULT_TYPEDEF_($C00D36DC);                   // 00460.001#define MF_E_STATE_TRANSITION_PENDING    _HRESULT_TYPEDEF_(0xC00D36DCL)
                                                                                          // 00461.001 
//                                                                                        // 00462.001 
// MessageId: MF_E_UNSUPPORTED_STATE_TRANSITION                                           // 00463.001 
//                                                                                        // 00464.001 
// MessageText:                                                                           // 00465.001 
//                                                                                        // 00466.001 
// The requested state transition is unsupported.%0                                       // 00467.001 
//                                                                                        // 00468.001 
  MF_E_UNSUPPORTED_STATE_TRANSITION     = _HRESULT_TYPEDEF_($C00D36DD);                   // 00469.001#define MF_E_UNSUPPORTED_STATE_TRANSITION _HRESULT_TYPEDEF_(0xC00D36DDL)
                                                                                          // 00470.001 
//                                                                                        // 00471.001 
// MessageId: MF_E_UNRECOVERABLE_ERROR_OCCURRED                                           // 00472.001 
//                                                                                        // 00473.001 
// MessageText:                                                                           // 00474.001 
//                                                                                        // 00475.001 
// An unrecoverable error has occurred.%0                                                 // 00476.001 
//                                                                                        // 00477.001 
  MF_E_UNRECOVERABLE_ERROR_OCCURRED     = _HRESULT_TYPEDEF_($C00D36DE);                   // 00478.001#define MF_E_UNRECOVERABLE_ERROR_OCCURRED _HRESULT_TYPEDEF_(0xC00D36DEL)
                                                                                          // 00479.001 
//                                                                                        // 00480.001 
// MessageId: MF_E_SAMPLE_HAS_TOO_MANY_BUFFERS                                            // 00481.001 
//                                                                                        // 00482.001 
// MessageText:                                                                           // 00483.001 
//                                                                                        // 00484.001 
// The provided sample has too many buffers.%0                                            // 00485.001 
//                                                                                        // 00486.001 
  MF_E_SAMPLE_HAS_TOO_MANY_BUFFERS      = _HRESULT_TYPEDEF_($C00D36DF);                   // 00487.001#define MF_E_SAMPLE_HAS_TOO_MANY_BUFFERS _HRESULT_TYPEDEF_(0xC00D36DFL)
                                                                                          // 00488.001 
//                                                                                        // 00489.001 
// MessageId: MF_E_SAMPLE_NOT_WRITABLE                                                    // 00490.001 
//                                                                                        // 00491.001 
// MessageText:                                                                           // 00492.001 
//                                                                                        // 00493.001 
// The provided sample is not writable.%0                                                 // 00494.001 
//                                                                                        // 00495.001 
  MF_E_SAMPLE_NOT_WRITABLE              = _HRESULT_TYPEDEF_($C00D36E0);                   // 00496.001#define MF_E_SAMPLE_NOT_WRITABLE         _HRESULT_TYPEDEF_(0xC00D36E0L)
                                                                                          // 00497.001 
//                                                                                        // 00498.001 
// MessageId: MF_E_INVALID_KEY                                                            // 00499.001 
//                                                                                        // 00500.001 
// MessageText:                                                                           // 00501.001 
//                                                                                        // 00502.001 
// The specified key is not valid.                                                        // 00503.001 
//                                                                                        // 00504.001 
  MF_E_INVALID_KEY                      = _HRESULT_TYPEDEF_($C00D36E2);                   // 00505.001#define MF_E_INVALID_KEY                 _HRESULT_TYPEDEF_(0xC00D36E2L)
                                                                                          // 00506.001 
//                                                                                        // 00507.001 
// MessageId: MF_E_BAD_STARTUP_VERSION                                                    // 00508.001 
//                                                                                        // 00509.001 
// MessageText:                                                                           // 00510.001 
//                                                                                        // 00511.001 
// You are calling MFStartup with the wrong MF_VERSION. Mismatched bits?                  // 00512.001 
//                                                                                        // 00513.001 
  MF_E_BAD_STARTUP_VERSION              = _HRESULT_TYPEDEF_($C00D36E3);                   // 00514.001#define MF_E_BAD_STARTUP_VERSION         _HRESULT_TYPEDEF_(0xC00D36E3L)
                                                                                          // 00515.001 
//                                                                                        // 00516.001 
// MessageId: MF_E_UNSUPPORTED_CAPTION                                                    // 00517.001 
//                                                                                        // 00518.001 
// MessageText:                                                                           // 00519.001 
//                                                                                        // 00520.001 
// The caption of the given URL is unsupported.%0                                         // 00521.001 
//                                                                                        // 00522.001 
  MF_E_UNSUPPORTED_CAPTION              = _HRESULT_TYPEDEF_($C00D36E4);                   // 00523.001#define MF_E_UNSUPPORTED_CAPTION         _HRESULT_TYPEDEF_(0xC00D36E4L)
                                                                                          // 00524.001 
//                                                                                        // 00525.001 
// MessageId: MF_E_INVALID_POSITION                                                       // 00526.001 
//                                                                                        // 00527.001 
// MessageText:                                                                           // 00528.001 
//                                                                                        // 00529.001 
// The operation on the current offset is not permitted.%0                                // 00530.001 
//                                                                                        // 00531.001 
  MF_E_INVALID_POSITION                 = _HRESULT_TYPEDEF_($C00D36E5);                   // 00532.001#define MF_E_INVALID_POSITION            _HRESULT_TYPEDEF_(0xC00D36E5L)
                                                                                          // 00533.001 
//                                                                                        // 00534.001 
// MessageId: MF_E_ATTRIBUTENOTFOUND                                                      // 00535.001 
//                                                                                        // 00536.001 
// MessageText:                                                                           // 00537.001 
//                                                                                        // 00538.001 
// The requested attribute was not found.%0                                               // 00539.001 
//                                                                                        // 00540.001 
  MF_E_ATTRIBUTENOTFOUND                = _HRESULT_TYPEDEF_($C00D36E6);                   // 00541.001#define MF_E_ATTRIBUTENOTFOUND           _HRESULT_TYPEDEF_(0xC00D36E6L)
                                                                                          // 00542.001 
//                                                                                        // 00543.001 
// MessageId: MF_E_PROPERTY_TYPE_NOT_ALLOWED                                              // 00544.001 
//                                                                                        // 00545.001 
// MessageText:                                                                           // 00546.001 
//                                                                                        // 00547.001 
// The specified property type is not allowed in this context.%0                          // 00548.001 
//                                                                                        // 00549.001 
  MF_E_PROPERTY_TYPE_NOT_ALLOWED        = _HRESULT_TYPEDEF_($C00D36E7);                   // 00550.001#define MF_E_PROPERTY_TYPE_NOT_ALLOWED   _HRESULT_TYPEDEF_(0xC00D36E7L)
                                                                                          // 00551.001 
//                                                                                        // 00552.001 
// MessageId: MF_E_PROPERTY_TYPE_NOT_SUPPORTED                                            // 00553.001 
//                                                                                        // 00554.001 
// MessageText:                                                                           // 00555.001 
//                                                                                        // 00556.001 
// The specified property type is not supported.%0                                        // 00557.001 
//                                                                                        // 00558.001 
  MF_E_PROPERTY_TYPE_NOT_SUPPORTED      = _HRESULT_TYPEDEF_($C00D36E8);                   // 00559.001#define MF_E_PROPERTY_TYPE_NOT_SUPPORTED _HRESULT_TYPEDEF_(0xC00D36E8L)
                                                                                          // 00560.001 
//                                                                                        // 00561.001 
// MessageId: MF_E_PROPERTY_EMPTY                                                         // 00562.001 
//                                                                                        // 00563.001 
// MessageText:                                                                           // 00564.001 
//                                                                                        // 00565.001 
// The specified property is empty.%0                                                     // 00566.001 
//                                                                                        // 00567.001 
  MF_E_PROPERTY_EMPTY                   = _HRESULT_TYPEDEF_($C00D36E9);                   // 00568.001#define MF_E_PROPERTY_EMPTY              _HRESULT_TYPEDEF_(0xC00D36E9L)
                                                                                          // 00569.001 
//                                                                                        // 00570.001 
// MessageId: MF_E_PROPERTY_NOT_EMPTY                                                     // 00571.001 
//                                                                                        // 00572.001 
// MessageText:                                                                           // 00573.001 
//                                                                                        // 00574.001 
// The specified property is not empty.%0                                                 // 00575.001 
//                                                                                        // 00576.001 
  MF_E_PROPERTY_NOT_EMPTY               = _HRESULT_TYPEDEF_($C00D36EA);                   // 00577.001#define MF_E_PROPERTY_NOT_EMPTY          _HRESULT_TYPEDEF_(0xC00D36EAL)
                                                                                          // 00578.001 
//                                                                                        // 00579.001 
// MessageId: MF_E_PROPERTY_VECTOR_NOT_ALLOWED                                            // 00580.001 
//                                                                                        // 00581.001 
// MessageText:                                                                           // 00582.001 
//                                                                                        // 00583.001 
// The vector property specified is not allowed in this context.%0                        // 00584.001 
//                                                                                        // 00585.001 
  MF_E_PROPERTY_VECTOR_NOT_ALLOWED      = _HRESULT_TYPEDEF_($C00D36EB);                   // 00586.001#define MF_E_PROPERTY_VECTOR_NOT_ALLOWED _HRESULT_TYPEDEF_(0xC00D36EBL)
                                                                                          // 00587.001 
//                                                                                        // 00588.001 
// MessageId: MF_E_PROPERTY_VECTOR_REQUIRED                                               // 00589.001 
//                                                                                        // 00590.001 
// MessageText:                                                                           // 00591.001 
//                                                                                        // 00592.001 
// A vector property is required in this context.%0                                       // 00593.001 
//                                                                                        // 00594.001 
  MF_E_PROPERTY_VECTOR_REQUIRED         = _HRESULT_TYPEDEF_($C00D36EC);                   // 00595.001#define MF_E_PROPERTY_VECTOR_REQUIRED    _HRESULT_TYPEDEF_(0xC00D36ECL)
                                                                                          // 00596.001 
//                                                                                        // 00597.001 
// MessageId: MF_E_OPERATION_CANCELLED                                                    // 00598.001 
//                                                                                        // 00599.001 
// MessageText:                                                                           // 00600.001 
//                                                                                        // 00601.001 
// The operation is cancelled.%0                                                          // 00602.001 
//                                                                                        // 00603.001 
  MF_E_OPERATION_CANCELLED              = _HRESULT_TYPEDEF_($C00D36ED);                   // 00604.001#define MF_E_OPERATION_CANCELLED         _HRESULT_TYPEDEF_(0xC00D36EDL)
                                                                                          // 00605.001 
//                                                                                        // 00606.001 
// MessageId: MF_E_BYTESTREAM_NOT_SEEKABLE                                                // 00607.001 
//                                                                                        // 00608.001 
// MessageText:                                                                           // 00609.001 
//                                                                                        // 00610.001 
// The provided bytestream was expected to be seekable and it is not.%0                   // 00611.001 
//                                                                                        // 00612.001 
  MF_E_BYTESTREAM_NOT_SEEKABLE          = _HRESULT_TYPEDEF_($C00D36EE);                   // 00613.001#define MF_E_BYTESTREAM_NOT_SEEKABLE     _HRESULT_TYPEDEF_(0xC00D36EEL)
                                                                                          // 00614.001 
//                                                                                        // 00615.001 
// MessageId: MF_E_DISABLED_IN_SAFEMODE                                                   // 00616.001 
//                                                                                        // 00617.001 
// MessageText:                                                                           // 00618.001 
//                                                                                        // 00619.001 
// The Media Foundation platform is disabled when the system is running in Safe Mode.%0   // 00620.001 
//                                                                                        // 00621.001 
  MF_E_DISABLED_IN_SAFEMODE             = _HRESULT_TYPEDEF_($C00D36EF);                   // 00622.001#define MF_E_DISABLED_IN_SAFEMODE        _HRESULT_TYPEDEF_(0xC00D36EFL)
                                                                                          // 00623.001 
//                                                                                        // 00624.001 
// MessageId: MF_E_CANNOT_PARSE_BYTESTREAM                                                // 00625.001 
//                                                                                        // 00626.001 
// MessageText:                                                                           // 00627.001 
//                                                                                        // 00628.001 
// The Media Source could not parse the byte stream.%0                                    // 00629.001 
//                                                                                        // 00630.001 
  MF_E_CANNOT_PARSE_BYTESTREAM          = _HRESULT_TYPEDEF_($C00D36F0);                   // 00631.001#define MF_E_CANNOT_PARSE_BYTESTREAM     _HRESULT_TYPEDEF_(0xC00D36F0L)
                                                                                          // 00632.001 
//                                                                                        // 00633.001 
// MessageId: MF_E_SOURCERESOLVER_MUTUALLY_EXCLUSIVE_FLAGS                                // 00634.001 
//                                                                                        // 00635.001 
// MessageText:                                                                           // 00636.001 
//                                                                                        // 00637.001 
// Mutually exclusive flags have been specified to source resolver. This flag combination is invalid.%0 // 00638.001 
//                                                                                        // 00639.001 
  MF_E_SOURCERESOLVER_MUTUALLY_EXCLUSIVE_FLAGS = _HRESULT_TYPEDEF_($C00D36F1);            // 00640.001#define MF_E_SOURCERESOLVER_MUTUALLY_EXCLUSIVE_FLAGS _HRESULT_TYPEDEF_(0xC00D36F1L)
                                                                                          // 00641.001 
//                                                                                        // 00642.001 
// MessageId: MF_E_MEDIAPROC_WRONGSTATE                                                   // 00643.001 
//                                                                                        // 00644.001 
// MessageText:                                                                           // 00645.001 
//                                                                                        // 00646.001 
// MediaProc is in the wrong state%0                                                      // 00647.001 
//                                                                                        // 00648.001 
  MF_E_MEDIAPROC_WRONGSTATE             = _HRESULT_TYPEDEF_($C00D36F2);                   // 00649.001#define MF_E_MEDIAPROC_WRONGSTATE        _HRESULT_TYPEDEF_(0xC00D36F2L)
                                                                                          // 00650.001 
//                                                                                        // 00651.001 
// MessageId: MF_E_RT_THROUGHPUT_NOT_AVAILABLE                                            // 00652.001 
//                                                                                        // 00653.001 
// MessageText:                                                                           // 00654.001 
//                                                                                        // 00655.001 
// Real time I/O service can not provide requested throughput.%0                          // 00656.001 
//                                                                                        // 00657.001 
  MF_E_RT_THROUGHPUT_NOT_AVAILABLE      = _HRESULT_TYPEDEF_($C00D36F3);                   // 00658.001#define MF_E_RT_THROUGHPUT_NOT_AVAILABLE _HRESULT_TYPEDEF_(0xC00D36F3L)
                                                                                          // 00659.001 
//                                                                                        // 00660.001 
// MessageId: MF_E_RT_TOO_MANY_CLASSES                                                    // 00661.001 
//                                                                                        // 00662.001 
// MessageText:                                                                           // 00663.001 
//                                                                                        // 00664.001 
// The workqueue cannot be registered with more classes.%0                                // 00665.001 
//                                                                                        // 00666.001 
  MF_E_RT_TOO_MANY_CLASSES              = _HRESULT_TYPEDEF_($C00D36F4);                   // 00667.001#define MF_E_RT_TOO_MANY_CLASSES         _HRESULT_TYPEDEF_(0xC00D36F4L)
                                                                                          // 00668.001 
//                                                                                        // 00669.001 
// MessageId: MF_E_RT_WOULDBLOCK                                                          // 00670.001 
//                                                                                        // 00671.001 
// MessageText:                                                                           // 00672.001 
//                                                                                        // 00673.001 
// This operation cannot succeed because another thread owns this object.%0               // 00674.001 
//                                                                                        // 00675.001 
  MF_E_RT_WOULDBLOCK                    = _HRESULT_TYPEDEF_($C00D36F5);                   // 00676.001#define MF_E_RT_WOULDBLOCK               _HRESULT_TYPEDEF_(0xC00D36F5L)
                                                                                          // 00677.001 
//                                                                                        // 00678.001 
// MessageId: MF_E_NO_BITPUMP                                                             // 00679.001 
//                                                                                        // 00680.001 
// MessageText:                                                                           // 00681.001 
//                                                                                        // 00682.001 
// Internal. Bitpump not found.%0                                                         // 00683.001 
//                                                                                        // 00684.001 
  MF_E_NO_BITPUMP                       = _HRESULT_TYPEDEF_($C00D36F6);                   // 00685.001#define MF_E_NO_BITPUMP                  _HRESULT_TYPEDEF_(0xC00D36F6L)
                                                                                          // 00686.001 
//                                                                                        // 00687.001 
// MessageId: MF_E_RT_OUTOFMEMORY                                                         // 00688.001 
//                                                                                        // 00689.001 
// MessageText:                                                                           // 00690.001 
//                                                                                        // 00691.001 
// No more RT memory available.%0                                                         // 00692.001 
//                                                                                        // 00693.001 
  MF_E_RT_OUTOFMEMORY                   = _HRESULT_TYPEDEF_($C00D36F7);                   // 00694.001#define MF_E_RT_OUTOFMEMORY              _HRESULT_TYPEDEF_(0xC00D36F7L)
                                                                                          // 00695.001 
//                                                                                        // 00696.001 
// MessageId: MF_E_RT_WORKQUEUE_CLASS_NOT_SPECIFIED                                       // 00697.001 
//                                                                                        // 00698.001 
// MessageText:                                                                           // 00699.001 
//                                                                                        // 00700.001 
// An MMCSS class has not been set for this work queue.%0                                 // 00701.001 
//                                                                                        // 00702.001 
  MF_E_RT_WORKQUEUE_CLASS_NOT_SPECIFIED = _HRESULT_TYPEDEF_($C00D36F8);                   // 00703.001#define MF_E_RT_WORKQUEUE_CLASS_NOT_SPECIFIED _HRESULT_TYPEDEF_(0xC00D36F8L)
                                                                                          // 00704.001 
//                                                                                        // 00705.001 
// MessageId: MF_E_INSUFFICIENT_BUFFER                                                    // 00706.001 
//                                                                                        // 00707.001 
// MessageText:                                                                           // 00708.001 
//                                                                                        // 00709.001 
// Insufficient memory for response.%0                                                    // 00710.001 
//                                                                                        // 00711.001 
  MF_E_INSUFFICIENT_BUFFER              = _HRESULT_TYPEDEF_($C00D7170);                   // 00712.001#define MF_E_INSUFFICIENT_BUFFER         _HRESULT_TYPEDEF_(0xC00D7170L)
                                                                                          // 00713.001 
//                                                                                        // 00714.001 
// MessageId: MF_E_CANNOT_CREATE_SINK                                                     // 00715.001 
//                                                                                        // 00716.001 
// MessageText:                                                                           // 00717.001 
//                                                                                        // 00718.001 
// Activate failed to create mediasink. Call OutputNode::GetUINT32(MF_TOPONODE_MAJORTYPE) for more information. %0 // 00719.001 
//                                                                                        // 00720.001 
  MF_E_CANNOT_CREATE_SINK               = _HRESULT_TYPEDEF_($C00D36FA);                   // 00721.001#define MF_E_CANNOT_CREATE_SINK          _HRESULT_TYPEDEF_(0xC00D36FAL)
                                                                                          // 00722.001 
//                                                                                        // 00723.001 
// MessageId: MF_E_BYTESTREAM_UNKNOWN_LENGTH                                              // 00724.001 
//                                                                                        // 00725.001 
// MessageText:                                                                           // 00726.001 
//                                                                                        // 00727.001 
// The length of the provided bytestream is unknown.%0                                    // 00728.001 
//                                                                                        // 00729.001 
  MF_E_BYTESTREAM_UNKNOWN_LENGTH        = _HRESULT_TYPEDEF_($C00D36FB);                   // 00730.001#define MF_E_BYTESTREAM_UNKNOWN_LENGTH   _HRESULT_TYPEDEF_(0xC00D36FBL)
                                                                                          // 00731.001 
//                                                                                        // 00732.001 
// MessageId: MF_E_SESSION_PAUSEWHILESTOPPED                                              // 00733.001 
//                                                                                        // 00734.001 
// MessageText:                                                                           // 00735.001 
//                                                                                        // 00736.001 
// The media session cannot pause from a stopped state.%0                                 // 00737.001 
//                                                                                        // 00738.001 
  MF_E_SESSION_PAUSEWHILESTOPPED        = _HRESULT_TYPEDEF_($C00D36FC);                   // 00739.001#define MF_E_SESSION_PAUSEWHILESTOPPED   _HRESULT_TYPEDEF_(0xC00D36FCL)
                                                                                          // 00740.001 
//                                                                                        // 00741.001 
// MessageId: MF_S_ACTIVATE_REPLACED                                                      // 00742.001 
//                                                                                        // 00743.001 
// MessageText:                                                                           // 00744.001 
//                                                                                        // 00745.001 
// The activate could not be created in the remote process for some reason it was replaced with empty one.%0 // 00746.001 
//                                                                                        // 00747.001 
  MF_S_ACTIVATE_REPLACED                = _HRESULT_TYPEDEF_($000D36FD);                   // 00748.001#define MF_S_ACTIVATE_REPLACED           _HRESULT_TYPEDEF_(0x000D36FDL)
                                                                                          // 00749.001 
//                                                                                        // 00750.001 
// MessageId: MF_E_FORMAT_CHANGE_NOT_SUPPORTED                                            // 00751.001 
//                                                                                        // 00752.001 
// MessageText:                                                                           // 00753.001 
//                                                                                        // 00754.001 
// The data specified for the media type is supported, but would require a format change, which is not supported by this object.%0 // 00755.001 
//                                                                                        // 00756.001 
  MF_E_FORMAT_CHANGE_NOT_SUPPORTED      = _HRESULT_TYPEDEF_($C00D36FE);                   // 00757.001#define MF_E_FORMAT_CHANGE_NOT_SUPPORTED _HRESULT_TYPEDEF_(0xC00D36FEL)
                                                                                          // 00758.001 
//                                                                                        // 00759.001 
// MessageId: MF_E_INVALID_WORKQUEUE                                                      // 00760.001 
//                                                                                        // 00761.001 
// MessageText:                                                                           // 00762.001 
//                                                                                        // 00763.001 
// The operation failed because an invalid combination of workqueue ID and flags was specified.%0 // 00764.001 
//                                                                                        // 00765.001 
  MF_E_INVALID_WORKQUEUE                = _HRESULT_TYPEDEF_($C00D36FF);                   // 00766.001#define MF_E_INVALID_WORKQUEUE           _HRESULT_TYPEDEF_(0xC00D36FFL)
                                                                                          // 00767.001 
//                                                                                        // 00768.001 
// MessageId: MF_E_DRM_UNSUPPORTED                                                        // 00769.001 
//                                                                                        // 00770.001 
// MessageText:                                                                           // 00771.001 
//                                                                                        // 00772.001 
// No DRM support is available.%0                                                         // 00773.001 
//                                                                                        // 00774.001 
  MF_E_DRM_UNSUPPORTED                  = _HRESULT_TYPEDEF_($C00D3700);                   // 00775.001#define MF_E_DRM_UNSUPPORTED             _HRESULT_TYPEDEF_(0xC00D3700L)
                                                                                          // 00776.001 
//                                                                                        // 00777.001 
// MessageId: MF_E_UNAUTHORIZED                                                           // 00778.001 
//                                                                                        // 00779.001 
// MessageText:                                                                           // 00780.001 
//                                                                                        // 00781.001 
// This operation is not authorized.%0                                                    // 00782.001 
//                                                                                        // 00783.001 
  MF_E_UNAUTHORIZED                     = _HRESULT_TYPEDEF_($C00D3701);                   // 00784.001#define MF_E_UNAUTHORIZED                _HRESULT_TYPEDEF_(0xC00D3701L)
                                                                                          // 00785.001 
//                                                                                        // 00786.001 
// MessageId: MF_E_OUT_OF_RANGE                                                           // 00787.001 
//                                                                                        // 00788.001 
// MessageText:                                                                           // 00789.001 
//                                                                                        // 00790.001 
// The value is not in the specified or valid range.%0                                    // 00791.001 
//                                                                                        // 00792.001 
  MF_E_OUT_OF_RANGE                     = _HRESULT_TYPEDEF_($C00D3702);                   // 00793.001#define MF_E_OUT_OF_RANGE                _HRESULT_TYPEDEF_(0xC00D3702L)
                                                                                          // 00794.001 
//                                                                                        // 00795.001 
// MessageId: MF_E_INVALID_CODEC_MERIT                                                    // 00796.001 
//                                                                                        // 00797.001 
// MessageText:                                                                           // 00798.001 
//                                                                                        // 00799.001 
// The registered codec merit is not valid.%0                                             // 00800.001 
//                                                                                        // 00801.001 
  MF_E_INVALID_CODEC_MERIT              = _HRESULT_TYPEDEF_($C00D3703);                   // 00802.001#define MF_E_INVALID_CODEC_MERIT         _HRESULT_TYPEDEF_(0xC00D3703L)
                                                                                          // 00803.001 
//                                                                                        // 00804.001 
// MessageId: MF_E_HW_MFT_FAILED_START_STREAMING                                          // 00805.001 
//                                                                                        // 00806.001 
// MessageText:                                                                           // 00807.001 
//                                                                                        // 00808.001 
// Hardware MFT failed to start streaming due to lack of hardware resources.%0            // 00809.001 
//                                                                                        // 00810.001 
  MF_E_HW_MFT_FAILED_START_STREAMING    = _HRESULT_TYPEDEF_($C00D3704);                   // 00811.001#define MF_E_HW_MFT_FAILED_START_STREAMING _HRESULT_TYPEDEF_(0xC00D3704L)
                                                                                          // 00812.001 
                                                                                          // 00813.001 
/////////////////////////////////////////////////////////////////////////                 // 00814.001 
//                                                                                        // 00815.001 
// MEDIAFOUNDATION ASF Parsing Informational Events                                       // 00816.001 
//                                                                                        // 00817.001 
/////////////////////////////////////////////////////////////////////////                 // 00818.001 
                                                                                          // 00819.001 
//                                                                                        // 00820.001 
// MessageId: MF_S_ASF_PARSEINPROGRESS                                                    // 00821.001 
//                                                                                        // 00822.001 
// MessageText:                                                                           // 00823.001 
//                                                                                        // 00824.001 
// Parsing is still in progress and is not yet complete.%0                                // 00825.001 
//                                                                                        // 00826.001 
  MF_S_ASF_PARSEINPROGRESS              = _HRESULT_TYPEDEF_($400D3A98);                   // 00827.001#define MF_S_ASF_PARSEINPROGRESS         _HRESULT_TYPEDEF_(0x400D3A98L)
                                                                                          // 00828.001 
                                                                                          // 00829.001 
/////////////////////////////////////////////////////////////////////////                 // 00830.001 
//                                                                                        // 00831.001 
// MEDIAFOUNDATION ASF Parsing Error Events                                               // 00832.001 
//                                                                                        // 00833.001 
/////////////////////////////////////////////////////////////////////////                 // 00834.001 
                                                                                          // 00835.001 
//                                                                                        // 00836.001 
// MessageId: MF_E_ASF_PARSINGINCOMPLETE                                                  // 00837.001 
//                                                                                        // 00838.001 
// MessageText:                                                                           // 00839.001 
//                                                                                        // 00840.001 
// Not enough data have been parsed to carry out the requested action.%0                  // 00841.001 
//                                                                                        // 00842.001 
  MF_E_ASF_PARSINGINCOMPLETE            = _HRESULT_TYPEDEF_($C00D3A98);                   // 00843.001#define MF_E_ASF_PARSINGINCOMPLETE       _HRESULT_TYPEDEF_(0xC00D3A98L)
                                                                                          // 00844.001 
//                                                                                        // 00845.001 
// MessageId: MF_E_ASF_MISSINGDATA                                                        // 00846.001 
//                                                                                        // 00847.001 
// MessageText:                                                                           // 00848.001 
//                                                                                        // 00849.001 
// There is a gap in the ASF data provided.%0                                             // 00850.001 
//                                                                                        // 00851.001 
  MF_E_ASF_MISSINGDATA                  = _HRESULT_TYPEDEF_($C00D3A99);                   // 00852.001#define MF_E_ASF_MISSINGDATA             _HRESULT_TYPEDEF_(0xC00D3A99L)
                                                                                          // 00853.001 
//                                                                                        // 00854.001 
// MessageId: MF_E_ASF_INVALIDDATA                                                        // 00855.001 
//                                                                                        // 00856.001 
// MessageText:                                                                           // 00857.001 
//                                                                                        // 00858.001 
// The data provided are not valid ASF.%0                                                 // 00859.001 
//                                                                                        // 00860.001 
  MF_E_ASF_INVALIDDATA                  = _HRESULT_TYPEDEF_($C00D3A9A);                   // 00861.001#define MF_E_ASF_INVALIDDATA             _HRESULT_TYPEDEF_(0xC00D3A9AL)
                                                                                          // 00862.001 
//                                                                                        // 00863.001 
// MessageId: MF_E_ASF_OPAQUEPACKET                                                       // 00864.001 
//                                                                                        // 00865.001 
// MessageText:                                                                           // 00866.001 
//                                                                                        // 00867.001 
// The packet is opaque, so the requested information cannot be returned.%0               // 00868.001 
//                                                                                        // 00869.001 
  MF_E_ASF_OPAQUEPACKET                 = _HRESULT_TYPEDEF_($C00D3A9B);                   // 00870.001#define MF_E_ASF_OPAQUEPACKET            _HRESULT_TYPEDEF_(0xC00D3A9BL)
                                                                                          // 00871.001 
//                                                                                        // 00872.001 
// MessageId: MF_E_ASF_NOINDEX                                                            // 00873.001 
//                                                                                        // 00874.001 
// MessageText:                                                                           // 00875.001 
//                                                                                        // 00876.001 
// The requested operation failed since there is no appropriate ASF index.%0              // 00877.001 
//                                                                                        // 00878.001 
  MF_E_ASF_NOINDEX                      = _HRESULT_TYPEDEF_($C00D3A9C);                   // 00879.001#define MF_E_ASF_NOINDEX                 _HRESULT_TYPEDEF_(0xC00D3A9CL)
                                                                                          // 00880.001 
//                                                                                        // 00881.001 
// MessageId: MF_E_ASF_OUTOFRANGE                                                         // 00882.001 
//                                                                                        // 00883.001 
// MessageText:                                                                           // 00884.001 
//                                                                                        // 00885.001 
// The value supplied is out of range for this operation.%0                               // 00886.001 
//                                                                                        // 00887.001 
  MF_E_ASF_OUTOFRANGE                   = _HRESULT_TYPEDEF_($C00D3A9D);                   // 00888.001#define MF_E_ASF_OUTOFRANGE              _HRESULT_TYPEDEF_(0xC00D3A9DL)
                                                                                          // 00889.001 
//                                                                                        // 00890.001 
// MessageId: MF_E_ASF_INDEXNOTLOADED                                                     // 00891.001 
//                                                                                        // 00892.001 
// MessageText:                                                                           // 00893.001 
//                                                                                        // 00894.001 
// The index entry requested needs to be loaded before it can be available.%0             // 00895.001 
//                                                                                        // 00896.001 
  MF_E_ASF_INDEXNOTLOADED               = _HRESULT_TYPEDEF_($C00D3A9E);                   // 00897.001#define MF_E_ASF_INDEXNOTLOADED          _HRESULT_TYPEDEF_(0xC00D3A9EL)    
                                                                                          // 00898.001 
//                                                                                        // 00899.001 
// MessageId: MF_E_ASF_TOO_MANY_PAYLOADS                                                  // 00900.001 
//                                                                                        // 00901.001 
// MessageText:                                                                           // 00902.001 
//                                                                                        // 00903.001 
// The packet has reached the maximum number of payloads.%0                               // 00904.001 
//                                                                                        // 00905.001 
  MF_E_ASF_TOO_MANY_PAYLOADS            = _HRESULT_TYPEDEF_($C00D3A9F);                   // 00906.001#define MF_E_ASF_TOO_MANY_PAYLOADS       _HRESULT_TYPEDEF_(0xC00D3A9FL)    
                                                                                          // 00907.001 
//                                                                                        // 00908.001 
// MessageId: MF_E_ASF_UNSUPPORTED_STREAM_TYPE                                            // 00909.001 
//                                                                                        // 00910.001 
// MessageText:                                                                           // 00911.001 
//                                                                                        // 00912.001 
// Stream type is not supported.%0                                                        // 00913.001 
//                                                                                        // 00914.001 
  MF_E_ASF_UNSUPPORTED_STREAM_TYPE      = _HRESULT_TYPEDEF_($C00D3AA0);                   // 00915.001#define MF_E_ASF_UNSUPPORTED_STREAM_TYPE _HRESULT_TYPEDEF_(0xC00D3AA0L)    
                                                                                          // 00916.001 
//                                                                                        // 00917.001 
// MessageId: MF_E_ASF_DROPPED_PACKET                                                     // 00918.001 
//                                                                                        // 00919.001 
// MessageText:                                                                           // 00920.001 
//                                                                                        // 00921.001 
// One or more ASF packets were dropped.%0                                                // 00922.001 
//                                                                                        // 00923.001 
  MF_E_ASF_DROPPED_PACKET               = _HRESULT_TYPEDEF_($C00D3AA1);                   // 00924.001#define MF_E_ASF_DROPPED_PACKET          _HRESULT_TYPEDEF_(0xC00D3AA1L)    
                                                                                          // 00925.001 
                                                                                          // 00926.001 
/////////////////////////////////////////////////////////////////////////                 // 00927.001 
//                                                                                        // 00928.001 
// MEDIAFOUNDATION Media Source Error Events                                              // 00929.001 
//                                                                                        // 00930.001 
/////////////////////////////////////////////////////////////////////////                 // 00931.001 
                                                                                          // 00932.001 
//                                                                                        // 00933.001 
// MessageId: MF_E_NO_EVENTS_AVAILABLE                                                    // 00934.001 
//                                                                                        // 00935.001 
// MessageText:                                                                           // 00936.001 
//                                                                                        // 00937.001 
// There are no events available in the queue.%0                                          // 00938.001 
//                                                                                        // 00939.001 
  MF_E_NO_EVENTS_AVAILABLE              = _HRESULT_TYPEDEF_($C00D3E80);                   // 00940.001#define MF_E_NO_EVENTS_AVAILABLE         _HRESULT_TYPEDEF_(0xC00D3E80L)
                                                                                          // 00941.001 
//                                                                                        // 00942.001 
// MessageId: MF_E_INVALID_STATE_TRANSITION                                               // 00943.001 
//                                                                                        // 00944.001 
// MessageText:                                                                           // 00945.001 
//                                                                                        // 00946.001 
// A media source cannot go from the stopped state to the paused state.%0                 // 00947.001 
//                                                                                        // 00948.001 
  MF_E_INVALID_STATE_TRANSITION         = _HRESULT_TYPEDEF_($C00D3E82);                   // 00949.001#define MF_E_INVALID_STATE_TRANSITION    _HRESULT_TYPEDEF_(0xC00D3E82L)
                                                                                          // 00950.001 
//                                                                                        // 00951.001 
// MessageId: MF_E_END_OF_STREAM                                                          // 00952.001 
//                                                                                        // 00953.001 
// MessageText:                                                                           // 00954.001 
//                                                                                        // 00955.001 
// The media stream cannot process any more samples because there are no more samples in the stream.%0 // 00956.001 
//                                                                                        // 00957.001 
  MF_E_END_OF_STREAM                    = _HRESULT_TYPEDEF_($C00D3E84);                   // 00958.001#define MF_E_END_OF_STREAM               _HRESULT_TYPEDEF_(0xC00D3E84L)
                                                                                          // 00959.001 
//                                                                                        // 00960.001 
// MessageId: MF_E_SHUTDOWN                                                               // 00961.001 
//                                                                                        // 00962.001 
// MessageText:                                                                           // 00963.001 
//                                                                                        // 00964.001 
// The request is invalid because Shutdown() has been called.%0                           // 00965.001 
//                                                                                        // 00966.001 
  MF_E_SHUTDOWN                         = _HRESULT_TYPEDEF_($C00D3E85);                   // 00967.001#define MF_E_SHUTDOWN                    _HRESULT_TYPEDEF_(0xC00D3E85L)
                                                                                          // 00968.001 
//                                                                                        // 00969.001 
// MessageId: MF_E_MP3_NOTFOUND                                                           // 00970.001 
//                                                                                        // 00971.001 
// MessageText:                                                                           // 00972.001 
//                                                                                        // 00973.001 
// The MP3 object was not found.%0                                                        // 00974.001 
//                                                                                        // 00975.001 
  MF_E_MP3_NOTFOUND                     = _HRESULT_TYPEDEF_($C00D3E86);                   // 00976.001#define MF_E_MP3_NOTFOUND                _HRESULT_TYPEDEF_(0xC00D3E86L)
                                                                                          // 00977.001 
//                                                                                        // 00978.001 
// MessageId: MF_E_MP3_OUTOFDATA                                                          // 00979.001 
//                                                                                        // 00980.001 
// MessageText:                                                                           // 00981.001 
//                                                                                        // 00982.001 
// The MP3 parser ran out of data before finding the MP3 object.%0                        // 00983.001 
//                                                                                        // 00984.001 
  MF_E_MP3_OUTOFDATA                    = _HRESULT_TYPEDEF_($C00D3E87);                   // 00985.001#define MF_E_MP3_OUTOFDATA               _HRESULT_TYPEDEF_(0xC00D3E87L)
                                                                                          // 00986.001 
//                                                                                        // 00987.001 
// MessageId: MF_E_MP3_NOTMP3                                                             // 00988.001 
//                                                                                        // 00989.001 
// MessageText:                                                                           // 00990.001 
//                                                                                        // 00991.001 
// The file is not really a MP3 file.%0                                                   // 00992.001 
//                                                                                        // 00993.001 
  MF_E_MP3_NOTMP3                       = _HRESULT_TYPEDEF_($C00D3E88);                   // 00994.001#define MF_E_MP3_NOTMP3                  _HRESULT_TYPEDEF_(0xC00D3E88L)
                                                                                          // 00995.001 
//                                                                                        // 00996.001 
// MessageId: MF_E_MP3_NOTSUPPORTED                                                       // 00997.001 
//                                                                                        // 00998.001 
// MessageText:                                                                           // 00999.001 
//                                                                                        // 01000.001 
// The MP3 file is not supported.%0                                                       // 01001.001 
//                                                                                        // 01002.001 
  MF_E_MP3_NOTSUPPORTED                 = _HRESULT_TYPEDEF_($C00D3E89);                   // 01003.001#define MF_E_MP3_NOTSUPPORTED            _HRESULT_TYPEDEF_(0xC00D3E89L)
                                                                                          // 01004.001 
//                                                                                        // 01005.001 
// MessageId: MF_E_NO_DURATION                                                            // 01006.001 
//                                                                                        // 01007.001 
// MessageText:                                                                           // 01008.001 
//                                                                                        // 01009.001 
// The Media stream has no duration.%0                                                    // 01010.001 
//                                                                                        // 01011.001 
  MF_E_NO_DURATION                      = _HRESULT_TYPEDEF_($C00D3E8A);                   // 01012.001#define MF_E_NO_DURATION                 _HRESULT_TYPEDEF_(0xC00D3E8AL)
                                                                                          // 01013.001 
//                                                                                        // 01014.001 
// MessageId: MF_E_INVALID_FORMAT                                                         // 01015.001 
//                                                                                        // 01016.001 
// MessageText:                                                                           // 01017.001 
//                                                                                        // 01018.001 
// The Media format is recognized but is invalid.%0                                       // 01019.001 
//                                                                                        // 01020.001 
  MF_E_INVALID_FORMAT                   = _HRESULT_TYPEDEF_($C00D3E8C);                   // 01021.001#define MF_E_INVALID_FORMAT              _HRESULT_TYPEDEF_(0xC00D3E8CL)
                                                                                          // 01022.001 
//                                                                                        // 01023.001 
// MessageId: MF_E_PROPERTY_NOT_FOUND                                                     // 01024.001 
//                                                                                        // 01025.001 
// MessageText:                                                                           // 01026.001 
//                                                                                        // 01027.001 
// The property requested was not found.%0                                                // 01028.001 
//                                                                                        // 01029.001 
  MF_E_PROPERTY_NOT_FOUND               = _HRESULT_TYPEDEF_($C00D3E8D);                   // 01030.001#define MF_E_PROPERTY_NOT_FOUND          _HRESULT_TYPEDEF_(0xC00D3E8DL)
                                                                                          // 01031.001 
//                                                                                        // 01032.001 
// MessageId: MF_E_PROPERTY_READ_ONLY                                                     // 01033.001 
//                                                                                        // 01034.001 
// MessageText:                                                                           // 01035.001 
//                                                                                        // 01036.001 
// The property is read only.%0                                                           // 01037.001 
//                                                                                        // 01038.001 
  MF_E_PROPERTY_READ_ONLY               = _HRESULT_TYPEDEF_($C00D3E8E);                   // 01039.001#define MF_E_PROPERTY_READ_ONLY          _HRESULT_TYPEDEF_(0xC00D3E8EL)
                                                                                          // 01040.001 
//                                                                                        // 01041.001 
// MessageId: MF_E_PROPERTY_NOT_ALLOWED                                                   // 01042.001 
//                                                                                        // 01043.001 
// MessageText:                                                                           // 01044.001 
//                                                                                        // 01045.001 
// The specified property is not allowed in this context.%0                               // 01046.001 
//                                                                                        // 01047.001 
  MF_E_PROPERTY_NOT_ALLOWED             = _HRESULT_TYPEDEF_($C00D3E8F);                   // 01048.001#define MF_E_PROPERTY_NOT_ALLOWED        _HRESULT_TYPEDEF_(0xC00D3E8FL)
                                                                                          // 01049.001 
//                                                                                        // 01050.001 
// MessageId: MF_E_MEDIA_SOURCE_NOT_STARTED                                               // 01051.001 
//                                                                                        // 01052.001 
// MessageText:                                                                           // 01053.001 
//                                                                                        // 01054.001 
// The media source is not started.%0                                                     // 01055.001 
//                                                                                        // 01056.001 
  MF_E_MEDIA_SOURCE_NOT_STARTED         = _HRESULT_TYPEDEF_($C00D3E91);                   // 01057.001#define MF_E_MEDIA_SOURCE_NOT_STARTED    _HRESULT_TYPEDEF_(0xC00D3E91L)
                                                                                          // 01058.001 
//                                                                                        // 01059.001 
// MessageId: MF_E_UNSUPPORTED_FORMAT                                                     // 01060.001 
//                                                                                        // 01061.001 
// MessageText:                                                                           // 01062.001 
//                                                                                        // 01063.001 
// The Media format is recognized but not supported.%0                                    // 01064.001 
//                                                                                        // 01065.001 
  MF_E_UNSUPPORTED_FORMAT               = _HRESULT_TYPEDEF_($C00D3E98);                   // 01066.001#define MF_E_UNSUPPORTED_FORMAT          _HRESULT_TYPEDEF_(0xC00D3E98L)
                                                                                          // 01067.001 
//                                                                                        // 01068.001 
// MessageId: MF_E_MP3_BAD_CRC                                                            // 01069.001 
//                                                                                        // 01070.001 
// MessageText:                                                                           // 01071.001 
//                                                                                        // 01072.001 
// The MPEG frame has bad CRC.%0                                                          // 01073.001 
//                                                                                        // 01074.001 
  MF_E_MP3_BAD_CRC                      = _HRESULT_TYPEDEF_($C00D3E99);                   // 01075.001#define MF_E_MP3_BAD_CRC                 _HRESULT_TYPEDEF_(0xC00D3E99L)
                                                                                          // 01076.001 
//                                                                                        // 01077.001 
// MessageId: MF_E_NOT_PROTECTED                                                          // 01078.001 
//                                                                                        // 01079.001 
// MessageText:                                                                           // 01080.001 
//                                                                                        // 01081.001 
// The file is not protected.%0                                                           // 01082.001 
//                                                                                        // 01083.001 
  MF_E_NOT_PROTECTED                    = _HRESULT_TYPEDEF_($C00D3E9A);                   // 01084.001#define MF_E_NOT_PROTECTED               _HRESULT_TYPEDEF_(0xC00D3E9AL)
                                                                                          // 01085.001 
//                                                                                        // 01086.001 
// MessageId: MF_E_MEDIA_SOURCE_WRONGSTATE                                                // 01087.001 
//                                                                                        // 01088.001 
// MessageText:                                                                           // 01089.001 
//                                                                                        // 01090.001 
// The media source is in the wrong state%0                                               // 01091.001 
//                                                                                        // 01092.001 
  MF_E_MEDIA_SOURCE_WRONGSTATE          = _HRESULT_TYPEDEF_($C00D3E9B);                   // 01093.001#define MF_E_MEDIA_SOURCE_WRONGSTATE     _HRESULT_TYPEDEF_(0xC00D3E9BL)
                                                                                          // 01094.001 
//                                                                                        // 01095.001 
// MessageId: MF_E_MEDIA_SOURCE_NO_STREAMS_SELECTED                                       // 01096.001 
//                                                                                        // 01097.001 
// MessageText:                                                                           // 01098.001 
//                                                                                        // 01099.001 
// No streams are selected in source presentation descriptor.%0                           // 01100.001 
//                                                                                        // 01101.001 
  MF_E_MEDIA_SOURCE_NO_STREAMS_SELECTED = _HRESULT_TYPEDEF_($C00D3E9C);                   // 01102.001#define MF_E_MEDIA_SOURCE_NO_STREAMS_SELECTED _HRESULT_TYPEDEF_(0xC00D3E9CL)
                                                                                          // 01103.001 
//                                                                                        // 01104.001 
// MessageId: MF_E_CANNOT_FIND_KEYFRAME_SAMPLE                                            // 01105.001 
//                                                                                        // 01106.001 
// MessageText:                                                                           // 01107.001 
//                                                                                        // 01108.001 
// No key frame sample was found.%0                                                       // 01109.001 
//                                                                                        // 01110.001 
  MF_E_CANNOT_FIND_KEYFRAME_SAMPLE      = _HRESULT_TYPEDEF_($C00D3E9D);                   // 01111.001#define MF_E_CANNOT_FIND_KEYFRAME_SAMPLE _HRESULT_TYPEDEF_(0xC00D3E9DL)
                                                                                          // 01112.001 
                                                                                          // 01113.001 
/////////////////////////////////////////////////////////////////////////                 // 01114.001 
//                                                                                        // 01115.001 
// MEDIAFOUNDATION Network Error Events                                                   // 01116.001 
//                                                                                        // 01117.001 
/////////////////////////////////////////////////////////////////////////                 // 01118.001 
                                                                                          // 01119.001 
//                                                                                        // 01120.001 
// MessageId: MF_E_NETWORK_RESOURCE_FAILURE                                               // 01121.001 
//                                                                                        // 01122.001 
// MessageText:                                                                           // 01123.001 
//                                                                                        // 01124.001 
// An attempt to acquire a network resource failed.%0                                     // 01125.001 
//                                                                                        // 01126.001 
  MF_E_NETWORK_RESOURCE_FAILURE         = _HRESULT_TYPEDEF_($C00D4268);                   // 01127.001#define MF_E_NETWORK_RESOURCE_FAILURE    _HRESULT_TYPEDEF_(0xC00D4268L)
                                                                                          // 01128.001 
//                                                                                        // 01129.001 
// MessageId: MF_E_NET_WRITE                                                              // 01130.001 
//                                                                                        // 01131.001 
// MessageText:                                                                           // 01132.001 
//                                                                                        // 01133.001 
// Error writing to the network.%0                                                        // 01134.001 
//                                                                                        // 01135.001 
  MF_E_NET_WRITE                        = _HRESULT_TYPEDEF_($C00D4269);                   // 01136.001#define MF_E_NET_WRITE                   _HRESULT_TYPEDEF_(0xC00D4269L)
                                                                                          // 01137.001 
//                                                                                        // 01138.001 
// MessageId: MF_E_NET_READ                                                               // 01139.001 
//                                                                                        // 01140.001 
// MessageText:                                                                           // 01141.001 
//                                                                                        // 01142.001 
// Error reading from the network.%0                                                      // 01143.001 
//                                                                                        // 01144.001 
  MF_E_NET_READ                         = _HRESULT_TYPEDEF_($C00D426A);                   // 01145.001#define MF_E_NET_READ                    _HRESULT_TYPEDEF_(0xC00D426AL)
                                                                                          // 01146.001 
//                                                                                        // 01147.001 
// MessageId: MF_E_NET_REQUIRE_NETWORK                                                    // 01148.001 
//                                                                                        // 01149.001 
// MessageText:                                                                           // 01150.001 
//                                                                                        // 01151.001 
// Internal. Entry cannot complete operation without network.%0                           // 01152.001 
//                                                                                        // 01153.001 
  MF_E_NET_REQUIRE_NETWORK              = _HRESULT_TYPEDEF_($C00D426B);                   // 01154.001#define MF_E_NET_REQUIRE_NETWORK         _HRESULT_TYPEDEF_(0xC00D426BL)
                                                                                          // 01155.001 
//                                                                                        // 01156.001 
// MessageId: MF_E_NET_REQUIRE_ASYNC                                                      // 01157.001 
//                                                                                        // 01158.001 
// MessageText:                                                                           // 01159.001 
//                                                                                        // 01160.001 
// Internal. Async op is required.%0                                                      // 01161.001 
//                                                                                        // 01162.001 
  MF_E_NET_REQUIRE_ASYNC                = _HRESULT_TYPEDEF_($C00D426C);                   // 01163.001#define MF_E_NET_REQUIRE_ASYNC           _HRESULT_TYPEDEF_(0xC00D426CL)
                                                                                          // 01164.001 
//                                                                                        // 01165.001 
// MessageId: MF_E_NET_BWLEVEL_NOT_SUPPORTED                                              // 01166.001 
//                                                                                        // 01167.001 
// MessageText:                                                                           // 01168.001 
//                                                                                        // 01169.001 
// Internal. Bandwidth levels are not supported.%0                                        // 01170.001 
//                                                                                        // 01171.001 
  MF_E_NET_BWLEVEL_NOT_SUPPORTED        = _HRESULT_TYPEDEF_($C00D426D);                   // 01172.001#define MF_E_NET_BWLEVEL_NOT_SUPPORTED   _HRESULT_TYPEDEF_(0xC00D426DL)
                                                                                          // 01173.001 
//                                                                                        // 01174.001 
// MessageId: MF_E_NET_STREAMGROUPS_NOT_SUPPORTED                                         // 01175.001 
//                                                                                        // 01176.001 
// MessageText:                                                                           // 01177.001 
//                                                                                        // 01178.001 
// Internal. Stream groups are not supported.%0                                           // 01179.001 
//                                                                                        // 01180.001 
  MF_E_NET_STREAMGROUPS_NOT_SUPPORTED   = _HRESULT_TYPEDEF_($C00D426E);                   // 01181.001#define MF_E_NET_STREAMGROUPS_NOT_SUPPORTED _HRESULT_TYPEDEF_(0xC00D426EL)
                                                                                          // 01182.001 
//                                                                                        // 01183.001 
// MessageId: MF_E_NET_MANUALSS_NOT_SUPPORTED                                             // 01184.001 
//                                                                                        // 01185.001 
// MessageText:                                                                           // 01186.001 
//                                                                                        // 01187.001 
// Manual stream selection is not supported.%0                                            // 01188.001 
//                                                                                        // 01189.001 
  MF_E_NET_MANUALSS_NOT_SUPPORTED       = _HRESULT_TYPEDEF_($C00D426F);                   // 01190.001#define MF_E_NET_MANUALSS_NOT_SUPPORTED  _HRESULT_TYPEDEF_(0xC00D426FL)
                                                                                          // 01191.001 
//                                                                                        // 01192.001 
// MessageId: MF_E_NET_INVALID_PRESENTATION_DESCRIPTOR                                    // 01193.001 
//                                                                                        // 01194.001 
// MessageText:                                                                           // 01195.001 
//                                                                                        // 01196.001 
// Invalid presentation descriptor.%0                                                     // 01197.001 
//                                                                                        // 01198.001 
  MF_E_NET_INVALID_PRESENTATION_DESCRIPTOR = _HRESULT_TYPEDEF_($C00D4270);                // 01199.001#define MF_E_NET_INVALID_PRESENTATION_DESCRIPTOR _HRESULT_TYPEDEF_(0xC00D4270L)
                                                                                          // 01200.001 
//                                                                                        // 01201.001 
// MessageId: MF_E_NET_CACHESTREAM_NOT_FOUND                                              // 01202.001 
//                                                                                        // 01203.001 
// MessageText:                                                                           // 01204.001 
//                                                                                        // 01205.001 
// Cannot find cache stream.%0                                                            // 01206.001 
//                                                                                        // 01207.001 
  MF_E_NET_CACHESTREAM_NOT_FOUND        = _HRESULT_TYPEDEF_($C00D4271);                   // 01208.001#define MF_E_NET_CACHESTREAM_NOT_FOUND   _HRESULT_TYPEDEF_(0xC00D4271L)
                                                                                          // 01209.001 
//                                                                                        // 01210.001 
// MessageId: MF_I_MANUAL_PROXY                                                           // 01211.001 
//                                                                                        // 01212.001 
// MessageText:                                                                           // 01213.001 
//                                                                                        // 01214.001 
// The proxy setting is manual.%0                                                         // 01215.001 
//                                                                                        // 01216.001 
  MF_I_MANUAL_PROXY                     = _HRESULT_TYPEDEF_($400D4272);                   // 01217.001#define MF_I_MANUAL_PROXY                _HRESULT_TYPEDEF_(0x400D4272L)
                                                                                          // 01218.001 
//duplicate removed                                                                       // 01219.001 
//MessageId=17011 Severity=Informational Facility=MEDIAFOUNDATION SymbolicName=MF_E_INVALID_REQUEST // 01220.001 
//Language=English                                                                        // 01221.001 
//The request is invalid in the current state.%0                                          // 01222.001 
//.                                                                                       // 01223.001 
//                                                                                        // 01224.001 
// MessageId: MF_E_NET_REQUIRE_INPUT                                                      // 01225.001 
//                                                                                        // 01226.001 
// MessageText:                                                                           // 01227.001 
//                                                                                        // 01228.001 
// Internal. Entry cannot complete operation without input.%0                             // 01229.001 
//                                                                                        // 01230.001 
  MF_E_NET_REQUIRE_INPUT                = _HRESULT_TYPEDEF_($C00D4274);                   // 01231.001#define MF_E_NET_REQUIRE_INPUT           _HRESULT_TYPEDEF_(0xC00D4274L)
                                                                                          // 01232.001 
//                                                                                        // 01233.001 
// MessageId: MF_E_NET_REDIRECT                                                           // 01234.001 
//                                                                                        // 01235.001 
// MessageText:                                                                           // 01236.001 
//                                                                                        // 01237.001 
// The client redirected to another server.%0                                             // 01238.001 
//                                                                                        // 01239.001 
  MF_E_NET_REDIRECT                     = _HRESULT_TYPEDEF_($C00D4275);                   // 01240.001#define MF_E_NET_REDIRECT                _HRESULT_TYPEDEF_(0xC00D4275L)
                                                                                          // 01241.001 
//                                                                                        // 01242.001 
// MessageId: MF_E_NET_REDIRECT_TO_PROXY                                                  // 01243.001 
//                                                                                        // 01244.001 
// MessageText:                                                                           // 01245.001 
//                                                                                        // 01246.001 
// The client is redirected to a proxy server.%0                                          // 01247.001 
//                                                                                        // 01248.001 
  MF_E_NET_REDIRECT_TO_PROXY            = _HRESULT_TYPEDEF_($C00D4276);                   // 01249.001#define MF_E_NET_REDIRECT_TO_PROXY       _HRESULT_TYPEDEF_(0xC00D4276L)
                                                                                          // 01250.001 
//                                                                                        // 01251.001 
// MessageId: MF_E_NET_TOO_MANY_REDIRECTS                                                 // 01252.001 
//                                                                                        // 01253.001 
// MessageText:                                                                           // 01254.001 
//                                                                                        // 01255.001 
// The client reached maximum redirection limit.%0                                        // 01256.001 
//                                                                                        // 01257.001 
  MF_E_NET_TOO_MANY_REDIRECTS           = _HRESULT_TYPEDEF_($C00D4277);                   // 01258.001#define MF_E_NET_TOO_MANY_REDIRECTS      _HRESULT_TYPEDEF_(0xC00D4277L)
                                                                                          // 01259.001 
//                                                                                        // 01260.001 
// MessageId: MF_E_NET_TIMEOUT                                                            // 01261.001 
//                                                                                        // 01262.001 
// MessageText:                                                                           // 01263.001 
//                                                                                        // 01264.001 
// The server, a computer set up to offer multimedia content to other computers, could not handle your request for multimedia content in a timely manner.  Please try again later.%0 // 01265.001 
//                                                                                        // 01266.001 
  MF_E_NET_TIMEOUT                      = _HRESULT_TYPEDEF_($C00D4278);                   // 01267.001#define MF_E_NET_TIMEOUT                 _HRESULT_TYPEDEF_(0xC00D4278L)
                                                                                          // 01268.001 
//                                                                                        // 01269.001 
// MessageId: MF_E_NET_CLIENT_CLOSE                                                       // 01270.001 
//                                                                                        // 01271.001 
// MessageText:                                                                           // 01272.001 
//                                                                                        // 01273.001 
// The control socket is closed by the client.%0                                          // 01274.001 
//                                                                                        // 01275.001 
  MF_E_NET_CLIENT_CLOSE                 = _HRESULT_TYPEDEF_($C00D4279);                   // 01276.001#define MF_E_NET_CLIENT_CLOSE            _HRESULT_TYPEDEF_(0xC00D4279L)
                                                                                          // 01277.001 
//                                                                                        // 01278.001 
// MessageId: MF_E_NET_BAD_CONTROL_DATA                                                   // 01279.001 
//                                                                                        // 01280.001 
// MessageText:                                                                           // 01281.001 
//                                                                                        // 01282.001 
// The server received invalid data from the client on the control connection.%0          // 01283.001 
//                                                                                        // 01284.001 
  MF_E_NET_BAD_CONTROL_DATA             = _HRESULT_TYPEDEF_($C00D427A);                   // 01285.001#define MF_E_NET_BAD_CONTROL_DATA        _HRESULT_TYPEDEF_(0xC00D427AL)
                                                                                          // 01286.001 
//                                                                                        // 01287.001 
// MessageId: MF_E_NET_INCOMPATIBLE_SERVER                                                // 01288.001 
//                                                                                        // 01289.001 
// MessageText:                                                                           // 01290.001 
//                                                                                        // 01291.001 
// The server is not a compatible streaming media server.%0                               // 01292.001 
//                                                                                        // 01293.001 
  MF_E_NET_INCOMPATIBLE_SERVER          = _HRESULT_TYPEDEF_($C00D427B);                   // 01294.001#define MF_E_NET_INCOMPATIBLE_SERVER     _HRESULT_TYPEDEF_(0xC00D427BL)
                                                                                          // 01295.001 
//                                                                                        // 01296.001 
// MessageId: MF_E_NET_UNSAFE_URL                                                         // 01297.001 
//                                                                                        // 01298.001 
// MessageText:                                                                           // 01299.001 
//                                                                                        // 01300.001 
// Url.%0                                                                                 // 01301.001 
//                                                                                        // 01302.001 
  MF_E_NET_UNSAFE_URL                   = _HRESULT_TYPEDEF_($C00D427C);                   // 01303.001#define MF_E_NET_UNSAFE_URL              _HRESULT_TYPEDEF_(0xC00D427CL)
                                                                                          // 01304.001 
//                                                                                        // 01305.001 
// MessageId: MF_E_NET_CACHE_NO_DATA                                                      // 01306.001 
//                                                                                        // 01307.001 
// MessageText:                                                                           // 01308.001 
//                                                                                        // 01309.001 
// Data is not available.%0                                                               // 01310.001 
//                                                                                        // 01311.001 
  MF_E_NET_CACHE_NO_DATA                = _HRESULT_TYPEDEF_($C00D427D);                   // 01312.001#define MF_E_NET_CACHE_NO_DATA           _HRESULT_TYPEDEF_(0xC00D427DL)
                                                                                          // 01313.001 
//                                                                                        // 01314.001 
// MessageId: MF_E_NET_EOL                                                                // 01315.001 
//                                                                                        // 01316.001 
// MessageText:                                                                           // 01317.001 
//                                                                                        // 01318.001 
// End of line.%0                                                                         // 01319.001 
//                                                                                        // 01320.001 
  MF_E_NET_EOL                          = _HRESULT_TYPEDEF_($C00D427E);                   // 01321.001#define MF_E_NET_EOL                     _HRESULT_TYPEDEF_(0xC00D427EL)
                                                                                          // 01322.001 
//                                                                                        // 01323.001 
// MessageId: MF_E_NET_BAD_REQUEST                                                        // 01324.001 
//                                                                                        // 01325.001 
// MessageText:                                                                           // 01326.001 
//                                                                                        // 01327.001 
// The request could not be understood by the server.%0                                   // 01328.001 
//                                                                                        // 01329.001 
  MF_E_NET_BAD_REQUEST                  = _HRESULT_TYPEDEF_($C00D427F);                   // 01330.001#define MF_E_NET_BAD_REQUEST             _HRESULT_TYPEDEF_(0xC00D427FL)
                                                                                          // 01331.001 
//                                                                                        // 01332.001 
// MessageId: MF_E_NET_INTERNAL_SERVER_ERROR                                              // 01333.001 
//                                                                                        // 01334.001 
// MessageText:                                                                           // 01335.001 
//                                                                                        // 01336.001 
// The server encountered an unexpected condition which prevented it from fulfilling the request.%0 // 01337.001 
//                                                                                        // 01338.001 
  MF_E_NET_INTERNAL_SERVER_ERROR        = _HRESULT_TYPEDEF_($C00D4280);                   // 01339.001#define MF_E_NET_INTERNAL_SERVER_ERROR   _HRESULT_TYPEDEF_(0xC00D4280L)
                                                                                          // 01340.001 
//                                                                                        // 01341.001 
// MessageId: MF_E_NET_SESSION_NOT_FOUND                                                  // 01342.001 
//                                                                                        // 01343.001 
// MessageText:                                                                           // 01344.001 
//                                                                                        // 01345.001 
// Session not found.%0                                                                   // 01346.001 
//                                                                                        // 01347.001 
  MF_E_NET_SESSION_NOT_FOUND            = _HRESULT_TYPEDEF_($C00D4281);                   // 01348.001#define MF_E_NET_SESSION_NOT_FOUND       _HRESULT_TYPEDEF_(0xC00D4281L)
                                                                                          // 01349.001 
//                                                                                        // 01350.001 
// MessageId: MF_E_NET_NOCONNECTION                                                       // 01351.001 
//                                                                                        // 01352.001 
// MessageText:                                                                           // 01353.001 
//                                                                                        // 01354.001 
// There is no connection established with the Windows Media server. The operation failed.%0 // 01355.001 
//                                                                                        // 01356.001 
  MF_E_NET_NOCONNECTION                 = _HRESULT_TYPEDEF_($C00D4282);                   // 01357.001#define MF_E_NET_NOCONNECTION            _HRESULT_TYPEDEF_(0xC00D4282L)
                                                                                          // 01358.001 
//                                                                                        // 01359.001 
// MessageId: MF_E_NET_CONNECTION_FAILURE                                                 // 01360.001 
//                                                                                        // 01361.001 
// MessageText:                                                                           // 01362.001 
//                                                                                        // 01363.001 
// The network connection has failed.%0                                                   // 01364.001 
//                                                                                        // 01365.001 
  MF_E_NET_CONNECTION_FAILURE           = _HRESULT_TYPEDEF_($C00D4283);                   // 01366.001#define MF_E_NET_CONNECTION_FAILURE      _HRESULT_TYPEDEF_(0xC00D4283L)
                                                                                          // 01367.001 
//                                                                                        // 01368.001 
// MessageId: MF_E_NET_INCOMPATIBLE_PUSHSERVER                                            // 01369.001 
//                                                                                        // 01370.001 
// MessageText:                                                                           // 01371.001 
//                                                                                        // 01372.001 
// The Server service that received the HTTP push request is not a compatible version of Windows Media Services (WMS).  This error may indicate the push request was received by IIS instead of WMS.  Ensure WMS is started and has the HTTP Server control protocol properly enabled and try again.%0 // 01373.001 
//                                                                                        // 01374.001 
  MF_E_NET_INCOMPATIBLE_PUSHSERVER      = _HRESULT_TYPEDEF_($C00D4284);                   // 01375.001#define MF_E_NET_INCOMPATIBLE_PUSHSERVER _HRESULT_TYPEDEF_(0xC00D4284L)
                                                                                          // 01376.001 
//                                                                                        // 01377.001 
// MessageId: MF_E_NET_SERVER_ACCESSDENIED                                                // 01378.001 
//                                                                                        // 01379.001 
// MessageText:                                                                           // 01380.001 
//                                                                                        // 01381.001 
// The Windows Media server is denying access.  The username and/or password might be incorrect.%0 // 01382.001 
//                                                                                        // 01383.001 
  MF_E_NET_SERVER_ACCESSDENIED          = _HRESULT_TYPEDEF_($C00D4285);                   // 01384.001#define MF_E_NET_SERVER_ACCESSDENIED     _HRESULT_TYPEDEF_(0xC00D4285L)
                                                                                          // 01385.001 
//                                                                                        // 01386.001 
// MessageId: MF_E_NET_PROXY_ACCESSDENIED                                                 // 01387.001 
//                                                                                        // 01388.001 
// MessageText:                                                                           // 01389.001 
//                                                                                        // 01390.001 
// The proxy server is denying access.  The username and/or password might be incorrect.%0 // 01391.001 
//                                                                                        // 01392.001 
  MF_E_NET_PROXY_ACCESSDENIED           = _HRESULT_TYPEDEF_($C00D4286);                   // 01393.001#define MF_E_NET_PROXY_ACCESSDENIED      _HRESULT_TYPEDEF_(0xC00D4286L)
                                                                                          // 01394.001 
//                                                                                        // 01395.001 
// MessageId: MF_E_NET_CANNOTCONNECT                                                      // 01396.001 
//                                                                                        // 01397.001 
// MessageText:                                                                           // 01398.001 
//                                                                                        // 01399.001 
// Unable to establish a connection to the server.%0                                      // 01400.001 
//                                                                                        // 01401.001 
  MF_E_NET_CANNOTCONNECT                = _HRESULT_TYPEDEF_($C00D4287);                   // 01402.001#define MF_E_NET_CANNOTCONNECT           _HRESULT_TYPEDEF_(0xC00D4287L)
                                                                                          // 01403.001 
//                                                                                        // 01404.001 
// MessageId: MF_E_NET_INVALID_PUSH_TEMPLATE                                              // 01405.001 
//                                                                                        // 01406.001 
// MessageText:                                                                           // 01407.001 
//                                                                                        // 01408.001 
// The specified push template is invalid.%0                                              // 01409.001 
//                                                                                        // 01410.001 
  MF_E_NET_INVALID_PUSH_TEMPLATE        = _HRESULT_TYPEDEF_($C00D4288);                   // 01411.001#define MF_E_NET_INVALID_PUSH_TEMPLATE   _HRESULT_TYPEDEF_(0xC00D4288L)
                                                                                          // 01412.001 
//                                                                                        // 01413.001 
// MessageId: MF_E_NET_INVALID_PUSH_PUBLISHING_POINT                                      // 01414.001 
//                                                                                        // 01415.001 
// MessageText:                                                                           // 01416.001 
//                                                                                        // 01417.001 
// The specified push publishing point is invalid.%0                                      // 01418.001 
//                                                                                        // 01419.001 
  MF_E_NET_INVALID_PUSH_PUBLISHING_POINT = _HRESULT_TYPEDEF_($C00D4289);                  // 01420.001#define MF_E_NET_INVALID_PUSH_PUBLISHING_POINT _HRESULT_TYPEDEF_(0xC00D4289L)
                                                                                          // 01421.001 
//                                                                                        // 01422.001 
// MessageId: MF_E_NET_BUSY                                                               // 01423.001 
//                                                                                        // 01424.001 
// MessageText:                                                                           // 01425.001 
//                                                                                        // 01426.001 
// The requested resource is in use.%0                                                    // 01427.001 
//                                                                                        // 01428.001 
  MF_E_NET_BUSY                         = _HRESULT_TYPEDEF_($C00D428A);                   // 01429.001#define MF_E_NET_BUSY                    _HRESULT_TYPEDEF_(0xC00D428AL)
                                                                                          // 01430.001 
//                                                                                        // 01431.001 
// MessageId: MF_E_NET_RESOURCE_GONE                                                      // 01432.001 
//                                                                                        // 01433.001 
// MessageText:                                                                           // 01434.001 
//                                                                                        // 01435.001 
// The Publishing Point or file on the Windows Media Server is no longer available.%0     // 01436.001 
//                                                                                        // 01437.001 
  MF_E_NET_RESOURCE_GONE                = _HRESULT_TYPEDEF_($C00D428B);                   // 01438.001#define MF_E_NET_RESOURCE_GONE           _HRESULT_TYPEDEF_(0xC00D428BL)
                                                                                          // 01439.001 
//                                                                                        // 01440.001 
// MessageId: MF_E_NET_ERROR_FROM_PROXY                                                   // 01441.001 
//                                                                                        // 01442.001 
// MessageText:                                                                           // 01443.001 
//                                                                                        // 01444.001 
// The proxy experienced an error while attempting to contact the media server.%0         // 01445.001 
//                                                                                        // 01446.001 
  MF_E_NET_ERROR_FROM_PROXY             = _HRESULT_TYPEDEF_($C00D428C);                   // 01447.001#define MF_E_NET_ERROR_FROM_PROXY        _HRESULT_TYPEDEF_(0xC00D428CL)
                                                                                          // 01448.001 
//                                                                                        // 01449.001 
// MessageId: MF_E_NET_PROXY_TIMEOUT                                                      // 01450.001 
//                                                                                        // 01451.001 
// MessageText:                                                                           // 01452.001 
//                                                                                        // 01453.001 
// The proxy did not receive a timely response while attempting to contact the media server.%0 // 01454.001 
//                                                                                        // 01455.001 
  MF_E_NET_PROXY_TIMEOUT                = _HRESULT_TYPEDEF_($C00D428D);                   // 01456.001#define MF_E_NET_PROXY_TIMEOUT           _HRESULT_TYPEDEF_(0xC00D428DL)
                                                                                          // 01457.001 
//                                                                                        // 01458.001 
// MessageId: MF_E_NET_SERVER_UNAVAILABLE                                                 // 01459.001 
//                                                                                        // 01460.001 
// MessageText:                                                                           // 01461.001 
//                                                                                        // 01462.001 
// The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.%0 // 01463.001 
//                                                                                        // 01464.001 
  MF_E_NET_SERVER_UNAVAILABLE           = _HRESULT_TYPEDEF_($C00D428E);                   // 01465.001#define MF_E_NET_SERVER_UNAVAILABLE      _HRESULT_TYPEDEF_(0xC00D428EL)
                                                                                          // 01466.001 
//                                                                                        // 01467.001 
// MessageId: MF_E_NET_TOO_MUCH_DATA                                                      // 01468.001 
//                                                                                        // 01469.001 
// MessageText:                                                                           // 01470.001 
//                                                                                        // 01471.001 
// The encoding process was unable to keep up with the amount of supplied data.%0         // 01472.001 
//                                                                                        // 01473.001 
  MF_E_NET_TOO_MUCH_DATA                = _HRESULT_TYPEDEF_($C00D428F);                   // 01474.001#define MF_E_NET_TOO_MUCH_DATA           _HRESULT_TYPEDEF_(0xC00D428FL)
                                                                                          // 01475.001 
//                                                                                        // 01476.001 
// MessageId: MF_E_NET_SESSION_INVALID                                                    // 01477.001 
//                                                                                        // 01478.001 
// MessageText:                                                                           // 01479.001 
//                                                                                        // 01480.001 
// Session not found.%0                                                                   // 01481.001 
//                                                                                        // 01482.001 
  MF_E_NET_SESSION_INVALID              = _HRESULT_TYPEDEF_($C00D4290);                   // 01483.001#define MF_E_NET_SESSION_INVALID         _HRESULT_TYPEDEF_(0xC00D4290L)
                                                                                          // 01484.001 
//                                                                                        // 01485.001 
// MessageId: MF_E_OFFLINE_MODE                                                           // 01486.001 
//                                                                                        // 01487.001 
// MessageText:                                                                           // 01488.001 
//                                                                                        // 01489.001 
// The requested URL is not available in offline mode.%0                                  // 01490.001 
//                                                                                        // 01491.001 
  MF_E_OFFLINE_MODE                     = _HRESULT_TYPEDEF_($C00D4291);                   // 01492.001#define MF_E_OFFLINE_MODE                _HRESULT_TYPEDEF_(0xC00D4291L)
                                                                                          // 01493.001 
//                                                                                        // 01494.001 
// MessageId: MF_E_NET_UDP_BLOCKED                                                        // 01495.001 
//                                                                                        // 01496.001 
// MessageText:                                                                           // 01497.001 
//                                                                                        // 01498.001 
// A device in the network is blocking UDP traffic.%0                                     // 01499.001 
//                                                                                        // 01500.001 
  MF_E_NET_UDP_BLOCKED                  = _HRESULT_TYPEDEF_($C00D4292);                   // 01501.001#define MF_E_NET_UDP_BLOCKED             _HRESULT_TYPEDEF_(0xC00D4292L)
                                                                                          // 01502.001 
//                                                                                        // 01503.001 
// MessageId: MF_E_NET_UNSUPPORTED_CONFIGURATION                                          // 01504.001 
//                                                                                        // 01505.001 
// MessageText:                                                                           // 01506.001 
//                                                                                        // 01507.001 
// The specified configuration value is not supported.%0                                  // 01508.001 
//                                                                                        // 01509.001 
  MF_E_NET_UNSUPPORTED_CONFIGURATION    = _HRESULT_TYPEDEF_($C00D4293);                   // 01510.001#define MF_E_NET_UNSUPPORTED_CONFIGURATION _HRESULT_TYPEDEF_(0xC00D4293L)
                                                                                          // 01511.001 
//                                                                                        // 01512.001 
// MessageId: MF_E_NET_PROTOCOL_DISABLED                                                  // 01513.001 
//                                                                                        // 01514.001 
// MessageText:                                                                           // 01515.001 
//                                                                                        // 01516.001 
// The networking protocol is disabled.%0                                                 // 01517.001 
//                                                                                        // 01518.001 
  MF_E_NET_PROTOCOL_DISABLED            = _HRESULT_TYPEDEF_($C00D4294);                   // 01519.001#define MF_E_NET_PROTOCOL_DISABLED       _HRESULT_TYPEDEF_(0xC00D4294L)
                                                                                          // 01520.001 
                                                                                          // 01521.001 
/////////////////////////////////////////////////////////////////////////                 // 01522.001 
//                                                                                        // 01523.001 
// MEDIAFOUNDATION WMContainer Error Events                                               // 01524.001 
//                                                                                        // 01525.001 
/////////////////////////////////////////////////////////////////////////                 // 01526.001 
                                                                                          // 01527.001 
//                                                                                        // 01528.001 
// MessageId: MF_E_ALREADY_INITIALIZED                                                    // 01529.001 
//                                                                                        // 01530.001 
// MessageText:                                                                           // 01531.001 
//                                                                                        // 01532.001 
// This object has already been initialized and cannot be re-initialized at this time.%0  // 01533.001 
//                                                                                        // 01534.001 
  MF_E_ALREADY_INITIALIZED              = _HRESULT_TYPEDEF_($C00D4650);                   // 01535.001#define MF_E_ALREADY_INITIALIZED         _HRESULT_TYPEDEF_(0xC00D4650L)
                                                                                          // 01536.001 
//                                                                                        // 01537.001 
// MessageId: MF_E_BANDWIDTH_OVERRUN                                                      // 01538.001 
//                                                                                        // 01539.001 
// MessageText:                                                                           // 01540.001 
//                                                                                        // 01541.001 
// The amount of data passed in exceeds the given bitrate and buffer window.%0            // 01542.001 
//                                                                                        // 01543.001 
  MF_E_BANDWIDTH_OVERRUN                = _HRESULT_TYPEDEF_($C00D4651);                   // 01544.001#define MF_E_BANDWIDTH_OVERRUN           _HRESULT_TYPEDEF_(0xC00D4651L)
                                                                                          // 01545.001 
//                                                                                        // 01546.001 
// MessageId: MF_E_LATE_SAMPLE                                                            // 01547.001 
//                                                                                        // 01548.001 
// MessageText:                                                                           // 01549.001 
//                                                                                        // 01550.001 
// The sample was passed in too late to be correctly processed.%0                         // 01551.001 
//                                                                                        // 01552.001 
  MF_E_LATE_SAMPLE                      = _HRESULT_TYPEDEF_($C00D4652);                   // 01553.001#define MF_E_LATE_SAMPLE                 _HRESULT_TYPEDEF_(0xC00D4652L)
                                                                                          // 01554.001 
//                                                                                        // 01555.001 
// MessageId: MF_E_FLUSH_NEEDED                                                           // 01556.001 
//                                                                                        // 01557.001 
// MessageText:                                                                           // 01558.001 
//                                                                                        // 01559.001 
// The requested action cannot be carried out until the object is flushed and the queue is emptied.%0 // 01560.001 
//                                                                                        // 01561.001 
  MF_E_FLUSH_NEEDED                     = _HRESULT_TYPEDEF_($C00D4653);                   // 01562.001#define MF_E_FLUSH_NEEDED                _HRESULT_TYPEDEF_(0xC00D4653L)
                                                                                          // 01563.001 
//                                                                                        // 01564.001 
// MessageId: MF_E_INVALID_PROFILE                                                        // 01565.001 
//                                                                                        // 01566.001 
// MessageText:                                                                           // 01567.001 
//                                                                                        // 01568.001 
// The profile is invalid.%0                                                              // 01569.001 
//                                                                                        // 01570.001 
  MF_E_INVALID_PROFILE                  = _HRESULT_TYPEDEF_($C00D4654);                   // 01571.001#define MF_E_INVALID_PROFILE             _HRESULT_TYPEDEF_(0xC00D4654L)
                                                                                          // 01572.001 
//                                                                                        // 01573.001 
// MessageId: MF_E_INDEX_NOT_COMMITTED                                                    // 01574.001 
//                                                                                        // 01575.001 
// MessageText:                                                                           // 01576.001 
//                                                                                        // 01577.001 
// The index that is being generated needs to be committed before the requested action can be carried out.%0 // 01578.001 
//                                                                                        // 01579.001 
  MF_E_INDEX_NOT_COMMITTED              = _HRESULT_TYPEDEF_($C00D4655);                   // 01580.001#define MF_E_INDEX_NOT_COMMITTED         _HRESULT_TYPEDEF_(0xC00D4655L)
                                                                                          // 01581.001 
//                                                                                        // 01582.001 
// MessageId: MF_E_NO_INDEX                                                               // 01583.001 
//                                                                                        // 01584.001 
// MessageText:                                                                           // 01585.001 
//                                                                                        // 01586.001 
// The index that is necessary for the requested action is not found.%0                   // 01587.001 
//                                                                                        // 01588.001 
  MF_E_NO_INDEX                         = _HRESULT_TYPEDEF_($C00D4656);                   // 01589.001#define MF_E_NO_INDEX                    _HRESULT_TYPEDEF_(0xC00D4656L)
                                                                                          // 01590.001 
//                                                                                        // 01591.001 
// MessageId: MF_E_CANNOT_INDEX_IN_PLACE                                                  // 01592.001 
//                                                                                        // 01593.001 
// MessageText:                                                                           // 01594.001 
//                                                                                        // 01595.001 
// The requested index cannot be added in-place to the specified ASF content.%0           // 01596.001 
//                                                                                        // 01597.001 
  MF_E_CANNOT_INDEX_IN_PLACE            = _HRESULT_TYPEDEF_($C00D4657);                   // 01598.001#define MF_E_CANNOT_INDEX_IN_PLACE       _HRESULT_TYPEDEF_(0xC00D4657L)
                                                                                          // 01599.001 
//                                                                                        // 01600.001 
// MessageId: MF_E_MISSING_ASF_LEAKYBUCKET                                                // 01601.001 
//                                                                                        // 01602.001 
// MessageText:                                                                           // 01603.001 
//                                                                                        // 01604.001 
// The ASF leaky bucket parameters must be specified in order to carry out this request.%0 // 01605.001 
//                                                                                        // 01606.001 
  MF_E_MISSING_ASF_LEAKYBUCKET          = _HRESULT_TYPEDEF_($C00D4658);                   // 01607.001#define MF_E_MISSING_ASF_LEAKYBUCKET     _HRESULT_TYPEDEF_(0xC00D4658L)
                                                                                          // 01608.001 
//                                                                                        // 01609.001 
// MessageId: MF_E_INVALID_ASF_STREAMID                                                   // 01610.001 
//                                                                                        // 01611.001 
// MessageText:                                                                           // 01612.001 
//                                                                                        // 01613.001 
// The stream id is invalid. The valid range for ASF stream id is from 1 to 127.%0        // 01614.001 
//                                                                                        // 01615.001 
  MF_E_INVALID_ASF_STREAMID             = _HRESULT_TYPEDEF_($C00D4659);                   // 01616.001#define MF_E_INVALID_ASF_STREAMID        _HRESULT_TYPEDEF_(0xC00D4659L)
                                                                                          // 01617.001 
                                                                                          // 01618.001 
/////////////////////////////////////////////////////////////////////////                 // 01619.001 
//                                                                                        // 01620.001 
// MEDIAFOUNDATION Media Sink Error Events                                                // 01621.001 
//                                                                                        // 01622.001 
/////////////////////////////////////////////////////////////////////////                 // 01623.001 
                                                                                          // 01624.001 
//                                                                                        // 01625.001 
// MessageId: MF_E_STREAMSINK_REMOVED                                                     // 01626.001 
//                                                                                        // 01627.001 
// MessageText:                                                                           // 01628.001 
//                                                                                        // 01629.001 
// The requested Stream Sink has been removed and cannot be used.%0                       // 01630.001 
//                                                                                        // 01631.001 
  MF_E_STREAMSINK_REMOVED               = _HRESULT_TYPEDEF_($C00D4A38);                   // 01632.001#define MF_E_STREAMSINK_REMOVED          _HRESULT_TYPEDEF_(0xC00D4A38L)
                                                                                          // 01633.001 
//                                                                                        // 01634.001 
// MessageId: MF_E_STREAMSINKS_OUT_OF_SYNC                                                // 01635.001 
//                                                                                        // 01636.001 
// MessageText:                                                                           // 01637.001 
//                                                                                        // 01638.001 
// The various Stream Sinks in this Media Sink are too far out of sync for the requested action to take place.%0 // 01639.001 
//                                                                                        // 01640.001 
  MF_E_STREAMSINKS_OUT_OF_SYNC          = _HRESULT_TYPEDEF_($C00D4A3A);                   // 01641.001#define MF_E_STREAMSINKS_OUT_OF_SYNC     _HRESULT_TYPEDEF_(0xC00D4A3AL)
                                                                                          // 01642.001 
//                                                                                        // 01643.001 
// MessageId: MF_E_STREAMSINKS_FIXED                                                      // 01644.001 
//                                                                                        // 01645.001 
// MessageText:                                                                           // 01646.001 
//                                                                                        // 01647.001 
// Stream Sinks cannot be added to or removed from this Media Sink because its set of streams is fixed.%0 // 01648.001 
//                                                                                        // 01649.001 
  MF_E_STREAMSINKS_FIXED                = _HRESULT_TYPEDEF_($C00D4A3B);                   // 01650.001#define MF_E_STREAMSINKS_FIXED           _HRESULT_TYPEDEF_(0xC00D4A3BL)
                                                                                          // 01651.001 
//                                                                                        // 01652.001 
// MessageId: MF_E_STREAMSINK_EXISTS                                                      // 01653.001 
//                                                                                        // 01654.001 
// MessageText:                                                                           // 01655.001 
//                                                                                        // 01656.001 
// The given Stream Sink already exists.%0                                                // 01657.001 
//                                                                                        // 01658.001 
  MF_E_STREAMSINK_EXISTS                = _HRESULT_TYPEDEF_($C00D4A3C);                   // 01659.001#define MF_E_STREAMSINK_EXISTS           _HRESULT_TYPEDEF_(0xC00D4A3CL)
                                                                                          // 01660.001 
//                                                                                        // 01661.001 
// MessageId: MF_E_SAMPLEALLOCATOR_CANCELED                                               // 01662.001 
//                                                                                        // 01663.001 
// MessageText:                                                                           // 01664.001 
//                                                                                        // 01665.001 
// Sample allocations have been canceled.%0                                               // 01666.001 
//                                                                                        // 01667.001 
  MF_E_SAMPLEALLOCATOR_CANCELED         = _HRESULT_TYPEDEF_($C00D4A3D);                   // 01668.001#define MF_E_SAMPLEALLOCATOR_CANCELED    _HRESULT_TYPEDEF_(0xC00D4A3DL)
                                                                                          // 01669.001 
//                                                                                        // 01670.001 
// MessageId: MF_E_SAMPLEALLOCATOR_EMPTY                                                  // 01671.001 
//                                                                                        // 01672.001 
// MessageText:                                                                           // 01673.001 
//                                                                                        // 01674.001 
// The sample allocator is currently empty, due to outstanding requests.%0                // 01675.001 
//                                                                                        // 01676.001 
  MF_E_SAMPLEALLOCATOR_EMPTY            = _HRESULT_TYPEDEF_($C00D4A3E);                   // 01677.001#define MF_E_SAMPLEALLOCATOR_EMPTY       _HRESULT_TYPEDEF_(0xC00D4A3EL)
                                                                                          // 01678.001 
//                                                                                        // 01679.001 
// MessageId: MF_E_SINK_ALREADYSTOPPED                                                    // 01680.001 
//                                                                                        // 01681.001 
// MessageText:                                                                           // 01682.001 
//                                                                                        // 01683.001 
// When we try to sopt a stream sink, it is already stopped %0                            // 01684.001 
//                                                                                        // 01685.001 
  MF_E_SINK_ALREADYSTOPPED              = _HRESULT_TYPEDEF_($C00D4A3F);                   // 01686.001#define MF_E_SINK_ALREADYSTOPPED         _HRESULT_TYPEDEF_(0xC00D4A3FL)
                                                                                          // 01687.001 
//                                                                                        // 01688.001 
// MessageId: MF_E_ASF_FILESINK_BITRATE_UNKNOWN                                           // 01689.001 
//                                                                                        // 01690.001 
// MessageText:                                                                           // 01691.001 
//                                                                                        // 01692.001 
// The ASF file sink could not reserve AVIO because the bitrate is unknown.%0             // 01693.001 
//                                                                                        // 01694.001 
  MF_E_ASF_FILESINK_BITRATE_UNKNOWN     = _HRESULT_TYPEDEF_($C00D4A40);                   // 01695.001#define MF_E_ASF_FILESINK_BITRATE_UNKNOWN _HRESULT_TYPEDEF_(0xC00D4A40L)
                                                                                          // 01696.001 
//                                                                                        // 01697.001 
// MessageId: MF_E_SINK_NO_STREAMS                                                        // 01698.001 
//                                                                                        // 01699.001 
// MessageText:                                                                           // 01700.001 
//                                                                                        // 01701.001 
// No streams are selected in sink presentation descriptor.%0                             // 01702.001 
//                                                                                        // 01703.001 
  MF_E_SINK_NO_STREAMS                  = _HRESULT_TYPEDEF_($C00D4A41);                   // 01704.001#define MF_E_SINK_NO_STREAMS             _HRESULT_TYPEDEF_(0xC00D4A41L)
                                                                                          // 01705.001 
//                                                                                        // 01706.001 
// MessageId: MF_S_SINK_NOT_FINALIZED                                                     // 01707.001 
//                                                                                        // 01708.001 
// MessageText:                                                                           // 01709.001 
//                                                                                        // 01710.001 
// The sink has not been finalized before shut down. This may cause sink generate a corrupted content.%0 // 01711.001 
//                                                                                        // 01712.001 
  MF_S_SINK_NOT_FINALIZED               = _HRESULT_TYPEDEF_($000D4A42);                   // 01713.001#define MF_S_SINK_NOT_FINALIZED          _HRESULT_TYPEDEF_(0x000D4A42L)
                                                                                          // 01714.001 
//                                                                                        // 01715.001 
// MessageId: MF_E_METADATA_TOO_LONG                                                      // 01716.001 
//                                                                                        // 01717.001 
// MessageText:                                                                           // 01718.001 
//                                                                                        // 01719.001 
// A metadata item was too long to write to the output container.%0                       // 01720.001 
//                                                                                        // 01721.001 
  MF_E_METADATA_TOO_LONG                = _HRESULT_TYPEDEF_($C00D4A43);                   // 01722.001#define MF_E_METADATA_TOO_LONG           _HRESULT_TYPEDEF_(0xC00D4A43L)
                                                                                          // 01723.001 
//                                                                                        // 01724.001 
// MessageId: MF_E_SINK_NO_SAMPLES_PROCESSED                                              // 01725.001 
//                                                                                        // 01726.001 
// MessageText:                                                                           // 01727.001 
//                                                                                        // 01728.001 
// The operation failed because no samples were processed by the sink.%0                  // 01729.001 
//                                                                                        // 01730.001 
  MF_E_SINK_NO_SAMPLES_PROCESSED        = _HRESULT_TYPEDEF_($C00D4A44);                   // 01731.001#define MF_E_SINK_NO_SAMPLES_PROCESSED   _HRESULT_TYPEDEF_(0xC00D4A44L)
                                                                                          // 01732.001 
                                                                                          // 01733.001 
/////////////////////////////////////////////////////////////////////////                 // 01734.001 
//                                                                                        // 01735.001 
// MEDIAFOUNDATION Renderer Error Events                                                  // 01736.001 
//                                                                                        // 01737.001 
/////////////////////////////////////////////////////////////////////////                 // 01738.001 
                                                                                          // 01739.001 
//                                                                                        // 01740.001 
// MessageId: MF_E_VIDEO_REN_NO_PROCAMP_HW                                                // 01741.001 
//                                                                                        // 01742.001 
// MessageText:                                                                           // 01743.001 
//                                                                                        // 01744.001 
// There is no available procamp hardware with which to perform color correction.%0       // 01745.001 
//                                                                                        // 01746.001 
  MF_E_VIDEO_REN_NO_PROCAMP_HW          = _HRESULT_TYPEDEF_($C00D4E20);                   // 01747.001#define MF_E_VIDEO_REN_NO_PROCAMP_HW     _HRESULT_TYPEDEF_(0xC00D4E20L)
                                                                                          // 01748.001 
//                                                                                        // 01749.001 
// MessageId: MF_E_VIDEO_REN_NO_DEINTERLACE_HW                                            // 01750.001 
//                                                                                        // 01751.001 
// MessageText:                                                                           // 01752.001 
//                                                                                        // 01753.001 
// There is no available deinterlacing hardware with which to deinterlace the video stream.%0 // 01754.001 
//                                                                                        // 01755.001 
  MF_E_VIDEO_REN_NO_DEINTERLACE_HW      = _HRESULT_TYPEDEF_($C00D4E21);                   // 01756.001#define MF_E_VIDEO_REN_NO_DEINTERLACE_HW _HRESULT_TYPEDEF_(0xC00D4E21L)
                                                                                          // 01757.001 
//                                                                                        // 01758.001 
// MessageId: MF_E_VIDEO_REN_COPYPROT_FAILED                                              // 01759.001 
//                                                                                        // 01760.001 
// MessageText:                                                                           // 01761.001 
//                                                                                        // 01762.001 
// A video stream requires copy protection to be enabled, but there was a failure in attempting to enable copy protection.%0 // 01763.001 
//                                                                                        // 01764.001 
  MF_E_VIDEO_REN_COPYPROT_FAILED        = _HRESULT_TYPEDEF_($C00D4E22);                   // 01765.001#define MF_E_VIDEO_REN_COPYPROT_FAILED   _HRESULT_TYPEDEF_(0xC00D4E22L)
                                                                                          // 01766.001 
//                                                                                        // 01767.001 
// MessageId: MF_E_VIDEO_REN_SURFACE_NOT_SHARED                                           // 01768.001 
//                                                                                        // 01769.001 
// MessageText:                                                                           // 01770.001 
//                                                                                        // 01771.001 
// A component is attempting to access a surface for sharing that is not shared.%0        // 01772.001 
//                                                                                        // 01773.001 
  MF_E_VIDEO_REN_SURFACE_NOT_SHARED     = _HRESULT_TYPEDEF_($C00D4E23);                   // 01774.001#define MF_E_VIDEO_REN_SURFACE_NOT_SHARED _HRESULT_TYPEDEF_(0xC00D4E23L)
                                                                                          // 01775.001 
//                                                                                        // 01776.001 
// MessageId: MF_E_VIDEO_DEVICE_LOCKED                                                    // 01777.001 
//                                                                                        // 01778.001 
// MessageText:                                                                           // 01779.001 
//                                                                                        // 01780.001 
// A component is attempting to access a shared device that is already locked by another component.%0 // 01781.001 
//                                                                                        // 01782.001 
  MF_E_VIDEO_DEVICE_LOCKED              = _HRESULT_TYPEDEF_($C00D4E24);                   // 01783.001#define MF_E_VIDEO_DEVICE_LOCKED         _HRESULT_TYPEDEF_(0xC00D4E24L)
                                                                                          // 01784.001 
//                                                                                        // 01785.001 
// MessageId: MF_E_NEW_VIDEO_DEVICE                                                       // 01786.001 
//                                                                                        // 01787.001 
// MessageText:                                                                           // 01788.001 
//                                                                                        // 01789.001 
// The device is no longer available. The handle should be closed and a new one opened.%0 // 01790.001 
//                                                                                        // 01791.001 
  MF_E_NEW_VIDEO_DEVICE                 = _HRESULT_TYPEDEF_($C00D4E25);                   // 01792.001#define MF_E_NEW_VIDEO_DEVICE            _HRESULT_TYPEDEF_(0xC00D4E25L)
                                                                                          // 01793.001 
//                                                                                        // 01794.001 
// MessageId: MF_E_NO_VIDEO_SAMPLE_AVAILABLE                                              // 01795.001 
//                                                                                        // 01796.001 
// MessageText:                                                                           // 01797.001 
//                                                                                        // 01798.001 
// A video sample is not currently queued on a stream that is required for mixing.%0      // 01799.001 
//                                                                                        // 01800.001 
  MF_E_NO_VIDEO_SAMPLE_AVAILABLE        = _HRESULT_TYPEDEF_($C00D4E26);                   // 01801.001#define MF_E_NO_VIDEO_SAMPLE_AVAILABLE   _HRESULT_TYPEDEF_(0xC00D4E26L)
                                                                                          // 01802.001 
//                                                                                        // 01803.001 
// MessageId: MF_E_NO_AUDIO_PLAYBACK_DEVICE                                               // 01804.001 
//                                                                                        // 01805.001 
// MessageText:                                                                           // 01806.001 
//                                                                                        // 01807.001 
// No audio playback device was found.%0                                                  // 01808.001 
//                                                                                        // 01809.001 
  MF_E_NO_AUDIO_PLAYBACK_DEVICE         = _HRESULT_TYPEDEF_($C00D4E84);                   // 01810.001#define MF_E_NO_AUDIO_PLAYBACK_DEVICE    _HRESULT_TYPEDEF_(0xC00D4E84L)
                                                                                          // 01811.001 
//                                                                                        // 01812.001 
// MessageId: MF_E_AUDIO_PLAYBACK_DEVICE_IN_USE                                           // 01813.001 
//                                                                                        // 01814.001 
// MessageText:                                                                           // 01815.001 
//                                                                                        // 01816.001 
// The requested audio playback device is currently in use.%0                             // 01817.001 
//                                                                                        // 01818.001 
  MF_E_AUDIO_PLAYBACK_DEVICE_IN_USE     = _HRESULT_TYPEDEF_($C00D4E85);                   // 01819.001#define MF_E_AUDIO_PLAYBACK_DEVICE_IN_USE _HRESULT_TYPEDEF_(0xC00D4E85L)
                                                                                          // 01820.001 
//                                                                                        // 01821.001 
// MessageId: MF_E_AUDIO_PLAYBACK_DEVICE_INVALIDATED                                      // 01822.001 
//                                                                                        // 01823.001 
// MessageText:                                                                           // 01824.001 
//                                                                                        // 01825.001 
// The audio playback device is no longer present.%0                                      // 01826.001 
//                                                                                        // 01827.001 
  MF_E_AUDIO_PLAYBACK_DEVICE_INVALIDATED = _HRESULT_TYPEDEF_($C00D4E86);                  // 01828.001#define MF_E_AUDIO_PLAYBACK_DEVICE_INVALIDATED _HRESULT_TYPEDEF_(0xC00D4E86L)
                                                                                          // 01829.001 
//                                                                                        // 01830.001 
// MessageId: MF_E_AUDIO_SERVICE_NOT_RUNNING                                              // 01831.001 
//                                                                                        // 01832.001 
// MessageText:                                                                           // 01833.001 
//                                                                                        // 01834.001 
// The audio service is not running.%0                                                    // 01835.001 
//                                                                                        // 01836.001 
  MF_E_AUDIO_SERVICE_NOT_RUNNING        = _HRESULT_TYPEDEF_($C00D4E87);                   // 01837.001#define MF_E_AUDIO_SERVICE_NOT_RUNNING   _HRESULT_TYPEDEF_(0xC00D4E87L)
                                                                                          // 01838.001 
                                                                                          // 01839.001 
/////////////////////////////////////////////////////////////////////////                 // 01840.001 
//                                                                                        // 01841.001 
// MEDIAFOUNDATION Topology Error Events                                                  // 01842.001 
//                                                                                        // 01843.001 
/////////////////////////////////////////////////////////////////////////                 // 01844.001 
                                                                                          // 01845.001 
//                                                                                        // 01846.001 
// MessageId: MF_E_TOPO_INVALID_OPTIONAL_NODE                                             // 01847.001 
//                                                                                        // 01848.001 
// MessageText:                                                                           // 01849.001 
//                                                                                        // 01850.001 
// The topology contains an invalid optional node.  Possible reasons are incorrect number of outputs and inputs or optional node is at the beginning or end of a segment. %0 // 01851.001 
//                                                                                        // 01852.001 
  MF_E_TOPO_INVALID_OPTIONAL_NODE       = _HRESULT_TYPEDEF_($C00D520E);                   // 01853.001#define MF_E_TOPO_INVALID_OPTIONAL_NODE  _HRESULT_TYPEDEF_(0xC00D520EL)
                                                                                          // 01854.001 
//                                                                                        // 01855.001 
// MessageId: MF_E_TOPO_CANNOT_FIND_DECRYPTOR                                             // 01856.001 
//                                                                                        // 01857.001 
// MessageText:                                                                           // 01858.001 
//                                                                                        // 01859.001 
// No suitable transform was found to decrypt the content. %0                             // 01860.001 
//                                                                                        // 01861.001 
  MF_E_TOPO_CANNOT_FIND_DECRYPTOR       = _HRESULT_TYPEDEF_($C00D5211);                   // 01862.001#define MF_E_TOPO_CANNOT_FIND_DECRYPTOR  _HRESULT_TYPEDEF_(0xC00D5211L)
                                                                                          // 01863.001 
//                                                                                        // 01864.001 
// MessageId: MF_E_TOPO_CODEC_NOT_FOUND                                                   // 01865.001 
//                                                                                        // 01866.001 
// MessageText:                                                                           // 01867.001 
//                                                                                        // 01868.001 
// No suitable transform was found to encode or decode the content. %0                    // 01869.001 
//                                                                                        // 01870.001 
  MF_E_TOPO_CODEC_NOT_FOUND             = _HRESULT_TYPEDEF_($C00D5212);                   // 01871.001#define MF_E_TOPO_CODEC_NOT_FOUND        _HRESULT_TYPEDEF_(0xC00D5212L)
                                                                                          // 01872.001 
//                                                                                        // 01873.001 
// MessageId: MF_E_TOPO_CANNOT_CONNECT                                                    // 01874.001 
//                                                                                        // 01875.001 
// MessageText:                                                                           // 01876.001 
//                                                                                        // 01877.001 
// Unable to find a way to connect nodes%0                                                // 01878.001 
//                                                                                        // 01879.001 
  MF_E_TOPO_CANNOT_CONNECT              = _HRESULT_TYPEDEF_($C00D5213);                   // 01880.001#define MF_E_TOPO_CANNOT_CONNECT         _HRESULT_TYPEDEF_(0xC00D5213L)
                                                                                          // 01881.001 
//                                                                                        // 01882.001 
// MessageId: MF_E_TOPO_UNSUPPORTED                                                       // 01883.001 
//                                                                                        // 01884.001 
// MessageText:                                                                           // 01885.001 
//                                                                                        // 01886.001 
// Unsupported operations in topoloader%0                                                 // 01887.001 
//                                                                                        // 01888.001 
  MF_E_TOPO_UNSUPPORTED                 = _HRESULT_TYPEDEF_($C00D5214);                   // 01889.001#define MF_E_TOPO_UNSUPPORTED            _HRESULT_TYPEDEF_(0xC00D5214L)
                                                                                          // 01890.001 
//                                                                                        // 01891.001 
// MessageId: MF_E_TOPO_INVALID_TIME_ATTRIBUTES                                           // 01892.001 
//                                                                                        // 01893.001 
// MessageText:                                                                           // 01894.001 
//                                                                                        // 01895.001 
// The topology or its nodes contain incorrectly set time attributes%0                    // 01896.001 
//                                                                                        // 01897.001 
  MF_E_TOPO_INVALID_TIME_ATTRIBUTES     = _HRESULT_TYPEDEF_($C00D5215);                   // 01898.001#define MF_E_TOPO_INVALID_TIME_ATTRIBUTES _HRESULT_TYPEDEF_(0xC00D5215L)
                                                                                          // 01899.001 
//                                                                                        // 01900.001 
// MessageId: MF_E_TOPO_LOOPS_IN_TOPOLOGY                                                 // 01901.001 
//                                                                                        // 01902.001 
// MessageText:                                                                           // 01903.001 
//                                                                                        // 01904.001 
// The topology contains loops, which are unsupported in media foundation topologies%0    // 01905.001 
//                                                                                        // 01906.001 
  MF_E_TOPO_LOOPS_IN_TOPOLOGY           = _HRESULT_TYPEDEF_($C00D5216);                   // 01907.001#define MF_E_TOPO_LOOPS_IN_TOPOLOGY      _HRESULT_TYPEDEF_(0xC00D5216L)
                                                                                          // 01908.001 
//                                                                                        // 01909.001 
// MessageId: MF_E_TOPO_MISSING_PRESENTATION_DESCRIPTOR                                   // 01910.001 
//                                                                                        // 01911.001 
// MessageText:                                                                           // 01912.001 
//                                                                                        // 01913.001 
// A source stream node in the topology does not have a presentation descriptor%0         // 01914.001 
//                                                                                        // 01915.001 
  MF_E_TOPO_MISSING_PRESENTATION_DESCRIPTOR = _HRESULT_TYPEDEF_($C00D5217);               // 01916.001#define MF_E_TOPO_MISSING_PRESENTATION_DESCRIPTOR _HRESULT_TYPEDEF_(0xC00D5217L)
                                                                                          // 01917.001 
//                                                                                        // 01918.001 
// MessageId: MF_E_TOPO_MISSING_STREAM_DESCRIPTOR                                         // 01919.001 
//                                                                                        // 01920.001 
// MessageText:                                                                           // 01921.001 
//                                                                                        // 01922.001 
// A source stream node in the topology does not have a stream descriptor%0               // 01923.001 
//                                                                                        // 01924.001 
  MF_E_TOPO_MISSING_STREAM_DESCRIPTOR   = _HRESULT_TYPEDEF_($C00D5218);                   // 01925.001#define MF_E_TOPO_MISSING_STREAM_DESCRIPTOR _HRESULT_TYPEDEF_(0xC00D5218L)
                                                                                          // 01926.001 
//                                                                                        // 01927.001 
// MessageId: MF_E_TOPO_STREAM_DESCRIPTOR_NOT_SELECTED                                    // 01928.001 
//                                                                                        // 01929.001 
// MessageText:                                                                           // 01930.001 
//                                                                                        // 01931.001 
// A stream descriptor was set on a source stream node but it was not selected on the presentation descriptor%0 // 01932.001 
//                                                                                        // 01933.001 
  MF_E_TOPO_STREAM_DESCRIPTOR_NOT_SELECTED = _HRESULT_TYPEDEF_($C00D5219);                // 01934.001#define MF_E_TOPO_STREAM_DESCRIPTOR_NOT_SELECTED _HRESULT_TYPEDEF_(0xC00D5219L)
                                                                                          // 01935.001 
//                                                                                        // 01936.001 
// MessageId: MF_E_TOPO_MISSING_SOURCE                                                    // 01937.001 
//                                                                                        // 01938.001 
// MessageText:                                                                           // 01939.001 
//                                                                                        // 01940.001 
// A source stream node in the topology does not have a source%0                          // 01941.001 
//                                                                                        // 01942.001 
  MF_E_TOPO_MISSING_SOURCE              = _HRESULT_TYPEDEF_($C00D521A);                   // 01943.001#define MF_E_TOPO_MISSING_SOURCE         _HRESULT_TYPEDEF_(0xC00D521AL)
                                                                                          // 01944.001 
//                                                                                        // 01945.001 
// MessageId: MF_E_TOPO_SINK_ACTIVATES_UNSUPPORTED                                        // 01946.001 
//                                                                                        // 01947.001 
// MessageText:                                                                           // 01948.001 
//                                                                                        // 01949.001 
// The topology loader does not support sink activates on output nodes.%0                 // 01950.001 
//                                                                                        // 01951.001 
  MF_E_TOPO_SINK_ACTIVATES_UNSUPPORTED  = _HRESULT_TYPEDEF_($C00D521B);                   // 01952.001#define MF_E_TOPO_SINK_ACTIVATES_UNSUPPORTED _HRESULT_TYPEDEF_(0xC00D521BL)
                                                                                          // 01953.001 
                                                                                          // 01954.001 
/////////////////////////////////////////////////////////////////////////                 // 01955.001 
//                                                                                        // 01956.001 
// MEDIAFOUNDATION Timeline Error Events                                                  // 01957.001 
//                                                                                        // 01958.001 
/////////////////////////////////////////////////////////////////////////                 // 01959.001 
                                                                                          // 01960.001 
//                                                                                        // 01961.001 
// MessageId: MF_E_SEQUENCER_UNKNOWN_SEGMENT_ID                                           // 01962.001 
//                                                                                        // 01963.001 
// MessageText:                                                                           // 01964.001 
//                                                                                        // 01965.001 
// The sequencer cannot find a segment with the given ID.%0\n.                            // 01966.001 
//                                                                                        // 01967.001 
  MF_E_SEQUENCER_UNKNOWN_SEGMENT_ID     = _HRESULT_TYPEDEF_($C00D61AC);                   // 01968.001#define MF_E_SEQUENCER_UNKNOWN_SEGMENT_ID _HRESULT_TYPEDEF_(0xC00D61ACL)
                                                                                          // 01969.001 
//                                                                                        // 01970.001 
// MessageId: MF_S_SEQUENCER_CONTEXT_CANCELED                                             // 01971.001 
//                                                                                        // 01972.001 
// MessageText:                                                                           // 01973.001 
//                                                                                        // 01974.001 
// The context was canceled.%0\n.                                                         // 01975.001 
//                                                                                        // 01976.001 
  MF_S_SEQUENCER_CONTEXT_CANCELED       = _HRESULT_TYPEDEF_($000D61AD);                   // 01977.001#define MF_S_SEQUENCER_CONTEXT_CANCELED  _HRESULT_TYPEDEF_(0x000D61ADL)
                                                                                          // 01978.001 
//                                                                                        // 01979.001 
// MessageId: MF_E_NO_SOURCE_IN_CACHE                                                     // 01980.001 
//                                                                                        // 01981.001 
// MessageText:                                                                           // 01982.001 
//                                                                                        // 01983.001 
// Cannot find source in source cache.%0\n.                                               // 01984.001 
//                                                                                        // 01985.001 
  MF_E_NO_SOURCE_IN_CACHE               = _HRESULT_TYPEDEF_($C00D61AE);                   // 01986.001#define MF_E_NO_SOURCE_IN_CACHE          _HRESULT_TYPEDEF_(0xC00D61AEL)
                                                                                          // 01987.001 
//                                                                                        // 01988.001 
// MessageId: MF_S_SEQUENCER_SEGMENT_AT_END_OF_STREAM                                     // 01989.001 
//                                                                                        // 01990.001 
// MessageText:                                                                           // 01991.001 
//                                                                                        // 01992.001 
// Cannot update topology flags.%0\n.                                                     // 01993.001 
//                                                                                        // 01994.001 
  MF_S_SEQUENCER_SEGMENT_AT_END_OF_STREAM = _HRESULT_TYPEDEF_($000D61AF);                 // 01995.001#define MF_S_SEQUENCER_SEGMENT_AT_END_OF_STREAM _HRESULT_TYPEDEF_(0x000D61AFL)
                                                                                          // 01996.001 
                                                                                          // 01997.001 
//////////////////////////////////////////////////////////////////////////////            // 01998.001 
//                                                                                        // 01999.001 
// Transform errors                                                                       // 02000.001 
//                                                                                        // 02001.001 
//////////////////////////////////////////////////////////////////////////////            // 02002.001 
                                                                                          // 02003.001 
//                                                                                        // 02004.001 
// MessageId: MF_E_TRANSFORM_TYPE_NOT_SET                                                 // 02005.001 
//                                                                                        // 02006.001 
// MessageText:                                                                           // 02007.001 
//                                                                                        // 02008.001 
// A valid type has not been set for this stream or a stream that it depends on.%0        // 02009.001 
//                                                                                        // 02010.001 
  MF_E_TRANSFORM_TYPE_NOT_SET           = _HRESULT_TYPEDEF_($C00D6D60);                   // 02011.001#define MF_E_TRANSFORM_TYPE_NOT_SET      _HRESULT_TYPEDEF_(0xC00D6D60L)
                                                                                          // 02012.001 
//                                                                                        // 02013.001 
// MessageId: MF_E_TRANSFORM_STREAM_CHANGE                                                // 02014.001 
//                                                                                        // 02015.001 
// MessageText:                                                                           // 02016.001 
//                                                                                        // 02017.001 
// A stream change has occurred. Output cannot be produced until the streams have been renegotiated.%0 // 02018.001 
//                                                                                        // 02019.001 
  MF_E_TRANSFORM_STREAM_CHANGE          = _HRESULT_TYPEDEF_($C00D6D61);                   // 02020.001#define MF_E_TRANSFORM_STREAM_CHANGE     _HRESULT_TYPEDEF_(0xC00D6D61L)
                                                                                          // 02021.001 
//                                                                                        // 02022.001 
// MessageId: MF_E_TRANSFORM_INPUT_REMAINING                                              // 02023.001 
//                                                                                        // 02024.001 
// MessageText:                                                                           // 02025.001 
//                                                                                        // 02026.001 
// The transform cannot take the requested action until all of the input data it currently holds is processed or flushed.%0 // 02027.001 
//                                                                                        // 02028.001 
  MF_E_TRANSFORM_INPUT_REMAINING        = _HRESULT_TYPEDEF_($C00D6D62);                   // 02029.001#define MF_E_TRANSFORM_INPUT_REMAINING   _HRESULT_TYPEDEF_(0xC00D6D62L)
                                                                                          // 02030.001 
//                                                                                        // 02031.001 
// MessageId: MF_E_TRANSFORM_PROFILE_MISSING                                              // 02032.001 
//                                                                                        // 02033.001 
// MessageText:                                                                           // 02034.001 
//                                                                                        // 02035.001 
// The transform requires a profile but no profile was supplied or found.%0               // 02036.001 
//                                                                                        // 02037.001 
  MF_E_TRANSFORM_PROFILE_MISSING        = _HRESULT_TYPEDEF_($C00D6D63);                   // 02038.001#define MF_E_TRANSFORM_PROFILE_MISSING   _HRESULT_TYPEDEF_(0xC00D6D63L)
                                                                                          // 02039.001 
//                                                                                        // 02040.001 
// MessageId: MF_E_TRANSFORM_PROFILE_INVALID_OR_CORRUPT                                   // 02041.001 
//                                                                                        // 02042.001 
// MessageText:                                                                           // 02043.001 
//                                                                                        // 02044.001 
// The transform requires a profile but the supplied profile was invalid or corrupt.%0    // 02045.001 
//                                                                                        // 02046.001 
  MF_E_TRANSFORM_PROFILE_INVALID_OR_CORRUPT = _HRESULT_TYPEDEF_($C00D6D64);               // 02047.001#define MF_E_TRANSFORM_PROFILE_INVALID_OR_CORRUPT _HRESULT_TYPEDEF_(0xC00D6D64L)
                                                                                          // 02048.001 
//                                                                                        // 02049.001 
// MessageId: MF_E_TRANSFORM_PROFILE_TRUNCATED                                            // 02050.001 
//                                                                                        // 02051.001 
// MessageText:                                                                           // 02052.001 
//                                                                                        // 02053.001 
// The transform requires a profile but the supplied profile ended unexpectedly while parsing.%0 // 02054.001 
//                                                                                        // 02055.001 
  MF_E_TRANSFORM_PROFILE_TRUNCATED      = _HRESULT_TYPEDEF_($C00D6D65);                   // 02056.001#define MF_E_TRANSFORM_PROFILE_TRUNCATED _HRESULT_TYPEDEF_(0xC00D6D65L)
                                                                                          // 02057.001 
//                                                                                        // 02058.001 
// MessageId: MF_E_TRANSFORM_PROPERTY_PID_NOT_RECOGNIZED                                  // 02059.001 
//                                                                                        // 02060.001 
// MessageText:                                                                           // 02061.001 
//                                                                                        // 02062.001 
// The property ID does not match any property supported by the transform.%0              // 02063.001 
//                                                                                        // 02064.001 
  MF_E_TRANSFORM_PROPERTY_PID_NOT_RECOGNIZED = _HRESULT_TYPEDEF_($C00D6D66);              // 02065.001#define MF_E_TRANSFORM_PROPERTY_PID_NOT_RECOGNIZED _HRESULT_TYPEDEF_(0xC00D6D66L)
                                                                                          // 02066.001 
//                                                                                        // 02067.001 
// MessageId: MF_E_TRANSFORM_PROPERTY_VARIANT_TYPE_WRONG                                  // 02068.001 
//                                                                                        // 02069.001 
// MessageText:                                                                           // 02070.001 
//                                                                                        // 02071.001 
// The variant does not have the type expected for this property ID.%0                    // 02072.001 
//                                                                                        // 02073.001 
  MF_E_TRANSFORM_PROPERTY_VARIANT_TYPE_WRONG = _HRESULT_TYPEDEF_($C00D6D67);              // 02074.001#define MF_E_TRANSFORM_PROPERTY_VARIANT_TYPE_WRONG _HRESULT_TYPEDEF_(0xC00D6D67L)
                                                                                          // 02075.001 
//                                                                                        // 02076.001 
// MessageId: MF_E_TRANSFORM_PROPERTY_NOT_WRITEABLE                                       // 02077.001 
//                                                                                        // 02078.001 
// MessageText:                                                                           // 02079.001 
//                                                                                        // 02080.001 
// An attempt was made to set the value on a read-only property.%0                        // 02081.001 
//                                                                                        // 02082.001 
  MF_E_TRANSFORM_PROPERTY_NOT_WRITEABLE = _HRESULT_TYPEDEF_($C00D6D68);                   // 02083.001#define MF_E_TRANSFORM_PROPERTY_NOT_WRITEABLE _HRESULT_TYPEDEF_(0xC00D6D68L)
                                                                                          // 02084.001 
//                                                                                        // 02085.001 
// MessageId: MF_E_TRANSFORM_PROPERTY_ARRAY_VALUE_WRONG_NUM_DIM                           // 02086.001 
//                                                                                        // 02087.001 
// MessageText:                                                                           // 02088.001 
//                                                                                        // 02089.001 
// The array property value has an unexpected number of dimensions.%0                     // 02090.001 
//                                                                                        // 02091.001 
  MF_E_TRANSFORM_PROPERTY_ARRAY_VALUE_WRONG_NUM_DIM = _HRESULT_TYPEDEF_($C00D6D69);       // 02092.001#define MF_E_TRANSFORM_PROPERTY_ARRAY_VALUE_WRONG_NUM_DIM _HRESULT_TYPEDEF_(0xC00D6D69L)
                                                                                          // 02093.001 
//                                                                                        // 02094.001 
// MessageId: MF_E_TRANSFORM_PROPERTY_VALUE_SIZE_WRONG                                    // 02095.001 
//                                                                                        // 02096.001 
// MessageText:                                                                           // 02097.001 
//                                                                                        // 02098.001 
// The array or blob property value has an unexpected size.%0                             // 02099.001 
//                                                                                        // 02100.001 
  MF_E_TRANSFORM_PROPERTY_VALUE_SIZE_WRONG = _HRESULT_TYPEDEF_($C00D6D6A);                // 02101.001#define MF_E_TRANSFORM_PROPERTY_VALUE_SIZE_WRONG _HRESULT_TYPEDEF_(0xC00D6D6AL)
                                                                                          // 02102.001 
//                                                                                        // 02103.001 
// MessageId: MF_E_TRANSFORM_PROPERTY_VALUE_OUT_OF_RANGE                                  // 02104.001 
//                                                                                        // 02105.001 
// MessageText:                                                                           // 02106.001 
//                                                                                        // 02107.001 
// The property value is out of range for this transform.%0                               // 02108.001 
//                                                                                        // 02109.001 
  MF_E_TRANSFORM_PROPERTY_VALUE_OUT_OF_RANGE = _HRESULT_TYPEDEF_($C00D6D6B);              // 02110.001#define MF_E_TRANSFORM_PROPERTY_VALUE_OUT_OF_RANGE _HRESULT_TYPEDEF_(0xC00D6D6BL)
                                                                                          // 02111.001 
//                                                                                        // 02112.001 
// MessageId: MF_E_TRANSFORM_PROPERTY_VALUE_INCOMPATIBLE                                  // 02113.001 
//                                                                                        // 02114.001 
// MessageText:                                                                           // 02115.001 
//                                                                                        // 02116.001 
// The property value is incompatible with some other property or mediatype set on the transform.%0 // 02117.001 
//                                                                                        // 02118.001 
  MF_E_TRANSFORM_PROPERTY_VALUE_INCOMPATIBLE = _HRESULT_TYPEDEF_($C00D6D6C);              // 02119.001#define MF_E_TRANSFORM_PROPERTY_VALUE_INCOMPATIBLE _HRESULT_TYPEDEF_(0xC00D6D6CL)
                                                                                          // 02120.001 
//                                                                                        // 02121.001 
// MessageId: MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_OUTPUT_MEDIATYPE                    // 02122.001 
//                                                                                        // 02123.001 
// MessageText:                                                                           // 02124.001 
//                                                                                        // 02125.001 
// The requested operation is not supported for the currently set output mediatype.%0     // 02126.001 
//                                                                                        // 02127.001 
  MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_OUTPUT_MEDIATYPE = _HRESULT_TYPEDEF_($C00D6D6D); // 02128.001#define MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_OUTPUT_MEDIATYPE _HRESULT_TYPEDEF_(0xC00D6D6DL)
                                                                                          // 02129.001 
//                                                                                        // 02130.001 
// MessageId: MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_INPUT_MEDIATYPE                     // 02131.001 
//                                                                                        // 02132.001 
// MessageText:                                                                           // 02133.001 
//                                                                                        // 02134.001 
// The requested operation is not supported for the currently set input mediatype.%0      // 02135.001 
//                                                                                        // 02136.001 
  MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_INPUT_MEDIATYPE = _HRESULT_TYPEDEF_($C00D6D6E); // 02137.001#define MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_INPUT_MEDIATYPE _HRESULT_TYPEDEF_(0xC00D6D6EL)
                                                                                          // 02138.001 
//                                                                                        // 02139.001 
// MessageId: MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_MEDIATYPE_COMBINATION               // 02140.001 
//                                                                                        // 02141.001 
// MessageText:                                                                           // 02142.001 
//                                                                                        // 02143.001 
// The requested operation is not supported for the currently set combination of mediatypes.%0 // 02144.001 
//                                                                                        // 02145.001 
  MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_MEDIATYPE_COMBINATION = _HRESULT_TYPEDEF_($C00D6D6F); // 02146.001#define MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_MEDIATYPE_COMBINATION _HRESULT_TYPEDEF_(0xC00D6D6FL)
                                                                                          // 02147.001 
//                                                                                        // 02148.001 
// MessageId: MF_E_TRANSFORM_CONFLICTS_WITH_OTHER_CURRENTLY_ENABLED_FEATURES              // 02149.001 
//                                                                                        // 02150.001 
// MessageText:                                                                           // 02151.001 
//                                                                                        // 02152.001 
// The requested feature is not supported in combination with some other currently enabled feature.%0 // 02153.001 
//                                                                                        // 02154.001 
  MF_E_TRANSFORM_CONFLICTS_WITH_OTHER_CURRENTLY_ENABLED_FEATURES = _HRESULT_TYPEDEF_($C00D6D70); // 02155.001#define MF_E_TRANSFORM_CONFLICTS_WITH_OTHER_CURRENTLY_ENABLED_FEATURES _HRESULT_TYPEDEF_(0xC00D6D70L)
                                                                                          // 02156.001 
//                                                                                        // 02157.001 
// MessageId: MF_E_TRANSFORM_NEED_MORE_INPUT                                              // 02158.001 
//                                                                                        // 02159.001 
// MessageText:                                                                           // 02160.001 
//                                                                                        // 02161.001 
// The transform cannot produce output until it gets more input samples.%0                // 02162.001 
//                                                                                        // 02163.001 
  MF_E_TRANSFORM_NEED_MORE_INPUT        = _HRESULT_TYPEDEF_($C00D6D72);                   // 02164.001#define MF_E_TRANSFORM_NEED_MORE_INPUT   _HRESULT_TYPEDEF_(0xC00D6D72L)
                                                                                          // 02165.001 
//                                                                                        // 02166.001 
// MessageId: MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_SPKR_CONFIG                         // 02167.001 
//                                                                                        // 02168.001 
// MessageText:                                                                           // 02169.001 
//                                                                                        // 02170.001 
// The requested operation is not supported for the current speaker configuration.%0      // 02171.001 
//                                                                                        // 02172.001 
  MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_SPKR_CONFIG = _HRESULT_TYPEDEF_($C00D6D73);     // 02173.001#define MF_E_TRANSFORM_NOT_POSSIBLE_FOR_CURRENT_SPKR_CONFIG _HRESULT_TYPEDEF_(0xC00D6D73L)
                                                                                          // 02174.001 
//                                                                                        // 02175.001 
// MessageId: MF_E_TRANSFORM_CANNOT_CHANGE_MEDIATYPE_WHILE_PROCESSING                     // 02176.001 
//                                                                                        // 02177.001 
// MessageText:                                                                           // 02178.001 
//                                                                                        // 02179.001 
// The transform cannot accept mediatype changes in the middle of processing.%0           // 02180.001 
//                                                                                        // 02181.001 
  MF_E_TRANSFORM_CANNOT_CHANGE_MEDIATYPE_WHILE_PROCESSING = _HRESULT_TYPEDEF_($C00D6D74); // 02182.001#define MF_E_TRANSFORM_CANNOT_CHANGE_MEDIATYPE_WHILE_PROCESSING _HRESULT_TYPEDEF_(0xC00D6D74L)
                                                                                          // 02183.001 
//                                                                                        // 02184.001 
// MessageId: MF_S_TRANSFORM_DO_NOT_PROPAGATE_EVENT                                       // 02185.001 
//                                                                                        // 02186.001 
// MessageText:                                                                           // 02187.001 
//                                                                                        // 02188.001 
// The caller should not propagate this event to downstream components.%0                 // 02189.001 
//                                                                                        // 02190.001 
  MF_S_TRANSFORM_DO_NOT_PROPAGATE_EVENT = _HRESULT_TYPEDEF_($000D6D75);                   // 02191.001#define MF_S_TRANSFORM_DO_NOT_PROPAGATE_EVENT _HRESULT_TYPEDEF_(0x000D6D75L)
                                                                                          // 02192.001 
//                                                                                        // 02193.001 
// MessageId: MF_E_UNSUPPORTED_D3D_TYPE                                                   // 02194.001 
//                                                                                        // 02195.001 
// MessageText:                                                                           // 02196.001 
//                                                                                        // 02197.001 
// The input type is not supported for D3D device.%0                                      // 02198.001 
//                                                                                        // 02199.001 
  MF_E_UNSUPPORTED_D3D_TYPE             = _HRESULT_TYPEDEF_($C00D6D76);                   // 02200.001#define MF_E_UNSUPPORTED_D3D_TYPE        _HRESULT_TYPEDEF_(0xC00D6D76L)
                                                                                          // 02201.001 
//                                                                                        // 02202.001 
// MessageId: MF_E_TRANSFORM_ASYNC_LOCKED                                                 // 02203.001 
//                                                                                        // 02204.001 
// MessageText:                                                                           // 02205.001 
//                                                                                        // 02206.001 
// The caller does not appear to support this transform's asynchronous capabilities.%0    // 02207.001 
//                                                                                        // 02208.001 
  MF_E_TRANSFORM_ASYNC_LOCKED           = _HRESULT_TYPEDEF_($C00D6D77);                   // 02209.001#define MF_E_TRANSFORM_ASYNC_LOCKED      _HRESULT_TYPEDEF_(0xC00D6D77L)
                                                                                          // 02210.001 
//                                                                                        // 02211.001 
// MessageId: MF_E_TRANSFORM_CANNOT_INITIALIZE_ACM_DRIVER                                 // 02212.001 
//                                                                                        // 02213.001 
// MessageText:                                                                           // 02214.001 
//                                                                                        // 02215.001 
// An audio compression manager driver could not be initialized by the transform.%0       // 02216.001 
//                                                                                        // 02217.001 
  MF_E_TRANSFORM_CANNOT_INITIALIZE_ACM_DRIVER = _HRESULT_TYPEDEF_($C00D6D78);             // 02218.001#define MF_E_TRANSFORM_CANNOT_INITIALIZE_ACM_DRIVER _HRESULT_TYPEDEF_(0xC00D6D78L)
                                                                                          // 02219.001 
                                                                                          // 02220.001 
//////////////////////////////////////////////////////////////////////////////            // 02221.001 
//                                                                                        // 02222.001 
// Content Protection errors                                                              // 02223.001 
//                                                                                        // 02224.001 
//////////////////////////////////////////////////////////////////////////////            // 02225.001 
                                                                                          // 02226.001 
//                                                                                        // 02227.001 
// MessageId: MF_E_LICENSE_INCORRECT_RIGHTS                                               // 02228.001 
//                                                                                        // 02229.001 
// MessageText:                                                                           // 02230.001 
//                                                                                        // 02231.001 
// You are not allowed to open this file. Contact the content provider for further assistance.%0 // 02232.001 
//                                                                                        // 02233.001 
  MF_E_LICENSE_INCORRECT_RIGHTS         = _HRESULT_TYPEDEF_($C00D7148);                   // 02234.001#define MF_E_LICENSE_INCORRECT_RIGHTS    _HRESULT_TYPEDEF_(0xC00D7148L)
                                                                                          // 02235.001 
//                                                                                        // 02236.001 
// MessageId: MF_E_LICENSE_OUTOFDATE                                                      // 02237.001 
//                                                                                        // 02238.001 
// MessageText:                                                                           // 02239.001 
//                                                                                        // 02240.001 
// The license for this media file has expired. Get a new license or contact the content provider for further assistance.%0 // 02241.001 
//                                                                                        // 02242.001 
  MF_E_LICENSE_OUTOFDATE                = _HRESULT_TYPEDEF_($C00D7149);                   // 02243.001#define MF_E_LICENSE_OUTOFDATE           _HRESULT_TYPEDEF_(0xC00D7149L)
                                                                                          // 02244.001 
//                                                                                        // 02245.001 
// MessageId: MF_E_LICENSE_REQUIRED                                                       // 02246.001 
//                                                                                        // 02247.001 
// MessageText:                                                                           // 02248.001 
//                                                                                        // 02249.001 
// You need a license to perform the requested operation on this media file.%0            // 02250.001 
//                                                                                        // 02251.001 
  MF_E_LICENSE_REQUIRED                 = _HRESULT_TYPEDEF_($C00D714A);                   // 02252.001#define MF_E_LICENSE_REQUIRED            _HRESULT_TYPEDEF_(0xC00D714AL)
                                                                                          // 02253.001 
//                                                                                        // 02254.001 
// MessageId: MF_E_DRM_HARDWARE_INCONSISTENT                                              // 02255.001 
//                                                                                        // 02256.001 
// MessageText:                                                                           // 02257.001 
//                                                                                        // 02258.001 
// The licenses for your media files are corrupted. Contact Microsoft product support.%0  // 02259.001 
//                                                                                        // 02260.001 
  MF_E_DRM_HARDWARE_INCONSISTENT        = _HRESULT_TYPEDEF_($C00D714B);                   // 02261.001#define MF_E_DRM_HARDWARE_INCONSISTENT   _HRESULT_TYPEDEF_(0xC00D714BL)
                                                                                          // 02262.001 
//                                                                                        // 02263.001 
// MessageId: MF_E_NO_CONTENT_PROTECTION_MANAGER                                          // 02264.001 
//                                                                                        // 02265.001 
// MessageText:                                                                           // 02266.001 
//                                                                                        // 02267.001 
// The APP needs to provide IMFContentProtectionManager callback to access the protected media file.%0 // 02268.001 
//                                                                                        // 02269.001 
  MF_E_NO_CONTENT_PROTECTION_MANAGER    = _HRESULT_TYPEDEF_($C00D714C);                   // 02270.001#define MF_E_NO_CONTENT_PROTECTION_MANAGER _HRESULT_TYPEDEF_(0xC00D714CL)
                                                                                          // 02271.001 
//                                                                                        // 02272.001 
// MessageId: MF_E_LICENSE_RESTORE_NO_RIGHTS                                              // 02273.001 
//                                                                                        // 02274.001 
// MessageText:                                                                           // 02275.001 
//                                                                                        // 02276.001 
// Client does not have rights to restore licenses.%0                                     // 02277.001 
//                                                                                        // 02278.001 
  MF_E_LICENSE_RESTORE_NO_RIGHTS        = _HRESULT_TYPEDEF_($C00D714D);                   // 02279.001#define MF_E_LICENSE_RESTORE_NO_RIGHTS   _HRESULT_TYPEDEF_(0xC00D714DL)
                                                                                          // 02280.001 
//                                                                                        // 02281.001 
// MessageId: MF_E_BACKUP_RESTRICTED_LICENSE                                              // 02282.001 
//                                                                                        // 02283.001 
// MessageText:                                                                           // 02284.001 
//                                                                                        // 02285.001 
// Licenses are restricted and hence can not be backed up.%0                              // 02286.001 
//                                                                                        // 02287.001 
  MF_E_BACKUP_RESTRICTED_LICENSE        = _HRESULT_TYPEDEF_($C00D714E);                   // 02288.001#define MF_E_BACKUP_RESTRICTED_LICENSE   _HRESULT_TYPEDEF_(0xC00D714EL)
                                                                                          // 02289.001 
//                                                                                        // 02290.001 
// MessageId: MF_E_LICENSE_RESTORE_NEEDS_INDIVIDUALIZATION                                // 02291.001 
//                                                                                        // 02292.001 
// MessageText:                                                                           // 02293.001 
//                                                                                        // 02294.001 
// License restore requires machine to be individualized.%0                               // 02295.001 
//                                                                                        // 02296.001 
  MF_E_LICENSE_RESTORE_NEEDS_INDIVIDUALIZATION = _HRESULT_TYPEDEF_($C00D714F);            // 02297.001#define MF_E_LICENSE_RESTORE_NEEDS_INDIVIDUALIZATION _HRESULT_TYPEDEF_(0xC00D714FL)
                                                                                          // 02298.001 
//                                                                                        // 02299.001 
// MessageId: MF_S_PROTECTION_NOT_REQUIRED                                                // 02300.001 
//                                                                                        // 02301.001 
// MessageText:                                                                           // 02302.001 
//                                                                                        // 02303.001 
// Protection for stream is not required.%0                                               // 02304.001 
//                                                                                        // 02305.001 
  MF_S_PROTECTION_NOT_REQUIRED          = _HRESULT_TYPEDEF_($000D7150);                   // 02306.001#define MF_S_PROTECTION_NOT_REQUIRED     _HRESULT_TYPEDEF_(0x000D7150L)
                                                                                          // 02307.001 
//                                                                                        // 02308.001 
// MessageId: MF_E_COMPONENT_REVOKED                                                      // 02309.001 
//                                                                                        // 02310.001 
// MessageText:                                                                           // 02311.001 
//                                                                                        // 02312.001 
// Component is revoked.%0                                                                // 02313.001 
//                                                                                        // 02314.001 
  MF_E_COMPONENT_REVOKED                = _HRESULT_TYPEDEF_($C00D7151);                   // 02315.001#define MF_E_COMPONENT_REVOKED           _HRESULT_TYPEDEF_(0xC00D7151L)
                                                                                          // 02316.001 
//                                                                                        // 02317.001 
// MessageId: MF_E_TRUST_DISABLED                                                         // 02318.001 
//                                                                                        // 02319.001 
// MessageText:                                                                           // 02320.001 
//                                                                                        // 02321.001 
// Trusted functionality is currently disabled on this component.%0                       // 02322.001 
//                                                                                        // 02323.001 
  MF_E_TRUST_DISABLED                   = _HRESULT_TYPEDEF_($C00D7152);                   // 02324.001#define MF_E_TRUST_DISABLED              _HRESULT_TYPEDEF_(0xC00D7152L)
                                                                                          // 02325.001 
//                                                                                        // 02326.001 
// MessageId: MF_E_WMDRMOTA_NO_ACTION                                                     // 02327.001 
//                                                                                        // 02328.001 
// MessageText:                                                                           // 02329.001 
//                                                                                        // 02330.001 
// No Action is set on WMDRM Output Trust Authority.%0                                    // 02331.001 
//                                                                                        // 02332.001 
  MF_E_WMDRMOTA_NO_ACTION               = _HRESULT_TYPEDEF_($C00D7153);                   // 02333.001#define MF_E_WMDRMOTA_NO_ACTION          _HRESULT_TYPEDEF_(0xC00D7153L)
                                                                                          // 02334.001 
//                                                                                        // 02335.001 
// MessageId: MF_E_WMDRMOTA_ACTION_ALREADY_SET                                            // 02336.001 
//                                                                                        // 02337.001 
// MessageText:                                                                           // 02338.001 
//                                                                                        // 02339.001 
// Action is already set on WMDRM Output Trust Authority.%0                               // 02340.001 
//                                                                                        // 02341.001 
  MF_E_WMDRMOTA_ACTION_ALREADY_SET      = _HRESULT_TYPEDEF_($C00D7154);                   // 02342.001#define MF_E_WMDRMOTA_ACTION_ALREADY_SET _HRESULT_TYPEDEF_(0xC00D7154L)
                                                                                          // 02343.001 
//                                                                                        // 02344.001 
// MessageId: MF_E_WMDRMOTA_DRM_HEADER_NOT_AVAILABLE                                      // 02345.001 
//                                                                                        // 02346.001 
// MessageText:                                                                           // 02347.001 
//                                                                                        // 02348.001 
// DRM Heaader is not available.%0                                                        // 02349.001 
//                                                                                        // 02350.001 
  MF_E_WMDRMOTA_DRM_HEADER_NOT_AVAILABLE = _HRESULT_TYPEDEF_($C00D7155);                  // 02351.001#define MF_E_WMDRMOTA_DRM_HEADER_NOT_AVAILABLE _HRESULT_TYPEDEF_(0xC00D7155L)
                                                                                          // 02352.001 
//                                                                                        // 02353.001 
// MessageId: MF_E_WMDRMOTA_DRM_ENCRYPTION_SCHEME_NOT_SUPPORTED                           // 02354.001 
//                                                                                        // 02355.001 
// MessageText:                                                                           // 02356.001 
//                                                                                        // 02357.001 
// Current encryption scheme is not supported.%0                                          // 02358.001 
//                                                                                        // 02359.001 
  MF_E_WMDRMOTA_DRM_ENCRYPTION_SCHEME_NOT_SUPPORTED = _HRESULT_TYPEDEF_($C00D7156);       // 02360.001#define MF_E_WMDRMOTA_DRM_ENCRYPTION_SCHEME_NOT_SUPPORTED _HRESULT_TYPEDEF_(0xC00D7156L)
                                                                                          // 02361.001 
//                                                                                        // 02362.001 
// MessageId: MF_E_WMDRMOTA_ACTION_MISMATCH                                               // 02363.001 
//                                                                                        // 02364.001 
// MessageText:                                                                           // 02365.001 
//                                                                                        // 02366.001 
// Action does not match with current configuration.%0                                    // 02367.001 
//                                                                                        // 02368.001 
  MF_E_WMDRMOTA_ACTION_MISMATCH         = _HRESULT_TYPEDEF_($C00D7157);                   // 02369.001#define MF_E_WMDRMOTA_ACTION_MISMATCH    _HRESULT_TYPEDEF_(0xC00D7157L)
                                                                                          // 02370.001 
//                                                                                        // 02371.001 
// MessageId: MF_E_WMDRMOTA_INVALID_POLICY                                                // 02372.001 
//                                                                                        // 02373.001 
// MessageText:                                                                           // 02374.001 
//                                                                                        // 02375.001 
// Invalid policy for WMDRM Output Trust Authority.%0                                     // 02376.001 
//                                                                                        // 02377.001 
  MF_E_WMDRMOTA_INVALID_POLICY          = _HRESULT_TYPEDEF_($C00D7158);                   // 02378.001#define MF_E_WMDRMOTA_INVALID_POLICY     _HRESULT_TYPEDEF_(0xC00D7158L)
                                                                                          // 02379.001 
//                                                                                        // 02380.001 
// MessageId: MF_E_POLICY_UNSUPPORTED                                                     // 02381.001 
//                                                                                        // 02382.001 
// MessageText:                                                                           // 02383.001 
//                                                                                        // 02384.001 
// The policies that the Input Trust Authority requires to be enforced are unsupported by the outputs.%0 // 02385.001 
//                                                                                        // 02386.001 
  MF_E_POLICY_UNSUPPORTED               = _HRESULT_TYPEDEF_($C00D7159);                   // 02387.001#define MF_E_POLICY_UNSUPPORTED          _HRESULT_TYPEDEF_(0xC00D7159L)
                                                                                          // 02388.001 
//                                                                                        // 02389.001 
// MessageId: MF_E_OPL_NOT_SUPPORTED                                                      // 02390.001 
//                                                                                        // 02391.001 
// MessageText:                                                                           // 02392.001 
//                                                                                        // 02393.001 
// The OPL that the license requires to be enforced are not supported by the Input Trust Authority.%0 // 02394.001 
//                                                                                        // 02395.001 
  MF_E_OPL_NOT_SUPPORTED                = _HRESULT_TYPEDEF_($C00D715A);                   // 02396.001#define MF_E_OPL_NOT_SUPPORTED           _HRESULT_TYPEDEF_(0xC00D715AL)
                                                                                          // 02397.001 
//                                                                                        // 02398.001 
// MessageId: MF_E_TOPOLOGY_VERIFICATION_FAILED                                           // 02399.001 
//                                                                                        // 02400.001 
// MessageText:                                                                           // 02401.001 
//                                                                                        // 02402.001 
// The topology could not be successfully verified.%0                                     // 02403.001 
//                                                                                        // 02404.001 
  MF_E_TOPOLOGY_VERIFICATION_FAILED     = _HRESULT_TYPEDEF_($C00D715B);                   // 02405.001#define MF_E_TOPOLOGY_VERIFICATION_FAILED _HRESULT_TYPEDEF_(0xC00D715BL)
                                                                                          // 02406.001 
//                                                                                        // 02407.001 
// MessageId: MF_E_SIGNATURE_VERIFICATION_FAILED                                          // 02408.001 
//                                                                                        // 02409.001 
// MessageText:                                                                           // 02410.001 
//                                                                                        // 02411.001 
// Signature verification could not be completed successfully for this component.%0       // 02412.001 
//                                                                                        // 02413.001 
  MF_E_SIGNATURE_VERIFICATION_FAILED    = _HRESULT_TYPEDEF_($C00D715C);                   // 02414.001#define MF_E_SIGNATURE_VERIFICATION_FAILED _HRESULT_TYPEDEF_(0xC00D715CL)
                                                                                          // 02415.001 
//                                                                                        // 02416.001 
// MessageId: MF_E_DEBUGGING_NOT_ALLOWED                                                  // 02417.001 
//                                                                                        // 02418.001 
// MessageText:                                                                           // 02419.001 
//                                                                                        // 02420.001 
// Running this process under a debugger while using protected content is not allowed.%0  // 02421.001 
//                                                                                        // 02422.001 
  MF_E_DEBUGGING_NOT_ALLOWED            = _HRESULT_TYPEDEF_($C00D715D);                   // 02423.001#define MF_E_DEBUGGING_NOT_ALLOWED       _HRESULT_TYPEDEF_(0xC00D715DL)
                                                                                          // 02424.001 
//                                                                                        // 02425.001 
// MessageId: MF_E_CODE_EXPIRED                                                           // 02426.001 
//                                                                                        // 02427.001 
// MessageText:                                                                           // 02428.001 
//                                                                                        // 02429.001 
// MF component has expired.%0                                                            // 02430.001 
//                                                                                        // 02431.001 
  MF_E_CODE_EXPIRED                     = _HRESULT_TYPEDEF_($C00D715E);                   // 02432.001#define MF_E_CODE_EXPIRED                _HRESULT_TYPEDEF_(0xC00D715EL)
                                                                                          // 02433.001 
//                                                                                        // 02434.001 
// MessageId: MF_E_GRL_VERSION_TOO_LOW                                                    // 02435.001 
//                                                                                        // 02436.001 
// MessageText:                                                                           // 02437.001 
//                                                                                        // 02438.001 
// The current GRL on the machine does not meet the minimum version requirements.%0       // 02439.001 
//                                                                                        // 02440.001 
  MF_E_GRL_VERSION_TOO_LOW              = _HRESULT_TYPEDEF_($C00D715F);                   // 02441.001#define MF_E_GRL_VERSION_TOO_LOW         _HRESULT_TYPEDEF_(0xC00D715FL)
                                                                                          // 02442.001 
//                                                                                        // 02443.001 
// MessageId: MF_E_GRL_RENEWAL_NOT_FOUND                                                  // 02444.001 
//                                                                                        // 02445.001 
// MessageText:                                                                           // 02446.001 
//                                                                                        // 02447.001 
// The current GRL on the machine does not contain any renewal entries for the specified revocation.%0 // 02448.001 
//                                                                                        // 02449.001 
  MF_E_GRL_RENEWAL_NOT_FOUND            = _HRESULT_TYPEDEF_($C00D7160);                   // 02450.001#define MF_E_GRL_RENEWAL_NOT_FOUND       _HRESULT_TYPEDEF_(0xC00D7160L)
                                                                                          // 02451.001 
//                                                                                        // 02452.001 
// MessageId: MF_E_GRL_EXTENSIBLE_ENTRY_NOT_FOUND                                         // 02453.001 
//                                                                                        // 02454.001 
// MessageText:                                                                           // 02455.001 
//                                                                                        // 02456.001 
// The current GRL on the machine does not contain any extensible entries for the specified extension GUID.%0 // 02457.001 
//                                                                                        // 02458.001 
  MF_E_GRL_EXTENSIBLE_ENTRY_NOT_FOUND   = _HRESULT_TYPEDEF_($C00D7161);                   // 02459.001#define MF_E_GRL_EXTENSIBLE_ENTRY_NOT_FOUND _HRESULT_TYPEDEF_(0xC00D7161L)
                                                                                          // 02460.001 
//                                                                                        // 02461.001 
// MessageId: MF_E_KERNEL_UNTRUSTED                                                       // 02462.001 
//                                                                                        // 02463.001 
// MessageText:                                                                           // 02464.001 
//                                                                                        // 02465.001 
// The kernel isn't secure for high security level content.%0                             // 02466.001 
//                                                                                        // 02467.001 
  MF_E_KERNEL_UNTRUSTED                 = _HRESULT_TYPEDEF_($C00D7162);                   // 02468.001#define MF_E_KERNEL_UNTRUSTED            _HRESULT_TYPEDEF_(0xC00D7162L)
                                                                                          // 02469.001 
//                                                                                        // 02470.001 
// MessageId: MF_E_PEAUTH_UNTRUSTED                                                       // 02471.001 
//                                                                                        // 02472.001 
// MessageText:                                                                           // 02473.001 
//                                                                                        // 02474.001 
// The response from protected environment driver isn't valid.%0                          // 02475.001 
//                                                                                        // 02476.001 
  MF_E_PEAUTH_UNTRUSTED                 = _HRESULT_TYPEDEF_($C00D7163);                   // 02477.001#define MF_E_PEAUTH_UNTRUSTED            _HRESULT_TYPEDEF_(0xC00D7163L)
                                                                                          // 02478.001 
//                                                                                        // 02479.001 
// MessageId: MF_E_NON_PE_PROCESS                                                         // 02480.001 
//                                                                                        // 02481.001 
// MessageText:                                                                           // 02482.001 
//                                                                                        // 02483.001 
// A non-PE process tried to talk to PEAuth.%0                                            // 02484.001 
//                                                                                        // 02485.001 
  MF_E_NON_PE_PROCESS                   = _HRESULT_TYPEDEF_($C00D7165);                   // 02486.001#define MF_E_NON_PE_PROCESS              _HRESULT_TYPEDEF_(0xC00D7165L)
                                                                                          // 02487.001 
//                                                                                        // 02488.001 
// MessageId: MF_E_REBOOT_REQUIRED                                                        // 02489.001 
//                                                                                        // 02490.001 
// MessageText:                                                                           // 02491.001 
//                                                                                        // 02492.001 
// We need to reboot the machine.%0                                                       // 02493.001 
//                                                                                        // 02494.001 
  MF_E_REBOOT_REQUIRED                  = _HRESULT_TYPEDEF_($C00D7167);                   // 02495.001#define MF_E_REBOOT_REQUIRED             _HRESULT_TYPEDEF_(0xC00D7167L)
                                                                                          // 02496.001 
//                                                                                        // 02497.001 
// MessageId: MF_S_WAIT_FOR_POLICY_SET                                                    // 02498.001 
//                                                                                        // 02499.001 
// MessageText:                                                                           // 02500.001 
//                                                                                        // 02501.001 
// Protection for this stream is not guaranteed to be enforced until the MEPolicySet event is fired.%0 // 02502.001 
//                                                                                        // 02503.001 
  MF_S_WAIT_FOR_POLICY_SET              = _HRESULT_TYPEDEF_($000D7168);                   // 02504.001#define MF_S_WAIT_FOR_POLICY_SET         _HRESULT_TYPEDEF_(0x000D7168L)
                                                                                          // 02505.001 
//                                                                                        // 02506.001 
// MessageId: MF_S_VIDEO_DISABLED_WITH_UNKNOWN_SOFTWARE_OUTPUT                            // 02507.001 
//                                                                                        // 02508.001 
// MessageText:                                                                           // 02509.001 
//                                                                                        // 02510.001 
// This video stream is disabled because it is being sent to an unknown software output.%0 // 02511.001 
//                                                                                        // 02512.001 
  MF_S_VIDEO_DISABLED_WITH_UNKNOWN_SOFTWARE_OUTPUT = _HRESULT_TYPEDEF_($000D7169);        // 02513.001#define MF_S_VIDEO_DISABLED_WITH_UNKNOWN_SOFTWARE_OUTPUT _HRESULT_TYPEDEF_(0x000D7169L)
                                                                                          // 02514.001 
//                                                                                        // 02515.001 
// MessageId: MF_E_GRL_INVALID_FORMAT                                                     // 02516.001 
//                                                                                        // 02517.001 
// MessageText:                                                                           // 02518.001 
//                                                                                        // 02519.001 
// The GRL file is not correctly formed, it may have been corrupted or overwritten.%0     // 02520.001 
//                                                                                        // 02521.001 
  MF_E_GRL_INVALID_FORMAT               = _HRESULT_TYPEDEF_($C00D716A);                   // 02522.001#define MF_E_GRL_INVALID_FORMAT          _HRESULT_TYPEDEF_(0xC00D716AL)
                                                                                          // 02523.001 
//                                                                                        // 02524.001 
// MessageId: MF_E_GRL_UNRECOGNIZED_FORMAT                                                // 02525.001 
//                                                                                        // 02526.001 
// MessageText:                                                                           // 02527.001 
//                                                                                        // 02528.001 
// The GRL file is in a format newer than those recognized by this GRL Reader.%0          // 02529.001 
//                                                                                        // 02530.001 
  MF_E_GRL_UNRECOGNIZED_FORMAT          = _HRESULT_TYPEDEF_($C00D716B);                   // 02531.001#define MF_E_GRL_UNRECOGNIZED_FORMAT     _HRESULT_TYPEDEF_(0xC00D716BL)
                                                                                          // 02532.001 
//                                                                                        // 02533.001 
// MessageId: MF_E_ALL_PROCESS_RESTART_REQUIRED                                           // 02534.001 
//                                                                                        // 02535.001 
// MessageText:                                                                           // 02536.001 
//                                                                                        // 02537.001 
// The GRL was reloaded and required all processes that can run protected media to restart.%0 // 02538.001 
//                                                                                        // 02539.001 
  MF_E_ALL_PROCESS_RESTART_REQUIRED     = _HRESULT_TYPEDEF_($C00D716C);                   // 02540.001#define MF_E_ALL_PROCESS_RESTART_REQUIRED _HRESULT_TYPEDEF_(0xC00D716CL)
                                                                                          // 02541.001 
//                                                                                        // 02542.001 
// MessageId: MF_E_PROCESS_RESTART_REQUIRED                                               // 02543.001 
//                                                                                        // 02544.001 
// MessageText:                                                                           // 02545.001 
//                                                                                        // 02546.001 
// The GRL was reloaded and the current process needs to restart.%0                       // 02547.001 
//                                                                                        // 02548.001 
  MF_E_PROCESS_RESTART_REQUIRED         = _HRESULT_TYPEDEF_($C00D716D);                   // 02549.001#define MF_E_PROCESS_RESTART_REQUIRED    _HRESULT_TYPEDEF_(0xC00D716DL)
                                                                                          // 02550.001 
//                                                                                        // 02551.001 
// MessageId: MF_E_USERMODE_UNTRUSTED                                                     // 02552.001 
//                                                                                        // 02553.001 
// MessageText:                                                                           // 02554.001 
//                                                                                        // 02555.001 
// The user space is untrusted for protected content play.%0                              // 02556.001 
//                                                                                        // 02557.001 
  MF_E_USERMODE_UNTRUSTED               = _HRESULT_TYPEDEF_($C00D716E);                   // 02558.001#define MF_E_USERMODE_UNTRUSTED          _HRESULT_TYPEDEF_(0xC00D716EL)
                                                                                          // 02559.001 
//                                                                                        // 02560.001 
// MessageId: MF_E_PEAUTH_SESSION_NOT_STARTED                                             // 02561.001 
//                                                                                        // 02562.001 
// MessageText:                                                                           // 02563.001 
//                                                                                        // 02564.001 
// PEAuth communication session hasn't been started.%0                                    // 02565.001 
//                                                                                        // 02566.001 
  MF_E_PEAUTH_SESSION_NOT_STARTED       = _HRESULT_TYPEDEF_($C00D716F);                   // 02567.001#define MF_E_PEAUTH_SESSION_NOT_STARTED  _HRESULT_TYPEDEF_(0xC00D716FL)
                                                                                          // 02568.001 
//                                                                                        // 02569.001 
// MessageId: MF_E_PEAUTH_PUBLICKEY_REVOKED                                               // 02570.001 
//                                                                                        // 02571.001 
// MessageText:                                                                           // 02572.001 
//                                                                                        // 02573.001 
// PEAuth's public key is revoked.%0                                                      // 02574.001 
//                                                                                        // 02575.001 
  MF_E_PEAUTH_PUBLICKEY_REVOKED         = _HRESULT_TYPEDEF_($C00D7171);                   // 02576.001#define MF_E_PEAUTH_PUBLICKEY_REVOKED    _HRESULT_TYPEDEF_(0xC00D7171L)
                                                                                          // 02577.001 
//                                                                                        // 02578.001 
// MessageId: MF_E_GRL_ABSENT                                                             // 02579.001 
//                                                                                        // 02580.001 
// MessageText:                                                                           // 02581.001 
//                                                                                        // 02582.001 
// The GRL is absent.%0                                                                   // 02583.001 
//                                                                                        // 02584.001 
  MF_E_GRL_ABSENT                       = _HRESULT_TYPEDEF_($C00D7172);                   // 02585.001#define MF_E_GRL_ABSENT                  _HRESULT_TYPEDEF_(0xC00D7172L)
                                                                                          // 02586.001 
//                                                                                        // 02587.001 
// MessageId: MF_S_PE_TRUSTED                                                             // 02588.001 
//                                                                                        // 02589.001 
// MessageText:                                                                           // 02590.001 
//                                                                                        // 02591.001 
// The Protected Environment is trusted.%0                                                // 02592.001 
//                                                                                        // 02593.001 
  MF_S_PE_TRUSTED                       = _HRESULT_TYPEDEF_($000D7173);                   // 02594.001#define MF_S_PE_TRUSTED                  _HRESULT_TYPEDEF_(0x000D7173L)
                                                                                          // 02595.001 
//                                                                                        // 02596.001 
// MessageId: MF_E_PE_UNTRUSTED                                                           // 02597.001 
//                                                                                        // 02598.001 
// MessageText:                                                                           // 02599.001 
//                                                                                        // 02600.001 
// The Protected Environment is untrusted.%0                                              // 02601.001 
//                                                                                        // 02602.001 
  MF_E_PE_UNTRUSTED                     = _HRESULT_TYPEDEF_($C00D7174);                   // 02603.001#define MF_E_PE_UNTRUSTED                _HRESULT_TYPEDEF_(0xC00D7174L)
                                                                                          // 02604.001 
//                                                                                        // 02605.001 
// MessageId: MF_E_PEAUTH_NOT_STARTED                                                     // 02606.001 
//                                                                                        // 02607.001 
// MessageText:                                                                           // 02608.001 
//                                                                                        // 02609.001 
// The Protected Environment Authorization service (PEAUTH) has not been started.%0       // 02610.001 
//                                                                                        // 02611.001 
  MF_E_PEAUTH_NOT_STARTED               = _HRESULT_TYPEDEF_($C00D7175);                   // 02612.001#define MF_E_PEAUTH_NOT_STARTED          _HRESULT_TYPEDEF_(0xC00D7175L)
                                                                                          // 02613.001 
//                                                                                        // 02614.001 
// MessageId: MF_E_INCOMPATIBLE_SAMPLE_PROTECTION                                         // 02615.001 
//                                                                                        // 02616.001 
// MessageText:                                                                           // 02617.001 
//                                                                                        // 02618.001 
// The sample protection algorithms supported by components are not compatible.%0         // 02619.001 
//                                                                                        // 02620.001 
  MF_E_INCOMPATIBLE_SAMPLE_PROTECTION   = _HRESULT_TYPEDEF_($C00D7176);                   // 02621.001#define MF_E_INCOMPATIBLE_SAMPLE_PROTECTION _HRESULT_TYPEDEF_(0xC00D7176L)
                                                                                          // 02622.001 
//                                                                                        // 02623.001 
// MessageId: MF_E_PE_SESSIONS_MAXED                                                      // 02624.001 
//                                                                                        // 02625.001 
// MessageText:                                                                           // 02626.001 
//                                                                                        // 02627.001 
// No more protected environment sessions can be supported.%0                             // 02628.001 
//                                                                                        // 02629.001 
  MF_E_PE_SESSIONS_MAXED                = _HRESULT_TYPEDEF_($C00D7177);                   // 02630.001#define MF_E_PE_SESSIONS_MAXED           _HRESULT_TYPEDEF_(0xC00D7177L)
                                                                                          // 02631.001 
//                                                                                        // 02632.001 
// MessageId: MF_E_HIGH_SECURITY_LEVEL_CONTENT_NOT_ALLOWED                                // 02633.001 
//                                                                                        // 02634.001 
// MessageText:                                                                           // 02635.001 
//                                                                                        // 02636.001 
// WMDRM ITA does not allow protected content with high security level for this release.%0 // 02637.001 
//                                                                                        // 02638.001 
  MF_E_HIGH_SECURITY_LEVEL_CONTENT_NOT_ALLOWED = _HRESULT_TYPEDEF_($C00D7178);            // 02639.001#define MF_E_HIGH_SECURITY_LEVEL_CONTENT_NOT_ALLOWED _HRESULT_TYPEDEF_(0xC00D7178L)
                                                                                          // 02640.001 
//                                                                                        // 02641.001 
// MessageId: MF_E_TEST_SIGNED_COMPONENTS_NOT_ALLOWED                                     // 02642.001 
//                                                                                        // 02643.001 
// MessageText:                                                                           // 02644.001 
//                                                                                        // 02645.001 
// WMDRM ITA cannot allow the requested action for the content as one or more components is not properly signed.%0 // 02646.001 
//                                                                                        // 02647.001 
  MF_E_TEST_SIGNED_COMPONENTS_NOT_ALLOWED = _HRESULT_TYPEDEF_($C00D7179);                 // 02648.001#define MF_E_TEST_SIGNED_COMPONENTS_NOT_ALLOWED _HRESULT_TYPEDEF_(0xC00D7179L)
                                                                                          // 02649.001 
//                                                                                        // 02650.001 
// MessageId: MF_E_ITA_UNSUPPORTED_ACTION                                                 // 02651.001 
//                                                                                        // 02652.001 
// MessageText:                                                                           // 02653.001 
//                                                                                        // 02654.001 
// WMDRM ITA does not support the requested action.%0                                     // 02655.001 
//                                                                                        // 02656.001 
  MF_E_ITA_UNSUPPORTED_ACTION           = _HRESULT_TYPEDEF_($C00D717A);                   // 02657.001#define MF_E_ITA_UNSUPPORTED_ACTION      _HRESULT_TYPEDEF_(0xC00D717AL)
                                                                                          // 02658.001 
//                                                                                        // 02659.001 
// MessageId: MF_E_ITA_ERROR_PARSING_SAP_PARAMETERS                                       // 02660.001 
//                                                                                        // 02661.001 
// MessageText:                                                                           // 02662.001 
//                                                                                        // 02663.001 
// WMDRM ITA encountered an error in parsing the Secure Audio Path parameters.%0          // 02664.001 
//                                                                                        // 02665.001 
  MF_E_ITA_ERROR_PARSING_SAP_PARAMETERS = _HRESULT_TYPEDEF_($C00D717B);                   // 02666.001#define MF_E_ITA_ERROR_PARSING_SAP_PARAMETERS _HRESULT_TYPEDEF_(0xC00D717BL)
                                                                                          // 02667.001 
//                                                                                        // 02668.001 
// MessageId: MF_E_POLICY_MGR_ACTION_OUTOFBOUNDS                                          // 02669.001 
//                                                                                        // 02670.001 
// MessageText:                                                                           // 02671.001 
//                                                                                        // 02672.001 
// The Policy Manager action passed in is invalid.%0                                      // 02673.001 
//                                                                                        // 02674.001 
  MF_E_POLICY_MGR_ACTION_OUTOFBOUNDS    = _HRESULT_TYPEDEF_($C00D717C);                   // 02675.001#define MF_E_POLICY_MGR_ACTION_OUTOFBOUNDS _HRESULT_TYPEDEF_(0xC00D717CL)
                                                                                          // 02676.001 
//                                                                                        // 02677.001 
// MessageId: MF_E_BAD_OPL_STRUCTURE_FORMAT                                               // 02678.001 
//                                                                                        // 02679.001 
// MessageText:                                                                           // 02680.001 
//                                                                                        // 02681.001 
// The structure specifying Output Protection Level is not the correct format.%0          // 02682.001 
//                                                                                        // 02683.001 
  MF_E_BAD_OPL_STRUCTURE_FORMAT         = _HRESULT_TYPEDEF_($C00D717D);                   // 02684.001#define MF_E_BAD_OPL_STRUCTURE_FORMAT    _HRESULT_TYPEDEF_(0xC00D717DL)
                                                                                          // 02685.001 
//                                                                                        // 02686.001 
// MessageId: MF_E_ITA_UNRECOGNIZED_ANALOG_VIDEO_PROTECTION_GUID                          // 02687.001 
//                                                                                        // 02688.001 
// MessageText:                                                                           // 02689.001 
//                                                                                        // 02690.001 
// WMDRM ITA does not recognize the Explicite Analog Video Output Protection guid specified in the license.%0 // 02691.001 
//                                                                                        // 02692.001 
  MF_E_ITA_UNRECOGNIZED_ANALOG_VIDEO_PROTECTION_GUID = _HRESULT_TYPEDEF_($C00D717E);      // 02693.001#define MF_E_ITA_UNRECOGNIZED_ANALOG_VIDEO_PROTECTION_GUID _HRESULT_TYPEDEF_(0xC00D717EL)
                                                                                          // 02694.001 
//                                                                                        // 02695.001 
// MessageId: MF_E_NO_PMP_HOST                                                            // 02696.001 
//                                                                                        // 02697.001 
// MessageText:                                                                           // 02698.001 
//                                                                                        // 02699.001 
// IMFPMPHost object not available.%0                                                     // 02700.001 
//                                                                                        // 02701.001 
  MF_E_NO_PMP_HOST                      = _HRESULT_TYPEDEF_($C00D717F);                   // 02702.001#define MF_E_NO_PMP_HOST                 _HRESULT_TYPEDEF_(0xC00D717FL)
                                                                                          // 02703.001 
//                                                                                        // 02704.001 
// MessageId: MF_E_ITA_OPL_DATA_NOT_INITIALIZED                                           // 02705.001 
//                                                                                        // 02706.001 
// MessageText:                                                                           // 02707.001 
//                                                                                        // 02708.001 
// WMDRM ITA could not initialize the Output Protection Level data.%0                     // 02709.001 
//                                                                                        // 02710.001 
  MF_E_ITA_OPL_DATA_NOT_INITIALIZED     = _HRESULT_TYPEDEF_($C00D7180);                   // 02711.001#define MF_E_ITA_OPL_DATA_NOT_INITIALIZED _HRESULT_TYPEDEF_(0xC00D7180L)
                                                                                          // 02712.001 
//                                                                                        // 02713.001 
// MessageId: MF_E_ITA_UNRECOGNIZED_ANALOG_VIDEO_OUTPUT                                   // 02714.001 
//                                                                                        // 02715.001 
// MessageText:                                                                           // 02716.001 
//                                                                                        // 02717.001 
// WMDRM ITA does not recognize the Analog Video Output specified by the OTA.%0           // 02718.001 
//                                                                                        // 02719.001 
  MF_E_ITA_UNRECOGNIZED_ANALOG_VIDEO_OUTPUT = _HRESULT_TYPEDEF_($C00D7181);               // 02720.001#define MF_E_ITA_UNRECOGNIZED_ANALOG_VIDEO_OUTPUT _HRESULT_TYPEDEF_(0xC00D7181L)
                                                                                          // 02721.001 
//                                                                                        // 02722.001 
// MessageId: MF_E_ITA_UNRECOGNIZED_DIGITAL_VIDEO_OUTPUT                                  // 02723.001 
//                                                                                        // 02724.001 
// MessageText:                                                                           // 02725.001 
//                                                                                        // 02726.001 
// WMDRM ITA does not recognize the Digital Video Output specified by the OTA.%0          // 02727.001 
//                                                                                        // 02728.001 
  MF_E_ITA_UNRECOGNIZED_DIGITAL_VIDEO_OUTPUT = _HRESULT_TYPEDEF_($C00D7182);              // 02729.001#define MF_E_ITA_UNRECOGNIZED_DIGITAL_VIDEO_OUTPUT _HRESULT_TYPEDEF_(0xC00D7182L)
                                                                                          // 02730.001 
                                                                                          // 02731.001 
//////////////////////////////////////////////////////////////////////////////            // 02732.001 
//                                                                                        // 02733.001 
// Clock errors                                                                           // 02734.001 
//                                                                                        // 02735.001 
//////////////////////////////////////////////////////////////////////////////            // 02736.001 
                                                                                          // 02737.001 
//                                                                                        // 02738.001 
// MessageId: MF_E_CLOCK_INVALID_CONTINUITY_KEY                                           // 02739.001 
//                                                                                        // 02740.001 
// MessageText:                                                                           // 02741.001 
//                                                                                        // 02742.001 
// The continuity key supplied is not currently valid.%0                                  // 02743.001 
//                                                                                        // 02744.001 
  MF_E_CLOCK_INVALID_CONTINUITY_KEY     = _HRESULT_TYPEDEF_($C00D9C40);                   // 02745.001#define MF_E_CLOCK_INVALID_CONTINUITY_KEY _HRESULT_TYPEDEF_(0xC00D9C40L)
                                                                                          // 02746.001 
//                                                                                        // 02747.001 
// MessageId: MF_E_CLOCK_NO_TIME_SOURCE                                                   // 02748.001 
//                                                                                        // 02749.001 
// MessageText:                                                                           // 02750.001 
//                                                                                        // 02751.001 
// No Presentation Time Source has been specified.%0                                      // 02752.001 
//                                                                                        // 02753.001 
  MF_E_CLOCK_NO_TIME_SOURCE             = _HRESULT_TYPEDEF_($C00D9C41);                   // 02754.001#define MF_E_CLOCK_NO_TIME_SOURCE        _HRESULT_TYPEDEF_(0xC00D9C41L)
                                                                                          // 02755.001 
//                                                                                        // 02756.001 
// MessageId: MF_E_CLOCK_STATE_ALREADY_SET                                                // 02757.001 
//                                                                                        // 02758.001 
// MessageText:                                                                           // 02759.001 
//                                                                                        // 02760.001 
// The clock is already in the requested state.%0                                         // 02761.001 
//                                                                                        // 02762.001 
  MF_E_CLOCK_STATE_ALREADY_SET          = _HRESULT_TYPEDEF_($C00D9C42);                   // 02763.001#define MF_E_CLOCK_STATE_ALREADY_SET     _HRESULT_TYPEDEF_(0xC00D9C42L)
                                                                                          // 02764.001 
//                                                                                        // 02765.001 
// MessageId: MF_E_CLOCK_NOT_SIMPLE                                                       // 02766.001 
//                                                                                        // 02767.001 
// MessageText:                                                                           // 02768.001 
//                                                                                        // 02769.001 
// The clock has too many advanced features to carry out the request.%0                   // 02770.001 
//                                                                                        // 02771.001 
  MF_E_CLOCK_NOT_SIMPLE                 = _HRESULT_TYPEDEF_($C00D9C43);                   // 02772.001#define MF_E_CLOCK_NOT_SIMPLE            _HRESULT_TYPEDEF_(0xC00D9C43L)
                                                                                          // 02773.001 
//                                                                                        // 02774.001 
// MessageId: MF_S_CLOCK_STOPPED                                                          // 02775.001 
//                                                                                        // 02776.001 
// MessageText:                                                                           // 02777.001 
//                                                                                        // 02778.001 
// Timer::SetTimer returns this success code if called happened while timer is stopped. Timer is not going to be dispatched until clock is running%0 // 02779.001 
//                                                                                        // 02780.001 
  MF_S_CLOCK_STOPPED                    = _HRESULT_TYPEDEF_($000D9C44);                   // 02781.001#define MF_S_CLOCK_STOPPED               _HRESULT_TYPEDEF_(0x000D9C44L)
                                                                                          // 02782.001 
                                                                                          // 02783.001 
//////////////////////////////////////////////////////////////////////////////            // 02784.001 
//                                                                                        // 02785.001 
// MF Quality Management errors                                                           // 02786.001 
//                                                                                        // 02787.001 
//////////////////////////////////////////////////////////////////////////////            // 02788.001 
                                                                                          // 02789.001 
//                                                                                        // 02790.001 
// MessageId: MF_E_NO_MORE_DROP_MODES                                                     // 02791.001 
//                                                                                        // 02792.001 
// MessageText:                                                                           // 02793.001 
//                                                                                        // 02794.001 
// The component does not support any more drop modes.%0                                  // 02795.001 
//                                                                                        // 02796.001 
  MF_E_NO_MORE_DROP_MODES               = _HRESULT_TYPEDEF_($C00DA028);                   // 02797.001#define MF_E_NO_MORE_DROP_MODES          _HRESULT_TYPEDEF_(0xC00DA028L)
                                                                                          // 02798.001 
//                                                                                        // 02799.001 
// MessageId: MF_E_NO_MORE_QUALITY_LEVELS                                                 // 02800.001 
//                                                                                        // 02801.001 
// MessageText:                                                                           // 02802.001 
//                                                                                        // 02803.001 
// The component does not support any more quality levels.%0                              // 02804.001 
//                                                                                        // 02805.001 
  MF_E_NO_MORE_QUALITY_LEVELS           = _HRESULT_TYPEDEF_($C00DA029);                   // 02806.001#define MF_E_NO_MORE_QUALITY_LEVELS      _HRESULT_TYPEDEF_(0xC00DA029L)
                                                                                          // 02807.001 
//                                                                                        // 02808.001 
// MessageId: MF_E_DROPTIME_NOT_SUPPORTED                                                 // 02809.001 
//                                                                                        // 02810.001 
// MessageText:                                                                           // 02811.001 
//                                                                                        // 02812.001 
// The component does not support drop time functionality.%0                              // 02813.001 
//                                                                                        // 02814.001 
  MF_E_DROPTIME_NOT_SUPPORTED           = _HRESULT_TYPEDEF_($C00DA02A);                   // 02815.001#define MF_E_DROPTIME_NOT_SUPPORTED      _HRESULT_TYPEDEF_(0xC00DA02AL)
                                                                                          // 02816.001 
//                                                                                        // 02817.001 
// MessageId: MF_E_QUALITYKNOB_WAIT_LONGER                                                // 02818.001 
//                                                                                        // 02819.001 
// MessageText:                                                                           // 02820.001 
//                                                                                        // 02821.001 
// Quality Manager needs to wait longer before bumping the Quality Level up.%0            // 02822.001 
//                                                                                        // 02823.001 
  MF_E_QUALITYKNOB_WAIT_LONGER          = _HRESULT_TYPEDEF_($C00DA02B);                   // 02824.001#define MF_E_QUALITYKNOB_WAIT_LONGER     _HRESULT_TYPEDEF_(0xC00DA02BL)
                                                                                          // 02825.001 
//                                                                                        // 02826.001 
// MessageId: MF_E_QM_INVALIDSTATE                                                        // 02827.001 
//                                                                                        // 02828.001 
// MessageText:                                                                           // 02829.001 
//                                                                                        // 02830.001 
// Quality Manager is in an invalid state. Quality Management is off at this moment.%0    // 02831.001 
//                                                                                        // 02832.001 
  MF_E_QM_INVALIDSTATE                  = _HRESULT_TYPEDEF_($C00DA02C);                   // 02833.001#define MF_E_QM_INVALIDSTATE             _HRESULT_TYPEDEF_(0xC00DA02CL)
                                                                                          // 02834.001 
                                                                                          // 02835.001 
//////////////////////////////////////////////////////////////////////////////            // 02836.001 
//                                                                                        // 02837.001 
// MF Transcode errors                                                                    // 02838.001 
//                                                                                        // 02839.001 
//////////////////////////////////////////////////////////////////////////////            // 02840.001 
                                                                                          // 02841.001 
//                                                                                        // 02842.001 
// MessageId: MF_E_TRANSCODE_NO_CONTAINERTYPE                                             // 02843.001 
//                                                                                        // 02844.001 
// MessageText:                                                                           // 02845.001 
//                                                                                        // 02846.001 
// No transcode output container type is specified.%0                                     // 02847.001 
//                                                                                        // 02848.001 
  MF_E_TRANSCODE_NO_CONTAINERTYPE       = _HRESULT_TYPEDEF_($C00DA410);                   // 02849.001#define MF_E_TRANSCODE_NO_CONTAINERTYPE  _HRESULT_TYPEDEF_(0xC00DA410L)
                                                                                          // 02850.001 
//                                                                                        // 02851.001 
// MessageId: MF_E_TRANSCODE_PROFILE_NO_MATCHING_STREAMS                                  // 02852.001 
//                                                                                        // 02853.001 
// MessageText:                                                                           // 02854.001 
//                                                                                        // 02855.001 
// The profile does not have a media type configuration for any selected source streams.%0 // 02856.001 
//                                                                                        // 02857.001 
  MF_E_TRANSCODE_PROFILE_NO_MATCHING_STREAMS = _HRESULT_TYPEDEF_($C00DA411);              // 02858.001#define MF_E_TRANSCODE_PROFILE_NO_MATCHING_STREAMS _HRESULT_TYPEDEF_(0xC00DA411L)
                                                                                          // 02859.001 
//                                                                                        // 02860.001 
// MessageId: MF_E_TRANSCODE_NO_MATCHING_ENCODER                                          // 02861.001 
//                                                                                        // 02862.001 
// MessageText:                                                                           // 02863.001 
//                                                                                        // 02864.001 
// Cannot find an encoder MFT that accepts the user preferred output type.%0              // 02865.001 
//                                                                                        // 02866.001 
  MF_E_TRANSCODE_NO_MATCHING_ENCODER    = _HRESULT_TYPEDEF_($C00DA412);                   // 02867.001#define MF_E_TRANSCODE_NO_MATCHING_ENCODER _HRESULT_TYPEDEF_(0xC00DA412L)
                                                                                          // 02868.001 
                                                                                          // 02869.001 
//////////////////////////////////////////////////////////////////////////////            // 02870.001 
//                                                                                        // 02871.001 
// MF HW Device Proxy errors                                                              // 02872.001 
//                                                                                        // 02873.001 
//////////////////////////////////////////////////////////////////////////////            // 02874.001 
                                                                                          // 02875.001 
//                                                                                        // 02876.001 
// MessageId: MF_E_ALLOCATOR_NOT_INITIALIZED                                              // 02877.001 
//                                                                                        // 02878.001 
// MessageText:                                                                           // 02879.001 
//                                                                                        // 02880.001 
// Memory allocator is not initialized.%0                                                 // 02881.001 
//                                                                                        // 02882.001 
  MF_E_ALLOCATOR_NOT_INITIALIZED        = _HRESULT_TYPEDEF_($C00DA7F8);                   // 02883.001#define MF_E_ALLOCATOR_NOT_INITIALIZED   _HRESULT_TYPEDEF_(0xC00DA7F8L)
                                                                                          // 02884.001 
//                                                                                        // 02885.001 
// MessageId: MF_E_ALLOCATOR_NOT_COMMITED                                                 // 02886.001 
//                                                                                        // 02887.001 
// MessageText:                                                                           // 02888.001 
//                                                                                        // 02889.001 
// Memory allocator is not committed yet.%0                                               // 02890.001 
//                                                                                        // 02891.001 
  MF_E_ALLOCATOR_NOT_COMMITED           = _HRESULT_TYPEDEF_($C00DA7F9);                   // 02892.001#define MF_E_ALLOCATOR_NOT_COMMITED      _HRESULT_TYPEDEF_(0xC00DA7F9L)
                                                                                          // 02893.001 
//                                                                                        // 02894.001 
// MessageId: MF_E_ALLOCATOR_ALREADY_COMMITED                                             // 02895.001 
//                                                                                        // 02896.001 
// MessageText:                                                                           // 02897.001 
//                                                                                        // 02898.001 
// Memory allocator has already been committed.%0                                         // 02899.001 
//                                                                                        // 02900.001 
  MF_E_ALLOCATOR_ALREADY_COMMITED       = _HRESULT_TYPEDEF_($C00DA7FA);                   // 02901.001#define MF_E_ALLOCATOR_ALREADY_COMMITED  _HRESULT_TYPEDEF_(0xC00DA7FAL)
                                                                                          // 02902.001 
//                                                                                        // 02903.001 
// MessageId: MF_E_STREAM_ERROR                                                           // 02904.001 
//                                                                                        // 02905.001 
// MessageText:                                                                           // 02906.001 
//                                                                                        // 02907.001 
// An error occurred in media stream.%0                                                   // 02908.001 
//                                                                                        // 02909.001 
  MF_E_STREAM_ERROR                     = _HRESULT_TYPEDEF_($C00DA7FB);                   // 02910.001#define MF_E_STREAM_ERROR                _HRESULT_TYPEDEF_(0xC00DA7FBL)
                                                                                          // 02911.001 
//                                                                                        // 02912.001 
// MessageId: MF_E_INVALID_STREAM_STATE                                                   // 02913.001 
//                                                                                        // 02914.001 
// MessageText:                                                                           // 02915.001 
//                                                                                        // 02916.001 
// Stream is not in a state to handle the request.%0                                      // 02917.001 
//                                                                                        // 02918.001 
  MF_E_INVALID_STREAM_STATE             = _HRESULT_TYPEDEF_($C00DA7FC);                   // 02919.001#define MF_E_INVALID_STREAM_STATE        _HRESULT_TYPEDEF_(0xC00DA7FCL)
                                                                                          // 02920.001 
//                                                                                        // 02921.001 
// MessageId: MF_E_HW_STREAM_NOT_CONNECTED                                                // 02922.001 
//                                                                                        // 02923.001 
// MessageText:                                                                           // 02924.001 
//                                                                                        // 02925.001 
// Hardware stream is not connected yet.%0                                                // 02926.001 
//                                                                                        // 02927.001 
  MF_E_HW_STREAM_NOT_CONNECTED          = _HRESULT_TYPEDEF_($C00DA7FD);                   // 02928.001#define MF_E_HW_STREAM_NOT_CONNECTED     _HRESULT_TYPEDEF_(0xC00DA7FDL)
                                                                                          // 02929.001 
                                                                                          // 02930.001#endif // _MFERROR_H
                                                                                          // 02931.001 
                                                                                          // 02932.001 

implementation

//--------------------- Helper functions -------------------------------------

{-----------------------------------------------------------------------------
  STATUS_SEVERITY       This macro will return the severity code from the
                        HR value
-----------------------------------------------------------------------------}
function STATUS_SEVERITY(hr: HRESULT): LongWord;
Begin
  Result:=(hr shr 30) and $3;
End;


end.
