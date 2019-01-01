(*

MIT License

Copyright (c) 2018 Ondrej Kelle

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*)

//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------

// NOTE: When changing this file, you may need to update the GUID in ByteCodeCacheReleaseFileVersion.h
// Please update the GUID when:
// * CHAKRA_CORE_VERSION_RELEASE is changed to 1
// * CHAKRA_CORE_VERSION_RELEASE is currently set to 1 and the bytecode changes
// See notes below about ReleaseVersioningScheme.

unit ChakraCoreVersion;

interface

// --------------
// VERSION NUMBER
// --------------

// ChakraCore version number definitions (used in ChakraCore binary metadata)
const
  CHAKRA_CORE_MAJOR_VERSION = 1;
  CHAKRA_CORE_MINOR_VERSION = 11;
  CHAKRA_CORE_PATCH_VERSION = 4;
  CHAKRA_CORE_VERSION_RELEASE_QFE = 0; // Redundant with PATCH_VERSION. Keep this value set to 0.

// -------------
// RELEASE FLAGS
// -------------

// NOTE: CHAKRA_CORE_VERSION_PRERELEASE can only be set to 1 when
// CHAKRA_CORE_VERSION_RELEASE is set to 1, or there is no effect.
// Here are the meanings of the various combinations:
//
//                  RELEASE**   PRERELEASE
// DEVELOPMENT      0           0
// <INVALID>        0           1           # INVALID but identical to DEVELOPMENT
// RELEASE**        1           0           #
// PRERELEASE       1           1
//                  ** Release flags are not related to build type (e.g. x64_release)
//
// Unless otherwise noted, the code affected by these flags lies mostly in bin/CoreCommon.ver
//
// DEVELOPMENT:
// * Uses EngineeringVersioningScheme (see lib/Runtime/ByteCode/ByteCodeSerializer.cpp)
// * Sets file flag VS_FF_PRIVATEBUILD
// * Adds "Private" to the file description
//
// RELEASE** and PRERELEASE (i.e. controlled by CHAKRA_CORE_VERSION_RELEASE flag):
// * Uses ReleaseVersioningScheme (see lib/Runtime/ByteCode/ByteCodeSerializer.cpp)
//
// PRERELEASE (preparing for release but not yet ready to release):
// * Sets file flag VS_FF_PRERELEASE
// * Adds "Pre-release" to the file description
//
// RELEASE** (code is ready to release)
// * Sets neither of the file flags noted above
// * Does not add anything to the file description

// ChakraCore RELEASE and PRERELEASE flags
  CHAKRA_CORE_VERSION_RELEASE = 1;
  CHAKRA_CORE_VERSION_PRERELEASE = 0;

// Chakra RELEASE flag
// Mostly redundant with CHAKRA_CORE_VERSION_RELEASE,
// but semantically refers to Chakra rather than ChakraCore.
  CHAKRA_VERSION_RELEASE = 0;

implementation

end.
