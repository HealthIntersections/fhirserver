Unit dicom_jpegls;

//
// A simplified interface to CharLS JPEG-lossless library in C
//
// https://github.com/team-charls/charls
//
{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$i fhir.inc}

Interface

Uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils,
  dicom_dictionary;


Type
  PCardinal = ^Cardinal;

  TJpegLSDecode = function (Response : Pointer; ResponseLength : Cardinal; pResultCode: PCardinal; Data : Pointer; Length : Cardinal;
                            Width: Cardinal; Height: Cardinal; Cbit: Cardinal; Ccomp: Cardinal) : Boolean; stdcall;
  TJpegLSEncode = function (Response : Pointer; pResponseLength : PCardinal; pResultCode: PCardinal; Data : Pointer; Length : Cardinal;
                            Width: Cardinal; Height: Cardinal; Cbit: Cardinal; Ccomp: Cardinal; allowedlossyerror : Cardinal) : Boolean; stdcall;

Var
  JpegLSDecode : TJpegLSDecode;
  JpegLSEncode : TJpegLSEncode;

Procedure LoadDll;
Procedure UnloadDll;

Implementation

Var
  GHandle : HModule;

Function DummyJpegLSDecode(Response : Pointer;  ResponseLength : Integer; pResultCode: PCardinal; Data : Pointer; Length : Integer; Width: Integer; Height: Integer; Cbit: Integer; Ccomp: Integer) : Boolean; stdcall;
Begin
  raise EDicomException.create('Unable to decode this image as the jpeg-ls library was not found');
End;

Function DummyJpegLSEncode(Response : Pointer; pResponseLength : PCardinal; pResultCode: PCardinal; Data : Pointer; Length : Cardinal; Width: Cardinal; Height: Cardinal; Cbit: Cardinal; Ccomp: Cardinal; allowedlossyerror : Cardinal) : Boolean; stdcall;
Begin
  raise EDicomException.create('Unable to decode this image as the jpeg-ls library was not found');
End;


Procedure LoadDll;
begin
  GHandle := LoadLibrary(pchar(ExtractFilePath(ParamStr(0))+'\jpegls_lib.dll'));
  If GHandle <> 0 Then
  Begin
    @JpegLSDecode := GetProcAddress(GHandle, 'jpegLsDecode');
    @JpegLSEncode := GetProcAddress(GHandle, 'jpegLsEncode');
  End
  Else
  Begin
    @JpegLSDecode := @DummyJpegLSDecode;
    @JpegLSEncode := @DummyJpegLSEncode;
  End;
End;


Procedure UnloadDll;
Begin
  If GHandle <> 0 Then
    freelibrary(GHandle);
End;

End.
