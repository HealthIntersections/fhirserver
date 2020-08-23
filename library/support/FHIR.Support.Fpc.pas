unit FHIR.Support.Fpc;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$IFDEF FPC}
{$mode objfpc}{$H+}

{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH TYPEHELPERS}
{$ENDIF}

interface

{$IFDEF FPC}
uses
  Classes, SysUtils, Character, RegExpr, FileUtil, Generics.Collections, Graphics, ZLib;

type

  { TCharHelper }

  TCharHelper = type helper for char
  public
    function isDigit : boolean;
    function IsNumber : boolean;
    function isUpper : boolean;
    function IsWhiteSpace : boolean;
  end;

  { TTimeZone }
  TTimeSpan = record
    TotalDays : Double;
  end;

  TTimeZone = class
  private
    class var FLocal: TTimeZone;
  public
    function GetUtcOffset(const ADateTime: TDateTime; const ForceDaylight: Boolean = False): TTimeSpan; inline;
    function ToLocalTime(const ADateTime: TDateTime): TDateTime;
    function ToUniversalTime(const ADateTime: TDateTime; const ForceDaylight: Boolean = False): TDateTime; inline;
    class property Local: TTimeZone read FLocal;
  end;

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;

function ColorToString(Color: TColor): AnsiString;

type
  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

const
    ZLevels: array[TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
    );

procedure ZCompress(const inBuffer: TBytes; out outBuffer: TBytes; level: TZCompressionLevel = zcDefault);
procedure ZDecompress(const inBuffer: TBytes; out outBuffer: TBytes; outEstimate: Integer = 0);

{$ENDIF}


implementation

{$IFDEF FPC}

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
begin
  result := FileUtil.DeleteDirectory(DirectoryName, OnlyChildren);
end;

{ TTimeZone }

function TTimeZone.GetUtcOffset(const ADateTime: TDateTime;
  const ForceDaylight: Boolean): TTimeSpan;
begin

end;

function TTimeZone.ToLocalTime(const ADateTime: TDateTime): TDateTime;
begin

end;

function TTimeZone.ToUniversalTime(const ADateTime: TDateTime;
  const ForceDaylight: Boolean): TDateTime;
begin

end;

{ TCharHelper }

function TCharHelper.isDigit: boolean;
begin
  result := Character.isDigit(self);
end;

function TCharHelper.IsNumber: boolean;
begin
  result := Character.isDigit(self);
end;

function TCharHelper.isUpper : boolean;
begin
  result := Character.IsUpper(self);
end;

function TCharHelper.IsWhiteSpace: boolean;
begin
  result := Character.IsWhiteSpace(self);
end;

function ColorToString(Color: TColor): AnsiString;
begin
  result := Graphics.ColorToString(Color);
end;

function ZCompressCheck(code: Integer; bufOk : boolean): Integer; overload;
begin
  Result := code;

  if code < 0 then
    case (code) of
     Z_NEED_DICT : raise Exception.Create('Dictionary Needed');
     Z_STREAM_END : raise Exception.Create('Input Ended Unexpectedly');
     Z_OK : ;
     Z_ERRNO : raise Exception.Create('Error in File');
     Z_STREAM_ERROR : raise Exception.Create('Error in Stream');
     Z_DATA_ERROR : raise Exception.Create('Error in Data');
     Z_MEM_ERROR : raise Exception.Create('Memory Error');
     Z_BUF_ERROR : if (not bufOk) then raise Exception.Create('Buffer Error');
     Z_VERSION_ERROR : raise Exception.Create('Version Error');
    end;
end;


procedure ZCompress(const inBuffer: TBytes; out outBuffer: TBytes; level: TZCompressionLevel);
const
  delta = 256;
var
  zstream: TZStreamRec;
  outSize,inSize: Integer;
begin
  zstream := Default(TZStreamRec);
  inSize := Length(inBuffer);
  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  SetLength(outBuffer, outSize);

  try
    zstream.next_in := @inBuffer[0];
    zstream.avail_in := inSize;
    zstream.next_out := @outBuffer[0];
    zstream.avail_out := outSize;

    ZCompressCheck(DeflateInit(zstream, ZLevels[level]), false);

    try
      while ZCompressCheck(deflate(zstream, Z_FINISH), false) <> Z_STREAM_END do
      begin
        Inc(outSize, delta);
        SetLength(outBuffer, outSize);
        zstream.next_out := @outBuffer[0];
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream), false);
    end;

    SetLength(outBuffer,zstream.total_out);

  except
    SetLength(outBuffer,0);
    raise;
  end;
end;


procedure ZDecompress(const inBuffer: TBytes; out outBuffer: TBytes; outEstimate: Integer);
var
  zstream: TZStreamRec;
  delta, inSize, outSize: Integer;
begin
  inSize := Length(inBuffer);
  if inSize = 0 then
    ZCompressCheck(Z_BUF_ERROR, false);

  zstream := Default(TZStreamRec);
  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then
    outSize := delta
  else
    outSize := outEstimate;
  if outSize = 0 then
    outSize := 16;

  SetLength(outBuffer, outSize);

  try
    zstream.next_in := @inBuffer[0];
    zstream.avail_in := inSize;
    zstream.next_out := @outBuffer[0];
    zstream.avail_out := outSize;

    ZCompressCheck(InflateInit(zstream), false);

    try
      while ZCompressCheck(inflate(zstream, Z_NO_FLUSH), true) <> Z_STREAM_END do
      begin
        Inc(outSize, delta);
        SetLength(outBuffer, outSize);
        zstream.next_out := PBytef(@outBuffer[0]) + zstream.total_out;
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(inflateEnd(zstream), false);
    end;

    SetLength(outBuffer, zstream.total_out);

  except
    SetLength(outBuffer,0);
    raise;
  end;
end;

{$ENDIF}

end.

