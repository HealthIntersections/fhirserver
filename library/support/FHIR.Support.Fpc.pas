unit FHIR.Support.Fpc;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{$I fhir.inc}

{$IFDEF FPC}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH TYPEHELPERS}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs, Character, Generics.Collections, ZLib;


{$IFNDEF FPC}
type
  UnicodeChar = char;
{$ENDIF}

// unicode helpers - make life easier for shared fpc/delphi code
function unicodeChars(s : String) : TArray<UnicodeChar>;
function strToWideString(s : String): WideString; {$IFDEF DELPHI} inline; {$ENDIF} // in delphi, this does nothing.


{$IFDEF FPC}

procedure InitializeCriticalSection(out cs : TRTLCriticalSection);
procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
function TryEnterCriticalSection(var cs : TRTLCriticalSection) : boolean;

type

  { TCharHelper }

  TCharHelper = type helper for char
  public
    function isDigit : boolean;
    function isLetter : boolean;
    function IsNumber : boolean;
    function isUpper : boolean;
    function IsWhiteSpace : boolean;
    function ToLower : char;
    function ToUpper : char;
  end;

  TShortStringHelper = type helper for ShortString
  public
    function substring(start, stop : integer) : String; overload;
    function substring(start : integer) : String; overload;
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

//  TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError, wrIOCompletion);

  { TSemaphore }

  TSemaphore = class
  private
    fMaxPermits: Cardinal;
    fPermits: Cardinal;
    fLock: TRTLCriticalSection;
    FBlockQueue: Contnrs.TQueue;
    function GetWaitCount: Cardinal;
  public
    constructor Create(MaxPermits: Cardinal); overload;
    constructor Create(SemaphoreAttributes: Pointer; AInitialCount, AMaximumCount: Integer; const Name: string); overload;
    destructor Destroy; override;

    procedure Wait;
    function WaitFor(timeout : integer) : TWaitResult;
    procedure Post;
    procedure Release;
    function Used: Boolean;
    property WaitCount: Cardinal read GetWaitCount;
    property Permits: Cardinal read fPermits;
    property MaxPermits: Cardinal read fMaxPermits;
  end;


function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
procedure FileSetReadOnly(const FileName : String; readOnly : boolean);

//function ColorToString(Color: TColor): AnsiString;

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

type

  { TZDecompressionStream }

  TZDecompressionStream = class (TStream)
  private
  public
    constructor Create(stream : TStream; level : integer);
  end;

{$ENDIF}

implementation


{$IFDEF FPC}
uses
  RegExpr, FileUtil, LazUTF8;
{$ENDIF}


{$IFDEF FPC}

function unicodeChars(s : String) : TArray<UnicodeChar>;
var
  i, c, l, cl : integer;
  ch : UnicodeChar;
  p: PChar;
begin
  l := length(s);
  SetLength(result, l); // maximum possible length
  i := 0;
  c := 1;
  p := @s[1];
  while l > 0 do
  begin
    ch := UnicodeChar(UTF8CodepointToUnicode(p, cl));
    result[i] := ch;
    inc(i);
    dec(l, cl);
    inc(p, cl);
  end;
  SetLength(result, i);
end;

function strToWideString(s : String): WideString;
var
  i, c, l, cl : integer;
  ch : UnicodeChar;
  p: PChar;
begin
  l := length(s);
  SetLength(result, l); // maximum possible length
  i := 1;
  c := 1;
  p := @s[1];
  while l > 0 do
  begin
    ch := UnicodeChar(UTF8CodepointToUnicode(p, cl));
    result[i] := ch;
    inc(i);
    dec(l, cl);
    inc(p, cl);
  end;
  SetLength(result, i);
end;

{$ELSE}

function unicodeChars(s : String) : TArray<UnicodeChar>;
var
  i : Integer;
begin
  SetLength(result, length(s));
  for i := 1 to length(s) do
    result[i-1] := s[i];
end;

function strToWideString(s : String): WideString;
begin
  result := s;
end;
{$ENDIF}

{$IFDEF FPC}

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
begin
  result := FileUtil.DeleteDirectory(DirectoryName, OnlyChildren);
end;


{ TZDecompressionStream }

constructor TZDecompressionStream.Create(stream: TStream; level: integer);
begin
  inherited create;
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

function TCharHelper.isLetter: boolean;
begin
  result := Character.isLetter(self);
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

function TCharHelper.ToLower: char;
begin
  result := Character.ToLower(self);
end;

function TCharHelper.ToUpper: char;
begin
  result := Character.ToUpper(self);
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

procedure FileSetReadOnly(const FileName : String; readOnly : boolean);
begin
  if readOnly then
    FileSetAttr(FileName, FileGetAttr(Filename) and faReadOnly)
  else
    FileSetAttr(FileName, FileGetAttr(Filename) and not faReadOnly);
end;

{ TSemaphore }

function TSemaphore.GetWaitCount: Cardinal;
begin
  EnterCriticalSection(fLock);
  try
    Result:= FBlockQueue.Count;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

procedure TSemaphore.Wait;
var
  aWait: Boolean;
  aEvent: PRTLEvent;
begin
  //writeln('Sem:');
  //writeln('  locking...');
  EnterCriticalSection(fLock);
  try
    //writeln('  locked');
    if (fPermits > 0) then begin
      Dec(fPermits);
      aWait:= False;
    end else begin
      aEvent:= RTLEventCreate;
      FBlockQueue.Push(aEvent);
      aWait:= True;
    end;
  finally
    LeaveCriticalSection(fLock);
  end;
  if aWait then begin
    //writeln('  waiting...');
    RTLeventWaitFor(aEvent);
    RTLEventDestroy(aEvent);
  end;
  //writeln('  aquired');
end;

function TSemaphore.WaitFor(timeout: integer) : TWaitResult;
begin
  wait;
  result := wrSignaled;
end;

procedure TSemaphore.Post;
begin
  EnterCriticalSection(fLock);
  try
    if FBlockQueue.Count > 0 then
      RTLEventSetEvent(PRTLEvent(FBlockQueue.Pop))
    else
      Inc(fPermits);
  finally
    LeaveCriticalSection(fLock);
  end;
end;

procedure TSemaphore.Release;
begin
  Post;
end;

function TSemaphore.Used: Boolean;
begin
  EnterCriticalSection(fLock);
  try
    Result := fPermits < fMaxPermits;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

constructor TSemaphore.Create(MaxPermits: Cardinal);
begin
  fMaxPermits := MaxPermits;
  fPermits := MaxPermits;
  InitCriticalSection(fLock);
  FBlockQueue:= TQueue.Create;
end;

constructor TSemaphore.Create(SemaphoreAttributes: Pointer; AInitialCount, AMaximumCount: Integer; const Name: string);
begin
  fMaxPermits:= AMaximumCount;
  fPermits := MaxPermits;
  InitCriticalSection(fLock);
  FBlockQueue:= TQueue.Create;
end;

destructor TSemaphore.Destroy;
begin
  DoneCriticalSection(fLock);
  FBlockQueue.Free;
  inherited Destroy;
end;

{ TShortStringHelper }

function TShortStringHelper.substring(start, stop : integer) : String;
begin
  result := copy(self, start, stop-start);
end;

function TShortStringHelper.substring(start : integer) : String;
begin
  result := copy(self, start, $FF);
end;

procedure InitializeCriticalSection(out cs : TRTLCriticalSection);
begin
  InitCriticalSection(cs);
end;

procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
begin
  DoneCriticalSection(cs);
end;

function TryEnterCriticalSection(var cs : TRTLCriticalSection) : boolean;
begin
  result := System.TryEnterCriticalSection(cs) > 0;
end;


{$ENDIF}

end.


