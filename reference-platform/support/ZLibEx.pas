{*****************************************************************************
*  ZLibEx.pas                                                                *
*                                                                            *
*  copyright (c) 2000-2002 base2 technologies                                *
*  copyright (c) 1997 Borland International                                  *
*                                                                            *
*  revision history                                                          *
*    2002.03.15  updated to zlib version 1.1.4                               *
*    2001.11.27  enhanced TZDecompressionStream.Read to adjust source        *
*                  stream position upon end of compression data              *
*                fixed endless loop in TZDecompressionStream.Read when       *
*                  destination count was greater than uncompressed data      *
*    2001.10.26  renamed unit to integrate "nicely" with delphi 6            *
*    2000.11.24  added soFromEnd condition to TZDecompressionStream.Seek     *
*                added ZCompressStream and ZDecompressStream                 *
*    2000.06.13  optimized, fixed, rewrote, and enhanced the zlib.pas unit   *
*                  included on the delphi cd (zlib version 1.1.3)            *
*                                                                            *
*  acknowledgements                                                          *
*    erik turner    Z*Stream routines                                        *
*    david bennion  finding the nastly little endless loop quirk with the    *
*                     TZDecompressionStream.Read method                      *
*    burak kalayci  informing me about the zlib 1.1.4 update                 *
*****************************************************************************}

unit ZLibEx;

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
interface

uses
  SysUtils, Classes;

const
  ZLIB_VERSION = '1.1.4';

type
  TZAlloc = function (opaque: Pointer; items, size: Integer): Pointer;
  TZFree  = procedure (opaque, block: Pointer);

  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

  {** TZStreamRec ***********************************************************}

  TZStreamRec = packed record
    next_in  : PAnsiChar;     // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far

    next_out : PAnsiChar;     // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far

    msg      : PAnsiChar;     // last error message, NULL if no error
    state    : Pointer;   // not visible by applications

    zalloc   : TZAlloc;   // used to allocate the internal state
    zfree    : TZFree;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree

    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;

  {** TCustomZStream ********************************************************}

  TCustomZStream = class(TStream)
  private
    FStream    : TStream;
    FStreamPos : Integer;
    FOnProgress: TNotifyEvent;

    FZStream   : TZStreamRec;
    FBuffer    : Array [Word] of AnsiChar;
  protected
    constructor Create(stream: TStream);

    procedure DoProgress; dynamic;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  {** TZCompressionStream ***************************************************}

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel = zcDefault);
    destructor  Destroy; override;

    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;

    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  {** TZDecompressionStream *************************************************}

  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(source: TStream);
    destructor  Destroy; override;

    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;

    property OnProgress;
  end;

{** zlib public routines ****************************************************}

function ZCompressCheck(code: Integer): Integer;
function ZDecompressCheck(code: Integer): Integer;

function DeflateInit(var stream: TZStreamRec; level: Integer): Integer;
function InflateInit(var stream: TZStreamRec): Integer;

function deflateInit_(var strm: TZStreamRec; level: Integer; version: PAnsiChar;
  recsize: Integer): Integer; 
function DeflateInit2_(var stream : TZStreamRec; level : integer; method : integer;
  windowBits : integer; memLevel : integer; strategy : integer; version : PAnsiChar;
  recsize : integer) : integer;
function deflate(var strm: TZStreamRec; flush: Integer): Integer;
function deflateEnd(var strm: TZStreamRec): Integer;

function inflateInit_(var strm: TZStreamRec; version: PAnsiChar;
  recsize: Integer): Integer;
function InflateInit2_(var stream : TZStreamRec; windowBits: integer;
  version: PAnsiChar; recsize: integer) : integer;
function inflate(var strm: TZStreamRec; flush: Integer): Integer;
function inflateEnd(var strm: TZStreamRec): Integer;
function inflateReset(var strm: TZStreamRec): Integer;

const
  {** flush constants *******************************************************}

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  {** return codes **********************************************************}

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  {** compression levels ****************************************************}

  Z_NO_COMPRESSION       =   0;
  Z_BEST_SPEED           =   1;
  Z_BEST_COMPRESSION     =   9;
  Z_DEFAULT_COMPRESSION  = (-1);

  {** compression strategies ************************************************}

  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_DEFAULT_STRATEGY    = 0;

  {** data types ************************************************************}

  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_UNKNOWN  = 2;

  {** compression methods ***************************************************}

  Z_DEFLATED = 8;

  {** return code messages **************************************************}

  _z_errmsg: array[0..9] of PChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    '',                     // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

  ZLevels: array [TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
  );

  SZInvalid = 'Invalid ZStream operation!';

{*****************************************************************************
*  ZCompress                                                                 *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer  = pointer to uncompressed data                                *
*    inSize    = size of inBuffer (bytes)                                    *
*    outBuffer = pointer (unallocated)                                       *
*    level     = compression level                                           *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to compressed data (allocated)                      *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel = zcDefault);

{*****************************************************************************
*  ZDecompress                                                               *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer    = pointer to compressed data                                *
*    inSize      = size of inBuffer (bytes)                                  *
*    outBuffer   = pointer (unallocated)                                     *
*    outEstimate = estimated size of uncompressed data (bytes)               *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to decompressed data (allocated)                    *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
 out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer = 0);

{** AnsiString routines *********************************************************}

function ZCompressStr(const s: AnsiString; level: TZCompressionLevel = zcDefault): AnsiString;

function ZDecompressStr(const s: AnsiString): AnsiString;
function ZDecompressBytes(const s: TBytes): TBytes;

{** stream routines *********************************************************}

procedure ZCompressStream(inStream, outStream: TStream;
  level: TZCompressionLevel = zcDefault);

procedure ZDecompressStream(inStream, outStream: TStream);

{****************************************************************************}

type
  EZLibError = class(Exception);

  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);

implementation

{** link zlib code **********************************************************}

{$L deflate.obj}
{$L inflate.obj}
{$L infblock.obj}
{$L inftrees.obj}
{$L infcodes.obj}
{$L infutil.obj}
{$L inffast.obj}
{$L trees.obj}
{$L adler32.obj}

{*****************************************************************************
*  note: do not reorder the above -- doing so will result in external        *
*  functions being undefined                                                 *
*****************************************************************************}

{** deflate routines ********************************************************}

function deflateInit_(var strm: TZStreamRec; level: Integer; version: PAnsiChar;
  recsize: Integer): Integer; external;

function DeflateInit2_(var stream : TZStreamRec; level : integer; method : integer;
  windowBits : integer; memLevel : integer; strategy : integer; version : PAnsiChar;
  recsize : integer) : integer; external;

function deflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;

function deflateEnd(var strm: TZStreamRec): Integer; external;

{** inflate routines ********************************************************}

function inflateInit_(var strm: TZStreamRec; version: PAnsiChar;
  recsize: Integer): Integer; external;

function InflateInit2_(var stream : TZStreamRec; windowBits: integer;
  version: PAnsiChar; recsize: integer) : integer; external;

function inflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;

function inflateEnd(var strm: TZStreamRec): Integer; external;

function inflateReset(var strm: TZStreamRec): Integer; external;

{** zlib function implementations *******************************************}

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result,items * size);
end;

procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;

{** c function implementations **********************************************}

procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
begin
  FillChar(p^,count,b);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^,dest^,count);
end;

{** custom zlib routines ****************************************************}

function DeflateInit(var stream: TZStreamRec; level: Integer): Integer;
begin
  result := DeflateInit_(stream,level,ZLIB_VERSION,SizeOf(TZStreamRec));
end;
(*
function DeflateInit2(var stream: TZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer): Integer;
begin
  result := DeflateInit2_(stream,level,method,windowBits,memLevel,
    strategy,ZLIB_VERSION,SizeOf(TZStreamRec));
end;
*)
function InflateInit(var stream: TZStreamRec): Integer;
begin
  result := InflateInit_(stream,ZLIB_VERSION,SizeOf(TZStreamRec));
end;
(*
function InflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
begin
  result := InflateInit2_(stream,windowBits,ZLIB_VERSION,SizeOf(TZStreamRec));
end;
*)
{****************************************************************************}

function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZCompressionError.Create(_z_errmsg[2 - code]);
  end;
end;

function ZDecompressCheck(code: Integer): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    raise EZDecompressionError.Create(_z_errmsg[2 - code]);
  end;
end;

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel);
const
  delta = 256;
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);

  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer,outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZCompressCheck(DeflateInit(zstream,ZLevels[level]));

    try
      while ZCompressCheck(deflate(zstream,Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(outSize,delta);
        ReallocMem(outBuffer,outSize);

        zstream.next_out := PAnsiChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;

    ReallocMem(outBuffer,zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer);
var
  zstream: TZStreamRec;
  delta  : Integer;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);

  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then outSize := delta
  else outSize := outEstimate;

  GetMem(outBuffer,outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZDecompressCheck(InflateInit(zstream));

    try
      while ZDecompressCheck(inflate(zstream,Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(outSize,delta);
        ReallocMem(outBuffer,outSize);

        zstream.next_out := PAnsiChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(inflateEnd(zstream));
    end;

    ReallocMem(outBuffer,zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

{** AnsiString routines *********************************************************}

function ZCompressStr(const s: AnsiString; level: TZCompressionLevel): AnsiString;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZCompress(PAnsiChar(s),Length(s),buffer,size,level);

  SetLength(result,size);
  Move(buffer^,result[1],size);

  FreeMem(buffer);
end;

function ZDecompressStr(const s: AnsiString): AnsiString;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress(PAnsiChar(s),Length(s),buffer,size);

  SetLength(result,size);
  Move(buffer^,result[1],size);

  FreeMem(buffer);
end;

function ZDecompressBytes(const s: TBytes): TBytes;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress(Pointer(s[0]),Length(s),buffer,size);

  SetLength(result,size);
  Move(buffer^,result[0],size);

  FreeMem(buffer);
end;

{** stream routines *********************************************************}

procedure ZCompressStream(inStream, outStream: TStream;
  level: TZCompressionLevel);
const
  bufferSize = 32768;
var
  zstream  : TZStreamRec;
  zresult  : Integer;
  inBuffer : Array [0..bufferSize-1] of AnsiChar;
  outBuffer: Array [0..bufferSize-1] of AnsiChar;
  inSize   : Integer;
  outSize  : Integer;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);

  ZCompressCheck(DeflateInit(zstream,ZLevels[level]));

  inSize := inStream.Read(inBuffer,bufferSize);

  while inSize > 0 do
  begin
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      ZCompressCheck(deflate(zstream,Z_NO_FLUSH));

      // outSize := zstream.next_out - outBuffer;
      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer,outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);

    inSize := inStream.Read(inBuffer,bufferSize);
  end;

  repeat
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(deflate(zstream,Z_FINISH));

    // outSize := zstream.next_out - outBuffer;
    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer,outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);

  ZCompressCheck(deflateEnd(zstream));
end;

procedure ZDecompressStream(inStream, outStream: TStream);
const
  bufferSize = 32768;
var
  zstream  : TZStreamRec;
  zresult  : Integer;
  inBuffer : Array [0..bufferSize-1] of AnsiChar;
  outBuffer: Array [0..bufferSize-1] of AnsiChar;
  inSize   : Integer;
  outSize  : Integer;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);

  ZCompressCheck(InflateInit(zstream));

  inSize := inStream.Read(inBuffer,bufferSize);

  while inSize > 0 do
  begin
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      ZCompressCheck(inflate(zstream,Z_NO_FLUSH));

      // outSize := zstream.next_out - outBuffer;
      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer,outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);

    inSize := inStream.Read(inBuffer,bufferSize);
  end;

  repeat
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(inflate(zstream,Z_FINISH));

    // outSize := zstream.next_out - outBuffer;
    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer,outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);

  ZCompressCheck(inflateEnd(zstream));
end;

{** TCustomZStream **********************************************************}

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;

  FStream := stream;
  FStreamPos := stream.Position;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Self);
end;

{** TZCompressionStream *****************************************************}

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel);
begin
  inherited Create(dest);

  FZStream.next_out := FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(DeflateInit(FZStream,ZLevels[compressionLevel]));
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := Nil;
  FZStream.avail_in := 0;

  try
    if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

    while ZCompressCheck(deflate(FZStream,Z_FINISH)) <> Z_STREAM_END do
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    deflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(deflate(FZStream,Z_NO_FLUSH));

    if FZStream.avail_out = 0 then
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer));

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);

      FStreamPos := FStream.Position;

      DoProgress;
    end;
  end;

  result := Count;
end;

function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    result := FZStream.total_in;
  end
  else raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then result := 0
  else result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{** TZDecompressionStream ***************************************************}

constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);

  FZStream.next_in := FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(InflateInit(FZStream));
end;

destructor TZDecompressionStream.Destroy;
begin
  inflateEnd(FZStream);

  inherited Destroy;
end;

function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
var
  zresult: Integer;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  zresult := Z_OK;

  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := FStream.Read(FBuffer,SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        result := count - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := FBuffer;
      FStreamPos := FStream.Position;

      DoProgress;
    end;

    zresult := ZDecompressCheck(inflate(FZStream,Z_NO_FLUSH));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    FStream.Position := FStream.Position - FZStream.avail_in;
    FStreamPos := FStream.Position;

    FZStream.avail_in := 0;
  end;

  result := count - FZStream.avail_out;
end;

function TZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;

function TZDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  buf: Array [0..8191] of AnsiChar;
  i  : Integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(inflateReset(FZStream));

    FZStream.next_in := FBuffer;
    FZStream.avail_in := 0;

    FStream.Position := 0;
    FStreamPos := 0;
  end
  else if ((offset >= 0) and (origin = soFromCurrent)) or
          (((offset - FZStream.total_out) > 0) and (origin = soFromBeginning)) then
  begin
    if origin = soFromBeginning then Dec(offset,FZStream.total_out);

    if offset > 0 then
    begin
      for i := 1 to offset div SizeOf(buf) do ReadBuffer(buf,SizeOf(buf));
      ReadBuffer(buf,offset mod SizeOf(buf));
    end;
  end
  else if (offset = 0) and (origin = soFromEnd) then
  begin
    while Read(buf,SizeOf(buf)) > 0 do ;
  end
  else raise EZDecompressionError.Create(SZInvalid);

  result := FZStream.total_out;
end;

end.
