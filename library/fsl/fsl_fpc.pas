unit fsl_fpc;

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

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  baseunix, unix,
  {$ENDIF}
  Classes, SysUtils, SyncObjs, Contnrs, Character, Generics.Collections, ZLib, Types
  {$IFDEF FPC}, RegExpr, dateutils, upascaltz {$ENDIF};

type
  {$IFDEF FPC}
  TUCharArray = SysUtils.TUnicodeCharArray;
  {$ELSE}
  TUCharArray = SysUtils.TCharArray;
  {$ENDIF}

{$IFNDEF FPC}
type
  UnicodeChar = char;
{$ELSE}


{$IFDEF WINDOWS}
// missing from windows.pas
function SetThreadUILanguage(LangId: LANGID): LANGID; stdcall;
{$EXTERNALSYM GetThreadUILanguage}
{$ENDIF}

{$ENDIF}

{$IFDEF WINDOWS}
type
  TLibHandle = THandle;
{$ELSE}
function RGB(r,g,b : longint) : DWORD; inline;
procedure setCurrentDirectory(dir : String);
{$ENDIF}

// unicode helpers - make life easier for shared fpc/delphi code
function unicodeChars(s : String) : TUCharArray;
function strToWideString(s : String): WideString; {$IFDEF DELPHI} inline; {$ENDIF} // in delphi, this does nothing.
function UCharArrayToString(chars : TUCharArray) : String;

{$IFDEF FPC}

procedure InitializeCriticalSection(out cs : TRTLCriticalSection);
procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
function TryEnterCriticalSection(var cs : TRTLCriticalSection) : boolean;

type

  { TSemaphore }

  TSemaphore = class (TEventObject)
  public
    constructor create(attr: PSecurityAttributes; AInitialCount, AMaximumCount: Integer; const Name: string);
    procedure Release; override;
  end;

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
    FZone : String;
    FOffset : Double;
  public
    constructor Create(zone : String);
    function GetUtcOffset(const ADateTime: TDateTime): TTimeSpan; inline;
    function ToLocalTime(const ADateTime: TDateTime): TDateTime;
    function ToUniversalTime(const ADateTime: TDateTime): TDateTime; inline;
    class function Local: TTimeZone;
  end;

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
procedure FileSetReadOnly(const FileName : String; readOnly : boolean);
procedure FileSetModified(const FileName : String; dateTime : TDateTime);

//function ColorToString(Color: TColor): AnsiString;

type
  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

  // CG: Define old enum for compression level
  TCompressionLevel = (clNone = Integer(zcNone), clFastest, clDefault, clMax);

  TZStreamRec = z_stream;

  {** TCustomZStream ********************************************************}

  TCustomZStream = class(TStream)
  private
    FStream: TStream;
    FStreamStartPos: Int64;
    FStreamPos: Int64;
    FOnProgress: TNotifyEvent;
    FZStream: TZStreamRec;
    FBuffer: TBytes;
  protected
    constructor Create(stream: TStream);
    procedure DoProgress; dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;
  // CG: Add alias of classname to old Zlib classname
  TCustomZLibStream = TCustomZStream;

  {** TZCompressionStream ***************************************************}

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream); overload;
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel; windowBits: Integer); overload;
    // CG: Add overloaded constructor for old parameter type and order
    constructor Create(compressionLevel: TCompressionLevel; dest: TStream); overload;
    destructor Destroy; override;
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  // CG: Add alias of classname to old Zlib classname
  TCompressionStream = TZCompressionStream;

  {** TZDecompressionStream *************************************************}

  TZDecompressionStream = class(TCustomZStream)
  private
    FOwnsStream: Boolean;
  public
    constructor Create(source: TStream); overload;
    constructor Create(source: TStream; WindowBits: Integer); overload;
    constructor Create(source: TStream; WindowBits: Integer; OwnsStream: Boolean); overload;
    destructor Destroy; override;
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property OnProgress;
  end;
  // CG: Add alias of classname to old Zlib classname
  TDecompressionStream = TZDecompressionStream;

const
    ZLevels: array[TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
    );

  _z_errmsg: array [0..9] of String = (
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

var
  GTimeZoneData : TPascalTZ;
  GLocalTZ : TTimeZone;
{$ENDIF}

procedure initialiseTZData(filename : String);

{$IFDEF FPC}
type
  TRegExOption = (roNone, roIgnoreCase, roMultiLine, roExplicitCapture,
    roCompiled, roSingleLine, roIgnorePatternSpace, roNotEmpty);
  TRegExOptions = set of TRegExOption;

  { TRegEx }

  TRegEx = class (Regexpr.TRegExpr)
  private
  public
    constructor Create(const Pattern: string; Options: TRegExOptions); overload;
    function IsMatch(const Input: string): Boolean; overload;

    class function isMatch(const input, pattern: string): Boolean; overload;
  end;


  { TDirectory }

  TDirectory = record
    class function GetFiles(const Path: string): TStringDynArray; overload;  static;
    class function GetFiles(const Path, Mask: string): TStringDynArray; overload;  static;
    class function getDirectories(const Path: string): TStringDynArray; overload;  static;
  end;

  { TFile }

  TFile = record
    class procedure copy(src, dest : String; dummy : boolean); overload;  static;
    class procedure delete(path : String); overload;  static;
  end;

{$ENDIF}

implementation

{$IFDEF FPC}
uses
  FileUtil, LazUTF8, uPascalTZ_Types,
  fsl_base, fsl_stream, fsl_utilities;
{$ENDIF}


{$IFDEF FPC}

{$IFDEF WINDOWS}
function SetThreadUILanguage; external kernel32 name 'SetThreadUILanguage'; // 5.1
{$ELSE}
function RGB(r,g,b : longint) : DWORD;
  begin
     RGB:=DWORD(((DWORD(BYTE(r))) or ((DWORD(WORD(g))) shl 8)) or ((DWORD(BYTE(b))) shl 16));
  end;

procedure setCurrentDirectory(dir : String);
begin
  chdir(dir);
end;

{$ENDIF}

function unicodeChars(s : String) : TUCharArray;
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

function UCharArrayToString(chars : TUCharArray) : String;
var
  bytes : TBytes;
begin
  bytes := TEncoding.UTF8.GetBytes(chars);
  result := TEncoding.UTF8.GetString(bytes);
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

function UCharArrayToString(chars : TUCharArray) : String;
begin
  SetString(Result, PChar(chars), Length(chars));
end;

{$ENDIF}

{$IFDEF FPC}

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
begin
  result := FileUtil.DeleteDirectory(DirectoryName, OnlyChildren);
end;

{ TSemaphore }

constructor TSemaphore.create(attr: PSecurityAttributes; AInitialCount, AMaximumCount: Integer; const Name: string);
begin
  inherited Create(attr, false, false, name);
end;

procedure TSemaphore.Release;
begin
  setEvent();
end;

{ TTimeZone }

constructor TTimeZone.Create(zone: String);
begin
  inherited Create;
  FZone := zone;
  if FZone = '' then
    FOffset := TPascalTZ.UniversalTime - TPascalTZ.LocalTime
  else
    FOffset := GetUtcOffset(now).totalDays;
end;

function TTimeZone.GetUtcOffset(const ADateTime: TDateTime): TTimeSpan;
var
  utc : TDateTime;
begin
  utc := GTimeZoneData.LocalTimeToGMT(ADateTime, TimeZoneIANAName);
  result.TotalDays := ADateTime - utc;
end;

function TTimeZone.ToLocalTime(const ADateTime: TDateTime): TDateTime;
begin
  result := GTimeZoneData.GMTToLocalTime(ADateTime, TimeZoneIANAName);
end;

function TTimeZone.ToUniversalTime(const ADateTime: TDateTime): TDateTime;
begin
  result := GTimeZoneData.LocalTimeToGMT(ADateTime, TimeZoneIANAName);
end;

class function TTimeZone.Local: TTimeZone;
begin
  result := GLocalTZ;
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


procedure FileSetReadOnly(const FileName : String; readOnly : boolean);
var
  i : integer;
begin
{$IFDEF WINDOWS}
  i := FileGetAttr(Filename);
  if readOnly then
    i := FileSetAttr(FileName, i or faReadOnly)
  else
    i := FileSetAttr(FileName, i and not faReadOnly);
  if i <> 0 then
    RaiseLastOSError(i);
  i := FileGetAttr(Filename);
{$ENDIF}
{$IFDEF LINUX}
  if readOnly then
    i := fpchmod(filename, S_IRUSR or S_IRGRP or S_IROTH or S_IRUSR or S_IXGRP or S_IXOTH)
  else
    i := fpchmod(filename, S_IRWXU or S_IRWXG or S_IRWXO);
  if (i <> 0) then
    raise Exception.create('chmod failed');
{$ENDIF}
{$IFDEF OSX}
begin
  raise Exception.create('Not supported');
end;
{$ENDIF}
end;

procedure FileSetModified(const FileName : String; dateTime : TDateTime);
begin
  FileSetDate(filename, DateTimeToFileDate(dateTIme));
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

function ZCompressCheck(code: Integer): Integer; overload;
begin
  Result := code;

  if code < 0 then
     raise EIOException.Create(string(_z_errmsg[2 - code]));
end;

function ZCompressCheckWithoutBufferError(code: Integer): Integer; overload;
      begin
  Result := code;

  if code < 0 then
    if (code <> Z_BUF_ERROR) then
     raise EIOException.Create(string(_z_errmsg[2 - code]));
      end;

function ZDecompressCheck(code: Integer): Integer; overload;
begin
  Result := code;

  if code < 0 then
     raise EIOException.Create(string(_z_errmsg[2 - code]));
end;

function ZDecompressCheckWithoutBufferError(code: Integer): Integer; overload;
begin
  Result := code;

  if code < 0 then
    if (code <> Z_BUF_ERROR) then
     raise EIOException.Create(string(_z_errmsg[2 - code]));
    end;



{ TCustomZStream }

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;
  FStream := stream;
  FStreamStartPos := Stream.Position;
  FStreamPos := FStreamStartPos;
  SetLength(FBuffer, $10000);
  end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Self);
end;


{ TZCompressionStream }

constructor TZCompressionStream.Create(dest: TStream);
begin
  Create(dest, zcDefault, 15);
end;

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel; windowBits: Integer);
begin
  inherited Create(dest);

  FZStream.next_out := @FBuffer[0];
  FZStream.avail_out := Length(FBuffer);

  ZCompressCheck(DeflateInit2(FZStream, ZLevels[compressionLevel], Z_DEFLATED, windowBits, 8, Z_DEFAULT_STRATEGY));
end;

constructor TZCompressionStream.Create(compressionLevel: TCompressionLevel; dest: TStream);
begin
  Create(dest, TZCompressionLevel(Byte(compressionLevel)), 15);
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := nil;
  FZStream.avail_in := 0;

    try
    if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

    while ZCompressCheckWithoutBufferError(deflate(FZStream, Z_FINISH)) <> Z_STREAM_END do
      begin
      FStream.WriteBuffer(FBuffer, Length(FBuffer) - Integer(FZStream.avail_out));

      FZStream.next_out := @FBuffer[0];
      FZStream.avail_out := Length(FBuffer);
    end;

    if Integer(FZStream.avail_out) < Length(FBuffer) then
    begin
      FStream.WriteBuffer(FBuffer, Length(FBuffer) - Integer(FZStream.avail_out));
      end;
    finally
    deflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EIOException.Create('Cannot read from a compression stream');
    end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheckWithoutBufferError(deflate(FZStream, Z_NO_FLUSH));

    if FZStream.avail_out = 0 then
    begin
      FStream.WriteBuffer(FBuffer, Length(FBuffer));

      FZStream.next_out := @FBuffer[0];
      FZStream.avail_out := Length(FBuffer);

      FStreamPos := FStream.Position;

      DoProgress;
  end;
end;

  result := Count;
end;

function TZCompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (offset = 0) and (origin = soCurrent) then
  begin
    result := FZStream.total_in;
  end
  else
    raise EIOException.Create('Invalid Operation');
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then result := 0
  else result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{ TZDecompressionStream }

constructor TZDecompressionStream.Create(source: TStream);
begin
  Create(source, 15, False);
end;

constructor TZDecompressionStream.Create(source: TStream; WindowBits: Integer);
begin
  Create(source, WindowBits, False);
end;

constructor TZDecompressionStream.Create(source: TStream; WindowBits: Integer; OwnsStream: Boolean);
begin
  inherited Create(source);
  FZStream.next_in := @FBuffer[0];
  FZStream.avail_in := 0;
  ZDecompressCheckWithoutBufferError(InflateInit2(FZStream, WindowBits));
  FOwnsStream := OwnsStream;
end;

destructor TZDecompressionStream.Destroy;
begin
  inflateEnd(FZStream);
  if FOwnsStream then
    FStream.Free;
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
      FZStream.avail_in := FStream.Read(FBuffer, Length(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        result := NativeUInt(count) - FZStream.avail_out;

        Exit;
      end;
      if (length(FBuffer) = 0) then
        raise Exception.create('read File returned an empty buffer but claimed it wasn''t');

      FZStream.next_in := @FBuffer[0];
      FStreamPos := FStream.Position;

      DoProgress;
    end;

    zresult := ZDecompressCheckWithoutBufferError(inflate(FZStream, Z_NO_FLUSH));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    FStream.Position := FStream.Position - FZStream.avail_in;
    FStreamPos := FStream.Position;

    FZStream.avail_in := 0;
  end;

  result := NativeUInt(count) - FZStream.avail_out;
end;

function TZDecompressionStream.Write(const buffer; count: Longint): Longint;
begin
  raise EIOException.Create('Invalid Operation');
end;

function TZDecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  BufSize = 8192;
var
  buf: TBytes;
  i: Integer;
  localOffset: Int64;
begin
  if (Offset = 0) and (Origin = soBeginning) then
  begin
    ZDecompressCheck(inflateReset(FZStream));

    FZStream.next_in := @FBuffer;
    FZStream.avail_in := 0;

    FStream.Position := FStreamStartPos;
    FStreamPos := FStreamStartPos;
  end
  else if ((Offset >= 0) and (Origin = soCurrent)) or
    (((NativeUInt(offset) - FZStream.total_out) > 0) and (Origin = soBeginning)) then
  begin
    localOffset := Offset;
    if (Origin = soBeginning) then Dec(localOffset, FZStream.total_out);

    if localOffset > 0 then
    begin
      SetLength(buf, BufSize);
      for i := 1 to localOffset div BufSize do ReadBuffer(buf, BufSize);
      ReadBuffer(buf, localOffset mod BufSize);
    end;
  end
  else if (Offset = 0) and (Origin = soEnd) then
  begin
    SetLength(buf, BufSize);
    while Read(buf, BufSize) > 0 do ;
  end
  else
    raise EIOException.Create('Invalid Operation');

  result := FZStream.total_out;
end;

{$ENDIF}

procedure initialiseTZData(filename : String);
{$IFDEF FPC}
var
  stream : TFileStream;
  z : TZDecompressionStream;
  tar : TTarArchive;
  entry : TTarDirRec;
  bi : TBytesStream;
begin
  GTimeZoneData := TPascalTZ.create;
  stream := TFileStream.Create(filename, fmOpenRead);
  try
    z := TZDecompressionStream.Create(stream, 15+16);
    try
      tar := TTarArchive.Create(z);
      try
        while tar.FindNext(entry) do
        begin
          if (StringArrayExists(TZ_FILES_STANDARD, entry.name)) then
          begin
            bi := TBytesStream.Create;
            try
              tar.ReadFile(bi);
              bi.position := 0;
              GTimeZoneData.ParseDatabaseFromStream(bi);
            finally
              bi.free;
            end;
          end;
        end;
      finally
        tar.free;
      end;
    finally
      z.free;
    end;
  finally
    stream.Free;
  end;
  GLocalTZ := TTimeZone.create('');
end;
{$ELSE}
begin
end;
{$ENDIF}

{$IFDEF FPC}

{ TRegEx }

constructor TRegEx.Create(const Pattern: string; Options: TRegExOptions);
begin
  inherited Create(pattern);
end;

function TRegEx.IsMatch(const Input: string): Boolean;
begin
  result := Exec(input);
end;

class function TRegEx.isMatch(const input, pattern : string): Boolean;
var
  this : TRegEx;
begin
  this := TRegEx.create(pattern);
  try
    result := this.isMatch(input);
  finally
    this.free;
  end;
end;

{ TFile }

class procedure TFile.copy(src, dest: String; dummy: boolean);
begin
  CopyFile(src, dest);
end;

class procedure TFile.delete(path: String);
begin
  FileDelete(path);

end;

{ TDirectory }

class function TDirectory.GetFiles(const Path: string): TStringDynArray;
var
  ts: TStringList;
  SearchRec: TSearchRec;
begin
  ts := TStringList.create;
  try
    if FindFirst(fsl_utilities.path([Path, '*']), faAnyFile, SearchRec) = 0 then // DO NOT LOCALIZE
    begin
      repeat
        if SearchRec.Attr and SysUtils.faDirectory = 0 then
          ts.add(fsl_utilities.path([Path, SearchRec.Name]));
      until FindNext(SearchRec) <> 0;
    end;

    result := ts.ToStringArray;
  finally
     ts.free;
  end;
end;

class function TDirectory.GetFiles(const Path, Mask: string): TStringDynArray;
var
  ts: TStringList;
  SearchRec: TSearchRec;
begin
  ts := TStringList.create;
  try
    if FindFirst(fsl_utilities.path([Path, Mask]), faAnyFile, SearchRec) = 0 then // DO NOT LOCALIZE
    begin
      repeat
        if SearchRec.Attr and SysUtils.faDirectory = 0 then
          ts.add(fsl_utilities.path([Path, SearchRec.Name]));
      until FindNext(SearchRec) <> 0;
    end;

    result := ts.ToStringArray;
  finally
     ts.free;
  end;
end;

class function TDirectory.getDirectories(const Path: string): TStringDynArray;
var
  ts: TStringList;
  SearchRec: TSearchRec;
begin
  ts := TStringList.create;
  try
    if FindFirst(fsl_utilities.path([Path, '*']), faAnyFile, SearchRec) = 0 then // DO NOT LOCALIZE
    begin
      repeat
        if (SearchRec.Attr and SysUtils.faDirectory <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          ts.add(fsl_utilities.path([Path, SearchRec.Name]));
      until FindNext(SearchRec) <> 0;
    end;

    result := ts.ToStringArray;
  finally
     ts.free;
  end;
end;

{$ENDIF}

initialization
finalization
{$IFDEF FPC}
  GTimeZoneData.free;
{$ENDIF}
end.


