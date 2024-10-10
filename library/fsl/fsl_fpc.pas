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
  {$IFDEF FPC},
  {$IFDEF OSX}
  MacOSAll, CFBase, CFString,
  {$ENDIF}
  ZStream, dateutils{$ENDIF};

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
function strToWideString(s : String): WideString; {$IFDEF DELPHI} inline; {$ENDIF} // in delphi, this function does nothing.
function UCharArrayToString(chars : TUCharArray) : String;

function FileCanBeReadOnly : boolean;

{$IFDEF FPC}

procedure InitializeCriticalSection(out cs : TRTLCriticalSection);
procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
function TryEnterCriticalSection(var cs : TRTLCriticalSection) : boolean;

type

  { TSemaphore }

  TSemaphore = class (TEventObject)
  public
    constructor Create(attr: PSecurityAttributes; AInitialCount, AMaximumCount: Integer; const Name: string);
    procedure Release; override;
  end;

  { TCharHelper }
  {$IFNDEF CODE_PARSER} // code parser doesn't know how to parse type helper
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
  {$ENDIF}

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
procedure FileSetReadOnly(const FileName : String; readOnly : boolean);
procedure FileSetModified(const FileName : String; dateTime : TDateTime);

//function ColorToString(Color: TColor): AnsiString;

{$ENDIF}

{$IFDEF FPC}
type
   TZDecompressionStream = TDecompressionStream;
   TZCompressionStream = TCompressionStream;


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

  { TTimespan }

  TTimespan = record
    FTicks : Int64;
    class function makeTicks(ticks : Int64) : TTimespan; static;
    function hours : integer;
    function minutes : integer;
  end;
{$ENDIF}

{$IFDEF OSX}
function getMacTimezoneName : String;
{$ENDIF}

implementation

{$IFDEF FPC}
uses
  FileUtil, LazUTF8,
  fsl_base, fsl_stream, fsl_utilities;
{$ENDIF}


{$IFDEF FPC}

{$IFDEF WINDOWS}
function SetThreadUILanguage; external kernel32 name 'SetThreadUILanguage'; // 5.1
{$ELSE}
function RGB(r,g,b : longint) : DWORD;
  begin
     RGB := DWORD(((DWORD(BYTE(r))) or ((DWORD(WORD(g))) shl 8)) or ((DWORD(BYTE(b))) shl 16));
  end;

procedure setCurrentDirectory(dir : String);
begin
  chdir(dir);
end;

{$ENDIF}

function unicodeChars(s : String) : TUCharArray;
var
  i, c, l, cl : integer;
  ch : LongWord;
  p: PChar;
begin
  l := length(s);
  SetLength(result, l); // maximum possible length
  i := 0;
  c := 1;
  p := @s[1];
  while l > 0 do
  begin
    ch := UTF8CodepointToUnicode(p, cl);
    result[i] := UnicodeChar(ch);
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

function FileCanBeReadOnly : boolean;
begin
  {$IFDEF OSX}
  result := false;
  {$ELSE}
  result := true;
  {$ENDIF}

end;


{$IFDEF FPC}

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
begin
  result := FileUtil.DeleteDirectory(DirectoryName, OnlyChildren);
end;

{ TTimespan }

class function TTimespan.makeTicks(ticks: Int64): TTimespan;
begin
  result.FTicks := ticks;
end;

function TTimespan.hours: integer;
begin
  result := FTicks div (60 * 60);
end;

function TTimespan.minutes: integer;
var
  t : int64;
begin
  t := (FTicks - (hours * 60 * 60));
  result := t div 60;
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
    raise EFslException.Create('chmod failed');
{$ENDIF}
{$IFDEF OSX}
  // nothing
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

{$ENDIF}

{$IFDEF FPC}

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
  ts := TStringList.Create;
  try
    if FindFirst(fsl_utilities.FilePath([Path, '*']), faAnyFile, SearchRec) = 0 then // DO NOT LOCALIZE
    begin
      repeat
        if SearchRec.Attr and SysUtils.faDirectory = 0 then
          ts.add(fsl_utilities.FilePath([Path, SearchRec.Name]));
      until FindNext(SearchRec) <> 0;
    end;
    FindClose(SearchRec);

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
  ts := TStringList.Create;
  try
    if FindFirst(fsl_utilities.FilePath([Path, Mask]), faAnyFile, SearchRec) = 0 then // DO NOT LOCALIZE
    begin
      repeat
        if SearchRec.Attr and SysUtils.faDirectory = 0 then
          ts.add(fsl_utilities.FilePath([Path, SearchRec.Name]));
      until FindNext(SearchRec) <> 0;
    end;
    FindClose(SearchRec);

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
  ts := TStringList.Create;
  try
    if FindFirst(fsl_utilities.FilePath([Path, '*']), faAnyFile, SearchRec) = 0 then // DO NOT LOCALIZE
    begin
      repeat
        if (SearchRec.Attr and SysUtils.faDirectory <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          ts.add(fsl_utilities.FilePath([Path, SearchRec.Name]));
      until FindNext(SearchRec) <> 0;
    end;
    FindClose(SearchRec);

    result := ts.ToStringArray;
  finally
     ts.free;
  end;
end;

{$ENDIF}

{$IFDEF OSX}
// borrowed from CarbonProcs
function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding): String;
var
  Str: Pointer;
  StrSize: CFIndex;
  StrRange: CFRange;
begin
  if AString = nil then
  begin
    Result := '';
    Exit;
  end;

  // Try the quick way first
  Str := CFStringGetCStringPtr(AString, Encoding);
  if Str <> nil then
    Result := PChar(Str)
  else
  begin
    // if that doesn't work this will
    StrRange.location := 0;
    StrRange.length := CFStringGetLength(AString);

    CFStringGetBytes(AString, StrRange, Encoding,
      Ord('?'), False, nil, 0, StrSize{%H-});
    SetLength(Result, StrSize);

    if StrSize > 0 then
      CFStringGetBytes(AString, StrRange, Encoding,
        Ord('?'), False, @Result[1], StrSize, StrSize);
  end;
end;

function getMacTimezoneName;
var
  tz : CFTimeZoneRef;
begin
  tz := CFTimeZoneCopySystem;
  result := CFStringToStr(CFTimeZoneGetName(tz), kCFStringEncodingUTF8);
end;
{$ENDIF}

end.


