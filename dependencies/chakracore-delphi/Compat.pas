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

unit Compat;

interface

{$include common.inc}

{$ifdef FPC}
  {$macro ON}
  {$warn SYMBOL_PLATFORM OFF}
{$endif}

uses
{$ifdef DELPHI}
  Windows,
{$endif}
  SysUtils;

{$ifndef HAS_NATIVEUINT}
type
  PNativeUInt = ^NativeUInt;
  NativeUInt = Cardinal;
{$endif}

{$ifndef HAS_RAWBYTESTRING}
type
  RawByteString = AnsiString;
{$endif}

{$ifndef HAS_UINTPTR}
type
  PUIntPtr = ^UIntPtr;
  UIntPtr = NativeUInt;
{$endif}

{$ifndef HAS_WSTRPOS}
function WStrPos(const Str1, Str2: PWideChar): PWideChar;
{$endif}

type
  PUnicodeChar = ^UnicodeChar;
  UnicodeChar = WideChar;

{$ifndef SUPPORTS_UNICODE_STRING}
type
  PUnicodeString = ^UnicodeString;
  UnicodeString = WideString;
{$endif}

function GetBuildInfoString: string;
function GetExeFileVersionString: string;

{$ifdef FPC}
var
  UTF8ToString: function(const S: RawByteString): UnicodeString = @UTF8Decode;
{$endif}

{$ifdef DELPHI}
// initialize to US format settings (use point as decimal separator for float values)
var
  DefaultFormatSettings: TFormatSettings;

procedure InitCriticalSection(var Lock: TRTLCriticalSection);
procedure DoneCriticalSection(var Lock: TRTLCriticalSection);

{$ifndef HAS_WIDESTRUTILS}
function WideStringReplace(const S, OldPattern, NewPattern: Widestring; Flags: TReplaceFlags): Widestring;
{$endif}

{$ifndef UNICODE}
var
  UTF8ToString: function(const S: UTF8String): WideString;
{$endif}
{$endif}

implementation

{$ifdef FPC}
uses
  {$ifdef WINDOWS}
  winpeimagereader,
  {$endif}
  {$ifdef LINUX}
  elfreader,
  {$endif}
  {$ifdef DARWIN}
  machoreader,
  {$endif}
  fileinfo;
{$endif}

{$ifdef FPC}
function GetBuildInfoString: string;
begin
  Result := Format('FPC %d.%d.%d for %s-%s', [FPC_VERSION, FPC_RELEASE, FPC_PATCH, lowercase({$i %FPCTARGETOS%}), lowercase({$i %FPCTARGETCPU%})])
end;

function GetExeFileVersionString: string;
var
  FileVersionInfo: TFileVersionInfo;
begin
  FileVersionInfo := TFileVersionInfo.Create(nil);
  try
    FileVersionInfo.ReadFileInfo;
    Result := FileVersionInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVersionInfo.Free;
  end;
end;

{$ifndef HAS_WSTRPOS}
function WStrPos(const Str1, Str2: PWideChar): PWideChar;
begin
  Result := strpos(Str1, Str2);
end;
{$endif}
{$endif}

{$ifdef DELPHI}

procedure InitCriticalSection(var Lock: TRTLCriticalSection);
begin
  InitializeCriticalSection(Lock);
end;

procedure DoneCriticalSection(var Lock: TRTLCriticalSection);
begin
  Windows.DeleteCriticalSection(Lock);
end;

{$ifndef HAS_WIDESTRUTILS}
function WideStringReplace(const S, OldPattern, NewPattern: Widestring; Flags: TReplaceFlags): Widestring;
var
  SearchStr, Patt, NewStr: Widestring;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := WideUpperCase(S);
    Patt := WideUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;
{$endif}

{$ifdef DELPHIXE2_UP}
const
  ArchitectureStrings: array[TOSVersion.TArchitecture] of string = ('x86', 'x64', 'arm32'{$ifdef DELPHIX_BERLIN_UP}, 'arm64'{$endif});
  PlatformStrings: array[TOSVersion.TPlatform] of string = ('Windows', 'MacOS', 'iOS', 'Android', 'WinRT', 'Linux');
{$endif}

function GetBuildInfoString: string;
begin
  {$ifdef DELPHIXE2_UP}
  Result := Format('Delphi %.1f for %s-%s', [System.CompilerVersion, PlatformStrings[TOSVersion.Platform],
    ArchitectureStrings[TOSVersion.Architecture]], DefaultFormatSettings);
  {$else}
  Result := Format('Delphi %.1f for Windows-x86', [CompilerVersion], DefaultFormatSettings);
  {$endif}
end;

function GetExeFileVersionString: string;
var
  Ver: LongRec;
begin
  Ver := LongRec(GetFileVersion(Paramstr(0)));
  Result := Format('%u.%u', [Ver.Hi, Ver.Lo]);
end;

{$ifndef HAS_WSTRPOS}
function WStrPos(const Str1, Str2: PWideChar): PWideChar;
var
  Str, SubStr: PWideChar;
  Ch: WideChar;
begin
  Result := nil;
  if (Str1 = nil) or (Str1^ = #0) or (Str2 = nil) or (Str2^ = #0) then Exit;
  Result := Str1;
  Ch := Str2^;
  repeat
    if Result^ = Ch then
    begin
      Str := Result;
      SubStr := Str2;
      repeat
        Inc(Str);
        Inc(SubStr);
        if SubStr^ = #0 then exit;
        if Str^ = #0 then
        begin
          Result := nil;
          exit;
        end;
        if Str^ <> SubStr^ then break;
      until (FALSE);
    end;
    Inc(Result);
  until (Result^ = #0);
  Result := nil;
end;
{$endif}
{$endif}

initialization
{$ifdef DELPHI}
{$ifndef UNICODE}
  UTF8ToString := @UTF8Decode;
{$endif}
{$ifdef DELPHIXE_UP}
  DefaultFormatSettings := TFormatSettings.Create('en-US');
{$else}
  GetLocaleFormatSettings(1033, DefaultFormatSettings);
{$endif}
{$endif}
{$ifdef FPC}
  {$ifdef WINDOWS}
  GetLocaleFormatSettings(1033, DefaultFormatSettings);
  {$endif}
{$endif}

finalization

end.
