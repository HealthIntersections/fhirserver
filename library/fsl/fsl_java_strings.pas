unit fsl_java_strings;

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

{$I fhir.inc}

interface

uses
  Sysutils;

type

pint = ^integer;

{$IF Declared(PUTF8Char)} {$ELSE} PUTF8char = PAnsiChar;
 {$IFEND}
  
  {$IF Declared(UTF8Char)} {$ELSE} UTF8char = AnsiChar;
 {$IFEND}
    
{$IF Declared(UTF8String)} {$ELSE} UTF8String = AnsiString;
 {$IFEND}

function UTF8StrAlloc(Size: Cardinal): PUTF8Char;
function StrMove(Dest: PUTF8Char; const Source: PUTF8Char; Count: Cardinal): PUTF8Char;
function StrNew(const Str: PUTF8Char): PUTF8Char; overload;
function StrNew(const Str: String): PUTF8Char; overload;
function StrLCopy(Dest: PUTF8Char; const Source: PUTF8Char; MaxLen: Cardinal): PUTF8Char;
function ExtractFileExt(const FileName: String): String;


IMPLEMENTATION

function UTF8StrAlloc(Size: Cardinal): PUTF8Char;
begin
  Inc(Size, SizeOf(Cardinal));
  GetMem(Result, Size);
  Cardinal(Pointer(Result)^) := Size;
  Inc(Result, SizeOf(Cardinal));
end;

function StrMove(Dest: PUTF8Char; const Source: PUTF8Char; Count: Cardinal): PUTF8Char;
begin
  Result := Dest;
  Move(Source^, Dest^, Count * SizeOf(UTF8Char));
end;

function StrNew(const Str: PUTF8Char): PUTF8Char;
var
  Size: Cardinal;
begin
  if Str = nil then Result := nil else
  begin
    Size := length(Str) + 1;
    Result := StrMove(UTF8StrAlloc(Size), Str, Size);
  end;
end;

function StrNew(const Str: String): PUTF8Char;
var
  Size: Cardinal;
  p : UTF8String;
begin
  if Str = '' then
    Result := nil
  else
  begin
    p := UTF8Encode(Str);
    Size := length(p) + 1;
    Result := StrMove(UTF8StrAlloc(Size), @p, Size);
  end;
end;


function StrLCopy(Dest: PUTF8Char; const Source: PUTF8Char; MaxLen: Cardinal): PUTF8Char;
var
  Len: Cardinal;
begin
  Result := Dest;
  Len := length(Source);
  if Len > MaxLen then
    Len := MaxLen;
  Move(Source^, Dest^, Len * SizeOf(UTF8Char));
  Dest[Len] := #0;
end;


function ExtractFileExt(const FileName: String): String;
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim, FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

end.

