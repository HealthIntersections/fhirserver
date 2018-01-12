
unit myUTF8Strings;

interface

uses system.sysutils;

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
function StrNew(const Str: PUTF8Char): PUTF8Char;
function StrLCopy(Dest: PUTF8Char; const Source: PUTF8Char; MaxLen: Cardinal): PUTF8Char;
function ExtractFileExt(const FileName: UTF8String): UTF8String;


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


function ExtractFileExt(const FileName: UTF8String): UTF8String;
var
  I: Integer;
begin
  I := LastDelimiter(UTF8String('.' + PathDelim + DriveDelim), FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

end.

