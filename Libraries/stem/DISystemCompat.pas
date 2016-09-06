{-------------------------------------------------------------------------------
 
 Copyright (c) 1999-2016 Ralf Junker, Yunqa
 Internet: http://www.yunqa.de
 E-Mail:   delphi@yunqa.de

-------------------------------------------------------------------------------}

unit DISystemCompat;

interface

{$I DICompilers.inc}

{$IFDEF FPC}

type

  RawByteString = AnsiString;

  {$ELSE FPC}

{$IFNDEF COMPILER_6_UP}
uses
  Windows;
{$ENDIF COMPILER_6_UP}

{$IFNDEF COMPILER_3_UP}

type

  PByte = ^Byte;

  {$ENDIF ~COMPILER_3_UP}

  {$IFNDEF COMPILER_4_UP}

type

  Int64 = packed record
    i1, i2: Integer;
  end;
  PInt64 = ^Int64;

  {$ENDIF ~COMPILER_4_UP}

  {$IFNDEF COMPILER_5_UP}

procedure FreeAndNil(var Obj);

{$ENDIF ~COMPILER_5_UP}

{$IFNDEF COMPILER_6_UP}

type

  TMethod = record
    Code, Data: Pointer;
  end;

  PBoolean = ^Boolean;
  PCardinal = ^Cardinal;
  PDouble = ^Double;
  PInteger = Windows.PInteger;
  PPAnsiChar = ^PAnsiChar;
  PPointer = ^Pointer;
  PPWideChar = ^PWideChar;
  PWord = ^Word;
  PWordBool = ^WordBool;

  Utf8String = type AnsiString;
  PUtf8String = ^Utf8String;

  IntegerArray = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;
  PIntegerArray = ^IntegerArray;

  TPointerArray = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
  PPointerArray = ^TPointerArray;

  TIntegerDynArray = array of Integer;
  TInt64DynArray = array of Int64;

  UCS4Char = type LongWord;
  PUCS4Char = ^UCS4Char;
  UCS4String = array of UCS4Char;

function UnicodeToUtf8(
  Dest: PAnsiChar;
  Source: PWideChar;
  MaxBytes: Integer): Integer; overload; {$IFDEF SUPPORTS_DEPRECATED}deprecated; {$ENDIF}
function UnicodeToUtf8(
  Dest: PAnsiChar;
  MaxDestBytes: Cardinal;
  Source: PWideChar;
  SourceChars: Cardinal): Cardinal; overload;

function Utf8Decode(
  const s: Utf8String): WideString; {$IFDEF SUPPORTS_DEPRECATED}deprecated; {$ENDIF}
function Utf8Encode(
  const WS: WideString): Utf8String;

function Utf8ToUnicode(
  Dest: PWideChar;
  Source: PChar;
  MaxChars: Integer): Integer; overload; {$IFDEF SUPPORTS_DEPRECATED}deprecated; {$ENDIF}
function Utf8ToUnicode(
  Dest: PWideChar;
  MaxDestChars: Cardinal;
  Source: PChar;
  SourceBytes: Cardinal): Cardinal; overload;

{$ENDIF ~COMPILER_6_UP}

{$IFNDEF COMPILER_7_UP}

const

  faSymLink = $00000400{$IFDEF SUPPORTS_PLATFORM}platform{$ENDIF};

  {$ENDIF ~COMPILER_7_UP}

  {$IFNDEF COMPILER_9_UP}

type

  UInt64 = 0..High(Int64);

  {$ENDIF ~COMPILE_9_UP}

  {$IFNDEF COMPILER_12_UP}

type

  NativeInt = Integer;

  RawByteString = AnsiString;
  PRawByteString = ^RawByteString;

  UnicodeString = WideString;
  PUnicodeString = ^UnicodeString;

function StringRefCount(const s: RawByteString): Integer; overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
function StringRefCount(const s: UnicodeString): Integer; overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}

function Utf8ToString(const s: RawByteString): UnicodeString; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;
function Utf8ToString(const s: PAnsiChar): UnicodeString; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;

function Utf8ToUnicodeString(const s: RawByteString): UnicodeString; overload;
function Utf8ToUnicodeString(const s: PAnsiChar): UnicodeString; overload;

{$ENDIF ~COMPILER_12_UP}

{$IFNDEF COMPILER_14_UP}

type

  NativeUInt = Cardinal;

  {$IFDEF MSWINDOWS}
const
  INVALID_FILE_ATTRIBUTES = Cardinal($FFFFFFFF);
  {$ENDIF MSWINDOWS]

    { Converts a UCS-4 encoded string to a Unicode string.

      Call UCS4StringToUnicodeString to convert a UCS-4 encoded string to Unicode.
      S is a string that contains UCS-4 encoded characters. The result of the
      function is the corresponding Unicode (UTF-16) string value.

      <<name>> assumes that s contains a terminating null character and
      converts ##Length(s) - 1## characters. }
function UCS4StringToUnicodeString(
  const s: UCS4String): UnicodeString;

{$ENDIF ~COMPILER_14_UP}

{$IFNDEF COMPILER_15_UP}

type

  PNativeInt = ^NativeInt;

  PNativeUInt = ^NativeUInt;

function IsRelativePath(
  const Path: string): Boolean; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}

function IsRelativePathA(
  const Path: RawByteString): Boolean;

function IsRelativePathW(
  const Path: UnicodeString): Boolean;

{$ENDIF ~COMPILER_15_UP}

{$IFNDEF COMPILER_17_UP}

type

  MarshaledAString = PAnsiChar;

function AtomicIncrement(
  var Target: Cardinal): Cardinal; overload;

function AtomicIncrement(
  var Target: Integer): Integer; overload;

function AtomicIncrement(
  var Target: Cardinal;
  Increment: Cardinal): Cardinal; overload;

function AtomicIncrement(
  var Target: Integer;
  Increment: Integer): Integer; overload;

function AtomicDecrement(
  var Target: Integer): Integer; overload;

function AtomicDecrement(
  var Target: Cardinal): Cardinal; overload;

function AtomicDecrement(
  var Target: Integer;
  Decrement: Integer): Integer; overload;

function AtomicDecrement(
  var Target: Cardinal;
  Decrement: Cardinal): Cardinal; overload;

{$ENDIF ~COMPILER_17_UP}

{$ENDIF FPC}

type

  TPAnsiCharArray = array[0..MaxInt div SizeOf(PAnsiChar) - 1] of PAnsiChar;
  PPAnsiCharArray = ^TPAnsiCharArray;
  PPPAnsiCharArray = ^PPAnsiCharArray;

  PUtf8Char = PAnsiChar;
  PPUtf8Char = ^PUtf8Char;

  TPUtf8CharArray = array[0..MaxInt div SizeOf(PUtf8Char) - 1] of PUtf8Char;
  PPUtf8CharArray = ^TPUtf8CharArray;
  PPPUtf8CharArray = ^PPUtf8CharArray;

implementation

{$IFDEF FPC}

{$ELSE FPC}

{$IFNDEF COMPILER_5_UP}

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

{$ENDIF ~COMPILER_5_UP}

{$IFNDEF COMPILER_6_UP}

function UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: Integer): Integer;
var
  Len: Cardinal;
begin
  Len := 0;
  if Assigned(Source) then
    while Source[Len] <> #0 do
      Inc(Len);
  Result := UnicodeToUtf8(Dest, MaxBytes, Source, Len);
end;

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, Count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  Count := 0;
  i := 0;
  if Dest <> nil then
    begin
      while (i < SourceChars) and (Count < MaxDestBytes) do
        begin
          c := Cardinal(Source[i]);
          Inc(i);
          if c <= $7F then
            begin
              Dest[Count] := Char(c);
              Inc(Count);
            end
          else if c > $7FF then
            begin
              if Count + 3 > MaxDestBytes then
                Break;
              Dest[Count] := Char($E0 or (c shr 12));
              Dest[Count + 1] := Char($80 or ((c shr 6) and $3F));
              Dest[Count + 2] := Char($80 or (c and $3F));
              Inc(Count, 3);
            end
          else
            begin
              if Count + 2 > MaxDestBytes then
                Break;
              Dest[Count] := Char($C0 or (c shr 6));
              Dest[Count + 1] := Char($80 or (c and $3F));
              Inc(Count, 2);
            end;
        end;
      if Count >= MaxDestBytes then Count := MaxDestBytes - 1;
      Dest[Count] := #0;
    end
  else
    begin
      while i < SourceChars do
        begin
          c := Integer(Source[i]);
          Inc(i);
          if c > $7F then
            begin
              if c > $7FF then
                Inc(Count);
              Inc(Count);
            end;
          Inc(Count);
        end;
    end;
  Result := Count + 1;
end;

function Utf8Decode(const s: Utf8String): WideString;
var
  l: Integer;
  Temp: WideString;
begin
  Result := '';
  if s = '' then Exit;
  SetLength(Temp, Length(s));

  l := Utf8ToUnicode(PWideChar(Temp), Length(Temp) + 1, PChar(s), Length(s));
  if l > 0 then
    SetLength(Temp, l - 1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Encode(const WS: WideString): Utf8String;
var
  l: Integer;
  Temp: Utf8String;
begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3);

  l := UnicodeToUtf8(PChar(Temp), Length(Temp) + 1, PWideChar(WS), Length(WS));
  if l > 0 then
    SetLength(Temp, l - 1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: Integer): Integer;
var
  Len: Cardinal;
begin
  Len := 0;
  if Source <> nil then
    while Source[Len] <> #0 do
      Inc(Len);
  Result := Utf8ToUnicode(Dest, MaxChars, Source, Len);
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, Count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
    begin
      Result := 0;
      Exit;
    end;
  Result := Cardinal(-1);
  Count := 0;
  i := 0;
  if Dest <> nil then
    begin
      while (i < SourceBytes) and (Count < MaxDestChars) do
        begin
          wc := Cardinal(Source[i]);
          Inc(i);
          if (wc and $80) <> 0 then
            begin
              if i >= SourceBytes then Exit;
              wc := wc and $3F;
              if (wc and $20) <> 0 then
                begin
                  c := Byte(Source[i]);
                  Inc(i);
                  if (c and $C0) <> $80 then Exit;
                  if i >= SourceBytes then Exit;
                  wc := (wc shl 6) or (c and $3F);
                end;
              c := Byte(Source[i]);
              Inc(i);
              if (c and $C0) <> $80 then Exit;

              Dest[Count] := WideChar((wc shl 6) or (c and $3F));
            end
          else
            Dest[Count] := WideChar(wc);
          Inc(Count);
        end;
      if Count >= MaxDestChars then Count := MaxDestChars - 1;
      Dest[Count] := #0;
    end
  else
    begin
      while (i < SourceBytes) do
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $80) <> 0 then
            begin
              if i >= SourceBytes then Exit;
              c := c and $3F;
              if (c and $20) <> 0 then
                begin
                  c := Byte(Source[i]);
                  Inc(i);
                  if (c and $C0) <> $80 then Exit;
                  if i >= SourceBytes then Exit;
                end;
              c := Byte(Source[i]);
              Inc(i);
              if (c and $C0) <> $80 then Exit;
            end;
          Inc(Count);
        end;
    end;
  Result := Count + 1;
end;

{$ENDIF ~COMPILER_6_UP}

{$IFNDEF COMPILER_12_UP}

function StringRefCount(const s: RawByteString): Integer;
begin
  Result := Integer(s);
  if Result <> 0 then
    Result := PInteger(Result - 8)^;
end;

function StringRefCount(const s: UnicodeString): Integer;
begin
  Result := Integer(s);
  if Result <> 0 then
    Result := PInteger(Result - 8)^;
end;

function Utf8ToString(const s: RawByteString): UnicodeString;
begin
  Result := Utf8ToUnicodeString(s);
end;

function Utf8ToString(const s: PAnsiChar): UnicodeString;
begin
  Result := Utf8ToUnicodeString(s);
end;

function Utf8ToUnicodeString(const s: RawByteString): UnicodeString;
var
  l: Cardinal;
begin
  l := Length(s);
  if l > 0 then
    begin
      SetString(Result, nil, l);
      l := Utf8ToUnicode(Pointer(Result), l + 1, Pointer(s), l);

      if (l > 0){$IFNDEF COMPILER_12_UP} and (l <> Cardinal(-1)){$ENDIF} then
        begin
          SetLength(Result, l - 1);
          Exit;
        end;
    end;
  Result := '';
end;

function Utf8ToUnicodeString(const s: PAnsiChar): UnicodeString; overload;
var
  l: Integer;
begin
  l := 0;
  if Assigned(s) then
    while s[l] <> #0 do
      Inc(l);
  if l > 0 then
    begin
      SetString(Result, nil, l);
      l := Utf8ToUnicode(Pointer(Result), l + 1, s, l);
      if l > 0 then
        begin
          SetLength(Result, l - 1);
          Exit;
        end;
    end;
  Result := '';
end;

{$ENDIF ~COMPILER_12_UP}

{$IFNDEF COMPILER_14_UP}

function UCS4StringToUnicodeString(const s: UCS4String): UnicodeString;
var
  c: UCS4Char;
  i, l: NativeInt;
  p: PWideChar;
begin
  l := Length(s) - 1;
  for i := Low(s) to High(s) - 1 do
    if s[i] >= $10000 then
      Inc(l);

  SetString(Result, nil, l);
  p := Pointer(Result);

  for i := Low(s) to High(s) - 1 do
    begin
      c := s[i];
      if c < $10000 then
        begin
          p[0] := WideChar(c);
          Inc(p);
        end
      else
        begin
          Dec(c, $10000);
          p[0] := WideChar($D800 + c shr 10);
          p[1] := WideChar($DC00 + c and $3FF);
          Inc(p, 2);
        end;
    end;
end;

{$ENDIF ~COMPILER_14_UP}

{$IFNDEF COMPILER_15_UP}

function IsRelativePath(const Path: string): Boolean;
begin
  Result := {$IFDEF UNICODE}IsRelativePathW{$ELSE}IsRelativePathA{$ENDIF}(Path);
end;

function IsRelativePathA(const Path: RawByteString): Boolean;
var
  l: Integer;
begin
  l := Length(Path);
  Result := (l > 0) and (Path[1] <> '/')
    {$IFDEF MSWINDOWS} and (l > 1) and (Path[2] <> ':'){$ENDIF MSWINDOWS};
end;

function IsRelativePathW(const Path: UnicodeString): Boolean;
var
  l: Integer;
begin
  l := Length(Path);
  Result := (l > 0) and (Path[1] <> '/')
    {$IFDEF MSWINDOWS} and (l > 1) and (Path[2] <> ':'){$ENDIF MSWINDOWS};
end;

{$ENDIF ~COMPILER_15_UP}

{$IFNDEF COMPILER_17_UP}

function AtomicIncrement(var Target: Cardinal): Cardinal; overload;
{$IFDEF CPUX86}
asm
  MOV ECX, EAX
  MOV EAX, 1
  LOCK XADD [ECX], EAX
  INC EAX
end;
{$ELSE CPUX86}
{$IFDEF CPUX64}
asm
  MOV EAX, 1
  LOCK XADD [RCX], EAX
  INC EAX
end;
{$ENDIF CPUX64}
{$ENDIF CPUX86}

function AtomicIncrement(var Target: Integer): Integer; overload;
{$IFDEF CPUX86}
asm
  MOV ECX, EAX
  MOV EAX, 1
  LOCK XADD [ECX], EAX
  INC EAX
end;
{$ELSE CPUX86}
{$IFDEF CPUX64}
asm
  MOV  EAX, 1
  LOCK XADD [RCX], EAX
  INC EAX
end;
{$ENDIF CPUX64}
{$ENDIF CPUX86}

function AtomicIncrement(var Target: Cardinal; Increment: Cardinal): Cardinal; overload;
{$IFDEF CPUX86}
asm
  MOV ECX, EAX
  MOV EAX, EDX
  LOCK XADD [ECX], EAX
  ADD EAX, EDX
end;
{$ELSE CPUX86}
{$IFDEF CPUX64}
asm
  MOV EAX, EDX
  LOCK XADD [RCX], EAX
  ADD EAX, EDX
end;
{$ENDIF CPUX64}
{$ENDIF CPUX86}

function AtomicIncrement(var Target: Integer; Increment: Integer): Integer; overload;
{$IFDEF CPUX86}
asm
  MOV ECX, EAX
  MOV EAX, EDX
  LOCK XADD [ECX], EAX
  ADD EAX, EDX
end;
{$ELSE CPUX86}
{$IFDEF CPUX64}
asm
  MOV EAX, EDX
  LOCK XADD [RCX], EAX
  ADD EAX, EDX
end;
{$ENDIF CPUX64}
{$ENDIF CPUX86}

function AtomicDecrement(var Target: Integer): Integer; overload;
{$IFDEF CPUX86}
asm
  MOV ECX, EAX
  MOV EAX, -1
  LOCK XADD [ECX], EAX
  DEC EAX
end;
{$ELSE CPUX86}
{$IFDEF CPUX64}
asm
  MOV EAX, -1
  LOCK XADD [RCX], EAX
  DEC EAX
end;
{$ENDIF CPUX64}
{$ENDIF CPUX86}

function AtomicDecrement(var Target: Cardinal): Cardinal; overload;
{$IFDEF CPUX86}
asm
  MOV ECX, EAX
  MOV EAX, -1
  LOCK XADD [ECX], EAX
  DEC EAX
end;
{$ELSE CPUX86}
{$IFDEF CPUX64}
asm
  MOV EAX, -1
  LOCK XADD [RCX], EAX
  DEC EAX
end;
{$ENDIF CPUX64}
{$ENDIF CPUX86}

function AtomicDecrement(var Target: Integer; Decrement: Integer): Integer; overload;
{$IFDEF CPUX86}
asm
  MOV ECX, EAX
  NEG EDX
  MOV EAX, EDX
  LOCK XADD [ECX], EAX
  ADD EAX, EDX
end;
{$ELSE CPUX86}
{$IFDEF CPUX64}
asm
  NEG EDX
  MOV EAX, EDX
  LOCK XADD [RCX], EAX
  ADD EAX, EDX
end;
{$ENDIF CPUX64}
{$ENDIF CPUX86}

function AtomicDecrement(var Target: Cardinal; Decrement: Cardinal): Cardinal; overload;
{$IFDEF CPUX86}
asm
  MOV ECX, EAX
  NEG EDX
  MOV EAX, EDX
  LOCK XADD [ECX], EAX
  ADD EAX, EDX
end;
{$ELSE CPUX86}
{$IFDEF CPUX64}
asm
  NEG EDX
  MOV EAX, EDX
  LOCK XADD [RCX], EAX
  ADD EAX, EDX
end;
{$ENDIF CPUX64}
{$ENDIF CPUX86}

{$ENDIF ~COMPILER_17_UP}

{$ENDIF FPC}

end.

