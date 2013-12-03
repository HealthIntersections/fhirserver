{! 1 !}
{-------------------------------------------------------------------------------
 
 Copyright (c) 1999-2010 Ralf Junker, The Delphi Inspiration
 Internet: http://www.yunqa.de/delphi/
 E-Mail:   delphi@yunqa.de

-------------------------------------------------------------------------------}

unit DISystemCompat;

interface

{$I DICompilers.inc}

{$IFDEF FPC}

type

  UnicodeString = WideString;
  PUnicodeString = ^UnicodeString;

  {$ELSE FPC}

{$IFNDEF COMPILER_6_UP}
uses
  Windows;
{$ENDIF COMPILER_6_UP}

{$IFNDEF COMPILER_12_UP}

type

  RawByteString = AnsiString;
  PRawByteString = ^RawByteString;

  UnicodeString = WideString;
  PUnicodeString = ^UnicodeString;

function StringRefCount(const s: RawByteString): Integer; overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}
function StringRefCount(const s: UnicodeString): Integer; overload; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}

function Utf8ToString(const s: RawByteString): UnicodeString; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;
function Utf8ToString(const s: PAnsiChar): UnicodeString; {$IFDEF SUPPORTS_INLINE}inline; {$ENDIF}overload;

function UTF8ToUnicodeString(const s: RawByteString): UnicodeString; overload;
function UTF8ToUnicodeString(const s: PAnsiChar): UnicodeString; overload;

{$IFNDEF COMPILER_6_UP}

type

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

  TMethod = record
    Code, Data: Pointer;
  end;

  TPointerArray = array[0..MaxInt div SizeOf(PPointer) - 1] of PPointer;
  PPointerArray = ^TPointerArray;

function UnicodeToUtf8(
  Dest: PAnsiChar;
  Source: PWideChar;
  MaxBytes: Integer): Integer; overload; {$IFDEF SUPPORTS_DEPRECATED}deprecated; {$ENDIF}
function UnicodeToUtf8(
  Dest: PAnsiChar;
  MaxDestBytes: Cardinal;
  Source: PWideChar;
  SourceChars: Cardinal): Cardinal; overload;

function Utf8Decode(const s: Utf8String): WideString; {$IFDEF SUPPORTS_DEPRECATED}deprecated; {$ENDIF}
function Utf8Encode(const WS: WideString): Utf8String;

function Utf8ToUnicode(
  Dest: PWideChar;
  Source: PChar;
  MaxChars: Integer): Integer; overload; {$IFDEF SUPPORTS_DEPRECATED}deprecated; {$ENDIF}
function Utf8ToUnicode(
  Dest: PWideChar;
  MaxDestChars: Cardinal;
  Source: PChar;
  SourceBytes: Cardinal): Cardinal; overload;

{$IFNDEF COMPILER_5_UP}

procedure FreeAndNil(var Obj);

{$IFNDEF COMPILER_4_UP}

type

  Int64 = packed record
    i1, i2: Integer;
  end;
  PInt64 = ^Int64;

  {$IFNDEF COMPILER_3_UP}

type

  PByte = ^Byte;

  {$ENDIF !COMPILER_4_UP}

  {$ENDIF !COMPILER_4_UP}
  {$ENDIF !COMPILER_5_UP}
  {$ENDIF !COMPILER_6_UP}
  {$ENDIF !COMPILER_12_UP}

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

function CompareMem(p1, p2: Pointer; Length: Integer): Boolean;

implementation

{$IFDEF FPC}

{$ELSE FPC}

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
  Result := UTF8ToUnicodeString(s);
end;

function Utf8ToString(const s: PAnsiChar): UnicodeString;
begin
  Result := UTF8ToUnicodeString(s);
end;

function UTF8ToUnicodeString(const s: RawByteString): UnicodeString;
var
  l: Cardinal;
begin
  l := Length(s);
  if l > 0 then
    begin
      SetString(Result, nil, l);
      l := Utf8ToUnicode(Pointer(Result), l + 1, Pointer(s), l);
      if l > 0 then
        begin
          SetLength(Result, l - 1);
          Exit;
        end;
    end;
  Result := '';
end;

function UTF8ToUnicodeString(const s: PAnsiChar): UnicodeString; overload;
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

{$IFNDEF COMPILER_6_UP}

function UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: Integer): Integer;
var
  Len: Cardinal;
begin
  Len := 0;
  if Source <> nil then
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

{$IFNDEF COMPILER_5_UP}

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

{$ENDIF !COMPILER_5_UP}
{$ENDIF !COMPILER_6_UP}
{$ENDIF !COMPILER_12_UP}

{$ENDIF FPC}

function CompareMem(p1, p2: Pointer; Length: Integer): Boolean;
asm

  sub ecx, 8
  jl @VerySmallCompare

  push ebx

  mov ebx, [eax]
  cmp ebx, [edx]
  je @FirstFourMatches
@InitialMismatch:
  xor eax, eax
  pop ebx
  ret
@FirstFourMatches:

  add eax, ecx
  add edx, ecx

  mov ebx, [eax]
  cmp ebx, [edx]
  jne @InitialMismatch

  mov ebx, [eax + 4]
  cmp ebx, [edx + 4]
  jne @InitialMismatch

  sub ecx, 4
  jle @InitialMatch

  push esi

  neg ecx
  add ecx, eax
  and ecx, -4
  sub ecx, eax

@CompareLoop:
  mov ebx, [eax + ecx]
  mov esi, [eax + ecx + 4]
  xor ebx, [edx + ecx]
  xor esi, [edx + ecx + 4]
  or ebx, esi
  jnz @LargeMismatch
  add ecx, 8
  jl @CompareLoop
  pop esi
@InitialMatch:
  pop ebx
@MatchSmall:
  mov al, True
  ret
@VerySmallCompare:
  add ecx, 8
  jle @MatchSmall
@SmallLoop:
  mov ch, [eax]
  xor ch, [edx]
  jnz @MismatchSmall
  add eax, 1
  add edx, 1
  sub cl, 1
  jnz @SmallLoop
  jmp @MatchSmall
@LargeMismatch:
  pop esi
  pop ebx
@MismatchSmall:
  xor eax, eax
end;

end.

