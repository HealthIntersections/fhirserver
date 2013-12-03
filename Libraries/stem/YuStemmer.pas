{! 1 !}
{-------------------------------------------------------------------------------
 
 Copyright (c) 1999-2010 Ralf Junker, The Delphi Inspiration
 Internet: http://www.yunqa.de/delphi/
 E-Mail:   delphi@yunqa.de

-------------------------------------------------------------------------------}

unit YuStemmer;

{$I DI.inc}

interface

uses
  DISystemCompat;

type

  SN_env = packed record
    p: PAnsiChar;
    c: Integer; l: Integer; lb: Integer; bra: Integer; ket: Integer;
    s: ^PAnsiChar;
    i: ^Integer;
    b: Pointer;
  end;

  SN_env_ptr = ^SN_env;

  sb_stemmer_close_func = procedure(z: SN_env_ptr);

  sb_stemmer_stem_func = function(z: SN_env_ptr): Integer;

type

  TYuStemmer = class
  private
    FStem: sb_stemmer_stem_func;
    FEnv: SN_env_ptr;
    FClose: sb_stemmer_close_func;
  public
    destructor Destroy; override;

    function Stem(const s: AnsiString): AnsiString; overload;

    procedure Stem(var p: PAnsiChar; var l: Integer); overload;
  end;

function GetStemmer(const AName: AnsiString): TYuStemmer;

const

  STEMMER_LIST: array[0..15] of AnsiString = (
    'danish',
    'dutch',
    'english',
    'finnish',
    'french',
    'german',
    'german2',
    'hungarian',
    'italian',
    'norwegian',
    'porter',
    'portuguese',
    'romanian',
    'russian',
    'spanish',
    'swedish'
    );

type

  TYuStemmer_8 = class
  private
    FStem: sb_stemmer_stem_func;
    FEnv: SN_env_ptr;
    FClose: sb_stemmer_close_func;
  public
    destructor Destroy; override;

    function Stem(const s: Utf8String): Utf8String; overload;

    procedure Stem(var p: PUtf8Char; var l: Integer); overload;
  end;

function GetStemmer_8(const AName: Utf8String): TYuStemmer_8;

const

  STEMMER_LIST_8: array[0..16] of Utf8String = (
    'danish',
    'dutch',
    'english',
    'finnish',
    'french',
    'german',
    'german2',
    'hungarian',
    'italian',
    'norwegian',
    'porter',
    'portuguese',
    'romanian',
    'russian',
    'spanish',
    'swedish',
    'turkish'
    );

type

  SN_env_16 = packed record
    p: PWideChar;
    c: Integer; l: Integer; lb: Integer; bra: Integer; ket: Integer;
    s: ^PWideChar;
    i: ^Integer;
    b: Pointer;
  end;

  SN_env_16_ptr = ^SN_env_16;

  sb_stemmer_16_close_func = procedure(z: SN_env_16_ptr);

  sb_stemmer_16_stem_func = function(z: SN_env_16_ptr): Integer;

type

  TYuStemmer_16 = class
  private
    FStem: sb_stemmer_16_stem_func;
    FEnv: SN_env_16_ptr;
    FClose: sb_stemmer_16_close_func;
  public
    destructor Destroy; override;

    function Stem(const s: UnicodeString): UnicodeString; overload;

    procedure Stem(var p: PWideChar; var l: Integer); overload;
  end;

function GetStemmer_16(const AName: UnicodeString): TYuStemmer_16;

const

  STEMMER_LIST_16: array[0..16] of UnicodeString = (
    'danish',
    'dutch',
    'english',
    'finnish',
    'french',
    'german',
    'german2',
    'hungarian',
    'italian',
    'norwegian',
    'porter',
    'portuguese',
    'romanian',
    'russian',
    'spanish',
    'swedish',
    'turkish'
    );

type

  TYuStemmer_Danish = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Danish_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Danish_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Dutch = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Dutch_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Dutch_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_English = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_English_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_English_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Finnish = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Finnish_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Finnish_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_French = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_French_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_French_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_German = class(TYuStemmer)
  public
    constructor Create;
  end;
  TYuStemmer_German_Class = class of TYuStemmer_German;

  TYuStemmer_German_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_German_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_German2 = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_German2_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_German2_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Hungarian = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Hungarian_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Hungarian_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Italian = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Italian_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Italian_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Norwegian = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Norwegian_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Norwegian_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Porter = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Porter_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Porter_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Portuguese = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Portuguese_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Portuguese_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Romanian = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Romanian_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Romanian_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Russian = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Russian_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Russian_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Spanish = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Spanish_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Spanish_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Swedish = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Swedish_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Swedish_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Turkish_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Turkish_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

implementation

type

  C_char = System.AnsiChar;
  C_char_ptr = System.PAnsiChar;
  C_char_ptr_ptr = ^C_char_ptr;
  C_char_ptr_array = packed array[0..MaxInt div SizeOf(C_char_ptr) - 1] of C_char_ptr;
  C_char_ptr_array_ptr = ^C_char_ptr_array;
  C_double = System.Double;
  C_double_ptr = ^C_double;
  C_float = System.Single;
  C_float_ptr = ^C_float;
  C_int = System.Integer;
  C_int_ptr = ^C_int;
  C_long = System.Integer;
  C_long_ptr = ^C_long;
  C_long_int = System.Integer;
  C_long_int_ptr = C_long_int;
  C_short = System.SmallInt;
  C_short_ptr = ^C_short;
  C_short_int = System.SmallInt;
  C_short_int_ptr = ^C_short_int;
  C_signed_char = System.ShortInt;
  C_signed_int = System.Integer;
  C_signed_int_ptr = ^C_signed_int;
  C_signed_long = System.Integer;
  C_signed_long_ptr = ^C_signed_long;
  C_signed_short = System.SmallInt;
  C_unsigned = System.Cardinal;
  C_unsigned_ptr = ^C_unsigned;
  C_unsigned_char = System.Byte;
  C_unsigned_char_array = packed array[0..MaxInt div SizeOf(C_unsigned_char) - 1] of C_unsigned_char;
  C_unsigned_char_array_ptr = ^C_unsigned_char_array;
  C_unsigned_char_ptr = ^C_unsigned_char;
  C_unsigned_char_ptr_array = packed array[0..MaxInt div SizeOf(C_unsigned_char_ptr) - 1] of C_unsigned_char_ptr;
  C_unsigned_char_ptr_array_ptr = ^C_unsigned_char_ptr_array;
  C_unsigned_int = System.Cardinal;
  C_unsigned_int_ptr = ^C_unsigned_int;
  C_unsigned_long = System.Cardinal;
  C_unsigned_long_ptr = ^C_unsigned_long;
  C_unsigned_short = System.Word;
  C_unsigned_short_ptr = ^C_unsigned_short;

  C_size_t = C_unsigned;

  C_void_ptr = System.Pointer;
  C_void_ptr_ptr = ^C_void_ptr;
  C_wchar_t = System.WideChar;

  C_wint_t = C_wchar_t;

  C_wchar_t_ptr = System.PWideChar;

type
  symbol_ptr = C_char_ptr;

function SN_set_current(
  z: SN_env_ptr;
  Size: C_int;
  const s: symbol_ptr): C_int; external;

type
  symbol_16_ptr = C_wchar_t_ptr;

function SN_set_current_16(
  z: SN_env_16_ptr;
  Size: C_int;
  const s: symbol_16_ptr): C_int; external;

function danish_iso_8859_1_create_env: SN_env_ptr; external;
procedure danish_iso_8859_1_close_env(z: SN_env_ptr); external;
function danish_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function danish_utf_8_create_env: SN_env_ptr; external;
procedure danish_utf_8_close_env(z: SN_env_ptr); external;
function danish_utf_8_stem(z: SN_env_ptr): C_int; external;

function danish_utf_16_create_env: SN_env_16_ptr; external;
procedure danish_utf_16_close_env(z: SN_env_16_ptr); external;
function danish_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function dutch_iso_8859_1_create_env: SN_env_ptr; external;
procedure dutch_iso_8859_1_close_env(z: SN_env_ptr); external;
function dutch_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function dutch_utf_8_create_env: SN_env_ptr; external;
procedure dutch_utf_8_close_env(z: SN_env_ptr); external;
function dutch_utf_8_stem(z: SN_env_ptr): C_int; external;

function dutch_utf_16_create_env: SN_env_16_ptr; external;
procedure dutch_utf_16_close_env(z: SN_env_16_ptr); external;
function dutch_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function english_iso_8859_1_create_env: SN_env_ptr; external;
procedure english_iso_8859_1_close_env(z: SN_env_ptr); external;
function english_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function english_utf_8_create_env: SN_env_ptr; external;
procedure english_utf_8_close_env(z: SN_env_ptr); external;
function english_utf_8_stem(z: SN_env_ptr): C_int; external;

function english_utf_16_create_env: SN_env_16_ptr; external;
procedure english_utf_16_close_env(z: SN_env_16_ptr); external;
function english_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function finnish_iso_8859_1_create_env: SN_env_ptr; external;
procedure finnish_iso_8859_1_close_env(z: SN_env_ptr); external;
function finnish_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function finnish_utf_8_create_env: SN_env_ptr; external;
procedure finnish_utf_8_close_env(z: SN_env_ptr); external;
function finnish_utf_8_stem(z: SN_env_ptr): C_int; external;

function finnish_utf_16_create_env: SN_env_16_ptr; external;
procedure finnish_utf_16_close_env(z: SN_env_16_ptr); external;
function finnish_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function french_iso_8859_1_create_env: SN_env_ptr; external;
procedure french_iso_8859_1_close_env(z: SN_env_ptr); external;
function french_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function french_utf_8_create_env: SN_env_ptr; external;
procedure french_utf_8_close_env(z: SN_env_ptr); external;
function french_utf_8_stem(z: SN_env_ptr): C_int; external;

function french_utf_16_create_env: SN_env_16_ptr; external;
procedure french_utf_16_close_env(z: SN_env_16_ptr); external;
function french_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function german_iso_8859_1_create_env: SN_env_ptr; external;
procedure german_iso_8859_1_close_env(z: SN_env_ptr); external;
function german_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function german_utf_8_create_env: SN_env_ptr; external;
procedure german_utf_8_close_env(z: SN_env_ptr); external;
function german_utf_8_stem(z: SN_env_ptr): C_int; external;

function german_UTF_16_create_env: SN_env_16_ptr; external;
procedure german_UTF_16_close_env(z: SN_env_16_ptr); external;
function german_UTF_16_stem(z: SN_env_16_ptr): C_int; external;

function german2_iso_8859_1_create_env: SN_env_ptr; external;
procedure german2_iso_8859_1_close_env(z: SN_env_ptr); external;
function german2_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function german2_utf_8_create_env: SN_env_ptr; external;
procedure german2_utf_8_close_env(z: SN_env_ptr); external;
function german2_utf_8_stem(z: SN_env_ptr): C_int; external;

function german2_utf_16_create_env: SN_env_16_ptr; external;
procedure german2_utf_16_close_env(z: SN_env_16_ptr); external;
function german2_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function hungarian_iso_8859_1_create_env: SN_env_ptr; external;
procedure hungarian_iso_8859_1_close_env(z: SN_env_ptr); external;
function hungarian_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function hungarian_utf_8_create_env: SN_env_ptr; external;
procedure hungarian_utf_8_close_env(z: SN_env_ptr); external;
function hungarian_utf_8_stem(z: SN_env_ptr): C_int; external;

function hungarian_utf_16_create_env: SN_env_16_ptr; external;
procedure hungarian_utf_16_close_env(z: SN_env_16_ptr); external;
function hungarian_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function italian_iso_8859_1_create_env: SN_env_ptr; external;
procedure italian_iso_8859_1_close_env(z: SN_env_ptr); external;
function italian_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function italian_utf_8_create_env: SN_env_ptr; external;
procedure italian_utf_8_close_env(z: SN_env_ptr); external;
function italian_utf_8_stem(z: SN_env_ptr): C_int; external;

function italian_utf_16_create_env: SN_env_16_ptr; external;
procedure italian_utf_16_close_env(z: SN_env_16_ptr); external;
function italian_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function norwegian_iso_8859_1_create_env: SN_env_ptr; external;
procedure norwegian_iso_8859_1_close_env(z: SN_env_ptr); external;
function norwegian_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function norwegian_utf_8_create_env: SN_env_ptr; external;
procedure norwegian_utf_8_close_env(z: SN_env_ptr); external;
function norwegian_utf_8_stem(z: SN_env_ptr): C_int; external;

function norwegian_utf_16_create_env: SN_env_16_ptr; external;
procedure norwegian_utf_16_close_env(z: SN_env_16_ptr); external;
function norwegian_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function porter_iso_8859_1_create_env: SN_env_ptr; external;
procedure porter_iso_8859_1_close_env(z: SN_env_ptr); external;
function porter_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function porter_utf_8_create_env: SN_env_ptr; external;
procedure porter_utf_8_close_env(z: SN_env_ptr); external;
function porter_utf_8_stem(z: SN_env_ptr): C_int; external;

function porter_utf_16_create_env: SN_env_16_ptr; external;
procedure porter_utf_16_close_env(z: SN_env_16_ptr); external;
function porter_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function portuguese_iso_8859_1_create_env: SN_env_ptr; external;
procedure portuguese_iso_8859_1_close_env(z: SN_env_ptr); external;
function portuguese_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function portuguese_utf_8_create_env: SN_env_ptr; external;
procedure portuguese_utf_8_close_env(z: SN_env_ptr); external;
function portuguese_utf_8_stem(z: SN_env_ptr): C_int; external;

function portuguese_utf_16_create_env: SN_env_16_ptr; external;
procedure portuguese_utf_16_close_env(z: SN_env_16_ptr); external;
function portuguese_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function romanian_iso_8859_2_create_env: SN_env_ptr; external;
procedure romanian_iso_8859_2_close_env(z: SN_env_ptr); external;
function romanian_iso_8859_2_stem(z: SN_env_ptr): C_int; external;

function romanian_utf_8_create_env: SN_env_ptr; external;
procedure romanian_utf_8_close_env(z: SN_env_ptr); external;
function romanian_utf_8_stem(z: SN_env_ptr): C_int; external;

function romanian_utf_16_create_env: SN_env_16_ptr; external;
procedure romanian_utf_16_close_env(z: SN_env_16_ptr); external;
function romanian_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function russian_KOI8_R_create_env: SN_env_ptr; external;
procedure russian_KOI8_R_close_env(z: SN_env_ptr); external;
function russian_KOI8_R_stem(z: SN_env_ptr): C_int; external;

function russian_utf_8_create_env: SN_env_ptr; external;
procedure russian_utf_8_close_env(z: SN_env_ptr); external;
function russian_utf_8_stem(z: SN_env_ptr): C_int; external;

function russian_utf_16_create_env: SN_env_16_ptr; external;
procedure russian_utf_16_close_env(z: SN_env_16_ptr); external;
function russian_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function spanish_iso_8859_1_create_env: SN_env_ptr; external;
procedure spanish_iso_8859_1_close_env(z: SN_env_ptr); external;
function spanish_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function spanish_utf_8_create_env: SN_env_ptr; external;
procedure spanish_utf_8_close_env(z: SN_env_ptr); external;
function spanish_utf_8_stem(z: SN_env_ptr): C_int; external;

function spanish_utf_16_create_env: SN_env_16_ptr; external;
procedure spanish_utf_16_close_env(z: SN_env_16_ptr); external;
function spanish_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function swedish_iso_8859_1_create_env: SN_env_ptr; external;
procedure swedish_iso_8859_1_close_env(z: SN_env_ptr); external;
function swedish_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function swedish_utf_8_create_env: SN_env_ptr; external;
procedure swedish_utf_8_close_env(z: SN_env_ptr); external;
function swedish_utf_8_stem(z: SN_env_ptr): C_int; external;

function swedish_utf_16_create_env: SN_env_16_ptr; external;
procedure swedish_utf_16_close_env(z: SN_env_16_ptr); external;
function swedish_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function turkish_utf_8_create_env: SN_env_ptr; external;
procedure turkish_utf_8_close_env(z: SN_env_ptr); external;
function turkish_utf_8_stem(z: SN_env_ptr): C_int; external;

function turkish_utf_16_create_env: SN_env_16_ptr; external;
procedure turkish_utf_16_close_env(z: SN_env_16_ptr); external;
function turkish_utf_16_stem(z: SN_env_16_ptr): C_int; external;

destructor TYuStemmer.Destroy;
begin
  FClose(FEnv);
  inherited;
end;

function TYuStemmer.Stem(const s: AnsiString): AnsiString;
begin
  if SN_set_current(FEnv, Length(s), Pointer(s)) = 0 then
    if FStem(FEnv) >= 0 then
      begin
        with FEnv^ do
          SetString(Result, p, l);
        Exit;
      end;
  Assert(False);
  Result := s;
end;

procedure TYuStemmer.Stem(var p: PAnsiChar; var l: Integer);
begin
  if SN_set_current(FEnv, l, p) = 0 then
    if FStem(FEnv) >= 0 then
      begin
        p := FEnv^.p; l := FEnv^.l;
        Exit;
      end;
  Assert(False);
end;

function GetStemmer(const AName: AnsiString): TYuStemmer;
begin

  if AName = 'danish' then Result := TYuStemmer_Danish.Create else
    if AName = 'dutch' then Result := TYuStemmer_Dutch.Create else
      if AName = 'english' then Result := TYuStemmer_English.Create else
        if AName = 'finnish' then Result := TYuStemmer_Finnish.Create else
          if AName = 'french' then Result := TYuStemmer_French.Create else
            if AName = 'german' then Result := TYuStemmer_German.Create else
              if AName = 'german2' then Result := TYuStemmer_German2.Create else
                if AName = 'hungarian' then Result := TYuStemmer_Hungarian.Create else
                  if AName = 'italian' then Result := TYuStemmer_Italian.Create else
                    if AName = 'norwegian' then Result := TYuStemmer_Norwegian.Create else
                      if AName = 'porter' then Result := TYuStemmer_Porter.Create else
                        if AName = 'portuguese' then Result := TYuStemmer_Portuguese.Create else
                          if AName = 'romanian' then Result := TYuStemmer_Romanian.Create else
                            if AName = 'russian' then Result := TYuStemmer_Russian.Create else
                              if AName = 'spanish' then Result := TYuStemmer_Spanish.Create else
                                if AName = 'swedish' then Result := TYuStemmer_Swedish.Create else
                                  Result := nil;
end;

destructor TYuStemmer_8.Destroy;
begin
  FClose(FEnv);
  inherited;

end;

function TYuStemmer_8.Stem(const s: Utf8String): Utf8String;
begin
  if SN_set_current(FEnv, Length(s), Pointer(s)) = 0 then
    if FStem(FEnv) >= 0 then
      begin
        with FEnv^ do
          SetString(Result, p, l);
        Exit;
      end;
  Assert(False);
  Result := s;
end;

procedure TYuStemmer_8.Stem(var p: PUtf8Char; var l: Integer);
begin
  if SN_set_current(FEnv, l, p) = 0 then
    if FStem(FEnv) >= 0 then
      begin
        p := FEnv^.p; l := FEnv^.l;
        Exit;
      end;
  Assert(False);
end;

function GetStemmer_8(const AName: Utf8String): TYuStemmer_8;
begin

  if AName = 'danish' then Result := TYuStemmer_Danish_8.Create else
    if AName = 'dutch' then Result := TYuStemmer_Dutch_8.Create else
      if AName = 'english' then Result := TYuStemmer_English_8.Create else
        if AName = 'finnish' then Result := TYuStemmer_Finnish_8.Create else
          if AName = 'french' then Result := TYuStemmer_French_8.Create else
            if AName = 'german' then Result := TYuStemmer_German_8.Create else
              if AName = 'german2' then Result := TYuStemmer_German2_8.Create else
                if AName = 'hungarian' then Result := TYuStemmer_Hungarian_8.Create else
                  if AName = 'italian' then Result := TYuStemmer_Italian_8.Create else
                    if AName = 'norwegian' then Result := TYuStemmer_Norwegian_8.Create else
                      if AName = 'porter' then Result := TYuStemmer_Porter_8.Create else
                        if AName = 'portuguese' then Result := TYuStemmer_Portuguese_8.Create else
                          if AName = 'romanian' then Result := TYuStemmer_Romanian_8.Create else
                            if AName = 'russian' then Result := TYuStemmer_Russian_8.Create else
                              if AName = 'spanish' then Result := TYuStemmer_Spanish_8.Create else
                                if AName = 'swedish' then Result := TYuStemmer_Swedish_8.Create else
                                  if AName = 'turkish' then Result := TYuStemmer_Turkish_8.Create else
                                    Result := nil;
end;

destructor TYuStemmer_16.Destroy;
begin
  FClose(FEnv);
  inherited;

end;

function TYuStemmer_16.Stem(const s: UnicodeString): UnicodeString;
begin
  if SN_set_current_16(FEnv, Length(s), Pointer(s)) = 0 then
    if FStem(FEnv) >= 0 then
      begin
        with FEnv^ do
          SetString(Result, p, l);
        Exit;
      end;
  Assert(False);
  Result := s;
end;

procedure TYuStemmer_16.Stem(var p: PWideChar; var l: Integer);
begin
  if SN_set_current_16(FEnv, l, p) = 0 then
    if FStem(FEnv) >= 0 then
      begin
        p := FEnv^.p; l := FEnv^.l;
        Exit;
      end;
  Assert(False);
end;

function GetStemmer_16(const AName: UnicodeString): TYuStemmer_16;
begin

  if AName = 'danish' then Result := TYuStemmer_Danish_16.Create else
    if AName = 'dutch' then Result := TYuStemmer_Dutch_16.Create else
      if AName = 'english' then Result := TYuStemmer_English_16.Create else
        if AName = 'finnish' then Result := TYuStemmer_Finnish_16.Create else
          if AName = 'french' then Result := TYuStemmer_French_16.Create else
            if AName = 'german' then Result := TYuStemmer_German_16.Create else
              if AName = 'german2' then Result := TYuStemmer_German2_16.Create else
                if AName = 'hungarian' then Result := TYuStemmer_Hungarian_16.Create else
                  if AName = 'italian' then Result := TYuStemmer_Italian_16.Create else
                    if AName = 'norwegian' then Result := TYuStemmer_Norwegian_16.Create else
                      if AName = 'porter' then Result := TYuStemmer_Porter_16.Create else
                        if AName = 'portuguese' then Result := TYuStemmer_Portuguese_16.Create else
                          if AName = 'romanian' then Result := TYuStemmer_Romanian_16.Create else
                            if AName = 'russian' then Result := TYuStemmer_Russian_16.Create else
                              if AName = 'spanish' then Result := TYuStemmer_Spanish_16.Create else
                                if AName = 'swedish' then Result := TYuStemmer_Swedish_16.Create else
                                  if AName = 'turkish' then Result := TYuStemmer_Turkish_16.Create else
                                    Result := nil;
end;

constructor TYuStemmer_Danish.Create;
begin
  inherited;

  FStem := danish_iso_8859_1_stem;
  FEnv := danish_iso_8859_1_create_env;
  FClose := danish_iso_8859_1_close_env;
end;

constructor TYuStemmer_Danish_8.Create;
begin
  inherited;

  FStem := danish_utf_8_stem;
  FEnv := danish_utf_8_create_env;
  FClose := danish_utf_8_close_env;
end;

constructor TYuStemmer_Danish_16.Create;
begin
  inherited;

  FStem := danish_utf_16_stem;
  FEnv := danish_utf_16_create_env;
  FClose := danish_utf_16_close_env;
end;

constructor TYuStemmer_Dutch.Create;
begin
  inherited;

  FStem := dutch_iso_8859_1_stem;
  FEnv := dutch_iso_8859_1_create_env;
  FClose := dutch_iso_8859_1_close_env;
end;

constructor TYuStemmer_Dutch_8.Create;
begin
  inherited;

  FStem := dutch_utf_8_stem;
  FEnv := dutch_utf_8_create_env;
  FClose := dutch_utf_8_close_env;
end;

constructor TYuStemmer_Dutch_16.Create;
begin
  inherited;

  FStem := dutch_utf_16_stem;
  FEnv := dutch_utf_16_create_env;
  FClose := dutch_utf_16_close_env;
end;

constructor TYuStemmer_English.Create;
begin
  inherited;

  FStem := english_iso_8859_1_stem;
  FEnv := english_iso_8859_1_create_env;
  FClose := english_iso_8859_1_close_env;
end;

constructor TYuStemmer_English_8.Create;
begin
  inherited;

  FStem := english_utf_8_stem;
  FEnv := english_utf_8_create_env;
  FClose := english_utf_8_close_env;
end;

constructor TYuStemmer_English_16.Create;
begin
  inherited;

  FStem := english_utf_16_stem;
  FEnv := english_utf_16_create_env;
  FClose := english_utf_16_close_env;
end;

constructor TYuStemmer_Finnish.Create;
begin
  inherited;

  FStem := finnish_iso_8859_1_stem;
  FEnv := finnish_iso_8859_1_create_env;
  FClose := finnish_iso_8859_1_close_env;
end;

constructor TYuStemmer_Finnish_8.Create;
begin
  inherited;

  FStem := finnish_utf_8_stem;
  FEnv := finnish_utf_8_create_env;
  FClose := finnish_utf_8_close_env;
end;

constructor TYuStemmer_Finnish_16.Create;
begin
  inherited;

  FStem := finnish_utf_16_stem;
  FEnv := finnish_utf_16_create_env;
  FClose := finnish_utf_16_close_env;
end;

constructor TYuStemmer_French.Create;
begin
  inherited;

  FStem := french_iso_8859_1_stem;
  FEnv := french_iso_8859_1_create_env;
  FClose := french_iso_8859_1_close_env;
end;

constructor TYuStemmer_French_8.Create;
begin
  inherited;

  FStem := french_utf_8_stem;
  FEnv := french_utf_8_create_env;
  FClose := french_utf_8_close_env;
end;

constructor TYuStemmer_French_16.Create;
begin
  inherited;

  FStem := french_utf_16_stem;
  FEnv := french_utf_16_create_env;
  FClose := french_utf_16_close_env;
end;

constructor TYuStemmer_German.Create;
begin
  inherited;

  FStem := german_iso_8859_1_stem;
  FEnv := german_iso_8859_1_create_env;
  FClose := german_iso_8859_1_close_env;
end;

constructor TYuStemmer_German_8.Create;
begin
  inherited;

  FStem := german_utf_8_stem;
  FEnv := german_utf_8_create_env;
  FClose := german_utf_8_close_env;
end;

constructor TYuStemmer_German_16.Create;
begin
  inherited;

  FStem := german_UTF_16_stem;
  FEnv := german_UTF_16_create_env;
  FClose := german_UTF_16_close_env;
end;

constructor TYuStemmer_German2.Create;
begin
  inherited;

  FStem := german2_iso_8859_1_stem;
  FEnv := german2_iso_8859_1_create_env;
  FClose := german2_iso_8859_1_close_env;
end;

constructor TYuStemmer_German2_8.Create;
begin
  inherited;

  FStem := german2_utf_8_stem;
  FEnv := german2_utf_8_create_env;
  FClose := german2_utf_8_close_env;
end;

constructor TYuStemmer_German2_16.Create;
begin
  inherited;

  FStem := german2_utf_16_stem;
  FEnv := german2_utf_16_create_env;
  FClose := german2_utf_16_close_env;
end;

constructor TYuStemmer_Hungarian.Create;
begin
  inherited;

  FStem := hungarian_iso_8859_1_stem;
  FEnv := hungarian_iso_8859_1_create_env;
  FClose := hungarian_iso_8859_1_close_env;
end;

constructor TYuStemmer_Hungarian_8.Create;
begin
  inherited;

  FStem := hungarian_utf_8_stem;
  FEnv := hungarian_utf_8_create_env;
  FClose := hungarian_utf_8_close_env;
end;

constructor TYuStemmer_Hungarian_16.Create;
begin
  inherited;

  FStem := hungarian_utf_16_stem;
  FEnv := hungarian_utf_16_create_env;
  FClose := hungarian_utf_16_close_env;
end;

constructor TYuStemmer_Italian.Create;
begin
  inherited;

  FStem := italian_iso_8859_1_stem;
  FEnv := italian_iso_8859_1_create_env;
  FClose := italian_iso_8859_1_close_env;
end;

constructor TYuStemmer_Italian_8.Create;
begin
  inherited;

  FStem := italian_utf_8_stem;
  FEnv := italian_utf_8_create_env;
  FClose := italian_utf_8_close_env;
end;

constructor TYuStemmer_Italian_16.Create;
begin
  inherited;

  FStem := italian_utf_16_stem;
  FEnv := italian_utf_16_create_env;
  FClose := italian_utf_16_close_env;
end;

constructor TYuStemmer_Norwegian.Create;
begin
  inherited;

  FStem := norwegian_iso_8859_1_stem;
  FEnv := norwegian_iso_8859_1_create_env;
  FClose := norwegian_iso_8859_1_close_env;
end;

constructor TYuStemmer_Norwegian_8.Create;
begin
  inherited;

  FStem := norwegian_utf_8_stem;
  FEnv := norwegian_utf_8_create_env;
  FClose := norwegian_utf_8_close_env;
end;

constructor TYuStemmer_Norwegian_16.Create;
begin
  inherited;

  FStem := norwegian_utf_16_stem;
  FEnv := norwegian_utf_16_create_env;
  FClose := norwegian_utf_16_close_env;
end;

constructor TYuStemmer_Porter.Create;
begin
  inherited;

  FStem := porter_iso_8859_1_stem;
  FEnv := porter_iso_8859_1_create_env;
  FClose := porter_iso_8859_1_close_env;
end;

constructor TYuStemmer_Porter_8.Create;
begin
  inherited;

  FStem := porter_utf_8_stem;
  FEnv := porter_utf_8_create_env;
  FClose := porter_utf_8_close_env;
end;

constructor TYuStemmer_Porter_16.Create;
begin
  inherited;

  FStem := porter_utf_16_stem;
  FEnv := porter_utf_16_create_env;
  FClose := porter_utf_16_close_env;
end;

constructor TYuStemmer_Portuguese.Create;
begin
  inherited;

  FStem := portuguese_iso_8859_1_stem;
  FEnv := portuguese_iso_8859_1_create_env;
  FClose := portuguese_iso_8859_1_close_env;
end;

constructor TYuStemmer_Portuguese_8.Create;
begin
  inherited;

  FStem := portuguese_utf_8_stem;
  FEnv := portuguese_utf_8_create_env;
  FClose := portuguese_utf_8_close_env;
end;

constructor TYuStemmer_Portuguese_16.Create;
begin
  inherited;

  FStem := portuguese_utf_16_stem;
  FEnv := portuguese_utf_16_create_env;
  FClose := portuguese_utf_16_close_env;
end;

constructor TYuStemmer_Romanian.Create;
begin
  inherited;

  FStem := romanian_iso_8859_2_stem;
  FEnv := romanian_iso_8859_2_create_env;
  FClose := romanian_iso_8859_2_close_env;
end;

constructor TYuStemmer_Romanian_8.Create;
begin
  inherited;

  FStem := romanian_utf_8_stem;
  FEnv := romanian_utf_8_create_env;
  FClose := romanian_utf_8_close_env;
end;

constructor TYuStemmer_Romanian_16.Create;
begin
  inherited;

  FStem := romanian_utf_16_stem;
  FEnv := romanian_utf_16_create_env;
  FClose := romanian_utf_16_close_env;
end;

constructor TYuStemmer_Russian.Create;
begin
  inherited;

  FStem := russian_KOI8_R_stem;
  FEnv := russian_KOI8_R_create_env;
  FClose := russian_KOI8_R_close_env;
end;

constructor TYuStemmer_Russian_8.Create;
begin
  inherited;

  FStem := russian_utf_8_stem;
  FEnv := russian_utf_8_create_env;
  FClose := russian_utf_8_close_env;
end;

constructor TYuStemmer_Russian_16.Create;
begin
  inherited;

  FStem := russian_utf_16_stem;
  FEnv := russian_utf_16_create_env;
  FClose := russian_utf_16_close_env;
end;

constructor TYuStemmer_Spanish.Create;
begin
  inherited;

  FStem := spanish_iso_8859_1_stem;
  FEnv := spanish_iso_8859_1_create_env;
  FClose := spanish_iso_8859_1_close_env;
end;

constructor TYuStemmer_Spanish_8.Create;
begin
  inherited;

  FStem := spanish_utf_8_stem;
  FEnv := spanish_utf_8_create_env;
  FClose := spanish_utf_8_close_env;
end;

constructor TYuStemmer_Spanish_16.Create;
begin
  inherited;

  FStem := spanish_utf_16_stem;
  FEnv := spanish_utf_16_create_env;
  FClose := spanish_utf_16_close_env;
end;

constructor TYuStemmer_Swedish.Create;
begin
  inherited;

  FStem := swedish_iso_8859_1_stem;
  FEnv := swedish_iso_8859_1_create_env;
  FClose := swedish_iso_8859_1_close_env;
end;

constructor TYuStemmer_Swedish_8.Create;
begin
  inherited;

  FStem := swedish_utf_8_stem;
  FEnv := swedish_utf_8_create_env;
  FClose := swedish_utf_8_close_env;
end;

constructor TYuStemmer_Swedish_16.Create;
begin
  inherited;

  FStem := swedish_utf_16_stem;
  FEnv := swedish_utf_16_create_env;
  FClose := swedish_utf_16_close_env;
end;

constructor TYuStemmer_Turkish_8.Create;
begin
  inherited;

  FStem := turkish_utf_8_stem;
  FEnv := turkish_utf_8_create_env;
  FClose := turkish_utf_8_close_env;
end;

constructor TYuStemmer_Turkish_16.Create;
begin
  inherited;

  FStem := turkish_utf_16_stem;
  FEnv := turkish_utf_16_create_env;
  FClose := turkish_utf_16_close_env;
end;

  {$Z1}

function calloc(NElem: Cardinal; elsize: Cardinal): Pointer;
begin
  GetMem(Result, NElem * elsize);
  FillChar(Result^, NElem * elsize, 0);
end;

procedure Free(Block: C_void_ptr);
var
  MemMgr: System.{$IFDEF COMPILER_10_UP}TMemoryManagerEx{$ELSE}TMemoryManager{$ENDIF};
begin

  if Assigned(Block) then
    begin
      GetMemoryManager(MemMgr);
      MemMgr.FreeMem(Block);
    end;
end;

function malloc(Size: C_size_t): C_void_ptr;
var
  MemMgr: System.{$IFDEF COMPILER_10_UP}TMemoryManagerEx{$ELSE}TMemoryManager{$ENDIF};
begin
  if Size > 0 then
    begin

      GetMemoryManager(MemMgr);
      Result := MemMgr.GetMem(Size);
    end
  else
    Result := nil;
end;

function MemCmp(const s1: C_void_ptr; const s2: C_void_ptr; n: C_size_t): C_int;
label
  Success;
type
  TByte4 = packed record
    b0, b1, b2, b3: Byte;
  end;
  PByte4 = ^TByte4;
var
  p1, p2: PByte4;
  b1, b2: Byte;
begin
  p1 := s1;
  p2 := s2;
  if p1 = p2 then goto Success;

  repeat
    if n = 0 then goto Success;
    b1 := p1^.b0; b2 := p2^.b0;
    if b1 <> b2 then Break;
    Dec(n);

    if n = 0 then goto Success;
    b1 := p1^.b1; b2 := p2^.b1;
    if b1 <> b2 then Break;
    Dec(n);

    if n = 0 then goto Success;
    b1 := p1^.b2; b2 := p2^.b2;
    if b1 <> b2 then Break;
    Dec(n);

    if n = 0 then goto Success;
    b1 := p1^.b3; b2 := p2^.b3;
    if b1 <> b2 then Break;

    Inc(p1);
    Inc(p2);
    Dec(n);
  until False;

  Result := b1 - b2;
  Exit;

  Success:
  Result := 0;
end;

function memmove(Dest: C_void_ptr; Src: C_void_ptr; const n: C_size_t): C_void_ptr;
begin
  Result := Dest;
  Move(Src^, Result^, n);
end;

function realloc(Block: C_void_ptr; Size: C_size_t): C_void_ptr;
var
  MemMgr: System.{$IFDEF COMPILER_10_UP}TMemoryManagerEx{$ELSE}TMemoryManager{$ENDIF};
begin
  Result := Block;
  if System.Assigned(Result) then
    begin
      GetMemoryManager(MemMgr);
      if Size > 0 then
        begin
          Result := MemMgr.ReallocMem(Result, Size);
        end
      else
        begin
          MemMgr.FreeMem(Result);
          Result := nil;
        end;
    end
  else
    Result := malloc(Size);
end;

{$L obj\stem_danish_iso_8859_1}
{$L obj\stem_danish_utf_8}
{$L obj\stem_danish_utf_16}

{$L obj\stem_dutch_iso_8859_1}
{$L obj\stem_dutch_utf_8}
{$L obj\stem_dutch_utf_16}

{$L obj\stem_english_iso_8859_1}
{$L obj\stem_english_utf_8}
{$L obj\stem_english_utf_16}

{$L obj\stem_finnish_iso_8859_1}
{$L obj\stem_finnish_utf_8}
{$L obj\stem_finnish_utf_16}

{$L obj\stem_french_iso_8859_1}
{$L obj\stem_french_utf_8}
{$L obj\stem_french_utf_16}

{$L obj\stem_german_iso_8859_1}
{$L obj\stem_german_utf_8}
{$L obj\stem_german_utf_16}

{$L obj\stem_german2_iso_8859_1}
{$L obj\stem_german2_utf_8}
{$L obj\stem_german2_utf_16}

{$L obj\stem_hungarian_iso_8859_1}
{$L obj\stem_hungarian_utf_8}
{$L obj\stem_hungarian_utf_16}

{$L obj\stem_italian_iso_8859_1}
{$L obj\stem_italian_utf_8}
{$L obj\stem_italian_utf_16}

{$L obj\stem_norwegian_iso_8859_1}
{$L obj\stem_norwegian_utf_8}
{$L obj\stem_norwegian_utf_16}

{$L obj\stem_porter_iso_8859_1}
{$L obj\stem_porter_utf_8}
{$L obj\stem_porter_utf_16}

{$L obj\stem_portuguese_iso_8859_1}
{$L obj\stem_portuguese_utf_8}
{$L obj\stem_portuguese_utf_16}

{$L obj\stem_romanian_iso_8859_2}
{$L obj\stem_romanian_utf_8}
{$L obj\stem_romanian_utf_16}

{$L obj\stem_russian_KOI8_R}
{$L obj\stem_russian_utf_8}
{$L obj\stem_russian_utf_16}

{$L obj\stem_spanish_iso_8859_1}
{$L obj\stem_spanish_utf_8}
{$L obj\stem_spanish_utf_16}

{$L obj\stem_swedish_iso_8859_1}
{$L obj\stem_swedish_utf_8}
{$L obj\stem_swedish_utf_16}

{$L obj\stem_turkish_utf_8}
{$L obj\stem_turkish_utf_16}

{$L obj\api}
{$L obj\utilities}

{$L obj\api16}
{$L obj\utilities16}

end.

