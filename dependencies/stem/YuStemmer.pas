{-------------------------------------------------------------------------------
 
 Copyright (c) 1999-2016 Ralf Junker, Yunqa
 Internet: http://www.yunqa.de
 E-Mail:   delphi@yunqa.de

-------------------------------------------------------------------------------}

unit YuStemmer;

{$I DICompilers.inc}
{$A+}
{$Z+}

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

function GetStemmer(const Name: AnsiString): TYuStemmer;

const

  STEMMER_LIST: array[0..19] of AnsiString = (
    'basque',
    'catalan',
    'czech',
    'danish',
    'dutch',
    'english',
    'finnish',
    'french',
    'german',
    'german2',
    'hungarian',
    'irish',
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

    function calc(const s: String): String;

    function Stem(const s: Utf8String): Utf8String; overload;

    procedure Stem(var p: PUtf8Char; var l: Integer); overload;
  end;

function GetStemmer_8(const Name: Utf8String): TYuStemmer_8;

const

  STEMMER_LIST_8: array[0..21] of Utf8String = (
    'armenian',
    'basque',
    'catalan',
    'czech',
    'danish',
    'dutch',
    'english',
    'finnish',
    'french',
    'german',
    'german2',
    'hungarian',
    'irish',
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

function GetStemmer_16(const Name: UnicodeString): TYuStemmer_16;

const

  STEMMER_LIST_16: array[0..21] of UnicodeString = (
    'armenian',
    'basque',
    'catalan',
    'czech',
    'danish',
    'dutch',
    'english',
    'finnish',
    'french',
    'german',
    'german2',
    'hungarian',
    'irish',
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

  TYuStemmer_Armenian_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Armenian_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Basque = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Basque_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Basque_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Catalan = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Catalan_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Catalan_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

  TYuStemmer_Czech = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Czech_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Czech_16 = class(TYuStemmer_16)
  public
    constructor Create;
  end;

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

  TYuStemmer_Irish = class(TYuStemmer)
  public
    constructor Create;
  end;

  TYuStemmer_Irish_8 = class(TYuStemmer_8)
  public
    constructor Create;
  end;

  TYuStemmer_Irish_16 = class(TYuStemmer_16)
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

  C___int8 = System.ShortInt;
  C___int16 = System.SmallInt;
  C___int32 = System.Integer;
  C___int64 = System.Int64;

  C___uint8 = System.Byte;
  C___uint16 = System.Word;
  C___uint32 = System.Cardinal;
  C___uint64 = System.{$IFDEF SUPPORTS_UINT64}UInt64{$ELSE}Int64{$ENDIF};

  C_char = System.AnsiChar;

  C_char_ptr = System.PAnsiChar;
  C_char_ptr_ptr = ^C_char_ptr;
  C_char_ptr_array = array[0..MaxInt div SizeOf(C_char_ptr) - 1] of C_char_ptr;
  C_char_ptr_array_ptr = ^C_char_ptr_array;
  C_char_num = System.ShortInt;

  C_char_num_ptr = ^C_char_num;
  C_double = System.Double;
  C_double_ptr = ^C_double;
  C_float = System.Single;
  C_float_ptr = ^C_float;

  C_int = System.Integer;
  C_int_ptr = ^C_int;
  C_int_array = array[0..MaxInt div SizeOf(C_int) - 1] of C_int;
  C_int_array_ptr = ^C_int_array;

  C_int8_t = C___int8;

  C_int8_t_ptr = ^C_int8_t;
  C_int8_t_array = array[0..MaxInt div SizeOf(C_int8_t) - 1] of C_int8_t;
  C_int8_t_array_ptr = ^C_int8_t_array;

  C_int16_t = C___int16;

  C_int16_t_ptr = ^C_int16_t;
  C_int16_t_array = array[0..MaxInt div SizeOf(C_int16_t) - 1] of C_int16_t;
  C_int16_t_array_ptr = ^C_int16_t_array;

  C_int32_t = C___int32;

  C_int32_t_ptr = ^C_int32_t;
  C_int32_t_array = array[0..MaxInt div SizeOf(C_int32_t) - 1] of C_int32_t;
  C_int32_t_array_ptr = ^C_int32_t_array;

  C_int64_t = C___int64;

  C_int64_t_ptr = ^C_int64_t;
  C_int64_t_array = array[0..MaxInt div SizeOf(C_int64_t) - 1] of C_int64_t;
  C_int64_t_array_ptr = ^C_int64_t_array;

  C_long = System.Integer;
  C_long_ptr = ^C_long;
  C_long_long = System.Int64;
  C_long_long_ptr = ^C_long_long;
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
  C_signed_long_long = System.Int64;
  C_signed_long_long_ptr = ^C_signed_long_long;
  C_signed_short = System.SmallInt;

  C_unsigned = System.Cardinal;
  C_unsigned_ptr = ^C_unsigned;

  C_unsigned_char = System.AnsiChar;

  C_unsigned_char_array = array[0..MaxInt div SizeOf(C_unsigned_char) - 1] of C_unsigned_char;

  C_unsigned_char_array_ptr = ^C_unsigned_char_array;

  C_unsigned_char_ptr = System.PAnsiChar;

  C_unsigned_char_ptr_array = array[0..MaxInt div SizeOf(C_unsigned_char_ptr) - 1] of C_unsigned_char_ptr;

  C_unsigned_char_ptr_array_ptr = ^C_unsigned_char_ptr_array;

  C_unsigned_char_ptr_ptr = ^C_unsigned_char_ptr;

  C_unsigned_char_num = System.Byte;

  C_unsigned_char_num_array = array[0..MaxInt div SizeOf(C_unsigned_char_num) - 1] of C_unsigned_char_num;

  C_unsigned_char_num_array_ptr = ^C_unsigned_char_num_array;

  C_unsigned_char_num_array_ptr_ptr = ^C_unsigned_char_num_array_ptr;

  C_unsigned_char_num_ptr = ^C_unsigned_char_num;

  C_unsigned_int = System.Cardinal;
  C_unsigned_int_ptr = ^C_unsigned_int;
  C_unsigned___int64 = C___uint64;
  C_unsigned_long = System.Cardinal;
  C_unsigned_long_int = System.Integer;
  C_unsigned_long_long = System.{$IFDEF SUPPORTS_UINT64}UInt64{$ELSE}Int64{$ENDIF};
  C_unsigned_long_ptr = ^C_unsigned_long;
  C_unsigned_short = System.Word;
  C_unsigned_short_ptr = ^C_unsigned_short;
  C_unsigned_short_int = System.Word;
  C_unsigned_short_int_ptr = ^C_unsigned_short_int;

  C_errno_t = C_int;

  C_ptrdiff_t = {$IFDEF CPUX64}C___int64{$ELSE}C___int32{$ENDIF};

  C_size_t = {$IFDEF CPUX64}C___uint64{$ELSE}C___uint32{$ENDIF};
  C_size_t_ptr = ^C_size_t;
  C_size_t_array = array[0..MaxInt div SizeOf(C_size_t) - 1] of C_size_t;
  C_size_t_array_ptr = ^C_size_t_array;

  C_ssize_t = {$IFDEF CPUX64}C___int64{$ELSE}C_int{$ENDIF};
  C_ssize_t_ptr = ^C_ssize_t;
  C_ssize_t_array = array[0..MaxInt div SizeOf(C_ssize_t) - 1] of C_ssize_t;
  C_ssize_t_array_ptr = ^C_ssize_t_array;

  C_uint8_t = C___uint8;

  C_uint8_t_ptr = ^C_uint8_t;
  C_uint8_t_ptr_ptr = ^C_uint8_t_ptr;
  C_uint8_t_array = array[0..MaxInt div SizeOf(C_uint8_t) - 1] of C_uint8_t;
  C_uint8_t_array_ptr = ^C_uint8_t_array;

  C_uint16_t = C___uint16;

  C_uint16_t_ptr = ^C_uint16_t;
  C_uint16_t_array = array[0..MaxInt div SizeOf(C_uint16_t) - 1] of C_uint16_t;
  C_uint16_t_array_ptr = ^C_uint16_t_array;

  C_uint32_t = C___uint32;

  C_uint32_t_ptr = ^C_uint32_t;
  C_uint32_t_array = array[0..MaxInt div SizeOf(C_uint32_t) - 1] of C_uint32_t;
  C_uint32_t_array_ptr = ^C_uint32_t_array;

  C_uint64_t = C___uint64;

  C_uint64_t_ptr = ^C_uint64_t;
  C_uint64_t_array = array[0..MaxInt div SizeOf(C_uint64_t) - 1] of C_uint64_t;
  C_uint64_t_array_ptr = ^C_uint64_t_array;

  C_void_ptr = System.Pointer;
  C_void_ptr_ptr = ^C_void_ptr;
  C_void_ptr_array = array[0..MaxInt div SizeOf(C_void_ptr) - 1] of C_void_ptr;
  C_void_ptr_array_ptr = ^C_void_ptr_array;

  C_wchar_t = System.WideChar;

  C_wint_t = C_wchar_t;

  C_wchar_t_ptr = System.PWideChar;
  C_wchar_t_ptr_ptr = ^C_wchar_t_ptr;

{$IFDEF MSWINDOWS}

{$ENDIF MSWINDOWS}

{$IFDEF MsWindows}

{$ENDIF MsWindows}

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

function armenian_utf_8_create_env: SN_env_ptr; external;
procedure armenian_utf_8_close_env(z: SN_env_ptr); external;
function armenian_utf_8_stem(z: SN_env_ptr): C_int; external;

function armenian_utf_16_create_env: SN_env_16_ptr; external;
procedure armenian_utf_16_close_env(z: SN_env_16_ptr); external;
function armenian_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function basque_iso_8859_1_create_env: SN_env_ptr; external;
procedure basque_iso_8859_1_close_env(z: SN_env_ptr); external;
function basque_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function basque_utf_8_create_env: SN_env_ptr; external;
procedure basque_utf_8_close_env(z: SN_env_ptr); external;
function basque_utf_8_stem(z: SN_env_ptr): C_int; external;

function basque_utf_16_create_env: SN_env_16_ptr; external;
procedure basque_utf_16_close_env(z: SN_env_16_ptr); external;
function basque_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function catalan_iso_8859_1_create_env: SN_env_ptr; external;
procedure catalan_iso_8859_1_close_env(z: SN_env_ptr); external;
function catalan_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function catalan_utf_8_create_env: SN_env_ptr; external;
procedure catalan_utf_8_close_env(z: SN_env_ptr); external;
function catalan_utf_8_stem(z: SN_env_ptr): C_int; external;

function catalan_utf_16_create_env: SN_env_16_ptr; external;
procedure catalan_utf_16_close_env(z: SN_env_16_ptr); external;
function catalan_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function czech_iso_8859_2_create_env: SN_env_ptr; external;
procedure czech_iso_8859_2_close_env(z: SN_env_ptr); external;
function czech_iso_8859_2_stem(z: SN_env_ptr): C_int; external;

function czech_utf_8_create_env: SN_env_ptr; external;
procedure czech_utf_8_close_env(z: SN_env_ptr); external;
function czech_utf_8_stem(z: SN_env_ptr): C_int; external;

function czech_utf_16_create_env: SN_env_16_ptr; external;
procedure czech_utf_16_close_env(z: SN_env_16_ptr); external;
function czech_utf_16_stem(z: SN_env_16_ptr): C_int; external;

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

function hungarian_iso_8859_2_create_env: SN_env_ptr; external;
procedure hungarian_iso_8859_2_close_env(z: SN_env_ptr); external;
function hungarian_iso_8859_2_stem(z: SN_env_ptr): C_int; external;

function hungarian_utf_8_create_env: SN_env_ptr; external;
procedure hungarian_utf_8_close_env(z: SN_env_ptr); external;
function hungarian_utf_8_stem(z: SN_env_ptr): C_int; external;

function hungarian_utf_16_create_env: SN_env_16_ptr; external;
procedure hungarian_utf_16_close_env(z: SN_env_16_ptr); external;
function hungarian_utf_16_stem(z: SN_env_16_ptr): C_int; external;

function irish_iso_8859_1_create_env: SN_env_ptr; external;
procedure irish_iso_8859_1_close_env(z: SN_env_ptr); external;
function irish_iso_8859_1_stem(z: SN_env_ptr): C_int; external;

function irish_utf_8_create_env: SN_env_ptr; external;
procedure irish_utf_8_close_env(z: SN_env_ptr); external;
function irish_utf_8_stem(z: SN_env_ptr): C_int; external;

function irish_utf_16_create_env: SN_env_16_ptr; external;
procedure irish_utf_16_close_env(z: SN_env_16_ptr); external;
function irish_utf_16_stem(z: SN_env_16_ptr): C_int; external;

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

function GetStemmer(const Name: AnsiString): TYuStemmer;
begin

  if Name = 'basque' then Result := TYuStemmer_Basque.Create
  else if Name = 'catalan' then Result := TYuStemmer_Catalan.Create
  else if Name = 'czech' then Result := TYuStemmer_Czech.Create
  else if Name = 'danish' then Result := TYuStemmer_Danish.Create
  else if Name = 'dutch' then Result := TYuStemmer_Dutch.Create
  else if Name = 'english' then Result := TYuStemmer_English.Create
  else if Name = 'finnish' then Result := TYuStemmer_Finnish.Create
  else if Name = 'french' then Result := TYuStemmer_French.Create
  else if Name = 'german' then Result := TYuStemmer_German.Create
  else if Name = 'german2' then Result := TYuStemmer_German2.Create
  else if Name = 'hungarian' then Result := TYuStemmer_Hungarian.Create
  else if Name = 'irish' then Result := TYuStemmer_Irish.Create
  else if Name = 'italian' then Result := TYuStemmer_Italian.Create
  else if Name = 'norwegian' then Result := TYuStemmer_Norwegian.Create
  else if Name = 'porter' then Result := TYuStemmer_Porter.Create
  else if Name = 'portuguese' then Result := TYuStemmer_Portuguese.Create
  else if Name = 'romanian' then Result := TYuStemmer_Romanian.Create
  else if Name = 'russian' then Result := TYuStemmer_Russian.Create
  else if Name = 'spanish' then Result := TYuStemmer_Spanish.Create
  else if Name = 'swedish' then Result := TYuStemmer_Swedish.Create
  else Result := nil;
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

function GetStemmer_8(const Name: Utf8String): TYuStemmer_8;
begin

  if Name = 'armenian' then Result := TYuStemmer_Armenian_8.Create
  else if Name = 'basque' then Result := TYuStemmer_Basque_8.Create
  else if Name = 'catalan' then Result := TYuStemmer_Catalan_8.Create
  else if Name = 'czech' then Result := TYuStemmer_Czech_8.Create
  else if Name = 'danish' then Result := TYuStemmer_Danish_8.Create
  else if Name = 'dutch' then Result := TYuStemmer_Dutch_8.Create
  else if Name = 'english' then Result := TYuStemmer_English_8.Create
  else if Name = 'finnish' then Result := TYuStemmer_Finnish_8.Create
  else if Name = 'french' then Result := TYuStemmer_French_8.Create
  else if Name = 'german' then Result := TYuStemmer_German_8.Create
  else if Name = 'german2' then Result := TYuStemmer_German2_8.Create
  else if Name = 'hungarian' then Result := TYuStemmer_Hungarian_8.Create
  else if Name = 'irish' then Result := TYuStemmer_Irish_8.Create
  else if Name = 'italian' then Result := TYuStemmer_Italian_8.Create
  else if Name = 'norwegian' then Result := TYuStemmer_Norwegian_8.Create
  else if Name = 'porter' then Result := TYuStemmer_Porter_8.Create
  else if Name = 'portuguese' then Result := TYuStemmer_Portuguese_8.Create
  else if Name = 'romanian' then Result := TYuStemmer_Romanian_8.Create
  else if Name = 'russian' then Result := TYuStemmer_Russian_8.Create
  else if Name = 'spanish' then Result := TYuStemmer_Spanish_8.Create
  else if Name = 'swedish' then Result := TYuStemmer_Swedish_8.Create
  else if Name = 'turkish' then Result := TYuStemmer_Turkish_8.Create
  else Result := nil;
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

function TYuStemmer_8.calc(const s: String): String;
begin
  result := String(Stem(UTF8String(s)));
end;

function GetStemmer_16(const Name: UnicodeString): TYuStemmer_16;
begin

  if Name = 'armenian' then Result := TYuStemmer_Armenian_16.Create
  else if Name = 'basque' then Result := TYuStemmer_Basque_16.Create
  else if Name = 'catalan' then Result := TYuStemmer_Catalan_16.Create
  else if Name = 'czech' then Result := TYuStemmer_Czech_16.Create
  else if Name = 'danish' then Result := TYuStemmer_Danish_16.Create
  else if Name = 'dutch' then Result := TYuStemmer_Dutch_16.Create
  else if Name = 'english' then Result := TYuStemmer_English_16.Create
  else if Name = 'finnish' then Result := TYuStemmer_Finnish_16.Create
  else if Name = 'french' then Result := TYuStemmer_French_16.Create
  else if Name = 'german' then Result := TYuStemmer_German_16.Create
  else if Name = 'german2' then Result := TYuStemmer_German2_16.Create
  else if Name = 'hungarian' then Result := TYuStemmer_Hungarian_16.Create
  else if Name = 'irish' then Result := TYuStemmer_Irish_16.Create
  else if Name = 'italian' then Result := TYuStemmer_Italian_16.Create
  else if Name = 'norwegian' then Result := TYuStemmer_Norwegian_16.Create
  else if Name = 'porter' then Result := TYuStemmer_Porter_16.Create
  else if Name = 'portuguese' then Result := TYuStemmer_Portuguese_16.Create
  else if Name = 'romanian' then Result := TYuStemmer_Romanian_16.Create
  else if Name = 'russian' then Result := TYuStemmer_Russian_16.Create
  else if Name = 'spanish' then Result := TYuStemmer_Spanish_16.Create
  else if Name = 'swedish' then Result := TYuStemmer_Swedish_16.Create
  else if Name = 'turkish' then Result := TYuStemmer_Turkish_16.Create
  else Result := nil;
end;

constructor TYuStemmer_Armenian_8.Create;
begin
  inherited;

  FStem := armenian_utf_8_stem;
  FEnv := armenian_utf_8_create_env;
  FClose := armenian_utf_8_close_env;
end;

constructor TYuStemmer_Armenian_16.Create;
begin
  inherited;

  FStem := armenian_utf_16_stem;
  FEnv := armenian_utf_16_create_env;
  FClose := armenian_utf_16_close_env;
end;

constructor TYuStemmer_Basque.Create;
begin
  inherited;

  FStem := basque_iso_8859_1_stem;
  FEnv := basque_iso_8859_1_create_env;
  FClose := basque_iso_8859_1_close_env;
end;

constructor TYuStemmer_Basque_8.Create;
begin
  inherited;

  FStem := basque_utf_8_stem;
  FEnv := basque_utf_8_create_env;
  FClose := basque_utf_8_close_env;
end;

constructor TYuStemmer_Basque_16.Create;
begin
  inherited;

  FStem := basque_utf_16_stem;
  FEnv := basque_utf_16_create_env;
  FClose := basque_utf_16_close_env;
end;

constructor TYuStemmer_Catalan.Create;
begin
  inherited;

  FStem := catalan_iso_8859_1_stem;
  FEnv := catalan_iso_8859_1_create_env;
  FClose := catalan_iso_8859_1_close_env;
end;

constructor TYuStemmer_Catalan_8.Create;
begin
  inherited;

  FStem := catalan_utf_8_stem;
  FEnv := catalan_utf_8_create_env;
  FClose := catalan_utf_8_close_env;
end;

constructor TYuStemmer_Catalan_16.Create;
begin
  inherited;

  FStem := catalan_utf_16_stem;
  FEnv := catalan_utf_16_create_env;
  FClose := catalan_utf_16_close_env;
end;

constructor TYuStemmer_Czech.Create;
begin
  inherited;

  FStem := czech_iso_8859_2_stem;
  FEnv := czech_iso_8859_2_create_env;
  FClose := czech_iso_8859_2_close_env;
end;

constructor TYuStemmer_Czech_8.Create;
begin
  inherited;

  FStem := czech_utf_8_stem;
  FEnv := czech_utf_8_create_env;
  FClose := czech_utf_8_close_env;
end;

constructor TYuStemmer_Czech_16.Create;
begin
  inherited;

  FStem := czech_utf_16_stem;
  FEnv := czech_utf_16_create_env;
  FClose := czech_utf_16_close_env;
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

  FStem := hungarian_iso_8859_2_stem;
  FEnv := hungarian_iso_8859_2_create_env;
  FClose := hungarian_iso_8859_2_close_env;
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

constructor TYuStemmer_Irish.Create;
begin
  inherited;

  FStem := irish_iso_8859_1_stem;
  FEnv := irish_iso_8859_1_create_env;
  FClose := irish_iso_8859_1_close_env;
end;

constructor TYuStemmer_Irish_8.Create;
begin
  inherited;

  FStem := irish_utf_8_stem;
  FEnv := irish_utf_8_create_env;
  FClose := irish_utf_8_close_env;
end;

constructor TYuStemmer_Irish_16.Create;
begin
  inherited;

  FStem := irish_utf_16_stem;
  FEnv := irish_utf_16_create_env;
  FClose := irish_utf_16_close_env;
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

function calloc(NElem: C_size_t; elsize: C_size_t): C_void_ptr;
var
  lBytes: C_size_t;
  MemMgr: System.{$IFDEF COMPILER_10_UP}TMemoryManagerEx{$ELSE}TMemoryManager{$ENDIF};
begin
  lBytes := NElem * elsize;
  if lBytes > 0 then
    begin

      GetMemoryManager(MemMgr);
      {$IFDEF COMPILER_10_UP}
      Result := MemMgr.AllocMem(lBytes);
      {$ELSE COMPILER_10_UP}
      Result := MemMgr.GetMem(lBytes);
      FillChar(Result^, lBytes, 0);
      {$ENDIF COMPILER_10_UP}
    end
  else
    Result := nil;
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

function memmove(Dest: C_void_ptr; const Src: C_void_ptr; n: C_size_t): C_void_ptr;
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

{$IFDEF MSWINDOWS}

{$ENDIF MSWINDOWS}

{$IFDEF MsWindows}

{$ENDIF MsWindows}

{$IFDEF CPUX64}{$L stemmer_win64\stem_armenian_utf_8}{$ELSE}{$L stemmer_win32\stem_armenian_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_armenian_utf_16}{$ELSE}{$L stemmer_win32\stem_armenian_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_basque_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_basque_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_basque_utf_8}{$ELSE}{$L stemmer_win32\stem_basque_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_basque_utf_16}{$ELSE}{$L stemmer_win32\stem_basque_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_catalan_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_catalan_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_catalan_utf_8}{$ELSE}{$L stemmer_win32\stem_catalan_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_catalan_utf_16}{$ELSE}{$L stemmer_win32\stem_catalan_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_czech_iso_8859_2}{$ELSE}{$L stemmer_win32\stem_czech_iso_8859_2}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_czech_utf_8}{$ELSE}{$L stemmer_win32\stem_czech_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_czech_utf_16}{$ELSE}{$L stemmer_win32\stem_czech_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_danish_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_danish_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_danish_utf_8}{$ELSE}{$L stemmer_win32\stem_danish_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_danish_utf_16}{$ELSE}{$L stemmer_win32\stem_danish_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_dutch_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_dutch_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_dutch_utf_8}{$ELSE}{$L stemmer_win32\stem_dutch_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_dutch_utf_16}{$ELSE}{$L stemmer_win32\stem_dutch_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_english_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_english_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_english_utf_8}{$ELSE}{$L stemmer_win32\stem_english_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_english_utf_16}{$ELSE}{$L stemmer_win32\stem_english_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_finnish_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_finnish_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_finnish_utf_8}{$ELSE}{$L stemmer_win32\stem_finnish_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_finnish_utf_16}{$ELSE}{$L stemmer_win32\stem_finnish_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_french_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_french_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_french_utf_8}{$ELSE}{$L stemmer_win32\stem_french_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_french_utf_16}{$ELSE}{$L stemmer_win32\stem_french_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_german_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_german_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_german_utf_8}{$ELSE}{$L stemmer_win32\stem_german_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_german_utf_16}{$ELSE}{$L stemmer_win32\stem_german_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_german2_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_german2_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_german2_utf_8}{$ELSE}{$L stemmer_win32\stem_german2_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_german2_utf_16}{$ELSE}{$L stemmer_win32\stem_german2_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_hungarian_iso_8859_2}{$ELSE}{$L stemmer_win32\stem_hungarian_iso_8859_2}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_hungarian_utf_8}{$ELSE}{$L stemmer_win32\stem_hungarian_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_hungarian_utf_16}{$ELSE}{$L stemmer_win32\stem_hungarian_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_irish_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_irish_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_irish_utf_8}{$ELSE}{$L stemmer_win32\stem_irish_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_irish_utf_16}{$ELSE}{$L stemmer_win32\stem_irish_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_italian_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_italian_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_italian_utf_8}{$ELSE}{$L stemmer_win32\stem_italian_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_italian_utf_16}{$ELSE}{$L stemmer_win32\stem_italian_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_norwegian_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_norwegian_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_norwegian_utf_8}{$ELSE}{$L stemmer_win32\stem_norwegian_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_norwegian_utf_16}{$ELSE}{$L stemmer_win32\stem_norwegian_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_porter_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_porter_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_porter_utf_8}{$ELSE}{$L stemmer_win32\stem_porter_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_porter_utf_16}{$ELSE}{$L stemmer_win32\stem_porter_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_portuguese_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_portuguese_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_portuguese_utf_8}{$ELSE}{$L stemmer_win32\stem_portuguese_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_portuguese_utf_16}{$ELSE}{$L stemmer_win32\stem_portuguese_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_romanian_iso_8859_2}{$ELSE}{$L stemmer_win32\stem_romanian_iso_8859_2}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_romanian_utf_8}{$ELSE}{$L stemmer_win32\stem_romanian_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_romanian_utf_16}{$ELSE}{$L stemmer_win32\stem_romanian_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_russian_KOI8_R}{$ELSE}{$L stemmer_win32\stem_russian_KOI8_R}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_russian_utf_8}{$ELSE}{$L stemmer_win32\stem_russian_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_russian_utf_16}{$ELSE}{$L stemmer_win32\stem_russian_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_spanish_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_spanish_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_spanish_utf_8}{$ELSE}{$L stemmer_win32\stem_spanish_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_spanish_utf_16}{$ELSE}{$L stemmer_win32\stem_spanish_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_swedish_iso_8859_1}{$ELSE}{$L stemmer_win32\stem_swedish_iso_8859_1}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_swedish_utf_8}{$ELSE}{$L stemmer_win32\stem_swedish_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_swedish_utf_16}{$ELSE}{$L stemmer_win32\stem_swedish_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\stem_turkish_utf_8}{$ELSE}{$L stemmer_win32\stem_turkish_utf_8}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\stem_turkish_utf_16}{$ELSE}{$L stemmer_win32\stem_turkish_utf_16}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\api}{$ELSE}{$L stemmer_win32\api}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\utilities}{$ELSE}{$L stemmer_win32\utilities}{$ENDIF}

{$IFDEF CPUX64}{$L stemmer_win64\api16}{$ELSE}{$L stemmer_win32\api16}{$ENDIF}
{$IFDEF CPUX64}{$L stemmer_win64\utilities16}{$ELSE}{$L stemmer_win32\utilities16}{$ENDIF}

end.

