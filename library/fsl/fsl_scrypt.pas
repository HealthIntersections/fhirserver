unit fsl_scrypt;

{$I fhir.inc}


(*
  Sample Usage
  ============

    //Hash a password
    hash := TSCrypt.HashPassword('correct horse battery staple');

    //Check if password matches previous hash
    isValid := TScrypt.CheckPassword('correct horse battery staple', hash);

    //Derive an encryption key from a password
    secretKey := TScrypt.GetBytes('correct horse battery staple', 'seasalt', 16); //returns 16 bytes (128 bits)
    secretKey := TScrypt.GetBytes('correct horse battery staple', 'seasalt', {r=}1, {N=}128}, {p=}8, 32); //returns 32 bytes (256 bits)



  Remarks
  =======

  scrypt is a key-derivation function.
  Key derivation functions are used to derive an encryption key from a password.

  To generate 16 bytes (128 bits) of key material, with automatically determined parameters, use:

    secretKey := TScrypt.GetBytes('correct horse battery staple', 'seasalt', 16); //returns 16 bytes (128 bits)

  If you know what values of the scrypt N (CostFactor), r (block size), and p (parallelization factor) parameters you want,
  you can specify them:

      secretKey := TScrypt.GetBytes('correct horse battery staple', 'seasalt', {N=}14, {r=}8, {p=}1, 32); //returns 32 bytes (256 bits)

  where
      BlockSize (r)             = 8
      CostFactor (N)            = 14 (i.e. 2^14 = 16,384 iterations)
      ParallelizationFactor (p) = 1
      DesiredBytes              = 32 (256 bits)

  Otherwise scrypt does a speed/memory test to determine the most appropriate parameters.

  Password Hashing
  ================

  SCrypt has also been used as password hashing algorithm.
  In order to make password storage easier, we generate the salt and store it with the returned string.
  This is similar to what OpenBSD has done with BCrypt.
  The downside is that there is no standard format out there for SCrypt representation of password hashes.

    hash := TSCrypt.HashPassword('correct horse battery staple');

  will return string in the format of:

    $s0$NNNNrrpp$salt$key

      s0     - version 0 of the format with 128-bit salt and 256-bit derived key
      params - 32-bit hex integer containing log2(N) (16 bits), r (8 bits), and p (8 bits)
      salt   - 24 base64-encoded characters of salt (128 bits ==> 16 bytes ==> 24 characters)
      key    - 44 base64-encoded characters derived key (256 bits ==> 32 bytes ==> 44 characters)

  Example for password of "secret":

    $s0$e0801$epIxT/h6HbbwHaehFnh/bw==$7H0vsXlY8UxxyW/BWx/9GuY7jEvGjT71GFd6O4SZND0=

      CostFactor = 0xE0 = 14 ==> N = 2^14 = 16,384
      r          = 0x08 = 8
      p          = 0x01 = 1
      salt       = epIxT/h6HbbwHaehFnh/bw==
      key        = 7H0vsXlY8UxxyW/BWx/9GuY7jEvGjT71GFd6O4SZND0=

  Version History
  ===============

  Version 1.5   20160603
      - Change: CheckPassword to take a boolean out parameter "PasswordRehashNeeded". PasswordRehashNeeded will contain true
        if you should call HashPassword again while you have the user's password handy.
        If computing power can now compute the hash in less than 250ms, it's time to use rehash te password.
        Or if the scrypt version or hash string format has changed, and we want you to use HashPassword to get a new format.
      - Added: HashPassword (the version where you let scrypt determine the parameters) will now do a mini benchmark
        (like Bcrypt for Delphi and canonical tarsnap scrypt does). The benchmark will try to ensure that scrypt
        runs in no less than 250ms (ideally 500ms). But under no circumstances will the benchmark drop below the
        default N=14,r=8,p=1. If the benchmark says that will take over 500ms to run, then that's the way it will be.
        The benchmark will only ever *increase* runtime over the default, never decrease it.

  Version 1.4   20160528
      - Switched password hashing Base64 to use standard Base64 rather than OpenBSD's custom Base64 that BCrypt uses
      - Hashing now compatible with wg/scrypt (added test to match their documented example)
      - Standard base64 hashing keeps trailing == padding (BCrypt's custom Base64 stripped it)
      - Removed unused variant of Salsa20 - the in-place is the only one used
      - tried to make scrypt work on non-MSWINDOWS (e.g. use of ComObj and ActiveX for bestcrypt and capi)
      - moved CreateObject to public rather than protected (no longer need cracker class to get at it).
        It exists so people can use it; but i'm not sure how i feel about it being truly public.
      - Fixed range-check bug when passing empty salt to PBKDF2

  Version 1.3   20160509
      - made compatible with Delphi 5/7
      - Change some array indexing to use pointer offsets instead; the unnecessary bounds checking was getting confused
      - Refactored core method to reinforce the idea that Scrypt is just simply PBKDF2 (with some fancy salt)
  Version 1.2   20150510
      - Use Cryptography Next Generation (Cng) API for SHA256 (requires Windows Vista or later)
      - Will still fallback to SHA256 CryptoApi CSP (Windows 2000) when on Windows platform
      - still falls back to internal PurePascal implementation if not WINDOWS
      - Changed the strings to pass to TScrypt.GetHashAlgorithm
      - Calling TScrypt.GetHashAlgoritm with "SHA1" or "SHA256" will now choose the best algorithm implementation
      - Pass "SHA1PurePascal" or "SHA256PurePascal" to specifically get the pure pascal versions
      - FIX: HashPassword overload that takes custom cost parameters was using stack garbage for salt

  Version 1.1   20150415
      - Support for actually verifying a password hash
      - 43% faster due to optimizations in XorBlock and Salsa20
      - TODO: Do the same thing canonical scrypt.c does, and do a benchmark before generation to determine parameters.

  Version 1.0   20150408
      - Inital release. Public domain.  Ian Boyd.
        This is free and unencumbered software released into the public domain.
        Anyone is free to copy, modify, publish, use, compile, sell, or
        distribute this software, either in source code form or as a compiled
        binary, for any purpose, commercial or non-commercial, and by any means.
        For more information, please refer to <http://unlicense.org>

  Benchmarks
  ==========

  20150412  Delphi XE6, Release, 32-bit, Intel i5-2500

    |  N |  r=1 |    r=2 |    r=3 |    r=4 |    r=5 |    r=6 |     r=7 |     r=8 |     r=9 |    r=10 |   r=11 |   r=12 |    r=13 |    r=14 |    r=15 |    r=16 |
    |----|------|--------|--------|--------|--------|--------|---------|---------|---------|---------|--------|--------|---------|---------|---------|---------|
    |  1 |  0.2 |    0.2 |    0.2 |    0.2 |    0.2 |    0.2 |     0.3 |     0.3 |     0.3 |     0.3 |    0.4 |    0.4 |     0.4 |     0.5 |     1.3 |     1.2 |
    |  2 |  0.2 |    0.2 |    0.2 |    0.2 |    0.2 |    0.3 |     0.3 |     0.3 |     0.3 |     0.4 |    0.4 |    0.4 |     0.4 |     0.5 |     0.5 |     0.5 |
    |  3 |  0.2 |    0.2 |    0.2 |    0.2 |    0.2 |    0.3 |     0.3 |     0.3 |     0.4 |     0.4 |    0.4 |    0.4 |     0.5 |     0.5 |     0.5 |     0.5 |
    |  4 |  0.2 |    0.2 |    0.2 |    0.3 |    0.3 |    1.1 |     0.4 |     1.3 |     0.6 |     0.7 |    0.6 |    0.6 |     0.7 |     0.7 |     0.7 |     0.8 |
    |  5 |  0.2 |    0.2 |    0.3 |    0.4 |    0.4 |    0.4 |     0.5 |     0.6 |     0.6 |     0.7 |    0.8 |    0.8 |     0.9 |     0.9 |     1.0 |     1.0 |
    |  6 |  0.2 |    0.3 |    0.4 |    0.5 |    0.6 |    0.7 |     0.9 |     0.9 |     1.0 |     1.1 |    1.2 |    1.3 |     1.4 |     1.4 |     1.6 |     1.8 |
    |  7 |  0.4 |    0.5 |    0.8 |    0.9 |    1.1 |    1.2 |     1.4 |     1.8 |     1.8 |     2.0 |    2.2 |    2.3 |     2.5 |     2.8 |     2.8 |     3.1 |
    |  8 |  0.6 |    1.0 |    1.3 |    1.6 |    2.0 |    2.4 |     2.7 |     3.1 |     3.5 |     3.8 |    4.2 |    7.2 |     4.5 |     4.8 |     5.5 |     6.9 |
    |  9 |  1.1 |    1.7 |    3.1 |    6.0 |    6.2 |    4.3 |     5.2 |     5.6 |     6.3 |     6.9 |    9.5 |   11.2 |    11.5 |     9.4 |    11.8 |    10.8 |
    | 10 |  2.0 |    3.2 |    4.8 |    6.2 |    7.8 |    8.5 |     9.6 |    11.3 |    15.7 |    18.4 |   21.1 |   21.0 |    20.9 |    20.1 |    22.9 |    23.1 |
    | 11 |  4.0 |    6.6 |    9.1 |   18.8 |   15.4 |   16.9 |    19.5 |    27.4 |    32.6 |    27.5 |   29.9 |   34.4 |    38.1 |    45.7 |    41.6 |    48.1 |
    | 12 |  7.6 |   14.0 |   19.9 |   25.3 |   30.0 |   34.1 |    41.6 |    49.4 |    61.9 |    58.8 |   63.5 |   73.6 |    74.6 |    83.0 |    86.4 |    92.5 |
    | 13 | 15.3 |   27.4 |   44.4 |   52.3 |   66.7 |   80.7 |    81.3 |    97.1 |   112.3 |   126.1 |  129.1 |  143.8 |   159.3 |   164.4 |   171.1 |   175.2 |
    | 14 | 37.3 |   51.3 |   75.4 |  101.9 |  130.5 |  149.5 |   184.1 |   195.7 |   219.6 |   258.3 |  250.7 |  280.6 |   305.9 |   324.9 |   360.2 |   370.2 |
    | 15 | 70.3 |  118.3 |  158.4 |  196.5 |  258.6 |  315.7 |   355.7 |   393.2 |   472.8 |   501.7 |  540.8 |  619.8 |   662.0 |   685.8 |   729.9 |   791.3 |
    | 16 | #N/A |  229.2 |  305.8 |  430.2 |  521.8 |  624.7 |   700.9 |   823.3 |   909.2 |  1013.5 | 1056.3 | 1190.5 |  1318.4 |  1412.5 |  1501.5 |  1583.2 |
    | 17 | #N/A |  505.1 |  691.5 |  845.0 | 1010.6 | 1243.0 |  1455.5 |  1602.0 |  1798.4 |  2031.1 | 2233.9 | 2436.9 |  2698.8 |  2856.4 |  3043.1 |  3240.8 |
    | 18 | #N/A | 1003.6 | 1415.8 | 1797.0 | 2218.8 | 2597.6 |  2995.2 |  3375.1 |  3749.6 |  4074.9 | 4360.2 | 4655.6 |  5746.6 |  5987.7 |  5804.7 |  6181.3 |
    | 19 | #N/A | 1911.7 | 2598.0 | 3296.0 | 4151.7 | 4880.7 |  5901.3 |  6304.4 |  7150.6 |  8091.7 | 8964.8 | 9909.5 | 10450.6 | 11452.8 | 12200.7 | 12931.8 |
    | 20 | #N/A | 4006.3 | 5673.7 | 7117.5 | 8781.7 | 9939.3 | 12146.8 | 13136.7 | 14539.6 | 16785.1 |   #mem |   #mem |    #mem |    #mem |    #mem |    #N/A |

  Delphi is limited to allocating $7FFFFFFF bytes when using GetMem or SetLength.
  (GetMem and SetLength are defined as taking a 32-bit integer, even in 64-bit applications)

  This means that N=20,r=16 requires 128*16*2^20 = 0x80000000 bytes of memory.

  This exceeds the amount you can ask for in an 32-bit Integer.

  In practice, your limit in a 32-bit process will be lower, given the 2GB limit of virtual address space,
  and that there are other things already in your address space (e.g. your application, dlls).


  References
  ==============
  The scrypt Password-Based Key Derivation Function
    http://tools.ietf.org/html/draft-josefsson-scrypt-kdf-02

  Java implementation of scrypt
    https://github.com/wg/scrypt

  Scrypt For Node/IO
    https://github.com/barrysteyn/node-scrypt
*)

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 15} //15 = Delphi 7
    {$DEFINE COMPILER_7_UP}
  {$IFEND}
  {$IF CompilerVersion = 15} //15 = Delphi 7
    {$DEFINE COMPILER_7}
    {$DEFINE COMPILER_7_DOWN}
  {$IFEND}
{$ELSE}
  {$IFDEF VER130} //Delphi 5
    {$DEFINE COMPILER_7_DOWN}
    {$DEFINE COMPILER_5_DOWN}
    {$DEFINE COMPILER_5}
    {$DEFINE MSWINDOWS} //Delphi 5 didn't define MSWINDOWS back then. And there was no other platform
  {$ENDIF}
{$ENDIF}

{$R-}

interface

uses
  SysUtils, Classes, Types, Generics.Collections,
  fsl_base;


{$IFDEF COMPILER_7} //Delphi 7
type
  TBytes = Types.TByteDynArray; //TByteDynArray wasn't added until around Delphi 7. Sometime later it moved to SysUtils.
{$ENDIF}
{$IFDEF COMPILER_5} //Delphi 5
type
  TBytes = array of Byte; //for old-fashioned Delphi 5, we have to do it ourselves
  IInterface = IUnknown;
!  TStringDynArray = array of String;
  EOSError = EWin32Error;
const
  RaiseLastOSError: procedure = SysUtils.RaiseLastWin32Error; //First appeared in Delphi 7
{$ENDIF}

type
{$M-}

  //As basic of a Hash interface as you can get
  IHashAlgorithm = interface(IInterface)
    ['{985B0964-C47A-4212-ADAA-C57B26F02CCD}']
    function GetBlockSize: Integer;
    function GetDigestSize: Integer;

    { Methods }
    procedure HashData(const Buffer; BufferLen: Integer);
    function Finalize: TBytes;

    { Properties }
    property BlockSize: Integer read GetBlockSize;
    property DigestSize: Integer read GetDigestSize;
  end;

  IHmacAlgorithm = interface(IInterface)
    ['{815787A8-D5E7-41C0-9F23-DF30D1532C49}']
    function GetDigestSize: Integer;
    function HashData(const Key; KeyLen: Integer; const Data; DataLen: Integer): TBytes;
    property DigestSize: Integer read GetDigestSize;
  end;

  IPBKDF2Algorithm = interface(IInterface)
    ['{93BB60D0-2C87-46CB-8A2A-A711F0BBEF0D}']
    function GetBytes(const Password: String; const Salt; const SaltLength: Integer; IterationCount, DesiredBytes: Integer): TBytes;
  end;

  TScrypt = class(TObject)
  protected
    procedure BurnBytes(var data: TBytes);
    class function StringToUtf8(const Source: String): TBytes;

    class function Base64Encode(const data: array of Byte): string;
    class function Base64Decode(const s: string): TBytes;

    class function Tokenize(const s: string; Delimiter: Char): TStringDynArray;
    function GenerateSalt: TBytes;

    class procedure XorBlockInPlace(var A; const B; Length: Integer);

    function PBKDF2(const Password: String; const Salt; const SaltLength: Integer; IterationCount, DesiredBytes: Integer): TBytes;

    procedure Salsa20InPlace(var Input); //four round version of Salsa20, termed Salsa20/8
    function BlockMix(const B: array of Byte): TBytes; //mixes r 128-byte blocks
    function ROMix(const B; BlockSize, CostFactor: Cardinal): TBytes;

    function GenerateScryptSalt(const Passphrase: String; const Salt: array of Byte; const CostFactor, BlockSizeFactor, ParallelizationFactor: Integer): TBytes;
    function DeriveBytes(const Passphrase: String; const Salt: array of Byte; const CostFactor, BlockSizeFactor, ParallelizationFactor: Integer; DesiredBytes: Integer): TBytes;

    procedure GetDefaultParameters(out CostFactor, BlockSizeFactor, ParallelizationFactor: Cardinal);
    function TryParseHashString(HashString: string; out CostFactor, BlockSizeFactor, ParallelizationFactor: Cardinal; out Salt: TBytes; out Data: TBytes): Boolean;
    function FormatPasswordHash(const costFactor, blockSizeFactor, parallelizationFactor: Integer; const Salt, DerivedBytes: array of Byte): string;
  public
    constructor Create;

    //Get a number of bytes using the default Cost and Parallelization factors
    class function GetBytes(const Passphrase: String; const Salt: String; nDesiredBytes: Integer): TBytes; overload;

    //Get a number of bytes, specifying the desired cost and parallelization factor
    class function GetBytes(const Passphrase: String; const Salt: String; CostFactor, BlockSizeFactor, ParallelizationFactor: Cardinal; DesiredBytes: Integer): TBytes; overload;

    {
      Scrypt is not meant for password storage; it is meant for key generation.
      But people can still use it for password hashing.
      Unlike Bcrypt, there is no standard representation for passwords hashed with Scrypt.
      So we can make one, and provide the function to validate it
    }
    class function HashPassword(const Passphrase: String): string; overload;
    class function HashPassword(const Passphrase: String; CostFactor, BlockSizeFactor, ParallelizationFactor: Cardinal): string; overload;
    class function CheckPassword(const Passphrase: String; ExpectedHashString: string; out PasswordRehashNeeded: Boolean): Boolean;

    {
      Let people have access to our hash functions. They've been tested and verified, and they work well.
      Besides, we have HMAC and PBKDF2. That's gotta be useful for someone.
    }
    class function CreateObject(ObjectName: string): IInterface;
  end;

  EScryptException = class(Exception);

implementation

{$IFDEF UnitTests}
  {$DEFINE ScryptUnitTests}
{$ENDIF}

{$IFDEF NoScryptUnitTests}
  {$UNDEF ScryptUnitTests}
{$ENDIF}

uses
  {$IFDEF ScryptUnitTests}ScryptTests,{$ENDIF}
  {$IFNDEF FPC}Windows, ComObj, ActiveX,{$ENDIF}
  Math, fsl_utilities;

{$IFDEF COMPILER_7_DOWN}
function MAKELANGID(p, s: WORD): WORD;
begin
  Result := WORD(s shl 10) or p;
end;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
begin
  Result := C in CharSet;
end;

type
  UInt64 = Int64;
  PUInt64 = ^UInt64;
{$ENDIF}

{$IFDEF FPC}
type
  LARGE_INTEGER = record
    case Integer of
    0: (
      LowPart: DWORD;
      HighPart: Longint);
    1: (
      QuadPart: Int64);
  end;
  ULARGE_INTEGER = record
    case Integer of
    0: (
      LowPart: DWORD;
      HighPart: DWORD);
    1: (
      QuadPart: UInt64);
  end;
{$ENDIF}

const
  SCRYPT_HASH_LEN = 64; //This can be user defined - but this is the reference size

  //The normal Base64 alphabet
  Base64EncodeTable: array[0..63] of Char =
      { 0:} 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
      {26:} 'abcdefghijklmnopqrstuvwxyz'+
      {52:} '0123456789+/';

  Base64DecodeTable: array[#0..#127] of Integer = (
      {  0:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ________________
      { 16:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ________________
      { 32:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,  // _______________/
      { 48:} 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,  // 0123456789______
      { 64:} -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,  // _ABCDEFGHIJKLMNO
      { 80:} 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,  // PQRSTUVWXYZ_____
      { 96:} -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,  // _abcdefghijklmno
      {113:} 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1); // pqrstuvwxyz_____

  //Unix password file use non-standard base64 alphabet
  BsdBase64EncodeTable: array[0..63] of Char =
      { 0:} './'+
      { 2:} 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
      {28:} 'abcdefghijklmnopqrstuvwxyz'+
      {54:} '0123456789';

  BsdBase64DecodeTable: array[#0..#127] of Integer = (
      {  0:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ________________
      { 16:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ________________
      { 32:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  1,  // ______________./
      { 48:} 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, -1, -1, -1, -1, -1, -1,  // 0123456789______
      { 64:} -1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,  // _ABCDEFGHIJKLMNO
      { 80:} 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1,  // PQRSTUVWXYZ_____
      { 96:} -1, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,  // _abcdefghijklmno
      {113:} 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, -1, -1, -1, -1, -1); // pqrstuvwxyz_____


type
  PLongWordArray = ^TLongWordArray_Unsafe;
  TLongWordArray_Unsafe = array[0..79] of LongWord; //SHA uses an array of 80 elements

{$IFNDEF FPC}
const
  //Cryptography Service Provider (CSP) items
  CALG_SHA1 = $00008004;
  CALG_SHA_256 = $0000800c;

type
//Cryptography Next Generation (Cng) items
  BCRYPT_HANDLE = THandle;
  BCRYPT_ALG_HANDLE = THandle;
  BCRYPT_KEY_HANDLE = THandle;
  BCRYPT_HASH_HANDLE = THandle;
  NTSTATUS = Cardinal;

const
  // Microsoft built-in providers. (OpenAlgorithmProvider.pszImplementation)
  MS_PRIMITIVE_PROVIDER: String = 'Microsoft Primitive Provider';
  MS_PLATFORM_CRYPTO_PROVIDER: String = 'Microsoft Platform Crypto Provider'; //i.e. TPM

  // OpenAlgorithmProvider.AlgorithmID
  BCRYPT_SHA256_ALGORITHM = 'SHA256';

  // OpenAlgorithmProvider.dwFlags
  BCRYPT_ALG_HANDLE_HMAC_FLAG = $00000008;

  // BCryptGetProperty property name
  BCRYPT_OBJECT_LENGTH: String = 'ObjectLength';

var
  _BCryptInitialized: Boolean = False;
  _BCryptAvailable: Boolean = False;
  _BCryptOpenAlgorithmProvider: function(out hAlgorithm: BCRYPT_ALG_HANDLE; pszAlgId, pszImplementation: PWideChar; dwFlags: Cardinal): NTSTATUS; stdcall;
  _BCryptCloseAlgorithmProvider: function(hAlgorithm: BCRYPT_ALG_HANDLE; dwFlags: Cardinal): NTSTATUS; stdcall;
  _BCryptGetProperty: function(hObject: BCRYPT_HANDLE; pszProperty: PWideChar; {out}pbOutput: Pointer; cbOutput: Cardinal; out cbResult: Cardinal; dwFlags: Cardinal): NTSTATUS; stdcall;
  _BCryptCreateHash: function(hAlgorithm: BCRYPT_ALG_HANDLE; out hHash: BCRYPT_HASH_HANDLE; pbHashObject: Pointer; cbHashObject: Cardinal; pbSecret: Pointer; cbSecret: Cardinal; dwFlags: DWORD): NTSTATUS; stdcall;
  _BCryptHashData: function(hHash: BCRYPT_HASH_HANDLE; pbInput: Pointer; cbInput: Cardinal; dwFlags: Cardinal): NTSTATUS; stdcall;
  _BCryptFinishHash: function(hHash: BCRYPT_HASH_HANDLE; pbOutput: Pointer; cbOutput: Cardinal; dwFlags: Cardinal): NTSTATUS; stdcall;
  _BCryptDestroyHash: function(hHash: BCRYPT_HASH_HANDLE): NTSTATUS; stdcall;
  _BCryptGenRandom: function({In_opt}hAlgorithm: BCRYPT_ALG_HANDLE; {Inout}pbBuffer: Pointer; cbBuffer: Cardinal; dwFlags: Cardinal): NTSTATUS; stdcall;
  _BCryptDeriveKeyPBKDF2: function(hPrf: BCRYPT_ALG_HANDLE; pbPassword: Pointer; cbPassword: Cardinal; pbSalt: Pointer; cbSalt: Cardinal; cIterations: UInt64; pbDerivedKey: Pointer; cbDerivedKey: Cardinal; dwFlags: Cardinal): NTSTATUS; stdcall;

function FormatNTStatusMessage(const NTStatusMessage: NTSTATUS): string;
var
  Buffer: PChar;
  Len: Integer;
  Hand: HMODULE;
begin
  {
    KB259693: How to translate NTSTATUS error codes to message strings

    Obtain the formatted message for the given Win32 ErrorCode
    Let the OS initialize the Buffer variable. Need to LocalFree it afterward.
  }
  Hand := SafeLoadLibrary('ntdll.dll');

  Len := FormatMessage(
      FORMAT_MESSAGE_ALLOCATE_BUFFER or
      FORMAT_MESSAGE_FROM_SYSTEM or
//      FORMAT_MESSAGE_IGNORE_INSERTS or
//      FORMAT_MESSAGE_ARGUMENT_ARRAY or
      FORMAT_MESSAGE_FROM_HMODULE,
      Pointer(Hand),
      NTStatusMessage, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
      @Buffer, 0, nil);
  try
    //Remove the undesired line breaks and '.' char
    while (Len > 0) and (CharInSet(Buffer[Len - 1], [#0..#32, '.'])) do Dec(Len);
    //Convert to Delphi string
    SetString(Result, Buffer, Len);
  finally
    //Free the OS allocated memory block
    LocalFree(HLOCAL(Buffer));
  end;
  FreeLibrary(Hand);
end;

procedure NTStatusCheck(Status: NTSTATUS);
const
  SNTError = 'NT Error 0x%.8x: %s';
begin
  if (Status and $80000000) = 0 then //00: success, 01:information, 10: warning, 11: error
    Exit;

  raise EOleSysError.CreateFmt(SNTError, [
      HResultFromNT(Status),
      FormatNTStatusMessage(Status)
  ]);
end;
{$ENDIF}

function RRot32(const X: LongWord; const c: Byte): LongWord; //inline;
begin
  //Any use of assembly is dwarfed by the fact that ASM functions cannot be inlined
  //Which forces a function call. Which drops us from 82MB/s -> 50 MB/s
  Result := (X shr c) or (X shl (32-c));
end;

function LRot32(X: LongWord; c: Byte): LongWord; //inline;
{IFDEF PUREPASCAL}
begin
  Result := (X shl c) or (X shr (32-c));
{ELSE !PUREPASCAL}
(*  {$IFDEF CPUX86}
  asm
    MOV cl, c;
    ROL eax, cl;
  {$ENDIF CPUX86}
  {$IFDEF CPUX64}
  //http://blogs.msdn.com/b/oldnewthing/archive/2004/01/14/58579.aspx
  //In x64 calling convention the first four parameters are passed in rcx, rdx, r8, r9
  //Return value is in RAX
  asm
    MOV eax, ecx; //store result in eax
    MOV cl, c;    //rol left only supports from rolling from cl
    ROL eax, cl;
  {$ENDIF}
*)
{ENDIF !PUREPASCAL}
end;

function ByteSwap(const X: Cardinal): Cardinal; //inline;
begin
{
  Reverses the byte order of a 32-bit register.
}
  Result :=
      ( X shr 24) or
      ((X shr  8) and $00FF00) or
      ((X shl  8) and $FF0000) or
      ( X shl 24);
end;

{$IFNDEF FPC}
procedure RaiseOSError(ErrorCode: DWORD; Msg: string);
var
  ex: EOSError;
begin
  ex := EOSError.Create(Msg);
  ex.ErrorCode := error;
  raise Ex;
end;
{$ENDIF}

type
  HCRYPTPROV = THandle;
  HCRYPTHASH = THandle;
  HCRYPTKEY = THandle;
  ALG_ID = LongWord; //unsigned int


{ SHA1 implemented in PurePascal}
type
  TSHA1 = class(TInterfacedObject, IHashAlgorithm)
  private
    FInitialized: Boolean;
    FHashLength: UInt64; //Number of bits put into the hash
    FHashBuffer: array[0..63] of Byte;  //one step before W0..W15
    FHashBufferIndex: Integer;  //Current position in HashBuffer
    FABCDEBuffer: array[0..4] of LongWord; //working hash buffer is 160 bits (20 bytes)
    procedure Compress;
    procedure UpdateLen(NumBytes: LongWord);
    procedure Burn;
  protected
    procedure HashCore(const Data; DataLen: Integer);
    function HashFinal: TBytes;

    function GetBlockSize: Integer;
    function GetDigestSize: Integer;

    procedure Initialize;
  public
    constructor Create;

    procedure HashData(const Buffer; BufferLen: Integer);
    function Finalize: TBytes;

    procedure SelfTest;
  end;

{$IFNDEF FPC}
{
  SHA-1 implemented by Microsoft Crypto Service Provider (CSP)
}
  TCspHash = class(TInterfacedObject, IHashAlgorithm)
  private
    FProvider: HCRYPTPROV;
    FAlgorithmID: Cardinal; //CALG_SHA1, CALG_SHA256
    FBlockSize: Integer; //CSP doesn't provide a way to get the block size of a hash
    FHash: HCRYPTHASH;
  protected
    function GetBlockSize: Integer; //SHA-1 compresses in blocks of 64 bytes
    function GetDigestSize: Integer; //SHA-1 digest is 20 bytes (160 bits)

    procedure Initialize;
    procedure Burn;
    procedure HashCore(const Data; DataLen: Integer);
    function HashFinal: TBytes;
  public
    constructor Create(AlgorithmID: Cardinal; BlockSize: Integer);
    destructor Destroy; override;

    procedure HashData(const Buffer; BufferLen: Integer);
    function Finalize: TBytes;
  end;

{
  Hash algorithms provided by the Microsoft Cryptography Next Generation (Cng) Provider
}
  TCngHash = class(TInterfacedObject, IHashAlgorithm, IHmacAlgorithm)
  private
    FHmacMode: Boolean;
    FAlgorithm: BCRYPT_ALG_HANDLE;
    FAlgorithmBlockSize: Integer;
    FAlgorithmDigestSize: Integer;
    FHashObjectBuffer: TBytes;
    FHash: BCRYPT_HASH_HANDLE;
    class function GetBcryptAlgorithmBlockSize(Algorithm: BCRYPT_ALG_HANDLE): Integer;
    class function GetBcryptAlgorithmDigestSize(Algorithm: BCRYPT_ALG_HANDLE): Integer;
  protected
    procedure RequireBCrypt;
    function GetBlockSize: Integer; //e.g. SHA-1 compresses in blocks of 64 bytes
    function GetDigestSize: Integer; //e.g. SHA-1 digest is 20 bytes (160 bits)

    class function InitializeBCrypt: Boolean;

    procedure Initialize;
    procedure Burn;
    procedure HashCore(Hash: BCRYPT_HASH_HANDLE; const Data; DataLen: Integer);
    function HashFinal(Hash: BCRYPT_HASH_HANDLE): TBytes;
  public
    constructor Create(const AlgorithmID: wideString; const Provider: PWideChar; HmacMode: Boolean);
    destructor Destroy; override;

    class function IsAvailable: Boolean;

    { IHashAlgorithm }
    procedure HashData(const Buffer; BufferLen: Integer); overload;
    function Finalize: TBytes;

    { IHmacAlgoritm }
    function HashData(const Key; KeyLen: Integer; const Data; DataLen: Integer): TBytes; overload;
  end;
  {$ENDIF}

{
  SHA256 implemented in Pascal
}
type
  TSHA256 = class(TInterfacedObject, IHashAlgorithm)
  private
    FInitialized: Boolean;
    FHashLength: UInt64; //Number of bits put into the hash
    FHashBuffer: array[0..63] of Byte;  //one step before W0..W15
    FHashBufferIndex: Integer;  //Current position in HashBuffer
    FCurrentHash: array[0..7] of LongWord;
    procedure Compress;
    procedure UpdateLen(NumBytes: LongWord);
    procedure Burn;
  protected
    function GetBlockSize: Integer;
    function GetDigestSize: Integer;

    procedure HashCore(const Data; DataLen: Integer);
    function HashFinal: TBytes;

    procedure Initialize;
  public
    constructor Create;

    procedure HashData(const Buffer; BufferLen: Integer);
    function Finalize: TBytes;
  end;

{
  SHA-256 implemented by Microsoft Crypto Service Provider (CSP)
}
  THmac = class(TInterfacedObject, IHmacAlgorithm)
  private
    FHash: IHashAlgorithm;
  protected
    function GetDigestSize: Integer;
  public
    constructor Create(Hash: IHashAlgorithm);
    function HashData(const Key; KeyLen: Integer; const Data; DataLen: Integer): TBytes;
  end;

  TRfc2898DeriveBytes = class(TInterfacedObject, IPBKDF2Algorithm)
  private
    FHMAC: IHmacAlgorithm;
  public
    constructor Create(HMAC: IHmacAlgorithm);
    function GetBytes(const Password: String; const Salt; const SaltLength: Integer; IterationCount, DesiredBytes: Integer): TBytes;
  end;

  {$IFNDEF FPC}
  TBCryptDeriveKeyPBKDF2 = class(TInterfacedObject, IPBKDF2Algorithm)
  private
    FAlgorithm: BCRYPT_ALG_HANDLE;
  public
    constructor Create(const AlgorithmID: String; const Provider: PWideChar);

    function GetBytes(const Password: String; const Salt; const SaltLength: Integer; IterationCount, DesiredBytes: Integer): TBytes;
  end;
  {$ENDIF}

{ TScrypt }

class function TScrypt.GetBytes(const Passphrase, Salt: String; nDesiredBytes: Integer): TBytes;
var
  scrypt: TScrypt;
  saltUtf8: TBytes;
  costFactor, blockSizeFactor, parallelizationFactor: Cardinal;
begin
  saltUtf8 := TScrypt.StringToUtf8(Salt);

  scrypt := TScrypt.Create;
  try
    scrypt.GetDefaultParameters(costFactor, blockSizeFactor, parallelizationFactor);

    Result := scrypt.DeriveBytes(Passphrase, saltUtf8, costFactor, blockSizeFactor, parallelizationFactor, nDesiredBytes);
  finally
    scrypt.Free;
  end;
end;

procedure TScrypt.GetDefaultParameters(out CostFactor, BlockSizeFactor, ParallelizationFactor: Cardinal);
const
  N_interactive = 14; //14 ==> 2^14 =    16,384
//  N_sensitive   = 20; //20 ==> 2^20 = 1,048,576
  r = 8;
  p = 1;
var
  t1, t2: UInt64;
  duration: Int64;
  testCostFactor: Cardinal;
begin
  {
    The time to run a full scrypt is linear in memory used; although CPU is slightly faster with doubled r over doubled N.

    Canonical scrypt runs a benchmark with N=14, r=1 (i.e. 128*1*2^14 = 128*1*16384 = 2MB)
    We'll do a 2MB benchmark, but using r=8, N=11    (i.e. 128*8*2^11 = 128*8* 2048 = 2MB)

    The target for a normal user is 250-500 ms
  }

  CostFactor := 14; //i.e. 2^14 = 16,384 iterations
  BlockSizefactor := 8; //will operate on 128*8 = 1,024 byte blocks, and randomly access 128*8*2^14 = 16 MB of memory during the calculation
  ParallelizationFactor := 1;

  //Benchmark the current computer, and see if it could be faster than 250ms to compute a hash

  testCostFactor := 11;
  t1 := GetTickCount64;
  TScrypt.HashPassword('Benchmark', testCostFactor, 8, 1);
  t2 := GetTickCount64;

  duration := (t2-t1);

  //Each single increase in CostFactor will double the execution time.
  //We don't want the execution time to exceed 500ms
  while (duration < 250) do
  begin
    duration := duration*2;
    testCostFactor := testCostFactor+1;
  end;

  //And we certainly won't go any lower than the default 14,8,1 (anyone remember 8,N,1 anymore?)
  if testCostFactor > CostFactor then
  begin
    {$IFNDEF FPC}
    OutputDebugString(PChar(Format('Increasing scrypt cost factor from default %d up to %d', [CostFactor, testCostFactor])));
    {$ENDIF}
    CostFactor := testCostFactor;
  end;
end;

class function TScrypt.Base64Decode(const s: string): TBytes;

  function Char64(character: Char): Integer;
  begin
    if (Ord(character) > Length(Base64DecodeTable)) then
    begin
      Result := -1;
      Exit;
    end;

    Result := Base64DecodeTable[character];
  end;

  procedure Append(value: Byte);
  var
    i: Integer;
  begin
    i := Length(Result);
    SetLength(Result, i+1);
    Result[i] := value;
  end;

var
  i: Integer;
  len: Integer;
  c1, c2, c3, c4: Integer;
begin
  SetLength(Result, 0);

  len := Length(s);
  i := 1;
  while i <= len do
  begin
    // We'll need to have at least 2 character to form one byte.
    // Anything less is invalid
    if (i+1) > len then
      raise EScryptException.Create('Invalid base64 hash string');

    c1 := Char64(s[i  ]);
    c2 := Char64(s[i+1]);
    c3 := Char64(s[i+2]);
    c4 := Char64(s[i+3]);
    Inc(i, 4);

    if (c1 = -1) or (c2 = -1) then
      raise EScryptException.Create('Invalid base64 hash string');

    //Now we have at least one byte in c1|c2
    // c1 = ..111111
    // c2 = ..112222
    Append( ((c1 and $3f) shl 2) or (c2 shr 4) );

    if (c3 = -1) then
      Exit;

    //Now we have the next byte in c2|c3
    // c2 = ..112222
    // c3 = ..222233
    Append( ((c2 and $0f) shl 4) or (c3 shr 2) );

    //If there's a 4th caracter, then we can use c3|c4 to form the third byte
    if (c4 = -1) then
      Exit;

    //Now we have the next byte in c3|c4
    // c3 = ..222233
    // c4 = ..333333
    Append( ((c3 and $03) shl 6) or c4 );
  end;
end;

class function TScrypt.Base64Encode(const data: array of Byte): string;

const
  b64: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  function EncodePacket(b1, b2, b3: Byte; Len: Integer): string;
  begin
    {
      11111111 22222222 33333333
      \____/\_____/\_____/\____/
        |      |      |     |
       c1     c2     c3    c4
    }
    Result := '====';

    Result[1] := Base64EncodeTable[b1 shr 2];
    Result[2] := Base64EncodeTable[((b1 and $03) shl 4) or (b2 shr 4)];
    if Len < 2 then Exit;

    Result[3] := Base64EncodeTable[((b2 and $0f) shl 2) or (b3 shr 6)];
    if Len < 3 then Exit;

    Result[4] := Base64EncodeTable[b3 and $3f];
  end;

var
  i: Integer;
  len: Integer;
  b1, b2: Integer;
begin
  Result := '';

  len := Length(data);
  if len = 0 then
    Exit;

  //encode whole 3-byte chunks  TV4S 6ytw fsfv kgY8 jIuc Drjc 8deX 1s.
  i := Low(data);
  while len >= 3 do
  begin
    Result := Result+EncodePacket(data[i], data[i+1], data[i+2], 3);
    Inc(i, 3);
    Dec(len, 3);
  end;

  if len = 0 then
    Exit;

  //encode partial final chunk
  Assert(len < 3);
  if len >= 1 then
    b1 := data[i]
  else
    b1 := 0;
  if len >= 2 then
    b2 := data[i+1]
  else
    b2 := 0;
  Result := Result+EncodePacket(b1, b2, 0, len);
end;

function TScrypt.BlockMix(const B: array of Byte): TBytes;
var
  r: Integer;
  X: array[0..15] of LongWord;
  i: Integer;
  Y: TBytes;
  ne, no: Integer; //index even, index odd
begin
{
  Mix r 128-byte blocks (which is equivalent of saying 2r 64-byte blocks)
}
  //Make sure we actually have an even multiple of 128 bytes
  if Length(B) mod 128 <> 0 then
    raise EScryptException.Create('');
  r := Length(B) div 128;

  SetLength(Y, 128*r);

  //X <-B B[2*r-1]
  //Copy last 64-byte block into X.
  Move(B[64*(2*r-1)], X[0], 64);


  for i := 0 to 2*r-1 do
  begin
    //T = X xor B[i]
    TScrypt.XorBlockInPlace(X[0], B[64*i], 64);

    //X = Salsa (T)
    Self.Salsa20InPlace(X[0]);

    //Y[i] = X
    Move(X[0], Y[64*i], 64);
  end;

  {
    Result = Y[0],Y[2],Y[4], ..., Y[2*r-2], Y[1],Y[3],Y[5], ..., Y[2*r-1]

    Result[ 0] := Y[ 0];
    Result[ 1] := Y[ 2];
    Result[ 2] := Y[ 4];
    Result[ 3] := Y[ 6];
    Result[ 4] := Y[ 8];
    Result[ 5] := Y[10];
    Result[ 6] := Y[ 1];
    Result[ 7] := Y[ 3];
    Result[ 8] := Y[ 5];
    Result[ 9] := Y[ 7];
    Result[10] := Y[ 9];
    Result[11] := Y[11];

    Result[ 0] := Y[ 0];
    Result[ 6] := Y[ 1];
    Result[ 1] := Y[ 2];
    Result[ 7] := Y[ 3];
    Result[ 2] := Y[ 4];
    Result[ 8] := Y[ 5];
    Result[ 3] := Y[ 6];
    Result[ 9] := Y[ 7];
    Result[ 4] := Y[ 8];
    Result[10] := Y[ 9];
    Result[ 5] := Y[10];
    Result[11] := Y[11];

  }
  SetLength(Result, Length(B));
  i := 0;
  ne := 0;
  no := r;
  while (i <= 2*r-1) do
  begin
    Move(Y[64*(i  )], Result[64*ne], 64);
    Move(Y[64*(i+1)], Result[64*no], 64);
    Inc(ne, 1);
    Inc(no, 1);
    Inc(i, 2);
  end;
end;

procedure TScrypt.BurnBytes(var data: TBytes);
begin
  if Length(data) <= 0 then
    Exit;

  FillChar(data[Low(data)], Length(data), 0);
  SetLength(data, 0);
end;

class function TScrypt.CheckPassword(const Passphrase: String; ExpectedHashString: string; out PasswordRehashNeeded: Boolean): Boolean;
var
  scrypt: TScrypt;
  costFactor, blockSizeFactor, parallelizationFactor: Cardinal;
  salt, expected, actual: TBytes;
  t1, t2, freq: Int64;
  duration: Real;
const
  SCouldNotParsePassword = 'Could not parse expected password hash';
begin
  {
    Validate the supplied password against the saved hash.

    Returns
      True: If the password is valid
      False: If the password is invalid

    PasswordRehashNeeded
      Contains true if the user's password should be re-hashed and the new hash stored in the database.
      The typical reason for rehashing is that the hash took less than the minimum 250ms to compute. The target is 250-500ms.
      Another reason would be if the scrypt version has been updated, and the stored hash needs to be updated to the new
      version. 

      Contains false if the password does not need to be rehashed.
  }
  Result := False;
  PasswordRehashNeeded := False;

  scrypt := TScrypt.Create;
  try
    if not scrypt.TryParseHashString(ExpectedHashString, {out}costFactor, blockSizeFactor, parallelizationFactor, salt, expected) then
      raise EScryptException.Create(SCouldNotParsePassword);
    try
      {$IFNDEF FPC}
      QueryPerformanceCounter(t1);
      {$ENDIF}
      actual := scrypt.DeriveBytes(Passphrase, salt, costFactor, blockSizeFactor, ParallelizationFactor, Length(expected));
      {$IFNDEF FPC}
      QueryPerformanceCounter(t2);
      {$ENDIF}

      if Length(actual) <> Length(expected) then
        Exit;

      Result := CompareMem(@expected[0], @actual[0], Length(expected));

      if Result then
      begin
        //Only advertise a rehash being needed if they got the correct password.
        //Don't want someone blindly re-hashing with a bad password because they forgot to check the result,
        //or because they decided to handle "PasswordRehashNeeded" first.
        {$IFNDEF FPC}
        if QueryPerformanceFrequency(freq) then
        begin
        {$ENDIF}
          duration := (t2-t1)/freq * 1000; //ms
          if duration < 250 then
            PasswordRehashNeeded := True;
        {$IFNDEF FPC}
        end;
        {$ENDIF}
      end;
    finally
      scrypt.BurnBytes(actual);
      scrypt.BurnBytes(expected);
    end;
  finally
    scrypt.Free;
  end;
end;

function TScrypt.DeriveBytes(const Passphrase: String; const Salt: array of Byte; const CostFactor,
    BlockSizeFactor, ParallelizationFactor: Integer; DesiredBytes: Integer): TBytes;
var
  saltEx: TBytes;
begin
  //Step 1. Use Scrypt to generate expensive salt
  saltEx := Self.GenerateScryptSalt(Passphrase, Salt, CostFactor, BlockSizeFactor, ParallelizationFactor);

  //Step 2. Use PBDKF2 with our password, but use B as the salt
  Result := Self.PBKDF2(Passphrase, Pointer(saltEx)^, Length(saltEx), 1, DesiredBytes);
end;

function TScrypt.FormatPasswordHash(const costFactor, blockSizeFactor, parallelizationFactor: Integer; const Salt,
  DerivedBytes: array of Byte): string;
const
  SCRYPT_MCF_ID = '$s1';
var
  parameters: Cardinal;
begin
  {
    We will use libscrypt's format

    Modular Crypt Format support for scrypt
    https://github.com/jvarho/pylibscrypt/blob/master/pylibscrypt/mcf.py

    Compatible with libscrypt scrypt_mcf_check also supports the $7$ format.

    libscrypt format:

      $s1$NNrrpp$salt$hash
        NN   - hex encoded N log2 (two hex digits)
        rr   - hex encoded r in 1-255
        pp   - hex encoded p in 1-255
        salt - base64 encoded salt 1-16 bytes decoded
        hash - base64 encoded 64-byte scrypt hash
  }
  if (CostFactor < 1) or (CostFactor > 255) then
    raise EScryptException.CreateFmt('Invalid CostFactor %d', [CostFactor]);
  if (BlockSizeFactor < 1) or (BlockSizeFactor > 255) then
    raise EScryptException.CreateFmt('Invalid BlockSizeFactor %d', [BlockSizeFactor]);
  if (ParallelizationFactor < 1) or (ParallelizationFactor > 255) then
    raise EScryptException.CreateFmt('Invalid ParallelizationFactor %d', [ParallelizationFactor]);

  parameters := (CostFactor shl 16)
      or (BlockSizeFactor shl 8)
      or (ParallelizationFactor);

  //$s1$0e0801$TWlzcyB5b3UgS2lyc3Rlbg==$SXQncyBkb2Vzbid0IHdvcmsgb3V0IGZvciBldmVyeW9uZS5Ob3QgZXZlcnlvbmUgZ2V0cyB0byBiZSBsb3ZlZA==

  Result := SCRYPT_MCF_ID+
      '$'+IntToHex(parameters, 6)+
      '$'+Self.Base64Encode(Salt)+
      '$'+Self.Base64Encode(DerivedBytes);
end;

constructor TScrypt.Create;
begin
  inherited Create;
end;

class function TScrypt.CreateObject(ObjectName: string): IInterface;
const
  BCRYPT_SHA1_ALGORITHM = 'SHA1';
  BCRYPT_SHA256_ALGORITHM = 'SHA256';

  SUnknownAlgorithm = 'Unknown algorithm "%s" requested';

  function IsAlgo(s: string): Boolean;
  begin
    Result := AnsiSameText(ObjectName, s);
  end;
begin
  {
    We contain a number of hash algorithms.
    It might be nice to let people outside us to get ahold of them.

    ObjectName can be one of the following strings

    | ObjectName                 | Description                         | Returned Interface  |
    |----------------------------|-------------------------------------|---------------------|
    | 'SHA1'                     | SHA-1, best implementation          | IHashAlgorithm      |
    | 'SHA1.Cng'                 | SHA-1, Crypto Next Gen (Cng)        | IHashAlgorithm      |
    | 'SHA1.Csp'                 | SHA-1, Crypto API                   | IHashAlgorithm      |
    | 'SHA1.PurePascal'          | SHA-1, pure pascal                  | IHashAlgorithm      |

    | 'SHA256'                   | SHA256, best implementation         | IHashAlgorithm      |
    | 'SHA256.Cng'               | SHA256, Crypto Next Gen (Cng)       | IHashAlgorithm      |
    | 'SHA256.Csp'               | SHA256, Crypto API                  | IHashAlgorithm      |
    | 'SHA256.PurePascal         | SHA256, pure pascal                 | IHashAlgorithm      |

    | 'HMAC.SHA1'                | HMAC-SHA1                           | IHmacAlgorithm      |
    | 'HMAC.SHA1.Cng'            | HMAC-SHA1, Crypto Next Gen          | IHmacAlgorithm      |
    | 'HMAC.SHA1.PurePascal'     | HMAC-SHA1, pure pascal              | IHmacAlgorithm      |

    | 'HMAC.SHA2561'             | HMAC-SHA256                         | IHmacAlgorithm      |
    | 'HMAC.SHA256.Cng'          | HMAC-SHA256, Crypto Next Gen        | IHmacAlgorithm      |
    | 'HMAC.SHA256.PurePascal'   | HMAC-SHA256, pure pascal            | IHmacAlgorithm      |

    | 'PBKDF2.SHA1'              | PBKDF-SHA1, best implementation     | IPBKDF2Algorithm    |
    | 'PBKDF2.SHA1.Cng'          | PBKDF-SHA1, Crypto Next Gen (Cng)   | IPBKDF2Algorithm    |
    | 'PBKDF2.SHA1.PurePascal'   | PBKDF-SHA1, Pure pascal             | IPBKDF2Algorithm    |

    | 'PBKDF2.SHA256'            | PBKDF-SHA256, best implementation   | IPBKDF2Algorithm    |
    | 'PBKDF2.SHA256.Cng'        | PBKDF-SHA256, Crypto Next Gen (Cng) | IPBKDF2Algorithm    |
    | 'PBKDF2.SHA256.PurePascal' | PBKDF-SHA256, Pure pascal           | IPBKDF2Algorithm    |

  }
  if IsAlgo('SHA1') then
  begin
{$IFNDEF FPC}
    //Microsoft SHA1 Cng and CSP versions are about 87% faster than our "PurePascal" versions
    if TCngHash.IsAvailable then
      ObjectName := 'SHA1.Cng'
    else
      ObjectName := 'SHA1.Csp';
{$ELSE}
    ObjectName := 'SHA1.PurePascal';
{$ENDIF}
    Result := TScrypt.CreateObject(ObjectName);
    Exit;
  end;

  if IsAlgo('SHA256') then
  begin
{$IFNDEF FPC}
    //Microsoft SHA256 Cng and CSP versions are about 87% faster than our "PurePascal" versions
    if TCngHash.IsAvailable then
      ObjectName := 'SHA256.Cng'
    else
      ObjectName := 'SHA256.Csp';
{$ELSE}
    ObjectName := 'SHA256.PurePascal';
{$ENDIF}
    Result := TScrypt.CreateObject(ObjectName);
    Exit;
  end;

  if IsAlgo('HMAC.SHA1') then
  begin
{$IFNDEF FPC}
    //Microsoft Cng provides a full HMAC implementation using SHA-1
    if TCngHash.IsAvailable then
      ObjectName := 'HMAC.SHA1.Cng'
    else
      ObjectName := 'HMAC.SHA1.PurePascal';
{$ELSE}
    ObjectName := 'HMAC.SHA1.PurePascal';
{$ENDIF}
    Result := TScrypt.CreateObject(ObjectName);
    Exit;
  end;

  if IsAlgo('HMAC.SHA256') then
  begin
{$IFNDEF FPC}
    //Microsoft Cng provides a full HMAC implementation using SHA256
    if TCngHash.IsAvailable then
      ObjectName := 'HMAC.SHA256.Cng'
    else
      ObjectName := 'HMAC.SHA256.PurePascal';
{$ELSE}
    ObjectName := 'HMAC.SHA256.PurePascal';
{$ENDIF}
    Result := TScrypt.CreateObject(ObjectName);
    Exit;
  end;


  { SHA1 }
  if      IsAlgo('SHA1.PurePascal')        then Result := TSHA1.Create
  {$IFNDEF FPC}
  else if IsAlgo('SHA1.Csp')               then Result := TCspHash.Create(CALG_SHA1, 64)
  else if IsAlgo('SHA1.Cng')               then Result := TCngHash.Create(BCRYPT_SHA1_ALGORITHM, nil, False)
  {$ENDIF}

  { SHA256 }
  else if IsAlgo('SHA256.PurePascal')      then Result := TSHA256.Create
  {$IFNDEF FPC}
  else if IsAlgo('SHA256.Csp')             then Result := TCspHash.Create(CALG_SHA_256, 64)
  else if IsAlgo('SHA256.Cng')             then Result := TCngHash.Create(BCRYPT_SHA256_ALGORITHM, nil, False)
  {$ENDIF}

  { HMAC - SHA1 }
  else if IsAlgo('HMAC.SHA1.PurePascal')   then Result := THmac.Create(TSHA1.Create)
  {$IFNDEF FPC}
  else if IsAlgo('HMAC.SHA1.csp')          then Result := THmac.Create(TCspHash.Create(CALG_SHA1, 64))
  else if IsAlgo('HMAC.SHA1.Cng')          then Result := TCngHash.Create(BCRYPT_SHA1_ALGORITHM, nil, True)
  {$ENDIF}

  { HMAC - SHA256 }
  else if IsAlgo('HMAC.SHA256.PurePascal') then Result := THmac.Create(TSHA256.Create)
  {$IFNDEF FPC}
  else if IsAlgo('HMAC.SHA256.csp')        then Result := THmac.Create(TCspHash.Create(CALG_SHA_256, 64))
  else if IsAlgo('HMAC.SHA256.Cng')        then Result := TCngHash.Create(BCRYPT_SHA256_ALGORITHM, nil, True)
  {$ENDIF}

  { PBKDF2 - SHA1 }
  else if IsAlgo('PBKDF2.SHA1') then
  begin
    {$IFNDEF FPC}
    if TCngHash.IsAvailable then
      Result := TScrypt.CreateObject('PBKDF2.SHA1.Cng')
    else
    {$ENDIF}
      Result := TScrypt.CreateObject('PBKDF2.SHA1.PurePascal');
  end
  else if IsAlgo('PBKDF2.SHA1.PurePascal') then Result := TRfc2898DeriveBytes.Create(THmac.Create(TSHA1.Create))
  {$IFNDEF FPC}
  else if IsAlgo('PBKDF2.SHA1.Cng')        then Result := TBCryptDeriveKeyPBKDF2.Create(BCRYPT_SHA1_ALGORITHM, nil)
  {$ENDIF}

  { PBKDF2 - SHA256 }
  else if IsAlgo('PBKDF2.SHA256') then
  begin
    {$IFNDEF FPC}
    if TCngHash.IsAvailable then
      Result := TScrypt.CreateObject('PBKDF2.SHA256.Cng')
    else
    {$ENDIF}
      Result := TScrypt.CreateObject('PBKDF2.SHA256.PurePascal');
  end
  else if IsAlgo('PBKDF2.SHA256.PurePascal') then Result := TRfc2898DeriveBytes.Create(THmac.Create(TSHA256.Create))
  {$IFNDEF FPC}
  else if IsAlgo('PBKDF2.SHA256.Cng')        then Result := TBCryptDeriveKeyPBKDF2.Create(BCRYPT_SHA256_ALGORITHM, nil)
  {$ENDIF}

  else
    raise ELibraryException.createFmt(SUnknownAlgorithm, [ObjectName]);
end;

function TScrypt.GenerateSalt: TBytes;
var
  type4Uuid: TGUID;
  salt: TBytes;
const
  SCRYPT_SALT_LEN = 16; //This is just a recommended size
begin
  //Salt is a 128-bit (16 byte) random value
  SetLength(salt, SCRYPT_SALT_LEN);

  //Type 4 UUID (RFC 4122) is a handy source of (almost) 128-bits of random data (actually 120 bits)
  //But the security doesn't come from the salt being secret, it comes from the salt being different each time
  type4Uuid := CreateGUID;

  Move(type4Uuid.D1, salt[0], SCRYPT_SALT_LEN); //i.e. move 16 bytes

  Result := salt;
end;

function TScrypt.GenerateScryptSalt(const Passphrase: String; const Salt: array of Byte; const CostFactor,
  BlockSizeFactor, ParallelizationFactor: Integer): TBytes;
var
  B: TBytes;
  i: Integer;
  blockSize: Integer;
  blockIndex: Integer;
  T: TBytes;
begin
  blockSize := 128*BlockSizeFactor;

  //Step 1. Use PBKDF2 to generate the initial blocks
  B := Self.PBKDF2(Passphrase, Addr(salt)^, Length(salt), 1, ParallelizationFactor*blockSize);

  //Step 2. Run RoMix on each block
  {
    Each each ROMix operation can run in parallal on each block.
    But the downside is that each ROMix itself will consume blockSize*Cost memory.

    LiteCoin uses
      Cost: 1,024 (costFactor=10 ==> 2^10 = 1024)
      blockSize: 128 bytes (r=1)
      parallelizationFactor: 1 (p=1)

      B: [128]
  }
  i := 0;
  SetLength(T, 0); //to shut up the compiler's "Variable 'T' might not have been initialized". It's because secretly a return value is actually a var parameter.
  while i < ParallelizationFactor do
  begin
    //B[i] <-B ROMix(B[i])
    blockIndex := i*blockSize;
    T := Self.ROMix(B[blockIndex], blockSize, CostFactor);
    Move(T[0], B[blockIndex], blockSize);
    Inc(i);
  end;

  Result := B;
end;

class function TScrypt.GetBytes(const Passphrase, Salt: String; CostFactor, BlockSizeFactor, ParallelizationFactor: Cardinal; DesiredBytes: Integer): TBytes;
var
  saltUtf8: TBytes;
  scrypt: TScrypt;
begin
  {
    BlockSize = BlockSizeFactor*128 bytes
    Iterations = 2^CostFactor

    Memory requirement: BlockSize * Iterations = 128*BlockSizeFactor*(2^CostFactor)
  }
  saltUtf8 := TScrypt.StringToUtf8(Salt);

  scrypt := TScrypt.Create;
  try
    Result := scrypt.DeriveBytes(Passphrase, saltUtf8, CostFactor, BlockSizeFactor, ParallelizationFactor, DesiredBytes);
  finally
    scrypt.Free;
  end;
end;

class function TScrypt.HashPassword(const Passphrase: String): string;
var
  costFactor: Cardinal;
  blockSizeFactor: Cardinal;
  parallelizationFactor: Cardinal;
  scrypt: TScrypt;
  salt, derivedBytes: TBytes;
begin
  {
    Generate a password hash, letting TScrypt decide the best parameters.

    Password hash is returned as a string of the form:

      $s1$NNrrpp$salt$hash
        NN   - hex encoded N log2 (two hex digits)
        rr   - hex encoded r in 1-255
        pp   - hex encoded p in 1-255
        salt - base64 encoded salt 1-16 bytes decoded
        hash - base64 encoded 64-byte scrypt hash

    For example.
      Password: "correct horse battery staple"
      Hash: "$s1$0E0801$G34Rvmk2DSkp9sFJyyM49O$z3XxEUNlHDhq2nCR1Yh4tqKCelFQ9gnFNgtmgoBJHW4zeAIDoAjV5zcOLYk5lLqoGEFQNQ6YoOvXHAlVjPJS9e"

          CostFactor: 0x0E (14), Cost=2^14 = 16384
          BlockSizeFacdtor: 0x08, BlockSize = 128 * 8 = 1,024 bytes
          Parallelization Factor: 0x1
          Salt (base64): G34Rvmk2DSkp9sFJyyM49O
          Password (base64): z3XxEUNlHDhq2nCR1Yh4tqKCelFQ9gnFNgtmgoBJHW4zeAIDoAjV5zcOLYk5lLqoGEFQNQ6YoOvXHAlVjPJS9e
  }
  scrypt := TScrypt.Create;
  try
    salt := scrypt.GenerateSalt;
    scrypt.GetDefaultParameters({out}costFactor, blockSizeFactor, parallelizationFactor);

    derivedBytes := scrypt.DeriveBytes(Passphrase, salt, costFactor, blockSizeFactor, parallelizationFactor, SCRYPT_HASH_LEN);

    Result := scrypt.FormatPasswordHash(costFactor, blockSizeFactor, parallelizationFactor, salt, derivedBytes);
  finally
    scrypt.Free;
  end;
end;

class function TScrypt.HashPassword(const Passphrase: String; CostFactor, BlockSizeFactor, ParallelizationFactor: Cardinal): string;
var
  scrypt: TScrypt;
  salt, derivedBytes: TBytes;
begin
  {
  Hash the password, using the supplied parameters.

    CostFactor:            log2(N), N = 2^costFactor
    BlockSizeFactor:       r
    ParallelizationFactor: p

  Password hash is returned as a string of the form:

    $s1$NNrrpp$salt$hash
      NN   - hex encoded cost factor (i.e. log2(N) ) (two hex digits)
      rr   - hex encoded r in 1-255
      pp   - hex encoded p in 1-255
      salt - base64 encoded salt 1-16 bytes decoded
      hash - base64 encoded 64-byte scrypt hash

  For example.
    Password: "correct horse battery staple"
    Hash: "$s1$0E0801$G34Rvmk2DSkp9sFJyyM49O$z3XxEUNlHDhq2nCR1Yh4tqKCelFQ9gnFNgtmgoBJHW4zeAIDoAjV5zcOLYk5lLqoGEFQNQ6YoOvXHAlVjPJS9e"

        CostFactor: 0x0E (14), Cost=2^14 = 16384
        BlockSizeFacdtor: 0x08, BlockSize = 128 * 8 = 1,024 bytes
        Parallelization Factor: 0x1
        Salt (base64): G34Rvmk2DSkp9sFJyyM49O
        Password (base64): z3XxEUNlHDhq2nCR1Yh4tqKCelFQ9gnFNgtmgoBJHW4zeAIDoAjV5zcOLYk5lLqoGEFQNQ6YoOvXHAlVjPJS9e

  Background
  ===========

  Someone already decided on a standard string way to represent scrypt passwords.
    https://github.com/wg/scrypt

  We'll gravitate to any existing standard we can find

    $s0$params$salt$key
      s0     - version 0 of the format with 128-bit salt and 256-bit derived key
      params - 32-bit hex integer containing log2(N) (16 bits), r (8 bits), and p (8 bits)
      salt   - base64-encoded salt
      key    - base64-encoded derived key

  Example:

    $s0$e0801$epIxT/h6HbbwHaehFnh/bw==$7H0vsXlY8UxxyW/BWx/9GuY7jEvGjT71GFd6O4SZND0=

      passwd = "secret"
      N = 16384
      r = 8
      p = 1

  There is another standard out there, published by the guy who authored the rfc.

  Unix crypt using scrypt
    https://gitorious.org/scrypt/ietf-scrypt/raw/7c4a7c47d32a5dbfd43b1223e4b9ac38bfe6f8a0:unix-scrypt.txt

    > This document specify a new Unix crypt method based on the scrypt
    > password-based key derivation function.  It uses the

    $<ID>$<SALT>$<PWD>

    convention introduced with the old MD5-based solution and also used by
    the more recent SHA-256/SHA-512 mechanism specified here:

      http://www.akkadia.org/drepper/sha-crypt.html

    The scrypt method uses the following value:

            ID       |    Method
          -------------------------------
            7        |    scrypt

        The scrypt method requires three parameters in the SALT value: N, r
        and p which are expressed like this:

          N=<N>,r=<r>,p=<p>$

        where N, r and p are unsigned decimal numbers that are used as the
        scrypt parameters.

        The PWD part is the password string, and the size is fixed to 86
        characters which corresponds to 64 bytes base64 encoded.

        To compute the PWD part, run the scrypt algorithm with the password,
        salt, parameters to generate 64 bytes and base64 encode it.

  And then theres:
    https://github.com/jvarho/pylibscrypt/blob/master/pylibscrypt/mcf.py

    Modular Crypt Format support for scrypt

    Compatible with libscrypt scrypt_mcf_check also supports the $7$ format.

  Which is the format i chose
  }
  scrypt := TScrypt.Create;
  try
    salt := scrypt.GenerateSalt;

    derivedBytes := scrypt.DeriveBytes(Passphrase, salt, costFactor, blockSizeFactor, ParallelizationFactor, SCRYPT_HASH_LEN);

    Result := scrypt.FormatPasswordHash(costFactor, blockSizeFactor, ParallelizationFactor, salt, derivedBytes);
  finally
    scrypt.Free;
  end;
end;

(*function TScrypt.HMAC(const Hash: IHashAlgorithm; const Key; KeyLen: Integer; const Data; DataLen: Integer): TBytes;
var
  oKeyPad, iKeyPad: TBytes;
  i, n: Integer;
  digest: TBytes;
  blockSize: Integer;

type
  PUInt64Array = ^TUInt64Array_Unsafe;
  TUInt64Array_Unsafe = array[0..MaxInt div 16] of UInt64;

begin
  {
    Implementation of RFC2104  HMAC: Keyed-Hashing for Message Authentication

    Tested with known test vectors from RFC2202: Test Cases for HMAC-MD5 and HMAC-SHA-1
  }
  blockSize := Hash.BlockSize;

  // Clear pads
  SetLength(oKeyPad, blockSize); //elements will be initialized to zero by SetLength
  SetLength(iKeyPad, blockSize); //elements will be initialized to zero by SetLength

  // if key is longer than blocksize: reset it to key=Hash(key)
  if KeyLen > blockSize then
  begin
    Hash.HashData(Key, KeyLen);
    digest := Hash.Finalize;

    //Store hashed key in pads
    Move(digest[0], iKeyPad[0], Length(digest)); //remaining bytes will remain zero
    Move(digest[0], oKeyPad[0], Length(digest)); //remaining bytes will remain zero
  end
  else
  begin
    //Store original key in pads
    Move(Key, iKeyPad[0], KeyLen); //remaining bytes will remain zero
    Move(Key, oKeyPad[0], KeyLen); //remaining bytes will remain zero
  end;

  {
    Xor key with ipad and ipod constants
      iKeyPad = key xor 0x36
      oKeyPad = key xor 0x5c

    DONE: Unroll this to blockSize div 4 xor's of $5c5c5c5c and $36363636
  }
  n := blockSize div SizeOf(UInt64);
  for i := 0 to n-1 do
    PUInt64Array(@oKeyPad[0])[i] := PUInt64Array(@oKeyPad[0])[i] xor UInt64($5c5c5c5c5c5c5c5c);
  for i := 0 to n-1 do
    PUInt64Array(@iKeyPad[0])[i] := PUInt64Array(@iKeyPad[0])[i] xor UInt64($3636363636363636);
  n := blockSize mod SizeOf(UInt64);
  if n <> 0 then
  begin
    //This should never happen in practice.
    //Hash block sizes are going to be multiple of 8 bytes
    for i := blockSize-1-n to blockSize-1 do
    begin
      oKeyPad[i] := oKeyPad[i] xor $5c;
      iKeyPad[i] := iKeyPad[i] xor $36;
    end;
  end;

  {
    Result := hash(oKeyPad || hash(iKeyPad || message))
  }
  // Perform inner hash: digest = Hash(iKeyPad || data)
  SetLength(iKeyPad, blockSize+DataLen);
  Move(data, iKeyPad[blockSize], DataLen);
  Hash.HashData(iKeyPad[0], Length(iKeyPad));
  digest := Hash.Finalize;

  // perform outer hash: result = Hash(oKeyPad || digest)
  SetLength(oKeyPad, blockSize+Length(digest));
  Move(digest[0], oKeyPad[blockSize], Length(digest));
  Hash.HashData(oKeyPad[0], Length(oKeyPad));
  Result := Hash.Finalize;
end;*)

function TScrypt.PBKDF2(const Password: String; const Salt; const SaltLength: Integer; IterationCount, DesiredBytes: Integer): TBytes;
var
  rfc: IPBKDF2Algorithm;
begin
  rfc := Self.CreateObject('PBKDF2.SHA256') as IPBKDF2Algorithm;
  Result := rfc.GetBytes(Password, Salt, SaltLength, IterationCount, DesiredBytes);
end;

function TScrypt.ROMix(const B; BlockSize, CostFactor: Cardinal): TBytes;
var
  r: Cardinal;
  N: UInt64;
  X: TBytes;
  V: TBytes;
  i: Cardinal;
  //j: UInt64;
  j: LongWord;
  T: TBytes;
const
  SInvalidBlockLength = 'ROMix input is not multiple of 128-bytes';
  SInvalidCostFactorTooLow = 'CostFactor %d must be greater than zero';
  SInvalidCostFactorArgument = 'CostFactor %d must be less than 16r (%d)';
begin
  {
    B: block of r*128 bytes.
    For example, r=5 ==> block size is 5*128 = 640 bytes

      B: [640 bytes]

    Cost: 2^CostFactor. Number of copies of B we will be working with

    For example, CostFactor=3 ==> Cost = 2^3 = 6

      V: [640 bytes][640 bytes][640 bytes][640 bytes][640 bytes][640 bytes]
          V0         V1         V2         V3         V4         V5

    LiteCoin, for example, uses a blocksize of 128 (r=1)
    and Cost of 1024:

      V: [128][128][128]...[128]    128KB total
         V0   V1   V2     V1024
  }
  if BlockSize mod 128 <> 0 then
    raise EScryptException.Create(SInvalidBlockLength);
  r := BlockSize div 128;

  {
    Cost (N) = 2^CostFactor (we specify cost factor like BCrypt does, as a the exponent of a two)

    SCrypt rule dictates:

      N < 2^(128*r/8)
      N < 2^(16r)

      2^CostFactor < 2^(16r)

      CostFactor < 16r
  }
  if CostFactor <= 0 then
    raise EScryptException.CreateFmt(SInvalidCostFactorTooLow, [CostFactor]);
  if CostFactor >= (16*r) then
    raise EScryptException.CreateFmt(SInvalidCostFactorArgument, [CostFactor, 16*r]);

  //N <- 2^CostFactor
  N := (1 shl CostFactor);

  //Delphi's GetMem and SetLength are limited to signed 32-bits (<21474836468)
  //That means that N*r*128 < 21474836468
  if (Int64(N)*Int64(r)*128) >= $7FFFFFFF then
    raise EScryptException.CreateFmt('Parameters N (%d) and r (%d) use exceed available memory usage (%d bytes)', [N, r, Int64(N)*r*128]);

  //Step 1: X <- B
  SetLength(X, BlockSize);
  Move(B, X[0], BlockSize);

  //Step 2 - Create N copies of B
  //V <- N copies of B
  SetLength(V, BlockSize*N);
  for i := 0 to N-1 do
  begin
    //V[i] <- X
    Move(X[0], V[BlockSize*i], BlockSize);

    //X <- BlockMix(X)
    X := Self.BlockMix(X); //first iteration values match the BlockMix test vectors
  end;

  //Step 3
  SetLength(T, BlockSize);
  for i := 0 to N-1 do
  begin
    //j <- Integerify(X) mod N

    //Convert first 8-bytes of the *last* 64-byte block of X to a UInt64, assuming little endian (Intel) format
    //j := PUInt64(@X[BlockSize-64])^; //0xE2B6E8D50510A964 = 16,336,500,699,943,709,028
    //j := j and (N-1); //because N is a power of 2 (N == 2^costFactor), an optimization is simple bitmasking

    j := PLongWord(@X[BlockSize-64])^; //0xE2B6E8D50510A964 = 16,336,500,699,943,709,028
    j := j and (N-1); //because N is a power of 2 (N == 2^costFactor), an optimization is simple bitmasking

    //T <- X xor V[j]
    //X <- BlockMix(T)
    Move(V[BlockSize*j], T[0], BlockSize);
    TScrypt.XorBlockInPlace(T[0], X[0], BlockSize);
    X := Self.BlockMix(T);
  end;

  Result := X;
end;

{$OVERFLOWCHECKS OFF}
procedure TScrypt.Salsa20InPlace(var Input);
var
  i: Integer;
  Result: PLongWordArray;
  x00, x01, x02, x03,
  x04, x05, x06, x07,
  x08, x09, x10, x11,
  x12, x13, x14, x15: LongWord;
begin
{
  The 64-byte input x to Salsa20 is viewed in little-endian form as 16 UInt32's
}
  //Storing array values in local variables can avoid array bounds checking and indirection lookups every time, giving 4.4% performance boost
  {
    |       |        Overall |
    |-------|----------------|
    | Array |   7,783.063 ms |
    | Vars  |   7,439.332 ms |
  }
  x00 := PLongWordArray(@Input)[0];
  x01 := PLongWordArray(@Input)[1];
  x02 := PLongWordArray(@Input)[2];
  x03 := PLongWordArray(@Input)[3];
  x04 := PLongWordArray(@Input)[4];
  x05 := PLongWordArray(@Input)[5];
  x06 := PLongWordArray(@Input)[6];
  x07 := PLongWordArray(@Input)[7];
  x08 := PLongWordArray(@Input)[8];
  x09 := PLongWordArray(@Input)[9];
  x10 := PLongWordArray(@Input)[10];
  x11 := PLongWordArray(@Input)[11];
  x12 := PLongWordArray(@Input)[12];
  x13 := PLongWordArray(@Input)[13];
  x14 := PLongWordArray(@Input)[14];
  x15 := PLongWordArray(@Input)[15];

  //It's a four round algorithm; when the documentation says it's 8 round.
  for i := 0 to 3 do
  begin
    {
      Reordering the assignments from the RFC gives us a free 27.4% speedup.
      It works because there are operations that can be done that do not (yet) depend on the previous result.
      So while one execution unit is calculating the sum+LRot+Xor of one tuple,
      we can go ahead and start calculating on a different tuple.

      |            |       Overall |
      |------------|---------------|
      | Original   | 11,264.682 ms |
      | Rearranged |  7,783.063 ms |

      TODO: Figure out a SIMD way to do these four parallel constructs in parallel.
    }

    {
      Mix DWORDs together between chunks
        <--- 256 bits--->   <----- 256 bits ----->
        <128 b>   <128 b>   <128 bit>  <128 bits >
      [ 0 1 2 3   4 5 6 7   8 9 10 11  12 13 14 15 ]
        a b   D   A b c       B  c  d  a      C  d
        a   C d   a b   D   A b  c         B  c  d
          B c d   a   C d   a b     D  A   b  c
        A b c       B c d   a    C  d  a   b     D
    }
    //First DWORD
    x04 := x04 xor LRot32(x00+x12, 7);
    x09 := x09 xor LRot32(x05+x01, 7);
    x14 := x14 xor LRot32(x10+x06, 7);
    x03 := x03 xor LRot32(x15+x11, 7);

    //Second DWORD
    x08 := x08 xor LRot32(x04+x00, 9);
    x13 := x13 xor LRot32(x09+x05, 9);
    x02 := x02 xor LRot32(x14+x10, 9);
    x07 := x07 xor LRot32(x03+x15, 9);

    //Third DWORD
    x12 := x12 xor LRot32(x08+x04,13);
    x01 := x01 xor LRot32(x13+x09,13);
    x06 := x06 xor LRot32(x02+x14,13);
    x11 := x11 xor LRot32(x07+x03,13);

    //Fourth DWORD
    x00 := x00 xor LRot32(x12+x08,18);
    x05 := x05 xor LRot32(x01+x13,18);
    x10 := x10 xor LRot32(x06+x02,18);
    x15 := x15 xor LRot32(x11+x07,18);

    {
      Mix the DWORDs within each 16 byte set.

      [ 0 1 2 3   4 5 6 7   8 9 10 11  12 13 14 15 ]
        a A   a   b b B       c  c  C   D     d  d
        a a A       b b B   C    c  c   d  D     d
          a a A   B   b b   c C     c   d  d  D
        A   a a   b B   b   c c  C         d  d  D
    }
    //Calculate first DWORD within each chunk
    x01 := x01 xor LRot32(x00+x03, 7);
    x06 := x06 xor LRot32(x05+x04, 7);
    x11 := x11 xor LRot32(x10+x09, 7);
    x12 := x12 xor LRot32(x15+x14, 7);

    //Calculate second DWORD within each chunk
    x02 := x02 xor LRot32(x01+x00, 9);
    x07 := x07 xor LRot32(x06+x05, 9);
    x08 := x08 xor LRot32(x11+x10, 9);
    x13 := x13 xor LRot32(x12+x15, 9);

    //Calculate third DWORD within each chunk
    x03 := x03 xor LRot32(x02+x01,13);
    x04 := x04 xor LRot32(x07+x06,13);
    x09 := x09 xor LRot32(x08+x11,13);
    x14 := x14 xor LRot32(x13+x12,13);

    //Calculate fourth DWORD within each chunk
    x00 := x00 xor LRot32(x03+x02,18);
    x05 := x05 xor LRot32(x04+x07,18);
    x10 := x10 xor LRot32(x09+x08,18);
    x15 := x15 xor LRot32(x14+x13,18);
  end;

  //Result <- Input + X;
  Result := PLongWordArray(@Input);
  Result[ 0] := Result[ 0] + X00;
  Result[ 1] := Result[ 1] + X01;
  Result[ 2] := Result[ 2] + X02;
  Result[ 3] := Result[ 3] + X03;
  Result[ 4] := Result[ 4] + X04;
  Result[ 5] := Result[ 5] + X05;
  Result[ 6] := Result[ 6] + X06;
  Result[ 7] := Result[ 7] + X07;
  Result[ 8] := Result[ 8] + X08;
  Result[ 9] := Result[ 9] + X09;
  Result[10] := Result[10] + X10;
  Result[11] := Result[11] + X11;
  Result[12] := Result[12] + X12;
  Result[13] := Result[13] + X13;
  Result[14] := Result[14] + X14;
  Result[15] := Result[15] + X15;
end;
{$OVERFLOWCHECKS ON}

class function TScrypt.StringToUtf8(const Source: String): TBytes;
begin
{
  For scrypt passwords we will use UTF-8 encoding.
}
  Result := TEncoding.UTF8.GetBytes(source);
end;

class function TScrypt.Tokenize(const s: string; Delimiter: Char): TStringDynArray;
var
  iLength: integer;
  i: integer;
  szOutput: string;
  n: Integer;
begin
  iLength := Length(s);

  SetLength(Result, 0);

  for i := 1 to iLength do
  begin
    if s[i] = Delimiter then
    begin
      n := Length(Result);
      SetLength(Result, n+1);
      Result[n] := szOutput;
      szOutput := '';
    end
    else
      szOutput := szOutput + s[i];
  end;

  if szOutput <> '' then
  begin
    n := Length(Result);
    SetLength(Result, n+1);
    Result[n] := szOutput;
  end;
end;

function TScrypt.TryParseHashString(HashString: string; out CostFactor, BlockSizeFactor, ParallelizationFactor: Cardinal;
    out Salt: TBytes; out Data: TBytes): Boolean;
var
  tokens: TStringDynArray;
  parameters: Cardinal;
begin
  Result := False;

  if HashString = '' then
    Exit; //raise EScryptException.Create('HashString cannot be empty');

{
  There are a number of different standards out there.

    $s1$NNrrpp$salt$hash
    $s0$NNrrpp$salt$key
    $7$N=14,r=4,p=1$epIxT/h6HbbwHaehFnh/bw==$MTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNA==
    $7$Nrrrrrpppppsalt$hash
}
  //All versions start with a "$"
  if HashString[1] <> '$' then
    Exit; //raise EScryptException.Create('HashString must start with ''$''');

  //All versions will have five tokens
  SetLength(tokens, 0); //Variable 'tokens' might not have been initialized
  tokens := Self.Tokenize(HashString, '$');
    //tokens[0] ==> "" (the space before the first $)
    //tokens[1] ==> "s01"
    //tokens[2] ==> parameters
    //tokens[3] ==> salt
    //tokens[4] ==> derived bytes
  if Length(tokens) < 5 then
    Exit; //raise EScryptException.CreateFmt('HashString string did not contain five tokens (%d)', [Length(tokens)]);

  if AnsiSameText(tokens[1], 's1') then
  begin
    {
      Modular Crypt Format support for scrypt
      https://github.com/jvarho/pylibscrypt/blob/master/pylibscrypt/mcf.py

      Compatible with libscrypt scrypt_mcf_check also supports the $7$ format.

      libscrypt format:

        $s1$NNrrpp$salt$hash
          NN   - hex encoded N log2 (two hex digits)
          rr   - hex encoded r in 1-255
          pp   - hex encoded p in 1-255
          salt - base64 encoded salt 1-16 bytes decoded
          hash - base64 encoded 64-byte scrypt hash
    }
    parameters := Cardinal(StrToInt('0x'+tokens[2]));
    CostFactor := (parameters and $FFFF0000) shr 16;
    BlockSizeFactor := (parameters and $0000FF00) shr 8;
    ParallelizationFactor := (parameters and $000000FF);

    Salt := TScrypt.Base64Decode(tokens[3]);
    Data := TScrypt.Base64Decode(tokens[4]);

    Result := True;
  end
  else if AnsiSameText(tokens[1], 's0') then
  begin
    {
      Java implementation of scrypt (Lambdaworks OSS)
      https://github.com/wg/scrypt

      $s0$params$salt$key

          s0     - version 0 of the format with 128-bit salt and 256-bit derived key
          params - 32-bit hex integer containing log2(N) (16 bits), r (8 bits), and p (8 bits)
          salt   - base64-encoded salt
          key    - base64-encoded derived key

      Example:

         $s0$e0801$epIxT/h6HbbwHaehFnh/bw==$7H0vsXlY8UxxyW/BWx/9GuY7jEvGjT71GFd6O4SZND0=

       passwd = "secret"
          N = 16384
          r = 8
          p = 1
    }
    parameters := Cardinal(StrToInt('0x'+tokens[2]));
    CostFactor := (parameters and $FFFF0000) shr 16;
    BlockSizeFactor := (parameters and $0000FF00) shr 8;
    ParallelizationFactor := (parameters and $000000FF);

    Salt := TScrypt.Base64Decode(tokens[3]);
    Data := TScrypt.Base64Decode(tokens[4]);

    Result := True;
  end
  else if AnsiSameText(tokens[1], '7') then
  begin
    {
      Unix crypt using scrypt
      https://gitorious.org/scrypt/ietf-scrypt/raw/7c4a7c47d32a5dbfd43b1223e4b9ac38bfe6f8a0:unix-scrypt.txt
      -----------------------

      $7$N=14,r=4,p=1$epIxT/h6HbbwHaehFnh/bw==$MTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNA==

      This document specify a new Unix crypt method based on the scrypt
      password-based key derivation function.  It uses the

        $<ID>$<SALT>$<PWD>

      convention introduced with the old MD5-based solution and also used by
      the more recent SHA-256/SHA-512 mechanism specified here:

        http://www.akkadia.org/drepper/sha-crypt.html

      The scrypt method uses the following value:

            ID       |    Method
          -------------------------------
            7        |    scrypt

      The scrypt method requires three parameters in the SALT value: N, r
      and p which are expressed like this:

        N=<N>,r=<r>,p=<p>$

      where N, r and p are unsigned decimal numbers that are used as the
      scrypt parameters.

      The PWD part is the password string, and the size is fixed to 86
      characters which corresponds to 64 bytes base64 encoded.

      To compute the PWD part, run the scrypt algorithm with the password,
      salt, parameters to generate 64 bytes and base64 encode it.
    }
  end
  else if AnsiSameText(tokens[1], '7') then
  begin
    {
      $7$ format
      https://github.com/jvarho/pylibscrypt/blob/master/pylibscrypt/mcf.py

        $7$Nrrrrrpppppsalt$hash
        N     - crypt base64 N log2
        rrrrr - crypt base64 r (little-endian 30 bits)
        ppppp - crypt base64 p (little-endian 30 bits)
        salt  - raw salt (0-43 bytes that should be limited to crypt base64)
        hash  - crypt base64 encoded 32-byte scrypt hash (43 bytes)

        (crypt base64 is base64 with the alphabet: ./0-9A-Za-z)

      This is a brain-dead format that needs to be uninvented.
    }
  end
  else
  begin
    //We don't know what it is. Tell the caller about it
    raise EScryptException.CreateFmt('Unknown scrypt hash format "%s"', [tokens[1]]);
  end;
end;

class procedure TScrypt.XorBlockInPlace(var A; const B; Length: Integer);
var
  i: Integer;
  blocks: Integer;
  n: Integer;

type
  PUInt64Array = ^TUInt64Array_Unsafe;
  TUInt64Array_Unsafe = array[0..MaxInt div 16] of UInt64;

begin
  //DONE: Unroll to 8-byte chunks
  //TODO: Detect 128-bit or 256-bit SIMD available, and unroll to XOR 16 or 32 bytes at at time.
  {
    Unrolling XOR to operate on 8 bytes at a time, rather than 1 byte at a time,
    gives a 5.3x speedup in the XORing operation, and a 1.6x speedup (35.7%) overall.

    | SIMD    | Time in XOR  | Overall time  |
    |---------|--------------|---------------|
    | 1 byte  | 8,682.402 ms | 17,511.904 ms |
    | 8 bytes | 1,631.759 ms | 11,253.510 ms |

    Note: Inlining this function has no performance improvement (if anything its 0.0007% slower)
  }
  blocks := Length div SizeOf(UInt64); //optimizes to "shr 3" anyway
  for i := 0 to blocks-1 do
    PUInt64Array(@A)[i] := PUInt64Array(@A)[i] xor PUInt64Array(@B)[i];

  //Finish up any remaining. This will never happen in practice; because 64 bytes is always a multiple of 8 bytes
  if (Length mod SizeOf(UInt64)) <> 0 then
  begin
    n := blocks*SizeOf(UInt64);
    for i := n to Length-1 do
      PByteArray(@A)[i] := PByteArray(@A)[i] xor PByteArray(@B)[i];
  end;
end;

{ TSHA1 }

constructor TSHA1.Create;
begin
  inherited Create;

  Initialize;
end;

function TSHA1.Finalize: TBytes;
begin
  Result := Self.HashFinal;
//  Self.Initialize; HashFinal does the burn
end;

procedure TSHA1.Burn;
begin
  //Empty the hash buffer
  FHashLength := 0;
  FHashBufferIndex := 0;
  FillChar(FHashBuffer[0], Length(FHashBuffer), 0);

  //And reset the current state of the hash to the default starting values
  FABCDEBuffer[0] := $67452301;
  FABCDEBuffer[1] := $EFCDAB89;
  FABCDEBuffer[2] := $98BADCFE;
  FABCDEBuffer[3] := $10325476;
  FABCDEBuffer[4] := $C3D2E1F0;

  FInitialized := True;
end;

procedure TSHA1.Compress;
{Call this when the HashBuffer is full, and can now be dealt with}
var
  A, B, C, D, E: LongWord;  //temporary buffer storage#1
  TEMP: LongWord;  //temporary buffer for a single Word
  Wt: array[0..79] of LongWord;  //temporary buffer storage#2
  W: PLongWordArray;
  i: integer;  //counter

//  function LRot32_5(const X: LongWord): LongWord; inline;
//  begin
//    Result := (X shl 5) or (X shr 27);
//  end;
begin
  {Reset HashBuffer index since it can now be reused
    (well, not _now_, but after .Compress}
  FHashBufferIndex := 0;

  W := PLongWordArray(@Wt[0]); //9.02% speedup by defeating range checking

  {Move HashBuffer into W, and change the Endian order}
  i := 0;
  while (i < 16) do
  begin
    //TODO: This can be vectorized
    W[i  ] := ByteSwap(PLongWordArray(@FHashBuffer[0])[i  ]);
    W[i+1] := ByteSwap(PLongWordArray(@FHashBuffer[0])[i+1]);
    W[i+2] := ByteSwap(PLongWordArray(@FHashBuffer[0])[i+2]);
    W[i+3] := ByteSwap(PLongWordArray(@FHashBuffer[0])[i+3]);
    Inc(i, 4);
  end;

  {Step B in 'FIPS PUB 180-1'
   - Calculate the rest of Wt

  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
  *   *           *            *        =
    *   *           *             *        =
     *   *           *             *         =
  }
  //160.5 MB/s
//  for i := 16 to 79 do
//    W[i] := LRot32(W[i-3] xor W[i- 8] xor W[i-14] xor W[i-16], 1); //164 MB/s

  {
    https://software.intel.com/en-us/articles/improving-the-performance-of-the-secure-hash-algorithm-1/
    https://blogs.oracle.com/DanX/entry/optimizing_solaris_x86_sha_1
  }
  //159.5 MB/s
{  for i := 16 to 31 do
    W[i] := LRot32(W[i-3] xor W[i- 8] xor W[i-14] xor W[i-16], 1); //164 MB/s
  for i := 32 to 79 do
    W[i] := LRot32(W[i-6] xor W[i-16] xor W[i-28] xor W[i-32], 2); //168 MB/s}


  //176 MB/s
  while (i < 32) do //16..31, 16 calculations, 2 at at time = 8 loops
  begin
    //This represents the form that can be vectorized: Two independant calculations at a time
    W[i  ] := LRot32(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16], 1);
    W[i+1] := LRot32(W[i-2] xor W[i-7] xor W[i-13] xor W[i-15], 1); //Delphi is unable to optimize -3+1 or 1-3 as -2
    Inc(i, 2);
  end;
  while (i < 80) do //32..79, 48 calculations, 6 at a time = 8 loops
  begin
    //This represents the form that can be vectorized: Six independant calcuations at a time
    W[i  ] := LRot32(W[i-6] xor W[i-16] xor W[i-28] xor W[i-32], 2);
    W[i+1] := LRot32(W[i-5] xor W[i-15] xor W[i-27] xor W[i-31], 2);
    W[i+2] := LRot32(W[i-4] xor W[i-14] xor W[i-26] xor W[i-30], 2);
    W[i+3] := LRot32(W[i-3] xor W[i-13] xor W[i-25] xor W[i-29], 2);
    W[i+4] := LRot32(W[i-2] xor W[i-12] xor W[i-24] xor W[i-28], 2);
    W[i+5] := LRot32(W[i-1] xor W[i-11] xor W[i-23] xor W[i-27], 2);
    Inc(i, 6)
  end;

  {Step C in 'FIPS PUB 180-1'
   - Copy the CurrentHash into the ABCDE buffer}
  A := FABCDEBuffer[0];
  B := FABCDEBuffer[1];
  C := FABCDEBuffer[2];
  D := FABCDEBuffer[3];
  E := FABCDEBuffer[4];

  {Step D in 'FIPS PUB 180-1}
  //These calculations are 15% faster if the XOR and ROT happen at the end of each assignment.
  //I don't know why; but we are where we are.
  {t=0..19 uses fa}
  for i := 0 to 19 do
  begin
  {$Q-}
    TEMP := $5A827999 + E + W[i] + (D xor (B and (C xor D))) + ((A shl 5) or (A shr 27));
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := TEMP;
  end;

  {t=20..39 uses fb}
  for i := 20 to 39 do
  begin
  {$Q-}
    TEMP := $6ED9EBA1 + E + W[i] + (B xor C xor D) + ((A shl 5) or (A shr 27));
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := TEMP;
  end;

  {t=40..59 uses fc}
  for i := 40 to 59 do
  begin
  {$Q-}
    TEMP := $8F1BBCDC + E + W[i] + ((B and C) or (D and (B or C))) + ((A shl 5) or (A shr 27));
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := TEMP;
  end;

  {t60..79 uses fd}
  for i := 60 to 79 do
  begin
  {$Q-}
    TEMP := $CA62C1D6 + E + W[i] + (B xor C xor D) + ((A shl 5) or (A shr 27));
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := TEMP;
  end;

  {Step E in 'FIPS PUB 180-1'
   - Update the Current hash values}
  FABCDEBuffer[0] := FABCDEBuffer[0] + A;
  FABCDEBuffer[1] := FABCDEBuffer[1] + B;
  FABCDEBuffer[2] := FABCDEBuffer[2] + C;
  FABCDEBuffer[3] := FABCDEBuffer[3] + D;
  FABCDEBuffer[4] := FABCDEBuffer[4] + E;

  {Clear out W and the HashBuffer}
  //14% faster by not doing these here
//  FillChar(W[0], SizeOf(W), 0); we don't *need* to empty W.
//  FillChar(FHashBuffer[0], SizeOf(FHashBuffer), 0);  //HashFinal takes care of this
end;

function TSHA1.GetBlockSize: Integer;
begin
  Result := 64; //block size of SHA1 is 64 bytes (512 bits)
end;

function TSHA1.GetDigestSize: Integer;
begin
  Result := 20; //SHA-1 digest size is 160 bits (20 bytes)
end;

procedure TSHA1.HashCore(const Data; DataLen: Integer);
//  Updates the state of the hash object so a correct hash value is returned at
//  the end of the data stream.
var
  bytesRemainingInHashBuffer: Integer;
  dummySize: Integer;
//  buffer: PByteArray;
  buffer: PByte;
  //dataOffset: Integer;
begin
{  Parameters
  array    input for which to compute the hash code.
  ibStart  offset into the byte array from which to begin using data.
  cbSize  number of bytes in the byte array to use as data.}
  if not FInitialized then
    raise EScryptException.Create('SHA1 not initialized');

  if (DataLen = 0) then
    Exit;

  //buffer := PByteArray(@Data);
  buffer := PByte(@Data);
  //dataOffset := 0;

  dummySize := DataLen;
  UpdateLen(dummySize);  //Update the Len variables given size

  while dummySize > 0 do
  begin
    bytesRemainingInHashBuffer := Length(FHashBuffer) - FHashBufferIndex;
    {HashBufferIndex is the next location to write to in hashbuffer
      Sizeof(HasBuffer) - HashBufferIndex = space left in HashBuffer}
    {cbSize is the number of bytes coming in from the user}
    if bytesRemainingInHashBuffer <= dummySize then
    begin
      {If there is enough data left in the buffer to fill the HashBuffer
        then copy enough to fill the HashBuffer}
      //Move(buffer[dataOffset], FHashBuffer[FHashBufferIndex], bytesRemainingInHashBuffer);
      Move(buffer^, FHashBuffer[FHashBufferIndex], bytesRemainingInHashBuffer);
      Dec(dummySize, bytesRemainingInHashBuffer);
      //Inc(dataOffset, bytesRemainingInHashBuffer);
      Inc(buffer, bytesRemainingInHashBuffer);
      Self.Compress;
    end
    else
    begin
      {
        20070508  Ian Boyd
        If the input length was not an even multiple of HashBufferSize (64 bytes i think),
        then there was a buffer overrun. Rather than Moving and incrementing by DummySize
        it was using cbSize, which is the size of the original buffer
      }
      //If there isn't enough data to fill the HashBuffer...
      //...copy as much as possible from the buffer into HashBuffer...
      //Move(buffer[dataOffset], FHashBuffer[FHashBufferIndex], dummySize);
      Move(buffer^, FHashBuffer[FHashBufferIndex], dummySize);
      //then move the HashBuffer Index to the next empty spot in HashBuffer
      Inc(FHashBufferIndex, dummySize);
      //And shrink the size in the buffer to zero
      dummySize := 0;
    end;
  end;
end;

procedure TSHA1.HashData(const Buffer; BufferLen: Integer);
begin
  Self.HashCore(Buffer, BufferLen);
end;

function TSHA1.HashFinal: TBytes;
{  This method finalizes any partial computation and returns the correct hash
  value for the data stream.}
type
  TLongWordBuffer = array[0..15] of FixedUInt;
begin
  {The final act is to tack on the size of the message}

  {Tack on the final bit 1 to the end of the data}
  FHashBuffer[FHashBufferIndex] := $80;

  //Zero out the byes from the $80 marker to the end of the buffer
  FillChar(FHashBuffer[FHashBufferIndex+1], 63-FHashBufferIndex, 0);


  {[56] is the start of the 2nd last word in HashBuffer}
  {if we are at (or past) it, then there isn't enough room for the whole
    message length (64-bits i.e. 2 words) to be added in}
  {The HashBuffer can essentially be considered full (even if the Index is not
    all the way to the end), since it the remaining zeros are prescribed padding
    anyway}
  if FHashBufferIndex >= 56 then
  begin
    Compress;
    FillChar(FHashBuffer[0], 64, 0);
  end;

  {Write in LenHi (it needs it's endian order changed)}
  {LenHi is the high order word of the Length of the message in bits}
  TLongWordBuffer(FHashBuffer)[14] := ByteSwap(ULARGE_INTEGER(FHashLength).HighPart);

  {[60] is the last word in HashBuffer}
  {Write in LenLo (it needs it's endian order changed)}
  {LenLo is the low order word of the length of the message}
  TLongWordBuffer(FHashBuffer)[15] := ByteSwap(ULARGE_INTEGER(FHashLength).LowPart);

  {The hashbuffer should now be filled up}
  Compress;

  {Finalize the hash value into CurrentHash}
  SetLength(Result, Self.GetDigestSize);
  PLongWordArray(Result)[0] := ByteSwap(FABCDEBuffer[0]);
  PLongWordArray(Result)[1] := ByteSwap(FABCDEBuffer[1]);
  PLongWordArray(Result)[2] := ByteSwap(FABCDEBuffer[2]);
  PLongWordArray(Result)[3] := ByteSwap(FABCDEBuffer[3]);
  PLongWordArray(Result)[4] := ByteSwap(FABCDEBuffer[4]);

  {Burn all the temporary areas}
  Burn;
end;

procedure TSHA1.Initialize;
begin
  Self.Burn;
end;

procedure TSHA1.SelfTest;
begin
  //call the selftest contained in the other unit
end;

procedure TSHA1.UpdateLen(NumBytes: LongWord);
//Len is the number of bytes in input buffer
//This is eventually used to pad out the final message block with
//   the number of bits in the block (a 64-bit number)
begin
  //the HashLength is in BITS, so multiply NumBytes by 8
  Inc(FHashLength, NumBytes * 8);
end;

{ TSHA2_256 }

procedure TSHA256.Burn;
begin
  FHashLength := 0;

  FillChar(FHashBuffer[0], Length(FHashBuffer), 0);
  FHashBufferIndex := 0;

  FCurrentHash[0] := $6a09e667;
  FCurrentHash[1] := $bb67ae85;
  FCurrentHash[2] := $3c6ef372;
  FCurrentHash[3] := $a54ff53a;
  FCurrentHash[4] := $510e527f;
  FCurrentHash[5] := $9b05688c;
  FCurrentHash[6] := $1f83d9ab;
  FCurrentHash[7] := $5be0cd19;

  FInitialized := True;
end;

procedure TSHA256.Compress;
{Call this when the HashBuffer is full, and can now be dealt with}
var
  a, b, c, d, e, f, g, h: LongWord;  //temporary buffer storage#1
  t: Integer;
  s0, s1: LongWord;
  temp1, temp2: LongWord;  //temporary buffer for a single Word
  Wt: array[0..79] of LongWord;  //temporary buffer storage#2
//  tCount: integer;  //counter
  W: PLongWordArray;

const
  K: array[0..63] of LongWord = (
      $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
      $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
      $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
      $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
      $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
      $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
      $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
      $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2
  );

begin
  W := PLongWordArray(@Wt[0]);

  {1. Prepare the message schedule W from the block we're processing. Start with the first 16 bytes}
  //Move(FHashBuffer[0], W[0], SizeOf(FHashBuffer) );
  for t := 0 to 15 do
  begin
    W[t] := ByteSwap(PLongWord(@FHashBuffer[t*4])^);
//    W[tCount] := ByteSwap(W[tCount]);
  end;

  { Calculate the rest of W (16..79) }
  for t := 16 to 79 do
  begin
    s0 := RRot32(W[t-15],  7) xor RRot32(W[t-15], 18) xor (W[t-15] shr  3); //s0(W[t-15]);
    s1 := RRot32(W[t- 2], 17) xor RRot32(W[t- 2], 19) xor (W[t- 2] shr 10); //s1(W[t-2]);
    W[t]:= W[t-16] + s0 + W[t-7] + s1;
  end;

  {2.  Initialize working variables a..h by copying CurrentHash into working variables }
  a := FCurrentHash[0];
  b := FCurrentHash[1];
  c := FCurrentHash[2];
  d := FCurrentHash[3];
  e := FCurrentHash[4];
  f := FCurrentHash[5];
  g := FCurrentHash[6];
  h := FCurrentHash[7];

  {3. }
  for t := 0 to 63 do
  begin
  {$Q-}
    //S0, ch, maj, S1, temp1, temp2, 79.5 MB/s
    //ch, S0, maj, S1, temp1, temp2: 78.5 MB/s
    //S0, S1, ch, maj, temp1, temp2: 74.8 MB/s
{    S0 := RRot32(a, 2) xor RRot32(a, 13) xor RRot32(a, 22); //S0(a)
    ch :=  (e and f) xor ((not e) and g); //Choose(e,f,g)
    maj := (a and b) xor (a and c) xor (b and c); //Majority(a,b,c)
    S1 := RRot32(e, 6) xor RRot32(e, 11) xor RRot32(e, 25); //S1(e)
    temp1 := h + S1 + ch + K[t] + W[t];
    temp2 := S0 + maj;}

    //83.2 MB/s

    temp1 := h + (RRot32(e, 6) xor RRot32(e, 11) xor RRot32(e, 25)) + ((e and f) xor ((not e) and g)) + K[t] + W[t];

    h := g;
    g := f;
    f := e;
    e := d + temp1;
    d := c;

    temp2 := (RRot32(a, 2) xor RRot32(a, 13) xor RRot32(a, 22)) + ((a and b) xor (a and c) xor (b and c));

    c := b;
    b := a;
    a := temp1 + temp2;
  end;

  { Update the current hash values}
  FCurrentHash[0] := FCurrentHash[0] + a;
  FCurrentHash[1] := FCurrentHash[1] + b;
  FCurrentHash[2] := FCurrentHash[2] + c;
  FCurrentHash[3] := FCurrentHash[3] + d;
  FCurrentHash[4] := FCurrentHash[4] + e;
  FCurrentHash[5] := FCurrentHash[5] + f;
  FCurrentHash[6] := FCurrentHash[6] + g;
  FCurrentHash[7] := FCurrentHash[7] + h;

  {Reset HashBuffer index since it can now be reused}
  FHashBufferIndex := 0;
  FillChar(FHashBuffer[0], Length(FHashBuffer), 0); //empty the buffer for the next set of writes
end;

constructor TSHA256.Create;
begin
  inherited Create;

  Initialize;
end;

function TSHA256.Finalize: TBytes;
begin
  Result := Self.HashFinal;
//  Self.Initialize; HashFinal does the burn and reset
end;

function TSHA256.GetBlockSize: Integer;
begin
  Result := 64; //block size of SHA2-256 is 512 bits
end;

function TSHA256.GetDigestSize: Integer;
begin
  Result := 32; //digest size of SHA2-256 is 256 bits (32 bytes)
end;

procedure TSHA256.HashCore(const Data; DataLen: Integer);
//  Updates the state of the hash object so a correct hash value is returned at
//  the end of the data stream.
var
  bytesRemainingInHashBuffer: Integer;
  dummySize: Integer;
  buffer: PByte;
  //dataOffset: Integer;
begin
{  Parameters
  array    input for which to compute the hash code.
  ibStart  offset into the byte array from which to begin using data.
  cbSize  number of bytes in the byte array to use as data.}
  if not FInitialized then
    raise EScryptException.Create('SHA1 not initialized');

  if (DataLen = 0) then
    Exit;

  buffer := PByte(@Data);
  //dataOffset := 0;

  dummySize := DataLen;
  UpdateLen(dummySize);  //Update the Len variables given size

  while dummySize > 0 do
  begin
    bytesRemainingInHashBuffer := Length(FHashBuffer) - FHashBufferIndex;
    {HashBufferIndex is the next location to write to in hashbuffer
      Sizeof(HasBuffer) - HashBufferIndex = space left in HashBuffer}
    {cbSize is the number of bytes coming in from the user}
    if bytesRemainingInHashBuffer <= dummySize then
    begin
      {If there is enough data left in the buffer to fill the HashBuffer
        then copy enough to fill the HashBuffer}
      Move(buffer^, FHashBuffer[FHashBufferIndex], bytesRemainingInHashBuffer);
      Dec(dummySize, bytesRemainingInHashBuffer);
      Inc(buffer, bytesRemainingInHashBuffer);
      Compress;
    end
    else
    begin
{ 20070508  Ian Boyd
    If the input length was not an even multiple of HashBufferSize (64 bytes i think), then
      there was a buffer overrun. Rather than Moving and incrementing by DummySize
      it was using cbSize, which is the size of the original buffer}

      {If there isn't enough data to fill the HashBuffer...}
      {...copy as much as possible from the buffer into HashBuffer...}
      Move(buffer^, FHashBuffer[FHashBufferIndex], dummySize);
      {then move the HashBuffer Index to the next empty spot in HashBuffer}
      Inc(FHashBufferIndex, dummySize);
      {And shrink the size in the buffer to zero}
      dummySize := 0;
    end;
  end;
end;

procedure TSHA256.HashData(const Buffer; BufferLen: Integer);
begin
  Self.HashCore(Buffer, BufferLen);
end;

function TSHA256.HashFinal: TBytes;
{  This method finalizes any partial computation and returns the correct hash
  value for the data stream.}
type
  TLongWordBuffer = array[0..15] of FixedUInt;
begin
  {The final act is to tack on the size of the message}

  {Tack on the final bit 1 to the end of the data}
  FHashBuffer[FHashBufferIndex] := $80;

  {[56] is the start of the 2nd last word in HashBuffer}
  {if we are at (or past) it, then there isn't enough room for the whole
    message length (64-bits i.e. 2 words) to be added in}
  {The HashBuffer can essentially be considered full (even if the Index is not
    all the way to the end), since it the remaining zeros are prescribed padding
    anyway}
  if FHashBufferIndex >= 56 then
    Compress;

  {Write in LenHi (it needs it's endian order changed)}
  {LenHi is the high order word of the Length of the message in bits}
  TLongWordBuffer(FHashBuffer)[14] := ByteSwap(ULARGE_INTEGER(FHashLength).HighPart);

  {[60] is the last word in HashBuffer}
  {Write in LenLo (it needs it's endian order changed)}
  {LenLo is the low order word of the length of the message}
  TLongWordBuffer(FHashBuffer)[15] := ByteSwap(ULARGE_INTEGER(FHashLength).LowPart);

  {The hashbuffer should now be filled up}
  Compress;

  {Finalize the hash value into CurrentHash}
  SetLength(Result, Self.GetDigestSize);
  PLongWordArray(Result)[0]:= ByteSwap(FCurrentHash[0]);
  PLongWordArray(Result)[1]:= ByteSwap(FCurrentHash[1]);
  PLongWordArray(Result)[2]:= ByteSwap(FCurrentHash[2]);
  PLongWordArray(Result)[3]:= ByteSwap(FCurrentHash[3]);
  PLongWordArray(Result)[4]:= ByteSwap(FCurrentHash[4]);
  PLongWordArray(Result)[5]:= ByteSwap(FCurrentHash[5]);
  PLongWordArray(Result)[6]:= ByteSwap(FCurrentHash[6]);
  PLongWordArray(Result)[7]:= ByteSwap(FCurrentHash[7]);

  {Burn all the temporary areas}
  Burn;
end;

procedure TSHA256.Initialize;
begin
  Self.Burn;

  FInitialized := True;
end;

procedure TSHA256.UpdateLen(NumBytes: LongWord);
//Len is the number of bytes in input buffer
//This is eventually used to pad out the final message block with
//   the number of bits in the block (a 64-bit number)
begin
  //the HashLength is in BITS, so multiply NumBytes by 8
  Inc(FHashLength, NumBytes * 8);
end;

{$IFNDEF FPC}

{ TSHA256CryptoServiceProvider }

const
  advapi32 = 'advapi32.dll';
const
  PROV_RSA_AES = 24; //Provider type; from WinCrypt.h
  MS_ENH_RSA_AES_PROV_W: String = 'Microsoft Enhanced RSA and AES Cryptographic Provider'; //Provider name
  MS_ENH_RSA_AES_PROV_XP_W: String = 'Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype)'; //Provider name
  // dwFlags definitions for CryptAcquireContext
  CRYPT_VERIFYCONTEXT = $F0000000;

  // dwParam
  KP_IV =     1;  // Initialization vector
  KP_MODE =   4;  // Mode of the cipher

  // KP_MODE
  CRYPT_MODE_CBC =       1;       // Cipher block chaining
  CRYPT_MODE_ECB =       2;       // Electronic code book
  CRYPT_MODE_OFB =       3;       // Output feedback mode
  CRYPT_MODE_CFB =       4;       // Cipher feedback mode
  CRYPT_MODE_CTS =       5;       // Ciphertext stealing mode
  CRYPT_MODE_CBCI =     6;   // ANSI CBC Interleaved
  CRYPT_MODE_CFBP =     7;   // ANSI CFB Pipelined
  CRYPT_MODE_OFBP =     8;   // ANSI OFB Pipelined
  CRYPT_MODE_CBCOFM =     9;   // ANSI CBC + OF Masking
  CRYPT_MODE_CBCOFMI =   10;  // ANSI CBC + OFM Interleaved

  HP_HASHVAL =         $0002;
  HP_HASHSIZE =         $0004;

  PLAINTEXTKEYBLOB = $8;
  CUR_BLOB_VERSION = 2;

  ALG_CLASS_DATA_ENCRYPT =   (3 shl 13);
  ALG_TYPE_BLOCK =         (3 shl 9);
  ALG_SID_AES_128 =       14;
  ALG_SID_AES_256 =       16;

  CALG_AES_128 = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_128);
  CALG_AES_256 = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_256);

function CryptAcquireContext(out phProv: HCRYPTPROV; pszContainer: PWideChar; pszProvider: PWideChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall; external advapi32 name 'CryptAcquireContextW';
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: DWORD): BOOL; stdcall; external advapi32;
function CryptGenRandom(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: Pointer): BOOL; stdcall; external advapi32;
function CryptCreateHash(hProv: HCRYPTPROV; Algid: ALG_ID; hKey: HCRYPTKEY; dwFlags: DWORD; out hHash: HCRYPTHASH): BOOL; stdcall; external advapi32;
function CryptHashData(hHash: HCRYPTHASH; pbData: PByte; dwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall; external advapi32;
function CryptGetHashParam(hHash: HCRYPTHASH; dwParam: DWORD; pbData: PByte; var dwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall; external advapi32;
function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall; external advapi32;

//function CryptImportKey(hProv: HCRYPTPROV; pbData: PByte; dwDataLen: DWORD; hPubKey: HCRYPTKEY; dwFlags: DWORD; out phKey: HCRYPTKEY): BOOL; stdcall; external advapi32;
//function CryptSetKeyParam(hKey: HCRYPTKEY; dwParam: DWORD; pbData: PByte; dwFlags: DWORD): BOOL; stdcall; external advapi32;
//function CryptDestroyKey(hKey: HCRYPTKEY): BOOL; stdcall; external advapi32;
//function CryptEncrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL; dwFlags: DWORD; pbData: PByte; var pdwDataLen: DWORD; dwBufLen: DWORD): BOOL; stdcall; external advapi32;
//function CryptDecrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL; dwFlags: DWORD; pbData: PByte; var pdwDataLen: DWORD): BOOL; stdcall; external advapi32;


{ TSHA1csp }

procedure TCspHash.Burn;
var
  le: DWORD;
begin
  if FHash = 0 then
    Exit;

  try
    if not CryptDestroyHash(FHash) then
    begin
      le := GetLastError;
      RaiseOSError(le, Format('Could not destroy current hash provider: %s (%d)', [SysErrorMessage(le), le]));
      Exit;
    end;
  finally
    FHash := 0;
  end;
end;

constructor TCspHash.Create(AlgorithmID: Cardinal; BlockSize: Integer);
var
  providerName: String;
  provider: HCRYPTPROV;
  le: DWORD;
const
  PROV_RSA_AES = 24; //Provider type; from WinCrypt.h
//  MS_ENH_RSA_AES_PROV_W: String = 'Microsoft Enhanced RSA and AES Cryptographic Provider'; //Provider name
//  MS_ENH_RSA_AES_PROV_XP_W: String = 'Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype)'; //Provider name

begin
  inherited Create;

  {
    Set ProviderName to either
      providerName = "Microsoft Enhanced RSA and AES Cryptographic Provider"
      providerName = "Microsoft Enhanced RSA and AES Cryptographic Provider (Prototype)"  //Windows XP and earlier
  }
{  providerName := MS_ENH_RSA_AES_PROV_W;
  //Before Vista it was a prototype provider
  if (Win32MajorVersion < 6) then //6.0 was Vista and Server 2008
    providerName := MS_ENH_RSA_AES_PROV_XP_W;}

//  if not CryptAcquireContext(provider, nil, PWideChar(providerName), PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
  if not CryptAcquireContext({out}provider, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
  begin
    le := GetLastError;
    RaiseOSError(le, Format('Could not acquire context to provider "%s" (Win32MajorVersion=%d)',
        [providerName, Win32MajorVersion]));
  end;

  FProvider := provider;
  FAlgorithmID := AlgorithmID;
  FBlockSize := BlockSize; //SHA1: 64 bytes, SHA256: 64 bytes

  Self.Initialize;
end;

destructor TCspHash.Destroy;
begin
  {
    //If there is a hash, we must destroy it before releasing the context (the hash keeps an internal reference to the provider,
    which would become invalid and possibly crash)
    https://blogs.msdn.microsoft.com/oldnewthing/20160127-00/?p=92934
  }
  Self.Burn;

  if FProvider <> 0 then
  begin
    CryptReleaseContext(FProvider, 0);
    FProvider := 0;
  end;

  inherited;
end;

function TCspHash.Finalize: TBytes;
begin
  Result := Self.HashFinal;
  Self.Initialize; //Get ready for another round of hashing
end;

function TCspHash.GetBlockSize: Integer;
begin
//  Result := 64; //block size of SHA1 is 64 bytes (512 bits)
  Result := FBlockSize; //64 for SHA1, 64 for SHA256
end;

function TCspHash.GetDigestSize: Integer;
var
  dataLen: Cardinal;
begin
//  Result := 20; //digest size of SHA-1 is 160 bits (20 bytes)

  //Works. But strictly speaking you must first call HP_HASHSIZE with nil,
  //in order to find out you must pass it a 4-byte integer to hold the resulting size
//  dataLen := SizeOf(Result);
//  if not CryptGetHashParam(FHash, HP_HASHSIZE, @Result, dataLen, 0) then
//    RaiseLastOSError;

  //Fails with 234 (More data is available)
//  if not CryptGetHashParam(FHash, HP_HASHVAL, @Result, dataLen, 0) then
//    RaiseLastOSError;

  //https://blogs.msdn.microsoft.com/oldnewthing/20160128-00/?p=92941
  //Use nil, and HP_HASHVAL will work
  if not CryptGetHashParam(FHash, HP_HASHVAL, nil, {var}dataLen, 0) then
    RaiseLastOSError;

  Result := Integer(dataLen);
end;

procedure TCspHash.HashCore(const Data; DataLen: Integer);
var
  le: DWORD;
begin
  if (FHash = 0) then
    raise ELibraryException.create('TCspHash is not initialized');

  if not CryptHashData(FHash, PByte(@Data), DataLen, 0) then
  begin
    le := GetLastError;
    raise ELibraryException.createFmt('Error hashing data: %s (%d)', [SysErrorMessage(le), le]);
  end;
end;

procedure TCspHash.HashData(const Buffer; BufferLen: Integer);
begin
  Self.HashCore(Buffer, BufferLen);
end;

function TCspHash.HashFinal: TBytes;
var
  digestSize: DWORD;
  le: DWORD;
begin
  digestSize := Self.GetDigestSize;
  SetLength(Result, digestSize);

  if not CryptGetHashParam(FHash, HP_HASHVAL, @Result[0], digestSize, 0) then
  begin
    le := GetLastError;
    raise ELibraryException.createFmt('Could not get Hash value from CSP: %s (%d)', [SysErrorMessage(le), le]);
  end;
end;

procedure TCspHash.Initialize;
var
  le: DWORD;
  hash: THandle;
const
  SCouldNotCreate = 'Could not create hash: %s (0x%.8x)';
begin
  Self.Burn;

  if not CryptCreateHash(FProvider, FAlgorithmID, 0, 0, {out}hash) then
  begin
    le := GetLastError;
    RaiseOSError(le, Format(SCouldNotCreate, [SysErrorMessage(le), le]));
    Exit;
  end;

  FHash := hash;
end;

{ TSHA1Cng }

procedure TCngHash.Burn;
begin
  if FHash <> 0 then
  begin
    _BCryptDestroyHash(FHash);
    FHash := 0;
    ZeroMemory(@FHashObjectBuffer[0], Length(FHashObjectBuffer));
  end;
end;

constructor TCngHash.Create(const AlgorithmID: WideString; const Provider: PWideChar; HmacMode: Boolean);
var
  nts: NTSTATUS;
  algorithm: BCRYPT_ALG_HANDLE;
  objectLength: DWORD;
  bytesReceived: Cardinal;
  dwFlags: Cardinal;
begin
  inherited Create;

  Self.RequireBCrypt;

  FHmacMode := HmacMode;
  if HmacMode then
    dwFlags := BCRYPT_ALG_HANDLE_HMAC_FLAG
  else
    dwFlags := 0;

  nts := _BCryptOpenAlgorithmProvider({out}algorithm,
      PWideChar(AlgorithmID), //Algorithm: e.g. BCRYPT_SHA1_ALGORITHM ("SHA1")
      Provider, //Provider. nil ==> default
      dwFlags
      );
  NTStatusCheck(nts);

  FAlgorithm := algorithm;

  //Get Algorithm block size
  FAlgorithmBlockSize := TCngHash.GetBcryptAlgorithmBlockSize(FAlgorithm);
  FAlgorithmDigestSize := TCngHash.GetBcryptAlgorithmDigestSize(FAlgorithm);

  //Get the length of the hash data object, so we can provide the required buffer
  nts := _BCryptGetProperty(algorithm,
      PWideChar(BCRYPT_OBJECT_LENGTH), @objectLength, SizeOf(objectLength), {out}bytesReceived, 0);
  NTStatusCheck(nts);

  SetLength(FHashObjectBuffer, objectLength);
end;

destructor TCngHash.Destroy;
begin
  Self.Burn;

  if FAlgorithm <> 0 then
  begin
    _BCryptCloseAlgorithmProvider(FAlgorithm, 0);
    FAlgorithm := 0;
  end;

  inherited;
end;

function TCngHash.Finalize: TBytes;
begin
  if FHmacMode then
    raise EScryptException.Create('Cng hash object is in HMAC mode');

  Result := Self.HashFinal(FHash);

  Self.Burn;
end;

function TCngHash.GetBlockSize: Integer;
begin
  Result := FAlgorithmBlockSize;
end;

function TCngHash.GetDigestSize: Integer;
begin
  Result := FAlgorithmDigestSize;
end;

class function TCngHash.GetBcryptAlgorithmBlockSize(Algorithm: BCRYPT_HASH_HANDLE): Integer;
const
  BCRYPT_HASH_BLOCK_LENGTH: WideString = 'HashBlockLength';
var
  blockSize: DWORD;
  bytesReceived: Cardinal;
  nts: NTSTATUS;
begin
  //Get the hash block size
  nts := _BCryptGetProperty(Algorithm, PWideChar(BCRYPT_HASH_BLOCK_LENGTH), @blockSize, SizeOf(blockSize), {out}bytesReceived, 0);
  NTStatusCheck(nts);

  Result := Integer(blockSize);
end;

class function TCngHash.GetBcryptAlgorithmDigestSize(Algorithm: BCRYPT_ALG_HANDLE): Integer;
const
  BCRYPT_HASH_LENGTH = 'HashDigestLength';
var
  digestSize: DWORD;
  bytesReceived: Cardinal;
  nts: NTSTATUS;
begin
  //Get the length of the hash digest
  nts := _BCryptGetProperty(Algorithm, BCRYPT_HASH_LENGTH, @digestSize, SizeOf(digestSize), {out}bytesReceived, 0);
  NTStatusCheck(nts);

  Result := Integer(digestSize);
end;

procedure TCngHash.HashCore(Hash: BCRYPT_HASH_HANDLE; const Data; DataLen: Integer);
var
  hr: NTSTATUS;
begin
  hr := _BCryptHashData(Hash, Pointer(@Data), DataLen, 0);
  NTStatusCheck(hr);
end;

function TCngHash.HashData(const Key; KeyLen: Integer; const Data; DataLen: Integer): TBytes;
var
  hash: BCRYPT_HASH_HANDLE;
  hr: NTSTATUS;
begin
  ZeroMemory(@FHashObjectBuffer[0], Length(FHashObjectBuffer));

  hr := _BCryptCreateHash(FAlgorithm, {out}hash, @FHashObjectBuffer[0], Length(FHashObjectBuffer), Pointer(@Key), KeyLen, 0);
  NTStatusCheck(hr);
  try
    Self.HashCore(hash, Data, DataLen);

    Result := Self.HashFinal(hash);
  finally
    _BCryptDestroyHash(hash);
    ZeroMemory(@FHashObjectBuffer[0], Length(FHashObjectBuffer));
  end;
end;

procedure TCngHash.HashData(const Buffer; BufferLen: Integer);
begin
  if FHmacMode then
    raise EScryptException.Create('Cng hash object is in HMAC mode');

  Self.Initialize;
  Self.HashCore(FHash, Buffer, BufferLen);
end;

function TCngHash.HashFinal(Hash: BCRYPT_HASH_HANDLE): TBytes;
var
  digestSize: DWORD;
  hr: NTSTATUS;
begin
  digestSize := Self.GetDigestSize;
  SetLength(Result, digestSize);

  hr :=_BCryptFinishHash(Hash, @Result[0], digestSize, 0);
  NTStatusCheck(hr);
end;

procedure TCngHash.Initialize;
var
  hash: BCRYPT_HASH_HANDLE;
  hr: NTSTATUS;
begin
  if FHash = 0 then
  begin
    hr := _BCryptCreateHash(FAlgorithm, {out}hash, @FHashObjectBuffer[0], Length(FHashObjectBuffer), nil, 0, 0);
    NTStatusCheck(hr);

    FHash := hash;
  end;
end;

class function TCngHash.InitializeBCrypt: Boolean;
var
  moduleHandle: HMODULE;
  available: Boolean;

  function GetProcedureAddress(procedureName: AnsiString; var FunctionFound: Boolean): Pointer;
  begin
    Result := GetProcAddress(moduleHandle, PAnsiChar(procedureName));
    if Result = nil then
      FunctionFound := False;
  end;
const
  BCrypt = 'BCrypt.dll';
begin
  if (not _BCryptInitialized) then
  begin
    moduleHandle := SafeLoadLibrary(PChar(BCrypt));
    if moduleHandle <> 0 then
    begin
      available := True;

      _BCryptOpenAlgorithmProvider := GetProcedureAddress('BCryptOpenAlgorithmProvider', available);
      _BCryptCloseAlgorithmProvider := GetProcedureAddress('BCryptCloseAlgorithmProvider', available);
      _BCryptGenRandom := GetProcedureAddress('BCryptGenRandom', available);
      _BCryptCreateHash := GetProcedureAddress('BCryptCreateHash', available);
      _BCryptHashData := GetProcedureAddress('BCryptHashData', available);
      _BCryptFinishHash := GetProcedureAddress('BCryptFinishHash', available);
      _BCryptDestroyHash := GetProcedureAddress('BCryptDestroyHash', available);
      _BCryptGetProperty := GetProcedureAddress('BCryptGetProperty', available);
      _BCryptDeriveKeyPBKDF2 := GetProcedureAddress('BCryptDeriveKeyPBKDF2', available);

      _BCryptAvailable := available;
    end;
    _BCryptInitialized := True;
  end;

  Result := _BCryptAvailable;
end;

class function TCngHash.IsAvailable: Boolean;
begin
  Result := TCngHash.InitializeBCrypt;
end;

procedure TCngHash.RequireBCrypt;
begin
  if not TCngHash.InitializeBCrypt then
    raise ELibraryException.create('BCrypt not available. Requires Windows Vista or greater');
end;

{$ENDIF}

{ THmac }

constructor THmac.Create(Hash: IHashAlgorithm);
begin
  inherited Create;

  FHash := Hash;
end;

function THmac.GetDigestSize: Integer;
begin
  Result := FHash.DigestSize;
end;

function THmac.HashData(const Key; KeyLen: Integer; const Data; DataLen: Integer): TBytes;
var
  oKeyPad, iKeyPad: TBytes;
  i, n: Integer;
  digest: TBytes;
  blockSize: Integer;

type
  PUInt64Array = ^TUInt64Array_Unsafe;
  TUInt64Array_Unsafe = array[0..MaxInt div 16] of UInt64;

begin
  {
    Implementation of RFC2104  HMAC: Keyed-Hashing for Message Authentication

    Tested with known test vectors from RFC2202: Test Cases for HMAC-MD5 and HMAC-SHA-1
  }
  blockSize := FHash.BlockSize;

  // Clear pads
  SetLength(oKeyPad, blockSize); //elements will be initialized to zero by SetLength
  SetLength(iKeyPad, blockSize); //elements will be initialized to zero by SetLength

  // if key is longer than blocksize: reset it to key=Hash(key)
  if KeyLen > blockSize then
  begin
    FHash.HashData(Key, KeyLen);
    digest := FHash.Finalize;

    //Store hashed key in pads
    Move(digest[0], iKeyPad[0], Length(digest)); //remaining bytes will remain zero
    Move(digest[0], oKeyPad[0], Length(digest)); //remaining bytes will remain zero
  end
  else
  begin
    //Store original key in pads
    Move(Key, iKeyPad[0], KeyLen); //remaining bytes will remain zero
    Move(Key, oKeyPad[0], KeyLen); //remaining bytes will remain zero
  end;

  {
    Xor key with ipad and ipod constants
      iKeyPad = key xor 0x36
      oKeyPad = key xor 0x5c

    DONE: Unroll this to blockSize div 4 xor's of $5c5c5c5c and $36363636
  }
  n := blockSize div SizeOf(UInt64);
  for i := 0 to n-1 do
    PUInt64Array(@oKeyPad[0])[i] := PUInt64Array(@oKeyPad[0])[i] xor UInt64($5c5c5c5c5c5c5c5c);
  for i := 0 to n-1 do
    PUInt64Array(@iKeyPad[0])[i] := PUInt64Array(@iKeyPad[0])[i] xor UInt64($3636363636363636);
  n := blockSize mod SizeOf(UInt64);
  if n <> 0 then
  begin
    //This should never happen in practice.
    //Hash block sizes are going to be multiple of 8 bytes
    for i := blockSize-1-n to blockSize-1 do
    begin
      oKeyPad[i] := oKeyPad[i] xor $5c;
      iKeyPad[i] := iKeyPad[i] xor $36;
    end;
  end;

  {
    Result := hash(oKeyPad || hash(iKeyPad || message))
  }
  // Perform inner hash: digest = Hash(iKeyPad || data)
  SetLength(iKeyPad, blockSize+DataLen);
  Move(data, iKeyPad[blockSize], DataLen);
  FHash.HashData(iKeyPad[0], Length(iKeyPad));
  digest := FHash.Finalize;

  // perform outer hash: result = Hash(oKeyPad || digest)
  SetLength(oKeyPad, blockSize+Length(digest));
  Move(digest[0], oKeyPad[blockSize], Length(digest));
  FHash.HashData(oKeyPad[0], Length(oKeyPad));
  Result := FHash.Finalize;
end;

{ TRfc2898DeriveBytes }

constructor TRfc2898DeriveBytes.Create(HMAC: IHmacAlgorithm);
begin
  inherited Create;

  FHMAC := HMAC;
end;

function TRfc2898DeriveBytes.GetBytes(const Password: String; const Salt; const SaltLength: Integer;
  IterationCount, DesiredBytes: Integer): TBytes;
var
  Ti: TBytes;
  V: TBytes;
  U: TBytes;
  hLen: Integer; //HMAC size in bytes
  cbSalt: Integer;
  l, r, i, j: Integer;
  dwULen: DWORD;
  derivedKey: TBytes;
  utf8Password: TBytes;
begin
  {
    Password-Based Key Derivation Function 2

    Implementation of RFC2898
        PKCS #5: Password-Based Cryptography Specification Version 2.0
        http://tools.ietf.org/html/rfc2898

    Given an arbitrary "password" string, and optionally some salt, PasswordKeyDeriveBytes
    can generate n bytes, suitable for use as a cryptographic key.

    e.g. AES commonly uses 128-bit (16 byte) or 256-bit (32 byte) keys.

    Tested with test vectors from RFC6070
        PKCS #5: Password-Based Key Derivation Function 2 (PBKDF2)  Test Vectors
        http://tools.ietf.org/html/rfc6070
  }
//  if DerivedKeyLength > 2^32*hLen then
//    raise ELibraryException.create('Derived key too long');

  if FHMAC = nil then
    raise EScryptException.Create('No HMAC algorithm supplied');

  hLen := FHMAC.DigestSize;

  l := Ceil(DesiredBytes / hLen);
  r := DesiredBytes - (l-1)*hLen;

  cbSalt := SaltLength;

  SetLength(Ti, hLen);
  SetLength(V,  hLen);
  SetLength(U,  Max(cbSalt+4, hLen));

  SetLength(derivedKey, DesiredBytes);

  utf8Password := TScrypt.StringToUtf8(Password);

  for i := 1 to l do
  begin
    FillChar(Ti[0], hLen, $0);
    for j := 1 to IterationCount do
    begin
      if j = 1 then
      begin
        //It's the first iteration, construct the input for the hmac function
        if cbSalt > 0 then
          Move(Salt, u[0], cbSalt);
        U[cbSalt]    := Byte((i and $FF000000) shr 24);
        U[cbSalt+ 1] := Byte((i and $00FF0000) shr 16);
        U[cbSalt+ 2] := Byte((i and $0000FF00) shr  8);
        U[cbSalt+ 3] := Byte((i and $000000FF)       );
        dwULen := cbSalt + 4;
      end
      else
      begin
        Move(V[0], U[0], hLen); //memcpy(U, V, hlen);
        dwULen := hLen;
      end;

      //Run Password and U through HMAC to get digest V
      V := FHMAC.HashData(utf8Password[0], Length(utf8Password), U[0], dwULen);

      //Ti <- Ti xor V
      TScrypt.XorBlockInPlace({var}Ti[0], V[0], hlen);
    end;

    if (i <> l) then
    begin
      Move(Ti[0], derivedKey[(i-1)*hLen], hLen); //memcpy(derivedKey[(i-1) * hlen], Ti, hlen);
    end
    else
    begin
      // Take only the first r bytes
      Move(Ti[0], derivedKey[(i-1)*hLen], r); //memcpy(derivedKey[(i-1) * hlen], Ti, r);
    end;
  end;

  Result := derivedKey;
end;

{$IFNDEF FPC}

{ TBCryptDeriveKeyPBKDF2 }

constructor TBCryptDeriveKeyPBKDF2.Create(const AlgorithmID: String; const Provider: PWideChar);
var
  hr: NTSTATUS;
  alg: BCRYPT_ALG_HANDLE;
begin
  inherited Create;

  hr := _BCryptOpenAlgorithmProvider({out}alg, PWideChar(AlgorithmID), Provider, BCRYPT_ALG_HANDLE_HMAC_FLAG);
  NTStatusCheck(hr);

  FAlgorithm := alg;
end;

function TBCryptDeriveKeyPBKDF2.GetBytes(const Password: String; const Salt; const SaltLength: Integer;
    IterationCount, DesiredBytes: Integer): TBytes;
var
  utf8Password: TBytes;
  hr: NTSTATUS;
begin
  SetLength(Result, DesiredBytes);

  utf8Password := TScrypt.StringToUtf8(Password); //needs to be before following Exit to avoid "'utf8Password' might not have been initialized" compiler bug

  if DesiredBytes = 0 then
    Exit;


  hr := _BCryptDeriveKeyPBKDF2(FAlgorithm,
      Pointer(utf8Password), Length(utf8Password),
      @Salt, SaltLength,
      IterationCount,
      Pointer(Result), Length(Result),
      0);
  NTStatusCheck(hr);
end;
{$ENDIF}

end.
