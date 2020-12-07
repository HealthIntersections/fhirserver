unit fsl_tests_scrypt;

{
see https://github.com/JackTrapper/scrypt-for-delphi
}

{$i fhir.inc}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils,
  fsl_testing, fsl_scrypt;

type
  TScryptTests = class (TFslTestCase)
  protected
    FScrypt: TScrypt;
    FFreq: Int64;
    Procedure SetUp; override;
    procedure TearDown; override;

    function GetTimestamp: Int64;

    class function StringToUtf8Bytes(const s: UnicodeString): TBytes;

    procedure Tester_HMAC_SHA1(HMACsha1: IHmacAlgorithm);
    procedure Tester_HMAC_SHA256(HMACsha256: IHmacAlgorithm);
    procedure Tester_PBKDF2_SHA1(Pbkdf: IPBKDF2Algorithm);
    procedure Tester_PBKDF2_SHA256(Pbkdf2sha256: IPBKDF2Algorithm);

    procedure Test_Scrypt_PasswordFormatting; //todo
  public
    procedure Benchmark_SHA1_PurePascal; //because native code should be fast
    procedure Benchmark_SHA256_PurePascal;
    procedure Benchmark_Hashes; //Compares SHA1 pure, Csp, Cng, SHA256 pure, Csp, Cng
    procedure Benchmark_HMACs;
    procedure Benchmark_PBKDF2s;
    procedure ScryptBenchmarks; //Duration for different cost factor (N) and block size factor (r)
  published
    procedure Test_Base64;

    //Even though we don't use SHA-1, we implemented it because PBKDF2_SHA1 is the only one with published test vectors
    procedure Test_SHA1;
    procedure Test_SHA1_PurePascal;
    {$IFNDEF FPC}
    procedure Test_SHA1_Csp;
                procedure Test_SHA1_Cng;
                {$ENDIF}

    //Scrypt uses PBKDF2_SHA256. We fallback through Cng -> Csp -> PurePascal
    procedure Test_SHA256;
    procedure Test_SHA256_PurePascal;
    {$IFNDEF FPC}procedure Test_SHA256_Csp;{$ENDIF}
    {$IFNDEF FPC}procedure Test_SHA256_Cng;{$ENDIF}

    //PBKDF2 uses HMAC. Test our implementation with known SHA1 and SHA256 test vectors
    procedure Test_HMAC_SHA1;
    procedure Test_HMAC_SHA1_PurePascal;
    {$IFNDEF FPC}procedure Test_HMAC_SHA1_Cng;{$ENDIF}

    procedure Test_HMAC_SHA256;
    procedure Test_HMAC_SHA256_PurePascal;
    {$IFNDEF FPC}procedure Test_HMAC_SHA256_Cng;{$ENDIF}

    //Test PBKDF implementations; test with known SHA1 and SHA256 test vectors
    procedure Test_PBKDF2_SHA1;
    procedure Test_PBKDF2_SHA1_PurePascal;
    {$IFNDEF FPC}procedure Test_PBKDF2_SHA1_Cng;{$ENDIF}

    procedure Test_PBKDF2_SHA256;
    procedure Test_PBKDF2_SHA256_PurePascal;
    {$IFNDEF FPC}procedure Test_PBKDF2_SHA256_Cng;{$ENDIF}

    //Salsa 20/8, BlockMix, and ROMix are the heart of scrypt. PBKDF2 is for key derivation. These are used for expensive salt
    procedure Test_Salsa208Core;
    procedure Test_BlockMix;
    procedure Test_ROMix;
    procedure Test_Scrypt; //official scrypt test vectors

    //Password hashing
    procedure Test_PasswordHashing; //Test, and verify, "correct horse battery staple"
    procedure Test_JavaWgScrypt; //the only other example out there
    {$IFNDEF FPC}procedure Test_RehashNeededKicksIn;{$ENDIF}
  end;

  TSHA1Tester = class(TObject)
  protected
    FSha1: IHashAlgorithm;
    procedure SelfTest_Sizes; //block and digest sizes are right
    procedure SelfTestA; //FIPS-180 AppendixA Test
    procedure SelfTestB; //FIPS-180 AppendixB Test
    procedure SelfTestC; //FIPS-180 AppendixC Test
    procedure SelfTestD; //string i found that crashes the algoritm
  public
    constructor Create(SHA1: IHashAlgorithm);
    destructor Destroy; override;

    class procedure Test(SHA1: IHashAlgorithm);
   end;

  TSHA256Tester = class(TObject)
  private
    FSHA256: IHashAlgorithm;
    function HexStringToBytes(s: string): TBytes;
    function StringOfString(const s: string; Count: Integer): string;
    procedure t(const s: string; expectedHash: string);
    procedure tb(const Buffer; BufferLength: Integer; ExpectedHash: string);
  protected
    procedure TestSizes;
    procedure OfficialVectors;
    procedure OfficialVectors_HugeBuffers; //hashing of 100MB, or even 1.6 GB buffers - slow and out of memory
    procedure UnofficialVectors;
  public
    constructor Create(SHA256: IHashAlgorithm);
    destructor Destroy; override;

    class procedure Test(SHA256: IHashAlgorithm);
   end;

  TScryptCracker = class(TScrypt)
  public
  end;

procedure registerTests;

implementation

function HexToBytes(s: string): TBytes;
var
  i, j: Integer;
  n: Integer;
begin
  for i := Length(s) downto 1 do
  begin
    if s[i] = ' ' then
      Delete(s, i, 1);
  end;

  SetLength(Result, Length(s) div 2);

  i := 1;
  j := 0;
  while (i < Length(s)) do
  begin
    n := StrToInt('0x'+s[i]+s[i+1]);
    Result[j] := n;
    Inc(i, 2);
    Inc(j, 1);
  end;
end;



{ TSHA1Tests }

constructor TSHA1Tester.Create(SHA1: IHashAlgorithm);
begin
  inherited Create;

  FSha1 := SHA1;
end;

destructor TSHA1Tester.Destroy;
begin
  FSHA1 := nil;

  inherited;
end;

procedure TSHA1Tester.SelfTestA;
const
  Answer: array[0..19] of Byte =
    ($A9, $99, $3E, $36,
     $47, $06, $81, $6A,
     $BA, $3E, $25, $71,
     $78, $50, $C2, $6C,
     $9C, $D0, $D8, $9D);
var
  szInData: UnicodeString;
  testData: TBytes;
  digest: TBytes;
begin
  {This is the test data from FIPS-180 Appendix A}
  szInData := 'abc';
  testData := TScryptTests.StringToUtf8Bytes(szInData);

  FSha1.HashData(testData[0], Length(testData));
  Digest := FSha1.Finalize;

  if not CompareMem(@digest[0], @Answer[0], Length(Answer)) then
    raise EScryptException.Create('SHA-1 self-test A failed');
end;

procedure TSHA1Tester.SelfTestB;
const
  Answer: array[0..19] of Byte =
    ($84, $98, $3E, $44,
     $1C, $3B, $D2, $6E,
     $BA, $AE, $4A, $A1,
     $F9, $51, $29, $E5,
     $E5, $46, $70, $F1);
var
  szInData: UnicodeString;
  testData: TBytes;
  digest: TBytes;
begin
  {This is the test data from FIPS-180 Appendix B}
  szInData := 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
  testData := TScryptTests.StringToUtf8Bytes(szInData);

  FSha1.HashData(TestData[0], Length(TestData));
  digest := FSha1.Finalize;

  if not CompareMem(@digest[0], @Answer[0], Sizeof(Answer)) then
    raise EScryptException.Create('SHA-1 self-test B failed');
end;

procedure TSHA1Tester.SelfTestC;
const
  Answer: array[0..19] of Byte =
    ($34, $AA, $97, $3C,
     $D4, $C4, $DA, $A4,
     $F6, $1E, $EB, $2B,
     $DB, $AD, $27, $31,
     $65, $34, $01, $6F);
var
  digest: TBytes;
  testData: TBytes;
  testValue: Byte;
begin
  {Build a string which consists of 1,000,000 repetitions of "a"}
  testValue := Ord('a');
  SetLength(TestData, 1000000);
  FillChar(TestData[0], 1000000, TestValue);

  FSha1.HashData(testData[0], Length(testData));
  digest := FSha1.Finalize;

  if not CompareMem(@Digest[0], @Answer[0], Length(Answer)) then
    raise EScryptException.Create('SHA-1 self-test C failed');
end;

procedure TSHA1Tester.SelfTestD;
const
  Answer: array[0..19] of Byte =
    ($85, $B6, $95, $33,
     $89, $5F, $9C, $08,
     $18, $4F, $1F, $16,
     $3C, $91, $51, $FD,
     $47, $B1, $E4, $9E);
var
  digest: TBytes;
  TestData: TBytes;
  TestValue: Byte;
begin
  {Build a string which consists of 202 repetitions of "a"}
  TestValue := Ord('a');
  SetLength(TestData, 202);
  FillChar(TestData[0], 202, TestValue);

  FSha1.HashData(TestData[0], Length(testData));
  digest := FSha1.Finalize;

  if not CompareMem(@digest[0], @Answer[0], Sizeof(Answer)) then
    raise EScryptException.Create('SHA-1 self-test A failed');
end;

procedure TSHA1Tester.SelfTest_Sizes;
begin
  if FSha1.BlockSize <> 64 then
    raise EScryptException.CreateFmt('SHA1 block size (%d) was not 64 bytes', [FSha1.BlockSize]);

  if FSha1.DigestSize <> 20 then
    raise EScryptException.CreateFmt('SHA1 digest size (%d) was not 20 bytes', [FSha1.DigestSize]);
end;

class procedure TSHA1Tester.Test(SHA1: IHashAlgorithm);
var
  tester: TSHA1Tester;
begin
  tester := TSHA1Tester.Create(SHA1);
  try
    tester.SelfTest_Sizes;
     tester.SelfTestA;
    tester.SelfTestB;
    tester.SelfTestC;
    tester.SelfTestD;
   finally
    tester.Free;
   end;
end;

{ TScryptTests }

procedure TScryptTests.Test_SHA256;
var
  sha256: IHashAlgorithm;
begin
  sha256 := TScryptCracker.CreateObject('SHA256') as IHashAlgorithm;
  TSHA256Tester.Test(sha256);
  assertPass();
end;

{$IFNDEF FPC}
procedure TScryptTests.Test_SHA256_Cng;
var
  sha256: IHashAlgorithm;
begin
  sha256 := TScryptCracker.CreateObject('SHA256.Cng') as IHashAlgorithm;
  TSHA256Tester.Test(sha256);
  assertPass();
end;

procedure TScryptTests.Test_SHA256_Csp;
var
  sha256: IHashAlgorithm;
begin
  sha256 := TScryptCracker.CreateObject('SHA256.Csp') as IHashAlgorithm;
  TSHA256Tester.Test(sha256);
  assertPass();
end;
{$ENDIF}

procedure TScryptTests.Test_SHA256_PurePascal;
var
  sha256: IHashAlgorithm;
begin
  sha256 := TScryptCracker.CreateObject('SHA256.PurePascal') as IHashAlgorithm;
  TSHA256Tester.Test(sha256);
  assertPass();
end;

procedure TScryptTests.SetUp;
begin
  inherited;

  FScrypt := TScryptCracker.Create;
        {$IFDEF WINDOWS}
  if not QueryPerformanceFrequency(FFreq) then //it's documented to never fail, but it can (see SO).
    FFreq := -1;
        {$ENDIF}
end;

procedure TScryptTests.TearDown;
begin
  FreeAndNil(FScrypt);

  inherited;
end;

class function TScryptTests.StringToUtf8Bytes(const s: UnicodeString): TBytes;
var
  strLen: Integer;
begin
  strLen := Length(s);

  if strLen = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end
  else
    result := TEncoding.UTF8.getBytes(s);
end;

procedure TScryptTests.Test_JavaWgScrypt;
var
  passwordRehashNeeded: Boolean;
begin
  {
    From: https://github.com/wg/scrypt

    Passwd = "secret"
      N = 16384
      r = 8
      p = 1


  }
  TScrypt.CheckPassword('secret', '$s0$e0801$epIxT/h6HbbwHaehFnh/bw==$7H0vsXlY8UxxyW/BWx/9GuY7jEvGjT71GFd6O4SZND0=', {out}passwordRehashNeeded);
  assertPass();
end;

procedure TScryptTests.Test_Base64;
var
  buffer, bufferActual: TBytes;

  function BASE64(s: AnsiString): string;
  var
    b: TBytes;
  begin
    SetLength(b, Length(s));
    if Length(s) > 0 then
      Move(s[1], b[0], Length(s));

    Result := TScryptCracker.Base64Encode(b);
  end;

  procedure Test(Original: AnsiString; Expected: string);
  var
    actual: string;
    recovered: AnsiString;
    data: TBytes;
  begin
    actual := BASE64(Original);
    CheckEquals(Expected, actual);

    //Do the reverse
    data := TScryptCracker.Base64Decode(Expected);
    SetLength(recovered, Length(data));
    if Length(data) > 0 then
      Move(data[0], recovered[1], Length(data));
    CheckEquals(Original, recovered);
  end;
begin
  {
    From RFC4648 - The Base16, Base32, and Base64 Data Encodings
    https://tools.ietf.org/html/rfc4648#section-10
  }
  Test('',  '');
  Test('f', 'Zg==');
  Test('fo', 'Zm8=');
  Test('foo', 'Zm9v');
  Test('foob', 'Zm9vYg==');
  Test('fooba', 'Zm9vYmE=');
  Test('foobar', 'Zm9vYmFy');

  {
    From the wg/scrypt example
    epIxT/h6HbbwHaehFnh/bw== ==> 122 146 49 79 248 122 29 182 240 29 167 161 22 120 127 111
  }
  buffer := HexToBytes('7a 92 31 4f f8 7a 1d b6 f0 1d a7 a1 16 78 7f 6f');
  CheckEquals('epIxT/h6HbbwHaehFnh/bw==', TScryptCracker.Base64Encode(buffer));

  bufferActual := TScryptCracker.Base64Decode('epIxT/h6HbbwHaehFnh/bw==');
  CheckEquals(Length(buffer), Length(bufferActual));
  {$IFNDEF FPC}
  CheckEqualsMem(@buffer[0], @bufferActual[0], Length(bufferActual));
  {$ENDIF}
end;

{$IFNDEF FPC}
procedure TScryptTests.Test_RehashNeededKicksIn;
var
  passwordRehashNeeded: Boolean;
const
  SHash = '$s1$0E0101$dVmS3NXyc0SHM0MBt4qR6Q==$kIF3PYaFHgoetpsSNxi08PGuC7/u+edZd1fRNCtUNnrQsKTCEjI2y6HOGH+S/J2yy7f1JywsSIyBkUfPeMrU9w==';
begin
  {
    N=14,r=1,p=1 (2 MB, 37ms) is too weak for any password on any computer. You target is:
      - regular passwords:      500ms
      - sensitive passwords: 12,000ms
  }

  CheckTrue(TScrypt.CheckPassword('Hashes too fast', SHash, {out}passwordRehashNeeded));
  CheckTrue(passwordRehashNeeded);
  assertPass();
end;
{$ENDIF}

{ TSHA256Tester }

function TSHA256Tester.StringOfString(const s: string; Count: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Count do
    Result := Result+s;
end;

constructor TSHA256Tester.Create(SHA256: IHashAlgorithm);
begin
  inherited Create;

  FSHA256 := SHA256;
end;

destructor TSHA256Tester.Destroy;
begin
  FSHA256 := nil;

  inherited;
end;

function TSHA256Tester.HexStringToBytes(s: string): TBytes;
var
   i, j: Integer;
   n: Integer;
begin
   for i := Length(s) downto 1 do
   begin
      if s[i] = ' ' then
         Delete(s, i, 1);
   end;

   SetLength(Result, Length(s) div 2);

   i := 1;
   j := 0;
   while (i < Length(s)) do
   begin
      n := StrToInt('0x'+s[i]+s[i+1]);
      Result[j] := n;
      Inc(i, 2);
      Inc(j, 1);
   end;
end;

procedure TSHA256Tester.tb(const Buffer; BufferLength: Integer; ExpectedHash: string);
var
   digest: TBytes;
   expectedBytes: TBytes;
begin
   FSHA256.HashData(Buffer, BufferLength);
  digest := FSHA256.Finalize;

   expectedBytes := HexStringToBytes(ExpectedHash);

   if not CompareMem(@digest[0], @expectedBytes[0], Length(expectedBytes)) then
      raise EScryptException.Create('SHA2-256 hash failed');
end;

procedure TSHA256Tester.t(const s: string; expectedHash: string);
var
   utf8Data: TBytes;
   digest: TBytes;
   expectedBytes: TBytes;
begin
  utf8Data := TScryptTests.StringToUtf8Bytes(s);
  expectedBytes := HexStringToBytes(expectedHash);

  FSHA256.HashData(Pointer(utf8Data)^, Length(utf8Data));
  digest := FSHA256.Finalize;

  if not CompareMem(@digest[0], @expectedBytes[0], Length(expectedBytes)) then
    raise EScryptException.Create('SHA2-256 hash failed');
end;

procedure TSHA256Tester.OfficialVectors;
var
  buff: TBytes;
begin
{
  http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/SHA256.pdf
}
  t('abc', 'BA7816BF 8F01CFEA 414140DE 5DAE2223 B00361A3 96177A9C B410FF61 F20015AD');
  t('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', '248D6A61 D20638B8 E5C02693 0C3E6039 A33CE459 64FF2167 F6ECEDD4 19DB06C1');

  SetLength(buff, 3);
  buff[0] := $61; //'a'
  buff[1] := $62; //'b'
  buff[2] := $63; //'c'
  tb(buff[0], Length(buff), 'BA7816BF 8F01CFEA 414140DE 5DAE2223 B00361A3 96177A9C B410FF61 F20015AD');

  {
    http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/SHA2_Additional.pdf
  }

  //#1) 1 byte 0xbd
  SetLength(buff, 1);
  buff[0] := $bd;
  tb(buff[0], Length(buff), '68325720 aabd7c82 f30f554b 313d0570 c95accbb 7dc4b5aa e11204c0 8ffe732b');

   //#2) 4 bytes 0xc98c8e55
  SetLength(buff, 4);
  PDWORD(@buff[0])^ := $558e8cc9;
  tb(buff[0], Length(buff), '7abc22c0 ae5af26c e93dbb94 433a0e0b 2e119d01 4f8e7f65 bd56c61c cccd9504');

  //#3) 55 bytes of zeros
  SetLength(buff, 55);
   FillChar(buff[0], Length(buff), 0);
  tb(buff[0], Length(buff), '02779466 cdec1638 11d07881 5c633f21 90141308 1449002f 24aa3e80 f0b88ef7');

  //#4) 56 bytes of zeros
  SetLength(buff, 56);
   FillChar(buff[0], Length(buff), 0);
  tb(buff[0], Length(buff), 'd4817aa5 497628e7 c77e6b60 6107042b bba31308 88c5f47a 375e6179 be789fbb');

  //#5) 57 bytes of zeros
  SetLength(buff, 57);
   FillChar(buff[0], Length(buff), 0);
  tb(buff[0], Length(buff), '65a16cb7 861335d5 ace3c607 18b5052e 44660726 da4cd13b b745381b 235a1785');

  //#6) 64 bytes of zeros
  SetLength(buff, 64);
   FillChar(buff[0], Length(buff), 0);
  tb(buff[0], Length(buff), 'f5a5fd42 d16a2030 2798ef6e d309979b 43003d23 20d9f0e8 ea9831a9 2759fb4b');

  //#7) 1000 bytes of zeros
  SetLength(buff, 1000);
   FillChar(buff[0], Length(buff) , 0);
  tb(buff[0], Length(buff), '541b3e9d aa09b20b f85fa273 e5cbd3e8 0185aa4e c298e765 db87742b 70138a53');

  //#8) 1000 bytes of 0x41 o?=Ao?=
  SetLength(buff, 1000);
   FillChar(buff[0], Length(buff), $41);
  tb(buff[0], Length(buff), 'c2e68682 3489ced2 017f6059 b8b23931 8b6364f6 dcd835d0 a519105a 1eadd6e4');

  //#9) 1005 bytes of 0x55 o?=Uo?=
  SetLength(buff, 1005);
   FillChar(buff[0], Length(buff), $55);
  tb(buff[0], Length(buff), 'f4d62dde c0f3dd90 ea1380fa 16a5ff8d c4c54b21 740650f2 4afc4120 903552b0');
end;

procedure TSHA256Tester.OfficialVectors_HugeBuffers;
var
  data: PByte;
begin
{
  http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/SHA256.pdf
}
  //#10) 1,000,000 bytes of zeros
  GetMem(data, 1000000);
  try
     FillChar(data^, 1000000, 0);
    tb(data^, 1000000, 'd29751f2 649b32ff 572b5e0a 9f541ea6 60a50f94 ff0beedf b0b692b9 24cc8025');
   finally
      FreeMem(data);
   end;

  //#11) 0x20000000 (536,870,912) bytes of 0x5a o?=Zo?=
  GetMem(data, $20000000);
  try
     FillChar(data^, $20000000, $5a);
    tb(data^, $20000000, '15a1868c 12cc5395 1e182344 277447cd 0979536b adcc512a d24c67e9 b2d4f3dd');
   finally
     FreeMem(data);
   end;

  //#12) 0x41000000 (1,090,519,040) bytes of zeros
  GetMem(data, $41000000);
  try
     FillChar(data^, $41000000, 0);
    tb(data^, $41000000, '461c19a9 3bd4344f 9215f5ec 64357090 342bc66b 15a14831 7d276e31 cbc20b53');
   finally
    FreeMem(data);
   end;

  //#13) 0x6000003e (1,610,612,798) bytes of 0x42 o?=Bo?=
  GetMem(data, $6000003e);
  try
    FillChar(data^, $6000003e, $42);
    tb(data^, $6000003e, 'c23ce8a7 895f4b21 ec0daf37 920ac0a2 62a22004 5a03eb2d fed48ef9 b05aabea');
   finally
    FreeMem(data);
   end;
end;

class procedure TSHA256Tester.Test(SHA256: IHashAlgorithm);
var
  t: TSHA256Tester;
begin
  t := TSHA256Tester.Create(SHA256);
  try
    t.TestSizes;
    t.OfficialVectors;
    t.UnofficialVectors;
  finally
    t.Free;
  end;
end;

procedure TSHA256Tester.TestSizes;
begin
  if FSha256.BlockSize <> 64 then
    raise EScryptException.CreateFmt('SHA256 block size (%d) was not 64 bytes', [FSha256.BlockSize]);

  if FSha256.DigestSize <> 32 then
    raise EScryptException.CreateFmt('SHA256 digest size (%d) was not 32 bytes', [FSha256.DigestSize]);
end;

procedure TSHA256Tester.UnofficialVectors;
begin
  {
    https://www.cosic.esat.kuleuven.be/nessie/testvectors/hash/sha/Sha-2-256.unverified.test-vectors
  }
  t('abcdefghijklmnopqrstuvwxyz', '71C480DF93D6AE2F1EFAD1447C66C9525E316218CF51FC8D9ED832F2DAF18B73');

  t('', 'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855');

  t('a', 'CA978112CA1BBDCAFAC231B39A23DC4DA786EFF8147C4E72B9807785AFEE48BB');

  t('abc', 'BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD');

  t('message digest', 'F7846F55CF23E14EEBEAB5B4E1550CAD5B509E3348FBC4EFA3A1413D393CB650');

  t('abcdefghijklmnopqrstuvwxyz', '71C480DF93D6AE2F1EFAD1447C66C9525E316218CF51FC8D9ED832F2DAF18B73');

  t('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', '248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1');

  t('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789', 'DB4BFCBD4DA0CD85A60C3C37D3FBD8805C77F15FC6B1FDFE614EE0A7C8FDB4C0');

  t(StringOfString('1234567890', 8), 'F371BC4A311F2B009EEF952DD83CA80E2B60026C8E935592D0F9C308453C813E');

  t(StringOfChar('a', 1000000), 'CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0');

  t('The quick brown fox jumps over the lazy dog', 'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592');
end;

procedure TScryptTests.Benchmark_Hashes;
var
  data: TBytes;

  procedure Test(HashAlgorithmName: string);
  var
    hash: IHashAlgorithm;
    t1, t2: Int64;
    bestTime: Int64;
    i: Integer;
  begin
    hash := TScryptCracker.CreateObject(HashAlgorithmName) as IHashAlgorithm;

    bestTime := 0;

    //Fastest time of 5 runs
    for i := 1 to 5 do
    begin
      t1 := GetTimestamp;
      hash.HashData(data[0], Length(data));
      t2 := GetTimestamp;

      t2 := t2-t1;
      if (bestTime = 0) or (t2 < bestTime) then
        bestTime := t2;
    end;

    Status(Format('%s    %.3f MB/s', [HashAlgorithmName, (Length(data)/1024/1024) / (bestTime/FFreq)]));
  end;
begin
  data := TScrypt.GetBytes('hash test', 'Scrypt for Delphi', 1, 1, 1, 1*1024*1024); //1 MB

  Status(Format('%s    %s', ['Algorithm', 'Speed (MB/s)']));

  Test('SHA1.PurePascal');
  Test('SHA1.Csp');
  Test('SHA1.Cng');
  Test('SHA256.PurePascal');
  Test('SHA256.Csp');
  Test('SHA256.Cng');
end;

procedure TScryptTests.Benchmark_SHA1_PurePascal;
var
  hash: IHashAlgorithm;
  t1, t2: Int64;
  data: TBytes;
  best: Int64;
  i: Integer;
begin
  //Generate 1 MB of test data to hash
  data := TScrypt.GetBytes('hash test', 'Scrypt for Delphi', 1, 1, 1, 1*1024*1024); //1 MB

  //Get our pure pascal SHA-1 implementation
  hash := TScryptCracker.CreateObject('SHA1.PurePascal') as IHashAlgorithm;

  best := 0;
  for i := 1 to 60 do
  begin
    t1 := GetTimestamp;
    hash.HashData(data[0], Length(data));
    t2 := GetTimestamp;
    if (((t2-t1) < best) or (best = 0)) then
      best := (t2-t1);
  end;

  Status(Format('%s: %.3f MB/s', ['TSHA1', (Length(data)/1024/1024) / (best/FFreq)]));
end;

procedure TScryptTests.Benchmark_SHA256_PurePascal;
var
  hash: IHashAlgorithm;
  t1, t2: Int64;
  data: TBytes;
  best: Int64;
  i: Integer;
begin
  //Generate 1 MB of test data to hash
  data := TScrypt.GetBytes('hash test', 'Scrypt for Delphi', 1, 1, 1, 1*1024*1024); //1 MB

  //Benchmark SHA256PurePascal with the test data
  best := 0;
  hash := TScryptCracker.CreateObject('SHA256.PurePascal') as IHashAlgorithm;
  for i := 1 to 60 do
  begin
    t1 := GetTimestamp;
    hash.HashData(data[0], Length(data));
    t2 := GetTimestamp;
    if (((t2-t1) < best) or (best = 0)) then
      best := (t2-t1);
  end;

  Status(Format('%s: %.3f MB/s', ['SHA256', (Length(data)/1024/1024) / (best/FFreq)]));
end;

function TScryptTests.GetTimestamp: Int64;
begin
  {$IFDEF FPC}
  result := GetTickCount64;
  {$ELSE}
  if not QueryPerformanceCounter(Result) then //it's documented to never fail; but it can. See SO
    Result := 0;
  {$ENDIF}
end;

procedure TScryptTests.ScryptBenchmarks;
var
  freq, t1, t2: Int64;
  N, r: Integer;
  s: string;

  RowLeader, ColumnSeparator, RowTrailer: string;

  function ElapsedTicks(StartTicks: Int64): Int64;
  var
    endTicks: Int64;
  begin
    endTicks := GetTimestamp;
    Result := (endTicks - StartTicks);
  end;

const
  UseTsv: Boolean = True;
  N_max = 20; //20 trips up on VM and address space limits
  r_max = 16; //8 ==> 1 KB block. 16 ==> 2 KB block

  //Tab separated values (copy-paste to Excel)
  TsvRowLeader = ''; //'|';
  TsvColumnSeparator = '  '; //'|';
  TsvRowTrailer = ''; //'|';

  //Markdown
  MRowLeader = '| ';
  MColumnSeparator = ' | ';
  MRowTrailer = ' |';
const
  STestPassword = 'correct horse battery staple';
begin
  {$IFDEF WINDOWS}
  QueryPerformanceFrequency(freq);
  {$ENDIF}
  if UseTsv then
  begin
    RowLeader := TsvRowLeader;
    ColumnSeparator := TsvColumnSeparator;
    RowTrailer := TsvRowTrailer;
  end
  else
  begin
    RowLeader := MRowLeader;
    ColumnSeparator := MColumnSeparator;
    RowTrailer := MRowTrailer;
  end;


  s := RowLeader+'N';
  for r := 1 to r_max do
    s := s+ColumnSeparator+'r='+IntToStr(r);
  s := s+RowTrailer;
  Status(s);

  if not UseTsv then
  begin
    s := '|---';
    for r := 1 to r_max do
      s := s+'|----';
    s := s+'|';
    Status(s);
  end;

  for N := 1 to N_max do
  begin
    s := RowLeader+IntToStr(N);
    for r := 1 to r_max do
    begin
      if (N < 16*r) and ((1 shl N)*r*Int(128) < $7FFFFFFF) then
      begin
        try
          t1 := GetTimestamp;
          TScrypt.HashPassword(STestPassword, N, r, 1);
          t2 := ElapsedTicks(t1);
          s := s+Format(ColumnSeparator+'%.1f', [t2/freq*1000]);
        except
          on E:EOutOfMemory do
            begin
              s := s+Format(ColumnSeparator+'%s', ['#mem']);
          end;
        end;

      end
      else
      begin
        //invalid cost
        s := s+ColumnSeparator+'#N/A';
      end;
    end;
    s := s+RowTrailer;
    Status(s);
  end;
end;

procedure TScryptTests.Tester_HMAC_SHA1(HMACsha1: IHmacAlgorithm);

  procedure t(const Key: AnsiString; const Data: AnsiString; const ExpectedDigest: array of Byte);
  var
    digest: TBytes;
  begin
    digest := HMACsha1.HashData(Key[1], Length(Key), Data[1], Length(Data));

    if (Length(ExpectedDigest) <> Length(digest)) then
      raise EScryptException.CreateFmt('Scrypt self-test failed: Length failed with Key "%s" and Data "%s"',
          [Key, Data]);

    if not CompareMem(@ExpectedDigest, @digest[0], 20) then
      raise EScryptException.CreateFmt('Scrypt self-test failed: Compare failed with Key "%s" and Data "%s"',
          [Key, Data]);
  end;

//var
//  key: TBytes;

begin
  //From RFC 2022: Test Cases for HMAC-MD5 and HMAC-SHA-1

{
  test_case =     1
  key =           0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b
  key_len =       20
  data =          "Hi There"
  data_len =      8
  digest =        0xb617318655057264e28bc0b6fb378c8ef146be00
}
  t(StringOfChar(AnsiChar($0b), 20), 'Hi There', [$b6, $17, $31, $86, $55, $05, $72, $64, $e2, $8b, $c0, $b6, $fb, $37, $8c, $8e, $f1, $46, $be, $00]);

{
  test_case =     2
  key =           "Jefe"
  key_len =       4
  data =          "what do ya want for nothing?"
  data_len =      28
  digest =        0xeffcdf6ae5eb2fa2d27416d5f184df9c259a7c79
}
  t('Jefe', 'what do ya want for nothing?', [$ef, $fc, $df, $6a, $e5, $eb, $2f, $a2, $d2, $74, $16, $d5, $f1, $84, $df, $9c, $25, $9a, $7c, $79]);

{
  test_case =     3
  key =           0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  key_len =       20
  data =          0xdd repeated 50 times
  data_len =      50
  digest =        0x125d7342b9ac11cd91a39af48aa17b4f63f175d3
}
  t(StringOfChar(AnsiChar($aa), 20), StringOfChar(AnsiChar($dd), 50), [$12, $5d, $73, $42, $b9, $ac, $11, $cd, $91, $a3, $9a, $f4, $8a, $a1, $7b, $4f, $63, $f1, $75, $d3]);

{
  test_case =     4
  key =           0x0102030405060708090a0b0c0d0e0f10111213141516171819
  key_len =       25
  data =          0xcd repeated 50 times
  data_len =      50
  digest =        0x4c9007f4026250c6bc8414f9bf50c86c2d7235da
}
  t(#$01#$02#$03#$04#$05#$06#$07#$08#$09#$0a#$0b#$0c#$0d#$0e#$0f#$10#$11#$12#$13#$14#$15#$16#$17#$18#$19,
      StringOfChar(AnsiChar($cd), 50),
      [$4c,$90,$07,$f4,$02,$62,$50,$c6,$bc,$84,$14,$f9,$bf,$50,$c8,$6c,$2d,$72,$35,$da]);

{
  test_case =     5
  key =           0x0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c
  key_len =       20
  data =          "Test With Truncation"
  data_len =      20
  digest =        0x4c1a03424b55e07fe7f27be1d58bb9324a9a5a04
  digest-96 =     0x4c1a03424b55e07fe7f27be1
}
  t(StringOfChar(AnsiChar($0c), 20), 'Test With Truncation',
      [$4c,$1a,$03,$42,$4b,$55,$e0,$7f,$e7,$f2,$7b,$e1,$d5,$8b,$b9,$32,$4a,$9a,$5a,$04]);

{
  test_case =     6
  key =           0xaa repeated 80 times
  key_len =       80
  data =          "Test Using Larger Than Block-Size Key - Hash Key First"
  data_len =      54
  digest =        0xaa4ae5e15272d00e95705637ce8a3b55ed402112
}
  t(StringOfChar(AnsiChar($aa), 80), 'Test Using Larger Than Block-Size Key - Hash Key First',
      [$aa,$4a,$e5,$e1,$52,$72,$d0,$0e,$95,$70,$56,$37,$ce,$8a,$3b,$55,$ed,$40,$21,$12]);

{
  test_case =     7
  key =           0xaa repeated 80 times
  key_len =       80
  data =          "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data"
  data_len =      73
  digest =        0xe8e99d0f45237d786d6bbaa7965c7808bbff1a91
}
  t(StringOfChar(AnsiChar($aa), 80),
      'Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data',
      [$e8,$e9,$9d,$0f,$45,$23,$7d,$78,$6d,$6b,$ba,$a7,$96,$5c,$78,$08,$bb,$ff,$1a,$91]);
end;

procedure TScryptTests.Test_HMAC_SHA1;
var
  hash: IHmacAlgorithm;
begin
  hash := TScryptCracker.CreateObject('HMAC.SHA1') as IHmacAlgorithm;
  Tester_HMAC_SHA1(hash);
  hash := nil;
  assertPass();
end;

{$IFNDEF FPC}
procedure TScryptTests.Test_HMAC_SHA1_Cng;
var
  hash: IHmacAlgorithm;
begin
  hash := TScryptCracker.CreateObject('HMAC.SHA1.Cng') as IHmacAlgorithm;
  Tester_HMAC_SHA1(hash);
  hash := nil;
  assertPass();
end;
{$ENDIF}

procedure TScryptTests.Test_HMAC_SHA1_PurePascal;
var
  hash: IHmacAlgorithm;
begin
  hash := TScryptCracker.CreateObject('HMAC.SHA1.PurePascal') as IHmacAlgorithm;
  Tester_HMAC_SHA1(hash);
  hash := nil;
  assertPass();
end;

procedure TScryptTests.Test_HMAC_SHA256;
var
  hmac: IHmacAlgorithm;
begin
  hmac := TScryptCracker.CreateObject('HMAC.SHA256') as IHmacAlgorithm;
  Tester_HMAC_SHA256(hmac);
  assertPass();
end;

{$IFNDEF FPC}
procedure TScryptTests.Test_HMAC_SHA256_Cng;
var
  hmac: IHmacAlgorithm;
begin
  hmac := TScryptCracker.CreateObject('HMAC.SHA256.Cng') as IHmacAlgorithm;
  Tester_HMAC_SHA256(hmac);
  assertPass();
end;
{$ENDIF}

procedure TScryptTests.Test_HMAC_SHA256_PurePascal;
var
  hmac: IHmacAlgorithm;
begin
  hmac := TScryptCracker.CreateObject('HMAC.SHA256.PurePascal') as IHmacAlgorithm;
  Tester_HMAC_SHA256(hmac);
  assertPass();
end;

procedure TScryptTests.Tester_HMAC_SHA256(HMACsha256: IHmacAlgorithm);

  procedure Test(const KeyHexString: string; const DataHexString: string; const ExpectedDigestHexString: string; TruncateToBytes: Integer=0);
  var
    key: TBytes;
    data: TBytes;
    expected: TBytes;
    actual: TBytes;
  begin
    //Deserialize the test data
    key := HexToBytes(KeyHexString);
    data := HexToBytes(DataHexString);
    expected := HexToBytes(ExpectedDigestHexString);

    actual := HMACsha256.HashData(key[0], Length(key), data[0], Length(data));

    if TruncateToBytes > 0 then
      actual := Copy(actual, 0, TruncateToBytes);

    if (Length(expected) <> Length(actual)) then
      raise EScryptException.CreateFmt('Scrypt self-test failed: Length failed with Key "%s" and Data "%s"',
          [KeyHexString, DataHexString]);

    if not CompareMem(@expected[0], @actual[0], Length(expected)) then
      raise EScryptException.CreateFmt('Scrypt self-test failed: Compare failed with Key "%s" and Data "%s"',
          [KeyHexString, DataHexString]);
  end;

begin
{
  From RFC4231
   Identifiers and Test Vectors for HMAC-SHA-224, HMAC-SHA-256, HMAC-SHA-384, and HMAC-SHA-512
  https://www.ietf.org/rfc/rfc4231.txt
}

  //Test Case 1
  Test(
      {Key= }      '0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b', // (20 bytes)
      {Data= }      '4869205468657265', // ("Hi There")
      {HMAC-SHA-256}  'b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7');

  //Test Case 2
  //Test with a key shorter than the length of the HMAC output.
  Test(
      {Key =}      '4a656665', // ("Jefe")
      {Data =}      '7768617420646f2079612077616e7420666f72206e6f7468696e673f', // ("what do ya want for nothing?")
      {HMAC-SHA-256}  '5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843');

  //Test Case 3
  //Test with a combined length of key and data that is larger than 64 bytes (= block-size of SHA-224 and SHA-256).
  Test(
      {Key}        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', // (20 bytes)
      {Data}      'dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd', // (50 bytes)
      {HMAC-SHA-256}  '773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe');

  //Test Case 4
  //Test with a combined length of key and data that is larger than 64 bytes (= block-size of SHA-224 and SHA-256).
  Test(
      {Key}        '0102030405060708090a0b0c0d0e0f10111213141516171819', // (25 bytes)
      {Data}      'cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd', // (50 bytes)
      {HMAC-SHA-256}  '82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b');

  //Test Case 5
  //Test with a truncation of output to 128 bits.
  Test(
      {Key}        '0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c', // (20 bytes)
      {Data}      '546573742057697468205472756e636174696f6e', // ("Test With Truncation")
      {HMAC-SHA-256}  'a3b6167473100ee06e0c796c2955552b', 16);

  //Test Case 6
  //Test with a key larger than 128 bytes (= block-size of SHA-384 and SHA-512).
  Test(
      {Key}        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
                'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
                'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
                'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', // (131 bytes)
      {Data}      '54657374205573696e67204c6172676572205468616e20426c6f636b2d53697a'+
                '65204b6579202d2048617368204b6579204669727374', //("Test Using Larger Than Block-Size Key - Hash Key")                      (" First")
      {HMAC-SHA-256}  '60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54');

  //Test Case 7
  //Test with a key and data that is larger than 128 bytes (= block-size of SHA-384 and SHA-512).
  Test(
      {Key}        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
                'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
                'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
                'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', // (131 bytes)
      {Data}      '5468697320697320612074657374207573696e672061206c6172676572207468'+
                '616e20626c6f636b2d73697a65206b657920616e642061206c61726765722074'+
                '68616e20626c6f636b2d73697a6520646174612e20546865206b6579206e6565'+
                '647320746f20626520686173686564206265666f7265206265696e6720757365'+
                '642062792074686520484d414320616c676f726974686d2e', //"This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm."
      {HMAC-SHA-256}  '9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2');
end;

procedure TScryptTests.Tester_PBKDF2_SHA256(Pbkdf2sha256: IPBKDF2Algorithm);

  procedure t(Password: UnicodeString; Salt: AnsiString; IterationCount: Integer; DerivedKeyLength: Integer;
      const ExpectedDerivedKeyHexString: string);
  var
    expected: TBytes;
    actual: TBytes; //derivedKey
  begin
    expected := HexToBytes(ExpectedDerivedKeyHexString);
    actual := Pbkdf2sha256.GetBytes(Password, Salt[1], Length(Salt), IterationCount, DerivedKeyLength);

    if (DerivedKeyLength <> Length(actual)) then
      raise EScryptException.CreateFmt('Scrypt self-test failed: Derived key length (%d) wasn''t the required %d',
          [Length(actual), DerivedKeyLength]);

    if not CompareMem(@expected[0], @actual[0], DerivedKeyLength) then
      raise EScryptException.CreateFmt('Scrypt self-test failed: Derived key was invalid for Password "%s", Salt="%s", IterationCount="%d", DerivedKeyLength=%d',
          [Password, Salt, IterationCount, DerivedKeyLength]);
  end;
begin
  {
    From http://stackoverflow.com/a/5136918/12597
  }

  {
    Input:
     P = "password" (8 octets)
     S = "salt" (4 octets)
     c = 1
     dkLen = 32

    Output:
     DK = 12 0f b6 cf fc f8 b3 2c 43 e7 22 52 56 c4 f8 37 a8 65 48 c9 2c cc 35 48 08 05 98 7c b7 0b e1 7b   (20 octets)
}
  t('password', 'salt', 1, 32, '12 0f b6 cf fc f8 b3 2c 43 e7 22 52 56 c4 f8 37 a8 65 48 c9 2c cc 35 48 08 05 98 7c b7 0b e1 7b');

{
    Input:
     P = "password" (8 octets)
     S = "salt" (4 octets)
     c = 2
     dkLen = 32

    Output:
     DK = ae 4d 0c 95 af 6b 46 d3 2d 0a df f9 28 f0 6d d0 2a 30 3f 8e f3 c2 51 df d6 e2 d8 5a 95 47 4c 43             (32 octets)
}
  t('password', 'salt', 2, 32, 'ae 4d 0c 95 af 6b 46 d3 2d 0a df f9 28 f0 6d d0 2a 30 3f 8e f3 c2 51 df d6 e2 d8 5a 95 47 4c 43');

{
    Input:
     P = "password" (8 octets)
     S = "salt" (4 octets)
     c = 4096
     dkLen = 32

    Output:
     DK = c5 e4 78 d5 92 88 c8 41 aa 53 0d b6 84 5c 4c 8d 96 28 93 a0 01 ce 4e 11 a4 96 38 73 aa 98 13 4a  (32 octets)
}
  t('password', 'salt', 4096, 20, 'c5 e4 78 d5 92 88 c8 41 aa 53 0d b6 84 5c 4c 8d 96 28 93 a0 01 ce 4e 11 a4 96 38 73 aa 98 13 4a');

{
    Input:
     P = "password" (8 octets)
     S = "salt" (4 octets)
     c = 16777216
     dkLen = 32

    Output:
     DK = cf 81 c6 6f e8 cf c0 4d 1f 31 ec b6 5d ab 40 89 f7 f1 79 e8 9b 3b 0b cb 17 ad 10 e3 ac 6e ba 46  (32 octets)
}
  //This test works, but it's 16,777,216 rounds
//  t('password', 'salt', 16777216, 20, 'cf 81 c6 6f e8 cf c0 4d 1f 31 ec b6 5d ab 40 89 f7 f1 79 e8 9b 3b 0b cb 17 ad 10 e3 ac 6e ba 46');

{
    Input:
     P = "passwordPASSWORDpassword" (24 octets)
     S = "saltSALTsaltSALTsaltSALTsaltSALTsalt" (36 octets)
     c = 4096
     dkLen = 40

    Output:
     DK = 34 8c 89 db cb d3 2b 2f 32 d8 14 b8 11 6e 84 cf 2b 17 34 7e bc 18 00 18 1c 4e 2a 1f b8 dd 53 e1 c6 35 51 8c 7d ac 47 e9  (40 octets)
}
  t('passwordPASSWORDpassword', 'saltSALTsaltSALTsaltSALTsaltSALTsalt', 4096, 25,
      '34 8c 89 db cb d3 2b 2f 32 d8 14 b8 11 6e 84 cf 2b 17 34 7e bc 18 00 18 1c 4e 2a 1f b8 dd 53 e1 c6 35 51 8c 7d ac 47 e9');

{
    Input:
     P = "pass\0word" (9 octets)
     S = "sa\0lt" (5 octets)
     c = 4096
     dkLen = 16

    Output:
     DK = 89 b6 9d 05 16 f8 29 89 3c 69 62 26 65 0a 86 87 (16 octets)
}
  t('pass'#0'word', 'sa'#0'lt', 4096, 16, '89 b6 9d 05 16 f8 29 89 3c 69 62 26 65 0a 86 87');




{
  http://tools.ietf.org/html/draft-josefsson-scrypt-kdf-00#section-10

  10.  Test Vectors for PBKDF2 with HMAC-SHA-256

    The test vectors below can be used to verify the PBKDF2-HMAC-SHA-256
    [RFC2898] function.  The password and salt strings are passed as
    sequences of ASCII [ANSI.X3-4.1986] octets.
}
  t('passwd', 'salt', 1, 64,
      '55 ac 04 6e 56 e3 08 9f ec 16 91 c2 25 44 b6 05 f9 41 85 21 6d de 04 65 e6 8b 9d 57 c2 0d ac bc'+
      '49 ca 9c cc f1 79 b6 45 99 16 64 b3 9d 77 ef 31 7c 71 b8 45 b1 e3 0b d5 09 11 20 41 d3 a1 97 83');


  t('Password', 'NaCl', 80000, 64,
      '4d dc d8 f6 0b 98 be 21 83 0c ee 5e f2 27 01 f9 64 1a 44 18 d0 4c 04 14 ae ff 08 87 6b 34 ab 56'+
      'a1 d4 25 a1 22 58 33 54 9a db 84 1b 51 c9 b3 17 6a 27 2b de bb a1 d0 78 47 8f 62 b3 97 f3 3c 8d');
end;

procedure TScryptTests.Test_PasswordHashing;
var
  password: string;
  hash: string;
  freq, t1, t2: Int64;
  passwordRehashNeeded: Boolean;
begin
  {
    Test round trip of generating a hash, and then verifying it
  }
   {$IFDEF WINDOWS}
  if not QueryPerformanceFrequency(freq) then freq := -1;
   {$ENDIF}

  password := 'correct horse battery staple';

  t1 := GetTimestamp;
  hash := TScrypt.HashPassword(password);
  t2 := GetTimestamp;

  Status('Password: "'+password+'"');
  Status('Hash: "'+hash+'"');
  Status(Format('Time to hash password: %.4f ms', [(t2-t1)/freq*1000]));

  Self.CheckFalse(hash='');

  t1 := GetTimestamp;
  Self.CheckTrue(TScrypt.CheckPassword('correct horse battery staple', hash, {out}passwordRehashNeeded));
  t2 := GetTimestamp;
  Status(Format('Time to verify password: %.4f ms', [(t2-t1)/freq*1000]));
  assertPass();
end;

procedure TScryptTests.Tester_PBKDF2_SHA1(Pbkdf: IPBKDF2Algorithm);

  procedure t(Password: UnicodeString; Salt: AnsiString; IterationCount: Integer; DerivedKeyLength: Integer;
      const ExpectedDerivedKey: array of byte);
  var
    derivedKey: TBytes; //derivedKey
    t1, t2: Int64;
    s: string;
  begin
    t1 := Self.GetTimestamp;
    derivedKey := Pbkdf.GetBytes(Password, Salt[1], Length(Salt), IterationCount, DerivedKeyLength);
    t2 := Self.GetTimestamp;

    s := Format('%.4f ms', [(t2-t1)/FFreq*1000]);
    Status(s);

    if (DerivedKeyLength <> Length(derivedKey)) then
      raise EScryptException.CreateFmt('Scrypt self-test failed: Derived key length (%d) wasn''t the required %d',
          [Length(DerivedKey), DerivedKeyLength]);

    if not CompareMem(@ExpectedDerivedKey[0], @derivedKey[0], DerivedKeyLength) then
      raise EScryptException.CreateFmt('Scrypt self-test failed: Derived key was invalid for Password "%s", Salt="%s", IterationCount="%d", DerivedKeyLength=%d',
          [Password, Salt, IterationCount, DerivedKeyLength]);
  end;
begin
{
  PKCS #5: Password-Based Key Derivation Function 2 (PBKDF2) Test Vectors
  http://tools.ietf.org/html/rfc6070


    Input:
     P = "password" (8 octets)
     S = "salt" (4 octets)
     c = 1
     dkLen = 20

    Output:
     DK = 0c 60 c8 0f 96 1f 0e 71 f3 a9 b5 24 af 60 12 06 2f e0 37 a6             (20 octets)
}
  t('password', 'salt', 1, 20, [$0c,$60,$c8,$0f,$96,$1f,$0e,$71,$f3,$a9,$b5,$24,$af,$60,$12,$06,$2f,$e0,$37,$a6]);

{
    Input:
     P = "password" (8 octets)
     S = "salt" (4 octets)
     c = 2
     dkLen = 20

    Output:
     DK = ea 6c 01 4d c7 2d 6f 8c cd 1e d9 2a ce 1d 41 f0 d8 de 89 57             (20 octets)
}
  t('password', 'salt', 2, 20, [$ea,$6c,$01,$4d,$c7,$2d,$6f,$8c,$cd,$1e,$d9,$2a,$ce,$1d,$41,$f0,$d8,$de,$89,$57]);

{
    Input:
     P = "password" (8 octets)
     S = "salt" (4 octets)
     c = 4096
     dkLen = 20

    Output:
     DK = 4b 00 79 01 b7 65 48 9a be ad 49 d9 26 f7 21 d0 65 a4 29 c1             (20 octets)
}
  t('password', 'salt', 4096, 20, [$4b,$00,$79,$01,$b7,$65,$48,$9a,$be,$ad,$49,$d9,$26,$f7,$21,$d0,$65,$a4,$29,$c1]);

{
    Input:
     P = "password" (8 octets)
     S = "salt" (4 octets)
     c = 16777216
     dkLen = 20

    Output:
     DK = ee fe 3d 61 cd 4d a4 e4 e9 94 5b 3d 6b a2 15 8c 26 34 e9 84             (20 octets)
}
  //This test passes, but it's 16,777,216 rounds (2m 17s)
//  t('password', 'salt', 16777216, 20, [$ee,$fe,$3d,$61,$cd,$4d,$a4,$e4,$e9,$94,$5b,$3d,$6b,$a2,$15,$8c,$26,$34,$e9,$84]);

{
    Input:
     P = "passwordPASSWORDpassword" (24 octets)
     S = "saltSALTsaltSALTsaltSALTsaltSALTsalt" (36 octets)
     c = 4096
     dkLen = 25

    Output:
     DK = 3d 2e ec 4f e4 1c 84 9b 80 c8 d8 36 62 c0 e4 4a 8b 29 1a 96 4c f2 f0 70 38    (25 octets)
}
  t('passwordPASSWORDpassword', 'saltSALTsaltSALTsaltSALTsaltSALTsalt', 4096, 25,
      [$3d,$2e,$ec,$4f,$e4,$1c,$84,$9b,$80,$c8,$d8,$36,$62,$c0,$e4,$4a,$8b,$29,$1a,$96,$4c,$f2,$f0,$70,$38]);

{
    Input:
     P = "pass\0word" (9 octets)
     S = "sa\0lt" (5 octets)
     c = 4096
     dkLen = 16

    Output:
     DK = 56 fa 6a a7 55 48 09 9d cc 37 d7 f0 34 25 e0 c3 (16 octets)
}
  t('pass'#0'word', 'sa'#0'lt', 4096, 16,
      [$56,$fa,$6a,$a7,$55,$48,$09,$9d,$cc,$37,$d7,$f0,$34,$25,$e0,$c3]);
end;

procedure TScryptTests.Benchmark_HMACs;
var
  data: TBytes;
const
  password: AnsiString = 'correct horse battery staple';

  procedure Test(HmacAlgorithmName: string);
  var
    hmac: IHmacAlgorithm;
    t1, t2: Int64;
    bestTime: Int64;
    i: Integer;
  begin
    hmac := TScryptCracker.CreateObject(HmacAlgorithmName) as IHmacAlgorithm;

    bestTime := 0;

    //Fastest time of 5 runs
    for i := 1 to 5 do
    begin
      t1 := GetTimestamp;
      hmac.HashData(password[1], Length(password), data[0], Length(data));
      t2 := GetTimestamp;

      t2 := t2-t1;
      if (bestTime = 0) or (t2 < bestTime) then
        bestTime := t2;
    end;

    Status(Format('%s  %.3f MB/s', [HmacAlgorithmName, (Length(data)/1024/1024) / (bestTime/FFreq)]));
  end;
begin
  //generate 1 MB of sample data to HMAC
  data := TScrypt.GetBytes('hash test', 'Scrypt for Delphi', 1, 1, 1, 1*1024*1024); //1 MB

  Status(Format('%s  %s', ['Algorithm', 'Speed (MB/s)']));

  Test('HMAC.SHA1');
  Test('HMAC.SHA1.PurePascal');
  Test('HMAC.SHA1.csp');
  Test('HMAC.SHA1.Cng');
  Test('HMAC.SHA256');
  Test('HMAC.SHA256.PurePascal');
  Test('HMAC.SHA256.csp');
  Test('HMAC.SHA256.Cng');
end;

procedure TScryptTests.Benchmark_PBKDF2s;
const
  password: string = 'correct horse battery staple';
  salt: AnsiString = 'sea salt';

  procedure Test(HmacAlgorithmName: string);
  var
    db: IPBKDF2Algorithm;
    t1, t2: Int64;
    bestTime: Int64;
    i: Integer;
  begin
    db := TScryptCracker.CreateObject(HmacAlgorithmName) as IPBKDF2Algorithm;

    bestTime := 0;

    //Fastest time of 5 runs
    for i := 1 to 5 do
    begin
      t1 := GetTimestamp;
      db.GetBytes(password, salt[1], Length(salt), 1000, 32);
      t2 := GetTimestamp;

      t2 := t2-t1;
      if (bestTime = 0) or (t2 < bestTime) then
        bestTime := t2;
    end;

    Status(Format('%s  %.4f ms', [HmacAlgorithmName, (bestTime/FFreq*1000)]));
  end;
begin
  Status(Format('%s  %s', ['Algorithm', 'Speed (ms)']));

  Test('PBKDF2.SHA1');
  Test('PBKDF2.SHA1.PurePascal');
//  Test('PBKDF2.SHA1.csp');
  Test('PBKDF2.SHA1.Cng');
  Test('PBKDF2.SHA256');
  Test('PBKDF2.SHA256.PurePascal');
//  Test('PBKDF2.SHA256.csp');
  Test('PBKDF2.SHA256.Cng');
end;

procedure TScryptTests.Test_Salsa208Core;
type
  TLongWordArray = array[0..15] of LongWord;
  PLongWordArray = ^TLongWordArray;
var
  actual: TBytes;
  expected: TBytes;
begin
{
  https://tools.ietf.org/html/draft-josefsson-scrypt-kdf-02#section-7

  7.  Test Vectors for Salsa20/8 Core

   Below is a sequence of octets to illustrate input and output values
   for the Salsa20/8 Core.  The octets are hex encoded and whitespace is
   inserted for readability.  The value corresponds to the first input
   and output pair generated by the first scrypt test vector below.

   INPUT:
   7e 87 9a 21 4f 3e c9 86 7c a9 40 e6 41 71 8f 26
   ba ee 55 5b 8c 61 c1 b5 0d f8 46 11 6d cd 3b 1d
   ee 24 f3 19 df 9b 3d 85 14 12 1e 4b 5a c5 aa 32
   76 02 1d 29 09 c7 48 29 ed eb c6 8d b8 b8 c2 5e

   OUTPUT:
   a4 1f 85 9c 66 08 cc 99 3b 81 ca cb 02 0c ef 05
   04 4b 21 81 a2 fd 33 7d fd 7b 1c 63 96 68 2f 29
   b4 39 31 68 e3 c9 e6 bc fe 6b c5 b7 a0 6d 96 ba
   e4 24 cc 10 2c 91 74 5c 24 ad 67 3d c7 61 8f 81
}
  actual := HexToBytes(
      '7e 87 9a 21 4f 3e c9 86 7c a9 40 e6 41 71 8f 26 '+
      'ba ee 55 5b 8c 61 c1 b5 0d f8 46 11 6d cd 3b 1d '+
      'ee 24 f3 19 df 9b 3d 85 14 12 1e 4b 5a c5 aa 32 '+
      '76 02 1d 29 09 c7 48 29 ed eb c6 8d b8 b8 c2 5e ');

  TScryptCracker(FScrypt).Salsa20InPlace(actual[0]);


  expected := HexToBytes(
      'a4 1f 85 9c 66 08 cc 99 3b 81 ca cb 02 0c ef 05 '+
      '04 4b 21 81 a2 fd 33 7d fd 7b 1c 63 96 68 2f 29 '+
      'b4 39 31 68 e3 c9 e6 bc fe 6b c5 b7 a0 6d 96 ba '+
      'e4 24 cc 10 2c 91 74 5c 24 ad 67 3d c7 61 8f 81 ');


  Self.CheckEquals(Length(expected), Length(actual), 'Salsa20/8 array length');
  {$IFNDEF FPC}
  Self.CheckEqualsMem(@expected[0], @actual[0], Length(expected), 'Salsa20/8 data failed');
  {$ENDIF}
end;

procedure TScryptTests.Test_Scrypt_PasswordFormatting;
begin

end;

procedure TScryptTests.Test_BlockMix;
var
  input: TBytes;
  expected: TBytes;
  actual: TBytes;
begin
  {
  8.  Test Vectors for scryptBlockMix
  https://tools.ietf.org/html/draft-josefsson-scrypt-kdf-02#section-8

  Below is a sequence of octets to illustrate input and output values
   for scryptBlockMix.  The test vector uses an r value of 1.  The
   octets are hex encoded and whitespace is inserted for readability.
   The value corresponds to the first input and output pair generated by
   the first scrypt test vector below.

   INPUT
   B[0] =  f7 ce 0b 65 3d 2d 72 a4 10 8c f5 ab e9 12 ff dd
           77 76 16 db bb 27 a7 0e 82 04 f3 ae 2d 0f 6f ad
           89 f6 8f 48 11 d1 e8 7b cc 3b d7 40 0a 9f fd 29
           09 4f 01 84 63 95 74 f3 9a e5 a1 31 52 17 bc d7

   B[1] =  89 49 91 44 72 13 bb 22 6c 25 b5 4d a8 63 70 fb
           cd 98 43 80 37 46 66 bb 8f fc b5 bf 40 c2 54 b0
           67 d2 7c 51 ce 4a d5 fe d8 29 c9 0b 50 5a 57 1b
           7f 4d 1c ad 6a 52 3c da 77 0e 67 bc ea af 7e 89

   OUTPUT
   B'[0] = a4 1f 85 9c 66 08 cc 99 3b 81 ca cb 02 0c ef 05
           04 4b 21 81 a2 fd 33 7d fd 7b 1c 63 96 68 2f 29
           b4 39 31 68 e3 c9 e6 bc fe 6b c5 b7 a0 6d 96 ba
           e4 24 cc 10 2c 91 74 5c 24 ad 67 3d c7 61 8f 81

   B'[1] = 20 ed c9 75 32 38 81 a8 05 40 f6 4c 16 2d cd 3c
           21 07 7c fe 5f 8d 5f e2 b1 a4 16 8f 95 36 78 b7
           7d 3b 3d 80 3b 60 e4 ab 92 09 96 e5 9b 4d 53 b6
           5d 2a 22 58 77 d5 ed f5 84 2c b9 f1 4e ef e4 25
}
  input := HexToBytes(
      'f7 ce 0b 65 3d 2d 72 a4 10 8c f5 ab e9 12 ff dd '+
      '77 76 16 db bb 27 a7 0e 82 04 f3 ae 2d 0f 6f ad '+
      '89 f6 8f 48 11 d1 e8 7b cc 3b d7 40 0a 9f fd 29 '+
      '09 4f 01 84 63 95 74 f3 9a e5 a1 31 52 17 bc d7 '+

      '89 49 91 44 72 13 bb 22 6c 25 b5 4d a8 63 70 fb '+
      'cd 98 43 80 37 46 66 bb 8f fc b5 bf 40 c2 54 b0 '+
      '67 d2 7c 51 ce 4a d5 fe d8 29 c9 0b 50 5a 57 1b '+
      '7f 4d 1c ad 6a 52 3c da 77 0e 67 bc ea af 7e 89 ');

  expected := HexToBytes(
      'a4 1f 85 9c 66 08 cc 99 3b 81 ca cb 02 0c ef 05 '+
      '04 4b 21 81 a2 fd 33 7d fd 7b 1c 63 96 68 2f 29 '+
      'b4 39 31 68 e3 c9 e6 bc fe 6b c5 b7 a0 6d 96 ba '+
      'e4 24 cc 10 2c 91 74 5c 24 ad 67 3d c7 61 8f 81 '+

      '20 ed c9 75 32 38 81 a8 05 40 f6 4c 16 2d cd 3c '+
      '21 07 7c fe 5f 8d 5f e2 b1 a4 16 8f 95 36 78 b7 '+
      '7d 3b 3d 80 3b 60 e4 ab 92 09 96 e5 9b 4d 53 b6 '+
      '5d 2a 22 58 77 d5 ed f5 84 2c b9 f1 4e ef e4 25 ');


  actual := TScryptCracker(FScrypt).BlockMix(input);

  Self.CheckEquals(Length(expected), Length(actual), 'BlockMix array length');
  {$IFNDEF FPC}
  Self.CheckEqualsMem(@expected[0], @actual[0], Length(expected), 'BlockMix data failed');
  {$ENDIF}
  assertPass();
end;

procedure TScryptTests.Test_ROMix;
var
  input: TBytes;
  actual: TBytes;
  expected: TBytes;
begin
{
  9.  Test Vectors for scryptROMix
  https://tools.ietf.org/html/draft-josefsson-scrypt-kdf-02#section-9


  Below is a sequence of octets to illustrate input and output values
  for scryptROMix.  The test vector uses an r value of 1 and an N value
  of 16.  The octets are hex encoded and whitespace is inserted for
  readability.  The value corresponds to the first input and output
  pair generated by the first scrypt test vector below.

   INPUT:
   B = f7 ce 0b 65 3d 2d 72 a4 10 8c f5 ab e9 12 ff dd
       77 76 16 db bb 27 a7 0e 82 04 f3 ae 2d 0f 6f ad
       89 f6 8f 48 11 d1 e8 7b cc 3b d7 40 0a 9f fd 29
       09 4f 01 84 63 95 74 f3 9a e5 a1 31 52 17 bc d7
       89 49 91 44 72 13 bb 22 6c 25 b5 4d a8 63 70 fb
       cd 98 43 80 37 46 66 bb 8f fc b5 bf 40 c2 54 b0
       67 d2 7c 51 ce 4a d5 fe d8 29 c9 0b 50 5a 57 1b
       7f 4d 1c ad 6a 52 3c da 77 0e 67 bc ea af 7e 89

   OUTPUT:
   B = 79 cc c1 93 62 9d eb ca 04 7f 0b 70 60 4b f6 b6
       2c e3 dd 4a 96 26 e3 55 fa fc 61 98 e6 ea 2b 46
       d5 84 13 67 3b 99 b0 29 d6 65 c3 57 60 1f b4 26
       a0 b2 f4 bb a2 00 ee 9f 0a 43 d1 9b 57 1a 9c 71
       ef 11 42 e6 5d 5a 26 6f dd ca 83 2c e5 9f aa 7c
       ac 0b 9c f1 be 2b ff ca 30 0d 01 ee 38 76 19 c4
       ae 12 fd 44 38 f2 03 a0 e4 e1 c4 7e c3 14 86 1f
       4e 90 87 cb 33 39 6a 68 73 e8 f9 d2 53 9a 4b 8e
}

  input := HexToBytes(
      'f7 ce 0b 65 3d 2d 72 a4 10 8c f5 ab e9 12 ff dd '+
      '77 76 16 db bb 27 a7 0e 82 04 f3 ae 2d 0f 6f ad '+
      '89 f6 8f 48 11 d1 e8 7b cc 3b d7 40 0a 9f fd 29 '+
      '09 4f 01 84 63 95 74 f3 9a e5 a1 31 52 17 bc d7 '+
      '89 49 91 44 72 13 bb 22 6c 25 b5 4d a8 63 70 fb '+
      'cd 98 43 80 37 46 66 bb 8f fc b5 bf 40 c2 54 b0 '+
      '67 d2 7c 51 ce 4a d5 fe d8 29 c9 0b 50 5a 57 1b '+
      '7f 4d 1c ad 6a 52 3c da 77 0e 67 bc ea af 7e 89 ');

  expected := HexToBytes(
      '79 cc c1 93 62 9d eb ca 04 7f 0b 70 60 4b f6 b6 '+
      '2c e3 dd 4a 96 26 e3 55 fa fc 61 98 e6 ea 2b 46 '+
      'd5 84 13 67 3b 99 b0 29 d6 65 c3 57 60 1f b4 26 '+
      'a0 b2 f4 bb a2 00 ee 9f 0a 43 d1 9b 57 1a 9c 71 '+
      'ef 11 42 e6 5d 5a 26 6f dd ca 83 2c e5 9f aa 7c '+
      'ac 0b 9c f1 be 2b ff ca 30 0d 01 ee 38 76 19 c4 '+
      'ae 12 fd 44 38 f2 03 a0 e4 e1 c4 7e c3 14 86 1f '+
      '4e 90 87 cb 33 39 6a 68 73 e8 f9 d2 53 9a 4b 8e ');

  actual := TScryptCracker(FScrypt).ROMix(input[0], Length(input), 4); //N=16 --> costFactor = 4 (2^4 = 16)

  Self.CheckEquals(Length(expected), Length(actual), 'ROMix array length');
  {$IFNDEF FPC}
  Self.CheckEqualsMem(@expected[0], @actual[0], Length(expected), 'ROMix data failed');
  {$ENDIF}
  assertPass();
end;

procedure TScryptTests.Test_PBKDF2_SHA1;
var
  db: IPBKDF2Algorithm;
begin
  db := TScryptCracker.CreateObject('PBKDF2.SHA1') as IPBKDF2Algorithm;
  Tester_PBKDF2_SHA1(db);
  assertPass();
end;

{$IFNDEF FPC}
procedure TScryptTests.Test_PBKDF2_SHA1_Cng;
var
  db: IPBKDF2Algorithm;
begin
  db := TScryptCracker.CreateObject('PBKDF2.SHA1.Cng') as IPBKDF2Algorithm;
  Tester_PBKDF2_SHA1(db);
  assertPass();
end;
{$ENDIF}

procedure TScryptTests.Test_PBKDF2_SHA1_PurePascal;
var
  db: IPBKDF2Algorithm;
begin
  db := TScryptCracker.CreateObject('PBKDF2.SHA1.PurePascal') as IPBKDF2Algorithm;
  Tester_PBKDF2_SHA1(db);
  assertPass();
end;

procedure TScryptTests.Test_PBKDF2_SHA256;
var
  db: IPBKDF2Algorithm;
begin
  db := TScryptCracker.CreateObject('PBKDF2.SHA256') as IPBKDF2Algorithm;
  Tester_PBKDF2_SHA256(db);
  assertPass();
end;

{$IFNDEF FPC}
procedure TScryptTests.Test_PBKDF2_SHA256_Cng;
var
  db: IPBKDF2Algorithm;
begin
  db := TScryptCracker.CreateObject('PBKDF2.SHA256.Cng') as IPBKDF2Algorithm;
  Tester_PBKDF2_SHA256(db);
  assertPass();
end;
{$ENDIF}

procedure TScryptTests.Test_PBKDF2_SHA256_PurePascal;
var
  db: IPBKDF2Algorithm;
begin
  db := TScryptCracker.CreateObject('PBKDF2.SHA256.PurePascal') as IPBKDF2Algorithm;
  Tester_PBKDF2_SHA256(db);
  assertPass();
end;

procedure TScryptTests.Test_Scrypt;
var
  freq: Int64;

  procedure test(password: string; salt: string; CostFactor, r, p: Integer; DesiredBytes: Integer; ExpectedHexString: string);
  var
    expected: TBytes;
    actual: TBytes;
    t1, t2: Int64;
  begin
    //N = CPU/memory cost parameter
    //p = parallelization parameter
    expected := HexToBytes(ExpectedHexString);

    t1 := GetTimestamp;
    actual := FScrypt.GetBytes(password, salt, CostFactor, r, p, DesiredBytes);
          t2 := GetTimestamp;

    Self.Status(Format('Test "%s" duration: %.4f ms', [password, (t2-t1)/freq*1000]));

    if Length(expected) <> Length(actual) then
      raise EScryptException.CreateFmt('Self-test failed: actual length (%d) does not match expected (%d)', [Length(actual), Length(expected)]);

    if not CompareMem(@expected[0], @actual[0], Length(expected)) then
      raise EScryptException.Create('Scrypt self-test failed: data does not match');
  end;

begin
  {
    From the official PDF (http://www.tarsnap.com/scrypt/scrypt.pdf)

    Appendix B. Test vectors

    For reference purposes, we provide the following test vectors for scrypt, where
    the password and salt strings are passed as sequences of ASCII bytes without a
    terminating NUL
  }
   {$IFDEF WINDOWS}
  if not QueryPerformanceFrequency(freq) then
    freq := -1;
   {$ENDIF}

  //uses 512 bytes
  test('', '', {N=16=2^}4, {r=}1, {p=}1, 64,
      '77 d6 57 62 38 65 7b 20 3b 19 ca 42 c1 8a 04 97 f1 6b 48 44 e3 07 4a e8 df df fa 3f ed e2 14 42'+
      'fc d0 06 9d ed 09 48 f8 32 6a 75 3a 0f c8 1f 17 e8 d3 e0 fb 2e 0d 36 28 cf 35 e2 0c 38 d1 89 06');

  //uses 1 MB
  test('password', 'NaCl', {N=1024=2^}10, {r=}8, {p=}16, 64,
      'fd ba be 1c 9d 34 72 00 78 56 e7 19 0d 01 e9 fe 7c 6a d7 cb c8 23 78 30 e7 73 76 63 4b 37 31 62'+
      '2e af 30 d9 2e 22 a3 88 6f f1 09 27 9d 98 30 da c7 27 af b9 4a 83 ee 6d 83 60 cb df a2 cc 06 40');

  //uses 16 MB
  test('pleaseletmein', 'SodiumChloride', {N=16384=2^}14, {r=}8, {p=}1, 64,
      '70 23 bd cb 3a fd 73 48 46 1c 06 cd 81 fd 38 eb fd a8 fb ba 90 4f 8e 3e a9 b5 43 f6 54 5d a1 f2'+
      'd5 43 29 55 61 3f 0f cf 62 d4 97 05 24 2a 9a f9 e6 1e 85 dc 0d 65 1e 40 df cf 01 7b 45 57 58 87');

  if FindCmdLineSwitch('IncludeSlowTests', ['-','/'], True) then
  begin
    //uses 1 GB
    test('pleaseletmein', 'SodiumChloride', {N=1048576=2^}20, {r=}8, {p=}1, 64,
        '21 01 cb 9b 6a 51 1a ae ad db be 09 cf 70 f8 81 ec 56 8d 57 4a 2f fd 4d ab e5 ee 98 20 ad aa 47'+
        '8e 56 fd 8f 4b a5 d0 9f fa 1c 6d 92 7c 40 f4 c3 37 30 40 49 e8 a9 52 fb cb f4 5c 6f a7 7a 41 a4');

  end
  else
    Status('1 GB slow test skipped. Use -IncludeSlowTests');
  assertPass();
end;

procedure TScryptTests.Test_SHA1_PurePascal;
var
  sha1: IHashAlgorithm;
begin
  sha1 := TScryptCracker(FScrypt).CreateObject('SHA1.PurePascal') as IHashAlgorithm;
  TSHA1Tester.Test(sha1);
  assertPass();
end;

procedure TScryptTests.Test_SHA1;
var
  sha1: IHashAlgorithm;
begin
  sha1 := TScryptCracker.CreateObject('SHA1') as IHashAlgorithm;
  TSHA1Tester.Test(sha1);
  assertPass();
end;

{$IFNDEF FPC}
procedure TScryptTests.Test_SHA1_Cng;
var
  sha1: IHashAlgorithm;
begin
  sha1 := TScryptCracker(FScrypt).CreateObject('SHA1.Cng') as IHashAlgorithm;
  TSHA1Tester.Test(sha1);
  assertPass();
end;

procedure TScryptTests.Test_SHA1_Csp;
var
  sha1: IHashAlgorithm;
begin
  sha1 := TScryptCracker(FScrypt).CreateObject('SHA1.Csp') as IHashAlgorithm;
  TSHA1Tester.Test(sha1);
  assertPass();
end;
{$ENDIF}
procedure registerTests;
begin
  RegisterTest('Library.Scrypt', TScryptTests.Suite);
end;

end.
