unit fsl_openssl;

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

{
Various helper utilities associated with supporting OpenSSL 1.1.x
}

interface

uses
  SysUtils, Classes,
  IdGlobal, IdCTypes, IdFIPS,
  IdOpenSSLHeaders_ossl_typ, IdOpenSSLHeaders_rsa, IdOpenSSLHeaders_dsa, IdOpenSSLHeaders_bn, IdOpenSSLHeaders_bio,
  IdOpenSSLHeaders_hmac, IdOpenSSLHeaders_pem, IdOpenSSLHeaders_err, IdOpenSSLHeaders_x509, IdOpenSSLHeaders_evp, IdOpenSSLHeaders_crypto,
  IdOpenSSLVersion, IdOpenSSLX509, IdOpenSSLLoader,
  fsl_base;

type
  TIdX509Helper = class helper for TIdOpenSSLX509
  private
    function GetCanonicalName: String;
  public
    property CanonicalName : String read GetCanonicalName;
  end;

function BN_num_bytes(const a: pBIGNUM): integer;
function BIO_read_filename(b : PBIO; name : PIdAnsiChar) : TIdC_LONG; inline;
function GetSSLErrorMessage: string;
//procedure EVP_SignInit(ctx: PEVP_MD_CTX; md: PEVP_MD);
//function EVP_SignUpdate(ctx: PEVP_MD_CTX; data: PByte; cnt: integer): integer;
//procedure EVP_VerifyInit(ctx: PEVP_MD_CTX; md: PEVP_MD);
//function EVP_VerifyUpdate(ctx: PEVP_MD_CTX; data: PByte; cnt: integer): integer;

function idb(b : TIdBytes) : TBytes; overload;
function idb(b : TBytes) : TIdBytes; overload;

procedure InitOpenSSL;

implementation

function BN_num_bytes(const a: pBIGNUM): integer;
begin
  result := (BN_num_bits(a) + 7) div 8;
end;

function BIO_read_filename(b : PBIO; name : PIdAnsiChar) : TIdC_LONG; inline;
begin
  Result := BIO_ctrl(b,BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_READ,name);
end;

function GetSSLErrorMessage: string;
const
  BUFF_SIZE = 128; // OpenSSL docs state should be >= 120 bytes
var
  err: TBytes;
begin
  SetLength(err, BUFF_SIZE);
  ERR_error_string(ERR_get_error, @err[0]);
  result := string(err);
end;


{ TIdX509Helper }

function TIdX509Helper.GetCanonicalName() : String;
//var
//  s : String;
//  p : TArray<String>;
begin
  result := String(self.Subject.CN);
//  p := s.Split(['/']);
//  result := '(no canonical name)';
//  for s in p do
//    if (s.StartsWith('CN=')) then
//      exit(s.Substring(3));
end;


function idb(b : TIdBytes) : TBytes;
begin
  SetLength(result, length(b));
  if (length(b) > 0) then
    move(b[0], result[0], length(b));
end;

function idb(b : TBytes) : TIdBytes;
begin
  SetLength(result, length(b));
  if (length(b) > 0) then
    move(b[0], result[0], length(b));
end;


function OpenSSLIsHashingIntfAvail : Boolean;
begin
  Result := Assigned(EVP_DigestInit_ex) and
            Assigned(EVP_DigestUpdate) and
            Assigned(EVP_DigestFinal_ex) ;
end;

function OpenSSLGetFIPSMode : Boolean;
begin
  Result := FIPS_mode <> 0;
end;

function OpenSSLSetFIPSMode(const AMode : Boolean) : Boolean;
begin
  //leave this empty as we may not be using something that supports FIPS
  if AMode then begin
    Result := FIPS_mode_set(1) = 1;
  end else begin
    Result := FIPS_mode_set(0) = 1;
  end;
end;

function OpenSSLGetDigestCtx( AInst : PEVP_MD) : TIdHashIntCtx; inline;
var LRet : Integer;
begin
  Result := AllocMem(SizeOf(EVP_MD_CTX));
  EVP_MD_CTX_reset(Result);

  LRet := EVP_DigestInit_ex(Result, AInst, nil);
  if LRet <> 1 then begin
    raise ELibraryException.create('EVP_DigestInit_ex error');
  end;
end;

function OpenSSLIsMD2HashIntfAvail: Boolean;
begin
  Result := False;
end;

function OpenSSLGetMD2HashInst : TIdHashIntCtx;
begin
  Result := nil;
end;

function OpenSSLIsMD4HashIntfAvail: Boolean;
begin
  Result := false;
end;

function OpenSSLGetMD4HashInst : TIdHashIntCtx;
begin
  Result := nil;
end;

function OpenSSLIsMD5HashIntfAvail: Boolean;
begin
  Result := Assigned(EVP_md5);
end;

function OpenSSLGetMD5HashInst : TIdHashIntCtx;
var
  LRet : PEVP_MD;
begin
  LRet := EVP_md5;
  Result := OpenSSLGetDigestCtx(LRet);
end;

function OpenSSLIsSHA1HashIntfAvail: Boolean;
begin
  Result := False;
end;

function OpenSSLGetSHA1HashInst : TIdHashIntCtx;
begin
  Result := nil;
end;

function OpenSSLIsSHA224HashIntfAvail: Boolean;
begin
  Result := Assigned(EVP_sha224);
end;

function OpenSSLGetSHA224HashInst : TIdHashIntCtx;
var
  LRet : PEVP_MD;
begin
  LRet := EVP_sha224;
  Result := OpenSSLGetDigestCtx(LRet);
end;

function OpenSSLIsSHA256HashIntfAvail: Boolean;
begin
  Result := Assigned(EVP_sha256);
end;

function OpenSSLGetSHA256HashInst : TIdHashIntCtx;
var
  LRet : PEVP_MD;
begin
  LRet := EVP_sha256;
  Result := OpenSSLGetDigestCtx(LRet);
end;

function OpenSSLIsSHA384HashIntfAvail: Boolean;
begin
  Result := Assigned(EVP_sha384);
end;

function OpenSSLGetSHA384HashInst : TIdHashIntCtx;
var
  LRet : PEVP_MD;
begin
  LRet := EVP_sha384;
  Result := OpenSSLGetDigestCtx(LRet);
end;

function OpenSSLIsSHA512HashIntfAvail: Boolean;
begin
  Result := Assigned(EVP_sha512);
end;

function OpenSSLGetSHA512HashInst : TIdHashIntCtx;
var
  LRet : PEVP_MD;
begin
  LRet := EVP_sha512;
  Result := OpenSSLGetDigestCtx(LRet);
end;

procedure OpenSSLUpdateHashInst(ACtx: TIdHashIntCtx; const AIn: TIdBytes);
var
  LRet : TIdC_Int;
begin
  LRet := EVP_DigestUpdate(ACtx, PByte(Ain), Length(AIn));
  if LRet <> 1 then
    raise ELibraryException.create('EVP_DigestUpdate error');
end;

function OpenSSLFinalHashInst(ACtx: TIdHashIntCtx): TIdBytes;
var
  LLen : TIdC_UInt;
  LRet : TIdC_Int;
begin
  SetLength(Result,EVP_MAX_MD_SIZE);
  LRet := EVP_DigestFinal_ex(ACtx, @Result[0], @LLen);
  if LRet <> 1 then
    raise ELibraryException.create('EVP_DigestFinal_ex error');

  SetLength(Result,LLen);
  EVP_MD_CTX_free(ACtx);
  FreeMem(ACtx,SizeOf(EVP_MD_CTX));
end;

function OpenSSLIsHMACAvail : Boolean;
begin
  Result := Assigned(HMAC_CTX_new) and Assigned(HMAC_Init_ex) and Assigned(HMAC_Update) and Assigned(HMAC_Final) and Assigned(HMAC_CTX_free);
end;

function OpenSSLIsHMACMD5Avail: Boolean;
begin
 Result := Assigned(EVP_md5);
end;

function OpenSSLGetHMACMD5Inst(const AKey : TIdBytes) : TIdHMACIntCtx;
begin
  Result := HMAC_CTX_new;
  HMAC_Init_ex(Result, PByte(AKey), Length(AKey), EVP_md5, nil);
end;

function OpenSSLIsHMACSHA1Avail: Boolean;
begin
  Result := Assigned(EVP_sha1);
end;

function OpenSSLGetHMACSHA1Inst(const AKey : TIdBytes) : TIdHMACIntCtx;
begin
  Result := HMAC_CTX_new;
  HMAC_Init_ex(Result, PByte(AKey), Length(AKey), EVP_sha1, nil);
end;

function OpenSSLIsHMACSHA224Avail: Boolean;
begin
  Result := Assigned(EVP_sha224);
end;

function OpenSSLGetHMACSHA224Inst(const AKey : TIdBytes) : TIdHMACIntCtx;
begin
  Result := HMAC_CTX_new;
  HMAC_Init_ex(Result, PByte(AKey), Length(AKey), EVP_sha224, nil);
end;

function OpenSSLIsHMACSHA256Avail: Boolean;
begin
  Result := Assigned(EVP_sha256);
end;

function OpenSSLGetHMACSHA256Inst(const AKey : TIdBytes) : TIdHMACIntCtx;
begin
  Result := HMAC_CTX_new;
  HMAC_Init_ex(Result, PByte(AKey), Length(AKey), EVP_sha256, nil);
end;

function OpenSSLIsHMACSHA384Avail: Boolean;
begin
  Result := Assigned(EVP_sha384);
end;

function OpenSSLGetHMACSHA384Inst(const AKey : TIdBytes) : TIdHMACIntCtx;
begin
  Result := HMAC_CTX_new;
  HMAC_Init_ex(Result, PByte(AKey), Length(AKey), EVP_sha384, nil);
end;

function OpenSSLIsHMACSHA512Avail: Boolean;
begin
  Result := Assigned(EVP_sha512);
end;

function OpenSSLGetHMACSHA512Inst(const AKey : TIdBytes) : TIdHMACIntCtx;
begin
  Result := HMAC_CTX_new;
  HMAC_Init_ex(Result, PByte(AKey), Length(AKey), EVP_sha512, nil);
end;

procedure OpenSSLUpdateHMACInst(ACtx : TIdHMACIntCtx; const AIn: TIdBytes);
begin
  HMAC_Update(ACtx, PByte(AIn), Length(AIn));
end;

function OpenSSLFinalHMACInst(ACtx: TIdHMACIntCtx): TIdBytes;
var
  LLen : TIdC_UInt;
begin
  LLen := EVP_MAX_MD_SIZE;
  SetLength(Result,LLen);
  HMAC_Final(ACtx, @Result[0], @LLen);
  SetLength(Result,LLen);
  HMAC_CTX_free(ACtx);
end;


procedure InstallIndyRoutines;
begin
//  SetFIPSMode := OpenSSLSetFIPSMode;
//  GetFIPSMode := OpenSSLGetFIPSMode;
//  IsHashingIntfAvail := OpenSSLIsHashingIntfAvail;
//  IsMD2HashIntfAvail := OpenSSLIsMD2HashIntfAvail;
//  GetMD2HashInst := OpenSSLGetMD2HashInst;
//  IsMD4HashIntfAvail := OpenSSLIsMD4HashIntfAvail;
//  GetMD4HashInst := OpenSSLGetMD4HashInst;
//  IsMD5HashIntfAvail := OpenSSLIsMD5HashIntfAvail;
//  GetMD5HashInst := OpenSSLGetMD5HashInst;
//  IsSHA1HashIntfAvail := OpenSSLIsSHA1HashIntfAvail;
//  GetSHA1HashInst := OpenSSLGetSHA1HashInst;
//  IsSHA224HashIntfAvail := OpenSSLIsSHA224HashIntfAvail;
//  GetSHA224HashInst := OpenSSLGetSHA224HashInst;
//  IsSHA256HashIntfAvail := OpenSSLIsSHA256HashIntfAvail;
//  GetSHA256HashInst := OpenSSLGetSHA256HashInst;
//  IsSHA384HashIntfAvail := OpenSSLIsSHA384HashIntfAvail;
//  GetSHA384HashInst := OpenSSLGetSHA384HashInst;
//  IsSHA512HashIntfAvail := OpenSSLIsSHA512HashIntfAvail;
//  GetSHA512HashInst := OpenSSLGetSHA512HashInst;
//  UpdateHashInst := OpenSSLUpdateHashInst;
//  FinalHashInst := OpenSSLFinalHashInst;

  IsHMACAvail := OpenSSLIsHMACAvail;

  //  IsHMACMD5Avail := OpenSSLIsHMACMD5Avail;
//  GetHMACMD5HashInst := OpenSSLGetHMACMD5Inst;
//  IsHMACSHA1Avail  := OpenSSLIsHMACSHA1Avail;
//  GetHMACSHA1HashInst:= OpenSSLGetHMACSHA1Inst;
//  IsHMACSHA224Avail := OpenSSLIsHMACSHA224Avail;
//  GetHMACSHA224HashInst:= OpenSSLGetHMACSHA224Inst;

  IsHMACSHA256Avail := OpenSSLIsHMACSHA256Avail;
  GetHMACSHA256HashInst:= OpenSSLGetHMACSHA256Inst;

//  IsHMACSHA384Avail := OpenSSLIsHMACSHA384Avail;
//  GetHMACSHA384HashInst:= OpenSSLGetHMACSHA384Inst;
//  IsHMACSHA512Avail := OpenSSLIsHMACSHA512Avail;
//  GetHMACSHA512HashInst:= OpenSSLGetHMACSHA512Inst;

  UpdateHMACInst := OpenSSLUpdateHMACInst;
  FinalHMACInst := OpenSSLFinalHMACInst;
end;

procedure InitOpenSSL;
begin
  if not GetOpenSSLLoader.Load then
    raise Exception.create('open SSL failed to load');
  InstallIndyRoutines;
end;

end.
