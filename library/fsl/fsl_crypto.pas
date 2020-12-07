unit fsl_crypto;

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
Digital Signature support for FHIR Server

introduction:

This is written to support digital signatures in the FHIR server, but
it can be used elsewhere too. Dependencies:
 - Indy 10
 - the support library for FHIR Server
It should compile and work under any unicode version of delphi, but
may be very fragile against changes to openXML. It's currently
developed and tested using XE5

About XML:

The hardest part of digital signatures is actually XML canonicalization.
You can't use MS-XML for this - it's too loose with the XML data. Instead,
this source uses the openXML provider. You can't change this here; You
can use ms-xml elsewhere, but not in here. This is why the interface
is entirely binary, and not based on a DOM.

OpenSSL

Tested against openSSL 1.0.1g. These interfaces are stable, but you shouldn't
use anything older than this anyway. note: you must have deployment infrastructure
for keeping up with openSSL bug changes. You have to initialise OpenSSL correctly
before using:
  LoadEAYExtensions;
  ERR_load_crypto_strings;
  OpenSSL_add_all_algorithms;

Since these only need to be done once, this object doesn't do them

Certificates

you have to generate certificates using openSSL or equivalent, and refer
to them here. You have to nominate a method (DSA/RSA) which matches the
certificate you nominate
}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes,
  IdGlobal, IdCTypes, IdHMAC, IdHash, IdHashSHA, IdHMACSHA1,
  IdOpenSSLHeaders_ossl_typ, IdOpenSSLHeaders_rsa, IdOpenSSLHeaders_dsa, IdOpenSSLHeaders_bn, IdOpenSSLHeaders_bio, IdOpenSSLHeaders_hmac,
  IdOpenSSLHeaders_pem, IdOpenSSLHeaders_err, IdOpenSSLHeaders_x509, IdOpenSSLHeaders_evp, IdOpenSSLX509,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_json, fsl_xml,
  fsl_openssl, fsl_fetcher;

Type
  TIdHMACClass = class of TIdHMAC;
  THMACUtils = class
  public
    class function HMAC(alg : TIdHMACClass; aKey, aMessage: TBytes): TBytes;
    class function HMAC_HexStr(alg : TIdHMACClass; aKey, aMessage: TBytes): TBytes;
    class function HMAC_Base64(alg : TIdHMACClass; aKey, aMessage: TBytes): AnsiString;
  end;

  // 1st, JWK
  TJWK = class (TFslObject)
  private
    FObj : TJsonObject;
    function GetExponent: TBytes;
    function GetId: String;
    function GetKey: TBytes;
    function GetKeyType: String;
    function GetPrivateKey: TBytes;
    function GetPublicKey: TBytes;
    procedure SetExponent(const Value: TBytes);
    procedure SetId(const Value: String);
    procedure SetKey(const Value: TBytes);
    procedure SetKeyType(const Value: String);
    procedure setObj(const Value: TJsonObject);
    procedure SetPrivateKey(const Value: TBytes);
    procedure SetPublicKey(const Value: TBytes);
    function GetHasExponent: boolean;
    function GetHasKey: boolean;
    function GetHasPrivateKey: boolean;
    function GetHasPublicKey: boolean;
    function GetG: TBytes;
    function GetP: TBytes;
    function GetQ: TBytes;
    function GetX: TBytes;
    function GetY: TBytes;
    procedure SetG(const Value: TBytes);
    procedure SetP(const Value: TBytes);
    procedure SetQ(const Value: TBytes);
    procedure SetX(const Value: TBytes);
    procedure SetY(const Value: TBytes);
    function GetHasG: boolean;
    function GetHasP: boolean;
    function GetHasQ: boolean;
    function GetHasX: boolean;
    function GetHasY: boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(obj : TJsonObject); overload;
    constructor Create(pkey : PRSA; loadPrivate : Boolean); overload;
    constructor Create(pkey : PDSA; loadPrivate : Boolean); overload;
    destructor Destroy; override;
    Property obj : TJsonObject read FObj write setObj;

    property keyType : String read GetKeyType write SetKeyType;
    property id : String read GetId write SetId;

    // RSA
    property key : TBytes read GetKey write SetKey; // k
    property publicKey : TBytes read GetPublicKey write SetPublicKey; // n
    property exponent : TBytes read GetExponent write SetExponent; // e
    property privateKey : TBytes read GetPrivateKey write SetPrivateKey; // d

    // DSA
    property P : TBytes read GetP write SetP;
    property Q : TBytes read GetQ write SetQ;
    property G : TBytes read GetG write SetG;
    property X : TBytes read GetX write SetX;
    property Y : TBytes read GetY write SetY;

    property hasKey : boolean read GetHasKey;
    property hasPublicKey : boolean read GetHasPublicKey;
    property hasExponent : boolean read GetHasExponent;
    property hasPrivateKey : boolean read GetHasPrivateKey;
    property hasP : boolean read GetHasP;
    property hasQ : boolean read GetHasQ;
    property hasG : boolean read GetHasG;
    property hasX : boolean read GetHasX;
    property hasY : boolean read GetHasY;

    procedure clearKey;
    procedure clearPublicKey;
    procedure clearExponent;
    procedure clearPrivateKey;

    function Load(privkey : boolean) : PRSA;
  end;

  TJWKList = class (TFslObjectList)
  private
    function GetKey(index: integer): TJWK;
    procedure Setkey(index: integer; const Value: TJWK);
  protected
    function ItemClass : TFslObjectClass; override;
  public
    constructor Create(obj : TJsonObject); overload;
    constructor Create(source : String); overload;
    destructor Destroy; override;
    procedure readFromJson(obj : TJsonObject);
    procedure writeToJson(obj : TJsonObject);

    property Key[index : integer] : TJWK read GetKey write Setkey; default;
  end;

  TJWTAlgorithm = (jwt_none, jwt_hmac_sha256, jwt_hmac_rsa256);


{

 +---------------+------------------------------+--------------------+
   | alg Parameter | Digital Signature or MAC     | Implementation     |
   | Value         | Algorithm                    | Requirements       |
   +---------------+------------------------------+--------------------+
   | HS256         | HMAC using SHA-256           | Required           |
   | HS384         | HMAC using SHA-384           | Optional           |
   | HS512         | HMAC using SHA-512           | Optional           |
   | RS256         | RSASSA-PKCS-v1_5 using       | Recommended        |
   |               | SHA-256                      |                    |
   | RS384         | RSASSA-PKCS-v1_5 using       | Optional           |
   |               | SHA-384                      |                    |
   | RS512         | RSASSA-PKCS-v1_5 using       | Optional           |
   |               | SHA-512                      |                    |
   | ES256         | ECDSA using P-256 and        | Recommended+       |
   |               | SHA-256                      |                    |
   | ES384         | ECDSA using P-384 and        | Optional           |
   |               | SHA-384                      |                    |
   | ES512         | ECDSA using P-521 and        | Optional           |
   |               | SHA-512                      |                    |
   | PS256         | RSASSA-PSS using SHA-256 and | Optional           |
   |               | MGF1 with SHA-256            |                    |
   | PS384         | RSASSA-PSS using SHA-384 and | Optional           |
   |               | MGF1 with SHA-384            |                    |
   | PS512         | RSASSA-PSS using SHA-512 and | Optional           |
   |               | MGF1 with SHA-512            |                    |
   | none          | No digital signature or MAC  | Optional           |
   |               | performed                    |                    |
   +---------------+------------------------------+--------------------+
}



  TJWTUtils = class (TFslObject)
  private
//    class function loadRSAPrivateKey(pemfile, pempassword : AnsiString) : PRSA;
//    class function loadDSAPrivateKey(pemfile, pempassword : AnsiString) : PDSA;
    class function loadRSAPublicKey(pemfile : AnsiString) : PRSA; overload;
    class function loadRSAPublicKey(contents : TBytes) : PRSA; overload;
    class function loadDSAPublicKey(pemfile, pempassword : AnsiString) : PDSA;

    class function Sign_Hmac_SHA256(input : TBytes; key: TJWK) : TBytes;
    class procedure Verify_Hmac_SHA256(input : TBytes; sig : TBytes; key: TJWK);
    class function Sign_Hmac_RSA256(input : TBytes; key: TJWK) : TBytes; overload;
    class procedure Verify_Hmac_RSA256(input : TBytes; sig : TBytes; header : TJsonObject; keys: TJWKList);
  public
    class function Sign_Hmac_RSA256(input : TBytes; pemfile, pempassword : String) : TBytes; overload;


    // general use: pack a JWT using the key speciifed. No key needed if method = none
    class function pack(jwt : TJWT; method : TJWTAlgorithm; key : TJWK) : String; overload;
    class function pack(jwt : TJWT; method : TJWTAlgorithm; key : TJWK; pemfile, pempassword : String) : String; overload;

    // special use - use an existing PEM to sign the JWT
    class function rsa_pack(jwt : TJWT; method : TJWTAlgorithm; pem_file, pem_password : String) : String; overload;


    // for testing only - need to control whitespace in order to reproduce signatures
    class function pack(header, payload : String; method : TJWTAlgorithm; key : TJWK) : String; overload;

    // read a JWT. if verify is true, at least one key must be provided.
    // the code will pick between multiple keys based on the key id.
    // if no key id is provided in the JWT, there can only be one key
    //
    // todo: what if you don't know?
    class function unpack(token : string; verify : boolean; keys : TJWKList) : TJWT;

    // load the publi key details from the provided filename
    class function loadKeyFromDSACert(filename, password : AnsiString) : TJWK;
    class function loadKeyFromRSACert(filename : AnsiString) : TJWK; overload;
    class function loadKeyFromRSACert(content : TBytes) : TJWK; overload;
  end;


Const
  NS_DS = 'http://www.w3.org/2000/09/xmldsig#';

Type
  TSignatureMethod = (sdXmlDSASha1, sdXmlRSASha1, sdXmlDSASha256, sdXmlRSASha256);

  TDigitalSignatureReference = class (TFslObject)
  private
    FUrl: String;
    FTransforms: TStringList;
    FContent: TBytes;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property url: String read FUrl write FUrl;
    property transforms : TStringList read FTransforms;
    property content : TBytes read FContent write FContent;
  end;

  TDigitalSignatureReferenceList = class (TFslObjectList)
  private
    function getReference(i: integer): TDigitalSignatureReference;

  protected
    function ItemClass : TFslObjectClass; override;
  public
    property reference[i : integer] : TDigitalSignatureReference read getReference; default;
  end;

  TKeyInfo = class (TFslObject)
  private
//    dsa : PDSA;
//    rsa : PRSA;
    function checkSignatureRSA(digest, signature : TBytes; method : TSignatureMethod) : boolean;
    function checkSignatureDSA(digest, signature : TBytes; method : TSignatureMethod) : boolean;
    function checkSignature(digest, signature : TBytes; method : TSignatureMethod) : boolean;
  public
    destructor Destroy; override;
  end;

  {$IFNDEF FPC}
  TSignatureLocationFinderMethod = reference to function (doc : TMXmlDocument) : TMXmlElement;
  {$ENDIF}

  TDigitalSigner = class (TFslObject)
  private
    FPublicKey: AnsiString;
    FPrivateKey: AnsiString;
    FKeyPassword: AnsiString;
//    FCertFile: AnsiString;

    // attribute / enum methods
    function canoncalizationSet(uri : String) : TXmlCanonicalisationMethodSet;
    function signatureMethod(uri : String) : TSignatureMethod;
    function digestAlgorithmForMethod(method: TSignatureMethod): String;
    function signAlgorithmForMethod(method: TSignatureMethod): String;

    // xml routines
    function loadXml(source: TBytes) : TMXmlDocument;
    function canonicaliseXml(method : TXmlCanonicalisationMethodSet; source : TBytes; var dom : TMXmlDocument) : TBytes; overload;
    function canonicaliseXml(method : TXmlCanonicalisationMethodSet; dom : TMXmlElement) : TBytes; overload;
//    function canonicaliseXml(method : TXmlCanonicalisationMethodSet; dom : TMXmlDocument) : TBytes; overload;
    function canonicaliseXml(method : TXmlCanonicalisationMethodSet; source : TBytes) : TBytes; overload;

    // digest and signing routine
    function digest(source : TBytes; method : TSignatureMethod) : TBytes;
    function digestSHA1(source : TBytes) : TBytes;
    function digestSHA256(source : TBytes) : TBytes;
    procedure checkDigest(ref : TMXmlElement; doc : TMXmlDocument);
    function sign(src: TBytes; method: TSignatureMethod): TBytes;
    function signDSA(src: TBytes; method: TSignatureMethod): TBytes;
    function signRSA(src: TBytes; method: TSignatureMethod): TBytes;

    // key/ certificate management routines
    function LoadKeyInfo(sig : TMXmlElement) : TKeyInfo;
//    function loadRSAKey: PRSA;
//    function loadDSAKey: PDSA;
    procedure AddKeyInfo(sig: TMXmlElement; method : TSignatureMethod);

    // source content management
    function resolveReference(url : string) : TBytes;

  public
    constructor Create; override;

    // certificate files, for signing
    Property PublicKey : AnsiString read FPublicKey write FPublicKey;
    Property PrivateKey : AnsiString read FPrivateKey write FPrivateKey;
    Property KeyPassword : AnsiString read FKeyPassword write FKeyPassword;

    // classic XML Enveloped Signature
    function signEnveloped(xml : TBytes; method : TSignatureMethod; keyinfo : boolean) : TBytes; overload;
    {$IFNDEF FPC}
    function signEnveloped(xml : TBytes; finder : TSignatureLocationFinderMethod; method : TSignatureMethod; keyinfo : boolean) : TBytes; overload;
    {$ENDIF}

    function signDetached(xml : TBytes; refUrl : String; method : TSignatureMethod; canonicalization : string; keyinfo : boolean) : TBytes; overload;

    function signExternal(references : TDigitalSignatureReferenceList; method : TSignatureMethod; keyinfo : boolean) : TBytes;
    function verifySignature(xml : TBytes) : boolean;
  end;


implementation

// utilities


function JWTBase64URL(b : TBytes) : TBytes; overload;
var
  b64 : String;
begin
  b64 := String(EncodeBase64(b));
  b64 := StringReplace(b64, #13#10, '').TrimRight(['=']);
  b64 := StringReplace(b64, '+', '-', [rfReplaceAll]);
  b64 := StringReplace(b64, '/', '_', [rfReplaceAll]);
  result := AnsiStringAsBytes(AnsiString(b64));
end;

function JWTBase64URLStr(b : TBytes) : String; overload;
var
  b64 : String;
begin
  b64 := String(EncodeBase64(b));
  b64 := StringReplace(b64, #13#10, '').TrimRight(['=']);
  b64 := StringReplace(b64, '+', '-', [rfReplaceAll]);
  b64 := StringReplace(b64, '/', '_', [rfReplaceAll]);
  result := b64;
end;

function JWTBase64URL(s : String) : TBytes; overload;
begin
  result := JWTBase64URL(TEncoding.UTF8.GetBytes(s));
end;

function JWTBase64URLStr(s : String) : String; overload;
begin
  result := JWTBase64URLStr(TEncoding.UTF8.GetBytes(s));
end;

function JWTDeBase64URL(s : String) : TBytes;
begin
  s := s + StringOfChar ('=', (4 - Length (s) mod 4) mod 4);
  s := StringReplace (s, '-', '+', [rfReplaceAll]);
  s := StringReplace (s, '_', '/', [rfReplaceAll]);
  result := DecodeBase64(AnsiString(s));
end;

function JWTDeBase64URLUTF8(s : String) : string;
begin
  result := TEncoding.UTF8.GetString(JWTDeBase64URL(s));
end;

procedure check(test: boolean; failmsg: string);
begin
  if not test then
    raise ELibraryException.create(failmsg);
end;

{ THMACUtils }

class function THMACUtils.HMAC(alg : TIdHMACClass; aKey, aMessage: TBytes): TBytes;
var
  _alg : TIdHMAC;
begin
  _alg := alg.Create;
  try
    _alg.Key := idb(aKey);
    Result:= idb(_alg.HashValue(idb(aMessage)));
  finally
    _alg.Free;
  end;
end;

class function THMACUtils.HMAC_HexStr(alg : TIdHMACClass; aKey, aMessage: TBytes): TBytes;
var
  I: Byte;
begin
  Result:= AnsiStringAsBytes('0x');
  for I in HMAC(alg, aKey, aMessage) do
    Result:= BytesAdd(Result, AnsiStringAsBytes(AnsiString(IntToHex(I, 2))));
end;

class function THMACUtils.HMAC_Base64(alg : TIdHMACClass; aKey, aMessage: TBytes): AnsiString;
var
  _HMAC: TBytes;
begin
  _HMAC := HMAC(alg, aKey, aMessage);
  Result := EncodeBase64(_HMAC);
end;

{ TJWK }

constructor TJWK.create(obj: TJsonObject);
begin
  create;
  FObj := obj;
end;

procedure TJWK.clearExponent;
begin
  FObj.clear('e');
end;

procedure TJWK.clearKey;
begin
  FObj.clear('k');
end;

procedure TJWK.clearPrivateKey;
begin
  FObj.clear('d');
end;

procedure TJWK.clearPublicKey;
begin
  FObj.clear('n');
end;

constructor TJWK.create(pkey: PRSA; loadPrivate : Boolean);
var
  b : TBytes;
  pe, pn, pd : PBIGNUM;
begin
  create;
  obj := TJsonObject.Create;
  keyType := 'RSA';

  RSA_get0_key(pKey, @pn, @pe, @pd);
  if (pe <> nil) then
  begin
    setlength(b,  BN_num_bytes(pe));
    BN_bn2bin(pe, @b[0]);
    exponent := b;
  end;
  if (pn <> nil) then
  begin
    setlength(b,  BN_num_bytes(pn));
    BN_bn2bin(pn, @b[0]);
    publicKey := b;
  end;
  if (pd <> nil) then
  begin
    setlength(b,  BN_num_bytes(pd));
    BN_bn2bin(pd, @b[0]);
    privateKey := b;
  end;
end;

destructor TJWK.Destroy;
begin
  FObj.Free;
  inherited;
end;

function TJWK.GetExponent: TBytes;
begin
  result := JWTDeBase64URL(FObj['e']);
end;

function TJWK.GetG: TBytes;
begin
  result := JWTDeBase64URL(FObj['g']);
end;

function TJWK.GetHasExponent: boolean;
begin
  result := FObj.has('e');
end;

function TJWK.GetHasG: boolean;
begin
  result := FObj.has('g');
end;

function TJWK.GetHasKey: boolean;
begin
  result := FObj.has('k');
end;

function TJWK.GetHasP: boolean;
begin
  result := FObj.has('p');
end;

function TJWK.GetHasPrivateKey: boolean;
begin
  result := FObj.has('d');
end;

function TJWK.GetHasPublicKey: boolean;
begin
  result := FObj.has('n');
end;

function TJWK.GetHasQ: boolean;
begin
  result := FObj.has('q');
end;

function TJWK.GetHasX: boolean;
begin
  result := FObj.has('x');
end;

function TJWK.GetHasY: boolean;
begin
  result := FObj.has('y');
end;

function TJWK.GetId: String;
begin
  result := FObj['kid'];
end;

function TJWK.GetKey: TBytes;
begin
  result := JWTDeBase64URL(FObj['k']);
end;

function TJWK.GetKeyType: String;
begin
  result := FObj['kty'];
end;

function TJWK.GetP: TBytes;
begin
  result := JWTDeBase64URL(FObj['p']);
end;

function TJWK.GetPrivateKey: TBytes;
begin
  result := JWTDeBase64URL(FObj['d']);
end;

function TJWK.GetPublicKey: TBytes;
begin
  result := JWTDeBase64URL(FObj['n']);
end;

function TJWK.GetQ: TBytes;
begin
  result := JWTDeBase64URL(FObj['q']);
end;

function TJWK.GetX: TBytes;
begin
  result := JWTDeBase64URL(FObj['x']);
end;

function TJWK.GetY: TBytes;
begin
  result := JWTDeBase64URL(FObj['y']);
end;

procedure TJWK.SetExponent(const Value: TBytes);
begin
  FObj['e'] := String(BytesAsAnsiString(JWTBase64URL(Value)));
end;

procedure TJWK.SetG(const Value: TBytes);
begin
  FObj['g'] := String(BytesAsAnsiString(JWTBase64URL(Value)));
end;

procedure TJWK.SetId(const Value: String);
begin
  FObj['kid'] := Value;
end;

procedure TJWK.SetKey(const Value: TBytes);
begin
  FObj['k'] := String(BytesAsAnsiString(JWTBase64URL(Value)));
end;

procedure TJWK.SetKeyType(const Value: String);
begin
  FObj['kty'] := Value;
end;

procedure TJWK.setObj(const Value: TJsonObject);
begin
  FObj.Free;
  FObj := Value;
end;

procedure TJWK.SetP(const Value: TBytes);
begin
  FObj['p'] := String(BytesAsAnsiString(JWTBase64URL(Value)));
end;

procedure TJWK.SetPrivateKey(const Value: TBytes);
begin
  FObj['d'] := String(BytesAsAnsiString(JWTBase64URL(Value)));
end;

procedure TJWK.SetPublicKey(const Value: TBytes);
begin
  FObj['n'] := String(BytesAsAnsiString(JWTBase64URL(Value)));
end;

procedure TJWK.SetQ(const Value: TBytes);
begin
  FObj['q'] := String(BytesAsAnsiString(JWTBase64URL(Value)));
end;

procedure TJWK.SetX(const Value: TBytes);
begin
  FObj['x'] := String(BytesAsAnsiString(JWTBase64URL(Value)));
end;

procedure TJWK.SetY(const Value: TBytes);
begin
  FObj['y'] := String(BytesAsAnsiString(JWTBase64URL(Value)));
end;

function TJWK.Load(privKey : boolean): PRSA;
var
  b : TBytes;
  pe, pn, pd : PBIGNUM;
begin
  check(keyType = 'RSA', 'RSA Key expected in JWK, but found '+KeyType);
  check(hasExponent, 'RSA Key needs an exponent');
  if (privkey) then
    check(hasPrivateKey, 'RSA Key needs an private key')
  else
    check(hasPublicKey, 'RSA Key needs an public key');

  result := RSA_new;
  b := exponent;
  pe := BN_bin2bn(@b[0], length(b), nil);
  if hasPublicKey then
  begin
    b := publicKey;
    pn := BN_bin2bn(@b[0], length(b), nil);
  end
  else
    raise Exception.Create('Cannot load a key without a public key');
  if privKey and hasPrivateKey then
  begin
    b := privateKey;
    pd := BN_bin2bn(@b[0], length(b), nil);
  end
  else
    pd := nil;
  RSA_set0_key(result, pn, pe, pd);
end;

constructor TJWK.create(pkey: PDSA; loadPrivate : Boolean);
var
  b : TBytes;
  pp, pq, pg, pPub, pPriv : PBIGNUM;
begin
  create;
  obj := TJsonObject.Create;
  keyType := 'DSA';
  DSA_get0_pqg(pkey, @pp, @pq, @pg);
  if (pp <> nil) then
  begin
    setlength(b,  BN_num_bytes(pp));
    BN_bn2bin(pp, @b[0]);
    P := b;
  end;
  if (pq <> nil) then
  begin
    setlength(b,  BN_num_bytes(pq));
    BN_bn2bin(pq, @b[0]);
    Q := b;
  end;
  if (pg <> nil) then
  begin
    setlength(b,  BN_num_bytes(pg));
    BN_bn2bin(pg, @b[0]);
    G := b;
  end;
  DSA_get0_key(pkey, @pPub, @pPriv);
  if (pPub <> nil) then
  begin
    setlength(b,  BN_num_bytes(pPub));
    BN_bn2bin(pPub, @b[0]);
    Y := b;
  end;
  if loadPrivate and (pPriv <> nil) then
  begin
    setlength(b,  BN_num_bytes(pPriv));
    BN_bn2bin(pPriv, @b[0]);
    X := b;
  end;
end;

function TJWK.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FObj.sizeInBytes);
end;

{ TJWKList }

constructor TJWKList.create(obj: TJsonObject);
begin
  Create;
  ReadFromJson(obj);
end;

constructor TJWKList.create(source: String);
var
  json : TJsonObject;
begin
  Create;
  json := TJSONParser.Parse(source);
  try
    readFromJson(json);
  finally
    json.Free;
  end;
end;

destructor TJWKList.Destroy;
begin
  inherited;
end;

function TJWKList.GetKey(index: integer): TJWK;
begin
  result := TJWK(ObjectByIndex[index]);
end;

function TJWKList.ItemClass: TFslObjectClass;
begin
  result := TJWK;
end;

procedure TJWKList.Setkey(index: integer; const Value: TJWK);
begin
  ObjectByIndex[index] := value;
end;

procedure TJWKList.readFromJson(obj : TJsonObject);
var
  arr : TJsonArray;
  i : integer;
begin
  clear;

  if obj.has('kty') then
  begin
    Add(TJWK.create(obj))
  end
  else if obj.has('keys') then
  begin
    arr := obj.Arr['keys'];
    for i := 0 to arr.Count  - 1 do
      Add(TJWK.create(arr.Obj[i].Link));
  end
end;

procedure TJWKList.writeToJson(obj: TJsonObject);
var
  arr : TJsonArray;
  i : integer;
begin
  arr := obj.forceArr['keys'];
  arr.clear;
  for i := 0 to count - 1 do
    arr.add(Key[i].obj.Link);
end;

{ TJWTUtils }

class function TJWTUtils.pack(jwt: TJWT; method: TJWTAlgorithm; key: TJWK): String;
var
  input, sig : TBytes;
begin
  jwt.header['typ'] := 'JWT';
  case method of
    jwt_none : jwt.header['alg'] := 'none';
    jwt_hmac_sha256 : jwt.header['alg'] := 'HS256';
    jwt_hmac_rsa256 : jwt.header['alg'] := 'RS256';
  else
    raise ELibraryException.create('Unsupported Message Encryption Format');
  end;
  if (key <> nil) and (method <> jwt_none) and (key.id <> '') then
    jwt.header['kid'] := key.id;

  input := JWTBase64URL(TJSONWriter.writeObject(jwt.header));
  input := BytesAdd(input, Byte('.'));
  input := BytesAdd(input, JWTBase64URL(TJSONWriter.writeObject(jwt.payload)));
  case method of
    jwt_none: SetLength(sig, 0);
    jwt_hmac_sha256: sig := Sign_Hmac_SHA256(input, key);
    jwt_hmac_rsa256: sig := Sign_Hmac_RSA256(input, key);
  end;
  result := BytesAsString(input)+'.'+BytesAsString(JWTBase64URL(sig));
end;

class function TJWTUtils.pack(jwt: TJWT; method: TJWTAlgorithm; key: TJWK; pemfile, pempassword : String): String;
var
  input, sig : TBytes;
begin
  jwt.header['typ'] := 'JWT';
  case method of
    jwt_none : jwt.header['alg'] := 'none';
    jwt_hmac_sha256 : jwt.header['alg'] := 'HS256';
    jwt_hmac_rsa256 : jwt.header['alg'] := 'RS256';
  else
    raise ELibraryException.create('Unsupported Message Encryption Format');
  end;
  if (key <> nil) and (method <> jwt_none) and (key.id <> '') then
    jwt.header['kid'] := key.id;

  input := JWTBase64URL(TJSONWriter.writeObject(jwt.header));
  input := BytesAdd(input, Byte('.'));
  input := BytesAdd(input, JWTBase64URL(TJSONWriter.writeObject(jwt.payload)));
  case method of
    jwt_none: SetLength(sig, 0);
    jwt_hmac_sha256: sig := Sign_Hmac_SHA256(input, key);
    jwt_hmac_rsa256: sig := Sign_Hmac_RSA256(input, pemfile, pempassword);
  end;
  result := BytesAsString(input)+'.'+BytesAsString(JWTBase64URL(sig));
end;


class function TJWTUtils.loadKeyFromRSACert(filename: AnsiString): TJWK;
var
  key : PRSA;
begin
  key := PRSA(LoadRSAPublicKey(filename));
  try
    result := TJWK.create(key, false);
  finally
    RSA_free(key);
  end;
end;

class function TJWTUtils.loadKeyFromDSACert(filename, password: AnsiString): TJWK;
var
  key : PDSA;
begin
  key := PDSA(LoadDSAPublicKey(filename, password));
  try
    result := TJWK.create(key, true);
  finally
    DSA_free(key);
  end;
end;

class function TJWTUtils.loadKeyFromRSACert(content: TBytes): TJWK;
var
  key : PRSA;
begin
  key := PRSA(LoadRSAPublicKey(content));
  try
    result := TJWK.create(key, false);
  finally
    RSA_free(key);
  end;
end;

//class function TJWTUtils.loadRSAPrivateKey(pemfile, pempassword: AnsiString): PRSA;
//var
//  bp: pBIO;
//  fn, pp: PAnsiChar;
//  pk: PRSA;
//begin
//  fn := PAnsiChar(pemfile);
//  pp := PAnsiChar(pempassword);
//  bp := BIO_new(BIO_s_file());
//  BIO_read_filename(bp, fn);
//  pk := nil;
//  result := PEM_read_bio_RSAPrivateKey(bp, @pk, nil, pp);
//  if result = nil then
//    raise ELibraryException.create('Private key failure.' + GetSSLErrorMessage);
//end;

class function TJWTUtils.loadRSAPublicKey(contents: TBytes): PRSA;
var
  fn : String;
begin
  fn := Path([SystemTemp, TFslDateTime.makeUTC.toString('yyyymmddhhnnss.zzz')+'.cer']);
  BytesToFile(contents, fn);
  try
    result := loadRSAPublicKey(AnsiString(fn));
  finally
    DeleteFile(fn)
  end;
end;

//class function TJWTUtils.loadDSAPrivateKey(pemfile, pempassword: AnsiString): PDSA;
//var
//  bp: pBIO;
//  fn, pp: PAnsiChar;
//  pk: PDSA;
//begin
//  fn := PAnsiChar(pemfile);
//  pp := PAnsiChar(pempassword);
//  bp := BIO_new(BIO_s_file());
//  BIO_read_filename(bp, fn);
//  pk := nil;
//  result := PEM_read_bio_DSAPrivateKey(bp, @pk, nil, pp);
//  if result = nil then
//    raise ELibraryException.create('Private key failure.' + GetSSLErrorMessage);
//end;
//
class function TJWTUtils.loadRSAPublicKey(pemfile: AnsiString) : PRSA;
var
  bp: pBIO;
  fn: PAnsiChar;
  xk : PX509;
  pk : PEVP_PKEY;
begin
  fn := PAnsiChar(pemfile);
  bp := BIO_new(BIO_s_file());
  BIO_read_filename(bp, fn);
  xk := nil;
  xk := PEM_read_bio_X509(bp, @xk, nil, nil);
  if xk = nil then
    raise ELibraryException.create('Public key failure.' + GetSSLErrorMessage);
  try
    pk := X509_get_pubkey(xk);
    try
      result := EVP_PKEY_get1_RSA(pk);
    finally
      EVP_PKEY_free(pk);
    end;
  finally
    X509_free(xk);
  end;
end;

class function TJWTUtils.loadDSAPublicKey(pemfile, pempassword: AnsiString) : PDSA;
var
  bp: pBIO;
  fn, pp: PAnsiChar;
  pk: PDSA;
begin
  fn := PAnsiChar(pemfile);
  pp := PAnsiChar(pempassword);
  bp := BIO_new(BIO_s_file());
  BIO_read_filename(bp, fn);
  pk := nil;
  result := PEM_read_bio_DSAPrivateKey(bp, @pk, nil, pp);
  if result = nil then
    raise ELibraryException.create('Private key failure.' + GetSSLErrorMessage);
end;

class function TJWTUtils.pack(header, payload: String; method: TJWTAlgorithm; key : TJWK): String;
var
  input, sig : TBytes;
begin
  case method of
    jwt_hmac_sha256 :
      begin
      check(key <> nil, 'A Key must be provided for HMAC/SHA-256');
      check(key.keyType = 'oct', 'A Symmetric Key must be provided for HMAC/SHA-256');
      end;
    jwt_hmac_rsa256 :
      begin
      check(key <> nil, 'A Key must be provided for HMAC/SHA-256');
      check(key.keyType = 'RSA', 'An RSA Key must be provided for HMAC/SHA-256');
      end;
  else
    raise ELibraryException.create('Unsupported Message Encryption Format');
  end;

  input := BytesAdd(JWTBase64URL(header),  Byte(Ord('.')));
  input := BytesAdd(input, JWTBase64URL(payload));
  case method of
    jwt_hmac_sha256: sig := Sign_Hmac_SHA256(input, key);
    jwt_hmac_rsa256: sig := Sign_Hmac_RSA256(input, key);
  end;
  result := BytesAsString(input)+'.'+BytesAsString(JWTBase64URL(sig));
end;

class function TJWTUtils.rsa_pack(jwt: TJWT; method: TJWTAlgorithm; pem_file, pem_password: String): String;
var
  input, sig : TBytes;
begin
  jwt.header['typ'] := 'JWT';
  case method of
    jwt_hmac_rsa256 : jwt.header['alg'] := 'RS256';
  else
    raise ELibraryException.create('Unsupported Message Encryption Format for PEM based signature');
  end;

  input := JWTBase64URL(TJSONWriter.writeObject(jwt.header));
  input := BytesAdd(input, Byte('.'));
  input := BytesAdd(input, JWTBase64URL(TJSONWriter.writeObject(jwt.payload)));
  sig := Sign_Hmac_RSA256(input, pem_file, pem_password);
  result := BytesAsString(input)+'.'+BytesAsString(JWTBase64URL(sig));
end;

class function TJWTUtils.unpack(token: string; verify: boolean; keys: TJWKList): TJWT;
var
  header, payload, sig : String;
  hb, pb : TBytes;
  h, p : TJsonObject;
begin
  StringSplit(token, '.', header, payload);
  StringSplit(payload, '.', payload, sig);
  check(header <> '', 'Header not found reading JWT');
  check(payload <> '', 'payload not found reading JWT');

  hb := JWTDeBase64URL(header);
  pb := JWTDeBase64URL(payload);

  h := TJSONParser.Parse(hb);
  try
    p := TJSONParser.Parse(pb);
    try
      if verify then
      begin
        if (h['alg'] = 'HS256') then
        begin
          check(keys.count = 1, 'There can only be a single key for HMA/SHA-256');
          verify_hmac_SHA256(AnsiStringAsBytes(AnsiString(header+'.'+payload)), JWTDeBase64URL(sig), keys[0])
        end
        else if (h['alg'] = 'RS256') then
          verify_hmac_RSA256(AnsiStringAsBytes(AnsiString(header+'.'+payload)), JWTDeBase64URL(sig), h, keys)
        else if (h['alg'] = 'none') then
          check(sig = '', 'There cannot be a sig when there is no algorithm')
        else
          raise ELibraryException.create('Unknown Algorithm '+h['alg']);
      end;
      result := TJWT.create(h.Link, p.Link);
      result.originalSource := token;
    finally
      p.free;
    end;
  finally
    h.Free;
  end;
end;

class function TJWTUtils.Sign_Hmac_RSA256(input: TBytes; pemfile, pempassword: String): TBytes;
begin
  result := nil;
end;

class function TJWTUtils.Sign_Hmac_SHA256(input: TBytes; key: TJWK): TBytes;
begin
  check(key <> nil, 'A Key must be provided for HMAC/SHA-256');
  check(key.keyType = 'oct', 'A Symmetric Key must be provided for HMAC/SHA-256');
  check(key.hasKey, 'A Symmetric Key Value must be provided for HMAC/SHA-256');
  check(Length(key.Key) > 0, 'A valid Symmetric Key Value must be provided for HMAC/SHA-256');
  result := THMACUtils.HMAC(TIdHMACSHA256, Key.key, input);

  Verify_Hmac_SHA256(input, result, key);
end;

class procedure TJWTUtils.Verify_Hmac_SHA256(input, sig: TBytes; key: TJWK);
var
  expected : TBytes;
begin
  check(key <> nil, 'A Key must be provided for HMAC/SHA-256');
  check(key.keyType = 'oct', 'A Symmetric Key must be provided for HMAC/SHA-256');
  check(key.hasKey, 'A Symmetric Key Value must be provided for HMAC/SHA-256');
  check(Length(key.Key) > 0, 'A valid Symmetric Key Value must be provided for HMAC/SHA-256');

  expected := THMACUtils.HMAC(TIdHMACSHA256, key.key, input);
  check(SameBytes(expected, sig),'Signature is not valid (HMAC/SHA-256)');
end;

class function TJWTUtils.Sign_Hmac_RSA256(input: TBytes; key: TJWK): TBytes;
var
  ctx : PEVP_MD_CTX;
  keysize : integer;
  len : Cardinal;
  pkey: PEVP_PKEY;
  rkey: PRSA;
  keys : TJWKList;
begin
  check(key <> nil, 'A key must be provided for RSA/SHA-256');

  // 1. Load the RSA private Key from FKey
  rkey := key.Load(true);
  try
    pkey := EVP_PKEY_new;
    try
      check(EVP_PKEY_set1_RSA(pkey, rkey) = 1, 'openSSL EVP_PKEY_set1_RSA failed');

      // 2. do the signing
      keysize := EVP_PKEY_size(pkey);
      SetLength(result, keysize);
      ctx := EVP_MD_CTX_new;
      try
        check(EVP_DigestSignInit(ctx, nil, EVP_sha256, nil, pKey) = 1, 'openSSL EVP_DigestInit_ex failed');
        check(EVP_DigestUpdate(ctx, @input[0], Length(input)) = 1, 'openSSL EVP_SignUpdate failed');
        check(EVP_DigestSignFinal(ctx, @result[0], @len) = 1, 'openSSL EVP_SignFinal failed');
        SetLength(result, len);
      finally
        EVP_MD_CTX_free(ctx);
      end;
    finally
      EVP_PKEY_free(pKey);
    end;
  finally
    RSA_free(rkey);
  end;

  keys := TJWKList.create;
  try
    keys.Add(key.Link);
    Verify_Hmac_RSA256(input, result, nil, keys);
  finally
    keys.Free;
  end;
end;

class procedure TJWTUtils.Verify_Hmac_RSA256(input, sig: TBytes; header : TJsonObject; keys: TJWKList);
var
  ctx : PEVP_MD_CTX;
  e: integer;
  pkey: PEVP_PKEY;
  rkey: PRSA;
  key : TJWK;
  i : integer;
begin
  check((keys <> nil) and (keys.Count > 0), 'No keys provided for RSA/SHA-256 verification');

  key := nil;
  if (header <> nil) and (header['kid'] <> '') then
  begin
    for i := 0 to keys.count - 1 do
      if keys[i].id = header['kid'] then
        key := keys[i];
    check(key <> nil, 'No matching key found for key '+header['kid']);
  end
  else
  begin
    check(keys.count = 1, 'No Key Id specified in JWT, and multiple possible keys specified');
    key := keys[0];
  end;

  // 1. Load the RSA private Key from FKey
  rkey := key.Load(false);
  try
    pkey := EVP_PKEY_new;
    try
      check(EVP_PKEY_set1_RSA(pkey, rkey) = 1, 'openSSL EVP_PKEY_set1_RSA failed');

      // 2. check the signature
      ctx := EVP_MD_CTX_new;
      try
        check(EVP_DigestVerifyInit(ctx, nil, EVP_sha256, nil, pkey) = 1, 'openSSL EVP_DigestVerifyInit failed');
        check(EVP_DigestUpdate(ctx, @input[0], Length(input)) = 1, 'openSSL EVP_DigestUpdate failed');
        e := EVP_DigestVerifyFinal(ctx, @sig[0], length(sig));
        check(e = 1, 'Signature is not valid (RSA) (e = '+inttostr(e)+')');
      finally
        EVP_MD_CTX_free(ctx);
      end;

    finally
      EVP_PKEY_free(pKey);
    end;
  finally
    RSA_free(rkey);
  end;
end;

function StripLeadingZeros(bytes : AnsiString) : AnsiString;
var
  i : integer;
begin
  i := 1;
  while (i < length(bytes)) and (bytes[i] = #0) do
    inc(i);
  result := copy(bytes, i, length(bytes));
end;

function BytesPairToAsn1(bytes : TBytes) : TBytes;
var
  r, s : AnsiString;
begin
  r := StripLeadingZeros(copy(BytesAsAnsiString(bytes), 1, length(bytes) div 2));
  s := StripLeadingZeros(copy(BytesAsAnsiString(bytes), length(bytes) div 2+1, length(bytes) div 2));
  if (r[1] >= #$80) then
    Insert(#0, r, 1);
  if (s[1] >= #$80) then
    Insert(#0, s, 1);
  result := AnsiStringAsBytes(ansichar($30)+ansichar(4+length(r)+length(s))+ansichar(02)+ansichar(length(r))+r+ansichar(02)+ansichar(length(s))+s);
end;

function asn1SigToBytePair(asn1 : TBytes) : TBytes;
var
  sv, r, s : AnsiString;
  l : integer;
begin
  sv := BytesAsAnsiString(asn1);
  if sv[1] <> #$30 then
    raise ELibraryException.create('Error 1 reading asn1 DER signature');
  if ord(sv[2]) <> length(sv)-2 then
    raise ELibraryException.create('Error 2 reading asn1 DER signature');
  delete(sv, 1, 2);
  if sv[1] <> #$02 then
    raise ELibraryException.create('Error 3 reading asn1 DER signature');
  r := copy(sv, 3, ord(sv[2]));
  delete(sv, 1, length(r)+2);
  if sv[1] <> #$02 then
    raise ELibraryException.create('Error 4 reading asn1 DER signature');
  s := copy(sv, 3, ord(sv[2]));
  delete(sv, 1, length(s)+2);
  if length(sv) <> 0 then
    raise ELibraryException.create('Error 5 reading asn1 DER signature');

  if (r[2] >= #$80) and (r[1] = #0) then
    delete(r, 1, 1);
  if (s[2] >= #$80) and (s[1] = #0) then
    delete(s, 1, 1);

  if (length(r) <= 20) and (length(s) <= 20) then
    l := 20
  else
    l := 32;
  while length(r) < l do
    insert(#0, r, 1);
  while length(s) < l do
    insert(#0, s, 1);
  result := AnsiStringAsBytes(r+s);
end;


{ TDigitalSigner }

function TDigitalSigner.canoncalizationSet(uri : String) : TXmlCanonicalisationMethodSet;
begin
  if uri = 'http://www.w3.org/TR/2001/REC-xml-c14n+0315' then
    result := [xcmCanonicalise]
  else if uri = 'http://www.w3.org/TR/2001/REC-xml-c14n+0315#WithComments' then
    result := [xcmCanonicalise, xcmComments]
  else
    raise ELibraryException.create('Canonicalization Method '+uri+' is not supported');
end;

function TDigitalSigner.canonicaliseXml(method: TXmlCanonicalisationMethodSet; source: TBytes): TBytes;
var
  xb : TFslXmlBuilder;
  dom : TMXmlDocument;
begin
  dom := loadXml(source);
  try
    xb := TFslXmlBuilder.Create;
    try
      xb.Canonicalise := method;
      xb.Start;
      xb.WriteXml(dom.docElement);
      xb.Finish;
      result := TEncoding.UTF8.GetBytes(xb.Build);
    finally
      xb.Free;
    end;
  finally
    dom.Free;
  end;

end;

function TDigitalSigner.canonicaliseXml(method : TXmlCanonicalisationMethodSet; source: TBytes; var dom : TMXmlDocument): TBytes;
var
  xb : TFslXmlBuilder;
begin
  dom := loadXml(source);

  xb := TFslXmlBuilder.Create;
  try
    xb.Canonicalise := method;
    xb.Start;
    xb.WriteXml(dom.docElement);
    xb.Finish;
    result := TEncoding.UTF8.GetBytes(xb.Build);
  finally
    xb.Free;
  end;
end;

function TDigitalSigner.canonicaliseXml(method : TXmlCanonicalisationMethodSet; dom: TMXmlElement): TBytes;
var
  xb : TFslXmlBuilder;
begin
  xb := TFslXmlBuilder.Create;
  try
    xb.Canonicalise := method;
    xb.Start;
    xb.WriteXml(dom);
    xb.Finish;
    result := TEncoding.UTF8.GetBytes(xb.Build);
  finally
    xb.Free;
  end;
end;

//function TDigitalSigner.canonicaliseXml(method : TXmlCanonicalisationMethodSet; dom: TMXMLDocument): TBytes;
//var
//  xb : TFslXmlBuilder;
//begin
//  xb := TFslXmlBuilder.Create;
//  try
//    xb.Canonicalise := method;
//    xb.Start;
//    xb.WriteXml(dom);
//    xb.Finish;
//    result := TEncoding.UTF8.GetBytes(xb.Build);
//  finally
//    xb.Free;
//  end;
//end;
//
procedure TDigitalSigner.checkDigest(ref: TMXmlElement; doc: TMXMLDocument);
var
  bytes, digest : TBytes;
  transforms, transform : TMXmlElement;
  bEnv : boolean;
  i : integer;
begin
  //Obtain the data object to be digested. (For example, the signature application may dereference the URI and execute Transforms provided by the signer in the Reference element, or it may obtain the content through other means such as a local cache.)
  if ref.attribute['URI'] = '' then
    bytes := canonicaliseXml([xcmCanonicalise], doc.docElement)
  else
    bytes := resolveReference(ref.attribute['URI']);
  BytesToFile(bytes, 'c:\temp\cand.xml');

  // check the transforms
  bEnv := false;
  transforms := ref.elementNS(NS_DS, 'Transforms');
  if transforms <> nil then
    for i := 0 to transforms.Children.Count - 1 do
    begin
      transform := transforms.Children[i];
      if (transform.NodeType = ntElement) and (transform.Name = 'Transform') then
        if transform.attribute['Algorithm'] = 'http://www.w3.org/2000/09/xmldsig#enveloped-signature' then
          bEnv := true
        else
          raise ELibraryException.create('Transform '+transform.attribute['Algorithm']+' is not supported');
    end;
  if (doc <> nil) and not bEnv then
    raise ELibraryException.create('Reference Transform is not http://www.w3.org/2000/09/xmldsig#enveloped-signature');

  //Digest the resulting data object using the DigestMethod specified in its Reference specification.
  if ref.elementNS(NS_DS, 'DigestMethod').attribute['Algorithm'] = 'http://www.w3.org/2000/09/xmldsig#sha1' then
    bytes := digestSHA1(bytes)
  else if ref.elementNS(NS_DS, 'DigestMethod').attribute['Algorithm'] = 'http://www.w3.org/2001/04/xmldsig-more#rsa-sha256' then
    bytes := digestSHA256(bytes)
  else
    raise ELibraryException.create('Unknown Digest method '+ref.elementNS(NS_DS, 'DigestMethod').attribute['Algorithm']);
  digest := decodeBase64(ref.elementNS(NS_DS, 'DigestValue').Text);

  //Compare the generated digest value against DigestValue in the SignedInfo Reference; if there is any mismatch, validation fails.
  if not SameBytes(bytes, digest) then
    raise ELibraryException.create('Digest mismatch on reference '+ref.attribute['URI']);
end;


constructor TDigitalSigner.create;
begin
  inherited;
//  LoadEAYExtensions(true);
//  ERR_load_crypto_strings;
//  OpenSSL_add_all_algorithms;
end;

function TDigitalSigner.verifySignature(xml: TBytes): boolean;
var
  doc : TMXmlDocument;
  sig, si : TMXmlElement;
  can, v : TBytes;
  key : TKeyInfo;
  i : integer;
begin
  doc := loadXml(xml);
  try
    if (doc.Name = 'Signature') and (doc.NamespaceURI = NS_DS) then
      sig := doc.docElement
    else
      sig := doc.docElement.elementNS(NS_DS, 'Signature');
    if (sig = nil) then
      raise ELibraryException.create('Signature not found');
    si := sig.elementNS(NS_DS, 'SignedInfo');
    if (si = nil) then
      raise ELibraryException.create('SignedInfo not found');
    if (sig <> doc) then
      doc.docElement.Children.Remove(sig)
    else
      doc := nil;

    //k. now we follow the method:
    // 1. Canonicalize the SignedInfo element based on the CanonicalizationMethod in SignedInfo.
    si.Name := '';
    si.LocalName := 'SignedInfo';
    si.NamespaceURI := NS_DS;
    can := canonicaliseXml(canoncalizationSet(si.elementNS(NS_DS, 'CanonicalizationMethod').attribute['Algorithm']), si);
    BytesToFile(can, 'c:\temp\can.xml');

    // 2. For each Reference in SignedInfo:
    for i := 0 to si.children.Count - 1 do
      if (si.children[i].NodeType = ntElement) and (si.children[i].Name = 'Reference') then
        checkDigest(si.children[i], doc);

    // 3. Obtain the keying information from KeyInfo or from an external source.
    key := LoadKeyInfo(sig);
    try
      // 4. Obtain the canonical form of the SignatureMethod using the CanonicalizationMethod and use the result (and previously obtained KeyInfo) to confirm the SignatureValue over the SignedInfo element.
      v := decodeBase64(sig.element('SignatureValue').text);
      result := key.checkSignature(can, v, signatureMethod(si.elementNS(NS_DS, 'SignatureMethod').attribute['Algorithm']));
    finally
      key.Free;
    end;
  finally
    doc.free;
  end;
end;

function TKeyInfo.checkSignature(digest, signature: TBytes; method: TSignatureMethod) : boolean;
begin
  if method in [sdXmlDSASha1, sdXmlDSASha256] then
    result := checkSignatureDSA(digest, signature, method)
  else
    result := checkSignatureRSA(digest, signature, method);
end;

function TKeyInfo.checkSignatureRSA(digest, signature: TBytes; method: TSignatureMethod) : boolean;
//var
//  ctx : EVP_MD_CTX;
//  e: integer;
//  pkey: PEVP_PKEY;
begin
//  pkey := EVP_PKEY_new;
//  try
//    check(EVP_PKEY_set1_RSA(pkey, rsa) = 1, 'openSSL EVP_PKEY_set1_RSA failed');
//
//    // 2. do the signing
//    EVP_MD_CTX_init(@ctx);
//    try
//      if method = sdXmlRSASha1 then
//        EVP_VerifyInit(@ctx, EVP_sha1)
//      else
//        EVP_VerifyInit(@ctx, EVP_sha256);
//      check(EVP_VerifyUpdate(@ctx, @digest[0], Length(digest)) = 1, 'openSSL EVP_VerifyUpdate failed');
//      e := EVP_VerifyFinal(@ctx, @signature[0], length(signature), pKey);
//      result := e = 1;
//    finally
//      EVP_MD_CTX_cleanup(@ctx);
//    end;
//  finally
//    EVP_PKEY_free(pKey);
//  end;
  result := false;
end;

function TKeyInfo.checkSignatureDSA(digest, signature: TBytes; method: TSignatureMethod) : Boolean;
//var
//  ctx : EVP_MD_CTX;
//  e: integer;
//  pkey: PEVP_PKEY;
//  err : Array [0..250] of ansichar;
//  m : String;
//  asn1 : TBytes;
begin
//  pkey := EVP_PKEY_new;
//  try
//    check(EVP_PKEY_set1_DSA(pkey, dsa) = 1, 'openSSL EVP_PKEY_set1_RSA failed');
//
//    // 2. do the signing
//    EVP_MD_CTX_init(@ctx);
//    try
//      if method = sdXmlDSASha1 then
//        EVP_VerifyInit(@ctx, EVP_sha1)
//      else
//        EVP_VerifyInit(@ctx, EVP_sha256);
//      check(EVP_VerifyUpdate(@ctx, @digest[0], Length(digest)) = 1, 'openSSL EVP_VerifyUpdate failed');
//      asn1 := BytesPairToAsn1(signature);
//      e := EVP_VerifyFinal(@ctx, @asn1[0], length(asn1), pKey);
//      if (e = -1) then
//      begin
//        m := '';
//        e := ERR_get_error;
//        repeat
//          ERR_error_string(e, @err);
//          m := m + inttohex(e, 8)+' ('+String(err)+')'+#13#10;
//          e := ERR_get_error;
//        until e = 0;
//        raise ELibraryException.create('OpenSSL Error verifying signature: '+#13#10+m);
//      end
//      else
//        result := e = 1;
//    finally
//      EVP_MD_CTX_cleanup(@ctx);
//    end;
//  finally
//    EVP_PKEY_free(pKey);
//  end;
  result := false;
end;


destructor TKeyInfo.Destroy;
begin
//  if dsa <> nil then
//    DSA_Free(dsa);
//  if rsa <> nil then
//    RSA_free(rsa);
  inherited;
end;

function TDigitalSigner.digest(source: TBytes; method: TSignatureMethod): TBytes;
begin
  if method in [sdXmlDSASha256, sdXmlRSASha256] then
    result := digestSHA256(source)
  else
    result := digestSHA1(source);
end;

function TDigitalSigner.digestSHA1(source: TBytes): TBytes;
var
  hash : TIdHashSHA1;
begin
  hash := TIdHashSHA1.Create;
  try
    result := idb(hash.HashBytes(idb(source)));
  finally
    hash.Free;
  end;
end;

function TDigitalSigner.digestSHA256(source: TBytes): TBytes;
var
  hash : TIdHashSHA256;
  b : TIdBytes;
begin
  hash := TIdHashSHA256.Create;
  try
    b := idb(source);
    b := hash.HashBytes(b);
    result := idb(b);
  finally
    hash.Free;
  end;
end;


//function TDigitalSigner.loadRSAKey: PRSA;
//var
//  bp: pBIO;
//  fn, pp: PAnsiChar;
//  pk: PRSA;
//begin
//  fn := PAnsiChar(FPrivateKey);
//  pp := PAnsiChar(FKeyPassword);
//  bp := BIO_new(BIO_s_file());
//  BIO_read_filename(bp, fn);
//  pk := nil;
//  result := PEM_read_bio_RSAPrivateKey(bp, @pk, nil, pp);
//  if result = nil then
//    raise ELibraryException.create('Private key failure.' + GetSSLErrorMessage);
//end;
//
//
//function TDigitalSigner.loadDSAKey: PDSA;
//var
//  bp: pBIO;
//  fn, pp: PAnsiChar;
//  pk: PDSA;
//begin
//  fn := PAnsiChar(FPrivateKey);
//  pp := PAnsiChar(FKeyPassword);
//  bp := BIO_new(BIO_s_file());
//  BIO_read_filename(bp, fn);
//  pk := nil;
//  result := PEM_read_bio_DSAPrivateKey(bp, @pk, nil, pp);
//  if result = nil then
//    raise ELibraryException.create('Private key failure.' + GetSSLErrorMessage);
//end;
//

function TDigitalSigner.sign(src : TBytes; method: TSignatureMethod) : TBytes;
begin
  if method in [sdXmlDSASha1, sdXmlDSASha256] then
    result := signDSA(src, method)
  else
    result := signRSA(src, method);
end;


function TDigitalSigner.signDSA(src : TBytes; method: TSignatureMethod) : TBytes;
//var
//  pkey: PEVP_PKEY;
//  dkey: PDSA;
//  ctx : EVP_MD_CTX;
//  len : integer;
//  asn1 : TBytes;
begin
//  // 1. Load the RSA private Key from FKey
//  dkey := loadDSAKey;
//  try
//    pkey := EVP_PKEY_new;
//    try
//      check(EVP_PKEY_set1_DSA(pkey, dkey) = 1, 'openSSL EVP_PKEY_set1_DSA failed');
//
//      // 2. do the signing
//      SetLength(asn1, EVP_PKEY_size(pkey));
//      EVP_MD_CTX_init(@ctx);
//      try
//        if method = sdXmlDSASha256 then
//          EVP_SignInit(@ctx, EVP_sha256)
//        else
//          EVP_SignInit(@ctx, EVP_sha1);
//        check(EVP_SignUpdate(@ctx, @src[0], Length(src)) = 1, 'openSSL EVP_SignUpdate failed');
//        check(EVP_SignFinal(@ctx, @asn1[0], len, pKey) = 1, 'openSSL EVP_SignFinal failed');
//        SetLength(asn1, len);
//        result := asn1SigToBytePair(asn1);
//      finally
//        EVP_MD_CTX_cleanup(@ctx);
//      end;
//    finally
//      EVP_PKEY_free(pKey);
//    end;
//  finally
//    DSA_free(dkey);
//  end;
  result := nil;
end;


function TDigitalSigner.signRSA(src : TBytes; method: TSignatureMethod) : TBytes;
//var
//  pkey: PEVP_PKEY;
//  rkey: PRSA;
//  ctx : EVP_MD_CTX;
//  keysize : integer;
//  len : integer;
begin
//  // 1. Load the RSA private Key from FKey
//  rkey := loadRSAKey;
//  try
//    pkey := EVP_PKEY_new;
//    try
//      check(EVP_PKEY_set1_RSA(pkey, rkey) = 1, 'openSSL EVP_PKEY_set1_RSA failed');
//
//      // 2. do the signing
//      keysize := EVP_PKEY_size(pkey);
//      SetLength(result, keysize);
//      EVP_MD_CTX_init(@ctx);
//      try
//        if method = sdXmlRSASha256 then
//          EVP_SignInit(@ctx, EVP_sha256)
//        else
//          EVP_SignInit(@ctx, EVP_sha1);
//        check(EVP_SignUpdate(@ctx, @src[0], Length(src)) = 1, 'openSSL EVP_SignUpdate failed');
//        check(EVP_SignFinal(@ctx, @result[0], len, pKey) = 1, 'openSSL EVP_SignFinal failed');
//        SetLength(result, len);
//      finally
//        EVP_MD_CTX_cleanup(@ctx);
//      end;
//    finally
//      EVP_PKEY_free(pKey);
//    end;
//  finally
//    RSA_free(rkey);
//  end;
  result := nil;
end;

function TDigitalSigner.signAlgorithmForMethod(method : TSignatureMethod) : String;
begin
  case method of
    sdXmlDSASha1 : result := 'http://www.w3.org/2000/09/xmldsig#dsa-sha1';
    sdXmlRSASha1 : result := 'http://www.w3.org/2000/09/xmldsig#rsa-sha1';
    sdXmlDSASha256 : result := 'http://www.w3.org/2000/09/xmldsig#dsa-sha256';
    sdXmlRSASha256 : result := 'http://www.w3.org/2001/04/xmldsig-more#rsa-sha256';
  else
    raise ELibraryException.create('unknown method');
  end;
end;

function TDigitalSigner.signDetached(xml : TBytes; refURL : String; method : TSignatureMethod; canonicalization : string; keyinfo : boolean) : TBytes;
var
  can, dig :  TBytes;
  dom : TMXmlDocument;
  sig, si, ref, trns: TMXmlElement;
  s : String;
begin
  can := canonicaliseXml([xcmCanonicalise], xml);
  dom := TMXmlDocument.CreateNS(ntDocument, NS_DS, 'Signature');
  try
    sig := dom.docElement;
    sig.attribute['xmlns'] := NS_DS;
    si := sig.addElementNS(NS_DS, 'SignedInfo');
    if canonicalization <> '' then
      si.addElementNS(NS_DS, 'CanonicalizationMethod').attribute['Algorithm'] := canonicalization
    else
      si.addElementNS(NS_DS, 'CanonicalizationMethod').attribute['Algorithm'] := 'http://www.w3.org/TR/2001/REC-xml-c14n+0315';
    si.addElementNS(NS_DS, 'SignatureMethod').attribute['Algorithm'] := signAlgorithmForMethod(method);
    ref := si.addElementNS(NS_DS, 'Reference');
    if (refUrl <> '') then
      ref.attribute['URI'] := refUrl;
    trns := ref.addElementNS(NS_DS, 'Transforms');
    trns.addElementNS(NS_DS, 'Transform').attribute['Algorithm'] := 'http://www.w3.org/2000/09/xmldsig#enveloped-signature';
    ref.addElementNS(NS_DS, 'DigestMethod').attribute['Algorithm'] := digestAlgorithmForMethod(method);
    dig := digest(can, method); // the method doesn't actually apply to this, but we figure that if the method specifies sha256, then it should be used here
    ref.addElementNS(NS_DS, 'DigestValue').addText(String(EncodeBase64(dig)));
    can := canonicaliseXml([xcmCanonicalise],si);
    dig := sign(can, method);
    s := string(EncodeBase64(dig));
    sig.addElementNS(NS_DS, 'SignatureValue').addText(s);

    if keyinfo then
      AddKeyInfo(sig, method);
    s := dom.ToXml(false, true);
    result := TEncoding.UTF8.GetBytes(s);
  finally
    dom.Free;
  end;
end;

function TDigitalSigner.digestAlgorithmForMethod(method : TSignatureMethod) : String;
begin
  case method of
    sdXmlDSASha256 : result := 'http://www.w3.org/2000/09/xmldsig#sha256';
    sdXmlRSASha256 : result := 'http://www.w3.org/2001/04/xmldsig-more#rsa-sha256';
  else
    result := 'http://www.w3.org/2000/09/xmldsig#sha1';
  end;
end;


function TDigitalSigner.signEnveloped(xml: TBytes; method : TSignatureMethod; keyinfo : boolean): TBytes;
begin
  {$IFDEF FPC}
  raise Exception.create('not done yet');
  {$ELSE}
  result := signEnveloped(xml, function (doc : TMXmlDocument) : TMXmlElement begin result := doc.docElement end, method, keyInfo);
  {$ENDIF}
end;

{$IFNDEF FPC}
function TDigitalSigner.signEnveloped(xml: TBytes; finder : TSignatureLocationFinderMethod; method : TSignatureMethod; keyinfo : boolean): TBytes;
var
  can, dig :  TBytes;
  dom : TMXmlDocument;
  sig, si, ref, trns: TMXmlElement;
  s : String;
begin
  can := canonicaliseXml([xcmCanonicalise], xml, dom);
  try
    sig := finder(dom).addElementNS(NS_DS, 'Signature');
    sig.attribute['xmlns'] := NS_DS;
    si := sig.addElementNS(NS_DS, 'SignedInfo');
    si.addElementNS(NS_DS, 'CanonicalizationMethod').attribute['Algorithm'] := 'http://www.w3.org/TR/2001/REC-xml-c14n+0315';
    si.addElementNS(NS_DS, 'SignatureMethod').attribute['Algorithm'] := signAlgorithmForMethod(method);
    ref := si.addElementNS(NS_DS, 'Reference');
    ref.attribute['URI'] := '';
    trns := ref.addElementNS(NS_DS, 'Transforms');
    trns.addElementNS(NS_DS, 'Transform').attribute['Algorithm'] := 'http://www.w3.org/2000/09/xmldsig#enveloped-signature';
    ref.addElementNS(NS_DS, 'DigestMethod').attribute['Algorithm'] := digestAlgorithmForMethod(method);
    dig := digest(can, method); // the method doesn't actually apply to this, but we figure that if the method specifies sha256, then it should be used here
    ref.addElementNS(NS_DS, 'DigestValue').addText(String(EncodeBase64(dig)));
    can := canonicaliseXml([xcmCanonicalise],si);
    dig := sign(can, method);
    s := string(EncodeBase64(dig));
    sig.addElementNS(NS_DS, 'SignatureValue').addText(s);

    if keyinfo then
      AddKeyInfo(sig, method);
    s := dom.ToXml(false, true);
    result := TEncoding.UTF8.GetBytes(s);
  finally
    dom.Free;
  end;
end;
{$ENDIF}

//function bn2Base64(p : PBigNum) : String;
//var
//  b : TBytes;
//begin
//  setlength(b,  BN_num_bytes(p));
//  BN_bn2bin(p, @b[0]);
//  result := String(EncodeBase64(b));
//end;

procedure TDigitalSigner.AddKeyInfo(sig : TMXmlElement; method : TSignatureMethod);
//var
//  kv: TMXmlElement;
//  dkey : PDSA;
//  rkey : PRSA;
begin
//  if method in [sdXmlDSASha1, sdXmlDSASha256] then
//  begin
//    kv := sig.AddElement('KeyInfo').AddElement('KeyValue').AddElement('DSAKeyValue');
//    dkey := LoadDSAKey;
//    try
//      kv.AddElement('P').AddText(bn2Base64(dkey.p));
//      kv.AddElement('Q').AddText(bn2Base64(dkey.q));
//      kv.AddElement('G').AddText(bn2Base64(dkey.g));
//      kv.AddElement('Y').AddText(bn2Base64(dkey.pub_key));
//    finally
//      DSA_free(dKey);
//    end;
//  end
//  else
//  begin
//    kv := sig.AddElement('KeyInfo').AddElement('KeyValue').AddElement('RSAKeyValue');
//    rkey := loadRSAKey;
//    try
//      kv.AddElement('Modulus').AddText(bn2Base64(rkey.n));
//      kv.AddElement('Exponent').AddText(bn2Base64(rkey.e));
//    finally
//      RSA_free(rkey);
//    end;
//  end;
end;

function TDigitalSigner.signExternal(references: TDigitalSignatureReferenceList; method : TSignatureMethod; keyinfo : boolean): TBytes;
//var
//  doc : TMXMLDocument;
//  sig, si, ref, trns : TMXmlElement;
//  i : integer;
//  reference : TDigitalSignatureReference;
//  s : AnsiString;
//  t : String;
//  can, dig : TBytes;
begin
//  doc := TMXMLDocument.Create();
//  sig := doc.addElementNS(NS_DS, 'Signature');
//  sig.attribute['xmlns'] := NS_DS;
//  si := sig.AddElement('SignedInfo');
//  si.AddElement('CanonicalizationMethod').attribute['Algorithm'] := 'http://www.w3.org/TR/2001/REC-xml-c14n+0315';
//  si.AddElement('SignatureMethod').attribute['Algorithm'] := signAlgorithmForMethod(method);
//  for i := 0 to references.Count - 1 do
//  begin
//    reference := references[i];
//    ref := si.AddElement('Reference');
//    ref.attribute['URI'] := reference.URL;
//    if reference.transforms.count > 0 then
//    begin
//      trns := ref.AddElement('Transforms');
//      for t in reference.transforms do
//        trns.AddElement('Transform').attribute['Algorithm'] := t;
//    end;
//    ref.AddElement('DigestMethod').attribute['Algorithm'] := digestAlgorithmForMethod(method);
//    dig := digest(reference.content, method);
//    ref.AddElement('DigestValue').Text := String(EncodeBase64(dig));
//  end;
//  can := canonicaliseXml([xcmCanonicalise],si);
//  dig := sign(can, method);
//  s := EncodeBase64(dig);
//  sig.AddElement('SignatureValue').Text := string(s);
//  if keyinfo then
//    AddKeyInfo(sig, method);
//  result := canonicaliseXml([xcmCanonicalise], sig);  // don't need to canonicalise the whole lot, but why not?
end;

function TDigitalSigner.LoadKeyInfo(sig: TMXmlElement): TKeyInfo;
//var
//  ki, kv, kd : TMXmlElement;
//  v : TBytes;
////  p : pansichar;
begin
//  result := TKeyInfo.Create;
//  try
//    ki := sig.elementNS(NS_DS, 'KeyInfo');
//    if ki = nil then
//      raise ELibraryException.create('No KeyInfo found in digital signature');
//    kv := ki.elementNS(NS_DS, 'KeyValue');
//    if kv = nil then
//      raise ELibraryException.create('No KeyValue found in digital signature');
//    kd := kv.elementNS(NS_DS, 'RSAKeyValue');
//    if kd <> nil then
//    begin
//      result.rsa := RSA_new;
//      v := DecodeBase64(kd.elementNS(NS_DS, 'Modulus').Text);
//      result.rsa.n := BN_bin2bn(@v[0], length(v), nil);
//      v := DecodeBase64(kd.elementNS(NS_DS, 'Exponent').Text);
//      result.rsa.e := BN_bin2bn(@v[0], length(v), nil);
//    end
//    else
//    begin
//      kd := kv.elementNS(NS_DS, 'DSAKeyValue');
//      if kd <> nil then
//      begin
//        result.dsa := DSA_new;
//        v := DecodeBase64(kd.elementNS(NS_DS, 'P').Text);
//        result.dsa.p := BN_bin2bn(@v[0], length(v), nil);
//        v := DecodeBase64(kd.elementNS(NS_DS, 'Q').Text);
//        result.dsa.q := BN_bin2bn(@v[0], length(v), nil);
//        v := DecodeBase64(kd.elementNS(NS_DS, 'G').Text);
//        result.dsa.g := BN_bin2bn(@v[0], length(v), nil);
//        v := DecodeBase64(kd.elementNS(NS_DS, 'Y').Text);
//        result.dsa.pub_key := BN_bin2bn(@v[0], length(v), nil);
//
////        if elementNS(kd, 'X', NS_DS) <> nil then
////        begin
////          v := DecodeBase64(elementNS(kd, 'X', NS_DS).Text);
////          result.dsa.priv_key := BN_bin2bn(@v[0], length(v), nil);
////        end;
//      end
//      else
//        raise ELibraryException.create('No Key Info found');
//    end;
//
//    result.Link;
//  finally
//    result.Free;
//  end;
  result := nil;
end;

function TDigitalSigner.loadXml(source: TBytes): TMXmlDocument;
begin
  result := TMXmlParser.parse(source, [xpResolveNamespaces]);
end;

function TDigitalSigner.resolveReference(url: string): TBytes;
var
  fetch : TInternetFetcher;
begin
  fetch := TInternetFetcher.Create;
  try
    fetch.URL := URL;
    fetch.Fetch;
    result := fetch.Buffer.AsBytes;
  finally
    fetch.Free;
  end;
end;

function TDigitalSigner.signatureMethod(uri: String): TSignatureMethod;
begin
  if uri = 'http://www.w3.org/2000/09/xmldsig#dsa-sha1' then
    result := sdXmlDSASha1
  else if uri = 'http://www.w3.org/2000/09/xmldsig#rsa-sha1' then
    result := sdXmlRSASha1
  else if uri = 'http://www.w3.org/2000/09/xmldsig#dsa-sha256' then
    result := sdXmlDSASha256
  else if uri = 'http://www.w3.org/2001/04/xmldsig-more#rsa-sha256' then
    result := sdXmlRSASha256
  else
    raise ELibraryException.create('Unsupported signature method '+uri);
end;

{ TDigitalSignatureReferenceList }

function TDigitalSignatureReferenceList.getReference(i: integer): TDigitalSignatureReference;
begin
  result := TDigitalSignatureReference(ObjectByIndex[i]);
end;

function TDigitalSignatureReferenceList.itemClass: TFslObjectClass;
begin
  result := TDigitalSignatureReference;
end;

{ TDigitalSignatureReference }

constructor TDigitalSignatureReference.Create;
begin
  inherited;
  FTransforms := TStringList.Create;
end;

destructor TDigitalSignatureReference.Destroy;
begin
  FTransforms.Free;
  inherited;
end;

function TDigitalSignatureReference.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FUrl.length * sizeof(char)) + 12);
  inc(result, FTransforms.sizeInBytes);
end;

end.


