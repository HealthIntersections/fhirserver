unit DigitalSignatures;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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


interface

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

uses
  SysUtils, Classes, {$IFNDEF VER260} System.NetEncoding, {$ENDIF}
  IdHashSHA, IdGlobal,
  BytesSupport, StringSupport, EncodeSupport, EncdDecd,
  AdvObjects, AdvObjectLists,
  MXml, XmlBuilder, AdvXmlBuilders,
  IdSSLOpenSSLHeaders, libeay32, HMAC, InternetFetcher;

Const
  NS_DS = 'http://www.w3.org/2000/09/xmldsig#';

Type
  TSignatureMethod = (sdXmlDSASha1, sdXmlRSASha1, sdXmlDSASha256, sdXmlRSASha256);

  TDigitalSignatureReference = class (TAdvObject)
  private
    FUrl: String;
    FTransforms: TStringList;
    FContent: TBytes;
  public
    constructor Create; override;
    destructor Destroy; override;

    property url: String read FUrl write FUrl;
    property transforms : TStringList read FTransforms;
    property content : TBytes read FContent write FContent;
  end;

  TDigitalSignatureReferenceList = class (TAdvObjectList)
  private
    function getReference(i: integer): TDigitalSignatureReference;

  protected
    function ItemClass : TAdvObjectClass; override;
  public
    property reference[i : integer] : TDigitalSignatureReference read getReference; default;
  end;

  TKeyInfo = class (TAdvObject)
  private
    dsa : PDSA;
    rsa : PRSA;
    function checkSignatureRSA(digest, signature : TBytes; method : TSignatureMethod) : boolean;
    function checkSignatureDSA(digest, signature : TBytes; method : TSignatureMethod) : boolean;
    function checkSignature(digest, signature : TBytes; method : TSignatureMethod) : boolean;
  public
    destructor Destroy; override;
  end;

  TSignatureLocationFinderMethod = reference to function (doc : TMXmlDocument) : TMXmlElement;

  TDigitalSigner = class (TAdvObject)
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
    function canonicaliseXml(method : TXmlCanonicalisationMethodSet; dom : TMXmlDocument) : TBytes; overload;
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
    function loadRSAKey: PRSA;
    function loadDSAKey: PDSA;
    procedure AddKeyInfo(sig: TMXmlElement; method : TSignatureMethod);

    // source content management
    function resolveReference(url : string) : TBytes;

  public
    Constructor Create; override;

    // certificate files, for signing
    Property PublicKey : AnsiString read FPublicKey write FPublicKey;
    Property PrivateKey : AnsiString read FPrivateKey write FPrivateKey;
    Property KeyPassword : AnsiString read FKeyPassword write FKeyPassword;

    // classic XML Enveloped Signature
    function signEnveloped(xml : TBytes; method : TSignatureMethod; keyinfo : boolean) : TBytes; overload;
    function signEnveloped(xml : TBytes; finder : TSignatureLocationFinderMethod; method : TSignatureMethod; keyinfo : boolean) : TBytes; overload;

    function signDetached(xml : TBytes; refUrl : String; method : TSignatureMethod; canonicalization : string; keyinfo : boolean) : TBytes; overload;

    function signExternal(references : TDigitalSignatureReferenceList; method : TSignatureMethod; keyinfo : boolean) : TBytes;
    function verifySignature(xml : TBytes) : boolean;
  end;


implementation

function base64(bytes : TBytes): String;
begin
  result := String(EncodeBase64(@bytes[0], length(bytes))).replace(#13#10, '');
end;

function unbase64(value : String): TBytes;
begin
  result := decodeBase64(AnsiString(value));
end;

procedure check(test: boolean; failmsg: string);
begin
  if not test then
    raise Exception.Create(failmsg);
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
    raise Exception.Create('Error 1 reading asn1 DER signature');
  if ord(sv[2]) <> length(sv)-2 then
    raise Exception.Create('Error 2 reading asn1 DER signature');
  delete(sv, 1, 2);
  if sv[1] <> #$02 then
    raise Exception.Create('Error 3 reading asn1 DER signature');
  r := copy(sv, 3, ord(sv[2]));
  delete(sv, 1, length(r)+2);
  if sv[1] <> #$02 then
    raise Exception.Create('Error 4 reading asn1 DER signature');
  s := copy(sv, 3, ord(sv[2]));
  delete(sv, 1, length(s)+2);
  if length(sv) <> 0 then
    raise Exception.Create('Error 5 reading asn1 DER signature');

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
  if uri = 'http://www.w3.org/TR/2001/REC-xml-c14n-20010315' then
    result := [xcmCanonicalise]
  else if uri = 'http://www.w3.org/TR/2001/REC-xml-c14n-20010315#WithComments' then
    result := [xcmCanonicalise, xcmComments]
  else
    raise Exception.Create('Canonicalization Method '+uri+' is not supported');
end;

function TDigitalSigner.canonicaliseXml(method: TXmlCanonicalisationMethodSet; source: TBytes): TBytes;
var
  xb : TAdvXmlBuilder;
  dom : TMXmlDocument;
begin
  dom := loadXml(source);
  try
    xb := TAdvXmlBuilder.Create;
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
  xb : TAdvXmlBuilder;
begin
  dom := loadXml(source);

  xb := TAdvXmlBuilder.Create;
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
  xb : TAdvXmlBuilder;
begin
  xb := TAdvXmlBuilder.Create;
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

function TDigitalSigner.canonicaliseXml(method : TXmlCanonicalisationMethodSet; dom: TMXMLDocument): TBytes;
var
  xb : TAdvXmlBuilder;
begin
  xb := TAdvXmlBuilder.Create;
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
          raise Exception.Create('Transform '+transform.attribute['Algorithm']+' is not supported');
    end;
  if (doc <> nil) and not bEnv then
    raise Exception.Create('Reference Transform is not http://www.w3.org/2000/09/xmldsig#enveloped-signature');

  //Digest the resulting data object using the DigestMethod specified in its Reference specification.
  if ref.elementNS(NS_DS, 'DigestMethod').attribute['Algorithm'] = 'http://www.w3.org/2000/09/xmldsig#sha1' then
    bytes := digestSHA1(bytes)
  else if ref.elementNS(NS_DS, 'DigestMethod').attribute['Algorithm'] = 'http://www.w3.org/2001/04/xmldsig-more#rsa-sha256' then
    bytes := digestSHA256(bytes)
  else
    raise Exception.Create('Unknown Digest method '+ref.elementNS(NS_DS, 'DigestMethod').attribute['Algorithm']);
  digest := unbase64(ref.elementNS(NS_DS, 'DigestValue').Text);

  //Compare the generated digest value against DigestValue in the SignedInfo Reference; if there is any mismatch, validation fails.
  if not SameBytes(bytes, digest) then
    raise Exception.Create('Digest mismatch on reference '+ref.attribute['URI']);
end;


constructor TDigitalSigner.create;
begin
  inherited;
  LoadEAYExtensions;
  ERR_load_crypto_strings;
  OpenSSL_add_all_algorithms;
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
      raise Exception.Create('Signature not found');
    si := sig.elementNS(NS_DS, 'SignedInfo');
    if (si = nil) then
      raise Exception.Create('SignedInfo not found');
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
      v := unbase64(sig.element('SignatureValue').text);
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
var
  ctx : EVP_MD_CTX;
  e: integer;
  pkey: PEVP_PKEY;
begin
  pkey := EVP_PKEY_new;
  try
    check(EVP_PKEY_set1_RSA(pkey, rsa) = 1, 'openSSL EVP_PKEY_set1_RSA failed');

    // 2. do the signing
    EVP_MD_CTX_init(@ctx);
    try
      if method = sdXmlRSASha1 then
        EVP_VerifyInit(@ctx, EVP_sha1)
      else
        EVP_VerifyInit(@ctx, EVP_sha256);
      check(EVP_VerifyUpdate(@ctx, @digest[0], Length(digest)) = 1, 'openSSL EVP_VerifyUpdate failed');
      e := EVP_VerifyFinal(@ctx, @signature[0], length(signature), pKey);
      result := e = 1;
    finally
      EVP_MD_CTX_cleanup(@ctx);
    end;
  finally
    EVP_PKEY_free(pKey);
  end;
end;

function TKeyInfo.checkSignatureDSA(digest, signature: TBytes; method: TSignatureMethod) : Boolean;
var
  ctx : EVP_MD_CTX;
  e: integer;
  pkey: PEVP_PKEY;
  err : Array [0..250] of ansichar;
  m : String;
  asn1 : TBytes;
begin
  pkey := EVP_PKEY_new;
  try
    check(EVP_PKEY_set1_DSA(pkey, dsa) = 1, 'openSSL EVP_PKEY_set1_RSA failed');

    // 2. do the signing
    EVP_MD_CTX_init(@ctx);
    try
      if method = sdXmlDSASha1 then
        EVP_VerifyInit(@ctx, EVP_sha1)
      else
        EVP_VerifyInit(@ctx, EVP_sha256);
      check(EVP_VerifyUpdate(@ctx, @digest[0], Length(digest)) = 1, 'openSSL EVP_VerifyUpdate failed');
      asn1 := BytesPairToAsn1(signature);
      e := EVP_VerifyFinal(@ctx, @asn1[0], length(asn1), pKey);
      if (e = -1) then
      begin
        m := '';
        e := ERR_get_error;
        repeat
          ERR_error_string(e, @err);
          m := m + inttohex(e, 8)+' ('+String(err)+')'+#13#10;
          e := ERR_get_error;
        until e = 0;
        raise Exception.Create('OpenSSL Error verifying signature: '+#13#10+m);
      end
      else
        result := e = 1;
    finally
      EVP_MD_CTX_cleanup(@ctx);
    end;
  finally
    EVP_PKEY_free(pKey);
  end;
end;


destructor TKeyInfo.Destroy;
begin
  if dsa <> nil then
    DSA_Free(dsa);
  if rsa <> nil then
    RSA_free(rsa);
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


function TDigitalSigner.loadRSAKey: PRSA;
var
  bp: pBIO;
  fn, pp: PAnsiChar;
  pk: PRSA;
begin
  fn := PAnsiChar(FPrivateKey);
  pp := PAnsiChar(FKeyPassword);
  bp := BIO_new(BIO_s_file());
  BIO_read_filename(bp, fn);
  pk := nil;
  result := PEM_read_bio_RSAPrivateKey(bp, @pk, nil, pp);
  if result = nil then
    raise Exception.Create('Private key failure.' + GetSSLErrorMessage);
end;


function TDigitalSigner.loadDSAKey: PDSA;
var
  bp: pBIO;
  fn, pp: PAnsiChar;
  pk: PDSA;
begin
  fn := PAnsiChar(FPrivateKey);
  pp := PAnsiChar(FKeyPassword);
  bp := BIO_new(BIO_s_file());
  BIO_read_filename(bp, fn);
  pk := nil;
  result := PEM_read_bio_DSAPrivateKey(bp, @pk, nil, pp);
  if result = nil then
    raise Exception.Create('Private key failure.' + GetSSLErrorMessage);
end;


function TDigitalSigner.sign(src : TBytes; method: TSignatureMethod) : TBytes;
begin
  if method in [sdXmlDSASha1, sdXmlDSASha256] then
    result := signDSA(src, method)
  else
    result := signRSA(src, method);
end;


function TDigitalSigner.signDSA(src : TBytes; method: TSignatureMethod) : TBytes;
var
  pkey: PEVP_PKEY;
  dkey: PDSA;
  ctx : EVP_MD_CTX;
  len : integer;
  asn1 : TBytes;
begin
  // 1. Load the RSA private Key from FKey
  dkey := loadDSAKey;
  try
    pkey := EVP_PKEY_new;
    try
      check(EVP_PKEY_set1_DSA(pkey, dkey) = 1, 'openSSL EVP_PKEY_set1_DSA failed');

      // 2. do the signing
      SetLength(asn1, EVP_PKEY_size(pkey));
      EVP_MD_CTX_init(@ctx);
      try
        if method = sdXmlDSASha256 then
          EVP_SignInit(@ctx, EVP_sha256)
        else
          EVP_SignInit(@ctx, EVP_sha1);
        check(EVP_SignUpdate(@ctx, @src[0], Length(src)) = 1, 'openSSL EVP_SignUpdate failed');
        check(EVP_SignFinal(@ctx, @asn1[0], len, pKey) = 1, 'openSSL EVP_SignFinal failed');
        SetLength(asn1, len);
        result := asn1SigToBytePair(asn1);
      finally
        EVP_MD_CTX_cleanup(@ctx);
      end;
    finally
      EVP_PKEY_free(pKey);
    end;
  finally
    DSA_free(dkey);
  end;
end;


function TDigitalSigner.signRSA(src : TBytes; method: TSignatureMethod) : TBytes;
var
  pkey: PEVP_PKEY;
  rkey: PRSA;
  ctx : EVP_MD_CTX;
  keysize : integer;
  len : integer;
begin
  // 1. Load the RSA private Key from FKey
  rkey := loadRSAKey;
  try
    pkey := EVP_PKEY_new;
    try
      check(EVP_PKEY_set1_RSA(pkey, rkey) = 1, 'openSSL EVP_PKEY_set1_RSA failed');

      // 2. do the signing
      keysize := EVP_PKEY_size(pkey);
      SetLength(result, keysize);
      EVP_MD_CTX_init(@ctx);
      try
        if method = sdXmlRSASha256 then
          EVP_SignInit(@ctx, EVP_sha256)
        else
          EVP_SignInit(@ctx, EVP_sha1);
        check(EVP_SignUpdate(@ctx, @src[0], Length(src)) = 1, 'openSSL EVP_SignUpdate failed');
        check(EVP_SignFinal(@ctx, @result[0], len, pKey) = 1, 'openSSL EVP_SignFinal failed');
        SetLength(result, len);
      finally
        EVP_MD_CTX_cleanup(@ctx);
      end;
    finally
      EVP_PKEY_free(pKey);
    end;
  finally
    RSA_free(rkey);
  end;
end;

function TDigitalSigner.signAlgorithmForMethod(method : TSignatureMethod) : String;
begin
  case method of
    sdXmlDSASha1 : result := 'http://www.w3.org/2000/09/xmldsig#dsa-sha1';
    sdXmlRSASha1 : result := 'http://www.w3.org/2000/09/xmldsig#rsa-sha1';
    sdXmlDSASha256 : result := 'http://www.w3.org/2000/09/xmldsig#dsa-sha256';
    sdXmlRSASha256 : result := 'http://www.w3.org/2001/04/xmldsig-more#rsa-sha256';
  else
    raise Exception.Create('unknown method');
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
      si.addElementNS(NS_DS, 'CanonicalizationMethod').attribute['Algorithm'] := 'http://www.w3.org/TR/2001/REC-xml-c14n-20010315';
    si.addElementNS(NS_DS, 'SignatureMethod').attribute['Algorithm'] := signAlgorithmForMethod(method);
    ref := si.addElementNS(NS_DS, 'Reference');
    if (refUrl <> '') then
      ref.attribute['URI'] := refUrl;
    trns := ref.addElementNS(NS_DS, 'Transforms');
    trns.addElementNS(NS_DS, 'Transform').attribute['Algorithm'] := 'http://www.w3.org/2000/09/xmldsig#enveloped-signature';
    ref.addElementNS(NS_DS, 'DigestMethod').attribute['Algorithm'] := digestAlgorithmForMethod(method);
    dig := digest(can, method); // the method doesn't actually apply to this, but we figure that if the method specifies sha256, then it should be used here
    ref.addElementNS(NS_DS, 'DigestValue').addText(String(EncodeBase64(@dig[0], length(dig))));
    can := canonicaliseXml([xcmCanonicalise],si);
    dig := sign(can, method);
    s := base64(dig);
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
  result := signEnveloped(xml, function (doc : TMXmlDocument) : TMXmlElement begin result := doc.docElement end, method, keyInfo);
end;

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
    si.addElementNS(NS_DS, 'CanonicalizationMethod').attribute['Algorithm'] := 'http://www.w3.org/TR/2001/REC-xml-c14n-20010315';
    si.addElementNS(NS_DS, 'SignatureMethod').attribute['Algorithm'] := signAlgorithmForMethod(method);
    ref := si.addElementNS(NS_DS, 'Reference');
    ref.attribute['URI'] := '';
    trns := ref.addElementNS(NS_DS, 'Transforms');
    trns.addElementNS(NS_DS, 'Transform').attribute['Algorithm'] := 'http://www.w3.org/2000/09/xmldsig#enveloped-signature';
    ref.addElementNS(NS_DS, 'DigestMethod').attribute['Algorithm'] := digestAlgorithmForMethod(method);
    dig := digest(can, method); // the method doesn't actually apply to this, but we figure that if the method specifies sha256, then it should be used here
    ref.addElementNS(NS_DS, 'DigestValue').addText(String(EncodeBase64(@dig[0], length(dig))));
    can := canonicaliseXml([xcmCanonicalise],si);
    dig := sign(can, method);
    s := base64(dig);
    sig.addElementNS(NS_DS, 'SignatureValue').addText(s);

    if keyinfo then
      AddKeyInfo(sig, method);
    s := dom.ToXml(false, true);
    result := TEncoding.UTF8.GetBytes(s);
  finally
    dom.Free;
  end;
end;

function bn2Base64(p : PBigNum) : String;
var
  b : TBytes;
begin
  setlength(b,  BN_num_bytes(p));
  BN_bn2bin(p, @b[0]);
  result := String(base64(b));
end;

procedure TDigitalSigner.AddKeyInfo(sig : TMXmlElement; method : TSignatureMethod);
var
  kv: TMXmlElement;
  dkey : PDSA;
  rkey : PRSA;
begin
  if method in [sdXmlDSASha1, sdXmlDSASha256] then
  begin
    kv := sig.AddElement('KeyInfo').AddElement('KeyValue').AddElement('DSAKeyValue');
    dkey := LoadDSAKey;
    try
      kv.AddElement('P').AddText(bn2Base64(dkey.p));
      kv.AddElement('Q').AddText(bn2Base64(dkey.q));
      kv.AddElement('G').AddText(bn2Base64(dkey.g));
      kv.AddElement('Y').AddText(bn2Base64(dkey.pub_key));
    finally
      DSA_free(dKey);
    end;
  end
  else
  begin
    kv := sig.AddElement('KeyInfo').AddElement('KeyValue').AddElement('RSAKeyValue');
    rkey := loadRSAKey;
    try
      kv.AddElement('Modulus').AddText(bn2Base64(rkey.n));
      kv.AddElement('Exponent').AddText(bn2Base64(rkey.e));
    finally
      RSA_free(rkey);
    end;
  end;
end;

function TDigitalSigner.signExternal(references: TDigitalSignatureReferenceList; method : TSignatureMethod; keyinfo : boolean): TBytes;
var
  doc : TMXMLDocument;
  sig, si, ref, trns : TMXmlElement;
  i : integer;
  reference : TDigitalSignatureReference;
  s, t : String;
  can, dig : TBytes;
begin
  doc := TMXMLDocument.Create();
  sig := doc.addElementNS(NS_DS, 'Signature');
  sig.attribute['xmlns'] := NS_DS;
  si := sig.AddElement('SignedInfo');
  si.AddElement('CanonicalizationMethod').attribute['Algorithm'] := 'http://www.w3.org/TR/2001/REC-xml-c14n-20010315';
  si.AddElement('SignatureMethod').attribute['Algorithm'] := signAlgorithmForMethod(method);
  for i := 0 to references.Count - 1 do
  begin
    reference := references[i];
    ref := si.AddElement('Reference');
    ref.attribute['URI'] := reference.URL;
    if reference.transforms.count > 0 then
    begin
      trns := ref.AddElement('Transforms');
      for t in reference.transforms do
        trns.AddElement('Transform').attribute['Algorithm'] := t;
    end;
    ref.AddElement('DigestMethod').attribute['Algorithm'] := digestAlgorithmForMethod(method);
    dig := digest(reference.content, method);
    ref.AddElement('DigestValue').Text := String(EncodeBase64(@dig[0], length(dig)));
  end;
  can := canonicaliseXml([xcmCanonicalise],si);
  dig := sign(can, method);
  s := base64(dig);
  sig.AddElement('SignatureValue').Text := s;
  if keyinfo then
    AddKeyInfo(sig, method);
  result := canonicaliseXml([xcmCanonicalise], sig);  // don't need to canonicalise the whole lot, but why not?
end;

function TDigitalSigner.LoadKeyInfo(sig: TMXmlElement): TKeyInfo;
var
  ki, kv, kd : TMXmlElement;
  v : TBytes;
//  p : pansichar;
begin
  result := TKeyInfo.Create;
  try
    ki := sig.elementNS(NS_DS, 'KeyInfo');
    if ki = nil then
      raise Exception.Create('No KeyInfo found in digital signature');
    kv := ki.elementNS(NS_DS, 'KeyValue');
    if kv = nil then
      raise Exception.Create('No KeyValue found in digital signature');
    kd := kv.elementNS(NS_DS, 'RSAKeyValue');
    if kd <> nil then
    begin
      result.rsa := RSA_new;
      v := unbase64(kd.elementNS(NS_DS, 'Modulus').Text);
      result.rsa.n := BN_bin2bn(@v[0], length(v), nil);
      v := unbase64(kd.elementNS(NS_DS, 'Exponent').Text);
      result.rsa.e := BN_bin2bn(@v[0], length(v), nil);
    end
    else
    begin
      kd := kv.elementNS(NS_DS, 'DSAKeyValue');
      if kd <> nil then
      begin
        result.dsa := DSA_new;
        v := unbase64(kd.elementNS(NS_DS, 'P').Text);
        result.dsa.p := BN_bin2bn(@v[0], length(v), nil);
        v := unbase64(kd.elementNS(NS_DS, 'Q').Text);
        result.dsa.q := BN_bin2bn(@v[0], length(v), nil);
        v := unbase64(kd.elementNS(NS_DS, 'G').Text);
        result.dsa.g := BN_bin2bn(@v[0], length(v), nil);
        v := unbase64(kd.elementNS(NS_DS, 'Y').Text);
        result.dsa.pub_key := BN_bin2bn(@v[0], length(v), nil);

//        if elementNS(kd, 'X', NS_DS) <> nil then
//        begin
//          v := unbase64(elementNS(kd, 'X', NS_DS).Text);
//          result.dsa.priv_key := BN_bin2bn(@v[0], length(v), nil);
//        end;
      end
      else
        raise Exception.Create('No Key Info found');
    end;

    result.Link;
  finally
    result.Free;
  end;
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
    raise Exception.Create('Unsupported signature method '+uri);
end;

{ TDigitalSignatureReferenceList }

function TDigitalSignatureReferenceList.getReference(i: integer): TDigitalSignatureReference;
begin
  result := TDigitalSignatureReference(ObjectByIndex[i]);
end;

function TDigitalSignatureReferenceList.itemClass: TAdvObjectClass;
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


end.
