unit jwt;

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

interface

uses
  SysUtils,
  fsl_utilities,
  fsl_base, fsl_json,
  fsl_crypto;

type
  TJWTServices = class (TFslObject)
  private
    FCert: String;
    FPassword: String;
    FJWKAddress : String;
    FDatabaseId : String;
    FHost: String;
    function jwk : TJWK;
  public
    function Link : TJWTServices; overload;

    property JWKAddress : String read FJWKAddress write FJWKAddress;
    property Cert : String read FCert write FCert;
    property Password : String read FPassword write FPassword;
    property DatabaseId : String read FDatabaseId write FDatabaseId;
    property Host : String read FHost write FHost;

    function jwkList : TJWKList;
    function makeJWK : String;
    function makeJWT(name : string = '') : String;
    function pack(jwt : TJWT) : String;
  end;

implementation

{ TJWTServices }

function TJWTServices.jwkList: TJWKList;
begin
  result := TJWKList.create;
  try
    result.Add(jwk);
    result.Link;
  finally
    result.Free;
  end;
end;

function TJWTServices.Link: TJWTServices;
begin
  result := TJWTServices(inherited Link);
end;

function TJWTServices.JWK : TJWK;
begin
  result := TJWTUtils.loadKeyFromRSACert(AnsiString(FCert));
  result.obj['alg'] := 'RS256';
  result.obj['use'] := 'sig';
  result.obj['kid'] := JWKAddress;
  result.obj['sub'] := Host;
  result.obj['iss'] := FDatabaseId;
end;

function TJWTServices.makeJWK : String;
var
  jwk : TJWK;
begin
  jwk := self.jwk;
  try
    result := TJSONWriter.writeObjectStr(jwk.obj, true);
  finally
    jwk.free;
  end;
end;

function TJWTServices.makeJWT(name : String) : String;
var
  jwk : TJWK;
  jwt :  TJWT;
begin
  jwk := TJWTUtils.loadKeyFromRSACert(AnsiString(FCert));
  try
    jwk.obj['alg'] := 'RS256';
    jwk.obj['use'] := 'sig';
    jwk.obj['kid'] := JWKAddress;
    if name = '' then
      jwk.obj['sub'] := Host
    else
      jwk.obj['sub'] := name;
    jwk.obj['iss'] := FDatabaseId;

    jwt := TJWT.Create;
    try
      jwt.subject := host;
      jwt.expires := now + 1;
      jwt.issuer := FDatabaseId;
      jwt.issuedAt := now;

      result := TJWTUtils.pack(jwt, jwt_hmac_rsa256, jwk, ChangeFileExt(FCert, '.key'), FPassword);
    finally
      jwt.free;
    end;
  finally
    jwk.free;
  end;
end;

function TJWTServices.pack(jwt: TJWT): String;
begin
  result := TJWTUtils.rsa_pack(jwt, jwt_hmac_rsa256, ChangeFileExt(FCert, '.key'), FPassword);
end;

end.

