unit JWTService;

interface

uses
  SysUtils,
  FileSupport,
  AdvObjects, AdvJson,
  JWT;

type
  TJWTServices = class (TAdvObject)
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
  authurl : String;
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

  if FSSLPort = '443' then
    authurl := 'https://'+FHost+FPath
  else
    authurl := 'https://'+FHost+':'+FSSLPort+FPath;
