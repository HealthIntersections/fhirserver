unit JWTService;

interface

uses
  SysUtils,
  AdvObjects, AdvJson,
  JWT;

type
  TJWTServices = class (TAdvObject)
  private
    FCert: String;
    FPassword: String;
    FHost: String;
    FServerPath: String;
  public
    function Link : TJWTServices; overload;

    property Host : String read FHost write FHost;
    property ServerPath : String read FServerPath write FServerPath;
    property Cert : String read FCert write FCert;
    property Password : String read FPassword write FPassword;

    function makeJWK : String;
    function makeJWT : String;
    function pack(jwt : TJWT) : String;
  end;

implementation

{ TJWTServices }

function TJWTServices.Link: TJWTServices;
begin
  result := TJWTServices(inherited Link);
end;

function TJWTServices.makeJWK : String;
var
  jwk : TJWK;
  authurl : String;
begin
  jwk := TJWTUtils.loadKeyFromRSACert(AnsiString(FCert));
  try
    jwk.obj['alg'] := 'RS256';
    jwk.obj['use'] := 'sig';
    jwk.obj['kid'] := authurl+'/auth_key';
    jwk.obj['sub'] := Host;

    result := TJSONWriter.writeObjectStr(jwk.obj, true);
  finally
    jwk.free;
  end;
end;

function TJWTServices.makeJWT: String;
var
  jwk : TJWK;
  jwt :  TJWT;
  authurl : String;
begin
  jwk := TJWTUtils.loadKeyFromRSACert(AnsiString(FCert));
  try
    jwk.obj['alg'] := 'RS256';
    jwk.obj['use'] := 'sig';
    jwk.obj['kid'] := authurl+'/auth_key';
    jwk.obj['sub'] := Host;

    jwt := TJWT.Create;
    try
      jwt.subject := host;
      jwt.expires := now + 1;
      jwt.issuer := Host;
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
