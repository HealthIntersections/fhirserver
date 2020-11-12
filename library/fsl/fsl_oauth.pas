unit fsl_oauth;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
  SysUtils, Classes,
  IdHTTP, idHMAC, IdHMACSHA1,
  IdOpenSSLIOHandlerClient, IdOpenSSLVersion,
  fsl_json, fsl_crypto, fsl_utilities,
  fsl_http, fsl_fetcher;

Function FacebookCheckLogin(id, secret, url, code : String; var token, expires, error : String) : boolean;
Function FacebookGetDetails(token : String; var id, name, email, error : String) : boolean;

Function GoogleCheckLogin(id, secret, url, code : String; var token, expires, jwt, error : String) : boolean;
Function GoogleGetDetails(token, key, jwtsrc : String; var id, name, email, error : String) : boolean;

function FitBitInitiate(secret, key, nonce, callback : String) : String;

implementation

Function FacebookCheckLogin(id, secret, url, code : String; var token, expires, error : String) : boolean;
var
  fetch : TInternetFetcher;
  json : TJSONObject;
begin
  result := false;
  try
    fetch := TInternetFetcher.create;
    try
      fetch.URL := 'https://graph.facebook.com/oauth/access_token?client_id='+id+'&redirect_uri='+url+'&client_secret='+secret+'&code='+code;
      fetch.Fetch;
      json := TJSONParser.Parse(fetch.Buffer.AsText);
      try
        token := json.str['access_token'];
        expires := json.str['expires_in'];
      finally
        json.free;
      end;
      result := true;
    finally
      fetch.free;
    end;
  except
    on e:exception do
      error := e.Message;
  end;
end;


Function GoogleCheckLogin(id, secret, url, code : String; var token, expires, jwt, error : String) : boolean;
var
  http: TIdHTTP;
  ssl : TIdOpenSSLIOHandlerClient;
  post, resp : TBytesStream;
  json : TJSONObject;
begin
  result := false;
  try
    post := TBytesStream.create(TEncoding.ASCII.getBytes(
        'code='+code+'&'+
        'client_id='+id+'&'+
        'client_secret='+secret+'&'+
        'redirect_uri='+url+'&'+
        'grant_type=authorization_code'));
    try
      result := false;
      http := TIdHTTP.Create(nil);
      Try
        ssl := TIdOpenSSLIOHandlerClient.Create(Nil);
        Try
          http.IOHandler := ssl;
          ssl.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_2;
          http.Request.ContentType := 'application/x-www-form-urlencoded';
          resp := TBytesStream.create;
          try
            http.Post('https://accounts.google.com/o/oauth2/token', post, resp);
            resp.position := 0;
            json := TJSONParser.Parse(resp);
            try
              token := json.vStr['access_token'];
              expires := json.vStr['expires_in'];
              jwt := json.vStr['id_token'];
              result := true;
            finally
              json.free;
            end;
          finally
            resp.free;
          end;
        finally
          ssl.free;
        end;
      finally
        http.free;
      end;
    finally
      post.free;
    end;
  except
    on e:exception do
      error := e.Message;
  end;
end;


Function FacebookGetDetails(token : String; var id, name, email, error : String) : boolean;
var
  fetch : TInternetFetcher;
  json : TJSONObject;
begin
  result := false;
  try
    fetch := TInternetFetcher.create;
    try
      fetch.URL := 'https://graph.facebook.com/me?access_token='+token;
      fetch.Fetch;
      json := TJSONParser.Parse(fetch.Buffer.AsBytes);
      try
        id := json.vStr['id'];
        name := json.vStr['name'];
        email := json.vStr['email'];
      finally
        json.free;
      end;
      result := true;
    finally
      fetch.free;
    end;
  except
    on e:exception do
      error := e.Message;
  end;
end;


Function GoogleGetDetails(token, key, jwtsrc : String; var id, name, email, error : String) : boolean;
var
  fetch : TInternetFetcher;
  json : TJSONObject;
  jwt : TJWT;
begin
  result := false;
  try
    fetch := TInternetFetcher.create;
    try
      fetch.URL := 'https://www.googleapis.com/plus/v1/people/me?access_token='+token+'&key='+key;
      fetch.Fetch;
      json := TJSONParser.Parse(fetch.Buffer.AsBytes);
      try
        id := json.vStr['id'];
        name := json.vStr['displayName'];
        email := json.vStr['email'];
      finally
        json.free;
      end;
      result := true;
    finally
      fetch.free;
    end;
    if (jwtsrc <>'') and ((email = '') or (id = '') or (name = '')) then
    begin
      jwt := TJWTUtils.unpack(jwtsrc, false, nil);
      try
        if (email = '') then
          email := jwt.email;
        if (id = '') then
          id := jwt.subject;
        if (name = '') then
          name := jwt.name;
      finally
        jwt.Free;
      end;
    end;
  except
    on e:exception do
      error := e.Message;
  end;
end;

function SecondsSince1970 : integer;
begin
  result := trunc((now - EncodeDate(1970,1,1)) / DATETIME_SECOND_ONE);
end;

function oAuthSign(secret, s : String) : String;
var
  bs, bsecret : TBytes;
begin
  bs := AnsiStringAsBytes(AnsiString(s));
  bsecret := AnsiStringAsBytes(AnsiString(secret));
//  result := EncodeMIME(BinToBase64(THMACUtils.HMAC_HexStr(TIdHMACSHA256, bsecret, bs)));
end;


function FitBitInitiate(secret, key, nonce, callback : String) : String;
var
  ts : String;
  base : String;
  header : String;
  fetch : TInternetFetcher;
begin
  ts := inttostr(SecondsSince1970);
  try

    // build the base correctly:
    base := 'POST&'+
    'http%3A%2F%2Fapi.fitbit.com%2Foauth%2Frequest_token&'+
    'oauth_callback%3D'+EncodeMIME(callback)+'%26'+
    'oauth_consumer_key%3D'+key+'%26'+
    'oauth_nonce%3D'+nonce+'%26'+
    'oauth_signature_method%3DHMAC-SHA1%26'+
    'oauth_timestamp%3D'+ts+'%26'+
    'oauth_version%3D1.0';

    header := 'OAuth oauth_consumer_key="'+key+'", '+
      'oauth_signature_method="HMAC-SHA1", '+
      'oauth_timestamp="'+ts+'", '+
      'oauth_nonce="'+nonce+'", '+
      'oauth_callback="'+EncodeMIME(callback)+'", '+
      'oauth_signature="'+oAuthSign(secret, base)+'"'+
      'oauth_version="1.0"';

    fetch := TInternetFetcher.create;
    try
      fetch.Method := imfPost;
      fetch.URL := 'https://https://www.fitbit.com/oauth/request_token';
      fetch.Fetch;

  //      FHIR.Web.Parsers := THTTPParameters.createSmart(fetch.Buffer.AsUnicode);
  //      try
  //        token := FHIR.Web.Parsers['access_token');
  //        expires := FHIR.Web.Parsers['expires');
  //      finally
  //        FHIR.Web.Parsers.free;
  //      end;
  //      result := true;
    finally
      fetch.free;
    end;
  except
    on e : Exception do
      result := '<p>Error: '+e.message+'</p>';

  end;
end;

end.
