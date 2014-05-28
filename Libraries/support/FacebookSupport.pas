unit FacebookSupport;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

uses
  SysUtils, Classes;

Function FacebookCheckLogin(id, secret, url, code : String; var token, expires, error : String) : boolean;
Function FacebookGetDetails(token : String; var id, name, email, error : String) : boolean;

Function GoogleCheckLogin(id, secret, url, code : String; var token, expires, error : String) : boolean;
Function GoogleGetDetails(token, key : String; var id, name, email, error : String) : boolean;

implementation

uses
  InternetFetcher,
  IdHTTP, IdSSLOpenSSL,
  AdvMemories,
  json, ParseMap;

Function FacebookCheckLogin(id, secret, url, code : String; var token, expires, error : String) : boolean;
var
  fetch : TgwInternetFetcher;
  parsemap : TParseMap;
begin
  result := false;
  try
    fetch := TgwInternetFetcher.create;
    try
      fetch.URL := 'https://graph.facebook.com/oauth/access_token?client_id='+id+'&redirect_uri='+url+'&client_secret='+secret+'&code='+code;
      fetch.Fetch;
      parsemap := TParseMap.createSmart(fetch.Buffer.AsUnicode);
      try
        token := parsemap.GetVar('access_token');
        expires := parsemap.GetVar('expires');
      finally
        parsemap.free;
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


Function GoogleCheckLogin(id, secret, url, code : String; var token, expires, error : String) : boolean;
var
  http: TIdHTTP;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
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
        ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
        Try
          http.IOHandler := ssl;
          ssl.SSLOptions.Mode := sslmClient;
          ssl.SSLOptions.Method := sslvSSLv3;
          http.Request.ContentType := 'application/x-www-form-urlencoded';
          resp := TBytesStream.create;
          try
            http.Post('https://accounts.google.com/o/oauth2/token', post, resp);
            resp.position := 0;
            resp.SaveToFile('c:\temp\google.json');
            resp.position := 0;
            json := TJSONParser.Parse(resp);
            try
              token := json.vStr['access_token'];
              expires := json.vStr['expires_in'];
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
  fetch : TgwInternetFetcher;
  json : TJSONObject;
begin
  result := false;
  try
    fetch := TgwInternetFetcher.create;
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


Function GoogleGetDetails(token, key : String; var id, name, email, error : String) : boolean;
var
  fetch : TgwInternetFetcher;
  json : TJSONObject;
begin
  result := false;
  try
    fetch := TgwInternetFetcher.create;
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
  except
    on e:exception do
      error := e.Message;
  end;
end;

end.
