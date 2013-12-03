unit FacebookSupport;

interface

uses
  SysUtils, Classes;

Function FacebookCheckLogin(id, secret, url, code : String; var token, expires, error : String) : boolean;
Function FacebookGetDetails(token : String; var id, name, error : String) : boolean;

Function GoogleCheckLogin(id, secret, url, code : String; var token, expires, error : String) : boolean;
Function GoogleGetDetails(token, key : String; var id, name, error : String) : boolean;

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
  prsr : TJSONParser;
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
            prsr := TJSONParser.create(resp);
            try
              while (token = '') or (expires = '') do
              begin
                if prsr.ItemName = 'access_token' then
                  token := prsr.ItemValue
                else if prsr.ItemName = 'expires_in' then
                  expires := prsr.ItemValue;
                prsr.next;
              end;
              result := true;
            finally
              prsr.free;
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


Function FacebookGetDetails(token : String; var id, name, error : String) : boolean;
var
  fetch : TgwInternetFetcher;
  json : TAdvMemoryStream;
  prsr : TJSONParser;
begin
  result := false;
  try
    fetch := TgwInternetFetcher.create;
    try
      fetch.URL := 'https://graph.facebook.com/me?access_token='+token;
      fetch.Fetch;
      json := TAdvMemoryStream.create;
      try
        json.Buffer := fetch.Buffer.link;
        prsr := TJSONParser.create(json);
        try
          while (id = '') or (name = '') do
          begin
            if prsr.ItemName = 'id' then
              id := prsr.ItemValue
            else if prsr.ItemName = 'name' then
              name := prsr.ItemValue;
            prsr.next;
          end;
        finally
          prsr.free;
        end;
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


Function GoogleGetDetails(token, key : String; var id, name, error : String) : boolean;
var
  fetch : TgwInternetFetcher;
  json : TAdvMemoryStream;
  prsr : TJSONParser;
begin
  result := false;
  try
    fetch := TgwInternetFetcher.create;
    try
      fetch.URL := 'https://www.googleapis.com/plus/v1/people/me?access_token='+token+'&key='+key;
      fetch.Fetch;
      json := TAdvMemoryStream.create;
      try
        json.Buffer := fetch.Buffer.link;
        prsr := TJSONParser.create(json);
        try
          while (id = '') or (name = '') do
          begin
            if prsr.ItemName = 'id' then
              id := prsr.ItemValue
            else if prsr.ItemName = 'displayName' then
              name := prsr.ItemValue;
            prsr.next;
          end;
        finally
          prsr.free;
        end;
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
