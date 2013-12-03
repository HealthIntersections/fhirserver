Unit InternetFetcher;

Interface

Uses
  AdvBuffers,
  AdvObjects;

Type
  TgwInternetFetcher = Class (TAdvObject)
  Private
    FURL: String;
    FBuffer: TAdvBuffer;
    FUsername: String;
    FPassword: String;
    procedure SetBuffer(const Value: TAdvBuffer);
    procedure SetPassword(const Value: String);
    procedure SetUsername(const Value: String);
  Public
    Constructor Create; Override;

    Destructor Destroy; Override;
     
    Property URL : String read FURL write FURL;
    Property Buffer : TAdvBuffer read FBuffer write SetBuffer;

    Function CanFetch : Boolean;
    Procedure Fetch;

    Property Username : String read FUsername write SetUsername;
    Property Password : String read FPassword write SetPassword;
  End;

Implementation

Uses
  StringSupport,

  SysUtils,
  Classes,
  
  IdURi,
  IdFTP,
  IdHTTP,
  IdSSLOpenSSL;

{ TgwInternetFetcher }

function TgwInternetFetcher.CanFetch: Boolean;
begin
  result := StringStartsWith(url, 'file:') Or
            StringStartsWith(url, 'http:') or
            StringStartsWith(url, 'https:') or
            StringStartsWith(url, 'ftp:');
end;

constructor TgwInternetFetcher.Create;
begin
  inherited;
  FBuffer := TAdvBuffer.create;
end;

destructor TgwInternetFetcher.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TgwInternetFetcher.Fetch;
var
  oUri : TIdURI;
  oHTTP: TIdHTTP;
  oMem : TMemoryStream;
  oSSL : TIdSSLIOHandlerSocketOpenSSL;
  oFtp : TIdFTP;
begin
  if StringStartsWith(url, 'file:') Then
      FBuffer.LoadFromFileName(Copy(url, 6, $FFFF))
  else
  Begin
    oUri := TIdURI.Create(url);
    Try
      if oUri.Protocol = 'http' Then
      Begin
        oHTTP := TIdHTTP.Create(nil);
        Try
          oHTTP.URL.URI := url;
          oMem := TMemoryStream.Create;
          try
            oHTTP.Get(url, oMem);
            oMem.position := 0;
            FBuffer.Capacity := oMem.Size;
            oMem.read(Fbuffer.Data^, oMem.Size);
          Finally
            oMem.Free;
          End;
        Finally
          oHTTP.Free;
        End;
      End
      Else if oUri.Protocol = 'https' Then
      Begin
        oHTTP := TIdHTTP.Create(nil);
        Try
          oSSL := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
          Try
            oHTTP.IOHandler := oSSL;
            oSSL.SSLOptions.Mode := sslmClient;
            oSSL.SSLOptions.Method := sslvSSLv3;
            oHTTP.URL.URI := url;
            oMem := TMemoryStream.Create;
            try
              oHTTP.Get(url, oMem);
              oMem.position := 0;
              FBuffer.Capacity := oMem.Size;
              oMem.read(Fbuffer.Data^, oMem.Size);
            Finally
              oMem.Free;
            End;
          Finally
            oSSL.Free;
          End;
        Finally
          oHTTP.Free;
        End;
      End
      Else if oUri.Protocol = 'ftp' then
      begin
        oFtp := TIdFTP.Create(nil);
        Try
          oFTP.Host := oUri.Host;
          if username = '' then
            oFTP.Username := 'anonymous'
          else
            oFTP.Username := username;
          oFTP.Password := password;
          oFTP.Connect;
          oFTP.Passive := true;
          oFTP.ChangeDir(oURI.Path);
          oMem := TMemoryStream.Create;
          try
            oFTP.Get(oURI.Document, oMem);
            oMem.position := 0;
            FBuffer.Capacity := oMem.Size;
            oMem.read(Fbuffer.Data^, oMem.Size);
          Finally
            oMem.Free;
          End;
        Finally
          oFtp.Free;
        End;
      End
      Else
        Raise Exception.Create('Protocol '+oUri.Protocol+' not supported');
    Finally
      oUri.Free;
    End;
  End;
end;

procedure TgwInternetFetcher.SetBuffer(const Value: TAdvBuffer);
begin
  FBuffer.Free;
  FBuffer := Value;
end;

procedure TgwInternetFetcher.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TgwInternetFetcher.SetUsername(const Value: String);
begin
  FUsername := Value;
end;

End.