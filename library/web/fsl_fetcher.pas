Unit fsl_fetcher;

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

Interface

Uses
  SysUtils, Classes,
  IdComponent, IdURI, IdFTP, IdHTTP,
  IdOpenSSLIOHandlerClient, IdOpenSSLVersion,
  fsl_base, fsl_utilities, fsl_stream, fsl_json;

Type
  TInternetFetcherMethod = (imfGet, imfPost);

  TProgressEvent = procedure(sender : TObject; progress : integer) of Object;

  { TInternetFetcher }

  TInternetFetcher = Class (TFslObject)
  Private
    FLastModified: TDateTime;
    FURL: String;
    FBuffer: TFslBuffer;
    FUsername: String;
    FPassword: String;
    FMethod: TInternetFetcherMethod;
    FContentType: String;
    FUserAgent : String;
    FBytesToTransfer: Int64;
    FOnProgress: TProgressEvent;
    FAccept: String;
    FTimeout: cardinal;
    FResponseCode : integer;
    FNoErrors: boolean;

    procedure SetBuffer(const Value: TFslBuffer);
    procedure SetPassword(const Value: String);
    procedure SetUsername(const Value: String);
    procedure HTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure HTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure HTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);

  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    constructor Create; Override;

    destructor Destroy; Override;

    Property URL : String read FURL write FURL;
    property Accept : String read FAccept write FAccept;
    Property Buffer : TFslBuffer read FBuffer write SetBuffer;
    Property UserAgent : String read FUserAgent write FUserAgent;

    Function CanFetch : Boolean;
    Procedure Fetch; overload;

    Property Username : String read FUsername write SetUsername;
    Property Password : String read FPassword write SetPassword;
    Property Method : TInternetFetcherMethod read FMethod write FMethod;
    Property ContentType : String read FContentType write FContentType;
    property ResponseCode : Integer read FResponseCode write FResponseCode;
    Property LastModified : TDateTime read FLastModified write FLastModified;
    property OnProgress : TProgressEvent read FOnProgress write FOnProgress;
    property Timeout : cardinal read FTimeout write FTimeout;
    property NoErrors : boolean read FNoErrors write FNoErrors;
                                                                            
    class function fetchUrlString(url : String; timeout : cardinal = 0) : String;
    class function fetchUrl(url : String; timeout : cardinal = 0) : TBytes;
    class function fetchJson(url : String; timeout : cardinal = 0) : TJsonObject;
    class function fetchJsonArray(url : String; timeout : cardinal = 0) : TJsonArray;
  End;

Implementation

{ TInternetFetcher }

function TInternetFetcher.CanFetch: Boolean;
begin
  result := StringStartsWith(url, 'file:') Or
            StringStartsWith(url, 'http:') or
            StringStartsWith(url, 'https:') or
            StringStartsWith(url, 'ftp:');
end;

constructor TInternetFetcher.Create;
begin
  inherited;
  FBuffer := TFslBuffer.Create;
  FMethod := imfGet;
end;

destructor TInternetFetcher.Destroy;
begin
  FBuffer.free;
  inherited;
end;

procedure TInternetFetcher.Fetch;
var
  oUri : TIdURI;
  oHTTP: TIdHTTP;
  oMem, pMem : TMemoryStream;
  oSSL : TIdOpenSSLIOHandlerClient;
  oFtp : TIdFTP;
begin
  if StringStartsWith(url, 'file:') Then
    FBuffer.LoadFromFileName(Copy(url, 6, $FFFF))
  else
  Begin
    oUri := TIdURI.Create(url);
    Try
      if (oUri.Protocol = 'http') or (oUri.Protocol = 'https') Then
      Begin
        oHTTP := TIdHTTP.Create(nil);
        pmem := nil;
        Try
          oSSL := TIdOpenSSLIOHandlerClient.Create(Nil);
          Try
            oHTTP.IOHandler := oSSL;
            oHTTP.OnWork := HTTPWork;
            oHTTP.OnWorkBegin := HTTPWorkBegin;
            oHTTP.OnWorkEnd := HTTPWorkEnd;
//            oSSL.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_2;
            oSSL.Options.VerifyServerCertificate := false;
            oHTTP.HandleRedirects := true;
            oHTTP.Request.Accept := FAccept;
            if FMethod = imfPost then
            begin
              oHTTP.Request.ContentType := FContentType;
              pmem := TMemoryStream.Create;
              FBuffer.SaveToStream(pmem);
              pmem.position := 0;
            end;
            if (UserAgent <> '') then
              oHTTP.Request.UserAgent := UserAgent;
            if (FUsername <> '') and (FPassword <> '') then
            begin
              oHttp.Request.BasicAuthentication := true;
              oHTTP.Request.Username := FUsername;
              oHTTP.Request.Password := FPassword;
            end;
            oHTTP.URL.URI := url;
            if FTimeout > 0 then
              oHTTP.ReadTimeout := FTimeout
            else
              oHTTP.ReadTimeout := 30000;
            if FNoErrors then
              oHTTP.HTTPOptions := oHTTP.HTTPOptions + [hoNoProtocolErrorException, hoWantProtocolErrorContent];
            oMem := TMemoryStream.Create;
            try
              if FMethod = imfPost then
                oHTTP.Post(url, pMem, oMem)
              else
                oHTTP.Get(url, oMem);
              oMem.position := 0;
              FBuffer.Capacity := oMem.Size;
              oMem.read(Fbuffer.Data^, oMem.Size);
              FContentType := oHTTP.Response.ContentType;
              FLastModified := oHTTP.Response.LastModified;
              FResponseCode := oHTTP.ResponseCode;
            Finally
              oMem.free;
            End;
          Finally
            oSSL.free;
          End;
        Finally
          oHTTP.free;
          pmem.free;
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
            oMem.free;
          End;
        Finally
          oFtp.free;
        End;
      End
      Else
        raise EWebException.create('Protocol '+oUri.Protocol+' not supported');
    Finally
      oUri.free;
    End;
  End;
end;

class function TInternetFetcher.fetchUrlString(url: String; timeout: cardinal): String;
begin
  result := TEncoding.UTF8.GetString(fetchUrl(url, timeout));
end;

function TInternetFetcher.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FURL.length * sizeof(char)) + 12);
  inc(result, FBuffer.sizeInBytes(magic));
  inc(result, (FUsername.length * sizeof(char)) + 12);
  inc(result, (FPassword.length * sizeof(char)) + 12);
  inc(result, (FContentType.length * sizeof(char)) + 12);
  inc(result, (FUserAgent.length * sizeof(char)) + 12);
  inc(result, (FAccept.length * sizeof(char)) + 12);
end;

class function TInternetFetcher.fetchUrl(url : String; timeout : cardinal = 0) : TBytes;
var
  this : TInternetFetcher;
begin
  this := TInternetFetcher.Create;
  try
    this.URL := url;
    this.timeout := timeout;
    this.Fetch;
    result := this.Buffer.AsBytes;
  finally
    this.free;
  end;
end;

class function TInternetFetcher.fetchJson(url : String; timeout : cardinal = 0) : TJsonObject;
begin
  result := TJSONParser.Parse(fetchUrl(url, timeout));
end;

class function TInternetFetcher.fetchJsonArray(url : String; timeout : cardinal = 0) : TJsonArray;
begin
  result := TJSONParser.ParseNode(fetchUrl(url, timeout)) as TJsonArray;
end;


procedure TInternetFetcher.HTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if FBytesToTransfer = 0 then // No Update File
    Exit;

  if Assigned(FOnProgress) then
    FOnProgress(self, Round((AWorkCount / FBytesToTransfer) * 100));
end;

procedure TInternetFetcher.HTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  FBytesToTransfer := AWorkCountMax;
end;

procedure TInternetFetcher.HTTPWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
  FBytesToTransfer := 0;
  if Assigned(FOnProgress) then
    FOnProgress(self, 100);
end;

procedure TInternetFetcher.SetBuffer(const Value: TFslBuffer);
begin
  FBuffer.free;
  FBuffer := Value;
end;

procedure TInternetFetcher.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TInternetFetcher.SetUsername(const Value: String);
begin
  FUsername := Value;
end;

End.
