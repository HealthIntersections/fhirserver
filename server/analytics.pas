unit analytics;

// For test.fhir.org myAnalyticsId should be 'UA-88535340-3'

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
  IdHTTP, IdSSLOpenSSL,
  fsl_base, fsl_threads, fsl_utilities,
  fhir_objects;

type
  TGoogleAnalyaticsEventData = class (TFslObject)
  private
    FResourceName: String;
    FUserId: String;
    FOperationName: String;
    FIp: String;
    FUserAgent: String;
    FCycle: Integer;
  public
    function Link : TGoogleAnalyaticsEventData; overload;

    property cycle : Integer read FCycle write FCycle;
    property resourceName : String read FResourceName write FResourceName;
    property operationName : String read FOperationName write FOperationName;
    property userId : String read FUserId write FUserId;
    property ip : String read FIp write FIp;
    property userAgent : String read FUserAgent write FUserAgent;
  end;

  TGoogleAnalyticsProvider = class (TFslObject)
  private
    FLock : TFslLock;
    FEvents : TFslList<TGoogleAnalyaticsEventData>;
    FServerId: String;
    FCycle : integer;
    procedure post(cnt : String);
    {$IFDEF FPC}
    function filter(sender : TObject; item : TGoogleAnalyaticsEventData) : boolean;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;

    property serverId : String read FServerId write FServerId;

    procedure recordEvent(resourceName : String; operationName : String; userId : String; ip : String; userAgent : String);
    procedure commit;
  end;



implementation

{ TGoogleAnalyticsProvider }

constructor TGoogleAnalyticsProvider.Create;
begin
  inherited;
  FLock := TFslLock.Create('GoogleAnalytics');
  FEvents := TFslList<TGoogleAnalyaticsEventData>.create;
end;

destructor TGoogleAnalyticsProvider.Destroy;
begin
  FEvents.Free;
  FLock.Free;
  inherited;
end;

procedure TGoogleAnalyticsProvider.post(cnt: String);
var
  http: TIdHTTP;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  post, resp : TStringStream;
begin
  post := TStringStream.create(cnt, TEncoding.UTF8);
  try
    http := TIdHTTP.Create(nil);
    Try
      ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
      Try
        http.IOHandler := ssl;
        ssl.SSLOptions.Mode := sslmClient;
        ssl.SSLOptions.Method := sslvTLSv1_2;
        http.Request.ContentType := 'application/x-www-form-urlencoded';
        resp := TStringStream.create;
        try
          try
            http.Post('https://www.google-analytics.com/batch', post, resp);
          except
            on e : EIdHTTPProtocolException do
              raise EFHIRException.create(e.message+' : '+e.ErrorMessage);
            on e:Exception do
              raise;
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
end;

procedure TGoogleAnalyticsProvider.recordEvent(resourceName, operationName, userId, ip, userAgent: String);
var
  event : TGoogleAnalyaticsEventData;
begin
  if FServerId = '' then
    exit;
  event := TGoogleAnalyaticsEventData.Create;
  try
    event.resourceName := resourceName;
    event.operationName := operationName;
    event.userId := userId;
    event.ip := ip;
    event.userAgent := userAgent;
    FLock.Lock;
    try
      FEvents.Add(event.Link);
    finally
      FLock.Unlock;
    end;
  finally
    event.Free;
  end;
end;

{$IFDEF FPC}
function TGoogleAnalyticsProvider.filter(sender : TObject; item : TGoogleAnalyaticsEventData) : boolean;
begin
  result := item.cycle = FCycle;
end;
{$ENDIF}

procedure TGoogleAnalyticsProvider.commit;
var
  b : TStringBuilder;
  event : TGoogleAnalyaticsEventData;
begin
  if FServerId = '' then
    exit;
  b := TStringBuilder.Create;
  try
    FLock.Lock;
    try
      inc(FCycle);
      for event in FEvents do
      begin
        event.cycle := FCycle;
        b.append('v=1');
        b.append('&tid=').append(FServerId);
        b.append('&t=event');
        b.append('&an=').append('http://test.fhir.org');
        b.append('&ec=').append(EncodeMIME(event.ResourceName));
        b.append('&ea=').append(event.operationName);
        b.append('&cid=').append(event.operationName);
        b.append('&uip=').append(EncodeMIME(event.ip));
        b.append('&ua=').append(EncodeMIME(event.userAgent));
        b.append(#10);
      end;
    finally
      FLock.Unlock;
    end;
    if b.ToString = '' then
      exit;
    post(b.ToString);
    FLock.Lock;
    try
      {$IFDEF FPC}
      FEvents.removeAll(filter);
      {$ELSE}
      FEvents.removeAll(function (item : TGoogleAnalyaticsEventData) : boolean
        begin
          result := item.cycle = FCycle;
        end);
      {$ENDIF}
    finally
      FLock.Unlock;
    end;
  finally
    b.Free;
  end;
end;

{ TGoogleAnalyaticsEventData }

function TGoogleAnalyaticsEventData.Link: TGoogleAnalyaticsEventData;
begin
  result := TGoogleAnalyaticsEventData(inherited Link);
end;

end.
