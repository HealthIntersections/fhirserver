unit GoogleAnalyticsProvider;

// For test.fhir.org myAnalyticsId should be 'UA-88535340-3'

interface

uses
  SysUtils, Classes,
  IdHTTP, IdSSLOpenSSL,
  kCritSct, EncodeSupport,
  AdvGenerics, AdvObjects;

type
  TGoogleAnalyaticsEventData = class (TAdvObject)
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

  TGoogleAnalyticsProvider = class (TAdvObject)
  private
    FLock : TCriticalSection;
    FEvents : TAdvList<TGoogleAnalyaticsEventData>;
    FServerId: String;
    FCycle : integer;
    procedure post(cnt : String);
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
  FLock := TCriticalSection.Create('GoogleAnalytics');
  FEvents := TAdvList<TGoogleAnalyaticsEventData>.create;
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
  s : String;
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
              raise Exception.Create(e.message+' : '+e.ErrorMessage);
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
      FEvents.removeAll(function (item : TGoogleAnalyaticsEventData) : boolean
        begin
          result := item.cycle = FCycle;
        end);
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
