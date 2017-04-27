unit ReverseClient;

interface

uses
  SysUtils, Classes,
  AdvObjects,
  IdCustomHttpServer, IdContext, IdHttp;

Type
  TReverseProxyInfo = class(TAdvObject)
  private
    Fpath: String;
    Ftarget: String;
  public
    Constructor Create(path, target : String);
    function link : TReverseProxyInfo; overload;
    property path : String read Fpath write Fpath;
    property target : String read Ftarget write Ftarget;
  end;

  TReverseClient = class (TAdvObject)
  private
    Frequest: TIdHTTPRequestInfo;
    Fresponse: TIdHTTPResponseInfo;
    Fcontext: TIdContext;
    Fproxy: TReverseProxyInfo;
    procedure Setproxy(const Value: TReverseProxyInfo);

    function getOutput : TStream;
  public
    destructor Destroy; override;
    property proxy: TReverseProxyInfo read Fproxy write Setproxy;
    property context: TIdContext read Fcontext write Fcontext;
    property request: TIdHTTPRequestInfo read Frequest write Frequest;
    property response: TIdHTTPResponseInfo read Fresponse write Fresponse;

    procedure execute;
  end;

implementation

{ TReverseProxyInfo }

constructor TReverseProxyInfo.Create(path, target: String);
begin
  inherited create;
  FPath := path;
  FTarget := target;
end;

function TReverseProxyInfo.link: TReverseProxyInfo;
begin
  result := TReverseProxyInfo(inherited Link);
end;

{ TReverseClient }

destructor TReverseClient.Destroy;
begin
  Fproxy.Free;
  inherited;
end;

procedure TReverseClient.Setproxy(const Value: TReverseProxyInfo);
begin
  Fproxy.Free;
  Fproxy := Value;
end;

procedure TReverseClient.execute;
var
  client : TIdCustomHTTP;
  url, s : String;
  i : integer;
begin
  client := TIdCustomHTTP.Create(nil);
  try
    client.HTTPOptions := [hoNoProtocolErrorException, hoNoParseMetaHTTPEquiv, hoWaitForUnexpectedData, hoNoReadMultipartMIME,  hoNoParseXmlCharset, hoWantProtocolErrorContent];

    client.Request.Accept := request.Accept;
    client.Request.AcceptCharSet := request.AcceptCharSet;
    client.Request.AcceptEncoding := request.AcceptEncoding;
    client.Request.AcceptLanguage := request.AcceptLanguage;
    client.Request.BasicAuthentication := request.BasicAuthentication;
    client.Request.Host := request.Host;
    client.Request.From := request.From;
    client.Request.Password := request.Password;
    client.Request.Referer := request.Referer;
    client.Request.UserAgent := request.UserAgent;
    client.Request.Username := request.Username;
    client.Request.ProxyConnection := request.ProxyConnection;
    client.Request.Range := request.Range;
    client.Request.Ranges := request.Ranges;
    client.Request.MethodOverride := request.MethodOverride;
    client.Request.CacheControl := request.CacheControl;
    client.Request.CharSet := request.CharSet;
    client.Request.Connection := request.Connection;
    client.Request.ContentDisposition := request.ContentDisposition;
    client.Request.ContentEncoding := request.ContentEncoding;
    client.Request.ContentLanguage := request.ContentLanguage;
    client.Request.ContentLength := request.ContentLength;
    client.Request.ContentRangeEnd := request.ContentRangeEnd;
    client.Request.ContentRangeStart := request.ContentRangeStart;
    client.Request.ContentRangeInstanceLength := request.ContentRangeInstanceLength;
    client.Request.ContentRangeUnits := request.ContentRangeUnits;
    client.Request.ContentType := request.ContentType;
    client.Request.ContentVersion := request.ContentVersion;
    client.Request.CustomHeaders := request.CustomHeaders;
    client.Request.Date := request.Date;
    client.Request.ETag := request.ETag;
    client.Request.Expires := request.Expires;
    client.Request.LastModified := request.LastModified;
    client.Request.Pragma := request.Pragma;
    client.Request.TransferEncoding := request.TransferEncoding;
    for s in request.CustomHeaders do
      client.Request.CustomHeaders.Add(s);
    s := request.RawHTTPCommand;
    s := s.Substring(s.IndexOf(' ')+1);
    s := s.substring(0, s.IndexOf(' '));
    url := proxy.FTarget + s.Substring(proxy.path.Length);
    try
      case request.CommandType of
        hcHEAD: client.Head(url);
        hcGET: client.Get(url, getOutput);
        hcPOST: client.Post(url, request.PostStream, getOutput);
        hcTRACE: client.Trace(url, getOutput);
        {$IFDEF VER260}
        hcDELETE: client.Delete(url);
        hcPUT: client.Put(url, request.PostStream); // broken...
        hcOPTION: client.Options(url);
        {$ELSE}
        hcDELETE: client.Delete(url, getOutput);
        hcPUT: client.Put(url, request.PostStream, getOutput);
        hcOPTION: client.Options(url, getOutput);
        // todo: patch...
        {$ENDIF}
      end;
    except
      // suppress the exception - will be handled below
    end;
    if assigned(response.ContentStream) then
      response.ContentStream.Position := 0;

    {$IFNDEF VER260} response.AcceptPatch := client.response.AcceptPatch; {$ENDIF}
    response.AcceptRanges := client.response.AcceptRanges;
    response.Location := client.response.Location;
    response.ProxyConnection := client.response.ProxyConnection;
    response.ProxyAuthenticate := client.response.ProxyAuthenticate;
    response.Server := client.response.Server;
    response.WWWAuthenticate := client.response.WWWAuthenticate;
    response.CacheControl := client.response.CacheControl;
    response.CharSet := client.response.CharSet;
    response.Connection := client.response.Connection;
    response.ContentDisposition := client.response.ContentDisposition;
    response.ContentEncoding := client.response.ContentEncoding;
    response.ContentLanguage := client.response.ContentLanguage;
    response.ContentLength := client.response.ContentLength;
    response.ContentRangeEnd := client.response.ContentRangeEnd;
    response.ContentRangeStart := client.response.ContentRangeStart;
    response.ContentRangeInstanceLength := client.response.ContentRangeInstanceLength;
    response.ContentRangeUnits := client.response.ContentRangeUnits;
    response.ContentType := client.response.ContentType;
    response.ContentVersion := client.response.ContentVersion;
    response.CustomHeaders := client.response.CustomHeaders;
    response.Date := client.response.Date;
    response.ETag := client.response.ETag;
    response.Expires := client.response.Expires;
    response.LastModified := client.response.LastModified;
    response.Pragma := client.response.Pragma;
    response.TransferEncoding := client.response.TransferEncoding;
    response.Cookies.AddCookies(client.CookieManager.CookieCollection);
    response.ServerSoftware := client.response.Server;
    response.ResponseText := client.response.ResponseText;
    response.ResponseNo := client.response.ResponseCode;


//    response.AuthRealm := client.Response.AuthRealm;
//    response.ResponseNo := client.Response.ResponseNo;
//    !
//    procedure Redirect(const AURL: string);
//    procedure WriteHeader;
//    procedure WriteContent;
//    //
//    function ServeFile(AContext: TIdContext; const AFile: String): Int64; virtual;
//    function SmartServeFile(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; const AFile: String): Int64;
//    //
//    property CloseConnection: Boolean read FCloseConnection write SetCloseConnection;
//    property ContentStream: TStream read FContentStream write FContentStream;
//    property ContentText: string read FContentText write FContentText;
//    property FreeContentStream: Boolean read FFreeContentStream write FFreeContentStream;
//    // writable for isapi compatibility. Use with care
//    property HeaderHasBeenWritten: Boolean read FHeaderHasBeenWritten write FHeaderHasBeenWritten;
//    property HTTPServer: TIdCustomHTTPServer read FHTTPServer;
//    property Session: TIdHTTPSession read FSession;
//
//    response
//    !
//    client.url := request.Document; // todo
//
//    property Method: TIdHTTPMethod read FMethod write FMethod;
//    property Source: TStream read FSourceStream write FSourceStream;
//    property UseProxy: TIdHTTPConnectionType read FUseProxy;
//    property IPVersion: TIdIPversion read FIPVersion write FIPVersion;
//    property Destination: string read FDestination write FDestination;

  finally
    client.Free;
  end;;
end;

function TReverseClient.getOutput: TStream;
begin
  response.ContentStream := TMemoryStream.create;
  response.FreeContentStream := true;
  result := response.ContentStream;
end;

end.
