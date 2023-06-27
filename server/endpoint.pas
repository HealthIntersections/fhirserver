unit endpoint;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

Uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes, Generics.Collections,
  IdCustomHTTPServer, IdContext, IdOpenSSLX509,
  fsl_base, fsl_threads, fsl_crypto, fsl_stream, fsl_utilities, fsl_http, fsl_json, fsl_npm_cache, fsl_i18n,
  fdb_manager,
  fhir_objects,
  server_config, utilities, session, tx_manager, kernel_thread,
  web_event, web_base, web_cache, time_tracker, server_stats;

type
  TFHIRWebServerClientInfo = class(TFslObject)
  private
    FContext: TIdContext;
    FActivity: String;
    FSession: TFHIRSession;
    FCount: integer;
    FStart: UInt64;
    procedure SetSession(const Value: TFHIRSession);
  public
    destructor Destroy; Override;
    property Context: TIdContext read FContext write FContext;
    property Session: TFHIRSession read FSession write SetSession;
    property Activity: String read FActivity write FActivity;
    property Count: integer read FCount write FCount;
    property Start : UInt64 read FStart write FStart;
  end;

  TTokenRedirectManager = class (TFslObject)
  private
    FLock : TFslLock;
    FMap : TDictionary<String, String>;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    procedure recordRedirect(token, url : String);
    function getRedirect(token : String; var url : String) : boolean;
    procedure clear;
  end;

  { TFhirWebServerEndpoint }

  TFhirWebServerEndpoint = class abstract (TFHIRWebServerBase)
  private
    FCode : String;
    FPathWithSlash : String;
    FPathNoSlash : String;
    FOnReturnFileSource : TWebReturnDirectFileEvent;
    FOnReturnFile : TWebReturnProcessedFileEvent;
    FOnProcessFile : TWebProcessFileEvent;
    FRequestCount : integer;
  protected
    FTokenRedirects : TTokenRedirectManager;

    function OAuthPath(secure: boolean): String;
    function AbsoluteURL(secure: boolean) : String;
    procedure cacheResponse(response: TIdHTTPResponseInfo; caching: TFHIRCacheControl);

    procedure countRequest;
    function processFile(session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>) : string; overload;
    procedure returnFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>); overload;
    procedure returnFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean); overload;
    procedure returnSecureFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; variables: TFslMap<TFHIRObject>); overload;
    procedure returnSecureFile(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String); overload;
  public
    constructor Create(code, path : String; common : TFHIRWebServerCommon);
    destructor Destroy; override;
    property PathNoSlash : String read FPathNoSlash;
    property PathWithSlash : String read FPathWithSlash;
    property code : String read FCode;
    function ClientAddress(secure: boolean): String;
    function logId : String; virtual; abstract;
    property RequestCount : integer read FRequestCount;

    property OnReturnFile : TWebReturnProcessedFileEvent read FOnReturnFile write FOnReturnFile;
    property OnReturnFileSource : TWebReturnDirectFileEvent read FOnReturnFileSource write FOnReturnFileSource;
    property OnProcessFile : TWebProcessFileEvent read FOnProcessFile write FOnProcessFile;

    function PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String; virtual; abstract;
    function SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TTimeTracker) : String; virtual; abstract;
    function description : string; virtual; abstract;

  end;

  { TFHIRServerEndPoint }
  // this is the base for actual end points - they are a pair - a web end point - the class above, a
  // and this class, which is the administrative base

  TFHIRServerEndPoint = class abstract (TFslObject)
  private
    FDatabase : TFDBManager;
    FConfig : TFHIRServerConfigSection;
    FSettings : TFHIRServerSettings;
    FTerminologies : TCommonTerminologies;
    FWebEndPoint : TFhirWebServerEndpoint;
    FCommon : TFHIRWebServerCommon;
    FI18n : TI18nSupport;
  protected
    FPcm : TFHIRPackageManager;
    function nonDefPort(port, def : word) : String;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; pcm : TFHIRPackageManager; i18n : TI18nSupport);
    destructor Destroy; override;
    function link : TFHIRServerEndPoint; overload;

    property Database : TFDBManager read FDatabase;
    property Config : TFHIRServerConfigSection read FConfig;
    property Settings : TFHIRServerSettings read FSettings;
    property Terminologies : TCommonTerminologies read FTerminologies;
    property WebEndPoint : TFhirWebServerEndpoint read FWebEndPoint write FWebEndPoint;
    property Common : TFHIRWebServerCommon read FCommon;
    property i18n : TI18nSupport read FI18n;

    function summary : String; virtual; abstract;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; virtual;
    function cacheSize(magic : integer) : UInt64; virtual;
    procedure clearCache; virtual;
    procedure SweepCaches; virtual;
    procedure SetCacheStatus(status : boolean); virtual;
    procedure getCacheInfo(ci: TCacheInformation); virtual;
    procedure recordStats(var rec : TStatusRecord); virtual;

    procedure InstallDatabase; virtual;
    procedure UninstallDatabase; virtual;
    procedure LoadPackages(plist : String); virtual;
    procedure updateAdminPassword; virtual;
    procedure Load; virtual;
    Procedure Unload; virtual;
    procedure internalThread(callback : TFhirServerMaintenanceThreadTaskCallBack); virtual;
  end;


implementation

{ TFHIRWebServerClientInfo }

destructor TFHIRWebServerClientInfo.Destroy;
begin
  FSession.Free;
  inherited;
end;

procedure TFHIRWebServerClientInfo.SetSession(const Value: TFHIRSession);
begin
  FSession.Free;
  FSession := Value;
end;

{ TTokenRedirectManager }

procedure TTokenRedirectManager.clear;
begin
  FLock.Lock('record');
  try
    FMap.clear;
  finally
    FLock.Unlock;
  end;
end;

constructor TTokenRedirectManager.Create;
begin
  inherited;
  FLock := TFslLock.Create('token.redirects');
  FMap := TDictionary<String,String>.create;
end;

destructor TTokenRedirectManager.Destroy;
begin
  FMap.Free;
  FLock.Free;
  inherited;
end;

function TTokenRedirectManager.getRedirect(token: String; var url: String): boolean;
begin
  FLock.Lock('record');
  try
    result := FMap.TryGetValue(token, url);
  finally
    FLock.Unlock;
  end;
end;

procedure TTokenRedirectManager.recordRedirect(token, url: String);
begin
  FLock.Lock('record');
  try
    FMap.AddOrSetValue(token, url);
  finally
    FLock.Unlock;
  end;
end;

function TTokenRedirectManager.sizeInBytesV(magic : integer) : cardinal;
var
  p : TPair<String, String>;
begin
  result := inherited sizeInBytesV(magic) + SizeoF(FLock);
  for p in FMap do
    result := result + p.Key.Length + p.Value.Length + 24;
end;

{ TFhirWebServerEndpoint }

constructor TFhirWebServerEndpoint.Create(code, path: String;
  common: TFHIRWebServerCommon);
begin
  inherited create(common);
  FCode := code;
  if (path.EndsWith('/')) then
  begin
    FPathWithSlash := path;
    FPathNoSlash := path.Substring(0, path.Length-1);
  end
  else
  begin
    FPathNoSlash := path;
    FPathWithSlash := path+'/';
  end;
  FTokenRedirects := TTokenRedirectManager.create;
end;

destructor TFhirWebServerEndpoint.Destroy;
begin
  FTokenRedirects.Free;
  inherited;
end;

function TFhirWebServerEndpoint.OAuthPath(secure: boolean): String;
begin
  if secure then
  begin
    if Common.StatedSSLPort = 443 then
      result := 'https://' + Common.Host + FPathNoSlash
    else
      result := 'https://' + Common.Host + ':' + inttostr(Common.StatedSSLPort) + FPathNoSlash;
  end
  else
  begin
    if Common.StatedPort = 80 then
      result := 'http://' + Common.Host + FPathNoSlash
    else
      result := 'http://' + Common.Host + ':' + inttostr(Common.StatedPort) + FPathNoSlash;
  end;
end;

function TFhirWebServerEndpoint.processFile(session: TFhirSession; named,
  path: String; secure: boolean; variables: TFslMap<TFHIRObject>): string;
begin
  FOnProcessFile(self, session, named, path, secure, variables, result);
end;

procedure TFhirWebServerEndpoint.returnFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String; secure: boolean; variables: TFslMap<TFHIRObject>);
begin
  FOnReturnFile(self, request, response, session, named, path, secure, variables);
end;

procedure TFhirWebServerEndpoint.returnFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String; secure: boolean);
begin
  FOnReturnFileSource(self, request, response, session, named, path);
end;

procedure TFhirWebServerEndpoint.returnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String; variables: TFslMap<TFHIRObject>);
begin
  FOnReturnFile(self, request, response, session, named, path, true, variables);
end;

procedure TFhirWebServerEndpoint.returnSecureFile(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFhirSession; named, path: String);
var
  variables : TFslMap<TFHIRObject>;
begin
  variables := TFslMap<TFHIRObject>.create;
  try
    FOnReturnFile(self, request, response, session, named, path, true, variables);
  finally
    variables.free;
  end;
end;

function TFhirWebServerEndpoint.ClientAddress(secure: boolean): String;
begin
  if secure then
    result := 'https://'+Common.host+ port(Common.StatedSSLPort, 443) + FPathNoSlash
  else
    result := 'http://'+Common.host+port(Common.StatedPort, 80) + FPathNoSlash;
end;

function TFhirWebServerEndpoint.AbsoluteURL(secure: boolean): String;
begin
  if secure then
    result := 'https://'+common.host+SSLPort(false)+FPathNoSlash
  else
    result := 'http://'+common.host+HTTPPort(false)+FPathNoSlash;
end;

procedure TFhirWebServerEndpoint.cacheResponse(response: TIdHTTPResponseInfo;
  caching: TFHIRCacheControl);
begin
  case caching of
    cacheNotAtAll:
      response.CacheControl := 'no-cache, no-store, must-revalidate';
    cacheAsException:
      response.CacheControl := 'public, max-age=600, error';
    cacheNormal:
      response.CacheControl := 'public, max-age=600';
    cacheLong:
      response.CacheControl := 'public, max-age=31536000';
  end;
end;

procedure TFhirWebServerEndpoint.countRequest;
begin
  interlockedIncrement(FRequestCount);
end;

//function TFhirWebServerEndPoint.EndPointDesc(secure: boolean): String;
//begin
//  result := '';
//  if (secure) then
//  begin
//    if FPathNoSlash <> '' then
//      result := result + ' <li><a href="http://' + Common.Host + port(Common.ActualPort, 80) + FPathNoSlash + '">Unsecured access at ' + FPathNoSlash +
//        '</a> - direct access with no security considerations</li>'#13#10;
//    if Common.ActualSSLPort <> 0 then
//      result := result + ' <li><a href="' + FPathNoSlash + '">Secured access at ' + FPathNoSlash +
//        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
//  end
//  else
//  begin
//    result := result + ' <li><a href="' + FPathNoSlash + '">Unsecured access at ' + FPathNoSlash +
//        '</a> - direct access with no security considerations</li>'#13#10;
//    if FPathNoSlash <> '' then
//      result := result + ' <li><a href="https://' + Common.Host + port(Common.ActualSSLPort, 443) + FPathNoSlash + '">Secured access at ' + FPathNoSlash +
//        '</a> - Login required using <a href="http://fhir-docs.smarthealthit.org/argonaut-dev/authorization/">SMART-ON-FHIR</a></li>'#13#10;
//  end;
//end;

{ TFHIRServerEndPoint }

function TFHIRServerEndPoint.cacheSize(magic : integer): UInt64;
begin
  if WebEndPoint <> nil then
    result := WebEndPoint.FTokenRedirects.sizeInBytes(magic) + WebEndPoint.Common.cache.sizeInBytes(magic)
  else
    result := 0;
end;

procedure TFHIRServerEndPoint.clearCache;
begin
  Terminologies.clearSnomed;
  if WebEndPoint <> nil then
  begin
    WebEndPoint.FTokenRedirects.clear;
    WebEndPoint.Common.Cache.Clear;
  end;
end;

procedure TFHIRServerEndPoint.SweepCaches;
begin
  // nothing
end;

constructor TFHIRServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; pcm : TFHIRPackageManager; i18n : TI18nSupport);
begin
  inherited create;
  FConfig := config;
  FSettings := settings;
  FDatabase := db;
  FTerminologies := common;
  FPcm := pcm;
  FI18n := i18n;
end;

destructor TFHIRServerEndPoint.Destroy;
begin
  FPcm.Free;
  FTerminologies.Free;
  FConfig.Free;
  FSettings.Free;
  FDatabase.Free;
  FCommon.Free;
  FI18n.free;
  inherited;
end;

procedure TFHIRServerEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  FTerminologies.getCacheInfo(ci);
  if WebEndPoint <> nil then
  begin
    ci.Add('WebEndPoint.FTokenRedirects', WebEndPoint.FTokenRedirects.sizeInBytes(ci.magic));
    ci.Add('WebEndPoint.Common.Cache', WebEndPoint.Common.Cache.sizeInBytes(ci.magic));
  end;
end;

procedure TFHIRServerEndPoint.recordStats(var rec: TStatusRecord);
begin
  Common.cache.recordStats(rec);
end;

procedure TFHIRServerEndPoint.InstallDatabase;
begin
 // nothing
end;

procedure TFHIRServerEndPoint.internalThread(
  callback: TFhirServerMaintenanceThreadTaskCallBack);
begin
  // nothing
end;

function TFHIRServerEndPoint.link: TFHIRServerEndPoint;
begin
  result := TFHIRServerEndPoint(inherited link);
end;

procedure TFHIRServerEndPoint.Load;
begin
 // nothing
end;

procedure TFHIRServerEndPoint.LoadPackages(plist: String);
begin
 // nothing
end;

function TFHIRServerEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  FCommon := common.link;
end;

procedure TFHIRServerEndPoint.SetCacheStatus(status: boolean);
begin
  if WebEndPoint <> nil then
    WebEndPoint.Common.Cache.Caching := status;
end;

procedure TFHIRServerEndPoint.updateAdminPassword;
begin
 // nothing
end;

procedure TFHIRServerEndPoint.UninstallDatabase;
begin
 // nothing
end;

procedure TFHIRServerEndPoint.Unload;
begin
 // nothing
end;

function TFHIRServerEndPoint.nonDefPort(port, def : word) : String;
begin
  if port = def then
    result := ''
  else
    result := ':'+inttostr(port);
end;


end.


