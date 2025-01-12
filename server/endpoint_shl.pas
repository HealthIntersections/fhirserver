unit endpoint_shl;
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

uses
  Sysutils, Classes, Graphics, {$IFDEF DELPHI}IOUtils, {$ENDIF}
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  PdfiumCore,
  fsl_base, fsl_utilities, fsl_stream, fsl_crypto, fsl_http, fsl_threads, fsl_i18n, fsl_logging, fsl_json,
  fdb_manager, fdb_dialects,
  fhir_objects, fhir_icao, fsl_web_stream,
  fhir4_factory, fhir4_ips,
  utilities, server_config,  storage, server_stats,
  web_base, endpoint, healthcard_generator;

type
  { TSHLWebServer }
  TSHLWebServer = class (TFhirWebServerEndpoint)
  private
    FPassword : String;
    FDB : TFDBManager;
    procedure SetDB(AValue: TFDBManager);
    function processCreate(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; c : TFDBConnection) : String;
    function processUpload(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; c : TFDBConnection) : String;
  public
    constructor Create(code, path : String; common : TFHIRWebServerCommon);
    destructor Destroy; override;
    function link : TSHLWebServer; overload;
    function description : String; override;
    function logId : string; override;       
    property DB : TFDBManager read FDB write SetDB;

    function PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TFslTimeTracker) : String; override;
    function SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TFslTimeTracker) : String; override;
  end;


  { TSHLWebEndPoint }

  TSHLWebEndPoint = class (TFHIRServerEndPoint)
  private
    FSHLServer : TSHLWebServer;
    FPassword : String;
    procedure checkDatabase;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; i18n : TI18nSupport; db : TFDBManager);
    destructor Destroy; override;

    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure recordStats(rec : TStatusRecord); override;
    procedure getCacheInfo(ci: TCacheInformation); override;
  end;

implementation

{ TSHLWebServer }

procedure TSHLWebServer.SetDB(AValue: TFDBManager);
begin
  FDB.free;
  FDB:=AValue;
end;

function TSHLWebServer.processCreate(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; c : TFDBConnection): String;
var
  req, resp : TJsonObject;
  exp : TDateTime;
  days : integer;
begin
  result := 'Create SHL context';
  req := TJsonParser.parse(request.PostStream);
  try
    if (req.str['password'] = FPassword) then
    begin
      days := req.int['days'];
      Logging.log(inttostr(days));
      exp := now + days;
      resp := TJsonObject.create;
      try
        resp.str['uuid'] := NewGuidId;
        resp.str['pword'] := NewGuidId;
        resp.str['link'] := 'http://'+common.host+PathWithSlash+resp.str['uuid'];

        c.SQL := 'Insert into SHL (uuid, pword, expiry, mimetype) values (:u, :p, :e, :m)';
        c.prepare;
        c.BindString('u', resp.str['uuid']);
        c.BindString('p', resp.str['pword']);
        c.BindTimeStamp('e', DateTimeToTS(exp));  
        c.BindString('m', req.str['mimetype']);
        c.execute;
        c.terminate;
        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        response.ContentText := TJSONWriter.writeObjectStr(resp, true);  
        c.sql := 'delete from SHL where expiry < :e';
        c.prepare;
        c.BindTimeStamp('e', DateTimeToTS(now));
        c.execute;
        c.terminate;
      finally
        resp.free;
      end;
    end
    else
      raise ERestfulException.create('processCreate', 404, itSecurity, 'Password Failure', nil);
  finally
    req.free;
  end;
end;

function TSHLWebServer.processUpload(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; c : TFDBConnection): String;
var
  p : THTTPParameters;
  bytes : TBytes;
begin
  result := 'upload SHL content';
  p := THTTPParameters.create(request.QueryParams, true);
  try
    bytes := StreamToBytes(request.PostStream);
    if (p.has('uuid') and p.has('pword')) then
    begin
      c.sql := 'select pword from SHL where uuid = '''+SQLWrapString(p['uuid'])+'''';
      c.Prepare;
      c.Execute;
      if not c.FetchNext then
        raise ERestfulException.create('processCreate', 404, itSecurity, 'uuid  "'+p['uuid']+'" not found', nil);
      if p['pword'] <> c.ColStringByName['pword'] then
        raise ERestfulException.create('processCreate', 404, itSecurity, 'password failure', nil);
      c.terminate;
      c.SQL := 'update SHL set blob = :b where uuid = '''+SQLWrapString(p['uuid'])+'''';
      c.prepare;
      c.BindBlob('b', bytes);
      c.Execute;
      c.terminate;
      response.ResponseNo := 200;
      response.ResponseText := 'OK';
      response.ContentText := '{ "msg": "OK" }';
    end
    else
      raise ERestfulException.create('processCreate', 404, itSecurity, 'uuid and/or pword not found', nil);
  finally
    p.free;
  end;
end;

constructor TSHLWebServer.Create(code, path: String; common: TFHIRWebServerCommon);
begin
  inherited;
end;

destructor TSHLWebServer.Destroy;
begin
  FDB.Free;
  inherited Destroy;
end;

function TSHLWebServer.link: TSHLWebServer;
begin
  result := TSHLWebServer(inherited link);
end;

function TSHLWebServer.description: String;
begin
  result := 'SHL services';
end;

function TSHLWebServer.logId: string;
begin
  result := 'SHL';
end;

function TSHLWebServer.PlainRequest(AContext: TIdContext; ip: String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; tt: TFslTimeTracker): String;
var
  c : TFDBConnection;
begin
  response.CustomHeaders.Add('Access-Control-Allow-Origin: *');
  response.CustomHeaders.Add('Access-Control-Expose-Headers: Content-Location, Location');
  response.CustomHeaders.Add('Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE');

  c := FDB.GetConnection('processCreate');
  try
    if (request.CommandType = hcPOST) and (request.Document = PathWithSlash+'create') then
      result := processCreate(request, response, c)
    else if (request.CommandType = hcPOST) and (request.Document = PathWithSlash+'upload') then
      result := processUpload(request, response, c)
    else if (request.CommandType = hcGET) and (request.Document.length > PathWithSlash.length) then
    begin
      c.sql := 'select * from SHL where uuid = :u';
      c.prepare;
      c.BindString('u', request.Document.subString(PathWithSlash.length));
      c.execute;
      if c.FetchNext then
      begin
        response.ResponseNo := 200;
        response.ResponseText:= 'OK';
        response.ContentStream := TBytesStream.create(c.GetColBlobByName('blob'));
        response.ContentType := c.GetColStringByName('mimetype');
        c.Terminate;
      end
      else
      begin
        c.Terminate;
        raise EFslException.create(request.Command+' '+request.Document+' not handled');
      end;
    end
    else if (request.Document = PathWithSlash) then
    begin   
      c.sql := 'select count(*) from SHL';
      c.prepare;
      c.execute;
      c.FetchNext;
      begin
        response.ResponseNo := 200;
        response.ResponseText := 'OK';
        response.ContentText := '{ "shlcount" : "'+c.ColString[1]+'" }';
        response.ContentType := 'application/json';
        c.Terminate;
      end;
    end
    else
    begin
      raise EFslException.create(request.Command+' '+request.Document+' not handled');
    end;
    c.Release;
  except
    on e : Exception do
    begin
      c.Error(e);
      raise;
    end;
  end;
end;

function TSHLWebServer.SecureRequest(AContext: TIdContext; ip: String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert: TIdOpenSSLX509; id: String; tt: TFslTimeTracker): String;
begin
  result := PlainRequest(AContext, ip, request, response, id, tt);
end;

{ TSHLWebEndPoint }

procedure TSHLWebEndPoint.checkDatabase;
var
  c : TFDBConnection;
  m : TFDBMetaData;
  t : TFDBTable;
begin
  c := Database.GetConnection('CheckDatabase');
  try
    m := c.FetchMetaData;
    try
      if not (m.HasTable('SHL')) then
      begin
        c.StartTransact;
        try
          c.ExecSQL('CREATE TABLE Config( '+#13#10+
               ' ConfigKey '+DBKeyType(c.owner.platform)+' '+ColCanBeNull(c.owner.platform, False)+',  '+#13#10+
               ' Value nchar(200) '+ColCanBeNull(c.owner.platform, False)+') '+CreateTableInfo(c.owner.platform));
          c.ExecSQL('Create INDEX SK_Config_ConfigKey ON Config (ConfigKey)');
          c.ExecSQL('Insert into Config (ConfigKey, Value) values (1, ''1'')'); // version
          c.ExecSQL('CREATE TABLE SHL ( '+#13#10+
               ' uuid nchar(40) '+ColCanBeNull(c.owner.platform, False)+', '+
               ' pword nchar(40) '+ColCanBeNull(c.owner.platform, False)+', '+
               ' mimetype nchar(60) '+ColCanBeNull(c.owner.platform, False)+', '+
               ' expiry '+DBDateTimeType(c.owner.platform)+' '+ColCanBeNull(c.owner.platform, False)+', '+
               ' blob '+DBBlobType(c.owner.platform)+' '+ColCanBeNull(c.owner.platform, true)+') '+
               CreateTableInfo(c.owner.platform));
          c.ExecSQL('Create INDEX SK_SHL_UUID ON SHL (uuid)');
          c.Commit;
        except
          on e:exception do
          begin
            Logging.log(e.message);
            c.Rollback;
            recordStack(e);
            raise;
          end;
        end;
      end
      else
      begin
      end;
    finally
      m.free;
    end;
    c.Release;
  except
    on e: Exception do
      c.Error(e);
  end;
end;

constructor TSHLWebEndPoint.Create(config: TFHIRServerConfigSection; settings: TFHIRServerSettings; i18n: TI18nSupport; db : TFDBManager);
begin
  inherited Create(config, settings, db, nil, nil, i18n);
  checkDatabase;
  FPassword := config['password'].value;
end;

destructor TSHLWebEndPoint.Destroy;
begin
  inherited Destroy;
end;

function TSHLWebEndPoint.summary: String;
begin
  result := 'SHL Services';
end;

function TSHLWebEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  Result := inherited makeWebEndPoint(common);
  FSHLServer := TSHLWebServer.Create(config.name, config['path'].value, common);
  FSHLServer.DB := Database.Link;
  FSHLServer.FPassword := FPassword;
  WebEndPoint := FSHLServer;
  result := FSHLServer.link;
end;

function TSHLWebEndPoint.cacheSize(magic: integer): UInt64;
begin
  Result := inherited cacheSize(magic);
end;

procedure TSHLWebEndPoint.recordStats(rec: TStatusRecord);
begin
  inherited recordStats(rec);
end;

procedure TSHLWebEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited getCacheInfo(ci);
end;

end.

