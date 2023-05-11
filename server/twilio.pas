unit twilio;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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
  IdContext, IdCustomHTTPServer,
  fsl_base, fsl_utilities, fsl_stream, fsl_threads, fsl_json,
  fsl_http,
  fdb_manager;


type
  TTwilioServer = class (TFslObject)
  private
    FLock : TFslLock;
    FDb : TFDBManager;
    FKey : Integer;
    FResponse : String;

    function processTwilioPost(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
    function processTwilioGet(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
  public
    Constructor Create(Db : TFDBManager; response : String);
    destructor Destroy; override;

    procedure sweep;
    function process(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
  end;

implementation

{ TTwilioServer }

constructor TTwilioServer.Create(Db : TFDBManager; response : String);
begin
  Inherited Create;
  FResponse := response;
  if FResponse = '' then
    FResponse := 'Thanks. Working...';
  FLock := TFslLock.Create('Twilio');
  FDb := db;
  FKey := FDB.countSql('Select Max(TwilioKey) from Twilio', 'twilio');
end;

destructor TTwilioServer.Destroy;
begin
  FDB.Free;
  FLock.Free;
  inherited;
end;

function TTwilioServer.process(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
begin
  if request.CommandType = hcPOST then
    result := processTwilioPost(request, response)
  else
    result := processTwilioGet(request, response);
end;

function TTwilioServer.processTwilioGet(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
var
  pm : THTTPParameters;
  a : String;
  json, obj : TJsonObject;
  arr : TJsonArray;
  conn : TFDBConnection;
begin
  pm := THTTPParameters.Create(request.UnparsedParams);
  try
    a := pm['AccountSid'];
    result := 'Twillio get '+a;
    json := TJsonObject.Create;
    try
      arr := json.forceArr['messages'];
      conn := FDB.getConnection('twilio');
      try
        conn.sql := 'Select SourceNum, CreatedDate, MsgBody from Twilio where AccountId = :a and Status = 1';
        conn.Prepare;
        conn.BindString('a', a);
        conn.Execute;
        while (conn.fetchNext) do
        begin
          obj := TJsonObject.Create;
          arr.add(obj);
          obj.vStr['from'] := conn.ColStringByName['SourceNum'];
          obj.vStr['date'] := conn.ColStringByName['CreatedDate'];
          obj.vStr['body'] := conn.ColBlobAsStringByName['MsgBody'];
        end;
        conn.Terminate;
        conn.sql := 'Update Twilio set Status = 2, DownloadedDate = getdate() where AccountId = :a';
        conn.Prepare;
        conn.BindString('a', a);
        conn.Execute;
        conn.Terminate;
        conn.release;
      except
        on e : Exception do
        begin
          conn.error(e);
          raise;
        end;
      end;
      response.ResponseNo := 200;
      response.ResponseText := 'OK';
      response.ContentType := 'application/json';
      response.ContentText := TJsonWriterDirect.writeObjectStr(json);
    finally
      json.Free;
    end;
  finally
    pm.Free;
  end;

end;

function TTwilioServer.processTwilioPost(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
var
  pm : THTTPParameters;
  tk : integer;
  a, f, b : String;
  conn : TFDBConnection;
begin
  FLock.Lock;
  try
    Inc(FKey);
    tk := FKey;
  finally
    FLock.Unlock;
  end;

  pm := THTTPParameters.Create(request.UnparsedParams);
  try
    a := pm['AccountSid'];
    result := 'Twillio POST '+a;
    f := pm['From'];
    b := pm['Body'];
    conn := FDB.getConnection('twilio');
    try
      conn.sql := 'Insert into Twilio (TwilioKey, AccountId, Status, SourceNum, CreatedDate, MsgBody) values (:k, :a, 1, :f, getDate(), :b)';
      conn.Prepare;
      conn.BindInteger('k', tk);
      conn.BindString('a', a);
      conn.BindString('f', f);
      conn.BindBlobFromString('b', b);
      conn.Execute;
      conn.Terminate;
      conn.release;
    except
      on e : Exception do
      begin
        conn.error(e);
        raise;
      end;
    end;
    response.ResponseNo := 200;
    response.ResponseText := 'OK';
    response.ContentType := 'application/xml';
    response.ContentText := '<?xml version="1.0" encoding="UTF-8" ?><Response><Message>'+FResponse+'</Message></Response>';
  finally
    pm.Free;
  end;
end;

procedure TTwilioServer.sweep;
var
  conn : TFDBConnection;
begin
  conn := FDB.getConnection('twilio');
  try
    conn.ExecSQL('Delete from Twilio where DownloadedDate < DATEADD(day, -2, GETDATE())');
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

end.
