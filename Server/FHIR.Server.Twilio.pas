unit FHIR.Server.Twilio;

interface

uses
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Threads, FHIR.Support.Json,
  FHIR.Web.Parsers,
  FHIR.DataBase.Manager;


type
  TTwilioServer = class (TFslObject)
  private
    FLock : TFslLock;
    FDb : TFslDBManager;
    FKey : Integer;
    FResponse : String;

    function processTwilioPost(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
    function processTwilioGet(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
  public
    Constructor Create(Db : TFslDBManager; response : String);
    destructor Destroy; override;

    procedure sweep;
    function process(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
  end;

implementation

{ TTwilioServer }

constructor TTwilioServer.Create(Db : TFslDBManager; response : String);
var
  conn : TFslDBConnection;
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
  conn : TFslDBConnection;
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
  conn : TFslDBConnection;
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
  conn : TFslDBConnection;
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
