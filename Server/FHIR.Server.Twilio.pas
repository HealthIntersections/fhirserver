unit FHIR.Server.Twilio;

interface

uses
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Threads,
  FHIR.Web.Parsers,
  FHIR.DataBase.Manager;


type
  TTwilioServer = class (TFslObject)
  private
    FLock : TFslLock;
    FDb : TFslDBManager;
    FKey : Integer;

    procedure processTwilioPost(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
  public
    Constructor Create(Db : TFslDBManager);
    destructor Destroy; override;

    procedure process(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
  end;

implementation

{ TTwilioServer }

constructor TTwilioServer.Create(Db : TFslDBManager);
begin
  Inherited Create;
  FLock := TFslLock.Create('Twilio');
  FDb := db;
  FDB.connection('twilio', Procedure (conn : TFslDBConnection)
  begin
    FKey := conn.CountSQL('Select Max(TwilioKey) from Twilio');
  end
  );
end;

destructor TTwilioServer.Destroy;
begin
  FDB.Free;
  FLock.Free;
  inherited;
end;

procedure TTwilioServer.process(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
begin
  if request.CommandType = hcPOST then
    processTwilioPost(request, response)
  else
  begin
    raise Exception.Create('Not done yet');
  end;
end;

procedure TTwilioServer.processTwilioPost(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  pm : TParseMap;
  tk : integer;
  a, f, b : String;
begin
  FLock.Lock;
  try
    Inc(FKey);
    tk := FKey;
  finally
    FLock.Unlock;
  end;

  pm := TParseMap.Create(request.UnparsedParams);
  try
    a := pm.GetVar('AccountSid');
    f := pm.GetVar('From');
    b := pm.GetVar('Body');
    FDB.connection('twilio', Procedure (conn : TFslDBConnection)
      begin
        conn.sql := 'Insert into Twilio (TwilioKey, AccountId, Status, Source, Created, Body) values (:k, :a, 1, :f, getDate(), :b)';
        conn.Prepare;
        conn.BindInteger('k', tk);
        conn.BindString('a', a);
        conn.BindString('f', f);
        conn.BindBlobFromString('b', b);
        conn.Execute;
        conn.Terminate;
      end);
    response.ResponseNo := 200;
    response.ResponseText := 'OK';
    response.ContentType := 'application/xml';
    response.ContentText := '<?xml version="1.0" encoding="UTF-8" ?><Response><Message>Your message has been received and is being processed</Message></Response>';
  finally
    pm.Free;
  end;
end;

end.
