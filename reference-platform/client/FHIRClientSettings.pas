unit FHIRClientSettings;

interface

uses
  SysUtils, Classes,
  AdvObjects, AdvJson,
  FHIRTypes, SmartOnFhirUtilities, CDSHooksUtilities;

type
  TFHIRClientSettings = class (TAdvObject)
  private
    FFilename : String;
    copy :  TJsonObject;

    procedure RegisterKnownServers;
  protected
    json : TJsonObject;
    FShuttingDown: boolean;
    procedure initSettings; virtual;
  public
    Constructor Create(filename: String);
    Destructor Destroy; override;
    procedure Save;

    procedure holdChanges;
    procedure CommitChanges;
    procedure AbandonChanges;
    property ShuttingDown : boolean read FShuttingDown write FShuttingDown;

    property FileName : String read FFilename;

    procedure ListServers(purpose : String; items : TStrings);
    procedure registerServer(purpose : String; server : TRegisteredFHIRServer);
    procedure updateServerInfo(purpose : String; index : integer; server : TRegisteredFHIRServer);
    function serverInfo(purpose : String; index : integer) : TRegisteredFHIRServer;
    function ServerCount(purpose : String) : integer;
    procedure DeleteServer(purpose : String; index : integer);
    procedure moveServer(purpose : String; index, delta : integer);
    function serverAddress(purpose : String; index : integer) : String;
  end;

implementation

function srvname(purpose : String): String;
begin
  if purpose = '' then
    result := 'Servers'
  else
    result := 'Servers-'+purpose;
end;


{ TFHIRClientSettings }

constructor TFHIRClientSettings.Create(filename: String);
var
  f : TFileStream;
begin
  Inherited Create;
  Ffilename := filename;
  if not FileExists(filename) then
  begin
    json := TJsonObject.Create;
    initSettings;
  end
  else
  begin
    f := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
    try
      json := TJSONParser.Parse(f)
    finally
      f.Free;
    end;
  end;
end;

destructor TFHIRClientSettings.Destroy;
begin
  json.Free;
  inherited;
end;

procedure TFHIRClientSettings.AbandonChanges;
begin
  json.Free;
  json := copy;
  copy := nil;
end;

procedure TFHIRClientSettings.CommitChanges;
begin
  copy.Free;
  copy := nil;
  Save;
end;

procedure TFHIRClientSettings.DeleteServer(purpose : String; index: integer);
begin
  json.forceArr[srvname(purpose)].remove(index);
end;


procedure TFHIRClientSettings.holdChanges;
var
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    copy := TJSONParser.Parse(f)
  finally
    f.Free;
  end;
end;

procedure TFHIRClientSettings.initSettings;
begin
  RegisterKnownServers;
end;


procedure TFHIRClientSettings.updateServerInfo(purpose : String; index: integer; server: TRegisteredFHIRServer);
var
  arr : TJsonArray;
  o : TJsonObject;
  i : integer;
  c : TFHIRCoding;
begin
  arr := json.arr[srvname(purpose)];

  for i := 0 to arr.Count - 1 do
    if i <> index then
      if (arr.Obj[i].str['Name'] = server.name) then
        raise exception.Create('Duplicate Server Name '+server.name);

  o := arr.Obj[index];
  server.writeToJson(o);
  Save;
end;

procedure TFHIRClientSettings.ListServers(purpose : String; items: TStrings);
var
  arr : TJsonArray;
  i : integer;
begin
  items.Clear;
  arr := json.forceArr[srvname(purpose)];
  for i := 0 to arr.Count - 1 do
    items.add(arr.Obj[i].str['name'] +' ('+arr.Obj[i].str['fhir']+')');
end;

procedure TFHIRClientSettings.moveServer(purpose : String; index, delta: integer);
var
  arr : TJsonArray;
  i : integer;
begin
  arr := json.forceArr[srvname(purpose)];
  arr.move(index, delta);
end;

procedure TFHIRClientSettings.RegisterKnownServers;
var
  server : TRegisteredFHIRServer;
begin
  server := TRegisteredFHIRServer.Create;
  try
    server.name := 'Reference Server';
    server.fhirEndpoint := 'http://test.fhir.org/r3';
    server.addCdsHook('Get Terminology Information', TCDSHooks.codeView);
    server.addCdsHook('Get Identifier Information', TCDSHooks.identifierView);
    server.addCdsHook('Fetch Patient Alerts', TCDSHooks.patientView).preFetch.Add('Patient/{{Patient.id}}');
    server.autoUseHooks := true;
    registerServer('', server);

    server.clear;
    server.name := 'Secure Reference Server';
    server.fhirEndpoint := 'https://test.fhir.org/r3';
    server.SmartAppLaunchMode := salmOAuthClient;
    server.clientid := '458EA027-CDCC-4E89-B103-997965132D0C';
    server.redirectport := 23145;
    server.tokenEndpoint := 'https://authorize-dstu2.smarthealthit.org/token';
    server.authorizeEndpoint := 'https://authorize-dstu2.smarthealthit.org/authorize';
    server.addCdsHook('Get Terminology Information', TCDSHooks.codeView);
    server.addCdsHook('Get Identifier Information', TCDSHooks.identifierView);
    server.addCdsHook('Fetch Patient Alerts', TCDSHooks.patientView).preFetch.Add('Patient/{{Patient.id}}');
    registerServer('', server);
  finally
    server.free;
  end;
end;

procedure TFHIRClientSettings.registerServer(purpose : String; server : TRegisteredFHIRServer);
var
  arr : TJsonArray;
  i : integer;
  o : TJsonObject;
  c : TFHIRCoding;
begin
  arr := json.forceArr[srvname(purpose)];
  for i := 0 to arr.Count - 1 do
    if (arr.Obj[i].str['Name'] = server.name) then
      raise exception.Create('Duplicate Server Name '+server.name);
  o := arr.addObject;
  server.writeToJson(o);
  Save;
end;


procedure TFHIRClientSettings.Save;
var
  f : TFileStream;
begin
  if copy <> nil then
    exit;

  f := TFileStream.Create(filename, fmCreate);
  try
    TJSONWriter.writeObject(f, json, true);
  finally
    f.Free;
  end;
end;

function TFHIRClientSettings.serverAddress(purpose: String; index: integer): String;
var
  o : TJsonObject;
begin
  o := json.forceArr[srvname(purpose)].Obj[index];
  result := o.vStr['fhir'];
end;

function TFHIRClientSettings.ServerCount(purpose : String): integer;
begin
  result := json.forceArr[srvname(purpose)].Count;
end;

function TFHIRClientSettings.serverInfo(purpose : String; index: integer): TRegisteredFHIRServer;
var
  o : TJsonObject;
begin
  result := TRegisteredFHIRServer.Create;
  try
    o := json.forceArr[srvname(purpose)].Obj[index];
    result.readFromJson(o);
    result.Link;
  finally
    result.Free;
  end;
end;



end.

