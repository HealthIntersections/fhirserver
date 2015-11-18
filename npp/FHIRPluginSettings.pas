unit FHIRPluginSettings;

interface

uses
  SysUtils, Classes,
  AdvObjects, AdvJson,
  SmartOnFhirUtilities;

type
  TFHIRPluginSettings = class
  private
    filename : String;
    json : TJsonObject;
    copy :  TJsonObject;
    procedure Save;
    function GetToolboxVisible: boolean;
    function GetDefinitionsSource: string;
    function GetTerminologyServer: string;
    function GetPath: String;
    function GetNoPathSummary: boolean;

    procedure SetDefinitionsSource(const Value: string);
    procedure SetTerminologyServer(const Value: string);
    procedure SetPath(const Value: String);
    procedure SetToolboxVisible(const Value: boolean);
    procedure SetNoPathSummary(const Value: boolean);

    procedure RegisterKnownServers;
  public
    Constructor Create(folder: String);
    Destructor Destroy; override;

    procedure holdChanges;
    procedure CommitChanges;
    procedure AbandonChanges;

    function SourceFile : String;

    procedure ListServers(items : TStrings);
    procedure registerServer(server : TRegisteredServer);
    function serverInfo(index : integer) : TRegisteredServer;
    function ServerCount : integer;
    procedure DeleteServer(index : integer);
    procedure moveServer(index, delta : integer);

    property ToolboxVisible : boolean read GetToolboxVisible write SetToolboxVisible;
    property TerminologyServer : string read GetTerminologyServer write SetTerminologyServer;
    property DefinitionsSource : string read GetDefinitionsSource write SetDefinitionsSource;
    property Path : String read GetPath write SetPath;
    property NoPathSummary : boolean read GetNoPathSummary write SetNoPathSummary;
  end;

var
  Settings : TFHIRPluginSettings;

implementation

{ TFHIRPluginSettings }

procedure TFHIRPluginSettings.AbandonChanges;
begin
  json.Free;
  json := copy;
  copy := nil;
end;

procedure TFHIRPluginSettings.CommitChanges;
begin
  copy.Free;
  copy := nil;
  Save;
end;

constructor TFHIRPluginSettings.Create(folder: String);
var
  f : TFileStream;
begin
  Inherited Create;
  filename := IncludeTrailingPathDelimiter(folder)+'fhirplugin.json';
  if not FileExists(filename) then
  begin
    json := TJsonObject.Create;
    json.vStr['DefinitionsSource'] := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(HInstance)))+'validation-min.xml.zip';
    json.vStr['TerminologyServer'] := 'http://fhir2.healthintersections.com.au/open';
    RegisterKnownServers;
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

procedure TFHIRPluginSettings.DeleteServer(index: integer);
begin
  json.forceArr['Servers'].remove(index);
end;

destructor TFHIRPluginSettings.Destroy;
begin
  json.Free;
  inherited;
end;

function TFHIRPluginSettings.GetDefinitionsSource: string;
begin
  result := json.vStr['DefinitionsSource'];
end;

function TFHIRPluginSettings.GetNoPathSummary: boolean;
begin
  result := json.bool['NoPathSummary'];
end;

function TFHIRPluginSettings.GetPath: String;
begin
  result := json.vStr['Path'];
end;

function TFHIRPluginSettings.GetTerminologyServer: string;
begin
  result := json.vStr['TerminologyServer'];
end;

function TFHIRPluginSettings.GetToolboxVisible: boolean;
begin
  result := json.bool['Toolbox'];
end;

procedure TFHIRPluginSettings.holdChanges;
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

procedure TFHIRPluginSettings.SetDefinitionsSource(const Value: string);
begin
  json.vStr['DefinitionsSource'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetNoPathSummary(const Value: boolean);
begin
  json.bool['NoPathSummary'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetPath(const Value: String);
begin
  json.vStr['Path'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetTerminologyServer(const Value: string);
begin
  json.vStr['TerminologyServer'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetToolboxVisible(const Value: boolean);
begin
  json.bool['Toolbox'] := Value;
  Save;
end;

function TFHIRPluginSettings.SourceFile: String;
begin
  result := filename;
end;

procedure TFHIRPluginSettings.ListServers(items: TStrings);
var
  arr : TJsonArray;
  i : integer;
begin
  arr := json.forceArr['Servers'];
  for i := 0 to arr.Count - 1 do
    items.add(arr.Obj[i].str['name'] +' ('+arr.Obj[i].str['fhirEndpoint']+')');
end;

procedure TFHIRPluginSettings.moveServer(index, delta: integer);
var
  arr : TJsonArray;
  i : integer;
begin
  arr := json.forceArr['Servers'];
  arr.move(index, delta);
end;

procedure TFHIRPluginSettings.RegisterKnownServers;
var
  server : TRegisteredServer;
begin
  FillChar(server, sizeof(TRegisteredServer), 0);
  server.name := 'Reference Server';
  server.fhirEndpoint := 'http://fhir2.healthintersections.com.au/open';
  registerServer(server);

  server.name := 'Secure Reference Server';
  server.fhirEndpoint := 'https://fhir2.healthintersections.com.au/closed';
  server.SmartOnFHIR := true;
  server.clientid := '458EA027-CDCC-4E89-B103-997965132D0C';
  server.redirectport := 23145;
  server.tokenEndpoint := 'https://authorize-dstu2.smarthealthit.org/token';
  server.authorizeEndpoint := 'https://authorize-dstu2.smarthealthit.org/authorize';
  registerServer(server);
end;

procedure TFHIRPluginSettings.registerServer(server : TRegisteredServer);
var
  arr : TJsonArray;
  i : integer;
  o : TJsonObject;
begin
  arr := json.forceArr['Servers'];
  for i := 0 to arr.Count - 1 do
    if (arr.Obj[i].str['Name'] = server.name) then
      raise exception.Create('Duplicate Server Name '+server.name);
  o := arr.addObject;
  o.vStr['name'] := server.name;
  o.vStr['fhir'] := server.fhirEndpoint;
  o.bool['smart'] := server.SmartOnFHIR;
  if server.SmartOnFHIR then
  begin
    o.vStr['token'] := server.tokenEndpoint;
    o.vStr['authorize'] := server.authorizeEndpoint;
    o.vStr['clientid'] := server.clientid;
    o.vStr['port'] := inttostr(server.redirectport);
    o.vStr['secret'] := server.clientsecret;
  end;
  Save;
end;


procedure TFHIRPluginSettings.Save;
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

function TFHIRPluginSettings.ServerCount: integer;
begin
  result := json.forceArr['Servers'].Count;
end;

function TFHIRPluginSettings.serverInfo(index: integer): TRegisteredServer;
var
  o : TJsonObject;
begin
  FillChar(result, sizeof(TRegisteredServer), 0);
  o := json.forceArr['Servers'].Obj[index];
  result.name := o.vStr['name'];
  result.fhirEndpoint := o.vStr['fhir'];
  result.SmartOnFHIR := o.bool['smart'];
  if result.SmartOnFHIR then
  begin
    result.tokenEndpoint := o.vStr['token'];
    result.authorizeEndpoint := o.vStr['authorize'];
    result.clientid := o.vStr['clientid'];
    result.redirectport := StrToInt(o.vStr['port']);
    result.clientsecret := o.vStr['secret'];
  end;
end;


end.
