unit FHIRPluginSettings;

interface

uses
  Windows, SysUtils, Classes,
  AdvObjects, AdvJson,
  FHIRTypes, SmartOnFhirUtilities, CDSHooksUtilities;

const
  DEF_ActivePage = 0;
  DEF_BreaksWidth = 200;
  DEF_SourceHeight = 70;
  DEF_Width = 720;
  DEF_Height = 300;

type
  TDefinitionsVersion = (defV2, defV3);

  TFHIRPluginSettings = class
  private
    filename : String;
    json : TJsonObject;
    copy :  TJsonObject;
    FShuttingDown: boolean;
    procedure Save;
    function GetToolboxVisible: boolean;
    function GetVisualiserVisible: boolean;
    function GetDefinitionsVersion: TDefinitionsVersion;
    function GetAdditionalDefinitions: string;
    function GetTerminologyServer: string;
    function GetPath: String;
    function GetNoPathSummary: boolean;
    function GetNoValidationSummary: boolean;
    function GetDebuggerActivePage: integer;
    function GetDebuggerBreaksWidth: integer;
    function GetDebuggerHeight: integer;
    function GetDebuggerSourceHeight: integer;
    function GetDebuggerWidth: integer;
    function GetDebuggerBreaks: String;
    function GetBackgroundValidation: boolean;
    function GetNoWelcomeScreen: boolean;
    function GetBuildPrompt: String;

    procedure SetDefinitionsVersion(const Value: TDefinitionsVersion);
    procedure SetAdditionalDefinitions(const Value: string);
    procedure SetTerminologyServer(const Value: string);
    procedure SetPath(const Value: String);
    procedure SetToolboxVisible(const Value: boolean);
    procedure SetVisualiserVisible(const Value: boolean);
    procedure SetNoPathSummary(const Value: boolean);
    procedure SetNoValidationSummary(const Value: boolean);
    procedure SetDebuggerActivePage(const Value: integer);
    procedure SetDebuggerBreaksWidth(const Value: integer);
    procedure SetDebuggerHeight(const Value: integer);
    procedure SetDebuggerSourceHeight(const Value: integer);
    procedure SetDebuggerWidth(const Value: integer);
    procedure SetDebuggerBreaks(const Value: String);
    procedure SetBackgroundValidation(const Value: boolean);
    procedure SetNoWelcomeScreen(const Value: boolean);
    procedure SetBuildPrompt(const Value: String);

    procedure RegisterKnownServers;
  public
    Constructor Create(folder: String);
    Destructor Destroy; override;

    procedure holdChanges;
    procedure CommitChanges;
    procedure AbandonChanges;
    property ShuttingDown : boolean read FShuttingDown write FShuttingDown;

    function SourceFile : String;

    procedure ListServers(items : TStrings);
    procedure registerServer(server : TRegisteredFHIRServer);
    procedure updateServerInfo(index : integer; server : TRegisteredFHIRServer);
    function serverInfo(index : integer) : TRegisteredFHIRServer;
    function ServerCount : integer;
    procedure DeleteServer(index : integer);
    procedure moveServer(index, delta : integer);

    function FontName : String;
    Function FontSize : integer;

    property ToolboxVisible : boolean read GetToolboxVisible write SetToolboxVisible;
    property VisualiserVisible : boolean read GetVisualiserVisible write SetVisualiserVisible;
    property TerminologyServer : string read GetTerminologyServer write SetTerminologyServer;
    property DefinitionsVersion : TDefinitionsVersion read GetDefinitionsVersion write SetDefinitionsVersion;
    property AdditionalDefinitions : string read GetAdditionalDefinitions write SetAdditionalDefinitions;
    property Path : String read GetPath write SetPath;
    property NoPathSummary : boolean read GetNoPathSummary write SetNoPathSummary;
    property NoValidationSummary : boolean read GetNoValidationSummary write SetNoValidationSummary;
    property NoWelcomeScreen : boolean read GetNoWelcomeScreen write SetNoWelcomeScreen;
    property BackgroundValidation : boolean read GetBackgroundValidation write SetBackgroundValidation;
    property BuildPrompt : String read GetBuildPrompt write SetBuildPrompt;

    property DebuggerHeight : integer read GetDebuggerHeight write SetDebuggerHeight;
    property DebuggerWidth : integer read GetDebuggerWidth write SetDebuggerWidth;
    property DebuggerSourceHeight : integer read GetDebuggerSourceHeight write SetDebuggerSourceHeight;
    property DebuggerBreaksWidth : integer read GetDebuggerBreaksWidth write SetDebuggerBreaksWidth;
    property DebuggerActivePage : integer read GetDebuggerActivePage write SetDebuggerActivePage;
    property DebuggerBreaks : String read GetDebuggerBreaks write SetDebuggerBreaks;
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
    json.vStr['DefinitionsVersion'] := 'r3';
    json.vStr['TerminologyServer'] := 'http://fhir3.healthintersections.com.au/open';
    json.bool['BackgroundValidation'] := true;

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

function TFHIRPluginSettings.FontName: String;
begin
  result := 'Courier New';
end;

function TFHIRPluginSettings.FontSize: integer;
begin
  result := 10;
end;

function TFHIRPluginSettings.GetAdditionalDefinitions: string;
begin
  result := json.vStr['AdditionalDefinitions'];
end;

function TFHIRPluginSettings.GetBackgroundValidation: boolean;
begin
  result := json.bool['BackgroundValidation'];
end;

function TFHIRPluginSettings.GetBuildPrompt: String;
begin
  result := json.vStr['BuildPrompt'];
end;

function TFHIRPluginSettings.GetDebuggerActivePage: integer;
var
  o : TJsonObject;
begin
  o := json.vObj['Debugger'];
  if (o = nil) then
    result := DEF_ActivePage
  else
    result := StrToIntDef(o.vStr['ActivePage'], DEF_ActivePage);
end;

function TFHIRPluginSettings.GetDebuggerBreaks: String;
var
  o : TJsonObject;
begin
  o := json.vObj['Debugger'];
  if (o = nil) then
    result := ''
  else
    result := o.vStr['Breaks'];
end;

function TFHIRPluginSettings.GetDebuggerBreaksWidth: integer;
var
  o : TJsonObject;
begin
  o := json.vObj['Debugger'];
  if (o = nil) then
    result := DEF_BreaksWidth
  else
    result := StrToIntDef(o.vStr['BreaksWidth'], DEF_BreaksWidth);
end;

function TFHIRPluginSettings.GetDebuggerHeight: integer;
var
  o : TJsonObject;
begin
  o := json.vObj['Debugger'];
  if (o = nil) then
    result := DEF_Height
  else
    result := StrToIntDef(o.vStr['Height'], DEF_Height);
end;

function TFHIRPluginSettings.GetDebuggerSourceHeight: integer;
var
  o : TJsonObject;
begin
  o := json.vObj['Debugger'];
  if (o = nil) then
    result := DEF_SourceHeight
  else
    result := StrToIntDef(o.vStr['SourceHeight'], DEF_SourceHeight);
end;

function TFHIRPluginSettings.GetDebuggerWidth: integer;
var
  o : TJsonObject;
begin
  o := json.vObj['Debugger'];
  if (o = nil) then
    result := DEF_Width
  else
    result := StrToIntDef(o.vStr['Width'], DEF_Width);
end;

function TFHIRPluginSettings.GetDefinitionsVersion: TDefinitionsVersion;
var
  s : string;
begin
  s := json.vStr['DefinitionsVersion'];
  if (s = 'r2') then
    result := defV2
  else
    result := defv3;
end;

function TFHIRPluginSettings.GetNoPathSummary: boolean;
begin
  result := json.bool['NoPathSummary'];
end;

function TFHIRPluginSettings.GetNoValidationSummary: boolean;
begin
  result := json.bool['NoValidationSummary'];
end;

function TFHIRPluginSettings.GetNoWelcomeScreen: boolean;
begin
  result := json.bool['NoWelcomeScreen'];
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

function TFHIRPluginSettings.GetVisualiserVisible: boolean;
begin
  result := json.bool['Visualiser'];
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

procedure TFHIRPluginSettings.SetAdditionalDefinitions(const Value: string);
begin
  if FShuttingDown then exit;
  json.vStr['AdditionalDefinitions'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetBackgroundValidation(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['BackgroundValidation'] := value;
end;

procedure TFHIRPluginSettings.SetBuildPrompt(const Value: String);
begin
  if FShuttingDown then exit;
  json.vStr['BuildPrompt'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetDebuggerActivePage(const Value: integer);
var
  o : TJsonObject;
begin
  if FShuttingDown then exit;
  o := json.forceObj['Debugger'];
  o.vStr['ActivePage'] := inttostr(value);
  Save;
end;

procedure TFHIRPluginSettings.SetDebuggerBreaks(const Value: String);
var
  o : TJsonObject;
begin
  if FShuttingDown then exit;
  o := json.forceObj['Debugger'];
  o.vStr['Breaks'] := value;
  Save;
end;

procedure TFHIRPluginSettings.SetDebuggerBreaksWidth(const Value: integer);
var
  o : TJsonObject;
begin
  if FShuttingDown then exit;
  o := json.forceObj['Debugger'];
  o.vStr['BreaksWidth'] := inttostr(value);
  Save;
end;

procedure TFHIRPluginSettings.SetDebuggerHeight(const Value: integer);
var
  o : TJsonObject;
begin
  if FShuttingDown then exit;
  o := json.forceObj['Debugger'];
  o.vStr['Height'] := inttostr(value);
  Save;
end;

procedure TFHIRPluginSettings.SetDebuggerSourceHeight(const Value: integer);
var
  o : TJsonObject;
begin
  if FShuttingDown then exit;
  o := json.forceObj['Debugger'];
  o.vStr['SourceHeight'] := inttostr(value);
  Save;
end;

procedure TFHIRPluginSettings.SetDebuggerWidth(const Value: integer);
var
  o : TJsonObject;
begin
  if FShuttingDown then exit;
  o := json.forceObj['Debugger'];
  o.vStr['Width'] := inttostr(value);
  Save;
end;

procedure TFHIRPluginSettings.SetDefinitionsVersion(const Value: TDefinitionsVersion);
begin
  if value = defV2 then
    json.vStr['DefinitionsVersion'] := 'r2'
  else
    json.vStr['DefinitionsVersion'] := 'r3';
  Save;
end;

procedure TFHIRPluginSettings.SetNoPathSummary(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['NoPathSummary'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetNoValidationSummary(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['NoValidationSummary'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetNoWelcomeScreen(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['NoWelcomeScreen'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetPath(const Value: String);
begin
  if FShuttingDown then exit;
  json.vStr['Path'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetTerminologyServer(const Value: string);
begin
  if FShuttingDown then exit;
  json.vStr['TerminologyServer'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetToolboxVisible(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['Toolbox'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetVisualiserVisible(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['Visualiser'] := Value;
  Save;
end;

function TFHIRPluginSettings.SourceFile: String;
begin
  result := filename;
end;

procedure TFHIRPluginSettings.updateServerInfo(index: integer; server: TRegisteredFHIRServer);
var
  arr : TJsonArray;
  o : TJsonObject;
  i : integer;
  c : TFHIRCoding;
begin
  arr := json.arr['Servers'];

  for i := 0 to arr.Count - 1 do
    if i <> index then
      if (arr.Obj[i].str['Name'] = server.name) then
        raise exception.Create('Duplicate Server Name '+server.name);

  o := arr.Obj[index];
  server.writeToJson(o);
  Save;
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
  server : TRegisteredFHIRServer;
begin
  server := TRegisteredFHIRServer.Create;
  try
    server.name := 'Reference Server';
    server.fhirEndpoint := 'http://fhir3.healthintersections.com.au/open';
    server.addCdsHook('Get Terminology Information', TCDSHooks.codeView);
    server.addCdsHook('Get Identifier Information', TCDSHooks.identifierView);
    server.addCdsHook('Fetch Patient Alerts', TCDSHooks.patientView).preFetch.Add('Patient/{{Patient.id}}');
    server.autoUseHooks := true;
    registerServer(server);

    server.clear;
    server.name := 'Secure Reference Server';
    server.fhirEndpoint := 'https://fhir3.healthintersections.com.au/closed';
    server.SmartOnFHIR := true;
    server.clientid := '458EA027-CDCC-4E89-B103-997965132D0C';
    server.redirectport := 23145;
    server.tokenEndpoint := 'https://authorize-dstu2.smarthealthit.org/token';
    server.authorizeEndpoint := 'https://authorize-dstu2.smarthealthit.org/authorize';
    server.addCdsHook('Get Terminology Information', TCDSHooks.codeView);
    server.addCdsHook('Get Identifier Information', TCDSHooks.identifierView);
    server.addCdsHook('Fetch Patient Alerts', TCDSHooks.patientView).preFetch.Add('Patient/{{Patient.id}}');
    registerServer(server);
  finally
    server.free;
  end;
end;

procedure TFHIRPluginSettings.registerServer(server : TRegisteredFHIRServer);
var
  arr : TJsonArray;
  i : integer;
  o : TJsonObject;
  c : TFHIRCoding;
begin
  arr := json.forceArr['Servers'];
  for i := 0 to arr.Count - 1 do
    if (arr.Obj[i].str['Name'] = server.name) then
      raise exception.Create('Duplicate Server Name '+server.name);
  o := arr.addObject;
  server.writeToJson(o);
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

function TFHIRPluginSettings.serverInfo(index: integer): TRegisteredFHIRServer;
var
  o : TJsonObject;
begin
  result := TRegisteredFHIRServer.Create;
  try
    o := json.forceArr['Servers'].Obj[index];
    result.readFromJson(o);
    result.Link;
  finally
    result.Free;
  end;
end;


end.
