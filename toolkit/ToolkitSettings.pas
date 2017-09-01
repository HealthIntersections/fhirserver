unit ToolkitSettings;

interface

uses
  SysUtils, Classes,
  AdvJson,
  SmartOnFHIRUtilities,
  FHIRClientSettings;

const
  DEF_TIMEOUT = 10;
  DEF_SHOWHELP = false;

type
  TFHIRToolkitSettings = class (TFHIRClientSettings)
  private
    function GetTimeout: integer;
    procedure SetTimeout(const Value: integer);
    procedure SetProxy(const Value: String);
    function GetProxy: String;
    function GetShowHelp: boolean;
    procedure SetShowHelp(const Value: boolean);
  protected
    procedure initSettings; virtual;
  public
    function link : TFHIRToolkitSettings; overload;

    property timeout: integer read GetTimeout write SetTimeout;
    property Proxy : String read GetProxy write SetProxy;
    property ShowHelp : boolean read GetShowHelp write SetShowHelp;

    procedure storeValues(name : String; values : TStrings); overload;
    procedure storeValue(section, name, value : String); overload;
    procedure storeValue(section, name : String; value : integer); overload;
    procedure storeValue(section, name : String; value : boolean); overload;

    procedure getValues(name : String; values : TStrings); overload;
    function getValue(section, name, default : String) : String; overload;
    function getValue(section, name : String; default : integer) : integer; overload;
    function getValue(section, name : String; default : boolean) : boolean; overload;
  end;

implementation

{ TFHIRToolkitSettings }

function TFHIRToolkitSettings.GetProxy: String;
var
  o : TJsonObject;
begin
  o := json.vObj['Client'];
  if (o = nil) then
    result := ''
  else
    result := o.vStr['Proxy'];
end;

function TFHIRToolkitSettings.GetShowHelp: boolean;
begin
  result := StrToBoolDef(json.vStr['ShowHelp'], DEF_SHOWHELP);
end;

function TFHIRToolkitSettings.GetTimeout: integer;
var
  o : TJsonObject;
begin
  o := json.vObj['Client'];
  if (o = nil) then
    result := DEF_TIMEOUT
  else
    result := StrToIntDef(o.vStr['Timeout'], DEF_TIMEOUT);
end;

function TFHIRToolkitSettings.getValue(section, name, default: String): String;
var
  o : TJsonObject;
begin
  o := json.forceObj['Memory'];
  o := o.forceObj[section];
  result := o.vStr[name];
  if result = '' then
    result := default;
end;

function TFHIRToolkitSettings.getValue(section, name: String; default: integer): integer;
var
  o : TJsonObject;
begin
  o := json.forceObj['Memory'];
  o := o.forceObj[section];
  result := StrToIntDef(o.vStr[name], default);
end;

function TFHIRToolkitSettings.getValue(section, name: String; default: boolean): boolean;
var
  o : TJsonObject;
begin
  o := json.forceObj['Memory'];
  o := o.forceObj[section];
  result := StrToBoolDef(o.vStr[name], default);
end;

procedure TFHIRToolkitSettings.getValues(name: String; values: TStrings);
var
  a : TJsonArray;
  i : TJsonNode;
begin
  values.Clear;
  a := json.forceArr[name];
  for i in a do
     if i is TJsonString then
       values.add(TJsonString(i).value);
end;

procedure TFHIRToolkitSettings.initSettings;
var
  server : TRegisteredFHIRServer;
begin
  server := TRegisteredFHIRServer.Create;
  try
    server.name := 'FHIR Terminology Server';
    server.fhirEndpoint := 'http://tx.fhir.org/r3';
    registerServer('Terminology', server);
  finally
    server.free;
  end;
end;

function TFHIRToolkitSettings.link: TFHIRToolkitSettings;
begin
  result := TFHIRToolkitSettings(inherited Link);
end;

procedure TFHIRToolkitSettings.SetProxy(const Value: String);
var
  o : TJsonObject;
begin
  o := json.forceObj['Client'];
  o.vStr['Proxy'] := value;
  Save;
end;

procedure TFHIRToolkitSettings.SetShowHelp(const Value: boolean);
begin
  json.vStr['ShowHelp'] := BoolToStr(value);
  Save;

end;

procedure TFHIRToolkitSettings.SetTimeout(const Value: integer);
var
  o : TJsonObject;
begin
  o := json.forceObj['Client'];
  o.vStr['Timeout'] := inttostr(value);
  Save;
end;

procedure TFHIRToolkitSettings.StoreValue(section, name: String; value: boolean);
var
  o : TJsonObject;
begin
  o := json.forceObj['Memory'];
  o := o.forceObj[section];
  o.vStr[name] := BoolToStr(value);
end;

procedure TFHIRToolkitSettings.storeValues(name: String; values: TStrings);
var
  a : TJsonArray;
  s : String;
begin
  a := json.forceArr[name];
  a.clear;
  for s in values do
    a.add(s);
end;

procedure TFHIRToolkitSettings.StoreValue(section, name: String; value: integer);
var
  o : TJsonObject;
begin
  o := json.forceObj['Memory'];
  o := o.forceObj[section];
  o.vStr[name] := inttostr(value);
end;

procedure TFHIRToolkitSettings.StoreValue(section, name, value: String);
var
  o : TJsonObject;
begin
  o := json.forceObj['Memory'];
  o := o.forceObj[section];
  o.vStr[name] := value;
end;

end.

// FIni.ReadInteger('HTTP', 'timeout', 5)

FIni.ReadString('HTTP', 'proxy', ''));
