unit ToolkitSettings;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
interface

uses
  SysUtils, Classes,
  fsl_json, fsl_utilities,
  fsl_npm_cache,
  fhir_factory,
  fhir_oauth, fhir_client_registry;

const
  DEF_TIMEOUT = 10;
  DEF_SHOWHELP = false;
  DEF_CHECKUPGRADES = true;

type
  TFHIRToolkitSettings = class (TFHIRClientRegistry)
  private
    FCacheManager: TFHIRPackageManager;
    FVersions: TFHIRVersionFactories;
    function GetTimeout: integer;
    procedure SetTimeout(const Value: integer);
    procedure SetProxy(const Value: String);
    function GetProxy: String;
    function GetShowHelp: boolean;
    procedure SetShowHelp(const Value: boolean);
    function GetCheckForUpgradesOnStart: boolean;
    procedure SetCheckForUpgradesOnStart(const Value: boolean);
    function GetRegistryPassword: String;
    function GetRegistryUsername: String;
    procedure SetRegistryPassword(const Value: String);
    procedure SetRegistryUsername(const Value: String);
    procedure SetCacheManager(const Value: TFHIRPackageManager);
    procedure SetVersions(const Value: TFHIRVersionFactories);
  protected
    procedure initSettings; virtual;
  public
    destructor Destroy; override;
    function link : TFHIRToolkitSettings; overload;

    property timeout: integer read GetTimeout write SetTimeout;
    property Proxy : String read GetProxy write SetProxy;
    property ShowHelp : boolean read GetShowHelp write SetShowHelp;
    property CheckForUpgradesOnStart : boolean read GetCheckForUpgradesOnStart write SetCheckForUpgradesOnStart;
    property RegistryUsername : String read GetRegistryUsername write SetRegistryUsername;
    property RegistryPassword : String read GetRegistryPassword write SetRegistryPassword;

    procedure storeValues(name : String; values : TStrings); overload;
    procedure storeValue(section, name, value : String); overload;
    procedure storeValue(section, name : String; value : integer); overload;
    procedure storeValue(section, name : String; value : boolean); overload;

    procedure getValues(name : String; values : TStrings); overload;
    function getValue(section, name, default : String) : String; overload;
    function getValue(section, name : String; default : integer) : integer; overload;
    function getValue(section, name : String; default : boolean) : boolean; overload;

    Property CacheManager : TFHIRPackageManager read FCacheManager write SetCacheManager;
    Property Versions : TFHIRVersionFactories read FVersions write SetVersions;
  end;

implementation

{ TFHIRToolkitSettings }

destructor TFHIRToolkitSettings.Destroy;
begin
  FVersions.Free;
  FCacheManager.free;
  inherited;
end;

function TFHIRToolkitSettings.GetCheckForUpgradesOnStart: boolean;
begin
  result := StrToBoolDef(json.vStr['CheckForUpgrades'], DEF_CHECKUPGRADES);
end;

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

function TFHIRToolkitSettings.GetRegistryPassword: String;
begin
  result := strDecrypt(json.vStr['RegistryPassword'], 53423);
end;

function TFHIRToolkitSettings.GetRegistryUsername: String;
begin
  result := json.vStr['RegistryUsername'];
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

procedure TFHIRToolkitSettings.SetCacheManager(const Value: TFHIRPackageManager);
begin
  FCacheManager.Free;
  FCacheManager := Value;
end;

procedure TFHIRToolkitSettings.SetCheckForUpgradesOnStart(const Value: boolean);
begin
  if value <> GetCheckForUpgradesOnStart then
  begin
    json.vStr['CheckForUpgrades'] := BoolToStr(value);
    Save;
  end;
end;

procedure TFHIRToolkitSettings.SetProxy(const Value: String);
var
  o : TJsonObject;
begin
  if value <> GetProxy then
  begin
    o := json.forceObj['Client'];
    o.vStr['Proxy'] := value;
    Save;
  end;
end;

procedure TFHIRToolkitSettings.SetRegistryPassword(const Value: String);
begin
  json.vStr['RegistryPassword'] := strEncrypt(Value, 53423);
end;

procedure TFHIRToolkitSettings.SetRegistryUsername(const Value: String);
begin
  json.vStr['RegistryUsername'] := value;
end;

procedure TFHIRToolkitSettings.SetShowHelp(const Value: boolean);
begin
  if value <> GetShowHelp then
  begin
    json.vStr['ShowHelp'] := BoolToStr(value);
    Save;
  end;
end;

procedure TFHIRToolkitSettings.SetTimeout(const Value: integer);
var
  o : TJsonObject;
begin
  if value <> GetTimeout then
  begin
    o := json.forceObj['Client'];
    o.vStr['Timeout'] := inttostr(value);
    Save;
  end;
end;

procedure TFHIRToolkitSettings.SetVersions(const Value: TFHIRVersionFactories);
begin
  FVersions.Free;
  FVersions := Value;
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

