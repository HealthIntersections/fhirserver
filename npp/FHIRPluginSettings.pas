unit FHIRPluginSettings;


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

interface

uses
  Windows, SysUtils, Classes,
  AdvObjects, AdvJson,
  FHIRTypes,
  SmartOnFhirUtilities, CDSHooksUtilities, FHIRClientSettings;

const
  DEF_ActivePage = 0;
  DEF_BreaksWidth = 200;
  DEF_SourceHeight = 70;
  DEF_Width = 720;
  DEF_Height = 300;

type
  TDefinitionsVersion = (defV2, defV3);

  TFHIRPluginSettings = class (TFHIRClientSettings)
  private
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
  protected
    procedure initSettings; virtual;
  public
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


procedure TFHIRPluginSettings.initSettings;
begin
  inherited;
  json.vStr['DefinitionsVersion'] := 'r3';
  json.vStr['TerminologyServer'] := 'http://tx.fhir.org/r3';
  json.bool['BackgroundValidation'] := true;
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

end.
