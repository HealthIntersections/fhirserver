unit FHIR.Npp.Settings;


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
  fsl_base, fsl_json,
  fhir_oauth, fhir_cdshooks, fhir_client_registry;

const
  DEF_ActivePage = 0;
  DEF_BreaksWidth = 200;
  DEF_SourceHeight = 70;
  DEF_Width = 720;
  DEF_Height = 300;

type
  TDefinitionsVersion = (defV2, defV3);

  TFHIRPluginSettings = class (TFHIRClientRegistry)
  private
    function GetToolboxVisible: boolean;
    function GetVisualiserVisible: boolean;
    function GetTerminologyServerR2: string;
    function GetTerminologyServerR3: string;
    function GetTerminologyServerR4: string;
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
    function GetNoWelcomeForm: boolean;
    function GetBuildPrompt: String;
    function GetUpdateResourceOnSend: boolean;
    function GetValidationAnnotations: boolean;

    procedure SetTerminologyServerR2(const Value: string);
    procedure SetTerminologyServerR3(const Value: string);
    procedure SetTerminologyServerR4(const Value: string);
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
    procedure SetNoWelcomeForm(const Value: boolean);
    procedure SetBuildPrompt(const Value: String);
    Procedure SetUpdateResourceOnSend(const Value: boolean);
    procedure SetValidationAnnotations(const Value: boolean);

    function GetLoadR2: boolean;
    function GetLoadR3: boolean;
    function GetLoadR4: boolean;
    procedure SetLoadR2(const Value: boolean);
    procedure SetLoadR3(const Value: boolean);
    procedure SetLoadR4(const Value: boolean);
  protected
    procedure initSettings; override;
  public
    function FontName : String;
    Function FontSize : integer;

    property ToolboxVisible : boolean read GetToolboxVisible write SetToolboxVisible;
    property VisualiserVisible : boolean read GetVisualiserVisible write SetVisualiserVisible;
    property loadR2 : boolean read GetLoadR2 write SetLoadR2;
    property loadR3 : boolean read GetLoadR3 write SetLoadR3;
    property loadR4 : boolean read GetLoadR4 write SetLoadR4;
    property TerminologyServerR2 : string read GetTerminologyServerR2 write SetTerminologyServerR2;
    property TerminologyServerR3 : string read GetTerminologyServerR3 write SetTerminologyServerR3;
    property TerminologyServerR4 : string read GetTerminologyServerR4 write SetTerminologyServerR4;
    property Path : String read GetPath write SetPath;
    property NoPathSummary : boolean read GetNoPathSummary write SetNoPathSummary;
    property NoValidationSummary : boolean read GetNoValidationSummary write SetNoValidationSummary;
    property NoWelcomeForm : boolean read GetNoWelcomeForm write SetNoWelcomeForm;
    property BackgroundValidation : boolean read GetBackgroundValidation write SetBackgroundValidation;
    property ValidationAnnotations : boolean read GetValidationAnnotations write SetValidationAnnotations;
    property BuildPrompt : String read GetBuildPrompt write SetBuildPrompt;

    property DebuggerHeight : integer read GetDebuggerHeight write SetDebuggerHeight;
    property DebuggerWidth : integer read GetDebuggerWidth write SetDebuggerWidth;
    property DebuggerSourceHeight : integer read GetDebuggerSourceHeight write SetDebuggerSourceHeight;
    property DebuggerBreaksWidth : integer read GetDebuggerBreaksWidth write SetDebuggerBreaksWidth;
    property DebuggerActivePage : integer read GetDebuggerActivePage write SetDebuggerActivePage;
    property DebuggerBreaks : String read GetDebuggerBreaks write SetDebuggerBreaks;

    property updateResourceOnSend : boolean read GetUpdateResourceOnSend write SetUpdateResourceOnSend;
  end;

var
  Settings : TFHIRPluginSettings;

implementation

{ TFHIRPluginSettings }


procedure TFHIRPluginSettings.initSettings;
begin
  inherited;
  json.vStr['DefinitionsVersion'] := 'r3';
  json.vStr['FHIR.Tx.Server'] := 'http://tx.fhir.org/r3';
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

function TFHIRPluginSettings.GetLoadR2: boolean;
begin
  result := not json.bool['no-load-r2'];
end;

function TFHIRPluginSettings.GetLoadR3: boolean;
begin
  result := not json.bool['no-load-r3'];
end;

function TFHIRPluginSettings.GetLoadR4: boolean;
begin
  result := not json.bool['no-load-r4'];
end;

function TFHIRPluginSettings.GetNoPathSummary: boolean;
begin
  result := json.bool['NoPathSummary'];
end;

function TFHIRPluginSettings.GetNoValidationSummary: boolean;
begin
  result := json.bool['NoValidationSummary'];
end;

function TFHIRPluginSettings.GetNoWelcomeForm: boolean;
begin
  result := json.bool['NoWelcomeForm'];
end;

function TFHIRPluginSettings.GetPath: String;
begin
  result := json.vStr['Path'];
end;

function TFHIRPluginSettings.GetTerminologyServerR2: string;
begin
  result := json.vStr['TerminologyServerR2'];
end;

function TFHIRPluginSettings.GetTerminologyServerR3: string;
begin
  result := json.vStr['TerminologyServerR3'];
end;

function TFHIRPluginSettings.GetTerminologyServerR4: string;
begin
  result := json.vStr['TerminologyServerR4'];
end;

function TFHIRPluginSettings.GetToolboxVisible: boolean;
begin
  result := json.bool['Toolbox'];
end;

function TFHIRPluginSettings.GetUpdateResourceOnSend: boolean;
begin
  result := json.bool['UpdateResourceOnSend'];
end;

function TFHIRPluginSettings.GetValidationAnnotations: boolean;
begin
  result := json.bool['ValidationAnnotations'];
end;

function TFHIRPluginSettings.GetVisualiserVisible: boolean;
begin
  result := json.bool['Visualiser'];
end;

procedure TFHIRPluginSettings.SetBackgroundValidation(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['BackgroundValidation'] := value;
  Save;
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

procedure TFHIRPluginSettings.SetLoadR2(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['no-load-r2'] := not value;
  save;
end;

procedure TFHIRPluginSettings.SetLoadR3(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['no-load-r3'] := not value;
  save;
end;

procedure TFHIRPluginSettings.SetLoadR4(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['no-load-r4'] := not value;
  save;
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

procedure TFHIRPluginSettings.SetNoWelcomeForm(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['NoWelcomeForm'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetPath(const Value: String);
begin
  if FShuttingDown then exit;
  json.vStr['Path'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetTerminologyServerR2(const Value: string);
begin
  if FShuttingDown then exit;
  json.vStr['TerminologyServerR2'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetTerminologyServerR3(const Value: string);
begin
  if FShuttingDown then exit;
  json.vStr['TerminologyServerR3'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetTerminologyServerR4(const Value: string);
begin
  if FShuttingDown then exit;
  json.vStr['TerminologyServerR4'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetToolboxVisible(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['Toolbox'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetUpdateResourceOnSend(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['UpdateResourceOnSend'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetValidationAnnotations(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['ValidationAnnotations'] := Value;
  Save;
end;

procedure TFHIRPluginSettings.SetVisualiserVisible(const Value: boolean);
begin
  if FShuttingDown then exit;
  json.bool['Visualiser'] := Value;
  Save;
end;

end.
