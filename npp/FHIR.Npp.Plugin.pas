unit FHIR.Npp.Plugin;


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

{
[27/10/2015 9:52:30 PM] Grahame Grieve: - validation as you type
- json <--> xml conversion
- smart on fhir rest operations
- cds-hook testing
[27/10/2015 9:52:35 PM] Grahame Grieve: other ideas?
[27/10/2015 9:57:43 PM] Grahame Grieve: + fhir path evaluation
[28/10/2015 12:53:56 AM] Ewout Kramer: "validate on server" ?
[28/10/2015 12:54:08 AM] Ewout Kramer: PUT/POST to server?
[28/10/2015 12:55:00 AM] Ewout Kramer: intellisense for  code elements with required bindings?
[28/10/2015 3:20:35 AM] Josh Mandel: Built-in vocab lookup when writing a Coding}

{
Commands:
About the FHIR Plugin
--
Change Format (XML <--> JSON)
Validate Resource
Clear Validation Information
generate diff
--
Connect to Server
--
New Resource (Template)
Open Resource on Server
PUT resource to existing ID
POST resource to new ID
POST resource as a transaction
Validate resource on server
--
Configure Tools
Close the FHIR Toolbox

}
interface

uses
  Windows, SysUtils, Classes, Forms, Vcl.Dialogs, Messages, Consts, UITypes, System.Generics.Defaults, ActiveX,
  FHIR.Npp.Base, FHIR.Npp.Scintilla,
  fsl_base, fsl_utilities, fsl_threads, fsl_stream, fsl_wininet, fsl_xml, fsl_json,
  fhir_objects, fhir_parser, fhir_validator, fhir_narrative, fhir_factory, fhir_pathengine, fhir_common, 
  fhir4_constants, fhir4_types, fhir4_resources, fhir4_common,
  FHIR.Npp.Context,
  FHIR.Npp.Settings, FHIR.Npp.Validator, fhir_xhtml,
  fhir_oauth, FHIR.Smart.Login, FHIR.Smart.LoginVCL, FHIR.Npp.Version, FHIR.Npp.Utilities,
  FHIR.Npp.Toolbox, FHIR.Npp.About, FHIR.Npp.Configuration, FHIR.Npp.Make, FHIR.Npp.Fetch, PathDialogForms, ValidationOutcomes, FHIR.Npp.CodeGen,
  FHIR.Npm.Manager, fsl_npm_cache,
  FHIR.Npp.Visualiser, FHIR.Base.PathDebugger, FHIR.Npp.Welcome, UpgradePrompt, fhir_diff, ResDisplayForm, FHIR.Npp.SaveAs;

const
  INDIC_INFORMATION = 21;
  INDIC_WARNING = 22;
  INDIC_ERROR = 23;
  INDIC_MATCH = 24;
  LEVEL_INDICATORS : array [TFHIRAnnotationLevel] of Integer = (INDIC_INFORMATION, INDIC_WARNING, INDIC_ERROR, INDIC_MATCH);


type
  TFHIRPlugin = class;

  TFHIRPluginFileStatus = (pfsUnknown, pfsNonFHIR, pfsResource);

  TFHIRPluginFileInformation = class (TFslObject)
  private
    FVersions : TFHIRVersionStatuses;
    FFormat: TFHIRFormat;
    FStatus: TFHIRPluginFileStatus;
    FUrl: String;
  public
    function link : TFHIRPluginFileInformation; overload;

    procedure init(preset : TFHIRPluginFileInformation);

    function workingVersion : TFHIRVersion;

    property status : TFHIRPluginFileStatus read FStatus write FStatus;
    property format : TFHIRFormat read FFormat write FFormat;
    property versions : TFHIRVersionStatuses read FVersions write FVersions;
    property url : String read FUrl write FUrl;

    function summary : String;
  end;

  TBackgroundValidatorEngineTask = class (TFslBuffer)
  private
    FVersion  : TFHIRVersion;
    FFormat : TFHIRFormat;
  public
    constructor Create(version : TFHIRVersion; format : TFHIRFormat; src : TBytes);

    property Version : TFHIRVersion read FVersion;
    property Format : TFHIRFormat read FFormat;
  end;

  TBackgroundValidatorEngine = class (TBackgroundTaskEngine)
  private
    function input : TBackgroundValidatorEngineTask;
    procedure validatorCheck(sender : TObject; message : String);
  public
    function name : String; override;
    procedure execute; override;
  end;

  TContextLoadingEngineTask = class (TFslObject)
  private
    FPlugin : TFHIRPlugin;
    FFactory : TFHIRFactory;
    FTerminologyServer : String;
  public
    constructor Create(plugin : TFHIRPlugin; factory : TFHIRFactory; TerminologyServer : string);
    property plugin : TFHIRPlugin read FPlugin;
    property factory : TFHIRFactory read FFactory;
    property terminologyServer : String read FTerminologyServer;
  end;

  TContextLoadingEngine = class (TBackgroundTaskEngine)
  private
    function input : TContextLoadingEngineTask;
  public
    function name : String; override;
    procedure Execute(); override;
  end;

  TUpgradeCheckEngineOutput = class (TFslObject)
  private
    FNotes : String;
    FReference : String;
    FGo : boolean;
  public
    property notes : string read FNotes write FNotes;
    property reference : String read FReference write FReference;
    property go : boolean read FGo write FGo;
  end;

  TUpgradeCheckEngine = class (TBackgroundTaskEngine)
  private
    function output : TUpgradeCheckEngineOutput;

    function getServerLink(doc: TMXmlDocument): string;
    function getUpgradeNotes(doc: TMXmlDocument; current: String): string;
  public
    function name : String; override;
    procedure Execute(); override;
  end;

  TFHIRPlugin = class(TNppPlugin)
  private
    FContext : TFHIRNppContext;
    FFileInfo : TFslMap<TFHIRPluginFileInformation>;
    FCurrentFileInfo : TFHIRPluginFileInformation;
    FPreset : TFHIRPluginFileInformation;

    tipShowing : boolean;
    errors : TFslList<TFHIRAnnotation>;
    matches : TFslList<TFHIRAnnotation>;
    errorSorter : TFHIRAnnotationComparer;
    init : boolean;
    FLastSrc : TBytes;
    FLastRes : TFHIRResourceV;
    FCurrentServer : TRegisteredFHIRServer;
    FWantUpdate : boolean;
    FRedoMatches : boolean;
    FLastParseError: String;

    FValidatorTask : integer;
    FLoader2Task : integer;
    FLoader3Task : integer;
    FLoader4Task : integer;
    FUpgradeTask : integer;

    // Scintilla control
    Procedure SetUpSquiggles;
    procedure squiggle(level : integer; line, start, length : integer; annotation : boolean; message : String);
    procedure clearSquiggle(level : integer; line, start, length : integer);

    // fhir stuff
    function determineFormat(src : TBytes) : TFHIRFormat; overload;
    function waitForContext(version : TFHIRVersion; manualOp : boolean) : boolean;
    function convertIssue(issue: TFhirOperationOutcomeIssueW): TFHIRAnnotation;
    function locate(res : TFHIRResourceV; var path : String; var focus : TArray<TFHIRObject>) : boolean;
    function parse(timeLimit : integer; var fmt : TFHIRFormat; var res : TFHIRResourceV) : boolean; overload;

    function parse(cnt : String; fmt : TFHIRFormat) : TFHIRResourceV; overload;
    function parse(cnt : TBytes; fmt : TFHIRFormat) : TFHIRResourceV; overload;
    function compose(cnt : TFHIRResourceV; fmt : TFHIRFormat) : TBytes; overload;

    procedure evaluatePath(r : TFHIRResourceV; out items : TFHIRSelectionList; out expr : TFHIRPathExpressionNodeV; out types : TFHIRTypeDetailsV);
//    function showOutcomes(fmt : TFHIRFormat; items : TFHIRObjectList; expr : TFHIRPathExpressionNode; types : TFslStringSet) : string;

    // smart on fhir stuff
//    function DoSmartOnFHIR(server : TRegisteredFHIRServer) : boolean;
    procedure configureSSL;

    procedure AnalyseFile(revisit : boolean);
    function checkSource(src : TBytes; v : TFHIRVersion; fmt : TFHIRFormat) : TFHIRVersionStatus;
    function checkContext(version: TFHIRVersion): boolean;

    procedure processValidatorResults(id : integer; response : TBackgroundTaskPackage);
    procedure processLoadingResults(id : integer; response : TBackgroundTaskPackage);
    procedure processUpgradeResults(id : integer; response : TBackgroundTaskPackage);

    function GetDebuggerSetting(name : TFHIRPathDebuggerFormSetting) : Integer;
    procedure SetDebuggerSetting(name : TFHIRPathDebuggerFormSetting; value : Integer);
    function GetDebuggerSettingStr(name : TFHIRPathDebuggerFormSetting) : String;
    procedure SetDebuggerSettingStr(name : TFHIRPathDebuggerFormSetting; value : String);
    function parseError : String;
  public
    constructor Create;
    destructor Destroy; override;

    property Context : TFHIRNppContext read FContext;

    // user interface
    procedure FuncValidate(ver : TFHIRVersion = fhirVersionUnknown);
    procedure FuncValidateClear;
    procedure FuncMatchesClear;
    procedure FuncToolbox;
    procedure FuncVisualiser;
    procedure FuncSettings(servers : boolean);
    procedure FuncPackageManager;
    procedure FuncAbout;
    procedure FuncFormat(fmt : TFHIRFormat);
    procedure FuncDebugPath;
    procedure FuncJumpToPath;
    procedure FuncExtractPath;
    procedure FuncServers;
    procedure FuncNewResource;
    procedure FuncOpen;
    procedure FuncPUT;
    procedure FuncPOST;
    procedure FuncTransaction;
    procedure FuncServerValidate;
    procedure FuncNarrative;
    procedure funcDifference;
    procedure funcGenerateCode;

    procedure reset;
    procedure SetSelection(start, stop : integer);

    // responding to np++ events
    procedure DoNppnReady; override; // install toolbox if necessary
    function  prepNarrative(r : TFHIRResourceV): String; overload;
    procedure DoNppnTextModified; override;
    procedure DoNppnBufferChange; override;
    procedure DoNppnDwellStart(offset : integer); override;
    procedure DoNppnDwellEnd; override;
    procedure DoNppnShutdown; override;
    procedure DoStateChanged; override;
    procedure DoNppnFileOpened; override;
    procedure DoNppnFilebeforeClose; override;
    procedure DoNppnFileClosed; override;

    // from other places in the plugin
    procedure refreshStatus;
    procedure checkTrigger;
    procedure TreatAsVersion(v : TFHIRVersion);
    procedure MarkAsVersion(v : TFHIRVersion);
    procedure newResource(src : String; version : TFHIRVersion; fmt : TFHIRFormat; url : String); overload;
    procedure newResource(r : TFHIRResourceV; version : TFHIRVersion; fmt : TFHIRFormat; url : String); overload;
  end;

procedure _FuncValidate; cdecl;
procedure _FuncValidateClear; cdecl;
procedure _FuncToolbox; cdecl;
procedure _FuncVisualiser; cdecl;
procedure _FuncAbout; cdecl;
procedure _FuncSettings; cdecl;
procedure _FuncPackageManager; cdecl;
procedure _FuncDebugPath; cdecl;
procedure _FuncExtractPath; cdecl;
procedure _FuncJumpToPath; cdecl;
procedure _FuncFormat; cdecl;
procedure _FuncServers; cdecl;
procedure _FuncNewResource; cdecl;
procedure _FuncOpen; cdecl;
procedure _FuncPUT; cdecl;
procedure _FuncPOST; cdecl;
procedure _FuncTransaction; cdecl;
procedure _FuncServerValidate; cdecl;
procedure _FuncNarrative; cdecl;
procedure _FuncDebug; cdecl;
procedure _FuncDifference; cdecl;
procedure _FuncGenerateCode; cdecl;

var
  FNpp: TFHIRPlugin;

implementation

uses
  IdSSLOpenSSLHeaders,
  fhir2_factory,
  fhir3_factory,
  fhir4_factory;

var
  ms : String;

procedure mcheck(i : integer);
begin
  ms := ms + inttostr(i) +' ';
end;


{ TFHIRPlugin }

constructor TFHIRPlugin.Create;
var
//  sk: TShortcutKey;
  i: Integer;
begin
  inherited;
  FContext := TFHIRNppContext.Create;
  FFileInfo := TFslMap<TFHIRPluginFileInformation>.create('FileInfo');
  FValidatorTask := GBackgroundTasks.registerTaskEngine(TBackgroundValidatorEngine.Create(processValidatorResults));
  FLoader2Task := GBackgroundTasks.registerTaskEngine(TContextLoadingEngine.Create(processLoadingResults));
  FLoader3Task := GBackgroundTasks.registerTaskEngine(TContextLoadingEngine.Create(processLoadingResults));
  FLoader4Task := GBackgroundTasks.registerTaskEngine(TContextLoadingEngine.Create(processLoadingResults));
  FUpgradeTask := GBackgroundTasks.registerTaskEngine(TUpgradeCheckEngine.Create(processUpgradeResults));

  errors := TFslList<TFHIRAnnotation>.create;
  errorSorter := TFHIRAnnotationComparer.create;
  errors.Sort(errorSorter.link);
  matches := TFslList<TFHIRAnnotation>.create;
  matches.Sort(errorSorter.link);

  self.PluginName := '&FHIR';
  i := 0;

{  sk.IsCtrl := true;
  sk.IsAlt := true;
  sk.Key := 'F';}

  self.AddFuncItem('Change &Format (XML <--> JSON)', _FuncFormat);
  self.AddFuncItem('&Validate Resource', _FuncValidate);
  self.AddFuncItem('Clear Validation Information', _FuncValidateClear);
  self.AddFuncItem('Make &Patch', _FuncDifference);
  self.AddFuncItem('Generate &Code', _FuncGenerateCode);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('&Jump to Path', _FuncJumpToPath);
  self.AddFuncItem('&Debug Path Expression', _FuncDebugPath);
  self.AddFuncItem('&Extract Path from Cursor', _FuncExtractPath);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('&New Resource (Template)', _FuncNewResource);
  self.AddFuncItem('&Open Resource on Server', _FuncOpen);
  self.AddFuncItem('&Save resource on Server', _FuncPUT);
  self.AddFuncItem('Save on &a Server', _FuncPOST);
  self.AddFuncItem('Validate &resource on server', _FuncServerValidate);
  self.AddFuncItem('V&2 IDE', _FuncDebug);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('Server &Manager', _FuncServers);
  self.AddFuncItem('Pac&kage Manager', _FuncPackageManager);
  self.AddFuncItem('Confi&gure Tools', _FuncSettings);
  self.AddFuncItem('View &Toolbox', _FuncToolbox);
  self.AddFuncItem('View Visuali&zer', _FuncVisualiser);
  self.AddFuncItem('Debug Install', _FuncDebug);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('&About the FHIR Plugin', _FuncAbout);
  configureSSL;
end;

function TFHIRPlugin.compose(cnt: TFHIRResourceV; fmt: TFHIRFormat): TBytes;
var
  s : TBytesStream;
  comp : TFHIRComposer;
begin
  s := TBytesStream.Create;
  try
    if fmt = ffXml then
      comp := FContext.Version[FCurrentFileInfo.workingVersion].makeComposer(ffXml)
    else
      comp := FContext.Version[FCurrentFileInfo.workingVersion].makeComposer(ffJson);
    try
      comp.Compose(s, cnt);
      result := s.Bytes;
    finally
     comp.free;
    end;
  finally
    s.Free;
  end;
end;

procedure TFHIRPlugin.configureSSL;
begin
  IdOpenSSLSetLibPath(ExtractFilePath(GetModuleName(HInstance)));
end;

procedure _FuncValidate; cdecl;
begin
  // so this is called during start up for unknown reasons?
  if (FNpp = nil) or (FNpp.FCurrentFileInfo = nil) then
    exit;
  FNpp.FuncValidate;
end;

procedure _FuncValidateClear; cdecl;
begin
  FNpp.FuncValidateClear;
end;

procedure _FuncAbout; cdecl;
begin
  FNpp.FuncAbout;
end;

procedure _FuncVisualiser; cdecl;
begin
  FNpp.FuncVisualiser;
end;

procedure _FuncToolbox; cdecl;
begin
  FNpp.FuncToolbox;
end;

procedure _FuncSettings; cdecl;
begin
  FNpp.FuncSettings(false);
end;

procedure _FuncPackageManager; cdecl;
begin
  FNpp.FuncPackageManager;
end;

procedure _FuncDebugPath; cdecl;
begin
  FNpp.FuncDebugPath;
end;

procedure _FuncJumpToPath; cdecl;
begin
  FNpp.FuncJumpToPath;
end;

procedure _FuncExtractPath; cdecl;
begin
  FNpp.FuncExtractPath;
end;

procedure _FuncFormat; cdecl;
begin
  FNpp.FuncFormat(ffUnspecified);
end;

procedure _FuncServers; cdecl;
begin
  FNpp.FuncServers;
end;


procedure _FuncNewResource; cdecl;
begin
  FNpp.FuncNewResource;
end;

procedure _FuncOpen; cdecl;
begin
  FNpp.FuncOpen;
end;

procedure _FuncPUT; cdecl;
begin
  FNpp.FuncPUT;
end;

procedure _FuncPOST; cdecl;
begin
  FNpp.FuncPOST;
end;

procedure _FuncTransaction; cdecl;
begin
  FNpp.FuncTransaction;
end;

procedure _FuncServerValidate; cdecl;
begin
  FNpp.FuncServerValidate;
end;

procedure _FuncGenerateCode; cdecl;
begin
  FNpp.FuncGenerateCode;
end;


procedure _FuncNarrative; cdecl;
begin
  FNpp.FuncNarrative;
end;

procedure _FuncDifference; cdecl;
begin
  FNpp.FuncDifference;
end;

procedure _FuncDebug; cdecl;
var
  s : String;
begin
  try
    s := 'plugin: '+inttohex(cardinal(FNpp), 8)+#13#10;
    s := s + 'config: '+IncludeTrailingPathDelimiter(FNpp.GetPluginsConfigDir)+'FHIR.Npp.Plugin.json';
    s := s + 'init: '+BoolToStr(FNpp.init)+#13#10;
    s := s + 'server: '+ inttohex(cardinal(FNpp.FCurrentServer), 8)+ #13#10;
  except
    on e : exception do
      s := s + 'exception: '+e.Message;
  end;
  ShowMessage(s);
end;


function TFHIRPlugin.convertIssue(issue : TFhirOperationOutcomeIssueW) : TFHIRAnnotation;
var
  s, e : integer;
  msg : String;
begin
  s := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, StrToIntDef(issue.element.Tags['s-l'], 1)-1, StrToIntDef(issue.element.Tags['s-c'], 1)-1);
  e := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, StrToIntDef(issue.element.Tags['e-l'], 1)-1, StrToIntDef(issue.element.Tags['e-c'], 1)-1);
  if (e = s) then
    e := s + 1;
  msg := issue.display;
  case issue.severity of
    isWarning : result := TFHIRAnnotation.create(alWarning, StrToIntDef(issue.element.Tags['s-l'], 1)-1, s, e, msg, msg);
    isInformation : result := TFHIRAnnotation.create(alHint, StrToIntDef(issue.element.Tags['s-l'], 1)-1, s, e, msg, msg);
  else
    result := TFHIRAnnotation.create(alError, StrToIntDef(issue.element.Tags['s-l'], 1)-1, s, e, msg, msg);
  end;
end;

procedure TFHIRPlugin.FuncValidate;
var
  src : TBytes;
  buffer : TFslBuffer;
  error : TFHIRAnnotation;
  iss : TFhirOperationOutcomeIssueW;
  ctxt: TFHIRValidatorContext;
  val : TFHIRValidatorV;
begin
  FuncValidateClear;
  if ver = fhirVersionUnknown then
    ver := FCurrentFileInfo.workingVersion;

  if not waitForContext(ver, true) then
    exit;

  GBackgroundTasks.killTask(FValidatorTask);

  if (FCurrentFileInfo.Format <> ffUnspecified) then
  begin
    src := CurrentBytes;
    try
      buffer := TFslBuffer.Create;
      try
        buffer.AsBytes := src;
        ctxt := TFHIRValidatorContext.Create;
        try
          ctxt.ResourceIdRule := risOptional;
          ctxt.OperationDescription := 'validate';
          val := FContext.Version[ver].makeValidator;
          try
            try
              val.validate(ctxt, buffer, FCurrentFileInfo.Format);
            except
              on e : exception do
              begin
                errors.add(TFHIRAnnotation.create(alError, 0, 0, 0, 'Validation Process Failed: '+e.Message, e.Message));
                raise;
              end;
            end;
          finally
            val.free;
          end;
          for iss in ctxt.Issues do
            errors.add(convertIssue(iss));
          if not ValidationSummary(self, FContext.Version[ver].Worker, ctxt.Issues) then
            MessageBeep(MB_OK);
        finally
          ctxt.Free;
        end;
      finally
        buffer.Free;
      end;
    except
      on e: Exception do
      begin
        if not ValidationError(self, e.message) then
          errors.Add(TFHIRAnnotation.create(alError, 0, 0, 4, e.Message, e.Message));
      end;
    end;
  end
  else if not ValidationError(self, parseError) then
    errors.Add(TFHIRAnnotation.create(alError, 0, 0, 4, parseError, ''));

  setUpSquiggles;
  for error in errors do
    squiggle(LEVEL_INDICATORS[error.level], error.line, error.start, error.stop - error.start, Settings.ValidationAnnotations, error.message);
  if FHIRVisualizer <> nil then
    FHIRVisualizer.setValidationOutcomes(errors);
end;

procedure TFHIRPlugin.FuncMatchesClear;
var
  annot : TFHIRAnnotation;
begin
  for annot in matches do
    clearSquiggle(LEVEL_INDICATORS[annot.level], annot.line, annot.start, annot.stop - annot.start);
  matches.Clear;
  if tipShowing then
    mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_CALLTIPCANCEL, 0, 0));
  FRedoMatches := true;
end;

procedure TFHIRPlugin.FuncValidateClear;
var
  annot : TFHIRAnnotation;
begin
  for annot in errors do
    clearSquiggle(LEVEL_INDICATORS[annot.level], annot.line, annot.start, annot.stop - annot.start);
  errors.Clear;
  if tipShowing then
    mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_CALLTIPCANCEL, 0, 0));
  if FHIRVisualizer <> nil then
    FHIRVisualizer.setValidationOutcomes(nil);
end;

procedure TFHIRPlugin.FuncVisualiser;
begin
  if (not Assigned(FHIRVisualizer)) then
    FHIRVisualizer := TFHIRVisualizer.Create(self, 2);
  FHIRVisualizer.Show;
end;

function TFHIRPlugin.GetDebuggerSetting(name: TFHIRPathDebuggerFormSetting): Integer;
begin
  case name of
    settingDebuggerHeight: result := Settings.DebuggerHeight;
    settingDebuggerWidth: result := Settings.DebuggerWidth;
    settingDebuggerSourceHeight: result := Settings.DebuggerSourceHeight;
    settingDebuggerBreaksWidth: result := Settings.DebuggerBreaksWidth;
    settingDebuggerActivePage: result := Settings.DebuggerActivePage;
    settingDebuggerFontSize: result := Settings.FontSize;
  else
    result := 0;
  end;
end;

function TFHIRPlugin.GetDebuggerSettingStr(name: TFHIRPathDebuggerFormSetting): String;
begin
  case name of
    settingDebuggerBreaksWidth : result := settings.DebuggerBreaks;
    settingDebuggerFontName : result := settings.FontName;
  else
    result := '';
  end;
end;

function SplitElement(var src : String) : String;
var
  i : integer;
  s : string;
begin
  if src = '' then
    exit('');

  if src.StartsWith('<?') then
    s := '?>'
  else if src.StartsWith('<!--') then
    s := '->'
  else if src.StartsWith('<!DOCTYPE') then
    s := ']>'
  else
    s := '>';

  i := 1;
  while (i <= length(src)) and not (src.Substring(i).StartsWith(s)) do
    inc(i);
  inc(i, length(s));
  result := src.Substring(0, i);
  src := src.Substring(i).trim;
end;

function TFHIRPlugin.determineFormat(src: TBytes): TFHIRFormat;
var
  s : String;
begin
  FLastParseError := '';
  result := ffUnspecified; // null
  try
    s := TEncoding.UTF8.GetString(src);
  except
    on e : Exception do
    begin
      result := ffUnspecified;
      FLastParseError := e.message;
    end;
  end;
  s := s.Trim;
  if (s <> '') then
  begin
    if s[1] = '<' then
    begin
      while s.StartsWith('<!') or s.StartsWith('<?') do
        splitElement(s);
      s := splitElement(s);
      if s.Contains('"http://hl7.org/fhir"') then
        result := ffXml
      else if s.Contains('''http://hl7.org/fhir''') then
        result := ffXml
    end
    else if s[1] = '{' then
    begin
      if s.Contains('"resourceType"') then
        result := ffJson;
    end
    else if s[1] = '@' then
    begin
      if s.Contains('@prefix') and s.Contains('fhir:') then
        result := ffTurtle;
    end;
  end;
end;


function TFHIRPlugin.locate(res: TFHIRResourceV; var path: String; var focus : TArray<TFHIRObject>): boolean;
var
  sp : integer;
  loc : TSourceLocation;
begin
  sp := SendMessage(NppData.ScintillaMainHandle, SCI_GETCURRENTPOS, 0, 0);
  loc.line := SendMessage(NppData.ScintillaMainHandle, SCI_LINEFROMPOSITION, sp, 0)+1;
  loc.col := sp - SendMessage(NppData.ScintillaMainHandle, SCI_POSITIONFROMLINE, loc.line-1, 0)+1;
  path := FContext.Version[FCurrentFileInfo.workingVersion].Engine.extractPath(res.fhirType, loc, res, focus);
  result := path <> '';
end;

procedure TFHIRPlugin.MarkAsVersion(v: TFHIRVersion);
var
  fmt : TFHIRFormat;
  s : TBytesStream;
  res : TFHIRResourceV;
  comp : TFHIRComposer;
begin
  if not init then
    exit;
  TreatAsVersion(v);
  if (parse(0, fmt, res)) then
  try
    FuncValidateClear;
    FuncMatchesClear;
    res.profileVersion := v;

    comp := FContext.Version[FCurrentFileInfo.workingVersion].makeComposer(fmt);
    try
      s := TBytesStream.Create();
      try
        comp.Compose(s, res);
        CurrentBytes := s.Bytes;
      finally
        s.Free;
      end;
    finally
      comp.Free;
    end;
  finally
    res.Free;
  end
  else
    ShowMessage(parseError);
end;

procedure TFHIRPlugin.newResource(r: TFHIRResourceV; version: TFHIRVersion; fmt: TFHIRFormat; url: String);
var
  c : TFHIRComposer;
begin
  c := FContext.Version[version].Factory.makeComposer(FContext.Version[version].Worker.link, fmt, FContext.Version[version].Worker.lang, OutputStylePretty);
  try
    newResource(c.Compose(r), version, fmt, url);
  finally
    c.Free;
  end;
end;

procedure TFHIRPlugin.newResource(src: String; version: TFHIRVersion; fmt: TFHIRFormat; url : String);
begin
  FPreset := TFHIRPluginFileInformation.Create;
  try
    FPreset.url := url;
    FPreset.format := fmt;
    FPreset.FVersions[version] := vsSpecified;
    NewFile(TEncoding.UTF8.GetBytes(src));
  finally
    FPreset.Free;
    FPreset := nil;
  end;
end;

procedure TFHIRPlugin.FuncServers;
begin
  FuncSettings(true);
end;

procedure TFHIRPlugin.FuncServerValidate;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncGenerateCode;
var
  fmt : TFHIRFormat;
  s : TStringStream;
  res : TFHIRResourceV;
  comp : TFHIRComposer;
begin
  if not init then
    exit;
  if not waitForContext(FCurrentFileInfo.workingVersion, true) then
    exit;

  if (parse(0, fmt, res)) then
  try
    CodeGeneratorForm := TCodeGeneratorForm.create(self);
    try
      CodeGeneratorForm.Resource := res.Link;
      CodeGeneratorForm.Context := FContext.Version[FCurrentFileInfo.workingVersion].Worker.Link;
      CodeGeneratorForm.showModal;
    finally
      CodeGeneratorForm.free;
    end;
  finally
    res.Free;
  end
  else
    ShowMessage(parseError);
end;

procedure TFHIRPlugin.FuncSettings(servers : boolean);
var
  a: TSettingForm;
begin
  a := TSettingForm.Create(self);
  try
    if servers then
      a.PageControl1.ActivePageIndex := 2
    else
      a.PageControl1.ActivePageIndex := 0;
    a.versions := FContext.versions.Link;
    a.Context := FContext.link;
    a.ShowModal;
  finally
    a.Free;
  end;
end;

procedure TFHIRPlugin.FuncPackageManager;
begin
  PackageCacheForm := TPackageCacheForm.Create(self);
  try
    PackageCacheForm.ShowModal;
  finally
    PackageCacheForm.Free;
  end;
end;

procedure TFHIRPlugin.FuncAbout;
var
  a: TAboutForm;
begin
  a := TAboutForm.Create(self);
  try
    a.Memo1.text := FContext.VersionInfo;
    a.ShowModal;
  finally
    a.Free;
  end;
end;

(*
procedure TFHIRPlugin.FuncConnect;
var
  index : integer;
  server : TRegisteredFHIRServer;
  ok : boolean;
begin
  index := 0;
  server := nil; // TRegisteredFHIRServer(FHIRToolbox.cbxServers.Items.Objects[FHIRToolbox.cbxServers.ItemIndex]).link;
  try
    try
      try
        OpMessage('Connecting to Server', 'Connecting to Server '+server.fhirEndpoint);
        FClient := TFhirClients.makeHTTP(FContext.Version[COMPILED_FHIR_VERSION].Worker.link as TFHIRWorkerContext, server.fhirEndpoint, false, 5000);
        ok := true;
        if server.SmartAppLaunchMode <> salmNone then
          if not DoSmartOnFHIR(server) then
          begin
            ok := false;
            FuncDisconnect;
          end;

        if ok then
        begin
          try
            FClient.format := ffXml;
            FCapabilityStatement := FClient.conformance(false);
          except
            FClient.format := ffJson;
            FCapabilityStatement := FClient.conformance(false);
          end;
          FCapabilityStatement.checkCompatible();
          if (Assigned(FHIRToolbox)) then
            if FClient.smartToken = nil then
              FHIRToolbox.connected(server.name, server.fhirEndpoint, '', '')
            else
              FHIRToolbox.connected(server.name, server.fhirEndpoint, FClient.smartToken.username, FClient.smartToken.scopes);
          FCurrentServer := server.Link;
          if assigned(FHIRVisualizer) and (FClient.smartToken <> nil) then
            FHIRVisualizer.CDSManager.connectToServer(server, FClient.smartToken);
        end;
      finally
        OpMessage('', '');
      end;
    except
      on e : Exception do
      begin
        MessageDlg('Error connecting to server: '+e.Message, mtError, [mbok], 0);
        FuncDisconnect;
      end;
    end;
  finally
    server.Free;
  end;
end;

procedure TFHIRPlugin.FuncDisconnect;
begin
  if (Assigned(FHIRVisualizer)) and (FCurrentServer <> nil) then
     FHIRVisualizer.CDSManager.disconnectFromServer(FCurrentServer);
  if (Assigned(FHIRToolbox)) then
    FHIRToolbox.disconnected;
  if (Assigned(FetchResourceFrm)) then
    FreeAndNil(FetchResourceFrm);
  FCurrentServer.Free;
  FCurrentServer := nil;
  FClient.Free;
  FClient := nil;
  FCapabilityStatement.Free;
  FCapabilityStatement := nil;
end;

*)

procedure TFHIRPlugin.FuncExtractPath;
var
  fmt : TFHIRFormat;
  res : TFHIRResourceV;
  sp : integer;
  loc : TSourceLocation;
begin
  if assigned(FHIRToolbox) and (parse(0, fmt, res)) then
  try
    sp := SendMessage(NppData.ScintillaMainHandle, SCI_GETCURRENTPOS, 0, 0);
    loc.line := SendMessage(NppData.ScintillaMainHandle, SCI_LINEFROMPOSITION, sp, 0)+1;
    loc.col := sp - SendMessage(NppData.ScintillaMainHandle, SCI_POSITIONFROMLINE, loc.line-1, 0)+1;
    FHIRToolbox.edtPath.Text := FContext.Version[FCurrentFileInfo.workingVersion].Engine.extractPath(res.fhirType, loc, res);
  finally
    res.Free;
  end;
end;

procedure TFHIRPlugin.FuncFormat(fmt : TFHIRFormat);
var
  afmt : TFHIRFormat;
  s : TBytesStream;
  res : TFHIRResourceV;
  comp : TFHIRComposer;
begin
  if not init then
    exit;
  if (parse(0, afmt, res)) then
  try
    FuncValidateClear;
    FuncMatchesClear;
    if fmt = ffUnspecified then
      if afmt = ffXml then
        fmt := ffJson
      else
        fmt := ffXml;
    comp := FContext.Version[FCurrentFileInfo.workingVersion].makeComposer(fmt);
    try
      s := TBytesStream.Create();
      try
        comp.Compose(s, res);
        FCurrentFileInfo.Format := comp.Format;
        CurrentBytes := s.Bytes;
      finally
        s.Free;
      end;
    finally
      comp.Free;
    end;
  finally
    res.Free;
  end
  else
    ShowMessage(parseError);
end;

procedure TFHIRPlugin.FuncJumpToPath;
var
  fmt : TFHIRFormat;
  res : TFHIRResourceV;
  items : TFHIRSelectionList;
  expr : TFHIRPathExpressionNodeV;
  engine : TFHIRPathEngineV;
  sp, ep : integer;
begin
  if assigned(FHIRToolbox) and (FHIRToolbox.hasValidPath) then
  begin
    if not waitForContext(FCurrentFileInfo.workingVersion, true) then
      exit;
    if parse(0, fmt, res) then
    try
      engine := FContext.Version[FCurrentFileInfo.workingVersion].Engine;
      expr := engine.parseV(FHIRToolbox.edtPath.Text);
      try
        items := engine.evaluate(nil, res, expr);
        try
          if (items.Count > 0) and not isNullLoc(items[0].value.LocationStart) then
          begin
            sp := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, items[0].value.LocationStart.line - 1, items[0].value.LocationStart.col-1);
            ep := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, items[0].value.LocationEnd.line - 1, items[0].value.LocationEnd.col-1);
            SetSelection(sp, ep);
          end
          else
            MessageBeep(MB_ICONERROR);
        finally
          items.Free;
        end;
      finally
        expr.Free;
      end;
    finally
      res.Free;
    end;
  end
  else
    MessageDlg('Enter a FHIRPath statement in the toolbox editor', mtInformation, [mbok], 0);
end;

procedure TFHIRPlugin.FuncNarrative;
var
  buffer : TFslBuffer;
  fmt : TFHIRFormat;
  s : TBytesStream;
  res : TFHIRResourceV;
  comp : TFHIRComposer;
  narr : TFHIRNarrativeGeneratorBase;
begin
  if not waitForContext(FCurrentFileInfo.workingVersion, true) then
    exit;
  if (parse(0, fmt, res)) then
  try
    FuncValidateClear;
    FuncMatchesClear;
    if res.isDomainResource then
    begin
      narr := FContext.Version[FCurrentFileInfo.workingVersion].makeNarrative;
      try
        narr.generate(res);
      finally
        narr.Free;
      end;
    end;

    comp := FContext.Version[FCurrentFileInfo.workingVersion].makeComposer(fmt);
    try
      s := TBytesStream.Create();
      try
        comp.Compose(s, res);
        CurrentBytes := s.Bytes;
      finally
        s.Free;
      end;
    finally
      comp.Free;
    end;
  finally
    res.Free;
  end
  else
    ShowMessage(parseError);
end;

procedure TFHIRPlugin.FuncNewResource;
begin
  if not waitForContext(FCurrentFileInfo.workingVersion, true) then
    exit;
  ResourceNewForm := TResourceNewForm.Create(self);
  try
    ResourceNewForm.Context := FContext.Link;
    ResourceNewForm.ShowModal;
  finally
    FreeAndNil(ResourceNewForm);
  end;
end;

procedure TFHIRPlugin.FuncOpen;
begin
  if not assigned(FetchResourceFrm) then
    FetchResourceFrm := TFetchResourceFrm.create(self);
  FetchResourceFrm.Context := FContext.Link;
  FetchResourceFrm.ShowModal;
end;

procedure TFHIRPlugin.FuncDebugPath;
var
  src : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  res : TFHIRResourceV;
  item : TFHIRSelection;
  allSource : boolean;
  sp, ep : integer;
  annot : TFHIRAnnotation;
  types : TFHIRTypeDetailsV;
  items : TFHIRSelectionList;
//  expr : TFHIRPathExpressionNode;
  ok : boolean;
  engine : TFHIRPathEngineV;
begin
  FuncMatchesClear;
  if not waitForContext(FCurrentFileInfo.workingVersion, true) then
    exit;

  if (parse(0, fmt, res)) then
  try
    FuncMatchesClear;
    engine := FContext.Version[FCurrentFileInfo.workingVersion].makePathEngine;
    try
      ok := RunPathDebugger(self, FContext.Version[FCurrentFileInfo.workingVersion].Worker, engine, GetDebuggerSetting, SetDebuggerSetting, GetDebuggerSettingStr, SetDebuggerSettingStr, FContext.Version[FCurrentFileInfo.workingVersion].Factory, res, res, FHIRToolbox.edtPath.Text, fmt, types, items);
      try
        if ok then
        begin
          allSource := true;
          for item in items do
            allSource := allSource and not isNullLoc(item.value.LocationStart);

          if Items.Count = 0 then
            pathOutcomeDialog(self, FHIRToolbox.edtPath.Text, res.fhirType, types, pomNoMatch, 'no items matched')
          else if not allSource then
            pathOutcomeDialog(self, FHIRToolbox.edtPath.Text, res.fhirType, types, pomNoMatch, FContext.Version[FCurrentFileInfo.workingVersion].Engine.convertToString(items))
          else
          begin
            if (items.Count = 1) then
              pathOutcomeDialog(self, FHIRToolbox.edtPath.Text, res.fhirType, types, pomMatch, '1 matching item')
            else
              pathOutcomeDialog(self, FHIRToolbox.edtPath.Text, res.fhirType, types, pomMatch, inttostr(items.Count)+' matching items');
          end;
        end;
      finally
        types.Free;
        items.Free;
      end;
    finally
      engine.Free;
    end;
  finally
    res.Free;
  end
  else
    ShowMessage(parseError);
end;

procedure TFHIRPlugin.funcDifference;
var
  current, original, output : TBytes;
  fmtc, fmto : TFHIRFormat;
  rc, ro : TFHIRResourceV;
  op : TFHIRParametersW;
  diff : TDifferenceEngine;
  html : String;
begin
  try
    if not waitForContext(FCurrentFileInfo.workingVersion, true) then
      exit;

    current := CurrentBytes;
    fmtc := determineFormat(current);
    if (fmtc = ffUnspecified) then
      raise EFHIRException.create('Unable to parse current content');
    original := FileToBytes(CurrentFileName);
    fmto := determineFormat(current);
    if (fmto = ffUnspecified) then
      raise EFHIRException.create('Unable to parse original file');

    rc := parse(current, fmtc);
    try
      ro := parse(original, fmto);
      try
        diff := TDifferenceEngine.Create(FContext.Version[FCurrentFileInfo.workingVersion].Worker.link, FContext.Version[FCurrentFileInfo.workingVersion].Factory.link);
        try
          op := diff.generateDifference(ro, rc, html);
          try
            output := compose(op.Resource, fmtc);
            ShowResource(self, 'Difference', html, TEncoding.UTF8.GetString(output));
          finally
            op.free;
          end;
        finally
          diff.Free;
        end;
      finally
        ro.Free;
      end;
    finally
      rc.free;
    end;
  except
    on e : exception do
      MessageDlg(e.Message, mtError, [mbok], 0);
  end;
end;

procedure TFHIRPlugin.FuncPOST;
var
  id : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  res : TFHIRResourceV;
  comp : TFHIRComposer;
begin
  if (parse(0, fmt, res)) then
  try
    SaveOnServerDialog := TSaveOnServerDialog.create(self);
    try
      SaveOnServerDialog.Resource := res.link;
      SaveOnServerDialog.context :=  FContext.Link;
      SaveOnServerDialog.source := CurrentBytes;
      SaveOnServerDialog.format := fmt;
      if SaveOnServerDialog.showModal = mrOk then
      begin
        if SaveOnServerDialog.cbUpdate.Checked then
        begin
          FuncValidateClear;
          FuncMatchesClear;
          comp := FContext.Version[SaveOnServerDialog.resource.fhirObjectVersion].makeComposer(fmt);
          try
            FCurrentFileInfo.Fversions[SaveOnServerDialog.resource.fhirObjectVersion] := vsSpecified;
            CurrentBytes := comp.ComposeBytes(SaveOnServerDialog.resource);
          finally
            comp.free;
          end;
        end;
      end;
    finally
      SaveOnServerDialog.free;
      SaveOnServerDialog := nil;
    end;
  finally
    res.Free;
  end;
end;

procedure TFHIRPlugin.FuncPUT;
var
  fmt : TFHIRFormat;
  res : TFHIRResourceV;
  base, tail : String;
  conn : TFHIRNppServerConnection;
  rs : TRegisteredFHIRServer;
begin
  if FCurrentFileInfo = nil then
    exit;
  if FCurrentFileInfo.url = '' then
  begin
    FuncPOST;
    exit;
  end;

  if (parse(0, fmt, res)) then
  try
    tail := res.fhirType+'/'+res.id;
    if not FCurrentFileInfo.url.endsWith(tail) then
      ShowMessage('Unable to save resource as te ID doesn''t match')
    else
    begin
      base := FCurrentFileInfo.url.subString(0, FCurrentFileInfo.url.length - tail.Length-1);
      conn := nil;
      if not FContext.connections.TryGetValue(base, conn) then
        // I don't think we can get here..
        ShowMessage('internal error - no server?')
      else
        try
          conn.client.updateResourceV(res);
          SoundBeepOK;
        except
          on e : Exception do
            MessageDlg('Error sending resource to server: '+e.Message,  mtError, [mbok], 0);
        end;
    end;
  finally
    res.Free;
  end;
end;

procedure TFHIRPlugin.FuncToolbox;
begin
  if (not Assigned(FHIRToolbox)) then
  begin
    FHIRToolbox := TFHIRToolbox.Create(self, 1);
    FHIRToolbox.Context := FContext.Link;
  end;
  FHIRToolbox.Show;
end;

procedure TFHIRPlugin.FuncTransaction;
var
  id : String;
  fmt : TFHIRFormat;
  r, res : TFHIRResourceV;
  s : TStringStream;
  comp : TFHIRComposer;
begin
  showMessage('not done yet');
(*  if (FClient = nil) then
  begin
    MessageDlg('You must connect to a server first', mtInformation, [mbok], 0);
    exit;
  end;
  if (parse(0, fmt, res)) then
  try
    r.id := '';
    if r.ResourceType <> frtBundle then
      ShowMessage('This is not a Bundle')
    else
    begin
      res := FClient.transaction(r as TFhirBundle);
      try
        if (MessageDlg('Success. Open transaction response?', mtConfirmation, mbYesNo, 0) = mrYes) then
        begin
          comp := FClient.makeComposer(FClient.format, OutputStylePretty);
          try
            s := TStringStream.Create;
            try
              comp.Compose(s, res);
              NewFile(s.DataString);
              saveFileAs(IncludeTrailingPathDelimiter(SystemTemp)+CODES_TFhirResourceType[res.ResourceType]+'-'+res.id+EXT_ACTUAL_TFHIRFormat[FClient.format]);
            finally
              s.Free;
            end;
          finally
            comp.Free;
          end;
        end;
      finally
        res.Free;
      end;
    end;
  finally
    r.Free;
  end
  else
    ShowMessage(parseError);
    *)
end;

function TFHIRPlugin.parse(timeLimit: integer; var fmt: TFHIRFormat; var res: TFHIRResourceV): boolean;
var
  src : TBytes;
  s : TStringStream;
  prsr : TFHIRParser;
begin
  if FCurrentFileInfo.Format = ffUnspecified then
    exit(false);

  result := true;
  fmt := FCurrentFileInfo.Format;
  src := CurrentBytes;
  if (length(src) = 0) then
    exit(false);

  FLastParseError := '';
  prsr := FContext.Version[FCurrentFileInfo.workingVersion].makeParser(fmt);
  try
    prsr.timeLimit := timeLimit;
    prsr.KeepLineNumbers := true;
    try
      res := prsr.parseResource(src);
    except
      // actually, we don't care why this excepted.
      on e : Exception do
      begin
        FLastParseError := e.message;
        exit(false);
      end;
    end;
  finally
    prsr.Free;
  end;
end;

function TFHIRPlugin.parse(cnt: String; fmt: TFHIRFormat): TFHIRResourceV;
var
  prsr : TFHIRParser;
  s : TStringStream;
begin
  s := TStringStream.Create(cnt, TEncoding.UTF8);
  try
    prsr := FContext.Version[FCurrentFileInfo.workingVersion].makeParser(FCurrentFileInfo.Format);
    try
      prsr.KeepLineNumbers := false;
      prsr.source := s;
      prsr.Parse;
      result := prsr.resource.Link;
    finally
      prsr.Free;
    end;
  finally
    s.free;
  end;
end;

{function TFHIRPlugin.parse(timeLimit : integer; var fmt : TFHIRFormat; var res : TFHIRResourceV) : boolean;
var
  src : String;
  s : TStringStream;
  prsr : TFHIRParser;
begin
  src := CurrentText;
  fmt := determineFormat(src);
  res := nil;
  result := fmt <> ffUnspecified;
  if (result) then
  begin
    s := TStringStream.Create(src);
    try
      prsr := FContext.Version[FCurrentFileInfo.workingVersion].makeParser(FCurrentFileInfo.Format);
      try
        prsr.timeLimit := timeLimit;
        prsr.KeepLineNumbers := true;
        prsr.source := s;
        try
          prsr.Parse;
        except
          // actually, we don't care why this excepted.
          on e : Exception do
          begin
            exit(false);
          end;
        end;
        res := prsr.resource.Link as TFHIRResource;
      finally
        prsr.Free;
      end;
    finally
      s.free;
    end;
  end;
end;
}
procedure TFHIRPlugin.refreshStatus;
begin
  AnalyseFile(true);
end;

procedure TFHIRPlugin.reset;
begin
  SetLength(FLastSrc, 1);
  FLastSrc[0] := 1;
end;

procedure TFHIRPlugin.SetDebuggerSetting(name: TFHIRPathDebuggerFormSetting; value: Integer);
begin
  case name of
    settingDebuggerHeight: Settings.DebuggerHeight := value;
    settingDebuggerWidth: Settings.DebuggerWidth := value;
    settingDebuggerSourceHeight: Settings.DebuggerSourceHeight := value;
    settingDebuggerBreaksWidth: Settings.DebuggerBreaksWidth := value;
    settingDebuggerActivePage: Settings.DebuggerActivePage := value;
  end;
end;

procedure TFHIRPlugin.SetDebuggerSettingStr(name: TFHIRPathDebuggerFormSetting; value: String);
begin
  case name of
    settingDebuggerBreaksWidth : settings.DebuggerBreaks := value;
  end;
end;

procedure TFHIRPlugin.SetSelection(start, stop: integer);
begin
  SendMessage(NppData.ScintillaMainHandle, SCI_SETSEL, start, stop);
end;

procedure TFHIRPlugin.setUpSquiggles;
begin
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETSTYLE, INDIC_INFORMATION, INDIC_SQUIGGLE));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETSTYLE, INDIC_WARNING, INDIC_SQUIGGLE));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETSTYLE, INDIC_ERROR, INDIC_SQUIGGLE));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETSTYLE, INDIC_MATCH, INDIC_SQUIGGLE));

  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_INFORMATION, $770000));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_WARNING, $7777FF));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_ERROR, $000077));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_MATCH, $007700));

  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_SETMOUSEDWELLTIME, 200, 0));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_ANNOTATIONSETVISIBLE, 2, 0));

{  squiggle(INDIC_INFORMATION, 0, 3, );
  squiggle(INDIC_WARNING, 4, 3);
  squiggle(INDIC_ERROR, 8, 3);
  squiggle(INDIC_MATCH, 11, 3);}
end;

{function TFHIRPlugin.showOutcomes(fmt : TFHIRFormat; items : TFHIRObjectList; expr : TFHIRPathExpressionNode; types : TFslStringSet): string;
var
  comp : TFHIRExpressionNodeComposer;
begin
  comp := TFHIRExpressionNodeComposer.create(FContext.Version[FCurrentFileInfo.workingVersion].Worker.link, OutputStylePretty, THTTPLanguages.create('en'));
  try
    result := comp.Compose(expr, fmt, items, types);
  finally
    comp.Free;
  end;
end;
}

procedure TFHIRPlugin.squiggle(level, line, start, length: integer; annotation : boolean; message : String);
var
  chars, styles : TBytes;
  i : integer;
begin
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_SETINDICATORCURRENT, level, 0));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICATORFILLRANGE, start, length));
  if annotation then
  begin
    chars := TEncoding.UTF8.GetBytes(message);
    SetLength(styles, System.length(chars));
    for i := 0 to System.length(styles)-1 do
      styles[i] := level;

    mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_ANNOTATIONSETTEXT, line, LPARAM(@chars[0])));
    mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_ANNOTATIONSETSTYLES, line, LPARAM(@styles[0])));
  end;
end;


procedure TFHIRPlugin.TreatAsVersion(v: TFHIRVersion);
var
  a : TFHIRVersion;
begin
  for a in FHIR_ALL_VERSIONS do
    if FCurrentFileInfo.FVersions[a] = vsSpecified then
      FCurrentFileInfo.FVersions[a] := vsValid;
  FCurrentFileInfo.FVersions[v] := vsSpecified;
  DoNppnBufferChange;
end;

function TFHIRPlugin.checkSource(src: Tbytes; v: TFHIRVersion; fmt: TFHIRFormat): TFHIRVersionStatus;
var
  p : TFHIRParser;
  r : TFHIRResourceV;
begin
  if FContext.VersionLoading[v] <> vlsLoaded then
    exit(vsInvalid);

  FLastParseError := '';
  try
    p := FContext.Version[v].makeParser(fmt);
    try
      r := p.parseResource(src);
      try
        if r.profileVersion = v then
          result := vsSpecified
        else
          result := vsValid;
      finally
        r.Free;
      end;
    finally
      p.Free;
    end;
  except
    on e : Exception do
    begin
      result := vsInvalid;
      FLastParseError := e.message;
    end;
  end;
end;

procedure TFHIRPlugin.checkTrigger;
begin
  if FWantUpdate then
  begin
    FWantUpdate := false;
    FCurrentFileInfo := nil;
    FFileInfo.Clear;
    DoNppnBufferChange;
  end
  else
    GBackgroundTasks.primaryThreadCheck;
end;

function TFHIRPlugin.checkContext(version: TFHIRVersion): boolean;
begin
  result := FContext.VersionLoading[version] =  vlsLoaded;
end;


function TFHIRPlugin.waitForContext(version: TFHIRVersion; manualOp : boolean): boolean;
var
  status : TFHIRVersionLoadingStatus;
begin
  result := true;
  status := FContext.VersionLoading[version];
  while status <> vlsLoaded do
  begin
    if status = vlsLoadingFailed then
      raise EFHIRException.create('Unable to load definitions for release '+CODES_TFHIRVersion[Version]);
    if status = vlsNotSupported then
      raise EFHIRException.create('Release '+CODES_TFHIRVersion[Version]+' is not supported (or Loaded)can be changed in plug-in settings');
    if status = vlsNotLoaded then
      raise EFHIRException.create('Release '+CODES_TFHIRVersion[Version]+' is not Loaded');
    if manualop then
      sleep(1000)
    else
      exit(false);
    status := FContext.VersionLoading[version];
  end;
end;

procedure TFHIRPlugin.AnalyseFile(revisit : boolean);
var
  info : TFHIRPluginFileInformation;
  src : TBytes;
  a : TFHIRVersion;
begin
  if not FFileInfo.TryGetValue(currentFileName, info) then
  begin
    info := TFHIRPluginFileInformation.Create;
    FFileInfo.AddOrSetValue(currentFileName, info.Link);
    info.Init(fPreset);
  end;
  FCurrentFileInfo := info;
  if (revisit) or ((info.status = pfsUnknown) or ((info.status = pfsResource) and (info.format = ffUnspecified))) then
  begin
    src := CurrentBytes;
    if (length(src) > 0) then
    begin
      if info.format = ffUnspecified then
        info.Format := determineFormat(src);
      if info.format <> ffUnspecified then
        info.status := pfsResource
      else if length(src) > 1024 then // if it's too long, we give up on it for efficiency reasons, otherwise we keep trying in case it turns into a valid resource
        info.status := pfsNonFHIR;

      if info.Format <> ffUnspecified then
        for a := fhirVersionRelease2 to fhirVersionRelease4 do
          if info.FVersions[a] = vsInvalid then
            info.FVersions[a] := checkSource(src, a, info.Format);
    end;
  end;
  if FHIRToolbox <> nil then
    FHIRToolbox.updateStatus(info.Format, info.Versions, FContext.VersionLoading[info.workingVersion] = vlsLoaded, info.url);
end;

procedure TFHIRPlugin.clearSquiggle(level, line, start, length: integer);
begin
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_SETINDICATORCURRENT, level, 0));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICATORCLEARRANGE, start, length));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_ANNOTATIONSETTEXT, line, 0));
end;

destructor TFHIRPlugin.Destroy;
begin
  GBackgroundTasks.stopAll;
  if FetchResourceFrm <> nil then
    FetchResourceFrm.Free;
  FCurrentServer.Free;
  FLastRes.free;
  FFileInfo.Free;
  inherited;
end;

procedure TFHIRPlugin.DoNppnReady;
begin
  init := true;
  Settings := TFHIRPluginSettings.create(IncludeTrailingPathDelimiter(GetPluginsConfigDir)+'FHIR.Npp.Plugin.json',
    [fhirVersionRelease2, fhirVersionRelease3, fhirVersionRelease4]);
  if (Settings.TerminologyServerR2 = '') then
    Settings.TerminologyServerR2 := 'http://tx.fhir.org/r2';
  if (Settings.TerminologyServerR3 = '') then
    Settings.TerminologyServerR3 := 'http://tx.fhir.org/r3';
  if (Settings.TerminologyServerR4 = '') then
    Settings.TerminologyServerR4 := 'http://tx.fhir.org/r4';
  if Settings.loadR2 then
    FContext.VersionLoading[fhirVersionRelease2] := vlsLoading;
  if Settings.loadR3 then
    FContext.VersionLoading[fhirVersionRelease3] := vlsLoading;
  if Settings.loadR4 then
    FContext.VersionLoading[fhirVersionRelease4] := vlsLoading;
  reset;
  if Settings.loadR2 and FContext.Cache.packageExists('hl7.fhir.r2.core', '1.0') then
    GBackgroundTasks.queueTask(FLoader2Task, TContextLoadingEngineTask.Create(self, TFHIRFactoryR2.create, Settings.TerminologyServerR2))
  else
    FContext.VersionLoading[fhirVersionRelease2] := vlsNotLoaded;
  if Settings.loadR3 and FContext.Cache.packageExists('hl7.fhir.r3.core', '3.0') then
    GBackgroundTasks.queueTask(FLoader3Task, TContextLoadingEngineTask.Create(self, TFHIRFactoryR3.create, Settings.TerminologyServerR3))
  else
    FContext.VersionLoading[fhirVersionRelease3] := vlsNotLoaded;
  if Settings.loadR4 and FContext.Cache.packageExists('hl7.fhir.r4.core', '4.0') then
    GBackgroundTasks.queueTask(FLoader4Task, TContextLoadingEngineTask.Create(self, TFHIRFactoryR4.create, Settings.TerminologyServerR4))
  else
    FContext.VersionLoading[fhirVersionRelease4] := vlsNotLoaded;
  GBackgroundTasks.queueTask(FUpgradeTask, TFslObject.create);
  if not Settings.NoWelcomeForm then
    ShowWelcomeForm(self);

  if Settings.VisualiserVisible then
    FuncVisualiser;
  if Settings.ToolboxVisible then
    FuncToolbox;
  if (FHIRToolbox <> nil) and (FCurrentFileInfo <> nil) then
    FHIRToolbox.updateStatus(FCurrentFileInfo.Format, FCurrentFileInfo.Versions, FContext.VersionLoading[FCurrentFileInfo.workingVersion] = vlsLoaded, FCurrentFileInfo.url);

  DoNppnBufferChange;
end;

procedure TFHIRPlugin.DoNppnShutdown;
begin
  inherited;
  try
    Settings.ShuttingDown := true;
    errors.Free;
    matches.Free;
    errorSorter.Free;
    FreeAndNil(FetchResourceFrm);
    FreeAndNil(FHIRToolbox);
    FreeAndNil(FHIRVisualizer);
    FContext.Free;
    Settings.Free;
  except
    // just hide it
  end;
end;

procedure TFHIRPlugin.DoNppnBufferChange;
begin
  try
    AnalyseFile(false);

    FuncValidateClear;
    FuncMatchesClear;
    DoNppnTextModified;
  except
  end;
end;

procedure TFHIRPlugin.DoNppnDwellEnd;
begin
  if tipShowing then
    mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_CALLTIPCANCEL, 0, 0));
end;

procedure TFHIRPlugin.DoNppnDwellStart(offset: integer);
var
  msg : TStringBuilder;
  annot : TFHIRAnnotation;
  first : boolean;
  chars : TBytes;
begin
  first := true;
  msg := TStringBuilder.Create;
  try
    for annot in errors do
    begin
      if (annot.start <= offset) and (annot.stop >= offset) then
      begin
        if first then
          first := false
        else
          msg.AppendLine;
        msg.Append(annot.message);
      end;
      if annot.start > offset then
        break;
    end;
    for annot in matches do
    begin
      if (annot.start <= offset) and (annot.stop >= offset) then
      begin
        if first then
          first := false
        else
          msg.AppendLine;
        msg.Append(annot.message);
      end;
      if annot.start > offset then
        break;
    end;
    if not first then
    begin
      chars := TEncoding.UTF8.GetBytes(msg.ToString);
      mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_CALLTIPSHOW, offset, LPARAM(chars)));
    end;
  finally
    msg.Free;
  end;
end;

procedure TFHIRPlugin.DoNppnFilebeforeClose;
begin
  FFileInfo.Remove(currentFileName);
  FCurrentFileInfo := nil;
end;

procedure TFHIRPlugin.DoNppnFileClosed;
begin
  DoNppnBufferChange;
end;

procedure TFHIRPlugin.DoNppnFileOpened;
begin
end;

procedure TFHIRPlugin.evaluatePath(r : TFHIRResourceV; out items : TFHIRSelectionList; out expr : TFHIRPathExpressionNodeV; out types : TFHIRTypeDetailsV);
var
  engine : TFHIRPathEngineV;
begin
  if not waitForContext(FCurrentFileInfo.workingVersion, true) then
    exit;
  engine := FContext.Version[FCurrentFileInfo.workingVersion].makePathEngine;
  try
    expr := engine.parseV(FHIRToolbox.edtPath.Text);
    try
      types := engine.check(nil, r.fhirType, r.fhirType, FHIRToolbox.edtPath.Text, expr, false);
      try
        items := engine.evaluate(nil, r, expr);
        types.Link;
      finally
        types.Free;
      end;
      expr.Link;
    finally
      expr.Free;
    end;
  finally
    engine.Free;
  end;
end;

function prepNarrativeHtml(s : String): String; overload;
begin
  result := '<html><body>'+s+'</body></html>';
end;

function TFHIRPlugin.prepNarrative(r : TFHIRResourceV): String;
var
  x : TFhirXHtmlNode;
begin
  x := FContext.Version[FCurrentFileInfo.workingVersion].Factory.getXhtml(r);
  if x = nil then
    result := prepNarrativeHtml('')
  else
    result := prepNarrativeHtml(TFHIRXhtmlParser.compose(x));
end;


procedure TFHIRPlugin.processLoadingResults(id: integer; response: TBackgroundTaskPackage);
begin
  FWantUpdate := true;
end;

procedure TFHIRPlugin.processUpgradeResults(id: integer; response: TBackgroundTaskPackage);
var
  outcome : TUpgradeCheckEngineOutput;
begin
  outcome := response as TUpgradeCheckEngineOutput;
  if outcome.go then
    ShowUpgradeprompt(self, outcome.reference, outcome.notes);
end;

procedure TFHIRPlugin.processValidatorResults(id: integer; response: TBackgroundTaskPackage);
var
  issues : TFslList<TFhirOperationOutcomeIssueW>;
  iss : TFhirOperationOutcomeIssueW;
  error : TFHIRAnnotation;
begin
  issues := response as TFslList<TFhirOperationOutcomeIssueW>;
  if issues <> nil then
  begin
    FuncValidateClear;
    for iss in Issues do
      errors.add(convertIssue(iss));
    setUpSquiggles;
    for error in errors do
      squiggle(LEVEL_INDICATORS[error.level], error.line, error.start, error.stop - error.start, Settings.ValidationAnnotations, error.message);
    if FHIRVisualizer <> nil then
      FHIRVisualizer.setValidationOutcomes(errors);
  end;
end;

procedure TFHIRPlugin.DoNppnTextModified;
var
  src : TBytes;
  path, fn : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  res : TFHIRResourceV;
  items : TFHIRSelectionList;
  expr : TFHIRPathExpressionNodeV;
  types : TFHIRTypeDetailsV;
  item : TFHIRSelection;
  focus : TArray<TFHIRObject>;
  sp, ep : integer;
  annot : TFHIRAnnotation;
  i : integer;
  b : TFslBuffer;
begin
  try
    if not init then
      exit;

    if (FCurrentFileInfo = nil) or (FCurrentFileInfo.Format = ffUnspecified) then
      AnalyseFile(false);

    if FCurrentFileInfo.Format = ffUnspecified then
      exit;

    if not checkContext(FCurrentFileInfo.workingVersion) then
      exit;

    src := CurrentBytes;
    if not FRedoMatches and SameBytes(src, FLastSrc) then
      exit;
    if (length(src) = 1) and (src[0] = 0) then
      exit;

    FRedoMatches := false;
    FLastSrc := src;
    FLastRes.free;
    FLastRes := nil;
    if not (parse(500, fmt, res)) then
    begin
      if (FHIRVisualizer <> nil) then
        case VisualiserMode of
          vmNarrative: FHIRVisualizer.setNarrative(prepNarrativeHtml(''));
          vmPath: FHIRVisualizer.setPathOutcomes('', nil, nil);
          vmFocus: FHIRVisualizer.setFocusInfo('', []);
        end;
      FCurrentFileInfo.Format := ffUnspecified;
    end
    else
    try
      if (Settings.BackgroundValidation) then
        GBackgroundTasks.queueTask(FValidatorTask, TBackgroundValidatorEngineTask.Create(FCurrentFileInfo.workingVersion, fmt, src));
      FLastRes := res.Link;
      if res = nil then
        case VisualiserMode of
          vmNarrative: FHIRVisualizer.setNarrative(prepNarrativeHtml(''));
          vmPath: FHIRVisualizer.setPathOutcomes('', nil, nil);
          vmFocus: FHIRVisualizer.setFocusInfo('', []);
        end
      else
      begin
        if (FHIRVisualizer <> nil) and (VisualiserMode = vmNarrative) then
          FHIRVisualizer.setNarrative(prepNarrative(res));
        if (FHIRVisualizer <> nil) and (VisualiserMode = vmFocus) then
        begin
          if locate(res, path, focus) then
            FHIRVisualizer.setFocusInfo(path, focus)
          else
            FHIRVisualizer.setFocusInfo('', []);
        end;
        if assigned(FHIRToolbox) and (FHIRToolbox.hasValidPath)  then
        begin
          FuncMatchesClear;
          evaluatePath(res, items, expr, types);
          try
            for item in items do
            begin
              sp := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, item.value.LocationStart.line - 1, item.value.LocationStart.col-1);
              ep := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, item.value.LocationEnd.line - 1, item.value.LocationEnd.col-1);
              if (ep = sp) then
                ep := sp + 1;
              matches.Add(TFHIRAnnotation.create(alMatch, item.value.LocationStart.line - 1, sp, ep, 'This element is a match to path "'+FHIRToolbox.edtPath.Text+'"', item.value.describe));
            end;
            if (FHIRVisualizer <> nil) and (VisualiserMode = vmPath) then
              FHIRVisualizer.setPathOutcomes(FHIRToolbox.edtPath.Text, matches, expr);
            setUpSquiggles;
            for annot in matches do
              squiggle(LEVEL_INDICATORS[annot.level], annot.line, annot.start, annot.stop - annot.start, false, '');
            FRedoMatches := false;
          finally
            items.Free;
            expr.Free;
            types.Free;
          end;
        end
        else if FHIRVisualizer <> nil then
          FHIRVisualizer.setPathOutcomes('', nil, nil);
      end;
    finally
      res.Free;
    end;
  except
      on e: exception do
        FLastParseError := e.message;
  end;
end;


(*
function TFHIRPlugin.DoSmartOnFHIR(server : TRegisteredFHIRServer) : boolean;
var
  mr : integer;
begin
  result := false;
  SmartOnFhirLoginForm := TSmartOnFhirLoginForm.Create(self);
  try
    SmartOnFhirLoginForm.logoPath := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(HInstance)))+'npp.png';
    SmartOnFhirLoginForm.Server := server.Link;
    SmartOnFhirLoginForm.scopes := 'openid profile user/*.*';
    SmartOnFhirLoginForm.handleError := true;
    mr := SmartOnFhirLoginForm.ShowModal;
    if mr = mrOK then
    begin
      FClient.SmartToken := SmartOnFhirLoginForm.Token.Link;
      result := true;
    end
    else if (mr = mrAbort) and (SmartOnFhirLoginForm.ErrorMessage <> '') then
      MessageDlg(SmartOnFhirLoginForm.ErrorMessage, mtError, [mbok], 0);
  finally
    SmartOnFhirLoginForm.Free;
  end;
end;
*)

procedure TFHIRPlugin.DoStateChanged;
var
  src : TBytes;
  path : String;
  focus : TArray<TFHIRObject>;
begin
  src := CurrentBytes;
  if src <> FLastSrc then
    DoNppnTextModified;
  // k. all up to date with FLastRes
  if (VisualiserMode  = vmFocus) and (FLastRes <> nil) then
  begin
    if locate(FLastRes, path, focus) then
      FHIRVisualizer.setFocusInfo(path, focus)
    else
      FHIRVisualizer.setFocusInfo('', []);
  end;
end;

function TFHIRPlugin.parse(cnt: TBytes; fmt: TFHIRFormat): TFHIRResourceV;
var
  prsr : TFHIRParser;
begin
  prsr := FContext.Version[FCurrentFileInfo.workingVersion].makeParser(FCurrentFileInfo.Format);
  try
    prsr.KeepLineNumbers := false;
    result := prsr.parseResource(cnt);
  finally
    prsr.Free;
  end;
end;

function TFHIRPlugin.parseError: String;
begin
  if FLastParseError <> '' then
    result := 'This content does not appear to be valid FHIR content ('+FLastParseError+')'
  else
    result := 'This content does not appear to be valid FHIR content'
end;

{ TUpgradeCheckEngine }

function TUpgradeCheckEngine.getServerLink(doc : TMXmlDocument) : string;
var
  e1, e2, e3 : TMXmlElement;
begin
  e1 := doc.docElement.firstElement;
  e2 := e1.firstElement;
  while (e2.Name <> 'item') do
    e2 := e2.nextElement;
  e3 := e2.firstElement;
  while (e3 <> nil) and (e3.Name <> 'link') do
    e3 := e3.nextElement;
  if (e3 = nil) then
    result := ''
  else
    result := e3.text;
end;

function TUpgradeCheckEngine.getUpgradeNotes(doc : TMXmlDocument; current : String) : string;
var
  e1, e2, e3 : TMXmlElement;
begin
  e1 := doc.docElement.firstElement;
  e2 := e1.firstElement;
  while (e2.Name <> 'item') do
    e2 := e2.nextElement;
  result := '';
  while (e2 <> nil) and (e2.Name = 'item') do
  begin
    e3 := e2.firstElement;
    while (e3.Name <> 'link') do
      e3 := e3.nextElement;
    if e3.text = current then
      exit;
    e3 := e2.firstElement;
    while (e3.Name <> 'description') do
      e3 := e3.nextElement;
    result := result + e3.text + #13#10;
    e2 := e2.nextElement;
  end;
end;

function TUpgradeCheckEngine.name: String;
begin
  result := 'Upgrade Checker';
end;

function TUpgradeCheckEngine.output: TUpgradeCheckEngineOutput;
begin
  result := response as TUpgradeCheckEngineOutput;
end;

procedure TUpgradeCheckEngine.Execute;
var
  web : TFslWinInetClient;
  doc : TMXmlDocument;
begin
  response := TUpgradeCheckEngineOutput.create;
  try
    web := TFslWinInetClient.Create;
    try
      web.UseWindowsProxySettings := true;
      web.Server := 'www.healthintersections.com.au';
      web.Resource := 'FhirServer/fhirnpp.rss';
      web.Response := TFslBuffer.Create;
      web.Execute;
      doc := TMXmlParser.parse(web.Response.AsBytes, [xpDropWhitespace, xpDropComments]);
      try
        output.reference := getServerLink(doc);
        output.notes  := getUpgradeNotes(doc, 'http://www.healthintersections.com.au/FhirServer/npp-install-1.0.'+inttostr(BuildCount)+'.exe');
        output.go := (output.reference > 'http://www.healthintersections.com.au/FhirServer/npp-install-1.0.'+inttostr(BuildCount)+'.exe') and (output.reference <> Settings.BuildPrompt);
      finally
        doc.Free;
      end;
    finally
      web.free;
    end;
  except
    // never complain
  end;
end;

{ TFHIRPluginFileInformation }

procedure TFHIRPluginFileInformation.init(preset : TFHIRPluginFileInformation);
var
  v : TFHIRVersion;
begin
  for v in FHIR_ALL_VERSIONS do
    FVersions[v] := vsInvalid;
  if preset <> nil then
  begin
    Format := preset.format;
    for v in FHIR_ALL_VERSIONS do
      FVersions[v] := preset.versions[v];
    FUrl := preset.url;
  end;
end;

function TFHIRPluginFileInformation.link: TFHIRPluginFileInformation;
begin
  result := TFHIRPluginFileInformation(inherited link);
end;

function TFHIRPluginFileInformation.summary: String;
begin
  if Format = ffUnspecified then
    exit('Not a FHIR Resource');

  result := CODES_TFHIRFormat[Format];
//  case FVersionStatus of
//    vsUnknown: result := result+ ', version unknown';
//    vsGuessed: result := result+ ', version might be '+CODES_TFHIRVersion[Version];
//    vsSpecified: result := result+ ', version = R'+CODES_TFHIRVersion[Version];
//  end;
end;

function TFHIRPluginFileInformation.workingVersion: TFHIRVersion;
var
  v : TFHIRVersion;
begin
  result := fhirVersionRelease3;
  for v in SUPPORTED_VERSIONS do
  begin
    if FVersions[v] = vsSpecified then
      exit(v);
    if FVersions[v] = vsValid then
      result := v;
  end;
end;

{ TContextLoadingEngine }

procedure TContextLoadingEngine.Execute;
var
  vf : TFHIRNppVersionFactory;
  ctxt : TFHIRWorkerContextWithFactory;
  rset : TFslStringSet;
begin
  vf := TFHIRNppVersionFactory.Create(input.factory.link);
  try
    try
      input.plugin.FContext.Version[input.factory.version] := vf.Link;
      ctxt := TFHIRPluginValidatorContext.create(input.factory.link, input.TerminologyServer);
      try
        // limit the amount of resource types loaded for convenience...
        rset := TFslStringSet.Create(['StructureDefinition', 'CodeSystem', 'ValueSet']);
        try
          input.Plugin.Context.Cache.loadPackage(input.Factory.corePackage, input.Factory.versionString, rset, ctxt.loadInfo);
        finally
          rset.Free;
        end;
        vf.Worker := ctxt.Link;
      finally
        ctxt.Free;
      end;
      input.Plugin.FContext.VersionLoading[input.Factory.version] := vlsLoaded;
      response := TFslObject.Create;
    except
      on e : Exception do
      begin
        input.Plugin.FContext.VersionLoading[input.Factory.version] := vlsLoadingFailed;
        vf.error := e.Message;
      end;
    end;
  finally
    vf.Free;
  end;
end;

function TContextLoadingEngine.input: TContextLoadingEngineTask;
begin
  result := request as TContextLoadingEngineTask;
end;

function TContextLoadingEngine.name: String;
begin
  result := 'Context Loader';

end;

{ TBackgroundValidatorEngine }

procedure TBackgroundValidatorEngine.execute;
var
  ctxt: TFHIRValidatorContext;
  val : TFHIRValidatorV;
  op : fhir4_resources.TFhirOperationOutcomeIssue; // version doesn't matter
begin
//  if not FNpp.waitForContext(input.Version, false) then
//    exit;
//  ctxt := TFHIRValidatorContext.Create;
//  try
//    ctxt.ResourceIdRule := risOptional;
//    ctxt.OperationDescription := 'validate';
//    val := FNpp.FContext.Version[input.Version].makeValidator;
//    try
//      val.OnProgress := validatorCheck;
//      try
//        val.validate(ctxt, input, input.Format);
//      except
//        on e : exception do
//        begin
//          if not (e is EAbort) then
//          begin
//            op := TFhirOperationOutcomeIssue.Create;
//            try
//              op.severity := IssueSeverityFatal;
//              op.details := TFhirCodeableConcept.Create;
//              op.details.text := 'Validation Process Failed: '+e.Message;
//              ctxt.Issues.add(TFhirOperationOutcomeIssue4.create(op.Link));
//            finally
//              op.Free;
//            end;
//          end;
//        end;
//      end;
//      response := ctxt.Issues.link;
//    finally
//      val.free;
//    end;
//  finally
//    ctxt.free;
//  end;
  response := TFslList<TFhirOperationOutcomeIssueW>.create;
end;

function TBackgroundValidatorEngine.input: TBackgroundValidatorEngineTask;
begin
  result := request as TBackgroundValidatorEngineTask;
end;

function TBackgroundValidatorEngine.name: String;
begin
  result := 'Background Validator';
end;

procedure TBackgroundValidatorEngine.validatorCheck(sender: TObject; message: String);
begin
  progress(message, -1);
end;

{ TBackgroundValidatorEngineTask }

constructor TBackgroundValidatorEngineTask.create(version: TFHIRVersion; format: TFHIRFormat; src: TBytes);
begin
  inherited create;
  FVersion := version;
  FFormat := format;
  AsBytes := src;
end;

{ TContextLoadingEngineTask }

constructor TContextLoadingEngineTask.Create(plugin: TFHIRPlugin; factory: TFHIRFactory; TerminologyServer: string);
begin
  inherited create;
  FPlugin := plugin;
  FFactory := factory;
  FTerminologyServer := TerminologyServer;
end;

initialization
  FNpp := TFHIRPlugin.Create;
end.


