unit FHIRPlugin;


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
  NppPlugin, SciSupport,
  GuidSupport, FileSupport, SystemSupport,
  AdvObjects, AdvGenerics, AdvBuffers, AdvWinInetClients,
  ParserSupport, XmlBuilder, MsXml, MsXmlParser, TextUtilities,

  FHIRBase, FHIRValidator, FHIRResources, FHIRTypes, FHIRParser, FHIRParserBase, FHIRUtilities, FHIRClient, FHIRConstants,
  FHIRPluginSettings, FHIRPluginValidator, FHIRNarrativeGenerator, FHIRPath, FHIRXhtml, FHIRContext,
  SmartOnFhirUtilities, SmartOnFhirLogin, nppBuildcount, PluginUtilities,
  FHIRToolboxForm, AboutForms, SettingsForm, NewResourceForm, FetchResourceForm, PathDialogForms, ValidationOutcomes, CodeGenerationForm,
  FHIRVisualiser, FHIRPathDebugger, WelcomeScreen, UpgradePrompt, DifferenceEngine, ResDisplayForm;

const
  INDIC_INFORMATION = 21;
  INDIC_WARNING = 22;
  INDIC_ERROR = 23;
  INDIC_MATCH = 24;
  LEVEL_INDICATORS : array [TFHIRAnnotationLevel] of Integer = (INDIC_INFORMATION, INDIC_WARNING, INDIC_ERROR, INDIC_MATCH);


type
  TFHIRPlugin = class;

  TUpgradeCheckThread = class(TThread)
  private
    FPlugin : TFHIRPlugin;
    function getServerLink(doc: IXMLDOMDocument2): string;
    function loadXml(b: TAdvBuffer): IXMLDOMDocument2;
    function getUpgradeNotes(doc: IXMLDOMDocument2; current: String): string;
  public
    constructor Create(plugin : TFHIRPlugin);
    procedure Execute(); override;
  end;

  TFHIRPlugin = class(TNppPlugin)
  private
    tipShowing : boolean;
    tipText : AnsiString;
    errors : TAdvList<TFHIRAnnotation>;
    matches : TAdvList<TFHIRAnnotation>;
    errorSorter : TFHIRAnnotationComparer;
    FWorker : TFHIRWorkerContext;
    FValidator : TFHIRValidator;
    FClient : TFhirHTTPClient;
    FCapabilityStatement : TFhirCapabilityStatement;
    init : boolean;
    FLastSrc : String;
    FLastRes : TFHIRResource;
    FUpgradeReference : String;
    FUpgradeNotes : String;
    FCurrentServer : TRegisteredFHIRServer;
    nonFHIRFiles : TStringList;

    function getDefPath(version : TDefinitionsVersion): String;

    // this procedure handles validation.
    // it is called whene the text of the scintilla buffer changes
    // first task is to clear any existing error notifications - if there is a reset
    // second task is to abort any existing validation process
    // third task is to start valdiating
    procedure NotifyContent(text : String; reset : boolean);

    // Scintilla control
    procedure setUpSquiggles;
    procedure squiggle(level : integer; line, start, length : integer; message : String);
    procedure clearSquiggle(level : integer; line, start, length : integer);

    // fhir stuff
    function determineFormat(src : String) : TFHIRFormat;
    procedure loadValidator;
    function convertIssue(issue: TFhirOperationOutcomeIssue): TFHIRAnnotation;
    function findPath(path : String; loc : TSourceLocation; context : TArray<TFHIRObject>; base : TFHIRObject; var focus : TArray<TFHIRObject>) : String;
    function locate(res : TFHIRResource; var path : String; var focus : TArray<TFHIRObject>) : boolean;
    function parse(timeLimit : integer; var fmt : TFHIRFormat; var res : TFHIRResource) : boolean; overload;

    function parse(cnt : String; fmt : TFHIRFormat) : TFHIRResource; overload;
    function compose(cnt : TFHIRResource; fmt : TFHIRFormat) : String; overload;

    procedure evaluatePath(r : TFHIRResource; out items : TFHIRSelectionList; out expr : TFHIRPathExpressionNode; out types : TFHIRTypeDetails);
    function showOutcomes(fmt : TFHIRFormat; items : TFHIRObjectList; expr : TFHIRPathExpressionNode; types : TAdvStringSet) : string;

    // smart on fhir stuff
    function DoSmartOnFHIR(server : TRegisteredFHIRServer) : boolean;
    procedure configureSSL;

    // version tracking
    procedure launchUpgradeCheck;
    procedure CheckUpgrade;


    // background validation
    procedure validate(r : TFHIRResource);
  public
    constructor Create;
    destructor Destroy; override;

    function connected : boolean;

    // user interface
    procedure FuncValidate;
    procedure FuncValidateClear;
    procedure FuncMatchesClear;
    procedure FuncToolbox;
    procedure FuncVisualiser;
    procedure FuncSettings;
    procedure FuncAbout;
    procedure FuncFormat;
    procedure FuncDebugPath;
    procedure FuncJumpToPath;
    procedure FuncExtractPath;
    procedure FuncServers;
    procedure FuncConnect;
    procedure FuncNewResource;
    procedure FuncOpen;
    procedure FuncPUT;
    procedure FuncPOST;
    procedure FuncTransaction;
    procedure FuncServerValidate;
    procedure FuncNarrative;
    procedure FuncDisconnect;
    procedure funcDifference;
    procedure funcGenerateCode;

    procedure reset;
    procedure SetSelection(start, stop : integer);

    // responding to np++ events
    procedure DoNppnReady; override; // install toolbox if necessary
    procedure DoNppnTextModified; override;
    procedure DoNppnBufferChange; override;
    procedure DoNppnDwellStart(offset : integer); override;
    procedure DoNppnDwellEnd; override;
    procedure DoNppnShutdown; override;
    procedure DoStateChanged; override;
    procedure DoNppnFileOpened; override;
    procedure DoNppnFileClosed; override;
  end;

procedure _FuncValidate; cdecl;
procedure _FuncValidateClear; cdecl;
procedure _FuncToolbox; cdecl;
procedure _FuncVisualiser; cdecl;
procedure _FuncAbout; cdecl;
procedure _FuncSettings; cdecl;
procedure _FuncDebugPath; cdecl;
procedure _FuncExtractPath; cdecl;
procedure _FuncJumpToPath; cdecl;
procedure _FuncFormat; cdecl;
procedure _FuncServers; cdecl;
procedure _FuncConnect; cdecl;
procedure _FuncNewResource; cdecl;
procedure _FuncOpen; cdecl;
procedure _FuncPUT; cdecl;
procedure _FuncPOST; cdecl;
procedure _FuncTransaction; cdecl;
procedure _FuncServerValidate; cdecl;
procedure _FuncNarrative; cdecl;
procedure _FuncDisconnect; cdecl;
procedure _FuncDebug; cdecl;
procedure _FuncDifference; cdecl;
procedure _FuncGenerateCode; cdecl;

var
  FNpp: TFHIRPlugin;

implementation

uses
  IdSSLOpenSSLHeaders;

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
  nonFHIRFiles := TStringList.create;
  nonFHIRFiles.Sorted := true;
  nonFHIRFiles.Duplicates := dupIgnore;
  errors := TAdvList<TFHIRAnnotation>.create;
  errorSorter := TFHIRAnnotationComparer.create;
  errors.Sort(errorSorter);
  matches := TAdvList<TFHIRAnnotation>.create;
  matches.Sort(errorSorter);

  self.PluginName := '&FHIR';
  i := 0;

{  sk.IsCtrl := true;
  sk.IsAlt := true;
  sk.Key := 'F';}

  self.AddFuncItem('&About the FHIR Plugin', _FuncAbout);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('Change &Format (XML <--> JSON)', _FuncFormat);
  self.AddFuncItem('&Validate Resource', _FuncValidate);
  self.AddFuncItem('Clear Validation Information', _FuncValidateClear);
  self.AddFuncItem('&Make Patch', _FuncDifference);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('&Jump to Path', _FuncJumpToPath);
  self.AddFuncItem('&Debug Path Expression', _FuncDebugPath);
  self.AddFuncItem('&Extract Path from Cursor', _FuncExtractPath);
  self.AddFuncItem('Generate &Code', _FuncGenerateCode);

  self.AddFuncItem('-', Nil);
  self.AddFuncItem('Connect to &Server', _FuncConnect);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('&New Resource (Template)', _FuncNewResource);
  self.AddFuncItem('&Open Resource on Server', _FuncOpen);
  self.AddFuncItem('P&UT resource to existing ID', _FuncPUT);
  self.AddFuncItem('&POST resource to new ID', _FuncPOST);
  self.AddFuncItem('POST resource as a &Transaction', _FuncTransaction);
  self.AddFuncItem('Validate &resource on server', _FuncServerValidate);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('Confi&gure Tools', _FuncSettings);
  self.AddFuncItem('Vie&w Toolbox', _FuncToolbox);
  self.AddFuncItem('View Visuali&zer', _FuncVisualiser);
  self.AddFuncItem('Debug Install', _FuncDebug);

  configureSSL;
end;

function TFHIRPlugin.compose(cnt: TFHIRResource; fmt: TFHIRFormat): String;
var
  s : TStringStream;
  comp : TFHIRComposer;
begin
  s := TStringStream.Create;
  try
    if fmt = ffXml then
      comp := TFHIRXmlComposer.Create(FWorker.link, OutputStylePretty, 'en')
    else
      comp := TFHIRJsonComposer.Create(FWorker.link, OutputStylePretty, 'en');
    try
      comp.Compose(s, cnt);
      result := s.DataString;
    finally
     comp.free;
    end;
  finally
    s.Free;
  end;
end;

procedure TFHIRPlugin.configureSSL;
begin
  IdOpenSSLSetLibPath(IncludeTrailingBackslash(ExtractFilePath(GetModuleName(HInstance)))+'ssl');
end;

procedure _FuncValidate; cdecl;
begin
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
  FNpp.FuncSettings;
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
  FNpp.FuncFormat;
end;

procedure _FuncServers; cdecl;
begin
  FNpp.FuncServers;
end;

procedure _FuncConnect; cdecl;
begin
  FNpp.FuncConnect;
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

procedure _FuncDisconnect; cdecl;
begin
  FNpp.FuncDisconnect;
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
    s := s + 'config: '+IncludeTrailingBackslash(FNpp.GetPluginsConfigDir)+'fhirplugin.json';
    s := s + 'init: '+BoolToStr(FNpp.init)+#13#10;
    s := s + 'validator: '+ inttohex(cardinal(FNpp.FValidator), 8)+ #13#10;
    s := s + 'client: '+ inttohex(cardinal(FNpp.FClient), 8)+ #13#10;
    s := s + 'conformance: '+ inttohex(cardinal(FNpp.FCapabilityStatement), 8)+ #13#10;
    s := s + 'server: '+ inttohex(cardinal(FNpp.FCurrentServer), 8)+ #13#10;
  except
    on e : exception do
      s := s + 'exception: '+e.Message;
  end;
  ShowMessage(s);
end;

function TFHIRPlugin.connected: boolean;
begin
  result := FClient <> nil;
end;

function TFHIRPlugin.convertIssue(issue : TFhirOperationOutcomeIssue) : TFHIRAnnotation;
var
  s, e : integer;
  msg : String;
begin
  s := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, StrToIntDef(issue.Tags['s-l'], 1)-1, StrToIntDef(issue.Tags['s-c'], 1)-1);
  e := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, StrToIntDef(issue.Tags['e-l'], 1)-1, StrToIntDef(issue.Tags['e-c'], 1)-1);
  if (e = s) then
    e := s + 1;
  msg := issue.diagnostics;
  if (issue.details <> nil) and (issue.details.text <> '') then
    msg := issue.details.text;
  case issue.severity of
    IssueSeverityWarning : result := TFHIRAnnotation.create(alWarning, StrToIntDef(issue.Tags['s-l'], 1)-1, s, e, msg, msg);
    IssueSeverityInformation : result := TFHIRAnnotation.create(alHint, StrToIntDef(issue.Tags['s-l'], 1)-1, s, e, msg, msg);
  else
    result := TFHIRAnnotation.create(alError, StrToIntDef(issue.Tags['s-l'], 1)-1, s, e, msg, msg);
  end;
end;

procedure TFHIRPlugin.FuncValidate;
var
  src : String;
  buffer : TAdvBuffer;
  error : TFHIRAnnotation;
  op : TFHIROperationOutcome;
  iss : TFhirOperationOutcomeIssue;
  fmt : TFHIRFormat;
  ctxt: TFHIRValidatorContext;
begin
  FuncValidateClear;
  src := CurrentText;
  fmt := determineFormat(src);
  if (fmt <> ffUnspecified) then
  begin
    try
      buffer := TAdvBuffer.Create;
      try
        buffer.AsUnicode := src;
        loadValidator;
        ctxt := TFHIRValidatorContext.Create;
        try
          ctxt.ResourceIdRule := risOptional;
          ctxt.OperationDescription := 'validate';
          FValidator.validate(ctxt, buffer, fmt, nil);
          op := FValidator.describe(ctxt);
        finally
          ctxt.Free;
        end;
      finally
        buffer.Free;
      end;
      for iss in op.issueList do
        errors.add(convertIssue(iss));
      if not ValidationSummary(self, op) then
        MessageBeep(MB_OK);
    except
      on e: Exception do
      begin
        if not ValidationError(self, e.message) then
          errors.Add(TFHIRAnnotation.create(alError, 0, 0, 4, e.Message, e.Message));
      end;
    end;
  end
  else if not ValidationError(self, 'This does not appear to be valid FHIR content') then
    errors.Add(TFHIRAnnotation.create(alError, 0, 0, 4, 'This does not appear to be valid FHIR content', ''));
  setUpSquiggles;
  for error in errors do
    squiggle(LEVEL_INDICATORS[error.level], error.line, error.start, error.stop - error.start, error.message);
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
  tipText := '';
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
  tipText := '';
end;

procedure TFHIRPlugin.FuncVisualiser;
begin
  if (not Assigned(FHIRVisualizer)) then FHIRVisualizer := TFHIRVisualizer.Create(self, 2);
  FHIRVisualizer.Show;
end;

function TFHIRPlugin.getDefPath(version: TDefinitionsVersion): String;
begin
  case version of
    defV2 : result := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(HInstance)))+'fhir\fhir2-definitions.zip';
    defV3 : result := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(HInstance)))+'fhir\fhir3-definitions.zip';
  else
    raise Exception.Create('not done yet');
  end;
  if not fileExists(result) then
    case version of
      defV3 : result := 'C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu3\build\publish\igpack.zip';
      defV2 : result := 'C:\work\org.hl7.fhir.old\org.hl7.fhir.dstu3\build\publish\definitions-r2asr3.xml.zip';
    else
      raise Exception.Create('not done yet');
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

function TFHIRPlugin.determineFormat(src: String): TFHIRFormat;
var
  s : String;
begin
  result := ffUnspecified; // null
  src := src.Trim;
  if (src <> '') then
    begin
    if src[1] = '<' then
    begin
      while src.StartsWith('<!') or src.StartsWith('<?') do
        splitElement(src);
      s := splitElement(src);
      if s.Contains('"http://hl7.org/fhir"') then
        result := ffXml
      else if s.Contains('''http://hl7.org/fhir''') then
        result := ffXml
    end
    else if src[1] = '{' then
    begin
      if src.Contains('"resourceType"') then
        result := ffJson;
    end;
  end;
end;

procedure TFHIRPlugin.launchUpgradeCheck;
begin
  TUpgradeCheckThread.create(self);
end;

procedure TFHIRPlugin.loadValidator;
var
  s : String;
begin
  if FValidator = nil then
  begin
    if (Settings.TerminologyServer = '') then
      Settings.TerminologyServer := 'http://tx.fhir.org/r3';

    OpMessage('Loading', 'Loading Definition Source');
    try
      TFHIRPluginValidatorContext(FWorker).LoadFromDefinitions(getDefPath(Settings.DefinitionsVersion));
      if Settings.AdditionalDefinitions <> '' then
        for s in Settings.AdditionalDefinitions.Split([',']) do
          TFHIRPluginValidatorContext(FWorker).LoadFromFile(s);
      FValidator := TFHIRValidator.Create(FWorker.link);
    finally
      OpMessage('', '');
    end;
  end;
end;

function TFHIRPlugin.locate(res: TFHIRResource; var path: String; var focus : TArray<TFHIRObject>): boolean;
var
  sp : integer;
  loc : TSourceLocation;
begin
  sp := SendMessage(NppData.ScintillaMainHandle, SCI_GETCURRENTPOS, 0, 0);
  loc.line := SendMessage(NppData.ScintillaMainHandle, SCI_LINEFROMPOSITION, sp, 0)+1;
  loc.col := sp - SendMessage(NppData.ScintillaMainHandle, SCI_POSITIONFROMLINE, loc.line-1, 0)+1;
  path := findPath(CODES_TFHIRResourceType[res.ResourceType], loc, [], res, focus);
  result := path <> '';
end;

procedure TFHIRPlugin.FuncServers;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncServerValidate;
begin
  if (FClient = nil) then
  begin
    MessageDlg('You must connect to a server first', mtInformation, [mbok], 0);
    exit;
  end;
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncGenerateCode;
var
  fmt : TFHIRFormat;
  s : TStringStream;
  res : TFHIRResource;
  comp : TFHIRComposer;
begin
  if not init then
    exit;
  loadValidator;
  if (parse(0, fmt, res)) then
  try
    CodeGeneratorForm := TCodeGeneratorForm.create(self);
    try
      CodeGeneratorForm.Resource := res.Link;
      CodeGeneratorForm.Context := FValidator.Context.Link;
      CodeGeneratorForm.showModal;
    finally
      CodeGeneratorForm.free;
    end;
  finally
    res.Free;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.FuncSettings;
var
  a: TSettingForm;
begin
  a := TSettingForm.Create(self);
  try
    a.ShowModal;
  finally
    a.Free;
  end;
end;

procedure TFHIRPlugin.FuncAbout;
var
  a: TAboutForm;
begin
  nonFHIRFiles.clear;
  a := TAboutForm.Create(self);
  try
    a.lblDefinitions.Caption := 'Definitions Source: '+getDefPath(Settings.DefinitionsVersion);
    a.ShowModal;
  finally
    a.Free;
  end;
end;

procedure TFHIRPlugin.FuncConnect;
var
  index : integer;
  server : TRegisteredFHIRServer;
  ok : boolean;
begin
  index := 0;
  if (Assigned(FHIRToolbox)) then
    index := FHIRToolbox.cbxServers.ItemIndex;
  server := Settings.serverInfo('', index);
  try
    try
      try
        OpMessage('Connecting to Server', 'Connecting to Server '+server.fhirEndpoint);
        FClient := TFhirHTTPClient.Create(FWorker.link, server.fhirEndpoint, false);
        FClient.timeout := 5000;
        FClient.allowR2 := true;
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
            FClient.json := false;
            FCapabilityStatement := FClient.conformance(false);
          except
            FClient.json := not FClient.Json;
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


function TFHIRPlugin.findPath(path : String; loc : TSourceLocation; context : TArray<TFHIRObject>; base : TFHIRObject; var focus : TArray<TFHIRObject>) : String;
var
  i, j : integer;
  pl : TFHIRPropertyList;
  p : TFHIRProperty;
  list : TArray<TFHIRObject>;
begin
  setlength(list, length(context) + 1);
  for i := 0 to length(context) - 1 do
    list[i] := context[i];
  list[length(list)-1] := base;

  if locLessOrEqual(loc, base.LocationEnd) then
  begin
    result := path;
    focus := list;
  end
  else
  begin
    result := '';
    pl := base.createPropertyList(false);
    try
      for i := pl.Count - 1 downto 0 do
      begin
        p := pl[i];
        if (p.hasValue) and locGreatorOrEqual(loc, p.Values[0].LocationStart) then
        begin
          path := path + '.'+p.Name;
          if p.IsList then
          begin
            for j := p.Values.Count - 1 downto 0 do
              if (result = '') and locGreatorOrEqual(loc, p.Values[j].LocationStart) then
                result := findPath(path+'.item('+inttostr(j)+')', loc, list, p.Values[j], focus);
          end
          else
            result := findPath(path, loc, list, p.Values[0], focus);
          break;
        end;
      end;
    finally
      pl.Free;
    end;
  end;
end;

procedure TFHIRPlugin.FuncExtractPath;
var
  fmt : TFHIRFormat;
  res : TFHIRResource;
  sp : integer;
  focus : TArray<TFHIRObject>;
  loc : TSourceLocation;
begin
  if assigned(FHIRToolbox) and (parse(0, fmt, res)) then
  try
    sp := SendMessage(NppData.ScintillaMainHandle, SCI_GETCURRENTPOS, 0, 0);
    loc.line := SendMessage(NppData.ScintillaMainHandle, SCI_LINEFROMPOSITION, sp, 0)+1;
    loc.col := sp - SendMessage(NppData.ScintillaMainHandle, SCI_POSITIONFROMLINE, loc.line-1, 0)+1;
    FHIRToolbox.mPath.Text := findPath(CODES_TFHIRResourceType[res.ResourceType], loc, [], res, focus);
  finally
    res.Free;
  end;
end;

procedure TFHIRPlugin.FuncFormat;
var
  fmt : TFHIRFormat;
  s : TStringStream;
  res : TFHIRResource;
  comp : TFHIRComposer;
begin
  if not init then
    exit;
  if (parse(0, fmt, res)) then
  try
    FuncValidateClear;
    FuncMatchesClear;
    if fmt = ffJson then
      comp := TFHIRXmlComposer.Create(FWorker.link, OutputStylePretty, 'en')
    else
      comp := TFHIRJsonComposer.Create(FWorker.link, OutputStylePretty, 'en');
    s := TStringStream.Create('');
    try
      comp.Compose(s, res);
      CurrentText := s.DataString;
    finally
      s.Free;
      comp.Free;
    end;
  finally
    res.Free;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.FuncJumpToPath;
var
  fmt : TFHIRFormat;
  res : TFHIRResource;
  items : TFHIRSelectionList;
  expr : TFHIRPathExpressionNode;
  engine : TFHIRPathEngine;
  sp, ep : integer;
begin
  if assigned(FHIRToolbox) and (FHIRToolbox.hasValidPath) and parse(0, fmt, res) then
  try
    loadValidator;
    engine := TFHIRPathEngine.Create(FWorker.Link);
    try
      expr := engine.parse(FHIRToolbox.mPath.Text);
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
      engine.Free;
    end;
  finally
    res.Free;
  end;
end;

procedure TFHIRPlugin.FuncNarrative;
var
  buffer : TAdvBuffer;
  fmt : TFHIRFormat;
  s : TStringStream;
  res : TFHIRResource;
  comp : TFHIRComposer;
  d : TFhirDomainResource;
  narr : TFHIRNarrativeGenerator;
begin
  if (parse(0, fmt, res)) then
  try
    FuncValidateClear;
    FuncMatchesClear;
    if (res is TFhirDomainResource) then
    begin
      d := res as TFhirDomainResource;
      d.text := nil;
      loadValidator;
      narr := TFHIRNarrativeGenerator.Create(FWorker.link);
      try
        narr.generate(d);
      finally
        narr.Free;
      end;
    end;

    if fmt = ffXml then
      comp := TFHIRXmlComposer.Create(FWorker.link, OutputStylePretty, 'en')
    else
      comp := TFHIRJsonComposer.Create(FWorker.link, OutputStylePretty, 'en');
    try
      s := TStringStream.Create('');
      try
        comp.Compose(s, res);
        CurrentText := s.DataString;
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
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.FuncNewResource;
begin
  loadValidator;
  ResourceNewForm := TResourceNewForm.Create(self);
  try
    ResourceNewForm.Context := FWorker.Link;
    ResourceNewForm.ShowModal;
  finally
    FreeAndNil(ResourceNewForm);
  end;
end;

procedure TFHIRPlugin.FuncOpen;
var
  res : TFHIRResource;
  comp : TFHIRComposer;
  s : TStringStream;
begin
  if (FClient = nil) then
  begin
    MessageDlg('You must connect to a server first', mtInformation, [mbok], 0);
    exit;
  end;
  loadValidator;
  if not assigned(FetchResourceFrm) then
    FetchResourceFrm := TFetchResourceFrm.create(self);
  FetchResourceFrm.Conformance := FCapabilityStatement.link;
  FetchResourceFrm.Client := FClient.link;
  FetchResourceFrm.Profiles := TFHIRPluginValidatorContext(FWorker).Profiles.Link;
  if FetchResourceFrm.ShowModal = mrOk then
  begin
    res := FClient.readResource(FetchResourceFrm.SelectedType, FetchResourceFrm.SelectedId);
    try
      if FetchResourceFrm.rbJson.Checked then
        comp := TFHIRJsonComposer.Create(FWorker.link, OutputStylePretty, 'en')
      else
        comp := TFHIRXmlComposer.Create(FWorker.link, OutputStylePretty, 'en');
      try
        s := TStringStream.Create;
        try
          comp.Compose(s, res);
          NewFile(s.DataString);
          if FetchResourceFrm.rbJson.Checked then
            saveFileAs(IncludeTrailingPathDelimiter(SystemTemp)+CODES_TFhirResourceType[res.ResourceType]+'-'+res.id+'.json')
          else
            saveFileAs(IncludeTrailingPathDelimiter(SystemTemp)+CODES_TFhirResourceType[res.ResourceType]+'-'+res.id+'.xml');
        finally
          s.Free;
        end;
      finally
        comp.Free;
      end;
    finally
      res.Free;
    end;
  end;
end;

procedure TFHIRPlugin.FuncDebugPath;
var
  src : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  res : TFHIRResource;
  query : TFHIRPathEngine;
  item : TFHIRSelection;
  allSource : boolean;
  sp, ep : integer;
  annot : TFHIRAnnotation;
  types : TFHIRTypeDetails;
  items : TFHIRSelectionList;
  expr : TFHIRPathExpressionNode;
  ok : boolean;
begin
  FuncMatchesClear;
  loadValidator;

  if (parse(0, fmt, res)) then
  try
    FuncMatchesClear;
    ok := RunPathDebugger(self, FWorker, res, res, FHIRToolbox.mPath.Text, fmt, types, items);
    try
      if ok then
      begin
        allSource := true;
        for item in items do
          allSource := allSource and not isNullLoc(item.value.LocationStart);

        if Items.Count = 0 then
          pathOutcomeDialog(self, FHIRToolbox.mPath.Text, CODES_TFHIRResourceType[res.ResourceType], types, pomNoMatch, 'no items matched')
        else if not allSource then
          pathOutcomeDialog(self, FHIRToolbox.mPath.Text, CODES_TFHIRResourceType[res.ResourceType], types, pomNoMatch, query.convertToString(items))
        else
        begin
          if (items.Count = 1) then
            pathOutcomeDialog(self, FHIRToolbox.mPath.Text, CODES_TFHIRResourceType[res.ResourceType], types, pomMatch, '1 matching item')
          else
            pathOutcomeDialog(self, FHIRToolbox.mPath.Text, CODES_TFHIRResourceType[res.ResourceType], types, pomMatch, inttostr(items.Count)+' matching items');
        end;
      end;
    finally
      types.Free;
      items.Free;
    end;
  finally
    res.Free;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.funcDifference;
var
  current, original, output : string;
  fmtc, fmto : TFHIRFormat;
  rc, ro, op : TFHIRResource;
  diff : TDifferenceEngine;
  html : String;
begin
  try
    loadValidator;
    current := CurrentText;
    fmtc := determineFormat(current);
    if (fmtc = ffUnspecified) then
      raise Exception.Create('Unable to parse current content');
    original := FileToString(CurrentFileName, TEncoding.UTF8);
    fmto := determineFormat(current);
    if (fmto = ffUnspecified) then
      raise Exception.Create('Unable to parse original file');

    rc := parse(current, fmtc);
    try
      ro := parse(original, fmto);
      try
        diff := TDifferenceEngine.Create(FValidator.Context.link);
        try
          op := diff.generateDifference(ro, rc, html);
          try
            output := compose(op, fmtc);
            ShowResource(self, 'Difference', html, output);
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
  res : TFHIRResource;
  comp : TFHIRComposer;
begin
  if (FClient = nil) then
  begin
    MessageDlg('You must connect to a server first', mtInformation, [mbok], 0);
    exit;
  end;
  if (parse(0, fmt, res)) then
  try
    FuncValidateClear;
    FuncMatchesClear;
    FClient.createResource(res, id).Free;
    res.id := id;
    if fmt = ffXml then
      comp := TFHIRXmlComposer.Create(FWorker.link, OutputStylePretty, 'en')
    else
      comp := TFHIRJsonComposer.Create(FWorker.link, OutputStylePretty, 'en');
    try
      s := TStringStream.Create('');
      try
        comp.Compose(s, res);
        CurrentText := s.DataString;
      finally
        s.Free;
      end;
    finally
      comp.Free;
    end;
    if fmt = ffJson then
      saveFileAs(IncludeTrailingPathDelimiter(ExtractFilePath(currentFileName))+CODES_TFhirResourceType[res.ResourceType]+'-'+id+'.json')
    else
      saveFileAs(IncludeTrailingPathDelimiter(ExtractFilePath(currentFileName))+CODES_TFhirResourceType[res.ResourceType]+'-'+id+'.xml');
    ShowMessage('POST completed. The resource ID has been updated');
  finally
    res.Free;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.FuncPUT;
var
  src : String;
  fmt : TFHIRFormat;
  res : TFHIRResource;
begin
  if (FClient = nil) then
  begin
    MessageDlg('You must connect to a server first', mtInformation, [mbok], 0);
    exit;
  end;
  if (parse(0, fmt, res)) then
  try
    if (res.id = '') then
      ShowMessage('Cannot PUT this as it does not have an id')
    else
    begin
      FClient.updateResource(res);
      ShowMessage('PUT succeded')
    end;
  finally
    res.Free;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.FuncToolbox;
begin
  if (not Assigned(FHIRToolbox)) then FHIRToolbox := TFHIRToolbox.Create(self, 1);
  FHIRToolbox.Show;
end;

procedure TFHIRPlugin.FuncTransaction;
var
  id : String;
  fmt : TFHIRFormat;
  r, res : TFHIRResource;
  s : TStringStream;
  comp : TFHIRComposer;
begin
  if (FClient = nil) then
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
          if FClient.Json then
            comp := TFHIRJsonComposer.Create(FWorker.link, OutputStylePretty, 'en')
          else
            comp := TFHIRXmlComposer.Create(FWorker.link, OutputStylePretty, 'en');
          try
            s := TStringStream.Create;
            try
              comp.Compose(s, res);
              NewFile(s.DataString);
              if FClient.Json then
                saveFileAs(IncludeTrailingPathDelimiter(SystemTemp)+CODES_TFhirResourceType[res.ResourceType]+'-'+res.id+'.json')
              else
                saveFileAs(IncludeTrailingPathDelimiter(SystemTemp)+CODES_TFhirResourceType[res.ResourceType]+'-'+res.id+'.xml');
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
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.NotifyContent(text: String; reset: boolean);
begin
  squiggle(INDIC_ERROR, 0, 2, 4, 'test');
end;

function TFHIRPlugin.parse(cnt: String; fmt: TFHIRFormat): TFHIRResource;
var
  prsr : TFHIRParser;
  s : TStringStream;
begin
  s := TStringStream.Create(cnt, TEncoding.UTF8);
  try
    if fmt = ffXml then
      prsr := TFHIRXmlParser.Create(FWorker.link, 'en')
    else
      prsr := TFHIRJsonParser.Create(FWorker.link, 'en');
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

function TFHIRPlugin.parse(timeLimit : integer; var fmt : TFHIRFormat; var res : TFHIRResource) : boolean;
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
      if fmt = ffXml then
        prsr := TFHIRXmlParser.Create(FWorker.link, 'en')
      else
        prsr := TFHIRJsonParser.Create(FWorker.link, 'en');
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
        res := prsr.resource.Link;
      finally
        prsr.Free;
      end;
    finally
      s.free;
    end;
  end;
end;

procedure TFHIRPlugin.reset;
begin
 FLastSrc := #1;
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

{  squiggle(INDIC_INFORMATION, 0, 3);
  squiggle(INDIC_WARNING, 4, 3);
  squiggle(INDIC_ERROR, 8, 3);
  squiggle(INDIC_MATCH, 11, 3); }
end;

function TFHIRPlugin.showOutcomes(fmt : TFHIRFormat; items : TFHIRObjectList; expr : TFHIRPathExpressionNode; types : TAdvStringSet): string;
var
  comp : TFHIRComposer;
begin
  if fmt = ffXml then
    comp := TFHIRXmlComposer.Create(FWorker.link, OutputStylePretty, 'en')
  else
    comp := TFHIRJsonComposer.Create(FWorker.link, OutputStylePretty, 'en');
  try
    result := comp.Compose(expr, items, types);
  finally
    comp.Free;
  end;
end;

procedure TFHIRPlugin.squiggle(level, line, start, length: integer; message : String);
begin
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_SETINDICATORCURRENT, level, 0));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICATORFILLRANGE, start, length));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_ANNOTATIONSETTEXT, line, LPARAM(PAnsiChar(message))));

end;



procedure TFHIRPlugin.validate(r: TFHIRResource);
begin
  // todo
end;

procedure TFHIRPlugin.CheckUpgrade;
var
  s : String;
begin
  if FUpgradeReference <> '' then
  begin
    s := FUpgradeReference;
    FUpgradeReference := '';
    ShowUpgradeprompt(self, s, FUpgradeNotes);
  end;
end;

procedure TFHIRPlugin.clearSquiggle(level, line, start, length: integer);
begin
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_SETINDICATORCURRENT, level, 0));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICATORCLEARRANGE, start, length));
end;

destructor TFHIRPlugin.Destroy;
begin
  nonFHIRFiles.Free;
  FCurrentServer.Free;
  FLastRes.free;
  inherited;
end;

procedure TFHIRPlugin.DoNppnReady;
begin
  Settings := TFHIRPluginSettings.create(IncludeTrailingPathDelimiter(GetPluginsConfigDir)+'fhirplugin.json');
  FWorker := TFHIRPluginValidatorContext.Create(Settings.TerminologyServer);
  if not Settings.NoWelcomeScreen then
    ShowWelcomeScreen(self);

  if Settings.VisualiserVisible then
    FuncVisualiser;
  if Settings.ToolboxVisible then
    FuncToolbox;
  init := true;
  reset;
  launchUpgradeCheck;
end;

procedure TFHIRPlugin.DoNppnShutdown;
begin
  inherited;
  try
    Settings.ShuttingDown := true;
    FValidator.Free;
    errors.Free;
    matches.Free;
    errorSorter.Free;
    FClient.Free;
    FCapabilityStatement.Free;
    FreeAndNil(FetchResourceFrm);
    FreeAndNil(FHIRToolbox);
    FreeAndNil(FHIRVisualizer);
    FWorker.Free;
    Settings.Free;
  except
    // just hide it
  end;
end;

procedure TFHIRPlugin.DoNppnBufferChange;
begin
  FuncValidateClear;
  FuncMatchesClear;
  DoNppnTextModified;
end;

procedure TFHIRPlugin.DoNppnDwellEnd;
begin
  if tipShowing then
    mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_CALLTIPCANCEL, 0, 0));
  tipText := '';
end;

procedure TFHIRPlugin.DoNppnDwellStart(offset: integer);
var
  msg : TStringBuilder;
  annot : TFHIRAnnotation;
  first : boolean;
begin
  CheckUpgrade;
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
      tipText := msg.ToString;
      mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_CALLTIPSHOW, offset, LPARAM(PAnsiChar(tipText))));
    end;
  finally
    msg.Free;
  end;
end;

procedure TFHIRPlugin.DoNppnFileClosed;
begin
  nonFHIRFiles.Clear;
end;

procedure TFHIRPlugin.DoNppnFileOpened;
begin
  nonFHIRFiles.Clear;
end;

procedure TFHIRPlugin.evaluatePath(r : TFHIRResource; out items : TFHIRSelectionList; out expr : TFHIRPathExpressionNode; out types : TFHIRTypeDetails);
var
  engine : TFHIRPathEngine;
begin
  loadValidator;
  engine := TFHIRPathEngine.Create(FWorker.Link);
  try
    expr := engine.parse(FHIRToolbox.mPath.Text);
    try
      types := engine.check(nil, CODES_TFHIRResourceType[r.ResourceType], CODES_TFHIRResourceType[r.ResourceType], FHIRToolbox.mPath.Text, expr, false);
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

function prepNarrative(s : String): String; overload;
begin
  result := '<html><body>'+s+'</body></html>';
end;

function prepNarrative(r : TFHIRResource): String; overload;
var
  dr : TFHIRDomainResource;
begin
  if (r = nil) or not (r is TFhirDomainResource) then
    result := prepNarrative('')
  else
  begin
    dr := r as TFhirDomainResource;
    if (dr.text = nil) or (dr.text.div_ = nil) then
      result := prepNarrative('')
    else
      result := prepNarrative(TFHIRXhtmlParser.compose(dr.text.div_));
  end;
end;


procedure TFHIRPlugin.DoNppnTextModified;
var
  src, path, fn : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  res : TFHIRResource;
  items : TFHIRSelectionList;
  expr : TFHIRPathExpressionNode;
  types : TFHIRTypeDetails;
  item : TFHIRSelection;
  focus : TArray<TFHIRObject>;
  sp, ep : integer;
  annot : TFHIRAnnotation;
  i : integer;
begin
  CheckUpgrade;
  if not init then
    exit;

  fn := CurrentFileName;
  if nonFHIRFiles.find(fn, i) then
   exit;

  src := CurrentText;
  if src = FLastSrc then
    exit;
  FLastSrc := src;
  FLastRes.free;
  FLastRes := nil;
//    // we need to parse if:
//    //  - we are doing background validation
//    //  - there's a path defined
//    //  - we're viewing narrative
//  else if (Settings.BackgroundValidation or
//          (assigned(FHIRToolbox) and (FHIRToolbox.hasValidPath)) or
//          (VisualiserMode in [vmNarrative, vmFocus])) then
  try
    if not (parse(500, fmt, res)) then
    begin
      if (FHIRVisualizer <> nil) then
        case VisualiserMode of
          vmNarrative: FHIRVisualizer.setNarrative(prepNarrative(''));
          vmPath: FHIRVisualizer.setPathOutcomes(nil, nil);
          vmFocus: FHIRVisualizer.setFocusInfo('', []);
        end;
      nonFHIRFiles.Add(fn);
    end
    else
    try
      FLastRes := res.Link;
      if res = nil then
        case VisualiserMode of
          vmNarrative: FHIRVisualizer.setNarrative(prepNarrative(''));
          vmPath: FHIRVisualizer.setPathOutcomes(nil, nil);
          vmFocus: FHIRVisualizer.setFocusInfo('', []);
        end
      else
      begin
        if (Settings.BackgroundValidation) then
          validate(res);
        if (FHIRVisualizer <> nil) and (VisualiserMode = vmNarrative) then
          FHIRVisualizer.setNarrative(prepNarrative(res));
        if (FHIRVisualizer <> nil) and (VisualiserMode = vmFocus) then
        begin
          if locate(res, path, focus) then
            FHIRVisualizer.setFocusInfo(path, focus)
          else
            FHIRVisualizer.setFocusInfo('', []);
        end;
        if (VisualiserMode = vmPath) then
        begin
          if assigned(FHIRToolbox) and (FHIRToolbox.hasValidPath) and (VisualiserMode = vmPath) then
          begin
            evaluatePath(res, items, expr, types);
            try
              for item in items do
              begin
                sp := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, item.value.LocationStart.line - 1, item.value.LocationStart.col-1);
                ep := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, item.value.LocationEnd.line - 1, item.value.LocationEnd.col-1);
                if (ep = sp) then
                  ep := sp + 1;
                matches.Add(TFHIRAnnotation.create(alMatch, item.value.LocationStart.line - 1, sp, ep, 'This element is a match to path "'+FHIRToolbox.mPath.Text+'"', item.value.describe));
              end;
              if VisualiserMode = vmPath then
                FHIRVisualizer.setPathOutcomes(matches, expr);
              setUpSquiggles;
              for annot in matches do
                squiggle(LEVEL_INDICATORS[annot.level], annot.line, annot.start, annot.stop - annot.start, annot.message);
            finally
              items.Free;
              expr.Free;
              types.Free;
            end;
          end
          else
            FHIRVisualizer.setPathOutcomes(nil, nil);
        end;
      end;
    finally
      res.Free;
    end;
  except
//      on e: exception do
//        showmessage(e.message);
  end;
end;



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

procedure TFHIRPlugin.DoStateChanged;
var
  src, path : String;
  focus : TArray<TFHIRObject>;
begin
  src := CurrentText;
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

{ TUpgradeCheckThread }

constructor TUpgradeCheckThread.Create(plugin: TFHIRPlugin);
begin
  Fplugin := plugin;
  inherited create(false);
end;

function TUpgradeCheckThread.loadXml(b : TAdvBuffer): IXMLDOMDocument2;
var
  v, vAdapter : Variant;
  s : TBytesStream;
begin
  v := LoadMsXMLDom;
  Result := IUnknown(TVarData(v).VDispatch) as IXMLDomDocument2;
  result.validateOnParse := False;
  result.preserveWhiteSpace := True;
  result.resolveExternals := False;
  result.setProperty('NewParser', True);
  s := TBytesStream.Create(b.AsBytes);
  try
    vAdapter := TStreamAdapter.Create(s) As IStream;
    result.load(vAdapter);
  finally
    s.Free;
  end;
end;

function TUpgradeCheckThread.getServerLink(doc : IXMLDOMDocument2) : string;
var
  e1, e2, e3 : IXMLDOMElement;
begin
  e1 := TMsXmlParser.FirstChild(doc.documentElement);
  e2 := TMsXmlParser.FirstChild(e1);
  while (e2.nodeName <> 'item') do
    e2 := TMsXmlParser.NextSibling(e2);
  e3 := TMsXmlParser.FirstChild(e2);
  while (e3 <> nil) and (e3.nodeName <> 'link') do
    e3 := TMsXmlParser.NextSibling(e3);
  if (e3 = nil) then
    result := ''
  else
    result := e3.text;
end;

function TUpgradeCheckThread.getUpgradeNotes(doc : IXMLDOMDocument2; current : String) : string;
var
  e1, e2, e3 : IXMLDOMElement;
begin
  e1 := TMsXmlParser.FirstChild(doc.documentElement);
  e2 := TMsXmlParser.FirstChild(e1);
  while (e2.nodeName <> 'item') do
    e2 := TMsXmlParser.NextSibling(e2);
  result := '';
  while (e2 <> nil) and (e2.nodeName = 'item') do
  begin
    e3 := TMsXmlParser.FirstChild(e2);
    while (e3.nodeName <> 'link') do
      e3 := TMsXmlParser.NextSibling(e3);
    if e3.text = current then
      exit;
    e3 := TMsXmlParser.FirstChild(e2);
    while (e3.nodeName <> 'description') do
      e3 := TMsXmlParser.NextSibling(e3);
    result := result + e3.text + #13#10;
    e2 := TMsXmlParser.NextSibling(e2);
  end;
  result := e3.text;
end;

procedure TUpgradeCheckThread.Execute;
var
  web : TAdvWinInetClient;
  doc : IXMLDOMDocument2;
  bc : string;
begin
  try
    web := TAdvWinInetClient.Create;
    try
      web.UseWindowsProxySettings := true;
      web.Server := 'www.healthintersections.com.au';
      web.Resource := 'FhirServer/fhirnpp.rss';
      web.Response := TAdvBuffer.Create;
      web.Execute;
      doc := loadXml(web.Response);
      bc := getServerLink(doc);
      if (bc > 'http://www.healthintersections.com.au/FhirServer/npp-install-1.0.'+inttostr(BuildCount)+'.exe') and (bc <> Settings.BuildPrompt) then
      begin
        FPlugin.FUpgradeNotes  := getUpgradeNotes(doc, 'http://www.healthintersections.com.au/FhirServer/npp-install-1.0.'+inttostr(BuildCount)+'.exe');
        FPlugin.FUpgradeReference := bc;
      end;
    finally
      web.free;
    end;
  except
    // never complain
  end;
end;

initialization
  FNpp := TFHIRPlugin.Create;
end.

// "C:\Users\Grahame Grieve\AppData\Roaming\Notepad++\plugins\npp.png"

