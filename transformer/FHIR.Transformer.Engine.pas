unit FHIR.Transformer.Engine;

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

{$I fhir.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_objects,  fhir_parser,
  FHIR.Javascript,
  v2_message, v2_javascript,
  cda_objects, cda_javascript,
  fhir4_context, fhir4_factory, fhir4_maputils, fhir4_types, fhir4_resources, fhir4_resources_base, fhir4_elementmodel, fhir4_profiles,
  fhir4_xml, fhir4_json, fhir4_liquid, fhir4_pathengine, fhir4_client, fhir_client_http, fhir4_javascript,
  FHIR.Conversion.Engine, fhir_javascript,
  FHIR.Transformer.Utilities, FHIR.Transformer.Workspace, FHIR.Transformer.Context, FHIR.Transformer.Editor;

type
  TTransformEngine = class;

  TCheckBreakpointEvent = function (line : integer) : boolean of Object;
  TTransformEngineFileEvent = procedure (sender : TTransformEngine; f : TWorkspaceFile) of object;
  TTransformEngineEvent = procedure (sender : TTransformEngine) of object;
  TTransformEngineStatusEvent = procedure (sender : TTransformEngine; color : TColor; msg: String; beep : UInt) of object;
  TTransformEngineLogEvent = procedure (sender : TTransformEngine; msg: String) of object;
  TTransformEngineOpenFileEvent = function (sender : TTransformEngine; f : TWorkspaceFile) : TEditorInformation of object;

  TLocalTransformerServices = class (TTransformerServices)
  private
    FEngine : TTransformEngine; // no link
    FOutcomes: TFslList<TFhirResource>;
    FContext : TFHIRTransformerContext;
    FFactory : TFHIRFactoryR4;
  public
    constructor Create(engine : TTransformEngine; context : TFHIRTransformerContext; factory : TFHIRFactoryR4);
    destructor Destroy; override;
    function link : TLocalTransformerServices; overload;
    property outcomes : TFslList<TFhirResource> read FOutcomes;

    function translate(appInfo : TFslObject; src : TFHIRCoding; conceptMapUrl : String) : TFHIRCoding; override;
    procedure log(s : String); override;
    function performSearch(appInfo : TFslObject; url : String) : TFslList<TFHIRObject>; override;
    function createType(appInfo : TFslObject; tn : String) : TFHIRObject; override;
    procedure createResource(appInfo : TFslObject; res : TFHIRObject; atRootofTransform : boolean); override;
  end;

  TMapBreakpointResolver = class (TFslObject)
  private
    FMap : TFhirStructureMap;
    function checkRule(rule : TFhirStructureMapGroupRule; line : integer) : boolean;
  public
    constructor Create(map : TFhirStructureMap);
    destructor Destroy; override;
    function checkBreakpoint(line : integer) : boolean;
  end;

  TTransformEngineExecutionVariableMode = (tvmNone, tvmInput, tvmOutput);

  TTransformEngineExecutionVariable = class abstract (TFslObject)
  protected
    FEngine : TTransformEngine;
    function GetName: String; virtual; abstract;
    function GetTypeName: String; virtual; abstract;
    function GetMode: TTransformEngineExecutionVariableMode; virtual; abstract;
    function GetValue: String; virtual; abstract;
    function GetObject: TFHIRObject; virtual; abstract;
  public
    destructor Destroy; override;
    function link : TTransformEngineExecutionVariable; overload;
    property name : String read GetName;
    property typeName : String read GetTypeName;
    property mode : TTransformEngineExecutionVariableMode read GetMode;
    property value : String read GetValue;
    property obj : TFHIRObject read GetObject;
  end;

  TTransformEngineExecutionPointKind = (epJavscript, epMap, epMapGroup, epMapRule, epMapTarget, epLiquidStatement, epLiquidTag);
  TTransformEngineExecutionPoint = class abstract (TFslObject)
  private
    FVariables: TFslList<TTransformEngineExecutionVariable>;
  protected
    FEngine : TTransformEngine;
    function GetDescription: String; virtual; abstract;
    function GetKind: TTransformEngineExecutionPointKind; virtual; abstract;
    function GetFile: TWorkspaceFile; virtual; abstract;
    function getStart: TSourceLocation; virtual; abstract;
    function getStop: TSourceLocation; virtual; abstract;
  public
    destructor Destroy; override;
    property desc : String read GetDescription;
    property kind : TTransformEngineExecutionPointKind read GetKind;
    property file_ : TWorkspaceFile read GetFile;
    property start : TSourceLocation read getStart;
    property stop : TSourceLocation read getStop;
    property variables : TFslList<TTransformEngineExecutionVariable> read FVariables;
  end;

  TTransformEngineDebuggingStatus = (tdsRunToBreakpoint, tdsStepOut, tdsStepOver, tdsStepIn);
  TTransformEngineDebugContext = class (TFslObject)
  private
    FCallStack: TFslList<TTransformEngineExecutionPoint>;
    FVariables: TFslList<TTransformEngineExecutionVariable>;
    FPastVariablesAvailable: boolean;
    FStatus: TTransformEngineDebuggingStatus;
  public
    destructor Destroy; override;
    function link : TTransformEngineDebugContext; overload;

    property callStack : TFslList<TTransformEngineExecutionPoint> read FCallStack;
    property variables : TFslList<TTransformEngineExecutionVariable> read FVariables;
    property pastVariablesAvailable : boolean read FPastVariablesAvailable write FPastVariablesAvailable;
    property status : TTransformEngineDebuggingStatus read FStatus write FStatus;
  end;
  TTransformEngineDebugEvent = procedure (sender : TTransformEngine; info : TTransformEngineDebugContext) of object;

  TMapTransformEngineExecutionVariable = class (TTransformEngineExecutionVariable)
  private
    FInfo : TVariable;
  protected
    function GetName: String; override;
    function GetMode: TTransformEngineExecutionVariableMode; override;
    function GetTypeName: String; override;
    function GetValue: String; override;
    function GetObject: TFHIRObject; override;
  public
    constructor Create(engine : TTransformEngine; info : TVariable);
    destructor Destroy; override;
  end;

  TMapTransformEngineExecutionPoint = class (TTransformEngineExecutionPoint)
  private
    FInfo : TFHIRStructureMapDebugContext;
  protected
    function GetDescription: String; override;
    function GetKind: TTransformEngineExecutionPointKind; override;
    function GetFile: TWorkspaceFile; override;
    function getStart: TSourceLocation; override;
    function getStop: TSourceLocation; override;
  public
    constructor Create(engine : TTransformEngine; info : TFHIRStructureMapDebugContext);
    destructor Destroy; override;
  end;


  TTransformEngine = class (TFslObject)
  private
    FOnFileUpdate: TTransformEngineFileEvent;
    FOnStatusMessage: TTransformEngineStatusEvent;
    FOnLog : TTransformEngineLogEvent;
    FOnStateUpdate: TTransformEngineEvent;
    FOnDebug: TTransformEngineDebugEvent;
    FOnOpenFile: TTransformEngineOpenFileEvent;

    FWorkspace: TWorkspace;
    FEditors: TFslList<TEditorInformation>;
    FContext : TFHIRTransformerContext;
    FPathEngine : TFHIRPathEngine;
    FServices : TLocalTransformerServices;
    FRunning: boolean;
    FTerminologyServer: string;
    FTxServer : TFhirClient4;


    function getOpenFile(f : TWorkspaceFile; out editor : TEditorInformation) : boolean;

    function compileV2(f : TWorkspaceFile; src : String) : TFslObject;
    function compileCDA(f : TWorkspaceFile; src : String) : TFslObject;
    function compileResource(f : TWorkspaceFile; src : String) : TFslObject;
    function compileJS(f : TWorkspaceFile; src : String) : TFslObject;
    function compileMap(f : TWorkspaceFile; src : String) : TFslObject;
    function compileTemplate(f : TWorkspaceFile; src : String) : TFslObject;
    procedure compileAll(list : TFslList<TWorkspaceFile>; all : boolean); overload;

    function checkCanRun(executionDetails : TWorkspaceExecConfig; upd : boolean; var s: String): boolean;
    procedure executeCDA(ev : TWorkspaceExecConfig; debug : boolean);
    procedure DebugTransform(sender : TObject; info : TFHIRStructureMapDebugContext);
    procedure SetTerminologyServer(const Value: string);
    procedure CheckTerminologyServer;
    procedure JSLog(sender : TJavascript; message : String);
  public
    constructor Create(workspace : TWorkspace);
    destructor Destroy; override;
    function link : TTransformEngine; overload;

    property workspace : TWorkspace read FWorkspace;
    property editors : TFslList<TEditorInformation> read FEditors;
    property Context : TFHIRTransformerContext read FContext;
    property terminologyServer : string read FTerminologyServer write SetTerminologyServer;

    property running : boolean read FRunning;
    property OnFileUpdate : TTransformEngineFileEvent read FOnFileUpdate write FOnFileUpdate;
    property OnStateUpdate : TTransformEngineEvent read FOnStateUpdate write FOnStateUpdate;
    property OnStatusMessage : TTransformEngineStatusEvent read FOnStatusMessage write FOnStatusMessage;
    property OnDebug : TTransformEngineDebugEvent read FOnDebug write FOnDebug;
    property OnLog : TTransformEngineLogEvent read FOnLog write FOnLog;
    property OnOpenFile : TTransformEngineOpenFileEvent read FOnOpenFile write FOnOpenFile;

    procedure initialise(cache : TResourceMemoryCache);
    procedure setActiveEditor(editor : TEditorInformation);
    procedure clear;
    procedure compileAll; overload;
    procedure compile(f: TWorkspaceFile; openIfFail : boolean);
    function canonical(f: TWorkspaceFile) : String;
    function canRun(executionDetails : TWorkspaceExecConfig; var s : String) : boolean;
    procedure run(executionDetails : TWorkspaceExecConfig);
    procedure debug(executionDetails : TWorkspaceExecConfig);
  end;

(*  TConversionEngine = class abstract (TFslObject)
  private
    FSource: TWorkspaceFile;
    FOnWantSource: TConversionEngineFetchSourceEvent;
    FOnStatus: TConversionEngineStatusEvent;
    FOnLog: TConversionEngineLogEvent;
    FOnCompiled: TConversionEngineCompiledEvent;
    FWorkspace: TWorkspace;
    FMap: TWorkspaceFile;
    FCache: TResourceMemoryCache;
    FOnTransformDebug: TFHIRStructureMapDebugEvent;
    FOutcomes: TFslList<TFhirObject>;
    procedure SetSource(const Value: TWorkspaceFile);
    procedure SetWorkspace(const Value: TWorkspace);
    procedure SetMap(const Value: TWorkspaceFile);
    procedure SetCache(const Value: TResourceMemoryCache);
    procedure relog(sender : TConversionEngine; message : String);
  protected
    FMapUtils : TFHIRStructureMapUtilities;
    FFactory : TFHIRFactoryR4;
    procedure log(msg : String);
    function fetchSource(f : TWorkspaceFile) : TStream;
    function parseMap(f : TWorkspaceFile) : TFhirStructureMap;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TConversionEngine; overload;

    property workspace : TWorkspace read FWorkspace write SetWorkspace;
    property source : TWorkspaceFile read FSource write SetSource;
    property map : TWorkspaceFile read FMap write SetMap;
    property cache : TResourceMemoryCache read FCache write SetCache;
    property OnWantSource : TConversionEngineFetchSourceEvent read FOnWantSource write FOnWantSource;
    property OnStatus : TConversionEngineStatusEvent read FOnStatus write FOnStatus;
    property OnLog : TConversionEngineLogEvent read FOnLog write FOnLog;
    property OnTransformDebug : TFHIRStructureMapDebugEvent read FOnTransformDebug write FOnTransformDebug;
    property OnCompiled: TConversionEngineCompiledEvent read FOnCompiled write FOnCompiled;

    property Outcomes: TFslList<TFhirObject> read FOutcomes;

    procedure load; virtual; abstract;
    procedure execute; virtual; abstract;
  end;

  TCDAConversionEngine = class (TConversionEngine)
  private
  public
    function link : TCDAConversionEngine; overload;

    procedure load; override;
    procedure execute; override;
  end;
  *)
const
  CODES_TTransformEngineExecutionVariableMode : array [TTransformEngineExecutionVariableMode] of String = ('', 'Input', 'Output');

implementation

(*
{ TConversionEngine }

constructor TConversionEngine.Create;
begin
  inherited;
  FOutcomes := TFslList<TFhirObject>.create;
end;

destructor TConversionEngine.Destroy;
begin
  FOutcomes.Free;
  FMapUtils.Free;
  FCache.Free;
  FWorkspace.Free;
  FMap.Free;
  FSource.Free;
  FFactory.Free;
  inherited;
end;

function TConversionEngine.fetchSource(f: TWorkspaceFile): TStream;
begin
  if Assigned(FOnWantSource) then
    result := FOnWantSource(self, f);
  if result = nil then
    result := f.stream;
end;

function TConversionEngine.link: TConversionEngine;
begin
  result := TConversionEngine(inherited Link);
end;

procedure TConversionEngine.log(msg: String);
begin
  if assigned(OnLog) then
    OnLog(self, msg);
end;

function TConversionEngine.parseMap(f: TWorkspaceFile): TFhirStructureMap;
var
  stream : TStream;
begin
  stream := fetchSource(f);
  try
    result := FMapUtils.parse(StreamToString(stream, TEncoding.UTF8), f.title);
  finally
    stream.free;
  end;
end;

procedure TConversionEngine.relog(sender: TConversionEngine; message: String);
begin
  if assigned(FOnLog) then
    FOnLog(self, message);
end;

procedure TConversionEngine.SetCache(const Value: TResourceMemoryCache);
begin
  FCache.Free;
  FCache := Value;
end;

procedure TConversionEngine.SetMap(const Value: TWorkspaceFile);
begin
  FMap.Free;
  FMap := Value;
end;

procedure TConversionEngine.SetSource(const Value: TWorkspaceFile);
begin
  FSource.Free;
  FSource := Value;
end;

procedure TConversionEngine.SetWorkspace(const Value: TWorkspace);
begin
  FWorkspace.Free;
  FWorkspace := Value;
end;

{ TCDAConversionEngine }

procedure TCDAConversionEngine.execute;
//var
//  f : TWorkspaceFile;
//  stream : TStream;
//  map : TFHIRStructureMap;
//  elem : TFHIRMMElement;
//  services : TLocalTransformerServices;
//  mbpr : TMapBreakpointResolver;
//  res : TFhirResource;
begin
//  log('Parse the Workspace Maps');
//  for f in FWorkspace.maps do
//  begin
//    map := parseMap(f);
//    try
//      f.parsed := map;
//      FMapUtils.Lib.Add(map.url, map.Link);
//      if assigned(FOnCompiled) then
//      begin
//        mbpr := TMapbreakpointResolver.create(map.Link);
//        try
//          FOnCompiled(self, f, mbpr.checkBreakPoint);
//        finally
//          mbpr.free;
//        end;
//      end;
//    finally
//      map.free;
//    end;
//  end;
//  try
//    log('Load the CDA Source');
//    stream := fetchSource(FSource);
//    try
//      elem := TFHIRMMManager.parse(FContext, stream, ffXml);
//      try
//        log('Parse the Map');
//        if self.map.parsed <> nil then
//          map := (self.map.parsed as TFhirStructureMap).link
//        else
//          map := parseMap(self.map);
//        try
//          log('Execute the Conversion [url]');
//          services := TLocalTransformerServices.Create(relog, FContext, FFactory.link);
//          try
//            FMapUtils.Services := Services.Link;
//            FMapUtils.OnDebug := FOnTransformDebug;
//            FMapUtils.transform(nil, elem, map, nil);
//            assert(services.outcomes.Count = 1);
//            for res in services.outcomes do
//              FOutcomes.Add(res.Link);
//          finally
//            services.Free;
//          end;
//        finally
//          map.Free;
//        end;
//      finally
//        elem.Free;
//      end;
//    finally
//      stream.free;
//    end;
//  finally
//    FWorkspace.ClearParsedObjects;
//  end;
end;

function TCDAConversionEngine.link: TCDAConversionEngine;
begin
  result := TCDAConversionEngine(inherited Link);
end;

procedure TCDAConversionEngine.load;
//var
//  r : TFhirResource;
begin
//  FFactory := TFHIRFactoryR4.Create;
//  FMapUtils := TFHIRStructureMapUtilities.Create(FContext.Link, TFslMap<TFHIRStructureMap>.create, nil, FFactory.link);
end;
*)

{ TLocalTransformerServices }

constructor TLocalTransformerServices.Create;
begin
  inherited Create;
  FEngine := engine;
  FOutcomes := TFslList<TFhirResource>.create;
  FFactory := factory;
  FContext := context;
end;

procedure TLocalTransformerServices.createResource(appInfo: TFslObject; res: TFHIRObject; atRootofTransform: boolean);
begin
  if (atRootofTransform) then
    FOutcomes.add(TFHIRResource(res).Link);
end;

function TLocalTransformerServices.createType(appInfo: TFslObject; tn: String): TFHIRObject;
var
  sd : TFHIRStructureDefinition;
begin
  sd := FContext.fetchResource(frtStructureDefinition, sdNs(tn)) as TFHIRStructureDefinition;
  try
    if (sd <> nil) and (sd.kind = StructureDefinitionKindLogical) then
    begin
      // result := Manager.build(context, sd);
      raise Exception.Create('Not Done yet');
    end
    else
    begin
      if (tn.startsWith('http://hl7.org/fhir/StructureDefinition/')) then
        tn := tn.substring('http://hl7.org/fhir/StructureDefinition/'.length);
      result := FFactory.makeByName(tn);
    end;
  finally
    sd.free;
  end;
end;

destructor TLocalTransformerServices.Destroy;
begin
  FOutcomes.Free;
  FFactory.Free;
  FContext.Free;
  inherited;
end;

function TLocalTransformerServices.link: TLocalTransformerServices;
begin
  result := TLocalTransformerServices(inherited link);
end;

procedure TLocalTransformerServices.log(s: String);
begin
 FEngine.FOnLog(nil, s);
end;

function TLocalTransformerServices.performSearch(appInfo: TFslObject; url: String): TFslList<TFHIRObject>;
begin
  raise EFHIRException.Create('Not implemented: performSearch');
end;

function TLocalTransformerServices.translate(appInfo: TFslObject; src: TFHIRCoding; conceptMapUrl: String): TFHIRCoding;
begin
  raise Exception.Create('Not done yet');
end;

{ TMapBreakpointResolver }

function TMapBreakpointResolver.checkRule(rule: TFhirStructureMapGroupRule; line: integer): boolean;
var
  tgt : TFhirStructureMapGroupRuleTarget;
  r : TFhirStructureMapGroupRule;
begin
  result := false;
  if rule.LocationStart.line = line then
    exit(true);
  for tgt in rule.targetList do
    if tgt.LocationStart.line = line then
      exit(true);
  for r in rule.ruleList do
    if checkRule(r, line) then
      exit(true);
end;

constructor TMapBreakpointResolver.Create(map: TFhirStructureMap);
begin
  Inherited Create;
  FMap := map;
end;

destructor TMapBreakpointResolver.Destroy;
begin
  FMap.Free;
  inherited;
end;

function TMapBreakpointResolver.checkBreakpoint(line: integer): boolean;
var
  grp : TFhirStructureMapGroup;
  rule : TFhirStructureMapGroupRule;
begin
  line := line + 1;
  result := false;
  for grp in FMap.groupList do
  begin
    if grp.LocationStart.line = line then
      exit(true);
    for rule in grp.ruleList do
    begin
      if checkRule(rule, line) then
        exit(true);
    end;
  end;
end;

{ TTransformEngine }

function TTransformEngine.canonical(f: TWorkspaceFile): String;
var
  ss : TStringStream;
  c : TFHIRComposer;
  mu : TFHIRStructureMapUtilities;
begin
  if f.compileStatus <> csOk then
    compile(f, false);
  if f.compileStatus = csOk then
    case f.format of
      fmtV2: result := TV2Composer.composeString(f.compiled as TV2Message, [v2coEscapeExtendedCharacters]);
      fmtCDA:
        begin
          ss := TStringStream.Create;
          try
            TFHIRMMManager.compose(FContext, f.compiled as TFHIRMMElement, ss, ffXml, OutputStylePretty);
            result := ss.DataString;
          finally
            ss.Free;
          end;
        end;
      fmtResource:
        begin
          if f.fmtInfo = 'xml' then
            c := TFHIRXmlComposer.Create(FContext.Link, OutputStylePretty, FContext.lang)
          else
            c := TFHIRJsonComposer.Create(FContext.Link, OutputStylePretty, FContext.lang);
          try
            result := c.Compose(f.compiled as TFhirResource);
          finally
            c.Free;
          end;
        end;
      fmtJS: raise ELibraryException.Create('Canonical Format is not supported for Javscript');
      fmtMap:
        begin
          mu := TFHIRStructureMapUtilities.Create(FContext.Link, nil, FServices.link, TFHIRFactoryR4.create);
          try
            result := mu.render(f.compiled as TFhirStructureMap);
          finally
            mu.Free;
          end;
        end;
      fmtTemplate:
        result := (f.compiled as TFHIRLiquidDocument).ToString;
    end;
end;

function TTransformEngine.canRun(executionDetails : TWorkspaceExecConfig; var s: String): boolean;
begin
  result := checkCanRun(executionDetails, false, s);
end;

function TTransformEngine.checkCanRun(executionDetails : TWorkspaceExecConfig; upd : boolean; var s: String): boolean;
  function checkFileSet(files : TFslList<TWorkspaceFile>) : boolean;
  var
    f : TWorkspaceFile;
  begin
    if upd then
      compileAll(files, false);
    for f in files do
    begin
      if f.compileStatus in [csError, csNotCompiled] then
      begin
        if f.compileStatus = csNotCompiled then
          s := 'Compilation is not complete yet'
        else
          s := f.title+': '+f.errorMsg;
        exit(false);
      end;
    end;
    result := true;
  end;
var
  f : TWorkspaceFile;
begin
  if FTerminologyServer = '' then
  begin
    s := 'No Terminology Server';
    exit(false);
  end;

  if not checkFileSet(FWorkspace.maps) then
    exit(false);
  if not checkFileSet(FWorkspace.scripts) then
    exit(false);
  if not checkFileSet(FWorkspace.templates) then
    exit(false);
  f := FWorkspace.findFileByName(executionDetails.script);
  if f = nil then
  begin
    s := 'Unable to find '+executionDetails.script;
    exit(false);
  end;
  f := FWorkspace.findFileByName(executionDetails.focus);
  if f = nil then
  begin
    s := 'Unable to find '+executionDetails.script;
    exit(false);
  end;

  if f.compileStatus in [csError, csNotCompiled] then
  begin
    if upd then
      compile(f, false);
    if f.compileStatus = csNotCompiled then
      s := 'Compilation is not complete yet'
    else
      s := f.title+': '+f.errorMsg;
    exit(false);
  end;
  result := true;
end;

procedure TTransformEngine.CheckTerminologyServer;
begin
  if FTxServer = nil then
  begin
    FTxServer := TFhirClient4.Create(FContext.Link, FContext.lang, TFHIRHTTPCommunicator.Create(FTerminologyServer));
    FTxServer.conformance(true); // check it inits ok
  end;
end;

procedure TTransformEngine.clear;
var
  f : TWorkspaceFile;
begin
  for f in FWorkspace.allFiles do
  begin
    f.compileStatus := csNotCompiled;
    f.compiled := nil;
    f.errorLine := -1;
    f.errorMsg := '';
    FOnFileUpdate(self, f);
  end;
end;

procedure TTransformEngine.compile(f: TWorkspaceFile; openIfFail: boolean);
var
  src : string;
  editor : TEditorInformation;
begin
  if not getOpenFile(f, editor) then
    editor := nil;
  if editor <> nil then
  begin
    editor.setErrorLine(-1);
    if editor.focus then
      openIfFail := true;
  end;
  if openIfFail then
    FOnStatusMessage(self, clNavy, 'Checking '+f.title, 0);
  try
    if editor <> nil then
      src := editor.memo.RawText
    else
      src := f.source;
    case f.format of
      fmtV2: f.compiled := compileV2(f, src);
      fmtCDA: f.compiled := compileCDA(f, src);
      fmtResource: f.compiled := compileResource(f, src);
      fmtJS: f.compiled := compileJS(f, src);
      fmtMap: f.compiled := compileMap(f, src);
      fmtTemplate: f.compiled := compileTemplate(f, src);
    end;
    f.compileStatus := csOk;
    if openIfFail then
      FOnStatusMessage(self, clGreen, f.title+' is syntactically valid at '+FormatDateTime('c', now), SOUND_SYSTEM_INFORMATION);
    FOnFileUpdate(self, f);
  except
    on e : EParserException do
    begin
      f.compiled := nil;
      f.compileStatus := csError;
      f.errorLine := e.Line-1;
      f.errorMsg := 'Error Checking: '+e.message;
      if (editor = nil) and openIfFail then
        editor := FOnOpenFile(self, f);
      if editor <> nil then
        editor.SetErrorLine(e.Line-1);
      if openIfFail then
        FOnStatusMessage(self, clMaroon, f.errorMsg, SOUND_SYSTEM_ERROR);
      FOnFileUpdate(self, f);
    end;
    on e : Exception do
    begin
      f.compiled := nil;
      f.compileStatus := csError;
      f.errorMsg := 'Error Checking: '+e.message;
      f.errorLine := -1;
      if (editor = nil) and openIfFail then
        editor := FOnOpenFile(self, f);
      if openIfFail then
        FOnStatusMessage(self, clMaroon, f.errorMsg, SOUND_SYSTEM_ERROR);
      FOnFileUpdate(self, f);
    end;
  end;
end;

procedure TTransformEngine.compileAll(list: TFslList<TWorkspaceFile>; all : boolean);
var
  f : TWorkspaceFile;
begin
  for f in list do
    if all or (f.compileStatus <> csOk) then
      compile(f, false);
end;

procedure TTransformEngine.compileAll;
begin
  compileAll(FWorkspace.messages, true);
  compileAll(FWorkspace.documents, true);
  compileAll(FWorkspace.resources, true);
  compileAll(FWorkspace.scripts, true);
  compileAll(FWorkspace.maps, true);
  compileAll(FWorkspace.templates, true);
end;

function TTransformEngine.compileCDA(f: TWorkspaceFile; src: String) : TFslObject;
var
  stream : TStringStream;
begin
  stream := TStringStream.Create(src, TEncoding.UTF8);
  try
    result := TFHIRMMManager.parse(FContext, stream, ffXml);
  finally
    stream.free;
  end;
end;

function TTransformEngine.compileJS(f: TWorkspaceFile; src: String) : TFslObject;
var
  js : TJavascript;
begin
  js := TJavascript.Create;
  try
    js.compile(src, f.title);
    result := nil; // for now
  finally
    js.Free;
  end;
end;

function TTransformEngine.compileMap(f: TWorkspaceFile; src: String) : TFslObject;
var
  utils : TFHIRStructureMapUtilities;
  map : TFHIRStructureMap;
  mbpr : TMapbreakpointResolver;
begin
  utils := TFHIRStructureMapUtilities.Create(nil, nil, nil, nil);
  try
    map := utils.parse(src, f.title);
    try
      mbpr := TMapbreakpointResolver.create(map.Link);
      try
//        DoCompiled(nil, f, mbpr.checkBreakPoint);
      finally
        mbpr.free;
      end;
      result := map.Link;
    finally
      map.free;
    end;
  finally
    utils.Free;
  end;
end;

function TTransformEngine.compileResource(f: TWorkspaceFile; src: String) : TFslObject;
var
  p : TFHIRParser;
begin
  if isXml(src) then
  begin
    f.fmtInfo := 'xml';
    p := TFHIRXmlParser.Create(nil, FContext.lang);
  end
  else
    p := TFHIRJsonParser.Create(nil, FContext.lang);
  try
    result := p.parseResource(src);
  finally
    p.free;
  end;
end;

function TTransformEngine.compileTemplate(f: TWorkspaceFile; src: String) : TFslObject;
var
  le : TFHIRLiquidEngine;
begin
  le := TFHIRLiquidEngine.Create(FPathEngine.Link);
  try
    result := le.parse(src, f.title);
  finally
    le.Free;
  end;
end;

function TTransformEngine.compileV2(f: TWorkspaceFile; src: String) : TFslObject;
begin
  result := TV2Parser.parse(src, [v2doPreventOddBinaryLengths]);
end;

constructor TTransformEngine.Create(workspace: TWorkspace);
begin
  inherited create;
  FWorkspace := workspace;
  FEditors := TFslList<TEditorInformation>.create;
end;

procedure TTransformEngine.debug(executionDetails : TWorkspaceExecConfig);
var
  msg : String;
  services : TFHIRConversionEngine;
  js : TFHIRJavascript;
begin
  if not checkCanRun(executionDetails, true, msg) then
    raise Exception.Create(msg);
  CheckTerminologyServer;
  services := TFHIRConversionEngine.Create(FContext.Link, FTxServer.link, nil);
  try
    js := TFHIRJavascript.Create;
    try
      services.registerConversionEngine(js, FContext);
      TV2JavascriptHelper.registerv2Objects(js);

//      !
      // v2, cda:

      js.OnLog := JSLog;

//      Assert.IsTrue(js.asString(js.execute('(()=>{return ''Hello world!'';})()', 'test.js')) = 'Hello world!');
  finally
    js.Free;
  end;

  //  create javascript
  //  register conversion engine
  //  go;

  finally
    services.Free;
  end;
end;

procedure TTransformEngine.DebugTransform(sender: TObject; info: TFHIRStructureMapDebugContext);
var
  ctxt : TTransformEngineDebugContext;
  d : TFHIRStructureMapDebugContext;
  bpi : TBreakPointInfo;
  f : TWorkspaceFile;
begin
  if (info.group = nil) then
    exit; // don't break for the root map

  f := FWorkspace.findFileByParsedObject(info.map);
  if (info.status = dsRunToBreakpoint) then
  begin
    if (f = nil) or not f.hasBreakPoint(info.line-1, bpi) or (info.target <> nil) then
      exit;
  end;

  ctxt := TTransformEngineDebugContext.create;
  try
    ctxt.FPastVariablesAvailable := true;
    ctxt.FCallStack := TFslList<TTransformEngineExecutionPoint>.create;
    d := info;
    while (d <> nil) do
    begin
      ctxt.callStack.Add(TMapTransformEngineExecutionPoint.Create(self.Link, d.link));
      d := d.parent;
    end;
    ctxt.FVariables := ctxt.FCallStack[0].FVariables.link;
    FOnDebug(self, ctxt);
  finally
    ctxt.free;
  end;
end;

destructor TTransformEngine.Destroy;
var
  f : TWorkspaceFile;
begin
  for f in workspace.allFiles do
  begin
    f.compiled := nil;
    f.compileStatus := csNotCompiled;
  end;
  FEditors.free;
  FPathEngine.Free;
  FContext.Free;
  FWorkspace.Free;
  FServices.Free;
  inherited;
end;

procedure TTransformEngine.executeCDA(ev: TWorkspaceExecConfig; debug: boolean);
//var
//  lib : TFslMap<TFHIRStructureMap>;
//  mu : TFHIRStructureMapUtilities;
//  map : TFHIRStructureMap;
//  f : TWorkspaceFile;
//  src : TFHIRMMElement;
begin
//  lib := TFslMap<TFHIRStructureMap>.create;
//  try
//    for f in workspace.maps do
//    begin
//      map := f.compiled as TFHIRStructureMap;
//      lib.Add(map.url, map.link);
//    end;
//    map := ev.script.compiled as TFhirStructureMap;
//    mu := TFHIRStructureMapUtilities.Create(FContext.Link, lib.Link, FServices.link, TFHIRFactoryR4.create);
//    try
//      src := ev.FFocus.compiled as TFHIRMMElement;
//      FServices.outcomes.Clear;
//      if debug then
//        mu.OnDebug := DebugTransform;
//      mu.transform(nil, src, map, nil);
//      assert(FServices.outcomes.Count = 1);
//    finally
//      mu.Free;
//    end;
//  finally
//    lib.Free;
//  end;
end;
(*
var
  engine : TCDAConversionEngine;
  f : TWorkspaceFile;
  editor :TEditorInformation;
  s, fns, fnt : String;
begin
  try
    startRunning;
    engine := TCDAConversionEngine.create;
    try
      engine.source := (cbxSource.Items.Objects[cbxSource.ItemIndex] as TWorkspaceFile).link;
      engine.map := (cbxScript.Items.Objects[cbxScript.ItemIndex] as TWorkspaceFile).link;
      engine.cache := FCache.Link;
      engine.workspace := FWorkspace.link;
      engine.OnWantSource := engineGetSource;
      engine.OnStatus := engineStatus;
      engine.OnLog := engineLog;
      if debug then
        engine.OnTransformDebug := DebugTransform;
      engine.load;
      engine.execute;
      if engine.Outcomes.Count = 0 then
        MessageDlg('No output from Transform', mtInformation, [mbok], 0)
      else if engine.Outcomes.Count > 1 then
        MessageDlg('Multiple outputs from Transform - not handled yet', mtError, [mbok], 0)
      else
        case FWorkspace.Outcome of
          tomIgnore : MessageDlg('Transform Complete with no errors', mtInformation, [mbok], 0);
          tomSaveTo :
            begin
            f := cbxTarget.Items.Objects[cbxTarget.ItemIndex] as TWorkspaceFile;
            editor := editorForFile(f);
            if (editor = nil) then
              editor := openWorkspaceFile(f);
            if isXml(editor.memo.RawText) then
              s := resourceToString(engine.Outcomes[0] as TFhirResource, ffXml, OutputStylePretty)
            else
              s := resourceToString(engine.Outcomes[0] as TFhirResource, ffJson, OutputStylePretty);
            updateWorkspaceFile(editor, s);
            end;
          tomCompare :
            begin
            f := cbxTarget.Items.Objects[cbxTarget.ItemIndex] as TWorkspaceFile;
            fns := f.actualName;
            fnt := Path([SystemTemp, 'generated-'+ExtractFileName(fns)]);
            if isXml(FileToString(fns, TEncoding.UTF8)) then
              StringToFile(resourceToString(engine.Outcomes[0] as TFhirResource, ffXml, OutputStylePretty), fnt, TEncoding.UTF8)
            else
              StringToFile(resourceToString(engine.Outcomes[0] as TFhirResource, ffJson, OutputStylePretty), fnt, TEncoding.UTF8);
            ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar('"'+fns+'" "'+fnt+'"'), true);
            end;
        end;
    finally
      engine.free;
    end;
  finally
    stopRunning;
    if FWantClose then
      Close;
  end;
end;
*)

function TTransformEngine.getOpenFile(f: TWorkspaceFile; out editor: TEditorInformation): boolean;
var
  e : TEditorInformation;
begin
  editor := nil;
  result := false;
  for e in FEditors do
    if (e.id = f) then
    begin
      editor := e;
      exit(true);
    end;
end;

procedure TTransformEngine.initialise(cache: TResourceMemoryCache);
begin
  FContext := TFHIRTransformerContext.Create(TFHIRFactoryR4.create);
  FContext.loadFromCache(cache);
  FPathEngine := TFHIRPathEngine.Create(FContext.Link, nil);
  FServices := TLocalTransformerServices.Create(self, FContext.Link, TFHIRFactoryR4.create);
end;

procedure TTransformEngine.JSLog(sender: TJavascript; message: String);
begin
  if assigned(FOnLog) then
    FOnLog(self, message);
end;

function TTransformEngine.link: TTransformEngine;
begin
  result := TTransformEngine(inherited Link);
end;

procedure TTransformEngine.run(executionDetails : TWorkspaceExecConfig);
var
  msg : String;
begin
//  if not checkCanRun(executionDetails, true, msg) then
//    raise Exception.Create(msg);
//  case executionDetails.kind of
//    ekV2 : raise Exception.Create('Not done yet');
//    ekCDA: executeCDA(executionDetails, false);
//  end;
end;

procedure TTransformEngine.setActiveEditor(editor: TEditorInformation);
var
  e : TEditorInformation;
begin
  for e in FEditors do
    e.focus := false;
  editor.focus := true;
end;

procedure TTransformEngine.SetTerminologyServer(const Value: string);
begin
  FTerminologyServer := Value;
  if (FTxServer <> nil) and (FTxServer.address <> Value) then
  begin
    FTxServer.Free;
    FTxServer := nil;
  end;
end;

{ TTransformEngineDebugContext }

destructor TTransformEngineDebugContext.Destroy;
begin
  FCallStack.Free;
  FVariables.Free;
  inherited;
end;

function TTransformEngineDebugContext.link: TTransformEngineDebugContext;
begin
  result := TTransformEngineDebugContext(inherited link);
end;

{ TTransformEngineExecutionPoint }

destructor TTransformEngineExecutionPoint.Destroy;
begin
  FEngine.Free;
  FVariables.Free;
  inherited;
end;


{ TMapTransformEngineExecutionPoint }

constructor TMapTransformEngineExecutionPoint.Create(engine : TTransformEngine; info: TFHIRStructureMapDebugContext);
var
  v : TVariable;
begin
  inherited create;
  FEngine := engine;
  FInfo := info;
  FVariables := TFslList<TTransformEngineExecutionVariable>.create;
  for v in info.variables.list do
    FVariables.Add(TMapTransformEngineExecutionVariable.Create(Fengine.link, v.link));
end;

destructor TMapTransformEngineExecutionPoint.Destroy;
begin
  FInfo.Free;
  inherited;
end;

function TMapTransformEngineExecutionPoint.GetDescription: String;
begin
  result := FInfo.name;
end;

function TMapTransformEngineExecutionPoint.GetFile: TWorkspaceFile;
begin
  result :=  FEngine.workspace.findFileByParsedObject(FInfo.map);
end;

function TMapTransformEngineExecutionPoint.GetKind: TTransformEngineExecutionPointKind;
begin
  if FInfo.target <> nil then
    result := epMapTarget
  else if FInfo.rule <> nil then
    result := epMapRule
  else if FInfo.group <> nil then
    result := epMapGroup
  else
    result := epMap;
end;

function TMapTransformEngineExecutionPoint.getStart: TSourceLocation;
begin
  result := FInfo.focus.LocationStart;
end;

function TMapTransformEngineExecutionPoint.getStop: TSourceLocation;
begin
  result := FInfo.focus.LocationEnd;
end;

{ TMapTransformEngineExecutionVariable }

constructor TMapTransformEngineExecutionVariable.Create(engine : TTransformEngine; info: TVariable);
begin
  inherited create;
  FEngine := engine;
  FInfo := info;
end;

destructor TMapTransformEngineExecutionVariable.Destroy;
begin
  FInfo.Free;
  inherited;
end;

function TMapTransformEngineExecutionVariable.GetName: String;
begin
  result := FInfo.name;
end;

function TMapTransformEngineExecutionVariable.GetObject: TFHIRObject;
begin
  result := FInfo.obj;
end;

function TMapTransformEngineExecutionVariable.GetMode: TTransformEngineExecutionVariableMode;
begin
  if FInfo.mode = vmINPUT then
    result := tvmInput
  else
    result := tvmOutput;
end;

function TMapTransformEngineExecutionVariable.GetTypeName: String;
begin
  if FInfo.obj = nil then
    result := 'null'
  else
    result := FInfo.obj.fhirType;
end;

function TMapTransformEngineExecutionVariable.GetValue: String;
begin
  if FInfo.obj.isPrimitive then
    result := FInfo.obj.primitiveValue
  else
    result := '';
end;

{ TTransformEngineExecutionVariable }

destructor TTransformEngineExecutionVariable.Destroy;
begin
  FEngine.Free;
  inherited;
end;

function TTransformEngineExecutionVariable.link: TTransformEngineExecutionVariable;
begin
  result := TTransformEngineExecutionVariable(inherited Link);
end;

end.

(*
procedure TTransformerForm.executeTransform(debug : boolean);
var
  engine : TCDAConversionEngine;
  f : TWorkspaceFile;
  editor :TEditorInformation;
  s, fns, fnt : String;
begin
  try
    startRunning;
    engine := TCDAConversionEngine.create;
    try
      engine.source := (cbxSource.Items.Objects[cbxSource.ItemIndex] as TWorkspaceFile).link;
      engine.map := (cbxScript.Items.Objects[cbxScript.ItemIndex] as TWorkspaceFile).link;
      engine.cache := FCache.Link;
      engine.workspace := FWorkspace.link;
      engine.OnWantSource := engineGetSource;
      engine.OnStatus := engineStatus;
      engine.OnLog := engineLog;
      if debug then
        engine.OnTransformDebug := DebugTransform;
      engine.load;
      engine.execute;
      if engine.Outcomes.Count = 0 then
        MessageDlg('No output from Transform', mtInformation, [mbok], 0)
      else if engine.Outcomes.Count > 1 then
        MessageDlg('Multiple outputs from Transform - not handled yet', mtError, [mbok], 0)
      else
        case FWorkspace.Outcome of
          tomIgnore : MessageDlg('Transform Complete with no errors', mtInformation, [mbok], 0);
          tomSaveTo :
            begin
            f := cbxTarget.Items.Objects[cbxTarget.ItemIndex] as TWorkspaceFile;
            editor := editorForFile(f);
            if (editor = nil) then
              editor := openWorkspaceFile(f);
            if isXml(editor.memo.RawText) then
              s := resourceToString(engine.Outcomes[0] as TFhirResource, ffXml, OutputStylePretty)
            else
              s := resourceToString(engine.Outcomes[0] as TFhirResource, ffJson, OutputStylePretty);
            updateWorkspaceFile(editor, s);
            end;
          tomCompare :
            begin
            f := cbxTarget.Items.Objects[cbxTarget.ItemIndex] as TWorkspaceFile;
            fns := f.actualName;
            fnt := Path([SystemTemp, 'generated-'+ExtractFileName(fns)]);
            if isXml(FileToString(fns, TEncoding.UTF8)) then
              StringToFile(resourceToString(engine.Outcomes[0] as TFhirResource, ffXml, OutputStylePretty), fnt, TEncoding.UTF8)
            else
              StringToFile(resourceToString(engine.Outcomes[0] as TFhirResource, ffJson, OutputStylePretty), fnt, TEncoding.UTF8);
            ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar('"'+fns+'" "'+fnt+'"'), true);
            end;
        end;
    finally
      engine.free;
    end;
  finally
    stopRunning;
    if FWantClose then
      Close;
  end;
end;


procedure TTransformerForm.DoCompiled(sender: TConversionEngine; f: TWorkspaceFile; checkBreakpointProc: TCheckBreakpointEvent);
var
  bpi : TBreakPointInfo;
  valid : boolean;
  editor : TEditorInformation;
begin
  editor := editorForFile(f);
  for bpi in f.BreakPoints do
  begin
    bpi.invalid := not checkBreakpointProc(bpi.line);
    editor.UpdateLineMarkers(bpi.line);
  end;
end;


*)
