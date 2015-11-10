unit FHIRPlugin;

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
  Windows, SysUtils, Classes, Forms, Vcl.Dialogs, Messages, Consts, UITypes, System.Generics.Defaults,
  NppPlugin, SciSupport,
  GuidSupport, FileSupport, SystemSupport,
  AdvObjects, AdvGenerics, AdvBuffers,
  XmlBuilder,
  FHIRBase, FHIRValidator, FHIRResources, FHIRTypes, FHIRParser, FHIRParserBase, FHIRUtilities, FHIRClient, FHIRConstants,
  FHIRPluginSettings, FHIRPluginValidator, FHIRNarrativeGenerator, FHIRPath,
  SmartOnFhirUtilities, SmartOnFhirLogin,
  FHIRToolboxForm, AboutForms, SettingsForm, NewResourceForm, FetchResourceForm;

const
  INDIC_INFORMATION = 25;
  INDIC_WARNING = 26;
  INDIC_ERROR = 27;
  INDIC_MATCH = 28;

type
  TFHIRAnnotation = class (TAdvObject)
  private
    FLevel: integer;
    FMessage: String;
    FStop: integer;
    FStart: integer;
  public
    Constructor create(level : integer; start, stop : integer; message : String); overload;

    property level : integer read FLevel write FLevel;
    property start : integer read FStart write FStart;
    property stop : integer read FStop write FStop;
    property message : String read FMessage write FMessage;
  end;

  TFHIRAnnotationComparer = class (TAdvObject, IComparer<TFHIRAnnotation>)
  public
    function Compare(const Left, Right: TFHIRAnnotation): Integer;
  end;

  TFHIRPlugin = class(TNppPlugin)
  private
    tipShowing : boolean;
    tipText : AnsiString;
    errors : TAdvList<TFHIRAnnotation>;
    matches : TAdvList<TFHIRAnnotation>;
    errorSorter : TFHIRAnnotationComparer;
    FValidator : TFHIRValidator;
    FClient : TFHIRClient;
    FConformance : TFHIRConformance;

    // this procedure handles validation.
    // it is called whene the text of the scintilla buffer changes
    // first task is to clear any existing error notifications - if there is a reset
    // second task is to abort any existing validation process
    // third task is to start valdiating
    procedure NotifyContent(text : String; reset : boolean);

    // Scintilla control
    procedure setUpSquiggles;
    procedure squiggle(level : integer; start, length : integer);
    procedure clearSquiggle(level : integer; start, length : integer);

    // fhir stuff
    function determineFormat(src : String) : TFHIRFormat;
    procedure loadValidator;
    function convertIssue(issue: TFhirOperationOutcomeIssue): TFHIRAnnotation;

    // smart on fhir stuff
    function DoSmartOnFHIR(server : TRegisteredServer) : boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function connected : boolean;

    // user interface
    procedure FuncValidate;
    procedure FuncValidateClear;
    procedure FuncMatchesClear;
    procedure FuncToolbox;
    procedure FuncSettings;
    procedure FuncAbout;
    procedure FuncFormat;
    procedure FuncPath;
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

    // responding to np++ events
    procedure DoNppnReady; override; // install toolbox if necessary
    procedure DoNppnTextModified; override;
    procedure DoNppnBufferChange; override;
    procedure DoNppnDwellStart(offset : integer); override;
    procedure DoNppnDwellEnd; override;
    procedure DoNppnShutdown; override;
  end;

procedure _FuncValidate; cdecl;
procedure _FuncValidateClear; cdecl;
procedure _FuncToolbox; cdecl;
procedure _FuncAbout; cdecl;
procedure _FuncSettings; cdecl;
procedure _FuncPath; cdecl;
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

var
  FNpp: TFHIRPlugin;

implementation

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
  self.AddFuncItem('&Clear Validation Information', _FuncValidateClear);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('E&xecute Path Query', _FuncPath);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('Connect to &Server', _FuncConnect);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('&New Resource (Template)', _FuncNewResource);
  self.AddFuncItem('&Open Resource on Server', _FuncOpen);
  self.AddFuncItem('P&UT resource to existing ID', _FuncPUT);
  self.AddFuncItem('&POST resource to new ID', _FuncPOST);
  self.AddFuncItem('POST resource as a &Transaction', _FuncTransaction);
  self.AddFuncItem('V&alidate resource on server', _FuncServerValidate);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('Confi&gure Tools', _FuncSettings);
  self.AddFuncItem('Vie&w Toolbox', _FuncToolbox);
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

procedure _FuncToolbox; cdecl;
begin
  FNpp.FuncToolbox;
end;

procedure _FuncSettings; cdecl;
begin
  FNpp.FuncSettings;
end;

procedure _FuncPath; cdecl;
begin
  FNpp.FuncPath;
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


procedure _FuncNarrative; cdecl;
begin
  FNpp.FuncNarrative;
end;

procedure _FuncDisconnect; cdecl;
begin
  FNpp.FuncDisconnect;
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
    IssueSeverityWarning : result := TFHIRAnnotation.create(INDIC_WARNING, s, e, msg);
    IssueSeverityInformation : result := TFHIRAnnotation.create(INDIC_INFORMATION, s, e, msg);
  else
    result := TFHIRAnnotation.create(INDIC_ERROR, s, e, msg);
  end;
end;

procedure TFHIRPlugin.FuncValidate;
var
  src : String;
  buffer : TAdvBuffer;
  error : TFHIRAnnotation;
  context : TFHIRValidatorContext;
  op : TFHIROperationOutcome;
  iss : TFhirOperationOutcomeIssue;
  fmt : TFHIRFormat;
begin
  FuncValidateClear;
  src := CurrentText;
  fmt := determineFormat(src);
  if (fmt <> ffasIs) then
  begin
    try
      buffer := TAdvBuffer.Create;
      try
        buffer.AsUnicode := src;
        loadValidator;
        context := FValidator.AcquireContext;
        try
          op := FValidator.validateInstance(context, buffer, fmt, 'validate', nil);
        finally
          FValidator.YieldContext(context);
        end;
      finally
        buffer.Free;
      end;
      for iss in op.issueList do
        errors.add(convertIssue(iss));
      MessageBeep(MB_OK);
    except
      on e: Exception do
        errors.Add(TFHIRAnnotation.create(INDIC_ERROR, 0, 4, e.Message));
    end;
  end
  else
    errors.Add(TFHIRAnnotation.create(INDIC_ERROR, 0, 4, 'This does not appear to be valid FHIR content'));
  for error in errors do
    squiggle(error.level, error.start, error.stop - error.start);
end;

procedure TFHIRPlugin.FuncMatchesClear;
var
  annot : TFHIRAnnotation;
begin
  for annot in matches do
    clearSquiggle(annot.level, annot.start, annot.stop - annot.start);
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
    clearSquiggle(annot.level, annot.start, annot.stop - annot.start);
  errors.Clear;
  if tipShowing then
    mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_CALLTIPCANCEL, 0, 0));
  tipText := '';
end;

function TFHIRPlugin.determineFormat(src: String): TFHIRFormat;
begin
  result := ffAsIs; // null
  src := src.Trim;
  if (src <> '') then
    begin
    if src[1] = '<' then
    begin
      if src.Contains('"http://hl7.org/fhir"') then
        result := ffXml
    end
    else if src[1] = '{' then
    begin
      if src.Contains('"resourceType"') then
        result := ffJson;
    end;
  end;
end;

procedure TFHIRPlugin.loadValidator;
begin
  if FValidator = nil then
  begin

    if (Settings.TerminologyServer = '') or (Settings.DefinitionsSource = '') then
    begin
      if MessageDlg('Validation is not configured. Would you like to configure it?', mtConfirmation, mbYesNo, 0) = mrYes then
        _FuncSettings;
      Abort;
    end;

    OpMessage('Loading', 'Loading Definition Source');
    try
      FValidator := TFHIRValidator.Create(Settings.TerminologyServer);
      FValidator.LoadFromDefinitions(Settings.DefinitionsSource);
    finally
      OpMessage('', '');
    end;
  end;
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
  a := TAboutForm.Create(self);
  try
    a.ShowModal;
  finally
    a.Free;
  end;
end;

procedure TFHIRPlugin.FuncConnect;
var
  index : integer;
  server : TRegisteredServer;
  ok : boolean;
begin
  index := 0;
  if (Assigned(FHIRToolbox)) then
    index := FHIRToolbox.cbxServers.ItemIndex;
  server := Settings.serverInfo(index);
  try
    try
      OpMessage('Connecting to Server', 'Connecting to Server '+server.fhirEndpoint);
      FClient := TFhirClient.Create(server.fhirEndpoint, false);
      ok := true;
      if server.SmartOnFHIR then
        if not DoSmartOnFHIR(server) then
        begin
          ok := false;
          FuncDisconnect;
        end;

      if ok then
      begin
        try
          FClient.json := false;
          FConformance := FClient.conformance(false);
        except
          FClient.json := not FClient.Json;
          FConformance := FClient.conformance(false);
        end;
        FConformance.checkCompatible();
        if (Assigned(FHIRToolbox)) then
          if FClient.smartToken = nil then
            FHIRToolbox.connected(server.name, server.fhirEndpoint, '', '')
          else
            FHIRToolbox.connected(server.name, server.fhirEndpoint, FClient.smartToken.username, FClient.smartToken.scopes);
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
end;

procedure TFHIRPlugin.FuncDisconnect;
begin
  if (Assigned(FHIRToolbox)) then
    FHIRToolbox.disconnected;
  if (Assigned(FetchResourceFrm)) then
    FreeAndNil(FetchResourceFrm);
  FClient.Free;
  FClient := nil;
  FConformance.Free;
  FConformance := nil;
end;

procedure TFHIRPlugin.FuncFormat;
var
  src : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  prsr : TFHIRParser;
  comp : TFHIRComposer;
begin
  src := CurrentText;
  fmt := determineFormat(src);
  if (fmt <> ffasIs) then
  begin
    FuncValidateClear;
    FuncMatchesClear;
    s := TStringStream.Create(src);
    if fmt = ffXml then
      prsr := TFHIRXmlParser.Create('en')
    else
      prsr := TFHIRJsonParser.Create('en');
    try
      prsr.source := s;
      prsr.Parse;
      if fmt = ffJson then
        comp := TFHIRXmlComposer.Create('en')
      else
        comp := TFHIRJsonComposer.Create('en');
      try
        s.Clear;
        comp.Compose(s, prsr.resource, true);
        CurrentText := s.DataString;
      finally
        comp.Free;
      end;
    finally
      prsr.Free;
      s.Free;
    end;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.FuncNarrative;
var
  src : String;
  buffer : TAdvBuffer;
  fmt : TFHIRFormat;
  s : TStringStream;
  prsr : TFHIRParser;
  comp : TFHIRComposer;
  d : TFhirDomainResource;
  narr : TFHIRNarrativeGenerator;
begin
  src := CurrentText;
  fmt := determineFormat(src);
  if (fmt <> ffasIs) then
  begin
    FuncValidateClear;
    FuncMatchesClear;
    s := TStringStream.Create(src);
    if fmt = ffXml then
      prsr := TFHIRXmlParser.Create('en')
    else
      prsr := TFHIRJsonParser.Create('en');
    try
      prsr.source := s;
      prsr.Parse;

      if (prsr.resource is TFhirDomainResource) then
      begin
        d := prsr.resource as TFhirDomainResource;
        d.text := nil;
        loadValidator;
        narr := TFHIRNarrativeGenerator.Create(FValidator.Link);
        try
          narr.generate(d);
        finally
          narr.Free;
        end;
      end;

      if fmt = ffXml then
        comp := TFHIRXmlComposer.Create('en')
      else
        comp := TFHIRJsonComposer.Create('en');
      try
        s.Clear;
        comp.Compose(s, prsr.resource, true);
        CurrentText := s.DataString;
      finally
        comp.Free;
      end;
    finally
      prsr.Free;
      s.Free;
    end;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.FuncNewResource;
begin
  loadValidator;
  ResourceNewForm := TResourceNewForm.Create(self);
  try
    ResourceNewForm.Validator := FValidator.Link;
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
  FetchResourceFrm.Conformance := FConformance.link;
  FetchResourceFrm.Client := FClient.link;
  FetchResourceFrm.Profiles := FValidator.Profiles.Link;
  if FetchResourceFrm.ShowModal = mrOk then
  begin
    res := FClient.readResource(FetchResourceFrm.SelectedType, FetchResourceFrm.SelectedId);
    try
      if FetchResourceFrm.rbJson.Checked then
        comp := TFHIRJsonComposer.Create('en')
      else
        comp := TFHIRXmlComposer.Create('en');
      try
        s := TStringStream.Create;
        try
          comp.Compose(s, res, true);
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

procedure TFHIRPlugin.FuncPath;
var
  src : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  prsr : TFHIRParser;
  items : TFHIRBaseList;
  query : TFHIRPathEvaluator;
  item : TFHIRObject;
  allSource : boolean;
  sp, ep : integer;
  annot : TFHIRAnnotation;
begin
  FuncMatchesClear;

  src := CurrentText;
  fmt := determineFormat(src);
  if (fmt <> ffasIs) then
  begin
    FuncMatchesClear;
    s := TStringStream.Create(src);
    if fmt = ffXml then
      prsr := TFHIRXmlParser.Create('en')
    else
      prsr := TFHIRJsonParser.Create('en');
    try
      prsr.KeepLineNumbers := true;
      prsr.source := s;
      prsr.Parse;
      query := TFHIRPathEvaluator.Create;
      try
        items := query.evaluate(prsr.resource, FHIRToolbox.mPath.Text);
        try
          allSource := true;
          for item in items do
            allSource := allSource and not isNullLoc(item.LocationStart);
          if Items.Count = 0 then
            MessageDlg('no items matched', mtInformation, [mbok], 0)
          else if not allSource then
            MessageDlg(query.convertToString(items), mtInformation, [mbok], 0)
          else
          begin
            for item in items do
            begin
              sp := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, item.LocationStart.line - 1, item.LocationStart.col-1);
              ep := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, item.LocationEnd.line - 1, item.LocationEnd.col-1);
              if (ep = sp) then
                ep := sp + 1;
              matches.Add(TFHIRAnnotation.create(INDIC_MATCH, sp, ep, 'This element is a match to path "'+FHIRToolbox.mPath.Text+'"'));
            end;
            for annot in matches do
              squiggle(annot.level, annot.start, annot.stop - annot.start);
          end;
        finally
          items.Free;
        end;
      finally
        query.Free;
      end;
    finally
      prsr.Free;
      s.Free;
    end;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.FuncPOST;
var
  src, id : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  prsr : TFHIRParser;
  comp : TFHIRComposer;
begin
  if (FClient = nil) then
  begin
    MessageDlg('You must connect to a server first', mtInformation, [mbok], 0);
    exit;
  end;
  src := CurrentText;
  fmt := determineFormat(src);
  if (fmt <> ffasIs) then
  begin
    FuncValidateClear;
    FuncMatchesClear;
    s := TStringStream.Create(src);
    if fmt = ffXml then
      prsr := TFHIRXmlParser.Create('en')
    else
      prsr := TFHIRJsonParser.Create('en');
    try
      prsr.source := s;
      prsr.Parse;
      prsr.resource.id := '';
      FClient.createResource(prsr.resource, id).Free;
      prsr.resource.id := id;
      if fmt = ffXml then
        comp := TFHIRXmlComposer.Create('en')
      else
        comp := TFHIRJsonComposer.Create('en');
      try
        s.Clear;
        comp.Compose(s, prsr.resource, true);
        CurrentText := s.DataString;
      finally
        comp.Free;
      end;
      if fmt = ffJson then
        saveFileAs(IncludeTrailingPathDelimiter(ExtractFilePath(currentFileName))+CODES_TFhirResourceType[prsr.resource.ResourceType]+'-'+id+'.json')
      else
        saveFileAs(IncludeTrailingPathDelimiter(ExtractFilePath(currentFileName))+CODES_TFhirResourceType[prsr.resource.ResourceType]+'-'+id+'.xml');
    finally
      prsr.Free;
      s.Free;
    end;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.FuncPUT;
var
  src : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  prsr : TFHIRParser;
begin
  if (FClient = nil) then
  begin
    MessageDlg('You must connect to a server first', mtInformation, [mbok], 0);
    exit;
  end;
  src := CurrentText;
  fmt := determineFormat(src);
  if (fmt <> ffasIs) then
  begin
    s := TStringStream.Create(src);
    if fmt = ffXml then
      prsr := TFHIRXmlParser.Create('en')
    else
      prsr := TFHIRJsonParser.Create('en');
    try
      prsr.source := s;
      prsr.Parse;
      if (prsr.resource.id = '') then
        ShowMessage('Cannot PUT this as it does not have an id')
      else
        FCLient.updateResource(prsr.resource);

    finally
      prsr.Free;
      s.Free;
    end;
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
  src, id : String;
  fmt : TFHIRFormat;
  s : TStringStream;
  prsr : TFHIRParser;
  comp : TFHIRComposer;
  res : TFHIRResource;
begin
  if (FClient = nil) then
  begin
    MessageDlg('You must connect to a server first', mtInformation, [mbok], 0);
    exit;
  end;
  src := CurrentText;
  fmt := determineFormat(src);
  if (fmt <> ffasIs) then
  begin
    s := TStringStream.Create(src);
    if fmt = ffXml then
      prsr := TFHIRXmlParser.Create('en')
    else
      prsr := TFHIRJsonParser.Create('en');
    try
      prsr.source := s;
      prsr.Parse;
      prsr.resource.id := '';
      if prsr.resource.ResourceType <> frtBundle then
        ShowMessage('This is not a Bundle')
      else
      begin
        res := FClient.transaction(prsr.resource as TFhirBundle);
        try
          if (MessageDlg('Success. Open transaction response?', mtConfirmation, mbYesNo, 0) = mrYes) then
          begin
            if FClient.Json then
              comp := TFHIRJsonComposer.Create('en')
            else
              comp := TFHIRXmlComposer.Create('en');
            try
              s := TStringStream.Create;
              try
                comp.Compose(s, res, true);
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
      prsr.Free;
      s.Free;
    end;
  end
  else
    ShowMessage('This does not appear to be valid FHIR content');
end;

procedure TFHIRPlugin.NotifyContent(text: String; reset: boolean);
begin
  squiggle(INDIC_ERROR, 2, 4);
end;

procedure TFHIRPlugin.setUpSquiggles;
begin
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETSTYLE, INDIC_INFORMATION, INDIC_SQUIGGLE));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETSTYLE, INDIC_WARNING, INDIC_SQUIGGLE));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETSTYLE, INDIC_ERROR, INDIC_SQUIGGLE));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETSTYLE, INDIC_MATCH, INDIC_SQUIGGLE));

  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_INFORMATION, $ff0000));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_WARNING, $7777FF));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_ERROR, $000077));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_MATCH, $007700));

  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_SETMOUSEDWELLTIME, 200, 0));
end;

procedure TFHIRPlugin.squiggle(level : integer; start, length: integer);
begin
  setUpSquiggles;
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_SETINDICATORCURRENT, level, 0));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICATORFILLRANGE, start, length));
end;



procedure TFHIRPlugin.clearSquiggle(level, start, length: integer);
begin
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_SETINDICATORCURRENT, level, 0));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICATORCLEARRANGE, start, length));
end;

destructor TFHIRPlugin.Destroy;
begin
  inherited;
end;

procedure TFHIRPlugin.DoNppnReady;
begin
  Settings := TFHIRPluginSettings.create(GetPluginsConfigDir);
  if Settings.ToolboxVisible then
    FuncToolbox;
end;

procedure TFHIRPlugin.DoNppnShutdown;
begin
  inherited;
  FValidator.Free;
  Settings.Free;
  errors.Free;
  matches.Free;
  errorSorter.Free;
  FClient.Free;
  FConformance.Free;
  FreeAndNil(FetchResourceFrm);
end;

procedure TFHIRPlugin.DoNppnBufferChange;
//var
//  s : String;
begin
  FuncValidateClear;
  FuncMatchesClear;
//  s := GetCurrentText;
//  NotifyContent(s, true);
//  if (Assigned(FHIRToolbox)) then
//    FHIRToolbox.Memo1.Text := s;
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

procedure TFHIRPlugin.DoNppnTextModified;
var
  s : String;
begin
//  s := GetCurrentText;
//  NotifyContent(s, false);
//  if (Assigned(FHIRToolbox)) then
//    FHIRToolbox.Memo1.Text := GetCurrentText;
end;



function TFHIRPlugin.DoSmartOnFHIR(server : TRegisteredServer) : boolean;
var
  mr : integer;
begin
  result := false;
  SmartOnFhirLoginForm := TSmartOnFhirLoginForm.Create(self);
  try
    SmartOnFhirLoginForm.logoPath := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(HInstance)))+'npp.png';
    SmartOnFhirLoginForm.Server := server;
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

{ TFHIRAnnotationComparer }

function TFHIRAnnotationComparer.Compare(const Left, Right: TFHIRAnnotation): Integer;
begin
  if (left.FStart < Right.FStart) then
    result := -1
  else if (left.FStart > Right.FStart) then
    result := 1
  else if (left.FStop < Right.FStop) then
    result := -1
  else if (left.FStop > Right.FStop) then
    result := 1
  else if (left.FLevel < Right.FLevel) then
    result := -1
  else if (left.FLevel > Right.FLevel) then
    result := 1
  else
    result := 0;
end;

{ TFHIRAnnotation }

constructor TFHIRAnnotation.create(level: integer; start, stop: integer; message: String);
begin
  Create;
  self.level := level;
  self.start := start;
  self.stop := stop;
  self.message := message;
end;



initialization
  FNpp := TFHIRPlugin.Create;
end.

// "C:\Users\Grahame Grieve\AppData\Roaming\Notepad++\plugins\npp.png"

