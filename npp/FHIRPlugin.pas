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
Execute Path Query
--
Configure Servers
(TComboBox)
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
  Windows, SysUtils, Classes,
  NppPlugin, SciSupport,
  Vcl.Dialogs,
  AdvGenerics, AdvBuffers, XmlBuilder,
  FHIRBase, FHIRValidator, FHIRResources, FHIRTypes, FHIRParser, FHIRParserBase,
  AboutForms, FHIRToolboxForm, FHIRPluginSettings, FHIRPluginErrors, FHIRPluginValidator;

type
  TFHIRPlugin = class(TNppPlugin)
  private
    tipShowing : boolean;
    tipText : AnsiString;
    errors : TAdvList<TValidationError>;
    errorSorter : TValidationErrorComparer;
    FValidator : TFHIRValidator;

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
    function convertIssue(issue: TFhirOperationOutcomeIssue): TValidationError;
  public
    constructor Create;
    destructor Destroy; override;

    // user interface
    procedure FuncValidate;
    procedure FuncValidateClear;
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

    // responding to np++ events
    procedure DoNppnReady; override; // install toolbox if necessary
    procedure DoNppnTextModified; override;
    procedure DoNppnBufferChange; override;
    procedure DoNppnDwellStart(offset : integer); override;
    procedure DoNppnDwellEnd; override;
  end;

procedure _FuncValidate; cdecl;
procedure _FuncValidateClear; cdecl;
procedure _FuncToolbox; cdecl;
procedure _FuncAbout; cdecl;
procedure _FuncSettings; cdecl;
procedure _FuncFormat; cdecl;
procedure _FuncPath; cdecl;
procedure _FuncServers; cdecl;
procedure _FuncConnect; cdecl;
procedure _FuncNewResource; cdecl;
procedure _FuncOpen; cdecl;
procedure _FuncPUT; cdecl;
procedure _FuncPOST; cdecl;
procedure _FuncTransaction; cdecl;
procedure _FuncServerValidate; cdecl;

var
  Npp: TFHIRPlugin;

implementation

var
  ms : String;

procedure mcheck(i : integer);
begin
  ms := ms + inttostr(i) +' ';
  if (FHIRToolbox <> nil) then
    FHIRToolbox.Memo1.Text := ms;
end;


{ TFHIRPlugin }

constructor TFHIRPlugin.Create;
var
//  sk: TShortcutKey;
  i: Integer;
begin
  inherited;
  errors := TAdvList<TValidationError>.create;
  errorSorter := TValidationErrorComparer.create;
  errors.Sort(errorSorter);

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
  self.AddFuncItem('Configure &Servers', _FuncServers);
// (TComboBox)
  self.AddFuncItem('&Connect to Server', _FuncConnect);
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
  Npp.FuncValidate;
end;

procedure _FuncValidateClear; cdecl;
begin
  Npp.FuncValidateClear;
end;

procedure _FuncAbout; cdecl;
begin
  Npp.FuncAbout;
end;

procedure _FuncToolbox; cdecl;
begin
  Npp.FuncToolbox;
end;

procedure _FuncSettings; cdecl;
begin
  Npp.FuncSettings;
end;

procedure _FuncFormat; cdecl;
begin
  Npp.FuncFormat;
end;

procedure _FuncPath; cdecl;
begin
  Npp.FuncPath;
end;

procedure _FuncServers; cdecl;
begin
  Npp.FuncServers;
end;

procedure _FuncConnect; cdecl;
begin
  Npp.FuncConnect;
end;

procedure _FuncNewResource; cdecl;
begin
  Npp.FuncNewResource;
end;

procedure _FuncOpen; cdecl;
begin
  Npp.FuncOpen;
end;

procedure _FuncPUT; cdecl;
begin
  Npp.FuncPUT;
end;

procedure _FuncPOST; cdecl;
begin
  Npp.FuncPOST;
end;

procedure _FuncTransaction; cdecl;
begin
  Npp.FuncTransaction;
end;

procedure _FuncServerValidate; cdecl;
begin
  Npp.FuncServerValidate;
end;


function TFHIRPlugin.convertIssue(issue : TFhirOperationOutcomeIssue) : TValidationError;
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
    IssueSeverityWarning : result := TValidationError.create(INDIC_WARNING, s, e, msg);
    IssueSeverityInformation : result := TValidationError.create(INDIC_INFORMATION, s, e, msg);
  else
    result := TValidationError.create(INDIC_ERROR, s, e, msg);
  end;
end;

procedure TFHIRPlugin.FuncValidate;
var
  src : String;
  buffer : TAdvBuffer;
  error : TValidationError;
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
    except
      on e: Exception do
        errors.Add(TValidationError.create(INDIC_ERROR, 0, 4, e.Message));
    end;
  end
  else
    errors.Add(TValidationError.create(INDIC_ERROR, 0, 4, 'This does not appear to be valid FHIR content'));
  for error in errors do
    squiggle(error.level, error.start, error.stop - error.start);
end;

procedure TFHIRPlugin.FuncValidateClear;
var
  src : String;
  error : TValidationError;
begin
  src := CurrentText;
  for error in errors do
    clearSquiggle(error.level, error.start, error.stop - error.start);
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
    FValidator := TFHIRValidator.Create;
    FValidator.SchematronSource := 'C:\work\org.hl7.fhir\build\publish';
    FValidator.LoadFromDefinitions('C:\work\org.hl7.fhir\build\publish\validation-min.xml.zip');
  end;
end;

procedure TFHIRPlugin.FuncServers;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncServerValidate;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncSettings;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncAbout;
var
  a: TAboutForm;
begin
  a := TAboutForm.Create(self);
  a.ShowModal;
  a.Free;
end;

procedure TFHIRPlugin.FuncConnect;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncFormat;
var
  src : String;
  buffer : TAdvBuffer;
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

procedure TFHIRPlugin.FuncNewResource;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncOpen;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncPath;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncPOST;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncPUT;
begin
  ShowMessage('not done yet');
end;

procedure TFHIRPlugin.FuncToolbox;
begin
  if (not Assigned(FHIRToolbox)) then FHIRToolbox := TFHIRToolbox.Create(self, 1);
  FHIRToolbox.Show;
end;

procedure TFHIRPlugin.FuncTransaction;
begin
  ShowMessage('not done yet');
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

  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_INFORMATION, $ff0000));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_WARNING, $00FF00));
  mcheck(SendMessage(NppData.ScintillaMainHandle, SCI_INDICSETFORE, INDIC_ERROR, $0000FF));

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
  FValidator.Free;
  Settings.Free;
  errors.Free;
  errorSorter.Free;
  inherited;
end;

procedure TFHIRPlugin.DoNppnReady;
begin
  Settings := TFHIRPluginSettings.create(GetPluginsConfigDir);
  if Settings.ToolboxVisible then
    FuncToolbox;
end;

procedure TFHIRPlugin.DoNppnBufferChange;
//var
//  s : String;
begin
  FuncValidateClear;
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
  error : TValidationError;
  first : boolean;
begin
  first := true;
  msg := TStringBuilder.Create;
  try
    for error in errors do
    begin
      if (error.start <= offset) and (error.stop >= offset) then
      begin
        if first then
          first := false
        else
          msg.AppendLine;
        msg.Append(error.message);
      end;
      if error.start > offset then
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



initialization
  Npp := TFHIRPlugin.Create;
end.
