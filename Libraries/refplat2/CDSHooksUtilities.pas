unit CDSHooksUtilities;

interface

uses
  SysUtils, Classes,
  TextUtilities, MarkDownProcessor, KCritSct, HashSupport, EncodeSupport,
  AdvObjects, AdvGenerics, AdvThreads,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, SmartOnFhirUtilities, FHIRClient, FHIRParser;

type
  TCDSHooks = class
  public
    class function patientView : TFhirCoding;
    class function isPatientView(c : TFhirCoding) : boolean;

    class function codeView : TFhirCoding;
    class function isCodeView(c : TFhirCoding) : boolean;

    class function identifierView : TFhirCoding;
    class function isIdentifierView(c : TFhirCoding) : boolean;

    class function allHooks : TAdvList<TFhirCoding>;
    class function isKnownHook(c : TFhirCoding) : boolean;
  end;

  TCDSHookRequest = class (TAdvObject)
  private
    Fencounter: String;
    FoauthExpires: Integer;
    FfhirServer: String;
    Fpatient: String;
    FoauthScope: String;
    FactivityInstance: String;
    Fuser: String;
    FoauthToken: String;
    Factivity: TFHIRCoding;
    Fredirect: String;
    FContext: TAdvList<TFHIRResource>;
    FBundle : TFHIRBundle;
    procedure Setactivity(const Value: TFHIRCoding);
    procedure SetBundle(const Value: TFHIRBundle);
  public
    constructor Create; Overload; Override;
    constructor Create(params : TFHIRParameters); Overload;
    destructor Destroy; override;

    function Link : TCDSHookRequest; overload;
    function hash : Cardinal;

    function AsParams : TFHIRParameters;
    property activity : TFHIRCoding read Factivity write Setactivity;
    property activityInstance : String read FactivityInstance write FactivityInstance;
    property fhirServer : String read FfhirServer write FfhirServer;
    property oauthToken : String read FoauthToken write FoauthToken;
    property oauthScope : String read FoauthScope write FoauthScope;
    property oauthExpires : Integer read FoauthExpires write FoauthExpires;
    property redirect : String read Fredirect write Fredirect;
    property user : String read Fuser write Fuser;
    property patient : String read Fpatient write Fpatient;
    property encounter : String read Fencounter write Fencounter;
    property context : TAdvList<TFHIRResource> read FContext;
    property preFetchData : TFHIRBundle read FBundle write SetBundle;
  end;

  TCDSHookCardSuggestion = class (TAdvObject)
  private
    FLabel: String;
  public
    constructor Create; Overload; Override;
    constructor Create(params : TFHIRParametersParameter); Overload;
    destructor Destroy; override;

    function Link : TCDSHookCardSuggestion; overload;
    function AsParam : TFHIRParametersParameter;

    property label_ : String read FLabel write FLabel;
  end;

  TCDSHookCardLink = class (TAdvObject)
  private
    FLabel: String;
    FUrl: String;
  public
    constructor Create; Overload; Override;
    constructor Create(params : TFHIRParametersParameter); Overload;
    destructor Destroy; override;

    function Link : TCDSHookCardLink; overload;
    function AsParam : TFHIRParametersParameter;

    property label_ : String read FLabel write FLabel;
    property url : String read FUrl write FUrl;
  end;

  TCDSHookCard = class (TAdvObject)
  private
    FSourceURL: String;
    Fdetail: String;
    FsourceLabel: String;
    Fsummary: String;
    Findicator: String;
    FSuggestions: TAdvList<TCDSHookCardSuggestion>;
    FLinks: TAdvList<TCDSHookCardLink>;
  public
    constructor Create; Overload; Override;
    constructor Create(params : TFHIRParametersParameter); Overload;
    constructor Create(summary, sourceLabel : String); Overload;
    constructor Create(summary, detail, sourceLabel : String); Overload;
    destructor Destroy; override;

    function Link : TCDSHookCard; overload;
    function AsParam : TFHIRParametersParameter;

    property summary : String read Fsummary write Fsummary;
    property indicator : String read Findicator write Findicator;
    property detail : String read Fdetail write Fdetail;
    property sourceLabel : String read FsourceLabel write FsourceLabel;
    property sourceURL : String read FSourceURL write FSourceURL;
    property suggestions : TAdvList<TCDSHookCardSuggestion> read FSuggestions;
    property links : TAdvList<TCDSHookCardLink> read FLinks;

    procedure addLink(label_, uri : String);
  end;

  TCDSHookResponse = class (TAdvObject)
  private
    FCards: TAdvList<TCDSHookCard>;
  public
    constructor Create; Overload; Override;
    constructor Create(params : TFHIRParameters); Overload;
    destructor Destroy; override;

    function Link : TCDSHookResponse; overload;
    function AsParams : TFHIRParameters;

    property cards : TAdvList<TCDSHookCard> read FCards;
    function addCard : TCDSHookCard;
  end;

  TCDSHookCache = class (TAdvObject)
  public
    function has(request : TCDSHookRequest; var response : TCDSHookResponse) : boolean;
    procedure see(reuqest : TCDSHookRequest; response : TCDSHookResponse);
  end;

  TCDSHooksManager = class;

  TOnAuthToServer = procedure (manager : TCDSHooksManager; server : TRegisteredFHIRServer) of object;

  // response might be nil, and then error will be provided instead
  // note: this event is called in the non-primary thread
  TOnCDSHookResponse = procedure (manager : TCDSHooksManager; server : TRegisteredFHIRServer; context : TObject; response : TCDSHookResponse; error : string) of object;

  TCDSHooksManagerServerInfo = class (TAdvObject)
  private
    FinUse: boolean;
    Finfo: TRegisteredFHIRServer;
    FToken: TSmartOnFhirAccessToken;
    procedure Setinfo(const Value: TRegisteredFHIRServer);
    procedure SetToken(const Value: TSmartOnFhirAccessToken);
  public
    destructor Destroy; override;
    function Link : TCDSHooksManagerServerInfo; overload;

    function okToUse : boolean;
    property info : TRegisteredFHIRServer read Finfo write Setinfo;
    property token : TSmartOnFhirAccessToken read FToken write SetToken;
    property inUse : boolean read FinUse write FinUse;
  end;

  TCDSHooksManagerCachedResponse = class (TAdvObject)
  private
    FResponse: TCDSHookResponse;
    FError: String;
    procedure SetResponse(const Value: TCDSHookResponse);
  public
    destructor Destroy; override;
    function Link : TCDSHooksManagerCachedResponse; overload;
    property response : TCDSHookResponse read FResponse write SetResponse;
    property error : String read FError write FError;
  end;

  TCDSHooksManagerWorkThread = class (TAdvThread)
  private
    Fmanager : TCDSHooksManager;
    Frequest: TCDSHookRequest;
    Fcontext: TObject;
    Fevent: TOnCDSHookResponse;
    Fserver: TRegisteredFHIRServer;
    FToken: TSmartOnFhirAccessToken;
    FHash : String;
    FClient : TFHIRCLient;
    FAlive : boolean;
    procedure Setrequest(const Value: TCDSHookRequest);
    procedure Setserver(const Value: TRegisteredFHIRServer);
    procedure SetToken(const Value: TSmartOnFhirAccessToken);
  protected
    Procedure Execute; override;
  public
    Constructor Create(manager : TCDSHooksManager);
    destructor Destroy; override;
    function Link : TCDSHooksManagerWorkThread; overload;

    procedure Cancel;

    property request : TCDSHookRequest read Frequest write Setrequest;
    property server : TRegisteredFHIRServer read Fserver write Setserver;
    property token : TSmartOnFhirAccessToken read FToken write SetToken;
    property event : TOnCDSHookResponse read Fevent write Fevent;
    property context : TObject read Fcontext write Fcontext;
    property hash : String read FHash write FHash;
  end;

  // manages an array of CDS Servers that might provide cdshooks support
  TCDSHooksManager = class (TAdvObject)
  private
    FOnAuthToServer: TOnAuthToServer;
    FServers : TAdvList<TCDSHooksManagerServerInfo>;
    FLock : TCriticalSection;
    FThreads : TAdvList<TCDSHooksManagerWorkThread>;
    FCache : TAdvMap<TCDSHooksManagerCachedResponse>;

    function getEntry(url : String) : TCDSHooksManagerServerInfo;
    procedure checkConnectServer(server : TCDSHooksManagerServerInfo);
    procedure closeThread(thread : TCDSHooksManagerWorkThread);
    procedure cacheResponse(hash : String; server : TRegisteredFHIRServer; response : TCDSHookResponse); overload;
    procedure cacheResponse(hash : String; server : TRegisteredFHIRServer; error : String); overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TCDSHooksManager; overload;

    // server registration and connection management

    // register this server as a hook provider.
    procedure registerServer(details : TRegisteredFHIRServer);

    // if the server list is reconfigured - need to re-register servers after this
    procedure clearServers;

    // the application has acquired valid SMART credentials
    procedure connectToServer(details : TRegisteredFHIRServer; token : TSmartOnFhirAccessToken);
    // the application has lost valid Smart credentials from the server
    procedure disconnectFromServer(details : TRegisteredFHIRServer);

    // if the application uses a hook, and there's a server not connected, then
    // the manager will auto-connect, unless it's needs Smart on FHIR login, in which case this
    // event will be called. The host needs to call connectToServer while responding to this,
    // or the server will be skipped
    property OnAuthToServer : TOnAuthToServer read FOnAuthToServer write FOnAuthToServer;

    // operational hook usage:

    // this function goes off and queries CDSHook servers that claim to support this
    procedure makeRequest(request : TCDSHookRequest; event : TOnCDSHookResponse; context : TObject);

    procedure listInProgress(list : TStrings);

    // user has changd
    procedure cancelAllRequests;

    // not sure why you'd need this...
    procedure dropCache;
  end;

function presentAsHtml(cards : TAdvList<TCDSHookCard>; inprogress, errors : TStringList) : String;

implementation

const
  CARDS_HTML_HEAD =
'<html>'+#13#10+
''+#13#10+
'<head>'+#13#10+
' <title>Focus Cards</title>'+#13#10+
' <style type="text/css">'+#13#10+
'  /*! normalize.css v3.0.3 | MIT License | github.com/necolas/normalize.css */'+#13#10+
''+#13#10+
' html {'+#13#10+
'     font-family: sans-serif;'+#13#10+
'     -ms-text-size-adjust: 100%;'+#13#10+
'     -webkit-text-size-adjust: 100%'+#13#10+
'        height: 100%'+#13#10+
'    }'+#13#10+
'    body {'+#13#10+
'        min-height: 100%;'+#13#10+
'        display: flex;'+#13#10+
'        flex-flow: column'+#13#10+
'    }'+#13#10+
'    .hook-editor {'+#13#10+
'        display: inline'+#13#10+
'    }'+#13#10+
'    .configure-hooks {'+#13#10+
'        float: right;'+#13#10+
'        cursor: pointer'+#13#10+
'    }'+#13#10+
'    .save-hook {'+#13#10+
'        float: right;'+#13#10+
'        background: #fff;'+#13#10+
'        border: 1px solid #565a5c;'+#13#10+
'        font-weight: 700;'+#13#10+
'        color: #565a5c'+#13#10+
'    }'+#13#10+
'    .save-hook:disabled {'+#13#10+
'        font-weight: 400'+#13#10+
'    }'+#13#10+
'    .edit-hook {'+#13#10+
'        margin: 2em'+#13#10+
'    }'+#13#10+
'    .edit-hook.new-hook .edit-hook-inner {'+#13#10+
'        border: 2px dashed #fff'+#13#10+
'    }'+#13#10+
'    .edit-hook-inner {'+#13#10+
'        border: 2px solid #fff;'+#13#10+
'        padding: 2em;'+#13#10+
'        display: block;'+#13#10+
'        font-family: Inconsolata, Menlo, Consolas, monospace!important;'+#13#10+
'        font-size: 1rem;'+#13#10+
'        margin-top: .5em'+#13#10+
'    }'+#13#10+
'    .decision-spacer {'+#13#10+
'        height: 2em'+#13#10+
'    }'+#13#10+
'    .decision-card:hover {'+#13#10+
'        background: #f6f6f6'+#13#10+
'    }'+#13#10+
'    .card-top {'+#13#10+
'        display: flex;'+#13#10+
'        flex-direction: column;'+#13#10+
'        flex-grow: 1'+#13#10+
'    }'+#13#10+
'    .decision-card {'+#13#10+
'        border: 2px solid #f3f3f3;'+#13#10+
'        border-radius: 5px;'+#13#10+
'        display: flex;'+#13#10+
'        flex-flow: column wrap;'+#13#10+
'        padding: 1em;'+#13#10+
'        border-left-width: 3px;'+#13#10+
'        background: #fcfcfc'+#13#10+
'    }'+#13#10+
'    .decision-card.alert-success {'+#13#10+
'        border-left-color: #449d44;'+#13#10+
'        color: #449d44'+#13#10+
'    }'+#13#10+
'    .decision-card.alert-info {'+#13#10+
'        border-left-color: #31b0d5'+#13#10+
'    }'+#13#10+
'    .alert-warning {'+#13#10+
'        color: #c57300!important'+#13#10+
'    }'+#13#10+
'    .decision-card.alert-warning {'+#13#10+
'        border-left-color: #ec971f'+#13#10+
'    }'+#13#10+
'    .decision-card.alert-danger {'+#13#10+
'        border-left-color: #c9302c'+#13#10+
'    }'+#13#10+
'    decision-card button {'+#13#10+
'        margin-top: .5em;'+#13#10+
'        margin-bottom: .5em;'+#13#10+
'        margin-right: .5em'+#13#10+
'    }'+#13#10+
'    .decision-card .glyphicon {'+#13#10+
'        margin-right: .4em'+#13#10+
'    }'+#13#10+
'    .decision-card img {'+#13#10+
'        max-width: 100%'+#13#10+
'    }'+#13#10+
'    .card-summary {'+#13#10+
'        font-weight: 200;'+#13#10+
'        font-size: 0.8em;'+#13#10+
'        flex-grow: 1'+#13#10+
'    }'+#13#10+
'    .card-source {'+#13#10+
'        font-size: .7em;'+#13#10+
'        font-style: italic'+#13#10+
'        text-alignment: right'+#13#10+
'    }'+#13#10+
'    .card-links {'+#13#10+
'        font-size: .7em;'+#13#10+
'        font-style: italic'+#13#10+
'        text-alignment: right'+#13#10+
'    }'+#13#10+
'    .card-helpful {'+#13#10+
'        font-style: normal'+#13#10+
'    }'+#13#10+
'    .card-helpful button.btn {'+#13#10+
'        vertical-align: initial'+#13#10+
'    }'+#13#10+
'    div.card-helpful button.btn {'+#13#10+
'        padding: 0;'+#13#10+
'        padding-left: 3px;'+#13#10+
'        padding-right: 3px;'+#13#10+
'        margin: 0'+#13#10+
'    }'+#13#10+
'    </style>'+#13#10+
'</head>'+#13#10+
'<body>'+#13#10;

  CARDS_HTML_FOOT =
'</body>'+#13#10+
'</html>'+#13#10;

function makeUrlSafe(url : String) : string;
begin
  if url.startsWith('http:') or url.startsWith('https:')  then
    result := url.replace('"', '%22').replace('>', '%3E')
  else
    result := 'about:security-risk';
end;

function presentAsHtml(cards : TAdvList<TCDSHookCard>; inprogress, errors : TStringList) : String;
var
  b : TStringBuilder;
  card : TCDSHookCard;
  md : TMarkdownProcessor;
  s : String;
  l : TCDSHookCardLink;
  first : boolean;
begin
  b := TStringBuilder.Create;
  try
    b.Append(CARDS_HTML_HEAD);
    for card in cards do
    begin
      b.Append('<div class="decision-card alert alert-info">'#13#10);
      b.Append(' <div class="card-top">'#13#10);
      b.Append('  <div class="card-summary">');
      if card.detail <> '' then
      begin
        md := TMarkdownProcessor.CreateDialect(mdDaringFireball);
        try
          md.UnSafe := false;
          b.Append(md.process(card.detail));
        finally
          md.Free;
        end;
      end
      else
        b.Append(FormatTextToHTML(card.summary));
      b.Append('</div>'#13#10);
      if card.links.Count > 0 then
      begin
        b.Append('  <div class="card-links"><p>Links: ');
        first := true;
        for l in card.links do
        begin
          if first then
            first := false
          else
            b.append(' | ');
          b.Append('<a href="'+makeUrlSafe(l.FUrl)+'">'+FormatTextToHTML(l.label_)+'</a>');
        end;
        b.Append('   </p></div>');
      end;
      b.Append('  <div class="card-source">');
      b.Append(FormatTextToHTML(card.sourceLabel));
      b.Append('</div>'#13#10);
      b.Append(' </div>'#13#10);
      b.Append('</div>'#13#10);
    end;
    if inprogress.Count > 0 then
    begin
      b.Append('<p>In Progress:</p>'#13#10+'<ul>'#13#10);
      for s in inprogress do
        b.Append(' <li>'+s+'</li>'#13#10);
      b.Append('</ul>'#13#10);
    end;

    if errors.Count > 0 then
    begin
      b.Append('<p>Errors:</p>'#13#10+'<ul>'#13#10);
      for s in errors do
        b.Append(' <li>'+s+'</li>'#13#10);
      b.Append('</ul>'#13#10);
    end;
    b.Append(CARDS_HTML_FOOT);
    result := b.toString();
  finally
    b.Free;
  end;
end;


{ TCDSHookRequest }

function TCDSHookRequest.AsParams: TFHIRParameters;
var
  p : TFhirParametersParameter;
  c : TFhirResource;
begin
  result := TFHIRParameters.create;
  try
    result.AddParameter('activity', Factivity.Link);
    result.AddParameter('activityInstance', FactivityInstance);
    result.AddParameter('fhirServer', FfhirServer);
    if (FoauthToken <> '') then
    begin
      p := Result.parameterList.Append;
      p.name := 'oauth';
      p.AddParameter('token', FoauthToken);
      p.AddParameter('scope', FoauthScope);
      p.AddParameter('expires', inttostr(FoauthExpires));
    end;
    result.AddParameter('redirect', Fredirect);
    result.AddParameter('user', Fuser);
    result.AddParameter('patient', Fpatient);
    result.AddParameter('encounter', Fencounter);
    for c in context do
      result.AddParameter('context', c.Link);
    if preFetchData <> nil then
      result.AddParameter('preFetchData', preFetchData.Link);
    result.link;
  finally
    result.free;
  end;
end;

constructor TCDSHookRequest.Create;
begin
  inherited Create;
  FContext := TAdvList<TFHIRResource>.create;
end;

constructor TCDSHookRequest.Create(params: TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  Create;
  Fencounter := params.str['encounter'];
  FfhirServer := params.str['fhirServer'];
  Fpatient := params.str['patient'];
  FactivityInstance := params.str['activityInstance'];
  Fuser := params.str['user'];
  Fredirect := params.str['redirect'];
  Factivity := (params['activity'] as TFHIRCoding).Link;
  p := params.param['oauth'];
  if (p <> nil) then
  begin
    FoauthExpires := StrToIntDef(p.str['expires'], 0);
    FoauthScope := p.str['scope'];
    FoauthToken := p.str['token'];
  end;
  for p in params.parameterList do
    if p.name = 'context' then
      FContext.Add(p.resource.Link)
    else if (p.name = 'preFetchData') and (preFetchData = nil) and (p.resource is TFhirBundle) then
      preFetchData := (p.resource as TFhirBundle).Link;
end;

destructor TCDSHookRequest.Destroy;
begin
  FBundle.Free;
  FContext.Free;
  Factivity.Free;
  inherited;
end;

function TCDSHookRequest.hash: Cardinal;
var
  params : TFhirParameters;
  json : TFHIRJsonComposer;
  s : String;
begin
  params := AsParams;
  try
    json := TFHIRJsonComposer.Create('en');
    try
      s := json.Compose('request', params, false);
      result := HashStringToCode32(s);
    finally
      json.Free;
    end;
  finally
    params.Free;
  end;
end;

function TCDSHookRequest.Link: TCDSHookRequest;
begin
  result := TCDSHookRequest(inherited Link);
end;

procedure TCDSHookRequest.Setactivity(const Value: TFHIRCoding);
begin
  Factivity.Free;
  Factivity := Value;
end;

procedure TCDSHookRequest.SetBundle(const Value: TFHIRBundle);
begin
  FBundle.Free;
  FBundle := Value;
end;

{ TCDSHookCardSuggestion }

function TCDSHookCardSuggestion.AsParam: TFHIRParametersParameter;
begin
  result := TFhirParametersParameter.Create;
  try
    result.AddParameter('label', FLabel);
    result.link;
  finally
    result.free;
  end;
end;

constructor TCDSHookCardSuggestion.Create;
begin
  inherited Create;
  // nothing yet
end;

constructor TCDSHookCardSuggestion.Create(params: TFHIRParametersParameter);
begin
  Create;
  FLabel := params.str['label'];
end;

destructor TCDSHookCardSuggestion.Destroy;
begin
  // nothing yet
  inherited;
end;

function TCDSHookCardSuggestion.Link: TCDSHookCardSuggestion;
begin
  result := TCDSHookCardSuggestion(inherited Link);

end;

{ TCDSHookCardLink }

function TCDSHookCardLink.AsParam: TFHIRParametersParameter;
begin
  result := TFhirParametersParameter.Create;
  try
    result.AddParameter('label', FLabel);
    result.AddParameter('url', FUrl);

    result.link;
  finally
    result.free;
  end;
end;

constructor TCDSHookCardLink.Create;
begin
  inherited Create;
  // nothing yet
end;

constructor TCDSHookCardLink.Create(params: TFHIRParametersParameter);
begin
  Create;
  FLabel := params.str['label'];
  FUrl := params.str['url'];
end;

destructor TCDSHookCardLink.Destroy;
begin
  // nothing yet
  inherited;
end;

function TCDSHookCardLink.Link: TCDSHookCardLink;
begin
  result := TCDSHookCardLink(inherited Link);
end;

{ TCDSHookCard }

procedure TCDSHookCard.addLink(label_, uri: String);
var
  link : TCDSHookCardLink;
begin
  link := TCDSHookCardLink.Create;
  try
    link.label_ := label_;
    link.url := uri;
    links.Add(link.Link);
  finally
    link.Free;
  end;
end;

function TCDSHookCard.AsParam: TFHIRParametersParameter;
var
  p : TFhirParametersParameter;
  link : TCDSHookCardLink;
begin
  result := TFhirParametersParameter.Create;
  try
    result.AddParameter('summary', Fsummary);
    result.AddParameter('indicator', Findicator);
    result.AddParameter('detail', Fdetail);
    if (sourceLabel <> '') then
    begin
      p := result.partList.Append;
      p.name := 'source';
      p.AddParameter('label', FsourceLabel);
      p.AddParameter('url', FSourceURL);
    end;
    for link in FLinks do
    begin
      p := link.AsParam;
      p.name := 'link';
      result.partList.Add(p);
    end;
    result.link;
  finally
    result.free;
  end;
end;

constructor TCDSHookCard.Create;
begin
  inherited Create;
  FSuggestions := TAdvList<TCDSHookCardSuggestion>.create;
  FLinks := TAdvList<TCDSHookCardLink>.create;
end;

constructor TCDSHookCard.Create(params: TFHIRParametersParameter);
var
  p : TFHIRParametersParameter;
begin
  Create;
  Fsummary := params.str['summary'];
  Findicator := params.str['indicator'];
  Fdetail := params.str['detail'];
  p := params.param['source'];
  if (p <> nil) then
  begin
    FsourceLabel := p.str['label'];
    FSourceURL := p.str['url'];
  end;
  for p in params.partList do
    if p.name = 'suggestion' then
      FSuggestions.Add(TCDSHookCardSuggestion.Create(p))
    else if p.name = 'link' then
      FLinks.Add(TCDSHookCardLink.Create(p))
end;

destructor TCDSHookCard.Destroy;
begin
  FSuggestions.Free;
  FLinks.Free;
  inherited;
end;

function TCDSHookCard.Link: TCDSHookCard;
begin
  result := TCDSHookCard(inherited Link);
end;

constructor TCDSHookCard.Create(summary, sourceLabel: String);
begin
  Create;
  self.summary := summary;
  self.sourceLabel := sourceLabel;
end;

constructor TCDSHookCard.Create(summary, detail, sourceLabel: String);
begin
  Create;
  self.summary := summary;
  self.detail := detail;
  self.sourceLabel := sourceLabel;
end;

{ TCDSHookResponse }

function TCDSHookResponse.addCard: TCDSHookCard;
begin
  result := TCDSHookCard.Create;
  cards.Add(result);
end;

function TCDSHookResponse.AsParams: TFHIRParameters;
var
  p : TFhirParametersParameter;
  card : TCDSHookCard;
begin
  result := TFHIRParameters.create;
  try
    for card in FCards do
    begin
      p := card.AsParam;
      p.name := 'card';
      result.parameterList.Add(p);
    end;
    result.link;
  finally
    result.free;
  end;
end;

constructor TCDSHookResponse.Create(params: TFHIRParameters);
var
  p : TFHIRParametersParameter;
begin
  Create;
  for p in params.parameterList do
    if p.name = 'card' then
      FCards.Add(TCDSHookCard.Create(p))
end;

constructor TCDSHookResponse.Create;
begin
  inherited Create;
  FCards := TAdvList<TCDSHookCard>.create
end;

destructor TCDSHookResponse.Destroy;
begin
  FCards.Free;
  inherited;
end;

function TCDSHookResponse.Link: TCDSHookResponse;
begin
  result := TCDSHookResponse(inherited Link);
end;




{ TCDSHookCache }

function TCDSHookCache.has(request: TCDSHookRequest;
  var response: TCDSHookResponse): boolean;
begin
   result := false;
end;

procedure TCDSHookCache.see(reuqest: TCDSHookRequest;
  response: TCDSHookResponse);
begin

end;

{ TCDSHooks }

class function TCDSHooks.allHooks: TAdvList<TFhirCoding>;
begin
  result := TAdvList<TFhirCoding>.create;
  result.Add(codeView);
  result.Add(identifierView);
  result.Add(patientView);
end;

class function TCDSHooks.identifierView: TFhirCoding;
begin
  result := TFhirCoding.Create;
  result.system := 'http://cds-hooks.smarthealthit.org/activity';
  result.code := 'identifier-view';
  result.display := 'Provide any available information about an Identifier';
end;

class function TCDSHooks.isIdentifierView(c: TFhirCoding): boolean;
begin
  result := (c.system = 'http://cds-hooks.smarthealthit.org/activity') and (c.code = 'identifier-view');
end;

class function TCDSHooks.isKnownHook(c: TFhirCoding): boolean;
begin
  result := isPatientView(c) or isCodeView(c) or isIdentifierView(c);
end;

class function TCDSHooks.patientView: TFhirCoding;
begin
  result := TFhirCoding.Create;
  result.system := 'http://cds-hooks.smarthealthit.org/activity';
  result.code := 'patient-view';
  result.display := 'Provide any important information about a patient';
end;

class function TCDSHooks.isPatientView(c: TFhirCoding): boolean;
begin
  result := (c.system = 'http://cds-hooks.smarthealthit.org/activity') and (c.code = 'patient-view');
end;

class function TCDSHooks.codeView: TFhirCoding;
begin
  result := TFhirCoding.Create;
  result.system := 'http://cds-hooks.smarthealthit.org/activity';
  result.code := 'code-view';
  result.display := 'Provide any available information about a Coding/CodeableConcept';
end;

class function TCDSHooks.isCodeView(c: TFhirCoding): boolean;
begin
  result := (c.system = 'http://cds-hooks.smarthealthit.org/activity') and (c.code = 'code-view');
end;

{ TCDSHooksManager }

constructor TCDSHooksManager.Create;
begin
  inherited;
  FServers := TAdvList<TCDSHooksManagerServerInfo>.create;
  FLock := TCriticalSection.Create('TCDSHooksManager');
  FThreads := TAdvList<TCDSHooksManagerWorkThread>.create;
  FCache := TAdvMap<TCDSHooksManagerCachedResponse>.create;
end;

destructor TCDSHooksManager.Destroy;
begin
  cancelAllRequests;
  FThreads.free;
  FServers.Free;
  FCache.Free;
  FLock.Free;
  inherited;
end;

function TCDSHooksManager.Link: TCDSHooksManager;
begin
  result := TCDSHooksManager(inherited Link);
end;

procedure TCDSHooksManager.listInProgress(list: TStrings);
var
  thread : TCDSHooksManagerWorkThread;
begin
  FLock.Lock;
  try
    for thread in FThreads do
      if thread.FAlive then
        list.Add(thread.server.name);
  finally
    FLock.Unlock;
  end;
end;

procedure TCDSHooksManager.registerServer(details: TRegisteredFHIRServer);
var
  entry : TCDSHooksManagerServerInfo;
begin
  entry := TCDSHooksManagerServerInfo.Create;
  try
    entry.FinUse := true;
    entry.Finfo := details.Link;
    FServers.Add(entry.Link);
  finally
    entry.Free;
  end;
end;

procedure TCDSHooksManager.checkConnectServer(server: TCDSHooksManagerServerInfo);
begin
  if not server.info.SmartOnFHIR then
    exit;
  if server.token <> nil then
    exit;
  if assigned(FOnAuthToServer) then
    FOnAuthToServer(self, server.info);
  if server.token = nil then
    server.inUse := false;
end;

procedure TCDSHooksManager.clearServers;
begin
  cancelAllRequests;
  FServers.Clear;
  dropCache;
end;

procedure TCDSHooksManager.closeThread(thread: TCDSHooksManagerWorkThread);
begin
  FLock.Lock;
  try
    FThreads.Remove(thread);
    thread.Fmanager := nil;
  finally
    FLock.Unlock;
  end;
end;

procedure TCDSHooksManager.connectToServer(details: TRegisteredFHIRServer; token : TSmartOnFhirAccessToken);
var
  entry : TCDSHooksManagerServerInfo;
begin
  entry := getEntry(details.fhirEndpoint);
  if (entry <> nil) then
  begin
    entry.inUse := true;
    entry.token := token.link;
  end;
end;

procedure TCDSHooksManager.disconnectFromServer(details: TRegisteredFHIRServer);
var
  entry : TCDSHooksManagerServerInfo;
begin
  entry := getEntry(details.fhirEndpoint);
  if (entry <> nil) then
    entry.token := nil;
end;

procedure TCDSHooksManager.dropCache;
begin
  FLock.Lock;
  try
    FCache.Clear;
  finally
    FLock.Unlock;
  end;
end;

function TCDSHooksManager.getEntry(url: String): TCDSHooksManagerServerInfo;
var
  i : TCDSHooksManagerServerInfo;
begin
  for i in FServers do
    if i.info.fhirEndpoint = url then
      exit(i);
  result := nil;
end;

procedure TCDSHooksManager.makeRequest(request: TCDSHookRequest; event: TOnCDSHookResponse; context: TObject);
var
  server : TCDSHooksManagerServerInfo;
  work : TCDSHooksManagerWorkThread;
  hash : String;
  cached : TCDSHooksManagerCachedResponse;
begin
  hash := IntToStr(request.hash);
  for server in FServers do
  begin
    if server.info.doesHook(request.activity) then
    begin
      FLock.Lock;
      try
        if FCache.TryGetValue(hash+'|'+server.info.fhirEndpoint, cached) then
          event(self, server.info, context, cached.response, cached.error)
        else
        begin
          checkConnectServer(server);
          if server.okToUse then
          begin
            work := TCDSHooksManagerWorkThread.create(self);
            try
              work.request := request.Link;
              work.server := server.info.Link;
              work.token := server.token.link; // do this here to avoid threading problems
              work.event := event;
              work.context := context;
              work.hash := hash;
              FThreads.add(work.link);
              work.Open;
            finally
              work.Free;
            end;
          end;
        end;
      finally
        FLock.Unlock;
      end;
    end;
  end;
end;

procedure TCDSHooksManager.cacheResponse(hash: String; server: TRegisteredFHIRServer; response: TCDSHookResponse);
var
  cache : TCDSHooksManagerCachedResponse;
begin
  cache := TCDSHooksManagerCachedResponse.Create;
  try
    cache.response := response.Link;
    FCache.AddOrSetValue(hash+'|'+server.fhirEndpoint, cache.link);
  finally
    cache.free;
  end;
end;

procedure TCDSHooksManager.cacheResponse(hash: String; server: TRegisteredFHIRServer; error: String);
var
  cache : TCDSHooksManagerCachedResponse;
begin
  cache := TCDSHooksManagerCachedResponse.Create;
  try
    cache.error := error;
    FCache.Add(hash+'|'+server.fhirEndpoint, cache.link);
  finally
    cache.free;
  end;
end;

procedure TCDSHooksManager.cancelAllRequests;
var
  thread : TCDSHooksManagerWorkThread;
begin
  FLock.Lock;
  try
    for thread in FThreads do
      thread.Cancel;
  finally
    FLock.Unlock;
  end;
end;

{ TCDSHooksManagerServerInfo }

destructor TCDSHooksManagerServerInfo.Destroy;
begin
  FToken.Free;
  FInfo.Free;
  inherited;
end;

function TCDSHooksManagerServerInfo.Link: TCDSHooksManagerServerInfo;
begin
  result := TCDSHooksManagerServerInfo(inherited Link);
end;

function TCDSHooksManagerServerInfo.okToUse: boolean;
begin
  result := FinUse and (not info.SmartOnFHIR or (token <> nil));
end;

procedure TCDSHooksManagerServerInfo.Setinfo(const Value: TRegisteredFHIRServer);
begin
  FInfo.Free;
  Finfo := Value;
end;

procedure TCDSHooksManagerServerInfo.SetToken(const Value: TSmartOnFhirAccessToken);
begin
  FToken.Free;
  FToken := Value;
end;

{ TCDSHooksManagerWorkThread }

procedure TCDSHooksManagerWorkThread.Cancel;
begin
  try
    FAlive := false;
  except
  end;
end;

constructor TCDSHooksManagerWorkThread.Create(manager: TCDSHooksManager);
begin
  inherited create;
  FManager := manager;
  FAlive := true;
end;

destructor TCDSHooksManagerWorkThread.Destroy;
begin
  Fserver.Free;
  Frequest.Free;
  FToken.Free;
  inherited;
end;

procedure TCDSHooksManagerWorkThread.Execute;
var
  pin, pout : TFhirParameters;
  resp : TCDSHookResponse;
begin
  try
    try
      try
        FClient := TFhirClient.Create(server.fhirEndpoint, server.format = ffJson);
        try
          FClient.smartToken := token.link;
          pin := Frequest.AsParams;
          try
            pout := FClient.operation(frtNull, 'cds-hook', pin) as TFhirParameters;
            try
              resp := TCDSHookResponse.Create(pout);
              try
               if FManager <> nil then
               begin
                 FManager.cacheResponse(hash, server, resp);
                 if (FAlive) then
                   event(FManager, server, FContext, resp, '');
               end;
              finally
                resp.Free;
              end;
            finally
              pout.Free;
            end;
          finally
            pin.Free;
          end;
        finally
          FClient.Free;
        end;
      except
        on e : Exception do
          if FManager <> nil then
          begin
            FManager.cacheResponse(hash, server, e.message);
            if (FAlive) then
              event(FManager, server, FContext, nil, e.message);
          end;
      end;
    finally
      if (FAlive) then
        Fmanager.closeThread(self);
    end;
  except
    // nothing. just suppress this
  end;
end;

function TCDSHooksManagerWorkThread.Link: TCDSHooksManagerWorkThread;
begin
  result := TCDSHooksManagerWorkThread(inherited Link);
end;

procedure TCDSHooksManagerWorkThread.Setrequest(const Value: TCDSHookRequest);
begin
  Frequest.Free;
  Frequest := Value;
end;

procedure TCDSHooksManagerWorkThread.Setserver(const Value: TRegisteredFHIRServer);
begin
  Fserver.Free;
  Fserver := Value;
end;

procedure TCDSHooksManagerWorkThread.SetToken(const Value: TSmartOnFhirAccessToken);
begin
  FToken.Free;
  FToken := Value;
end;

{ TCDSHooksManagerCachedResponse }

destructor TCDSHooksManagerCachedResponse.Destroy;
begin
  FResponse.Free;
  inherited;
end;

function TCDSHooksManagerCachedResponse.Link: TCDSHooksManagerCachedResponse;
begin
  result := TCDSHooksManagerCachedResponse(inherited Link);
end;

procedure TCDSHooksManagerCachedResponse.SetResponse(const Value: TCDSHookResponse);
begin
  FResponse.Free;
  FResponse := Value;
end;

end.
