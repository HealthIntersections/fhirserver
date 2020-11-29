unit fhir_cdshooks;

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
  {$IFDEF WINDOWS} Windows, ActiveX, {$ENDIF}
  SysUtils, Classes,
  MarkdownProcessor,
  fsl_base, fsl_utilities, fsl_stream, fsl_threads, fsl_json, fsl_http,
  fhir_objects, fhir_parser, fhir_oauth, fhir_client, fhir_client_http;

type
  TCDSHooks = class
  public
    class function patientView : String;
    class function isPatientView(c : String) : boolean;

    class function codeView : String;
    class function isCodeView(c : String) : boolean;

    class function identifierView : String;
    class function isIdentifierView(c : String) : boolean;

    class function allHooks : TStringList;
    class function isKnownHook(c : String) : boolean;
  end;

  TCDSRequestOAuthDetails = class (TFslObject)
  private
    FExpires : integer;
    FScopes : string;
    FToken : string;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Property Expires : integer read FExpires write FExpires;
    Property Scopes : string read FScopes write FScopes;
    Property Token : string read FToken write FToken;
  end;

  TCDSHookRequest = class (TFslObject)
  private
    FHook : String;
    FHookInstance : String;
    FFhirServer: String;
    Foauth: TCDSRequestOAuthDetails;
    Fredirect: String;
    Fuser: String;
    Fpatient: String;
    Fencounter: String;
    FLang : THTTPLanguages;
    FContext: TFslList<TFHIRResourceV>;
    FPreFetch : TFslMap<TFhirObject>;
    FBaseUrl: String;

  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Overload; Override;
    constructor Create(json : TJsonObject); Overload;
    destructor Destroy; override;

    function Link : TCDSHookRequest; overload;
    function hash : Integer;

    function AsJson : String;

    property hook : String read FHook write FHook;
    property hookInstance : String read FHookInstance write FHookInstance;
    property fhirServer : String read FfhirServer write FfhirServer;
    property oauth : TCDSRequestOAuthDetails read Foauth write Foauth;
    property redirect : String read Fredirect write Fredirect;
    property user : String read Fuser write Fuser;
    property patient : String read Fpatient write Fpatient;
    property encounter : String read Fencounter write Fencounter;
    property context : TFslList<TFHIRResourceV> read FContext;
    property preFetch : TFslMap<TFhirObject{BundleEntry}> read FPreFetch;
    property lang : THTTPLanguages read FLang write FLang;
    property baseURL : String read FBaseUrl write FBaseUrl;
  end;

  TCDSHookCardSuggestion = class (TFslObject)
  private
    FLabel: String;
    FUUID: String;
    FCreate: String;
    FDelete: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Overload; Override;
    constructor Create(json : TJsonObject); Overload;
    destructor Destroy; override;

    function Link : TCDSHookCardSuggestion; overload;
    procedure AsJson(json : TJsonWriter);

    property label_ : String read FLabel write FLabel;
    property uuid : String read FUUID write FUUID;
    property create_ : String read FCreate write FCreate;
    property delete : String read FDelete write FDelete;
  end;

  TCDSHookCardLink = class (TFslObject)
  private
    FLabel: String;
    FUrl: String;
    FType: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Overload; Override;
    constructor Create(json : TJsonObject); Overload;
    destructor Destroy; override;

    function Link : TCDSHookCardLink; overload;
    procedure AsJson(json : TJsonWriter);

    property label_ : String read FLabel write FLabel;
    property url : String read FUrl write FUrl;
    property type_ : String read FType write FType;
  end;

  TCDSHookCard = class (TFslObject)
  private
    FSourceURL: String;
    Fdetail: String;
    FsourceLabel: String;
    Fsummary: String;
    Findicator: String;
    FSuggestions: TFslList<TCDSHookCardSuggestion>;
    FLinks: TFslList<TCDSHookCardLink>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Overload; Override;
    constructor Create(json : TJsonObject); Overload;
    constructor Create(summary, sourceLabel : String); Overload;
    constructor Create(summary, detail, sourceLabel : String); Overload;
    destructor Destroy; override;

    function Link : TCDSHookCard; overload;
    procedure AsJson(json : TJsonWriter);

    property summary : String read Fsummary write Fsummary;
    property detail : String read Fdetail write Fdetail;
    property indicator : String read Findicator write Findicator;
    property sourceLabel : String read FsourceLabel write FsourceLabel;
    property sourceURL : String read FSourceURL write FSourceURL;
    property suggestions : TFslList<TCDSHookCardSuggestion> read FSuggestions;
    property links : TFslList<TCDSHookCardLink> read FLinks;

    procedure addLink(label_, uri : String);
  end;

  TCDSHookDecision = class (TFslObject)
  private
    FCreate: TStringList;
    FDelete: TStringList;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Overload; Override;
    constructor Create(json : TJsonObject); Overload;
    destructor Destroy; override;

    function Link : TCDSHookDecision; overload;
    procedure AsJson(json : TJsonWriter);

    property Create_ : TStringList read FCreate;
    property Delete : TStringList read FDelete;
  end;

  TCDSHookResponse = class (TFslObject)
  private
    FCards: TFslList<TCDSHookCard>;
    FDecisions : TFslList<TCDSHookDecision>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Overload; Override;
    constructor Create(json : TJsonObject); Overload;
    destructor Destroy; override;

    function Link : TCDSHookResponse; overload;
    function AsJson : String;

    property cards : TFslList<TCDSHookCard> read FCards;
    function addCard : TCDSHookCard;
    property decisions : TFslList<TCDSHookDecision> read FDecisions;
    function addDecision : TCDSHookDecision;
  end;

  TCDSHookCache = class (TFslObject)
  public
    function has(request : TCDSHookRequest; var response : TCDSHookResponse) : boolean;
    procedure see(reuqest : TCDSHookRequest; response : TCDSHookResponse);
  end;

type
  TCDSHooksManager = class;

  TOnAuthToServer = procedure (manager : TCDSHooksManager; server : TRegisteredFHIRServer) of object;

  // response might be nil, and then error will be provided instead
  // note: this event is called in the non-primary thread
  TOnCDSHookResponse = procedure (manager : TCDSHooksManager; server : TRegisteredFHIRServer; context : TObject; response : TCDSHookResponse; error : string) of object;

  TCDSHooksManagerServerInfo = class (TFslObject)
  private
    FinUse: boolean;
    Finfo: TRegisteredFHIRServer;
    FToken: TClientAccessToken;
    procedure Setinfo(const Value: TRegisteredFHIRServer);
    procedure SetToken(const Value: TClientAccessToken);
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    function Link : TCDSHooksManagerServerInfo; overload;

    function okToUse : boolean;
    property info : TRegisteredFHIRServer read Finfo write Setinfo;
    property token : TClientAccessToken read FToken write SetToken;
    property inUse : boolean read FinUse write FinUse;
  end;

  TCDSHooksManagerCachedResponse = class (TFslObject)
  private
    FResponse: TCDSHookResponse;
    FError: String;
    procedure SetResponse(const Value: TCDSHookResponse);
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    function Link : TCDSHooksManagerCachedResponse; overload;
    property response : TCDSHookResponse read FResponse write SetResponse;
    property error : String read FError write FError;
  end;

  {$HINTS OFF} // while in dev
  TCDSHooksManagerWorkThread = class (TFslThread)
  private
    Fmanager : TCDSHooksManager;
    Frequest: TCDSHookRequest;
    Fcontext: TObject;
    Fevent: TOnCDSHookResponse;
    Fserver: TRegisteredFHIRServer;
    FToken: TClientAccessToken;
    FHash : String;
    FAlive : boolean;
    FID: String;
    FHeaders : THTTPHeaders;
    procedure Setrequest(const Value: TCDSHookRequest);
    procedure Setserver(const Value: TRegisteredFHIRServer);
    procedure SetToken(const Value: TClientAccessToken);

    function makeBody : TFslBuffer;
    function readBody(body : TFslBuffer) : TCDSHookResponse;
  protected
    Procedure Execute; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(manager : TCDSHooksManager);
    destructor Destroy; override;
    function Link : TCDSHooksManagerWorkThread; overload;

    procedure Cancel;

    property id : String read FID write FID;
    property request : TCDSHookRequest read Frequest write Setrequest;
    property server : TRegisteredFHIRServer read Fserver write Setserver;
    property token : TClientAccessToken read FToken write SetToken;
    property event : TOnCDSHookResponse read Fevent write Fevent;
    property context : TObject read Fcontext write Fcontext;
    property hash : String read FHash write FHash;
  end;

  // manages an array of CDS Servers that might provide cdshooks support
  TCDSHooksManager = class (TFslObject)
  private
    FOnAuthToServer: TOnAuthToServer;
    FServers : TFslList<TCDSHooksManagerServerInfo>;
    FLock : TFslLock;
    FThreads : TFslList<TCDSHooksManagerWorkThread>;
    FCache : TFslMap<TCDSHooksManagerCachedResponse>;

    function getEntry(url : String) : TCDSHooksManagerServerInfo;
    procedure checkConnectServer(server : TCDSHooksManagerServerInfo);
    procedure closeThread(thread : TCDSHooksManagerWorkThread);
    procedure cacheResponse(hash : String; server : TRegisteredFHIRServer; response : TCDSHookResponse); overload;
    procedure cacheResponse(hash : String; server : TRegisteredFHIRServer; error : String); overload;
  protected
    function sizeInBytesV : cardinal; override;
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
    procedure connectToServer(details : TRegisteredFHIRServer; token : TClientAccessToken);
    // the application has lost valid Smart credentials from the server
    procedure disconnectFromServer(details : TRegisteredFHIRServer);

    // if the application uses a hook, and there's a server not connected, then
    // the manager will auto-connect, unless it's needs Smart App Launch login, in which case this
    // event will be called. The host needs to call connectToServer while responding to this,
    // or the server will be skipped
    property OnAuthToServer : TOnAuthToServer read FOnAuthToServer write FOnAuthToServer;

    // operational hook usage:

    // this function goes off and queries CDSHook servers that claim to support this
    procedure makeRequest(request : TCDSHookRequest; event : TOnCDSHookResponse; context : TObject);

    procedure listInProgress(list : TStrings);

    // user has changd
    procedure cancelAllRequests;
    function waiting : boolean;

    // not sure why you'd need this...
    procedure dropCache;
  end;

function presentAsHtml(cards : TFslList<TCDSHookCard>; inprogress, errors : TStringList) : String;

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

function presentAsHtml(cards : TFslList<TCDSHookCard>; inprogress, errors : TStringList) : String;
var
  b : TStringBuilder;
  card : TCDSHookCard;
  md : TMarkdownProcessor;
  s : String;
  l : TCDSHookCardLink;
  first : boolean;
  cnt : boolean;
begin
  b := TStringBuilder.Create;
  try
    b.Append(CARDS_HTML_HEAD);
    cnt := false;
    for card in cards do
    begin
      cnt := true;
      b.Append('<div class="decision-card alert alert-info">'#13#10);
      b.Append(' <div class="card-top">'#13#10);
      b.Append('  <div class="card-summary">');
      if card.detail <> '' then
      begin
        md := TMarkdownProcessor.CreateDialect(mdCommonMark);
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
    if (inprogress <> nil) and (inprogress.Count > 0) then
    begin
      cnt := true;
      b.Append('<p>In Progress:</p>'#13#10+'<ul>'#13#10);
      for s in inprogress do
        b.Append(' <li>'+s+'</li>'#13#10);
      b.Append('</ul>'#13#10);
    end;

    if errors.Count > 0 then
    begin
      cnt := true;
      b.Append('<p>Errors:</p>'#13#10+'<ul>'#13#10);
      for s in errors do
        b.Append(' <li>'+s+'</li>'#13#10);
      b.Append('</ul>'#13#10);
    end;
    if not cnt then
      b.append('<p><i>Nothing to display</i></p>');
    b.Append(CARDS_HTML_FOOT);
    result := b.toString();
  finally
    b.Free;
  end;
end;


{ TCDSHookRequest }

function TCDSHookRequest.AsJson: String;
//var
//  ss : TFslStringStream;
//  writer : TJSONWriter;
//  c : TFhirResourceV;
//  comp : TFHIRComposer;
//  s : String;
//  be : TFhirObject{BundleEntry};
begin
(*  ss := TFslStringStream.Create;
  try
    writer := TJsonWriterDirect.create;
    try
      writer.HasWhitespace := true;
      writer.Stream := ss.Link;
      writer.Start;
      writer.Value('hook', FHook);
      writer.Value('hookInstance', FHookInstance);
      writer.Value('fhirServer', FfhirServer);
      if (Foauth <> nil) then
      begin
        writer.ValueObject('oauth');
        writer.Value('token', Foauth.Token);
        writer.Value('scope', Foauth.Scopes);
        writer.Value('expires', Foauth.Expires);
        writer.FinishObject;
      end;
      writer.Value('redirect', Fredirect);
      writer.Value('user', Fuser);
      writer.Value('patient', Fpatient);
      writer.Value('encounter', Fencounter);
      writer.ValueArray('context');
      for c in context do
      begin
        comp := TFHIRJsonComposer.create(nil, OutputStyleNormal, THTTPLanguages.create('en'));
        try
          comp.Compose(writer, c);
        finally
          comp.Free;
        end;
      end;
      writer.finishArray;
      if preFetch.Count > 0 then
      begin
        writer.ValueObject('prefetch');
        for s in prefetch.SortedKeys do
        begin
          be := prefetch[s];
          writer.ValueObject(s);
          if (be.response <> nil) then
          begin
            writer.ValueObject('response');
            writer.Value('status', be.response.status);
            writer.FinishObject;
          end;
          if be.resource <> nil then
          begin
            writer.ValueObject('resource');
            comp := TFHIRJsonComposer.create(nil, OutputStyleNormal, THTTPLanguages.create('en'));
            try
              comp.Compose(writer, be.resource);
            finally
              comp.Free;
            end;
            writer.FinishObject;
          end;
          writer.FinishObject;
        end;
        writer.FinishObject;
      end;
      writer.Finish;
    finally
      writer.Free;
    end;
    result := ss.Data;
  finally
    ss.Free;
  end;
  *)
end;

constructor TCDSHookRequest.Create;
begin
  inherited Create;
//  FContext := TFslList<TFHIRResource>.create;
//  FPreFetch := TFslMap<TFhirBundleEntry>.create;
end;

constructor TCDSHookRequest.Create(json: TJsonObject);
(*var
  a : TJsonArray;
  o, e : TJsonObject;
  n : TJsonNode;
  p : TFHIRJsonParser;
  s : String;
  be : TFhirBundleEntry;*)
begin
(*  Create;
  Fencounter := json.str['encounter'];
  FfhirServer := json.str['fhirServer'];
  Fpatient := json.str['patient'];
  FHookInstance := json.str['hookInstance'];
  Fuser := json.str['user'];
  Fredirect := json.str['redirect'];
  FHook := json.str['hook'];
  o := json.obj['oauth'];
  if (o <> nil) then
  begin
    Foauth := TCDSRequestOAuthDetails.Create;
    Foauth.Expires := StrToIntDef(o.str['expires'], 0);
    Foauth.Scopes := o.str['scope'];
    Foauth.Token := o.str['token'];
  end;
  a := json.arr['context'];
  for n in a do
  begin
    p := TFHIRJsonParser.Create(nil, THTTPLanguages.create('en'));
    try
      p.Parse(TJsonObject(n));
      FContext.Add(p.resource.Link as TFhirResource);
    finally
      p.Free;
    end;
  end;
  o := json.obj['prefetch'];
  if (o <> nil) then
  begin
    for s in o.properties.Keys do
    begin
      be := TFhirBundleEntry.Create;
      try
        e := o.properties[s] as TJsonObject;
        if e.has('response') then
        begin
          be.response := TFhirBundleEntryResponse.Create;
          be.response.status := e.obj['response'].str['status'];
        end;
        if e.has('resource') then
        begin
          p := TFHIRJsonParser.Create(nil, THTTPLanguages.create('en'));
          try
            p.Parse(e.obj['resource']);
            be.resource := p.resource.Link as TFhirResource;
          finally
            p.Free;
          end;
        end;
        prefetch.add(s, be.Link);
      finally
        be.Free;
      end;
    end;
  end;
  *)
end;

destructor TCDSHookRequest.Destroy;
begin
  FPreFetch.Free;
  FContext.Free;
  inherited;
end;

function TCDSHookRequest.hash: Integer;
var
  s : String;
begin
  s := AsJson;
  result := HashStringToCode32(s);
end;

function TCDSHookRequest.Link: TCDSHookRequest;
begin
  result := TCDSHookRequest(inherited Link);
end;

function TCDSHookRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FHook.length * sizeof(char)) + 12);
  inc(result, (FHookInstance.length * sizeof(char)) + 12);
  inc(result, (FFhirServer.length * sizeof(char)) + 12);
  inc(result, Foauth.sizeInBytes);
  inc(result, (Fredirect.length * sizeof(char)) + 12);
  inc(result, (Fuser.length * sizeof(char)) + 12);
  inc(result, (Fpatient.length * sizeof(char)) + 12);
  inc(result, (Fencounter.length * sizeof(char)) + 12);
  inc(result, FContext.sizeInBytes);
  inc(result, FPreFetch.sizeInBytes);
  inc(result, (FBaseUrl.length * sizeof(char)) + 12);
end;

{ TCDSHookCardSuggestion }

procedure TCDSHookCardSuggestion.AsJson(json: TJsonWriter);
begin
  json.Value('label', FLabel);
  if (uuid <> '') then
    json.Value('uuid', FUUID);
  if (FCreate <> '') then
    json.Value('create', FCreate);
  if (FDelete <> '') then
    json.Value('delete', FDelete);
end;

constructor TCDSHookCardSuggestion.Create;
begin
  inherited Create;
  // nothing yet
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

constructor TCDSHookCardSuggestion.Create(json: TJsonObject);
begin
  Create;
  FLabel := json.str['label'];
  FUUID := json.str['uuid'];
  FCreate := json.str['create'];
  FDelete := json.str['delete'];
end;

function TCDSHookCardSuggestion.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FLabel.length * sizeof(char)) + 12);
  inc(result, (FUUID.length * sizeof(char)) + 12);
  inc(result, (FCreate.length * sizeof(char)) + 12);
  inc(result, (FDelete.length * sizeof(char)) + 12);
end;

{ TCDSHookCardLink }

procedure TCDSHookCardLink.AsJson(json: TJsonWriter);
begin
  json.Value('label', FLabel);
  if FUrl <> '' then
    json.Value('url', FURL);
  if FType <> '' then
    json.Value('type', FType);
end;

constructor TCDSHookCardLink.Create;
begin
  inherited Create;
  // nothing yet
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

constructor TCDSHookCardLink.Create(json: TJsonObject);
begin
  Create;
  FLabel := json.str['label'];
  FUrl := json.str['url'];
  FType := json.str['type'];
end;

function TCDSHookCardLink.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FLabel.length * sizeof(char)) + 12);
  inc(result, (FUrl.length * sizeof(char)) + 12);
  inc(result, (FType.length * sizeof(char)) + 12);
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

procedure TCDSHookCard.AsJson(json: TJsonWriter);
var
  link : TCDSHookCardLink;
  suggestion : TCDSHookCardSuggestion;
begin
  json.Value('summary', Fsummary);
  if Findicator <> '' then
    json.Value('indicator', Findicator);
  if Fdetail <> '' then
    json.Value('detail', Fdetail);
  if (sourceLabel <> '') then
  begin
    json.ValueObject('source');
    json.Value('label', FsourceLabel);
    json.Value('url', FSourceURL);
    json.FinishObject;
  end;
  if FLinks.Count > 0 then
  begin
    json.ValueArray('links');
    for link in FLinks do
      link.AsJson(json);
    json.FinishArray;
  end;
  if FSuggestions.Count > 0 then
  begin
    json.ValueArray('suggestions');
    for suggestion in FSuggestions do
      suggestion.AsJson(json);
    json.FinishArray;
  end;
end;

constructor TCDSHookCard.Create;
begin
  inherited Create;
  FSuggestions := TFslList<TCDSHookCardSuggestion>.create;
  FLinks := TFslList<TCDSHookCardLink>.create;
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

constructor TCDSHookCard.Create(json: TJsonObject);
var
  p : TJsonObject;
  n : TJsonNode;
begin
  Create;
  Fsummary := json.str['summary'];
  Findicator := json.str['indicator'];
  Fdetail := json.str['detail'];
  p := json.obj['source'];
  if (p <> nil) then
  begin
    FsourceLabel := p.str['label'];
    FSourceURL := p.str['url'];
  end;
  for n in json.forceArr['suggestions'] do
    FSuggestions.Add(TCDSHookCardSuggestion.Create(TJsonObject(n)));
  for n in json.forceArr['links'] do
    FLinks.Add(TCDSHookCardLink.Create(TJsonObject(n)));
end;



function TCDSHookCard.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSourceURL.length * sizeof(char)) + 12);
  inc(result, (Fdetail.length * sizeof(char)) + 12);
  inc(result, (FsourceLabel.length * sizeof(char)) + 12);
  inc(result, (Fsummary.length * sizeof(char)) + 12);
  inc(result, (Findicator.length * sizeof(char)) + 12);
  inc(result, FSuggestions.sizeInBytes);
  inc(result, FLinks.sizeInBytes);
end;

{ TCDSHookResponse }

function TCDSHookResponse.addCard: TCDSHookCard;
begin
  result := TCDSHookCard.Create;
  cards.Add(result);
end;

function TCDSHookResponse.addDecision: TCDSHookDecision;
begin
  result := TCDSHookDecision.Create;
  decisions.add(result);
end;

function TCDSHookResponse.AsJson: String;
var
  ss : TFslStringStream;
  writer : TJSONWriter;
//  c : TFhirResource;
//  comp : TFHIRJsonComposer;
  card : TCDSHookCard;
begin
  ss := TFslStringStream.Create;
  try
    writer := TJsonWriterDirect.create;
    try
      writer.HasWhitespace := true;
      writer.Stream := ss.Link;
      writer.Start(true);
      writer.ValueArray('cards');
      for card in FCards do
      begin
        writer.ValueObject;
        card.AsJson(writer);
        writer.FinishObject;
      end;
      writer.FinishArray;
      writer.Finish(true);
    finally
      writer.Free;
    end;
    result := string(ss.Data);
  finally
    ss.Free;
  end;
end;


constructor TCDSHookResponse.Create(json: TJsonObject);
var
  a : TJsonArray;
  n : TJsonNode;
begin
  Create;
  a := json.arr['cards'];
  if a <> nil then
    for n in a do
      cards.Add(TCDSHookCard.Create(TJsonObject(n)));
  a := json.arr['decisions'];
  if a <> nil then
    for n in a do
      decisions.Add(TCDSHookDecision.Create(TJsonObject(n)));
end;

constructor TCDSHookResponse.Create;
begin
  inherited Create;
  FCards := TFslList<TCDSHookCard>.create;
  FDecisions := TFslList<TCDSHookDecision>.create;
end;

destructor TCDSHookResponse.Destroy;
begin
  FCards.Free;
  FDecisions.Free;
  inherited;
end;

function TCDSHookResponse.Link: TCDSHookResponse;
begin
  result := TCDSHookResponse(inherited Link);
end;




function TCDSHookResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCards.sizeInBytes);
  inc(result, FDecisions.sizeInBytes);
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

class function TCDSHooks.allHooks: TStringList;
begin
  result := TStringList.create;
  result.Add(codeView);
  result.Add(identifierView);
  result.Add(patientView);
end;

class function TCDSHooks.identifierView: String;
begin
  result := 'identifier-view';
end;

class function TCDSHooks.isIdentifierView(c: String): boolean;
begin
  result := (c = 'identifier-view');
end;

class function TCDSHooks.isKnownHook(c: String): boolean;
begin
  result := isPatientView(c) or isCodeView(c) or isIdentifierView(c);
end;

class function TCDSHooks.patientView: String;
begin
  result := 'patient-view';
end;

class function TCDSHooks.isPatientView(c: String): boolean;
begin
  result := (c = 'patient-view');
end;

class function TCDSHooks.codeView: String;
begin
  result := 'code-view';
end;

class function TCDSHooks.isCodeView(c: String): boolean;
begin
  result := (c = 'code-view');
end;

{ TCDSHookDecision }

procedure TCDSHookDecision.AsJson(json: TJsonWriter);
begin

end;

constructor TCDSHookDecision.Create;
begin
  inherited Create;
  FCreate := TStringList.create;
  FDelete := TStringList.create;
end;

constructor TCDSHookDecision.Create(json: TJsonObject);
var
  n : TJsonNode;
begin
  Create;
  if json.has('create') then
    for n in json.arr['create'] do
      FCreate.add(TJsonString(n).value);
  if json.has('delete') then
    for n in json.arr['delete'] do
      FDelete.add(TJsonString(n).value);
end;

destructor TCDSHookDecision.Destroy;
begin
  FCreate.Free;
  FDelete.Free;
  inherited;
end;

function TCDSHookDecision.Link: TCDSHookDecision;
begin
  result := TCDSHookDecision(inherited Link);
end;

function TCDSHookDecision.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCreate.sizeInBytes);
  inc(result, FDelete.sizeInBytes);
end;

{ TCDSHooksManager }

constructor TCDSHooksManager.Create;
begin
  inherited;
  FServers := TFslList<TCDSHooksManagerServerInfo>.create;
  FLock := TFslLock.Create('TCDSHooksManager');
  FThreads := TFslList<TCDSHooksManagerWorkThread>.create;
  FCache := TFslMap<TCDSHooksManagerCachedResponse>.create('CDS Hooks Cache');
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

function TCDSHooksManager.waiting: boolean;
var
  thread : TCDSHooksManagerWorkThread;
begin
  result := false;
  FLock.Lock;
  try
    for thread in FThreads do
      if thread.FAlive then
      begin
        result := true;
        break;
      end;
  finally
    FLock.Unlock;
  end;
end;

procedure TCDSHooksManager.checkConnectServer(server: TCDSHooksManagerServerInfo);
begin
  if server.info.SmartAppLaunchMode <> salmOAuthClient then
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

procedure TCDSHooksManager.connectToServer(details: TRegisteredFHIRServer; token : TClientAccessToken);
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
  hook : TRegisteredCDSHook;
begin
  hash := inttostr(request.hash);
  for server in FServers do
  begin
    if server.info.doesHook(request.hook, hook) then
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
              work.id := hook.name;
              work.request := request.Link;
              work.server := server.info.Link;
              work.token := server.token.link; // do this here to avoid threading problems
              work.event := event;
              work.context := context;
              work.hash := hash;
              FThreads.add(work.link);
              work.Start;
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

function TCDSHooksManager.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FServers.sizeInBytes);
  inc(result, FThreads.sizeInBytes);
  inc(result, FCache.sizeInBytes);
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
  result := FinUse and ((info.SmartAppLaunchMode = salmNone) or (token <> nil));
end;

procedure TCDSHooksManagerServerInfo.Setinfo(const Value: TRegisteredFHIRServer);
begin
  FInfo.Free;
  Finfo := Value;
end;

procedure TCDSHooksManagerServerInfo.SetToken(const Value: TClientAccessToken);
begin
  FToken.Free;
  FToken := Value;
end;

function TCDSHooksManagerServerInfo.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Finfo.sizeInBytes);
  inc(result, FToken.sizeInBytes);
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
//var
//  body, rep : TFslBuffer;
//  resp : TCDSHookResponse;
//  client : TFhirClient;
begin
(*  SetThreadName('CDSHooks manager');t
try
  {$IFDEF WINDOWS}
  CoInitialize(nil); // though there's no reason internal reason to initialize
  {$ENDIF}
  try
    try
      try
        try
          client := TFhirClients.makeHTTP(nil, server.fhirEndpoint, true, 15000);
          try
            client.smartToken := token.link;
            body := makeBody;
            try
              rep := client.customPost(UrlPath(['cds-services', id]), FHeaders, body);
              try
                resp := readBody(rep);
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
                rep.Free;
              end;
            finally
              body.free;
            end;
          finally
            client.Free;
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
  finally
    // todo: detach from Java...
    {$IFDEF WINDOWS}
    CoUninitialize;
    {$ENDIF}
  end;
  closeThread();\*)
end;

function TCDSHooksManagerWorkThread.Link: TCDSHooksManagerWorkThread;
begin
  result := TCDSHooksManagerWorkThread(inherited Link);
end;

function TCDSHooksManagerWorkThread.makeBody: TFslBuffer;
begin
  result := TFslBuffer.Create;
  try
    result.AsBytes := TEncoding.UTF8.GetBytes(FRequest.AsJson);
    result.Link;
  finally
    result.Free;
  end;
end;

function TCDSHooksManagerWorkThread.readBody(body: TFslBuffer): TCDSHookResponse;
var
  json : TJsonObject;
begin
  json := TJSONParser.Parse(body.AsBytes);
  try
    result := TCDSHookResponse.Create(json);
  finally
    json.Free;
  end;
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

procedure TCDSHooksManagerWorkThread.SetToken(const Value: TClientAccessToken);
begin
  FToken.Free;
  FToken := Value;
end;

function TCDSHooksManagerWorkThread.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fmanager.sizeInBytes);
  inc(result, Frequest.sizeInBytes);
  inc(result, Fserver.sizeInBytes);
  inc(result, FToken.sizeInBytes);
  inc(result, (FHash.length * sizeof(char)) + 12);
  inc(result, (FID.length * sizeof(char)) + 12);
  inc(result, FHeaders.sizeInBytes);
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


function TCDSHooksManagerCachedResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FResponse.sizeInBytes);
  inc(result, (FError.length * sizeof(char)) + 12);
end;

function TCDSRequestOAuthDetails.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FScopes.length * sizeof(char)) + 12);
  inc(result, (FToken.length * sizeof(char)) + 12);
end;

end.
