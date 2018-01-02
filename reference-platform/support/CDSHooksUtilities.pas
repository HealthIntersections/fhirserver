unit CDSHooksUtilities;

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
  SysUtils, Classes,
  TextUtilities, MarkDownProcessor, KCritSct, HashSupport, EncodeSupport,
  AdvObjects, AdvGenerics, AdvJson, AdvStringStreams,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, SmartOnFhirUtilities, FHIRParserBase, FHIRParser;

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

  TCDSRequestOAuthDetails = class (TAdvObject)
  private
    FExpires : integer;
    FScopes : string;
    FToken : string;
  public
    Property Expires : integer read FExpires write FExpires;
    Property Scopes : string read FScopes write FScopes;
    Property Token : string read FToken write FToken;
  end;

  TCDSHookRequest = class (TAdvObject)
  private
    FHook : String;
    FHookInstance : String;
    FFhirServer: String;
    Foauth: TCDSRequestOAuthDetails;
    Fredirect: String;
    Fuser: String;
    Fpatient: String;
    Fencounter: String;
    FLang : String;
    FContext: TAdvList<TFHIRResource>;
    FPreFetch : TAdvMap<TFhirBundleEntry>;
    FBaseUrl: String;

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
    property context : TAdvList<TFHIRResource> read FContext;
    property preFetch : TAdvMap<TFhirBundleEntry> read FPreFetch;
    property lang : String read FLang write FLang;
    property baseURL : String read FBaseUrl write FBaseUrl;
  end;

  TCDSHookCardSuggestion = class (TAdvObject)
  private
    FLabel: String;
    FUUID: String;
    FCreate: String;
    FDelete: String;
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

  TCDSHookCardLink = class (TAdvObject)
  private
    FLabel: String;
    FUrl: String;
    FType: String;
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
    property suggestions : TAdvList<TCDSHookCardSuggestion> read FSuggestions;
    property links : TAdvList<TCDSHookCardLink> read FLinks;

    procedure addLink(label_, uri : String);
  end;

  TCDSHookDecision = class (TAdvObject)
  private
    FCreate: TStringList;
    FDelete: TStringList;
  public
    constructor Create; Overload; Override;
    constructor Create(json : TJsonObject); Overload;
    destructor Destroy; override;

    function Link : TCDSHookDecision; overload;
    procedure AsJson(json : TJsonWriter);

    property Create_ : TStringList read FCreate;
    property Delete : TStringList read FDelete;
  end;

  TCDSHookResponse = class (TAdvObject)
  private
    FCards: TAdvList<TCDSHookCard>;
    FDecisions : TAdvList<TCDSHookDecision>;
  public
    constructor Create; Overload; Override;
    constructor Create(json : TJsonObject); Overload;
    destructor Destroy; override;

    function Link : TCDSHookResponse; overload;
    function AsJson : String;

    property cards : TAdvList<TCDSHookCard> read FCards;
    function addCard : TCDSHookCard;
    property decisions : TAdvList<TCDSHookDecision> read FDecisions;
    function addDecision : TCDSHookDecision;
  end;

  TCDSHookCache = class (TAdvObject)
  public
    function has(request : TCDSHookRequest; var response : TCDSHookResponse) : boolean;
    procedure see(reuqest : TCDSHookRequest; response : TCDSHookResponse);
  end;

function presentAsHtml(cards : TAdvList<TCDSHookCard>; inprogress, errors : TStringList) : String;

implementation

uses
  FHIRClient;

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
var
  ss : TAdvStringStream;
  writer : TJSONWriter;
  c : TFhirResource;
  comp : TFHIRJsonComposer;
  s : String;
  be : TFhirBundleEntry;
begin
  ss := TAdvStringStream.Create;
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
        comp := TFHIRJsonComposer.create(nil, OutputStyleNormal, 'en');
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
            comp := TFHIRJsonComposer.create(nil, OutputStyleNormal, 'en');
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
end;

constructor TCDSHookRequest.Create;
begin
  inherited Create;
  FContext := TAdvList<TFHIRResource>.create;
  FPreFetch := TAdvMap<TFhirBundleEntry>.create;
end;

constructor TCDSHookRequest.Create(json: TJsonObject);
var
  a : TJsonArray;
  o, e : TJsonObject;
  n : TJsonNode;
  p : TFHIRJsonParser;
  s : String;
  be : TFhirBundleEntry;
begin
  Create;
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
    p := TFHIRJsonParser.Create(nil, 'en');
    try
      p.Parse(TJsonObject(n));
      FContext.Add(p.resource.Link);
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
          p := TFHIRJsonParser.Create(nil, 'en');
          try
            p.Parse(e.obj['resource']);
            be.resource := p.resource.Link;
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
  FSuggestions := TAdvList<TCDSHookCardSuggestion>.create;
  FLinks := TAdvList<TCDSHookCardLink>.create;
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
  ss : TAdvStringStream;
  writer : TJSONWriter;
//  c : TFhirResource;
//  comp : TFHIRJsonComposer;
  card : TCDSHookCard;
begin
  ss := TAdvStringStream.Create;
  try
    writer := TJsonWriterDirect.create;
    try
      writer.HasWhitespace := true;
      writer.Stream := ss.Link;
      writer.Start;
      writer.ValueArray('cards');
      for card in FCards do
      begin
        writer.ValueObject;
        card.AsJson(writer);
        writer.FinishObject;
      end;
      writer.FinishArray;
      writer.Finish;
    finally
      writer.Free;
    end;
    result := ss.Data;
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
  FCards := TAdvList<TCDSHookCard>.create;
  FDecisions := TAdvList<TCDSHookDecision>.create;
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


end.
