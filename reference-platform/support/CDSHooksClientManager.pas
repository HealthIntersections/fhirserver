unit CDSHooksClientManager;


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
  {$IFDEF MSWINDOWS} Windows, ActiveX, {$ENDIF}
  SysUtils, Classes,
  TextUtilities, MarkDownProcessor, KCritSct, HashSupport, EncodeSupport,
  AdvObjects, AdvGenerics, AdvThreads, AdvJson, AdvStringStreams,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, SmartOnFhirUtilities, FHIRParser,
  CDSHooksUtilities;

type
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
    FAlive : boolean;
    FID: String;
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

    property id : String read FID write FID;
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
          b.Append('<a href="'+makeUrlSafe(l.Url)+'">'+FormatTextToHTML(l.label_)+'</a>');
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
  result := FinUse and ((info.SmartAppLaunchMode = salmNone) or (token <> nil));
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
  resp : TCDSHookResponse;
  client : TFhirHTTPClient;
begin
  {$IFDEF MSWINDOWS}
  CoInitialize(nil);
  {$ENDIF}
  try
    try
      try
        try
          client := TFhirHTTPClient.Create(nil, server.fhirEndpoint, true);
          try
            client.timeout := 15000;
            {$IFNDEF FHIR2}
            client.allowR2 := true;
            {$ENDIF}
            client.smartToken := token.link;
            resp := client.cdshook(FID, FRequest);
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
    {$IFDEF MSWINDOWS}
    CoUninitialize;
    {$ENDIF}
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
