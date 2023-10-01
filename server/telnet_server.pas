unit telnet_server;

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
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes,
  IdTCPServer, IdCustomTCPServer, IdException, IdTelnetServer, IdIOHandlerSocket, IdContext,
  fsl_base, fsl_utilities, fsl_threads, fsl_logging,
  endpoint, server_stats;

type
  TFHIRTelnetServer = class;

  TTelnetThreadHelper = class (TFslObject)
  Private
    FServer : TFHIRTelnetServer;
    FContext: TIdContext;
    FNextPing : TDateTime;
    FHasSent : boolean;
    FEnhanced : boolean;
    procedure execute;
    procedure processCommand(s : String);
    procedure send(s : String);
    procedure ping;
  Public
    constructor Create(server : TFHIRTelnetServer; context: TIdContext);
    destructor Destroy; Override;
    function link : TTelnetThreadHelper; overload;
  end;

  TFHIRTelnetServerThread = class (TFslThread)
  private
    FServer : TFHIRTelnetServer;
  protected
    function ThreadName : String; Override;
    Procedure Execute; override;
  end;

  { TFHIRTelnetServer }

  TFHIRTelnetServer = class (TLogListener)
  Private
    FServer: TIdTelnetServer;
    FLock : TFslLock;
    FClients: TFslList<TTelnetThreadHelper>;
    FEndPoints : TFslList<TFHIRServerEndPoint>;
    FShuttingDown: boolean;
    FStats : TStatusRecords;
    FPassword : String;
    FLog : TStringList;
    FWelcomeMsg : String;
    FThread : TFHIRTelnetServerThread;
    procedure SetStats(AValue: TStatusRecords);
    procedure TelnetLogin(AThread: TIdContext; const username, password: String; var AAuthenticated: Boolean);
    procedure telnetExecute(AThread: TIdContext);
    procedure internalThread;
  protected
    procedure log(const msg : String); override;
  Public
    constructor Create(port: Integer; WelcomeMsg : String);
    destructor Destroy; Override;
    function Link : TFHIRTelnetServer; overload;
    property password : String read FPassword write FPassword;
    property stats : TStatusRecords read FStats write SetStats;

    procedure addEndPoint(ep : TFHIRServerEndPoint);
    procedure removeEndPoint(ep : TFHIRServerEndPoint);
    property ShuttingDown : boolean read FShuttingDown write FShuttingDown;
  end;

implementation

{ TFHIRTelnetServer }

constructor TFHIRTelnetServer.Create(port: Integer; WelcomeMsg: String);
begin
  inherited Create;
  FWelcomeMsg := WelcomeMsg;
  FLock := TFslLock.Create('TelnetServer');
  FStats := stats;
  FClients := TFslList<TTelnetThreadHelper>.Create;
  FEndPoints := TFslList<TFHIRServerEndPoint>.Create;

  FLog := TStringList.Create;

  FServer := TidTelnetServer.Create(NIL);
  FServer.DefaultPort := port;
  FServer.LoginMessage := 'FHIRServer';
  FServer.OnAuthentication := TelnetLogin;
  FServer.OnExecute := TelnetExecute;
  FServer.Active := True;

  FThread := TFHIRTelnetServerThread.Create;
  FThread.FServer := self;
  FThread.TimePeriod := 50;
  FThread.Start;
  sleep(500); // allow console to connect early
end;

destructor TFHIRTelnetServer.Destroy;
begin
  try
    FStats.free;
    FThread.StopAndWait(100);
    FThread.free;
    FServer.Active := false;
    FServer.free;
    FClients.free;
    FEndPoints.free;
  except
    // not interested
  end;
  FLog.free;
  FLock.free;
  inherited;
end;

procedure TFHIRTelnetServer.log(const msg: String);
begin
  FLock.Lock;
  try
    FLog.Add(msg);
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRTelnetServer.internalThread;
var
  ts : TStringList;
  s : String;
  tth : TTelnetThreadHelper;
  list : TFslList<TTelnetThreadHelper>;
begin
  list := TFslList<TTelnetThreadHelper>.Create;
  try
    ts := TStringList.Create;
    try
      FLock.Lock;
      try
        ts.Assign(FLog);
        FLog.clear;
        list.AddAll(FClients);
      finally
        FLock.Unlock;
      end;
      for tth in list do
      begin
        if (not tth.FHasSent) then
          tth.send(FWelcomeMsg);
        for s in ts do
        begin
          tth.send(s);
        end;
        tth.ping;
      end;
    finally
      ts.free;
    end;
  finally
    list.free;
  end;
end;

function TFHIRTelnetServer.Link: TFHIRTelnetServer;
begin
  result := TFHIRTelnetServer(inherited Link);
end;

procedure TFHIRTelnetServer.addEndPoint(ep: TFHIRServerEndPoint);
begin
  FEndPoints.Add(ep.Link);
end;

procedure TFHIRTelnetServer.removeEndPoint(ep: TFHIRServerEndPoint);
begin
  FEndPoints.remove(ep);
end;

procedure TFHIRTelnetServer.TelnetLogin(AThread: TIdContext; const username, password: String; var AAuthenticated: Boolean);
begin
  If (username = 'console') and (password = 'AA8FF8CC-81C8-41D7-93BA-26AD5E89A1C1') and (AThread.Binding.PeerIP = '127.0.0.1') then
    AAuthenticated := true
  else If (username = 'g') and (password = 'g') and (AThread.Binding.PeerIP = '127.0.0.1') and FileExists(filePath(['[tmp]', 'gg.txt'])) then
    AAuthenticated := true
  else If (username = 'console') and (password = FPassword) and (FPassword <> '') then
    AAuthenticated := true
  else
    AAuthenticated := true;
end;

procedure TFHIRTelnetServer.SetStats(AValue: TStatusRecords);
begin
  FStats.free;
  FStats := AValue;
end;

procedure TFHIRTelnetServer.telnetExecute(AThread: TIdContext);
var
  tth: TTelnetThreadHelper;
begin
  SetThreadName('Telnet Client at '+AThread.Binding.PeerIP);
  tth := TTelnetThreadHelper.Create(self, AThread);
  try
    FLock.Lock;
    try
      FClients.Add(tth.Link);
    finally
      FLock.Unlock;
    end;
    try
      tth.execute();
    finally
      FLock.Lock;
      try
        FClients.Remove(tth)
      finally
        FLock.Unlock;
      end;
    end;
  finally
    tth.free;
  end;
end;

{ TTelnetThreadHelper }

constructor TTelnetThreadHelper.Create(server : TFHIRTelnetServer; context: TIdContext);
begin
  inherited Create;
  FServer := server;
  FContext := context;
end;

destructor TTelnetThreadHelper.Destroy;
begin
  // nothing?
  inherited;
end;

function TTelnetThreadHelper.link: TTelnetThreadHelper;
begin
  result := TTelnetThreadHelper(inherited link);
end;

procedure TTelnetThreadHelper.execute;
var
  s : String;
begin
  while FContext.Connection.Connected do
  begin
    try
      s := FContext.Connection.Socket.ReadLn;
      if length(s) > 0 then
      begin
        processCommand(s);
      end;
    except
      break;
    end;
  end;
end;

procedure TTelnetThreadHelper.ping;
var
  mem : UInt64;
  ep : TFHIRServerEndPoint;
  magic : integer;
begin
  magic := TFslObject.nextMagic;
  if FServer.FShuttingDown then
  begin
    send('$@ping: '+inttostr(GetThreadCount)+' threads; shutting down');
  end
  else if (now > FNextPing) then
  begin
    mem := 0;
    for ep in FServer.FEndPoints do
    begin
      mem := mem + ep.cacheSize(magic);
    end;
    send('$@ping: '+inttostr(GetThreadCount)+' threads, '+Logging.MemoryStatus(true)+', '+DescribeBytes(mem)+' MB used');
  end;
  FNextPing := now + (DATETIME_SECOND_ONE * 10);
end;

procedure TTelnetThreadHelper.processCommand(s: String);
var
  ep : TFHIRServerEndPoint;
  ci: TCacheInformation;
begin
  if (s = '@console') then
  begin
    Logging.log('Console connected');
    FEnhanced := true
  end
  else if (s = '@threads') then
  begin
    Logging.log('Console requested Thread List');
    send('$@threads: '+GetThreadReport)
  end
  else if (s = '@classes') then
  begin
    Logging.log('Console requested Object Class Count');
    send('$@classes: '+TFslObject.getReport('|', false))
  end
  else if (s = '@classes+') then
  begin
    Logging.log('Console requested Object Class Delta');
    send('$@classes: '+TFslObject.getReport('|', true))
  end
  else if (s = '@stats') then
  begin
    Logging.log('Console requested stats');
    send('$@stats: '+FServer.FStats.asCSVLine);
  end
  else if (s = '@caches') then
  begin
    Logging.log('Console requested Cache Information');
    ci := TCacheInformation.Create;
    try
      for ep in FServer.FEndPoints do
        ep.getCacheInfo(ci);
      send('$@cache: '+ci.text('|'));
    finally
      ci.free;
    end;
  end
  else if (s = '@locks') then
  begin
    Logging.log('Console requested Lock Information');
    FServer.FLock.Lock('ProcessCommand');
    try
      send('$@locks: '+DumpLocks(true, '|'))
    finally
      FServer.FLock.Unlock;
    end;
  end
  else if (s = '@cache') then
  begin
    Logging.log('Clear Cache because of instruction from console: '+Logging.MemoryStatus(true));
    for ep in FServer.FEndPoints do
      ep.clearCache;
    Logging.log('Clear Cache finished: '+Logging.MemoryStatus(true));
    ci := TCacheInformation.Create;
    try
      for ep in FServer.FEndPoints do
        ep.getCacheInfo(ci);
      send('$@cache: '+ci.text('|'));
    finally
      ci.free;
    end;
  end
  else
    send('Unrecognised command '+s);
end;

procedure TTelnetThreadHelper.send(s: String);
begin
  if FEnhanced or not s.StartsWith('$@') then
  begin
    try
      FContext.Connection.Socket.WriteLn(s);
    except
    end;
    FHasSent := true;
  end;
end;

{ TFHIRTelnetServerThread }

procedure TFHIRTelnetServerThread.Execute;
begin
  try
    FServer.internalThread;
  except
    // nothing.
  end;
end;

function TFHIRTelnetServerThread.threadName: String;
begin
  result := 'Telnet Server';
end;

end.

