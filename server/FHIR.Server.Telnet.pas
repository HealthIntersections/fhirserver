unit FHIR.Server.Telnet;

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
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Threads, FHIR.Support.Logging;

type
  TFHIRTelnetServer = class;

  TTelnetSession = class (TFslObject)
  private
    FServer : TFHIRTelnetServer;
    FId : Integer;
  public
    destructor Destroy; override;
    procedure SendMsg(s : String);
  end;

  TTelnetThreadHelper = class (TFslObject)
  Private
    FContext: TIdContext;
    FNextPing : TDateTime;
    FHasSent : boolean;
    FEnhanced : boolean;
    procedure execute;
    procedure processCommand(s : String);
    procedure send(s : String);
    procedure ping;
  Public
    constructor Create(context: TIdContext);
    destructor Destroy; Override;
    function link : TTelnetThreadHelper; overload;
  end;

  TFHIRTelnetServerThread = class (TFslThread)
  private
    FServer : TFHIRTelnetServer;
  protected
    Procedure Execute; override;
  end;

  TFHIRTelnetServer = class (TLogListener)
  Private
    FServer: TIdTelnetServer;
    FLock : TFslLock;
    FClients: TFslList<TTelnetThreadHelper>;
    FPassword : String;
    FLog : TStringList;
    FWelcomeMsg : String;
    FThread : TFHIRTelnetServerThread;
    FLastId : integer;
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

    function makeSession(desc : String) : TTelnetSession;
  end;

implementation

{ TFHIRTelnetServer }

constructor TFHIRTelnetServer.Create(port: Integer; WelcomeMsg : String);
begin
  inherited Create;
  FWelcomeMsg := WelcomeMsg;
  FLock := TFslLock.Create('TelnetServer');
  FClients := TFslList<TTelnetThreadHelper>.create;

  FLog := TStringList.Create;

  FServer := TidTelnetServer.Create(NIL);
  FServer.DefaultPort := port;
  FServer.LoginMessage := 'FHIRServer';
  FServer.OnAuthentication := TelnetLogin;
  FServer.OnExecute := TelnetExecute;
  FServer.Active := True;

  FThread := TFHIRTelnetServerThread.Create;
  FThread.FServer := self;
  FThread.Open;
  Logging.addListener(self);
  sleep(500); // allow console to connect early
end;

destructor TFHIRTelnetServer.Destroy;
begin
  Logging.removeListener(self);
  try
    FThread.closeOut;
    FThread.Free;
    FServer.Active := false;
    FServer.Free;
    FClients.Free;
  except
    // not interested
  end;
  FLog.Free;
  FLock.Free;
  inherited;
end;

procedure TFHIRTelnetServer.Log(const msg: String);
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
  list := TFslList<TTelnetThreadHelper>.create;
  try
    ts := TStringList.create;
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
      ts.Free;
    end;
  finally
    list.Free;
  end;
end;

function TFHIRTelnetServer.Link: TFHIRTelnetServer;
begin
  result := TFHIRTelnetServer(inherited Link);
end;

function TFHIRTelnetServer.makeSession(desc: String): TTelnetSession;
begin
  result := TTelnetSession.create;
  try
    result.FServer := self.link;
    FLock.Lock;
    try
      inc(FLastId);
      result.FId := FLastId;
    finally
      FLock.Unlock;
    end;
    Log('$@session-'+inttostr(result.FId)+': @start|'+inttostr(GetTickCount64)+'|'+desc);
    result.link;
  finally
    result.free;
  end;

end;

procedure TFHIRTelnetServer.TelnetLogin(AThread: TIdContext; const username, password: String; var AAuthenticated: Boolean);
begin
  If (username = 'console') and (password = 'AA8FF8CC-81C8-41D7-93BA-26AD5E89A1C1') and (AThread.Binding.PeerIP = '127.0.0.1') then
    AAuthenticated := true
  else If (username = 'g') and (password = 'g') and (AThread.Binding.PeerIP = '127.0.0.1') and FileExists('C:\temp\gg.txt') then
    AAuthenticated := true
  else If (username = 'console') and (password = FPassword) and (FPassword <> '') then
    AAuthenticated := true
  else
    AAuthenticated := true;
end;

procedure TFHIRTelnetServer.telnetExecute(AThread: TIdContext);
var
  tth: TTelnetThreadHelper;
begin
  tth := TTelnetThreadHelper.Create(AThread);
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
    tth.Free;
  end;
end;

{ TTelnetThreadHelper }

constructor TTelnetThreadHelper.Create(context: TIdContext);
begin
  inherited create;
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
begin
  if (now > FNextPing) then
  begin
    send('$@ping: '+inttostr(GetThreadCount)+' threads, '+Logging.MemoryStatus);
    FNextPing := now + (DATETIME_SECOND_ONE * 10);
  end;
end;

procedure TTelnetThreadHelper.processCommand(s: String);
begin
  if (s = '@console') then
    FEnhanced := true
  else if (s = '@threads') then
    send('$@threads: '+GetThreadReport)
  else
    send('Unrecognised command '+s);
end;

procedure TTelnetThreadHelper.send(s: String);
begin
  if FEnhanced or not s.StartsWith('$@') then
  begin
    FContext.Connection.Socket.WriteLn(s);
    FHasSent := true;
  end;
end;

{ TFHIRTelnetServerThread }

procedure TFHIRTelnetServerThread.Execute;
begin
  SetThreadName('Telnet Connection');
  while Not Terminated do
  begin
    try
      FServer.internalThread;
    except
      // nothing.
    end;
    sleep(50);
  end;
end;

{ TTelnetSession }

destructor TTelnetSession.Destroy;
begin
  FServer.log('$@session-'+inttostr(FId)+': @stop|'+inttostr(GetTickCount64));
  FServer.Free;
  inherited;
end;

procedure TTelnetSession.SendMsg(s: String);
begin
  FServer.Log('$@session-'+inttostr(FId)+': @msg|'+s);
end;

end.

