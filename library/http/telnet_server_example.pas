program TelnetServerExample;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  telnet_server;

type
  TMyApplication = class
  private
    FTelnetServer: TTelnetServer;
    FLogMessages: TStringList;
    FUserDatabase: TStringList; // Simple username:password pairs
    
    function OnAuthenticate(const Username, Password: string): Boolean;
    function OnCommand(Client: TTelnetClient; const Command: string): string;
    procedure OnClientConnect(Client: TTelnetClient);
    procedure OnClientDisconnect(Client: TTelnetClient);
    
    procedure SetupUserDatabase;
    procedure SimulateLogMessages;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Run;
    procedure Stop;
    
    // Public methods to send log messages (thread-safe)
    procedure LogMessage(const Message: string);
    procedure LogToUser(const Username, Message: string);
  end;

{ TMyApplication }

constructor TMyApplication.Create;
begin
  inherited Create;
  
  FTelnetServer := TTelnetServer.Create;
  FLogMessages := TStringList.Create;
  FUserDatabase := TStringList.Create;
  
  // Set up event handlers
  FTelnetServer.OnAuthenticate := OnAuthenticate;
  FTelnetServer.OnCommand := OnCommand;
  FTelnetServer.OnClientConnect := OnClientConnect;
  FTelnetServer.OnClientDisconnect := OnClientDisconnect;
  
  // Configure server settings
  FTelnetServer.MaxClients := 5;
  FTelnetServer.AuthTimeout := 30;
  FTelnetServer.IdleTimeout := 1800; // 30 minutes
  
  SetupUserDatabase;
end;

destructor TMyApplication.Destroy;
begin
  Stop;
  FTelnetServer.Free;
  FLogMessages.Free;
  FUserDatabase.Free;
  inherited Destroy;
end;

procedure TMyApplication.SetupUserDatabase;
begin
  // Simple username:password database
  FUserDatabase.Add('admin:password123');
  FUserDatabase.Add('user1:secret');
  FUserDatabase.Add('monitor:watch');
  FUserDatabase.Add('guest:guest');
end;

function TMyApplication.OnAuthenticate(const Username, Password: string): Boolean;
var
  i: Integer;
  UserPass: string;
begin
  Result := False;
  
  // Check against simple user database
  UserPass := Username + ':' + Password;
  for i := 0 to FUserDatabase.Count - 1 do
  begin
    if SameText(FUserDatabase[i], UserPass) then
    begin
      Result := True;
      WriteLn(Format('User authenticated: %s', [Username]));
      Exit;
    end;
  end;
  
  WriteLn(Format('Authentication failed for user: %s', [Username]));
end;

function TMyApplication.OnCommand(Client: TTelnetClient; const Command: string): string;
var
  Cmd: string;
  ClientList: TStringList;
  i: Integer;
begin
  Cmd := LowerCase(Trim(Command));
  
  WriteLn(Format('Command from %s: %s', [Client.Username, Command]));
  
  // Handle various commands
  if Cmd = 'status' then
  begin
    Result := Format('Server Status: %d clients connected, Server uptime: running', 
      [FTelnetServer.ClientCount]);
  end
  else if Cmd = 'time' then
  begin
    Result := 'Current server time: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  end
  else if Cmd = 'uptime' then
  begin
    Result := 'Server has been running since startup';
  end
  else if Cmd = 'memory' then
  begin
    Result := 'Memory usage information not available';
  end
  else if Cmd.StartsWith('echo ') then
  begin
    Result := 'Echo: ' + Copy(Command, 6, Length(Command));
  end
  else if Cmd = 'version' then
  begin
    Result := 'Telnet Server Example v1.0';
  end
  else if Cmd = 'whoami' then
  begin
    Result := Format('You are logged in as: %s from %s', [Client.Username, Client.ClientIP]);
  end
  else if Cmd = 'broadcast' then
  begin
    // Special command that requires admin privileges
    if SameText(Client.Username, 'admin') then
    begin
      LogMessage(Format('Admin %s initiated a test broadcast', [Client.Username]));
      Result := 'Test broadcast sent to all clients';
    end
    else
      Result := 'Access denied: Admin privileges required';
  end
  else
  begin
    Result := Format('Unknown command: %s. Type "help" for available commands.', [Command]);
  end;
end;

procedure TMyApplication.OnClientConnect(Client: TTelnetClient);
begin
  WriteLn(Format('Client connected: %s from %s', [Client.Username, Client.ClientIP]));
  
  // Send a welcome message to all other clients
  LogMessage(Format('User %s has connected', [Client.Username]));
  
  // Send recent log messages to the new client
  // (In a real application, you might want to send last N messages)
  Client.SendLogMessage('Connected to server. Welcome!');
end;

procedure TMyApplication.OnClientDisconnect(Client: TTelnetClient);
begin
  WriteLn(Format('Client disconnected: %s', [Client.Username]));
  
  // Notify other clients
  if Client.Username <> '' then
    LogMessage(Format('User %s has disconnected', [Client.Username]));
end;

procedure TMyApplication.LogMessage(const Message: string);
begin
  // Thread-safe logging to all connected clients
  WriteLn(Format('[LOG] %s', [Message]));
  FTelnetServer.BroadcastMessage(Message);
  
  // Store in local log (you might want to persist this)
  FLogMessages.Add(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Message]));
  
  // Keep only last 1000 messages to prevent memory growth
  if FLogMessages.Count > 1000 then
    FLogMessages.Delete(0);
end;

procedure TMyApplication.LogToUser(const Username, Message: string);
begin
  WriteLn(Format('[LOG->%s] %s', [Username, Message]));
  FTelnetServer.BroadcastToUser(Username, Message);
end;

procedure TMyApplication.SimulateLogMessages;
var
  Counter: Integer;
begin
  Counter := 0;
  
  // Simulate periodic log messages (in a real app, these would come from your application)
  while FTelnetServer.Active do
  begin
    Sleep(5000); // Wait 5 seconds
    
    Inc(Counter);
    case Counter mod 4 of
      0: LogMessage('System status check completed successfully');
      1: LogMessage(Format('Processing queue: %d items pending', [Random(50)]));
      2: LogMessage('Database connection verified');
      3: LogMessage(Format('Memory usage: %d MB', [Random(512) + 256]));
    end;
    
    // Send a targeted message occasionally
    if (Counter mod 10) = 0 then
      LogToUser('admin', 'Special admin notification: System running normally');
  end;
end;

procedure TMyApplication.Run;
begin
  try
    WriteLn('Starting Telnet Server Example...');
    WriteLn('Available test users:');
    WriteLn('  admin:password123');
    WriteLn('  user1:secret');
    WriteLn('  monitor:watch');
    WriteLn('  guest:guest');
    WriteLn('');
    
    // Start the telnet server on port 2323
    FTelnetServer.Start(2323);
    WriteLn('Telnet server started on port 2323');
    WriteLn('Connect with: telnet localhost 2323');
    WriteLn('');
    
    // Send initial log message
    LogMessage('Server started and ready for connections');
    
    // Start simulating log messages in a separate thread
    TThread.CreateAnonymousThread(SimulateLogMessages).Start;
    
    WriteLn('Press ENTER to stop server...');
    ReadLn;
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ReadLn;
    end;
  end;
end;

procedure TMyApplication.Stop;
begin
  if Assigned(FTelnetServer) then
  begin
    LogMessage('Server shutting down...');
    Sleep(1000); // Give time for message to be sent
    FTelnetServer.Stop;
  end;
end;

var
  App: TMyApplication;

begin
  try
    App := TMyApplication.Create;
    try
      App.Run;
    finally
      App.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      ReadLn;
    end;
  end;
end.'clients' then
  begin
    ClientList := TStringList.Create;
    try
      FTelnetServer.GetClientList(ClientList);
      if ClientList.Count = 0 then
        Result := 'No clients connected'
      else
      begin
        Result := 'Connected clients:';
        for i := 0 to ClientList.Count - 1 do
          Result := Result + #13#10 + '  ' + ClientList[i];
      end;
    finally
      ClientList.Free;
    end;
  end
  else if Cmd = 