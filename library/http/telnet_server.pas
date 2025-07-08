unit telnet_server;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Math, SyncObjs, Sockets,
  {$IFDEF MSWINDOWS}
  WinSock2, Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix, Unix,
  {$ENDIF}
  Generics.Collections;

type
  // Forward declarations
  TTelnetServer = class;
  TTelnetClient = class;
  
  // Event types
  TAuthenticateEvent = function(const Username, Password: string): Boolean of object;
  TCommandEvent = function(Client: TTelnetClient; const Command: string): string of object;
  TClientConnectEvent = procedure(Client: TTelnetClient) of object;
  TClientDisconnectEvent = procedure(Client: TTelnetClient) of object;
  
  // Client connection class
  TTelnetClient = class(TThread)
  private
    {$IFDEF MSWINDOWS}
    FSocket: TSocket;
    {$ELSE}
    FSocket: cint;
    {$ENDIF}
    FServer: TTelnetServer;
    FUsername: string;
    FAuthenticated: Boolean;
    FLastActivity: TDateTime;
    FLogBuffer: TStringList;
    FLogLock: TCriticalSection;
    FSendLock: TCriticalSection;
    FClientIP: string;
    FConnectTime: TDateTime;
    
    procedure HandleAuthentication;
    function ReadLine: string;
    procedure SendLine(const Line: string);
    procedure SendPrompt;
    procedure ProcessCommand(const Command: string);
    function GetClientInfo: string;
    
  protected
    procedure Execute; override;
    
  public
    {$IFDEF MSWINDOWS}
    constructor Create(ASocket: TSocket; AServer: TTelnetServer);
    {$ELSE}
    constructor Create(ASocket: cint; AServer: TTelnetServer);
    {$ENDIF}
    destructor Destroy; override;
    
    procedure Disconnect;
    procedure SendLogMessage(const Message: string);
    
    property Username: string read FUsername;
    property Authenticated: Boolean read FAuthenticated;
    property ClientIP: string read FClientIP;
    property ConnectTime: TDateTime read FConnectTime;
    property LastActivity: TDateTime read FLastActivity;
  end;
  
  // Main telnet server class
  TTelnetServer = class(TThread)
  private
    FPort: Word;
    FActive: Boolean;
    {$IFDEF MSWINDOWS}
    FListenSocket: TSocket;
    FShutdownEvent: THandle;
    {$ELSE}
    FListenSocket: cint;
    FPipeFds: TFilDes;
    {$ENDIF}
    FClients: TObjectList<TTelnetClient>;
    FClientsLock: TCriticalSection;
    FMaxClients: Integer;
    FAuthTimeout: Integer; // seconds
    FIdleTimeout: Integer; // seconds
    
    // Events
    FOnAuthenticate: TAuthenticateEvent;
    FOnCommand: TCommandEvent;
    FOnClientConnect: TClientConnectEvent;
    FOnClientDisconnect: TClientDisconnectEvent;
    
    procedure InitializeSockets;
    procedure CleanupSockets;
    procedure AcceptConnections;
    procedure CleanupIdleClients;
    
  protected
    procedure Execute; override;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Start(Port: Word);
    procedure Stop;
    
    // Client management
    procedure AddClient(Client: TTelnetClient);
    procedure RemoveClient(Client: TTelnetClient);
    function GetClientCount: Integer;
    procedure GetClientList(List: TStringList);
    
    // Broadcasting
    procedure BroadcastMessage(const Message: string);
    procedure BroadcastToUser(const Username, Message: string);
    
    // Authentication and command handling
    function AuthenticateUser(const Username, Password: string): Boolean;
    function HandleCommand(Client: TTelnetClient; const Command: string): string;
    
    // Properties
    property Port: Word read FPort;
    property Active: Boolean read FActive;
    property MaxClients: Integer read FMaxClients write FMaxClients;
    property AuthTimeout: Integer read FAuthTimeout write FAuthTimeout;
    property IdleTimeout: Integer read FIdleTimeout write FIdleTimeout;
    property ClientCount: Integer read GetClientCount;
    
    // Events
    property OnAuthenticate: TAuthenticateEvent read FOnAuthenticate write FOnAuthenticate;
    property OnCommand: TCommandEvent read FOnCommand write FOnCommand;
    property OnClientConnect: TClientConnectEvent read FOnClientConnect write FOnClientConnect;
    property OnClientDisconnect: TClientDisconnectEvent read FOnClientDisconnect write FOnClientDisconnect;
  end;

implementation

uses
  DateUtils;

const
  {$IFDEF MSWINDOWS}
  INVALID_SOCKET_VALUE = INVALID_SOCKET;
  {$ELSE}
  INVALID_SOCKET_VALUE = -1;
  {$ENDIF}
  
  CRLF = #13#10;
  DEFAULT_MAX_CLIENTS = 10;
  DEFAULT_AUTH_TIMEOUT = 30; // seconds
  DEFAULT_IDLE_TIMEOUT = 3600; // 1 hour
  BUFFER_SIZE = 1024;

{ TTelnetClient }

{$IFDEF MSWINDOWS}
constructor TTelnetClient.Create(ASocket: TSocket; AServer: TTelnetServer);
{$ELSE}
constructor TTelnetClient.Create(ASocket: cint; AServer: TTelnetServer);
{$ENDIF}
begin
  inherited Create(False);
  FSocket := ASocket;
  FServer := AServer;
  FUsername := '';
  FAuthenticated := False;
  FLastActivity := Now;
  FConnectTime := Now;
  FLogBuffer := TStringList.Create;
  FLogLock := TCriticalSection.Create;
  FSendLock := TCriticalSection.Create;
  
  // Try to get client IP (simplified)
  FClientIP := 'unknown';
  
  FreeOnTerminate := True;
end;

destructor TTelnetClient.Destroy;
begin
  Disconnect;
  FLogBuffer.Free;
  FLogLock.Free;
  FSendLock.Free;
  inherited Destroy;
end;

procedure TTelnetClient.Execute;
var
  Command : String;
begin
  try
    FServer.AddClient(Self);
    
    try
      // Handle authentication first
      HandleAuthentication;
      
      if FAuthenticated then
      begin
        SendLine('Welcome! Type commands or wait for log messages.');
        SendPrompt;
        
        // Main command loop
        while not Terminated and FAuthenticated do
        begin
          if Terminated then Break;
          
          // Check for incoming commands (non-blocking)
          Sleep(50); // Small delay to prevent busy waiting
          
          // Process any pending log messages
          FLogLock.Enter;
          try
            while FLogBuffer.Count > 0 do
            begin
              SendLine(FLogBuffer[0]);
              FLogBuffer.Delete(0);
            end;
          finally
            FLogLock.Leave;
          end;
          
          // Try to read a command
          try
            if Terminated then Break;
            
            // Simple check for data availability would go here
            // For now, we'll use a blocking read with timeout
            Command := ReadLine;
            if Command <> '' then
            begin
              FLastActivity := Now;
              ProcessCommand(Trim(Command));
              if FAuthenticated then
                SendPrompt;
            end;
          except
            // Connection lost or error
            Break;
          end;
        end;
      end;
      
    finally
      FServer.RemoveClient(Self);
    end;
    
  except
    on E: Exception do
    begin
      // Log error if needed
      FServer.RemoveClient(Self);
    end;
  end;
end;

procedure TTelnetClient.HandleAuthentication;
var
  Username, Password: string;
  Attempts: Integer;
  StartTime: TDateTime;
begin
  StartTime := Now;
  Attempts := 0;
  
  while (not Terminated) and (not FAuthenticated) and (Attempts < 3) do
  begin
    // Check timeout
    if SecondsBetween(Now, StartTime) > FServer.AuthTimeout then
    begin
      SendLine('Authentication timeout.');
      Exit;
    end;
    
    try
      SendLine('Username: ');
      Username := Trim(ReadLine);
      if Username = '' then Continue;
      
      SendLine('Password: ');
      Password := Trim(ReadLine);
      
      if FServer.AuthenticateUser(Username, Password) then
      begin
        FUsername := Username;
        FAuthenticated := True;
        FLastActivity := Now;
        SendLine('Authentication successful.');
        
        // Notify server of successful connection
        if Assigned(FServer.OnClientConnect) then
          FServer.OnClientConnect(Self);
      end
      else
      begin
        Inc(Attempts);
        SendLine(Format('Authentication failed. Attempts remaining: %d', [3 - Attempts]));
      end;
      
    except
      // Connection error during auth
      Exit;
    end;
  end;
  
  if not FAuthenticated then
  begin
    SendLine('Authentication failed. Disconnecting.');
    Sleep(1000); // Give time for message to send
  end;
end;

function TTelnetClient.ReadLine: string;
var
  Buffer: array[0..BUFFER_SIZE-1] of Byte;
  BytesRead: Integer;
  i: Integer;
  Line: string;
  C: Char;
begin
  Result := '';
  Line := '';
  
  try
    while not Terminated do
    begin
      {$IFDEF MSWINDOWS}
      BytesRead := recv(FSocket, Buffer, 1, 0);
      {$ELSE}
      BytesRead := fprecv(FSocket, @Buffer, 1, 0);
      {$ENDIF}
      
      if BytesRead <= 0 then
      begin
        // Connection closed or error
        Exit;
      end;
      
      C := Chr(Buffer[0]);
      
      if C = #13 then
        Continue; // Skip CR
      if C = #10 then
      begin
        // End of line
        Result := Line;
        Exit;
      end;
      
      if Ord(C) >= 32 then // Printable character
        Line := Line + C;
        
      if Length(Line) > 1000 then // Prevent huge lines
      begin
        Result := Line;
        Exit;
      end;
    end;
  except
    // Connection error
    Result := '';
  end;
end;

procedure TTelnetClient.SendLine(const Line: string);
var
  Data: string;
  Bytes: TBytes;
  BytesSent: Integer;
  TotalSent: Integer;
begin
  if FSocket = INVALID_SOCKET_VALUE then Exit;
  
  FSendLock.Enter;
  try
    Data := Line + CRLF;
    Bytes := TEncoding.ASCII.GetBytes(Data);
    TotalSent := 0;
    
    while TotalSent < Length(Bytes) do
    begin
      {$IFDEF MSWINDOWS}
      BytesSent := send(FSocket, Bytes[TotalSent], Length(Bytes) - TotalSent, 0);
      {$ELSE}
      BytesSent := fpsend(FSocket, @Bytes[TotalSent], Length(Bytes) - TotalSent, 0);
      {$ENDIF}
      
      if BytesSent <= 0 then
        Break;
        
      Inc(TotalSent, BytesSent);
    end;
  finally
    FSendLock.Leave;
  end;
end;

procedure TTelnetClient.SendPrompt;
begin
  SendLine('> ');
end;

procedure TTelnetClient.ProcessCommand(const Command: string);
var
  Response: string;
begin
  if Command = '' then Exit;
  
  try
    // Handle built-in commands
    if SameText(Command, 'quit') or SameText(Command, 'exit') then
    begin
      SendLine('Goodbye!');
      FAuthenticated := False;
      Terminate;
      Exit;
    end
    else if SameText(Command, 'help') then
    begin
      SendLine('Available commands: help, quit, exit, info');
      SendLine('Other commands are sent to the host application.');
    end
    else if SameText(Command, 'info') then
    begin
      SendLine(GetClientInfo);
    end
    else
    begin
      // Send to host application
      Response := FServer.HandleCommand(Self, Command);
      if Response <> '' then
        SendLine(Response);
    end;
  except
    on E: Exception do
      SendLine('Error processing command: ' + E.Message);
  end;
end;

function TTelnetClient.GetClientInfo: string;
begin
  Result := Format('User: %s, IP: %s, Connected: %s, Last Activity: %s',
    [FUsername, FClientIP, 
     FormatDateTime('yyyy-mm-dd hh:nn:ss', FConnectTime),
     FormatDateTime('yyyy-mm-dd hh:nn:ss', FLastActivity)]);
end;

procedure TTelnetClient.Disconnect;
begin
  if FSocket <> INVALID_SOCKET_VALUE then
  begin
    {$IFDEF MSWINDOWS}
    closesocket(FSocket);
    {$ELSE}
    fpclose(FSocket);
    {$ENDIF}
    FSocket := INVALID_SOCKET_VALUE;
  end;
  
  if Assigned(FServer.OnClientDisconnect) then
    FServer.OnClientDisconnect(Self);
end;

procedure TTelnetClient.SendLogMessage(const Message: string);
begin
  if not FAuthenticated then Exit;
  
  FLogLock.Enter;
  try
    FLogBuffer.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now), Message]));
  finally
    FLogLock.Leave;
  end;
end;

{ TTelnetServer }

constructor TTelnetServer.Create;
begin
  inherited Create(True); // Create suspended
  FPort := 0;
  FActive := False;
  FListenSocket := INVALID_SOCKET_VALUE;
  FClients := TObjectList<TTelnetClient>.Create(False); // Don't own objects
  FClientsLock := TCriticalSection.Create;
  FMaxClients := DEFAULT_MAX_CLIENTS;
  FAuthTimeout := DEFAULT_AUTH_TIMEOUT;
  FIdleTimeout := DEFAULT_IDLE_TIMEOUT;
  
  {$IFDEF MSWINDOWS}
  FShutdownEvent := 0;
  {$ELSE}
  FPipeFds[0] := -1;
  FPipeFds[1] := -1;
  {$ENDIF}
  
  FreeOnTerminate := False;
end;

destructor TTelnetServer.Destroy;
begin
  Stop;
  FClients.Free;
  FClientsLock.Free;
  inherited Destroy;
end;

procedure TTelnetServer.Start(Port: Word);
begin
  if FActive then Exit;
  
  FPort := Port;
  InitializeSockets;
  FActive := True;
  
  Resume; // Start the thread
end;

procedure TTelnetServer.Stop;
var
  Client: TTelnetClient;
  SignalByte: Byte;
begin
  if not FActive then Exit;
  
  FActive := False;
  Terminate;
  
  // Signal shutdown
  {$IFDEF MSWINDOWS}
  if FShutdownEvent <> 0 then
    SetEvent(FShutdownEvent);
  {$ELSE}
  if FPipeFds[1] <> -1 then
  begin
    SignalByte := 1;
    fpwrite(FPipeFds[1], SignalByte, 1);
  end;
  {$ENDIF}
  
  WaitFor;
  
  // Disconnect all clients
  FClientsLock.Enter;
  try
    for Client in FClients do
    begin
      Client.Terminate;
      Client.Disconnect;
    end;
    FClients.Clear;
  finally
    FClientsLock.Leave;
  end;
  
  CleanupSockets;
end;

procedure TTelnetServer.Execute;
begin
  try
    while not Terminated and FActive do
    begin
      AcceptConnections;
      CleanupIdleClients;
      Sleep(100); // Small delay
    end;
  except
    on E: Exception do
    begin
      // Log error if needed
      FActive := False;
    end;
  end;
end;

procedure TTelnetServer.InitializeSockets;
var
  Addr: sockaddr_in;
  OptVal: Integer;
begin
  {$IFDEF MSWINDOWS}
  var WSAData: TWSAData;
  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
    raise Exception.Create('Failed to initialize Winsock');
    
  FShutdownEvent := CreateEvent(nil, True, False, nil);
  {$ELSE}
  if fppipe(FPipeFds) <> 0 then
    raise Exception.Create('Failed to create pipe');
  {$ENDIF}
  
  {$IFDEF MSWINDOWS}
  FListenSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  {$ELSE}
  FListenSocket := fpsocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  {$ENDIF}
  if FListenSocket = INVALID_SOCKET_VALUE then
    raise Exception.Create('Failed to create socket');
    
  // Set socket options
  OptVal := 1;
  {$IFDEF MSWINDOWS}
  setsockopt(FListenSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));
  {$ELSE}
  fpsetsockopt(FListenSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));
  {$ENDIF}
  
  // Bind and listen
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_addr.s_addr := 0; // INADDR_ANY
  Addr.sin_port := htons(FPort);
  
  {$IFDEF MSWINDOWS}
  if bind(FListenSocket, PSockAddr(@Addr), SizeOf(Addr)) <> 0 then
    raise Exception.CreateFmt('Failed to bind to port %d', [FPort]);
    
  if listen(FListenSocket, SOMAXCONN) <> 0 then
    raise Exception.Create('Failed to listen on socket');
  {$ELSE}
  if fpbind(FListenSocket, @Addr, SizeOf(Addr)) <> 0 then
    raise Exception.CreateFmt('Failed to bind to port %d', [FPort]);
    
  if fplisten(FListenSocket, SOMAXCONN) <> 0 then
    raise Exception.Create('Failed to listen on socket');
  {$ENDIF}
end;

procedure TTelnetServer.CleanupSockets;
begin
  if FListenSocket <> INVALID_SOCKET_VALUE then
  begin
    {$IFDEF MSWINDOWS}
    closesocket(FListenSocket);
    {$ELSE}
    fpclose(FListenSocket);
    {$ENDIF}
    FListenSocket := INVALID_SOCKET_VALUE;
  end;
  
  {$IFDEF MSWINDOWS}
  if FShutdownEvent <> 0 then
  begin
    CloseHandle(FShutdownEvent);
    FShutdownEvent := 0;
  end;
  WSACleanup;
  {$ELSE}
  if FPipeFds[0] <> -1 then
  begin
    fpclose(FPipeFds[0]);
    FPipeFds[0] := -1;
  end;
  if FPipeFds[1] <> -1 then
  begin
    fpclose(FPipeFds[1]);
    FPipeFds[1] := -1;
  end;
  {$ENDIF}
end;

procedure TTelnetServer.AcceptConnections;
var
  {$IFDEF MSWINDOWS}
  ClientSocket: TSocket;
  ReadSet: TFDSet;
  Timeout: TTimeVal;
  {$ELSE}
  ClientSocket: cint;
  ReadSet: TFDSet;
  Timeout: TTimeVal;
  MaxSocket: cint;
  {$ENDIF}
  Client: TTelnetClient;
  Msg : String;
begin
  if not FActive then Exit;
  
  {$IFDEF MSWINDOWS}
  FD_ZERO(ReadSet);
  FD_SET(FListenSocket, ReadSet);
  
  Timeout.tv_sec := 1;
  Timeout.tv_usec := 0;
  
  if select(FListenSocket + 1, @ReadSet, nil, nil, @Timeout) > 0 then
  begin
    if FD_ISSET(FListenSocket, ReadSet) then
    begin
      ClientSocket := accept(FListenSocket, nil, nil);
      if ClientSocket <> INVALID_SOCKET_VALUE then
      begin
        if GetClientCount >= FMaxClients then
        begin
          // Too many clients
          Msg := 'Server full. Try again later.' + CRLF;
          send(ClientSocket, PAnsiChar(Msg)^, Length(Msg), 0);
          closesocket(ClientSocket);
        end
        else
        begin
          Client := TTelnetClient.Create(ClientSocket, Self);
        end;
      end;
    end;
  end;
  {$ELSE}
  fpFD_ZERO(ReadSet);
  fpFD_SET(FListenSocket, ReadSet);
  fpFD_SET(FPipeFds[0], ReadSet);
  MaxSocket := Math.Max(FListenSocket, FPipeFds[0]);
  
  Timeout.tv_sec := 1;
  Timeout.tv_usec := 0;
  
  if fpselect(MaxSocket + 1, @ReadSet, nil, nil, @Timeout) > 0 then
  begin
    if fpFD_ISSET(FPipeFds[0], ReadSet) <> 0 then
      Exit; // Shutdown signal
      
    if fpFD_ISSET(FListenSocket, ReadSet) <> 0 then
    begin
      ClientSocket := fpaccept(FListenSocket, nil, nil);
      if ClientSocket <> -1 then
      begin
        if GetClientCount >= FMaxClients then
        begin
          // Too many clients
          Msg := 'Server full. Try again later.' + CRLF;
          fpsend(ClientSocket, @Msg[1], Length(Msg), 0);
          fpclose(ClientSocket);
        end
        else
        begin
          Client := TTelnetClient.Create(ClientSocket, Self);
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TTelnetServer.CleanupIdleClients;
var
  Client: TTelnetClient;
  i: Integer;
  Now: TDateTime;
begin
  Now := SysUtils.Now;
  
  FClientsLock.Enter;
  try
    for i := FClients.Count - 1 downto 0 do
    begin
      Client := FClients[i];
      
      // Check for idle timeout
      if SecondsBetween(Now, Client.LastActivity) > FIdleTimeout then
      begin
        Client.SendLine('Idle timeout. Disconnecting.');
        Client.Terminate;
        Client.Disconnect;
        FClients.Delete(i);
      end;
    end;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TTelnetServer.AddClient(Client: TTelnetClient);
begin
  FClientsLock.Enter;
  try
    FClients.Add(Client);
  finally
    FClientsLock.Leave;
  end;
end;

procedure TTelnetServer.RemoveClient(Client: TTelnetClient);
begin
  FClientsLock.Enter;
  try
    FClients.Remove(Client);
  finally
    FClientsLock.Leave;
  end;
end;

function TTelnetServer.GetClientCount: Integer;
begin
  FClientsLock.Enter;
  try
    Result := FClients.Count;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TTelnetServer.GetClientList(List: TStringList);
var
  Client: TTelnetClient;
begin
  List.Clear;
  
  FClientsLock.Enter;
  try
    for Client in FClients do
    begin
      if Client.Authenticated then
        List.Add(Format('%s (%s) - %s', 
          [Client.Username, Client.ClientIP, 
           FormatDateTime('yyyy-mm-dd hh:nn:ss', Client.ConnectTime)]));
    end;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TTelnetServer.BroadcastMessage(const Message: string);
var
  Client: TTelnetClient;
begin
  FClientsLock.Enter;
  try
    for Client in FClients do
    begin
      if Client.Authenticated then
        Client.SendLogMessage(Message);
    end;
  finally
    FClientsLock.Leave;
  end;
end;

procedure TTelnetServer.BroadcastToUser(const Username, Message: string);
var
  Client: TTelnetClient;
begin
  FClientsLock.Enter;
  try
    for Client in FClients do
    begin
      if Client.Authenticated and SameText(Client.Username, Username) then
        Client.SendLogMessage(Message);
    end;
  finally
    FClientsLock.Leave;
  end;
end;

function TTelnetServer.AuthenticateUser(const Username, Password: string): Boolean;
begin
  Result := False;
  if Assigned(FOnAuthenticate) then
    Result := FOnAuthenticate(Username, Password);
end;

function TTelnetServer.HandleCommand(Client: TTelnetClient; const Command: string): string;
begin
  Result := '';
  if Assigned(FOnCommand) then
    Result := FOnCommand(Client, Command);
end;

end.
