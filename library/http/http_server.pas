unit http_server;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  {$IFDEF MSWINDOWS}
  WinSock2, Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix, Unix, NetDB, Sockets,
  {$ENDIF}
  http_support;

type
  // Forward declarations
  TLightHttpServer = class;

  // Event handler type
  THttpRequestHandler = procedure(Request: THttpRequest; Response: THttpResponse) of object;

  // Request processing statistics
  TServerStats = record
    ActiveRequests: Integer;
    TotalRequests: Int64;
    AverageResponseTimeMs: Double;
    ServerStartTime: TDateTime;
  end;

  // Client connection state
  TClientConnection = class
  private
    {$IFDEF MSWINDOWS}
    FSocket: TSocket;
    {$ELSE}
    FSocket: cint;
    {$ENDIF}
    FBuffer: TBytes;
    FBufferPos: Integer;
    FBufferSize: Integer;
    FRequestComplete: Boolean;
    FKeepAlive: Boolean;
    FLastActivity: TDateTime;
    FServer: TLightHttpServer;
    FRequest: THttpRequest;
    FResponse: THttpResponse;

    procedure GrowBuffer(MinSize: Integer);
    function FindHeaderEnd: Integer;
    function ParseHttpRequest: Boolean;
    procedure ProcessRequest;
    procedure SendResponse;
    function ReadMoreData: Boolean;
  public
    {$IFDEF MSWINDOWS}
    constructor Create(ASocket: TSocket; AServer: TLightHttpServer);
    {$ELSE}
    constructor Create(ASocket: cint; AServer: TLightHttpServer);
    {$ENDIF}
    destructor Destroy; override;

    procedure HandleData;
    procedure Disconnect;

    {$IFDEF MSWINDOWS}
    property Socket: TSocket read FSocket;
    {$ELSE}
    property Socket: cint read FSocket;
    {$ENDIF}
    property LastActivity: TDateTime read FLastActivity;
    property KeepAlive: Boolean read FKeepAlive;
  end;

  // Base server class - platform independent interface
  TLightHttpServer = class
  private
    FPort: Word;
    FActive: Boolean;
    FRequestHandler: THttpRequestHandler;
    FConnections: TThreadList;
    FCriticalSection: TCriticalSection;
    FStats: TServerStats;
    FMaxConnections: Integer;
    FConnectionTimeoutSec: Integer;

  protected
    // Platform-specific implementations will override these
    procedure DoStart; virtual; abstract;
    procedure DoStop; virtual; abstract;
    procedure DoAcceptConnections; virtual; abstract;

    // Common functionality
    procedure AddConnection(Connection: TClientConnection);
    procedure RemoveConnection(Connection: TClientConnection);
    procedure CleanupIdleConnections;
    procedure UpdateStats(ResponseTimeMs: Cardinal);

  public
    constructor Create;
    destructor Destroy; override;

    // Server control
    procedure Start;
    procedure Stop;
    procedure SetRequestHandler(Handler: THttpRequestHandler);

    // Internal methods called by connections
    procedure HandleRequest(Request: THttpRequest; Response: THttpResponse);

    // Properties
    property Port: Word read FPort write FPort;
    property Active: Boolean read FActive;
    property Stats: TServerStats read FStats;
    property MaxConnections: Integer read FMaxConnections write FMaxConnections;
    property ConnectionTimeoutSec: Integer read FConnectionTimeoutSec write FConnectionTimeoutSec;
  end;

{$IFDEF MSWINDOWS}
  // Windows server thread
  TWindowsServerThread = class(TThread)
  private
    FServer: TLightHttpServerWindows;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TLightHttpServerWindows);
  end;

  // Windows implementation using select()
  TLightHttpServerWindows = class(TLightHttpServer)
  private
    FListenSocket: TSocket;
    FServerThread: TWindowsServerThread;
    FShutdownEvent: THandle;

  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure DoAcceptConnections; override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ServerThreadExecute;
  end;
{$ENDIF}

{$IFDEF UNIX}
  TLightHttpServerUnix = class;

  // Unix server thread
  TUnixServerThread = class(TThread)
  private
    FServer: TLightHttpServerUnix;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TLightHttpServerUnix);
  end;

  // Unix implementation using epoll/kqueue
  TLightHttpServerUnix = class(TLightHttpServer)
  private
    FListenSocket: cint;
    FServerThread: TThread;
    FPipeFds: TFilDes; // For shutdown signaling
    {$IFDEF LINUX}
    FEpollFd: cint;
    {$ENDIF}

    procedure ServerThreadExecute;
    procedure InitializeEventSystem;
    procedure CleanupEventSystem;

  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure DoAcceptConnections; override;

  public
    constructor Create;
    destructor Destroy; override;
  end;
{$ENDIF}

// Factory function to create appropriate server for platform
function CreateHttpServer: TLightHttpServer;

implementation

uses
  DateUtils, Math;

const
  BUFFER_INITIAL_SIZE = 8192;
  BUFFER_MAX_SIZE = 1024 * 1024; // 1MB max request size
  DEFAULT_CONNECTION_TIMEOUT = 30; // seconds
  DEFAULT_MAX_CONNECTIONS = 1000;

  // Cross-platform socket constants
  {$IFDEF MSWINDOWS}
  INVALID_SOCKET_VALUE = INVALID_SOCKET;
  SOCKET_ERROR_VALUE = SOCKET_ERROR;
  {$ELSE}
  INVALID_SOCKET_VALUE = -1;
  SOCKET_ERROR_VALUE = -1;
  {$ENDIF}

{ TClientConnection }

{$IFDEF MSWINDOWS}
constructor TClientConnection.Create(ASocket: TSocket; AServer: TLightHttpServer);
{$ELSE}
constructor TClientConnection.Create(ASocket: cint; AServer: TLightHttpServer);
{$ENDIF}
begin
  inherited Create;
  FSocket := ASocket;
  FServer := AServer;
  FBufferSize := BUFFER_INITIAL_SIZE;
  SetLength(FBuffer, FBufferSize);
  FBufferPos := 0;
  FRequestComplete := False;
  FKeepAlive := False;
  FLastActivity := Now;
  FRequest := THttpRequest.Create;
  FResponse := THttpResponse.Create;
end;

destructor TClientConnection.Destroy;
begin
  if FSocket <> INVALID_SOCKET_VALUE then
    Disconnect;
  FRequest.Free;
  FResponse.Free;
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

procedure TClientConnection.GrowBuffer(MinSize: Integer);
var
  NewSize: Integer;
begin
  if MinSize <= FBufferSize then Exit;

  NewSize := FBufferSize * 2;
  while NewSize < MinSize do
    NewSize := NewSize * 2;

  if NewSize > BUFFER_MAX_SIZE then
    raise Exception.Create('Request too large');

  FBufferSize := NewSize;
  SetLength(FBuffer, FBufferSize);
end;

function TClientConnection.ReadMoreData: Boolean;
var
  BytesRead: Integer;
  AvailableSpace: Integer;
begin
  Result := False;

  AvailableSpace := FBufferSize - FBufferPos;
  if AvailableSpace < 1024 then
  begin
    GrowBuffer(FBufferPos + 4096);
    AvailableSpace := FBufferSize - FBufferPos;
  end;

  {$IFDEF MSWINDOWS}
  BytesRead := recv(FSocket, FBuffer[FBufferPos], AvailableSpace, 0);
  {$ELSE}
  BytesRead := fprecv(FSocket, @FBuffer[FBufferPos], AvailableSpace, 0);
  {$ENDIF}

  if BytesRead > 0 then
  begin
    Inc(FBufferPos, BytesRead);
    FLastActivity := Now;
    Result := True;
  end
  else if BytesRead = 0 then
  begin
    // Connection closed by client
    Disconnect;
  end
  else
  begin
    // Error occurred
    {$IFDEF MSWINDOWS}
    if WSAGetLastError <> WSAEWOULDBLOCK then
      Disconnect;
    {$ELSE}
    if fpgeterrno <> ESysEAGAIN then
      Disconnect;
    {$ENDIF}
  end;
end;

function TClientConnection.FindHeaderEnd: Integer;
var
  i: Integer;
begin
  Result := -1;

  // Look for \r\n\r\n (end of headers)
  for i := 0 to FBufferPos - 4 do
  begin
    if (FBuffer[i] = 13) and (FBuffer[i+1] = 10) and
       (FBuffer[i+2] = 13) and (FBuffer[i+3] = 10) then
    begin
      Result := i + 4;
      Exit;
    end;
  end;
end;

function TClientConnection.ParseHttpRequest: Boolean;
var
  HeaderEnd: Integer;
  HeaderText: string;
  Lines: TStringArray;
  i: Integer;
  Line: string;
  ColonPos: Integer;
  HeaderName, HeaderValue: string;
  ContentLength: Integer;
  BodyStart: Integer;
begin
  Result := False;

  HeaderEnd := FindHeaderEnd;
  if HeaderEnd = -1 then Exit; // Headers not complete yet

  // Extract and parse headers
  HeaderText := TEncoding.UTF8.GetString(FBuffer, 0, HeaderEnd - 4);
  Lines := HeaderText.Split([#13#10]);

  if Length(Lines) = 0 then Exit;

  // Parse request line
  if not FRequest.ParseRequestLine(Lines[0]) then Exit;

  // Parse headers
  for i := 1 to High(Lines) do
  begin
    Line := Trim(Lines[i]);
    if Line = '' then Continue;

    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
    begin
      HeaderName := Trim(Copy(Line, 1, ColonPos - 1));
      HeaderValue := Trim(Copy(Line, ColonPos + 1, Length(Line)));
      FRequest.Headers.SetHeader(HeaderName, HeaderValue);
    end;
  end;

  // Check if we need to read body
  ContentLength := FRequest.GetContentLength;
  BodyStart := HeaderEnd;

  if ContentLength > 0 then
  begin
    // Make sure we have the complete body
    if FBufferPos < BodyStart + ContentLength then
      Exit; // Body not complete yet

    // Extract body
    FRequest.SetBody(Copy(FBuffer, BodyStart, ContentLength));
  end;

  // Check for keep-alive
  FKeepAlive := SameText(FRequest.GetHeader('Connection'), 'keep-alive');

  Result := True;
end;

procedure TClientConnection.HandleData;
begin
  if not ReadMoreData then Exit;

  if not FRequestComplete then
  begin
    FRequestComplete := ParseHttpRequest;
    if FRequestComplete then
      ProcessRequest;
  end;
end;

procedure TClientConnection.ProcessRequest;
var
  StartTime: TDateTime;
  ResponseTime: Cardinal;
begin
  StartTime := Now;

  try
    // Clear previous response
    FResponse := THttpResponse.Create;

    // Set client IP
    FRequest.ClientIP := '127.0.0.1'; // TODO: Extract real client IP

    // Call the request handler
    FServer.HandleRequest(FRequest, FResponse);

    // Send response
    SendResponse;

    // Update statistics
    ResponseTime := MilliSecondsBetween(Now, StartTime);
    FServer.UpdateStats(ResponseTime);

  except
    on E: Exception do
    begin
      // Send error response
      FResponse.SetStatus(500, 'Internal Server Error');
      FResponse.SetTextContent('Internal Server Error: ' + E.Message);
      SendResponse;
    end;
  end;

  // Reset for next request or close connection
  if FKeepAlive then
  begin
    // Reset for next request on same connection
    FRequestComplete := False;
    FBufferPos := 0;
    FreeAndNil(FRequest);
    FreeAndNil(FResponse);
    FRequest := THttpRequest.Create;
    FResponse := THttpResponse.Create;
  end
  else
  begin
    Disconnect;
  end;
end;

procedure TClientConnection.SendResponse;
var
  ResponseData: TBytes;
  BytesSent: Integer;
  TotalSent: Integer;
begin
  ResponseData := FResponse.BuildResponse;
  TotalSent := 0;

  while TotalSent < Length(ResponseData) do
  begin
    {$IFDEF MSWINDOWS}
    BytesSent := send(FSocket, ResponseData[TotalSent], Length(ResponseData) - TotalSent, 0);
    {$ELSE}
    BytesSent := fpsend(FSocket, @ResponseData[TotalSent], Length(ResponseData) - TotalSent, 0);
    {$ENDIF}
    if BytesSent <= 0 then
      Break;
    Inc(TotalSent, BytesSent);
  end;
end;

procedure TClientConnection.Disconnect;
begin
  if FSocket <> INVALID_SOCKET_VALUE then
  begin
    {$IFDEF MSWINDOWS}
    closesocket(FSocket);
    {$ELSE}
    fpclose(FSocket);
    {$ENDIF}
    FSocket := INVALID_SOCKET_VALUE;
    FServer.RemoveConnection(Self);
  end;
end;

{ TLightHttpServer }

constructor TLightHttpServer.Create;
begin
  inherited Create;
  FPort := 8080;
  FActive := False;
  FConnections := TThreadList.Create;
  FCriticalSection := TCriticalSection.Create;
  FMaxConnections := DEFAULT_MAX_CONNECTIONS;
  FConnectionTimeoutSec := DEFAULT_CONNECTION_TIMEOUT;

  // Initialize stats
  FillChar(FStats, SizeOf(FStats), 0);
  FStats.ServerStartTime := Now;
end;

destructor TLightHttpServer.Destroy;
begin
  Stop;
  FConnections.Free;
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TLightHttpServer.Start;
begin
  if FActive then Exit;

  if not Assigned(FRequestHandler) then
    raise Exception.Create('No request handler assigned');

  FStats.ServerStartTime := Now;
  DoStart;
  FActive := True;
end;

procedure TLightHttpServer.Stop;
begin
  if not FActive then Exit;

  FActive := False;
  DoStop;

  // Close all connections
  CleanupIdleConnections;
end;

procedure TLightHttpServer.SetRequestHandler(Handler: THttpRequestHandler);
begin
  FRequestHandler := Handler;
end;

procedure TLightHttpServer.HandleRequest(Request: THttpRequest; Response: THttpResponse);
begin
  FCriticalSection.Enter;
  try
    Inc(FStats.ActiveRequests);
    Inc(FStats.TotalRequests);
  finally
    FCriticalSection.Leave;
  end;

  try
    if Assigned(FRequestHandler) then
      FRequestHandler(Request, Response)
    else
    begin
      Response.SetStatus(501, 'Not Implemented');
      Response.SetTextContent('No request handler configured');
    end;
  finally
    FCriticalSection.Enter;
    try
      Dec(FStats.ActiveRequests);
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

procedure TLightHttpServer.AddConnection(Connection: TClientConnection);
var
  List: TList;
begin
  List := FConnections.LockList;
  try
    List.Add(Connection);
  finally
    FConnections.UnlockList;
  end;
end;

procedure TLightHttpServer.RemoveConnection(Connection: TClientConnection);
var
  List: TList;
begin
  List := FConnections.LockList;
  try
    List.Remove(Connection);
  finally
    FConnections.UnlockList;
  end;
end;

procedure TLightHttpServer.CleanupIdleConnections;
var
  List: TList;
  i: Integer;
  Connection: TClientConnection;
  Now: TDateTime;
begin
  Now := SysUtils.Now;
  List := FConnections.LockList;
  try
    for i := List.Count - 1 downto 0 do
    begin
      Connection := TClientConnection(List[i]);
      if SecondsBetween(Now, Connection.LastActivity) > FConnectionTimeoutSec then
      begin
        Connection.Disconnect;
        Connection.Free;
        List.Delete(i);
      end;
    end;
  finally
    FConnections.UnlockList;
  end;
end;

procedure TLightHttpServer.UpdateStats(ResponseTimeMs: Cardinal);
const
  ALPHA = 0.1; // Exponential moving average factor
begin
  FCriticalSection.Enter;
  try
    if FStats.AverageResponseTimeMs = 0 then
      FStats.AverageResponseTimeMs := ResponseTimeMs
    else
      FStats.AverageResponseTimeMs := (1 - ALPHA) * FStats.AverageResponseTimeMs + ALPHA * ResponseTimeMs;
  finally
    FCriticalSection.Leave;
  end;
end;

{$IFDEF MSWINDOWS}

{ TWindowsServerThread }

constructor TWindowsServerThread.Create(AServer: TLightHttpServerWindows);
begin
  inherited Create(False);
  FServer := AServer;
  FreeOnTerminate := False;
end;

procedure TWindowsServerThread.Execute;
begin
  FServer.ServerThreadExecute;
end;

{ TLightHttpServerWindows }

constructor TLightHttpServerWindows.Create;
var
  WSAData: TWSAData;
begin
  inherited Create;

  // Initialize Winsock
  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
    raise Exception.Create('Failed to initialize Winsock');

  FListenSocket := INVALID_SOCKET_VALUE;
  FShutdownEvent := CreateEvent(nil, True, False, nil);
end;

destructor TLightHttpServerWindows.Destroy;
begin
  Stop;
  if FShutdownEvent <> 0 then
    CloseHandle(FShutdownEvent);
  WSACleanup;
  inherited Destroy;
end;

procedure TLightHttpServerWindows.DoStart;
var
  Addr: TSockAddrIn;
  OptVal: Integer;
begin
  FListenSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FListenSocket = INVALID_SOCKET_VALUE then
    raise Exception.Create('Failed to create socket');

  // Set socket options
  OptVal := 1;
  setsockopt(FListenSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

  // Make socket non-blocking
  OptVal := 1;
  ioctlsocket(FListenSocket, FIONBIO, OptVal);

  // Bind and listen
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_addr.S_addr := INADDR_ANY;
  Addr.sin_port := htons(FPort);

  if bind(FListenSocket, @Addr, SizeOf(Addr)) = SOCKET_ERROR_VALUE then
    raise Exception.CreateFmt('Failed to bind to port %d', [FPort]);

  if listen(FListenSocket, SOMAXCONN) = SOCKET_ERROR_VALUE then
    raise Exception.Create('Failed to listen on socket');

  // Start server thread
  ResetEvent(FShutdownEvent);
  FServerThread := TWindowsServerThread.Create(Self);
end;

procedure TLightHttpServerWindows.DoStop;
begin
  SetEvent(FShutdownEvent);

  if Assigned(FServerThread) then
  begin
    FServerThread.WaitFor;
    FreeAndNil(FServerThread);
  end;

  if FListenSocket <> INVALID_SOCKET_VALUE then
  begin
    closesocket(FListenSocket);
    FListenSocket := INVALID_SOCKET_VALUE;
  end;
end;

procedure TLightHttpServerWindows.DoAcceptConnections;
begin
  // Implementation in ServerThreadExecute
end;

procedure TLightHttpServerWindows.ServerThreadExecute;
var
  ReadSet, WriteSet, ErrorSet: TFDSet;
  Timeout: TTimeVal;
  ClientSocket: TSocket;
  Connection: TClientConnection;
  List: TList;
  i: Integer;
  MaxSocket: TSocket;
begin
  while not TThread.CurrentThread.CheckTerminated do
  begin
    // Check for shutdown
    if WaitForSingleObject(FShutdownEvent, 0) = WAIT_OBJECT_0 then
      Break;

    FD_ZERO(ReadSet);
    FD_ZERO(WriteSet);
    FD_ZERO(ErrorSet);

    // Add listen socket
    FD_SET(FListenSocket, ReadSet);
    MaxSocket := FListenSocket;

    // Add client sockets
    List := FConnections.LockList;
    try
      for i := 0 to List.Count - 1 do
      begin
        Connection := TClientConnection(List[i]);
        if Connection.Socket <> INVALID_SOCKET_VALUE then
        begin
          FD_SET(Connection.Socket, ReadSet);
          FD_SET(Connection.Socket, ErrorSet);
          if Connection.Socket > MaxSocket then
            MaxSocket := Connection.Socket;
        end;
      end;
    finally
      FConnections.UnlockList;
    end;

    // Wait for activity
    Timeout.tv_sec := 1;
    Timeout.tv_usec := 0;

    if select(MaxSocket + 1, @ReadSet, @WriteSet, @ErrorSet, @Timeout) > 0 then
    begin
      // Check for new connections
      if FD_ISSET(FListenSocket, ReadSet) then
      begin
        ClientSocket := accept(FListenSocket, nil, nil);
        if ClientSocket <> INVALID_SOCKET_VALUE then
        begin
          // Make client socket non-blocking
          i := 1;
          ioctlsocket(ClientSocket, FIONBIO, i);

          Connection := TClientConnection.Create(ClientSocket, Self);
          AddConnection(Connection);
        end;
      end;

      // Check client connections
      List := FConnections.LockList;
      try
        for i := List.Count - 1 downto 0 do
        begin
          Connection := TClientConnection(List[i]);
          if Connection.Socket <> INVALID_SOCKET_VALUE then
          begin
            if FD_ISSET(Connection.Socket, ReadSet) then
              Connection.HandleData
            else if FD_ISSET(Connection.Socket, ErrorSet) then
              Connection.Disconnect;
          end;
        end;
      finally
        FConnections.UnlockList;
      end;
    end;

    // Periodic cleanup
    CleanupIdleConnections;
  end;
end;

{$ENDIF} // MSWINDOWS

{$IFDEF UNIX}

{ TUnixServerThread }

constructor TUnixServerThread.Create(AServer: TLightHttpServerUnix);
begin
  inherited Create(False);
  FServer := AServer;
  FreeOnTerminate := False;
end;

procedure TUnixServerThread.Execute;
begin
  FServer.ServerThreadExecute;
end;

{ TLightHttpServerUnix }

constructor TLightHttpServerUnix.Create;
begin
  inherited Create;
  FListenSocket := -1;
  FPipeFds[0] := -1;
  FPipeFds[1] := -1;
  {$IFDEF LINUX}
  FEpollFd := -1;
  {$ENDIF}
end;

destructor TLightHttpServerUnix.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TLightHttpServerUnix.InitializeEventSystem;
begin
  // Create pipe for shutdown signaling
  if fppipe(FPipeFds) <> 0 then
    raise Exception.Create('Failed to create pipe');

  {$IFDEF LINUX}
  // Create epoll instance
  FEpollFd := epoll_create1(0);
  if FEpollFd = -1 then
    raise Exception.Create('Failed to create epoll instance');
  {$ENDIF}
end;

procedure TLightHttpServerUnix.CleanupEventSystem;
begin
  {$IFDEF LINUX}
  if FEpollFd <> -1 then
  begin
    fpclose(FEpollFd);
    FEpollFd := -1;
  end;
  {$ENDIF}

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
end;

procedure TLightHttpServerUnix.DoStart;
var
  Addr: TSockAddr;
  OptVal: cint;
begin
  InitializeEventSystem;

  FListenSocket := fpsocket(AF_INET, SOCK_STREAM, 0);
  if FListenSocket = -1 then
    raise Exception.Create('Failed to create socket');

  // Set socket options
  OptVal := 1;
  fpsetsockopt(FListenSocket, SOL_SOCKET, SO_REUSEADDR, @OptVal, SizeOf(OptVal));

  // Make socket non-blocking
  fpfcntl(FListenSocket, F_SETFL, fpfcntl(FListenSocket, F_GETFL) or O_NONBLOCK);

  // Bind and listen
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_addr.s_addr := 0; // INADDR_ANY
  Addr.sin_port := htons(FPort);

  if fpbind(FListenSocket, @Addr, SizeOf(Addr)) <> 0 then
    raise Exception.CreateFmt('Failed to bind to port %d', [FPort]);

  if fplisten(FListenSocket, SOMAXCONN) <> 0 then
    raise Exception.Create('Failed to listen on socket');

  // Start server thread
  FServerThread := TUnixServerThread.Create(Self);
end;

procedure TLightHttpServerUnix.DoStop;
var
  SignalByte: Byte;
begin
  // Signal shutdown
  if FPipeFds[1] <> -1 then
  begin
    SignalByte := 1;
    fpwrite(FPipeFds[1], SignalByte, 1);
  end;

  if Assigned(FServerThread) then
  begin
    FServerThread.WaitFor;
    FreeAndNil(FServerThread);
  end;

  if FListenSocket <> -1 then
  begin
    fpclose(FListenSocket);
    FListenSocket := -1;
  end;

  CleanupEventSystem;
end;

procedure TLightHttpServerUnix.DoAcceptConnections;
begin
  // Implementation in ServerThreadExecute
end;

procedure TLightHttpServerUnix.ServerThreadExecute;
var
  ReadSet: TFDSet;
  Timeout: TTimeVal;
  ClientSocket: cint;
  Connection: TClientConnection;
  List: TList;
  i: Integer;
  MaxSocket: cint;
begin
  while not TThread.CurrentThread.CheckTerminated do
  begin
    fpFD_ZERO(ReadSet);

    // Add listen socket and shutdown pipe
    fpFD_SET(FListenSocket, ReadSet);
    fpFD_SET(FPipeFds[0], ReadSet);
    MaxSocket := Max(FListenSocket, FPipeFds[0]);

    // Add client sockets
    List := FConnections.LockList;
    try
      for i := 0 to List.Count - 1 do
      begin
        Connection := TClientConnection(List[i]);
        if Connection.Socket <> -1 then
        begin
          fpFD_SET(Connection.Socket, ReadSet);
          if Connection.Socket > MaxSocket then
            MaxSocket := Connection.Socket;
        end;
      end;
    finally
      FConnections.UnlockList;
    end;

    // Wait for activity
    Timeout.tv_sec := 1;
    Timeout.tv_usec := 0;

    if fpselect(MaxSocket + 1, @ReadSet, nil, nil, @Timeout) > 0 then
    begin
      // Check for shutdown signal
      if fpFD_ISSET(FPipeFds[0], ReadSet) <> 0 then
        Break;

      // Check for new connections
      if fpFD_ISSET(FListenSocket, ReadSet)  <> 0 then
      begin
        ClientSocket := fpaccept(FListenSocket, nil, nil);
        if ClientSocket <> -1 then
        begin
          // Make client socket non-blocking
          fpfcntl(ClientSocket, F_SETFL, fpfcntl(ClientSocket, F_GETFL) or O_NONBLOCK);

          Connection := TClientConnection.Create(ClientSocket, Self);
          AddConnection(Connection);
        end;
      end;

      // Check client connections
      List := FConnections.LockList;
      try
        for i := List.Count - 1 downto 0 do
        begin
          Connection := TClientConnection(List[i]);
          if (Connection.Socket <> -1) and (fpFD_ISSET(Connection.Socket, ReadSet)  <> 0) then
            Connection.HandleData;
        end;
      finally
        FConnections.UnlockList;
      end;
    end;

    // Periodic cleanup
    CleanupIdleConnections;
  end;
end;

{$ENDIF} // UNIX

// Factory function
function CreateHttpServer: TLightHttpServer;
begin
{$IFDEF MSWINDOWS}
  Result := TLightHttpServerWindows.Create;
{$ELSE}
  Result := TLightHttpServerUnix.Create;
{$ENDIF}
end;

end.

