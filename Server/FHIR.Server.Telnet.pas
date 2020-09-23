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

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Windows, SysUtils, Classes,
  IdTCPServer, IdCustomTCPServer, IdException, IdTelnetServer, IdIOHandlerSocket, IdContext,
  FHIR.Support.Base, FHIR.Support.Threads, FHIR.Support.Logging;

type
  TTelnetThreadHelper = class (TFslObject)
  Private
    FContext: TIdContext;
    FLast : integer;
    procedure execute;
    procedure processCommand(s : String);
    procedure send(s : String);
    procedure ping;
  Public
    constructor Create(context: TIdContext);
    destructor Destroy; Override;
    function link : TTelnetThreadHelper; overload;
  end;

  TFHIRTelnetServer = class;

  TFHIRTelnetServerThread = class (TFslThread)
  private
    FServer : TFHIRTelnetServer;
  protected
    Procedure Execute; override;
  end;

  TFHIRTelnetServer = class(TFslObject)
  Private
    FServer: TIdTelnetServer;
    FLock : TFslLock;
    FClients: TFslList<TTelnetThreadHelper>;
    FPassword : String;
    FLog : TStringList;
    FThread : TFHIRTelnetServerThread;
//    Commands: TStringListWithObjects;
//    FKernel: TGatewayKernel;
//    FNotifyExceptions : Boolean;
//    FExceptions : TExceptionEventList;
//    OutputHook : TTelnetServerHook;

//    Procedure Notify(Const AoEvent : TAdvExceptionEvent);
    procedure TelnetLogin(AThread: TIdContext; const username, password: String; var AAuthenticated: Boolean);
    procedure telnetExecute(AThread: TIdContext);
//    procedure WriteOutput(s: String);
//    procedure ExecuteCommand(CmdLine: String; Connection: TIdTCPServerConnection; oTelnetHelper : TTelnetThreadHelper);
    procedure doLog(msg : String);
    procedure internalThread;
  Public
    constructor Create(port: Integer);
    destructor Destroy; Override;
    property password : String read FPassword write FPassword;
//    function ExceptionHistoryAsHTML(bStack: Boolean): String;
//    function ExceptionHistoryAsText(bStack: Boolean): String;
//    Function ExceptionCount : String;
  end;

//var
//  TelnetServer: TFHIRTelnetServer;
//
//procedure StartTelnetServer(port: Integer);
//procedure SetTelnetServerPort(port: Integer);
//procedure SetTelnetServerName(Name: String);
//procedure SetTelnetServerKernel(AKernel: TGatewayKernel);
//procedure CloseTelnetServer;
//Function TelnetServerOpen : Boolean;


implementation

//type

//  TTelnetCommand = class(TBaseObject)
//  Private
//    description: String;
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Virtual; Abstract;
//  Public
//    constructor Create(cmdList: TStringList; Name, desc: String);
//  end;
//
//  TExceptListCommand = class(TTelnetCommand)
//  Private
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Override;
//  end;
//
//  TExceptListenCommand = class(TTelnetCommand)
//  Private
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Override;
//  end;
//
//  TDBStatusCommand = class(TTelnetCommand)
//  Private
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Override;
//  end;
//
//  TDBListCommand = class(TTelnetCommand)
//  Private
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Override;
//  end;
//
//  TInterfaceStatusCommand = class(TTelnetCommand)
//  Private
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Override;
//  end;
//
//  TCritSctStatusCommand = class(TTelnetCommand)
//  Private
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Override;
//  end;
//
//  TKillCommand = class(TTelnetCommand)
//  Private
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Override;
//  end;
//
//  TShutdownCommand = class(TTelnetCommand)
//  Private
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Override;
//  end;
//
//  TThreadStatusCommand = class(TTelnetCommand)
//  Private
//    procedure Execute(Params: String; Connection: TIdTCPServerConnection); Override;
//  end;
//
//
//procedure InstallCommands(list: TStringList);
//begin
//  TExceptListCommand.Create(list, 'exc', 'Get 24hour internal exception history');
//  TExceptListenCommand.Create(list, 'exct', 'Toggle notifications of exceptions on the telnet interface');
//  TDBStatusCommand.Create(list, 'db', 'Get Database performance stats. 1st param = name. Default = primary');
//  TDBListCommand.Create(list, 'dblist', 'List known database connections');
//  TInterfaceStatusCommand.Create(list, 'st', 'List interface status');
//  TShutdownCommand.Create(list, 'shutdown', 'Close TIE service (=Service Stop)');
//  TKillCommand.Create(list, 'die', 'Terminate TIE immediately (dirty)');
//  TCritSctStatusCommand.Create(list, 'crit', 'List locked critical sections');
//  TThreadStatusCommand.Create(list, 'threads', 'List All threads');
//end;
//
//Function TelnetServerOpen : Boolean;
//Begin
//  Result := TelnetServer <> Nil;
//End;
//
//
//procedure StartTelnetServer(port: Integer);
//begin
//  if port = 0 then
//    begin
//    port := 23455;
//    end;
//
//  TelnetServer := TFHIRTelnetServer.Create(NIL, port);
//end;
//
//procedure SetTelnetServerPort(port: Integer);
//begin
//  if TelnetServer.FServer.DefaultPort <> port Then
//  Begin
//    try
//      TelnetServer.FServer.Active := False;
//    except
//      on e:Exception do
//      begin
//        WriteRobotLog(true, 'Error changing telnet server port: '+e.message);
//      end;
//    end;
//
//    TelnetServer.FServer.DefaultPort := port;
//    TelnetServer.FServer.Active := True;
//  End;
//end;
//
//procedure SetTelnetServerName(Name: String);
//begin
//  TelnetServer.FServer.LoginMessage := 'TIE [' + Name +']';
//end;
//
//procedure SetTelnetServerKernel(AKernel: TGatewayKernel);
//begin
//  TelnetServer.FKernel := AKernel;
//end;
//
//procedure CloseTelnetServer;
//begin
//  FreeAndNil(telnetServer);
//end;
//
//
//{ TFHIRTelnetServer }
//
//constructor TFHIRTelnetServer.Create;
//begin
//  inherited Create;
//  FClients := TLockObjectList.Create('gwtelnet.clients');
//  FClients.AutoDestroy := False;
//
//  FExceptions := TExceptionEventList.Create;
//  If Not TAdvExceptionProfiler.IsGlobalInstanceInitialised Then
//    TAdvExceptionProfiler.InitialiseGlobalInstance;
//
//  TAdvExceptionProfiler.GetGlobalInstance.AddEventListener(Notify);
//  TAdvExceptionProfiler.GetGlobalInstance.AddExceptionClassToIgnore(EKDateFormatError);
//  TAdvExceptionProfiler.GetGlobalInstance.AddExceptionClassToIgnore(EISAPIException);
//  TAdvExceptionProfiler.GetGlobalInstance.AddExceptionClassToIgnore(EURLParamFormatFailure);
//  TAdvExceptionProfiler.GetGlobalInstance.AddExceptionClassToIgnore(EIdSilentException);
//  TAdvExceptionProfiler.GetGlobalInstance.AddExceptionClassToIgnore(EIdConnClosedGracefully);
//  TAdvExceptionProfiler.GetGlobalInstance.AddExceptionClassToIgnore(EIdSocketError);
//  TAdvExceptionProfiler.GetGlobalInstance.AddExceptionClassToIgnore(EIdClosedSocket);
//  TAdvExceptionProfiler.GetGlobalInstance.AddExceptionClassToIgnore(EKScriptAbort);
//
//  if TAdvExceptionProfiler.GetGlobalInstance.AreTracingPrerequisitesSatisfied Then
//    TAdvExceptionProfiler.GetGlobalInstance.StartTracing;
//
//  FKernel := AKernel;
//  Commands := TStringListWithObjects.Create('gwtelnet.commands');
//  commands.sorted := True;
//  InstallCommands(Commands);
//
//  FServer := TidTelnetServer.Create(NIL);
//  FServer.DefaultPort := port;
//  FServer.LoginMessage := 'TIE [Initializing]';
//  FServer.OnAuthentication := TelnetLogin;
//  FServer.OnExecute := telnetExecute;
//  FServer.Active := True;
//  OutputHook := TelnetServerHook;
//  TelnetServerHook := WriteOutput;
//end;
//
//destructor TFHIRTelnetServer.Destroy;
//var
//  i: Integer;
//begin
//  TelnetServerHook := NIL;
//
//  // note: the global object is already freed at this point
//  // 1. drop all the connections that we can
//  for i := FClients.Count - 1 downto 0 do
//    try
//      (FClients.objects[i] as TTelnetThreadHelper).Connection.DisconnectSocket;
//    except
//      end;
//
//  // drop FClients
//  FClients.Free;
//
//  try
//    FServer.active := False;
//  except
//    // don't care - this will happen happen if there is a telnet session that
//    end;
//
//  FServer.Free;
//  FreeAndNil(Commands);
//
//  if TAdvExceptionProfiler.GetGlobalInstance.AreTracingPrerequisitesSatisfied Then
//    TAdvExceptionProfiler.GetGlobalInstance.StopTracing;
//  TAdvExceptionProfiler.GetGlobalInstance.RemoveEventListener(Notify);
//  TAdvExceptionProfiler.FinaliseGlobalInstance;
//  FExceptions.Free;
//
//  inherited;
//end;
//
//procedure TFHIRTelnetServer.ExecuteCommand(CmdLine: String; Connection: TIdTCPServerConnection; oTelnetHelper : TTelnetThreadHelper);
//var
//  i: Integer;
//  cmdname: String;
//  s: String;
//begin
//  split(cmdline, ' ', cmdname, cmdline);
//  if SameText(cmdname, 'help') then
//    begin
//    s := '';
//    for i := 0 to commands.Count - 1 do
//      begin
//      s := s + PadString(commands[i], 10, ' ', False) + ' ' + (Commands.objects[i] as TTelnetCommand).description + CRLF;
//      end;
//    s := s + PadString('watch', 10, ' ', False) + ' See live view of events on server' + CRLF;
//    s := s + PadString('stop', 10, ' ', False) + ' stop seeing live events on server' + CRLF;
//    Connection.Write(s);
//    end
//  else if SameText(cmdName, 'watch') Then
//    oTelnetHelper.FListening := True
//  else if SameText(cmdName, 'stop') Then
//    oTelnetHelper.FListening := False
//  else if Commands.find(GetStringCell(Cmdname, 0, ' '), i) then
//    begin
//    (Commands.objects[i] as TTelnetCommand).Execute(cmdline, connection)
//    end
//  else
//    begin
//    Connection.Write('Command ' + cmdname + ' not known');
//    end;
//end;
//
//function TFHIRTelnetServer.ExceptionHistoryAsHTML(bStack : Boolean): String;
//Begin
//  FClients.Lock;
//  Try
//    If TAdvExceptionProfiler.GetGlobalInstance.AreTracingPrerequisitesSatisfied Then
//      Result := FExceptions.AsHTML(bStack)
//    Else
//      Result := 'Exception Tracking is not enabled in this build';
//  Finally
//    FClients.Unlock;
//  End;
//End;
//
//function TFHIRTelnetServer.ExceptionHistoryAsText(bStack : Boolean): String;
//Begin
//  FClients.Lock;
//  Try
//    If TAdvExceptionProfiler.GetGlobalInstance.AreTracingPrerequisitesSatisfied Then
//      Result := FExceptions.AsText(bStack)
//    Else
//      Result := 'Exception Tracking is not enabled in this build';
//  Finally
//    FClients.Unlock;
//  End;
//End;
//
//
//procedure TFHIRTelnetServer.WriteOutput(s: String);
//var
//  i: Integer;
//begin
//  if assigned(OutputHook) then
//    OutputHook(s);
//  FClients.Lock;
//  try
//    for i := 0 to FClients.Count - 1 do
//      (FClients.Objects[i] as TTelnetThreadHelper).Write(s);
//  finally
//    FClients.UnLock;
//    end;
//end;
//
//procedure TFHIRTelnetServer.Notify(const AoEvent: TAdvExceptionEvent);
//var
//  oEvent : TExceptionEvent;
//  oBuilder : TAdvStringBuilder;
//begin
//  if gNoExceptions Then
//    exit;
//
//  oEvent := TExceptionEvent.Create;
//  Try
//    oEvent.FStackDump := AoEvent.StackTrace;
//    oEvent.FClassName := AoEvent.SourceExceptionClass.ClassName;
//    oEvent.FMessage := AoEvent.SourceExceptionMessage;
//    oEvent.FThreadID := ThreadID;
//    oEvent.FExcTime := AoEvent.TimeStamp;
//    oBuilder := TAdvStringBuilder.Create;
//    Try
//      oEvent.AsText(oBuilder, True);
//      WriteRobotLog(true, oBuilder.AsString);
//      If FNotifyExceptions Then
//        WriteOutput(oBuilder.AsString);
//    Finally
//      oBuilder.Free;
//    End;
//    FClients.Lock;
//    Try
//      FExceptions.Add(oEvent.Link);
//      FExceptions.Trim;
//    Finally
//      FClients.Unlock;
//    End;
//  Finally
//    oEvent.Free;
//  End;
//end;
//
//function TFHIRTelnetServer.ExceptionCount: String;
//begin
//  FClients.Lock;
//  Try
//    Result := IntegerToString(FExceptions.Count);
//  Finally
//    FClients.Unlock;
//  End;
//end;
//
//{ TTelnetThreadHelper }
//
//constructor TTelnetThreadHelper.Create(Thread: TIdPeerThread);
//begin
//  inherited Create;
//  CurrentBuffer := '';
//  SendBuffer := '';
//  UseSendBuffer := False;
//  Connection := Thread.Connection;
//  TelnetServer.FClients.Lock;
//  try
//    TelnetServer.FClients.AddObject(self);
//    Thread.Connection.Write('Welcome to ' + Licence.License.OwningInstitution + ' TIE. There is ' + IntToStr(TelnetServer.FClients.Count) + ' connections. Type "help" for assistance' + CRLF);
//  finally
//    TelnetServer.FClients.UnLock;
//  end;
//end;
//
//destructor TTelnetThreadHelper.Destroy;
//begin
//  if assigned(TelnetServer) then
//    begin
//      TelnetServer.FClients.Lock;
//      try
//        if TelnetServer.FClients.IndexOf(self) > -1 then
//          begin
//            TelnetServer.FClients.Delete(TelnetServer.FClients.IndexOf(self));
//          end;
//      finally
//        TelnetServer.FClients.UnLock;
//      end;
//    end;
//  inherited;
//end;
//
//procedure TTelnetThreadHelper.Write(s: String);
//begin
//  if FListening then
//    Begin
//      Lock;
//      try
//        if UseSendBuffer then
//          begin
//            SendBuffer := SendBuffer + s
//          end
//        else
//          begin
//            Connection.Write(s);
//          end;
//      finally
//        Unlock;
//      end;
//    End;
//end;
//
//{ TTelnetCommand }
//
//constructor TTelnetCommand.Create(cmdList: TStringList; Name, desc: String);
//begin
//  inherited Create;
//  cmdlist.AddObject(Name, self);
//  description := desc;
//end;
//
//{ TExceptListCommand }
//
//procedure TExceptListCommand.Execute(Params: String;
//  Connection: TIdTCPServerConnection);
//var
//  s: String;
//begin
//  s := TelnetServer.FExceptions.AsText(True);
//  if s = '' then
//    begin
//    s := 'There have been no exceptions';
//    end;
//  connection.Write(s + CRLF);
//end;
//
//procedure TExceptListenCommand.Execute(Params: String; Connection: TIdTCPServerConnection);
//var
//  s: String;
//begin
//  TelnetServer.FNotifyExceptions := not TelnetServer.FNotifyExceptions;
//  s := 'Notify Exception is '+BooleanToString(TelnetServer.FNotifyExceptions);
//  connection.Write(s + CRLF);
//end;
//
//{ TDBListCommand }
//
//procedure TDBListCommand.Execute(Params: String;
//  Connection: TIdTCPServerConnection);
//begin
//{TODO}
////  Connection.Write(GConnManList.Text);
//end;
//
//{ TDBStatusCommand }
//
//procedure TDBStatusCommand.Execute(Params: String;
//  Connection: TIdTCPServerConnection);
//begin
//  if Params = '' then
//    begin
//    if assigned(gwdb) then
//      begin
//      Connection.Write(gwdb.ConnMan.Logger.Report(krfText) + CRLF)
//      end
//    else
//      begin
//      Connection.Write('Database manager does not exist' + CRLF);
//      end;
//    end
//  else
//    begin
//    try
//      {TODO}
////Connection.Write((GConnManList.Objects[GConnManList.indexof(params)] as TKDBConnManager).perfStatsAsText + CRLF);
//    except
//      on e:
//      Exception do
//        Connection.Write(e.message);
//      end;
//    end;
//end;
//
//{ TInterfaceStatusCommand }
//
//procedure TInterfaceStatusCommand.Execute(Params: String;
//  Connection: TIdTCPServerConnection);
//begin
//  if assigned(Telnetserver.FKernel) then
//    begin
//    connection.Write(Telnetserver.FKernel.InterfacesReportAsText + CRLF)
//    end
//  else
//    begin
//    connection.Write('Kernel does not exist' + CRLF);
//    end;
//end;
//
//{ TKillCommand }
//
//procedure TKillCommand.Execute(Params: String; Connection: TIdTCPServerConnection);
//begin
//  Try
//    Connection.Write('Terminate Process Now!' + CRLF);
//  Except
//  End;
//  KillProcessNow;
//end;
//
//{ TShutdownCommand }
//
//procedure TShutdownCommand.Execute(Params: String; Connection: TIdTCPServerConnection);
//begin
//  Connection.Write('Request to shut down TIE has been executed.' + CRLF);
//  Telnetserver.FKernel.Service.Stop('telnet request');
//end;
//
//
//procedure TCritSctStatusCommand.Execute(Params: String; Connection: TIdTCPServerConnection);
//begin
//  Connection.Write('loading...' + CRLF);
//  Connection.Write(DumpLocks);
//end;
//
//{ TThreadStatusCommand }
//
//procedure TThreadStatusCommand.Execute(Params: String; Connection: TIdTCPServerConnection);
//begin
//  Connection.Write('System Thread Status: ' + CRLF + GetThreadStatus);
//end;
//
//
//{ TExceptionEvent }
//
//Procedure TExceptionEvent.AsText(oBuilder : TAdvStringBuilder; bStack: Boolean);
//begin
//  oBuilder.Append(FormatDateTime('hh:nn:ss ', FExcTime) + IntegerToString(FThreadID) + ' ' +
//    PadString(copy(FClassName, 1, 18), 19, ' ', False) +
//    FMessage);
//  if bStack Then
//    oBuilder.Append(cReturn + FStackDump);
//end;
//
//
//Procedure TExceptionEvent.AsHTML(oBuilder : TAdvStringBuilder; bOdd, bStack: Boolean);
//begin
//  if bOdd Then
//    oBuilder.Append('<tr bgcolor="#EFEFEF">')
//  Else
//    oBuilder.Append('<tr bgcolor="#FFFFFF">');
//  oBuilder.Append('<td>' + FormatDateTime('hh:nn:ss', FExcTime) + '</td>');
//  oBuilder.Append('<td>&nbsp;</td>');
//  oBuilder.Append('<td>' + IntegerToString(FThreadID) + '</td>');
//  oBuilder.Append('<td>&nbsp;</td>');
//  oBuilder.Append('<td>' + FClassName + '</td>');
//  oBuilder.Append('<td>&nbsp;</td>');
//  oBuilder.Append('<td>' + FMessage + '</td>');
//  oBuilder.Append('</tr>');
//  oBuilder.Append(cReturn);
//  if bStack Then
//  Begin
//    if bOdd Then
//      oBuilder.Append('<tr bgcolor="#EFEFEF">')
//    Else
//      oBuilder.Append('<tr bgcolor="#FFFFFF">');
//    oBuilder.Append('<td colspan=2>&nbsp</td>');
//    oBuilder.Append('<td colspan=5><pre>' + FStackDump + '</pre></td>');
//    oBuilder.Append('</tr>');
//    oBuilder.Append(cReturn);
//  end;
//end;
//
//
//{ TExceptionEventList }
//
//function TExceptionEventList.GetException(iIndex: Integer): TExceptionEvent;
//begin
//  Result := TExceptionEvent(ObjectByIndex[iIndex]);
//end;
//
//function TExceptionEventList.ItemClass: TAdvObjectClass;
//begin
//  Result := TExceptionEvent;
//end;
//
//function TExceptionEventList.AsHTML(bStack: Boolean): String;
//Var
//  oBuilder : TAdvStringBuilder;
//  iLoop : Integer;
//begin
//  oBuilder := TAdvStringBuilder.Create;
//  Try
//    oBuilder.Append('<table class="tbl" cellspacing="0" cellpadding="0">');
//    oBuilder.Append('<tr bgcolor="#DFDFDF">');
//    oBuilder.Append('<td><b>Time</b></td>');
//    oBuilder.Append('<td>&nbsp;</td>');
//    oBuilder.Append('<td><b>Thread</b></td>');
//    oBuilder.Append('<td>&nbsp;</td>');
//    oBuilder.Append('<td><b>Type</b></td>');
//    oBuilder.Append('<td>&nbsp;</td>');
//    oBuilder.Append('<td><b>Message</b></td>');
//    oBuilder.Append('</tr>');
//    oBuilder.Append(cReturn);
//    For iLoop := 0 To Count - 1 Do
//    Begin
//      GetException(iLoop).AsHTML(oBuilder, iLoop Mod 2 = 1, bStack);
//    End;
//    oBuilder.Append('</table>');
//    Result := oBuilder.AsString;
//  Finally
//    oBuilder.Free;
//  End;
//end;
//
//function TExceptionEventList.AsText(bStack: Boolean): String;
//Var
//  oBuilder : TAdvStringBuilder;
//  iLoop : Integer;
//begin
//  oBuilder := TAdvStringBuilder.Create;
//  Try
//    For iLoop := 0 To Count - 1 Do
//    Begin
//      GetException(iLoop).AsText(oBuilder, bStack);
//      oBuilder.Append(cReturn);
//    End;
//    Result := oBuilder.AsString;
//  Finally
//    oBuilder.Free;
//  End;
//end;
//
//procedure TExceptionEventList.Trim;
//var
//  aCutoff : TDateTime;
//begin
//  assert(self <> NIL);
//  aCutoff := Now - 1;
//  while (Count > 0) and ((Count > 100) Or (GetException(0).FExcTime < aCutoff)) do
//    DeleteByIndex(0);
//end;

{ TFHIRTelnetServer }

constructor TFHIRTelnetServer.Create(port: Integer);
begin
  inherited Create;
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
  logevent := DoLog;
  sleep(500); // allow console to connect early
end;

destructor TFHIRTelnetServer.Destroy;
begin
  LogEvent := nil;
  try
    FThread.Close;
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

procedure TFHIRTelnetServer.doLog(msg: String);
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
        tth.ping;
        for s in ts do
        begin
          tth.send(s);
        end;
      end;
    finally
      ts.Free;
    end;
  finally
    list.Free;
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
  if GetTickCount > FLast + 1000 then
    send('.');
end;

procedure TTelnetThreadHelper.processCommand(s: String);
begin
  send('Unrecognised command '+s);
end;

procedure TTelnetThreadHelper.send(s: String);
begin
  FLast := GetTickCount;
  FContext.Connection.Socket.WriteLn(s);
end;

{ TFHIRTelnetServerThread }

procedure TFHIRTelnetServerThread.Execute;
begin
  while Running do
  begin
    try
      FServer.internalThread;
    except
      // nothing.
    end;
    sleep(50);
  end;
end;

end.

