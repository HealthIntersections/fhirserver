unit v2_protocol;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)

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

{
This unit uses indy 10 and implements the HL7 v2 Minimal Lower Layer Protocol.

In Indy9, it was published as TIdHL7, but then it fell out of Indy10,
so republished here
}

interface

Uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  Classes, Contnrs, SyncObjs, SysUtils,
  IdContext, IdBaseComponent, IdException, IdGlobal, IdStackConsts, IdIOHandlerSocket, IdTCPClient, IdTCPConnection, IdTCPServer,
  fsl_threads;

Const
  MSG_START : AnsiString = #11;
  MSG_END : AnsiString = #28#13;
  BUFFER_SIZE_LIMIT = $FFFFFFF;  // buffer is allowed to grow to this size without any valid messages. Will be truncated with no notice (DoS protection) (268MB)
  WAIT_STOP = 5000; // nhow long we wait for things to shut down cleanly

Type
  EHL7CommunicationError = Class(EIdException)
  Protected
    FInterfaceName: String;
  Public
    Constructor Create(AnInterfaceName, AMessage: String);
    Property InterfaceName: String Read FInterfaceName;
  End;

  THL7CommunicationMode = (cmUnknown,        // not valid - default setting must be changed by application
    cmAsynchronous,   // see comments below for meanings of the other parameters
    cmSynchronous,
    cmSingleThread);

  TSendResponse = (srNone,          // internal use only - never returned
    srError,         // internal use only - never returned
    srNoConnection,  // you tried to send but there was no connection
    srSent,          // you asked to send without waiting, and it has been done
    srOK,            // sent ok, and response returned
    srTimeout);      // we sent but there was no response (connection will be dropped internally

  Tv2ProtocolStatus = (isStopped,       // not doing anything
    isNotConnected,  // not Connected (Server state)
    isConnecting,    // Client is attempting to connect
    isWaitReconnect, // Client is in delay loop prior to attempting to connect
    isConnected,     // connected OK
    isTimedOut,      // we are a client, and there was no traffic, we we closed the connection (and we are not listening)
    isUnusable       // Not Usable - stop failed
    );

Const
  { default property values }
  DEFAULT_ADDRESS = '';
  DEFAULT_PORT = 0;
  DEFAULT_TIMEOUT = 30000;
  DEFAULT_RECEIVE_TIMEOUT = 30000;
  NULL_IP = '0.0.0.0';
  DEFAULT_CONN_LIMIT = 1;
  DEFAULT_RECONNECT_DELAY = 15000;
  DEFAULT_CONNECTION_TIMEOUT = 0;
  DEFAULT_COMM_MODE = cmUnknown;
  DEFAULT_IS_LISTENER = True;
  MILLISECOND_LENGTH = (1 / (24 * 60 * 60 * 1000));
  SEND_RESPONSE_NAMES : Array [TSendResponse] Of String = ('None', 'Error', 'NoConnection', 'Sent', 'OK', 'Timeout');

Type
  // the connection is provided in these events so that applications can obtain information about the
  // the peer. It's never OK to write to these connections
  TMessageArriveEvent = Procedure(ASender: TObject; AConnection: TIdTCPConnection; AMsg: TBytes) Of Object;
  TMessageReceiveEvent = Procedure(ASender: TObject; AConnection: TIdTCPConnection; AMsg: TBytes; Var VHandled: Boolean; Var VReply: TBytes) Of Object;
  TReceiveErrorEvent = Procedure(ASender: TObject; AConnection: TIdTCPConnection; AMsg: TBytes; AException: Exception; Var VReply: TBytes; Var VDropConnection: Boolean) Of Object;

  Tv2Protocol = Class;
  Tv2ProtocolConnCountEvent = Procedure(ASender: Tv2Protocol; AConnCount: Integer) Of Object;

  TIdPeerThread = TIdContext;

  Tv2ProtocolClientThread = Class(TThread)
  Protected
    FClient: TIdTCPClient;
    FCloseEvent: TIdLocalEvent;
    FOwner: Tv2Protocol;
    FLastTraffic : TDateTime;
    Function TimedOut : Boolean;
    Procedure Execute; Override;
    Procedure PollStack;
  Public
    Constructor Create(aOwner: Tv2Protocol);
    Destructor Destroy; Override;
  End;

  Tv2Protocol = Class(TIdBaseComponent)
  Private
    FConnectionTimeout: Cardinal;
    FKeepAlive: Boolean;
  Protected
    FLock: TCriticalSection;
    FStatus: Tv2ProtocolStatus;
    FStatusDesc: String;

    // these queues hold messages when running in singlethread mode
    FMsgQueue: TList;
    FHndMsgQueue: TList;

    FAddress: String;
    FCommunicationMode: THL7CommunicationMode;
    FConnectionLimit: Word;
    FIPMask: String;
    FIPRestriction: String;
    FIPMaskVal : Cardinal;
    FIPRestrictionVal : Cardinal;

    FIsListener: Boolean;
    FObject: TObject;
    FPreStopped: Boolean;
    FPort: Word;
    FReconnectDelay: Cardinal;
    FTimeOut: Cardinal;
    FReceiveTimeout: Cardinal;
    FServerConnections : TObjectList;

    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnConnCountChange: Tv2ProtocolConnCountEvent;
    FOnMessageArrive: TMessageArriveEvent;
    FOnReceiveMessage: TMessageReceiveEvent;
    FOnReceiveError: TReceiveErrorEvent;

    FIsServer: Boolean;
    // current connection count (server only) (can only exceed 1 when mode is not
    // asynchronous and we are listening)
    FConnCount: Integer;
    FServer: TIdTCPServer;
    // if we are a server, and the mode is not asynchronous, and we are not listening, then
    // we will track the current server connection with this, so we can initiate sending on it

    FServerConn: TIdTCPConnection;

    // A thread exists to connect and receive incoming tcp traffic
    FClientThread: Tv2ProtocolClientThread;
    FClient: TIdTCPClient;

    // these fields are used for handling message response in synchronous mode
    FWaitingForAnswer: Boolean;
    FWaitStop: TDateTime;
    FMsgReply: TBytes;
    FReplyResponse: TSendResponse;
    FWaitEvent: TIdLocalEvent;

    Procedure SetAddress(Const AValue: String);
    procedure SetKeepAlive(const Value: Boolean);
    Procedure SetConnectionLimit(Const AValue: Word);
    Procedure SetIPMask(Const AValue: String);
    Procedure SetIPRestriction(Const AValue: String);
    Procedure SetPort(Const AValue: Word);
    Procedure SetReconnectDelay(Const AValue: Cardinal);
    Procedure SetConnectionTimeout(Const AValue: Cardinal);
    Procedure SetTimeOut(Const AValue: Cardinal);
    Procedure SetCommunicationMode(Const AValue: THL7CommunicationMode);
    Procedure SetIsListener(Const AValue: Boolean);
    Function GetStatus: Tv2ProtocolStatus;
    Function GetStatusDesc: String;

    Procedure InternalSetStatus(Const AStatus: Tv2ProtocolStatus; ADesc: String);

    Procedure CheckServerParameters;
    Procedure StartServer;
    Procedure StopServer;
    Procedure DropServerConnection;
    Procedure ServerConnect(AThread: TIdPeerThread);
    Procedure ServerExecute(AThread: TIdPeerThread);
    Procedure ServerDisconnect(AThread: TIdPeerThread);

    Procedure CheckClientParameters;
    Procedure StartClient;
    Procedure StopClient;
    Procedure DropClientConnection;
    Procedure ReConnectFromTimeout;

    Procedure HandleIncoming(Var VBuf: TBytes; var len : integer; AConnection: TIdTCPConnection);
    Function HandleMessage(Const AMsg: TBytes; AConn: TIdTCPConnection; Var VReply: TBytes): Boolean;
  Public
    Procedure InitComponent; Override;

    Destructor Destroy; Override;

    Procedure EnforceWaitReplyTimeout;

    Function Going: Boolean;

    // for the app to use to hold any related object
    Property ObjTag: TObject Read FObject Write FObject;

    // status
    Property Status: Tv2ProtocolStatus Read GetStatus;
    Property StatusDesc: String Read GetStatusDesc;
    Function Connected: Boolean;

    Property IsServer: Boolean Read FIsServer;
    Procedure Start;
    Procedure PreStop; // call this in advance to start the shut down process. You do not need to call this
    Procedure Stop;

    Procedure WaitForConnection(AMaxLength: Integer); // milliseconds

    function ConvertIPtoCardinal(const AStr: String): Cardinal;

    // asynchronous.
    Function AsynchronousSend(AMsg: TBytes): TSendResponse;
    Property OnMessageArrive: TMessageArriveEvent Read FOnMessageArrive Write FOnMessageArrive;

    // synchronous
    Function SynchronousSend(AMsg: TBytes; Var VReply: TBytes): TSendResponse;
    Property OnReceiveMessage: TMessageReceiveEvent Read FOnReceiveMessage Write FOnReceiveMessage;
    Procedure CheckSynchronousSendResult(AResult: TSendResponse; AMsg: String);

    // single thread - like SynchronousSend, but don't hold the thread waiting
    Procedure SendMessage(AMsg: TBytes);
    // you can't call SendMessage again without calling GetReply first
    Function GetReply(Var VReply: TBytes): TSendResponse;
    Function GetMessage(Var VMsg: TBytes): pointer;  // return nil if no messages
    // if you don't call SendReply then no reply will be sent.
    Procedure SendReply(AMsgHnd: pointer; AReply: TBytes);

    Function HasClientConnection : Boolean;
    Procedure Disconnect;
  Published
    // basic properties
    Property Address: String Read FAddress Write SetAddress;  // leave blank and we will be server
    Property Port: Word Read FPort Write SetPort Default DEFAULT_PORT;

    Property KeepAlive : Boolean read FKeepAlive write SetKeepAlive;

    // milliseconds - message timeout - how long we wait for other system to reply
    Property TimeOut: Cardinal Read FTimeOut Write SetTimeOut Default DEFAULT_TIMEOUT;

    // milliseconds - message timeout. When running cmSingleThread, how long we wait for the application to process an incoming message before giving up
    Property ReceiveTimeout: Cardinal Read FReceiveTimeout Write FReceiveTimeout Default DEFAULT_RECEIVE_TIMEOUT;

    // server properties
    Property ConnectionLimit: Word Read FConnectionLimit Write SetConnectionLimit Default DEFAULT_CONN_LIMIT; // ignored if isListener is false
    Property IPRestriction: String Read FIPRestriction Write SetIPRestriction;
    Property IPMask: String Read FIPMask Write SetIPMask;

    // client properties

    // milliseconds - how long we wait after losing connection to retry
    Property ReconnectDelay: Cardinal Read FReconnectDelay Write SetReconnectDelay Default DEFAULT_RECONNECT_DELAY;

    // milliseconds - how long we allow a connection to be open without traffic (damn firewalls)
    Property ConnectionTimeout : Cardinal Read FConnectionTimeout Write SetConnectionTimeout Default DEFAULT_CONNECTION_TIMEOUT;
    // message flow

    // Set this to one of 4 possibilities:
    //
    //    cmUnknown
    //       Default at start up. You must set a value before starting
    //
    //    cmAsynchronous
    //        Send Messages with AsynchronousSend. does not wait for
    //                   remote side to respond before returning
    //        Receive Messages with OnMessageArrive. Message may
    //                   be response or new message
    //       The application is responsible for responding to the remote
    //       application and dropping the link as required
    //       You must hook the OnMessageArrive Event before setting this mode
    //       The property IsListener has no meaning in this mode
    //
    //   cmSynchronous
    //       Send Messages with SynchronousSend. Remote applications response
    //                   will be returned (or timeout). Only use if IsListener is false
    //       Receive Messages with OnReceiveMessage. Only if IsListener is
    //                   true
    //       In this mode, the object will wait for a response when sending,
    //       and expects the application to reply when a message arrives.
    //       In this mode, the interface can either be the listener or the
    //       initiator but not both. IsListener controls which one.
    //       note that OnReceiveMessage must be thread safe if you allow
    //       more than one connection to a server
    //
    //   cmSingleThread
    //       Send Messages with SendMessage. Poll for answer using GetReply.
    //                   Only if isListener is false
    //       Receive Messages using GetMessage. Return a response using
    //                   SendReply. Only if IsListener is true
    //       This mode is the same as cmSynchronous, but the application is
    //       assumed to be single threaded. The application must poll to
    //       find out what is happening rather than being informed using
    //       an event in a different thread

    Property CommunicationMode: THL7CommunicationMode Read FCommunicationMode Write SetCommunicationMode Default DEFAULT_COMM_MODE;

    // note that IsListener is not related to which end is client. Either end
    // may make the connection, and thereafter only one end will be the initiator
    // and one end will be the listener. Generally it is recommended that the
    // listener be the server. If the client is listening, network conditions
    // may lead to a state where the client has a phantom connection and it will
    // never find out since it doesn't initiate traffic. In this case, restart
    // the interface if there isn't traffic for a period
    Property IsListener: Boolean Read FIsListener Write SetIsListener Default DEFAULT_IS_LISTENER;

    // useful for application
    Property OnConnect: TNotifyEvent Read FOnConnect Write FOnConnect;
    Property OnDisconnect: TNotifyEvent Read FOnDisconnect Write FOnDisconnect;
    // this is called whenever OnConnect and OnDisconnect are called, and at other times, but only when server
    // it will be called after OnConnect and before OnDisconnect
    Property OnConnCountChange: Tv2ProtocolConnCountEvent Read FOnConnCountChange Write FOnConnCountChange;

    // this is called when an unhandled exception is generated by the
    // hl7 object or the application. It allows the application to
    // construct a useful return error, log the exception, and drop the
    // connection if it wants
    Property OnReceiveError: TReceiveErrorEvent Read FOnReceiveError Write FOnReceiveError;
  End;

Implementation

//Uses
//  IdResourceStrings;

ResourceString
  {HL7 Lower Layer Protocol Messages}
  RSHL7StatusStopped           = 'Stopped';
  RSHL7StatusNotConnected      = 'Not Connected';
  RSHL7StatusFailedToStart     = 'Failed to Start: %s';
  RSHL7StatusFailedToStop      = 'Failed to Stop: %s';
  RSHL7StatusConnected         = 'Connected';
  RSHL7StatusConnecting        = 'Connecting';
  RSHL7StatusReConnect         = 'Reconnect in %s: %s';
  RSHL7StatusTimedOut          = 'Not Connected - Timed out, waiting for a message';
  RSHL7NotWhileWorking         = 'You cannot set %s while the HL7 Component is working';
  RSHL7NotWorking              = 'Attempt to %s while the HL7 Component is not working';
  RSHL7NotFailedToStop         = 'Interface is unusable due to failure to stop';
  RSHL7AlreadyStarted          = 'Interface was already started';
  RSHL7AlreadyStopped          = 'Interface was already stopped';
  RSHL7ModeNotSet              = 'Mode is not initialised';
  RSHL7NoAsynEvent             = 'Component is in Asynchronous mode but OnMessageArrive has not been hooked';
  RSHL7NoSynEvent              = 'Component is in Synchronous mode but  OnMessageReceive has not been hooked';
  RSHL7InvalidPort             = 'Assigned Port value %d is invalid';
  RSHL7ImpossibleMessage       = 'A message has been received but the commication mode is unknown';
  RSHL7UnexpectedMessage       = 'Unexpected message arrived to an interface that is not listening';
  RSHL7UnknownMode             = 'Unknown mode';
  RSHL7ClientThreadNotStopped  = 'Unable to stop client thread';
  RSHL7SendMessage             = 'Send a message';
  RSHL7NoConnectionFound       = 'Server Connection not locatable when sending message';
  RSHL7WaitForAnswer           = 'You cannot send a message while you are still waiting for an answer';

Type
  TIdQueuedMessage = Class(TInterfacedObject)
  Private
    FEvent: TIdLocalEvent;
    FMsg: TBytes;
    FTimeOut: Cardinal;
    FReply: TBytes;
    Procedure Wait;
  Public
    Constructor Create(aMsg: TBytes; ATimeOut: Cardinal);
    Destructor Destroy; Override;
    Function _AddRef: Integer; Stdcall;
    Function _Release: Integer; Stdcall;
  End;

  { TIdQueuedMessage }

Constructor TIdQueuedMessage.Create(aMsg: TBytes; ATimeOut: Cardinal);
Begin
  Assert(Length(aMsg) > 0, 'Attempt to queue an empty message');
  Assert(ATimeout <> 0, 'Attempt to queue a message with a 0 timeout');
  Inherited Create;
  FEvent := TIdLocalEvent.Create(False, False);
  FMsg := aMsg;
  FTimeOut := ATimeOut;
End;

Destructor TIdQueuedMessage.Destroy;
Begin
  Assert(Self <> Nil);
  FreeAndNil(FEvent);
  Inherited;
End;

Procedure TIdQueuedMessage.Wait;
Begin
  Assert(Self <> Nil);
  Assert(Assigned(FEvent));
  FEvent.WaitFor(FTimeOut);
End;

Function TIdQueuedMessage._AddRef: Integer;
Begin
  Result := Inherited _AddRef;
End;

Function TIdQueuedMessage._Release: Integer;
Begin
  Result := Inherited _Release;
End;

{ EHL7CommunicationError }

Constructor EHL7CommunicationError.Create(AnInterfaceName, AMessage: String);
Begin
  //  assert(AInterfaceName <> '', 'Attempt to create an exception for an unnamed interface')
  //  assert(AMessage <> '', 'Attempt to create an exception with an empty message')
  //  actually, we do not enforce either of these conditions, though they should both be true,
  //  since we are already raising an exception
  FInterfaceName := AnInterfaceName;
  If FInterfaceName <> '' Then
    Begin
    Inherited Create('[' + AnInterfaceName + '] ' + AMessage)
    End
  Else
    Begin
    Inherited Create(AMessage);
    End
End;

{ Tv2Protocol }

Procedure Tv2Protocol.InitComponent;
Begin
  inherited;
  // partly redundant initialization of properties

  FIsListener := DEFAULT_IS_LISTENER;
  FCommunicationMode := DEFAULT_COMM_MODE;
  FTimeOut := DEFAULT_TIMEOUT;
  FReconnectDelay := DEFAULT_RECONNECT_DELAY;
  FReceiveTimeout := DEFAULT_RECEIVE_TIMEOUT;
  FConnectionLimit := DEFAULT_CONN_LIMIT;
  FIPMask := NULL_IP;
  FIPRestriction := NULL_IP;
  FAddress := DEFAULT_ADDRESS;
  FPort := DEFAULT_PORT;
  FOnReceiveMessage := Nil;
  FOnConnect := Nil;
  FOnDisconnect := Nil;
  FObject := Nil;

  // initialise status
  FStatus := IsStopped;
  FStatusDesc := RSHL7StatusStopped;

  // build internal infrastructure
  Flock := TCriticalSection.Create;
  FConnCount := 0;
  FServer := Nil;
// todo  FServerConn := Nil;
  FClientThread := Nil;
  FClient := Nil;
  FMsgQueue := TList.Create;
  FHndMsgQueue := TList.Create;
  FWaitingForAnswer := False;
  SetLength(FMsgReply, 0);
  FReplyResponse := srNone;
  FWaitEvent := TIdLocalEvent.Create(False, False);
  FServerConnections := TObjectList.Create;
  FServerConnections.OwnsObjects := False;
End;

Destructor Tv2Protocol.Destroy;
Begin
  Try
    If Going Then
      Begin
      Stop;
      End;
  Finally
    FreeAndNil(FServerConnections);
    FreeAndNil(FMsgQueue);
    FreeAndNil(FHndMsgQueue);
    FreeAndNil(FWaitEvent);
    FreeAndNil(FLock);
    Inherited;
    End;
End;

function Tv2Protocol.ConvertIPtoCardinal(const AStr: String): Cardinal;
var
  LArray: array [1..4] of Byte;
  LSeg, i, LLen: Word;
begin
  if aStr = '' Then
    result := 0
  Else
  Begin
    FillChar(LArray, 4, #0);
    LSeg := 1;
    i := 1;
    LLen := Length(AStr);
    while (i <= LLen) do
      begin
      if AStr[i] = '.' then
      begin
        inc(LSeg);
        if lSeg > 4 Then
          raise EHL7CommunicationError.create(Name, 'The value "'+aStr+'" is not a valid IP Address');
      end
      else if CharInSet(AStr[i], ['0'..'9']) Then
        LArray[LSeg] := (LArray[LSeg] shl 3) + (LArray[LSeg] shl 1) + Ord(AStr[i]) - Ord('0')
      Else
        raise EHL7CommunicationError.create(Name, 'The value "'+aStr+'" is not a valid IP Address');
      inc(i);
      end;
    Result := LArray[1] shl 24 + LArray[2] shl 16 + LArray[3] shl 8 + LArray[4];
  End;
end;

{==========================================================
  Property Servers
 ==========================================================}

Procedure Tv2Protocol.SetAddress(Const AValue: String);
Begin
  // we don't make any assertions about AValue - will be '' if we are a server
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['Address']));   {do not localize??}
    End;
  FAddress := AValue;
End;

Procedure Tv2Protocol.SetConnectionLimit(Const AValue: Word);
Begin
  // no restrictions on AValue
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['ConnectionLimit'])); {do not localize??}
    End;
  FConnectionLimit := AValue;
End;

Procedure Tv2Protocol.SetIPMask(Const AValue: String);
Begin
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['IP Mask']));  {do not localize??}
    End;
  FIPMaskVal := ConvertIPtoCardinal(AValue);
  FIPMask := AValue;
End;

Procedure Tv2Protocol.SetIPRestriction(Const AValue: String);
Begin
  // to do: enforce that AValue is a valid IP address range
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['IP Restriction']));    {do not localize??}
    End;
  FIPRestrictionVal := ConvertIPtoCardinal(AValue);
  FIPRestriction := AValue;
End;

Procedure Tv2Protocol.SetPort(Const AValue: Word);
Begin
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['Port']));          {do not localize??}
    End;
  FPort := AValue;
End;

Procedure Tv2Protocol.SetReconnectDelay(Const AValue: Cardinal);
Begin
  // any value for AValue is accepted, although this may not make sense
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['Reconnect Delay']));   {do not localize??}
    End;
  FReconnectDelay := AValue;
End;

Procedure Tv2Protocol.SetTimeOut(Const AValue: Cardinal);
Begin
  Assert(FTimeout > 0, 'Attempt to configure Tv2Protocol with a Timeout of 0');
  // we don't fucntion at all if timeout is 0, though there is circumstances where it's not relevent
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['Time Out']));          {do not localize??}
    End;
  FTimeOut := AValue;
End;

Procedure Tv2Protocol.SetCommunicationMode(Const AValue: THL7CommunicationMode);
Begin
  Assert((AValue >= Low(THL7CommunicationMode)) And (AValue <= High(THL7CommunicationMode)), 'Value for Tv2Protocol.CommunicationMode not in range');
  // only could arise if someone is typecasting?
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['Communication Mode'])); {do not localize??}
    End;
  FCommunicationMode := AValue;
End;

Procedure Tv2Protocol.SetIsListener(Const AValue: Boolean);
Begin
  // AValue isn't checked
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['IsListener']));         {do not localize??}
    End;
  FIsListener := AValue;
End;

Function Tv2Protocol.GetStatus: Tv2ProtocolStatus;
Begin
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    Result := FStatus;
  Finally
    FLock.Leave;
    End;
End;

Function Tv2Protocol.Connected: Boolean;
Begin
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    Result := FStatus = IsConnected;
  Finally
    FLock.Leave;
    End;
End;

Function Tv2Protocol.GetStatusDesc: String;
Begin
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    Result := FStatusDesc;
  Finally
    FLock.Leave;
    End;
End;

Procedure Tv2Protocol.InternalSetStatus(Const AStatus: Tv2ProtocolStatus; ADesc: String);
Begin
  Assert((AStatus >= Low(Tv2ProtocolStatus)) And (AStatus <= High(Tv2ProtocolStatus)), 'Value for Tv2Protocol.CommunicationMode not in range');
  // ADesc is allowed to be anything at all
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    FStatus := AStatus;
    FStatusDesc := ADesc;
  Finally
    FLock.Leave;
    End;
End;

{==========================================================
  Application Control
 ==========================================================}

Procedure Tv2Protocol.Start;
Var
  LStatus: Tv2ProtocolStatus;
Begin
  LStatus := GetStatus;
  If LStatus = IsUnusable Then
    Begin
    Raise EHL7CommunicationError.Create(Name, RSHL7NotFailedToStop);
    End;
  If LStatus <> IsStopped Then
    Begin
    Raise EHL7CommunicationError.Create(Name, RSHL7AlreadyStarted);
    End;
  If FCommunicationMode = cmUnknown Then
    Begin
    Raise EHL7CommunicationError.Create(Name, RSHL7ModeNotSet);
    End;
  If FCommunicationMode = cmAsynchronous Then
    Begin
    If Not Assigned(FOnMessageArrive) Then
      Begin
      Raise EHL7CommunicationError.Create(Name, RSHL7NoAsynEvent);
      End;
    End;
  If (FCommunicationMode = cmSynchronous) And IsListener Then
    Begin
    If Not Assigned(FOnReceiveMessage) Then
      Begin
      Raise EHL7CommunicationError.Create(Name, RSHL7NoSynEvent);
      End;
    End;
  FIsServer := (FAddress = '');
  If FIsServer Then
    Begin
    StartServer
    End
  Else
    Begin
    StartClient;
    End;
  FPreStopped := False;
  FWaitingForAnswer := False;
End;

Procedure Tv2Protocol.PreStop;
  Procedure JoltList(l: TList);
  Var
    i: Integer;
    Begin
    For i := 0 To l.Count - 1 Do
      Begin
      TIdQueuedMessage(l[i]).FEvent.SetEvent;
      End;
    End;
Begin
  If FCommunicationMode = cmSingleThread Then
    Begin
    Assert(Assigned(FLock));
    Assert(Assigned(FMsgQueue));
    Assert(Assigned(FHndMsgQueue));
    FLock.Enter;
    Try
      JoltList(FMsgQueue);
      JoltList(FHndMsgQueue);
    Finally
      FLock.Leave;
      End;
    End
  Else If FCommunicationMode = cmSynchronous Then
    Begin
    FWaitEvent.SetEvent;
    End;
  FPreStopped := True;
End;

Procedure Tv2Protocol.Stop;
Begin
  If Not Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, RSHL7AlreadyStopped);
    End;

  If Not FPreStopped Then
    Begin
    PreStop;
    sleep(10); // give other threads a chance to clean up
    End;

  If FIsServer Then
    Begin
    StopServer
    End
  Else
    Begin
    StopClient;
    End;
End;

{==========================================================
  Server Connection Maintainance
 ==========================================================}

Procedure Tv2Protocol.EnforceWaitReplyTimeout;
Begin
  Stop;
  Start;
End;

Function Tv2Protocol.Going: Boolean;
Var
  LStatus: Tv2ProtocolStatus;
Begin
  LStatus := GetStatus;
  Result := (LStatus <> IsStopped) And (LStatus <> IsUnusable);
End;

Procedure Tv2Protocol.WaitForConnection(AMaxLength: Integer);
Var
  LStopWaiting: TDateTime;
Begin
  LStopWaiting := Now + (AMaxLength * ((1 / (24 * 60)) / (60 * 1000)));
  While Not Connected And (LStopWaiting > now) Do
    sleep(50);
End;

Procedure Tv2Protocol.CheckSynchronousSendResult(AResult: TSendResponse; AMsg: String);
Begin
  Case AResult Of
    srNone:
      Raise EHL7CommunicationError.Create(Name, 'Internal error in IdHL7.pas: SynchronousSend returned srNone');
    srError:
      Raise EHL7CommunicationError.Create(Name, AMsg);
    srNoConnection:
      Raise EHL7CommunicationError.Create(Name, 'Not connected');
    srSent:
      Raise EHL7CommunicationError.Create(Name, 'Internal error in IdHL7.pas: SynchronousSend returned srSent');  // cause this should only be returned asynchronously
    srOK:; // all ok
    srTimeout:
      Raise EHL7CommunicationError.Create(Name, 'No response from remote system');
    Else
      Raise EHL7CommunicationError.Create(Name, 'Internal error in IdHL7.pas: SynchronousSend returned an unknown value ' + IntToStr(Ord(AResult)));
    End;
End;

Procedure Tv2Protocol.SetConnectionTimeout(Const AValue: Cardinal);
Begin
  // any value for AValue is accepted, although this may not make sense
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['Connection Timeout']));   {do not localize??}
    End;
  FConnectionTimeout := AValue;
End;

Procedure Tv2Protocol.ReConnectFromTimeout;
Var
  iLoop : Integer;
Begin
  Assert(Not FIsServer, 'Cannot try to reconnect from a timeout if is a server');
  StartClient;
  sleep(50);
  iLoop := 0;
  While Not Connected And (iLoop < 100) And Not FPreStopped Do
    Begin
    sleep(100);
    Inc(iLoop);
    End;
End;

procedure Tv2Protocol.SetKeepAlive(const Value: Boolean);
begin
  If Going Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWhileWorking, ['KeepAlive']));   {do not localize??}
    End;
  FKeepAlive := Value;
end;

function Tv2Protocol.HasClientConnection: Boolean;
begin
  result := FClientThread <> nil;
end;

procedure Tv2Protocol.Disconnect;
var
  i : integer;
begin
  if FIsServer Then
  Begin
    FLock.Enter;
    Try
      For i := 0 to FServerConnections.Count - 1 Do
        (FServerConnections[i] as TIdPeerThread).Connection.Disconnect;
    Finally
      FLock.Leave;
    End;
  End
  Else
    FClientThread.FClient.Disconnect;
end;

{ Tv2ProtocolPeerThread }

Procedure Tv2Protocol.CheckServerParameters;
Begin
  If (FCommunicationMode = cmAsynchronous) Or Not FIsListener Then
    Begin
    FConnectionLimit := 1;
    End;

  If (FPort < 1) Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7InvalidPort, [FPort]));
    End;
End;

Procedure Tv2Protocol.StartServer;
var
  i : integer;
  d : Cardinal;
Begin
  CheckServerParameters;
  FServer := TIdTCPServer.Create(Nil);
  Try
    FServer.DefaultPort := FPort;
    Fserver.OnConnect := ServerConnect;
    FServer.OnExecute := ServerExecute;
    FServer.OnDisconnect := ServerDisconnect;
    FServer.Active := True;
    if FKeepAlive Then
    Begin
      d := $FFFFFFFF;
      for i := 0 to FServer.Bindings.count - 1 Do
        FServer.Bindings[i].SetSockOpt(Id_SOL_SOCKET, Id_SO_KEEPALIVE, d);
    End;

    InternalSetStatus(IsNotConnected, RSHL7StatusNotConnected);
  Except
    On e:
    Exception Do
      Begin
      InternalSetStatus(IsStopped, Format(RSHL7StatusFailedToStart, [e.Message]));
      FreeAndNil(FServer);
      Raise;
      End;
    End;
End;

Procedure Tv2Protocol.StopServer;
Begin
  Try
    FServer.Active := False;
    FreeAndNil(FServer);
    InternalSetStatus(IsStopped, RSHL7StatusStopped);
  Except
    On e:Exception Do
      Begin
      // somewhat arbitrary decision: if for some reason we fail to shutdown,
      // we will stubbornly refuse to work again.
      InternalSetStatus(IsUnusable, Format(RSHL7StatusFailedToStop, [e.Message]));
      FServer := Nil;
      Raise
      End;
    End;
End;

Procedure Tv2Protocol.ServerConnect(AThread: TIdPeerThread);
Var
  LNotify: Boolean;
  LConnCount: Integer;
  LValid: Boolean;
  sIp : String;
  iIp : Cardinal;
Begin
  Assert(Assigned(AThread));
  Assert(Assigned(FLock));
  LConnCount := 0;

  sIp := (AThread.Connection.IOHandler as TIdIOHandlerSocket).Binding.PeerIP;
  iIp := ConvertIPtoCardinal(sIp);
  If (iIp Xor FIPRestrictionVal) And FIPMaskVal <> 0 Then
    raise exception.Create('Denied');

  FLock.Enter;
  Try
    LNotify := FConnCount = 0;
    LValid := FConnCount < FConnectionLimit;
    If LValid Then
    Begin
      If (FConnCount = 0) Then
      Begin
        FServerConn := AThread.Connection
      End
      Else
      Begin
        FServerConn := Nil;
      End;
      FServerConnections.Add(AThread);
      If LNotify Then
      Begin
        InternalSetStatus(IsConnected, RSHL7StatusConnected);
      End;
      Inc(FConnCount);
      LConnCount := FConnCount;
      AThread.Data := Self;
    End;
  Finally
    FLock.Leave;
  End;

  If LValid Then
    Begin
    If LNotify And Assigned(FOnConnect) Then
      Begin
      FOnConnect(Self);
      End;
    If Assigned(FOnConnCountChange) And (FConnectionLimit <> 1) Then
      Begin
      FOnConnCountChange(Self, LConnCount);
      End;
    End
  Else
    Begin
    // Thread exceeds connection limit
    // it would be better to stop getting here in the case of an invalid connection
    // cause here we drop it - nasty for the client. To be investigated later
    AThread.Connection.Disconnect;
    AThread.Data := nil;
    End;
End;

Procedure Tv2Protocol.ServerDisconnect(AThread: TIdPeerThread);
Var
  LNotify: Boolean;
  LConnCount: Integer;
  LIndex : Integer;
Begin
  Assert(Assigned(AThread));
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    if (AThread.Data <> nil) Then
      Dec(FConnCount);
    AThread.Data := nil;
    LNotify := FConnCount = 0;
    LConnCount := FConnCount;
    LIndex := FServerConnections.IndexOf(AThread);
    if LIndex <> -1 Then
      FServerConnections.Delete(LIndex);

    If AThread.Connection = FServerConn Then
      Begin
      FServerConn := Nil;
      End;
    If LNotify Then
      Begin
      InternalSetStatus(IsNotConnected, RSHL7StatusNotConnected);
      End;
  Finally
    FLock.Leave;
    End;
  If Assigned(FOnConnCountChange) And (FConnectionLimit <> 1) Then
    Begin
    FOnConnCountChange(Self, LConnCount);
    End;
  If LNotify And Assigned(FOnDisconnect) Then
    Begin
    FOnDisconnect(Self);
    End;
End;

Procedure Tv2Protocol.ServerExecute(AThread: TIdPeerThread);
var
  s : TBytes;
  len : integer;
begin
  assert(Assigned(Self));
  assert(Assigned(AThread));

  SetLength(s, 10000);
  len := 0;
  try
    // 1. prompt the network for content.
//    AThread.Connection.IOHandler.ReadLn(MSG_START); // throw this content away
    while Assigned(AThread.Connection.IOHandler) do
      begin
      // here, we use AnsiEncoding - whatever the bytes that are sent, they will be round tripped into a
      // ansi string which is actually bytes not chars. But usually it would be chars anyway
      //s := TBytes(AThread.Connection.IOHandler.ReadLn(MSG_END, FReceiveTimeout, -1, TEncoding.ANSI));
      if length(s) = len then
        SetLength(s, length(s)+10000);
      s[len] := AThread.Connection.IOHandler.ReadByte;
      inc(len);
      if len > 0 then
        HandleIncoming(s, len, AThread.Connection);
      end;
  except
    try
      // well, there was some network error. We aren't sure what it
      // was, and it doesn't matter for this layer. we're just going
      // to make sure that we start again.
      // to review: what happens to the error messages?
      AThread.Connection.Disconnect;
    except
    end;
  end;
End;

Procedure Tv2Protocol.DropServerConnection;
Begin
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    If Assigned(FServerConn) Then
      FServerConn.Disconnect;
  Finally
    FLock.Leave;
    End;
End;

{==========================================================
  Client Connection Maintainance
 ==========================================================}

Procedure Tv2Protocol.CheckClientParameters;
Begin
  If (FPort < 1) Then
    Begin
    Raise EHL7CommunicationError.Create(Name, Format(RSHL7InvalidPort, [FPort]));
    End;
End;

Procedure Tv2Protocol.StartClient;
Begin
  CheckClientParameters;
  FClientThread := Tv2ProtocolClientThread.Create(Self);
  InternalSetStatus(isConnecting, RSHL7StatusConnecting);
End;

Procedure Tv2Protocol.StopClient;
Var
  LFinished: Boolean;
  LStartTime : Cardinal;
Begin
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    If Assigned(FClientThread) Then
      Begin
      FClientThread.Terminate;
      FClientThread.FClient.Disconnect;
      FClientThread.FCloseEvent.SetEvent;
      End
    Else
      InternalSetStatus(isStopped, 'Stopped');
  Finally
    FLock.Leave;
    End;
  LStartTime := GetTickCount;
  Repeat
    LFinished := (GetStatus = IsStopped);
    If Not LFinished Then
      Begin
      sleep(10);
      End;
  Until LFinished Or (GetTickDiff64(LStartTime,GetTickCount) > WAIT_STOP);
  if not LFinished and (FClientThread <> nil) then
    FClientThread.FOwner := nil;
  If GetStatus <> IsStopped Then
    Begin
    // for some reason the client failed to shutdown. We will stubbornly refuse to work again
    InternalSetStatus(IsUnusable, Format(RSHL7StatusFailedToStop, [RSHL7ClientThreadNotStopped]));
    End;
End;

Procedure Tv2Protocol.DropClientConnection;
Begin
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    If Assigned(FClientThread) And Assigned(FClientThread.FClient) Then
      Begin
      FClientThread.FClient.Disconnect;
      End
    Else
      Begin
      // This may happen validly because both ends are trying to drop the connection simultaineously
      End;
  Finally
    FLock.Leave;
    End;
End;

{ Tv2ProtocolClientThread }

Constructor Tv2ProtocolClientThread.Create(aOwner: Tv2Protocol);
Begin
  Assert(Assigned(AOwner));
  FOwner := aOwner;
  FCloseEvent := TIdLocalEvent.Create(false, False);
  FreeOnTerminate := True;
  Inherited Create(False);
End;

Destructor Tv2ProtocolClientThread.Destroy;
Begin
  if (FOwner <> nil) then
  begin
    Assert(Assigned(FOwner.FLock));
    FreeAndNil(FCloseEvent);
    Try
      FOwner.FLock.Enter;
      Try
        FOwner.FClientThread := Nil;
        If Not TimedOut Then
          FOwner.InternalSetStatus(isStopped, RSHL7StatusStopped);
      Finally
        FOwner.FLock.Leave;
        End;
    Except
      // it's really vaguely possible that the owner
      // may be dead before we are. If that is the case, we blow up here.
      // who cares.
      End;
  end;
  Inherited;
End;

Procedure Tv2ProtocolClientThread.PollStack;
var
  LBuffer: TBytes;
  len : integer;
begin
  assert(Assigned(Self));
  SetLength(LBuffer, 10000);
  len := 0;
  repeat
    // we don't send here - we just poll the stack for content
    // if the application wants to terminate us at this point,
    // then it will disconnect the socket and we will get thrown
    // out
    // we really don't care at all whether the disconnect was clean or ugly

    // but we do need to suppress exceptions that come from
    // indy otherwise the client thread will terminate

    try
      while Assigned(FClient.IOHandler) do
      begin
        if (length(LBuffer) = len) then
          SetLength(LBuffer, length(LBuffer)+10000);
        LBuffer[len] := FClient.IOHandler.ReadByte;
        inc(len);
        FOwner.HandleIncoming(LBuffer, len, FClient);
      end;
    except
      try
        // well, there was some network error. We aren't sure what it
        // was, and it doesn't matter for this layer. we're just going
        // to make sure that we start again.
        // to review: what happens to the error messages?
        FClient.Disconnect;
      except
        end;
      end;
  until Terminated or not FClient.Connected;
End;

const
  MINUTE_LENGTH = 1 / (24 * 60);
  SECOND_LENGTH = MINUTE_LENGTH / 60;

function DescribePeriod(Period: TDateTime): String;
begin
  if period < 0 then
    period := -period;
  if Period < SECOND_LENGTH then
    Result := IntToStr(trunc(Period * 1000 / SECOND_LENGTH)) + 'ms'
  else if Period < 180 * SECOND_LENGTH then
    Result := IntToStr(trunc(Period / SECOND_LENGTH)) + 'sec'
  else if Period < 180 * MINUTE_LENGTH then
    Result := IntToStr(trunc(Period / MINUTE_LENGTH)) + 'min'
  else if Period < 72 * 60 * MINUTE_LENGTH then
    Result := IntToStr(trunc(Period / (MINUTE_LENGTH * 60))) + 'hr'
  else
    Result := IntToStr(trunc(Period)) + ' days';
end;

Procedure Tv2ProtocolClientThread.Execute;
Var
  LRecTime: TDateTime;
  d : integer;
Begin
  SetThreadName('v2.Client');
  Try
    FClient := TIdTCPClient.Create(Nil);
    Try
      FClient.Host := FOwner.FAddress;
      FClient.Port := FOwner.FPort;
      Repeat
        // try to connect. Try indefinitely but wait Owner.FReconnectDelay
        // between attempts.
        SetThreadStatus('Connecting');
        Repeat
          FOwner.InternalSetStatus(IsConnecting, rsHL7StatusConnecting);
          Try
            FClient.Connect;
            FClient.Socket.UseNagle := true;
            FLastTraffic := now;
          Except
            On e:
            Exception Do
              Begin
              LRecTime := Now + ((FOwner.FReconnectDelay / 1000) * {second length} (1 / (24 * 60 * 60)));
              FOwner.InternalSetStatus(IsWaitReconnect, Format(rsHL7StatusReConnect, [DescribePeriod(LRecTime - Now), e.Message])); {do not localize??}
              End;
            End;
          If Not Terminated And Not FClient.Connected Then
            Begin
            FCloseEvent.WaitFor(FOwner.FReconnectDelay);
            End;
        Until Terminated Or FClient.Connected;
        If Terminated Then
          Begin
          Exit;
          End;
        SetThreadStatus('Connected');
        if FOwner.FKeepAlive Then
        Begin
          d := -1;
          FCLient.Socket.Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_KEEPALIVE, d);
        End;

        FOwner.FLock.Enter;
        Try
          FOwner.FClient := FClient;
          FOwner.InternalSetStatus(IsConnected, rsHL7StatusConnected);
        Finally
          FOwner.FLock.Leave;
          End;
        If Assigned(FOwner.FOnConnect) Then
          Begin
          FOwner.FOnConnect(FOwner);
          End;
        Try
          PollStack;
        Finally
          if FOwner <> nil then
          begin
            FOwner.FLock.Enter;
            Try
              FOwner.FClient := Nil;
              If TimedOut Then
                FOwner.InternalSetStatus(isTimedOut, RSHL7StatusTimedout)
              Else
                FOwner.InternalSetStatus(IsNotConnected, RSHL7StatusNotConnected);
            Finally
              FOwner.FLock.Leave;
              End;
            If Assigned(FOwner.FOnDisconnect) Then
              Begin
              FOwner.FOnDisconnect(FOwner);
              End;
            End;
          End;
        If TimedOut Then
          Begin
          FClient.Disconnect;
          End
        Else If Not Terminated Then
          Begin
          // we got disconnected. ReconnectDelay applies.
          LRecTime := Now + ((FOwner.FReconnectDelay / 1000) * {second length} (1 / (24 * 60 * 60)));
          if FOwner <> nil then
          begin
            FOwner.InternalSetStatus(IsWaitReconnect, Format(rsHL7StatusReConnect, [DescribePeriod(LRecTime - now), 'Disconnected'])); {do not localize??}
            FCloseEvent.WaitFor(FOwner.FReconnectDelay);
          end;
          End;
      Until Terminated Or (Not FOwner.IsListener And TimedOut);
    Finally
      FreeAndNil(FClient);
      SetThreadStatus('Done');
    End;
  Except
    On e: Exception Do
      // presumably some comms or indy related exception
      // there's not really anyplace good to put this????
    End;
  closeThread;
End;

{==========================================================
  Internal process management
 ==========================================================}

function find(buf : TBytes; len : integer; val : AnsiString) : integer;
var
  i,j : integer;
  ok : boolean;
begin
  result := -1;
  for i := 0 to len - 1 do
  begin
    ok := true;
    for j := 0 to length(val) - 1 do
      if buf[i+j] <> ord(val[1+j]) then
        ok := false;
    if ok then
      exit(i);
  end;
end;

Procedure Tv2Protocol.HandleIncoming(Var VBuf: TBytes; var len : integer; AConnection: TIdTCPConnection);
Var
  LStart, LEnd: Integer;
  LMsg, LReply : TBytes;
  LBytes : TIdBytes;
Begin
  Assert(len > 0, 'Attempt to handle an empty buffer');
  Assert(Assigned(AConnection));
  Try
    // process any messages in the buffer (may get more than one per packet)
    Repeat
      LStart := Find(VBuf, len, MSG_START);
      LEnd := Find(VBuf, len, MSG_END);

      If (LStart > -1) And (LEnd > -1) Then
        Begin
        LMsg := Copy(VBuf, LStart + Length(MSG_START), LEnd - (LStart + Length(MSG_START)));
        if LEnd = len - Length(MSG_END) then
          len := 0
        else
        begin
          move(VBuf[LEnd + Length(MSG_END)], VBuf[0], len - (LEnd + Length(MSG_START)));
          len := len - (LEnd + Length(MSG_START));
        end;
        If HandleMessage(LMsg, AConnection, LReply) Then
          Begin
          If Length(LReply) > 0 Then
            Begin
            SetLength(LBytes, Length(LReply)+ 3);
            move(MSG_START[1], LBytes[0], 1);
            move(LReply[0], LBytes[1], length(LReply));
            move(MSG_END[1], LBytes[length(LBytes)-2], 2);
            AConnection.IOHandler.Write(LBytes);
            End;
          End
        Else
          Begin
          AConnection.Disconnect;
          End;
        End
      Else if (LStart = -1) And (LEnd >= 0) Then
      begin
        if LEnd < len - Length(MSG_START) then
          move(VBuf[LEnd + Length(MSG_END)], VBuf[0], len - (LEnd + Length(MSG_START)));
        len := len - (LEnd + Length(MSG_START));
      end;
    Until (LEnd = -1);
    If len > BUFFER_SIZE_LIMIT Then
      AConnection.Disconnect;
  Except
    // well, we need to suppress the exception, and force a reconnection
    // we don't know why an exception has been allowed to propagate back
    // to us, it shouldn't be allowed. so what we're going to do, is drop
    // the connection so that we force all the network layers on both
    // ends to reconnect.
    // this is a waste of time of the error came from the application but
    // this is not supposed to happen
    Try
      AConnection.Disconnect;
    Except
      // nothing - suppress
      End;
    End;
End;

Function Tv2Protocol.HandleMessage(Const AMsg: TBytes; AConn: TIdTCPConnection; Var VReply: TBytes): Boolean;
Var
  LQueMsg: TIdQueuedMessage;
  LIndex: Integer;
Begin
  Assert(length(AMsg) > 0, 'Attempt to handle an empty Message');
  Assert(Assigned(FLock));
  SetLength(VReply, 0);
  Result := True;
  Try
    Case FCommunicationMode Of
      cmUnknown:
        Begin
        Raise EHL7CommunicationError.Create(Name, RSHL7ImpossibleMessage);
        End;
      cmAsynchronous:
        Begin
        FOnMessageArrive(Self, AConn, Amsg);
        End;
      cmSynchronous, cmSingleThread:
        Begin
        If IsListener Then
          Begin
          If FCommunicationMode = cmSynchronous Then
            Begin
            Result := False;
            FOnReceiveMessage(Self, AConn, AMsg, Result, VReply)
            End
          Else
            Begin
            LQueMsg := TIdQueuedMessage.Create(AMsg, FReceiveTimeout);
            LQueMsg._AddRef;
            Try
              FLock.Enter;
              Try
                FMsgQueue.Add(LQueMsg);
              Finally
                FLock.Leave;
                End;
              LQueMsg.wait;
              // no locking. There is potential problems here. To be reviewed
              VReply := LQueMsg.FReply;
            Finally
              FLock.Enter;
              Try
                LIndex := FMsgQueue.IndexOf(LQueMsg);
                If LIndex > -1 Then
                  FMsgQueue.Delete(LIndex);
              Finally
                FLock.Leave;
                End;
              LQueMsg._Release;
              End;
            End
          End
        Else
          Begin
          FLock.Enter;
          Try
            If FWaitingForAnswer Then
              Begin
              FWaitingForAnswer := False;
              FMsgReply := AMsg;
              FReplyResponse := srOK;
              If FCommunicationMode = cmSynchronous Then
                Begin
                Assert(Assigned(FWaitEvent));
                FWaitEvent.SetEvent;
                End;
              End
            Else
              Begin
              // we could have got here by timing out, but this is quite unlikely,
              // since the connection will be dropped in that case. We will report
              // this as a spurious message
              Raise EHL7CommunicationError.Create(Name, RSHL7UnexpectedMessage);
              End;
          Finally
            FLock.Leave;
            End;
          End
        End;
      Else
        Begin
        Raise EHL7CommunicationError.Create(Name, RSHL7UnknownMode);
        End;
      End;
  Except
    On e:
    Exception Do
      If Assigned(FOnReceiveError) Then
        Begin
        FOnReceiveError(Self, AConn, AMsg, e, VReply, Result)
        End
    Else
      Begin
      Result := False;
      End;
    End;
End;

{==========================================================
  Sending
 ==========================================================}

// this procedure is not technically thread safe.
// if the connection is disappearing when we are attempting
// to write, we can get transient access violations. Several
// strategies are available to prevent this but they significantly
// increase the scope of the locks, which costs more than it gains

Function Tv2Protocol.AsynchronousSend(AMsg: TBytes): TSendResponse;
var
  LBytes : TIdBytes;
Begin
  Assert(Length(AMsg) > 0, 'Attempt to send an empty message');
  Assert(Assigned(FLock));
//  Result := srNone; // just to suppress the compiler warning
  If GetStatus = isTimedOut Then
    Begin
    ReConnectFromTimeout;
    End;

  FLock.Enter;
  Try
    If Not Going Then
      Begin
      Raise EHL7CommunicationError.Create(Name, Format(RSHL7NotWorking, [RSHL7SendMessage]))
      End
    Else If GetStatus <> isConnected Then
      Begin
      Result := srNoConnection
      End
    Else
      Begin
      If FIsServer Then
        Begin
        If Assigned(FServerConn) Then
        Begin
          SetLength(LBytes, Length(AMsg)+ 3);
          move(MSG_START[1], LBytes[0], 1);
          move(AMsg[0], LBytes[1], length(AMsg));
          move(MSG_END[1], LBytes[length(LBytes)-2], 2);
          FServerConn.IOHandler.Write(LBytes);
          Result := srSent
        End
        Else
          Begin
          Raise EHL7CommunicationError.Create(Name, RSHL7NoConnectionFound);
          End
        End
      Else
      Begin
        SetLength(LBytes, Length(AMsg)+ 3);
        move(MSG_START[1], LBytes[0], 1);
        move(AMsg[0], LBytes[1], length(AMsg));
        move(MSG_END[1], LBytes[length(LBytes)-2], 2);
        FClient.IOHandler.Write(LBytes);
        FClientThread.FLastTraffic := Now;
        Result := srSent
        End;
      End;
  Finally
    FLock.Leave;
    End
End;

Function Tv2Protocol.SynchronousSend(AMsg: TBytes; Var VReply: TBytes): TSendResponse;
Begin
  Assert(Length(AMsg) > 0, 'Attempt to send an empty message');
  Assert(Assigned(FLock));
  Result := srError;
  FLock.Enter;
  Try
    FWaitingForAnswer := True;
    FWaitStop := now + (FTimeOut * MILLISECOND_LENGTH);
    FReplyResponse := srTimeout;
    SetLength(FMsgReply, 0);
  Finally
    FLock.Leave;
    End;
  Try
    Result := AsynchronousSend(AMsg);
    If Result = srSent Then
      Begin
      Assert(Assigned(FWaitEvent));
      FWaitEvent.WaitFor(FTimeOut);
      End;
  Finally
    FLock.Enter;
    Try
      FWaitingForAnswer := False;
      If Result = srSent Then
        Begin
        Result := FReplyResponse;
        End;
      If Result = srTimeout Then
        Begin
        If FIsServer Then
          DropServerConnection
        Else
          DropClientConnection;
        End;
      VReply := FMsgReply;
    Finally
      FLock.Leave;
      End;
    End;
End;

Procedure Tv2Protocol.SendMessage(AMsg: TBytes);
Begin
  Assert(Length(AMsg) > 0, 'Attempt to send an empty message');
  Assert(Assigned(FLock));
  If FWaitingForAnswer Then
    Raise EHL7CommunicationError.Create(Name, RSHL7WaitForAnswer);

  FLock.Enter;
  Try
    FWaitingForAnswer := True;
    FWaitStop := now + (FTimeOut * MILLISECOND_LENGTH);
    SetLength(FMsgReply, 0);
    FReplyResponse := AsynchronousSend(AMsg);
  Finally
    FLock.Leave;
    End;
End;

Function Tv2Protocol.GetReply(Var VReply: TBytes): TSendResponse;
Begin
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    If FWaitingForAnswer Then
      Begin
      If FWaitStop < now Then
        Begin
        Result := srTimeout;
        SetLength(VReply, 0);
        FWaitingForAnswer := False;
        FReplyResponse := srError;
        End
      Else
        Begin
        Result := srNone;
        End;
      End
    Else
      Begin
      Result := FReplyResponse;
      If Result = srSent Then
        Begin
        Result := srTimeOut;
        End;
      VReply := FMsgReply;
      FWaitingForAnswer := False;
      FReplyResponse := srError;
      End;
  Finally
    FLock.Leave;
    End;
End;

Function Tv2Protocol.GetMessage(Var VMsg: TBytes): pointer;
Begin
  Assert(Assigned(FLock));
  Assert(Assigned(FMsgQueue));
  FLock.Enter;
  Try
    If FMsgQueue.Count = 0 Then
      Begin
      Result := Nil
      End
    Else
      Begin
      Result := FMsgQueue[0];
      TIdQueuedMessage(Result)._AddRef;
      VMsg := TIdQueuedMessage(Result).FMsg;
      FMsgQueue.Delete(0);
      FHndMsgQueue.Add(Result);
      End;
  Finally
    FLock.Leave;
    End;
End;

Procedure Tv2Protocol.SendReply(AMsgHnd: pointer; AReply: TBytes);
Var
  qm: TIdQueuedMessage;
Begin
  Assert(Assigned(AMsgHnd));
  Assert(Length(AReply) > 0, 'Attempt to send an empty reply');
  Assert(Assigned(FLock));
  FLock.Enter;
  Try
    qm := TObject(AMsgHnd) As TIdQueuedMessage;
    qm.FReply := AReply;
    qm.FEvent.SetEvent;
    qm._Release;
    FHndMsgQueue.Delete(FHndMsgQueue.IndexOf(AMsgHnd));
  Finally
    FLock.Leave;
    End;
End;

Function Tv2ProtocolClientThread.TimedOut: Boolean;
Var
  lGap : TDateTime;
Begin
  lGap := (now - FLastTraffic) * 24 * 60 * 60 * 1000;
  Result := (FOwner = nil) or (FOwner.FConnectionTimeout > 0) And ( lGap > FOwner.FConnectionTimeout);
End;

end.
