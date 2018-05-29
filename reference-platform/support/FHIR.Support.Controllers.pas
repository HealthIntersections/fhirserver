Unit FHIR.Support.Controllers;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

Interface


Uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils, Classes,
  FHIR.Support.System, FHIR.Support.Math, FHIR.Support.Strings, FHIR.Support.DateTime, FHIR.Support.Osx,
  FHIR.Support.Objects, FHIR.Support.Exceptions, FHIR.Support.Collections;

Type
  TFslThreadHandle = TThreadHandle;
  TFslThreadID = TThreadID;

  TFslThreadDelegate = Procedure Of Object;

  TFslThread = Class(TFslObject)
    Private
      FInternal : TThread; // Handle to the Windows thread.
      FID : TFslThreadID;         // Unique ID of the Windows thread.
      FActive : Boolean;          // Run thread has finished.
      FDelegate : TFslThreadDelegate;

    Protected
      Procedure Execute; Virtual;
      Procedure Interrupt; Virtual;
      Function Running : Boolean; Virtual;

      Procedure ExecuteYield(Const iTimeout : Cardinal);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TFslThread;

      Procedure Open;
      Procedure Close;
      Procedure Stop; Virtual;

      Procedure Wait;
      Function WaitTimeout(iTimeout : Cardinal) : Boolean;

      Procedure Kill;

      Function Active : Boolean;

      Property ID : TFslThreadID Read FID Write FID;
      Property Delegate : TFslThreadDelegate Read FDelegate Write FDelegate;
//    Property Processor : Cardinal Write SetProcessor; // see comments in SetProcessor
  End;

  TFslThreadClass = Class Of TFslThread;

  TFslThreadList = Class(TFslObjectList)
    Private
      Function GetThread(iIndex: Integer): TFslThread;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Active : Boolean;

      Property Threads[iIndex : Integer] : TFslThread Read GetThread; Default;
  End;

  TFslObjectClass = FHIR.Support.Objects.TFslObjectClass;

  TFslExclusiveCriticalSection = Class(TFslObject)
    Private
      FHandle : TRTLCriticalSection;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Lock; Virtual;
      Procedure Unlock; Virtual;
      Function TryLock : Boolean; Virtual;

      Property Handle : TRTLCriticalSection Read FHandle;
  End;

  TFslExclusiveStateCriticalSection = Class(TFslExclusiveCriticalSection)
    Private
      FThreadID : TFslThreadHandle;
      FNested : Integer;

    Protected
      Procedure Enter;
      Procedure Leave;

    Public
      Procedure Lock; Override;
      Procedure Unlock; Override;
      Function TryLock : Boolean; Override;

      Function IsNestedLocked : Boolean;
      Function IsLocked : Boolean;
      Function IsLockedToThread(Const aThread : TFslThreadHandle) : Boolean;
      Function IsLockedToCurrentThread : Boolean;
  End;

{
Type
  TFslSignalHandle = THandle;

  TFslSignal = Class(TFslObject)
    Private
      FName : String;
      FHandle : TFslSignalHandle;
      FAutoHide : Boolean;

    Protected
      Procedure Open(bShow: Boolean);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TFslSignal;

      Procedure OpenShow;
      Procedure OpenHide;
      Procedure Close;

      Procedure Show;
      Procedure Hide;
      Procedure Flash;
      Function Wait : Boolean;
      Function WaitTimeout(Const iTimeout : Cardinal) : Boolean;

      Function Active : Boolean;

      Property Handle : TFslSignalHandle Read FHandle Write FHandle;
      Property Name : String Read FName Write FName;
      Property AutoHide : Boolean Read FAutoHide Write FAutoHide;
  End;

  TFslSignalList = Class(TFslObjectList)
    Private
      Function GetSignalByIndex(iIndex: Integer): TFslSignal;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property SignalByIndex[iIndex : Integer] : TFslSignal Read GetSignalByIndex; Default;
  End;

  TFslSignalManager = Class(TFslObject)
    Private
      FSignalList : TFslSignalList;
      FSignalHandleArray : TWOHandleArray;
      FSignalHandleCount : Integer;
      FActive : Boolean;

      Function WaitTimeout(Const iTimeout : Cardinal; Const bAll : Boolean) : Boolean;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Prepare;
      Procedure Terminate;

      Procedure AddSignal(oSignal : TFslSignal);
      Procedure DeleteSignal(oSignal : TFslSignal);
      Procedure DeleteAllSignals;

      Function WaitTimeoutForAll(Const iTimeout : Cardinal) : Boolean;
      Function WaitTimeoutForAny(Const iTimeout : Cardinal) : Boolean;
  End;

 }
Type
  TFslMethod = Procedure Of Object;
  PFslMethod = ^TFslMethod;

  TFslMethodItem = Record
    Code, Data : Pointer;
  End;

  PFslMethodItem = ^TFslMethodItem;

  TFslMethodItemArray = Array[0..(MaxInt Div SizeOf(TFslMethodItem)) - 1] Of TFslMethodItem;
  PFslMethodItemArray = ^TFslMethodItemArray;

  TFslMethodList = Class(TFslItemList)
    Private
      FMethodArray : PFslMethodItemArray;

      Function GetMethodItem(iIndex : Integer): TFslMethod;
      Procedure SetMethodItem(iIndex : Integer; Const aValue : TFslMethod);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; Value: Pointer); Override;

      Procedure AssignItem(oItems : TFslItemList; iIndex : Integer); Override;

      Function CompareItem(pA, pB: Pointer): Integer; Overload; Override;

      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount: Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CapacityLimit : Integer; Override;

      Function Find(aValue : TFslMethod; Out iResult : Integer) : Boolean;

    Public
      Function IndexByValue(aValue : TFslMethod) : Integer;
      Function ExistsByValue(aValue: TFslMethod): Boolean;

      Function Add(aValue : TFslMethod) : Integer;
      Procedure Insert(iIndex : Integer; aValue : TFslMethod);
      Procedure DeleteByValue(aValue : TFslMethod);

      Property MethodByIndex[iIndex : Integer] : TFslMethod Read GetMethodItem Write SetMethodItem; Default;
  End;

  TFslEvent = Procedure (Sender : TFslObject) Of Object;

  TFslEventList = Class(TFslMethodList)
    Private
      Function GetEvent(iIndex : Integer): TFslEvent;
      Procedure SetEvent(iIndex : Integer; Const aValue : TFslEvent);

    Public
      Function IndexByValue(aValue : TFslEvent) : Integer;
      Function ExistsByValue(aValue : TFslEvent) : Boolean;
      Function Add(aValue : TFslEvent) : Integer;
      Procedure Insert(iIndex : Integer; aValue : TFslEvent);
      Procedure DeleteByValue(aValue : TFslEvent);

      Property EventByIndex[iIndex : Integer] : TFslEvent Read GetEvent Write SetEvent; Default;
  End;



Type
  TFslDispatched = Class(TFslStringHashEntry)
    Private
      FEventList : TFslEventList;

    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Override;

      Property Events : TFslEventList Read FEventList;
  End;

  TFslDispatcher = Class(TFslStringHashTable)
    Private
      FHashEntry : TFslDispatched;

    Protected
      Function ItemClass : TFslHashEntryClass; Override;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure AddDispatched(Const sName : String; aEvent : TFslEvent);
      Procedure RemoveDispatched(Const sName : String; aEvent : TFslEvent);
      Function GetDispatched(Const sName : String) : TFslDispatched;
      Function ForceDispatched(Const sName : String) : TFslDispatched;
      Function ExistsDispatched(Const sName : String) : Boolean;

      Procedure Register(Const sName : String; aEvent : TFslEvent; bRegister : Boolean = True); Overload;

      Function Call(oSender : TFslObject; oDispatched : TFslDispatched) : Boolean; Virtual;
  End;

  EFslDispatcher = Class(EFslException);

  TFslStringEvent = Procedure (oSender : TFslObject; Const sValue : String) Of Object;

  TFslStringDispatcher = Class(TFslDispatcher)
    Public
      Procedure Register(Const sName : String; aEvent : TFslStringEvent; bRegister : Boolean = True); Overload;

      Function CallString(oSender : TFslObject; Const sValue : String) : Boolean;
  End;

  TFslIntegerEvent = Procedure (oSender : TFslObject; Const iValue : Integer) Of Object;

  TFslIntegerDispatcher = Class(TFslDispatcher)
    Public
      Function CallInteger(oSender : TFslObject; iValue : Integer) : Boolean; Overload;

      Procedure Register(iValue : Integer; aEvent : TFslIntegerEvent; bRegister : Boolean = True); Overload;
  End;

  TFslObjectClassEvent = Procedure (oSender : TFslObject; Const oValue : TFslObject) Of Object;

  TFslObjectClassDispatcher = Class(TFslDispatcher)
    Protected
      Procedure CallClass(oDispatched : TFslDispatched; oSender, oValue : TFslObject); Overload;

    Public
      Function CallClass(oSender, oValue : TFslObject) : Boolean; Overload;

      Procedure Register(Const aClass : TFslObjectClass; aEvent : TFslObjectClassEvent; bRegister : Boolean = True); Overload;
  End;

  TFslObjectEvent = Procedure (oSender : TFslObject; Const oValue : TFslObject) Of Object;

  TFslObjectDispatcher = Class(TFslDispatcher)
    Public
      Function Declare(oObject : TFslObject) : String;

      Procedure Register(Const aObject : TFslObject; aEvent : TFslObjectEvent; bRegister : Boolean = True); Overload;

      Function Call(oSender : TFslObject; oDispatched : TFslDispatched) : Boolean; Overload; Override;
  End;

  TFslController = Class(TFslObject)
    Protected
      Function ErrorClass : EFslExceptionClass; Override;

    Public
      Function Link : TFslController;

      Procedure Open; Virtual;
      Procedure Close; Virtual;
  End;

  EFslController = Class(EFslException);

  TFslControllerList = Class(TFslObjectList)
    Private
      Function GetControllerByIndex(Const iIndex : Integer) : TFslController;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property ControllerByIndex[Const iIndex : Integer] : TFslController Read GetControllerByIndex; Default;
  End;

  TFslParameters = Class(TFslCollection)
    Protected
      Function GetParameter(iIndex : Integer) : String;
      Function GetKey(iIndex : Integer) : String;
      Function GetValue(iIndex : Integer) : String;
      Function GetCount : Integer;

      Procedure GetKeyValue(iIndex : Integer; Out sKey, sValue : String);

    Public
      // Single switched parameter, prefixed by a / and separated only by whitespace.
      Function Switched(Const sSwitch : String) : Boolean;

      // Key:Value parameters
      Function Exists(Const sKey : String) : Boolean;
      Function IndexOf(Const sKey : String) : Integer;
      Function Get(Const sKey : String) : String;

      Function Valid(iIndex : Integer) : Boolean;

      // Original command line string.
      Function AsText : String;

      // Full file path of executable.
      Function Executable : String;

      // Full folder path of executable
      Function Folder : String;

      Property Parameters[iIndex : Integer] : String Read GetParameter; Default;
      Property Keys[iIndex : Integer] : String Read GetKey;
      Property Values[iIndex : Integer] : String Read GetValue;
      Property Count : Integer Read GetCount;
  End;

Implementation




type
  TInternalThread = class (TThread)
  private
    FOwner : TFslThread;
  protected
    procedure Execute; override;
  public
    Constructor Create(thread : TFslThread);
  end;

Constructor TFslThread.Create;
Begin
  Inherited;

  FInternal := nil;
End;


Destructor TFslThread.Destroy;
Begin
  Inherited;
End;


Function TFslThread.Link: TFslThread;
Begin
  Result := TFslThread(Inherited Link);
End;


Procedure TFslThread.Execute;
Begin
  If Assigned(FDelegate) Then
    FDelegate;
End;


Procedure TFslThread.Interrupt;
Begin
End;


Function TFslThread.Running: Boolean;
Begin
  Result := True;
End;



Procedure TFslThread.Open;
Begin
  If FActive Then
    RaiseError('Open', 'Thread is already active.');

  FActive := True;

  System.IsMultiThread := True;

  FInternal := TInternalThread.create(self);
End;


Procedure TFslThread.Close;
Begin
  FInternal.Terminate;
End;


Procedure TFslThread.Kill;
Begin
  FInternal.Terminate;
  {$IFDEF MSWINDOWS}
  TerminateThread(FInternal.Handle, 0);
  {$ENDIF}
  FInternal.Free;
  FInternal := nil;

  FActive := False;
End;


Procedure TFslThread.Stop;
Begin
  FActive := False;

  FInternal.Terminate;
End;


Procedure TFslThread.Wait;
Begin
  FInternal.WaitFor;
End;


function TFslThread.WaitTimeout(iTimeout: Cardinal): Boolean;
begin
  result := FInternal.WaitFor > 0;// todo
end;

Procedure TFslThread.ExecuteYield(Const iTimeout: Cardinal);
Begin
  FHIR.Support.System.ThreadSleep(iTimeout);
End;


Function TFslThread.Active : Boolean;
Begin
  Result := FActive And Running;
End;


Function TFslThreadList.ItemClass : TFslObjectClass;
Begin
  Result := TFslThread;
End;


Function TFslThreadList.GetThread(iIndex: Integer): TFslThread;
Begin
  Result := TFslThread(ObjectByIndex[iIndex]);
End;


Function TFslThreadList.Active : Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  iLoop := 0;
  While (iLoop < Count) And Not Result Do
  Begin
    Result := Threads[iLoop].Active;
    Inc(iLoop);
  End;
End;


{ TInternalThread }

constructor TInternalThread.Create(thread: TFslThread);
begin
  FOwner := thread;
  inherited create(false);
end;

procedure TInternalThread.execute;
begin
  SetThreadName('TInternalThread');
  Try
    FOwner.Execute;
  Except
    // ignore any further exceptions
  End;
  FOwner.FActive := False;
  SetThreadName('');
end;


Constructor TFslExclusiveCriticalSection.Create;
Begin
  Inherited;

  InitializeCriticalSection(FHandle);
End;


Destructor TFslExclusiveCriticalSection.Destroy;
Begin
  DeleteCriticalSection(FHandle);

  Inherited;
End;  


Procedure TFslExclusiveCriticalSection.Lock;
Begin
  EnterCriticalSection(FHandle);
End;


Function TFslExclusiveCriticalSection.TryLock : Boolean;
Begin 
  Result := TryEnterCriticalSection(FHandle);
End;  


Procedure TFslExclusiveCriticalSection.Unlock;
Begin
  LeaveCriticalSection(FHandle);
End;


Procedure TFslExclusiveStateCriticalSection.Lock;
Begin 
  Inherited;

  Enter;
End;  


Procedure TFslExclusiveStateCriticalSection.Unlock;
Begin 
  Assert(CheckCondition(IsLockedToCurrentThread, 'Unlock', 'Cannot unlock as the critical section is not locked to the current thread.'));

  Leave;

  Inherited;
End;  


Function TFslExclusiveStateCriticalSection.TryLock : Boolean;
Begin 
  Result := Inherited TryLock;

  If Result Then
    Enter;
End;  


Function TFslExclusiveStateCriticalSection.IsLocked : Boolean;
Begin
  Result := FThreadID <> INVALID_HANDLE_VALUE;
End;


Function TFslExclusiveStateCriticalSection.IsLockedToCurrentThread : Boolean;
Begin 
  Result := IsLockedToThread(GetCurrentThreadID);
End;  


Function TFslExclusiveStateCriticalSection.IsLockedToThread(Const aThread: TFslThreadHandle): Boolean;
Begin 
  Result := FThreadID = aThread;
End;  


Function TFslExclusiveStateCriticalSection.IsNestedLocked : Boolean;
Begin 
  Result := FNested > 1;
End;  


Procedure TFslExclusiveStateCriticalSection.Enter;
Begin 
  If FNested = 0 Then
    FThreadID := GetCurrentThreadID;

  Inc(FNested);
End;  


Procedure TFslExclusiveStateCriticalSection.Leave;
Begin 
  Dec(FNested);

  If FNested = 0 Then
    FThreadID := INVALID_HANDLE_VALUE;
End;


{Constructor TFslSignal.Create;
Begin
  Inherited;

  FHandle := 0;
End;


Destructor TFslSignal.Destroy;
Begin
  If Active Then
    Close;

  Inherited;
End;


Function TFslSignal.Link: TFslSignal;
Begin
  Result := TFslSignal(Inherited Link);
End;


Procedure TFslSignal.Open(bShow : Boolean);
Var
  pName : PChar;
Begin
  Assert(CheckCondition(Not Active, 'Open', 'Signal must not already be active.'));

  If FName = '' Then
    pName := Nil
  Else
    pName := PChar(FName);

  FHandle := CreateEvent(Nil, Not FAutoHide, bShow, pName);
End;


Procedure TFslSignal.OpenHide;
Begin
  Open(False);
End;


Procedure TFslSignal.OpenShow;
Begin
  Open(True);
End;


Procedure TFslSignal.Close;
Begin
  Assert(CheckCondition(Active, 'Close', 'Signal must be active.'));

  CloseHandle(FHandle);

  FHandle := 0;
End;


Procedure TFslSignal.Hide;
Begin
  Assert(CheckCondition(Active, 'Hide', 'Signal must be active.'));

  If Not ResetEvent(FHandle) Then
    RaiseError('Hide', ErrorAsString);
End;


Procedure TFslSignal.Show;
Begin
  Assert(CheckCondition(Active, 'Show', 'Signal must be active.'));

  If Not SetEvent(FHandle) Then
    RaiseError('Show', ErrorAsString);
End;


Procedure TFslSignal.Flash;
Begin
  Assert(CheckCondition(Active, 'Flash', 'Signal must be active.'));

  If Not PulseEvent(FHandle) Then
    RaiseError('Flash', ErrorAsString);
End;


Function TFslSignal.Wait : Boolean;
Begin
  Result := WaitTimeout(INFINITE);
End;


Function TFslSignal.WaitTimeout(Const iTimeout: Cardinal) : Boolean;
Var
  iWaitResult : Cardinal;
Begin
  Assert(CheckCondition(Active, 'WaitTimeout', 'Signal must be active.'));

  iWaitResult := WaitForSingleObject(FHandle, iTimeout);

  if (iWaitResult = WAIT_FAILED) Then
    RaiseError('WaitTimeout', ErrorAsString);

  Result := iWaitResult = WAIT_OBJECT_0;
End;


Function TFslSignalList.GetSignalByIndex(iIndex: Integer): TFslSignal;
Begin
  Result := TFslSignal(ObjectByIndex[iIndex]);
End;


Function TFslSignalList.ItemClass: TFslObjectClass;
Begin
  Result := TFslSignal;
End;


Function TFslSignal.Active: Boolean;
Begin
  Result := FHandle <> 0;
End;


Constructor TFslSignalManager.Create;
Begin
  Inherited;

  FSignalList := TFslSignalList.Create;
  FSignalList.SortedByReference;
  FSignalList.PreventDuplicates;
End;


Destructor TFslSignalManager.Destroy;
Begin
  If FActive Then
    Terminate;

  FSignalList.Free;

  Inherited;
End;


Procedure TFslSignalManager.AddSignal(oSignal: TFslSignal);
Begin
  Assert(CheckCondition(Not FActive, 'AddSignal', 'Cannot add a signal to a prepared signal manager.'));

  If (FSignalList.Count >= MAXIMUM_WAIT_OBJECTS) Then
  Begin
    oSignal.Free;

    RaiseError('AddSignal', StringFormat('The signal manager only supports up to %d signals', [MAXIMUM_WAIT_OBJECTS]));
  End;

  FSignalList.Add(oSignal);
End;


Procedure TFslSignalManager.DeleteSignal(oSignal: TFslSignal);
Begin
  Assert(CheckCondition(Not FActive, 'AddSignal', 'Cannot delete a signal to a prepared signal manager.'));

  FSignalList.DeleteByReference(oSignal);
End;


Procedure TFslSignalManager.DeleteAllSignals;
Begin
  Assert(CheckCondition(Not FActive, 'AddSignal', 'Cannot delete all signal with a prepared signal manager.'));

  FSignalList.Clear;
End;


Procedure TFslSignalManager.Prepare;
Var
  iSignalIndex : Integer;
  oSignal : TFslSignal;
Begin
  Assert(CheckCondition(Not FActive, 'Prepare', 'Cannot double prepare a signal manager.'));

  If (FSignalList.Count > MAXIMUM_WAIT_OBJECTS) Then
    RaiseError('AddSignal', StringFormat('The signal manager only supports up to %d signals', [MAXIMUM_WAIT_OBJECTS]));

  FActive := True;

  FSignalHandleCount := FSignalList.Count;

  For iSignalIndex := 0 To FSignalHandleCount - 1 Do
  Begin
    oSignal := FSignalList[iSignalIndex];

    If Not oSignal.Active Then
      RaiseError('Prepare', 'All signals must be active.');

    FSignalHandleArray[iSignalIndex] := oSignal.Handle;
  End;
End;


Procedure TFslSignalManager.Terminate;
Begin
  Assert(CheckCondition(FActive, 'Terminate', 'Cannot double terminate a signal manager.'));

  FSignalHandleCount := 0;
  FActive := False;
End;


Function TFslSignalManager.WaitTimeout(Const iTimeout: Cardinal; Const bAll: Boolean): Boolean;
Var
  iWaitResult : Cardinal;
Begin
  Assert(CheckCondition(FActive, 'WaitTimeout', 'Cannot wait on a signal manager that has not been prepared.'));

  iWaitResult := WaitForMultipleObjects(FSignalHandleCount, @FSignalHandleArray, bAll, iTimeout);

  if (iWaitResult = WAIT_FAILED) Then
    RaiseError('WaitTimeout', ErrorAsString);

  Result := (iWaitResult <> WAIT_TIMEOUT) And IntegerBetween(WAIT_OBJECT_0, iWaitResult, WAIT_OBJECT_0 + FSignalHandleCount - 1);
End;


Function TFslSignalManager.WaitTimeoutForAll(Const iTimeout: Cardinal): Boolean;
Begin
  Result := WaitTimeout(iTimeout, True);
End;


Function TFslSignalManager.WaitTimeoutForAny(Const iTimeout: Cardinal): Boolean;
Begin
  Result := WaitTimeout(iTimeout, False);
End;
 }

Procedure TFslMethodList.AssignItem(oItems : TFslItemList; iIndex : Integer);
Begin
  FMethodArray^[iIndex] := TFslMethodList(oItems).FMethodArray^[iIndex];
End;


Function TFslMethodList.CompareItem(pA, pB : Pointer) : Integer;
Begin
  Result := IntegerCompare(Integer(PFslMethodItem(pA)^.Code), Integer(PFslMethodItem(pB)^.Code));

  If Result = 0 Then
    Result := IntegerCompare(Integer(PFslMethodItem(pA)^.Data), Integer(PFslMethodItem(pB)^.Data));
End;


Procedure TFslMethodList.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMethodArray, Capacity * SizeOf(TFslMethodItem), iValue * SizeOf(TFslMethodItem));
End;


Procedure TFslMethodList.InternalEmpty(iIndex, iLength: Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMethodArray) + (iIndex * SizeOf(TFslMethodItem))), (iLength * SizeOf(TFslMethodItem)));
End;


Procedure TFslMethodList.InternalCopy(iSource, iTarget, iCount: Integer);
Begin
  Inherited;

  System.Move(FMethodArray^[iSource], FMethodArray^[iTarget], iCount * SizeOf(TFslMethodItem));
End;


Function TFslMethodList.IndexByValue(aValue : TFslMethod): Integer;
Begin
  If Not Find(aValue, Result) Then
    Result := -1;
End;


Function TFslMethodList.ExistsByValue(aValue: TFslMethod): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(aValue));
End;


Function TFslMethodList.Add(aValue : TFslMethod): Integer;
Begin
  Result := -1;

  If Not IsAllowDuplicates And Find(aValue, Result) Then
  Begin
    If IsPreventDuplicates Then
      RaiseError('Add', 'Item already exists in list');

    // Result is the index of the existing key
  End
  Else
  Begin
    If Not IsSorted Then
      Result := Count
    Else If (Result < 0) Then
      Find(aValue, Result);

    Insert(Result, aValue);
  End;
End;


Procedure TFslMethodList.Insert(iIndex : Integer; aValue : TFslMethod);
Begin
  InternalInsert(iIndex);

  FMethodArray^[iIndex] := TFslMethodItem(aValue);
End;


Procedure TFslMethodList.InternalInsert(iIndex : Integer);
Begin
  Inherited;

  FMethodArray^[iIndex].Code := Nil;
  FMethodArray^[iIndex].Data := Nil;
End;


Procedure TFslMethodList.DeleteByValue(aValue : TFslMethod);
Var
  iIndex : Integer;
Begin
  If Not Find(aValue, iIndex) Then
    RaiseError('DeleteByValue', 'Method not found in list');

  DeleteByIndex(iIndex);
End;


Procedure TFslMethodList.InternalExchange(iA, iB: Integer);
Var
  aTemp : TFslMethodItem;
Begin
  aTemp := FMethodArray^[iA];
  FMethodArray^[iA] := FMethodArray^[iB];
  FMethodArray^[iB] := aTemp;
End;


Function TFslMethodList.GetItem(iIndex : Integer): Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMethodArray^[iIndex];
End;


Procedure TFslMethodList.SetItem(iIndex : Integer; Value: Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FMethodArray^[iIndex] := PFslMethodItem(Value)^;
End;


Function TFslMethodList.GetMethodItem(iIndex : Integer): TFslMethod;
Begin
  Assert(ValidateIndex('GetMethodItem', iIndex));

  Result := TFslMethod(FMethodArray^[iIndex]);
End;


Procedure TFslMethodList.SetMethodItem(iIndex : Integer; Const aValue : TFslMethod);
Begin
  Assert(ValidateIndex('SetMethodItem', iIndex));

  FMethodArray^[iIndex] := TFslMethodItem(aValue);
End;


Function TFslMethodList.CapacityLimit : Integer;
Begin
  Result := High(TFslMethodItemArray);
End;


Function TFslMethodList.Find(aValue: TFslMethod; Out iResult: Integer): Boolean;
Begin
  Result := Inherited Find(@TFslMethodItem(aValue), iResult);
End;


Function TFslEventList.IndexByValue(aValue : TFslEvent): Integer;
Begin
  Result := Inherited IndexByValue(TFslMethod(aValue));
End;


Function TFslEventList.ExistsByValue(aValue: TFslEvent): Boolean;
Begin
  Result := Inherited ExistsByValue(TFslMethod(aValue));
End;


Function TFslEventList.Add(aValue : TFslEvent): Integer;
Begin
  Result := Inherited Add(TFslMethod(aValue));
End;


Procedure TFslEventList.Insert(iIndex : Integer; aValue : TFslEvent);
Begin
  Inherited Insert(iIndex, TFslMethod(aValue));
End;


Procedure TFslEventList.DeleteByValue(aValue : TFslEvent);
Begin
  Inherited DeleteByValue(TFslMethod(aValue));
End;


Function TFslEventList.GetEvent(iIndex : Integer): TFslEvent;
Begin
  Assert(ValidateIndex('GetEvent', iIndex));

  Result := TFslEvent(MethodByIndex[iIndex]);
End;


Procedure TFslEventList.SetEvent(iIndex : Integer; Const aValue : TFslEvent);
Begin
  Assert(ValidateIndex('SetEvent', iIndex));

  MethodByIndex[iIndex] := TFslMethod(aValue);
End;


Constructor TFslDispatched.Create;
Begin
  Inherited;

  FEventList := TFslEventList.Create;
End;


Destructor TFslDispatched.Destroy;
Begin
  FEventList.Free;

  Inherited;
End;


Constructor TFslDispatcher.Create;
Begin
  Inherited;

  FHashEntry := TFslDispatched(ItemNew);

  Capacity := 127;
End;


Destructor TFslDispatcher.Destroy;
Begin
  FHashEntry.Free;

  Inherited;
End;


Function TFslDispatcher.ItemClass : TFslHashEntryClass;
Begin
  Result := TFslDispatched;
End;


Function TFslDispatcher.ForceDispatched(Const sName: String): TFslDispatched;
Begin
  FHashEntry.Name := sName;

  Result := TFslDispatched(Force(FHashEntry));
End;


Function TFslDispatcher.GetDispatched(Const sName: String): TFslDispatched;
Begin
  FHashEntry.Name := sName;

  Result := TFslDispatched(Get(FHashEntry));
End;


Procedure TFslDispatcher.AddDispatched(Const sName : String; aEvent: TFslEvent);
Begin
  ForceDispatched(sName).Events.Add(aEvent);
End;


Procedure TFslDispatcher.RemoveDispatched(Const sName: String; aEvent: TFslEvent);
Var
  oDispatched : TFslDispatched;
Begin
  oDispatched := GetDispatched(sName);

  If Assigned(oDispatched) Then
  Begin
    oDispatched.Events.DeleteByValue(aEvent);

    If oDispatched.Events.Count <= 0 Then
      Delete(oDispatched);
  End;
End;


Procedure TFslDispatcher.Register(Const sName : String; aEvent : TFslEvent; bRegister : Boolean);
Begin

  If bRegister Then
    AddDispatched(sName, aEvent)
  Else
    RemoveDispatched(sName, aEvent);
End;


Function TFslDispatcher.Call(oSender: TFslObject; oDispatched : TFslDispatched) : Boolean;
Var
  iLoop : Integer;
  oFetched : TFslDispatched;
Begin
  oFetched := TFslDispatched(Get(oDispatched));

  Result := Assigned(oFetched);

  If Result Then
  Begin
    For iLoop := 0 To oFetched.Events.Count - 1 Do
      TFslEvent(oFetched.Events[iLoop])(oSender);
  End;
End;


Function TFslDispatcher.ExistsDispatched(Const sName: String): Boolean;
Begin
  FHashEntry.Name := sName;

  Result := Exists(FHashEntry);
End;


Procedure TFslStringDispatcher.Register(Const sName : String; aEvent : TFslStringEvent; bRegister : Boolean);
Begin
  TFslDispatcher(Self).Register(sName, TFslEvent(aEvent), bRegister);
End;


Function TFslStringDispatcher.CallString(oSender: TFslObject; Const sValue : String) : Boolean;
Var
  oDispatched : TFslDispatched;
  iLoop       : Integer;
Begin
  oDispatched := GetDispatched(sValue);

  Result := Assigned(oDispatched);

  If Result Then
  Begin
    For iLoop := 0 To oDispatched.Events.Count - 1 Do
      TFslStringEvent(oDispatched.Events[iLoop])(oSender, sValue);
  End;
End;


Procedure TFslObjectClassDispatcher.Register(Const aClass: TFslObjectClass; aEvent: TFslObjectClassEvent; bRegister : Boolean);
Begin
  TFslDispatcher(Self).Register(aClass.ClassName, TFslEvent(aEvent), bRegister);
End;


Procedure TFslObjectClassDispatcher.CallClass(oDispatched: TFslDispatched; oSender, oValue: TFslObject);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oDispatched.Events.Count - 1 Do
    TFslObjectClassEvent(oDispatched.Events[iLoop])(oSender, oValue);
End;


Function TFslObjectClassDispatcher.CallClass(oSender, oValue : TFslObject) : Boolean;
Var
  oDispatched : TFslDispatched;
Begin
  oDispatched := GetDispatched(oValue.ClassName);

  Result := Assigned(oDispatched);

  If Result Then
    CallClass(oDispatched, oSender, oValue);
End;


Procedure TFslObjectDispatcher.Register(Const aObject: TFslObject; aEvent: TFslObjectEvent; bRegister : Boolean);
Begin
  TFslDispatcher(Self).Register(Declare(aObject), TFslEvent(aEvent), bRegister);
End;


Function TFslObjectDispatcher.Declare(oObject: TFslObject): String;
Begin
  Result := IntegerToString(Integer(oObject));
End;


Procedure TFslIntegerDispatcher.Register(iValue: Integer; aEvent: TFslIntegerEvent; bRegister : Boolean);
Begin
  TFslDispatcher(Self).Register(IntegerToString(iValue), TFslEvent(aEvent), bRegister);
End;


Function TFslObjectDispatcher.Call(oSender: TFslObject; oDispatched: TFslDispatched): Boolean;
Var
  iLoop  : Integer;
  oValue : TFslObject;
  oFetched : TFslDispatched;
Begin
  oFetched := TFslDispatched(Get(oDispatched));

  Result := Assigned(oFetched);

  If Result Then
  Begin
    oValue := TFslObject(StringToInteger32(oFetched.Name));

    For iLoop := 0 To oFetched.Events.Count - 1 Do
      TFslObjectEvent(oFetched.Events[iLoop])(oSender, oValue);
  End;
End;


Function TFslIntegerDispatcher.CallInteger(oSender: TFslObject; iValue : Integer) : Boolean;
Begin
  Result := Call(oSender, GetDispatched(IntegerToString(iValue)));
End;

Function TFslController.Link: TFslController;
Begin
  Result := TFslController(Inherited Link);
End;


Function TFslController.ErrorClass: EFslExceptionClass;
Begin
  Result := EFslController;
End;


Procedure TFslController.Open;
Begin
End;


Procedure TFslController.Close;
Begin
End;


Function TFslControllerList.ItemClass : TFslObjectClass;
Begin
  Result := TFslController;
End;


Function TFslControllerList.GetControllerByIndex(Const iIndex : Integer) : TFslController;
Begin
  Result := TFslController(ObjectByIndex[iIndex]);
End;


Const
  setSwitches = ['/', '\', '-'];


Function TFslParameters.Switched(Const sSwitch: String): Boolean;
Begin
  Result := FindCmdLineSwitch(sSwitch, setSwitches, True);
End;


Function TFslParameters.Valid(iIndex: Integer): Boolean;
Begin
  Result := (iIndex >= 0) And (iIndex < Count);
End;


Function TFslParameters.Executable : String;
Begin
  Result := ParamStr(0);
End;


Function TFslParameters.Folder : String;
Begin
  Result := PathFolder(ParamStr(0));
End;


Function TFslParameters.GetCount : Integer;
Begin
  Result := ParamCount;
End;


Function TFslParameters.GetParameter(iIndex: Integer): String;
Begin
  Assert(CheckCondition(Valid(iIndex), 'GetParameter', StringFormat('Index not valid in bounds [0..%d]', [iIndex])));

  Result := ParamStr(iIndex + 1);
End;


Function TFslParameters.AsText : String;
Var
  iIndex : Integer;
Begin
  Result := '"' + Executable + '""';

  For iIndex := 0 To Count - 1 Do
    StringAppend(Result, Parameters[iIndex]);
End;


Function TFslParameters.Get(Const sKey: String): String;
Var
  iIndex : Integer;
Begin
  iIndex := IndexOf(sKey);

  If Valid(iIndex) Then
    Result := Values[iIndex]
  Else
    Result := '';
End;


Function TFslParameters.IndexOf(Const sKey: String): Integer;
Begin
  Result := Count - 1;
  While (Result >= 0) And Not StringEquals(sKey, Keys[Result]) Do
    Dec(Result);
End;


Function TFslParameters.Exists(Const sKey: String): Boolean;
Begin
  Result := Valid(IndexOf(sKey));
End;


Function TFslParameters.GetKey(iIndex: Integer): String;
Var
  sValue : String;
Begin
  GetKeyValue(iIndex, Result, sValue);
End;


Function TFslParameters.GetValue(iIndex: Integer): String;
Var
  sKey : String;
Begin
  GetKeyValue(iIndex, sKey, Result);
End;


Procedure TFslParameters.GetKeyValue(iIndex: Integer; Out sKey, sValue: String);
Var
  sParameter : String;
Begin
  sParameter := Parameters[iIndex];

  If Not StringSplit(sParameter, ':', sKey, sValue) Then
  Begin
    sKey := sParameter;
    sValue := '';
  End;

  sKey := StringTrimSetLeft(sKey, setSwitches);
End;



End.

