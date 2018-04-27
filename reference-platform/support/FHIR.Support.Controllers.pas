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
  FHIR.Support.System, FHIR.Support.Math, FHIR.Support.Strings, FHIR.Support.Filers, FHIR.Support.DateTime,
  FHIR.Support.Objects, FHIR.Support.Exceptions, FHIR.Support.Collections;

Type
  TAdvThreadHandle = TThreadHandle;
  TAdvThreadID = TThreadID;

  TAdvThreadDelegate = Procedure Of Object;

  TAdvThread = Class(TAdvObject)
    Private
      FInternal : TThread; // Handle to the Windows thread.
      FID : TAdvThreadID;         // Unique ID of the Windows thread.
      FActive : Boolean;          // Run thread has finished.
      FDelegate : TAdvThreadDelegate;

    Protected
      Procedure Execute; Virtual;
      Procedure Interrupt; Virtual;
      Function Running : Boolean; Virtual;

      Procedure ExecuteYield(Const iTimeout : Cardinal);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvThread;

      Procedure Open;
      Procedure Close;
      Procedure Stop; Virtual;

      Procedure Wait;

      Procedure Kill;

      Function Active : Boolean;

      Property ID : TAdvThreadID Read FID Write FID;
      Property Delegate : TAdvThreadDelegate Read FDelegate Write FDelegate;
//    Property Processor : Cardinal Write SetProcessor; // see comments in SetProcessor
  End;

  TAdvThreadClass = Class Of TAdvThread;

  TAdvThreadList = Class(TAdvObjectList)
    Private
      Function GetThread(iIndex: Integer): TAdvThread;

    Protected
      Function ItemClass : TAdvObjectClass; Override;

    Public
      Function Active : Boolean;

      Property Threads[iIndex : Integer] : TAdvThread Read GetThread; Default;
  End;

  TAdvObjectClass = FHIR.Support.Objects.TAdvObjectClass;

  TAdvExclusiveCriticalSection = Class(TAdvObject)
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

  TAdvExclusiveStateCriticalSection = Class(TAdvExclusiveCriticalSection)
    Private
      FThreadID : TAdvThreadHandle;
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
      Function IsLockedToThread(Const aThread : TAdvThreadHandle) : Boolean;
      Function IsLockedToCurrentThread : Boolean;
  End;


Type
  TAdvSignalHandle = THandle;

  TAdvSignal = Class(TAdvObject)
    Private
      FName : String;
      FHandle : TAdvSignalHandle;
      FAutoHide : Boolean;

    Protected
      Procedure Open(bShow: Boolean);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvSignal;

      Procedure OpenShow;
      Procedure OpenHide;
      Procedure Close;

      Procedure Show;
      Procedure Hide;
      Procedure Flash;
      Function Wait : Boolean;
      Function WaitTimeout(Const iTimeout : Cardinal) : Boolean;

      Function Active : Boolean;

      Property Handle : TAdvSignalHandle Read FHandle Write FHandle;
      Property Name : String Read FName Write FName;
      Property AutoHide : Boolean Read FAutoHide Write FAutoHide;
  End;

  TAdvSignalList = Class(TAdvObjectList)
    Private
      Function GetSignalByIndex(iIndex: Integer): TAdvSignal;

    Protected
      Function ItemClass : TAdvObjectClass; Override;

    Public
      Property SignalByIndex[iIndex : Integer] : TAdvSignal Read GetSignalByIndex; Default;
  End;

  TAdvSignalManager = Class(TAdvObject)
    Private
      FSignalList : TAdvSignalList;
      FSignalHandleArray : TWOHandleArray;
      FSignalHandleCount : Integer;
      FActive : Boolean;

      Function WaitTimeout(Const iTimeout : Cardinal; Const bAll : Boolean) : Boolean;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Prepare;
      Procedure Terminate;

      Procedure AddSignal(oSignal : TAdvSignal);
      Procedure DeleteSignal(oSignal : TAdvSignal);
      Procedure DeleteAllSignals;

      Function WaitTimeoutForAll(Const iTimeout : Cardinal) : Boolean;
      Function WaitTimeoutForAny(Const iTimeout : Cardinal) : Boolean;
  End;


Type
  TAdvMethod = Procedure Of Object;
  PAdvMethod = ^TAdvMethod;

  TAdvMethodItem = Record
    Code, Data : Pointer;
  End;

  PAdvMethodItem = ^TAdvMethodItem;

  TAdvMethodItemArray = Array[0..(MaxInt Div SizeOf(TAdvMethodItem)) - 1] Of TAdvMethodItem;
  PAdvMethodItemArray = ^TAdvMethodItemArray;

  TAdvMethodList = Class(TAdvItemList)
    Private
      FMethodArray : PAdvMethodItemArray;

      Function GetMethodItem(iIndex : Integer): TAdvMethod;
      Procedure SetMethodItem(iIndex : Integer; Const aValue : TAdvMethod);

    Protected
      Function GetItem(iIndex : Integer) : Pointer; Override;
      Procedure SetItem(iIndex : Integer; Value: Pointer); Override;

      Procedure AssignItem(oItems : TAdvItemList; iIndex : Integer); Override;

      Function CompareItem(pA, pB: Pointer): Integer; Overload; Override;

      Procedure InternalResize(iValue : Integer); Override;
      Procedure InternalCopy(iSource, iTarget, iCount: Integer); Override;
      Procedure InternalEmpty(iIndex, iLength : Integer); Override;
      Procedure InternalInsert(iIndex : Integer); Overload; Override;
      Procedure InternalExchange(iA, iB : Integer); Overload; Override;

      Function CapacityLimit : Integer; Override;

      Function Find(aValue : TAdvMethod; Out iResult : Integer) : Boolean;

    Public
      Function IndexByValue(aValue : TAdvMethod) : Integer;
      Function ExistsByValue(aValue: TAdvMethod): Boolean;

      Function Add(aValue : TAdvMethod) : Integer;
      Procedure Insert(iIndex : Integer; aValue : TAdvMethod);
      Procedure DeleteByValue(aValue : TAdvMethod);

      Property MethodByIndex[iIndex : Integer] : TAdvMethod Read GetMethodItem Write SetMethodItem; Default;
  End;

  TAdvEvent = Procedure (Sender : TAdvObject) Of Object;

  TAdvEventList = Class(TAdvMethodList)
    Private
      Function GetEvent(iIndex : Integer): TAdvEvent;
      Procedure SetEvent(iIndex : Integer; Const aValue : TAdvEvent);

    Public
      Function IndexByValue(aValue : TAdvEvent) : Integer;
      Function ExistsByValue(aValue : TAdvEvent) : Boolean;
      Function Add(aValue : TAdvEvent) : Integer;
      Procedure Insert(iIndex : Integer; aValue : TAdvEvent);
      Procedure DeleteByValue(aValue : TAdvEvent);

      Property EventByIndex[iIndex : Integer] : TAdvEvent Read GetEvent Write SetEvent; Default;
  End;



Type
  TAdvDispatched = Class(TAdvStringHashEntry)
    Private
      FEventList : TAdvEventList;

    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Override;

      Property Events : TAdvEventList Read FEventList;
  End;

  TAdvDispatcher = Class(TAdvStringHashTable)
    Private
      FHashEntry : TAdvDispatched;

    Protected
      Function ItemClass : TAdvHashEntryClass; Override;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure AddDispatched(Const sName : String; aEvent : TAdvEvent);
      Procedure RemoveDispatched(Const sName : String; aEvent : TAdvEvent);
      Function GetDispatched(Const sName : String) : TAdvDispatched;
      Function ForceDispatched(Const sName : String) : TAdvDispatched;
      Function ExistsDispatched(Const sName : String) : Boolean;

      Procedure Register(Const sName : String; aEvent : TAdvEvent; bRegister : Boolean = True); Overload;

      Function Call(oSender : TAdvObject; oDispatched : TAdvDispatched) : Boolean; Virtual;
  End;

  EAdvDispatcher = Class(EAdvException);

  TAdvStringEvent = Procedure (oSender : TAdvObject; Const sValue : String) Of Object;

  TAdvStringDispatcher = Class(TAdvDispatcher)
    Public
      Procedure Register(Const sName : String; aEvent : TAdvStringEvent; bRegister : Boolean = True); Overload;

      Function CallString(oSender : TAdvObject; Const sValue : String) : Boolean;
  End;

  TAdvIntegerEvent = Procedure (oSender : TAdvObject; Const iValue : Integer) Of Object;

  TAdvIntegerDispatcher = Class(TAdvDispatcher)
    Public
      Function CallInteger(oSender : TAdvObject; iValue : Integer) : Boolean; Overload;

      Procedure Register(iValue : Integer; aEvent : TAdvIntegerEvent; bRegister : Boolean = True); Overload;
  End;

  TAdvObjectClassEvent = Procedure (oSender : TAdvObject; Const oValue : TAdvObject) Of Object;

  TAdvObjectClassDispatcher = Class(TAdvDispatcher)
    Protected
      Procedure CallClass(oDispatched : TAdvDispatched; oSender, oValue : TAdvObject); Overload;

    Public
      Function CallClass(oSender, oValue : TAdvObject) : Boolean; Overload;

      Procedure Register(Const aClass : TAdvObjectClass; aEvent : TAdvObjectClassEvent; bRegister : Boolean = True); Overload;
  End;

  TAdvObjectEvent = Procedure (oSender : TAdvObject; Const oValue : TAdvObject) Of Object;

  TAdvObjectDispatcher = Class(TAdvDispatcher)
    Public
      Function Declare(oObject : TAdvObject) : String;

      Procedure Register(Const aObject : TAdvObject; aEvent : TAdvObjectEvent; bRegister : Boolean = True); Overload;

      Function Call(oSender : TAdvObject; oDispatched : TAdvDispatched) : Boolean; Overload; Override;
  End;

  TAdvController = Class(TAdvPersistent)
    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

    Public
      Function Link : TAdvController;

      Procedure Open; Virtual;
      Procedure Close; Virtual;
  End;

  EAdvController = Class(EAdvException);

  TAdvControllerList = Class(TAdvPersistentList)
    Private
      Function GetControllerByIndex(Const iIndex : Integer) : TAdvController;

    Protected
      Function ItemClass : TAdvObjectClass; Override;

    Public
      Property ControllerByIndex[Const iIndex : Integer] : TAdvController Read GetControllerByIndex; Default;
  End;

  TAdvParameters = Class(TAdvCollection)
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
    FOwner : TAdvThread;
  protected
    procedure Execute; override;
  public
    Constructor Create(thread : TAdvThread);
  end;

Constructor TAdvThread.Create;
Begin
  Inherited;

  FInternal := nil;
End;


Destructor TAdvThread.Destroy;
Begin
  Inherited;
End;


Function TAdvThread.Link: TAdvThread;
Begin
  Result := TAdvThread(Inherited Link);
End;


Procedure TAdvThread.Execute;
Begin
  If Assigned(FDelegate) Then
    FDelegate;
End;


Procedure TAdvThread.Interrupt;
Begin
End;


Function TAdvThread.Running: Boolean;
Begin
  Result := True;
End;



Procedure TAdvThread.Open;
Begin
  If FActive Then
    RaiseError('Open', 'Thread is already active.');

  FActive := True;

  System.IsMultiThread := True;

  FInternal := TInternalThread.create(self);
End;


Procedure TAdvThread.Close;
Begin
  FInternal.Terminate;
End;


Procedure TAdvThread.Kill;
Begin
  FInternal.Terminate;
  {$IFDEF MSWINDOWS}
  TerminateThread(FInternal.Handle, 0);
  {$ENDIF}
  FInternal.Free;
  FInternal := nil;

  FActive := False;
End;


Procedure TAdvThread.Stop;
Begin
  FActive := False;

  FInternal.Terminate;
End;


Procedure TAdvThread.Wait;
Begin
  FInternal.WaitFor;
End;


Procedure TAdvThread.ExecuteYield(Const iTimeout: Cardinal);
Begin
  FHIR.Support.System.ThreadSleep(iTimeout);
End;


Function TAdvThread.Active : Boolean;
Begin
  Result := FActive And Running;
End;


Function TAdvThreadList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvThread;
End;


Function TAdvThreadList.GetThread(iIndex: Integer): TAdvThread;
Begin
  Result := TAdvThread(ObjectByIndex[iIndex]);
End;


Function TAdvThreadList.Active : Boolean;
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

constructor TInternalThread.Create(thread: TAdvThread);
begin
  FOwner := thread;
  inherited create(false);
end;

procedure TInternalThread.execute;
begin
  Try
    FOwner.Execute;
  Except
    // ignore any further exceptions
  End;
  FOwner.FActive := False;
end;


Constructor TAdvExclusiveCriticalSection.Create;
Begin
  Inherited;

  InitializeCriticalSection(FHandle);
End;


Destructor TAdvExclusiveCriticalSection.Destroy;
Begin
  DeleteCriticalSection(FHandle);

  Inherited;
End;  


Procedure TAdvExclusiveCriticalSection.Lock;
Begin
  EnterCriticalSection(FHandle);
End;


Function TAdvExclusiveCriticalSection.TryLock : Boolean;
Begin 
  Result := TryEnterCriticalSection(FHandle);
End;  


Procedure TAdvExclusiveCriticalSection.Unlock;
Begin
  LeaveCriticalSection(FHandle);
End;


Procedure TAdvExclusiveStateCriticalSection.Lock;
Begin 
  Inherited;

  Enter;
End;  


Procedure TAdvExclusiveStateCriticalSection.Unlock;
Begin 
  Assert(CheckCondition(IsLockedToCurrentThread, 'Unlock', 'Cannot unlock as the critical section is not locked to the current thread.'));

  Leave;

  Inherited;
End;  


Function TAdvExclusiveStateCriticalSection.TryLock : Boolean;
Begin 
  Result := Inherited TryLock;

  If Result Then
    Enter;
End;  


Function TAdvExclusiveStateCriticalSection.IsLocked : Boolean;
Begin
  Result := FThreadID <> INVALID_HANDLE_VALUE;
End;


Function TAdvExclusiveStateCriticalSection.IsLockedToCurrentThread : Boolean;
Begin 
  Result := IsLockedToThread(GetCurrentThreadID);
End;  


Function TAdvExclusiveStateCriticalSection.IsLockedToThread(Const aThread: TAdvThreadHandle): Boolean;
Begin 
  Result := FThreadID = aThread;
End;  


Function TAdvExclusiveStateCriticalSection.IsNestedLocked : Boolean;
Begin 
  Result := FNested > 1;
End;  


Procedure TAdvExclusiveStateCriticalSection.Enter;
Begin 
  If FNested = 0 Then
    FThreadID := GetCurrentThreadID;

  Inc(FNested);
End;  


Procedure TAdvExclusiveStateCriticalSection.Leave;
Begin 
  Dec(FNested);

  If FNested = 0 Then
    FThreadID := INVALID_HANDLE_VALUE;
End;


Constructor TAdvSignal.Create;
Begin
  Inherited;

  FHandle := 0;
End;


Destructor TAdvSignal.Destroy;
Begin
  If Active Then
    Close;

  Inherited;
End;


Function TAdvSignal.Link: TAdvSignal;
Begin
  Result := TAdvSignal(Inherited Link);
End;


Procedure TAdvSignal.Open(bShow : Boolean);
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


Procedure TAdvSignal.OpenHide;
Begin
  Open(False);
End;


Procedure TAdvSignal.OpenShow;
Begin
  Open(True);
End;


Procedure TAdvSignal.Close;
Begin
  Assert(CheckCondition(Active, 'Close', 'Signal must be active.'));

  CloseHandle(FHandle);

  FHandle := 0;
End;


Procedure TAdvSignal.Hide;
Begin
  Assert(CheckCondition(Active, 'Hide', 'Signal must be active.'));

  If Not ResetEvent(FHandle) Then
    RaiseError('Hide', ErrorAsString);
End;


Procedure TAdvSignal.Show;
Begin
  Assert(CheckCondition(Active, 'Show', 'Signal must be active.'));

  If Not SetEvent(FHandle) Then
    RaiseError('Show', ErrorAsString);
End;


Procedure TAdvSignal.Flash;
Begin
  Assert(CheckCondition(Active, 'Flash', 'Signal must be active.'));

  If Not PulseEvent(FHandle) Then
    RaiseError('Flash', ErrorAsString);
End;


Function TAdvSignal.Wait : Boolean;
Begin
  Result := WaitTimeout(INFINITE);
End;


Function TAdvSignal.WaitTimeout(Const iTimeout: Cardinal) : Boolean;
Var
  iWaitResult : Cardinal;
Begin
  Assert(CheckCondition(Active, 'WaitTimeout', 'Signal must be active.'));

  iWaitResult := WaitForSingleObject(FHandle, iTimeout);

  if (iWaitResult = WAIT_FAILED) Then
    RaiseError('WaitTimeout', ErrorAsString);

  Result := iWaitResult = WAIT_OBJECT_0;
End;


Function TAdvSignalList.GetSignalByIndex(iIndex: Integer): TAdvSignal;
Begin
  Result := TAdvSignal(ObjectByIndex[iIndex]);
End;


Function TAdvSignalList.ItemClass: TAdvObjectClass;
Begin
  Result := TAdvSignal;
End;


Function TAdvSignal.Active: Boolean;
Begin
  Result := FHandle <> 0;
End;


Constructor TAdvSignalManager.Create;
Begin
  Inherited;

  FSignalList := TAdvSignalList.Create;
  FSignalList.SortedByReference;
  FSignalList.PreventDuplicates;
End;


Destructor TAdvSignalManager.Destroy;
Begin
  If FActive Then
    Terminate;

  FSignalList.Free;

  Inherited;
End;


Procedure TAdvSignalManager.AddSignal(oSignal: TAdvSignal);
Begin
  Assert(CheckCondition(Not FActive, 'AddSignal', 'Cannot add a signal to a prepared signal manager.'));

  If (FSignalList.Count >= MAXIMUM_WAIT_OBJECTS) Then
  Begin
    oSignal.Free;

    RaiseError('AddSignal', StringFormat('The signal manager only supports up to %d signals', [MAXIMUM_WAIT_OBJECTS]));
  End;

  FSignalList.Add(oSignal);
End;


Procedure TAdvSignalManager.DeleteSignal(oSignal: TAdvSignal);
Begin
  Assert(CheckCondition(Not FActive, 'AddSignal', 'Cannot delete a signal to a prepared signal manager.'));

  FSignalList.DeleteByReference(oSignal);
End;


Procedure TAdvSignalManager.DeleteAllSignals;
Begin
  Assert(CheckCondition(Not FActive, 'AddSignal', 'Cannot delete all signal with a prepared signal manager.'));

  FSignalList.Clear;
End;


Procedure TAdvSignalManager.Prepare;
Var
  iSignalIndex : Integer;
  oSignal : TAdvSignal;
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


Procedure TAdvSignalManager.Terminate;
Begin
  Assert(CheckCondition(FActive, 'Terminate', 'Cannot double terminate a signal manager.'));

  FSignalHandleCount := 0;
  FActive := False;
End;


Function TAdvSignalManager.WaitTimeout(Const iTimeout: Cardinal; Const bAll: Boolean): Boolean;
Var
  iWaitResult : Cardinal;
Begin
  Assert(CheckCondition(FActive, 'WaitTimeout', 'Cannot wait on a signal manager that has not been prepared.'));

  iWaitResult := WaitForMultipleObjects(FSignalHandleCount, @FSignalHandleArray, bAll, iTimeout);

  if (iWaitResult = WAIT_FAILED) Then
    RaiseError('WaitTimeout', ErrorAsString);

  Result := (iWaitResult <> WAIT_TIMEOUT) And IntegerBetween(WAIT_OBJECT_0, iWaitResult, WAIT_OBJECT_0 + FSignalHandleCount - 1);
End;


Function TAdvSignalManager.WaitTimeoutForAll(Const iTimeout: Cardinal): Boolean;
Begin
  Result := WaitTimeout(iTimeout, True);
End;


Function TAdvSignalManager.WaitTimeoutForAny(Const iTimeout: Cardinal): Boolean;
Begin
  Result := WaitTimeout(iTimeout, False);
End;

Procedure TAdvMethodList.AssignItem(oItems : TAdvItemList; iIndex : Integer);
Begin
  FMethodArray^[iIndex] := TAdvMethodList(oItems).FMethodArray^[iIndex];
End;


Function TAdvMethodList.CompareItem(pA, pB : Pointer) : Integer;
Begin
  Result := IntegerCompare(Integer(PAdvMethodItem(pA)^.Code), Integer(PAdvMethodItem(pB)^.Code));

  If Result = 0 Then
    Result := IntegerCompare(Integer(PAdvMethodItem(pA)^.Data), Integer(PAdvMethodItem(pB)^.Data));
End;


Procedure TAdvMethodList.InternalResize(iValue : Integer);
Begin
  Inherited;

  MemoryResize(FMethodArray, Capacity * SizeOf(TAdvMethodItem), iValue * SizeOf(TAdvMethodItem));
End;


Procedure TAdvMethodList.InternalEmpty(iIndex, iLength: Integer);
Begin
  Inherited;

  MemoryZero(Pointer(NativeUInt(FMethodArray) + (iIndex * SizeOf(TAdvMethodItem))), (iLength * SizeOf(TAdvMethodItem)));
End;


Procedure TAdvMethodList.InternalCopy(iSource, iTarget, iCount: Integer);
Begin
  Inherited;

  System.Move(FMethodArray^[iSource], FMethodArray^[iTarget], iCount * SizeOf(TAdvMethodItem));
End;


Function TAdvMethodList.IndexByValue(aValue : TAdvMethod): Integer;
Begin
  If Not Find(aValue, Result) Then
    Result := -1;
End;


Function TAdvMethodList.ExistsByValue(aValue: TAdvMethod): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(aValue));
End;


Function TAdvMethodList.Add(aValue : TAdvMethod): Integer;
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


Procedure TAdvMethodList.Insert(iIndex : Integer; aValue : TAdvMethod);
Begin
  InternalInsert(iIndex);

  FMethodArray^[iIndex] := TAdvMethodItem(aValue);
End;


Procedure TAdvMethodList.InternalInsert(iIndex : Integer);
Begin
  Inherited;

  FMethodArray^[iIndex].Code := Nil;
  FMethodArray^[iIndex].Data := Nil;
End;


Procedure TAdvMethodList.DeleteByValue(aValue : TAdvMethod);
Var
  iIndex : Integer;
Begin
  If Not Find(aValue, iIndex) Then
    RaiseError('DeleteByValue', 'Method not found in list');

  DeleteByIndex(iIndex);
End;


Procedure TAdvMethodList.InternalExchange(iA, iB: Integer);
Var
  aTemp : TAdvMethodItem;
Begin
  aTemp := FMethodArray^[iA];
  FMethodArray^[iA] := FMethodArray^[iB];
  FMethodArray^[iB] := aTemp;
End;


Function TAdvMethodList.GetItem(iIndex : Integer): Pointer;
Begin
  Assert(ValidateIndex('GetItem', iIndex));

  Result := @FMethodArray^[iIndex];
End;


Procedure TAdvMethodList.SetItem(iIndex : Integer; Value: Pointer);
Begin
  Assert(ValidateIndex('SetItem', iIndex));

  FMethodArray^[iIndex] := PAdvMethodItem(Value)^;
End;


Function TAdvMethodList.GetMethodItem(iIndex : Integer): TAdvMethod;
Begin
  Assert(ValidateIndex('GetMethodItem', iIndex));

  Result := TAdvMethod(FMethodArray^[iIndex]);
End;


Procedure TAdvMethodList.SetMethodItem(iIndex : Integer; Const aValue : TAdvMethod);
Begin
  Assert(ValidateIndex('SetMethodItem', iIndex));

  FMethodArray^[iIndex] := TAdvMethodItem(aValue);
End;


Function TAdvMethodList.CapacityLimit : Integer;
Begin
  Result := High(TAdvMethodItemArray);
End;


Function TAdvMethodList.Find(aValue: TAdvMethod; Out iResult: Integer): Boolean;
Begin
  Result := Inherited Find(@TAdvMethodItem(aValue), iResult);
End;


Function TAdvEventList.IndexByValue(aValue : TAdvEvent): Integer;
Begin
  Result := Inherited IndexByValue(TAdvMethod(aValue));
End;


Function TAdvEventList.ExistsByValue(aValue: TAdvEvent): Boolean;
Begin
  Result := Inherited ExistsByValue(TAdvMethod(aValue));
End;


Function TAdvEventList.Add(aValue : TAdvEvent): Integer;
Begin
  Result := Inherited Add(TAdvMethod(aValue));
End;


Procedure TAdvEventList.Insert(iIndex : Integer; aValue : TAdvEvent);
Begin
  Inherited Insert(iIndex, TAdvMethod(aValue));
End;


Procedure TAdvEventList.DeleteByValue(aValue : TAdvEvent);
Begin
  Inherited DeleteByValue(TAdvMethod(aValue));
End;


Function TAdvEventList.GetEvent(iIndex : Integer): TAdvEvent;
Begin
  Assert(ValidateIndex('GetEvent', iIndex));

  Result := TAdvEvent(MethodByIndex[iIndex]);
End;


Procedure TAdvEventList.SetEvent(iIndex : Integer; Const aValue : TAdvEvent);
Begin
  Assert(ValidateIndex('SetEvent', iIndex));

  MethodByIndex[iIndex] := TAdvMethod(aValue);
End;


Constructor TAdvDispatched.Create;
Begin
  Inherited;

  FEventList := TAdvEventList.Create;
End;


Destructor TAdvDispatched.Destroy;
Begin
  FEventList.Free;

  Inherited;
End;


Constructor TAdvDispatcher.Create;
Begin
  Inherited;

  FHashEntry := TAdvDispatched(ItemNew);

  Capacity := 127;
End;


Destructor TAdvDispatcher.Destroy;
Begin
  FHashEntry.Free;

  Inherited;
End;


Function TAdvDispatcher.ItemClass : TAdvHashEntryClass;
Begin
  Result := TAdvDispatched;
End;


Function TAdvDispatcher.ForceDispatched(Const sName: String): TAdvDispatched;
Begin
  FHashEntry.Name := sName;

  Result := TAdvDispatched(Force(FHashEntry));
End;


Function TAdvDispatcher.GetDispatched(Const sName: String): TAdvDispatched;
Begin
  FHashEntry.Name := sName;

  Result := TAdvDispatched(Get(FHashEntry));
End;


Procedure TAdvDispatcher.AddDispatched(Const sName : String; aEvent: TAdvEvent);
Begin
  ForceDispatched(sName).Events.Add(aEvent);
End;


Procedure TAdvDispatcher.RemoveDispatched(Const sName: String; aEvent: TAdvEvent);
Var
  oDispatched : TAdvDispatched;
Begin
  oDispatched := GetDispatched(sName);

  If Assigned(oDispatched) Then
  Begin
    oDispatched.Events.DeleteByValue(aEvent);

    If oDispatched.Events.Count <= 0 Then
      Delete(oDispatched);
  End;
End;


Procedure TAdvDispatcher.Register(Const sName : String; aEvent : TAdvEvent; bRegister : Boolean);
Begin

  If bRegister Then
    AddDispatched(sName, aEvent)
  Else
    RemoveDispatched(sName, aEvent);
End;


Function TAdvDispatcher.Call(oSender: TAdvObject; oDispatched : TAdvDispatched) : Boolean;
Var
  iLoop : Integer;
  oFetched : TAdvDispatched;
Begin
  oFetched := TAdvDispatched(Get(oDispatched));

  Result := Assigned(oFetched);

  If Result Then
  Begin
    For iLoop := 0 To oFetched.Events.Count - 1 Do
      TAdvEvent(oFetched.Events[iLoop])(oSender);
  End;
End;


Function TAdvDispatcher.ExistsDispatched(Const sName: String): Boolean;
Begin
  FHashEntry.Name := sName;

  Result := Exists(FHashEntry);
End;


Procedure TAdvStringDispatcher.Register(Const sName : String; aEvent : TAdvStringEvent; bRegister : Boolean);
Begin
  TAdvDispatcher(Self).Register(sName, TAdvEvent(aEvent), bRegister);
End;


Function TAdvStringDispatcher.CallString(oSender: TAdvObject; Const sValue : String) : Boolean;
Var
  oDispatched : TAdvDispatched;
  iLoop       : Integer;
Begin
  oDispatched := GetDispatched(sValue);

  Result := Assigned(oDispatched);

  If Result Then
  Begin
    For iLoop := 0 To oDispatched.Events.Count - 1 Do
      TAdvStringEvent(oDispatched.Events[iLoop])(oSender, sValue);
  End;
End;


Procedure TAdvObjectClassDispatcher.Register(Const aClass: TAdvObjectClass; aEvent: TAdvObjectClassEvent; bRegister : Boolean);
Begin
  TAdvDispatcher(Self).Register(aClass.ClassName, TAdvEvent(aEvent), bRegister);
End;


Procedure TAdvObjectClassDispatcher.CallClass(oDispatched: TAdvDispatched; oSender, oValue: TAdvObject);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oDispatched.Events.Count - 1 Do
    TAdvObjectClassEvent(oDispatched.Events[iLoop])(oSender, oValue);
End;


Function TAdvObjectClassDispatcher.CallClass(oSender, oValue : TAdvObject) : Boolean;
Var
  oDispatched : TAdvDispatched;
Begin
  oDispatched := GetDispatched(oValue.ClassName);

  Result := Assigned(oDispatched);

  If Result Then
    CallClass(oDispatched, oSender, oValue);
End;


Procedure TAdvObjectDispatcher.Register(Const aObject: TAdvObject; aEvent: TAdvObjectEvent; bRegister : Boolean);
Begin
  TAdvDispatcher(Self).Register(Declare(aObject), TAdvEvent(aEvent), bRegister);
End;


Function TAdvObjectDispatcher.Declare(oObject: TAdvObject): String;
Begin
  Result := IntegerToString(Integer(oObject));
End;


Procedure TAdvIntegerDispatcher.Register(iValue: Integer; aEvent: TAdvIntegerEvent; bRegister : Boolean);
Begin
  TAdvDispatcher(Self).Register(IntegerToString(iValue), TAdvEvent(aEvent), bRegister);
End;


Function TAdvObjectDispatcher.Call(oSender: TAdvObject; oDispatched: TAdvDispatched): Boolean;
Var
  iLoop  : Integer;
  oValue : TAdvObject;
  oFetched : TAdvDispatched;
Begin
  oFetched := TAdvDispatched(Get(oDispatched));

  Result := Assigned(oFetched);

  If Result Then
  Begin
    oValue := TAdvObject(StringToInteger32(oFetched.Name));

    For iLoop := 0 To oFetched.Events.Count - 1 Do
      TAdvObjectEvent(oFetched.Events[iLoop])(oSender, oValue);
  End;
End;


Function TAdvIntegerDispatcher.CallInteger(oSender: TAdvObject; iValue : Integer) : Boolean;
Begin
  Result := Call(oSender, GetDispatched(IntegerToString(iValue)));
End;

Function TAdvController.Link: TAdvController;
Begin
  Result := TAdvController(Inherited Link);
End;


Function TAdvController.ErrorClass: EAdvExceptionClass;
Begin
  Result := EAdvController;
End;


Procedure TAdvController.Open;
Begin
End;


Procedure TAdvController.Close;
Begin
End;


Function TAdvControllerList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvController;
End;


Function TAdvControllerList.GetControllerByIndex(Const iIndex : Integer) : TAdvController;
Begin
  Result := TAdvController(ObjectByIndex[iIndex]);
End;


Const
  setSwitches = ['/', '\', '-'];


Function TAdvParameters.Switched(Const sSwitch: String): Boolean;
Begin
  Result := FindCmdLineSwitch(sSwitch, setSwitches, True);
End;


Function TAdvParameters.Valid(iIndex: Integer): Boolean;
Begin
  Result := (iIndex >= 0) And (iIndex < Count);
End;


Function TAdvParameters.Executable : String;
Begin
  Result := ParamStr(0);
End;


Function TAdvParameters.Folder : String;
Begin
  Result := PathFolder(ParamStr(0));
End;


Function TAdvParameters.GetCount : Integer;
Begin
  Result := ParamCount;
End;


Function TAdvParameters.GetParameter(iIndex: Integer): String;
Begin
  Assert(CheckCondition(Valid(iIndex), 'GetParameter', StringFormat('Index not valid in bounds [0..%d]', [iIndex])));

  Result := ParamStr(iIndex + 1);
End;


Function TAdvParameters.AsText : String;
Var
  iIndex : Integer;
Begin
  Result := '"' + Executable + '""';

  For iIndex := 0 To Count - 1 Do
    StringAppend(Result, Parameters[iIndex]);
End;


Function TAdvParameters.Get(Const sKey: String): String;
Var
  iIndex : Integer;
Begin
  iIndex := IndexOf(sKey);

  If Valid(iIndex) Then
    Result := Values[iIndex]
  Else
    Result := '';
End;


Function TAdvParameters.IndexOf(Const sKey: String): Integer;
Begin
  Result := Count - 1;
  While (Result >= 0) And Not StringEquals(sKey, Keys[Result]) Do
    Dec(Result);
End;


Function TAdvParameters.Exists(Const sKey: String): Boolean;
Begin
  Result := Valid(IndexOf(sKey));
End;


Function TAdvParameters.GetKey(iIndex: Integer): String;
Var
  sValue : String;
Begin
  GetKeyValue(iIndex, Result, sValue);
End;


Function TAdvParameters.GetValue(iIndex: Integer): String;
Var
  sKey : String;
Begin
  GetKeyValue(iIndex, sKey, Result);
End;


Procedure TAdvParameters.GetKeyValue(iIndex: Integer; Out sKey, sValue: String);
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



End. // AdvExclusiveCriticalSections //
