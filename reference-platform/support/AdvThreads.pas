Unit AdvThreads;

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
  ThreadSupport, MathSupport, ErrorSupport,
  AdvExceptions, AdvObjects, AdvObjectLists;


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

  TAdvObjectClass = AdvObjects.TAdvObjectClass;


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
  ThreadSupport.ThreadSleep(iTimeout);
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


End. // AdvThreads //
