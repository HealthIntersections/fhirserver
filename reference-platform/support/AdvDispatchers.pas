Unit AdvDispatchers;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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
  StringSupport,
  AdvObjects, AdvHashes, AdvStringHashes, AdvExceptions, AdvEvents;


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


Implementation


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


End. // AdvDispatchers //
