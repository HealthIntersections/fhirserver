Unit AdvStreamFilers;

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
  MathSupport, HashSupport,
  AdvItems, AdvObjects, AdvPersistents, AdvStreams, AdvFilers, AdvHashes;


Type
  TAdvStreamFilerReferenceHashEntry = Class(TAdvHashEntry)
    Private
      FKey : Pointer;
      FValue : Pointer;

      Procedure SetKey(Const Value: Pointer);

    Protected
      Procedure Generate; Override;

    Public
      Procedure Assign(oSource : TAdvObject); Override;

      Property Key : Pointer Read FKey Write SetKey;
      Property Value : Pointer Read FValue Write FValue;
  End;

  TAdvStreamFilerReferenceHashTable = Class(TAdvHashTable)
    Protected
      Function ItemClass : TAdvHashEntryClass; Override;

      Function Equal(oA, oB : TAdvHashEntry) : Integer; Override;
  End;

  TAdvStreamFilerReferenceManager = Class(TAdvObject)
    Private
      FHashTable : TAdvStreamFilerReferenceHashTable;
      FLookupHashEntry : TAdvStreamFilerReferenceHashEntry;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvStreamFilerReferenceManager;

      Procedure Clear;

      Procedure Bind(oKey, oValue : TAdvPersistent); 
      Function Get(oKey : TAdvPersistent) : TAdvPersistent; 
      Function Exists(oKey : TAdvPersistent) : Boolean; 

      Property HashTable : TAdvStreamFilerReferenceHashTable Read FHashTable;
  End;

  TAdvStreamFilerResourceManager = Class(TAdvObject)
    Public
      Function Link : TAdvStreamFilerResourceManager;

      Procedure Clear; Virtual;

      Function ResolveObject(Const sResource : String; Const aClass : TAdvObjectClass) : TAdvObject; Virtual;
      Function ResolveID(Const oObject : TAdvObject) : String; Virtual;
  End;

  TAdvStreamFiler = Class(TAdvFiler)
    Private
      FStream : TAdvStream;
      FReferenceManager : TAdvStreamFilerReferenceManager;
      FResourceManager : TAdvStreamFilerResourceManager;
      FReferential : Boolean;
      FPermitExternalStreamManipulation : Boolean;

    {$IFOPT C+}
      Function GetResourceManager: TAdvStreamFilerResourceManager;
    {$ENDIF}
      Procedure SetResourceManager(Const Value: TAdvStreamFilerResourceManager);

    {$IFOPT C+}
      Function GetReferenceManager: TAdvStreamFilerReferenceManager;
    {$ENDIF}
      Procedure SetReferenceManager(Const Value: TAdvStreamFilerReferenceManager);

    {$IFOPT C+}
      Function GetStream: TAdvStream;
    {$ENDIF}
      Procedure SetStream(oStream : TAdvStream);

    Protected
      Procedure ApplyStream; Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvStreamFiler;

      Procedure Clear; Virtual; 

      Function HasResourceManager : Boolean;
      Function HasReferenceManager : Boolean;
      Function HasStream : Boolean;

      Property Stream : TAdvStream Read {$IFOPT C+}GetStream{$ELSE}FStream{$ENDIF} Write SetStream;
      Property ResourceManager : TAdvStreamFilerResourceManager Read {$IFOPT C+}GetResourceManager{$ELSE}FResourceManager{$ENDIF} Write SetResourceManager;
      Property ReferenceManager : TAdvStreamFilerReferenceManager Read {$IFOPT C+}GetReferenceManager{$ELSE}FReferenceManager{$ENDIF} Write SetReferenceManager;
      Property Referential : Boolean Read FReferential Write FReferential;
      Property PermitExternalStreamManipulation : Boolean Read FPermitExternalStreamManipulation Write FPermitExternalStreamManipulation;
  End;

  TAdvStreamFilerClass = Class Of TAdvStreamFiler;

  TAdvObject = AdvObjects.TAdvObject;
  TAdvObjectClass = AdvObjects.TAdvObjectClass;


Implementation


Procedure TAdvStreamFilerReferenceHashEntry.Assign(oSource: TAdvObject);
Begin
  Inherited;

  Key := TAdvStreamFilerReferenceHashEntry(oSource).Key;
  Value := TAdvStreamFilerReferenceHashEntry(oSource).Value;
End;


Function TAdvStreamFilerReferenceHashTable.Equal(oA, oB: TAdvHashEntry): Integer;
Begin
  Result := Inherited Equal(oA, oB);

  If Result = 0 Then
    Result := IntegerCompare(Integer(TAdvStreamFilerReferenceHashEntry(oA).Key), Integer(TAdvStreamFilerReferenceHashEntry(oB).Key));
End;


Function TAdvStreamFilerReferenceHashTable.ItemClass: TAdvHashEntryClass;
Begin
  Result := TAdvStreamFilerReferenceHashEntry;
End;


Procedure TAdvStreamFilerReferenceHashEntry.Generate;
Begin
  Inherited;

  Code := HashIntegerToCode32(Integer(FKey));
End;


Procedure TAdvStreamFilerReferenceHashEntry.SetKey(Const Value: Pointer);
Begin
  If FKey <> Value Then
  Begin
    FKey := Value;
    Generate;
  End;
End;


Constructor TAdvStreamFilerReferenceManager.Create;
Begin
  Inherited;

  FHashTable := TAdvStreamFilerReferenceHashTable.Create;
  FHashTable.Capacity := 47;

  FLookupHashEntry := TAdvStreamFilerReferenceHashEntry.Create;
End;


Destructor TAdvStreamFilerReferenceManager.Destroy;
Begin
  FHashTable.Free;
  FLookupHashEntry.Free;

  Inherited;
End;


Function TAdvStreamFilerReferenceManager.Link: TAdvStreamFilerReferenceManager;
Begin
  Result := TAdvStreamFilerReferenceManager(Inherited Link);
End;


Procedure TAdvStreamFilerReferenceManager.Clear;
Begin
  FHashTable.Clear;
End;


Procedure TAdvStreamFilerReferenceManager.Bind(oKey, oValue: TAdvPersistent);
Var
  oHashEntry : TAdvStreamFilerReferenceHashEntry;
Begin
  If Assigned(oKey) Then
  Begin
    oHashEntry := TAdvStreamFilerReferenceHashEntry.Create;
    oHashEntry.Key := Pointer(oKey);
    oHashEntry.Value := Pointer(oValue);
    FHashTable.Add(oHashEntry);
  End;
End;


Function TAdvStreamFilerReferenceManager.Get(oKey : TAdvPersistent): TAdvPersistent;
Var
  oHashEntry : TAdvStreamFilerReferenceHashEntry;
Begin
  FLookupHashEntry.Key := Pointer(oKey);

  oHashEntry := TAdvStreamFilerReferenceHashEntry(FHashTable.Get(FLookupHashEntry));

  If Assigned(oHashEntry) Then
    Result := TAdvPersistent(oHashEntry.Value)
  Else
    Result := Nil;
End;


Function TAdvStreamFilerReferenceManager.Exists(oKey: TAdvPersistent): Boolean;
Begin
  FLookupHashEntry.Key := Pointer(oKey);

  Result := FHashTable.Exists(FLookupHashEntry);
End;


Procedure TAdvStreamFilerResourceManager.Clear;
Begin
End;


Function TAdvStreamFilerResourceManager.ResolveObject(Const sResource: String; Const aClass : TAdvObjectClass): TAdvObject;
Begin
  RaiseError('ResolveObject', 'ResolveObject must be overriden.');

  Result := Nil;
End;  


Function TAdvStreamFilerResourceManager.ResolveID(Const oObject: TAdvObject): String;
Begin 
  RaiseError('ResolveID', 'ResolveObject must be overriden.');

  Result := '';
End;  


Function TAdvStreamFilerResourceManager.Link : TAdvStreamFilerResourceManager;
Begin
  Result := TAdvStreamFilerResourceManager(Inherited Link);
End;


Constructor TAdvStreamFiler.Create;
Begin
  Inherited;

  FReferenceManager := TAdvStreamFilerReferenceManager.Create;
  FResourceManager := Nil;
  FStream := Nil;

  FReferential := True;
End;


Destructor TAdvStreamFiler.Destroy;
Begin
  FStream.Free;
  FReferenceManager.Free;
  FResourceManager.Free;

  Inherited;
End;


Function TAdvStreamFiler.Link : TAdvStreamFiler;
Begin
  Result := TAdvStreamFiler(Inherited Link);
End;


{$IFOPT C+}
Function TAdvStreamFiler.GetStream: TAdvStream;
Begin
  Assert(Invariants('GetStream', FStream, TAdvStream, 'FStream'));

  Result := FStream;
End;
{$ENDIF}


Procedure TAdvStreamFiler.SetStream(oStream : TAdvStream);
Begin
  Assert(Not Assigned(oStream) Or Invariants('SetStream', oStream, TAdvStream, 'oStream'));

  FStream.Free;
  FStream := oStream;

  ApplyStream;
  Clear;
End;


Function TAdvStreamFiler.HasStream: Boolean;
Begin
  Result := Assigned(FStream);
End;


{$IFOPT C+}
Function TAdvStreamFiler.GetResourceManager : TAdvStreamFilerResourceManager;
Begin
  Assert(Invariants('GetResourceManager', FResourceManager, TAdvStreamFilerResourceManager, 'FResourceManager'));

  Result := FResourceManager;
End;
{$ENDIF}


Procedure TAdvStreamFiler.SetResourceManager(Const Value: TAdvStreamFilerResourceManager);
Begin
  Assert((Not Assigned(Value)) Or Invariants('SetResourceManager', Value, TAdvStreamFilerResourceManager, 'Value'));

  FResourceManager.Free;
  FResourceManager := Value;
End;


Function TAdvStreamFiler.HasResourceManager : Boolean;
Begin
  Result := Assigned(FResourceManager);
End;


{$IFOPT C+}
Function TAdvStreamFiler.GetReferenceManager : TAdvStreamFilerReferenceManager;
Begin
  Assert(Invariants('GetReferenceManager', FReferenceManager, TAdvStreamFilerReferenceManager, 'FReferenceManager'));

  Result := FReferenceManager;
End;
{$ENDIF}


Procedure TAdvStreamFiler.SetReferenceManager(Const Value: TAdvStreamFilerReferenceManager);
Begin
  Assert((Not Assigned(Value)) Or Invariants('SetReferenceManager', Value, TAdvStreamFilerReferenceManager, 'Value'));

  FReferenceManager.Free;
  FReferenceManager := Value;
End;


Function TAdvStreamFiler.HasReferenceManager : Boolean;
Begin
  Result := Assigned(FReferenceManager);
End;


Procedure TAdvStreamFiler.ApplyStream;
Begin
End;


Procedure TAdvStreamFiler.Clear;
Begin
  If HasReferenceManager Then
    ReferenceManager.Clear;

  If HasResourceManager Then
    ResourceManager.Clear;
End;


End. // AdvStreamFilers //
