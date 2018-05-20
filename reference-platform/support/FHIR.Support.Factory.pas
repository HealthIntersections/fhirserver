Unit FHIR.Support.Factory;

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
  Windows, // MessageBox
  SysUtils, // Exceptions
  FHIR.Support.Strings, FHIR.Support.System, FHIR.Support.DateTime,
  FHIR.Support.Objects, FHIR.Support.Exceptions, FHIR.Support.Controllers,
  FHIR.Support.Stream, FHIR.Support.Collections;


Type
  TFslFactorySerial = Cardinal;

  TFslFactory = Class(TFslObject)
    Private
      FClassHashTable : TFslObjectClassHashTable;            // Class factory
      FLookupClassHashEntry : TFslObjectClassHashEntry;  // Accessor to the class factory
      FCritical : TFslExclusiveCriticalSection;                   // Critical section for instance repository
      FCriticalDepth : Integer;                   // Number of nested critical section locks.
      FProfiled : Boolean;                        // Save the profile report when the application finishes.
      FLaunched : Boolean;                        // Launch the profile   report when the application finishes.
      FFolder : String;                           // Filename location of leak profile report.

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Active : Boolean;

      Function ActiveClassTracking : Boolean;

      // Called from Initialise/Finalise.
      Procedure Open;
      Procedure Close;

      // For locking before accessing property Objects.
      Procedure Lock;
      Procedure Unlock;
      Function Nested : Boolean;

      Function Get(Const sName : String) : TClass;
      Function Make(Const sName : String) : TObject; Overload;
      Function Make(Const aClass : TClass) : TObject; Overload;

      // Register Objects (Always returns True so can be called from Assert without raising an error and compiled out when $C-).
      Function Construct(oObject : TObject; Out iSerial : TFslFactorySerial) : Boolean; Overload;
      Function Construct(oObject : TObject) : Boolean; Overload;
      Function Destruct(oObject : TObject) : Boolean; Overload;
      Function Valid(oObject : TObject) : Boolean;

      // Register Breakpoints
      Procedure Track(oObject : TObject);
      Procedure Notify(oObject : TObject);

      // Object freezing
      Function Freeze(oObject : TObject) : Boolean;
      Function IsFrozen(oObject : TObject) : Boolean;

      Function MarkDestructable(oObject : TObject) : Boolean;
      Function MarkIndestructable(oObject : TObject) : Boolean;
      Function IsIndestructable(oObject : TObject) : Boolean;

      Property ClassHashTable : TFslObjectClassHashTable Read FClassHashTable;
      Property Profiled : Boolean Read FProfiled Write FProfiled;
      Property Launched : Boolean Read FLaunched Write FLaunched;
      Property Folder : String Read FFolder Write FFolder;
  End;

  EFslFactory = Class(EFslException);

  TFslObjectClass = FHIR.Support.Objects.TFslObjectClass;


Const
  SERIAL_INVALID = High(TFslFactorySerial);


Function Factory : TFslFactory;
Function HasFactory : Boolean;


Implementation


Var
  gFactory : TFslFactory;


Function Factory : TFslFactory;
Begin
  Result := gFactory;
End;


Function HasFactory : Boolean;
Begin
  Result := Assigned(gFactory);
End;


Constructor TFslFactory.Create;
Begin
  Inherited;

  FProfiled := True;
  FLaunched := True;
  FFolder := SystemTemp;

  FClassHashTable := TFslObjectClassHashTable.Create;
  FLookupClassHashEntry := TFslObjectClassHashEntry.Create;

  FCritical := TFslExclusiveCriticalSection.Create;


  FClassHashTable.Capacity := 50021; // 0.2MB of array pointers
End;


Destructor TFslFactory.Destroy;
Begin
  FCritical.Free;
  FClassHashTable.Free;
  FLookupClassHashEntry.Free;

  Inherited;
End;


Function TFslFactory.ActiveClassTracking: Boolean;
Begin
  Result := Active;
End;



Function TFslFactory.Active : Boolean;
Begin
  Result := Assigned(Self);
End;



Procedure TFslFactory.Open;
Begin
  // Add the following objects to the Object hash.
  Assert(Construct(Self));
  Assert(Construct(FLookupClassHashEntry));
  Assert(Construct(FCritical));
  Assert(Construct(FClassHashTable));
End;


Procedure TFslFactory.Close;
Begin
  // Ensure the classes HashEntrys are destroyed.
  FClassHashTable.Clear;

  // Remove the following objects from the object hash.
  Assert(Destruct(FCritical));
  Assert(Destruct(FLookupClassHashEntry));
  Assert(Destruct(FClassHashTable));
  Assert(Destruct(Self));

  // If there are no memory leaks then the object hash should be empty.
End;


Function TFslFactory.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslFactory;
End;


Function TFslFactory.Get(Const sName: String): TClass;
Var
  oLookup : TFslObjectClassHashEntry;
  oClass  : TFslObjectClassHashEntry;
Begin 
  oLookup := TFslObjectClassHashEntry.Create;
  Try
    oLookup.Name := sName;

    oClass := TFslObjectClassHashEntry(FClassHashTable.Get(oLookup));

    If Assigned(oClass) Then
      Result := oClass.Data
    Else
      Result := Nil;
  Finally
    oLookup.Free;
  End;
End;


Function TFslFactory.Make(Const sName : String) : TObject;
Var
  aClass : TClass;
Begin
  aClass := Get(sName);

  If Not Assigned(aClass) Then
    RaiseError('Make', StringFormat('Class ''%s'' was not registered with the factory.', [sName]));

  Result := Make(aClass);
End;  


Function TFslFactory.Make(Const aClass: TClass): TObject;
Begin
  If aClass.InheritsFrom(TFslObject) Then
    Result := TFslObjectClass(aClass).Create
  Else
    Result := aClass.Create;
End;


Procedure TFslFactory.Lock;
Begin 
  FCritical.Lock;
  Inc(FCriticalDepth);
End;  


Procedure TFslFactory.Unlock;
Begin 
  Dec(FCriticalDepth);
  FCritical.Unlock;
End;  


Function TFslFactory.Nested : Boolean;
Begin
  Result := (FCriticalDepth > 1);
End;


Function TFslFactory.Construct(oObject: TObject; Out iSerial : TFslFactorySerial): Boolean;
Begin 
  // Add the object to the Object hash and return a unique serial number.

  Result := True;
  iSerial := SERIAL_INVALID;
End;


Function TFslFactory.Construct(oObject: TObject) : Boolean;
Begin
  // Add the object to the Object hash.

  Result := True;

End;  


Function TFslFactory.Destruct(oObject: TObject) : Boolean;
Begin 
  // Remove the object from the Object hash.

  Result := True;

End;


Function TFslFactory.Valid(oObject: TObject): Boolean;
Begin 
  // Returns true if the object is assigned, valid and of the correct class.

  Result := True;

End;


Procedure TFslFactory.Track(oObject: TObject);
Begin 
  // Mark the Object as requiring a breakpoint if Notify is called with the same Object.

End;


Procedure TFslFactory.Notify(oObject: TObject);
Begin
  // Interrupt the application execution with a breakpoint if the Object has been tracked.

End;


Function TFslFactory.MarkDestructable(oObject: TObject) : Boolean;
Begin 
  // Mark the object as Destructable - not ever needing to be freed and won't turn up in leak profiler.


  Result := True;
End;


Function TFslFactory.MarkIndestructable(oObject: TObject) : Boolean;
Begin 
  // Mark the object as Indestructable - not ever needing to be freed and won't turn up in leak profiler.


  Result := True;
End;  


Function TFslFactory.IsIndestructable(oObject: TObject): Boolean;
Begin 
  Result := False;

End;  


Function TFslFactory.Freeze(oObject: TObject): Boolean;
Begin 
  Result := True;

  // Mark the object as frozen (immutable).
End;


Function TFslFactory.IsFrozen(oObject: TObject): Boolean;
Begin 
  Result := False;

End;



Procedure Initialise;
Begin 
  gFactory := TFslFactory.Create;
  gFactory.Open;
End;  


Procedure Finalise;
Var
  oTemporary : TFslFactory;
Begin 
  Try
    gFactory.Close;

    oTemporary := gFactory;
    Try
      gFactory := Nil;

//      If oTemporary.Profiled And oTemporary.Lives Then
 //       oTemporary.Report;
    Finally
      oTemporary.Free;
    End;  
  Except
    MessageBox(0, PChar(ExceptObject.Message), 'Finalise', MB_OK);
  End;  
End;


Initialization
  Initialise;
Finalization
  Finalise;
End.

