Unit FHIR.Support.Stream;

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
  {$IFDEF MACOS} FHIR.Support.Osx, {$ELSE} Windows, ActiveX, {$ENDIF}
  SysUtils, Classes,
  FHIR.Support.Objects, FHIR.Support.Collections, FHIR.Support.Exceptions, FHIR.Support.System;



type

  TFslStream = Class(TFslObject)
    Protected
      Function ErrorClass : EFslExceptionClass; Override;

    Public
      Function Link : TFslStream;

      Function Assignable : Boolean; Override;

      Procedure Read(Var Buffer; iCount : Cardinal); Virtual; // can't mark as overload
      Procedure Write(Const Buffer; iCount : Cardinal); Virtual; // can't mark as overload

      Function Readable : Int64; Virtual;
      Function Writeable : Int64; Virtual;
  End;

  TFslStreamClass = Class Of TFslStream;

  TFslStreamList = Class(TFslObjectList)
    Private
      Function GetStream(iIndex: Integer): TFslStream;
      Procedure SetStream(iIndex: Integer; Const Value: TFslStream);

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property Streams[iIndex : Integer] : TFslStream Read GetStream Write SetStream; Default;
  End;

  TFslStreamAdapter = Class(TFslStream)
    Private
      FStream : TFslStream;

    Protected
    {$IFOPT C+}
      Function GetStream: TFslStream; Virtual;
    {$ENDIF}
      Procedure SetStream(oStream : TFslStream); Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Read(Var Buffer; iCount : Cardinal); Override;
      Procedure Write(Const Buffer; iCount : Cardinal); Override;

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Function HasStream : Boolean; Virtual;

      Property Stream : TFslStream Read {$IFOPT C+}GetStream{$ELSE}FStream{$ENDIF} Write SetStream;
  End;

  TFslAccessStream = Class(TFslStream)
    Protected
      Function GetPosition : Int64; Virtual;
      Procedure SetPosition(Const Value : Int64); Virtual;

      Function GetSize : Int64; Virtual;
      Procedure SetSize(Const Value : Int64); Virtual;

    Public
      Function Link : TFslAccessStream;

      Property Size : Int64 Read GetSize Write SetSize;
      Property Position : Int64 Read GetPosition Write SetPosition;
  End;

  TFslAccessStreamList = Class(TFslStreamList)
  End;

  TFslAccessStreamClass = Class Of TFslAccessStream;

  TFslAccessStreamAdapter = Class(TFslAccessStream)
    Private
      FStream : TFslAccessStream;

    Protected
      Function GetPosition : Int64; Override;
      Procedure SetPosition(Const Value : Int64); Override;

      Function GetSize : Int64; Override;
      Procedure SetSize(Const Value : Int64); Override;

      Function GetStream: TFslAccessStream; Virtual;
      Procedure SetStream(oStream : TFslAccessStream); Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Read(Var Buffer; iCount : Cardinal); Override;
      Procedure Write(Const Buffer; iCount : Cardinal); Override;

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Property Stream : TFslAccessStream Read GetStream Write SetStream;
  End; 

  EFslStream = Class(EFslException);

  EFslExceptionClass = FHIR.Support.Exceptions.EFslExceptionClass;

  TFslObjectClass = FHIR.Support.Objects.TFslObjectClass;

  TFslStringStream = Class(TFslAccessStream)
    Private
      FData : AnsiString;
      FIndex : Cardinal;

      Procedure SetData(Const Value: AnsiString);
      function GetBytes: TBytes;
      procedure SetBytes(const Value: TBytes);

    Protected
      Function GetPosition : Int64; Override;
      Procedure SetPosition(Const iValue : Int64); Override;

      Function GetSize : Int64; Override;
      Procedure SetSize(Const iValue : Int64); Override;

    Public
      Procedure Read(Var aBuffer; iCount : Cardinal); Override;
      Procedure Write(Const aBuffer; iCount : Cardinal); Override;

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Property Data : AnsiString Read FData Write SetData;
      Property Bytes : TBytes Read GetBytes Write SetBytes;
  End;

  TFslFile = Class(TFslAccessStream)
  Private
    FStream : TFileStream;
    function GetHandle: THandle;
  Protected
    Function GetPosition : Int64; Override;
    Procedure SetPosition(Const Value : Int64); Override;

    Function GetSize : Int64; Override;
    Procedure SetSize(Const iValue : Int64); Override;

    Procedure RaiseError(aException : EFslExceptionClass; Const sMethod, sMessage : String); Overload; Override;

    Function ErrorClass : EFslExceptionClass; Override;

  Public
    constructor Create(const AFileName: string; Mode: Word); overload;
    Destructor Destroy; override;

    function Link : TFslFile; overload;

    Procedure Read(Var aBuffer; iCount : Cardinal); Override;
    Procedure Write(Const aBuffer; iCount : Cardinal); Override;
    Function Readable : Int64; Override;
    Function Writeable : Int64; Override;

    property Handle: THandle read GetHandle;
  End;

  EFslFile = Class(EFslStream);

    {
    A list of bytes
  }

  TFslBuffer = Class(TFslObject)
    Private
      FData : Pointer;
      FCapacity : Integer;
      FOwned : Boolean;
      {$IFNDEF VER130}
      FEncoding: TEncoding;
    FFormat: String;
      function GetAsUnicode: String;
      procedure SetAsUnicode(const Value: String);
      Function ExtractUnicode(Const iLength : Integer) : String;

      {$ENDIF}

      Procedure SetCapacity(Const Value : Integer);
      Procedure SetData(Const Value : Pointer);
      Procedure SetOwned(Const Value : Boolean);

      Function GetAsText: AnsiString;
      Procedure SetAsText(Const Value: AnsiString);

      Function ExtractAscii(Const iLength : Integer) : AnsiString;
      function GetAsBytes: TBytes;
      procedure SetAsBytes(const Value: TBytes);
    function GetHasFormat: boolean;
    Public
      Constructor Create; Override;
{$IFNDEF UT}
      Constructor Create(sText : String); Overload;
{$ENDIF}
      Destructor Destroy; Override;

      Function Link : TFslBuffer;
      Function Clone : TFslBuffer;

      Procedure Assign(oObject : TFslObject); Override;


      {
        Make the buffer empty.

        note that valid buffers must have content
      }
      Procedure Clear;

      {
        Fill the buffer with contents from the named file
      }
      Procedure LoadFromFileName(Const sFilename : String);

      {
        Save the buffer contents to the named file
      }
      Procedure SaveToFileName(Const sFilename : String);

      Function Equal(oBuffer : TFslBuffer) : Boolean;
      Procedure Copy(oBuffer : TFslBuffer);
      Procedure CopyRange(oBuffer : TFslBuffer; Const iIndex, iLength : Integer);
      Function Compare(oBuffer : TFslBuffer) : Integer;

      Procedure Move(Const iSource, iTarget, iLength : Integer);

      Function Offset(iIndex : Integer) : Pointer;
      Function StartsWith(Const sValue : String) : Boolean;

      Procedure LoadFromFile(oFile : TFslFile);
      Procedure SaveToFile(oFile : TFslFile);
      Procedure LoadFromStream(oStream : TFslStream); overload;
      Procedure SaveToStream(oStream : TFslStream); overload;
      Procedure LoadFromStream(oStream : TStream); overload;
      Procedure SaveToStream(oStream : TStream); overload;

      Property Data : Pointer Read FData Write SetData;
      Property Capacity : Integer Read FCapacity Write SetCapacity;
      Property Owned : Boolean Read FOwned Write SetOwned;
      Property AsText : String Read GetAsUnicode Write SetAsUnicode;
      Property Encoding : TEncoding read FEncoding write FEncoding;
      Property AsBytes : TBytes read GetAsBytes write SetAsBytes;
      Property AsAscii : AnsiString Read GetAsText Write SetAsText;
      Property Size : Integer Read FCapacity Write SetCapacity;
      Property Format : String read FFormat write FFormat;
      property HasFormat : boolean read GetHasFormat;

  End;

  TFslBufferClass = Class Of TFslBuffer;


  PByte = ^Byte;

  TFslMemoryStream = Class(TFslAccessStream)
    Private
      FBuffer : TFslBuffer;
      FCurrentPointer : PByte;
      FSize : Int64;
      FPosition : Int64;
      FExpand : Boolean;

      Function GetCapacity: Int64;
      Procedure SetCapacity(Const Value: Int64);

      Function GetDataPointer : Pointer;
      Procedure SetDataPointer(Const Value : Pointer);

      Procedure SetBuffer(Const Value: TFslBuffer);

      Function GetAsText: String;
      Procedure SetAsText(Const Value: String);

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

      Function GetSize : Int64; Override;
      Procedure SetSize(Const Value : Int64); Override;

      Function GetPosition : Int64; Override;
      Procedure SetPosition(Const Value : Int64); Override;

      Function ValidPosition(Const iValue : Int64) : Boolean; 

      Procedure UpdateCurrentPointer;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Clone : TFslMemoryStream;
      Function Link : TFslMemoryStream;

      Procedure Assign(oObject : TFslObject); Override;

      Procedure Read(Var aBuffer; iSize : Cardinal); Override;
      Procedure Write(Const aBuffer; iSize : Cardinal); Override;

      Procedure DeleteRange(Const iFromPosition, iToPosition : Integer);

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Function Assignable : Boolean; Override;

      Function Equal(oMemory : TFslMemoryStream) : Boolean;

      Property Buffer : TFslBuffer Read FBuffer Write SetBuffer;
      Property DataPointer : Pointer Read GetDataPointer Write SetDataPointer;
      Property CurrentPointer : PByte Read FCurrentPointer;
      Property Capacity : Int64 Read GetCapacity Write SetCapacity;
      Property Size; // declared in TFslAccessStream.
      Property Expand : Boolean Read FExpand Write FExpand;
      Property AsText : String Read GetAsText Write SetAsText;
  End;

  EFslMemoryStream = Class(EFslStream);

  TFslVCLStream = Class(TFslStream)
    Private
      FStream : TStream;

      Function GetStream: TStream;
      Procedure SetStream(Const Value: TStream);

    Public
      Procedure Read(Var aBuffer; iCount : Cardinal); Override;
      Procedure Write(Const aBuffer; iCount : Cardinal); Override;

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Property Stream : TStream Read GetStream Write SetStream;
  End;

  TVCLStream = Class(TStream)
    Private
      FStream : TFslStream;

      Function GetStream: TFslStream;
      Procedure SetStream(Const Value: TFslStream);

    Protected
      Procedure SetSize(NewSize: LongInt); Override;

    Public
      Constructor Create; Overload; Virtual;
      Constructor Create(Stream : TFslStream); Overload; Virtual;
      Destructor Destroy; Override;

      Function Read(Var aBuffer; iCount: LongInt): LongInt; Override;
      Function Write(Const aBuffer; iCount: LongInt): LongInt; Override;
      Function Seek(iOffset: LongInt; iOrigin: Word): LongInt; Override;

      Property Stream : TFslStream Read GetStream Write SetStream;
  End;

  // dealing with changes to the Stream Interface:
  {$IFDEF VER260}
  TStreamDWord = longint;
  TStreamFixedUInt = longint;
  PStreamFixedUInt = plongint;
  TStreamLargeUInt = largeint;
  {$ELSE}
  TStreamDWord = DWord; // or longint
  TStreamFixedUInt = FixedUInt;  // or longint
  PStreamFixedUInt = PFixedUInt; // or plongint
  TStreamLargeUInt = LargeUInt; // or largeint
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  TFslStreamAdapterI = Class(TStreamAdapter)
    Public
      Function Stat(Out statstg: TStatStg; grfStatFlag: TStreamDWord): HResult; Override; Stdcall;
  End;

  TFslIStreamAdapter = Class(TFslObject, IStream)
    Private
      FStream : TFslAccessStream;

      Function GetStream: TFslAccessStream;
      Procedure SetStream(Const Value: TFslAccessStream);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      function Seek(dlibMove: Largeint; dwOrigin: TStreamDWord; out libNewPosition: TStreamLargeUInt): HResult; stdcall;
      function SetSize(libNewSize: TStreamLargeUInt): HResult; stdcall;
      function CopyTo(stm: IStream; cb: TStreamLargeUInt; out cbRead: TStreamLargeUInt; out cbWritten: TStreamLargeUInt): HResult; stdcall;
      function Commit(grfCommitFlags: TStreamDWord): HResult; stdcall;
      function Revert: HResult; stdcall;
      function LockRegion(libOffset: TStreamLargeUInt; cb: TStreamLargeUInt; dwLockType: TStreamDWord): HResult; stdcall;
      function UnlockRegion(libOffset: TStreamLargeUInt; cb: TStreamLargeUInt; dwLockType: TStreamDWord): HResult; stdcall;
      function Stat(out statstg: TStatStg; grfStatFlag: TStreamDWord): HResult; stdcall;
      function Clone(out stm: IStream): HResult; stdcall;
      function Read(pv: Pointer; cb: TStreamFixedUInt; pcbRead: PStreamFixedUInt): HResult; stdcall;
      function Write(pv: Pointer; cb: TStreamFixedUInt; pcbWritten: PStreamFixedUInt): HResult; stdcall;

      Property Stream: TFslAccessStream Read GetStream Write SetStream;
  End;
  {$ENDIF}

  TStream = Classes.TStream;


Type
  TFslStreamFilerReferenceHashEntry = Class(TFslHashEntry)
    Private
      FKey : Pointer;
      FValue : Pointer;

      Procedure SetKey(Const Value: Pointer);

    Protected
      Procedure Generate; Override;

    Public
      Procedure Assign(oSource : TFslObject); Override;

      Property Key : Pointer Read FKey Write SetKey;
      Property Value : Pointer Read FValue Write FValue;
  End;

  TFslStreamFilerReferenceHashTable = Class(TFslHashTable)
    Protected
      Function ItemClass : TFslHashEntryClass; Override;

      Function Equal(oA, oB : TFslHashEntry) : Integer; Override;
  End;

  TFslStreamFilerReferenceManager = Class(TFslObject)
    Private
      FHashTable : TFslStreamFilerReferenceHashTable;
      FLookupHashEntry : TFslStreamFilerReferenceHashEntry;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TFslStreamFilerReferenceManager;

      Procedure Clear;

      Procedure Bind(oKey, oValue : TFslObject); 
      Function Get(oKey : TFslObject) : TFslObject; 
      Function Exists(oKey : TFslObject) : Boolean; 

      Property HashTable : TFslStreamFilerReferenceHashTable Read FHashTable;
  End;

  TFslStreamFilerResourceManager = Class(TFslObject)
    Public
      Function Link : TFslStreamFilerResourceManager;

      Procedure Clear; Virtual;

      Function ResolveObject(Const sResource : String; Const aClass : TFslObjectClass) : TFslObject; Virtual;
      Function ResolveID(Const oObject : TFslObject) : String; Virtual;
  End;

  TFslBufferList = Class(TFslObjectList)
    Private
      Function GetBuffer(iIndex: Integer): TFslBuffer;

    Protected
      Function ItemClass: TFslObjectClass; Override;

    Public
      Property Buffers[iIndex : Integer] : TFslBuffer Read GetBuffer; Default;
  End;


  TFslNameBuffer = Class(TFslBuffer)
    Private
      FName : String;

    Public
      Function Link : TFslNameBuffer;
      Function Clone : TFslNameBuffer;

      Procedure Assign(oObject : TFslObject); Override;

      Property Name : String Read FName Write FName;
  End;

  TFslNameBufferList = Class(TFslBufferList)
    Private
      Function GetBuffer(iIndex : Integer) : TFslNameBuffer;

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Procedure DefaultCompare(Out aEvent : TFslItemListCompare); Override;

      Function CompareByName(pA, pB : Pointer) : Integer; Virtual;

    Public
      Function Link : TFslNameBufferList;
      Function Clone : TFslNameBufferList; 

      Function GetByName(Const sName : String) : TFslNameBuffer;
      Function IndexByName(Const sName : String) : Integer;
      Function ExistsByName(Const sName : String) : Boolean;
      Procedure Merge(oBuffers : TFslNameBufferList);

      Property Buffer[iIndex : Integer] : TFslNameBuffer Read GetBuffer; Default;
  End;


Type

  TAfsMode = (amRead, amWrite, amCreate);
  TAfsShare = (asNone, asRead, asWrite);

  // Simple types

  TAfsHandle = NativeUInt;

  // Forward declarations

  TAfsVolume = Class;
  TAfsList = Class;
  TAfsEntity = Class;

  EAfs = Class(EFslException);

  TAfsObject = Class(TFslStringHashEntry)
  Protected
    Procedure RaiseError(Const sMethod, sException : String); Override;
  End; { TAfsObject }

  TAfsClass = Class Of TAfsObject;

  TAfsIterator = Class(TFslIterator)
  Private
    FVolume : TAfsVolume;
    Procedure SetVolume(Const Value: TAfsVolume);
  Protected
    Procedure RaiseError(Const sMethod, sMessage : String); Override;
    Function GetCurrent : TAfsEntity; Virtual; Abstract;
  Public
    Constructor Create(oVolume : TAfsVolume); Overload; Virtual;
    Destructor Destroy; Override;

    Property Current : TAfsEntity Read GetCurrent;
    Property Volume : TAfsVolume Read FVolume Write SetVolume;
  End; { TAfsIterator }

  TAfsIteratorClass = Class Of TAfsIterator;

  TAfsVolume = Class(TAfsObject)
  Private
    FMode : TAfsMode;
  Protected
    Function GetAllocation : Cardinal; Virtual;
    Procedure SetAllocation(Value : Cardinal); Virtual;
  Public
    Function Link : TAfsVolume; Overload;
    Function Clone : TAfsVolume; Overload;

    Procedure Open; Overload; Virtual;
    Procedure Format; Overload; Virtual;
    Procedure Close; Overload; Virtual;

    Procedure Delete; Overload; Virtual;
    Function Exists : Boolean; Overload; Virtual; Abstract;
    Function Active : Boolean; Overload; Virtual; Abstract;

    Function Open(Const libName, sName : String; amMode : TAfsMode; asShare : TAfsShare) : TAfsHandle; Overload; Virtual; Abstract;
    Procedure Read(oHandle : TAfsHandle; Var Buffer; iCount : Cardinal); Virtual; Abstract;
    Procedure Write(oHandle : TAfsHandle; Const Buffer; iCount : Cardinal); Virtual; Abstract;
    Procedure Close(oHandle : TAfsHandle); Overload; Virtual; Abstract;

    Function GetSize(oHandle : TAfsHandle) : Int64; Virtual; Abstract;
    Function GetPosition(oHandle : TAfsHandle) : Int64; Virtual; Abstract;
    Procedure SetPosition(oHandle : TAfsHandle; Const iValue : Int64); Virtual; Abstract;

    Function Exists(Const sName : String) : Boolean; Overload; Virtual; Abstract;
    Procedure Rename(Const sSource, sDestination : String); Overload; Virtual; Abstract;
    Procedure Delete(Const sName : String); Overload; Virtual; Abstract;

    Function OpenIterator : TAfsIterator; Virtual; Abstract;
    Procedure CloseIterator(oIterator : TAfsIterator); Virtual; Abstract;

    Property Allocation : Cardinal Read GetAllocation Write SetAllocation;
    Property Mode : TAfsMode Read FMode Write FMode;
  End; { TAfsVolume }

  TAfsVolumeClass = Class Of TAfsVolume;

  TAfsStream = Class(TFslAccessStream)
  Private
    FVolume : TAfsVolume;
    FHandle : TAfsHandle;
    Procedure SetVolume(Const Value: TAfsVolume);
  Protected
    Function GetPosition : Int64; Override;
    Procedure SetPosition(Const Value : Int64); Override;

    Function GetSize : Int64; Override;
    Procedure SetSize(Const Value : Int64); Override;
  Public
    Constructor Create(oVolume : TAfsVolume; oHandle : TAfsHandle); Overload;
    Destructor Destroy; Override;

    Procedure Read(Var Buffer; iCount : Cardinal); Override;
    Procedure Write(Const Buffer; iCount : Cardinal); Override;

    Function Readable : Int64; Override;

    Property Volume : TAfsVolume Read FVolume Write SetVolume;
    Property Handle : TAfsHandle Read FHandle Write FHandle;
  End; { TAfsStream }

  TAfsEntity = Class(TAfsObject)
  Private
    FVolume : TAfsVolume;
    FStream : TAfsStream;
    FMode   : TAfsMode;
    FShare  : TAfsShare;

    Procedure SetMode(Value : TAfsMode);
    Procedure SetShare(Value : TAfsShare);
    Procedure SetVolume(Const Value: TAfsVolume);

  Public
    Constructor Create; Overload; Override;
    Destructor Destroy; Override;
    Constructor Create(oVolume : TAfsVolume; Const sName : String = ''); Overload;

    Procedure Assign(oSource : TFslObject); Override;

    Procedure Open; Overload; Virtual;
    Procedure Open(amMode : TAfsMode; asShare : TAfsShare = asRead); Overload;
    Procedure Open(Const sName : String; amMode : TAfsMode; asShare : TAfsShare = asRead); Overload;
    Procedure Close;

    Function Valid : Boolean; Overload; Virtual;

    Property Volume : TAfsVolume Read FVolume Write SetVolume;
    Property Stream : TAfsStream Read FStream;
    Property Mode : TAfsMode Read FMode Write SetMode;
    Property Share : TAfsShare Read FShare Write SetShare;
  End; { TAfsEntity }

  TAfsEntityClass = Class Of TAfsEntity;

  TAfsEntities = Class(TFslStringHashTable)
  End; { TAfsEntities }

  TAfsContainer = Class(TAfsEntity)
  Private
    FItems : TAfsEntities;
  Protected
    Property Items : TAfsEntities Read FItems;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
  End; { TAfsContainer }

  TAfsRoot = Class(TAfsContainer)
  Public
    Property Items;
  End; { TAfsRoot }

  TAfsFolder = Class(TAfsContainer)
  Public
    Property Items;
  End; { TAfsFolder }

  TAfsFile = Class(TAfsEntity)
  End; { TAfsFile }

  TAfsList = Class(TFslObjectList)
  Private
    Function GetEntities(Index : Integer) : TAfsEntity;
    Procedure SetEntities(Index : Integer; Const Value : TAfsEntity);
  Protected
    Function CompareName(pA, pB : Pointer) : Integer; Overload; Virtual;

    Procedure DefaultCompare(Out aCompare : TFslItemListCompare); Overload; Override;

    Function ItemClass : TFslClass; Override;
  Public
    Property Entities[Index : Integer] : TAfsEntity Read GetEntities Write SetEntities; Default;
  End; { TAfsList }

  TAfsFiles = Class(TAfsList)
  Private
    Function GetFiles(Index : Integer) : TAfsFile;
    Procedure SetFiles(Index : Integer; Const Value : TAfsFile);
  Public
    Property Files[Index : Integer] : TAfsFile Read GetFiles Write SetFiles; Default;
  End; { TAfsFiles }


Type
  TAfsStreamManager = Class(TFslObject)
  Private
    FVolume  : TAfsVolume;
    FMode    : TAfsMode;
    FStreams : TFslObjectMatch;

    Procedure SetVolume(Const Value: TAfsVolume);

  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Open; Overload; Virtual;
    Procedure Close; Overload; Virtual;

    Function Open(Const sName : String) : TFslStream; Overload; Virtual;
    Procedure Close(oStream : TFslStream); Overload; Virtual;
    Procedure Delete(Const sName : String); Overload; Virtual;
    Procedure Clear; Overload; Virtual;

    Property Volume : TAfsVolume Read FVolume Write SetVolume;
    Property Mode : TAfsMode Read FMode Write FMode;
  End; { TAfsStreamManager }



Type
  TAfsResourceVolume = Class(TAfsVolume)
  Public
    Procedure Open; Overload; Override;
    Procedure Format; Override;
    Procedure Close; Overload; Override;
    Procedure Delete; Overload; Override;

    Function Exists : Boolean; Overload; Override;
    Function Active : Boolean; Overload; Override;

    Function Open(Const libName, sName : String; amMode : TAfsMode; asShare : TAfsShare) : TAfsHandle; Overload; Override;
    Procedure Read(oHandle : TAfsHandle; Var Buffer; iCount : Cardinal); Override;
    Procedure Write(oHandle : TAfsHandle; Const Buffer; iCount : Cardinal); Override;
    Procedure Close(oHandle : TAfsHandle); Overload; Override;

    Function GetSize(oHandle : TAfsHandle) : Int64; Override;
    Function GetPosition(oHandle : TAfsHandle) : Int64; Override;
    Procedure SetPosition(oHandle : TAfsHandle; Const iValue : Int64); Override;

    Function Exists(Const sName : String) : Boolean; Overload; Override;
    Procedure Rename(Const sSource, sDest : String); Override;
    Procedure Delete(Const sName : String); Overload; Override;

    Function OpenIterator : TAfsIterator; Override;
    Procedure CloseIterator(oIterator : TAfsIterator); Override;
  End; { TAfsResourceVolume }

  TAfsResourceManager = Class(TAfsStreamManager)
    Public
      Constructor Create(Const sName : String); Overload; Virtual;
      Constructor Create; Overload; Override;
  End; { TAfsResourceManager }


Implementation

uses
  FHIR.Support.Binary, FHIR.Support.Math, FHIR.Support.Strings;

Function TFslStream.Link : TFslStream;
Begin
  Result := TFslStream(Inherited Link);
End;


Function TFslStream.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslStream;
End;


Procedure TFslStream.Read(Var Buffer; iCount: Cardinal);
Begin
End;


Procedure TFslStream.Write(Const Buffer; iCount: Cardinal);
Begin
End;


Function TFslStream.Readable : Int64;
Begin
  Result := 0;
End;


Function TFslStream.Writeable : Int64;
Begin
  Result := 0;
End;


Function TFslStream.Assignable: Boolean;
Begin
  Result := False;
End;


Function TFslStreamList.ItemClass: TFslObjectClass;
Begin
  Result := TFslStream;
End;


Function TFslStreamList.GetStream(iIndex: Integer): TFslStream;
Begin
  Result := TFslStream(ObjectByIndex[iIndex]);
End;


Procedure TFslStreamList.SetStream(iIndex: Integer; Const Value: TFslStream);
Begin
  ObjectByIndex[iIndex] := Value;
End;


Constructor TFslStreamAdapter.Create;
Begin
  Inherited;

  FStream := Nil;
End;


Destructor TFslStreamAdapter.Destroy;
Begin
  FStream.Free;
  FStream := Nil;

  Inherited;
End;

{$IFOPT C+}
Function TFslStreamAdapter.GetStream: TFslStream;
Begin
  Assert(Invariants('GetStream', FStream, TFslStream, 'FStream'));

  Result := FStream;
End;
{$ENDIF}

Procedure TFslStreamAdapter.SetStream(oStream : TFslStream);
Begin
  Assert(Not Assigned(oStream) Or Invariants('SetStream', oStream, TFslStream, 'oStream'));

  FStream.Free;
  FStream := oStream;
End;


Function TFslStreamAdapter.HasStream: Boolean;
Begin
  Result := Assigned(FStream);
End;


Procedure TFslStreamAdapter.Read(Var Buffer; iCount: Cardinal);
Begin 
  Stream.Read(Buffer, iCount);
End;


Procedure TFslStreamAdapter.Write(Const Buffer; iCount: Cardinal);
Begin
  Stream.Write(Buffer, iCount);
End;


Function TFslStreamAdapter.Readable : Int64;
Begin
  Result := Stream.Readable;
End;  


Function TFslStreamAdapter.Writeable : Int64;
Begin 
  Result := Stream.Writeable;
End;  


Function TFslAccessStream.Link : TFslAccessStream;
Begin 
  Result := TFslAccessStream(Inherited Link);
End;


Function TFslAccessStream.GetPosition : Int64;
Begin 
  Result := 0;
End;  


Function TFslAccessStream.GetSize : Int64;
Begin 
  Result := 0;
End;  


Procedure TFslAccessStream.SetPosition(Const Value: Int64);
Begin 
End;  


Procedure TFslAccessStream.SetSize(Const Value: Int64);
Begin 
End;


Function TFslAccessStreamAdapter.GetPosition : Int64;
Begin
  Result := Stream.Position;
End;


Function TFslAccessStreamAdapter.GetSize : Int64;
Begin 
  Result := Stream.Size;
End;  


Procedure TFslAccessStreamAdapter.SetPosition(Const Value: Int64);
Begin
  Stream.Position := Value;
End;  


Procedure TFslAccessStreamAdapter.SetSize(Const Value: Int64);
Begin 
  Stream.Size := Value;
End;  


Procedure TFslAccessStreamAdapter.Read(Var Buffer; iCount: Cardinal);
Begin 
  Stream.Read(Buffer, iCount);
End;  


Procedure TFslAccessStreamAdapter.Write(Const Buffer; iCount: Cardinal);
Begin
  Stream.Write(Buffer, iCount);
End;  


Function TFslAccessStreamAdapter.Writeable : Int64;
Begin
  Result := Stream.Writeable;
End;


Function TFslAccessStreamAdapter.Readable : Int64;
Begin
  Result := Stream.Readable;
End;


Function TFslAccessStreamAdapter.GetStream: TFslAccessStream;
Begin
  Assert(Invariants('GetStream', FStream, TFslAccessStream, 'FStream'));

  Result := FStream;
End;


Procedure TFslAccessStreamAdapter.SetStream(oStream: TFslAccessStream);
Begin
  Assert(Not Assigned(oStream) Or Invariants('SetStream', oStream, TFslAccessStream, 'oStream'));

  FStream.Free;
  FStream := oStream;
End;


Constructor TFslAccessStreamAdapter.Create;
Begin
  Inherited;

  FStream := Nil;
End;


Destructor TFslAccessStreamAdapter.Destroy;
Begin
  FStream.Free;
  FStream := Nil;

  Inherited;
End;


Procedure TFslStringStream.Read(Var aBuffer; iCount: Cardinal);
Begin
  If FIndex + iCount > Size Then
    RaiseError('Read', 'Unable to read past end of string.');

  Move((PAnsiChar(FData) + FIndex)^, aBuffer, iCount);
  Inc(FIndex, iCount);
End;


Procedure TFslStringStream.Write(Const aBuffer; iCount: Cardinal);
Begin
  If FIndex + iCount > Size Then
    Size := FIndex + iCount;

  Move(aBuffer, (PAnsiChar(FData) + FIndex)^, iCount);
  Inc(FIndex, iCount);
End;


Function TFslStringStream.Writeable : Int64;
Begin
  Result := High(Result);
End;


Function TFslStringStream.Readable : Int64;
Begin
  Result := Size - Position;
End;


function TFslStringStream.GetBytes: TBytes;
begin
  result := AnsiStringAsBytes(FData);
end;

Function TFslStringStream.GetPosition : Int64;
Begin
  Result := FIndex;
End;


Procedure TFslStringStream.SetPosition(Const iValue: Int64);
Begin
  FIndex := iValue;
End;


Function TFslStringStream.GetSize : Int64;
Begin
  Result := Length(FData);
End;


Procedure TFslStringStream.SetSize(Const iValue: Int64);
Begin
  SetLength(FData, iValue);
  If FIndex > Cardinal(Length(FData)) Then
    FIndex := Length(FData);
End;


procedure TFslStringStream.SetBytes(const Value: TBytes);
begin
  FData := BytesAsAnsiString(value);
end;

Procedure TFslStringStream.SetData(Const Value: AnsiString);
Begin
  FData := Value;
  If FIndex > Cardinal(Length(FData)) Then
    FIndex := Length(FData)
  Else
    FIndex := 0;
End;



Procedure TFslVCLStream.Read(Var aBuffer; iCount: Cardinal);
Begin
  Stream.Read(aBuffer, iCount);
End;


Procedure TFslVCLStream.Write(Const aBuffer; iCount: Cardinal);
Begin
  Stream.Write(aBuffer, iCount);
End;


Function TFslVCLStream.Readable : Int64;
Begin
  Result := Stream.Size - Stream.Position;
End;


Function TFslVCLStream.Writeable : Int64;
Begin
  Result := Stream.Size - Stream.Position;
End;


Function TFslVCLStream.GetStream: TStream;
Begin
  Assert(CheckCondition(Assigned(FStream), 'GetStream', 'No VCL Stream available.'));

  Result := FStream;
End;


Procedure TFslVCLStream.SetStream(Const Value: TStream);
Begin
  FStream := Value;
End;


Constructor TVCLStream.Create;
Begin
  Inherited;

  FStream := Nil;
End;


constructor TVCLStream.Create(Stream: TFslStream);
begin
  Create;
  FStream := Stream;
end;

Destructor TVCLStream.Destroy;
Begin
  FStream.Free;

  Inherited;
End;


Function TVCLStream.Read(Var aBuffer; iCount: Integer): LongInt;
Var
  iReadable : Integer;
Begin
  iReadable := Stream.Readable;
  If iReadable > iCount Then
    iReadable := iCount;

  Stream.Read(aBuffer, iReadable);
  Result := iReadable;
End;


Function TVCLStream.Seek(iOffset: Integer; iOrigin: Word): LongInt;
Var
  oAccess : TFslAccessStream;
Begin
  If Not (Stream Is TFslAccessStream) Then
    Raise EFslStream.Create(Self, 'Seek', 'Unable to seek in a non-access stream'); // Error is not available.

  oAccess := TFslAccessStream(Stream);

  Case iOrigin Of
    soFromBeginning : Result := iOffset;
    soFromCurrent   : Result := oAccess.Position + iOffset;
    soFromEnd       : Result := oAccess.Size - iOffset;
  Else
    Result := iOffset;
  End;

  oAccess.Position := Result;
End;


Procedure TVCLStream.SetSize(NewSize: Integer);
Var
  oAccess : TFslAccessStream;
Begin
  If Not (Stream Is TFslAccessStream) Then
    Raise EFslStream.Create(Self, 'SetSize', 'Unable to set the size of a non-access stream'); // Error is not available.

  oAccess := TFslAccessStream(Stream);

  oAccess.Size := NewSize;
End;


Function TVCLStream.GetStream: TFslStream;
Begin
  Result := FStream;
End;


Procedure TVCLStream.SetStream(Const Value: TFslStream);
Begin
  FStream.Free;
  FStream := Value;
End;


Function TVCLStream.Write(Const aBuffer; iCount: Integer): LongInt;
Begin
  FStream.Write(aBuffer, iCount);
  Result := iCount;
End;

{$IFDEF MSWINDOWS}
Function TFslStreamAdapterI.Stat(Out statstg: TStatStg; grfStatFlag: TStreamDWord): HResult;
Begin
  // TStreamAdapter.stat does not clear the STATSTG structure.
  // http://qc.embarcadero.com/wc/qcmain.aspx?d=45528

  FillChar(statstg, SizeOf(statstg), 0);
  Result := Inherited Stat(statstg, grfStatFlag);
End;


Constructor TFslIStreamAdapter.Create;
Begin
  Inherited;

  FStream := TFslAccessStream.Create;
End;

Destructor TFslIStreamAdapter.Destroy;
Begin
  FStream.Free;

  Inherited;
End;


Function TFslIStreamAdapter.Read(pv: Pointer; cb: TStreamFixedUInt; pcbRead: PStreamFixedUInt): HResult;

Var
  iReadable : TStreamFixedUInt;
Begin
  Try
    If pv = Nil Then
    Begin
      Result := STG_E_INVALIDPOINTER;
      Exit;
    End;

    iReadable := Stream.Readable;
    If iReadable > cb Then
      iReadable := cb;

    FStream.Read(pv^, iReadable);

    If pcbRead <> Nil Then
      pcbRead^ := iReadable;

    Result := S_OK;
  Except
    Result := S_FALSE;
  End;
End;

Function TFslIStreamAdapter.Write(pv: Pointer; cb: TStreamFixedUInt; pcbWritten: PStreamFixedUInt): HResult;
Begin
  Try
    If pv = Nil Then
    Begin
      Result := STG_E_INVALIDPOINTER;
      Exit;
    End;

    FStream.Write(pv^, cb);

    If pcbWritten <> Nil Then
      pcbWritten^ := cb;

    Result := S_OK;
  Except
    Result := STG_E_CANTSAVE;
  End;
End;


Function TFslIStreamAdapter.Seek(dlibMove: Largeint; dwOrigin: TStreamDWORD; out libNewPosition: TStreamLargeUInt): HResult;
Var
  iNewPos: Integer;
Begin
  Try
    If (dwOrigin > STREAM_SEEK_END) Then
    Begin
      Result := STG_E_INVALIDFUNCTION;
      Exit;
    End;

    Case dwOrigin Of
      STREAM_SEEK_SET : iNewPos := dlibMove;
      STREAM_SEEK_CUR : iNewPos := Stream.Position + dlibMove;
      STREAM_SEEK_END : iNewPos := Stream.Size - dlibMove;
    Else
      iNewPos := dlibMove;
    End;

    Stream.Position := iNewPos;

    If @libNewPosition <> Nil Then
      libNewPosition := iNewPos;

    Result := S_OK;
  Except
    Result := STG_E_INVALIDPOINTER;
  End;
End;


Function TFslIStreamAdapter.Revert: HResult;
Begin
  Result := STG_E_REVERTED;
End;


Function TFslIStreamAdapter.SetSize(libNewSize: TStreamLargeUInt): HResult;
Begin
  Try
    Stream.Size := LongInt(libNewSize);
    
    If libNewSize <> Stream.Size Then
      Result := E_FAIL
    Else
      Result := S_OK;
  Except
    Result := E_UNEXPECTED;
  End;
End;


Function TFslIStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: TStreamDWORD): HResult;
Begin
  Result := S_OK;
  Try
    If (@statstg <> Nil) Then
    Begin
      FillChar(statstg, SizeOf(statstg), 0);

      statstg.dwType := STGTY_STREAM;
      statstg.cbSize := FStream.Size;
      statstg.grfLocksSupported := LOCK_WRITE;
    End;
  Except
    Result := E_UNEXPECTED;
  End;
End;


Function TFslIStreamAdapter.UnlockRegion(libOffset: TStreamLargeUInt; cb: TStreamLargeUInt; dwLockType: TStreamDWORD): HResult;
Begin
  Result := STG_E_INVALIDFUNCTION;
End;


Function TFslIStreamAdapter.Clone(Out stm: IStream): HResult;
Begin
  Result := E_NOTIMPL;
End;


Function TFslIStreamAdapter.Commit(grfCommitFlags: TStreamDWORD): HResult;
Begin
  Result := S_OK;
End;


Function TFslIStreamAdapter.CopyTo(stm: IStream; cb: TStreamLargeUInt; out cbRead: TStreamLargeUInt; out cbWritten: TStreamLargeUInt): HResult;
Const
  MaxBufSize = 1024 * 1024;  // 1mb
Var
  Buffer: Pointer;
  BufSize, N, I: Integer;
  BytesRead, BytesWritten, W: LargeInt;
  iNumRead : Integer;
Begin
  Result := S_OK;
  BytesRead := 0;
  BytesWritten := 0;
  Try
    If cb > MaxBufSize Then
      BufSize := MaxBufSize
    Else
      BufSize := Integer(cb);

    GetMem(Buffer, BufSize);
    Try
      While cb > 0 Do
      Begin
        If cb > MaxInt Then
          I := MaxInt
        Else
          I := cb;

        While I > 0 Do
        Begin
          If I > BufSize Then
            N := BufSize
          Else
            N := I;

          Read(Buffer, N, @iNumRead);
          Inc(BytesRead, iNumRead);
          //Inc(BytesRead, FStream.Read(Buffer^, N));

          W := 0;
          Result := stm.Write(Buffer, N, @W);
          
          Inc(BytesWritten, W);

          If (Result = S_OK) And (Integer(W) <> N) Then
            Result := E_FAIL;

          If Result <> S_OK Then
            Exit;
            
          Dec(I, N);
        End;
        
        Dec(cb, I);
      End;
    Finally
      FreeMem(Buffer);
      If (@cbWritten <> Nil) Then
        cbWritten := BytesWritten;

      If (@cbRead <> Nil) Then
        cbRead := BytesRead;
    End;
  Except
    Result := E_UNEXPECTED;
  End;
End;


Function TFslIStreamAdapter.LockRegion(libOffset: TStreamLargeUInt; cb: TStreamLargeUInt; dwLockType: TStreamDWORD): HResult;
Begin
  Result := STG_E_INVALIDFUNCTION;
End;


Procedure TFslIStreamAdapter.SetStream(Const Value: TFslAccessStream);
Begin
  FStream.Free;
  FStream := Value;
End;


Function TFslIStreamAdapter.GetStream: TFslAccessStream;
Begin
  Result := FStream;
End;

{$ENDIF}



Procedure TFslStreamFilerReferenceHashEntry.Assign(oSource: TFslObject);
Begin
  Inherited;

  Key := TFslStreamFilerReferenceHashEntry(oSource).Key;
  Value := TFslStreamFilerReferenceHashEntry(oSource).Value;
End;


Function TFslStreamFilerReferenceHashTable.Equal(oA, oB: TFslHashEntry): Integer;
Begin
  Result := Inherited Equal(oA, oB);

  If Result = 0 Then
    Result := IntegerCompare(Integer(TFslStreamFilerReferenceHashEntry(oA).Key), Integer(TFslStreamFilerReferenceHashEntry(oB).Key));
End;


Function TFslStreamFilerReferenceHashTable.ItemClass: TFslHashEntryClass;
Begin
  Result := TFslStreamFilerReferenceHashEntry;
End;


Procedure TFslStreamFilerReferenceHashEntry.Generate;
Begin
  Inherited;

  Code := HashIntegerToCode32(Integer(FKey));
End;


Procedure TFslStreamFilerReferenceHashEntry.SetKey(Const Value: Pointer);
Begin
  If FKey <> Value Then
  Begin
    FKey := Value;
    Generate;
  End;
End;


Constructor TFslStreamFilerReferenceManager.Create;
Begin
  Inherited;

  FHashTable := TFslStreamFilerReferenceHashTable.Create;
  FHashTable.Capacity := 47;

  FLookupHashEntry := TFslStreamFilerReferenceHashEntry.Create;
End;


Destructor TFslStreamFilerReferenceManager.Destroy;
Begin
  FHashTable.Free;
  FLookupHashEntry.Free;

  Inherited;
End;


Function TFslStreamFilerReferenceManager.Link: TFslStreamFilerReferenceManager;
Begin
  Result := TFslStreamFilerReferenceManager(Inherited Link);
End;


Procedure TFslStreamFilerReferenceManager.Clear;
Begin
  FHashTable.Clear;
End;


Procedure TFslStreamFilerReferenceManager.Bind(oKey, oValue: TFslObject);
Var
  oHashEntry : TFslStreamFilerReferenceHashEntry;
Begin
  If Assigned(oKey) Then
  Begin
    oHashEntry := TFslStreamFilerReferenceHashEntry.Create;
    oHashEntry.Key := Pointer(oKey);
    oHashEntry.Value := Pointer(oValue);
    FHashTable.Add(oHashEntry);
  End;
End;


Function TFslStreamFilerReferenceManager.Get(oKey : TFslObject): TFslObject;
Var
  oHashEntry : TFslStreamFilerReferenceHashEntry;
Begin
  FLookupHashEntry.Key := Pointer(oKey);

  oHashEntry := TFslStreamFilerReferenceHashEntry(FHashTable.Get(FLookupHashEntry));

  If Assigned(oHashEntry) Then
    Result := TFslObject(oHashEntry.Value)
  Else
    Result := Nil;
End;


Function TFslStreamFilerReferenceManager.Exists(oKey: TFslObject): Boolean;
Begin
  FLookupHashEntry.Key := Pointer(oKey);

  Result := FHashTable.Exists(FLookupHashEntry);
End;


Procedure TFslStreamFilerResourceManager.Clear;
Begin
End;


Function TFslStreamFilerResourceManager.ResolveObject(Const sResource: String; Const aClass : TFslObjectClass): TFslObject;
Begin
  RaiseError('ResolveObject', 'ResolveObject must be overriden.');

  Result := Nil;
End;  


Function TFslStreamFilerResourceManager.ResolveID(Const oObject: TFslObject): String;
Begin 
  RaiseError('ResolveID', 'ResolveObject must be overriden.');

  Result := '';
End;  


Function TFslStreamFilerResourceManager.Link : TFslStreamFilerResourceManager;
Begin
  Result := TFslStreamFilerResourceManager(Inherited Link);
End;

Constructor TFslBuffer.Create;
Begin 
  Inherited;
  {$IFNDEF VER130}
  FEncoding := TEncoding.UTF8;
  {$ENDIF}
  FOwned := True;
End;  


Destructor TFslBuffer.Destroy;
Begin 
  If FOwned Then
    MemoryDestroy(FData, FCapacity);

  Inherited;
End;  


Function TFslBuffer.Clone : TFslBuffer;
Begin 
  Result := TFslBuffer(Inherited Clone);
End;  


Function TFslBuffer.Link : TFslBuffer;
Begin 
  Result := TFslBuffer(Inherited Link);
End;  


Procedure TFslBuffer.Assign(oObject : TFslObject);
Begin 
  Inherited;

  Copy(TFslBuffer(oObject));
End;




Procedure TFslBuffer.LoadFromStream(oStream: TFslStream);
Begin
  Assert(Invariants('LoadFromStream', oStream, TFslStream, 'oStream'));

  oStream.Read(Data^, Capacity);
End;


Procedure TFslBuffer.LoadFromStream(oStream: TStream);
Begin
//  Assert(Invariants('LoadFromStream', oStream, TStream, 'oStream'));

  Capacity := oStream.Size - oStream.Position;
  oStream.Read(Data^, Capacity);
End;


Procedure TFslBuffer.SaveToStream(oStream: TFslStream);
Begin
  Assert(Invariants('SaveToStream', oStream, TFslStream, 'oStream'));

  oStream.Write(Data^, Capacity);
End;


Procedure TFslBuffer.SaveToStream(oStream: TStream);
var
  i : integer;
Begin
 // Assert(Invariants('SaveToStream', oStream, TStream, 'oStream'));

   i := oStream.Position;
   oStream.Write(Data^, Capacity);
   assert(oStream.Position = i + Capacity);
End;


Procedure TFslBuffer.LoadFromFile(oFile: TFslFile);
Begin
  Assert(Invariants('LoadFromFile', oFile, TFslFile, 'oFile'));

  Capacity := oFile.Size;

  LoadFromStream(oFile);
End;


Procedure TFslBuffer.SaveToFile(oFile: TFslFile);
Begin
  Assert(Invariants('SaveToFile', oFile, TFslFile, 'oFile'));

  SaveToStream(oFile);
End;


Procedure TFslBuffer.LoadFromFileName(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmOpenRead);
  Try
    LoadFromFile(oFile);
  Finally
    oFile.Free;
  End;
End;


Procedure TFslBuffer.SaveToFileName(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmCreate);
  Try
    SaveToFile(oFile);
  Finally
    oFile.Free;
  End;
End;


Procedure TFslBuffer.Clear;
Begin 

  Capacity := 0;
End;  


Function TFslBuffer.Equal(oBuffer: TFslBuffer): Boolean;
Begin
  Assert(Invariants('Equal', oBuffer, TFslBuffer, 'oBuffer'));

  Result := Compare(oBuffer) = 0;
End;


Function TFslBuffer.Compare(oBuffer: TFslBuffer): Integer;
Begin
  Assert(Invariants('Compare', oBuffer, TFslBuffer, 'oBuffer'));

  Result := IntegerCompare(Capacity, oBuffer.Capacity);

  If Result = 0 Then
    Result := MemoryCompare(Data, oBuffer.Data, Capacity);
End;  


Procedure TFslBuffer.SetCapacity(Const Value: Integer);
Begin 
  If (Value <> Capacity) Then
  Begin 
    Assert(CheckCondition(Value >= 0, 'SetCapacity', StringFormat('Unable to change the Capacity to %d', [Value])));

    If FOwned Then
      MemoryResize(FData, FCapacity, Value);

    FCapacity := Value;
  End;  
End;  


Procedure TFslBuffer.SetData(Const Value: Pointer);
Begin 

  If FData <> Value Then
  Begin 
    SetCapacity(0);

    FData := Value;
    FOwned := False;
  End;  
End;  


Procedure TFslBuffer.SetOwned(Const Value: Boolean);
Begin 

  FOwned := Value;
End;  


Function TFslBuffer.GetAsText : AnsiString;
Begin
  Result := ExtractAscii(Capacity);
End;


{$IFNDEF VER130}

function TFslBuffer.GetAsUnicode: String;
var
  chars: SysUtils.TCharArray;
begin
  chars := FEncoding.GetChars(AsBytes);
  SetString(Result, PChar(chars), Length(chars));
end;

function TFslBuffer.GetHasFormat: boolean;
begin
  result := FFormat <> '';
end;

procedure TFslBuffer.SetAsUnicode(const Value: String);
begin
  AsBytes := FEncoding.GetBytes(Value);
end;
{$ENDIF}

Procedure TFslBuffer.SetAsText(Const Value: AnsiString);
Begin

  Capacity := Length(Value);
  MemoryMove(Pointer(Value), Data, Capacity);
End;


Procedure TFslBuffer.Copy(oBuffer: TFslBuffer);
Begin
  CopyRange(oBuffer, 0, oBuffer.Capacity);
End;


Procedure TFslBuffer.CopyRange(oBuffer: TFslBuffer; Const iIndex, iLength : Integer);
Begin
  Assert(Invariants('CopyRange', oBuffer, TFslBuffer, 'oBuffer'));
  Assert(CheckCondition((iIndex >= 0) And (iIndex + iLength <= oBuffer.Capacity), 'CopyRange', 'Attempted to copy invalid part of the buffer.'));

  SetCapacity(iLength);

  MemoryMove(oBuffer.Offset(iIndex), Data, iLength);
End;  


Procedure TFslBuffer.Move(Const iSource, iTarget, iLength : Integer);
Begin
  Assert(CheckCondition((iSource >= 0) And (iSource + iLength <= Capacity), 'Copy', 'Attempted to move from an invalid part of the buffer.'));
  Assert(CheckCondition((iTarget >= 0) And (iTarget + iLength <= Capacity), 'Copy', 'Attempted to move to an invalid part of the buffer.'));

  MemoryMove(Offset(iSource), Offset(iTarget), iLength);
End;  


Function TFslBuffer.Offset(iIndex: Integer): Pointer;
Begin 
  Assert(CheckCondition((iIndex >= 0) And (iIndex <= Capacity), 'Offset', 'Attempted to access invalid offset in the buffer.'));

  Result := Pointer(NativeUInt(Data) + NativeUInt(iIndex));
End;  


Function TFslBufferList.GetBuffer(iIndex: Integer): TFslBuffer;
Begin 
  Result := TFslBuffer(ObjectByIndex[iIndex]);
End;  


Function TFslBufferList.ItemClass : TFslObjectClass;
Begin
  Result := TFslBuffer;
End;


Function TFslBuffer.ExtractAscii(Const iLength: Integer): AnsiString;
Begin
  Result := MemoryToString(Data, iLength);
End;

{$IFNDEF VER130}
Function TFslBuffer.ExtractUnicode(Const iLength: Integer): String;
Begin
  result := System.copy(GetAsUnicode, 1, iLength);
End;
{$ENDIF}


Function TFslBuffer.StartsWith(Const sValue: String): Boolean;
Begin
  {$IFDEF VER130}
  Result := (Length(sValue) <= Capacity) And StringStartsWith(ExtractAscii(Length(sValue)), sValue);
  {$ELSE}
  Result := (Length(sValue) <= Capacity) And StringStartsWith(ExtractUnicode(Length(sValue)), sValue);
  {$ENDIF}
End;


{$IFNDEF UT}
constructor TFslBuffer.Create(sText: String);
begin
  Create;
  AsText := stext;
end;
{$ENDIF}

function TFslBuffer.GetAsBytes: TBytes;
begin
  SetLength(result, Capacity);
  if Capacity > 0 then
    System.move(FData^, result[0], capacity);
end;

procedure TFslBuffer.SetAsBytes(const Value: TBytes);
begin
  SetCapacity(length(Value));
  if capacity > 0 then
    System.move(Value[0], FData^, Capacity);
end;

Procedure TFslNameBuffer.Assign(oObject : TFslObject);
Begin
  Inherited;

  FName := TFslNameBuffer(oObject).FName;
End;


Function TFslNameBuffer.Link : TFslNameBuffer;
Begin
  Result := TFslNameBuffer(Inherited Link);
End;


Function TFslNameBuffer.Clone : TFslNameBuffer;
Begin
  Result := TFslNameBuffer(Inherited Clone);
End;


Function TFslNameBufferList.Clone : TFslNameBufferList;
Begin
  Result := TFslNameBufferList(Inherited Clone);
End;


Function TFslNameBufferList.Link : TFslNameBufferList;
Begin
  Result := TFslNameBufferList(Inherited Link);
End;


Function TFslNameBufferList.ItemClass : TFslObjectClass;
Begin
  Result := TFslNameBuffer;
End;


Procedure TFslNameBufferList.Merge(oBuffers : TFslNameBufferList);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oBuffers.Count - 1 Do
    Add(oBuffers[iLoop].Clone);
End;


Function TFslNameBufferList.GetBuffer(iIndex : Integer) : TFslNameBuffer;
Begin
  Result := TFslNameBuffer(ObjectByIndex[iIndex]);
End;


Function TFslNameBufferList.GetByName(Const sName : String) : TFslNameBuffer;
Begin
  Result := TFslNameBuffer(Get(IndexByName(sName)));
End;


Function TFslNameBufferList.ExistsByName(Const sName : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(sName));
End;


Function TFslNameBufferList.CompareByName(pA, pB: Pointer): Integer;
Begin
  Result := StringCompare(TFslNameBuffer(pA).Name, TFslNameBuffer(pB).Name);
End;


Function TFslNameBufferList.IndexByName(Const sName: String): Integer;
Var
  oBuffer : TFslNameBuffer;
Begin
  oBuffer := TFslNameBuffer(ItemNew);
  Try
    oBuffer.Name := sName;

    If Not Find(oBuffer, Result, CompareByName) Then
      Result := -1;
  Finally
    oBuffer.Free;
  End;
End;


Procedure TFslNameBufferList.DefaultCompare(Out aEvent: TFslItemListCompare);
Begin
  aEvent := CompareByName;
End;


Constructor TFslMemoryStream.Create;
Begin
  Inherited;

  FBuffer := TFslBuffer.Create;
  FExpand := True;
End;


Destructor TFslMemoryStream.Destroy;
Begin
  FBuffer.Free;

  Inherited;
End;


Function TFslMemoryStream.Clone : TFslMemoryStream;
Begin
  Result := TFslMemoryStream(Inherited Clone);
End;  


Function TFslMemoryStream.Link : TFslMemoryStream;
Begin 
  Result := TFslMemoryStream(Inherited Link);
End;  


Procedure TFslMemoryStream.Assign(oObject: TFslObject);
Begin 
  Inherited;

  FBuffer.Assign(TFslMemoryStream(oObject).Buffer);
  FSize := TFslMemoryStream(oObject).Size;
  FPosition := TFslMemoryStream(oObject).Position;

  UpdateCurrentPointer;
End;  

Procedure TFslMemoryStream.UpdateCurrentPointer;
Begin 
  FCurrentPointer := Pointer(NativeUInt(FBuffer.Data) + FPosition);
End;  


Procedure TFslMemoryStream.Read(Var aBuffer; iSize : Cardinal);
Begin
  If iSize > 0 Then
  Begin 
    Assert(CheckCondition(FPosition + iSize <= FBuffer.Capacity, 'Read', 'Unable to read past the end of the buffer.'));
    Assert(CheckCondition(FPosition + iSize <= FSize, 'Read', 'Unable to read past the end of the stream.'));
    Assert(CheckCondition(Assigned(FCurrentPointer), 'Read', 'Current must be assigned.'));

    Move(FCurrentPointer^, aBuffer, iSize);

    Inc(FPosition, iSize);
    Inc(FCurrentPointer, iSize);
  End;  
End;  


Procedure TFslMemoryStream.Write(Const aBuffer; iSize : Cardinal);
Begin
  If iSize > 0 Then
  Begin
    If FExpand And (FPosition + iSize > FBuffer.Capacity) Then
      SetCapacity(FPosition + iSize);

    Assert(CheckCondition(FPosition + iSize <= FBuffer.Capacity, 'Write', 'Unable to write past the end of the buffer.'));
    Assert(CheckCondition(Assigned(FCurrentPointer), 'Read', 'Current must be assigned.'));

    Move(aBuffer, FCurrentPointer^, iSize);

    Inc(FPosition, iSize);
    Inc(FCurrentPointer, iSize);

    If FPosition > FSize Then
      FSize := FPosition;
  End;  
End;  


Function TFslMemoryStream.Readable : Int64;
Begin 
  Result := FSize - FPosition;
End;  


Function TFslMemoryStream.Writeable : Int64;
Begin 
  If FExpand Then
    Result := MaxInt
  Else
    Result := FBuffer.Capacity - FPosition;
End;  


Function TFslMemoryStream.GetSize : Int64;
Begin 
  Result := FSize;
End;  


Procedure TFslMemoryStream.SetSize(Const Value: Int64);
Begin 
  Assert(CheckCondition(Value >= 0, 'SetSize', 'Attempted to set size to an invalid value.'));

  If FSize <> Value Then
  Begin
    If Value > FBuffer.Capacity Then
      SetCapacity(Value);

    FSize := Value;

    If FPosition > FSize Then
      FPosition := FSize;

    UpdateCurrentPointer;
  End;
End;


Function TFslMemoryStream.GetPosition : Int64;
Begin 
  Result := FPosition;
End;  


Procedure TFslMemoryStream.SetPosition(Const Value: Int64);
Begin
  Assert(CheckCondition((Value >= 0) And (Value <= FBuffer.Capacity), 'SetPosition', 'Attempted to set position outside of memory.'));

  FPosition := Value;
  UpdateCurrentPointer;
End;  


Function TFslMemoryStream.GetCapacity : Int64;
Begin 
  Result := FBuffer.Capacity;
End;  


Procedure TFslMemoryStream.SetCapacity(Const Value: Int64);
Begin 
  Assert(CheckCondition((Value >= Size), 'SetCapacity', StringFormat('Unable to change the capacity to less than the size %d.', [Value])));

  FBuffer.Capacity := Value;
  UpdateCurrentPointer;
End;


Function TFslMemoryStream.GetDataPointer : Pointer;
Begin 
  Result := FBuffer.Data;
End;  


Procedure TFslMemoryStream.SetDataPointer(Const Value: Pointer);
Begin 
  FBuffer.Data := Value;
  UpdateCurrentPointer;
End;  


Procedure TFslMemoryStream.SetBuffer(Const Value: TFslBuffer);
Begin 
  Assert(Not Assigned(Value) Or Invariants('SetBuffer', Value, TFslBuffer, 'Value'));

  FBuffer.Free;
  FBuffer := Value;

  If Assigned(FBuffer) Then
  Begin
    FSize := FBuffer.Capacity;
    FPosition := 0;

    UpdateCurrentPointer;
  End
  Else
  Begin
    FSize := 0;
    FPosition := 0;

    FCurrentPointer := Nil;
  End;
End;


Function TFslMemoryStream.GetAsText : String;
Begin
{$IFDEF VER130}
  Result := MemoryToString(DataPointer, Size);
{$ELSE}
  Result := SysUtils.TEncoding.ASCII.GetString(TBytes(DataPointer), 0, Size);
{$ENDIF}
End;


Procedure TFslMemoryStream.SetAsText(Const Value: String);
{$IFNDEF VER130}
Var
  aBytes : TBytes;
{$ENDIF}
Begin
  Size := Length(Value);

{$IFDEF VER130}
  MemoryMove(Pointer(Value), DataPointer, Size);
{$ELSE}
  aBytes := SysUtils.TEncoding.ASCII.GetBytes(Value);
  MemoryMove(@aBytes[0], DataPointer, Size);
{$ENDIF}

  If Position > Size Then
    Position := Size;
End;  


Function TFslMemoryStream.ErrorClass : EFslExceptionClass;
Begin 
  Result := EFslMemoryStream;
End;  


Function TFslMemoryStream.Equal(oMemory: TFslMemoryStream): Boolean;
Begin 
  Result := (Size = oMemory.Size) And (MemoryCompare(Buffer.Data, oMemory.Buffer.Data, Size) = 0);
End;  


Procedure TFslMemoryStream.DeleteRange(Const iFromPosition, iToPosition: Integer);
Var
  iDifference : Integer;
Begin 
  Assert(CheckCondition(ValidPosition(iFromPosition), 'DeleteRange', 'Delete from position is invalid.'));
  Assert(CheckCondition(ValidPosition(iToPosition), 'DeleteRange', 'Delete to position is invalid.'));

  iDifference := iToPosition - iFromPosition + 1;

  Assert(CheckCondition(iDifference > 0, 'DeleteRange', 'Delete from position <= to position.'));

  Buffer.Move(iToPosition, iFromPosition, iDifference);

  Size := Size - iDifference;
End;  


Function TFslMemoryStream.ValidPosition(Const iValue: Int64): Boolean;
Begin
  Result := (iValue >= 0) And (iValue <= Size);
End;


Function TFslMemoryStream.Assignable: Boolean;
Begin
  Result := True;
End;


{ TFslFile }

constructor TFslFile.Create(const AFileName: string; Mode: Word);
begin
  inherited create;
  FStream := TFileStream.Create(AFileName, mode);
end;

destructor TFslFile.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TFslFile.ErrorClass: EFslExceptionClass;
begin
  Result := EFslFile;
end;

function TFslFile.GetHandle: THandle;
begin
  result := FStream.Handle;
end;

function TFslFile.GetPosition: Int64;
begin
  result := FStream.Position;
end;

function TFslFile.GetSize: Int64;
begin
  result := FStream.Size;
end;

function TFslFile.Link: TFslFile;
begin
  result := TFslFile(Inherited Link);
end;

procedure TFslFile.RaiseError(aException: EFslExceptionClass; const sMethod, sMessage: String);
begin
  Inherited RaiseError(aException, sMethod, StringFormat('%s: ''%s''', [sMessage, FStream.FileName]));
end;

procedure TFslFile.Read(var aBuffer; iCount: Cardinal);
begin
  if FStream.Read(aBuffer, iCount) < iCount then
    RaiseError('Read', 'Unable to read past end of file');
end;

function TFslFile.Readable: Int64;
begin
  result := FStream.Size - FStream.Position;
end;

procedure TFslFile.SetPosition(const Value: Int64);
begin
  FStream.Position := value;
end;

procedure TFslFile.SetSize(const iValue: Int64);
begin
  FStream.Size := iValue;
end;

procedure TFslFile.Write(const aBuffer; iCount: Cardinal);
begin
  If (FStream.Write(aBuffer, iCount) < iCount) Then
    RaiseError('Read', 'Unable to write the entire buffer');
end;

function TFslFile.Writeable: Int64;
begin
  result := 0; // ?
end;


Procedure TAfsObject.RaiseError(Const sMethod, sException : String);

Begin { Procedure TAfsObject.Error }
  RaiseError(EAfs, sMethod, sException);
End;  { Procedure TAfsObject.Error }



Procedure TAfsIterator.RaiseError(Const sMethod, sMessage : String);

Begin { Procedure TAfsIterator.Error }
  RaiseError(EAfs, sMethod, sMessage);
End;  { Procedure TAfsIterator.Error }

Constructor TAfsIterator.Create(oVolume : TAfsVolume);

Begin { Constructor TAfsIterator.Create }
  Create;
  FVolume := oVolume;
End;  { Constructor TAfsIterator.Create }



Function TAfsVolume.GetAllocation : Cardinal;

Begin { Function TAfsVolume.GetAllocation }
  Result := 0;
End;  { Function TAfsVolume.GetAllocation }

Procedure TAfsVolume.SetAllocation(Value : Cardinal);

Begin { Procedure TAfsVolume.SetAllocation }
  RaiseError('SetAllocation', 'Cannot set allocation unit size.');
End;  { Procedure TAfsVolume.SetAllocation }

Function TAfsVolume.Clone: TAfsVolume;
Begin
  Result := TAfsVolume(Inherited Clone);
End;

Function TAfsVolume.Link: TAfsVolume;
Begin
  Result := TAfsVolume(Inherited Link);
End;

Procedure TAfsVolume.Open;

Begin { Procedure TAfsVolume.Open }
End;  { Procedure TAfsVolume.Open }

Procedure TAfsVolume.Format;

Begin { Procedure TAfsVolume.Format }
End;  { Procedure TAfsVolume.Format }

Procedure TAfsVolume.Close;

Begin { Procedure TAfsVolume.Close }
End;  { Procedure TAfsVolume.Close }

Procedure TAfsVolume.Delete;

Begin { Procedure TAfsVolume.Delete }
End;  { Procedure TAfsVolume.Delete }



Function TAfsStream.GetPosition : Int64;

Begin { Function TAfsStream.GetPosition }
  Result := FVolume.GetPosition(FHandle);
End;  { Function TAfsStream.GetPosition }

Procedure TAfsStream.SetPosition(Const Value : Int64);

Begin { Procedure TAfsStream.SetPosition }
  FVolume.SetPosition(FHandle, Value);
End;  { Procedure TAfsStream.SetPosition }

Function TAfsStream.GetSize : Int64;

Begin { Function TAfsStream.GetSize }
  Result := FVolume.GetSize(FHandle);
End;  { Function TAfsStream.GetSize }

Procedure TAfsStream.SetSize(Const Value : Int64);

Begin { Procedure TAfsStream.SetSize }
  RaiseError('SetSize', 'Not implemented');
//  FVolume.SetSize(FHandle, Value);
End;  { Procedure TAfsStream.SetSize }

Constructor TAfsStream.Create(oVolume : TAfsVolume; oHandle : TAfsHandle);

Begin { Constructor TAfsStream.Create }
  Create;
  FVolume := oVolume;
  FHandle := oHandle;
End;  { Constructor TAfsStream.Create }

Procedure TAfsStream.Read(Var Buffer; iCount : Cardinal);

Begin { Procedure TAfsStream.Read }
  FVolume.Read(FHandle, Buffer, iCount);
End;  { Procedure TAfsStream.Read }

Procedure TAfsStream.Write(Const Buffer; iCount : Cardinal);

Begin { Procedure TAfsStream.Write }
  FVolume.Write(FHandle, Buffer, iCount);
End;  { Procedure TAfsStream.Write }

Function TAfsStream.Readable : Int64;

Begin { Function TAfsStream.Readable }
  Result := Size - Position;
End;  { Function TAfsStream.Readable }



Procedure TAfsEntity.SetMode(Value : TAfsMode);

Begin { Procedure TAfsEntity.SetMode }
  FMode := Value;
End;  { Procedure TAfsEntity.SetMode }

Procedure TAfsEntity.SetShare(Value : TAfsShare);

Begin { Procedure TAfsEntity.SetShare }
  FShare := Value;
End;  { Procedure TAfsEntity.SetShare }

Constructor TAfsEntity.Create;

Begin { Constructor TAfsEntity.Create }
  Inherited Create;

  FStream := TAfsStream.Create;

  FShare := asRead;
End;  { Constructor TAfsEntity.Create }

Destructor TAfsEntity.Destroy;

Begin { Destructor TAfsEntity.Destroy }
  FStream.Free;
  FVolume.Free;

  Inherited;
End;  { Destructor TAfsEntity.Destroy }

Constructor TAfsEntity.Create(oVolume : TAfsVolume; Const sName : String);

Begin { Constructor TAfsEntity.Create }
  Create;

  FVolume := oVolume;
  Name := sName;
End;  { Constructor TAfsEntity.Create }

Procedure TAfsEntity.Assign(oSource : TFslObject);

Begin { Procedure TAfsEntity.Assign }
  Inherited;

  FVolume := TAfsEntity(oSource).Volume;
  FMode := TAfsEntity(oSource).Mode;
  FShare := TAfsEntity(oSource).Share;
End;  { Procedure TAfsEntity.Assign }

Procedure TAfsEntity.Open;

Begin { Procedure TAfsEntity.Open }
  If Not Assigned(FVolume) Then
    RaiseError('Open', 'AFS volume not assigned.')
  Else
  Begin { If }
    FStream.Volume := FVolume.Link;
    FStream.Handle := FVolume.Open('', Name, FMode, FShare);

    If FStream.Handle = 0 Then
      RaiseError('Open', StringFormat('Unable to open ''%s'' in volume ''%s''', [Name, FVolume.Name]));
  End   { If }
End;  { Procedure TAfsEntity.Open }

Procedure TAfsEntity.Open(amMode : TAfsMode; asShare : TAfsShare = asRead);
Begin { Procedure TAfsEntity.Open }
  FMode := amMode;
  FShare := asShare;
  Open;
End;  { Procedure TAfsEntity.Open }


Procedure TAfsEntity.Open(Const sName : String; amMode : TAfsMode; asShare : TAfsShare);
Begin { Procedure TAfsEntity.Open }
  Name := sName;
  Open(amMode, asShare);
End;  { Procedure TAfsEntity.Open }


Procedure TAfsEntity.Close;
Begin { Procedure TAfsEntity.Close }
  If FStream.Handle <> 0 Then
  Begin { If }
    FVolume.Close(FStream.Handle);
    FStream.Handle := 0;
    FStream.Volume := Nil;
  End;  { If }
End;  { Procedure TAfsEntity.Close }


Function TAfsEntity.Valid : Boolean;
Begin { Function TAfsEntity.Valid }
  Result := Assigned(FVolume) And Assigned(FStream);
End;  { Function TAfsEntity.Valid }


Procedure TAfsEntity.SetVolume(Const Value: TAfsVolume);
Begin { Procedure TAfsEntity.SetVolume }
  FVolume.Free;
  FVolume := Value;
End;  { Procedure TAfsEntity.SetVolume }


Constructor TAfsContainer.Create;
Begin { Constructor TAfsContainer.Create }
  Inherited Create;

  FItems := TAfsEntities.Create;
  FItems.Capacity := 37; // Arbitrary low prime
End;  { Constructor TAfsContainer.Create }


Destructor TAfsContainer.Destroy;
Begin { Destructor TAfsContainer.Destroy }
  FItems.Free;

  Inherited;
End;  { Destructor TAfsContainer.Destroy }


Function TAfsList.GetEntities(Index : Integer) : TAfsEntity;
Begin { Function TAfsList.GetEntities }
  Result := TAfsEntity(ItemByIndex[Index]);
End;  { Function TAfsList.GetEntities }


Procedure TAfsList.SetEntities(Index : Integer; Const Value : TAfsEntity);
Begin { Procedure TAfsList.SetEntities }
  ItemByIndex[Index] := Value;
End;  { Procedure TAfsList.SetEntities }


Function TAfsList.CompareName(pA, pB : Pointer) : Integer;
Begin { Function TAfsList.DefaultCompare }
  Result := IntegerCompare(Integer(TAfsEntity(pA).ClassType), Integer(TAfsEntity(pB).ClassType));

  If Result = 0 Then
    Result := StringCompare(TAfsEntity(pA).Name, TAfsEntity(pB).Name);
End;  { Function TAfsList.DefaultCompare }


Procedure TAfsList.DefaultCompare(Out aCompare: TFslItemListCompare);
Begin { Procedure TAfsList.DefaultCompare }
  aCompare := CompareName;
End;  { Procedure TAfsList.DefaultCompare }


Function TAfsList.ItemClass : TFslClass;
Begin { Function TAfsList.ItemClass }
  Result := TAfsEntity;
End;  { Function TAfsList.ItemClass }


Function TAfsFiles.GetFiles(Index : Integer) : TAfsFile;
Begin { Function TAfsFiles.GetFiles }
  Result := TAfsFile(Entities[Index]);
End;  { Function TAfsFiles.GetFiles }


Procedure TAfsFiles.SetFiles(Index : Integer; Const Value : TAfsFile);
Begin { Procedure TAfsFiles.SetFiles }
  Entities[Index] := Value;
End;  { Procedure TAfsFiles.SetFiles }


Destructor TAfsIterator.Destroy;
Begin { Destructor TAfsIterator.Destroy }
  FVolume.Free;

  Inherited;
End;  { Destructor TAfsIterator.Destroy }


Procedure TAfsIterator.SetVolume(Const Value: TAfsVolume);
Begin { Procedure TAfsIterator.SetVolume }
  FVolume.Free;
  FVolume := Value;
End;  { Procedure TAfsIterator.SetVolume }


Destructor TAfsStream.Destroy;
Begin { Destructor TAfsStream.Destroy }
  FVolume.Free;

  Inherited;
End;  { Destructor TAfsStream.Destroy }


Procedure TAfsStream.SetVolume(Const Value: TAfsVolume);
Begin { Procedure TAfsStream.SetVolume }
  FVolume.Free;
  FVolume := Value;
End;  { Procedure TAfsStream.SetVolume }


Constructor TAfsStreamManager.Create;
Begin { Constructor TAfsStreamManager.Create }
  Inherited;

  FStreams := TFslObjectMatch.Create;
End;  { Constructor TAfsStreamManager.Create }


Destructor TAfsStreamManager.Destroy;
Begin { Destructor TAfsStreamManager.Destroy }
  FStreams.Free;
  FVolume.Free;

  Inherited;
End;  { Destructor TAfsStreamManager.Destroy }


Procedure TAfsStreamManager.Open;
Begin { Procedure TAfsStreamManager.Open }
  FVolume.Open;
End;  { Procedure TAfsStreamManager.Open }


Procedure TAfsStreamManager.Close;
Begin { Procedure TAfsStreamManager.Close }
  FVolume.Close;
End;  { Procedure TAfsStreamManager.Close }


Function TAfsStreamManager.Open(Const sName : String) : TFslStream;
Var
  oFile : TAfsFile;
Begin { Function TAfsStreamManager.Open }
  oFile := TAfsFile.Create(FVolume.Link);
  Try
    // TODO: Add sharing flag correctly
    oFile.Open(sName, FMode, asRead);

    Result := oFile.Stream;

    FStreams.Add(Result.Link, oFile.Link);
  Finally
    oFile.Free;
  End; { Try }
End;  { Function TAfsStreamManager.Open }


Procedure TAfsStreamManager.Close(oStream : TFslStream);
Var
  iIndex : Integer;
  oFile : TAfsFile;
Begin { Procedure TAfsStreamManager.Close }
  iIndex := FStreams.IndexByKey(oStream);

  If FStreams.ExistsByIndex(iIndex) Then
  Begin { If }
    oFile := TAfsFile(FStreams.Values[iIndex]);
    Try
      oFile.Close;
    Finally
      FStreams.DeleteByIndex(iIndex);
    End;  { Finally }
  End;  { If }
End;  { Procedure TAfsStreamManager.Close }


Procedure TAfsStreamManager.Delete(Const sName : String);
Begin { Procedure TAfsStreamManager.Delete }
  FVolume.Delete(sName);
End;  { Procedure TAfsStreamManager.Delete }


Procedure TAfsStreamManager.Clear;
Begin { Procedure TAfsStreamManager.Clear }
  FVolume.Format;
End;  { Procedure TAfsStreamManager.Clear }


Procedure TAfsStreamManager.SetVolume(Const Value: TAfsVolume);
Begin { Procedure TAfsStreamManager.SetVolume }
  FVolume.Free;
  FVolume := Value;
End;  { Procedure TAfsStreamManager.SetVolume }


Type
  TAfsResourceFile = Class(TFslMemoryStream)
  Private
    FMode : TAfsMode;
    FName : String;
    FType : PChar;
    FDirty : Boolean;
  Protected
    Procedure RaiseError(Const sMethod, sException : String); Override;
  Public
    Procedure Write(Const Buffer; iSize : Cardinal); Override;

    Procedure SetName(Const sName : String);

    Property Mode : TAfsMode Read FMode Write FMode;
    Property Name : String Read FName;
    Property ResourceType : PChar Read FType;
    Property Dirty : Boolean Read FDirty Write FDirty;
  End; { TAfsResourceFile }

  TAfsResourceIterator = Class(TAfsIterator)
  Private
    FModule : THandle;
    FCurrent : TAfsFile;
    FItems : TFslStringList;
    FIndex : Integer;
  Protected
    Function GetCurrent : TAfsEntity; Override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure First; Override;
    Procedure Next; Override;
    Function More : Boolean; Override;

    Property Module : THandle Read FModule;
    Property Items : TFslStringList Read FItems;
  End; { TAfsResourceIterator }


Const
  CAPACITY_INCREASE = 12; // 1 Shl 12 = 4096
  CAPACITY_INITIAL = 1 Shl CAPACITY_INCREASE;


Procedure ExpandName(Const sName : String; Var sResource : String; Var iType : PChar);
Var
  sTemp : String;
  iLoop : Integer;
Begin { Procedure ExpandName }
  iLoop := 1;
  If sName <> '' Then
  Begin { If }
    While (iLoop < Length(sName)) And (sName[iLoop] <> ',') Do
      Inc(iLoop);

    If sName[iLoop] = ',' Then
    Begin { If }
      sResource := Copy(sName, 1, iLoop - 1);

      sTemp := Copy(sName, iLoop + 1, Length(sName));
      If sTemp[1] = '#' Then
        iType := PChar(StringToInteger32(Copy(sTemp, 2, Length(sTemp))));
    End;  { If }
  End;  { If }
End;  { Procedure ExpandName }


Function EnumNames(hModule : Cardinal; sType, sName : PChar; lParam : Cardinal) : Boolean; Stdcall;
Begin { Function EnumNames }
  If Hi(Cardinal(sType)) = 0 Then // IsIntResource(lpszType)
    TAfsResourceIterator(lParam).Items.Add(StringFormat('%s,#%d', [sName, Integer(sType)]))
  Else
    TAfsResourceIterator(lParam).Items.Add(StringFormat('%s,%s', [sName, sType]));

  Result := True;
End;  { Function EnumNames }


Function EnumTypes(hModule : Cardinal; lpszType : PChar; lParam : Cardinal) : Boolean; Stdcall;
Var
  sName : String;
  iType : PChar;
Begin { Function EnumTypes }
  If Hi(Cardinal(lpszType)) = 0 Then // IsIntResource(lpszType)
    iType := lpszType
  Else
    ExpandName(',' + lpszType, sName, iType);

  EnumResourceNames(hModule, iType, @EnumNames, lParam);

  Result := True;
End;  { Function EnumTypes }


Procedure TAfsResourceVolume.Open;
Begin { Procedure TAfsResourceVolume.Open }
  Inherited;
End;  { Procedure TAfsResourceVolume.Open }


Procedure TAfsResourceVolume.Close;
Begin { Procedure TAfsResourceVolume.Close }
  Inherited;
End;  { Procedure TAfsResourceVolume.Close }


Procedure TAfsResourceVolume.Format;
Var
  hUpdate : Cardinal;
Begin { Procedure TAfsResourceVolume.Format }
  hUpdate := BeginUpdateResource(PChar(Name), True);
  If hUpdate <> 0 Then
    EndUpdateResource(hUpdate, False)
  Else
    RaiseError('Format', 'Could not open update handle.');
End;  { Procedure TAfsResourceVolume.Format }


Procedure TAfsResourceVolume.Delete;
Begin { Procedure TAfsResourceVolume.Delete }
  FileDelete(Name);
End;  { Procedure TAfsResourceVolume.Delete }


Function TAfsResourceVolume.Exists : Boolean;
Begin { Function TAfsResourceVolume.Exists }
  Result := FileExists(Name);
End;  { Function TAfsResourceVolume.Exists }


Function TAfsResourceVolume.Active : Boolean;
Begin { Function TAfsResourceVolume.Active }
  Result := Exists;
End;  { Function TAfsResourceVolume.Active }


Function TAfsResourceVolume.Open(Const libName, sName : String; amMode : TAfsMode; asShare : TAfsShare) : TAfsHandle;
Var
  oFile : TAfsResourceFile;
  hModule, hResource, iResource, hGlobal : Cardinal;
  pResource : Pointer;
Begin { Function TAfsResourceVolume.Open }
  If amMode > Mode Then
    RaiseError('Open', FHIR.Support.Strings.StringFormat('Requested access mode denied on "%s"', [sName]));

  iResource := 0;
  pResource := Nil;

  oFile := TAfsResourceFile.Create;
  Try
    oFile.Mode := amMode;
    oFile.SetName(sName);

    Case amMode Of
      amRead, amWrite:
      Begin { amRead }
        hModule := LoadLibrary(PChar(libName));
        Try
          hResource := FindResource(hModule, PChar(oFile.Name), oFile.ResourceType);
          If hResource <> 0 Then
          Begin { If }
            iResource := SizeofResource(hModule, hResource);
            If iResource <> 0 Then
            Begin { If }
              hGlobal := LoadResource(hModule, hResource);
              If hGlobal <> 0 Then
              Begin { If }
                pResource := LockResource(hGlobal);
              End;  { If }
            End;  { If }
          End   { If }
          Else
            RaiseError('Open', FHIR.Support.Strings.StringFormat('File not found "%s"', [sName]));

          oFile.Capacity := iResource;
          Move(pResource^, oFile.DataPointer^, iResource);
          oFile.Size := iResource;
        Finally
          FreeLibrary(hModule);
        End; { Try }
      End;  { amRead }

      amCreate:
      Begin { amCreate }
        oFile.Capacity := CAPACITY_INITIAL;
      End;  { amCreate }
    End;  { Case }

    Result := TAfsHandle(oFile);
  Except
    // Prevent object leaking if initialisation code raises an exception
    oFile.Free;
    Result := 0;
  End; { Try }
End;  { Function TAfsResourceVolume.Open }


Procedure TAfsResourceVolume.Read(oHandle : TAfsHandle; Var Buffer; iCount : Cardinal);
Begin { Procedure TAfsResourceVolume.Read }
  TAfsResourceFile(oHandle).Read(Buffer, iCount);
End;  { Procedure TAfsResourceVolume.Read }


Procedure TAfsResourceVolume.Write(oHandle : TAfsHandle; Const Buffer; iCount : Cardinal);
Begin { Procedure TAfsResourceVolume.Write }
  TAfsResourceFile(oHandle).Write(Buffer, iCount);
End;  { Procedure TAfsResourceVolume.Write }


Procedure TAfsResourceVolume.Close(oHandle : TAfsHandle);

Var
  oFile : TAfsResourceFile;
  hUpdate: Cardinal;

Begin { Procedure TAfsResourceVolume.Close }
  oFile := TAfsResourceFile(oHandle);
  Try
    If oFile.Dirty Then
    Begin { If }
      hUpdate := BeginUpdateResource(PChar(Name), False);
      If hUpdate <> 0 Then
      Begin { If }
        Try
          If Not UpdateResource(hUpdate, oFile.ResourceType, PChar(oFile.Name), 0 {iLanguage}, oFile.DataPointer, oFile.Size) Then
            RaiseError('Close', ErrorAsString);
          EndUpdateResource(hUpdate, False);
        Except
          EndUpdateResource(hUpdate, True);
        End; { Try }
      End   { If }
      Else
        RaiseError('Close', 'Could not open update handle.');
    End;  { If }
  Finally
    oFile.Free;
  End; { Try }
End;  { Procedure TAfsResourceVolume.Close }


Function TAfsResourceVolume.GetSize(oHandle : TAfsHandle) : Int64;
Begin { Function TAfsResourceVolume.GetSize }
  Result := TAfsResourceFile(oHandle).Size;
End;  { Function TAfsResourceVolume.GetSize }


Function TAfsResourceVolume.GetPosition(oHandle : TAfsHandle) : Int64;
Begin { Function TAfsResourceVolume.GetPosition }
  Result := TAfsResourceFile(oHandle).Position;
End;  { Function TAfsResourceVolume.GetPosition }


Procedure TAfsResourceVolume.SetPosition(oHandle : TAfsHandle; Const iValue : Int64);
Begin { Procedure TAfsResourceVolume.SetPosition }
  TAfsResourceFile(oHandle).Position := iValue;
End;  { Procedure TAfsResourceVolume.SetPosition }


Function TAfsResourceVolume.Exists(Const sName : String) : Boolean;
Var
  hModule : Cardinal;
  sResource : String;
  iType : PChar;
Begin { Function TAfsResourceVolume.Exists }
  ExpandName(sName, sResource, iType);
  hModule := LoadLibrary(PChar(Name));
  Try
    Result := FindResource(hModule, PChar(sResource), iType) <> 0;
  Finally
    FreeLibrary(hModule);
  End; { Try }
End;  { Function TAfsResourceVolume.Exists }


Procedure TAfsResourceVolume.Rename(Const sSource, sDest : String);
Var
  hSource, hDest : TAfsHandle;
Begin { Procedure TAfsResourceVolume.Rename }
  If Exists(sDest) Then
    RaiseError('Rename', FHIR.Support.Strings.StringFormat('Cannot rename to "%s" - target filename already exists', [sDest]))
  Else
  Begin { If }
    hSource := Open('', sSource, amRead, asWrite);
    hDest := Open('', sDest, amCreate, asNone);
    Try
      // Have to call the Write method to update the dirty flag - still,
      // we bypass a buffer copy doing it this way.
      Write(hDest, TAfsResourceFile(hSource).DataPointer^, TAfsResourceFile(hSource).Size);
    Finally
      Close(hSource);
      Close(hDest);
    End; { Try }
  End;  { If }
End;  { Procedure TAfsResourceVolume.Rename }


Procedure TAfsResourceVolume.Delete(Const sName : String);
Var
  hUpdate : Integer;
  sResource : String;
  iType : PChar;
Begin { Procedure TAfsResourceVolume.Delete }
  ExpandName(sName, sResource, iType);
  hUpdate := BeginUpdateResource(PChar(Name), False);
  If hUpdate <> 0 Then
  Begin { If }
    Try
      If Not UpdateResource(hUpdate, iType, PChar(sResource), 0 {iLanguage}, Nil, 0) Then
        RaiseError('Delete', ErrorAsString);
      EndUpdateResource(hUpdate, False);
    Except
      EndUpdateResource(hUpdate, True);
    End; { Try }
  End   { If }
  Else
    RaiseError('Close', 'Could not open update handle.');
End;  { Procedure TAfsResourceVolume.Delete }


Function TAfsResourceVolume.OpenIterator : TAfsIterator;
Begin { Function TAfsResourceVolume.OpenIterator }
  Result := TAfsResourceIterator.Create(Self.Link);
End;  { Function TAfsResourceVolume.OpenIterator }


Procedure TAfsResourceVolume.CloseIterator(oIterator : TAfsIterator);
Begin { Procedure TAfsResourceVolume.CloseIterator }
  oIterator.Free;
End;  { Procedure TAfsResourceVolume.CloseIterator }


Procedure TAfsResourceFile.RaiseError(Const sMethod, sException : String);
Begin { Procedure TAfsResourceFile.Error }
  RaiseError(EAfs, sMethod, sException);
End;  { Procedure TAfsResourceFile.Error }


Procedure TAfsResourceFile.Write(Const Buffer; iSize : Cardinal);
Begin { Procedure TAfsResourceFile.Write }
  If Mode = amRead Then
    RaiseError('Write', 'File was opened read-only.');

  // Calculate required capacity increase - round up to next 4Kb
  If Writeable < iSize Then
    Capacity := (((Capacity + iSize) Shr CAPACITY_INCREASE) + 1) Shl CAPACITY_INCREASE;

  Inherited;
  FDirty := True;
End;  { Procedure TAfsResourceFile.Write }


Procedure TAfsResourceFile.SetName(Const sName : String);
Begin { Procedure TAfsResourceFile.SetName }
  ExpandName(sName, FName, FType);

  Assert(CheckCondition((FName <> ''), 'SetName', 'Resource name must be of form <Name>,#<Type>'));
End;  { Procedure TAfsResourceFile.SetName }


Function TAfsResourceIterator.GetCurrent : TAfsEntity;
Begin { Function TAfsResourceIterator.GetCurrent }
  If FItems.ExistsByIndex(FIndex) Then
  Begin { If }
    FCurrent.Name := FItems[FIndex];
    Result := FCurrent;
  End   { If }
  Else
    Result := Nil;
End;  { Function TAfsResourceIterator.GetCurrent }


Constructor TAfsResourceIterator.Create;
Begin { Constructor TAfsResourceIterator.Create }
  Inherited Create;

  FCurrent := TAfsFile.Create;
  FItems := TFslStringList.Create;
End;  { Constructor TAfsResourceIterator.Create }


Destructor TAfsResourceIterator.Destroy;
Begin { Destructor TAfsResourceIterator.Destroy }
  FItems.Free;
  FCurrent.Free;
  Inherited;
End;  { Destructor TAfsResourceIterator.Destroy }


Procedure TAfsResourceIterator.First;
Begin { Procedure TAfsResourceIterator.First }
  FItems.Clear;
  FCurrent.Volume := Volume.Link;
  FModule := LoadLibrary(PChar(Volume.Name));
  Try
    EnumResourceTypes(FModule, @EnumTypes, Integer(Self));
    FIndex := 0;
  Finally
    FreeLibrary(FModule);
    FModule := 0;
  End; { Try }
End;  { Procedure TAfsResourceIterator.First }


Procedure TAfsResourceIterator.Next;
Begin { Procedure TAfsResourceIterator.Next }
  Inc(FIndex);
End;  { Procedure TAfsResourceIterator.Next }


Function TAfsResourceIterator.More : Boolean;
Begin { Function TAfsResourceIterator.More }
  Result := FItems.ExistsByIndex(FIndex);
End;  { Function TAfsResourceIterator.More }


Constructor TAfsResourceManager.Create;
Begin { Constructor TAfsResourceManager.Create }
  Inherited;

  Volume := TAfsResourceVolume.Create;
  Volume.Mode := amRead;

  Mode := amRead;
End;  { Constructor TAfsResourceManager.Create }


Constructor TAfsResourceManager.Create(Const sName: String);
Begin { Constructor TAfsResourceManager.Create }
  Create;

  Volume.Name := sName;
End;  { Constructor TAfsResourceManager.Create }


End.

