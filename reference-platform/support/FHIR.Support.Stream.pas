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
  FHIR.Support.Objects, FHIR.Support.Collections, FHIR.Support.Exceptions, FHIR.Support.Filers, FHIR.Support.System;



type

  TAdvStream = Class(TAdvObject)
    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

    Public
      Function Link : TAdvStream;

      Function Assignable : Boolean; Override;

      Procedure Read(Var Buffer; iCount : Cardinal); Virtual; // can't mark as overload
      Procedure Write(Const Buffer; iCount : Cardinal); Virtual; // can't mark as overload

      Function Readable : Int64; Virtual;
      Function Writeable : Int64; Virtual;
  End;

  TAdvStreamClass = Class Of TAdvStream;

  TAdvStreamList = Class(TAdvObjectList)
    Private
      Function GetStream(iIndex: Integer): TAdvStream;
      Procedure SetStream(iIndex: Integer; Const Value: TAdvStream);

    Protected
      Function ItemClass : TAdvObjectClass; Override;

    Public
      Property Streams[iIndex : Integer] : TAdvStream Read GetStream Write SetStream; Default;
  End;

  TAdvStreamAdapter = Class(TAdvStream)
    Private
      FStream : TAdvStream;

    Protected
    {$IFOPT C+}
      Function GetStream: TAdvStream; Virtual;
    {$ENDIF}
      Procedure SetStream(oStream : TAdvStream); Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Read(Var Buffer; iCount : Cardinal); Override;
      Procedure Write(Const Buffer; iCount : Cardinal); Override;

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Function HasStream : Boolean; Virtual;

      Property Stream : TAdvStream Read {$IFOPT C+}GetStream{$ELSE}FStream{$ENDIF} Write SetStream;
  End;

  TAdvAccessStream = Class(TAdvStream)
    Protected
      Function GetPosition : Int64; Virtual;
      Procedure SetPosition(Const Value : Int64); Virtual;

      Function GetSize : Int64; Virtual;
      Procedure SetSize(Const Value : Int64); Virtual;

    Public
      Function Link : TAdvAccessStream;

      Property Size : Int64 Read GetSize Write SetSize;
      Property Position : Int64 Read GetPosition Write SetPosition;
  End;

  TAdvAccessStreamList = Class(TAdvStreamList)
  End;

  TAdvAccessStreamClass = Class Of TAdvAccessStream;

  TAdvAccessStreamAdapter = Class(TAdvAccessStream)
    Private
      FStream : TAdvAccessStream;

    Protected
      Function GetPosition : Int64; Override;
      Procedure SetPosition(Const Value : Int64); Override;

      Function GetSize : Int64; Override;
      Procedure SetSize(Const Value : Int64); Override;

      Function GetStream: TAdvAccessStream; Virtual;
      Procedure SetStream(oStream : TAdvAccessStream); Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Read(Var Buffer; iCount : Cardinal); Override;
      Procedure Write(Const Buffer; iCount : Cardinal); Override;

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Property Stream : TAdvAccessStream Read GetStream Write SetStream;
  End; 

  EAdvStream = Class(EAdvException);

  EAdvExceptionClass = FHIR.Support.Exceptions.EAdvExceptionClass;

  TAdvObjectClass = FHIR.Support.Objects.TAdvObjectClass;

  TAdvStringStream = Class(TAdvAccessStream)
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

  TAdvFile = Class(TAdvAccessStream)
  Private
    FStream : TFileStream;
    function GetHandle: THandle;
  Protected
    Function GetPosition : Int64; Override;
    Procedure SetPosition(Const Value : Int64); Override;

    Function GetSize : Int64; Override;
    Procedure SetSize(Const iValue : Int64); Override;

    Procedure RaiseError(aException : EAdvExceptionClass; Const sMethod, sMessage : String); Overload; Override;

    Function ErrorClass : EAdvExceptionClass; Override;

  Public
    constructor Create(const AFileName: string; Mode: Word); overload;
    Destructor Destroy; override;

    function Link : TAdvFile; overload;

    Procedure Read(Var aBuffer; iCount : Cardinal); Override;
    Procedure Write(Const aBuffer; iCount : Cardinal); Override;
    Function Readable : Int64; Override;
    Function Writeable : Int64; Override;

    property Handle: THandle read GetHandle;
  End;

  EAdvFile = Class(EAdvStream);

    {@class TAdvBuffer
    A list of bytes
  }
  {!.Net HL7Connect.Util.Buffer}

  TAdvBuffer = Class(TAdvPersistent)
    Private
      FData : Pointer;
      FCapacity : Integer;
      FOwned : Boolean;
      {$IFNDEF VER130}
      FEncoding: TEncoding;
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
    Public
      {!script hide}
      Constructor Create; Override;
{$IFNDEF UT}
      Constructor Create(sText : String); Overload;
{$ENDIF}
      Destructor Destroy; Override;

      Function Link : TAdvBuffer;
      Function Clone : TAdvBuffer;

      Procedure Define(oFiler : TAdvFiler); Override;
      Procedure Load(oFiler : TAdvFiler); Override;
      Procedure Save(oFiler : TAdvFiler); Override;
      Procedure Assign(oObject : TAdvObject); Override;

      {!script show}

      {@member Clear
        Make the buffer empty.

        note that valid buffers must have content
      }
      Procedure Clear;

      {@member LoadFromFileName
        Fill the buffer with contents from the named file
      }
      Procedure LoadFromFileName(Const sFilename : String);

      {@member SaveToFileName
        Save the buffer contents to the named file
      }
      Procedure SaveToFileName(Const sFilename : String);

      {!script hide}
      Function Equal(oBuffer : TAdvBuffer) : Boolean;
      Procedure Copy(oBuffer : TAdvBuffer);
      Procedure CopyRange(oBuffer : TAdvBuffer; Const iIndex, iLength : Integer);
      Function Compare(oBuffer : TAdvBuffer) : Integer;

      Procedure Move(Const iSource, iTarget, iLength : Integer);

      Function Offset(iIndex : Integer) : Pointer;
      Function StartsWith(Const sValue : String) : Boolean;

      Procedure LoadFromFile(oFile : TAdvFile);
      Procedure SaveToFile(oFile : TAdvFile);
      Procedure LoadFromStream(oStream : TAdvStream); overload;
      Procedure SaveToStream(oStream : TAdvStream); overload;
      Procedure LoadFromStream(oStream : TStream); overload;
      Procedure SaveToStream(oStream : TStream); overload;

      Property Data : Pointer Read FData Write SetData;
      Property Capacity : Integer Read FCapacity Write SetCapacity;
      Property Owned : Boolean Read FOwned Write SetOwned;
{$IFNDEF UT}
      Property AsText : AnsiString Read GetAsText Write SetAsText;
{$ENDIF}
      {$IFNDEF VER130}
      Property AsUnicode : String Read GetAsUnicode Write SetAsUnicode;
      Property Encoding : TEncoding read FEncoding write FEncoding;
      {$ENDIF}
      Property AsBytes : TBytes read GetAsBytes write SetAsBytes;
      {!script show}
  Published

      {@member Size
        The number of bytes in the buffer
      }
      Property Size : Integer Read FCapacity Write SetCapacity;

      {@member AsAscii
        The contents of the buffer as a string, one byte per character.
        The content may include the ascii characters with value < 32,
        including character 0.
      }
      Property AsAscii : AnsiString Read GetAsText Write SetAsText;

  End;

  TAdvBufferClass = Class Of TAdvBuffer;


  PByte = ^Byte;

  TAdvMemoryStream = Class(TAdvAccessStream)
    Private
      FBuffer : TAdvBuffer;
      FCurrentPointer : PByte;
      FSize : Int64;
      FPosition : Int64;
      FExpand : Boolean;

      Function GetCapacity: Int64;
      Procedure SetCapacity(Const Value: Int64);

      Function GetDataPointer : Pointer;
      Procedure SetDataPointer(Const Value : Pointer);

      Procedure SetBuffer(Const Value: TAdvBuffer);

      Function GetAsText: String;
      Procedure SetAsText(Const Value: String);

    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

      Function GetSize : Int64; Override;
      Procedure SetSize(Const Value : Int64); Override;

      Function GetPosition : Int64; Override;
      Procedure SetPosition(Const Value : Int64); Override;

      Function ValidPosition(Const iValue : Int64) : Boolean; 

      Procedure UpdateCurrentPointer;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Clone : TAdvMemoryStream;
      Function Link : TAdvMemoryStream;

      Procedure Assign(oObject : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler);

      Procedure Read(Var aBuffer; iSize : Cardinal); Override;
      Procedure Write(Const aBuffer; iSize : Cardinal); Override;

      Procedure DeleteRange(Const iFromPosition, iToPosition : Integer);

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Function Assignable : Boolean; Override;

      Function Equal(oMemory : TAdvMemoryStream) : Boolean;

      Property Buffer : TAdvBuffer Read FBuffer Write SetBuffer;
      Property DataPointer : Pointer Read GetDataPointer Write SetDataPointer;
      Property CurrentPointer : PByte Read FCurrentPointer;
      Property Capacity : Int64 Read GetCapacity Write SetCapacity;
      Property Size; // declared in TAdvAccessStream.
      Property Expand : Boolean Read FExpand Write FExpand;
      Property AsText : String Read GetAsText Write SetAsText;
  End;

  EAdvMemoryStream = Class(EAdvStream);

  TAdvVCLStream = Class(TAdvStream)
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
      FStream : TAdvStream;

      Function GetStream: TAdvStream;
      Procedure SetStream(Const Value: TAdvStream);

    Protected
      Procedure SetSize(NewSize: LongInt); Override;

    Public
      Constructor Create; Overload; Virtual;
      Constructor Create(Stream : TAdvStream); Overload; Virtual;
      Destructor Destroy; Override;

      Function Read(Var aBuffer; iCount: LongInt): LongInt; Override;
      Function Write(Const aBuffer; iCount: LongInt): LongInt; Override;
      Function Seek(iOffset: LongInt; iOrigin: Word): LongInt; Override;

      Property Stream : TAdvStream Read GetStream Write SetStream;
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
  TAdvStreamAdapterI = Class(TStreamAdapter)
    Public
      Function Stat(Out statstg: TStatStg; grfStatFlag: TStreamDWord): HResult; Override; Stdcall;
  End;

  TAdvIStreamAdapter = Class(TAdvObject, IStream)
    Private
      FStream : TAdvAccessStream;

      Function GetStream: TAdvAccessStream;
      Procedure SetStream(Const Value: TAdvAccessStream);

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

      Property Stream: TAdvAccessStream Read GetStream Write SetStream;
  End;
  {$ENDIF}

  TStream = Classes.TStream;


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

  TAdvObject = FHIR.Support.Objects.TAdvObject;


Type
  TAdvBinaryFiler = Class(TAdvStreamFiler)
    Private
//    FReducedClassNames : Boolean;
//    FReducedClassList : TAdvClassList;

    Protected
      Procedure DefineBlock(Var Value; Count : Integer); Virtual;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function Link : TAdvBinaryFiler;

      Procedure Clear; Override;

      Procedure DefineInteger(Var Value : Integer); Override;
      Procedure DefineInteger(Var Value : Int64); Override;
      Procedure DefineInteger(Var Value : Cardinal); Override;
      Procedure DefineInteger(Var Value : Word); Override;
      Procedure DefineInteger(Var Value : Byte); Override;

      Procedure DefineReal(Var Value : Real); Override;
      Procedure DefineReal(Var Value : Extended); Override;

      Procedure DefineBoolean(Var Value : Boolean); Override;

      Procedure DefineString(Var Value : TShortString); Overload; Override;
      Procedure DefineString(Var Value : TLongString); Overload; Override;

      Procedure DefineBinary(Var Buffer; iCount : Integer); Override;

      Procedure DefineEnumerated(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = ''); Override;
      Procedure DefineSet(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = ''); Override;

      Procedure DefineDateTime(Var Value : TDateTime); Override;
      Procedure DefineDuration(Var Value : TDuration); Override;
      Procedure DefineCurrency(Var Value : TCurrency); Override;
      Procedure DefineColour(Var Value : TColour); Override;

//    Property ReducedClassNames : Boolean Read FReducedClassNames Write FReducedClassNames;
  End;

  TAdvBinaryWriter = Class(TAdvBinaryFiler)
    Private
      Procedure WriteString(Const sValue : String);
      Procedure WriteClass(Const sValue : String);

    Protected
      Procedure DefineBlock(Var Value; Count : Integer); Override;

    Public
      Procedure DefineValue(Value : TAdvTag); Override;

      Procedure DefineClass(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineReference(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineObject(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineResource(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineChar(Var Value : Char); Override;
      Procedure DefineString(Var Value : TLongString); Override;
  End;

  TAdvBinaryReader = Class(TAdvBinaryFiler)
    Private
      FCache : TAdvTag;

      Function ReadString: String;
      Function ReadClass: String;

    Protected
      Procedure DefineBlock(Var Value; Count : Integer); Override;

    Public
      Procedure Clear; Override;

      Function Peek : TAdvTag; Override;

      Procedure DefineValue(Value : TAdvTag); Override;

      Procedure DefineClass(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineReference(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineObject(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineResource(Var Value; aClass : TAdvObjectClass = Nil); Override;
      Procedure DefineChar(Var Value : Char); Override;
      Procedure DefineString(Var Value : TLongString); Override;
  End;

  TAdvBufferList = Class(TAdvPersistentList)
    Private
      Function GetBuffer(iIndex: Integer): TAdvBuffer;

    Protected
      Function ItemClass: TAdvObjectClass; Override;

    Public
      Property Buffers[iIndex : Integer] : TAdvBuffer Read GetBuffer; Default;
  End;


  TAdvNameBuffer = Class(TAdvBuffer)
    Private
      FName : String;

    Public
      Function Link : TAdvNameBuffer;
      Function Clone : TAdvNameBuffer;

      Procedure Assign(oObject : TAdvObject); Override;
      Procedure Define(oFiler : TAdvFiler); Override;

      Property Name : String Read FName Write FName;
  End;

  TAdvNameBufferList = Class(TAdvBufferList)
    Private
      Function GetBuffer(iIndex : Integer) : TAdvNameBuffer;

    Protected
      Function ItemClass : TAdvObjectClass; Override;

      Procedure DefaultCompare(Out aEvent : TAdvItemListCompare); Override;

      Function CompareByName(pA, pB : Pointer) : Integer; Virtual;

    Public
      Function Link : TAdvNameBufferList;
      Function Clone : TAdvNameBufferList; 

      Function GetByName(Const sName : String) : TAdvNameBuffer;
      Function IndexByName(Const sName : String) : Integer;
      Function ExistsByName(Const sName : String) : Boolean;
      Procedure Merge(oBuffers : TAdvNameBufferList);

      Property Buffer[iIndex : Integer] : TAdvNameBuffer Read GetBuffer; Default;
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

  EAfs = Class(EAdvException);

  TAfsObject = Class(TAdvStringHashEntry)
  Protected
    Procedure RaiseError(Const sMethod, sException : String); Override;
  End; { TAfsObject }

  TAfsClass = Class Of TAfsObject;

  TAfsIterator = Class(TAdvIterator)
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

  TAfsStream = Class(TAdvAccessStream)
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

    Procedure Assign(oSource : TAdvObject); Override;

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

  TAfsEntities = Class(TAdvStringHashTable)
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

  TAfsList = Class(TAdvObjectList)
  Private
    Function GetEntities(Index : Integer) : TAfsEntity;
    Procedure SetEntities(Index : Integer; Const Value : TAfsEntity);
  Protected
    Function CompareName(pA, pB : Pointer) : Integer; Overload; Virtual;

    Procedure DefaultCompare(Out aCompare : TAdvItemListCompare); Overload; Override;

    Function ItemClass : TAdvClass; Override;
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
  TAfsStreamManager = Class(TAdvPersistent)
  Private
    FVolume  : TAfsVolume;
    FMode    : TAfsMode;
    FStreams : TAdvObjectMatch;

    Procedure SetVolume(Const Value: TAfsVolume);

  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Open; Overload; Virtual;
    Procedure Close; Overload; Virtual;

    Function Open(Const sName : String) : TAdvStream; Overload; Virtual;
    Procedure Close(oStream : TAdvStream); Overload; Virtual;
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
  FHIR.Support.Binary, FHIR.Support.Math, FHIR.Support.Strings, FHIR.Support.Factory;

Function TAdvStream.Link : TAdvStream;
Begin
  Result := TAdvStream(Inherited Link);
End;


Function TAdvStream.ErrorClass : EAdvExceptionClass;
Begin
  Result := EAdvStream;
End;


Procedure TAdvStream.Read(Var Buffer; iCount: Cardinal);
Begin
End;


Procedure TAdvStream.Write(Const Buffer; iCount: Cardinal);
Begin
End;


Function TAdvStream.Readable : Int64;
Begin
  Result := 0;
End;


Function TAdvStream.Writeable : Int64;
Begin
  Result := 0;
End;


Function TAdvStream.Assignable: Boolean;
Begin
  Result := False;
End;


Function TAdvStreamList.ItemClass: TAdvObjectClass;
Begin
  Result := TAdvStream;
End;


Function TAdvStreamList.GetStream(iIndex: Integer): TAdvStream;
Begin
  Result := TAdvStream(ObjectByIndex[iIndex]);
End;


Procedure TAdvStreamList.SetStream(iIndex: Integer; Const Value: TAdvStream);
Begin
  ObjectByIndex[iIndex] := Value;
End;


Constructor TAdvStreamAdapter.Create;
Begin
  Inherited;

  FStream := Nil;
End;


Destructor TAdvStreamAdapter.Destroy;
Begin
  FStream.Free;
  FStream := Nil;

  Inherited;
End;

{$IFOPT C+}
Function TAdvStreamAdapter.GetStream: TAdvStream;
Begin
  Assert(Invariants('GetStream', FStream, TAdvStream, 'FStream'));

  Result := FStream;
End;
{$ENDIF}

Procedure TAdvStreamAdapter.SetStream(oStream : TAdvStream);
Begin
  Assert(Not Assigned(oStream) Or Invariants('SetStream', oStream, TAdvStream, 'oStream'));

  FStream.Free;
  FStream := oStream;
End;


Function TAdvStreamAdapter.HasStream: Boolean;
Begin
  Result := Assigned(FStream);
End;


Procedure TAdvStreamAdapter.Read(Var Buffer; iCount: Cardinal);
Begin 
  Stream.Read(Buffer, iCount);
End;


Procedure TAdvStreamAdapter.Write(Const Buffer; iCount: Cardinal);
Begin
  Stream.Write(Buffer, iCount);
End;


Function TAdvStreamAdapter.Readable : Int64;
Begin
  Result := Stream.Readable;
End;  


Function TAdvStreamAdapter.Writeable : Int64;
Begin 
  Result := Stream.Writeable;
End;  


Function TAdvAccessStream.Link : TAdvAccessStream;
Begin 
  Result := TAdvAccessStream(Inherited Link);
End;


Function TAdvAccessStream.GetPosition : Int64;
Begin 
  Result := 0;
End;  


Function TAdvAccessStream.GetSize : Int64;
Begin 
  Result := 0;
End;  


Procedure TAdvAccessStream.SetPosition(Const Value: Int64);
Begin 
End;  


Procedure TAdvAccessStream.SetSize(Const Value: Int64);
Begin 
End;


Function TAdvAccessStreamAdapter.GetPosition : Int64;
Begin
  Result := Stream.Position;
End;


Function TAdvAccessStreamAdapter.GetSize : Int64;
Begin 
  Result := Stream.Size;
End;  


Procedure TAdvAccessStreamAdapter.SetPosition(Const Value: Int64);
Begin
  Stream.Position := Value;
End;  


Procedure TAdvAccessStreamAdapter.SetSize(Const Value: Int64);
Begin 
  Stream.Size := Value;
End;  


Procedure TAdvAccessStreamAdapter.Read(Var Buffer; iCount: Cardinal);
Begin 
  Stream.Read(Buffer, iCount);
End;  


Procedure TAdvAccessStreamAdapter.Write(Const Buffer; iCount: Cardinal);
Begin
  Stream.Write(Buffer, iCount);
End;  


Function TAdvAccessStreamAdapter.Writeable : Int64;
Begin
  Result := Stream.Writeable;
End;


Function TAdvAccessStreamAdapter.Readable : Int64;
Begin
  Result := Stream.Readable;
End;


Function TAdvAccessStreamAdapter.GetStream: TAdvAccessStream;
Begin
  Assert(Invariants('GetStream', FStream, TAdvAccessStream, 'FStream'));

  Result := FStream;
End;


Procedure TAdvAccessStreamAdapter.SetStream(oStream: TAdvAccessStream);
Begin
  Assert(Not Assigned(oStream) Or Invariants('SetStream', oStream, TAdvAccessStream, 'oStream'));

  FStream.Free;
  FStream := oStream;
End;


Constructor TAdvAccessStreamAdapter.Create;
Begin
  Inherited;

  FStream := Nil;
End;


Destructor TAdvAccessStreamAdapter.Destroy;
Begin
  FStream.Free;
  FStream := Nil;

  Inherited;
End;


Procedure TAdvStringStream.Read(Var aBuffer; iCount: Cardinal);
Begin
  If FIndex + iCount > Size Then
    RaiseError('Read', 'Unable to read past end of string.');

  Move((PAnsiChar(FData) + FIndex)^, aBuffer, iCount);
  Inc(FIndex, iCount);
End;


Procedure TAdvStringStream.Write(Const aBuffer; iCount: Cardinal);
Begin
  If FIndex + iCount > Size Then
    Size := FIndex + iCount;

  Move(aBuffer, (PAnsiChar(FData) + FIndex)^, iCount);
  Inc(FIndex, iCount);
End;


Function TAdvStringStream.Writeable : Int64;
Begin
  Result := High(Result);
End;


Function TAdvStringStream.Readable : Int64;
Begin
  Result := Size - Position;
End;


function TAdvStringStream.GetBytes: TBytes;
begin
  result := AnsiStringAsBytes(FData);
end;

Function TAdvStringStream.GetPosition : Int64;
Begin
  Result := FIndex;
End;


Procedure TAdvStringStream.SetPosition(Const iValue: Int64);
Begin
  FIndex := iValue;
End;


Function TAdvStringStream.GetSize : Int64;
Begin
  Result := Length(FData);
End;


Procedure TAdvStringStream.SetSize(Const iValue: Int64);
Begin
  SetLength(FData, iValue);
  If FIndex > Cardinal(Length(FData)) Then
    FIndex := Length(FData);
End;


procedure TAdvStringStream.SetBytes(const Value: TBytes);
begin
  FData := BytesAsAnsiString(value);
end;

Procedure TAdvStringStream.SetData(Const Value: AnsiString);
Begin
  FData := Value;
  If FIndex > Cardinal(Length(FData)) Then
    FIndex := Length(FData)
  Else
    FIndex := 0;
End;



Procedure TAdvVCLStream.Read(Var aBuffer; iCount: Cardinal);
Begin
  Stream.Read(aBuffer, iCount);
End;


Procedure TAdvVCLStream.Write(Const aBuffer; iCount: Cardinal);
Begin
  Stream.Write(aBuffer, iCount);
End;


Function TAdvVCLStream.Readable : Int64;
Begin
  Result := Stream.Size - Stream.Position;
End;


Function TAdvVCLStream.Writeable : Int64;
Begin
  Result := Stream.Size - Stream.Position;
End;


Function TAdvVCLStream.GetStream: TStream;
Begin
  Assert(CheckCondition(Assigned(FStream), 'GetStream', 'No VCL Stream available.'));

  Result := FStream;
End;


Procedure TAdvVCLStream.SetStream(Const Value: TStream);
Begin
  FStream := Value;
End;


Constructor TVCLStream.Create;
Begin
  Inherited;

  FStream := Nil;
End;


constructor TVCLStream.Create(Stream: TAdvStream);
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
  oAccess : TAdvAccessStream;
Begin
  If Not (Stream Is TAdvAccessStream) Then
    Raise EAdvStream.Create(Self, 'Seek', 'Unable to seek in a non-access stream'); // Error is not available.

  oAccess := TAdvAccessStream(Stream);

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
  oAccess : TAdvAccessStream;
Begin
  If Not (Stream Is TAdvAccessStream) Then
    Raise EAdvStream.Create(Self, 'SetSize', 'Unable to set the size of a non-access stream'); // Error is not available.

  oAccess := TAdvAccessStream(Stream);

  oAccess.Size := NewSize;
End;


Function TVCLStream.GetStream: TAdvStream;
Begin
  Result := FStream;
End;


Procedure TVCLStream.SetStream(Const Value: TAdvStream);
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
Function TAdvStreamAdapterI.Stat(Out statstg: TStatStg; grfStatFlag: TStreamDWord): HResult;
Begin
  // TStreamAdapter.stat does not clear the STATSTG structure.
  // http://qc.embarcadero.com/wc/qcmain.aspx?d=45528

  FillChar(statstg, SizeOf(statstg), 0);
  Result := Inherited Stat(statstg, grfStatFlag);
End;


Constructor TAdvIStreamAdapter.Create;
Begin
  Inherited;

  FStream := TAdvAccessStream.Create;
End;

Destructor TAdvIStreamAdapter.Destroy;
Begin
  FStream.Free;

  Inherited;
End;


Function TAdvIStreamAdapter.Read(pv: Pointer; cb: TStreamFixedUInt; pcbRead: PStreamFixedUInt): HResult;

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

Function TAdvIStreamAdapter.Write(pv: Pointer; cb: TStreamFixedUInt; pcbWritten: PStreamFixedUInt): HResult;
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


Function TAdvIStreamAdapter.Seek(dlibMove: Largeint; dwOrigin: TStreamDWORD; out libNewPosition: TStreamLargeUInt): HResult;
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


Function TAdvIStreamAdapter.Revert: HResult;
Begin
  Result := STG_E_REVERTED;
End;


Function TAdvIStreamAdapter.SetSize(libNewSize: TStreamLargeUInt): HResult;
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


Function TAdvIStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: TStreamDWORD): HResult;
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


Function TAdvIStreamAdapter.UnlockRegion(libOffset: TStreamLargeUInt; cb: TStreamLargeUInt; dwLockType: TStreamDWORD): HResult;
Begin
  Result := STG_E_INVALIDFUNCTION;
End;


Function TAdvIStreamAdapter.Clone(Out stm: IStream): HResult;
Begin
  Result := E_NOTIMPL;
End;


Function TAdvIStreamAdapter.Commit(grfCommitFlags: TStreamDWORD): HResult;
Begin
  Result := S_OK;
End;


Function TAdvIStreamAdapter.CopyTo(stm: IStream; cb: TStreamLargeUInt; out cbRead: TStreamLargeUInt; out cbWritten: TStreamLargeUInt): HResult;
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


Function TAdvIStreamAdapter.LockRegion(libOffset: TStreamLargeUInt; cb: TStreamLargeUInt; dwLockType: TStreamDWORD): HResult;
Begin
  Result := STG_E_INVALIDFUNCTION;
End;


Procedure TAdvIStreamAdapter.SetStream(Const Value: TAdvAccessStream);
Begin
  FStream.Free;
  FStream := Value;
End;


Function TAdvIStreamAdapter.GetStream: TAdvAccessStream;
Begin
  Result := FStream;
End;

{$ENDIF}



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




Constructor TAdvBinaryFiler.Create;
Begin
  Inherited;

//FReducedClassList := TAdvClassList.Create;
//FReducedClassList.Sorted;
End;


Destructor TAdvBinaryFiler.Destroy;
Begin
//FReducedClassList.Free;

  Inherited;
End;


Function TAdvBinaryFiler.Link : TAdvBinaryFiler;
Begin
  Result := TAdvBinaryFiler(Inherited Link);
End;


Procedure TAdvBinaryFiler.Clear;
Begin
  Inherited;

//FReducedClassList.Clear;
End;


Procedure TAdvBinaryFiler.DefineBlock(Var Value; Count: Integer);
Begin
End;


Procedure TAdvBinaryFiler.DefineBinary(Var Buffer; iCount : Integer);
Begin 
  Inherited;

  DefineBlock(iCount, SizeOf(iCount));

  DefineBlock(Buffer, iCount);
End;  


Procedure TAdvBinaryFiler.DefineBoolean(Var Value: Boolean);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Integer);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Int64);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Cardinal);
Begin
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Word);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineInteger(Var Value: Byte);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End; 


Procedure TAdvBinaryFiler.DefineReal(Var Value: Real);
Begin
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineReal(Var Value: Extended);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineString(Var Value: TShortString);
Begin 
  Inherited;

  DefineBlock(Value[0], 1);
  DefineBlock(Value[1], Ord(Value[0]));
End;  


Procedure TAdvBinaryFiler.DefineString(Var Value: TLongString);
Begin
  Inherited;
End;


Procedure TAdvBinaryFiler.DefineEnumerated(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = '');
Begin
  // TODO: remove (Peek = atEnumerated8) as it only required for legacy streams

  If (Length(aNames) <= 256) Or (Peek = atEnumerated8) Then
  Begin
    DefineValue(atEnumerated8);
    DefineBlock(Value, 1);

    Assert(CheckCondition((Byte(Value) <= High(aNames)), 'DefineEnumerated', 'Enumeration defined out of range.'));
  End
  Else
  Begin
    DefineValue(atEnumerated16);
    DefineBlock(Value, 2);

    Assert(CheckCondition((Word(Value) <= High(aNames)), 'DefineEnumerated', 'Enumeration defined out of range.'));
  End;
End;


Procedure TAdvBinaryFiler.DefineSet(Var Value; Const aNames : Array Of String; Const sEnumerationName : String = '');
Begin 
  Inherited;

  DefineBlock(Value, RealCeiling(Length(aNames) / 8));
End;  


Procedure TAdvBinaryFiler.DefineDateTime(Var Value: TDateTime);
Begin
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineCurrency(Var Value: TCurrency);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineDuration(Var Value: TDuration);
Begin 
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;  


Procedure TAdvBinaryFiler.DefineColour(Var Value: TColour);
Begin
  Inherited;

  DefineBlock(Value, SizeOf(Value));
End;


Procedure TAdvBinaryWriter.DefineBlock(Var Value; Count: Integer);
Begin
  Stream.Write(Value, Count);
End;


Procedure TAdvBinaryWriter.DefineValue(Value: TAdvTag);
Begin
  DefineBlock(Value, SizeOf(Value));
End;


Procedure TAdvBinaryWriter.WriteString(const sValue: String);
Var
  iLength : Integer;
{$IFNDEF VER130}
  Bytes : TBytes;
{$ENDIF}
Begin
  iLength := Length(sValue);
  DefineBlock(iLength, SizeOf(iLength));

{$IFDEF VER130}
  DefineBlock(Pointer(sValue)^, iLength);
{$ELSE}
  Bytes := SysUtils.TEncoding.ASCII.GetBytes(sValue);
  DefineBlock(Bytes[0], Length(Bytes));
{$ENDIF}
end;


Procedure TAdvBinaryWriter.WriteClass(const sValue: String);
Var
  iLength : Byte;
{$IFNDEF VER130}
  Bytes : TBytes;
{$ENDIF}
Begin
  Assert(CheckCondition(Length(sValue) <= 256, 'WriteClass', 'Class must be less than 256 characters.'));

  iLength := Length(sValue);
  DefineBlock(iLength, SizeOf(iLength));

{$IFDEF VER130}
  DefineBlock(Pointer(sValue)^, iLength);
{$ELSE}
  Bytes := SysUtils.TEncoding.ASCII.GetBytes(sValue);
  DefineBlock(Bytes[0], Length(Bytes));
{$ENDIF}
end;


Procedure TAdvBinaryWriter.DefineString(Var Value: TLongString);
Begin
  Inherited;

  WriteString(Value);
End;


Procedure TAdvBinaryWriter.DefineChar(Var Value : Char);
{$IFNDEF VER130}
Var
  Bytes : TBytes;
{$ENDIF}
Begin
  Inherited;

{$IFDEF VER130}
  DefineBlock(Value, SizeOf(Value));
{$ELSE}
  Bytes := SysUtils.TEncoding.ASCII.GetBytes(Value);
  DefineBlock(Bytes[0], Length(Bytes));
{$ENDIF}
End;


Procedure TAdvBinaryWriter.DefineClass(Var Value; aClass : TAdvObjectClass);
Begin
  If Assigned(TClass(Value)) Then
  Begin
    Assert(Not Assigned(aClass) Or Invariants('DefineClass', TClass(Value), aClass, 'Value'));

    DefineValue(atClass);

    WriteClass(TClass(Value).ClassName);
  End
  Else
  Begin 
    DefineValue(atNil);
  End;  
End;  


Procedure TAdvBinaryWriter.DefineReference(Var Value; aClass : TAdvObjectClass);
Var
  oValue : TObject;
Begin 
  oValue := TObject(Value);

  If Assigned(oValue) Then
  Begin 
    Assert(Assigned(aClass) Or Invariants('DefineReference', oValue, TObject, 'oValue'));
    Assert(Not Assigned(aClass) Or Invariants('DefineReference', oValue, aClass, 'oValue'));

    DefineValue(atReference);
    DefineBlock(Value, SizeOf(TObject));
  End   
  Else
  Begin 
    DefineValue(atNil);
  End;  
End;  


Procedure TAdvBinaryWriter.DefineObject(Var Value; aClass : TAdvObjectClass);
Var
  oObject : TAdvPersistent;
//aClassType : TClass;
//iClassTypeIndex : Integer;
Begin
  oObject := TAdvPersistent(Value);

  If Not Assigned(oObject) Then
  Begin
    DefineValue(atNil);
  End
  Else If Referential And ReferenceManager.Exists(oObject) Then
  Begin
    DefineReference(oObject, aClass);
  End
  Else
  Begin
    Assert(Assigned(aClass) Or Invariants('DefineObject', oObject, TAdvPersistent, 'oObject'));
    Assert(Not Assigned(aClass) Or Invariants('DefineObject', oObject, aClass, 'oObject'));

    // Write the object tag.
    DefineValue(atObject);

    // Write the classname
(*
    aClassType := oObject.ClassType;

    If FReducedClassNames And FReducedClassList.Find(aClassType, iClassTypeIndex) Then
    Begin
      iLength := 0;
      DefineBlock(iLength, SizeOf(iLength));
      DefineBlock(aClassType, SizeOf(aClassType));
    End
    Else
    Begin
*)
      WriteClass(oObject.ClassName);

(*
      If FReducedClassNames Then
        FReducedClassList.Insert(iClassTypeIndex, aClassType);
    End;
*)
    // Write the object reference identifier.
    DefineBlock(oObject, SizeOf(oObject));

    // Remember that this instance has been filed once.
    If Referential Then
      ReferenceManager.Bind(oObject, Nil);

    // DefineBlock the persistent object
    DefineBegin;

    oObject.Save(Self);

    DefineEnd;
  End;
End;


Procedure TAdvBinaryWriter.DefineResource(Var Value; aClass: TAdvObjectClass);
Var
  oObject : TAdvObject;
  sResource : String;
Begin
  oObject := TAdvObject(Value);

  If Not Assigned(oObject) Then
    DefineValue(atNil)
  Else
  Begin
    Assert(Assigned(aClass) Or Invariants('DefineResource', oObject, TAdvObject, 'oObject'));
    Assert(Not Assigned(aClass) Or Invariants('DefineResource', oObject, aClass, 'oObject'));

    // Write the resource tag.
    DefineValue(atResource);

    sResource := ResourceManager.ResolveID(oObject);

    Assert(CheckCondition(sResource <> '', 'DefineResource', 'Resource string must be specified.'));

    WriteString(sResource);
  End;
End;  


Procedure TAdvBinaryReader.Clear;
Begin
  Inherited;

  FCache := atUnknown;
End;


Procedure TAdvBinaryReader.DefineBlock(Var Value; Count: Integer);
Begin
  Stream.Read(Value, Count);
End;


Function TAdvBinaryReader.Peek : TAdvTag;
Begin 
  If FCache = atUnknown Then
    DefineBlock(FCache, SizeOf(FCache)); // Read the tag off the stream

  Result := FCache;
End;


Procedure TAdvBinaryReader.DefineValue(Value: TAdvTag);
Begin 
  If FCache = atUnknown Then
    DefineBlock(FCache, SizeOf(TAdvTag));

  // If read tag doesn't match the supplied type then an exception has occurred
  If FCache <> Value Then
    RaiseError('DefineValue', StringFormat('Expected ''%s'' but found ''%s''', [TagToString(Value), TagToString(FCache)]));

  // Cache will be remember for the next call if the above exception is raised.
  FCache := atUnknown;
End;  


Procedure TAdvBinaryReader.DefineClass(Var Value; aClass : TAdvObjectClass);
Var
  sClass : String;
Begin
  Case Peek Of
    atNil :
    Begin
      DefineValue(atNil);
      TClass(Value) := Nil;
    End;  

    atClass :
    Begin
      DefineValue(atClass);

      sClass := ReadClass;

      TClass(Value) := Factory.Get(sClass);

      Assert(CheckCondition(Assigned(TClass(Value)), 'DefineClass', 'Class not registered ' + sClass));

      Assert(Not Assigned(aClass) Or Invariants('DefineClass', TClass(Value), aClass, 'Value'));
    End;  
  Else
    RaiseError('DefineClass', StringFormat('Expected ''%s'' or ''%s'' but found ''%s''', [TagToString(atClass), TagToString(atNil), TagToString(Peek)]));
  End;
End;  


Procedure TAdvBinaryReader.DefineReference(Var Value; aClass : TAdvObjectClass);
Var
  pObject : ^TObject;
  oReference : TAdvPersistent;
  oObject : TAdvPersistent;
Begin 
  pObject := @TObject(Value);

  Case Peek Of
    atNil :
    Begin
      DefineValue(atNil);

      pObject^.Free;
      pObject^ := Nil;
    End;  

    atReference :
    Begin
      DefineValue(atReference);
      DefineBlock(oReference, SizeOf(oReference));

      oObject := ReferenceManager.Get(oReference);
      If Not Assigned(oObject) Then
        RaiseError('DefineReference', 'Reference does not have an associated object.');

      pObject^.Free;
      pObject^ := oObject;
    End;   
  Else
    RaiseError('DefineReference', StringFormat('Expected ''%s'' or ''%s'' but found ''%s''', [TagToString(atReference), TagToString(atNil), TagToString(Peek)]));
  End;  
End;  


Procedure TAdvBinaryReader.DefineObject(Var Value; aClass : TAdvObjectClass);
Var
  sClass  : String;
  pObject : ^TAdvPersistent;
  oReference : TAdvPersistent;
{$IFOPT C+}
  aRequiredClass : TAdvObjectClass;
{$ENDIF}
Begin
{$IFOPT C+}
  If aClass = Nil Then
    aRequiredClass := TAdvPersistent
  Else
    aRequiredClass := aClass;
{$ENDIF}

  // Pointer to the variable parameter, this is done so we can change its value.
  pObject := @TAdvPersistent(Value);

  // Read the type off the stream
  Case Peek Of
    atNil :
    Begin
      DefineValue(atNil);

      pObject^.Free;
      pObject^ := Nil;
    End;

    atObject :
    Begin
      DefineValue(atObject);

      // Read the class name.
      sClass := ReadClass;

      If Not Assigned(pObject^) Then
      Begin
        pObject^ := TAdvPersistent(Factory.Make(sClass));
      End
      Else
      Begin
      {$IFOPT C+}
        Assert(Invariants('DefineObject', pObject^, aRequiredClass, 'pObject^'));
      {$ENDIF}

        If Not Factory.IsEquivalentClass(pObject^.ClassType, sClass) Then
          RaiseError('DefineObject', StringFormat('Expected object ''%s'' but found object ''%s''', [pObject^.ClassName, sClass]));
      End;

      // Read the objects old reference and match to the new reference.
      DefineBlock(oReference, SizeOf(oReference));

      If Referential Then
      Begin
        ReferenceManager.Bind(oReference, pObject^);
      {$IFOPT C+}
        Assert(Invariants('DefineObject', pObject^, aRequiredClass, 'pObject^'));
      {$ENDIF}
      End;

      // Load the object's properties
      DefineBegin;

      pObject^.Load(Self);

      DefineEnd;
    End;

    atReference :
    Begin
      If Not Referential Then
        RaiseError('DefineObject', 'Can only load from references if the filer is in referential mode.');

      DefineValue(atReference);
      DefineBlock(oReference, SizeOf(oReference));

      pObject^.Free;
      pObject^ := TAdvPersistent(TAdvObject(ReferenceManager.Get(oReference)).Link);
    End; 
  Else
    RaiseError('DefineObject', StringFormat('Expected ''%s'', ''%s'' or ''%s'' but found ''%s''', [TagToString(atObject), TagToString(atReference), TagToString(atNil), TagToString(Peek)]));
  End; 
End;


Procedure TAdvBinaryReader.DefineResource(Var Value; aClass: TAdvObjectClass);
Var
  sResource : String;
  oObject : TAdvObject;
  pObject : ^TAdvObject;
Begin
  // Pointer to the variable parameter, this is done so we can change its value.
  pObject := @TAdvObject(Value);

  Case Peek Of
    atNil :
    Begin
      DefineValue(atNil);

      pObject^.Free;
      pObject^ := Nil;
    End;

    atResource :
    Begin
      DefineValue(atResource);

      sResource := ReadString;

      oObject := ResourceManager.ResolveObject(sResource, aClass);

    {$IFOPT C+}
      If Assigned(oObject) Then
      Begin
        If Assigned(aClass) Then
          Invariants('DefineResource', oObject, aClass, 'oObject')
        Else
          Invariants('DefineResource', oObject, TAdvObject, 'oObject');
      End;
    {$ENDIF}

      pObject^.Free;
      pObject^ := oObject.Link;
    End;
  Else
    RaiseError('DefineResource', StringFormat('Expected ''%s'' or ''%s'' but found ''%s''', [TagToString(atResource), TagToString(atNil), TagToString(Peek)]));
  End;
End;


Function TAdvBinaryReader.ReadClass : String;
Var
  iLength : Byte;
{$IFNDEF VER130}
  aBuffer : TBytes;
{$ENDIF}
Begin
  DefineBlock(iLength, SizeOf(iLength));

{$IFDEF VER130}
  SetLength(Result, iLength);
  DefineBlock(Pointer(Result)^, iLength);
{$ELSE}
  SetLength(aBuffer, iLength);
  DefineBlock(aBuffer[0], iLength);

  Result := SysUtils.TEncoding.ASCII.GetString(aBuffer);
{$ENDIF}
End;


Function TAdvBinaryReader.ReadString : String;
Var
  iLength : Integer;
{$IFNDEF VER130}
  aBuffer : TBytes;
{$ENDIF}
Begin
  DefineBlock(iLength, SizeOf(iLength));

{$IFDEF VER130}
  SetLength(Result, iLength);
  DefineBlock(Pointer(Result)^, iLength);
{$ELSE}
  SetLength(aBuffer, iLength);
  DefineBlock(aBuffer[0], iLength);

  Result := SysUtils.TEncoding.ASCII.GetString(aBuffer);
{$ENDIF}
End;


Procedure TAdvBinaryReader.DefineString(Var Value: TLongString);
Begin
  Inherited;

  Value := ReadString;
End;


Procedure TAdvBinaryReader.DefineChar(Var Value : Char);
{$IFNDEF VER130}
Var
  aBuffer : TBytes;
{$ENDIF}
Begin
  Inherited;

{$IFDEF VER130}
  DefineBlock(Value, SizeOf(Value));
{$ELSE}
  SetLength(aBuffer, 1);
  DefineBlock(aBuffer[0], Length(aBuffer));
  Value := SysUtils.TEncoding.ASCII.GetString(aBuffer)[1];
{$ENDIF}
End;



Constructor TAdvBuffer.Create;
Begin 
  Inherited;
  {$IFNDEF VER130}
  FEncoding := TEncoding.UTF8;
  {$ENDIF}
  FOwned := True;
End;  


Destructor TAdvBuffer.Destroy;
Begin 
  If FOwned Then
    MemoryDestroy(FData, FCapacity);

  Inherited;
End;  


Function TAdvBuffer.Clone : TAdvBuffer;
Begin 
  Result := TAdvBuffer(Inherited Clone);
End;  


Function TAdvBuffer.Link : TAdvBuffer;
Begin 
  Result := TAdvBuffer(Inherited Link);
End;  


Procedure TAdvBuffer.Assign(oObject : TAdvObject);
Begin 
  Inherited;

  Copy(TAdvBuffer(oObject));
End;


Procedure TAdvBuffer.Define(oFiler : TAdvFiler);
Begin 
  Inherited;
End;


Procedure TAdvBuffer.Load(oFiler: TAdvFiler);
Var
  iCapacity : Integer;
Begin

  Define(oFiler);

  oFiler['Size'].DefineInteger(iCapacity);

  If Not FOwned Then
  Begin
    FData := Nil;
    FOwned := True;
  End;

  SetCapacity(iCapacity);

  oFiler['Data'].DefineBinary(FData^, iCapacity);
End;


Procedure TAdvBuffer.Save(oFiler: TAdvFiler);
Begin
  Define(oFiler);

  oFiler['Size'].DefineInteger(FCapacity);
  oFiler['Data'].DefineBinary(FData^, FCapacity);
End;


Procedure TAdvBuffer.LoadFromStream(oStream: TAdvStream);
Begin
  Assert(Invariants('LoadFromStream', oStream, TAdvStream, 'oStream'));

  oStream.Read(Data^, Capacity);
End;


Procedure TAdvBuffer.LoadFromStream(oStream: TStream);
Begin
//  Assert(Invariants('LoadFromStream', oStream, TStream, 'oStream'));

  Capacity := oStream.Size - oStream.Position;
  oStream.Read(Data^, Capacity);
End;


Procedure TAdvBuffer.SaveToStream(oStream: TAdvStream);
Begin
  Assert(Invariants('SaveToStream', oStream, TAdvStream, 'oStream'));

  oStream.Write(Data^, Capacity);
End;


Procedure TAdvBuffer.SaveToStream(oStream: TStream);
var
  i : integer;
Begin
 // Assert(Invariants('SaveToStream', oStream, TStream, 'oStream'));

   i := oStream.Position;
   oStream.Write(Data^, Capacity);
   assert(oStream.Position = i + Capacity);
End;


Procedure TAdvBuffer.LoadFromFile(oFile: TAdvFile);
Begin
  Assert(Invariants('LoadFromFile', oFile, TAdvFile, 'oFile'));

  Capacity := oFile.Size;

  LoadFromStream(oFile);
End;


Procedure TAdvBuffer.SaveToFile(oFile: TAdvFile);
Begin
  Assert(Invariants('SaveToFile', oFile, TAdvFile, 'oFile'));

  SaveToStream(oFile);
End;


Procedure TAdvBuffer.LoadFromFileName(Const sFilename: String);
Var
  oFile : TAdvFile;
Begin
  oFile := TAdvFile.Create(sFilename, fmOpenRead);
  Try
    LoadFromFile(oFile);
  Finally
    oFile.Free;
  End;
End;


Procedure TAdvBuffer.SaveToFileName(Const sFilename: String);
Var
  oFile : TAdvFile;
Begin
  oFile := TAdvFile.Create(sFilename, fmCreate);
  Try
    SaveToFile(oFile);
  Finally
    oFile.Free;
  End;
End;


Procedure TAdvBuffer.Clear;
Begin 

  Capacity := 0;
End;  


Function TAdvBuffer.Equal(oBuffer: TAdvBuffer): Boolean;
Begin
  Assert(Invariants('Equal', oBuffer, TAdvBuffer, 'oBuffer'));

  Result := Compare(oBuffer) = 0;
End;


Function TAdvBuffer.Compare(oBuffer: TAdvBuffer): Integer;
Begin
  Assert(Invariants('Compare', oBuffer, TAdvBuffer, 'oBuffer'));

  Result := IntegerCompare(Capacity, oBuffer.Capacity);

  If Result = 0 Then
    Result := MemoryCompare(Data, oBuffer.Data, Capacity);
End;  


Procedure TAdvBuffer.SetCapacity(Const Value: Integer);
Begin 
  If (Value <> Capacity) Then
  Begin 
    Assert(CheckCondition(Value >= 0, 'SetCapacity', StringFormat('Unable to change the Capacity to %d', [Value])));

    If FOwned Then
      MemoryResize(FData, FCapacity, Value);

    FCapacity := Value;
  End;  
End;  


Procedure TAdvBuffer.SetData(Const Value: Pointer);
Begin 

  If FData <> Value Then
  Begin 
    SetCapacity(0);

    FData := Value;
    FOwned := False;
  End;  
End;  


Procedure TAdvBuffer.SetOwned(Const Value: Boolean);
Begin 

  FOwned := Value;
End;  


Function TAdvBuffer.GetAsText : AnsiString;
Begin
  Result := ExtractAscii(Capacity);
End;


{$IFNDEF VER130}

function TAdvBuffer.GetAsUnicode: String;
var
  chars: SysUtils.TCharArray;
begin
  chars := FEncoding.GetChars(AsBytes);
  SetString(Result, PChar(chars), Length(chars));
end;

procedure TAdvBuffer.SetAsUnicode(const Value: String);
begin
  AsBytes := FEncoding.GetBytes(Value);
end;
{$ENDIF}

Procedure TAdvBuffer.SetAsText(Const Value: AnsiString);
Begin

  Capacity := Length(Value);
  MemoryMove(Pointer(Value), Data, Capacity);
End;


Procedure TAdvBuffer.Copy(oBuffer: TAdvBuffer);
Begin
  CopyRange(oBuffer, 0, oBuffer.Capacity);
End;


Procedure TAdvBuffer.CopyRange(oBuffer: TAdvBuffer; Const iIndex, iLength : Integer);
Begin
  Assert(Invariants('CopyRange', oBuffer, TAdvBuffer, 'oBuffer'));
  Assert(CheckCondition((iIndex >= 0) And (iIndex + iLength <= oBuffer.Capacity), 'CopyRange', 'Attempted to copy invalid part of the buffer.'));

  SetCapacity(iLength);

  MemoryMove(oBuffer.Offset(iIndex), Data, iLength);
End;  


Procedure TAdvBuffer.Move(Const iSource, iTarget, iLength : Integer);
Begin
  Assert(CheckCondition((iSource >= 0) And (iSource + iLength <= Capacity), 'Copy', 'Attempted to move from an invalid part of the buffer.'));
  Assert(CheckCondition((iTarget >= 0) And (iTarget + iLength <= Capacity), 'Copy', 'Attempted to move to an invalid part of the buffer.'));

  MemoryMove(Offset(iSource), Offset(iTarget), iLength);
End;  


Function TAdvBuffer.Offset(iIndex: Integer): Pointer;
Begin 
  Assert(CheckCondition((iIndex >= 0) And (iIndex <= Capacity), 'Offset', 'Attempted to access invalid offset in the buffer.'));

  Result := Pointer(NativeUInt(Data) + NativeUInt(iIndex));
End;  


Function TAdvBufferList.GetBuffer(iIndex: Integer): TAdvBuffer;
Begin 
  Result := TAdvBuffer(ObjectByIndex[iIndex]);
End;  


Function TAdvBufferList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvBuffer;
End;


Function TAdvBuffer.ExtractAscii(Const iLength: Integer): AnsiString;
Begin
  Result := MemoryToString(Data, iLength);
End;

{$IFNDEF VER130}
Function TAdvBuffer.ExtractUnicode(Const iLength: Integer): String;
Begin
  result := System.copy(GetAsUnicode, 1, iLength);
End;
{$ENDIF}


Function TAdvBuffer.StartsWith(Const sValue: String): Boolean;
Begin
  {$IFDEF VER130}
  Result := (Length(sValue) <= Capacity) And StringStartsWith(ExtractAscii(Length(sValue)), sValue);
  {$ELSE}
  Result := (Length(sValue) <= Capacity) And StringStartsWith(ExtractUnicode(Length(sValue)), sValue);
  {$ENDIF}
End;


{$IFNDEF UT}
constructor TAdvBuffer.Create(sText: String);
begin
  Create;
  AsUnicode := stext;
end;
{$ENDIF}

function TAdvBuffer.GetAsBytes: TBytes;
begin
  SetLength(result, Capacity);
  if Capacity > 0 then
    System.move(FData^, result[0], capacity);
end;

procedure TAdvBuffer.SetAsBytes(const Value: TBytes);
begin
  SetCapacity(length(Value));
  if capacity > 0 then
    System.move(Value[0], FData^, Capacity);
end;

Procedure TAdvNameBuffer.Assign(oObject : TAdvObject);
Begin
  Inherited;

  FName := TAdvNameBuffer(oObject).FName;
End;


Procedure TAdvNameBuffer.Define(oFiler : TAdvFiler);
Begin
  Inherited;

  oFiler['Name'].DefineString(FName);
End;


Function TAdvNameBuffer.Link : TAdvNameBuffer;
Begin
  Result := TAdvNameBuffer(Inherited Link);
End;


Function TAdvNameBuffer.Clone : TAdvNameBuffer;
Begin
  Result := TAdvNameBuffer(Inherited Clone);
End;


Function TAdvNameBufferList.Clone : TAdvNameBufferList;
Begin
  Result := TAdvNameBufferList(Inherited Clone);
End;


Function TAdvNameBufferList.Link : TAdvNameBufferList;
Begin
  Result := TAdvNameBufferList(Inherited Link);
End;


Function TAdvNameBufferList.ItemClass : TAdvObjectClass;
Begin
  Result := TAdvNameBuffer;
End;


Procedure TAdvNameBufferList.Merge(oBuffers : TAdvNameBufferList);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oBuffers.Count - 1 Do
    Add(oBuffers[iLoop].Clone);
End;


Function TAdvNameBufferList.GetBuffer(iIndex : Integer) : TAdvNameBuffer;
Begin
  Result := TAdvNameBuffer(ObjectByIndex[iIndex]);
End;


Function TAdvNameBufferList.GetByName(Const sName : String) : TAdvNameBuffer;
Begin
  Result := TAdvNameBuffer(Get(IndexByName(sName)));
End;


Function TAdvNameBufferList.ExistsByName(Const sName : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(sName));
End;


Function TAdvNameBufferList.CompareByName(pA, pB: Pointer): Integer;
Begin
  Result := StringCompare(TAdvNameBuffer(pA).Name, TAdvNameBuffer(pB).Name);
End;


Function TAdvNameBufferList.IndexByName(Const sName: String): Integer;
Var
  oBuffer : TAdvNameBuffer;
Begin
  oBuffer := TAdvNameBuffer(ItemNew);
  Try
    oBuffer.Name := sName;

    If Not Find(oBuffer, Result, CompareByName) Then
      Result := -1;
  Finally
    oBuffer.Free;
  End;
End;


Procedure TAdvNameBufferList.DefaultCompare(Out aEvent: TAdvItemListCompare);
Begin
  aEvent := CompareByName;
End;


Constructor TAdvMemoryStream.Create;
Begin
  Inherited;

  FBuffer := TAdvBuffer.Create;
  FExpand := True;
End;


Destructor TAdvMemoryStream.Destroy;
Begin
  FBuffer.Free;

  Inherited;
End;


Function TAdvMemoryStream.Clone : TAdvMemoryStream;
Begin
  Result := TAdvMemoryStream(Inherited Clone);
End;  


Function TAdvMemoryStream.Link : TAdvMemoryStream;
Begin 
  Result := TAdvMemoryStream(Inherited Link);
End;  


Procedure TAdvMemoryStream.Assign(oObject: TAdvObject);
Begin 
  Inherited;

  FBuffer.Assign(TAdvMemoryStream(oObject).Buffer);
  FSize := TAdvMemoryStream(oObject).Size;
  FPosition := TAdvMemoryStream(oObject).Position;

  UpdateCurrentPointer;
End;  


Procedure TAdvMemoryStream.Define(oFiler: TAdvFiler);
Begin 
  // Introduced to circumvent the inability of a TAdvStream descendent to be persistent.

  oFiler['Buffer'].DefineObject(FBuffer);
  oFiler['Size'].DefineInteger(FSize);
  oFiler['Position'].DefineInteger(FPosition);

  UpdateCurrentPointer;
End;  


Procedure TAdvMemoryStream.UpdateCurrentPointer;
Begin 
  FCurrentPointer := Pointer(NativeUInt(FBuffer.Data) + FPosition);
End;  


Procedure TAdvMemoryStream.Read(Var aBuffer; iSize : Cardinal);
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


Procedure TAdvMemoryStream.Write(Const aBuffer; iSize : Cardinal);
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


Function TAdvMemoryStream.Readable : Int64;
Begin 
  Result := FSize - FPosition;
End;  


Function TAdvMemoryStream.Writeable : Int64;
Begin 
  If FExpand Then
    Result := MaxInt
  Else
    Result := FBuffer.Capacity - FPosition;
End;  


Function TAdvMemoryStream.GetSize : Int64;
Begin 
  Result := FSize;
End;  


Procedure TAdvMemoryStream.SetSize(Const Value: Int64);
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


Function TAdvMemoryStream.GetPosition : Int64;
Begin 
  Result := FPosition;
End;  


Procedure TAdvMemoryStream.SetPosition(Const Value: Int64);
Begin
  Assert(CheckCondition((Value >= 0) And (Value <= FBuffer.Capacity), 'SetPosition', 'Attempted to set position outside of memory.'));

  FPosition := Value;
  UpdateCurrentPointer;
End;  


Function TAdvMemoryStream.GetCapacity : Int64;
Begin 
  Result := FBuffer.Capacity;
End;  


Procedure TAdvMemoryStream.SetCapacity(Const Value: Int64);
Begin 
  Assert(CheckCondition((Value >= Size), 'SetCapacity', StringFormat('Unable to change the capacity to less than the size %d.', [Value])));

  FBuffer.Capacity := Value;
  UpdateCurrentPointer;
End;


Function TAdvMemoryStream.GetDataPointer : Pointer;
Begin 
  Result := FBuffer.Data;
End;  


Procedure TAdvMemoryStream.SetDataPointer(Const Value: Pointer);
Begin 
  FBuffer.Data := Value;
  UpdateCurrentPointer;
End;  


Procedure TAdvMemoryStream.SetBuffer(Const Value: TAdvBuffer);
Begin 
  Assert(Not Assigned(Value) Or Invariants('SetBuffer', Value, TAdvBuffer, 'Value'));

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


Function TAdvMemoryStream.GetAsText : String;
Begin
{$IFDEF VER130}
  Result := MemoryToString(DataPointer, Size);
{$ELSE}
  Result := SysUtils.TEncoding.ASCII.GetString(TBytes(DataPointer), 0, Size);
{$ENDIF}
End;


Procedure TAdvMemoryStream.SetAsText(Const Value: String);
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


Function TAdvMemoryStream.ErrorClass : EAdvExceptionClass;
Begin 
  Result := EAdvMemoryStream;
End;  


Function TAdvMemoryStream.Equal(oMemory: TAdvMemoryStream): Boolean;
Begin 
  Result := (Size = oMemory.Size) And (MemoryCompare(Buffer.Data, oMemory.Buffer.Data, Size) = 0);
End;  


Procedure TAdvMemoryStream.DeleteRange(Const iFromPosition, iToPosition: Integer);
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


Function TAdvMemoryStream.ValidPosition(Const iValue: Int64): Boolean;
Begin
  Result := (iValue >= 0) And (iValue <= Size);
End;


Function TAdvMemoryStream.Assignable: Boolean;
Begin
  Result := True;
End;


{ TAdvFile }

constructor TAdvFile.Create(const AFileName: string; Mode: Word);
begin
  inherited create;
  FStream := TFileStream.Create(AFileName, mode);
end;

destructor TAdvFile.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TAdvFile.ErrorClass: EAdvExceptionClass;
begin
  Result := EAdvFile;
end;

function TAdvFile.GetHandle: THandle;
begin
  result := FStream.Handle;
end;

function TAdvFile.GetPosition: Int64;
begin
  result := FStream.Position;
end;

function TAdvFile.GetSize: Int64;
begin
  result := FStream.Size;
end;

function TAdvFile.Link: TAdvFile;
begin
  result := TAdvFile(Inherited Link);
end;

procedure TAdvFile.RaiseError(aException: EAdvExceptionClass; const sMethod, sMessage: String);
begin
  Inherited RaiseError(aException, sMethod, StringFormat('%s: ''%s''', [sMessage, FStream.FileName]));
end;

procedure TAdvFile.Read(var aBuffer; iCount: Cardinal);
begin
  if FStream.Read(aBuffer, iCount) < iCount then
    RaiseError('Read', 'Unable to read past end of file');
end;

function TAdvFile.Readable: Int64;
begin
  result := FStream.Size - FStream.Position;
end;

procedure TAdvFile.SetPosition(const Value: Int64);
begin
  FStream.Position := value;
end;

procedure TAdvFile.SetSize(const iValue: Int64);
begin
  FStream.Size := iValue;
end;

procedure TAdvFile.Write(const aBuffer; iCount: Cardinal);
begin
  If (FStream.Write(aBuffer, iCount) < iCount) Then
    RaiseError('Read', 'Unable to write the entire buffer');
end;

function TAdvFile.Writeable: Int64;
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

Procedure TAfsEntity.Assign(oSource : TAdvObject);

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


Procedure TAfsList.DefaultCompare(Out aCompare: TAdvItemListCompare);
Begin { Procedure TAfsList.DefaultCompare }
  aCompare := CompareName;
End;  { Procedure TAfsList.DefaultCompare }


Function TAfsList.ItemClass : TAdvClass;
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

  FStreams := TAdvObjectMatch.Create;
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


Function TAfsStreamManager.Open(Const sName : String) : TAdvStream;
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


Procedure TAfsStreamManager.Close(oStream : TAdvStream);
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
  TAfsResourceFile = Class(TAdvMemoryStream)
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
    FItems : TAdvStringList;
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
    Property Items : TAdvStringList Read FItems;
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
  FItems := TAdvStringList.Create;
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


End. // FHIR.Support.Stream //
