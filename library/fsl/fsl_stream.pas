Unit fsl_stream;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

{$I fhir.inc}

Interface


Uses
  {$IFDEF WINDOWS} Windows, ActiveX, {$ENDIF}
  {$IFDEF LINUX} unixtype, baseunix, unix, {$ENDIF}
  {$IFNDEF FPC} AnsiStrings, {$ENDIF}
  SysUtils,Classes, RTLConsts, ZLib,
  IdHeaderList, IdGlobal, IdGlobalProtocols,
  fsl_fpc, fsl_base, fsl_collections, fsl_utilities;

type
  EParserException = class;

  { TSourceRange }

  TSourceRange = record
    lines : integer;
    cols : integer;
    function isNull : boolean;
    procedure incCols;
    procedure incLines;

    class function Create : TSourceRange; overload; static;
    class function Create(l, c : integer) : TSourceRange; overload; static;
    class function CreateNull : TSourceRange; static;
  end;

  { TSourceLocation }

  // TSourceLocation is zero based - both row and column
  // Location also acts as a
  TSourceLocation = record
    line : integer;
    col : integer;

    procedure incCol;
    procedure incLine;
    function checkChar(ch : char; last13 : boolean) : boolean;

    function nonZero : boolean;
    function isNull : boolean;
    function inSpan(lower, upper : TSourceLocation) : boolean;

    // for error messages - humans report in 1 based
    function lineForHuman : integer;
    function colForHuman : integer;
    function describe : String;
    function exception(msg : String) : EParserException;

    class function Create : TSourceLocation; overload; static;
    class function Create(l, c : integer) : TSourceLocation; overload; static;
    class function CreateNull : TSourceLocation; static;
    class function min(src1, src2 : TSourceLocation) : TSourceLocation; static;
    class function max(src1, src2 : TSourceLocation) : TSourceLocation; static;

    class operator add(position : TSourceLocation; range : TSourceRange) : TSourceLocation;
    class operator subtract(start, finish : TSourceLocation) : TSourceRange;
    class operator subtract(position : TSourceLocation; range : TSourceRange) : TSourceLocation;
    class operator equal(src1, src2 : TSourceLocation) : boolean;
    class operator notEqual(src1, src2 : TSourceLocation) : boolean;
    class operator lessThan(src1, src2 : TSourceLocation) : boolean;
    class operator greaterThan(src1, src2 : TSourceLocation) : boolean;
    class operator lessThanOrEqual(src1, src2 : TSourceLocation) : boolean;
    class operator greaterThanOrEqual(src1, src2 : TSourceLocation) : boolean;

      // SynEdit is one based. toPoint/fromPoint correct for this
    class function fromPoint(p : TPoint) : TSourceLocation; static;
    function toPoint : TPoint;
  end;

  { EParserException }

  EParserException = class (EFslException)
  private
    FLocation : TSourceLocation;
    function GetColNumber: integer;
    function GetLineNumber: integer;
  public
    constructor Create(msg : String; Location : TSourceLocation); // Line and Col are zero based
    Property Location : TSourceLocation read FLocation;
    Property LineNumber : integer read GetLineNumber;
    Property ColNumber : integer read GetColNumber;
  end;
  EJsonParserException = class (EParserException); // error reading or writing Json

  TSourceLocationObject = class (TFslObject)
  public
    locationStart : TSourceLocation;
    locationEnd : TSourceLocation;
  end;

type

  TFslStream = Class(TFslObject)
    Protected
      Function ErrorClass : EFslExceptionClass; Override;

    Public
      Function Link : TFslStream;

      Function Assignable : Boolean; Override;

      Procedure Read(Var Buffer; iCount : Integer); Virtual; // can't mark as overload
      Procedure Write(Const Buffer; iCount : Integer); Virtual; // can't mark as overload

      Function Readable : Int64; Virtual;
      Function Writeable : Int64; Virtual;
      Function percent : Integer; Virtual;
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

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Read(Var Buffer; iCount : Integer); Override;
      Procedure Write(Const Buffer; iCount : Integer); Override;

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;
      Function percent : Integer; override;

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
      Function percent : Integer; override;
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

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Read(Var Buffer; iCount : Integer); Override;
      Procedure Write(Const Buffer; iCount : Integer); Override;

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Property Stream : TFslAccessStream Read GetStream Write SetStream;
  End;

  TFslObjectClass = fsl_base.TFslObjectClass;

  TFslStringStream = Class(TFslAccessStream)
    Private
      FData : AnsiString;
      FIndex : Integer;

      Procedure SetData(Const Value: AnsiString);
      function GetBytes: TBytes;
      procedure SetBytes(const Value: TBytes);

    Protected
      Function GetPosition : Int64; Override;
      Procedure SetPosition(Const iValue : Int64); Override;

      Function GetSize : Int64; Override;
      Procedure SetSize(Const iValue : Int64); Override;

      function sizeInBytesV : cardinal; override;
    Public
      Procedure Read(Var aBuffer; iCount : Integer); Override;
      Procedure Write(Const aBuffer; iCount : Integer); Override;

      Function Readable : Int64; Override;
      Function Writeable : Int64; Override;

      Property Data : AnsiString Read FData Write SetData;
      Property Bytes : TBytes Read GetBytes Write SetBytes;
  End;

  { TFslFile }

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

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(const AFileName: string; Mode: Word); overload;
    destructor Destroy; override;

    function Link : TFslFile; overload;

    Procedure Read(Var aBuffer; iCount : Integer); Override;
    Procedure Write(Const aBuffer; iCount : Integer); Override;
    Function Readable : Int64; Override;
    Function Writeable : Int64; Override;

    property Handle: THandle read GetHandle;
  End;

    {
    A list of bytes
  }

  TFslBuffer = Class(TFslObject)
  Private
    FData : Pointer;
    FCapacity : Cardinal;
    FOwned : Boolean;
    FEncoding: TEncoding;
    FFormat: String;
    function GetAsUnicode: String;
    procedure SetAsUnicode(const Value: String);

    Procedure SetCapacity(Const Value : cardinal);
    Procedure SetData(Const Value : Pointer);
    Procedure SetOwned(Const Value : Boolean);

    Function GetAsText: AnsiString;
    Procedure SetAsText(Const Value: AnsiString);

    Function ExtractAscii(Const iLength : Integer) : AnsiString;
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const Value: TBytes);
    function GetHasFormat: boolean;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    constructor Create(bytes : TBytes); Overload;
{$IFNDEF UT}
    constructor Create(sText : String); Overload;
{$ENDIF}
    destructor Destroy; Override;

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
    Procedure CopyRange(oBuffer : TFslBuffer; Const iIndex, iLength : cardinal);
    Function Compare(oBuffer : TFslBuffer) : Integer;

    Procedure Move(Const iSource, iTarget, iLength : cardinal);

    Function Offset(iIndex : cardinal) : Pointer;

    Procedure LoadFromFile(oFile : TFslFile);
    Procedure SaveToFile(oFile : TFslFile);
    Procedure LoadFromStream(oStream : TFslStream); overload;
    Procedure SaveToStream(oStream : TFslStream); overload;
    Procedure LoadFromStream(oStream : TStream); overload;
    Procedure SaveToStream(oStream : TStream); overload;

    Property Data : Pointer Read FData Write SetData;
    Property Capacity : Cardinal Read FCapacity Write SetCapacity;
    Property Owned : Boolean Read FOwned Write SetOwned;
    Property AsText : String Read GetAsUnicode Write SetAsUnicode;
    Property Encoding : TEncoding read FEncoding write FEncoding;
    Property AsBytes : TBytes read GetAsBytes write SetAsBytes;
    Property AsAscii : AnsiString Read GetAsText Write SetAsText;
    Property Size : Cardinal Read FCapacity Write SetCapacity;
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

  function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    constructor Create(cnt : TBytes); Overload;

    destructor Destroy; Override;

    Function Clone : TFslMemoryStream;
    Function Link : TFslMemoryStream;

    Procedure Assign(oObject : TFslObject); Override;

    Procedure Read(Var aBuffer; iSize : Integer); Override;
    Procedure Write(Const aBuffer; iSize : Integer); Override;

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

  TFslVCLStream = Class(TFslStream)
  Private
    FStream : TStream;

    Function GetStream: TStream;
    Procedure SetStream(Const Value: TStream);

  Public
    Procedure Read(Var aBuffer; iCount : Integer); Override;
    Procedure Write(Const aBuffer; iCount : Integer); Override;

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
      constructor Create; Overload; Virtual;
      constructor Create(Stream : TFslStream); Overload; Virtual;
      destructor Destroy; Override;

      Function Read(Var aBuffer; iCount: LongInt): LongInt; Override;
      Function Write(Const aBuffer; iCount: LongInt): LongInt; Override;
      Function Seek(iOffset: LongInt; iOrigin: Word): LongInt; Override;

      Property Stream : TFslStream Read GetStream Write SetStream;
  End;

  TStream = Classes.TStream;

const
  MAP_ATTR_NAME = 'B88BF977DA9543B8A5915C84A70F03F7';


Type
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

  protected
    function sizeInBytesV : cardinal; override;
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


{$IFDEF WINDOWS}
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
    constructor Create(oVolume : TAfsVolume); Overload; Virtual;
    destructor Destroy; Override;

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
    Procedure Read(oHandle : TAfsHandle; Var Buffer; iCount : Integer); Virtual; Abstract;
    Procedure Write(oHandle : TAfsHandle; Const Buffer; iCount : Integer); Virtual; Abstract;
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
    constructor Create(oVolume : TAfsVolume; oHandle : TAfsHandle); Overload;
    destructor Destroy; Override;

    Procedure Read(Var Buffer; iCount : Integer); Override;
    Procedure Write(Const Buffer; iCount : Integer); Override;

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
    constructor Create; Overload; Override;
    destructor Destroy; Override;
    constructor Create(oVolume : TAfsVolume; Const sName : String = ''); Overload;

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
    constructor Create; Override;
    destructor Destroy; Override;
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
    constructor Create; Override;
    destructor Destroy; Override;

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
    Procedure Read(oHandle : TAfsHandle; Var Buffer; iCount : Integer); Override;
    Procedure Write(oHandle : TAfsHandle; Const Buffer; iCount : Integer); Override;
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
      constructor Create(Const sName : String); Overload; Virtual;
      constructor Create; Overload; Override;
  End; { TAfsResourceManager }

{$ENDIF}

Type
  TCharArray = SysUtils.TCharArray;

  TFslTextReader = class (TFslObject)
  public
    procedure Close; virtual; abstract;
    function Peek: Integer; virtual; abstract;
    function Read: Integer; overload; virtual; abstract;
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; virtual; abstract;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; virtual; abstract;
    function ReadLine: string; virtual; abstract;
    function ReadToEnd: string; virtual; abstract;

    Function ReadString(Var s : String; iLength : Integer) : Integer;
  end;

  TFslStringReader = class(TFslTextReader)
  private
    FContent : String;
    FCursor : integer;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(content : String);
    function Peek: Integer; override;
    function Read: Integer; overload; override;
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; override;
    function ReadLine: string; override;
    function ReadToEnd: string; override;
    procedure Close; override;
  end;

  TFslStreamReader = class(TFslTextReader)
  private
    FBufferedData: TCharArray;
    FBufferStart : Integer;
    FBufferEnd : Integer;
    FBufferSize: Integer;
    FDetectBOM: Boolean;
    FNoDataInStream: Boolean;
    FSkipPreamble: Boolean;
    FStream: TFslStream;
    FCursor : Integer;
    FEncoding: TEncoding;
    FCheckEncoding : boolean;
    FClosed : boolean;
    function DoDetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
    function SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
    procedure FillBuffer(var Encoding: TEncoding);
    function GetEndOfStream: Boolean;
  protected
    Property Stream : TFslStream read FStream;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aStream: TFslStream); overload;
    constructor Create(aStream: TFslStream; DetectBOM: Boolean); overload;
    constructor Create(const Filename: string); overload;
    constructor Create(const Filename: string; DetectBOM: Boolean); overload;
    constructor Create(aStream: TFslStream; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0); overload;
    constructor Create(const Filename: string; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure DiscardBufferedData;
//    procedure OwnStream; inline;
    function Peek: Integer; override;
    function Read: Integer; overload; override;
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; override;
    function ReadLine: string; override;
    function ReadToEnd: string; override;
    property BaseStream: TFslStream read FStream;
    property CurrentEncoding: TEncoding read FEncoding;
    property EndOfStream: Boolean read GetEndOfStream;
    function percent : integer;
  end;

  TFslFormatter = Class(TFslStreamAdapter)
    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

      Procedure SetStream(oStream : TFslStream); Override;

    Public
      Procedure Clear; Overload; Virtual;

      Procedure ProduceBytes(Const aBytes : TBytes); Overload; Virtual;
      Procedure Produce(Const sText: String); Overload; Virtual;
  End;

  EFslExtractor = Class(EFslException);
  TFslExtractor = Class(TFslStreamAdapter)
    Protected

      Function ErrorClass : EFslExceptionClass; Overload; Override;

      Procedure SetStream(oStream : TFslStream); Override;

    Public
      Procedure Clear; Virtual;

      Function More : Boolean; Virtual;
  End;

  { TFslTextFormatter }

  TFslTextFormatter = Class(TFslFormatter)
    Private
      FLocation : TSourceLocation;
      FLevel : Integer;
      FHasWhitespace : Boolean;
      FWhitespaceCharacter : Char;
      FWhitespaceMultiple : Integer;
      {$IFNDEF VER130}
      FEncoding: TEncoding;
      {$ENDIF}

    Protected
      Function BeforeWhitespace : String;
      Function AfterWhitespace : String;

      function AdjustLocation(location: TSourceLocation; const sText: String): TSourceLocation;
    Public
      constructor Create; Override;

      Function Link : TFslTextFormatter;

      Procedure Clear; Override;

      Procedure ProduceBytes(Const aBytes : TBytes); Overload; override;
      Procedure Produce(Const sText: String); Overload; override;

      Procedure ProduceNewLine; Virtual;
      Procedure ProduceLine(Const sValue : String);
      Procedure ProduceInline(Const sValue : String);

      Procedure ProduceFragment(Const sValue : String);

      Procedure LevelDown;
      Procedure LevelUp;

      Property HasWhitespace : Boolean Read FHasWhitespace Write FHasWhitespace;
      Property WhitespaceCharacter : Char Read FWhitespaceCharacter Write FWhitespaceCharacter;
      Property WhitespaceMultiple : Integer Read FWhitespaceMultiple Write FWhitespaceMultiple;
      Property Level : Integer Read FLevel;
      {$IFNDEF VER130}
      Property Encoding : TEncoding read FEncoding Write FEncoding;
      {$ENDIF}
      property Location  : TSourceLocation read FLocation;
  End;

  TFslTextFormatterClass = Class Of TFslTextFormatter;

  TFslTextExtractor = Class(TFslStreamReader)
    Private
      FLine : Integer;
      FCache : String;

      FBuilder : TFslStringBuilder;

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

      Procedure CacheAdd(Const sValue : String); Virtual;
      Procedure CacheRemove(Const sValue : String); Virtual;

      Procedure RaiseError(Const sMethod, sMessage : String); Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Override;

      Function More : Boolean; Virtual;

      Function ConsumeLine : String;

      Procedure ProduceString(Const sToken : String);
      Procedure ProduceCharacter(Const cToken : Char);

      Function ConsumeCharacter : Char; Overload; Virtual;
      Function ConsumeString(Const sToken : String) : String;
      Function ConsumeCharacter(Const cToken : Char) : Char; Overload;
      Function ConsumeCharacterCount(Const iCharacterCount : Integer) : String;

      Function ConsumeWhileCharacter(Const cToken : Char) : String;
      Function ConsumeWhileCharacterSet(Const aTokenSet : TCharSet) : String; Overload;
      Function ConsumeWhileCharacterSet(Const oCharacterSet : TFslCharacterSet) : String; Overload;

      Function ConsumeUntilCharacter(Const cToken : Char) : String;
      Function ConsumeUntilCharacterSet(Const aTokenSet : TCharSet) : String;
      Function ConsumeUntilString(Const sToken : String) : String; Overload;
      Function ConsumeUntilString(Const aStringArray : Array Of String) : String; Overload;

      Function ConsumeRestStream : String;

      Function MatchString(Const sToken : String) : Boolean;
      Function MatchStringArray(Const aTokenSet : Array Of String) : Integer;

      Function NextCharacter : Char;

      Function CacheLength : Integer;
      Function StreamPosition : Int64;

      Property Line : Integer Read FLine Write FLine;
  End;

  EFslTextExtractor = Class(EFslExtractor);


  TFslCSVExtractor = Class(TFslTextExtractor)
  Private
    FSeparator : Char;
    FQuote : Char;
    FHasQuote : Boolean;
    FIgnoreWhitespace: boolean;
  Public
    constructor Create; Override;

    Procedure ConsumeEntries(oEntries : TFslStringList); Overload;
    Procedure ConsumeEntries; Overload;
    Function ConsumeEntry : String;
    Function MoreEntries : Boolean;

    Property Separator : Char Read FSeparator Write FSeparator;
    Property Quote : Char Read FQuote Write FQuote;
    Property HasQuote : Boolean Read FHasQuote Write FHasQuote;
    Property IgnoreWhitespace : boolean read FIgnoreWhitespace write FIgnoreWhitespace;
  End;

  TFslCSVFormatter = Class(TFslTextFormatter)
  Private
    FSeparator : Char;
    FQuote : Char;
    FHasQuote : Boolean;
    FEmptyLine : Boolean;

  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Procedure Clear; Override;

    Procedure ProduceEntryStringArray(Const aEntryStringArray : Array Of String);
    Procedure ProduceEntryStringList(oEntryStringList : TFslStringList);
    Procedure ProduceEntry(Const sEntry : String);
    Procedure ProduceSeparator;

    Procedure ProduceNewLine; Override;

    Property Separator : Char Read FSeparator Write FSeparator;
    Property Quote : Char Read FQuote Write FQuote;
    Property HasQuote : Boolean Read FHasQuote Write FHasQuote;
  End;

  TCSVWriter = class (TFslCSVFormatter)
  private
  public
    procedure cell(s : String); overload;
    procedure cell(b : boolean); overload;
    procedure cell(i : integer); overload;
    procedure line;
  end;

{$IFNDEF FPC}
  TGetCsvValues = reference to Procedure (csv : TCSVWriter);

procedure produceCsv(filename : String; headers : Array of String; values : TGetCsvValues);
{$ENDIF}



function StringToUTF8Stream(value : String) : TStream;
function UTF8StreamToString(value : TStream) : String; overload;
function UTF8StreamToString(value : TFslAccessStream) : String; overload;

function FileToString(filename : String; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
function StreamToString(stream : TStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String; overload;
function StreamToString(stream : TFslStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String; overload;
procedure StringToFile(content, filename : String; encoding : TEncoding);
procedure StringToStream(content: String; stream : TStream; encoding : TEncoding); overload;
procedure StringToStream(content: String; stream : TFslStream; encoding : TEncoding); overload;

procedure BytesToFile(bytes : TBytes; filename : String); overload;
procedure BytesToFile(bytes : TBytes; start, length : integer; filename : String); overload;
function FileToBytes(filename : String; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : TBytes;

procedure StreamToFile(stream : TStream; filename : String);


Const
  SIG_LOCAL_FILE_HEADER = $04034B50;
  SIG_DATA_DESCRIPTOR = $08074B50;
  SIG_CENTRAL_DIRECTORY_HEADER = $02014B50;
  SIG_DIGITAL_SIGNATURE = $05054B50;
  SEG_TERMINATION = $06054b50;

  METHOD_NONE = 0;
  METHOD_DEFLATE = 8;

Type
  TZipFlag = (
     flagEncrypted,           // Bit 0: If set, indicates that the file is encrypted.

     flagImploding1,
     flagImploding2,

     {     (For Method 6 - Imploding)
          Bit 1: If the compression method used was type 6,
                 Imploding, then this bit, if set, indicates
                 an 8K sliding dictionary was used.  If clear,
                 then a 4K sliding dictionary was used.
          Bit 2: If the compression method used was type 6,
                 Imploding, then this bit, if set, indicates
                 3 Shannon-Fano trees were used to encode the
                 sliding dictionary output.  If clear, then 2
                 Shannon-Fano trees were used.

          (For Methods 8 and 9 - Deflating)
          Bit 2  Bit 1
            0      0    Normal (-en) compression option was used.
            0      1    Maximum (-exx/-ex) compression option was used.
            1      0    Fast (-ef) compression option was used.
            1      1    Super Fast (-es) compression option was used.

          Note:  Bits 1 and 2 are undefined if the compression
                 method is any other.
         }
     flagUsesDataDescriptor,
{          Bit 3: If this bit is set, the fields crc-32, compressed
                 size and uncompressed size are set to zero in the
                 local header.  The correct values are put in the
                 data descriptor immediately following the compressed
                 data.  (Note: PKZIP version 2.04g for DOS only
                 recognizes this bit for method 8 compression, newer
                 versions of PKZIP recognize this bit for any
                 compression method.)                        }
     flagEnhancedDeflate,
                           {
          Bit 4: Reserved for use with method 8, for enhanced
                 deflating. }
     flagCompressPatched
     {
          Bit 5: If this bit is set, this indicates that the file is
                 compressed patched data.  (Note: Requires PKZIP
                 version 2.70 or greater)
      }
  );

Function Bit(iFlags : Word; aFlag : TZipFlag) : Boolean;

Function TimeAndDateToDateTime(iDate, iTime : Word) : TDateTime;
Procedure DateTimeToTimeAndDate(aValue : TDateTime; Out iDate, iTime : Word);

Function GetCRC(bytes : TBytes) : LongWord;

type
    TFslZipPart = Class(TFslNameBuffer)
    Private
      FTimestamp: TDateTime;
      FComment : String;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TFslZipPart;
      Function Clone : TFslZipPart;

      Procedure Assign(oObject : TFslObject); Override;

      Property Timestamp : TDateTime Read FTimestamp Write FTimestamp;
      Property Comment : String Read FComment Write FComment;
  End;

  TFslZipPartList = Class(TFslNameBufferList)
    Private
      Function GetPart(iIndex : Integer) : TFslZipPart;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Link : TFslZipPartList;
      Function Clone : TFslZipPartList;

      Function GetByName(Const sName : String) : TFslZipPart;

      Property Part[iIndex : Integer] : TFslZipPart Read GetPart; Default;

      procedure add(name : String; bytes : TBytes); overload;
  End;


  TFslZipWorker = Class (TFslObject)
    Private
      FStream : TFslStream;
      FParts : TFslZipPartList;
      Function GetStream : TFslStream;
      Function GetParts : TFslZipPartList;
      Procedure SetStream(oValue : TFslStream);
      Procedure SetParts(oValue : TFslZipPartList);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function HasStream : Boolean;
      Function HasParts : Boolean;

      Property Stream : TFslStream Read GetStream Write SetStream;
      Property Parts : TFslZipPartList Read GetParts Write SetParts;
  End;

  TFslZipReader = Class (TFslZipWorker)
    Private
      Function EndCondition(iLongWord : LongWord) : Boolean;
      Function ReadLongWord : LongWord;
      Function ReadWord : Word;
      Function ReadByte : Byte;
      Procedure ReadPart;
      Procedure Skip(iCount : Integer);
      Function ReadString(iLength : Word):AnsiString;
      Procedure ReadData(partName : string; iFlags, iComp : Word; iSizeComp, iSizeUncomp : LongWord; oBuffer : TFslBuffer);
      Procedure ReadDeflate(iFlags : Word; partName : string; iSizeComp, iSizeUncomp : LongWord; oBuffer: TFslBuffer);
      Procedure ReadUncompressed(iSizeComp : LongWord; oBuffer: TFslBuffer);
      Procedure ReadUnknownLengthDeflate(partName : string; oBuffer : TFslBuffer);
      Procedure ReadKnownDeflate(pIn : Pointer; partName : string; iSizeComp, iSizeDecomp : LongWord; oBuffer : TFslBuffer);
      Procedure ReadDirectory(iCount : Integer);
      Procedure ReadDigSig;
      Procedure ReadTermination;
    Public
      Procedure ReadZip; Overload; Virtual;
  End;

  Type
  TFslZippedData = Class (TFslObject)
  Private
    FOffset : Integer;
    FCrc : LongWord;
    FCompressedSized : LongWord;
    FDate : Word;
    FTime : Word;
  End;

  TFslZipWriter = Class (TFslZipWorker)
  Private
    FPartInfo : TFslObjectMatch;
    FOffset : Integer;
    FDirOffset : Integer;
    Procedure WriteLongWord(iValue : LongWord);
    Procedure WriteWord(iValue : Word);
    Procedure WriteString(Const sValue : AnsiString);

    Procedure Compress(oSource, oDestination : TFslBuffer);

    Procedure WritePart(oPart : TFslZipPart);
    Procedure WriteDirectory(oPart : TFslZipPart);
    Procedure WriteEnd(iCount : Integer);
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Procedure WriteZip;

    procedure addFile(name, actual : String);
  End;

(*
copied from FreePascal libtar

===============================================================================================
TTarArchive Usage
-----------------
- Choose a constructor
- Make an instance of TTarArchive                  TA := TTarArchive.Create (Filename);
- Scan through the archive                         TA.Reset;
                                                   WHILE TA.FindNext (DirRec) DO BEGIN
- Evaluate the DirRec for each file                  ListBox.Items.Add (DirRec.Name);
- read out the current file                          TA.ReadFile (DestFilename);
  (You can ommit this if you want to
  read in the directory only)                        end;
- You're done                                      TA.Free;
TTarWriter Usage
----------------
- Choose a constructor
- Make an instance of TTarWriter                   TW := TTarWriter.Create ('my.tar');
- Add a file to the tar archive                    TW.AddFile ('foobar.txt');
- Add a String as a file                           TW.AddString (SL.Text, 'joe.txt', Now);
- Destroy TarWriter instance                       TW.Free;
- Now your tar file is ready.
Source
--------------------------
The official site to get this code is http://www.destructor.de/
Donateware
----------
If you like this code, you are free to donate
http://www.destructor.de/donateware.htm
===============================================================================================
!!!  All parts of this code which are not finished or known to be buggy
     are marked with three exclamation marks
===============================================================================================
*)

type
  // --- File Access Permissions
  TTarPermission  = (tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                     tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                     tpReadByOther, tpWriteByOther, tpExecuteByOther);
  TTarPermissions = set of TTarPermission;

  // --- Type of File
  TFileType = (ftNormal,          // Regular file
               ftLink,            // Link to another, previously archived, file (LinkName)
               ftSymbolicLink,    // Symbolic link to another file              (LinkName)
               ftCharacter,       // Character special files
               ftBlock,           // Block special files
               ftDirectory,       // Directory entry. Size is zero (unlimited) or max. number of bytes
               ftFifo,            // FIFO special file. No data stored in the archive.
               ftContiguous,      // Contiguous file, if supported by OS
               ftDumpDir,         // List of files
               ftMultiVolume,     // Multi-volume file part
               ftVolumeHeader);   // Volume header. Can appear only as first record in the archive

  // --- Mode
  TTarMode  = (tmSetUid, tmSetGid, tmSaveText);
  TTarModes = set of TTarMode;

  // --- Record for a Directory Entry
  //     Adjust the ClearDirRec procedure when this record changes!
  TTarDirRec  = record
    Name        : AnsiString;            // File path and name
    Size        : int64;             // File size in Bytes
    DateTime    : TDateTime;         // Last modification date and time
    Permissions : TTarPermissions;   // Access permissions
    FileType    : TFileType;         // Type of file
    LinkName    : AnsiString;            // Name of linked file (for ftLink, ftSymbolicLink)
    UID         : integer;           // User ID
    GID         : integer;           // Group ID
    UserName    : AnsiString;            // User name
    GroupName   : AnsiString;            // Group name
    ChecksumOK  : boolean;           // Checksum was OK
    Mode        : TTarModes;         // Mode
    Magic       : AnsiString;            // Contents of the "Magic" field
    MajorDevNo  : integer;           // Major Device No. for ftCharacter and ftBlock
    MinorDevNo  : integer;           // Minor Device No. for ftCharacter and ftBlock
//    FilePos     : int64;             // Position in TAR file
  end;

  // --- The TAR Archive CLASS
  TTarArchive = class (TFslObject)
  protected
    FStream     : TStream;   // Internal Stream
    FOwnsStream : boolean;   // True if FStream is owned by the TTarArchive instance
    FBytesToGo  : int64;     // Bytes until the next Header Record
  public
    constructor Create (Stream   : TStream); overload;
    constructor Create (Filename : String; FileMode : WORD = fmOpenRead or fmShareDenyWrite);  overload;
    destructor Destroy; override;
    procedure Reset;                                         // Reset File Pointer
    function  FindNext (var DirRec : TTarDirRec) : boolean;  // Reads next Directory Info Record. FALSE if EOF reached
    procedure ReadFile (Buffer   : pointer); overload;       // Reads file data for last Directory Record
    procedure ReadFile (Stream   : TStream); overload;       // -;-
    procedure ReadFile (Filename : String);  overload;       // -;-
    function  ReadFile : TBytes; overload;         // -;-  RawByteString in D2009+. Not active due to FPC unicode architecture not being finalized
    procedure GetFilePos (var Current, Size : int64);        // Current File Position
    procedure SetFilePos (NewPos : int64);                   // Set new Current File Position
  end;

  // --- The TAR Archive Writer CLASS
  TTarWriter = class (TFslObject)
  protected
    FStream      : TStream;
    FOwnsStream  : boolean;
    FFinalized   : boolean;
                                   // --- Used at the next "Add" method call: ---
    FPermissions : TTarPermissions;   // Access permissions
    FUID         : integer;           // User ID
    FGID         : integer;           // Group ID
    FUserName    : AnsiString;            // User name
    FGroupName   : AnsiString;            // Group name
    FMode        : TTarModes;         // Mode
    FMagic       : AnsiString;            // Contents of the "Magic" field
  PUBLIC
    constructor CreateEmpty;
    constructor Create (TargetStream   : TStream); overload;
    constructor Create (TargetFilename : String; Mode : integer = fmCreate); overload;
    destructor Destroy; override;                   // Writes End-Of-File Tag
    procedure AddFile   (Filename : AnsiString;  TarFilename : AnsiString = '');
    procedure AddStream (Stream   : TStream; TarFilename : AnsiString; FileDateGmt : TDateTime);
    procedure AddString (Contents : Ansistring;  TarFilename : AnsiString; FileDateGmt : TDateTime);  // RawByteString
    procedure AddDir          (Dirname            : AnsiString; DateGmt : TDateTime; MaxDirSize : int64 = 0);
    procedure AddSymbolicLink (Filename, Linkname : AnsiString; DateGmt : TDateTime);
    procedure AddLink         (Filename, Linkname : AnsiString; DateGmt : TDateTime);
    procedure AddVolumeHeader (VolumeId           : AnsiString; DateGmt : TDateTime);
    procedure Finalize;
    property Permissions : TTarPermissions read FPermissions write FPermissions;   // Access permissions
    property UID         : integer         read FUID         write FUID;           // User ID
    property GID         : integer         read FGID         write FGID;           // Group ID
    property UserName    : AnsiString          read FUserName    write FUserName;      // User name
    property GroupName   : AnsiString          read FGroupName   write FGroupName;     // Group name
    property Mode        : TTarModes       read FMode        write FMode;          // Mode
    property Magic       : AnsiString          read FMagic       write FMagic;         // Contents of the "Magic" field
    end;

// --- Some useful constants
CONST
  FILETYPE_NAME : ARRAY [TFileType] OF String =
                  ('Regular', 'Link', 'Symbolic Link', 'Char File', 'Block File',
                   'Directory', 'FIFO File', 'Contiguous', 'Dir Dump', 'Multivol', 'Volume Header');

  ALL_PERMISSIONS     = [tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                         tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                         tpReadByOther, tpWriteByOther, tpExecuteByOther];
  READ_PERMISSIONS    = [tpReadByOwner, tpReadByGroup,  tpReadByOther];
  WRITE_PERMISSIONS   = [tpWriteByOwner, tpWriteByGroup, tpWriteByOther];
  EXECUTE_PERMISSIONS = [tpExecuteByOwner, tpExecuteByGroup, tpExecuteByOther];


function  PermissionString      (Permissions : TTarPermissions) : String;
function  ConvertFilename       (Filename    : AnsiString)      : AnsiString;
function  FileTimeGMT           (FileName    : AnsiString)      : TDateTime;  overload;
function  FileTimeGMT           (SearchRec   : TSearchRec)      : TDateTime;  overload;
procedure ClearDirRec           (var DirRec  : TTarDirRec);


type
  TMimeBase = class (TFslObject)
  private
    FHeaders: TIdHeaderList;
    procedure ReadHeaders(AStream : TStream);
    procedure WriteHeaders(AStream : TStream);
    function ReadBytes(AStream: TStream; AByteCount: Integer): AnsiString;
    function ReadToValue(AStream: TStream; AValue: AnsiString): AnsiString;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Headers : TIdHeaderList read FHeaders;
  end;

  TMimePart = class (TMimeBase)
  private
    FContent: TFslBuffer;
    FId : string;
    procedure SetContent(const AValue: TFslBuffer);
    procedure DecodeContent;
    procedure ReadFromStream(AStream : TStream; ABoundary : AnsiString);
    procedure WriteToStream(AStream : TStream);
    function GetMediaType: String;
    function GetTransferEncoding: String;
    procedure SetMediaType(const AValue: String);
    procedure SetTransferEncoding(const AValue: String);
    procedure SetId(const AValue: String);
    function GetContentDisposition: String;
    procedure SetContentDisposition(const sValue: String);
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TMimePart; overload;

    property Content : TFslBuffer read FContent write SetContent;
    property Id : String read FId write SetId;
    property MediaType : String read GetMediaType write SetMediaType;
    property ContentDisposition : String read GetContentDisposition write SetContentDisposition;
    property TransferEncoding : String read GetTransferEncoding write SetTransferEncoding;

    function ParamName : String;
    function FileName : String;
  end;

  TMimeMessage = class (TMimeBase)
  private
    FParts: TFslList<TMimePart>;
    FBoundary : Ansistring;
    FStart : String;
    FMainType : String;
    procedure AnalyseContentType(AContent : String);
    function GetMainPart: TMimePart;
    procedure Validate;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TMimeMessage; overload;
    property Parts : TFslList<TMimePart> read FParts;
    function AddPart(id: String) : TMimePart;
    property MainPart : TMimePart read GetMainPart;
    property Boundary : ansistring read FBoundary write FBoundary;
    property Start : String read FStart write FStart;
    property MainType : String read FMainType write FMainType;

    // for multi-part forms
    function getparam(name : String) : TMimePart;
    function hasParam(name : String) : Boolean;

    procedure ReadFromStream(AStream : TStream); overload;
    procedure ReadFromStream(AStream : TStream; AContentType : String); overload; // headers are not part of the stream

    function GetContentTypeHeader : String;
    procedure WriteToStream(AStream : TStream; AHeaders : boolean);
  end;


var
  Eolns : TBytes;

Type
  TFslByteExtractor = Class(TFslExtractor)
  Private
    FCache : AnsiString;
    FBuilder : TFslBytesBuilder;
    Function CacheLength : Integer;
  Public
    Constructor Create; Overload; Override;
    Destructor Destroy; Override;

    Procedure Clear; Override;

    Function More : Boolean; Override;

    Function ConsumeByte : TByte; Overload; Virtual;
    Function ConsumeByteCount(Const iByteCount : Integer) : TBytes;
    Function ConsumeWhileByte(Const iToken : TByte) : TBytes;
    Function ConsumeWhileBytes(Const iTokens : TBytes) : TBytes;
    Function ConsumeUntilByte(Const iToken : TByte) : TBytes;
    Function ConsumeUntilBytes(Const iTokens : TBytes) : TBytes;
    Function ConsumeLine : TBytes;
    Function ConsumeRestStream : TBytes;

    Function NextByte : TByte;

    Function StreamPosition : Int64;
  End;


Implementation

{ TSourceRange }

function TSourceRange.isNull: boolean;
begin
  result := (lines = -1) and (cols = -1);
end;

procedure TSourceRange.incCols;
begin
  inc(cols);
end;

procedure TSourceRange.incLines;
begin
  inc(lines);
  cols := 0;
end;

class function TSourceRange.Create: TSourceRange;
begin
  result := Create(0, 0);
end;

class function TSourceRange.Create(l, c: integer): TSourceRange;
begin
  result.lines := l;
  result.cols := c;
end;

class function TSourceRange.CreateNull: TSourceRange;
begin
  result := Create(-1, -1);
end;

Function TFslStream.Link : TFslStream;
Begin
  Result := TFslStream(Inherited Link);
End;


function TFslStream.percent: Integer;
begin
  result := 0;
end;

Function TFslStream.ErrorClass : EFslExceptionClass;
Begin
  Result := EIOException;
End;


Procedure TFslStream.Read(Var Buffer; iCount : Integer);
Begin
End;


Procedure TFslStream.Write(Const Buffer; iCount : Integer);
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


function TFslStreamAdapter.percent: Integer;
begin
  result := Stream.percent;
end;

Procedure TFslStreamAdapter.Read(Var Buffer; iCount : Integer);
Begin 
  Stream.Read(Buffer, iCount);
End;


Procedure TFslStreamAdapter.Write(Const Buffer; iCount : Integer);
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


function TFslStreamAdapter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStream.sizeInBytes);
end;

Function TFslAccessStream.Link : TFslAccessStream;
Begin 
  Result := TFslAccessStream(Inherited Link);
End;


function TFslAccessStream.percent: Integer;
begin
  result := trunc(position / size * 100);
end;

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


Procedure TFslAccessStreamAdapter.Read(Var Buffer; iCount : Integer);
Begin 
  Stream.Read(Buffer, iCount);
End;  


Procedure TFslAccessStreamAdapter.Write(Const Buffer; iCount : Integer);
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


function TFslAccessStreamAdapter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStream.sizeInBytes);
end;

Procedure TFslStringStream.Read(Var aBuffer; iCount : Integer);
Begin
  If FIndex + iCount > Size Then
    RaiseError('Read', 'Unable to read past end of string.');

  Move((PAnsiChar(FData) + FIndex)^, aBuffer, iCount);
  Inc(FIndex, iCount);
End;


Procedure TFslStringStream.Write(Const aBuffer; iCount : Integer);
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
  If FIndex > Length(FData) Then
    FIndex := Length(FData);
End;


procedure TFslStringStream.SetBytes(const Value: TBytes);
begin
  FData := BytesAsAnsiString(value);
end;

Procedure TFslStringStream.SetData(Const Value: AnsiString);
Begin
  FData := Value;
  If FIndex > Length(FData) Then
    FIndex := Length(FData)
  Else
    FIndex := 0;
End;



function TFslStringStream.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, length(FData) + 12);
end;

Procedure TFslVCLStream.Read(Var aBuffer; iCount : Integer);
Begin
  Stream.Read(aBuffer, iCount);
End;


Procedure TFslVCLStream.Write(Const aBuffer; iCount : Integer);
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


Function TVCLStream.Read(Var aBuffer; iCount: LongInt): LongInt;
Var
  iReadable : Integer;
Begin
  iReadable := Stream.Readable;
  If iReadable > iCount Then
    iReadable := iCount;

  Stream.Read(aBuffer, iReadable);
  Result := iReadable;
End;


Function TVCLStream.Seek(iOffset: LongInt; iOrigin: Word): LongInt;
Var
  oAccess : TFslAccessStream;
Begin
  If Not (Stream Is TFslAccessStream) Then
    Raise EIOException.Create(Self, 'Seek', 'Unable to seek in a non-access stream'); // Error is not available.

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


Procedure TVCLStream.SetSize(NewSize: LongInt);
Var
  oAccess : TFslAccessStream;
Begin
  If Not (Stream Is TFslAccessStream) Then
    Raise EIOException.Create(Self, 'SetSize', 'Unable to set the size of a non-access stream'); // Error is not available.

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


Function TVCLStream.Write(Const aBuffer; iCount: LongInt): LongInt;
Begin
  FStream.Write(aBuffer, iCount);
  Result := iCount;
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
   assert(oStream.Position = i + integer(Capacity));
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


Procedure TFslBuffer.SetCapacity(Const Value: cardinal);
Begin 
  If (Value <> Capacity) Then
  Begin
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
begin
  result := FEncoding.GetString(AsBytes);
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


Procedure TFslBuffer.CopyRange(oBuffer: TFslBuffer; Const iIndex, iLength : cardinal);
Begin
  Assert(Invariants('CopyRange', oBuffer, TFslBuffer, 'oBuffer'));
  Assert(CheckCondition((iIndex + iLength <= oBuffer.Capacity), 'CopyRange', 'Attempted to copy invalid part of the buffer.'));

  SetCapacity(iLength);

  MemoryMove(oBuffer.Offset(iIndex), Data, iLength);
End;


Procedure TFslBuffer.Move(Const iSource, iTarget, iLength : cardinal);
Begin
  Assert(CheckCondition((iSource + iLength <= Capacity), 'Copy', 'Attempted to move from an invalid part of the buffer.'));
  Assert(CheckCondition((iTarget + iLength <= Capacity), 'Copy', 'Attempted to move to an invalid part of the buffer.'));

  MemoryMove(Offset(iSource), Offset(iTarget), iLength);
End;


Function TFslBuffer.Offset(iIndex: cardinal): Pointer;
Begin 
  Assert(CheckCondition((iIndex <= Capacity), 'Offset', 'Attempted to access invalid offset in the buffer.'));

  Result := Pointer(NativeUInt(Data) + NativeUInt(iIndex));
End;


function TFslBuffer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCapacity);
  inc(result, sizeof(FEncoding));
  inc(result, (FFormat.length * sizeof(char)) + 12);
end;

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

{$IFNDEF UT}
constructor TFslBuffer.Create(sText: String);
begin
  Create;
  AsText := stext;
end;

{$ENDIF}

constructor TFslBuffer.Create(bytes: TBytes);
begin
  Create;
  Asbytes := bytes;
end;

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


function TFslNameBuffer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

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


constructor TFslMemoryStream.Create(cnt: TBytes);
begin
  Create;
  Buffer.AsBytes := cnt;
  Size := Buffer.Size;
  Position := 0;
end;

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
  FCurrentPointer := Pointer(NativeUInt(FBuffer.Data) + NativeUInt(FPosition));
End;  


Procedure TFslMemoryStream.Read(Var aBuffer; iSize : Integer);
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


Procedure TFslMemoryStream.Write(Const aBuffer; iSize : Integer);
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
  Result := EIOException;
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


function TFslMemoryStream.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBuffer.sizeInBytes);
end;

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
  Result := EIOException;
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

procedure TFslFile.Read(var aBuffer; iCount: Integer);
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

procedure TFslFile.Write(const aBuffer; iCount: Integer);
begin
  If (FStream.Write(aBuffer, iCount) < iCount) Then
    RaiseError('Read', 'Unable to write the entire buffer');
end;

function TFslFile.Writeable: Int64;
begin
  result := 0; // ?
end;

function TFslFile.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, sizeof(FStream));
end;

{$IFDEF WINDOWS}

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

Procedure TAfsStream.Read(Var Buffer; iCount : Integer);

Begin { Procedure TAfsStream.Read }
  FVolume.Read(FHandle, Buffer, iCount);
End;  { Procedure TAfsStream.Read }

Procedure TAfsStream.Write(Const Buffer; iCount : Integer);

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
    Procedure Write(Const Buffer; iSize : Integer); Override;

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
    constructor Create; Override;
    destructor Destroy; Override;

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
    RaiseError('Open', StringFormat('Requested access mode denied on "%s"', [sName]));

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
            RaiseError('Open', StringFormat('File not found "%s"', [sName]));

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


Procedure TAfsResourceVolume.Read(oHandle : TAfsHandle; Var Buffer; iCount : Integer);
Begin { Procedure TAfsResourceVolume.Read }
  TAfsResourceFile(oHandle).Read(Buffer, iCount);
End;  { Procedure TAfsResourceVolume.Read }


Procedure TAfsResourceVolume.Write(oHandle : TAfsHandle; Const Buffer; iCount : Integer);
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
    RaiseError('Rename', StringFormat('Cannot rename to "%s" - target filename already exists', [sDest]))
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


Procedure TAfsResourceFile.Write(Const Buffer; iSize : Integer);
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

{$ENDIF}


Constructor TFslCSVExtractor.Create;
Begin
  Inherited;

  FSeparator := ',';
  FQuote := '"';
  FHasQuote := True;
End;


Procedure TFslCSVExtractor.ConsumeEntries(oEntries : TFslStringList);
Var
  sEntry : String;
Begin
  If Assigned(oEntries) Then
    oEntries.Clear;

  // Consume all preceeding whitespace.
  if IgnoreWhitespace then
    ConsumeWhileCharacterSet(setControls + setVertical + setHorizontal)
  else
    ConsumeWhileCharacterSet(setVertical);

  While MoreEntries Do
  Begin
    sEntry := ConsumeEntry;

    If Assigned(oEntries) Then
      oEntries.Add(sEntry);
  End;
End;


Function TFslCSVExtractor.ConsumeEntry : String;
Var
  bMore : Boolean;
Begin
  // strip all leading whitespace.
  if IgnoreWhitespace then
    ConsumeWhileCharacterSet(setControls + setHorizontal);

  If More Then
  Begin
    If Not FHasQuote Or (NextCharacter <> FQuote) Then
    Begin
      // If it doesn't start with a quote then the entry is ended by a new line or the separator character.

      Result := ConsumeUntilCharacterSet([FSeparator] + setVertical);
    End
    Else
    Begin
      // Otherwise, if it is quoted, the entry is ended only by a closing quote.
      // Double quotes within the entry are resolved to a single quote.

      ConsumeCharacter(FQuote);

      Result := '';
      bMore := True;
      While bMore And More Do
      Begin
        If NextCharacter = FQuote Then
        Begin
          ConsumeCharacter(FQuote);

          bMore := More And (NextCharacter = FQuote);

          If bMore Then
            Result := Result + ConsumeCharacter
          Else
            ProduceString(FQuote);
        End
        Else
        Begin
          Result := Result + ConsumeCharacter;
        End;
      End;

      If More Then
        ConsumeCharacter(FQuote);
    End;

    If More Then
    Begin
      // strip trailing whitespace.
      if IgnoreWhitespace then
        ConsumeWhileCharacterSet(setControls + setHorizontal - setVertical);

      If More And (NextCharacter = FSeparator) Then
      Begin
        // strip separator character.
        ConsumeCharacter(FSeparator);

        // strip trailing non-newline whitespace after separator.
      if IgnoreWhitespace then
        ConsumeWhileCharacterSet(setControls + setHorizontal - setVertical);
      End;
    End;
  End
  Else
  Begin
    Result := '';
  End;
End;


Procedure TFslCSVExtractor.ConsumeEntries;
Begin
  ConsumeEntries(Nil);
End;


Function TFslCSVExtractor.MoreEntries : Boolean;
Begin
  Result := More And Not CharInSet(NextCharacter, setVertical);
End;

Constructor TFslCSVFormatter.Create;
Begin
  Inherited;

  FSeparator := ',';
  FQuote := '"';
  FHasQuote := True;
  FEmptyLine := True;
End;


Destructor TFslCSVFormatter.Destroy;
Begin
  Inherited;
End;


Procedure TFslCSVFormatter.Clear;
Begin
  Inherited;

  FEmptyLine := True;
End;


Procedure TFslCSVFormatter.ProduceEntryStringList(oEntryStringList: TFslStringList);
Var
  iEntryIndex : Integer;
Begin
  For iEntryIndex := 0 To oEntryStringList.Count - 1 Do
    ProduceEntry(oEntryStringList[iEntryIndex]);
End;


Procedure TFslCSVFormatter.ProduceEntryStringArray(Const aEntryStringArray: Array Of String);
Var
  iEntryIndex : Integer;
Begin
  For iEntryIndex := Low(aEntryStringArray) To High(aEntryStringArray) Do
    ProduceEntry(aEntryStringArray[iEntryIndex]);
End;


Procedure TFslCSVFormatter.ProduceEntry(Const sEntry : String);
Begin
  If FEmptyLine Then
    FEmptyLine := False
  Else
    ProduceSeparator;

  if sEntry <> '' then
    If FHasQuote Then
      Produce(EncodeQuotedString(sEntry, FQuote))
    Else
      Produce(sEntry)
End;


Procedure TFslCSVFormatter.ProduceNewLine;
Begin
  Inherited;

  FEmptyLine := True;
End;


Procedure TFslCSVFormatter.ProduceSeparator;
Begin
  Produce(FSeparator);
End;


Constructor TFslTextFormatter.Create;
Begin
  Inherited;

  FHasWhitespace := True;
  FWhitespaceCharacter := ' ';
  FWhitespaceMultiple := 2;
  {$IFNDEF VER130}
  Encoding := SysUtils.TEncoding.UTF8;
  {$ENDIF}
  FLocation := TSourceLocation.Create;
End;


Function TFslTextFormatter.Link : TFslTextFormatter;
Begin
  Result := TFslTextFormatter(Inherited Link);
End;


Procedure TFslTextFormatter.Clear;
Begin
  Inherited;

  FLevel := 0;
End;


Function TFslTextFormatter.BeforeWhitespace : String;
Begin
  // Multiply of the space character by FLevel * 2 is more efficient than Multiply of string '  ' by FLevel because it uses FillChar.

  If FHasWhitespace Then
    Result := StringMultiply(FWhitespaceCharacter, FLevel * FWhitespaceMultiple)
  Else
    Result := '';
End;


Function TFslTextFormatter.AfterWhitespace : String;
Begin
  If FHasWhitespace Then
    Result := cReturn
  Else
    Result := '';
End;

function TFslTextFormatter.AdjustLocation(location : TSourceLocation; const sText: String) : TSourceLocation;
var
  i : integer;
  nl : boolean;
begin
  result := location;
  nl := false;
  for i := 1 to length(sText) do
  begin
    case sText[i] of
      #13:
        begin
        nl := true;
        result.incLine;
        end;
      #10:
        if not nl then
        begin
        result.incLine;
        end;
    else
      begin
        nl := false;
        result.incCol;
      end;
    end;
  end;
end;

procedure TFslTextFormatter.Produce(const sText: String);
begin
  FLocation := AdjustLocation(FLocation, sText);
  inherited;
end;

procedure TFslTextFormatter.ProduceBytes(const aBytes: TBytes);
var
  i : integer;
  nl : boolean;
begin
  nl := false;
  // it's possible to get #10 and #13 from various unicode characters; this will cause miscounts here, but we take that risk
  for i := 0 to length(aBytes) - 1 do
  case aBytes[i] of
    13:
      begin
      nl := true;
      Flocation.incLine;
      end;
    10:
      if not nl then
      begin
      Flocation.incLine;
      end;
  else
    nl := false;
    FLocation.incCol;
  end;
  inherited;
end;

Procedure TFslTextFormatter.ProduceFragment(Const sValue: String);
Begin
  Produce(sValue);
End;


Procedure TFslTextFormatter.ProduceLine(Const sValue: String);
Begin
  Produce(BeforeWhitespace + sValue + AfterWhitespace);
End;


Procedure TFslTextFormatter.ProduceNewLine;
Begin
  Produce(cReturn);
End;


Procedure TFslTextFormatter.LevelDown;
Begin
  Inc(FLevel);
End;


Procedure TFslTextFormatter.LevelUp;
Begin
  Dec(FLevel);
End;


Procedure TFslTextFormatter.ProduceInline(Const sValue: String);
Begin
  Produce(sValue);
End;

Constructor TFslTextExtractor.Create;
Begin
  Inherited;

  FBuilder := TFslStringBuilder.Create;
End;


Destructor TFslTextExtractor.Destroy;
Begin
  FBuilder.Free;

  Inherited;
End;


Procedure TFslTextExtractor.CacheAdd(Const sValue: String);
Begin
  Inherited;

  Dec(FLine, StringCount(sValue, cEnter));
End;


Procedure TFslTextExtractor.CacheRemove(Const sValue: String);
Begin
  Inherited;

  Inc(FLine, StringCount(sValue, cEnter));
End;


Function TFslTextExtractor.ConsumeLine : String;
Begin
  Result := ConsumeUntilCharacterSet(setVertical);

  ConsumeWhileCharacterSet(setVertical);
End;


Procedure TFslTextExtractor.RaiseError(Const sMethod, sMessage: String);
Begin
  Inherited RaiseError(sMethod, StringFormat('Line %d: %s', [FLine, sMessage]));
End;


Function TFslTextExtractor.ErrorClass: EFslExceptionClass;
Begin
  Result := EFslTextExtractor;
End;



Procedure TFslTextExtractor.ProduceString(Const sToken : String);
Begin
  FCache := sToken + FCache;

  CacheAdd(sToken);
End;


Procedure TFslTextExtractor.ProduceCharacter(Const cToken : Char);
Begin
  ProduceString(cToken);
End;


Function TFslTextExtractor.MatchString(Const sToken: String): Boolean;
Var
  iCacheLength : Integer;
  iTokenLength : Integer;
  iReadSize : Integer;
  sNextString : String;
  Buffer: SysUtils.TCharArray;
Begin
  iTokenLength := Length(sToken);
  iCacheLength := Length(FCache);
  SetLength(Buffer, iTokenLength - iCacheLength);
  iReadSize := Read(Buffer, 0, iTokenLength - iCacheLength);

  If iReadSize > 0 Then
  Begin
    SetLength(Buffer, iReadSize);
    SetString(sNextString, pchar(Buffer), Length(Buffer));
    FCache := FCache + sNextString;
  End;

  Result := StringEquals(FCache, sToken, iTokenLength);
End;


Function TFslTextExtractor.MatchStringArray(Const aTokenSet: Array Of String): Integer;
Begin
  Result := High(aTokenSet);
  While (Result >= Low(aTokenSet)) And Not MatchString(aTokenSet[Result]) Do
    Dec(Result);
End;


Function TFslTextExtractor.NextCharacter : Char;
var
  Buffer: SysUtils.TCharArray;
Begin
  If Length(FCache) = 0 Then
  Begin
    SetLength(Buffer, 1);
    if Read(Buffer, 0, 1) = 1 Then
      result := Buffer[0]
    Else
      result := #0;
    FCache := Result;
  End
  Else
  Begin
    Result := FCache[1];
  End;
End;


Function TFslTextExtractor.ConsumeCharacter : Char;
Begin
  Result := NextCharacter;

  Delete(FCache, 1, 1);

  CacheRemove(Result);
End;


Function TFslTextExtractor.ConsumeCharacter(Const cToken : Char) : Char;

  Function ToCharacter(Const cChar : Char) : String;
  Begin
    If (cChar >= ' ') And (cChar <= #127) Then
      Result := cChar
    Else
      Result := '$' + inttohex(Word(cChar), 4);
  End;

Begin
  If Not StringEquals(cToken, NextCharacter) Then
    RaiseError('Consume(Char)', StringFormat('Expected token ''%s'' but found token ''%s''', [ToCharacter(cToken), ToCharacter(NextCharacter)]));

  Result := ConsumeCharacter;
End;


Function TFslTextExtractor.ConsumeString(Const sToken : String) : String;
Begin
  If Not MatchString(sToken) Then
    RaiseError('Consume(String)', StringFormat('Expected token ''%s'' but found token ''%s''', [sToken, Copy(FCache, 1, Length(sToken))]));

  Delete(FCache, 1, Length(sToken));

  CacheRemove(sToken);

  Result := sToken;
End;


Function TFslTextExtractor.ConsumeCharacterCount(Const iCharacterCount : Integer) : String;
Var
  iLoop : Integer;
Begin
  SetLength(Result, iCharacterCount);

  For iLoop := 1 To iCharacterCount Do
    Result[iLoop] := ConsumeCharacter;
End;


Function TFslTextExtractor.ConsumeUntilCharacterSet(Const aTokenSet: TCharSet): String;
Begin
  FBuilder.Clear;
  While More And Not CharInSet(NextCharacter, aTokenSet) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeUntilString(Const sToken: String): String;
Begin
  FBuilder.Clear;
  While More And Not MatchString(sToken) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeUntilString(Const aStringArray: Array Of String): String;
Begin
  FBuilder.Clear;
  While More And Not (MatchStringArray(aStringArray) >= 0) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeUntilCharacter(Const cToken : Char) : String;
Begin
  FBuilder.Clear;
  While More And (NextCharacter <> cToken) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeRestStream : String;
Begin
  FBuilder.Clear;
  While More Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeWhileCharacter(Const cToken : Char) : String;
Begin
  FBuilder.Clear;
  While More And (NextCharacter = cToken) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeWhileCharacterSet(Const aTokenSet : TCharSet) : String;
Begin
  FBuilder.Clear;
  While More And CharInSet(NextCharacter, aTokenSet) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeWhileCharacterSet(Const oCharacterSet: TFslCharacterSet): String;
Begin
  FBuilder.Clear;
  While More And oCharacterSet.ContainsValue(NextCharacter) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.CacheLength: Integer;
Begin
  Result := Length(FCache);
End;


Function TFslTextExtractor.StreamPosition: Int64;
Begin
  Assert(Invariants('StreamPosition', BaseStream, TFslAccessStream, 'Stream'));

  Result := TFslAccessStream(BaseStream).Position - CacheLength;
End;


Function TFslTextExtractor.More : Boolean;
Begin
  Result := Not Inherited EndOfStream Or (Length(FCache) > 0);
End;

function TFslTextExtractor.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCache.length * sizeof(char)) + 12);
  inc(result, FBuilder.sizeInBytes);
end;

Function TFslExtractor.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslExtractor;
End;


Procedure TFslExtractor.SetStream(oStream: TFslStream);
Begin
  Inherited;

  Clear;
End;


Procedure TFslExtractor.Clear;
Begin
End;


Function TFslExtractor.More: Boolean;
Begin
  Result := (Stream.Readable > 0);
End;



Function TFslFormatter.ErrorClass : EFslExceptionClass;
Begin
  Result := EIOException;
End;


Procedure TFslFormatter.Clear;
Begin
End;


Procedure TFslFormatter.ProduceBytes(Const aBytes : TBytes);
Begin
  Write(aBytes[0], Length(aBytes));
End;

Procedure TFslFormatter.Produce(Const sText: String);
{$IFDEF VER130}
Begin
  Write(Pointer(sText)^, Length(sText));
End;
{$ELSE}
Var
  Bytes : TBytes;
Begin
  Assert(CheckCondition(sText <> '', 'Produce', 'Text must not be empty.'));

  Bytes := SysUtils.TEncoding.UTF8.GetBytes(sText);

  Write(Bytes[0], Length(Bytes));
End;
{$ENDIF}


Procedure TFslFormatter.SetStream(oStream: TFslStream);
Begin
  Inherited;

  Clear;
End;


{ TFslStreamReader }

constructor TFslStreamReader.Create(aStream: TFslStream);
begin
  Create(aStream, TEncoding.UTF8, True);
end;

constructor TFslStreamReader.Create(aStream: TFslStream; DetectBOM: Boolean);
begin
  Create(aStream, TEncoding.UTF8, DetectBOM);
end;

constructor TFslStreamReader.Create(aStream: TFslStream; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0);
begin
  Create;

  if not Assigned(aStream) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Stream']); // DO NOT LOCALIZE
  if not Assigned(Encoding) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  FBufferSize := BufferSize;
  if FBufferSize = 0 then
    FBufferSize := aStream.Readable;
  if FBufferSize = 0 then
    FBufferSize := 128;
  SetLength(FBufferedData, BufferSize);
  FBufferEnd := 0;
  FBufferStart := 0;
  FClosed := false;

  FEncoding := Encoding;
  FNoDataInStream := False;
  FStream := aStream;
  FDetectBOM := DetectBOM;
  FSkipPreamble := not FDetectBOM;
  FCursor := 0;
end;

constructor TFslStreamReader.Create(const Filename: string; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0);
var
  oFile : TFslFile;
begin
  oFile := TFslFile.Create(FileName, fmOpenRead);
  Try
    Create(oFile.Link, Encoding, DetectBOM, BufferSize);
  Finally
    oFile.Free;
  End;
end;

constructor TFslStreamReader.Create(const Filename: string; DetectBOM: Boolean);
begin
  Create(Filename, TEncoding.UTF8, DetectBOM);
end;

constructor TFslStreamReader.Create(const Filename: string);
begin
  Create(Filename, TEncoding.UTF8, true);
end;

destructor TFslStreamReader.Destroy;
begin
  Close;
  inherited;
end;

procedure TFslStreamReader.Close;
begin
  FStream.Free;
  FStream := nil;

  DiscardBufferedData;
  FClosed := true;
end;

procedure TFslStreamReader.DiscardBufferedData;
begin
  if not FClosed then
  begin
    FBufferEnd := 0;
    FBufferStart := 0;
    FNoDataInStream := False;
  end;
end;

function TFslStreamReader.doDetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
var
  LEncoding: TEncoding;
begin
  // try to automatically detect the buffer encoding
  LEncoding := nil;
  Result := TEncoding.GetBufferEncoding(Buffer, LEncoding);

  // detected encoding points to Default and param Encoding requests some other
  // type of Encoding; set the Encoding param to UTF8 as it can also read ANSI (Default)
  if (LEncoding = TEncoding.Default) and (Encoding <> TEncoding.Default) then
    Encoding := TEncoding.UTF8
  else
    Encoding := LEncoding;

  FDetectBOM := False;
end;

procedure TFslStreamReader.FillBuffer(var Encoding: TEncoding);
const
  BufferPadding = 4;
var
  LString: string;
  LBuffer: TBytes;
  BytesRead: Integer;
  StartIndex: Integer;
  ByteBufLen: Integer;
  ok : boolean;
  tries : integer;
//  dbg : String;
begin
  SetLength(LBuffer, FBufferSize + BufferPadding);

  // Read data from stream
  BytesRead := IntegerMin(FBufferSize, FStream.Readable);
  FStream.Read(LBuffer[0], BytesRead);
  inc(FCursor, BytesRead);
  FNoDataInStream := FStream.Readable = 0;

  // Check for byte order mark and calc start index for character data
  if FDetectBOM then
    StartIndex := doDetectBOM(Encoding, LBuffer)
  else if FSkipPreamble then
    StartIndex := SkipPreamble(Encoding, LBuffer)
  else
    StartIndex := 0;

  // Convert to string and calc byte count for the string
  ByteBufLen := BytesRead - StartIndex;
  ok := false;
  tries := 0;
  repeat
    try
      if FCheckEncoding and (FEncoding.GetCharCount(LBuffer, StartIndex, ByteBufLen) = 0) then
      begin
        SetLength(LBuffer, Length(LBuffer) + BufferPadding);
        if FStream.Readable = 0 then // we're screwed:
          tries := maxint
        else
        begin
          inc(tries);
          FStream.Read(LBuffer[ByteBufLen], 1);
          inc(ByteBufLen)
        end;
      end;

      LString := FEncoding.GetString(LBuffer, StartIndex, ByteBufLen);
      ok := true;
    except
      on e : exception do
      begin
//        dbg := TEncoding.ANSI.getString(LBuffer, StartIndex, ByteBufLen);
//        BytesToFile(LBuffer, StartIndex, ByteBufLen, 'c:\temp\encoding.bin');
        if FCheckEncoding and (tries > FEncoding.GetMaxByteCount(1)) then
         raise EEncodingError.create(e.message{+ '('+dbg+')'})
        else
         FCheckEncoding := true;
      end;
    end;
  until ok;

  if (Length(LString) > 0) then
  begin
    // Add string to character data buffer
    if (FBufferStart > 0) and (FBufferEnd = FBufferStart) then
    begin
      FBufferStart := 0;
      FBufferEnd := 0;
    end;
    if Length(FBufferedData) < FBufferEnd + length(LString) then
      SetLength(FBufferedData, Length(FBufferedData) + length(LString) * 2);

    Move(LString[1], FBufferedData[FBufferEnd], length(LString) * SizeOf(Char));
    inc(FBufferEnd, length(LString));
  end;
end;

function TFslStreamReader.GetEndOfStream: Boolean;
begin
  if not FNoDataInStream and (not FClosed) and (FBufferEnd <= FBufferStart) then
    FillBuffer(FEncoding);
  Result := FNoDataInStream and ((FClosed) or (FBufferEnd = FBufferStart));
end;

function TFslStreamReader.Peek: Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    if FBufferEnd < 1 + FBufferStart  then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData[FBufferStart]);
  end;
end;

function TFslStreamReader.percent: integer;
begin
  result := FStream.percent;
end;

function TFslStreamReader.Read(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    while (FBufferEnd < Count + FBufferStart) and (not EndOfStream) and (not FNoDataInStream) do
      FillBuffer(FEncoding);

    if FBufferEnd > Count + FBufferStart then
      Result := Count
    else
      Result := FBufferEnd - FBufferStart;

    move(FBufferedData[FBufferStart], buffer[0], result * Sizeof(char));
    inc(FBufferStart, result);
  end;
end;

function TFslStreamReader.ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  Result := Read(Buffer, Index, Count);
end;

function TFslStreamReader.Read: Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    if FBufferEnd < 1 + FBufferStart then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData[FBufferStart]);
    inc(FBufferStart);
  end;
end;

function TFslStreamReader.ReadLine: string;
{var
  NewLineIndex: Integer;
  PostNewLineIndex: Integer;}
begin
  raise ELibraryException.create('This needs debugging for buffer changes');
{  Result := '';
  if FClosed then
    Exit;
  NewLineIndex := 0;
  PostNewLineIndex := 0;

  while True do
  begin
    if (NewLineIndex + 2 > FBufferEnd + FBufferStart) and (not FNoDataInStream) then
      FillBuffer(FEncoding);

    if NewLineIndex >= FBufferEnd + FBufferStart then
    begin
      if FNoDataInStream then
      begin
        PostNewLineIndex := NewLineIndex;
        Break;
      end
      else
      begin
        FillBuffer(FEncoding);
        if FBufferEnd = FBufferStart then
          Break;
      end;
    end;
    if FBufferedData[NewLineIndex + FBufferStart] = #10 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end
    else
    if (FBufferedData[NewLineIndex + FBufferStart] = #13) and (NewLineIndex + 1 < FBufferEnd + FBufferStart) and (FBufferedData[NewLineIndex + 1] = #10) then
    begin
      PostNewLineIndex := NewLineIndex + 2;
      Break;
    end
    else
    if FBufferedData[NewLineIndex + FBufferStart] = #13 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end;

    Inc(NewLineIndex);
  end;

  Result := FBufferedData.ToString.Substring(FBufferStart);
  SetLength(Result, NewLineIndex);
  inc(FBufferStart, PostNewLineIndex);}
end;

function TFslStreamReader.ReadToEnd: string;
begin
  raise ELibraryException.create('This needs debugging for FBufferStart');
  Result := '';
  if (not FClosed) and (not EndOfStream) then
  begin
    repeat
      FillBuffer(FEncoding);
    until FNoDataInStream;
    SetLength(result, FBufferEnd - FBufferStart);
    Move(FBufferedData[FBufferStart], result[1], length(result) * Sizeof(Char));
    FBufferEnd := 0;
    FBufferStart := 0;
  end;
end;

function TFslStreamReader.SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
var
  I: Integer;
  LPreamble: TBytes;
  BOMPresent: Boolean;
begin
  Result := 0;
  LPreamble := Encoding.GetPreamble;
  if (Length(LPreamble) > 0) then
  begin
    if Length(Buffer) >= Length(LPreamble) then
    begin
      BOMPresent := True;
      for I := 0 to Length(LPreamble) - 1 do
        if LPreamble[I] <> Buffer[I] then
        begin
          BOMPresent := False;
          Break;
        end;
      if BOMPresent then
        Result := Length(LPreamble);
    end;
  end;
  FSkipPreamble := False;
end;


function TFslStreamReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, length(FBufferedData));
  inc(result, FStream.sizeInBytes);
  inc(result, sizeof(FEncoding));
end;

{ TFslTextReader }

function TFslTextReader.ReadString(var s: String; iLength: Integer): Integer;
var
  oBuffer : TCharArray;
begin
  SetLength(oBuffer, iLength);
  result := ReadBlock(oBuffer, 0, iLength);
  SetString(s, pchar(oBuffer), result);
end;

{ TFslStringReader }

procedure TFslStringReader.Close;
begin
 // nothing
end;

constructor TFslStringReader.Create(content: String);
begin
  inherited Create;
  FContent := content;
  FCursor := 1;
end;

function TFslStringReader.Peek: Integer;
begin
  if FCursor > FContent.Length then
    result := -1
  else
    result := ord(FContent[FCursor]);
end;

function TFslStringReader.Read: Integer;
begin
  if FCursor > FContent.Length then
    result := -1
  else
  begin
    result := ord(FContent[FCursor]);
    inc(FCursor);
  end;
end;

function TFslStringReader.Read(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  raise ETodo.create('TFslStringReader.Read');
end;

function TFslStringReader.ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  raise ETodo.create('TFslStringReader.ReadBlock');
end;

function TFslStringReader.ReadLine: string;
begin
  raise ETodo.create('TFslStringReader.ReadLine');
end;

function TFslStringReader.ReadToEnd: string;
begin
  raise ETodo.create('TFslStringReader.ReadToEnd');
end;


{$IFNDEF FPC}
procedure produceCsv(filename : String; headers : Array of String; values : TGetCsvValues);
var
  csv : TCSVWriter;
  s : String;
begin
  csv := TCSVWriter.Create;
  try
    csv.Stream := TFslFile.Create(filename, fmCreate);
    for s in headers do
      csv.cell(s);
    csv.line;
    values(csv);
  finally
    csv.Free;
  end;
end;
{$ENDIF}

function TFslStringReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FContent.length * sizeof(char)) + 12);
end;

{ TCSVWriter }

procedure TCSVWriter.cell(s: String);
begin
  HasQuote := (s.Contains('"') or s.Contains(','));
  ProduceEntry(s);
end;

procedure TCSVWriter.cell(b: boolean);
begin
  if (b) then
    ProduceEntry('true')
  else
    ProduceEntry('false');
end;

procedure TCSVWriter.cell(i: integer);
begin
  cell(inttostr(i));
end;

procedure TCSVWriter.line;
begin
  ProduceNewLine;
end;




procedure StringToFile(content, filename : String; encoding : TEncoding);
var
  LFileStream: TFilestream;
  bytes : TBytes;
begin
  LFileStream := TFileStream.Create(filename, fmCreate);
  try
    bytes := encoding.GetBytes(content);
    LFileStream.write(bytes[0], length(bytes));
  finally
    LFileStream.Free;
  end;
end;

procedure StringToStream(content: String; stream : TStream; encoding : TEncoding);
var
  bytes : TBytes;
begin
  bytes := encoding.GetBytes(content);
  if (length(bytes) > 0) then
    stream.write(bytes[0], length(bytes));
end;

procedure StringToStream(content: String; stream : TFslStream; encoding : TEncoding);
var
  bytes : TBytes;
begin
  bytes := encoding.GetBytes(content);
  if (length(bytes) > 0) then
    stream.write(bytes[0], length(bytes));
end;

function FileToString(filename : String; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
var
  LFileStream: TFilestream;
  bytes : TBytes;
begin
  if FileExists(filename) then
    begin
    LFileStream := TFileStream.Create(filename, aShareMode);
    try
      SetLength(bytes, LFileStream.Size);
      if LFileStream.Size > 0 then
        LFileStream.Read(bytes[0], LFileStream.size);
    finally
      LFileStream.Free;
    end;
      result := encoding.GetString(bytes);
    end
  else
    raise ELibraryException.create('File "' + filename + '" not found');
end;

function StreamToString(stream : TStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
var
  bytes : TBytes;
begin
  SetLength(bytes, stream.Size);
  if stream.Size > 0 then
    stream.Read(bytes[0], stream.size);
  result := encoding.GetString(bytes);
end;

function StreamToString(stream : TFslStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
var
  bytes : TBytes;
begin
  SetLength(bytes, stream.Readable);
  if stream.Readable > 0 then
    stream.Read(bytes[0], stream.Readable);
  result := encoding.GetString(bytes);
end;

function StringToUTF8Stream(value : String):TStream;
begin
  result := TBytesStream.Create(TEncoding.UTF8.GetBytes(value));
end;

function UTF8StreamToString(value : TStream) : String;
var
  b : TBytes;
begin
  SetLength(b, value.Size);
  if (value.Size > 0) then
    value.Read(b[0], value.Size);
  result := TEncoding.UTF8.GetString(b);
end;

function UTF8StreamToString(value : TFslAccessStream) : String;
var
  b : TBytes;
begin
  SetLength(b, value.Size);
  if (value.Size > 0) then
    value.Read(b[0], value.Size);
  result := TEncoding.UTF8.GetString(b);
end;

procedure BytesToFile(bytes : TBytes; filename : String);
var
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmCreate);
  try
    if length(bytes) > 0 then
      f.Write(bytes[0], length(bytes));
  finally
    f.Free;
  end;
end;

procedure BytesToFile(bytes : TBytes; start, length : integer; filename : String);
var
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmCreate);
  try
    if length > 0 then
      f.Write(bytes[start], length);
  finally
    f.Free;
  end;
end;


procedure StreamToFile(stream : TStream; filename : String);
var
  f : TFileStream;
  i : integer;
begin
  f := TFileStream.Create(filename, fmCreate);
  try
    i := stream.Position;
    f.CopyFrom(stream, stream.Size - stream.Position);
    stream.Position := i;
  finally
    f.Free;
  end;
end;

function FileToBytes(filename : String; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : TBytes;
var
  LFileStream: TFilestream;
begin
  if FileExists(filename) then
    begin
    LFileStream := TFileStream.Create(filename, aShareMode);
    try
      SetLength(result, LFileStream.Size);
      if LFileStream.Size > 0 then
        LFileStream.Read(result[0], LFileStream.size);
    finally
      LFileStream.Free;
    end;
    end
  else
    raise ELibraryException.create('File "' + filename + '" not found');
end;


{ TFslZipWorker }

Constructor TFslZipWorker.Create;
Begin
  Inherited;
  FParts := TFslZipPartList.Create;
End;

Destructor TFslZipWorker.Destroy;
Begin
  FStream.Free;
  FParts.Free;
  Inherited;
End;

Function TFslZipWorker.GetParts: TFslZipPartList;
Begin
  Assert(Invariants('GetParts', FParts, TFslZipPartList, 'Parts'));
  Result := FParts;
End;

Function TFslZipWorker.GetStream: TFslStream;
Begin
  Assert(Invariants('GetStream', FStream, TFslStream, 'Stream'));
  Result := FStream;
End;

Function TFslZipWorker.HasParts: Boolean;
Begin
  Result := FParts <> Nil;
End;

Function TFslZipWorker.HasStream: Boolean;
Begin
  Result := FStream <> Nil;
End;

Procedure TFslZipWorker.SetParts(oValue: TFslZipPartList);
Begin
  FParts.Free;
  FParts := oValue;
End;

Procedure TFslZipWorker.SetStream(oValue: TFslStream);
Begin
  FStream.Free;
  FStream := oValue;
End;

Function Bit(iFlags : Word; aFlag : TZipFlag) : Boolean;
Var
  iVal : Word;
Begin
  iVal := 1 Shl Ord(aFlag);
  Result := iFlags And iVal > 0;
End;


function TFslZipWorker.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStream.sizeInBytes);
  inc(result, FParts.sizeInBytes);
end;

Procedure TFslZipPart.Assign(oObject : TFslObject);
Begin
  Inherited;
  FTimestamp := TFslZipPart(oObject).FTimestamp;
  FComment := TFslZipPart(oObject).FComment;
End;



Function TFslZipPart.Link : TFslZipPart;
Begin
  Result := TFslZipPart(Inherited Link);
End;


Function TFslZipPart.Clone : TFslZipPart;
Begin
  Result := TFslZipPart(Inherited Clone);
End;


function TFslZipPart.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FComment.length * sizeof(char)) + 12);
end;

procedure TFslZipPartList.add(name: String; bytes: TBytes);
var
  part : TFslZipPart;
begin
  part := TFslZipPart.Create;
  try
    part.Name := name;
    part.AsBytes := bytes;
    add(part.Link);
  finally
    part.Free;
  end;
end;

Function TFslZipPartList.Clone : TFslZipPartList;
Begin
  Result := TFslZipPartList(Inherited Clone);
End;


Function TFslZipPartList.Link : TFslZipPartList;
Begin
  Result := TFslZipPartList(Inherited Link);
End;


Function TFslZipPartList.ItemClass : TFslObjectClass;
Begin
  Result := TFslZipPart;
End;



Function TFslZipPartList.GetPart(iIndex : Integer) : TFslZipPart;
Begin
  Result := TFslZipPart(ObjectByIndex[iIndex]);
End;


Function TFslZipPartList.GetByName(Const sName: String): TFslZipPart;
Begin
  Result := TFslZipPart(Inherited GetByName(sName));
End;


Function TFslZipReader.ReadLongWord: LongWord;
Begin
  Result := 0;
  Stream.Read(Result, SizeOf(Result));
End;

Function TFslZipReader.ReadWord: Word;
Begin
  Result := 0;
  Stream.Read(Result, SizeOf(Result));
End;

Function TFslZipReader.ReadByte: Byte;
Begin
  Result := 0;
  Stream.Read(Result, SizeOf(Result));
End;


Procedure TFslZipReader.ReadPart;
Var
  iFlags : Word;
  iComp : Word;
  iSizeComp : LongWord;
  iSizeUncomp : LongWord;
  iNameLen : Word;
  iExtraLen : Word;
  iDate : Word;
  iTime : Word;
  oBuffer : TFslZipPart;
Begin
  oBuffer := TFslZipPart(Parts.New);
  Try
    ReadWord;                          // version needed to extract       2 bytes
    iFlags := ReadWord;               // general purpose bit flag        2 bytes
    iComp := ReadWord;                // compression method              2 bytes
    iTime := ReadWord;                // last mod file time              2 bytes
    iDate := ReadWord;                // last mod file date              2 bytes
    oBuffer.Timestamp := TimeAndDateToDateTime(iDate, iTime);
    ReadLongWord;                          // crc-32                          4 bytes
    iSizeComp := ReadLongWord;           // compressed size                 4 bytes
    iSizeUncomp := ReadLongWord;         // uncompressed size               4 bytes
    iNameLen := ReadWord;             // filename length                 2 bytes
    iExtraLen := ReadWord;            // extra field length              2 bytes
    oBuffer.Name := string(ReadString(iNameLen));    // filename (variable size)
    Skip(iExtraLen);                  // extra field (variable size)

    {Immediately following the local header for a file
      is the compressed or stored data for the file. }
    ReadData(oBuffer.Name, iFlags, iComp, iSizeComp, iSizeUncomp, oBuffer);

    Parts.Add(oBuffer.Link);
  Finally
    oBuffer.Free;
  End;
End;

Procedure TFslZipReader.ReadZip;
Var
  iSig : LongWord;
  iCount : Integer;
Begin
  Parts.Clear;
  Parts.Unsorted;

  iSig := ReadLongWord;
  While (iSig = SIG_LOCAL_FILE_HEADER) Do
  Begin
    ReadPart;
    iSig := ReadLongWord;
  End;
  iCount := 0;
  While (iSig = SIG_CENTRAL_DIRECTORY_HEADER) Do
  Begin
    ReadDirectory(iCount);
    Inc(iCount);
    iSig := ReadLongWord;
  End;
  If (iSig = SIG_DIGITAL_SIGNATURE) Then
  Begin
    ReadDigSig;
    iSig := ReadLongWord;
  End;
  If (iSig = SEG_TERMINATION) Then
  Begin
    ReadTermination;
  End;
  // we ignore the rest of the file!
End;

Procedure TFslZipReader.ReadUncompressed(iSizeComp : LongWord; oBuffer: TFslBuffer);
Begin
  oBuffer.Capacity := iSizeComp;
  If (iSizeComp > 0) Then
    Stream.Read(oBuffer.Data^, oBuffer.Capacity);
End;

Procedure TFslZipReader.ReadDeflate(iFlags : Word; partName : string; iSizeComp, iSizeUncomp : LongWord; oBuffer: TFslBuffer);
Var
  pIn : PAnsiChar;
Begin
  If Bit(iFlags, flagUsesDataDescriptor) Then
    ReadUnknownLengthDeflate(partName, oBuffer)
  Else
  Begin
    GetMem(pIn, iSizeComp+2);
    Try
      pIn[0] := AnsiChar(120);
      pIn[1] := AnsiChar(156);
      Stream.Read(pIn[2], iSizeComp);
      ReadKnownDeflate(pIn, partName, iSizeComp+2, iSizeUncomp, oBuffer);
    Finally
      FreeMem(pIn);
    End;
  End;
End;

Function TFslZipReader.EndCondition(iLongWord : LongWord) : Boolean;
Begin
  Result := //(Stream.Readable = 0) Or // shouldn't run out - should run into the central directory
            (iLongWord = SIG_DATA_DESCRIPTOR);
End;


Procedure TFslZipReader.ReadUnknownLengthDeflate(partName : string; oBuffer : TFslBuffer);
Var
  iCurrent{, iCRC} : LongWord;
  iByte : Byte;
  oMem : TFslMemoryStream;
  iSizeComp : LongWord;
  iSizeUncomp : LongWord;
//  count : integer;
  first : boolean;
Begin
  // well, we don't know how long it's going to be.
  // what we're going to do is read this a byte at a time until
  // we are at the next header or the source runs out. There's a minor chance
  // that this will terminate early (1 in 2^32)
  // then we lop off the last 12 bytes, and treat this is the decompressible
  // we can start with a 4 byte read because we know we have at least 12 bytes
  oMem := TFslMemoryStream.Create;
  Try
    iByte := 120;
    oMem.Write(iByte, 1);
    iByte := 156;
    oMem.Write(iByte, 1);
    iCurrent := ReadLongWord;
//    count := 0;
    repeat
      first := true;
      While first or (Not EndCondition(iCurrent)) Do
      Begin
        iByte := iCurrent And $FF;
        oMem.Write(iByte, 1);
        iCurrent := (iCurrent And $FFFFFF00) Shr 8 + ReadByte Shl 24;
        first := false;
      End;
//      inc(count);
    until true; // (partName <> 'fhir.schema.json.zip') or (count > 1);
    If iCurrent <> SIG_DATA_DESCRIPTOR Then
      RaiseError('ReadUnknownLengthDeflate', 'Error in zip structure: Source is not terminated by a Data Descriptor');
    {iCRC := }ReadLongWord;                // crc-32                          4 bytes
    iSizeComp := ReadLongWord;           // compressed size                 4 bytes
    iSizeUncomp := ReadLongWord;         // uncompressed size               4 bytes
    {$WARNINGS OFF} // a widening notifications in this check and the assertion in ReadKnownDeflate
    If oMem.Buffer.Capacity < iSizeComp + 2 Then
      RaiseError('ReadUnknownLengthDeflate', 'Compressed length expected to be '+IntegerToString(iSizeComp)+' bytes but found '+IntegerToString(oMem.Buffer.Capacity)+' bytes');
    ReadKnownDeflate(oMem.Buffer.Data, partName, iSizeComp + 2, iSizeUncomp, oBuffer);
  Finally
    oMem.Free;
  End;
End;

Type
  TPointerMemoryStream = Class (TCustomMemoryStream)
    constructor Create(pData : Pointer; iSize : Integer);
    Function Write(Const Buffer; Count: LongInt): LongInt; Override;
  End;

Constructor TPointerMemoryStream.Create(pData : Pointer; iSize : Integer);
Begin
  Inherited Create;
  SetPointer(pData, iSize);
End;

Function TPointerMemoryStream.Write(Const Buffer; Count: LongInt): LongInt;
Begin
  Raise EFslException.Create('Should never be called');
End;


{$IFDEF FPC}
type
   TZDecompressionStream = TDecompressionStream;
{$ENDIF}

Procedure TFslZipReader.ReadKnownDeflate(pIn : Pointer; partName : string; iSizeComp, iSizeDecomp : LongWord; oBuffer : TFslBuffer);
Var
  oSrc : TStream;
  oDecompressor : TZDecompressionStream;

{$IFOPT C+}
  iRead : Integer;
{$ENDIF}
Begin
  If iSizeDecomp > 0 Then
  Begin
    oSrc := TPointerMemoryStream.Create(pIn, iSizeComp);
    Try
      oDecompressor := TZDecompressionStream.Create(oSrc);
      Try
        oBuffer.Capacity := iSizeDecomp;

      {$IFOPT C+}
        iRead := oDecompressor.Read(oBuffer.Data^, iSizeDecomp);
        Assert(CheckCondition(iRead = iSizeDecomp, 'ReadKnownDeflate', partName+': Expected to read '+IntegerToString(iSizeDecomp)+
            ' bytes, but actually found '+IntegerToString(iRead)+' bytes'));
      {$ELSE}
        oDecompressor.Read(oBuffer.Data^, iSizeDecomp);
      {$ENDIF}
      Finally
        oDecompressor.Free;
      End;
    Finally
      oSrc.Free;
    End;
  End;
End;

Procedure TFslZipReader.ReadData(partName : string; iFlags, iComp : Word; iSizeComp, iSizeUncomp: LongWord; oBuffer: TFslBuffer);
Begin
  Case iComp Of
    METHOD_NONE: ReadUncompressed(iSizeComp, oBuffer);
    METHOD_DEFLATE: ReadDeflate(iFlags, partName, iSizeComp, iSizeUncomp, oBuffer);
  Else
    RaiseError('Decompress', 'Unknown Compression type '+IntegerToString(iComp));
  End;
End;

Function TFslZipReader.ReadString(iLength: Word): AnsiString;
Begin
  SetLength(Result, iLength);
  If (iLength > 0) Then
    Stream.Read(Result[1], iLength);
End;

Procedure TFslZipReader.Skip(iCount: Integer);
Begin
  If iCount > 0 Then
    ReadString(iCount);
End;

Procedure TFslZipReader.ReadDigSig;
Var
  iLen : Word;
Begin
  iLen := ReadWord;
  Skip(iLen);
End;

Procedure TFslZipReader.ReadDirectory(iCount: Integer);
Var
  oPart : TFslZipPart;
  iNameLen : Word;
  iExtraLen : Word;
  iCommLen : Word;
Begin
  oPart := Parts.Part[iCount];
  ReadWord; //  version made by                 2 bytes    63 vs 20
  ReadWord; //  version needed to extract       2 bytes
  ReadWord; //  general purpose bit flag        2 bytes
  ReadWord; //  compression method              2 bytes
  ReadWord; //  last mod file time              2 bytes
  ReadWord; //  last mod file date              2 bytes
  ReadLongWord; //  crc-32                          4 bytes
  ReadLongWord; //  compressed size                 4 bytes
  ReadLongWord; //  uncompressed size               4 bytes
  iNameLen := ReadWord;  // filename length                 2 bytes
  iExtraLen := ReadWord; // extra field length              2 bytes   36 vs 0
  iCommLen := ReadWord; // file comment length             2 bytes
  ReadWord; // disk number start               2 bytes
  ReadWord; // internal file attributes        2 bytes
  ReadLongWord; // external file attributes        4 bytes  32 vs 0
  ReadLongWord; // relative offset of local header 4 bytes
  Skip(iNameLen);
  Skip(iExtraLen);
  oPart.Comment := string(ReadString(iCommLen));
End;

Procedure TFslZipReader.ReadTermination;
Begin

End;


Constructor TFslZipWriter.Create;
Begin
  Inherited;
  FPartInfo := TFslObjectMatch.Create;
End;

Destructor TFslZipWriter.Destroy;
Begin
  FPartInfo.Free;
  Inherited;
End;


Procedure TFslZipWriter.WriteZip;
Var
  iLoop : Integer;
Begin
  FOffset := 0;
  FPartInfo.Clear;

  For iLoop := 0 To Parts.Count - 1 Do
    WritePart(Parts[iLoop]);
  FDirOffset := FOffset;
  For iLoop := 0 To Parts.Count - 1 Do
    WriteDirectory(Parts[iLoop]);
  WriteEnd(Parts.Count);
End;


Procedure TFslZipWriter.WriteLongWord(iValue : LongWord);
Begin
  Stream.Write(iValue, 4);
  Inc(FOffset, 4);
End;


Procedure TFslZipWriter.WriteWord(iValue : Word);
Begin
  Stream.Write(iValue, 2);
  Inc(FOffset, 2);
End;


Procedure TFslZipWriter.WriteString(Const sValue : AnsiString);
Begin
  If (sValue <> '') Then
  Begin
    Stream.Write(sValue[1], Length(sValue));
    Inc(FOffset, Length(sValue));
  End;
End;


Procedure TFslZipWriter.WritePart(oPart: TFslZipPart);
Var
  oCompressed : TFslBuffer;
  oInfo : TFslZippedData;
Begin
  oInfo := TFslZippedData.Create;
  Try
    oInfo.FOffset := FOffset;
    WriteLongWord(SIG_LOCAL_FILE_HEADER);
    WriteWord($14);    // version needed to extract. don't know why $14, just observed in any .zip file
    WriteWord(0);      // general purpose bit flag. We don't set any flags
    If oPart.Capacity > 0 Then   // compression method  2 bytes
      WriteWord(8)
    Else
      WriteWord(0);
    DateTimeToTimeAndDate(oPart.TimeStamp, oInfo.FDate, oInfo.FTime);
    WriteWord(oInfo.FTime);      // last mod file time              2 bytes
    WriteWord(oInfo.FDate);      // last mod file date              2 bytes
    oInfo.FCrc := GetCRC(oPart.AsBytes);
    WriteLongWord(oInfo.FCrc);     // crc-32

    oCompressed := TFslBuffer.Create;
    Try
      If (oPart.Capacity > 0) Then
        Compress(oPart, oCompressed);

      oInfo.FCompressedSized := oCompressed.Capacity;
      WriteLongWord(oInfo.FCompressedSized); // compressed size                 4 bytes
      WriteLongWord(oPart.Capacity);       // uncompressed size               4 bytes
      WriteWord(Length(oPart.Name));    // filename length                 2 bytes
      WriteWord(0);                     // extra field length - we don't use
      WriteString(AnsiString(oPart.Name));

      If (oCompressed.Capacity > 0) Then
      Begin
        Stream.Write(oCompressed.Data^, oCompressed.Capacity);
        Inc(FOffset, oCompressed.Capacity);
      End;

    Finally
      oCompressed.Free;
    End;
    FPartInfo.Add(oPart.Link, oInfo.Link);
  Finally
    oInfo.Free;
  End;
End;


procedure TFslZipWriter.addFile(name, actual: String);
var
  part : TFslZipPart;
begin
  part := TFslZipPart.Create;
  try
    part.Name := name;
    part.Timestamp := FileGetModified(actual);
    part.LoadFromFileName(actual);
    Parts.Add(part.Link);
  finally
    part.Free;
  end;
end;

Procedure TFslZipWriter.Compress(oSource, oDestination: TFslBuffer);
Var
  oCompressor: TCompressionStream;
  oCompressedStream: TMemoryStream;
  pData : PAnsiChar;
Begin
  oCompressedStream := TMemoryStream.Create;
  Try
    oCompressor := TCompressionStream.Create(clMax, oCompressedStream);
    Try
      oCompressor.Write(oSource.Data^, oSource.Capacity);
    Finally
      oCompressor.Free;
    End;
    pData := oCompressedStream.Memory;
    pData := PData + 2;
    oDestination.Capacity := oCompressedStream.Size - 6; // 2 off the front, 4 off the back
    MemoryMove(oDestination.Data, pData, oDestination.Capacity);
  Finally
    oCompressedStream.Free;
  End;
End;

Procedure TFslZipWriter.WriteDirectory(oPart: TFslZipPart);
Var
  oInfo : TFslZippedData;
Begin
  oInfo := TFslZippedData(FPartInfo.GetValueByKey(oPart));

  WriteLongWord(SIG_CENTRAL_DIRECTORY_HEADER);
  WriteWord($14); // version made by                 2 bytes
  WriteWord($14); // version needed to extract       2 bytes
  WriteWord(0);      // general purpose bit flag. We don't set any flags
  If oPart.Capacity > 0 Then   // compression method  2 bytes
    WriteWord(8)
  Else
    WriteWord(0);
  WriteWord(oInfo.FTime);      // last mod file time              2 bytes
  WriteWord(oInfo.FDate);      // last mod file date              2 bytes
  WriteLongWord(GetCRC(oPart.AsBytes));     // crc-32  TODO: cache this?
  WriteLongWord(oInfo.FCompressedSized); // compressed size                 4 bytes
  WriteLongWord(oPart.Capacity);       // uncompressed size               4 bytes
  WriteWord(Length(oPart.Name));    // filename length                 2 bytes
  WriteWord(0);                     // extra field length - we don't use
  WriteWord(Length(oPart.Comment));    // file comment length             2 bytes
  WriteWord(0);                     // disk number start               2 bytes
  WriteWord(0);                     // internal file attributes        2 bytes
  WriteLongWord(0);                 // external file attributes        4 bytes
  WriteLongWord(oInfo.FOffset);
  WriteString(AnsiString(oPart.Name));          // filename (variable size)
  WriteString(AnsiString(oPart.Comment));       // file comment (variable size)

End;

Procedure TFslZipWriter.WriteEnd(iCount : Integer);
Var
  iOffset : Integer;
Begin
  iOffset := FOffset;
  WriteLongWord(SEG_TERMINATION); // end of central dir signature    4 bytes  (0x06054b50)
  WriteWord(0);     //  number of this disk             2 bytes
  WriteWord(0);     // number of the disk with the start of the central directory  2 bytes
  WriteWord(iCount); // total number of entries in the central dir on this disk    2 bytes
  WriteWord(iCount); // total number of entries in the central dir                 2 bytes
  WriteLongWord(iOffset-FDirOffset); // size of the central directory   4 bytes
  WriteLongWord(FDirOffset);         // offset of start of central
  WriteLongWord(0);     // directory with respect to the starting disk number        4 bytes
  WriteWord(0);     //  .ZIP file comment length        2 bytes
  //       .ZIP file comment       (variable size)
End;


Function TimeAndDateToDateTime(iDate, iTime : Word) : TDateTime;
Var
  iCombined : Integer;
Begin
  LongRec(iCombined).Lo := iTime;
  LongRec(iCombined).Hi := iDate;
  Result := FileDateToDateTime(iCombined);
End;

Procedure DateTimeToTimeAndDate(aValue : TDateTime; Out iDate, iTime : Word);
Var
  iCombined : Integer;
Begin
  If (aValue = 0) Then
    aValue := Now;
  iCombined := DateTimeToFileDate(aValue);
  iTime := LongRec(iCombined).Lo;
  iDate := LongRec(iCombined).Hi;
End;

Function GetCRC(bytes : TBytes) : LongWord;
Const
  CRCtable: Array[0..255] Of LongWord = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4,
    $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064,
    $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A,
    $8A65C9EC, $14015C4F, $63066CD9,
    $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1,
    $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF,
    $ABD13D59, $26D930AC, $51DE003A,
    $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E,
    $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106, $98D220BC,
    $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1,
    $F50FC457, $65B0D9C6, $12B7E950,
    $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158,
    $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846,
    $DA60B8D0, $44042D73, $33031DE5,
    $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525,
    $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B,
    $C0BA6CAD, $EDB88320, $9ABFB3B6,
    $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683, $E3630B12,
    $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB,
    $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F,
    $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD,
    $48B2364B, $D80D2BDA, $AF0A1B4C,
    $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, $CB61B38C,
    $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92,
    $5CB36A04, $C2D7FFA7, $B5D0CF31,
    $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9,
    $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7,
    $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354,
    $3903B3C2, $A7672661, $D06016F7,
    $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53,
    $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729,
    $23D967BF, $B3667A2E, $C4614AB8,
    $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);
Var
  iLoop : Integer;
Begin
  Result := $FFFFFFFF;
  For iLoop := 0 To length(bytes) - 1 Do
    Result := (Result Shr 8) Xor (CRCtable[Byte(Result) Xor Ord(bytes[iLoop])]);
  Result := Result Xor $FFFFFFFF;
End;


function PermissionString (Permissions : TTarPermissions) : String;
begin
  Result := '';
  if tpReadByOwner    in Permissions then Result := Result + 'r' else Result := Result + '-';
  if tpWriteByOwner   in Permissions then Result := Result + 'w' else Result := Result + '-';
  if tpExecuteByOwner in Permissions then Result := Result + 'x' else Result := Result + '-';
  if tpReadByGroup    in Permissions then Result := Result + 'r' else Result := Result + '-';
  if tpWriteByGroup   in Permissions then Result := Result + 'w' else Result := Result + '-';
  if tpExecuteByGroup in Permissions then Result := Result + 'x' else Result := Result + '-';
  if tpReadByOther    in Permissions then Result := Result + 'r' else Result := Result + '-';
  if tpWriteByOther   in Permissions then Result := Result + 'w' else Result := Result + '-';
  if tpExecuteByOther in Permissions then Result := Result + 'x' else Result := Result + '-';
end;


function ConvertFilename  (Filename : AnsiString) : AnsiString;
// Converts the filename to Unix conventions
// could be empty and inlined away for FPC. FPC I/O should be
// forward/backward slash safe.
begin
  (*$IFDEF Unix *)
  Result := Filename;
  (*$else *)
  Result := StringReplace (Filename, '\', '/', [rfReplaceAll]);
  (*$ENDIF *)
end;

function FileTimeGMT (FileName: AnsiString): TDateTime;
         // Returns the Date and Time of the last modification of the given File
         // The Result is zero if the file could not be found
         // The Result is given in UTC (GMT) time zone
var
  SR : TSearchRec;
begin
  Result := 0.0;
  if FindFirst (FileName, faAnyFile, SR) = 0 then
    Result := FileTimeGMT (SR);
  FindClose (SR);
end;


function FileTimeGMT (SearchRec : TSearchRec) : TDateTime;
{$IFDEF WINDOWS}
var
  SystemFileTime: TSystemTime;
begin
  {$WARNINGS OFF}
  Result := 0.0;
  if (SearchRec.FindData.dwFileAttributes and faDirectory) = 0 then
    if FileTimeToSystemTime (SearchRec.FindData.ftLastWriteTime, SystemFileTime) then
      Result := EncodeDate (SystemFileTime.wYear, SystemFileTime.wMonth, SystemFileTime.wDay)
              + EncodeTime (SystemFileTime.wHour, SystemFileTime.wMinute, SystemFileTime.wSecond, SystemFileTime.wMilliseconds);
  {$WARNINGS ON}
end;
{$ENDIF}
{$IFDEF LINUX}
var
  TimeVal  : TTimeVal;
  TimeZone : baseunix.TTimeZone;
begin
  Result := 0.0;
  if SearchRec.Attr and faDirectory = 0 then
  begin
    FillChar(TimeVal, SizeOf(TimeVal), #0);
    FillChar(TimeZone, SizeOf(TimeZone), #0);
    Result := FileDateToDateTime (SearchRec.Time);
    fpGetTimeOfDay (@TimeVal, @TimeZone);
    Result := Result + TimeZone.tz_minuteswest / (60 * 24);
  end;
end;
{$ENDIF}
{$IFDEF OSX}
begin
  raise Exception.create('To do');
end;
{$ENDIF}

procedure ClearDirRec (var DirRec : TTarDirRec);
          // This is included because a FillChar (DirRec, SizeOf (DirRec), 0)
          // will destroy the long String pointers, leading to strange bugs
begin
  WITH DirRec DO
  begin
    Name        := '';
    Size        := 0;
    DateTime    := 0.0;
    Permissions := [];
    FileType    := TFileType (0);
    LinkName    := '';
    UID         := 0;
    GID         := 0;
    UserName    := '';
    GroupName   := '';
    ChecksumOK  := FALSE;
    Mode        := [];
    Magic       := '';
    MajorDevNo  := 0;
    MinorDevNo  := 0;
//    FilePos     := 0;
  end;
end;

(*
===============================================================================================
TAR format
===============================================================================================
*)

const
  RECORDSIZE = 512;
  NAMSIZ     = 100;
  TUNMLEN    =  32;
  TGNMLEN    =  32;
  CHKBLANKS  = #32#32#32#32#32#32#32#32;

TYPE
  TTarHeader = packed record
                 Name     : array [0..NAMSIZ-1] of AnsiChar;
                 Mode     : array [0..7] of AnsiChar;
                 UID      : array [0..7] of AnsiChar;
                 GID      : array [0..7] of AnsiChar;
                 Size     : array [0..11] of AnsiChar;
                 MTime    : array [0..11] of AnsiChar;
                 ChkSum   : array [0..7] of AnsiChar;
                 LinkFlag : AnsiChar;
                 LinkName : array [0..NAMSIZ-1] of AnsiChar;
                 Magic    : array [0..7] of AnsiChar;
                 UName    : array [0..TUNMLEN-1] of AnsiChar;
                 GName    : array [0..TGNMLEN-1] of AnsiChar;
                 DevMajor : array [0..7] of AnsiChar;
                 DevMinor : array [0..7] of AnsiChar;
               end;

function ExtractText (P : PAnsiChar) : AnsiString;
begin
  Result := AnsiString(P);
end;


function ExtractNumber (P : PAnsiChar) : integer; overload;
var
  Strg : AnsiString;
begin
  Strg := Trim ({$IFNDEF FPC}AnsiStrings.{$ENDIF}StrPas (P));
  P := PAnsiChar (Strg);
  Result := 0;
  while (P^ <> #32) and (P^ <> #0) DO
  begin
    Result := (ORD (P^) - ORD ('0')) or (Result SHL 3);
    INC (P);
  end;
end;

function ExtractNumber64 (P : PAnsiChar) : int64; overload;
var
  Strg : AnsiString;
begin
  Strg := Trim ({$IFNDEF FPC}AnsiStrings.{$ENDIF}StrPas (P));
  P := PAnsiChar (Strg);
  Result := 0;
  while (P^ <> #32) and (P^ <> #0) DO
  begin
    Result := (ORD (P^) - ORD ('0')) or (Result SHL 3);
    INC (P);
  end;
end;


function ExtractNumber (P : PAnsiChar; MaxLen : integer) : integer; overload;
var
  S0   : array [0..255] of AnsiChar;
  Strg : AnsiString;
begin
  {$IFNDEF FPC}AnsiStrings.{$ENDIF}StrLCopy (S0, P, MaxLen);
  Strg := Trim ({$IFNDEF FPC}AnsiStrings.{$ENDIF}StrPas (S0));
  P := PAnsiChar (Strg);
  Result := 0;
  while (P^ <> #32) and (P^ <> #0) DO
  begin
    Result := (ORD (P^) - ORD ('0')) or (Result SHL 3);
    INC (P);
  end;
end;


function ExtractNumber64 (P : PAnsiChar; MaxLen : integer) : int64; overload;
var
  S0   : array [0..255] of AnsiChar;
  Strg : AnsiString;
begin
  {$IFNDEF FPC}AnsiStrings.{$ENDIF}StrLCopy (S0, P, MaxLen);
  Strg := Trim ({$IFNDEF FPC}AnsiStrings.{$ENDIF}StrPas (S0));
  P := PAnsiChar (Strg);
  Result := 0;
  while (P^ <> #32) and (P^ <> #0) do
  begin
    Result := (ORD (P^) - ORD ('0')) or (Result SHL 3);
    INC (P);
  end;
end;


function Records (Bytes : int64) : int64;
begin
  Result := Bytes DIV RECORDSIZE;
  if Bytes MOD RECORDSIZE > 0 then
    INC (Result);
end;


procedure Octal (N : integer; P : PAnsiChar; Len : integer);
         // Makes a String of octal digits
         // The String will always be "Len" characters long
var
  I     : integer;
begin
  FOR I := Len-2 DOWNTO 0 do
  begin
    (P+I)^ := AnsiChar (ORD ('0') + ORD (N and $07));
    N := N SHR 3;
  end;
  FOR I := 0 TO Len-3 do
    if (P+I)^ = '0' then
      (P+I)^ := #32
    else
      BREAK;
  (P+Len-1)^ := #32;
end;


procedure Octal64 (N : int64; P : PAnsiChar; Len : integer);
         // Makes a String of octal digits
         // The String will always be "Len" characters long
var
  I     : integer;
begin
  FOR I := Len-2 DOWNTO 0 do
  begin
    (P+I)^ := AnsiChar (ORD ('0') + ORD (N and $07));
    N := N SHR 3;
  end;
  FOR I := 0 TO Len-3 do
    if (P+I)^ = '0' then
      (P+I)^ := #32
    else
      BREAK;
  (P+Len-1)^ := #32;
end;


procedure OctalN (N : integer; P : PAnsiChar; Len : integer);
begin
  Octal (N, P, Len-1);
  (P+Len-1)^ := #0;
end;


procedure WriteTarHeader (Dest : TStream; DirRec : TTarDirRec);
var
  Rec      : array [0..RECORDSIZE-1] of AnsiChar;
  TH       : TTarHeader ABSOLUTE Rec;
  Mode     : integer;
  NullDate : TDateTime;
  Checksum : CARDINAL;
  I        : integer;
begin
  FillChar (Rec, RECORDSIZE, 0);
  {$IFNDEF FPC}AnsiStrings.{$ENDIF}StrLCopy (TH.Name, PAnsiChar (DirRec.Name), NAMSIZ);
  CASE DirRec.FileType of
    ftNormal, ftLink  : Mode := $08000;
    ftSymbolicLink    : Mode := $0A000;
    ftDirectory         : Mode := $04000;
    else                  Mode := 0;
    end;
  if tmSaveText in DirRec.Mode then Mode := Mode or $0200;
  if tmSetGid   in DirRec.Mode then Mode := Mode or $0400;
  if tmSetUid   in DirRec.Mode then Mode := Mode or $0800;
  if tpReadByOwner    in DirRec.Permissions then Mode := Mode or $0100;
  if tpWriteByOwner   in DirRec.Permissions then Mode := Mode or $0080;
  if tpExecuteByOwner in DirRec.Permissions then Mode := Mode or $0040;
  if tpReadByGroup    in DirRec.Permissions then Mode := Mode or $0020;
  if tpWriteByGroup   in DirRec.Permissions then Mode := Mode or $0010;
  if tpExecuteByGroup in DirRec.Permissions then Mode := Mode or $0008;
  if tpReadByOther    in DirRec.Permissions then Mode := Mode or $0004;
  if tpWriteByOther   in DirRec.Permissions then Mode := Mode or $0002;
  if tpExecuteByOther in DirRec.Permissions then Mode := Mode or $0001;
  OctalN (Mode, @TH.Mode, 8);
  OctalN (DirRec.UID, @TH.UID, 8);
  OctalN (DirRec.GID, @TH.GID, 8);
  Octal64 (DirRec.Size, @TH.Size, 12);
  NullDate := EncodeDate (1970, 1, 1);
  if DirRec.DateTime >= NullDate
    then Octal (Trunc ((DirRec.DateTime - NullDate) * 86400.0), @TH.MTime, 12)
    else Octal (Trunc (                   NullDate  * 86400.0), @TH.MTime, 12);
  CASE DirRec.FileType of
    ftNormal       : TH.LinkFlag := '0';
    ftLink         : TH.LinkFlag := '1';
    ftSymbolicLink : TH.LinkFlag := '2';
    ftCharacter    : TH.LinkFlag := '3';
    ftBlock        : TH.LinkFlag := '4';
    ftDirectory    : TH.LinkFlag := '5';
    ftFifo         : TH.LinkFlag := '6';
    ftContiguous   : TH.LinkFlag := '7';
    ftDumpDir      : TH.LinkFlag := 'D';
    ftMultiVolume  : TH.LinkFlag := 'M';
    ftVolumeHeader : TH.LinkFlag := 'V';
    end;
  {$IFNDEF FPC}AnsiStrings.{$ENDIF}StrLCopy (TH.LinkName, PAnsiChar (DirRec.LinkName), NAMSIZ);
  {$IFNDEF FPC}AnsiStrings.{$ENDIF}StrLCopy (TH.Magic, PAnsiChar (DirRec.Magic + #32#32#32#32#32#32#32#32), 7);
  {$IFNDEF FPC}AnsiStrings.{$ENDIF}StrLCopy (TH.UName, PAnsiChar (DirRec.UserName), TUNMLEN);
  {$IFNDEF FPC}AnsiStrings.{$ENDIF}StrLCopy (TH.GName, PAnsiChar (DirRec.GroupName), TGNMLEN);
  OctalN (DirRec.MajorDevNo, @TH.DevMajor, 8);
  OctalN (DirRec.MinorDevNo, @TH.DevMinor, 8);
  {$IFNDEF FPC}AnsiStrings.{$ENDIF}StrMove (TH.ChkSum, CHKBLANKS, 8);

  CheckSum := 0;
  FOR I := 0 TO SizeOf (TTarHeader)-1 do
    INC (CheckSum, integer (ORD (Rec [I])));
  OctalN (CheckSum, @TH.ChkSum, 8);

  Dest.write (TH, RECORDSIZE);
end;



(*
===============================================================================================
TTarArchive
===============================================================================================
*)

constructor TTarArchive.Create (Stream : TStream);
begin
  inherited Create;
  FStream     := Stream;
  FOwnsStream := FALSE;
end;


constructor TTarArchive.Create (Filename : String; FileMode : WORD);
begin
  inherited Create;
  FStream     := TFileStream.Create (Filename, FileMode);
  FOwnsStream := TRUE;
end;


destructor TTarArchive.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;


procedure TTarArchive.Reset;
          // Reset File pointer
begin
  FStream.Position := 0;
  FBytesToGo       := 0;
end;


function  TTarArchive.FindNext (var DirRec : TTarDirRec) : boolean;
          // Reads next Directory Info Record
          // The Stream pointer must point to the first byte of the tar header
var
  Rec          : array [0..RECORDSIZE-1] of CHAR;
  Header       : TTarHeader ABSOLUTE Rec;
  I            : integer;
  HeaderChkSum : WORD;
  Checksum     : CARDINAL;
begin
  // --- Scan until next pointer
  if FBytesToGo > 0 then
    FStream.Seek(Records (FBytesToGo) * RECORDSIZE, soCurrent);

  // --- EOF reached?
  Result := FALSE;
    FStream.ReadBuffer (Rec, RECORDSIZE);
    if Rec [0] = #0 then EXIT;   // EOF reached
  Result := TRUE;

  ClearDirRec (DirRec);

  DirRec.Name := ExtractText (Header.Name);
  DirRec.Size := ExtractNumber64 (@Header.Size, 12);
  DirRec.DateTime := EncodeDate (1970, 1, 1) + (ExtractNumber (@Header.MTime, 12) / 86400.0);
  I := ExtractNumber (@Header.Mode);
  if I and $0100 <> 0 then Include (DirRec.Permissions, tpReadByOwner);
  if I and $0080 <> 0 then Include (DirRec.Permissions, tpWriteByOwner);
  if I and $0040 <> 0 then Include (DirRec.Permissions, tpExecuteByOwner);
  if I and $0020 <> 0 then Include (DirRec.Permissions, tpReadByGroup);
  if I and $0010 <> 0 then Include (DirRec.Permissions, tpWriteByGroup);
  if I and $0008 <> 0 then Include (DirRec.Permissions, tpExecuteByGroup);
  if I and $0004 <> 0 then Include (DirRec.Permissions, tpReadByOther);
  if I and $0002 <> 0 then Include (DirRec.Permissions, tpWriteByOther);
  if I and $0001 <> 0 then Include (DirRec.Permissions, tpExecuteByOther);
  if I and $0200 <> 0 then Include (DirRec.Mode, tmSaveText);
  if I and $0400 <> 0 then Include (DirRec.Mode, tmSetGid);
  if I and $0800 <> 0 then Include (DirRec.Mode, tmSetUid);
  CASE Header.LinkFlag of
    #0, '0' : DirRec.FileType := ftNormal;
    '1'     : DirRec.FileType := ftLink;
    '2'     : DirRec.FileType := ftSymbolicLink;
    '3'     : DirRec.FileType := ftCharacter;
    '4'     : DirRec.FileType := ftBlock;
    '5'     : DirRec.FileType := ftDirectory;
    '6'     : DirRec.FileType := ftFifo;
    '7'     : DirRec.FileType := ftContiguous;
    'D'     : DirRec.FileType := ftDumpDir;
    'M'     : DirRec.FileType := ftMultiVolume;
    'V'     : DirRec.FileType := ftVolumeHeader;
    end;
  DirRec.LinkName   := ExtractText (Header.LinkName);
  DirRec.UID        := ExtractNumber (@Header.UID);
  DirRec.GID        := ExtractNumber (@Header.GID);
  DirRec.UserName   := ExtractText (Header.UName);
  DirRec.GroupName  := ExtractText (Header.GName);
  DirRec.Magic      := Trim (ExtractText (Header.Magic));
  DirRec.MajorDevNo := ExtractNumber (@Header.DevMajor);
  DirRec.MinorDevNo := ExtractNumber (@Header.DevMinor);

  HeaderChkSum := ExtractNumber (@Header.ChkSum);   // Calc Checksum
  CheckSum := 0;
  {$IFNDEF FPC}AnsiStrings.{$ENDIF}StrMove (Header.ChkSum, CHKBLANKS, 8);
  FOR I := 0 TO SizeOf (TTarHeader)-1 do
    INC (CheckSum, integer (ORD (Rec [I])));
  DirRec.CheckSumOK := WORD (CheckSum) = WORD (HeaderChkSum);

  if DirRec.FileType in [ftLink, ftSymbolicLink, ftDirectory, ftFifo, ftVolumeHeader]
    then FBytesToGo := 0
    else FBytesToGo := DirRec.Size;
end;


procedure TTarArchive.ReadFile (Buffer : pointer);
          // Reads file data for the last Directory Record. The entire file is read into the buffer.
          // The buffer must be large enough to take up the whole file.
var
  RestBytes : LongInt;
begin
  if FBytesToGo = 0 then EXIT;
  RestBytes := Records (FBytesToGo) * RECORDSIZE - FBytesToGo;
  FStream.ReadBuffer (Buffer^, FBytesToGo);
  FStream.Seek (RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;


procedure TTarArchive.ReadFile (Stream : TStream);
          // Reads file data for the last Directory Record.
          // The entire file is written out to the stream.
          // The stream is left at its current position prior to writing
var
  RestBytes : longint;
begin
  if FBytesToGo = 0 then EXIT;
  RestBytes := Records (FBytesToGo) * RECORDSIZE - FBytesToGo;
  Stream.CopyFrom (FStream, FBytesToGo);
  FStream.Seek (RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;


procedure TTarArchive.ReadFile (Filename : String);
          // Reads file data for the last Directory Record.
          // The entire file is saved in the given Filename
var
  FS : TFileStream;
begin
  FS := TFileStream.Create (Filename, fmCreate);
  TRY
    ReadFile (FS);
  FINALLY
    FS.Free;
    end;
end;


function  TTarArchive.ReadFile : TBytes;
          // Reads file data for the last Directory Record. The entire file is returned
          // as a large ANSI String.
var
  RestBytes : longint;
begin
  if FBytesToGo = 0 then EXIT;
  RestBytes := Records (FBytesToGo) * RECORDSIZE - FBytesToGo;
  SetLength (Result, FBytesToGo);
  FStream.ReadBuffer(Result, {$IFNDEF FPC}0, {$ENDIF}FBytesToGo);
  FStream.Seek (RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;


procedure TTarArchive.GetFilePos (var Current, Size : int64);
          // Returns the Current Position in the TAR stream
begin
  Current := FStream.Position;
  Size    := FStream.Size;
end;


procedure TTarArchive.SetFilePos (NewPos : int64);                   // Set new Current File Position
begin
  if NewPos < FStream.Size then
    FStream.Seek(NewPos, soBeginning);
end;


(*
===============================================================================================
TTarWriter
===============================================================================================
*)

constructor TTarWriter.CreateEmpty;
var
  TP : TTarPermission;
begin
  inherited Create;
  FOwnsStream  := FALSE;
  FFinalized   := FALSE;
  FPermissions := [];
  FOR TP := Low (TP) TO High (TP) do
    Include (FPermissions, TP);
  FUID       := 0;
  FGID       := 0;
  FUserName  := '';
  FGroupName := '';
  FMode      := [];
  FMagic     := 'ustar';
end;

constructor TTarWriter.Create (TargetStream   : TStream);
begin
  CreateEmpty;
  FStream     := TargetStream;
  FOwnsStream := FALSE;
end;


constructor TTarWriter.Create (TargetFilename : String; Mode : integer = fmCreate);
begin
  CreateEmpty;
  FStream     := TFileStream.Create (TargetFilename, Mode);
  FOwnsStream := TRUE;
end;


destructor TTarWriter.Destroy;
begin
  if NOT FFinalized then begin
    Finalize;
    FFinalized := TRUE;
    end;
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;


procedure TTarWriter.AddFile   (Filename : AnsiString;  TarFilename : AnsiString = '');
var
  S    : TFileStream;
  Date : TDateTime;
begin
  Date := FileTimeGMT (Filename);
  if TarFilename = '' then
    TarFilename := ConvertFilename (Filename)
  else
    TarFilename := ConvertFilename (TarFilename);
  S := TFileStream.Create (String(Filename), fmOpenRead or fmShareDenyWrite);
  TRY
    AddStream (S, TarFilename, Date);
  FINALLY
    S.Free
    end;
end;


procedure TTarWriter.AddStream (Stream : TStream; TarFilename : AnsiString; FileDateGmt : TDateTime);
var
  DirRec      : TTarDirRec;
  Rec         : array [0..RECORDSIZE-1] of CHAR;
  BytesToRead : int64;      // Bytes to read from the Source Stream
  BlockSize   : int64;      // Bytes to write out for the current record
begin
  ClearDirRec (DirRec);
  DirRec.Name        := TarFilename;
  DirRec.Size        := Stream.Size - Stream.Position;
  DirRec.DateTime    := FileDateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftNormal;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
  BytesToRead := DirRec.Size;
  while BytesToRead > 0 do begin
    BlockSize := BytesToRead;
    if BlockSize > RECORDSIZE then BlockSize := RECORDSIZE;
    FillChar (Rec, RECORDSIZE, 0);
    Stream.read (Rec, BlockSize);
    FStream.write (Rec, RECORDSIZE);
    DEC (BytesToRead, BlockSize);
    end;
end;


procedure TTarWriter.AddString (Contents : AnsiString; TarFilename : AnsiString; FileDateGmt : TDateTime); // rawbytestring
var
  S : TStringStream;
begin
  S := TStringStream.Create (Contents);
  TRY
    AddStream (S, TarFilename, FileDateGmt);
  FINALLY
    S.Free
    END
end;


procedure TTarWriter.AddDir (Dirname : AnsiString; DateGmt : TDateTime; MaxDirSize : int64 = 0);
var
  DirRec      : TTarDirRec;
begin
  ClearDirRec (DirRec);
  DirRec.Name        := Dirname;
  DirRec.Size        := MaxDirSize;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftDirectory;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
end;


procedure TTarWriter.AddSymbolicLink (Filename, Linkname : AnsiString; DateGmt : TDateTime);
var
  DirRec : TTarDirRec;
begin
  ClearDirRec (DirRec);
  DirRec.Name        := Filename;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftSymbolicLink;
  DirRec.LinkName    := Linkname;
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
end;


procedure TTarWriter.AddLink (Filename, Linkname : AnsiString; DateGmt : TDateTime);
var
  DirRec : TTarDirRec;
begin
  ClearDirRec (DirRec);
  DirRec.Name        := Filename;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftLink;
  DirRec.LinkName    := Linkname;
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
end;


procedure TTarWriter.AddVolumeHeader (VolumeId           : AnsiString; DateGmt : TDateTime);
var
  DirRec : TTarDirRec;
begin
  ClearDirRec (DirRec);
  DirRec.Name        := VolumeId;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftVolumeHeader;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader (FStream, DirRec);
end;


procedure TTarWriter.Finalize;
          // Writes the End-of-File Tag
          // Data after this tag will be ignored
          // The destructor calls this automatically if you didn't do it before
var
  Rec : array [0..RECORDSIZE-1] of CHAR;
begin
  FillChar (Rec, SizeOf (Rec), 0);
  FStream.write (Rec, RECORDSIZE);
  {
    Avoid warning: 'tar: A lone zero block at *'
    The reason for this message is that GNU tar format has been changed
    to require TWO zero blocks marking the end of the archive.
    Thus write a second zero block.
  }
  FStream.write (Rec, RECORDSIZE);
  FFinalized := TRUE;
end;





const
  MIME_TRANSFERENCODING = 'Content-Transfer-Encoding';
  MIME_DEFAULT_START = 'uuid:{FF461456-FE30-4933-9AF6-F8EB226E1BF7}';
  MIME_DEFAULT_BOUNDARY = 'MIME_boundary';
  MIME_ID = 'Content-ID';
  EOL_WINDOWS = CR + LF;
  EOL_PLATFORM = EOL_WINDOWS;
  CONTENT_TYPE = 'Content-Type';
  CONTENT_DISPOSITION = 'Content-Disposition';
  MULTIPART_RELATED : AnsiString = 'multipart/related';
  MULTIPART_FORMDATA : AnsiString = 'multipart/form-data';
  MIME_BOUNDARY : AnsiString = 'boundary';
  MIME_START : AnsiString = 'start';
  MIME_TYPE : AnsiString = 'type';

{ Stream Readers }

function TMimeBase.ReadToValue(AStream : TStream; AValue : AnsiString):AnsiString;
var
  c : AnsiChar;
begin
  result := '';
  while copy(result, length(result)-length(AValue)+1, length(AValue)) <> AValue do
    begin
    CheckCondition(AStream.Size - AStream.Position <> 0, 'ReadToValue', 'Premature termination of stream looking for value "'+string(AValue)+'"');
    AStream.Read(c, 1);
    result := result + c;
    end;
  delete(result, length(result)-length(AValue)+1, length(AValue));
end;

function TMimeBase.ReadBytes(AStream : TStream; AByteCount : Integer):AnsiString;
begin
  CheckCondition(AStream.Size - AStream.Position >= AByteCount, 'ReadBytes', 'Premature termination of stream reading "'+inttostr(AByteCount)+'" bytes');
  SetLength(result, AByteCount);
  if AByteCount > 0 then
    begin
    AStream.Read(result[1], AByteCount);
    end;
end;

{ Stream Writers }

procedure WriteString(AStream : TStream; Const AStr : AnsiString);
begin
  If AStr <> '' then
    begin
    AStream.Write(AStr[1], length(AStr));
    end;
end;

{ TMimeBase }

constructor TMimeBase.create;
begin
  inherited;
  FHeaders := TIdHeaderList.create(QuotePlain);
end;

destructor TMimeBase.destroy;
begin
  FreeAndNil(FHeaders);
  inherited;
end;

procedure TMimeBase.ReadHeaders(AStream: TStream);
var
  LHeader : AnsiString;
  LFound : Boolean;
begin
  LFound := false;
  repeat
    LHeader := ReadToValue(AStream, EOL);
    if LHeader <> '' then
      begin
      LFound := true;
      FHeaders.Add(string(LHeader));
      end
  until LFound and (LHeader = '');
end;

procedure TMimeBase.WriteHeaders(AStream: TStream);
var
  i : integer;
begin

  For i := 0 to FHeaders.Count - 1 do
    begin
    WriteString(AStream, ansiString(FHeaders[i])+EOL);
    end;
  WriteString(AStream, EOL);
end;

{ TMimePart }

constructor TMimePart.create;
begin
  inherited;
  FContent := TFslBuffer.create;
end;

destructor TMimePart.destroy;
begin
  FContent.Free;
  inherited;
end;

function TMimePart.FileName: String;
var
  s : String;
begin
  s := Headers.Values['Content-Disposition'];
  StringSplit(s, ';', s, result);
  if (s = 'form-data') and result.Contains('filename="') then
  begin
    result := result.Substring(result.IndexOf('filename="')+10);
    result := copy(result, 1, pos('"', result)-1);
  end
  else
    result := '';

end;

procedure TMimePart.DecodeContent;
var
  LCnt : String;
begin
  LCnt := FHeaders.Values[MIME_TRANSFERENCODING];

  // possible values (rfc 2045):
  // "7bit" / "8bit" / "binary" / "quoted-printable" / "base64"
  // and extendible with ietf-token / x-token

  // we only process base64. everything else is considered to be binary
  // (this is not an email processor). Where this is a problem, notify
  // the indysoap team *with an example*, and this will be extended
  if AnsiSameText(LCnt, 'base64') then
    begin
    raise ETodo.create('TMimePart.DecodeContent');
//    Content := Base64Decode(Content);
    end
  else
    begin
    // well, we leave the content unchanged
    end;
end;

procedure TMimePart.ReadFromStream(AStream: TStream; ABoundary: AnsiString);
var
  LBuffer : pansichar;
  LEnd : word;
  LComp0 : Pointer;
  LComp1 : Pointer;
  LCompLen : Word;
  offset : integer;
  b : TBytes;
const
  BUF_LEN = 1024;
begin

  ReadHeaders(AStream);
  FId := FHeaders.Values[MIME_ID];
  if (FId <> '') and (FId[1] = '<') then
    delete(FId, 1, 1);
  if (FId <> '') and (FId[length(fId)] = '>') then
    delete(FId, length(FId), 1);

  // do this fast
  GetMem(LBuffer, BUF_LEN);
  try
    FillChar(LBuffer^, BUF_LEN, 0);
    LEnd := 0;
    LCompLen := length(ABoundary);
    LComp1 := pAnsichar(ABoundary);
    while true do
      begin
      if LEnd = BUF_LEN-1 then
        begin
        offset := length(b);
        SetLength(b, offset + LEnd - LCompLen);
        move(LBuffer^, b[offset], LEnd - LCompLen);
        move(LBuffer[LEnd - LCompLen], LBuffer[0], LCompLen);
        LEnd := LCompLen;
        FillChar(LBuffer[LEnd], BUF_LEN - LEnd, 0);
        end
      else
        begin
        AStream.Read(LBuffer[LEnd], 1);
        inc(LEnd);
        end;
      LComp0 := pointer(NativeUInt(LBuffer)+LEnd-LCompLen);
      if (LEnd >= LCompLen) and CompareMem(LComp0, LComp1, LCompLen) then
        begin
        offset := length(b);
        SetLength(b, offset + LEnd - (LCompLen + 2 + 2));// -2 for the EOL, +2 for the other EOL
        if (length(b) > offset) then
          move(LBuffer^, b[offset], LEnd - (LCompLen + 2 + 2));
        FContent.AsBytes := b;
        break;
        end;
      end;
  finally
    FreeMem(LBuffer);
  end;
  DecodeContent;
end;

procedure TMimePart.SetContent(const AValue: TFslBuffer);
begin
  FContent.Free;
  FContent := AValue;
end;

procedure TMimePart.WriteToStream(AStream: TStream);
var
  LTemp : AnsiString;
begin

  WriteHeaders(AStream);
  if FHeaders.Values[MIME_TRANSFERENCODING] = 'base64' then
    begin
    raise ETodo.create('TMimePart.WriteToStream#1');
//    WriteString(AStream, Base64EncodeAnsi(FContent, true)+EOL_WINDOWS);
    end
  else
    begin
    raise ETodo.create('TMimePart.WriteToStream#2');

    //AStream.CopyFrom(FContent, (FContent.Size - FContent.Position)-2);
//    if FContent.Size - FContent.Position >= 2 then
//      LTemp := ReadBytes(FContent, 2)
//    else
//      LTemp := '';
    WriteString(AStream, LTemp);
    if LTemp <> EOL_WINDOWS then
      begin
      WriteString(AStream, EOL_WINDOWS);
      end;
    end;
end;

function TMimePart.GetMediaType: String;
begin
  result := FHeaders.Values[CONTENT_TYPE];
end;

function TMimePart.GetTransferEncoding: String;
begin
  result := FHeaders.Values[MIME_TRANSFERENCODING];
end;

function TMimePart.Link: TMimePart;
begin
  result := TMimePart(inherited Link);
end;

Function StringSplit(Const sValue, sDelimiter : String; Var sLeft, sRight: String) : Boolean;
Var
  iIndex : Integer;
  sA, sB : String;
Begin
  // Find the delimiter within the source string
  iIndex := Pos(sDelimiter, sValue);
  Result := iIndex <> 0;

  If Not Result Then
  Begin
    sA := sValue;
    sB := '';
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + Length(sDelimiter), MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;

function StartsWith(s, test : String):Boolean;
begin
  result := lowercase(copy(s, 1, length(test))) = lowercase(test);
end;

function TMimePart.ParamName: String;
var
  s : String;
begin
  s := Headers.Values['Content-Disposition'];
  StringSplit(s, ';', s, result);
  if (s = 'form-data') and StartsWith(trim(result), 'name="') then
  begin
    result := copy(result, 8, $FFFF);
    result := copy(result, 1, pos('"', result)-1);
  end
  else
    result := '';
end;

procedure TMimePart.SetMediaType(const AValue: String);
begin
  FHeaders.Values[CONTENT_TYPE] := AValue;
end;

procedure TMimePart.SetTransferEncoding(const AValue: String);
begin
  FHeaders.Values[MIME_TRANSFERENCODING] := AValue;
end;

procedure TMimePart.SetId(const AValue: String);
begin
  FId := AValue;
  FHeaders.Values[MIME_ID] := '<'+AValue+'>';
end;

function TMimePart.GetContentDisposition: String;
begin
  result := FHeaders.Values[CONTENT_DISPOSITION];
end;

procedure TMimePart.SetContentDisposition(const sValue: String);
begin
  FHeaders.Values[CONTENT_DISPOSITION] := sValue;
end;

(*{ TMimePartList }

function TMimePart.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContent.sizeInBytes);
  inc(result, (FId.length * sizeof(char)) + 12);
end;

function TMimePartList.GetPartByIndex(i: integer): TMimePart;
begin
  result := Items[i] as TMimePart;
end;

function TMimePartList.GetPart(AName : String): TMimePart;
var
  i : integer;
  s : String;
begin
  if (AName <> '') and (AName[1] = '<') then
    System.delete(AName, 1, 1);
  if (AName <> '') and (AName[length(AName)] = '>') then
    System.delete(AName, length(AName), 1);

  result := nil;
  for i := 0 to Count - 1 do
    begin
    s := (Items[i] as TMimePart).Id;
    if s = AName then
      begin
      result := Items[i] as TMimePart;
      exit;
      end
    end;
  Condition(False, 'Part "'+AName+'" not found in parts '+CommaList);
end;

function TMimePartList.CommaList: String;
var
  i : integer;
begin
  result := '';
  for i := 0 to Count -1 do
    begin
    result := CommaAdd(result, (Items[i] as TMimePart).Id);
    end;
end;

function TMimePartList.AddPart(AId: String): TMimePart;
begin
  result := TMimePart.create;
  result.Id := AId;
  add(result);
end;
  *)

{ TMimeMessage }

function IdAnsiSameText(const S1, S2: ansistring): Boolean;
begin
  Result := AnsiCompareText(String(S1), String(S2)) = 0;
end;


function TMimeMessage.AddPart(id: String): TMimePart;
begin
  result := TMimePart.create;
  FParts.Add(result);
  result.Id := id;
end;

procedure TMimeMessage.AnalyseContentType(AContent: String);
var
  s, l, r, id : AnsiString;
begin
  // this parser is weak - and needs review
  StringSplitAnsi(AnsiString(Trim(AContent)), ';', l, s);
  CheckCondition(IdAnsiSameText(l, MULTIPART_RELATED) or IdAnsiSameText(l, MULTIPART_FORMDATA), 'AnalyseContentType', 'attempt to read content as Mime, but the content-Type is "'+String(l)+'", not "'+String(MULTIPART_RELATED)+'" or "'+String(MULTIPART_FORMDATA)+'" in header '+String(AContent));
  while s <> '' do
    begin
    StringSplitAnsi(s, ';',l, s);
    StringSplitAnsi(AnsiString(trim(String(l))), '=', l, r);
    CheckCondition(l <> '', 'AnalyseContentType', 'Unnamed part in Content_type header '+AContent);
    CheckCondition(r <> '', 'AnalyseContentType', 'Unvalued part in Content_type header '+AContent);
    if r[1] = '"' then
      begin
      delete(r, 1, 1);
      end;
    if r[length(r)] = '"' then
      begin
      delete(r, length(r), 1);
      end;
    if AnsiSameText(string(l), string(MIME_BOUNDARY)) then
      begin
      FBoundary := r;
      end
    else if IdAnsiSameText(l, MIME_START) then
      begin
      id := r;
      if (id <> '') and (id[1] = '<') then
        delete(id, 1, 1);
      if (id <> '') and (id[length(id)] = '>') then
        delete(id, length(id), 1);
      FStart := string(id);
      end
    else if IdAnsiSameText(l, MIME_TYPE) then
      begin
      FMainType := String(r);
      end;
    end;
end;

constructor TMimeMessage.create;
begin
  inherited create;
  FParts := TFslList<TMimePart>.create;
end;

destructor TMimeMessage.destroy;
begin
  FParts.Free;
  inherited;
end;

procedure TMimeMessage.ReadFromStream(AStream: TStream);
var
  LHeader : String;
begin
  ReadHeaders(AStream);

  LHeader := FHeaders.Values[CONTENT_TYPE];
  CheckCondition(LHeader <> '', 'ReadFromStream', ''+CONTENT_TYPE+' header not found in '+FHeaders.CommaText);
  ReadFromStream(AStream, LHeader);
end;

function TMimeMessage.GetMainPart: TMimePart;
begin
  CheckCondition(FStart <> '', 'GetMainPart', 'Start header not valid');
  result := getparam(FStart);
end;

function TMimeMessage.getparam(name: String): TMimePart;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Parts.Count - 1 do
    if Parts[i].ParamName = name then
    begin
      result := Parts[i];
      exit;
    end;
end;

function TMimeMessage.hasParam(name: String): Boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Parts.Count - 1 do
    result := result or (Parts[i].ParamName = name);
end;

function TMimeMessage.Link: TMimeMessage;
begin
  result := TMimeMessage(inherited Link);
end;

procedure TMimeMessage.ReadFromStream(AStream: TStream; AContentType: String);
var
  LTemp : AnsiString;
  LPart : TMimePart;
begin
  CheckCondition(AContentType <> '', 'ReadFromStream', 'Content-Type header not valid');
  AnalyseContentType(AContentType);

  LTemp := ReadToValue(AStream, FBoundary);
  // that was getting going. usually LTemp would be empty, but we will throw it away
  repeat
    LTemp := ReadBytes(AStream, 2);
    if LTemp = EOL then
      begin
      LPart := TMimePart.create;
      try
        LPart.ReadFromStream(AStream, FBoundary);
      except
        FreeAndNil(LPart);
        raise;
      end;
      FParts.Add(LPart);
      end
  until LTemp = '--';
end;

function TMimeMessage.GetContentTypeHeader: String;
begin

  result := String(MULTIPART_RELATED)+'; type="application/xop+xml"; '+String(MIME_START)+'="'+FStart+'"; start-info="application/soap+xml"; '+String(MIME_BOUNDARY)+'="'+String(FBoundary)+'"; action="urn:ihe:iti:2007:ProvideAndRegisterDocumentSet-b"';
  if FMainType <> '' then
    begin
    result := result + '; '+String(MIME_TYPE)+'="'+FMainType+'"';
    end;
// oracle   Content-Type: multipart/related; type="application/xop+xml"; start="<rootpart@soapui.org>"; start-info="application/soap+xml"; action="ProvideAndRegisterDocumentSet-b"; boundary="----=_Part_2_2098391526.1311207545005"

end;

procedure TMimeMessage.Validate;
var
  i, j : integer;
  LFound : boolean;
begin
  CheckCondition(FBoundary <> '', 'Validate', 'Boundary is not valid');
  CheckCondition(FStart <> '', 'Validate', 'Start is not valid');
  LFound := false;
  for i := 0 to FParts.Count - 1 do
    begin
    CheckCondition(FParts[i].Id <> '', 'Validate', 'Part['+inttostr(i)+'].Id is not valid');
    LFound := LFound or (FParts[i].Id = FStart);
    for j := 0 to i - 1 do
      begin
      CheckCondition(FParts[i].Id <> FParts[j].Id, 'Validate', 'Part['+inttostr(i)+'].Id is a duplicate of Part['+inttostr(j)+'].Id ("'+FParts[i].Id+'")');
      end;
    end;
  CheckCondition(LFound, 'Validate', 'The Start Part "'+FStart+'" was not found in the part list');
end;

procedure TMimeMessage.WriteToStream(AStream: TStream; AHeaders: boolean);
var
  i : integer;
begin
  Validate;

  if AHeaders then
    begin
    WriteHeaders(AStream);
    end;
  for i := 0 to FParts.Count - 1 do
    begin
    WriteString(AStream, '--'+FBoundary+EOL);
    FParts[i].WriteToStream(AStream);
    end;
  WriteString(AStream, '--'+FBoundary+'--');
end;

Constructor TFslByteExtractor.Create;
Begin
  Inherited;
  FBuilder := TFslBytesBuilder.Create;
End;

Destructor TFslByteExtractor.Destroy;
Begin
  FBuilder.Free;
  Inherited;
End;


Function TFslByteExtractor.ConsumeLine : TBytes;
Begin
  Result := ConsumeUntilBytes(Eolns);

  ConsumeWhileBytes(Eolns);
End;


Procedure TFslByteExtractor.Clear;
Begin
  Inherited;

  FCache := '';
End;



Function TFslByteExtractor.NextByte : TByte;
Begin
  If Length(FCache) = 0 Then
  Begin
    Stream.Read(Result, 1);
    FCache := AnsiChar(Result);
  End
  Else
  Begin
    Result := TByte(FCache[1]);
  End;
End;


Function TFslByteExtractor.ConsumeByte : TByte;
Begin
  Result := NextByte;

  Delete(FCache, 1, 1);
End;


Function TFslByteExtractor.ConsumeByteCount(Const iByteCount : Integer) : TBytes;
Var
  iLoop : Integer;
Begin
  SetLength(Result, iByteCount);

  For iLoop := 1 To iByteCount Do
    Result[iLoop] := ConsumeByte;
End;


Function TFslByteExtractor.ConsumeUntilBytes(Const iTokens: TBytes): TBytes;
Begin
  FBuilder.Clear;
  While More And Not BytesContains(iTokens, NextByte) Do
    FBuilder.Append(ConsumeByte);
  Result := FBuilder.AsBytes;
End;


Function TFslByteExtractor.ConsumeUntilByte(Const iToken : TByte) : TBytes;
Begin
  FBuilder.Clear;
  While More And (NextByte <> iToken) Do
    FBuilder.Append(ConsumeByte);
  Result := FBuilder.AsBytes;
End;


Function TFslByteExtractor.ConsumeRestStream : TBytes;
Begin
  FBuilder.Clear;
  While More Do
    FBuilder.Append(ConsumeByte);
  Result := FBuilder.AsBytes;
End;


Function TFslByteExtractor.ConsumeWhileByte(Const iToken : TByte) : TBytes;
Begin
  FBuilder.Clear;
  While More And (NextByte = iToken) Do
    FBuilder.Append(ConsumeByte);
  Result := FBuilder.AsBytes;
End;


Function TFslByteExtractor.ConsumeWhileBytes(Const iTokens : TBytes) : TBytes;
Begin
  FBuilder.Clear;
  While More And BytesContains(iTokens, NextByte) Do
    FBuilder.Append(ConsumeByte);
  Result := FBuilder.AsBytes;
End;



Function TFslByteExtractor.CacheLength: Integer;
Begin
  Result := Length(FCache);
End;


Function TFslByteExtractor.StreamPosition: Int64;
Begin
  Assert(Invariants('StreamPosition', Stream, TFslAccessStream, 'Stream'));

  Result := TFslAccessStream(Stream).Position - CacheLength;
End;


Function TFslByteExtractor.More : Boolean;
Begin
  Result := Inherited More Or (Length(FCache) > 0);
End;

{ TSourceLocation }

class function TSourceLocation.Create(l, c: integer): TSourceLocation;
begin
  result.line := l;
  result.col := c;
end;

function TSourceLocation.nonZero: boolean;
begin
  result := (line > 0) or (col > 0);
end;

class function TSourceLocation.Create : TSourceLocation;
begin
  result := Create(0, 0);
end;


class function TSourceLocation.CreateNull: TSourceLocation;
begin
  result := Create(-1, -1);
end;

class function TSourceLocation.min(src1, src2 : TSourceLocation) : TSourceLocation;
begin
  if (src1.line < src2.line) then
    result := src1
  else if (src2.line < src1.line) then
    result := src2
  else if (src1.col < src2.col) then
    result := src1
  else
    result := src2
end;

class function TSourceLocation.max(src1, src2 : TSourceLocation) : TSourceLocation;
begin
  if (src1.line > src2.line) then
    result := src1
  else if (src2.line > src1.line) then
    result := src2
  else if (src1.col > src2.col) then
    result := src1
  else
    result := src2
end;

function TSourceLocation.isNull : boolean;
begin
  result := (line = -1) and (col = -1);
end;

class operator TSourceLocation.equal(src1, src2 : TSourceLocation) : boolean;
begin
  if src1.isNull and src2.isNull then
    result := true
  else if src1.isNull or src2.isNull then
    result := false
  else
    result := (src1.line = src2.line) and (src1.col = src2.col);
end;

class operator TSourceLocation.notEqual(src1, src2 : TSourceLocation ) : boolean;
begin
  if src1.isNull and src2.isNull then
    result := false
  else if src1.isNull or src2.isNull then
    result := true
  else
    result := not ((src1.line = src2.line) and (src1.col = src2.col));
end;

class operator TSourceLocation.lessThan(src1, src2 : TSourceLocation ) : boolean;
begin
  if src1.line < src2.line then
    result := true
  else if src1.line > src2.line then
    result := false
  else
    result := src1.col < src2.col;
end;

class operator TSourceLocation.greaterThan(src1, src2 : TSourceLocation ) : boolean;
begin
  if src1.line > src2.line then
    result := true
  else if src1.line < src2.line then
    result := false
  else
    result := src1.col > src2.col;
end;

class operator TSourceLocation.lessThanOrEqual(src1, src2 : TSourceLocation) : boolean;
begin
  if src1.line < src2.line then
    result := true
  else if src1.line > src2.line then
    result := false
  else
    result := src1.col <= src2.col;
end;

class operator TSourceLocation.greaterThanOrEqual(src1, src2 : TSourceLocation) : boolean;
begin
  if src1.line > src2.line then
    result := true
  else if src1.line < src2.line then
    result := false
  else
    result := src1.col >= src2.col;
end;

function TSourceLocation.inSpan(lower, upper : TSourceLocation) : boolean;
begin
  result := (self >= lower) and (self <= upper);
end;

function TSourceLocation.toPoint : TPoint;
begin
  result.x := col + 1;
  result.y := line + 1;
end;

procedure TSourceLocation.incCol;
begin
  inc(col);
end;

procedure TSourceLocation.incLine;
begin
  inc(line);
  col := 0;
end;

function TSourceLocation.lineForHuman : integer;
begin
  result := line + 1;
end;

function TSourceLocation.checkChar(ch: char; last13: boolean): boolean;
begin
  if (ch = #13) then
  begin
    incLine();
    exit(true);
  end
  else if (ch = #10) then
  begin
    if (not last13) then
      incLine();
    exit(false);
  end
  else
  begin
    incCol;
    exit(false);
  end;
end;

function TSourceLocation.colForHuman : integer;
begin
  result := col + 1;
end;

function TSourceLocation.describe : String;
begin
  if isNull then
    result := 'Unknown Location'
  else
    result := 'Line '+inttostr(lineForHuman)+', Column '+inttostr(colForHuman);
end;

function TSourceLocation.exception(msg: String) : EParserException;
begin
  result := EParserException.create(msg + ' at '+describe, self);
end;

class operator TSourceLocation.add(position : TSourceLocation; range : TSourceRange) : TSourceLocation;
begin
  if position.isNull or range.isNull then
    result := TSourceLocation.CreateNull
  else if (range.lines = 0) then
    result := TSourceLocation.Create(position.line, position.col + range.cols)
  else
    result := TSourceLocation.Create(position.line + range.lines, range.cols)
end;

class operator TSourceLocation.subtract(start, finish : TSourceLocation) : TSourceRange;
begin
  if start.isNull or finish.isNull then
    result := TSourceRange.CreateNull
  else if start.line > finish.line then
    result := TSourceRange.CreateNull
  else if start.line < finish.line then
    result := TSourceRange.Create(finish.line - start.line, finish.col)
  else if start.col > finish.col then
    result := TSourceRange.CreateNull
  else
    result := TSourceRange.Create(0, finish.col - start.col);
end;

class operator TSourceLocation.subtract(position : TSourceLocation; range : TSourceRange) : TSourceLocation;
begin
  if position.isNull or range.isNull then
    result := TSourceLocation.CreateNull
  else if range.lines = 0 then
  begin
    if range.cols < position.col then
      result := TSourceLocation.CreateNull
    else
      result := TSourceLocation.Create(position.line, position.col - range.cols);
  end
  else if range.lines > position.line then
    result := TSourceLocation.CreateNull
  else
    result := TSourceLocation.Create(position.line - range.lines, 0);
end;

class function TSourceLocation.fromPoint(p : TPoint) : TSourceLocation;
begin
  result := Create(p.y-1, p.x-1);
end;

{ EParserException }

function EParserException.GetColNumber: integer;
begin
  result := Location.col;
end;

function EParserException.GetLineNumber: integer;
begin
  result := Location.line;
end;

constructor EParserException.Create(msg: String; Location: TSourceLocation);
begin
  Inherited Create(msg);
  FLocation := location;
end;

Procedure Init;
Begin
  Eolns := Bytes([10, 11, 13]);
End;


Initialization
  Init;
End.

