unit FastMMMemoryModule;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  * Memory DLL loading code
  * ------------------------
  *
  * Original C Code
  * Memory DLL loading code
  * Version 0.0.4
  *
  * Copyright ( c ) 2004-2015 by Joachim Bauch / mail@joachim-bauch.de
  * http://www.joachim-bauch.de
  *
  * The contents of this file are subject to the Mozilla Public License Version
  * 2.0 ( the "License" ); you may not use this file except in compliance with
  * the License. You may obtain a copy of the License at
  * http://www.mozilla.org/MPL/
  *
  * Software distributed under the License is distributed on an "AS IS" basis,
  * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  * for the specific language governing rights and limitations under the
  * License.
  *
  * The Original Code is MemoryModule.c
  *
  * The Initial Developer of the Original Code is Joachim Bauch.
  *
  * Portions created by Joachim Bauch are Copyright ( C ) 2004-2015
  * Joachim Bauch. All Rights Reserved.
  *
  * ================== MemoryModule "Conversion to Delphi" ==================
  *
  * Copyright ( c ) 2015 by Fr0sT / https://github.com/Fr0sT-Brutal
  *
  * Initially based on the code by:
  *   Copyright ( c ) 2005 - 2006 by Martin Offenwanger / coder@dsplayer.de / http://www.dsplayer.de
  *   Carlo Pasolini / cdpasop@hotmail.it / http://pasotech.altervista.org
  *
  * NOTE
  *   This code is Delphi translation of original C code taken from https://github.com/fancycode/MemoryModule
  *     ( commit dc173ca from Mar 1, 2015 ).
  *   Resource loading and exe loading, custom functions, user data not implemented yet.
  *   Tested under RAD Studio XE2 and XE6 32/64-bit, Lazarus 32-bit
  * }

interface

{$DEFINE FastMM4} // FastMM4 Version
{.$DEFINE FastMM5} // FastMM5 Version
{.$DEFINE LZMA}

{$DEFINE MANIFEST} // WinSxS Support
{$DEFINE ALLOW_LOAD_FILES}
{$DEFINE LOAD_FROM_RESOURCE} // MS add support to extract dlls from resource to load via LoadLibrary
{$IF Defined( LOAD_FROM_RESOURCE ) AND ( NOT Defined( FastMM4 ) AND NOT Defined( FastMM5 ) )}
  {$DEFINE USE_STREAMS} 
{$IFEND}

{$DEFINE GetModuleHandle_BuildImportTable} // Try GetModuleHandle for BuildImportTable

{$DEFINE GetModuleHandle}
{$IF Defined( GetModuleHandle ) AND ( NOT Defined( FastMM4 ) AND NOT Defined( FastMM5 ) )}
  {$DEFINE UnloadAllOnFinalize} // Unload all Modules during Finalization of this Unit
  {$DEFINE GetModuleHandleCriticalSection} // Thread-Safe
{$IFEND GetModuleHandle}

{$IFDEF MANIFEST}
  {$OPTIMIZATION OFF}
{$ENDIF}

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN COMBINING_SIGNED_UNSIGNED OFF}

// To compile under FPC, Delphi mode must be used
// Also define CPUX64 for simplicity
{$IFDEF FPC}
  {$mode delphi}
  {$IFDEF CPU64}
    {$DEFINE CPUX64}
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
{$IF CompilerVersion >= 23}
  {$LEGACYIFEND ON}
  {$WARN IMPLICIT_STRING_CAST OFF}
{$ELSE}
  {$RANGECHECKS OFF} // RangeCheck might cause Internal-Error C1118
{$IFEND}
{$ENDIF}

uses
  Windows;

type
  {$IF NOT DECLARED( PIMAGE_NT_HEADERS )}
  PIMAGE_NT_HEADERS = ^IMAGE_NT_HEADERS;
  {$IFEND}

  TMemoryModuleModules = record
    Handle : HMODULE;
    {$IFDEF GetModuleHandle_BuildImportTable}
    Free   : boolean;
    {$ENDIF GetModuleHandle_BuildImportTable}
  end;

  TMemoryModule = record
    headers     : PIMAGE_NT_HEADERS;
    codeBase    : Pointer; // ModuleHandle
    modules     : array of TMemoryModuleModules;
    initialized : Boolean;
    isRelocated : Boolean;
    pageSize    : Cardinal;
  end;
  PMemoryModule = ^TMemoryModule;  

  { ++++++++++++++++++++++++++++++++++++++++++++++++++
    ***  Memory DLL loading functions Declaration  ***
    -------------------------------------------------- }

// return value is nil if function fails
function MemoryLoadLibrary( data: Pointer; var Module : PMemoryModule ): ShortInt; stdcall; {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}overload;{$IFEND}
{$IFDEF ALLOW_LOAD_FILES}
function MemoryLoadLibraryFile( FileName : string; var Module : PMemoryModule ): ShortInt; stdcall;
{$ENDIF ALLOW_LOAD_FILES}
{$IFDEF LOAD_FROM_RESOURCE}
function MemoryLoadLibrary( ResourceName : string; var Module : PMemoryModule{$IFDEF USE_STREAMS}; Password : string = ''{$ENDIF} ): ShortInt; stdcall; overload;
function MemoryResourceExists( var ResourceName : string ) : HRSRC;
{$ENDIF LOAD_FROM_RESOURCE}

{$IFDEF GetModuleHandle}
function MemoryGetModuleHandle( data: Pointer ): PMemoryModule; stdcall; {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}overload;{$IFEND}
{$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
function MemoryGetModuleHandle( FileName : string{$IFDEF LOAD_FROM_RESOURCE}; IsResource : boolean = False{$ENDIF} ): PMemoryModule; stdcall; overload;
{$IFEND}
{$ENDIF GetModuleHandle}

// return value is nil if function fails
function MemoryGetProcAddress( module: PMemoryModule; const name: PAnsiChar ): Pointer; stdcall;
// free module
procedure MemoryFreeLibrary( var module: PMemoryModule ); stdcall;

function MemoryEnumerateImports( data: Pointer; var Modules : string; DelimiterString : String = ';' ): ShortInt; stdcall; overload;
{$IFDEF ALLOW_LOAD_FILES}
function MemoryEnumerateImportsFile( FileName : string; var Modules : string; DelimiterString : String = ';' ): ShortInt; stdcall; overload;
{$ENDIF}
{$IFDEF LOAD_FROM_RESOURCE}
function MemoryEnumerateImports( ResourceName : string; var Modules : string; DelimiterString : String = ';'{$IFDEF USE_STREAMS}; Password : string = ''{$ENDIF} ): ShortInt; stdcall; overload;
{$ENDIF}

implementation

uses
  {$IF Defined( FastMM4 ) OR Defined( FastMM5 )}
  ZLibMinimal,	
	{$IFDEF FastMM5}FastMM5{$ELSE}FastMM4{$ENDIF}	
  {$ELSE}
  ZLib
  {$IFEND}
  {$IFDEF lzma},LZMA, LZMA2{$ENDIF}
  {$IF ( Defined( GetModuleHandle ) AND Defined( GetModuleHandleCriticalSection ) )},SyncObjs{$IFEND}
  {$IFDEF USE_STREAMS},Classes, JclCompression{$ENDIF}
  ;

  { ++++++++++++++++++++++++++++++++++++++++
    ***  Missing Windows API Definitions ***
    ---------------------------------------- }
type
  {$IF CompilerVersion < 21}
  NativeUInt = Cardinal;
  NativeInt  = Integer;
  IntPtr     = NativeInt;  
  {$IFEND}

  {$IF NOT DECLARED(PULONGLONG)}
  PULONGLONG = ^UINT64;
  {$IFEND}

  {$IFDEF FastMM4}
  PByte = System.PByte;
  {$ENDIF FastMM4}

  {$IF NOT DECLARED( IMAGE_BASE_RELOCATION )}
  {$ALIGN 4}
  IMAGE_BASE_RELOCATION = record
    VirtualAddress : Cardinal;
    SizeOfBlock    : Cardinal;
  end;
  {$ALIGN ON}
  PIMAGE_BASE_RELOCATION = ^IMAGE_BASE_RELOCATION;
  {$IFEND}

  // Types that are declared in Pascal-style ( ex.: PImageOptionalHeader ); redeclaring them in C-style
  {$IF NOT DECLARED( PIMAGE_DATA_DIRECTORY )}
  PIMAGE_DATA_DIRECTORY = ^IMAGE_DATA_DIRECTORY;
  {$IFEND}

  {$IF NOT DECLARED( PIMAGE_SECTION_HEADER )}
  PIMAGE_SECTION_HEADER = ^IMAGE_SECTION_HEADER;
  {$IFEND}

  {$IF NOT DECLARED( PIMAGE_EXPORT_DIRECTORY )}
  PIMAGE_EXPORT_DIRECTORY = ^IMAGE_EXPORT_DIRECTORY;
  {$IFEND}

  {$IF NOT DECLARED( PIMAGE_DOS_HEADER )}
  PIMAGE_DOS_HEADER = ^IMAGE_DOS_HEADER;
  {$IFEND}

  {$IF NOT DECLARED( PUINT_PTR )}
  PUINT_PTR = ^UINT_PTR;
  {$IFEND}

  {$IF NOT DECLARED( _IMAGE_TLS_DIRECTORY32 )}
  _IMAGE_TLS_DIRECTORY32 = record
    StartAddressOfRawData: Cardinal;
    EndAddressOfRawData: Cardinal;
    AddressOfIndex: Cardinal;             // PDWORD
    AddressOfCallBacks: Cardinal;         // PIMAGE_TLS_CALLBACK *
    SizeOfZeroFill: Cardinal;
    Characteristics: Cardinal;
  end;
  {$IFEND}

  {$IF NOT DECLARED( PIMAGE_TLS_DIRECTORY32 )}
  PIMAGE_TLS_DIRECTORY32 = ^_IMAGE_TLS_DIRECTORY32;
  {$IFEND}

  {$IF NOT DECLARED( PIMAGE_TLS_DIRECTORY )}
  PIMAGE_TLS_DIRECTORY = PIMAGE_TLS_DIRECTORY32;
  {$IFEND}

  {$IF NOT DECLARED( PIMAGE_TLS_CALLBACK )}
  PIMAGE_TLS_CALLBACK = procedure ( DllHandle: Pointer; Reason: Cardinal; Reserved: Pointer ) stdcall;
  {$IFEND}

  {$IF NOT DECLARED( _IMAGE_IMPORT_DESCRIPTOR )}
  _IMAGE_IMPORT_DESCRIPTOR = record
    case Byte of
      0: ( Characteristics: Cardinal );          // 0 for terminating null import descriptor
      1: ( OriginalFirstThunk: Cardinal;        // RVA to original unbound IAT ( PIMAGE_THUNK_DATA )
          TimeDateStamp: Cardinal;             // 0 if not bound,
                                            // -1 if bound, and real date\time stamp
                                            //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT ( new BIND )
                                            // O.W. date/time stamp of DLL bound to ( Old BIND )

          ForwarderChain: Cardinal;            // -1 if no forwarders
          Name: Cardinal;
          FirstThunk: Cardinal );                // RVA to IAT ( if bound this IAT has actual addresses )
  end;
  {$IFEND}

  {$IF NOT DECLARED( PIMAGE_IMPORT_DESCRIPTOR )}
  PIMAGE_IMPORT_DESCRIPTOR = ^_IMAGE_IMPORT_DESCRIPTOR;
  {$IFEND}

  {$IF NOT DECLARED( _IMAGE_IMPORT_BY_NAME )}
  _IMAGE_IMPORT_BY_NAME = record
    Hint: Word;
    Name: array[ 0..0 ] of Byte;
  end;
  {$IFEND}

  {$IF NOT DECLARED( PIMAGE_IMPORT_BY_NAME )}
  PIMAGE_IMPORT_BY_NAME = ^_IMAGE_IMPORT_BY_NAME;
  {$IFEND}

  {$IF NOT DECLARED( _IMAGE_IMPORT_DESCRIPTOR )}
  _IMAGE_IMPORT_DESCRIPTOR = record
    case Byte of
      0: ( Characteristics: Cardinal );          // 0 for terminating null import descriptor
      1: ( OriginalFirstThunk: Cardinal;        // RVA to original unbound IAT ( PIMAGE_THUNK_DATA )
          TimeDateStamp: Cardinal;             // 0 if not bound,
                                            // -1 if bound, and real date\time stamp
                                            //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT ( new BIND )
                                            // O.W. date/time stamp of DLL bound to ( Old BIND )

          ForwarderChain: Cardinal;            // -1 if no forwarders
          Name: Cardinal;
          FirstThunk: Cardinal );                // RVA to IAT ( if bound this IAT has actual addresses )
  end;
  {$IFEND}

  {$IF NOT DECLARED( IMAGE_IMPORT_DESCRIPTOR )}
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;
  {$IFEND}

  {$IF NOT DECLARED( LPSYSTEM_INFO )}
  LPSYSTEM_INFO = ^SYSTEM_INFO;
  {$IFEND}

// Missing constants
const
  IMAGE_SIZEOF_BASE_RELOCATION = 8;
  IMAGE_REL_BASED_ABSOLUTE = 0;
  IMAGE_REL_BASED_HIGHLOW = 3;
  IMAGE_REL_BASED_DIR64 = 10;
{$IF NOT Defined( FPC ) AND ( CompilerVersion < 23 )}
  IMAGE_ORDINAL_FLAG64 = UInt64( $8000000000000000 );
  IMAGE_ORDINAL_FLAG32 = LongWord( $80000000 );
  IMAGE_ORDINAL_FLAG = IMAGE_ORDINAL_FLAG32;
  HEAP_ZERO_MEMORY   = $00000008;
{$IFEND}

// Things that are incorrectly defined at least up to XE6 ( miss x64 mapping )
{$IFDEF CPUX64}
type
  PIMAGE_TLS_DIRECTORY = PIMAGE_TLS_DIRECTORY64;
const
  IMAGE_ORDINAL_FLAG = IMAGE_ORDINAL_FLAG64;
{$ENDIF}

type
  TDllEntryProc = function( hinstDLL: HINST; fdwReason: Cardinal; lpReserved: Pointer ): BOOL; stdcall;

  TSectionFinalizeData = record
    address: Pointer;
    alignedAddress: Pointer;
    size: Cardinal;
    characteristics: Cardinal;
    last: Boolean;
  end;

// Explicitly export these functions to allow hooking of their origins
function GetProcAddress_Internal( hModule: HMODULE; lpProcName: LPCSTR ): FARPROC; stdcall; external kernel32 name 'GetProcAddress';
function LoadLibraryA_Internal( lpLibFileName: LPCSTR ): HMODULE; stdcall; external kernel32 name 'LoadLibraryA';
function FreeLibrary_Internal( hLibModule: HMODULE ): BOOL; stdcall; external kernel32 name 'FreeLibrary';

{$IF NOT Defined( FPC ) AND ( CompilerVersion < 23 )}
procedure GetNativeSystemInfo( lpSystemInfo: LPSYSTEM_INFO ); stdcall; external kernel32 name 'GetNativeSystemInfo';
{$IFEND}

// Copy from SysUtils to get rid of this unit
function StrComp( const Str1, Str2: PAnsiChar ): Integer;
var
  P1, P2: PAnsiChar;
begin
  P1 := Str1;
  P2 := Str2;
  while True do
    begin
    if ( P1^ <> P2^ ) or ( P1^ = #0 ) then
      {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
      Exit( Ord( P1^ ) - Ord( P2^ ) );
      {$ELSE}
      begin
      result := Ord( P1^ ) - Ord( P2^ );
      Exit;
      end;
      {$IFEND}
    Inc( P1 );
    Inc( P2 );
    end;
end;

{$IF NOT Declared( FileExists )}
function FileExists(const FileName: string): Boolean;
//function FileAge(const FileName: string): Integer;
type
  LongRec = packed record
    case Integer of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
  tmp : Integer;
begin
  Result := False;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(tmp).Hi, LongRec(tmp).Lo) then
        begin
        result := True;
        Exit;
        end;
    end;
  end;
//  Result := -1;
//end;
//begin
//  Result := FileAge(FileName) <> -1;
end;
{$IFEND}

{$IF NOT Declared( ExceptionErrorMessage )}
type
  Exception = class(TObject)
  private
    Message: string;
  end;
function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer; Buffer: PChar; Size: Integer): Integer;
{$IFDEF MSWINDOWS}
  function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar; overload;
  var
    Len: Cardinal;
  begin
    Result := Dest;
    Len := Length(Source);
    if Len > MaxLen then
      Len := MaxLen;
    Move(Source^, Dest^, Len * SizeOf(Char));
    Dest[Len] := #0;
  end;

  function StrScan(const Str: PChar; Chr: Char): PChar;
  begin
    Result := Str;
    while Result^ <> #0 do
    begin
      if Result^ = Chr then
        Exit;
      Inc(Result);
    end;
    if Chr <> #0 then
      Result := nil;
  end;

  function AnsiStrScan(Str: PChar; Chr: Char): PChar;
  begin
    Result := StrScan(Str, Chr);
(* Needs SysLocale // MS
    {$IFNDEF UNICODE}
    while Result <> nil do
    begin
  {$IFDEF MSWINDOWS}
      case StrByteType(Str, Integer(Result-Str)) of
        mbSingleByte: Exit;
        mbLeadByte: Inc(Result);
      end;
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
      if StrByteType(Str, Integer(Result-Str)) = mbSingleByte then
        Exit;
  {$ENDIF POSIX}
      Inc(Result);
      Result := StrScan(Result, Chr);
    end;
    {$ENDIF}
*)
  end;

  {$IFDEF UNICODE}
  function StrRScan(const Str: PWideChar; Chr: WideChar): PWideChar;
    function StrEnd(const Str: PWideChar): PWideChar;
    begin
      Result := Str;
      while Result^ <> #0 do
        Inc(Result);
    end;
  var
    MostRecentFound: PWideChar;
  begin
    if Chr = #0 then
      Result := StrEnd(Str)
    else
    begin
      Result := nil;

      MostRecentFound := Str;
      while True do
      begin
        while MostRecentFound^ <> Chr do
        begin
          if MostRecentFound^ = #0 then
            Exit;
          Inc(MostRecentFound);
        end;
        Result := MostRecentFound;
        Inc(MostRecentFound);
      end;
    end;
  end;
  {$ENDIF}

  function AnsiStrRScan(Str: PChar; Chr: Char): PChar;
  begin
    {$IFDEF UNICODE}
    result := StrRScan(Str, Chr);
    {$ELSE}
    Str := AnsiStrScan(Str, Chr);
    Result := Str;
    if Chr <> AnsiChar(#$0) then
    begin
      while Str <> nil do
      begin
        Result := Str;
        Inc(Str);
        Str := AnsiStrScan(Str, Chr);
      end;
    end
    {$ENDIF}
  end;

  function PointerToHex( P : Pointer; TypeSize : Byte; BigEndian : boolean = false ): string;
    function ByteToHex( B : Byte ) : ShortString;
    const
      HexTable : Array[ 0..15 ] of AnsiChar = ( '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' );
    begin
      result := HexTable[ b div 16 ] + HexTable[ b mod 16 ];
    end;
  type
    TBytes = Array [ 0..15 ] of Byte;
    PBytes = ^TBytes;
  var
    Bytes : PBytes absolute P;
    i     : Integer;
  begin
    result := '';
    if ( P = nil ) then
      Exit;
    if ( TypeSize > SizeOf( TBytes ) ) then
      Exit;

    if BigEndian then
      begin
      for i := Low( Bytes^ ) To TypeSize-1 do
        result := result + ByteToHex( Bytes^[ i ] );
      end
    else
      begin
      for i := TypeSize-1 downTo Low( Bytes^ ) do
        result := result + ByteToHex( Bytes^[ i ] );
      end;

    i := Pos( ' ', Result );
    while ( i <> 0 ) do
      begin
      Result[ I ] := '0';
      i := Pos( ' ', Result );
      end;
  end;

  function ConvertAddr(Address: Pointer): Pointer;
  {$IFDEF Win64}
  begin
    Result := Address;
    if Result <> nil then
      Dec(PByte(Result), $1000);
  end;
  {$ELSE}
  asm //StackAlignSafe
          TEST    EAX,EAX         { Always convert nil to nil }
          JE      @@1
          SUB     EAX, $1000      { offset from code start; code start set by linker to $1000 }
  @@1:
  end;
  {$ENDIF Win64}
var
  MsgPtr: PChar;
  MsgEnd: PChar;
  MsgLen: Integer;
  ModuleName: array[0..MAX_PATH] of Char;
  Temp: array[0..MAX_PATH] of Char;
//  Format: array[0..255] of Char;
  Info: TMemoryBasicInformation;
  ConvertedAddress: Pointer;
  S : String;
begin
  VirtualQuery(ExceptAddr, Info, sizeof(Info));
  if (Info.State <> MEM_COMMIT) or
    (GetModuleFilename(THandle(Info.AllocationBase), Temp, SizeOf(Temp)) = 0) then
  begin
    GetModuleFileName(HInstance, Temp, SizeOf(Temp));
    ConvertedAddress := ConvertAddr(ExceptAddr);
  end
  else
    IntPtr(ConvertedAddress) := IntPtr(ExceptAddr) - IntPtr(Info.AllocationBase);
  StrLCopy( ModuleName, AnsiStrRScan(Temp, '\') + 1, SizeOf(ModuleName) - 1 );
  MsgPtr := '';
  MsgEnd := '';

  if Assigned( ExceptObject ) then // ExceptObject.ClassNameIs( 'Exception' ) then
  begin
    MsgPtr := PChar(Exception(ExceptObject).Message);
    MsgLen := Length(MsgPtr);
    if (MsgLen <> 0) and (MsgPtr[MsgLen - 1] <> '.') then
      MsgEnd := '.';
  end;
//  LoadString(FindResourceHInstance(HInstance),
//    PResStringRec(@SException).Identifier, Format, SizeOf(Format));

  S := 'Exception' + ' ' + ExceptObject.ClassName +' in Modul ' + ModuleName + ' bei ' + PointerToHex( @ConvertedAddress, SizeOf( ConvertedAddress ) ) + '.'+#13#10 + ' ' + MsgPtr + MsgEnd + #13#10;
  FillChar( Buffer[ 0 ], Size, 0 );
  Result := Length( S );
  Move( S[ 1 ], Buffer[ 0 ], Result );
end;
{$ENDIF}
{$IF defined(LINUX) or defined(MACOS) or defined(ANDROID)}
const
  UnknownModuleName = '<unknown>';
var
  MsgPtr: PChar;
  MsgEnd: PChar;
  MsgLen: Integer;
  Modulename: string;
  Info: dl_info;
begin
  MsgPtr := '';
  MsgEnd := '';
  if ExceptObject is Exception then
  begin
    MsgPtr := PChar(Exception(ExceptObject).Message);
    MsgLen := StrLen(MsgPtr);
    if (MsgLen <> 0) and (MsgPtr[MsgLen - 1] <> '.') then MsgEnd := '.';
  end;
  if (dladdr(IntPtr(ExceptAddr), Info) <> 0) and (Info.dli_fname <> nil) then
  begin
    ModuleName := string(Info.dli_fname);
    ModuleName := Modulename.SubString(Modulename.LastIndexOf( PathDelim) + 1)
  end
  else
  begin
    ModuleName := UnknownModuleName;
  end;
  StrLFmt(Buffer, Size, PChar(SException), [ExceptObject.ClassName, ModuleName,
    ExceptAddr, MsgPtr, MsgEnd]);
  Result := StrLen(Buffer);
end;
{$IFEND LINUX or MACOS or ANDROID}
{$IFEND}

  { +++++++++++++++++++++++++++++++++++++++++++++++++++++
    ***                Missing WinAPI macros          ***
    ----------------------------------------------------- }

{$IF NOT DECLARED( IMAGE_ORDINAL )}
//  #define IMAGE_ORDINAL64( Ordinal ) ( Ordinal & 0xffff )
//  #define IMAGE_ORDINAL32( Ordinal ) ( Ordinal & 0xffff )
function IMAGE_ORDINAL( Ordinal: NativeUInt ): Word; {$IF Defined( FPC ) OR ( CompilerVersion >= 22 )}inline;{$IFEND}
begin
  Result := Ordinal and $FFFF;
end;
{$IFEND}

{$IF NOT DECLARED( IMAGE_SNAP_BY_ORDINAL )}
//  IMAGE_SNAP_BY_ORDINAL64( Ordinal ) ( ( Ordinal & IMAGE_ORDINAL_FLAG64 ) != 0 )
//  IMAGE_SNAP_BY_ORDINAL32( Ordinal ) ( ( Ordinal & IMAGE_ORDINAL_FLAG32 ) != 0 )
function IMAGE_SNAP_BY_ORDINAL( Ordinal: NativeUInt ): Boolean; {$IF Defined( FPC ) OR ( CompilerVersion >= 22 )}inline;{$IFEND}
begin
  Result := ( ( Ordinal and IMAGE_ORDINAL_FLAG ) <> 0 );
end;
{$IFEND}

function GET_HEADER_DICTIONARY( module: PMemoryModule; idx: Integer ): PIMAGE_DATA_DIRECTORY;
begin
  Result := PIMAGE_DATA_DIRECTORY( @( module.headers.OptionalHeader.DataDirectory[ idx ] ) );
end;

{$IF NOT DECLARED( IMAGE_FIRST_SECTION )}
function IMAGE_FIRST_SECTION( NtHeader: PIMAGE_NT_HEADERS ): PImageSectionHeader;
var
  OptionalHeaderAddr: PByte;
begin
  OptionalHeaderAddr := @NtHeader^.OptionalHeader;
  Inc( OptionalHeaderAddr, NtHeader^.FileHeader.SizeOfOptionalHeader );
  Result := PImageSectionHeader( OptionalHeaderAddr );
end;
{$IFEND}

function CopySections( data: Pointer; old_headers: PIMAGE_NT_HEADERS; module: PMemoryModule ): Boolean;
var
  i, size: Integer;
  codebase: Pointer;
  dest: Pointer;
  section: PIMAGE_SECTION_HEADER;
begin
  codebase := module.codeBase;
  {$IF NOT Defined( FPC ) AND ( CompilerVersion < 23 )}
  section := PIMAGE_SECTION_HEADER( IMAGE_FIRST_SECTION( module.headers ) );
  {$ELSE}
  section := PIMAGE_SECTION_HEADER( IMAGE_FIRST_SECTION( module.headers{$IFNDEF FPC}^{$ENDIF} ) );
  {$IFEND}
  for i := 0 to module.headers.FileHeader.NumberOfSections - 1 do
    begin
    // section doesn't contain data in the dll itself, but may define
    // uninitialized data
    if section.SizeOfRawData = 0 then
      begin
      size := old_headers.OptionalHeader.SectionAlignment;
      if size > 0 then
        begin
        dest := VirtualAlloc( 
                             {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
                             PByte( codebase ) + section.VirtualAddress,
                             {$ELSE}
                             PAnsiChar( codebase ) + section.VirtualAddress,
                             {$IFEND}
                             size, MEM_COMMIT, PAGE_EXECUTE_READWRITE );
        if dest = nil then
          {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
          Exit( false );
          {$ELSE}
          begin
          result := false;
          Exit;
          end;
          {$IFEND}

        // Always use position from file to support alignments smaller
        // than page size.
        {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
        dest := PByte( codebase ) + section.VirtualAddress;
        {$ELSE}
        dest := PAnsiChar( codebase ) + section.VirtualAddress;
        {$IFEND}
        section.Misc.PhysicalAddress := Cardinal( dest );
        ZeroMemory( dest, size );
        end;
      // section is empty
      Inc( section );
      Continue;
      end;

    // commit memory block and copy data from dll
    dest := VirtualAlloc( 
                         {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
                         PByte( codebase ) + section.VirtualAddress,
                         {$ELSE}
                         PAnsiChar( codebase ) + section.VirtualAddress,
                         {$IFEND}
                         section.SizeOfRawData,
                         MEM_COMMIT,
                         PAGE_EXECUTE_READWRITE );
    if dest = nil then
      {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
      Exit( false );
      {$ELSE}
      begin
      result := false;
      Exit;
      end;
      {$IFEND}

    // Always use position from file to support alignments smaller
    // than page size.
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    dest := PByte( codebase ) + section.VirtualAddress;
    CopyMemory( dest, PByte( data ) + section.PointerToRawData, section.SizeOfRawData );
    {$ELSE}
    dest := PAnsiChar( codebase ) + section.VirtualAddress;
    CopyMemory( dest, PAnsiChar( data ) + section.PointerToRawData, section.SizeOfRawData );
    {$IFEND}
    section.Misc.PhysicalAddress := Cardinal( dest );
    Inc(section); 
    end; // for

  Result := True;
end;

// Protection flags for memory pages ( Executable, Readable, Writeable )
const
  ProtectionFlags: array[ Boolean, Boolean, Boolean ] of Cardinal =
  ( 
    ( 
        // not executable
        ( PAGE_NOACCESS, PAGE_WRITECOPY ),
        ( PAGE_READONLY, PAGE_READWRITE )
    ),
    ( 
        // executable
        ( PAGE_EXECUTE, PAGE_EXECUTE_WRITECOPY ),
        ( PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE )
    )
 );

function FinalizeSection( module: PMemoryModule; const sectionData: TSectionFinalizeData ): Boolean;
var
  protect, oldProtect: Cardinal;
  executable, readable, writeable: Boolean;
begin
  if sectionData.size = 0 then
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    Exit( True );
    {$ELSE}
    begin
    result := True;
    Exit;
    end;
    {$IFEND}

  if ( sectionData.characteristics and IMAGE_SCN_MEM_DISCARDABLE ) <> 0 then
    begin
    // section is not needed any more and can safely be freed
    if ( sectionData.address = sectionData.alignedAddress ) and
       ( sectionData.last or
         ( module.headers.OptionalHeader.SectionAlignment = module.pageSize ) or
         ( sectionData.size mod module.pageSize = 0 )
       ) then
         // Only allowed to decommit whole pages
         VirtualFree( sectionData.address, sectionData.size, MEM_DECOMMIT );
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    Exit( True );
    {$ELSE}
    result := True;
    Exit;
    {$IFEND}
    end;

  // determine protection flags based on characteristics
  executable := ( sectionData.characteristics and IMAGE_SCN_MEM_EXECUTE ) <> 0;
  readable   := ( sectionData.characteristics and IMAGE_SCN_MEM_READ ) <> 0;
  writeable  := ( sectionData.characteristics and IMAGE_SCN_MEM_WRITE ) <> 0;
  protect := ProtectionFlags[ executable ][ readable ][ writeable ];
  if ( sectionData.characteristics and IMAGE_SCN_MEM_NOT_CACHED ) <> 0 then
    protect := protect or PAGE_NOCACHE;

  // change memory access flags
  Result := VirtualProtect( sectionData.address, sectionData.size, protect, oldProtect );
end;

function FinalizeSections( module: PMemoryModule ): Boolean;
  function GetRealSectionSize( module: PMemoryModule; section: PIMAGE_SECTION_HEADER ): Cardinal;
  begin
    Result := section.SizeOfRawData;
    if Result = 0 then
      if ( section.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA ) <> 0 then
        Result := module.headers.OptionalHeader.SizeOfInitializedData
      else if ( section.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA ) <> 0 then
        Result := module.headers.OptionalHeader.SizeOfUninitializedData;
  end;
  function ALIGN_DOWN( address: Pointer; alignment: Cardinal ): Pointer;
  begin
    Result := Pointer( UINT_PTR( address ) and not ( alignment - 1 ) );
  end;

var
  i: Integer;
  section: PIMAGE_SECTION_HEADER;
  imageOffset: UINT_PTR;
  sectionData: TSectionFinalizeData;
  sectionAddress, alignedAddress: Pointer;
  sectionSize: Cardinal;
begin
  {$IF CompilerVersion < 23}
  section := PIMAGE_SECTION_HEADER( IMAGE_FIRST_SECTION( module.headers ) );
  {$ELSE}
  section := PIMAGE_SECTION_HEADER( IMAGE_FIRST_SECTION( module.headers{$IFNDEF FPC}^{$ENDIF} ) );
  {$IFEND}
  {$IFDEF CPUX64}
  imageOffset := ( NativeUInt( module.codeBase ) and $ffffffff00000000 );
  {$ELSE}
  imageOffset := 0;
  {$ENDIF}

  sectionData.address := Pointer( UINT_PTR( section.Misc.PhysicalAddress ) or imageOffset );
  sectionData.alignedAddress := ALIGN_DOWN( sectionData.address, module.pageSize );
  sectionData.size := GetRealSectionSize( module, section );
  sectionData.characteristics := section.Characteristics;
  sectionData.last := False;
  Inc( section );

  // loop through all sections and change access flags

  for i := 1 to module.headers.FileHeader.NumberOfSections - 1 do
    begin
    sectionAddress := Pointer( UINT_PTR( section.Misc.PhysicalAddress ) or imageOffset );
    alignedAddress := ALIGN_DOWN( sectionData.address, module.pageSize );
    sectionSize := GetRealSectionSize( module, section );
    // Combine access flags of all sections that share a page
    // TODO( fancycode ): We currently share flags of a trailing large section
    //   with the page of a first small section. This should be optimized.
    if ( sectionData.alignedAddress = alignedAddress ) or
        {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
       ( PByte( sectionData.address ) + sectionData.size > PByte( alignedAddress ) ) then
        {$ELSE}
       ( PAnsiChar( sectionData.address ) + sectionData.size > PAnsiChar( alignedAddress ) ) then
        {$IFEND}
      begin
      // Section shares page with previous
      if ( section.Characteristics and IMAGE_SCN_MEM_DISCARDABLE = 0 ) or
         ( sectionData.Characteristics and IMAGE_SCN_MEM_DISCARDABLE = 0 ) then
        sectionData.characteristics := ( sectionData.characteristics or section.Characteristics ) and not IMAGE_SCN_MEM_DISCARDABLE
      else
        sectionData.characteristics := sectionData.characteristics or section.Characteristics;

      {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
      sectionData.size := PByte( sectionAddress ) + sectionSize - PByte( sectionData.address );
      {$ELSE}
      sectionData.size := PAnsiChar( sectionAddress ) + sectionSize - PAnsiChar( sectionData.address );
      {$IFEND}

      Inc( section );
      Continue;
      end;

    if not FinalizeSection( module, sectionData ) then
      {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
      Exit( false );
      {$ELSE}
      begin
      result := false;
      Exit;
      end;
      {$IFEND}

    sectionData.address := sectionAddress;
    sectionData.alignedAddress := alignedAddress;
    sectionData.size := sectionSize;
    sectionData.characteristics := section.Characteristics;

    Inc( section );
    end; // for

  sectionData.last := True;
  if not FinalizeSection( module, sectionData ) then
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    Exit( false );
    {$ELSE}
    begin
    result := false;
    Exit;
    end;
    {$IFEND}

  Result := True;
end;

function ExecuteTLS( module: PMemoryModule ): Boolean;
var
  codeBase: Pointer;
  // TLS callback pointers are VA's ( ImageBase included ) so if the module resides at
  // the other ImageBage they become invalid. This routine relocates them to the
  // actual ImageBase.
  // The case seem to happen with DLLs only and they rarely use TLS callbacks.
  // Moreover, they probably don't work at all when using DLL dynamically which is
  // the case in our code.
  function FixPtr( OldPtr: Pointer ): Pointer;
  begin
    Result := Pointer( NativeInt( OldPtr ) - module.headers.OptionalHeader.ImageBase + NativeInt( codeBase ) );
  end;
var
  directory: PIMAGE_DATA_DIRECTORY;
  tls: PIMAGE_TLS_DIRECTORY;
  callback: PPointer; // =^PIMAGE_TLS_CALLBACK;
begin
  Result := False;
  codeBase := module.codeBase;

  directory := GET_HEADER_DICTIONARY( module, IMAGE_DIRECTORY_ENTRY_TLS );
  if directory.VirtualAddress = 0 then
    Exit;
  tls := PIMAGE_TLS_DIRECTORY( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codeBase ) + directory.VirtualAddress );

  // Delphi syntax is quite awkward when dealing with proc pointers so we have to
  // use casts to untyped pointers
  callback := Pointer( tls.AddressOfCallBacks );
  if callback <> nil then
    begin
    callback := FixPtr( callback );

    if ( NativeUInt( callback ) < NativeUInt( module^.codeBase ) ) or
       ( NativeUInt( callback ) >= NativeUInt( module^.codeBase ) + Module^.headers.OptionalHeader.SizeOfCode ) then // MS Validate me
      Exit;

    try
      while callback^ <> nil do
        begin
        PIMAGE_TLS_CALLBACK( FixPtr( callback^ ) )( codeBase, DLL_PROCESS_ATTACH, nil );
        Inc( callback );
        end;
    except
      Exit;
    end;
    end
  else
    Exit;
  Result := True;
end;

function PerformBaseRelocation( module: PMemoryModule; delta: NativeInt ): Boolean;
var
  i: Cardinal;
  codebase: Pointer;
  directory: PIMAGE_DATA_DIRECTORY;
  relocation: PIMAGE_BASE_RELOCATION;
  dest: Pointer;
  relInfo: {PUINT16}PWORD;
  patchAddrHL: PDWORD;
// patchAddrHL: PULONGLONG; // MS Fix for MIL ... to validate
 {$IFDEF CPUX64}
  patchAddr64: PULONGLONG;
  {$ENDIF}
  relType, offset: Integer;
begin
  codebase := module.codeBase;
  directory := GET_HEADER_DICTIONARY( module, IMAGE_DIRECTORY_ENTRY_BASERELOC );
  if directory.Size = 0 then
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    Exit( delta = 0 );
    {$ELSE}
    begin
    result := delta = 0;
    Exit;
    end;
    {$IFEND}

  {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
  relocation := PIMAGE_BASE_RELOCATION( PByte( codebase ) + directory.VirtualAddress );
  {$ELSE}
  relocation := PIMAGE_BASE_RELOCATION( PAnsiChar( codebase ) + directory.VirtualAddress );
  {$IFEND}

  while relocation.VirtualAddress > 0 do
    begin
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    dest := Pointer( PByte( codebase ) + relocation.VirtualAddress );
    relInfo := Pointer( PByte( relocation ) + IMAGE_SIZEOF_BASE_RELOCATION );
    {$ELSE}
    dest := Pointer( PAnsiChar( codebase ) + relocation.VirtualAddress );
    relInfo := Pointer( PAnsiChar( relocation ) + IMAGE_SIZEOF_BASE_RELOCATION );
    {$IFEND}

    for i := 0 to Trunc( ( ( relocation.SizeOfBlock - IMAGE_SIZEOF_BASE_RELOCATION ) / 2 ) ) - 1 do
      begin
      // the upper 4 bits define the type of relocation
      relType := relInfo^ shr 12;
      // the lower 12 bits define the offset
      offset := relInfo^ and $FFF;

      case relType of
        IMAGE_REL_BASED_ABSOLUTE: ; // skip relocation
        IMAGE_REL_BASED_HIGHLOW:
          begin
          // change complete 32 bit address
          {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
          patchAddrHL := Pointer( PByte( dest ) + offset );
          {$ELSE}
          patchAddrHL := Pointer( PAnsiChar( dest ) + offset );
          {$IFEND}

          {$R-}
          patchAddrHL^ := patchAddrHL^+delta;
          {$R+}
          end;

        {$IFDEF CPUX64}
        IMAGE_REL_BASED_DIR64:
          begin
          patchAddr64 := Pointer( PByte( dest ) + offset );
          Inc( patchAddr64^, delta );
          end;
        {$ENDIF}
      end;

      Inc( relInfo );
      end; // for

    // advance to next relocation block
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    relocation := PIMAGE_BASE_RELOCATION( PByte( relocation ) + relocation.SizeOfBlock );
    {$ELSE}
    relocation := PIMAGE_BASE_RELOCATION( PAnsiChar( relocation ) + relocation.SizeOfBlock );
    {$IFEND}
    end; // while

  Result := True;
end;

function BuildImportTable( module: PMemoryModule ): Boolean; stdcall;
var
  codebase: Pointer;
  directory: PIMAGE_DATA_DIRECTORY;
  importDesc: PIMAGE_IMPORT_DESCRIPTOR;
  thunkRef: PUINT_PTR;
  funcRef: ^FARPROC;
  handle: HMODULE;
  thunkData: PIMAGE_IMPORT_BY_NAME;
  vName : PAnsiChar;
  {$IFDEF GetModuleHandle_BuildImportTable}
  vFree : Boolean;
  {$ENDIF GetModuleHandle_BuildImportTable}
begin
  codebase := module.codeBase;
  Result := True;

  directory := GET_HEADER_DICTIONARY( module, IMAGE_DIRECTORY_ENTRY_IMPORT );
  if directory.Size = 0 then
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    Exit( True );
    {$ELSE}
    begin
    result := True;
    Exit;
    end;
    {$IFEND}

  {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
  importDesc := PIMAGE_IMPORT_DESCRIPTOR( PByte( codebase ) + directory.VirtualAddress );
  {$ELSE}
  importDesc := PIMAGE_IMPORT_DESCRIPTOR( PAnsiChar( codebase ) + directory.VirtualAddress );
  {$IFEND}

  while ( not IsBadReadPtr( importDesc, SizeOf( IMAGE_IMPORT_DESCRIPTOR ) ) ) and ( importDesc.Name <> 0 ) do
    begin
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    vName := PAnsiChar( PByte( codebase ) + importDesc.Name );
    {$ELSE}
    vName := PAnsiChar( PAnsiChar( codebase ) + importDesc.Name );
    {$IFEND}

    {$IFDEF GetModuleHandle_BuildImportTable}
    handle := GetModuleHandleA( vName );
    vFree := ( handle = 0 );
    if vFree then
    {$ENDIF GetModuleHandle_BuildImportTable}
      handle := LoadLibraryA_Internal( vName );

    if ( handle = 0 ) then
      begin
      SetLastError( ERROR_MOD_NOT_FOUND );
      Result := False;
      Break;
      end;

    try
      SetLength( module.modules, Length( module.modules )+1 );
    except
      FreeLibrary_Internal( handle );
      SetLastError( ERROR_OUTOFMEMORY );
      Result := False;
      Break;
    end;
    module.modules[ High( module.Modules ) ].Handle := handle;
    {$IFDEF GetModuleHandle_BuildImportTable}
    module.modules[ High( module.Modules ) ].Free   := vFree;
    {$ENDIF GetModuleHandle_BuildImportTable}

    if importDesc.OriginalFirstThunk <> 0 then
      begin
      {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
      thunkRef := Pointer( PByte( codebase ) + importDesc.OriginalFirstThunk );
      funcRef := Pointer( PByte( codebase ) + importDesc.FirstThunk );
      {$ELSE}
      thunkRef := Pointer( PAnsiChar( codebase ) + importDesc.OriginalFirstThunk );
      funcRef := Pointer( PAnsiChar( codebase ) + importDesc.FirstThunk );
      {$IFEND}
      end
    else
      begin
      // no hint table
      {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
      thunkRef := Pointer( PByte( codebase ) + importDesc.FirstThunk );
      funcRef := Pointer( PByte( codebase ) + importDesc.FirstThunk );
      {$ELSE}
      thunkRef := Pointer( PAnsiChar( codebase ) + importDesc.FirstThunk );
      funcRef := Pointer( PAnsiChar( codebase ) + importDesc.FirstThunk );
      {$IFEND}
      end;

    while thunkRef^ <> 0 do
      begin
      if IMAGE_SNAP_BY_ORDINAL( thunkRef^ ) then
        funcRef^ := GetProcAddress_Internal( handle, PAnsiChar( IMAGE_ORDINAL( thunkRef^ ) ) )
      else
        begin
        {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
        thunkData := PIMAGE_IMPORT_BY_NAME( PByte( codebase ) + thunkRef^ );
        {$ELSE}
        thunkData := PIMAGE_IMPORT_BY_NAME( PAnsiChar( codebase ) + thunkRef^ ); // RangeCheck causing Internal-Error C1118
        {$IFEND}
        funcRef^ := GetProcAddress_Internal( handle, PAnsiChar( @( thunkData.Name ) ) );
        end;
      if funcRef^ = nil then
        begin
        Result := False;
        Break;
        end;
      Inc( funcRef );
      Inc( thunkRef );
      end; // while

    if not Result then
      begin
      FreeLibrary_Internal( handle );
      SetLastError( ERROR_PROC_NOT_FOUND );
      Break;
      end;

    Inc( importDesc );
    end; // while
end;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{$IFDEF GetModuleHandle}
type
  tModuleManager = class
  private
    fItems : Array of packed record
                      Handle     : PMemoryModule;
                      Data       : Pointer;
                      {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
                      FileName   : string;
                      {$IFEND}
                      {$IFDEF LOAD_FROM_RESOURCE}
                      IsResource : Boolean;
                      {$ENDIF LOAD_FROM_RESOURCE}

                      {$IFDEF UnloadAllOnFinalize}
                      DontUnload : Boolean;
                      {$ENDIF UnloadAllOnFinalize}
                      end;
    {$IFDEF GetModuleHandleCriticalSection}
    fCrit  : TCriticalSection;
    {$ENDIF GetModuleHandleCriticalSection}
    procedure   DelID( ID : Word );
    function    GetHandle( Data : Pointer ) : PMemoryModule; {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}overload;{$IFEND}
    {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
    function    GetHandle_( Name : String ) : PMemoryModule;
    function    GetName( Handle : PMemoryModule ) : String;
    {$IFEND Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
    {$IFDEF LOAD_FROM_RESOURCE}
    function    GetIsResource( Handle : PMemoryModule ) : boolean;
    {$ENDIF LOAD_FROM_RESOURCE}
    {$IFDEF UnloadAllOnFinalize}
    procedure   UnloadAll;
    {$ENDIF UnloadAllOnFinalize}
  public
    constructor Create; reintroduce;
    destructor  Destroy; override;
    procedure   Add( Handle : PMemoryModule; Data : Pointer{$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}; Name : String{$IFEND}{$IFDEF LOAD_FROM_RESOURCE}; IsResource : boolean{$ENDIF} );
    procedure   Del( Handle : PMemoryModule ); overload;
    procedure   DelData( Data : Pointer );
    {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
    procedure   Del( Name : String{$IFDEF LOAD_FROM_RESOURCE}; IsResource : boolean{$ENDIF} ); overload;
    {$IFEND Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
    {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
    function    GetHandle( Name : String{$IFDEF LOAD_FROM_RESOURCE}; IsResource : boolean{$ENDIF} ) : PMemoryModule; overload;
    {$IFEND Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
    property    ID[ Name : String ]        : PMemoryModule read GetHandle_;
    {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
    property    Name[ ID : PMemoryModule ] : string        read GetName;
    {$IFEND Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
    {$IFDEF LOAD_FROM_RESOURCE}
    property    IsResource[ ID : PMemoryModule ] : boolean read GetIsResource;
    {$ENDIF LOAD_FROM_RESOURCE}
  end;

var
  ModuleManager : tModuleManager = nil;
  {$IFDEF UnloadAllOnFinalize}
  InitializationDone : boolean = False;
  {$ENDIF UnloadAllOnFinalize}

constructor tModuleManager.Create;
begin
  inherited;
  SetLength( fItems, 0 );
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit := TCriticalSection.Create;
  {$ENDIF GetModuleHandleCriticalSection}

  {$IF Defined( FastMM4 ) OR Defined( FastMM5 )}
  {$if Declared( FastMM_RegisterExpectedMemoryLeak )}
  FastMM_RegisterExpectedMemoryLeak( Self );
  {$ELSE}
  RegisterExpectedMemoryLeak( Self );
  {$IFEND}
  {$IFEND}
end;

destructor tModuleManager.Destroy;
begin
  {$IFDEF UnloadAllOnFinalize}
  UnloadAll;
  {$ENDIF UnloadAllOnFinalize}
  SetLength( fItems, 0 );
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Free;
  {$ENDIF GetModuleHandleCriticalSection}
  inherited;
end;

procedure tModuleManager.Add( Handle : PMemoryModule; Data : Pointer{$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}; Name : String{$IFEND}{$IFDEF LOAD_FROM_RESOURCE}; IsResource : boolean{$ENDIF} );
var
  i : Integer;
begin
  if NOT Assigned( self ) then
    Exit;
  if NOT Assigned( Handle ) then
    Exit;
//  if ( Name = '' ) then
//    Exit;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Enter;
  {$ENDIF GetModuleHandleCriticalSection}
  for i := Low( fItems ) to High( fItems ) do
    begin
    if ( fItems[ i ].Handle = Handle ) then
      begin
      {$IFDEF LOAD_FROM_RESOURCE}
      if IsResource then // Allow override for Compressed Resources
        fItems[ i ].Data     := Data;
      {$ENDIF}
      {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
      fItems[ i ].FileName   := Name;
      {$IFEND}
      {$IFDEF LOAD_FROM_RESOURCE}
      fItems[ i ].IsResource := IsResource;
      {$ENDIF LOAD_FROM_RESOURCE}

      {$IFDEF GetModuleHandleCriticalSection}
      fCrit.Leave;
      {$ENDIF GetModuleHandleCriticalSection}
      Exit;
      end;
    end;

  SetLength( fItems, Length( fItems )+1 );
  fItems[ High( fItems ) ].Handle   := Handle;
  fItems[ High( fItems ) ].Data     := Data;
  {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
  fItems[ High( fItems ) ].FileName := Name;
  {$IFEND}
  {$IFDEF LOAD_FROM_RESOURCE}
  fItems[ High( fItems ) ].IsResource := IsResource;
  {$ENDIF LOAD_FROM_RESOURCE}
  {$IFDEF UnloadAllOnFinalize}
  fItems[ High( fItems ) ].DontUnload := NOT InitializationDone;
  {$ENDIF UnloadAllOnFinalize}
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Leave;
  {$ENDIF GetModuleHandleCriticalSection}
end;

procedure tModuleManager.DelID( ID : Word );
var
  i : Integer;
begin
  if NOT Assigned( self ) then
    Exit;
  if ( ID > High( fItems ) ) then
    Exit;
//  {$IFDEF GetModuleHandleCriticalSection}
//  fCrit.Enter;
//  {$ENDIF GetModuleHandleCriticalSection}
  for i := ID to High( fItems )-1 do
    fItems[ i ] := fItems[ i+1 ];
  SetLength( fItems, Length( fItems )-1 );
//  {$IFDEF GetModuleHandleCriticalSection}
//  fCrit.Leave;
//  {$ENDIF GetModuleHandleCriticalSection}
end;

procedure tModuleManager.Del( Handle : PMemoryModule );
var
  i : Integer;
begin
  if NOT Assigned( self ) then
    Exit;
  if NOT Assigned( Handle ) then
    Exit;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Enter;
  {$ENDIF GetModuleHandleCriticalSection}
  for i := High( fItems ) downTo Low( fItems ) do
    begin
    if ( fItems[ i ].Handle = Handle ) then
      begin
      DelID( i );
      break;
      end;
    end;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Leave;
  {$ENDIF GetModuleHandleCriticalSection}
end;

procedure tModuleManager.DelData( Data : Pointer );
var
  i : Integer;
begin
  if NOT Assigned( self ) then
    Exit;
  if NOT Assigned( Data ) then
    Exit;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Enter;
  {$ENDIF GetModuleHandleCriticalSection}
  for i := High( fItems ) downTo Low( fItems ) do
    begin
    if ( fItems[ i ].Data = Data ) then
      begin
      DelID( i );
//      break;
      end;
    end;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Leave;
  {$ENDIF GetModuleHandleCriticalSection}
end;

{$IFDEF UnloadAllOnFinalize}
procedure tModuleManager.UnloadAll;
var
  i : Integer;
begin
  if NOT Assigned( self ) then
    Exit;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Enter;
  {$ENDIF GetModuleHandleCriticalSection}
  for i := High( fItems ) downTo Low( fItems ) do
    begin
    {$IFDEF UnloadAllOnFinalize}
    if NOT fItems[ i ].DontUnload then
    {$ENDIF UnloadAllOnFinalize}
    MemoryFreeLibrary( fItems[ i ].Handle );
    end;
  SetLength( fItems, 0 );    
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Leave;
  {$ENDIF GetModuleHandleCriticalSection}
end;
{$ENDIF UnloadAllOnFinalize}

function tModuleManager.GetHandle( Data : Pointer ) : PMemoryModule;
var
  i : Integer;
begin
  result := nil;
  if NOT Assigned( self ) then
    Exit;
  if NOT Assigned( Data ) then
    Exit;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Enter;
  {$ENDIF GetModuleHandleCriticalSection}
  for i := Low( fItems ) to High( fItems ) do
    begin
    if ( fItems[ i ].Data = Data ) then
      begin
      result := fItems[ i ].Handle;
      break;
      end;
    end;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Leave;
  {$ENDIF GetModuleHandleCriticalSection}
end;

{$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
procedure tModuleManager.Del( Name : String{$IFDEF LOAD_FROM_RESOURCE}; IsResource : boolean{$ENDIF} );
var
  i : Integer;
begin
  if NOT Assigned( self ) then
    Exit;
//  if ( Name = '' ) then
//    Exit;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Enter;
  {$ENDIF GetModuleHandleCriticalSection}
  for i := High( fItems ) downTo Low( fItems ) do
    begin
    if ( CompareString( LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar( fItems[ i ].FileName ), Length( fItems[ i ].FileName ), PChar( Name ), Length( Name ) ) = 2 )
        {$IFDEF LOAD_FROM_RESOURCE}AND ( fItems[ i ].IsResource = IsResource ){$ENDIF} then
      begin
      DelID( i );
//      break;
      end;
    end;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Leave;
  {$ENDIF GetModuleHandleCriticalSection}
end;

function tModuleManager.GetHandle_( Name : String ) : PMemoryModule;
begin
  result := GetHandle( Name, False );
end;

function tModuleManager.GetHandle( Name : String{$IFDEF LOAD_FROM_RESOURCE}; IsResource : boolean{$ENDIF} ) : PMemoryModule;
var
  i : Integer;
begin
  result := nil;
  if NOT Assigned( self ) then
    Exit;
  if ( Name = '' ) then
    Exit;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Enter;
  {$ENDIF GetModuleHandleCriticalSection}
  for i := Low( fItems ) to High( fItems ) do
    begin
    if ( CompareString( LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar( fItems[ i ].FileName ), Length( fItems[ i ].FileName ), PChar( Name ), Length( Name ) ) = 2 )
        {$IFDEF LOAD_FROM_RESOURCE}AND ( fItems[ i ].IsResource = IsResource ){$ENDIF} then
      begin
      result := fItems[ i ].Handle;
      break;
      end;
    end;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Leave;
  {$ENDIF GetModuleHandleCriticalSection}
end;

function tModuleManager.GetName( Handle : PMemoryModule ) : String;
var
  i : Integer;
begin
  result := '';
  if NOT Assigned( self ) then
    Exit;
  if NOT Assigned( Handle ) then
    Exit;
//  if ( Name = '' ) then
//    Exit;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Enter;
  {$ENDIF GetModuleHandleCriticalSection}
  for i := Low( fItems ) to High( fItems ) do
    begin
    if ( fItems[ i ].Handle = Handle ) then
      begin
      result := fItems[ i ].FileName;
      break;
      end;
    end;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Leave;
  {$ENDIF GetModuleHandleCriticalSection}
end;
{$IFEND Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}

{$IFDEF LOAD_FROM_RESOURCE}
function tModuleManager.GetIsResource( Handle : PMemoryModule ) : boolean;
var
  i : Integer;
begin
  result := False;
  if NOT Assigned( self ) then
    Exit;
  if NOT Assigned( Handle ) then
    Exit;
//  if ( Name = '' ) then
//    Exit;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Enter;
  {$ENDIF GetModuleHandleCriticalSection}
  for i := Low( fItems ) to High( fItems ) do
    begin
    if ( fItems[ i ].Handle = Handle ) then
      begin
      result := fItems[ i ].IsResource;
      break;
      end;
    end;
  {$IFDEF GetModuleHandleCriticalSection}
  fCrit.Leave;
  {$ENDIF GetModuleHandleCriticalSection}
end;
{$ENDIF LOAD_FROM_RESOURCE}

{$ENDIF GetModuleHandle}

  { +++++++++++++++++++++++++++++++++++++++++++++++++++++
    ***  Memory DLL loading functions Implementation  ***
    ----------------------------------------------------- }
function MemoryLoadLibrary_1( data: Pointer; var Code : Pointer; var Module : PMemoryModule ) : ShortInt; stdcall;
var
  dos_header: PIMAGE_DOS_HEADER;
  old_header: PIMAGE_NT_HEADERS;
//  code,
  headers: Pointer;
  locationdelta: NativeInt;
  sysInfo: SYSTEM_INFO;
begin
  Result := -9;
  if NOT Assigned( Data ) then
    Exit;
  Result := -8;
  module := nil;

  try
    dos_header := PIMAGE_DOS_HEADER( data );
    if ( dos_header.e_magic <> IMAGE_DOS_SIGNATURE ) then
      begin
      Result := -7;
      SetLastError( ERROR_BAD_EXE_FORMAT );
      Exit;
      end;

    // old_header = ( PIMAGE_NT_HEADERS )&( ( const unsigned char * )( data ) )[ dos_header->e_lfanew ];
    old_header := PIMAGE_NT_HEADERS( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( data ) + dos_header._lfanew );
    if old_header.Signature <> IMAGE_NT_SIGNATURE then
      begin
      Result := -6;
      SetLastError( ERROR_BAD_EXE_FORMAT );
      Exit;
      end;

    {$IFDEF CPUX64}
    if old_header.FileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64 then
    {$ELSE}
    if old_header.FileHeader.Machine <> IMAGE_FILE_MACHINE_I386 then
    {$ENDIF}
      begin
      Result := -5;
      SetLastError( ERROR_BAD_EXE_FORMAT );
      Exit;
      end;

    if ( old_header.OptionalHeader.SectionAlignment and 1 ) <> 0 then
      begin
      // Only support section alignments that are a multiple of 2
      Result := -4;
      SetLastError( ERROR_BAD_EXE_FORMAT );
      Exit;
      end;

    // reserve memory for image of library
    // XXX: is it correct to commit the complete memory region at once?
    //      calling DllEntry raises an exception if we don't...
    code := VirtualAlloc( Pointer( old_header.OptionalHeader.ImageBase ),
                         old_header.OptionalHeader.SizeOfImage,
                         MEM_RESERVE or MEM_COMMIT,
                         PAGE_READWRITE );
    if code = nil then
      begin
      // try to allocate memory at arbitrary position
      code := VirtualAlloc( nil,
                           old_header.OptionalHeader.SizeOfImage,
                           MEM_RESERVE or MEM_COMMIT,
                           PAGE_READWRITE );
      if code = nil then
        begin
        Result := -3;
        SetLastError( ERROR_OUTOFMEMORY );
        Exit;
        end;
      end;

    module := PMemoryModule( HeapAlloc( GetProcessHeap, HEAP_ZERO_MEMORY, SizeOf( TMemoryModule ) ) );
    if module = nil then
      begin
      VirtualFree( code, 0, MEM_RELEASE );
      Result := -2;
      SetLastError( ERROR_OUTOFMEMORY );
      Exit;
      end;

    // memory is zeroed by HeapAlloc
    module.codeBase := code;
    GetNativeSystemInfo( {$IF ( CompilerVersion < 23 ) OR Defined( FPC )}@{$IFEND}sysInfo );
    module.pageSize := sysInfo.dwPageSize;

    // commit memory for headers
    headers := VirtualAlloc( code, old_header.OptionalHeader.SizeOfHeaders, MEM_COMMIT, PAGE_READWRITE );

    // copy PE header to code
    CopyMemory( headers, dos_header, old_header.OptionalHeader.SizeOfHeaders );
    // result->headers = ( PIMAGE_NT_HEADERS )&( ( const unsigned char * )( headers ) )[ dos_header->e_lfanew ];
    module.headers := PIMAGE_NT_HEADERS( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( headers ) + dos_header._lfanew );

    // copy sections from DLL file block to new memory location
    if not CopySections( data, old_header, module ) then
      begin
      Result := -1;
      SetLastError( ERROR_SUCCESS );
      MemoryFreeLibrary( module );
      Exit;
      end;

    // adjust base address of imported data
    locationdelta := NativeInt( code ) - old_header.OptionalHeader.ImageBase;
    if locationdelta <> 0 then
      module.isRelocated := PerformBaseRelocation( module, locationdelta )
    else
      module.isRelocated := True;

    result := 0;
  except
    // cleanup
    MemoryFreeLibrary( module );
    Exit;
  end;
end;

{$IFDEF MANIFEST}
{$IF NOT DECLARED( tagACTCTXA )}
{$DEFINE ActCtx_NeedsInit}
type
  PULONG_PTR = PULONG;
var
  CreateActCtx     : function( ActCtx: Pointer{PACTCTX} ): THandle; stdcall;
  ReleaseActCtx    : procedure( hActCtx: THandle ); stdcall;
  ActivateActCtx   : function( hActCtx: THandle; lpCookie: PULONG_PTR ): BOOL; stdcall;
  DeactivateActCtx : function( dwFlags: DWORD; ulCookie: THandle ): BOOL; stdcall;
{$IFEND}
// Loading from HMODULE only works for FULLY initialized Modules
// LOAD_LIBRARY_AS_DATAFILE isnt sufficient
function LoadManifest( Module : THandle; var hActCtx : THandle; Cookie : PULONG_PTR; TempFile : boolean = True ) : boolean;
  {$IF NOT DECLARED(tagACTCTXA)}
  const
    ACTCTX_FLAG_RESOURCE_NAME_VALID           = $00000008;
    ACTCTX_FLAG_HMODULE_VALID                 = $00000080;
  type
    tagACTCTXA = record
      cbSize: ULONG;
      dwFlags: DWORD;
      lpSource: LPCSTR;
      wProcessorArchitecture: WORD;
      wLangId: LANGID;
      lpAssemblyDirectory: LPCSTR;
      lpResourceName: LPCSTR;
      lpApplicationName: LPCSTR;
      hModule: HMODULE;
    end;
    tagACTCTXW = record
      cbSize: ULONG;
      dwFlags: DWORD;
      lpSource: LPCWSTR;
      wProcessorArchitecture: WORD;
      wLangId: LANGID;
      lpAssemblyDirectory: LPCWSTR;
      lpResourceName: LPCWSTR;
      lpApplicationName: LPCWSTR;
      hModule: HMODULE;
    end;
    TActCtx = {$IFDEF UNICODE}tagACTCTXW{$ELSE}tagACTCTXA{$ENDIF};
    PActCtx = {$IFDEF UNICODE}^tagACTCTXW{$ELSE}^tagACTCTXA{$ENDIF};
  {$IFEND}

  function ExtractManifest( Module : HMODULE; FileName : string ) : boolean;
    function SetPrivilege( Privilege: PChar; EnablePrivilege: Boolean; out PreviousState: Boolean ): DWORD;
    var
      Token: THandle;
      NewState: TTokenPrivileges;
      Luid: TLargeInteger;
      PrevState: TTokenPrivileges;
      Return: DWORD;
    begin
      PreviousState := True;
      if ( GetVersion( ) > $80000000 ) then // Win9x 
        Result := ERROR_SUCCESS
      else
      begin // WinNT
        if not OpenProcessToken( GetCurrentProcess( ), MAXIMUM_ALLOWED, Token ) then
          Result := GetLastError( )
        else
        try
          if not LookupPrivilegeValue( nil, Privilege, Luid ) then
            Result := GetLastError( )
          else
          begin
            NewState.PrivilegeCount := 1;
            NewState.Privileges[0].Luid := Luid;
            if EnablePrivilege then
              NewState.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
            else
              NewState.Privileges[0].Attributes := 0;
            if not AdjustTokenPrivileges( Token, False, NewState,
              SizeOf( TTokenPrivileges ), PrevState, Return ) then
              Result := GetLastError( )
            else
            begin
              Result := ERROR_SUCCESS;
              PreviousState := ( PrevState.Privileges[0].Attributes and SE_PRIVILEGE_ENABLED <> 0 );
            end;
          end;
        finally
          CloseHandle( Token );
        end;
      end;
    end;
  var
    HRes : HRSRC;
    HG   : HGlobal;
  f      : Textfile;
    S    : String;
    B    : Boolean;
  begin
    result := False;
    if ( Module = 0 ) OR ( Module = INVALID_HANDLE_VALUE ) then
      Exit;
    if ( FileName = '' ) then
      Exit;

    SetPrivilege( 'SeDebugPrivilege', True, B );
//    if IsExe( Module ) then
//      HRes := FindResource( Module, PChar( 1 ), PChar( 24 ){RT_MANIFEST} )
//    else
  HRes := FindResource( Module, PChar( 2 ), PChar( 24 ){RT_MANIFEST} );
  if ( HRes <> 0 ) then
    begin
      HG := LoadResource( Module, HRes );
    if HG = 0 then
      begin
        SetPrivilege( 'SeDebugPrivilege', B, B );
        Exit;
        end;
      try
        SetString( S, PAnsiChar( LockResource( HG ) ), SizeOfResource( Module, HRes ) );
      finally
        UnlockResource( HG );
        FreeResource( HG );
      end;

      try
        AssignFile( f, FileName );
      ReWrite( f );
      WriteLn( f, S );
      Flush( f );
        result := True;
    finally
      CloseFile( f );
    end;
      end;
    SetPrivilege( 'SeDebugPrivilege', B, B );
  end;
const
  TMP_MANIFEST = 'tmp.manifest';
var
  ActCtx : TActCtx;
{$IFDEF ActCtx_NeedsInit}
  tModule: THandle;
{$ENDIF ActCtx_NeedsInit}
begin
  result := false;
  hActCtx := INVALID_HANDLE_VALUE;
  if ( Cookie = nil ) then
    Exit;
  Cookie^ := 0;
  {$IFDEF ActCtx_NeedsInit}
  if NOT Assigned( CreateActCtx ) then
    begin
    tModule      := GetModuleHandle( kernel32 );
    CreateActCtx := GetProcAddress( tModule, 'CreateActCtx' + {$IFDEF UNICODE}'W'{$ELSE}'A'{$ENDIF} );
    if Assigned( CreateActCtx ) then
      begin
      ReleaseActCtx    := GetProcAddress( tModule, 'ReleaseActCtx' );
      ActivateActCtx   := GetProcAddress( tModule, 'ActivateActCtx' );
      DeactivateActCtx := GetProcAddress( tModule, 'DeactivateActCtx' );
      end
    else
      Exit;
    end;
  {$ENDIF}

  FillChar( ActCtx, SizeOf( ActCtx ), 0 );
  ActCtx.cbSize := SizeOf( ActCtx );
  if TempFile then
    begin
    if NOT ExtractManifest( Module, TMP_MANIFEST ) then
      Exit;
    ActCtx.dwFlags  := 0;
    actCtx.lpSource := TMP_MANIFEST;
    end
  else
    begin
    ActCtx.dwFlags        := ACTCTX_FLAG_RESOURCE_NAME_VALID or ACTCTX_FLAG_HMODULE_VALID;
    if ( Module = 0 ) OR ( Module = INVALID_HANDLE_VALUE ) then
      ActCtx.hModule      := HInstance
    else
      ActCtx.hModule      := Module;
//    if IsExe( Module ) then
//      ActCtx.lpResourceName := MakeIntResource( 1 )
//    else
      ActCtx.lpResourceName := MakeIntResource( 2 );
    end;

  SetLastError( ERROR_SUCCESS );
  hActCtx := CreateActCtx( @ActCtx );
  result := ( hActCtx <> INVALID_HANDLE_VALUE ) AND ActivateActCtx( hActCtx, Cookie );

  if TempFile then
    DeleteFile( TMP_MANIFEST );
end;

function UnloadManifest( hActCtx : THandle; Cookie : THandle ) : boolean;
begin
  result := ( Cookie = 0 ) AND ( ( hActCtx <> 0 ) OR ( hActCtx <> INVALID_HANDLE_VALUE ) );
  if ( Cookie <> 0 ) then
    result := DeactivateActCtx( 0, Cookie );

  if ( hActCtx <> INVALID_HANDLE_VALUE ) then
    ReleaseActCtx( hActCtx );
end;
{$ENDIF}

function MemoryLoadLibrary_2( var module: PMemoryModule; Code : Pointer ): ShortInt; stdcall;
var
  DllEntry    : TDllEntryProc;
  successfull : Boolean;
{$IFDEF MANIFEST}
  hActCtx     : THandle;
  Cookie      : NativeUInt;
{$ENDIF}
{$IF NOT Declared( SysUtils )}
  S           : string;
{$IFEND NOT Declared( SysUtils )}
begin
  result := -19;
  if NOT Assigned( Module ) then
    Exit;
  if NOT Assigned( Code ) then
    Exit;

  {$IFDEF MANIFEST}
  try
    LoadManifest( THandle( module^.codeBase ), hActCtx, @Cookie );
  except
  end;
  {$ENDIF}

  result := -18;
  try
    // load required dlls and adjust function table of imports
    if not BuildImportTable( module ) then
      begin
      result := -17;
  {$IFDEF MANIFEST}
  try
    UnloadManifest( hActCtx, Cookie );
  except
  end;
  {$ENDIF}
      SetLastError( ERROR_SUCCESS );
      MemoryFreeLibrary( module );
      Exit;
      end;
  except
    // cleanup
    MemoryFreeLibrary( module );
    Exit;
  end;

  {$IFDEF MANIFEST}
  try
    UnloadManifest( hActCtx, Cookie );
  except
  end;
  {$ENDIF}

  result := -16;
  try
    // mark memory pages depending on section headers and release
    // sections that are marked as "discardable"
    if not FinalizeSections( module ) then
      begin
      result := -15;
      SetLastError( ERROR_SUCCESS );
      MemoryFreeLibrary( module );
      Exit;
      end;

    // TLS callbacks are executed BEFORE the main loading
    ExecuteTLS( module );
//    if not ExecuteTLS( module ) then
//      begin
//      result := -14;
//      SetLastError( ERROR_SUCCESS );
//      MemoryFreeLibrary( module );
//      Exit;
//      end;
  except
    // cleanup
    MemoryFreeLibrary( module );
    Exit;
  end;

  // get entry point of loaded library
  result := -13;
  if module.headers.OptionalHeader.AddressOfEntryPoint <> 0 then
    begin
    @DllEntry := Pointer( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( code ) + module.headers.OptionalHeader.AddressOfEntryPoint );
//    successfull := false;
    try
      successfull := DllEntry( HINST( code ), DLL_PROCESS_ATTACH, nil ); // notify library about attaching to process
    except
    {$IF Declared( SysUtils )}
      on E : Exception do
        begin
        if ( E.ClassName = 'EExternalException' ) AND ( E.Message = 'External exception E06D7363' ) then // Win-SxS
          successfull := True
        else
          begin
          result := -12;
          MemoryFreeLibrary( module );
          Exit;
          end;
        end;
    {$ELSE}
      SetLength( S, 255 );
      SetLength( S, ExceptionErrorMessage( ExceptObject, ExceptAddr, PChar( S ), Length( S ) ) );
      if ( ExceptObject.ClassName = 'EExternalException' ) AND ( S = 'External exception E06D7363' ) then // Win-SxS
        successfull := True
      else
        begin
        result := -12;
        MemoryFreeLibrary( module );
        Exit;
        end;
    {$IFEND}
    end;

    if successfull then
      SetLastError( ERROR_SUCCESS )
    else
      begin
      result := -11;
      SetLastError( ERROR_DLL_INIT_FAILED );
      MemoryFreeLibrary( module );
      Exit;
      end;

    module.initialized := True;
    result := 0;
    end;
end;

{$IFDEF ALLOW_LOAD_FILES}
function FileToPointer( lpFileStr: String; var Data : PByte ) : Cardinal;
var
  H   : THandle;
  Cnt : Cardinal;
begin
  result := 0;
  Data   := nil;
  if NOT FileExists( lpFileStr ) then
    Exit;

  H := CreateFile( PChar( lpFileStr ), GENERIC_READ, FILE_SHARE_DELETE OR FILE_SHARE_READ OR FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
  if ( H = 0 ) OR ( H = INVALID_HANDLE_VALUE ) then
    Exit;

  result := GetFileSize( H, nil );
  GetMem( Data, result );
  if NOT ReadFile( H, Data^, result, Cnt, nil ) OR ( result <> Cnt ) then
    result := 0;
  CloseHandle( H );
end;

function MemoryLoadLibraryFile( FileName : string; var Module : PMemoryModule ): ShortInt; stdcall;
var
  Data    : Pointer;
  Cnt     : Cardinal;
  SPPath  : array of char;
  PathStr : PChar;
begin
  result := -32;
  Module := nil;
  if NOT FileExists( FileName ) then
    begin
    // EnvironmentPath
    Cnt := SearchPath( nil, PChar( Filename ), nil, 0, nil, PathStr );
    if ( Cnt = 0 ) then
      Exit;
    SetLength( SPPath, Cnt );
    if ( SearchPath( nil, PChar( Filename ), nil, 255, @SPPath[ 0 ], PathStr ) > 0 ) then
      FileName := string( SPPath );
    SetLength( SPPath, 0 );

    if NOT FileExists( FileName ) then
      Exit;
    end;

  Data := nil;
  FileToPointer( FileName, PByte( Data ) );
  if NOT Assigned( Data ) then
    begin
    result := -31;
    Exit;
    end;

  result := MemoryLoadLibrary( Data, Module );
  {$IFDEF GetModuleHandle}
  if ( result = 0 ) then
    ModuleManager.Add( Module, nil{Data}, FileName, False{IsResource} );
  {$ENDIF GetModuleHandle}
  FreeMem( Data );
end;
{$ENDIF}

{$IFDEF LOAD_FROM_RESOURCE}
{$IF Declared( Classes )}
type
  TMemoryStream_ = class( Classes.TMemoryStream );
{$IFEND Declared( Classes )}

function ExtractPointer( Source : Pointer; Len : Cardinal; var Decompressed : Pointer; Password : string = ''; ArchiveID : Byte = 0 ) : Int64;
  function ExtractZLIB( Source : Pointer; Len : Cardinal; var Decompressed : Pointer; ZLibHeader: Boolean = True ) : Int64;
  {$IF NOT Declared( MAX_WBITS )}
  const
    MAX_WBITS = 15; // 32K LZ77 window
  {$IFEND MAX_WBITS}
  var
    strm : TZStreamRec;
    {$IF Declared( inflateInit2_ )}
    Bits : integer;
    {$IFEND inflateInit2_}
    L    : {$IF Declared( zlibCompileFlags )}Cardinal{$ELSE}Integer{$IFEND}; // Native D7 uses Integer while JCL uses Cardinal 
  begin
    result := -10;
    if NOT Assigned( Source ) OR ( Len = 0 ) then
      Exit;
    if Assigned( Decompressed ) then
      ReallocMem( Decompressed, 0 );
    // Initialize ZLib StreamRecord
    FillChar( strm, SizeOf( strm ), 0 );
    strm.next_in   := Source;
    strm.avail_in  := Len;

    {$IF Declared( inflateInit2_ )}
    if ZLibHeader then
      Bits := MAX_WBITS
    else
      Bits := -MAX_WBITS; // no zLib header => .zip compatible

    Result := inflateInit2_( strm, Bits, ZLIB_VERSION, sizeof(TZStreamRec) );
    {$ELSE}
    Result := inflateInit_( strm, ZLIB_VERSION, sizeof(TZStreamRec) );
    {$IFEND inflateInit2_}
    if Result < 0 then
      Exit;

    L := Len*4;
    GetMem( Decompressed, L );

    strm.next_out := Decompressed;
    strm.avail_out := L;
    while ( strm.avail_in > 0 ) do
      begin
      if ( strm.avail_out = 0 ) then
        begin
        strm.avail_out := strm.avail_in*4;
        try
          ReallocMem( Decompressed, L+strm.avail_out );
        except
          ReallocMem( Decompressed, 0 );
          inflateEnd( strm );
          result := 0;
          Exit;
        end;
        strm.next_out := Decompressed;
        Inc( strm.next_out, L );
        L := L+strm.avail_out;
        end;

      Result := inflate( strm, Z_NO_FLUSH );
      case Result of
        Z_STREAM_END             : break;
        Z_VERSION_ERROR..Z_ERRNO : begin
                                   ReallocMem( Decompressed, 0 );
                                   inflateEnd( strm );
                                   result := 0;
                                   Exit;
                                   end;
      end;
      end;

    Result := strm.total_out;
    if ( L <> Result ) then
      ReallocMem( Decompressed, Result );
    inflateEnd( strm );
  end;

  {$IF Declared( inflateInit2_ )}
  function ExtractGZIP( Source : PByte; Len : Cardinal; var Decompressed : Pointer{$IF Declared( CRC32 )}; CheckHeaderCRC : boolean = True{$IFEND} ) : Int64;
    function ReadBuffer( var Source : PByte; var Len : Cardinal; const Dest : PByte; count : Cardinal{$IF Declared( CRC32 )}; var HeaderCRC : Cardinal; ComputeHeaderCRC : boolean = True{$IFEND} ) : boolean; {$IF CompilerVersion >= 23}inline;{$IFEND}
    begin
      result := False;
      if ( Count > Len ) then
        Exit;
      Move( Source^, dest^, count );
      Inc( Source, count );
      Dec( Len, count );

      {$IF Declared( CRC32 )}
      if ComputeHeaderCRC then
        HeaderCRC := crc32( HeaderCRC, {$IFDEF JCL}PBytef{$ELSE}PByte{$ENDIF}( Dest ), count );
      {$IFEND CRC32}

      result := True;
    end;
    function ReadCString( var Source : PByte; var Len : Cardinal{$IF Declared( CRC32 )}; var HeaderCRC : Cardinal; ComputeHeaderCRC : boolean = True{$IFEND} ): AnsiString; {$IF CompilerVersion >= 23}inline;{$IFEND}
    var
      Buf : AnsiChar;
    begin
      Result := '';
      Buf    := #0;
      repeat
        if NOT ReadBuffer( Source, Len, @Buf, SizeOf( Buf ){$IF Declared( CRC32 )}, HeaderCRC, ComputeHeaderCRC{$IFEND} ) then
          Break;
        if Buf = #0 then
          Break;
        Result := Result + Buf;
      until False;
    end;
  type
    TJclGZIPHeader = packed record
      ID1: Byte;
      ID2: Byte;
      CompressionMethod: Byte;
      Flags: Byte;
      ModifiedTime: Cardinal;
      ExtraFlags: Byte;
      OS: Byte;
    end;
  const
    // ID1 and ID2 fields
    JCL_GZIP_ID1 = $1F; // value for the ID1 field
    JCL_GZIP_ID2 = $8B; // value for the ID2 field
    // Compression Model field
    JCL_GZIP_CM_DEFLATE = 8; // Zlib classic

    // Flags field : extra fields for the header
    JCL_GZIP_FLAG_CRC     = $02; // a CRC16 for the header is present  
    JCL_GZIP_FLAG_EXTRA   = $04; // extra fields present
    JCL_GZIP_FLAG_NAME    = $08; // original file name is present
    JCL_GZIP_FLAG_COMMENT = $10; // comment is present
  var
    Header           : TJclGZIPHeader;
    ExtraFieldLength : Word;
    ExtraField       : string;
    OriginalFileName : {$IF Declared( TFileName )}TFileName{$ELSE}String{$IFEND};
    Comment          : String;
  {$IF Declared( CRC32 )}
    HeaderCRC        : Cardinal;
  {$IFEND CRC32}
    StoredHeaderCRC16: Word;
  begin
    result := -17;
  {$IF Declared( CRC32 )}
    HeaderCRC := 0; // crc32(0, nil, 0);
  {$IFEND CRC32}
    if NOT ReadBuffer( Source, Len, @Header, SizeOf( Header ){$IF Declared( CRC32 )}, HeaderCRC, CheckHeaderCRC{$IFEND} ) then
      Exit;

    if ( Header.ID1 <> JCL_GZIP_ID1 ) or ( Header.ID2 <> JCL_GZIP_ID2 ) then
      begin
      result := -16;
      Exit;
  //    raise EJclCompressionError.CreateResFmt( @RsCompressionGZipInvalidID, [ Header.ID1, Header.ID2 ] );
      end;

    if ( Header.CompressionMethod <> JCL_GZIP_CM_DEFLATE ) then
      begin
      result := -15;
      Exit;
  //    raise EJclCompressionError.CreateResFmt( @RsCompressionGZipUnsupportedCM, [ Header.CompressionMethod ] );
      end;

    if ( ( Header.Flags and JCL_GZIP_FLAG_EXTRA ) <> 0 ) then
      begin
      ExtraFieldLength := 0;
      if NOT ReadBuffer( Source, Len, @ExtraFieldLength, SizeOf( ExtraFieldLength ){$IF Declared( CRC32 )}, HeaderCRC, CheckHeaderCRC{$IFEND} ) then
        begin
        result := -14;
        Exit;
        end;

      SetLength( ExtraField, ExtraFieldLength );
      if NOT ReadBuffer( Source, Len, @ExtraField[1], ExtraFieldLength{$IF Declared( CRC32 )}, HeaderCRC, CheckHeaderCRC{$IFEND} ) then
        begin
        result := -13;
        Exit;
        end;
      end;

    if ( ( Header.Flags and JCL_GZIP_FLAG_NAME ) <> 0 ) then
      OriginalFileName := {$IF Declared( TFileName )}TFileName{$ELSE}String{$IFEND}( ReadCString( Source, Len{$IF Declared( CRC32 )}, HeaderCRC, CheckHeaderCRC{$IFEND} ) );
    if ( ( Header.Flags and JCL_GZIP_FLAG_COMMENT ) <> 0 ) then
      Comment := string( ReadCString( Source, Len{$IF Declared( CRC32 )}, HeaderCRC, CheckHeaderCRC{$IFEND} ) );

  //{$IF Declared( CRC32 )}
  //  if CheckHeaderCRC then
  //    ComputedHeaderCRC16 := HeaderCRC and $FFFF;
  //{$IFEND CRC32}

    if ( ( Header.Flags and JCL_GZIP_FLAG_CRC ) <> 0 ) then
      begin
      if NOT ReadBuffer( Source, Len, @StoredHeaderCRC16, SizeOf( StoredHeaderCRC16 ){$IF Declared( CRC32 )}, HeaderCRC, False{$IFEND} ) then
        begin
        result := -12;
        Exit;
        end;
      {$IF Declared( CRC32 )}
      if CheckHeaderCRC and ( {ComputedHeaderCRC16}( HeaderCRC and $FFFF ) <> StoredHeaderCRC16 ) then
        begin
        result := -11;
        Exit;
  //      raise EJclCompressionError.CreateRes( @RsCompressionGZipHeaderCRC );
        end;
      {$IFEND CRC32}
      end;

    result := ExtractZLIB( Source, Len, Decompressed, {ZLibHeader}False );
  end;
  {$IFEND inflateInit2_}

  {$IF Declared( Classes )}
  function ExtractStreamGZIP( var ASourceStream : TStream ) : Integer;
  var
    Archive : TJclDecompressStream;
    Header  : Array [ 0..3 ] of AnsiChar;
    Dst     : TStream;
  begin
    result := -5;
    if NOT Assigned( ASourceStream ) then
      Exit;
    Dst := TMemoryStream.Create;

    if ( ASourceStream.Position = ASourceStream.Size ) then
      ASourceStream.Position := 0;    

    result := -4;
    if ( ASourceStream.Size-ASourceStream.Position <= Length( Header ) ) then
      Exit;
    ASourceStream.Read( Header[ Low( Header ) ], Length( Header ) );
  //  ASourceStream.Position := ASourceStream.Position-Length( Header );
    result := -3;
    if ( Header <> 'GZIP' ) then
      Exit;

    Archive := TJclGZIPDecompressionStream.Create( ASourceStream );
    Archive.SaveToStream( Dst );

    Archive.Free;
    ASourceStream.free;
    ASourceStream := dst;
    ASourceStream.Position := 0;

    result := 0;
  end;

  function ExtractStream7z( var ASourceStream : TStream; Password : string = ''; ArchiveID : Byte = 0 ) : Integer;
  var
    Archive : TJclDecompressArchive;
    Header  : Array [0..1] of AnsiChar;
    Dst     : TStream;
  begin
    {$IF Declared( DLL7z_IsInitDLL )}
    result := -6;
    if NOT DLL7z_IsInitDLL then
      Exit;
    {$IFEND}

    result := -5;
    if NOT Assigned( ASourceStream ) then
      Exit;
    Dst := TMemoryStream.Create;

    if ( ASourceStream.Position = ASourceStream.Size ) then
      ASourceStream.Position := 0;

    result := -4;
    if ( ASourceStream.Size-ASourceStream.Position <= Length( Header ) ) then
      Exit;
    ASourceStream.Read( Header[ Low( Header ) ], Length( Header ) );
    ASourceStream.Position := ASourceStream.Position-Length( Header );

    result := -3;
    if ( Header <> '7z' ) AND ( Header <> '7Z' ) then
      Exit;

    Archive := TJcl7zDecompressArchive.Create( ASourceStream, 0, False );
    Archive.Password := Password;
    try
      Archive.ListFiles;
    except
      Archive.Free;
      result := -2;
      Exit;
    end;

    if ( ArchiveID < Archive.ItemCount ) then
      begin
      Archive.Items[ ArchiveID ].Stream     := Dst;
      Archive.Items[ ArchiveID ].OwnsStream := false;
      Archive.Items[ ArchiveID ].Selected   := True;
      Archive.ExtractSelected;
      Dst.Position := 0;

      ASourceStream.free;
      ASourceStream := dst;

      result := 0;
      end
    else
      result := -1;
    Archive.Free;
  end;
  {$IFEND}
const
  PREFIX_ : Array [ 0..3{$IFDEF LZMA}+2{$ENDIF} ] of AnsiString = ( 'mz', '7z', 'zlib', 'gzip'{$IFDEF LZMA}, 'lzma', 'lzm2'{$ENDIF} );
{$IF Declared( Classes )}
var
  S : TStream;
{$IFEND}
begin
  result := -5;
  if NOT Assigned( Source ) then
    Exit;

  result := -4;
  if ( Len < 4 ) then
    Exit;
  result := -3;
  if ( CompareStringA( LOCALE_USER_DEFAULT, NORM_IGNORECASE, Source, Length( PREFIX_[ 0 ] ), PAnsiChar( PREFIX_[ 0 ] ), Length( PREFIX_[ 0 ] ) ) = 2 ) then // Uncompressed
    begin
//    ReallocMem( Decompressed, Len );
//    Move( Source^, Decompressed^, Len );
    Decompressed := Source;
    result := Len;
    Exit;
    end
  else if ( CompareStringA( LOCALE_USER_DEFAULT, NORM_IGNORECASE, Source, Length( PREFIX_[ 1 ] ), PAnsiChar( PREFIX_[ 1 ] ), Length( PREFIX_[ 1 ] ) ) = 2 ) then // 7z
    begin
    {$IF Declared( Classes )}
    S := TMemoryStream.Create;
    TMemoryStream_( S ).SetPointer( Source, Len );
//    S.Write( Source^, Len );
    result := ExtractStream7z( S, Password, ArchiveID );
    if ( result = 0 ) then
      begin
      ReallocMem( Decompressed, S.Size );
      Move( TMemoryStream( S ).Memory^, Decompressed^, S.Size );
      result := S.Size;
      end;
    S.free;
    {$ELSE}
    result := -6;
    {$IFEND}
    end
  else if ( CompareStringA( LOCALE_USER_DEFAULT, NORM_IGNORECASE, Source, Length( PREFIX_[ 2 ] ), PAnsiChar( PREFIX_[ 2 ] ), Length( PREFIX_[ 2 ] ) ) = 2 ) then // ZLib
    result := ExtractZLIB( {$IF CompilerVersion < 23}PByte( PAnsiChar( Source )+Length( PREFIX_[ 2 ] ) ){$ELSE}PByte( Source )+Length( PREFIX_[ 2 ] ){$IFEND}, Len-Length( PREFIX_[ 2 ] ), Decompressed )
  {$IFDEF lzma}
  else if ( CompareStringA( LOCALE_USER_DEFAULT, NORM_IGNORECASE, Source, Length( PREFIX_[ 4 ] ), PAnsiChar( PREFIX_[ 4 ] ), Length( PREFIX_[ 4 ] ) ) = 2 ) then // LZMA
    result := ExtractLZMA( {$IF CompilerVersion < 23}PByte( PAnsiChar( Source )+Length( PREFIX_[ 4 ] ) ){$ELSE}PByte( Source )+Length( PREFIX_[ 4 ] ){$IFEND}, Len-Length( PREFIX_[ 4 ] ), Decompressed )
  else if ( CompareStringA( LOCALE_USER_DEFAULT, NORM_IGNORECASE, Source, Length( PREFIX_[ 5 ] ), PAnsiChar( PREFIX_[ 5 ] ), Length( PREFIX_[ 5 ] ) ) = 2 ) then // LZMA2
    result := ExtractLZMA2( {$IF CompilerVersion < 23}PByte( PAnsiChar( Source )+Length( PREFIX_[ 5 ] ) ){$ELSE}PByte( Source )+Length( PREFIX_[ 5 ] ){$IFEND}, Len-Length( PREFIX_[ 5 ] ), Decompressed )
  {$ENDIF lzma}
  else if ( CompareStringA( LOCALE_USER_DEFAULT, NORM_IGNORECASE, Source, Length( PREFIX_[ 3 ] ), PAnsiChar( PREFIX_[ 3 ] ), Length( PREFIX_[ 3 ] ) ) = 2 ) then // Gzip
    {$IF Declared( ExtractGZIP )}
    result := ExtractGZIP( {$IF CompilerVersion < 23}PByte( PAnsiChar( Source )+Length( PREFIX_[ 3 ] ) ){$ELSE}PByte( Source )+Length( PREFIX_[ 3 ] ){$IFEND}, Len-Length( PREFIX_[ 3 ] ), Decompressed );
    {$ELSEIF Declared( Classes )}
    begin
    S := TMemoryStream.Create;
    TMemoryStream_( S ).SetPointer( PByte( {$IF CompilerVersion < 23}PAnsiChar( Source )+Length( PREFIX_[ 3 ] ){$ELSE}PByte( Source )+Length( PREFIX_[ 3 ] ){$IFEND} ), Len-Length( PREFIX_[ 3 ] ) );
//    S.Write( PByte( {$IF CompilerVersion < 23}PAnsiChar( Source )+Length( PREFIX_[ 3 ] ){$ELSE}PByte( Source )+Length( PREFIX_[ 3 ] ){$IFEND} )^, Len-Length( PREFIX_[ 2 ] ) );
    result := ExtractStreamGZIP( S );
    if ( result = 0 ) then
      begin
      ReallocMem( Decompressed, S.Size );
      Move( TMemoryStream( S ).Memory^, Decompressed^, S.Size );
      result := S.Size;
      end;
    S.free;
    end;
    {$ELSE}
    result := -6;
    {$IFEND}
end;

function MemoryResourceExists( var ResourceName : string ) : HRSRC;
  {$IF NOT Declared( ChangeFileExt )}
  function ChangeFileExt( FileName : string; Extension : String = '' ) : String;
    function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
    var
      I,X: Integer;
      Len, LenSubStr: Integer;
    begin
      if Offset = 1 then
        Result := Pos(SubStr, S)
      else
      begin
        I := Offset;
        LenSubStr := Length(SubStr);
        Len := Length(S) - LenSubStr + 1;
        while I <= Len do
        begin
          if S[I] = SubStr[1] then
          begin
            X := 1;
            while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
              Inc(X);
            if (X = LenSubStr) then
            begin
              Result := I;
              exit;
            end;
          end;
          Inc(I);
        end;
        Result := 0;
      end;
    end;
  const
    TD = '\';
    P  = '.';
  var
    i, j : Integer;
  begin
    i := Pos( TD, FileName );
    if ( i > 0 ) then
      begin
      j := i;
      while ( j > 0 ) do
        begin
        j := PosEx( TD, FileName, j+1 );
        if ( j > 0 ) then
          i := j;
        end;
      end;

    j := PosEx( P, FileName, i+1 );
    if ( j > 0 ) then
      begin
      i := j;
      while ( j > 0 ) do
        begin
        j := PosEx( TD, FileName, j+1 );
        if ( j > 0 ) then
          i := j;
        end;
      end;
    if ( i = 0 ) then
      result := FileName + Extension
    else
      result := Copy( FileName, 1, i-1 ) + Extension;
  end;
  {$IFEND}
const
  NamePrefix = 'DLL'; // Resource DLLs need to start with a Letter
  RES_TYPE_  = 'DLL'; // RT_RCDATA{10};  
begin
  result := 0;
  if ( ResourceName = '' ) then
    Exit;
  ResourceName := ChangeFileExt( ResourceName, '' );
  {$IF Declared( CharInSet )}
  if not CharInSet( ResourceName[ {$IF CompilerVersion >= 24}Low( ResourceName ){$ELSE}1{$IFEND} ], ['a'..'z','A'..'Z'] ) then
  {$ELSE}
  if ( ResourceName[ {$IF CompilerVersion >= 24}Low( ResourceName ){$ELSE}1{$IFEND} ] < 'a' ) and
     ( ResourceName[ {$IF CompilerVersion >= 24}Low( ResourceName ){$ELSE}1{$IFEND} ] > 'z' ) and
     ( ResourceName[ {$IF CompilerVersion >= 24}Low( ResourceName ){$ELSE}1{$IFEND} ] < 'A' ) and
     ( ResourceName[ {$IF CompilerVersion >= 24}Low( ResourceName ){$ELSE}1{$IFEND} ] > 'Z' ) then
  {$IFEND}
    ResourceName := NamePrefix + ResourceName;

  result := FindResource( hInstance, PChar( ResourceName ), RES_TYPE_ );
end;

function MemoryLoadLibrary( ResourceName : string; var Module : PMemoryModule{$IFDEF USE_STREAMS}; Password : string = ''{$ENDIF} ): ShortInt; stdcall;
const
  RES_TYPE_ = 'DLL'; // RT_RCDATA{10};
var
  HRes   : HRSRC;
  HG     : HGlobal;
  Data   : Pointer;
  Extract: Pointer;
  res    : Int64;
begin
  result := -32;
  Module := nil;

  HRes := MemoryResourceExists( ResourceName );
  if ( HRes = 0 ) then
    begin
    SetLastError( ERROR_RESOURCE_NAME_NOT_FOUND{1814} );
    Exit;
    end;

  HG := LoadResource( hInstance, HRes );
  if ( HG = 0 ) then
    begin
    SetLastError( ERROR_INVALID_DATA{13} );
    Exit;
    end;
  Data := LockResource( HG );

  Extract := nil;
  res := ExtractPointer( Data, SizeOfResource( hInstance, HRes ), Extract{$IFDEF USE_STREAMS}, Password{$ENDIF} );
  if ( res > 0 ) then
    begin
    result := MemoryLoadLibrary( Extract, {SizeOfResource( hInstance, HRes ),} Module );
    {$IFDEF GetModuleHandle}
    if ( result = 0 ) then
      ModuleManager.Add( Module, Data, ResourceName, True{IsResource} );
    {$ENDIF GetModuleHandle}
    end
  else if ( res = 0 ) then
    result := -32
  else
    result := res;

  if ( res <= 0 ) then
    SetLastError( ERROR_INVALID_DATA{13} );
  UnlockResource( HG );
  FreeResource( HG );
  if ( Extract <> Data ) then
    ReallocMem( Extract, 0 );
end;
{$ENDIF}

function MemoryLoadLibrary( data: Pointer; var Module : PMemoryModule ): ShortInt; stdcall;
var
  Code : Pointer;
begin
  Code   := nil;
  Module := nil;
  result := MemoryLoadLibrary_1( data, Code, Module );
  if ( result < 0 ) then
    Exit;
  result := MemoryLoadLibrary_2( Module, Code );

  {$IFDEF GetModuleHandle}
  if ( result = 0 ) then
    begin
    if NOT Assigned( ModuleManager ) then
      ModuleManager := tModuleManager.Create;
    ModuleManager.Add( Module, Data, '', False{IsResource} );
    end;
  {$ENDIF GetModuleHandle}
end;

function MemoryGetProcAddress( module: PMemoryModule; const name: PAnsiChar ): Pointer; stdcall;
var
  codebase: Pointer;
  idx: Integer;
  i: Cardinal;
  nameRef: PDWORD;
  ordinal: PWord;
  exportDir: PIMAGE_EXPORT_DIRECTORY;
  directory: PIMAGE_DATA_DIRECTORY;
  temp: PDWORD;
begin
  Result := nil;
  if NOT Assigned( module ) then
    Exit;
  if ( Name = '' ) then
    Exit;

  codebase := module.codeBase;
  directory := GET_HEADER_DICTIONARY( module, IMAGE_DIRECTORY_ENTRY_EXPORT );
  // no export table found
  if directory.Size = 0 then
    begin
    SetLastError( ERROR_PROC_NOT_FOUND );
    Exit;
    end;

  exportDir := PIMAGE_EXPORT_DIRECTORY( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codebase ) + directory.VirtualAddress );
  // DLL doesn't export anything
  if ( exportDir.NumberOfNames = 0 ) or ( exportDir.NumberOfFunctions = 0 ) then
    begin
    SetLastError( ERROR_PROC_NOT_FOUND );
    Exit;
    end;

  // search function name in list of exported names
  nameRef := Pointer( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codebase ) + {$IF Defined( FPC ) OR ( CompilerVersion < 20 )}Cardinal{$IFEND}( exportDir.AddressOfNames ) );
  ordinal := Pointer( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codebase ) + {$IF Defined( FPC ) OR ( CompilerVersion < 20 )}Cardinal{$IFEND}( exportDir.AddressOfNameOrdinals ) );
  idx := -1;
  for i := 0 to exportDir.NumberOfNames - 1 do
    begin
    if StrComp( name, PAnsiChar( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codebase ) + nameRef^ ) ) = 0 then
      begin
      idx := ordinal^;
      Break;
      end;
    Inc( nameRef );
    Inc( ordinal );
    end;

  // exported symbol not found
  if ( idx = -1 ) then
    begin
    SetLastError( ERROR_PROC_NOT_FOUND );
    Exit;
    end;

  // name <-> ordinal number don't match
  if ( Cardinal( idx ) > exportDir.NumberOfFunctions ) then
    begin
    SetLastError( ERROR_PROC_NOT_FOUND );
    Exit;
    end;

  // AddressOfFunctions contains the RVAs to the "real" functions 
  temp := Pointer( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codebase ) + {$IF Defined( FPC ) OR ( CompilerVersion < 20 )}Cardinal{$IFEND}( exportDir.AddressOfFunctions ) + {$IF Defined( FPC ) OR ( CompilerVersion < 20 )}Cardinal{$IFEND}( idx )*4 );
  Result := Pointer( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codebase ) + temp^ );
end;

procedure MemoryFreeLibrary( var module: PMemoryModule ); stdcall;
var
  i: Integer;
  DllEntry: TDllEntryProc;
begin
  if module = nil then
    Exit;

  if module.initialized then
    begin
    // notify library about detaching from process
    @DllEntry := Pointer( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( module.codeBase ) + module.headers.OptionalHeader.AddressOfEntryPoint );
    DllEntry( HINST( module.codeBase ), DLL_PROCESS_DETACH, nil );
    end;

  if Length( module.modules ) <> 0 then
    begin
    // free previously opened libraries
    for i := Low( module.Modules ) to High( module.Modules ) do
      begin
      if ( module.modules[ i ].Handle <> 0 ) {$IFDEF GetModuleHandle_BuildImportTable}and module.modules[ i ].Free{$ENDIF} then
        FreeLibrary_Internal( module.modules[ i ].Handle );
      end;
    SetLength( module.modules, 0 );
    end;

  if ( module.codeBase <> nil ) then // release memory of library
    VirtualFree( module.codeBase, 0, MEM_RELEASE );

  HeapFree( GetProcessHeap, 0, module );
  {$IFDEF GetModuleHandle}
  ModuleManager.Del( Module );
  {$ENDIF GetModuleHandle}
  Module := nil;  
end;

// Import-Enumeration
function EnumerateImportTable( module: PMemoryModule; var Modules : String; DelimiterString : String = ';' ): Boolean; stdcall;
var
  codebase: Pointer;
  directory: PIMAGE_DATA_DIRECTORY;
  importDesc: PIMAGE_IMPORT_DESCRIPTOR;
begin
  codebase := module.codeBase;
  Result := True;

  directory := GET_HEADER_DICTIONARY( module, IMAGE_DIRECTORY_ENTRY_IMPORT );
  if directory.Size = 0 then
    {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}
    Exit( True );
    {$ELSE}
    begin
    result := True;
    Exit;
    end;
    {$IFEND}

  importDesc := PIMAGE_IMPORT_DESCRIPTOR( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codebase ) + directory.VirtualAddress );
  while ( not IsBadReadPtr( importDesc, SizeOf( IMAGE_IMPORT_DESCRIPTOR ) ) ) and ( importDesc.Name <> 0 ) do
    begin
    if ( Modules = '' ) then
      Modules := PAnsiChar( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codebase ) + importDesc.Name )
    else
      Modules := Modules + DelimiterString + PAnsiChar( {$IF Defined( FPC ) OR ( CompilerVersion >= 20 )}PByte{$ELSE}PAnsiChar{$IFEND}( codebase ) + importDesc.Name );

    Inc( importDesc );
    end; // while
end;

function MemoryEnumerateImports( data: Pointer; var Modules : string; DelimiterString : String = ';' ): ShortInt; stdcall;
var
  Code : Pointer;
  module: PMemoryModule;
{$IFDEF MANIFEST}
  hActCtx     : THandle;
  Cookie      : NativeUInt;
{$ENDIF}
begin
  module  := nil;
  Modules := '';
  Code    := nil;
  result  := MemoryLoadLibrary_1( data, Code, module );
  if ( result = 0 ) AND Assigned( module ) then
    begin
    {$IFDEF MANIFEST}
    try
      LoadManifest( THandle( module^.codeBase ), hActCtx, @Cookie );
    except
    end;
    {$ENDIF}

    if NOT EnumerateImportTable( module, Modules, DelimiterString ) then
      result := -99;

    {$IFDEF MANIFEST}
    try
      UnloadManifest( hActCtx, Cookie );
    except
    end;
    {$ENDIF}
    end;

  MemoryFreeLibrary( module );
end;

{$IFDEF ALLOW_LOAD_FILES}
function MemoryEnumerateImportsFile( FileName : string; var Modules : string; DelimiterString : String = ';' ): ShortInt; stdcall;
var
  Data : Pointer;
begin
  Data := nil;
  FileToPointer( FileName, PByte( Data ) );
  if Assigned( Data ) then
    begin
    result := MemoryEnumerateImports( Data, Modules, DelimiterString );
    FreeMem( Data );
    end
  else
    result := -31;
end;
{$ENDIF}

{$IFDEF LOAD_FROM_RESOURCE}
function MemoryEnumerateImports( ResourceName : string; var Modules : string; DelimiterString : String = ';'{$IFDEF USE_STREAMS}; Password : string = ''{$ENDIF} ): ShortInt; stdcall;
const
  RES_TYPE_ = 'DLL'; // RT_RCDATA{10};
var
  HRes   : HRSRC;
  HG     : HGlobal;
  Data   : Pointer;
  Extract: Pointer;
  res    : Int64;
begin
  result := -32;
  HRes := MemoryResourceExists( ResourceName );
  if ( HRes = 0 ) then
    Exit;

  HG := LoadResource( hInstance, HRes );
  if ( HG = 0 ) then
    Exit;
  Data := LockResource( HG );
  
  Extract := nil;
  res := ExtractPointer( Data, SizeOfResource( hInstance, HRes ), Extract{$IFDEF USE_STREAMS}, Password{$ENDIF} );
  if ( res = 0 ) then
    result := -31
  else if ( res > 0 ) then
    result := MemoryEnumerateImports( Extract, Modules, DelimiterString )
  else
    result := res;
  UnlockResource( HG );
  FreeResource( HG );
  if ( Extract <> Data ) then
    ReallocMem( Extract, 0 );
end;
{$ENDIF}

{$IFDEF GetModuleHandle}
function MemoryGetModuleHandle( data: Pointer ): PMemoryModule; stdcall; {$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}overload;{$IFEND}
begin
  result := nil;
  if NOT Assigned( ModuleManager ) then
    Exit;
  result := ModuleManager.GetHandle( data );
end;

{$IF Defined( ALLOW_LOAD_FILES ) OR Defined( LOAD_FROM_RESOURCE )}
function MemoryGetModuleHandle( FileName : string{$IFDEF LOAD_FROM_RESOURCE}; IsResource : boolean = False{$ENDIF} ): PMemoryModule; stdcall;
begin
  result := nil;
  if NOT Assigned( ModuleManager ) then
    Exit;
  result := ModuleManager.GetHandle( FileName{$IFDEF LOAD_FROM_RESOURCE}, IsResource{$ENDIF} );
end;
{$IFEND}
{$ENDIF GetModuleHandle}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{$IF Defined( GetModuleHandle ) AND ( NOT Defined( FastMM4 ) AND NOT Defined( FastMM5 ) )}
initialization
  if NOT Assigned( ModuleManager ) then // LoadLibrary called before initialization
    ModuleManager := tModuleManager.Create;
  {$IFDEF UnloadAllOnFinalize}
  InitializationDone := True;
  {$ENDIF UnloadAllOnFinalize}

finalization
  {$IFDEF UnloadAllOnFinalize}
  InitializationDone := False;
  {$ENDIF UnloadAllOnFinalize}
  ModuleManager.free;
  ModuleManager := nil;
{$IFEND}

end.
