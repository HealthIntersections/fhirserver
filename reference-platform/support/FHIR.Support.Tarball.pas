unit FHIR.Support.Tarball;

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


interface

uses
   Windows, SysUtils, Classes,
   FHIR.Support.Objects;

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
    FilePos     : int64;             // Position in TAR file
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
    function  ReadFile : String; overload;         // -;-  RawByteString in D2009+. Not active due to FPC unicode architecture not being finalized
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
    procedure AddFile   (Filename : String;  TarFilename : AnsiString = '');
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
function  ConvertFilename       (Filename    : String)          : String;
function  FileTimeGMT           (FileName    : String)          : TDateTime;  overload;
function  FileTimeGMT           (SearchRec   : TSearchRec)      : TDateTime;  overload;
procedure ClearDirRec           (var DirRec  : TTarDirRec);

implementation

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


function ConvertFilename  (Filename : String) : String;
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

function FileTimeGMT (FileName: String): TDateTime;
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
(*$IFDEF MSWINDOWS *)
var
  SystemFileTime: TSystemTime;
(*$ENDIF *)
(*$IFDEF Unix *)
var
  TimeVal  : TTimeVal;
  TimeZone : TTimeZone;
(*$ENDIF *)
begin
  Result := 0.0;
  (*$IFDEF MSWINDOWS *) (*$WARNINGS OFF *)
    if (SearchRec.FindData.dwFileAttributes and faDirectory) = 0 then
      if FileTimeToSystemTime (SearchRec.FindData.ftLastWriteTime, SystemFileTime) then
        Result := EncodeDate (SystemFileTime.wYear, SystemFileTime.wMonth, SystemFileTime.wDay)
                + EncodeTime (SystemFileTime.wHour, SystemFileTime.wMinute, SystemFileTime.wSecond, SystemFileTime.wMilliseconds);
  (*$ENDIF *) (*$WARNINGS ON *)
  (*$IFDEF Unix *)
     if SearchRec.Attr and faDirectory = 0 then
     begin
       FillChar(TimeVal, SizeOf(TimeVal), #0);
       FillChar(TimeZone, SizeOf(TimeZone), #0);
       Result := FileDateToDateTime (SearchRec.Time);
       {$IFDEF Kylix}
       GetTimeOfDay (TimeVal, TimeZone);
       {$else}
       fpGetTimeOfDay (@TimeVal, @TimeZone);
       {$ENDIF}
       Result := Result + TimeZone.tz_minuteswest / (60 * 24);
     end;
  (*$ENDIF *)
end;


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
    FilePos     := 0;
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
  Strg := Trim (StrPas (P));
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
  Strg := Trim (StrPas (P));
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
  StrLCopy (S0, P, MaxLen);
  Strg := Trim (StrPas (S0));
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
  StrLCopy (S0, P, MaxLen);
  Strg := Trim (StrPas (S0));
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
  StrLCopy (TH.Name, PAnsiChar (DirRec.Name), NAMSIZ);
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
  StrLCopy (TH.LinkName, PAnsiChar (DirRec.LinkName), NAMSIZ);
  StrLCopy (TH.Magic, PAnsiChar (DirRec.Magic + #32#32#32#32#32#32#32#32), 7);
  StrLCopy (TH.UName, PAnsiChar (DirRec.UserName), TUNMLEN);
  StrLCopy (TH.GName, PAnsiChar (DirRec.GroupName), TGNMLEN);
  OctalN (DirRec.MajorDevNo, @TH.DevMajor, 8);
  OctalN (DirRec.MinorDevNo, @TH.DevMinor, 8);
  StrMove (TH.ChkSum, CHKBLANKS, 8);

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
  Reset;
end;


constructor TTarArchive.Create (Filename : String; FileMode : WORD);
begin
  inherited Create;
  FStream     := TFileStream.Create (Filename, FileMode);
  FOwnsStream := TRUE;
  Reset;
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
  CurFilePos   : integer;
  Header       : TTarHeader ABSOLUTE Rec;
  I            : integer;
  HeaderChkSum : WORD;
  Checksum     : CARDINAL;
begin
  // --- Scan until next pointer
  if FBytesToGo > 0 then
    FStream.Seek (Records (FBytesToGo) * RECORDSIZE, soFromCurrent);

  // --- EOF reached?
  Result := FALSE;
  CurFilePos := FStream.Position;
  TRY
    FStream.ReadBuffer (Rec, RECORDSIZE);
    if Rec [0] = #0 then EXIT;   // EOF reached
  EXCEPT
    EXIT;   // EOF reached, too
    end;
  Result := TRUE;

  ClearDirRec (DirRec);

  DirRec.FilePos := CurFilePos;
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
  StrMove (Header.ChkSum, CHKBLANKS, 8);
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
  RestBytes : integer;
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
  RestBytes : integer;
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


function  TTarArchive.ReadFile : String;
          // Reads file data for the last Directory Record. The entire file is returned
          // as a large ANSI String.
var
  RestBytes : integer;
begin
  if FBytesToGo = 0 then EXIT;
  RestBytes := Records (FBytesToGo) * RECORDSIZE - FBytesToGo;
  SetLength (Result, FBytesToGo);
  FStream.ReadBuffer (PAnsiChar (Result)^, FBytesToGo);
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
    FStream.Seek (NewPos, soFromBeginning);
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


procedure TTarWriter.AddFile   (Filename : String;  TarFilename : AnsiString = '');
var
  S    : TFileStream;
  Date : TDateTime;
begin
  Date := FileTimeGMT (Filename);
  if TarFilename = '' then
    TarFilename := ConvertFilename (Filename)
  else TarFilename := ConvertFilename (TarFilename);
  S := TFileStream.Create (Filename, fmOpenRead or fmShareDenyWrite);
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


END.

© 2018 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
API
Training
Shop
Blog
About
Press h to open a hovercard with more details.
*)
implementation

end.
