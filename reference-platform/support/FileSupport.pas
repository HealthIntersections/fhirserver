Unit FileSupport;

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
  Windows, SysUtils,
  StringSupport, MathSupport, MemorySupport, DateSupport;

Type
  TFileAttribute =
    (FileAttributeArchive, FileAttributeHidden, FileAttributeNormal, FileAttributeOffline, FileAttributeCompressed, FileAttributeReadOnly,
     FileAttributeSystem, FileAttributeTemporary, FileAttributeFolder, FileAttributeWriteThrough);

  TFileAttributes = Set Of TFileAttribute;

  TFileTime = Record
    Value : Int64;
  End;

  TFileHandle = Record
    Value : Cardinal;
  End;

  TFileVersion = Record
    Major : Integer;
    Minor : Integer;
    Release : Integer;
    Build : Integer;
  End;


Function FileGetAttributes(Const sFilename : String) : TFileAttributes; Overload;
Function FileSetAttributes(Const sFilename : String; iAttributes : TFileAttributes) : Boolean; Overload;
Function FileExists(Const sFilename : String) : Boolean; Overload;
Function FileDelete(Const sFilename : String) : Boolean; Overload;
Function FileHandleInvalid : TFileHandle; Overload;
Function FileHandleIsValid(Const aFileHandle : TFileHandle) : Boolean; Overload;
Function FileHandleOpen(Const aValue : Cardinal) : TFileHandle; Overload;
Function FileAttributesToCardinal(Const aAttributes : TFileAttributes) : Cardinal; Overload;
Function CardinalToFileAttributes(Const iValue : Cardinal) : TFileAttributes; Overload;
Procedure FileHandleClose(Var aFileHandle : TFileHandle); Overload;
Function FileGetReadOnlyAttribute(Const sFilename : String) : Boolean; Overload;
Procedure FileSetReadOnlyAttribute(Const sFilename : String; Const bReadOnly : Boolean); Overload;
Function PathFolder(Const sFilename : String) : String; Overload;
Function FileCopyAttempt(Const sSource, sDestination : String) : Boolean; Overload;
Function FileCopyForce(Const sSource, sDestination : String) : Boolean; Overload;
Function ForceFolder(dir: String): Boolean;

Function PathIncludeExtension(Const sFilename, sExtension : String) : String; Overload;
Function PathExcludeExtension(Const sFilename : String) : String; Overload;
Function PathFilename(Const sFilename : String) : String; Overload;
Function PathTitle(Const sFilename : String) : String; Overload;
Function PathExtension(Const sFilename : String) : String; Overload;
Function FolderExists(Const sFolder : String) : Boolean;

Function FileTimeToDateTime(Const Value : TFileTime) : TDateTime;
Function DateTimeToFileTime(Const Value : TDateTime) : TFileTime;
Function FileTimeZero : TFileTime; Overload;
Function WindowsFileTimeToFileTime(Const aFileTime : Windows.FileTime) : TFileTime;
Function FileTimeToWindowsFileTime(Const aFileTime : TFileTime) : Windows.FileTime;

Function FileGetModified(Const sFileName : String) : TFileTime; Overload;
Function FileSize(Const sFileName : String) : Int64; Overload;

// File versions
Function FileVersion(Const sFilename : String) : TFileVersion; Overload;
Function ApplicationFileVersion(Const sFilename : String) : TFileVersion; Overload;
Function DataFileVersion(Const sFilename : String) : TFileVersion; Overload;
Function FileVersionToString(Const aVersion : TFileVersion) : String; Overload;
Function FileVersionZero : TFileVersion; Overload;

Implementation


Type
  TLargeInteger = Record
    Low : Cardinal;
    High : Cardinal;
  End;

  PLargeInteger = ^TLargeInteger;


Const
  FILEATTRIBUTE_CARDINALS : Array[TFileAttribute] Of Cardinal =
    (FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_HIDDEN, FILE_ATTRIBUTE_NORMAL, FILE_ATTRIBUTE_OFFLINE, FILE_ATTRIBUTE_COMPRESSED,
     FILE_ATTRIBUTE_READONLY, FILE_ATTRIBUTE_SYSTEM, FILE_ATTRIBUTE_TEMPORARY, SysUtils.faDirectory, FILE_FLAG_WRITE_THROUGH);


Function FileAttributesToCardinal(Const aAttributes : TFileAttributes) : Cardinal;
Var
  aAttributeIndex : TFileAttribute;
Begin
  Result := 0;

  For aAttributeIndex := Low(aAttributeIndex) To High(aAttributeIndex) Do
  Begin
    If aAttributeIndex In aAttributes Then
      Result := Result Or FILEATTRIBUTE_CARDINALS[aAttributeIndex];
  End;
End;


Function CardinalToFileAttributes(Const iValue : Cardinal) : TFileAttributes;
Var
  aAttributeIndex : TFileAttribute;
Begin
  Result := [];

  For aAttributeIndex := Low(aAttributeIndex) To High(aAttributeIndex) Do
  Begin
    If (FILEATTRIBUTE_CARDINALS[aAttributeIndex] And iValue) > 0 Then
      Result := Result + [aAttributeIndex];
  End;
End;

Function FileGetAttributes(Const sFilename : String) : TFileAttributes;
Var
  iValue : Cardinal;
Begin
  iValue := GetFileAttributes(PChar(sFilename));

  If iValue = FileHandleInvalid.Value Then
    Result := []
  Else
    Result := CardinalToFileAttributes(iValue);
End;


Function FileSetAttributes(Const sFilename : String; iAttributes : TFileAttributes) : Boolean;
Begin
  Result := SetFileAttributes(PChar(sFilename), FileAttributesToCardinal(iAttributes));
End;

Function FileHandleInvalid : TFileHandle;
Begin
  Result.Value := $FFFFFFFF;
End;

Function FileHandleIsValid(Const aFileHandle : TFileHandle) : Boolean;
Begin
  Result := aFileHandle.Value <> FileHandleInvalid.Value;
End;

Function FileHandleOpen(Const aValue : Cardinal) : TFileHandle;
Begin
  Result.Value := aValue;
End;



Procedure FileHandleClose(Var aFileHandle : TFileHandle);
Begin
  Windows.CloseHandle(aFileHandle.Value);
  aFileHandle := FileHandleInvalid;
End;

Function PathFolder(Const sFilename : String) : String;
Var
  iIndex : Integer;
Begin
  iIndex := LastDelimiter('\:', sFilename);

  If (iIndex > 1) And (sFilename[iIndex] = '\:') And
     (Not CharInSet(sFilename[iIndex - 1], ['\', ':']) Or
     (ByteType(sFilename, iIndex - 1) = mbTrailByte)) Then
    Dec(iIndex);

  Result := StringIncludeAfter(Copy(sFilename, 1, iIndex), '\');
End;


Function FileExists(Const sFilename : String) : Boolean;
Var
  hHandle : NativeUInt;
  aFindData : TWin32FindData;
Begin
  hHandle := Windows.FindFirstFile(PChar(sFileName), aFindData);

  Result := (hHandle <> INVALID_HANDLE_VALUE) And (aFindData.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY = 0);

  Windows.FindClose(hHandle);
End;

Function FileDelete(Const sFilename : String) : Boolean;
Begin
  Result := Windows.DeleteFile(PChar(sFilename));
End;

Function FileGetReadOnlyAttribute(Const sFilename : String) : Boolean;
Begin
  Result := FileAttributeReadOnly In FileGetAttributes(sFilename);
End;


Procedure FileSetReadOnlyAttribute(Const sFilename : String; Const bReadOnly : Boolean);
Var
  aAttributes : TFileAttributes;
Begin
  If bReadOnly Then
    aAttributes := FileGetAttributes(sFilename) + [FileAttributeReadOnly]
  Else
    aAttributes := FileGetAttributes(sFilename) - [FileAttributeReadOnly];

  FileSetAttributes(sFilename, aAttributes);
End;

Function FileCopyAttempt(Const sSource, sDestination : String) : Boolean;
Begin
  Result := Windows.CopyFile(PChar(sSource), PChar(sDestination), True);
End;


Function FileCopyForce(Const sSource, sDestination : String) : Boolean;
Begin
  Result := Windows.CopyFile(PChar(sSource), PChar(sDestination), False);
End;



Function PathIncludeExtension(Const sFilename, sExtension : String) : String;
Begin
  Result := sFilename + sExtension;
End;

Function PathExcludeExtension(Const sFilename : String) : String;
Begin
  Result := StringExcludeAfter(sFilename, PathExtension(sFilename));
End;


Function PathExtension(Const sFilename : String) : String;
Begin
  // Return the extension including the '.'.  Eg. PathExtension('notepad.exe') = '.exe'

  Result := SysUtils.ExtractFileExt(sFilename);
End;

Function PathFilename(Const sFilename : String) : String;
Begin
  Result := Copy(sFileName, LastDelimiter('\/:', sFileName) + 1, MaxInt);
End;


Function PathRelative(Const sFolder, sFilename : String) : String;
Var
  iPos : Integer;
Begin
  Result := sFilename;

  iPos := Pos(StringUpper(sFolder), StringUpper(Result));
  If iPos > 0 Then
    Delete(Result, 1, Length(sFolder));
End;


Function PathTitle(Const sFilename : String) : String;
Begin
  // Return the filename without the path or the extension.

  Result := PathFilename(sFilename);

  Result := Copy(Result, 1, Length(Result) - Length(PathExtension(sFilename)));
End;


Function FileTimeToDateTime(Const Value : TFileTime) : TDateTime;
Var
  SystemTime : TSystemTime;
Begin
  If FileTimeToSystemTime(Windows.FileTime(Value), SystemTime) Then
    Result := SystemTimeToDateTime(SystemTime) + TimezoneBias
  Else
    Result := 0;
End;


Function DateTimeToFileTime(Const Value : TDateTime) : TFileTime;
Var
  SystemTime : TSystemTime;
  iValue : Windows.FileTime;
Begin
  DateTimeToSystemTime(Value - TimezoneBias, SystemTime);

  If SystemTimeToFileTime(SystemTime, iValue) Then
    Result := WindowsFileTimeToFileTime(iValue)
  Else
    Result := FileTimeZero;
End;



Function FileTimeZero : TFileTime;
Begin
  Result.Value := 0;
End;


Function FileTimeToWindowsFileTime(Const aFileTime : TFileTime) : Windows.FileTime;
Begin
  Result.dwLowDateTime := Cardinal(aFileTime.Value);
  Result.dwHighDateTime := Cardinal(aFileTime.Value Shr 32);
End;


Function WindowsFileTimeToFileTime(Const aFileTime : Windows.FileTime) : TFileTime;
Begin
  Result.Value := Int64(aFileTime);
End;


Function FileGetModified(Const sFileName : String) : TFileTime;
Var
  aHandle : TFileHandle;
Begin
  aHandle := FileHandleOpen(CreateFile(PChar(sFileName), GENERIC_READ, FILE_SHARE_READ, Nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL Or FILE_FLAG_BACKUP_SEMANTICS, 0));
  Try
    If FileHandleIsValid(aHandle) Then
      GetFileTime(aHandle.Value, Nil, Nil, @Result)
    Else
      Result := FileTimeZero;
  Finally
    FileHandleClose(aHandle);
  End;
End;


Function FileVersionToString(Const aVersion : TFileVersion) : String;
Begin
  Result := StringFormat('%d.%d.%d.%d', [aVersion.Major, aVersion.Minor, aVersion.Release, aVersion.Build]);
End;




Function FileVersion(Const sFilename: String): TFileVersion;
var
  s : string;
Begin
  s := PathExtension(sFilename);
  If (s = '.exe') or (s = '.dll') Then
    Result := ApplicationFileVersion(sFilename)
  Else
    Result := DataFileVersion(sFilename);
End;


Function FileVersionZero : TFileVersion;
Begin
  MemoryZero(@Result, SizeOf(Result));
End;


Function DataFileVersion(Const sFilename: String): TFileVersion;
Var
  aFileTime : TFileTime;
  iFileSize : Int64;
Begin
  // Compile a version number from the modified time and size of the file.

  aFileTime := FileGetModified(sFilename);

  Result.Major := Integer(Windows.FileTime(aFileTime).dwHighDateTime);
  Result.Minor := Integer(Windows.FileTime(aFileTime).dwLowDateTime);

  iFileSize := FileSize(sFilename);

  Result.Release := Integer(iFileSize);
  Result.Build := Integer(iFileSize Shr 32);
End;


Function ApplicationFileVersion(Const sFilename : String) : TFileVersion;
Var
  pBuffer : Pointer;
  iSize   : Integer;
  iTemp   : Cardinal;
  pInfo   : Pointer;
Begin
  FillChar(Result, SizeOf(Result), 0);

  iSize := GetFileVersionInfoSize(PChar(sFilename), iTemp);

  If iSize > 0 Then
  Begin
    MemoryCreate(pBuffer, iSize);
    Try
      If GetFileVersionInfo(PChar(sFilename), 0, iSize, pBuffer) And VerQueryValue(pBuffer, '\', pInfo, iTemp) Then
      Begin
        Result.Major := PVSFixedFileInfo(pInfo)^.dwFileVersionMS Shr 16;
        Result.Minor := PVSFixedFileInfo(pInfo)^.dwFileVersionMS And $FFFF;
        Result.Release := PVSFixedFileInfo(pInfo)^.dwFileVersionLS Shr 16;
        Result.Build := PVSFixedFileInfo(pInfo)^.dwFileVersionLS And $FFFF;
      End;
    Finally
      MemoryDestroy(pBuffer, iSize);
    End;
  End;
End;


Function FileSize(Const sFileName : String) : Int64;
Var
  pResult : PLargeInteger;
  aHandle : TFileHandle;
Begin
  aHandle := FileHandleOpen(CreateFile(PChar(sFileName), GENERIC_READ, FILE_SHARE_READ Or FILE_SHARE_WRITE, Nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0));
  Try
    If FileHandleIsValid(aHandle) Then
    Begin
      pResult := PLargeInteger(@Result);

      pResult^.Low := GetFileSize(aHandle.Value, @pResult^.High);

      If pResult^.Low = FileHandleInvalid.Value Then
        Result := 0;
    End
    Else
    Begin
      Result := 0;
    End;
  Finally
    FileHandleClose(aHandle);
  End;
End;


Function FolderExists(Const sFolder : String) : Boolean;
Var
  iCode : Cardinal;
Begin
  iCode := GetFileAttributes(PChar(sFolder));

  Result := (iCode <> High(Cardinal)) And (FILE_ATTRIBUTE_DIRECTORY And iCode <> 0);
End;


Function last(Const s: String; c: Char): Cardinal;
Var
  i: Word;
Begin
  i := Length(s);
  Result := 0;
  While (i > 0) Do
    Begin
    If s[i] = c Then
      Begin
      Result := i;
      Exit;
      End;
    Dec(i);
    End;
End;

Function ForceFolder(dir: String): Boolean;
Var
  x: Integer;
Begin
  x := last(dir, '\');
  If x > 1 Then
    ForceFolder(Copy(dir, 1, x - 1));
  Result := windows.Createdirectory(PChar(dir), Nil);
  If Not Result Then
    If (GetLastError = 183) Then
      Result := True; // directory existed. ( but thats O.K.)
End;


End. // FileSupport //
