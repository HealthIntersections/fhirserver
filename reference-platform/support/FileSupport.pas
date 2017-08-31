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
  {$IFDEF MACOS} OSXUtils, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  StringSupport, MathSupport, MemorySupport, DateSupport;

Type
  TFileHandle = Record
    Value : Cardinal;
  End;

  TFileVersion = Record
    Major : Integer;
    Minor : Integer;
    Release : Integer;
    Build : Integer;
  End;

Function FileGetModified(Const sFileName : String) : TDateTime; Overload;

Function FileExists(Const sFilename : String) : Boolean; Overload;
Function FileDelete(Const sFilename : String) : Boolean; Overload;
Function FileHandleInvalid : TFileHandle; Overload;
Function FileHandleIsValid(Const aFileHandle : TFileHandle) : Boolean; Overload;
Function FileHandleOpen(Const aValue : Cardinal) : TFileHandle; Overload;
Procedure FileHandleClose(Var aFileHandle : TFileHandle); Overload;
Function PathFolder(Const sFilename : String) : String; Overload;
Function ForceFolder(dir: String): Boolean;

Function PathFilename(Const sFilename : String) : String; Overload;
Function PathTitle(Const sFilename : String) : String; Overload;
Function PathExtension(Const sFilename : String) : String; Overload;
Function FolderExists(Const sFolder : String) : Boolean;

Function FileSize(Const sFileName : String) : Int64; Overload;
function Path(parts : array of String) : String;
function URLPath(parts : array of String) : String;

Implementation

Function FileGetModified(Const sFileName : String) : TDateTime;
var
  info : TDateTimeInfoRec;
begin
  if FileGetDateTimeInfo(sFilename, info) then
    result := info.TimeStamp
  else
    result := 0;
end;



Type
  TLargeInteger = Record
    Low : Cardinal;
    High : Cardinal;
  End;

  PLargeInteger = ^TLargeInteger;


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
Begin
  result := SysUtils.FileExists(sFilename);
End;

Function FileDelete(Const sFilename : String) : Boolean;
Begin
  {$IFDEF MACOS}
  if FileIsReadOnly(sFileName) then
    result := false
  else
  {$ENDIF}
  Result := SysUtils.DeleteFile(sFilename);
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

Function PathTitle(Const sFilename : String) : String;
Begin
  // Return the filename without the path or the extension.

  Result := PathFilename(sFilename);

  Result := Copy(Result, 1, Length(Result) - Length(PathExtension(sFilename)));
End;


Function FileSize(Const sFileName : String) : Int64;
{$IFDEF MACOS}
var
  f : TFileStream;
begin
  f := TFileStream.create(sFileName, fmOpenRead);
  try
    result := f.size;
  finally
    f.free;
  end;
end;
{$ELSE}
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
{$ENDIF}

Function FolderExists(Const sFolder : String) : Boolean;
Begin
  result := SysUtils.DirectoryExists(sFolder, false);
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
begin
  result := SysUtils.ForceDirectories(dir);
end;

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
{$IFDEF MACOS}
begin
end;
{$ELSE}
Begin
  Windows.CloseHandle(aFileHandle.Value);
  aFileHandle := FileHandleInvalid;
End;
{$ENDIF}

function Path(parts : array of String) : String;
var
  part : String;
begin
  result := '';
  for part in parts do
    if result = '' then
      result := part
    else
      result := IncludeTrailingPathDelimiter(result)+ part;
end;

function URLPath(parts : array of String) : String;
var
  part : String;
begin
  result := '';
  for part in parts do
    if result = '' then
      result := part
    else if result.EndsWith('/') then
      result := result+ part
    else
      result := result+'/'+part;
end;


End. // FileSupport //
