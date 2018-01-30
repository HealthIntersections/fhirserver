Unit Logging;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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
  {$IFDEF MACOS} OSXUtils, {$ELSE} Windows, {$ENDIF}
  SysUtils, kCritSct,
  FileSupport,
  StringSupport,
  AdvObjects,
  AdvObjectLists,
  AdvStringBuilders,
  AdvStringLists;

Type
  TLogFullPolicy = (lfpStop, lfpWipe, lfpChop, lfpCycle);

  TLoggerPolicy = Class (TAdvObject)
  private
    FClear: Boolean;
    FAllowExceptions: Boolean;
    FMaximumSize: Cardinal;
    FChopAmount: Cardinal;
    FDescription: String;
    FHeader: String;
    FFullPolicy: TLogFullPolicy;
  Public
    Constructor Create; Override; //

    Function ClonePolicy : TLoggerPolicy;

    // debugging description for file
    Property Description : String Read FDescription Write FDescription;

    // whether to clear the file when initializing
    Property Clear : Boolean Read FClear Write FClear;

    // whethe to allow exceptions to carry out of the logger
    Property AllowExceptions : Boolean Read FAllowExceptions Write FAllowExceptions;

    // Header to add to the file when first created
    Property Header : String Read FHeader Write FHeader;

    // what to do when the log file is full. chop some out (from the front)
    // and keep logging (default, 0, = no more logging). This can slow the process down
    // when chopping the file from the front - and can potentially leave damaged
    // logs if the process crashes while it is lopping from the front
    Property FullPolicy : TLogFullPolicy Read FFullPolicy Write FFullPolicy;

    // Size limit (in bytes) of log file. Default, 0, means no restriction
    Property MaximumSize : Cardinal Read FMaximumSize Write FMaximumSize;

    // amount to chop off when chopping file. 0 means 1k
    Property ChopAmount : Cardinal Read FChopAmount Write FChopAmount;

  End;

  TLogger = Class (TAdvObject)
  Private
    FLock : TCriticalSection;
    FFilename : String;
    FFilenameCanChange : Boolean;
    FPolicy : TLoggerPolicy;

    Function ProcessFileName : String;

    Procedure CutFile(sName : String);
    Procedure CycleFile(sName : String);

  Public
    Constructor Create(filename : String);
    Destructor Destroy; Override;

    Function Description : String;
    Procedure WriteToLog(s: String); overload;
    Procedure WriteToLog(bytes: TBytes); overload;

    Property Policy : TLoggerPolicy read FPolicy;
  End;

Implementation



{ TLogger }

Constructor TLogger.Create(filename : String);
Begin
  Inherited Create;
  FLock := TCriticalSection.Create('Log '+filename);
  FPolicy := TLoggerPolicy.Create;
  FFilename := filename;
End;

Destructor TLogger.Destroy;
Begin
  FPolicy.Free;
  FLock.Free;
  Inherited;
End;

Procedure TLogger.WriteToLog(s: String);
Begin
  If s = '' Then
    Exit;
  WriteToLog(TEncoding.UTF8.GetBytes(s));
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

Function CreateDir(dir: String): Boolean;
Var
  x: Integer;
Begin
  x := last(dir, '\');
  If x > 1 Then
    CreateDir(Copy(dir, 1, x - 1));
  Result := ForceFolder(dir);
  If Not Result Then
    If (GetLastError = 183) Then
      Result := True; // directory existed. ( but thats O.K.)
End;


Procedure TLogger.CutFile(sName : String);
{$IFDEF MACOS}
begin
  raise Exception.Create('Not done yet');
end;
{$ELSE}
Var
  src, dst: THandle;
  buf: Array [0..65536] Of Byte;
  readbytes, writebytes: Cardinal;
Begin
  DeleteFile(PChar(sName + '.tmp'));
  RenameFile(PChar(sName), PChar(sName + '.tmp'));
  src := CreateFile(PChar(sName + '.tmp'), GENERIC_READ, FILE_SHARE_READ, Nil, OPEN_EXISTING, 0, 0);
  Try
    dst := CreateFile(PChar(sName), GENERIC_WRITE, 0, Nil, CREATE_ALWAYS, 0, 0);
    Try
      SetFilePointer(src, FPolicy.ChopAmount, Nil, FILE_BEGIN);
      Repeat
        ReadFile(src, buf, 65536, readbytes, Nil);
        WriteFile(dst, buf, readbytes, writebytes, Nil);
        If writebytes <> readbytes Then
          Begin
          If FPolicy.AllowExceptions Then
            Try
              Raise Exception.Create('error chopping file');
            Except
              End;
          readbytes := 0;
          End;
      Until ReadBytes = 0;
    Finally
      CloseHandle(dst);
    End;
  Finally
    CloseHandle(src);
  End;
  DeleteFile(PChar(sName + '.tmp'));
End;
{$ENDIF}

Procedure TLogger.CycleFile(sName : String);
Var
  LNewName : String;
  i : Integer;
Begin
  i := 0;
  Repeat
    LNewName := PathFolder(sName)+PathTitle(sName)+StringPadLeft(IntegerToString(i), '0', 4)+PathExtension(sName);
    Inc(i);
  Until Not FileExists(LNewName);
  RenameFile(sName, LNewName);
End;

Function DescribeSize(b, min: Cardinal): String;
Begin
  If b = $FFFFFFFF Then
    Result := '??'
  Else If (min <> 0) And (b = min) Then
    Result := '0'
  Else If b = 0 Then
    Result := '0'
  Else If b < 1024 Then
    Result := IntToStr(b) + 'b'
  Else If b < 1024 * 1024 Then
    Result := IntToStr(b Div 1024) + 'kb'
  Else If b < 1204 * 1024 * 1024 Then
    Result := IntToStr(b Div (1024 * 1024)) + 'Mb'
  Else
    Result := IntToStr(b Div (1024 * 1024 * 1024)) + 'Gb';
End;


function TLogger.Description: String;
begin
  Result := FFilename;
end;

{ TLoggingPolicy }

function TLoggerPolicy.ClonePolicy: TLoggerPolicy;
begin
  Result := TLoggerPolicy.Create;
  Result.FClear := FClear;
  Result.FAllowExceptions := FAllowExceptions;
  Result.FMaximumSize := FMaximumSize;
  Result.FChopAmount := FChopAmount;
  Result.FDescription := FDescription;
  Result.FHeader := FHeader;
  Result.FFullPolicy := FFullPolicy;
end;

constructor TLoggerPolicy.Create;
begin
  inherited;
  FFullPolicy := lfpChop;
end;


function TLogger.ProcessFileName: String;
var
  i, j : integer;
  s : String;
  aNow : TDateTime;
begin
  aNow := Now;
  Result := FFilename;
  i := pos('$', result);
  While i > 0 Do
  Begin
    s := copy(result, i + 1, $FF);
    j := pos(')', s);
    if j <= 3 Then
      Raise Exception.Create('Syntax Error in File name '+FFilename+' (unterminated command)');
    s := copy(s, 1, j-1);
    if (s[1] = 'd') or (s[1] = 'D') or (s[1] = 't') or (s[1] = 'T') Then
      result := copy(result, 1, i-1) + FormatDateTime(copy(s, 3, $FF), aNow)+ copy(result, i+j+1, $FF)
    Else
      Raise Exception.Create('Syntax Error in File name '+FFilename+' (unknown command '+s[1]+')');
    i := pos('$', result);
  End;
end;


procedure TLogger.WriteToLog(bytes: TBytes);
{$IFDEF MACOS}
begin
  If length(b) = 0 Then
    Exit;
  raise Exception.Create('Not done yet');
end;
{$ELSE}
Var
  sName : string;
  res: Boolean;
  done: Cardinal;
  d: Integer;
  f: HFile;
  FileSize: Cardinal;
Begin
  FLock.Lock;
  Try
    sName := ProcessFileName;

    f := CreateFile(PChar(sName), GENERIC_WRITE, FILE_SHARE_READ, Nil, OPEN_ALWAYS, 0, 0);
    If f = INVALID_HANDLE_VALUE Then
      If FPolicy.AllowExceptions Then
        Raise Exception.Create('Unable to begin logging for file "' + sName + '": ' + SysErrorMessage(GetLastError))
      Else
        Exit;
    If FPolicy.MaximumSize > 0 Then
      Begin
      FileSize := windows.GetFileSize(F, Nil);
      If (FileSize > FPolicy.MaximumSize) Then
        Begin
        CloseHandle(f);
        Case FPolicy.FullPolicy Of
          lfpWipe : DeleteFile(sName);
          lfpChop : CutFile(sName);
          lfpCycle : CycleFile(sName);
        Else
          Exit; // lfpStop
        End;
        f := CreateFile(PChar(sName), GENERIC_WRITE, FILE_SHARE_READ, Nil, OPEN_ALWAYS, 0, 0);
        If f = INVALID_HANDLE_VALUE Then
          If FPolicy.AllowExceptions Then
            Raise Exception.Create('Unable to begin logging for file "' + sName + '": ' + SysErrorMessage(GetLastError))
          Else
            Exit;
        End;
      End;

    SetFilePointer(f, 0, Nil, FILE_END);
    res := WriteFile(f, bytes[0], Length(bytes), done, Nil);
    CloseHandle(f);
    d := Done; // suppress warning
    If FPolicy.AllowExceptions And Not res Or (d <> Length(bytes)) Then
      Raise Exception.Create('Unable to write to file "' + sName + '": ' + SysErrorMessage(GetLastError));
  Finally
    FLock.UnLock;
  End;
End;
{$ENDIF}

End.

