Unit Logging;

Interface

Uses
  Windows,
  FileSupport,
  StringSupport,
  AdvObjects,
  AdvObjectLists,
  AdvExclusiveCriticalSections,
  AdvStringBuilders,
  AdvStringLists;

Type
  TLogFullPolicy = (lfpStop, lfpWipe, lfpChop, lfpCycle);

  TLoggerPolicy = Class (TAdvObject)
  private
    FClear: Boolean;
    FAllowExceptions: Boolean;
    FBufferLimit: Integer;
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

    // number of entries stored in the buffer before committing to file (faster)
    Property BufferLimit : Integer Read FBufferLimit Write FBufferLimit;
  End;

  TLogger = Class (TAdvObject)
  Private
    FLock : TAdvExclusiveCriticalSection;
    FBuffer : TAdvStringList;
    FFilename : String;
    FFilenameCanChange : Boolean;
    FPolicy : TLoggerPolicy;

    Function Header : String;

    Function ProcessFileName : String;

    Procedure ActualLog(Const s: String);

    Procedure FlushLogQueue;
    Procedure CutFile(sName : String);
    Procedure CycleFile(sName : String);

  Public
    Constructor Create(filename : String);
    Destructor Destroy; Override;

    Function Description : String;
    Procedure WriteToLog(s: String);

    Property Policy : TLoggerPolicy read FPolicy;
  End;

Implementation

uses
  SysUtils;


{ TLogger }

Constructor TLogger.Create(filename : String);
Begin
  Inherited Create;
  FLock := TAdvExclusiveCriticalSection.Create;
  FPolicy := TLoggerPolicy.Create;
  FFilename := filename;
End;

Destructor TLogger.Destroy;
Begin
  FlushLogQueue;
  FPolicy.Free;
  FBuffer.Free;
  FLock.Free;
  Inherited;
End;

Function TLogger.Header : String;
Begin
  If FPolicy.Header <> '' Then
    Result := FPolicy.Header + #13#10
  Else If FPolicy.Description <> '' Then
    Result := FormatDateTime('hh:nn:ss.zzzz dd-mmm yyyy', Now) + ' Log: ' + FPolicy.Description + #13#10 +
      '=================================================================' + #13#10
  Else
    Result := '';
End;

Procedure TLogger.FlushLogQueue;
var
  iLoop : Integer;
  oBuilder : TAdvStringBuilder;
Begin
  // must be locked
  if (FBuffer <> Nil) Then
  Begin
    oBuilder := TAdvStringBuilder.Create;
    Try
      For iLoop := 0 to FBuffer.Count - 1 Do
        oBuilder.Append(FBuffer[iLoop]);
      ActualLog(oBuilder.AsString);
    Finally
      oBuilder.Free;
    End;
    FBuffer.Clear;
  End;
End;

Procedure TLogger.WriteToLog(s: String);
Begin
  If s = '' Then
    Exit;

  FLock.Lock;
  Try
    If (FPolicy.BufferLimit = 0) Or FFilenameCanChange Then
      ActualLog(s)
    Else
    Begin
      if FBuffer = nil Then
        FBuffer := TAdvStringList.Create;
      FBuffer.Add(s);
      If FBuffer.Count > FPolicy.BufferLimit Then
        FlushLogQueue;
    End;
  Finally
    FLock.UnLock;
  End;
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
  Result := windows.Createdirectory(PChar(dir), Nil);
  If Not Result Then
    If (GetLastError = 183) Then
      Result := True; // directory existed. ( but thats O.K.)
End;


Procedure TLogger.ActualLog(Const s: String);
Var
  sName : string;
  res: Boolean;
  done: Cardinal;
  d: Integer;
  {$IFNDEF LINUX}
  f: HFile;
  {$ELSE}
  f: Integer;
  {$ENDIF}
  FileSize: Cardinal;
  bytes : TBytes;
Begin
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
  bytes := TEncoding.UTF8.GetBytes(s);
  res := WriteFile(f, bytes[0], Length(bytes), done, Nil);
  CloseHandle(f);
  d := Done; // suppress warning
  If FPolicy.AllowExceptions And Not res Or (d <> Length(s)) Then
    Raise Exception.Create('Unable to write to file "' + sName + '": ' + SysErrorMessage(GetLastError));
End;

Procedure TLogger.CutFile(sName : String);
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
  Result.FBufferLimit := FBufferLimit;
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


End.

