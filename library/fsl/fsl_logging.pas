unit fsl_logging;

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

{$I fhir.inc}

interface

uses
  {$IFDEF WINDOWS} Windows, {$IFDEF DELPHI} FastMM4, {$ENDIF} {$IFDEF FPC}JwaPsApi, {$ELSE} PsApi, {$ENDIF}{$ENDIF}
  SysUtils, Classes,
  fsl_threads, fsl_base, fsl_utilities, fsl_collections;

Type
  TLogEvent = procedure (msg : String) of object;


Type
  TLogFullPolicy = (lfpStop, lfpWipe, lfpChop, lfpCycle);

  TLoggerPolicy = Class (TFslObject)
  private
    FClear: Boolean;
    FAllowExceptions: Boolean;
    FMaximumSize: Cardinal;
    FChopAmount: Cardinal;
    FDescription: String;
    FHeader: String;
    FFullPolicy: TLogFullPolicy;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override; //

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

  TLogger = Class (TFslObject)
  Private
    FLock : TFslLock;
    FFilename : String;
    FPolicy : TLoggerPolicy;
    FStream : TFileStream;
    FOpenName : String;

    Function ProcessFileName : String;

    Procedure CutFile(sName : String);
    Procedure CycleFile(sName : String);

  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(filename : String);
    destructor Destroy; Override;

    Function Description : String;
    Procedure WriteToLog(s: String); overload;
    Procedure WriteToLog(bytes: TBytes); overload;
    procedure clear;

    Property Policy : TLoggerPolicy read FPolicy;
    Property Filename : String read FFilename;
  End;

  TLogListener = class (TFslObject)
  protected
    procedure newDay(const s : String); virtual;
    procedure log(const s : String); virtual;
    procedure closing; virtual;

    function transient : boolean; virtual;
    procedure logStart(s : String); virtual;
    procedure logContinue(s : String); virtual;
    procedure logFinish(s : String); virtual;
  public
    function link : TLogListener; overload;
  end;

  { TLogging }

  TLogging = class (TFslObject)
  private
    FLogToConsole : boolean;
    FFileLogger : TLogger;
    FListeners : TFslList<TLogListener>;
    FStarting: boolean;
    FStartTime : TDateTime;
    FLastday : integer;
    FWorkingLine : String;
    FCount : integer;

    procedure checkDay;
    procedure close;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Starting : boolean read FStarting write FStarting;
    property LogToConsole : boolean read FLogToConsole write FLogToConsole;

    property FileLog : TLogger read FFileLogger;

    procedure logToFile(filename : String);
    procedure addListener(listener : TLogListener);
    procedure removeListener(listener : TLogListener);

    procedure log(s : String);
    procedure start(s : String);
    procedure continue(s : String);
    procedure finish(s : String = '');

    function Counter : String;

    Function DescribeSize(b, min: Cardinal): String;
    function MemoryStatus : String;
  end;

var
  Logging : TLogging;


implementation

{ TLoggingPolicy }

constructor TLoggerPolicy.Create;
begin
  inherited;
  FFullPolicy := lfpChop;
end;

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

function TLoggerPolicy.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, (FHeader.length * sizeof(char)) + 12);
end;

{ TLogger }

Constructor TLogger.Create(filename : String);
Begin
  Inherited Create;
  FLock := TFslLock.Create('Log '+filename);
  FPolicy := TLoggerPolicy.Create;
  FFilename := filename;
  FStream := nil;
  FOpenName := '';
End;

Destructor TLogger.Destroy;
Begin
  FStream.Free;
  FPolicy.Free;
  FLock.Free;
  Inherited;
End;

procedure TLogger.clear;
begin
  DeleteFile(FFilename);
end;

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
    {$IFDEF WINDOWS}
    If (GetLastError = 183) Then
    {$ELSE}
    if (DirectoryExists(dir)) then
    {$ENDIF}
      Result := True; // directory existed. ( but thats O.K.)
End;


Procedure TLogger.CutFile(sName : String);
var
  src, dst : TFileStream;
  cs : integer;
Begin
  if FPolicy.ChopAmount <> 0 then
    cs := FPolicy.ChopAmount
  else
    cs := FPolicy.FMaximumSize div 5;

  DeleteFile(PChar(sName + '.tmp'));
  RenameFile(PChar(sName), PChar(sName + '.tmp'));

  src := TFileStream.Create(sName + '.tmp', fmopenRead + fmShareDenyWrite);
  try
    dst := TFileStream.Create(sName, fmCreate);
    try
      src.Position := cs;
      dst.CopyFrom(src, src.Size - cs);
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
  DeleteFile(PChar(sName + '.tmp'));
end;

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

function TLogger.Description: String;
begin
  Result := FFilename;
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
      raise EIOException.create('Syntax Error in File name '+FFilename+' (unterminated command)');
    s := copy(s, 1, j-1);
    if (s[1] = 'd') or (s[1] = 'D') or (s[1] = 't') or (s[1] = 'T') Then
      result := copy(result, 1, i-1) + FormatDateTime(copy(s, 3, $FF), aNow)+ copy(result, i+j+1, $FF)
    Else
      raise EIOException.create('Syntax Error in File name '+FFilename+' (unknown command '+s[1]+')');
    i := pos('$', result);
  End;
end;

procedure TLogger.WriteToLog(bytes: TBytes);
var
  size: Cardinal;
  sName : string;
  exists : boolean;
begin
  if self = nil then
    exit;
  If length(bytes) = 0 Then
    Exit;
  FLock.Lock;
  Try
    sName := ProcessFileName;
    size := 0;
    if (FOpenName = '') then
    begin
      exists := FileExists(sName);
    end
    else if FOpenName <> sName then
    begin
      FStream.Free;
      FStream := nil;
      FOpenName := '';
      exists := FileExists(sName);
    end
    else
    begin
      size := FStream.Size;
      exists := true;
    end;

    If exists and (FPolicy.MaximumSize > 0) Then
    Begin
      if size = 0 then
        size := FileSize(sName);

      If (size > FPolicy.MaximumSize) Then
      Begin
        if FStream <> nil then
        begin
          FStream.Free;
          FStream := nil;
        end;

        Case FPolicy.FullPolicy Of
          lfpWipe : DeleteFile(sName);
          lfpChop : CutFile(sName);
          lfpCycle : CycleFile(sName);
        Else
          Exit; // lfpStop
        End;
        if FPolicy.FullPolicy <> lfpChop then
          exists := false;
      end;
    End;
    if (FStream = nil) then
    begin
      if exists then
      begin
        FStream := TFileStream.create(sname, fmOpenWrite + fmShareDenyWrite);
        FStream.seek(FStream.size, soBeginning);
      end
      else
        FStream := TFileStream.create(sname, fmCreate);
      FOpenName := sName;
    end;
    FStream.Write(bytes[0], length(bytes));
  Finally
    FLock.UnLock;
  End;
end;

function TLogger.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFilename.length * sizeof(char)) + 12);
  inc(result, FPolicy.sizeInBytes);
  inc(result, (FOpenName.length * sizeof(char)) + 12);
end;

{ TLogListener }

function TLogListener.link: TLogListener;
begin
  result := TLogListener(inherited link);
end;

procedure TLogListener.closing;
begin
end;

procedure TLogListener.log(const s: String);
begin
end;

procedure TLogListener.newDay(const s: String);
begin
end;

function TLogListener.transient: boolean;
begin
  result := false;
end;

procedure TLogListener.logStart(s: String);
begin
end;

procedure TLogListener.logContinue(s: String);
begin
end;

procedure TLogListener.logFinish(s: String);
begin
end;

{ TLogging }

constructor TLogging.Create;
begin
  inherited;
  FListeners := TFslList<TLogListener>.create;
  FStarting := true;
  FStartTime := now;
  FLastDay := 0;
end;

destructor TLogging.Destroy;
begin
  close;
  FFileLogger.Free;
  FListeners.Free;
  inherited;
end;

procedure TLogging.addListener(listener: TLogListener);
begin
  FListeners.Add(listener.Link)
end;

procedure TLogging.removeListener(listener: TLogListener);
begin
  FListeners.Remove(listener);
end;

procedure TLogging.logToFile(filename : String);
begin
  FFileLogger := TLogger.Create(filename);
end;

function TLogging.DescribeSize(b, min: Cardinal): String;
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


function memToMb(v : UInt64) : string;
begin
  v := v div 1024;
  v := v div 1024;
  result := inttostr(v)+'MB';
end;

function OSMem : UInt64;
{$IFDEF WINDOWS}
var
  hProcess: THandle;
  pmc: PROCESS_MEMORY_COUNTERS;
begin
  result := 0;
  hProcess := GetCurrentProcess;
  try
    if (GetProcessMemoryInfo(hProcess, {$IFNDEF FPC}@{$ENDIF}pmc, SizeOf(pmc))) then
      exit(pmc.WorkingSetSize + pmc.QuotaPagedPoolUsage + pmc.QuotaNonPagedPoolUsage);
  finally
    CloseHandle(hProcess);
  end;
{$ELSE}
begin
  result := 0;
{$ENDIF}
end;

function intMem : Uint64;
{$IFDEF DELPHI}
var
  st : TMemoryManagerUsageSummary;
{$ENDIF}
begin
{$IFDEF DELPHI}
  GetMemoryManagerUsageSummary(st);
  result := st.AllocatedBytes + st.OverheadBytes;
{$ELSE}
  result := 0;
{$ENDIF}
end;

function TLogging.MemoryStatus : String;
// memory status has 2 parts: internal and OS
var
  os : UInt64;
begin
  os := OSMem;
  if os <> 0 then
    result := memToMB(intMem) + ' / '+memToMB(os)
  else
    result := memToMB(intMem);
end;

procedure TLogging.checkDay;
var
  today : integer;
  s : String;
  listener : TLogListener;
begin
  today := trunc(now);
  if today <> FLastday then
  begin
    s := FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------';
    if FFileLogger <> nil then
      FFileLogger.WriteToLog(s+#13#10);
    if FLogToConsole then
    begin
      try
        System.Writeln(s);
      except
        FLogToConsole := false;
      end;
    end;
    for listener in FListeners do
    begin
      try
        listener.newDay(s);
      except
      end;
    end;
    FLastDay := today;
  end;
end;

procedure TLogging.close;
var
  listener : TLogListener;
begin
  for listener in FListeners do
  begin
    try
      listener.closing;
    except
    end;
  end;

end;

procedure TLogging.log(s: String);
var
  listener : TLogListener;
begin
  checkDay;
  if FStarting then
    s := FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - FStartTime)+' '+s
  else
    s := FormatDateTime('hh:nn:ss', now)+ ' '+s;
  if FFileLogger <> nil then
    FFileLogger.WriteToLog(s+#13#10);
  if FLogToConsole then
  begin
    try
      System.Writeln(s);
    except
      FLogToConsole := false;
    end;
  end;
  for listener in FListeners do
  begin
    try
      listener.log(s);
    except
    end;
  end;
end;

procedure TLogging.start(s : String);
var
  listener : TLogListener;
begin
  checkDay;
  FWorkingLine := s;

  if FStarting then
    s := FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - FStartTime)+' '+s
  else
    s := FormatDateTime('hh:nn:ss', now)+ ' '+s;
  if FLogToConsole then
  begin
    try
      System.Write(s);
    except
      FLogToConsole := false;
    end;
  end;
  for listener in FListeners do
  begin
    try
      if listener.transient then
        listener.logStart(s);
    except
    end;
  end;
end;

procedure TLogging.continue(s : String);
var
  listener : TLogListener;
begin
  FWorkingLine := FWorkingLine+s;
  if FLogToConsole then
    System.Write(s);
  for listener in FListeners do
  begin
    try
      if listener.transient then
        listener.logContinue(s);
    except
    end;
  end;
end;

procedure TLogging.finish(s : String = '');
var
  listener : TLogListener;
  msg : String;
begin
  if FLogToConsole then
    System.Writeln(s);

  msg := FWorkingLine + s;
  FWorkingLine := '';
  if FStarting then
    msg := FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - FStartTime)+' '+s
  else
    msg := FormatDateTime('hh:nn:ss', now)+ ' '+s;

  if FFileLogger <> nil then
    FFileLogger.WriteToLog(s+#13#10);
  for listener in FListeners do
  begin
    try
      if listener.transient then
        listener.logFinish(s)
      else
        listener.log(msg);
    except
    end;
  end;
end;

function TLogging.Counter: String;
begin
  inc(FCount);
  result := ' #'+inttostr(FCount);
end;

function TLogging.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFileLogger.sizeInBytes);
  inc(result, FListeners.sizeInBytes);
  inc(result, (FWorkingLine.length * sizeof(char)) + 12);
end;

Initialization
  Logging := TLogging.create;
Finalization
  Logging.Free;
end.

