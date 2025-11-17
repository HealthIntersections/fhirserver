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
  {$IFDEF WINDOWS} Windows, {$IFDEF FPC}JwaPsApi, {$ELSE} PsApi, {$ENDIF}{$ENDIF}
  SysUtils, Classes,
  fsl_threads, fsl_base, fsl_utilities, fsl_collections, fsl_cpu{$IFDEF FPC}, fsl_fpc_memory{$ENDIF};

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
    FCloseLog: boolean;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
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

    // usually, the log will keep the file open, for performamce reasons,
    // but holding the log open can be problematic for other uses, so you
    // get the log to close where the log isn't used intensively
    property closeLog : boolean read FCloseLog write FCloseLog;

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
    function sizeInBytesV(magic : integer) : cardinal; override;
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
    FShuttingDown: boolean;
    FStarting: boolean;
    FStartTime : TDateTime;
    FLastday : integer;
    FWorkingLine : String;
    FCount : integer;
    FHeld : TStringlist;
    FLock : TFslLock;
    FCPU : TCPUUsageData;

    procedure checkDay;
    procedure close;
    procedure LogDoubleFreeCallBack(name1, name2: String);
    procedure SetShuttingDown(AValue: boolean);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Starting : boolean read FStarting write FStarting;
    property LogToConsole : boolean read FLogToConsole write FLogToConsole;

    property FileLog : TLogger read FFileLogger;
    property CPU : TCPUUsageData read FCPU;

    procedure logToFile(filename : String);
    procedure addListener(listener : TLogListener);
    procedure removeListener(listener : TLogListener);

    procedure log(s : String);
    procedure start(s : String);
    procedure continue(s : String);
    procedure finish(s : String = '');
    procedure finishIfNeeded;

    function Counter : String;

    Function DescribeSize(b, min: UInt64): String;
    function MemoryStatus(full : boolean) : String;

    function InternalMem : UInt64;

    property shuttingDown : boolean read FShuttingDown write SetShuttingDown;
  end;

var
  Logging : TLogging;

type

    { TFslTimeTracker }

    TFslTimeTracker = class (TFslObject)
    private
      FStart : int64;
      FLast : Int64;
      FLog : TFslStringBuilder;
    public
      constructor Create; override;
      destructor Destroy; override;
      function link : TFslTimeTracker; overload;

      procedure step(name : String);
      function total : integer;
      function log : String;
    end;

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

function TLoggerPolicy.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
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
  FStream.free;
  FPolicy.free;
  FLock.free;
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
      dst.free;
    end;
  finally
    src.free;
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
  FLock.Lock('WriteToLog');
  Try
    sName := ProcessFileName;
    size := 0;
    if (FOpenName = '') then
    begin
      exists := FileExists(sName);
    end
    else if FOpenName <> sName then
    begin
      FStream.free;
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
          FStream.free;
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
    if FPolicy.closeLog then
    begin
      FStream.free;
      FStream := nil;
      FOpenName := '';
    end;
  Finally
    FLock.UnLock;
  End;
end;

function TLogger.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FFilename.length * sizeof(char)) + 12);
  inc(result, FPolicy.sizeInBytes(magic));
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
  FLock := TFslLock.Create('console');
  FListeners := TFslList<TLogListener>.Create;
  FStarting := true;
  FStartTime := now;
  FLastDay := 0;
  FHeld := TStringList.Create;
  DoubleFreeCallBack := LogDoubleFreeCallBack;
  FCPU := TCPUUsageData.create();
end;

destructor TLogging.Destroy;
begin
  FCPU.free;
  DoubleFreeCallBack := nil;
  close;
  FHeld.free;
  FFileLogger.free;
  FListeners.free;
  FLock.free;
  inherited;
end;

procedure TLogging.LogDoubleFreeCallBack(name1, name2 : String);
begin
  log('Attempt to free a class a second time (of type '+name1+' or '+name2+'?)');
end;

procedure TLogging.SetShuttingDown(AValue: boolean);
begin
  if FShuttingDown=AValue then Exit;
  FShuttingDown:=AValue;
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
  if (FFileLogger <> nil) then
  begin
    FFileLogger.free;
    FFileLogger := nil;
  end;
  if (filename <> '') then
    FFileLogger := TLogger.Create(filename);
end;

const
  kb : UInt64 = 1024;

function TLogging.DescribeSize(b, min: UInt64): String;
Begin
  If b = $FFFFFFFF Then
    Result := '??'
  Else If (min <> 0) And (b = min) Then
    Result := '0'
  Else If b = 0 Then
    Result := '0'
  Else If b < kb * 4 Then
    Result := IntToStr(b) + 'b'
  Else If b < kb * kb * 4 Then
    Result := IntToStr(b Div 1024) + 'kb'
  Else If b < kb * kb * kb * 4 Then
    Result := IntToStr(b Div (kb * kb)) + 'Mb'
  Else
    Result := IntToStr(b Div (kb * kb * kb)) + 'Gb';
End;

//
//function memToMb(v : UInt64) : string;
//begin
//  v := v div 1024;
//  v := v div 1024;
//  result := inttostr(v)+'MB';
//end;
//
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

function TLogging.InternalMem : UInt64;
{$IFDEF DELPHI}
var
  st : THeapStatus;
{$ENDIF}
begin
{$IFDEF DELPHI}
  st := GetHeapStatus;
  result := st.TotalAllocated + st.Overhead;
{$ELSE}
  result := TFPCMemoryManagerTracker.totalMemory;
{$ENDIF}
end;

function TLogging.MemoryStatus(full : boolean) : String;
// memory status has 2 parts: internal and OS
var
  os : UInt64;
begin
  if full then
  begin
  os := OSMem;
  if os <> 0 then
    result := DescribeSize(Logging.InternalMem, 0) + ' / '+DescribeSize(os, 0)
  else
    result := DescribeSize(Logging.InternalMem, 0);
  end
  else
    result := DescribeSize(Logging.InternalMem, 0);
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
        FLock.Lock('checkDay');
        try
          System.Writeln(s);
        finally
          FLock.unlock;
        end;
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
  FLock.Lock('log');
  try
    if FWorkingLine <> '' then
    begin
      FHeld.add(s);
      exit;
    end;
  finally
    FLock.unlock;
  end;

  checkDay;
  if FStarting then
    s := FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - FStartTime)+' '+MemoryStatus(false)+' '+FCPU.usage+' '+s
  else
    s := FormatDateTime('hh:nn:ss', now)+ ' '+s;
  if FFileLogger <> nil then
    FFileLogger.WriteToLog(s+#13#10);
  if FLogToConsole then
  begin
    try
      FLock.Lock('log2');
      try
        System.Writeln(s);
      finally
        FLock.unlock;
      end;
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
      FLock.Lock('start');
      try
        System.Write(s);
      finally
        FLock.unlock;
      end;
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
  begin
    FLock.Lock('continue');
    try
      System.Write(s);
    finally
      FLock.unlock;
    end;
  end;
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
  msg, h : String;
begin
  if FLogToConsole then
  begin
    FLock.Lock('finish');
    try
      System.Writeln(s);
    finally
      FLock.unlock;
    end;
  end;

  msg := FWorkingLine + s;
  FWorkingLine := '';
  if FStarting then
    msg := FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - FStartTime)+' '+msg
  else
    msg := FormatDateTime('hh:nn:ss', now)+ ' '+msg;

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
  FLock.Lock('finish2');
  try
    for h in FHeld do
      log(h);
    FHeld.clear;
  finally
    FLock.unlock;
  end;
end;

procedure TLogging.finishIfNeeded;
begin
  if FWorkingLine <> '' then
    finish;
end;

function TLogging.Counter: String;
begin
  inc(FCount);
  result := ' #'+inttostr(FCount);
end;

function TLogging.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FFileLogger.sizeInBytes(magic));
  inc(result, FListeners.sizeInBytes(magic));
  inc(result, (FWorkingLine.length * sizeof(char)) + 12);
end;


{ TFslTimeTracker }

constructor TFslTimeTracker.Create;
begin
  inherited;
  FStart := GetTickCount64;
  FLast := FStart;
  FLog := TFslStringBuilder.create;
  Flog.append('0 0 : start'#13#10);
end;

destructor TFslTimeTracker.Destroy;
begin
  FLog.free;
  inherited;
end;

function TFslTimeTracker.link: TFslTimeTracker;
begin
  result := TFslTimeTracker(inherited Link);
end;

function TFslTimeTracker.log : String;
begin
  result := Flog.AsString;
end;

procedure TFslTimeTracker.step(name: String);
var
  t : int64;
begin
  t := GetTickCount64;
  Flog.append(StringPadRight(inttostr(t-FStart)+' '+inttostr(t - FLast), ' ', 8)+': '+name+#13#10);
  FLast := t;
end;

function TFslTimeTracker.total: integer;
begin
  result := GetTickCount64 - FStart;
end;


Initialization
  Logging := TLogging.Create;
Finalization
  Logging.free;
end.

