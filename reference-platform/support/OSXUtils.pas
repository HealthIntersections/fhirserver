unit OSXUtils;

interface

{
other code to fix:
 - FileSupport
 - AdvFiles

 GetExtForMimeType
}
{$IFDEF MACOS}

uses
  Posix.Unistd, Posix.Pthread, Posix.Wctype,
  MacApi.CoreServices, Macapi.Mach,
  Math, SysUtils;

const
  ERROR_SUCCESS = 0;
  FILE_ATTRIBUTE_NORMAL = 0;

type
  DWord = UInt32;
  LargeUInt = UInt64;

  TRTLCriticalSection = class (TObject);

  TSystemTime = record
  end;

procedure EnterCriticalSection(var cs : TRTLCriticalSection);
function TryEnterCriticalSection(var cs : TRTLCriticalSection) : boolean;
procedure LeaveCriticalSection(var cs : TRTLCriticalSection);
procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
procedure DeleteCriticalSection(var cs : TRTLCriticalSection);

function InterlockedDecrement(var i : integer) :integer;
function InterlockedIncrement(var i : integer) :integer;

procedure Sleep(iTime : cardinal);
function GetCurrentThreadID : Cardinal;

procedure QueryPerformanceFrequency(var freq : Int64);
procedure QueryPerformanceCounter(var count : Int64);
function GetTickCount : cardinal;

{$ENDIF}

implementation

{$IFDEF MACOS}

procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
begin
  cs :=  TRTLCriticalSection.Create;
end;

procedure EnterCriticalSection(var cs : TRTLCriticalSection);
begin
  TMonitor.Enter(cs);
end;

function TryEnterCriticalSection(var cs : TRTLCriticalSection) : boolean;
begin
  result := TMonitor.TryEnter(cs);
end;

procedure LeaveCriticalSection(var cs : TRTLCriticalSection);
begin
  TMonitor.Exit(cs);
end;

procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
begin
  cs.Free;
end;

function InterlockedDecrement(var i : integer) :integer;
begin
  AtomicDecrement(i);
  result := i;
end;

function InterlockedIncrement(var i : integer) :integer;
begin
  AtomicIncrement(i);
  result := i;
end;

procedure Sleep(iTime : cardinal);
begin
  usleep(iTime * 1000);
end;

function GetCurrentThreadID : Cardinal;
begin
  result := Posix.Pthread.GetCurrentThreadID;
end;

procedure QueryPerformanceFrequency(var freq : Int64);
begin
  freq := 1000000000; // nano seconds
end;

procedure QueryPerformanceCounter(var count : Int64);
begin
  count := AbsoluteToNanoseconds(mach_absolute_time);
end;

function GetTickCount : cardinal;
begin
  result := AbsoluteToNanoseconds(mach_absolute_time) div 1000000;
end;

{$ENDIF}


end.
