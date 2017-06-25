unit OSXUtils;

interface

{
other code to fix:
 - timezone stuff in DateSupport
 - FileSupport
 - AdvFiles
}
{$IFDEF OSX}

uses
  Posix.Unistd, Posix.Pthread, Posix.Wctype,
  SysUtils;

const
  ERROR_SUCCESS = 0;
  FILE_ATTRIBUTE_NORMAL = 0;

type
  DWord = UInt32;

  TRTLCriticalSection = class (TObject);

  TSystemTime = record
  end;

procedure EnterCriticalSection(var cs : TRTLCriticalSection);
procedure LeaveCriticalSection(var cs : TRTLCriticalSection);
procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
procedure DeleteCriticalSection(var cs : TRTLCriticalSection);

function InterlockedDecrement(var i : integer) :integer;
function InterlockedIncrement(var i : integer) :integer;

procedure Sleep(iTime : cardinal);
function GetCurrentThreadID : Cardinal;

{$ENDIF}

implementation

{$IFDEF OSX}

procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
begin
  cs :=  TRTLCriticalSection.Create;
end;

procedure EnterCriticalSection(var cs : TRTLCriticalSection);
begin
  TMonitor.Enter(cs);
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


{$ENDIF}


end.
