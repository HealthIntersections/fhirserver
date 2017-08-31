unit OSXUtils;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
