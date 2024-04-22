unit fsl_cpu;

{$i fhir.inc}

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
// adapted from https://w-shadow.com/blog/2006/08/27/how-to-get-the-cpu-usage-of-a-process/ and

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_threads;

const
  wsMinMeasurementInterval=250;
  {minimum amount of time that must have elapsed to calculate CPU usage, miliseconds. If time elapsed is less than this, previous result is returned, or zero, if there is no previous result.}

type

  { TCPUUsageData }

  TCPUUsageData = class (TFslObject)
  private
    FLock : TFslLock;
    FPID : cardinal;
    {$IFDEF WINDOWS}
    FHandle:cardinal;
    {$ENDIF}
    FOldUser, FOldKernel : Int64;
    FLastUpdateTime : cardinal;
    FLastUsage : single; //Last result of wsGetCpuUsage is saved here
  public
    constructor Create(PID : cardinal = 0);
    destructor Destroy; override;

    function cpuUsage : Single;
    function usage : String;
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows;
{$ELSE}
  baseunix;
{$ENDIF}

constructor TCPUUsageData.Create(PID : cardinal = 0);  
{$IFDEF WINDOWS}
var
  mCreationTime, mExitTime, mKernelTime, mUserTime:_FILETIME;
  h : cardinal;
{$ENDIF}
begin
  inherited create;
  FLock := TFslLock.create('cpu.usage');
{$IFDEF WINDOWS} 
  if FPID = 0 then
    FPID := GetCurrentProcessId;

  //We need a handle with PROCESS_QUERY_INFORMATION privileges
  FHandle := OpenProcess(PROCESS_QUERY_INFORMATION,false,FPID);
  if FHandle <> 0 then
  begin
    FLastUpdateTime := GetTickCount64;
    FLastUsage:=0;
    if GetProcessTimes(FHandle, mCreationTime, mExitTime, mKernelTime, mUserTime) then
    begin
      //convert _FILETIME to Int64
      FOldKernel:=int64(mKernelTime.dwLowDateTime or (mKernelTime.dwHighDateTime shr 32));
      FOldUser:=int64(mUserTime.dwLowDateTime or (mUserTime.dwHighDateTime shr 32));
    end
  end;
{$ELSE}
{$ENDIF};
end;

destructor TCPUUsageData.Destroy;
begin
{$IFDEF WINDOWS}  
  CloseHandle(FHandle);
{$ELSE}
{$ENDIF};
  FLock.Free;
  inherited;
end;

//{$IFNDEF WINDOWS}
//function getrusage(usage : integer; var data : rusage) : integer;
//begin
////  result := host_processor_info();
//  // result := Getrusage();
//end;
//
//{$ENDIF}

function TCPUUsageData.cpuUsage : Single;    
{$IFDEF WINDOWS}
var
  mCreationTime, mExitTime, mKernelTime, mUserTime:_FILETIME;
  DeltaMs, ThisTime:cardinal;
  mKernel, mUser, mDelta:int64;
  {$ENDIF}
begin
  result := 0;
  {$IFDEF WINDOWS}
    if (FHandle <> 0) then
    begin
      FLock.Lock('cpuUage');
      try
        result := FLastUsage;
        ThisTime := GetTickCount; //Get the time elapsed since last query
        DeltaMs := ThisTime - FLastUpdateTime;
        if DeltaMs > wsMinMeasurementInterval then
        begin
          FLastUpdateTime := ThisTime;
          GetProcessTimes(FHandle, mCreationTime, mExitTime, mKernelTime, mUserTime);
          //convert _FILETIME to Int64.
          mKernel := int64(mKernelTime.dwLowDateTime or (mKernelTime.dwHighDateTime shr 32));
          mUser := int64(mUserTime.dwLowDateTime or (mUserTime.dwHighDateTime shr 32));
          //get the delta
          mDelta := mUser + mKernel - FOldUser - FOldKernel;
          FOldUser := mUser;
          FOldKernel := mKernel;
          Result := (mDelta / DeltaMs) / 100; //mDelta is in units of 100 nanoseconds, so…
          FLastUsage := Result;       //just in case you want to use it later, too
        end;
      finally
        FLock.Unlock;
      end;
    end;
  {$ELSE}
    // getrusage...
  {$ENDIF};
end;

function TCPUUsageData.usage: String;
begin
  result := inttostr(trunc(cpuUsage))+'%';
end;


end.

