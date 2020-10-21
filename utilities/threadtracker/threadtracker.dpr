library ThreadTracker;

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

uses
  Windows,
  System.SysUtils,
  System.Classes;

{$R *.res}

type
  TTheadRecord = record
    id : cardinal;
    name : AnsiString;
    state : AnsiString;
  end;
  PTheadRecord = ^TTheadRecord;

var
  GLock : TRTLCriticalSection;
  GList : TList;

procedure setup;
begin
  InitializeCriticalSection(GLock);
  GList := TList.Create;
end;

procedure recordThread;
var
  id : cardinal;
  i : integer;
  p : PTheadRecord;
begin
  id := GetCurrentThreadId;
  EnterCriticalSection(GLock);
  try
    for i := GList.Count - 1 downto 0 do
    begin
      p := GList[i];
      if (p.id = id) then
      begin
        Dispose(p);
        Glist.Delete(i);
      end;
    end;
    new(p);
    p.id := id;
    p.name := 'Unknown';
    GList.Add(p);
  finally
    LeaveCriticalSection(GLock);
  end;
end;

procedure scrubThread;
var
  id : cardinal;
  i : integer;
  p : PTheadRecord;
begin
  id := GetCurrentThreadId;
  EnterCriticalSection(GLock);
  try
    for i := GList.Count - 1 downto 0 do
    begin
      p := GList[i];
      if (p.id = id) then
      begin
        Dispose(p);
        Glist.Delete(i);
      end;
    end;
  finally
    LeaveCriticalSection(GLock);
  end;
end;

procedure tearDown;
var
  i : integer;
  p : PTheadRecord;
begin
  for i := GList.Count - 1 downto 0 do
  begin
    p := GList[i];
    Dispose(p);
    Glist.Delete(i);
  end;
  GList.Free;
  DeleteCriticalSection(GLock);
end;

procedure THThreadName(name : PAnsiChar); stdcall; export;
var
  id : cardinal;
  i : integer;
  p : PTheadRecord;
begin
  id := GetCurrentThreadId;
  EnterCriticalSection(GLock);
  try
    for i := GList.Count - 1 downto 0 do
    begin
      p := GList[i];
      if (p.id = id) then
      begin
        p.name := name;
        exit;
      end;
    end;
    new(p);
    p.id := id;
    p.name := name;
    GList.Add(p);
  finally
    LeaveCriticalSection(GLock);
  end;
end;

procedure THThreadStatus(status : PAnsiChar); stdcall; export;
var
  id : cardinal;
  i : integer;
  p : PTheadRecord;
begin
  id := GetCurrentThreadId;
  EnterCriticalSection(GLock);
  try
    for i := GList.Count - 1 downto 0 do
    begin
      p := GList[i];
      if (p.id = id) then
      begin
        p.state := status;
        exit;
      end;
    end;
    new(p);
    p.id := id;
    p.name := 'Unknown';
    p.state := status;
    GList.Add(p);
  finally
    LeaveCriticalSection(GLock);
  end;
end;

function THThreadInfo : PAnsiChar; stdcall; export;
var
  id : cardinal;
  i : integer;
  p : PTheadRecord;
  s : AnsiString;
begin
  s := 'Unknown thread';
  id := GetCurrentThreadId;
  EnterCriticalSection(GLock);
  try
    for i := GList.Count - 1 downto 0 do
    begin
      p := GList[i];
      if (p.id = id) then
      begin
        if p.state <> '' then
          s := p.name+' = '+p.state
        else
          s := p.name;
        break;
      end;
    end;
  finally
    LeaveCriticalSection(GLock);
  end;
  result := GetMemory(length(s)+1);
  ZeroMemory(result, length(s)+1);;
  move(s[1], result^, length(s));
end;

function THThreadCount : Cardinal; stdcall; export;
begin
  EnterCriticalSection(GLock);
  try
    result := Glist.Count;
  finally
    LeaveCriticalSection(GLock);
  end;

end;

function THGetReport : PAnsiChar; stdcall; export;
var
  i : integer;
  s : AnsiString;
  p : PTheadRecord;
begin
  s := '';
  EnterCriticalSection(GLock);
  try
    for i := 0 to GList.Count - 1 do
    begin
      p := GList[i];
      if (s <> '') then
        s := s + '|';
      if (p.state <> '') then
        s := s + inttohex(p.id, 8)+': '+p.name+' = '+p.state
      else
        s := s + inttohex(p.id, 8)+': '+p.name;
    end;
  finally
    LeaveCriticalSection(GLock);
  end;
  result := GetMemory(length(s)+1);
  ZeroMemory(result, length(s)+1);;
  move(s[1], result^, length(s));
end;

procedure THFreeMem(rep : PAnsiChar); stdcall; export;
begin
  Dispose(rep);
end;

procedure DLLEntryPoint(dwReason: DWord);
begin
  case dwReason of
  DLL_PROCESS_ATTACH: setUp;
  DLL_PROCESS_DETACH: tearDown;
  DLL_THREAD_ATTACH: recordThread;
  DLL_THREAD_DETACH: scrubThread;
  end;
end;

exports
  THThreadName, THThreadStatus, THThreadInfo, THThreadCount, THGetReport, THFreeMem;

begin
  DllProc := @DLLEntryPoint;
  DLLEntryPoint(DLL_PROCESS_ATTACH);
end.

