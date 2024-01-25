unit fsl_fpc_memory;

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

type

  // this unit (and class) exists to work around the fact that the FPC heap tracking
  // is thread specific, and out of the box, there's no way to get the status of the
  // entire heap. This class intercepts the calls and tracks the entire memory usage.
  //
  // there's a small performance overhead for this (~0.3%?)
  //
  // It should be installed sometime during the start up process before any additional
  // threads are started. There's no need to uninstall it
  //

  { TFPCMemoryManagerTracker }

  TFPCMemoryManagerTracker = class
  public
    class function totalMemory : UInt64;
    class procedure install;
  end;

implementation

{ TFPCMemoryManagerTracker }

var
  GRealMM : TMemoryManager;
  GTotalMemory : UInt64;

Function MemSize(p : pointer) : ptruint;
begin
  if p = nil then
    result := 0
  else
    result := GRealMM.MemSize(p);
end;

Function RGetMem(Size:ptruint):Pointer;
begin
  result := GRealMM.GetMem(size);
  interlockedExchangeAdd64(GTotalMemory, MemSize(result));
end;

Function RFreemem(p:pointer):ptruint;
begin
  interlockedExchangeAdd64(GTotalMemory, -MemSize(p));
  result := GRealMM.FreeMem(p);
end;

Function RFreememSize(p:pointer;Size:ptruint):ptruint;
begin
  interlockedExchangeAdd64(GTotalMemory, -MemSize(p));
  result := GRealMM.FreememSize(p, size);
end;

Function RAllocMem(Size:ptruint):Pointer;
begin
  result := GRealMM.AllocMem(Size);
  interlockedExchangeAdd64(GTotalMemory, MemSize(result));
end;

Function RReAllocMem(var p:pointer;Size:ptruint):Pointer;
var
  s : ptruint;
begin
  s := MemSize(p);
  result := GRealMM.ReAllocMem(p, size);
  interlockedExchangeAdd64(GTotalMemory, MemSize(result)-s);
end;

class function TFPCMemoryManagerTracker.totalMemory: UInt64;
begin
  result := GTotalMemory;
end;

class procedure TFPCMemoryManagerTracker.install;
var
  mm : TMemoryManager;
begin
  GetMemoryManager(GRealMM);
  mm := GRealMM;

  mm.Getmem      := RGetMem;
  mm.Freemem     := RFreemem;
  mm.FreememSize := RFreememSize;
  mm.AllocMem    := RAllocMem;
  mm.ReAllocMem  := RReAllocMem;

  GTotalMemory := GetFPCHeapStatus.CurrHeapSize;
  SetMemoryManager(mm);
end;

end.

