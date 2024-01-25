unit ftk_console;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$i fhir.inc}

interface

uses
  Sysutils, Classes,
  fsl_base, fsl_utilities, fsl_threads, fsl_logging;

type

  { TToolkitConsole }

  TToolkitConsole = class (TLogListener)
  private
    FLock : TFslLock;
    FLines : TStringList;

  protected
    procedure newDay(const s : String); override;
    procedure log(const s : String); override;
    procedure closing; override;

    function transient : boolean; override;
    procedure logStart(s : String); override;
    procedure logContinue(s : String); override;
    procedure logFinish(s : String); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GetIncoming(list : TStringList);
  end;

implementation

{ TToolkitConsole }

procedure TToolkitConsole.newDay(const s: String);
begin
end;

procedure TToolkitConsole.log(const s: String);
begin
  FLock.Lock;
  try
    FLines.add(s);
  finally
    FLock.Unlock;
  end;
end;

procedure TToolkitConsole.closing;
begin
end;

function TToolkitConsole.transient: boolean;
begin
  Result := false
end;

procedure TToolkitConsole.logStart(s: String);
begin
  inherited logStart(s);
end;

procedure TToolkitConsole.logContinue(s: String);
begin
  inherited logContinue(s);
end;

procedure TToolkitConsole.logFinish(s: String);
begin
  inherited logFinish(s);
end;

constructor TToolkitConsole.Create;
begin
  inherited Create;
  FLock := TFslLock.Create('console');
  FLines := TStringList.Create;
end;

destructor TToolkitConsole.Destroy;
begin
  FLines.free;
  FLock.free;
  inherited Destroy;
end;

procedure TToolkitConsole.GetIncoming(list: TStringList);
begin
  FLock.Lock;
  try
    list.assign(FLines);
    FLines.Clear;
  finally
    FLock.Unlock;
  end;
end;

end.
