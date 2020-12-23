unit fsl_service;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

{$I fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities;

type

  { TSystemService }

  TSystemService = class abstract (TFslObject)
  Private
    FSystemName : String;
    FDisplayName : String;
    FDebugMode : boolean;
    FWantStop : boolean;
    FStopReason : String;
    FTellUser : boolean;
    FIsContained: Boolean;
    FStartTime : TDateTime;
  Protected
    function CanInstall : boolean; Virtual;
    function CanStart : boolean; Virtual;
    procedure postStart; Virtual;
    procedure DoStop; Virtual;
    procedure DoRemove; Virtual;

    procedure dump; virtual;
    function CheckClose(var s: String): Boolean; Virtual;
    function command(cmd: String): boolean; virtual;
  Public
    constructor Create(const ASystemName, ADisplayName: String);
    property DebugMode : Boolean read FDebugMode write FDebugMode;
    property WantStop : Boolean read FWantStop;
    property StopReason: String read FStopReason;
    procedure Stop(AReason : String; ATellUser : boolean = true);
    procedure Execute;
    procedure ConsoleExecute;
    Property DisplayName : String read FDisplayName;
  end;

var
  GService : TSystemService = nil; // there can only be one....

implementation

uses
  fsl_threads,
  fsl_logging;

{ TSystemService }

constructor TSystemService.Create(const ASystemName, ADisplayName: String);
begin
  inherited create;
  FStartTime := now;
  FSystemName := ASystemName;
  FDisplayName := ADisplayName;
  FTellUser := true;
  GService := self;
  FDebugMode := true;
end;

// stub virtual methods
function TSystemService.CanInstall: boolean;
begin
  result := true;
end;

function TSystemService.CanStart: boolean;
begin
  result := true;
end;

function TSystemService.CheckClose(var s: String): Boolean;
begin
  result := false;
end;

function TSystemService.command(cmd: String): boolean;
begin
  result := false;
end;

procedure TSystemService.DoRemove;
begin
  // nothing
end;

procedure TSystemService.DoStop;
begin
  // nothing
end;

procedure TSystemService.dump;
begin
end;

procedure TSystemService.Execute;
var
  LMsg : string;
  LCheckTime : TDateTime;
begin
  LCheckTime := 0;
  try
    if CanStart then
      begin
      try
        postStart;
        repeat
          // todo: is there a way to do this? SetConsoleTitle(pChar(FDisplayName+MemoryStatus));
          if (LCheckTime < Now) then
            begin
            LCheckTime := now + 10 * DATETIME_SECOND_ONE;
            if CheckClose(LMsg) then
              begin
              Stop(LMsg);
              end;
            end;
          Sleep(100);
        until FWantStop;
      finally
        DoStop;
      end;
      end;
  except
    on e:exception do
    begin
      Logging.log('Exception ['+e.classname+'] in Service Execution: '+#13#10+e.message);
    end
  end;
end;

procedure TSystemService.ConsoleExecute;
begin
  Execute;
end;

procedure TSystemService.postStart;
begin
// nothing
end;

procedure TSystemService.Stop(AReason: String; ATellUser : boolean = true);
begin
  FStopReason := AReason;
  FWantStop := true;
  FTellUser := ATellUser;
end;

end.

