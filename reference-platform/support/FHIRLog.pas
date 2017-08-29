unit FHIRLog;

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


interface

uses
  SysUtils;

Type
  TLogEvent = procedure (msg : String) of object;

var
  filelog : boolean = false;
  consolelog : boolean = false;
  logfile : String = '';
  logevent : TLogEvent;

procedure logt(s : String);
procedure logtn(s : String);


implementation

uses
  logging;

var
  lastday : integer = 0;
  starttime : TDateTime = 0;
  log : TLogger;

procedure checklog;
begin
  if (log = nil) and (logfile <> '') then
  begin
    DeleteFile(logfile);
    log := TLogger.Create(logfile);
    log.Policy.Description := 'FHIRServer Start Log';
    log.Policy.AllowExceptions := false;
    log.Policy.FullPolicy := lfpChop;
    log.Policy.MaximumSize := 10240;
  end;
end;

procedure logt(s : String);
var
  today : integer;
begin
  checklog;
  if starttime = 0 then
    starttime := now;
  today := trunc(now);
  if today <> lastday then
  begin
    if filelog then
      log.WriteToLog(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------'+#13#10);
    if consolelog then
    begin
      try
        System.Writeln(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------');
      except
        consolelog := false;
      end;
    end;
    lastDay := today;
  end;

  if filelog then
    log.WriteToLog(FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - startTime)+' '+s+#13#10);
  if consolelog then
    try
      System.Writeln(FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - startTime)+' '+s);
    except
      consolelog := false;
    end;
  if (assigned(logEvent)) then
    logEvent(s);
end;

procedure logtn(s : String);
var
  today : integer;
begin
  checklog;
  today := trunc(now);
  if today <> lastday then
  begin
    if filelog then
      log.WriteToLog(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------'+#13#10);
    if consolelog then
      try
        System.Writeln(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------');
      except
        consolelog := false;
      end;
    lastDay := today;
  end;

  if filelog then
    log.WriteToLog(FormatDateTime('hh:nn:ss', now)+ ' '+s+#13#10);
  if consolelog then
    try
      System.Write(FormatDateTime('hh:nn:ss', now)+ ' '+s);
    except
      consolelog := false;
    end;
  if (assigned(logEvent)) then
    logEvent(s);
end;

Initialization
Finalization
  if log <> nil then
    log.Free;
end.
