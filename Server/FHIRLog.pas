unit FHIRLog;

interface

uses
  SysUtils;

var
  filelog : boolean = false;
  logfile : String = '';

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
      log.WriteToLog(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------'+#13#10)
    else
      System.Writeln(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------');
    lastDay := today;
  end;

  if filelog then
    log.WriteToLog(FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - startTime)+' '+s+#13#10)
  else
    System.Writeln(FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - startTime)+' '+s);
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
      log.WriteToLog(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------'+#13#10)
    else
      System.Writeln(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------');
    lastDay := today;
  end;

  if filelog then
    log.WriteToLog(FormatDateTime('hh:nn:ss', now)+ ' '+s+#13#10)
  else
    System.Write(FormatDateTime('hh:nn:ss', now)+ ' '+s);
end;

Initialization
Finalization
  if log <> nil then
    log.Free;
end.
