unit FHIRLog;

interface

uses
  SysUtils;

procedure logt(s : String);
procedure logtn(s : String);

implementation

var
  lastday : integer = 0;
  starttime : TDateTime = 0;

procedure logt(s : String);
var
  today : integer;
begin
  if starttime = 0 then
    starttime := now;
  today := trunc(now);
  if today <> lastday then
  begin
    System.Writeln(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------');
    lastDay := today;
  end;

  System.Writeln(FormatDateTime('hh:nn:ss', now)+ ' '+FormatDateTime('hh:nn:ss', now - startTime)+' '+s);
end;

procedure logtn(s : String);
var
  today : integer;
begin
  today := trunc(now);
  if today <> lastday then
  begin
    System.Writeln(FormatDateTime('yyyy-mm-dd', today)+ '--------------------------------');
    lastDay := today;
  end;

  System.Write(FormatDateTime('hh:nn:ss', now)+ ' '+s);
end;



end.
