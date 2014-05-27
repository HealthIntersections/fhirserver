{! 1 !}
{0.00-006  02 Sep 04 12:49  [21370]  User: Grahame Grieve    add total}

unit KDBLogging;

interface

uses
  Classes,
  AdvObjects,
  kCritSct,
  SysUtils;

type
  TKDBReportFormat = (krfText, krfHTML, krfXML);

  TKDBLogEntry = {private} class (TAdvObject)
  private
    FUsage : String;
    FTotalCount : integer;
    FErrorCount : Integer;
    FMinLength : TDateTime;
    FMaxLength : TDateTime;
    FTotalLen : TDateTime;
    FTotalRows : Integer;
    FTotalPreps : Integer;
    FRunningAvg : TDateTime;
    FExceptions : TStringList;
    procedure RecordUsage(AWhenStarted : TDateTime; ARowCount, APrepareCount : integer; AException : Exception; AErrMsg : string);
    class function HTMLDoco : String;
    class function Header(AFormat : TKDBReportFormat) : String; Overload;
    class function Footer(AFormat : TKDBReportFormat) : String; Overload;
    function Report(AExceptions : Boolean; AFormat : TKDBReportFormat; APrefix : String = '') : String; Overload;
  public
    constructor create(AUsage : String);
    destructor Destroy; override;
  end;

  TKDBLogger = class (TAdvObject)
  private
    FList : TStringList;
    FTotal : Integer;
    FLock : TCriticalSection;
    function GetLogEntry(AUsage : String) : TKDBLogEntry;
  public
    Constructor Create; override;
    destructor Destroy; override;
    procedure RecordUsage(AUsage : String; AWhenStarted : TDateTime; ARowCount, APrepareCount : integer; AException : Exception; AErrMsg : string);
    function Report(AFormat : TKDBReportFormat) : String; Overload;
    function InteractiveReport(AParam : String; APrefix : String) : string;
    function ErrorReport(AUsage : String; AFormat : TKDBReportFormat) : String;

    property Total : Integer read FTotal;
  end;

implementation

uses
  IdSoapConsts,
  KDate,
  Math,
  StringSupport;

const
  ASSERT_UNIT = 'KDBLogger';
  MAX_EXCEPTION_LIST = 20;
  MILLI_SECOND_LENGTH = SECOND_LENGTH / 1000;

{ TKDBLogger }

constructor TKDBLogger.create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FTotal := 0;
  FList := TStringList.create(true);
  FList.Sorted := true;
  FList.Duplicates := dupError;
end;

destructor TKDBLogger.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogger.destroy';
begin
  FList.free;
  FLock.Free;
  inherited;
end;

procedure TKDBLogger.RecordUsage(AUsage: String; AWhenStarted: TDateTime; ARowCount, APrepareCount : integer; AException: Exception; AErrMsg : string);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogger.RecordUsage';
begin
  assert(AUsage <> '', ASSERT_LOCATION+': Usage is not valid');
  assert(AWhenStarted > 0, ASSERT_LOCATION+': WhenStarted is not valid');


  FLock.Lock;
  try
    inc(FTotal);
    GetLogEntry(AUsage).RecordUsage(AWhenStarted, ARowCount, APrepareCount, AException, AErrMsg);
  finally
    FLock.UnLock;
  end;
end;

function TKDBLogger.ErrorReport(AUsage: String; AFormat : TKDBReportFormat): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogger.ErrorReport';
begin
  assert(AUsage <> '', ASSERT_LOCATION+': Usage is not valid');

  FLock.Lock;
  try
    inc(FTotal);
    result := GetLogEntry(AUsage).Report(true, AFormat)
  finally
    FLock.UnLock;
  end;
end;

function TKDBLogger.Report(AFormat: TKDBReportFormat): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogger.Report';
var
  i : integer;
begin
  FLock.Lock;
  try
    result := TKDBLogEntry.Header(AFormat);
    for i := 0 to FList.Count - 1 do
      begin
      result := result + (FList.Objects[i] as TKDBLogEntry).Report(false, AFormat);
      end;
    result := result + TKDBLogEntry.Footer(AFormat);
  finally
    FLock.UnLock;
  end;
end;

function TKDBLogger.GetLogEntry(AUsage: String): TKDBLogEntry;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogger.GetLogEntry';
var
  LIndex : integer;
begin
  assert(AUsage <> '', ASSERT_LOCATION+': Usage is not valid');
  assert(FLock.LockedToMe, ASSERT_LOCATION+': not locked');

  if FList.Find(AUsage, LIndex) then
    begin
    result := FList.Objects[LIndex] as TKDBLogEntry;
    assert(SameText(result.FUsage, AUsage), ASSERT_LOCATION+': Usage mismatch "'+Result.FUsage+'"/"'+AUsage+'"');
    end
  else
    begin
    result := TKDBLogEntry.create(AUsage);
    FList.AddObject(AUsage, result);
    end;
end;

function TKDBLogger.InteractiveReport(AParam, APrefix: String): string;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogger.InteractiveReport';
var
  i : integer;
begin
  FLock.Lock;
  try
    if AParam <> '' then
      begin
      result := result + GetLogEntry(AParam).Report(true, krfHTML, '');
      end
    else
      begin
      result := TKDBLogEntry.Header(krfHTML);
      for i := 0 to FList.Count - 1 do
        begin
        result := result + (FList.Objects[i] as TKDBLogEntry).Report(false, krfHTML, APrefix);
        end;
      result := result + TKDBLogEntry.Footer(krfHTML);
      end;
  finally
    FLock.UnLock;
  end;
end;

{ TKDBLogEntry }

constructor TKDBLogEntry.create(AUsage : String);
begin
  inherited create;
  FUsage := AUsage;
  FTotalCount := 0;
  FErrorCount := 0;
  FMinLength := 0;
  FMaxLength := 0;
  FTotalLen := 0;
  FTotalRows := 0;
  FTotalPreps := 0;
  FRunningAvg := 0;
  FExceptions := TStringList.create(false);
end;

destructor TKDBLogEntry.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogEntry.destroy';
begin
  FExceptions.free;
  inherited;
end;

procedure TKDBLogEntry.RecordUsage(AWhenStarted: TDateTime; ARowCount, APrepareCount : integer; AException: Exception; AErrMsg : string);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogEntry.RecordUsage';
var
  LLen : TDateTime;
  LNow : TDateTime;
begin
  assert(AWhenStarted > 0, ASSERT_LOCATION+': WhenStarted is not valid');
  LNow := Now;

  if assigned(AException) or (AErrMsg <> '') then
    begin
    inc(FErrorCount);
    if assigned(AException) then
      begin
      FExceptions.add(AException.ClassName+#1+AException.Message+#1+FormatDateTime('c', LNow));
      end
    else
      begin
      FExceptions.add('unknown'+#1+AErrMsg+#1+FormatDateTime('c', LNow));
      end;
    if FExceptions.Count > MAX_EXCEPTION_LIST then
      begin
      FExceptions.Delete(0);
      end;
    end;

  LLen := LNow - AWhenStarted;
  if FTotalCount = 0 then
    begin
    FMinLength := LLen;
    FMaxLength := LLen;
    end
  else
    begin
    FMinLength := Min(FMinLength, LLen);
    FMaxLength := Max(FMinLength, LLen);
    end;
  FTotalLen := FTotalLen + LLen;
  FTotalRows := FTotalRows + ARowCount;
  FTotalPreps := FTotalPreps + APrepareCount;
  FRunningAvg := ((FRunningAvg * 19) + LLen) / 20;
  inc(FTotalCount);
end;

class function TKDBLogEntry.HTMLDoco : String;
begin
  result :=
  '<p><b>In use</b></p>'+#13#10+
  ''+#13#10+
  '<p>Lists the actual current usage of connections. Typically, a few connections'+#13#10+
  'are in use at any one time - the usage list should roll over constantly. If'+#13#10+
  'connections are persisting in this list, then there is a problem in the code'+#13#10+
  'that manages the connection (either in HL7Connect, or in a script - check'+#13#10+
  'scripts to see whether the given name comes from a script).</p>'+#13#10+
  ''+#13#10+
  '<p><b>Table</b></p>'+#13#10+
  '<p>The table summarises the past history of connection usage, and doesn''t include connections in use</p>'+#13#10+
  '<table>'+#13#10+
  ' <tr><td>Usage</td><td>The name given when the connection is acquired - describes the usage kind'+#13#10+
  ' <tr><td>Count</td><td>The number of times a connection has been used with this name'+#13#10+
  ' <tr><td>Error</td><td>The number of times this usage has had an error (the error may be in the code that uses the data, not the database itself)'+#13#10+
  ' <tr><td>Rows</td><td>The total number of rows returned for this usage (may be 0 if the usage is associated with update/insert/delete or metadata statements)'+#13#10+
  ' <tr><td>Stmts</td><td>The number of SQL statements executed'+#13#10+
  ' <tr><td>Min</td><td>The shortest recorded time to execute the usage (all times in ms)'+#13#10+
  ' <tr><td>Max</td><td>The longest recorded time to execute the usage'+#13#10+
  ' <tr><td>Average</td><td>The average time taken for this usage (total use)'+#13#10+
  ' <tr><td>Running</td><td>The weighted average of time (over the last 20 uses)'+#13#10+
  '</table>'+#13#10+
  '<p>The first four columns are used to estimate the importance of a usage for the need to optimisation - rare usages are not important, where as'+#13#10+
  'common usages with high utilisation counts are important. The last 4 columns give a sense of actual performance</p>'+#13#10;
end;

class function TKDBLogEntry.Footer(AFormat: TKDBReportFormat): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogEntry.TKDBLogEntry';
begin
  case AFormat of
    krfText :result := '';
    krfHTML :result := '</table>'+HTMLDoco;
    krfXML  :result := '</report>';
  else
    assert(false, ASSERT_LOCATION +': unknown format '+inttostr(ord(AFormat)));
  end;
end;


function MakeLength(AStr : String; ALen : Integer):String;
begin
  if Length(AStr) >= ALen then
    begin
    AStr := copy(AStr, 1, ALen-1)+'_';
    end;
  result := StringPadRight(AStr, ' ', ALen)+' ';
end;

class function TKDBLogEntry.Header(AFormat: TKDBReportFormat): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogEntry.Header';
begin
  case AFormat of
    krfText :
        result :=
          MakeLength('Usage', 20)+
          MakeLength('Count', 8)+
          MakeLength('Error', 6)+
          MakeLength('Rows', 8)+
          MakeLength('Stmts', 8)+
          MakeLength('Min.', 8)+
          MakeLength('Max.', 8)+
          MakeLength('Average', 8)+
          MakeLength('Running', 8)+EOL_PLATFORM;
    krfHTML : result := '<table cellspacing=0 cellpadding=2 border=1 style="font-size:11px;"><th>Usage</th><th>Count</th><th>Error</th><th>Rows</th><th>Stmts</th><th>Min.</th><th>Max.</th><th>Average</th><th>Running</th>'+EOL_PLATFORM;
    krfXML  :  result := '<report>';
  else
    assert(false, ASSERT_LOCATION +': unknown format '+inttostr(ord(AFormat)));
  end;

end;

function TKDBLogEntry.Report(AExceptions: Boolean; AFormat: TKDBReportFormat; APrefix : String = ''): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBLogEntry.Report';
var
  i : integer;
  s, l, r : String;
begin
  if AExceptions and (AFormat <> krfXML) then
    begin
    result := '';
    case AFormat of
      krfText :
        for i := FExceptions.count -1 downto 0 do
          begin
          Stringsplit(FExceptions[i], #1, l, s);
          StringSplit(s, #1, r, s);
          result := result + MakeLength(l, 20)+ MakeLength(r, 14)+s+EOL_PLATFORM;
          end;
      krfHTML :
         begin
         result := '<table cellspacing=0 cellpadding=2 border=1><th>Class</th><th>Error</th><th>Time</th>'+EOL_PLATFORM;
         for i := FExceptions.count -1 downto 0 do
           begin
           StringSplit(FExceptions[i], #1, l, s);
           StringSplit(s, #1, r, s);
           result := result + '<tr><td>'+l+'</td><td>'+r+'</td><td>'+s+'</td></tr>'+EOL_PLATFORM;
           end;
         result := result + '</table>'+EOL_PLATFORM;
         end;
    else
      assert(false, ASSERT_LOCATION +': unknown format '+inttostr(ord(AFormat)));
    end;
    end
  else
    begin
    case AFormat of
      krfText :
        begin
        result :=
            MakeLength(FUsage, 20)+
            MakeLength(inttostr(FTotalCount), 8)+
            MakeLength(inttostr(FErrorCount), 8)+
            MakeLength(inttostr(FTotalRows), 8)+
            MakeLength(inttostr(FTotalPreps), 8)+
            MakeLength(IntToStr(trunc((FMinLength) / MILLI_SECOND_LENGTH)), 8)+
            MakeLength(IntToStr(trunc((FMaxLength) / MILLI_SECOND_LENGTH)), 8)+
            MakeLength(IntToStr(trunc((FTotalLen/FTotalCount) / MILLI_SECOND_LENGTH)), 8);
        if FTotalCount >= 20 then
          result := result + MakeLength(IntToStr(trunc((FRunningAvg) / MILLI_SECOND_LENGTH)), 8)+EOL_PLATFORM
        else
          result := result + MakeLength('--', 8)+EOL_PLATFORM;
        end;
      krfHTML :
        begin
        result := '<tr>';
        if (APrefix <> '') and (FErrorCount > 0) then
          begin
          result := result +'<td><a href="'+APrefix+'/'+FUsage+'">'+FUsage+'</a></td>';
          end
        else
          begin
          result := result +'<td>'+FUsage+'</td>';
          end;
        result := result +
            '<td>'+inttostr(FTotalCount)+'</td>'+
            '<td>'+inttostr(FErrorCount)+'</td>'+
            '<td>'+inttostr(FTotalRows)+'</td>'+
            '<td>'+inttostr(FTotalPreps)+'</td>'+
            '<td>'+IntToStr(trunc((FMinLength) / MILLI_SECOND_LENGTH))+'</td>'+
            '<td>'+IntToStr(trunc((FMaxLength) / MILLI_SECOND_LENGTH))+'</td>'+
            '<td>'+IntToStr(trunc((FTotalLen/FTotalCount) / MILLI_SECOND_LENGTH))+'</td>';
        if FTotalCount >= 20 then
          result := result +
            '<td>'+IntToStr(trunc((FRunningAvg) / MILLI_SECOND_LENGTH))+'</td></tr>'+EOL_PLATFORM
        else
          result := result +
            '<td>--</td></tr>'+EOL_PLATFORM
        end;
      krfXML  :
        begin
        result :=
            '<Usage id="'+FUsage+'">'+
            '<TotalCount>'+inttostr(FTotalCount)+'</TotalCount>'+
            '<ErrorCount>'+inttostr(FErrorCount)+'</ErrorCount>'+
            '<TotalRows>'+inttostr(FTotalRows)+'</TotalRows>'+
            '<Statements>'+inttostr(FTotalPreps)+'</Statements>'+
            '<MinLength>'+IntToStr(trunc((FMinLength) / MILLI_SECOND_LENGTH))+'</MinLength>'+
            '<MaxLength>'+IntToStr(trunc((FMaxLength) / MILLI_SECOND_LENGTH))+'</MaxLength>'+
            '<Average>'+IntToStr(trunc((FTotalLen/FTotalCount) / MILLI_SECOND_LENGTH))+'</Average>';
        if FTotalCount >= 20 then
          result := result +
            '<RunningAvg>'+IntToStr(trunc((FRunningAvg) / MILLI_SECOND_LENGTH))+'</RunningAvg>';
        if FExceptions.count > 0 then
          begin
          result := result + '<errors>';
          for i := FExceptions.count -1 downto 0 do
            begin
            StringSplit(FExceptions[i], #1, l, s);
            StringSplit(s, #1, r, s);
            result := result + '<error><class>'+l+'</class><msg>'+r+'</msg><time>'+s+'</time></error>';
            end;
          result := result + '</errors>';
          end;
        result := result + '</Usage>';
        end;
    else
      assert(false, ASSERT_LOCATION +': unknown format '+inttostr(ord(AFormat)));
    end;
  end;
end;

end.

