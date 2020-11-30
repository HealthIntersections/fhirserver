unit fdb_logging;

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

uses
  Classes,
  fsl_base,
  fsl_threads,
  SysUtils;

type
  TFDBReportFormat = (krfText, krfHTML, krfXML);

  TFDBLogEntry = {private} class (TFslObject)
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
    class function Header(AFormat : TFDBReportFormat) : String; Overload;
    class function Footer(AFormat : TFDBReportFormat) : String; Overload;
    function Report(AExceptions : Boolean; AFormat : TFDBReportFormat; APrefix : String = '') : String; Overload;
  public
    constructor Create(AUsage : String);
    destructor Destroy; override;
  end;

  TFDBLogger = class (TFslObject)
  private
    FList : TStringList;
    FTotal : Integer;
    FLock : TFslLock;
    function GetLogEntry(AUsage : String) : TFDBLogEntry;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RecordUsage(AUsage : String; AWhenStarted : TDateTime; ARowCount, APrepareCount : integer; AException : Exception; AErrMsg : string);
    function Report(AFormat : TFDBReportFormat) : String; Overload;
    function InteractiveReport(AParam : String; APrefix : String) : string;
    function ErrorReport(AUsage : String; AFormat : TFDBReportFormat) : String;

    property Total : Integer read FTotal;
  end;

implementation

uses
  fsl_utilities,
  Math,
  IdGlobal;

const
  ASSERT_UNIT = 'KDBLogger';
  MAX_EXCEPTION_LIST = 20;
  MILLI_SECOND_LENGTH = DATETIME_SECOND_ONE / 1000;
  EOL_WINDOWS = CR + LF;
  EOL_PLATFORM = EOL_WINDOWS;

{ TFDBLogger }

constructor TFDBLogger.create;
begin
  inherited;
  FLock := TFslLock.Create;
  FTotal := 0;
  FList := TStringList.create;
  FList.OwnsObjects := true;
  FList.Sorted := true;
  FList.Duplicates := dupError;
end;

destructor TFDBLogger.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogger.destroy';
begin
  FList.free;
  FLock.Free;
  inherited;
end;

procedure TFDBLogger.RecordUsage(AUsage: String; AWhenStarted: TDateTime; ARowCount, APrepareCount : integer; AException: Exception; AErrMsg : string);
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogger.RecordUsage';
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

function TFDBLogger.ErrorReport(AUsage: String; AFormat : TFDBReportFormat): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogger.ErrorReport';
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

function TFDBLogger.Report(AFormat: TFDBReportFormat): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogger.Report';
var
  i : integer;
begin
  FLock.Lock;
  try
    result := TFDBLogEntry.Header(AFormat);
    for i := 0 to FList.Count - 1 do
      begin
      result := result + (FList.Objects[i] as TFDBLogEntry).Report(false, AFormat);
      end;
    result := result + TFDBLogEntry.Footer(AFormat);
  finally
    FLock.UnLock;
  end;
end;

function TFDBLogger.GetLogEntry(AUsage: String): TFDBLogEntry;
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogger.GetLogEntry';
var
  LIndex : integer;
begin
  assert(AUsage <> '', ASSERT_LOCATION+': Usage is not valid');
  assert(FLock.LockedToMe, ASSERT_LOCATION+': not locked');

  if FList.Find(AUsage, LIndex) then
    begin
    result := FList.Objects[LIndex] as TFDBLogEntry;
    assert(SameText(result.FUsage, AUsage), ASSERT_LOCATION+': Usage mismatch "'+Result.FUsage+'"/"'+AUsage+'"');
    end
  else
    begin
    result := TFDBLogEntry.create(AUsage);
    FList.AddObject(AUsage, result);
    end;
end;

function TFDBLogger.InteractiveReport(AParam, APrefix: String): string;
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogger.InteractiveReport';
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
      result := TFDBLogEntry.Header(krfHTML);
      for i := 0 to FList.Count - 1 do
        begin
        result := result + (FList.Objects[i] as TFDBLogEntry).Report(false, krfHTML, APrefix);
        end;
      result := result + TFDBLogEntry.Footer(krfHTML);
      end;
  finally
    FLock.UnLock;
  end;
end;

function TFDBLogger.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFDBLogEntry }

constructor TFDBLogEntry.create(AUsage : String);
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
  FExceptions := TStringList.create;
end;

destructor TFDBLogEntry.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogEntry.destroy';
begin
  FExceptions.free;
  inherited;
end;

procedure TFDBLogEntry.RecordUsage(AWhenStarted: TDateTime; ARowCount, APrepareCount : integer; AException: Exception; AErrMsg : string);
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogEntry.RecordUsage';
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

class function TFDBLogEntry.HTMLDoco : String;
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

class function TFDBLogEntry.Footer(AFormat: TFDBReportFormat): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogEntry.TFDBLogEntry';
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

class function TFDBLogEntry.Header(AFormat: TFDBReportFormat): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogEntry.Header';
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

function TFDBLogEntry.Report(AExceptions: Boolean; AFormat: TFDBReportFormat; APrefix : String = ''): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TFDBLogEntry.Report';
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

