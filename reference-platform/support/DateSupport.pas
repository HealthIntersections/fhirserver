Unit DateSupport;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

Interface

Uses
  SysUtils, SysConst, DateUtils, Math, System.TimeSpan,
  StringSupport, MathSupport;

Const
  DATETIME_MIN = -693593; // '1/01/0001'
  DATETIME_MAX = 2958465; // '31/12/9999'

  DATETIME_DAY_HOURS = 24;
  DATETIME_DAY_MINUTES = DATETIME_DAY_HOURS * 60;
  DATETIME_DAY_SECONDS = DATETIME_DAY_MINUTES * 60;
  DATETIME_DAY_MILLISECONDS = DATETIME_DAY_SECONDS * 1000;
  DATETIME_DAY_ONE = 1.0;
  DATETIME_HOUR_ONE = DATETIME_DAY_ONE / DATETIME_DAY_HOURS;
  DATETIME_MINUTE_ONE = DATETIME_DAY_ONE / DATETIME_DAY_MINUTES;
  DATETIME_SECOND_ONE = DATETIME_DAY_ONE / DATETIME_DAY_SECONDS;
  DATETIME_MILLISECOND_ONE = DATETIME_DAY_ONE / DATETIME_DAY_MILLISECONDS;

type
  TDuration = Int64;             // Number of milliseconds.
  TMonthOfYear =
    (MonthOfYearNone, MonthOfYearJanuary, MonthOfYearFebruary, MonthOfYearMarch, MonthOfYearApril, MonthOfYearMay, MonthOfYearJune,
     MonthOfYearJuly, MonthOfYearAugust, MonthOfYearSeptember, MonthOfYearOctober, MonthOfYearNovember, MonthOfYearDecember);
  TMonthDays = Array [TMonthOfYear] Of Word;
  TDateTimeExPrecision = (dtpYear, dtpMonth, dtpDay, dtpHour, dtpMin, dtpSec, dtpNanoSeconds);
  TDateTimeExTimezone = (dttzUnknown, dttzUTC, dttzLocal, dttzSpecified);

Const
  codes_TDateTimeExPrecision : Array [TDateTimeExPrecision] of String = ('Year', 'Month', 'Day', 'Hour', 'Min', 'Sec', 'NanoSeconds');
  codes_TDateTimeExTimezone : Array [TDateTimeExTimezone] of String = ('Unknown', 'UTC', 'Local', 'Specified');
  MONTHOFYEAR_LONG : Array[TMonthOfYear] Of String =
    ('', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
  MONTHOFYEAR_SHORT : Array [TMonthOfYear] Of String =
    ('', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  MONTHS_DAYS : Array [Boolean{IsLeapYear}] Of TMonthDays =
    ((0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));


type
  EDateFormatError = class(Exception);

  TTimeStamp = record
    year: Smallint;
    month: Word;
    day: Word;
    hour: Word;
    minute: Word;
    second: Word;
    fraction: Cardinal;
  end;

  TDateTimeEx = record
  private
    Source : String; // for debugging convenience, and also used to mark whether the record has any content
    year: Smallint;
    month: Word;
    day: Word;
    hour: Word;
    minute: Word;
    second: Word;
    fraction: Cardinal;
    {@member Precision
      The precision to which the date and time is specified
    }
    FPrecision : TDateTimeExPrecision;
    FractionPrecision : integer;
    {@member TimezoneType
      The type of timezone
    }

    TimezoneType : TDateTimeExTimezone;
    TimeZoneHours : Integer;
    TimezoneMins : Integer;
  private
    procedure clear;
    procedure check;
    function checkNoException : boolean;
    procedure RollUp;
    function privToString: String;
  public
    class function makeNull : TDateTimeEx; static;
    class function makeUTC : TDateTimeEx; overload; static;
    class function makeUTC(value : TDateTime) : TDateTimeEx; overload; static;
    class function makeToday : TDateTimeEx; overload; static;
    class function makeLocal : TDateTimeEx; overload; static;
    class function makeLocal(precision : TDateTimeExPrecision) : TDateTimeEx; overload; static;
    class function makeLocal(value : TDateTime) : TDateTimeEx; overload; static;
    class function make(value : TDateTime; tz : TDateTimeExTimezone) : TDateTimeEx; static;
    class function fromHL7(value : String) : TDateTimeEx; static;
    class function fromXML(value : String) : TDateTimeEx; static;
    class function fromTS(value : TTimestamp; tz : TDateTimeExTimezone = dttzLocal) : TDateTimeEx; overload; static;
    class function fromDB(value : String; tz : TDateTimeExTimezone = dttzUTC) : TDateTimeEx; static; // mainly for SQLite support
      {
         Read a date (date) given the specified format. The specified
         format can be any combination of YYYY, YYY, YY, MM, MMM, DD, HH, NN, SS.

         Use spaces for parts of the date that are just separators.
         This method can't cope with variable length date representations such as full month names.
         If the year is < 100, it will be adjusted to a current year (irrespective of the year length YYYY or YY). See ReadDateStrict
      }
    class function fromFormat(format, date: String; AllowBlankTimes: Boolean = False; allowNoDay: Boolean = False; allownodate: Boolean = False; noFixYear : boolean = false) : TDateTimeEx; static;

      {
        Like ReadDate, but years < 100 will not be corrected as if they are 2 digit year representations
      }
    class function fromFormatStrict(format, date: String; AllowBlankTimes: Boolean = False; allowNoDay: Boolean = False; allownodate: Boolean = False) : TDateTimeEx; static;

    function null : boolean;
    function notNull : boolean;

    function DateTime : TDateTime;
    function TimeStamp : TTimeStamp;

    function fixPrecision(FPrecision : TDateTimeExPrecision) : TDateTimeEx;

    function Local : TDateTimeEx;
    function UTC : TDateTimeEx;
    function Min : TDateTimeEx;
    function Max : TDateTimeEx;
    function AsTz(hr, min : Integer):TDateTimeEx; // this date and time in the specified timezone.

    function IncrementMonth: TDateTimeEx;
    function IncrementYear: TDateTimeEx;
    function IncrementWeek: TDateTimeEx;
    function IncrementDay: TDateTimeEx;
    function add(length : TDateTime) : TDateTimeEx;
    function subtract(length : TDateTime) : TDateTimeEx;
    function lessPrecision: TDateTimeEx;

    function equal(other : TDateTimeEx) : Boolean;  // returns true if the timezone, FPrecision, and actual instant are the same
    function sameTime(other : TDateTimeEx) : Boolean; // returns true if the specified instant is the same allowing for specified FPrecision - corrects for timezone
    function after(other : TDateTimeEx; inclusive : boolean):boolean;
    function before(other : TDateTimeEx; inclusive : boolean):boolean;
    function between(min, max : TDateTimeEx; inclusive : boolean):boolean;
    function compare(other : TDateTimeEx) : integer;

    {@
    Valid formatting strings are

    yyyy    4 digit year
    yy      2 digit year
    mmmm    long month name
    mmm     short month name
    mm      2 digit month number (will include leading 0 if needed)
    m       month number (no leading 0)
    dd      2 digit day number (will include leading 0 if needed)
    d       day number (no leading 0)
    hh      2 digit hour number
    nn      2 digit minutes
    ss      2 digit seconds
    }
    function toString(format: String): String; overload;
    function toString: String;  overload; // as human readable
    function toHL7: String; // as yyyymmddhhnnss.zzz+T
    function toXML : String;
    function toDB : String; // as yyyy-mm-dd hh:nn:ss.zzz

    class function isValidXmlDate(value : String) : Boolean; static;

    property Precision : TDateTimeExPrecision read FPrecision;
  end;


Function TimeZoneBias : TDateTime; Overload;
Function TimeZoneBias(when : TDateTime) : TDateTime; Overload;
function TSToDateTime(TS: TTimeStamp): TDateTime;
function DateTimeToTS(Value : TDateTime): TTimeStamp;
function SameInstant(t1, t2 : TDateTime) : boolean;
Function DateTimeMax(Const aA, aB : TDateTime) : TDateTime; Overload;
Function DateTimeMin(Const aA, aB : TDateTime) : TDateTime; Overload;
Function DateTimeCompare(Const aA, aB : TDateTime) : Integer; Overload;
Function DateTimeCompare(Const aA, aB, aThreshold : TDateTime) : Integer; Overload;
function DescribePeriod(Period: TDateTime): String;
Function DateTimeToXMLDateTimeTimeZoneString(Const aTimestamp, aTimeZone : TDateTime) : String;

Implementation

uses
  GuidSupport,
  EncodeSupport;


{ TDateTimeEx }

function TDateTimeEx.add(length: TDateTime): TDateTimeEx;
begin
  result := makeUTC(dateTime + length);
  result.FPrecision := FPrecision;
  result.FractionPrecision := FractionPrecision;
  result.TimezoneType := TimezoneType;
  result.TimeZoneHours := TimeZoneHours;
  result.TimezoneMins := TimezoneMins;
end;

procedure TDateTimeEx.check;
var
  err : String;
begin
  err := '';
  if (year < 1000) or (year > 3000) then
    err := 'Year is not valid'
  else if (FPrecision >= dtpMonth) and ((Month > 12) or (Month < 1)) then
    err := 'Month is not valid'
  else if (FPrecision >= dtpDay) and ((Day < 1) or (Day >= 32) or (MONTHS_DAYS[IsLeapYear(Year)][TMonthOfYear(Month)] < Day)) then
    err := 'Day is not valid for '+inttostr(Year)+'/'+inttostr(Month)
  else if (FPrecision >= dtpHour) and (Hour > 23) then
    err := 'Hour is not valid'
  else if (FPrecision >= dtpMin) and (Minute > 59) then
    err := 'Minute is not valid'
  else if (FPrecision >= dtpSec) and (Second > 59) then
    err := 'Second is not valid'
  else if (FPrecision >= dtpNanoSeconds) and (FractionPrecision > 999999999) then
    err := 'Fraction is not valid'
  else if (TimezoneType = dttzSpecified) and ((TimezoneHours < -13) or (TimezoneHours > 14)) then
    err := 'Timezone hours is not valid'
  else if (TimezoneType = dttzSpecified) and not ((TimezoneMins = 0) or (TimezoneMins = 15) or (TimezoneMins = 30) or (TimezoneMins = 45)) then
    err := 'Timezone minutes is not valid';
  if err <> '' then
    raise EDateFormatError.Create(err+' ('+privToString+')');
end;

function TDateTimeEx.checkNoException: boolean;
var
  err : String;
begin
  err := '';
  if (year < 1000) or (year > 3000) then
    err := 'Year is not valid'
  else if (FPrecision >= dtpMonth) and ((Month > 12) or (Month < 1)) then
    err := 'Month is not valid'
  else if (FPrecision >= dtpDay) and ((Day < 1) or (Day >= 32) or (MONTHS_DAYS[IsLeapYear(Year)][TMonthOfYear(Month)] < Day)) then
    err := 'Day is not valid for '+inttostr(Year)+'/'+inttostr(Month)
  else if (FPrecision >= dtpHour) and (Hour > 23) then
    err := 'Hour is not valid'
  else if (FPrecision >= dtpMin) and (Minute > 59) then
    err := 'Minute is not valid'
  else if (FPrecision >= dtpSec) and (Second > 59) then
    err := 'Second is not valid'
  else if (FPrecision >= dtpNanoSeconds) and (FractionPrecision > 999999999) then
    err := 'Fraction is not valid'
  else if (TimezoneType = dttzSpecified) and ((TimezoneHours < -13) or (TimezoneHours > 14)) then
    err := 'Timezone hours is not valid'
  else if (TimezoneType = dttzSpecified) and not ((TimezoneMins = 0) or (TimezoneMins = 15) or (TimezoneMins = 30) or (TimezoneMins = 45)) then
    err := 'Timezone minutes is not valid';
  result := err = '';
end;

procedure TDateTimeEx.clear;
begin
  Source := '';
  year := 0;
  month := 0;
  day := 0;
  hour := 0;
  minute := 0;
  second := 0;
  fraction := 0;
  FPrecision := dtpYear;
  FractionPrecision := 0;
  TimezoneType := dttzUnknown;
  TimeZoneHours := 0;
  TimezoneMins := 0;
end;

function TDateTimeEx.compare(other: TDateTimeEx): integer;
begin
  if null or other.null then
    result := 0
  else if after(other, false) then
    result := 1
  else if other.after(self, false) then
    result := -1
  else
    result := 0;
end;

function TDateTimeEx.DateTime: TDateTime;
begin
  check;
  case FPrecision of
    dtpYear : Result := EncodeDate(Year, 1, 1);
    dtpMonth : Result := EncodeDate(Year, Month, 1);
    dtpDay: Result := EncodeDate(Year, Month, Day);
    dtpHour : Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, 0, 0, 0);
    dtpMin : Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, 0, 0);
    dtpSec : Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
    dtpNanoSeconds : Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, Fraction div 1000000);
  else
    Result := 0;
  end;
end;

procedure FindBlock(ch: Char; const s: String; var start, blength: Integer);
begin
  start := pos(ch, s);
  if start = 0 then
    blength := 0
  else
    begin
    blength := 1;
    while (start + blength <= length(s)) and (s[start + blength] = ch) do
      inc(blength);
    end;
end;

function UserStrToInt(st, Info: String): Integer; { raise an EHL7UserException }
var
  E: Integer;
begin
  Val(St, Result, E);
  if E <> 0 then
    raise EDateFormatError.CreateFmt(SInvalidInteger + ' reading ' + Info, [St]);
end;

class function TDateTimeEx.make(value: TDateTime; tz: TDateTimeExTimezone): TDateTimeEx;
var
  ms: Word;
  yr: Word;
begin
  result.clear;
  DecodeTime(value, result.Hour, result.Minute, result.Second, ms);
  result.Fraction := ms * 1000000;
  if result.second > 59 then
    raise Exception.Create('Fail!');
  DecodeDate(value, yr, result.Month, result.Day);
  result.Year := yr;
  result.FPrecision := dtpSec;
  result.FractionPrecision := 0;
  result.TimezoneType := tz;
  result.Source := 'makeDT';
end;

class function TDateTimeEx.makeLocal(precision: TDateTimeExPrecision): TDateTimeEx;
begin
  result := TDateTimeEx.makeLocal(now).fixPrecision(precision);
end;

function checkFormat(format : String) : string;
begin
  if (format = 'c') then
    result := FormatSettings.shortDateFormat+' '+FormatSettings.ShortTimeFormat
  else if (format = 'cd') then
    result := FormatSettings.shortDateFormat
  else if (format = 'ct') then
    result := FormatSettings.ShortTimeFormat
  else if (format = 'C') then
    result := FormatSettings.longDateFormat+' '+FormatSettings.LongTimeFormat
  else if (format = 'CD') then
    result := FormatSettings.LongDateFormat
  else if (format = 'CC') then
    result := FormatSettings.LongTimeFormat
  else if (format = 'x') then
    result := 'yyyy-dd-mmThh:nn:ss'
  else
    result := format;
end;


class function TDateTimeEx.fromDB(value: String; tz : TDateTimeExTimezone = dttzUTC): TDateTimeEx;
begin
  result := fromFormat('yyyy-mm-dd hh:nn:ss.sss', value, true, true, false);
  result.TimezoneType := tz;
end;

class function TDateTimeEx.fromFormat(format, date: String; AllowBlankTimes: Boolean = False; allowNoDay: Boolean = False; allownodate: Boolean = False; noFixYear : boolean = false) : TDateTimeEx;
var
  start, length: Integer;
  s: String;
  tmp: String;
begin
  result.clear;
  format := checkFormat(format);
  Result.year := 0;
  Result.month := 0;
  Result.day := 0;
  Result.hour := 0;
  Result.minute := 0;
  Result.second := 0;
  Result.fraction := 0;
  FindBlock('y', Format, start, length);
  tmp := copy(date, start, length);
  if lowercase(tmp) = 'nown' then
    exit;
  if (tmp = '') and AllowNoDate then
    // we don't bother with the year
  else
    begin
    Result.year := UserStrToInt(tmp, 'Year from "' + date + '"');
    if not NoFixYear then
    begin
      if Result.year < 100 then
        if abs(Result.year) > {$IFNDEF VER130}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow then      //abs as result.year is a smallint, twodigityearcenturywindow is a word (range checking)
          inc(Result.year, 1900)
        else
          inc(Result.year, 2000);
    end;
    end;
  FindBlock('m', Format, start, length);
  s := lowercase(copy(date, start, length));
  if AllowNoDate and (tmp = '') then
    // we don't worry about the month
  else
    begin
    if length > 2 then
      begin
      if (s = 'jan') or (s = 'january') then
        Result.month := 1
      else if (s = 'feb') or (s = 'february') then
        Result.month := 2
      else if (s = 'mar') or (s = 'march') then
        Result.month := 3
      else if (s = 'apr') or (s = 'april') then
        Result.month := 4
      else if (s = 'may') then
        Result.month := 5
      else if (s = 'jun') or (s = 'june') then
        Result.month := 6
      else if (s = 'jul') or (s = 'july') then
        Result.month := 7
      else if (s = 'aug') or (s = 'august') then
        Result.month := 8
      else if (s = 'sep') or (s = 'september') then
        Result.month := 9
      else if (s = 'oct') or (s = 'october') then
        Result.month := 10
      else if (s = 'nov') or (s = 'november') then
        Result.month := 11
      else if (s = 'dec') or (s = 'december') then
        Result.month := 12
      else
        raise EDateFormatError.Create('The Month "' + s + '" is unknown');
      end
    else if s = '' then
      Result.Month := 1
    else
      Result.month := UserStrToInt(s, 'Month from "' + date + '"');
    if (Result.month > 12) or (Result.month < 1) then
      raise EDateFormatError.Create('invalid month ' + IntToStr(Result.month));
    end;
  FindBlock('d', Format, start, length);
  tmp := copy(date, start, length);
  if (AllowNoday or AllowNoDate) and (tmp = '') then
    // we don't check the day
  else
    begin
    Result.day := UserStrToInt(tmp, 'Day from "' + date + '"');
    end;
  FindBlock('h', Format, start, length);
  if length <> 0 then
    if AllowBlankTimes then
      Result.hour := StrToIntDef(copy(date, start, length), 0)
    else
      Result.hour := UserStrToInt(copy(date, start, length), 'Hour from "' + date + '"');
  FindBlock('s', Format, start, length);
  if length <> 0 then
    if AllowBlankTimes then
      Result.second := StrToIntDef(copy(date, start, length), 0)
    else
      Result.second := UserStrToInt(copy(date, start, length), 'Second from "' + date + '"');
  FindBlock('n', Format, start, length);
  if length <> 0 then
    if AllowBlankTimes then
      Result.minute := StrToIntDef(copy(date, start, length), 0)
    else
      Result.minute := UserStrToInt(copy(date, start, length), 'Minute from "' + date + '"');
  FindBlock('z', Format, start, length);
  if length <> 0 then
    if AllowBlankTimes then
      Result.fraction := StrToIntDef(copy(date, start, length), 0);
  FindBlock('x', Format, start, length);
  if length <> 0 then
    if uppercase(copy(date, start, length)) = 'AM' then
      begin
      if Result.hour = 12 then
        Result.hour := 0
      end
    else
      inc(Result.hour, 12);

  if Result.hour = 24 then
  begin
    Result.hour := 0;
    inc(result.day);
  end;
  result.RollUp;

  result.check;
  if (result.Hour = 0) and (result.Minute = 0) and (result.Second = 0) then
    result.FPrecision := dtpDay
  else
    result.FPrecision := dtpSec;
  result.FractionPrecision := 0;
  result.TimezoneType := dttzLocal;
  result.Source := date;
end;

class function TDateTimeEx.fromFormatStrict(format, date: String; AllowBlankTimes: Boolean = False; allowNoDay: Boolean = False; allownodate: Boolean = False) : TDateTimeEx;
begin
  result := fromFormat(format, date, AllowBlankTimes, allowNoDay, allownodate, true);
end;

function TDateTimeEx.IncrementMonth : TDateTimeEx;
begin
  result := self;
  if result.month = 12 then
    begin
    inc(result.year);
    result.month := 1
    end
  else
    inc(result.month);
  if result.day > MONTHS_DAYS[IsLeapYear(result.Year), TMonthOfYear(result.month)] then
    result.Day := MONTHS_DAYS[IsLeapYear(result.Year), TMonthOfYear(result.month)];
end;

function TDateTimeEx.IncrementYear : TDateTimeEx;
begin
  result := self;
  inc(result.year);
end;

function vsv(value : String; start, len, min, max : Integer; name : String):Integer;
var
  v : String;
begin
  v := copy(value, start, len);
  if not StringIsInteger16(v) then
    exit(-1);
  result := StrToInt(v);
  if (result < min) or (result > max) then
    exit(-1);
end;

class function TDateTimeEx.isValidXmlDate(value: String): Boolean;
var
  s : String;
  neg : boolean;
  res : TDateTimeEx;
begin
  if value = '' then
    exit(false);

  res.Source := Value;
  if pos('Z', value) = length(value) then
  begin
    res.TimezoneType := dttzUTC;
    Delete(value, length(value), 1);
  end
  else if (pos('T', value) > 0) and StringContainsAny(copy(Value, pos('T', value)+1, $FF), ['-', '+']) then
  begin
    neg := Pos('-', copy(Value, pos('T', value)+1, $FF)) > 0;
    StringSplitRight(value, ['-', '+'], value, s);
    if length(s) <> 5 then
      raise Exception.create('Unable to parse date/time "'+value+'": timezone is illegal length - must be 5');
    res.TimezoneHours := vsv(s, 1, 2, 0, 14, 'timezone hours');
    res.TimezoneMins := vsv(s, 4, 2, 0, 59, 'timezone minutes');
    res.TimezoneType := dttzSpecified;
    if neg then
      res.TimezoneHours := -res.TimezoneHours;
  end;

  res.FractionPrecision := 0;

  if Length(value) >=4 then
    res.Year := vsv(Value, 1, 4, 1800, 2100, 'years');
  if Length(value) < 7 then
    res.FPrecision := dtpYear
  else
  begin
    res.Month := vsv(Value, 6, 2, 1, 12, 'months');
    if length(Value) < 10 then
      res.FPrecision := dtpMonth
    else
    begin
      res.Day := vsv(Value, 9, 2, 1, 31, 'days');
      if length(Value) < 13 then
        res.FPrecision := dtpDay
      else
      begin
        res.Hour := vsv(Value, 12, 2, 0, 23, 'hours');
        if length(Value) < 15 then
          res.FPrecision := dtpHour
        else
        begin
          res.Minute := vsv(Value, 15, 2, 0, 59, 'minutes');
          if length(Value) < 18 then
            res.FPrecision := dtpMin
          else
          begin
            res.Second := vsv(Value, 18, 2, 0, 59, 'seconds');
            if length(Value) <= 20 then
              res.FPrecision := dtpSec
            else
            begin
              s := copy(Value, 21, 6);
              res.FractionPrecision := length(s);
              res.fraction := trunc(vsv(Value, 21, 4, 0, 999999, 'fractions') * power(10, 9 - res.FractionPrecision));
              res.FPrecision := dtpNanoSeconds;
            end;
          end;
        end;
      end;
    end;
  end;
  result := res.checkNoException;
end;

function TDateTimeEx.lessPrecision: TDateTimeEx;
begin
  result := self;
  if result.FPrecision > dtpYear then
    result.FPrecision := pred(result.FPrecision);
  result.RollUp;
end;

function TDateTimeEx.IncrementWeek : TDateTimeEx;
var
  i: Integer;
begin
  result := self;
  for i := 1 to 7 do
  begin
    inc(result.day);
    result.rollUp;
  end;
end;

function TDateTimeEx.IncrementDay : TDateTimeEx;
begin
  result := self;
  inc(result.day);
  result.RollUp;
end;

function ReplaceSubString(var AStr: String; const ASearchStr, AReplaceStr: String): Boolean;
var
  sStr : String;
begin
  sStr := StringReplace(aStr, ASearchStr, AReplaceStr, [rfReplaceAll, rfIgnoreCase]);
  result := aStr <> sStr;
  aStr := sStr;
end;

function TDateTimeEx.ToString(format: String): String;
begin
  if Source = '' then
    exit('');
  check;
  format := checkFormat(format);
  Result := format;
  if not ReplaceSubString(Result, 'yyyy', StringPadRight(IntToStr(year), '0', 4)) then
    replaceSubstring(Result, 'yy', copy(IntToStr(year), 3, 2));
  if not ReplaceSubString(Result, 'mmmm', copy(MONTHOFYEAR_LONG[TMonthOfYear(month)], 1, 4)) then
    if not ReplaceSubString(Result, 'mmm', MONTHOFYEAR_SHORT[TMonthOfYear(month)]) then
      if not ReplaceSubString(Result, 'mm', StringPadLeft(IntToStr(month), '0', 2)) then
        ReplaceSubString(Result, 'm', IntToStr(month));
  if not ReplaceSubString(Result, 'dd', StringPadLeft(IntToStr(day), '0', 2)) then
    ReplaceSubString(Result, 'd', IntToStr(day));
  ReplaceSubString(Result, 'HH', StringPadLeft(IntToStr(hour), '0', 2));
  ReplaceSubString(Result, 'H', IntToStr(hour));
  ReplaceSubString(Result, 'hh', StringPadLeft(IntToStr(hour mod 12), '0', 2));
  ReplaceSubString(Result, 'h', IntToStr(hour mod 12));
  ReplaceSubString(Result, 'nn', StringPadLeft(IntToStr(minute), '0', 2));
  ReplaceSubString(Result, 'ss', StringPadLeft(IntToStr(second), '0', 2));
  if hour < 12 then
    ReplaceSubString(Result, 'AMPM', 'AM')
  else
    ReplaceSubString(Result, 'AMPM', 'PM');
end;

function vs(value : String; start, len, min, max : Integer; name : String):Integer;
var
  v : String;
begin
  v := copy(value, start, len);
  if not StringIsInteger16(v) then
    raise exception.create('Unable to parse date/time "'+value+'" at '+name);
  result := StrToInt(v);
  if (result < min) or (result > max) then
    raise exception.create('Value for '+name+' in date/time "'+value+'" is not allowed');
end;


class function TDateTimeEx.fromHL7(value: String) : TDateTimeEx;
var
  s : String;
  neg : boolean;
begin
  if value = '' then
    exit(makeNull);

  result.clear;
  result.Source := Value;

  if pos('Z', value) = length(value) then
  begin
    result.TimezoneType := dttzUTC;
    Delete(value, length(value), 1);
  end
  else if StringContainsAny(Value, ['-', '+']) then
  begin
    neg := Pos('-', Value) > 0;
    StringSplit(value, ['-', '+'], value, s);
    if length(s) <> 4 then
      raise Exception.create('Unable to parse date/time "'+value+'": timezone is illegal length - must be 4');
    result.TimezoneHours := vs(s, 1, 2, 0, 13, 'timezone hours');
    result.TimezoneMins := vs(s, 3, 2, 0, 59, 'timezone minutes');
    result.TimezoneType := dttzSpecified;
    if neg then
      result.TimezoneHours := -result.TimezoneHours;
  end;

  result.FractionPrecision := 0;

  if Length(value) >=4 then
    result.Year := vs(Value, 1, 4, 1800, 2100, 'years');
  if Length(value) < 6 then
    result.FPrecision := dtpYear
  else
  begin
    result.Month := vs(Value, 5, 2, 1, 12, 'months');
    if length(Value) < 8 then
      result.FPrecision := dtpMonth
    else
    begin
      result.Day := vs(Value, 7, 2, 1, 31, 'days');
      if length(Value) < 10 then
        result.FPrecision := dtpDay
      else
      begin
        result.Hour := vs(Value, 9, 2, 0, 23, 'hours');
        if length(Value) < 12 then
          result.FPrecision := dtpHour
        else
        begin
          result.Minute := vs(Value, 11, 2, 0, 59, 'minutes');
          if length(Value) < 14 then
            result.FPrecision := dtpMin
          else
          begin
            result.Second := vs(Value, 13, 2, 0, 59, 'seconds');
            if length(Value) <= 15 then
              result.FPrecision := dtpSec
            else
            begin
              s := copy(Value, 16, 4);
              result.FractionPrecision := length(s);
              result.fraction := trunc(vs(Value, 16, 4, 0, 9999, 'fractions') * power(10, 9 - result.FractionPrecision));
              result.FPrecision := dtpNanoSeconds;
            end;
          end;
        end;
      end;
    end;
  end;
  result.check;
  result.source := value;
end;

class function TDateTimeEx.fromXML(value: String) : TDateTimeEx;
var
  s : String;
  neg : boolean;
begin
  if value = '' then
    exit(makeNull);

  result.clear;
  result.Source := Value;
  if pos('Z', value) = length(value) then
  begin
    result.TimezoneType := dttzUTC;
    Delete(value, length(value), 1);
  end
  else if (pos('T', value) > 0) and StringContainsAny(copy(Value, pos('T', value)+1, $FF), ['-', '+']) then
  begin
    neg := Pos('-', copy(Value, pos('T', value)+1, $FF)) > 0;
    StringSplitRight(value, ['-', '+'], value, s);
    if length(s) <> 5 then
      raise Exception.create('Unable to parse date/time "'+value+'": timezone is illegal length - must be 5');
    result.TimezoneHours := vs(s, 1, 2, 0, 14, 'timezone hours');
    result.TimezoneMins := vs(s, 4, 2, 0, 59, 'timezone minutes');
    result.TimezoneType := dttzSpecified;
    if neg then
      result.TimezoneHours := -result.TimezoneHours;
  end;

  result.FractionPrecision := 0;

  if Length(value) >=4 then
    result.Year := vs(Value, 1, 4, 1800, 2100, 'years');
  if Length(value) < 7 then
    result.FPrecision := dtpYear
  else
  begin
    result.Month := vs(Value, 6, 2, 1, 12, 'months');
    if length(Value) < 10 then
      result.FPrecision := dtpMonth
    else
    begin
      result.Day := vs(Value, 9, 2, 1, 31, 'days');
      if length(Value) < 13 then
        result.FPrecision := dtpDay
      else
      begin
        result.Hour := vs(Value, 12, 2, 0, 23, 'hours');
        if length(Value) < 15 then
          result.FPrecision := dtpHour
        else
        begin
          result.Minute := vs(Value, 15, 2, 0, 59, 'minutes');
          if length(Value) < 18 then
            result.FPrecision := dtpMin
          else
          begin
            result.Second := vs(Value, 18, 2, 0, 59, 'seconds');
            if length(Value) <= 20 then
              result.FPrecision := dtpSec
            else
            begin
              s := copy(Value, 21, 6);
              result.FractionPrecision := length(s);
              result.fraction := trunc(vs(Value, 21, 4, 0, 999999, 'fractions') * power(10, 9 - result.FractionPrecision));
              result.FPrecision := dtpNanoSeconds;
            end;
          end;
        end;
      end;
    end;
  end;
  result.check;
  result.source := value;
end;

Function sv(i, w : integer):String;
begin
  result := StringPadLeft(inttostr(abs(i)), '0', w);
  if i < 0 then
    result := '-'+result;
end;

function TDateTimeEx.toDB: String;
begin
  case FPrecision of
    dtpYear:        result := sv(Year, 4);
    dtpMonth:       result := sv(Year, 4) + '-'+sv(Month, 2);
    dtpDay:         result := sv(Year, 4) + '-'+sv(Month, 2) + '-'+sv(Day, 2);
    dtpHour:        result := sv(Year, 4) + '-'+sv(Month, 2) + '-'+sv(Day, 2) + ' '+sv(hour, 2);
    dtpMin:         result := sv(Year, 4) + '-'+sv(Month, 2) + '-'+sv(Day, 2) + ' '+sv(hour, 2)+ ':'+sv(Minute, 2);
    dtpSec:         result := sv(Year, 4) + '-'+sv(Month, 2) + '-'+sv(Day, 2) + ' '+sv(hour, 2)+ ':'+sv(Minute, 2)+ ':'+sv(Second, 2);
    dtpNanoSeconds: result := sv(Year, 4) + '-'+sv(Month, 2) + '-'+sv(Day, 2) + ' '+sv(hour, 2)+ ':'+sv(Minute, 2)+ ':'+sv(Second, 2)+'.'+copy(sv(Fraction, 9), 1, IntegerMax(FractionPrecision, 3));
  end;
end;

function TDateTimeEx.toHL7: String;
begin
  if null then
    exit('');
  case FPrecision of
    dtpYear:  result := sv(Year, 4);
    dtpMonth: result := sv(Year, 4) + sv(Month, 2);
    dtpDay:   result := sv(Year, 4) + sv(Month, 2) + sv(Day, 2);
    dtpHour:  result := sv(Year, 4) + sv(Month, 2) + sv(Day, 2) + sv(hour, 2);
    dtpMin:   result := sv(Year, 4) + sv(Month, 2) + sv(Day, 2) + sv(hour, 2)+ sv(Minute, 2);
    dtpSec:   result := sv(Year, 4) + sv(Month, 2) + sv(Day, 2) + sv(hour, 2)+ sv(Minute, 2)+ sv(Second, 2);
    dtpNanoSeconds: result := sv(Year, 4) + sv(Month, 2) + sv(Day, 2) + sv(hour, 2)+ sv(Minute, 2)+ sv(Second, 2)+'.'+copy(sv(Fraction, 9), 1, IntegerMax(FractionPrecision, 3));
  end;
  case TimezoneType of
    dttzUTC : result := result + 'Z';
    dttzSpecified :
      if TimezoneHours < 0 then
        result := result + sv(TimezoneHours, 2) + sv(TimezoneMins, 2)
      else
        result := result + '+'+sv(TimezoneHours, 2) + sv(TimezoneMins, 2);
    dttzLocal :
      if TimeZoneBias > 0 then
        result := result + '+'+FormatDateTime('hhnn', TimeZoneBias, FormatSettings)
      else
        result := result + '-'+FormatDateTime('hhnn', -TimeZoneBias, FormatSettings);
  {else
    dttzUnknown - do nothing }
  end;
end;

function TDateTimeEx.toXml: String;
begin
  if null then
    exit('');

  case FPrecision of
    dtpYear:  result := sv(Year, 4);
    dtpMonth: result := sv(Year, 4) + '-' + sv(Month, 2);
    dtpDay:   result := sv(Year, 4) + '-' + sv(Month, 2) + '-' + sv(Day, 2);
    dtpHour:  result := sv(Year, 4) + '-' + sv(Month, 2) + '-' + sv(Day, 2) + 'T' + sv(hour, 2) + ':' + sv(Minute, 2); // note minutes anyway in this case
    dtpMin:   result := sv(Year, 4) + '-' + sv(Month, 2) + '-' + sv(Day, 2) + 'T' + sv(hour, 2) + ':' + sv(Minute, 2);
    dtpSec:   result := sv(Year, 4) + '-' + sv(Month, 2) + '-' + sv(Day, 2) + 'T' + sv(hour, 2) + ':' + sv(Minute, 2)+ ':' + sv(Second, 2);
    dtpNanoSeconds: result := sv(Year, 4) + '-' + sv(Month, 2) + '-' + sv(Day, 2) + 'T' + sv(hour, 2) + ':' + sv(Minute, 2)+ ':' + sv(Second, 2)+'.'+copy(sv(Fraction, 9), 1, FractionPrecision);
  end;
  if (FPrecision > dtpDay) then
    case TimezoneType of
      dttzUTC : result := result + 'Z';
      dttzSpecified :
        if TimezoneHours < 0 then
          result := result + sv(TimezoneHours, 2) + ':'+sv(TimezoneMins, 2)
        else
          result := result + '+'+sv(TimezoneHours, 2) + ':'+sv(TimezoneMins, 2);
      dttzLocal :
        if TimeZoneBias > 0 then
          result := result + '+'+FormatDateTime('hh:nn', TimeZoneBias, FormatSettings)
        else
          result := result + '-'+FormatDateTime('hh:nn', -TimeZoneBias, FormatSettings);
    {else
      dttzUnknown - do nothing }
    end;
end;

function TDateTimeEx.Local: TDateTimeEx;
var
  bias : TDateTime;
begin
  if FPrecision >= dtpHour then
    case TimezoneType of
      dttzUTC : result := TDateTimeEx.makeLocal(TTimeZone.Local.ToLocalTime(self.DateTime));
      dttzSpecified :
        begin
        if TimezoneHours < 0 then
          bias := - (-TimezoneHours * DATETIME_HOUR_ONE) + (TimezoneMins * DATETIME_MINUTE_ONE)
        else
          bias := (TimezoneHours * DATETIME_HOUR_ONE) + (TimezoneMins * DATETIME_MINUTE_ONE);
        result := TDateTimeEx.makeLocal(TTimeZone.Local.ToLocalTime(self.DateTime-bias));
        end
    else
      result := self;
    end;
  result.FPrecision := FPrecision;
  result.FractionPrecision := FractionPrecision;
  result.TimezoneType := dttzLocal;
end;

function TDateTimeEx.Max: TDateTimeEx;
begin
  result := self;
  case FPrecision of
    dtpYear:
      begin
      inc(result.year);
      result.Month := 1;
      result.Day := 1;
      result.Hour := 0;
      result.minute := 0;
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpMonth:
      begin
      inc(result.Month);
      result.Day := 1;
      result.Hour := 0;
      result.minute := 0;
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpDay:
      begin
      inc(result.Day);
      result.Hour := 0;
      result.minute := 0;
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpHour:
      begin
      inc(result.Hour);
      result.minute := 0;
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpMin:
      begin
      inc(result.minute);
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpSec:
      begin
      inc(result.Second);
      result.fraction := 0;
      end;
    dtpNanoSeconds:
      begin
      inc(result.fraction);
      end;
  end;
  result.RollUp;
  result.FPrecision := dtpNanoSeconds;
  result.check;
end;

function TDateTimeEx.Min: TDateTimeEx;
begin
  result := self;
  case FPrecision of
    dtpYear:
      begin
      result.Month := 1;
      result.Day := 1;
      result.Hour := 0;
      result.minute := 0;
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpMonth:
      begin
      result.Day := 1;
      result.Hour := 0;
      result.minute := 0;
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpDay:
      begin
      result.Hour := 0;
      result.minute := 0;
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpHour:
      begin
      result.minute := 0;
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpMin:
      begin
      result.Second := 0;
      result.fraction := 0;
      end;
    dtpSec:
      begin
      result.fraction := 0;
      end;
    dtpNanoSeconds:
      begin
      end;
  end;
  result.FPrecision := dtpNanoSeconds;
end;

function TDateTimeEx.notNull: boolean;
begin
  result := Source <> '';
end;

function TDateTimeEx.null: boolean;
begin
  result := Source = '';
end;

function TDateTimeEx.privToString: String;
begin
  Result := 'yyyymmddhhnnss';
  if not ReplaceSubString(Result, 'yyyy', StringPadRight(IntToStr(year), '0', 4)) then
    replaceSubstring(Result, 'yy', copy(IntToStr(year), 3, 2));
  if month <> 0 then
  begin
    if not ReplaceSubString(Result, 'mmmm', copy(MONTHOFYEAR_LONG[TMonthOfYear(month)], 1, 4)) then
      if not ReplaceSubString(Result, 'mmm', MONTHOFYEAR_SHORT[TMonthOfYear(month)]) then
        if not ReplaceSubString(Result, 'mm', StringPadLeft(IntToStr(month), '0', 2)) then
          ReplaceSubString(Result, 'm', IntToStr(month));
    if day > 0 then
    begin
      if not ReplaceSubString(Result, 'dd', StringPadLeft(IntToStr(day), '0', 2)) then
        ReplaceSubString(Result, 'd', IntToStr(day));
      ReplaceSubString(Result, 'hh', StringPadLeft(IntToStr(hour), '0', 2));
      ReplaceSubString(Result, 'nn', StringPadLeft(IntToStr(minute), '0', 2));
      ReplaceSubString(Result, 'ss', StringPadLeft(IntToStr(second), '0', 2));
    end;
  end;
end;

function TDateTimeEx.AsTz(hr, min: Integer): TDateTimeEx;
var
  bias : TDateTime;
  nbias : TDateTime;
begin
  result := self;
  if hr < 0 then
    nbias := - (-Hr * DATETIME_HOUR_ONE) + (min * DATETIME_MINUTE_ONE)
  else
    nbias := (Hr * DATETIME_HOUR_ONE) + (min * DATETIME_MINUTE_ONE);

  bias := timezoneBias;
  result.TimezoneType := dttzLocal;
  if FPrecision >= dtpHour then
    case TimezoneType of
      dttzUTC : result := TDateTimeEx.makeLocal(self.DateTime+nbias);
      dttzLocal : result := TDateTimeEx.makeLocal(TTimeZone.Local.ToUniversalTime(self.DateTime-bias)+nbias);
      dttzSpecified :
        begin
        if TimezoneHours < 0 then
          bias := - (-TimezoneHours * DATETIME_HOUR_ONE) + (TimezoneMins * DATETIME_MINUTE_ONE)
        else
          bias := (TimezoneHours * DATETIME_HOUR_ONE) + (TimezoneMins * DATETIME_MINUTE_ONE);
        result := TDateTimeEx.makeLocal(self.DateTime-bias+nbias);
        end;
    else
      result := self;
    end;
  result.FPrecision := FPrecision;
  result.FractionPrecision := FractionPrecision;
  result.TimezoneType := dttzSpecified;
  result.TimezoneHours := hr;
  result.TimezoneMins := min;
end;

function TDateTimeEx.UTC: TDateTimeEx;
var
  bias : TDateTime;
begin
  result := self;
  if FPrecision >= dtpHour then
    case TimezoneType of
      dttzLocal : result := TDateTimeEx.makeUTC(TTimeZone.Local.ToUniversalTime(self.DateTime));
      dttzSpecified :
        begin
        if TimezoneHours < 0 then
          bias := - (-TimezoneHours * DATETIME_HOUR_ONE) + (TimezoneMins * DATETIME_MINUTE_ONE)
        else
          bias := (TimezoneHours * DATETIME_HOUR_ONE) + (TimezoneMins * DATETIME_MINUTE_ONE);
        result := TDateTimeEx.makeUTC(self.DateTime+bias);
        end;
    else
    end;
  result.FPrecision := FPrecision;
  result.FractionPrecision := FractionPrecision;
  result.TimezoneType := dttzUTC;
end;

class function TDateTimeEx.makeUTC : TDateTimeEx;
begin
  result := TDateTimeEx.makeUTC(TTimeZone.Local.ToUniversalTime(now));
end;

class function TDateTimeEx.makeUTC(value: TDateTime) : TDateTimeEx;
begin
  result := make(value, dttzUTC);
end;


class function TDateTimeEx.makeLocal : TDateTimeEx;
begin
  result := TDateTimeEx.makeLocal(now);
end;

class function TDateTimeEx.makeLocal(value: TDateTime) : TDateTimeEx;
begin
  result := make(value, dttzLocal);
end;

class function TDateTimeEx.makeNull: TDateTimeEx;
begin
  result.clear;
  result.Source := '';
end;

class function TDateTimeEx.makeToday: TDateTimeEx;
begin
  result := TDateTimeEx.makeLocal(trunc(now));
  result.FPrecision := dtpDay;
end;

class function TDateTimeEx.fromTS(value: TTimestamp; tz : TDateTimeExTimezone): TDateTimeEx;
begin
  result.clear;
  result.Year := value.year;
  result.month := value.month;
  result.day := value.day;
  result.hour := value.hour;
  result.minute := value.minute;
  result.second := value.second;
  result.Fraction := value.fraction;
  result.FPrecision := dtpNanoSeconds;
  result.FractionPrecision := 0;
  result.TimezoneType := tz;
  result.Source := 'fromTS';
end;

function TDateTimeEx.Equal(other: TDateTimeEx): Boolean;
var
  src : TDateTimeEx;
begin
  src := TDateTimeEx(other);
  result := (year = src.year) and
    ((FPrecision < dtpMonth) or (month = src.month)) and
    ((FPrecision < dtpDay) or (day = src.day)) and
    ((FPrecision < dtpHour) or (hour = src.hour)) and
    ((FPrecision < dtpMin) or (minute = src.minute)) and
    ((FPrecision < dtpSec) or (second = src.second)) and
    ((FPrecision < dtpNanoSeconds) or (fraction = src.fraction)) and (FPrecision = src.FPrecision) and (FractionPrecision = src.FractionPrecision) and
    (TimezoneType = src.TimezoneType) and (TimeZoneHours = src.TimeZoneHours) and (TimezoneMins = src.TimezoneMins);
end;


function TDateTimeEx.fixPrecision(FPrecision: TDateTimeExPrecision): TDateTimeEx;
begin
  result := Self;
  result.FPrecision := FPrecision;
end;

function TDateTimeEx.SameTime(other: TDateTimeEx): Boolean;
begin
  if (TimezoneType = dttzUnknown) or (other.TimezoneType = dttzUnknown)  then
    result := false // unknown, really
  else
    result := sameInstant(UTC.DateTime, other.UTC.DateTime);
end;

function TDateTimeEx.ToString: String;
begin
  if null then
    exit('');
  Result := DateTimeToStr(DateTime);
end;

function IsAfterLastMonthDay(y, m, d : integer) : boolean;
begin
  case m of
    1: result := d > 31;
    2: if IsLeapYear(y) then result := d > 29 else result := d > 28;
    3: result := d > 31;
    4: result := d > 30;
    5: result := d > 31;
    6: result := d > 30;
    7: result := d > 31;
    8: result := d > 31;
    9: result := d > 30;
    10: result := d > 31;
    11: result := d > 30;
    12: result := d > 31;
  else
    result := false;
  end;
end;

procedure TDateTimeEx.RollUp;
begin
  if second >= 60 then
  begin
    inc(minute);
    second := 0;
  end;
  if minute >= 60 then
  begin
    inc(hour);
    minute := 0;
  end;
  if hour >= 24 then
  begin
    inc(day);
    hour := 0;
  end;
  if day > MONTHS_DAYS[IsLeapYear(Year), TMonthOfYear(month)] then
  begin
    inc(month);
    day := 1;
  end;
  if month > 12 then
  begin
    inc(year);
    month := 1;
  end;
end;

function TDateTimeEx.subtract(length: TDateTime): TDateTimeEx;
begin
  result := TDateTimeEx.makeUTC(dateTime - length);
  result.FPrecision := FPrecision;
  result.FractionPrecision := FractionPrecision;
  result.TimezoneType := TimezoneType;
  result.TimeZoneHours := TimeZoneHours;
  result.TimezoneMins := TimezoneMins;
end;

function TDateTimeEx.TimeStamp: TTimeStamp;
begin
  FillChar(result, sizeof(result), 0);
  result.Year := year;
  if FPrecision >= dtpMonth then
    result.month := month;
  if FPrecision >= dtpDay then
    result.day := day;
  if FPrecision >= dtpHour then
    result.hour := hour;
  if FPrecision >= dtpMin then
    result.minute := minute;
  if FPrecision >= dtpSec then
    result.second := second;
  if FPrecision >= dtpNanoSeconds then
    result.Fraction := fraction;
end;

function afterInstant(uSelf, uOther : TDateTimeEx) : boolean;
begin
  if (uSelf.year <> uOther.year) then
    result := uSelf.year > uOther.year
  else if (uSelf.month <> uOther.month) then
    result := uSelf.month > uOther.month
  else if (uSelf.day <> uOther.day) then
    result := uSelf.day > uOther.day
  else if (uSelf.hour <> uOther.hour) then
    result := uSelf.hour > uOther.hour
  else if (uSelf.minute <> uOther.minute) then
    result := uSelf.minute > uOther.minute
  else if (uSelf.second <> uOther.second) then
    result := uSelf.second > uOther.second
  else
    result := (uSelf.fraction > uOther.fraction);
end;

function TDateTimeEx.after(other : TDateTimeEx; inclusive : boolean) : boolean;
var
  uSelf, uOther : TDateTimeEx;
begin
  uSelf := UTC;
  uOther := other.UTC;
  if uSelf.equal(uOther) then
    exit(inclusive);
  result := afterInstant(uSelf, uOther);
end;

function TDateTimeEx.before(other : TDateTimeEx; inclusive : boolean):boolean;
var
  uSelf, uOther : TDateTimeEx;
begin
  uSelf := UTC;
  uOther := other.UTC;
  if uSelf.equal(uOther) then
    exit(inclusive);
  result := afterInstant(uOther, uSelf);
end;

function TDateTimeEx.between(min, max : TDateTimeEx; inclusive : boolean):boolean;
begin
  result := after(min, inclusive) and before(max, inclusive);
end;

const
  DATETIME_FORMAT_XML = 'yyyy-mm-dd"T"hh:nn:ss';

Function DateTimeFormat(Const aDateTime : TDateTime; Const sFormat : String) : String;
Begin
  Result := SysUtils.FormatDateTime(sFormat, aDateTime);
End;

Function TimeZoneBiasToString(Const aTimeZoneBias : TDateTime) : String;
Begin
  // -10:00 etc

  If aTimeZoneBias < 0 Then
    Result := DateTimeFormat(-aTimeZoneBias, '"-"hh:nn')
  Else
    Result := DateTimeFormat(aTimeZoneBias, '"+"hh:nn');
End;

Function DateTimeToXMLTimeZoneString(Const aValue : TDateTime) : String;
Begin
  If aValue = 0 Then
    Result := TimeZoneBiasToString(TimeZoneBias)
  Else
    Result := TimeZoneBiasToString(aValue);
End;

Function DateTimeToXMLDateTimeString(Const aValue : TDateTime) : String;
Begin
  Result := DateTimeFormat(aValue, DATETIME_FORMAT_XML);
End;

Function DateTimeToXMLDateTimeTimeZoneString(Const aTimestamp, aTimeZone : TDateTime) : String;
Begin
  Result := DateTimeToXMLDateTimeString(aTimestamp) + 'T' + DateTimeToXMLTimeZoneString(aTimeZone);
End;

Function TimeZoneBias : TDateTime;
begin
  result := TTimeZone.Local.GetUtcOffset(now).TotalDays;
end;

Function TimeZoneBias(when : TDateTime) : TDateTime;
begin
  result := TTimeZone.Local.GetUtcOffset(when).TotalDays;
end;


Function DateTimeCompare(Const aA, aB : TDateTime) : Integer;
Begin
  Result := MathSupport.RealCompare(aA, aB);
End;


Function DateTimeCompare(Const aA, aB, aThreshold : TDateTime) : Integer;
Begin
  Result := MathSupport.RealCompare(aA, aB, aThreshold);
End;

Function DateTimeMax(Const aA, aB : TDateTime) : TDateTime;
Begin
  If DateTimeCompare(aA, aB) > 0 Then
    Result := aA
  Else
    Result := aB;
End;


Function DateTimeMin(Const aA, aB : TDateTime) : TDateTime;
Begin
  If DateTimeCompare(aA, aB) < 0 Then
    Result := aA
  Else
    Result := aB;
End;

function TSToDateTime(TS: TTimeStamp): TDateTime;
begin
  with TS do
    Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, Fraction div 1000000);
end;

function DateTimeToTS(Value : TDateTime): TTimeStamp;
var
  DTYear, DTMonth, DTDay, DTHour, DTMinute, DTSecond, DTFraction: Word;
begin
  DecodeDate(Value, DTYear, DTMonth, DTDay);
  DecodeTime(Value, DTHour, DTMinute, DTSecond, DTFraction);
  with Result do
    begin
    Year := DTYear;
    Month := DTMonth;
    Day := DTDay;
    Hour := DTHour;
    Minute := DTMinute;
    Second := DTSecond;
    Fraction := 0; // DTFraction * 1000000;
    end;
end;

const
  MINUTE_LENGTH = 1 / (24 * 60);
  SECOND_LENGTH = MINUTE_LENGTH / 60;

function DescribePeriod(Period: TDateTime): String;
begin
  if period < 0 then
    period := -period;
  if Period < SECOND_LENGTH then
    Result := IntToStr(trunc(Period * 1000 / SECOND_LENGTH)) + 'ms'
  else if Period < 180 * SECOND_LENGTH then
    Result := IntToStr(trunc(Period / SECOND_LENGTH)) + 'sec'
  else if Period < 180 * MINUTE_LENGTH then
    Result := IntToStr(trunc(Period / MINUTE_LENGTH)) + 'min'
  else if Period < 72 * 60 * MINUTE_LENGTH then
    Result := IntToStr(trunc(Period / (MINUTE_LENGTH * 60))) + 'hr'
  else
    Result := IntToStr(trunc(Period)) + ' days';
end;

function sameInstant(t1, t2 : TDateTime) : boolean;
begin
  result := abs(t1-t2) < DATETIME_SECOND_ONE;
end;

End. // DateSupport //


