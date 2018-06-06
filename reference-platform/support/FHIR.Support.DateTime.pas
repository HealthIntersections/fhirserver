Unit FHIR.Support.DateTime;

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

{$IFDEF FPC}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH TYPEHELPERS}
{$ENDIF}

Interface

Uses
  {$IFDEF OSX} Posix.SysTypes,  {$ELSE} Windows, Registry, {$ENDIF}
  SysUtils, SysConst, DateUtils, Math, System.TimeSpan,
  FHIR.Support.Exceptions, FHIR.Support.Strings, FHIR.Support.Math, FHIR.Support.Fpc;

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
  PMonthDays = ^TMonthDays;
  TDateTimeExPrecision = (dtpYear, dtpMonth, dtpDay, dtpHour, dtpMin, dtpSec, dtpNanoSeconds);
  TDateTimeExTimezone = (dttzUnknown, dttzUTC, dttzLocal, dttzSpecified);

Const
  DELTA_DATE = 693594; // Days between 1/1/0001 and 12/31/1899

  DATETIME_FORMAT_XML = 'yyyy-mm-dd"T"hh:nn:ss';
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
  TYear = Word;

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

  TDateTimeEx = record {$IFNDEF FPC}
  private {$ENDIF}
    Source : String; // for debugging convenience, and also used to mark whether the record has any content
    year: Smallint;
    month: Word;
    day: Word;
    hour: Word;
    minute: Word;
    second: Word;
    fraction: Cardinal;
    {
      The precision to which the date and time is specified
    }
    FPrecision : TDateTimeExPrecision;
    FractionPrecision : integer;
    {
      The type of timezone
    }

    TimezoneType : TDateTimeExTimezone;
    TimeZoneHours : Integer;
    TimezoneMins : Integer;
    procedure clear();
    procedure check;
    function checkNoException : boolean;
    procedure RollUp;
    function privToString: String;{$IFNDEF FPC}
  public {$ENDIF}
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

    function fixPrecision(Precision : TDateTimeExPrecision) : TDateTimeEx;

    function Local : TDateTimeEx;
    function UTC : TDateTimeEx;
    function Min : TDateTimeEx;
    function Max : TDateTimeEx;
    function AsTz(ihr, imin : Integer):TDateTimeEx; // this date and time in the specified timezone.

    function IncrementMonth: TDateTimeEx;
    function IncrementYear: TDateTimeEx;
    function IncrementWeek: TDateTimeEx;
    function IncrementDay: TDateTimeEx;
    function add(length : TDateTime) : TDateTimeEx;
    function subtract(length : TDateTime) : TDateTimeEx;
    function lessPrecision: TDateTimeEx;

    function equal(other : TDateTimeEx) : Boolean; overload; // returns true if the timezone, FPrecision, and actual instant are the same
    function equal(other : TDateTimeEx; precision : TDateTimeExPrecision) : Boolean; overload; // returns true if the timezone, FPrecision, and actual instant are the same
    function sameTime(other : TDateTimeEx) : Boolean; // returns true if the specified instant is the same allowing for specified FPrecision - corrects for timezone
    function after(other : TDateTimeEx; inclusive : boolean):boolean;
    function before(other : TDateTimeEx; inclusive : boolean):boolean;
    function between(imin, imax : TDateTimeEx; inclusive : boolean):boolean;
    function compare(other : TDateTimeEx) : integer;

    {
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

    function clone : TDateTimeEx;
    function link : TDateTimeEx;
  end;

{$IFDEF MSWINDOWS}
type
  TTimeZoneCode = (TimeZoneUnknown,
      // proven supported, fixed where windows is wrong.
      TimeZoneNZ, TimeZoneAustraliaVicNSW, TimeZoneAustraliaQLD, TimeZoneAustraliaTas, TimeZoneAustraliaSA, TimeZoneAustraliaNT, TimeZoneAustraliaWA,

      // accepted windows at face value
      TimeZoneAfghanistan,
      TimeZoneUSAlaska,
      TimeZoneArab_Riyadh,
      TimeZoneArab_AbuDhabi,
      TimeZoneArab_Baghdad,
      TimeZoneArgentina,
      TimeZoneArmenia,
      TimeZoneCanadaAtlantic,
      TimeZoneAzerbaijan,
      TimeZoneAzores,
      TimeZoneCanadaCentral,
      TimeZoneCapeVerde,
      TimeZoneCaucasus,
      TimeZoneCentralAmerica,
      TimeZoneCentralAsia,
      TimeZoneBrazilCentral,
      TimeZoneEuropeCentral_Budapest,
      TimeZoneEuropeCentral_Warsaw,
      TimeZonePacificCentral,
      TimeZoneUSACentral,
      TimeZoneMexicoCentral,
      TimeZoneChina,
      TimeZoneAfricaEastern,
      TimeZoneEuropeEastern, //_Minsk
      TimeZoneSouthAmericaEastern_Brasilia,
      TimeZoneUSEastern,
      TimeZoneEgypt,
      TimeZoneEkaterinburg,
      TimeZoneFiji,
      TimeZoneFLE,
      TimeZoneGeorgia,
      TimeZoneGMT,
      TimeZoneGreenland,
      TimeZoneGreenwich,
      TimeZoneGTB,
      TimeZoneUSHawaii,
      TimeZoneIndia,
      TimeZoneIran,
      TimeZoneIsrael,
      TimeZoneJordan,
      TimeZoneKamchatka,
      TimeZoneKorea,
      TimeZoneMauritius,
      TimeZoneMexico1,
      TimeZoneMexico2,
      TimeZoneMidAtlantic,
      TimeZoneMiddleEast,
      TimeZoneMontevideo,
      TimeZoneMorocco,
      TimeZoneUSArizona,
      TimeZoneMexicoMountain,
      TimeZoneMyanmar,
      TimeZoneAsiaNorthCentral,
      TimeZoneNamibia,
      TimeZoneNepal,
      TimeZoneNewfoundland,
      TimeZoneAsiaNorthEast,
      TimeZoneAsiaNorth,
      TimeZonePacificChile,
      TimeZonePacific,
      TimeZoneMexicoPacific,
      TimeZonePakistan,
      TimeZoneParaguay,
      TimeZoneRomance,
      TimeZoneRussian,
      TimeZoneSouthAmericaEastern_Cayenne,
      TimeZoneSouthAmericaPacific,
      TimeZoneSouthAmericaWestern,
      TimeZoneSamoa,
      TimeZoneAsiaSouthEast,
      TimeZoneSingapore,
      TimeZoneSouthAfrica,
      TimeZoneSriLanka,
      TimeZoneTaipei,
      TimeZoneTokyo,
      TimeZoneTonga,
      TimeZoneUSIndiana,
      TimeZoneUSMountain,
      TimeZoneUTC,
      TimeZoneVenezuela,
      TimeZoneVladivostok,
      TimeZoneAfricaWestCentral,
      TimeZoneEuropeWestern,
      TimeZoneAsiaWest,
      TimeZonePacificWest,
      TimeZoneYakutsk
      );
  TTimeZoneNameArray = Array[TTimeZoneCode] Of String;
  TTimeZoneYearInfo = Class
  public
    Year : TYear;
    HasDaylightSaving : Boolean;
    StandardStart : TSystemTime;
    StandardBias : Double; // number of minutes
    DaylightStart : TSystemTime;
    DaylightBias : Double; // number of minutes
  End;
  TTimeZoneInformation = Class
  public
    Identity : TTimeZoneCode;
    Name : String;
    WindowsName : String;
    StandardName : String;
    DaylightName : String;
    Display : String;
    BaseRules : TTimeZoneYearInfo;
    YearRules : Array Of TTimeZoneYearInfo;
  End;
  {$ENDIF}

  TDateTimeOffset = Record
    Value : TDateTime;
    Offset : Integer; // TODO: could be smallint - what are the consequences of changing it now?
  End;
  PDateTimeOffset = ^TDateTimeOffset;
  TDateToken = (dttDay, dttMonth, dttYear);
  TDateIndex = Integer;
  TDateIndices = Array [TDateToken] Of TDateIndex;
  TDateFormat = String;          // eg. DD/MM/YYYY
  TTimeFormat = String;          // eg. HH:NN:SS

const
  DATEFORMAT_INDICES_SHORT : Array[TDateToken] Of Char = ('D', 'M', 'Y');
{$IFDEF MSWINDOWS}
  NAMES_TIMEZONES : TTimeZoneNameArray =
    ('Unknown', 'NewZealand', 'Australia-VIC/NSW/ACT', 'Australia-QLD', 'Australia-TAS', 'Australia-SA', 'Australia-NT', 'Australia-WA',
     'Afghan',
     'US-Alaska',
     'Arab(Riyadh)',
     'Arab(AbuDhabi)',
     'Arab(Baghdad)',
     'Argentina',
     'Armenia',
     'CA-Atlantic',
     'Azerbaijan',
     'Azores',
     'CA-Central',
     'CapeVerde',
     'Caucasus',
     'America-Central',
     'Asia-Central',
     'Brazil-Central',
     'Europe(Budapest)',
     'Europe(Warsaw)',
     'Pacific-Central',
     'US-Central',
     'Mex-Central',
     'China',
     'Africa-East',
     'Europe-East',
     'SA-East(Brasilia)',
     'US-East',
     'Egypt',
     'Ekaterinburg',
     'Fiji',
     'FLE',
     'Georgia',
     'Greenich Mean Time (GMT)',
     'Greenland',
     'Greenwich',
     'GTB',
     'US-Hawaii',
     'India',
     'Iran',
     'Israel',
     'Jordan',
     'Kamchatka',
     'Korea',
     'Mauritius',
     'Mexico1',
     'Mexico2',
     'MidAtlantic',
     'MiddleEast',
     'Montevideo',
     'Morocco',
     'US-Arizona',
     'Mex-Mountain',
     'Myanmar',
     'Asia-NC',
     'Namibia',
     'Nepal',
     'Newfoundland',
     'Asia-NE',
     'Asia-N',
     'Chile',
     'Pacific',
     'Mex-Pacific',
     'Pakistan',
     'Paraguay',
     'Romance',
     'Russian',
     'SA-East(Cayenne)',
     'SA-Pacific',
     'SA-Western',
     'Samoa',
     'Asia-SE',
     'Singapore',
     'SouthAfrica',
     'SriLanka',
     'Taipei',
     'Tokyo',
     'Tonga',
     'US-Indiana',
     'US-Mountain',
     'UTC',
     'Venezuela',
     'Vladivostok',
     'Africa-WC',
     'Europe-W',
     'Asia-W',
     'Pacific-W',
     'Yakutsk'
    );
  WINDOWS_NAMES_TIMEZONES : Array[TTimeZoneCode] Of String =
    ('',
     'New Zealand Standard Time',
     'AUS Eastern Standard Time',
     'E. Australia Standard Time',
     'Tasmania Standard Time',
     'Cen. Australia Standard Time',
     'AUS Central Standard Time',
     'W. Australia Standard Time',
     'Afghanistan Standard Time',
     'Alaskan Standard Time',
     'Arab Standard Time',
     'Arabian Standard Time',
     'Arabic Standard Time',
     'Argentina Standard Time',
     'Armenian Standard Time',
     'Atlantic Standard Time',
     'Azerbaijan Standard Time',
     'Azores Standard Time',
     'Canada Central Standard Time',
     'Cape Verde Standard Time',
     'Caucasus Standard Time',
     'Central America Standard Time',
     'Central Asia Standard Time',
     'Central Brazilian Standard Time',
     'Central Europe Standard Time',
     'Central European Standard Time',
     'Central Pacific Standard Time',
     'Central Standard Time',
     'Central Standard Time (Mexico)',
     'China Standard Time',
     'E. Africa Standard Time',
     'E. Europe Standard Time',
     'E. South America Standard Time',
     'Eastern Standard Time',
     'Egypt Standard Time',
     'Ekaterinburg Standard Time',
     'Fiji Standard Time',
     'FLE Standard Time',
     'Georgian Standard Time',
     'GMT Standard Time',
     'Greenland Standard Time',
     'Greenwich Standard Time',
     'GTB Standard Time',
     'Hawaiian Standard Time',
     'India Standard Time',
     'Iran Standard Time',
     'Israel Standard Time',
     'Jordan Standard Time',
     'Kamchatka Standard Time',
     'Korea Standard Time',
     'Mauritius Standard Time',
     'Mexico Standard Time',
     'Mexico Standard Time 2',
     'Mid-Atlantic Standard Time',
     'Middle East Standard Time',
     'Montevideo Standard Time',
     'Morocco Standard Time',
     'Mountain Standard Time',
     'Mountain Standard Time (Mexico)',
     'Myanmar Standard Time',
     'N. Central Asia Standard Time',
     'Namibia Standard Time',
     'Nepal Standard Time',
     'Newfoundland Standard Time',
     'North Asia East Standard Time',
     'North Asia Standard Time',
     'Pacific SA Standard Time',
     'Pacific Standard Time',
     'Pacific Standard Time (Mexico)',
     'Pakistan Standard Time',
     'Paraguay Standard Time',
     'Romance Standard Time',
     'Russian Standard Time',
     'SA Eastern Standard Time',
     'SA Pacific Standard Time',
     'SA Western Standard Time',
     'Samoa Standard Time',
     'SE Asia Standard Time',
     'Singapore Standard Time',
     'South Africa Standard Time',
     'Sri Lanka Standard Time',
     'Taipei Standard Time',
     'Tokyo Standard Time',
     'Tonga Standard Time',
     'US Eastern Standard Time',
     'US Mountain Standard Time',
     'UTC',
     'Venezuela Standard Time',
     'Vladivostok Standard Time',
     'W. Central Africa Standard Time',
     'W. Europe Standard Time',
     'West Asia Standard Time',
     'West Pacific Standard Time',
     'Yakutsk Standard Time'
     );
{$ENDIF}


{$IFDEF MSWINDOWS}
Function UniversalDate : TDateTime; Overload;
Function UniversalTime : TDateTime; Overload;
Function UniversalDateTime : TDateTime; Overload;
Function UniversalDateTimeOffset : TDateTimeOffset; Overload;
Function UniversalDateTimeToDateTimeOffset(Const aDateTime : TDateTime) : TDateTimeOffset;
{$ENDIF}

Function LocalDate : TDateTime; Overload;
Function LocalTime : TDateTime; Overload;
Function LocalDateTime : TDateTime; Overload;

Function TimeZoneBias : TDateTime; Overload;
Function CheckDateFormat(Const sFormat, sContent : String; Var sError : String) : Boolean;
function SameInstant(t1, t2 : TDateTime) : boolean;
Function TimeSeparator : Char; Overload;
Function DateSeparator : Char; Overload;
Function DecimalSeparator : Char; Overload;
Function ToDateTime(Const sValue, sFormat : String) : TDateTime; Overload;
Function ToDateTime(Const sValue, sFormat : String; Out aDateTime : TDateTime) : Boolean; Overload;
Function ToDateTime(Const sValue : String) : TDateTime; Overload;
Function ToDateTime(Const sValue : String; rDefault : TDateTime) : TDateTime; Overload;
Function ToDateFormat(Const aIndices : TDateIndices) : TDateFormat; Overload;
Function ToDateFormat(Const sValue : String) : TDateFormat; Overload;
Function ToDateIndices(Const sFormat : String; Out aIndices : TDateIndices) : Boolean; Overload;
Function ToDateIndices(Const sFormat : String) : TDateIndices; Overload;
Function ShortDateFormat : TDateFormat; Overload;
Function ShortTimeFormat : TTimeFormat; Overload;
Function LongDateFormat : TDateFormat; Overload;
Function LongTimeFormat : TTimeFormat; Overload;
Function TimeSeparators : TCharSet; Overload;
Function DateSeparators : TCharSet; Overload;
Function IsDateTime(Const sValue, sFormat : String) : Boolean; Overload;
Function StringToTime(Const sValue : String) : TTime; Overload;
Function StringToTime(Const sValue : String; Out aTime : TTime) : Boolean; Overload;
Function StringIsTime(Const sValue : String) : Boolean; Overload;
Function StringIsDate(Const sValue, sFormat : String) : Boolean; Overload;
Function StringIsDate(Const sValue : String) : Boolean; Overload;
Function StringToDate(Const sValue : String) : TDate; Overload;
Function TryEncodeDate(iYear, iMonth, iDay : Word; Out aDate : TDateTime) : Boolean; Overload;
Function TryEncodeDate(iYear, iMonth, iDay : Word; Out aDateTimeOffset : TDateTimeOffset) : Boolean; Overload;
Function AsDate(Const aValue : TDateTime) : TDate; Overload;
Function AsDate(Const aValue : TDateTimeOffset) : TDate; Overload;
Function AsTime(Const aValue : TDateTime) : TTime; Overload;
Function AsTime(Const aValue : TDateTimeOffset) : TTime; Overload;
Function IsLeapYearByYear(Const iYear : TYear) : Boolean; Overload;
Function IsLeapYearByDateTime(Const Value : TDateTime) : Boolean; Overload;
Function ToYear(Const Value : TDateTime) : TYear; Overload;
Function DateTimeMax(Const aA, aB : TDateTime) : TDateTime; Overload;
Function DateTimeMin(Const aA, aB : TDateTime) : TDateTime; Overload;
function DescribePeriod(Period: TDateTime): String;
function TSToDateTime(TS: TTimeStamp): TDateTime;
function DateTimeToTS(Value : TDateTime): TTimeStamp;

{$IFDEF MSWINDOWS}
Function ToDateTimeOffset(Const sValue, sFormat : String) : TDateTimeOffset; Overload;
Function ToDateTimeOffset(Const sValue, sFormat : String; Out aDateTime : TDateTimeOffset) : Boolean; Overload;

Function LocalDateTimeOffset : TDateTimeOffset; Overload;
Function TimeZoneBias(when : TDateTime) : TDateTime; Overload;
Function DateTimeCompare(Const aA, aB : TDateTime) : Integer; Overload;
Function DateTimeCompare(Const aA, aB, aThreshold : TDateTime) : Integer; Overload;
Function DateTimeToXMLDateTimeTimeZoneString(Const aTimestamp, aTimeZone : TDateTime) : String;


Function FirstOfMonth(Const Value : TDateTime) : TDateTime; Overload;
Function LastOfMonth(Const Value : TDateTime) : TDateTime; Overload;
Function StringToDuration(Const sValue : String) : TDuration; Overload;
Function StringToDuration(Const sValue : String; Out aValue : TDuration) : Boolean; Overload;
Function StringIsDuration(Const sValue : String) : Boolean; Overload;

Function DateTimeEquals(Const aA, aB : TDateTime) : Boolean; Overload;
Function DateTimeEquals(Const aA, aB : TDateTime; Const aThreshold : TDateTime) : Boolean; Overload;
Function DateTimeEquals(Const aA, aB : TDateTimeOffset) : Boolean; Overload; // obsolete.
Function DateTimeOffsetEquals(Const aA, aB : TDateTimeOffset) : Boolean; Overload; // Compared by converting to UTC
Function DateTimeOffsetEquals(Const aA, aB : TDateTimeOffset; Const aThreshold : TDateTime) : Boolean; Overload; // Compared by converting to UTC
Function ToUniversalDateTime(Const aValue : TDateTimeOffset) : TDateTime;
Function DateTimeFormat(Const aDateTime : TDateTime; Const sFormat : String) : String; Overload;
Function DateTimeFormat(Const aDateTimeOffset : TDateTimeOffset; Const sFormat : String) : String; Overload;
Function DateTimeOffsetZero : TDateTimeOffset;

Function DurationToDaysString(Const aValue : TDuration) : String; Overload;
Function DurationToHoursString(Const aValue : TDuration) : String; Overload;
Function DurationToMinutesString(Const aValue : TDuration) : String; Overload;
Function DurationToSecondsString(Const aValue : TDuration) : String; Overload;
Function DurationToShortSecondsString(Const aValue : TDuration) : String; Overload;
Function DurationToMillisecondsString(Const aValue : TDuration) : String; Overload;


function FileTimeToDateTime(Time: TFileTime; bTz : boolean): TDateTime;
Function TimeZoneOffset : Integer; Overload;
Function TimeZoneOffset(Const aDateTime : TDateTime; Const aTimeZoneInformation : TTimeZoneInformation) : Integer; Overload;
Function TimeZone : TTimeZoneCode; Overload;
Function CreateTimeZoneInformation(Const aTimeZone : TTimeZoneCode) : TTimeZoneInformation;
Procedure DestroyTimeZoneInformation(Var aTimeZoneInformation : TTimeZoneInformation);
{$ENDIF}
function DateTimeToUnix(ConvDate: TDateTime): Longint;
function UnixToDateTime(USec: Longint): TDateTime;

Implementation

uses
  FHIR.Support.System;


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

function TDateTimeEx.clone: TDateTimeEx;
begin
  result.Source := Source;
  result.year := year;
  result.month := month;
  result.day := day;
  result.hour := hour;
  result.minute := minute;
  result.second := second;
  result.fraction := fraction;
  result.FPrecision := FPrecision;
  result.FractionPrecision := FractionPrecision;
  result.TimezoneType := TimezoneType;
  result.TimeZoneHours := TimeZoneHours;
  result.TimezoneMins := TimezoneMins;
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

function TDateTimeEx.equal(other: TDateTimeEx; precision: TDateTimeExPrecision): Boolean;
begin
  result :=
    (FPrecision >= precision) and (other.Precision >= precision) and
    (year = other.year) and
    ((precision < dtpMonth) or (month = other.month)) and
    ((precision < dtpDay) or (day = other.day)) and
    ((precision < dtpHour) or (hour = other.hour)) and
    ((precision < dtpMin) or (minute = other.minute)) and
    ((precision < dtpSec) or (second = other.second)) and
    ((precision < dtpNanoSeconds) or (fraction = other.fraction)) and (FractionPrecision = other.FractionPrecision) and
    (TimezoneType = other.TimezoneType) and (TimeZoneHours = other.TimeZoneHours) and (TimezoneMins = other.TimezoneMins);
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
    raise ELibraryException.create('Fail!');
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
      raise ELibraryException.create('Unable to parse date/time "'+value+'": timezone is illegal length - must be 5');
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

function TDateTimeEx.link: TDateTimeEx;
begin
  result := self;
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
    raise ELibraryException.create('Unable to parse date/time "'+value+'" at '+name);
  result := StrToInt(v);
  if (result < min) or (result > max) then
    raise ELibraryException.create('Value for '+name+' in date/time "'+value+'" is not allowed');
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
      raise ELibraryException.create('Unable to parse date/time "'+value+'": timezone is illegal length - must be 4');
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
      raise ELibraryException.create('Unable to parse date/time "'+value+'": timezone is illegal length - must be 5');
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

function TDateTimeEx.AsTz(ihr, imin: Integer): TDateTimeEx;
var
  bias : TDateTime;
  nbias : TDateTime;
begin
  result := self;
  if ihr < 0 then
    nbias := - (-iHr * DATETIME_HOUR_ONE) + (imin * DATETIME_MINUTE_ONE)
  else
    nbias := (iHr * DATETIME_HOUR_ONE) + (imin * DATETIME_MINUTE_ONE);

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
  result.TimezoneHours := ihr;
  result.TimezoneMins := imin;
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
begin
  result := (year = other.year) and
    ((FPrecision < dtpMonth) or (month = other.month)) and
    ((FPrecision < dtpDay) or (day = other.day)) and
    ((FPrecision < dtpHour) or (hour = other.hour)) and
    ((FPrecision < dtpMin) or (minute = other.minute)) and
    ((FPrecision < dtpSec) or (second = other.second)) and
    ((FPrecision < dtpNanoSeconds) or (fraction = other.fraction)) and (FPrecision = other.FPrecision) and (FractionPrecision = other.FractionPrecision) and
    (TimezoneType = other.TimezoneType) and (TimeZoneHours = other.TimeZoneHours) and (TimezoneMins = other.TimezoneMins);
end;


function TDateTimeEx.fixPrecision(Precision: TDateTimeExPrecision): TDateTimeEx;
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

function TDateTimeEx.between(imin, imax : TDateTimeEx; inclusive : boolean):boolean;
begin
  result := after(imin, inclusive) and before(imax, inclusive);
end;

Function DateTimeFormat(Const aDateTime : TDateTime; Const sFormat : String) : String; overload;
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

Function TimeZoneBias(when : TDateTime) : TDateTime; Overload;
begin
  result := TTimeZone.Local.GetUtcOffset(when).TotalDays;
end;


Function DateTimeCompare(Const aA, aB : TDateTime) : Integer; overload;
Begin
  Result := FHIR.Support.Math.RealCompare(aA, aB);
End;


Function DateTimeCompare(Const aA, aB, aThreshold : TDateTime) : Integer; Overload;
Begin
  Result := FHIR.Support.Math.RealCompare(aA, aB, aThreshold);
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


Function CheckTimezone(bTimezone : Boolean; Const sContent, sOrigFormat : String; Var sError : String) : Boolean;
Begin
  If sContent <> '' Then
    Begin
    If bTimezone Then
      Begin
      Result := CheckDateFormat('HHNN', sContent, sError);
      If Not Result Then
        sError := sError + ' in the timezone portion';
      End
    Else
      Begin
      sError := 'The value "'+sContent+'" does not conform to the expected format for the date "'+sOrigFormat+'" because a timezone was found';
      Result := False;
      End
    End
  Else
    Result := True;
End;

Function Splice(Var sSource : String; sMatch : String) : Boolean; Overload;
Begin
  If Copy(sSource, 1, Length(sMatch)) = sMatch Then
    Begin
    Result := True;
    sSource := Copy(sSource, Length(sMatch)+ 1, $FF);
    End
  Else
    Result := False;
End;

Function Splice(Var sSource : String; iLength : Integer) : String; Overload;
Begin
  Result := Copy(sSource, 1, iLength);
  sSource := Copy(sSource, iLength + 1, $FF);
End;

Function CheckYear(sValue : String; Var sError : String; Var iYear : Integer) : Boolean;
Begin
  Result := False;
  If Length(sValue) <> 4 Then
    sError := 'Year Value '+sValue+' is not 4 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Year Value '+sValue+' is not numerical'
  Else
    Begin
    iYear := StringToInteger32(sValue);
    If (iYear <= 0) Then
      sError := 'Year Value '+sValue+': negative numbers are not supported'
    Else
      Result := True;
    End;
End;

Function CheckMonth(sValue : String; Var sError : String; Var iMonth : Integer) : Boolean;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Month Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Month Value '+sValue+' is not numerical'
  Else
    Begin
    iMonth := StringToInteger32(sValue);
    If (iMonth <= 0) Or (iMonth > 12) Then
      sError := 'Month Value '+sValue+': month must be 1 - 12'
    Else
      Result := True;
    End;
End;

Function CheckDay(sValue : String; iYear, iMonth : Integer; Var sError : String) : Boolean;
Var
  iDay : Integer;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Day Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Day Value '+sValue+' is not numerical'
  Else
    Begin
    iDay := StringToInteger32(sValue);
    If (iDay <= 0) Then
      sError := 'Day Value '+sValue+': Day must be >= 1'
    Else If iMonth = 0 Then
      sError := 'Day Value '+sValue+': Month must be known'
    Else If iDay > MONTHS_DAYS[IsLeapYear(iYear)][TMonthOfYear(iMonth-1)] Then
      sError := 'Day Value '+sValue+': Value is not valid for '+MONTHOFYEAR_SHORT[TMonthOfYear(iMonth-1)]+'-'+IntegerToString(iYear)
    Else
      Result := True;
    End;
End;

Function CheckHour(sValue : String; Var sError : String) : Boolean;
Var
  iHour : Integer;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Hour Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Hour Value '+sValue+' is not numerical'
  Else
    Begin
    iHour := StringToInteger32(sValue);
    If (iHour < 0) Or (iHour > 23) Then
      sError := 'Hour Value '+sValue+': Hour must be 0 and 23'
    Else
      Result := True;
    End;
End;

Function CheckMinute(sValue : String; Var sError : String) : Boolean;
Var
  iMinute : Integer;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Minute Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Minute Value '+sValue+' is not numerical'
  Else
    Begin
    iMinute := StringToInteger32(sValue);
    If (iMinute < 0) Or (iMinute > 59) Then
      sError := 'Minute Value '+sValue+': Minute must be 0 and 59'
    Else
      Result := True;
    End;
End;

Function CheckSecond(sValue : String; Var sError : String) : Boolean;
Var
  iSecond : Integer;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Second Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Second Value '+sValue+' is not numerical'
  Else
    Begin
    iSecond := StringToInteger32(sValue);
    If (iSecond < 0) Or (iSecond > 59) Then
      sError := 'Second Value '+sValue+': Second must be 0 and 59'
    Else
      Result := True;
    End;
End;

Function CheckDot(sValue : String; Var sError : String; Var bInFraction : Boolean) : Boolean;
Begin
  If sValue = '.' Then
    Begin
    Result := True;
    bInFraction := True;
    End
  Else
    Begin
    Result := False;
    sError := 'Expected "."';
    End;
End;

Function CheckFraction(sValue : String; Var sError : String) : Boolean;
Begin
  Result := False;
  If Not StringIsInteger32(sValue) Then
    sError := 'Fraction Value '+sValue+' is not numerical'
  Else
    Result := True;
End;


Function CheckSection(sFormat, sContent : String; Var iYear, iMonth : Integer; Var sError : String; Var bInFraction : Boolean) : Boolean;
Begin
  Result := True;

  If Splice(sFormat, 'YYYY') Then
    Result := CheckYear(splice(sContent, 4), sError, iYear);

  If Result And Splice(sFormat, 'MM') Then
    Result := CheckMonth(splice(sContent, 2), sError, iMonth);

  If Result And Splice(sFormat, 'DD') Then
    Result := CheckDay(splice(sContent, 2), iYear, iMonth, sError);

  If Result And Splice(sFormat, 'HH') Then
    Result := CheckHour(splice(sContent, 2), sError);

  If Result And Splice(sFormat, 'NN') Then
    Result := CheckMinute(splice(sContent, 2), sError);

  If Result And Not bInFraction And Splice(sFormat, 'SS') Then
    Result := CheckSecond(splice(sContent, 2), sError);

  If Result And Not bInFraction And Splice(sFormat, '.') Then
    Result := CheckDot(splice(sContent, 2), sError, bInFraction);

  While Result And bInFraction And Splice(sFormat, 'S') Do
    Result := CheckFraction(splice(sContent, 2), sError);

  If sFormat <> '' Then
    Begin
    Result := False;
    sError := 'The Date Format '+sFormat+' is not known';
    End;
End;

Function CheckSections(sFormat, sContent : String; Const  sOrigFormat, sOrigValue : String; Var sError : String) : Boolean;
Var
  bFirst : Boolean;
  sToken : String;
  sSection : String;
  bInFraction : Boolean;
  iYear : Integer;
  iMonth : Integer;
Begin
  Result := True;
  sFormat := StringStrip(sFormat, ']');
  bInFraction := False;
  bFirst := True;
  iYear := 0;
  iMonth := 0;
  Repeat
    StringSplit(sFormat, '[', sToken, sFormat);
    If sToken <> '' Then  // support use of [ at first point to make everything optional
      Begin
      sSection := Copy(sContent, 1, Length(sToken));
      If sSection = '' Then
        Begin
        If bFirst Then
          Begin
          Result := False;
          sError := StringFormat('The section %s in the Date format %s was not found in the value %s', [sToken, sOrigFormat, sOrigValue]);
          End;
        End
      Else If Length(sSection) < Length(sToken) Then
        Begin
        Result := False;
        sError := StringFormat('The section %s in the Date format %s was not completed in the value %s - value was %s', [sToken, sOrigFormat, sSection, sOrigValue]);
        End
      Else If Not CheckSection(sToken, sSection, iYear, iMonth, sError, bInFraction) Then
        Begin
        Result := False;
        sError := StringFormat('%s (in the Date format %s and the value %s)', [sError, sOrigFormat, sOrigValue]);
        End
      Else
        sContent := Copy(sContent, Length(sSection)+1, $FF);
      End;
    bFirst := False;
  Until Not Result Or (sFormat = '') Or (sContent = '');
End;

Function CheckDateFormat(Const sFormat, sContent : String; Var sError : String) : Boolean;
Var
  bTimezone : Boolean;
  sValue : String;
  sTimezone : String;
  sDateFormat : String;
Begin
  sDateFormat := sFormat;
  bTimezone := Pos('[+/-ZZZZ]', sDateFormat) > 0;
  If bTimezone Then
    Delete(sDateFormat, Pos('[+/-ZZZZ]', sDateFormat), 9);
  StringSplit(sContent, ['+', '-'], sValue, sTimezone);

  Result := CheckSections(sDateFormat, sValue, sFormat, sContent, sError);

  If Result Then
    Result := CheckTimezone(bTimezone, sTimezone, sFormat, sError);
End;

{$IFDEF MSWINDOWS}
function FileTimeToDateTime(Time: TFileTime; bTz : boolean): TDateTime;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
function FileTimeToDateTime(Time: time_t; bTz : boolean): TDateTime;
{$ENDIF POSIX}

  function InternalEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
    AMilliSecond: Word): TDateTime;
  var
    LTime: TDateTime;
    Success: Boolean;
  begin
    Result := 0;
    Success := TryEncodeDate(AYear, AMonth, ADay, Result);
    if Success then
    begin
      Success := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, LTime);
      if Success then
        if Result >= 0 then
          Result := Result + LTime
        else
          Result := Result - LTime
    end;
  end;

{$IFDEF MSWINDOWS}
var
  LFileTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result := 0;
  if bTz then
    FileTimeToLocalFileTime(Time, LFileTime)
  else
    lFileTime := time;

  if FileTimeToSystemTime(LFileTime, SysTime) then
    Result := InternalEncodeDateTime(SysTime.wYear, SysTime.wMonth, SysTime.wDay,
      SysTime.wHour, SysTime.wMinute, SysTime.wSecond, SysTime.wMilliseconds);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
//var
//  LDecTime: tm;
begin
//  Result := 0;
//
//  if localtime_r(Time, LDecTime) <> nil then
//    Result := InternalEncodeDateTime(LDecTime.tm_year + 1900, LDecTime.tm_mon + 1,
//      LDecTime.tm_mday, LDecTime.tm_hour, LDecTime.tm_min, LDecTime.tm_sec, 0);
end;
{$ENDIF POSIX}

Function DateSeparators : TCharSet;
Begin
  Result := [' ', ',', '-', '.', '/', '\', DateSeparator, TimeSeparator];
End;


Function TimeSeparators : TCharSet;
Begin
  Result := ['.', ':', TimeSeparator];
End;


Function TimeSeparator : Char;
Begin
{$IFDEF VER130}
  Result := SysUtils.TimeSeparator;
{$ELSE}
  Result := FormatSettings.TimeSeparator;
{$ENDIF}
End;


Function DateSeparator : Char;
Begin
{$IFDEF VER130}
  Result := SysUtils.DateSeparator;
{$ELSE}
  Result := FormatSettings.DateSeparator;
{$ENDIF}
End;


Function DecimalSeparator : Char;
Begin
{$IFDEF VER130}
  Result := SysUtils.DecimalSeparator;
{$ELSE}
  Result := FormatSettings.DecimalSeparator;
{$ENDIF}
End;

Function IsDateTime(Const sValue, sFormat : String) : Boolean;
Var
  aTemp : TDateTime;
Begin
  Result := ToDateTime(sValue, sFormat, aTemp);
End;

Function ToDateTime(Const sValue : String) : TDateTime;
Begin
  Result := ToDateTime(sValue, ToDateFormat(sValue));
End;

Function ToDateTime(Const sValue, sFormat: String): TDateTime;
Begin
  If Not ToDateTime(sValue, sFormat, Result) Then
    raise ELibraryException.create(StringFormat('Unable to convert ''%s'' as ''%s''', [sValue, sFormat]));
End;

Function PCharToTime(Var pValue : PChar; Out aTime : TTime) : Boolean;
Const
  ORD0 = Ord('0');
Var
  aSeps  : TCharSet;
  iHour, iMinute, iSecond, iMillisecond : Word;
Begin
  aSeps := TimeSeparators;
  iHour := 0;
  iMinute := 0;
  iSecond := 0;
  iMillisecond := 0;

  Result := Assigned(pValue);

  If Result Then
  Begin
    // Ignore Whitespace
    While CharInSet(pValue^, setWhitespace) Do
      Inc(pValue);

    // Hour
    While CharInSet(pValue^, ['0'..'9']) Do
    Begin
      iHour := (iHour * 10) + (Ord(pValue^) - ORD0);
      Inc(pValue);
    End;

    Result := (iHour < 24) And CharInSet(pValue^, aSeps);

    If Result Then
    Begin
      // Minute
      Inc(pValue);
      While CharInSet(pValue^, ['0'..'9']) Do
      Begin
        iMinute := (iMinute * 10) + (Ord(pValue^) - ORD0);
        Inc(pValue);
      End;

      Result := (iMinute < 60);

      If Result And CharInSet(pValue^, aSeps) Then
      Begin
        // Second
        Inc(pValue);

        If CharInSet(pValue^, ['0'..'9']) Then
        Begin
          // First digit
          iSecond := (Ord(pValue^) - ORD0);
          Inc(pValue);

          If CharInSet(pValue^, ['0'..'9']) Then
          Begin
            // second digit
            iSecond := (iSecond * 10) + (Ord(pValue^) - ORD0);
            Inc(pValue);

            // Ignore trailing numbers after a two digit seconds.
            While CharInSet(pValue^, ['0'..'9']) Do
              Inc(pValue);
          End;

          Result := (iSecond < 60);

          // Optional milliseconds
          If CharInSet(pValue^, aSeps) Then
          Begin
            // Millisecond
            Inc(pValue);
            While CharInSet(pValue^, ['0'..'9']) Do
            Begin
              iMillisecond := (iMillisecond * 10) + (Ord(pValue^) - ORD0);
              Inc(pValue);
            End;

            Result := (iMillisecond < 1000);
          End;
        End;
      End;

      // Optional 12 hour display of AM or PM.
      If pValue^ <> #0 Then
      Begin
        // Ignore whitespace
        While (pValue^ = ' ') Do
          Inc(pValue);

        If CharInSet(pValue^, ['A', 'a']) Then
        Begin
          Result := (iHour <= 12);

          If iHour = 12 Then
            iHour := 0;
        End
        Else If CharInSet(pValue^, ['P', 'p']) Then
        Begin
          Result := (iHour <= 12);

          If iHour < 12 Then
            Inc(iHour, 12);
        End;
      End;
    End;
  End;

  aTime := ((iHour * 3600000) + (iMinute * 60000) + (iSecond * 1000) + iMillisecond) / DATETIME_DAY_MILLISECONDS;
End;


type
  TDateNumber = Word;
  PDateNumber = ^TDateNumber;
  TDateNumbers = Array[0..Integer(High(TDateToken))] Of TDateNumber;
  TDateNumberLengths = Array[0..Integer(High(TDateToken))] Of Integer;

Function ToDateTimeElement(Var pData : PChar) : TDateNumber;
Const
  ORD_0 = Ord('0');
Var
  pStart : PChar;
  iLength : Integer;
Begin
  If CharInSet(pData^, setNumbers) Then
  Begin
    Result := 0;
    While CharInSet(pData^, setNumbers) And (Result <= 1000) Do // Less than year 10,000 in the next call.
    Begin
      Result := (Result * 10) + (Ord(pData^) - ORD_0);
      Inc(pData);
    End;
  End
  Else
  Begin
    pStart := pData;
    iLength := 0;
    While StringIsAlphabetic(pData^) Do
    Begin
      Inc(pData);
      Inc(iLength);
    End;

    If iLength = 3 Then
      Result := StringArrayIndexOf({$IFNDEF VER130}FormatSettings.{$ENDIF}ShortMonthNames, Copy(pStart, 1, iLength)) + 1
    Else
      Result := StringArrayIndexOf({$IFNDEF VER130}FormatSettings.{$ENDIF}LongMonthNames, Copy(pStart, 1, iLength)) + 1;
  End;
End;

Function ToDateTime(Const sValue, sFormat : String; Out aDateTime : TDateTime) : Boolean; Overload;
Var
  pStart : PChar;
  pValue : PChar;
  iLoop  : Integer;
  aLoop : TDateToken;
  aSeparators : TCharSet;
  aTime  : TTime;
  iYear  : TDateNumber;
  aNumbers : TDateNumbers;
  aLengths : TDateNumberLengths;
  aIndices : TDateIndices;
  iIndices : Integer;
  bLeadingTime : Boolean;
Begin
  // TODO: this routine doesn't suppose continuous date formats - eg. 'yyyymmdd'.
  //       date formats must have separation characters.

  aIndices := ToDateIndices(sFormat);
  iIndices := IntegerArrayMax(aIndices);
  pValue := PChar(sValue);

  bLeadingTime := CharInSet(charUpper(StringGet(sFormat, 1)), ['H', 'N', 'S', 'Z']);

  If bLeadingTime Then
    Result := PCharToTime(pValue, aTime)
  Else
    Result := iIndices > 0;

  If Result Then
  Begin
    If iIndices < High(aNumbers) Then
    Begin
      For aLoop := Low(aIndices) To High(aIndices) Do
      Begin
        If aIndices[aLoop] < 0 Then
          aIndices[aLoop] := iIndices + 1;
      End;
    End;

    // Whitespace
    While CharInSet(pValue^, setWhitespace) Do
      Inc(pValue);

    // Date
    aSeparators := DateSeparators;
    iLoop := Low(aNumbers);
    While (pValue^ <> #0) And (iLoop <= High(aNumbers)) Do
    Begin
      pStart := pValue;

      aNumbers[iLoop] := ToDateTimeElement(pValue);
      aLengths[iLoop] := Integer(pValue) - Integer(pStart);

      While CharInSet(pValue^, aSeparators) Do
        Inc(pValue);

      Inc(iLoop);
    End;

    Result := iLoop >= iIndices;

    If Result Then
    Begin
      While (iLoop <= High(aNumbers)) Do
      Begin
        aNumbers[iLoop] := 1;
        Inc(iLoop);
      End;

      iYear := aNumbers[aIndices[dttYear]];

      If (iYear < 100) And (aLengths[aIndices[dttYear]] <= 2) Then
      Begin
        If iYear > {$IFNDEF VER130}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow Then
          Inc(iYear, 1900)
        Else
          Inc(iYear, 2000);
      End;

      // Return
      Result := TryEncodeDate(iYear, aNumbers[aIndices[dttMonth]], aNumbers[aIndices[dttDay]], aDateTime);
    End;
  End;

  If Result And Not bLeadingTime Then
  Begin
    While Not CharInSet(pValue^, [#0, '0'..'9']) Do
      Inc(pValue);

    If (pValue^ <> #0) Then
      Result := PCharToTime(pValue, aTime)
    Else
      aTime := 0;
  End;

  If Result Then
    aDateTime := Trunc(aDateTime) + Frac(aTime);
End;


Function ToDateTime(Const sValue : String; rDefault : TDateTime) : TDateTime;
Begin
  Try
    Result := StrToDateTime(sValue);
  Except
    Result := rDefault;
  End;
End;

Function ToDateFormatDateIndicesArrayIndexOf(Const aArray : Array Of TDateIndices; Const aIndices : TDateIndices) : Integer;
Begin
  Result := High(aArray);
  While (Result >= Low(aArray)) And (MemoryCompare(@aArray[Result], @aIndices, SizeOf(aIndices)) <> 0) Do
    Dec(Result);
End;


Function ToDateFormatMatchNumber(iA, iB : TDateIndex) : Boolean;
Begin
  Result := ((iA < 0) Or (iB < 0) Or (iA = iB));
End;


Function ToDateFormatMatchArray(Const aA, aB : TDateIndices) : Boolean;
Begin
  Result := ToDateFormatMatchNumber(aB[dttYear], aA[dttYear]) And ToDateFormatMatchNumber(aB[dttMonth], aA[dttMonth]) And ToDateFormatMatchNumber(aB[dttDay], aA[dttDay]);
End;


Function ToDateFormatVerify(Const aNumbers : TDateNumbers; Const aIndices : TDateIndices) : Boolean;
Var
  aDate : TDateTime;
  iYear : TDateNumber;
Begin
  iYear := aNumbers[aIndices[dttYear]];

  If iYear < 100 Then
  Begin
    If iYear > {$IFNDEF VER130}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow Then
      Inc(iYear, 1900)
    Else
      Inc(iYear, 2000);
  End;

  Result := TryEncodeDate(iYear, aNumbers[aIndices[dttMonth]], aNumbers[aIndices[dttDay]], aDate);
End;


Function ToDateFormatChoose(Const aNumbers : TDateNumbers; Var aGuess, aCurrent : TDateIndices) : Boolean;
Const
  TOKEN_INDICES : Array[0..5] Of TDateIndices =
    ((1, 0, 2), (0, 1, 2), (2, 1, 0), (2, 0, 1), (1, 2, 0), (0, 2, 1));
Var
  iIndex : Integer;
  iLoop  : Integer;
  iLast  : Integer;
Begin
  Result := ToDateFormatMatchArray(aCurrent, aGuess) And ToDateFormatVerify(aNumbers, aCurrent);

  If Not Result Then
  Begin
    iLast := Length(TOKEN_INDICES);
    iIndex := IntegerMax(ToDateFormatDateIndicesArrayIndexOf(TOKEN_INDICES, aCurrent), 0);
    iLoop := 0;

    While (iLoop < Length(TOKEN_INDICES)) Do
    Begin
      While (iLoop < Length(TOKEN_INDICES)) And Not ToDateFormatMatchArray(TOKEN_INDICES[iIndex], aGuess) Do
      Begin
        iIndex := SignedMod(iIndex + 1, Length(TOKEN_INDICES));
        Inc(iLoop);
      End;

      If (iLoop < Length(TOKEN_INDICES)) And ToDateFormatVerify(aNumbers, TOKEN_INDICES[iIndex]) And (iIndex < iLast) Then
        iLast := iIndex;

      iIndex := SignedMod(iIndex + 1, Length(TOKEN_INDICES));
      Inc(iLoop);
    End;

    Result := iLast < Length(TOKEN_INDICES);

    If Result Then
      aCurrent := TOKEN_INDICES[iLast];
  End;
End;


Function ToDateFormat(Const sValue : String) : TDateFormat;
Var
  pValue : PChar;
  iLoop  : Integer;
  aGuess : TDateIndices;
  aCurrent : TDateIndices;
  aNumbers : TDateNumbers;
  iNumber : TDateNumber;
  aSeparators : TCharSet;
  bAlpha : Boolean;
Begin
  aSeparators := DateSeparators;
  pValue := PChar(sValue);

  // Whitespace
  While CharInSet(pValue^, setWhitespace) Do
    Inc(pValue);

  // Date
  FillChar(aGuess, SizeOf(aGuess), -1);

  iLoop := Low(aNumbers);
  While (pValue^ <> #0) And (iLoop <= High(aNumbers)) Do
  Begin
    bAlpha := StringIsAlphabetic(pValue^);

    iNumber := ToDateTimeElement(pValue);

    If (iNumber = 0) Or (iNumber > 31) Then
      aGuess[dttYear] := iLoop
    Else If (iNumber > 12) Then
      aGuess[dttDay] := iLoop
    Else If bAlpha Then
      aGuess[dttMonth] := iLoop;

    aNumbers[iLoop] := iNumber;

    While CharInSet(pValue^, aSeparators) Do
      Inc(pValue);

    Inc(iLoop);
  End;

  aCurrent := ToDateIndices(ShortDateFormat);

  If (iLoop > High(aNumbers)) And ToDateFormatChoose(aNumbers, aGuess, aCurrent) Then
    Result := ToDateFormat(aCurrent)
  Else
    Result := '';
End;


Function ToDateIndices(Const sFormat : String) : TDateIndices;
Begin
  If Not ToDateIndices(sFormat, Result) Then
    raise ELibraryException.create(StringFormat('Unable to determine the meaning of the date format ''%s''', [sFormat]));
End;


Function ToDateFormat(Const aIndices : TDateIndices) : TDateFormat;
Var
  aLoop : TDateToken;
Begin
  SetLength(Result, Length(aIndices) + 2);

  For aLoop := Low(aIndices) To High(aIndices) Do
    Result[(aIndices[aLoop] * 2) + 1] := DATEFORMAT_INDICES_SHORT[aLoop];

  Result[2] := DateSeparator;
  Result[4] := DateSeparator;
End;


Function ToDateIndices(Const sFormat : String; Out aIndices : TDateIndices) : Boolean;
Const
  SET_FORMAT = ['d', 'D', 'm', 'M', 'y', 'Y'];
Var
  iLoop  : Integer;
  aSeparators : TCharSet;
  iFormat : Integer;
  iLength : Integer;
  cFound : Char;
Begin
  FillChar(aIndices, SizeOf(aIndices), -1);

  aSeparators := DateSeparators;
  iLoop := 0;
  iFormat := 1;
  iLength := Length(sFormat);

  While (iFormat <= iLength) Do
  Begin
    While (iFormat <= iLength) And Not CharInSet(sFormat[iFormat], SET_FORMAT) Do
      Inc(iFormat);

    If (iFormat <= iLength) Then
    Begin
      cFound := sFormat[iFormat];
      Inc(iFormat);

      Case cFound Of
        'd', 'D' : aIndices[dttDay] := iLoop;
        'm', 'M' : aIndices[dttMonth] := iLoop;
        'y', 'Y' : aIndices[dttYear] := iLoop;
      End;

      // Skip format character pairs eg. 'dd/mm/yyyy'
      While (iFormat <= iLength) And (sFormat[iFormat] = cFound) Do
        Inc(iFormat);
    End;

    Inc(iLoop);
  End;

  Result := False;
  iLoop := Integer(Low(aIndices));
  While (iLoop <= Integer(High(aIndices))) And Not Result Do
  Begin
    Result := aIndices[TDateToken(iLoop)] >= 0;
    Inc(iLoop);
  End;
End;

Function ShortDateFormat : TDateFormat;
Begin
{$IFDEF VER130}
  Result := SysUtils.ShortDateFormat;
{$ELSE}
  Result := FormatSettings.ShortDateFormat;
{$ENDIF}
End;


Function ShortTimeFormat : TTimeFormat;
Begin
{$IFDEF VER130}
  Result := SysUtils.ShortTimeFormat;
{$ELSE}
  Result := FormatSettings.ShortTimeFormat;
{$ENDIF}
End;


Function LongDateFormat : TDateFormat;
Begin
{$IFDEF VER130}
  Result := SysUtils.LongDateFormat;
{$ELSE}
  Result := FormatSettings.LongDateFormat;
{$ENDIF}
End;


Function LongTimeFormat : TTimeFormat;
Begin
{$IFDEF VER130}
  Result := SysUtils.LongTimeFormat;
{$ELSE}
  Result := FormatSettings.LongTimeFormat;
{$ENDIF}
End;


Function StringIsTime(Const sValue : String) : Boolean;
Var
  aTemp : TTime;
Begin
  Result := StringToTime(sValue, aTemp);
End;



Function StringToTime(Const sValue : String) : TTime;
Begin
  If Not StringToTime(sValue, Result) Then
    raise ELibraryException.create(StringFormat('''%s'' is not a valid time', [sValue]));
End;

Function StringToTime(Const sValue : String; Out aTime : TTime) : Boolean;
Var
  pValue : PChar;
Begin
  pValue := PChar(sValue);

  Result := PCharToTime(pValue, aTime);
End;

Function IsLeapYearByYear(Const iYear : TYear) : Boolean;
Begin
  Result := SysUtils.IsLeapYear(iYear);
End;


Function IsLeapYearByDateTime(Const Value : TDateTime) : Boolean;
Begin
  Result := IsLeapYearByYear(ToYear(Value));
End;


{$IFDEF MSWINDOWS}

Function LocalDateTimeOffset : TDateTimeOffset;
Begin
  Result.Value := SysUtils.Now;
  Result.Offset := TimezoneOffset;
End;

Function TimezoneOffset : Integer;
Var
  aTimeZoneInformation : TTimeZoneInformation;
Begin
  aTimeZoneInformation := CreateTimeZoneInformation(TimeZone);
  Try
    Result := TimezoneOffset(LocalDateTime, aTimeZoneInformation);
  Finally
    DestroyTimeZoneInformation(aTimeZoneInformation);
  End;
End;

Function GetTargetDay(Const iYear, iMonth, iWeek, iDayOfWeek : Integer) : Integer;
Var
  iLoop : Integer;
Begin
  If (iWeek < 1) Or (iWeek > 4) Then
  Begin
    iLoop := MONTHS_DAYS[IsLeapYearByYear(iYear)][TMonthOfYear(iMonth-1)];
    While DayOfWeek(EncodeDate(iYear, iMonth, iLoop)) - 1 <> iDayOfWeek Do
      Dec(iLoop);
  End
  Else
  Begin
    iLoop := 7 * (iWeek - 1) + 1;
    While DayOfWeek(EncodeDate(iYear, iMonth, iLoop)) - 1 <> iDayOfWeek Do
      Inc(iLoop);
  End;
  Result := iLoop;
End;

Function IsAfterDaylightTime(Const aInstant : TDateTime; Const aTime : TSystemTime) : Boolean;
Var
  iYear, iMonth, iDay, iMin, iHour, iSec, iMSec : Word;
  iTarget : Integer;
Begin
  // we have in aTime
  //   month - the month it starts
  //   day of week - day that it clicks in (0 = Sunday)
  //   day - which week in the month it clicks in. 1 = 1st, 2 = 2nd, 3 = 3rd, 4 = 4th, else "last"
  //   min/sec
  DecodeDate(aInstant, iYear, iMonth, iDay);
  DecodeTime(aInstant, iHour, iMin, iSec, iMSec);
  Result := False;
  If (iMonth > aTime.wMonth) Then
  Begin
    Result := True;
  End
  Else If (iMonth = aTime.wMonth) Then
  Begin
    iTarget := getTargetDay(iYear, iMonth, aTime.wDay, aTime.wDayOfWeek);
    If (iDay > iTarget) Then
    Begin
      Result := True;
    End
    Else If (iDay = iTarget) Then
    Begin
      If (iHour > aTime.wHour) Then
        Result := True
      Else If (iHour = aTime.wHour) Then
        Result := (iMin >= aTime.wMinute);
    End;
  End;
End;


Function isDayLightSaving(Const aRules : TTimeZoneYearInfo; Const aInstant: TDateTime; Const bInstantIsUTC : Boolean) : Boolean;
Begin
  If Not aRules.HasDaylightSaving Then
  Begin
    Result := False;
  End
  Else If (aRules.StandardStart.wMonth < aRules.DaylightStart.wMonth) Or (aRules.DaylightStart.wMonth = 0) Then
  Begin
    // we start the year in daylight saving
    If bInstantIsUTC Then
    Begin
      If (aRules.DaylightStart.wMonth <> 0) And IsAfterDaylightTime(aInstant - (aRules.StandardBias / 1440), aRules.DaylightStart) Then
        Result := True
      Else
        Result := (aRules.StandardStart.wMonth <> 0) And Not IsAfterDaylightTime(aInstant - (aRules.DaylightBias / 1440), aRules.StandardStart);
    End
    Else
    Begin
      // we ignore the vexed question of whether aInstant is standard or daylight saving
      If (aRules.DaylightStart.wMonth <> 0) And IsAfterDaylightTime(aInstant, aRules.DaylightStart) Then
        Result := True
      Else
        Result := (aRules.StandardStart.wMonth <> 0) And Not IsAfterDaylightTime(aInstant, aRules.StandardStart);
    End;
  End
  Else
  Begin
    // we start the year in standard time

    If bInstantIsUTC Then
    Begin
      If IsAfterDaylightTime(aInstant - (aRules.DaylightBias / 1440), aRules.StandardStart) Then
        Result := False
      Else
        Result := IsAfterDaylightTime(aInstant - (aRules.StandardBias / 1440), aRules.DaylightStart);
    End
    Else
    Begin
      If IsAfterDaylightTime(aInstant, aRules.StandardStart) Then
        Result := False
      Else
        Result := IsAfterDaylightTime(aInstant, aRules.DaylightStart);
    End;
  End;
End;


Function isDayLightSavings(Const aTimeZone : TTimeZoneInformation; Const aInstant : TDateTime; Const bInstantIsUTC : Boolean) : Boolean; Overload;
Var
  bFound : Boolean;
  iLoop : Integer;
  iYear, iMonth, iDay : Word;
Begin
  // aInstant is in UTC. Get the year for the instant in local time
  // strictly, we should recalculate the year for each year
  // if there's ever a timezone where the timezone changes at the click of the new year, and it does so
  // for a particular year, this will be wrong
  If Not bInstantIsUTC Then
    DecodeDate(aInstant, iYear, iMonth, iDay)
  Else If (aTimeZone.BaseRules.StandardStart.wMonth < aTimeZone.BaseRules.DaylightStart.wMonth) And (aTimeZone.BaseRules.DaylightStart.wMonth <> 0) Then
    // because on the turn of the year, we're in daylight saving time
    DecodeDate(aInstant - (aTimeZone.BaseRules.DaylightBias / 1440), iYear, iMonth, iDay)
  Else
    DecodeDate(aInstant - (aTimeZone.BaseRules.StandardBias / 1440), iYear, iMonth, iDay);

  // First of all, which information applies?
  bFound := False;
  Result := False;

  For iLoop := Low(aTimeZone.YearRules) To High(aTimeZone.YearRules) Do
  Begin
    If iYear = aTimeZone.YearRules[iLoop].Year Then
    Begin
      Result := isDayLightSaving(aTimeZone.YearRules[iLoop], aInstant, bInstantIsUTC);
      bFound := True;

      Break;
    End;
  End;

  If Not bFound Then
    Result := isDayLightSaving(aTimeZone.BaseRules, aInstant, bInstantIsUTC)
End;





Function TimeZoneOffset(Const aDateTime : TDateTime; Const aTimeZoneInformation : TTimeZoneInformation) : Integer;
Begin
  If isDayLightSavings(aTimeZoneInformation, aDateTime, False) Then
    Result := Trunc(-aTimeZoneInformation.BaseRules.DaylightBias)
  Else
    Result := Trunc(-aTimeZoneInformation.BaseRules.StandardBias);
End;

Const
  KEY_ROOT = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\';
Function CloneTimeZoneYearInfo(Const aTimeZoneYearInfo : TTimeZoneYearInfo) : TTimeZoneYearInfo;
Begin
  Result := TTimeZoneYearInfo.Create;
  Result.Year := aTimeZoneYearInfo.Year;
  Result.HasDaylightSaving := aTimeZoneYearInfo.HasDaylightSaving;
  Result.StandardStart := aTimeZoneYearInfo.StandardStart;
  Result.StandardBias := aTimeZoneYearInfo.StandardBias;
  Result.DaylightStart := aTimeZoneYearInfo.DaylightStart;
  Result.DaylightBias := aTimeZoneYearInfo.DaylightBias;
End;


Function CreateTimeZoneInformation(Const aTimeZone : TTimeZoneCode) : TTimeZoneInformation;
Type
  REG_TZI_FORMAT = Packed Record
    Bias: LongInt; { Current Bias of the Time Zone. }
    StandardBias: LongInt; { Standard Time Bias, normally 0, for the Time Zone. }
    DaylightBias: LongInt; { Daylight Savings Bias of the Time Zone. }
    StandardDate: TSystemTime; { Date Standard Time takes over if Daylight Savings Time is used in the Time Zone. }
    DaylightDate: TSystemTime; { Date Daylight Savings Time takes over if Daylight Savings Time used in the Time Zone. }
  End;
Var
  oReg : TRegistry;
  aInfo : TIME_ZONE_INFORMATION;
  aRegInfo : REG_TZI_FORMAT;
  iStart : Integer;
  iLast : Integer;
  iLoop : Integer;
  oTimeZoneYearInfo : TTimeZoneYearInfo;
Begin
  // Get the information used interanally which is extracted from the registry - cache it up for quicker usage.

  Result := TTimeZoneInformation.Create;
  Try
    Result.Identity := aTimeZone;
    Result.Name := NAMES_TIMEZONES[aTimeZone];
    Result.BaseRules := TTimeZoneYearInfo.Create;

    oReg := TRegistry.Create;
    Try
      If aTimeZone = TimeZoneUnknown Then
      Begin
        If GetTimeZoneInformation(aInfo) = $FFFFFFFF Then
          raise ELibraryException.create(StringFormat('Unable to get time zone information [%s]', [ErrorAsString]));
        Result.WindowsName := aInfo.StandardName;
        Result.Display := '(unknown timezone)';
        Result.BaseRules.Year := 0;
        Result.BaseRules.StandardStart := aInfo.StandardDate;
        Result.BaseRules.StandardBias := aInfo.Bias + aInfo.StandardBias;
        Result.BaseRules.DaylightStart := aInfo.DaylightDate;
        Result.BaseRules.DaylightBias := aInfo.Bias + aInfo.DaylightBias;
      End
      Else
      Begin
        Result.WindowsName := WINDOWS_NAMES_TIMEZONES[aTimeZone];
        oReg.RootKey := HKEY_LOCAL_MACHINE;
        If Not oReg.OpenKeyReadOnly(KEY_ROOT + Result.WindowsName) Then
          raise ELibraryException.create(StringFormat('Unable to load time zone information [%s]', [ErrorAsString]));
        Result.Display := oReg.ReadString('Display');
        If oReg.ReadBinaryData('TZI', aRegInfo, SizeOf(aRegInfo)) <> SizeOf(aRegInfo) Then
          raise ELibraryException.create(StringFormat('Unable to load time zone binary information [%s]', [ErrorAsString]));
        Result.BaseRules.Year := 0;
        Result.BaseRules.StandardStart := aRegInfo.StandardDate;
        Result.BaseRules.StandardBias := aRegInfo.Bias + aRegInfo.StandardBias;
        Result.BaseRules.DaylightStart := aRegInfo.DaylightDate;
        Result.BaseRules.DaylightBias := aRegInfo.Bias + aRegInfo.DaylightBias;
        Result.StandardName := oReg.ReadString('Std');
        Result.DaylightName := oReg.ReadString('Dlt')
      End;
      Result.BaseRules.HasDaylightSaving := Result.BaseRules.DayLightStart.wMonth <> 0;
      oReg.CloseKey;
      If oReg.OpenKeyReadOnly(KEY_ROOT + Result.WindowsName+'\Dynamic DST') Then
      Begin
        iStart := oReg.ReadInteger('FirstEntry');
        iLast := oReg.ReadInteger('LastEntry');
        SetLength(Result.YearRules, iLast - iStart + 1);
        For iLoop := iStart To iLast Do
        Begin
          oTimeZoneYearInfo := TTimeZoneYearInfo.Create;
          Result.YearRules[iLoop - iStart] := oTimeZoneYearInfo;

          oTimeZoneYearInfo.Year := iLoop;
          If oReg.ReadBinaryData(IntegerToString(iLoop), aRegInfo, SizeOf(aRegInfo)) <> SizeOf(aRegInfo) Then
            raise ELibraryException.create(StringFormat('Unable to load time zone binary information [%s] for [%s]', [ErrorAsString, IntegerToString(iLoop)]));
          oTimeZoneYearInfo.StandardStart := aRegInfo.StandardDate;
          oTimeZoneYearInfo.StandardBias := aRegInfo.Bias + aRegInfo.StandardBias;
          oTimeZoneYearInfo.DaylightStart := aRegInfo.DaylightDate;
          oTimeZoneYearInfo.DaylightBias := aRegInfo.Bias + aRegInfo.DaylightBias;
          oTimeZoneYearInfo.HasDaylightSaving := oTimeZoneYearInfo.DayLightStart.wMonth <> 0;
        End;
      End;
    Finally
      oReg.Free;
    End;
    // This is a temporary workaround for erroneous information in
    // some windows registries. Fix is http://support.microsoft.com/hotfix/KBHotfix.aspx?kbnum=974176&kbln=en-us
    // but it is not widely applied
    If (aTimeZone = TimeZoneAustraliaWA) And (Result.BaseRules.DaylightStart.wMonth <> 0) Then
    Begin
      SetLength(Result.YearRules, Length(Result.YearRules) + 2);
      // first year, 2005, just repeats no daylight
      // second year, 2006, claims daylight savings has already started
      Result.YearRules[1].StandardStart.wMonth := 0;
      Result.YearRules[High(Result.YearRules) - 1] := CloneTimeZoneYearInfo(Result.BaseRules);
      Result.YearRules[High(Result.YearRules) - 1].Year := 2008;
      Result.BaseRules.DaylightStart.wMonth := 0; // Daylight saving ended in March 2009
      Result.YearRules[High(Result.YearRules)] := CloneTimeZoneYearInfo(Result.BaseRules);
      Result.YearRules[High(Result.YearRules)].Year := 2009;
      Result.BaseRules.StandardStart.wMonth := 0; // no more daylight saving
      Result.BaseRules.HasDaylightSaving := False;
    End;
  Except
    DestroyTimeZoneInformation(Result);

    Raise;
  End;
End;


Procedure DestroyTimeZoneInformation(Var aTimeZoneInformation : TTimeZoneInformation);
Var
  iYearRuleIndex : Integer;
Begin
  If Assigned(aTimeZoneInformation) Then
  Begin
    FreeAndNil(aTimeZoneInformation.BaseRules);

    For iYearRuleIndex := Low(aTimeZoneInformation.YearRules) To High(aTimeZoneInformation.YearRules) Do
      FreeAndNil(aTimeZoneInformation.YearRules[iYearRuleIndex]);

    aTimeZoneInformation.Free;
    aTimeZoneInformation := Nil;
  End;
End;

Function TimeZone : TTimeZoneCode;
Var
  iRet : DWord;
  aInfo : TIME_ZONE_INFORMATION;
  aLoop : TTimeZoneCode;
Begin
  // this is the current TimeZone.

  Result := TimeZoneUnknown;

  iRet := GetTimeZoneInformation(aInfo);

  If iRet = $FFFFFFFF Then
    raise ELibraryException.create(StringFormat('Unable to get time zone information [%s]', [ErrorAsString]));

  For aLoop := Low(TTimeZoneCode) To High(TTimeZoneCode) Do
  Begin
    If aInfo.StandardName = WINDOWS_NAMES_TIMEZONES[aLoop] Then
      Result := aLoop;
  End;

  If Result = TimeZoneUnknown Then
  Begin
    If (StringStartsWith(aInfo.StandardName, 'Tasmania')) Then
    Begin
      Result := TimeZoneAustraliaTas;
    End
    Else If (StringStartsWith(aInfo.StandardName, 'New Zealand')) Then
    Begin
      Result := TimeZoneNZ;
    End
    Else If aInfo.Bias = -600 Then
    Begin
      If (iRet = 0) Then
        Result := TimeZoneAustraliaQLD
      Else
        Result := TimeZoneAustraliaVicNSW;
    End
    Else If aInfo.Bias = -570 Then
    Begin
      If (iRet = 0) Then
        Result := TimeZoneAustraliaNT
      Else
        Result := TimeZoneAustraliaSA;
    End
    Else If aInfo.Bias = -480 Then
      Result := TimeZoneAustraliaWA
    Else
      Result := TimeZoneUnknown;
  End;
End;
{$ENDIF}

{$IFDEF MSWINDOWS}
Function UniversalDate : TDateTime;
Begin
  Result := AsDate(UniversalDateTime);
End;


Function UniversalTime : TDateTime;
Begin
 Result := AsTime(UniversalDateTime);
End;


Function UniversalDateTime : TDateTime;
Var
  LrSystemTime : TSystemTime;
Begin
  GetSystemTime(LrSystemTime);

  Result := SystemTimeToDateTime(LrSystemTime);
End;

Function UniversalDateTimeOffset : TDateTimeOffset;
Begin
  Result.Value := UniversalDateTime;
  Result.Offset := 0;
End;


Function UniversalDateTimeToDateTimeOffset(Const aDateTime : TDateTime) : TDateTimeOffset;
Begin
  Result.Value := aDateTime;
  Result.Offset := 0;
End;
{$ENDIF}


Function LocalDate : TDateTime;
Begin
  Result := AsDate(LocalDateTime);
End;


Function LocalTime : TDateTime;
Begin
  Result := AsTime(LocalDateTime);
End;


Function LocalDateTime : TDateTime;
Begin
  Result := SysUtils.Now;
End;


Function AsDate(Const aValue : TDateTime) : TDate;
Begin
  Result := Trunc(aValue);
End;


Function AsTime(Const aValue : TDateTime) : TTime;
Begin
  Result := Frac(aValue);
End;


Function AsDate(Const aValue : TDateTimeOffset) : TDate;
Begin
  Result := AsDate(aValue.Value);
End;


Function AsTime(Const aValue : TDateTimeOffset) : TTime;
Begin
  Result := Frac(aValue.Value);
End;

Function ToYear(Const Value : TDateTime) : Word;
Var
  iTemp : Word;
Begin
  DecodeDate(Value, Result, iTemp, iTemp);
End;


Function FirstOfMonth(Const Value : TDateTime) : TDateTime;
Var
  Y, M, D : Word;
Begin
  DecodeDate(Value, Y, M, D);

  Result := EncodeDate(Y, M, 1);
End;




Function LastOfMonth(Const Value : TDateTime) : TDateTime;
Var
  Y, M, D : Word;
Begin
  DecodeDate(Value, Y, M, D);

  Result := EncodeDate(Y, M, MonthDays[IsLeapYearByYear(Y), M]);
End;


Function ToFactors(Const sDuration : String; aSeparators : TCharSet; Const aMultipliers : Array Of Real; Const aLimits : Array Of Integer; Out aValue : Real) : Boolean;
Var
  iLast : Integer;
  iNext : Integer;
  iLoop : Integer;
  iValue : Integer;
  iUpper : Integer;
Begin
  Result := True;
  aValue := 0;
  iLast := 1;
  iNext := StringFind(sDuration, aSeparators, iLast);

  iLoop := Low(aMultipliers);
  iUpper := IntegerMin(Integer(High(aMultipliers)), Integer(High(aLimits)));
  While (iNext > 0) And (iLoop <= iUpper) And Result Do
  Begin
    iValue := StrToIntDef(Copy(sDuration, iLast, iNext - iLast), -1);
    Result := (iValue >= 0) And (iValue <= aLimits[iLoop]);

    aValue := aValue + iValue * aMultipliers[iLoop];
    iLast := iNext + 1;
    iNext := StringFind(sDuration, aSeparators, iLast);

    Inc(iLoop);
  End;

  If Result And (iLast <= Length(sDuration)) And (iLoop <= iUpper) Then
  Begin
    iValue := StrToIntDef(Copy(sDuration, iLast, MaxInt), -1);
    Result := (iValue >= 0) And (iValue <= aLimits[iLoop]);

    aValue := aValue + (iValue * aMultipliers[iLoop]);
  End;
End;




Function StringToDuration(Const sValue : String; Out aValue : TDuration) : Boolean; overload;
Const
  DURATION_MULTIPLIERS : Array[0..3] Of Real = (3600000, 60000, 1000, 1);
  DURATION_LIMITS : Array[0..3] Of Integer = (MaxInt, 59, 59, 999);
Var
  rDuration : Real;
  sDuration : String;
  iFactor   : Integer;
Begin
  Result := (Length(sValue) > 0);

  If Result Then
  Begin
    If sValue[1] = '-' Then
    Begin
      sDuration := Copy(sValue, 2, MaxInt);
      iFactor := -1;
    End
    Else If sValue[1] = '+' Then
    Begin
      sDuration := Copy(sValue, 2, MaxInt);
      iFactor := 1;
    End
    Else
    Begin
      sDuration := sValue;
      iFactor := 1;
    End;

    Result := ToFactors(sDuration, TimeSeparators, DURATION_MULTIPLIERS, DURATION_LIMITS, rDuration);

    If Result Then
      aValue := Trunc(rDuration) * iFactor
    Else
      aValue := 0;
  End;
End;

Function StringIsDuration(Const sValue : String) : Boolean;
Var
  aDuration : TDuration;
Begin
  Result := StringToDuration(sValue, aDuration);
End;

Function StringToDuration(Const sValue : String) : TDuration; overload;
Begin
  If Not StringToDuration(sValue, Result) Then
    raise ELibraryException.create(sValue + ' is not a valid duration.');
End;


Function DateTimeEquals(Const aA, aB : TDateTime) : Boolean;
Begin
  Result := DateTimeCompare(aA, aB) = 0;
End;

{$IFDEF MSWINDOWS}
Function DateTimeOffsetCompare(Const aA, aB : TDateTimeOffset) : Integer; overload;
Begin
  Result := FHIR.Support.Math.RealCompare(ToUniversalDatetime(aA), ToUniversalDateTime(aB));

  If Result = 0 Then
    Result := FHIR.Support.Math.IntegerCompare(aA.Offset, aB.Offset);
End;


Function DateTimeOffsetCompare(Const aA, aB : TDateTimeOffset; Const aThreshold : TDateTime) : Integer; overload;
Begin
  Result := FHIR.Support.Math.RealCompare(ToUniversalDatetime(aA), ToUniversalDateTime(aB), aThreshold);

  If Result = 0 Then
    Result := FHIR.Support.Math.IntegerCompare(aA.Offset, aB.Offset);
End;


Function DateTimeOffsetEquals(Const aA, aB : TDateTimeOffset) : Boolean;
Begin
  Result := DateTimeOffsetCompare(aA, aB) = 0;
End;


Function DateTimeOffsetEquals(Const aA, aB : TDateTimeOffset; Const aThreshold : TDateTime) : Boolean;
Begin
  Result := DateTimeOffsetCompare(aA, aB, aThreshold) = 0;
End;


Function DateTimeEquals(Const aA, aB : TDateTimeOffset) : Boolean; // obsolete.
Begin
  Result := DateTimeOffsetEquals(aA, aB);
End;


Function DateTimeEquals(Const aA, aB : TDateTime; Const aThreshold : TDateTime) : Boolean;
Begin
  Result := DateTimeCompare(aA, aB, aThreshold) = 0;
End;
{$ENDIF}

Function ToUniversalDateTime(Const aValue : TDateTimeOffset) : TDateTime;
Begin
  //Result := aValue.Value + (aValue.Offset * DATETIME_MINUTE_ONE);
  Result := aValue.Value - (aValue.Offset * DATETIME_MINUTE_ONE);
End;

Function DateTimeOffsetZero : TDateTimeOffset;
Begin
  Result.Value := 0;
  Result.Offset := 0;
End;

Function LocalDateTimeFormat(Const aDateTimeOffset : TDateTimeOffset; Const sFormat : String) : String;
Begin
  Result := SysUtils.FormatDateTime(sFormat, aDateTimeOffset.Value);
End;

Function DateTimeFormat(Const aDateTimeOffset : TDateTimeOffset; Const sFormat : String) : String; overload;
Begin
  Result := LocalDateTimeFormat(aDateTimeOffset, sFormat);
End;

Function DurationToDaysString(Const aValue : TDuration) : String;
Begin
  Result := StringFormat('%0.2f', [aValue / DATETIME_DAY_MILLISECONDS]);
End;

Function FormatDuration(Const sFormat : String; Const aValue : TDuration) : String;
Var
  iMilliseconds : Integer;
  iSeconds : Integer;
  iMinutes : Integer;
  iHours : Int64;
  iDays : Int64;

  bUsingDays : Boolean;
Begin
  // Return negative durations as a '-' + ToDurationMS(-Value)
  iDays := Abs(aValue);

  bUsingDays := StringExists(sFormat, '%1:');

  iMilliseconds := Abs(SignedMod(iDays, 1000));
  iDays := iDays Div 1000;

  iSeconds := Abs(SignedMod(iDays, 60));
  iDays := iDays Div 60;

  iMinutes := Abs(SignedMod(iDays, 60));
  iDays := iDays Div 60;

  If bUsingDays Then
  Begin
    iHours := Abs(SignedMod(iDays, 24));
    iDays := iDays Div 24;
  End
  Else
  Begin
    iHours := iDays;
  End;

  Result := StringFormat(sFormat, [TimeSeparator, iDays, iHours, iMinutes, iSeconds, DecimalSeparator, iMilliseconds]);

  If aValue < 0 Then
    Result := '-' + Result;
End;


Function DurationToHoursString(Const aValue : TDuration) : String;
Begin
  Result := FormatDuration('%2:.2d', aValue);
End;


Function DurationToMinutesString(Const aValue : TDuration) : String;
Begin
  Result := FormatDuration('%2:.2d%0:s%3:.2d', aValue);
End;


Function DurationToSecondsString(Const aValue : TDuration) : String;
Begin
  Result := FormatDuration('%2:.2d%0:s%3:.2d%0:s%4:.2d', aValue);
End;


Function DurationToShortSecondsString(Const aValue : TDuration) : String;
Begin
  Result := FormatDuration('%3:.2d%0:s%4:.2d', aValue);
End;


Function DurationToMillisecondsString(Const aValue : TDuration) : String;
Begin
  Result := FormatDuration('%2:.2d%0:s%3:.2d%0:s%4:.2d%5:s%6:.3d', aValue);
End;

Function StringIsDate(Const sValue, sFormat : String) : Boolean;
Begin
  Result := IsDateTime(sValue, sFormat);
End;


Function StringIsDate(Const sValue : String) : Boolean;
Begin
  Result := IsDateTime(sValue, ShortDateFormat);
End;

Function StringToDate(Const sValue : String) : TDate;
Begin
  Result := StrToDate(sValue);
End;

Function ToUniversalDateTimeOffset(Const sValue, sFormat : String) : TDateTimeOffset;
Begin
  Result.Value := ToDateTime(sValue, sFormat);
  Result.Offset := 0;
End;

{$IFDEF MSWINDOWS}
Function ToLocalDateTimeOffset(Const sValue, sFormat : String) : TDateTimeOffset;
Begin
  Result.Value := ToDateTime(sValue, sFormat);
  Result.Offset := LocalDateTimeOffset.Offset;
End;




Function ToDateTimeOffset(Const sValue, sFormat : String) : TDateTimeOffset;
Begin
  Result := ToLocalDateTimeOffset(sValue, sFormat);
End;


Function ToDateTimeOffset(Const sValue, sFormat : String; Out aDateTime : TDateTimeOffset) : Boolean;
Var
  pStart : PChar;
  pValue : PChar;
  iLoop  : Integer;
  aLoop : TDateToken;
  aSeparators : TCharSet;
  aTime  : TTime;
  iYear  : TDateNumber;
  aNumbers : TDateNumbers;
  aLengths : TDateNumberLengths;
  aIndices : TDateIndices;
  iIndices : Integer;
  bLeadingTime : Boolean;
Begin
  // TODO: this routine doesn't suppose continuous date formats - eg. 'yyyymmdd'.
  //       date formats must have separation characters.

  aIndices := ToDateIndices(sFormat);
  iIndices := IntegerArrayMax(aIndices);
  pValue := PChar(sValue);

  bLeadingTime := CharInSet(charUpper(StringGet(sFormat, 1)), ['H', 'N', 'S', 'Z']);

  If bLeadingTime Then
    Result := PCharToTime(pValue, aTime)
  Else
    Result := iIndices > 0;

  If Result Then
  Begin
    If iIndices < High(aNumbers) Then
    Begin
      For aLoop := Low(aIndices) To High(aIndices) Do
      Begin
        If aIndices[aLoop] < 0 Then
          aIndices[aLoop] := iIndices + 1;
      End;
    End;

    // Whitespace
    While CharInSet(pValue^, setWhitespace) Do
      Inc(pValue);

    // Date
    aSeparators := DateSeparators;
    iLoop := Low(aNumbers);
    While (pValue^ <> #0) And (iLoop <= High(aNumbers)) Do
    Begin
      pStart := pValue;

      aNumbers[iLoop] := ToDateTimeElement(pValue);
      aLengths[iLoop] := Integer(pValue) - Integer(pStart);

      While CharInSet(pValue^, aSeparators) Do
        Inc(pValue);

      Inc(iLoop);
    End;

    Result := iLoop >= iIndices;

    If Result Then
    Begin
      While (iLoop <= High(aNumbers)) Do
      Begin
        aNumbers[iLoop] := 1;
        Inc(iLoop);
      End;

      iYear := aNumbers[aIndices[dttYear]];

      If (iYear < 100) And (aLengths[aIndices[dttYear]] <= 2) Then
      Begin
        If iYear > {$IFNDEF VER130}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow Then
          Inc(iYear, 1900)
        Else
          Inc(iYear, 2000);
      End;

      // Return
      Result := TryEncodeDate(iYear, aNumbers[aIndices[dttMonth]], aNumbers[aIndices[dttDay]], aDateTime);
    End;
  End;

  If Result And Not bLeadingTime Then
  Begin
    While Not CharInSet(pValue^, [#0, '0'..'9']) Do
      Inc(pValue);

    If (pValue^ <> #0) Then
      Result := PCharToTime(pValue, aTime)
    Else
      aTime := 0;
  End;

  If Result Then
  Begin
    aDateTime.Value := Trunc(aDateTime.Value) + Frac(aTime);
    aDateTime.Offset := LocalDateTimeOffset.Offset;
  End;
End;
{$ENDIF}


Function TryEncodeDate(iYear, iMonth, iDay : Word; Out aDate : TDateTime) : Boolean;
Var
  iTemp  : Integer;
  pTable : PMonthDays;
Begin
  aDate := 0;

  pTable := @MONTHS_DAYS[IsLeapYearByYear(iYear)];

  Result := (iYear >= 1) And (iYear <= 9999) And (iMonth >= 1) And (iMonth <= 12) And (iDay >= 1) And (iDay <= pTable^[TMonthOfYear(iMonth - 1)]);

  If Result Then
  Begin
    For iTemp := 0 To iMonth - 2 Do
      Inc(iDay, pTable^[TMonthOfYear(iTemp)]);

    iTemp := iYear - 1;

    aDate := (iTemp * 365) + (iTemp Div 4) - (iTemp Div 100) + (iTemp Div 400) + iDay - DELTA_DATE;
  End;
End;


Function TryEncodeDate(iYear, iMonth, iDay : Word; Out aDateTimeOffset : TDateTimeOffset) : Boolean;
Begin
  Result := TryEncodeDate(iYear, iMonth, iDay, aDateTimeOffset.Value);
End;


const
  // Sets UnixStartDate to TDateTime of 01/01/1970
  UnixStartDate: TDateTime = 25569.0;

function DateTimeToUnix(ConvDate: TDateTime): Longint;
begin
  Result := Round((ConvDate - UnixStartDate) * 86400);
end;

function UnixToDateTime(USec: Longint): TDateTime;
begin
  Result := (Usec / 86400) + UnixStartDate;
end;

End.


