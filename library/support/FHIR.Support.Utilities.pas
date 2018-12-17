﻿Unit FHIR.Support.Utilities;

{$IFDEF FPC}{$mode delphi}{$ENDIF}
{$DEFINE UT}

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
  {$IFDEF MACOS} FHIR.Support.Osx, MacApi.Foundation, Posix.SysTypes, Posix.Stdlib, {$ELSE} Windows, ShellApi, ShlObj,  MMSystem, Winsock, Registry, MultiMon, {$ENDIF}
  SysUtils, System.Types, System.TimeSpan, System.NetEncoding, Classes, EncdDecd, Generics.Collections, UIConsts, Math, TypInfo, Character, RegularExpressions, SysConst,
  FHIR.Support.Fpc, FHIR.Support.Base;


Function IntegerCompare(Const iA, iB : Byte) : Integer; Overload;
Function IntegerCompare(Const iA, iB : Word) : Integer; Overload;
Function IntegerCompare(Const iA, iB : Integer) : Integer; Overload;
Function IntegerCompare(Const iA, iB : Cardinal) : Integer; Overload;
Function IntegerCompare(Const iA, iB : Int64) : Integer; Overload;
Function IntegerCompare(Const iA, iB, iThreshold : Int64) : Integer; Overload;
Function BooleanCompare(Const bA, bB : Boolean) : Integer; Overload;
Function RealCompare(Const rA, rB : Extended) : Integer; Overload;
{$IFNDEF FPC}
Function RealCompare(Const rA, rB : Real) : Integer; Overload;
{$ENDIF}
Function RealCompare(Const rA, rB, rThreshold : Real) : Integer; Overload;

Function IntegerEquals(Const iA, iB : Byte) : Boolean; Overload;
Function IntegerEquals(Const iA, iB : Word) : Boolean; Overload;
Function IntegerEquals(Const iA, iB : Integer) : Boolean; Overload;
Function IntegerEquals(Const iA, iB : Cardinal) : Boolean; Overload;
Function IntegerEquals(Const iA, iB : Int64) : Boolean; Overload;
Function IntegerEquals(Const iA, iB, iThreshold : Int64) : Boolean; Overload;
Function BooleanEquals(Const bA, bB : Boolean) : Boolean; Overload;
Function RealEquals(Const rA, rB : Real) : Boolean; Overload;
Function RealEquals(Const rA, rB, rThreshold : Real) : Boolean; Overload;

Function IntegerBetweenInclusive(Const iLeft, iCheck, iRight : Integer) : Boolean; Overload;
Function IntegerBetweenExclusive(Const iLeft, iCheck, iRight : Integer) : Boolean; Overload;
Function IntegerBetween(Const iLeft, iCheck, iRight : Integer) : Boolean; Overload;

Function CardinalBetweenInclusive(Const iLeft, iCheck, iRight : Cardinal) : Boolean; Overload;
Function CardinalBetweenExclusive(Const iLeft, iCheck, iRight : Cardinal) : Boolean; Overload;

Function IntegerBetweenInclusive(Const iLeft, iCheck, iRight : Int64) : Boolean; Overload;
Function IntegerBetweenExclusive(Const iLeft, iCheck, iRight : Int64) : Boolean; Overload;
Function IntegerBetween(Const iLeft, iCheck, iRight : Int64) : Boolean; Overload;

Function RealBetweenInclusive(Const rLeft, rCheck, rRight : Real) : Boolean; Overload;
Function RealBetweenExclusive(Const rLeft, rCheck, rRight : Real) : Boolean; Overload;
Function RealBetween(Const rLeft, rCheck, rRight : Real) : Boolean; Overload;

Function RealBetweenInclusive(Const rLeft, rCheck, rRight : Extended) : Boolean; Overload;
Function RealBetweenExclusive(Const rLeft, rCheck, rRight : Extended) : Boolean; Overload;
Function RealBetween(Const rLeft, rCheck, rRight : Extended) : Boolean; Overload;

Function IntegerMin(Const A, B : Integer) : Integer; Overload;
Function IntegerMax(Const A, B : Integer) : Integer; Overload;
Function IntegerMin(Const A, B : Cardinal) : Cardinal; Overload;
Function IntegerMax(Const A, B : Cardinal) : Cardinal; Overload;
Function IntegerMin(Const A, B : Word) : Word; Overload;
Function IntegerMax(Const A, B : Word) : Word; Overload;
Function IntegerMin(Const A, B : Byte) : Byte; Overload;
Function IntegerMax(Const A, B : Byte) : Byte; Overload;
Function IntegerMin(Const A, B : Int64) : Int64; Overload;
Function IntegerMax(Const A, B : Int64) : Int64; Overload;
Function RealMin(Const A, B : Real) : Real; Overload;
Function RealMax(Const A, B : Real) : Real; Overload;

Function RealCeiling(Const rValue : Real) : Int64; Overload;
Function RealFloor(Const rValue : Real) : Int64; Overload;

Function Int64Round(Const iValue, iFactor : Int64) : Int64; Overload;

Function RealSquare(Const rValue : Extended) : Extended; Overload;
Function RealSquareRoot(Const rValue : Extended) : Extended; Overload;

Function IntegerArrayMax(Const aIntegers : Array Of Integer) : Integer; Overload;
Function IntegerArrayMin(Const aIntegers : Array Of Integer) : Integer; Overload;
Function IntegerArraySum(Const aIntegers : Array Of Integer) : Integer; Overload;
Function IntegerArrayIndexOf(Const aIntegers : Array Of Word; iValue : Word) : Integer; Overload;
Function IntegerArrayIndexOf(Const aIntegers : Array Of Integer; iValue : Integer) : Integer; Overload;
Function IntegerArrayIndexOf(Const aIntegers : Array Of Cardinal; iValue : Cardinal) : Integer; Overload;
Function IntegerArrayExists(Const aIntegers : Array Of Integer; iValue : Integer) : Boolean; Overload;
Function IntegerArrayValid(Const aIntegers : Array Of Integer; iIndex : Integer) : Boolean; Overload;

Function RealRoundToInteger(Const aValue : Extended) : Int64; Overload;

Function Abs(Const iValue : Int64) : Int64; Overload;
Function Abs(Const iValue : Integer) : Integer; Overload;
Function Abs(Const rValue : Real) : Real; Overload;
Function Sign(Const iValue : Integer) : Integer; Overload;

Function IntegerConstrain(Const iValue, iMin, iMax : Integer) : Integer; Overload;
Function RealConstrain(Const rValue, rMin, rMax : Real) : Real; Overload;

Function Sin(Const Theta : Extended) : Extended; Overload;
Function Cos(Const Theta : Extended) : Extended; Overload;
Function ArcTan(Const Theta : Extended) : Extended; Overload;
Function ArcTan2(Const X, Y : Extended) : Extended; Overload;

Function DegreesToRadians(Const rDegrees : Extended) : Extended; Overload;
Function RadiansToDegrees(Const rRadians : Extended) : Extended; Overload;

Function Power(Const rBase : Extended; Const iExponent : Integer) : Extended; Overload;
Function Power(Const rBase, rExponent : Extended) : Extended; Overload;

Function LnXP1(Const X : Extended) : Extended; Overload;
Function LogN(Const Base, X : Extended) : Extended; Overload;
Function Log10(Const X : Extended) : Extended; Overload;
Function Log2(Const X : Extended)  : Extended; Overload;
Function Hypotenuse(Const X, Y : Extended) : Extended; Overload;

Function Percentage(Const iPart, iTotal : Integer) : Real; Overload;

Function SignedMod(Const iValue : Integer; Const iRange : Integer) : Integer; Overload;
Function SignedMod(Const iValue : Int64; Const iRange : Int64) : Int64; Overload;
Function UnsignedMod(Const iValue : Cardinal; Const iRange : Cardinal) : Cardinal; Overload;
Function UnsignedMod(Const iValue : Integer; Const iRange : Integer) : Integer; Overload;
Function UnsignedMod(Const iValue : Int64; Const iRange : Int64) : Int64; Overload;

Function RemoveRemainder(Const iValue : Cardinal; Const iRange : Cardinal) : Cardinal; Overload;
Function RemoveRemainder(Const iValue : Integer; Const iRange : Integer) : Integer; Overload;
Function RemoveRemainder(Const iValue : Int64; Const iRange : Int64) : Int64; Overload;

Function GreatestCommonDivisor(Const iA, iB : Integer) : Integer;

type
  {$IFDEF FPC}
  TUCharArray = SysUtils.TUnicodeCharArray;
  {$ELSE}
  TUCharArray = SysUtils.TCharArray;
  {$ENDIF}


Type
  TCharArray = Array Of WideChar;
  TCharSet = Set Of AnsiChar;

  TLongString = String;
  PLongString = ^TLongString;

  TShortString = String[255];
  PShortString = ^TShortString;


  

Const
  cNull = #0;
  cBackspace = #8;
  cTab = #9;
  cVerticalTab = #11;
  cFeed = #10;
  cEnter = #13;
  cEscape = #27;
  cSpace = ' ';
  cReturn = cEnter + cFeed;

  setUniversal = [#0..#255];
  setControls = [#0..#31];
  setVertical = [cEnter, cFeed, cVerticalTab];
  setHorizontal = [cTab, cSpace, cBackspace];
  setWhitespace = setHorizontal + setVertical;
  setSigns = ['-', '+'];
  setNumbers = ['0'..'9'];
  setLowerCase = ['a'..'z'];
  setUpperCase = ['A'..'Z'];
  setAlphabet = setLowerCase + setUpperCase;
  setVowels = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'];
  setConsonants = setAlphabet - setVowels;
  setAlphanumeric = setNumbers + setAlphabet;
  setIntegers = setNumbers + setSigns;
  setReals = setNumbers + setSigns + ['.'];
  setHexadecimal = ['a'..'f', 'A'..'F'] + setNumbers;
  setSpecials = ['`', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '+', '=', '/', '?', '|', '<', '>', ',', ';', ':', '\', '"', '.', '[', ']', '{', '}', '-', '_', ''''];
  setFilename = setAlphanumeric + setSpecials - ['<', '>', '?', '*', '|', '\', '/', ':'] + [' '];
  setKeyboard = setSpecials + setAlphanumeric;


// NOTE: declared in the initialization.
Var
  UnicodeWhitespaceArray : TCharArray;

function clength(S : String) : cardinal; overload;
function clength(b : TBytes) : cardinal; overload;

Procedure StringAppend(Var sTarget : String; Const sSource : String; Const sDelimiter : String = ''); Overload;
Function StringCopy(Const sValue : String; iIndex, iCount : Integer) : String; Overload;
Function StringTruncateStart(Const sValue : String; iLength : Integer) : String; Overload;
Function StringCompare(Const sA, sB : String) : Integer; Overload;
Function StringCompare(Const sA, sB : String; Const iLength : Integer) : Integer; Overload;
Function StringCompareInsensitive(Const sA, sB : String) : Integer; Overload;
Function StringCompareSensitive(Const sA, sB : String) : Integer; Overload;
Function StringCompareInsensitive(Const sA, sB : String; Const iLength : Integer) : Integer; Overload;
Function StringCompareSensitive(Const sA, sB : String; Const iLength : Integer) : Integer; Overload;

Function StringEquals(Const sA, sB : String) : Boolean; Overload;
Function StringEquals(Const sA, sB : String; Const iLength : Integer) : Boolean; Overload;
Function StringEqualsSensitive(Const sA, sB : String) : Boolean; Overload;
Function StringEqualsSensitive(Const sA, sB : String; Const iLength : Integer) : Boolean; Overload;
Function StringEqualsInsensitive(Const sA, sB : String) : Boolean; Overload;
Function StringEqualsInsensitive(Const sA, sB : String; Const iLength : Integer) : Boolean; Overload;
Function StringFormat(Const sFormat : String; Const aArgs : Array Of Const) : String; Overload;
Function StringArrayIndexOfInsensitive(Const aNames : Array Of String; Const sName : String): Integer; Overload;
Function StringArrayIndexOfSensitive(Const aNames : Array Of String; Const sName : String): Integer; Overload;
Function StringArrayIndexOf(Const aNames : Array Of String; Const sName: String) : Integer; Overload;
Function StringArrayToString(Const aNames : Array Of String): String; Overload;
Function StringExists(Const sValue, sFind : String) : Boolean; Overload;
Function StringExists(Const sValue : String; Const aFind : TCharSet) : Boolean; Overload;
Function StringExistsInsensitive(Const sValue, sFind : String): Boolean; Overload;
Function StringExistsSensitive(Const sValue, sFind : String): Boolean; Overload;

Function StringArrayExistsInsensitive(Const aNames : Array Of String; Const sName : String) : Boolean; Overload;
Function StringArrayExistsSensitive(Const aNames : Array Of String; Const sName : String) : Boolean; Overload;
Function StringArrayExists(Const aNames : Array Of String; Const sName: String) : Boolean; Overload;
Function StringGet(Const sValue : String; iIndex : Integer) : Char; Overload;
Function StringStartsWith(Const sValue, sFind : String; sensitive : boolean = false) : Boolean; Overload;
Function StringStartsWithSensitive(Const sValue, sFind : String) : Boolean; Overload;
Function StringStartsWithInsensitive(Const sValue, sFind : String) : Boolean; Overload;
Function StringEndsWith(Const sValue, sFind : String; sensitive : boolean = false) : Boolean; Overload;
Function StringEndsWithSensitive(Const sValue, sFind : String) : Boolean; Overload;
Function StringEndsWithInsensitive(Const sValue, sFind : String) : Boolean; Overload;
Function StringStrip(Const sValue, sFind : String) : String; Overload;
Function StringStrip(Const sValue : String; Const aFind : TCharSet) : String; Overload;
Function StringStripDuplicates(Const sValue : String) : String; Overload;
Function StringCount(Const sValue : String; Const aFind : Char) : Integer; Overload;
Function StringSplit(Const sValue : String; Const aDelimiters : TCharSet; Var sLeft, sRight: String) : Boolean; Overload;
Function StringSplit(Const sValue, sDelimiter : String; Var sLeft, sRight: String) : Boolean; Overload;
Function StringSplitAnsi(Const sValue, sDelimiter : AnsiString; Var sLeft, sRight: AnsiString) : Boolean; Overload;
Function StringSplitRight(Const sValue : String; Const aDelimiters : TCharSet; Var sLeft, sRight: String) : Boolean; Overload;
Function StringSplitRight(Const sValue, sDelimiter : String; Var sLeft, sRight: String) : Boolean; Overload;
Function StringUpper(Const sValue : String) : String; Overload;
Function StringMultiply(cChar : Char; iCount : Integer) : String; Overload;

Function StringFind(Const sValue, sFind : String) : Integer; Overload;
Function StringFindSensitive(Const sValue, sFind : String) : Integer; Overload;
Function StringFindInsensitive(Const sValue, sFind : String) : Integer; Overload;
Function StringFind(Const sValue : String; aFind : TCharSet) : Integer; Overload;
Function StringFind(Const sValue, sFind : String; iStart : Integer) : Integer; Overload;
Function StringFind(Const sValue : String; aFind : TCharSet; iStart : Integer) : Integer; overload;

Function StringKeep(Const sValue : String; Const aFind : TCharSet) : String; Overload;
Function StringReplace(Const sValue, sFind, sReplace : String) : String; Overload;
Function StringReplace(Const sValue : String; Const aFind : TCharSet; cReplace : Char) : String; Overload;
Function StringReplace(Const sValue : String; Const aFind : TCharSet; Const sReplace : String) : String; Overload;
Function StringReplaceBefore(Const sValue, sFind, sReplace : String) : String; Overload;
Function StringReplaceAfter(Const sValue, sFind, sReplace : String) : String; Overload;
Function StringReplaceAll(Const sValue, sFind, sReplace : String; Const iStartPos : Integer = 1; Const iEndPos : Integer = 0) : String; Overload;
Function StringPadLeft(Const sValue : String; cPad : Char; iLength : Integer) : String; Overload;
Function StringPadRight(Const sValue : String; cPad : Char; iLength : Integer) : String; Overload;
Function StringSlice(Const sValue : String; iBegin, iEnd : Integer) : String; Overload;

Function StringTrimWhitespace(Const sValue : String) : String; Overload;
Function StringTrimWhitespaceRight(Const sValue : String) : String; Overload;
Function StringTrimWhitespaceLeft(Const sValue : String) : String; Overload;
Function StringTrimSetRight(Const sValue : String; aChars : TCharSet) : String; Overload;
Function StringTrimSetLeft(Const sValue : String; aChars : TCharSet) : String; Overload;
Function StringTrimSet(Const sValue : String; aChars : TCharSet) : String; Overload;

Function StringToBoolean(Const sValue : String) : Boolean; Overload;
Function StringToReal(Const sValue : String) : Real; Overload;
Function StringIsReal(Const sValue : String) : Boolean; Overload;

Function IntegerToString(Value : Integer) : String; Overload;

Function RealToString(Value : Real) : String; Overload;
Function BooleanToString(Value : Boolean) : String; Overload;
Function LCBooleanToString(Value : Boolean) : String;

Function StringToInteger32(Const sValue : String) : Integer; Overload;
Function StringToInteger32(Const sValue : String; iDefault : Integer) : Integer; Overload;
Function StringToInteger64(Const sValue : String) : Int64; Overload;

Function StringContainsOnly(Const sValue : String; Const aSet : TCharSet) : Boolean; Overload;
Function StringContainsAny(Const sValue : String; Const aSet : TCharSet) : Boolean; Overload;
Function StringIsCardinal16(Const sValue : String) : Boolean; Overload;
Function StringIsInteger16(Const sValue : String) : Boolean; Overload;
Function StringIsInteger32(Const sValue : String) : Boolean; Overload;
Function StringIsInteger64(Const sValue : String) : Boolean; Overload;
Function StringIsAlphabetic(Const sValue : String) : Boolean; Overload;
Function StringIsWhitespace(Const sValue : String) : Boolean; Overload;
function GetStringCell(const ADelimitedString: String; ACell: Cardinal; ADelimiter: String): String; Overload;
function SQLWrapString(const AStr: String): String;
function SQLWrapStrings(const AStr: String): String;
function AppendForwardSlash(const AStr: String): String;
Function DescribeBytes(i : int64) : String;

procedure CommaAdd(var AStr: String; AStrToAdd: String);
function RemoveQuotes(AStr: String; AUpperCaseString: Boolean = false): String;
function IsNumericString(st: String): Boolean;
function RemoveAccents(const aStr: String): String;

function charLower(ch : char) : char; overload;
function charLower(s : String) : char; overload;
function charUpper(ch : char) : char; overload;
function charUpper(s : String) : char; overload;
function capitalise(s : String) : String; overload;
function uncapitalise(s : String) : String; overload;
Function StringTitleCase(Const sValue: String): String; Overload;
Function StringTitleCase(Const sValue: String; Const sExceptions : Array of String): String; Overload;
Function StringToggleCase(Const sValue: String): String; Overload;
Function StringSentence(Const sValue : String) : String; Overload;

Function StringIsAlphanumeric(Const cValue : Char) : Boolean; Overload;
Function StringIsAlphanumeric(Const sValue : String) : Boolean; Overload;
Function StringIsNumeric(Const cValue : Char) : Boolean; Overload;
Function StringIsNumeric(Const sValue : String) : Boolean; Overload;
Function StringSingular(Const sValue : String) : String; Overload;
Function StringPlural(Const sValue : String; iValue : Integer = 0) : String; Overload;
Function StringCamel(Const sValue : String) : String; Overload;
Function StringReverseCamel(Const sValue : String) : String; Overload;

{$IFDEF FPC}
Function CharInSet(C: Char; Const CharSet: TCharSet): Boolean;
{$ENDIF}
{$IFDEF VER130}
Function CharInSet(C: AnsiChar; Const CharSet: TCharSet): Boolean; Overload;
Function CharInSet(C: WideChar; Const CharSet: TCharSet): Boolean; Overload;
{$ENDIF}

function TryStrToUINT64(StrValue:String; var uValue:UInt64 ):Boolean;
function StrToUINT64(Value:String):UInt64;
function StrToUInt64Def(Value:String; def : UInt64):UInt64;

function EnumIsValid(ATypeInfo: PTypeInfo; AIndex: Integer): Boolean;
function EnumStrIsValid(ATypeInfo: PTypeInfo; AValue : string): Boolean;
function EnumToString(ATypeInfo: PTypeInfo; AIndex: Integer): String;
function StringToEnum(ATypeInfo: PTypeInfo; const AStr: String): Integer;

Function StringIncludeAround(Const sText, sLeft, sRight : String) : String; Overload;
Function StringIncludeAround(Const sText, sSymbol : String) : String; Overload;
Function StringIncludeBefore(Const sText, sSymbol : String) : String; Overload;
Function StringIncludeBefore(Const sText : String; cSymbol : Char) : String; Overload;
Function StringIncludeAfter(Const sText : String; cSymbol : Char) : String; Overload;
Function StringIncludeAfter(Const sText, sSymbol : String) : String; Overload;

Function StringExcludeAround(Const sText, sSymbol : String) : String; Overload;
Function StringExcludeAround(Const sText, sLeft, sRight : String) : String; Overload;
Function StringExcludeBefore(Const sText : String; cSymbol : Char) : String; Overload;
Function StringExcludeBefore(Const sText, sSymbol : String) : String; Overload;
Function StringExcludeAfter(Const sText : String; cSymbol : Char) : String; Overload;
Function StringExcludeAfter(Const sText, sSymbol : String) : String; Overload;

Function CharArrayIndexOfInsensitive(Const aNames : Array Of Char; Const cName: Char): Integer; Overload;
Function CharArrayIndexOfSensitive(Const aNames : Array Of Char; Const cName: Char): Integer; Overload;
Function CharArrayIndexOf(Const aNames : Array Of Char; Const cName : Char): Integer; Overload;
Function CharArrayValid(Const aNames : Array Of Char; Const iIndex : Integer) : Boolean; Overload;

function jsonEscape(s : String; isString : boolean) : String;

function StringFindEndOfNumber(const s : String; index : integer) : integer;
function isAbsoluteUrl(s: String): boolean;


Const
  OID_LOINC = '2.16.840.1.113883.6.1';
  OID_SNOMED = '2.16.840.1.113883.6.96';
  OID_REGEX = '[0-2](\.(0|[1-9][0-9]*))*';
  UUID_REGEX = '[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}';

Function isOid(oid : String) : Boolean;
Function isUUid(oid : String) : Boolean;

function UriForKnownOid(oid : String) : String;

type
  TAnsiStringBuilder = Class(TFslObject)
    Private
      FContent : AnsiString;
      FLength : Integer;
      FBufferSize : integer;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function AsString : AnsiString;
      Function ToString : String; override;

      Procedure Clear;

      Procedure Append(ch : AnsiChar); Overload;
      Procedure Append(Const sStr : AnsiString); Overload;
      Procedure AppendLine(Const sStr : AnsiString); Overload;
      Procedure Append(Const iInt : Integer); Overload;
      Procedure AppendPadded(Const sStr : AnsiString; iCount : Integer; cPad : AnsiChar = ' ');
      Procedure AppendFixed(Const sStr : AnsiString; iCount : Integer; cPad : AnsiChar = ' ');
      Procedure Append(const bBytes : Array of Byte; amount : Integer); Overload;
      Procedure Append(Const oBuilder : TAnsiStringBuilder); Overload;
      Procedure AppendEOL;

      Procedure CommaAdd(Const sStr : AnsiString); Overload;

      Procedure AddByteAsBytes(iVal : Byte);
      Procedure AddWordAsBytes(iVal : word);
      Procedure AddCardinalAsBytes(iVal : Cardinal);
      Procedure AddInt64AsBytes(iVal : Int64);

      // index is zero based. zero means insert before first character
      Procedure Insert(Const sStr : AnsiString; iIndex : Integer); Overload;
      Procedure Insert(Const oBuilder : TAnsiStringBuilder; iIndex : Integer); Overload;

      Procedure Delete(iIndex, iLength : Integer);

      Property BufferSize : integer read FBufferSize write FBufferSize;
      Function IndexOf(Const sStr : AnsiString; bCase : Boolean = False) : Integer;
      Function LastIndexOf(Const sStr : AnsiString; bCase : Boolean = False) : Integer;

      Property Length : Integer Read FLength;
      Procedure Read(index : integer; var buffer; ilength : integer);
      Procedure Overwrite(index : integer; content : AnsiString);
  End;

type
  TAnsiCharSet = set of AnsiChar;

Function AnsiStringSplit(Const sValue : AnsiString; Const aDelimiters : TAnsiCharSet; Var sLeft, sRight: AnsiString) : Boolean;
Function AnsiPadString(const AStr: AnsiString; AWidth: Integer; APadChar: AnsiChar; APadLeft: Boolean): AnsiString;

{$IFDEF FPC}
  {$DEFINE NO_BUILDER}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE NO_BUILDER}
{$ENDIF}

Type
  {$IFDEF NO_BUILDER}
  TFslStringBuilder = Class(TFslObject)
    Private
      FContent : String;
      FLength : Integer;
      FBufferSize : integer;

      Procedure Append(Const bytes : TBytes); Overload;
      Procedure Insert(Const bytes : TBytes; iBytes : Integer; iIndex : Integer); Overload;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function AsString : String;
      Function ToString : String;

      Procedure Clear;

      Procedure Append(ch : Char); Overload;
      Procedure Append(Const sStr : String); Overload;
      Procedure AppendLine(Const sStr : String); Overload;
      Procedure Append(Const iInt : Integer); Overload;
      Procedure AppendPadded(Const sStr : String; iCount : Integer; cPad : Char = ' ');
      Procedure AppendFixed(Const sStr : String; iCount : Integer; cPad : Char = ' ');
      Procedure Append(const bBytes : Array of Byte; amount : Integer); Overload;
      Procedure Append(Const oBuilder : TFslStringBuilder); Overload;
      Procedure AppendEOL;

      Procedure CommaAdd(Const sStr : String); Overload;

      Procedure AddByteAsBytes(iVal : Byte);
      Procedure AddWordAsBytes(iVal : word);
      Procedure AddCardinalAsBytes(iVal : Cardinal);
      Procedure AddInt64AsBytes(iVal : Int64);

      // index is zero based. zero means insert before first character
      Procedure Insert(Const sStr : String; iIndex : Integer); Overload;
      Procedure Insert(Const oBuilder : TFslStringBuilder; iIndex : Integer); Overload;

      Procedure Delete(iIndex, iLength : Integer);

      Property BufferSize : integer read FBufferSize write FBufferSize;
      Function IndexOf(Const sStr : String; bCase : Boolean = False) : Integer;
      Function LastIndexOf(Const sStr : String; bCase : Boolean = False) : Integer;

      Property Length : Integer Read FLength;
      Procedure Read(index : integer; var buffer; ilength : integer);
      Procedure Overwrite(index : integer; content : String);
      function toString: String; override;
  End;


  {$ELSE}
  TFslStringBuilder = Class (TFslObject)
    Private
      FBuilder : TStringBuilder;

      Function GetLength : Integer;
      function GetAsString: String;
    Public
      constructor Create; Override;
      destructor Destroy; Override;
      Property AsString : String read GetAsString;

      Procedure Clear;

      Procedure Append(ch : Char); Overload;
      Procedure Append(Const sStr : String); Overload;
      Procedure Append(Const sStr : AnsiString); Overload;
      Procedure AppendLine(Const sStr : String); Overload;
      Procedure AppendPadded(Const sStr : String; iCount : Integer; cPad : Char = ' ');
      Procedure AppendFixed(Const sStr : String; iCount : Integer; cPad : Char = ' ');
      Procedure Append(Const oBuilder : TFslStringBuilder); Overload;
      Procedure AppendEOL;

      Procedure CommaAdd(Const sStr : String); Overload;

      Procedure AddByteAsBytes(iVal : Byte);
      Procedure AddWordAsBytes(iVal : word);
      Procedure AddCardinalAsBytes(iVal : Cardinal);
      Procedure AddInt64AsBytes(iVal : Int64);

      // index is zero based. zero means insert before first character
      Procedure Insert(Const sStr : String; iIndex : Integer); Overload;
      Procedure Insert(Const oBuilder : TFslStringBuilder; iIndex : Integer); Overload;

      Procedure Delete(iIndex, iLength : Integer);

      Procedure WriteToStream(aStream : TStream; encoding : TEncoding = nil); overload;

      Property Length : Integer Read GetLength;
      function ToString : String; override;
  End;
  {$ENDIF}

type
  TCommaBuilder = class (TFslObject)
  private
    list : TStringList;
    FIgnoreDuplicates: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure add(s : String);
    function asString : string;

    property ignoreDuplicates : Boolean read FIgnoreDuplicates write FIgnoreDuplicates;
  end;


//Type
//  TXmlEncodingMode = (xmlText, xmlAttribute, xmlCanonical);
//
//  TEolnOption = (eolnIgnore, eolnCanonical, eolnEscape);

Function EncodeNYSIIS(Const sValue : String) : String;
{
}

//Function EncodeXML(Const sValue : String; mode : TXmlEncodingMode; eoln : TEolnOption = eolnIgnore) : String; Overload;
//Function DecodeXML(Const sValue : String) : String; Overload;
Function EncodeQuotedString(Const sValue : String; Const cQuote : Char) : String; Overload;
Function EncodeMIME(Const sValue : String) : String; Overload;
Function DecodeMIME(Const sValue : String) : String; Overload;
Function DecodeMIMEURL(Const sValue : String) : String;  overload;
Function SizeOfDecodeHexadecimal(Const aBuffer; iSize : Cardinal) : Cardinal; Overload;
Function DecodeHexadecimal(Const cHigh, cLow : AnsiChar) : Byte; Overload;
Function DecodeHexadecimal(Const sValue : String) : TBytes; Overload;
Procedure DecodeHexadecimal(Const sValue : AnsiString; Var aBuffer; iCount : Integer); Overload;
Function StringIsHexadecimal(Const sValue : String) : Boolean;

Function SizeOfEncodeHexadecimal(Const aBuffer; iSize : Cardinal) : Cardinal; Overload;
Function EncodeHexadecimal(Const iValue : Byte) : AnsiString; Overload;
Function EncodeHexadecimal(Const aBuffer; iCount : Integer) : AnsiString; Overload;
Function EncodeHexadecimal(Const sValue : TBytes) : AnsiString; Overload;

// Quick String Encryption
function GetCryptKey(const AStr: String): Word;
// convert a string to an encyption key
function strEncrypt(const AStr: String; AKey: Word): String; // encrypt a string. Result is Mime Encoded so is still a valid string in many contexts (but is longer)
function strDecrypt(const AStr: String; AKey: Word): String; // decrypt a string encrypted with above procedure


  
Function SystemTemp : String;
Function SystemManualTemp : String;
Function ProgData : String;
Function UserFolder : String;
function tempFileName(prefix : String): String;


Type
  TInstallerCallback = procedure(IntParam: Integer; StrParam: String) of object;

type
  TColour = integer;
  THTMLColours = (
        hcAliceblue, hcAntiquewhite, hcAqua, hcAquamarine, hcAzure,
        hcBeige, hcBisque, hcBlack, hcBlanchedalmond, hcBlue,
        hcBlueviolet, hcBrown, hcBurlywood, hcCadetblue, hcChartreuse,
        hcChocolate, hcCoral, hcCornflowerblue, hcCornsilk, hcCrimson,
        hcCyan, hcDarkblue, hcDarkcyan, hcDarkgoldenrod, hcDarkgray,
        hcDarkgreen, hcDarkkhaki, hcDarkmagenta, hcDarkolivegreen, hcDarkorange,
        hcDarkorchid, hcDarkred, hcDarksalmon, hcDarkseagreen, hcDarkslateblue,
        hcDarkslategray, hcDarkturquoise, hcDarkviolet, hcdeeppink, hcDeepskyblue,
        hcDimgray, hcDodgerblue, hcFirebrick, hcFloralwhite, hcForestgreen,
        hcFuchsia, hcGainsboro, hcGhostwhite, hcGold, hcGoldenrod,
        hcGray, hcGreen, hcGreenyellow, hcHoneydew, hcHotpink,
        hcIndianred, hcIndigo, hcIvory, hcKhaki, hcLavendar,
        hcLavenderblush, hcLawngreen, hcLemonchiffon, hcLightblue, hcLightcoral,
        hcLightcyan, hcLightgoldenrodyellow, hcLightgreen, hcLightgrey, hcLightpink,
        hcLightsalmon, hcLightseagreen, hcLightskyblue, hcLightslategray, hcLightsteelblue,
        hcLightyellow, hcLime, hcLimegreen, hcLinen, hcMagenta,
        hcMaroon, hcMediumauqamarine, hcMediumblue, hcMediumorchid, hcMediumpurple,
        hcMediumseagreen, hcMediumslateblue, hcMediumspringgreen, hcMediumturquoise, hcMediumvioletred,
        hcMidnightblue, hcMintcream, hcMistyrose, hcMoccasin, hcNavajowhite,
        hcNavy, hcOldlace, hcOlive, hcOlivedrab, hcOrange,
        hcOrangered, hcOrchid, hcPalegoldenrod, hcPalegreen, hcPaleturquoise,
        hcPalevioletred, hcPapayawhip, hcPeachpuff, hcPeru, hcPink,
        hcPlum, hcPowderblue, hcPurple, hcRed, hcRosybrown,
        hcRoyalblue, hcSaddlebrown, hcSalmon, hcSandybrown, hcSeagreen,
        hcSeashell, hcSienna, hcSilver, hcSkyblue, hcSlateblue,
        hcSlategray, hcSnow, hcSpringgreen, hcSteelblue, hcTan,
        hcTeal, hcThistle, hcTomato, hcTurquoise, hcViolet,
        hcWheat, hcWhite, hcWhitesmoke, hcYellow, hcYellowGreen);

Const
  CURRENCY_MINIMUM = -922337203685477.58;
  CURRENCY_MAXIMUM = 922337203685477.58;

  clTransparent = -1;

  HTML_COLOUR_VALUES : Array [THTMLColours] Of TColour = (
      $00FFF8F0, $00D7EBFA, $00FFFF00, $00D4FF7F, $00FFFFF0,
      $00DCF5F5, $00C4E4FF, $00000000, $00CDEBFF, $00FF0000,
      $00E22B8A, $002A2AA5, $0087B8DE, $00A09E5F, $0000FF7F,
      $001E69D2, $00507FFF, $00ED9564, $00DCF8FF, $003C14DC,
      $00FFFF00, $008B0000, $008B8B00, $000B86B8, $00A9A9A9,
      $00006400, $006BB7BD, $008B008B, $002F6B55, $00008CFF,
      $00CC3299, $0000008B, $007A96E9, $008FBC8F, $008B3D48,
      $004F4F2F, $00D1CE00, $00D30094, $009314FF, $00FFBF00,
      $00696969, $00FF901E, $002222B2, $00F0FAFF, $00228B22,
      $00FF00FF, $00DCDCDC, $00FFF8F8, $0000D7FF, $0020A5DA,
      $00808080, $00008000, $002FFFAD, $00F0FFF0, $00B469FF,
      $005C5CCD, $0082004B, $00F0FFFF, $008CE6F0, $00FAE6E6,
      $00F5F0FF, $0000FC7C, $00CDFAFF, $00E6D8AD, $008080F0,
      $00FFFFE0, $00D2FAFA, $0090EE90, $00D3D3D3, $00C1B6FF,
      $007AA0FF, $00AAB220, $00FACE87, $00998877, $00DEC4B0,
      $00E0FFFF, $0000FF00, $0032CD32, $00E6F0FA, $00FF00FF,
      $00000080, $00AACD66, $00CD0000, $00D355BA, $00D87093,
      $0071B33C, $00EE687B, $009AFA00, $00CCD148, $008515C7,
      $00701919, $00FAFFF5, $00E1E4FF, $00B5E4FF, $00ADDEFF,
      $00800000, $00E6F5FD, $00008080, $00238E68, $0000A5FF,
      $000045FF, $00D670DA, $00AAE8EE, $0098FB98, $00EEEEAF,
      $009370D8, $00D5EFFF, $00B9DAFF, $003F85CD, $00CBC0FF,
      $00DDA0DD, $00E6E0B0, $00800080, $000000FF, $008F8FBC,
      $00E16941, $0013458B, $007280FA, $0060A4F4, $00578B2E,
      $00EEF5FF, $002D52A0, $00C0C0C0, $00EBCE87, $00CD5A6A,
      $00908070, $00FAFAFF, $007FFF00, $00B48246, $008CB4D2,
      $00808000, $00D8BFD8, $004763FF, $00D0E040, $00EE82EE,
      $00B3DEF5, $00FFFFFF, $00F5F5F5, $0000FFFF, $0032CD9A);

  HTML_COLOUR_NAMES : Array [THTMLColours] Of String = (
      'Aliceblue', 'Antiquewhite', 'Aqua', 'Aquamarine', 'Azure',
      'Beige', 'Bisque', 'Black', 'Blanchedalmond', 'Blue',
      'Blueviolet', 'Brown', 'Burlywood', 'Cadetblue', 'Chartreuse',
      'Chocolate', 'Coral', 'Cornflowerblue', 'Cornsilk', 'Crimson',
      'Cyan', 'Darkblue', 'Darkcyan', 'Darkgoldenrod', 'Darkgray',
      'Darkgreen', 'Darkkhaki', 'Darkmagenta', 'Darkolivegreen', 'Darkorange',
      'Darkorchid', 'Darkred', 'Darksalmon', 'Darkseagreen', 'Darkslateblue',
      'Darkslategray', 'Darkturquoise', 'Darkviolet', 'deeppink', 'Deepskyblue',
      'Dimgray', 'Dodgerblue', 'Firebrick', 'Floralwhite', 'Forestgreen',
      'Fuchsia', 'Gainsboro', 'Ghostwhite', 'Gold', 'Goldenrod',
      'Gray', 'Green', 'Greenyellow', 'Honeydew', 'Hotpink',
      'Indianred', 'Indigo', 'Ivory', 'Khaki', 'Lavendar',
      'Lavenderblush', 'Lawngreen', 'Lemonchiffon', 'Lightblue', 'Lightcoral',
      'Lightcyan', 'Lightgoldenrodyellow', 'Lightgreen', 'Lightgrey', 'Lightpink',
      'Lightsalmon', 'Lightseagreen', 'Lightskyblue', 'Lightslategray', 'Lightsteelblue',
      'Lightyellow', 'Lime', 'Limegreen', 'Linen', 'Magenta',
      'Maroon', 'Mediumauqamarine', 'Mediumblue', 'Mediumorchid', 'Mediumpurple',
      'Mediumseagreen', 'Mediumslateblue', 'Mediumspringgreen', 'Mediumturquoise', 'Mediumvioletred',
      'Midnightblue', 'Mintcream', 'Mistyrose', 'Moccasin', 'Navajowhite',
      'Navy', 'Oldlace', 'Olive', 'Olivedrab', 'Orange',
      'Orangered', 'Orchid', 'Palegoldenrod', 'Palegreen', 'Paleturquoise',
      'Palevioletred', 'Papayawhip', 'Peachpuff', 'Peru', 'Pink',
      'Plum', 'Powderblue', 'Purple', 'Red', 'Rosybrown',
      'Royalblue', 'Saddlebrown', 'Salmon', 'Sandybrown', 'Seagreen',
      'Seashell', 'Sienna', 'Silver', 'Skyblue', 'Slateblue',
      'Slategray', 'Snow', 'Springgreen', 'Steelblue', 'Tan',
      'Teal', 'Thistle', 'Tomato', 'Turquoise', 'Violet',
      'Wheat', 'White', 'Whitesmoke', 'Yellow', 'YellowGreen');

  HTML_COLOUR_TITLES : Array [THTMLColours] Of String = (
      'Alice Blue', 'Antique White', 'Aqua', 'Aquamarine', 'Azure',
      'Beige', 'Bisque', 'Black', 'Blanched Almond', 'Blue',
      'Blue Violet', 'Brown', 'Burlywood', 'Cadet Blue', 'Chartreuse',
      'Chocolate', 'Coral', 'Cornflower Blue', 'Cornsilk', 'Crimson',
      'Cyan', 'Dark Blue', 'Dark Cyan', 'Dark Goldenrod', 'Dark Gray',
      'Dark Green', 'Dark Khaki', 'Dark Magenta', 'Dark Olive Green', 'Dark Orange',
      'Dark Orchid', 'Dark Red', 'Dark Salmon', 'Dark Sea Green', 'Dark Slate Blue',
      'Dark Slate Gray', 'Dark Turquoise', 'Dark Violet', 'Deep Pink', 'Deep Sky Blue',
      'Dim Gray', 'Dodger Blue', 'Firebrick', 'Floral White', 'Forest Green',
      'Fuchsia', 'Gainsboro', 'Ghost White', 'Gold', 'Goldenrod',
      'Gray', 'Green', 'Green Yellow', 'Honeydew', 'Hot Pink',
      'Indian Red', 'Indigo', 'Ivory', 'Khaki', 'Lavendar',
      'Lavender Blush', 'Lawn Green', 'Lemon Chiffon', 'Light Blue', 'Light Coral',
      'Light Cyan', 'Light Goldenrod Yellow', 'Light Green', 'Light Grey', 'Light Pink',
      'Light Salmon', 'Light Sea Green', 'Light Sky Blue', 'Light Slate Gray', 'Light Steel Blue',
      'Light Yellow', 'Lime', 'Lime Green', 'Linen', 'Magenta',
      'Maroon', 'Medium Aquamarine', 'Medium Blue', 'Medium Orchid', 'Medium Purple',
      'Medium Seagreen', 'Medium Slate Blue', 'Medium Spring Green', 'Medium Turquoise', 'Medium Violet Red',
      'Midnight Blue', 'Mint Cream', 'Misty Rose', 'Moccasin', 'Navajo White',
      'Navy', 'Old Lace', 'Olive', 'Olive Drab', 'Orange',
      'Orange Red', 'Orchid', 'Pale Goldenrod', 'Pale Green', 'Pale Turquoise',
      'Pale Violet Red', 'Papaya Whip', 'Peach Puff', 'Peru', 'Pink',
      'Plum', 'Powder Blue', 'Purple', 'Red', 'Rosy Brown',
      'Royal Blue', 'Saddle Brown', 'Salmon', 'Sandy Brown', 'Sea Green',
      'Seashell', 'Sienna', 'Silver', 'Sky Blue', 'Slate Blue',
      'Slate Gray', 'Snow', 'Spring Green', 'Steel Blue', 'Tan',
      'Teal', 'Thistle', 'Tomato', 'Turquoise', 'Violet',
      'Wheat', 'White', 'White Smoke', 'Yellow', 'Yellow Green');

Type
 TColourRatio = Record
    Blue  : Real;
    Green : Real;
    Red   : Real;
    Alpha : Real;
  End;

  TColourParts = Packed Record
    Red : Byte;
    Green : Byte;
    Blue : Byte;
    Alpha : Byte;
  End;

Function ColourCompose(iRed, iGreen, iBlue, iAlpha : Byte) : TColour; Overload;
Function HTMLColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour; Overload;
Function HTMLColourStringToColour(Const sColour : String) : TColour; Overload;
Function StringIsHTMLColour(Const sColour : String) : Boolean; Overload;
Function HTMLEncodedColourStringToColour(Const sColour : String) : TColour; Overload;
Function HTMLEncodedColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour; Overload;
Function ColourToHTMLColourString(Const iColour : TColour) : String; Overload;
Function ColourToHTMLColourTitleString(Const iColour : TColour) : String; Overload;
Function ColourToXMLColourString(Const iColour : TColour) : String; Overload;
Function XMLColourStringToColour(Const sColour : String) : TColour; Overload;
Function XMLColourStringToColourOrDefault(Const sColour : String; Const aDefault : TColour) : TColour; Overload;
Function ColourMakeGrey(iColour : TColour) : TColour; Overload;
Function ColourMultiply(iColour : TColour; Const aRatio : TColourRatio) : TColour; Overload;
Function ColourMultiply(iColour : TColour; Const iRatio : Real) : TColour; Overload;
Function ColourInverse(iColour : TColour) : TColour;
Function ColourToString(iColour : TColour) : String; Overload;



type
  TFileHandle = Record
    Value : Cardinal;
  End;

  TFileVersion = Record
    Major : Integer;
    Minor : Integer;
    Release : Integer;
    Build : Integer;
  End;

Function FileGetModified(Const sFileName : String) : TDateTime; Overload;
  {$IFDEF MSWINDOWS}
procedure FileSetModified(Const sFileName : String; time : TDateTime); Overload;
{$ENDIF}

Function FileExists(Const sFilename : String) : Boolean; Overload;
Function FileDelete(Const sFilename : String) : Boolean; Overload;
Function FileHandleInvalid : TFileHandle; Overload;
Function FileHandleIsValid(Const aFileHandle : TFileHandle) : Boolean; Overload;
Function FileHandleOpen(Const aValue : Cardinal) : TFileHandle; Overload;
Procedure FileHandleClose(Var aFileHandle : TFileHandle); Overload;
Function PathFolder(Const sFilename : String) : String; Overload;
Function ForceFolder(dir: String): Boolean;
Function FolderDelete(Const sFolder : String) : Boolean; Overload;

Function PathFilename(Const sFilename : String) : String; Overload;
Function PathTitle(Const sFilename : String) : String; Overload;
Function PathExtension(Const sFilename : String) : String; Overload;
Function FolderExists(Const sFolder : String) : Boolean;

Function FileSize(Const sFileName : String) : Int64; Overload;
function Path(parts : array of String) : String;
function URLPath(parts : array of String) : String;
function makeRelativePath(path, base : String): String;
function makeAbsolutePath(fn, base : String): String;

Function CreateGUID : TGUID;
Function GUIDToString(Const aGUID : TGUID) : String;
Function StringToGUID(Const sGUID: String) : TGUID;
Function NilGUID : TGUID;
Function IsNilGUID(const guid : TGUID) : boolean;
Function GUIDAsOID(Const aGUID : TGUID) : String;
Function IsGuid(s : String): Boolean;
function NewGuidURN : String;
function NewGuidId : String;

// Always pass the pointer
Procedure MemoryCreate(Var pBuffer; iSize : Integer);
Procedure MemoryResize(Var pBuffer; iOldSize, iNewSize : Integer);
Procedure MemoryDestroy(Var pBuffer; iSize : Integer);
Function MemoryCompare(pA, pB : Pointer; iLength : Integer) : Integer;
Procedure MemoryZero(Const pBuffer : Pointer; iSize : Integer);
Procedure MemoryFill(Const pBuffer : Pointer; iSize : Integer);
Procedure MemoryMove(Const aSource, aTarget : Pointer; iSize : Integer);
Function MemoryToString(pData : Pointer; iPosition, iLength : Integer) : AnsiString; Overload;
Function MemoryToString(pData : Pointer; iLength : Integer) : AnsiString; Overload;


Function HashStringToCode32(Const sValue : String) : Integer;
Function HashStringToCode64(Const sValue : String) : Int64;
Function HashIntegerToCode32(Const iValue : Integer) : Integer;
Function HashInteger64ToCode32(Const iValue : Int64) : Integer;


Function ErrorAsString : String; Overload;
Function ErrorAsString(iError : Integer) : String; Overload;
Function ErrorAsMessage(iError : Integer) : String;
Function ErrorAsNumber : Integer; Overload;

Function RandomBoolean : Boolean; Overload;
Function RandomDateTime(Const aLowest, aHighest : TDateTime) : TDateTime; Overload;
Function RandomInteger64(Const iLowest, iHighest : Int64) : Int64; Overload;
Function RandomInteger(Const iUpper : Integer) : Integer; Overload;
Function RandomInteger(Const iLowest, iHighest : Integer) : Integer; Overload;
Function RandomReal(Const iUpper : Real) : Real; Overload;
Function RandomReal(Const iLowest, iHighest : Real) : Real; Overload;
Function RandomAlphabeticString(Const iLength : Integer) : String; Overload;
Function RandomAlphabeticCharacter : Char; Overload;


Type
  TMediaSoundBeepType = (MediaSoundBeepTypeAsterisk, MediaSoundBeepTypeExclamation, MediaSoundBeepTypeHand, MediaSoundBeepTypeQuestion, MediaSoundBeepTypeOk);

  {$IFDEF MSWINDOWS}

Procedure SoundPlay(Const sFilename : String); Overload;
Procedure SoundStop(Const sFilename : String); Overload;
Procedure SoundBeepType(Const aSoundBeepType : TMediaSoundBeepType); Overload;
Procedure SoundBeepAsterisk; Overload;
Procedure SoundBeepExclamation; Overload;
Procedure SoundBeepOK; Overload;
Procedure SoundBeepRange(Const iFrequency, iDuration : Cardinal); Overload;
{$ENDIF}


Type
  TCurrency = Currency;
  TCurrencyCents = Int64;
  TCurrencyDollars = Int64;
 {$IFDEF FPC}
 TSystemMemory = Windows.MEMORYSTATUS;
 {$ELSE}
{$IFDEF CPUx86}
  TSystemMemory = Record   // Windows.MEMORYSTATUS
    Length : Cardinal;
    TotalLoad : Cardinal;
    TotalPhysical : Cardinal;
    AvailablePhysical : Cardinal;
    TotalPageFile : Cardinal;
    AvailablePageFile : Cardinal;
    TotalVirtual : Cardinal;
    AvailableVirtual : Cardinal;
  End;
{$ELSE}
  {$IFDEF CPUx64}
  TSystemMemory = Record
    Length : Cardinal;
    MemoryLoad : Cardinal;
    TotalPhysical : UInt64;
    AvailablePhysical : UInt64;
    TotalPageFile : UInt64;
    AvailablePageFile : UInt64;
    TotalVirtual : UInt64;
    AvailableVirtual : UInt64;
    AvailableExtendedVirtual : UInt64;
  End;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}


Function CurrencyCompare(Const rA, rB : TCurrency) : Integer; Overload;
Function CurrencyCompare(Const rA, rB, rThreshold : TCurrency) : Integer; Overload;
Function CurrencyEquals(Const rA, rB, rThreshold : TCurrency) : Boolean; Overload;
Function CurrencyEquals(Const rA, rB : TCurrency) : Boolean; Overload;
Function CurrencyEqualsZero(Const rAmount : TCurrency) : Boolean;

Function CurrencyToString(Const rValue : TCurrency; iDigits : Integer = 2) : String;
Function StringToCurrency(Const sValue : String; Const rDefault : TCurrency) : TCurrency; Overload;
Function StringToCurrency(Const sValue : String) : TCurrency; Overload;

Function CentsToCurrency(Const iCents : TCurrencyCents) : TCurrency;
Function CurrencyToCents(Const iCurrency : TCurrency) : TCurrencyCents;

Function CurrencyDifference(Const iAmount1, iAmount2 : TCurrency) : TCurrency;
Function CurrencyRoundUp(Const iValue : TCurrency; Const iRoundCents : Integer) : TCurrency;
Function CurrencyRoundDown(Const iValue : TCurrency; Const iRoundCents : Integer) : TCurrency;
Function CurrencyRoundNearest(Const iValue : TCurrency; Const iRoundCents : Integer) : TCurrency;
Function CurrencyRoundBankers(Const rValue : TCurrency; Const iRoundCents : Integer = 1) : TCurrency;

Function StringToCents(Const sValue : String) : TCurrencyCents;
Function CentsToString(Const iCents : TCurrencyCents) : String;

Function StringIsCurrency(Const sValue : String) : Boolean;

Function CurrencySymbol : String;

Function CurrencyMin(Const rA, rB : TCurrency) : TCurrency;
Function CurrencyMax(Const rA, rB : TCurrency) : TCurrency;

Function CurrencyAdjusted(Const rAmount, rGap : TCurrency; Const rPercentage : Real) : TCurrency;
Function CurrencyApplyPercentages(Const rAmount : TCurrency; Const rPercentageBefore, rPercentageAfter : Real) : TCurrency;
Function CurrencyTruncateToCents(Const rAmount : TCurrency) : TCurrency;
Function CurrencyTruncateToDollars(Const rAmount : TCurrency) : TCurrency;

  {$IFDEF MSWINDOWS}

Function SystemIsWindowsNT : Boolean;
Function SystemIsWindows2K : Boolean;
Function SystemIsWindowsXP : Boolean;
Function SystemIsWindowsVista : Boolean;
Function SystemIsWindows7 : Boolean;

Function SystemIsWin64 : Boolean;
Function SystemName : String;
Function SystemPlatform : String;
Function SystemArchitecture : String;
Function SystemResolution : String;
Function SystemBootStatus : String;
Function SystemInformation : String;
Function SystemPath : String;
Function SystemTimezone : String;
Function SystemLanguage : String;
Function SystemProcessors : Cardinal;
Function SystemPageSize : Cardinal;
{$IFDEF CPUx86}
Function SystemMemory : TSystemMemory;
{$ELSE}
  {$IFDEF CPUx64}
Function SystemMemory : TSystemMemory;
  {$ENDIF}
{$ENDIF}
Function SystemProcessorName : String;
Function SystemProcessorIdentifier : String;
Function SystemProcessorVendorIdentifier : String;
Function ProcessName : String; Overload;

Type
  TInternetHost = Cardinal;
  TInternetPort = Word;

  TIP = TInternetHost;
  TPort = TInternetPort;

Function IPToString(Const iValue : TIP) : String; Overload;
Function HostIP : TIP;
Function HostName : String;
Function LogonName : String;
Function LocalComputerName : String;
Function RemoteComputerName : String;
Function IsRemoteSession : Boolean;
Function DomainName : String;
Function DomainLogonName : String;
Function ProxyServer : String;

Function MonitorInfoFromRect(aRect : TRect): TMonitorInfo;
{$ENDIF}

type
  TFileLauncher = class
    class procedure Open(const FilePath: string);
  end;


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
  TDurationMS = Int64;             // Number of milliseconds.
  TDuration = Double;             // TDateTime based duration
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
    function truncToDay : TDateTimeEx;

    function fixPrecision(precision : TDateTimeExPrecision) : TDateTimeEx;

    function Local : TDateTimeEx;
    function UTC : TDateTimeEx;
    function Min : TDateTimeEx;
    function Max : TDateTimeEx;
    function AsTz(ihr, imin : Integer):TDateTimeEx; // this date and time in the specified timezone.

    function IncrementMonth: TDateTimeEx;
    function IncrementYear: TDateTimeEx;
    function IncrementWeek: TDateTimeEx;
    function IncrementDay: TDateTimeEx;
    function add(length : TDuration) : TDateTimeEx;
    function subtract(length : TDuration) : TDateTimeEx;
    function difference(other : TDateTimeEx) : TDuration;
    function lessPrecision: TDateTimeEx;

    function equal(other : TDateTimeEx) : Boolean; overload; // returns true if the timezone, FPrecision, and actual instant are the same
    function equal(other : TDateTimeEx; precision : TDateTimeExPrecision) : Boolean; overload; // returns true if the timezone, FPrecision, and actual instant are the same
    function sameTime(other : TDateTimeEx) : Boolean; // returns true if the specified instant is the same allowing for specified FPrecision - corrects for timezone
    function after(other : TDateTimeEx; inclusive : boolean):boolean; // returns true if this is after other
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

    class operator Trunc(a : TDateTimeEx) : TDateTimeEx;
    class operator Equal(a: TDateTimeEx; b: TDateTimeEx) : Boolean;
    class operator NotEqual(a: TDateTimeEx; b: TDateTimeEx) : Boolean;
    class operator GreaterThan(a: TDateTimeEx; b: TDateTimeEx) : Boolean;
    class operator GreaterThanOrEqual(a: TDateTimeEx; b: TDateTimeEx) : Boolean;
    class operator LessThan(a: TDateTimeEx; b: TDateTimeEx) : Boolean;
    class operator LessThanOrEqual(a: TDateTimeEx; b: TDateTimeEx) : Boolean;
    class operator Add(a: TDateTimeEx; b: TDateTime) : TDateTimeEx; // date time is duration here
    class operator Subtract(a: TDateTimeEx; b: TDateTime) : TDateTimeEx; // date time is duration here
    class operator Subtract(a: TDateTimeEx; b: TDateTimeEx) : TDateTime; // date time is duration here
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
Function StringToDuration(Const sValue : String) : TDurationMS; Overload;
Function StringToDuration(Const sValue : String; Out aValue : TDurationMS) : Boolean; Overload;
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

Function DurationToDaysString(Const aValue : TDurationMS) : String; Overload;
Function DurationToHoursString(Const aValue : TDurationMS) : String; Overload;
Function DurationToMinutesString(Const aValue : TDurationMS) : String; Overload;
Function DurationToSecondsString(Const aValue : TDurationMS) : String; Overload;
Function DurationToShortSecondsString(Const aValue : TDurationMS) : String; Overload;
Function DurationToMillisecondsString(Const aValue : TDurationMS) : String; Overload;


function FileTimeToDateTime(Time: TFileTime; bTz : boolean): TDateTime;
Function TimeZoneOffset : Integer; Overload;
Function TimeZoneOffset(Const aDateTime : TDateTime; Const aTimeZoneInformation : TTimeZoneInformation) : Integer; Overload;
Function TimeZone : TTimeZoneCode; Overload;
Function CreateTimeZoneInformation(Const aTimeZone : TTimeZoneCode) : TTimeZoneInformation;
Procedure DestroyTimeZoneInformation(Var aTimeZoneInformation : TTimeZoneInformation);
{$ENDIF}
function DateTimeToUnix(ConvDate: TDateTime): Longint;
function UnixToDateTime(USec: Longint): TDateTime;
  
Const
  INTEGER_PRECISION = 24;
  
Type
{TFslDecimal
    Precision aware Decimal implementation. Any size number with any number of significant digits is supported.

    Note that operations are precision aware operations. Note that whole numbers are assumed to have
    unlimited precision. For example:
      2 x 2 = 4
      2.0 x 2.0 = 4.0
      2.00 x 2.0 = 4.0
    and
     10 / 3 = 3.33333333333333333333333333333333333333333333333
     10.0 / 3 = 3.33
     10.00 / 3 = 3.333
     10.00 / 3.0 = 3.3
     10 / 3.0 = 3.3

    Addition
      2 + 0.001 = 2.001
      2.0 + 0.001 = 2.0

    Note that the string representation is precision limited, but the internal representation
    is not.
}
  TFslDecimalStatus = (sdsUnknown, sdsUndefined, sdsNumber, sdsInfinite); // undefined is the mathematical concept - different to unknown

  TFslDecimal = record
  // data members - never use these - use the helper methods instead
  private
    FStatus : TFslDecimalStatus;
    FNegative : Boolean;
    FDigits : String;
    FDecimal : integer;
    FPrecision : integer;
    FScientific : Boolean;  // remember the representation

  // operator overloading
  public
    class operator Implicit(a : TFslDecimal) : String;
    class operator Implicit(a : TFslDecimal) : Double;
    class operator Implicit(a : TFslDecimal) : Integer;
    class operator Explicit(a : String) : TFslDecimal;
    class operator Explicit(a : Double) : TFslDecimal;
    class operator Explicit(a : Integer) : TFslDecimal;
    class operator Negative(a : TFslDecimal) : TFslDecimal;
    class operator Positive(a : TFslDecimal) : TFslDecimal;
    class operator Trunc(a : TFslDecimal) : TFslDecimal;
    class operator Equal(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator NotEqual(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator GreaterThan(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator GreaterThanOrEqual(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator LessThan(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator LessThanOrEqual(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator Add(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator Subtract(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator Multiply(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator Divide(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator IntDivide(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator Modulus(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
  end;

  TFslDecimalHelper = record helper for TFslDecimal
  private
    Procedure SetValue(sValue : String);
    Procedure SetValueDecimal(sValue : String);
    Procedure SetValueScientific(sValue : String);
    Function GetValue: String;
    Function GetValueDecimal : String;
    Function GetValueScientific : String;

    Function DoAdd(oOther : TFslDecimal) : TFslDecimal;
    Function DoSubtract(oOther : TFslDecimal) : TFslDecimal;

    Function StringAddition(s1, s2 : String):String;
    Function StringSubtraction(s1, s2 : String):String;

    Function dig(c : Char) : integer;
    Function cdig(i : integer) : Char;
    function GetPrecision: integer;
    procedure SetPrecision(const Value: integer);
  public
    // constructors
    class Function ValueOf(value : String) : TFslDecimal; Overload; static; inline;
    class Function ValueOf(value : Integer) : TFslDecimal; Overload; static; inline;
    class Function ValueOf(value : int64) : TFslDecimal; Overload; static; inline;
    class Function ValueOf(value : Double) : TFslDecimal; Overload; static; inline;
    class Function Create(value : String) : TFslDecimal; Overload; static; inline;
    class Function Create(value : Integer) : TFslDecimal; Overload; static; inline;
    class Function Create(value : int64) : TFslDecimal; Overload; static; inline;
    class Function Create(value : Double) : TFslDecimal; Overload; static; inline;

    function Link : TFslDecimal;
    function Clone : TFslDecimal;

    // utility functions
    class Function Equals(oOne, oTwo : TFslDecimal) : Boolean; overload; static; inline;
    class Function Compares(oOne, oTwo : TFslDecimal) : Integer; overload; static;

    class Function makeZero : TFslDecimal; overload; static; inline;
    class Function makeOne : TFslDecimal; overload; static; inline;
    class Function makeInfinity : TFslDecimal; overload; static; inline;
    class Function makeUndefined : TFslDecimal; overload; static; inline;
    class function makeNull : TFslDecimal; static; inline;

    class function CheckValue(sValue : String) : boolean; static; inline;
    class function CheckValueDecimal(sValue : String) : boolean; static; inline;
    class function CheckValueScientific(sValue : String) : boolean; static; inline;

    // accessors
    property value : String read GetValue write SetValue;
    property precision : integer read GetPrecision write SetPrecision;

    Property AsString : String read GetValue;
    Function AsCardinal : Cardinal;
    Function AsInteger : Integer;
    Function AsInt64 : Int64;
    Property AsScientific : String read GetValueScientific;
    Property AsDecimal : String read GetValuedecimal;
    function AsDouble : Double;

    {
     This method exists to populate a database with relatively accurately searchable decimal values wher ethe search is by string semantics
    }
    function normaliseDecimal(digits, decimals : integer; defUp : boolean) : String;

    // properties
    Function IsZero : Boolean;
    Function IsNegative : boolean;
    Function IsOne : Boolean;
    Function IsInfinite : Boolean;
    Function IsWholeNumber : Boolean;
    function IsNull : boolean;
    function IsUndefined : boolean;
    function isANumber : boolean;
    Function upperBound : TFslDecimal; // the upper bound given the imprecision. for example, if the value is 1,0, then the upper bound is 1.05.
    Function lowerBound : TFslDecimal; // the lower bound given the imprecision. for example, if the value is 1,0, then the lower bound is 0.95.
    Function immediateUpperBound : TFslDecimal; // the upper bound given the face value. for example, if the value is 1.0, then the upper bound is 1.000000000000000000000000001.
    Function immediateLowerBound : TFslDecimal;  // the immediate lower bound given the face value. for example, if the value is 1,0, then the upper bound is 0.99999999999999999999999999999999.

    // operators
    Function Trunc : TFslDecimal;
    Function Multiply(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function Multiply(iOther : Integer) : TFslDecimal; Overload;
    Function Divide(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function Divide(iOther : Integer) : TFslDecimal; Overload;
    {
      return the number of times other will "fit into" this number. This is usually called
      Integer Division, but in this implementation, neither this nor other needs to be
      a whole number; however the result of this operation will always be a whole number
    }
    Function DivInt(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function DivInt(iOther : Integer) : TFslDecimal; Overload;
    Function Modulo(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function Modulo(iOther : Integer) : TFslDecimal; Overload;
    Function Add(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function Add(iOther : Integer) : TFslDecimal; Overload;
    Function Subtract(iOther : Integer) : TFslDecimal; Overload;
    Function Subtract(oOther : TFslDecimal) : TFslDecimal; Overload;
    function Abs: TFslDecimal; overload;
    function Negated: TFslDecimal; overload;
    Function Equals(oOther : TFslDecimal) : Boolean; overload;
    Function Compares(oOther : TFslDecimal) : Integer; overload;

end;


Function GUIDAsOIDWrong(Const aGUID : TGUID) : String;
Function GUIDAsOIDRight(Const aGUID : TGUID) : String;

function StringIsDecimal(s : String) : Boolean;

type
  TByte = Byte;

  TFslBytesBuilder = Class (TFslObject)
    Private
      FContent : TBytes;
      FLength : Cardinal;
      FBufferSize : Cardinal;
    Public
      constructor Create; Override;
      Function AsBytes : TBytes;

      Procedure Clear;

      Procedure Append(ch : AnsiChar); Overload;
      Procedure Append(b : Byte); Overload;
      Procedure Append(Const bytes : TBytes); Overload;
      Procedure Append(mem : Pointer; len : word); Overload;

      Function EndsWith(aBytes : TBytes) : Boolean;
      Property Length : Cardinal Read FLength;

      Procedure AddWord(val : word);
      Procedure AddCardinal(val : cardinal);
      Procedure AddInteger(val : integer);
      Procedure AddUInt64(val : UInt64);
      Procedure AddString(val : String);
      procedure AddUtf8(val : String);
      Procedure AddAnsiString(val : AnsiString);
      Procedure addBase64(val : TBytes);

      procedure Read(index : cardinal; var buffer; ilength : cardinal);

      Procedure WriteWord(index : Cardinal; val : word);
      Procedure WriteCardinal(index : Cardinal; val : cardinal);
      Procedure WriteUInt64(index : Cardinal; val : UInt64);
      Procedure WriteString(index : Cardinal; val : String);
  End;


Function BytesContains(bytes : TBytes; value : TByte): Boolean;
Function BytesAdd(bytes1, bytes2 : TBytes) : TBytes; Overload;
Function BytesAdd(bytes : TBytes; byte : TByte) : TBytes; Overload;
function CompareBytes(bytes1, bytes2 : TBytes) : Integer; Overload;
function SameBytes(bytes1, bytes2 : TBytes) : Boolean; Overload;
{$IFDEF UT}
function CompareBytes(bytes1 : TBytes; bytes2 : AnsiString) : Integer; Overload;
function SameBytes(bytes1 : TBytes; bytes2 : AnsiString) : Boolean; Overload;
{$ENDIF}
Function AnsiStringAsBytes(s : AnsiString) : TBytes;
Function StringAsBytes(s : String) : TBytes;
Function BytesAsString(a : TBytes) : String;
Function BytesAsAnsiString(a : TBytes) : AnsiString;
Function BytesAsMime(a : TBytes) : String;
Function BytesReplace(const a, OldPattern, NewPattern: TBytes): TBytes;
Function Bytes(a : Array of byte) : TBytes;
Function Fillbytes(b : byte; count : Integer): TBytes; Overload;
Function Fillbytes(init : TBytes; b : byte; count : Integer): TBytes; Overload;
Function BytesStartsWith(bytes, find : TBytes) : Boolean;
Function BytesSplit(Const sValue, sDelimiter : TBytes; Var sLeft, sRight: TBytes) : Boolean;
function StreamToBytes(AStream: TStream): TBytes;
function FileToBytes(filename : string) : TBytes;
procedure BytesToFile(bytes : TBytes; filename : string);
Function BytePos(find : Byte; source : TBytes ) : integer; Overload;
Function BytePos(find : TBytes; source : TBytes ) : integer; Overload;

function EncodeBase64(const value : TBytes) : AnsiString; overload;
{$IFNDEF FPC}
Function DecodeBase64(Const value : AnsiString) : TBytes; Overload;
{$ENDIF}
Function DecodeBase64(Const value : String) : TBytes; Overload;

type
  TXmlEncodingMode = (xmlText, xmlAttribute, xmlCanonical);
  TEolnOption = (eolnIgnore, eolnCanonical, eolnEscape);

function FormatTextToHTML(AStr: String): String; // translate ready for use in HTML
function FormatTextToXML(AStr: String; mode : TXmlEncodingMode): String;
function FormatCodeToXML(AStr: String): String;
function FormatXMLToHTML(AStr : String):String;
function FormatXMLToHTMLPlain(AStr : String):String;
function FormatXMLForTextArea(AStr: String): String;
function FormatJsonToHTML(AStr : String):String;

Function EncodeXML(Const sValue : String; bEoln : Boolean = True) : String; Overload;
Function DecodeXML(Const sValue : String) : String; Overload;

Function EncodePercent(Const sValue : String) : String; Overload;
Function DecodePercent(Const sValue : String) : String; Overload;


Type
//
// Mask Format:
//
//  C = Any character
//  9 = Numbers (0..9);
//  A = Alpha numeric (0..9, a-z)
//  L = Alphabet letter (a-z)
//  N = Non-alphanumeric
//
//  Use MinLength to indicate the required and optional parts of the mask.
//  Characters beyond the mask and before MaxLength are considered to have the format
//  of C = any character.
//
//  All other characters are considered literal.  Backslash '\' can be used to
//  escape the mask characters.  It is good practice to backslash your literal
//  characters to allow future expandability.
//
//  eg #1: Post Code = 9999
//     #2: 8.3 filename = AAAAAAAA\.AAA
//     #3: XML Date Time Format = 9999\/99\/99\T99\:99\:99
//
  TFslCodeMaskAllow = TCharSet;
  TFslCodeMaskAllowArray = Array Of TFslCodeMaskAllow;

  TFslCodeMaskFormat = String;

  TFslCodeMask = Class(TFslObject)
    Private
      FMinLength : Integer;
      FMaxLength : Integer;

      FFixedLength : Integer;
      FFixedFormat : TFslCodeMaskFormat;

      FFormat : TFslCodeMaskFormat;
      FAllowed : TFslCodeMaskAllowArray;

      FCapitals : Boolean;
      FCaseSensitive : Boolean;

      Procedure SetFormat(Const Value : TFslCodeMaskFormat);

      Function GetFixedFormat : TFslCodeMaskFormat;
      Function GetFixedLength : Integer;

      Function GetFixedCount: Integer;

    Protected
      Function Translate(Const cSymbol : Char) : TFslCodeMaskAllow;

    Public
      Function Link : TFslCodeMask;

      // Querying aspects of the mask.
      Function Allowed(iIndex : Integer) : TFslCodeMaskAllow;
      Function Fixed(iIndex : Integer) : Boolean;
      Function Count : Integer;
      Function LeadingFixedLength : Integer;
      Function LeadingFixedText : String;
      Function TrailingFixedLength(Const iPosition : Integer) : Integer;
      Function AllowedAsText(iIndex : Integer) : String;
      Function FormatFromText(Const sText : String) : TFslCodeMaskFormat;
      Function Conforms(Const sValue : String) : Boolean;

      // Rendering strings using Format.
      Function Formatted(Const sText : String) : String;
      Function Unformatted(Const sText : String) : String;

      Property Format : TFslCodeMaskFormat Read FFormat Write SetFormat;
      Property FixedFormat : TFslCodeMaskFormat Read GetFixedFormat;
      Property FixedLength : Integer Read GetFixedLength;
      Property FixedCount : Integer Read GetFixedCount;
      Property MinLength : Integer Read FMinLength Write FMinLength;
      Property MaxLength : Integer Read FMaxLength Write FMaxLength;
      Property Capitals : Boolean Read FCapitals Write FCapitals;
      Property CaseSensitive : Boolean Read FCaseSensitive Write FCaseSensitive;
  End;



Implementation

Uses
  {$IFDEF MACOS}
  FMX.Graphics,
  {$ELSE}
  ActiveX, ComObj,
  {$ENDIF}
  IOUtils, DateUtils;


Function Percentage(Const iPart, iTotal : Integer) : Real;
Begin
  If (iTotal = 0) Then
    Result := 0
  Else
    Result := iPart / iTotal;
End;


Function IntegerBetweenInclusive(Const iLeft, iCheck, iRight : Integer) : Boolean;
Begin
  Result := (iLeft <= iCheck) And (iCheck <= iRight);
End;


Function IntegerBetweenExclusive(Const iLeft, iCheck, iRight : Integer) : Boolean;
Begin
  Result := (iLeft < iCheck) And (iCheck < iRight);
End;


Function IntegerBetween(Const iLeft, iCheck, iRight : Integer) : Boolean;
Begin
  Result := IntegerBetweenInclusive(iLeft, iCheck, iRight);
End;


Function CardinalBetweenInclusive(Const iLeft, iCheck, iRight : Cardinal) : Boolean;
Begin
  Result := (iLeft <= iCheck) And (iCheck <= iRight);
End;


Function CardinalBetweenExclusive(Const iLeft, iCheck, iRight : Cardinal) : Boolean;
Begin
  Result := (iLeft < iCheck) And (iCheck < iRight);
End;


Function IntegerBetweenInclusive(Const iLeft, iCheck, iRight : Int64) : Boolean;
Begin
  Result := (iLeft <= iCheck) And (iCheck <= iRight);
End;


Function IntegerBetweenExclusive(Const iLeft, iCheck, iRight : Int64) : Boolean;
Begin
  Result := (iLeft < iCheck) And (iCheck < iRight);
End;


Function IntegerBetween(Const iLeft, iCheck, iRight : Int64) : Boolean;
Begin
  Result := IntegerBetweenInclusive(iLeft, iCheck, iRight);
End;


Function RealBetweenInclusive(Const rLeft, rCheck, rRight : Real) : Boolean;
Begin
  Result := (rLeft <= rCheck) And (rCheck <= rRight);
End;


Function RealBetweenExclusive(Const rLeft, rCheck, rRight : Real) : Boolean;
Begin
  Result := (rLeft < rCheck) And (rCheck < rRight);
End;


Function RealBetween(Const rLeft, rCheck, rRight : Real) : Boolean;
Begin
  Result := RealBetweenInclusive(rLeft, rCheck, rRight);
End;


Function RealBetweenInclusive(Const rLeft, rCheck, rRight : Extended) : Boolean;
Begin
  Result := (rLeft <= rCheck) And (rCheck <= rRight);
End;


Function RealBetweenExclusive(Const rLeft, rCheck, rRight : Extended) : Boolean;
Begin
  Result := (rLeft < rCheck) And (rCheck < rRight);
End;


Function RealBetween(Const rLeft, rCheck, rRight : Extended) : Boolean;
Begin
  Result := RealBetweenInclusive(rLeft, rCheck, rRight);
End;


Function IntegerMin(Const A, B : Integer) : Integer;
Begin
  If A < B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerMax(Const A, B : Integer) : Integer;
Begin
  If A > B Then
    Result := A
  Else
    Result := B;
End;


Function RealMin(Const A, B : Real) : Real;
Begin
  If A < B Then
    Result := A
  Else
    Result := B;
End;


Function RealMax(Const A, B : Real) : Real;
Begin
  If A > B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerMin(Const A, B : Cardinal) : Cardinal;
Begin
  If A < B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerMax(Const A, B : Cardinal) : Cardinal;
Begin
  If A > B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerMin(Const A, B : Word) : Word;
Begin
  If A < B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerMax(Const A, B : Word) : Word;
Begin
  If A > B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerMin(Const A, B : Byte) : Byte;
Begin
  If A < B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerMax(Const A, B : Byte) : Byte;
Begin
  If A > B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerMin(Const A, B : Int64) : Int64;
Begin
  If A < B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerMax(Const A, B : Int64) : Int64;
Begin
  If A > B Then
    Result := A
  Else
    Result := B;
End;


Function IntegerArrayMin(Const aIntegers : Array Of Integer) : Integer;
Var
  iLoop : Integer;
Begin
  Result := aIntegers[Low(aIntegers)];

  For iLoop := Low(aIntegers) + 1 To High(aIntegers) Do
  Begin
    If Result > aIntegers[iLoop] Then
      Result := aIntegers[iLoop];
  End;
End;


Function IntegerArrayMax(Const aIntegers : Array Of Integer) : Integer;
Var
  iLoop : Integer;
Begin
  Result := aIntegers[Low(aIntegers)];

  For iLoop := Low(aIntegers) + 1 To High(aIntegers) Do
  Begin
    If Result < aIntegers[iLoop] Then
      Result := aIntegers[iLoop];
  End;
End;


Function IntegerArraySum(Const aIntegers : Array Of Integer) : Integer;
Var
  iLoop : Integer;
Begin
  Result := 0;
  For iLoop := 0 To Length(aIntegers) - 1 Do
    Inc(Result, aIntegers[iLoop]);
End;


Function RealCeiling(Const rValue : Real) : Int64;
Begin
  Result := Trunc(rValue);
  If Frac(rValue) > 0 Then
    Inc(Result);
End;


Function RealFloor(Const rValue : Real) : Int64;
Begin
  Result := Trunc(rValue);
  If Frac(rValue) < 0 Then
    Dec(Result);
End;


Function Int64Round(Const iValue, iFactor : Int64) : Int64;
Var
  iFulcrum : Int64;
  iRemain : Int64;
Begin 
  iFulcrum := iFactor Div 2;
  iRemain := SignedMod(iValue, iFactor);

  If iRemain < iFulcrum Then
    Result := iValue - iRemain
  Else If iRemain > iFulcrum Then
    Result := iValue + (iFactor - iRemain)
  Else
    Result := iValue;
End;  


Function RealSquare(Const rValue : Extended) : Extended;
Begin
  Result := rValue * rValue;
End;


Function RealSquareRoot(Const rValue : Extended) : Extended;
Begin
  Result := System.Sqrt(rValue);
End;


Function IntegerArrayIndexOf(Const aIntegers : Array Of Word; iValue : Word) : Integer;
Begin
  Result := High(aIntegers);
  While (Result >= 0) And (aIntegers[Result] <> iValue) Do
    Dec(Result);
End;


Function IntegerArrayIndexOf(Const aIntegers : Array Of Integer; iValue : Integer) : Integer;
Begin
  Result := High(aIntegers);
  While (Result >= 0) And (aIntegers[Result] <> iValue) Do
    Dec(Result);
End;


Function IntegerArrayIndexOf(Const aIntegers : Array Of Cardinal; iValue : Cardinal) : Integer;
Begin 
  Result := High(aIntegers);
  While (Result >= 0) And (aIntegers[Result] <> iValue) Do
    Dec(Result);
End;


Function IntegerArrayExists(Const aIntegers : Array Of Integer; iValue : Integer) : Boolean;
Begin
  Result := IntegerArrayValid(aIntegers, IntegerArrayIndexOf(aIntegers, iValue));
End;


Function IntegerArrayValid(Const aIntegers : Array Of Integer; iIndex : Integer) : Boolean;
Begin
  Result := IntegerBetweenInclusive(Low(aIntegers), iIndex, High(aIntegers));
End;


Function RealRoundToInteger(Const aValue : Extended) : Int64; 
Begin
  Result := System.Round(aValue);
End;


Function Sign(Const iValue : Integer) : Integer;
Begin 
  If iValue < 0 Then
    Result := -1
  Else If iValue > 0 Then
    Result := 1
  Else
    Result := 0;
End;  


Function IntegerConstrain(Const iValue, iMin, iMax : Integer) : Integer;
Begin
  If iValue < iMin Then
    Result := iMin
  Else If iValue > iMax Then
    Result := iMax
  Else
    Result := iValue;
End;


Function RealConstrain(Const rValue, rMin, rMax : Real) : Real;
Begin 
  If rValue < rMin Then
    Result := rMin
  Else If rValue > rMax Then
    Result := rMax
  Else
    Result := rValue;
End;  


Function Abs(Const iValue : Int64) : Int64;
Begin
  If iValue < 0 Then
    Result := -iValue
  Else
    Result := iValue;
End;


Function Abs(Const iValue : Integer) : Integer;
Begin
  If iValue < 0 Then
    Result := -iValue
  Else
    Result := iValue;
End;


Function Abs(Const rValue : Real) : Real;
Begin 
  If rValue < 0 Then
    Result := -rValue
  Else
    Result := rValue;
End;  


Function IntegerCompare(Const iA, iB : Byte) : Integer;
Begin
  If iA < iB Then
    Result := -1
  Else If iA > iB Then
    Result := 1
  Else
    Result := 0
End;


Function BooleanCompare(Const bA, bB : Boolean) : Integer;
Begin
  If bA < bB Then
    Result := -1
  Else If bA > bB Then
    Result := 1
  Else
    Result := 0
End;


Function IntegerCompare(Const iA, iB : Word) : Integer;
Begin
  If iA < iB Then
    Result := -1
  Else If iA > iB Then
    Result := 1
  Else
    Result := 0
End;


Function IntegerCompare(Const iA, iB, iThreshold : Int64) : Integer;
Begin
  If iA - iThreshold > iB Then
    Result := 1
  Else If iB - iThreshold > iA Then
    Result := -1
  Else
    Result := 0;
End;


Function IntegerCompare(Const iA, iB : Integer) : Integer;
Begin 
  If iA < iB Then
    Result := -1
  Else If iA > iB Then
    Result := 1
  Else
    Result := 0
End;


Function IntegerCompare(Const iA, iB : Cardinal) : Integer;
Begin
  If iA < iB Then
    Result := -1
  Else If iA > iB Then
    Result := 1
  Else
    Result := 0
End;


Function IntegerCompare(Const iA, iB : Int64) : Integer;
Begin
  If iA < iB Then
    Result := -1
  Else If iA > iB Then
    Result := 1
  Else
    Result := 0
End;


Function RealCompare(Const rA, rB : Real) : Integer;
Begin
  If rA > rB Then
    Result := 1
  Else If rA < rB Then
    Result := -1
  Else
    Result := 0;
End;


Function RealCompare(Const rA, rB : Extended) : Integer;
Begin
  If rA > rB Then
    Result := 1
  Else If rA < rB Then
    Result := -1
  Else
    Result := 0;
End;


Function RealCompare(Const rA, rB, rThreshold : Real) : Integer;
Begin
  If rA - rThreshold > rB Then
    Result := 1
  Else If rB - rThreshold > rA Then
    Result := -1
  Else
    Result := 0;
End;  


Function RealEquals(Const rA, rB, rThreshold : Real) : Boolean;
Begin 
  Result := RealCompare(rA, rB, rThreshold) = 0;
End;  


Function BooleanEquals(Const bA, bB : Boolean) : Boolean;
Begin 
  Result := BooleanCompare(bA, bB) = 0;
End;


Function IntegerEquals(Const iA, iB : Byte) : Boolean;
Begin
  Result := IntegerCompare(iA, iB) = 0;
End;


Function IntegerEquals(Const iA, iB : Word) : Boolean;
Begin
  Result := IntegerCompare(iA, iB) = 0;
End;


Function IntegerEquals(Const iA, iB : Integer) : Boolean;
Begin
  Result := IntegerCompare(iA, iB) = 0;
End;


Function IntegerEquals(Const iA, iB : Cardinal) : Boolean;
Begin
  Result := IntegerCompare(iA, iB) = 0;
End;


Function IntegerEquals(Const iA, iB : Int64) : Boolean;
Begin
  Result := IntegerCompare(iA, iB) = 0;
End;


Function IntegerEquals(Const iA, iB, iThreshold : Int64) : Boolean;
Begin
  Result := IntegerCompare(iA, iB, iThreshold) = 0;
End;


Function RealEquals(Const rA, rB : Real) : Boolean;
Begin
  Result := RealCompare(rA, rB) = 0;
End;


Function Sin(Const Theta : Extended) : Extended;
Begin
  Result := System.Sin(Theta);
End;


Function Cos(Const Theta : Extended) : Extended;
Begin 
  Result := System.Cos(Theta);
End;  


Function ArcTan(Const Theta : Extended) : Extended;
Begin 
  Result := System.ArcTan(Theta);
End;  


Function ArcTan2(Const X, Y : Extended) : Extended;
Begin 
  Result := Math.ArcTan2(X, Y);
End;  


Function DegreesToRadians(Const rDegrees : Extended) : Extended;
Begin 
  Result := Math.DegToRad(rDegrees);
End;  


Function RadiansToDegrees(Const rRadians : Extended) : Extended;
Begin 
  Result := Math.RadToDeg(rRadians);
End;  


Function Power(Const rBase : Extended; Const iExponent: Integer): Extended;
Begin 
  Result := Math.Power(rBase, iExponent);
End;  


Function Power(Const rBase, rExponent: Extended): Extended;
Begin 
  Result := Math.Power(rBase, rExponent);
End;  


Function LnXP1(Const X: Extended): Extended;
Begin 
  Result := Math.LnXP1(X);
End;  


Function LogN(Const Base, X: Extended): Extended;
Begin 
  Result := Math.LogN(Base, X);
End;  


Function Log10(Const X: Extended): Extended;
Begin 
  Result := Math.Log10(X);
End;  


Function Log2(Const X: Extended): Extended;
Begin 
  Result := Math.Log2(X);
End;  


Function Hypotenuse(Const X, Y: Extended): Extended;
Begin 
  Result := Math.Hypot(X, Y);
End;


Function SignedMod(Const iValue : Integer; Const iRange : Integer) : Integer;
Begin
  Result := iValue Mod iRange;
End;


Function SignedMod(Const iValue : Int64; Const iRange : Int64) : Int64;
Begin
  Result := iValue Mod iRange;
End;


Function UnsignedMod(Const iValue : Cardinal; Const iRange : Cardinal) : Cardinal;
Begin
  Result := iValue Mod iRange;
End;


Function UnsignedMod(Const iValue : Integer; Const iRange : Integer) : Integer;
Begin
  Result := iValue Mod iRange;

  If Result < 0 Then
    Result := Result + iRange;
End;


Function UnsignedMod(Const iValue : Int64; Const iRange : Int64) : Int64;
Begin
  Result := iValue Mod iRange;

  If Result < 0 Then
    Result := Result + iRange;
End;


Function RemoveRemainder(Const iValue : Cardinal; Const iRange : Cardinal) : Cardinal;
Begin
  Result := iValue - UnsignedMod(iValue, iRange);
End;


Function RemoveRemainder(Const iValue : Integer; Const iRange : Integer) : Integer;
Begin
  Result := iValue - UnsignedMod(iValue, iRange);
End;


Function RemoveRemainder(Const iValue : Int64; Const iRange : Int64) : Int64;
Begin
  Result := iValue - UnsignedMod(iValue, iRange);
End;


Function GreatestCommonDivisor(Const iA, iB : Integer) : Integer;
Var
  iMinValue : Integer;
  iMaxValue : Integer;
  iCurrentNumerator : Integer;
  iCurrentDenominator : Integer;
  iCurrentRemainder : Integer;
Begin
  If iA = iB Then
  Begin
    Result := iA;
  End
  Else
  Begin
    If iA > iB Then
    Begin
      iMaxValue := iA;
      iMinValue := iB;
    End
    Else
    Begin
      iMaxValue := iB;
      iMinValue := iA;
    End;

    If (iMinValue = 0) Then
    Begin
      Result := 0;
    End
    Else 
    Begin
      iCurrentNumerator := iMaxValue;
      iCurrentDenominator := iMinValue;

      Repeat
        iCurrentRemainder := iCurrentNumerator Mod iCurrentDenominator;

        iCurrentNumerator := iCurrentDenominator;
        iCurrentDenominator := iCurrentRemainder;
      Until iCurrentRemainder = 0;

      Result := iCurrentNumerator;
    End;
End;
  End;
  
Function FileGetModified(Const sFileName : String) : TDateTime;
begin
  FileAge(sFilename, result, true);
end;

{$IFDEF MSWINDOWS}
function ConvertDateTimeToFileTime(const DateTime: TDateTime): TFileTime;
var
  LFileTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result.dwLowDateTime := 0;
  Result.dwLowDateTime := 0;
  DecodeDateTime(DateTime, SysTime.wYear, SysTime.wMonth, SysTime.wDay,
    SysTime.wHour, SysTime.wMinute, SysTime.wSecond, SysTime.wMilliseconds);

  if SystemTimeToFileTime(SysTime, LFileTime) then
    LocalFileTimeToFileTime(LFileTime, Result)
end;
{$ENDIF}
{$IFDEF POSIX}
function ConvertDateTimeToFileTime(const DateTime: TDateTime;
  const UseLocalTimeZone: Boolean): time_t;
begin
  { Use the time zone if necessary }
  if not UseLocalTimeZone then
    Result := DateTimeToFileDate(TTimeZone.Local.ToLocalTime(DateTime))
  else
    Result := DateTimeToFileDate(DateTime);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure FileSetModified(Const sFileName : String; time : TDateTime);
Var
  aHandle : TFileHandle;
  aTime : TFileTime;
Begin
  aTime := ConvertDateTimeToFileTime(time);
  aHandle := FileHandleOpen(CreateFile(PChar(sFileName), GENERIC_WRITE, FILE_SHARE_READ, Nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL Or FILE_FLAG_BACKUP_SEMANTICS, 0));
  Try
    If FileHandleIsValid(aHandle) Then
      SetFileTime(aHandle.Value, Nil, Nil, @aTime);
  Finally
    FileHandleClose(aHandle);
  End;
End;
{$ENDIF}



Type
  TLargeInteger = Record
    Low : Cardinal;
    High : Cardinal;
  End;

  PLargeInteger = ^TLargeInteger;

const psc = {$IFDEF MSWINDOWS} '\' {$ELSE} '/' {$ENDIF};


Function PathFolder(Const sFilename : String) : String;
Var
  iIndex : Integer;
Begin
  iIndex := LastDelimiter(psc+':', sFilename);

  If (iIndex > 1) And (sFilename[iIndex] = psc+':') And
     (Not CharInSet(sFilename[iIndex - 1], [psc, ':']) Or
     (ByteType(sFilename, iIndex - 1) = mbTrailByte)) Then
    Dec(iIndex);

  Result := StringIncludeAfter(Copy(sFilename, 1, iIndex), psc);
End;

Function FileExists(Const sFilename : String) : Boolean;
Begin
  result := SysUtils.FileExists(sFilename);
End;

Function FileDelete(Const sFilename : String) : Boolean;
Begin
  {$IFDEF MACOS}
  if FileIsReadOnly(sFileName) then
    result := false
  else
  {$ENDIF}
  Result := SysUtils.DeleteFile(sFilename);
End;


Function PathExtension(Const sFilename : String) : String;
Begin
  // Return the extension including the '.'.  Eg. PathExtension('notepad.exe') = '.exe'

  Result := SysUtils.ExtractFileExt(sFilename);
End;


Function PathFilename(Const sFilename : String) : String;
Begin
  Result := Copy(sFileName, LastDelimiter('\/:', sFileName) + 1, MaxInt);
End;

Function PathTitle(Const sFilename : String) : String;
Begin
  // Return the filename without the path or the extension.

  Result := PathFilename(sFilename);

  Result := Copy(Result, 1, Length(Result) - Length(PathExtension(sFilename)));
End;


Function FileSize(Const sFileName : String) : Int64;
{$IFDEF MACOS}
var
  f : TFileStream;
begin
  f := TFileStream.create(sFileName, fmOpenRead);
  try
    result := f.size;
  finally
    f.free;
  end;
end;
{$ELSE}
Var
  pResult : PLargeInteger;
  aHandle : TFileHandle;
Begin
  aHandle := FileHandleOpen(CreateFile(PChar(sFileName), GENERIC_READ, FILE_SHARE_READ Or FILE_SHARE_WRITE, Nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0));
  Try
    If FileHandleIsValid(aHandle) Then
    Begin
      pResult := PLargeInteger(@Result);

      pResult^.Low := GetFileSize(aHandle.Value, @pResult^.High);

      If pResult^.Low = FileHandleInvalid.Value Then
        Result := 0;
    End
    Else
    Begin
      Result := 0;
    End;
  Finally
    FileHandleClose(aHandle);
  End;
End;
{$ENDIF}

Function FolderExists(Const sFolder : String) : Boolean;
Begin
  result := SysUtils.DirectoryExists(sFolder);
End;


Function last(Const s: String; c: Char): Cardinal;
Var
  i: Word;
Begin
  i := Length(s);
  Result := 0;
  While (i > 0) Do
    Begin
    If s[i] = c Then
      Begin
      Result := i;
      Exit;
      End;
    Dec(i);
    End;
End;

Function ForceFolder(dir: String): Boolean;
begin
  result := SysUtils.ForceDirectories(dir);
end;

Function FolderDelete(Const sFolder : String) : Boolean;
{$IFDEF FPC}
begin
  Result := DeleteDirectory(sFolder,True);
  if Result then
    Result := RemoveDir(sFolder);
{$ELSE}
var
  s : string;
begin
  result := true;
  for s in TDirectory.GetDirectories(sFolder) do
    if not FolderDelete(s) then
      exit(false);
  for s in TDirectory.GetFiles(sFolder) do
    if not DeleteFile(s) then
      exit(false);
  SysUtils.RemoveDir(sFolder);
{$ENDIF}
end;

Function FileHandleInvalid : TFileHandle;
Begin
  Result.Value := $FFFFFFFF;
End;


Function FileHandleIsValid(Const aFileHandle : TFileHandle) : Boolean;
Begin
  Result := aFileHandle.Value <> FileHandleInvalid.Value;
End;

Function FileHandleOpen(Const aValue : Cardinal) : TFileHandle;
Begin
  Result.Value := aValue;
End;

Procedure FileHandleClose(Var aFileHandle : TFileHandle);
{$IFDEF MACOS}
begin
end;
{$ELSE}
Begin
  Windows.CloseHandle(aFileHandle.Value);
  aFileHandle := FileHandleInvalid;
End;
{$ENDIF}

function makeRelativePath(path, base : String): String;
begin
  if path.StartsWith(base) then
    result := path.Substring(base.Length)
  else
    result := path;
end;

function makeAbsolutePath(fn, base : String): String;
begin
  if fn.Contains(':') then
    result := fn
  else
    result := path([base, fn]);
end;

function Path(parts : array of String) : String;
var
  part : String;
  s : String;
begin
  result := '';
  for part in parts do
  begin
    s := part.Replace('/', psc).Replace('\', psc);
    if result = '' then
      result := s
    else if not result.EndsWith(psc) and not s.startsWith(psc) then
      result := result + psc + s
    else if not result.EndsWith(psc) or not s.startsWith(psc) then
      result := result + s
    else
      result := result + s.substring(1);
  end;
end;

function URLPath(parts : array of String) : String;
var
  part : String;
begin
  result := '';
  for part in parts do
    if result = '' then
      result := part
    else if not result.EndsWith('/') and not part.startsWith('/') then
      result := result +'/'+part
    else if not result.EndsWith('/') or not part.startsWith('/') then
      result := result + part
    else
      result := result + part.substring(1);
end;

Function CreateGUID : TGUID;
Begin
  SysUtils.CreateGuid(Result);
End;


Function GUIDToString(Const aGUID : TGUID) : String;
Begin
  Result := SysUtils.GUIDToString(aGuid);
End;


Function StringToGUID(Const sGUID: String) : TGUID;
Begin
  Result := SysUtils.StringToGUID(sGUID);
End;


Function NilGUID : TGUID;
Begin
  Result := StringToGUID('{00000000-0000-0000-0000-000000000000}');
End;

Function IsNilGUID(const guid : TGUID) : boolean;
begin
  result := (guid.D1 = 0) and (guid.D2 = 0) and (guid.D3 = 0);
end;

Function GUIDAsOID(Const aGUID : TGUID) : String;
begin
  result := GUIDAsOIDRight(aGUID);
end;

Function IsGuid(s : String): Boolean;
begin
  if length(s) < 36  then
    result := false
  else
  begin
    if (s[1] = '{') then
    begin
      delete(s, 1, 1);
      delete(s, length(s), 1);
    end;
    s := lowercase(s);
    result := (Length(s) = 36) and
      CharInSet(s[01], ['a'..'f', '0'..'9']) and
      CharInSet(s[02], ['a'..'f', '0'..'9']) and
      CharInSet(s[03], ['a'..'f', '0'..'9']) and
      CharInSet(s[04], ['a'..'f', '0'..'9']) and
      CharInSet(s[05], ['a'..'f', '0'..'9']) and
      CharInSet(s[06], ['a'..'f', '0'..'9']) and
      CharInSet(s[07], ['a'..'f', '0'..'9']) and
      CharInSet(s[08], ['a'..'f', '0'..'9']) and
      (s[09] = '-') and
      CharInSet(s[10], ['a'..'f', '0'..'9']) and
      CharInSet(s[11], ['a'..'f', '0'..'9']) and
      CharInSet(s[12], ['a'..'f', '0'..'9']) and
      CharInSet(s[13], ['a'..'f', '0'..'9']) and
      (s[14] = '-') and
      CharInSet(s[15], ['a'..'f', '0'..'9']) and
      CharInSet(s[16], ['a'..'f', '0'..'9']) and
      CharInSet(s[17], ['a'..'f', '0'..'9']) and
      CharInSet(s[18], ['a'..'f', '0'..'9']) and
      (s[19] = '-') and
      CharInSet(s[20], ['a'..'f', '0'..'9']) and
      CharInSet(s[21], ['a'..'f', '0'..'9']) and
      CharInSet(s[22], ['a'..'f', '0'..'9']) and
      CharInSet(s[23], ['a'..'f', '0'..'9']) and
      (s[24] = '-') and
      CharInSet(s[25], ['a'..'f', '0'..'9']) and
      CharInSet(s[26], ['a'..'f', '0'..'9']) and
      CharInSet(s[27], ['a'..'f', '0'..'9']) and
      CharInSet(s[28], ['a'..'f', '0'..'9']) and
      CharInSet(s[29], ['a'..'f', '0'..'9']) and
      CharInSet(s[30], ['a'..'f', '0'..'9']) and
      CharInSet(s[31], ['a'..'f', '0'..'9']) and
      CharInSet(s[32], ['a'..'f', '0'..'9']) and
      CharInSet(s[33], ['a'..'f', '0'..'9']) and
      CharInSet(s[34], ['a'..'f', '0'..'9']) and
      CharInSet(s[35], ['a'..'f', '0'..'9']) and
      CharInSet(s[36], ['a'..'f', '0'..'9']);
  end;
end;

function NewGuidURN : String;
begin
  result := 'urn:uuid:'+NewGuidId;
end;

function NewGuidId : String;
begin
  result := copy(GUIDToString(CreateGUID), 2, 36).ToLower;
end;


Function ColourCompose(iRed, iGreen, iBlue, iAlpha : Byte) : TColour;
Begin
  TColourParts(Result).Red := iRed;
  TColourParts(Result).Green := iGreen;
  TColourParts(Result).Blue := iBlue;
  TColourParts(Result).Alpha := iAlpha;
End;

Function ColourMultiply(iColour : TColour; Const iRatio : Real) : TColour;
Begin
  TColourParts(Result).Red := Trunc(RealMin(TColourParts(iColour).Red * iRatio, 255));
  TColourParts(Result).Green := Trunc(RealMin(TColourParts(iColour).Green * iRatio, 255));
  TColourParts(Result).Blue := Trunc(RealMin(TColourParts(iColour).Blue * iRatio, 255));
  TColourParts(Result).Alpha := Trunc(RealMin(TColourParts(iColour).Alpha * iRatio, 255));
End;


Function ColourMultiply(iColour : TColour; Const aRatio : TColourRatio) : TColour;
Begin
  TColourParts(Result).Red := Trunc(RealMin(TColourParts(iColour).Red * aRatio.Red, 255));
  TColourParts(Result).Green := Trunc(RealMin(TColourParts(iColour).Green * aRatio.Green, 255));
  TColourParts(Result).Blue := Trunc(RealMin(TColourParts(iColour).Blue * aRatio.Blue, 255));
  TColourParts(Result).Alpha := Trunc(RealMin(TColourParts(iColour).Alpha * aRatio.Alpha, 255));
End;


Function ColourInverse(iColour : TColour) : TColour;
Begin
  TColourParts(Result).Red := Abs(255 - TColourParts(iColour).Red);
  TColourParts(Result).Green := Abs(255 - TColourParts(iColour).Green);
  TColourParts(Result).Blue := Abs(255 - TColourParts(iColour).Blue);
  TColourParts(Result).Alpha := TColourParts(iColour).Alpha;
End;

Function ColourToString(iColour : TColour) : String;
Begin
  Result := ColorToString(iColour);
End;

Function ColourMakeGrey(iColour : TColour) : TColour; Overload;
var
  c : Word;
Begin
  c := Trunc((TColourParts(iColour).Red + TColourParts(iColour).Green + TColourParts(iColour).Blue) / 3);

  TColourParts(Result).Red := c;
  TColourParts(Result).Green := c;
  TColourParts(Result).Blue := c;
  TColourParts(Result).Alpha := TColourParts(iColour).Alpha;
End;

Function HTMLColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour;
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOf(HTML_COLOUR_NAMES, sColour);

  If iIndex = -1 Then
    iIndex := StringArrayIndexOf(HTML_COLOUR_TITLES, sColour);

  If iIndex > -1 Then
    Result := HTML_COLOUR_VALUES[THTMLColours(iIndex)]
  Else
    Result := HTMLEncodedColourStringToColour(sColour, aDefault);
End;


Function HTMLColourStringToColour(Const sColour : String) : TColour;
Begin
  Result := HTMLColourStringToColour(sColour, 0);
End;


Function StringIsHTMLColour(Const sColour : String) : Boolean;
Begin
  Result := StringArrayExistsInsensitive(HTML_COLOUR_NAMES, sColour) Or StringArrayExistsInsensitive(HTML_COLOUR_TITLES, sColour) Or
     ((Length(sColour) = 7) And (sColour[1] = '#') And StringIsHexadecimal(Copy(sColour, 2, 2)) And StringIsHexadecimal(Copy(sColour, 4, 2)) And StringIsHexadecimal(Copy(sColour, 6, 2)));
End;

Function HTMLEncodedColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour;
Begin
  If (Length(sColour) < 7) Or (sColour[1] <> '#') Then
    Result := aDefault
  Else
  Begin
    TColourParts(Result).Red := DecodeHexadecimal(AnsiChar(sColour[2]), AnsiChar(sColour[3]));
    TColourParts(Result).Green := DecodeHexadecimal(AnsiChar(sColour[4]), AnsiChar(sColour[5]));
    TColourParts(Result).Blue := DecodeHexadecimal(AnsiChar(sColour[6]), AnsiChar(sColour[7]));
    TColourParts(Result).Alpha := 0;
  End;
End;


Function HTMLEncodedColourStringToColour(Const sColour : String) : TColour;
Begin
  Result := HTMLEncodedColourStringToColour(sColour, 0);
End;

Function ColourToHTMLEncodedColourString(Const iColour : TColour) : String;
Begin
  Result := '#' + string(EncodeHexadecimal(TColourParts(iColour).Red) + EncodeHexadecimal(TColourParts(iColour).Green) + EncodeHexadecimal(TColourParts(iColour).Blue));
End;


Function ColourToHTMLColourString(Const iColour : TColour) : String;
Var
  iIndex : Integer;
Begin
  iIndex := IntegerArrayIndexOf(HTML_COLOUR_VALUES, iColour);

  If iIndex > -1 Then
    Result := HTML_COLOUR_NAMES[THTMLColours(iIndex)]
  Else
    Result := ColourToHTMLEncodedColourString(iColour);
End;


Function ColourToHTMLColourTitleString(Const iColour : TColour) : String;
Var
  iIndex : Integer;
Begin
  iIndex := IntegerArrayIndexOf(HTML_COLOUR_VALUES, iColour);

  If iIndex > -1 Then
    Result := HTML_COLOUR_TITLES[THTMLColours(iIndex)]
  Else
    Result := ColourToHTMLEncodedColourString(iColour);
End;



Var
{$IFDEF MSWINDOWS}
  gOSInfo : TOSVersionInfo;
  gSystemInfo : TSystemInfo;
{$ENDIF}
  gNTDLLDebugBreakPointIssuePatched : Boolean = False;

Function SystemManualTemp : String;
Begin
  {$IFDEF MACOS}
  result := '/tmp';
  {$ELSE}
  result := 'c:/temp';
  {$ENDIF}
End;

Function SystemTemp : String;
  {$IFDEF MACOS}
Begin
  result := UTF8ToString(TNSString.Wrap(NSString(NSTemporaryDirectory)).UTF8String); {todo-osx}
  {$ELSE}
Var
  iLength : Integer;
Begin
  SetLength(Result, MAX_PATH + 1);

  iLength := GetTempPath(MAX_PATH, PChar(Result));

  If Not IsPathDelimiter(Result, iLength) Then
  Begin
    Inc(iLength);
    Result[iLength] := psc;
  End;

  SetLength(Result, iLength);
  {$ENDIF}
End;

Function SystemIsWindowsNT : Boolean;
Begin
  {$IFDEF MACOS}
  Result := false;
  {$ELSE}
  Result := gOSInfo.dwPlatformId >= VER_PLATFORM_WIN32_NT;
  {$ENDIF}
End;

Function SystemIsWindows7 : Boolean;
Begin
  {$IFDEF MACOS}
  Result := false;
  {$ELSE}
  Result := SystemIsWindowsNT And (gOSInfo.dwMajorVersion >= 6) And (gOSInfo.dwMinorVersion >= 1);
  {$ENDIF}
End;

{$IFDEF MACOS}
Function ProgData : String;
Begin
  Result := '/Applications';
End;

{$ELSE}
Function ShellFolder(iID : Integer) : String;
Var
  sPath : Array[0..2048] Of Char;
  pIDs  : PItemIDList;
Begin
  Result := '';

  If SHGetSpecialFolderLocation(0, iID, pIDs) = S_OK Then
  Begin
    FillChar(sPath, SizeOf(sPath), #0);

    If ShGetPathFromIDList(pIDs, sPath) Then
      Result := IncludeTrailingPathDelimiter(sPath);
  End;
End;

Function ProgData : String;
Begin
  Result := ShellFolder(CSIDL_COMMON_APPDATA);
End;
{$ENDIF}

Function UserFolder : String;
Begin
  {$IFDEF MSWINDOWS}
  Result := ShellFolder(CSIDL_PROFILE);
  {$ELSE}
  result := IOUtils.TPath.GetHomePath;
  {$ENDIF}
End;


Var
  gCriticalSection : TRTLCriticalSection;

{$IFOPT C+}
Var
  gLiveMemorySize : Int64 = 0;
  gActiveMemoryTracking : Boolean = False;
{$ENDIF}


Procedure MemorySet(Const pBuffer : Pointer; iSize : Integer; Const iValue : Byte);
Begin
  If (iSize > 0) And Assigned(pBuffer) Then
    FillChar(pBuffer^, iSize, iValue);
End;


Procedure MemoryZero(Const pBuffer : Pointer; iSize: Integer);
Begin
  MemorySet(pBuffer, iSize, $00);
End;


Procedure MemoryFill(Const pBuffer : Pointer; iSize : Integer);
Begin
  MemorySet(pBuffer, iSize, $FF);
End;


Procedure MemoryCreate(Var pBuffer; iSize : Integer);
Begin
  // Untyped because otherwise we'd have to pass in a literal Pointer type.

  GetMem(Pointer(pBuffer), iSize);

{$IFOPT C+}
  Assert(gActiveMemoryTracking, 'Memory tracking not available for call to MemoryCreate');
  EnterCriticalSection(gCriticalSection);
  Inc(gLiveMemorySize, iSize);
  LeaveCriticalSection(gCriticalSection);
{$ENDIF}
End;

Procedure MemoryResize(Var pBuffer; iOldSize, iNewSize : Integer);
Begin
  // Untyped because otherwise we'd have to pass in a literal Pointer type.

  ReAllocMem(Pointer(pBuffer), iNewSize);

{$IFOPT C+}
  Assert(gActiveMemoryTracking, 'Memory tracking not available for call to MemoryResize');
  EnterCriticalSection(gCriticalSection);
  Inc(gLiveMemorySize, iNewSize - iOldSize);
  LeaveCriticalSection(gCriticalSection);
{$ENDIF}
End;


Procedure MemoryDestroy(Var pBuffer; iSize : Integer);
Begin
  // Untyped because otherwise we'd have to pass in a literal Pointer type.

  FreeMem(Pointer(pBuffer), iSize);

{$IFOPT C+}
  Assert(gActiveMemoryTracking, 'Memory tracking not available for call to MemoryDestroy');
  EnterCriticalSection(gCriticalSection);
  Dec(gLiveMemorySize, iSize);
  LeaveCriticalSection(gCriticalSection);
{$ENDIF}
End;

Procedure MemoryMove(Const aSource, aTarget : Pointer; iSize : Integer);
Begin
  If (iSize > 0) And Assigned(aSource) And Assigned(aTarget) Then
    System.Move(aSource^, aTarget^, iSize);
End;

Function MemoryCompare(pA : Pointer; pB : Pointer; iLength : Integer) : Integer;
Begin
  Result := Integer(Not CompareMem(pA, pB, iLength));
End;


Function MemoryToString(pData : Pointer; iPosition, iLength : Integer) : AnsiString;
Begin
  SetString(Result, PAnsiChar(Integer(pData) + iPosition), iLength - iPosition);
End;


Function MemoryToString(pData : Pointer; iLength : Integer) : AnsiString;
Begin
  Result := MemoryToString(pData, 0, iLength);
End;


Function ErrorAsNumber : Integer;
Begin
  Result := GetLastError;
  SetLastError(0);
End;

Function ErrorAsString(iError : Integer) : String;
Begin
  Result := StringFormat('%d: %s', [iError, ErrorAsMessage(iError)]);
End;


Function ErrorAsMessage(iError : Integer) : String;
Var
{$IFDEF VER200}
  sTemp : PWideChar;
{$ELSE}
  sTemp : PChar;
{$ENDIF}
  iSize : Cardinal;
Begin
  iSize := 512;

  MemoryCreate(sTemp, iSize);
  Try
    // Get the last error number and convert it to text
 {$IFDEF MSWINDOWS}
    If FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM Or FORMAT_MESSAGE_ARGUMENT_ARRAY, Nil, DWORD(iError), LANG_NEUTRAL, sTemp, iSize, Nil) <> 0 Then
      Result := StringTrimWhitespace(Copy(StrPas(sTemp), 1, iSize))
    Else
 {$ENDIF}
      Result := 'Error Message missing on OSX';

  Finally
    MemoryDestroy(sTemp, iSize);
  End;
End;

Function ErrorAsString : String;
Var
  iError : Cardinal;
Begin
  iError := ErrorAsNumber;

  If iError = ERROR_SUCCESS Then
    Result := StringFormat('%d: Unknown Windows API error.', [iError])
  Else
    Result := ErrorAsString(iError);
End;

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
Function HashStringToCode32(Const sValue : String) : Integer;
Var
  cFirst  : Char;
  cSecond : Char;
  iLength : Cardinal;
  iLoop   : Integer;
Begin
  Result := 0;
  iLength := Length(sValue);

  If iLength > 0 Then
  Begin
    cFirst := sValue[1];

    If (cFirst >= 'a') And (cFirst <= 'z') Then
      Dec(cFirst, 32);

    For iLoop := 2 To iLength Do
    Begin
      cSecond := sValue[iLoop];

      If (cSecond >= 'a') And (cSecond <= 'z') Then
        Dec(cSecond, 32);

      Inc(Result, Ord(cFirst) * Ord(cSecond) * iLoop);

      cFirst := cSecond;
    End;
  End;
End;
{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}


{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
Function HashStringToCode64(Const sValue : String) : Int64;
Begin
  // TODO: implement.

  raise ELibraryException.create('HashStringToCode64 is not implemented.');

  Result := 0;
End;
{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}


{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
Function HashIntegerToCode32(Const iValue : Integer) : Integer;
Begin
  Result := iValue And $7FFFFFFF;
End;
{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}


{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
Function HashInteger64ToCode32(Const iValue : Int64) : Integer;
Begin
  Result := (iValue Shr 32) Xor (iValue And $7FFFFFFF);
End;
{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}

function tempFileName(prefix : String): String;
begin
  result := Path([SystemTemp, prefix+'-'+NewGuidId+'.tmp']);
end;


Function RandomBoolean : Boolean;
Begin
  Result := RandomInteger(2) = 1;
End;


Function RandomDateTime(Const aLowest, aHighest : TDateTime) : TDateTime;
Begin
  Result := aLowest + RandomReal(aHighest - aLowest);
End;


Function RandomInteger(Const iUpper : Integer) : Integer;
Begin
  Result := System.Random(iUpper);
End;


Function RandomInteger(Const iLowest, iHighest : Integer) : Integer;
Begin
  Result := System.Random(iHighest - iLowest + 1) + iLowest;
End;


Function RandomReal(Const iUpper : Real) : Real;
Begin
  Result := iUpper * System.Random;
End;


Function RandomReal(Const iLowest, iHighest : Real) : Real;
Begin
  Result := RandomReal(iHighest - iLowest) + iLowest;
End;


Function RandomInteger64(Const iLowest, iHighest : Int64) : Int64;
Begin
  Result := RealRoundToInteger((iHighest - iLowest) * System.Random) + iLowest;
End;


Function RandomAlphabeticString(Const iLength : Integer) : String;
Var
  iLoop : Integer;
Begin
  SetLength(Result, iLength);

  For iLoop := 1 To iLength Do
    Result[iLoop] := RandomAlphabeticCharacter;
End;


Function RandomAlphabeticCharacter : Char; Overload;
Const
  ALPHABET_STRING = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
Begin
  Result := ALPHABET_STRING[RandomInteger(Length(ALPHABET_STRING)) + 1]
End;


Function ColourToXMLColourString(Const iColour : TColour) : String;
Begin
  If TColourParts(iColour).Alpha > 0 Then
    Result := string(EncodeHexadecimal(TColourParts(iColour).Alpha))
  Else
    Result := '';

  Result := '#' + string(EncodeHexadecimal(TColourParts(iColour).Red) + EncodeHexadecimal(TColourParts(iColour).Green) + EncodeHexadecimal(TColourParts(iColour).Blue)) + Result;
End;


Function XMLColourStringToColour(Const sColour : String) : TColour;
Begin
  If (Length(sColour) >= 7) And (sColour[1] = '#') And StringIsHexadecimal(Copy(sColour, 2, Length(sColour))) Then
  Begin
    TColourParts(Result).Red := DecodeHexadecimal(AnsiChar(sColour[2]), AnsiChar(sColour[3]));
    TColourParts(Result).Green := DecodeHexadecimal(AnsiChar(sColour[4]), AnsiChar(sColour[5]));
    TColourParts(Result).Blue := DecodeHexadecimal(AnsiChar(sColour[6]), AnsiChar(sColour[7]));

    If (Length(sColour) >= 9) Then
      TColourParts(Result).Alpha := DecodeHexadecimal(AnsiChar(sColour[8]), AnsiChar(sColour[9]))
    Else
      TColourParts(Result).Alpha := $00;
  End
  Else
  Begin
    Result := HTMLEncodedColourStringToColour(sColour);
  End;
End;


Function XMLColourStringToColourOrDefault(Const sColour : String; Const aDefault : TColour) : TColour;
Begin
  If sColour = '' Then
    Result := aDefault
  Else
    Result := XMLColourStringToColour(sColour);
End;


{$IFDEF MSWINDOWS}
Procedure SoundPlay(Const sFilename : String);
Begin
  PlaySound(PChar(sFilename), 0, SND_FILENAME Or SND_ASYNC);
End;


Procedure SoundStop(Const sFilename : String);
Begin
  PlaySound(PChar(sFilename), 0, SND_FILENAME Or SND_PURGE);
End;


Procedure SoundBeepAsterisk;
Begin
  SoundBeepType(MediaSoundBeepTypeAsterisk);
End;


Procedure SoundBeepExclamation;
Begin
  SoundBeepType(MediaSoundBeepTypeExclamation);
End;


Procedure SoundBeepOK;
Begin
  SoundBeepType(MediaSoundBeepTypeOK);
End;


Procedure SoundBeepType(Const aSoundBeepType : TMediaSoundBeepType);
Const
  API_MESSAGE_BEEP : Array[TMediaSoundBeepType] Of Integer =
    (MB_ICONASTERISK, MB_ICONEXCLAMATION, MB_ICONHAND, MB_ICONQUESTION, MB_OK);
Begin
  Windows.MessageBeep(API_MESSAGE_BEEP[aSoundBeepType]);
End;


Procedure SoundBeepRange(Const iFrequency, iDuration : Cardinal);
Begin
  Windows.Beep(iFrequency, iDuration);
End;

{$ENDIF}


Function CurrencySymbol : String;
Begin
{$IFDEF VER130}
  Result := SysUtils.CurrencyString;
{$ELSE}
  Result := FormatSettings.CurrencyString;
{$ENDIF}
End;


Function CurrencyToString(Const rValue : TCurrency; iDigits : Integer = 2) : String;
Begin
  If (iDigits < 0) Or (iDigits > 4) Then
    iDigits := 2;

  Result := SysUtils.CurrToStrF(rValue, ffFixed, iDigits);
End;


Function StringToCurrency(Const sValue : String) : TCurrency;
Begin
  Result := StrToCurr(StringReplace(sValue, CurrencySymbol, ''));
End;


Function StringToCurrency(Const sValue : String; Const rDefault : TCurrency) : TCurrency;
Begin
  Try
    Result := StringToCurrency(sValue);
  Except
    Result := rDefault;
  End;
End;


Function StringIsCurrency(Const sValue : String) : Boolean;
Var
  rDummy : Currency;
Begin
  Result := TextToFloat(PChar(sValue), rDummy, fvCurrency);
End;


Function StringToCents(Const sValue : String) : TCurrencyCents;
Var
  sNormal : String;
  iPoint : Integer;
Begin
  iPoint := Pos('.', sValue);
  If iPoint = 0 Then
    sNormal := sValue + '00'
  Else If iPoint = Length(sValue) Then
    sNormal := Copy(sValue, 1, Length(sValue) - 1) + '00'
  Else If iPoint = Length(sValue) - 1 Then
    sNormal := Copy(sValue, 1, Length(sValue) - 2) + sValue[Length(sValue)] + '0'
  Else
    sNormal := Copy(sValue, 1, iPoint - 1) + Copy(sValue, iPoint + 1, MaxInt);

  Result := Abs(StringToInteger32(sNormal));
End;


Function CentsToString(Const iCents : TCurrencyCents) : String;
Begin
  Result := IntegerToString(Abs(iCents));

  While Length(Result) < 3 Do
    Result := '0' + Result;

  System.Insert('.', Result, Length(Result) - 1);

  If iCents < 0 Then
    Result := '(' + Result + ')';
End;


Function CurrencyRoundUp(Const iValue : TCurrency; Const iRoundCents : Integer) : TCurrency;
Var
  iIncrement : Currency;
  iDollars : Currency;
  iCents : Currency;
Begin
  iIncrement := iRoundCents / 100;
  iDollars := Trunc(iValue);
  iCents := iValue - iDollars;

  Result :=  iDollars + (iIncrement * RealCeiling(iCents / iIncrement));
End;


Function CurrencyRoundDown(Const iValue : TCurrency; Const iRoundCents : Integer) : TCurrency;
Var
  iIncrement : Currency;
  iDollars : Currency;
  iCents : Currency;
Begin
  iIncrement := iRoundCents / 100;
  iDollars := Trunc(iValue);
  iCents := iValue - iDollars;

  Result :=  iDollars + (iIncrement * RealFloor(iCents / iIncrement));
End;


Function CurrencyRoundNearest(Const iValue : TCurrency; Const iRoundCents : Integer) : TCurrency;
Var
  iIncrement : Currency;
  iDollars : Currency;
  iCents : Currency;
Begin
  iIncrement := iRoundCents / 100;
  iDollars := Trunc(iValue);
  iCents := iValue - iDollars;

  Result :=  iDollars + (iIncrement * RealFloor((iCents + (iIncrement / 2)) / iIncrement));
End;


Function CurrencyRoundBankers(Const rValue : TCurrency; Const iRoundCents : Integer) : TCurrency;
Var
  iWhole : Int64;
  rFraction : Extended;
Begin
  iWhole := Trunc(CurrencyToCents(rValue) / iRoundCents);
  rFraction := (CurrencyToCents(rValue) - (iWhole * iRoundCents)) / iRoundCents;

  If (rFraction > 0.5) Or ((rFraction = 0.5) And (SignedMod(iWhole, 2) = 1)) Then
    Result := iWhole + 1
  Else
    Result := iWhole;

  Result := (Result * iRoundCents) / 100;
End;


Function CurrencyToCents(Const iCurrency : TCurrency) : TCurrencyCents;
Begin
  Result := Trunc(iCurrency * 100);
End;


Function CentsToCurrency(Const iCents : TCurrencyCents) : TCurrency;
Begin
  Result := iCents;
  Result := Result / 100;
End;


Function CurrencyMin(Const rA, rB : TCurrency) : TCurrency;
Begin
  If rA < rB Then
    Result := rA
  Else
    Result := rB;
End;


Function CurrencyMax(Const rA, rB : TCurrency) : TCurrency;
Begin
  If rA > rB Then
    Result := rA
  Else
    Result := rB;
End;


Function CurrencyCompare(Const rA, rB : TCurrency) : Integer;
Begin
  If rA < rB Then
    Result := -1
  Else If rA > rB Then
    Result := 1
  Else
    Result := 0
End;


Function CurrencyDifference(Const iAmount1, iAmount2 : TCurrency) : TCurrency;
Begin
  If iAmount1 > iAmount2 Then
    Result := iAmount1 - iAmount2
  Else
    Result := iAmount2 - iAmount1;
End;


Function CurrencyAdjusted(Const rAmount, rGap : TCurrency; Const rPercentage : Real) : TCurrency;
Begin
  Result := CurrencyRoundUp(rAmount * rPercentage / 100, 5);

  If (Result <> 0) And (rPercentage < 100) And (rGap <> 0) And (Abs(rAmount - Result) > rGap) Then
    Result := rAmount - rGap;
End;


Function CurrencyApplyPercentages(Const rAmount : TCurrency; Const rPercentageBefore, rPercentageAfter : Real):TCurrency;
Begin
  Result := rAmount;

  If rPercentageBefore <> 100 Then
    Result := CurrencyAdjusted(Result, 0, rPercentageBefore);

  If rPercentageAfter <> 100 Then
    Result := CurrencyRoundUp(Result * rPercentageAfter / 100, 5);
End;


Function CurrencyTruncateToCents(Const rAmount : TCurrency):TCurrency;
Begin
  Result := Trunc(rAmount * 100) / 100;
End;


Function CurrencyTruncateToDollars(Const rAmount : TCurrency): TCurrency;
Begin
  Result := Trunc(rAmount);
End;


Function CurrencyCompare(Const rA, rB, rThreshold : TCurrency) : Integer;
Begin
  If rA - rThreshold > rB Then
    Result := 1
  Else If rB - rThreshold > rA Then
    Result := -1
  Else
    Result := 0;
End;


Function CurrencyEquals(Const rA, rB, rThreshold : TCurrency) : Boolean;
Begin
  Result := CurrencyCompare(rA, rB, rThreshold) = 0;
End;


Function CurrencyEqualsZero(Const rAmount : TCurrency) : Boolean;
Begin
  Result := CurrencyEquals(rAmount, 0, 0.001);
End;


Function CurrencyEquals(Const rA, rB : TCurrency) : Boolean;
Begin
  Result := rA = rB;
End;


Function SystemIsWindows2K : Boolean;
Begin
  {$IFDEF MSWINDOWS}
  Result := SystemIsWindowsNT And (gOSInfo.dwMajorVersion >= 5){ And (gOSInfo.dwMinorVersion >= 0)};
  {$ELSE}
  result := false;
  {$ENDIF}
End;


Function SystemIsWindowsXP : Boolean;
Begin
  {$IFDEF MSWINDOWS}
  Result := SystemIsWindowsNT And (gOSInfo.dwMajorVersion >= 5) And (gOSInfo.dwMinorVersion >= 1);
  {$ELSE}
  result := false;
  {$ENDIF}
End;


Function SystemIsWindowsVista : Boolean;
Begin
  {$IFDEF MSWINDOWS}
  Result := SystemIsWindowsNT And (gOSInfo.dwMajorVersion >= 6){ And (gOSInfo.dwMinorVersion >= 0)};
  {$ELSE}
  result := false;
  {$ENDIF}
End;


Function SystemIsWin64 : Boolean;
Begin
  {$IFDEF MSWINDOWS}
  Result := SystemIsWindowsNT And (gSystemInfo.dwAllocationGranularity = 65536);
  {$ELSE}
  result := false;
  {$ENDIF}
End;

  {$IFDEF MSWINDOWS}
Function SystemArchitecture : String;
Const
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_MIPS = 1;
  PROCESSOR_ARCHITECTURE_ALPHA = 2;
  PROCESSOR_ARCHITECTURE_PPC = 3;
  PROCESSOR_ARCHITECTURE_SHX = 4;
  PROCESSOR_ARCHITECTURE_ARM = 5;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_ALPHA64 = 7;
  PROCESSOR_ARCHITECTURE_MSIL = 8;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10;
Begin
  Case gSystemInfo.wProcessorArchitecture Of
    PROCESSOR_ARCHITECTURE_INTEL :
    Begin
      Case gSystemInfo.wProcessorLevel Of
        3 : Result := 'Intel 80386';
        4 : Result := 'Intel 80486';
        5 : Result := 'Intel Pentium';
        6 : Result := 'Intel Pentium II/III'; // ??
       15 : Result := 'Intel Pentium IV';
      Else
        Result := StringFormat('Intel Unknown (%d)', [gSystemInfo.wProcessorLevel]);
      End;
    End;

    PROCESSOR_ARCHITECTURE_MIPS :
    Begin
      Case gSystemInfo.wProcessorLevel Of
        4 : Result := 'MIPS R4000';
      Else
        Result := StringFormat('MIPS Unknown (%d)', [gSystemInfo.wProcessorLevel]);
      End;
    End;

    PROCESSOR_ARCHITECTURE_ALPHA :
    Begin
      Case gSystemInfo.wProcessorLevel Of
        21064 : Result := 'Alpha 21064';
        21066 : Result := 'Alpha 21066';
        21164 : Result := 'Alpha 21164';
      Else
        Result := StringFormat('Alpha Unknown (%d)', [gSystemInfo.wProcessorLevel]);
      End;
    End;

    PROCESSOR_ARCHITECTURE_PPC :
    Begin
      Case gSystemInfo.wProcessorLevel Of
        1  : Result := 'PPC 601';
        3  : Result := 'PPC 603';
        4  : Result := 'PPC 604';
        6  : Result := 'PPC 603+';
        9  : Result := 'PPC 604+';
        20 : Result := 'PPC 620';
      Else
        Result := StringFormat('PPC Unknown (%d)', [gSystemInfo.wProcessorLevel]);
      End;
    End;
  Else
    Result := 'Unknown'; // PROCESSOR_ARCHITECTURE_UNKNOWN
  End;
End;
{$ENDIF}

Function SystemPlatform: String;
Begin
  {$IFDEF MSWINDOWS}
  Case gOSInfo.dwPlatformId Of
    VER_PLATFORM_WIN32s :
    Begin
      Result := StringFormat('Windows %d.%d', [gOSInfo.dwMajorVersion, gOSInfo.dwMinorVersion]);
    End;

    VER_PLATFORM_WIN32_WINDOWS :
    Begin
      Case gOSInfo.dwMinorVersion Of
        0  : Result := 'Windows 95';
        10 : Result := 'Windows 98';
      Else
        Result := 'Windows ME';
      End;
    End;

    VER_PLATFORM_WIN32_NT :
    Begin
      Case gOSInfo.dwMajorVersion Of
        5 :
        Begin
          Case gOSInfo.dwMinorVersion Of
            0 : Result := 'Windows 2000';
            1 : Result := 'Windows XP';
          Else
            Result := 'Windows NT';
          End;
        End;
      Else
        Result := 'Windows NT ' + IntegerToString(gOSInfo.dwMajorVersion);
      End;
    End;
  Else
    Result := 'Windows';
  End;

  If Not SystemIsWindowsNT Then
    gOSInfo.dwBuildNumber := gOSInfo.dwBuildNumber And $FFFF;

  Result := Result + StringFormat(' [%d.%d.%d]', [gOSInfo.dwMajorVersion, gOSInfo.dwMinorVersion, gOSInfo.dwBuildNumber]);
  {$ELSE}
  Result := 'OSX';
  {$ENDIF}
End;

  {$IFDEF MSWINDOWS}
Function SystemTimezone : String;
Begin
  Result := NAMES_TIMEZONES[Timezone];
End;


function GetLocaleInformation(Flag: integer): string;
var
  pcLCA: array[0..20] of char;
begin
  if (GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, Flag, pcLCA, 19) <= 0) then
  begin
    pcLCA[0] := #0;
  end;
  Result := pcLCA;
end;
  {$ENDIF}

Function SystemLanguage : String;
Begin
  {$IFDEF OSX}
  result := 'en';
  {$ELSE}
  {$IFDEF FPC}
  {$IFDEF MSWINDOWS}
   Result := GetLocaleInformation(LOCALE_SENGLANGUAGE);
  {$ELSE}
   Result := SysUtils.GetEnvironmentVariable('LANG');
  {$ENDIF}
  {$ELSE}
  Result := Languages.NameFromLocaleID[GetUserDefaultLCID];
  {$ENDIF}
  {$ENDIF}
End;

  {$IFDEF MSWINDOWS}

Function SystemProcessors : Cardinal;
Begin
  Result := gSystemInfo.dwNumberOfProcessors;
End;


Function SystemPageSize : Cardinal;
Begin
  Result := gSystemInfo.dwPageSize;
End;
{$ENDIF}

{$IFDEF MSWINDOWS}
{$IFDEF CPUx86}
Function SystemMemory : TSystemMemory;
Begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Length := SizeOf(Result);

  GlobalMemoryStatus(TMemoryStatus(Result));
End;
{$ELSE}
  {$IFDEF CPUx64}
Function SystemMemory : TSystemMemory;
Begin
  FillChar(Result, SizeOf(Result), 0);
  {$IFDEF FPC}
  GlobalMemoryStatus(Result);
  {$ELSE}
  GlobalMemoryStatusEx(TMemoryStatusEx(Result));
  {$ENDIF}
End;
  {$ENDIF}
{$ENDIF}
{$ENDIF}

  {$IFDEF MSWINDOWS}

Function SystemName : String;
Var
  aBuffer : Array[0..255] Of Char;
  iLength : Cardinal;
Begin
  iLength := 255;

  GetComputerName(aBuffer, iLength);

  Result := aBuffer;
End;


Function SystemBootStatus: String;
Begin
  Case GetSystemMetrics(SM_CLEANBOOT) Of
    0 : Result := 'Normal boot';
    1 : Result := 'Fail-safe boot';
    2 : Result := 'Fail-safe boot with network boot';
  Else
    Result := 'Unknown boot';
  End;
End;

Const
  HKEY_CLASSES_ROOT = Cardinal($80000000);
  HKEY_CURRENT_USER = Cardinal($80000001);
  HKEY_LOCAL_MACHINE = Cardinal($80000002);
  HKEY_USERS = Cardinal($80000003);
  HKEY_PERFORMANCE_DATA = Cardinal($80000004);
  HKEY_CURRENT_CONFIG = Cardinal($80000005);
  HKEY_DYN_DATA = Cardinal($80000006);


Type
  TRegistryKey = Type Cardinal;
  TRegistryMode = (rmRead, rmWrite);

  TFslRegistryRootKeyType = (FslRegistryRootKeyClassesRoot, FslRegistryRootKeyCurrentUser, FslRegistryRootKeyLocalMachine,
    FslRegistryRootKeyUsers, FslRegistryRootKeyPerformanceData, FslRegistryRootKeyCurrentConfig, FslRegistryRootKeyDynData);

Const
  FslRegistryRootKeyNameArray : Array [TFslRegistryRootKeyType] Of String =
    ('HKEY_CLASSES_ROOT', 'HKEY_CURRENT_USER', 'HKEY_LOCAL_MACHINE', 'HKEY_USERS',
     'HKEY_PERFORMANCE_DATA', 'HKEY_CURRENT_CONFIG', 'HKEY_DYN_DATA');

  FslRegistryRootKeyValueArray : Array [TFslRegistryRootKeyType] Of TRegistryKey =
    (HKEY_CLASSES_ROOT, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_USERS,
     HKEY_PERFORMANCE_DATA, HKEY_CURRENT_CONFIG, HKEY_DYN_DATA);

Const
  MODE_ACCESS : Array[TRegistryMode] Of Cardinal = (KEY_READ, KEY_ALL_ACCESS);



Function SystemCentralProcessorStringRegistryValue(Const Value : String) : String;
Const
  CENTRAL_PROCESSOR_KEY = 'HARDWARE\DESCRIPTION\System\CentralProcessor\0';
//Var
//  oRegistry : TFslRegistry;
Begin
//  Result := '';
//
//  oRegistry := TFslRegistry.Create;
//  Try
//    oRegistry.ReadMode;
//    oRegistry.UseLocalMachineAsRootKey;
//
//    If oRegistry.KeyExists(CENTRAL_PROCESSOR_KEY) Then
//    Begin
//      oRegistry.Key := CENTRAL_PROCESSOR_KEY;
//
//      If oRegistry.ValueExists(Value) Then
//      Begin
//        oRegistry[Value];
//        oRegistry.DefineString(Result);
//      End;
//    End;
//  Finally
//    oRegistry.Free;
//  End;
End;


Function SystemProcessorIdentifier : String;
Begin
  Result := SystemCentralProcessorStringRegistryValue('Identifier');
End;


Function SystemProcessorVendorIdentifier : String;
Begin
  Result := SystemCentralProcessorStringRegistryValue('VendorIdentifier');
End;

Function SystemResolution : String;
Begin
  Result := StringFormat('%dx%d', [GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN)]);
End;

Function SystemInformation: String;
Begin
  Result := gOSInfo.szCSDVersion;
End;

Function SystemPath : String;
Var
  iLength : Integer;
Begin
  SetLength(Result, MAX_PATH + 1);

  iLength := GetSystemDirectory(PChar(Result), MAX_PATH);

  If Not IsPathDelimiter(Result, iLength) Then
  Begin
    Inc(iLength);
    Result[iLength] := psc;
  End;

  SetLength(Result, iLength);
End;

Function SystemProcessorName : String;
Begin
  Result := SystemCentralProcessorStringRegistryValue('ProcessorNameString');
End;

Function ProcessName : String;
Var
  aBuffer : Array[0..260] Of Char;
Begin
  Result := Copy(aBuffer, 1, GetModuleFileName(HInstance, aBuffer, SizeOf(aBuffer)));
End;

Function IPToString(Const iValue : TIP) : String;
Begin
  Result := StringFormat('%d.%d.%d.%d', [(iValue And $FF000000) Shr 24, (iValue And $FF0000) Shr 16, (iValue And $FF00) Shr 8, iValue And $FF]);
End;

Function HostIP : TIP;
Const
  SIZE_BUFFER = 255;
Var
  aBuffer : Array[0..SIZE_BUFFER] Of AnsiChar;
  pHost : PHostEnt;
Begin
  WinSock.gethostname(PAnsiChar(@aBuffer), SIZE_BUFFER);

  pHost := WinSock.GetHostByName(PAnsiChar(@aBuffer));

  If Not Assigned(pHost) Then
    Result := WinSock.htonl($07000001)  // 127.0.0.1
  Else
    Result := TIP(Pointer(pHost^.h_addr_list^)^);
End;

Function HostName : String;
Const
  SIZE_BUFFER = 255;
Var
  aBuffer : Array[0..SIZE_BUFFER] Of AnsiChar;
Begin
  aBuffer[0] := #0;

  WinSock.gethostname(PAnsiChar(@aBuffer), SIZE_BUFFER);

  If aBuffer = '' Then
    Result := 'localhost'
  Else
    Result := String(aBuffer);
End;

Function LogonName : String;
Var
  aBuffer : Array[0..255] Of Char;
  iSize   : Cardinal;
Begin
  iSize := 255;

  If GetUserName(aBuffer, iSize) Then
    Result := aBuffer
  Else
    Result := '';
End;


Function DomainLogonName : String;
Begin
  Result := DomainName;

  If Result <> '' Then
    Result := Result + '\';

  Result := Result + LogonName;
End;


Function LocalComputerName : String;
Const
  MAX_COMPUTERNAME_LENGTH = 256;
Var
  aBuffer : Array[0..MAX_COMPUTERNAME_LENGTH] Of Char;
  iSize   : Cardinal;
Begin
  iSize := MAX_COMPUTERNAME_LENGTH;

  If GetComputerName(aBuffer, iSize) Then
    Result := aBuffer
  Else
    Result := '';
End;


Function IsRemoteSession : Boolean;
Const
  SM_REMOTESESSION = $1000;
Begin
  Result := GetSystemMetrics(SM_REMOTESESSION) <> 0;
End;


Type

  TComputerNameFormat = (
    ComputerNameNetBIOS,
    ComputerNameDnsHostname,
    ComputerNameDnsDomain,
    ComputerNameDnsFullyQualified,
    ComputerNamePhysicalNetBIOS,
    ComputerNamePhysicalDnsHostname,
    ComputerNamePhysicalDnsDomain,
    ComputerNamePhysicalDnsFullyQualified,
    ComputerNameMax
  );

Function WTSQuerySessionInformationA(hServer : THandle; SessionId : Cardinal; WTSInfoClass : Integer; Var ppBuffer : Pointer; Var pBytesReturned : Cardinal) : Boolean; Stdcall; external 'wtsapi32.dll';
Procedure WTSFreeMemory(pMemory : Pointer); Stdcall; external 'wtsapi32.dll';


Function RemoteComputerName : String;
Const
  WTS_CURRENT_SERVER_HANDLE : THandle = 0;
  WTS_CURRENT_SESSION : Cardinal = $FFFFFFFF;
Type
  TWtsInfoClass = (WtsInfoClassInitialProgram, WtsInfoClassApplicationName, WtsInfoClassWorkingDirectory,
    WtsInfoClassOemId, WtsInfoClassSessionId, WtsInfoClassUserName, WtsInfoClassWinStationName,
    WtsInfoClassDomainName, WtsInfoClassConnectState, WtsInfoClassClientBuildNumber,
    WtsInfoClassClientName, WtsInfoClassClientDirectory, WtsInfoClassClientProductId,
    WtsInfoClassClientHardwareId, WtsInfoClassClientAddress, WtsInfoClassClientDisplay,
    WtsInfoClassClientProtocolType, WtsInfoClassIdleTime, WtsInfoClassLogonTime,
    WtsInfoClassIncomingBytes, WtsInfoClassOutgoingBytes, WtsInfoClassIncomingFrames, WtsInfoClassOutgoingFrames);
Var
  pData : Pointer;
  iLength : Cardinal;
Begin
  If WTSQuerySessionInformationA(WTS_CURRENT_SERVER_HANDLE, WTS_CURRENT_SESSION, Integer(WtsInfoClassClientName), pData, iLength) Then
  Begin
    Result := PChar(pData);
    WTSFreeMemory(pData);
  End
  Else
    Result := '';
End;

Function DomainName : String;
Type
  PSID = Pointer;

  SID_AND_ATTRIBUTES = Record
    Sid : PSID;
    Attributes : DWORD;
  End;

  PTOKEN_USER = ^TOKEN_USER;
  TOKEN_USER = Record
    User : SID_AND_ATTRIBUTES;
  End;
Var
  hProcess, hAccessToken : THandle;
  InfoBuffer : Array[0..999] Of Char;
  szAccountName, szDomainName: Array[0..199] Of Char;
  dwInfoBufferSize : Cardinal;
  dwAccountSize : Cardinal;
  dwDomainSize: Cardinal;
  pTokenUser : PTOKEN_USER;
  snu : SID_NAME_USE;
  oRegistry : TRegIniFile;
Begin
  Result := '';

  If SystemIsWindowsNT Then
  Begin
    pTokenUser := PTOKEN_USER(@InfoBuffer);

    dwAccountSize := 200;
    dwDomainSize := 200;

    hProcess := GetCurrentProcess;

    OpenProcessToken(hProcess, TOKEN_READ, hAccessToken);

    GetTokenInformation(hAccessToken, TokenUser, @InfoBuffer, 1000, dwInfoBufferSize);

    LookupAccountSid(Nil, pTokenUser.User.Sid, szAccountName, dwAccountSize, szDomainName, dwDomainSize, snu);

    CloseHandle(hAccessToken);

    Result := szDomainName;
  End
  Else
  Begin
    oRegistry := TRegINIFile.Create('');
    Try
      oRegistry.RootKey := HKEY_LOCAL_MACHINE;

      If oRegistry.OpenKey('System\CurrentControlSet\Services\MSNP32', False) Then
        Result := oRegistry.ReadString('NetWorkProvider', 'AuthenticatingAgent', '')
      Else If oRegistry.OpenKey('SYSTEM\CurrentControlSet\Services\VxD', False) Then
        Result := oRegistry.ReadString('VNetSup', 'WorkGroup', '');

      oRegistry.CloseKey;
    Finally
      oRegistry.Free;
    End;
  End;
End;


Function ProxyServer : String;
Var
  oRegistry: TRegistry;
Begin
  oRegistry := TRegistry.Create;
  Try
    oRegistry.RootKey := HKEY_CURRENT_USER;

    If oRegistry.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Internet Settings') And oRegistry.ValueExists('ProxyEnable') And (oRegistry.ReadInteger('ProxyEnable') = 1) And oRegistry.ValueExists('ProxyServer') Then
      Result := oRegistry.ReadString('ProxyServer')
    Else
      Result := '';
  Finally
   oRegistry.Free;
  End;
End;


Function MonitorInfoFromRect(aRect : TRect): TMonitorInfo;
Var
  pMonitorInformation : PMonitorInfo;
Begin
  MemoryCreate(pMonitorInformation, SizeOf(TMonitorInfo));
  Try
    pMonitorInformation^.cbSize := SizeOf(TMonitorInfo);

    GetMonitorInfo(MonitorFromRect(@aRect, MONITOR_DEFAULTTONEAREST), pMonitorInformation);

    Result := pMonitorInformation^;
  Finally
    MemoryDestroy(pMonitorInformation, SizeOf(TMonitorInfo));
  End;
End;

{$ENDIF}

class procedure TFileLauncher.Open(const FilePath: string);
begin
{$IFDEF MSWINDOWS}
ShellExecute(0, 'OPEN', PChar(FilePath), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}

{$IFDEF MACOS}
_system(PAnsiChar('open '+'"'+AnsiString(FilePath)+'"'));
{$ENDIF MACOS}
end;


Const
  STRING_BOOLEAN : Array[Boolean] Of String = ('False', 'True');
  PLURAL_EXCEPTIONS : Array [0..0] Of String = ('series');

{$IFDEF FPC}
Function CharInSet(C: Char; Const CharSet: TCharSet): Boolean;
Begin
  Result := C In CharSet;
End;
{$ENDIF}

{$IFDEF VER130}

Function CharInSet(C: AnsiChar; Const CharSet: TCharSet): Boolean;
Begin
  Result := C In CharSet;
End;


Function CharInSet(C: WideChar; Const CharSet: TCharSet): Boolean;
Begin
  Result := (C < #1#0) And (AnsiChar(C) In CharSet);
End;
{$ENDIF}



{$IFOPT C+}
Procedure CharArrayError(Const sConstant, sMessage : String);
Begin
  raise ELibraryException.create('(FHIR.Support.Utilities.' + sConstant + '): ' + sMessage);
End;


Procedure CharArrayVerify(Const CharArray : TCharArray; Const ConstantName : String);
Var
  CharIndex : Integer;
  Current : WideChar;
  Previous : WideChar;
Begin
  If (Length(CharArray) = 0) Then
    CharArrayError(ConstantName, 'Must have at least one element.');

  Previous := CharArray[Low(CharArray)];

  For CharIndex := Low(CharArray) + 1 To High(CharArray) Do
  Begin
    Current := CharArray[CharIndex];

    If (Previous > Current) Then
      CharArrayError(ConstantName, 'Must have all elements declared in ascending order.');
  End;
End;
{$ENDIF}

Procedure StringAppend(Var sTarget : String; Const sSource, sDelimiter : String);
Begin
  If sTarget = '' Then
    sTarget := sSource
  Else
    sTarget := sTarget + sDelimiter + sSource;
End;


Function StringTruncateStart(Const sValue : String; iLength : Integer) : String;
Var
  iCurrent : Integer;
Begin
  iCurrent := Length(sValue);

  If iLength >= iCurrent Then
    Result := sValue
  Else
    Result := Copy(sValue, iCurrent - iLength + 1, iLength);
End;

Function StringCopy(Const sValue : String; iIndex, iCount : Integer) : String;
Begin
  Result := System.Copy(sValue, iIndex, iCount);
End;


Function StringCompare(Const sA, sB : String) : Integer;
Begin
  Result := StringCompareInsensitive(sA, sB);
End;


Function StringCompareSensitive(Const sA, sB : String) : Integer;
Begin
  Result := CompareStr(sA, sB);
End;


Function StringCompareInsensitive(Const sA, sB : String; Const iLength : Integer) : Integer;
Begin
  Result := StrLIComp(PChar(sA), PChar(sB), iLength);
End;


Function StringCompareSensitive(Const sA, sB : String; Const iLength : Integer) : Integer;
Begin
  Result := StrLComp(PChar(sA), PChar(sB), iLength);
End;


Function StringCompare(Const sA, sB : String; Const iLength : Integer) : Integer;
Begin
  Result := StrLIComp(PChar(sA), PChar(sB), iLength);
End;

Function StringCompareInsensitive(Const sA, sB : String) : Integer;
Begin
  Result := CompareText(sA, sB);
End;


Function StringEqualsInsensitive(Const sA, sB : String) : Boolean;
Begin
  Result := StringCompareInsensitive(sA, sB) = 0;
End;

Function StringEquals(Const sA, sB : String) : Boolean;
Begin
  Result := StringEqualsInsensitive(sA, sB);
End;

Function StringEqualsSensitive(Const sA, sB : String; Const iLength : Integer) : Boolean;
Begin
  Result := StringCompareSensitive(sA, sB, iLength) = 0;
End;

Function StringEqualsInsensitive(Const sA, sB : String; Const iLength : Integer) : Boolean;
Begin
  Result := StringCompareInsensitive(sA, sB, iLength) = 0;
End;


Function StringEquals(Const sA, sB : String; Const iLength : Integer) : Boolean;
Begin
  Result := StringEqualsInsensitive(sA, sB, iLength);
End;

Function StringFormat(Const sFormat : String; Const aArgs : Array Of Const) : String;
Begin
  FmtStr(Result, sFormat, aArgs);
End;

Function StringStrip(Const sValue, sFind : String) : String;
Begin
  Result := StringReplace(sValue, sFind, '');
End;

Function StringStrip(Const sValue : String; Const aFind : TCharSet) : String;
Begin
  Result := StringReplace(sValue, aFind, '');
End;



Function StringStripWhitespace(Const sValue : String) : String; Overload;
Begin
  Result := StringStrip(sValue, setWhitespace);
End;



Function StringStripDuplicatesSensitive(Const sValue : String) : String;
Var
  cLast : Char;
  cCurrent : Char;
  iLoop : Integer;
Begin 
  Result := sValue;

  If Result <> '' Then
  Begin 
    cLast := Result[Length(Result)];

    For iLoop := Length(Result) - 1 DownTo 1 Do
    Begin
      cCurrent := Result[iLoop];

      If cCurrent = cLast Then
        Delete(Result, iLoop + 1, 1);

      cLast := cCurrent;
    End;
  End;  
End;  


Function StringStripDuplicatesInsensitive(Const sValue : String) : String;
Var
  cLast : Char;
  cCurrent : Char;
  iLoop : Integer;
Begin 
  Result := sValue;

  If Result <> '' Then
  Begin
    cLast := upcase(Result[Length(Result)]);

    For iLoop := Length(Result) - 1 DownTo 1 Do
    Begin
      cCurrent := Upcase(Result[iLoop]);

      If cCurrent = cLast Then
        Delete(Result, iLoop + 1, 1);

      cLast := cCurrent;
    End;
  End;
End;

Function StringStripDuplicates(Const sValue : String) : String;
Begin
  Result := StringStripDuplicatesInsensitive(sValue);
End;

Function StringGet(Const sValue : String; iIndex : Integer) : Char;
Begin
  If (iIndex < 1) Or (iIndex > Length(sValue)) Then
    Result := #0
  Else
    Result := sValue[iIndex];
End;


Function StringSplit(Const sValue, sDelimiter : String; Var sLeft, sRight: String) : Boolean;
Var
  iIndex : Integer;
  sA, sB : String;
Begin
  // Find the delimiter within the source string
  iIndex := Pos(sDelimiter, sValue);
  Result := iIndex <> 0;

  If Not Result Then
  Begin
    sA := sValue;
    sB := '';
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + Length(sDelimiter), MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;

Function StringSplitAnsi(Const sValue, sDelimiter : AnsiString; Var sLeft, sRight: AnsiString) : Boolean;
Var
  iIndex : Integer;
  sA, sB : AnsiString;
Begin
  // Find the delimiter within the source string
  iIndex := Pos(sDelimiter, sValue);
  Result := iIndex <> 0;

  If Not Result Then
  Begin
    sA := sValue;
    sB := '';
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + Length(sDelimiter), MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;


Function StringSplit(Const sValue : String; Const aDelimiters : TCharSet; Var sLeft, sRight: String) : Boolean;
Var
  iIndex : Integer;
  sA, sB : String;
Begin 
  // Find the delimiter within the source string
  iIndex := StringFind(sValue, aDelimiters);
  Result := iIndex <> 0;

  If Not Result Then
  Begin 
    sA := sValue;
    sB := '';
  End   
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + 1, MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;


Function StringReversePos(Const cFind : Char; Const sValue : String) : Integer; overload;
Begin
  Result := Length(sValue);
  While (Result > 0) And (sValue[Result] <> cFind) Do
    Dec(Result);
End;


Function StringReversePos(Const aFinds : TCharSet; Const sValue : String) : Integer; overload;
Begin
  Result := Length(sValue);
  While (Result > 0) And Not CharInSet(sValue[Result], aFinds) Do
    Dec(Result);
End;


Function StringReversePos(Const sFind : String; Const sValue : String) : Integer; Overload;
Begin
  Result := Length(sValue);
  While (Result > 0) And Not StringEquals(Copy(sValue, Result, Length(sFind)), sFind) Do
    Dec(Result);
End;


Function StringSplitRight(Const sValue, sDelimiter : String; Var sLeft, sRight: String) : Boolean;
Var
  iIndex : Integer;
  sA, sB : String;
Begin
  // Find the delimiter within the source string
  iIndex := StringReversePos(sDelimiter, sValue);
  Result := iIndex <> 0;

  If Not Result Then
  Begin
    sA := '';
    sB := sValue;
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + Length(sDelimiter), MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;


Function StringSplitRight(Const sValue : String; Const aDelimiters : TCharSet; Var sLeft, sRight: String) : Boolean;
Var
  iIndex : Integer;
  sA, sB : String;
Begin
  // Find the delimiter within the source string
  iIndex := StringReversePos(aDelimiters, sValue);
  Result := iIndex <> 0;

  If Not Result Then
  Begin
    sA := '';
    sB := sValue;
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + 1, MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;

Function StringFindSensitive(Const sValue, sFind : String) : Integer;
Begin
  Result := Pos(sFind, sValue);
End;


Function StringExistsSensitive(Const sValue, sFind : String) : Boolean;
Begin
  Result := StringFindSensitive(sValue, sFind) > 0;
End;

Function StringFind(Const sValue, sFind : String) : Integer;
Begin
  Result := StringFindSensitive(sValue, sFind);
End;


Function StringFindInsensitive(Const sValue, sFind : String) : Integer;
Begin
  Result := StringFindSensitive(StringUpper(sValue), StringUpper(sFind));
End;


Function StringExistsInsensitive(Const sValue, sFind : String) : Boolean;
Begin
  Result := StringFindInsensitive(sValue, sFind) > 0;
End;

Function StringMultiply(cChar : Char; iCount : Integer) : String;
Var
  Index : Integer;
Begin
  Result := '';

  If iCount > 0 Then
  Begin
    SetLength(Result, iCount);

    For Index := 1 To iCount Do
      Result[Index] := cChar;
  End;
End;

Function StringCount(Const sValue : String; Const aFind : Char) : Integer;
Var
  iLoop : Integer;
Begin
  Result := 0;
  For iLoop := 1 To Length(sValue) Do
    Inc(Result, Integer(sValue[iLoop] = aFind));
End;

Function StringArrayIndexOfSensitive(Const aNames: Array Of String; Const sName: String): Integer;
Begin
  Result := High(aNames);
  While (Result >= Low(aNames)) And (StringCompareSensitive(sName, aNames[Result]) <> 0) Do
    Dec(Result);
End;


Function StringArrayIndexOfInsensitive(Const aNames: Array Of String; Const sName: String): Integer;
Begin
  Result := High(aNames);
  While (Result >= Low(aNames)) And (StringCompareInsensitive(sName, aNames[Result]) <> 0) Do
    Dec(Result);
End;

Function StringArrayToString(Const aNames : Array Of String): String;
var
  s : String;
begin
  result := '';
  for s in aNames do
    result := ', '+s;
  result := result.Substring(1).trim;
end;

Function StringArrayIndexOf(Const aNames: Array Of String; Const sName: String): Integer;
Begin
  Result := StringArrayIndexOfInsensitive(aNames, sName);
End;

Function StringArrayExistsSensitive(Const aNames: Array Of String; Const sName: String): Boolean;
Begin
  Result := StringArrayIndexOfSensitive(aNames, sName) >= 0;
End;


Function StringArrayExistsInsensitive(Const aNames: Array Of String; Const sName: String): Boolean;
Begin
  Result := StringArrayIndexOfInsensitive(aNames, sName) >= 0;
End;


Function StringArrayExists(Const aNames: Array Of String; Const sName: String): Boolean;
Begin
  Result := StringArrayExistsInsensitive(aNames, sName);
End;

Function StringExists(Const sValue, sFind : String) : Boolean;
Begin
  Result := StringExistsInsensitive(sValue, sFind);
End;


Function StringExists(Const sValue : String; Const aFind : TCharSet) : Boolean;
Begin
  Result := StringFind(sValue, aFind) > 0;
End;




Function StringUpper(Const sValue : String) : String;
Begin
  Result := SysUtils.UpperCase(sValue);
End;


Function BooleanToString(Value : Boolean) : String;
Begin
  Result := STRING_BOOLEAN[Value];
End;

Function LCBooleanToString(Value : Boolean) : String;
Begin
  Result := Lowercase(STRING_BOOLEAN[Value]);
End;


Function IntegerToString(Value : Integer) : String;
Begin
  Result := IntToStr(Value);
End;

Function RealToString(Value : Real) : String;
Begin
  Result := FloatToStr(Value);
End;

Function StringIsInteger16(Const sValue : String) : Boolean;
Var
  iValue : Integer;
  iError : Integer;
Begin
  Result := sValue <> '';

  If Result Then
  Begin
    Val(sValue, iValue, iError);

    Result := (iError = 0) And (iValue <= 32767) And (iValue >= -32768);
  End;
End;

Function StringIsInteger32(Const sValue : String) : Boolean;
Var
  iValue : Integer;
  iError : Integer;
Begin
  Result := sValue <> '';

  If Result Then
  Begin
    Val(sValue, iValue, iError);

    Result := (iError = 0) And (iValue = iValue); // 2nd part to remove warning.
  End;
End;


Function StringIsInteger64(Const sValue : String) : Boolean;
Var
  iValue : Int64;
  iError : Integer;
Begin
  Result := sValue <> '';

  If Result Then
  Begin
    Val(sValue, iValue, iError);

    Result := (iError = 0) And (iValue = iValue); // 2nd part to remove warning.
  End;
End;


Function StringToInteger32(Const sValue : String) : Integer;
Begin
  Result := StrToInt(sValue);
End;

Function StringToInteger32(Const sValue : String; iDefault : Integer) : Integer;
Begin
  Result := StrToIntDef(sValue, iDefault);
End;

Function StringToReal(Const sValue : String) : Real;
Begin
  Result := StrToFloat(sValue);
End;

Function StringIsReal(Const sValue : String) : Boolean;
Var
  rDummy : Extended;
Begin
  Result := TextToFloat(PChar(sValue), rDummy, fvExtended);
End;

Function StringToBooleanCheck(Const sValue : String; Out bValue : Boolean) : Boolean;
Begin
  Result := True;

  If StringEquals(STRING_BOOLEAN[False], sValue) Then
    bValue := False
  Else If StringEquals(STRING_BOOLEAN[True], sValue) Then
    bValue := True
  Else
  Begin
    bValue := False;
    Result := False;
  End;
End;

Function StringToBoolean(Const sValue : String) : Boolean;
Begin
  StringToBooleanCheck(sValue, Result);
End;

Function StringToInteger64(Const sValue : String) : Int64;
Begin
  Result := StrToInt64(sValue);
End;


Function StringIsCardinal16(Const sValue : String) : Boolean;
Var
  iValue : Word;
  iError : Integer;
Begin
  Result := sValue <> '';

  If Result Then
  Begin
    Val(sValue, iValue, iError);

    Result := (iError = 0) And (iValue = iValue); // 2nd part to remove warning.
  End;
End;

Function StringIsAlphabetic(Const sValue : String) : Boolean;
Begin
  Result := StringContainsOnly(sValue, setAlphabet);
End;

Function StringIsWhitespace(Const sValue : String) : Boolean;
Begin
  Result := (sValue = '') Or StringContainsOnly(sValue, setWhitespace);
End;

Function StringKeep(Const sValue : String; Const aFind : TCharSet) : String;
Begin
  Result := StringStrip(sValue, setUniversal - aFind);
End;

Function StringFind(Const sValue : String; aFind : TCharSet) : Integer;
Var
  iLength : Integer;
Begin
  iLength := Length(sValue);
  Result := 1;
  While (Result <= iLength) And Not CharInSet(sValue[Result], aFind) Do
    Inc(Result);

  If Result > iLength Then
    Result := 0;
End;

Function StringPadLeft(Const sValue : String; cPad : Char; iLength : Integer) : String;
Begin
  Result := StringMultiply(cPad, iLength - Length(sValue)) + sValue;
End;


Function StringPadRight(Const sValue : String; cPad : Char; iLength : Integer) : String;
Begin
  Result := sValue + StringMultiply(cPad, iLength - Length(sValue));
End;

Function StringTrimWhitespaceLeft(Const sValue : String) : String;
Begin
  Result := SysUtils.TrimLeft(sValue);
End;

Function StringSlice(Const sValue : String; iBegin, iEnd : Integer) : String;
Begin
  Result := Copy(sValue, iBegin, iEnd - iBegin + 1);
End;


Function StringTrimWhitespaceRight(Const sValue : String) : String;
Begin
  Result := SysUtils.TrimRight(sValue);
End;


Function StringTrimSetLeft(Const sValue : String; aChars : TCharSet) : String;
Var
  iStart : Integer;
Begin
  iStart := 1;
  While (iStart < Length(sValue)) And (CharInSet(sValue[iStart], aChars)) Do
    Inc(iStart);

  Result := Copy(sValue, iStart, MaxInt);
End;

Function StringTrimSetRight(Const sValue : String; aChars : TCharSet) : String;
Var
  iFinish : Integer;
Begin
  iFinish := Length(sValue);
  While (iFinish >= 1) And CharInSet(sValue[iFinish], aChars) Do
    Dec(iFinish);

  Result := Copy(sValue, 1, iFinish);
End;


Function StringTrimWhitespace(Const sValue : String) : String;
Begin
  Result := SysUtils.Trim(sValue);
End;

Function StringTrimSet(Const sValue : String; aChars : TCharSet) : String;
Var
  iStart, iFinish : Integer;
Begin
  iStart := 1;
  While (iStart < Length(sValue)) And CharInSet(sValue[iStart], aChars) Do
    Inc(iStart);

  iFinish := Length(sValue);
  While (iFinish >= iStart) And CharInSet(sValue[iFinish], aChars) Do
    Dec(iFinish);

  Result := Copy(sValue, iStart, iFinish - iStart + 1);
End;


Function StringIncludeAfter(Const sText, sSymbol : String) : String;
Var
  iSymbol, iText : Integer;
Begin
  iSymbol := Length(sSymbol);
  iText := Length(sText);

  If (iText < iSymbol) Or (StringCompare(Copy(sText, iText - iSymbol + 1, iSymbol), sSymbol) <> 0) Then
    Result := sText + sSymbol
  Else
    Result := sText;
End;

Function StringExcludeBefore(Const sText, sSymbol : String) : String;
Begin
  If (StringCompare(sSymbol, sText, Length(sSymbol)) <> 0) Then
    Result := sText
  Else
    Result := Copy(sText, Length(sSymbol) + 1, MaxInt);
End;

Function StringExcludeAfter(Const sText, sSymbol : String) : String;
Var
  iSymbol, iText : Integer;
Begin
  iSymbol := Length(sSymbol);
  iText := Length(sText);

  If (iText >= iSymbol) And (StringCompare(Copy(sText, iText - iSymbol + 1, iSymbol), sSymbol) = 0) Then
    Result := Copy(sText, 1, iText - iSymbol)
  Else
    Result := sText;
End;

Function StringReplaceBefore(Const sValue, sFind, sReplace : String) : String;
Begin
  If StringStartsWith(sValue, sFind) Then
    Result := sReplace + StringExcludeBefore(sValue, sFind)
  Else
    Result := sValue;
End;


Function StringReplaceAfter(Const sValue, sFind, sReplace : String) : String;
Begin
  If StringEndsWith(sValue, sFind) Then
    Result := StringExcludeAfter(sValue, sFind) + sReplace
  Else
    Result := sValue;
End;

Function StringContainsOnly(Const sValue : String; Const aSet : TCharSet) : Boolean;
Var
  iLoop : Integer;
Begin
  Result := True;
  iLoop := 1;
  While (iLoop <= Length(sValue)) And Result Do
  Begin
    Result := CharInSet(sValue[iLoop], aSet);
    Inc(iLoop);
  End;
End;

Function StringContainsAny(Const sValue : String; Const aSet : TCharSet) : Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  iLoop := 1;
  While (iLoop <= Length(sValue)) And Not Result Do
  Begin
    Result := CharInSet(sValue[iLoop], aSet);
    Inc(iLoop);
  End;
End;

Function StringStartsWith(Const sValue, sFind : String; sensitive : boolean = false) : Boolean;
Begin
  if sensitive then
    Result := StringStartsWithSensitive(sValue, sFind)
  else
    Result := StringStartsWithInsensitive(sValue, sFind);
End;



Function StringEqualsSensitive(Const sA, sB : String) : Boolean;
Begin
  Result := StringCompareSensitive(sA, sB) = 0;
End;



Function EqualsSensitive(Const sA, sB : String) : Boolean;
Begin
  Result := StringEqualsSensitive(sA, sB);
End;


Function StringStartsWithSensitive(Const sValue, sFind : String) : Boolean;
Begin
  Result := EqualsSensitive(Copy(sValue, 1, Length(sFind)), sFind);
End;


Function StringStartsWithInsensitive(Const sValue, sFind : String) : Boolean;
Begin
  Result := StringEqualsInsensitive(Copy(sValue, 1, Length(sFind)), sFind);
End;

Function StringReplaceInsensitive(Const sValue, sFind, sReplace : String) : String;
Begin
  Result := SysUtils.StringReplace(sValue, sFind, sReplace, [rfReplaceAll, rfIgnoreCase]);
End;


Function StringEndsWith(Const sValue, sFind : String; sensitive : boolean = false) : Boolean;
Begin
  if sensitive then
    Result := StringEndsWithSensitive(sValue, sFind)
  else
    Result := StringEndsWithInsensitive(sValue, sFind);
End;


Function StringEndsWithSensitive(Const sValue, sFind : String) : Boolean;
Begin
  Result := EqualsSensitive(Copy(sValue, Length(sValue) - Length(sFind) + 1, Length(sFind)), sFind);
End;


Function StringEndsWithInsensitive(Const sValue, sFind : String) : Boolean;
Begin
  Result := StringEqualsInsensitive(Copy(sValue, Length(sValue) - Length(sFind) + 1, Length(sFind)), sFind);
End;


Function StringReplace(Const sValue, sFind, sReplace : String) : String;
Begin
  Result := StringReplaceInsensitive(sValue, sFind, sReplace);
End;


Function StringReplace(Const sValue : String; Const aFind : TCharSet; cReplace : Char) : String;
Var
  cChar  : Char;
  iLoop  : Integer;
  iCount : Integer;
Begin
  iCount := Length(sValue);

  SetLength(Result, iCount);

  For iLoop := 1 To iCount Do
  Begin
    cChar := sValue[iLoop];

    If CharInSet(cChar, aFind) Then
      Result[iLoop] := cReplace
    Else
      Result[iLoop] := cChar;
  End;
End;

Function StringReplace(Const sValue : String; Const aFind : TCharSet; Const sReplace : String) : String;
Var
  cChar : Char;
  iLoop : Integer;
Begin
  Result := '';

  For iLoop := 1 To Length(sValue) Do
  Begin
    cChar := sValue[iLoop];

    If CharInSet(cChar, aFind) Then
      Result := Result + sReplace
    Else
      Result := Result + cChar;
  End;
End;


Function StringReplaceAll(Const sValue, sFind, sReplace : String; Const iStartPos : Integer = 1; Const iEndPos : Integer = 0) : String;
Var
  iFind : Integer;
  iIndex : Integer;
  iReplace : Integer;
Begin
  If (iStartPos = 1) And (iEndPos = 0) Then
    Result := StringReplace(sValue, sFind, sReplace)
  Else
  Begin
    Result := sValue;

    iFind := Length(sFind);
    iIndex := iStartPos;
    iReplace := Length(sReplace);

    While iIndex <= (Length(Result) - iEndPos) Do
    Begin
      If StringEquals(Copy(Result, iIndex, iFind), sFind) Then
      Begin
        Delete(Result, iIndex, iFind);
        Insert(sReplace, Result, iIndex);

        Inc(iIndex, iReplace - 1);
      End;

      Inc(iIndex);
    End;
  End;
End;

Function StringLength(Const sValue : String) : Integer;
Begin
  Result := System.Length(sValue);
End;

function GetStringCell(const ADelimitedString: String; ACell: Cardinal; ADelimiter: String): String; Overload;
  // returns the string corresponding to cell ACell in a delimited string
  // first cell is 0. returns '' if ACell > actual number
var
  j, k: Integer;
begin
  Result := ADelimitedString;
  for k := 1 to ACell do
    begin
    j := Pos(ADelimiter, Result);
    if j = 0 then
      begin
      Result := '';
      break;
      end;
    Result := copy(Result, j + length(ADelimiter), length(Result));
    end;
  j := Pos(ADelimiter, Result);
  if j <> 0 then
    Result := copy(Result, 1, j - 1);
end;

function SQLWrapString(const AStr: String): String;
var
  i: Integer;
begin
  Result := AStr;
  for i := Length(Result) downto 1 do
    if Result[i] = '''' then
      Insert('''', Result, i);
end;

function SQLWrapStrings(const AStr: String): String;
var
  sl : TArray<String>;
  b : TFslStringBuilder;
  s : String;
  first : boolean;
begin
  sl := aStr.Split([',']);
  b := TFslStringBuilder.Create;
  try
    first := true;
    for s in sl do
    begin
      if first then
        first := false
      else
        b.Append(',');
      b.Append('''');
      b.Append(sqlwrapstring(s));
      b.Append('''');
    end;
    result := b.ToString;
  finally
    b.free;
  end;
end;

function AppendForwardSlash(const AStr: String): String;
begin
  if length(AStr) = 0 then
    Result := '/'
  else if AStr[length(AStr)] = '/' then
    Result := AStr
  else
    Result := AStr + '/';
end;

Function DescribeBytes(i : int64) : String;
Begin
  Case i Of
    0..1000:
      Result := IntToStr(i) + ' bytes';
    1001..1000000:
      Result := floattostrF(i / 1024, ffFixed, 18, 1) + ' KB';
    1000001..1000000000:
      Result := floattostrF(i / 1048576, ffFixed, 18, 1) + ' MB';
  Else
    Result := floattostrF(i / 1073741824, ffFixed, 18, 1) + ' GB';
  End;
End;

procedure CommaAdd(var AStr: String; AStrToAdd: String);
begin
  if AStr = '' then
    AStr := AStrToAdd
  else
    AStr := AStr + ', ' + AStrToAdd;
end;

function RemoveQuotes(AStr: String; AUpperCaseString: Boolean = false): String;
begin
  if Length(AStr) >= 1 then
    if AStr[1] = '"' then
      Delete(AStr, 1, 1);
  if Length(AStr) >= 1 then
    if AStr[Length(AStr)] = '"' then
      Delete(AStr, Length(AStr), 1);
  if AUpperCaseString then
    Result := UpperCase(AStr)
  else
    Result := AStr;
end;

function isNumeric(AChar: Char): Boolean;
begin
  Result := CharInSet(AChar, ['0'..'9']);
end;


function IsNumericString(st: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(st) do
    begin
    if not IsNumeric(st[i]) then
      begin
      Result := False;
      Exit;
      end;
    end;
end;

function isAbsoluteUrl(s: String): boolean;
begin
  result := s.StartsWith('urn:') or s.StartsWith('http:') or s.StartsWith('https:') or s.StartsWith('ftp:');
end;

function charLower(ch : char) : char;
begin
  result := lowercase(ch){$IFNDEF FPC}[1]{$ENDIF};
end;

function charLower(s : string) : char;
begin
  if s = '' then
    result := #0
  else
    result := lowercase(s)[1];
end;

function charUpper(ch : char) : char;
begin
  result := uppercase(ch)[1];
end;

function charUpper(s : string) : char;
begin
  if s = '' then
    result := #0
  else
    result := uppercase(s)[1];
end;

Function StringTitleCase(Const sValue: String; Const sExceptions : Array of String): String; Overload;
var
  i, j : integer;
  iSkip: Integer;
  bOk : Boolean;
  bStartWord : Boolean;
Begin
  result := sValue;
  i := 1;
  iSkip := 0;
  bStartWord := true;
  While (i <= length(result)) do
  Begin
    If (Result[i] = ' ') Or (Result[i] = '/') Then
      bStartWord := True
    else if CharInSet(result[i], ['a'..'z', 'A'..'Z']) Then
    Begin
      bOk := true;
      If bStartWord then
        For j := 0 to High(sExceptions) Do
          If lowercase(copy(result, i, length(sExceptions[j]))) = lowercase(sExceptions[j]) Then
          Begin
            bOk := false;
            iSkip := length(sExceptions[j]) - 1;
          End;
      If bOk Then
      Begin
        If bStartWord Then
          Result[i] := UpCase(Result[i])
        Else
          Result[i] := LowerCase(Result[i]){$IFNDEF FPC}[1]{$ENDIF};
      End
      Else
        i := i + iSkip;
      bStartWord := false;
    End;

    inc(i);
  End;
End;

Function StringDelimiterCase(Const sValue : String; Const aDelimiters : TCharSet) : String;
Var
  iLoop: Integer;
  bStartWord: Boolean;
Begin
  Result := SysUtils.LowerCase(sValue);

  bStartWord := True;
  For iLoop := 1 To Length(Result) Do
  Begin
    If CharInSet(Result[iLoop], aDelimiters) Then
      bStartWord := True
    Else If bStartWord Then
    Begin
      bStartWord := False;
      Result[iLoop] := UpCase(Result[iLoop]);
    End;
  End;
End;


Function StringTitleCase(Const sValue: String): String;
Begin
  Result := StringDelimiterCase(sValue, [' ']);
End;

Function StringToggleCase(Const sValue: String): String;
Var
  iLoop: Integer;
Begin
  Result := SysUtils.LowerCase(sValue);

  For iLoop := 1 To Length(Result) Do
  Begin
    If Result[iLoop] = sValue[iLoop] Then
      Result[iLoop] := UpCase(Result[iLoop]);
  End;
End;

function capitalise(s : String) : String;
begin
  if (s = '') then
    result := s
  else
    result := UpperCase(s[1]) + s.Substring(1);
end;

function uncapitalise(s : String) : String; overload;
begin
  if (s = '') then
    result := s
  else
    result := LowerCase(s[1]) + s.Substring(1);
end;

Function StringSentence(Const sValue : String) : String;
Begin
  Result := Lowercase(sValue);

  If Length(Result) > 1 Then
    Result[1] := charUpper(Result[1]);
End;



// http://stackoverflow.com/questions/1891196/convert-hi-ansi-chars-to-ascii-equivalent-e-e-in-delphi2007/1892432#1892432

function RemoveAccents(const aStr: String): String;
type
  USASCIIString = type AnsiString(20127);//20127 = us ascii
begin
  Result := String(USASCIIString(aStr));
end;

// http://stackoverflow.com/questions/6077258/theres-a-uinttostr-in-delphi-to-let-you-display-uint64-values-but-where-is-strt
function TryStrToUINT64(StrValue:String; var uValue:UInt64 ):Boolean;
var
  Start,Base,Digit:Cardinal;
  n:Integer;
  Nextvalue:UInt64;
begin
  result := false;
  Base := 10;
  Start := 1;
  StrValue := Trim(UpperCase(StrValue));
  if StrValue='' then
    exit;
  if StrValue[1]='-' then
    exit;
  if StrValue[1]='$' then
  begin
    Base := 16;
    Start := 2;
    if Length(StrValue)>17 then // $+16 hex digits = max hex length.
        exit;
  end;
  uValue := 0;
  for n := Start to Length(StrValue) do
  begin
      if StrValue[n].IsDigit then
          Digit := Ord(StrValue[n])-Ord('0')
      else if  (Base=16) and (StrValue[n] >= 'A') and (StrValue[n] <= 'F') then
          Digit := (Ord(StrValue[n])-Ord('A'))+10
      else
          exit;// invalid digit.

      Nextvalue := (uValue*base)+digit;
      if (Nextvalue<uValue) then
          exit;
      uValue := Nextvalue;
  end;
  result := true; // success.
end;

function StrToUINT64(Value:String):UInt64;
begin
  if not TryStrToUINT64(Value,result) then
    raise EConvertError.Create('Invalid uint64 value "'+value+'"');
end;

function StrToUInt64Def(Value:String; def : UInt64):UInt64;
begin
  if not TryStrToUINT64(Value,result) then
    result := def;
end;

function EnumIsValid(ATypeInfo: PTypeInfo; AIndex: Integer): Boolean;
var
  LTypeData: PTypeData;
begin
  // no check on AIndex
  if ATypeInfo^.Kind = tkEnumeration then
    begin
    LTypeData := GetTypeData(ATypeInfo);
    Result := (AIndex >= LTypeData^.MinValue) and (AIndex <= LTypeData^.MaxValue);
    end
  else
    Result := False;
end;

const
  RS_ERR_ENGINE_BAD_ENUM_TYPE = 'Enumeration %s has irregular values';
  RS_ERR_ENGINE_ENUM_OUT_RANGE = 'The Value %s is out of range for the type %s';
  RS_ERR_ENGINE_NOT_ENUM_TYPE = 'The Type %s is not an enumerated type';
  RS_ERR_ENGINE_ENUM_UNIT_WRONG = 'RTTI Error, Unit for Enumeration "%s" is %s';

function EnumStrIsValid(ATypeInfo: PTypeInfo; AValue : string): Boolean;
var
  LTypeData: PTypeData;
  LPChar: PChar;
  LCount : integer;
  LValue : ShortString;
begin
  LValue := ShortString(AValue);

  if ATypeInfo^.Kind = tkEnumeration then
    begin
    LTypeData := GetTypeData(ATypeInfo);
    if LTypeData^.MinValue <> 0 then
      begin
      raise ELibraryException.create(Format(RS_ERR_ENGINE_BAD_ENUM_TYPE, [ATypeInfo^.Name]))
      end;
    LPChar := @LTypeData^.NameList[0];
    LCount := 0;
    while (LCount <= LTypeData^.MaxValue) and (ShortString(pointer(LPChar)^) <> LValue) do
      begin
      inc(LPChar, Ord(LPChar^) + 1);  // move to next string
      inc(LCount);
      end;
    result := LCount <= LTypeData^.MaxValue;
    end
  else
    begin
    raise ELibraryException.create(Format(RS_ERR_ENGINE_NOT_ENUM_TYPE, [ATypeInfo^.Name]))
    end;
end;

function EnumToString(ATypeInfo: PTypeInfo; AIndex: Integer): String;
var
  i: Integer;
  LTypeData: PTypeData;
  LPChar: PAnsiChar;
begin
  // no check on AIndex (Yet)
  if ATypeInfo^.Kind = tkEnumeration then
    begin
    LTypeData := GetTypeData(ATypeInfo);
    if LTypeData^.MinValue <> 0 then
      begin
      raise ELibraryException.create(Format(RS_ERR_ENGINE_BAD_ENUM_TYPE, [ATypeInfo^.Name]))
      end;
    if AIndex > LTypeData^.MaxValue then
      begin
      raise ELibraryException.create(Format(RS_ERR_ENGINE_ENUM_OUT_RANGE, [IntToStr(AIndex), ATypeInfo^.Name]))
      end;
    LPChar := PAnsiChar(@LTypeData^.NameList[0]);
    for i := 1 to AIndex do
      begin
      inc(LPChar, Ord(LPChar^) + 1);  // move to next string
      end;
    Result := String(ShortString(pointer(LPChar)^));
    end
  else
    raise ELibraryException.create(Format(RS_ERR_ENGINE_NOT_ENUM_TYPE, [ATypeInfo^.Name]))
end;

function StringToEnum(ATypeInfo: PTypeInfo; const AStr: String): Integer;
var
  LTypeData: PTypeData;
  LPChar: PAnsiChar;
  LValue : ShortString;
begin
  LValue := ShortString(AStr);

  if ATypeInfo^.Kind = tkEnumeration then
    begin
    LTypeData := GetTypeData(ATypeInfo);
    if LTypeData^.MinValue <> 0 then
      begin
      raise ELibraryException.create(Format(RS_ERR_ENGINE_BAD_ENUM_TYPE, [ATypeInfo^.Name]))
      end;
    LPChar := @LTypeData^.NameList[0];
    Result := 0;
    while (Result <= LTypeData^.MaxValue) and (ShortString(pointer(LPChar)^) <> LValue) do
      begin
      inc(LPChar, Ord(LPChar^) + 1);  // move to next string
      inc(Result);
      end;
    if Result > LTypeData^.MaxValue then
      begin
      raise ELibraryException.create(Format(RS_ERR_ENGINE_ENUM_OUT_RANGE, [AStr, ATypeInfo^.Name]))
      end;
    end
  else
    begin
    raise ELibraryException.create(Format(RS_ERR_ENGINE_NOT_ENUM_TYPE, [ATypeInfo^.Name]))
    end;
end;


Function StringIncludeAround(Const sText, sLeft, sRight : String) : String;
Begin
  Result := StringIncludeAfter(StringIncludeBefore(sText, sLeft), sRight);
End;


Function StringIncludeAround(Const sText, sSymbol : String) : String;
Begin
  Result := StringIncludeAfter(StringIncludeBefore(sText, sSymbol), sSymbol);
End;


Function StringIncludeBefore(Const sText : String; cSymbol : Char) : String;
Begin
  Result := sText;
  If (Length(Result) = 0) Or (Result[1] <> cSymbol) Then
    Result := cSymbol + Result;
End;


Function StringIncludeBefore(Const sText, sSymbol : String) : String;
Begin
  Result := sText;
  If (StringCompare(sSymbol, sText, Length(sSymbol)) <> 0) Then
    Result := sSymbol + Result;
End;


Function StringIncludeAfter(Const sText : String; cSymbol : Char) : String;
Begin
  Result := sText;
  If (Length(Result) > 0) And (Result[Length(Result)] <> cSymbol) Then
    Result := Result + cSymbol;
End;




Function StringExcludeAround(Const sText, sSymbol : String) : String;
Begin
  Result := StringExcludeAround(sText, sSymbol, sSymbol);
End;


Function StringExcludeAround(Const sText, sLeft, sRight : String) : String;
Begin
  Result := StringExcludeAfter(StringExcludeBefore(sText, sLeft), sRight);
End;


Function StringExcludeBefore(Const sText : String; cSymbol : Char) : String;
Begin
  Result := sText;
  If (Length(sText) > 0) And (sText[1] = cSymbol) Then
    Delete(Result, 1, 1);
End;



Function StringExcludeAfter(Const sText : String; cSymbol : Char) : String;
Begin
  Result := sText;
  If (Length(sText) > 0) And (sText[Length(sText)] = cSymbol) Then
    Delete(Result, Length(sText), 1);
End;


function jsonEscape(s : String; isString : boolean) : String;
var
  b : TFslStringBuilder;
  c : char;
begin
  if s = '' then
    exit('');
  b := TFslStringBuilder.Create;
  try
    for c in s do
    begin
      if (c = #13) then
        b.append('\r')
      else if (c = #10) then
        b.append('\n')
      else if (c = '"') then
        b.append('\"')
      else if (c = '\') then
        b.append('\\')
      else if (c = '/') and not isString then
        b.append('\/')
      else
        b.append(c);
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function StringFindEndOfNumber(const s : String; index : integer) : integer;
var
  dec : boolean;
begin
  result := index;
  if s.Length <= index then
    exit;
  if s[index] = '-' then
    inc(index);
  if s.Length <= result then
    exit;
  if CharInSet(s[index], ['0'..'9']) then
  begin
    dec := false;
    while (index <= s.Length) and (CharInSet(s[index], ['0'..'9']) or (not dec and (s[index] = '.'))) do
    begin
      if s[index] = '.' then
        dec := true;
      inc(index);
    end;
    exit(index-1);
  end;
end;


Function isOid(oid : String) : Boolean;
var
  regex : TRegEx;
Begin
  if (pos('.', oid) = 0) or (length(oid) > 64) then
    result := false
  else
  begin
    {$IFNDEF FPC}
    regex := TRegEx.Create(OID_REGEX, [roCompiled]);
    result := regex.IsMatch(oid);
    {$ENDIF}
  end;
End;

Function isUUid(oid : String) : Boolean;
var
  regex : TRegEx;
Begin
  {$IFNDEF FPC}
  regex := TRegEx.Create(UUID_REGEX, [roCompiled]);
  result := regex.IsMatch(oid);
  {$ENDIF}
End;

function UriForKnownOid(oid : String) : String;
begin
  if oid = '2.16.840.1.113883.6.96' then
    exit('http://snomed.info/sct');
  if oid = '2.16.840.1.113883.6.1' then
    exit('http://loinc.org');
  if oid = '2.16.840.1.113883.6.8' then
    exit('http://unitsofmeasure.org');
  if oid = '2.16.840.1.113883.6.3' then
    exit('http://hl7.org/fhir/sid/icd-10');
  if oid = '2.16.840.1.113883.6.42' then
    exit('http://hl7.org/fhir/sid/icd-9');
  if oid = '2.16.840.1.113883.6.104' then
    exit('http://hl7.org/fhir/sid/icd-9');
  if oid = '2.16.840.1.113883.6.103' then
    exit('http://hl7.org/fhir/sid/icd-9'); //todo: confirm this
  if oid = '2.16.840.1.113883.6.73' then
    exit('http://hl7.org/fhir/sid/atc');
  if oid = '2.16.840.1.113883.3.26.1.1' then
    exit('http://ncimeta.nci.nih.gov');
  if oid = '2.16.840.1.113883.3.26.1.1.1' then
    exit('http://ncimeta.nci.nih.gov');
  if oid = '2.16.840.1.113883.6.88' then
    exit('http://www.nlm.nih.gov/research/umls/rxnorm'); // todo: confirm this

  if oid = '2.16.840.1.113883.5.1008' then
    exit('http://hl7.org/fhir/v3/NullFlavor');
  if oid = '2.16.840.1.113883.5.111' then
    exit('http://hl7.org/fhir/v3/RoleCode');
  if oid = '2.16.840.1.113883.5.4' then
    exit('http://hl7.org/fhir/v3/ActCode');
  if oid = '2.16.840.1.113883.5.8' then
    exit('http://hl7.org/fhir/v3/ActReason');
  if oid = '2.16.840.1.113883.5.83' then
    exit('http://hl7.org/fhir/v3/ObservationInterpretation');
  if oid = '2.16.840.1.113883.6.238' then
    exit('http://hl7.org/fhir/v3/Race');

  if oid = '2.16.840.1.113883.6.59' then
    exit('http://hl7.org/fhir/sid/cvx');
  if oid = '2.16.840.1.113883.12.292' then
    exit('http://hl7.org/fhir/sid/cvx');

  if oid = '2.16.840.1.113883.6.12' then
    exit('http://www.ama-assn.org/go/cpt');

  if oid = '2.16.840.1.113883.12.' then
    exit('http://hl7.org/fhir/sid/v2-'+oid.substring(21));
  result := '';
end;


Const
  BUFFER_INCREMENT_SIZE = 1024;


Constructor TAnsiStringBuilder.Create;
Begin
  Inherited;

  FBufferSize := BUFFER_INCREMENT_SIZE;
End;


Destructor TAnsiStringBuilder.Destroy;
Begin
  inherited;
End;


Procedure TAnsiStringBuilder.Clear;
Begin
  FContent := '';
  FLength := 0;
End;


Function TAnsiStringBuilder.ToString : String;
Begin
  Result := Copy(String(FContent), 1, FLength);
End;


Procedure TAnsiStringBuilder.AppendPadded(Const sStr : AnsiString; iCount : Integer; cPad : AnsiChar = ' ');
Var
  iLen : Integer;
Begin
  iLen := IntegerMax(System.Length(sStr), iCount);

  If (iLen > 0) Then
  Begin
    If FLength + iLen > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iLen));

    Move(sStr[1], FContent[FLength + 1], System.Length(sStr));

    If iLen = iCount Then
      FillChar(FContent[FLength + 1 + System.Length(sStr)], (iCount - System.Length(sStr)), cPad);

    Inc(FLength, iLen);
  End;
End;


Function TAnsiStringBuilder.AsString: AnsiString;
Begin
  Result := Copy(FContent, 1, FLength);
End;


Procedure TAnsiStringBuilder.AppendFixed(Const sStr : AnsiString; iCount : Integer; cPad : AnsiChar = ' ');
Begin
  If (iCount > 0) Then
  Begin
    If FLength + iCount > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iCount));
    Move(sStr[1], FContent[FLength + 1], IntegerMin(System.Length(sStr), iCount));

    If System.Length(sStr) < iCount Then
      FillChar(FContent[FLength + 1 + System.Length(sStr)], (iCount - System.Length(sStr)), cPad);

    Inc(FLength, iCount);
  End;
End;


Procedure TAnsiStringBuilder.Append(ch : AnsiChar);
Begin
  If FLength + 1 > System.Length(FContent) Then
    SetLength(FContent, System.Length(FContent) + FBufferSize);

  Move(ch, FContent[FLength + 1], 1);
  Inc(FLength);
End;


Procedure TAnsiStringBuilder.Append(Const sStr : AnsiString);
Begin
 If (sStr <> '') Then
  Begin
    If FLength + System.Length(sStr) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, System.Length(sStr)));

    Move(sStr[1], FContent[FLength + 1], System.Length(sStr));

    Inc(FLength, System.Length(sStr));
  End;
End;


Procedure TAnsiStringBuilder.Append(Const oBuilder : TAnsiStringBuilder);
Begin
  Append(oBuilder.AsString);
End;


Procedure TAnsiStringBuilder.AppendEOL;
Begin
  Append(cReturn);
End;


Procedure TAnsiStringBuilder.Append(Const iInt : Integer);
Begin
  Append(AnsiString(IntegerToString(iInt)));
End;


Procedure TAnsiStringBuilder.Insert(Const sStr : AnsiString; iIndex : Integer);
Begin
  If (sStr <> '') Then
  Begin
    If FLength + System.Length(sStr) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, System.Length(sStr)));

    If (iIndex) <> FLength Then
      Move(FContent[iIndex+1], FContent[iIndex+1 + System.Length(sStr)], (FLength - iIndex));

    Move(sStr[1], FContent[iIndex+1], System.Length(sStr));

    Inc(FLength, System.Length(sStr));
  End;
End;


Procedure TAnsiStringBuilder.Insert(Const oBuilder : TAnsiStringBuilder; iIndex : Integer);
Begin
  Insert(oBuilder.AsString, iIndex);
End;


Procedure TAnsiStringBuilder.Delete(iIndex, iLength : Integer);
Begin
  System.Delete(FContent, iIndex+1, iLength);
  Dec(FLength, iLength);
End;


Function TAnsiStringBuilder.IndexOf(Const sStr : AnsiString; bCase : Boolean = False) : Integer;
Var
  iLoop : Integer;
  iUpper : Integer;
  iLen : Integer;
Begin
  Result := -1;
  iLoop := 1;
  iLen := System.Length(sStr);
  iUpper := FLength - iLen + 1;

  While (Result = -1) And (iLoop <= iUpper) Do
  Begin
    If (bCase And (Copy(FContent, iLoop, iLen) = sStr)) Or (Not bCase And (Copy(FContent, iLoop, iLen) = sStr)) Then
      Result := iLoop - 1;

    Inc(iLoop);
  End;
End;


Function TAnsiStringBuilder.LastIndexOf(Const sStr : AnsiString; bCase : Boolean = False) : Integer;
Var
  iLoop : Integer;
  iUpper : Integer;
  iLen : Integer;
Begin
  Result := -1;
  iLen := System.Length(sStr);
  iUpper := FLength - iLen + 1;
  iLoop := iUpper;
  While (Result = -1) And (iLoop > 0) Do
  Begin
    If (bCase And (Copy(FContent, iLoop, iLen) = sStr)) Or (Not bCase And (Copy(FContent, iLoop, iLen) = sStr)) Then
      Result := iLoop - 1;

    Dec(iLoop);
  End;
End;



Procedure TAnsiStringBuilder.CommaAdd(const sStr: AnsiString);
Begin
  if Length > 0 Then
    Append(', ');
  Append(sStr);
End;


Procedure TAnsiStringBuilder.AddCardinalAsBytes(iVal: Cardinal);
Var
  s : AnsiString;
Begin
  SetLength(s, 4);
  move(iVal, s[1], 4);
  Append(s);
End;


Procedure TAnsiStringBuilder.AddWordAsBytes(iVal: word);
Var
  s : AnsiString;
Begin
  SetLength(s, 2);
  move(iVal, s[1], 2);
  Append(s);
End;


Procedure TAnsiStringBuilder.AddInt64AsBytes(iVal: Int64);
Var
  s : AnsiString;
Begin
  SetLength(s, 8);
  move(iVal, s[1], 8);
  Append(s);
End;


Procedure TAnsiStringBuilder.AddByteAsBytes(iVal: Byte);
Var
  s : AnsiString;
Begin
  SetLength(s, 1);
  move(iVal, s[1], 1);
  Append(s);
End;


Procedure TAnsiStringBuilder.AppendLine(const sStr: AnsiString);
Begin
  Append(sStr);
  AppendEOL;
End;

procedure TAnsiStringBuilder.Append(const bBytes: array of Byte; amount: Integer);
var
  i : integer;
begin
  for i := 0 to amount - 1 Do
    Append(AnsiChar(bBytes[i]));
end;


procedure TAnsiStringBuilder.Overwrite(index: integer; content: AnsiString);
begin
  if index < 1 Then
    RaiseError('Overwrite', 'index < 1');
  if index + System.length(Content) > FLength Then
    RaiseError('Overwrite', 'index > length');
  if content <> '' Then
    Move(Content[1], FContent[index], System.length(Content));
end;

procedure TAnsiStringBuilder.Read(index: integer; var buffer; ilength: integer);
begin
  if index < 1 Then
    RaiseError('Read', 'index < 1');
  if index + ilength > FLength Then
    RaiseError('Read', 'index > length');
  Move(FContent[index], buffer, ilength);
end;


Function AnsiStringSplit(Const sValue : AnsiString; Const aDelimiters : TAnsiCharSet; Var sLeft, sRight: AnsiString) : Boolean;
Var
  iIndex : Integer;
  sA, sB : AnsiString;
Begin
  // Find the delimiter within the source string
  iIndex := StringFind(String(sValue), aDelimiters);
  Result := iIndex <> 0;

  If Not Result Then
  Begin
    sA := sValue;
    sB := '';
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + 1, MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;


function AnsiPadString(const AStr: AnsiString; AWidth: Integer; APadChar: AnsiChar; APadLeft: Boolean): AnsiString;
begin
  if Length(AStr) >= AWidth then
    Result := AStr
  else
    begin
    SetLength(Result, AWidth);
    FillChar(Result[1], AWidth, APadChar);
    if AStr <> '' then
      if APadLeft then
        Move(AStr[1], Result[(AWidth - Length(AStr)) + 1], Length(AStr))
      else
        Move(AStr[1], Result[1], Length(AStr))
    end;
end;

{$IFDEF NO_BUILDER}


Constructor TFslStringBuilder.Create;
Begin
  Inherited;

  FBufferSize := BUFFER_INCREMENT_SIZE;
End;


Destructor TFslStringBuilder.Destroy;
Begin
  inherited;
End;


Procedure TFslStringBuilder.Clear;
Begin
  FContent := '';
  FLength := 0;
End;


Function TFslStringBuilder.ToString : String;
Begin
  Result := Copy(FContent, 1, FLength);
End;


Procedure TFslStringBuilder.AppendPadded(Const sStr : String; iCount : Integer; cPad : Char = ' ');
Var
  iLen : Integer;
Begin
  iLen := IntegerMax(System.Length(sStr), iCount);

  If (iLen > 0) Then
  Begin
    If FLength + iLen > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iLen));

    Move(sStr[1], FContent[FLength + 1], System.Length(sStr) * SizeOf(Char));

    If iLen = iCount Then
      FillChar(FContent[FLength + 1 + System.Length(sStr)], (iCount - System.Length(sStr)) * SizeOf(Char), cPad);

    Inc(FLength, iLen);
  End;
End;


Function TFslStringBuilder.AsString: String;
Begin
  Result := ToString;
End;


Procedure TFslStringBuilder.AppendFixed(Const sStr : String; iCount : Integer; cPad : Char = ' ');
Begin
  If (iCount > 0) Then
  Begin
    If FLength + iCount > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iCount));
    Move(sStr[1], FContent[FLength + 1], IntegerMin(System.Length(sStr), iCount) * SizeOf(Char));

    If System.Length(sStr) < iCount Then
      FillChar(FContent[FLength + 1 + System.Length(sStr)], (iCount - System.Length(sStr)) * SizeOf(Char), cPad);

    Inc(FLength, iCount);
  End;
End;


Procedure TFslStringBuilder.Append(ch : Char);
Begin
  If FLength + 1 > System.Length(FContent) Then
    SetLength(FContent, System.Length(FContent) + FBufferSize);

  Move(ch, FContent[FLength + 1], SizeOf(Char));
  Inc(FLength);
End;


Procedure TFslStringBuilder.Append(Const sStr : String);
Begin
 If (sStr <> '') Then
  Begin
    If FLength + System.Length(sStr) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, System.Length(sStr)));

    Move(sStr[1], FContent[FLength + 1], System.Length(sStr) * SizeOf(Char));

    Inc(FLength, System.Length(sStr));
  End;
End;


Procedure TFslStringBuilder.Append(Const oBuilder : TFslStringBuilder);
Begin
  Append(oBuilder.ToString);
End;


Procedure TFslStringBuilder.AppendEOL;
Begin
  Append(cReturn);
End;


Procedure TFslStringBuilder.Append(Const iInt : Integer);
Begin
  Append(IntegerToString(iInt));
End;


Procedure TFslStringBuilder.Insert(Const sStr : String; iIndex : Integer);
Begin
  If (sStr <> '') Then
  Begin
    If FLength + System.Length(sStr) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, System.Length(sStr)));

    If (iIndex) <> FLength Then
      Move(FContent[iIndex+1], FContent[iIndex+1 + System.Length(sStr)], (FLength - iIndex)  * SizeOf(Char));

    Move(sStr[1], FContent[iIndex+1], System.Length(sStr) * SizeOf(Char));

    Inc(FLength, System.Length(sStr));
  End;
End;


Procedure TFslStringBuilder.Insert(Const oBuilder : TFslStringBuilder; iIndex : Integer);
Begin
  Insert(oBuilder.ToString, iIndex);
End;


Procedure TFslStringBuilder.Delete(iIndex, iLength : Integer);
Begin
  System.Delete(FContent, iIndex+1, iLength);
  Dec(FLength, iLength);
End;


Function TFslStringBuilder.IndexOf(Const sStr : String; bCase : Boolean = False) : Integer;
Var
  iLoop : Integer;
  iUpper : Integer;
  iLen : Integer;
Begin
  Result := -1;
  iLoop := 1;
  iLen := System.Length(sStr);
  iUpper := FLength - iLen + 1;

  While (Result = -1) And (iLoop <= iUpper) Do
  Begin
    If (bCase And (Copy(FContent, iLoop, iLen) = sStr)) Or (Not bCase And StringEquals(Copy(FContent, iLoop, iLen), sStr)) Then
      Result := iLoop - 1;

    Inc(iLoop);
  End;
End;


Function TFslStringBuilder.LastIndexOf(Const sStr : String; bCase : Boolean = False) : Integer;
Var
  iLoop : Integer;
  iUpper : Integer;
  iLen : Integer;
Begin
  Result := -1;
  iLen := System.Length(sStr);
  iUpper := FLength - iLen + 1;
  iLoop := iUpper;
  While (Result = -1) And (iLoop > 0) Do
  Begin
    If (bCase And (Copy(FContent, iLoop, iLen) = sStr)) Or (Not bCase And StringEquals(Copy(FContent, iLoop, iLen), sStr)) Then
      Result := iLoop - 1;

    Dec(iLoop);
  End;
End;


Procedure TFslStringBuilder.Append(Const bytes : TBytes);
Begin
{$IFNDEF FPC}
If (System.length(bytes) > 0) Then
  Begin
    If FLength + iBytes > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + Integermax(FBufferSize, iBytes));

    oStream.Read(FContent[FLength + 1], iBytes);

    Inc(FLength, iBytes);
  End;
{$ENDIF}
End;


Procedure TFslStringBuilder.Insert(Const bytes : TBytes; iBytes : Integer; iIndex : Integer);
Begin
  {$IFNDEF FPC}
  If (iBytes > 0) Then
  Begin
    If FLength + iBytes > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iBytes));

    If (iIndex) <> FLength Then
      Move(FContent[iIndex+1], FContent[iIndex+1 + iBytes], (FLength - iIndex) * SizeOf(Char));

    oStream.Read(FContent[iIndex + 1], iBytes);

    Inc(FLength, iBytes);
  End;
  {$ENDIF}
End;



Procedure TFslStringBuilder.CommaAdd(const sStr: String);
Begin
  if Length > 0 Then
    Append(', ');
  Append(sStr);
End;


Procedure TFslStringBuilder.AddCardinalAsBytes(iVal: Cardinal);
Var
  s : AnsiString;
Begin
  SetLength(s, 4);
  move(iVal, s[1], 4);
  Append(s);
End;


Procedure TFslStringBuilder.AddWordAsBytes(iVal: word);
Var
  s : AnsiString;
Begin
  SetLength(s, 2);
  move(iVal, s[1], 2);
  Append(s);
End;


Procedure TFslStringBuilder.AddInt64AsBytes(iVal: Int64);
Var
  s : AnsiString;
Begin
  SetLength(s, 8);
  move(iVal, s[1], 8);
  Append(s);
End;


Procedure TFslStringBuilder.AddByteAsBytes(iVal: Byte);
Var
  s : AnsiString;
Begin
  SetLength(s, 1);
  move(iVal, s[1], 1);
  Append(s);
End;


Procedure TFslStringBuilder.AppendLine(const sStr: String);
Begin
  Append(sStr);
  AppendEOL;
End;

procedure TFslStringBuilder.Append(const bBytes: array of Byte; amount: Integer);
var
  i : integer;
begin
  for i := 0 to amount - 1 Do
    Append(chr(bBytes[i]));

end;


procedure TFslStringBuilder.Overwrite(index: integer; content: String);
begin
  if index < 1 Then
    RaiseError('Overwrite', 'index < 1');
  if index + System.length(Content) > FLength Then
    RaiseError('Overwrite', 'index > length');
  if content <> '' Then
    Move(Content[1], FContent[index], System.length(Content));
end;

procedure TFslStringBuilder.Read(index: integer; var buffer; ilength: integer);
begin
  if index < 1 Then
    RaiseError('Read', 'index < 1');
  if index + length > FLength Then
    RaiseError('Read', 'index > length');
  Move(FContent[index], buffer, length);
end;

{$ELSE}


Procedure TFslStringBuilder.Clear;
Begin
  FBuilder.Clear;
End;


Function TFslStringBuilder.GetAsString : String;
Begin
  Result := FBuilder.ToString;
End;



Procedure TFslStringBuilder.AppendPadded(Const sStr : String; iCount : Integer; cPad : Char = ' ');
Begin
  FBuilder.Append(StringPadRight(sStr, cPad, iCount));
End;

Procedure TFslStringBuilder.AppendFixed(Const sStr : String; iCount : Integer; cPad : Char = ' ');
Begin
  FBuilder.Append(StringPadRight(copy(sStr, 1, iCount), cPad, iCount));
End;


Procedure TFslStringBuilder.Append(ch : Char);
Begin
  FBuilder.Append(ch);
End;

Procedure TFslStringBuilder.Append(Const sStr : String);
Begin
  FBuilder.Append(sStr);
End;



Procedure TFslStringBuilder.Append(Const oBuilder : TFslStringBuilder);
Begin
  Append(oBuilder.AsString);
End;


procedure TFslStringBuilder.Append(const sStr: AnsiString);
begin

end;

Procedure TFslStringBuilder.AppendEOL;
Begin
  Append(cReturn);
End;


Procedure TFslStringBuilder.Insert(Const sStr : String; iIndex : Integer);
Begin
  FBuilder.Insert(iIndex, sStr);
End;


Procedure TFslStringBuilder.Insert(Const oBuilder : TFslStringBuilder; iIndex : Integer);
Begin
  Insert(oBuilder.AsString, iIndex);
End;

function TFslStringBuilder.toString: String;
begin
  result := AsString;
end;

procedure TFslStringBuilder.WriteToStream(aStream: TStream; encoding : TEncoding);
var
  a : TBytes;
Begin
  if encoding = nil then
    encoding := TEncoding.UTF8;
  a := encoding.GetBytes(FBuilder.ToString);
  if System.length(a) > 0 then
    aStream.Write(a[0], System.length(a));
End;

Procedure TFslStringBuilder.Delete(iIndex, iLength : Integer);
Begin
  FBuilder.Remove(iIndex, iLength);
End;


destructor TFslStringBuilder.Destroy;
begin
  FBuilder.Free;
  inherited;
end;

function TFslStringBuilder.GetLength: Integer;
begin
  result := FBuilder.Length;
end;

constructor TFslStringBuilder.Create;
begin
  inherited;
  FBuilder := TStringBuilder.Create;
end;

procedure TFslStringBuilder.CommaAdd(const sStr: String);
begin
  if Length > 0 Then
    Append(', ');
  Append(sStr);
end;

procedure TFslStringBuilder.AddCardinalAsBytes(iVal: Cardinal);
var
  s : AnsiString;
begin
  SetLength(s, 4);
  move(iVal, s[1], 4);
  Append(s);
end;

procedure TFslStringBuilder.AddWordAsBytes(iVal: word);
var
  s : AnsiString;
begin
  SetLength(s, 2);
  move(iVal, s[1], 2);
  Append(s);
end;

procedure TFslStringBuilder.AddInt64AsBytes(iVal: Int64);
var
  s : AnsiString;
begin
  SetLength(s, 8);
  move(iVal, s[1], 8);
  Append(s);
end;

procedure TFslStringBuilder.AddByteAsBytes(iVal: Byte);
var
  s : AnsiString;
begin
  SetLength(s, 1);
  move(iVal, s[1], 1);
  Append(s);
end;

procedure TFslStringBuilder.AppendLine(const sStr: String);
begin
  Append(sStr);
  AppendEOL;
end;
{$ENDIF}


{ TCommaBuilder }

procedure TCommaBuilder.add(s: String);
begin
  if s <> '' then
    if not FIgnoreDuplicates or (list.IndexOf(s) = -1) then
      list.add(s);
end;

function TCommaBuilder.asString: string;
var
  i : integer;
begin
  if list.Count = 0 then
    result := ''
  else
  begin
    result := list[0];
    for i := 1 to list.Count - 1 do
      result := result + ', '+list[i];
  end;
end;

constructor TCommaBuilder.Create;
begin
  inherited create;
  list := TStringList.Create;
end;

destructor TCommaBuilder.Destroy;
begin
  list.Free;
  inherited;
end;


{$IFDEF VER130}
Type
  PByte = ^Byte;
{$ENDIF}

Const
  setMIME = setAlphanumeric + ['/', '?', ':', ';', '.', '+'];

Function SizeOfDecodeHexadecimal(Const aBuffer; iSize : Cardinal) : Cardinal;
Begin
  Result := RealCeiling(iSize / 2);
End;

Function DecodeHexadecimal(Const cValue : AnsiChar; Out iValue : Byte) : Boolean; overload;
Begin
  Result := True;

  Case cValue Of
    '0'..'9' : iValue := Ord(cValue) - Ord('0');
    'A'..'F' : iValue := Ord(cValue) - Ord('A') + 10;
    'a'..'f' : iValue := Ord(cValue) - Ord('a') + 10;
  Else
    iValue := 0;

    Result := False;
  End;
End;

Function StringIsHexadecimal(Const sValue : String) : Boolean;
Begin
  Result := (sValue <> '') And (StringContainsOnly(sValue, setHexadecimal));
End;


Procedure DecodeHexadecimal(Const cHigh, cLow : AnsiChar; Out iValue : Byte); overload;
Var
  iLow, iHigh : Byte;
Begin
  If DecodeHexadecimal(cHigh, iHigh) And DecodeHexadecimal(cLow, iLow) Then
    iValue := (iHigh Shl 4) Or iLow
  Else
    iValue := 0;
End;


Function DecodeHexadecimal(Const cHigh, cLow : AnsiChar) : Byte;
Begin
  DecodeHexadecimal(cHigh, cLow, Result);
End;
Procedure DecodeHexadecimal(Const sValue : AnsiString; Var aBuffer; iCount : Integer);
Type
  PByte = ^Byte;
Var
  pTarget : PByte;
  pSource : PAnsiChar;
  iLoop   : Integer;
Begin
  pTarget := @aBuffer;
  pSource := PAnsiChar(sValue);

  For iLoop := 0 To iCount - 1 Do
  Begin
    pTarget^ := DecodeHexadecimal(pSource^, (pSource + 1)^);

    Inc(pSource, 2);
    Inc(pTarget);
  End;
End;

Function SizeOfDecodeHexadecimal(Const sValue : AnsiString) : Cardinal; overload;
Begin
  Result := SizeOfDecodeHexadecimal(Pointer(sValue)^, Length(sValue));
End;


Function DecodeHexadecimal(Const sValue : String) : TBytes;
Begin
  SetLength(Result, SizeOfDecodeHexadecimal(AnsiString(sValue)));

  DecodeHexadecimal(AnsiString(sValue), Pointer(Result)^, Length(Result));
End;


Function SizeOfEncodeHexadecimal(Const aBuffer; iSize : Cardinal) : Cardinal;
Begin
  Result := iSize * 2;
End;

Function EncodeHexadecimal(Const iValue : Byte) : AnsiString;
Begin
  Result := EncodeHexadecimal(iValue, SizeOf(iValue));
End;

Function EncodeHexadecimal(Const sValue : TBytes) : AnsiString;
Begin
  Result := EncodeHexadecimal(Pointer(sValue)^, Length(sValue));
End;

Function EncodeHexadecimal(Const aBuffer; iCount : Integer) : AnsiString;
Const
  cHex : Array[0..15] Of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
Type
  PByte = ^Byte;
Var
  pBuffer : PByte;
  iResult : Integer;
  iLoop   : Integer;
Begin
  SetLength(Result, SizeOfEncodeHexadecimal(aBuffer, iCount));

  pBuffer := @aBuffer;
  iResult := 1;

  For iLoop := 0 To iCount - 1 Do
  Begin
    Result[iResult] := cHex[Byte(pBuffer^ Shr 4)];   // High Nibble
    Inc(iResult);

    Result[iResult] := cHex[Byte(pBuffer^ And $0F)]; // Low Nibble
    Inc(iResult);

    Inc(pBuffer);
  End;
End;

//
//Function EncodeXML(Const sValue : String; mode : TXmlEncodingMode; eoln : TEolnOption = eolnIgnore) : String;
//Var
//  iLoop : Integer;
//  cValue : Char;
//Begin
//  Result := sValue;
//  iLoop := 1;
//  While iLoop <= Length(Result) Do
//  Begin
//    cValue := Result[iLoop];
//
//    Case cValue Of
//      #10, #13:
//      if (mode = xmlAttribute) or (cValue = #13) then
//      Begin
//        Delete(Result, iLoop, 1);
//        Insert('&#x' + IntToHex(Ord(cValue), 1) + ';', Result, iLoop);
//        Inc(iLoop, 4);
//      End
//      else
//      case eoln of
//        eolnIgnore : Inc(iLoop);
//        eolnEscape :
//          Begin
//          Delete(Result, iLoop, 1);
//          Insert('&#x' + IntToHex(Ord(cValue), 1) + ';', Result, iLoop);
//          Inc(iLoop, 4);
//          End
//      else //
//        if cValue = #13 then
//          Delete(Result, iLoop, 1)
//         else
//           Inc(iLoop);
//      end;
//
//      #9 : if mode <> xmlAttribute then
//            Inc(iLoop)
//          else
//          begin
//            Delete(Result, iLoop, 1);
//            Insert('&#x9;', Result, iLoop);
//            Inc(iLoop, 4);
//          End;
//
//      #0..#8, #11..#12, #14..#31{, #127..#255} :
//      Begin
//        Delete(Result, iLoop, 1);
//        Insert('&#x' + IntToHex(Ord(cValue), 2) + ';', Result, iLoop);
//        Inc(iLoop, 5);
//      End;
//
//      '<':
//      Begin
//        Delete(Result, iLoop, 1);
//        if (mode = xmlCanonical) then
//          Insert('&#'+IntToStr(Ord('<'))+';', Result, iLoop)
//        else
//          Insert('&lt;', Result, iLoop);
//        Inc(iLoop, 4);
//      End;
//
//      '>': if mode = xmlAttribute then
//             Inc(iLoop)
//           else
//           Begin
//             Delete(Result, iLoop, 1);
//            if (mode = xmlCanonical) then
//              Insert('&#'+IntToStr(Ord('>'))+';', Result, iLoop)
//            else
//              Insert('&gt;', Result, iLoop);
//             Inc(iLoop, 4);
//           End;
//
//      '"' :if mode <> xmlAttribute then
//             Inc(iLoop)
//           else
//           Begin
//             Delete(Result, iLoop, 1);
//              if (mode = xmlCanonical) then
//              begin
//                Insert('&#'+IntToStr(Ord('"'))+';', Result, iLoop);
//                Inc(iLoop, 5);
//              end
//              else
//              begin
//               Insert('&quot;', Result, iLoop);
//               Inc(iLoop, 6);
//              end;
//           End;
//
//      '&':
//      Begin
//        // Preceding '&' already exists in string.
//        if (mode = xmlCanonical) then
//          Insert('&#'+IntToStr(Ord('&'))+';', Result, iLoop)
//        else
//          Insert('amp;', Result, iLoop + 1);
//        Inc(iLoop, 4);
//      End;
//
//      // Only need to encode &quot; and &apos; in XML attributes...
//    Else if false {ord(cValue) > 255} then
//      Begin
//        Delete(Result, iLoop, 1);
//        Insert('&#x' + IntToHex(Ord(cValue), 4) + ';', Result, iLoop);
//        Inc(iLoop, 7);
//      End
//    Else
//      Inc(iLoop);
//    End;
//  End;
//End;
//
//Function DecodeXML(Const sValue : String) : String;
//Var
//  iLoop : Integer;
//  pValue : PChar;
//  iValue : Byte;
//  sPrefixedEncodedDec : String;
//  sRemainder : String;
//  sEncodedDec : String;
//  iEncodedDec : Integer;
//Begin
//  Result := sValue;
//  iLoop := 1;
//  While iLoop <= Length(Result) Do
//  Begin
//    pValue := @Result[iLoop];
//
//    If pValue^ = '&' Then
//    Begin
//      If StringEquals(pValue, '&lt;', 4) Then
//      Begin
//        Delete(Result, iLoop, 4);
//        Insert('<', Result, iLoop);
//      End
//      Else If StringEquals(pValue, '&gt;', 4) Then
//      Begin
//        Delete(Result, iLoop, 4);
//        Insert('>', Result, iLoop);
//      End
//      Else If StringEquals(pValue, '&amp;', 5) Then
//      Begin
//        Delete(Result, iLoop, 5);
//        Insert('&', Result, iLoop);
//      End
//      Else If StringEquals(pValue, '&quot;', 6) Then
//      Begin
//        Delete(Result, iLoop, 6);
//        Insert('"', Result, iLoop);
//      End
//      Else If StringEquals(pValue, '&apos;', 6) Then
//      Begin
//        Delete(Result, iLoop, 6);
//        Insert('''', Result, iLoop);
//      End
//      Else If StringEquals(pValue, '&#x', 3) Then
//      Begin
//        StringSplit(pValue, ';', sPrefixedEncodedDec, sRemainder);
//        sEncodedDec := '0x'+copy(sPrefixedEncodedDec, 4, length(sPrefixedEncodedDec));
//        iEncodedDec := StringToInteger32(sEncodedDec);
//        iValue := iEncodedDec;
//        Delete(Result, iLoop, Length(sPrefixedEncodedDec) + 1);
//        Insert(Char(iValue), Result, iLoop);
//      End
//      Else If StringEquals(pValue, '&#', 2) Then
//      Begin
//        StringSplit(pValue, ';', sPrefixedEncodedDec, sRemainder);
//        sEncodedDec := sPrefixedEncodedDec.substring(2);
//
//        iEncodedDec := StringToInteger32(sEncodedDec);
//
//        If (iEncodedDec >= 0) And (iEncodedDec <= 65535) Then
//        Begin
//          iValue := iEncodedDec;
//
//          Delete(Result, iLoop, Length(sPrefixedEncodedDec) + 1); // eg. '&#13;' or '&#220;'
//          Insert(Char(iValue), Result, iLoop);
//        End;
//      End;
//    End;
//
//    Inc(iLoop);
//  End;
//End;

Function EncodeQuotedString(Const sValue : String; Const cQuote : Char) : String;
Begin
  Result := AnsiQuotedStr(sValue, cQuote);
End;

Function EncodeMIME(Const sValue : String) : String;  overload;
Var
  iLoop : Integer;
  cValue : Char;
Begin
  Result := sValue;
  iLoop := 1;

  While (iLoop <= Length(Result)) Do
  Begin
    cValue := Result[iLoop];

    If Not (CharInSet(cValue, setMIME)) or (cValue = ':') Then
    Begin
      Result[iLoop] := '%';
      Inc(iLoop);

      System.Insert(string(EncodeHexadecimal(Byte(cValue))), Result, iLoop);
      Inc(iLoop);
    End;

    Inc(iLoop);
  End;
End;


Function DecodeMIME(Const sValue : String) : String;  overload;
Var
  iLoop : Integer;
  cValue : Char;
Begin
  Result := sValue;
  iLoop := 1;

  While (iLoop <= Length(Result)) Do
  Begin
    cValue := Result[iLoop];

    if cValue = '+' then
      Result[iLoop] := ' ';

    If (iLoop <= Length(Result) - 2) and (cValue = '%') Then
    Begin
      Result[iLoop] := Char(DecodeHexadecimal(AnsiChar(Result[iLoop + 1]), AnsiChar(Result[iLoop + 2])));
      Delete(Result, iLoop + 1, 2);
    End;

    Inc(iLoop);
  End;
End;

Function DecodeMIMEURL(Const sValue : String) : String;  overload;
Var
  bytes : TBytes;
  iStr, iBytes : Integer;
  cValue : Char;
Begin
  Result := sValue;
  iStr := 1;
  iBytes := 0;

  SetLength(bytes, sValue.length);
  While (iStr <= Length(Result)) Do
  Begin
    cValue := Result[iStr];

    if cValue = '+' then
      bytes[iBytes] := 32 // ' '
    else If (iStr <= Length(Result) - 2) and (cValue = '%') Then
    Begin
      bytes[iBytes] := DecodeHexadecimal(AnsiChar(Result[iStr + 1]), AnsiChar(Result[iStr + 2]));
      inc(iStr, 2);
    End
    else
      bytes[iBytes] := ord(cValue);
    Inc(iStr);
    Inc(iBytes);
  End;
  result := TEncoding.UTF8.GetString(bytes, 0, iBytes);
End;

Function EncodeNYSIIS(Const sValue : String) : String;
// NYSIIS Phonetic Encoder
// Based on Pseudo code from http://www.dropby.com/NYSIIS.html
Var
  cFirst : Char;
Begin

  // Remove non-alpha characters
  Result := StringKeep(StringUpper(RemoveAccents(sValue)), setAlphabet);

  //Remove space and control characters from end
  Result := StringTrimWhitespaceRight(Result);

  //Remove trailing 'S' and 'Z' characters
  Result := StringTrimSetRight(Result, ['S', 'Z']);

  If Length(Result) = 0 Then
    Result := 'S';


  If Result <> '' Then
  Begin
    // Transcode initial strings MAC => MC; PF => F
    Result := StringReplaceBefore(Result, 'MAC', 'MC');
    Result := StringReplaceBefore(Result, 'PF', 'F');

    // Transcode trailing strings as follows : IX => IC; EX => EC; YE, EE, IE => Y; NT, ND => D
    Result := StringReplaceAfter(Result, 'IX',  'IC');
    Result := StringReplaceAfter(Result, 'EX',  'EE');
    Result := StringReplaceAfter(Result, 'YE',  'Y');
    Result := StringReplaceAfter(Result, 'EE',  'Y');
    Result := StringReplaceAfter(Result, 'IE',  'Y');
    Result := StringReplaceAfter(Result, 'NT',  'N');
    Result := StringReplaceAfter(Result, 'ND',  'N');
    // Next 3 are not in the pseudo code, but are in the javascript from site
    Result := StringReplaceAfter(Result, 'DT',  'D');
    Result := StringReplaceAfter(Result, 'RT',  'D');
    Result := StringReplaceAfter(Result, 'RD',  'D');


    // Transcode 'EV' to 'EF' if not at the start of string
    Result := StringReplaceAll(Result, 'EV', 'EF', 2);

    cFirst := Result[1];

    // Replace all vowels with 'A'
    Result := StringReplace(Result, ['E', 'I', 'O', 'U'], 'A');

    // Remove any 'W' that follows a vowel
    Result := StringReplaceAll(Result, 'AW', 'A');

    // Transcode 'GHT' => 'GT'; 'DG' => 'G'; 'PH' => 'F'
    Result := StringReplaceAll(Result, 'GHT', 'GT');
    Result := StringReplaceAll(Result, 'DG', 'G');
    Result := StringReplaceAll(Result, 'PH', 'F');

    // If not first character, eliminate all 'H' preceded or followed by a vowel
    Result := StringReplaceAll(Result, 'AH', 'A');
    Result := StringReplaceAll(Result, 'HA', 'A', 2);

    // Change 'KN' to 'N', else 'K' to 'C'
    Result := StringReplaceAll(Result, 'KN', 'N');
    Result := StringReplaceAll(Result, 'K', 'C');

    // If not first character, change 'M' to 'N' & 'Q' to 'G'
    Result := StringReplaceAll(Result, 'M', 'N', 2);
    Result := StringReplaceAll(Result, 'Q', 'G', 2);

    // Transcode 'SH' to 'S', 'SCH' to 'S', 'YW' to 'Y'
    Result := StringReplaceAll(Result, 'SH', 'S');
    Result := StringReplaceAll(Result, 'SCH', 'S');
    Result := StringReplaceAll(Result, 'YW', 'Y');

    // If not first or last character, change 'Y' to 'A'
    Result := StringReplaceAll(Result, 'Y', 'A', 2, 1);

    // Transcode 'WR' to 'R'
    Result := StringReplaceAll(Result, 'WR', 'R');

    // If not first character, change 'Z' to 'S'
    Result := StringReplaceAll(Result, 'Z', 'S', 2);

    // Transcode terminal 'AY' to 'Y'
    Result := StringReplaceAfter(Result, 'AY', 'Y');

    Result := StringStripDuplicates(StringTrimSetRight(Result, setVowels));

    // If first char is a vowel, use original character as first character instead of A
    Result := StringReplaceBefore(Result, 'A', cFirst);

    If Result = '' Then
      Result := cFirst;
  End;
End;

function GetCryptKey(const AStr: String): Word;
var
  i: Integer;
begin
  {$R-}
  Result := 1;
  for i := 1 to Length(AStr) do
    Result := Result * i * Ord(AStr[i]);
  {$R+}
end;

{$Q-}
  {$R-}
const
  C1 = 52845;
  C2 = 22719;

function strEncrypt(const AStr: String; AKey: Word): String;
var
  i: Integer;
begin
  Result := '';
  setlength(Result, Length(AStr));
  for i := 1 to Length(AStr) do
    begin
    Result[i] := Char(Ord(AStr[i]) xor (AKey shr 8));
    AKey := (Ord(Result[i]) + AKey) * C1 + C2;
    end;
  Result := EncodeMIME(Result);
end;

function strDecrypt(const AStr: String; AKey: Word): String;
var
  i: Integer;
  LStr: String;
begin
  Result := '';
  LStr := DecodeMime(AStr);
  setlength(Result, length(LStr));
  for i := 1 to Length(LStr) do
    begin
    Result[i] := Char(Ord(LStr[i]) xor (AKey shr 8));
    AKey := (Ord(LStr[i]) + AKey) * C1 + C2;
    end;
end;
{$R+}
{$Q+}

Function StringIsAlphanumeric(Const cValue : Char) : Boolean;
Begin
  Result := CharInSet(cValue, setAlphanumeric);
End;


Function StringIsAlphanumeric(Const sValue : String) : Boolean;
Begin
  Result := StringContainsOnly(sValue, setAlphanumeric);
End;


Function StringIsNumeric(Const cValue : Char) : Boolean;
Begin
{$IFDEF VER130}
  Result := CharInSet(cValue, setNumbers);
{$ELSE}
  Result := cValue.IsNumber;
{$ENDIF}
End;


Function StringIsNumeric(Const sValue : String) : Boolean;
Begin
  Result := (sValue <> '') And (StringContainsOnly(sValue, setNumbers));
End;

Function StringSingular(Const sValue : String) : String;
Var
  bException : Boolean;
  iCount : Integer;
  iLength : Integer;
  iLoop : Integer;
Begin
  iLength := Length(sValue);

  If (iLength <= 0) Or (iLength = 1) Or (sValue[iLength] <> 's') Then
    Result := sValue
  Else
  Begin
    bException := False;

    iLoop := 0;
    iCount := Length(PLURAL_EXCEPTIONS);

    While Not bException And (iLoop < iCount) Do
    Begin
      bException := StringEndsWith(sValue, PLURAL_EXCEPTIONS[iLoop]);

      Inc(iLoop);
    End;

    If bException Then
      Result := sValue
    Else
    Begin
      Case sValue[iLength - 1] Of
       'e' :
        Begin
          If (iLength > 2) And (sValue[iLength - 2] = 'i') Then
          Begin
            If iLength > 3 Then
              Result := Copy(sValue, 1, iLength - 3) + 'y'
            Else
              Result := Copy(sValue, 1, iLength - 1);
          End
          Else If (iLength > 3) And
            ((CharInSet(sValue[iLength - 2], ['j', 'x', 'o'])) Or
            ((sValue[iLength - 3] = 'z') And (sValue[iLength - 2] = 'z')) Or
            ((sValue[iLength - 3] = 's') And (CharInSet(sValue[iLength - 2], ['s', 'h'])))) Then

            Result := Copy(sValue, 1, iLength - 2)
          Else
            Result := Copy(sValue, 1, iLength - 1);
        End;
      Else
        Result := Copy(sValue, 1, iLength - 1);
      End;
    End;
  End;
End;


Function StringPlural(Const sValue : String; iValue : Integer) : String;
Var
  bException : Boolean;
  iCount : Integer;
  iLength : Integer;
  iLoop : Integer;
  cLastCharacter : Char;
  cSecondLastCharacter : Char;
  sPluralisation : String;
Begin
  Result := sValue;

  iLength := Length(sValue);

  If (iLength > 0) And (iValue <> 1) Then
  Begin
    bException := False;

    iLoop := 0;
    iCount := Length(PLURAL_EXCEPTIONS);

    While Not bException And (iLoop < iCount) Do
    Begin
      bException := StringEndsWith(sValue, PLURAL_EXCEPTIONS[iLoop]);

      Inc(iLoop);
    End;

    If Not bException Then
    Begin
      cLastCharacter := sValue[iLength];

      Case charUpper(cLastCharacter) Of
        'H' :
        Begin
          cSecondLastCharacter := charUpper(sValue[iLength - 1]);
          If (iLength > 1) And ((cSecondLastCharacter = 'T') Or (cSecondLastCharacter = 'G')) Then
            sPluralisation := 's'
          Else
            sPluralisation := 'es';
        End;
        'S', 'X', 'J', 'Z' :
        Begin
            sPluralisation := 'es';
        End;
        'Y' :
        Begin
          If (iLength > 2) And CharInSet(sValue[iLength - 1], setConsonants) Then
          Begin
            Result := Copy(sValue, 1, iLength - 1);
            sPluralisation := 'ies';
          End
          Else
          Begin
            sPluralisation := 's';
          End;
        End;
      Else
        sPluralisation := 's';
      End;

      If cLastCharacter.IsUpper Then
        sPluralisation := StringUpper(sPluralisation);

      Result := Result + sPluralisation;
    End;
  End;
End;


Function StringCamel(Const sValue : String) : String;
Var
  cCurrent : Char;
  iLoop : Integer;
Begin
  Result := sValue;

  If Result <> '' Then
    Result[1] := charUpper(Result[1]);

  For iLoop := 1 To Length(Result) - 1 Do
  Begin
    cCurrent := Result[iLoop];

    If CharInSet(cCurrent, [' ', '_']) Then
      Result[iLoop + 1] := charUpper(Result[iLoop + 1]);
  End;
End;


Function StringReverseCamel(Const sValue : String) : String;
Var
  bNumeric : Boolean;
  bUpper : Boolean;
  iLoop : Integer;
  cCurrent : Char;
  cLast : Char;
Begin
  Result := sValue;

  cLast := ' ';

  For iLoop := Length(Result) DownTo 2 Do
  Begin
    cCurrent := Result[iLoop];

    If (cCurrent <> '') And (cLast <> '') Then
    Begin
      bUpper := cCurrent.isUpper And Not cLast.isUpper;
      bNumeric := StringIsNumeric(cCurrent) And Not StringIsNumeric(cLast);

      If bUpper Or bNumeric Then
        Insert(' ', Result, iLoop);
    End;

    cLast := cCurrent;
  End;
End;

Function StringFind(Const sValue, sFind : String; iStart : Integer) : Integer;
Begin
  If iStart > Length(sValue) Then
    Result := 0
  Else
  Begin
    Result := StringFind(Copy(sValue, iStart, MaxInt), sFind);

    If Result > 0 Then
      Inc(Result, iStart - 1);
  End;
End;

Function StringFind(Const sValue : String; aFind : TCharSet; iStart : Integer) : Integer;
Begin
  If iStart > Length(sValue) Then
    Result := 0
  Else
  Begin
    Result := StringFind(Copy(sValue, iStart, MaxInt), aFind);

    If Result > 0 Then
      Inc(Result, iStart - 1);
  End;
End;

Function CharArrayIndexOfInsensitive(Const aNames : Array Of Char; Const cName: Char): Integer;
Begin
  Result := High(aNames);
  While (Result >= Low(aNames)) And (StringCompareInsensitive(cName, aNames[Result]) <> 0) Do
    Dec(Result);
End;


Function CharArrayIndexOfSensitive(Const aNames : Array Of Char; Const cName: Char): Integer;
Begin
  Result := High(aNames);
  While (Result >= Low(aNames)) And (StringCompareSensitive(cName, aNames[Result]) <> 0) Do
    Dec(Result);
End;


Function CharArrayIndexOf(Const aNames : Array Of Char; Const cName : Char): Integer;
Begin
  Result := CharArrayIndexOfInsensitive(aNames, cName);
End;

Function CharArrayValid(Const aNames : Array Of Char; Const iIndex : Integer) : Boolean;
Begin
  Result := IntegerBetweenInclusive(Low(aNames), iIndex, High(aNames));
End;


function SimpleStringIsDecimal(s : String; allowDec : boolean) : boolean;
var
  bDec : Boolean;
  i : integer;
Begin
  if s.StartsWith('+') or s.StartsWith('-')  then
    delete(s, 1, 1);
  if s = '' then
    exit(false);

  bDec := false;
  result := true;
  for i := 1 to length(s) Do
  begin
    if not (CharInSet(s[i], ['0'..'9'])) then
      if s[i] <> '.' then
        exit(false)
      else if bDec then
        exit(false)
      else
        bDec := true;
  end;
end;


{ TDateTimeEx }

function TDateTimeEx.add(length: TDuration): TDateTimeEx;
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

function TDateTimeEx.difference(other : TDateTimeEx): TDuration;
begin
  Result := self.UTC.DateTime - other.UTC.DateTime;
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
    ((precision < dtpNanoSeconds) or ((fraction = other.fraction) and (FractionPrecision = other.FractionPrecision))) and
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
  result.FPrecision := dtpNanoSeconds;
  result.FractionPrecision := 3;
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

class operator TDateTimeEx.LessThan(a, b: TDateTimeEx): Boolean;
begin
  result := a.before(b, false);
end;

class operator TDateTimeEx.LessThanOrEqual(a, b: TDateTimeEx): Boolean;
begin
  result := a.before(b, true);
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

class operator TDateTimeEx.GreaterThan(a, b: TDateTimeEx): Boolean;
begin
  result := a.after(b, false);
end;

class operator TDateTimeEx.GreaterThanOrEqual(a, b: TDateTimeEx): Boolean;
begin
  result := a.after(b, true);
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
    dtpNanoSeconds:
       if FractionPrecision = 0 then
         result := sv(Year, 4) + '-' + sv(Month, 2) + '-' + sv(Day, 2) + 'T' + sv(hour, 2) + ':' + sv(Minute, 2)+ ':' + sv(Second, 2)
       else
         result := sv(Year, 4) + '-' + sv(Month, 2) + '-' + sv(Day, 2) + 'T' + sv(hour, 2) + ':' + sv(Minute, 2)+ ':' + sv(Second, 2)+'.'+copy(sv(Fraction, 9), 1, FractionPrecision);
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

class operator TDateTimeEx.Trunc(a: TDateTimeEx): TDateTimeEx;
begin
  result := a.truncToDay;
end;

function TDateTimeEx.truncToDay: TDateTimeEx;
begin
  result := clone;
  if result.FPrecision > dtpDay then
    result.FPrecision := dtpDay;
  result.hour := 0;
  result.minute := 0;
  result.second := 0;
  result.fraction := 0;
  result.TimezoneType := dttzUnknown;
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

class operator TDateTimeEx.NotEqual(a, b: TDateTimeEx): Boolean;
begin
  result := not a.equal(b);
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
          bias := (-TimezoneHours * DATETIME_HOUR_ONE) + (TimezoneMins * DATETIME_MINUTE_ONE)
        else
          bias := -(TimezoneHours * DATETIME_HOUR_ONE) + (TimezoneMins * DATETIME_MINUTE_ONE);
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


function TDateTimeEx.fixPrecision(precision: TDateTimeExPrecision): TDateTimeEx;
begin
  result := Self;
  result.FPrecision := precision;
end;

function TDateTimeEx.SameTime(other: TDateTimeEx): Boolean;
begin
  if (TimezoneType = dttzUnknown) or (other.TimezoneType = dttzUnknown)  then
    result := false // unknown, really
  else
    result := sameInstant(UTC.DateTime, other.UTC.DateTime);
end;

class operator TDateTimeEx.subtract(a, b: TDateTimeEx): TDateTime;
begin
  result := a.difference(b);
end;

class operator TDateTimeEx.subtract(a: TDateTimeEx; b: TDateTime): TDateTimeEx;
begin
  result := a.subtract(b);
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

function TDateTimeEx.subtract(length: TDuration): TDateTimeEx;
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

class operator TDateTimeEx.add(a: TDateTimeEx; b: TDateTime): TDateTimeEx;
begin
  result := a.add(b);
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
  Result := RealCompare(aA, aB);
End;


Function DateTimeCompare(Const aA, aB, aThreshold : TDateTime) : Integer; Overload;
Begin
  Result := RealCompare(aA, aB, aThreshold);
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




Function StringToDuration(Const sValue : String; Out aValue : TDurationMS) : Boolean; overload;
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
  aDuration : TDurationMS;
Begin
  Result := StringToDuration(sValue, aDuration);
End;

Function StringToDuration(Const sValue : String) : TDurationMS; overload;
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
  Result := RealCompare(ToUniversalDatetime(aA), ToUniversalDateTime(aB));

  If Result = 0 Then
    Result := IntegerCompare(aA.Offset, aB.Offset);
End;


Function DateTimeOffsetCompare(Const aA, aB : TDateTimeOffset; Const aThreshold : TDateTime) : Integer; overload;
Begin
  Result := RealCompare(ToUniversalDatetime(aA), ToUniversalDateTime(aB), aThreshold);

  If Result = 0 Then
    Result := IntegerCompare(aA.Offset, aB.Offset);
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

Function DurationToDaysString(Const aValue : TDurationMS) : String;
Begin
  Result := StringFormat('%0.2f', [aValue / DATETIME_DAY_MILLISECONDS]);
End;

Function FormaTDurationMS(Const sFormat : String; Const aValue : TDurationMS) : String;
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


Function DurationToHoursString(Const aValue : TDurationMS) : String;
Begin
  Result := FormaTDurationMS('%2:.2d', aValue);
End;


Function DurationToMinutesString(Const aValue : TDurationMS) : String;
Begin
  Result := FormaTDurationMS('%2:.2d%0:s%3:.2d', aValue);
End;


Function DurationToSecondsString(Const aValue : TDurationMS) : String;
Begin
  Result := FormaTDurationMS('%2:.2d%0:s%3:.2d%0:s%4:.2d', aValue);
End;


Function DurationToShortSecondsString(Const aValue : TDurationMS) : String;
Begin
  Result := FormaTDurationMS('%3:.2d%0:s%4:.2d', aValue);
End;


Function DurationToMillisecondsString(Const aValue : TDurationMS) : String;
Begin
  Result := FormaTDurationMS('%2:.2d%0:s%3:.2d%0:s%4:.2d%5:s%6:.3d', aValue);
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

class operator TDateTimeEx.equal(a, b: TDateTimeEx): Boolean;
begin
  result := a.equal(b);
end;

function StringIsDecimal(s : String) : Boolean;
var
  l, r : String;
begin
  if (s.Contains('e')) then
  begin
    StringSplit(s, 'e', l, r);
    result := SimpleStringIsDecimal(l, true) and SimpleStringIsDecimal(r, false)
  end
  else if (s.Contains('e')) or (s.Contains('E')) then
  begin
    StringSplit(s, 'E', l, r);
    result := SimpleStringIsDecimal(l, true) and SimpleStringIsDecimal(r, false)
  end
  else
    result := SimpleStringIsDecimal(s, true);
end;


class function TFslDecimalHelper.CheckValue(sValue: String): boolean;
begin
  if (sValue= '') or (sValue = '-') then
    result := false
  else if pos('e', lowercase(sValue)) > 0 then
    result := CheckValueScientific(lowercase(sValue))
  else
    result := CheckValueDecimal(sValue);
end;

class function TFslDecimalHelper.CheckValueDecimal(sValue: String): boolean;
var
  iDecimal : integer;
  i : integer;
Begin
  result := false;
  iDecimal := 0;
  if (sValue[1] = '-') then
    delete(sValue, 1, 1);

  while (sValue[1] = '0') And (length(sValue) > 1) Do
    delete(sValue, 1, 1);

  for i := 1 to length(sValue) do
    if (sValue[i] = '.') And (iDecimal = 0) then
      iDecimal := i
    else if not CharInSet(sValue[i], ['0'..'9']) then
      exit;

  if iDecimal <> length(sValue) then
    result := true
end;

class function TFslDecimalHelper.CheckValueScientific(sValue: String): boolean;
var
  i : integer;
  s, e : String;
begin
  result := false;
  StringSplit(sValue, 'e', s, e);

  if (s= '') or (s = '-') or not StringIsDecimal(s) then
    exit;

  if (e= '') or (e = '-') or not StringIsDecimal(e) then
    exit;

  if not checkValueDecimal(s) then
    exit;

  // now check for exponent

  if e[1] = '-' then
    i := 2
  Else
    i := 1;
  while i <= length(e) Do
  begin
    if not CharInSet(e[i], ['0'..'9']) then
      exit;
    inc(i);
  end;
  result := true;
end;

function TFslDecimalHelper.Clone: TFslDecimal;
begin
  result.FStatus := FStatus;
  result.FPrecision := FPrecision;
  result.FScientific := FScientific;
  result.FNegative := FNegative;
  result.FDigits := FDigits;
  result.FDecimal := FDecimal;
end;

procedure TFslDecimalHelper.SetPrecision(const Value: integer);
begin
  FPrecision := value;
end;

Procedure TFslDecimalHelper.SetValue(sValue : String);
Begin
  FStatus := sdsUnknown;
  FPrecision := 0;
  FScientific := false;
  FNegative := false;
  FDigits := '';
  FDecimal := 0;

  // special cases:
  if (sValue = '-0') then
    sValue := '0';
  sValue := sValue.ToLower;

  if (sValue = '') then
    FStatus := sdsUnknown
  else if (sValue = 'inf') or (sValue = '+inf') then
    FStatus := sdsInfinite
  else if (sValue = '-inf') then
  begin
    FStatus := sdsInfinite;
    FNegative := true
  end
  else if (sValue = 'nan') or (sValue = '?') then
    FStatus := sdsUndefined
  else
  begin
    FStatus := sdsNumber;
    if (sValue = '-') then
      raise EFslException.create('"'+sValue+'" is not a valid decimal');
    sValue := lowercase(sValue);
    if pos('e', sValue) > 0 then
      SetValueScientific(sValue)
    Else
      SetValueDecimal(sValue);
  end;
end;


Function FirstNonZero(s : String):integer;
var
  i : integer;
begin
  result := length(s);
  for i := 1 to length(s) Do
    if not CharInSet(s[i], ['0', '.']) then
      result := IntegerMin(result, i);
end;

Function AllZerosButLast(s : String; iStart : integer):Boolean;
var
  i : integer;
begin
  result := iStart < length(s) - 1;
  for i := iStart to length(s) - 1 Do
    if s[i] <> '0' then
      result := false;
end;


Function AllZeros(s : String; iStart : integer):Boolean;
var
  i : integer;
begin
  result := true;
  for i := iStart to length(s) Do
    if s[i] <> '0' then
      result := false;
end;

Function CountSignificants(sValue : String):Integer;
var
  i : integer;
Begin
  i := pos('.', sValue);
  if i > 0 then
    delete(sValue, i, 1);
  while (sValue[1] = '0') do
    delete(sValue, 1, 1);
  result := length(sValue);
end;

Procedure TFslDecimalHelper.SetValueDecimal(sValue : String);
var
  iDecimal : integer;
  i : integer;
Begin
  FStatus := sdsNumber;
  FScientific := false;
  iDecimal := 0;
  FNegative := (sValue[1] = '-');
  if FNegative then
    delete(sValue, 1, 1);

  while (sValue[1] = '0') And (length(sValue) > 1) Do
    delete(sValue, 1, 1);

  for i := 1 to length(sValue) do
    if (sValue[i] = '.') And (iDecimal = 0) then
      iDecimal := i
    else if not CharInSet(sValue[i], ['0'..'9']) then
      raise ELibraryException.create('"'+sValue+'" is not a valid decimal');

  if iDecimal = 0 then
  Begin
    Precision := Length(sValue);
    FDecimal := Length(sValue)+1;
    FDigits := sValue;
  end
  else if iDecimal = length(sValue) then
    raise ELibraryException.create('"'+sValue+'" is not a valid FDecimal')
  else
  begin
    FDecimal := iDecimal;
    if AllZeros(sValue, 2) then
      Precision := length(sValue) - 1
    Else
      Precision := CountSignificants(sValue);
    FDigits := sValue;
    Delete(FDigits, FDecimal, 1);
    if AllZeros(FDigits, 1) then
      inc(FPrecision)
    else
      while (FDigits[1] = '0') Do
      begin
        delete(FDigits, 1, 1);
        dec(FDecimal);
      end;
  end;
end;

function TFslDecimalHelper.Abs: TFslDecimal;
begin
  result := self;
  if result.FStatus in [sdsNumber, sdsInfinite] then
    result.FNegative := false;
end;

Function TFslDecimalHelper.Add(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := Valueof(iOther);
  result := Add(oTemp);
end;

Function TFslDecimalHelper.Add(oOther : TFslDecimal) : TFslDecimal;
Begin
  if isNull or oOther.IsNull then
    exit(makeNull)
  else if IsUndefined or oOther.IsUndefined then
    exit(makeUndefined)
  else if IsInfinite or oOther.IsInfinite then
  begin
    if IsNegative <> oOther.IsNegative then
      exit(makeUndefined)
    else if IsNegative then
      exit(makeInfinity.Negated)
    else
      exit(makeInfinity);
  end
  else if (FNegative = oOther.FNegative) then
  Begin
    result := DoAdd(oOther);
    result.FNegative := FNegative;
  end
  else if (FNegative) then
    result := oOther.DoSubtract(self)
  else
    result := DoSubtract(oOther);
end;

Function TFslDecimalHelper.StringAddition(s1, s2 : String):String;
var
  i, t, c : Integer;
Begin
  assert(length(s1) = length(s2));
  result := stringmultiply('0', length(s2));
  c := 0;
  for i := length(s1) downto 1 do
  begin
    t := c + dig(s1[i]) + dig(s2[i]);
    result[i] := cdig(t mod 10);
    c := t div 10;
  end;
  assert(c = 0);
end;

Function TFslDecimalHelper.DoAdd(oOther : TFslDecimal) : TFslDecimal;
var
  iMax : Integer;
  s1, s2, s3 : String;
Begin
  iMax := IntegerMax(FDecimal, oOther.FDecimal);
  s1 := StringMultiply('0', iMax - FDecimal+1) + FDigits;
  s2 := StringMultiply('0', iMax - oOther.FDecimal+1) + oOther.FDigits;
  if Length(s1) < length(s2) then
    s1 := s1 + StringMultiply('0', length(s2) - length(s1))
  else if Length(s2) < length(s1) then
    s2 := s2 + StringMultiply('0', length(s1) - length(s2));


  s3 := StringAddition(s1, s2);

  if s3[1] = '1' then
    inc(iMax)
  else
    delete(s3, 1, 1);
  if iMax <> length(s3)+1 then
  Begin
    if iMax <= 0 then
      raise ELibraryException.create('unhandled')
    else if imax <= length(s3) then
      insert('.', s3, iMax)
    else
      raise ELibraryException.create('unhandled')
  end;

  result := valueOf(s3);
  result.FScientific := FScientific or oOther.FScientific;
  // todo: the problem with this is you have to figure out the absolute FPrecision and take the lower of the two, not the relative one
  if FDecimal < oOther.FDecimal then
    result.FPrecision := FPrecision
  else if oOther.FDecimal < FDecimal then
    result.FPrecision := oOther.FPrecision
  else
    result.FPrecision := IntegerMin(FPrecision, oOther.FPrecision);
end;


Function TFslDecimalHelper.StringSubtraction(s1, s2 : String):String;
var
  i, t, c : integer;
Begin
  assert(length(s1) = length(s2));

  result := stringmultiply('0', length(s2));
  c := 0;
  for i := length(s1) downto 1 do
  begin
    t := c + (dig(s1[i]) - dig(s2[i]));
    if t < 0 then
    Begin
      inc(t, 10);
      if i = 1 then
        raise ELibraryException.create('internal logic error')
      else
        s1[i-1] := cdig(dig(s1[i-1])-1);
    end;
    result[i] := cdig(t);
  end;
  assert(c = 0);
end;

Function TFslDecimalHelper.DoSubtract(oOther : TFslDecimal) : TFslDecimal;
var
  iMax : Integer;
  s1, s2, s3 : String;
  bNeg : Boolean;
Begin
  iMax := IntegerMax(FDecimal, oOther.FDecimal);
  s1 := StringMultiply('0', iMax - FDecimal+1) + FDigits;
  s2 := StringMultiply('0', iMax - oOther.FDecimal+1) + oOther.FDigits;
  if Length(s1) < length(s2) then
    s1 := s1 + StringMultiply('0', length(s2) - length(s1))
  else if Length(s2) < length(s1) then
    s2 := s2 + StringMultiply('0', length(s1) - length(s2));

  bNeg := (s1 < s2);
  if bNeg then
  Begin
    s3 := s2;
    s2 := s1;
    s1 := s3;
  end;

  s3 := StringSubtraction(s1, s2);

  if s3[1] = '1' then
    inc(iMax)
  else
    delete(s3, 1, 1);
  if iMax <> length(s3)+1 then
  Begin
    if iMax <= 0 then
      raise ELibraryException.create('unhandled')
    else if imax <= length(s3) then
      insert('.', s3, iMax)
    else
      raise ELibraryException.create('unhandled');
  end;

  result := valueOf(s3);
  result.FNegative := bNeg;
  result.FScientific := FScientific or oOther.FScientific;
  if FDecimal < oOther.FDecimal then
    FPrecision := FPrecision
  else if oOther.FDecimal < FDecimal then
    FPrecision := oOther.FPrecision
  else
    FPrecision := IntegerMin(FPrecision, oOther.FPrecision);
end;

Function TFslDecimalHelper.Multiply(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Multiply(oTemp);
end;

function TFslDecimalHelper.Negated: TFslDecimal;
begin
  result := clone;
  if FStatus in [sdsNumber, sdsInfinite] then
    result.FNegative := not result.FNegative;
end;

Function TFslDecimalHelper.Multiply(oOther : TFslDecimal) : TFslDecimal;
var
  iMax : Integer;
  s1, s2, s3 : String;
  s : Array of String;
  res : String;
  i, j, c, t : integer;
  iDec : Integer;
  iPrec : Integer;
Begin
  if isNull or oOther.isNull then
    result := MakeNull
  else if IsUndefined or oOther.IsUndefined then
    result := makeUndefined
  else if ((self.isZero) and (oOther.isInfinite)) or ((oOther.IsZero) and (self.IsInfinite)) then
    result := makeUndefined
  else if ((self.isInfinite)) or (oOther.IsInfinite) then
  begin
    result := makeInfinity;
    if self.IsNegative xor oOther.IsNegative then
      result := result.Negated;
  end
  else if (self.isZero) or (oOther.IsZero) then
    result := makeZero
  else if oOther.isOne then
    result := self
  else if self.isOne then
    result := oOther
  else
  Begin
    iMax := IntegerMax(FDecimal, oOther.FDecimal);
    s1 := StringMultiply('0', iMax - FDecimal+1) + FDigits;
    s2 := StringMultiply('0', iMax - oOther.FDecimal+1) + oOther.FDigits;
    if Length(s1) < length(s2) then
      s1 := s1 + StringMultiply('0', length(s2) - length(s1))
    else if Length(s2) < length(s1) then
      s2 := s2 + StringMultiply('0', length(s1) - length(s2));

    if s2 > s1 then
    Begin
      s3 := s1;
      s1 := s2;
      s2 := s3;
    end;
    SetLength(s, length(s2));

    t := 0;
    for i := length(s2) downto 1 do
    begin
      s[i-1] := StringMultiply('0', length(s2)-i);
      c := 0;
      for j := length(s1) downto 1 do
      begin
        t := c + (dig(s1[j]) * dig(s2[i]));
        insert(cdig(t mod 10), s[i-1], 1);
        c := t div 10;
      end;
      while c > 0 Do
      Begin
        insert(cdig(t mod 10), s[i-1], 1);
        c := t div 10;
      end;
    end;

    t := 0;
    for i := Low(s) to High(s) Do
      t := IntegerMax(t, Length(s[i]));
    for i := Low(s) to High(s) Do
      s[i] := StringMultiply('0', t-Length(s[i]))+s[i];

    res := '';
    c := 0;
    for i := t Downto 1 do
    Begin
      for j := Low(s) to High(s) Do
        c := c + dig(s[j][i]);
      insert(cdig(c mod 10), res, 1);
      c := c div 10;
    end;
      while c > 0 Do
      Begin
        assert(false, 'not implemented yet?');
//        s[i-1] := s[i-1] + cdig(t mod 10);
 //       c := t div 10;
      end;

    iDec := Length(res) + 1 - ((length(s1)-iMax)*2);

    while (res <> '') And (res <> '0') And (res[1] = '0') Do
    begin
      delete(res, 1, 1);
      dec(iDec);
    end;

    if IsWholeNumber and oOther.IsWholeNumber Then
      iPrec := INTEGER_PRECISION
    else if IsWholeNumber then
      iPrec := oOther.FPrecision
    else if oOther.IsWholeNumber then
      iPrec := FPrecision
    Else
      iPrec := IntegerMin(FPrecision, oOther.FPrecision);
    while (length(res) > iPrec) And (res[Length(res)] = '0') Do
      delete(res, Length(res), 1);

    result := valueOf(res);
    result.FStatus := sdsNumber;
    result.FPrecision := iPrec;
    result.FDecimal := iDec;
    result.FNegative := FNegative <> oOther.FNegative;
    result.FScientific := FScientific or oOther.FScientific;
//    writeln('  '+asdecimal+' x ' +oOther.AsDecimal+' = ' +result.AsDecimal);
  end;
end;

Function TFslDecimalHelper.Divide(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Divide(oTemp);
end;


Function TrimLeadingZeros(s : String):String;
begin
  result := s;
  while (result <> '') And (result[1] = '0') do
    delete(result, 1, 1);
  if result = '' then
    result := '0';
end;

Function RoundUp(const s : String; var d : Integer):String;
var
  i : integer;
  bUp : Boolean;
Begin
  result := s;
  i := length(s);
  bUp := true;
  while bUp and (i > 0) Do
  begin
    bUp := result[i] = '9';
    if bUp then
      result[i] := '0'
    else
      result[i] := char(ord(result[i])+1);
    dec(i);
  end;
  if bUp then
  begin
    result := '1'+result;
    inc(d);
  end;
end;

Function TFslDecimalHelper.Divide(oOther : TFslDecimal) : TFslDecimal;
var
  s : String;
  w, r, v : String;
  i, l, m, d, vi, iPrec : integer;
  tens : Array of String;
  bProc, bHandled, bUp : Boolean;
  Procedure Grabnext;
  Begin
    if (vi <= length(v)) then
    begin
      w := w + v[vi];
      inc(vi);
      bhandled := false;
    end
    else
    Begin
      w := w + '0';
      inc(d);
    end;
    while (length(w) < length(tens[0])) do
      w := '0'+w;
  end;
  Function finished : Boolean;
  begin
    result := bhandled and ((l > m) or ((vi > length(v)) And ((w = '') or AllZeros(w, 1))));
  end;
Begin
  if isNull or oOther.isNull then
    result := MakeNull
  else if IsUndefined or oOther.IsUndefined then
    result := makeUndefined
  else if (oOther.isInfinite or self.IsInfinite) then
    result := makeUndefined
  else if IsZero Then
    result := makeZero
  else if oOther.IsZero then
    result := makeUndefined
  else if oOther.isOne then
    result := self
  else
  Begin
    s := '0'+oOther.FDigits;
    m := IntegerMax(length(FDigits), Length(oOther.FDigits)) + 40; // max loops we'll do
    SetLength(tens, 10);
    tens[0] := StringAddition(StringMultiply('0', length(s)), s);
    for i := 1 to 9 do
      tens[i] := StringAddition(tens[i-1], s);
    v := FDigits;
    r := '';
    l := 0;
    d := (length(FDigits) - FDecimal + 1) - (length(oOther.FDigits) - oOther.FDecimal + 1);

    while length(v) < length(tens[0]) do
    begin
      v := v+'0';
      inc(d);
    end;
    if copy(v, 1, length(oOther.FDigits)) < oOther.FDigits then
    Begin
      if length(v) = length(tens[0]) then
      begin
        v := v + '0';
        inc(d);
      end;
      w := copy(v, 1, length(oOther.FDigits)+1);
      vi := length(w)+1;
    end
    Else
    Begin
      w := '0'+copy(v, 1, length(oOther.FDigits));
      vi := length(w);
    end;
    while not finished Do
    Begin
      inc(l);
      bHandled := true;
      bProc := false;
      for i := 8 downto 0 Do
      begin
        if tens[i] <= w then
        Begin
          bProc := true;
          r := r + cdig(i+1);
          w := TrimLeadingZeros(StringSubtraction(w, tens[i]));
          if not finished then
            grabnext;
          break;
        end;
      end;
      if not bProc then
      begin
        assert(w[1] = '0');
        delete(w, 1, 1);
        r := r + '0';
        if not finished then
          grabNext;
      end;
    end;
    if isWholeNumber And oOther.IsWholeNumber and (l < m) then
    begin
      for i := 1 to d do
        if (r[length(r)] = '0') then
        begin
          delete(r, length(r), 1);
          dec(d);
        end;
      iPrec := INTEGER_PRECISION;
    end
    Else
    Begin
      if IsWholeNumber and oOther.IsWholeNumber Then
        iPrec := INTEGER_PRECISION
      else if IsWholeNumber then
        iPrec := IntegerMax(oOther.FPrecision, length(r) - d)
      else if oOther.IsWholeNumber then
        iPrec := IntegerMax(FPrecision, length(r) - d)
      Else
        iPrec := IntegerMax(IntegerMin(FPrecision, oOther.FPrecision), length(r) - d);
      while (length(r) > iPrec) Do
      begin
        bUp := (r[Length(r)] > '5');
        delete(r, Length(r), 1);
        if bUp then
          r := roundUp(r, d);
        dec(d);
      end;
    end;
    result := valueOf(r);
    result.FStatus := sdsNumber;
    result.FDecimal := length(r) - d + 1;
    result.FNegative := FNegative <> oOther.FNegative;
    result.FPrecision := iPrec;
    result.FScientific := FScientific or oOther.FScientific;
//    writeln('  '+asdecimal+' / ' +oOther.AsDecimal+' = ' +result.AsDecimal);
  end;
end;

Function TFslDecimalHelper.DivInt(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := DivInt(oTemp);
end;


Function TFslDecimalHelper.DivInt(oOther : TFslDecimal) : TFslDecimal;
Begin
  result := Divide(oOther);
  if (result.FStatus = sdsNumber) then
    result := result.Trunc;
end;


Function TFslDecimalHelper.Modulo(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Modulo(oTemp);
end;


Function TFslDecimalHelper.Modulo(oOther : TFslDecimal) : TFslDecimal;
var
  t, t2 : TFslDecimal;
Begin
  t := DivInt(oOther);
  if (t.FStatus = sdsNumber) then
  begin
    t2 := t.Multiply(oOther);
    result := subtract(t2);
  end
  else
    result := t;
end;


Function TFslDecimalHelper.Subtract(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Subtract(oTemp);
end;

Function TFslDecimalHelper.Subtract(oOther : TFslDecimal) : TFslDecimal;
Begin
  if isNull or oOther.IsNull then
    exit(makeNull)
  else if IsUndefined or oOther.IsUndefined then
    exit(makeUndefined)
  else if IsInfinite or oOther.IsInfinite then
  begin
    if IsNegative = oOther.IsNegative then
      exit(makeUndefined)
    else if IsNegative then
      exit(makeInfinity)
    else
      exit(makeInfinity.Negated);
  end
  else if (FNegative and not oOther.FNegative) then
  Begin
    result := DoAdd(oOther);
    result.FNegative := true;
  end
  Else if (not FNegative and oOther.FNegative) then
    result := DoAdd(oOther)
  Else if (FNegative and oOther.FNegative) then
  begin
    result := DoSubtract(oOther);
    result.FNegative := not result.FNegative;
  end
  Else
  begin
    result := oOther.DoSubtract(self);
    result.FNegative := not result.FNegative;
  end;
end;

Function TFslDecimalHelper.Equals(oOther : TFslDecimal) : Boolean;
Begin
  result := Equals(self, oOther);
end;


Procedure TFslDecimalHelper.SetValueScientific(sValue: String);
var
  i : integer;
  s, e : String;
begin
  StringSplit(sValue, 'e', s, e);

  if (s= '') or (s = '-') or not StringIsDecimal(s) then
    raise ELibraryException.create('"'+sValue+'" is not a valid FDecimal (numeric)');
  if (e= '') or (e = '-') or not StringIsDecimal(e) then
    raise ELibraryException.create('"'+sValue+'" is not a valid FDecimal (exponent)');

  SetValueDecimal(s);
  FScientific := true;

  // now adjust for exponent

  if e[1] = '-' then
    i := 2
  Else
    i := 1;
  while i <= length(e) Do
  begin
    if not CharInSet(e[i], ['0'..'9', '-', '+']) then
      raise ELibraryException.create('"'+sValue+'" is not a valid FDecimal');
    inc(i);
  end;
  i := StrToInt(e);
  FDecimal := FDecimal + i;
end;

function TFslDecimalHelper.GetPrecision: integer;
begin
  result := FPrecision;
end;

Function TFslDecimalHelper.GetValue: String;
begin
  if IsInfinite then
  begin
    if IsNegative then
      result := '-∞'
    else
      result := '∞';
  end
  else if IsUndefined then
    result := '?'
  else if IsNull then
    result := ''
  else if FScientific then
    result := GetValueScientific
  Else
    result := GetValueDecimal;
end;
{
Function TFslDecimal.GetValueByPrecision: String;
begin
  if FScientific then
    result := GetValueScientificByPrecision
  Else
    result := GetValueDecimalByPrecision;
end;
}

Function TFslDecimalHelper.GetValueScientific: String;
var
  bZero : Boolean;
begin
  if IsInfinite then
  begin
    if IsNegative then
      result := '-∞'
    else
      result := '∞';
  end
  else if IsUndefined then
    result := '?'
  else if IsNull then
    result := ''
  else
  begin
    result := FDigits;
    bZero := AllZeros(result, 1);
    if bZero then
    begin
      if FPrecision < 2 then
        result := '0e0'
      Else
        result := '0.'+StringMultiply('0', FPrecision-1)+'e0';
    end
    Else
    begin
      if Length(FDigits) > 1 then
        insert('.', result, 2);
      result := result + 'e'+inttostr(FDecimal - 2);
    end;
    if FNegative and not bZero then
      result := '-' + result;
  end;
end;


function TFslDecimalHelper.immediateLowerBound: TFslDecimal;
var
  i : integer;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else if IsZero then
  begin
    result := immediateUpperBound;
    result.FNegative := true;
  end
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.FPrecision);
    if FNegative then
    result.FDigits := result.FDigits + StringMultiply('0', 25 - length(result.FDigits) - result.FDecimal)+'1'
    else
    begin
      i := length(result.FDigits);
      result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      while (i > 0) and (result.FDigits[i] < '0') do
      begin
        result.FDigits[i] := '9';
        dec(i);
        result.FDigits[i] := char(ord(result.FDigits[i]) - 1)
      end;
      assert(i > 0);
    result.FDigits := result.FDigits + StringMultiply('9', 24 - length(result.FDigits))+'9'
    end;
  end;
end;

function TFslDecimalHelper.immediateUpperBound: TFslDecimal;
var
  i : integer;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.FPrecision);
    if not FNegative then
      result.FDigits := result.FDigits + StringMultiply('0', 25 - length(result.FDigits) - result.FDecimal)+'1'
    else
    begin
      i := length(result.FDigits);
      result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      while (i > 1) and (result.FDigits[i] < '0') do
      begin
        result.FDigits[i] := '0';
        dec(i);
        result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      end;
      assert(i > 0);
      result.FDigits := result.FDigits + StringMultiply('0', 24 - length(result.FDigits))+'9'
    end;
  end;
end;

Function TFslDecimalHelper.GetValueDecimal: String;
begin
  if IsInfinite then
  begin
    if IsNegative then
      result := '-∞'
    else
      result := '∞';
  end
  else if IsUndefined then
    result := '?'
  else if IsNull then
    result := ''
  else
  begin
    result := FDigits;
    if FDecimal <> length(FDigits) + 1 then
      if FDecimal < 1 then
        result := '0.'+StringMultiply('0', 1-FDecimal)+FDigits
      Else if FDecimal <= length(result) then
        if (FDecimal = 1) then
          result := '0.'+result
        Else
          insert('.', result, FDecimal)
      Else
        result := result + stringMultiply('0', FDecimal - length(result)-1);
    if (FPrecision = INTEGER_PRECISION) and result.Contains('.') and (AllZerosButLast(result, pos('.', result)+1)) then
      result := copy(result, 1, pos('.', result)-1);
    if FNegative and not AllZeros(result, 1) then
      result := '-' + result;
  end;
end;

Function TFslDecimalHelper.cdig(i: integer): Char;
begin
//  assert((i >= 0) and (I <= 9));
  result := char(i + ord('0'));
end;

Function TFslDecimalHelper.dig(c: Char): integer;
begin
//  assert(c in ['0'..'9']);
  result := ord(c) - ord('0');
end;

Function TFslDecimalHelper.IsZero: Boolean;
begin
  if FStatus = sdsNumber then
    result := AllZeros(FDigits, 1)
  else
    result := false;
end;

function TFslDecimalHelper.Link: TFslDecimal;
begin
  result.FStatus := FStatus;
  result.FPrecision := FPrecision;
  result.FScientific := FScientific;
  result.FNegative := FNegative;
  result.FDigits := FDigits;
  result.FDecimal := FDecimal;
end;

class function TFslDecimalHelper.makeInfinity: TFslDecimal;
begin
  result := makeNull;
  result.FStatus := sdsInfinite;
end;

function TFslDecimalHelper.IsInfinite: Boolean;
begin
  result := FStatus = sdsInfinite;
end;

function TFslDecimalHelper.IsNegative: boolean;
begin
  if FStatus in [sdsNumber, sdsInfinite] then
    result := FNegative
  else
    result := false;
end;

function TFslDecimalHelper.isANumber: boolean;
begin
  result := FStatus = sdsNumber;
end;

function TFslDecimalHelper.IsNull: boolean;
begin
  result := FStatus = sdsUnknown;
end;

Function TFslDecimalHelper.IsOne: Boolean;
begin
  result := Compares(makeOne) = 0;
end;

function TFslDecimalHelper.IsUndefined: boolean;
begin
  result := FStatus = sdsUndefined;
end;

Function TFslDecimalHelper.Compares(oOther: TFslDecimal): Integer;
Begin
  result := Compares(self, oOther);
end;

class function TFslDecimalHelper.Create(value: String): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(value);
end;

class function TFslDecimalHelper.Create(value: Integer): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(inttostr(value));
end;

class function TFslDecimalHelper.Create(value: int64): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(inttostr(value));
end;

Function TFslDecimalHelper.isWholeNumber: Boolean;
begin
  if FStatus = sdsNumber then
    result := pos('.', GetValueDecimal) = 0
  else
    result := false;
end;

Function CardinalToint64(i : Cardinal):Int64;
Begin
  result := i;
end;

Function TFslDecimalHelper.Trunc: TFslDecimal;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else if FDecimal < 1 then
    result := makeZero
  Else
  begin
    result := self;
    if Length(result.FDigits) >= FDecimal then
      SetLength(result.FDigits, FDecimal-1);
    if result.FDigits = '' then
    begin
      result.FDigits := '0';
      result.FDecimal := 2;
    end;
  end;
end;

class function TFslDecimalHelper.makeUndefined: TFslDecimal;
begin
  result.FStatus := sdsUndefined;
end;

function TFslDecimalHelper.upperBound: TFslDecimal;
var
  i : integer;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.FPrecision);
    if not FNegative then
      result.FDigits := result.FDigits + '5'
    else
    begin
      i := length(result.FDigits);
      result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      while (i > 1) and (result.FDigits[i] < '0') do
      begin
        result.FDigits[i] := '0';
        dec(i);
        result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      end;
      assert(i > 0);
      result.FDigits := result.FDigits + '5'
    end;
  end;
end;

class function TFslDecimalHelper.ValueOf(value: Double): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(FloatToStr(value));
end;

Function TFslDecimalHelper.AsCardinal: Cardinal;
var
  r : Int64;
  m : Int64;
begin
  r := AsInt64;
  if r < 0 then
    raise ELibraryException.create('Unable to represent '+AsString+' as an unsigned 4 byte number');
  m := High(Cardinal);
  if r > m then
    raise ELibraryException.create('Unable to represent '+AsString+' as an unsigned 4 byte number');
  result := r;
end;

function TFslDecimalHelper.normaliseDecimal(digits, decimals : integer; defUp : boolean) : String;
var
  s, sd : String;
  d, a : TFslDecimal;
  i : integer;
  function allChar(s : String; tch : char) : boolean;
  var
    ch : char;
  begin
    result := true;
    for ch in s do
      if ch <> tch then
        exit(false);
  end;
begin
  if IsNull or IsUndefined then
    exit('?'+StringPadLeft('', '?', digits)+'.'+StringPadLeft('', '?', decimals));

  d := ValueOf('1'+StringPadRight('', '0', digits));
  if IsNegative then
  begin
    a := Abs;
    if Compares(a, d) > 0 then // this number is smaller than we can cope with
      if defUp then
        exit('!'+StringPadLeft('', '0', digits)+'.'+StringPadLeft('', '0', decimals))
      else
        exit('!'+StringPadLeft('', '#', digits)+'.'+StringPadLeft('', '#', decimals));
    s := d.Subtract(a).AsDecimal;
  end
  else
  begin
    if Compares(self, d) >= 0 then // this number is bigger than we can cope with
      if defUp then
        exit('0'+StringPadLeft('', 'X', digits)+'.'+StringPadLeft('', 'X', decimals))
      else
        exit('0'+StringPadLeft('', '9', digits)+'.'+StringPadLeft('', '9', decimals));
    s := AsDecimal;
  end;

  i := s.IndexOf('.');
  if i < 0 then
    i := s.Length;
  result := s.subString(0, i);
  result := StringPadLeft(result, '0', digits);
  sd := '';
  if i < s.Length then
    sd := s.Substring(i+1);
  if (sd.Length > decimals) then
  begin
    sd := sd.Substring(0, decimals);
    if IsNegative then
    begin
      if allChar(sd, '9') and allChar(result, '9') then
        if defUp then
          exit('0'+StringPadLeft('', '0', digits)+'.'+StringPadLeft('', '0', decimals))
        else
          exit('!'+StringPadLeft('', '9', digits)+'.'+StringPadLeft('', '9', decimals));
    end
    else if allChar(sd, '0') and allChar(result, '0') then
    begin
      if defUp then
        exit('0'+StringPadLeft('', '0', digits)+'.'+StringPadLeft('', '0', decimals-1)+'1')
      else
        exit('0'+StringPadLeft('', '0', digits)+'.'+StringPadLeft('', '0', decimals));
    end;
  end
  else
    sd := StringPadRight(sd, '0', decimals);
  result := result + '.' + sd;
  if IsNegative then
    result := '!'+result
  else
    result := '0'+result;
end;

function TFslDecimalHelper.AsDouble: Double;
begin
  if not isANumber then
    raise ELibraryException.create('Unable to represent '+AsString+' as an double');
  result := StrToFloat(AsScientific);
end;

function TFslDecimalHelper.AsInt64: Int64;
var
  t : TFslDecimal;
begin
  if not isWholeNumber then
    raise ELibraryException.create('Unable to represent '+AsString+' as an integer');
  t := valueOf(Low(Int64));
  if Compares(self, t) < 0 then
    raise ELibraryException.create('Unable to represent '+AsString+' as a signed 8 byte integer');
  t := valueOf(High(Int64));
  if Compares(self, t) > 0 then
    raise ELibraryException.create('Unable to represent '+AsString+' as a signed 8 byte integer');
  result := StrToInt64(AsDecimal);
end;

function TFslDecimalHelper.AsInteger: Integer;
var
  r : Int64;
  m : Int64;
begin
  r := AsInt64;
  m := Low(Integer);
  if r < m then
    raise ELibraryException.create('Unable to represent '+AsString+' as a signed 4 byte number');
  m := high(Integer);
  if r > m then
    raise ELibraryException.create('Unable to represent '+AsString+' as a signed 4 byte number');
  result := r;
end;

class Function TFslDecimalHelper.valueOf(value : String) : TFslDecimal;
begin
  result := makeNull;
  result.SetValue(value);
end;

class Function TFslDecimalHelper.valueOf(value : Integer) : TFslDecimal;
begin
  result := makeNull;
  result.SetValue(inttostr(value));
end;

class Function TFslDecimalHelper.valueOf(value : int64) : TFslDecimal;
begin
  result := makeNull;
  result.SetValue(inttostr(value));
end;

class Function TFslDecimalHelper.Equals(oOne, oTwo : TFslDecimal) : Boolean;
Begin
  if (oOne.FStatus = sdsInfinite) and (oTwo.FStatus = sdsInfinite) then
    result := oOne.IsNegative = oTwo.IsNegative
  else if (oOne.FStatus = sdsUndefined) and (oTwo.FStatus = sdsUndefined) then
    result := true
  else if (oOne.FStatus = sdsNumber) and (oTwo.FStatus = sdsNumber) then
    result := Compares(oOne, oTwo) = 0
  else
    result := false;
end;

class Function TFslDecimalHelper.makeOne: TFslDecimal;
begin
  result := ValueOf(1);
end;

class Function TFslDecimalHelper.makeZero: TFslDecimal;
begin
  result := ValueOf(0);
end;


function round(prefix, s : String; i : integer) : String;
begin
  result := s;
  if s.Length > i then
  begin
    result := copy(result, 1, i);
    if (s[i+1]) >= '5' then
      result[i] := chr(ord(result[i])+1);
    while (i > 0) and  (result[i] > '9') do
    begin
      result[i] := '0';
      dec(i);
      result[i] := chr(ord(result[i])+1);
    end;
  end;
  if i = 0 then
  begin
    result := copy(prefix, 1, length(prefix)-1) +'1' + result;
  end
  else
    result := prefix + result;
end;

class Function TFslDecimalHelper.Compares(oOne, oTwo: TFslDecimal): Integer;
var
  iMax : Integer;
  s1, s2 : String;
Begin
  if (oOne.FStatus in [sdsUnknown, sdsUndefined]) or (oTwo.FStatus in [sdsUnknown, sdsUndefined]) then
    raise ELibraryException.Create('Unable to compare "'+oOne.AsString+'" and "'+oTwo.AsString+'"')
  else if oOne.FNegative and not oTwo.FNegative then
    result := -1
  else if not oOne.FNegative and oTwo.FNegative then
    result := 1
  else if oOne.IsInfinite and oTwo.IsInfinite then
    result := 0
  else if oOne.IsInfinite then
    result := 1
  else if oTwo.IsInfinite then
    result := -1
  else
  begin
    iMax := IntegerMax(oOne.FDecimal, oTwo.FDecimal);
    s1 := round('0'+StringMultiply('0', iMax - oOne.FDecimal+1), oOne.FDigits, oOne.FPrecision);
    s2 := round('0'+StringMultiply('0', iMax - oTwo.FDecimal+1), oTwo.FDigits, oTwo.FPrecision);
    if Length(s1) < length(s2) then
      s1 := s1 + StringMultiply('0', length(s2) - length(s1))
    else if Length(s2) < length(s1) then
      s2 := s2 + StringMultiply('0', length(s1) - length(s2));
    result := StringCompare(s1, s2);
    if oOne.FNegative then
      result := -result;
  End;
end;


Function TFslDecimalHelper.lowerBound: TFslDecimal;
var
  i : integer;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else if IsZero then
  begin
    result := upperBound;
    result.FNegative := true;
  end
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.FPrecision);
    if FNegative then
      result.FDigits := result.FDigits + '5'
    else
    begin
      i := length(result.FDigits);
      result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      while (i > 0) and (result.FDigits[i] < '0') do
      begin
        result.FDigits[i] := '9';
        dec(i);
        result.FDigits[i] := char(ord(result.FDigits[i]) - 1)
      end;
      assert(i > 0);
      result.FDigits := result.FDigits + '5';
    end;
  end;
end;


class function TFslDecimalHelper.makeNull: TFslDecimal;
begin
  result.FStatus := sdsUnknown;
  result.FNegative := false;
  result.FDigits := '';
  result.FDecimal := 0;
  result.FPrecision := 0;
  result.FScientific := false;
end;

Function GUIDAsOIDWrong(Const aGUID : TGUID) : String;
var
  sGuid, s : String;
  r1, r2, r3, r4 : int64;
  c : integer;
  b1, b2, b3, b4, bs : TFslDecimal;
Begin
  sGuid := GUIDToString(aGuid);
  s := copy(sGuid, 30, 8);
  Val('$'+s, r1, c);
  s := copy(sGuid, 21, 4)+copy(sGuid, 26, 4);
  Val('$'+s, r2, c);
  s := copy(sGuid, 11, 4)+copy(sGuid, 26, 4);
  Val('$'+s, r3, c);
  s := copy(sGuid, 2, 8);
  Val('$'+s, r4, c);

  b1 := TFslDecimal.valueOf(r1);
  b2 := TFslDecimal.valueOf(r2);
  b3 := TFslDecimal.valueOf(r3);
  b4 := TFslDecimal.valueOf(r4);
  bs := TFslDecimal.valueOf('4294967296');
  b2 := b2.Multiply(bs);
  bs := TFslDecimal.valueOf('18446744073709551616');
  b3 := b3.Multiply(bs);
  bs := TFslDecimal.valueOf('79228162514264337593543950336');
  b4 := b4.Multiply(bs);
  b1 := b1.Add(b2);
  b1 := b1.Add(b3);
  b1 := b1.Add(b4);
  result := '2.25.'+b1.AsString;
end;


Function GUIDAsOIDRight(Const aGUID : TGUID) : String;
var
  sGuid, s : String;
  r1, r2, r3, r4 : int64;
  c : integer;
  b1, b2, b3, b4, bs : TFslDecimal;
Begin
  sGuid := GUIDToString(aGuid);
  s := copy(sGuid, 30, 8);
  Val('$'+s, r1, c);
  s := copy(sGuid, 21, 4)+copy(sGuid, 26, 4);
  Val('$'+s, r2, c);
  s := copy(sGuid, 11, 4)+copy(sGuid, 16, 4);
  Val('$'+s, r3, c);
  s := copy(sGuid, 2, 8);
  Val('$'+s, r4, c);

  b1 := TFslDecimal.valueOf(r1);
  b2 := TFslDecimal.valueOf(r2);
  b3 := TFslDecimal.valueOf(r3);
  b4 := TFslDecimal.valueOf(r4);
  bs := TFslDecimal.valueOf('4294967296');
  b2 := b2.Multiply(bs);
  bs := TFslDecimal.valueOf('18446744073709551616');
  b3 := b3.Multiply(bs);
  bs := TFslDecimal.valueOf('79228162514264337593543950336');
  b4 := b4.Multiply(bs);
  b1 := b1.Add(b2);
  b1 := b1.Add(b3);
  b1 := b1.Add(b4);
  result := '2.25.'+b1.AsString;
end;

{ TFslDecimal }

class operator TFslDecimal.Add(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Add(b);
end;

class operator TFslDecimal.Divide(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Divide(b);
end;

class operator TFslDecimal.Equal(a, b: TFslDecimal): Boolean;
begin
  result := Equals(a, b);
end;

class operator TFslDecimal.Explicit(a: String): TFslDecimal;
begin
  result := TFslDecimal.ValueOf(a);
end;

class operator TFslDecimal.Explicit(a: Double): TFslDecimal;
begin
  result := TFslDecimal.ValueOf(a);
end;

class operator TFslDecimal.Explicit(a: Integer): TFslDecimal;
begin
  result := TFslDecimal.ValueOf(a);
end;

class operator TFslDecimal.GreaterThan(a, b: TFslDecimal): Boolean;
begin
  result := TFslDecimal.Compares(a, b) > 0;
end;

class operator TFslDecimal.GreaterThanOrEqual(a, b: TFslDecimal): Boolean;
begin
  result := TFslDecimal.Compares(a, b) >= 0;
end;

class operator TFslDecimal.Implicit(a: TFslDecimal): Integer;
begin
  result := a.AsInteger;
end;

class operator TFslDecimal.Implicit(a: TFslDecimal): Double;
begin
  result := a.AsDouble;
end;

class operator TFslDecimal.Implicit(a: TFslDecimal): String;
begin
  result := a.AsString;
end;

class operator TFslDecimal.IntDivide(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.DivInt(b);
end;

class operator TFslDecimal.LessThan(a, b: TFslDecimal): Boolean;
begin
  result := TFslDecimal.Compares(a, b) < 0;
end;

class operator TFslDecimal.LessThanOrEqual(a, b: TFslDecimal): Boolean;
begin
  result := TFslDecimal.Compares(a, b) <= 0;
end;

class operator TFslDecimal.Modulus(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Modulo(b);
end;

class operator TFslDecimal.Multiply(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Multiply(b);
end;

class operator TFslDecimal.Negative(a: TFslDecimal): TFslDecimal;
begin
  result := a.Negated;
end;

class operator TFslDecimal.NotEqual(a, b: TFslDecimal): Boolean;
begin
  result := not TFslDecimal.Equals(a, b);
end;

class operator TFslDecimal.Positive(a: TFslDecimal): TFslDecimal;
begin
  result := a;
end;

class operator TFslDecimal.Subtract(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Subtract(b);
end;

class operator TFslDecimal.Trunc(a: TFslDecimal): TFslDecimal;
begin
  result := a.Trunc;
end;

class function TFslDecimalHelper.Create(value: Double): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(FloatToStr(value));
end;

{$IFNDEF UT}

function TFslBytesBuilder.AsBytes : TBytes;
Begin
  result := AsString;
End;

Function BytesContains(bytes : TBytes; value : TByte): Boolean;
Begin
  result := pos(char(value), bytes) > 0;
End;

Function BytesAdd(bytes1, bytes2 : TBytes) : TBytes;
Begin
  result := bytes1 + bytes2;
End;

Function BytesAdd(bytes : TBytes; byte : TByte) : TBytes;
Begin
  result := bytes + byte;
End;

function CompareBytes(bytes1, bytes2 : TBytes) : Integer;
Begin
  result := CompareStr(bytes1, bytes2);
End;

function SameBytes(bytes1, bytes2 : TBytes) : Boolean;
Begin
  result := bytes1 = bytes2;
End;

Function StringAsBytes(s : AnsiString) : TBytes;
Begin
  result := s;
End;


Function BytesAsString(a : TBytes) : AnsiString;
Begin
  result := a;
End;

Function BytesAsMime(a : TBytes) : String;
var
  o : TFslStringBuilder;
  i : integer;
begin
  o := TFslStringBuilder.Create;
  try
    for i := 1 to length(a) do
      if (a[i] >= char(32)) or (a[i] <= char(127)) Then
        o.Append(a[i])
      else
        o.Append('#'+inttostr(ord(a[i])));
    result := o.AsString;
  Finally
    o.Free;
  End;
End;

Function BytesReplace(const a, OldPattern, NewPattern: TBytes): TBytes;
begin
  result := StringReplace(a, oldPattern, NewPattern, [rfReplaceAll]);
End;

Function Bytes(a : Array of byte) : TBytes;
var
  i : integer;
Begin
  SetLength(result, length(a));
  for i := Low(a) to high(a) do
    result[i+1] := char(a[i]);
End;

{$ELSE}

  (*
Procedure TFslBytesBuilder.Clear;
Begin
  FContent := '';
  FLength := 0;
End;


Function TFslBytesBuilder.AsBytes : TBytes;
Begin
//  Result := Copy(FContent, 1, FLength);
  SetLength(result, FLength);
  move(FContent, result, FLength);
End;


Procedure TFslBytesBuilder.WriteToStream(aStream : TFslStream);
Begin
  If FLength > 0 Then
    aStream.Write(FContent[1], FLength);
End;


Procedure TFslBytesBuilder.AppendPadded(Const sStr : String; iCount : Integer; cPad : Char = ' ');
Var
  iLen : Integer;
Begin
  iLen := IntegerMax(System.Length(sStr), iCount);

  If (iLen > 0) Then
  Begin
    If FLength + iLen > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iLen));

    Move(sStr[1], FContent[FLength + 1], System.Length(sStr));

    If iLen = iCount Then
      FillChar(FContent[FLength + 1 + System.Length(sStr)], iCount - System.Length(sStr), cPad);

    Inc(FLength, iLen);
  End;
End;

Procedure TFslBytesBuilder.AppendFixed(Const sStr : String; iCount : Integer; cPad : Char = ' ');
Begin
  If (iCount > 0) Then
  Begin
    If FLength + iCount > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iCount));
    Move(sStr[1], FContent[FLength + 1], IntegerMin(System.Length(sStr), iCount));

    If System.Length(sStr) < iCount Then
      FillChar(FContent[FLength + 1 + System.Length(sStr)], iCount - System.Length(sStr), cPad);

    Inc(FLength, iCount);
  End;
End;


Procedure TFslBytesBuilder.Append(ch : Char);
Begin
  If FLength + 1 > System.Length(FContent) Then
    SetLength(FContent, System.Length(FContent) + FBufferSize);

  Move(ch, FContent[FLength + 1], 1);
  Inc(FLength);
End;

Procedure TFslBytesBuilder.Append(Const sStr : String);
Begin
  If (sStr <> '') Then
  Begin
    If FLength + System.Length(sStr) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, System.Length(sStr)));

    Move(sStr[1], FContent[FLength + 1], System.Length(sStr));

    Inc(FLength, System.Length(sStr));
  End;
End;


Procedure TFslBytesBuilder.Append(Const oStream : TFslStream; iBytes : Integer);
Begin
  If (iBytes > 0) Then
  Begin
    If FLength + iBytes > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + Integermax(FBufferSize, iBytes));

    oStream.Read(FContent[FLength + 1], iBytes);

    Inc(FLength, iBytes);
  End;
End;


Procedure TFslBytesBuilder.Append(Const oBuilder : TFslBytesBuilder);
Begin
  Append(oBuilder.AsBytes);
End;


Procedure TFslBytesBuilder.AppendEOL;
Begin
  Append(cReturn);
End;


Procedure TFslBytesBuilder.Insert(Const sStr : String; iIndex : Integer);
Begin
  If (sStr <> '') Then
  Begin
    If FLength + System.Length(sStr) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, System.Length(sStr)));

    If (iIndex) <> FLength Then
      Move(FContent[iIndex+1], FContent[iIndex+1 + System.Length(sStr)], FLength - iIndex);

    Move(sStr[1], FContent[iIndex+1], System.Length(sStr));

    Inc(FLength, System.Length(sStr));
  End;
End;


Procedure TFslBytesBuilder.Insert(Const oStream : TFslStream; iBytes : Integer; iIndex : Integer);
Begin
  If (iBytes > 0) Then
  Begin
    If FLength + iBytes > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iBytes));

    If (iIndex) <> FLength Then
      Move(FContent[iIndex+1], FContent[iIndex+1 + iBytes], FLength - iIndex);

    oStream.Read(FContent[iIndex + 1], iBytes);

    Inc(FLength, iBytes);
  End;
End;


Procedure TFslBytesBuilder.Insert(Const oBuilder : TFslBytesBuilder; iIndex : Integer);
Begin
  Insert(oBuilder.AsBytes, iIndex);
End;


Procedure TFslBytesBuilder.Delete(iIndex, iLength : Integer);
Begin
  System.Delete(FContent, iIndex+1, iLength);
  Dec(FLength, iLength);
End;



constructor TFslBytesBuilder.Create;
begin
  inherited;
  FBufferSize := BUFFER_INCREMENT_SIZE;
end;

procedure TFslBytesBuilder.CommaAdd(const sStr: String);
begin
  if Length > 0 Then
    Append(', ');
  Append(sStr);
end;

procedure TFslBytesBuilder.AddCardinalAsBytes(iVal: Cardinal);
var
  s : String;
begin
  SetLength(s, 4);
  move(iVal, s[1], 4);
  Append(s);
end;

procedure TFslBytesBuilder.AddWordAsBytes(iVal: word);
var
  s : String;
begin
  SetLength(s, 2);
  move(iVal, s[1], 2);
  Append(s);
end;

procedure TFslBytesBuilder.AddInt64AsBytes(iVal: Int64);
var
  s : String;
begin
  SetLength(s, 8);
  move(iVal, s[1], 8);
  Append(s);
end;

procedure TFslBytesBuilder.AddByteAsBytes(iVal: TByte);
var
  s : String;
begin
  SetLength(s, 1);
  move(iVal, s[1], 1);
  Append(s);
end;

procedure TFslBytesBuilder.Append(const bBytes: array of TByte; amount: Integer);
var
  i : integer;
begin
  for i := 0 to amount - 1 Do
    Append(chr(bBytes[i]));

end;

procedure TFslBytesBuilder.Append(const aBytes: TBytes);
begin

end;

procedure TFslBytesBuilder.Insert(const aBytes: TBytes; iIndex: Integer);
begin

end;
    *)

Function BytesContains(bytes : TBytes; value : TByte): Boolean;
var
  i : integer;
Begin
  result := true;
  for i := Low(bytes) to high(bytes) do
    if bytes[i] = value then
      exit;
  result := false;
End;

Function BytesAdd(bytes1, bytes2 : TBytes) : TBytes;
Begin
  SetLength(result, length(bytes1) + length(bytes2));
  move(bytes1[0], result[0], length(bytes1));
  move(bytes2[0], result[length(bytes1)], length(bytes2));
End;

Function BytesAdd(bytes : TBytes; byte : TByte) : TBytes;
Begin
  SetLength(result, length(bytes) + 1);
  move(bytes[0], result[0], length(bytes));
  result[length(result)-1] := byte;
End;

(*
procedure TFslBytesBuilder.Append(b: TByte);
begin
  Append(AnsiChar(b));
end;
  *)

function CompareBytes(bytes1, bytes2 : TBytes) : Integer;
begin
  result := length(bytes1) - length(bytes2);
  if result = 0 then
    if (length(bytes1) = 0) then
      result := 0
    else
      result := integer(not compareMem(@bytes1[0], @bytes2[0], length(bytes1)));
End;

function SameBytes(bytes1, bytes2 : TBytes) : Boolean;
Begin
  result := CompareBytes(bytes1, bytes2) = 0;
End;

function CompareBytes(bytes1 : TBytes; bytes2 : AnsiString) : Integer;
Begin
  result := CompareBytes(bytes1, AnsiStringAsBytes(bytes2));
End;

function SameBytes(bytes1 : TBytes; bytes2 : AnsiString) : Boolean;
Begin
  result := SameBytes(bytes1, AnsiStringAsBytes(bytes2));
End;

Function AnsiStringAsBytes(s : AnsiString):TBytes;
Begin
  setLength(result, length(s));
  move(s[1], result[0], length(s));
End;

Function StringAsBytes(s : String):TBytes;
Begin
  {$IFDEF VER130}

  {$ELSE}
  result := TEncoding.UTF8.GetBytes(s);
  {$ENDIF}
End;

Function BytesAsAnsiString(a : TBytes) : AnsiString;
Begin
  setLength(result, length(a));
  move(a[0], result[1], length(a));
End;

Function BytesAsString(a : TBytes) : String;
var
  i : integer;
Begin
  setLength(result, length(a));
  for i := Low(a) to High(a) do
   result[i + 1] := Char(a[i]);
End;

Function BytesAsMime(a : TBytes) : String;
var
  o : TFslStringBuilder;
  i : integer;
begin
  o := TFslStringBuilder.Create;
  try
    for i := low(a) to high(a) do
      if (a[i] >= 32) or (a[i] <= 127) Then
        o.Append(char(a[i]))
      else
        o.Append('#'+inttostr(a[i]));
    result := o.AsString;
  Finally
    o.Free;
  End;
End;

Function Bytes(a : Array of TByte) : TBytes;
var
  i : integer;
Begin
  SetLength(result, length(a));
  for i := Low(result) to high(result) do
    result[i] := a[i];
End;

Function BytesReplace(const a, OldPattern, NewPattern: TBytes): TBytes;
var
  v : String;
begin
  v := SysUtils.StringReplace(BytesAsString(a), BytesAsString(OldPattern), BytesAsString(NewPattern), [rfReplaceAll]);
  result := StringAsBytes(v);
end;

{$ENDIF}

Function Fillbytes(b : byte; count : Integer): TBytes;
Begin
  SetLength(result, count);
  FillChar(result[1], count, b);
End;

Function Fillbytes(init : TBytes; b : byte; count : Integer): TBytes;
Begin
  SetLength(result, Max(count, length(init)));
  FillChar(result[1], count, b);
  if Length(init) > 0 Then
    move(init[1], result[1], length(init));
End;

Function BytesStartsWith(bytes, find : TBytes) : Boolean;
var
  i : integer;
Begin
  Result := length(bytes) >= length(find);
  i := 0;
  while result and (i < length(find)) do
  Begin
    result := bytes[i] = find[i];
    inc(i);
  End;
End;

constructor TFslBytesBuilder.Create;
begin
  inherited;
  FBufferSize := BUFFER_INCREMENT_SIZE;
end;

function TFslBytesBuilder.EndsWith(aBytes: TBytes): Boolean;
var
  i : integer;
begin
  result := (FLength >= System.Length(aBytes)) And (System.Length(aBytes) > 0);
  if result then
    for i := Low(aBytes) to High(aBytes) do
      result := result And (aBytes[i] = FContent[FLength - System.Length(aBytes) + i]);
end;

procedure TFslBytesBuilder.Read(index : cardinal; var buffer; ilength: cardinal);
begin
  if index < 1 Then
    RaiseError('Read', 'index < 1');
  if index + ilength > FLength Then
    RaiseError('Read', 'index > length');
  Move(FContent[index], buffer, ilength);
end;

procedure TFslBytesBuilder.WriteCardinal(index: cardinal; val: cardinal);
begin
  if index < 1 Then
    RaiseError('Overwrite', 'index < 1');
  if index + 4 > FLength Then
    RaiseError('Overwrite', 'index > length');
  Move(val, FContent[index], 4);
end;

procedure TFslBytesBuilder.WriteString(index: cardinal; val: String);
begin
  if index < 1 Then
    RaiseError('Overwrite', 'index < 1');
  if index + clength(val)*2 > FLength Then
    RaiseError('Overwrite', 'index > length');
  Move(val[1], FContent[index], (val.Length*2));
end;

procedure TFslBytesBuilder.WriteUInt64(index: cardinal; val: UInt64);
begin
  if index < 1 Then
    RaiseError('Overwrite', 'index < 1');
  if index + 8 > FLength Then
    RaiseError('Overwrite', 'index > length');
  Move(val, FContent[index], 8);
end;

procedure TFslBytesBuilder.WriteWord(index: cardinal; val: word);
begin
  if index < 1 Then
    RaiseError('Overwrite', 'index < 1');
  if index + 2 > FLength Then
    RaiseError('Overwrite', 'index > length');
  Move(val, FContent[index], 2);
end;

Procedure TFslBytesBuilder.Clear;
Begin
  FContent := nil;
  FLength := 0;
End;


Function TFslBytesBuilder.AsBytes : TBytes;
Begin
  Result := Copy(FContent, 0, FLength);
End;


Procedure TFslBytesBuilder.Append(ch : AnsiChar);
Begin
  If FLength + 1 > System.Length(FContent) Then
    SetLength(FContent, System.Length(FContent) + FBufferSize);

  Move(ch, FContent[FLength], 1);
  Inc(FLength);
End;

Procedure TFslBytesBuilder.Append(b : Byte);
Begin
  If FLength + 1 > System.Length(FContent) Then
    SetLength(FContent, System.Length(FContent) + FBufferSize);

  Move(b, FContent[FLength], 1);
  Inc(FLength);
End;

procedure TFslBytesBuilder.AddAnsiString(val: AnsiString);
Var
  s : TBytes;
Begin
  SetLength(s, System.Length(val));
  move(val[1], s[0], System.Length(val));
  Append(s);
end;

procedure TFslBytesBuilder.addBase64(val: TBytes);
begin
  AddAnsiString(EncodeBase64(val));
end;

procedure TFslBytesBuilder.AddCardinal(val: cardinal);
Var
  s : TBytes;
Begin
  SetLength(s, 4);
  move(val, s[0], 4);
  Append(s);
end;

procedure TFslBytesBuilder.AddInteger(val: integer);
Var
  s : TBytes;
Begin
  SetLength(s, 4);
  move(val, s[0], 4);
  Append(s);
end;

procedure TFslBytesBuilder.AddString(val: String);
Var
  s : TBytes;
Begin
  if val.length > 0 then
  begin
    SetLength(s, val.Length*2);
    move(val[1], s[0], val.Length*2);
    Append(s);
  end;
end;

procedure TFslBytesBuilder.AddUInt64(val: UInt64);
Var
  s : TBytes;
Begin
  SetLength(s, 8);
  move(val, s[0], 8);
  Append(s);
end;

procedure TFslBytesBuilder.AddUtf8(val: String);
begin
  append(TEncoding.UTF8.GetBytes(val));
end;

procedure TFslBytesBuilder.AddWord(val: word);
Var
  s : TBytes;
Begin
  SetLength(s, 2);
  move(val, s[0], 2);
  Append(s);
end;

procedure TFslBytesBuilder.Append(mem: Pointer; len: word);
begin
  If len > 0 Then
  Begin
    If FLength + len > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, len));

    Move(mem^, FContent[FLength], len);

    Inc(FLength, len);
  End;
end;

Procedure TFslBytesBuilder.Append(Const bytes : TBytes);
Begin
  If System.Length(bytes) > 0 Then
  Begin
    If FLength + System.Length(bytes) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, System.Length(bytes)));

    Move(bytes[0], FContent[FLength], System.Length(bytes));

    Inc(FLength, System.Length(bytes));
  End;
End;

function BytesPos(const SubStr, Str: TBytes; offset : integer = 0): Integer;
var
  i, j: Integer;
  found : boolean;
begin
  Result := -1;
  if length(SubStr) > 0 then
    for I := Offset to Length(Str) - Length(SubStr) - 1 do
    begin
      found := true;
      for j := 0 to length(SubStr)-1 do
        if Str[I + J] <> SubStr[J] then
        begin
          found := false;
          break;
        end;
      if found then
      begin
        result := i;
        exit;
      end;
    end;
end;


Function BytesSplit(Const sValue, sDelimiter : TBytes; Var sLeft, sRight: TBytes) : Boolean;
Var
  iIndex : Integer;
  sA, sB : TBytes;
Begin
  // Find the delimiter within the source string
  iIndex := BytesPos(sDelimiter, sValue);
  Result := iIndex <> -1;

  If Not Result Then
  Begin
    sA := sValue;
    SetLength(sB, 0);
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + Length(sDelimiter), MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;

function StreamToBytes(AStream: TStream): TBytes;
begin
  SetLength(Result, AStream.Size);
  if AStream.Size > 0 then
    begin
    AStream.Position := 0;
    AStream.Read(Result[0], AStream.Size);
    end;
end;

procedure BytesToFile(bytes : TBytes; filename : string);
var
  f : TFileStream;
begin
  f :=  TFileStream.Create(filename, fmCreate);
  try
    f.write(bytes[0], length(bytes));
  finally
    f.Free;
  end;
end;

function FileToBytes(filename : string) : TBytes;
var
  f : TFileStream;
begin
  f :=  TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    setLength(result, f.Size);
    f.Read(result[0], f.Size);
  finally
    f.Free;
  end;
end;

Function BytePos(find : Byte; source : TBytes ) : integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to length(source)-1 do
    if source[i] = find then
    begin
      result := i;
      exit;
    end;
end;

Function BytePos(find : TBytes; source : TBytes ) : integer; Overload;
var
  i : integer;
begin
  result := -1;
  for i := 0 to length(source)-1 do
    if SameBytes(copy(source, i, length(find)), find) then
    begin
      result := i;
      exit;
    end;
end;


function EncodeBase64(const value : TBytes): AnsiString;
begin
  {$IFDEF FPC}
  {$ELSE}
  result := EncdDecd.EncodeBase64(@value[0], length(value));
  {$ENDIF}
end;

{$IFNDEF FPC}
Function DecodeBase64(Const value : AnsiString) : TBytes; Overload;
begin
  result := EncdDecd.DecodeBase64(value);
end;
{$ENDIF}

Function DecodeBase64(Const value : String) : TBytes; Overload;
begin
  {$IFDEF FPC}
  {$ELSE}
  result := EncdDecd.DecodeBase64(AnsiString(value));
  {$ENDIF}
end;


function FormatTextToHTML(AStr: String): String;
var
  LIsNewLine: Boolean;
  I: Integer;
  b : TFslStringBuilder;
begin
  Result := '';
  if Length(AStr) <= 0 then
    exit;
  b := TFslStringBuilder.Create;
  try
    LIsNewLine := True;
    i := 1;
    while i <= Length(AStr) do
      begin
      if (AStr[i] <> #32) and (LIsNewLine) then
        LIsNewLine := False;
      case AStr[i] of
        '''': b.Append('&#' + IntToStr(Ord(AStr[i])) + ';');
        '"': b.Append('&quot;');
        '&': b.Append('&amp;');
        '<': b.Append('&lt;');
        '>': b.Append('&gt;');
        #32:
          begin
          if LIsNewLine then
            b.Append('&nbsp;')
          else
            b.Append(' ');
          end;
        #13:
          begin
          if i < Length(AStr) then
            if AStr[i + 1] = #10 then
              Inc(i);
          b.Append('<br/>');
          LIsNewLine := True;
          end;
        else
          begin
          if CharInSet(AStr[i], [' '..'~']) then
            b.Append(AStr[i])
          else
            b.Append('&#' + IntToStr(Ord(AStr[i])) + ';');
          end;
        end;
      Inc(i);
      end;
    result := b.ToString;
  finally
    b.free;
  end;
end;


function FormatCodeToXML(AStr: String): String;
var
  i: Integer;
begin
  Result := '';
  if Length(AStr) <= 0 then
    exit;
  i := 1;
  while i <= Length(AStr) do
    begin
    case AStr[i] of
      '''':
        Result := Result + '&#' + IntToStr(Ord(AStr[i])) + ';';
      '"':
        Result := Result + '&quot;';
      '&':
        Result := Result + '&amp;';
      '<':
        Result := Result + '&lt;';
      '>':
        Result := Result + '&gt;';
      #32:
        Result := Result + ' ';
      else
        begin
        if CharInSet(AStr[i], [#13, #10, ' '..'~']) then
          Result := Result + AStr[i]
        else
          Result := Result + '&#' + IntToStr(Ord(AStr[i])) + ';';
        end;
      end;
    Inc(i);
    end;
end;

function FormatTextToXML(AStr: String; mode : TXmlEncodingMode): String;
var
  i: Integer;
  b : TFslStringBuilder;
begin
  b := TFslStringBuilder.Create;
  try
    i := 1;
    while i <= Length(AStr) do
      begin
      case AStr[i] of
        '"':
          case mode of
            xmlAttribute : b.append('&quot;');
            xmlCanonical : b.append('&#' + IntToStr(Ord(AStr[i])) + ';');
            xmlText : b.append(AStr[i]);
          end;
        '''':
          case mode of
            xmlAttribute : b.append('&apos;');
            xmlCanonical : b.append('&#' + IntToStr(Ord(AStr[i])) + ';');
            xmlText : b.append(AStr[i]);
          end;
        '&':
          if mode = xmlCanonical then
            b.append('&#' + IntToStr(Ord(AStr[i])) + ';')
          else
            b.append('&amp;');
        '<':
          if mode = xmlCanonical then
            b.append('&#' + IntToStr(Ord(AStr[i])) + ';')
          else
            b.append('&lt;');
        '>':
          if mode = xmlCanonical then
            b.append('&#' + IntToStr(Ord(AStr[i])) + ';')
          else
            b.append('&gt;');
        #32:
          b.append(' ');
        #13, #10:
          begin
          case mode of
            xmlAttribute : b.append('&#' + IntToStr(Ord(AStr[i])) + ';');
            xmlCanonical : b.append('&#' + IntToStr(Ord(AStr[i])) + ';');
            xmlText : b.append(AStr[i]);
          end;
//        canonical?
//          if i < length(AStr) then
//            if AStr[i + 1] = #10 then
//              Inc(i);
//          b.append(' ');
          end;
        else
          begin
          if CharInSet(AStr[i], [' '..'~']) then
            b.append(AStr[i])
          else
            b.append('&#' + IntToStr(Ord(AStr[i])) + ';');
          end;
        end;
      Inc(i);
      end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function FormatXMLForTextArea(AStr: String): String;
var
  i: Integer;
begin
  Result := '';
  if Length(AStr) <= 0 then
    exit;
  i := 1;
  while i <= Length(AStr) do
    begin
    case AStr[i] of
      '''':
        Result := Result + '&#' + IntToStr(Ord(AStr[i])) + ';';
      '"':
        Result := Result + '&quot;';
      '&':
        Result := Result + '&amp;';
      '<':
        Result := Result + '&lt;';
      '>':
        Result := Result + '&gt;';
      #32:
        Result := Result + ' ';
      #13:
        begin
        if i < length(AStr) then
          if AStr[i + 1] = #10 then
            Inc(i);
        Result := Result + #13#10;
        end;
      else
        begin
        if CharInSet(AStr[i], [' '..'~', #10]) then
          Result := Result + AStr[i]
        else
          Result := Result + '&#' + IntToStr(Ord(AStr[i])) + ';';
        end;
      end;
    Inc(i);
    end;
end;

function FormatXMLToHTML(AStr : String):String;
var
  LIsNewLine: Boolean;
  I: Integer;
  LInAmp : Boolean;
  LInTag : boolean;
  LInAttr : Boolean;
  b : TFslStringBuilder;
begin
  LInAmp := false;
  LInTag := false;
  LInAttr := false;
  Result := '';
  if Length(AStr) <= 0 then
    exit;
  b := TFslStringBuilder.Create;
  try
    LIsNewLine := True;
    i := 1;
    while i <= Length(AStr) do
      begin
      if (AStr[i] <> #32) and (LIsNewLine) then
        LIsNewLine := False;
      case AStr[i] of
        '''':
          b.Append('&#' + IntToStr(Ord(AStr[i])) + ';');
        '"':
          begin
          if LInAttr then
            b.Append('</font>');
          b.Append('&quot;');
          if LInTag then
            begin
            if LInAttr then
              LInAttr := false
            else
             begin
             LInAttr := true;
             b.Append('<font color="Green">');
             end;
            end;
          end;
        '&':
          begin
          b.Append('<b>&amp;');
          LInAmp := true;
          end;
        ';':
          begin
          if LInAmp then
            b.Append(';</b>');
          LInAmp := false;
          end;
        '<':
          begin
          b.Append('<font color="navy">&lt;</font><font color="maroon">');
          LInTag := AStr[i+1] <> '!';
          end;
        '>':
          begin
          LInTag := false;
          b.Append('</font><font color="navy">&gt;</font>');
          end;
        #32:
          begin
          if LIsNewLine then
            b.Append('&nbsp;')
          else
            b.Append(' ');
          end;
        #13:
          begin
          if i < Length(AStr) then
            if AStr[i + 1] = #10 then
              Inc(i);
          b.Append('<br>');
          LIsNewLine := True;
          end;
        else
          begin
          if CharINSet(AStr[i], [' '..'~']) then
            b.Append(AStr[i])
          else
            b.Append('&#' + IntToStr(Ord(AStr[i])) + ';');
          end;
        end;
      Inc(i);
      end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function FormatXMLToHTMLPLain(AStr : String):String;
var
  LIsNewLine: Boolean;
  I: Integer;
  b : TFslStringBuilder;
begin
  Result := '';
  if Length(AStr) <= 0 then
    exit;
  b := TFslStringBuilder.Create;
  try
    LIsNewLine := True;
    i := 1;
    while i <= Length(AStr) do
      begin
      if (AStr[i] <> #32) and (LIsNewLine) then
        LIsNewLine := False;
      case AStr[i] of
        '''':
          b.Append('&#' + IntToStr(Ord(AStr[i])) + ';');
        '"':
          begin
          b.Append('&quot;');
          end;
        '&':
          begin
          b.Append('&amp;');
          end;
        ';':
          begin
          b.Append(';');
          end;
        '<':
          begin
          b.Append('&lt;');
          end;
        '>':
          begin
          b.Append('&gt;');
          end;
        #32:
          begin
          if LIsNewLine then
            b.Append('&nbsp;')
          else
            b.Append(' ');
          end;
        #13:
          begin
          if i < Length(AStr) then
            if AStr[i + 1] = #10 then
              Inc(i);
          b.Append('<br>');
          LIsNewLine := True;
          end;
        else
          begin
          if CharINSet(AStr[i], [' '..'~']) then
            b.Append(AStr[i])
          else
            b.Append('&#' + IntToStr(Ord(AStr[i])) + ';');
          end;
        end;
      Inc(i);
      end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

Function EncodeXML(Const sValue : String; bEoln : Boolean = True) : String;
Var
  iLoop : Integer;
  cValue : Char;
Begin
  Result := sValue;
  iLoop := 1;
  While iLoop <= Length(Result) Do
  Begin
    cValue := Result[iLoop];

    Case cValue Of
      #10, #13:
      If bEoln Then
        Begin
          Delete(Result, iLoop, 1);
          Insert('&#x' + string(EncodeHexadecimal(Ord(cValue))) + ';', Result, iLoop);
          Inc(iLoop, 5);
        End
      Else
        Inc(iLoop);

      #0..#9, #11..#12, #14..#31, #127..#255 :
      Begin
        Delete(Result, iLoop, 1);
        Insert('&#x' + string(EncodeHexadecimal(Ord(cValue))) + ';', Result, iLoop);
        Inc(iLoop, 5);
      End;

      '<':
      Begin
        Delete(Result, iLoop, 1);
        Insert('&lt;', Result, iLoop);
        Inc(iLoop, 4);
      End;

      '>':
      Begin
        Delete(Result, iLoop, 1);
        Insert('&gt;', Result, iLoop);
        Inc(iLoop, 4);
      End;

      '&':
      Begin
        // Preceding '&' already exists in string.
        Insert('amp;', Result, iLoop + 1);
        Inc(iLoop, 4);
      End;

      // Only need to encode &quot; and &apos; in XML attributes...
    Else
      Inc(iLoop);
    End;
  End;
End;

Function DecodeXML(Const sValue : String) : String;
Var
  iLoop : Integer;
  pValue : PChar;
  iValue : Word;
  sPrefixedEncodedDec : String;
  sRemainder : String;
  sEncodedDec : String;
  iEncodedDec : Integer;
Begin
  Result := sValue;
  iLoop := 1;
  While iLoop <= Length(Result) Do
  Begin
    pValue := @Result[iLoop];

    If pValue^ = '&' Then
    Begin
      If StringEquals(pValue, '&lt;', 4) Then
      Begin
        Delete(Result, iLoop, 4);
        Insert('<', Result, iLoop);
      End
      Else If StringEquals(pValue, '&gt;', 4) Then
      Begin
        Delete(Result, iLoop, 4);
        Insert('>', Result, iLoop);
      End
      Else If StringEquals(pValue, '&amp;', 5) Then
      Begin
        Delete(Result, iLoop, 5);
        Insert('&', Result, iLoop);
      End
      Else If StringEquals(pValue, '&quot;', 6) Then
      Begin
        Delete(Result, iLoop, 6);
        Insert('"', Result, iLoop);
      End
      Else If StringEquals(pValue, '&apos;', 6) Then
      Begin
        Delete(Result, iLoop, 6);
        Insert('''', Result, iLoop);
      End
      Else If StringEquals(pValue, '&#x', 3) Then
      Begin
        StringSplit(pValue, ';', sPrefixedEncodedDec, sRemainder);
        sEncodedDec := '0x'+copy(sPrefixedEncodedDec, 4, length(sPrefixedEncodedDec));
        iEncodedDec := StringToInteger32(sEncodedDec);
        if iEncodedDec > 65535 then
          iValue := Ord('?')
        else
          iValue := iEncodedDec;
        Delete(Result, iLoop, Length(sPrefixedEncodedDec) + 1);

        Insert(Char(iValue), Result, iLoop);
      End
      Else If StringEquals(pValue, '&#', 2) Then
      Begin
        StringSplit(pValue, ';', sPrefixedEncodedDec, sRemainder);
        sEncodedDec := StringTruncateStart(sPrefixedEncodedDec, 2);

        iEncodedDec := StringToInteger32(sEncodedDec);

        If (iEncodedDec >= 0) And (iEncodedDec <= 255) Then
        Begin
        if iEncodedDec > 65535 then
          iValue := Ord('?')
        else
          iValue := iEncodedDec;

          Delete(Result, iLoop, Length(sPrefixedEncodedDec) + 1); // eg. '&#13;' or '&#220;'
          Insert(Char(iValue), Result, iLoop);
        End;
      End;
    End;

    Inc(iLoop);
  End;
End;

function FormatJsonToHTML(AStr : String):String;
var
  inQuoted : Boolean;
  isNewLine : boolean;
  isValue : boolean;
  isList : boolean;
  b : TFslStringBuilder;
  i : integer;
begin
  inQuoted := false;
  isList := false;
  Result := '';
  if Length(AStr) <= 0 then
    exit;
  isValue := false;
  b := TFslStringBuilder.Create;
  try
    isNewLine := True;
    i := 1;
    while i <= Length(AStr) do
      begin
      if (AStr[i] <> #32) and (isNewLine) then
        isNewLine := False;
      case AStr[i] of
        '"':
          begin
          if inQuoted and not isValue then
            b.Append('</font>');
          b.Append('&quot;');
          if inQuoted then
            inQuoted := false
          else
           begin
           inQuoted := true;
           if not isValue then
             b.Append('<font color="Navy">');
           end;
          end;
        '&': b.Append('&amp;');
        '<': b.Append('&lt;');
        '>': b.Append('&gt;');
        '[':
          begin
          b.Append('<font color="maroon"><b>'+AStr[i]+'</b></font>');
          isList := true;
          end;
        ':':
          begin
          b.Append('<font color="maroon"><b>'+AStr[i]+'</b></font>');
          isValue := true;
          end;
        ',':
          begin
          b.Append(AStr[i]);
          if not isList then
            isValue := false;
          end;
        ']':
          begin
          b.Append('<font color="maroon"><b>'+AStr[i]+'</b></font>');
          isList := false;
          isValue := false;
          end;
        '}': b.Append('<font color="maroon"><b>'+AStr[i]+'</b></font>');
        '{' :
          begin
          b.Append('<font color="maroon"><b>'+AStr[i]+'</b></font>');
          isList := false; // we might need to stack this state if we start worrying about mixed lists
          isValue := false;
          end;
        #32: if isNewLine then b.Append('&nbsp;') else b.Append(' ');
        #13:
          begin
          if i < Length(AStr) then
            if AStr[i + 1] = #10 then
              Inc(i);
          b.Append('<br>');
          isNewLine := True;
          end;
        else
          begin
          if CharINSet(AStr[i], [' '..'~']) then
            b.Append(AStr[i])
          else
            b.Append('&#' + IntToStr(Ord(AStr[i])) + ';');
          end;
        end;
      Inc(i);
      end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

Const
  setUriUnreserved : Set Of AnsiChar = ['a'..'z', 'A'..'Z', '0'..'9', '-', '.', '_', '~'];


Function TFslCodeMask.Link: TFslCodeMask;
Begin
  Result := TFslCodeMask(Inherited Link);
End;


Procedure TFslCodeMask.SetFormat(Const Value: TFslCodeMaskFormat);
Var
  aSet : TFslCodeMaskAllow;
  iFormat : Integer;
  iMasks : Integer;
  cMask : Char;
Begin
  FFormat := Value;

  // Set the arrays to the maximum possible length.
  SetLength(FAllowed, Length(FFormat));
  SetLength(FFixedFormat, Length(FFormat));

  iMasks := 0;
  iFormat := 1;
  FFixedLength := 0;

  While (iFormat <= Length(FFormat)) Do
  Begin
    aSet := [];

    cMask := FFormat[iFormat];

    Case charUpper(cMask) Of
      '0', '9' : aSet := setNumbers;
      'A'      : aSet := setAlphanumeric;
      'C'      : aSet := setUniversal;
      'L'      : aSet := setAlphabet;
      'N'      : aSet := setUniversal - setAlphanumeric;
    Else
      If cMask = '\' Then
      Begin
        Inc(iFormat);

        If (iFormat <= Length(FFormat)) Then
          cMask := FFormat[iFormat];
      End;

      aSet := Translate(cMask);
    End;

    If FCapitals Then
      aSet := aSet - setLowerCase;

    FAllowed[iMasks] := aSet;

    If aSet = Translate(cMask) Then
    Begin
      FFixedFormat[iMasks + 1] := cMask;

      If FFixedLength = iMasks Then
        Inc(FFixedLength);
    End
    Else
    Begin
      FFixedFormat[iMasks + 1] := ' ';
    End;

    Inc(iMasks);
    Inc(iFormat);
  End;

  // Set the arrays to the actual length.
  SetLength(FAllowed, iMasks);
  SetLength(FFixedFormat, iMasks);
End;


Function TFslCodeMask.Allowed(iIndex: Integer): TFslCodeMaskAllow;
Begin
  If (iIndex >= 1) And (iIndex <= Length(FAllowed)) Then
    Result := FAllowed[iIndex - 1]
  Else
    Result := setUniversal;
End;


Function TFslCodeMask.AllowedAsText(iIndex: Integer): String;
Var
  aAllow : TFslCodeMaskAllow;
Begin
  aAllow := Allowed(iIndex);

  If aAllow = setUniversal Then
    Result := 'Any'
  Else If aAllow = [FixedFormat[iIndex]] Then
    Result := FixedFormat[iIndex]
  Else
  Begin
    Result := '';

    If setLowerCase * aAllow = setLowerCase Then
      StringAppend(Result, 'a-z', ', ');

    If setUpperCase * aAllow = setUpperCase Then
      StringAppend(Result, 'A-Z', ', ');

    If setNumbers * aAllow = setNumbers Then
      StringAppend(Result, '0-9', ', ');

    If setSpecials * aAllow = setSpecials Then
      StringAppend(Result, 'Symbols', ', ');

    If setWhitespace * aAllow = setWhitespace Then
      StringAppend(Result, 'Whitespace', ', ');
  End;
End;


Function TFslCodeMask.Fixed(iIndex: Integer): Boolean;
Begin
  // If there is only one available choice, then it is fixed.

  Result := (iIndex >= 1) And (iIndex <= Length(FAllowed)) And (iIndex <= Length(FFixedFormat)) And (FAllowed[iIndex - 1] = Translate(FFixedFormat[iIndex]));
End;


Function TFslCodeMask.LeadingFixedLength: Integer;
Begin
  Result := 0;
  While (Result < FixedLength) And Fixed(Result + 1) Do
    Inc(Result);
End;


Function TFslCodeMask.TrailingFixedLength(Const iPosition : Integer) : Integer;
Var
  iIndex : Integer;
Begin
  Result := 0;
  iIndex := iPosition + 1;

  While (iIndex < FixedLength) And Fixed(iIndex) Do
  Begin
    Inc(Result);
    Inc(iIndex);
  End;
End;


Function TFslCodeMask.LeadingFixedText : String;
Var
  iIndex : Integer;
Begin
  Result := '';

  iIndex := 1;
  While (iIndex <= FixedLength) And Fixed(iIndex) Do
  Begin
    Result := Result + FixedFormat[iIndex];
    Inc(iIndex);
  End;
End;


Function TFslCodeMask.Count: Integer;
Begin
  Result := FixedLength;
End;


Function TFslCodeMask.GetFixedFormat: TFslCodeMaskFormat;
Begin
  Result := FFixedFormat;

  If (FMaxLength > 0) And (Length(Result) < FMaxLength) Then
    Result := Result + StringMultiply(' ', MaxLength - Length(Result));
End;


Function TFslCodeMask.GetFixedCount: Integer;
Begin
  Result := Length(FFixedFormat); // NOTE: private field.
End;


Function TFslCodeMask.GetFixedLength: Integer;
Begin
  Result := Length(FixedFormat); // NOTE: public property.
End;


Function TFslCodeMask.FormatFromText(Const sText: String) : TFslCodeMaskFormat;
Var
  iLoop : Integer;
Begin
  Result := sText;
  For iLoop := Length(Result) DownTo 1 Do
    System.Insert('\', Result, iLoop);
End;


Function TFslCodeMask.Formatted(Const sText: String): String;
Var
  iText : Integer;
  iFormat : Integer;
Begin
  Result := '';
  iText := 1;

  For iFormat := 1 To Length(FFormat) Do
  Begin
    If iText <= Length(sText) Then
    Begin
      If Fixed(iFormat) Then
      Begin
        Result := Result + FixedFormat[iFormat];
      End
      Else
      Begin
        Result := Result + sText[iText];

        Inc(iText);
      End;
    End;
  End;

  Result := Result + Copy(sText, iText, MaxInt);
End;


Function TFslCodeMask.Unformatted(Const sText: String): String;
Var
  iText : Integer;
  iFormat : Integer;
Begin
  Result := '';
  iText := 1;

  For iFormat := 1 To Length(FFormat) Do
  Begin
    If (iText <= Length(sText)) Then
    Begin
      If Not Fixed(iFormat) Then
        Result := Result + sText[iText];

      Inc(iText);
    End;
  End;

  Result := Result + Copy(sText, iText, MaxInt);
End;


Function TFslCodeMask.Conforms(Const sValue: String): Boolean;
Var
  iLoop : Integer;
Begin
  Result := True;
  iLoop := 1;

  While (iLoop <= Length(sValue)) And Result Do
  Begin
    Result := CharInSet(sValue[iLoop], Allowed(iLoop));
    Inc(iLoop);
  End;
End;


Function TFslCodeMask.Translate(Const cSymbol: Char): TFslCodeMaskAllow;
Begin
  If FCaseSensitive Then
    Result := [cSymbol]
  Else
    Result := [charLower(cSymbol), charUpper(cSymbol)];
End;


Function EncodePercent(Const sValue : String) : String;
var
  b : TStringBuilder;
  c : char;
  i : integer;
begin
  b := TStringBuilder.create;
  try
    for c in sValue do
    begin
      if CharInSet(c, ['a'..'z', 'A'..'Z', '0'..'9']) then
        b.append(c)
      else
      begin
        i := ord(c);
        if i <= 255 then
        begin
          b.Append('%');
          b.append(inttostr(i));
        end
        else
          raise EFslException.Create('Not handled - non-ansi characters in URLs');
      end;
    end;
    result := b.toString;
  finally
    b.free;
  end;
end;

Function DecodePercent(Const sValue : String) : String;
var
  i : integer;
  b : TStringBuilder;
  c : char;
begin
  b := TStringBuilder.create;
  try
    i := 1;
    while i <= length(sValue) do
    begin
      c := sValue[i];
      if (c = '%') and (i <= length(sValue)-2) then
      begin
        b.append(chr(StrToInt(copy(sValue, i+1, 2))));
        inc(i, 2);
      end
      else
        b.append(c);
      inc(i);
    end;
    result := b.toString;
  finally
    b.free;
  end;
end;


procedure init;
begin
  SetLength(UnicodeWhitespaceArray, 26);

  UnicodeWhitespaceArray[0] := #$0009; //
  UnicodeWhitespaceArray[1] := #$000A; //
  UnicodeWhitespaceArray[2] := #$000B; //
  UnicodeWhitespaceArray[3] := #$000C; //
  UnicodeWhitespaceArray[4] := #$000D; //
  UnicodeWhitespaceArray[5] := #$0020; // SPACE
  UnicodeWhitespaceArray[6] := #$0085; // NEL (control character next line)
  UnicodeWhitespaceArray[7] := #$00A0; // NBSP (NO-BREAK SPACE)
  UnicodeWhitespaceArray[8] := #$1680; // OGHAM SPACE MARK
  UnicodeWhitespaceArray[9] := #$180E; // MONGOLIAN VOWEL SEPARATOR
  UnicodeWhitespaceArray[10] := #$2000; //
  UnicodeWhitespaceArray[11] := #$2001; //
  UnicodeWhitespaceArray[12] := #$2002; //
  UnicodeWhitespaceArray[13] := #$2003; //
  UnicodeWhitespaceArray[14] := #$2004; //
  UnicodeWhitespaceArray[15] := #$2005; //
  UnicodeWhitespaceArray[16] := #$2006; //
  UnicodeWhitespaceArray[17] := #$2007; //
  UnicodeWhitespaceArray[18] := #$2008; //
  UnicodeWhitespaceArray[19] := #$2009; //
  UnicodeWhitespaceArray[20] := #$200A; // different sorts of spaces)
  UnicodeWhitespaceArray[21] := #$2028; // LS (LINE SEPARATOR)
  UnicodeWhitespaceArray[22] := #$2029; // PS (PARAGRAPH SEPARATOR)
  UnicodeWhitespaceArray[23] := #$202F; // NNBSP (NARROW NO-BREAK SPACE)
  UnicodeWhitespaceArray[24] := #$205F; // MMSP (MEDIUM MATHEMATICAL SPACE)
  UnicodeWhitespaceArray[25] := #$3000; // IDEOGRAPHIC SPACE

{$IFOPT C+}
  CharArrayVerify(UnicodeWhitespaceArray, 'UnicodeWhitespaceArray');
{$ENDIF}

  Randomize;
  InitializeCriticalSection(gCriticalSection);
  {$IFOPT C+}
  gActiveMemoryTracking := True;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FillChar(gSystemInfo, SizeOf(gSystemInfo), 0);
  FillChar(gOSInfo, SizeOf(gOSInfo), 0);

  gOSInfo.dwOSVersionInfoSize := SizeOf(gOSInfo);

  GetVersionEx(gOSInfo);
  GetSystemInfo(gSystemInfo);

  If SystemIsWindows7 Then
  Begin
    // NOTE: Windows 7 changes the behaviour of GetThreadLocale.
    //       This is a workaround to force sysutils to use the correct locale.

    SetThreadLocale(GetUserDefaultLCID);
    SysUtils.GetFormatSettings;
  End;
  {$ENDIF}
end;

procedure fin;
begin
  {$IFOPT C+}
  EnterCriticalSection(gCriticalSection);
  Try
    Assert(gLiveMemorySize = 0, 'Memory leak of '+inttostr(gLiveMemorySize)+' bytes');
  Except
    // MessageBox(0, PChar(StringFormat('Memory has not been properly released (%d bytes). Please check your usage of Memory Create/Destroy/Resize routines.', [gLiveMemorySize])), 'MemorySupport', MB_OK);
  End;

  gActiveMemoryTracking := False;

  LeaveCriticalSection(gCriticalSection);

  {$ENDIF}
  DeleteCriticalSection(gCriticalSection);
end;

function clength(S : String) : cardinal;
begin
  result := cardinal(length(s));
end;

function clength(b : TBytes) : cardinal;
begin
  result := cardinal(length(b));
end;

Initialization
  init;
Finalization
  fin;
End.

