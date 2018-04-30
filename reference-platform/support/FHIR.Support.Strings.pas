Unit FHIR.Support.Strings;

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
  SysUtils, Classes, Character, SysConst, TypInfo, RegularExpressions,
  FHIR.Support.Math,
  FHIR.Support.Objects;


Type
  TCharArray = Array Of WideChar;
  TCharSet = Set Of AnsiChar;

  TLongString = String;
  PLongString = ^TLongString;

  TShortString = String[255];
  PShortString = ^TShortString;

{$IFDEF VER130}
  TBytes = Array Of Byte;
{$ENDIF}
  

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
Function StringFind(Const sValue : String; aFind : TCharSet) : Integer; Overload;
Function StringKeep(Const sValue : String; Const aFind : TCharSet) : String; Overload;
Function StringReplace(Const sValue, sFind, sReplace : String) : String; Overload;
Function StringReplace(Const sValue : String; Const aFind : TCharSet; cReplace : Char) : String; Overload;
Function StringReplace(Const sValue : String; Const aFind : TCharSet; Const sReplace : String) : String; Overload;
Function StringReplaceBefore(Const sValue, sFind, sReplace : String) : String; Overload;
Function StringReplaceAfter(Const sValue, sFind, sReplace : String) : String; Overload;
Function StringReplaceAll(Const sValue, sFind, sReplace : String; Const iStartPos : Integer = 1; Const iEndPos : Integer = 0) : String; Overload;
Function StringPadLeft(Const sValue : String; cPad : Char; iLength : Integer) : String; Overload;
Function StringPadRight(Const sValue : String; cPad : Char; iLength : Integer) : String; Overload;

Function StringTrimWhitespace(Const sValue : String) : String; Overload;
Function StringTrimWhitespaceRight(Const sValue : String) : String; Overload;
Function StringTrimWhitespaceLeft(Const sValue : String) : String; Overload;
Function StringTrimSetRight(Const sValue : String; aChars : TCharSet) : String; Overload;
Function StringTrimSetLeft(Const sValue : String; aChars : TCharSet) : String; Overload;
Function StringTrimSet(Const sValue : String; aChars : TCharSet) : String; Overload;

Function StringToBoolean(Const sValue : String) : Boolean; Overload;
Function StringToReal(Const sValue : String) : Real; Overload;

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


function capitalise(s : String) : String;
function jsonEscape(s : String; isString : boolean) : String;

function StringFindEndOfNumber(const s : String; index : integer) : integer;


Const
  OID_LOINC = '2.16.840.1.113883.6.1';
  OID_SNOMED = '2.16.840.1.113883.6.96';
  OID_REGEX = '[0-2](\.(0|[1-9][0-9]*))*';

Function isOid(oid : String) : Boolean;

function UriForKnownOid(oid : String) : String;

type
  TAnsiStringBuilder = Class(TFslObject)
    Private
      FContent : AnsiString;
      FLength : Integer;
      FBufferSize : integer;
    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

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

      Procedure Append(Const oStream : TFslStream; iBytes : Integer); Overload;
      Procedure Insert(Const oStream : TFslStream; iBytes : Integer; iIndex : Integer); Overload;
    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

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
  End;


  {$ELSE}
  TFslStringBuilder = Class (TFslObject)
    Private
      FBuilder : TStringBuilder;

      Function GetLength : Integer;
      function GetAsString: String;
    Public
      Constructor Create; Override;
      Destructor Destroy; Override;
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
  End;
  {$ENDIF}

type
  TCommaBuilder = class (TFslObject)
  private
    list : TStringList;
    FIgnoreDuplicates: Boolean;
  public
    Constructor Create;
    Destructor Destroy; override;

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
Function EncodeBase64(Const sValue : TBytes) : AnsiString; Overload;
Function DecodeBase64(Const sValue : AnsiString) : TBytes; Overload;
}

//Function EncodeXML(Const sValue : String; mode : TXmlEncodingMode; eoln : TEolnOption = eolnIgnore) : String; Overload;
//Function DecodeXML(Const sValue : String) : String; Overload;
Function EncodeQuotedString(Const sValue : String; Const cQuote : Char) : String; Overload;
Function EncodeMIME(Const sValue : String) : String; Overload;
Function DecodeMIME(Const sValue : String) : String; Overload;
Function DecodeMIMEURL(Const sValue : String) : String;  overload;
Function SizeOfDecodeHexadecimal(Const aBuffer; iSize : Cardinal) : Cardinal; Overload;
Function DecodeHexadecimal(Const cHigh, cLow : AnsiChar) : Byte; Overload;
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

Implementation


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
  Raise Exception.Create('(FHIR.Support.Strings.' + sConstant + '): ' + sMessage);
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
  b : TStringBuilder;
  s : String;
  first : boolean;
begin
  sl := aStr.Split([',']);
  b := TStringBuilder.Create;
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
      Result := floattostrF(i / 1024, ffFixed, 18, 2) + ' KB';
    1000001..1000000000:
      Result := floattostrF(i / 1048576, ffFixed, 18, 2) + ' MB';
  Else
    Result := floattostrF(i / 1073741824, ffFixed, 18, 2) + ' GB';
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
  Start,Base,Digit:Integer;
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
      raise Exception.Create(Format(RS_ERR_ENGINE_BAD_ENUM_TYPE, [ATypeInfo^.Name]))
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
    raise Exception.Create(Format(RS_ERR_ENGINE_NOT_ENUM_TYPE, [ATypeInfo^.Name]))
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
      raise Exception.Create(Format(RS_ERR_ENGINE_BAD_ENUM_TYPE, [ATypeInfo^.Name]))
      end;
    if AIndex > LTypeData^.MaxValue then
      begin
      raise Exception.Create(Format(RS_ERR_ENGINE_ENUM_OUT_RANGE, [IntToStr(AIndex), ATypeInfo^.Name]))
      end;
    LPChar := PAnsiChar(@LTypeData^.NameList[0]);
    for i := 1 to AIndex do
      begin
      inc(LPChar, Ord(LPChar^) + 1);  // move to next string
      end;
    Result := String(ShortString(pointer(LPChar)^));
    end
  else
    raise Exception.Create(Format(RS_ERR_ENGINE_NOT_ENUM_TYPE, [ATypeInfo^.Name]))
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
      raise Exception.Create(Format(RS_ERR_ENGINE_BAD_ENUM_TYPE, [ATypeInfo^.Name]))
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
      raise Exception.Create(Format(RS_ERR_ENGINE_ENUM_OUT_RANGE, [AStr, ATypeInfo^.Name]))
      end;
    end
  else
    begin
    raise Exception.Create(Format(RS_ERR_ENGINE_NOT_ENUM_TYPE, [ATypeInfo^.Name]))
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

function capitalise(s : String) : String;
begin
  if (s = '') then
    result := s
  else
    result := UpperCase(s[1]) + s.Substring(1);
end;

function jsonEscape(s : String; isString : boolean) : String;
var
  b : TStringBuilder;
  c : char;
begin
  if s = '' then
    exit('');
  b := TStringBuilder.Create;
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
    regex := TRegEx.Create(OID_REGEX, [roCompiled]);
    result := regex.IsMatch(oid);
  end;
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

Const
  BUFFER_INCREMENT_SIZE = 1024;


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


Procedure TFslStringBuilder.Append(Const oStream : TFslStream; iBytes : Integer);
Begin
  If (iBytes > 0) Then
  Begin
    If FLength + iBytes > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + Integermax(FBufferSize, iBytes));

    oStream.Read(FContent[FLength + 1], iBytes);

    Inc(FLength, iBytes);
  End;
End;


Procedure TFslStringBuilder.Insert(Const oStream : TFslStream; iBytes : Integer; iIndex : Integer);
Begin
  If (iBytes > 0) Then
  Begin
    If FLength + iBytes > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iBytes));

    If (iIndex) <> FLength Then
      Move(FContent[iIndex+1], FContent[iIndex+1 + iBytes], (FLength - iIndex) * SizeOf(Char));

    oStream.Read(FContent[iIndex + 1], iBytes);

    Inc(FLength, iBytes);
  End;
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
    Error('Overwrite', 'index < 1');
  if index + System.length(Content) > FLength Then
    Error('Overwrite', 'index > length');
  if content <> '' Then
    Move(Content[1], FContent[index], System.length(Content));
end;

procedure TFslStringBuilder.Read(index: integer; var buffer; ilength: integer);
begin
  if index < 1 Then
    Error('Read', 'index < 1');
  if index + length > FLength Then
    Error('Read', 'index > length');
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

  ENCODE_BASE64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  DECODE_BASE64 : Array[AnsiChar] Of Byte =
    (
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 062, 255, 255, 255, 063,
    052, 053, 054, 055, 056, 057, 058, 059, 060, 061, 255, 255, 255, 255, 255, 255,
    255, 000, 001, 002, 003, 004, 005, 006, 007, 008, 009, 010, 011, 012, 013, 014,
    015, 016, 017, 018, 019, 020, 021, 022, 023, 024, 025, 255, 255, 255, 255, 255,
    255, 026, 027, 028, 029, 030, 031, 032, 033, 034, 035, 036, 037, 038, 039, 040,
    041, 042, 043, 044, 045, 046, 047, 048, 049, 050, 051, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
    );

Function SizeOfEncodeBase64(Const aBuffer; iSize : Cardinal) : Cardinal; Overload;
Begin
  Result := (RealCeiling(iSize / 3)) * 4;
End;

Function SizeOfDecodeBase64(Const aBuffer; iSize : Cardinal) : Cardinal; Overload;
Begin
  Result := (RealCeiling(iSize / 4)) * 3;
End;

Function SizeOfDecodeBase64(Const sValue : AnsiString) : Cardinal; Overload;
Begin
  Result := SizeOfDecodeBase64(Pointer(sValue)^, Length(sValue));
End;

Function DecodeBase64(Const cValue : AnsiChar) : Byte; Overload;
Begin
  Result := DECODE_BASE64[cValue];
End;


Function DecodeBase64(Const aSource : AnsiString; Var aTarget; iCount : Cardinal) : Cardinal; Overload;
Var
  pSource  : PAnsiChar;
  iSource  : Cardinal;

  Function Decode : Byte;
  Begin
    Result := 255;

    While (Cardinal(pSource) < iSource) And (Result = 255) Do
    Begin
      Result := DecodeBase64(pSource^);
      Inc(pSource);
    End;
  End;

Var
  pTarget  : PByte;
  iTarget  : Cardinal;
  iData    : Integer;
Begin
  pSource := @aSource;
  pTarget := @aTarget;

  If iCount > 0 Then
  Begin
    iSource := Cardinal(@aSource) + iCount;
    iTarget := Cardinal(@aSource) + ((RealCeiling(iCount / 4) - 1) * 4);

    // Process all complete 4 byte elements.
    While (Cardinal(pSource) < iTarget) Do
    Begin
      iData := (Decode Shl 18) Or (Decode Shl 12) Or (Decode Shl 6) Or Decode;

      pTarget^ := Byte(iData Shr 16);
      Inc(pTarget);

      pTarget^ := Byte(iData Shr 8);
      Inc(pTarget);

      pTarget^ := Byte(iData);
      Inc(pTarget);
    End;

    If Cardinal(pSource) < iSource Then
    Begin
      // Read first byte and start of second byte.
      iData := (Decode Shl 18);
      iData := iData Or (Decode Shl 12);

      // Write first byte.
      pTarget^ := Byte(iData Shr 16);
      Inc(pTarget);

      If (Cardinal(pSource) < iSource) And (pSource^ <> '=') Then
      Begin
        // Read end of second and start of third byte.
        iData := iData Or (Decode Shl 6);

        // Write second byte.
        pTarget^ := Byte(iData Shr 8);
        Inc(pTarget);

        If (Cardinal(pSource) < iSource) And (pSource^ <> '=') Then
        Begin
          // Read end of third byte.
          iData := iData Or Decode;

          // Write third byte.
          pTarget^ := Byte(iData);
          Inc(pTarget);
        End;
      End;
    End;
  End;

  Result := Cardinal(pTarget) - Cardinal(@aTarget);
End;

Function DecodeBase64(Const sValue : AnsiString) : TBytes; Overload;
Begin
  SetLength(Result, SizeOfDecodeBase64(sValue));

  SetLength(Result, DecodeBase64(sValue, Pointer(Result)^, Length(sValue)));
End;

Function EncodeBase64(Const iValue : Byte) : AnsiChar; Overload;
Begin
  Result := AnsiChar(ENCODE_BASE64[iValue + 1]); // Array is 1-based.
End;


Function EncodeBase64(Const aSource; Var aTarget; iCount : Cardinal) : Cardinal; Overload;

  Function Flip(iValue : Integer) : Integer;
  Var
    pSource : PByte;
    pTarget : PByte;
  Begin
    pSource := PByte(@iValue);
    pTarget := PByte(Integer(@Result) + SizeOf(Result) - 1);

    pTarget^ := pSource^;
    Inc(pSource);
    Dec(pTarget);

    pTarget^ := pSource^;
    Inc(pSource);
    Dec(pTarget);

    pTarget^ := pSource^;
    Inc(pSource);
    Dec(pTarget);

    pTarget^ := pSource^;
  End;

Var
  pSource : ^Integer;
  pTarget : PAnsiChar;
  iSource : Integer;
  iLoop   : Integer;
  iTarget : Integer;
  iData   : Cardinal;
Begin
  pSource := @aSource;
  pTarget := @aTarget;

  iSource := (iCount Div 3);

  For iLoop := 0 To iSource - 1 Do
  Begin
    iData := Cardinal(Flip(pSource^));

    pTarget^ := EncodeBase64((iData Shr 26) And $3F);
    Inc(pTarget);

    pTarget^ := EncodeBase64((iData Shr 20) And $3F);
    Inc(pTarget);

    pTarget^ := EncodeBase64((iData Shr 14) And $3F);
    Inc(pTarget);

    pTarget^ := EncodeBase64((iData Shr 8) And $3F);
    Inc(pTarget);

    pSource := Pointer(NativeUInt(pSource) + 3);
  End;

  iTarget := SignedMod(iCount, 3);

  If iTarget >= 1 Then
  Begin
    iData := Cardinal(Flip(pSource^));

    If iTarget = 1 Then
      iData := iData And $FF000000
    Else If iTarget = 2 Then
      iData := iData And $FFFF0000;

    pTarget^ := EncodeBase64((iData Shr 26) And $3F);
    Inc(pTarget);

    pTarget^ := EncodeBase64((iData Shr 20) And $3F);
    Inc(pTarget);

    If iTarget >= 2 Then
      pTarget^ := EncodeBase64((iData Shr 14) And $3F)
    Else
      pTarget^ := '=';
    Inc(pTarget);

    pTarget^ := '=';
    Inc(pTarget);
  End;

  Result := Cardinal(pTarget) - Cardinal(@aTarget);
End;

Function EncodeBase64(Const sValue : TBytes) : AnsiString; Overload;
Begin
  SetLength(Result, SizeOfEncodeBase64(Pointer(sValue)^, Length(sValue)));

  SetLength(Result, EncodeBase64(Pointer(sValue)^, Pointer(Result)^, Length(sValue)));
End;

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


Initialization

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
End. // FHIR.Support.Strings //
