Unit FHIR.Support.Text;

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
  SysUtils, RTLConsts, Classes, EncdDecd, System.NetEncoding,
  FHIR.Support.Strings,
  FHIR.Support.Collections, FHIR.Support.Stream, FHIR.Support.Exceptions;

Type
  TCharArray = SysUtils.TCharArray;
  TFslCharacterSet = Class(TFslOrdinalSet)
    Private
      FDataSet : TCharSet;

      Function GetAsText: String;
      Procedure SetAsText(Const Value: String);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure AddRange(Const aFromValue, aToValue : Char);
      Procedure AddValue(Const aValue : Char);
      Function ContainsValue(Const aValue : Char) : Boolean;

      Property AsText : String Read GetAsText Write SetAsText;
      Property Data : TCharSet Read FDataSet Write FDataSet;
  End;


  TFslTextReader = class (TFslObject)
  public
    procedure Close; virtual; abstract;
    function Peek: Integer; virtual; abstract;
    function Read: Integer; overload; virtual; abstract;
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; virtual; abstract;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; virtual; abstract;
    function ReadLine: string; virtual; abstract;
    function ReadToEnd: string; virtual; abstract;

    Function ReadString(Var s : String; iLength : Integer) : Integer;
  end;

  TFslStringReader = class(TFslTextReader)
  private
    FContent : String;
    FCursor : integer;
  public
    constructor Create(content : String);
    function Peek: Integer; override;
    function Read: Integer; overload; override;
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; override;
    function ReadLine: string; override;
    function ReadToEnd: string; override;
    procedure Close; override;
  end;

  TFslStreamReader = class(TFslTextReader)
  private
    FBufferedData: TCharArray;
    FBufferStart : Integer;
    FBufferEnd : Integer;
    FBufferSize: Integer;
    FDetectBOM: Boolean;
    FNoDataInStream: Boolean;
    FSkipPreamble: Boolean;
    FStream: TFslStream;
    FCursor : Integer;
    FEncoding: TEncoding;
    FCheckEncoding : boolean;
    FClosed : boolean;
    function DoDetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
    function SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
    procedure FillBuffer(var Encoding: TEncoding);
    function GetEndOfStream: Boolean;
  protected
    Property Stream : TFslStream read FStream;
  public
    constructor Create(aStream: TFslStream); overload;
    constructor Create(aStream: TFslStream; DetectBOM: Boolean); overload;
    constructor Create(const Filename: string); overload;
    constructor Create(const Filename: string; DetectBOM: Boolean); overload;
    constructor Create(aStream: TFslStream; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0); overload;
    constructor Create(const Filename: string; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure DiscardBufferedData;
//    procedure OwnStream; inline;
    function Peek: Integer; override;
    function Read: Integer; overload; override;
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; override;
    function ReadLine: string; override;
    function ReadToEnd: string; override;
    property BaseStream: TFslStream read FStream;
    property CurrentEncoding: TEncoding read FEncoding;
    property EndOfStream: Boolean read GetEndOfStream;
  end;

  TFslFormatter = Class(TFslStreamAdapter)
    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

      Procedure SetStream(oStream : TFslStream); Override;

    Public
      Procedure Clear; Overload; Virtual;

      Procedure ProduceBytes(Const aBytes : TBytes); Overload; Virtual;
      Procedure Produce(Const sText: String); Overload; Virtual;
  End;

  EFslFormatter = Class(EFslStream);

  EFslExtractor = Class(EFslException);
  TFslExtractor = Class(TFslStreamAdapter)
    Protected

      Function ErrorClass : EFslExceptionClass; Overload; Override;

      Procedure SetStream(oStream : TFslStream); Override;

    Public
      Procedure Clear; Virtual;

      Function More : Boolean; Virtual;
  End;

  TFslTextFormatter = Class(TFslFormatter)
    Private
      FLevel : Integer;
      FHasWhitespace : Boolean;
      FWhitespaceCharacter : Char;
      FWhitespaceMultiple : Integer;
      {$IFNDEF VER130}
      FEncoding: TEncoding;
      {$ENDIF}

    Protected
      Function BeforeWhitespace : String;
      Function AfterWhitespace : String;

    Public
      Constructor Create; Override;

      Function Link : TFslTextFormatter;

      Procedure Clear; Override;

      Procedure ProduceNewLine; Virtual;
      Procedure ProduceLine(Const sValue : String);
      Procedure ProduceInline(Const sValue : String);

      Procedure ProduceFragment(Const sValue : String);

      Procedure LevelDown;
      Procedure LevelUp;

      Property HasWhitespace : Boolean Read FHasWhitespace Write FHasWhitespace;
      Property WhitespaceCharacter : Char Read FWhitespaceCharacter Write FWhitespaceCharacter;
      Property WhitespaceMultiple : Integer Read FWhitespaceMultiple Write FWhitespaceMultiple;
      Property Level : Integer Read FLevel;
      {$IFNDEF VER130}
      Property Encoding : TEncoding read FEncoding Write FEncoding;
      {$ENDIF}
  End;

  TFslTextFormatterClass = Class Of TFslTextFormatter;

  TFslTextExtractor = Class(TFslStreamReader)
    Private
      FLine : Integer;
      FCache : String;

      FBuilder : TFslStringBuilder;

    Protected
      Function ErrorClass : EFslExceptionClass; Override;

      Procedure CacheAdd(Const sValue : String); Virtual;
      Procedure CacheRemove(Const sValue : String); Virtual;

      Procedure RaiseError(Const sMethod, sMessage : String); Override;

    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Override;

      Function More : Boolean; Virtual;

      Function ConsumeLine : String;

      Procedure ProduceString(Const sToken : String);
      Procedure ProduceCharacter(Const cToken : Char);

      Function ConsumeCharacter : Char; Overload; Virtual;
      Function ConsumeString(Const sToken : String) : String;
      Function ConsumeCharacter(Const cToken : Char) : Char; Overload;
      Function ConsumeCharacterCount(Const iCharacterCount : Integer) : String;

      Function ConsumeWhileCharacter(Const cToken : Char) : String;
      Function ConsumeWhileCharacterSet(Const aTokenSet : TCharSet) : String; Overload;
      Function ConsumeWhileCharacterSet(Const oCharacterSet : TFslCharacterSet) : String; Overload;

      Function ConsumeUntilCharacter(Const cToken : Char) : String;
      Function ConsumeUntilCharacterSet(Const aTokenSet : TCharSet) : String;
      Function ConsumeUntilString(Const sToken : String) : String; Overload;
      Function ConsumeUntilString(Const aStringArray : Array Of String) : String; Overload;

      Function ConsumeRestStream : String;

      Function MatchString(Const sToken : String) : Boolean;
      Function MatchStringArray(Const aTokenSet : Array Of String) : Integer;

      Function NextCharacter : Char;

      Function CacheLength : Integer;
      Function StreamPosition : Int64;

      Property Line : Integer Read FLine Write FLine;
  End;

  EFslTextExtractor = Class(EFslExtractor);


  TFslCSVExtractor = Class(TFslTextExtractor)
    Private
      FSeparator : Char;
      FQuote : Char;
      FHasQuote : Boolean;

    Public
      Constructor Create; Override;

      Procedure ConsumeEntries(oEntries : TFslStringList); Overload; 
      Procedure ConsumeEntries; Overload;
      Function ConsumeEntry : String;
      Function MoreEntries : Boolean;

      Property Separator : Char Read FSeparator Write FSeparator;
      Property Quote : Char Read FQuote Write FQuote;
      Property HasQuote : Boolean Read FHasQuote Write FHasQuote;
  End;

  TFslCSVFormatter = Class(TFslTextFormatter)
    Private
      FSeparator : Char;
      FQuote : Char;
      FHasQuote : Boolean;
      FEmptyLine : Boolean;

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Procedure Clear; Override;

      Procedure ProduceEntryStringArray(Const aEntryStringArray : Array Of String);
      Procedure ProduceEntryStringList(oEntryStringList : TFslStringList);
      Procedure ProduceEntry(Const sEntry : String);
      Procedure ProduceSeparator;

      Procedure ProduceNewLine; Override;

      Property Separator : Char Read FSeparator Write FSeparator;
      Property Quote : Char Read FQuote Write FQuote;
      Property HasQuote : Boolean Read FHasQuote Write FHasQuote;
  End;

  TCSVWriter = class (TFslCSVFormatter)
  private
  public
    procedure cell(s : String); overload;
    procedure cell(b : boolean); overload;
    procedure cell(i : integer); overload;
    procedure line;
  end;

{$IFNDEF FPC}
  TGetCsvValues = reference to Procedure (csv : TCSVWriter);

procedure produceCsv(filename : String; headers : Array of String; values : TGetCsvValues);
{$ENDIF}


type
  TXmlEncodingMode = (xmlText, xmlAttribute, xmlCanonical);
  TEolnOption = (eolnIgnore, eolnCanonical, eolnEscape);

function FormatTextToHTML(AStr: String): String; // translate ready for use in HTML
function FormatTextToXML(AStr: String; mode : TXmlEncodingMode): String;
function FormatCodeToXML(AStr: String): String;
function FormatXMLToHTML(AStr : String):String;
function FormatXMLToHTMLPlain(AStr : String):String;
function FormatXMLForTextArea(AStr: String): String;

function StringToUTF8Stream(value : String) : TStream;
function UTF8StreamToString(value : TStream) : String; overload;
function UTF8StreamToString(value : TFslAccessStream) : String; overload;

function FileToString(filename : String; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
function StreamToString(stream : TStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String; overload;
function StreamToString(stream : TFslStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String; overload;
procedure StringToFile(content, filename : String; encoding : TEncoding);
procedure StringToStream(content: String; stream : TStream; encoding : TEncoding); overload;
procedure StringToStream(content: String; stream : TFslStream; encoding : TEncoding); overload;

procedure BytesToFile(bytes : TBytes; filename : String);
function FileToBytes(filename : String; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : TBytes;

procedure StreamToFile(stream : TStream; filename : String);
Function EncodeXML(Const sValue : String; bEoln : Boolean = True) : String; Overload;
Function DecodeXML(Const sValue : String) : String; Overload;
Function EncodePercent(Const sValue : String) : String; Overload;
Function DecodePercent(Const sValue : String) : String; Overload;

function EncodeBase64(const value : TBytes) : AnsiString; overload;
{$IFNDEF FPC}
Function DecodeBase64(Const value : AnsiString) : TBytes; Overload;
{$ENDIF}
Function DecodeBase64(Const value : String) : TBytes; Overload;


type
  TSourceLocation = record
    line, col : integer;
  end;

  TSourceLocationObject = class (TFslObject)
  public
    locationStart : TSourceLocation;
    locationEnd : TSourceLocation;
  end;

const
  MAP_ATTR_NAME = 'B88BF977DA9543B8A5915C84A70F03F7';

function minLoc(src1, src2 : TSourceLocation) : TSourceLocation;
function maxLoc(src1, src2 : TSourceLocation) : TSourceLocation;
function nullLoc : TSourceLocation;
function isNullLoc(src : TSourceLocation) : boolean;
function locLessOrEqual(src1, src2 : TSourceLocation) : boolean;
function locGreatorOrEqual(src1, src2 : TSourceLocation) : boolean;

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

Type
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

uses
  FHIR.Support.Math;

Constructor TFslCSVExtractor.Create;
Begin 
  Inherited;

  FSeparator := ',';
  FQuote := '"';
  FHasQuote := True;
End;  


Procedure TFslCSVExtractor.ConsumeEntries(oEntries : TFslStringList);
Var
  sEntry : String;
Begin
  If Assigned(oEntries) Then
    oEntries.Clear;

  // Consume all preceeding whitespace.
  ConsumeWhileCharacterSet(setControls + setVertical + setHorizontal);

  While MoreEntries Do
  Begin 
    sEntry := ConsumeEntry;

    If Assigned(oEntries) Then
      oEntries.Add(sEntry);
  End;  
End;  


Function TFslCSVExtractor.ConsumeEntry : String;
Var
  bMore : Boolean;
Begin
  // strip all leading whitespace.
  ConsumeWhileCharacterSet(setControls + setHorizontal);

  If More Then
  Begin 
    If Not FHasQuote Or (NextCharacter <> FQuote) Then
    Begin
      // If it doesn't start with a quote then the entry is ended by a new line or the separator character.

      Result := ConsumeUntilCharacterSet([FSeparator] + setVertical);
    End
    Else
    Begin
      // Otherwise, if it is quoted, the entry is ended only by a closing quote.
      // Double quotes within the entry are resolved to a single quote.

      ConsumeCharacter(FQuote);
                    
      Result := '';
      bMore := True;
      While bMore And More Do
      Begin 
        If NextCharacter = FQuote Then
        Begin 
          ConsumeCharacter(FQuote);

          bMore := More And (NextCharacter = FQuote);

          If bMore Then
            Result := Result + ConsumeCharacter
          Else
            ProduceString(FQuote);
        End
        Else
        Begin
          Result := Result + ConsumeCharacter;
        End;
      End;

      If More Then
        ConsumeCharacter(FQuote);
    End;  

    If More Then
    Begin
      // strip trailing whitespace.
      ConsumeWhileCharacterSet(setControls + setHorizontal - setVertical);

      If More And (NextCharacter = FSeparator) Then
      Begin
        // strip separator character.
        ConsumeCharacter(FSeparator);

        // strip trailing non-newline whitespace after separator.
        ConsumeWhileCharacterSet(setControls + setHorizontal - setVertical);
      End;  
    End;
  End
  Else
  Begin
    Result := '';
  End;
End;


Procedure TFslCSVExtractor.ConsumeEntries;
Begin 
  ConsumeEntries(Nil);
End;  


Function TFslCSVExtractor.MoreEntries : Boolean;
Begin 
  Result := More And Not CharInSet(NextCharacter, setVertical);
End;



Constructor TFslCSVFormatter.Create;
Begin
  Inherited;

  FSeparator := ',';
  FQuote := '"';
  FHasQuote := True;
  FEmptyLine := True;
End;


Destructor TFslCSVFormatter.Destroy;
Begin
  Inherited;
End;


Procedure TFslCSVFormatter.Clear;
Begin
  Inherited;

  FEmptyLine := True;
End;


Procedure TFslCSVFormatter.ProduceEntryStringList(oEntryStringList: TFslStringList);
Var
  iEntryIndex : Integer;
Begin
  For iEntryIndex := 0 To oEntryStringList.Count - 1 Do
    ProduceEntry(oEntryStringList[iEntryIndex]);
End;


Procedure TFslCSVFormatter.ProduceEntryStringArray(Const aEntryStringArray: Array Of String);
Var
  iEntryIndex : Integer;
Begin
  For iEntryIndex := Low(aEntryStringArray) To High(aEntryStringArray) Do
    ProduceEntry(aEntryStringArray[iEntryIndex]);
End;


Procedure TFslCSVFormatter.ProduceEntry(Const sEntry : String);
Begin
  If FEmptyLine Then
    FEmptyLine := False
  Else
    ProduceSeparator;

  if sEntry <> '' then
    If FHasQuote Then
      Produce(EncodeQuotedString(sEntry, FQuote))
    Else
      Produce(sEntry)
End;


Procedure TFslCSVFormatter.ProduceNewLine;
Begin
  Inherited;

  FEmptyLine := True;
End;


Procedure TFslCSVFormatter.ProduceSeparator;
Begin
  Produce(FSeparator);
End;


Constructor TFslTextFormatter.Create;
Begin
  Inherited;

  FHasWhitespace := True;
  FWhitespaceCharacter := ' ';
  FWhitespaceMultiple := 2;
  {$IFNDEF VER130}
  Encoding := SysUtils.TEncoding.UTF8;
  {$ENDIF}
End;


Function TFslTextFormatter.Link : TFslTextFormatter;
Begin
  Result := TFslTextFormatter(Inherited Link);
End;


Procedure TFslTextFormatter.Clear;
Begin 
  Inherited;

  FLevel := 0;
End;  


Function TFslTextFormatter.BeforeWhitespace : String;
Begin 
  // Multiply of the space character by FLevel * 2 is more efficient than Multiply of string '  ' by FLevel because it uses FillChar.

  If FHasWhitespace Then
    Result := StringMultiply(FWhitespaceCharacter, FLevel * FWhitespaceMultiple)
  Else
    Result := '';
End;  


Function TFslTextFormatter.AfterWhitespace : String;
Begin 
  If FHasWhitespace Then
    Result := cReturn
  Else
    Result := '';
End;


Procedure TFslTextFormatter.ProduceFragment(Const sValue: String);
Begin 
  Produce(sValue);
End;  


Procedure TFslTextFormatter.ProduceLine(Const sValue: String);
Begin
  Produce(BeforeWhitespace + sValue + AfterWhitespace);
End;


Procedure TFslTextFormatter.ProduceNewLine;
Begin 
  Produce(cReturn);
End;  


Procedure TFslTextFormatter.LevelDown;
Begin 
  Inc(FLevel);
End;  


Procedure TFslTextFormatter.LevelUp;
Begin 
  Dec(FLevel);
End;  


Procedure TFslTextFormatter.ProduceInline(Const sValue: String);
Begin
  Produce(sValue);
End;


Constructor TFslTextExtractor.Create;
Begin
  Inherited;

  FBuilder := TFslStringBuilder.Create;
End;


Destructor TFslTextExtractor.Destroy;
Begin
  FBuilder.Free;

  Inherited;
End;


Procedure TFslTextExtractor.CacheAdd(Const sValue: String);
Begin
  Inherited;

  Dec(FLine, StringCount(sValue, cEnter));
End;


Procedure TFslTextExtractor.CacheRemove(Const sValue: String);
Begin
  Inherited;

  Inc(FLine, StringCount(sValue, cEnter));
End;


Function TFslTextExtractor.ConsumeLine : String;
Begin
  Result := ConsumeUntilCharacterSet(setVertical);

  ConsumeWhileCharacterSet(setVertical);
End;


Procedure TFslTextExtractor.RaiseError(Const sMethod, sMessage: String);
Begin
  Inherited RaiseError(sMethod, StringFormat('Line %d: %s', [FLine, sMessage]));
End;


Function TFslTextExtractor.ErrorClass: EFslExceptionClass;
Begin
  Result := EFslTextExtractor;
End;



Procedure TFslTextExtractor.ProduceString(Const sToken : String);
Begin
  FCache := sToken + FCache;

  CacheAdd(sToken);
End;


Procedure TFslTextExtractor.ProduceCharacter(Const cToken : Char);
Begin
  ProduceString(cToken);
End;


Function TFslTextExtractor.MatchString(Const sToken: String): Boolean;
Var
  iCacheLength : Integer;
  iTokenLength : Integer;
  iReadSize : Integer;
  sNextString : String;
  Buffer: SysUtils.TCharArray;
Begin
  iTokenLength := Length(sToken);
  iCacheLength := Length(FCache);
  SetLength(Buffer, iTokenLength - iCacheLength);
  iReadSize := Read(Buffer, 0, iTokenLength - iCacheLength);

  If iReadSize > 0 Then
  Begin
    SetLength(Buffer, iReadSize);
    SetString(sNextString, pchar(Buffer), Length(Buffer));
    FCache := FCache + sNextString;
  End;

  Result := StringEquals(FCache, sToken, iTokenLength);
End;


Function TFslTextExtractor.MatchStringArray(Const aTokenSet: Array Of String): Integer;
Begin
  Result := High(aTokenSet);
  While (Result >= Low(aTokenSet)) And Not MatchString(aTokenSet[Result]) Do
    Dec(Result);
End;


Function TFslTextExtractor.NextCharacter : Char;
var
  Buffer: SysUtils.TCharArray;
Begin
  If Length(FCache) = 0 Then
  Begin
    SetLength(Buffer, 1);
    if Read(Buffer, 0, 1) = 1 Then
      result := Buffer[0]
    Else
      result := #0;
    FCache := Result;
  End
  Else
  Begin
    Result := FCache[1];
  End;
End;


Function TFslTextExtractor.ConsumeCharacter : Char;
Begin
  Result := NextCharacter;

  Delete(FCache, 1, 1);

  CacheRemove(Result);
End;


Function TFslTextExtractor.ConsumeCharacter(Const cToken : Char) : Char;

  Function ToCharacter(Const cChar : Char) : String;
  Begin
    If (cChar >= ' ') And (cChar <= #127) Then
      Result := cChar
    Else
      Result := '$' + inttohex(Word(cChar), 4);
  End;

Begin
  If Not StringEquals(cToken, NextCharacter) Then
    RaiseError('Consume(Char)', StringFormat('Expected token ''%s'' but found token ''%s''', [ToCharacter(cToken), ToCharacter(NextCharacter)]));

  Result := ConsumeCharacter;
End;


Function TFslTextExtractor.ConsumeString(Const sToken : String) : String;
Begin
  If Not MatchString(sToken) Then
    RaiseError('Consume(String)', StringFormat('Expected token ''%s'' but found token ''%s''', [sToken, Copy(FCache, 1, Length(sToken))]));

  Delete(FCache, 1, Length(sToken));

  CacheRemove(sToken);

  Result := sToken;
End;


Function TFslTextExtractor.ConsumeCharacterCount(Const iCharacterCount : Integer) : String;
Var
  iLoop : Integer;
Begin
  SetLength(Result, iCharacterCount);

  For iLoop := 1 To iCharacterCount Do
    Result[iLoop] := ConsumeCharacter;
End;


Function TFslTextExtractor.ConsumeUntilCharacterSet(Const aTokenSet: TCharSet): String;
Begin
  FBuilder.Clear;
  While More And Not CharInSet(NextCharacter, aTokenSet) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeUntilString(Const sToken: String): String;
Begin
  FBuilder.Clear;
  While More And Not MatchString(sToken) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeUntilString(Const aStringArray: Array Of String): String;
Begin
  FBuilder.Clear;
  While More And Not (MatchStringArray(aStringArray) >= 0) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeUntilCharacter(Const cToken : Char) : String;
Begin
  FBuilder.Clear;
  While More And (NextCharacter <> cToken) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeRestStream : String;
Begin
  FBuilder.Clear;
  While More Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeWhileCharacter(Const cToken : Char) : String;
Begin
  FBuilder.Clear;
  While More And (NextCharacter = cToken) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeWhileCharacterSet(Const aTokenSet : TCharSet) : String;
Begin
  FBuilder.Clear;
  While More And CharInSet(NextCharacter, aTokenSet) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.ConsumeWhileCharacterSet(Const oCharacterSet: TFslCharacterSet): String;
Begin
  FBuilder.Clear;
  While More And oCharacterSet.ContainsValue(NextCharacter) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TFslTextExtractor.CacheLength: Integer;
Begin
  Result := Length(FCache);
End;


Function TFslTextExtractor.StreamPosition: Int64;
Begin
  Assert(Invariants('StreamPosition', BaseStream, TFslAccessStream, 'Stream'));

  Result := TFslAccessStream(BaseStream).Position - CacheLength;
End;


Function TFslTextExtractor.More : Boolean;
Begin
  Result := Not Inherited EndOfStream Or (Length(FCache) > 0);
End;

Function TFslExtractor.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslExtractor;
End;


Procedure TFslExtractor.SetStream(oStream: TFslStream);
Begin
  Inherited;

  Clear;
End;


Procedure TFslExtractor.Clear;
Begin
End;


Function TFslExtractor.More: Boolean;
Begin
  Result := (Stream.Readable > 0);
End;



Function TFslFormatter.ErrorClass : EFslExceptionClass;
Begin 
  Result := EFslFormatter;
End;


Procedure TFslFormatter.Clear;
Begin
End;


Procedure TFslFormatter.ProduceBytes(Const aBytes : TBytes);
Begin
  Write(aBytes[0], Length(aBytes));
End;

Procedure TFslFormatter.Produce(Const sText: String);
{$IFDEF VER130}
Begin
  Write(Pointer(sText)^, Length(sText));
End;
{$ELSE}
Var
  Bytes : TBytes;
Begin
  Assert(CheckCondition(sText <> '', 'Produce', 'Text must not be empty.'));

  Bytes := SysUtils.TEncoding.UTF8.GetBytes(sText);

  Write(Bytes[0], Length(Bytes));
End;
{$ENDIF}


Procedure TFslFormatter.SetStream(oStream: TFslStream);
Begin
  Inherited;

  Clear;
End;


{ TFslStreamReader }

constructor TFslStreamReader.Create(aStream: TFslStream);
begin
  Create(aStream, TEncoding.UTF8, True);
end;

constructor TFslStreamReader.Create(aStream: TFslStream; DetectBOM: Boolean);
begin
  Create(aStream, TEncoding.UTF8, DetectBOM);
end;

constructor TFslStreamReader.Create(aStream: TFslStream; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0);
begin
  Create;

  if not Assigned(aStream) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Stream']); // DO NOT LOCALIZE
  if not Assigned(Encoding) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  FBufferSize := BufferSize;
  if FBufferSize = 0 then
    FBufferSize := aStream.Readable;
  if FBufferSize = 0 then
    FBufferSize := 128;
  SetLength(FBufferedData, BufferSize);
  FBufferEnd := 0;
  FBufferStart := 0;
  FClosed := false;

  FEncoding := Encoding;
  FNoDataInStream := False;
  FStream := aStream;
  FDetectBOM := DetectBOM;
  FSkipPreamble := not FDetectBOM;
  FCursor := 0;
end;

constructor TFslStreamReader.Create(const Filename: string; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0);
var
  oFile : TFslFile;
begin
  oFile := TFslFile.Create(FileName, fmOpenRead);
  Try
    Create(oFile.Link, Encoding, DetectBOM, BufferSize);
  Finally
    oFile.Free;
  End;
end;

constructor TFslStreamReader.Create(const Filename: string; DetectBOM: Boolean);
begin
  Create(Filename, TEncoding.UTF8, DetectBOM);
end;

constructor TFslStreamReader.Create(const Filename: string);
begin
  Create(Filename, TEncoding.UTF8, true);
end;

destructor TFslStreamReader.Destroy;
begin
  Close;
  inherited;
end;

procedure TFslStreamReader.Close;
begin
  FStream.Free;
  FStream := nil;

  DiscardBufferedData;
  FClosed := true;
end;

procedure TFslStreamReader.DiscardBufferedData;
begin
  if not FClosed then
  begin
    FBufferEnd := 0;
    FBufferStart := 0;
    FNoDataInStream := False;
  end;
end;

function TFslStreamReader.doDetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
var
  LEncoding: TEncoding;
begin
  // try to automatically detect the buffer encoding
  LEncoding := nil;
  Result := TEncoding.GetBufferEncoding(Buffer, LEncoding);

  // detected encoding points to Default and param Encoding requests some other
  // type of Encoding; set the Encoding param to UTF8 as it can also read ANSI (Default)
  if (LEncoding = TEncoding.Default) and (Encoding <> TEncoding.Default) then
    Encoding := TEncoding.UTF8
  else
    Encoding := LEncoding;

  FDetectBOM := False;
end;

procedure TFslStreamReader.FillBuffer(var Encoding: TEncoding);
const
  BufferPadding = 4;
var
  LString: string;
  LBuffer: TBytes;
  BytesRead: Integer;
  StartIndex: Integer;
  ByteBufLen: Integer;
  ok : boolean;
  tries : integer;
begin
  SetLength(LBuffer, FBufferSize + BufferPadding);

  // Read data from stream
  BytesRead := IntegerMin(FBufferSize, FStream.Readable);
  FStream.Read(LBuffer[0], BytesRead);
  inc(FCursor, BytesRead);
  FNoDataInStream := FStream.Readable = 0;

  // Check for byte order mark and calc start index for character data
  if FDetectBOM then
    StartIndex := doDetectBOM(Encoding, LBuffer)
  else if FSkipPreamble then
    StartIndex := SkipPreamble(Encoding, LBuffer)
  else
    StartIndex := 0;

  // Convert to string and calc byte count for the string
  ByteBufLen := BytesRead - StartIndex;
  ok := false;
  tries := 0;
  repeat
    try
      if FCheckEncoding and (FEncoding.GetCharCount(LBuffer, StartIndex, ByteBufLen) = 0) then
      begin
        SetLength(LBuffer, Length(LBuffer) + BufferPadding);
        FStream.Read(LBuffer[ByteBufLen], 1);
        inc(ByteBufLen);
        inc(tries);
      end;

      LString := FEncoding.GetString(LBuffer, StartIndex, ByteBufLen);
      ok := true;
    except
      if FCheckEncoding and (tries > FEncoding.GetMaxByteCount(1)) then
        raise
      else
        FCheckEncoding := true;
    end;
  until ok;

  if (Length(LString) > 0) then
  begin
    // Add string to character data buffer
    if (FBufferStart > 0) and (FBufferEnd = FBufferStart) then
    begin
      FBufferStart := 0;
      FBufferEnd := 0;
    end;
    if Length(FBufferedData) < FBufferEnd + length(LString) then
      SetLength(FBufferedData, Length(FBufferedData) + length(LString) * 2);

    Move(LString[1], FBufferedData[FBufferEnd], length(LString) * SizeOf(Char));
    inc(FBufferEnd, length(LString));
  end;
end;

function TFslStreamReader.GetEndOfStream: Boolean;
begin
  if not FNoDataInStream and (not FClosed) and (FBufferEnd <= FBufferStart) then
    FillBuffer(FEncoding);
  Result := FNoDataInStream and ((FClosed) or (FBufferEnd = FBufferStart));
end;

function TFslStreamReader.Peek: Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    if FBufferEnd < 1 + FBufferStart  then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData[FBufferStart]);
  end;
end;

function TFslStreamReader.Read(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    while (FBufferEnd < Count + FBufferStart) and (not EndOfStream) and (not FNoDataInStream) do
      FillBuffer(FEncoding);

    if FBufferEnd > Count + FBufferStart then
      Result := Count
    else
      Result := FBufferEnd - FBufferStart;

    move(FBufferedData[FBufferStart], buffer[0], result * Sizeof(char));
    inc(FBufferStart, result);
  end;
end;

function TFslStreamReader.ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  Result := Read(Buffer, Index, Count);
end;

function TFslStreamReader.Read: Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    if FBufferEnd < 1 + FBufferStart then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData[FBufferStart]);
    inc(FBufferStart);
  end;
end;

function TFslStreamReader.ReadLine: string;
{var
  NewLineIndex: Integer;
  PostNewLineIndex: Integer;}
begin
  raise Exception.Create('This needs debugging for buffer changes');
{  Result := '';
  if FClosed then
    Exit;
  NewLineIndex := 0;
  PostNewLineIndex := 0;

  while True do
  begin
    if (NewLineIndex + 2 > FBufferEnd + FBufferStart) and (not FNoDataInStream) then
      FillBuffer(FEncoding);

    if NewLineIndex >= FBufferEnd + FBufferStart then
    begin
      if FNoDataInStream then
      begin
        PostNewLineIndex := NewLineIndex;
        Break;
      end
      else
      begin
        FillBuffer(FEncoding);
        if FBufferEnd = FBufferStart then
          Break;
      end;
    end;
    if FBufferedData[NewLineIndex + FBufferStart] = #10 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end
    else
    if (FBufferedData[NewLineIndex + FBufferStart] = #13) and (NewLineIndex + 1 < FBufferEnd + FBufferStart) and (FBufferedData[NewLineIndex + 1] = #10) then
    begin
      PostNewLineIndex := NewLineIndex + 2;
      Break;
    end
    else
    if FBufferedData[NewLineIndex + FBufferStart] = #13 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end;

    Inc(NewLineIndex);
  end;

  Result := FBufferedData.ToString.Substring(FBufferStart);
  SetLength(Result, NewLineIndex);
  inc(FBufferStart, PostNewLineIndex);}
end;

function TFslStreamReader.ReadToEnd: string;
begin
  raise Exception.Create('This needs debugging for FBufferStart');
  Result := '';
  if (not FClosed) and (not EndOfStream) then
  begin
    repeat
      FillBuffer(FEncoding);
    until FNoDataInStream;
    SetLength(result, FBufferEnd - FBufferStart);
    Move(FBufferedData[FBufferStart], result[1], length(result) * Sizeof(Char));
    FBufferEnd := 0;
    FBufferStart := 0;
  end;
end;

function TFslStreamReader.SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
var
  I: Integer;
  LPreamble: TBytes;
  BOMPresent: Boolean;
begin
  Result := 0;
  LPreamble := Encoding.GetPreamble;
  if (Length(LPreamble) > 0) then
  begin
    if Length(Buffer) >= Length(LPreamble) then
    begin
      BOMPresent := True;
      for I := 0 to Length(LPreamble) - 1 do
        if LPreamble[I] <> Buffer[I] then
        begin
          BOMPresent := False;
          Break;
        end;
      if BOMPresent then
        Result := Length(LPreamble);
    end;
  end;
  FSkipPreamble := False;
end;


{ TFslTextReader }

function TFslTextReader.ReadString(var s: String; iLength: Integer): Integer;
var
  oBuffer : TCharArray;
begin
  SetLength(oBuffer, iLength);
  result := ReadBlock(oBuffer, 0, iLength);
  SetString(s, pchar(oBuffer), result);
end;

{ TFslStringReader }

procedure TFslStringReader.Close;
begin
 // nothing
end;

constructor TFslStringReader.Create(content: String);
begin
  inherited Create;
  FContent := content;
  FCursor := 1;
end;

function TFslStringReader.Peek: Integer;
begin
  if FCursor > FContent.Length then
    result := -1
  else
    result := ord(FContent[FCursor]);
end;

function TFslStringReader.Read: Integer;
begin
  if FCursor > FContent.Length then
    result := -1
  else
  begin
    result := ord(FContent[FCursor]);
    inc(FCursor);
  end;
end;

function TFslStringReader.Read(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  raise Exception.Create('Not done yet');
end;

function TFslStringReader.ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  raise Exception.Create('Not done yet');
end;

function TFslStringReader.ReadLine: string;
begin
  raise Exception.Create('Not done yet');
end;

function TFslStringReader.ReadToEnd: string;
begin
  raise Exception.Create('Not done yet');
end;


Constructor TFslCharacterSet.Create;
Begin
  Inherited;

  Owns := False;
  Parts := @FDataSet;
  Size := SizeOf(FDataSet);
End;


Destructor TFslCharacterSet.Destroy;
Begin
  Inherited;
End;


Procedure TFslCharacterSet.AddRange(Const aFromValue, aToValue: Char);
Begin
  FDataSet := FDataSet + [aFromValue..aToValue];
End;


Procedure TFslCharacterSet.AddValue(Const aValue: Char);
Begin
  FDataSet := FDataSet + [aValue];
End;


Function TFslCharacterSet.ContainsValue(Const aValue: Char): Boolean;
Begin
  Result := CharInSet(aValue, FDataSet);
End;


Function TFslCharacterSet.GetAsText : String;
Var
  iLoop : Integer;
  iStart : Integer;
Begin
  iLoop := 0;
  Result := '';

  While (iLoop < Count) Do
  Begin
    iStart := iLoop;
    While (iLoop < Count) And Checked(iLoop) Do
      Inc(iLoop);

    If iLoop = iStart + 1 Then
      StringAppend(Result, Char(iStart), ', ')
    Else If iLoop > iStart + 1 Then
      StringAppend(Result, Char(iStart) + '-' + Char(iLoop - 1), ', ');

    Inc(iLoop);
  End;
End;


Procedure TFslCharacterSet.SetAsText(Const Value: String);
Var
  oStrings : TFslStringList;
  iLoop    : Integer;
  sField   : String;
  sLeft    : String;
  sRight   : String;
Begin
  Fill(False);

  oStrings := TFslStringList.Create;
  Try
    oStrings.Symbol := ',';

    oStrings.AsText := Value;

    For iLoop := 0 To oStrings.Count - 1 Do
    Begin
      sField := StringTrimWhitespace(oStrings[iLoop]);

      If sField <> '' Then
      Begin
        If Length(sField) = 1 Then
          Check(Ord(sField[1]))
        Else If StringSplit(sField, '-', sLeft, sRight) And (Length(sLeft) = 1) And (Length(sRight) = 1) Then
          CheckRange(Ord(sLeft[1]), Ord(sRight[1]));
      End;
    End;
  Finally
    oStrings.Free;
  End;
End;

{$IFNDEF FPC}
procedure produceCsv(filename : String; headers : Array of String; values : TGetCsvValues);
var
  csv : TCSVWriter;
  arr : TArray<String>;
  s : String;
begin
  csv := TCSVWriter.Create;
  try
    csv.Stream := TFslFile.Create(filename, fmCreate);
    for s in headers do
      csv.cell(s);
    csv.line;
    values(csv);
  finally
    csv.Free;
  end;
end;
{$ENDIF}

{ TCSVWriter }

procedure TCSVWriter.cell(s: String);
begin
  HasQuote := (s.Contains('"') or s.Contains(','));
  ProduceEntry(s);
end;

procedure TCSVWriter.cell(b: boolean);
begin
  if (b) then
    ProduceEntry('true')
  else
    ProduceEntry('false');
end;

procedure TCSVWriter.cell(i: integer);
begin
  cell(inttostr(i));
end;

procedure TCSVWriter.line;
begin
  ProduceNewLine;
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

procedure StringToFile(content, filename : String; encoding : TEncoding);
var
  LFileStream: TFilestream;
  bytes : TBytes;
begin
  LFileStream := TFileStream.Create(filename, fmCreate);
  try
    bytes := encoding.GetBytes(content);
    LFileStream.write(bytes[0], length(bytes));
  finally
    LFileStream.Free;
  end;
end;

procedure StringToStream(content: String; stream : TStream; encoding : TEncoding);
var
  bytes : TBytes;
begin
  bytes := encoding.GetBytes(content);
  if (length(bytes) > 0) then
    stream.write(bytes[0], length(bytes));
end;

procedure StringToStream(content: String; stream : TFslStream; encoding : TEncoding);
var
  bytes : TBytes;
begin
  bytes := encoding.GetBytes(content);
  if (length(bytes) > 0) then
    stream.write(bytes[0], length(bytes));
end;

function FileToString(filename : String; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
var
  LFileStream: TFilestream;
  bytes : TBytes;
begin
  if FileExists(filename) then
    begin
    LFileStream := TFileStream.Create(filename, aShareMode);
    try
      SetLength(bytes, LFileStream.Size);
      if LFileStream.Size > 0 then
        LFileStream.Read(bytes[0], LFileStream.size);
    finally
      LFileStream.Free;
    end;
      result := encoding.GetString(bytes);
    end
  else
    raise Exception.Create('File "' + filename + '" not found');
end;

function StreamToString(stream : TStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
var
  bytes : TBytes;
begin
  SetLength(bytes, stream.Size);
  if stream.Size > 0 then
    stream.Read(bytes[0], stream.size);
  result := encoding.GetString(bytes);
end;

function StreamToString(stream : TFslStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
var
  bytes : TBytes;
begin
  SetLength(bytes, stream.Readable);
  if stream.Readable > 0 then
    stream.Read(bytes[0], stream.Readable);
  result := encoding.GetString(bytes);
end;

function StringToUTF8Stream(value : String):TStream;
begin
  result := TBytesStream.Create(TEncoding.UTF8.GetBytes(value));
end;

function UTF8StreamToString(value : TStream) : String;
var
  b : TBytes;
begin
  SetLength(b, value.Size);
  if (value.Size > 0) then
    value.Read(b[0], value.Size);
  result := TEncoding.UTF8.GetString(b);
end;

function UTF8StreamToString(value : TFslAccessStream) : String;
var
  b : TBytes;
begin
  SetLength(b, value.Size);
  if (value.Size > 0) then
    value.Read(b[0], value.Size);
  result := TEncoding.UTF8.GetString(b);
end;

procedure BytesToFile(bytes : TBytes; filename : String);
var
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmCreate);
  try
    if length(bytes) > 0 then
      f.Write(bytes[0], length(bytes));
  finally
    f.Free;
  end;
end;

procedure StreamToFile(stream : TStream; filename : String);
var
  f : TFileStream;
  i : integer;
begin
  f := TFileStream.Create(filename, fmCreate);
  try
    i := stream.Position;
    f.CopyFrom(stream, stream.Size - stream.Position);
    stream.Position := i;
  finally
    f.Free;
  end;
end;

function FileToBytes(filename : String; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : TBytes;
var
  LFileStream: TFilestream;
begin
  if FileExists(filename) then
    begin
    LFileStream := TFileStream.Create(filename, aShareMode);
    try
      SetLength(result, LFileStream.Size);
      if LFileStream.Size > 0 then
        LFileStream.Read(result[0], LFileStream.size);
    finally
      LFileStream.Free;
    end;
    end
  else
    raise Exception.Create('File "' + filename + '" not found');
end;


function minLoc(src1, src2 : TSourceLocation) : TSourceLocation;
begin
  if (src1.line < src2.line) then
    result := src1
  else if (src2.line < src1.line) then
    result := src2
  else if (src1.col < src2.col) then
    result := src1
  else
    result := src2
end;

function maxLoc(src1, src2 : TSourceLocation) : TSourceLocation;
begin
  if (src1.line > src2.line) then
    result := src1
  else if (src2.line > src1.line) then
    result := src2
  else if (src1.col > src2.col) then
    result := src1
  else
    result := src2
end;

function nullLoc : TSourceLocation;
begin
  result.line := -1;
  result.col := -1;
end;


function isNullLoc(src : TSourceLocation) : boolean;
begin
  result := (src.line = -1) and (src.col = -1);
end;

function locLessOrEqual(src1, src2 : TSourceLocation) : boolean;
begin
  if src1.line < src2.line then
    result := true
  else if src1.line > src2.line then
    result := false
  else
    result := src1.col <= src2.col;
end;

function locGreatorOrEqual(src1, src2 : TSourceLocation) : boolean;
begin
  if src1.line > src2.line then
    result := true
  else if src1.line < src2.line then
    result := false
  else
    result := src1.col >= src2.col;
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
          Insert('&#x' + EncodeHexadecimal(Ord(cValue)) + ';', Result, iLoop);
          Inc(iLoop, 5);
        End
      Else
        Inc(iLoop);

      #0..#9, #11..#12, #14..#31, #127..#255 :
      Begin
        Delete(Result, iLoop, 1);
        Insert('&#x' + EncodeHexadecimal(Ord(cValue)) + ';', Result, iLoop);
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
  {$IFDEF VER130}
  iValue : Byte;
  {$ELSE}
  iValue : Word;
  {$ENDIF}
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
        if iEncodedDec > {$IFDEF VER130} 255 {$ELSE} 65535 {$ENDIF} then
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
        if iEncodedDec > {$IFDEF VER130} 255 {$ELSE} 65535 {$ENDIF} then
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

Const
  setUriUnreserved : Set Of AnsiChar = ['a'..'z', 'A'..'Z', '0'..'9', '-', '.', '_', '~'];


Function EncodePercent(Const aBuffer; iSize : Integer) : AnsiString; overload;
Var
  iSource, iDest : Integer;
  pSource : PAnsiChar;
Begin
  SetLength(Result, iSize);

  pSource := PAnsiChar(@aBuffer);
  iSource := 0;
  iDest := 1;

  While iSource < iSize Do
  Begin
    If CharInSet(pSource^, setUriUnreserved) Then
    Begin
      Result[iDest] := pSource^;
      Inc(iDest);
    End
    Else
    Begin
      Result[iDest] := '%';
      Inc(iDest);

      System.Insert(EncodeHexadecimal(Byte(pSource^)), Result, iDest);
      Inc(iDest, 2);
    End;

    Inc(pSource);
    Inc(iSource);
  End;
End;


Function DecodePercent(Const sValue : AnsiString; Const aBuffer; iSize : Integer) : Integer; Overload;
Var
  iSourceSize : Integer;
  pDest : PAnsiChar;
  iSource, iDest : Integer;
  cHi, cLo : AnsiChar;
Begin
  iSourceSize := Length(sValue);
  pDest := PAnsiChar(@aBuffer);
  iSource := 1;
  iDest := 0;

  While (iSource <= iSourceSize) And (iDest < iSize) Do
  Begin
    If sValue[iSource] = '%' Then
    Begin
      cHi := #0;
      cLo := #0;

      Inc(iSource);

      If iSource <= iSourceSize Then
      Begin
        cHi := sValue[iSource];
        Inc(iSource);
      End;

      If iSource <= iSourceSize Then
      Begin
        cLo := sValue[iSource];
        Inc(iSource);
      End;

      If (cHi <> #0) And (cLo <> #0) Then
      Begin
        pDest^ := AnsiChar(DecodeHexadecimal(cHi, cLo));
        Inc(pDest);
        Inc(iDest);
      End;
    End
    Else
    Begin
      pDest^ := sValue[iSource];
      Inc(iSource);
      Inc(pDest);
      Inc(iDest);
    End;
  End;

  Result := iDest;
End;

Function EncodePercent(Const sValue : String) : String; overload;
Begin
  If Length(sValue) > 0 Then
    Result := EncodePercent(sValue[1], Length(sValue))
  Else
    Result := '';
End;


Function DecodePercent(Const sValue : String) : String; overload;
Var
  iSize : Integer;
Begin
  Result := '';

  If Length(sValue) > 0 Then
  Begin
    SetLength(Result, Length(sValue));
    iSize := DecodePercent(sValue, Result[1], Length(Result));
    SetLength(Result, iSize);
  End;
End;

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

End.

