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
  SysUtils, RTLConsts, Classes,
  FHIR.Support.Strings,
  FHIR.Support.Collections, FHIR.Support.Stream, FHIR.Support.Exceptions;

Type
  TCharArray = SysUtils.TCharArray;
  TAdvCharacterSet = Class(TAdvOrdinalSet)
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


  TAdvTextReader = class (TAdvObject)
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

  TAdvStringReader = class(TAdvTextReader)
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

  TAdvStreamReader = class(TAdvTextReader)
  private
    FBufferedData: TCharArray;
    FBufferStart : Integer;
    FBufferEnd : Integer;
    FBufferSize: Integer;
    FDetectBOM: Boolean;
    FNoDataInStream: Boolean;
    FSkipPreamble: Boolean;
    FStream: TAdvStream;
    FCursor : Integer;
    FEncoding: TEncoding;
    FCheckEncoding : boolean;
    FClosed : boolean;
    function DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
    function SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
    procedure FillBuffer(var Encoding: TEncoding);
    function GetEndOfStream: Boolean;
  protected
    Property Stream : TAdvStream read FStream;
  public
    constructor Create(aStream: TAdvStream); overload;
    constructor Create(aStream: TAdvStream; DetectBOM: Boolean); overload;
    constructor Create(const Filename: string); overload;
    constructor Create(const Filename: string; DetectBOM: Boolean); overload;
    constructor Create(aStream: TAdvStream; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0); overload;
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
    property BaseStream: TAdvStream read FStream;
    property CurrentEncoding: TEncoding read FEncoding;
    property EndOfStream: Boolean read GetEndOfStream;
  end;

  TAdvFormatter = Class(TAdvStreamAdapter)
    Protected
      Function ErrorClass : EAdvExceptionClass; Overload; Override;

      Procedure SetStream(oStream : TAdvStream); Override;

    Public
      Procedure Clear; Overload; Virtual;

      Procedure ProduceBytes(Const aBytes : TBytes); Overload; Virtual;
      Procedure Produce(Const sText: String); Overload; Virtual;
  End;

  EAdvFormatter = Class(EAdvStream);

  EAdvExtractor = Class(EAdvException);
  TAdvExtractor = Class(TAdvStreamAdapter)
    Protected

      Function ErrorClass : EAdvExceptionClass; Overload; Override;

      Procedure SetStream(oStream : TAdvStream); Override;

    Public
      Procedure Clear; Virtual;

      Function More : Boolean; Virtual;
  End;

  TAdvTextFormatter = Class(TAdvFormatter)
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

      Function Link : TAdvTextFormatter;

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

  TAdvTextFormatterClass = Class Of TAdvTextFormatter;

  TAdvTextExtractor = Class(TAdvStreamReader)
    Private
      FLine : Integer;
      FCache : String;

      FBuilder : TAdvStringBuilder;

    Protected
      Function ErrorClass : EAdvExceptionClass; Override;

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
      Function ConsumeWhileCharacterSet(Const oCharacterSet : TAdvCharacterSet) : String; Overload;

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

  EAdvTextExtractor = Class(EAdvExtractor);


  TAdvCSVExtractor = Class(TAdvTextExtractor)
    Private
      FSeparator : Char;
      FQuote : Char;
      FHasQuote : Boolean;

    Public
      Constructor Create; Override;

      Procedure ConsumeEntries(oEntries : TAdvStringList); Overload; 
      Procedure ConsumeEntries; Overload;
      Function ConsumeEntry : String;
      Function MoreEntries : Boolean;

      Property Separator : Char Read FSeparator Write FSeparator;
      Property Quote : Char Read FQuote Write FQuote;
      Property HasQuote : Boolean Read FHasQuote Write FHasQuote;
  End;

  TAdvCSVFormatter = Class(TAdvTextFormatter)
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
      Procedure ProduceEntryStringList(oEntryStringList : TAdvStringList);
      Procedure ProduceEntry(Const sEntry : String);
      Procedure ProduceSeparator;

      Procedure ProduceNewLine; Override;

      Property Separator : Char Read FSeparator Write FSeparator;
      Property Quote : Char Read FQuote Write FQuote;
      Property HasQuote : Boolean Read FHasQuote Write FHasQuote;
  End;

  TCSVWriter = class (TAdvCSVFormatter)
  private
  public
    procedure cell(s : String); overload;
    procedure cell(b : boolean); overload;
    procedure cell(i : integer); overload;
    procedure line;
  end;

  TGetCsvValues = reference to Procedure (csv : TCSVWriter);

procedure produceCsv(filename : String; headers : Array of String; values : TGetCsvValues);


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
function UTF8StreamToString(value : TAdvAccessStream) : String; overload;

function FileToString(filename : String; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
function StreamToString(stream : TStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String; overload;
function StreamToString(stream : TAdvStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String; overload;
procedure StringToFile(content, filename : String; encoding : TEncoding);
procedure StringToStream(content: String; stream : TStream; encoding : TEncoding); overload;
procedure StringToStream(content: String; stream : TAdvStream; encoding : TEncoding); overload;

procedure BytesToFile(bytes : TBytes; filename : String);
function FileToBytes(filename : String; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : TBytes;

procedure StreamToFile(stream : TStream; filename : String);


type
  TSourceLocation = record
    line, col : integer;
  end;

  TSourceLocationObject = class (TAdvObject)
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

Implementation

uses
  FHIR.Support.Math;

Constructor TAdvCSVExtractor.Create;
Begin 
  Inherited;

  FSeparator := ',';
  FQuote := '"';
  FHasQuote := True;
End;  


Procedure TAdvCSVExtractor.ConsumeEntries(oEntries : TAdvStringList);
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


Function TAdvCSVExtractor.ConsumeEntry : String;
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


Procedure TAdvCSVExtractor.ConsumeEntries;
Begin 
  ConsumeEntries(Nil);
End;  


Function TAdvCSVExtractor.MoreEntries : Boolean;
Begin 
  Result := More And Not CharInSet(NextCharacter, setVertical);
End;



Constructor TAdvCSVFormatter.Create;
Begin
  Inherited;

  FSeparator := ',';
  FQuote := '"';
  FHasQuote := True;
  FEmptyLine := True;
End;


Destructor TAdvCSVFormatter.Destroy;
Begin
  Inherited;
End;


Procedure TAdvCSVFormatter.Clear;
Begin
  Inherited;

  FEmptyLine := True;
End;


Procedure TAdvCSVFormatter.ProduceEntryStringList(oEntryStringList: TAdvStringList);
Var
  iEntryIndex : Integer;
Begin
  For iEntryIndex := 0 To oEntryStringList.Count - 1 Do
    ProduceEntry(oEntryStringList[iEntryIndex]);
End;


Procedure TAdvCSVFormatter.ProduceEntryStringArray(Const aEntryStringArray: Array Of String);
Var
  iEntryIndex : Integer;
Begin
  For iEntryIndex := Low(aEntryStringArray) To High(aEntryStringArray) Do
    ProduceEntry(aEntryStringArray[iEntryIndex]);
End;


Procedure TAdvCSVFormatter.ProduceEntry(Const sEntry : String);
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


Procedure TAdvCSVFormatter.ProduceNewLine;
Begin
  Inherited;

  FEmptyLine := True;
End;


Procedure TAdvCSVFormatter.ProduceSeparator;
Begin 
  Produce(FSeparator);
End;


Constructor TAdvTextFormatter.Create;
Begin
  Inherited;

  FHasWhitespace := True;
  FWhitespaceCharacter := ' ';
  FWhitespaceMultiple := 2;
  {$IFNDEF VER130}
  Encoding := SysUtils.TEncoding.UTF8;
  {$ENDIF}
End;


Function TAdvTextFormatter.Link : TAdvTextFormatter;
Begin
  Result := TAdvTextFormatter(Inherited Link);
End;


Procedure TAdvTextFormatter.Clear;
Begin 
  Inherited;

  FLevel := 0;
End;  


Function TAdvTextFormatter.BeforeWhitespace : String;
Begin 
  // Multiply of the space character by FLevel * 2 is more efficient than Multiply of string '  ' by FLevel because it uses FillChar.

  If FHasWhitespace Then
    Result := StringMultiply(FWhitespaceCharacter, FLevel * FWhitespaceMultiple)
  Else
    Result := '';
End;  


Function TAdvTextFormatter.AfterWhitespace : String;
Begin 
  If FHasWhitespace Then
    Result := cReturn
  Else
    Result := '';
End;


Procedure TAdvTextFormatter.ProduceFragment(Const sValue: String);
Begin 
  Produce(sValue);
End;  


Procedure TAdvTextFormatter.ProduceLine(Const sValue: String);
Begin
  Produce(BeforeWhitespace + sValue + AfterWhitespace);
End;


Procedure TAdvTextFormatter.ProduceNewLine;
Begin 
  Produce(cReturn);
End;  


Procedure TAdvTextFormatter.LevelDown;
Begin 
  Inc(FLevel);
End;  


Procedure TAdvTextFormatter.LevelUp;
Begin 
  Dec(FLevel);
End;  


Procedure TAdvTextFormatter.ProduceInline(Const sValue: String);
Begin
  Produce(sValue);
End;


Constructor TAdvTextExtractor.Create;
Begin
  Inherited;

  FBuilder := TAdvStringBuilder.Create;
End;


Destructor TAdvTextExtractor.Destroy;
Begin
  FBuilder.Free;

  Inherited;
End;


Procedure TAdvTextExtractor.CacheAdd(Const sValue: String);
Begin
  Inherited;

  Dec(FLine, StringCount(sValue, cEnter));
End;


Procedure TAdvTextExtractor.CacheRemove(Const sValue: String);
Begin
  Inherited;

  Inc(FLine, StringCount(sValue, cEnter));
End;


Function TAdvTextExtractor.ConsumeLine : String;
Begin
  Result := ConsumeUntilCharacterSet(setVertical);

  ConsumeWhileCharacterSet(setVertical);
End;


Procedure TAdvTextExtractor.RaiseError(Const sMethod, sMessage: String);
Begin
  Inherited RaiseError(sMethod, StringFormat('Line %d: %s', [FLine, sMessage]));
End;


Function TAdvTextExtractor.ErrorClass: EAdvExceptionClass;
Begin
  Result := EAdvTextExtractor;
End;



Procedure TAdvTextExtractor.ProduceString(Const sToken : String);
Begin
  FCache := sToken + FCache;

  CacheAdd(sToken);
End;


Procedure TAdvTextExtractor.ProduceCharacter(Const cToken : Char);
Begin
  ProduceString(cToken);
End;


Function TAdvTextExtractor.MatchString(Const sToken: String): Boolean;
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


Function TAdvTextExtractor.MatchStringArray(Const aTokenSet: Array Of String): Integer;
Begin
  Result := High(aTokenSet);
  While (Result >= Low(aTokenSet)) And Not MatchString(aTokenSet[Result]) Do
    Dec(Result);
End;


Function TAdvTextExtractor.NextCharacter : Char;
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


Function TAdvTextExtractor.ConsumeCharacter : Char;
Begin
  Result := NextCharacter;

  Delete(FCache, 1, 1);

  CacheRemove(Result);
End;


Function TAdvTextExtractor.ConsumeCharacter(Const cToken : Char) : Char;

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


Function TAdvTextExtractor.ConsumeString(Const sToken : String) : String;
Begin
  If Not MatchString(sToken) Then
    RaiseError('Consume(String)', StringFormat('Expected token ''%s'' but found token ''%s''', [sToken, Copy(FCache, 1, Length(sToken))]));

  Delete(FCache, 1, Length(sToken));

  CacheRemove(sToken);

  Result := sToken;
End;


Function TAdvTextExtractor.ConsumeCharacterCount(Const iCharacterCount : Integer) : String;
Var
  iLoop : Integer;
Begin
  SetLength(Result, iCharacterCount);

  For iLoop := 1 To iCharacterCount Do
    Result[iLoop] := ConsumeCharacter;
End;


Function TAdvTextExtractor.ConsumeUntilCharacterSet(Const aTokenSet: TCharSet): String;
Begin
  FBuilder.Clear;
  While More And Not CharInSet(NextCharacter, aTokenSet) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TAdvTextExtractor.ConsumeUntilString(Const sToken: String): String;
Begin
  FBuilder.Clear;
  While More And Not MatchString(sToken) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TAdvTextExtractor.ConsumeUntilString(Const aStringArray: Array Of String): String;
Begin
  FBuilder.Clear;
  While More And Not (MatchStringArray(aStringArray) >= 0) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TAdvTextExtractor.ConsumeUntilCharacter(Const cToken : Char) : String;
Begin
  FBuilder.Clear;
  While More And (NextCharacter <> cToken) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TAdvTextExtractor.ConsumeRestStream : String;
Begin
  FBuilder.Clear;
  While More Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TAdvTextExtractor.ConsumeWhileCharacter(Const cToken : Char) : String;
Begin
  FBuilder.Clear;
  While More And (NextCharacter = cToken) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TAdvTextExtractor.ConsumeWhileCharacterSet(Const aTokenSet : TCharSet) : String;
Begin
  FBuilder.Clear;
  While More And CharInSet(NextCharacter, aTokenSet) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TAdvTextExtractor.ConsumeWhileCharacterSet(Const oCharacterSet: TAdvCharacterSet): String;
Begin
  FBuilder.Clear;
  While More And oCharacterSet.ContainsValue(NextCharacter) Do
    FBuilder.Append(ConsumeCharacter);
  Result := FBuilder.AsString;
End;


Function TAdvTextExtractor.CacheLength: Integer;
Begin
  Result := Length(FCache);
End;


Function TAdvTextExtractor.StreamPosition: Int64;
Begin
  Assert(Invariants('StreamPosition', BaseStream, TAdvAccessStream, 'Stream'));

  Result := TAdvAccessStream(BaseStream).Position - CacheLength;
End;


Function TAdvTextExtractor.More : Boolean;
Begin
  Result := Not Inherited EndOfStream Or (Length(FCache) > 0);
End;

Function TAdvExtractor.ErrorClass : EAdvExceptionClass;
Begin
  Result := EAdvExtractor;
End;


Procedure TAdvExtractor.SetStream(oStream: TAdvStream);
Begin
  Inherited;

  Clear;
End;


Procedure TAdvExtractor.Clear;
Begin
End;


Function TAdvExtractor.More: Boolean;
Begin
  Result := (Stream.Readable > 0);
End;



Function TAdvFormatter.ErrorClass : EAdvExceptionClass;
Begin 
  Result := EAdvFormatter;
End;


Procedure TAdvFormatter.Clear;
Begin
End;


Procedure TAdvFormatter.ProduceBytes(Const aBytes : TBytes);
Begin
  Write(aBytes[0], Length(aBytes));
End;

Procedure TAdvFormatter.Produce(Const sText: String);
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


Procedure TAdvFormatter.SetStream(oStream: TAdvStream);
Begin
  Inherited;

  Clear;
End;


{ TAdvStreamReader }

constructor TAdvStreamReader.Create(aStream: TAdvStream);
begin
  Create(aStream, TEncoding.UTF8, True);
end;

constructor TAdvStreamReader.Create(aStream: TAdvStream; DetectBOM: Boolean);
begin
  Create(aStream, TEncoding.UTF8, DetectBOM);
end;

constructor TAdvStreamReader.Create(aStream: TAdvStream; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0);
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

constructor TAdvStreamReader.Create(const Filename: string; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0);
var
  oFile : TAdvFile;
begin
  oFile := TAdvFile.Create(FileName, fmOpenRead);
  Try
    Create(oFile.Link, Encoding, DetectBOM, BufferSize);
  Finally
    oFile.Free;
  End;
end;

constructor TAdvStreamReader.Create(const Filename: string; DetectBOM: Boolean);
begin
  Create(Filename, TEncoding.UTF8, DetectBOM);
end;

constructor TAdvStreamReader.Create(const Filename: string);
begin
  Create(Filename, TEncoding.UTF8, true);
end;

destructor TAdvStreamReader.Destroy;
begin
  Close;
  inherited;
end;

procedure TAdvStreamReader.Close;
begin
  FStream.Free;
  FStream := nil;

  DiscardBufferedData;
  FClosed := true;
end;

procedure TAdvStreamReader.DiscardBufferedData;
begin
  if not FClosed then
  begin
    FBufferEnd := 0;
    FBufferStart := 0;
    FNoDataInStream := False;
  end;
end;

function TAdvStreamReader.DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
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

procedure TAdvStreamReader.FillBuffer(var Encoding: TEncoding);
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
    StartIndex := DetectBOM(Encoding, LBuffer)
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

function TAdvStreamReader.GetEndOfStream: Boolean;
begin
  if not FNoDataInStream and (not FClosed) and (FBufferEnd <= FBufferStart) then
    FillBuffer(FEncoding);
  Result := FNoDataInStream and ((FClosed) or (FBufferEnd = FBufferStart));
end;

function TAdvStreamReader.Peek: Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    if FBufferEnd < 1 + FBufferStart  then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData[FBufferStart]);
  end;
end;

function TAdvStreamReader.Read(const Buffer: TCharArray; Index, Count: Integer): Integer;
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

function TAdvStreamReader.ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  Result := Read(Buffer, Index, Count);
end;

function TAdvStreamReader.Read: Integer;
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

function TAdvStreamReader.ReadLine: string;
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

function TAdvStreamReader.ReadToEnd: string;
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

function TAdvStreamReader.SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
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


{ TAdvTextReader }

function TAdvTextReader.ReadString(var s: String; iLength: Integer): Integer;
var
  oBuffer : TCharArray;
begin
  SetLength(oBuffer, iLength);
  result := ReadBlock(oBuffer, 0, iLength);
  SetString(s, pchar(oBuffer), result);
end;

{ TAdvStringReader }

procedure TAdvStringReader.Close;
begin
 // nothing
end;

constructor TAdvStringReader.Create(content: String);
begin
  inherited Create;
  FContent := content;
  FCursor := 1;
end;

function TAdvStringReader.Peek: Integer;
begin
  if FCursor > FContent.Length then
    result := -1
  else
    result := ord(FContent[FCursor]);
end;

function TAdvStringReader.Read: Integer;
begin
  if FCursor > FContent.Length then
    result := -1
  else
  begin
    result := ord(FContent[FCursor]);
    inc(FCursor);
  end;
end;

function TAdvStringReader.Read(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  raise Exception.Create('Not done yet');
end;

function TAdvStringReader.ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  raise Exception.Create('Not done yet');
end;

function TAdvStringReader.ReadLine: string;
begin
  raise Exception.Create('Not done yet');
end;

function TAdvStringReader.ReadToEnd: string;
begin
  raise Exception.Create('Not done yet');
end;


Constructor TAdvCharacterSet.Create;
Begin
  Inherited;

  Owns := False;
  Parts := @FDataSet;
  Size := SizeOf(FDataSet);
End;


Destructor TAdvCharacterSet.Destroy;
Begin
  Inherited;
End;


Procedure TAdvCharacterSet.AddRange(Const aFromValue, aToValue: Char);
Begin
  FDataSet := FDataSet + [aFromValue..aToValue];
End;


Procedure TAdvCharacterSet.AddValue(Const aValue: Char);
Begin
  FDataSet := FDataSet + [aValue];
End;


Function TAdvCharacterSet.ContainsValue(Const aValue: Char): Boolean;
Begin
  Result := CharInSet(aValue, FDataSet);
End;


Function TAdvCharacterSet.GetAsText : String;
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


Procedure TAdvCharacterSet.SetAsText(Const Value: String);
Var
  oStrings : TAdvStringList;
  iLoop    : Integer;
  sField   : String;
  sLeft    : String;
  sRight   : String;
Begin
  Fill(False);

  oStrings := TAdvStringList.Create;
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

procedure produceCsv(filename : String; headers : Array of String; values : TGetCsvValues);
var
  csv : TCSVWriter;
  arr : TArray<String>;
  s : String;
begin
  csv := TCSVWriter.Create;
  try
    csv.Stream := TAdvFile.Create(filename, fmCreate);
    for s in headers do
      csv.cell(s);
    csv.line;
    values(csv);
  finally
    csv.Free;
  end;
end;

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
  b : TStringBuilder;
begin
  Result := '';
  if Length(AStr) <= 0 then
    exit;
  b := TStringBuilder.Create;
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
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
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
  b : TStringBuilder;
begin
  LInAmp := false;
  LInTag := false;
  LInAttr := false;
  Result := '';
  if Length(AStr) <= 0 then
    exit;
  b := TStringBuilder.Create;
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
  b : TStringBuilder;
begin
  Result := '';
  if Length(AStr) <= 0 then
    exit;
  b := TStringBuilder.Create;
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

procedure StringToStream(content: String; stream : TAdvStream; encoding : TEncoding);
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

function StreamToString(stream : TAdvStream; encoding : TEncoding; AShareMode : Word = fmOpenRead + fmShareDenyWrite) : String;
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

function UTF8StreamToString(value : TAdvAccessStream) : String;
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



End. // AdvCSVExtractors //
