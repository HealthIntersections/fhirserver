unit ScintFormats;

{
Copyright (c) 2014+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

uses
  SysUtils, Classes, Graphics,
  ScintEdit,
  FHIR.Support.Base;

type
  TXmlStylerStyle = (
    stText,
    stStartElement,
    stElementWhitespace,
    stEndElement,
    stElementName,
    stAttributeName,
    stAttributeStart,
    stAttributeValue,
    stAttributeEnd,
    stComment,
    stEntity,
    stInstruction,
    stPCData
  );

  TXmlStyler = class(TScintCustomStyler)
  private
    procedure scanText;
    procedure scanStartElement;
    procedure scanElementWhitespace;
    procedure scanEndElement;
    procedure scanElementName;
    procedure scanAttributeName;
    procedure scanAttributeStart;
    procedure scanAttributeValue;
    procedure scanAttributeEnd;
    procedure scanComment;
    procedure scanEntity;
    procedure scanInstruction;
    procedure scanPCData;
  protected
    procedure CommitStyle(Style: TXmlStylerStyle);
    procedure GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes); override;
    function LineTextSpans(const S: TScintRawString): Boolean; override;
    procedure StyleNeeded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
  end;

  TJsonStylerStyle = (
    jstWhitespace,
    jstGrammar,
    jstValueString,
    jstValueOther
  );

  TJsonStyler = class(TScintCustomStyler)
  private
    procedure scanWhitespace;
    procedure scanNumber;
    procedure scanToken(s : String);
    procedure scanString(ch : ansichar);
  protected
    procedure CommitStyle(Style: TJsonStylerStyle);
    procedure GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes); override;
    function LineTextSpans(const S: TScintRawString): Boolean; override;
    procedure StyleNeeded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
  end;

  TLiquidStylerStyle = (
    lqSpace,
    lqDelimiters,
    lqObject,
    lqTag,
    lqComment,
    lqCommented
  );

  TLiquidStyler = class(TScintCustomStyler)
  private
    procedure scanSpace;
    procedure scanObject;
    procedure scanTag;
    procedure scanComment;
    procedure scanCommented;
  protected
    procedure CommitStyle(Style: TLiquidStylerStyle);
    procedure GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes); override;
    function LineTextSpans(const S: TScintRawString): Boolean; override;
    procedure StyleNeeded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
  end;

implementation

const
  AllChars = [#0..#255];
  WhitespaceChars = [#0..' '];
  AlphaChars = ['A'..'Z', 'a'..'z'];
  DigitChars = ['0'..'9'];
  HexDigitChars = DigitChars + ['A'..'F', 'a'..'f'];
  AlphaUnderscoreChars = AlphaChars + ['_'];
  AlphaDigitChars = AlphaChars + DigitChars;
  AlphaDigitUnderscoreChars = AlphaChars + DigitChars + ['_'];
  XmlNameChars = AlphaDigitUnderscoreChars + [':', '-'];

  COLOUR_ACT = $D1B1ff;
  COLOUR_PARTICIPATION = $ffffD1;
  COLOUR_ROLE = $D1ffff;
  COLOUR_ENTITY = $D1ffD1;
  COLOUR_ACT_RELATIONSHIP = $EAEAff;
  COLOUR_DATATYPE = $DFDFDF;
  COLOUR_NARRATIVE = $ffD1D1;
  COLOUR_EXTENSION = $D1D1D1;

  COLOUR_HEADER = $EEFFFF;
  COLOUR_BODY = $FFEEFF;
  COLOUR_ENTRY = $FFFFEE;

{ TXmlStyler }

procedure TXmlStyler.CommitStyle(Style: TXmlStylerStyle);
begin
  LineState := Ord(Style);
  inherited CommitStyle(LineState);
end;

constructor TXmlStyler.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TXmlStyler.Destroy;
begin
  inherited;
end;

function TXmlStyler.LineTextSpans(const S: TScintRawString): Boolean;
begin
  result := false;
end;

procedure TXmlStyler.StyleNeeded;
var
  startState : TXmlStylerStyle;
begin
  startState := TXmlStylerStyle(LineState and $F);

  case startState of
    stText: scanText;
    stStartElement : scanStartElement;
    stElementWhitespace : scanElementWhitespace;
    stEndElement: scanEndElement;
    stElementName: scanElementName;
    stAttributeName: scanAttributeName;
    stAttributeValue: scanAttributeValue;
    stComment: scanComment;
    stEntity: scanEntity;
    stInstruction: scanInstruction;
    stPCData: scanPCData;
  else
    raise ELibraryException.create('Error Message');
  end;
end;

procedure TXmlStyler.scanAttributeName;
begin
  ConsumeChars(XmlNameChars);
  CommitStyle(stAttributeName);
  if not EndOfLine then
    scanAttributeStart;
end;

procedure TXmlStyler.scanAttributeStart;
begin
  if ConsumeChar('=') and ConsumeChar('"') then
  begin
    CommitStyle(stAttributeStart);
    scanAttributeValue;
  end
  else if not EndOfLine then
    scanElementWhitespace;
end;

procedure TXmlStyler.scanAttributeValue;
begin
  ConsumeUntil('"', false);
  CommitStyle(stAttributeValue);
  if not EndOfLine then
    scanAttributeEnd;
end;

procedure TXmlStyler.scanAttributeEnd;
begin
  if ConsumeChar('"') then
    CommitStyle(stAttributeEnd);
  scanElementWhitespace;
end;

procedure TXmlStyler.scanComment;
begin
  ConsumeUntil('-->', true);
  CommitStyle(stComment);
  scanText;
end;

procedure TXmlStyler.scanElementName;
begin
  ConsumeChars(XmlNameChars);
  CommitStyle(stElementName);
  scanElementWhitespace;
end;

procedure TXmlStyler.scanElementWhitespace;
begin
  ConsumeChars(WhitespaceChars);
  CommitStyle(stElementWhitespace);
  if not EndOfLine then
    if ConsumeChar('=') or ConsumeChar('"') then
     scanAttributeStart
    else if ConsumeChar('/') or ConsumeChar('>') then
      scanEndElement
    else
      scanAttributeName;
end;

procedure TXmlStyler.scanEndElement;
begin
  ConsumeChar('/');
  ConsumeChar('>');
  CommitStyle(stEndElement);
  scanText;
end;

procedure TXmlStyler.scanEntity;
begin

end;

procedure TXmlStyler.scanInstruction;
begin
  ConsumeUntil('?>', true);
  CommitStyle(stInstruction);
  scanText;
end;

procedure TXmlStyler.scanPCData;
begin

end;

procedure TXmlStyler.scanStartElement;
begin
  if ConsumeChar('?') then
    scanInstruction
  else if ConsumeChar('!') and ConsumeChar('-') and ConsumeChar('-')  then
    scanComment
  else if not EndOfLine then
  begin
    ConsumeChar('/');
    CommitStyle(stStartElement);
    scanElementName();
  end;
end;

procedure TXmlStyler.scanText;
begin
  ConsumeCharsNot(['<'{, '&'}]);
  CommitStyle(stText);
  if ConsumeChar('<') then
    scanStartElement;
end;


procedure TXmlStyler.GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes);
var
  st : TXMLStylerStyle;
begin
  st := TXMLStylerStyle(style and $F);
  case st of
    stText :
      begin
      Attributes.ForeColor := clBlack;
      end;
    stStartElement, stElementWhitespace, stEndElement, stAttributeStart, stAttributeEnd:
      begin
      Attributes.ForeColor := clPurple;
      end;
    stElementName :
      begin
      Attributes.ForeColor := clBlack;
      Attributes.FontStyle := [fsBold];
      end;
    stAttributeName :
      begin
      Attributes.ForeColor := clNavy;
      end;
    stAttributeValue :
      begin
      Attributes.ForeColor := clBlack;
      Attributes.FontStyle := [fsItalic];
      end;
    stComment :
      begin
      Attributes.ForeColor := clGray;
      end;
    stEntity :
      begin
      Attributes.ForeColor := clBlack;
      Attributes.FontStyle := [fsItalic];
      end;
    stInstruction :
      begin
      Attributes.ForeColor := clTeal;
      end;
    stPCData :
      begin
      Attributes.ForeColor := clBlack;
      Attributes.BackColor := clLtGray;
      end;
  end;
end;


{ TJsonStyler }

destructor TJsonStyler.Destroy;
begin
  inherited;
end;

procedure TJsonStyler.CommitStyle(Style: TJsonStylerStyle);
begin
  LineState := Ord(Style);
  inherited CommitStyle(LineState);
end;

constructor TJsonStyler.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TJsonStyler.GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes);
var
  st : TJsonStylerStyle;
begin
  st := TJsonStylerStyle(style and $F);
  case st of
    jstWhitespace :
      Attributes.ForeColor := clBlack;
    jstGrammar:
      begin
      Attributes.FontStyle := [fsBold];
      Attributes.ForeColor := clPurple;
      end;
    jstValueString : Attributes.ForeColor := clBlack;
    jstValueOther : Attributes.ForeColor := clNavy;
  end;
end;

function TJsonStyler.LineTextSpans(const S: TScintRawString): Boolean;
begin
  result := false;
end;

procedure TJsonStyler.scanToken;
var
  i : integer;
begin
  for i := 1 to s.Length do
    ConsumeChar;
  CommitStyle(jstValueOther);
end;

procedure TJsonStyler.scanNumber;
begin
  ConsumeChars(['0'..'9', 'e', '-', '.']);
  CommitStyle(jstValueOther);
end;

procedure TJsonStyler.scanString(ch : ansichar);
var
  done : boolean;
begin
  ConsumeChar(ch);
  done := false;
  while not done do
  begin
    ConsumeCharsNot([ch, '\']);
    done := not (CurCharIs('\') and NextCharIs(ch));
    if not done and CurCharIs('\') then
      ConsumeChar;

  end;
  ConsumeChar(ch);
  CommitStyle(jstValueString);
end;

procedure TJsonStyler.scanWhitespace;
begin
  while not EndOfLine do
  begin
    if CurCharIn(WhitespaceChars) then
    begin
      ConsumeChars(WhitespaceChars);
      CommitStyle(jstWhitespace);
    end
    else if CurCharIn(['{', '[', ']', '}', ',', ':']) then
    begin
      ConsumeChar;
      CommitStyle(jstGrammar);
    end
    else if CurCharIs('''') then
      ScanString('''')
    else if CurCharIs('"') then
      ScanString('"')
    else if hasToken('true') then
      ScanToken('true')
    else if hasToken('false') then
      ScanToken('true')
    else if hasToken('null') then
      ScanToken('true')
    else if CurCharIn(['-', '0'..'9']) then
      ScanNumber()
    else
    begin
      ConsumeAllRemaining;
      CommitStyle(jstWhitespace);
    end;
  end;
end;

procedure TJsonStyler.StyleNeeded;
begin
  scanWhitespace;
end;

{ TLiquidStyler }

constructor TLiquidStyler.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TLiquidStyler.Destroy;
begin

  inherited;
end;

procedure TLiquidStyler.CommitStyle(Style: TLiquidStylerStyle);
begin
  LineState := Ord(Style);
  inherited CommitStyle(LineState);
end;

procedure TLiquidStyler.GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes);
var
  st : TLiquidStylerStyle;
begin
  st := TLiquidStylerStyle(style and $F);
  case st of
    lqSpace : Attributes.ForeColor := clBlack;
    lqDelimiters :
      begin
      Attributes.ForeColor := clPurple;
      Attributes.FontStyle := [fsBold];
      end;
    lqObject : Attributes.BackColor := clYellow;
    lqTag : Attributes.BackColor := clAqua;
    lqComment : Attributes.BackColor := clLtGray;
    lqCommented : Attributes.ForeColor := clDkGray;
  end;
end;

function TLiquidStyler.LineTextSpans(const S: TScintRawString): Boolean;
begin
  result := false;
end;

procedure TLiquidStyler.scanComment;
begin
  ConsumeChar(9);
  CommitStyle(lqComment);
  ConsumeChar(2);
  CommitStyle(lqDelimiters);
  scanCommented;
end;

procedure TLiquidStyler.scanCommented;
begin
  while not EndOfLine do
  begin
    if hasGrammar('{% endcomment %}') then
    begin
      ConsumeChar(2);
      CommitStyle(lqDelimiters);
      ConsumeChar(12);
      CommitStyle(lqComment);
      ConsumeChar(2);
      CommitStyle(lqDelimiters);
      scanSpace;
    end
    else if CurCharIs('{') then
    begin
      ConsumeChar();
      CommitStyle(lqCommented);
    end
    else
    begin
      ConsumeCharsNot(['{']);
      CommitStyle(lqCommented);
    end;
  end;
end;

procedure TLiquidStyler.scanObject;
var
  done : boolean;
begin
  repeat
    ConsumeCharsNot(['}']);
    done := (CurCharIs('}') and NextCharIs('}'));
    if not done then
      ConsumeChar;
  until EndOfLine or done;
  CommitStyle(lqObject);
  ConsumeChar;
  ConsumeChar;
  CommitStyle(lqDelimiters);
  scanSpace;
end;

procedure TLiquidStyler.scanSpace;
begin
  while not EndOfLine do
  begin
    ConsumeCharsNot(['{']);
    CommitStyle(lqSpace);
    if CurCharIs('{') and NextCharIs('{') then
    begin
      ConsumeChar;
      ConsumeChar;
      CommitStyle(lqDelimiters);
      scanObject;
    end
    else if CurCharIs('{') and NextCharIs('%') then
    begin
      ConsumeChar;
      ConsumeChar;
      CommitStyle(lqDelimiters);
      scanTag;
    end
    else if CurCharIs('{') then
    begin
      ConsumeChar;
      CommitStyle(lqSpace);
    end;
  end;
end;

procedure TLiquidStyler.scanTag;
var
  done : boolean;
begin
  if hasGrammar(' comment %}') then
    scanComment
  else
  begin
    repeat
      ConsumeCharsNot(['%']);
      done := (CurCharIs('%') and NextCharIs('}'));
      if not done then
        ConsumeChar;
    until EndOfLine or done;
    CommitStyle(lqTag);
    ConsumeChar;
    ConsumeChar;
    CommitStyle(lqDelimiters);
    scanSpace;
  end;
end;

procedure TLiquidStyler.StyleNeeded;
var
  startState : TLiquidStylerStyle;
begin
  startState := TLiquidStylerStyle(LineState and $F);
  case startState of
    lqSpace : scanSpace;
    lqDelimiters : scanSpace;
    lqObject : scanObject;
    lqTag : scanTag;
    lqComment : scanComment;
    lqCommented : scanCommented;
  else
    raise ELibraryException.create('Error Message');
  end;
end;

end.
