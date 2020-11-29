unit cda_scint;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)

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
  SysUtils, Classes, Vcl.Graphics,
  ScintEdit,
  fsl_base, fsl_stream,
  cda_base, cda_objects, cda_documents;
(*
  gwCDA, v3Infrastructure, XMLBuilder, ReferenceModelBase, CDATypes;
*)


type
  TCDAStylerStyle = (
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

  TCDAStyleCategory = (scNull, scHeader, scSpacer, scBody, scEntry);

  TCDAStylerMode = (csmNone, csmRIM, csmCategory);

  TCDAStyler = class(TScintCustomStyler)
  private
    FDoc : TCDADocument;
    FLastType : TCDAClassType;
    FLastCategory : TCDAStyleCategory;

    FMode: TCDAStylerMode;
    function inRange(current, start, end_ : TSourceLocation) : boolean;
    function getTypeForLocation(line, col : integer) : TCDAClassType; overload;
    function getTypeForLocation(location : TSourceLocation): TCDAClassType; overload;
    function getCategoryForLocation(line, col : integer) : TCDAStyleCategory; overload;
    function getCategoryForLocation(location : TSourceLocation): TCDAStyleCategory; overload;
    function getElementForLocation(base : Tv3Base; location : TSourceLocation) : Tv3Base;
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
    procedure SetDoc(const Value: TCDADocument);
    procedure SetMode(const Value: TCDAStylerMode);
  protected
    procedure CommitStyle(Style: TCDAStylerStyle);
    procedure GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes); override;
    function LineTextSpans(const S: TScintRawString): Boolean; override;
    procedure StyleNeeded; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;

    Property Doc : TCDADocument read FDoc write SetDoc;
    Property Mode : TCDAStylerMode read FMode write SetMode;
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

{ TCDAStyler }

procedure TCDAStyler.CommitStyle(Style: TCDAStylerStyle);
var
  ct : TCDAClassType;
  sc : TCDAStyleCategory;
begin
  case FMode of
    csmNone: LineState := Ord(Style);
    csmRIM:
      begin
        ct := getTypeForLocation(FirstLine, CurIndex);
        LineState := Ord(Style) + (Ord(ct)) shl 4;
      end;
    csmCategory:
      begin
        sc := getCategoryForLocation(FirstLine, CurIndex);
        LineState := Ord(Style) + (Ord(sc)) shl 4;
      end;
  end;
  inherited CommitStyle(LineState);
end;

constructor TCDAStyler.Create(AOwner: TComponent);
begin
  inherited;
  Mode := csmCategory;
end;

destructor TCDAStyler.Destroy;
begin
  FDoc.Free;
  inherited;
end;

function TCDAStyler.inRange(current, start, end_ : TSourceLocation):Boolean;
begin
  result := ((current.line > start.line) or ((current.line = start.line) and (current.col >= start.col))) and
            ((current.line < end_.line) or ((current.line = end_.line) and (current.col <= end_.col)));
end;

function TCDAStyler.getElementForLocation(base : Tv3Base; location : TSourceLocation): Tv3Base;
var
  iter : Tv3DataTypePropertyIterator;
  i : integer;
begin
  if (base = nil) or not inRange(location, base.sourcelocation, base.sourcelocationEnd) then
    result := nil
  else
  begin
    result := nil;
    iter := base.createIterator(true);
    try
      while (result = nil) and (iter.More) do
      begin
//        if iter.Current.PropertyType = rmpdtClass then
//          if iter.Current.CollectionState = rmpctNone then
//            result := getElementForLocation(Tv3Base(iter.Current.AsType(Tv3Base)), location)
//          else
//            for i := 0 to iter.Current.Collection.Count - 1 do
//            begin
//              result := getElementForLocation(TFslObject(iter.Current.Collection[i]) as Tv3Base, location);
//              if result <> nil then
//                break;
//            end;
//        iter.next;
      end;
    finally
      iter.Free;
    end;
    if result = nil then
      result := base;
  end;
end;

procedure TCDAStyler.GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes);
var
  st : TCDAStylerStyle;
  ct : TCDAClassType;
  sc : TCDAStyleCategory;
begin
  st := TCDAStylerStyle(style and $F);
  case FMode of
    csmRIM:
      begin
      ct := TCDAClassType(style shr 4);
      case ct of
        itNull, etSpacer: Attributes.BackColor := clWhite;
        etAct: Attributes.BackColor := COLOUR_ACT;
        etParticipation: Attributes.BackColor := COLOUR_PARTICIPATION;
        etRole: Attributes.BackColor := COLOUR_ROLE;
        etEntity: Attributes.BackColor := COLOUR_ENTITY;
        etActRel: Attributes.BackColor := COLOUR_ACT_RELATIONSHIP;
        etDatatype: Attributes.BackColor := COLOUR_DATATYPE;
        etNarrative: Attributes.BackColor := COLOUR_NARRATIVE;
        etExtension: Attributes.BackColor := COLOUR_EXTENSION;
      end;
      end;
    csmCategory:
      begin
      sc := TCDAStyleCategory(style shr 4);
      case sc of
        scHeader: Attributes.BackColor := COLOUR_HEADER;
        scBody: Attributes.BackColor := COLOUR_BODY;
        scEntry: Attributes.BackColor := COLOUR_ENTRY;
      end;
      end;
  end;

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

function TCDAStyler.getTypeForLocation(location : TSourceLocation): TCDAClassType;
var
  el : Tv3Base;
begin
  if (FLastType <> itNull) then
    result := FLastType
  else if FDoc = nil then
    result := itNull
  else
  begin
    el := getElementForLocation(FDoc.Root, location);
    if el = nil then
      result := itNull
    else
      result := el.CDAClassType;
    while (result = etDatatype) and (el.Parent <> nil) do
    begin
      el := el.Parent;
      result := el.CDAClassType;
    end;
    FLastType := result;
  end;
end;

function TCDAStyler.getTypeForLocation(line, col: integer): TCDAClassType;
var
  location : TSourceLocation;
begin
  location.line := line+1;
  location.col := col;
  result := getTypeForLocation(location);
end;

function TCDAStyler.getCategoryForLocation(location : TSourceLocation): TCDAStyleCategory;
var
  el : Tv3Base;
begin
  if (FLastType <> itNull) then
    result := FLastCategory
  else if FDoc = nil then
    result := scNull
  else
  begin
    el := getElementForLocation(FDoc.Root, location);
    if el = nil then
      result := scNull
    else
      result := scHeader;
    while (result = scHeader) and (el <> nil) do
    begin
      if (el is TcdaEntry) then
        result := scEntry
      else if el is TcdaComponent2 then
        result := scBody;
      el := el.Parent;
    end;
    FLastCategory := result;
  end;
end;

function TCDAStyler.getCategoryForLocation(line, col: integer): TCDAStyleCategory;
var
  location : TSourceLocation;
begin
  location.line := line+1;
  location.col := col;
  result := getCategoryForLocation(location);
end;

function TCDAStyler.LineTextSpans(const S: TScintRawString): Boolean;
begin
  result := false;
end;

procedure TCDAStyler.SetDoc(const Value: TCDADocument);
begin
  FDoc.Free;
  FDoc := Value;
end;

procedure TCDAStyler.SetMode(const Value: TCDAStylerMode);
begin
  FMode := Value;
end;

procedure TCDAStyler.StyleNeeded;
var
  startState : TCDAStylerStyle;
begin
  startState := TCDAStylerStyle(LineState and $F);

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

procedure TCDAStyler.scanAttributeName;
begin
  ConsumeChars(XmlNameChars);
  CommitStyle(stAttributeName);
  if not EndOfLine then
    scanAttributeStart;
end;

procedure TCDAStyler.scanAttributeStart;
begin
  if ConsumeChar('=') and ConsumeChar('"') then
  begin
    CommitStyle(stAttributeStart);
    scanAttributeValue;
  end
  else if not EndOfLine then
    scanElementWhitespace;
end;

procedure TCDAStyler.scanAttributeValue;
begin
  ConsumeUntil('"', false);
  CommitStyle(stAttributeValue);
  if not EndOfLine then
    scanAttributeEnd;
end;

procedure TCDAStyler.scanAttributeEnd;
begin
  if ConsumeChar('"') then
    CommitStyle(stAttributeEnd);
  scanElementWhitespace;
end;

procedure TCDAStyler.scanComment;
begin
  ConsumeUntil('-->', true);
  CommitStyle(stComment);
  scanText;
end;

procedure TCDAStyler.scanElementName;
var
  ct : TCDAClassType;
begin
  ConsumeChars(XmlNameChars);
  CommitStyle(stElementName);
  scanElementWhitespace;
end;

procedure TCDAStyler.scanElementWhitespace;
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

procedure TCDAStyler.scanEndElement;
begin
  ConsumeChar('/');
  ConsumeChar('>');
  CommitStyle(stEndElement);
  FLastType := itNull;
  FLastCategory := scNull;
  scanText;
end;

procedure TCDAStyler.scanEntity;
begin

end;

procedure TCDAStyler.scanInstruction;
begin
  ConsumeUntil('?>', true);
  CommitStyle(stInstruction);
  scanText;
end;

procedure TCDAStyler.scanPCData;
begin

end;

procedure TCDAStyler.scanStartElement;
begin
  FLastType := itNull;
  FLastCategory := scNull;
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

procedure TCDAStyler.scanText;
begin
  ConsumeCharsNot(['<'{, '&'}]);
  CommitStyle(stText);
  if ConsumeChar('<') then
    scanStartElement;
end;


function TCDAStyler.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDoc.sizeInBytes);
  inc(result, FLastType.sizeInBytes);
  inc(result, FLastCategory.sizeInBytes);
  inc(result, FMode.sizeInBytes);
end;

end.
