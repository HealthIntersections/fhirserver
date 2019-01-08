unit FHIR.R4.Scint;

interface

uses
  SysUtils, Classes, Graphics,
  ScintEdit,
  FHIR.Support.Base;


const
  AlphaChars = ['A'..'Z', 'a'..'z'];
  DigitChars = ['0'..'9'];
  HexDigitChars = DigitChars + ['A'..'F', 'a'..'f'];
  AlphaUnderscoreChars = AlphaChars + ['_'];
  AlphaDigitChars = AlphaChars + DigitChars;
  AlphaDigitUnderscoreChars = AlphaChars + DigitChars + ['_'];
  MAP_RESERVED_WORDS_GEN : array of String = ['map', 'uses', 'source', 'target', 'produced', 'consumed', 'alias', 'as', 'imports', 'group'];
  MAP_RESERVED_WORDS_RULE : array of String = ['group', 'source', 'target', 'as', 'where', 'check', 'then', 'log', '$this'];
  MAP_GRAMMAR_CHARS = ['[', ']', ',', ':', '/', '*', '&', '|', '!', '=', '{', '}', '(', ')', ';', '%', '.', '?', '+', '-', '<', '>'];
  MAP_TOKEN_START = AlphaUnderscoreChars;
  MAP_TOKEN_CHAR = AlphaDigitUnderscoreChars;

type
  TMapStylerStyle = (
    // outside a group
    stGenText,
    stGenComment,
    stGenKnownToken,
    stGenGrammar,
    stGenString,
    stGenError,
    // inside a group
    stText,
    stComment,
    stKnownToken,
    stGrammar,
    stString,
    stField,
    stNumber
  );

  TFHIRMapStyler = class(TScintCustomStyler)
  private
    procedure CommitStyle(Style: TMapStylerStyle);
    procedure scanGen;
    procedure scanGroup;
    procedure scanNumber;
    procedure scanToken(style : TMapStylerStyle);
    procedure scanString(ch : ansichar; style : TMapStylerStyle);
    procedure scanReservedWord(s : String; style : TMapStylerStyle);
  protected
    procedure GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes); override;
    function LineTextSpans(const S: TScintRawString): Boolean; override;
    procedure StyleNeeded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
  end;

implementation

const
  WhitespaceChars = [#0..' '];


{ TFHIRMapStyler }

constructor TFHIRMapStyler.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TFHIRMapStyler.Destroy;
begin
  inherited;
end;

procedure TFHIRMapStyler.CommitStyle(Style: TMapStylerStyle);
begin
  LineState := Ord(Style);
  inherited CommitStyle(LineState);
end;

procedure TFHIRMapStyler.GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes);
var
  st : TMapStylerStyle;
begin
  st := TMapStylerStyle(style);
  case st of
    stGenText, stText: ; // nothing.
    stGenComment, stComment:
      begin
      Attributes.FontStyle := [fsItalic];
      Attributes.ForeColor := clGreen;
      end;
    stGenKnownToken, stKnownToken :
      begin
      Attributes.FontStyle := [fsBold];
      Attributes.ForeColor := clBlack;
      end;
    stGenString, stString :
      begin
      Attributes.FontStyle := [];
      Attributes.ForeColor := clNavy;
      end;
    stGenError, stField :
      begin
      Attributes.FontStyle := [];
      Attributes.ForeColor := clMaroon;
      end;
    stGenGrammar, stGrammar :
      begin
      Attributes.FontStyle := [fsBold];
      Attributes.ForeColor := clPurple;
      end;
    stNumber : Attributes.ForeColor := clBlue;
  end;
end;

function TFHIRMapStyler.LineTextSpans(const S: TScintRawString): Boolean;
begin
  result := false;
end;

procedure TFHIRMapStyler.scanGen;
var
  rw : boolean;
  s : String;
begin
  while not EndOfLine do
  begin
    if CurCharIn(WhitespaceChars) then
    begin
      ConsumeChars(WhitespaceChars);
      CommitStyle(stGenText);
    end
    else if hasGrammar('//') then
    begin
      ConsumeAllRemaining;
      CommitStyle(stGenComment);
      CommitStyle(stGenText);
    end
    else if CurCharIn(MAP_GRAMMAR_CHARS) then
    begin
      ConsumeChar;
      CommitStyle(stGenGrammar);
    end
    else if CurCharIs('''') then
      ScanString('''', stGenError)
    else if CurCharIs('"') then
      ScanString('"', stGenString)
    else if hasToken('group') then
      ScanGroup
    else
    begin
      rw := false;
      for s in MAP_RESERVED_WORDS_GEN do
      begin
        if hasToken(s) then
        begin
          rw := true;
          scanReservedWord(s, stGenKnownToken);
          break;
        end;
      end;
      if not rw then
      begin
        if CurCharIn(MAP_TOKEN_START) then
          ScanToken(stGenText)
        else
        begin
          ConsumeAllRemaining;
          CommitStyle(stGenText);
        end;
      end;
    end;
  end;
end;

procedure TFHIRMapStyler.scanGroup;
var
  rw : boolean;
  s : String;
begin
  while not EndOfLine do
  begin
    if CurCharIn(WhitespaceChars) then
    begin
      ConsumeChars(WhitespaceChars);
      CommitStyle(stText);
    end
    else if hasGrammar('//') then
    begin
      ConsumeAllRemaining;
      CommitStyle(stComment);
      CommitStyle(stText);
    end
    else if CurCharIn(MAP_GRAMMAR_CHARS) then
    begin
      ConsumeChar;
      CommitStyle(stGrammar);
    end
    else if CurCharIs('''') then
      ScanString('''', stString)
    else if CurCharIs('"') then
      ScanString('"', stField)
    else
    begin
      rw := false;
      for s in MAP_RESERVED_WORDS_GEN do
      begin
        if hasToken(s) then
        begin
          rw := true;
          scanReservedWord(s, stKnownToken);
          break;
        end;
      end;
      if not rw then
      begin
        if CurCharIn(['0'..'9']) then
          ScanNumber()
        else if CurCharIn(MAP_TOKEN_START) then
          ScanToken(stText)
        else
        begin
          ConsumeAllRemaining;
          CommitStyle(stText);
        end;
      end;
    end;
  end;
end;

procedure TFHIRMapStyler.scanNumber;
begin
  if CurCharIs('0') and NextCharIs('x') then
  begin
    ConsumeChar;
    ConsumeChar;
    ConsumeChars(['0'..'7', 'a'..'f', 'A'..'F']);
  end
  else
    ConsumeChars(['0'..'9', 'e', '-', '.']);
  CommitStyle(stNumber);
end;

procedure TFHIRMapStyler.scanReservedWord(s: String; style: TMapStylerStyle);
var
  i : integer;
begin
  for i := 1 to s.Length do
    ConsumeChar;
  CommitStyle(style);
end;

procedure TFHIRMapStyler.scanString(ch : ansichar; style : TMapStylerStyle);
var
  done : boolean;
begin
  ConsumeChar(ch);
  done := false;
  while not done and not EndOfLine do
  begin
    ConsumeCharsNot([ch, '\']);
    done := CurCharIs(ch);
    if not done and CurCharIs('\') then
      ConsumeChar(2);
  end;
  ConsumeChar(ch);
  CommitStyle(style);
end;

procedure TFHIRMapStyler.scanToken(style: TMapStylerStyle);
begin
  ConsumeChar;
  while CurCharIn(MAP_TOKEN_CHAR) do
    ConsumeChar;
  CommitStyle(style);
end;

procedure TFHIRMapStyler.StyleNeeded;
var
  startState : TMapStylerStyle;
begin
  startState := TMapStylerStyle(LineState);
  if startState in [stGenText..stGenError] then
    scanGen
  else
    ScanGroup;
end;

end.
