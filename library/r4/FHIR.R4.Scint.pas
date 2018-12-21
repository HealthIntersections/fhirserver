unit FHIR.R4.Scint;

interface

uses
  SysUtils, Classes, Graphics,
  ScintEdit,
  FHIR.Support.Base;


type
  // 4 bits
  TMapStylerZone = (
    szBase,
    szMap1,
    szMap2,
    szUses1,
    szUses2,
    szImports,
    szGroup,
    szRule
  );

  TMapStylerStyle = (
    stNull,
    stNoUse,
    stComment,
    stKnownToken,
    stGrammar,
    stString,
    stField
//    stCommentConceptMap,
//    stCommentstGroup,
//    stRule,
//    stStringDoubleQuot,
//    stStringSingleQuote
  );

  TFHIRMapStyler = class(TScintCustomStyler)
  private
    procedure CommitStyle(Zone : TMapStylerZone; Style: TMapStylerStyle);
    procedure scanZone(zone : TMapStylerZone);
    procedure scanBase;
    procedure scanMap1;
    procedure scanMap2;
    procedure scanUses1;
    procedure scanUses2;
    procedure scanImports;
    procedure scanGroup;
    procedure scanRule;
    procedure scanComment(zone : TMapStylerZone);
    procedure scanString(zone : TMapStylerZone; ch : AnsiChar);
    procedure scanSuspectString(zone : TMapStylerZone; ch, endchar : AnsiChar);
    procedure scanToken(zone: TMapStylerZone); overload;
    procedure scanToken(zone: TMapStylerZone; token : String); overload;
    procedure scanGrammar(zone: TMapStylerZone; token : String); overload;
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
  EolnChars = [#13, #10];
  ALL_ZONES = [szBase, szMap1, szMap2, szUses1, szUses2, szImports, szGroup, szRule];


{ TFHIRMapStyler }

constructor TFHIRMapStyler.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TFHIRMapStyler.Destroy;
begin
  inherited;
end;

procedure TFHIRMapStyler.CommitStyle(Zone : TMapStylerZone; Style: TMapStylerStyle);
begin
  LineState := Ord(Style) + (Ord(Zone) shl 4);
  inherited CommitStyle(LineState);
end;

procedure TFHIRMapStyler.GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes);
var
  st : TMapStylerStyle;
  zone : TMapStylerZone;
begin
  st := TMapStylerStyle(style and $F);
  zone := TMapStylerZone(style shr 4);
  if (zone in ALL_ZONES) then
  begin
    case st of
      stNull, stNoUse: ; // nothing.
      stComment:
        begin
        Attributes.FontStyle := [fsItalic];
        Attributes.ForeColor := clGreen;
        end;
      stKnownToken :
        begin
        Attributes.FontStyle := [fsBold];
        Attributes.ForeColor := clPurple;
        end;
      stString :
        begin
        Attributes.FontStyle := [];
        Attributes.ForeColor := clNavy;
        end;
      stField :
        begin
        Attributes.FontStyle := [];
        Attributes.ForeColor := clMaroon;
        end;
      stGrammar :
        begin
        Attributes.FontStyle := [fsBold];
        Attributes.ForeColor := clBlack;
        end;
    end;
  end;

//  case zone of
//    szBase, stNoUse : Attributes.BackColor := clWhite;
//    szMap1 : Attributes.BackColor := clAqua;
//    szMap2 : Attributes.BackColor := clAqua;
//    szUses : Attributes.BackColor := clYellow;
//    szImports : Attributes.BackColor := clCream;
//    szGroup : Attributes.BackColor := clLime;
//    szRule : Attributes.BackColor := clSilver;
//  end;
end;

function TFHIRMapStyler.LineTextSpans(const S: TScintRawString): Boolean;
begin
  result := false;
end;

procedure TFHIRMapStyler.StyleNeeded;
var
  startState : TMapStylerStyle;
  zone : TMapStylerZone;
begin
  startState := TMapStylerStyle(LineState and $f);
  zone := TMapStylerZone(LineState shr 4);
  case startState of
    stNull, stKnownToken: scanZone(zone);
    stComment: scanComment(zone);
    stString, stField: scanString(zone, '"');
    stGrammar: scanZone(zone);
  else
    raise ELibraryException.create('Error Message');
  end;
  inherited;
end;

procedure TFHIRMapStyler.scanBase;
begin
  ConsumeChars(WhitespaceChars);
  CommitStyle(szBase, stNull);
  if CurCharIs('/') and NextCharIs('/') then
    ScanComment(szBase)
  else if hasToken('map') then
    scanMap1
  else if hasToken('uses') then
    scanUses1
  else if hasToken('imports') then
    scanImports
  else if hasToken('group') then
    scanGroup
  else
  begin
    ConsumeAllRemaining;
    CommitStyle(szBase, stNull);
  end;
end;

procedure TFHIRMapStyler.scanComment(zone : TMapStylerZone);
begin
  ConsumeCharsNot(EolnChars);
  CommitStyle(zone, stComment);
  scanZone(zone);
end;

procedure TFHIRMapStyler.scanGrammar(zone: TMapStylerZone; token: String);
var
  i : integer;
begin
  for i := 1 to token.Length do
    ConsumeChar();
  CommitStyle(zone, stGrammar);
end;

procedure TFHIRMapStyler.scanGroup;
begin
  while not EndOfLine do
  begin
    ConsumeChars(WhitespaceChars);
    CommitStyle(szGroup, stNull);
    if hasGrammar('//') then
      scanComment(szGroup)
    else if hasToken('group') or hasToken('source') or hasToken('target') or hasToken('extends') then
      scanToken(szGroup)
    else if hasGrammar('<<types>>') then
      scanToken(szGroup, '<<types>>')
    else if hasGrammar('<<type+>>') then
      scanToken(szGroup, '<<type+>>')
    else if hasGrammar('(') then
      scanGrammar(szGroup, ')')
    else if hasGrammar(')') then
      scanGrammar(szGroup, ')')
    else if hasGrammar(',') then
      scanGrammar(szGroup, ',')
    else if hasGrammar('{') then
    begin
      scanGrammar(szRule, '{');
      scanRule;
    end
    else
    begin
      ConsumeCharsNot(WhitespaceChars+['(', ',', ')']);
      CommitStyle(szGroup, stNull);
    end;
  end;
end;

procedure TFHIRMapStyler.scanImports;
begin
  if not CurCharIs('"') then
  begin
    ConsumeCharsNot(WhitespaceChars);
    CommitStyle(szImports, stKnownToken);
  end;
  ConsumeChars(WhitespaceChars);
  CommitStyle(szImports, stNull);
  if CurCharIs('"') then
  begin
    scanString(szImports, '"');
    ConsumeAllRemaining;
    CommitStyle(szBase, stNull);
  end
  else
    CommitStyle(szImports, stNull);
end;

procedure TFHIRMapStyler.scanMap1;
begin
  if not CurCharIs('"') then
  begin
    ConsumeCharsNot(WhitespaceChars);
    CommitStyle(szMap1, stKnownToken);
  end;
  ConsumeChars(WhitespaceChars);
  CommitStyle(szMap1, stNull);
  if CurCharIs('"') then
    scanString(szMap1, '"');
  ConsumeChars(WhitespaceChars);
  CommitStyle(szMap1, stNull);
  if hasGrammar('=') then
    scanMap2
  else
  begin
    ConsumeAllRemaining;
    CommitStyle(szMap1, stNull);
  end;
end;

procedure TFHIRMapStyler.scanMap2;
begin
  if not CurCharIs('"') then
  begin
    ConsumeCharsNot(WhitespaceChars);
    CommitStyle(szMap2, stKnownToken);
  end;
  ConsumeChars(WhitespaceChars);
  CommitStyle(szMap2, stNull);
  if CurCharIs('"') then
  begin
    scanString(szMap2, '"');
    CommitStyle(szBase, stNull);
  end
  else
  begin
    ConsumeAllRemaining;
    CommitStyle(szMap2, stNull);
  end;
end;

procedure TFHIRMapStyler.scanRule;
begin
  CommitStyle(szRule, stNull);
  while not EndOfLine do
  begin
    ConsumeChars(WhitespaceChars);
    CommitStyle(szRule, stNull);
    if hasGrammar('//') then
      scanComment(szRule)
    else if CurCharIs('"') then
      scanSuspectString(szRule, '"', ';')
    else if CurCharIs('''') then
      scanString(szRule, '''')
    else if hasGrammar('->') then
      scanToken(szGroup, '->')
    else if hasToken('then') or hasToken('check') or hasToken('where') or hasToken('log') or hasToken('as') then
      scanToken(szRule)
    else if hasGrammar('(') then
      scanGrammar(szRule, ')')
    else if hasGrammar(')') then
      scanGrammar(szRule, ')')
    else if hasGrammar(',') then
      scanGrammar(szRule, ',')
    else if hasGrammar(';') then
      scanGrammar(szRule, ';')
    else if hasGrammar(':') then
      scanGrammar(szRule, ':')
    else if hasGrammar('..') then
      scanGrammar(szRule, '..')
    else if hasGrammar('}') then
    begin
      scanGrammar(szRule, '}');
      if not CharInSet(NextCharNotInSet(WhitespaceChars), [';', '"']) then // end of group as a whole
      begin
        scanGroup;
        CommitStyle(szGroup, stNull);
      end;
    end
    else if hasGrammar('{') then
    begin
      scanGrammar(szRule, '{');
      scanRule;
    end
    else
    begin
      ConsumeCharsNot(WhitespaceChars+['(', ',', ')']);
      CommitStyle(szRule, stNull);
    end;
  end;

end;

procedure TFHIRMapStyler.scanString(zone: TMapStylerZone; ch : AnsiChar);
begin
  ConsumeChar(ch);
  ConsumeCharsNot([ch]);
  ConsumeChar(ch);
  CommitStyle(zone, stString);
end;

procedure TFHIRMapStyler.scanSuspectString(zone: TMapStylerZone; ch, endchar: AnsiChar);
begin
  ConsumeChar(ch);
  ConsumeCharsNot([ch]);
  ConsumeChar(ch);
  if NextCharNotInSet(WhitespaceChars) <> endchar then
    CommitStyle(zone, stField)
  else
    CommitStyle(zone, stString);
end;

procedure TFHIRMapStyler.scanToken(zone: TMapStylerZone; token: String);
var
  i : integer;
begin
  for i := 1 to token.Length do
    ConsumeChar();
  CommitStyle(zone, stKnownToken);
end;

procedure TFHIRMapStyler.scanToken(zone: TMapStylerZone);
begin
  ConsumeChars(['a'..'z', 'A'..'Z', '0'..'9', '_']);
  CommitStyle(zone, stKnownToken);
end;

procedure TFHIRMapStyler.scanUses1;
begin
  if not CurCharIs('"') then
  begin
    ConsumeCharsNot(WhitespaceChars);
    CommitStyle(szUses1, stKnownToken);
  end;
  while not EndOfLine and not hasToken('as') do
  begin
    ConsumeChars(WhitespaceChars);
    CommitStyle(szUses1, stNull);
    if CurCharIs('"') then
      scanString(szUses1, '"')
    else if hasToken('alias') then
      scanToken(szUses1)
    else if not hasToken('as') then
    begin
      ConsumeCharsNot(WhitespaceChars);
      CommitStyle(szUses1, stNull);
    end;
  end;
  if not EndOfLine then
  begin
    scanToken(szUses1);
    scanUses2;
  end;
end;

procedure TFHIRMapStyler.scanUses2;
begin
  ConsumeChars(WhitespaceChars);
  CommitStyle(szMap2, stNull);
  if not EndOfLine then
  begin
    ConsumeCharsNot(WhitespaceChars);
    CommitStyle(szBase, stNull);
  end;
end;

procedure TFHIRMapStyler.scanZone(zone: TMapStylerZone);
begin
  case zone of
    szBase : scanBase;
    szMap1 : scanMap1;
    szMap2 : scanMap2;
    szUses1 : scanUses1;
    szUses2 : scanUses2;
    szImports : scanImports;
    szGroup : scanGroup;
    szRule : scanRule;
  end;
end;

end.
