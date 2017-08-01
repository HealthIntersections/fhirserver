Unit UcumExpressions;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

uses
  SysUtils,
  StringSupport,
  AdvStringBuilders,
  DecimalSupport,
  AdvObjects,
  Ucumhandlers,
  Ucum;

Type
  EUcumException = class (Exception);

  TUcumComponent = class (TAdvObject)
  public
    Function Link : TUcumComponent; Overload;
  End;

  TUcumFactor = class (TUcumComponent)
  private
    FFactor: Integer;
    FAnnotation : String;
  public
    Constructor Create(iFactor : integer); overload;
    Constructor Create(iFactor : integer; Annotation : String); overload;
    Function Link : TUcumFactor; Overload;
    Property Factor : Integer read FFactor write FFactor;
    Property Annotation : String read FAnnotation write FAnnotation;
  End;

  TUcumOperator = (NOOP, MULTIPLICATION, DIVISION);

  TUcumSymbol = class (TUcumComponent)
  private
    FPrefix: TUcumPrefix;
    FUnit_: TUcumUnit;
    FExponent: Integer;
    procedure SetPrefix(const Value: TUcumPrefix);
    procedure SetUnit_(const Value: TUcumUnit);
    procedure SetExponent(const Value: Integer);
  public
    Constructor Create(oUnit_ : TUcumUnit; oPrefix : TUcumPrefix; iExponent : Integer); Overload;
    Destructor Destroy; Override;
    Function Link : TUcumSymbol; Overload;

    Procedure invertExponent;
    Property Unit_ : TUcumUnit read FUnit_ write SetUnit_; // may be Base Unit or DefinedUnit
    Property Prefix : TUcumPrefix read FPrefix write SetPrefix; // only if unit is metric
    Property Exponent : Integer read FExponent write SetExponent;
  End;

  TUcumTerm = class (TUcumComponent)
  Private
    FComponent : TUcumComponent;
    FOperator : TUcumOperator;
    FTerm : TUcumTerm;
    procedure SetComponent(const Value: TUcumComponent);
    procedure SetTerm(const Value: TUcumTerm);
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TUcumTerm; Overload;

    Procedure setTermCheckOp(oTerm : TUcumTerm);

    Property Component : TUcumComponent read FComponent write SetComponent;
    Property Operator : TUcumOperator read FOperator write FOperator;
    Property Term : TUcumTerm read FTerm write SetTerm;
  End;

  TUcumLexerTokenType = (NONE, NUMBER, SYMBOL, SOLIDUS, PERIOD, OPEN, CLOSE, ANNOTATION);

  TUcumLexer = class (TAdvObject)
  private
    Fsource : String;
    Findex : integer;
    Ftoken : String;
    Ftype : TUcumLexerTokenType;
    Fstart : integer;
    Function checkNumber(ch : Char) : boolean;
    Function checkNumberOrSymbol(ch : Char) : boolean;
    Function checkBrackets(ch : Char; inBrackets : boolean) : boolean;
    Function isValidSymbolChar(ch : Char; allowDigits, inSquares: boolean): boolean;
    Function checkAnnotation(ch : Char) : boolean;
    function checkSingle(ch : Char; test : char; atype : TUcumLexerTokenType ) : boolean;
    function nextChar() : char;
    function peekChar() : char;
  public
    Constructor Create(sSource : String);

    procedure consume;
    Property Token : String read FToken;
    Property TokenType : TUcumLexerTokenType read FType;
    procedure error(errMsg : String);
    function getTokenAsInt() : integer;
  End;

  TUcumExpressionParser = class (TAdvObject)
  private
    FModel : TUcumModel;
    FLexer : TUcumLexer;
    function parseSymbol : TUcumSymbol;
    function parseComp : TUcumComponent;
    function ParseTerm(bFirst : Boolean) : TUcumTerm;
  public
    Destructor Destroy; Override;
    class function Parse(oModel : TUcumModel; sExpression : String): TUcumTerm;
  End;


  TUcumExpressionComposer = class (TAdvObject)
  private
    class Procedure composeTerm(oBuilder : TAdvStringBuilder; term : TUcumTerm);
    class Procedure composeComp(oBuilder : TAdvStringBuilder; comp : TUcumComponent);
    class Procedure composeSymbol(oBuilder : TAdvStringBuilder; symbol : TUcumSymbol);
    class Procedure composeFactor(oBuilder : TAdvStringBuilder; comp : TUcumFactor);
    class Procedure composeOp(oBuilder : TAdvStringBuilder; op : TUcumOperator);
  public
    class Function compose(Term : TUcumTerm) : String;
  End;

  TUcumFormalStructureComposer = class (TAdvObject)
  private
    class Procedure composeTerm(oBldr : TAdvStringBuilder; oTerm : TUcumTerm);
    class Procedure composeComp(oBldr : TAdvStringBuilder; oComp : TUcumComponent);
    class Procedure composeSymbol(oBldr : TAdvStringBuilder; oSymbol : TUcumSymbol);
    class Procedure composeFactor(oBldr : TAdvStringBuilder; oFactor : TUcumFactor);
    class Procedure composeOp(oBldr : TAdvStringBuilder; aOperator : TUcumOperator);
  Public
    class Function compose(oTerm : TUcumTerm) : String;
  End;




  TUcumCanonical = class (TAdvObject)
  private
    FValue : TSmartDecimal;
    FUnit : TUcumTerm;
    Procedure SetUnit(Value : TUcumTerm);
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    procedure multiplyValue(i : TSmartDecimal); overload;
    procedure multiplyValue(i : integer); overload;
    procedure divideValue(i : TSmartDecimal); overload;
    procedure divideValue(i : integer); overload;

    Property Value : TSmartDecimal read FValue write FValue;
    Property Unit_ : TUcumTerm read FUnit write SetUnit;
  End;

  TUcumConverter = class (TAdvObject)
  private
    Fmodel : TUcumModel;
    Fhandlers : TUcumRegistry;
    Fone : TUcumFactor;

    function SortTerms(oTerm : TUcumTerm): Boolean; Overload;
    Function SortTerms(oCan : TUcumCanonical): Boolean; Overload;

    Function  convertTerm(oTerm: TUcumTerm) : TUcumCanonical;
    Procedure debug(sState : String; oUnit : TUcumTerm);
    Function  getEndTerm(oTerm: TUcumTerm) : TUcumTerm;
    Function  removeDuplicateComponents(oUnit : TUcumTerm) : TUcumTerm;
    Function  findDuplicateCompOwner(oTerm: TUcumTerm; oComp : TUcumSymbol) : TUcumTerm;
    Procedure flipExponents(oTerm: TUcumTerm);
    Function  convertComp(oContext : TUcumCanonical; oComp : TUcumComponent) : TUcumComponent;
    Function  convertSymbol(oContext : TUcumCanonical; oComp : TUcumSymbol) : TUcumComponent;
    Procedure applyExponent(oTerm: TUcumTerm; iExponent : Integer);
  public
    Constructor Create(oModel : TUcumModel; oHandlers : TUcumRegistry);
    Destructor Destroy; Override;
    Function convert(oTerm: TUcumTerm) : TUcumCanonical;
  End;

Const
  NO_CHAR = #0;

Const
  CODES_UcumLexerTokenType : Array[TUcumLexerTokenType] of String = ('NONE', 'NUMBER', 'SYMBOL', 'SOLIDUS', 'PERIOD', 'OPEN', 'CLOSE', 'ANNOTATION');

Implementation

{ TUcumFactor }

constructor TUcumFactor.Create(iFactor: integer);
begin
  Inherited Create;
  FFactor := iFactor;
end;

constructor TUcumFactor.Create(iFactor: integer; Annotation : String);
begin
  Inherited Create;
  FFactor := iFactor;
  self.Annotation := Annotation;
end;

function TUcumFactor.Link: TUcumFactor;
begin
  result := TUcumFactor(Inherited Link);
end;

{ TUcumSymbol }

constructor TUcumSymbol.Create(oUnit_: TUcumUnit; oPrefix: TUcumPrefix; iExponent: Integer);
begin
  inherited Create;
  Unit_ := oUnit_.Link;
  Prefix := oPrefix.Link;
  FExponent := iExponent;
end;

destructor TUcumSymbol.Destroy;
begin
  FPrefix.Free;
  FUnit_.Free;
  inherited;
end;

procedure TUcumSymbol.invertExponent;
begin
  FExponent := -FExponent;
end;

function TUcumSymbol.Link: TUcumSymbol;
begin
  result := TUcumSymbol(Inherited Link);
end;

procedure TUcumSymbol.SetExponent(const Value: Integer);
begin
  FExponent := Value;
end;

procedure TUcumSymbol.SetPrefix(const Value: TUcumPrefix);
begin
  FPrefix.Free;
  FPrefix := Value;
end;

procedure TUcumSymbol.SetUnit_(const Value: TUcumUnit);
begin
  FUnit_.Free;
  FUnit_ := Value;
end;

{ TUcumTerm }

constructor TUcumTerm.Create;
begin
  inherited;

end;

destructor TUcumTerm.Destroy;
begin
  FTerm.Free;
  FComponent.Free;
  inherited;
end;

function TUcumTerm.Link: TUcumTerm;
begin
  result := TUcumTerm(inherited Link);
end;

procedure TUcumTerm.SetComponent(const Value: TUcumComponent);
begin
  FComponent.Free;
  FComponent := Value;
end;

procedure TUcumTerm.SetTerm(const Value: TUcumTerm);
begin
  FTerm.Free;
  FTerm := Value;
end;


procedure TUcumTerm.setTermCheckOp(oTerm: TUcumTerm);
begin
  Term := oTerm;
  if (term = nil) Then
    Operator := NOOP;
end;

{ TUcumExpressionParser }

class function TUcumExpressionParser.Parse(oModel : TUcumModel; sExpression : String): TUcumTerm;
var
  oSelf : TUcumExpressionParser;
Begin
  oSelf := TUcumExpressionParser.Create;
  try
    oSelf.FModel := oModel.Link;
    oSelf.FLexer := TUcumLexer.Create(sExpression);
    result := oSelf.ParseTerm(true);
  Finally
    oSelf.Free;
  End;
End;


Destructor TUcumExpressionParser.Destroy;
Begin
  FLexer.Free;
  FModel.Free;
  Inherited;
End;

function TUcumExpressionParser.ParseTerm(bFirst : Boolean) : TUcumTerm;
Begin
  result := TUcumTerm.Create;
  Try
    if bFirst and (FLexer.TokenType = NONE) Then
      result.Component := TUcumFactor.Create(1)
//    else if (FLexer.TokenType = ANNOTATION) Then
//      result.Component := TUcumFactor.Create(1, FLexer.Ftoken)
    else if (FLexer.TokenType = SOLIDUS) Then
    Begin
      result.Operator := DIVISION;
      FLexer.consume();
      result.Term := parseTerm(false);
    End
    else
    Begin
      result.Component := parseComp;
      if (FLexer.TokenType <> NONE) And (FLexer.TokenType <> CLOSE) Then
      Begin
        if (FLexer.TokenType = SOLIDUS) Then
        begin
          result.Operator := DIVISION;
          FLexer.consume();
        end
        else if (FLexer.TokenType = PERIOD) Then
        begin
          result.Operator := MULTIPLICATION;
          FLexer.consume();
        end
        else if (FLexer.TokenType = ANNOTATION) then
          result.Operator := MULTIPLICATION // implicit
        else
          FLexer.error('Expected "/" or "."');
        result.Term := parseTerm(false);
      End;
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;

function TUcumExpressionParser.parseComp : TUcumComponent;
Begin
  result := nil;
  if (FLexer.TokenType = NUMBER) Then
  Begin
    result := TUcumFactor.Create(FLexer.getTokenAsInt());
    FLexer.consume();
  End
  else if (FLexer.TokenType = ANNOTATION) Then
  begin
    result := TUcumFactor.Create(1, FLexer.Ftoken);
    FLexer.consume();
  end
  else if (FLexer.TokenType = SYMBOL) Then
    result := parseSymbol
  else if (FLexer.TokenType = NONE) Then
    FLexer.error('unexpected end of expression looking for a symbol or a number')
  else if (FLexer.TokenType = OPEN) Then
  Begin
    FLexer.consume;
    result := ParseTerm(true);
    if (FLexer.TokenType = CLOSE) Then
      FLexer.consume
    Else
      FLexer.error('Unexpected Token Type '''+CODES_UcumLexerTokenType[FLexer.TokenType]+''' looking for a close backet');
  End
  else
    FLexer.error('Unexpected Token Type '''+CODES_UcumLexerTokenType[FLexer.TokenType]+''' looking for a symbol or a number');
End;

function TUcumExpressionParser.parseSymbol : TUcumSymbol;
var
  sym : String;
  selected : TUcumPrefix;
  unit_ : TUcumUnit;
  i : Integer;
Begin
  result := TUcumSymbol.Create;
  Try
    sym := FLexer.Token;

    // now, can we pick a prefix that leaves behind a metric Unit_?
    selected := nil;
    Unit_ := nil;
    for i := 0 to Fmodel.Prefixes.Count - 1 Do
    Begin
      if StringStartsWithSensitive(sym, Fmodel.Prefixes[i].Code) Then
      Begin
        Unit_ := Fmodel.getUnit(Copy(sym, Length(Fmodel.Prefixes[i].Code)+1, $FF));
	if (Unit_ <> nil) and ((Unit_.Kind = UcumBASEUNIT) or TUcumDefinedUnit(Unit_).metric) Then
        Begin
          selected := Fmodel.Prefixes[i];
	  break;
	End;
      End;
    End;

    if (selected <> nil) Then
    Begin
      result.Prefix := selected.Link;
      result.Unit_ := Unit_.Link;
    End
    else
    Begin
      Unit_ := Fmodel.getUnit(sym);
      if (Unit_ <> nil) Then
        result.Unit_ := Unit_.Link
      else if (sym <> '1') Then
        FLexer.error('The unit "'+sym+'" is unknown');
    End;

    FLexer.consume();
    if (FLexer.TokenType = NUMBER) Then
    Begin
      result.Exponent := FLexer.getTokenAsInt;
      FLexer.consume();
    End
    else
      result.Exponent := 1;

    result.Link;
  Finally
    result.Free;
  End;
End;




{ TUcumLexer }

Constructor TUcumLexer.Create(sSource : String);
Begin
  Inherited Create;
  Fsource := ssource;
  Findex := 1;
  consume();
End;

procedure TUcumLexer.consume;
var
  ch : Char;
Begin
  Ftoken := '';
  Ftype := NONE;
  Fstart := Findex;
  if (Findex <= Length(FSource)) Then
  Begin
    ch := nextChar();
    if (not (checkSingle(ch, '/', SOLIDUS) or
        checkSingle(ch, '.', PERIOD) or
        checkSingle(ch, '(', OPEN) or
        checkSingle(ch, ')', CLOSE) or
        checkAnnotation(ch) or
        checkNumber(ch) or
        checkNumberOrSymbol(ch))) Then
      raise Exception.Create('Error processing Unit_ "'+FSource+'": unexpected character "'+ch+'" at position '+IntToStr(FStart));
  End;
End;

Function TUcumLexer.checkNumber(ch : Char) : boolean;
Begin
  if (ch = '+') or (ch = '-') Then
  Begin
    FToken := ch;
    ch := peekChar();
    while ((ch >= '0') and (ch <= '9')) Do
    Begin
      FToken := FToken + ch;
      inc(FIndex);
      ch := peekChar();
    End;
    if (Length(FToken) = 1) Then
      raise Exception.Create('Error processing Unit_"'+FSource+'": unexpected character "'+ch+'" at position '+IntToStr(FStart)+': a + or - must be followed by at least one digit');
    Ftype := NUMBER;
    result := true;
  End
  else
    result := false;
End;

Function TUcumLexer.checkNumberOrSymbol(ch : Char) : boolean;
var
  isSymbol, inBrackets : boolean;
Begin
  isSymbol := false;
  inBrackets := false;
  if (isValidSymbolChar(ch, true, false)) Then
  Begin
    FToken := ch;
    isSymbol := isSymbol or not ((ch >= '0') and (ch <= '9'));
    inBrackets := checkBrackets(ch, inBrackets);
    ch := peekChar();
    inBrackets := checkBrackets(ch, inBrackets);
    while (isValidSymbolChar(ch, not isSymbol or inBrackets, inBrackets)) Do
    Begin
      FToken := FToken + ch;
      isSymbol := isSymbol or ((ch <> NO_CHAR) and not  ((ch >= '0') and (ch <= '9')));
      inc(Findex);
      ch := peekChar();
      inBrackets := checkBrackets(ch, inBrackets);
    End;
    if (isSymbol) Then
      FType := SYMBOL
    else
      FType := NUMBER;
    result := true;
  End
  else
    result := false;
End;


Function TUcumLexer.checkBrackets(ch : Char; inBrackets : boolean) : boolean;
Begin
  result := false;
  if (ch = '[') Then
    if (inBrackets) Then
      Error('Nested [')
    else
     result := true
  else if (ch = ']') Then
    if (not inBrackets) Then
      error('] without [')
    else
      result := false
  else
    result := inBrackets;
End;

Function TUcumLexer.isValidSymbolChar(ch : Char; allowDigits, inSquares : boolean): boolean;
Begin
  result := ((allowDigits) and (ch >= '0') and (ch <= '9')) or (inSquares or ((ch >= 'a') and (ch <= 'z')) or ((ch >= 'A') and (ch <= 'Z')) or
       (ch = '[') or (ch = ']') or (ch = '%') or (ch = '*') or (ch = '^') or (ch = '''') or
       (ch = '"') or (ch = '_')) or (inSquares and (ch = '.'));
End;

Function TUcumLexer.checkAnnotation(ch : Char) : boolean;
var
  s : String;
Begin
  if (ch = '{') Then
  Begin
    s := '';
    while (ch <> '}') Do
    Begin
      ch := nextChar();
      if ord(ch) > 255 then
        raise Exception.Create('Error processing Unit_"'+FSource+'": annotation contains non-ascii characters');
      if (ch = #0) Then
        raise Exception.Create('Error processing Unit_"'+FSource+'": unterminated annotation');
      if (ch <> '}') then
        s := s + ch;
    End;
    // got to the end of the annotation - need to do it again
    FToken := s;
    FType := ANNOTATION;
    result := true;
  End
  else
    result := false;
End;

function TUcumLexer.checkSingle(ch : Char; test : char; atype : TUcumLexerTokenType ) : boolean;
Begin
  if (ch = test) Then
  Begin
    FToken := ch;
    FType := aType;
    result := true;
  End
  Else
    result := false;
End;

function TUcumLexer.nextChar() : char;
Begin
  if Findex <= Length(FSource) Then
    result := FSource[Findex]
  else
    result := NO_CHAR;
  inc(Findex);
End;

function TUcumLexer.peekChar() : char;
Begin
  if Findex <= Length(FSource) Then
    result := FSource[Findex]
  else
    result := NO_CHAR;
End;


Procedure TUcumLexer.error(errMsg : String);
Begin
  raise EUcumException.Create('Error processing Unit: '''+FSource+''': '+ errMsg +' at character '+IntToStr(FStart));
End;

Function TUcumLexer.getTokenAsInt() : Integer;
Begin
  if FToken[1] = '+' Then
    result := StrToInt(Copy(FToken, 2, $FF))
  else
    result := StrToInt(FToken);
End;


class Function TUcumExpressionComposer.compose(Term : TUcumTerm) : String;
var
  oBuilder : TAdvStringBuilder;
begin
  if (Term = nil) Then
    result := '1'
  Else
  Begin
    oBuilder := TAdvStringBuilder.Create;
    try
      composeTerm(oBuilder, Term);
      result := oBuilder.AsString;
    Finally
      oBuilder.Free;
    End;
  End;
End;


class Procedure TUcumExpressionComposer.composeTerm(oBuilder : TAdvStringBuilder; Term : TUcumTerm);
Begin
  if (Term.Component <> nil) Then
    composeComp(oBuilder, Term.Component);
  if (Term.Operator <> NOOP) Then
    composeOp(oBuilder, Term.Operator);
  if (Term.Term <> nil) Then
    composeTerm(oBuilder, Term.Term);
End;


class Procedure TUcumExpressionComposer.composeComp(oBuilder : TAdvStringBuilder; comp : TUcumComponent);
Begin
  if (comp is TUcumFactor) Then
    composeFactor(oBuilder, TUcumFactor(comp))
  else if (comp is TUcumSymbol) Then
    composeSymbol(oBuilder, TUcumSymbol(comp))
  else if (comp is TUcumTerm) Then
  Begin
    oBuilder.append('(');
    composeTerm(oBuilder, TUcumTerm(comp));
    oBuilder.append(')');
  End
  else
    oBuilder.append('?');
End;

class Procedure TUcumExpressionComposer.composeSymbol(oBuilder : TAdvStringBuilder; symbol : TUcumSymbol);
Begin
  if (symbol.Prefix <> nil) Then
    oBuilder.append(symbol.Prefix.Code);
  oBuilder.append(symbol.Unit_.Code);
  if (symbol.Exponent <> 1) Then
    oBuilder.append(inttostr(symbol.Exponent));
End;

class Procedure TUcumExpressionComposer.composeFactor(oBuilder : TAdvStringBuilder; comp : TUcumFactor);
begin
  oBuilder.append(inttostr(comp.Factor));
End;

class Procedure TUcumExpressionComposer.composeOp(oBuilder : TAdvStringBuilder; op : TUcumOperator);
begin
  if (op = DIVISION) Then
    oBuilder.append('/')
  else
    oBuilder.append('.');
End;

{ TUcumConverter }

constructor TUcumConverter.Create(oModel: TUcumModel; oHandlers: TUcumRegistry);
begin
  Inherited Create;
  Fmodel := oModel;
  Fhandlers := ohandlers;
  FOne := TUcumFactor.Create(1);
end;

destructor TUcumConverter.Destroy;
begin
  Fmodel.Free;
  Fhandlers.Free;
  FOne.Free;
  inherited;
end;

procedure TUcumConverter.applyExponent(oTerm: TUcumTerm; iExponent: Integer);
var
  sym : TUcumSymbol;
begin
  if (oTerm <> nil) Then
  Begin
    if (oTerm.Component <> nil) Then
    Begin
      if (oTerm.Component is TUcumTerm) Then
        applyExponent(TUcumTerm(oTerm.Component), iExponent)
      else if (oTerm.Component is TUcumSymbol) Then
      Begin
        sym := TUcumSymbol(oTerm.Component);
        sym.Exponent := sym.Exponent * iExponent;
      End;
    End;
    applyExponent(oTerm.Term, iExponent);
  End;
end;


function TUcumConverter.convert(oTerm: TUcumTerm): TUcumCanonical;
var
  bSorted : Boolean;
begin
  result := convertTerm(oTerm);
  repeat
    bSorted := SortTerms(result);
  until not bSorted;
end;

function TUcumConverter.convertComp(oContext: TUcumCanonical; oComp: TUcumComponent): TUcumComponent;
var
  t : TUcumCanonical;
begin
  if oComp is TUcumTerm Then
  Begin
    t := convertTerm(TUcumTerm(oComp));
    oContext.multiplyValue(t.Value);
    result := t.Unit_.Link;
  End else if oComp is TUcumFactor Then
  Begin
    oContext.multiplyValue(TUcumFactor(oComp).Factor);
    result := Fone.Link; // nothing to convert
  End else if oComp is TUcumSymbol Then
    result := convertSymbol(oContext, TUcumSymbol(oComp))
  else
    raise Exception.Create('unknown TUcumComponent type '+oComp.ClassName);
end;

function TUcumConverter.convertSymbol(oContext: TUcumCanonical; oComp: TUcumSymbol): TUcumComponent;
var
  sym : TUcumSymbol;
  unit_ : TUcumDefinedUnit;
  u : string;
  oCanonical : TUcumTerm;                                                                                                              
  ret : TUcumTerm;
  i : integer;
  t1 : TUcumCanonical;
  t : TUcumComponent;
begin
  if oComp.Prefix <> nil Then
  Begin
    //oContext.MultiplyValue(oComp.Prefix.Value);
    for i := 1 to abs(oComp.Exponent) do
      if oComp.Exponent < 0 then
        oContext.DivideValue(oComp.Prefix.Value)
      Else
        oContext.MultiplyValue(oComp.Prefix.Value);
  End;

  if oComp.Unit_.Kind = UcumBASEUNIT Then
  Begin
    sym := TUcumSymbol.Create;
    sym.Unit_ := oComp.Unit_.Link;
    sym.Exponent := oComp.Exponent;
    result := sym;
  End
  else
  Begin
    unit_ := TUcumDefinedUnit(oComp.Unit_);
    u := unit_.Value.unit_;
    if unit_.isSpecial Then
    Begin
      if not Fhandlers.ExistsByName(unit_.Code) Then
        raise Exception.Create('Not handled yet (special unit: '+unit_.Code+')')
      else
      Begin
        u := Fhandlers.HandlerByCode[unit_.code].Units;
        oContext.multiplyValue(Fhandlers.HandlerByCode[unit_.code].Value);
      End;
    End
    else
    Begin
      for i := 1 to abs(oComp.Exponent) do
        if oComp.Exponent < 0 then
          oContext.DivideValue(unit_.Value.Value)
        Else
          oContext.MultiplyValue(unit_.Value.Value);
//      oContext.multiplyValue(unit_.Value.Value);
    End;
    oCanonical := TUcumExpressionParser.parse(FModel, u);
    Try
      if (oCanonical.Component <> nil) and (oCanonical.Operator <> NOOP) And (oCanonical.Term = nil) Then
      Begin
        result := convertComp(oContext, oCanonical.Component);
        if (oComp.Exponent <> 1) Then
          if result is TUcumFactor Then
            TUcumFactor(result).Factor := oComp.Exponent + TUcumFactor(result).Factor
          else if result is TUcumSymbol Then
            TUcumSymbol(result).Exponent := oComp.Exponent * TUcumSymbol(result).Exponent
          else if result is TUcumTerm Then
            applyExponent(TUcumTerm(result), oComp.Exponent)
          else
            raise Exception.Create('unknown TUcumComponent type '+oComp.ClassName);
      End
      else
      Begin
        t1 := convertTerm(oCanonical);
        Try
          ret := t1.Unit_;
          result := ret.Link;
          if (oComp.Exponent = -1) and (ret.Component <> nil) and (ret.Operator <> NOOP) and (ret.Term <> nil) and (ret.Term.Component <> nil)
              and (ret.Term.Operator = NOOP) And (ret.Term.Term = nil) Then
          Begin
            t := ret.Term.Component.Link;
            ret.Term.Component := ret.Component.Link;
            ret.Component := t.Link;
            oContext.divideValue(t1.Value);
          End
          else if oComp.Exponent <> 1 Then
          Begin
            assert(oComp.Exponent <> 0);
            for i := 1 to abs(oComp.Exponent) Do
              if oComp.Exponent < 0 Then
                  oContext.DivideValue(t1.Value)
                else
                  oContext.multiplyValue(t1.Value);
            // what we have to do is push the exponent into the all the symbols contained herein
            applyExponent(ret, oComp.Exponent);
          End
          else
            oContext.multiplyValue(t1.Value);
        finally
          t1.Free;
        End;
      End;
    Finally
      oCanonical.Free;
    End;
  End;
end;

function TUcumConverter.convertTerm(oTerm: TUcumTerm): TUcumCanonical;
var
  t : TUcumCanonical;
  end_ : TUcumTerm;
begin
  result := TUcumCanonical.Create();
  Try
    result.FValue := TSmartDecimal.valueOf(1);
    result.FValue.Precision := 24; // there's no question about that precision
    result.FUnit := TUcumTerm.Create;
    if (oTerm.Component <> nil) Then
      Result.Unit_.Component := convertComp(Result, oTerm.Component);
    if (oTerm.Operator <> NOOP) Then
      Result.Unit_.Operator := oTerm.Operator;
    if (oTerm.Term <> nil) Then
    Begin
      t := convertTerm(oTerm.Term);
      Try
        if (result.Unit_.Operator = DIVISION) and (t.Unit_ <> nil) Then
        Begin
          debug('going to flip @ '+result.value.AsDecimal+'/'+t.value.AsDecimal, t.Unit_);
          result.Value := Result.Value.Divide(t.Value);
          Result.Unit_.Operator := MULTIPLICATION;
          flipExponents(t.Unit_);
          Result.Unit_.setTermCheckOp(t.Unit_.Link);
          debug('flipped @ -> '+result.value.AsDecimal, result.Unit_);
        End
        Else
        Begin
          result.Value := Result.Value.Multiply(t.Value);
          if (t.Unit_ <> Nil) Then
            Result.Unit_.setTermCheckOp(t.Unit_.Link)
          else
            Result.Unit_.Operator := NOOP;
        End;
      finally
        t.Free;
      End;
    End;

    // normalise
    debug('normalise', Result.Unit_);
    if (Result.Unit_.Operator <> NOOP) And (Result.Unit_.Operator = DIVISION) Then
    Begin
      assert(false);
//      Result.Unit_.Operator := MULTIPLICATION;
//      flipExponents(Result.Unit_.Term);
//      debug('flipped', Result.Unit_);
    End;

    if (Result.Unit_.Component = nil) or (Result.Unit_.Component = Fone) Then
    Begin
      Result.Unit_ := Result.Unit_.Term.Link;
      debug('trimmed', Result.Unit_);
    End;

    // everything in scope is a multiplication operation. If oComp is a TUcumTerm, then
    // we are going to tack our TUcumTerm on the end of that TUcumTerm as a multiplication, and
    // make oComp our TUcumTerm

    if (Result.Unit_ <> Nil) And (Result.Unit_.Component <> nil) And (Result.Unit_.Component is TUcumTerm) Then
    Begin
      end_ := getEndTerm(TUcumTerm(Result.Unit_.Component));
      assert(end_.Operator = NOOP);
      end_.Operator := MULTIPLICATION;
      end_.setTermCheckOp(Result.Unit_.Term.Link);
      Result.Unit_ := TUcumTerm(Result.Unit_.Component).Link;
      debug('reorged', Result.Unit_);
    End;

    if (Result.Unit_ <> Nil) And ((Result.Unit_.Component = nil) or (Result.Unit_.Component = Fone)) Then
    Begin
     Result.Unit_ := Result.Unit_.Term.link;
     debug('trimmed', Result.Unit_);
    End;

    // now we have a linear list of terms, each with one TUcumComponent.
    // we scan through the list looking for common components to factor out
    // we have to scan into the list because we may have deep duplicates
    // from the previous flattening operation. we also remove anything that's
    // ended up with an exponent of 0 during this operation
    if (Result.Unit_ <> Nil) Then
      Result.Unit_ := removeDuplicateComponents(Result.Unit_).Link;

    if (Result.Unit_ <> Nil) And (Result.Unit_.Term = nil) Then
      Result.Unit_.Operator := NOOP;

    debug('norm finished', Result.Unit_);
//  System.out.println("value: "+Result.Value.toPlainString()+"; units: "+new ExpResultsionComposer().compose(Result.Unit_));
    Result.Link;
  Finally
    result.Free;
  End;
end;

procedure TUcumConverter.debug(sState: String; oUnit: TUcumTerm);
begin
//  writeln(sstate+': '+TUcumExpressionComposer.compose(oUnit));
end;

function TUcumConverter.findDuplicateCompOwner(oTerm: TUcumTerm; oComp: TUcumSymbol): TUcumTerm;
var
  sym : TUcumSymbol;
begin
  result := nil;
  if (oTerm <> nil) Then
  Begin
    if (oTerm.Component is TUcumSymbol) Then
    Begin
       sym := TUcumSymbol(oTerm.Component);
       if (sym.Prefix = oComp.Prefix) And (// i.e. nil
                                          sym.Unit_ = oComp.Unit_) Then
         result := oTerm
    End;
    if result = nil Then
      result := findDuplicateCompOwner(oTerm.Term, oComp);
  End;
end;

procedure TUcumConverter.flipExponents(oTerm: TUcumTerm);
begin
  if (oTerm.Component is TUcumSymbol) Then
    TUcumSymbol(oTerm.Component).invertExponent;

  if (oTerm.Term <> nil) Then
    flipExponents(oTerm.Term);
end;

function TUcumConverter.getEndTerm(oTerm: TUcumTerm): TUcumTerm;
begin
  if (oTerm.Term <> nil) Then
    result := getEndTerm(oTerm.Term)
  else
    result := oTerm;
end;

function TUcumConverter.removeDuplicateComponents(oUnit: TUcumTerm): TUcumTerm;
var
  symO, symI : TUcumSymbol;
  inner : TUcumTerm;
begin
  if (ounit = nil) Then
    result := nil                
  Else
  Begin
   assert(oUnit.Component is TUcumSymbol); // because that should be all that's left
   symO := TUcumSymbol(oUnit.Component);
   inner := findDuplicateCompOwner(oUnit.Term, symO);
   if (inner <> nil) Then
   Begin
     symI := TUcumSymbol(inner.Component);
     symI.Exponent := symI.Exponent+symO.Exponent;
     result := removeDuplicateComponents(oUnit.Term);
   End
   Else if (symO.Exponent = 0) Then
     result := removeDuplicateComponents(oUnit.Term)
   Else
   Begin
     oUnit.setTermCheckOp(removeDuplicateComponents(oUnit.Term).Link);
     result := oUnit;
   End;
  End;
end;

function TUcumConverter.SortTerms(oTerm : TUcumTerm): Boolean;
var
  t1,t2 : TUcumTerm;
begin
  if oTerm.Term.Term = nil then
    result := false
  Else
  begin
    assert(oTerm.Term.Operator = MULTIPLICATION);
    assert(oTerm.Term.Component is TUcumSymbol);
    assert(oTerm.Term.Term.Component is TUcumSymbol);
    if TUcumSymbol(oTerm.Term.Component).FUnit_.code > TUcumSymbol(oTerm.Term.Term.Component).FUnit_.code Then
    Begin                
      result := true;
      t1 := oTerm.Term.Link;
      t2 := oTerm.Term.Term.Link;
      t1.Term := t2.Term.Link;
      oTerm.Term := t2;
      oTerm.Term.Term := t1;
      oTerm.Term.Operator := MULTIPLICATION;
      if oTerm.Term.Term.Term <> nil Then
        oTerm.Term.Term.Operator := MULTIPLICATION
      Else
        oTerm.Term.Term.Operator := NOOP;
    End
    else
      result := SortTerms(oTerm.Term);
  End;
End;

function TUcumConverter.SortTerms(oCan : TUcumCanonical): Boolean;
var
  t1,t2 : TUcumTerm;
begin
  if (oCan.Unit_ = nil) or (oCan.Unit_.Term = nil) then
    result := false
  Else
  begin
    debug('before sort', oCan.Unit_);
    assert(oCan.Unit_.Operator = MULTIPLICATION);
    assert(oCan.Unit_.Component is TUcumSymbol);
    assert(oCan.Unit_.Term.Component is TUcumSymbol);
    if TUcumSymbol(oCan.Unit_.Component).FUnit_.code > TUcumSymbol(oCan.Unit_.Term.Component).FUnit_.code Then
    Begin
      result := true;
      t1 := oCan.Unit_.Link;
      t2 := oCan.Unit_.Term.Link;
      t1.Term := t2.Term.Link;
      oCan.Unit_ := t2;
      oCan.Unit_.Term := t1;
      oCan.Unit_.Operator := MULTIPLICATION;
      if oCan.Unit_.Term.Term <> nil Then
        oCan.Unit_.Term.Operator := MULTIPLICATION
      Else
        oCan.Unit_.Term.Operator := NOOP;
    End
    else
      result := SortTerms(oCan.Unit_);
    debug('after sort', oCan.Unit_);
  End;
end;

{ TUcumComponent }

function TUcumComponent.Link: TUcumComponent;
begin
  result := TUcumComponent(Inherited Link);
end;

{ TUcumCanonical }

constructor TUcumCanonical.Create;
begin
  inherited;

end;

destructor TUcumCanonical.Destroy;
begin
  FUnit.Free;
  inherited;
end;

procedure TUcumCanonical.divideValue(i: integer);
begin
  Value := FValue.Divide(i);
end;

procedure TUcumCanonical.divideValue(i: TSmartDecimal);
begin
  Value := FValue.Divide(i);
end;

procedure TUcumCanonical.multiplyValue(i: integer);
begin
  Value := FValue.Multiply(i);
end;

procedure TUcumCanonical.multiplyValue(i: TSmartDecimal);
begin
  Value := FValue.Multiply(i);
end;

procedure TUcumCanonical.SetUnit(Value: TUcumTerm);
begin
  FUnit.Free;
  FUnit := Value;
end;


class Function TUcumFormalStructureComposer.compose(oTerm : TUcumTerm) : String;
var
  oBldr : TAdvStringBuilder;
Begin
  oBldr := TAdvStringBuilder.Create;
  Try
    composeTerm(oBldr, oTerm);
    result := obldr.AsString;
  Finally
    oBldr.Free;
  End;
End;

class Procedure TUcumFormalStructureComposer.composeTerm(oBldr : TAdvStringBuilder; oTerm : TUcumTerm);
Begin
  if (oTerm.Component <> nil) Then
    composeComp(oBldr, oTerm.Component);
  if (oTerm.Operator <> NOOP) Then
  begin
    if (oTerm.Operator = MULTIPLICATION) and (oTerm.Term <> nil) and (oTerm.Term.Component <> nil) and (oTerm.Term.Component is TUcumFactor) and (TUcumFactor(oTerm.Term.Component).Annotation <> '') then
      oBldr.Append(' ')
    else
      composeOp(oBldr, oTerm.Operator);
  end;
  if (oTerm.Term <> nil) Then
    composeTerm(oBldr, oTerm.Term);
End;

class Procedure TUcumFormalStructureComposer.composeComp(oBldr : TAdvStringBuilder; oComp : TUcumComponent);
Begin
  if (oComp is TUcumFactor) Then
    composeFactor(oBldr, TUcumFactor(oComp))
  else if (oComp is TUcumSymbol) Then
    composeSymbol(oBldr, TUcumSymbol(oComp))
  else if (oComp is TUcumTerm) Then
    composeTerm(oBldr, TUcumTerm(oComp))
  else
    oBldr.append('?');
End;

class Procedure TUcumFormalStructureComposer.composeSymbol(oBldr : TAdvStringBuilder; oSymbol : TUcumSymbol);
Begin
  if oSymbol.Exponent <> 1 then
    oBldr.append('(');
  if (oSymbol.Prefix <> nil) Then
    oBldr.append(oSymbol.Prefix.Names[0]);

  oBldr.append(oSymbol.Unit_.Names[0]);
  if (oSymbol.Exponent <> 1) Then
  Begin
    oBldr.append(' ^ ');
    oBldr.append(inttostr(oSymbol.Exponent));
  End;
  if oSymbol.Exponent <> 1 then
    oBldr.append(')');
End;

class Procedure TUcumFormalStructureComposer.composeFactor(oBldr : TAdvStringBuilder; oFactor : TUcumFactor);
Begin
  if (oFactor.Factor = 1) and (oFactor.Annotation <> '') then
    oBldr.append('"'+oFactor.Annotation+'"')
  else
    oBldr.append(inttostr(oFactor.Factor));
End;

class Procedure TUcumFormalStructureComposer.composeOp(oBldr : TAdvStringBuilder; aOperator : TUcumOperator);
Begin
  if (aOperator = DIVISION) Then
    oBldr.append(' / ')
  else
    oBldr.append(' * ');
End;


End.
