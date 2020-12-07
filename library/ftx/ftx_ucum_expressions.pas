Unit ftx_ucum_expressions;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$I fhir.inc}

Interface

uses
  SysUtils, Generics.Defaults,
  fsl_base, fsl_utilities, fsl_fpc,
  ftx_ucum_handlers, ftx_ucum_base;

Type
  TUcumComponent = class (TFslObject)
  public
    Function Link : TUcumComponent; Overload;
  End;

  TUcumFactor = class (TUcumComponent)
  private
    FFactor: Integer;
    FAnnotation : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(iFactor : integer); overload;
    constructor Create(iFactor : integer; Annotation : String); overload;
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
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(oUnit_ : TUcumUnit; oPrefix : TUcumPrefix; iExponent : Integer); Overload;
    destructor Destroy; Override;
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
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TUcumTerm; Overload;

    Procedure setTermCheckOp(oTerm : TUcumTerm);

    Property Component : TUcumComponent read FComponent write SetComponent;
    Property Operator : TUcumOperator read FOperator write FOperator;
    Property Term : TUcumTerm read FTerm write SetTerm;
  End;

  TUcumLexerTokenType = (NONE, NUMBER, SYMBOL, SOLIDUS, PERIOD, OPEN, CLOSE, ANNOTATION);

  TUcumLexer = class (TFslObject)
  private
    FSourceString : String;
    Fsource : TArray<UnicodeChar>;
    Findex : integer;
    Ftoken : String;
    Ftype : TUcumLexerTokenType;
    Fstart : integer;
    Function checkNumber(ch : UnicodeChar) : boolean;
    Function checkNumberOrSymbol(ch : UnicodeChar) : boolean;
    Function checkBrackets(ch : UnicodeChar; inBrackets : boolean) : boolean;
    Function isValidSymbolChar(ch : UnicodeChar; allowDigits, inSquares: boolean): boolean;
    Function checkAnnotation(ch : UnicodeChar) : boolean;
    function checkSingle(ch : UnicodeChar; test : UnicodeChar; atype : TUcumLexerTokenType ) : boolean;
    function nextChar() : UnicodeChar;
    function peekChar() : UnicodeChar;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(sSource : String);

    procedure consume;
    Property Token : String read FToken;
    Property TokenType : TUcumLexerTokenType read FType;
    procedure error(errMsg : String);
    function getTokenAsInt() : integer;
  End;

  TUcumCanonicalUnit = class (TFslObject)
  private
    FBase : TUcumBaseUnit;
    FExponent : integer;
    procedure SetBase(value : TUcumBaseUnit);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(base : TUcumBaseUnit; exponent : integer); overload;
    destructor Destroy; Override;
    property base : TUcumBaseUnit read FBase write SetBase;
    property exponent : integer read FExponent write FExponent;
  end;


  TUcumCanonical = class (TFslObject)
  private
    FValue : TFslDecimal;
    FUnits : TFslList<TUcumCanonicalUnit>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    procedure multiplyValue(i : TFslDecimal); overload;
    procedure multiplyValue(i : integer); overload;
    procedure divideValue(i : TFslDecimal); overload;
    procedure divideValue(i : integer); overload;

    Property Value : TFslDecimal read FValue write FValue;
    Property Units : TFslList<TUcumCanonicalUnit> read FUnits;
  End;

  TUcumConverter = class (TFslObject)
  private
    Fmodel : TUcumModel;
    Fhandlers : TUcumRegistry;
    Fone : TUcumFactor;

    Procedure debug(indent, state : String; oUnit : TUcumTerm); overload;
    Procedure debug(indent, state : String; can : TUcumCanonical); overload;
    function expandDefinedUnit(indent : String; unit_ : TUcumDefinedUnit) : TUcumCanonical; overload;
     function normalise(indent : String; term : TUcumTerm) : TUcumCanonical; overload;
     function normalise(indent : String; sym : TUcumSymbol) : TUcumCanonical; overload;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(oModel : TUcumModel; oHandlers : TUcumRegistry);
    destructor Destroy; Override;
    Function convert(oTerm: TUcumTerm) : TUcumCanonical;
  End;

  TUcumExpressionParser = class (TFslObject)
  private
    FModel : TUcumModel;
    FLexer : TUcumLexer;
    function parseSymbol : TUcumSymbol;
    function parseComp : TUcumComponent;
    function ParseTerm(bFirst : Boolean) : TUcumTerm;
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;
    class function Parse(oModel : TUcumModel; sExpression : String): TUcumTerm;
  End;


  TUcumExpressionComposer = class (TFslObject)
  private
    class Procedure composeTerm(oBuilder : TFslStringBuilder; term : TUcumTerm);
    class Procedure composeComp(oBuilder : TFslStringBuilder; comp : TUcumComponent);
    class Procedure composeSymbol(oBuilder : TFslStringBuilder; symbol : TUcumSymbol);
    class Procedure composeFactor(oBuilder : TFslStringBuilder; comp : TUcumFactor);
    class Procedure composeOp(oBuilder : TFslStringBuilder; op : TUcumOperator);
  public
    class Function compose(Term : TUcumTerm) : String; overload;
    class Function compose(can : TUcumCanonical; value : boolean) : String;  overload;
  End;

  TUcumFormalStructureComposer = class (TFslObject)
  private
    class Procedure composeTerm(oBldr : TFslStringBuilder; oTerm : TUcumTerm);
    class Procedure composeComp(oBldr : TFslStringBuilder; oComp : TUcumComponent);
    class Procedure composeSymbol(oBldr : TFslStringBuilder; oSymbol : TUcumSymbol);
    class Procedure composeFactor(oBldr : TFslStringBuilder; oFactor : TUcumFactor);
    class Procedure composeOp(oBldr : TFslStringBuilder; aOperator : TUcumOperator);
  Public
    class Function compose(oTerm : TUcumTerm) : String; overload;
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

function TUcumFactor.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FAnnotation.length * sizeof(char)) + 12);
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

function TUcumSymbol.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPrefix.sizeInBytes);
  inc(result, FUnit_.sizeInBytes);
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

function TUcumTerm.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FComponent.sizeInBytes);
  inc(result, FTerm.sizeInBytes);
end;

{ TUcumExpressionParser }

function TUcumExpressionParser.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FModel.sizeInBytes);
  inc(result, FLexer.sizeInBytes);
end;

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
  FSourceString := sSource;
  Fsource := unicodeChars(sSource);
  Findex := 1;
  consume();
End;

procedure TUcumLexer.consume;
var
  ch : UnicodeChar;
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
      raise ETerminologyError.create('Error processing Unit_ "'+FSourceString+'": unexpected character "'+ch+'" at position '+IntToStr(FStart));
  End;
End;

Function TUcumLexer.checkNumber(ch : UnicodeChar) : boolean;
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
      raise ETerminologyError.create('Error processing Unit_"'+FSourceString+'": unexpected character "'+ch+'" at position '+IntToStr(FStart)+': a + or - must be followed by at least one digit');
    Ftype := NUMBER;
    result := true;
  End
  else
    result := false;
End;

Function TUcumLexer.checkNumberOrSymbol(ch : UnicodeChar) : boolean;
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


Function TUcumLexer.checkBrackets(ch : UnicodeChar; inBrackets : boolean) : boolean;
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

Function TUcumLexer.isValidSymbolChar(ch : UnicodeChar; allowDigits, inSquares : boolean): boolean;
Begin
  result := ((allowDigits) and (ch >= '0') and (ch <= '9')) or (inSquares or ((ch >= 'a') and (ch <= 'z')) or ((ch >= 'A') and (ch <= 'Z')) or
       (ch = '[') or (ch = ']') or (ch = '%') or (ch = '*') or (ch = '^') or (ch = '''') or
       (ch = '"') or (ch = '_')) or (inSquares and (ch = '.'));
End;

Function TUcumLexer.checkAnnotation(ch : UnicodeChar) : boolean;
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
        raise ETerminologyError.create('Error processing Unit_"'+FSourceString+'": annotation contains non-ascii characters');
      if (ch = #0) Then
        raise ETerminologyError.create('Error processing Unit_"'+FSourceString+'": unterminated annotation');
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

function TUcumLexer.checkSingle(ch : UnicodeChar; test : UnicodeChar; atype : TUcumLexerTokenType ) : boolean;
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

function TUcumLexer.nextChar() : UnicodeChar;
Begin
  if Findex <= Length(FSource) Then
    result := FSource[Findex-1]
  else
    result := NO_CHAR;
  inc(Findex);
End;

function TUcumLexer.peekChar() : UnicodeChar;
Begin
  if Findex <= Length(FSource) Then
    result := FSource[Findex-1]
  else
    result := NO_CHAR;
End;


Procedure TUcumLexer.error(errMsg : String);
Begin
  raise ETerminologyError.Create('Error processing Unit: '''+FSourceString+''': '+ errMsg +' at character '+IntToStr(FStart));
End;

Function TUcumLexer.getTokenAsInt() : Integer;
Begin
  if FToken[1] = '+' Then
    result := StrToInt(Copy(FToken, 2, $FF))
  else
    result := StrToInt(FToken);
End;


function TUcumLexer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSourceString.length * sizeof(char)) + 12);
  inc(result, length(Fsource) * Sizeof(UnicodeChar));
  inc(result, (Ftoken.length * sizeof(char)) + 12);
end;

class Function TUcumExpressionComposer.compose(Term : TUcumTerm) : String;
var
  oBuilder : TFslStringBuilder;
begin
  if (Term = nil) Then
    result := '1'
  Else
  Begin
    oBuilder := TFslStringBuilder.Create;
    try
      composeTerm(oBuilder, Term);
      result := oBuilder.AsString;
    Finally
      oBuilder.Free;
    End;
  End;
End;


class Procedure TUcumExpressionComposer.composeTerm(oBuilder : TFslStringBuilder; Term : TUcumTerm);
Begin
  if (Term.Component <> nil) Then
    composeComp(oBuilder, Term.Component);
  if (Term.Operator <> NOOP) Then
    composeOp(oBuilder, Term.Operator);
  if (Term.Term <> nil) Then
    composeTerm(oBuilder, Term.Term);
End;


class Procedure TUcumExpressionComposer.composeComp(oBuilder : TFslStringBuilder; comp : TUcumComponent);
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

class Procedure TUcumExpressionComposer.composeSymbol(oBuilder : TFslStringBuilder; symbol : TUcumSymbol);
Begin
  if (symbol.Prefix <> nil) Then
    oBuilder.append(symbol.Prefix.Code);
  oBuilder.append(symbol.Unit_.Code);
  if (symbol.Exponent <> 1) Then
    oBuilder.append(inttostr(symbol.Exponent));
End;

class Procedure TUcumExpressionComposer.composeFactor(oBuilder : TFslStringBuilder; comp : TUcumFactor);
begin
  oBuilder.append(inttostr(comp.Factor));
End;

class Procedure TUcumExpressionComposer.composeOp(oBuilder : TFslStringBuilder; op : TUcumOperator);
begin
  if (op = DIVISION) Then
    oBuilder.append('/')
  else
    oBuilder.append('.');
End;

class Function TUcumExpressionComposer.compose(can : TUcumCanonical; value : boolean) : String;
var
  b : TStringBuilder;
  first : boolean;
  c : TUcumCanonicalUnit;
begin
  b := TStringBuilder.Create;
  try
    if (value) then
      b.append(can.Value.asDecimal);
    first := true;
    for c in can.units do
    begin
      if (first) then
        first := false
      else
        b.append('.');
      b.append(c.Base.Code);
      if (c.exponent <> 1) then
        b.append(c.exponent);
    end;
    result := b.ToString;
  finally
    b.free;
  end;
end;

{ TUcumUnitSorter }

Type
  TUcumUnitSorter = class (TFslComparer<TUcumCanonicalUnit>)
  public
    function Compare(const l, r : TUcumCanonicalUnit) : integer; override;
  end;

function TUcumUnitSorter.Compare(const l, r : TUcumCanonicalUnit) : integer;
begin
  result := l.Base.Code.compareTo(r.Base.Code);
end;

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

function TUcumConverter.convert(oTerm: TUcumTerm): TUcumCanonical;
begin
  result := normalise('  ', oTerm);
end;

function TUcumConverter.normalise(indent : String; term : TUcumTerm) : TUcumCanonical;
var
  temp : TUcumCanonical;
  divb : boolean;
  t : TUcumTerm;
  c, sf, st : TUcumCanonicalUnit;
  o : TUcumSymbol;
  i, j : integer;
begin
  result := TUcumCanonical.create();
  try
    result.Value := TFslDecimal.Create(1);
    debug(indent, 'canonicalise', term);
    divb := false;
    t := term;
    while (t <> nil) do
    begin
      if (t.Component is TUcumTerm) then
      begin
        temp := normalise(indent+'  ', t.Component as TUcumTerm);
        try
          if (divb) then
          begin
            result.divideValue(temp.Value);
            for c in temp.Units do
              c.exponent := 0 - c.exponent;
          end
          else
          begin
            result.multiplyValue(temp.Value);
          end;
          result.Units.addAll(temp.Units);
        finally
          temp.Free;
        end;
      end
      else if (t.Component is TUcumFactor) then
      begin
        if (divb) then
          result.divideValue((t.Component as TUcumFactor).Factor)
        else
          result.multiplyValue((t.Component as TUcumFactor).Factor);
      end
      else if (t.Component is TUcumSymbol) then
      begin
        o := t.Component as TUcumSymbol;
        temp := normalise(indent, o);
        try
          if (divb) then
          begin
            result.divideValue(temp.Value);
            for c in temp.Units do
              c.exponent := 0 - c.exponent;
          end
          else
          begin
            result.multiplyValue(temp.Value);
          end;
          result.Units.addAll(temp.Units);
        finally
          temp.Free;
        end;
      end;
      divb := t.Operator = DIVISION;
      t := t.term;
    end;

    debug(indent, 'collate', Result);

    for i := result.Units.Count - 1 downto 0 do
    begin
      sf := result.Units[i];
      for j := i-1 downto 0 do
      begin
        st := result.Units[j];
        if (st.Base = sf.Base) then
        begin
          st.exponent := sf.exponent+st.exponent;
          result.Units.Delete(i);
          break;
        end;
      end;
    end;

    for i := result.Units.Count - 1 downto 0 do
    begin
      sf := result.Units[i];
      if (sf.exponent = 0) then
      result.Units.delete(i);
    end;

    debug(indent, 'sort', result);
    result.Units.Sort(TUcumUnitSorter.create);
    debug(indent, 'done', result);
    result.Link;
  finally
    result.Free;
  end;
end;

Procedure TUcumConverter.debug(indent, state : String; oUnit : TUcumTerm);
begin
//  writeln(indent+state+': '+TUcumExpressionComposer.compose(oUnit));
end;

Procedure TUcumConverter.debug(indent, state : String; can : TUcumCanonical);
begin
//  writeln(indent+state+': '+TUcumExpressionComposer.compose(can, true));
end;

function TUcumConverter.normalise(indent : String; sym : TUcumSymbol) : TUcumCanonical;
var
  can : TUcumCanonical;
  c : TUcumCanonicalUnit;
  i : integer;
begin
  result := TUcumCanonical.Create;
  try
    result.Value := TFSLDecimal.create(1);
    if (sym.Unit_ is TUcumBaseUnit) then
    begin
      result.Units.add(TUcumCanonicalUnit.create(sym.Unit_.Link as TUcumBaseUnit, sym.exponent));
    end
    else
    begin
      can := expandDefinedUnit(indent, sym.Unit_ as TUcumDefinedUnit);
      try
        for c in can.Units do
          c.exponent := c.exponent * sym.exponent;
        result.Units.addAll(can.Units);
        if (sym.exponent > 0) then
          for i := 1 to sym.exponent do
            result.multiplyValue(can.Value)
        else
          for i := -1 downto sym.exponent do
            result.divideValue(can.Value);
      finally
        can.free;
      end;
    end;
    if (sym.Prefix <> nil) then
    begin
      if (sym.Exponent > 0) then
        for i := 1 to sym.exponent do
          result.multiplyValue(sym.Prefix.Value)
      else
        for i := -1 downto sym.exponent do
          result.divideValue(sym.Prefix.Value);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TUcumConverter.expandDefinedUnit(indent : String; unit_ : TUcumDefinedUnit) : TUcumCanonical;
var
  u : string;
  h : TUcumUnitHandler;
  t : TUcumTerm;
begin
  u := unit_.Value.unit_;
  if (unit_.isSpecial) then
  begin
    h := Fhandlers.HandlerByCode[unit_.code];
    if (h = nil) then
      raise ETerminologyError.create('Not handled yet (special unit)')
    else
       u := h.Units;
  end;

  t := TUcumExpressionParser.Parse(Fmodel, u);
  try
    debug(indent, 'now handle', t);
    result := normalise(indent+'  ', t);
    try
      result.multiplyValue(unit_.Value.Value);
      result.Link;
    finally
      result.Free;
    end;
  finally
    t.Free;
  end;
end;


function TUcumConverter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fmodel.sizeInBytes);
  inc(result, Fhandlers.sizeInBytes);
  inc(result, Fone.sizeInBytes);
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
  FUnits := TFslList<TUcumCanonicalUnit>.create;
end;

destructor TUcumCanonical.Destroy;
begin
  FUnits.Free;
  inherited;
end;

procedure TUcumCanonical.divideValue(i: integer);
begin
  Value := FValue.Divide(i);
end;

procedure TUcumCanonical.divideValue(i: TFslDecimal);
begin
  Value := FValue.Divide(i);
end;

procedure TUcumCanonical.multiplyValue(i: integer);
begin
  Value := FValue.Multiply(i);
end;

procedure TUcumCanonical.multiplyValue(i: TFslDecimal);
begin
  Value := FValue.Multiply(i);
end;

function TUcumCanonical.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FUnits.sizeInBytes);
end;

class Function TUcumFormalStructureComposer.compose(oTerm : TUcumTerm) : String;
var
  oBldr : TFslStringBuilder;
Begin
  oBldr := TFslStringBuilder.Create;
  Try
    composeTerm(oBldr, oTerm);
    result := obldr.AsString;
  Finally
    oBldr.Free;
  End;
End;

class Procedure TUcumFormalStructureComposer.composeTerm(oBldr : TFslStringBuilder; oTerm : TUcumTerm);
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

class Procedure TUcumFormalStructureComposer.composeComp(oBldr : TFslStringBuilder; oComp : TUcumComponent);
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

class Procedure TUcumFormalStructureComposer.composeSymbol(oBldr : TFslStringBuilder; oSymbol : TUcumSymbol);
Begin
//  if oSymbol.Exponent <> 1 then
  oBldr.append('(');
  if (oSymbol.Prefix <> nil) Then
    oBldr.append(oSymbol.Prefix.Names[0]);

  oBldr.append(oSymbol.Unit_.Names[0]);
  if (oSymbol.Exponent <> 1) Then
  Begin
    oBldr.append(' ^ ');
    oBldr.append(inttostr(oSymbol.Exponent));
  End;
//  if oSymbol.Exponent <> 1 then
  oBldr.append(')');
End;

class Procedure TUcumFormalStructureComposer.composeFactor(oBldr : TFslStringBuilder; oFactor : TUcumFactor);
Begin
  if (oFactor.Factor = 1) and (oFactor.Annotation <> '') then
    oBldr.append('"'+oFactor.Annotation+'"')
  else
    oBldr.append(inttostr(oFactor.Factor));
End;

class Procedure TUcumFormalStructureComposer.composeOp(oBldr : TFslStringBuilder; aOperator : TUcumOperator);
Begin
  if (aOperator = DIVISION) Then
    oBldr.append(' / ')
  else
    oBldr.append(' * ');
End;

{ TUcumCanonicalUnit }

constructor TUcumCanonicalUnit.Create(base : TUcumBaseUnit; exponent : integer);
begin
  Inherited Create;
  self.base := base;
  self.exponent := exponent;
end;

destructor TUcumCanonicalUnit.Destroy;
begin
  FBase.Free;
  inherited;
end;

procedure TUcumCanonicalUnit.SetBase(value : TUcumBaseUnit);
begin
  FBase.Free;
  FBase := value;
end;


function TUcumCanonicalUnit.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBase.sizeInBytes);
end;

End.
