unit fhir_vcl;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Classes, Generics.Collections, TypInfo,
  fsl_base, fsl_utilities, fsl_crypto,
  fhir_objects, fhir_common, fhir_factory, fhir_parser;

type
  EVCLParseException = class(Exception)
  private
    FPosition: Integer;
  public
    constructor Create(const AMessage: String; APosition: Integer = -1);
    property Position: Integer read FPosition;
  end;

  TVCLTokenType = (
    ttDash, ttOpen, ttClose, ttSemi, ttComma, ttDot, ttStar,
    ttEQ, ttIsA, ttIsNotA, ttDescOf, ttRegex, ttIn, ttNotIn,
    ttGeneralizes, ttChildOf, ttDescLeaf, ttExists,
    ttURI, ttSCode, ttQuotedValue, ttEOF
  );

  TVCLToken = record
    TokenType: TVCLTokenType;
    Value: String;
    Position: Integer;
    constructor Create(ATokenType: TVCLTokenType; const AValue: String; APosition: Integer);
    function ToString: String;
  end;

  { TVCLLexer }

  TVCLLexer = class
  private
    FInput: String;
    FPos: Integer;

    function Peek(Offset: Integer = 0): Char;
    procedure SkipWhitespace;
    function IsIdentifierChar(C: Char): Boolean;
    function IsUriChar(C: Char): Boolean;
    function IsCodeChar(C: Char): Boolean;
    function IsVersionChar(C: Char): Boolean;
    function ReadIdentifierChars: String;
    function ReadUriChars: String;
    function ReadCodeChars: String;
    function ReadVersionChars: String;
    function ReadQuotedValue(StartPos: Integer): TVCLToken;
  public
    constructor Create(const Input: String);
    function Tokenize: TArray<TVCLToken>;
  end;

  { TVCLParser }

  TVCLParser = class
  private
    FTokens: TArray<TVCLToken>;
    FPos: Integer;
    FFactory: TFHIRFactory;
    FValueSet: TFhirValueSetW;

    function Current: TVCLToken;
    function Peek: TVCLToken;
    procedure Consume(Expected: TVCLTokenType);
    procedure Expect(Expected: TVCLTokenType);

    function IsFilterOperator(TokenType: TVCLTokenType): Boolean;
    function IsSimpleCodeList: Boolean;

    procedure ParseExpr;
    procedure ParseSubExpr(IsExclusion: Boolean);
    procedure ParseSimpleExpr(const SystemUri: String; IsExclusion: Boolean);
    procedure ParseSimpleCodeList(const SystemUri: String; IsExclusion: Boolean);
    procedure ParseExprWithinParentheses(IsExclusion: Boolean);
    procedure ParseFilter(ConceptSet: TFhirValueSetComposeIncludeW; const PropertyCode: String);
    procedure ParseIncludeVs(ConceptSet: TFhirValueSetComposeIncludeW);
    procedure ParseConjunction;
    procedure ParseConjunctionWithFlag(IsExclusion: Boolean);
    procedure ParseDisjunction;
    procedure ParseDisjunctionWithFlag(IsExclusion: Boolean);
    procedure ParseExclusion;

    function ParseCode: String;
    function ParseQuotedString: String;
    function ParseFilterValue: String;

    function CreateConceptSet(const SystemUri: String; IsExclusion: Boolean): TFhirValueSetComposeIncludeW;
    function GetCurrentConceptSet(IsExclusion: Boolean): TFhirValueSetComposeIncludeW;
    function TokenTypeToFilterOperator(TokenType: TVCLTokenType): TFilterOperator;
  public
    constructor Create(Factory: TFHIRFactory; const Tokens: TArray<TVCLToken>);
    destructor Destroy; override;
    function Parse: TFhirValueSetW;
  end;

// Main parsing functions
function ParseVCL(Factory: TFHIRFactory; const VclExpression: String): TFhirValueSetW;
function ParseVCLAndSetId(Factory: TFHIRFactory; const VclExpression: String): TFhirValueSetW;

// Convenience functions
function ValidateVCLExpression(const VclExpression: String): Boolean;
function CreateVCLValueSet(Factory: TFHIRFactory; const Id, Name, Description: String): TFhirValueSetW;
procedure SplitSystemUri(const SystemUri: String; out System, Version: String);
function IsVCLCompatible(ValueSet: TFhirValueSetW): Boolean;

implementation
      
function IfThen(test : boolean; msgY, msgN : String) : String;
begin
  if test then
    result := msgY
  else
    result := msgN;
end;

const
  IDENTIFIER_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', ':', '?', '&', '%', '+', '-', '.', '@', '#', '$', '!', '{', '}', '_'];
  URI_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '?', '&', '%', '+', '-', '.', '@', '#', '$', '!', '{', '}', '_', '/'];
  CODE_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '-', '_'];
  VERSION_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '-', '_', '.', '+'];

{ EVCLParseException }

constructor EVCLParseException.Create(const AMessage: String; APosition: Integer);
begin
  inherited Create(AMessage + IfThen(APosition >= 0, ' at position ' + IntToStr(APosition), ''));
  FPosition := APosition;
end;

{ TVCLToken }

constructor TVCLToken.Create(ATokenType: TVCLTokenType; const AValue: String; APosition: Integer);
begin
  TokenType := ATokenType;
  Value := AValue;
  Position := APosition;
end;

function TVCLToken.ToString: String;
begin
  Result := GetEnumName(TypeInfo(TVCLTokenType), Ord(TokenType)) + '(' + Value + ')';
end;

{ TVCLLexer }

constructor TVCLLexer.Create(const Input: String);
begin
  inherited Create;
  FInput := Trim(Input);
  FPos := 1; // Pascal strings are 1-based
end;

function TVCLLexer.Peek(Offset: Integer): Char;
var
  PeekPos: Integer;
begin
  PeekPos := FPos + 1 + Offset;
  if PeekPos <= Length(FInput) then
    Result := FInput[PeekPos]
  else
    Result := #0;
end;

procedure TVCLLexer.SkipWhitespace;
begin
  while (FPos <= Length(FInput)) and (FInput[FPos] in [' ', #9]) do
    Inc(FPos);
end;

function TVCLLexer.IsIdentifierChar(C: Char): Boolean;
begin
  Result := C in IDENTIFIER_CHARS;
end;

function TVCLLexer.IsUriChar(C: Char): Boolean;
begin
  Result := C in URI_CHARS;
end;

function TVCLLexer.IsCodeChar(C: Char): Boolean;
begin
  Result := C in CODE_CHARS;
end;

function TVCLLexer.IsVersionChar(C: Char): Boolean;
begin
  Result := C in VERSION_CHARS;
end;

function TVCLLexer.ReadIdentifierChars: String;
begin
  Result := '';
  while (FPos <= Length(FInput)) and IsIdentifierChar(FInput[FPos]) do
  begin
    Result := Result + FInput[FPos];
    Inc(FPos);
  end;
end;

function TVCLLexer.ReadUriChars: String;
begin
  Result := '';
  while (FPos <= Length(FInput)) and IsUriChar(FInput[FPos]) do
  begin
    Result := Result + FInput[FPos];
    Inc(FPos);
  end;
end;

function TVCLLexer.ReadCodeChars: String;
begin
  Result := '';
  while (FPos <= Length(FInput)) and IsCodeChar(FInput[FPos]) do
  begin
    Result := Result + FInput[FPos];
    Inc(FPos);
  end;
end;

function TVCLLexer.ReadVersionChars: String;
begin
  Result := '';
  while (FPos <= Length(FInput)) and IsVersionChar(FInput[FPos]) do
  begin
    Result := Result + FInput[FPos];
    Inc(FPos);
  end;
end;

function TVCLLexer.ReadQuotedValue(StartPos: Integer): TVCLToken;
var
  Value: String;
  Ch: Char;
begin
  Value := '';
  Inc(FPos); // Skip opening quote

  while FPos <= Length(FInput) do
  begin
    Ch := FInput[FPos];
    if Ch = '"' then
    begin
      Inc(FPos);
      Exit(TVCLToken.Create(ttQuotedValue, Value, StartPos));
    end
    else if (Ch = '\') and (FPos + 1 <= Length(FInput)) then
    begin
      Inc(FPos);
      Ch := FInput[FPos];
      if Ch in ['"', '\'] then
        Value := Value + Ch
      else
        Value := Value + '\' + Ch;
      Inc(FPos);
    end
    else
    begin
      Value := Value + Ch;
      Inc(FPos);
    end;
  end;

  raise EVCLParseException.Create('Unterminated quoted string', StartPos);
end;

function TVCLLexer.Tokenize: TArray<TVCLToken>;
var
  Tokens: TList<TVCLToken>;
  StartPos: Integer;
  Ch: Char;
  Value: String;
begin
  Tokens := TList<TVCLToken>.Create;
  try
    while FPos <= Length(FInput) do
    begin
      SkipWhitespace;
      if FPos > Length(FInput) then
        Break;

      StartPos := FPos;
      Ch := FInput[FPos];

      case Ch of
        '-': begin Tokens.Add(TVCLToken.Create(ttDash, '-', StartPos)); Inc(FPos); end;
        '(': begin Tokens.Add(TVCLToken.Create(ttOpen, '(', StartPos)); Inc(FPos); end;
        ')': begin Tokens.Add(TVCLToken.Create(ttClose, ')', StartPos)); Inc(FPos); end;
        ';': begin Tokens.Add(TVCLToken.Create(ttSemi, ';', StartPos)); Inc(FPos); end;
        ',': begin Tokens.Add(TVCLToken.Create(ttComma, ',', StartPos)); Inc(FPos); end;
        '.': begin Tokens.Add(TVCLToken.Create(ttDot, '.', StartPos)); Inc(FPos); end;
        '*': begin Tokens.Add(TVCLToken.Create(ttStar, '*', StartPos)); Inc(FPos); end;
        '=': begin Tokens.Add(TVCLToken.Create(ttEQ, '=', StartPos)); Inc(FPos); end;
        '/': begin Tokens.Add(TVCLToken.Create(ttRegex, '/', StartPos)); Inc(FPos); end;
        '^': begin Tokens.Add(TVCLToken.Create(ttIn, '^', StartPos)); Inc(FPos); end;
        '>':
          if Peek = '>' then
          begin
            Tokens.Add(TVCLToken.Create(ttGeneralizes, '>>', StartPos));
            Inc(FPos, 2);
          end
          else
            raise EVCLParseException.Create('Unexpected character: ' + Ch, FPos);
        '<':
          if Peek = '<' then
          begin
            Tokens.Add(TVCLToken.Create(ttIsA, '<<', StartPos));
            Inc(FPos, 2);
          end
          else if Peek = '!' then
          begin
            Tokens.Add(TVCLToken.Create(ttChildOf, '<!', StartPos));
            Inc(FPos, 2);
          end
          else
          begin
            Tokens.Add(TVCLToken.Create(ttDescOf, '<', StartPos));
            Inc(FPos);
          end;
        '~':
          if (Peek = '<') and (Peek(1) = '<') then
          begin
            Tokens.Add(TVCLToken.Create(ttIsNotA, '~<<', StartPos));
            Inc(FPos, 3);
          end
          else if Peek = '^' then
          begin
            Tokens.Add(TVCLToken.Create(ttNotIn, '~^', StartPos));
            Inc(FPos, 2);
          end
          else
            raise EVCLParseException.Create('Unexpected character: ' + Ch, FPos);
        '!':
          if (Peek = '!') and (Peek(1) = '<') then
          begin
            Tokens.Add(TVCLToken.Create(ttDescLeaf, '!!<', StartPos));
            Inc(FPos, 3);
          end
          else
            raise EVCLParseException.Create('Unexpected character: ' + Ch, FPos);
        '?': begin Tokens.Add(TVCLToken.Create(ttExists, '?', StartPos)); Inc(FPos); end;
        '"': Tokens.Add(ReadQuotedValue(StartPos));
      else
        if Ch in ['a'..'z', 'A'..'Z'] then
        begin
          Value := ReadIdentifierChars;

          if Pos(':', Value) > 0 then
          begin
            // Read rest of URI
            Value := Value + ReadUriChars;

            // Check for version
            if (FPos <= Length(FInput)) and (FInput[FPos] = '|') then
            begin
              Inc(FPos);
              Value := Value + '|' + ReadVersionChars;
            end;
            Tokens.Add(TVCLToken.Create(ttURI, Value, StartPos));
          end
          else
            Tokens.Add(TVCLToken.Create(ttSCode, Value, StartPos));
        end
        else if Ch in ['0'..'9'] then
        begin
          Value := ReadCodeChars;
          Tokens.Add(TVCLToken.Create(ttSCode, Value, StartPos));
        end
        else
          raise EVCLParseException.Create('Unexpected character: ' + Ch, FPos);
      end;
    end;

    Tokens.Add(TVCLToken.Create(ttEOF, '', FPos));
    Result := Tokens.ToArray;
  finally
    Tokens.Free;
  end;
end;

{ TVCLParser }

constructor TVCLParser.Create(Factory: TFHIRFactory; const Tokens: TArray<TVCLToken>);
var
  ValueSetResource: TFHIRResourceV;
begin
  inherited Create;
  FFactory := Factory;
  FTokens := Tokens;
  FPos := 0;

  // Create ValueSet using factory
  ValueSetResource := FFactory.makeResource('ValueSet');
  FValueSet := FFactory.wrapValueSet(ValueSetResource);

  // Set basic properties
  FValueSet.status := psDraft;
end;

destructor TVCLParser.Destroy;
begin
  FValueSet.Free;
  inherited;
end;

function TVCLParser.Current: TVCLToken;
begin
  if FPos < Length(FTokens) then
    Result := FTokens[FPos]
  else
    Result := TVCLToken.Create(ttEOF, '', -1);
end;

function TVCLParser.Peek: TVCLToken;
begin
  if FPos + 1 < Length(FTokens) then
    Result := FTokens[FPos + 1]
  else
    Result := TVCLToken.Create(ttEOF, '', -1);
end;

procedure TVCLParser.Consume(Expected: TVCLTokenType);
begin
  if Current.TokenType <> Expected then
    raise EVCLParseException.Create('Expected ' + GetEnumName(TypeInfo(TVCLTokenType), Ord(Expected)) +
      ' but got ' + GetEnumName(TypeInfo(TVCLTokenType), Ord(Current.TokenType)), Current.Position);
  Inc(FPos);
end;

procedure TVCLParser.Expect(Expected: TVCLTokenType);
begin
  if Current.TokenType <> Expected then
    raise EVCLParseException.Create('Expected ' + GetEnumName(TypeInfo(TVCLTokenType), Ord(Expected)) +
      ' but got ' + GetEnumName(TypeInfo(TVCLTokenType), Ord(Current.TokenType)), Current.Position);
end;

function TVCLParser.IsFilterOperator(TokenType: TVCLTokenType): Boolean;
begin
  Result := TokenType in [ttEQ, ttIsA, ttIsNotA, ttDescOf, ttRegex, ttIn, ttNotIn,
                          ttGeneralizes, ttChildOf, ttDescLeaf, ttExists];
end;

function TVCLParser.TokenTypeToFilterOperator(TokenType: TVCLTokenType): TFilterOperator;
begin
  case TokenType of
    ttEQ: Result := foEqual;
    ttIsA: Result := foIsA;
    ttIsNotA: Result := foIsNotA;
    ttDescOf: Result := foDescendentOf;
    ttRegex: Result := foRegex;
    ttIn: Result := foIn;
    ttNotIn: Result := foNotIn;
    ttGeneralizes: Result := foGeneralizes;
    ttChildOf: Result := foChildOf;
    ttDescLeaf: Result := foDescendentLeaf;
    ttExists: Result := foExists;
  else
    Result := foNull;
  end;
end;

function TVCLParser.IsSimpleCodeList: Boolean;
var
  Lookahead: Integer;
  Token: TVCLToken;
begin
  Lookahead := FPos;
  while Lookahead < Length(FTokens) do
  begin
    Token := FTokens[Lookahead];

    if Token.TokenType = ttClose then
      Exit(True);

    if (Token.TokenType = ttOpen) and (Lookahead + 2 < Length(FTokens)) then
    begin
      if (FTokens[Lookahead + 1].TokenType = ttURI) and
         (FTokens[Lookahead + 2].TokenType = ttClose) then
      begin
        Inc(Lookahead, 3);
        Continue;
      end;
    end;

    if (Token.TokenType = ttOpen) or (Token.TokenType = ttDash) or IsFilterOperator(Token.TokenType) then
      Exit(False);

    Inc(Lookahead);
  end;
  Result := True;
end;

function TVCLParser.CreateConceptSet(const SystemUri: String; IsExclusion: Boolean): TFhirValueSetComposeIncludeW;
var
  PipePos: Integer;
  System, Version: String;
begin
  if IsExclusion then
    Result := FValueSet.addExclude
  else
    Result := FValueSet.addInclude;

  if SystemUri <> '' then
  begin
    // Split system URI and version if pipe is present
    PipePos := Pos('|', SystemUri);
    if PipePos > 0 then
    begin
      System := Copy(SystemUri, 1, PipePos - 1);
      Version := Copy(SystemUri, PipePos + 1, Length(SystemUri));
      Result.systemUri := System;
      Result.version := Version;
    end
    else
      Result.systemUri := SystemUri;
  end;
end;

function TVCLParser.GetCurrentConceptSet(IsExclusion: Boolean): TFhirValueSetComposeIncludeW;
var
  List: TFslList<TFhirValueSetComposeIncludeW>;
begin
  if IsExclusion then
    List := FValueSet.excludes
  else
    List := FValueSet.includes;

  if List.Count = 0 then
    Result := CreateConceptSet('', IsExclusion)
  else
    Result := List[List.Count - 1];
end;

procedure TVCLParser.ParseExpr;
begin
  ParseSubExpr(False);

  case Current.TokenType of
    ttComma: ParseConjunction;
    ttSemi: ParseDisjunction;
    ttDash: ParseExclusion;
  end;
end;

procedure TVCLParser.ParseSubExpr(IsExclusion: Boolean);
var
  SystemUri: String;
begin
  SystemUri := '';

  // Check for system URI in parentheses
  if (Current.TokenType = ttOpen) and (Peek.TokenType = ttURI) then
  begin
    Consume(ttOpen);
    SystemUri := Current.Value;
    Consume(ttURI);
    Consume(ttClose);
  end;

  if Current.TokenType = ttOpen then
  begin
    Consume(ttOpen);

    // Check for nested system URI
    if (Current.TokenType = ttOpen) and (Peek.TokenType = ttURI) then
    begin
      Consume(ttOpen);
      SystemUri := Current.Value;
      Consume(ttURI);
      Consume(ttClose);
    end;

    if IsSimpleCodeList then
      ParseSimpleCodeList(SystemUri, IsExclusion)
    else
      ParseExprWithinParentheses(IsExclusion);

    Consume(ttClose);
  end
  else
    ParseSimpleExpr(SystemUri, IsExclusion);
end;

procedure TVCLParser.ParseSimpleCodeList(const SystemUri: String; IsExclusion: Boolean);
var
  ConceptSet: TFhirValueSetComposeIncludeW;
  Code: String;
  Concept: TFhirValueSetComposeIncludeConceptW;
  Filter: TFhirValueSetComposeIncludeFilterW;
begin
  ConceptSet := CreateConceptSet(SystemUri, IsExclusion);

  if Current.TokenType = ttStar then
  begin
    Consume(ttStar);
    Filter := ConceptSet.addFilter;
    Filter.prop := 'concept';
    Filter.op := foExists;
    Filter.value := 'true';
    Exit;
  end
  else if Current.TokenType = ttIn then
  begin
    ParseIncludeVs(ConceptSet);
    Exit;
  end
  else
  begin
    Code := ParseCode;
    Concept := ConceptSet.addConcept;
    Concept.code := Code;
  end;

  while Current.TokenType in [ttSemi, ttComma] do
  begin
    Consume(Current.TokenType);

    if Current.TokenType = ttStar then
    begin
      Consume(ttStar);
      Filter := ConceptSet.addFilter;
      Filter.prop := 'concept';
      Filter.op := foExists;
      Filter.value := 'true';
    end
    else if Current.TokenType = ttIn then
      ParseIncludeVs(ConceptSet)
    else
    begin
      Code := ParseCode;
      Concept := ConceptSet.addConcept;
      Concept.code := Code;
    end;
  end;
end;

procedure TVCLParser.ParseSimpleExpr(const SystemUri: String; IsExclusion: Boolean);
var
  ConceptSet: TFhirValueSetComposeIncludeW;
  Code: String;
  Concept: TFhirValueSetComposeIncludeConceptW;
  Filter: TFhirValueSetComposeIncludeFilterW;
begin
  ConceptSet := CreateConceptSet(SystemUri, IsExclusion);

  if Current.TokenType = ttStar then
  begin
    Consume(ttStar);
    Filter := ConceptSet.addFilter;
    Filter.prop := 'concept';
    Filter.op := foExists;
    Filter.value := 'true';
  end
  else if Current.TokenType in [ttSCode, ttQuotedValue] then
  begin
    Code := ParseCode;

    if IsFilterOperator(Current.TokenType) then
      ParseFilter(ConceptSet, Code)
    else
    begin
      Concept := ConceptSet.addConcept;
      Concept.code := Code;
    end;
  end
  else if Current.TokenType = ttIn then
    ParseIncludeVs(ConceptSet)
  else
    raise EVCLParseException.Create('Expected code, filter, or include', Current.Position);
end;

procedure TVCLParser.ParseExprWithinParentheses(IsExclusion: Boolean);
begin
  ParseSubExpr(IsExclusion);

  while Current.TokenType in [ttComma, ttSemi, ttDash] do
  begin
    case Current.TokenType of
      ttComma: ParseConjunctionWithFlag(IsExclusion);
      ttSemi: ParseDisjunctionWithFlag(IsExclusion);
      ttDash: ParseExclusion;
    end;
  end;
end;

procedure TVCLParser.ParseFilter(ConceptSet: TFhirValueSetComposeIncludeW; const PropertyCode: String);
var
  Filter: TFhirValueSetComposeIncludeFilterW;
  Op: TVCLTokenType;
begin
  Filter := ConceptSet.addFilter;
  Filter.prop := PropertyCode;

  Op := Current.TokenType;
  Consume(Op);

  Filter.op := TokenTypeToFilterOperator(Op);

  case Op of
    ttEQ, ttIsA, ttIsNotA, ttDescOf, ttGeneralizes, ttChildOf, ttDescLeaf, ttExists:
      Filter.value := ParseCode;
    ttRegex:
      Filter.value := ParseQuotedString;
    ttIn, ttNotIn:
      Filter.value := ParseFilterValue;
  else
    raise EVCLParseException.Create('Unexpected filter operator: ' + GetEnumName(TypeInfo(TVCLTokenType), Ord(Op)), Current.Position);
  end;
end;

procedure TVCLParser.ParseIncludeVs(ConceptSet: TFhirValueSetComposeIncludeW);
var
  Uri: String;
begin
  Consume(ttIn);

  if Current.TokenType = ttURI then
  begin
    Uri := Current.Value;
    Consume(ttURI);
    ConceptSet.addValueSet(Uri);
  end
  else if Current.TokenType = ttOpen then
  begin
    Consume(ttOpen);
    Uri := Current.Value;
    Consume(ttURI);
    Consume(ttClose);
    ConceptSet.addValueSet(Uri);
  end
  else
    raise EVCLParseException.Create('Expected URI after ^', Current.Position);
end;

procedure TVCLParser.ParseConjunction;
var
  CurrentConceptSet: TFhirValueSetComposeIncludeW;
  Code: String;
  Concept: TFhirValueSetComposeIncludeConceptW;
begin
  CurrentConceptSet := GetCurrentConceptSet(False);

  while Current.TokenType = ttComma do
  begin
    Consume(ttComma);

    if Current.TokenType in [ttSCode, ttQuotedValue] then
    begin
      Code := ParseCode;
      if IsFilterOperator(Current.TokenType) then
        ParseFilter(CurrentConceptSet, Code)
      else
      begin
        Concept := CurrentConceptSet.addConcept;
        Concept.code := Code;
      end;
    end
    else
      ParseSubExpr(False);
  end;
end;

procedure TVCLParser.ParseConjunctionWithFlag(IsExclusion: Boolean);
var
  CurrentConceptSet: TFhirValueSetComposeIncludeW;
  Code: String;
  Concept: TFhirValueSetComposeIncludeConceptW;
begin
  CurrentConceptSet := GetCurrentConceptSet(IsExclusion);

  while Current.TokenType = ttComma do
  begin
    Consume(ttComma);

    if Current.TokenType in [ttSCode, ttQuotedValue] then
    begin
      Code := ParseCode;
      if IsFilterOperator(Current.TokenType) then
        ParseFilter(CurrentConceptSet, Code)
      else
      begin
        Concept := CurrentConceptSet.addConcept;
        Concept.code := Code;
      end;
    end
    else
      ParseSubExpr(IsExclusion);
  end;
end;

procedure TVCLParser.ParseDisjunction;
begin
  while Current.TokenType = ttSemi do
  begin
    Consume(ttSemi);
    ParseSubExpr(False);
  end;
end;

procedure TVCLParser.ParseDisjunctionWithFlag(IsExclusion: Boolean);
begin
  while Current.TokenType = ttSemi do
  begin
    Consume(ttSemi);
    ParseSubExpr(IsExclusion);
  end;
end;

procedure TVCLParser.ParseExclusion;
begin
  Consume(ttDash);
  ParseSubExpr(True);
end;

function TVCLParser.ParseCode: String;
begin
  if Current.TokenType = ttSCode then
  begin
    Result := Current.Value;
    Consume(ttSCode);
  end
  else if Current.TokenType = ttQuotedValue then
  begin
    Result := Current.Value;
    Consume(ttQuotedValue);
  end
  else
    raise EVCLParseException.Create('Expected code', Current.Position);
end;

function TVCLParser.ParseQuotedString: String;
begin
  if Current.TokenType = ttQuotedValue then
  begin
    Result := Current.Value;
    Consume(ttQuotedValue);
  end
  else
    raise EVCLParseException.Create('Expected quoted string', Current.Position);
end;

function TVCLParser.ParseFilterValue: String;
var
  Codes: TStringList;
begin
  if Current.TokenType = ttOpen then
  begin
    Consume(ttOpen);
    Codes := TStringList.Create;
    try
      Codes.Add(ParseCode);

      while Current.TokenType = ttComma do
      begin
        Consume(ttComma);
        Codes.Add(ParseCode);
      end;

      Consume(ttClose);
      Result := Codes.CommaText;
    finally
      Codes.Free;
    end;
  end
  else if Current.TokenType = ttURI then
  begin
    Result := Current.Value;
    Consume(ttURI);
  end
  else
    Result := ParseCode;
end;

function TVCLParser.Parse: TFhirValueSetW;
begin
  ParseExpr;
  Expect(ttEOF);
  Result := FValueSet;
  FValueSet := nil; // Transfer ownership
end;

{ Main parse function }

function ParseVCL(Factory: TFHIRFactory; const VclExpression: String): TFhirValueSetW;
var
  Lexer: TVCLLexer;
  Parser: TVCLParser;
  Tokens: TArray<TVCLToken>;
begin
  if Trim(VclExpression) = '' then
    raise EVCLParseException.Create('VCL expression cannot be empty');

  Lexer := TVCLLexer.Create(VclExpression);
  try
    Tokens := Lexer.Tokenize;
  finally
    Lexer.Free;
  end;

  Parser := TVCLParser.Create(Factory, Tokens);
  try
    Result := Parser.Parse;
  finally
    Parser.Free;
  end;
end;

function ParseVCLAndSetId(Factory: TFHIRFactory; const VclExpression: String): TFhirValueSetW;
var
  JsonComposer: TFHIRComposer;
  JsonString: String;
  HashCode: Cardinal;
begin
  Result := ParseVCL(Factory, VclExpression);

  // Generate JSON and create hash-based ID (similar to Java version)
  JsonComposer := Factory.makeComposer(nil, ffJson, nil, OutputStyleNormal);
  try
    JsonString := JsonComposer.Compose(Result.Resource);
    HashCode := HashStringToCode32(JsonString);
    Result.url := 'cid:' + IntToStr(HashCode);
  finally
    JsonComposer.Free;
  end;
end;

{ Convenience functions }

function ValidateVCLExpression(const VclExpression: String): Boolean;
var
  Lexer: TVCLLexer;
  Tokens: TArray<TVCLToken>;
begin
  Result := False;
  if Trim(VclExpression) = '' then
    Exit;

  try
    Lexer := TVCLLexer.Create(VclExpression);
    try
      Tokens := Lexer.Tokenize;
      // If we can tokenize without exception, basic syntax is valid
      Result := Length(Tokens) > 1; // At least one token plus EOF
    finally
      Lexer.Free;
    end;
  except
    on EVCLParseException do
      Result := False;
  end;
end;

function CreateVCLValueSet(Factory: TFHIRFactory; const Id, Name, Description: String): TFhirValueSetW;
var
  ValueSetResource: TFHIRResourceV;
begin
  ValueSetResource := Factory.makeResource('ValueSet');
  Result := Factory.wrapValueSet(ValueSetResource);

  if Id <> '' then
    Result.id := Id;
  if Name <> '' then
    Result.name := Name;
  if Description <> '' then
    Result.description := Description;

  Result.status := psDraft;
  Result.experimental := True;
end;

procedure SplitSystemUri(const SystemUri: String; out System, Version: String);
var
  PipePos: Integer;
begin
  PipePos := Pos('|', SystemUri);
  if PipePos > 0 then
  begin
    System := Copy(SystemUri, 1, PipePos - 1);
    Version := Copy(SystemUri, PipePos + 1, Length(SystemUri));
  end
  else
  begin
    System := SystemUri;
    Version := '';
  end;
end;

function IsVCLCompatible(ValueSet: TFhirValueSetW): Boolean;
var
  Include: TFhirValueSetComposeIncludeW;
  Filter: TFhirValueSetComposeIncludeFilterW;
  i, j: Integer;
  Includes, Excludes: TFslList<TFhirValueSetComposeIncludeW>;
begin
  Result := False;

  if not ValueSet.checkCompose('IsVCLCompatible', 'validation') then
    Exit;

  // Check that all includes and excludes use VCL-compatible patterns
  Includes := ValueSet.includes;
  Excludes := ValueSet.excludes;

  // Check includes
  for i := 0 to Includes.Count - 1 do
  begin
    Include := Includes[i];

    // If it has filters, check they're VCL-compatible
    if Include.hasFilters then
    begin
      for j := 0 to Include.filterCount - 1 do
      begin
        Filter := Include.filters[j];
        // Check filter operator is supported by VCL
        if not (Filter.op in [foEqual, foIsA, foIsNotA, foDescendentOf, foRegex,
                              foIn, foNotIn, foGeneralizes, foChildOf, foDescendentLeaf, foExists]) then
          Exit;
      end;
    end;
  end;

  // Check excludes (same logic)
  for i := 0 to Excludes.Count - 1 do
  begin
    Include := Excludes[i];

    if Include.hasFilters then
    begin
      for j := 0 to Include.filterCount - 1 do
      begin
        Filter := Include.filters[j];
        if not (Filter.op in [foEqual, foIsA, foIsNotA, foDescendentOf, foRegex,
                              foIn, foNotIn, foGeneralizes, foChildOf, foDescendentLeaf, foExists]) then
          Exit;
      end;
    end;
  end;

  Result := True;
end;

end.
