unit FHIR.Cql.Parser;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Generics.Collections,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_pathengine,
  fhir_objects, fhir4_pathnode, fhir4_pathengine,
  FHIR.CQL.Model;

Type
  TCqlLexer = class (TFHIRPathLexer4)
  protected
    procedure checkStart(var details : TCqlIntervalOperationDetails);
    procedure readDateTimePrecision(var details : TCqlIntervalOperationDetails);
    procedure readProperly(var details : TCqlIntervalOperationDetails);
    function readQuantity(var details : TCqlIntervalOperationDetails) : boolean;
    function readQuantityOffset(var details : TCqlIntervalOperationDetails) : boolean;

    function readSame(var details : TCqlIntervalOperationDetails) : boolean;
    function readIncludes(var details : TCqlIntervalOperationDetails) : boolean;
    function readMeets(var details : TCqlIntervalOperationDetails) : boolean;
    function readOverlaps(var details : TCqlIntervalOperationDetails) : boolean;
    function readStarts(var details : TCqlIntervalOperationDetails) : boolean;
    function readEnds(var details : TCqlIntervalOperationDetails) : boolean;
    function readDuring(var details : TCqlIntervalOperationDetails) : boolean;
    function readIncludedIn(var details : TCqlIntervalOperationDetails) : boolean;
    function readWithin(var details : TCqlIntervalOperationDetails) : boolean;
    function readAfter(var details : TCqlIntervalOperationDetails; checkOnOr : boolean) : boolean;
    function readBefore(var details : TCqlIntervalOperationDetails; checkOnOr : boolean) : boolean;

    function isIntervalOperator(var details : TCqlIntervalOperationDetails; doRevert : boolean) : boolean;
    function isUnaryOperator : boolean;
  protected
    function collapseEmptyLists : boolean; override;
    function opCodes : TArray<String>; override;
  public
    procedure next; override;
  end;

  TCqlFunction = class (TFslObject)
  private
    FName: String;
    FIndex: integer;
    FMaxParamCount: integer;
    FTypeName: String;
    FMinParamCount: integer;
  public
    constructor Create(name, typeName : String; minParamCount, maxParamCount : Integer);
    function Link : TCqlFunction;

    property name : String read FName write FName;
    property typeName : String read FTypeName write FTypeName;
    property minParamCount : integer read FMinParamCount write FMinParamCount;
    property maxParamCount : integer read FMaxParamCount write FMaxParamCount;

    property index : integer read FIndex write FIndex;
  end;

  TCqlFunctionRegistry = class (TFslObject)
  private
    FMap : TFslMap<TCqlFunction>;
    FList : TFslList<TCqlFunction>;
    procedure add(func : TCqlFunction);
    procedure populate;
    function GetDefinition(i: integer): TCqlFunction;
  public
    constructor Create; override;
    destructor Destroy; override;
    property definition[i : integer] : TCqlFunction read GetDefinition; default;
    function IndexOf(name : String) : integer;
  end;

  TCqlParserState = (cpsProximal, cpsCheckAlias, cpsAllowInner, cpsNoprocessEnd);
  TCqlParserStateSet = set of TCqlParserState;

  TCqlParser = class (TFHIRPathParser)
  private
    FRegistry : TCqlFunctionRegistry;
    function isReservedWord(word : String) : boolean;
    function isStructuralWord(word : String) : boolean;

    procedure readVersion(lexer : TCqlLexer; item : TCqlVersioned);
    function readContext(lexer : TCqlLexer) : TCqlContextType;
    function expConstant(lexer : TCqlLexer; value : String) : TCqlExpressionNode;
    function expNull(lexer : TCqlLexer) : TCqlExpressionNode;

    procedure organisePrecedence(lexer : TCqlLexer; var node: TCqlExpressionNode);

    procedure readConstant(lexer: TCqlLexer; expression : TCqlExpressionNode);
    procedure readGroup(lexer: TCqlLexer; expression : TCqlExpressionNode);
    procedure readRetrieve(lexer: TCqlLexer; expression : TCqlExpressionNode; states : TCqlParserStateSet);
    procedure checkForAlias(lexer: TCqlLexer; expression : TCqlExpressionNode);
    procedure processFilters(lexer: TCqlLexer; expression : TCqlExpressionNode);
    procedure readTupleOrList(lexer : TCqlLexer; expression : TCqlExpressionNode; name : String; alreadyEntered : boolean);
    procedure readFunction(lexer : TCqlLexer; expression : TCqlExpressionNode; name : String);
    procedure readMultiSource(lexer : TCqlLexer; expression : TCqlExpressionNode);
    procedure readCode(lexer : TCqlLexer; expression : TCqlExpressionNode);
    procedure readConcept(lexer : TCqlLexer; expression : TCqlExpressionNode);
    procedure readConvert(lexer : TCqlLexer; expression : TCqlExpressionNode);
    procedure readCast(lexer : TCqlLexer; expression : TCqlExpressionNode);
    procedure readInterval(lexer : TCqlLexer; expression : TCqlExpressionNode);
    procedure readIf(lexer : TCqlLexer; expression : TCqlExpressionNode);
    procedure readCase(lexer : TCqlLexer; expression : TCqlExpressionNode);
    function readIndexer(lexer : TCqlLexer; expression : TCqlExpressionNode) : TCqlExpressionNode;
    function parseTypeExpression(lexer : TCqlLexer) : TCQLExpressionNode;
    function parseExpression(lexer : TCqlLexer; states : TCqlParserStateSet) : TCQLExpressionNode;

    function parseListType(lexer : TCqlLexer) : TCqlTypeSpecifier;
    function parseIntervalType(lexer : TCqlLexer) : TCqlTypeSpecifier;
    function parseTupleType(lexer : TCqlLexer) : TCqlTypeSpecifier;
    function parseChoiceType(lexer : TCqlLexer) : TCqlTypeSpecifier;
    function parseSimpleType(lexer : TCqlLexer; token : string) : TCqlTypeSpecifier;
    function parseTypeDetails(lexer : TCqlLexer) : TCqlTypeSpecifier;
    function parseDefine(lexer : TCqlLexer; context : TCqlContextType) : TCqlExpressionDefinition;
    function parseFunction(lexer : TCqlLexer; context : TCqlContextType) : TCqlFunctionDefinition;
    function parseValueSet(lexer : TCqlLexer; access : TCqlAccessLevel) : TCqlValueSetReference;
    function parseCode(lexer : TCqlLexer; access : TCqlAccessLevel) : TCqlCodeDefinition;
    function parseConcept(lexer : TCqlLexer; access : TCqlAccessLevel) : TCqlConceptDefinition;
    function parseCodeSystem(lexer : TCqlLexer; access : TCqlAccessLevel) : TCqlCodeSystemReference;
    function parseParameter(lexer : TCqlLexer) : TCqlParameterDefinition;
    function parseInclude(lexer : TCqlLexer) : TCqlInclude;
    function parseUsing(lexer : TCqlLexer) : TCqlUsing;
    function parseLibrary(lexer : TCqlLexer) : TCqlLibrary;

  public
    constructor Create; override;
    destructor Destroy; override;
    function parseCql(lib : String) : TCqlLibrary;
  end;

implementation

{ TCqlParser }

function TCqlParser.parseCode(lexer: TCqlLexer; access: TCqlAccessLevel): TCqlCodeDefinition;
var
  s : String;
begin
  result := TCqlCodeDefinition.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.readConstant('code name');
    result.AccessLevel := access;
    lexer.token(':');
    result.code := lexer.readIdentifier('code code');
    lexer.token('from');
    s := lexer.readIdentifier('code system');
    if lexer.takeToken('.') then
    begin
      result.system.LibraryName := s;
      result.system.Id := lexer.readIdentifier('code system');
    end
    else
      result.system.Id := s;
    if lexer.takeToken('display') then
      result.display := lexer.readConstant('display');

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseCodeSystem(lexer: TCqlLexer; access : TCqlAccessLevel): TCqlCodeSystemReference;
begin
  result := TCqlCodeSystemReference.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.readConstant('codesystem name');
    result.AccessLevel := access;
    lexer.token(':');
    result.URL := lexer.readConstant('url');
    readVersion(lexer, result);

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseConcept(lexer: TCqlLexer; access: TCqlAccessLevel): TCqlConceptDefinition;
var
  s : String;
  first : boolean;
  code : TCqlScopedIdReference;
begin
  result := TCqlConceptDefinition.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.readConstant('code name');
    result.AccessLevel := access;
    lexer.token(':');
    lexer.token('{');
    first := true;
    while not lexer.takeToken('}') do
    begin
      if first then
        first := false
      else
        lexer.token(',');
      s := lexer.readIdentifier('concept code');
      code := TCqlScopedIdReference.Create;
      try
        if lexer.takeToken('.') then
        begin
          code.LibraryName := s;
          code.Id := lexer.readIdentifier('concept code');
        end
        else
          code.Id := s;
        result.Codes.Add(code.Link);
      finally
        code.Free;
      end;
    end;

    if lexer.takeToken('display') then
      result.display := lexer.readConstant('display');

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseCql(lib: String): TCqlLibrary;
var
  lexer : TCqlLexer;
begin
  lexer := TCqlLexer.Create(fpV2, lib);
  try
    result := parseLibrary(lexer);
  finally
    lexer.Free;
  end;
end;

function TCqlParser.parseFunction(lexer: TCqlLexer; context: TCqlContextType): TCqlFunctionDefinition;
var
  first : boolean;
  param : TCqlFunctionParameterDefinition;
  access: TCqlAccessLevel;
begin
  access := CqlAccessDefault;
  lexer.Next;
  result := TCqlFunctionDefinition.create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.AccessLevel := access;
    result.Context := context;

    result.Name := lexer.readIdentifier('define');
    lexer.token('(');
    first := true;
    while not lexer.hasToken(')') do
    begin
      if first then
        first := false
      else
        lexer.token(',');
      param := TCqlFunctionParameterDefinition.create;
      try
        param.name := lexer.readIdentifier('parameter name');
        param.typeDetails := parseTypeDetails(lexer);
        result.parameters.add(param.link);
      finally
        param.Free;
      end;
    end;
    lexer.token(')');

    if (lexer.takeToken('returns')) then
      result.typeInfo := parseTypeDetails(lexer);
    lexer.token(':');
    result.body := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.Free;
  end;
end;

function TCqlParser.parseDefine(lexer: TCqlLexer; context: TCqlContextType): TCqlExpressionDefinition;
var
  access: TCqlAccessLevel;
begin
  access := CqlAccessDefault;
  result := TCqlExpressionDefinition.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.AccessLevel := access;
    result.Context := context;

    if (lexer.takeToken('public')) then
      access := CqlAccessPublic
    else if (lexer.takeToken('private')) then
      access := CqlAccessPrivate;

    result.Name := lexer.readIdentifier('define name');
    result.Access := access;
    lexer.token(':');
    result.expression := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;


function TCqlParser.parseInclude(lexer: TCqlLexer): TCqlInclude;
begin
  result := TCqlInclude.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.take;
    readVersion(lexer, result);
    if lexer.hasToken('called') then
    begin
      lexer.next;
      result.Alias := lexer.take;
    end
    else
      result.Alias := Result.Name;

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseLibrary(lexer: TCqlLexer): TCqlLibrary;
var
  token : String;
  access : TCqlAccessLevel;
  context : TCqlContextType;
begin
  context := CqlContextPatient;
  result := TCqlLibrary.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    lexer.token('library');
    result.Name := lexer.take;
    readVersion(lexer, result);
    access := CqlAccessDefault;

    while not lexer.done do
    begin
      token := lexer.take;
      if (token = 'public') then
        access := CqlAccessPublic
      else if (token = 'private') then
        access := CqlAccessPrivate
      else
      begin
        if (token = 'codesystem') then
          result.CodeSystems.Add(parseCodeSystem(lexer, access))
        else if (token = 'valueset') then
          result.ValueSets.Add(parseValueSet(lexer, access))
        else if (token = 'code') then
          result.Codes.Add(parseCode(lexer, access))
        else if (token = 'concept') then
          result.Concepts.Add(parseConcept(lexer, access))
        else if access <> CqlAccessDefault then
          raise ELibraryException.create('Unexpected token '+CODES_AccessLevel[access])
        else if (token = 'define') then
          if lexer.hasToken('function') then
            result.Functions.add(parseFunction(lexer, context))
          else
            result.Definitions.add(parseDefine(lexer, context))
        else if (token = 'using') then
          result.Using.Add(parseUsing(lexer))
        else if (token = 'include') then
          result.Includes.Add(parseInclude(lexer))
        else if (token = 'parameter') then
          result.Parameters.Add(parseParameter(lexer))
        else if (token = 'context') then
          context := readContext(lexer)
        else
          raise lexer.error('not done yet: '+token);
        access := CqlAccessDefault;
      end
    end;

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;


function TCqlParser.parseParameter(lexer: TCqlLexer): TCqlParameterDefinition;
begin
  result := TCqlParameterDefinition.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.take;
    // what comes next may be a type
    if not isStructuralWord(lexer.current) then
    begin
      if not lexer.hasToken('default') then
        result.TypeDetails := parseTypeDetails(lexer);
      if lexer.hasToken('default') then
      begin
        lexer.Next;
        result.DefaultValue := parseExpression(lexer, [cpsProximal]);
      end;
    end;
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseListType(lexer : TCqlLexer) : TCqlTypeSpecifier;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeList;
    result.Id := 'List';
    lexer.Next;
    result.Parameters.Add(parseTypeDetails(lexer));
    lexer.Token('>');
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseIntervalType(lexer : TCqlLexer) : TCqlTypeSpecifier;
var
  param : TCqlTypeSpecifier;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeInterval;
    result.Id := 'Interval';
    lexer.Next;
    param := TCqlTypeSpecifier.Create;
    try
      param.Id := lexer.readIdentifier('Interval Type');
      result.Parameters.Add(param.link);
    finally
      param.free;
    end;
    lexer.Token('>');
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseTupleType(lexer : TCqlLexer) : TCqlTypeSpecifier;
var
  loc : TSourceLocation;
  s : String;
  td : TCqlTypeSpecifier;
  first : boolean;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeTuple;
    result.Id := 'Tuple';
    lexer.Next;
    first := true;
    while not lexer.hasToken('}') do
    begin
      if not first and lexer.hasToken(',') then
        lexer.Next;
      first := false;
      loc := lexer.CurrentLocation;
      s := lexer.take;
      td := parseTypeDetails(lexer);
      td.StartPosition := loc;
      result.Elements.Add(s, td);
    end;
    lexer.next;
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.isReservedWord(word: String): boolean;
begin
  result := StringArrayExistsSensitive(['Choice', 'Interval', 'List', 'Tuple', 'after', 'and', 'as', 'asc', 'ascending', 'before', 'between', 'by', 'called', 'case', 'cast', 'codesystem',
      'codesystems', 'collapse', 'context', 'convert', 'day', 'days', 'default', 'define', 'desc', 'descending', 'difference', 'div', 'duration', 'during',
      'else', 'ends', 'except', 'external', 'false', 'flatten', 'from', 'function', 'hour', 'hours', 'if', 'implies', 'in', 'include', 'included', 'includes',
      'intersect', 'is', 'less', 'let', 'library', 'maximum', 'meets', 'millisecond', 'milliseconds', 'minimum', 'minute', 'minutes', 'mod', 'month', 'months',
      'more', 'null', 'occurs', 'of', 'on', 'or', 'overlaps', 'parameter', 'point', 'predecessor', 'private', 'properly', 'public', 'return', 'returns', 'same',
      'second', 'seconds', 'singleton', 'sort', 'starts', 'successor', 'such', 'than', 'that', 'then', 'to', 'true', 'union', 'using', 'valueset', 'week',
      'weeks', 'when', 'width', 'with', 'within', 'without', 'xor', 'year', 'years'
      ], word)
    or StringArrayExistsSensitive(CODES_CqlOperationId, word);
end;


function TCqlParser.isStructuralWord(word: String): boolean;
begin
  result := StringArrayExistsSensitive(['codesystem', 'define', 'code', 'concept', 'valueset', 'includes', 'parameter'], word);
end;

procedure TCqlParser.organisePrecedence(lexer: TCqlLexer; var node: TCqlExpressionNode);
begin
  // nothing yet
end;

function TCqlParser.parseChoiceType(lexer : TCqlLexer) : TCqlTypeSpecifier;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeChoice;
    result.Id := 'Choice';
    lexer.Next;
    result.Parameters.Add(parseTypeDetails(lexer));
    while (lexer.hasToken(',')) do
    begin
      lexer.Next;
      result.Parameters.Add(parseTypeDetails(lexer));
    end;
    lexer.Token('>');
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseSimpleType(lexer : TCqlLexer; token : string) : TCqlTypeSpecifier;
begin
  result := TCqlTypeSpecifier.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Kind := CqlTypeSimple;
    result.Id := token;
    if lexer.hasToken('.') then
    begin
      result.LibraryName := result.Id;
      lexer.take;
      result.Id := lexer.take;
    end;
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseTypeDetails(lexer: TCqlLexer): TCqlTypeSpecifier;
var
  token : string;
begin
  token := lexer.readIdentifier('Type Details');
  if (token = 'List') and lexer.hasToken('<') then
    result := parseListType(lexer)
  else if (token = 'Interval') and lexer.hasToken('<') then
    result := parseIntervalType(lexer)
  else if (token = 'Tuple') and lexer.hasToken('{') then
    result := parseTupleType(lexer)
  else if (token = 'Choice') and lexer.hasToken('{') then
    result := parseChoiceType(lexer)
  else
    result := parseSimpleType(lexer, token);
end;

function TCqlParser.parseTypeExpression(lexer: TCqlLexer): TCQLExpressionNode;
begin
  result := TCqlExpressionNode.Create(lexer.nextId);
  try
    result.kind := enkStructure;
    result.StructureType := cstType;
    result.TypeInfo := parseTypeDetails(lexer);
    result.link;
  finally
    result.Free;
  end;;
end;

function TCqlParser.parseUsing(lexer: TCqlLexer): TCqlUsing;
begin
  result := TCqlUsing.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.Name := lexer.take;
    readVersion(lexer, result);
    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.parseValueSet(lexer: TCqlLexer; access : TCqlAccessLevel): TCqlValueSetReference;
var
  ref : TCqlScopedIdReference;
begin
  result := TCqlValueSetReference.Create;
  try
    result.StartPosition := lexer.CurrentLocation;
    result.AccessLevel := access;
    result.Name := lexer.readConstant('valueset name');
    lexer.token(':');
    result.URL := lexer.readConstant('url');
    readVersion(lexer, result);
    if lexer.hasToken('codesystems') then
    begin
      lexer.take;
      lexer.token('{');
      while not lexer.hasToken('}') do
      begin
        ref := TCqlScopedIdReference.Create;
        try
          ref.StartPosition := lexer.CurrentLocation;
          ref.Id := lexer.take;
          if lexer.hasToken('.') then
          begin
            ref.LibraryName := ref.Id;
            lexer.take;
            ref.Id := lexer.take;
          end;
          result.CodeSystems.Add(ref.link);
          ref.EndPosition := lexer.CurrentLocation;
        finally
          ref.Free;
        end;
      end;
      lexer.token('}');
    end;

    result.EndPosition := lexer.CurrentLocation;
    result.Link;
  finally
    result.free;
  end;
end;

function TCqlParser.readContext(lexer: TCqlLexer): TCqlContextType;
var
  token : String;
begin
  token := lexer.take;
  if token = 'Patient' then
    result := CqlContextPatient
  else if token = 'Population' then
    result := CqlContextPopulation
  else
    raise lexer.error('Unknown value for context: "'+token+'"');
end;

procedure TCqlParser.readConvert(lexer: TCqlLexer; expression: TCqlExpressionNode);
begin
  expression.name := 'convert';
  expression.kind := enkStructure;
  expression.StructureType := cstConvert;

  expression.Items.Add(parseExpression(lexer, [cpsProximal]));
  lexer.token('to');
  expression.TypeInfo := parseTypeDetails(lexer);
end;

procedure TCqlParser.readCast(lexer: TCqlLexer; expression: TCqlExpressionNode);
begin
  expression.name := 'cast';
  expression.kind := enkStructure;
  expression.StructureType := cstConvert;

  expression.Items.Add(parseExpression(lexer, [cpsProximal]));
end;

procedure TCqlParser.readVersion(lexer: TCqlLexer; item: TCqlVersioned);
begin
  if lexer.hasToken('version') then
  begin
    lexer.next;
    item.Version := lexer.readConstant('version');
  end;
end;

constructor TCqlParser.Create;
begin
  inherited;
  FRegistry := TCqlFunctionRegistry.create;

end;

destructor TCqlParser.Destroy;
begin
  FRegistry.Free;
  inherited;
end;

function TCqlParser.expConstant(lexer : TCqlLexer; value: String): TCqlExpressionNode;
begin
  result := TCqlExpressionNode.Create(lexer.nextId);
  try
    result.constant := lexer.processConstant;
    result.kind := enkConstant;
    result.Link;
  finally
    result.Free;
  end;
end;

function TCqlParser.expNull(lexer: TCqlLexer): TCqlExpressionNode;
begin
  result := TCqlExpressionNode.Create(lexer.nextId);
  try
    result.constant := TFHIRConstant.create('$$null');
    result.kind := enkConstant;
    result.Link;
  finally
    result.Free;
  end;
end;

{ TCqlLexer }

procedure TCqlLexer.checkStart(var details: TCqlIntervalOperationDetails);
begin
  if takeToken('starts') then
    details.leftQualfier := CqlLeftStarts
  else if takeToken('ends') then
    details.leftQualfier := CqlLeftEnds
  else if takeToken('occurs') then
    details.leftQualfier := CqlLeftOccurs
end;

procedure TCqlLexer.readDateTimePrecision(var details: TCqlIntervalOperationDetails);
begin
  if (StringArrayExistsSensitive(['year', 'month', 'week', 'day', 'hour', 'minute', 'second', 'millisecond'], Current)) then
    details.precision := TCqlIntervalPrecision(StringArrayIndexOfSensitive(['year', 'month', 'week', 'day', 'hour', 'minute', 'second', 'millisecond'], take));
end;

procedure TCqlLexer.readProperly(var details: TCqlIntervalOperationDetails);
begin
  if takeToken('properly') then
    details.properly := true;
end;

function TCqlLexer.readQuantity(var details : TCqlIntervalOperationDetails): boolean;
begin
  if not isNumericalConstant then
    exit(false);
  try
    details.value := take;
  except
    exit(false);
  end;
  if not StringIsDecimal(details.value) then
    exit(false);
  if not (isUnit or isConstant) then
    exit(false);
  details.units := take;
  result := true;
end;

function TCqlLexer.readQuantityOffset(var details: TCqlIntervalOperationDetails): boolean;
begin
  if takeToken('less') then
  begin
    if not takeToken('than') then
      exit(false);
    details.relativeness := CqlRelativeLessThan;

    exit(readQuantity(details));
  end
  else if takeToken('more') then
  begin
    if not takeToken('than') then
      exit(false);
    details.relativeness := CqlRelativeMoreThan;
    exit(readQuantity(details));
  end
  else if readQuantity(details) then
  begin
    if not takeToken('or') then
      exit(true);
    if takeToken('less') then
      details.relativeness := CqlRelativeOrLess
    else if takeToken('more') then
      details.relativeness := CqlRelativeOrMore
    else
      exit(false);
    result := true;
  end
  else
    result := true; // note: here we return true if we didn't consume anything, irrespective of whether there's a match
end;

function TCqlLexer.readSame(var details: TCqlIntervalOperationDetails): boolean;
begin
  details.operation := CqlIntervalSame;
  readDateTimePrecision(details);
  if not takeToken('as') then
  begin
    if not takeToken('or') then
      exit(false);
    if takeToken('before') then
      details.relativeness := CqlRelativeBefore
    else if takeToken('after') then
      details.relativeness := CqlRelativeAfter
    else
      exit(false);
  end;
  if takeToken('start') then
  begin
    details.rightQualifier := CqlRightStart;
    takeToken('of');
  end
  else if takeToken('end') then
  begin
    details.rightQualifier := CqlRightEnd;
    takeToken('of');
  end;
  result := true;
end;

function TCqlLexer.readIncludes(var details: TCqlIntervalOperationDetails): boolean;
begin
  details.operation := CqlIntervalIncludes;
  readDateTimePrecision(details);
  if takeToken('start') then
  begin
    details.rightQualifier := CqlRightStart;
    takeToken('of');
  end
  else if takeToken('end') then
  begin
    details.rightQualifier := CqlRightEnd;
    takeToken('of');
  end;
  result := true;
end;

function TCqlLexer.readMeets(var details : TCqlIntervalOperationDetails) : boolean;
begin
  details.operation := CqlIntervalMeets;
  if takeToken('before') then
    details.relativeness := CqlRelativeBefore
  else if takeToken('after') then
    details.relativeness := CqlRelativeAfter;
  readDateTimePrecision(details);
  result := true;
end;

function TCqlLexer.readOverlaps(var details : TCqlIntervalOperationDetails) : boolean;
begin
  details.operation := CqlIntervalOverlaps;
  if takeToken('before') then
    details.relativeness := CqlRelativeBefore
  else if takeToken('after') then
    details.relativeness := CqlRelativeAfter;
  readDateTimePrecision(details);
  result := true;
end;

function TCqlLexer.readStarts(var details : TCqlIntervalOperationDetails) : boolean;
begin
  details.operation := CqlIntervalStarts;
  readDateTimePrecision(details);
  result := true;
end;

function TCqlLexer.readEnds(var details : TCqlIntervalOperationDetails) : boolean;
begin
  details.operation := CqlIntervalEnds;
  readDateTimePrecision(details);
  result := true;
end;

function TCqlLexer.readDuring(var details : TCqlIntervalOperationDetails) : boolean;
begin
  details.operation := CqlIntervalDuring;
  readDateTimePrecision(details);
  result := true;
end;

function TCqlLexer.readIncludedIn(var details : TCqlIntervalOperationDetails) : boolean;
begin
  details.operation := CqlIntervalIncludedIn;
  if not takeToken('in') then
    exit(false);
  readDateTimePrecision(details);
  result := true;
end;

function TCqlLexer.readWithin(var details : TCqlIntervalOperationDetails) : boolean;
begin
  details.operation := CqlIntervalWithin;
  if not readQuantity(details) then
    exit(false);

  if not takeToken('of') then
    exit(false);
  if takeToken('start') then
  begin
    details.rightQualifier := CqlRightStart;
    takeToken('of');
  end
  else if takeToken('end') then
  begin
    details.rightQualifier := CqlRightEnd;
    takeToken('of');
  end;
  result := true;
end;

function TCqlLexer.readAfter(var details : TCqlIntervalOperationDetails; checkOnOr : boolean) : boolean;
begin
  details.operation := CqlIntervalAfter;
  if (checkOnOr) then
    if (takeToken('on')) then
      begin
        if not (takeToken('or')) then
          exit(false);
        details.onOr := true;
      end;
  readDateTimePrecision(details);
  if takeToken('start') then
  begin
    details.rightQualifier := CqlRightStart;
    takeToken('of');
  end
  else if takeToken('end') then
  begin
    details.rightQualifier := CqlRightEnd;
    takeToken('of');
  end;
  result := true;
end;

function TCqlLexer.readBefore(var details : TCqlIntervalOperationDetails; checkOnOr : boolean) : boolean;
begin
  details.operation := CqlIntervalBefore;
  if (checkOnOr) then
    if (takeToken('or')) then
      begin
        if not (takeToken('on')) then
          exit(false);
        details.onOr := true;
      end;
  readDateTimePrecision(details);
  if takeToken('start') then
  begin
    details.rightQualifier := CqlRightStart;
    takeToken('of');
  end
  else if takeToken('end') then
  begin
    details.rightQualifier := CqlRightEnd;
    takeToken('of');
  end;
  result := true;
end;


function TCqlLexer.isIntervalOperator(var details: TCqlIntervalOperationDetails; doRevert : boolean): boolean;
begin
  mark;
  try
    // looking for same:
    details.clear();
    checkStart(details);
    if takeToken('meets') then
      exit(readMeets(details))
    else if takeToken('overlaps') then
      exit(readOverlaps(details))
    else if takeToken('same') then
      exit(readSame(details))
    else
    begin
      readProperly(details);
      if takeToken('during') then
        exit(readDuring(details))
      else if takeToken('included') then
        exit(readIncludedIn(details))
      else if takeToken('within') then
        exit(readWithin(details))
    end;

    revert;
    details.clear();
    checkStart(details);
    if readQuantityOffset(details) then
    begin
      if (takeToken('on')) then
      begin
        if not (takeToken('or')) then
          exit(false);
        details.onOr := true;
        if takeToken('after') then
          exit(readAfter(details, false))
        else if takeToken('before') then
          exit(readBefore(details, false))
        else
          exit(false);
      end
      else if takeToken('after') then
        exit(readAfter(details, true))
      else if takeToken('before') then
        exit(readBefore(details, true));
    end;

    revert;
    details.clear();
    if takeToken('starts') then
      exit(readStarts(details))
    else if takeToken('ends') then
      exit(readEnds(details));

    revert;
    details.clear();
    readProperly(details);
    if takeToken('includes') then
      exit(readIncludes(details));

    revert;
    result := false;
  finally
    if doRevert then
      revert;
  end;
end;

function TCqlLexer.isUnaryOperator: boolean;
var
  t1, t2 : String;
  cop : TCqlOperationId;
begin
  if StringArrayExistsSensitive(['+', '-'], Current) then
    exit(true);

  t1 := current;
  result := StringArrayExistsSensitive(NAMES_UNPREFIXED_OPERATORS, t1);
  if result then
  begin
    cop := TCqlOperationId(StringArrayIndexOfSensitive(CODES_CqlOperationId, t1));
    if (FOLLOWING_WORDS_CqlOperationId[cop] <> '') then
    begin
      mark;
      next;
      t2 := current;
      revert;
      result := t2 = FOLLOWING_WORDS_CqlOperationId[cop];
    end;
  end;
end;

procedure TCqlLexer.next;
begin
  inherited next;
  while hasComment do
    inherited next;
end;

function TCqlLexer.opCodes: TArray<String>;
var
  i : integer;
begin
  setLength(result, length(CODES_TFHIRPathOperation));
  for i := 0 to length(CODES_TFHIRPathOperation) - 1 do
    result[i] := CODES_TFHIRPathOperation[TFHIRPathOperation(i)];
end;

function TCqlLexer.collapseEmptyLists: boolean;
begin
  result := false;
end;

{ TCqlFunction }

constructor TCqlFunction.Create(name, typeName: String; minParamCount, maxParamCount: Integer);
begin
  inherited Create;
  FName := name;
  FTypeName := typeName;
  FMinParamCount := minParamCount;
  FMaxParamCount := maxParamCount;
end;

function TCqlFunction.Link: TCqlFunction;
begin
  result := TCqlFunction(inherited Link);
end;

{ TCqlFunctionRegistry }

constructor TCqlFunctionRegistry.Create;
begin
  inherited;
  FMap := TFslMap<TCqlFunction>.create('Functions');
  FList := TFslList<TCqlFunction>.create;
  Populate;
end;

destructor TCqlFunctionRegistry.Destroy;
begin
  FMap.Free;
  FList.Free;
  inherited;
end;

procedure TCqlFunctionRegistry.populate;
begin
  add(TCqlFunction.Create('retrieve', 'List', 3, 3));
  add(TCqlFunction.Create('query', 'List', 6, 6));
  add(TCqlFunction.Create('Code', 'Concept', 2, 3));
  add(TCqlFunction.Create('queryRef', 'List', 3, 4));
  add(TCqlFunction.Create('AgeInYears', 'integer', 0, 0));
  add(TCqlFunction.Create('AgeInYearsAt', 'integer', 1, 1));
  add(TCqlFunction.Create('Today', 'dateTime', 0, 0));
  add(TCqlFunction.Create('DateTime', 'dateTime', 3, 7));
  add(TCqlFunction.Create('Last', 'object', 1, 1));
  add(TCqlFunction.Create('Count', 'integer', 1, 1));
end;

procedure TCqlFunctionRegistry.add(func: TCqlFunction);
begin
  func.index := FList.Add(func)+1; // note one offset
  FMap.Add(func.name, func.Link);
end;

function TCqlFunctionRegistry.GetDefinition(i: integer): TCqlFunction;
begin
  result := FList[i-1];
end;

function TCqlFunctionRegistry.IndexOf(name: String): integer;
var
  func : TCqlFunction;
begin
  if FMap.TryGetValue(name, func) then
    result := func.index
  else
    result := 0;
end;

procedure TCqlParser.readConcept(lexer: TCqlLexer; expression: TCqlExpressionNode);
var
  first : boolean;
begin
  lexer.token('{');

  if lexer.hasToken('Code') then
  begin
    expression.kind := enkFunction;
    expression.FunctionId := pfCustom;
    expression.CqlFunctionId := cfConcept;

    first := true;
    while not (lexer.takeToken('}')) do
    begin
      if first then
        first := false
      else
        lexer.token(',');
      expression.Parameters.Add(parseExpression(lexer, [cpsProximal, cpsAllowInner]));
    end;
    if (lexer.hasToken('display')) then
    begin
      lexer.token('display');
      expression.Parameters.Add(parseExpression(lexer, [cpsProximal, cpsAllowInner]));
    end
    else
      expression.Parameters.Add(expNull(lexer));
  end
  else
    readTupleOrList(lexer, expression, 'Concept', true);
end;

procedure TCqlParser.readConstant(lexer: TCqlLexer; expression : TCqlExpressionNode);
begin
  if lexer.current.startsWith('''') then
    lexer.processConstant(lexer.current);
  expression.Constant := lexer.processConstant;
  expression.kind := enkConstant;
  expression.SourceLocationEnd := lexer.CurrentLocation;
  if lexer.isUnit() or lexer.isConstant then
    expression.units := lexer.take;
end;

procedure TCqlParser.readGroup(lexer: TCqlLexer; expression : TCqlExpressionNode);
begin
  lexer.next;
  expression.kind := enkGroup;
  expression.group := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
  if lexer.current <> ')' then
    raise lexer.error('Found '+lexer.current+' expecting a ")"');
  expression.SourceLocationEnd := lexer.CurrentLocation;
  lexer.next;
end;

procedure TCqlParser.readRetrieve(lexer: TCqlLexer; expression : TCqlExpressionNode; states : TCqlParserStateSet);
var
  s : String;
begin
  expression.name := 'Retrieve';
  expression.kind := enkStructure;
  expression.StructureType := cstRetrieve;
  lexer.token('[');
  s := lexer.readIdentifier('Retrieve source');
  if (lexer.hasToken('.')) then
  begin
    expression.LibraryName := s;
    lexer.Next;
    expression.Name := lexer.readIdentifier('scoped id');
  end
  else
    expression.Name := s;

  if not lexer.takeToken(']') then
  begin
    lexer.token(':');
    s := lexer.readIdentifier('retrieve rule');
    if (lexer.hasToken('in')) then
    begin
      expression.codePath := s;
      lexer.Next;
      s := lexer.readIdentifier('retrieve rule');
    end;
    if lexer.hasToken('.') then
    begin
      lexer.next;
      expression.Terminology := s+'.'+lexer.readIdentifier('terminology id');
    end
    else
      expression.Terminology := s;
    lexer.token(']');
  end;
  if (cpsCheckAlias in states) then
    checkForAlias(lexer, expression);
end;

procedure TCqlParser.checkForAlias(lexer: TCqlLexer; expression : TCqlExpressionNode);
begin
  if not lexer.done and lexer.isNameToken and not isReservedWord(lexer.current) and not lexer.endingToken(lexer.current) and
      not StringArrayExistsSensitive(CODES_TFHIRPathOperation, lexer.current) and
      not StringArrayExistsSensitive(CODES_CqlOperationId, lexer.current) then
  begin
    if (expression.kind = enkName) then
    begin
      expression.kind := enkStructure;
      expression.StructureType := cstRetrieve;
    end;
    expression.alias := lexer.readIdentifier('alias');
    while (lexer.hasToken('with')) do
    begin
      lexer.token('with');
      expression.withStmt := parseExpression(lexer, [cpsProximal]);
      expression.withStmt.alias := lexer.readIdentifier('With Alias');
      lexer.token('such');
      lexer.token('that');
      expression.suchThat := parseExpression(lexer, [cpsProximal]);
    end;
    processFilters(lexer, expression);
  end;
end;

procedure TCqlParser.processFilters(lexer: TCqlLexer; expression : TCqlExpressionNode);
var
  first : boolean;
  c : TCqlExpressionNode;
  s : String;
begin
  if lexer.takeToken('let') then
  begin
    repeat
      s := lexer.readIdentifier('let name');
      lexer.token(':');
      expression.elements.add(s, parseExpression(lexer, [cpsProximal, cpsCheckAlias]));
    until not lexer.takeToken(',');
  end;
  if (lexer.hasToken('where')) then
  begin
    lexer.token('where');
    expression.where := parseExpression(lexer, [cpsProximal]);
  end;
  if (lexer.hasToken('return')) then
  begin
    lexer.token('return');
    if (lexer.takeToken('all')) then
      expression.returnType := cqlReturnAll
    else if (lexer.takeToken('distinct')) then
      expression.returnType := cqlReturnDistinct
    else
      expression.returnType := cqlReturnUnspecified;
    expression.return := parseExpression(lexer, [cpsProximal]);
  end;
  if (lexer.hasToken('sort')) then
  begin
    lexer.token('sort');
    if (lexer.takeToken('by')) then
    begin
      first := true;
      repeat
        if first then
          first := false
        else
          lexer.token(',');
        c := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
        try
          if lexer.takeToken('asc') then
            c.sortOrder := cqlSortAscending
          else if lexer.takeToken('desc') then
            c.sortOrder := cqlSortDescending;
          expression.sort.Add(c.Link);
        finally
          c.Free;
        end;
      until not lexer.hasToken(',');
    end
    else if lexer.takeToken('asc') then
      expression.sortOrder := cqlSortAscending
    else
    begin
      lexer.takeToken('desc');
      expression.sortOrder := cqlSortDescending;
    end;
  end;
end;

procedure TCqlParser.readTupleOrList(lexer : TCqlLexer; expression : TCqlExpressionNode; name : String; alreadyEntered : boolean);
var
  first, list, tuple : boolean;
  s : String;
  l : TCqlExpressionNode;
begin
  list := false;
  tuple := false;
  expression.kind := enkStructure;

  if not alreadyEntered then
  begin
    if (name = '') then
      if lexer.takeToken('Tuple') then
        tuple := true
     else if lexer.takeToken('List') then
        list := true;

    if lexer.takeToken('<') then
    begin
      expression.typeInfo := parseTypeDetails(lexer);
      lexer.token('>');
    end;

    lexer.takeToken('{');
  end;
  first := true;
  while not lexer.hasToken('}') do
  begin
    if first then
      first := false
    else
      lexer.token(',');

    if list then
      expression.items.Add(parseExpression(lexer, [cpsProximal, cpsCheckAlias]))
    else if tuple then
    begin
      s := lexer.readIdentifier('Tuple Field Name');
      lexer.token(':');
      expression.elements.Add(s, parseExpression(lexer, [cpsProximal, cpsCheckAlias]));
    end
    else
    begin
      l := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
      try
        if lexer.hasToken(':') then // we're a tuple:
        begin
          tuple := true;
          lexer.token(':');
          expression.elements.Add(l.name, parseExpression(lexer, [cpsProximal, cpsCheckAlias]));
        end
        else
        begin
          list := true;
          expression.items.Add(l.Link);
        end;
      finally
        l.Free;
      end;
    end;
  end;
  lexer.token('}');

  if expression.name = '' then
    if tuple then
      expression.name := 'Tuple'
    else
      expression.name := 'List';
  if tuple then
    expression.StructureType := cstTuple
  else
    expression.StructureType := cstList;
end;

procedure TCqlParser.readFunction(lexer : TCqlLexer; expression : TCqlExpressionNode; name : String);
var
  first : boolean;
begin
  expression.Name := name;
  if StringArrayExistsSensitive(CODES_TFHIRPathFunctions, expression.Name) then
    expression.FunctionId := TFHIRPathFunction(StringArrayIndexOfSensitive(CODES_TFHIRPathFunctions, expression.Name))
  else
  begin
    expression.FunctionId := pfCustom;
    if StringArrayExistsSensitive(NAMES_CqlFunctions, expression.Name) then
      expression.CqlFunctionId := TCqlFunctionDefinitionId(StringArrayIndexOfSensitive(NAMES_CqlFunctions, expression.Name))
    else
      expression.CqlFunctionId := cfNull; // try to resolve later after parsing completes
  end;
  expression.kind := enkFunction;
  lexer.next;
  first := true;
  while lexer.current <> ')' do
  begin
    if first then
      first := false
    else
      lexer.token(',');
    expression.Parameters.add(parseExpression(lexer, [cpsProximal, cpsCheckAlias]));
  end;
  lexer.next;
  checkParameters(lexer, expression.SourceLocationStart, expression);
end;

procedure TCqlParser.readCase(lexer: TCqlLexer; expression: TCqlExpressionNode);
var
  c  : TCqlExpressionNode;
begin
  lexer.token('case');
  expression.name := 'case';
  expression.kind := enkStructure;
  expression.StructureType := cstCase;

  if not lexer.hasToken('when') then
    expression.ifTest := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);

  while lexer.takeToken('when') do
  begin
    c := TCqlExpressionNode.Create(lexer.nextId);
    try
      c.kind := enkStructure;
      c.StructureType := cstCaseItem;
      c.ifTest := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
      lexer.token('then');
      c.thenStmt := parseExpression(lexer, [cpsProximal, cpsCheckAlias, cpsNoprocessEnd]);
      expression.items.add(c.Link);
    finally
      c.Free;
    end;
  end;
  if lexer.takeToken('else') then
    expression.elseStmt := parseExpression(lexer, [cpsProximal, cpsCheckAlias, cpsNoprocessEnd]);
  lexer.token('end');
end;

procedure TCqlParser.readCode(lexer : TCqlLexer; expression : TCqlExpressionNode);
begin
  expression.kind := enkFunction;
  expression.FunctionId := pfCustom;
  expression.CqlFunctionId := cfCode;
  expression.Parameters.Add(parse(lexer));
  lexer.token('from');
  expression.Parameters.Add(parse(lexer));
  if (lexer.hasToken('display')) then
  begin
    lexer.token('display');
    expression.Parameters.Add(parse(lexer));
  end;
end;

procedure TCqlParser.readIf(lexer: TCqlLexer; expression: TCqlExpressionNode);
begin
  lexer.token('if');
  expression.name := 'if';
  expression.kind := enkStructure;
  expression.StructureType := cstIf;

  expression.ifTest := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
  lexer.token('then');
  expression.thenStmt := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
  if lexer.takeToken('else') then
    expression.elseStmt := parseExpression(lexer, [cpsProximal, cpsCheckAlias]);
end;

function TCqlParser.readIndexer(lexer: TCqlLexer; expression: TCqlExpressionNode) : TCqlExpressionNode;
begin
  lexer.next();
  result := TCqlExpressionNode.Create(lexer.nextId);
  try
    result.Kind := enkFunction;
    result.Functionid := pfItem;
    result.Parameters.add(parse(lexer));
    if (lexer.current <> ']') then
      raise lexer.error('The token '+lexer.Current+' is not expected here - a "]" expected');
    lexer.next;
    expression.inner := result.link;
  finally
    result.free;
  end;
end;

procedure TCqlParser.readInterval(lexer : TCqlLexer; expression : TCqlExpressionNode);
begin
  expression.kind := enkFunction;
  expression.FunctionId := pfCustom;
  expression.CqlFunctionId := cfInterval;

  if lexer.takeToken('(') then
    expression.Parameters.Add(expConstant(lexer, 'open'))
  else
  begin
    lexer.token('[');
    expression.Parameters.Add(expConstant(lexer, 'closed'))
  end;


  expression.Parameters.Add(parseExpression(lexer, [cpsProximal, cpsAllowInner]));
  lexer.token(',');
  expression.Parameters.Add(parseExpression(lexer, [cpsProximal, cpsAllowInner]));
  if lexer.takeToken(')') then
    expression.Parameters.Add(expConstant(lexer, 'open'))
  else
  begin
    lexer.token(']');
    expression.Parameters.Add(expConstant(lexer, 'closed'))
  end;

end;

procedure TCqlParser.readMultiSource(lexer: TCqlLexer; expression: TCqlExpressionNode);
begin
  lexer.token('from');
  expression.kind := enkStructure;
  expression.StructureType := cstMultiRetrieve;
  repeat
    expression.items.Add(parseExpression(lexer, [cpsProximal, cpsCheckAlias]));
  until not lexer.takeToken(',');
  processFilters(lexer, expression);
end;

function TCqlParser.parseExpression(lexer: TCqlLexer; states : TCqlParserStateSet): TCQLExpressionNode;
var
  s : String;
  focus : TCQLExpressionNode;
  localStates : TCqlParserStateSet;
  details : TCqlIntervalOperationDetails;
begin
  localStates := states;
  result := TCQLExpressionNode.Create(lexer.nextId);
  try
    result.SourceLocationStart := lexer.CurrentLocation;
    lexer.checkArithmeticPrefixes;

    if lexer.current = '(' then
    begin
      readGroup(lexer, result);
      localStates := localStates + [cpsAllowInner];
    end
    else if (lexer.current = '[') then
    begin
      readRetrieve(lexer, result, localStates);
      localStates := localStates - [cpsCheckAlias];
    end
    else if (StringArrayExistsSensitive(['Tuple', 'List', '{'], lexer.current)) then
      readTupleOrList(lexer, result, '', false)
    else if (lexer.current = 'if') then
      readIf(lexer, result)
    else if (lexer.current = 'case') then
      readCase(lexer, result)
    else if (lexer.current = 'from') then
      readMultiSource(lexer, result)
    else if (cpsProximal in localStates) and lexer.isUnaryOperator then
    begin
      result.kind := enkStructure;
      result.StructureType := cstPlaceHolder;
      localStates := localStates - [cpsCheckAlias];
    end
    else
    begin
      if not (lexer.isConstant or lexer.isToken or lexer.current.startsWith('"') or StringArrayExistsSensitive(CODES_CqlModifier, lexer.Current) or StringArrayExistsSensitive(['('], lexer.current)) then
        raise lexer.error('Found '+lexer.current+' expecting a constant or a token name');
      if not lexer.isConstant and StringArrayExistsSensitive(CODES_CqlModifier, lexer.Current) then
      begin
        result.Modifier := TCqlModifier(StringArrayIndexOfSensitive(CODES_CqlModifier, result.Name));
        lexer.Next;
      end;
      if lexer.current = '(' then
      begin
        readGroup(lexer, result);
        localStates := localStates + [cpsAllowInner];
      end
      else if lexer.isConstant then
        readConstant(lexer, result)
      else
      begin
        s := lexer.readIdentifier('Expression Token');
        if s = 'Interval' then
          readInterval(lexer, result)
        else if (lexer.current = '(') then
        begin
          readFunction(lexer, result, s);
          localStates := localStates + [cpsAllowInner];
        end
        else if s = 'Concept' then
          readConcept(lexer, result)
        else if (lexer.current = '{') then
        begin
          readTupleOrList(lexer, result, s, false);
          localStates := localStates + [cpsAllowInner];
        end
        else if s = 'Code' then
          readCode(lexer, result)
        else if s = 'convert' then
          readConvert(lexer, result)
        else if s = 'cast' then
          readCast(lexer, result)
        else
        begin
          result.Name := s;
          localStates := localStates + [cpsAllowInner];
        end;
      end;
    end;
    result.SourceLocationEnd := lexer.CurrentLocation;

    focus := result;
    while (lexer.current = '[') do
      focus := readIndexer(lexer, focus);

    if (cpsAllowInner in localStates) and (lexer.current = '.') then
    begin
      lexer.next;
      focus.Inner := parseExpression(lexer, [cpsAllowInner]);
    end;

    if cpsCheckAlias in localStates then
      checkForAlias(lexer, result);

    result.Proximal := cpsProximal in states;
    if (result.proximal) then
    begin
      focus := result;
      while not lexer.done and (lexer.isOp or StringArrayExistsSensitive(CODES_CqlOperationId, lexer.current) or lexer.isNumericalConstant or lexer.isIntervalOperator(details, true)) and
        not ((cpsNoprocessEnd in states) and (lexer.current = 'end')) do
      begin
        if lexer.isOp then
          focus.Operation := TFHIRPathOperation(StringArrayIndexOfSensitive(CODES_TFHIRPathOperation, lexer.current))
        else if StringArrayExistsSensitive(CODES_CqlOperationId, lexer.current) then
        begin
          focus.Operation := popCustom;
          focus.CqlOperation := TCqlOperationId(StringArrayIndexOfSensitive(CODES_CqlOperationId, lexer.current))
        end
        else if lexer.isIntervalOperator(details, false) then
        begin
          focus.Operation := popCustom;
          focus.CqlOperation := copInterval;
          focus.IntervalOpDetails := details;
        end
        else if lexer.isNumericalConstant then
        begin
          focus.Operation := popCustom;
          focus.CqlOperation := copUnnamedWhen;
        end
        else
          raise lexer.error('internal Error');
        focus.OpSourceLocationStart := lexer.CurrentStartLocation;
        focus.OpSourceLocationEnd := lexer.CurrentLocation;
        if not lexer.isNumericalConstant and not (focus.CqlOperation = copInterval) then
          lexer.next;
        if (focus.CqlOperation <> copNull) and (lexer.current = FOLLOWING_WORDS_CqlOperationId[focus.CqlOperation]) then
          lexer.Next;
        if (focus.Operation = popAs) or ((focus.Operation = popIs) and not StringArrayExistsSensitive(['not', 'null'], lexer.current)) then
          focus.OpNext := parseTypeExpression(lexer)
        else
          focus.opNext := parseExpression(lexer, states - [cpsProximal]);
        focus := focus.OpNext as TCQLExpressionNode;
      end;
      organisePrecedence(lexer, result);
    end;
    result.link;
  finally
    result.Free;
  end;
end;

end.


